%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2021, Tony Rogvall
%%% @doc
%%%    "simple" udev monitor
%%% @end
%%% Created :  7 Jan 2021 by Tony Rogvall <tony@rogvall.se>

-module(udev_monitor).
-export([start/0, start/1, start/2]).
-export([test/0]).

-export([loop/4, select/5]).
-export([inpevt_callback/4]).

start() -> start([]).
start(Opts) -> start(Opts, fun(_Action,_Info,_Dev,Acc) -> Acc end).
start(Opts, Callback) ->
    Udev = udev:new(),
    Mon = udev:monitor_new_from_netlink(Udev, udev),
    case proplists:get_value(subsystem, Opts, undefined) of
	undefined ->
	    ok;
	SubSys ->
	    DevType = proplists:get_value(devtype, Opts, null),
	    ok = udev:monitor_filter_add_match_subsystem_devtype(Mon,SubSys,DevType)
    end,
    %% add tags
    lists:foreach(
      fun(Tag) ->
	      ok = udev:monitor_filter_add_match_tag(Mon,Tag)
      end, proplists:get_all_values(tag, Opts)),

    ok = udev:monitor_enable_receiving(Mon),

    %% now when we are receiveing, go through existing devices
    Sj = udev_devices:fold(
	   Udev, Opts, 
	   fun(Dev, Si) ->
		   Info = udev:get_device_info(Dev),
		   case match_dev(Dev, Info, Opts) of
		       true ->
			   Callback("add", Info, Dev, Si);
		       false ->
			   Si
		   end
	   end,  #{ opts=>Opts }),
    loop(Udev, Mon, Callback,Sj).

loop(Udev, Mon, Callback, State) ->
    Ref = erlang:make_ref(),
    case udev:select(Mon, Ref) of
	select ->
	    select(Udev, Mon, Ref, Callback, State);
	Error ->
	    Error
    end.

select(Udev, Mon, Ref, Callback, State) ->
    receive
	{select, Mon, Ref, ready_input} ->
	    case udev:monitor_receive_device(Mon) of
		undefined ->
		    loop(Udev, Mon, Callback, State);
		Dev ->
		    Action = udev:device_get_action(Dev),
		    Info = udev:get_device_info(Dev),
		    io:format("action=~s, info=~p\n", [Action,Info]),
		    Opts = maps:get(opts,State,[]),
		    State1 = case match_dev(Dev, Info, Opts) of
				 true ->
				     Callback(Action, Info, Dev, State);
				 false ->
				     State
			     end,
		    ?MODULE:loop(Udev, Mon, Callback, State1)
	    end;
	Event ->
	    State1 = Callback("event", [], Event, State),
	    ?MODULE:select(Udev, Mon, Ref, Callback, State1)
    end.

%% Match device parent names if this is a devnode
%% 
match_dev(Dev, Info, Opts) ->
    Names = proplists:get_all_values(name, Opts),
    Props = proplists:get_all_values(property, Opts),
    match_name(Dev, Info, Names) andalso match_property(Dev, Info, Props).

match_property(_Dev, Info, MatchProps) ->
    Prop = proplists:get_value(properties, Info, []),
    io:format("match properties Prop=~p, Match=~p\n", [Prop, MatchProps]),
    match_props(MatchProps, Prop).

match_props([{Name,Value}|MatchProps], Prop) ->
    case proplists:get_value(Name, Prop, false) of
	false -> match_props(MatchProps, Prop);
	Value -> true; %% any match!  regexp?
	_ -> match_props(MatchProps, Prop)
    end;
match_props([], _Prop) ->
    false.

match_name(Dev, Info, Names) ->
    Prop = proplists:get_value(properties, Info, []),
    DevNode = proplists:get_value(devnode, Info, ""),
    NAME = proplists:get_value("NAME",Prop,undefined),
    io:format("match names NAME=~p, Names=~p\n", [NAME, Names]),
    if is_list(DevNode), NAME =:= undefined ->
	    case udev:device_get_parent(Dev) of
		false -> false;
		Parent ->
		    PProp = udev:device_get_properties(Parent),
		    PNAME = stripq(proplists:get_value("NAME",PProp,undefined)),
		    io:format("match names PNAME=~p\n", [PNAME]),
		    match_names(PNAME, Names)
	    end;
       true ->
	    false
    end.

match_names(_PNAME, []) -> true;  %% match all if no names given
match_names(undefined, _Names) -> false;
match_names(PNAME, Names) ->
    lists:any(fun(N) -> 
		      case re:run(PNAME, N) of
			  {match,_} -> true;
			  _ -> false
		      end
	      end, Names).

%% strip double quotes
stripq(Atom) when is_atom(Atom) -> Atom;
stripq(String) when is_list(String) ->
    case String of
	[$"|String0] ->
	    case lists:reverse(String0) of
		[$"|String1] -> lists:reverse(String1);
		_ -> String0
	    end;
	_ -> String
    end.

%%
%% Test require inpevt
%%
test() ->
    inpevt:start(),
    start([{subsystem, "input"},
	   {tag, "power-switch"},
	   {property, {"ID_BUS", "bluetooth"}},
	   {property, {"ID_BUS", "usb"}},
	   {name, "Jabra"},
	   {name, "OpenMove"}],
	  fun inpevt_callback/4).

%% add and subscribe to input event if node has got devnode
%% "add"/"remove" and "event" other events
inpevt_callback("add", Info, _Dev, State) ->
    case proplists:get_value(devnode, Info, undefined) of
	undefined -> State;
	DevNode when is_list(DevNode) ->
	    case inpevt:add_device(#{device => DevNode}) of
		[] ->
		    io:format("unable to open ~p\n", [DevNode]),
		    State;
		[Added] ->
		    case inpevt:subscribe(Added) of
			{_Ref,_DevN} ->
			    io:format("devnode => ~p\n", [_DevN]),
			    State#{ DevNode => Added };
			Res ->
			    io:format("error: ~p\n", [Res]),
			    State
		    end
	    end
    end;
inpevt_callback("remove", Info, _Dev, State) ->
    DevNode = proplists:get_value(devnode, Info, ""),
    case maps:take(DevNode, State) of
	error -> State;
	{D, State1} ->
	    inpevt:delete_device(D),
	    State1
    end;
inpevt_callback("event", _Info, Event, State) ->
    io:format("event: ~p\n", [Event]),
    State.


