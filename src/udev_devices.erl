%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2022, Tony Rogvall
%%% @doc
%%%    match/list current device 
%%% @end
%%% Created : 12 Oct 2022 by Tony Rogvall <tony@rogvall.se>

-module(udev_devices).

-export([i/0, i/1]).
-export([list/0, list/1]).
-export([fold/3, fold/4]).

-export([test/0]).

test() ->
    i([{subsystem, "input"},
       {tag, "power-switch"},
       {property, {"ID_BUS", "bluetooth"}},
       {property, {"ID_BUS", "usb"}},
       {name, "Jabra"},
       {name, "OpenMove"}
      ]).

i() -> i([]).
i(Opts) ->
    fold(Opts, fun(Dev,_Acc) -> format(Dev) end, []).

format(Dev) ->
    io:format("~p\n", [udev:get_device_info(Dev)]).
		       

list() ->
    fold([], fun(Dev,Acc) -> [udev:get_device_info(Dev)|Acc] end, []).
list(Opts) ->
    fold(Opts, fun(Dev,Acc) -> [udev:get_device_info(Dev)|Acc] end, []).
    

fold(Opts, Fun, Acc) ->  %% proplist
    Udev = udev:new(),
    fold(Udev, Opts, Fun, Acc).

fold(Udev, Opts, Fun, Acc) ->
    Enum = udev:enumerate_new(Udev),
    case proplists:get_value(subsystem, Opts, undefined) of
	undefined -> ok;
	SubSys ->
	    %%DevType = maps:get(devtype, Opts, null),
	    ok = udev:enumerate_add_match_subsystem(Enum,SubSys)
    end,
    %% add tags
    lists:foreach(
      fun(Tag) ->
	      ok = udev:enumerate_add_match_tag(Enum,Tag)
      end, proplists:get_all_values(tag, Opts)),
    %% add properties
    lists:foreach(
      fun({Name,Value}) ->
	      ok = udev:enumerate_add_match_property(Enum,Name,Value)
      end, proplists:get_all_values(property, Opts)),
    fold_match(Opts, Udev, Enum, Fun, Acc).


fold_match(Opts, Udev, Enum, Fun, Acc) ->
    lists:foldl(fun(Path,Acc0) ->
			case udev:device_new_from_syspath(Udev, Path) of
			    false -> 
				Acc0;
			    Dev ->
				case match_name(Dev, Opts) of
				    true ->
					Fun(Dev, Acc0);
				    false ->
					Acc0
				end
			end
		end,
		Acc, 
		udev:enumerate_get_devices(Enum)).

%% Match device parent names if this is a devnode
match_name(Dev, Opts) ->
    Prop = udev:device_get_properties(Dev),
    DevNode = udev:device_get_devnode(Dev),
    NAME = proplists:get_value("NAME",Prop,undefined),
    if is_list(DevNode), NAME =:= undefined ->
	    case udev:device_get_parent(Dev) of
		false -> false;
		Parent ->
		    PProp = udev:device_get_properties(Parent),
		    PNAME = stripq(proplists:get_value("NAME",PProp,undefined)),
		    Names = proplists:get_all_values(name, Opts),
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
