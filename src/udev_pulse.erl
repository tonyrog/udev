%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2022, Tony Rogvall
%%% @doc
%%%    Detect when virtual input device is registered for bluetooh headset
%%% @end
%%% Created : 28 Sep 2022 by Tony Rogvall <tony@rogvall.se>

-module(udev_pulse).

-export([start/0]).
-export([list/0]).

start() ->
    Udev = udev:new(),
    Mon = udev:monitor_new_from_netlink(Udev, udev),
    SubSys = "input",
    DevType = null,
    ok = udev:monitor_filter_add_match_subsystem_devtype(Mon,SubSys,DevType),
    %% ok = udev:monitor_filter_add_match_tag(Mon,Tag)
    ok = udev:monitor_filter_add_match_tag(Mon,"power-switch"),
    ok = udev:monitor_enable_receiving(Mon),
    loop(Udev, Mon).

loop(Udev, Mon) ->
    Ref = erlang:make_ref(),
    case udev:select(Mon, Ref) of
	select ->
	    select(Udev, Mon, Ref);
	Error ->
	    Error
    end.

select(Udev, Mon, Ref) ->
    receive
	{select, Mon, Ref, ready_input} ->
	    case udev:monitor_receive_device(Mon) of
		undefined ->
		    loop(Udev, Mon);
		Dev ->
		    Action = udev:device_get_action(Dev),
		    Properties = udev:device_get_properties(Dev),
		    DevNode = udev:device_get_devnode(Dev),
		    NAME = proplists:get_value("NAME", Properties, undefined),
		    if is_list(DevNode), NAME =:= undefined ->
			    %% check&match NAME in parent
			    Parent = udev:device_get_parent(Dev),
			    PProperties = udev:device_get_properties(Parent),
			    PNAME = proplists:get_value("NAME", PProperties, undefined),
			    io:format("~s: PNAME=~p\n", [Action,stripq(PNAME)]),
			    if Action =:= "add" ->
				    Info = file:read_file_info(DevNode),
				    io:format("    devnode=~p\n", [DevNode]),
				    io:format("    info(devnode)=~p\n", [Info]);
			       true ->
				    ok
			    end,
			    loop(Udev, Mon);
		       true ->
			    loop(Udev, Mon)
		    end
	    end;
	Other ->
	    io:format("Got ~p\n", [Other]),
	    select(Udev, Mon, Ref)
    end.

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

%% list pulse/input devices

list() ->
    U = udev:new(),
    E = udev:enumerate_new(U),
    udev:enumerate_add_match_subsystem(E, "input"),
    %% udev:enumerate_add_match_tag(E, "seat"),
    udev:enumerate_add_match_tag(E, "power-switch"),
    udev:enumerate_add_match_property(E, "ID_BUS", "bluetooth"),
    Ds = lists:map(
	   fun(Path) ->
		   udev:device_new_from_syspath(U, Path)
	   end, udev:enumerate_get_devices(E)),
    dump_devices(1, Ds).

dump_devices(I, [D|Ds]) ->
    io:format("~w: ~p\n", [I,udev_monitor:get_device_info(D)]),
    dump_devices(I+1, Ds);
dump_devices(_, []) ->
    ok.
