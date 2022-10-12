%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2021, Tony Rogvall
%%% @doc
%%%    simple udev monitor
%%% @end
%%% Created :  7 Jan 2021 by Tony Rogvall <tony@rogvall.se>

-module(udev_monitor).
-export([start/0]).
-export([start/1]).

%% subsystem   devtype
%% "acpi"
%% "net"
%%             "wlan"
%%
%% "usb"
%%             "usb_interface"
%%             "usb_device"
%% "power_supply"
%%
%% "drm"
%% "video4linux" 
%% "cgroup" ?
%%
%% tags
%%    tags => ["tag1", ..., "tagn"]
%%    

start() ->
    start(#{}).

start(Opts) ->
    Udev = udev:new(),
    Mon = udev:monitor_new_from_netlink(Udev, udev),
    case maps:get(subsystem, Opts, undefined) of
	undefined ->
	    ok;
	SubSys ->
	    DevType = maps:get(devtype, Opts, null),
	    ok = udev:monitor_filter_add_match_subsystem_devtype(Mon,SubSys,DevType)
    end,
    %% add tags
    lists:foreach(
      fun(Tag) ->
	      ok = udev:monitor_filter_add_match_tag(Mon,Tag)
      end, maps:get(tags, Opts, [])),
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
		    io:format("action=~s\n",
			      [udev:device_get_action(Dev)]),
		    io:format("  ~p\n", [udev:get_device_info(Dev)]),
		    loop(Udev, Mon)
	    end;
	Other ->
	    io:format("Got ~p\n", [Other]),
	    select(Udev, Mon, Ref)
    end.
