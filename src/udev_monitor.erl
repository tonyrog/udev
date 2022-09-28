%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2021, Tony Rogvall
%%% @doc
%%%    simple udev monitor
%%% @end
%%% Created :  7 Jan 2021 by Tony Rogvall <tony@rogvall.se>

-module(udev_monitor).
-export([start/0]).
-export([start/1]).
-export([get_device_info/1]).

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
		    io:format("  ~p\n", [get_device_info(Dev)]),
		    loop(Udev, Mon)
	    end;
	Other ->
	    io:format("Got ~p\n", [Other]),
	    select(Udev, Mon, Ref)
    end.

get_device_info(Dev) ->
    [
     {devpath,udev:device_get_devpath(Dev)},
     {subsystem,udev:device_get_subsystem(Dev)},
     {devtype,udev:device_get_devtype(Dev)},
     {syspath,udev:device_get_syspath(Dev)},
     {sysname,udev:device_get_sysname(Dev)},
     {sysnum,udev:device_get_sysnum(Dev)},
     {devnode,udev:device_get_devnode(Dev)},
     {is_initialized,udev:device_get_is_initialized(Dev)},
     {driver,udev:device_get_driver(Dev)},
     {devnum,udev:device_get_devnum(Dev)},
     {seqnum,udev:device_get_seqnum(Dev)},
     {usec_since_initialized,udev:device_get_usec_since_initialized(Dev)},
     {devlinks, udev:device_get_devlinks(Dev)},
     {tags, udev:device_get_tags(Dev)},
     {properties, udev:device_get_properties(Dev)},
     {sysattrs, get_sysattrs(Dev)}
    ].

get_sysattrs(Dev) ->
    Attrs = udev:device_get_sysattrs(Dev),
    lists:map(
      fun(Attr) ->
	      {Attr, udev:device_get_sysattr_value(Dev, Attr)}
      end, Attrs).


     
     

