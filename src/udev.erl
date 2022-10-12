%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2021, Tony Rogvall
%%% @doc
%%%    libudev api
%%% @end
%%% Created :  7 Jan 2021 by Tony Rogvall <tony@rogvall.se>

-module(udev).

-on_load(init/0).
-export([new/0]).
-export([monitor_new_from_netlink/2]).
-export([monitor_filter_add_match_subsystem_devtype/3]).
-export([monitor_filter_add_match_tag/2]).
-export([monitor_filter_update/1]).
-export([monitor_filter_remove/1]).
-export([monitor_enable_receiving/1]).
-export([select/1, select/2]).
-export([monitor_receive_device/1]).

%% devices
-export([device_new_from_syspath/2]).
-export([device_new_from_devnum/2]).
-export([device_new_from_subsystem_sysname/3]).
-export([device_new_from_device_id/2]).
-export([device_new_from_environment/1]).

-export([device_get_parent/1]).
-export([device_get_parent_with_subsystem_devtype/3]).

-export([device_get_devpath/1]).
-export([device_get_subsystem/1]).
-export([device_get_devtype/1]).
-export([device_get_syspath/1]).
-export([device_get_sysname/1]).
-export([device_get_sysnum/1]).
-export([device_get_devnode/1]).
-export([device_get_is_initialized/1]).
-export([device_get_driver/1]).
-export([device_get_devnum/1]).
-export([device_get_action/1]).
-export([device_get_seqnum/1]).
-export([device_get_usec_since_initialized/1]).
-export([device_get_sysattr_value/2]).
-export([device_set_sysattr_value/3]).
-export([device_has_tag/2]).
-export([device_get_devlinks/1]).
-export([device_get_properties/1]).
-export([device_get_tags/1]).
-export([device_get_sysattrs/1]).
-export([device_get_property_value/2]).
%% enumerate
-export([enumerate_new/1]).
-export([enumerate_add_match_subsystem/2]).
-export([enumerate_add_nomatch_subsystem/2]).
-export([enumerate_add_match_sysattr/3]).
-export([enumerate_add_nomatch_sysattr/3]).
-export([enumerate_add_match_property/3]).
-export([enumerate_add_match_sysname/2]).
-export([enumerate_add_match_tag/2]).
-export([enumerate_add_match_parent/2]).
-export([enumerate_add_match_is_initialized/1]).
-export([enumerate_add_syspath/2]).
-export([enumerate_get_devices/1]).
-export([enumerate_get_subsystems/1]).

%% util
-export([get_device_info/1, get_sysattrs/1]).


-type udev() :: {udev, integer(), reference()}.
-type udev_monitor() :: {monitor, integer(), reference()}.
-type udev_device() :: {device, integer(), reference()}.
-type udev_enumerate() :: {enumerate, integer(), reference()}.

-define(nif_stub(),
	erlang:nif_error({nif_not_loaded,module,?MODULE,line,?LINE})).

init() ->
    Nif = filename:join(code:priv_dir(udev), "udev_nif"),
    erlang:load_nif(Nif, 0).

-spec new() -> udev().
new() ->
    ?nif_stub().

-spec monitor_new_from_netlink(UDev::udev(), Name::string()) -> 
	  udev_monitor().

monitor_new_from_netlink(_UDev, _Name) ->
    ?nif_stub().

-spec monitor_filter_add_match_subsystem_devtype(_Mon::udev_monitor(),
						 _SubSystem::string(),
						 _SysName::string()) -> ok.
	   
monitor_filter_add_match_subsystem_devtype(_Mon, _SubSystem, _SysName) ->
    ?nif_stub().

-spec monitor_filter_add_match_tag(Mon::udev_monitor(), Tag::string()) -> ok.
monitor_filter_add_match_tag(_Mon, _Tag) ->
    ?nif_stub().    

%% NOTE! must set filters tag/subsys above before switch to receiving mode    
-spec monitor_enable_receiving(Mon::udev_monitor()) -> ok.
monitor_enable_receiving(_Mon) ->
    ?nif_stub().

%% update current filters (tag/match)  
-spec monitor_filter_update(Mon::udev_monitor()) -> ok.   
monitor_filter_update(_Mon) ->
    ?nif_stub().

%% remove all filters
-spec monitor_filter_remove(Mon::udev_monitor()) -> ok.
monitor_filter_remove(_Mon) ->
    ?nif_stub().    

-spec select(Mon::udev_monitor()) -> ok.
select(_Mon) ->
    ?nif_stub().

-spec select(Mon::udev_monitor(),Ref::reference()) -> ok.
select(_Mon, _Ref) ->
    ?nif_stub().

-spec monitor_receive_device(Mon::udev_monitor()) -> udev_device().
monitor_receive_device(_Mon) ->
    ?nif_stub().    

-spec device_new_from_syspath(UDev::udev(), SysPath::string()) ->
	  undefined | udev_device().
device_new_from_syspath(_UDev, _SysPath) ->
    ?nif_stub().

-spec device_new_from_devnum(UDev::udev(), DevNum::integer()) ->
	  undefined | udev_device().
device_new_from_devnum(_UDev, _DevNum) ->
    ?nif_stub().    

-spec device_new_from_subsystem_sysname(UDev::udev(),
					SubSys::string(),
					SysName::string()) ->
	  undefined | udev_device().
device_new_from_subsystem_sysname(_UDev,_SubSys,_SysName) ->
    ?nif_stub().

-spec device_new_from_device_id(UDev::udev(),Id::string()) -> 
	  undefined | udev_device().
device_new_from_device_id(_UDev,_Id) ->
    ?nif_stub().

-spec device_new_from_environment(UDev::udev()) -> 
	  undefined | udev_device().
device_new_from_environment(_UDev) ->
    ?nif_stub().    

-spec device_get_parent(Dev::udev_device()) -> false | udev_device().
device_get_parent(_Dev) -> 
    ?nif_stub().

-spec device_get_parent_with_subsystem_devtype(Dev::udev_device(),
					       SubSys::string(),
					       DevType::string()) ->
	  false | udev_device().
device_get_parent_with_subsystem_devtype(_Dev,_SubSys,_DevType) ->
    ?nif_stub().    

-spec device_get_devpath(Dev::udev_device()) -> undefined | string().
device_get_devpath(_Dev) ->
    ?nif_stub().    

-spec device_get_subsystem(Dev::udev_device()) -> undefined | string().
device_get_subsystem(_Dev) ->
    ?nif_stub().

-spec device_get_devtype(Dev::udev_device()) -> undefined | string().
device_get_devtype(_Dev) ->
    ?nif_stub().

-spec device_get_syspath(Dev::udev_device()) -> undefined | string().
device_get_syspath(_Dev) ->
    ?nif_stub().

-spec device_get_sysname(Dev::udev_device()) -> undefined | string().
device_get_sysname(_Dev) ->
    ?nif_stub().

-spec device_get_sysnum(Dev::udev_device()) -> undefined | string().
device_get_sysnum(_Dev) ->
    ?nif_stub().

-spec device_get_devnode(Dev::udev_device()) -> undefined | string().
device_get_devnode(_Dev) ->
    ?nif_stub().

-spec device_get_is_initialized(Dev::udev_device()) -> boolean().
device_get_is_initialized(_Dev) ->
    ?nif_stub().

-spec device_get_driver(Dev::udev_device()) -> undefined | string().
device_get_driver(_Dev) ->
    ?nif_stub().

-spec device_get_devnum(Dev::udev_device()) -> undefined | integer().
device_get_devnum(_Dev) ->
    ?nif_stub().

-spec device_get_action(Dev::udev_device()) -> undefined | string().
device_get_action(_Dev) ->
    ?nif_stub().

-spec device_get_seqnum(Dev::udev_device()) -> integer().
device_get_seqnum(_Dev) ->
    ?nif_stub().

-spec device_get_usec_since_initialized(Dev::udev_device()) -> integer().
device_get_usec_since_initialized(_Dev) ->
    ?nif_stub().

-spec device_get_sysattr_value(Dev::udev_device(), Attr::string()) ->
	  undefined | string().
device_get_sysattr_value(_Dev, _Attr) ->
    ?nif_stub().

-spec device_set_sysattr_value(Dev::udev_device(), Attr::string(), Value::string()) ->
	  undefined | string().
device_set_sysattr_value(_Dev, _Attr, _Value) ->
    ?nif_stub().

-spec device_has_tag(Dev::udev_device(), Tag::string()) -> boolean().
device_has_tag(_Dev, _Tag) ->
    ?nif_stub().

-spec device_get_devlinks(Dev::udev_device()) ->  [{string(),string()}].
device_get_devlinks(_Dev) ->
    ?nif_stub().

-spec device_get_properties(Dev::udev_device()) ->  [{string(),string()}].
device_get_properties(_Dev) ->
    ?nif_stub().

-spec device_get_tags(Dev::udev_device()) ->  [{string(),string()}].
device_get_tags(_Dev) ->
    ?nif_stub().

-spec device_get_sysattrs(Dev::udev_device()) -> [{string(),string()}].
device_get_sysattrs(_Dev) ->
    ?nif_stub().

-spec device_get_property_value(Dev::udev_device(), Property::string()) ->
	  undefined | string().
device_get_property_value(_Dev, _Property) ->
    ?nif_stub().


-spec enumerate_new(UDev::udev()) ->
	  udev_enumerate().
enumerate_new(_UDev) ->
    ?nif_stub().

-spec enumerate_add_match_subsystem(Enum::udev_enumerate(), SubSys::string()) -> boolean().
enumerate_add_match_subsystem(_Enum, _SubSys) ->
    ?nif_stub().
-spec enumerate_add_nomatch_subsystem(Enum::udev_enumerate(), SubSys::string()) -> boolean().
enumerate_add_nomatch_subsystem(_Enum, _SubSys) ->
    ?nif_stub().
-spec enumerate_add_match_sysattr(Enum::udev_enumerate(), SysAttr::string(), Value::string()) -> boolean().
enumerate_add_match_sysattr(_Enum, _SysAttr, _Value) ->
    ?nif_stub().
-spec enumerate_add_nomatch_sysattr(Enum::udev_enumerate(), SysAttr::string(), Value::string()) -> boolean().
enumerate_add_nomatch_sysattr(_Enum, _SysAttr, _Value) ->
    ?nif_stub().
-spec enumerate_add_match_property(Enum::udev_enumerate(), Property::string(), Value::string()) -> boolean().
enumerate_add_match_property(_Enum, _Property, _Value) ->
    ?nif_stub().
-spec enumerate_add_match_sysname(Enum::udev_enumerate(), Sysname::string()) -> boolean().
enumerate_add_match_sysname(_Enum, _Sysname) ->
    ?nif_stub().
-spec enumerate_add_match_tag(Enum::udev_enumerate(), Tag::string()) -> boolean().
enumerate_add_match_tag(_Enum, _Tag) ->
    ?nif_stub().    
-spec enumerate_add_match_parent(Enum::udev_enumerate(), Parent::udev_device()) -> boolean().
enumerate_add_match_parent(_Enum, _Parent) ->
    ?nif_stub().
-spec enumerate_add_match_is_initialized(Enum::udev_enumerate()) -> boolean().
enumerate_add_match_is_initialized(_Enum) ->
    ?nif_stub().    
-spec enumerate_add_syspath(Enum::udev_enumerate(), SysPath::string()) -> boolean().
enumerate_add_syspath(_Enum, _SysPath) ->
    ?nif_stub().

%% Get a list of devpaths (from current filters)
-spec enumerate_get_devices(Enum::udev_enumerate()) ->
	  [string()].
enumerate_get_devices(_Enum) ->
    ?nif_stub().

%% Get a list of devpaths (from current filters)
-spec enumerate_get_subsystems(Enum::udev_enumerate()) ->
	  [string()].
enumerate_get_subsystems(_Enum) ->
    ?nif_stub().

%% utils
get_device_info(Dev) ->
    [
     {devpath,device_get_devpath(Dev)},
     {subsystem,device_get_subsystem(Dev)},
     {devtype,device_get_devtype(Dev)},
     {syspath,device_get_syspath(Dev)},
     {sysname,device_get_sysname(Dev)},
     {sysnum,device_get_sysnum(Dev)},
     {devnode,device_get_devnode(Dev)},
     {is_initialized,device_get_is_initialized(Dev)},
     {driver,device_get_driver(Dev)},
     {devnum,device_get_devnum(Dev)},
     {seqnum,device_get_seqnum(Dev)},
     {usec_since_initialized,device_get_usec_since_initialized(Dev)},
     {devlinks, device_get_devlinks(Dev)},
     {tags, device_get_tags(Dev)},
     {properties, device_get_properties(Dev)},
     {sysattrs, get_sysattrs(Dev)}
    ].

get_sysattrs(Dev) ->
    Attrs = device_get_sysattrs(Dev),
    lists:map(
      fun(Attr) ->
	      {Attr, device_get_sysattr_value(Dev, Attr)}
      end, Attrs).
