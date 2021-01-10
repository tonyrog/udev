// libudev api
#include <stdio.h>
#include <memory.h>
#include <math.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>

#include "erl_driver.h"
#include "erl_nif.h"

// #define DEBUG
// #define NIF_TRACE

#define UNUSED(a) ((void) a)

#ifdef DEBUG
#include <stdio.h>
#define DBG(...) printf(__VA_ARGS__)
#define BADARG(env) printf("udev_nif.c: badarg line=%d\r\n", __LINE__), enif_make_badarg((env))
#else
#define DBG(...)
#define BADARG(env) enif_make_badarg((env))
#endif

// Atom macros
#define ATOM(name) atm_##name

#define DECL_ATOM(name) \
    ERL_NIF_TERM atm_##name = 0

// require env in context (ugly)
#define LOAD_ATOM(name)			\
    atm_##name = enif_make_atom(env,#name)

#define LOAD_ATOM_STRING(name,string)			\
    atm_##name = enif_make_atom(env,string)


#include <libudev.h>

#define MAX_STRING_LEN 255
#define MAX_STRING_SIZE (MAX_STRING_LEN+1)
    
typedef enum {
    UDEVOBJ_TYPE_UDEV,
    UDEVOBJ_TYPE_DEVICE,
    UDEVOBJ_TYPE_MONITOR,
    UDEVOBJ_TYPE_ENUMERATE,
    UDEVOBJ_TYPE_QUEUE,
    UDEVOBJ_TYPE_HWDB
} udevobj_type_t;


#define get_udevobj_type(t) *udevobj_type[(t)]

typedef struct _udevobj_t
{
    udevobj_type_t type;
    union {
	struct udev* udev;
	struct udev_device* device;
	struct udev_monitor* monitor;
	struct udev_enumerate* enumerate;
	struct udev_queue* queue;
	struct udev_hwdb* hwdb;
	void* ptr;       
    };
} udevobj_t;

static ErlNifResourceType* nif_udev_res;

DECL_ATOM(ok);
DECL_ATOM(true);
DECL_ATOM(false);
DECL_ATOM(undefined);
DECL_ATOM(error);
DECL_ATOM(null);
DECL_ATOM(select);
DECL_ATOM(failed);
DECL_ATOM(invalid);
DECL_ATOM(ready_input);
DECL_ATOM(ready_output);

// types
DECL_ATOM(udev);
DECL_ATOM(device);
DECL_ATOM(monitor);
DECL_ATOM(enumerate);
DECL_ATOM(queue);
DECL_ATOM(hwdb);

static int nif_udev_load(ErlNifEnv* env, void** priv_data,
			 ERL_NIF_TERM load_info);
static int nif_udev_upgrade(ErlNifEnv* env, void** priv_data,
			    void** old_priv_data,
			    ERL_NIF_TERM load_info);
static void nif_udev_unload(ErlNifEnv* env, void* priv_data);

// Dirty optional since 2.7 and mandatory since 2.12
#if (ERL_NIF_MAJOR_VERSION > 2) || ((ERL_NIF_MAJOR_VERSION == 2) && (ERL_NIF_MINOR_VERSION >= 7))
#ifdef USE_DIRTY_SCHEDULER
#define NIF_FUNC(name,arity,fptr) {(name),(arity),(fptr),(ERL_NIF_DIRTY_JOB_CPU_BOUND)}
#define NIF_DIRTY_FUNC(name,arity,fptr) {(name),(arity),(fptr),(ERL_NIF_DIRTY_JOB_CPU_BOUND)}
#else
#define NIF_FUNC(name,arity,fptr) {(name),(arity),(fptr),(0)}
#define NIF_DIRTY_FUNC(name,arity,fptr) {(name),(arity),(fptr),(ERL_NIF_DIRTY_JOB_CPU_BOUND)}
#endif
#else
#define NIF_FUNC(name,arity,fptr) {(name),(arity),(fptr)}
#define NIF_DIRTY_FUNC(name,arity,fptr) {(name),(arity),(fptr)}
#endif

#define NIF_LIST \
    NIF("new", 0, nif_new)					     \
    NIF("monitor_new_from_netlink", 2, nif_monitor_new_from_netlink)	\
    NIF("monitor_filter_add_match_subsystem_devtype", 3, nif_monitor_filter_add_match_subsystem_devtype) \
    NIF("monitor_filter_add_match_tag",2,nif_monitor_filter_add_match_tag) \
    NIF("monitor_filter_update",1,nif_monitor_filter_update)		\
    NIF("monitor_filter_remove",1,nif_monitor_filter_remove)		\
    NIF("monitor_enable_receiving", 1, nif_monitor_enable_receiving)	\
    NIF("select", 1,nif_select)						\
    NIF("select", 2,nif_select)						\
    NIF("monitor_receive_device",1,nif_monitor_receive_device)		\
    NIF("device_new_from_syspath",2,nif_device_new_from_syspath)	\
    NIF("device_new_from_devnum",2,nif_device_new_from_devnum)		\
    NIF("device_new_from_subsystem_sysname",3,nif_device_new_from_subsystem_sysname) \
    NIF("device_new_from_device_id",2,nif_device_new_from_device_id)	\
    NIF("device_new_from_environment",1,nif_device_new_from_environment) \
    NIF("device_get_parent",1,nif_device_get_parent)			\
    NIF("device_get_parent_with_subsystem_devtype",3,nif_device_get_parent_with_subsystem_devtype) \
    NIF("device_get_devpath", 1, nif_device_get_devpath)		\
    NIF("device_get_subsystem", 1, nif_device_get_subsystem)		\
    NIF("device_get_devtype", 1, nif_device_get_devtype)		\
    NIF("device_get_syspath", 1, nif_device_get_syspath)		\
    NIF("device_get_sysname", 1, nif_device_get_sysname)		\
    NIF("device_get_sysnum", 1, nif_device_get_sysnum)			\
    NIF("device_get_devnode", 1, nif_device_get_devnode)		\
    NIF("device_get_is_initialized", 1, nif_device_get_is_initialized)	\
    NIF("device_get_driver", 1, nif_device_get_driver)			\
    NIF("device_get_devnum", 1, nif_device_get_devnum)			\
    NIF("device_get_action", 1, nif_device_get_action)			\
    NIF("device_get_seqnum", 1, nif_device_get_seqnum)			\
    NIF("device_get_usec_since_initialized", 1, nif_device_get_usec_since_initialized) \
    NIF("device_get_sysattr_value", 2, nif_device_get_sysattr_value)	\
    NIF("device_set_sysattr_value", 3, nif_device_set_sysattr_value)	\
    NIF("device_has_tag", 2, nif_device_has_tag)			\
    NIF("device_get_devlinks", 1, nif_device_get_devlinks)		\
    NIF("device_get_properties", 1, nif_device_get_properties)		\
    NIF("device_get_tags", 1, nif_device_get_tags)			\
    NIF("device_get_sysattrs", 1, nif_device_get_sysattrs)		\
    NIF("device_get_property_value", 2, nif_device_get_property_value)	\
    NIF("enumerate_new",1,nif_enumerate_new)				\
    NIF("enumerate_add_match_subsystem",2,nif_enumerate_add_match_subsystem) \
    NIF("enumerate_add_nomatch_subsystem",2,nif_enumerate_add_nomatch_subsystem) \
    NIF("enumerate_add_match_sysattr",3,nif_enumerate_add_match_sysattr) \
    NIF("enumerate_add_nomatch_sysattr",3,nif_enumerate_add_nomatch_sysattr) \
    NIF("enumerate_add_match_property",3,nif_enumerate_add_match_property) \
    NIF("enumerate_add_match_sysname",2,nif_enumerate_add_match_sysname) \
    NIF("enumerate_add_match_tag",2,nif_enumerate_add_match_tag)	\
    NIF("enumerate_add_match_parent",2,nif_enumerate_add_match_parent)	\
    NIF("enumerate_add_match_is_initialized",1,nif_enumerate_add_match_is_initialized) \
    NIF("enumerate_add_syspath",2,nif_enumerate_add_syspath)		\
    NIF("enumerate_get_devices",1,nif_enumerate_get_devices)		\
    NIF("enumerate_get_subsystems",1,nif_enumerate_get_subsystems)

//
// Declare all nif functions
//

#ifdef NIF_TRACE
#define NIF(name, arity, func) \
    static ERL_NIF_TERM func(ErlNifEnv* env, int argc,const ERL_NIF_TERM argv[]); \
    static ERL_NIF_TERM trace##_##func##_##arity(ErlNifEnv* env, int argc,const ERL_NIF_TERM argv[]);
#else
#define NIF(name, arity, func) \
    static ERL_NIF_TERM func(ErlNifEnv* env, int argc,const ERL_NIF_TERM argv[]);
#endif

NIF_LIST
#undef NIF

//
// Create NIF table
//

#ifdef NIF_TRACE
#define NIF(name,arity,func) NIF_FUNC(name, arity, trace##_##func##_##arity),
#else
#define NIF(name,arity,func) NIF_FUNC(name, arity, func),
#endif

ErlNifFunc nif_udev_funcs[] =
{
    NIF_LIST
};
#undef NIF


static const char* udevobj_type_name[] =
{
    [UDEVOBJ_TYPE_UDEV] = "udev",
    [UDEVOBJ_TYPE_DEVICE] = "device",
    [UDEVOBJ_TYPE_MONITOR] = "monitor", 
    [UDEVOBJ_TYPE_ENUMERATE] = "enumerate",
    [UDEVOBJ_TYPE_QUEUE] = "queue",
    [UDEVOBJ_TYPE_HWDB] = "hwdb"
};

static const ERL_NIF_TERM* udevobj_type[] =
{
    [UDEVOBJ_TYPE_UDEV] = &ATOM(udev),
    [UDEVOBJ_TYPE_DEVICE] = &ATOM(device),
    [UDEVOBJ_TYPE_MONITOR] = &ATOM(monitor), 
    [UDEVOBJ_TYPE_ENUMERATE] = &ATOM(enumerate),
    [UDEVOBJ_TYPE_QUEUE] = &ATOM(queue),
    [UDEVOBJ_TYPE_HWDB] = &ATOM(hwdb)
};



static ERL_NIF_TERM make_error(ErlNifEnv* env, int e)
{
    return enif_make_tuple2(env,
			    ATOM(error),
			    enif_make_atom(env, erl_errno_id(e)));
}

// For now, wrap the resource object {type,pointer-val,handle}
static ERL_NIF_TERM make_object(ErlNifEnv* env,
				udevobj_t* obj)
{
    return enif_make_tuple3(env,
			    get_udevobj_type(obj->type),
			    enif_make_ulong(env, (unsigned long) obj),
			    enif_make_resource(env, (void*)obj));
}

static int get_object(ErlNifEnv* env, ERL_NIF_TERM term,
		      udevobj_type_t type,
		      void** objp)
{
    int arity;
    ErlNifUInt64 ptr;
    const ERL_NIF_TERM* elem;
    
    if (!enif_get_tuple(env, term, &arity, &elem))
	return 0;
    if (arity != 3)
	return 0;
    switch(type) {
    case UDEVOBJ_TYPE_UDEV:
	if (elem[0] != ATOM(udev)) return 0;
	break;
    case UDEVOBJ_TYPE_DEVICE:
	if (elem[0] != ATOM(device)) return 0;
	break;		
    case UDEVOBJ_TYPE_MONITOR:
	if (elem[0] != ATOM(monitor)) return 0;
	break;
    case UDEVOBJ_TYPE_ENUMERATE:
	if (elem[0] != ATOM(enumerate)) return 0;
	break;
    case UDEVOBJ_TYPE_QUEUE:
	if (elem[0] != ATOM(queue)) return 0;
	break;
    case UDEVOBJ_TYPE_HWDB:
	if (elem[0] != ATOM(hwdb)) return 0;
	break;
    }
    if (!enif_get_uint64(env,elem[1],&ptr)) return 0;
    if (!enif_get_resource(env,elem[2],nif_udev_res, objp)) return 0;
    if (ptr != (ErlNifUInt64) *objp) return 0;
    return 1;
}

// FIXME: add iolist/binary support somehow
int get_string(ErlNifEnv* env, ERL_NIF_TERM arg, char* buf, size_t buf_size,
	       char** pstr)
{
    int n;
    if ((arg == ATOM(null)) || (arg == ATOM(undefined))) {
	*pstr = NULL;
	return 1;
    }
    if ((n = enif_get_string(env, arg, buf, buf_size, ERL_NIF_LATIN1)) == 0) {
	if (!enif_get_atom(env, arg, buf, buf_size, ERL_NIF_LATIN1))
	    return 0;
	*pstr = buf;
	return 1;
    }
    else if (n < 0)
	return 0;
    *pstr = buf;    
    return 1;
}

static void obj_unref(void* ptr, udevobj_type_t type)
{
    DBG("unref %s %p\r\n", udevobj_type_name[type], ptr);    
    switch(type) {
    case UDEVOBJ_TYPE_UDEV: udev_unref(ptr); break;
    case UDEVOBJ_TYPE_DEVICE: udev_device_unref(ptr); break;
    case UDEVOBJ_TYPE_MONITOR: udev_monitor_unref(ptr); break;
    case UDEVOBJ_TYPE_ENUMERATE: udev_enumerate_unref(ptr); break;
    case UDEVOBJ_TYPE_QUEUE: udev_queue_unref(ptr); break;       
    case UDEVOBJ_TYPE_HWDB: udev_hwdb_unref(ptr); 	break;
    default: break;
    }
}

static udevobj_t* alloc_object(void* ptr, udevobj_type_t type)
{
    udevobj_t* obj;

    if ((obj = enif_alloc_resource(nif_udev_res, sizeof(udevobj_t))) == NULL) {
	obj_unref(obj, type);
	return NULL;
    }
    obj->type = type;
    obj->ptr = ptr;
    DBG("alloc %s %p\r\n", udevobj_type_name[type], ptr);
    return obj;
}

static void nif_udev_dtor(ErlNifEnv* env, udevobj_t* obj)
{
    UNUSED(env);
    DBG("dtor %s %p\r\n", udevobj_type_name[obj->type], obj->ptr);
    obj_unref(obj->ptr, obj->type);
}

static void nif_udev_stop(ErlNifEnv* env, udevobj_t* obj,
			  ErlNifEvent event, int is_direct_call)
{
    UNUSED(env);
    DBG("stop: fd=%d, type=%d, ptr=%p\r\n",
	(int)event, obj->type, obj->ptr);
}

static void nif_udev_down(ErlNifEnv* env, udevobj_t* obj,
			  const ErlNifPid* pid, const ErlNifMonitor* mon)
{
    UNUSED(env);
    DBG("down: type=%d, ptr=%p\r\n", obj->type, obj->ptr);
}


static ERL_NIF_TERM nif_new(ErlNifEnv* env, int argc,
			    const ERL_NIF_TERM argv[])
{
    udevobj_t* obj;
    struct udev* ptr;
    ERL_NIF_TERM t;
    
    if ((ptr = udev_new()) == NULL)
	return enif_make_badarg(env);
    if ((obj = alloc_object(ptr, UDEVOBJ_TYPE_UDEV)) == NULL)
	return enif_make_badarg(env);
    t = make_object(env,obj);
    enif_release_resource(obj);
    return t;
}

static ERL_NIF_TERM nif_monitor_new_from_netlink(ErlNifEnv* env, int argc,
						 const ERL_NIF_TERM argv[])
{
    struct udev_monitor* ptr;    
    udevobj_t* udev_obj;
    udevobj_t* obj;
    ERL_NIF_TERM t;    
    char name[MAX_STRING_SIZE];
    char* name_ptr;
    
    if (!get_object(env, argv[0], UDEVOBJ_TYPE_UDEV, (void**)&udev_obj))
	return enif_make_badarg(env);
    if (!get_string(env, argv[1], name, sizeof(name), &name_ptr))
	return enif_make_badarg(env);
    if (!(ptr = udev_monitor_new_from_netlink(udev_obj->udev, name_ptr)))
	return enif_make_badarg(env);
    if ((obj = alloc_object(ptr, UDEVOBJ_TYPE_MONITOR)) == NULL)
	return enif_make_badarg(env);
    t = make_object(env,obj);
    enif_release_resource(obj);
    return t;    
}

static ERL_NIF_TERM nif_monitor_filter_add_match_subsystem_devtype(
    ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    udevobj_t* mon;
    char subsystem[MAX_STRING_SIZE];
    char* subsystem_ptr;
    char devtype[MAX_STRING_SIZE];
    char* devtype_ptr;
    
    if (!get_object(env,argv[0], UDEVOBJ_TYPE_MONITOR, (void**)&mon))
	return enif_make_badarg(env);
    if (!get_string(env,argv[1],subsystem,sizeof(subsystem),&subsystem_ptr))
	return enif_make_badarg(env);
    if (!get_string(env,argv[2],devtype,sizeof(devtype),&devtype_ptr))
	return enif_make_badarg(env);
    if (udev_monitor_filter_add_match_subsystem_devtype(mon->monitor,
							subsystem_ptr,
							devtype_ptr) < 0)
	return enif_make_badarg(env);
    return ATOM(ok);
}

static ERL_NIF_TERM nif_monitor_filter_add_match_tag(
    ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    udevobj_t* mon;
    char tag[MAX_STRING_SIZE];
    char* tag_ptr;
    
    if (!get_object(env, argv[0], UDEVOBJ_TYPE_MONITOR, (void**)&mon))
	return enif_make_badarg(env);
    if (!get_string(env, argv[1], tag, sizeof(tag), &tag_ptr))
	return enif_make_badarg(env);
    if (udev_monitor_filter_add_match_tag(mon->monitor, tag_ptr) < 0)
	return enif_make_badarg(env);
    return ATOM(ok);    
}

static ERL_NIF_TERM nif_monitor_filter_update(
    ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    udevobj_t* mon;

    if (!get_object(env, argv[0], UDEVOBJ_TYPE_MONITOR, (void**)&mon))
	return enif_make_badarg(env);
    if (udev_monitor_filter_update(mon->monitor) < 0)
	return enif_make_badarg(env);
    return ATOM(ok);
}

static ERL_NIF_TERM nif_monitor_filter_remove(
    ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    udevobj_t* mon;

    if (!get_object(env, argv[0], UDEVOBJ_TYPE_MONITOR, (void**)&mon))
	return enif_make_badarg(env);
    if (udev_monitor_filter_remove(mon->monitor) < 0)
	return enif_make_badarg(env);
    return ATOM(ok);    
}

static ERL_NIF_TERM nif_monitor_enable_receiving(
    ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    udevobj_t* mon;
    
    if (!get_object(env, argv[0], UDEVOBJ_TYPE_MONITOR, (void**)&mon))
	return enif_make_badarg(env);
    if (udev_monitor_enable_receiving(mon->monitor) < 0)
	return enif_make_badarg(env);
    return ATOM(ok);
}

static ERL_NIF_TERM nif_select(
    ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    udevobj_t* mon;
    int fd;
    int r;
    ERL_NIF_TERM ref = ATOM(undefined);
    ERL_NIF_TERM msg;
    ErlNifEnv* msg_env;
    
    if (!get_object(env, argv[0], UDEVOBJ_TYPE_MONITOR, (void**)&mon))
	return enif_make_badarg(env);
    if (argc > 1) {
	if (!enif_is_ref(env, argv[1]))
	    return enif_make_badarg(env);
	ref = argv[1];
    }
    if ((fd = udev_monitor_get_fd(mon->monitor)) < 0)
	return enif_make_badarg(env);
    DBG("select fd=%d\r\n", fd);
    // create msg = {select,{monitor,Obj,Ptr},Ref,ready_input}
    msg_env = enif_alloc_env();
    if (ref != ATOM(undefined))
	ref = enif_make_copy(msg_env, ref);
    msg = enif_make_tuple4(msg_env,
			   ATOM(select),
			   make_object(msg_env,mon),
			   ref,
			   ATOM(ready_input));
    if ((r = enif_select_read(env, (ErlNifEvent)((long)fd), mon, 
			      NULL, msg, msg_env)) < 0) {
	DBG("select error r=%d\r\n", r);
	enif_free_env(msg_env);
	if (r & ERL_NIF_SELECT_INVALID_EVENT)
	    return enif_make_tuple2(env, ATOM(error), ATOM(invalid));
	else if (r & ERL_NIF_SELECT_FAILED)
	    return enif_make_tuple2(env, ATOM(error), ATOM(failed));
	else
	    return make_error(env, errno);
    }
    else if (r > 0) {
	DBG("select cancel r=%d\r\n", r);
	return ATOM(false);
    }
    return ATOM(select); 
}

static ERL_NIF_TERM nif_monitor_receive_device(
    ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    udevobj_t* mon;
    udevobj_t* obj;
    struct udev_device* ptr;
    ERL_NIF_TERM t;
    
    if (!get_object(env, argv[0], UDEVOBJ_TYPE_MONITOR, (void**)&mon))
	return enif_make_badarg(env);
    if (!(ptr = udev_monitor_receive_device(mon->monitor)))
	return enif_make_badarg(env);
    if ((obj = alloc_object(ptr, UDEVOBJ_TYPE_DEVICE)) == NULL)
	return enif_make_badarg(env);
    t = make_object(env,obj);
    enif_release_resource(obj);
    return t;
}


static ERL_NIF_TERM device_get_string(
    ErlNifEnv* env, ERL_NIF_TERM arg,
    const char* (*getstr)(struct udev_device*))
{
    udevobj_t* dev;
    const char* str;
    
    if (!get_object(env, arg, UDEVOBJ_TYPE_DEVICE, (void**)&dev))
	return enif_make_badarg(env);
    if ((str = (*getstr)(dev->device)) == NULL)
	return ATOM(undefined);
    return enif_make_string(env, str, ERL_NIF_LATIN1); 
}

static ERL_NIF_TERM get_udev_list(ErlNifEnv* env,
				  struct udev_list_entry *ent,
				  int want_value)
{
    ERL_NIF_TERM list = enif_make_list(env, 0);

    while(ent) {
	const char* name;
	ERL_NIF_TERM elem;
	ERL_NIF_TERM name_term;	
	
	name = udev_list_entry_get_name(ent);
	name_term = enif_make_string(env, name, ERL_NIF_LATIN1);
	if (want_value) {
	    const char* value;
	    ERL_NIF_TERM value_term;
	    
	    if ((value = udev_list_entry_get_value(ent)) == NULL)
		value_term = ATOM(undefined);
	    else
		value_term = enif_make_string(env, value, ERL_NIF_LATIN1);
	    elem = enif_make_tuple2(env, name_term, value_term);
	}
	else {
	    elem = name_term;
	}
	list = enif_make_list_cell(env, elem, list);
	ent = udev_list_entry_get_next(ent);
    }
    return list;    
}
				 
static ERL_NIF_TERM device_get_udev_list(
    ErlNifEnv* env, ERL_NIF_TERM arg, int want_value,
    struct udev_list_entry * (*getlist)(struct udev_device*))
{
    udevobj_t* dev;    

    if (!get_object(env, arg, UDEVOBJ_TYPE_DEVICE, (void**)&dev))
	return enif_make_badarg(env);
    return get_udev_list(env, getlist(dev->device), want_value);
}

static ERL_NIF_TERM nif_device_new_from_syspath(
    ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    udevobj_t* udev;
    udevobj_t* obj;
    struct udev_device* ptr;
    ERL_NIF_TERM t;
    char syspath[MAX_STRING_SIZE];
    char* syspath_ptr;

    if (!get_object(env, argv[0], UDEVOBJ_TYPE_UDEV, (void**)&udev))
	return enif_make_badarg(env);    
    if (!get_string(env,argv[1],syspath,sizeof(syspath),&syspath_ptr))
	return enif_make_badarg(env);
    if (!(ptr = udev_device_new_from_syspath(udev->udev, syspath_ptr)))
	return ATOM(undefined);
    if ((obj = alloc_object(ptr, UDEVOBJ_TYPE_DEVICE)) == NULL)
	return enif_make_badarg(env);
    t = make_object(env,obj);
    enif_release_resource(obj);
    return t;
}

static ERL_NIF_TERM nif_device_new_from_devnum(
    ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    udevobj_t* udev;
    udevobj_t* obj;
    struct udev_device* ptr;
    ERL_NIF_TERM t;
    int type;
    long num;
    
    if (!get_object(env, argv[0], UDEVOBJ_TYPE_UDEV, (void**)&udev))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[1], &type))
	return enif_make_badarg(env);
    if (!enif_get_long(env, argv[2], &num))
	return enif_make_badarg(env);
    if (!(ptr = udev_device_new_from_devnum(udev->udev,(char)type,(dev_t)num)))
	return ATOM(undefined);
    if ((obj = alloc_object(ptr, UDEVOBJ_TYPE_DEVICE)) == NULL)
	return enif_make_badarg(env);
    t = make_object(env,obj);
    enif_release_resource(obj);
    return t;    
}

static ERL_NIF_TERM nif_device_new_from_subsystem_sysname(
    ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    udevobj_t* udev;
    udevobj_t* obj;
    struct udev_device* ptr;
    ERL_NIF_TERM t;    
    char subsystem[MAX_STRING_SIZE];
    char* subsystem_ptr;
    char sysname[MAX_STRING_SIZE];
    char* sysname_ptr;
    
    if (!get_object(env, argv[0], UDEVOBJ_TYPE_UDEV, (void**)&udev))
	return enif_make_badarg(env);
    if (!get_string(env,argv[1],subsystem,sizeof(subsystem),&subsystem_ptr))
	return enif_make_badarg(env);
    if (!get_string(env,argv[2],sysname,sizeof(sysname),&sysname_ptr))
	return enif_make_badarg(env);
    if (!(ptr = udev_device_new_from_subsystem_sysname(udev->udev, subsystem_ptr, sysname_ptr)))
	return ATOM(undefined);
    if ((obj = alloc_object(ptr, UDEVOBJ_TYPE_DEVICE)) == NULL)
	return enif_make_badarg(env);
    t = make_object(env,obj);
    enif_release_resource(obj);
    return t;    
}

static ERL_NIF_TERM nif_device_new_from_device_id(
    ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    udevobj_t* udev;
    udevobj_t* obj;
    struct udev_device* ptr;
    ERL_NIF_TERM t;
    char devid[MAX_STRING_SIZE];
    char* devid_ptr;
    
    if (!get_object(env, argv[0], UDEVOBJ_TYPE_UDEV, (void**)&udev))
	return enif_make_badarg(env);
    if (!get_string(env,argv[1],devid,sizeof(devid),&devid_ptr))
	return enif_make_badarg(env);
    if (!(ptr = udev_device_new_from_device_id(udev->udev, devid_ptr)))
	return ATOM(undefined);
    if ((obj = alloc_object(ptr, UDEVOBJ_TYPE_DEVICE)) == NULL)
	return enif_make_badarg(env);
    t = make_object(env,obj);
    enif_release_resource(obj);
    return t;
}

static ERL_NIF_TERM nif_device_new_from_environment(
    ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    udevobj_t* udev;
    udevobj_t* obj;
    struct udev_device* ptr;
    ERL_NIF_TERM t;
    
    if (!get_object(env, argv[0], UDEVOBJ_TYPE_UDEV, (void**)&udev))
	return enif_make_badarg(env);
    if (!(ptr = udev_device_new_from_environment(udev->udev)))
	return ATOM(undefined);
    if ((obj = alloc_object(ptr, UDEVOBJ_TYPE_DEVICE)) == NULL)
	return enif_make_badarg(env);
    t = make_object(env,obj);
    enif_release_resource(obj);
    return t;            
}

static ERL_NIF_TERM nif_device_get_parent(
    ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    udevobj_t* obj;
    udevobj_t* dev;
    struct udev_device *parent;
    ERL_NIF_TERM t;
    
    if (!get_object(env, argv[0], UDEVOBJ_TYPE_DEVICE, (void**)&dev))
	return enif_make_badarg(env);
    if ((parent = udev_device_get_parent(dev->device)) == NULL)
	return ATOM(false);
    udev_device_ref(parent);  // get_parent does not add a ref!
    if ((obj = alloc_object(parent, UDEVOBJ_TYPE_DEVICE)) == NULL)
	return enif_make_badarg(env);
    t = make_object(env,obj);
    enif_release_resource(obj);
    return t;
}

static ERL_NIF_TERM nif_device_get_parent_with_subsystem_devtype(
    ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    udevobj_t* obj;
    udevobj_t* dev;
    struct udev_device *parent;
    ERL_NIF_TERM t;
    char subsystem[MAX_STRING_SIZE];
    char* subsystem_ptr;
    char devtype[MAX_STRING_SIZE];
    char* devtype_ptr;
    
    if (!get_object(env, argv[0], UDEVOBJ_TYPE_DEVICE, (void**)&dev))
	return enif_make_badarg(env);
    if (!get_string(env,argv[1],subsystem,sizeof(subsystem),&subsystem_ptr))
	return enif_make_badarg(env);
    if (!get_string(env,argv[2],devtype,sizeof(devtype),&devtype_ptr))
	return enif_make_badarg(env);
    if ((parent = udev_device_get_parent_with_subsystem_devtype(dev->device,subsystem_ptr,devtype_ptr)) == NULL)
	return ATOM(false);
    udev_device_ref(parent);  // get_parent does not add a ref!
    if ((obj = alloc_object(parent, UDEVOBJ_TYPE_DEVICE)) == NULL)
	return enif_make_badarg(env);
    t = make_object(env,obj);
    enif_release_resource(obj);
    return t;    
}


static ERL_NIF_TERM nif_device_get_devpath(
    ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    return device_get_string(env, argv[0], udev_device_get_devpath);
}

static ERL_NIF_TERM nif_device_get_subsystem(
    ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    return device_get_string(env, argv[0], udev_device_get_subsystem);
}

static ERL_NIF_TERM nif_device_get_devtype(
    ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    return device_get_string(env, argv[0], udev_device_get_devtype);
}

static ERL_NIF_TERM nif_device_get_syspath(
    ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    return device_get_string(env, argv[0], udev_device_get_syspath);
}

static ERL_NIF_TERM nif_device_get_sysname(
    ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    return device_get_string(env, argv[0], udev_device_get_sysname);    
}

static ERL_NIF_TERM nif_device_get_sysnum(
    ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    return device_get_string(env, argv[0], udev_device_get_sysnum);    
}

static ERL_NIF_TERM nif_device_get_devnode(
    ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    return device_get_string(env, argv[0], udev_device_get_devnode);
}

static ERL_NIF_TERM nif_device_get_is_initialized(
    ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    udevobj_t* dev;
    
    if (!get_object(env, argv[0], UDEVOBJ_TYPE_DEVICE, (void**)&dev))
	return enif_make_badarg(env);
    if (udev_device_get_is_initialized(dev->device))
	return ATOM(true);
    return ATOM(false);
}

static ERL_NIF_TERM nif_device_get_driver(
    ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    return device_get_string(env, argv[0], udev_device_get_driver);
}


static ERL_NIF_TERM nif_device_get_devnum(
    ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    udevobj_t* dev;
    dev_t num;

    if (!get_object(env, argv[0], UDEVOBJ_TYPE_DEVICE, (void**)&dev))
	return enif_make_badarg(env);
    num = udev_device_get_devnum(dev->device);
    return enif_make_long(env, num);
}

static ERL_NIF_TERM nif_device_get_action(
    ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    return device_get_string(env, argv[0], udev_device_get_action);    
}

static ERL_NIF_TERM nif_device_get_seqnum(
    ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    udevobj_t* dev;
    ErlNifUInt64 seq;

    if (!get_object(env, argv[0], UDEVOBJ_TYPE_DEVICE, (void**)&dev))
	return enif_make_badarg(env);
    seq = udev_device_get_seqnum(dev->device);
    return enif_make_uint64(env, seq);
}

static ERL_NIF_TERM nif_device_get_usec_since_initialized(
    ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    udevobj_t* dev;
    ErlNifUInt64 usec;

    if (!get_object(env, argv[0], UDEVOBJ_TYPE_DEVICE, (void**)&dev))
	return enif_make_badarg(env);
    usec = udev_device_get_usec_since_initialized(dev->device);
    return enif_make_uint64(env, usec);
}

static ERL_NIF_TERM nif_device_get_sysattr_value(
    ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    udevobj_t* dev;
    const char* str;    
    char sysattr[MAX_STRING_SIZE];
    char* sysattr_ptr;
    
    if (!get_object(env, argv[0], UDEVOBJ_TYPE_DEVICE, (void**)&dev))
	return enif_make_badarg(env);
    if (!get_string(env, argv[1], sysattr, sizeof(sysattr), &sysattr_ptr))
	return enif_make_badarg(env);
    if ((str = udev_device_get_sysattr_value(dev->device, sysattr_ptr)) == NULL)
	return ATOM(undefined);
    return enif_make_string(env, str, ERL_NIF_LATIN1);
}

static ERL_NIF_TERM nif_device_set_sysattr_value(
    ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    udevobj_t* dev;
    char sysattr[MAX_STRING_SIZE];
    char* sysattr_ptr;
    char value[MAX_STRING_SIZE];
    char* value_ptr;    
    
    if (!get_object(env, argv[0], UDEVOBJ_TYPE_DEVICE, (void**)&dev))
	return enif_make_badarg(env);
    if (!get_string(env, argv[1], sysattr, sizeof(sysattr), &sysattr_ptr))
	return enif_make_badarg(env);
    if (!get_string(env, argv[2], value, sizeof(value), &value_ptr))
	return enif_make_badarg(env);    
    if (udev_device_set_sysattr_value(dev->device, sysattr_ptr, value_ptr) < 0)
	return enif_make_badarg(env);	
    return ATOM(ok);
}

static ERL_NIF_TERM nif_device_has_tag(
    ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    udevobj_t* dev;
    char tag[MAX_STRING_SIZE];
    char* tag_ptr;
    
    if (!get_object(env, argv[0], UDEVOBJ_TYPE_DEVICE, (void**)&dev))
	return enif_make_badarg(env);
    if (!get_string(env, argv[1], tag, sizeof(tag), &tag_ptr))
	return enif_make_badarg(env);    
    if (udev_device_has_tag(dev->device, tag_ptr))
	return ATOM(true);
    return ATOM(false);
}

static ERL_NIF_TERM nif_device_get_devlinks(
    ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    return device_get_udev_list(env, argv[0], 0,
				udev_device_get_devlinks_list_entry);
}

static ERL_NIF_TERM nif_device_get_properties(
    ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    return device_get_udev_list(env, argv[0], 1,
				udev_device_get_properties_list_entry);
}

static ERL_NIF_TERM nif_device_get_tags(
    ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    return device_get_udev_list(env, argv[0], 0,
				udev_device_get_tags_list_entry);
}

static ERL_NIF_TERM nif_device_get_sysattrs(
    ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    return device_get_udev_list(env, argv[0], 0,
				udev_device_get_sysattr_list_entry);
}

static ERL_NIF_TERM nif_device_get_property_value(
    ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    udevobj_t* dev;
    const char* str;    
    char prop[MAX_STRING_SIZE];
    char* prop_ptr;
    
    if (!get_object(env, argv[0], UDEVOBJ_TYPE_DEVICE, (void**)&dev))
	return enif_make_badarg(env);
    if (!get_string(env, argv[1], prop, sizeof(prop), &prop_ptr))
	return enif_make_badarg(env);
    if ((str = udev_device_get_property_value(dev->device, prop_ptr)) == NULL)
	return ATOM(undefined);
    return enif_make_string(env, str, ERL_NIF_LATIN1);
}

static ERL_NIF_TERM nif_enumerate_new(
    ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    udevobj_t* udev;
    struct udev_enumerate* ptr;
    udevobj_t* obj;
    ERL_NIF_TERM t;
    
    if (!get_object(env, argv[0], UDEVOBJ_TYPE_UDEV, (void**)&udev))
	return enif_make_badarg(env);
    if (!(ptr = udev_enumerate_new(udev->udev)))
	return enif_make_badarg(env);	
    if ((obj = alloc_object(ptr, UDEVOBJ_TYPE_ENUMERATE)) == NULL)
	return enif_make_badarg(env);
    t = make_object(env,obj);
    enif_release_resource(obj);
    return t;
}

static ERL_NIF_TERM nif_enumerate_add_match_subsystem(
    ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    udevobj_t* uenum;
    char subsystem[MAX_STRING_SIZE];
    char* subsystem_ptr;
    
    if (!get_object(env, argv[0], UDEVOBJ_TYPE_ENUMERATE, (void**)&uenum))
	return enif_make_badarg(env);
    if (!get_string(env, argv[1], subsystem, sizeof(subsystem), &subsystem_ptr))
	return enif_make_badarg(env);
    if (udev_enumerate_add_match_subsystem(uenum->enumerate, subsystem_ptr) < 0)
	return make_error(env, errno);
    return ATOM(ok);                                
}

static ERL_NIF_TERM nif_enumerate_add_nomatch_subsystem(
    ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    udevobj_t* uenum;
    char subsystem[MAX_STRING_SIZE];
    char* subsystem_ptr;
    
    if (!get_object(env, argv[0], UDEVOBJ_TYPE_ENUMERATE, (void**)&uenum))
	return enif_make_badarg(env);
    if (!get_string(env, argv[1], subsystem, sizeof(subsystem), &subsystem_ptr))
	return enif_make_badarg(env);
    if (udev_enumerate_add_nomatch_subsystem(uenum->enumerate, subsystem_ptr) < 0)
	return make_error(env, errno);
    return ATOM(ok);                            
}

static ERL_NIF_TERM nif_enumerate_add_match_sysattr(
    ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    udevobj_t* uenum;
    char sysattr[MAX_STRING_SIZE];
    char* sysattr_ptr;
    char value[MAX_STRING_SIZE];
    char* value_ptr;
    
    if (!get_object(env, argv[0], UDEVOBJ_TYPE_ENUMERATE, (void**)&uenum))
	return enif_make_badarg(env);
    if (!get_string(env, argv[1], sysattr, sizeof(sysattr), &sysattr_ptr))
	return enif_make_badarg(env);
    if (!get_string(env, argv[2], value, sizeof(value), &value_ptr))
	return enif_make_badarg(env);
    if (udev_enumerate_add_match_sysattr(uenum->enumerate, sysattr_ptr, value_ptr) < 0)
	return make_error(env, errno);
    return ATOM(ok);                        
}

static ERL_NIF_TERM nif_enumerate_add_nomatch_sysattr(
    ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    udevobj_t* uenum;
    char sysattr[MAX_STRING_SIZE];
    char* sysattr_ptr;    
    char value[MAX_STRING_SIZE];
    char* value_ptr;
    
    if (!get_object(env, argv[0], UDEVOBJ_TYPE_ENUMERATE, (void**)&uenum))
	return enif_make_badarg(env);
    if (!get_string(env, argv[1], sysattr, sizeof(sysattr), &sysattr_ptr))
	return enif_make_badarg(env);
    if (!get_string(env, argv[2], value, sizeof(value), &value_ptr))
	return enif_make_badarg(env);
    if (udev_enumerate_add_nomatch_sysattr(uenum->enumerate, sysattr_ptr, value_ptr) < 0)
	return make_error(env, errno);
    return ATOM(ok);                    
}

static ERL_NIF_TERM nif_enumerate_add_match_property(
    ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    udevobj_t* uenum;
    char prop[MAX_STRING_SIZE];
    char* prop_ptr;
    char value[MAX_STRING_SIZE];
    char* value_ptr;
    
    if (!get_object(env, argv[0], UDEVOBJ_TYPE_ENUMERATE, (void**)&uenum))
	return enif_make_badarg(env);
    if (!get_string(env, argv[1], prop, sizeof(prop), &prop_ptr))
	return enif_make_badarg(env);
    if (!get_string(env, argv[2], value, sizeof(value), &value_ptr))
	return enif_make_badarg(env);
    if (udev_enumerate_add_match_property(uenum->enumerate, prop_ptr, value_ptr) < 0)
	return make_error(env, errno);
    return ATOM(ok);                
}

static ERL_NIF_TERM nif_enumerate_add_match_sysname(
    ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    udevobj_t* uenum;
    char sysname[MAX_STRING_SIZE];
    char* sysname_ptr;
    
    if (!get_object(env, argv[0], UDEVOBJ_TYPE_ENUMERATE, (void**)&uenum))
	return enif_make_badarg(env);
    if (!get_string(env, argv[1], sysname, sizeof(sysname), &sysname_ptr))
	return enif_make_badarg(env);
    if (udev_enumerate_add_match_sysname(uenum->enumerate, sysname_ptr) < 0)
	return make_error(env, errno);
    return ATOM(ok);            
}

static ERL_NIF_TERM nif_enumerate_add_match_tag(
    ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    udevobj_t* uenum;
    char tag[MAX_STRING_SIZE];
    char* tag_ptr;
    
    if (!get_object(env, argv[0], UDEVOBJ_TYPE_ENUMERATE, (void**)&uenum))
	return enif_make_badarg(env);
    if (!get_string(env, argv[1], tag, sizeof(tag), &tag_ptr))
	return enif_make_badarg(env);
    if (udev_enumerate_add_match_tag(uenum->enumerate, tag_ptr) < 0)
	return make_error(env, errno);
    return ATOM(ok);        
}

static ERL_NIF_TERM nif_enumerate_add_match_parent(
    ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    udevobj_t* uenum;
    udevobj_t* dev;    
    
    if (!get_object(env, argv[0], UDEVOBJ_TYPE_ENUMERATE, (void**)&uenum))
	return enif_make_badarg(env);
    if (!get_object(env, argv[1], UDEVOBJ_TYPE_DEVICE, (void**)&dev))
	return enif_make_badarg(env);
    if (udev_enumerate_add_match_parent(uenum->enumerate, dev->device) < 0)
	return make_error(env, errno);
    return ATOM(ok);
}

static ERL_NIF_TERM nif_enumerate_add_match_is_initialized(
    ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    udevobj_t* uenum;
    
    if (!get_object(env, argv[0], UDEVOBJ_TYPE_ENUMERATE, (void**)&uenum))
	return enif_make_badarg(env);
    if (udev_enumerate_add_match_is_initialized(uenum->enumerate) < 0)
	return make_error(env, errno);
    return ATOM(ok);
}

static ERL_NIF_TERM nif_enumerate_add_syspath(
    ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    udevobj_t* uenum;
    char syspath[MAX_STRING_SIZE];
    char* syspath_ptr;
    
    if (!get_object(env, argv[0], UDEVOBJ_TYPE_ENUMERATE, (void**)&uenum))
	return enif_make_badarg(env);
    if (!get_string(env, argv[1], syspath, sizeof(syspath), &syspath_ptr))
	return enif_make_badarg(env);
    if (udev_enumerate_add_syspath(uenum->enumerate, syspath_ptr) < 0)
	return make_error(env, errno);
    return ATOM(ok);
}

static ERL_NIF_TERM nif_enumerate_get_devices(
    ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    udevobj_t* uenum;
    struct udev_list_entry *ent;
    
    // run search and get matching list
    if (!get_object(env, argv[0], UDEVOBJ_TYPE_ENUMERATE, (void**)&uenum))
	return enif_make_badarg(env);
    if (udev_enumerate_scan_devices(uenum->enumerate) < 0)
	return enif_make_badarg(env);	
    ent = udev_enumerate_get_list_entry(uenum->enumerate);
    return get_udev_list(env, ent, 0);
}

static ERL_NIF_TERM nif_enumerate_get_subsystems(
    ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    udevobj_t* uenum;
    struct udev_list_entry *ent;
    
    // run search and get matching list
    if (!get_object(env, argv[0], UDEVOBJ_TYPE_ENUMERATE, (void**)&uenum))
	return enif_make_badarg(env);
    if (udev_enumerate_scan_subsystems(uenum->enumerate) < 0)
	return enif_make_badarg(env);
    ent = udev_enumerate_get_list_entry(uenum->enumerate);
    return get_udev_list(env, ent, 0);
}

// create all tracing NIFs
#ifdef NIF_TRACE

int enif_print(FILE* out, ERL_NIF_TERM term)
{
    return enif_fprintf(out, "%T", term);
}

static void trace_print_arg_list(ErlNifEnv* env,int argc,const ERL_NIF_TERM argv[])
{
    enif_fprintf(stdout, "(");
    if (argc > 0) {
	int i;
	if (enif_is_ref(env, argv[0]))
	    enif_fprintf(stdout, "$VP");
	else
	    enif_print(stdout, argv[0]);
	for (i = 1; i < argc; i++) {
	    enif_fprintf(stdout, ",");
	    enif_print(stdout, argv[i]);
	}
    }
    enif_fprintf(stdout, ")");
}

#define NIF(name, arity, func) \
static ERL_NIF_TERM trace##_##func##_##arity(ErlNifEnv* env, int argc,const ERL_NIF_TERM argv[]) \
{ \
    ERL_NIF_TERM result;					\
    enif_fprintf(stdout, "ENTER %s", (name));			\
    trace_print_arg_list(env, argc, argv);			\
    enif_fprintf(stdout, "\r\n");				\
    result = func(env, argc, argv);				\
    enif_fprintf(stdout, "  RESULT=%T\r\n", (result));		\
    enif_fprintf(stdout, "LEAVE %s\r\n", (name));		\
    return result;						\
}

NIF_LIST
#undef NIF

#endif


static void load_atoms(ErlNifEnv* env)
{
    LOAD_ATOM(ok);
    LOAD_ATOM(true);
    LOAD_ATOM(false);
    LOAD_ATOM(undefined);
    LOAD_ATOM(error);
    LOAD_ATOM(null);
    LOAD_ATOM(select);
    LOAD_ATOM(failed);
    LOAD_ATOM(invalid);
    LOAD_ATOM(ready_input);
    LOAD_ATOM(ready_output);    
    
    // types
    LOAD_ATOM(udev);
    LOAD_ATOM(device);
    LOAD_ATOM(monitor);
    LOAD_ATOM(enumerate);
    LOAD_ATOM(queue);
    LOAD_ATOM(hwdb);
}

static int nif_udev_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    UNUSED(load_info);
    ErlNifResourceFlags tried;
    ErlNifResourceTypeInit cb;
    
    DBG("udev_nif_load\r\n");

    load_atoms(env);

    cb.dtor = (ErlNifResourceDtor*) nif_udev_dtor;
    cb.stop = (ErlNifResourceStop*) nif_udev_stop;
    cb.down = (ErlNifResourceDown*) nif_udev_down;
    
    nif_udev_res = enif_open_resource_type_x(env, "udev",
					     &cb, ERL_NIF_RT_CREATE, &tried);
    *priv_data = 0;
    return 0;    
}

static int nif_udev_upgrade(ErlNifEnv* env, void** priv_data,
			    void** old_priv_data,
			    ERL_NIF_TERM load_info)
{
    UNUSED(env);
    UNUSED(load_info);
    DBG("udev_nif_upgrade\r\n");
    *priv_data = *old_priv_data;
    return 0;
}

static void nif_udev_unload(ErlNifEnv* env, void* priv_data)
{
    UNUSED(env);
    UNUSED(priv_data);
    DBG("udev_nif_unload\r\n");
}


ERL_NIF_INIT(udev, nif_udev_funcs,
	     nif_udev_load,
	     NULL,
	     nif_udev_upgrade,
	     nif_udev_unload)
