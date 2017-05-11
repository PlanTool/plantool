
/***********************************************************************/
/**   microCLAIRE                                       Yves Caseau    */
/**   Kernel.cpp pp                                                    */
/**  Copyright (C) 1998-2003 Yves Caseau. All Rights Reserved.         */
/**  cf claire.h                                                       */
/***********************************************************************/

#include <claire.h>
#include <Kernel.h>
#include <marie.h>

// create the namespace
KernelClass Kernel;
NameSpace claire;
NameSpace mClaire;
ClaireBoolean *CTRUE = (ClaireBoolean *) 1;    // v3.2.04
ClaireBoolean *CFALSE = (ClaireBoolean *) 0;
OID CNULL;

ClaireResource *ClRes;
ClaireEnvironment *ClEnv;

// --------------------------------------------------------------
// this file contains the meta-description of the Kernel module
// ---------------------------------------------------------------

/*********************************************************************/
/** Contents                                                         */
/**    1. Bootstrap                                                  */
/**    2. metaLoad                                                   */
/*********************************************************************/


/*********************************************************************/
/**    1. Bootstrap                                                  */
/*********************************************************************/


// this is the bootstraping kernel ====================================================
// handcrafted ...


void KernelClass::bootstrap()
{CTRUE = (ClaireBoolean *) ClAlloc->makeAny(4);
 CFALSE = (ClaireBoolean *) ClAlloc->makeAny(4);
 CNULL = _oid_(ClAlloc->makeAny(4));
 ctrue = _oid_(CTRUE);
 cfalse = _oid_(CFALSE);
 NoDefault = (thing *) ClAlloc->makeAny(4);
 emptySet = set::empty();
 emptySet->of = emptySet;
 nil = list::empty();
 nil->of = emptySet;

 _integer = NULL;
 _float = NULL;

 // step 1 : define the object shells
 _class =  (ClaireClass *) ClAlloc->makeStatic(18);  // creates the 3 most basic class templates
 _set =  (ClaireClass *) ClAlloc->makeStatic(18);
 _list =  (ClaireClass *) ClAlloc->makeStatic(18);
 _class = ClaireClass::make(_class);              // now we can call make
 _class->comment = "class";
 _class->isa = _class;

 _symbol = ClaireClass::make(ClAlloc->makeStatic(18));
 _module = ClaireClass::make(ClAlloc->makeStatic(18));
 _module->comment = "module";
 _list = ClaireClass::make(_list);
 _set = ClaireClass::make(_set);
 emptySet->isa = _set;
 claire.it = (module *) _module->instantiate(10);
 mClaire.it = (module *) _module->instantiate(10);
 it = (module *) _module->instantiate(10);
 ClEnv->module_I = it;
 claire.it->part_of = NULL;
 claire.it->parts = list::alloc(1,_oid_(mClaire.it));
 claire.it->status = 3;
 claire.it->comment = "claire";
 claire.it->made_of = list::empty();
 claire.it->uses = list::empty();
 mClaire.it->part_of = claire.it;
 mClaire.it->parts = list::alloc(1,_oid_(it));
 mClaire.it->status = 3;
 mClaire.it->comment = "mClaire";
 mClaire.it->made_of = list::empty();
 mClaire.it->uses = list::empty();
 it->uses = list::empty();
 it->parts = list::empty();
 it->part_of = mClaire.it;
 it->status = 3;
 it->made_of = list::empty();
 it->comment = "Kernel";
 _class->instances = list::domain(5,_class,_symbol,_module,_set,_list);
 // step 2: define the symbols
 _class->name = symbol::make("class",claire.it,it);
 _list->name = symbol::make("list",claire.it,it);
 _set->name = symbol::make("set",claire.it,it);
 _symbol->name = symbol::make("symbol",claire.it,it);
 _module->name = symbol::make("module",claire.it,it);
 claire.it->name = symbol::make("claire",claire.it,claire.it);
 Kernel.it->name = symbol::make("Kernel",claire.it,claire.it);
 mClaire.it->name = symbol::make("mClaire",claire.it,claire.it);
 NoDefault->name = symbol::make("NoDefault",it,it);
 symbol::make("true",claire.it,claire.it)->value = _oid_(CTRUE);
 symbol::make("false",claire.it,claire.it)->value = _oid_(CFALSE);
 unknownName = symbol::make("unknown",claire.it,claire.it);
 unknownName->value = CNULL;
 PRIVATE = symbol::make("private",claire.it,claire.it);
 PRIVATE->value = _oid_(PRIVATE);
// STATIC = symbol::make("static",claire.it,claire.it);            // v3.4
//
 _class->name->value = _oid_(_class);
 _list->name->value = _oid_(_list);
 _set->name->value = _oid_(_set);
 _symbol->name->value = _oid_(_symbol);
 _module->name->value = _oid_(_module);
 claire.it->name->value = _oid_(claire.it);
 Kernel.it->name->value = _oid_(Kernel.it);
 mClaire.it->name->value = _oid_(mClaire.it);
 NoDefault->name->value = _oid_(NoDefault);
 Kernel.nil->isa = _list;
 Kernel.emptySet->isa = _set;
 // step 3: we can now create a few class templates
 _property = ClaireClass::make("property");
 _restriction = ClaireClass::make("restriction");
 _slot = ClaireClass::make("slot");
 _any = ClaireClass::make("any");
 _type = ClaireClass::make("type");
 _float = ClaireClass::make("float");          // used in methods
// _void = ClaireClass::make("void");
 OBJECT(ClaireAny,CNULL)->isa = _any;
 _void = ClaireClass::make("void");
 _boolean = ClaireClass::make("boolean");
    CTRUE->isa = _boolean;
    CFALSE->isa = _boolean;

 // step 4: now we can build prototypes
 // isa/name/comment/slot/sclass/sub/ances/desc/eval/open/inst/proto/par/code/dict
 _class->prototype = list::alloc(18, 0, 0, 0, _oid_(list::empty(_slot)), 0,
                                 _oid_(set::empty(_class)),
                                 _oid_(list::empty(_class)),_oid_(set::empty(_class)),
                                 0,2,_oid_(list::empty()),_oid_(list::empty()),
                                 _oid_(list::empty()),0,_oid_(list::empty()),
                                 _oid_(CTRUE),CNULL,_oid_(list::empty()));
 _class->ancestors = list::alloc(1,_oid_(_class));
  // JUST CHANGED in v3.0.54 ! was :
  // _class->ancestors = list::alloc(3,_oid_(_class),1,_oid_(_class));
  // isa/name/comment/dom/ran/ifwrite/store/inv/open/multi/trace/restric/defini/dict/
  
 _property->prototype =  list::alloc(16, 0, 0, 0, _oid_(_any), _oid_(_any), CNULL, _oid_(CFALSE),
                                 0, 2, _oid_(CFALSE), 0, _oid_(list::empty(_restriction)),
                                _oid_(list::empty(_restriction)),_oid_(list::empty()),
                                _oid_(CFALSE), 0);
 // isa/module/comment/dom/range/select/srange/default/index

 // step5: this is the first set of properties
 isa = property::make("isa",ClEnv->close,claire.it);
 name = property::make("name",ClEnv->close,claire.it);
 sname = property::make("sname",ClEnv->close,claire.it);
 comment = property::make("comment",claire.it);
 slots = property::make("slots",ClEnv->close,claire.it);
 superclass = property::make("superclass",ClEnv->close,claire.it);
 subclass = property::make("subclass",ClEnv->close,claire.it);
 ancestors = property::make("ancestors",ClEnv->close,claire.it);
 descendents = property::make("descendents",ClEnv->close,claire.it);
// open = property::make("open",ClEnv->close,claire.it);
 instances = property::make("instances",ClEnv->close,claire.it);
 prototype = property::make("prototype",ClEnv->close,mClaire.it);
 params = property::make("params",ClEnv->close,claire.it);
 code = property::make("code",ClEnv->close,claire.it);
 ident_ask = property::make("ident?",claire.it);
 dictionary = property::make("dictionary",ClEnv->close,mClaire.it);
 dispatcher = property::make("dispatcher",ClEnv->close,mClaire.it);
 reified = property::make("reified",claire.it);
 graph = property::make("graph",ClEnv->close,mClaire.it);
 domain = property::make("domain",ClEnv->close,claire.it);
 range = property::make("range",ClEnv->close,claire.it);
 selector = property::make("selector",ClEnv->close,claire.it);
 srange = property::make("srange",ClEnv->close,mClaire.it);
 trace_I = property::make("trace!",it);
 restrictions = property::make("restrictions",ClEnv->close,claire.it);
 definition = property::make("definition",ClEnv->close,mClaire.it);
 inverse = property::make("inverse",claire.it);
 index = property::make("index",ClEnv->close,mClaire.it);
 base = property::make("base",ClEnv->close,mClaire.it);
 debug_I = property::make("debug!",mClaire.it);
 module_I = property::make("module!",claire.it);
 evaluate = property::make("evaluate",mClaire.it);
 formula = property::make("formula",claire.it);
 typing = property::make("typing",it);
 body = property::make("body",claire.it);
 if_write = property::make("if_write",claire.it);
 store_ask = property::make("store?",claire.it);
 multivalued_ask = property::make("multivalued?",claire.it);
 arg = property::make("arg",claire.it);
 value = property::make("value",claire.it);
 functional = property::make("functional",claire.it);
 precedence = property::make("precedence",claire.it);
 status = property::make("status",mClaire.it);
 parts = property::make("parts",claire.it);
 part_of = property::make("part_of",claire.it);
 made_of = property::make("made_of",claire.it);
 source = property::make("source",claire.it);
 uses = property::make("uses",claire.it);
 inline_ask = property::make("inline?",claire.it);
 verbose = property::make("verbose",claire.it);
 exception_I = property::make("exception!",claire.it);
 trace_I = property::make("trace!",claire.it);
 step_I = property::make("step!",claire.it);
 spy_I = property::make("spy!",claire.it); 
 close = property::make("close",claire.it);
 ABSTRACT = property::make("abstract",claire.it);
 FINAL = property::make("final",claire.it);
 DEFAULT = property::make("default",claire.it);
 open = property::make("open",claire.it);
 ephemeral = property::make("ephemeral",claire.it);
 cout = property::make("cout",claire.it);
 ctrace = property::make("ctrace",claire.it);
 last_debug = property::make("last_debug",it);
 last_index = property::make("last_index",it);
 count_call = property::make("count_call",mClaire.it);
 count_level = property::make("count_level",mClaire.it);
 count_trigger = property::make("count_trigger",mClaire.it);
 version = property::make("version",claire.it);
 external = property::make("external",claire.it);


// step6: this is the first set of classes [we do the root, void, by hand]
//_void = ClaireClass::make("void");   already done
    _void->superclass = NULL;
    _void->ancestors = list::empty(_class)->addFast(_oid_(_void));
    _void->descendents = set::empty(_class)->addFast(_oid_(_void));
    _void->prototype = list::empty();
    _void->code = 0;

_any = ClaireClass::make("any",_void,claire.it);
_integer = ClaireClass::make("integer",_any,claire.it);
_object = ClaireClass::make("object",_any,claire.it);
   CL_ADDSLOT(_object,ClaireObject,isa,_class,CNULL);
// primitive
_primitive = ClaireClass::make("primitive",_any,claire.it);
_cl_import = ClaireClass::make("import",_primitive,claire.it);    // v3.3.22
_float = ClaireClass::make("float",_primitive,claire.it);
_string = ClaireClass::make("string",_primitive,claire.it);
_array = ClaireClass::make("array",_primitive,claire.it);
// _function = ClaireClass::make("function",_primitive,claire.it);
_port = ClaireClass::make("port",_cl_import,claire.it);    // v3.3.22
// objects
_system_object = ClaireClass::make("system_object",_object,claire.it);
_symbol = ClaireClass::make("symbol",_system_object,claire.it);
   CL_ADDSLOT(_symbol,symbol,name,_string,CNULL);       // char* part      m1/toto -> "toto"
   CL_ADDSLOT(_symbol,symbol,module_I,_module,CNULL);   // the name space  m1/toto -> m1
   CL_ADDSLOT(_symbol,symbol,definition,_module,CNULL); // where the symbol is defined (NULL => PRIVATE)
   CL_ADDSLOT(_symbol,symbol,value,_any,CNULL);         // the "content" of the symbol

_function = ClaireClass::make("function",_system_object,claire.it);

// collections .....
_collection = ClaireClass::make("collection",_object,claire.it);
_type = ClaireClass::make("type",_collection,claire.it);
_bag = ClaireClass::make("bag",_type,claire.it);
_list = ClaireClass::make("list",_bag,claire.it);
_listargs = ClaireClass::make("listargs",_list,claire.it);
_set = ClaireClass::make("set",_bag,claire.it);
_tuple = ClaireClass::make("tuple",_bag,claire.it);
   _list->ident_ask = CFALSE;
   _set->ident_ask = CFALSE;
   _tuple->ident_ask = CFALSE;
_class = ClaireClass::make("class",_type,claire.it);
    CL_ADDSLOT(_class,ClaireClass,name,_symbol,CNULL);
    CL_ADDSLOT(_class,ClaireClass,comment,_string,CNULL);
    CL_ADDSLOT(_class,ClaireClass,slots,_list,_oid_(list::empty(_slot)));
    CL_ADDSLOT(_class,ClaireClass,superclass,_class,CNULL);
    CL_ADDSLOT(_class,ClaireClass,subclass,_set,_oid_(set::empty(_class)));
    CL_ADDSLOT(_class,ClaireClass,ancestors,_list,_oid_(list::empty(_class)));
    CL_ADDSLOT(_class,ClaireClass,descendents,_set,_oid_(set::empty(_class)));
    CL_ADDSLOT(_class,ClaireClass,evaluate,_function,CNULL);
    CL_ADDSLOT(_class,ClaireClass,open,_integer,2);
    CL_ADDSLOT(_class,ClaireClass,instances,_list,_oid_(list::empty()));
    CL_ADDSLOT(_class,ClaireClass,prototype,_list,_oid_(list::empty()));
    CL_ADDSLOT(_class,ClaireClass,params,_list,_oid_(list::empty()));
    CL_ADDSLOT(_class,ClaireClass,code,_integer,0);
    CL_ADDSLOT(_class,ClaireClass,dictionary,_list,_oid_(list::empty()));
    CL_ADDSLOT(_class,ClaireClass,ident_ask,_boolean,_oid_(CTRUE));
    CL_ADDSLOT(_class,ClaireClass,if_write,_any,CNULL);
    CL_ADDSLOT(_class,ClaireClass,dispatcher,_object,_oid_(list::empty()));
params->multivalued_ask = CFALSE;
dictionary->multivalued_ask = CFALSE;

//see("class prototype",_oid_(_class->prototype),1);
_thing = ClaireClass::make("thing",_object,claire.it);
    CL_ADDSLOT(_thing,thing,name,_symbol,_oid_(symbol::make("unamed",claire.it,claire.it))); // CNULL);
    NoDefault->isa = _thing;

_system_thing = ClaireClass::make("system_thing",_thing,claire.it);
_boolean      = ClaireClass::make("boolean",_system_object,claire.it);
        _boolean->instances = list::alloc(2,_oid_(CTRUE),_oid_(CFALSE));
CTRUE->isa = _boolean;
CFALSE->isa = _boolean;
_restriction = ClaireClass::make("restriction",_system_object,claire.it);
    CL_ADDSLOT(_restriction,restriction,module_I,_module,CNULL);
    CL_ADDSLOT(_restriction,restriction,comment,_string,CNULL);
    CL_ADDSLOT(_restriction,restriction,domain,_list,_oid_(list::empty(_type)));
    CL_ADDSLOT(_restriction,restriction,range,_type,_oid_(_any));
    CL_ADDSLOT(_restriction,restriction,selector,_property,CNULL);
 //   _restriction,X,srange,_class,CNULL);
domain->multivalued_ask = CFALSE;
_slot = ClaireClass::make("slot",_restriction,claire.it);
    CL_ADDSLOT(_slot,slot,srange,_class,CNULL);
    CL_ADDSLOT(_slot,slot,DEFAULT,_any,CNULL);
    CL_ADDSLOT(_slot,slot,index,_integer,0);
_method = ClaireClass::make("method",_restriction,claire.it);
     CL_ADDSLOT(_method,method,srange,_list,_oid_(list::empty(_type)));
     CL_ADDSLOT(_method,method,formula,_object,CNULL);
     CL_ADDSLOT(_method,method,functional,_function,CNULL);
     CL_ADDSLOT(_method,method,evaluate,_function,CNULL);
     CL_ADDSLOT(_method,method,typing,_any,CNULL);
     CL_ADDSLOT(_method,method,status,_integer,0);
     CL_ADDSLOT(_method,method,inline_ask,_boolean,_oid_(CFALSE));
_char =  ClaireClass::make("char",_system_object,claire.it);
ClaireChar::init();
_relation = ClaireClass::make("relation",_system_thing,claire.it);
    CL_ADDSLOT(_relation,ClaireRelation,comment,_string,CNULL);
    CL_ADDSLOT(_relation,ClaireRelation,domain,_type,_oid_(_any));
    CL_ADDSLOT(_relation,ClaireRelation,range,_type,_oid_(_any));
    CL_ADDSLOT(_relation,ClaireRelation,if_write,_any,CNULL);
    CL_ADDSLOT(_relation,ClaireRelation,store_ask,_boolean,_oid_(CFALSE));
    CL_ADDSLOT(_relation,ClaireRelation,inverse,_relation,CNULL);
    CL_ADDSLOT(_relation,ClaireRelation,open,_integer,2);
    CL_ADDSLOT(_relation,ClaireRelation,multivalued_ask,_object,_oid_(CFALSE)); // an interesting situation ...
_property = ClaireClass::make("property",_relation,claire.it);
    CL_ADDSLOT(_property,property,trace_I,_integer,0);
    CL_ADDSLOT(_property,property,restrictions,_list,_oid_(list::empty(_restriction)));
    CL_ADDSLOT(_property,property,definition,_list,_oid_(list::empty(_restriction)));
 //   CL_ADDSLOT(_property,property,dictionary,_object,_oid_(list::empty()));
    CL_ADDSLOT(_property,property,dictionary,_boolean,_oid_(CFALSE));         // v3.2.58 cleanup
    CL_ADDSLOT(_property,property,reified,_object,_oid_(CFALSE));
    CL_ADDSLOT(_property,property,dispatcher,_integer,0);
_operation = ClaireClass::make("operation",_property,claire.it);
    CL_ADDSLOT(_operation,operation,precedence,_integer,0);
_table = ClaireClass::make("table",_relation,claire.it);
    CL_ADDSLOT(_table,table,graph,_bag,CNULL);
    CL_ADDSLOT(_table,table,params,_any,CNULL);
    CL_ADDSLOT(_table,table,DEFAULT,_any,CNULL);
_module = ClaireClass::make("module",_system_thing,claire.it);
    CL_ADDSLOT(_module,module,comment,_string,CNULL);
    CL_ADDSLOT(_module,module,parts,_list,_oid_(list::empty(_module)));
    CL_ADDSLOT(_module,module,part_of,_module,_oid_(claire.it));   // v2.9
    CL_ADDSLOT(_module,module,uses,_list,_oid_(list::empty(_module)));
    CL_ADDSLOT(_module,module,source,_string,_string_(""));
    CL_ADDSLOT(_module,module,made_of,_list,_oid_(list::empty(_string)));
    CL_ADDSLOT(_module,module,status,_integer,0);
    CL_ADDSLOT(_module,module,evaluate,_function,CNULL);
    CL_ADDSLOT(_module,module,external,_string,CNULL);
_exception = ClaireClass::make("exception",_system_object,claire.it);
_error = ClaireClass::make("error",_exception,claire.it);
_system_error = ClaireClass::make("system_error",_error,claire.it);     // v3.0.54
    CL_ADDSLOT(_system_error,system_error,index,_integer,0);
    CL_ADDSLOT(_system_error,system_error,value,_any,0);
    CL_ADDSLOT(_system_error,system_error,arg,_any,0);
_environment = ClaireClass::make("environment",_system_object,claire.it);
        CL_ADDSLOT(_environment,ClaireEnvironment,verbose,_integer,0);
        CL_ADDSLOT(_environment,ClaireEnvironment,exception_I,_exception,0);
        CL_ADDSLOT(_environment,ClaireEnvironment,module_I,_module,0);
        CL_ADDSLOT(_environment,ClaireEnvironment,name,_string,CNULL);
        CL_ADDSLOT(_environment,ClaireEnvironment,version,_float,_float_(0.0));
        CL_ADDSLOT(_environment,ClaireEnvironment,ctrace,_port,0);
        CL_ADDSLOT(_environment,ClaireEnvironment,cout,_port,0);
        CL_ADDSLOT(_environment,ClaireEnvironment,index,_integer,0);
        CL_ADDSLOT(_environment,ClaireEnvironment,base,_integer,0);
        CL_ADDSLOT(_environment,ClaireEnvironment,debug_I,_integer,0);
        CL_ADDSLOT(_environment,ClaireEnvironment,trace_I,_integer,0);
        CL_ADDSLOT(_environment,ClaireEnvironment,step_I,_integer,0);
        CL_ADDSLOT(_environment,ClaireEnvironment,last_debug,_integer, 0);        // last value of the debug index
        CL_ADDSLOT(_environment,ClaireEnvironment,last_index,_integer,0);         // last value of the top of eval stack
        CL_ADDSLOT(_environment,ClaireEnvironment,spy_I,_object,0);         // store the spy method if any
        CL_ADDSLOT(_environment,ClaireEnvironment,count_call,_integer, -1);      // count the numbers of call
        CL_ADDSLOT(_environment,ClaireEnvironment,count_level,_integer, 0);      // level at which something happens ...
        CL_ADDSLOT(_environment,ClaireEnvironment,count_trigger,_any,CNULL);     // what should happen
        CL_ADDSLOT(_environment,ClaireEnvironment,params,_list,_oid_(list::empty()));    // list of arguments v2.4.07
        CL_ADDSLOT(_environment,ClaireEnvironment,close,_integer,0);              // integer constants for open(x)
        CL_ADDSLOT(_environment,ClaireEnvironment,ABSTRACT,_integer,0);           // ....
        CL_ADDSLOT(_environment,ClaireEnvironment,FINAL,_integer,0);
        CL_ADDSLOT(_environment,ClaireEnvironment,DEFAULT,_integer,0);
        CL_ADDSLOT(_environment,ClaireEnvironment,open,_integer,0);
        CL_ADDSLOT(_environment,ClaireEnvironment,ephemeral,_integer,0);
    
ClEnv->moduleStack = list::empty();
ClEnv->isa = _environment;
symbol::make("system",claire.it,claire.it)->value = _oid_(ClEnv);

Kernel._unbound_symbol = ClaireClass::make("unbound_symbol",Kernel._system_object,claire.it);
    CL_ADDSLOT(_unbound_symbol,unbound_symbol,name,_symbol,CNULL);
_keyword = ClaireClass::make("keyword",_system_thing,claire.it);

_module->instances = list::alloc(3,_oid_(claire.it),_oid_(it),_oid_(mClaire.it));

{ITERATE(c);
 for (START(_primitive->descendents); NEXT(c);)
    OBJECT(ClaireClass,c)->open = ClEnv->close;}
_cl_import->open = ClEnv->ABSTRACT,             // v3.3.22
_object->open = ClEnv->ABSTRACT,
_class->open = ClEnv->FINAL;
_slot->open = ClEnv->FINAL;
_boolean->open = ClEnv->FINAL;  // ClEnv->ABSTRACT;
_bag->open = ClEnv->FINAL; // v3.2.24 was ClEnv->ABSTRACT !
_list->open = ClEnv->FINAL; // ClEnv->ABSTRACT;
_set->open = ClEnv->FINAL; // ClEnv->ABSTRACT;
_any->open = ClEnv->FINAL; // ClEnv->ABSTRACT;
_exception->open = 4;
_error->open = 4;

 }


// ===================== end of bootstrap =========================================

void KernelClass::metaLoad()
{bootstrap();
// properties of module Kernel
copy = property::make("copy",claire.it);
_equal = operation::make("=",claire.it,60);
slot_get = property::make("slot_get",claire.it);
_Z = operation::make("%",claire.it,50);
hash = property::make("hash",claire.it);
add_slot = property::make("add_slot",claire.it);
sort_I = property::make("sort!",claire.it);
add_method = property::make("add_method",claire.it);
_delete = operation::make("delete",claire.it,20);
add = operation::make("add",claire.it,10);
add_I = operation::make("add!",claire.it,10);
cons = operation::make("cons",claire.it,10);
make_list = property::make("make_list",claire.it);
add_star = operation::make("add*",claire.it,10);
_7_plus = operation::make("/+",claire.it,10);
nth_plus = property::make("nth+",claire.it);
nth_dash = property::make("nth-",claire.it);
cdr = property::make("cdr",claire.it);
shrink = property::make("shrink",claire.it);
_exp = operation::make("^",claire.it,5);
set_I = property::make("set!",1,claire.it,1);  // v3.0.52 !!!
list_I = property::make("list!",claire.it);
_dot_dot = operation::make("..",claire.it,30);
_dash_dash_ask = operation::make("--?",claire.it,30);
length = property::make("length",claire.it);
of = property::make("of",claire.it);
nth_get = property::make("nth_get",claire.it);
nth_put = property::make("nth_put",claire.it);
array_I = property::make("array!",claire.it);
princ = property::make("princ",claire.it);
self_print = property::make("self_print",claire.it);
self_print->open = 3;                         // v3.2.00
integer_I = property::make("integer!",claire.it);
substring = property::make("substring",claire.it);
get = property::make("get",claire.it);
nth = property::make("nth",claire.it);
nth_equal = property::make("nth=",claire.it);
begin = property::make("begin",claire.it);
end = property::make("end",claire.it);
defined = property::make("defined",claire.it);
gensym = property::make("gensym",claire.it);
_dash = operation::make("-",claire.it,20);
mod = operation::make("mod",claire.it,10);
_7 = operation::make("/",claire.it,10);
_exp2 = property::make("^2",claire.it);
char_I = property::make("char!",claire.it);
string_I = property::make("string!",claire.it);
make_string = property::make("make_string",claire.it);
random = property::make("random",claire.it);
random_I = property::make("random!",claire.it);
_star = operation::make("*",claire.it,10);
float_I = property::make("float!",claire.it);
_inf = operation::make("<",4,claire.it,60);                       // v3.2.04
_inf_equal = operation::make("<=",claire.it,60);                  // status 4 means that a new definition
_sup = operation::make(">",4,claire.it,60);                       // that creates a conflict is allowed
_sup_equal = operation::make(">=",4,claire.it,60);                //
use_as_output = property::make("use_as_output",claire.it);
fclose = property::make("fclose",claire.it);
putc = property::make("putc",claire.it);
put = property::make("put",claire.it);
getc = property::make("getc",claire.it);
flush = property::make("flush",claire.it);
fopen = property::make("fopen",claire.it);
port_I = property::make("port!",claire.it);
set_length = property::make("set_length",mClaire.it);
read_string = property::make("read_string",mClaire.it);
read_ident = property::make("read_ident",mClaire.it);
read_number = property::make("read_number",mClaire.it);
read_thing = property::make("read_thing",mClaire.it);
c_princ = property::make("c_princ",claire.it);
mem = property::make("mem",claire.it);
stat = property::make("stat",claire.it);
// reset_stack = property::make("reset_stack",claire.it);    -> not used
// stack_update = property::make("stack_update",claire.it);  -> use stack_apply
stack_apply = property::make("stack_apply",claire.it);
fastcall = property::make("fastcall",claire.it);            // KEEP HERE !
funcall = property::make("funcall",claire.it);
store = property::make("store",claire.it);
//store_add = property::make("store_add",claire.it);
choice = property::make("choice",claire.it);         // world+
backtrack = property::make("backtrack",claire.it);   // world-
commit = property::make("commit",claire.it);         // world-!
commit0 = property::make("commit0",claire.it);       // world-0
world_ask = property::make("world?",claire.it);
world_id = property::make("world_id",claire.it);
contain_ask = property::make("contain?",claire.it);
print = property::make("print",claire.it);
member_ask = property::make("member?",claire.it);
cast_I = property::make("cast!",claire.it);
_inf_inf = operation::make("<<",claire.it,10);
symbol_I = property::make("symbol!",claire.it);
boolean_I = property::make("boolean!",claire.it);
restore_state = property::make("restore_state",mClaire.it);
date_I = property::make("date!",claire.it);
empty = property::make("empty",claire.it);
free_I = property::make("free!",claire.it);           // v3.2.40

// methods
// method definition of module Kernel -------------------------------------
// we follow the same order as in the claire.h API list
// TODO: check all status

copy->addMethod( list::domain(1,_object),_object,
                 NEW_ALLOC, _function_(copy_object,"copy_object"));
_equal->addMethod(   list::domain(2,_any,_any),_boolean,
                    0,_function_(equal,"equal"));
slot_get->addMethod( list::domain(3,_object,_integer,_class),_any,
                    0,_function_(slot_get_object,"slot_get_object"));
_Z->addMethod( list::domain(2,_any,_any),_boolean,
                    0,_function_(belong_to,"belong_to"));
boolean_I->addMethod( list::domain(1,_any),_boolean,
                    0,_function_(boolean_I_any,"boolean_I_any"));
hash->addMethod( list::domain(2,_list,_any),_integer,
                0,_function_(hash_list,"hash_list"));
_Z->range = _boolean;

// === class & properties ===================================================
add_slot->addMethod(list::domain(5,_class,_property,_type,_any,_integer),_slot,
                    0,_function_(add_slot_class,"add_slot_class"));
sort_I->addMethod(  list::domain(1,_class),_class,
                    0,_function_(sort_I_class,"sort_I_class"));
add_method->addMethod(  list::domain(5,_property,_list,_type,_any,_any),_method,
                        0,_function_(add_method_property,"add_method_property")),
index->addMethod(   list::domain(2,_table,_any),_integer,
                    0,_function_(index_table,"index_table"));
index->addMethod(   list::domain(3,_table,_any,_any),_integer,
                    0,_function_(index_table2,"index_table2"));


// === bags =================================================================
copy->addMethod(    list::domain(1,_bag),_bag,
                    0,_function_(copy_bag,"copy_bag"));   // v3.3.38 : no status because protection is implicit !
empty->addMethod(    list::domain(1,_bag),_bag,
                    0,_function_(empty_bag,"empty_bag"));
_delete->addMethod(  list::domain(2,_bag,_any),_bag,
                    0,_function_(delete_bag,"delete_bag"));
of->addMethod(  list::domain(1,_bag),_type,
                0,_function_(of_bag,"of_bag"));
cast_I->addMethod(  list::domain(2,_bag,_type),_bag,
                    0,_function_(cast_I_bag,"cast_I_bag"));

// === list =================================================================
add->addMethod( list::domain(2,_list,_any),_list,
                NEW_ALLOC+SAFE_RESULT,_function_(add_list,"add_list"));
add_I->addMethod( list::domain(2,_list,_any),_list,
                NEW_ALLOC+SAFE_RESULT,_function_(add_I_list,"add_I_list"));
cons->addMethod(    list::domain(2,_any,_list),_list,
                    NEW_ALLOC+SAFE_RESULT,_function_(cons_any,"cons_any"));
make_list->addMethod(   list::domain(2,_integer,_any),_list,
                        NEW_ALLOC+SAFE_RESULT ,_function_(make_list_integer,"make_list_integer"));
add_star->addMethod(list::domain(2,_list,_list),_list,
                    NEW_ALLOC+SAFE_RESULT,_function_(add_star_list,"add_star_list"));
_7_plus->addMethod(  list::domain(2,_list,_list),_list,
                    NEW_ALLOC+SAFE_RESULT,_function_(append_list,"append_list"));
nth_plus->addMethod(  list::domain(3,_list,_integer,_any),_list,
                    NEW_ALLOC+BAG_UPDATE+SAFE_RESULT,_function_(add_at_list,"add_at_list"));
nth_dash->addMethod(   list::domain(2,_list,_integer),_list,
                        RETURN_ARG,_function_(delete_at_list,"delete_at_list"));
_inf_inf->addMethod(list::domain(2,_list,_integer),_list,
                0,_function_(skip_list,"skip_list"));
cdr->addMethod( list::domain(1,_list),_list,
                NEW_ALLOC+SAFE_RESULT,_function_(cdr_list,"cdr_list"));  // safe_result  ->
shrink->addMethod(  list::domain(2,_bag,_integer),_bag,                  // v3.2.20
                    0,_function_(shrink_list,"shrink_list"));
contain_ask->addMethod( list::domain(2,_list,_any),_boolean,
                        0,_function_(contain_ask_list,"contain_ask_list"));
get->addMethod( list::domain(2,_list,_any),_integer,
                0,_function_(index_list,"index_list"));
// new
make_string->addMethod( list::domain(1,_list),_string,
                        NEW_ALLOC,_function_(make_string_list,"make_string_list"));


// === sets =================================================================
add->addMethod( list::domain(2,_set,_any),_set,
                NEW_ALLOC+SAFE_RESULT,_function_(add_set,"add_set"));
add_I->addMethod( list::domain(2,_set,_any),_set,
                  NEW_ALLOC+SAFE_RESULT,_function_(add_I_set,"add_I_set"));
_exp->addMethod(    list::domain(2,_set,_set),_set,
                    NEW_ALLOC+SAFE_RESULT,_function_(_exp_set,"_exp_set"));
contain_ask->addMethod( list::domain(2,_set,_any),_boolean,
                        0,_function_(contain_ask_set,"contain_ask_set"));
_7_plus->addMethod(  list::domain(2,_set,_set),_set,
                    NEW_ALLOC+SAFE_RESULT,_function_(append_set,"append_set"));
//set_I->addMethod(   list::domain(1,_bag),_set,
//                    NEW_ALLOC+SAFE_RESULT,_function_(set_I_bag,"set_I_bag"));
//list_I->addMethod(  list::domain(1,_set),_list,
//                    NEW_ALLOC+SAFE_RESULT,_function_(list_I_set,"list_I_set"));
_dash_dash_ask->addMethod(list::domain(2,_integer,_integer),_collection,
                    NEW_ALLOC+SAFE_RESULT,_function_(sequence_integer,"sequence_integer"));

                    
// === tuple =============================================================
list_I->addMethod(  list::domain(1,_tuple),_list,
                    NEW_ALLOC+SAFE_RESULT,_function_(list_I_tuple,"list_I_tuple"));

// === array ==================================================================
copy->addMethod(list::domain(1,_array),_array,
                NEW_ALLOC,_function_(copy_array,"copy_array"));
length->addMethod(  list::domain(1,_array),_integer,
                    0,_function_(length_array,"length_array"));
of->addMethod(  list::domain(1,_array),_type,
                0,_function_(of_array,"of_array"));
nth_get->addMethod( list::domain(2,_array,_integer),_any,     // v3.0.54 thanxs FXJ !
                    0,_function_(nth_get_array,"nth_get_array"));
nth_put->addMethod( list::domain(3,_array,_integer,_any),_void,
                    0,_function_(nth_put_array,"nth_put_array"));

// === strings =============================================================
copy->addMethod(    list::domain(1,_string),_string,
                    NEW_ALLOC,_function_(copy_string,"copy_string"));
//_equal->addMethod(  list::domain(2,_string,_string),_boolean,
//                    0,_function_(equal_string,"equal_string"));
princ->addMethod(   list::domain(1,_string),_void,
                    0,_function_(princ_string,"princ_string"));
self_print->addMethod(  list::domain(1,_string),_void,
                        0,_function_(self_print_string,"self_print_string"));
_7_plus->addMethod(  list::domain(2,_string,_string),_string,
                    NEW_ALLOC,_function_(append_string,"append_string"));
integer_I->addMethod(   list::domain(1,_string),_integer,
                        0,_function_(integer_I_string,"integer_I_string"));
substring->addMethod(   list::domain(3,_string,_integer,_integer),_string,
                        NEW_ALLOC,_function_(substring_string,"substring_string"));
get->addMethod( list::domain(2,_string,_char),_integer,
                0,_function_(get_string,"get_string"));
_inf_equal->addMethod(   list::domain(2,_string,_string),_boolean,
                    0,_function_(_less_string,"_less_string"));
substring->addMethod(list::domain(3,_string,_string,_boolean),_integer,
                    0,_function_(included_string,"included_string"));
nth->addMethod( list::domain(2,_string,_integer),_char,
                0,_function_(nth_string,"nth_string"));
nth_equal->addMethod( list::domain(3,_string,_integer,_char), _void,
                    0,_function_(nth_set_string,"nth_set_string"));
shrink->addMethod(  list::domain(2,_string,_integer),_string,
                    0,_function_(shrink_string,"shrink_string"));
value->addMethod(   list::domain(1,_string),_any,
                    0,_function_(value_string,"value_string"));
value->addMethod(   list::domain(2,_module,_string),_any,
                    0,_function_(value_module,"value_module"));
date_I->addMethod(  list::domain(1,_integer),_string,
                    0,_function_(date_I_integer,"date_I_integer"));

// === ports ==================================================================

use_as_output->addMethod(   list::domain(1,_port),_port,
                            0,_function_(use_as_output_port,"use_as_output_port"));
fclose->addMethod(list::domain(1,_port),_void,
                 0,_function_(fclose_port,"fclose_port"));
putc->addMethod(list::domain(2,_char,_port),_void,
                0,_function_(putc_char,"putc_char"));
getc->addMethod(list::domain(1,_port),_char,
                0,_function_(getc_port,"getc_port"));
flush->addMethod(   list::domain(1, _port),_void,
                    0,_function_(flush_port,"flush_port"));       // v3.2.38 - fxj
flush->addMethod(   list::domain(2, _port,_integer),_void,
                    0,_function_(pushback_port,"pushback_port"));
fopen->addMethod(   list::domain( 2, _string,_string), _port,
                    0,_function_(fopen_string, "fopen_string"));
port_I->addMethod(  list::domain( 1, _void), _port,
                    0,_function_(port_I_void,"port_I_void"));
port_I->addMethod(  list::domain( 1,_string), _port,
                    0,_function_(port_I_string,"port_I_string"));
string_I->addMethod(list::domain( 1,_port),_string,
                    0,_function_(string_I_port, "string_I_port"));
length->addMethod(  list::domain( 1,_port),_integer,
                    0,_function_(length_port,"length_port"));
set_length->addMethod(list::domain( 2,_port,_integer),_port,
                      0,_function_(set_length_port,"set_length_port"));
read_string->addMethod( list::domain( 1,_port), _string,
                        0,_function_(read_string_port, "read_string_port"));
read_ident->addMethod(  list::domain( 1,_port), _any,
                        0,_function_( read_ident_port,"read_ident_port"));
read_number->addMethod( list::domain( 1,_port),_any,
                        0,_function_(read_number_port,"read_number_port"));
read_thing->addMethod(  list::domain(4,_port,_module,_char,_module),_any,
                        0,_function_(read_thing_port,"read_thing_port"));
free_I->addMethod(list::domain(1,_port),_void,                            // v3.2.40 -> enable free
                  0,_function_(free_I_port,"free_I_port"));


// ==== modules & symbols =====================================================
begin->addMethod(   list::domain(1,_module),_void,
                    0,_function_(begin_module,"begin_module"));
end->addMethod( list::domain(1,_module),_void,
                0,_function_(end_module,"end_module"));
_7_plus->addMethod(  list::domain(2,_symbol,_any),_symbol,
                    NEW_ALLOC,_function_(append_symbol,"append_symbol"));
princ->addMethod(   list::domain(1,_symbol),_void,
                    0,_function_(princ_symbol,"princ_symbol"));
defined->addMethod( list::domain(1,_symbol),_module,
                    0,_function_(defined_symbol,"defined_symbol"));
gensym->addMethod(  list::domain(1,_string),_symbol,
                    0,_function_(gensym_string ,"gensym_string"));
string_I->addMethod(list::domain( 1,_symbol),_string,
                    0,_function_(string_I_symbol, "string_I_symbol"));
get->addMethod( list::domain(1,_symbol),_any,
                0,_function_(get_symbol,"get_symbol"));
put->addMethod( list::domain(2,_symbol,_any),_any,
                0,_function_(put_symbol,"put_symbol"));
symbol_I->addMethod(list::domain(2,Kernel._string,Kernel._module),Kernel._symbol,
        0,_function_(symbol_I_string,"symbol!_string"));
integer_I->addMethod(   list::domain(1,_symbol),_integer,
                        0,_function_(integer_I_symbol,"integer_I_symbol"));
                    
// === integer & floats =======================================================
_dash->addMethod(  list::domain(1,_integer),_integer,
                    0,_function_(ch_sign,"ch_sign"));
mod->addMethod( list::domain(2,_integer,_integer),_integer,
                0,_function_(mod_integer,"mod_integer"));
_7->addMethod(  list::domain(2,_integer,_integer),_integer,
                0,_function_(_7_integer,"_7_integer"));
_exp->addMethod(    list::domain(2,_integer,_integer),_integer,
                    0,_function_(_exp_integer,"_exp_integer"));
_exp2->addMethod(    list::domain(1,_integer),_integer,
                    0,_function_(exp2_integer,"exp2_integer"));
char_I->addMethod(  list::domain(1,_integer),_char,
                    0,_function_(char_I_integer,"char_I_integer"));
string_I->addMethod(list::domain(1,_integer),_string,
                    NEW_ALLOC,_function_(string_I_integer ,"string_I_integer "));
make_string->addMethod( list::domain(2,_integer,_char),_string,
                        NEW_ALLOC,_function_(make_string_integer,"make_string_integer"));
random->addMethod(  list::domain(1,_integer),_integer,
                    0,_function_(random_integer,"random_integer"));
random_I->addMethod(  list::domain(1,_integer),_void,                       // v3.2.36
                    0,_function_(srand,"srand"));
_star->addMethod(  list::domain(2,_integer,_integer),_integer,
                    0,_function_(times_integer,"times_integer"));
princ->addMethod(   list::domain(1,_integer),_void,
                    0,_function_(princ_integer,"princ_integer"));

float_I->addFloatMethod( list::domain(1,_integer), _float,
                    0,_function_(to_float ,"to_float"),
                    _function_(to_float_ ,"to_float_"));
integer_I->addFloatMethod(   list::domain(1,_float),_integer,
                        0,_function_(integer_I_float,"integer_I_float")
                         ,_function_(integer_I_float_,"integer_I_float_"));   // v3.0.4
_inf->addFloatMethod(    list::domain(2,_float,_float),_boolean,
                    0,_function_(_inf_float,"_inf_float"),
                    _function_(_inf_float_,"_inf_float_"));
_inf_equal->addFloatMethod(list::domain(2,_float,_float),_boolean,
                 0,_function_(_inf_equal_float,"_inf_equal_float"),
                 _function_(_inf_equal_float_,"_inf_equal_float_"));
_sup->addFloatMethod(list::domain(2,_float,_float),_boolean,
                 0,_function_(_sup_float,"_sup_float"),
                 _function_(_sup_float_,"*_sup_float_"));
_sup_equal->addFloatMethod(  list::domain(2,_float,_float),_boolean,
                    0,_function_(_sup_equal_float,"_sup_equal_float"),
                    _function_(_sup_equal_float_,"_sup_equal_float_"));
princ->addFloatMethod(   list::domain(1,_float),_void,
                         0,_function_(princ_float,"princ_float"),
                         _function_(princ_float_,"princ_float_"));


// === char & system ==========================================================

princ->addMethod(  list::domain(1,_char),_void,
                   0,_function_(princ_char,"princ_char"));
c_princ->addMethod( list::domain(1,_char),_void,
                    0,_function_(c_princ_c,"c_princ_c"));
c_princ->addMethod(list::domain(1,_string),_void,
                    0,_function_(c_princ_string,"c_princ_string"));
c_princ->addMethod( list::domain(1,_symbol),_void,
                    0,_function_(c_princ_symbol,"c_princ_symbol"));
mem->addMethod( list::domain(1,_any),_any,
                NEW_ALLOC,_function_(claire_mem,"claire_mem"));
stat->addMethod(    list::domain(1,_void),_void,
                    0,_function_(claire_stat,"claire_stat"));
integer_I->addMethod(   list::domain(1,_char),_integer,
                        0,_function_(integer_I_char,"integer_I_char"));

// === system functions =======================================================
close->addMethod(   list::domain(1,_exception), Kernel.emptySet,
                    0,_function_(close_exception,"close_exception"));
//->addMethod(    list::domain(2,_any),_integer),_any,
//                0,_function_(reset_stack,"reset_stack"));
stack_apply->addMethod(list::domain(1,_integer),_void,
                       0,_function_(stack_add,"stack_add"));
funcall->addMethod( list::domain(4,_function,_class,_any,_class),_any,
                    0,_function_(fcall1,"fcall1"));
funcall->addMethod( list::domain(6,_function,_class,_any,_class,_any,_class),_any,
                    0,_function_(fcall2,"fcall2"));
funcall->addMethod( list::domain(8,_function,_class,_any,_class,_any,_class,_any,_class),
                    _any, 0,_function_(fcall3,"fcall3"));
stack_apply->addMethod( list::domain(4,_function,_list,_integer,_integer),_any,
                        0,_function_(stack_apply_function,"stack_apply_function"));
string_I->addMethod(list::domain(1,_function),_string,
                    0,_function_(string_I_function,"string_I_function"));
store->addMethod(   list::domain(4,_list,_integer,_any,_boolean),_any,
                    BAG_UPDATE ,_function_(store_list,"store_list"));
store->addMethod(   list::domain(4,_array,_integer,_any,_boolean),_any,
                    0,_function_(store_array,"store_array"));
store->addMethod(   list::domain(5,_object,_integer,_class,_any,_boolean),_any,
                    SLOT_UPDATE,_function_(store_object,"store_object"));
store->addMethod(   list::domain(2,_list,_any),_list,
                        NEW_ALLOC + BAG_UPDATE,_function_(store_add,"store_add"));
choice->addMethod(  list::domain(1,_void),_void,
                        0,_function_(world_push,"world_push"));
backtrack->addMethod( list::domain(1,_void),_void,
                        0,_function_(world_pop,"world_pop"));
commit->addMethod(   list::domain(1,_void),_void,
                            0,_function_(world_remove,"world_remove"));
commit0->addMethod(    list::domain(1,_void),_void,
                        0,_function_(world_slaughter,"world_slaughter"));
world_ask->addMethod(   list::domain(1,_void),_integer,
                        0,_function_(world_number,"world_number"));
world_id->addMethod(   list::domain(1,_void),_integer,
                        0,_function_(world_get_id,"world_get_id"));
restore_state->addMethod(   list::domain(1,_void),_void,
                        0,_function_(restore_state_void,"restore_state_void"));

put_symbol(symbol_I_string("STDIN",claire.it),
           ClAlloc->import(Kernel._port, (int *) ClAlloc->stdIn));                     
put_symbol(symbol_I_string("STDOUT",claire.it),
           ClAlloc->import(Kernel._port, (int *) ClAlloc->stdOut));
Kernel._set->ident_ask = CFALSE;                   
Kernel._list->ident_ask = CFALSE;                   
Kernel._tuple->ident_ask = CFALSE;                   
Kernel._string->ident_ask = CFALSE;                   
Kernel._float->ident_ask = CFALSE;                   
Kernel._port->ident_ask = CFALSE;                   
}
 
