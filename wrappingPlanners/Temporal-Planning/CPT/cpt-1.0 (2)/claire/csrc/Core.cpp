/***** CLAIRE Compilation of file Core.cl 
         [version 3.3.4 / safety 5] Sat Oct 16 06:53:26 2004 *****/

#include <claire.h>
#include <Kernel.h>
#include <Core.h>


CoreClass Core;

// definition of the meta-model for Core 

void CoreClass::metaLoad() { 
  GC_BIND;
  ClEnv->module_I = it;
// definition of the properties 
  Core.class_I = property::make("class!",claire.it);
  Core.owner = property::make("owner",claire.it);
  Core.check_in = property::make("check_in",2,claire.it);
  Core.initialize = property::make("initialize",Core.it);
  Core.uniform = property::make("uniform",Core.it);
  Core.hashinsert = property::make("hashinsert",Core.it);
  Core.hashget = property::make("hashget",Core.it);
  Core.param_I = property::make("param!",Core.it);
  Core.size = property::make("size",1,claire.it,Kernel._any,2);
  Core.end_of_string = property::make("end_of_string",claire.it);
  Core.apply = property::make("apply",claire.it);
  Core.finite_ask = property::make("finite?",claire.it);
  Core.call_step = property::make("call_step",3,Core.it);
  Core.spy = property::make("spy",3,claire.it);
  Core.release = property::make("release",claire.it);
  Core._at = operation::make("@",claire.it,10);
  Core.about = property::make("about",claire.it);
  Core.get_args = property::make("get_args",mClaire.it);
  Core.push_debug = property::make("push_debug",Core.it);
  Core.pop_debug = property::make("pop_debug",Core.it);
  Core.tr_indent = property::make("tr_indent",Core.it);
  Core._plus = operation::make("+",claire.it,20);
  Core.identified_ask = property::make("identified?",Core.it);
  Core.identical_ask = property::make("identical?",claire.it);
  Core.get_index = property::make("get_index",Core.it);
  Core.factor_ask = property::make("factor?",claire.it);
  Core.divide_ask = property::make("divide?",claire.it);
  Core.Id = property::make("Id",claire.it);
  Core.pair_1 = property::make("pair_1",claire.it);
  Core.pair_2 = property::make("pair_2",claire.it);
  Core.check_inverse = property::make("check_inverse",Core.it);
  Core.invert = property::make("invert",claire.it);
  Core.domain_I = property::make("domain!",claire.it);
  Core.methods = property::make("methods",claire.it);
  Core.cause = property::make("cause",mClaire.it);
  Core.wrong = property::make("wrong",Core.it);
  Core.format = property::make("format",claire.it);
  Core.tformat = property::make("tformat",Core.it);
  Core.contradiction_I = property::make("contradiction!",claire.it);
  Core.get_stack = property::make("get_stack",mClaire.it);
  Core.put_stack = property::make("put_stack",mClaire.it);
  Core.push_I = property::make("push!",mClaire.it);
  Core.gc = property::make("gc",claire.it);
  Core.time_get = property::make("time_get",claire.it);
  Core.time_set = property::make("time_set",claire.it);
  Core.time_show = property::make("time_show",2,claire.it);
  Core.print_in_string = property::make("print_in_string",claire.it);
  Core.buffer_length = property::make("buffer_length",mClaire.it);
  Core.buffer_set_length = property::make("buffer_set_length",mClaire.it);
  Core.NOT = property::make("not",claire.it);
  Core.make_function = property::make("make_function",claire.it);
  Core.externC = property::make("externC",claire.it);
  Core.shell = property::make("shell",claire.it);
  Core.getenv = property::make("getenv",2,claire.it);
  Core._dash_dash_ask = property::make("--?",Core.it);
  Core.exit = property::make("exit",claire.it);
  Core.last = property::make("last",claire.it);
  Core.rmlast = property::make("rmlast",claire.it);
  Core.car = property::make("car",claire.it);
  Core.hashlist = property::make("hashlist",Core.it);
  Core.hashsize = property::make("hashsize",Core.it);
  Core.sort = property::make("sort",claire.it);
  Core.quicksort = property::make("quicksort",Core.it);
  Core.build_powerset = property::make("build_powerset",Core.it);
  Core.difference = property::make("difference",claire.it);
  Core.of_extract = property::make("of_extract",Core.it);
  Core.member = property::make("member",claire.it);
  Core.Address = property::make("Address",Core.it);
  Core.Oid = property::make("Oid",Core.it);
  Core.Oid_tilda = property::make("Oid~",Core.it);
  Core.get_value = property::make("get_value",claire.it);
  Core.enumerate = property::make("enumerate",Core.it);
  Core.t1 = property::make("t1",mClaire.it);
  Core.t2 = property::make("t2",mClaire.it);
  Core.tuple_I = property::make("tuple!",claire.it);
  Core.Uall = property::make("Uall",claire.it);
  Core.unique_ask = property::make("unique?",claire.it);
  Core.the = property::make("the",claire.it);
  Core.abstract_type = property::make("abstract_type",Core.it);
  Core.NEW = property::make("new",claire.it);
  Core.sqrt = property::make("sqrt",claire.it);
  Core.insert_definition = property::make("insert_definition",Core.it);
  Core.make_array = property::make("make_array",claire.it);
  Core.cpretty = property::make("cpretty",mClaire.it);
  Core.cprevious = property::make("cprevious",mClaire.it);
  Core.width = property::make("width",mClaire.it);
  Core.pprint = property::make("pprint",mClaire.it);
  Core.pbreak = property::make("pbreak",mClaire.it);
  Core.base_I = property::make("base!",mClaire.it);
  Core.set_base = property::make("set_base",mClaire.it);
  Core.index_I = property::make("index!",mClaire.it);
  Core.set_index = property::make("set_index",mClaire.it);
  Core.complete_I = property::make("complete!",mClaire.it);
  Core._Ztype = property::make("%type",mClaire.it);
  Core.update = property::make("update",mClaire.it);
  Core.make_set = property::make("make_set",claire.it);
  Core.get_symbol = property::make("get_symbol",claire.it);
  Core.time_read = property::make("time_read",2,claire.it);
  Core.first_arg_type = property::make("first_arg_type",Core.it);
  Core.second_arg_type = property::make("second_arg_type",Core.it);
  Core.meet_arg_types = property::make("meet_arg_types",Core.it);
  Core.make_copy_list = property::make("make_copy_list",claire.it);
  Core.log = property::make("log",claire.it);
  Core.new_I = property::make("new!",mClaire.it);
  Core.atan = property::make("atan",mClaire.it);
  Core.make_table = property::make("make_table",claire.it);
  Core.first_member_type = property::make("first_member_type",Core.it);
  
  // instructions from module sources
  { (Core.vars = property::make("vars",2,claire.it,Kernel._any,0));
    ;} 
  
  { (Core.dimension = property::make("dimension",1,claire.it,Kernel._any,0));
    ;} 
  
  { (Kernel.version = property::make("version",1,claire.it,Kernel._any,0));
    ;} 
  
  (Core._ephemeral_object = ClaireClass::make("ephemeral_object",Kernel._object,claire.it));
  
  { (Core._lambda = ClaireClass::make("lambda",Kernel._object,claire.it));
    CL_ADD_SLOT(Core._lambda,lambda,Core.vars,vars,Kernel._list,CNULL);
    CL_ADD_SLOT(Core._lambda,lambda,Kernel.body,body,Kernel._any,CNULL);
    CL_ADD_SLOT(Core._lambda,lambda,Core.dimension,dimension,Kernel._integer,CNULL);
    } 
  
  { set_range_property(Kernel.formula,Kernel._method,Core._lambda);
    ephemeral_class(Core._ephemeral_object);
    final_class(Kernel._method);
    } 
  
  { (Core.execute = property::make("execute",1,claire.it,Kernel._any,0));
    ;} 
  
  { (Core.debug = property::make("debug",2,claire.it,Kernel._any,0));
    ;} 
  
  { (Core.eval_message = property::make("eval_message",1,claire.it,Kernel._any,0));
    ;} 
  
  { (Core.noeval_message = property::make("noeval_message",1,Core.it,Kernel._any,0));
    ;} 
  
  { (Core.eval = property::make("eval",1,claire.it,Kernel._any,0));
    ;} 
  
  { (Core.call = property::make("call",1,claire.it,Kernel._any,0));
    ;} 
  
  { (Core.self_eval = property::make("self_eval",2,claire.it,Kernel._any,0));
    ;} 
  
  { (Core.read = property::make("read",2,claire.it,Kernel._any,0));
    ;} 
  
  { (Core.inlineok_ask = property::make("inlineok?",2,claire.it,Kernel._any,0));
    ;} 
  
  { (Kernel.restore_state = property::make("restore_state",1,mClaire.it,Kernel._any,0));
    ;} 
  
  { (Core.hold_ask = property::make("hold?",2,claire.it,Kernel._any,0));
    ;} 
  
  { (Core.write = property::make("write",1,claire.it,Kernel._any,0));
    ;} 
  
  { (Core.range_is_wrong = property::make("range_is_wrong",1,claire.it,Kernel._any,0));
    ;} 
  
  { (Core.update_plus = property::make("update+",1,Core.it,Kernel._any,0));
    ;} 
  
  { (Core.update_dash = property::make("update-",1,Core.it,Kernel._any,0));
    ;} 
  
  { (Core.add_value = property::make("add_value",1,claire.it,Kernel._any,0));
    ;} 
  
  { (Core.known_ask = property::make("known?",1,claire.it,Kernel._any,0));
    ;} 
  
  { (Core.unknown_ask = property::make("unknown?",1,claire.it,Kernel._any,0));
    ;} 
  
  { (Core.erase = property::make("erase",2,claire.it,Kernel._any,0));
    ;} 
  
  { (Core.set_range = property::make("set_range",2,claire.it,Kernel._any,0));
    ;} 
  
  { (Core.put_store = property::make("put_store",2,claire.it,Kernel._any,0));
    ;} 
  
  { (Core.matching_ask = property::make("matching?",1,Core.it,Kernel._any,0));
    ;} 
  
  { (Core.vmatch_ask = property::make("vmatch?",1,Core.it,Kernel._any,0));
    ;} 
  
  { (Core.tmatch_ask = property::make("tmatch?",1,Core.it,Kernel._any,0));
    ;} 
  
  { (Core.find_which = property::make("find_which",1,Core.it,Kernel._any,0));
    ;} 
  
  { (Core.main = property::make("main",2,claire.it,Kernel._any,0));
    ;} 
  
  Core.eval_message->addMethod(list::domain(4,Kernel._property,
    Kernel._object,
    Kernel._integer,
    Kernel._boolean),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(eval_message_property,"eval_message_property"));
  
  Core.noeval_message->addMethod(list::domain(2,Kernel._property,Kernel._integer),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(noeval_message_property2,"noeval_message_property2"));
  
  Core.execute->addMethod(list::domain(3,Kernel._method,Kernel._integer,Kernel._boolean),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(execute_method,"execute_method"));
  
  Core.eval->addMethod(list::domain(1,Kernel._any),Kernel._any,
  	NEW_ALLOC,_function_(eval_any,"eval_any"));
  
  Core.self_eval->addMethod(list::domain(1,Kernel._object),Kernel._any,
  	RETURN_ARG,_function_(self_eval_ClaireObject,"self_eval_ClaireObject"));
  
  { ClaireFunction * f = ((ClaireFunction *) _function_(self_eval_ClaireObject,"self_eval_ClaireObject"));
    { ITERATE(x);
      bag *x_support;
      x_support = Kernel._class->instances;
      for (START(x_support); NEXT(x);)
      (OBJECT(ClaireClass,x)->evaluate = f);
      } 
    } 
  
  Core.inlineok_ask->addMethod(list::domain(2,Kernel._method,Kernel._string),Kernel._method,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+SAFE_RESULT,_function_(inlineok_ask_method,"inlineok?_method"));
  
  Kernel.get->addMethod(list::domain(2,Kernel._slot,Kernel._object),Kernel._any,
  	0,_function_(get_slot,"get_slot"));
  
  Kernel.put->addMethod(list::domain(3,Kernel._slot,Kernel._object,Kernel._any),Kernel._any,
  	SLOT_UPDATE+RETURN_ARG,_function_(put_slot,"put_slot"));
  
  Kernel.get->addMethod(list::domain(2,Kernel._property,Kernel._object),Kernel._any,
  	NEW_ALLOC,_function_(get_property,"get_property"));
  
  Kernel.funcall->addMethod(list::domain(2,Kernel._property,Kernel._any),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(funcall_property,"funcall_property"));
  
  Core.read->addMethod(list::domain(2,Kernel._property,Kernel._object),Kernel._any,
  	NEW_ALLOC,_function_(read_property,"read_property"));
  
  Core.hold_ask->addMethod(list::domain(3,Kernel._property,Kernel._object,Kernel._any),Kernel._boolean,
  	NEW_ALLOC,_function_(hold_ask_property,"hold?_property"));
  
  Core.write->addMethod(list::domain(3,Kernel._property,Kernel._object,Kernel._any),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(write_property,"write_property"));
  
  Core.range_is_wrong->addMethod(list::domain(2,Kernel._slot,Kernel._any),Kernel._any,
  	0,_function_(range_is_wrong_slot,"range_is_wrong_slot"));
  
  Kernel.put->addMethod(list::domain(5,Kernel._property,
    Kernel._object,
    Kernel._integer,
    Kernel._class,
    Kernel._any),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(put_property1,"put_property1"));
  
  Core.update->addMethod(list::domain(5,Kernel._property,
    Kernel._object,
    Kernel._integer,
    Kernel._class,
    Kernel._any),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(update_property,"update_property"));
  
  Core.update_plus->addMethod(list::domain(3,Kernel._relation,Kernel._any,Kernel._any),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(update_plus_relation,"update+_relation"));
  
  Core.update_dash->addMethod(list::domain(3,Kernel._relation,Kernel._any,Kernel._any),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(update_dash_relation,"update-_relation"));
  
  Kernel.add_I->addMethod(list::domain(4,Kernel._property,
    Kernel._object,
    Kernel._integer,
    Kernel._any),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(add_I_property,"add!_property"));
  
  Core.add_value->addMethod(list::domain(5,Kernel._property,
    Kernel._object,
    Kernel._integer,
    Kernel._bag,
    Kernel._any),Kernel._boolean,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(add_value_property,"add_value_property"));
  
  Kernel.add->addMethod(list::domain(3,Kernel._property,Kernel._object,Kernel._any),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(add_property,"add_property"));
  
  Core.known_ask->addMethod(list::domain(2,Kernel._property,Kernel._object),Kernel._boolean,
  	NEW_ALLOC,_function_(known_ask_property,"known?_property"));
  
  Core.unknown_ask->addMethod(list::domain(2,Kernel._property,Kernel._object),Kernel._boolean,
  	NEW_ALLOC,_function_(unknown_ask_property,"unknown?_property"));
  
  Kernel._delete->addMethod(list::domain(3,Kernel._property,Kernel._object,Kernel._any),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(delete_property,"delete_property"));
  
  Core.erase->addMethod(list::domain(2,Kernel._property,Kernel._object),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(erase_property,"erase_property"));
  
  Core.set_range->addMethod(list::domain(3,Kernel._property,Kernel._class,Kernel._type),Kernel._void,
  	NEW_ALLOC+SLOT_UPDATE+RETURN_ARG,_function_(set_range_property,"set_range_property"));
  
  Core.put_store->addMethod(list::domain(4,Kernel._property,
    Kernel._object,
    Kernel._any,
    Kernel._boolean),Kernel._void,
  	NEW_ALLOC+SLOT_UPDATE+RETURN_ARG,_function_(put_store_property2,"put_store_property2"));
  
  { (Core.multi_ask = property::make("multi?",1,claire.it,Kernel._any,0));
    ;} 
  
  Core.multi_ask->addMethod(list::domain(1,Kernel._any),Kernel._boolean,
  	0,_function_(multi_ask_any,"multi?_any"));
  
  Kernel.fastcall->addMethod(list::domain(3,Kernel._relation,Kernel._any,Kernel._any),Kernel._void,
  	NEW_ALLOC,_function_(fastcall_relation2,"fastcall_relation2"));
  
  { (Core.join = (operation *) Kernel._operation->instantiate("join",claire.it));
    ;} 
  
  Core.insert_definition->addMethod(list::domain(2,Kernel._property,Kernel._restriction),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(insert_definition_property,"insert_definition_property"));
  
  Core.initialize->addMethod(list::domain(3,Kernel._restriction,Kernel._class,Kernel._list),Kernel._list,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(initialize_restriction1,"initialize_restriction1"));
  
  Core.uniform->addMethod(list::domain(1,Kernel._restriction),Kernel._boolean,
  	NEW_ALLOC,_function_(uniform_restriction,"uniform_restriction"));
  
  Core.uniform->addMethod(list::domain(1,Kernel._property),Kernel._boolean,
  	NEW_ALLOC,_function_(uniform_property,"uniform_property"));
  
  Core.initialize->addMethod(list::domain(2,Kernel._restriction,Kernel._list),Kernel._list,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(initialize_restriction2,"initialize_restriction2"));
  
  Core.hashinsert->addMethod(list::domain(1,Kernel._restriction),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(hashinsert_restriction,"hashinsert_restriction"));
  
  Core.hashinsert->addMethod(list::domain(2,Kernel._class,Kernel._method),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(hashinsert_class,"hashinsert_class"));
  
  Core.hashinsert->addMethod(list::domain(2,Kernel._list,Kernel._method),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+RETURN_ARG,_function_(hashinsert_list,"hashinsert_list"));
  
  Core.hashget->addMethod(list::domain(2,Kernel._class,Kernel._property),Kernel._object,
  	RETURN_ARG,_function_(hashget_class,"hashget_class"));
  
  Core.join->addMethod(list::domain(2,Kernel._list,Kernel._list),Kernel._boolean,
  	NEW_ALLOC,_function_(join_list,"join_list"));
  
  Core._at->addMethod(list::domain(2,Kernel._property,Kernel._class),Kernel._object,
  	NEW_ALLOC+RETURN_ARG,_function_(_at_property1,"@_property1"));
  
  Core._at->addMethod(list::domain(2,Kernel._property,Kernel._list),Kernel._object,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(_at_property2,"@_property2"));
  
  Core.matching_ask->addMethod(list::domain(3,Kernel._list,Kernel._integer,Kernel._integer),Kernel._boolean,
  	NEW_ALLOC,_function_(matching_ask_list,"matching?_list"));
  
  Core.vmatch_ask->addMethod(list::domain(3,Kernel._any,Kernel._any,Kernel._integer),Kernel._boolean,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(vmatch_ask_any,"vmatch?_any"));
  
  Core.tmatch_ask->addMethod(list::domain(2,Kernel._list,Kernel._list),Kernel._boolean,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(tmatch_ask_list,"tmatch?_list"));
  
  Core.tmatch_ask->addMethod(list::domain(3,Kernel._any,Kernel._any,Kernel._list),Kernel._boolean,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(tmatch_ask_any,"tmatch?_any"));
  
  Core.find_which->addMethod(list::domain(3,Kernel._property,Kernel._integer,Kernel._class),Kernel._object,
  	RETURN_ARG,_function_(find_which_property,"find_which_property"));
  
  Core.find_which->addMethod(list::domain(4,Kernel._list,
    Kernel._class,
    Kernel._integer,
    Kernel._integer),Kernel._object,
  	0,_function_(find_which_list,"find_which_list"));
  
  Core.find_which->addMethod(list::domain(4,Kernel._class,
    Kernel._list,
    Kernel._integer,
    Kernel._integer),Kernel._object,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(find_which_class,"find_which_class"));
  /*  
  { (ClEnv->version = 3.4);
    princ_string("-- CLAIRE run-time library v 3.");
    princ_float(3.4);
    princ_string(" [os: ");
    princ_string("ntv");
    princ_string(", C++:");
    princ_string("MS VC++");
    princ_string(" ] --\n");
    } 
  */
  Core.release->addMethod(list::domain(1,Kernel._void),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(release_void,"release_void"));
  
  Core.about->addMethod(list::domain(1,Kernel._void),Kernel._any,
  	0,_function_(about_void,"about_void"));
  
  Core.get_args->addMethod(list::domain(1,Kernel._integer),Kernel._list,
  	NEW_ALLOC,_function_(get_args_integer,"get_args_integer"));
  
  Kernel.funcall->addMethod(list::domain(2,Kernel._method,Kernel._any),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(funcall_method1,"funcall_method1"));
  
  Kernel.funcall->addMethod(list::domain(3,Kernel._method,Kernel._any,Kernel._any),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(funcall_method2,"funcall_method2"));
  
  Kernel.funcall->addMethod(list::domain(4,Kernel._method,
    Kernel._any,
    Kernel._any,
    Kernel._any),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(funcall_method3,"funcall_method3"));
  
  Core.apply->addMethod(list::domain(3,Kernel._function,Kernel._list,Kernel._list),Kernel._any,
  	0,_function_(apply_function,"apply_function"));
  
  Core.call->addMethod(list::domain(2,Kernel._property,Kernel._listargs),Kernel._any,
  	NEW_ALLOC+SLOT_UPDATE,_function_(call_property,"call_property"));
  
  Core.apply->addMethod(list::domain(2,Kernel._property,Kernel._list),Kernel._any,
  	NEW_ALLOC+SLOT_UPDATE,_function_(apply_property,"apply_property"));
  
  Core.apply->addMethod(list::domain(2,Kernel._method,Kernel._list),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(apply_method,"apply_method"));
  
  Core.push_debug->addMethod(list::domain(3,Kernel._property,Kernel._integer,Kernel._integer),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(push_debug_property,"push_debug_property"));
  
  Core.pop_debug->addMethod(list::domain(3,Kernel._property,Kernel._integer,Kernel._any),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(pop_debug_property,"pop_debug_property"));
  
  Core.tr_indent->addMethod(list::domain(2,Kernel._boolean,Kernel._integer),Kernel._void,
  	0,_function_(tr_indent_boolean,"tr_indent_boolean"));
  
  Core.identified_ask->addMethod(list::domain(1,Kernel._class),Kernel._boolean,
  	0,_function_(identified_ask_class,"identified?_class"));
  
  Core.identical_ask->addMethod(list::domain(2,Kernel._any,Kernel._any),Kernel._boolean,
  	0,_function_(identical_ask_any,"identical?_any"));
  
  Kernel.put->addMethod(list::domain(3,Kernel._property,Kernel._object,Kernel._any),Kernel._any,
  	NEW_ALLOC+SLOT_UPDATE+RETURN_ARG,_function_(put_property2,"put_property2"));
  
  Core.add_value->addMethod(list::domain(3,Kernel._property,Kernel._object,Kernel._any),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(add_value_property3,"add_value_property3"));
  
  (Kernel.nth->addMethod(list::domain(2,Kernel._table,Kernel._any),Kernel._any,
  	0,_function_(nth_table1,"nth_table1"))->typing = _oid_(_function_(nth_table1_type,"nth_table1_type")));
  
  (Kernel.get->addMethod(list::domain(2,Kernel._table,Kernel._any),Kernel._any,
  	RETURN_ARG,_function_(get_table,"get_table"))->typing = _oid_(_function_(get_table_type,"get_table_type")));
  
  Kernel.nth_equal->addMethod(list::domain(3,Kernel._table,Kernel._any,Kernel._any),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(nth_equal_table1,"nth=_table1"));
  
  Kernel.nth_put->addMethod(list::domain(3,Kernel._table,Kernel._any,Kernel._any),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(nth_put_table,"nth_put_table"));
  
  Kernel.put->addMethod(list::domain(3,Kernel._table,Kernel._any,Kernel._any),Kernel._void,
  	BAG_UPDATE,_function_(put_table,"put_table"));
  
  Kernel.add->addMethod(list::domain(3,Kernel._table,Kernel._any,Kernel._any),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(add_table,"add_table"));
  
  Kernel.add_I->addMethod(list::domain(3,Kernel._table,Kernel._any,Kernel._any),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(add_I_table,"add!_table"));
  
  Core.add_value->addMethod(list::domain(4,Kernel._table,
    Kernel._integer,
    Kernel._bag,
    Kernel._any),Kernel._boolean,
  	NEW_ALLOC+BAG_UPDATE,_function_(add_value_array,"add_value_array"));
  
  Core.add_value->addMethod(list::domain(3,Kernel._table,Kernel._any,Kernel._any),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE,_function_(add_value_table3,"add_value_table3"));
  
  Kernel._delete->addMethod(list::domain(3,Kernel._table,Kernel._any,Kernel._any),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(delete_table,"delete_table"));
  
  (Kernel.nth->addMethod(list::domain(3,Kernel._table,Kernel._any,Kernel._any),Kernel._any,
  	0,_function_(nth_table2,"nth_table2"))->typing = _oid_(_function_(nth_table2_type,"nth_table2_type")));
  
  Kernel.nth_equal->addMethod(list::domain(4,Kernel._table,
    Kernel._any,
    Kernel._any,
    Kernel._any),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(nth_equal_table2,"nth=_table2"));
  
  Core.get_index->addMethod(list::domain(2,Kernel._table,Kernel._any),Kernel._integer,
  	RETURN_ARG,_function_(get_index_table1,"get_index_table1"));
  
  Core.get_index->addMethod(list::domain(3,Kernel._table,Kernel._integer,Kernel._integer),Kernel._integer,
  	0,_function_(get_index_table2,"get_index_table2"));
  
  Core.erase->addMethod(list::domain(1,Kernel._table),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+RETURN_ARG,_function_(erase_table,"erase_table"));
  
  Core.make_table->addMethod(list::domain(3,Kernel._type,Kernel._type,Kernel._any),Kernel._table,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(make_table_type,"make_table_type"));
  
  { (Core.StopProperty = (table *) Kernel._table->instantiate("StopProperty",Core.it));
    (Core.StopProperty->multivalued_ask = Kernel._list);
    (Core.StopProperty->range = Kernel._list);
    (Core.StopProperty->params = _oid_(Kernel._any));
    (Core.StopProperty->domain = Kernel._property);
    (Core.StopProperty->graph = make_list_integer(29,CNULL));
    (Core.StopProperty->DEFAULT = CNULL);
    } 
  
  Kernel.funcall->addMethod(list::domain(2,Core._lambda,Kernel._any),Kernel._any,
  	NEW_ALLOC,_function_(funcall_lambda1,"funcall_lambda1"));
  
  Kernel.funcall->addMethod(list::domain(3,Core._lambda,Kernel._any,Kernel._any),Kernel._any,
  	NEW_ALLOC,_function_(funcall_lambda2,"funcall_lambda2"));
  
  Kernel.funcall->addMethod(list::domain(4,Core._lambda,
    Kernel._any,
    Kernel._any,
    Kernel._any),Kernel._any,
  	NEW_ALLOC,_function_(funcall_lambda3,"funcall_lambda3"));
  
  { (Core.pname = property::make("pname",2,mClaire.it,Kernel._any,0));
    ;} 
  
  Core.check_inverse->addMethod(list::domain(2,Kernel._any,Kernel._any),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(check_inverse_any,"check_inverse_any"));
  
  { (Kernel._relation->open = 0);
    update_property(Kernel.inverse,
      Kernel.inverse,
      8,
      Kernel._object,
      _oid_(Kernel.inverse));
    (Kernel.inverse->if_write = _oid_(CLREAD(method,_at_property1(Core.check_inverse,Kernel._any),functional)));
    } 
  
  Core.invert->addMethod(list::domain(2,Kernel._relation,Kernel._any),Kernel._bag,
  	NEW_ALLOC,_function_(invert_relation,"invert_relation"));
  
  Core.domain_I->addMethod(list::domain(1,Kernel._restriction),Kernel._class,
  	NEW_ALLOC+RETURN_ARG,_function_(domain_I_restriction,"domain!_restriction"));
  
  Core.methods->addMethod(list::domain(2,Kernel._class,Kernel._class),Kernel._set,
  	NEW_ALLOC,_function_(methods_class,"methods_class"));
  
  { (Core.reify = property::make("reify",1,claire.it,Kernel._any,0));
    ;} 
  
  Core.reify->addMethod(list::domain(1,Kernel._listargs),Kernel._void,
  	SLOT_UPDATE+RETURN_ARG,_function_(reify_listargs,"reify_listargs"));
  
  { (Core._star_stararg = property::make("**arg",0,Core.it,Kernel._any,0));
    (Core._star_stararg->open = 0);
    ;} 
  
  { (Core.args = property::make("args",0,claire.it,Kernel._any,0));
    (Core.args->open = 0);
    ;} 
  
  { (Kernel.value = property::make("value",1,claire.it,Kernel._any,0));
    ;} 
  
  { (Core._general_error = ClaireClass::make("general_error",Kernel._error,claire.it));
    CL_ADD_SLOT(Core._general_error,general_error,Core.cause,cause,Kernel._any,CNULL);
    CL_ADD_SLOT(Core._general_error,general_error,Kernel.arg,arg,Kernel._any,CNULL);
    } 
  
  Kernel.self_print->addMethod(list::domain(1,Core._general_error),Kernel._void,
  	NEW_ALLOC+SLOT_UPDATE,_function_(self_print_general_error_Core,"self_print_general_error_Core"));
  
  { (Core._read_slot_error = ClaireClass::make("read_slot_error",Kernel._error,claire.it));
    CL_ADD_SLOT(Core._read_slot_error,read_slot_error,Kernel.arg,arg,Kernel._any,CNULL);
    CL_ADD_SLOT(Core._read_slot_error,read_slot_error,Core.wrong,wrong,Kernel._any,CNULL);
    } 
  
  Kernel.self_print->addMethod(list::domain(1,Core._read_slot_error),Kernel._void,
  	NEW_ALLOC,_function_(self_print_read_slot_error_Core,"self_print_read_slot_error_Core"));
  
  { (Core._range_error = ClaireClass::make("range_error",Kernel._error,claire.it));
    CL_ADD_SLOT(Core._range_error,range_error,Core.cause,cause,Kernel._any,CNULL);
    CL_ADD_SLOT(Core._range_error,range_error,Kernel.arg,arg,Kernel._any,CNULL);
    CL_ADD_SLOT(Core._range_error,range_error,Core.wrong,wrong,Kernel._any,CNULL);
    } 
  
  Kernel.self_print->addMethod(list::domain(1,Core._range_error),Kernel._void,
  	NEW_ALLOC,_function_(self_print_range_error_Core,"self_print_range_error_Core"));
  
  { (Core._selector_error = ClaireClass::make("selector_error",Kernel._error,claire.it));
    CL_ADD_SLOT(Core._selector_error,selector_error,Kernel.selector,selector,Kernel._any,CNULL);
    CL_ADD_SLOT(Core._selector_error,selector_error,Kernel.arg,arg,Kernel._any,CNULL);
    } 
  
  Kernel.self_print->addMethod(list::domain(1,Core._selector_error),Kernel._void,
  	NEW_ALLOC,_function_(self_print_selector_error_Core,"self_print_selector_error_Core"));
  
  { (Core._return_error = ClaireClass::make("return_error",Kernel._error,claire.it));
    CL_ADD_SLOT(Core._return_error,return_error,Kernel.arg,arg,Kernel._any,CNULL);
    } 
  
  Kernel.self_print->addMethod(list::domain(1,Core._return_error),Kernel._void,
  	0,_function_(self_print_return_error_Core,"self_print_return_error_Core"));
  
  Kernel.self_print->addMethod(list::domain(1,Kernel._system_error),Kernel._void,
  	NEW_ALLOC+SLOT_UPDATE,_function_(self_print_system_error_Core,"self_print_system_error_Core"));
  
  (Core._contradiction = ClaireClass::make("contradiction",Kernel._exception,claire.it));
  
  Kernel.self_print->addMethod(list::domain(1,Core._contradiction),Kernel._void,
  	0,_function_(self_print_contradiction_Core,"self_print_contradiction_Core"));
  
  Core.format->addMethod(list::domain(2,Kernel._string,Kernel._list),Kernel._void,
  	NEW_ALLOC+SLOT_UPDATE,_function_(format_string,"format_string"));
  
  Core.tformat->addMethod(list::domain(3,Kernel._string,Kernel._integer,Kernel._list),Kernel._any,
  	NEW_ALLOC+SLOT_UPDATE,_function_(tformat_string,"tformat_string"));
  
  Kernel.princ->addMethod(list::domain(1,Kernel._bag),Kernel._void,
  	NEW_ALLOC+SLOT_UPDATE,_function_(princ_bag,"princ_bag"));
  
  { (Core._global_variable = ClaireClass::make("global_variable",Kernel._system_thing,claire.it));
    CL_ADD_SLOT(Core._global_variable,global_variable,Kernel.value,value,Kernel._any,CNULL);
    CL_ADD_SLOT(Core._global_variable,global_variable,Kernel.range,range,Kernel._type,_oid_(Kernel._any));
    CL_ADD_SLOT(Core._global_variable,global_variable,Kernel.store_ask,store_ask,Kernel._boolean,Kernel.cfalse);
    } 
  
  Kernel.close->addMethod(list::domain(1,Core._global_variable),Core._global_variable,
  	SAFE_RESULT,_function_(close_global_variable,"close_global_variable"));
  
  { (Core._inf_equal2 = (operation *) Kernel._operation->instantiate("<=2",claire.it));
    ;} 
  
  { global_variable * _CL_obj = (Core.contradiction_occurs = (global_variable *) Core._global_variable->instantiate("contradiction_occurs",claire.it));
    (_CL_obj->range = Core._contradiction);
    (_CL_obj->value = _oid_(new_object_class(Core._contradiction)));
    close_global_variable(_CL_obj);
    } 
  
  Core.contradiction_I->addMethod(list::domain(1,Kernel._void),Kernel._void,
  	RETURN_ARG,_function_(contradiction_I_void,"contradiction!_void"));
  
  { global_variable * _CL_obj = (Core.nil = (global_variable *) Core._global_variable->instantiate("nil",claire.it));
    (_CL_obj->range = Kernel.emptySet);
    (_CL_obj->value = _oid_(Kernel.nil));
    close_global_variable(_CL_obj);
    } 
  
  { global_variable * _CL_obj = (Core.claire_date = (global_variable *) Core._global_variable->instantiate("claire_date",claire.it));
    (_CL_obj->range = Kernel._string);
    (_CL_obj->value = _string_("Sat Oct 16 06:53:25 2004\n"));
    close_global_variable(_CL_obj);
    } 
  
  { (Core._I_equal = (operation *) Kernel._operation->instantiate("!=",claire.it));
    (Core._I_equal->precedence = 60);
    ;} 
  
  { (Kernel._inf_inf = (operation *) Kernel._operation->instantiate("<<",claire.it));
    ;} 
  
  { (Core._sup_sup = (operation *) Kernel._operation->instantiate(">>",claire.it));
    ;} 
  
  { (Core.ClaireAnd = (operation *) Kernel._operation->instantiate("and",claire.it));
    ;} 
  
  { (Core.ClaireOr = (operation *) Kernel._operation->instantiate("or",claire.it));
    ;} 
  
  { (Core.U = (operation *) Kernel._operation->instantiate("U",claire.it));
    (Core.U->precedence = 50);
    ;} 
  
  { (Core.less_ask = (operation *) Kernel._operation->instantiate("less?",claire.it));
    (Core.less_ask->precedence = 60);
    (Core.less_ask->range = Kernel._boolean);
    ;} 
  
  { (Core._and = (operation *) Kernel._operation->instantiate("&",claire.it));
    ;} 
  
  { (Core.min = (operation *) Kernel._operation->instantiate("min",claire.it));
    (Core.min->precedence = 20);
    ;} 
  
  { (Core.max = (operation *) Kernel._operation->instantiate("max",claire.it));
    (Core.max->precedence = 20);
    ;} 
  
  { (Core.meet = (operation *) Kernel._operation->instantiate("meet",claire.it));
    ;} 
  
  { (Core.inherit_ask = (operation *) Kernel._operation->instantiate("inherit?",claire.it));
    ;} 
  
  { (Core.cpstack = property::make("cpstack",1,Core.it,Kernel._any,0));
    ;} 
  
  { (Core._pretty_printer = ClaireClass::make("pretty_printer",Kernel._thing,claire.it));
    CL_ADD_SLOT(Core._pretty_printer,pretty_printer,Core.cpretty,cpretty,Kernel._port,CNULL);
    CL_ADD_SLOT(Core._pretty_printer,pretty_printer,Core.cprevious,cprevious,Kernel._integer,0);
    CL_ADD_SLOT(Core._pretty_printer,pretty_printer,Kernel.index,index,Kernel._integer,0);
    CL_ADD_SLOT(Core._pretty_printer,pretty_printer,Core.width,width,Kernel._integer,75);
    CL_ADD_SLOT(Core._pretty_printer,pretty_printer,Core.pprint,pprint,Kernel._boolean,Kernel.cfalse);
    CL_ADD_SLOT(Core._pretty_printer,pretty_printer,Core.pbreak,pbreak,Kernel._boolean,Kernel.cfalse);
    CL_ADD_SLOT(Core._pretty_printer,pretty_printer,Core.cpstack,cpstack,Kernel._list,CNULL);
    } 
  
  { (Core.pretty = (pretty_printer *) Core._pretty_printer->instantiate("pretty",claire.it));
    (Core.pretty->cpretty = port_I_void());
    (Core.pretty->cpstack = Kernel.nil);
    ;} 
  
  { (Core.apply_self_print = property::make("apply_self_print",1,claire.it,Kernel._any,0));
    ;} 
  
  { (Core.short_enough = property::make("short_enough",1,claire.it,Kernel._any,0));
    ;} 
  
  { (Kernel.print = property::make("print",1,claire.it,Kernel._any,0));
    ;} 
  
  Core.print_in_string->addMethod(list::domain(1,Kernel._void),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(print_in_string_void,"print_in_string_void"));
  
  Core.end_of_string->addMethod(list::domain(1,Kernel._void),Kernel._string,
  	SLOT_UPDATE,_function_(end_of_print_void,"end_of_print_void"));
  
  Core.buffer_length->addMethod(list::domain(1,Kernel._void),Kernel._integer,
  	0,_function_(buffer_length_void,"buffer_length_void"));
  
  Core.buffer_set_length->addMethod(list::domain(1,Kernel._integer),Kernel._void,
  	0,_function_(buffer_set_length_integer,"buffer_set_length_integer"));
  
  Core.apply_self_print->addMethod(list::domain(1,Kernel._any),Kernel._void,
  	NEW_ALLOC+SLOT_UPDATE+RETURN_ARG,_function_(apply_self_print_any,"apply_self_print_any"));
  
  Kernel.self_print->addMethod(list::domain(1,Kernel._any),Kernel._void,
  	NEW_ALLOC+SLOT_UPDATE,_function_(self_print_any_Core,"self_print_any_Core"));
  
  Kernel.self_print->addMethod(list::domain(1,Kernel._boolean),Kernel._void,
  	0,_function_(self_print_boolean_Core,"self_print_boolean_Core"));
  
  Kernel.self_print->addMethod(list::domain(1,Kernel._function),Kernel._void,
  	0,_function_(self_print_function_Core,"self_print_function_Core"));
  
  Kernel.self_print->addMethod(list::domain(1,Kernel._restriction),Kernel._void,
  	NEW_ALLOC,_function_(self_print_restriction_Core,"self_print_restriction_Core"));
  
  (Core._much_too_far = ClaireClass::make("much_too_far",Kernel._error,claire.it));
  
  Kernel.print->addMethod(list::domain(1,Kernel._any),Kernel._void,
  	NEW_ALLOC+SLOT_UPDATE,_function_(print_any,"print_any"));
  
  Core.short_enough->addMethod(list::domain(1,Kernel._integer),Kernel._boolean,
  	0,_function_(short_enough_integer,"short_enough_integer"));
  
  Core.complete_I->addMethod(list::domain(1,Kernel._object),Kernel._object,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(complete_I_object,"complete_I_object"));
  
  { (Core.kill_I = property::make("kill!",2,claire.it,Kernel._any,0));
    ;} 
  
  Core.kill_I->addMethod(list::domain(1,Kernel._any),Kernel._any,
  	0,_function_(kill_I_any,"kill_I_any"));
  
  Core.NOT->addMethod(list::domain(1,Kernel._any),Kernel._boolean,
  	0,_function_(not_any,"not_any"));
  
  Core._I_equal->addMethod(list::domain(2,Kernel._any,Kernel._any),Kernel._boolean,
  	0,_function_(_I_equal_any,"!=_any"));
  
  Core.owner->addMethod(list::domain(1,Kernel._any),Kernel._class,
  	0,_function_(owner_any,"owner_any"));
  
  Core.known_ask->addMethod(list::domain(1,Kernel._any),Kernel._boolean,
  	0,_function_(known_ask_any,"known?_any"));
  
  Core.unknown_ask->addMethod(list::domain(1,Kernel._any),Kernel._boolean,
  	0,_function_(unknown_ask_any,"unknown?_any"));
  
  Core.check_in->addMethod(list::domain(2,Kernel._any,Kernel._type),Kernel._any,
  	RETURN_ARG,_function_(check_in_any,"check_in_any"));
  
  Core.check_in->addMethod(list::domain(3,Kernel._bag,Kernel._class,Kernel._type),Kernel._bag,
  	0,_function_(check_in_bag,"check_in_bag"));
  
  Kernel._inf->addMethod(list::domain(2,Kernel._any,Kernel._any),Kernel._boolean,
  	NEW_ALLOC,_function_(_inf_any,"<_any"));
  
  Kernel._sup->addMethod(list::domain(2,Kernel._any,Kernel._any),Kernel._boolean,
  	NEW_ALLOC,_function_(_sup_any,">_any"));
  
  Kernel.ephemeral->addMethod(list::domain(1,Kernel._class),Kernel._any,
  	SLOT_UPDATE+RETURN_ARG,_function_(ephemeral_class,"ephemeral_class"));
  
  Kernel.ABSTRACT->addMethod(list::domain(1,Kernel._class),Kernel._any,
  	SLOT_UPDATE+SAFE_RESULT,_function_(abstract_class,"abstract_class"));
  
  Kernel.FINAL->addMethod(list::domain(1,Kernel._class),Kernel._any,
  	SLOT_UPDATE+SAFE_RESULT,_function_(final_class,"final_class"));
  
  (Core.NEW->addMethod(list::domain(1,Kernel._class),Kernel._object,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(new_class1,"new_class1"))->typing = _oid_(_function_(new_class1_type,"new_class1_type")));
  
  (Core.NEW->addMethod(list::domain(2,Kernel._class,Kernel._symbol),Kernel._thing,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(new_class2,"new_class2"))->typing = _oid_(_function_(new_class2_type,"new_class2_type")));
  
  (Core.new_I->addMethod(list::domain(1,Kernel._class),Kernel._object,
  	NEW_ALLOC,_function_(new_object_class,"new_object_class"))->typing = _oid_(_function_(new_object_class_type,"new_object_class_type")));
  
  (Core.new_I->addMethod(list::domain(2,Kernel._class,Kernel._symbol),Kernel._thing,
  	NEW_ALLOC,_function_(new_thing_class,"new_thing_class"))->typing = _oid_(_function_(new_thing_class_type,"new_thing_class_type")));
  
  Core.meet->addMethod(list::domain(2,Kernel._class,Kernel._class),Kernel._class,
  	RETURN_ARG,_function_(meet_class,"meet_class"));
  
  Core.inherit_ask->addMethod(list::domain(2,Kernel._class,Kernel._class),Kernel._boolean,
  	0,_function_(inherit_ask_class,"inherit?_class"));
  
  Core.class_I->addMethod(list::domain(2,Kernel._symbol,Kernel._class),Kernel._class,
  	NEW_ALLOC,_function_(class_I_symbol,"class!_symbol"));
  
  Kernel.ABSTRACT->addMethod(list::domain(1,Kernel._property),Kernel._any,
  	SLOT_UPDATE+SAFE_RESULT,_function_(abstract_property,"abstract_property"));
  
  Kernel.FINAL->addMethod(list::domain(1,Kernel._relation),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(final_relation,"final_relation"));
  
  Kernel.close->addMethod(list::domain(1,Kernel._module),Kernel._module,
  	NEW_ALLOC+SLOT_UPDATE+SAFE_RESULT,_function_(close_module,"close_module"));
  
  Core.get_symbol->addMethod(list::domain(2,Kernel._module,Kernel._string),Kernel._any,
  	0,_function_(get_symbol_module,"get_symbol_module"));
  
  Core.get_symbol->addMethod(list::domain(1,Kernel._string),Kernel._any,
  	0,_function_(get_symbol_string,"get_symbol_string"));
  
  Core.gc->addMethod(list::domain(1,Kernel._void),Kernel._void,
  	0,_function_(claire_gc,"claire_gc"));
  
  Core.time_get->addMethod(list::domain(1,Kernel._void),Kernel._integer,
  	0,_function_(time_get_void,"time_get_void"));
  
  Core.time_read->addMethod(list::domain(1,Kernel._void),Kernel._integer,
  	0,_function_(time_read_void,"time_read_void"));
  
  Core.time_set->addMethod(list::domain(1,Kernel._void),Kernel._void,
  	0,_function_(time_set_void,"time_set_void"));
  
  Core.time_show->addMethod(list::domain(1,Kernel._void),Kernel._void,
  	0,_function_(time_show_void,"time_show_void"));
  
  Kernel.gensym->addMethod(list::domain(1,Kernel._void),Kernel._symbol,
  	0,_function_(gensym_void,"gensym_void"));
  
  Kernel.store->addMethod(list::domain(3,Kernel._list,Kernel._integer,Kernel._any),Kernel._any,
  	BAG_UPDATE+RETURN_ARG,_function_(store_list4,"store_list4"));
  
  Kernel.store->addMethod(list::domain(3,Kernel._array,Kernel._integer,Kernel._any),Kernel._any,
  	RETURN_ARG,_function_(store_array1,"store_array1"));
  
  Kernel.commit->addMethod(list::domain(1,Kernel._integer),Kernel._void,
  	0,_function_(commit_integer,"commit_integer"));
  
  Kernel.backtrack->addMethod(list::domain(1,Kernel._integer),Kernel._void,
  	0,_function_(backtrack_integer,"backtrack_integer"));
  
  { global_variable * _CL_obj = (Core.world_plus = (global_variable *) Core._global_variable->instantiate("world+",claire.it));
    (_CL_obj->range = Kernel.emptySet);
    (_CL_obj->value = _oid_(Kernel.choice));
    close_global_variable(_CL_obj);
    } 
  
  { global_variable * _CL_obj = (Core.world_dash = (global_variable *) Core._global_variable->instantiate("world-",claire.it));
    (_CL_obj->range = Kernel.emptySet);
    (_CL_obj->value = _oid_(Kernel.backtrack));
    close_global_variable(_CL_obj);
    } 
  
  { global_variable * _CL_obj = (Core.world_dash_I = (global_variable *) Core._global_variable->instantiate("world-!",claire.it));
    (_CL_obj->range = Kernel.emptySet);
    (_CL_obj->value = _oid_(Kernel.commit));
    close_global_variable(_CL_obj);
    } 
  
  Kernel.store->addMethod(list::domain(1,Kernel._listargs),Kernel._any,
  	SLOT_UPDATE+RETURN_ARG,_function_(store_listargs,"store_listargs"));
  
  Kernel.length->addMethod(list::domain(1,Kernel._string),Kernel._integer,
  	0,_function_(strlen,"strlen"));
  
  Core.make_function->addMethod(list::domain(1,Kernel._string),Kernel._function,
  	0,_function_(make_function_string,"make_function_string"));
  
  Kernel.symbol_I->addMethod(list::domain(1,Kernel._string),Kernel._symbol,
  	0,_function_(symbol_I_string2,"symbol!_string2"));
  
  Core.externC->addMethod(list::domain(1,Kernel._string),Kernel._void,
  	SAFE_RESULT,_function_(externC_string,"externC_string"));
  
  (Core.externC->addMethod(list::domain(2,Kernel._string,Kernel._class),Kernel._any,
  	SAFE_RESULT,_function_(externC_string2,"externC_string2"))->typing = _oid_(_function_(externC_string2_type,"externC_string2_type")));
  
  Kernel.nth_get->addMethod(list::domain(3,Kernel._string,Kernel._integer,Kernel._integer),Kernel._char,
  	RETURN_ARG,_function_(nth_get_string,"nth_get_string"));
  
  Kernel.nth_put->addMethod(list::domain(4,Kernel._string,
    Kernel._integer,
    Kernel._char,
    Kernel._integer),Kernel._void,
  	BAG_UPDATE+RETURN_ARG,_function_(nth_put_string,"nth_put_string"));
  
  Core.shell->addMethod(list::domain(1,Kernel._string),Kernel._void,
  	0,_function_(claire_shell,"claire_shell"));
  
  Core.getenv->addMethod(list::domain(1,Kernel._string),Kernel._string,
  	0,_function_(getenv_string,"getenv_string"));
  
  Core.get_value->addMethod(list::domain(1,Kernel._string),Kernel._any,
  	0,_function_(value_string,"value_string"));
  
  Core.get_value->addMethod(list::domain(2,Kernel._module,Kernel._string),Kernel._any,
  	0,_function_(value_module,"value_module"));
  
  Kernel.make_string->addMethod(list::domain(1,Kernel._symbol),Kernel._string,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(make_string_symbol,"make_string_symbol"));
  
  Kernel.self_print->addMethod(list::domain(1,Kernel._symbol),Kernel._void,
  	NEW_ALLOC+SLOT_UPDATE,_function_(self_print_symbol_Core,"self_print_symbol_Core"));
  
  (Core._plus->addMethod(list::domain(2,Kernel._integer,Kernel._integer),Kernel._integer,
  	0,_function_(_plus_integer,"+_integer"))->typing = _oid_(_function_(_plus_integer_type,"+_integer_type")));
  
  (Kernel._dash->addMethod(list::domain(2,Kernel._integer,Kernel._integer),Kernel._integer,
  	0,_function_(_dash_integer1,"-_integer1"))->typing = _oid_(_function_(_dash_integer1_type,"-_integer1_type")));
  
  Core._dash_dash_ask->addMethod(list::domain(2,Kernel._integer,Kernel._integer),Kernel._set,
  	NEW_ALLOC,_function_(sequence_integer,"sequence_integer"));
  
  Core.exit->addMethod(list::domain(1,Kernel._integer),Kernel._void,
  	0,_function_(CL_exit,"CL_exit"));
  
  Kernel._inf_inf->addMethod(list::domain(2,Kernel._integer,Kernel._integer),Kernel._integer,
  	0,_function_(_inf_inf_integer,"<<_integer"));
  
  Core._sup_sup->addMethod(list::domain(2,Kernel._integer,Kernel._integer),Kernel._integer,
  	0,_function_(_sup_sup_integer,">>_integer"));
  
  Core.ClaireAnd->addMethod(list::domain(2,Kernel._integer,Kernel._integer),Kernel._integer,
  	0,_function_(and_integer,"and_integer"));
  
  Core.ClaireOr->addMethod(list::domain(2,Kernel._integer,Kernel._integer),Kernel._integer,
  	0,_function_(or_integer,"or_integer"));
  
  Kernel._inf->addMethod(list::domain(2,Kernel._integer,Kernel._integer),Kernel._boolean,
  	0,_function_(_inf_integer,"<_integer"));
  
  Kernel._inf_equal->addMethod(list::domain(2,Kernel._integer,Kernel._integer),Kernel._boolean,
  	0,_function_(_inf_equal_integer,"<=_integer"));
  
  Kernel._sup->addMethod(list::domain(2,Kernel._integer,Kernel._integer),Kernel._boolean,
  	0,_function_(_sup_integer,">_integer"));
  
  Kernel.nth->addMethod(list::domain(2,Kernel._integer,Kernel._integer),Kernel._boolean,
  	0,_function_(nth_integer,"nth_integer"));
  
  Core.factor_ask->addMethod(list::domain(2,Kernel._integer,Kernel._integer),Kernel._boolean,
  	0,_function_(factor_ask_integer,"factor?_integer"));
  
  Core.divide_ask->addMethod(list::domain(2,Kernel._integer,Kernel._integer),Kernel._boolean,
  	0,_function_(divide_ask_integer,"divide?_integer"));
  
  (Core.Id->addMethod(list::domain(1,Kernel._any),Kernel._any,
  	RETURN_ARG,_function_(Id_any,"Id_any"))->typing = _oid_(_function_(Id_any_type,"Id_any_type")));
  
  { (Core.pair = (operation *) Kernel._operation->instantiate("pair",claire.it));
    ;} 
  
  Core.pair->addMethod(list::domain(2,Kernel._any,Kernel._any),Kernel._list,
  	NEW_ALLOC,_function_(pair_any,"pair_any"));
  
  (Core.pair_1->addMethod(list::domain(1,Kernel._list),Kernel._any,
  	RETURN_ARG,_function_(pair_1_list,"pair_1_list"))->typing = _oid_(_function_(pair_1_list_type,"pair_1_list_type")));
  
  (Core.pair_2->addMethod(list::domain(1,Kernel._list),Kernel._any,
  	RETURN_ARG,_function_(pair_2_list,"pair_2_list"))->typing = _oid_(_function_(pair_2_list_type,"pair_2_list_type")));
  
  Kernel.self_print->addFloatMethod(list::domain(1,Kernel._float),Kernel._void,
  	0,_function_(print_float,"print_float"),_function_(print_float_,"print_float_"));
  
  Core._plus->addFloatMethod(list::domain(2,Kernel._float,Kernel._float),Kernel._float,
  	0,_function_(_plus_float,"_plus_float"),_function_(_plus_float_,"_plus_float_"));
  
  Kernel._dash->addFloatMethod(list::domain(2,Kernel._float,Kernel._float),Kernel._float,
  	0,_function_(_dash_float,"_dash_float"),_function_(_dash_float_,"_dash_float_"));
  
  Kernel._star->addFloatMethod(list::domain(2,Kernel._float,Kernel._float),Kernel._float,
  	0,_function_(_star_float,"_star_float"),_function_(_star_float_,"_star_float_"));
  
  Kernel._7->addFloatMethod(list::domain(2,Kernel._float,Kernel._float),Kernel._float,
  	0,_function_(_7_float,"_7_float"),_function_(_7_float_,"_7_float_"));
  
  Kernel._dash->addFloatMethod(list::domain(1,Kernel._float),Kernel._float,
  	NEW_ALLOC,_function_(_dash_float2,"-_float2"),_function_(_dash_float2_,"-_float2_"));
  
  Core.sqrt->addFloatMethod(list::domain(1,Kernel._float),Kernel._float,
  	0,_function_(sqrt_float,"sqrt_float"),_function_(sqrt_float_,"sqrt_float_"));
  
  Kernel._exp->addFloatMethod(list::domain(2,Kernel._float,Kernel._float),Kernel._float,
  	NEW_ALLOC,_function_(_exp_float,"_exp_float"),_function_(_exp_float_,"_exp_float_"));
  
  Core.log->addFloatMethod(list::domain(1,Kernel._float),Kernel._float,
  	NEW_ALLOC,_function_(log_float,"log_float"),_function_(log_float_,"log_float_"));
  
  Core.atan->addFloatMethod(list::domain(1,Kernel._float),Kernel._float,
  	NEW_ALLOC,_function_(atan_float,"atan_float"),_function_(atan_float_,"atan_float_"));
  
  Kernel.string_I->addFloatMethod(list::domain(1,Kernel._float),Kernel._string,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(string_I_float,"string!_float"),_function_(string_I_float_,"string!_float_"));
  
  Kernel.length->addMethod(list::domain(1,Kernel._bag),Kernel._integer,
  	0,_function_(length_bag,"length_bag"));
  
  (Kernel.nth->addMethod(list::domain(2,Kernel._bag,Kernel._integer),Kernel._any,
  	RETURN_ARG,_function_(nth_bag,"nth_bag"))->typing = _oid_(_function_(nth_bag_type,"nth_bag_type")));
  
  Kernel.nth_get->addMethod(list::domain(2,Kernel._bag,Kernel._integer),Kernel._any,
  	RETURN_ARG,_function_(nth_get_bag,"nth_get_bag"));
  
  (Core.min->addMethod(list::domain(2,Kernel._method,Kernel._bag),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(min_method,"min_method"))->typing = _oid_(_function_(min_method_type,"min_method_type")));
  
  (Core.max->addMethod(list::domain(2,Kernel._method,Kernel._bag),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(max_method,"max_method"))->typing = _oid_(_function_(max_method_type,"max_method_type")));
  
  Kernel._7_plus->addMethod(list::domain(2,Kernel._bag,Kernel._bag),Kernel._list,
  	NEW_ALLOC,_function_(_7_plus_bag,"/+_bag"));
  
  (Core.last->addMethod(list::domain(1,Kernel._list),Kernel._any,
  	RETURN_ARG,_function_(last_list,"last_list"))->typing = _oid_(_function_(last_list_type,"last_list_type")));
  
  Core.rmlast->addMethod(list::domain(1,Kernel._list),Kernel._list,
  	RETURN_ARG,_function_(rmlast_list,"rmlast_list"));
  
  Kernel.nth_equal->addMethod(list::domain(3,Kernel._list,Kernel._integer,Kernel._any),Kernel._any,
  	BAG_UPDATE+RETURN_ARG,_function_(nth_set_list,"nth_set_list"));
  
  Core.car->addMethod(list::domain(1,Kernel._list),Kernel._any,
  	RETURN_ARG,_function_(car_list,"car_list"));
  
  Core.hashlist->addMethod(list::domain(1,Kernel._integer),Kernel._list,
  	NEW_ALLOC,_function_(hashlist_integer,"hashlist_integer"));
  
  Core.hashsize->addMethod(list::domain(1,Kernel._list),Kernel._integer,
  	0,_function_(hashsize_list,"hashsize_list"));
  
  Core.sort->addMethod(list::domain(2,Kernel._method,Kernel._list),Kernel._list,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(sort_method,"sort_method"));
  
  Core.quicksort->addMethod(list::domain(4,Kernel._list,
    Kernel._method,
    Kernel._integer,
    Kernel._integer),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(quicksort_list,"quicksort_list"));
  
  Core.build_powerset->addMethod(list::domain(1,Kernel._list),Kernel._set,
  	NEW_ALLOC+BAG_UPDATE,_function_(build_powerset_list,"build_powerset_list"));
  
  Core.tuple_I->addMethod(list::domain(1,Kernel._list),Kernel._tuple,
  	NEW_ALLOC,_function_(tuple_I_list,"tuple!_list"));
  
  Core.make_copy_list->addMethod(list::domain(2,Kernel._integer,Kernel._any),Kernel._list,
  	NEW_ALLOC+BAG_UPDATE,_function_(make_copy_list_integer,"make_copy_list_integer"));
  
  Core.difference->addMethod(list::domain(2,Kernel._set,Kernel._set),Kernel._set,
  	NEW_ALLOC,_function_(difference_set,"difference_set"));
  
  Kernel.nth_equal->addMethod(list::domain(3,Kernel._array,Kernel._integer,Kernel._any),Kernel._void,
  	RETURN_ARG,_function_(nth_equal_array,"nth=_array"));
  
  Kernel.self_print->addMethod(list::domain(1,Kernel._array),Kernel._void,
  	NEW_ALLOC,_function_(self_print_array_Core,"self_print_array_Core"));
  
  Kernel.self_print->addMethod(list::domain(1,Kernel._char),Kernel._void,
  	0,_function_(self_print_char_Core,"self_print_char_Core"));
  
  Kernel._inf_equal->addMethod(list::domain(2,Kernel._char,Kernel._char),Kernel._boolean,
  	0,_function_(_inf_equal_char,"<=_char"));
  
  Core.Address->addMethod(list::domain(1,Kernel._any),Kernel._integer,
  	0,_function_(CL_Address,"CL_Address"));
  
  Core.Oid->addMethod(list::domain(1,Kernel._any),Kernel._string,
  	0,_function_(CL_Oid,"CL_Oid"));
  
  Core.Oid_tilda->addMethod(list::domain(1,Kernel._string),Kernel._any,
  	0,_function_(CL_Oid_inv,"CL_Oid_inv"));
  
  { (Core.arg1 = property::make("arg1",1,claire.it,Kernel._any,0));
    ;} 
  
  { (Core.arg2 = property::make("arg2",1,claire.it,Kernel._any,0));
    ;} 
  
  { (Core._dash_dash = (operation *) Kernel._operation->instantiate("--",claire.it));
    (Core._dash_dash->precedence = Kernel._dot_dot->precedence);
    ;} 
  
  Core.finite_ask->addMethod(list::domain(1,Kernel._type),Kernel._boolean,
  	NEW_ALLOC,_function_(finite_ask_type,"finite?_type"));
  
  Core.enumerate->addMethod(list::domain(1,Kernel._any),Kernel._bag,
  	NEW_ALLOC+RETURN_ARG,_function_(enumerate_any,"enumerate_any"));
  
  { (Core._equaltype_ask = (operation *) Kernel._operation->instantiate("=type?",claire.it));
    ;} 
  
  Core._equaltype_ask->addMethod(list::domain(2,Kernel._type,Kernel._type),Kernel._boolean,
  	NEW_ALLOC,_function_(_equaltype_ask_any,"=type?_any"));
  
  Kernel.sort_I->addMethod(list::domain(1,Kernel._type),Kernel._class,
  	NEW_ALLOC+RETURN_ARG,_function_(sort_I_type,"sort!_type"));
  
  Kernel._Z->addMethod(list::domain(2,Kernel._any,Kernel._class),Kernel._boolean,
  	0,_function_(_Z_any1,"%_any1"));
  
  Core._Ztype->addMethod(list::domain(2,Kernel._any,Kernel._any),Kernel._boolean,
  	NEW_ALLOC,_function_(Ztype_any,"Ztype_any"));
  
  (Core._Type = ClaireClass::make("Type",Kernel._type,claire.it));
  
  { (Core._Union = ClaireClass::make("Union",Core._Type,claire.it));
    CL_ADD_SLOT(Core._Union,Union,Core.t1,t1,Kernel._type,CNULL);
    CL_ADD_SLOT(Core._Union,Union,Core.t2,t2,Kernel._type,CNULL);
    } 
  
  Kernel.self_print->addMethod(list::domain(1,Core._Union),Kernel._void,
  	NEW_ALLOC,_function_(self_print_Union_Core,"self_print_Union_Core"));
  
  Core.finite_ask->addMethod(list::domain(1,Core._Union),Kernel._boolean,
  	NEW_ALLOC,_function_(finite_ask_Union,"finite?_Union"));
  
  { (Core._Interval = ClaireClass::make("Interval",Core._Type,claire.it));
    CL_ADD_SLOT(Core._Interval,Interval,Core.arg1,arg1,Kernel._integer,CNULL);
    CL_ADD_SLOT(Core._Interval,Interval,Core.arg2,arg2,Kernel._integer,CNULL);
    } 
  
  Kernel.self_print->addMethod(list::domain(1,Core._Interval),Kernel._void,
  	NEW_ALLOC,_function_(self_print_Interval_Core,"self_print_Interval_Core"));
  
  Core.finite_ask->addMethod(list::domain(1,Core._Interval),Kernel._boolean,
  	0,_function_(finite_ask_Interval,"finite?_Interval"));
  
  Core._dash_dash->addMethod(list::domain(2,Kernel._integer,Kernel._integer),Core._Interval,
  	NEW_ALLOC,_function_(_dash_dash_integer,"--_integer"));
  
  { (Core._Param = ClaireClass::make("Param",Core._Type,claire.it));
    CL_ADD_SLOT(Core._Param,Param,Kernel.arg,arg,Kernel._class,CNULL);
    CL_ADD_SLOT(Core._Param,Param,Kernel.params,params,Kernel._list,CNULL);
    CL_ADD_SLOT(Core._Param,Param,Core.args,args,Kernel._list,CNULL);
    } 
  
  Kernel.self_print->addMethod(list::domain(1,Core._Param),Kernel._void,
  	NEW_ALLOC+SLOT_UPDATE,_function_(self_print_Param_Core,"self_print_Param_Core"));
  
  Core.finite_ask->addMethod(list::domain(1,Core._Param),Kernel._boolean,
  	NEW_ALLOC,_function_(finite_ask_Param,"finite?_Param"));
  
  { (Core._subtype = ClaireClass::make("subtype",Core._Type,claire.it));
    CL_ADD_SLOT(Core._subtype,subtype,Kernel.arg,arg,Kernel._class,CNULL);
    CL_ADD_SLOT(Core._subtype,subtype,Core.t1,t1,Kernel._type,CNULL);
    } 
  
  Kernel.self_print->addMethod(list::domain(1,Core._subtype),Kernel._void,
  	NEW_ALLOC,_function_(self_print_subtype_Core,"self_print_subtype_Core"));
  
  Core.finite_ask->addMethod(list::domain(1,Core._subtype),Kernel._boolean,
  	NEW_ALLOC,_function_(finite_ask_subtype,"finite?_subtype"));
  
  Kernel.nth->addMethod(list::domain(2,Kernel._class,Kernel._type),Kernel._type,
  	NEW_ALLOC,_function_(nth_class1,"nth_class1"));
  
  Kernel.nth->addMethod(list::domain(3,Kernel._class,Kernel._list,Kernel._list),Kernel._type,
  	NEW_ALLOC+SLOT_UPDATE+RETURN_ARG,_function_(nth_class2,"nth_class2"));
  
  Core.param_I->addMethod(list::domain(2,Kernel._class,Kernel._type),Kernel._type,
  	NEW_ALLOC,_function_(param_I_class,"param_I_class"));
  
  Kernel.nth->addMethod(list::domain(1,Kernel._type),Kernel._type,
  	NEW_ALLOC,_function_(nth_type,"nth_type"));
  
  Core.finite_ask->addMethod(list::domain(1,Kernel._tuple),Kernel._boolean,
  	NEW_ALLOC,_function_(finite_ask_tuple,"finite?_tuple"));
  
  { (Core._Reference = ClaireClass::make("Reference",Core._Type,claire.it));
    CL_ADD_SLOT(Core._Reference,Reference,Core.args,args,Kernel._list,CNULL);
    CL_ADD_SLOT(Core._Reference,Reference,Kernel.index,index,Kernel._integer,CNULL);
    CL_ADD_SLOT(Core._Reference,Reference,Kernel.arg,arg,Kernel._boolean,Kernel.cfalse);
    } 
  
  Kernel.self_print->addMethod(list::domain(1,Core._Reference),Kernel._void,
  	NEW_ALLOC,_function_(self_print_Reference_Core,"self_print_Reference_Core"));
  
  Kernel.get->addMethod(list::domain(2,Core._Reference,Kernel._any),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(get_Reference,"get_Reference"));
  
  Core._at->addMethod(list::domain(3,Core._Reference,Kernel._list,Kernel._any),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(_at_Reference,"@_Reference"));
  
  Kernel.set_I->addMethod(list::domain(1,Kernel._collection),Kernel._set,
  	NEW_ALLOC,_function_(set_I_collection,"set!_collection"));
  
  Core.size->addMethod(list::domain(1,Kernel._collection),Kernel._integer,
  	NEW_ALLOC,_function_(size_collection,"size_collection"));
  
  ;
  Kernel.set_I->addMethod(list::domain(1,Kernel._set),Kernel._set,
  	RETURN_ARG,_function_(set_I_set,"set!_set"));
  
  Core.size->addMethod(list::domain(1,Kernel._set),Kernel._integer,
  	0,_function_(size_set,"size_set"));
  
  Core.size->addMethod(list::domain(1,Kernel._list),Kernel._integer,
  	NEW_ALLOC,_function_(size_list2_Core,"size_list2_Core"));
  
  Kernel.set_I->addMethod(list::domain(1,Kernel._class),Kernel._set,
  	NEW_ALLOC+RETURN_ARG,_function_(set_I_class,"set!_class"));
  
  Core.size->addMethod(list::domain(1,Kernel._class),Kernel._integer,
  	SAFE_RESULT,_function_(size_class,"size_class"));
  
  Kernel.set_I->addMethod(list::domain(1,Core._Union),Kernel._set,
  	NEW_ALLOC,_function_(set_I_Union,"set!_Union"));
  
  Core.size->addMethod(list::domain(1,Core._Union),Kernel._integer,
  	NEW_ALLOC,_function_(size_Union,"size_Union"));
  
  Kernel.set_I->addMethod(list::domain(1,Core._Interval),Kernel._set,
  	NEW_ALLOC,_function_(set_I_Interval,"set!_Interval"));
  
  Core.size->addMethod(list::domain(1,Core._Interval),Kernel._integer,
  	0,_function_(size_Interval,"size_Interval"));
  
  Kernel.set_I->addMethod(list::domain(1,Core._Param),Kernel._set,
  	NEW_ALLOC,_function_(set_I_Param,"set!_Param"));
  
  Core.size->addMethod(list::domain(1,Core._Param),Kernel._integer,
  	NEW_ALLOC,_function_(size_Param,"size_Param"));
  
  Kernel.set_I->addMethod(list::domain(1,Core._subtype),Kernel._set,
  	NEW_ALLOC+BAG_UPDATE,_function_(set_I_subtype,"set!_subtype"));
  
  Core.size->addMethod(list::domain(1,Core._subtype),Kernel._integer,
  	NEW_ALLOC,_function_(size_subtype,"size_subtype"));
  
  Kernel.set_I->addMethod(list::domain(1,Kernel._tuple),Kernel._set,
  	NEW_ALLOC,_function_(set_I_tuple,"set!_tuple"));
  
  Core.size->addMethod(list::domain(1,Kernel._tuple),Kernel._integer,
  	NEW_ALLOC,_function_(size_tuple,"size_tuple"));
  
  Kernel.member_ask->addMethod(list::domain(2,Kernel._any,Kernel._type),Kernel._boolean,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(member_ask_any,"member?_any"));
  
  Core.class_I->addMethod(list::domain(1,Kernel._type),Kernel._class,
  	NEW_ALLOC+RETURN_ARG,_function_(class_I_type,"class!_type"));
  
  { ephemeral_class(Core._Union);
    ephemeral_class(Core._Param);
    ephemeral_class(Core._Interval);
    ephemeral_class(Core._subtype);
    } 
  
  Core.U->addMethod(list::domain(2,Kernel._type,Kernel._type),Kernel._type,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(U_type,"U_type"));
  
  (Kernel._dot_dot->addMethod(list::domain(2,Kernel._integer,Kernel._integer),nth_class1(Kernel._type,Kernel._integer),
  	NEW_ALLOC,_function_(_dot_dot_integer,".._integer"))->typing = _oid_(_function_(_dot_dot_integer_type,".._integer_type")));
  
  { (Core.but = (operation *) Kernel._operation->instantiate("but",claire.it));
    ;} 
  
  (Core.but->addMethod(list::domain(2,Kernel._any,Kernel._any),nth_class1(Kernel._bag,Kernel._any),
  	NEW_ALLOC,_function_(but_any,"but_any"))->typing = _oid_(_function_(but_any_type,"but_any_type")));
  
  { (Core._backslash = (operation *) Kernel._operation->instantiate("\\",claire.it));
    (Core._backslash->precedence = Core.U->precedence);
    ;} 
  
  Core._backslash->addMethod(list::domain(2,Kernel._type,Kernel._type),Kernel._set,
  	NEW_ALLOC,_function_(_backslash_type,"\\_type"));
  
  { (Core.glb = (operation *) Kernel._operation->instantiate("glb",claire.it));
    (Core.glb->precedence = Kernel._exp->precedence);
    (Core.glb->domain = Kernel._type);
    (Core.glb->range = Kernel._type);
    (Core.glb->dispatcher = 4);
    ;} 
  
  Core.glb->addMethod(list::domain(2,Kernel._set,Kernel._type),Kernel._set,
  	NEW_ALLOC,_function_(glb_set,"glb_set"));
  
  Core.glb->addMethod(list::domain(2,Core._Union,Kernel._type),Kernel._type,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(glb_Union,"glb_Union"));
  
  Core.glb->addMethod(list::domain(2,Core._Interval,Kernel._type),Kernel._type,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(glb_Interval,"glb_Interval"));
  
  Core.glb->addMethod(list::domain(2,Kernel._class,Kernel._type),Kernel._type,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(glb_class,"glb_class"));
  
  Core.glb->addMethod(list::domain(2,Core._Param,Kernel._type),Kernel._type,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(glb_Param,"glb_Param"));
  
  Core.glb->addMethod(list::domain(2,Core._subtype,Kernel._type),Kernel._type,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(glb_subtype,"glb_subtype"));
  
  Core.glb->addMethod(list::domain(2,Kernel._tuple,Kernel._type),Kernel._type,
  	NEW_ALLOC+RETURN_ARG,_function_(glb_tuple,"glb_tuple"));
  
  Core.glb->addMethod(list::domain(2,Core._Reference,Kernel._type),Kernel._type,
  	RETURN_ARG,_function_(glb_Reference,"glb_Reference"));
  
  Kernel._exp->addMethod(list::domain(2,Kernel._type,Kernel._type),Kernel._type,
  	NEW_ALLOC+RETURN_ARG,_function_(_exp_type,"^_type"));
  
  Core.join->addMethod(list::domain(2,Kernel._class,Kernel._class),Kernel._type,
  	0,_function_(join_class,"join_class"));
  
  Kernel._exp->addMethod(list::domain(2,Kernel._list,Kernel._list),Kernel._list,
  	NEW_ALLOC,_function_(_exp_list,"^_list"));
  
  Core.Uall->addMethod(list::domain(1,Kernel._list),Kernel._type,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(Uall_list,"Uall_list"));
  
  { (Core._inf_equalt = (operation *) Kernel._operation->instantiate("<=t",Core.it));
    (Core._inf_equalt->precedence = Kernel._inf_equal->precedence);
    (Core._inf_equalt->domain = Kernel._type);
    (Core._inf_equalt->range = Kernel._boolean);
    (Core._inf_equalt->dispatcher = 5);
    ;} 
  
  Core._inf_equalt->addMethod(list::domain(2,Kernel._bag,Kernel._type),Kernel._boolean,
  	NEW_ALLOC,_function_(_inf_equalt_bag2,"<=t_bag2"));
  
  Core._inf_equalt->addMethod(list::domain(2,Kernel._class,Kernel._type),Kernel._boolean,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(_inf_equalt_class,"<=t_class"));
  
  Core._inf_equalt->addMethod(list::domain(2,Core._Union,Kernel._type),Kernel._boolean,
  	NEW_ALLOC,_function_(_inf_equalt_Union,"<=t_Union"));
  
  Core._inf_equalt->addMethod(list::domain(2,Core._Interval,Kernel._type),Kernel._boolean,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(_inf_equalt_Interval,"<=t_Interval"));
  
  Core._inf_equalt->addMethod(list::domain(2,Core._subtype,Kernel._type),Kernel._boolean,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(_inf_equalt_subtype,"<=t_subtype"));
  
  Core._inf_equalt->addMethod(list::domain(2,Core._Param,Kernel._type),Kernel._boolean,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(_inf_equalt_Param,"<=t_Param"));
  
  Core._inf_equalt->addMethod(list::domain(2,Core._Reference,Kernel._type),Kernel._boolean,
  	0,_function_(_inf_equalt_Reference,"<=t_Reference"));
  
  Core._inf_equalt->addMethod(list::domain(2,Kernel._tuple,Kernel._type),Kernel._boolean,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(_inf_equalt_tuple,"<=t_tuple"));
  
  Core._inf_equalt->addMethod(list::domain(2,Kernel._type,Kernel._type),Kernel._boolean,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(_inf_equalt_type,"<=t_type"));
  
  Kernel._inf_equal->addMethod(list::domain(2,Kernel._type,Kernel._type),Kernel._boolean,
  	NEW_ALLOC,_function_(_inf_equal_type,"<=_type"));
  
  Core.member->addMethod(list::domain(1,Kernel._type),Kernel._type,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(member_type,"member_type"));
  
  Core.of_extract->addMethod(list::domain(1,Kernel._type),Kernel._type,
  	RETURN_ARG,_function_(of_extract_type,"of_extract_type"));
  
  Core._at->addMethod(list::domain(2,Kernel._type,Kernel._property),Kernel._type,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(_at_type,"@_type"));
  
  Core.unique_ask->addMethod(list::domain(1,Kernel._type),Kernel._boolean,
  	0,_function_(unique_ask_type,"unique?_type"));
  
  Core.the->addMethod(list::domain(1,Kernel._type),Kernel._any,
  	NEW_ALLOC+RETURN_ARG,_function_(the_type,"the_type"));
  
  Kernel.integer_I->addMethod(list::domain(1,nth_class1(Kernel._set,Kernel._integer)),Kernel._integer,
  	NEW_ALLOC,_function_(integer_I_set,"integer!_set"));
  
  Core.make_set->addMethod(list::domain(1,Kernel._integer),Kernel._set,
  	NEW_ALLOC,_function_(make_set_integer,"make_set_integer"));
  
  Core.abstract_type->addMethod(list::domain(1,Kernel._set),Kernel._type,
  	NEW_ALLOC,_function_(abstract_type_set,"abstract_type_set"));
  
  Core.abstract_type->addMethod(list::domain(3,Kernel._operation,Kernel._type,Kernel._type),Kernel._type,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(abstract_type_operation,"abstract_type_operation"));
  
  { set_range_property(Kernel.subclass,Kernel._class,GC_OBJECT(ClaireType,nth_class2(Kernel._set,list::alloc(Kernel._any,1,_oid_(Kernel.of)),list::alloc(1,_oid_(set::alloc(1,_oid_(Kernel._class)))))));
    set_range_property(Kernel.ancestors,Kernel._class,GC_OBJECT(ClaireType,nth_class2(Kernel._list,list::alloc(Kernel._any,1,_oid_(Kernel.of)),list::alloc(1,_oid_(set::alloc(1,_oid_(Kernel._class)))))));
    set_range_property(Kernel.descendents,Kernel._class,GC_OBJECT(ClaireType,nth_class2(Kernel._set,list::alloc(Kernel._any,1,_oid_(Kernel.of)),list::alloc(1,_oid_(set::alloc(1,_oid_(Kernel._class)))))));
    set_range_property(Kernel.definition,Kernel._property,GC_OBJECT(ClaireType,nth_class2(Kernel._list,list::alloc(Kernel._any,1,_oid_(Kernel.of)),list::alloc(1,_oid_(set::alloc(1,_oid_(Kernel._restriction)))))));
    set_range_property(Kernel.restrictions,Kernel._property,GC_OBJECT(ClaireType,nth_class2(Kernel._list,list::alloc(Kernel._any,1,_oid_(Kernel.of)),list::alloc(1,_oid_(set::alloc(1,_oid_(Kernel._restriction)))))));
    set_range_property(Kernel.domain,Kernel._restriction,GC_OBJECT(ClaireType,nth_class2(Kernel._list,list::alloc(Kernel._any,1,_oid_(Kernel.of)),list::alloc(1,_oid_(set::alloc(1,_oid_(Kernel._type)))))));
    set_range_property(Kernel.slots,Kernel._class,GC_OBJECT(ClaireType,nth_class2(Kernel._list,list::alloc(Kernel._any,1,_oid_(Kernel.of)),list::alloc(1,_oid_(set::alloc(1,_oid_(Kernel._slot)))))));
    } 
  
  Core.first_arg_type->addMethod(list::domain(2,Kernel._type,Kernel._type),Kernel._type,
  	RETURN_ARG,_function_(first_arg_type_type,"first_arg_type_type"));
  
  Core.first_arg_type->addMethod(list::domain(3,Kernel._type,Kernel._type,Kernel._type),Kernel._type,
  	RETURN_ARG,_function_(first_arg_type_type2,"first_arg_type_type2"));
  
  Core.second_arg_type->addMethod(list::domain(2,Kernel._type,Kernel._type),Kernel._type,
  	RETURN_ARG,_function_(second_arg_type_type,"second_arg_type_type"));
  
  Core.meet_arg_types->addMethod(list::domain(2,Kernel._type,Kernel._type),Kernel._type,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(meet_arg_types_type,"meet_arg_types_type"));
  
  Core.first_member_type->addMethod(list::domain(2,Kernel._type,Kernel._type),Kernel._type,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(first_member_type_type,"first_member_type_type"));
  
  (Kernel.nth->addMethod(list::domain(2,Kernel._array,Kernel._integer),Kernel._any,
  	RETURN_ARG,_function_(nth_array,"nth_array"))->typing = _oid_(_function_(nth_array_type,"nth_array_type")));
  
  (Core.make_array->addMethod(list::domain(3,Kernel._integer,Kernel._type,Kernel._any),Kernel._array,
  	NEW_ALLOC,_function_(make_array_integer,"make_array_integer"))->typing = _oid_(_function_(make_array_integer_type,"make_array_integer_type")));
  
  (Kernel.make_list->addMethod(list::domain(3,Kernel._integer,Kernel._type,Kernel._any),Kernel._list,
  	NEW_ALLOC,_function_(make_list_integer2,"make_list_integer2"))->typing = _oid_(_function_(make_list_integer2_type,"make_list_integer2_type")));
  
  (Core.make_set->addMethod(list::domain(1,nth_class2(Kernel._array,list::alloc(Kernel._any,1,_oid_(Kernel.of)),list::alloc(Kernel.emptySet,1,_oid_(Kernel._any)))),Kernel._set,
  	NEW_ALLOC+RETURN_ARG,_function_(make_set_array,"make_set_array"))->typing = _oid_(_function_(make_set_array_type,"make_set_array_type")));
  
  (Kernel.list_I->addMethod(list::domain(1,nth_class2(Kernel._array,list::alloc(Kernel._any,1,_oid_(Kernel.of)),list::alloc(Kernel.emptySet,1,_oid_(Kernel._any)))),Kernel._list,
  	NEW_ALLOC,_function_(list_I_array,"list_I_array"))->typing = _oid_(_function_(list_I_array_type,"list_I_array_type")));
  
  (Kernel.array_I->addMethod(list::domain(1,nth_class2(Kernel._list,list::alloc(Kernel._any,1,_oid_(Kernel.of)),list::alloc(Kernel.emptySet,1,_oid_(Kernel._any)))),Kernel._array,
  	NEW_ALLOC,_function_(array_I_list,"array_I_list"))->typing = _oid_(_function_(array_I_list_type,"array_I_list_type")));
  
  (Kernel.set_I->addMethod(list::domain(1,nth_class2(Kernel._list,list::alloc(Kernel._any,1,_oid_(Kernel.of)),list::alloc(Kernel.emptySet,1,_oid_(Kernel._any)))),Kernel._set,
  	NEW_ALLOC+SAFE_RESULT,_function_(set_I_bag,"set_I_bag"))->typing = _oid_(_function_(set_I_bag_type,"set_I_bag_type")));
  
  (Kernel.list_I->addMethod(list::domain(1,nth_class2(Kernel._set,list::alloc(Kernel._any,1,_oid_(Kernel.of)),list::alloc(Kernel.emptySet,1,_oid_(Kernel._any)))),Kernel._list,
  	NEW_ALLOC+SAFE_RESULT,_function_(list_I_set,"list_I_set"))->typing = _oid_(_function_(list_I_set_type,"list_I_set_type")));
  
  { { ITERATE(r);
      for (START(Kernel.copy->restrictions); NEXT(r);)
      (OBJECT(method,r)->typing = _oid_(Core.Id));
      } 
    { ITERATE(r);
      for (START(Kernel.empty->restrictions); NEXT(r);)
      (OBJECT(method,r)->typing = _oid_(Core.Id));
      } 
    { ITERATE(r);
      for (START(Core.sort->restrictions); NEXT(r);)
      (OBJECT(method,r)->typing = _oid_(Core.second_arg_type));
      } 
    { ITERATE(r);
      for (START(Kernel._7_plus->restrictions); NEXT(r);)
      (OBJECT(method,r)->typing = _oid_(Core.meet_arg_types));
      } 
    (CLREAD(method,_at_property1(Kernel.nth_get,Kernel._array),typing) = _oid_(Core.first_member_type));
    { ITERATE(r);
      for (START(Kernel.nth_plus->restrictions); NEXT(r);)
      (OBJECT(method,r)->typing = _oid_(Core.first_arg_type));
      } 
    { ITERATE(r);
      for (START(Kernel.add->restrictions); NEXT(r);)
      if (OBJECT(restriction,r)->domain->length == 2)
       (OBJECT(method,r)->typing = _oid_(Core.first_arg_type));
      } 
    { ITERATE(r);
      for (START(Kernel._delete->restrictions); NEXT(r);)
      if (OBJECT(restriction,r)->domain->length == 2)
       (OBJECT(method,r)->typing = _oid_(Core.first_arg_type));
      } 
    } 
  
  GC_UNBIND;} 

