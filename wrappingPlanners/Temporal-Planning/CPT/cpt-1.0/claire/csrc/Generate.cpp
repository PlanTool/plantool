/***** CLAIRE Compilation of file Generate.cl 
         [version 3.3.4 / safety 5] Sat Oct 16 06:53:39 2004 *****/

#include <claire.h>
#include <Kernel.h>
#include <Core.h>
#include <Language.h>
#include <Reader.h>
#include <Optimize.h>
#include <Generate.h>


GenerateClass Generate;

// definition of the meta-model for Generate 

void GenerateClass::metaLoad() { 
  GC_BIND;
  ClEnv->module_I = it;
// definition of the properties 
  Generate.open_comparators = property::make("open_comparators",Generate.it);
  Generate.open_operators = property::make("open_operators",Generate.it);
  Generate.extension = property::make("extension",Generate.it);
  Generate.interfaces = property::make("interfaces",Generate.it);
  Generate.interface_I = property::make("interface!",Generate.it);
  Generate.indent_c = property::make("indent_c",Generate.it);
  Generate.breakline = property::make("breakline",Generate.it);
  Generate.new_block = property::make("new_block",Generate.it);
  Generate.close_block = property::make("close_block",Generate.it);
  Generate.c_test = property::make("c_test",claire.it);
  Generate.c_func = property::make("c_func",Compile.it);
  Generate.expression = property::make("expression",Generate.it);
  Generate.statement = property::make("statement",Generate.it);
  Generate.compile = property::make("compile",claire.it);
  Generate.parents = property::make("parents",Generate.it);
  Generate.outmodule = property::make("outmodule",2,Generate.it);
  Generate.generate_files = property::make("generate_files",Generate.it);
  Generate.generate_classes = property::make("generate_classes",Generate.it);
  Generate.generate_c2f = property::make("generate_c2f",Generate.it);
  Generate.generate_f2f = property::make("generate_f2f",Generate.it);
  Generate.generate_interface = property::make("generate_interface",Generate.it);
  Generate.start_module_interface = property::make("start_module_interface",Generate.it);
  Generate.generate_file = property::make("generate_file",Generate.it);
  Generate.start_file = property::make("start_file",Generate.it);
  Generate.generate_meta_load = property::make("generate_meta_load",Generate.it);
  Generate.generate_start_file = property::make("generate_start_file",Generate.it);
  Generate.generate_functions = property::make("generate_functions",Generate.it);
  Generate.generate_objects = property::make("generate_objects",Generate.it);
  Generate.generate_end_file = property::make("generate_end_file",Generate.it);
  Generate.typed_args_list = property::make("typed_args_list",Generate.it);
  Generate.namespace_I = property::make("namespace!",Generate.it);
  Generate.public_static = property::make("public_static",Generate.it);
  Generate.declare = property::make("declare",Generate.it);
  Generate.print_c_function = property::make("print_c_function",Generate.it);
  Generate.create_function_entry = property::make("create_function_entry",Generate.it);
  Generate.check_sort = property::make("check_sort",Generate.it);
  Generate.protect_result = property::make("protect_result",Generate.it);
  Generate.need_debug_ask = property::make("need_debug?",Generate.it);
  Generate.set_outfile = property::make("set_outfile",4,Generate.it);
  Generate.generate_profile = property::make("generate_profile",Generate.it);
  Generate.debug_intro = property::make("debug_intro",Generate.it);
  Generate.inner_statement = property::make("inner_statement",Generate.it);
  Generate.update_function_entry = property::make("update_function_entry",Generate.it);
  Generate.get_dependents = property::make("get_dependents",Generate.it);
  Generate.produce = property::make("produce",Generate.it);
  Generate.at = property::make("at",Generate.it);
  Generate.bag_expression = property::make("bag_expression",Generate.it);
  Generate.inline_exp = property::make("inline_exp",4,Generate.it);
  Generate.gc_protection_exp = property::make("gc_protection_exp",Generate.it);
  Generate.exp_to_protect = property::make("exp_to_protect",Generate.it);
  Generate.gc_protect = property::make("gc_protect",Generate.it);
  Generate.gassign = property::make("gassign",Generate.it);
  Generate.to_cl = property::make("to_cl",Generate.it);
  Generate.to_c = property::make("to_c",Generate.it);
  Generate.call_slot = property::make("call_slot",Generate.it);
  Generate.call_table = property::make("call_table",Generate.it);
  Generate.call_array = property::make("call_array",Generate.it);
  Generate.update = property::make("update",Generate.it);
  Generate.sign_equal = property::make("sign_equal",Generate.it);
  Generate.sign_or = property::make("sign_or",Generate.it);
  Generate.macro = property::make("macro",Generate.it);
  Generate.equal_exp = property::make("equal_exp",Generate.it);
  Generate.object_test = property::make("object_test",Generate.it);
  Generate.bitvector_exp = property::make("bitvector_exp",Generate.it);
  Generate.inherit_exp = property::make("inherit_exp",Generate.it);
  Generate.args_list = property::make("args_list",Generate.it);
  Generate.check_var = property::make("check_var",Generate.it);
  Generate.build_Variable = property::make("build_Variable",Generate.it);
  Generate.unfold_args = property::make("unfold_args",Generate.it);
  Generate.c_type_sort = property::make("c_type_sort",Generate.it);
  Generate.unfold_arg = property::make("unfold_arg",Generate.it);
  Generate.unfold_use = property::make("unfold_use",Generate.it);
  Generate.self_statement = property::make("self_statement",Generate.it);
  Generate.stat_construct = property::make("stat_construct",Generate.it);
  Generate.init_var = property::make("init_var",Generate.it);
  Generate.stat_while = property::make("stat_while",Generate.it);
  Generate.stat_gassign = property::make("stat_gassign",Generate.it);
  Generate.stat_for = property::make("stat_for",Generate.it);
  Generate.stat_iteration = property::make("stat_iteration",Generate.it);
  Generate.stat_super = property::make("stat_super",Generate.it);
  Generate.stat_handle = property::make("stat_handle",Generate.it);
  Generate.any_interface = property::make("any_interface",Generate.it);
  Generate.pointer_cast = property::make("pointer_cast",Generate.it);
  Generate.bad_names = property::make("bad_names",Generate.it);
  Generate.good_names = property::make("good_names",Generate.it);
  Generate.generate_float_function = property::make("generate_float_function",Generate.it);
  Generate.generate_regular_function = property::make("generate_regular_function",Generate.it);
  Generate.bitvectorSum = property::make("bitvectorSum",Generate.it);
  Generate.c_member = property::make("c_member",Generate.it);
  Generate.addFast = property::make("addFast",Generate.it);
  Generate.print_external_call = property::make("print_external_call",Generate.it);
  Generate.getC = property::make("getC",Generate.it);
  Generate.signature_I = property::make("signature!",Generate.it);
  Generate.bitvector_I = property::make("bitvector!",Generate.it);
  Generate.get_restrictions = property::make("get_restrictions",Generate.it);
  Generate.c_sorted_args = property::make("c_sorted_args",Generate.it);
  Generate.c_sorted_arg = property::make("c_sorted_arg",Generate.it);
  Generate.generate_function_start = property::make("generate_function_start",Generate.it);
  Generate.print_body = property::make("print_body",Generate.it);
  Generate.c_safe = property::make("c_safe",Generate.it);
  Generate.outer_statement = property::make("outer_statement",Generate.it);
  Generate.methods_interface = property::make("methods_interface",Generate.it);
  Generate.methods_bodies = property::make("methods_bodies",Generate.it);
  Generate.gc_introduction = property::make("gc_introduction",Generate.it);
  Generate.gc_usage = property::make("gc_usage",Generate.it);
  Generate.gc_usage_star = property::make("gc_usage*",Generate.it);
  Generate.gc_or = property::make("gc_or",Generate.it);
  Generate.stat_exp = property::make("stat_exp",Generate.it);
  Generate.define_variable = property::make("define_variable",Generate.it);
  Generate.char_exp_ask = property::make("char_exp?",Generate.it);
  Generate.char_exp = property::make("char_exp",Generate.it);
  Generate.bool_exp_ask = property::make("bool_exp?",Generate.it);
  Generate.bool_exp_I = property::make("bool_exp!",Generate.it);
  Generate.bexpression = property::make("bexpression",Generate.it);
  Generate.end_module_interface = property::make("end_module_interface",Generate.it);
  Generate.stat_let = property::make("stat_let",Generate.it);
  Generate.exp_Assign = property::make("exp_Assign",Generate.it);
  Generate.generate_tuple_function = property::make("generate_tuple_function",Generate.it);
  Generate.generate_s_file = property::make("generate_s_file",Generate.it);
  Generate.create_load_modules = property::make("create_load_modules",Generate.it);
  Generate.global_var_def_ask = property::make("global_var_def?",Generate.it);
  Generate.global_var_def_I = property::make("global_var_def!",Generate.it);
  Generate.getRange = property::make("getRange",Generate.it);
  Generate.globalVar = property::make("globalVar",Generate.it);
  Generate.c_string = property::make("c_string",Generate.it);
  Generate.any_bool_exp = property::make("any_bool_exp",Generate.it);
  Generate.class_princ = property::make("class_princ",Generate.it);
  
  // instructions from module sources
  { global_variable * v2072 = (Generate._star_ask_interval_star = (global_variable *) Core._global_variable->instantiate("*?_interval*",Compile.it));
    (v2072->range = Kernel.emptySet);
    (v2072->value = _oid_(_at_property1(Kernel.set_I,Core._Interval)));
    close_global_variable(v2072);
    } 
  
  { global_variable * v2072 = (Generate._star_dash_dash_integer_star = (global_variable *) Core._global_variable->instantiate("*--_integer*",Compile.it));
    (v2072->range = Kernel.emptySet);
    (v2072->value = _oid_(_at_property1(Kernel._dot_dot,Kernel._integer)));
    close_global_variable(v2072);
    } 
  
  { global_variable * v2072 = (Generate._star_plus_integer_star = (global_variable *) Core._global_variable->instantiate("*+_integer*",Compile.it));
    (v2072->range = Kernel.emptySet);
    (v2072->value = _oid_(_at_property1(Core._plus,Kernel._integer)));
    close_global_variable(v2072);
    } 
  
  { global_variable * v2072 = (Generate._starnth_integer_star = (global_variable *) Core._global_variable->instantiate("*nth_integer*",Compile.it));
    (v2072->range = Kernel.emptySet);
    (v2072->value = _oid_(_at_property1(Kernel.nth,Kernel._integer)));
    close_global_variable(v2072);
    } 
  
  { global_variable * v2072 = (Generate._starnth_list_star = (global_variable *) Core._global_variable->instantiate("*nth_list*",Compile.it));
    (v2072->range = Kernel.emptySet);
    (v2072->value = _oid_(_at_property1(Kernel.nth,Kernel._list)));
    close_global_variable(v2072);
    } 
  
  { global_variable * v2072 = (Generate._starnth_1_bag_star = (global_variable *) Core._global_variable->instantiate("*nth_1_bag*",Compile.it));
    (v2072->range = Kernel.emptySet);
    (v2072->value = _oid_(_at_property1(Kernel.nth_get,Kernel._bag)));
    close_global_variable(v2072);
    } 
  
  { global_variable * v2072 = (Generate._starnth_string_star = (global_variable *) Core._global_variable->instantiate("*nth_string*",Compile.it));
    (v2072->range = Kernel.emptySet);
    (v2072->value = _oid_(_at_property1(Kernel.nth,Kernel._string)));
    close_global_variable(v2072);
    } 
  
  { global_variable * v2072 = (Generate._starnth_1_string_star = (global_variable *) Core._global_variable->instantiate("*nth_1_string*",Compile.it));
    (v2072->range = Kernel.emptySet);
    (v2072->value = _oid_(_at_property1(Kernel.nth_get,Kernel._string)));
    close_global_variable(v2072);
    } 
  
  { global_variable * v2072 = (Generate._starnth_equal_list_star = (global_variable *) Core._global_variable->instantiate("*nth=_list*",Compile.it));
    (v2072->range = Kernel.emptySet);
    (v2072->value = _oid_(_at_property1(Kernel.nth_equal,Kernel._list)));
    close_global_variable(v2072);
    } 
  
  { global_variable * v2072 = (Generate._starnot_star = (global_variable *) Core._global_variable->instantiate("*not*",Compile.it));
    (v2072->range = Kernel.emptySet);
    (v2072->value = _oid_(_at_property1(Core.NOT,Kernel._any)));
    close_global_variable(v2072);
    } 
  
  { global_variable * v2072 = (Generate._starknown_star = (global_variable *) Core._global_variable->instantiate("*known*",Compile.it));
    (v2072->range = Kernel.emptySet);
    (v2072->value = _oid_(_at_property1(Core.known_ask,Kernel._any)));
    close_global_variable(v2072);
    } 
  
  { global_variable * v2072 = (Generate._starunknown_star = (global_variable *) Core._global_variable->instantiate("*unknown*",Compile.it));
    (v2072->range = Kernel.emptySet);
    (v2072->value = _oid_(_at_property1(Core.unknown_ask,Kernel._any)));
    close_global_variable(v2072);
    } 
  
  { global_variable * v2072 = (Generate._starnot_equal_star = (global_variable *) Core._global_variable->instantiate("*not_equal*",Compile.it));
    (v2072->range = Kernel.emptySet);
    (v2072->value = _oid_(_at_property1(Core._I_equal,Kernel._any)));
    close_global_variable(v2072);
    } 
  
  { global_variable * v2072 = (Generate._starcontain_star = (global_variable *) Core._global_variable->instantiate("*contain*",Compile.it));
    (v2072->range = Kernel.emptySet);
    (v2072->value = _oid_(_at_property1(Kernel.contain_ask,Kernel._list)));
    close_global_variable(v2072);
    } 
  
  { global_variable * v2072 = (Generate._starmin_integer_star = (global_variable *) Core._global_variable->instantiate("*min_integer*",Compile.it));
    (v2072->range = Kernel.emptySet);
    (v2072->value = _oid_(_at_property1(Core.min,Kernel._integer)));
    close_global_variable(v2072);
    } 
  
  { global_variable * v2072 = (Generate._starmax_integer_star = (global_variable *) Core._global_variable->instantiate("*max_integer*",Compile.it));
    (v2072->range = Kernel.emptySet);
    (v2072->value = _oid_(_at_property1(Core.max,Kernel._integer)));
    close_global_variable(v2072);
    } 
  
  { global_variable * v2072 = (Generate._starlength_array_star = (global_variable *) Core._global_variable->instantiate("*length_array*",Compile.it));
    (v2072->range = Kernel.emptySet);
    (v2072->value = _oid_(_at_property1(Kernel.length,Kernel._array)));
    close_global_variable(v2072);
    } 
  
  { global_variable * v2072 = (Generate._starlength_bag_star = (global_variable *) Core._global_variable->instantiate("*length_bag*",Compile.it));
    (v2072->range = Kernel.emptySet);
    (v2072->value = _oid_(_at_property1(Kernel.length,Kernel._bag)));
    close_global_variable(v2072);
    } 
  
  { global_variable * v2072 = (Generate._starclose_exception_star = (global_variable *) Core._global_variable->instantiate("*close_exception*",Compile.it));
    (v2072->range = Kernel.emptySet);
    (v2072->value = _oid_(_at_property1(Kernel.close,Kernel._exception)));
    close_global_variable(v2072);
    } 
  
  { (Generate._producer = ClaireClass::make("producer",Kernel._thing,Generate.it));
    CL_ADD_SLOT(Generate._producer,Generate_producer,Generate.open_comparators,open_comparators,nth_class1(Kernel._list,Kernel._operation),CNULL);
    CL_ADD_SLOT(Generate._producer,Generate_producer,Generate.open_operators,open_operators,nth_class1(Kernel._list,Kernel._operation),CNULL);
    CL_ADD_SLOT(Generate._producer,Generate_producer,Kernel.body,body,Kernel._any,0);
    CL_ADD_SLOT(Generate._producer,Generate_producer,Generate.extension,extension,Kernel._string,CNULL);
    CL_ADD_SLOT(Generate._producer,Generate_producer,Kernel.comment,comment,Kernel._string,CNULL);
    CL_ADD_SLOT(Generate._producer,Generate_producer,Generate.interfaces,interfaces,Kernel._list,CNULL);
    CL_ADD_SLOT(Generate._producer,Generate_producer,Kernel.stat,stat,Kernel._integer,0);
    } 
  
  { global_variable * v2072 = (Generate.PRODUCER = (global_variable *) Core._global_variable->instantiate("PRODUCER",claire.it));
    (v2072->range = Generate._producer);
    (v2072->value = CNULL);
    close_global_variable(v2072);
    } 
  
  Language.ident->addMethod(list::domain(1,Kernel._symbol),Kernel._void,
  	NEW_ALLOC+RETURN_ARG,_function_(ident_symbol,"ident_symbol"));
  
  Language.ident->addMethod(list::domain(1,Kernel._thing),Kernel._void,
  	NEW_ALLOC+RETURN_ARG,_function_(ident_thing,"ident_thing"));
  
  Language.ident->addMethod(list::domain(1,Kernel._class),Kernel._void,
  	NEW_ALLOC+RETURN_ARG,_function_(ident_class,"ident_class"));
  
  Generate.interface_I->addMethod(list::domain(1,Kernel._class),Kernel._void,
  	NEW_ALLOC,_function_(interface_I_class,"interface!_class"));
  
  Generate.class_princ->addMethod(list::domain(1,Kernel._class),Kernel._void,
  	NEW_ALLOC+RETURN_ARG,_function_(class_princ_class,"class_princ_class"));
  
  Generate.indent_c->addMethod(list::domain(1,Kernel._void),Kernel._any,
  	0,_function_(indent_c_void,"indent_c_void"));
  
  Generate.breakline->addMethod(list::domain(1,Kernel._void),Kernel._any,
  	0,_function_(breakline_void,"breakline_void"));
  
  Generate.new_block->addMethod(list::domain(1,Kernel._void),Kernel._void,
  	SLOT_UPDATE,_function_(new_block_void,"new_block_void"));
  
  Generate.close_block->addMethod(list::domain(1,Kernel._void),Kernel._void,
  	SLOT_UPDATE,_function_(close_block_void,"close_block_void"));
  
  (Generate._producer->open = 3);
  
  Generate.c_test->addMethod(list::domain(1,Kernel._any),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(c_test_any,"c_test_any"));
  
  Generate.c_test->addMethod(list::domain(1,Kernel._method),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(c_test_method,"c_test_method"));
  
  Generate.compile->addMethod(list::domain(1,Kernel._module),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(compile_module,"compile_module"));
  
  Generate.generate_files->addMethod(list::domain(1,Kernel._module),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(generate_files_module,"generate_files_module"));
  
  Generate.generate_f2f->addMethod(list::domain(1,Kernel._module),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(generate_f2f_module,"generate_f2f_module"));
  
  Generate.generate_file->addMethod(list::domain(2,Kernel._string,Kernel._module),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(generate_file_string1,"generate_file_string1"));
  
  { (Generate.classFile = (table *) Kernel._table->instantiate("classFile",Generate.it));
    (Generate.classFile->range = Kernel._port);
    (Generate.classFile->params = _oid_(Kernel._any));
    (Generate.classFile->domain = Kernel._class);
    (Generate.classFile->graph = make_list_integer(29,CNULL));
    (Generate.classFile->DEFAULT = CNULL);
    } 
  
  Generate.generate_classes->addMethod(list::domain(1,Kernel._module),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(generate_classes_module,"generate_classes_module"));
  
  Generate.generate_c2f->addMethod(list::domain(1,Kernel._module),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(generate_c2f_module,"generate_c2f_module"));
  
  Generate.generate_interface->addMethod(list::domain(2,Kernel._module,Kernel._bag),Kernel._void,
  	NEW_ALLOC+SLOT_UPDATE,_function_(generate_interface_module,"generate_interface_module"));
  
  Generate.generate_objects->addMethod(list::domain(1,Kernel._module),Kernel._void,
  	NEW_ALLOC+SLOT_UPDATE,_function_(generate_objects_module,"generate_objects_module"));
  
  Generate.generate_meta_load->addMethod(list::domain(1,Kernel._module),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(generate_meta_load_module,"generate_meta_load_module"));
  
  Generate.global_var_def_ask->addMethod(list::domain(1,Kernel._any),Kernel._boolean,
  	NEW_ALLOC,_function_(global_var_def_ask_any,"global_var_def?_any"));
  
  Generate.getRange->addMethod(list::domain(1,Core._global_variable),Kernel._class,
  	NEW_ALLOC+RETURN_ARG,_function_(getRange_global_variable,"getRange_global_variable"));
  
  Generate.generate_functions->addMethod(list::domain(1,Kernel._module),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(generate_functions_module,"generate_functions_module"));
  
  Generate.parents->addMethod(list::domain(2,Kernel._module,Kernel._list),Kernel._list,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(parents_module,"parents_module"));
  
  Generate.parents->addMethod(list::domain(1,Kernel._list),Kernel._list,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(parents_list,"parents_list"));
  
  Kernel.get->addMethod(list::domain(1,Kernel._module),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE,_function_(get_module2,"get_module2"));
  
  Generate.generate_file->addMethod(list::domain(2,Kernel._string,Kernel._string),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(generate_file_string2,"generate_file_string2"));
  
  Optimize.make_c_function->addMethod(list::domain(3,Core._lambda,Kernel._string,Kernel._any),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(make_c_function_lambda_Generate,"make_c_function_lambda_Generate"));
  
  Generate.print_c_function->addMethod(list::domain(3,Core._lambda,Kernel._string,Kernel._any),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(print_c_function_lambda2,"print_c_function_lambda2"));
  
  Generate.print_body->addMethod(list::domain(5,Language._If,
    Kernel._string,
    Kernel._class,
    Language._If,
    Kernel._boolean),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(print_body_If,"print_body_If"));
  
  Generate.print_body->addMethod(list::domain(5,Kernel._any,
    Kernel._string,
    Kernel._class,
    Kernel._any,
    Kernel._boolean),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(print_body_any,"print_body_any"));
  
  Generate.print_body->addMethod(list::domain(5,Language._Do,
    Kernel._string,
    Kernel._class,
    Kernel._any,
    Kernel._boolean),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(print_body_Do,"print_body_Do"));
  
  Generate.outer_statement->addMethod(list::domain(4,Kernel._any,
    Kernel._string,
    Kernel._class,
    Kernel._boolean),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(outer_statement_any,"outer_statement_any"));
  
  Generate.c_safe->addMethod(list::domain(1,Kernel._any),Kernel._boolean,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(c_safe_any,"c_safe_any"));
  
  { global_variable * v2072 = (Generate.WrongMethod = (global_variable *) Core._global_variable->instantiate("WrongMethod",claire.it));
    (v2072->range = Kernel._any);
    (v2072->value = 1);
    close_global_variable(v2072);
    } 
  
  Generate.check_sort->addMethod(list::domain(1,Kernel._method),Kernel._class,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(check_sort_method,"check_sort_method"));
  
  Generate.typed_args_list->addMethod(list::domain(1,Kernel._list),Kernel._any,
  	NEW_ALLOC+RETURN_ARG,_function_(typed_args_list_list,"typed_args_list_list"));
  
  Generate.need_debug_ask->addMethod(list::domain(1,Kernel._any),Kernel._boolean,
  	NEW_ALLOC,_function_(need_debug_ask_any,"need_debug?_any"));
  
  Generate.get_dependents->addMethod(list::domain(1,Kernel._method),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(get_dependents_method,"get_dependents_method"));
  
  Kernel.c_princ->addMethod(list::domain(1,Kernel._function),Kernel._void,
  	0,_function_(c_princ_function,"c_princ_function"));
  
  Generate.set_outfile->addMethod(list::domain(1,Core._lambda),Kernel._void,
  	NEW_ALLOC+SLOT_UPDATE,_function_(set_outfile_lambda,"set_outfile_lambda"));
  
  { (Generate.set_outfile->open = 4);
    (Generate.inline_exp->open = 4);
    } 
  
  Generate.c_func->addMethod(list::domain(1,Kernel._any),Kernel._boolean,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(c_func_any,"c_func_any"));
  
  Generate.expression->addMethod(list::domain(2,Kernel._thing,Kernel._any),Kernel._void,
  	NEW_ALLOC,_function_(expression_thing,"expression_thing"));
  
  Generate.expression->addMethod(list::domain(2,Kernel._integer,Kernel._any),Kernel._void,
  	0,_function_(expression_integer,"expression_integer"));
  
  Generate.expression->addMethod(list::domain(2,Kernel._any,Kernel._any),Kernel._void,
  	NEW_ALLOC,_function_(expression_any,"expression_any"));
  
  Generate.expression->addMethod(list::domain(2,Kernel._string,Kernel._any),Kernel._void,
  	NEW_ALLOC,_function_(expression_string,"expression_string"));
  
  Generate.expression->addFloatMethod(list::domain(2,Kernel._float,Kernel._any),Kernel._void,
  	0,_function_(expression_float,"expression_float"),_function_(expression_float_,"expression_float_"));
  
  Generate.expression->addMethod(list::domain(2,Kernel._boolean,Kernel._any),Kernel._void,
  	NEW_ALLOC,_function_(expression_boolean,"expression_boolean"));
  
  Generate.expression->addMethod(list::domain(2,Kernel._environment,Kernel._any),Kernel._void,
  	NEW_ALLOC,_function_(expression_environment,"expression_environment"));
  
  Generate.expression->addMethod(list::domain(2,Language._Variable,Kernel._any),Kernel._void,
  	NEW_ALLOC+RETURN_ARG,_function_(expression_Variable,"expression_Variable"));
  
  Generate.expression->addMethod(list::domain(2,Core._global_variable,Kernel._any),Kernel._void,
  	NEW_ALLOC,_function_(expression_global_variable,"expression_global_variable"));
  
  Generate.expression->addMethod(list::domain(2,Language._Set,Kernel._any),Kernel._void,
  	NEW_ALLOC,_function_(expression_Set,"expression_Set"));
  
  Generate.expression->addMethod(list::domain(2,Kernel._set,Kernel._any),Kernel._void,
  	NEW_ALLOC,_function_(expression_set2,"expression_set2"));
  
  Generate.expression->addMethod(list::domain(2,Language._Tuple,Kernel._any),Kernel._void,
  	NEW_ALLOC,_function_(expression_Tuple,"expression_Tuple"));
  
  Generate.expression->addMethod(list::domain(2,Kernel._tuple,Kernel._any),Kernel._void,
  	NEW_ALLOC,_function_(expression_tuple,"expression_tuple"));
  
  Generate.expression->addMethod(list::domain(2,Language._List,Kernel._any),Kernel._void,
  	NEW_ALLOC,_function_(expression_List,"expression_List"));
  
  Generate.expression->addMethod(list::domain(2,Kernel._list,Kernel._any),Kernel._void,
  	NEW_ALLOC,_function_(expression_list,"expression_list"));
  
  Generate.expression->addMethod(list::domain(2,Language._Call,Kernel._any),Kernel._void,
  	NEW_ALLOC+RETURN_ARG,_function_(expression_Call2,"expression_Call2"));
  
  Generate.expression->addMethod(list::domain(2,Language._Call_method1,Kernel._any),Kernel._void,
  	NEW_ALLOC+RETURN_ARG,_function_(expression_Call_method12,"expression_Call_method12"));
  
  Generate.expression->addMethod(list::domain(2,Language._Call_method2,Kernel._any),Kernel._void,
  	NEW_ALLOC+RETURN_ARG,_function_(expression_Call_method22,"expression_Call_method22"));
  
  Generate.expression->addMethod(list::domain(2,Language._Call_method,Kernel._any),Kernel._void,
  	NEW_ALLOC+RETURN_ARG,_function_(expression_Call_method2,"expression_Call_method2"));
  
  Generate.bexpression->addMethod(list::domain(2,Kernel._any,Kernel._any),Kernel._void,
  	NEW_ALLOC+RETURN_ARG,_function_(bexpression_any,"bexpression_any"));
  
  Generate.expression->addMethod(list::domain(2,Language._If,Kernel._any),Kernel._void,
  	NEW_ALLOC+SLOT_UPDATE+RETURN_ARG,_function_(expression_If,"expression_If"));
  
  Generate.expression->addMethod(list::domain(2,Language._Assign,Kernel._any),Kernel._void,
  	NEW_ALLOC,_function_(expression_Assign,"expression_Assign"));
  
  Generate.expression->addMethod(list::domain(2,Optimize._to_protect,Kernel._any),Kernel._void,
  	NEW_ALLOC+RETURN_ARG,_function_(expression_to_protect,"expression_to_protect"));
  
  Generate.gc_protect->addMethod(list::domain(1,Kernel._class),Kernel._string,
  	0,_function_(gc_protect_class,"gc_protect_class"));
  
  Generate.expression->addMethod(list::domain(2,Language._Gassign,Kernel._any),Kernel._void,
  	NEW_ALLOC,_function_(expression_Gassign,"expression_Gassign"));
  
  Generate.expression->addMethod(list::domain(2,Language._And,Kernel._any),Kernel._void,
  	NEW_ALLOC,_function_(expression_And,"expression_And"));
  
  Generate.expression->addMethod(list::domain(2,Language._Or,Kernel._any),Kernel._void,
  	NEW_ALLOC,_function_(expression_Or,"expression_Or"));
  
  Generate.expression->addMethod(list::domain(2,Optimize._to_CL,Kernel._any),Kernel._void,
  	NEW_ALLOC+RETURN_ARG,_function_(expression_to_CL,"expression_to_CL"));
  
  Generate.expression->addMethod(list::domain(2,Optimize._to_C,Kernel._any),Kernel._void,
  	NEW_ALLOC+RETURN_ARG,_function_(expression_to_C,"expression_to_C"));
  
  Generate.expression->addMethod(list::domain(2,Optimize._C_cast,Kernel._any),Kernel._void,
  	NEW_ALLOC,_function_(expression_C_cast,"expression_C_cast"));
  
  Generate.expression->addMethod(list::domain(2,Language._Call_slot,Kernel._any),Kernel._void,
  	NEW_ALLOC,_function_(expression_Call_slot,"expression_Call_slot"));
  
  Generate.expression->addMethod(list::domain(2,Language._Call_table,Kernel._any),Kernel._void,
  	NEW_ALLOC,_function_(expression_Call_table,"expression_Call_table"));
  
  Generate.expression->addMethod(list::domain(2,Language._Call_array,Kernel._any),Kernel._void,
  	NEW_ALLOC,_function_(expression_Call_array,"expression_Call_array"));
  
  Generate.expression->addMethod(list::domain(2,Language._Update,Kernel._any),Kernel._void,
  	NEW_ALLOC,_function_(expression_Update,"expression_Update"));
  
  Generate.sign_equal->addMethod(list::domain(1,Kernel._boolean),Kernel._void,
  	0,_function_(sign_equal_boolean,"sign_equal_boolean"));
  
  Generate.sign_or->addMethod(list::domain(1,Kernel._boolean),Kernel._void,
  	0,_function_(sign_or_boolean,"sign_or_boolean"));
  
  Optimize.bool_exp->addMethod(list::domain(3,Kernel._any,Kernel._boolean,Kernel._any),Kernel._void,
  	NEW_ALLOC,_function_(bool_exp_any_Generate,"bool_exp_any_Generate"));
  
  Generate.any_bool_exp->addMethod(list::domain(4,Kernel._any,
    Kernel._boolean,
    Kernel._any,
    Kernel._boolean),Kernel._void,
  	NEW_ALLOC,_function_(any_bool_exp_any,"any_bool_exp_any"));
  
  Optimize.bool_exp->addMethod(list::domain(3,Optimize._to_CL,Kernel._boolean,Kernel._any),Kernel._void,
  	NEW_ALLOC+RETURN_ARG,_function_(bool_exp_to_CL_Generate,"bool_exp_to_CL_Generate"));
  
  Optimize.bool_exp->addMethod(list::domain(3,Language._If,Kernel._boolean,Kernel._any),Kernel._void,
  	NEW_ALLOC,_function_(bool_exp_If_Generate,"bool_exp_If_Generate"));
  
  Optimize.bool_exp->addMethod(list::domain(3,Language._And,Kernel._boolean,Kernel._any),Kernel._void,
  	NEW_ALLOC+SLOT_UPDATE+RETURN_ARG,_function_(bool_exp_And_Generate,"bool_exp_And_Generate"));
  
  Optimize.bool_exp->addMethod(list::domain(3,Language._Or,Kernel._boolean,Kernel._any),Kernel._void,
  	NEW_ALLOC+SLOT_UPDATE+RETURN_ARG,_function_(bool_exp_Or_Generate,"bool_exp_Or_Generate"));
  
  Optimize.bool_exp->addMethod(list::domain(3,Language._Call_method1,Kernel._boolean,Kernel._any),Kernel._void,
  	NEW_ALLOC+RETURN_ARG,_function_(bool_exp_Call_method1_Generate,"bool_exp_Call_method1_Generate"));
  
  Optimize.bool_exp->addMethod(list::domain(3,Language._Call_method2,Kernel._boolean,Kernel._any),Kernel._void,
  	NEW_ALLOC+SLOT_UPDATE+RETURN_ARG,_function_(bool_exp_Call_method2_Generate,"bool_exp_Call_method2_Generate"));
  
  Generate.bool_exp_ask->addMethod(list::domain(1,Kernel._any),Kernel._boolean,
  	NEW_ALLOC,_function_(bool_exp_ask_any,"bool_exp?_any"));
  
  Generate.args_list->addMethod(list::domain(3,Kernel._bag,Kernel._any,Kernel._boolean),Kernel._void,
  	NEW_ALLOC+SLOT_UPDATE+RETURN_ARG,_function_(args_list_bag,"args_list_bag"));
  
  Generate.check_var->addMethod(list::domain(3,Kernel._string,Kernel._any,Kernel._any),Kernel._string,
  	NEW_ALLOC+RETURN_ARG,_function_(check_var_string,"check_var_string"));
  
  Generate.build_Variable->addMethod(list::domain(2,Kernel._string,Kernel._any),Language._Variable,
  	NEW_ALLOC,_function_(build_Variable_string,"build_Variable_string"));
  
  Generate.unfold_args->addMethod(list::domain(1,Kernel._list),Kernel._list,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(unfold_args_list,"unfold_args_list"));
  
  Generate.c_type_sort->addMethod(list::domain(1,Kernel._any),Kernel._type,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(c_type_sort_any,"c_type_sort_any"));
  
  Generate.unfold_arg->addMethod(list::domain(3,Kernel._list,Kernel._list,Kernel._any),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(unfold_arg_list,"unfold_arg_list"));
  
  Generate.unfold_use->addMethod(list::domain(4,Kernel._list,
    Kernel._any,
    Kernel._any,
    Kernel._any),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(unfold_use_list,"unfold_use_list"));
  
  Generate.statement->addMethod(list::domain(3,Kernel._any,Kernel._any,Kernel._any),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(statement_any,"statement_any"));
  
  Generate.self_statement->addMethod(list::domain(3,Language._Construct,Kernel._any,Kernel._any),Kernel._void,
  	NEW_ALLOC,_function_(self_statement_Construct,"self_statement_Construct"));
  
  Generate.self_statement->addMethod(list::domain(3,Language._If,Kernel._any,Kernel._any),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(self_statement_If,"self_statement_If"));
  
  Generate.self_statement->addMethod(list::domain(3,Language._Do,Kernel._any,Kernel._any),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(self_statement_Do,"self_statement_Do"));
  
  Generate.inner_statement->addMethod(list::domain(3,Kernel._any,Kernel._any,Kernel._any),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(inner_statement_any,"inner_statement_any"));
  
  Generate.self_statement->addMethod(list::domain(3,Language._Let,Kernel._any,Kernel._any),Kernel._void,
  	NEW_ALLOC,_function_(self_statement_Let,"self_statement_Let"));
  
  Generate.self_statement->addMethod(list::domain(3,Language._And,Kernel._any,Kernel._any),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(self_statement_And,"self_statement_And"));
  
  Generate.self_statement->addMethod(list::domain(3,Language._Or,Kernel._any,Kernel._any),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(self_statement_Or,"self_statement_Or"));
  
  Generate.self_statement->addMethod(list::domain(3,Language._While,Kernel._any,Kernel._any),Kernel._void,
  	NEW_ALLOC,_function_(self_statement_While,"self_statement_While"));
  
  Generate.self_statement->addMethod(list::domain(3,Language._Assign,Kernel._any,Kernel._any),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(self_statement_Assign,"self_statement_Assign"));
  
  Generate.self_statement->addMethod(list::domain(3,Language._Gassign,Kernel._any,Kernel._any),Kernel._void,
  	NEW_ALLOC,_function_(self_statement_Gassign,"self_statement_Gassign"));
  
  Generate.self_statement->addMethod(list::domain(3,Optimize._to_protect,Kernel._any,Kernel._any),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(self_statement_to_protect,"self_statement_to_protect"));
  
  Generate.self_statement->addMethod(list::domain(3,Language._For,Kernel._any,Kernel._any),Kernel._void,
  	NEW_ALLOC,_function_(self_statement_For,"self_statement_For"));
  
  Generate.self_statement->addMethod(list::domain(3,Language._Iteration,Kernel._any,Kernel._any),Kernel._void,
  	NEW_ALLOC,_function_(self_statement_Iteration,"self_statement_Iteration"));
  
  Generate.self_statement->addMethod(list::domain(3,Language._Return,Kernel._any,Kernel._any),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(self_statement_Return,"self_statement_Return"));
  
  Generate.self_statement->addMethod(list::domain(3,Language._Call,Kernel._any,Kernel._any),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(self_statement_Call,"self_statement_Call"));
  
  Generate.self_statement->addMethod(list::domain(3,Language._Call_method,Kernel._any,Kernel._any),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(self_statement_Call_method,"self_statement_Call_method"));
  
  Generate.self_statement->addMethod(list::domain(3,Language._Call_method1,Kernel._any,Kernel._any),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(self_statement_Call_method1,"self_statement_Call_method1"));
  
  Generate.self_statement->addMethod(list::domain(3,Language._Call_method2,Kernel._any,Kernel._any),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(self_statement_Call_method2,"self_statement_Call_method2"));
  
  Generate.self_statement->addMethod(list::domain(3,Language._Super,Kernel._any,Kernel._any),Kernel._void,
  	NEW_ALLOC,_function_(self_statement_Super,"self_statement_Super"));
  
  Generate.self_statement->addMethod(list::domain(3,Language._Cast,Kernel._any,Kernel._any),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(self_statement_Cast,"self_statement_Cast"));
  
  Generate.self_statement->addMethod(list::domain(3,Language._Handle,Kernel._any,Kernel._any),Kernel._void,
  	NEW_ALLOC,_function_(self_statement_Handle,"self_statement_Handle"));
  
  Generate.self_statement->addMethod(list::domain(3,Optimize._to_CL,Kernel._any,Kernel._any),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(self_statement_to_CL,"self_statement_to_CL"));
  
  Generate.self_statement->addMethod(list::domain(3,Optimize._to_C,Kernel._any,Kernel._any),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(self_statement_to_C,"self_statement_to_C"));
  
  Generate.self_statement->addMethod(list::domain(3,Optimize._C_cast,Kernel._any,Kernel._any),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(self_statement_C_cast,"self_statement_C_cast"));
  
  Generate.self_statement->addMethod(list::domain(3,Language._Call_slot,Kernel._any,Kernel._any),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(self_statement_Call_slot,"self_statement_Call_slot"));
  
  Generate.self_statement->addMethod(list::domain(3,Language._Call_table,Kernel._any,Kernel._any),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(self_statement_Call_table,"self_statement_Call_table"));
  
  Generate.self_statement->addMethod(list::domain(3,Language._Call_array,Kernel._any,Kernel._any),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(self_statement_Call_array,"self_statement_Call_array"));
  
  Generate.self_statement->addMethod(list::domain(3,Language._Update,Kernel._any,Kernel._any),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(self_statement_Update,"self_statement_Update"));
  
  { (Generate._c_producer = ClaireClass::make("c_producer",Generate._producer,Generate.it));
    CL_ADD_SLOT(Generate._c_producer,Generate_c_producer,Generate.bad_names,bad_names,nth_class1(Kernel._list,Kernel._symbol),CNULL);
    CL_ADD_SLOT(Generate._c_producer,Generate_c_producer,Generate.good_names,good_names,nth_class1(Kernel._list,Kernel._symbol),CNULL);
    } 
  
  { (Generate.C_plus_plusPRODUCER = (Generate_c_producer *) Generate._c_producer->instantiate("C++PRODUCER",Generate.it));
    (Generate.C_plus_plusPRODUCER->open_comparators = list::alloc(4,_oid_(Kernel._inf),
      _oid_(Kernel._sup),
      _oid_(Kernel._sup_equal),
      _oid_(Kernel._inf_equal)));
    (Generate.C_plus_plusPRODUCER->open_operators = list::alloc(5,_oid_(Core._plus),
      _oid_(Kernel._dash),
      _oid_(Kernel._star),
      _oid_(Kernel._7),
      _oid_(Core._sup_sup)));
    (Generate.C_plus_plusPRODUCER->extension = ".cpp");
    (Generate.C_plus_plusPRODUCER->comment = "C++");
    { Generate_c_producer * v6880 = Generate.C_plus_plusPRODUCER; 
      list * v6881;
      { GC_ANY(v6881= list::empty(Kernel.emptySet));
        ((list *) v6881)->addFast(_oid_(symbol_I_string2("do")));
        ((list *) v6881)->addFast(_oid_(symbol_I_string2("if")));
        ((list *) v6881)->addFast(_oid_(symbol_I_string2("and")));
        ((list *) v6881)->addFast(_oid_(symbol_I_string2("or")));
        ((list *) v6881)->addFast(_oid_(symbol_I_string2("not")));
        ((list *) v6881)->addFast(_oid_(symbol_I_string2("printf")));
        ((list *) v6881)->addFast(_oid_(symbol_I_string2("void")));
        ((list *) v6881)->addFast(_oid_(Optimize._Pattern->name));
        ((list *) v6881)->addFast(_oid_(symbol_I_string2("exception")));
        ((list *) v6881)->addFast(_oid_(symbol_I_string2("return")));
        ((list *) v6881)->addFast(_oid_(symbol_I_string2("new")));
        ((list *) v6881)->addFast(_oid_(symbol_I_string2("class")));
        ((list *) v6881)->addFast(_oid_(symbol_I_string2("private")));
        ((list *) v6881)->addFast(_oid_(symbol_I_string2("operator")));
        ((list *) v6881)->addFast(_oid_(symbol_I_string2("default")));
        ((list *) v6881)->addFast(_oid_(symbol_I_string2("Handle")));
        ((list *) v6881)->addFast(_oid_(symbol_I_string2("import")));
        ((list *) v6881)->addFast(_oid_(symbol_I_string2("catch")));
        ((list *) v6881)->addFast(_oid_(symbol_I_string2("stdout")));
        ((list *) v6881)->addFast(_oid_(symbol_I_string2("stdin")));
        ((list *) v6881)->addFast(_oid_(symbol_I_string2("break")));
        ((list *) v6881)->addFast(_oid_(symbol_I_string2("char")));
        ((list *) v6881)->addFast(_oid_(symbol_I_string2("interface")));
        ((list *) v6881)->addFast(_oid_(symbol_I_string2("EOF")));
        ((list *) v6881)->addFast(_oid_(symbol_I_string2("relation")));
        ((list *) v6881)->addFast(_oid_(symbol_I_string2("System")));
        ((list *) v6881)->addFast(_oid_(symbol_I_string2("object")));
        ((list *) v6881)->addFast(_oid_(symbol_I_string2("delete")));
        ((list *) v6881)->addFast(_oid_(symbol_I_string2("boolean")));
        ((list *) v6881)->addFast(_oid_(symbol_I_string2("function")));
        ((list *) v6881)->addFast(_oid_(symbol_I_string2("type")));
        ((list *) v6881)->addFast(_oid_(symbol_I_string2("system_thing")));
        ((list *) v6881)->addFast(_oid_(symbol_I_string2("environment")));
        ((list *) v6881)->addFast(_oid_(symbol_I_string2("abstract")));
        ((list *) v6881)->addFast(_oid_(symbol_I_string2("final")));
        ((list *) v6881)->addFast(_oid_(symbol_I_string2("system_object")));
        ((list *) v6881)->addFast(_oid_(symbol_I_string2("NEW_ALLOC")));
        ((list *) v6881)->addFast(_oid_(symbol_I_string2("BAG_UPDATE")));
        ((list *) v6881)->addFast(_oid_(symbol_I_string2("SLOT_UPDATE")));
        ((list *) v6881)->addFast(_oid_(symbol_I_string2("RETURN_ARG")));
        ((list *) v6881)->addFast(_oid_(symbol_I_string2("SAFE_RESULT")));
        ((list *) v6881)->addFast(_oid_(symbol_I_string2("SAFE_GC")));
        ((list *) v6881)->addFast(_oid_(symbol_I_string2("collection")));
        ((list *) v6881)->addFast(_oid_(symbol_I_string2("error")));} 
      (v6880->bad_names = v6881);} 
    { Generate_c_producer * v6882 = Generate.C_plus_plusPRODUCER; 
      list * v6883;
      { GC_ANY(v6883= list::empty(Kernel.emptySet));
        ((list *) v6883)->addFast(_oid_(symbol_I_string2("DO")));
        ((list *) v6883)->addFast(_oid_(symbol_I_string2("IF")));
        ((list *) v6883)->addFast(_oid_(symbol_I_string2("ClaireAnd")));
        ((list *) v6883)->addFast(_oid_(symbol_I_string2("ClaireOr")));
        ((list *) v6883)->addFast(_oid_(symbol_I_string2("NOT")));
        ((list *) v6883)->addFast(_oid_(symbol_I_string2("PRINTF")));
        ((list *) v6883)->addFast(_oid_(symbol_I_string2("ClaireVoid")));
        ((list *) v6883)->addFast(_oid_(symbol_I_string2("ClairePattern")));
        ((list *) v6883)->addFast(_oid_(symbol_I_string2("ClaireException")));
        ((list *) v6883)->addFast(_oid_(symbol_I_string2("RETURN")));
        ((list *) v6883)->addFast(_oid_(symbol_I_string2("NEW")));
        ((list *) v6883)->addFast(_oid_(symbol_I_string2("ClaireClass")));
        ((list *) v6883)->addFast(_oid_(symbol_I_string2("PRIVATE")));
        ((list *) v6883)->addFast(_oid_(symbol_I_string2("ClaireOperator")));
        ((list *) v6883)->addFast(_oid_(symbol_I_string2("DEFAULT")));
        ((list *) v6883)->addFast(_oid_(symbol_I_string2("ClaireHandle")));
        ((list *) v6883)->addFast(_oid_(symbol_I_string2("ClaireImport")));
        ((list *) v6883)->addFast(_oid_(symbol_I_string2("CATCH")));
        ((list *) v6883)->addFast(_oid_(symbol_I_string2("STDOUT")));
        ((list *) v6883)->addFast(_oid_(symbol_I_string2("STDIN")));
        ((list *) v6883)->addFast(_oid_(symbol_I_string2("BREAK")));
        ((list *) v6883)->addFast(_oid_(symbol_I_string2("ClaireChar")));
        ((list *) v6883)->addFast(_oid_(symbol_I_string2("ClaireInterface")));
        ((list *) v6883)->addFast(_oid_(symbol_I_string2("_eof")));
        ((list *) v6883)->addFast(_oid_(symbol_I_string2("ClaireRelation")));
        ((list *) v6883)->addFast(_oid_(symbol_I_string2("Core")));
        ((list *) v6883)->addFast(_oid_(symbol_I_string2("ClaireObject")));
        ((list *) v6883)->addFast(_oid_(symbol_I_string2("_delete")));
        ((list *) v6883)->addFast(_oid_(symbol_I_string2("ClaireBoolean")));
        ((list *) v6883)->addFast(_oid_(symbol_I_string2("ClaireFunction")));
        ((list *) v6883)->addFast(_oid_(symbol_I_string2("ClaireType")));
        ((list *) v6883)->addFast(_oid_(symbol_I_string2("SystemThing")));
        ((list *) v6883)->addFast(_oid_(symbol_I_string2("ClaireEnvironment")));
        ((list *) v6883)->addFast(_oid_(symbol_I_string2("ABSTRACT")));
        ((list *) v6883)->addFast(_oid_(symbol_I_string2("FINAL")));
        ((list *) v6883)->addFast(_oid_(symbol_I_string2("SystemObject")));
        ((list *) v6883)->addFast(_oid_(symbol_I_string2("_NEW_ALLOC")));
        ((list *) v6883)->addFast(_oid_(symbol_I_string2("_BAG_UPDATE")));
        ((list *) v6883)->addFast(_oid_(symbol_I_string2("_SLOT_UPDATE")));
        ((list *) v6883)->addFast(_oid_(symbol_I_string2("_RETURN_ARG")));
        ((list *) v6883)->addFast(_oid_(symbol_I_string2("_SAFE_RESULT")));
        ((list *) v6883)->addFast(_oid_(symbol_I_string2("_SAFE_GC")));
        ((list *) v6883)->addFast(_oid_(symbol_I_string2("ClaireCollection")));
        ((list *) v6883)->addFast(_oid_(symbol_I_string2("ClaireError")));} 
      (v6882->good_names = v6883);} 
    (Generate.C_plus_plusPRODUCER->interfaces = list::alloc(12,_oid_(Kernel._port),
      _string_("ClairePort *"),
      _oid_(Kernel._string),
      _string_("char *"),
      _oid_(Kernel._char),
      _string_("ClaireChar *"),
      _oid_(Kernel._float),
      _string_("double "),
      _oid_(Kernel._array),
      _string_("OID *"),
      _oid_(Kernel._function),
      _string_("OID (*)()")));
    ;} 
  
  (Generate.PRODUCER->value= _oid_(Generate.C_plus_plusPRODUCER));
  
  Generate.c_string->addMethod(list::domain(2,Generate._c_producer,Language._Variable),Kernel._string,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(c_string_c_producer1,"c_string_c_producer1"));
  
  Generate.c_string->addMethod(list::domain(2,Generate._c_producer,Kernel._symbol),Kernel._string,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(c_string_c_producer2,"c_string_c_producer2"));
  
  Kernel.string_I->addMethod(list::domain(2,Generate._c_producer,Language._Variable),Kernel._string,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(string_I_c_producer1,"string!_c_producer1"));
  
  Kernel.string_I->addMethod(list::domain(2,Generate._c_producer,Kernel._symbol),Kernel._string,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(string_I_c_producer2,"string!_c_producer2"));
  
  Language.ident->addMethod(list::domain(2,Generate._c_producer,Language._Variable),Kernel._void,
  	NEW_ALLOC,_function_(ident_c_producer3,"ident_c_producer3"));
  
  Language.ident->addMethod(list::domain(2,Generate._c_producer,Kernel._symbol),Kernel._void,
  	0,_function_(ident_c_producer,"ident_c_producer"));
  
  Generate.class_princ->addMethod(list::domain(2,Generate._c_producer,Kernel._class),Kernel._void,
  	RETURN_ARG,_function_(class_princ_c_producer,"class_princ_c_producer"));
  
  Generate.produce->addMethod(list::domain(2,Generate._c_producer,Kernel._any),Kernel._void,
  	NEW_ALLOC+SLOT_UPDATE,_function_(produce_c_producer2,"produce_c_producer2"));
  
  Generate.globalVar->addMethod(list::domain(2,Generate._c_producer,Core._global_variable),Kernel._void,
  	NEW_ALLOC,_function_(globalVar_c_producer,"globalVar_c_producer"));
  
  Generate.stat_exp->addMethod(list::domain(3,Generate._c_producer,Kernel._any,Kernel._any),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(stat_exp_c_producer,"stat_exp_c_producer"));
  
  Generate.namespace_I->addMethod(list::domain(2,Generate._c_producer,Kernel._module),Kernel._void,
  	NEW_ALLOC,_function_(namespace_I_c_producer,"namespace!_c_producer"));
  
  Kernel.module_I->addMethod(list::domain(2,Generate._c_producer,Kernel._module),Kernel._void,
  	RETURN_ARG,_function_(module_I_c_producer,"module!_c_producer"));
  
  Generate.declare->addMethod(list::domain(2,Generate._c_producer,Kernel._property),Kernel._void,
  	NEW_ALLOC+SLOT_UPDATE,_function_(declare_c_producer,"declare_c_producer"));
  
  Generate.start_module_interface->addMethod(list::domain(2,Generate._c_producer,Kernel._module),Kernel._void,
  	NEW_ALLOC+SLOT_UPDATE,_function_(start_module_interface_c_producer,"start_module_interface_c_producer"));
  
  Generate.end_module_interface->addMethod(list::domain(2,Generate._c_producer,Kernel._module),Kernel._void,
  	0,_function_(end_module_interface_c_producer,"end_module_interface_c_producer"));
  
  Generate.generate_end_file->addMethod(list::domain(2,Generate._c_producer,Kernel._module),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(generate_end_file_c_producer,"generate_end_file_c_producer"));
  
  Generate.generate_classes->addMethod(list::domain(3,Generate._c_producer,Kernel._string,Kernel._module),Kernel._void,
  	NEW_ALLOC,_function_(generate_classes_c_producer,"generate_classes_c_producer"));
  
  Generate.generate_start_file->addMethod(list::domain(2,Generate._c_producer,Kernel._module),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(generate_start_file_c_producer,"generate_start_file_c_producer"));
  
  Generate.generate_meta_load->addMethod(list::domain(2,Generate._c_producer,Kernel._module),Kernel._void,
  	SLOT_UPDATE,_function_(generate_meta_load_c_producer,"generate_meta_load_c_producer"));
  
  Generate.start_file->addMethod(list::domain(2,Kernel._string,Kernel._module),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(start_file_string,"start_file_string"));
  
  Generate.define_variable->addMethod(list::domain(3,Generate._c_producer,Kernel._class,Kernel._string),Kernel._void,
  	NEW_ALLOC,_function_(define_variable_c_producer2,"define_variable_c_producer2"));
  
  Generate.generate_profile->addMethod(list::domain(2,Generate._c_producer,Kernel._any),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(generate_profile_c_producer,"generate_profile_c_producer"));
  
  Generate.generate_interface->addMethod(list::domain(2,Generate._c_producer,Kernel._module),Kernel._void,
  	NEW_ALLOC+SLOT_UPDATE,_function_(generate_interface_c_producer,"generate_interface_c_producer"));
  
  Generate.global_var_def_I->addMethod(list::domain(3,Generate._c_producer,Kernel._module,Language._Let),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(global_var_def_I_c_producer,"global_var_def!_c_producer"));
  
  Generate.gc_introduction->addMethod(list::domain(2,Generate._c_producer,Kernel._any),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(gc_introduction_c_producer,"gc_introduction_c_producer"));
  
  Generate.gc_usage->addMethod(list::domain(2,Kernel._any,Kernel._boolean),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(gc_usage_any,"gc_usage_any"));
  
  Generate.gc_or->addMethod(list::domain(2,Kernel._any,Kernel._any),Kernel._any,
  	RETURN_ARG,_function_(gc_or_any,"gc_or_any"));
  
  Generate.gc_usage_star->addMethod(list::domain(2,Kernel._list,Kernel._boolean),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(gc_usage_star_list,"gc_usage*_list"));
  
  Generate.debug_intro->addMethod(list::domain(3,Generate._c_producer,Core._lambda,Kernel._method),Kernel._void,
  	NEW_ALLOC,_function_(debug_intro_c_producer,"debug_intro_c_producer"));
  
  Generate.protect_result->addMethod(list::domain(4,Generate._c_producer,
    Kernel._class,
    Kernel._boolean,
    Kernel._any),Kernel._string,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(protect_result_c_producer,"protect_result_c_producer"));
  
  Generate.generate_function_start->addMethod(list::domain(5,Generate._c_producer,
    Core._lambda,
    Kernel._class,
    Kernel._any,
    Kernel._string),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(generate_function_start_c_producer,"generate_function_start_c_producer"));
  
  Generate.generate_regular_function->addMethod(list::domain(6,Generate._c_producer,
    Core._lambda,
    Kernel._function,
    Kernel._class,
    Kernel._any,
    Kernel._list),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(generate_regular_function_c_producer,"generate_regular_function_c_producer"));
  
  Generate.generate_float_function->addMethod(list::domain(3,Generate._c_producer,Kernel._method,Kernel._string),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(generate_float_function_c_producer,"generate_float_function_c_producer"));
  
  Generate.at->addMethod(list::domain(1,Generate._c_producer),Kernel._void,
  	0,_function_(at_c_producer,"at_c_producer"));
  
  Generate.generate_tuple_function->addMethod(list::domain(3,Generate._c_producer,Kernel._method,Kernel._string),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(generate_tuple_function_c_producer,"generate_tuple_function_c_producer"));
  
  Generate.create_function_entry->addMethod(list::domain(4,Generate._c_producer,
    Core._lambda,
    Kernel._string,
    Kernel._any),Kernel._any,
  	0,_function_(create_function_entry_c_producer,"create_function_entry_c_producer"));
  
  Generate.update_function_entry->addMethod(list::domain(4,Generate._c_producer,
    Kernel._function,
    Kernel._list,
    Kernel._class),Kernel._any,
  	NEW_ALLOC+SLOT_UPDATE,_function_(update_function_entry_c_producer,"update_function_entry_c_producer"));
  
  Optimize.c_interface->addMethod(list::domain(1,Kernel._class),Kernel._string,
  	RETURN_ARG,_function_(c_interface_class1_Generate,"c_interface_class1_Generate"));
  
  Optimize.c_interface->addMethod(list::domain(2,Kernel._class,Kernel._string),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(c_interface_class2_Generate,"c_interface_class2_Generate"));
  
  Optimize.c_interface->addMethod(list::domain(1,Kernel._method),Kernel._void,
  	NEW_ALLOC,_function_(c_interface_method_Generate,"c_interface_method_Generate"));
  
  Generate.interface_I->addMethod(list::domain(2,Generate._c_producer,Kernel._class),Kernel._void,
  	NEW_ALLOC,_function_(interface_I_c_producer,"interface!_c_producer"));
  
  Generate.to_cl->addMethod(list::domain(4,Generate._c_producer,
    Kernel._any,
    Kernel._class,
    Kernel._any),Kernel._void,
  	NEW_ALLOC+RETURN_ARG,_function_(to_cl_c_producer,"to_cl_c_producer"));
  
  Generate.to_c->addMethod(list::domain(4,Generate._c_producer,
    Kernel._any,
    Kernel._class,
    Kernel._any),Kernel._void,
  	NEW_ALLOC+RETURN_ARG,_function_(to_c_c_producer1,"to_c_c_producer1"));
  
  Generate.to_c->addMethod(list::domain(2,Generate._c_producer,Kernel._class),Kernel._void,
  	NEW_ALLOC,_function_(to_c_c_producer2,"to_c_c_producer2"));
  
  Generate.public_static->addMethod(list::domain(1,Generate._c_producer),Kernel._void,
  	0,_function_(public_static_c_producer,"public_static_c_producer"));
  
  Generate.bool_exp_I->addMethod(list::domain(3,Generate._c_producer,Kernel._any,Kernel._any),Kernel._void,
  	NEW_ALLOC,_function_(bool_exp_I_c_producer,"bool_exp!_c_producer"));
  
  Generate.inherit_exp->addMethod(list::domain(4,Generate._c_producer,
    Kernel._any,
    Kernel._any,
    Kernel._any),Kernel._void,
  	NEW_ALLOC,_function_(inherit_exp_c_producer,"inherit_exp_c_producer"));
  
  Generate.bitvector_exp->addMethod(list::domain(4,Generate._c_producer,
    Kernel._any,
    Kernel._any,
    Kernel._any),Kernel._void,
  	NEW_ALLOC,_function_(bitvector_exp_c_producer,"bitvector_exp_c_producer"));
  
  Generate.equal_exp->addMethod(list::domain(5,Generate._c_producer,
    Kernel._any,
    Kernel._boolean,
    Kernel._any,
    Kernel._any),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(equal_exp_c_producer,"equal_exp_c_producer"));
  
  Generate.char_exp_ask->addMethod(list::domain(2,Generate._c_producer,Kernel._any),Kernel._boolean,
  	0,_function_(char_exp_ask_c_producer2,"char_exp?_c_producer2"));
  
  Generate.char_exp->addMethod(list::domain(3,Generate._c_producer,Kernel._any,Kernel._any),Kernel._void,
  	NEW_ALLOC,_function_(char_exp_c_producer2,"char_exp_c_producer2"));
  
  Generate.c_member->addMethod(list::domain(5,Generate._c_producer,
    Kernel._any,
    Kernel._class,
    Kernel._property,
    Kernel._any),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(c_member_c_producer,"c_member_c_producer"));
  
  Generate.addFast->addMethod(list::domain(1,Generate._c_producer),Kernel._void,
  	0,_function_(addFast_c_producer,"addFast_c_producer"));
  
  Kernel.cast_I->addMethod(list::domain(3,Generate._c_producer,Optimize._C_cast,Kernel._any),Kernel._void,
  	NEW_ALLOC,_function_(cast_I_c_producer,"cast!_c_producer"));
  
  Generate.gc_protection_exp->addMethod(list::domain(5,Generate._c_producer,
    Language._Variable,
    Kernel._boolean,
    Kernel._any,
    Kernel._any),Kernel._void,
  	NEW_ALLOC+SLOT_UPDATE,_function_(gc_protection_exp_c_producer,"gc_protection_exp_c_producer"));
  
  Generate.bag_expression->addMethod(list::domain(5,Generate._c_producer,
    Kernel._class,
    Kernel._bag,
    Kernel._type,
    Kernel._any),Kernel._void,
  	NEW_ALLOC+SLOT_UPDATE,_function_(bag_expression_c_producer,"bag_expression_c_producer"));
  
  Generate.generate_s_file->addMethod(list::domain(3,Kernel._string,Kernel._list,Kernel._any),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(generate_s_file_string,"generate_s_file_string"));
  
  Generate.create_load_modules->addMethod(list::domain(4,Kernel._string,
    Kernel._port,
    Kernel._list,
    Kernel._any),Kernel._void,
  	NEW_ALLOC+SLOT_UPDATE,_function_(create_load_modules_string,"create_load_modules_string"));
  
  Generate.methods_interface->addMethod(list::domain(2,Generate._c_producer,Kernel._class),Kernel._void,
  	NEW_ALLOC,_function_(methods_interface_c_producer,"methods_interface_c_producer"));
  
  Generate.methods_bodies->addMethod(list::domain(2,Generate._c_producer,Kernel._class),Kernel._void,
  	NEW_ALLOC,_function_(methods_bodies_c_producer,"methods_bodies_c_producer"));
  
  Generate.inline_exp->addMethod(list::domain(3,Generate._c_producer,Language._Call_method1,Kernel._any),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(inline_exp_c_producer1,"inline_exp_c_producer1"));
  
  Generate.inline_exp->addMethod(list::domain(3,Generate._c_producer,Language._Call_method2,Kernel._any),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(inline_exp_c_producer2,"inline_exp_c_producer2"));
  
  Generate.inline_exp->addMethod(list::domain(3,Generate._c_producer,Language._Call_method,Kernel._any),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(inline_exp_c_producer3,"inline_exp_c_producer3"));
  
  Generate.print_external_call->addMethod(list::domain(3,Generate._c_producer,Language._Call_method,Kernel._any),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(print_external_call_c_producer,"print_external_call_c_producer"));
  
  Generate.inline_exp->addMethod(list::domain(3,Generate._c_producer,Language._Call,Kernel._any),Kernel._void,
  	NEW_ALLOC+SLOT_UPDATE+RETURN_ARG,_function_(inline_exp_c_producer5,"inline_exp_c_producer5"));
  
  { (Generate.fcall_ask = property::make("fcall?",3,Generate.it,Kernel._any,0));
    (Generate.fcall_ask->open = 3);
    ;} 
  
  { (Generate.fcall_exp = property::make("fcall_exp",3,Generate.it,Kernel._any,0));
    (Generate.fcall_exp->open = 3);
    ;} 
  
  { global_variable * v2072 = (Generate.FCLimit = (global_variable *) Core._global_variable->instantiate("FCLimit",Compile.it));
    (v2072->range = Kernel.emptySet);
    (v2072->value = 3);
    close_global_variable(v2072);
    } 
  
  { global_variable * v2072 = (Generate.FCALLSTINKS = (global_variable *) Core._global_variable->instantiate("FCALLSTINKS",claire.it));
    (v2072->range = Kernel._boolean);
    (v2072->value = Kernel.cfalse);
    close_global_variable(v2072);
    } 
  
  Generate.fcall_ask->addMethod(list::domain(1,Language._Call),Kernel._boolean,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(fcall_ask_Call2_Generate,"fcall?_Call2_Generate"));
  
  Generate.get_restrictions->addMethod(list::domain(2,Language._Call,Kernel._list),param_I_class(Kernel._list,Kernel._method),
  	NEW_ALLOC,_function_(get_restrictions_Call2,"get_restrictions_Call2"));
  
  Generate.fcall_exp->addMethod(list::domain(2,Language._Call,Kernel._any),Kernel._void,
  	NEW_ALLOC,_function_(fcall_exp_Call2_Generate,"fcall_exp_Call2_Generate"));
  
  Generate.c_sorted_arg->addMethod(list::domain(4,Kernel._any,
    Kernel._class,
    Kernel._any,
    Kernel._boolean),Kernel._void,
  	NEW_ALLOC,_function_(c_sorted_arg_any,"c_sorted_arg_any"));
  
  Generate.c_sorted_args->addMethod(list::domain(4,Language._Call,
    Kernel._list,
    Kernel._any,
    Kernel._boolean),Kernel._void,
  	NEW_ALLOC,_function_(c_sorted_args_Call,"c_sorted_args_Call"));
  
  Generate.bitvector_I->addMethod(list::domain(2,Generate._c_producer,Kernel._any),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(bitvector_I_c_producer,"bitvector!_c_producer"));
  
  { global_variable * v2072 = (Generate.bitvectorList = (global_variable *) Core._global_variable->instantiate("bitvectorList",Compile.it));
    (v2072->range = Kernel.emptySet);
    (v2072->value = _oid_(list::alloc(6,_string_("NEW_ALLOC"),
      _string_("BAG_UPDATE"),
      _string_("SLOT_UPDATE"),
      _string_("RETURN_ARG"),
      _string_("SAFE_RESULT"),
      _string_("SAFE_GC"))));
    close_global_variable(v2072);
    } 
  
  Generate.bitvectorSum->addMethod(list::domain(1,Kernel._integer),Kernel._void,
  	0,_function_(bitvectorSum_integer,"bitvectorSum_integer"));
  
  Generate.signature_I->addMethod(list::domain(2,Generate._c_producer,Kernel._any),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(signature_I_c_producer,"signature!_c_producer"));
  
  Generate.getC->addMethod(list::domain(1,Kernel._any),Kernel._any,
  	NEW_ALLOC+RETURN_ARG,_function_(getC_any,"getC_any"));
  
  Generate.gassign->addMethod(list::domain(3,Generate._c_producer,Language._Gassign,Kernel._any),Kernel._void,
  	NEW_ALLOC,_function_(gassign_c_producer,"gassign_c_producer"));
  
  Generate.call_slot->addMethod(list::domain(3,Generate._c_producer,Language._Call_slot,Kernel._any),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(call_slot_c_producer,"call_slot_c_producer"));
  
  Generate.call_table->addMethod(list::domain(3,Generate._c_producer,Language._Call_table,Kernel._any),Kernel._void,
  	NEW_ALLOC,_function_(call_table_c_producer,"call_table_c_producer"));
  
  Generate.call_array->addMethod(list::domain(3,Generate._c_producer,Language._Call_array,Kernel._any),Kernel._void,
  	NEW_ALLOC,_function_(call_array_c_producer,"call_array_c_producer"));
  
  Generate.update->addMethod(list::domain(3,Generate._c_producer,Language._Update,Kernel._any),Kernel._void,
  	NEW_ALLOC,_function_(update_c_producer,"update_c_producer"));
  
  Generate.object_test->addMethod(list::domain(4,Generate._c_producer,
    Kernel._any,
    Kernel._boolean,
    Kernel._any),Kernel._void,
  	NEW_ALLOC,_function_(object_test_c_producer,"object_test_c_producer"));
  
  Generate.exp_to_protect->addMethod(list::domain(3,Generate._c_producer,Optimize._to_protect,Kernel._any),Kernel._void,
  	NEW_ALLOC+RETURN_ARG,_function_(exp_to_protect_c_producer,"exp_to_protect_c_producer"));
  
  Generate.macro->addMethod(list::domain(1,Generate._c_producer),Kernel._void,
  	0,_function_(macro_c_producer,"macro_c_producer"));
  
  Generate.init_var->addMethod(list::domain(2,Generate._c_producer,Kernel._class),Kernel._void,
  	0,_function_(init_var_c_producer,"init_var_c_producer"));
  
  Generate.any_interface->addMethod(list::domain(1,Generate._c_producer),Kernel._void,
  	0,_function_(any_interface_c_producer,"any_interface_c_producer"));
  
  Generate.pointer_cast->addMethod(list::domain(2,Generate._c_producer,Kernel._class),Kernel._void,
  	0,_function_(pointer_cast_c_producer,"pointer_cast_c_producer"));
  
  Generate.exp_Assign->addMethod(list::domain(3,Generate._c_producer,Language._Assign,Kernel._any),Kernel._void,
  	NEW_ALLOC,_function_(exp_Assign_c_producer,"exp_Assign_c_producer"));
  
  Generate.stat_handle->addMethod(list::domain(4,Generate._c_producer,
    Language._Handle,
    Kernel._any,
    Kernel._any),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(stat_handle_c_producer,"stat_handle_c_producer"));
  
  Generate.stat_construct->addMethod(list::domain(4,Generate._c_producer,
    Language._Construct,
    Kernel._any,
    Kernel._any),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(stat_construct_c_producer,"stat_construct_c_producer"));
  
  Generate.stat_while->addMethod(list::domain(4,Generate._c_producer,
    Language._While,
    Kernel._any,
    Kernel._any),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(stat_while_c_producer,"stat_while_c_producer"));
  
  Generate.stat_gassign->addMethod(list::domain(4,Generate._c_producer,
    Language._Gassign,
    Kernel._any,
    Kernel._any),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(stat_gassign_c_producer,"stat_gassign_c_producer"));
  
  Generate.stat_for->addMethod(list::domain(4,Generate._c_producer,
    Language._For,
    Kernel._any,
    Kernel._any),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(stat_for_c_producer,"stat_for_c_producer"));
  
  Generate.stat_iteration->addMethod(list::domain(4,Generate._c_producer,
    Language._Iteration,
    Kernel._any,
    Kernel._any),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(stat_iteration_c_producer,"stat_iteration_c_producer"));
  
  Generate.stat_super->addMethod(list::domain(4,Generate._c_producer,
    Language._Super,
    Kernel._any,
    Kernel._any),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(stat_super_c_producer,"stat_super_c_producer"));
  
  Generate.stat_let->addMethod(list::domain(4,Generate._c_producer,
    Language._Let,
    Kernel._any,
    Kernel._any),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(stat_let_c_producer,"stat_let_c_producer"));
  
  GC_UNBIND;} 

