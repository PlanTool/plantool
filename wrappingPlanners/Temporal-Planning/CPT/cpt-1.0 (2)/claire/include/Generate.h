// interface defination for module Generate, Sat Oct 16 06:53:38 2004
#ifndef CLAIREH_Generate
#define CLAIREH_Generate


class Generate_producer;
class Generate_c_producer;

class Generate_producer: public thing{ 
  public:
     list *open_comparators;
     list *open_operators;
     OID body;
     char *extension;
     char *comment;
     list *interfaces;
     int stat;} 
;

class Generate_c_producer: public Generate_producer{ 
  public:
     list *bad_names;
     list *good_names;} 
;
extern void  ident_symbol(symbol *v1140);
extern void  ident_thing(thing *v1140);
extern void  ident_class(ClaireClass *v1140);
extern void  interface_I_class(ClaireClass *v7227);
extern void  class_princ_class(ClaireClass *v7227);
extern OID  indent_c_void();
extern OID  breakline_void();
extern void  new_block_void();
extern void  close_block_void();
extern void  c_test_any(OID v7248);
extern void  c_test_method(method *v7237);
extern void  compile_module(module *v1140);
extern void  generate_files_module(module *v1140);
extern void  generate_f2f_module(module *v7237);
extern void  generate_file_string1(char *v7243,module *v7237);
extern void  generate_classes_module(module *v1140);
extern void  generate_c2f_module(module *v1140);
extern void  generate_interface_module(module *v1140,bag *v7236);
extern void  generate_objects_module(module *v1140);
extern void  generate_meta_load_module(module *v1140);
extern ClaireBoolean * global_var_def_ask_any(OID v7248);
extern ClaireClass * getRange_global_variable(global_variable *v7248);
extern void  generate_functions_module(module *v1140);
extern list * parents_module(module *v1140,list *v7236);
extern list * parents_list(list *v1140);
extern void  get_module2(module *v7237);
extern void  generate_file_string2(char *v1140,char *v11188);
extern OID  make_c_function_lambda_Generate(lambda *v1140,char *v12719,OID v7237);
extern OID  print_c_function_lambda2(lambda *v1140,char *v12719,OID v7237);
extern void  print_body_If(If *v3871,char *v4031,ClaireClass *v7243,If *v6350,ClaireBoolean *v14719);
extern void  print_body_any(OID v3871,char *v4031,ClaireClass *v7243,OID v6350,ClaireBoolean *v14719);
extern OID  print_body_Do(Do *v3871,char *v4031,ClaireClass *v7243,OID v6350,ClaireBoolean *v14719);
extern void  outer_statement_any(OID v3871,char *v4031,ClaireClass *v7243,ClaireBoolean *v14719);
extern ClaireBoolean * c_safe_any(OID v7248);
extern ClaireClass * check_sort_method(method *v1140);
extern OID  typed_args_list_list(list *v1140);
extern ClaireBoolean * need_debug_ask_any(OID v7237);
extern void  get_dependents_method(method *v7237);
extern void  c_princ_function(ClaireFunction *v1140);
extern void  set_outfile_lambda(lambda *v1140);
extern ClaireBoolean * c_func_any(OID v1140);
extern void  expression_thing(thing *v1140,OID v15308);
extern void  expression_integer(int v1140,OID v15308);
extern void  expression_any(OID v1140,OID v15308);
extern void  expression_string(char *v1140,OID v15308);
extern void  expression_float(double v1140,OID v15308);
extern void  expression_float_(OID v6626,OID v6627);
extern void  expression_boolean(ClaireBoolean *v1140,OID v15308);
extern void  expression_environment(ClaireEnvironment *v1140,OID v15308);
extern void  expression_Variable(Variable *v1140,OID v15308);
extern void  expression_global_variable(global_variable *v1140,OID v15308);
extern void  expression_Set(Set *v1140,OID v15308);
extern void  expression_set2(set *v1140,OID v15308);
extern void  expression_Tuple(Tuple *v1140,OID v15308);
extern void  expression_tuple(tuple *v1140,OID v15308);
extern void  expression_List(List *v1140,OID v15308);
extern void  expression_list(list *v1140,OID v15308);
extern void  expression_Call2(Call *v1140,OID v15308);
extern void  expression_Call_method12(Call_method1 *v1140,OID v15308);
extern void  expression_Call_method22(Call_method2 *v1140,OID v15308);
extern void  expression_Call_method2(Call_method *v1140,OID v15308);
extern void  bexpression_any(OID v1140,OID v15308);
extern void  expression_If(If *v1140,OID v15308);
extern void  expression_Assign(Assign *v1140,OID v15308);
extern void  expression_to_protect(Compile_to_protect *v1140,OID v15308);
extern char * gc_protect_class(ClaireClass *v7227);
extern void  expression_Gassign(Gassign *v1140,OID v15308);
extern void  expression_And(And *v1140,OID v15308);
extern void  expression_Or(Or *v1140,OID v15308);
extern void  expression_to_CL(Compile_to_CL *v1140,OID v15308);
extern void  expression_to_C(Compile_to_C *v1140,OID v15308);
extern void  expression_C_cast(Compile_C_cast *v1140,OID v15308);
extern void  expression_Call_slot(Call_slot *v1140,OID v15308);
extern void  expression_Call_table(Call_table *v1140,OID v15308);
extern void  expression_Call_array(Call_array *v1140,OID v15308);
extern void  expression_Update(Update *v1140,OID v15308);
extern void  sign_equal_boolean(ClaireBoolean *v1140);
extern void  sign_or_boolean(ClaireBoolean *v1140);
extern void  bool_exp_any_Generate(OID v1140,ClaireBoolean *v3475,OID v15308);
extern void  any_bool_exp_any(OID v1140,ClaireBoolean *v3475,OID v15308,ClaireBoolean *v12390);
extern void  bool_exp_to_CL_Generate(Compile_to_CL *v1140,ClaireBoolean *v3475,OID v15308);
extern void  bool_exp_If_Generate(If *v1140,ClaireBoolean *v3475,OID v15308);
extern void  bool_exp_And_Generate(And *v1140,ClaireBoolean *v3475,OID v15308);
extern void  bool_exp_Or_Generate(Or *v1140,ClaireBoolean *v3475,OID v15308);
extern void  bool_exp_Call_method1_Generate(Call_method1 *v1140,ClaireBoolean *v3475,OID v15308);
extern void  bool_exp_Call_method2_Generate(Call_method2 *v1140,ClaireBoolean *v3475,OID v15308);
extern ClaireBoolean * bool_exp_ask_any(OID v7248);
extern void  args_list_bag(bag *v1140,OID v15308,ClaireBoolean *v750);
extern char * check_var_string(char *v1140,OID v7243,OID v15308);
extern Variable * build_Variable_string(char *v7243,OID v7244);
extern list * unfold_args_list(list *v7236);
extern ClaireType * c_type_sort_any(OID v7248);
extern OID  unfold_arg_list(list *v7236,list *v11424,OID v7248);
extern void  unfold_use_list(list *v4417,OID v7248,OID v7243,OID v15308);
extern void  statement_any(OID v1140,OID v7243,OID v15308);
extern void  self_statement_Construct(Construct *v1140,OID v7243,OID v15308);
extern void  self_statement_If(If *v1140,OID v7243,OID v15308);
extern void  self_statement_Do(Do *v1140,OID v7243,OID v15308);
extern void  inner_statement_any(OID v1140,OID v7243,OID v15308);
extern void  self_statement_Let(Let *v1140,OID v7243,OID v15308);
extern void  self_statement_And(And *v1140,OID v7243,OID v15308);
extern void  self_statement_Or(Or *v1140,OID v7243,OID v15308);
extern void  self_statement_While(While *v1140,OID v7243,OID v15308);
extern void  self_statement_Assign(Assign *v1140,OID v7243,OID v15308);
extern void  self_statement_Gassign(Gassign *v1140,OID v7243,OID v15308);
extern void  self_statement_to_protect(Compile_to_protect *v1140,OID v7243,OID v15308);
extern void  self_statement_For(For *v1140,OID v7243,OID v15308);
extern void  self_statement_Iteration(Iteration *v1140,OID v7243,OID v15308);
extern void  self_statement_Return(Return *v1140,OID v7243,OID v15308);
extern void  self_statement_Call(Call *v1140,OID v7243,OID v15308);
extern void  self_statement_Call_method(Call_method *v1140,OID v7243,OID v15308);
extern void  self_statement_Call_method1(Call_method1 *v1140,OID v7243,OID v15308);
extern void  self_statement_Call_method2(Call_method2 *v1140,OID v7243,OID v15308);
extern void  self_statement_Super(Super *v1140,OID v7243,OID v15308);
extern void  self_statement_Cast(Cast *v1140,OID v7243,OID v15308);
extern void  self_statement_Handle(ClaireHandle *v1140,OID v7243,OID v15308);
extern void  self_statement_to_CL(Compile_to_CL *v1140,OID v7243,OID v15308);
extern void  self_statement_to_C(Compile_to_C *v1140,OID v7243,OID v15308);
extern void  self_statement_C_cast(Compile_C_cast *v1140,OID v7243,OID v15308);
extern void  self_statement_Call_slot(Call_slot *v1140,OID v7243,OID v15308);
extern void  self_statement_Call_table(Call_table *v1140,OID v7243,OID v15308);
extern void  self_statement_Call_array(Call_array *v1140,OID v7243,OID v15308);
extern void  self_statement_Update(Update *v1140,OID v7243,OID v15308);
extern char * c_string_c_producer1(Generate_c_producer *v7227,Variable *v1140);
extern char * c_string_c_producer2(Generate_c_producer *v7227,symbol *v1140);
extern char * string_I_c_producer1(Generate_c_producer *v7227,Variable *v1140);
extern char * string_I_c_producer2(Generate_c_producer *v7227,symbol *v1140);
extern void  ident_c_producer3(Generate_c_producer *v7227,Variable *v7247);
extern void  ident_c_producer(Generate_c_producer *v7227,symbol *v7243);
extern void  class_princ_c_producer(Generate_c_producer *v7227,ClaireClass *v1140);
extern void  produce_c_producer2(Generate_c_producer *v7227,OID v7248);
extern void  globalVar_c_producer(Generate_c_producer *v7227,global_variable *v7248);
extern void  stat_exp_c_producer(Generate_c_producer *v7227,OID v1140,OID v15308);
extern void  namespace_I_c_producer(Generate_c_producer *v7227,module *v7237);
extern void  module_I_c_producer(Generate_c_producer *v7227,module *v7237);
extern void  declare_c_producer(Generate_c_producer *v7227,property *v7240);
extern void  start_module_interface_c_producer(Generate_c_producer *v7227,module *v1140);
extern void  end_module_interface_c_producer(Generate_c_producer *v7227,module *v1140);
extern void  generate_end_file_c_producer(Generate_c_producer *v7234,module *v7237);
extern void  generate_classes_c_producer(Generate_c_producer *v7234,char *v7243,module *v7237);
extern void  generate_start_file_c_producer(Generate_c_producer *v7227,module *v7237);
extern void  generate_meta_load_c_producer(Generate_c_producer *v7227,module *v7237);
extern OID  start_file_string(char *v7243,module *v7237);
extern void  define_variable_c_producer2(Generate_c_producer *v7227,ClaireClass *v7244,char *v7247);
extern void  generate_profile_c_producer(Generate_c_producer *v7227,OID v7237);
extern void  generate_interface_c_producer(Generate_c_producer *v7227,module *v1140);
extern void  global_var_def_I_c_producer(Generate_c_producer *v7227,module *v1140,Let *v7248);
extern void  gc_introduction_c_producer(Generate_c_producer *v7227,OID v9221);
extern OID  gc_usage_any(OID v1140,ClaireBoolean *v15308);
extern OID  gc_or_any(OID v7248,OID v7249);
extern OID  gc_usage_star_list(list *v7236,ClaireBoolean *v15308);
extern void  debug_intro_c_producer(Generate_c_producer *v7227,lambda *v1140,method *v7248);
extern char * protect_result_c_producer(Generate_c_producer *v7227,ClaireClass *v7243,ClaireBoolean *v13756,OID v7248);
extern void  generate_function_start_c_producer(Generate_c_producer *v7227,lambda *v1140,ClaireClass *v7243,OID v7237,char *v12719);
extern void  generate_regular_function_c_producer(Generate_c_producer *v7227,lambda *v1140,ClaireFunction *v9226,ClaireClass *v7243,OID v7237,list *v11442);
extern void  generate_float_function_c_producer(Generate_c_producer *v7227,method *v7237,char *v12719);
extern void  at_c_producer(Generate_c_producer *v7227);
extern void  generate_tuple_function_c_producer(Generate_c_producer *v7227,method *v7237,char *v12719);
extern OID  create_function_entry_c_producer(Generate_c_producer *v7227,lambda *v11422,char *v9226,OID v7237);
extern OID  update_function_entry_c_producer(Generate_c_producer *v7227,ClaireFunction *v9226,list *v11442,ClaireClass *v7243);
extern char * c_interface_class1_Generate(ClaireClass *v1140);
extern void  c_interface_class2_Generate(ClaireClass *v1140,char *v7243);
extern void  c_interface_method_Generate(method *v1140);
extern void  interface_I_c_producer(Generate_c_producer *v7227,ClaireClass *v1140);
extern void  to_cl_c_producer(Generate_c_producer *v7227,OID v7248,ClaireClass *v7243,OID v15308);
extern void  to_c_c_producer1(Generate_c_producer *v7227,OID v7248,ClaireClass *v7243,OID v15308);
extern void  to_c_c_producer2(Generate_c_producer *v7227,ClaireClass *v7243);
extern void  public_static_c_producer(Generate_c_producer *v7227);
extern void  bool_exp_I_c_producer(Generate_c_producer *v7227,OID v1140,OID v15308);
extern void  inherit_exp_c_producer(Generate_c_producer *v7227,OID v11032,OID v11033,OID v15308);
extern void  bitvector_exp_c_producer(Generate_c_producer *v7227,OID v11032,OID v11033,OID v15308);
extern void  equal_exp_c_producer(Generate_c_producer *v7227,OID v11032,ClaireBoolean *v3475,OID v11033,OID v7260);
extern ClaireBoolean * char_exp_ask_c_producer2(Generate_c_producer *v7227,OID v7248);
extern void  char_exp_c_producer2(Generate_c_producer *v7227,OID v7248,OID v15308);
extern void  c_member_c_producer(Generate_c_producer *v7227,OID v1140,ClaireClass *v7243,property *v7248,OID v15308);
extern void  addFast_c_producer(Generate_c_producer *v7227);
extern void  cast_I_c_producer(Generate_c_producer *v7227,Compile_C_cast *v1140,OID v15308);
extern void  gc_protection_exp_c_producer(Generate_c_producer *v7227,Variable *v7247,ClaireBoolean *v12010,OID v7245,OID v15308);
extern void  bag_expression_c_producer(Generate_c_producer *v11165,ClaireClass *v7227,bag *v7236,ClaireType *v7244,OID v15308);
extern void  generate_s_file_string(char *v1140,list *v7236,OID v7237);
extern void  create_load_modules_string(char *v1140,ClairePort *v7240,list *v10516,OID v7237);
extern void  methods_interface_c_producer(Generate_c_producer *v7227,ClaireClass *v7248);
extern void  methods_bodies_c_producer(Generate_c_producer *v7227,ClaireClass *v7248);
extern void  inline_exp_c_producer1(Generate_c_producer *v7227,Call_method1 *v1140,OID v15308);
extern void  inline_exp_c_producer2(Generate_c_producer *v7227,Call_method2 *v1140,OID v15308);
extern void  inline_exp_c_producer3(Generate_c_producer *v7227,Call_method *v1140,OID v15308);
extern void  print_external_call_c_producer(Generate_c_producer *v7227,Call_method *v1140,OID v15308);
extern void  inline_exp_c_producer5(Generate_c_producer *v7227,Call *v1140,OID v15308);
extern ClaireBoolean * fcall_ask_Call2_Generate(Call *v7248);
extern list * get_restrictions_Call2(Call *v7248,list *v11440);
extern void  fcall_exp_Call2_Generate(Call *v7248,OID v15308);
extern void  c_sorted_arg_any(OID v7248,ClaireClass *v7243,OID v15308,ClaireBoolean *v4284);
extern void  c_sorted_args_Call(Call *v7248,list *v11439,OID v15308,ClaireBoolean *v4284);
extern void  bitvector_I_c_producer(Generate_c_producer *v7227,OID v7248);
extern void  bitvectorSum_integer(int v7248);
extern void  signature_I_c_producer(Generate_c_producer *v7227,OID v7248);
extern OID  getC_any(OID v7248);
extern void  gassign_c_producer(Generate_c_producer *v7227,Gassign *v1140,OID v15308);
extern void  call_slot_c_producer(Generate_c_producer *v7227,Call_slot *v1140,OID v15308);
extern void  call_table_c_producer(Generate_c_producer *v7227,Call_table *v1140,OID v15308);
extern void  call_array_c_producer(Generate_c_producer *v7227,Call_array *v1140,OID v15308);
extern void  update_c_producer(Generate_c_producer *v7227,Update *v1140,OID v15308);
extern void  object_test_c_producer(Generate_c_producer *v7227,OID v11032,ClaireBoolean *v3475,OID v15308);
extern void  exp_to_protect_c_producer(Generate_c_producer *v7227,Compile_to_protect *v1140,OID v15308);
extern void  macro_c_producer(Generate_c_producer *v7227);
extern void  init_var_c_producer(Generate_c_producer *v7227,ClaireClass *v7243);
extern void  any_interface_c_producer(Generate_c_producer *v7227);
extern void  pointer_cast_c_producer(Generate_c_producer *v7227,ClaireClass *v7243);
extern void  exp_Assign_c_producer(Generate_c_producer *v7227,Assign *v1140,OID v15308);
extern void  stat_handle_c_producer(Generate_c_producer *v7227,ClaireHandle *v1140,OID v7243,OID v15308);
extern void  stat_construct_c_producer(Generate_c_producer *v7227,Construct *v1140,OID v7243,OID v15308);
extern void  stat_while_c_producer(Generate_c_producer *v7227,While *v1140,OID v7243,OID v15308);
extern void  stat_gassign_c_producer(Generate_c_producer *v7227,Gassign *v1140,OID v7243,OID v15308);
extern void  stat_for_c_producer(Generate_c_producer *v7227,For *v1140,OID v7243,OID v15308);
extern void  stat_iteration_c_producer(Generate_c_producer *v7227,Iteration *v1140,OID v7243,OID v15308);
extern void  stat_super_c_producer(Generate_c_producer *v7227,Super *v1140,OID v7243,OID v15308);
extern void  stat_let_c_producer(Generate_c_producer *v7227,Let *v1140,OID v7243,OID v15308);

// namespace class for Generate 
class GenerateClass: public NameSpace {
public:

global_variable * _star_ask_interval_star;
global_variable * _star_dash_dash_integer_star;
global_variable * _star_plus_integer_star;
global_variable * _starnth_integer_star;
global_variable * _starnth_list_star;
global_variable * _starnth_1_bag_star;
global_variable * _starnth_string_star;
global_variable * _starnth_1_string_star;
global_variable * _starnth_equal_list_star;
global_variable * _starnot_star;
global_variable * _starknown_star;
global_variable * _starunknown_star;
global_variable * _starnot_equal_star;
global_variable * _starcontain_star;
global_variable * _starmin_integer_star;
global_variable * _starmax_integer_star;
global_variable * _starlength_array_star;
global_variable * _starlength_bag_star;
global_variable * _starclose_exception_star;
ClaireClass * _producer;
global_variable * PRODUCER;
table * classFile;
global_variable * WrongMethod;
ClaireClass * _c_producer;
Generate_c_producer * C_plus_plusPRODUCER;
property * fcall_ask;
property * fcall_exp;
global_variable * FCLimit;
global_variable * FCALLSTINKS;
global_variable * bitvectorList;
property * open_comparators;// Generate/"open_comparators"
property * open_operators;// Generate/"open_operators"
property * extension;// Generate/"extension"
property * interfaces;// Generate/"interfaces"
property * interface_I;// Generate/"interface!"
property * indent_c;// Generate/"indent_c"
property * breakline;// Generate/"breakline"
property * new_block;// Generate/"new_block"
property * close_block;// Generate/"close_block"
property * c_test;// claire/"c_test"
property * c_func;// Compile/"c_func"
property * expression;// Generate/"expression"
property * statement;// Generate/"statement"
property * compile;// claire/"compile"
property * parents;// Generate/"parents"
property * outmodule;// Generate/"outmodule"
property * generate_files;// Generate/"generate_files"
property * generate_classes;// Generate/"generate_classes"
property * generate_c2f;// Generate/"generate_c2f"
property * generate_f2f;// Generate/"generate_f2f"
property * generate_interface;// Generate/"generate_interface"
property * start_module_interface;// Generate/"start_module_interface"
property * generate_file;// Generate/"generate_file"
property * start_file;// Generate/"start_file"
property * generate_meta_load;// Generate/"generate_meta_load"
property * generate_start_file;// Generate/"generate_start_file"
property * generate_functions;// Generate/"generate_functions"
property * generate_objects;// Generate/"generate_objects"
property * generate_end_file;// Generate/"generate_end_file"
property * typed_args_list;// Generate/"typed_args_list"
property * namespace_I;// Generate/"namespace!"
property * public_static;// Generate/"public_static"
property * declare;// Generate/"declare"
property * print_c_function;// Generate/"print_c_function"
property * create_function_entry;// Generate/"create_function_entry"
property * check_sort;// Generate/"check_sort"
property * protect_result;// Generate/"protect_result"
property * need_debug_ask;// Generate/"need_debug?"
property * set_outfile;// Generate/"set_outfile"
property * generate_profile;// Generate/"generate_profile"
property * debug_intro;// Generate/"debug_intro"
property * inner_statement;// Generate/"inner_statement"
property * update_function_entry;// Generate/"update_function_entry"
property * get_dependents;// Generate/"get_dependents"
property * produce;// Generate/"produce"
property * at;// Generate/"at"
property * bag_expression;// Generate/"bag_expression"
property * inline_exp;// Generate/"inline_exp"
property * gc_protection_exp;// Generate/"gc_protection_exp"
property * exp_to_protect;// Generate/"exp_to_protect"
property * gc_protect;// Generate/"gc_protect"
property * gassign;// Generate/"gassign"
property * to_cl;// Generate/"to_cl"
property * to_c;// Generate/"to_c"
property * call_slot;// Generate/"call_slot"
property * call_table;// Generate/"call_table"
property * call_array;// Generate/"call_array"
property * update;// Generate/"update"
property * sign_equal;// Generate/"sign_equal"
property * sign_or;// Generate/"sign_or"
property * macro;// Generate/"macro"
property * equal_exp;// Generate/"equal_exp"
property * object_test;// Generate/"object_test"
property * bitvector_exp;// Generate/"bitvector_exp"
property * inherit_exp;// Generate/"inherit_exp"
property * args_list;// Generate/"args_list"
property * check_var;// Generate/"check_var"
property * build_Variable;// Generate/"build_Variable"
property * unfold_args;// Generate/"unfold_args"
property * c_type_sort;// Generate/"c_type_sort"
property * unfold_arg;// Generate/"unfold_arg"
property * unfold_use;// Generate/"unfold_use"
property * self_statement;// Generate/"self_statement"
property * stat_construct;// Generate/"stat_construct"
property * init_var;// Generate/"init_var"
property * stat_while;// Generate/"stat_while"
property * stat_gassign;// Generate/"stat_gassign"
property * stat_for;// Generate/"stat_for"
property * stat_iteration;// Generate/"stat_iteration"
property * stat_super;// Generate/"stat_super"
property * stat_handle;// Generate/"stat_handle"
property * any_interface;// Generate/"any_interface"
property * pointer_cast;// Generate/"pointer_cast"
property * bad_names;// Generate/"bad_names"
property * good_names;// Generate/"good_names"
property * generate_float_function;// Generate/"generate_float_function"
property * generate_regular_function;// Generate/"generate_regular_function"
property * bitvectorSum;// Generate/"bitvectorSum"
property * c_member;// Generate/"c_member"
property * addFast;// Generate/"addFast"
property * print_external_call;// Generate/"print_external_call"
property * getC;// Generate/"getC"
property * signature_I;// Generate/"signature!"
property * bitvector_I;// Generate/"bitvector!"
property * get_restrictions;// Generate/"get_restrictions"
property * c_sorted_args;// Generate/"c_sorted_args"
property * c_sorted_arg;// Generate/"c_sorted_arg"
property * generate_function_start;// Generate/"generate_function_start"
property * print_body;// Generate/"print_body"
property * c_safe;// Generate/"c_safe"
property * outer_statement;// Generate/"outer_statement"
property * methods_interface;// Generate/"methods_interface"
property * methods_bodies;// Generate/"methods_bodies"
property * gc_introduction;// Generate/"gc_introduction"
property * gc_usage;// Generate/"gc_usage"
property * gc_usage_star;// Generate/"gc_usage*"
property * gc_or;// Generate/"gc_or"
property * stat_exp;// Generate/"stat_exp"
property * define_variable;// Generate/"define_variable"
property * char_exp_ask;// Generate/"char_exp?"
property * char_exp;// Generate/"char_exp"
property * bool_exp_ask;// Generate/"bool_exp?"
property * bool_exp_I;// Generate/"bool_exp!"
property * bexpression;// Generate/"bexpression"
property * end_module_interface;// Generate/"end_module_interface"
property * stat_let;// Generate/"stat_let"
property * exp_Assign;// Generate/"exp_Assign"
property * generate_tuple_function;// Generate/"generate_tuple_function"
property * generate_s_file;// Generate/"generate_s_file"
property * create_load_modules;// Generate/"create_load_modules"
property * global_var_def_ask;// Generate/"global_var_def?"
property * global_var_def_I;// Generate/"global_var_def!"
property * getRange;// Generate/"getRange"
property * globalVar;// Generate/"globalVar"
property * c_string;// Generate/"c_string"
property * any_bool_exp;// Generate/"any_bool_exp"
property * class_princ;// Generate/"class_princ"

// module definition 
 void metaLoad();};

extern GenerateClass Generate;
extern NameSpace Compile;
extern NameSpace iClaire;

#endif
