// interface defination for module Core, Sat Oct 16 06:53:25 2004
#ifndef CLAIREH_Core
#define CLAIREH_Core


class ephemeral_object;
class lambda;
class general_error;
class read_slot_error;
class range_error;
class selector_error;
class return_error;
class contradiction;
class global_variable;
class pretty_printer;
class much_too_far;
class Type;
class Union;
class Interval;
class Param;
class subtype;
class Reference;

class ephemeral_object: public ClaireObject{ 
  public:} 
;

class lambda: public ClaireObject{ 
  public:
     list *vars;
     OID body;
     int dimension;} 
;

class general_error: public ClaireError{ 
  public:
     OID cause;
     OID arg;} 
;

class read_slot_error: public ClaireError{ 
  public:
     OID arg;
     OID wrong;} 
;

class range_error: public ClaireError{ 
  public:
     OID cause;
     OID arg;
     OID wrong;} 
;

class selector_error: public ClaireError{ 
  public:
     OID selector;
     OID arg;} 
;

class return_error: public ClaireError{ 
  public:
     OID arg;} 
;

class contradiction: public ClaireException{ 
  public:} 
;

class global_variable: public SystemThing{ 
  public:
     OID value;
     ClaireType *range;
     ClaireBoolean *store_ask;} 
;

class pretty_printer: public thing{ 
  public:
     ClairePort *cpretty;
     int cprevious;
     int index;
     int width;
     ClaireBoolean *pprint;
     ClaireBoolean *pbreak;
     list *cpstack;} 
;

class much_too_far: public ClaireError{ 
  public:} 
;

class Type: public ClaireType{ 
  public:} 
;

class Union: public Type{ 
  public:
     ClaireType *t1;
     ClaireType *t2;} 
;

class Interval: public Type{ 
  public:
     int arg1;
     int arg2;} 
;

class Param: public Type{ 
  public:
     ClaireClass *arg;
     list *params;
     list *args;} 
;

class subtype: public Type{ 
  public:
     ClaireClass *arg;
     ClaireType *t1;} 
;

class Reference: public Type{ 
  public:
     list *args;
     int index;
     ClaireBoolean *arg;} 
;
extern OID  eval_message_property(property *self,ClaireObject *r,int start,ClaireBoolean *int_ask);
extern OID  noeval_message_property2(property *self,int start);
extern OID  execute_method(method *self,int start,ClaireBoolean *int_ask);
extern OID  eval_any(OID self);
extern OID  self_eval_ClaireObject(ClaireObject *self);
extern method * inlineok_ask_method(method *self,char *s);
extern OID  get_slot(slot *s,ClaireObject *x);
extern OID  put_slot(slot *s,ClaireObject *x,OID y);
extern OID  get_property(property *self,ClaireObject *x);
extern OID  funcall_property(property *self,OID x);
extern OID  read_property(property *self,ClaireObject *x);
extern ClaireBoolean * hold_ask_property(property *self,ClaireObject *x,OID y);
extern void  write_property(property *self,ClaireObject *x,OID y);
extern OID  range_is_wrong_slot(slot *self,OID y);
extern void  put_property1(property *p,ClaireObject *x,int n,ClaireClass *s,OID y);
extern void  update_property(property *p,ClaireObject *x,int n,ClaireClass *s,OID y);
extern void  update_plus_relation(ClaireRelation *self,OID x,OID y);
extern void  update_dash_relation(ClaireRelation *r,OID x,OID y);
extern void  add_I_property(property *self,ClaireObject *x,int n,OID y);
extern ClaireBoolean * add_value_property(property *self,ClaireObject *x,int n,bag *l,OID y);
extern void  add_property(property *self,ClaireObject *x,OID y);
extern ClaireBoolean * known_ask_property(property *self,ClaireObject *x);
extern ClaireBoolean * unknown_ask_property(property *self,ClaireObject *x);
extern OID  delete_property(property *self,ClaireObject *x,OID y);
extern OID  erase_property(property *self,ClaireObject *x);
extern void  set_range_property(property *p,ClaireClass *c,ClaireType *r);
extern void  put_store_property2(property *self,ClaireObject *x,OID y,ClaireBoolean *b);
extern ClaireBoolean * multi_ask_any(OID x);
extern void  fastcall_relation2(ClaireRelation *r,OID x,OID y);
extern void  insert_definition_property(property *p,restriction *r);
extern list * initialize_restriction1(restriction *x,ClaireClass *d,list *l);
extern ClaireBoolean * uniform_restriction(restriction *x);
extern ClaireBoolean * uniform_property(property *p);
extern list * initialize_restriction2(restriction *x,list *l);
extern OID  hashinsert_restriction(restriction *m);
extern OID  hashinsert_class(ClaireClass *c,method *x);
extern OID  hashinsert_list(list *l,method *x);
extern ClaireObject * hashget_class(ClaireClass *c,property *p);
extern ClaireBoolean * join_list(list *x,list *y);
extern ClaireObject * _at_property1(property *self,ClaireClass *x);
extern ClaireObject * _at_property2(property *self,list *lt);
extern ClaireBoolean * matching_ask_list(list *l,int n,int m);
extern ClaireBoolean * vmatch_ask_any(OID t,OID x,int n);
extern ClaireBoolean * tmatch_ask_list(list *l,list *l2);
extern ClaireBoolean * tmatch_ask_any(OID t,OID t2,list *l);
extern ClaireObject * find_which_property(property *p,int n,ClaireClass *c);
extern ClaireObject * find_which_list(list *l,ClaireClass *c,int n,int m);
extern ClaireObject * find_which_class(ClaireClass *c,list *l,int n,int m);
extern OID  release_void();
extern OID  about_void();
extern list * get_args_integer(int i);
extern OID  funcall_method1(method *self,OID x);
extern OID  funcall_method2(method *self,OID x,OID y);
extern OID  funcall_method3(method *self,OID x,OID y,OID z);
extern OID  apply_function(ClaireFunction *self,list *ls,list *l);
extern OID  call_property(property *p,listargs *l);
extern OID  apply_property(property *p,list *l);
extern OID  apply_method(method *m,list *l);
extern void  push_debug_property(property *prop,int arity,int start);
extern void  pop_debug_property(property *self,int n,OID val);
extern void  tr_indent_boolean(ClaireBoolean *return_ask,int n);
extern ClaireBoolean * identified_ask_class(ClaireClass *self);
extern ClaireBoolean * identical_ask_any(OID x,OID y);
extern OID  put_property2(property *self,ClaireObject *x,OID y);
extern void  add_value_property3(property *self,ClaireObject *x,OID y);
extern OID  nth_table1(table *a,OID x);
extern ClaireType * nth_table1_type(ClaireType *a,ClaireType *x);
extern OID  get_table(table *a,OID x);
extern ClaireType * get_table_type(ClaireType *a,ClaireType *x);
extern void  nth_equal_table1(table *a,OID x,OID y);
extern void  nth_put_table(table *a,OID x,OID y);
extern void  put_table(table *a,OID x,OID y);
extern void  add_table(table *a,OID x,OID y);
extern void  add_I_table(table *a,OID x,OID y);
extern ClaireBoolean * add_value_array(table *self,int n,bag *l,OID y);
extern void  add_value_table3(table *self,OID x,OID y);
extern OID  delete_table(table *a,OID x,OID y);
extern OID  nth_table2(table *a,OID x,OID y);
extern ClaireType * nth_table2_type(ClaireType *a,ClaireType *x,ClaireType *y);
extern void  nth_equal_table2(table *a,OID x,OID y,OID z);
extern int  get_index_table1(table *a,OID x);
extern int  get_index_table2(table *a,int x,int y);
extern void  erase_table(table *a);
extern table * make_table_type(ClaireType *_Zdomain,ClaireType *_Zrange,OID _Zdefault);
extern OID  funcall_lambda1(lambda *self,OID x);
extern OID  funcall_lambda2(lambda *self,OID x,OID y);
extern OID  funcall_lambda3(lambda *self,OID x,OID y,OID z);
extern void  check_inverse_any(OID _Zr1,OID _Zr2);
extern bag * invert_relation(ClaireRelation *r,OID x);
extern ClaireClass * domain_I_restriction(restriction *x);
extern set * methods_class(ClaireClass *d,ClaireClass *r);
extern void  reify_listargs(listargs *l);
extern void  self_print_general_error_Core(general_error *self);
extern void  self_print_read_slot_error_Core(read_slot_error *self);
extern void  self_print_range_error_Core(range_error *self);
extern void  self_print_selector_error_Core(selector_error *self);
extern void  self_print_return_error_Core(return_error *self);
extern void  self_print_system_error_Core(system_error *self);
extern void  self_print_contradiction_Core(contradiction *x);
extern void  format_string(char *self,list *larg);
extern OID  tformat_string(char *self,int i,list *l);
extern void  princ_bag(bag *s);
extern global_variable * close_global_variable(global_variable *self);
extern void  contradiction_I_void();
extern void  print_in_string_void();
extern char * end_of_print_void();
extern int  buffer_length_void();
extern void  buffer_set_length_integer(int i);
extern void  apply_self_print_any(OID self);
extern void  self_print_any_Core(OID self);
extern void  self_print_boolean_Core(ClaireBoolean *self);
extern void  self_print_function_Core(ClaireFunction *self);
extern void  self_print_restriction_Core(restriction *self);
extern void  print_any(OID x);
extern ClaireBoolean * short_enough_integer(int self);
extern ClaireObject * complete_I_object(ClaireObject *self);
extern ClaireBoolean * not_any(OID self);
extern ClaireBoolean * _I_equal_any(OID self,OID x);
extern ClaireClass * owner_any(OID self);
extern ClaireBoolean * known_ask_any(OID self);
extern ClaireBoolean * unknown_ask_any(OID self);
extern OID  check_in_any(OID self,ClaireType *y);
extern bag * check_in_bag(bag *self,ClaireClass *c,ClaireType *y);
extern ClaireBoolean * _inf_any(OID self,OID x);
extern ClaireBoolean * _sup_any(OID self,OID x);
extern OID  ephemeral_class(ClaireClass *self);
extern OID  abstract_class(ClaireClass *c);
extern OID  final_class(ClaireClass *c);
extern ClaireObject * new_class1(ClaireClass *self);
extern ClaireType * new_class1_type(ClaireType *self);
extern thing * new_class2(ClaireClass *self,symbol *_Znom);
extern ClaireType * new_class2_type(ClaireType *self,ClaireType *_Znom);
extern ClaireType * new_object_class_type(ClaireType *self);
extern ClaireType * new_thing_class_type(ClaireType *self,ClaireType *_Znom);
extern ClaireClass * meet_class(ClaireClass *self,ClaireClass *ens);
extern ClaireBoolean * inherit_ask_class(ClaireClass *self,ClaireClass *ens);
extern OID  abstract_property(property *p);
extern void  final_relation(ClaireRelation *r);
extern module * close_module(module *self);
extern OID  get_symbol_string(char *self);
extern symbol * gensym_void();
extern OID  store_list4(list *l,int n,OID y);
extern OID  store_array1(OID *l,int n,OID y);
extern void  commit_integer(int n);
extern void  backtrack_integer(int n);
extern OID  store_listargs(listargs *l);
extern symbol * symbol_I_string2(char *self);
extern void  externC_string(char *s);
extern OID  externC_string2(char *s,ClaireClass *c);
extern ClaireType * externC_string2_type(ClaireType *s,ClaireType *c);
extern ClaireChar * nth_get_string(char *s,int n,int max);
extern void  nth_put_string(char *s,int n,ClaireChar *c,int max);
extern char * make_string_symbol(symbol *self);
extern void  self_print_symbol_Core(symbol *self);
extern int  _plus_integer(int self,int x);
extern ClaireType * _plus_integer_type(ClaireType *self,ClaireType *x);
extern int  _dash_integer1(int self,int x);
extern ClaireType * _dash_integer1_type(ClaireType *self,ClaireType *x);
extern int  _inf_inf_integer(int x,int y);
extern int  _sup_sup_integer(int x,int y);
extern int  and_integer(int x,int y);
extern int  or_integer(int x,int y);
extern ClaireBoolean * _inf_integer(int self,int x);
extern ClaireBoolean * _inf_equal_integer(int self,int x);
extern ClaireBoolean * _sup_integer(int self,int x);
extern ClaireBoolean * nth_integer(int self,int y);
extern ClaireBoolean * factor_ask_integer(int x,int y);
extern ClaireBoolean * divide_ask_integer(int x,int y);
extern OID  Id_any(OID x);
extern ClaireType * Id_any_type(ClaireType *x);
extern list * pair_any(OID x,OID y);
extern OID  pair_1_list(list *x);
extern ClaireType * pair_1_list_type(ClaireType *x);
extern OID  pair_2_list(list *x);
extern ClaireType * pair_2_list_type(ClaireType *x);
extern double  _plus_float(double self,double x);
extern OID  _plus_float_(OID g0073,OID g0074);
extern double  _dash_float(double self,double x);
extern OID  _dash_float_(OID g0075,OID g0076);
extern double  _star_float(double self,double x);
extern OID  _star_float_(OID g0077,OID g0078);
extern double  _7_float(double self,double x);
extern OID  _7_float_(OID g0079,OID g0080);
extern double  _dash_float2(double self);
extern OID  _dash_float2_(OID g0081);
extern double  sqrt_float(double self);
extern OID  sqrt_float_(OID g0082);
extern double  _exp_float(double self,double x);
extern OID  _exp_float_(OID g0083,OID g0084);
extern double  log_float(double self);
extern OID  log_float_(OID g0085);
extern double  atan_float(double self);
extern OID  atan_float_(OID g0086);
extern char * string_I_float(double self);
extern char * string_I_float_(OID g0087);
extern int  length_bag(bag *self);
extern OID  nth_bag(bag *self,int x);
extern ClaireType * nth_bag_type(ClaireType *self,ClaireType *x);
extern OID  nth_get_bag(bag *self,int x);
extern OID  min_method(method *f,bag *self);
extern ClaireType * min_method_type(ClaireType *f,ClaireType *self);
extern OID  max_method(method *f,bag *self);
extern ClaireType * max_method_type(ClaireType *f,ClaireType *self);
extern list * _7_plus_bag(bag *x,bag *y);
extern OID  last_list(list *self);
extern ClaireType * last_list_type(ClaireType *self);
extern list * rmlast_list(list *self);
extern OID  nth_set_list(list *self,int x,OID y);
extern OID  car_list(list *self);
extern list * hashlist_integer(int n);
extern int  hashsize_list(list *l);
extern list * sort_method(method *f,list *self);
extern void  quicksort_list(list *self,method *f,int n,int m);
extern set * build_powerset_list(list *self);
extern list * make_copy_list_integer(int n,OID d);
extern set * difference_set(set *self,set *x);
extern void  nth_equal_array(OID *self,int x,OID y);
extern void  self_print_array_Core(OID *self);
extern void  self_print_char_Core(ClaireChar *self);
extern ClaireBoolean * _inf_equal_char(ClaireChar *c1,ClaireChar *c2);
extern ClaireBoolean * finite_ask_type(ClaireType *self);
extern bag * enumerate_any(OID self);
extern ClaireBoolean * _equaltype_ask_any(ClaireType *self,ClaireType *ens);
extern ClaireClass * sort_I_type(ClaireType *x);
extern ClaireBoolean * _Z_any1(OID self,ClaireClass *ens);
extern ClaireBoolean * Ztype_any(OID x,OID y);
extern void  self_print_Union_Core(Union *self);
extern ClaireBoolean * finite_ask_Union(Union *self);
extern void  self_print_Interval_Core(Interval *self);
extern ClaireBoolean * finite_ask_Interval(Interval *self);
extern Interval * _dash_dash_integer(int x,int y);
extern void  self_print_Param_Core(Param *self);
extern ClaireBoolean * finite_ask_Param(Param *self);
extern void  self_print_subtype_Core(subtype *self);
extern ClaireBoolean * finite_ask_subtype(subtype *self);
extern ClaireType * nth_class1(ClaireClass *self,ClaireType *x);
extern ClaireType * nth_class2(ClaireClass *self,list *l1,list *l2);
extern ClaireType * param_I_class(ClaireClass *self,ClaireType *tx);
extern ClaireType * nth_type(ClaireType *self);
extern ClaireBoolean * finite_ask_tuple(tuple *self);
extern void  self_print_Reference_Core(Reference *self);
extern OID  get_Reference(Reference *self,OID y);
extern OID  _at_Reference(Reference *self,list *l,OID y);
extern set * set_I_collection(ClaireCollection *x);
extern int  size_collection(ClaireCollection *x);
extern set * set_I_set(set *x);
extern int  size_set(set *x);
extern int  size_list2_Core(list *x);
extern set * set_I_class(ClaireClass *x);
extern int  size_class(ClaireClass *self);
extern set * set_I_Union(Union *x);
extern int  size_Union(Union *x);
extern set * set_I_Interval(Interval *x);
extern int  size_Interval(Interval *self);
extern set * set_I_Param(Param *x);
extern int  size_Param(Param *x);
extern set * set_I_subtype(subtype *x);
extern int  size_subtype(subtype *x);
extern set * set_I_tuple(tuple *x);
extern int  size_tuple(tuple *l);
extern ClaireBoolean * member_ask_any(OID x,ClaireType *y);
extern ClaireClass * class_I_type(ClaireType *x);
extern ClaireType * U_type(ClaireType *x,ClaireType *y);
extern ClaireType * _dot_dot_integer(int x,int y);
extern ClaireType * _dot_dot_integer_type(ClaireType *x,ClaireType *y);
extern bag * but_any(OID s,OID x);
extern ClaireType * but_any_type(ClaireType *s,ClaireType *x);
extern set * _backslash_type(ClaireType *x,ClaireType *y);
extern set * glb_set(set *x,ClaireType *y);
extern ClaireType * glb_Union(Union *x,ClaireType *y);
extern ClaireType * glb_Interval(Interval *x,ClaireType *y);
extern ClaireType * glb_class(ClaireClass *x,ClaireType *y);
extern ClaireType * glb_Param(Param *x,ClaireType *y);
extern ClaireType * glb_subtype(subtype *x,ClaireType *y);
extern ClaireType * glb_tuple(tuple *x,ClaireType *y);
extern ClaireType * glb_Reference(Reference *x,ClaireType *y);
extern ClaireType * _exp_type(ClaireType *x,ClaireType *y);
extern ClaireType * join_class(ClaireClass *x,ClaireClass *y);
extern list * _exp_list(list *x,list *y);
extern ClaireType * Uall_list(list *l);
extern ClaireBoolean * _inf_equalt_bag2(bag *s,ClaireType *y);
extern ClaireBoolean * _inf_equalt_class(ClaireClass *x,ClaireType *y);
extern ClaireBoolean * _inf_equalt_Union(Union *x,ClaireType *y);
extern ClaireBoolean * _inf_equalt_Interval(Interval *x,ClaireType *y);
extern ClaireBoolean * _inf_equalt_subtype(subtype *x,ClaireType *y);
extern ClaireBoolean * _inf_equalt_Param(Param *x,ClaireType *y);
extern ClaireBoolean * _inf_equalt_Reference(Reference *x,ClaireType *y);
extern ClaireBoolean * _inf_equalt_tuple(tuple *x,ClaireType *y);
extern ClaireBoolean * _inf_equalt_type(ClaireType *x,ClaireType *y);
extern ClaireBoolean * _inf_equal_type(ClaireType *x,ClaireType *y);
extern ClaireType * member_type(ClaireType *x);
extern ClaireType * of_extract_type(ClaireType *x);
extern ClaireType * _at_type(ClaireType *x,property *p);
extern ClaireBoolean * unique_ask_type(ClaireType *x);
extern OID  the_type(ClaireType *x);
extern int  integer_I_set(set *s);
extern set * make_set_integer(int x);
extern ClaireType * abstract_type_set(set *xt1);
extern ClaireType * abstract_type_operation(operation *p,ClaireType *xt1,ClaireType *xt2);
extern ClaireType * first_arg_type_type(ClaireType *x,ClaireType *y);
extern ClaireType * first_arg_type_type2(ClaireType *x,ClaireType *y,ClaireType *z);
extern ClaireType * second_arg_type_type(ClaireType *x,ClaireType *y);
extern ClaireType * meet_arg_types_type(ClaireType *x,ClaireType *y);
extern ClaireType * first_member_type_type(ClaireType *x,ClaireType *y);
extern OID  nth_array(OID *self,int x);
extern ClaireType * nth_array_type(ClaireType *self,ClaireType *x);
extern ClaireType * make_array_integer_type(ClaireType *i,ClaireType *t,ClaireType *v);
extern list * make_list_integer2(int n,ClaireType *t,OID x);
extern ClaireType * make_list_integer2_type(ClaireType *n,ClaireType *t,ClaireType *x);
extern set * make_set_array(OID *self);
extern ClaireType * make_set_array_type(ClaireType *self);
extern ClaireType * list_I_array_type(ClaireType *a);
extern ClaireType * array_I_list_type(ClaireType *a);
extern ClaireType * set_I_bag_type(ClaireType *l);
extern ClaireType * list_I_set_type(ClaireType *l);

// namespace class for Core 
class CoreClass: public NameSpace {
public:

property * vars;
property * dimension;
property * version;
ClaireClass * _ephemeral_object;
ClaireClass * _lambda;
property * execute;
property * debug;
property * eval_message;
property * noeval_message;
property * eval;
property * call;
property * self_eval;
property * read;
property * inlineok_ask;
property * restore_state;
property * hold_ask;
property * write;
property * range_is_wrong;
property * update_plus;
property * update_dash;
property * add_value;
property * known_ask;
property * unknown_ask;
property * erase;
property * set_range;
property * put_store;
property * matching_ask;
property * vmatch_ask;
property * tmatch_ask;
property * find_which;
property * main;
property * multi_ask;
operation * join;
table * StopProperty;
property * pname;
property * reify;
property * _star_stararg;
property * args;
property * value;
ClaireClass * _general_error;
ClaireClass * _read_slot_error;
ClaireClass * _range_error;
ClaireClass * _selector_error;
ClaireClass * _return_error;
ClaireClass * _contradiction;
ClaireClass * _global_variable;
operation * _inf_equal2;
global_variable * contradiction_occurs;
global_variable * nil;
global_variable * claire_date;
operation * _I_equal;
operation * _inf_inf;
operation * _sup_sup;
operation * ClaireAnd;
operation * ClaireOr;
operation * U;
operation * less_ask;
operation * _and;
operation * min;
operation * max;
operation * meet;
operation * inherit_ask;
property * cpstack;
ClaireClass * _pretty_printer;
pretty_printer * pretty;
property * apply_self_print;
property * short_enough;
property * print;
ClaireClass * _much_too_far;
property * kill_I;
global_variable * world_plus;
global_variable * world_dash;
global_variable * world_dash_I;
operation * pair;
property * arg1;
property * arg2;
operation * _dash_dash;
operation * _equaltype_ask;
ClaireClass * _Type;
ClaireClass * _Union;
ClaireClass * _Interval;
ClaireClass * _Param;
ClaireClass * _subtype;
ClaireClass * _Reference;
operation * but;
operation * _backslash;
operation * glb;
operation * _inf_equalt;
property * class_I;// claire/"class!"
property * owner;// claire/"owner"
property * check_in;// claire/"check_in"
property * initialize;// Core/"initialize"
property * uniform;// Core/"uniform"
property * hashinsert;// Core/"hashinsert"
property * hashget;// Core/"hashget"
property * param_I;// Core/"param!"
property * size;// claire/"size"
property * end_of_string;// claire/"end_of_string"
property * apply;// claire/"apply"
property * finite_ask;// claire/"finite?"
property * call_step;// Core/"call_step"
property * spy;// claire/"spy"
property * release;// claire/"release"
operation * _at;// claire/"@"
property * about;// claire/"about"
property * get_args;// mClaire/"get_args"
property * push_debug;// Core/"push_debug"
property * pop_debug;// Core/"pop_debug"
property * tr_indent;// Core/"tr_indent"
operation * _plus;// claire/"+"
property * identified_ask;// Core/"identified?"
property * identical_ask;// claire/"identical?"
property * get_index;// Core/"get_index"
property * factor_ask;// claire/"factor?"
property * divide_ask;// claire/"divide?"
property * Id;// claire/"Id"
property * pair_1;// claire/"pair_1"
property * pair_2;// claire/"pair_2"
property * check_inverse;// Core/"check_inverse"
property * invert;// claire/"invert"
property * domain_I;// claire/"domain!"
property * methods;// claire/"methods"
property * cause;// mClaire/"cause"
property * wrong;// Core/"wrong"
property * format;// claire/"format"
property * tformat;// Core/"tformat"
property * contradiction_I;// claire/"contradiction!"
property * get_stack;// mClaire/"get_stack"
property * put_stack;// mClaire/"put_stack"
property * push_I;// mClaire/"push!"
property * gc;// claire/"gc"
property * time_get;// claire/"time_get"
property * time_set;// claire/"time_set"
property * time_show;// claire/"time_show"
property * print_in_string;// claire/"print_in_string"
property * buffer_length;// mClaire/"buffer_length"
property * buffer_set_length;// mClaire/"buffer_set_length"
property * NOT;// claire/"not"
property * make_function;// claire/"make_function"
property * externC;// claire/"externC"
property * shell;// claire/"shell"
property * getenv;// claire/"getenv"
property * _dash_dash_ask;// Core/"--?"
property * exit;// claire/"exit"
property * last;// claire/"last"
property * rmlast;// claire/"rmlast"
property * car;// claire/"car"
property * hashlist;// Core/"hashlist"
property * hashsize;// Core/"hashsize"
property * sort;// claire/"sort"
property * quicksort;// Core/"quicksort"
property * build_powerset;// Core/"build_powerset"
property * difference;// claire/"difference"
property * of_extract;// Core/"of_extract"
property * member;// claire/"member"
property * Address;// Core/"Address"
property * Oid;// Core/"Oid"
property * Oid_tilda;// Core/"Oid~"
property * get_value;// claire/"get_value"
property * enumerate;// Core/"enumerate"
property * t1;// mClaire/"t1"
property * t2;// mClaire/"t2"
property * tuple_I;// claire/"tuple!"
property * Uall;// claire/"Uall"
property * unique_ask;// claire/"unique?"
property * the;// claire/"the"
property * abstract_type;// Core/"abstract_type"
property * NEW;// claire/"new"
property * sqrt;// claire/"sqrt"
property * insert_definition;// Core/"insert_definition"
property * make_array;// claire/"make_array"
property * cpretty;// mClaire/"cpretty"
property * cprevious;// mClaire/"cprevious"
property * width;// mClaire/"width"
property * pprint;// mClaire/"pprint"
property * pbreak;// mClaire/"pbreak"
property * base_I;// mClaire/"base!"
property * set_base;// mClaire/"set_base"
property * index_I;// mClaire/"index!"
property * set_index;// mClaire/"set_index"
property * complete_I;// mClaire/"complete!"
property * _Ztype;// mClaire/"%type"
property * update;// mClaire/"update"
property * make_set;// claire/"make_set"
property * get_symbol;// claire/"get_symbol"
property * time_read;// claire/"time_read"
property * first_arg_type;// Core/"first_arg_type"
property * second_arg_type;// Core/"second_arg_type"
property * meet_arg_types;// Core/"meet_arg_types"
property * make_copy_list;// claire/"make_copy_list"
property * log;// claire/"log"
property * new_I;// mClaire/"new!"
property * atan;// mClaire/"atan"
property * make_table;// claire/"make_table"
property * first_member_type;// Core/"first_member_type"

// module definition 
 void metaLoad();};

extern CoreClass Core;
extern NameSpace mClaire;

#endif
