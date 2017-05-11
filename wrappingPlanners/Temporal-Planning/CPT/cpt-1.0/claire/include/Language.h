// interface defination for module Language, Sat Oct 16 06:53:29 2004
#ifndef CLAIREH_Language
#define CLAIREH_Language


class Instruction;
class Basic_instruction;
class Variable;
class Vardef;
class Complex_instruction;
class Instruction_with_var;
class Control_structure;
class Call;
class Call_star;
class Call_plus;
class Assign;
class Gassign;
class And;
class Or;
class Quote;
class Optimized_instruction;
class Call_method;
class Call_method1;
class Call_method2;
class Call_slot;
class Call_array;
class Call_table;
class Update;
class Super;
class Cast;
class Return;
class If;
class Do;
class Let;
class When;
class Let_plus;
class Let_star;
class Iteration;
class For;
class Collect;
class Image;
class Select;
class Lselect;
class Exists;
class Case;
class While;
class ClaireHandle;
class Construct;
class List;
class Tuple;
class Set;
class Array;
class Printf;
class Error;
class Branch;
class Macro;
class Trace;
class Assert;
class Defclaire;
class Definition;
class Defobj;
class Defclass;
class Defmethod;
class Defarray;
class Defrule;
class Defvar;
class Language_demon;
class Language_rule_object;

class Instruction: public SystemObject{ 
  public:} 
;

class Basic_instruction: public Instruction{ 
  public:} 
;

class Variable: public Basic_instruction{ 
  public:
     symbol *pname;
     ClaireType *range;
     int index;} 
;

class Vardef: public Variable{ 
  public:} 
;

class Complex_instruction: public Instruction{ 
  public:} 
;

class Instruction_with_var: public Complex_instruction{ 
  public:
     Variable *var;} 
;

class Control_structure: public Complex_instruction{ 
  public:} 
;

class Call: public Control_structure{ 
  public:
     property *selector;
     list *args;} 
;

class Call_star: public Call{ 
  public:} 
;

class Call_plus: public Call{ 
  public:} 
;

class Assign: public Basic_instruction{ 
  public:
     OID var;
     OID arg;} 
;

class Gassign: public Basic_instruction{ 
  public:
     global_variable *var;
     OID arg;} 
;

class And: public Control_structure{ 
  public:
     list *args;} 
;

class Or: public Control_structure{ 
  public:
     list *args;} 
;

class Quote: public Basic_instruction{ 
  public:
     OID arg;} 
;

class Optimized_instruction: public Complex_instruction{ 
  public:} 
;

class Call_method: public Optimized_instruction{ 
  public:
     method *arg;
     list *args;} 
;

class Call_method1: public Call_method{ 
  public:} 
;

class Call_method2: public Call_method{ 
  public:} 
;

class Call_slot: public Optimized_instruction{ 
  public:
     slot *selector;
     OID arg;
     ClaireBoolean *test;} 
;

class Call_array: public Optimized_instruction{ 
  public:
     OID selector;
     OID arg;
     OID test;} 
;

class Call_table: public Optimized_instruction{ 
  public:
     table *selector;
     OID arg;
     ClaireBoolean *test;} 
;

class Update: public Optimized_instruction{ 
  public:
     OID selector;
     OID arg;
     OID value;
     OID var;} 
;

class Super: public Control_structure{ 
  public:
     property *selector;
     ClaireType *cast_to;
     list *args;} 
;

class Cast: public Basic_instruction{ 
  public:
     OID arg;
     ClaireType *set_arg;} 
;

class Return: public Basic_instruction{ 
  public:
     OID arg;} 
;

class If: public Control_structure{ 
  public:
     OID test;
     OID arg;
     OID other;} 
;

class Do: public Control_structure{ 
  public:
     list *args;} 
;

class Let: public Instruction_with_var{ 
  public:
     OID value;
     OID arg;} 
;

class When: public Let{ 
  public:
     OID other;} 
;

class Let_plus: public Let{ 
  public:} 
;

class Let_star: public Let{ 
  public:} 
;

class Iteration: public Instruction_with_var{ 
  public:
     OID set_arg;
     OID arg;} 
;

class For: public Iteration{ 
  public:} 
;

class Collect: public Iteration{ 
  public:
     ClaireType *of;} 
;

class Image: public Iteration{ 
  public:
     ClaireType *of;} 
;

class Select: public Iteration{ 
  public:
     ClaireType *of;} 
;

class Lselect: public Iteration{ 
  public:
     ClaireType *of;} 
;

class Exists: public Iteration{ 
  public:
     OID other;} 
;

class Case: public Control_structure{ 
  public:
     OID var;
     list *args;} 
;

class While: public Control_structure{ 
  public:
     OID test;
     OID arg;
     ClaireBoolean *other;} 
;

class ClaireHandle: public Control_structure{ 
  public:
     OID test;
     OID arg;
     OID other;} 
;

class Construct: public Complex_instruction{ 
  public:
     list *args;} 
;

class List: public Construct{ 
  public:
     ClaireType *of;} 
;

class Tuple: public Construct{ 
  public:} 
;

class Set: public Construct{ 
  public:
     ClaireType *of;} 
;

class Array: public Construct{ 
  public:
     ClaireType *of;} 
;

class Printf: public Construct{ 
  public:} 
;

class Error: public Construct{ 
  public:} 
;

class Branch: public Construct{ 
  public:} 
;

class Macro: public Construct{ 
  public:} 
;

class Trace: public Construct{ 
  public:} 
;

class Assert: public Construct{ 
  public:
     int index;
     char *external;} 
;

class Defclaire: public Complex_instruction{ 
  public:} 
;

class Definition: public Defclaire{ 
  public:
     ClaireClass *arg;
     list *args;} 
;

class Defobj: public Definition{ 
  public:
     symbol *ident;} 
;

class Defclass: public Defobj{ 
  public:
     list *params;
     ClaireBoolean *forward_ask;} 
;

class Defmethod: public Defclaire{ 
  public:
     Call *arg;
     OID set_arg;
     OID body;
     OID inline_ask;} 
;

class Defarray: public Defmethod{ 
  public:} 
;

class Defrule: public Defclaire{ 
  public:
     symbol *ident;
     list *args;
     OID arg;
     OID body;} 
;

class Defvar: public Defclaire{ 
  public:
     Variable *ident;
     OID arg;} 
;

class Language_demon: public lambda{ 
  public:
     symbol *pname;
     int priority;
     lambda *formula;} 
;

class Language_rule_object: public property{ 
  public:} 
;
extern OID  no_eval_Instruction(Instruction *self);
extern void  self_print_unbound_symbol_Language(unbound_symbol *self);
extern OID  self_eval_unbound_symbol(unbound_symbol *self);
extern void  self_print_Variable_Language(Variable *self);
extern void  ppvariable_Variable(Variable *self);
extern void  ppvariable_list(list *self);
extern OID  self_eval_Variable(Variable *self);
extern OID  write_value_Variable(Variable *self,OID val);
extern OID  self_eval_Vardef(Vardef *self);
extern OID  self_eval_global_variable(global_variable *self);
extern OID  write_value_global_variable(global_variable *self,OID val);
extern OID  apply_lambda(lambda *self,list *_Zl);
extern OID  call_lambda2(lambda *self,listargs *l);
extern OID  self_print_lambda_Language(lambda *self);
extern lambda * lambda_I_list(list *lvar,OID self);
extern OID  lexical_build_any(OID self,list *lvar,int n);
extern OID  lexical_change_any(OID self,list *lvar);
extern ClaireClass * close_class(ClaireClass *self);
extern symbol * extract_symbol_any(OID self);
extern property * make_a_property_any(OID self);
extern OID  lbreak_void();
extern OID  put_buffer_void();
extern OID  checkfar_void();
extern OID  lbreak_integer(int n);
extern OID  indent_integer(int limit);
extern void  set_level_void();
extern void  set_level_integer(int n);
extern OID  printbox_bag1(bag *self,int start,int finish,char *s);
extern OID  printbox_bag2(bag *self);
extern OID  printbox_bag3(bag *self,char *s);
extern void  printl_bag(bag *self,char *s);
extern void  printexp_any(OID self,ClaireBoolean *comp);
extern void  pretty_print_any(OID self);
extern void  self_print_list_Language(list *self);
extern void  self_print_set_Language(set *self);
extern void  self_print_tuple_Language(tuple *self);
extern void  self_print_Call_Language(Call *self);
extern OID  self_print_Call_plus_Language(Call_plus *self);
extern OID  self_eval_Call(Call *self);
extern OID  self_eval_Call_plus(Call_plus *self);
extern void  printe_any(OID self,property *s);
extern ClaireBoolean * sugar_ask_any(OID x,OID x2,OID o,OID a);
extern void  self_print_Assign_Language(Assign *self);
extern OID  self_eval_Assign(Assign *self);
extern void  self_print_Gassign_Language(Gassign *self);
extern OID  self_eval_Gassign(Gassign *self);
extern void  self_print_And_Language(And *self);
extern OID  self_eval_And(And *self);
extern void  self_print_Or_Language(Or *self);
extern OID  self_eval_Or(Or *self);
extern void  self_print_Quote_Language(Quote *self);
extern OID  self_eval_Quote(Quote *self);
extern void  self_print_Call_method_Language(Call_method *self);
extern OID  self_eval_Call_method(Call_method *self);
extern OID  self_eval_Call_method1(Call_method1 *self);
extern OID  self_eval_Call_method2(Call_method2 *self);
extern void  self_print_Call_slot_Language(Call_slot *self);
extern OID  self_eval_Call_slot(Call_slot *self);
extern void  self_print_Call_array_Language(Call_array *self);
extern OID  self_eval_Call_array(Call_array *self);
extern void  self_print_Call_table_Language(Call_table *self);
extern OID  self_eval_Call_table(Call_table *self);
extern void  self_print_Update_Language(Update *self);
extern OID  self_eval_Update(Update *self);
extern void  self_print_Super_Language(Super *self);
extern OID  self_eval_Super(Super *self);
extern void  self_print_Cast_Language(Cast *x);
extern OID  self_eval_Cast(Cast *self);
extern void  self_print_Return_Language(Return *self);
extern OID  self_eval_Return(Return *self);
extern OID  substitution_any(OID self,Variable *x,OID val);
extern int  occurrence_any(OID self,Variable *x);
extern OID  instruction_copy_any(OID self);
extern void  self_print_If_Language(If *self);
extern void  printstat_If(If *self);
extern void  printif_any(OID self);
extern void  printelse_If(If *self);
extern OID  self_eval_If(If *self);
extern void  self_print_Do_Language(Do *self);
extern void  printdo_list(list *l,ClaireBoolean *clo);
extern void  printblock_any(OID x);
extern OID  self_eval_Do(Do *self);
extern void  self_print_Let_Language(Let *self);
extern void  printbody_Let(Let *self);
extern OID  self_eval_Let(Let *self);
extern void  self_print_When_Language(When *self);
extern OID  self_eval_When(When *self);
extern void  self_print_Let_plus_Language(Let_plus *self);
extern void  self_print_Let_star_Language(Let_star *self);
extern void  self_print_For_Language(For *self);
extern OID  self_eval_For(For *self);
extern void  self_print_Collect_Language(Collect *self);
extern OID  self_eval_Collect(Collect *self);
extern void  self_print_Image_Language(Image *self);
extern OID  self_eval_Image(Image *self);
extern void  self_print_Select_Language(Select *self);
extern OID  self_eval_Select(Select *self);
extern void  self_print_Lselect_Language(Lselect *self);
extern OID  self_eval_Lselect(Lselect *self);
extern void  self_print_Exists_Language(Exists *self);
extern OID  self_eval_Exists(Exists *self);
extern void  self_print_Case_Language(Case *self);
extern OID  self_eval_Case(Case *self);
extern void  self_print_While_Language(While *self);
extern OID  self_eval_While(While *self);
extern void  self_print_Handle_Language(ClaireHandle *self);
extern OID  self_eval_Handle(ClaireHandle *self);
extern void  self_print_Construct_Language(Construct *self);
extern OID  self_eval_List(List *self);
extern OID  self_eval_Set(Set *self);
extern OID  self_eval_Tuple(Tuple *self);
extern OID  self_eval_Array2(Array *self);
extern OID  self_eval_Macro2(Macro *self);
extern void  self_eval_Error(Error *self);
extern OID  self_eval_Printf(Printf *self);
extern OID  self_eval_Trace(Trace *self);
extern OID  self_eval_Assert(Assert *self);
extern OID  self_eval_Branch(Branch *self);
extern OID  self_print_Definition_Language(Definition *self);
extern void  self_print_Defobj_Language(Defobj *self);
extern void  self_print_Defclass_Language(Defclass *self);
extern void  self_print_Defmethod_Language(Defmethod *self);
extern void  self_print_Defarray_Language(Defarray *self);
extern void  self_print_Defrule_Language(Defrule *self);
extern void  self_print_Defvar_Language(Defvar *self);
extern OID  self_eval_Definition(Definition *self);
extern OID  complete_object(ClaireObject *self,list *_Zl);
extern OID  self_eval_Defobj(Defobj *self);
extern OID  self_eval_Defclass(Defclass *self);
extern OID  self_eval_Defmethod(Defmethod *self);
extern void  attach_comment_any(OID x);
extern list * extract_signature_list(list *l);
extern OID  extract_pattern_any(OID x,list *path);
extern ClaireType * extract_type_any(OID x);
extern OID  extract_item_any(OID x,OID y);
extern OID  extract_pattern_nth_list(list *l,list *path);
extern ClaireObject * extract_class_call_class(ClaireClass *self,list *l);
extern list * extract_range_any(OID x,list *lvar,list *ldef);
extern int  bit_vector_listargs2(listargs *l);
extern list * extract_status_any(OID x);
extern ClaireType * type_I_any(OID x);
extern OID  self_eval_Defarray(Defarray *self);
extern void  self_print_demon(Language_demon *self);
extern OID  funcall_demon1(Language_demon *self,OID x,OID y);
extern OID  funcall_demon2(Language_demon *self,OID x,OID y,OID z);
extern OID  self_eval_Defrule(Defrule *self);
extern ClaireBoolean * eventMethod_ask_relation2(ClaireRelation *r);
extern tuple * make_filter_any(OID cond);
extern tuple * make_filter_any_(OID g0107);
extern Language_demon * make_demon_relation(ClaireRelation *R,symbol *n,list *lvar,OID cond,OID conc);
extern Call * readCall_relation(ClaireRelation *R,OID x);
extern Call * putCall_relation2(ClaireRelation *R,OID x,OID y);
extern ClaireType * safeRange_relation(ClaireRelation *x);
extern void  eval_if_write_relation(ClaireRelation *R);
extern void  eventMethod_property(property *p);
extern void  interface_property(property *p);
extern void  interface_class(ClaireClass *c,listargs *l);

// namespace class for Language 
class LanguageClass: public NameSpace {
public:

ClaireClass * _Instruction;
ClaireClass * _Basic_instruction;
global_variable * typing;
global_variable * index;
ClaireClass * _Variable;
ClaireClass * _Vardef;
ClaireClass * _Complex_instruction;
ClaireClass * _Instruction_with_var;
ClaireClass * _Control_structure;
global_variable * _eof;
global_variable * EOS;
global_variable * MAX_INTEGER;
global_variable * _starvariable_index_star;
property * printl;
global_variable * LastCall;
ClaireClass * _Call;
ClaireClass * _Call_star;
ClaireClass * _Call_plus;
ClaireClass * _Assign;
ClaireClass * _Gassign;
ClaireClass * _And;
ClaireClass * _Or;
ClaireClass * _Quote;
ClaireClass * _Optimized_instruction;
ClaireClass * _Call_method;
ClaireClass * _Call_method1;
ClaireClass * _Call_method2;
ClaireClass * _Call_slot;
ClaireClass * _Call_array;
ClaireClass * _Call_table;
ClaireClass * _Update;
ClaireClass * _Super;
ClaireClass * _Cast;
ClaireClass * _Return;
ClaireClass * _If;
ClaireClass * _Do;
ClaireClass * _Let;
ClaireClass * _When;
ClaireClass * _Let_plus;
ClaireClass * _Let_star;
ClaireClass * _Iteration;
property * iterate;
property * Iterate;
ClaireClass * _For;
ClaireClass * _Collect;
ClaireClass * _Image;
ClaireClass * _Select;
ClaireClass * _Lselect;
ClaireClass * _Exists;
ClaireClass * _Case;
ClaireClass * _While;
ClaireClass * _Handle;
ClaireClass * _Construct;
ClaireClass * _List;
ClaireClass * _Tuple;
ClaireClass * _Set;
ClaireClass * _Array;
ClaireClass * _Printf;
ClaireClass * _Error;
ClaireClass * _Branch;
ClaireClass * _Macro;
property * macroexpand;
ClaireClass * _Trace;
ClaireClass * _Assert;
property * extract_item;
property * function_I;
global_variable * LastComment;
global_variable * NeedComment;
ClaireClass * _Defclaire;
ClaireClass * _Definition;
ClaireClass * _Defobj;
ClaireClass * _Defclass;
ClaireClass * _Defmethod;
ClaireClass * _Defarray;
ClaireClass * _Defrule;
ClaireClass * _Defvar;
property * complete;
global_variable * LDEF;
global_variable * _NEW_ALLOC;
global_variable * _BAG_UPDATE;
global_variable * _SLOT_UPDATE;
global_variable * _RETURN_ARG;
global_variable * _SAFE_RESULT;
global_variable * _SAFE_GC;
property * bit_vector;
ClaireClass * _demon;
table * demons;
operation * _inf_dash;
ClaireClass * _rule_object;
table * relations;
table * last_rule;
property * eval_rule;
table * InterfaceList;
property * ClaireInterface;
property * no_eval;// Language/"no_eval"
property * ppvariable;// Language/"ppvariable"
property * write_value;// Language/"write_value"
property * var;// claire/"var"
property * lambda_I;// iClaire/"lambda!"
property * lexical_build;// iClaire/"lexical_build"
property * lexical_change;// iClaire/"lexical_change"
property * extract_symbol;// iClaire/"extract_symbol"
property * make_a_property;// iClaire/"make_a_property"
property * lbreak;// Language/"lbreak"
property * put_buffer;// Language/"put_buffer"
property * checkfar;// Language/"checkfar"
property * indent;// Language/"indent"
property * set_level;// Language/"set_level"
property * printbox;// Language/"printbox"
property * printexp;// Language/"printexp"
property * pretty_print;// claire/"pretty_print"
property * assign;// Language/"assign"
property * printe;// Language/"printe"
property * sugar_ask;// Language/"sugar?"
property * cast_to;// iClaire/"cast_to"
property * set_arg;// iClaire/"set_arg"
property * substitution;// claire/"substitution"
property * occurrence;// Language/"occurrence"
property * instruction_copy;// Language/"instruction_copy"
property * other;// iClaire/"other"
property * test;// iClaire/"test"
property * printstat;// Language/"printstat"
property * printif;// Language/"printif"
property * printelse;// Language/"printelse"
property * printdo;// Language/"printdo"
property * printblock;// Language/"printblock"
property * printbody;// Language/"printbody"
property * ident;// iClaire/"ident"
property * attach_comment;// Language/"attach_comment"
property * extract_signature;// iClaire/"extract_signature"
property * extract_pattern;// iClaire/"extract_pattern"
property * extract_type;// iClaire/"extract_type"
property * extract_pattern_nth;// Language/"extract_pattern_nth"
property * extract_class_call;// iClaire/"extract_class_call"
property * extract_range;// iClaire/"extract_range"
property * extract_status;// iClaire/"extract_status"
property * type_I;// iClaire/"type!"
property * forward_ask;// iClaire/"forward?"
property * pname;// Language/"pname"
property * priority;// Language/"priority"
property * make_filter;// Language/"make_filter"
property * make_demon;// Language/"make_demon"
property * eval_if_write;// Language/"eval_if_write"
property * readCall;// Language/"readCall"
property * eventMethod;// Language/"eventMethod"
property * eventMethod_ask;// Language/"eventMethod?"
property * putCall;// Language/"putCall"
property * safeRange;// Language/"safeRange"

// module definition 
 void metaLoad();};

extern LanguageClass Language;
extern NameSpace iClaire;

#endif
