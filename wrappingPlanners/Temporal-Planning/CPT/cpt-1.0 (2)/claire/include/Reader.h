// interface defination for module Reader, Sat Oct 16 06:53:32 2004
#ifndef CLAIREH_Reader
#define CLAIREH_Reader


class delimiter;
class reserved_keyword;
class meta_reader;
class PRcount;

class delimiter: public global_variable{ 
  public:} 
;

class reserved_keyword: public keyword{ 
  public:} 
;

class meta_reader: public thing{ 
  public:
     char *source;
     int s_index;
     ClairePort *fromp;
     int nb_line;
     char *external;
     int index;
     OID last_form;
     int maxstack;
     ClaireBoolean *toplevel;
     int eof;
     int space;
     int tab;
     OID bracket;
     OID paren;
     OID comma;
     OID curly;
     ClaireBoolean *last_arrow;
     set *s_properties;} 
;

class PRcount: public ClaireObject{ 
  public:
     int rtime;
     int rdepth;
     int rnum;
     int rloop;
     int rstart;} 
;
extern ClaireBoolean * keyword_ask_any(OID x);
extern int  next_meta_reader(meta_reader *r);
extern int  firstc_meta_reader(meta_reader *r);
extern OID  stop_ask_integer(int n);
extern OID  nextunit_meta_reader(meta_reader *r);
extern OID  nexts_meta_reader(meta_reader *r,keyword *e);
extern OID  loopexp_meta_reader(meta_reader *r,OID x,keyword *e,ClaireBoolean *loop);
extern OID  extended_operator_property(property *p,OID x,OID y);
extern OID  nexte_meta_reader(meta_reader *r);
extern OID  nextexp_meta_reader(meta_reader *r,ClaireBoolean *str);
extern OID  nexti_meta_reader(meta_reader *r,OID val);
extern OID  read_escape_meta_reader(meta_reader *r);
extern OID  nextvariable_meta_reader(meta_reader *r,OID val);
extern OID  nexts_I_meta_reader1(meta_reader *r,keyword *e);
extern OID  nexte_I_meta_reader(meta_reader *r,keyword *e);
extern OID  nexts_I_meta_reader2(meta_reader *r,int e);
extern OID  nexts_I_meta_reader3(meta_reader *r,keyword *e,int n);
extern ClaireBoolean * extended_comment_ask_meta_reader(meta_reader *r,char *s);
extern OID  extended_comment_I_meta_reader(meta_reader *r,char *s);
extern Call * DBregister_Call(Call *c);
extern Call * Call_I_property(property *p,list *l);
extern ClaireBoolean * operation_ask_any(OID y);
extern OID  combine_any(OID x,OID y,OID z);
extern OID  combine_I_any(OID x,OID y,OID z);
extern OID  operation_I_any(OID x);
extern OID  operand_I_any(OID x,int n);
extern int  precedence_I_any(OID y);
extern OID  nextstruct_meta_reader(meta_reader *r,keyword *_Zfirst,keyword *e);
extern OID  readlet_meta_reader(meta_reader *r,keyword *e);
extern OID  readlet_star_meta_reader(meta_reader *r,list *l,int n,keyword *e);
extern OID  readwhen_meta_reader(meta_reader *r,keyword *e);
extern OID  readif_meta_reader(meta_reader *r,int e);
extern OID  readcase_meta_reader(meta_reader *r,keyword *e);
extern OID  readset_meta_reader(meta_reader *r,OID _Za1);
extern OID  dereference_any(OID x);
extern OID  nextseq_meta_reader(meta_reader *r,int e);
extern OID  readblock_meta_reader(meta_reader *r,OID x,int e);
extern OID  Do_I_any(OID x,OID y);
extern ClaireType * extract_of_type_Call(Call *x);
extern OID  readcall_meta_reader(meta_reader *r,OID x,OID t);
extern OID  nextdefinition_meta_reader(meta_reader *r,OID x,OID y,ClaireBoolean *old_ask);
extern OID  nextmethod_meta_reader(meta_reader *r,OID x,OID y,ClaireBoolean *table_ask,ClaireBoolean *old_ask,ClaireBoolean *inl_ask);
extern OID  nextinst_meta_reader(meta_reader *r,OID x);
extern Defclass * nextDefclass_meta_reader(meta_reader *r,OID x,ClaireBoolean *old_ask);
extern OID  self_eval_delimiter(delimiter *self);
extern ClaireBoolean * useless_c_integer(int r);
extern OID  skipc_meta_reader(meta_reader *self);
extern OID  skipc_I_meta_reader(meta_reader *r);
extern meta_reader * cnext_meta_reader(meta_reader *self);
extern ClaireBoolean * findeol_meta_reader(meta_reader *self);
extern OID  checkno_meta_reader(meta_reader *r,int n,OID y);
extern OID  verify_any(OID t,OID x,OID y);
extern void  Serror_string(char *s,list *la);
extern Variable * extract_variable_any(OID self);
extern list * bind_I_meta_reader(meta_reader *self,Variable *_Zv);
extern OID  unbind_I_meta_reader(meta_reader *self,list *_Zfirst);
extern char * _7_string(char *s,char *s2);
extern void  restore_state_meta_reader(meta_reader *self);
extern OID  load_file_string(char *self,ClaireBoolean *b);
extern OID  load_string(char *self);
extern OID  sload_string(char *self);
extern void  load_file_module(module *self,ClaireBoolean *b);
extern OID  load_module(module *self);
extern OID  sload_module(module *self);
extern list * add_modules_module(module *self,set *l,list *result);
extern list * add_modules_list(list *self);
extern OID  eload_string(char *self);
extern OID  readblock_port(ClairePort *p);
extern OID  read_port(ClairePort *p);
extern OID  read_string(char *self);
extern OID  debug_if_possible_void();
extern OID  print_exception_void();
extern OID  show_any(OID self);
extern OID  kill_object(ClaireObject *self);
extern OID  kill_class(ClaireClass *self);
extern int  min_integer(int x,int y);
extern int  max_integer(int x,int y);
extern double  min_float(double x,double y);
extern OID  min_float_(OID g0082,OID g0083);
extern double  max_float(double x,double y);
extern OID  max_float_(OID g0084,OID g0085);
extern OID  min_any(OID x,OID y);
extern OID  max_any(OID x,OID y);
extern list * hashgrow_list(list *l,property *hi);
extern ClaireBoolean * known_ask_table(table *a,OID x);
extern ClaireBoolean * unknown_ask_table(table *a,OID x);
extern double  float_I_string(char *self);
extern OID  float_I_string_(char *g0086);
extern ClaireBoolean * _sup_equal_any(OID self,OID x);
extern OID  inspect_any(OID self);
extern OID  inspect_loop_any(OID _Zread,list *old);
extern OID  get_from_integer_any(OID self,int n);
extern OID  trace_on_any(OID self);
extern OID  untrace_any(OID self);
extern void  spy_listargs2_Reader(listargs *l);
extern OID  self_trace_Trace(Trace *self);
extern void  trace_rule_relation(ClaireRelation *R,char *s,OID x,OID y,OID u,OID v);
extern OID  stop_property(property *p,listargs *l);
extern void  debug_void();
extern OID  call_debug_void();
extern void  breakpoint_void();
extern void  dn_integer(int x);
extern void  up_integer(int x);
extern void  where_integer(int x);
extern void  print_debug_info_integer(int index,int stack_level,int cur_index);
extern OID  Show_integer(int n);
extern void  block_integer(int x);
extern list * closure_build_lambda(lambda *self);
extern void  closure_build_any(OID self,list *lvar);
extern void  call_step_property_Reader(property *pr);
extern void  step_any(OID x);
extern int  mem_class(ClaireClass *c);
extern PRcount * PRget_property(property *p);
extern OID  PRlook_property2(property *p);
extern void  PRshow_property(property *p);
extern int  PRtime_property(property *p);
extern int  PRcounter_property(property *p);
extern void  PRshow_void();
extern set * dependents_method(method *self);
extern OID  dependents_any(OID self);
extern void  PRdepends_property(property *p,property *p2);

// namespace class for Reader 
class ReaderClass: public NameSpace {
public:

ClaireClass * _delimiter;
global_variable * arrow;
global_variable * triangle;
ClaireClass * _reserved_keyword;
reserved_keyword * _cl_else;
reserved_keyword * _cl_for;
reserved_keyword * _cl_case;
reserved_keyword * _cl_while;
reserved_keyword * _cl_until;
reserved_keyword * _cl_let;
reserved_keyword * _cl_when;
reserved_keyword * _cl_try;
reserved_keyword * _cl_if;
reserved_keyword * _cl_Zif;
reserved_keyword * _cl_branch;
keyword * forall;
keyword * none;
keyword * None;
keyword * L__equal;
keyword * L_;
keyword * CATCH;
keyword * in;
keyword * as;
keyword * L_L_;
keyword * PRINTF;
keyword * assert;
keyword * RETURN;
keyword * BREAK;
keyword * trace;
keyword * exists;
keyword * some;
keyword * _equal_sup;
keyword * _ask;
keyword * rule;
keyword * quote;
property * inspect;
property * known_I;
ClaireClass * _meta_reader;
global_variable * AND;
global_variable * OR;
table * DBline;
meta_reader * reader;
global_variable * STDOUT;
global_variable * STDIN;
global_variable * _starfs_star;
keyword * q;
property * call_debug;
table * EVAL;
property * pretty_show;
property * execute_do;
property * execute_bk;
property * inspect_loop;
property * get_from_integer;
property * top_debugger;
global_variable * _starlast_star;
global_variable * _starindex_star;
global_variable * _starmaxd_star;
global_variable * _starcurd_star;
global_variable * _starshowall_star;
property * up;
property * dn;
property * where;
ClaireClass * _PRcount;
table * PRdependent;
table * PRdependentOf;
property * s_index;// Reader/"s_index"
property * fromp;// Reader/"fromp"
property * nb_line;// Reader/"nb_line"
property * firstc;// Reader/"firstc"
property * last_form;// Reader/"last_form"
property * maxstack;// Reader/"maxstack"
property * toplevel;// Reader/"toplevel"
property * eof;// Reader/"eof"
property * space;// Reader/"space"
property * tab;// Reader/"tab"
property * bracket;// Reader/"bracket"
property * paren;// Reader/"paren"
property * comma;// Reader/"comma"
property * curly;// Reader/"curly"
property * next;// Reader/"next"
property * keyword_ask;// Reader/"keyword?"
property * stop_ask;// Reader/"stop?"
property * nextunit;// Reader/"nextunit"
property * nexts;// Reader/"nexts"
property * loopexp;// Reader/"loopexp"
property * nexte;// Reader/"nexte"
property * nextexp;// Reader/"nextexp"
property * nexti;// Reader/"nexti"
property * read_escape;// Reader/"read_escape"
property * nextvariable;// Reader/"nextvariable"
property * nexts_I;// Reader/"nexts!"
property * nexte_I;// Reader/"nexte!"
property * extended_comment_ask;// Reader/"extended_comment?"
property * extended_comment_I;// Reader/"extended_comment!"
property * DBregister;// Reader/"DBregister"
property * Call_I;// Reader/"Call!"
property * operation_ask;// Reader/"operation?"
property * combine;// Reader/"combine"
property * combine_I;// Reader/"combine!"
property * operation_I;// Reader/"operation!"
property * operand_I;// Reader/"operand!"
property * precedence_I;// Reader/"precedence!"
property * nextstruct;// Reader/"nextstruct"
property * readlet;// Reader/"readlet"
property * readlet_star;// Reader/"readlet*"
property * readwhen;// Reader/"readwhen"
property * show;// claire/"show"
property * readif;// Reader/"readif"
property * readcase;// Reader/"readcase"
property * readset;// Reader/"readset"
property * dereference;// Reader/"dereference"
property * nextseq;// Reader/"nextseq"
property * readblock;// claire/"readblock"
property * Do_I;// Reader/"Do!"
property * readcall;// Reader/"readcall"
property * nextdefinition;// Reader/"nextdefinition"
property * nextmethod;// Reader/"nextmethod"
property * nextinst;// Reader/"nextinst"
property * nextDefclass;// Reader/"nextDefclass"
property * useless_c;// Reader/"useless_c"
property * skipc;// Reader/"skipc"
property * skipc_I;// Reader/"skipc!"
property * cnext;// Reader/"cnext"
property * findeol;// Reader/"findeol"
property * checkno;// Reader/"checkno"
property * verify;// Reader/"verify"
property * Serror;// Reader/"Serror"
property * extract_variable;// Reader/"extract_variable"
property * bind_I;// Reader/"bind!"
property * unbind_I;// Reader/"unbind!"
property * load_file;// Reader/"load_file"
property * load;// claire/"load"
property * sload;// claire/"sload"
property * add_modules;// Reader/"add_modules"
property * eload;// claire/"eload"
property * debug_if_possible;// Reader/"debug_if_possible"
property * print_exception;// Reader/"print_exception"
property * kill;// claire/"kill"
property * hashgrow;// mClaire/"hashgrow"
property * inspect_system;// Reader/"inspect_system"
property * debug_system;// Reader/"debug_system"
property * step_system;// Reader/"step_system"
property * CommandLoop;// Reader/"CommandLoop"
property * trace_on;// Reader/"trace_on"
property * untrace;// Reader/"untrace"
property * self_trace;// Reader/"self_trace"
property * trace_rule;// Reader/"trace_rule"
property * stop;// Reader/"stop"
property * breakpoint;// Reader/"breakpoint"
property * print_debug_info;// Reader/"print_debug_info"
property * Show;// Reader/"Show"
property * block;// claire/"block"
property * closure_build;// Reader/"closure_build"
property * step;// claire/"step"
property * rtime;// Reader/"rtime"
property * rdepth;// Reader/"rdepth"
property * rnum;// Reader/"rnum"
property * rloop;// Reader/"rloop"
property * rstart;// Reader/"rstart"
property * PRget;// claire/"PRget"
property * PRlook;// claire/"PRlook"
property * dependents;// Reader/"dependents"
property * extract_of_type;// Reader/"extract_of_type"
property * PRdepends;// claire/"PRdepends"
property * call_count;// Kernel/"call_count"
property * PRshow;// claire/"PRshow"
property * PRtime;// claire/"PRtime"
property * PRcounter;// claire/"PRcounter"
property * last_arrow;// Reader/"last_arrow"
property * s_properties;// Reader/"s_properties"
property * extended_operator;// Reader/"extended_operator"

// module definition 
 void metaLoad();};

extern ReaderClass Reader;
extern NameSpace iClaire;

#endif
