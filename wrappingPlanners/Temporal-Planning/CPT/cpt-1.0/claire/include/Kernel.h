/** @package 

        Kernel.h
        
        Copyright(c) BOUYGUES 2000
        
        Author: YVES CASEAU
        Created: YC  26/01/2003 10:13:39
	Last change: YC 14/06/2003 18:57:15
*/
/***********************************************************************/
/**   microCLAIRE                                       Yves Caseau    */
/**   Kernel.h                                                         */
/**  Copyright (C) 1998-99 Yves Caseau. All Rights Reserved.           */
/**  Redistribution and use in source and binary forms are permitted   */
/**  provided that source distribution retains this entire copyright   */
/**  notice and comments.                                              */
/***********************************************************************/

// --------------------------------------------------------------------------
// this file is the public header file for the Kernel module.
// it is similar to the files produced by the CLAIRE compiler for each module
// it contains the class definitions and the API function signatures
// --------------------------------------------------------------------------

class slot;        // forward
class thing;
class lambda;

// ***************************************************************************
// * PART 1: CLAIRE class hierarchy                                          *
// ***************************************************************************

// Any is the union of object, primitive and integer - In Java it is a class but
// not in C++. Notice that the isa slot exists for Objects (as seen by CLAIRE)
// but also for primitive (not seen by CLAIRE but convenient)
// thus we use a common root ClaireAny that is not reified ...
class ClaireAny  {
  public:
  ClaireClass *isa; };

// ---------------------------- primitive -----------------------------------------------
// primitive objects are objects that are not instantiated the claire way ...
class ClairePrimitive : public ClaireAny {};

// imports are for extensibility
class ClaireImport : public ClairePrimitive
{ public:
  int value;};                           // the 32 bits address of the thing that we import

// same thing for floats but in CLAIRE 2.9 we use a 64bits double
class ClaireFloat: public ClairePrimitive
{public:
 double value;
 };

// Functions are object-pointer to C functions
class ClaireFunction: public ClairePrimitive
 {public:
  int value;
  char* name;};

// arrays are imported that are allocated in a special way so that their tail is a memory
// we only pass the exported OID/double*, from which we can get the Imported back.
// we use the two array_v and _array_ macros
// the
class ClaireArray: public ClaireImport { };


// Ports in v2.9 are full C++ objects (there is actually a hierarchy of such)
// we allocate them through C++
// they are IMPORTED into the Claire world through an import object. Thus, they are not part of
// the claire hierarchy (i.e.: ClairePort could be stubstituted by a proper C++ library, which
// was the original intent)
class ClairePort
{public:
 int status;                            // used to keep some info
 int firstc;                            // used in the nextc/firstc mode
 int getNext();                         // we use the int representation of char

 static ClairePort *make(FILE *f);      // used for stdio ....
 virtual char get();
 virtual void put(char c) ;
 virtual void put(int n);
 virtual void put(double x);
 virtual void prettyp(double x);        // v3.2.54
 virtual void flush();
 virtual void pclose();
 };                  //  v3.3.40 remove debugSee

// ----------------------------- objects -----------------------------------------------

// a root for all claire-instantiated objects
class ClaireObject: public ClaireAny {};

class SystemObject: public ClaireObject{};


// ----------------------------- collections -------------------------------------------

// in claire v2.9 we regroup collections together
// however the set of "abstract set" (iteration + membership) is larger:
//  collection + set + array
// thus use the OID sort
class ClaireCollection: public ClaireObject {
    public:
    ClaireBoolean *contains(OID x);              // private method
    };

// this is the CLAIRE Type System reflective definition ----------------------------------
class ClaireType: public ClaireCollection {};

class property;

// bag is a common root for lists, sets and arrays (special !)
// note that a bag is now a type, even though we only use sets in CLAIRE
class bag: public ClaireType
{
  public:
  int length;
  ClaireType *of;
  OID* content;

  inline OID &operator[](int i);     // TODO : make inline !!!
  static void quickSort(OID *a, int i, int m);
  static int quickClean(OID *a, int m);
  list *bag::clone();                      // v3.2: needed for Collect
  list *bag::clone(ClaireType *t);         // v3.2

 };

// lists are typed, ordered collections stored in the content[] array (OIDs)
class list: public bag
{public:
 static list *empty();
 static list *empty(ClaireType *t);
 static list *make();                          // simple intermediate constructor
 static list *make(OID x);                     // constructor for list(x)
 static list *make(OID x, OID y);              // list(x,y)
// static list *makeStack(OID x, OID y);         // list(x,y) allocated on the stack
 static list *alloc(int n,...);                // create a list with variable num of args
 static list *alloc(ClaireType *t, int n,...);
 static list *domain(int n, ...);              // sugar for list of types

 list *addFast(OID x);         // add x at the end of l
 ClaireBoolean *equalList(list *l2);
 ClaireBoolean *list::memq(OID val);
// virtual ClaireBoolean *isEqual(OID x2);
 };

// a set is implemented as a sorted list (system order, no duplicates)
class set: public bag
 {public:
   static inline set *make();          // contructors
   static set *empty();
   static set *empty(ClaireType *t);

   static set *alloc(int n,...);              // creates a set with variable number of args
   static set *alloc(ClaireType *t, int n,...);

   set *addFast(OID val);
// virtual ClaireBoolean *isEqual(OID x2);
   ClaireBoolean *equalSet(set *l2);
    };

// tuples are value (constant bags)
class tuple: public bag
{public:
   static inline tuple *make();          // contructors
   static tuple *alloc(int n,...);              // creates a set with variable number of args
   static tuple *empty();
   static tuple *allocStack(int n,...);           // for dynamic tuples
   tuple *addFast(OID x);         // add x at the end of l
   tuple *copyIfNeeded();
 
};

class listargs: public list {};

// in v2.9 the class is put in the type hierarchy.
// classes are described in ClReflect for the C++ part and in CLAIRE for the rest (management
// of restrictions)
class ClaireClass: public ClaireType
{public:
  symbol *name;
  char *comment;                // a comment that describes the class
  list *slots;                  // a list of slots
  ClaireClass *superclass;      // father in hierarchy
  set *subclass;
  list *ancestors;
  set *descendents;
  ClaireFunction *evaluate;
  int open;                     // tells the degree of evolvability (cf.clReflect )
  list *instances;
  list *prototype;
  list *params;
  int code;                     // lattice encoding
  list *dictionary;             // dictionary (defined in CLAIRE !)
  ClaireBoolean *ident_ask;     // true <=> ( = <=> identical)
  OID if_write;                 // new in 3.0 : rules on instantiation !
  list *dispatcher;             // new in 3.1 : fast dispatch => list of functions

  static ClaireClass *make(ClaireAny *x);  // make a class (simple version without alloc) for bootstrap
  static ClaireClass *make(char *n);    // same with a name
  static ClaireClass *make(char *name, ClaireClass *c2, module *def);   // regular constructor
  ClaireObject *instantiate(int n);      // very crude instantiation (only the zone)
  ClaireObject *instantiate();           // use the prototype list as the pattern
  thing *instantiate(char *n, module *def);  // same for named objects
  void genealogy();                                 // creates the genealogy
  slot *addSlot(property *p,ClaireType *t,OID def,int ix); // add a slot
//  slot *addSlotNew(property *p,ClaireType *t,OID def,int ix); // add a slot
  int nBits(int n);  // number of bits that are necessary to encode n children
  int totalBits();   // number of bits used for encoding
  void encode();   // incremental encoding
  void recode(int n);
  void nodeCode(int cx,int n,int m,int i);
  ClaireObject *operator() (OID arg1);                        // replaces item1
  ClaireObject *operator() (OID arg1, OID arg2);                        // replaces item2
  ClaireObject *operator() (OID arg1, OID arg2, OID arg3);              // replaces OF(L_p,...)
  
};


// all the other types are defined in the System module


// unbound symbols are now in Kernel/unbound
class unbound_symbol : public SystemObject{
    public:
 symbol *name;};

// Symbols are regular objects but they are stored in the CLRes hash table
class symbol: public SystemObject
{public:
 char* name;                  // char* part      m1/toto -> "toto"
 module *module_I;             // the name space  m1/toto -> m1
 module *definition;            // where the symbol is defined (NULL => PRIVATE)
 OID value;                     // the "content" of the symbol

 static symbol *make(char *s, module *m1, module *m2);           // create a symbol
 OID getValue();                // read a value or create an UnboundSymbol
 };

// we create out 256 chars as objects - stored in the ClRes __ table
class ClaireChar: public SystemObject
{public:
int ascii;

static void init();
};

// exceptions
class ClaireException: public SystemObject {};

class ClaireError: public ClaireException {};

class system_error: public ClaireError {
public:
    int index;
    OID value;
    OID arg;
    static system_error *make(int a, OID i, OID j);
        };

// restrictions (slots & methods) are system objects
class restriction: public SystemObject {
 public:
  module *module_I;             // module in which the restriction is defined
  char *comment;                // a comment
  list *domain;
  ClaireType *range;
  property *selector;           // the property
                                // v3.2.38: removed some junk ... :-) FXJ
};

// a slot = instance variable
class slot: public restriction {
 public:
  ClaireClass *srange;               // sort for the slot's range: list(method) or type(slot)
  OID DEFAULT;                  // default value
  int index;                    // position in the class structure
};

// a method
class method: public restriction
{ public:
  list *srange;                 // sort for the slot's range: list(method) or type(slot)
  lambda *formula;              // a lambda
  ClaireFunction *functional;   // a function that is used by the compiler
  ClaireFunction *evaluate;     // a function that is used by the interpreter
  OID typing;                   // a second order type
  OID status;                   // a bitvector of abstract properties
  ClaireBoolean *inline_ask;               // require macr-expansion by the compiler

  method *inlineDef(char *s);           // add an inline definition to a method
};

// a thing is a named object
class thing: public ClaireObject
{public:
 symbol *name;
};

class SystemThing: public thing {};
class keyword: public SystemThing {};

// the two booleans are objects
class ClaireBoolean: public SystemObject {};

// relation is the abstraction of properties and table (defined in System)
class ClaireRelation: public SystemThing {
public:
  char *comment;
  ClaireType *domain;
  ClaireType *range;
  OID if_write;                         // a list, a Function or CNULL
  ClaireBoolean *store_ask;             // defeasible updates
  ClaireRelation *inverse;              // inverse or NULL
  int open;                             // the degree of evolvability
  ClaireObject *multivalued_ask;        // list, true or false
};

// a property is a collection of restrictions that share the same selector
class property: public ClaireRelation {
public:
  int trace_I;                               // trace status
  list *restrictions;
  list *definition;
  ClaireObject *dictionary;                  // FALSE or list
  ClaireObject *reified;                     // boolean or a PRcount
  int dispatcher;                            // new in v3.1 : index for the fast dispatch

  static property *make(char *name, module *m);               // create a property
  static property *make(char *name, int op, module *m);       // create a property
  static property *make(char *name, int op, module *m, int i);// create a property
  static property *make(char *name, int op, module *m, ClaireClass *c, int i);// create a property
  OID operator() (OID arg1);                                  // replaces OF(L_p,...)
  OID operator() (OID arg1, OID arg2);                        // replaces OF(L_p,...)
  OID operator() (OID arg1, OID arg2, OID arg3);              // replaces OF(L_p,...)
  OID operator() (OID arg1, OID arg2, OID arg3, OID arg4);    // replaces OF(L_p,...)
  OID operator() (OID arg1, OID arg2, OID arg3, OID arg4, OID arg5);
  OID operator() (OID arg1, OID arg2, OID arg3, OID arg4, OID arg5, OID arg6);
  OID operator() (OID arg1, OID arg2, OID arg3, OID arg4, OID arg5, OID arg6, OID arg7);
  OID operator() (OID arg1, OID arg2, OID arg3, OID arg4, OID arg5, OID arg6, OID arg7, OID arg8);
  OID operator() (OID arg1, OID arg2, OID arg3, OID arg4, OID arg5, OID arg6, OID arg7, OID arg8,
                  OID arg9);
  OID operator() (OID arg1, OID arg2, OID arg3, OID arg4, OID arg5, OID arg6, OID arg7, OID arg8,
                  OID arg9, OID arg10);
  OID operator() (OID arg1, OID arg2, OID arg3, OID arg4, OID arg5, OID arg6, OID arg7, OID arg8,
                  OID arg9, OID arg10, OID arg11);
  OID operator() (OID arg1, OID arg2, OID arg3, OID arg4, OID arg5, OID arg6,
                  OID arg7, OID arg8, OID arg9, OID arg10, OID arg11, OID arg12);
  OID operator() (OID arg1, OID arg2, OID arg3, OID arg4, OID arg5, OID arg6, OID arg7, OID arg8,
                  OID arg9, OID arg10, OID arg11, OID arg12, OID arg13);
  OID operator() (OID arg1, OID arg2, OID arg3, OID arg4, OID arg5, OID arg6, OID arg7, OID arg8,
                  OID arg9, OID arg10, OID arg11, OID arg12, OID arg13, OID arg14);
  OID operator() (OID arg1, OID arg2, OID arg3, OID arg4, OID arg5, OID arg6, OID arg7, OID arg8,
                  OID arg9, OID arg10, OID arg11, OID arg12, OID arg13, OID arg14, OID arg15);
  OID operator() (OID arg1, OID arg2, OID arg3, OID arg4, OID arg5, OID arg6, OID arg7, OID arg8,
                  OID arg9, OID arg10, OID arg11, OID arg12, OID arg13, OID arg14, OID arg15,
                  OID arg16);
  OID operator() (OID arg1, OID arg2, OID arg3, OID arg4, OID arg5, OID arg6, OID arg7, OID arg8,
                  OID arg9, OID arg10, OID arg11, OID arg12, OID arg13, OID arg14, OID arg15,
                  OID arg16, OID arg17);
  OID operator() (OID arg1, OID arg2, OID arg3, OID arg4, OID arg5, OID arg6, OID arg7, OID arg8,
                  OID arg9, OID arg10, OID arg11, OID arg12, OID arg13, OID arg14, OID arg15,
                  OID arg16, OID arg17, OID arg18);
  OID operator() (OID arg1, OID arg2, OID arg3, OID arg4, OID arg5, OID arg6, OID arg7, OID arg8,
                  OID arg9, OID arg10, OID arg11, OID arg12, OID arg13, OID arg14, OID arg15,
                  OID arg16, OID arg17, OID arg18, OID arg19);
  OID operator() (OID arg1, OID arg2, OID arg3, OID arg4, OID arg5, OID arg6, OID arg7, OID arg8,
                  OID arg9, OID arg10, OID arg11, OID arg12, OID arg13, OID arg14, OID arg15,
                  OID arg16, OID arg17, OID arg18, OID arg19, OID arg20);
   // v3.2.24 inline fcall
  inline int fcall (int a1);
  inline int fcall (int a1, int a2);
  inline int fcall (int a1, int a2, int a3);
  inline int fcall (int a1, int a2, int a3, int a4);

  OID stack_apply(int i);                             // apply to args in stack
  OID super(ClaireClass *c,int size);                 // used by compiler
  method *addMethod(list *dom, ClaireType *ran, int sta, ClaireFunction *f);  // add method
  method *addFloatMethod(list *dom, ClaireType *ran, int sta, ClaireFunction *f,
                         ClaireFunction *f2);  // add method for floats

  } ;

// an operation is a property with a special syntax 
class operation: public property {
public:
  int precedence;
  static operation *make(char *name, int op, module *m, int p);        // create an operation
  static operation *make(char *name, module *m, int p);        // create an operation
  } ;


// tables
class table: public ClaireRelation {
public:
  bag *graph;                  // the graph is a list or an array
  OID params;                  // integer, or pair of integer
  OID DEFAULT;                 // default value

  int expand(OID x);
  int insertHash(list *l, OID x, OID val);
};

// modules are regular objects in 2.9 (no more integer IDs)
// they are defined in System and inherit from NameSpace (an internal class)
class module: public SystemThing {
public:
    char *comment;
    list *parts;                // list of sub-modules
    module *part_of;             // sub-module of
    list *uses;                 // other modules that are used
    char *source;               // directory where the sources can be found
    list *made_of;
    int status;                 // new (0:default, 1:loaded, 2 compiled, 3:c+loaded, 4:c+l+traced)
    ClaireFunction *evaluate;   // function
    char *external;             // external of the module: .lib library
   
    static module *make(char *s, module *sup);          // constructor
    unsigned hash(char *s);                             // classical has function
    symbol *lookup(char *name);                         // symbol lookup
    symbol *getSymbol(char *name, module *def);

 };

// stacks: the ClaireEnv object could be duplicated in a multi-proces environt
class ClaireEnvironment: public SystemObject
{public:
  // --- this part is visible from CLAIRE through a meta-description ---------------
  int verbose;                          // verbosity level
  ClaireException* exception_I;         // the last error
  module* module_I;                     // current module we are in
  char *name;                          // external name
  double version;                      // note the real float !
  ClairePort *ctrace;                   // current trace port
  ClairePort *cout;                     // current out port
  int index;                            // eval stack index (top of stack)
  int base;
  int debug_I;                          // debug
  int trace_I;                          // for tracing
  int step_I;
  int last_debug;                       // last value of the debug index
  int last_index;                       // last value of the top of eval stack
  ClaireObject * spy_I;                 // store the spy method if any
  int count_call;                       // count the numbers of call
  int count_level;                      // level at which something happens ...
  OID count_trigger;                    // what should happen
  list *params;                         // list of arguments v2.4.07
  int close;                            // c: do not touch,   p:read only (-1)
  int ABSTRACT;                         // c: no more instances           (0)
  int FINAL;                            // c: no more subclasses          (1)
  int DEFAULT;                          //                                (2)
  int open;                             // p: open property (extensible)  (3)
  int ephemeral;                        // c:ephemeral objects            (4)

  // --- this part is only accessible through the Kernel -------------------------
  int abortOnError;                     // 0: OK, 1: stop if error (useful for debug)
  ClaireAny* moduleStack;                  // stack for begin/end (NEW)
  jmp_buf *handlers;                    // stack of handlers
  int cHandle;                          // the current level of handler
  int *stack;                           // evaluation stack
  int tIndex;                           // time counter index /* time counters                      */
  int tStack[10];                       // stack of time counters
  char buffer[MAXBUF];                  // a local buffer for string creation
  int bLength;                          // its length
  int gensym;                           // a seed for symbol generation

  // methods
  void init();
  void abort();
  inline void bufferStart();            // makes a string from the buffer
  inline void pushChar(char c);         // prints a char in the string buffer
  inline void put(char c);              // prints a char on the current port
  char *bufferCopy();                   // makes a string from the buffer
  void pushInteger(int n);              // prints an integer in the string buffer
  char *localString(char* s);           // make sure that a string is local

};

extern ClaireEnvironment *ClEnv;


// ***************************************************************************
// * PART 2: C++ functions that define methods                               *
// ***************************************************************************

// === objects ==============================================================
extern ClaireObject *copy_object(ClaireObject *x);
extern ClaireBoolean *equal(OID n, OID m);
extern OID slot_get_object(ClaireObject *x, int y, ClaireClass *s);
extern ClaireBoolean *belong_to(OID oself, OID ens);
extern int hash_list(list *l, OID x);
extern int Cerror(int n, OID a, OID b);
extern ClaireBoolean *boolean_I_ClaireAny(ClaireAny *x);
extern ClaireBoolean *boolean_I_any(OID n);

// === class & properties ===================================================
extern void add_slot_class(ClaireClass *c, property *p, ClaireType *t, OID def,int i);
// extern void add_slot_classNew(ClaireClass *c, property *p, ClaireType *t, OID def, int i);
extern ClaireClass *sort_I_class(ClaireClass *c);
extern method *add_method_property(property *p, list *dom, ClaireType *r,
                                   int status, OID f);
extern ClaireObject *new_object_class(ClaireClass *c);
extern thing *new_thing_class(ClaireClass *c, symbol *s);

extern int index_table(table *a, OID x);
extern int index_table2(table *a, OID x, OID y);
extern ClaireClass *class_I_symbol(symbol *s, ClaireClass *c2);

// === bags =================================================================
extern bag *copy_bag(bag *l);
extern bag *empty_bag(bag *l);
extern bag *delete_bag(bag *l, OID x);
extern ClaireType * of_bag(bag *l);
extern bag *cast_I_bag(bag *l, ClaireType *x);

// === list =================================================================
extern list *add_list(list *l, OID val);
extern list *add_I_list(list *l, OID val);
extern list *cons_any(OID val, list *l);
extern list *make_list_integer(int n, OID m);
extern int index_list (list *l, OID val);
extern list *add_star_list(list *l1, list *l2);
extern list *append_list(list *l1, list *l2);
extern list *add_at_list(list *l, int n, OID val);
extern list *delete_at_list (list *l, int n);
extern list *skip_list(list *l, int n);
extern list *cdr_list(list *l);
extern bag *shrink_list (bag *l, int n);
extern ClaireBoolean *contain_ask_list(list *l,OID x);
extern tuple *tuple_I_list(list *l);
extern list *list_I_tuple(tuple *l);
extern char *make_string_list(list *l);

// === sets =================================================================
extern set *add_set(set *l, OID val);
extern set *add_I_set(set *l, OID val);
extern set *_exp_set(set *l1, set *l2);
extern set *append_set (set *l1, set *l2);
extern set *set_I_bag (bag *l);
extern list *list_I_set (set *l);
extern set *sequence_integer(int n, int m);
extern ClaireBoolean *contain_ask_set(set *l,OID x);

// === array ==================================================================
extern OID *copy_array(OID *a);
extern int length_array(OID *a);
extern ClaireType  *of_array(OID *a);
extern OID *make_array_integer(int n, ClaireType *t, OID v);
extern OID nth_get_array(OID *a, int n);
extern void nth_put_array(OID *a, int n, OID y);
extern list *list_I_array(OID *a);
extern OID *array_I_list(list *l);
extern ClaireBoolean *contain_ask_array(OID *a, OID val);

// === strings =============================================================
extern char *copy_string(char *s);
extern ClaireBoolean *equal_string(register char *s1, register char *s2);
extern void princ_string(char *ss);
extern void self_print_string(char *ss);
extern char *append_string(char *ss1, char *ss2);
extern int integer_I_string(char *s);
extern char *substring_string(char *ss, int n, int m);
extern int get_string(char *ss, ClaireChar *c);
extern ClaireBoolean *_less_string(char *s1, char *s2);
extern int included_string(char *s1, char *s2,ClaireBoolean *p);
extern ClaireChar *nth_string (char *ss, int n);
extern void nth_set_string (char *ss, int n, ClaireChar *c);
extern char *shrink_string(char *ss, int n);
extern OID value_string(char *name);
extern OID value_module(module *m, char *name);
extern OID get_symbol_module(module *m, char *name);
extern symbol *symbol_I_string(char *s,module *m);
extern ClaireFunction *make_function_string(char *s);
extern char *date_I_integer(int i);
extern char* getenv_string(char *s);

// ==== modules & symbols =====================================================
extern void begin_module (module *x);
extern void end_module (module *x);
extern symbol *append_symbol(symbol *s1, OID s2);
extern void princ_symbol(symbol *s);
extern module *defined_symbol (symbol *s);
extern module *module_I_symbol (symbol *s);
extern symbol *gensym_string (char *s);
extern OID get_symbol (symbol *s);
extern OID put_symbol(symbol *s, OID x);
extern char* string_I_symbol(symbol *s);
extern int integer_I_symbol(symbol *s);


// === integer & floats =======================================================
// extern int _integer_(int n);
// extern OID _float_(double v);

extern int ch_sign(int n);
extern int mod_integer(int n, int m);
extern int _7_integer(int n, int m);
extern int _exp_integer(int n, int m);
extern int exp2_integer(int n);
extern ClaireChar *char_I_integer(int n);
extern char *string_I_integer (int n);
extern char *make_string_integer(int n, ClaireChar *c);
extern int random_integer(int n);
extern int times_integer(int m, int n);
extern void princ_integer(int n);

extern double to_float (int n);
extern OID to_float_(int n);
extern int integer_I_float(double n);
extern ClaireBoolean *_inf_float(double n, double m);
extern ClaireBoolean *_inf_equal_float(double n, double m);
extern ClaireBoolean *_sup_float(double n, double m);
extern ClaireBoolean *_sup_equal_float(double n, double m);
extern int integer_I_float_(OID n);
extern ClaireBoolean *_inf_float_(OID n, OID m);
extern ClaireBoolean *_inf_equal_float_(OID n, OID m);
extern ClaireBoolean *_sup_float_(OID n, OID m);
extern ClaireBoolean *_sup_equal_float_(OID n, OID m);
extern void princ_float(double x);
extern void princ_float_(OID x);
extern void print_float(double x);
extern void print_float_(OID x);

// === ports ==================================================================

extern ClairePort *use_as_output_port(ClairePort *p);
extern void fclose_port(ClairePort *p);
extern void putc_char(ClaireChar *c,ClairePort *p);
extern ClaireChar * getc_port(ClairePort *p);
extern void flush_port(ClairePort *p);
extern ClairePort *fopen_string(char *s, char *m);
extern ClairePort *port_I_void();
extern ClairePort *port_I_string(char *s);
extern char *string_I_port(ClairePort *p);
extern OID length_port(ClairePort *p);
extern void set_length_port(ClairePort *p, int m);
extern char *read_string_port(ClairePort *p);
extern OID read_ident_port(ClairePort *p);
extern OID read_number_port(ClairePort *p);
extern OID read_thing_port(ClairePort *p, module *app, ClaireChar *cur, module *def);
extern void pushback_port(ClairePort *p,int n);
extern void free_I_port(ClairePort *p);

// === char & system ==========================================================
extern void princ_char(ClaireChar *c);
extern void c_princ_c(ClaireChar *c);
extern void c_princ_string(char *ss);
extern void c_princ_symbol(symbol *s);
extern OID claire_mem(OID n);
extern void claire_stat();
extern ClaireBoolean *alpha_char (ClaireChar *cx);
extern int integer_I_char(ClaireChar *c);


// === system functions =======================================================
extern OID close_exception(ClaireException *x);
extern OID safe_exception(ClaireException *x);
extern OID reset_stack(OID x, int n);
extern void stack_add(int x);
extern OID fcall1(ClaireFunction *f,ClaireClass *s1,OID a1,ClaireClass *s);
extern OID fcall2(ClaireFunction *f,ClaireClass *s1,OID a1,ClaireClass *s2, OID a2,
                  ClaireClass *s);
extern OID fcall3(ClaireFunction *f,ClaireClass *s1,OID a1,ClaireClass *s2, OID a2,
                  ClaireClass *s3, OID a3, ClaireClass *s);
extern OID stack_apply_function(ClaireFunction *f, list *l, int n, int m);
extern void fastcall_any(OID x, OID y, OID z);
extern char *string_I_function(ClaireFunction *f);
extern OID store_list(list *l, int n, OID y, ClaireBoolean *b);
extern OID store_array(OID* a, int n, OID y, ClaireBoolean *b);
extern OID store_object(ClaireObject *x, int n, ClaireClass *s, OID y, ClaireBoolean *b);
extern list *store_add(list *l,OID y);
extern void world_push();
extern void world_pop();
extern void world_remove();
extern void world_slaughter();
extern int world_number();
extern int world_get_id();
extern void claire_gc();
extern void restore_state_void();
extern ClaireAny *GC_OBJ_F(ClaireAny *x);  // v3.3.36
extern OID GC_OID(OID x);
extern void GC__OID(OID x, int m);
extern char *GC_STRING(char *s);
extern void GC__STRING(char *s,int n);
extern OID *GC_ARRAY(OID *a);
extern void GC_RESERVE(int n);
extern int GC_DEBUG(int n);
extern void kill_I_any(OID n);
extern void claire_shell(char *s);
extern void time_set_void();
extern int time_get_void();
extern int time_read_void();
extern void time_show_void();

// for debug
extern int CL_Address(OID x);
extern char *CL_Oid(int x) ;
extern OID CL_Oid_inv(char *s) ;
extern void CL_exit(int i);
extern void CL_system(char *s);
extern void *CL_alloc(int n);
extern void checkOID(OID n);


// interface with outside env
extern int InspectLoop(list * l);
extern void DebugLoop();
extern int StepLoop();
extern char *CommandLoopVoid();


// DEBUG: to remove (needed for the testI.exe version)
extern void see(OID x);
extern void see(OID x, int i);
extern void see(char *c,OID x,int i);
extern void see(char *c,OID x);


// ***************************************************************************
// * PART 3: The Kernel object                                               *
// ***************************************************************************

// a first subclass for Kernel
class KernelClass: public NameSpace
{public: 

// class hierachy (alphabetical order)
ClaireClass *_any;
ClaireClass *_array;
ClaireClass *_bag;
ClaireClass *_boolean;
ClaireClass *_char;
ClaireClass *_class;
ClaireClass *_collection;
ClaireClass *_environment;
ClaireClass *_error;
ClaireClass *_exception;
ClaireClass *_float;
ClaireClass *_function;
ClaireClass *_cl_import;          // v3.3.22
ClaireClass *_integer;
ClaireClass *_keyword;
ClaireClass *_list;
ClaireClass *_listargs;
ClaireClass *_method;
ClaireClass *_module;
ClaireClass *_primitive;
ClaireClass *_object;
ClaireClass *_operation;
ClaireClass *_port;
ClaireClass *_property;
ClaireClass *_relation;
ClaireClass *_restriction;
ClaireClass *_set;
ClaireClass *_slot;
ClaireClass *_string;
ClaireClass *_symbol;
ClaireClass *_system_error;
ClaireClass *_system_object;
ClaireClass *_system_thing;
ClaireClass *_table;
ClaireClass *_thing;
ClaireClass *_tuple;
ClaireClass *_type;
ClaireClass *_unbound_symbol;
ClaireClass *_void;

// properties
operation *_dot_dot;
operation *_dash_dash_ask;    // new in v3.0.54
operation *_equal;
operation *_exp;
property *_exp2;
operation *_inf;
operation *_inf_equal;
operation *_inf_inf;
operation *_dash;
operation *_sup;
operation *_sup_equal;
operation *_star;
operation *_7;
operation *_7_plus;
operation *_Z;
property *DEFAULT;
property *ABSTRACT;
property *FINAL;
operation *add;
operation *add_I;
property *add_method;
property *add_slot;
property *add_star;
property *ancestors;
property *array_I;
property *arg;
property *backtrack;             // backtrack()
property *base;
property *begin;
property *body;
property *boolean_I;
property *c_princ;
property *cast_I;
property *cdr;
property *char_I;
property *choice;                    // branch()
property *close;
property *code;
property *cons;
property *contain_ask;
property *copy;
property *cout;
property *ctrace;                      
property *comment;
property *commit;            // commit()
property *commit0;                      // commit0()
property *count_call;                              // count the numbers of call
property *count_level;                 // level at which something happens ...
property *count_trigger;               // what should happen
property *date_I;
property *debug_I;
property *definition;
operation *_delete;
property *defined;
property *descendents;
property *dictionary;
property *dispatcher;                  // new in v3.1: fast dispatch
property *domain;
property *empty;                       // new in v3.1
property *end;
property *ephemeral;
property *evaluate;
property *exception_I;
property *external;
property *fastcall;
property *fclose;
property *float_I;
property *flush;
property *fopen;
property *formula;
property *free_I;
property *funcall;
property *functional;
property *gensym;
property *get;
property *getc;
property *graph;
property *hash;
property *ident_ask;
property *if_write;
property *included;
property *index;
property *inline_ask;
property *integer_I;
property *instances;
property *inverse;
property *isa;
property *last_debug;
property *last_index;         // last value of the top of eval stack
property *length;
property *list_I;
property *made_of;
property *make_list;
property *make_string;
property *mem;
property *member_ask;
property *mod;
property *module_I;           //  was module
property *multivalued_ask;
property *name;
property *nth;
property *nth_get;
property *nth_put;
property *nth_equal;
property *nth_plus;
property *nth_dash;
property *of;
property *open;
property *params;
property *parts;
property *part_of;
property *port_I;
property *precedence;
property *princ;
property *print;
property *prototype;
property *put;
property *putc;
property *random;
property *random_I;
property *range;
property *read_ident;
property *read_number;
property *read_string;
property *read_thing;
property *reified;
property *restrictions;
property *restore_state;
property *selector;
property *self_print;
property *set_I;
property *set_length;
property *shrink;
property *symbol_I;
property *slots;
property *slot_get;
property *sname;
property *sort_I;
property *source;
property *spy_I;                       // store the spy method if any
property *srange;
property *stat;
property *status;
property *stack_apply;
property *step_I;
property *store;
property *store_ask;
property *string_I;
property *superclass;
property *subclass;
property *substring;
property *trace_I;
property *typing;
property *use_as_output;
property *uses;
property *value;
property *verbose;
property *version;
property *world_ask;
property *world_id;

// tables
// global variables
symbol *PRIVATE;
thing *NoDefault;
set *emptySet;                          // one unique representation for emptyset
list *nil;
symbol *unknownName;
OID ctrue;
OID cfalse;
void bootstrap();
void metaLoad();                     // creates all the metaObjects
};

extern KernelClass Kernel;


// definition of the inline methods that need to be shared (MACROS) and that use
// the definitions of Kernel
inline void ClaireEnvironment::bufferStart() { bLength = 0;}  // makes a string from the buffer
inline void ClaireEnvironment::pushChar(char c)               // prints a char in the string buffer
    { ((bLength > MAXBUF) ? Cerror(16,0,0) : buffer[bLength++] = c);}
inline void ClaireEnvironment::put(char c) {cout->put(c);}   // prints a char on the current port



// profiler methods (ClaireProfile -> KernelProfile)
// note: although PRcount is not defined here, it is Kernel.h
extern class PRcount *PRstart(PRcount *p);          // the class keyword is key here !
extern void PRend(PRcount *p);
extern void PRloop(PRcount *p);
extern void default_main();                         // v3.2.50


// definitions from <marie.h> that are needed for inline methods
#define getADR(A) (((int) A - (int) &Cmemory[0]) >> 2)  // gets the ADR from the object
typedef int (*fptr1) (int);
typedef int (*fptr2) (int,int);
typedef int (*fptr3) (int,int,int);
typedef int (*fptr4) (int,int,int,int);

//to add when inline
inline OID &bag::operator[](int i)  {
#ifdef CLDEBUG
   if (ClEnv->verbose > 12) printf("BAG:~%x [%d] -> %x (%d)\n",this,i,content[i],getADR(content) + i);
   return ((i <= length) ? content[i] : (Cerror(41,_oid_(this),i), content[1]));
#else
    return this->content[i];
#endif
}


// v3.2.24 inline fcall
inline int property::fcall(int a1)
    {ClaireClass *c = ((ClaireObject *) a1)->isa;
     return ((fptr1) OBJECT(ClaireFunction,
              ((list *) (c->dispatcher))->content[dispatcher])->value)(a1);}

inline int property::fcall(int a1,int a2)
    {ClaireClass *c = ((ClaireObject *) a1)->isa;
     return ((fptr2) OBJECT(ClaireFunction,
             ((list *) (c->dispatcher))->content[dispatcher])->value)(a1,a2);}

inline int property::fcall(int a1, int a2, int a3)
    {ClaireClass *c = ((ClaireObject *) a1)->isa;
     return ((fptr3) OBJECT(ClaireFunction,
             ((list *) (c->dispatcher))->content[dispatcher])->value)(a1,a2,a3);}

inline int property::fcall(int a1, int a2, int a3, int a4)
    {ClaireClass *c = ((ClaireObject *) a1)->isa;
     return ((fptr4) OBJECT(ClaireFunction,
             ((list *) (c->dispatcher))->content[dispatcher])->value)(a1,a2,a3,a4);}


// TODO : ffcall with range double ! ...........
// v3.2.28 inline small functions that are really needed ---------------------------------

// create a float  (specialized version of import)
// note that the automatic protection is a necessary burden that frees the compiler from
// generating protection that will be un-necessary if the optimization produces native
// code, which is expected in most cases
inline OID _float_(double v)
{ClaireFloat *obj = (ClaireFloat *) ClAlloc-> makeAny(4);
   if (ClAlloc->statusGC != 2) GC_PUSH(obj);                // v3.2.28   (cf import in ClAlloc), v3.2.30
   obj->isa = Kernel._float;
   obj->value = v;
   return _oid_(obj);}

// makes a oid from an integer
inline OID _integer_(int n)
{  if INTEGERP(n) return n; else Cerror(19,0,0); return 1;}



