/***********************************************************************/
/**   microCLAIRE                                       Yves Caseau    */
/**   clString.cpp                                                     */
/**  Copyright (C) 1998-2003 Yves Caseau. All Rights Reserved.         */
/**  cf claire.h                                                       */
/***********************************************************************/

#include <claire.h>
#include <Kernel.h>
#include <marie.h>

// this file contains the C++ code for the imported objects, mostly strings
// (and the associated symbols, although they are not imported) but also
// integers and floats. Note the double implementation for floats

/*********************************************************************/
/** Contents                                                         */
/**    1. Strings                                                    */
/**    2. Modules                                                    */
/**    3. Symbol                                                     */
/**    4. integer                                                    */
/**    5. floats                                                     */
/*********************************************************************/

/*********************************************************************/
/**    1: String functions                                           */
/*********************************************************************/

// --- there are all API functions since string are imported char* -----

// make a local copy of a string
char *copy_string(char *s)
{int i;
  int j = strlen(s);
  char *a = ClAlloc->makeString(1 + (j+1) / 4);
  for (i = 0; s[i] != '\0'; i++) a[i] = s[i];
  a[i] = '\0';
  return a;}

// equality on strings
ClaireBoolean *equal_string(register char *s1, register char *s2)
{register OID i;
  for (i=0; s1[i]!='\0'; i++) if (s1[i] != s2[i]) return(CFALSE);
  if (s2[i] != '\0') return(CFALSE);
  return CTRUE;}

// internal form of princ_string for the compiler
void princ_string(char *ss)
{int i;
  for (i=0; ss[i] != '\0'; i++)
     ClEnv->cout->put(ss[i]); }

// print a string with the "" (what you see is what you read)
void self_print_string(char *ss)
{int i;
  ClEnv->put('\"');
  for (i=0; ss[i] != '\0'; i++)
       {if ((ss[i] == '\"') || (ss[i] == '\\')) ClEnv->put('\\');
        if (ss[i] == '\n') { ClEnv->put('\\'); ClEnv->put('n');}
        else if (ss[i] == '\t') { ClEnv->put('\\'); ClEnv->put('t');}
        else ClEnv->put(ss[i]);}
  ClEnv->put('\"');}

// concatenate two strings
char *append_string(char *ss1, char *ss2)
{ int i;
  ClEnv->bufferStart();
  for (i = 0; ss1[i] != '\0'; i++) ClEnv->pushChar(ss1[i]);
  for (i = 0; ss2[i] != '\0'; i++) ClEnv->pushChar(ss2[i]);
  return ClEnv->bufferCopy();}

// finds the integer value
int integer_I_string(char *s)
{ return strtol(s,NULL,10);}

// create a substring from a string
char *substring_string(char *ss, int n, int m)
{int i,l = 10000000; // TEST (was) strlen(ss);
  ClEnv->bufferStart();
  if ((n >= 1) && (n <= l))
     for (i = n-1; ((ss[i] != '\0') && (i < m)); i++) ClEnv->pushChar(ss[i]);
  return ClEnv->bufferCopy();}

// look for th eposition of the CHAR c in s
int get_string(char *ss, ClaireChar *c)
{ OID i;
  char c2 = (char) c->ascii;
  for (i=0; ss[i] != '\0'; i++) if (ss[i] == c2) return(i+1);
  return 0;}

// compare two strings
ClaireBoolean *_less_string(char *s1, char *s2)
{int i;
 for (i = 0; s1[i] != '\0'; i++)
    {if (s1[i] < s2[i]) return(CTRUE);
     else if (s1[i] > s2[i]) return (CFALSE);}           // include s2[i] = '0' !!!
 return CTRUE;}                                          // v3.1.12 ????

// test is a string is included into another
int included_string(char *s1, char *s2,ClaireBoolean *p)
{int c,i,j;
  for (i = 0; s1[i] != '\0'; i++)
    for (j = 0;; j++)
      { c = s1[i+j] - s2[j];
        if ((c == 0) || ((p == CTRUE) &&
             (((c == 32) && ((s2[j] >= 'A') && (s2[j] <= 'Z'))) ||
              ((c == -32) && ((s2[j] >= 'a') && (s2[j] <= 'z'))))))
           {if (s2[j+1] == '\0') return i+1;}
        else break;}
  return 0;}

// get the CHAR at the i-th place in s
ClaireChar *nth_string (char *ss, int n)
{ if (n <= strlen(ss)) return char_I_integer((int) ss[n-1]);      // v3.2.44
  else Cerror(11,n,_string_(ss)); return ClRes->ascii[1];}

// set the char at the i_th place in s
void nth_set_string (char *ss, int n, ClaireChar *c)
{if (n <= strlen(ss)) 
         ss[n-1] = (char) c->ascii;
 else Cerror(11,n,_string_(ss));}

// shrinks a string by placing the '\0' marker
char *shrink_string(char *ss, int n)
{if (n <= strlen(ss)) ss[n] = '\0';         // v3.1.10  allows identity ...
 else Cerror(11,n,_string_(ss));
 return ss;}

// the old internal function
// watch out: TODO use claire ?
OID value_string(char *name)
{return value_module(ClEnv->module_I,name);}

// the new internal function
OID value_module(module *m, char *name)
{symbol *s = m->lookup(name);
   if (s == NULL) return CNULL;
   else return s->value;}

// new: access to the symbol the new internal function
OID get_symbol_module(module *m, char *name)
{symbol *s = m->lookup(name);
   if (s == NULL) return CNULL;
   else return _oid_(s);}

// new: return the current date
char *date_I_integer(int i)
{struct tm *newtime;
 time_t aclock;
 time(&aclock);
 newtime = localtime(&aclock);
 return copy_string(asctime(newtime)); }

// for upward compatibility
char* getenv_string(char *s)
{char *s2 = getenv(s);
 if ( s2 == NULL) s2 = "";
 return s2;}
 
/*********************************************************************/
/**    2: Modules                                                    */
/*********************************************************************/

// ------------- member functions for modules ---------------------------------------

// constructor: create a module from the name and the father in the hierarchy
module *module::make(char *s, module *sup)
{module *m = (module *) ClAlloc->makeAny(10);
  m->isa = Kernel._module;
  m->name = symbol::make(s,claire.it,claire.it);
  m->name->value = _oid_(m);
  m->part_of = sup;
  m->status = 0;
  m->parts = list::empty(Kernel._module);
  // initialize the module - v3.3.3
  m->comment = NULL;
  m->made_of = NULL;
  m->evaluate = NULL;
  m->external = NULL;
  return m;}


// hash function.
unsigned module::hash(register char *s)
{unsigned val;
  for (val= (int) this; *s != '\0'; s++)
      val = *s + 31 * val;
  return val & ClAlloc->hashMask;}

// lookup: check if a string in a given module is represented by a symbol (returns NULL
// if no symbol is found - does NOT create a new symbol
// this method embodies the strategy for looking in upper modules (namespace inheritance)
symbol *module::lookup(char *name)
{int i = hash(name);
   while ((ClRes->sTable[i] != NULL) &&
          ((this != ClRes->sTable[i]->module_I) ||
           (equal_string(name,ClRes->sTable[i]->name) == CFALSE))) i++;
   if (i == ClAlloc->maxHash) Cerror(12,0,0);
   symbol *cur = ClRes->sTable[i];
   if (cur != NULL || this == claire.it) return cur;   // v3.2.38 - Thanks to FXJ !
   else return part_of->lookup(name); }


// Get a symbol (even if none is there => create it) in the module with the given name,
// this is a combination of lookup + symbol::make(...)
// notice that we do not inherit junk (undefined) but rather create a new symbol
symbol *module::getSymbol(char *name, module *def)
{symbol *cur = lookup(name);
  if ((cur != NULL) &&
      ((cur->value != CNULL) || (cur->module_I == this) || (cur == Kernel.unknownName)))
     return cur;
  else return symbol::make(GC_STRING(copy_string(name)),this,def); }   // v3.2.50

// create the module associated to a namespace (used by the compiler)
// assumes the existence of the claire module
void NameSpace::initModule(char *nom, module *father)
{it = module::make(nom,father);}

// similar but also fills the key slots for the module (compiler method)
// new status (0:default, 1:start, 2 compiled, 3:c+loaded, 4:c+l+trace, 5:c+delayed)
void NameSpace::initModule(char *nom, module *father, list* usage, char *dir, list *files)
{it = module::make(nom,father);
 it->uses = usage;                 // other modules that are used
 it->source = dir;               // directory where the sources can be found
 it->made_of = files;
 it->comment = nom;
 it->status = 3;
 Kernel._module->instances->addFast(_oid_(it));
 father->parts->addFast(_oid_(it));   // manage the inverse
}

// --- API functions for modules ---------------------------------------------------

/* open a module x with module identifier index */
void begin_module (module *x)
{ ((list *) ClEnv->moduleStack)->addFast(_oid_(ClEnv->module_I));
  ClEnv->module_I = x;}

/* close an application */
void end_module (module *x)
{int n = ((bag *) ClEnv->moduleStack)->length;
  if (n == 0) ClEnv->module_I = claire.it;
  else {ClEnv->module_I = OBJECT(module,(*((bag *) ClEnv->moduleStack))[n]);
        delete_at_list(((list *) ClEnv->moduleStack),n);}}



/*********************************************************************/
/**    3. Symbols                                                    */
/*********************************************************************/


// -------------- member functions --------------------------------------------

// create a claire symbol from an internal C string and a status, represented by
// def, which is NULL for private symbols and the definition (owner) for other symbols
// 
symbol *symbol::make(char *name, module *ns, module *def)
{int i = ns->hash(name);
   while ((ClRes->sTable[i] != NULL) &&
          ((ns != ClRes->sTable[i]->module_I) ||
           (equal_string(name,ClRes->sTable[i]->name) == CFALSE))) i++;
   if (i >= ClAlloc->maxHash) Cerror(12,0,0);
   if (ClRes->sTable[i] == NULL)
      {symbol *s = (symbol *) ClAlloc->makeAny(4);
       s->isa = Kernel._symbol;
       s->name = name;
       s->module_I = ns;
       s->definition = def;                  // def = NULL means private
       s->value = CNULL;
       ClRes->sTable[i] = s;}
   return ClRes->sTable[i];}

// read the value bound to a given symbol s. We create an unbound
//   symbol object if necessary
OID symbol::getValue()
{ if (value == CNULL && this != Kernel.unknownName)
     {unbound_symbol *o = (unbound_symbol *) Kernel._unbound_symbol->instantiate();
      o->name = this;
      return _oid_(o);}
  else return value;}

// to remove
void symbolDebug(symbol *s)
{int i = s->module_I->hash(s->name);
 printf("______ symbol debug s = %x __________________\n",s);
 printf(" string is %s, module:%s\n",s->name, s->module_I->comment);
 printf(" position is %d",i);
 if (ClRes->sTable[i] != s) printf("problem with the table !!!\n");
// see("content is ",s->value);
 printf("_________________________________\n");}
 
// --------------- API functions ---------------------------------------------

// create a symbol in the current module
symbol *symbol_I_string(char *s,module *m)
{return symbol::make(s,m,ClEnv->module_I);}

// writes the value of a symbol
OID put_symbol(symbol *s, OID x)
{s->value = x;
 return x;}

OID get_symbol(symbol *s) {return ((s->value == CNULL) ? CNULL : s->value);}

// concatenate two symbols, or a symbol and a string or a symbol and an integer
// the result is a symbol in the module of the first symbol
symbol *append_symbol(symbol *s1, OID s2)
{ OID i;
  char *ss1 = s1->name;
  ClEnv->bufferStart();
  for (i = 0; ss1[i] != '\0'; i++) ClEnv->pushChar(ss1[i]);
  if INTEGERP(s2) ClEnv->pushInteger(s2);
  else {if (OWNER(s2) == Kernel._symbol) ss1 = OBJECT(symbol,s2)->name;
        else if (OWNER(s2) == Kernel._string) ss1 = string_v(s2);
        else ss1 = "";
        for (i = 0; ss1[i] != '\0'; i++) ClEnv->pushChar(ss1[i]);}
  return symbol::make(GC_STRING(ClEnv->bufferCopy()),s1->module_I,s1->definition); }  // v3.3.34 - thanks to Sylvain

// print a symbol with its application name
void princ_symbol(symbol *s)
{ int i;
  char *ss1 = s->name;
  if ((s->module_I != claire.it) && (s->module_I != ClEnv->module_I))
     {princ_symbol(s->module_I->name); ClEnv->put('/');}
  for (i=0; ss1[i] != '\0'; i++) ClEnv->put(ss1[i]);}

// find the module where the object is defined
module *defined_symbol (symbol *s)
{ if (s->definition != NULL) return s->definition; else return s->module_I;}

module *module_I_symbol (symbol *s) {return s->module_I;}

char *string_I_symbol(symbol *s)
{return s->name;}

// create a new name
symbol *gensym_string (char *s)
{ ClEnv->bufferStart();
  for (; *s != '\0'; s++) ClEnv->pushChar(*s);
  ClEnv->pushChar('0' + ((char) ((ClEnv->gensym % 10000) / 1000)));
  ClEnv->pushChar('0' + ((char) ((ClEnv->gensym % 1000) / 100)));
  ClEnv->pushChar('0' + ((char) ((ClEnv->gensym % 100) / 10)));
  ClEnv->pushChar('0' + ((char) (ClEnv->gensym % 10)));
  ClEnv->gensym++;
  return claire.it->getSymbol(ClEnv->bufferCopy(),claire.it);}

// useful to represent a symbol with an integer
int integer_I_symbol (symbol *s)
{ int i = s->module_I->hash(s->name);
   while ((ClRes->sTable[i] != NULL) &&
          ((s->module_I != ClRes->sTable[i]->module_I) ||
           (equal_string(s->name,ClRes->sTable[i]->name) == CFALSE))) i++;
   return i;}


/*********************************************************************/
/**    4. integer & float API functions                              */
/*********************************************************************/

// useful upper and lower bound to check overflow
double CLMAXFLOAT = 1073741823.0;              // v3.3.12
double CLMINFLOAT = -1073741822.0;

void princ_integer(int i) {ClEnv->cout->put(i);}

// arithmetic functions
OID ch_sign(int n)
{ return -n ;}

int _7_integer(int n, int m)
{ if (m == 0) Cerror(20,n,0); return (n / m);}

int mod_integer(int n, int m)
{ if (m == 0) Cerror(20,n,0); return (n % m);}

// v3.3.16: use float exponentiation and check overflow
int _exp_integer(int n, int m)
{double a = (double) n, b = (double) m,  c = pow(a,b);
  if (c < CLMINFLOAT || c > CLMAXFLOAT) Cerror(40,n,m);
  return (int) c; }

// return a power of 2
int exp2_integer(int n)
{if ((n >= 0) && (n <= 31)) return (1 << n);
 else Cerror(19,0,0); return 1;}

// translate a integer into a char - v3.2.44 : supports encoding both on (-255 -- 256) or (0 -- 511)
ClaireChar *char_I_integer(int n)
{if ((n < -510) || (n > 511)) Cerror(21,n,0);
 else if (n < 0)  return ClRes->ascii[512 + n];
 return ClRes->ascii[n];}

// create a new string
char *string_I_integer (int n)
{ ClEnv->bufferStart();
  ClEnv->pushInteger(n);
  return ClEnv->bufferCopy();}

// allocate a list with n member equal to m */
char *make_string_integer(int n, ClaireChar *c)
{ if (n < 0) {Cerror(22,n,0); return "";}
  else {int i;
        char *s = ClAlloc->makeString(1 + n / 4);
          for (i = 0; i < n; i++) s[i] = (char) c->ascii;
          s[i] = '\0';
          return s; }}

// create a string from a list (v3.0.44) - should have been there from day one !
// TODO: trap the error nicely
char *make_string_list(list *l)
{if (l->of != Kernel._char) Cerror(22,0,0);
 int i,n = l->length;
 char *s = ClAlloc->makeString(1 + n / 4);
          for (i = 0; i < n; i++) s[i] = (char) OBJECT(ClaireChar,(*(l))[i + 1])->ascii;
          s[i] = '\0';
          return s; }

// give a safe multiplication
int times_integer(int n, int m)
{double a = (double) n, b = (double) m,  c = a * b;
  if (c < CLMINFLOAT || c > CLMAXFLOAT) Cerror(40,n,m);
  return n * m; }

// use C random generator
// v3.3.18: support long integer (> 16 bits) - v3.3.20 : use ALi's ideas to simplify

// v3.3.24 makes sure that rand() returns a > 0 number on all platforms
// v3.3.26 is even stronger, following Sylvain's suggestion
#ifdef CLWIN
#define C_RAND() rand()
#define C_RANDMAX RAND_MAX
#else
#define C_RAND() (rand() & 0x00007FFF)
#define C_RANDMAX (RAND_MAX & 0x00007FFF)
#endif

int random_integer(int n)
{if (n <= 1) return 0;
 else if (n <= 1000) return (C_RAND() % n);
 else if (n <= 30000) return ((C_RAND() * 16384 + (C_RAND() % 16383)) % n);
 else {double ratio = (double) n / (double) (C_RANDMAX + 1);
       double draw = (double) C_RAND() + ((double) C_RAND() / (double) C_RANDMAX);
       //printf("%g * %g = %g\n",draw,ratio, draw * ratio);
       return floor(draw * ratio);}}


// print
void princ_int(int n) {ClEnv->cout->put(n);}

/*********************************************************************/
/**    5. floats                                                     */
/*********************************************************************/

// all API functions are doubled:
// op_float(...) returns a double  [float parameters are doubles]
// op_float_(...) returns an OID   [float parameters are OID]

// makes an integer into a float */
OID to_float_ (int n) {return _float_((double) n);}
double to_float (int n) {return  ((double) n);}

// create a  claire integer from a claire float */
int integer_I_float_(OID n) {return integer_I_float(float_v(n));}
int integer_I_float(double n)
{ if (n < CLMINFLOAT || n > CLMAXFLOAT) Cerror(39,_float_(n),0);
  return _integer_((int) floor(n));}                         // v3.3

// the classical order comparisons for two float
ClaireBoolean *_inf_float(double n,double m)
{ if (n < m) return CTRUE;  else return CFALSE;}

ClaireBoolean *_inf_equal_float(double n,double m)
{ if (n <= m) return CTRUE;  else return CFALSE;}

ClaireBoolean *_sup_float(double n,double m)
{ if (n > m) return CTRUE;  else return CFALSE;}

ClaireBoolean *_sup_equal_float(double n,double m)
{ if (n >= m) return CTRUE;  else return CFALSE;}

ClaireBoolean *_inf_float_(OID n, OID m)
{ if (float_v(n) < float_v(m)) return CTRUE;  else return CFALSE;}

ClaireBoolean *_inf_equal_float_(OID n, OID m)
{ if (float_v(n) <= float_v(m)) return CTRUE;  else return CFALSE;}

ClaireBoolean *_sup_float_(OID n, OID m)
{ if (float_v(n) > float_v(m)) return CTRUE;  else return CFALSE;}

ClaireBoolean *_sup_equal_float_(OID n, OID m)
{ if (float_v(n) >= float_v(m)) return CTRUE;  else return CFALSE;}

// printing
void princ_float(double x) {ClEnv->cout->prettyp(x);}            // v3.2.54
void princ_float_(OID x) {ClEnv->cout->prettyp(float_v(x));}     // v3.2.54

void print_float(double x) {ClEnv->cout->put(x);}            // v3.2.54
void print_float_(OID x) {ClEnv->cout->put(float_v(x));}     // v3.2.54


