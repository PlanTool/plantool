/***********************************************************************/
/**   microCLAIRE                                       Yves Caseau    */
/**   clAlloc.cpp                                                      */
/**  Copyright (C) 1998-99 Yves Caseau. All Rights Reserved.           */
/**  cf claire.h                                                       */
/***********************************************************************/

#include "claire.h"
#include "Kernel.h"
#include "marie.h"
#include <conio.h>

// this is the stand_alone test file for the Claire kernel

/*********************************************************************/
/** Table of contents                                                */
/**    1. system definition kernel                                   */
/**    2. System module interface                                    */
/**    3. tests sequences                                            */
/**    4. Inspection & debug                                         */
/*********************************************************************/

// this should be in Systen.h (used for demonstration purposes)
class SystemClass: public NameSpace {
    public:
    void metaLoad();};


void runTests();

/*********************************************************************/
/**    1. system definition kernel                                   */
/*********************************************************************/

// although we have no other modules to link we show the code as an example
SystemClass System;

void loadModules ()
{// module definitions
 System.initModule("System",claire.it,list::alloc(1,_oid_(Kernel.it)),
                   "source dir",list::alloc(2,_string_("file1"),_string_("file2")));
 // module load
 System.metaLoad(); }


// this is our main function (a simpler version of what is in main.cpp)
main()
{int t1,t2;
  ClAlloc = new ClaireAllocation;
  ClAlloc->logList = 20;
  ClAlloc->maxList = (1 << 20);
  ClAlloc->maxSize = (1 << 20) + (1 << 20);
  ClAlloc->maxGC = 100000;
  ClAlloc->maxStack = 20000;                   // <yc> 9/98 changed because of GC
  ClAlloc->maxHist = 10000;
  ClAlloc->maxEnv = 400;
  ClAlloc->maxHash = ((1 << 11) + 200);       // <yc> 7/98 Changed for claire3 !
  ClAlloc->hashMask = ((1 << 11) - 1);
  ClAlloc->stdOut = ClairePort::make(stdout);
  ClAlloc->stdIn = ClairePort::make(stdin);
  ClAlloc->stdErr = ClairePort::make(stderr);

  printf("------------- Kernel version %g ------------------\n",CKernelRelease);
  msec(t1);
  ClaireResource::run();
  msec(t2);
  printf("------------- build complete in %d ms.\n", t2 - t1);
  runTests();
  CL_exit(2);
  return 1;}

/*********************************************************************/
/**    2. System module interface                                    */
/*********************************************************************/


// these are three functions that the console must provide to the claire
// system

// exit
void CL_exit(int i) { exit(i);}

// call to system
void CL_system(char *s) {system(s);}

// allocate a block of size n * size(OID) if possible
void *CL_alloc(int n)
{int *x;
  x = (int *) malloc( (size_t) (n * sizeof(OID)));
  if (x == NULL)
  {printf("There is not enough memory for CLAIRE, try claire -s 0 0\n");
   for (n = 1; n < 20000000; n++ ) x = (int *) n;
   exit(0);}
  return x;}


void SystemClass::metaLoad() {}

// in the debug mode, the definition is a copy of the restriction
void insert_definition_property(property *p,restriction *r)
{p->definition->addFast(_oid_(r)); }

ClaireClass *class_I_type(ClaireType *x)
{if (x->isa == Kernel._class) return (ClaireClass *) x;
 return Kernel._any;}

ClaireType *of_extract_type(ClaireType *t)
{return Kernel._any;}

// methods that are defined in the CLAIRE code (a dumb equivalent exists in the test file)
void push_debug_property(property *p,int n, int m) {}
void pop_debug_property(property *p,int n,OID v) {}

// we make a simple search in the restriction list
ClaireObject* find_which_class(ClaireClass *c,list *l, int i, int j)
 {ITERATE(r);
 see("call find which on ",_oid_(l),1);
 for (START(l); NEXT(r); )
    if INHERIT(c,class_I_type(OBJECT(ClaireType,(*(OBJECT(method,r)->domain))[1])))
       return OBJECT(method,r);
 return CFALSE;}

ClaireObject* find_which_property(property *p,int i, ClaireClass *c)
{ITERATE(r);
 for (START(p->restrictions); NEXT(r); )
    if INHERIT(c,class_I_type(OBJECT(ClaireType,(*(OBJECT(method,r)->domain))[1])))
       return OBJECT(method,r);
 return CFALSE;}

// a test version that only works on compiled method
OID eval_message_property(property *p,ClaireObject *r,int i,ClaireBoolean *b)
{if (r->isa == Kernel._method)
    {method *m = (method *) r;
      see("apply method",_oid_(m));
      return stack_apply_function(m->functional,(list *) m->srange,i,ClEnv->index);}
 else return 1;}

class demon {};
OID funcall_demon(demon *x, OID y, OID z)
{return 1;}

method *inlineok_ask_method(method *m, char *s) {return m;}

ClaireObject * hashget_class(ClaireClass *c,property *p) {return CFALSE;}


/*********************************************************************/
/**    3. tests sequences                                            */
/*********************************************************************/

int checkValue, checkZero = 0;
void testList();
void testSet();
void testArray();
void testString();
void testCall();

int testFail(int n)
{printf("error in line %d\n",n);
 checkValue = 1 / checkZero;
 return 1;}

// macro used for debug
#define check(A) ((A) ? checkValue = 1 : testFail(__LINE__));

void runTests()
{int t1,t2;
   {msec(t1);
    testList();
    msec(t2);
    printf("--- List test completed in %d ms.\n", t2 - t1);}
   {msec(t1);
    testSet();
    msec(t2);
    printf("--- Set test completed in %d ms.\n", t2 - t1);}
   {msec(t1);
    testArray();
    msec(t2);
    printf("--- Array test completed in %d ms.\n", t2 - t1);}
   {msec(t1);
    testString();
    msec(t2);
    printf("--- string test completed in %d ms.\n", t2 - t1);}
   {msec(t1);
    testCall();
    msec(t2);
    printf("--- call test completed in %d ms.\n", t2 - t1);}
   {msec(t1);
    claire_gc();
    msec(t2);
    printf("--- gc test completed in %d ms.\n", t2 - t1);}
}


// tests the lists
void testList()
{int i,j;
 list *l = list::empty(Kernel._integer);
 for (i = 1; i < 100; i++)
   {check(l->length == 0);
    for (j = 1; j < 1000; j++) l->addFast(j);
    check(l->length == 999);
    check((*l)[500] == 500);
    check(belong_to(444,_oid_(l)) == CTRUE);
    for (j = 1; j <= 1000; j++) delete_bag(l,j);
    check(l->length == 0);}
}

// tests the sets
void testSet()
{int i,j;
 set *l = set::empty(Kernel._integer);
 for (i = 1; i < 100; i++)
   {check(l->length == 0);
    for (j = 1; j < 1000; j++) l->addFast(j);
    check(l->length == 999);
    check((*l)[500] == 500);
    check(belong_to(444,_oid_(l)) == CTRUE);
    for (j = 1; j <= 1000; j++) delete_bag(l,j);
    check(l->length == 0);}
}

// tests the arrays
void testArray()
{int i,j,k,z;
 for (k = 1;k < 1000 ;k++)
 {OID *l = make_array_integer(100,Kernel._integer,0);
  j = 0;
  check(length_array(l) == 100);
  check(of_array(l) == Kernel._integer);
  for (i = 1; i <= 100; i++) l[i] = i;
  nth_put_array(l,50,50);
  check(nth_get_array(l,40) == 40);
  check(nth_get_array(l,50) == 50);
  for (z = 1; z <= 100; z++)
     for (i = 1; i <= 100; i++) j += l[i];
  check(j == 100 * 101 * 50);
 }
}

// test the tables

// test the worlds

// test the calls
void testCall()
{printf("----------- enter test Call ---------------------\n");
 check((*Kernel.mod)(7,4) == 3);
 check(OWNER(slot_get_object(Kernel._class,3,Kernel._string)) == Kernel._string);
 store_object(Kernel._class,3,Kernel._string,_string_("Claire Class"),CFALSE);
 check(equal_string("Claire Class",
                    string_v(slot_get_object(Kernel._class,3,Kernel._string))) == CTRUE);
 ITERATE(c1);
 for (START(Kernel._class->instances); NEXT(c1);)
   {ITERATE(c2);
    for (START(Kernel._class->instances); NEXT(c2);)
       {ClaireBoolean *inh = belong_to(c2,_oid_(OBJECT(ClaireClass,c1)->ancestors));
          if  INHERIT(OBJECT(ClaireClass,c1), OBJECT(ClaireClass,c2))
              {check(inh == CTRUE);}
          else check(inh == CFALSE);  }}
}

// test the tuples (later)

// test the floats

// test the strings and chars
void testString()
{char *s1 = make_string_integer(10,ClRes->ascii['1']);
 char *s2 = copy_string("testAB");
 nth_set_string(s2,3,ClRes->ascii['s']);
 check(equal_string(s2,copy_string(s2)) == CTRUE);
 check(equal_string(s2,"testBA") == CFALSE);
 check(nth_string(s2,2)->ascii == (int) 'e');
 check(strlen(s1) == 10);
 check(OWNER(_oid_(ClRes->ascii[30])) == Kernel._char);
 check(get_string(s2,ClRes->ascii['B']) == 6);
 check(OWNER(_string_(s2)) == Kernel._string);
 check(equal_string(substring_string(append_string(s2,s2),5,8),"ABte") == CTRUE);
}

/*********************************************************************/
/**    4. Inspection & debug                                         */
/*********************************************************************/


// printf an object with nested level of detail dp

// printf an object with nested level of detail dp
void see(OID n,int dp)
{ if (n == CNULL)        printf("CNULL");
  else if (n == NOTHING) printf("NOTHING");
  else if (CTAG(n) == NIET_CODE)
    {printf("Chunk[%d:%d](",ADR(n),Cmemory[ADR(n)]);
     if (Cmemory[ADR(n) + 1] == NOTHING) printf("EMPTY");
     else if (Cmemory[ADR(n)] > 100 || dp == 0) printf("...");
     else for (int i = 1; i < Cmemory[ADR(n)]; i++)
         {see(Cmemory[ADR(n) + i], dp - 1); printf("@%d\n, ",i);}
     printf(")");}
  else if INTEGERP(n) princ_integer(n);
  else if (OWNER(n) = Kernel._float) princ_float(float_v(n));
  else if (CTAG(n) == OBJ_CODE)
      {ClaireAny *x = OBJECT(ClaireAny,n);
       int u = getADR(x);
         if (x->isa == Kernel._list || x->isa == Kernel._set)
           {ITERATE(y);
            int i = 1;
            printf("%s[%d:%d](",((x->isa == Kernel._list) ? "list" : "set"),
                             ((bag *) x)->length, (((bag *) x)->content)[0]);
            if (((bag *) x)->length < 20 && dp > 0)
               {for (START(((bag *) x)); NEXT(y); )
                {see(y, dp - 1);printf(", ");}}
            else printf("...");
            printf(")");}
         else if (x->isa == Kernel._string) printf("'%s",((ClaireImport *) x)->value);
         else if (x == CTRUE) printf("CTRUE");
         else if (x == CFALSE) printf("CFALSE");
         else if (x->isa == Kernel._class)
              princ_string(((ClaireClass *) x)->comment);
         else if ((x->isa == Kernel._property) || (x->isa == Kernel._operation))
                  princ_string(((property *) x)->name->name);
         else if (x->isa == Kernel._char) printf("'%c'",((ClaireChar *) x)->ascii);
         else if (x->isa == Kernel._slot) {printf("slot<");
                                                 see(_oid_(((slot *) x)->selector));
                                                 printf(">");}
         else if (x->isa == Kernel._method) {printf("method<");
                                                 see(_oid_(((slot *) x)->selector));
                                                 printf("@");
                                                 see(_oid_(((method *) x)->domain),1);
                                                 printf(">");}
         else printf("<%s:%d>",x->isa->comment,ADR(n));}
 }


void see(OID x) {see(x,0);}

void see(char *s, OID x, int i)
{printf("%s: ",s);
 see(x,i);
 printf("\n");}

void see(char *s, OID x) {see(s,x,0);}

