/***********************************************************************/
/**   microCLAIRE                                       Yves Caseau    */
/**   clAlloc.cpp                                                      */
/**  Copyright (C) 1998-99 Yves Caseau. All Rights Reserved.           */
/**  cf claire.h                                                       */
/***********************************************************************/

#include <claire.h>
#include <Kernel.h>
#include <marie.h>
#include <Core.h>
#include <conio.h>

// this is the stand_alone test file for the Claire microLibrary

/*********************************************************************/
/** Table of contents                                                */
/**    1. system definition kernel                                   */
/**    2. System module interface                                    */
/**    3. tests sequences                                            */
/**    4. Inspection & debug                                         */
/*********************************************************************/

void runTests();

/*********************************************************************/
/**    1. system definition kernel                                   */
/*********************************************************************/

void loadModules ()
{// module definitions
 Core.initModule("Core",claire.it,list::alloc(1,_oid_(Kernel.it)),
                 "meta",
                 list::alloc(4,_string_("method"),_string_("object"),
                             _string_("function"),_string_("types")));
 // module load
 Core.metaLoad(); }


// this is our main function (a simpler version of what is in main.cpp)
main()
{int t1,t2;
  ClAlloc = new ClaireAllocation;
  ClAlloc->logList = 18;
  ClAlloc->maxList = (1 << 18);
  ClAlloc->maxSize = (1 << 18) + (1 << 18);
  ClAlloc->maxGC = 20000;
  ClAlloc->maxStack = 8000;                   // <yc> 9/98 changed because of GC
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

void CL_exit(int i)
  {if (i = 2) {printf("--- See you later, Alligator .... \n");
#ifdef CLDEBUG
for (int i = 1; i < 500000000; i++) i = (i + 1 - 1);
#else
for (int i = 1; i < 500000000; i++) i = (i + 1) * 2 - i - 2;
#endif
               printf("See you in a while, Crocodile !\n");}
   exit(1);}

void CL_system(char *s) {};

// allocate a block of size n * size(OID) if possible
void *CL_largealloc(int n)
{int *x;
  x = (int *) malloc( (size_t) (n * sizeof(OID)));
  if (x == NULL)
  {printf("There is not enough memory for CLAIRE, try claire -s 0 0\n");
   for (n = 1; n < 20000000; n++ ) x = (int *) n;
   exit(0);}
  return x;}



/*********************************************************************/
/**    3. tests sequences                                            */
/*********************************************************************/

int checkValue, checkZero = 0;

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
    testCall();
    msec(t2);
    printf("--- call test completed in %d ms.\n", t2 - t1);}
   {msec(t1);
    claire_gc();
    msec(t2);
    printf("--- gc test completed in %d ms.\n", t2 - t1);}
}

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

// test the print

// test the types

/*********************************************************************/
/**    4. Inspection & debug                                         */
/*********************************************************************/


// printf an object with nested level of detail dp
void see(OID n,int dp)
{ for (int i = 0; i < dp; i++) printf("--");                  // indentation
  if (n == CNULL)        printf("CNULL");
  else if (n == NOTHING) printf("NOTHING");
  else if (CTAG(n) == NIET_CODE)
    {printf("Chunk[%d:%d](",ADR(n),Cmemory[ADR(n)]);
     if (Cmemory[ADR(n) + 1] == NOTHING) printf("EMPTY");
     else if (Cmemory[ADR(n)] > 100 || dp == 0) printf("...");
     else for (int i = 1; i < Cmemory[ADR(n)]; i++)
         {see(Cmemory[ADR(n) + i], dp - 1); printf("@%d\n, ",i);}
     printf(")");}
  else if INTEGERP(n) princ_integer(n);
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
{if (ClEnv->verbose >= 10)
   {printf("%s: ",s);
    see(x,i);
    printf("\n");}}

void see(char *s, OID x) {see(s,x,1);}

