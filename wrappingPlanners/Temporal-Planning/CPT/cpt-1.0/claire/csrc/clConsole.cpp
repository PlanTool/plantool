/***********************************************************************/
/**  microCLAIRE                                       Yves Caseau     */
/**  testiClaire.cpp                                                   */
/**  Copyright (C) 1994-2003 Yves Caseau. All Rights Reserved.         */
/**  cf claire.h                                                       */
/***********************************************************************/

#include <claire.h>
#include <Kernel.h>
//#include <marie.h>
#include <Core.h>
#include <Language.h>
#include <Reader.h>
#include <Optimize.h>
#include <Generate.h>

#ifdef CLPC
#include <conio.h>
#endif

// this is the stand_alone test file for the Claire microLibrary

/*********************************************************************/
/** Table of contents                                                */
/**    1. C++ main                                                   */
/**    2. System functions API                                       */
/**    3. toplevel                                                   */
/**    4. Inspection & debug                                         */
/*********************************************************************/


/*********************************************************************/
/**    1. C++ main                                                   */
/*********************************************************************/


void topLevel(meta_reader *r);
extern void call_main();


/* creates a protected list of arguments */
list *C_to_Claire(int argc, char *argv[])
{int i;
 list *l;
  GC_BIND;
  l = list::empty();
  for (i=1; i<argc; i++) l->addFast(_string_(argv[i]));  // v3.2
  return GC_OBJECT(list,l);
}

// this is our main function (a simpler version of what is in main.cpp)
main(int argc, char *argv[])
{int k = 1, i = 7, j = 11;
  if ((argc > 3) && (equal_string("-s",argv[1]) == CTRUE))
    { i = atoi(argv[2]); j = atoi(argv[3]);  k = 4;
      if ((i < 0) || (i > 20) || (j < 0) || (j > 20)) exit(1);
      //printf("increasing memory size by 2^%d and 2^%d \n",i,j);
    }
 {int t1,t2;
  ClAlloc = new ClaireAllocation;
  ClAlloc->logList = 18 + i;
  ClAlloc->maxList = (1 << (18 + i));
  ClAlloc->maxSize = (2 << (18 + i));
  ClAlloc->maxGC = 20000 * (1 << j);
  ClAlloc->maxStack = 8000 * (1 << j);                   // <yc> 9/98 changed because of GC
  ClAlloc->maxHist = 10000 * (1 << j);
  ClAlloc->maxEnv = 400 * (1 << j);
  ClAlloc->maxHash = ((1 << (11 + i)) + 200);       // <yc> 7/98 Changed for claire3 !
  ClAlloc->hashMask = ((1 << (11 + i)) - 1);
  ClAlloc->stdOut = ClairePort::make(stdout);
  ClAlloc->stdIn = ClairePort::make(stdin);
  ClAlloc->stdErr = ClairePort::make(stdout);
  ClaireResource::run();
  ClEnv->params = C_to_Claire(argc,argv);
  Reader.reader->fromp = ClAlloc->stdIn;  // special ?
  Reader.reader->nb_line = 0;
  Reader.reader->external = "toplevel";
  call_main();
  topLevel(Reader.reader);
  CL_exit(2);
  return 1;}}

/*********************************************************************/
/**    2. System functions API                                       */
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



/*********************************************************************/
/**    3. console top level                                          */
/*********************************************************************/

int NBEVAL = 0,MODE = 0;
list *STACK;                // history stack of commands in inspect mode
#define TOP 0               // 3 modes for top level
#define DEBUG 1
#define INSPECT 2

int store_i,store_b,store_d;
void loadInit();

/* top-level loop */
void topLevel(meta_reader *r)
{OID res = 0, stop = _oid_(Reader.q);
  while (res != stop)
     { princ_string(((MODE == TOP) ? 
                     string_I_symbol(ClEnv->module_I->name) :
                    ((MODE == DEBUG) ? "debug" : "inspect")));
       princ_string("> ");
       ClaireHandler c = ClaireHandler();
       if ERROR_IN { GC_BIND;
                     r->toplevel = CTRUE;
                     if (ClEnv->count_call > 0) ClEnv->count_call = 1;
                     res = nextunit_meta_reader(r);
                         if (MODE == TOP) ClEnv->index = 20;
                         if ((MODE == INSPECT) && (res != stop))
                             inspect_loop_any(res,STACK);
                         else {if (MODE == TOP) {princ_string("eval[");
                                        print_any(NBEVAL++);
                                        princ_string("]> ");} 
                            else princ_string("> ");
                            res = eval_any(GC_OID(res));
                            if (res != stop)
                               {print_any(res); ClEnv->cout->put('\n');}}
                       GC_UNBIND;
                       ERROR_FREE;}
        else {c.catchIt();
              if (ClEnv->exception_I->isa == Kernel._system_error &&
                  ((system_error *) ClEnv->exception_I)->index == -1)
                 {MODE = TOP; res = stop;}
              else {
               restore_state_meta_reader(r);
               ClEnv->cout = ClAlloc->stdOut;
               if (equal_string(r->external,"toplevel") == CFALSE)
                 {princ_string("---- file: "), princ_string(r->external);
                  princ_string(", line: "), princ_integer(r->nb_line);
                  ClEnv->cout->put('\n');}
                debug_if_possible_void();
                ClEnv->cout->put('\n'); }}
       if ((MODE != TOP) && (res == stop)) 
          {if (MODE == DEBUG) {ClEnv->index= store_i;
                               ClEnv->base= store_b;
                               ClEnv->trace_I = 1;
                               ClEnv->debug_I = store_d; }
           res = CNULL; MODE = TOP;}
     }
  exit(1); }
        
/* starts an inspector */
int InspectLoop(list *l)
{STACK = l;
 if (MODE == DEBUG) (ClEnv->trace_I = 1);
 MODE = INSPECT; 
 return 1;} 

 void loadInit()
{ClaireHandler c = ClaireHandler();
  if ERROR_IN {load_string("d:\\claire\\v2.9\\src\\make\\init.cl");
               ERROR_FREE;}
  else {c.catchIt();
         printf(">>>>>>>>>>>>>> please fix the init file ... \n");} }   

/* start a debug loop */
void DebugLoop()
{   store_d= ClEnv->debug_I;
    store_b= ClEnv->base;
    store_i= ClEnv->index;
    Reader.reader->toplevel = CTRUE;
    MODE = DEBUG;
    princ_string("--------------- Debug -------------------\n");
}
 

/*reads a character on the keyboard */
int StepLoop()
{int c;
  c = (int) getc(stdin);
  if (c != 10) getc(stdin);
  return c;}       

/*reads a string on the keyboad */
char *CommandLoopVoid()
{int i = 0;
 char c;
 static char buff[100];
   fflush(stdin);
   fflush(stdout);
   c = (char) getc(stdin);
   while (((c != '\n') && (c != '\r')) || (i == 0))
         {buff[i++] = c; c =  getc(stdin);}
   buff[i] = '\0';
   return buff;}  



// default main
void default_main()
{list *larg = ClEnv->params;
 int i = index_list(larg,_string_("-f"));
 if (i > 0)
     { ClaireHandler c_handle = ClaireHandler();
      if ERROR_IN 
        { load_string(string_v((*(larg))[i + 1]));
          ClEnv->cHandle--;}
      else if (belong_to(_oid_(ClEnv->exception_I),_oid_(Kernel._any)) == CTRUE)
        { c_handle.catchIt();
          restore_state_meta_reader(Reader.reader);
          debug_if_possible_void();
          }
      else PREVIOUS_HANDLER;} 
    } 
  

/*********************************************************************/
/**    4. Inspection & debug                                         */
/*********************************************************************/

// for debug
#define getADR(A) (((int) A - (int) &Cmemory[0]) >> 2)  // gets the ADR from the object
#define ADR(A) (A & ADR_MASK)         /* the address                   */


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
  else if (OWNER(n) == Kernel._float) princ_float(float_v(n));
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

void seeIt(char *s, OID x)
{printf("%s: ",s);
 see(x,1);
 printf("\n");}
    
void see(char *s, OID x) {see(s,x,1);}

