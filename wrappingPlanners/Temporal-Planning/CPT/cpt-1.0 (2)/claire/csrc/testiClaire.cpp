/***********************************************************************/
/**  microCLAIRE                                       Yves Caseau     */
/**  testiClaire.cpp                                                   */
/**  Copyright (C) 1994-99 Yves Caseau. All Rights Reserved.           */
/**  cf claire.h                                                       */
/***********************************************************************/

#include <claire.h>
#include <Kernel.h>
#include <marie.h>
#include <Core.h>
#include <Language.h>
#include <Reader.h>
#include <conio.h>


// this is the stand_alone test file for the Claire microLibrary

/*********************************************************************/
/** Table of contents                                                */
/**    1. system definition kernel                                   */
/**    2. System module interface                                    */
/**    3. tests sequences                                            */
/**    4. Inspection & debug                                         */
/*********************************************************************/

void topLevel(meta_reader *r);

/*********************************************************************/
/**    1. system definition kernel                                   */
/*********************************************************************/

void loadModules ()
{// module definitions
 Core.initModule("Core",mClaire.it,list::alloc(1,_oid_(Kernel.it)),
           "meta",
           list::alloc(4,_string_("method"),_string_("object"),
                         _string_("function"),_string_("types")));
 iClaire.initModule("iClaire",claire.it,list::alloc(1,_oid_(mClaire.it)),
              "", list::empty());
 Language.initModule("Language",iClaire.it,list::alloc(2,_oid_(Kernel.it),_oid_(Core.it)),
           "meta",
           list::alloc(4,_string_("pretty"),_string_("call"),
                         _string_("control"),_string_("define")));
 Reader.initModule("Reader",iClaire.it,list::alloc(3,_oid_(Kernel.it),
                                               _oid_(Core.it), _oid_(Language.it)),
            "meta",
           list::alloc(4,_string_("read"),_string_("syntax"),
                         _string_("file"),_string_("inspect")));
 // module load
 Core.metaLoad();
 Language.metaLoad();
 Reader.metaLoad();
 ClEnv->module_I = claire.it;
 /*printf("address de env %x -> %x \n", ClEnv,_oid_(ClEnv));
 printf("address de verbose %x \n",&(ClEnv->verbose));
 printf("address de exception %x \n",&(ClEnv->exception_I));
 printf("address de module %x \n",&(ClEnv->module_I));
 printf("address de name %x \n",&(ClEnv->name));
 printf("address de version %x \n",&(ClEnv->version));
 printf("address de ctrace %x \n",&(ClEnv->ctrace));
 printf("address de cout %x \n",&(ClEnv->cout));*/
}

/* creates a protected list of arguments */
list *C_to_Claire(int argc, char *argv[])
{int i;
 list *l;
  GC_BIND;
  l = list::empty();
  for (i=1; i<argc; i++) add_list(l,_string_(argv[i]));
  return GC_OBJECT(list,l);
}

// this is our main function (a simpler version of what is in main.cpp)
main(int argc, char *argv[])
{int k = 1, i = 5, j = 5;
  if ((argc > 3) && (equal_string("-s",argv[1]) == CTRUE))
    { i = atoi(argv[2]); j = atoi(argv[3]);  k = 4;
      if ((i < 0) || (i > 10) || (j < 0) || (j > 10)) exit(1);
      printf("increasing memory size by 2^%d and 2^%d \n",i,j);}
 {int t1,t2;
  ClAlloc = new ClaireAllocation;
  ClAlloc->logList = 18 + i;
  printf("Loglist = %d\n",ClAlloc->logList);
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
  printf("------------- Kernel version %g ------------------\n",CKernelRelease);
  msec(t1);
  ClaireResource::run();
  msec(t2);
  printf("------------- build complete in %d ms.\n", t2 - t1);
  ClEnv->params = C_to_Claire(argc,argv);   
  topLevel(Reader.reader);
  CL_exit(2);
  return 1;}}

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
void *CL_alloc(int n)
{int *x;
  x = (int *) malloc( (size_t) (n * sizeof(OID)));
  if (x == NULL)
  {printf("There is not enough memory for CLAIRE, try claire -s 0 0\n");
   for (n = 1; n < 20000000; n++ ) x = (int *) n;
   exit(0);}
  return x;}



/*********************************************************************/
/**    3. top level for iclaire                                      */
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
  r->fromp = ClAlloc->stdIn;  // special ?
  r->nb_line = 0;
  r->external = "toplevel";
  // ClEnv->verbose = 11;
  loadInit();
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
               ClEnv->cout->put('\n');
                 }}
       if ((MODE != TOP) && (res == stop)) 
          {if (MODE == DEBUG) {ClEnv->index= store_i;
                               ClEnv->base= store_b;
                               ClEnv->trace_I = 1;
                               ClEnv->debug_I = store_d; }
           res = CNULL; MODE = TOP;}
     }
 princ_string("Bye.\n");  
 exit(1); }
        
/* starts an inspector */
int InspectLoop(list *l)
{STACK = l;
 if (MODE == DEBUG) (ClEnv->trace_I = 1);
 MODE = INSPECT; 
 return 1;} 

 void loadInit()
{ClaireHandler c = ClaireHandler();
  if ERROR_IN {load_string("d:\\claire\\v2.9\\src\\kernel\\THEinit.cl");
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
 

/*********************************************************************/
/**    4. Inspection & debug                                         */
/*********************************************************************/


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

