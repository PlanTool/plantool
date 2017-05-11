/***********************************************************************/
/**   microCLAIRE                                       Yves Caseau    */
/**   clEnv.cpp                                                        */
/**  Copyright (C) 1998-2003 Yves Caseau. All Rights Reserved.         */
/**  cf claire.h                                                       */
/***********************************************************************/

#include <claire.h>
#include <Kernel.h>
#include <marie.h>

// This file contains the definition of the environment classes

/**********************************************************************/
/** Contents                                                          */
/**    1. exception handling functions                                */
/**    2. ClRes functions                                             */
/**    3. Evaluator + ClEnv functions                                 */
/**    4. Function Evaluation (stack_apply)                           */
/**    5. World functions                                             */
/**    6: Encoding & Miscellaneous (will move)                        */
/**********************************************************************/

double CKernelRelease = 0.1;

/***********************************************************************/
/**                 1: exception handling functions                    */
/***********************************************************************/

// create an exception - API function
OID close_exception(ClaireException *x)
{
  ClEnv->exception_I = x;
  longjmp(ClEnv->handlers[ClEnv->cHandle],3);
  return 0;}

// used during debug mode : safe exception 
OID safe_exception(ClaireException *x)
{ ClEnv->exception_I = x;
  longjmp(ClEnv->handlers[ClEnv->cHandle],3);
  return 0;}
  
// returns the index of the lexical base in the stack (constructor)
ClaireHandler::ClaireHandler()
{ if (ClEnv->cHandle >= ClAlloc->maxEnv) Cerror(33,0,0);
  ClEnv->cHandle++ ;                                           // v3.3.34 (Nicolas Museux)
  sIndex = ClEnv->index;
  sBase = ClEnv->base;
  debug = ClEnv->debug_I;
  trace = ClEnv->trace_I;
  gIndex = ClAlloc->index;
  gBase = ClAlloc->base;}

// restore a good state
void ClaireHandler::catchIt()
{ ClEnv->cHandle--;
  ClEnv->last_debug = ClEnv->debug_I;          // "last" values for debugger
  ClEnv->last_index = ClEnv->index;         // same
  ClEnv->index = sIndex;
  ClEnv->base = sBase;
  ClEnv->debug_I = debug;
  ClEnv->trace_I = trace;
  ClAlloc->index = gIndex;
  ClAlloc->base = gBase;}

// system error handling - C++ API ---------------------------------
int Cerror(int n, OID a, OID b)
{ if (n == -2) {ClEnv->cout = ClAlloc->stdErr;             // v3.3
                princ_string("Assertion violation in file "); princ_string((char *) a);
                princ_string(" [line "); princ_integer(b); princ_string(" \n");
                ClEnv->abort();}
  else if (n == -1) { ClEnv->cout = ClAlloc->stdErr;             // v3.3
                      princ_string("System Error: "); princ_string((char *) a);
                      princ_string(" \n");
                      ClEnv->abort();}
  else if (n == 36) safe_exception((ClaireException *) system_error::make(n,a,b));
  else close_exception((ClaireException *) system_error::make(n,a,b));
  return 0;}


// signal handling for C-C interupt   ---- old stuff that is really not great
#ifdef CLPC
void my_handler (int x)
{ princ_string("Aha ! ^C was entered on a PC ... Too bad\n");
  abort();}
#else
void my_handler (int x)
{ Cerror(34,0,0);}
#endif

// create a system_error
system_error *system_error::make(int n, OID x, OID y)
{system_error *o = (system_error *) ClAlloc->makeAny(4);
  o->isa = Kernel._system_error;
  o->index = n;
  o->value = x;
  o->arg = y;
  return o;}

/***********************************************************************/
/**                 2: System creation functions                       */
/***********************************************************************/

extern void loadModules();


// we now assume that a project/module file p.cpp is generated that contains the
// following functions:
// loadModules() which (a) build all the modules (fill the associated namespaces)
//                     (b) run the generated metaLoad() methods


// how to run the CLAIRE system
void ClaireResource::run()
{  ClAlloc->init();                  // memory  allocation
   ClRes->init();                    // initialization of the Resource object
   ClEnv->init();                    // initialization of the Environment object
   signal(SIGINT,  my_handler);
//   signal(SIGQUIT, my_handler);
   Kernel.metaLoad();                   // load the first module
   {ClaireHandler c = ClaireHandler();
      if ERROR_IN {loadModules();      // load all the other modules
                   GC_BIND;
                   ERROR_FREE;}
       else {c.catchIt();
             princ_string("\nAn error has occured in the compiled code ...\n");
             (*Kernel.print)(_oid_(ClEnv->exception_I));
             CL_exit(1);}}
    GC_UNBIND;
}


// initialization of ClRes
void ClaireResource::init()
{ cWorld = 0;
  cWorldId = 0;
  iIndex = 0;                   // base (previous top) for integer world stack
  iBase = 0;                    // base (previous top) for integer world stack
  oIndex = 0;                   // index (current top)
  oBase = 0;                    // base (previous top) for integer world stack
  fIndex = 0;                   // index (current top)
  fBase = 0;                    // index (current top)
  magicInit();}

// initialize the magic array through dynamic programming
// magicCode[i][j] tells how to encode the i-th son of a node using j bits
// this number of bits j is found with the magicNbits array: we can encode
// up to magicNbits[k] sons with k bits
void ClaireResource::magicInit()
{int k,i;
 int a[8] = {0,1,2,3,6,10,20,35};
 for (i = 1; i <= 7; i++) magicNbits[i] = a[i];
 for (k = 1; k <= 7; k++)
     for (i = 1; i <= magicNbits[k]; i++)
        {if (k <= 3) magicCode[i][k] = (1 << (i - 1));
         else if (k == 4)
            {if (i <= 3) magicCode[i][k] = 1 + (1 << i);
             else magicCode[i][k] = 2 * (7 - magicCode[i - 3][3]);}
         else if (k == 5)
            {if (i <= 4) magicCode[i][k] = 1 + (1 << i);
             else magicCode[i][k] = 2 * magicCode[i - 4][4];}
         else if (k == 6)
            {if (i <= 10) magicCode[i][k] = 1 + 2 * magicCode[i][5];
             else magicCode[i][k] = 2 * (31 - magicCode[i - 10][5]);}
         else {if (i <= 5) magicCode[i][k] = 3 + 2 * (1 << i);
               else if (i <= 15) magicCode[i][k] = 1 + 4 * magicCode[i - 5][5];
               else  magicCode[i][k] = 2 * (31 - magicCode[i - 15][6]);}}}

// use the previous array to return the fragment of bitcode for encoding the i-th
// child using m bits
int ClaireResource::makeCode(int n, int i, int m) {return (magicCode[i][m] << n);}

// generic hashing function: this is the heart of powerfull alists !
// The mask is supposed to be 0x001...1 and the result is between 0 and mask
int ClaireResource::hashOid(int mask, OID x)
{if IDENTIFIED(x)                    // v3.1.10 -> there was a strange bug with CTAG
    return (x & mask);
 else {ClairePrimitive *z = OBJECT(ClairePrimitive,x);
        if ((z->isa == Kernel._list) || (z->isa == Kernel._set) || (z->isa == Kernel._tuple)) // v3.2.16
          {ITERATE(j);
           int r = 0;
             for (START(((bag *) z)); NEXT(j);)  r = r + hashOid(mask,j);
             return r & mask;}
         else if (z->isa == Kernel._string)
              return claire.it->hash(string_v(x));
         else if (z->isa == Kernel._cl_import)              // v3.3.22
             return (((ClaireImport *) z)->value & mask);
         else if (z->isa == Kernel._float)
            return (((int) ((ClaireFloat *) z)->value) & mask);
         else return (x & mask);}
 }

/***********************************************************************/
/**        3: ClaireEnvironement functions                             */
/***********************************************************************/

void ClaireEnvironment::init()

{ cout = ClAlloc->stdOut;
  module_I = claire.it;
  cHandle = 0;                  // first handler pointer
  gensym = 0;
  tIndex = 0;                   // first time counter
  base = 0;                     // gc stack base
  trace_I = 0;
  debug_I = -1;                 // -1 means no debug
  index = 1;
  step_I = 0;
  count_trigger = 0;                    // v3.1.16 !!
  name = "Kernel";
  close = -1;                           // c: do not touch,   p:read only
  verbose = 0;
  ABSTRACT = 0;                         // c: no more instances
  FINAL = 1;                            // c: no more subclasses
  DEFAULT = 2;                          //
  open = 3;                             // p: open property (extensible)
                                        // c: use dynamic compiling
  ephemeral = 4;}                       // c:ephemeral objects


// creates a fatal exception
void ClaireEnvironment::abort()
{ close_exception(system_error::make(-1,0,0));}

// restore a good working state. m is the reader or 0
// used to be called clean_state -> 
void restore_state_void()
{ ClAlloc->index = 3;
  ClAlloc->base = 1;
  ClEnv->cout = ClAlloc->stdOut;}

// buffer functions ------------------------------------------------------------
// makes a string from the buffer
char *ClaireEnvironment::bufferCopy()
{int i;
 char *NEW = ClAlloc->makeString(1 + bLength / 4);        // v3.2.01 !
   for (i = 0; i < bLength; i++) NEW[i] = buffer[i];
   NEW[i] = '\0';
   return NEW;}

// prints an integer in the string buffer
void ClaireEnvironment::pushInteger(int n)
{ sprintf(&buffer[bLength],"%d",n);
  for ( ; buffer[bLength] != '\0'; bLength++);
  if (bLength > MAXBUF) Cerror(9,_string_(bufferCopy()),0);}

// make sure that a string is local
char *ClaireEnvironment::localString(char* s)
{return (CLMEM(s) ? s : copy_string(s)); }

/***********************************************************************/
/**        4: Function handling                                        */
/***********************************************************************/


// ----------- API functions -------------------------------------------

// retunrs the C value that we put in the stack (a function to avoid overNesting)
int Cpointer(OID x, ClaireClass *c) {return CPOINTER(x,c);}

// reset the stack
OID reset_stack(OID x, int n) { ClEnv->index -= n; return x;}

// moves the stack pointer up by x units */
void stack_add(int x)
{ClEnv->index = ClEnv->base + x;
 if (ClEnv->index >= ClAlloc->maxStack) Cerror(24,ClEnv->index,0);}

// apply a function on an argument, two arguments or more
OID fcall1(ClaireFunction *f,ClaireClass *s1,OID a1,ClaireClass *s)
{int x;
   x = ((fptr1) f->value)((int) CPOINTER(a1,s1));
   return CLAIREOID(x,s);}

OID fcall2(ClaireFunction *f,ClaireClass *s1, OID a1,ClaireClass *s2, OID a2,ClaireClass *s)
{int x;
   x = ((fptr2) f->value)((int)CPOINTER(a1,s1), (int) CPOINTER(a2,s2));
   return CLAIREOID(x,s);}

OID fcall3(ClaireFunction *f,ClaireClass *s1, OID a1,ClaireClass *s2,
           OID a2, ClaireClass *s3, OID a3, ClaireClass *s)
{int x;
   x = ((fptr3) f->value)((int)CPOINTER(a1,s1), (int) CPOINTER(a2,s2),
                          (int) CPOINTER(a2,s2));
   return CLAIREOID(x,s);}


// two macros to make the next function simpler !
#define AR(m) Cpointer(ClEnv->stack[n + m],OBJECT(ClaireClass,(*l)[1 + m]))
#define FAPPLY(F,f,l) (((F) f->value)l)


// apply a function to a list of arguments placed into the stack (m to n)*/
OID stack_apply_function(ClaireFunction *f, list *l, int n, int m)
{int x;
 m = m - n;
 ClaireClass *s = OBJECT(ClaireClass,(*l)[m + 1]);
 if (ClEnv->index >= ClAlloc->maxStack) Cerror(24,ClEnv->index,0);
 if (m == 1) x = FAPPLY(fptr1,f,(AR(0)));
 else if (m == 2) x = FAPPLY(fptr2,f,(AR(0),AR(1)));
 else if (m == 3) x = FAPPLY(fptr3,f,(AR(0),AR(1),AR(2)));
 else if (m == 4) x = FAPPLY(fptr4,f,(AR(0),AR(1),AR(2),AR(3)));
 else if (m == 5) x = FAPPLY(fptr5,f,(AR(0),AR(1),AR(2),AR(3),AR(4)));
 else if (m == 6) x = FAPPLY(fptr6,f,(AR(0),AR(1),AR(2),AR(3),AR(4),AR(5)));
 else if (m == 7) x = FAPPLY(fptr7,f,(AR(0),AR(1),AR(2),AR(3),AR(4),AR(5),AR(6)));
 else if (m == 8) x = FAPPLY(fptr8,f,(AR(0),AR(1),AR(2),AR(3),AR(4),AR(5),AR(6),AR(7)));
 else if (m == 9)
  x = FAPPLY(fptr9,f,(AR(0),AR(1),AR(2),AR(3),AR(4),AR(5),AR(6),AR(7),AR(8)));
 else if (m == 10)
  x = FAPPLY(fptr10,f,(AR(0),AR(1),AR(2),AR(3),AR(4),AR(5),AR(6),AR(7),AR(8),AR(9)));
 else if (m == 11)
  x = FAPPLY(fptr11,f,(AR(0),AR(1),AR(2),AR(3),AR(4),AR(5),AR(6),AR(7),AR(8),AR(9),AR(10)));
 else if (m == 12)
  x = FAPPLY(fptr12,f,(AR(0),AR(1),AR(2),AR(3),AR(4),AR(5),AR(6),AR(7),
                       AR(8),AR(9),AR(10),AR(11)));
 else if (m == 13)
  x = FAPPLY(fptr13,f,(AR(0),AR(1),AR(2),AR(3),AR(4),AR(5),AR(6),AR(7),
                       AR(8),AR(9),AR(10),AR(11),AR(12)));
 else if (m == 14)
  x = FAPPLY(fptr14,f,(AR(0),AR(1),AR(2),AR(3),AR(4),AR(5),AR(6),AR(7),
                       AR(8),AR(9),AR(10),AR(11),AR(12),AR(13)));
 else if (m == 15)
  x = FAPPLY(fptr15,f,(AR(0),AR(1),AR(2),AR(3),AR(4),AR(5),AR(6),AR(7),
                       AR(8),AR(9),AR(10),AR(11),AR(12),AR(13),AR(14)));
 else if (m == 16)
  x = FAPPLY(fptr16,f,(AR(0),AR(1),AR(2),AR(3),AR(4),AR(5),AR(6),AR(7),
                       AR(8),AR(9),AR(10),AR(11),AR(12),AR(13),AR(14),AR(15)));
 else if (m == 17)
  x = FAPPLY(fptr17,f,(AR(0),AR(1),AR(2),AR(3),AR(4),AR(5),AR(6),AR(7),
                       AR(8),AR(9),AR(10),AR(11),AR(12),AR(13),AR(14),AR(15), AR(16)));
 else if (m == 18)
  x = FAPPLY(fptr18,f,(AR(0),AR(1),AR(2),AR(3),AR(4),AR(5),AR(6),AR(7),
                       AR(8),AR(9),AR(10),AR(11),AR(12),AR(13),AR(14),AR(15), AR(16),
                       AR(17)));
 else if (m == 19)
  x = FAPPLY(fptr19,f,(AR(0),AR(1),AR(2),AR(3),AR(4),AR(5),AR(6),AR(7),
                       AR(8),AR(9),AR(10),AR(11),AR(12),AR(13),AR(14),AR(15), AR(16),
                       AR(17),AR(18)));
 else if (m == 20)
  x = FAPPLY(fptr20,f,(AR(0),AR(1),AR(2),AR(3),AR(4),AR(5),AR(6),AR(7),
                       AR(8),AR(9),AR(10),AR(11),AR(12),AR(13),AR(14),AR(15), AR(16),
                       AR(17),AR(18),AR(19)));
 else Cerror(32,_oid_(f),m);
 if (s == Kernel._void) x = CNULL; else x = CLAIREOID(x,s);
 ClEnv->index = n;
 return x;}

// the name of a function
char *string_I_function(ClaireFunction *f)
{ return f->name;}


/*********************************************************************/
/**    5. World functions                                            */
/*********************************************************************/

// defeasible update on a list
OID store_list(list *l, int n, OID y, ClaireBoolean *b)
{int *z = &(l->content[n]);
   if (b == CTRUE && ClRes->cWorld != 0) STOREOID(z,*z)
   return (*z = y);}

// defeasible update on a list
OID store_array(OID* a, int n, OID y, ClaireBoolean *b)
{if ARRAYFLOAT(a)
    {double *z = &(((double *) a)[n]);
     if (b == CTRUE && ClRes->cWorld != 0) STOREFLOAT(z,*z)
     return (*z = float_v(y));}
 else {int *z = &a[n];
       if (b == CTRUE && ClRes->cWorld != 0) STOREOID(z,*z)
       return (*z = y);}
}

// get the value, even if it is unknown
// we do not need to copy floats anymore since the stack has its own storage
OID store_object(ClaireObject *x, int n, ClaireClass *s, OID y, ClaireBoolean *b)
{if (s == Kernel._float)
    {double *z = (double *) SLOTADR(x,n);
     if (b == CTRUE && ClRes->cWorld != 0)  STOREFLOAT(z,*z)
     *z = float_v(y);
     return y;}
 else if (s == Kernel._any || s == Kernel._integer)
      {int *z = SLOTADR(x,n);
       if (b == CTRUE && ClRes->cWorld != 0)  STOREOID(z,*z)
       return (*z = y);}
 else {int *z = SLOTADR(x,n);
       if (b == CTRUE && ClRes->cWorld != 0)
          STOREOBJ(z,(ClaireObject *) *z)
       *z = ((y == CNULL) ? 0 : CPOINTER(y,s));
       return y;}}

// performs an addition to a list and store the relevant changes
// v3.3.06: this only works if enough memory has beed added beforehand
list *store_add(list *l,OID y)
{if (l->length + 1 == (*l)[0]) Cerror(43, _oid_(l), l->length);
 if (ClRes->cWorld) STOREOID(&(l->length),l->length)
 return l->addFast(y); }


// add one new world
void world_push ()
{if (ClRes->iIndex >= ClAlloc->maxHist || ClRes->oIndex >= ClAlloc->maxHist ||
     ClRes->fIndex >= ClAlloc->maxHist)
    Cerror(37,0,0);
 ClRes->cWorld++;
 ClRes->cWorldId++;                  // v3.2.04
 ClRes->haiStack[++ClRes->iIndex] = (int *) ClRes->iBase;
 ClRes->iBase = ClRes->iIndex;
 ClRes->haoStack[++ClRes->oIndex] = (int *) ClRes->oBase;
 ClRes->hvoStack[ClRes->oIndex] = NULL;
 ClRes->oBase = ClRes->oIndex;
 ClRes->hafStack[++ClRes->fIndex] = (int *) ClRes->fBase;
 ClRes->fBase = ClRes->fIndex;}

// remove a world and perform all modifications stored in the stack
void world_pop ()
{ClRes->cWorldId++;                       // v3.2.04
 if (ClRes->cWorld-- == 0) ClRes->cWorld++;
 else {int x = ClRes->iIndex + 1, y = ClRes->iBase;
       while (--x != y) {*(ClRes->haiStack[x]) = ClRes->hviStack[x];}
       ClRes->iIndex = y - 1;
       ClRes->iBase = (int) ClRes->haiStack[y];
       x = ClRes->oIndex + 1; y = ClRes->oBase;
       while (--x != y) {*(ClRes->haoStack[x]) = (int) ClRes->hvoStack[x];}
       ClRes->oIndex = y - 1;
       ClRes->oBase = (int) ClRes->haoStack[y];
       x = ClRes->fIndex + 1; y = ClRes->fBase;
       while (--x != y) {*( (double *) ClRes->hafStack[x]) = ClRes->hvfStack[x];}
       ClRes->fIndex = y - 1;
       ClRes->fBase = (int) ClRes->hafStack[y];}}


// commit: all updates are accepted but the traing info is conserved unless in world 1
void world_remove (void)
{if (ClRes->cWorld <= 1) world_slaughter();
 else { ClRes->cWorld--;
        if (ClRes->cWorld == -1) ClRes->cWorld++;
        int y = ClRes->iBase;
        ClRes->iBase = (int) ClRes->haiStack[y];       // v3.2.04: base(n - 1) is restored ...
        ClRes->haiStack[y] = &(ClRes->hviStack[y]);    // .. and the cell for base(n) is neutralized
        y = ClRes->oBase;
        ClRes->oBase = (int) ClRes->haoStack[y];
        ClRes->haoStack[y] = (int *) &(ClRes->hvoStack[y]);
        y = ClRes->fBase;
        ClRes->fBase = (int) ClRes->hafStack[y];
        ClRes->hafStack[y] = (int *) &(ClRes->hvfStack[y]);}}
       

// this is the tough version that is irreversible
void world_slaughter (void)
{if (ClRes->cWorld-- == 0)
    {ClRes->cWorld++; ClRes->iBase = 0; ClRes->iIndex = 0;  // yc: crude ... may be wrong
     ClRes->oBase = 0; ClRes->oIndex = 0;
     ClRes->fBase = 0; ClRes->fIndex = 0;}
 else  {int y = ClRes->iBase;
        ClRes->iIndex = y - 1;
        ClRes->iBase = (int) ClRes->haiStack[y];
        y = ClRes->oBase;
        ClRes->oIndex = y - 1;
        ClRes->oBase = (int) ClRes->haoStack[y];
        y = ClRes->fBase;
        ClRes->fIndex = y - 1;
        ClRes->fBase = (int) ClRes->hafStack[y];}}

// give the current world
int world_number (void)  {return ClRes->cWorld;}

// give the current world
int world_get_id (void)  {return ClRes->cWorldId;}

