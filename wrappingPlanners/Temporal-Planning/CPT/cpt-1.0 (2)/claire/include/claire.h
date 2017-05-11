/***********************************************************************/
/**   microCLAIRE                                       Yves Caseau    */
/**   claire.h                                                         */
/**  Copyright (C) 1998-99 Yves Caseau. All Rights Reserved.           */
/**  Redistribution and use in source and binary forms are permitted   */
/**  provided that source distribution retains this entire copyright   */
/**  notice and comments.                                              */
/**  THIS SOFTWARE IS PROVIDED AS IS AND WITHOUT ANY WARRANTY,         */
/**  INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF          */
/**  MERCHANTABILTY AND FITNESS FOR A PARTICULAR PURPOSE               */
/***********************************************************************/

// --------------------------------------------------------------------------
// this file is the general public header file for the CLAIRE system
// it contains the important macros, constants and system objects
// --------------------------------------------------------------------------

#ifndef __CLAIRE__
#define __CLAIRE__

#include <stdio.h>
#include <string.h>
#include <math.h>
#include <setjmp.h>
#include <stdlib.h>

// ***************************************************************************
// * Table of Contents:                                                      *
// * PART 1: Constants and OID structure                                     *
// * PART 2: CLAIRE system classes (resources)                               *
// * PART 3: CLAIRE class hierarchy                                          *
// * PART 4: CLAIRE Namespaces hierarchy                                     *
// * PART 5: API functions                                                   *
// ***************************************************************************

// ***************************************************************************
// * PART 1: Constants and OID structure                                     *
// ***************************************************************************

#define OID int                             /* to be changed if needed */

// we represent OIDs using 30bits of value and 2 bits of tags
// There are only two types of OID:
//  integer (TAG 0,1,2,3 & C,D,E,F), object (identifiable) and Primitive
//  object, which is used both for primitive and instantiated entities
//   Primitive are: imported (include array, string, port, float) [they are their own sort]
//             bags: lists and  sets
//             functions (external)      |

#define OBJ_CODE     0x40000000    /* objects                           */
#define NIET_CODE    0x80000000    /* things that DO NOT belong         */

#define ADR_MASK  0x0FFFFFFF  /* the bit mask to get the address        */
#define TAG_MASK  0xF0000000  /* the bit mask to get the tag            */
#define INT_MASK  0xC0000000  /* the first 2 bits */

// we need one value that is NOT a valid OID not the adress of a class
#define NOTHING    0x80000001      /* end of list                       */
// note that LTRUE, LFALSE and LUNKNOWN are replaced by claire.true, claire.false, claire.unknown

#define MAXBUF 50000          // v3.2.04

/** OID macros ---------------------------------------------------------*/

#define CTAG(x) (x & TAG_MASK)
#define INTEGERP(x) ((INT_MASK & x) != OBJ_CODE)            // assumes no niet !

#define OBJECT(A,B) ((class A *) &Cmemory[(B & ADR_MASK) + 1])
#define EXPORT(A,B) (A ((ClaireImport *) &Cmemory[(B & ADR_MASK) + 1])->value)
#define IDENTIFIED(x) (((x & TAG_MASK) != OBJ_CODE) ||\
                       (OBJECT(ClaireAny,x)->isa->ident_ask == CTRUE))
#define _oid_(A) (OBJ_CODE + (((int) A - (int) &Cmemory[0]) >> 2) - 1)
#define _array_(A) (OBJ_CODE + (((int) A - (int) &Cmemory[0]) >> 2) - 3)
#define _void_(x) (x,1)
#define _char_(c) (ClRes->ascii[(c & 0x000001FF)])     // NOT AN  OID !!
#define _function_(f,s) ClAlloc->makeFunction((fptr) f, s)
#define _string_(x) ClAlloc->import(Kernel._string,(int *) x)

#define float_v(n) (OBJECT(ClaireFloat,n)->value)
#define char_v(n) ((char) OBJECT(ClaireChar,n)->ascii)
#define string_v(B) ((char *) OBJECT(ClaireImport,B)->value)
#define array_v(A) ((OID *) &Cmemory[(A & ADR_MASK) + 3])


// these constant are used for the method status
// note that they are bitvectors whereas in CLAIRE they are indices
#define NEW_ALLOC (1 << 1)        // a new allocation may be done by running the method
#define BAG_UPDATE (1 << 2)       // a list is updated whose content is not gcsafe
#define SLOT_UPDATE (1 << 3)      // an slot is updated whose content is not gcsafe
#define RETURN_ARG (1 << 4)       // returns one of the input arg
#define SAFE_RESULT (1 << 5)      // the result (not gcsafe) does not need protection
                                  // mostly because it was protected internally
#define SAFE_GC (1 << 6)          // the arguments do not need to be protected although
                                  // an allocation may occur in the method, mostly because the
                                  // arguments are pushed on the stack !

/* declare function pointers for functions with arbitrary args */
#ifdef CLPC
typedef int (*fptr) (int);
#else
typedef int (*fptr) (...);
#endif

// forward definitions
class ClaireClass;
class ClaireChar;
class ClairePrimitive;
class ClaireException;
class ClaireFloat;
class ClaireFunction;
class ClaireImport;
class ClairePort;
class ClaireBoolean;
class ClaireObject;
class ClaireType;
class module;
class ClaireAny;
class bag;
class list;
class symbol;

extern int Cerror(int n, OID a, OID b);

// ***************************************************************************
// * PART 2: CLAIRE system resources                                         *
// ***************************************************************************

// resource object: manages the symbol table and the worlds
// the object ClRes is the unique instance and can be passed to CLAIRE runtime
//
class ClaireResource
{public:
   ClaireChar* *ascii;
   int   iBase;                         // start of current world in stack
   int   iIndex;                        // top of stack
   int*  *haiStack;                     // address part of the stack
   int   *hviStack;                     // value part of the stack
   int   oBase;                         // start of current world in stack
   int   oIndex;                        // top of stack
   int*  *haoStack;                     // address part of the stack
   ClaireObject*  *hvoStack;            // value part of the stack
   int   fBase;                         // start of current world in stack
   int   fIndex;                        // top of stack
   int*   *hafStack;                    // address part of the stack
   double *hvfStack;                    // value part of the stack
   int   cWorld;                        // current world number
   int   cWorldId;                      // v3.2.04: a unique ID for each world
   symbol* *sTable;                     // symbol hash table
   int magicCode[36][8];                // resource for tree encoding
   int magicNbits[8];                   // num of children that we can code with n bits

 static void run();
 void init();
 void magicInit();
 int makeCode(int n, int i, int m);
 int hashOid(int mask, OID x);

 };
   

// each handler is represented by a structure that contains the "environment" that
// must be restored
class ClaireHandler {
  int sIndex;          // keep the stack index
  int sBase;           // keep the stack base
  int debug;           // keep the debug info ????
  int trace;           // keep the trace info
  int gIndex;          // keep the gc stack index
  int gBase;           // keep the gc stack base

  public:
  ClaireHandler();     // constructor (use C++ alloc)
  void catchIt();      //
  };



// C++ allocator
class ClaireAllocation
{// private stuff
 int entryList[28];                     // keep track of free chunks by size

 int newChunk(int n);         // chunk allocation
 int newShort(int n);            // short object allocation
 int newLong(int n);
 int freeChunk(int n);           // free an unused chunk
 int freeLoop(int n);
 int freeSimple(int n,int size);
 int mergeRight(int a, int b, int size);
 int mergeLeft(int a, int b, int size);
 void freeObject(int n);
 void freeString(char *s);

 void markHash();                // mark the content of the hash table
 void markStack();               // mark the items in the various stacks
 void markPushed(int i);
 void mark(OID n);               // marks anything seen
 void markAny(ClaireAny *x);
 void markObject(ClaireObject *x);
 void markPrimitive(ClairePrimitive *x);
 void markBag(bag *s);
 void markString(char *s);
 void markArray(OID* a);
 void sweepChunk();
 void sweepObject();

 public:
 int statusGC;                          // 0:normal, 1:dump, 2:noGC
 int numGC;                             // num of calls to garbage collector
 ClaireAny *probe;                      // for debuging purposes
 int maxStack, maxHist, maxEnv,         // parameters for memory allocation
     maxGC, maxHash,maxList,maxSize;
 int hashMask;                          // hashing mask for symbol table
 int logList;                           // log size of chunk zone
 int usedCells;                         // book-keeping
 int firstFree;                         // current start of long free zone
 int alertFree;                         // current start of long free zone
 int nextFree;                          // start of the small object chain
 ClaireAny* *gcStack;                   // stack for protecting variable's content
 int index,base;                        // stack management variables
 ClairePort *stdIn,*stdOut,*stdErr;
 ClaireObject *currentNew;              // pointer to what is currently allocated

 // memory management
 static int log2up(int n);              // log - test in v3.2.38 use static methods properly ....
 ClaireAny *makeStatic(int n);          // static object allocation
 char *makeString (int n);              // string (char*) allocation
 OID *makeArray (int n, ClaireType *t); // array allocation
 OID *makeContent (int n);              // bag's content allocation (NOT THE BAG itself)
 ClaireAny *makeAny(int n);             // simple allocation for an object
 OID import(ClaireClass *c,int *n);     // imports into a claire OID
 // ClaireImport *makeImportString(char *s); // create a Claire String (an imported object)
 ClaireFloat *makeFloat();                       // allocates the memory for a ClaireFloat
 ClaireFunction *makeFunction(fptr f, char *s);  // creates a ClaireFunction
 void gc(char *s);                  // call the garbage collector
 void init();                       // allocate the memory zones
 void kill(OID n);                  // deallocates an ClaireAny
 void memStat();
 list *memList(ClaireBoolean *x);
};


// ***************************************************************************
// * PART 3: CLAIRE Namespaces hierarchy                                     *
// ***************************************************************************

// A namespace is a root class for the namespaces that we will generate for each module:
// if m is a CLAIRE module, we create
//   mClass, a subclass of NameSpace
//   m is a C++ instance of mClass, with a slot it that points to the
//   module object
class NameSpace
{public:
   module *it;                  // the module associated to the namespace
   void initModule(char *name, module *father);       // simple version for kernel
   void initModule(char *name, module *father,list* usage, char *dir, list *files);       // more complex version
};

// the root of all modules is represented directly with a global variable (claire)
// the "claire namespace" only owns a few constants (CTRUE,...),
// represented with C++ variables
extern NameSpace claire;
extern NameSpace mClaire;
extern ClaireBoolean *CTRUE;
extern ClaireBoolean *CFALSE;
extern OID CNULL;
extern ClaireResource *ClRes;
extern ClaireAllocation *ClAlloc;     // Alloc object
extern int *Cmemory;                  // memory zone


// ***************************************************************************
// * PART 4: API functions & macros                                          *
// ***************************************************************************

//   1. C++ functions --------------------------------------------------------
extern OID gc_error();

// 2:  MACROS  ------------------------------------------------------------------
// these are the public macros that are described in the microCLAIRE guide

#define CLREAD(A,B,C) ((class A *) B)->C
#define OWNER(x) (((INT_MASK & x) != OBJ_CODE) ? Kernel._integer : OBJECT(ClaireAny,x)->isa)
#define OPT_EVAL(x) (((INT_MASK & x) == OBJ_CODE) ? \
      ((fptr) OBJECT(ClaireAny,x)->isa->evaluate->value)((int) OBJECT(ClaireAny,x)) : x)

// OTRUEP(x) works on an OID and TRUEP(x) on a ClaireAny
#define TRUEP(x) (boolean_I_ClaireAny(x) == CTRUE)
#define OTRUEP(x) (boolean_I_any(x) == CTRUE)
#define FALSEP(x) (boolean_I_ClaireAny(x) == CFALSE)

#ifdef CLDEBUG
#define ASSERT(x) ((x) ? (int) NULL : Cerror(-2,(int) __FILE__,__LINE__))
#else
#define ASSERT(x) NULL
#endif


/** 2.b simplifies the generated code -----------------------------------*/

// put something on top of claire stack
// v3.2.18 VERY IMPORTANT CHANGE !!!!
#ifdef CLSMALL
#define INHERIT(self,ens) ((self->code & ens->code) == ens->code)
#else
#define INHERIT(self,ens) (ens->ancestors->length <= self->ancestors->length && \
                           self->ancestors->content[ens->ancestors->length] == _oid_(ens))
#endif

#define PUSH(A)  (ClEnv->stack[ClEnv->index++] = A)
#define BCONTAIN(s,x) (((s >> x) & 1) == 1)

/* macros for the GC protection */
#define GC_BIND ((ClAlloc->index < ClAlloc->maxGC) ? \
          (ClAlloc->gcStack[GC_DEBUG(ClAlloc->index)] = (ClaireAny *) ClAlloc->base, ClAlloc->base = ClAlloc->index++) \
           : gc_error())
#define GC_UNBIND  (ClAlloc->base = (int) ClAlloc->gcStack[(ClAlloc->index = ClAlloc->base)])


// GC_ANY is used to protect assignment -> does not return a typed value
#ifdef CPDEBUG     //debug version -> call to GC_DEBUG so that traces are possible
#define GC_ANY(x) (ClAlloc->gcStack[GC_DEBUG(ClAlloc->index++)] = x)
#define GC__ANY(x,y) (ClAlloc->gcStack[GC_DEBUG(ClAlloc->base + y)] = x)
#else
#ifdef CLWIN  // seems to work on Windows
#define GC_ANY(x) (ClAlloc->gcStack[ClAlloc->index++] = x)
#else         // a "safer form" for UNIX - use a function (x is evaluated first) - v3.3.34
//#define GC_ANY(x) GC_OBJ_F(x)                   // v3.3.36
#define GC_ANY(x) (ClAlloc->gcStack[ClAlloc->index++] = x)
#endif
#define GC__ANY(x,y) (ClAlloc->gcStack[ClAlloc->base + y] = x)
#endif

#define GC_LOOP (gc_local = ClAlloc->index)
#define GC_UNLOOP (ClAlloc->index = gc_local)

// save version of GC_ANY is re-introduced in v3.2.30
#define GC_PUSH(x) ((ClAlloc->index < ClAlloc->maxGC) ? GC_ANY(x) : (gc_error(), x))

// this is a type-safe version -> there is a special version for windows
// since visualC++ is not working properly
#ifdef CLWIN
#define GC_OBJECT(y,x) ((class y *) GC_OBJ_F((ClaireObject *) x))
#else
#define GC_OBJECT(y,x) ((class y *) GC_ANY(x))
#endif

/* macros for the debugger */
#define DB_BIND(m,p,n,l) if (m.it->status == 4) {l push_debug_property(p,n,ClEnv->index - n);}
#define DB_UNBIND(m,p,v) if (m.it->status == 4) pop_debug_property(p,1,v)

// macros for safe bag iterations (the bag does NOT change)
// {OID *CLcurrent,*CLlast,x;
//      for ( START(S); NEXT(x); )  ....  iterates over the bag S (no updates on S !)
// a safer way is  (to be used by the compiler)
//      for (i = 0;(i <= S.length & x = S.content[i]) ; )
#define ITERATE(x) OID *CLcurrent,*CLlast,x
#define START(S) (CLlast = &((CLcurrent = S->content)[S->length - 1]))
#define NEXT(x) (CLcurrent++ <= CLlast && (x = *CLcurrent, 1))
// #define LIST_OF(l) GC_PUSH(make_list_integer(NTH(l,0),0))

/* macros for exceptions */
#define ERROR_IN  (setjmp(ClEnv->handlers[ClEnv->cHandle]) == 0)
#define PREVIOUS_HANDLER  longjmp(ClEnv->handlers[--ClEnv->cHandle],3)
#define ERROR_FREE  (ClEnv->cHandle--)

// three storage macro for defeasible updates (depend on sort of y)
// integer and OIDs
#define STOREI(x,y) ((x != y) ? ((ClRes->cWorld != 0) ? \
        ((ClRes->iIndex++ <= ClAlloc->maxHist) ? \
         (x = (ClRes->hviStack[ClRes->iIndex] = \
                  *(ClRes->haiStack[ClRes->iIndex] = ((int *) &(x))), y)) : \
          Cerror(37,0,0)) : \
          (x = y)) : 0 )

// objects and primitive
#define STOREO(x,y) ((x != y) ? ((ClRes->cWorld != 0) ? \
        ((ClRes->oIndex++ <= ClAlloc->maxHist) ? \
         (x = (ClRes->hvoStack[ClRes->oIndex] = (ClaireObject *) \
                  *(ClRes->haoStack[ClRes->oIndex] = ((int *) &(x))), y)) : \
          (Cerror(37,0,0),y)) : \
          (x = y)) : y )

// floats
#define STOREF(x,y) ((x != y) ? ((ClRes->cWorld != 0) ? \
        ((ClRes->fIndex++ <= ClAlloc->maxHist) ? \
         (x = (ClRes->hvfStack[ClRes->fIndex] = \
                  *( (double *) (ClRes->hafStack[ClRes->fIndex] = ((int *) &(x)))), y)) : \
          Cerror(37,0,0)) : \
          (x = y)) : y )

// new in v3.2: give the address of a slot
extern int *CL_dummy;
#define CL_IDX(c,s) (1 + ((((int) &(((c *) CL_dummy)->s)) - (int) CL_dummy) / sizeof(OID)))  
#define CL_ADDSLOT(c,CC,p,r,v) c->addSlot(p,r,v,CL_IDX(CC,p))  
#define CL_ADD_SLOT(c,CC,p,PP,r,v) c->addSlot(p,r,v,CL_IDX(CC,PP))    

// temporary
#define CASTOBJECT(A,B) ((A *) B)

extern void symbolDebug(symbol *s);
extern void seeIt(char *s, OID x);

// macros for reading
#define CHANGE(x,y) ((x != y) ? ((x = y), 1) : 0)
#define KNOWN(p,y) ((y == CNULL) ? (Cerror(38,_oid_(p),0),y) : y)
#define NOTNULL(p,y) ((y == NULL) ? (Cerror(38,_oid_(p),0),y) : y)

#endif
