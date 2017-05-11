/*****C******************************************************************/
/**   microCLAIRE                                       Yves Caseau    */
/**   marie .h                                                         */
/**  Copyright (C) 1998-99 Yves Caseau. All Rights Reserved.           */
/**  Redistribution and use in source and binary forms are permitted   */
/**  provided that source distribution retains this entire copyright   */
/**  notice and comments.                                              */
/**  THIS SOFTWARE IS PROVIDED AS IS AND WITHOUT ANY WARRANTY,         */
/**  INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF          */
/**  MERCHANTABILTY AND FITNESS FOR A PARTICULAR PURPOSE               */
/***********************************************************************/

// this file contains the private header for the Kernel module

#include <signal.h>
#include <stdarg.h>
#ifdef CLMAC
#include <stdlib.h>
#include <types.h>
#include <time.h>
#define SIGQUIT SIGINT
#endif
#ifdef CLPC
#define SIGQUIT SIGINT
#include <time.h>
#endif
#ifdef CLUNIX
// #define CLFLPAR 1               // LINUX requires this flag: cf clReflect.cpp
#include <time.h>               // with LINUX, #include <time.h> works  ...
// #include <sys/types.h>       // ... but on some other UNIX OSs it does not !!!!
// #include <sys/times.h>       // this may prove useful
// #define CLOCKS_PER_SEC 100   // sometimes this is needed !!!!
#endif
#ifdef CLOSX
#include <time.h>
#endif

/***********************************************************************/
/** Contents                                                           */
/**    1. Constants                                                    */
/**    2. Structures                                                   */
/**    3. Macros                                                       */
/***********************************************************************/

/***********************************************************************/
/**    1. Constants                                                    */
/***********************************************************************/

/* in some machines (compilers) RAND is not be defined (appears to be true
   for both cc and g++ on sun. Note the actual constant may be machine
   dependent */
#ifndef RAND_MAX
#define RAND_MAX 0x7FFFFFFF
#endif

extern double CKernelRelease;

/************************************************************************/
/**    3. Macros & external functions                                   */
/************************************************************************/


#define msec( t )  {double x; \
x = ((double) clock()) * 1000.0 / ((double) CLOCKS_PER_SEC); t = floor(x); }


#define ADR(A) (A & ADR_MASK)         /* the address                   */


/* tells where the string is allocated */
#define CLMEM(s) (((int) s > (int) &Cmemory[0]) && ((int) s < (int) &Cmemory[ClAlloc->maxSize]))

// these are the two macros to convert between the CLAIRE & C++ representation

#define CLAIREOID(x,s) ((s == Kernel._object) ? _oid_(x) : \
                       ((s == Kernel._any) ? x : \
                       ((s == Kernel._integer) ? _integer_((int) x) : \
                       ((s == Kernel._array) ? _array_(x) : \
                       ((s == Kernel._void) ? CNULL : ClAlloc->import(s,(int *) x))))))

#define CPOINTER(x,s) ((s == Kernel._object) ? ((int) &Cmemory[(x & ADR_MASK) + 1]) : \
                      ((s == Kernel._any || s == Kernel._integer) ? x : \
                      ((s == Kernel._array) ? ((int) array_v(x)) : \
                      ((s == Kernel._void) ? NULL : OBJECT(ClaireImport,x)->value))))

// read the n-th slot of the object x
#define SLOTADR(x,n) ((int *) x + (n - 1))
// no longer used #define SORT(r) ((class ClaireClass *) Lmemory[ADR(r) + 7])

#define ARRAYTYPE(a)  ((ClaireType *) a[-1])
#define ARRAYLENGTH(a)  a[0]
#define ARRAYFLOAT(a) (a[-1] == (int) Kernel._float)


// we now have three macros STOREOID, STOREOBJ and STOREFLOAT
// STOREOID(x,y)  does the book-keeping so that the value y (an OID) for x (a container) in remembered
// for a defeasible update
#define STOREOID(x,y) {ClRes->haiStack[++ClRes->iIndex] = x; ClRes->hviStack[ClRes->iIndex] = y;\
                       if (ClRes->iIndex >= ClAlloc->maxHist) Cerror(37,1,0); }

// same when y is an object (X*) and x a containter
#define STOREOBJ(x,y) {ClRes->haoStack[++ClRes->oIndex] = x; ClRes->hvoStack[ClRes->oIndex] = y;\
                       if (ClRes->oIndex >= ClAlloc->maxHist) Cerror(37,2,0); }

#define STOREFLOAT(x,y) {ClRes->hafStack[++ClRes->fIndex] = (int *) x;\
                         ClRes->hvfStack[ClRes->fIndex] = y;\
                         if (ClRes->fIndex >= ClAlloc->maxHist) Cerror(37,3,0); }

// methods that are defined in the CLAIRE code (a dumb equivalent exists in the test file)
extern ClaireType *of_extract_type(ClaireType *t);
extern void push_debug_property(property *p,int n, int m);
extern void pop_debug_property(property *p,int n,OID v);
extern ClaireClass *class_I_type(ClaireType *t);
extern void insert_definition_property(property *p,restriction *s);
extern ClaireObject* find_which_class(ClaireClass *c,list *l,int i, int j);
extern ClaireObject* find_which_property(property *p,int i, ClaireClass *c);
extern OID eval_message_property(property *p,ClaireObject *r,int i,ClaireBoolean *b);
extern method *inlineok_ask_method(method *m, char *s);
extern ClaireObject * hashget_class(ClaireClass *c,property *p);


extern void see(OID x);
extern void see(OID x, int i);
extern void see(char *c,OID x,int i);
extern void see(char *c,OID x);

// for debug
#define getADR(A) (((int) A - (int) &Cmemory[0]) >> 2)  // gets the ADR from the object
#define _chunk_(A) (NIET_CODE + getADR(A))              // makes a chunk address into an OID

// stupid list but necessary for true C++ portability ------------------
// note that we got rid of half of this junk by being optimistic .....
typedef int (*fptr1) (int);
typedef int (*fptr2) (int,int);
typedef int (*fptr3) (int,int,int);
typedef int (*fptr4) (int,int,int,int);
typedef int (*fptr5) (int,int,int,int,int);
typedef int (*fptr6) (int,int,int,int,int,int);
typedef int (*fptr7) (int,int,int,int,int,int,int);
typedef int (*fptr8) (int,int,int,int,int,int,int,int);
typedef int (*fptr9) (int,int,int,int,int,int,int,int,int);
typedef int (*fptr10) (int,int,int,int,int,int,int,int,int,int);
typedef int (*fptr11) (int,int,int,int,int,int,int,int,int,int,int);
typedef int (*fptr12) (int,int,int,int,int,int,int,int,int,int,int,int);
typedef int (*fptr13) (int,int,int,int,int,int,int,int,int,int,int,int,int);
typedef int (*fptr14) (int,int,int,int,int,int,int,int,int,int,int,int,int,int);
typedef int (*fptr15) (int,int,int,int,int,int,int,int,int,int,int,int,int,int,int);
typedef int (*fptr16) (int,int,int,int,int,int,int,int,int,int,int,int,int,int,int,int);
typedef int (*fptr17) (int,int,int,int,int,int,int,int,int,int,int,int,int,int,int,int,int);
typedef int (*fptr18) (int,int,int,int,int,int,int,int,int,int,int,int,int,int,int,int,int,int);
typedef int (*fptr19) (int,int,int,int,int,int,int,int,int,int,int,int,int,int,int,int,int,int,int);
typedef int (*fptr20) (int,int,int,int,int,int,int,int,int,int,int,int,int,int,int,int,int,int,int,int);


// used to compile properly the system profiling methods defined in ClPort
// note that this definition is produced in Reader.h
class PRcount: public ClaireObject{ 
  public:
     int rtime;
     int rdepth;
     int rnum;
     int rloop;
     int rstart;};


