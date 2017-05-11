/***********************************************************************/
/**   microCLAIRE                                       Yves Caseau    */
/**   clAlloc.cpp                                                      */
/**  Copyright (C) 1998-2003 Yves Caseau. All Rights Reserved.         */
/**  cf claire.h                                                       */
/***********************************************************************/

#include "claire.h"
#include "Kernel.h"
#include "marie.h"

// for debug, to remove
void checkNextFree()
{ if (ClAlloc->nextFree != NOTHING && ClAlloc->nextFree > ClAlloc->maxSize)
     printf("== bug because of cell %d\n",ClAlloc->nextFree);
}
      

// this is the allocator file. We use do not use C++ new and delete, so that
// we can allocate everything with a same reference address (&Cmemory[0])
// which is necessary for OIDs. This is slower than system allocation for
// chunks but actually faster for small items

/*********************************************************************/
/** Table of contents                                                */
/**    1. Interface methods                                          */
/**    2. Allocation methods                                         */
/**    3. De-allocation methods                                      */
/**    4. Garbage Collection                                         */
/**    5. Interface functions                                        */
/*********************************************************************/

int BadGuy = 0;      // a remettre a 0 !;
int maxIndex = 0;
int *CL_dummy;       // not used really

// debug version that can be instrumented
int GC_DEBUG(int n)
  { return n;}

// we use a special object to represent the allocator
// we could use a class with static methods but this is more homogeneous
ClaireAllocation *ClAlloc;
int *Cmemory;

// macros for this file -------------------------------------------------
#define CAR 1
#define FOLLOW 2
#define PREVIOUS 3
// x is a pointer, returns the ADR (faster than ADR(_oid_(x))
#define POINTOADR(x) (((int) x - ((int) &Cmemory[0])) / sizeof(int))
#define SIZE(n) Cmemory[ADR(n)]             // returns the size of the object

#define OPTIMIZE 6          /*size of the basic object (has to be even) */

#ifdef CLDEBUG              // little trick to check that values are OK
int ISCHUNK(int n)
{if (n == NOTHING || (n > 0 && n < ClAlloc->maxList)) return n;
 else return (n / 0);}
#else
#define ISCHUNK(n) n
#endif

/*********************************************************************/
/**    1. Interface methods                                         */
/*********************************************************************/

// init
// contains all the calls to malloc for CLAIRE
void ClaireAllocation::init()
{int i;
 ClRes = new ClaireResource;
 ClRes->sTable = (symbol **) CL_alloc(maxHash + 1);
 ClRes->ascii = (ClaireChar **) new int[512];
 ClRes->haiStack = (int **) CL_alloc(maxHist);                    // address part of the stack
 ClRes->hviStack = (int *) CL_alloc(maxHist);                     // value part of the stack
 ClRes->haoStack = (int **) CL_alloc(maxHist);                    // address part of the stack
 ClRes->hvoStack = (ClaireObject **) CL_alloc(maxHist);           // value part of the stack
 ClRes->hafStack = (int **) CL_alloc(maxHist);                    // address part of the stack
 ClRes->hvfStack = (double *) CL_alloc(maxHist * 2);              // value part of the stack
// gcStack = (ClaireAny **) new int[maxGC];                       // GC stack
// Cmemory = new int[maxSize + 1];                                // global memory zone
 gcStack = (ClaireAny **) CL_alloc(maxGC);                        // GC stack  (v 3.2.38)
 Cmemory = (int *) CL_alloc(maxSize + 1);                         // global memory zone
 if (Cmemory == NULL) CL_exit(0);
 #ifdef CLUNIX
 Cmemory = &(Cmemory[1]);                                         // respect alignment !
 #endif
 // for ( int i = 0; i < maxSize; i++) Cmemory[i] = i + 1;  try to live without it
 nextFree = NOTHING;
 firstFree = maxList + 2;    // needs to be even for alignment ! used in sweep
 alertFree = maxList + ((5 * (maxSize - maxList)) / 6);
 statusGC = 0;              // we like GC, don't we ?
 for (i=0; i <= maxHash; i++) ClRes->sTable[i] = NULL;
 hashMask = ((1 << (log2up(maxHash) - 1)) - 1);
 // initialization of the Chunk allocation mechanisms: entryList and chained lists (FOLLOW,PREVIOUS)
 for (i = 0; i < logList; i++) entryList[i] = NOTHING;
 entryList[logList] = 1;
 Cmemory[1] = (1 << logList); Cmemory[1 + CAR] = NOTHING;
 Cmemory[1 + PREVIOUS] = NOTHING; Cmemory[1 + FOLLOW] = NOTHING;
 usedCells = 0;
 base = 0;                     // gc stack base
 index = 1;
 probe = NULL;                // v3.0.4
 ClEnv = (ClaireEnvironment *) makeStatic(sizeof(ClaireEnvironment) / 4 + 2);
 ClEnv->handlers = (jmp_buf *) CL_alloc( (size_t) (maxEnv * sizeof(jmp_buf))) ;  // v3.3.34 - a buf fix for FXJ
 ClEnv->stack = (int *) CL_alloc(maxStack);                        // eval stack
 currentNew = NULL;                                                // v3.0.60
 numGC = 0;                                                        // v3.2.50
}


/*********************************************************************/
/**    2. Allocation methods                                         */
/*********************************************************************/

// note: new* methods return an integer value relative to &Cmemory[O]
//       make* methods return a true pointer

// roughly the logarithm of n with base 2, n = 0 is an horror !
int ClaireAllocation::log2up(int n)
{int value;
  for (value = 1; (n != 1);) {n = n >> 1; ++value;}
  return(value);
}

// allocate the space a chunk of n items
// entryList[size] is either a free chunk or NOTHING (free chunks are chained)
// in that case, we need to cut a larger chunk into two pieces (recursively)
// if needed we call the garbage collector to free more chunks
// A free chunk i is organized as follow
//     Cmemory[x + CAR]   = NOTHING  (to know that the chunk is free)
//     Cmemory[x + NEXT]  = address of next free chunk of same size
//     Cmemory[x + FOLLOW] = address of the previous chunk in the chain
int ClaireAllocation::newChunk(int n)
{int size,value,next,i,j;
  size = log2up(n);                         /* note that 2^size is always > n */
  if (size > logList) Cerror(1,size,0);
  if (entryList[size] == NOTHING)
     { for (i = size; (entryList[i] == NOTHING); i++) if (i > logList) break;
       for (j = i + 1; j <= logList; j++) if (entryList[j] != NOTHING) break;
       if ((i > logList) || (i < logList - 1 && j > logList))  // new anti-fragmentation device
          {gc("Chunk");                // we have not found a free (larger) chunk
           if (entryList[size] != NOTHING)
              {value = entryList[size];
               entryList[size] = ISCHUNK(Cmemory[value + FOLLOW]);
               return value;}
           for (i = size + 1; (entryList[i] == NOTHING); i++) if (i > logList) break;
           if (i > logList) Cerror(2,n,0);}     // fails after GC -> no more memory
       value = entryList[i];                    // we have a chunk of size i
       entryList[i] = ISCHUNK(Cmemory[value + FOLLOW]);  // update next free chunk
       for (j = i; (j != size); j--)            // cuts into two until right size
           {next = value + (1 << (j-1));
            Cmemory[next] = (1 << (j - 1));
            Cmemory[next + FOLLOW] = NOTHING;
            Cmemory[next + CAR] = NOTHING;
            entryList[j - 1] = ISCHUNK(next);}
       Cmemory[value] = (1 << size);}
   else
     { value = entryList[size];
        entryList[size] = ISCHUNK(Cmemory[value + FOLLOW]);}
   usedCells += Cmemory[value];                 // book-keeping
  #ifdef CLDEBUG              // useful
   if (value == BadGuy)
     printf("====== allocate bad guy \n");
  #endif
  return value;}

// allocation of a short object: this is much simpler and uses a chain of small blocks
// of size OPTIMIZE
int ClaireAllocation::newShort(int n)
{int value;
  if (nextFree != NOTHING)                     // chain is not empty
     {value = nextFree;
      Cmemory[value] = n;
      nextFree = Cmemory[nextFree+1];
      #ifdef CLDEBUG              // little trick to check that values are OK
      checkNextFree();
      #endif
      }
  else {if (firstFree > alertFree)                                         // chain is empty
         {gc("Object");
          if (nextFree != NOTHING) value = newShort(n);
          else value = newLong(OPTIMIZE);}                                 // v3.3.34 (thanks to Sylvain) !
        else {value = firstFree;
              Cmemory[value] = n;
              firstFree += OPTIMIZE;}}
  #ifdef CLDEBUG              // useful
  if (value == BadGuy)
    printf("====== allocate bad guy \n");
  #endif
  return value;}


// allocation of a long object is pure stacking
int ClaireAllocation::newLong(int n)
{int value;
  value = firstFree;
  Cmemory[firstFree] = n;
  firstFree += (n + 1);
  if (firstFree > maxSize) Cerror(3,n,0);
  return value;}

// allocation of a long object is pure stacking
ClaireAny *ClaireAllocation::makeStatic(int n)
{return (ClaireAny *) &Cmemory[newLong(((n < OPTIMIZE) ? OPTIMIZE : n)) + 1];}

// allocator for our own use
ClaireAny *ClaireAllocation::makeAny(int n)
{int m = ((n < OPTIMIZE) ? newShort(n) : newChunk(n));
 // if (ALTALK > 0) printf("ClAlloc::makeAny(%d) -> %x (ADR = %d) \n",n, ((int *) &Cmemory[m + 1]), m);
 return (ClaireAny *) &Cmemory[m + 1];
}
// return (ClaireAny *) &Cmemory[1 + ((n < OPTIMIZE) ? newShort(n) : newChunk(n))];}


// we allocate the string so that it looks like an imported but is is really special
// string is an import on a char* that is itself allocated by CLAIRE (with a primitive
// isa marking)
char *ClaireAllocation::makeString (int n)
{int m;
  if (n < OPTIMIZE) n = OPTIMIZE;
  m = newChunk(n+3);
  Cmemory[m+1] = (int) Kernel._primitive;     // allows for a simpler string pushing
  // test that the future code will work
   {char *s = (char *) &Cmemory[m+2];
    int x = POINTOADR(s) - 1;         // works because the class is the string marker NOTE: -1 to get the object !
      ClaireObject *y = (ClaireObject *) &Cmemory[x];
      ClaireClass *c = y->isa;
      ASSERT(c == Kernel._primitive);}

  return (char *) &Cmemory[m+2];}

// similar trick: allocation for an array
// we allocate both the container and the contents thus
OID *ClaireAllocation::makeArray (int n, ClaireType *t)
{int m = newChunk(((t == Kernel._float) ? (2 * n) : n) + 4); // v3.2.34 : 4 vs. 3 is necessary for some strange reason
  Cmemory[m+1] = (int) Kernel._array;
  Cmemory[m+2] = (int) t;
  Cmemory[m+3] = n;
  return (OID *) &Cmemory[m + 3];}

// allocate a memory zone for a bag
OID *ClaireAllocation::makeContent (int n)
{int x;
  if (n < (OPTIMIZE - 1)) x= newShort(OPTIMIZE-1);
  else x = newChunk(n);
  return &Cmemory[x];}

// creates an imported object
// notice the automatic protection, which is why the compiler does not protect constants from gc,
// since further optimization are meant to remove the use of import
OID ClaireAllocation::import(ClaireClass *c, int *x)
{if (c == Kernel._string) GC_STRING((char *) x);               // v3.2.01 -> protect the content
 int adr = newShort(3);
 ClaireImport *obj = (ClaireImport *) &Cmemory[adr + 1];
   if (ClAlloc->statusGC != 2)  GC_PUSH(obj);                   // v3.1.06 -> protect the container, v3.2.30 check stack
   obj->isa = c;
   obj->value = (int) x;
   return OBJ_CODE + adr;}

// a function is a system object (sort = object) allocated with a special
// method 
ClaireFunction *ClaireAllocation::makeFunction(fptr f,char* s)
{ ClaireFunction *obj = (ClaireFunction *) &Cmemory[newShort(4) + 1];
    obj->isa = Kernel._function;
    obj->value = (int) f;
    obj->name = s;
    return obj;}

// creates a function without a body
OID error_undefined_function(void)
{Cerror(31,0,0); return 1;}


ClaireFunction *make_function_string(char *s)
{return ClAlloc->makeFunction((fptr) error_undefined_function,s);}

/***********************************************************************/
/**    3. Memory De-allocation   ---------------------------------     */
/***********************************************************************/


//  de-allocation of a chunk of dynamic memory,
//  the returned value is important, it is the address of the next chunk for the
//  sweep loop
int ClaireAllocation::freeChunk(int n)
{int size,l,j;
  l = Cmemory[n];             /* real length of the allocated space */
  size = log2up(l) - 1;
  #ifdef CLDEBUG              // useful
  if (l != (1 << size) )
     {printf("free ADR %d causes a bug (size = %d not a chunk)\n",n,l);}
  #endif
  usedCells -= l;
  if (Cmemory[n + 1] == (int) Kernel._primitive || Cmemory[n + 1] == (int) Kernel._array )
     for(j=2; j< l; j++) Cmemory[n+j] = 0;        // CLEAN ARRAYS OR STRINGS !
  ASSERT(l == (1 << size));
  return freeLoop(n);}

// we first check if it can be merged with its twin
//  if yes we do it, otherwise we just free the space
// a twin is the adjacent chunk of the same size, which can be on the left or the
// right
int ClaireAllocation::freeLoop(int n)
{int size,l;
  l = Cmemory[n];             // real length of the allocated space
  size = log2up(l) - 1;
  if (((n >> size) & 1) == 0) // determines if the twin is on the right
     {if ((Cmemory[n + l] == l) & (Cmemory[n + l + CAR] == NOTHING))
          return mergeRight(n, n + l, size);
      else return freeSimple(n,size);}
  else                       // twin is on the left
     {if ((Cmemory[n - l] == l) & (Cmemory[n - l + CAR] == NOTHING))
          return mergeLeft(n - l, n, size);
      else return freeSimple(n,size);}}

// giving back a list to the data zone
int ClaireAllocation::freeSimple(int n, int size)
{ if (entryList[size] != NOTHING) Cmemory[entryList[size] + PREVIOUS] = n;
  Cmemory[n + CAR] = NOTHING;
  Cmemory[n + FOLLOW] = entryList[size];
  Cmemory[n + PREVIOUS] = NOTHING;
  entryList[size] = n;
  return n;
}

// merging two twins free lists , a is the new one
int ClaireAllocation::mergeRight(int a, int b, int size)
{
  if (entryList[size] == b)    /*b is the first free of this size */
      entryList[size] = Cmemory[b+FOLLOW];
  else
     { Cmemory[Cmemory[b+PREVIOUS] + FOLLOW] = Cmemory[b+FOLLOW];
       if (Cmemory[b+FOLLOW] != NOTHING)
         Cmemory[Cmemory[b+FOLLOW] + PREVIOUS] = Cmemory[b+PREVIOUS];}
  Cmemory[a] = Cmemory[a] * 2;
  return freeLoop(a);
}

// symetrical: b is the new one
int ClaireAllocation::mergeLeft(int a, int b, int size)
{
  if (entryList[size] == a) /* a is the first free cell */
     entryList[size] = Cmemory[a+FOLLOW];
  else
     { Cmemory[Cmemory[a+PREVIOUS] + FOLLOW] = Cmemory[a+FOLLOW];
       if (Cmemory[a+FOLLOW] != NOTHING)
           Cmemory[Cmemory[a+FOLLOW] + PREVIOUS] = Cmemory[a+PREVIOUS];}
  Cmemory[a] = Cmemory[a] * 2;
  return freeLoop(a);
}

//  desallocation if any need, (instruction object) of the memory zone
void ClaireAllocation::freeObject(int n)
{ if (Cmemory[n] < OPTIMIZE)
     {Cmemory[n+1] = nextFree;
      Cmemory[n] = 0;
      #ifdef CLDEBUG              // useful - v3.3.4 : make a CLDEBUG option
      if (n == BadGuy)
         printf("====== free bad guy -> %d\n", nextFree);
      #endif
      nextFree = n;
      #ifdef CLDEBUG              // useful
      checkNextFree();
      #endif
      }
}

// free a string
void ClaireAllocation::freeString(char *s)
{ if CLMEM(s)
   {int x = POINTOADR(s) - 2;
    freeChunk(x);}}


/*********************************************************************/
/**    3. Garbage Collection                                         */
/*********************************************************************/

// mark a cell ! [v3.1.04 use a macro => easier to read + debug]
#define MARKCELL(x) (Cmemory[x] = -Cmemory[x])

void freeChain()
{int i = ClAlloc->nextFree, j = 0, prev = 0;
   while (i != NOTHING )
   {if (i > ClAlloc->maxSize)
      printf("[%d] Chaine buggee %d -> %d \n",j,prev,i);
    j++;
    prev = i;
    i = Cmemory[i + 1];
   }
 printf("Free chain OK: %d members\n",j);
}

// call the garbage collector
// this is a classical maek&sweep designed that has shown to be robust & fast
void ClaireAllocation::gc(char *cause)
{ClairePort *p = ClEnv->cout;
 ClEnv->cout = stdOut;
 // freeChain();
 // printf("start GC with stack index = %d, gc index = %d\n",ClEnv->index, ClAlloc->index);
 if (probe != NULL) {printf("--- the probe is %d -> adr = %d ----\n",probe,getADR(probe));
                     printf("*[O] = %d, 1:%d, 2:%d\n",probe[1],probe[2],probe[3]);}
 // use_as_output_port(p);
 if (ClEnv->verbose > 0 || (numGC % 10) == 0 || 1 == 1)
    {if (statusGC == 2)
	    {
		princ_string(cause); princ_string(" ["); princ_integer(numGC);
     		princ_string("] not enough memory !\n");        // v3.2.34
	    }
     }
 if (statusGC == 2) Cerror(27,0,0);                                     // v3.1.12  -> no GC allowed !
 else {int topStack = ClEnv->index;
       numGC++;
       markStack();
       markHash();
       markPushed(topStack);
       sweepChunk();
       sweepObject();
       ClEnv->index = topStack;}
 if (ClEnv->verbose > 2) princ_string(" ... done.\n");
   // freeChain();
 if (probe != NULL) {printf("--- the probe is %d -> adr = %d ----\n",probe,getADR(probe));
                     printf("*[O] = %d, 1:%d, 2:%d\n",probe[1],probe[2],probe[3]);}
  ClEnv->cout = p;}

// marks all the object contained in the hash table.
void ClaireAllocation::markHash()
{int i;
 mark(_oid_(ClEnv->moduleStack));
 for (i = 0; i < 512; i++) mark(_oid_(ClRes->ascii[i]));
 for (i = 0; i < maxHash; i++)
     {symbol *x = ClRes->sTable[i];
      if (x != NULL)
         {mark(_oid_(x));
          markString(x->name);
          mark(x->value);}}}

// mark the items in the various stacks
void ClaireAllocation::markStack()
{int i;
 if (currentNew != NULL)
    {OID n = _oid_(currentNew);
        if (SIZE(n) > 0 && !INHERIT(OWNER(n),Kernel._class))  // v3.3.38: Things must be protected properly
           MARKCELL(ADR(n)); }
 for (i=0; i < ClEnv->index; i++) mark(ClEnv->stack[i]);
 for (i=1; i < index; i++)
    {if ((int) gcStack[i] > (int) &Cmemory) mark(_oid_(gcStack[i]));}
 for (i=1; i <= ClRes->oIndex; i++)
    {ClaireObject *x = ClRes->hvoStack[i];
      if (x != NULL) mark(_oid_(x));}      // v3.3.28: MARKCELL + markObject !
 for (i=1; i <= ClRes->iIndex; i++) mark(ClRes->hviStack[i]);      // int or ANY !
 }

// mark bags and objects recursively that ware pushed on the stack
void ClaireAllocation::markPushed(int i)
{while (i < ClEnv->index)
   {OID n = ClEnv->stack[i];
      if  (CTAG(n) == OBJ_CODE) markAny(OBJECT(ClaireAny,n));  // markAny since the cell is marked
      i++; }}

// marks anything seen (an OID)
// note: MARKCELL mark the cell
void ClaireAllocation::mark(OID n)
{ if (CTAG(n) == OBJ_CODE)
    {if (SIZE(n) > 0)
        {MARKCELL(ADR(n));
         if (ClEnv->index >= maxStack) Cerror(-1,(int) "Stack overflow during gc",0);
         else if INHERIT(OWNER(n),Kernel._thing) PUSH(n);
         else markAny(OBJECT(ClaireAny,n));}}
}

// similar function for a ClaireAny pointer
// warning: does not mark the cell (assumes that it was done earlier !)
void ClaireAllocation::markAny(ClaireAny *x)
{ClaireClass *c = x->isa;
   if INHERIT(c,Kernel._bag) markBag( (bag *) x);
   else if INHERIT(c,Kernel._primitive) markPrimitive( (ClairePrimitive *) x);
   else markObject((ClaireObject *) x);   // could be a type !
}


// mark a (Pushed) object (could be a type)
void ClaireAllocation::markObject (ClaireObject *x)
{ITERATE(s);
 #ifdef CLDEBUG              // useful
 int badADR = ADR(_oid_(x));
 #endif
 list *l = x->isa->slots;
 if (x == probe) princ_string("probe found and marked --\n");
 if (x->isa == Kernel._function) markString( ((ClaireFunction *) x)->name);
 for (START(l);
      NEXT(s) ; )
   {int z = *SLOTADR(x, OBJECT(slot,s)->index);
    int i = OBJECT(slot,s)->index;
    if (z != 0)
     {ClaireClass *c = (ClaireClass *) OBJECT(slot,s)->srange;
       // we need to go through mark to perform the MARKCELL routine ! (v3.1.04)
       if ( c == Kernel._object) mark(_oid_(z));
       else if ( c == Kernel._any ) mark(z);
       else if ( c == Kernel._string ) markString((char *) z);
       else if ( c == Kernel._array )
          {int  x = POINTOADR(z) - 3;              // index in Cmemory
           if (Cmemory[x] > 0)                     // avoid mark => perform test !
             {MARKCELL(x);
              markArray((OID *) z);}}}}}           // markArray does not protect !

// mark a ClairePrimitive item
// this assumes that the cell associated to x has been marked already
void ClaireAllocation::markPrimitive(ClairePrimitive *x)
{ClaireClass *c = x->isa;
  if (x == probe) princ_string("primitive probe found and marked --\n");
  if (c == Kernel._array)
    {markArray(array_v(_oid_(x)));}
  else if (c == Kernel._string)
     markString((char *) ((ClaireImport *) x)->value);   // different cell for x and x->value
  else if (c == Kernel._primitive)
     markString((char *) &Cmemory[POINTOADR(x) + 1]);
}

//  mark a list or a set
void ClaireAllocation::markBag(bag *x)
{int i;
    if (x == probe) princ_string("bag probe found and marked --\n");
    if (x->of != NULL) mark(_oid_(x->of));                     // v3.1.04 !!!
    if (x->content != NULL)
      {if ((*x)[0] > 0) (*x)[0] = -((*x)[0]);                 // should use a MARKCELL
       for (i = 1; i <= x->length; i++)
         {// if (ClEnv->verbose > 10) printf("bag[%d] -> %d\n",i, (*x)[i]);
          mark((*x)[i]); } } }


// this is new and beautiful, we look if the string was allocated
// by Claire and we mark it ...
void ClaireAllocation::markString(char *s)
{if CLMEM(s)   // a CLAIRE string
   {int x = POINTOADR(s) - 2;                                  // index in Cmemory
    if (Cmemory[x] > 0) MARKCELL(x);}}

// mark the memory chunk allocated for an array
void ClaireAllocation::markArray(OID* a)
{int i;                                                          // index in Cmemory
 ClaireType *t = ARRAYTYPE(a);
 if (t != NULL) mark(_oid_(t));                                   // v3.1.04 !!!
 if (t != Kernel._float)
    for (i = 1; i <= ((int) a[0]); i++) mark(a[i]); }


//  go through the array of items
void ClaireAllocation::sweepChunk()
{OID i = 1,p;
 while (i < maxList)
  {p = Cmemory[i];
//   printf("chunk[%d] size:%d\n",i,p);
   ASSERT(p != 0);
   if (p < 0) Cmemory[i]= -p;          // chunk was marked -> keep it
   else if (Cmemory[i + 1] != NOTHING) // chunk was not marked and is being used
       {if (i == getADR(probe)) printf("free the probe chunk !\n");
        i = freeChunk(i);}
   i = i + Cmemory[i];}
}



// go through all the (short) objects
void ClaireAllocation::sweepObject()
{OID p,i = maxList + 2;                       // first free position (defined in claire.cp)
 // printf("sweep Object start at %d upto %d\n",i,firstFree);
 while (i < firstFree)
   {p = Cmemory[i];
    // printf("Memory[%d] -> size is %d\n",i,p);
    if (p < 0)                                  // marked short
       {p = -p;
        Cmemory[i] = p; p++;}
    else if (p == 0) p = OPTIMIZE;              // this is already a free cell
    else {if (i == getADR(probe)) printf("free the probe short object !\n");
          if (Cmemory[i + 1] != (int) Kernel._function) freeObject(i);
          p++;}
    if (p < OPTIMIZE) p = OPTIMIZE;
    ASSERT(p <= 50 || i == maxList + 2);
    i = i + p;}
 }



// this is a small useful method for debugging
// new in v3.0.54
void checkOID(OID n)
{int u = ADR(n);
 if (ClAlloc->numGC > 0)
   { if (Cmemory[u] == 0)
       {printf("OID %d [ADR = %d] has size 0!",n,u);
        Cerror(-1,0,0);}
     if (Cmemory[u + 1] == NOTHING)
       {printf("OID %d [ADR = %d] has been freed",n,u);
        Cerror(-1,0,0);}
     ClaireClass * c = OWNER(n);
     if (c->isa != Kernel._class)
       {printf("OID %d [ADR = %d] has a corrupted owner !",n,u);
        Cerror(-1,0,0);} }}


/*********************************************************************/
/**    4. Interface functions & Compiler functions                   */
/*********************************************************************/

void ClaireAllocation::kill(OID n)
{if  (CTAG(n) == OBJ_CODE)
  {ClaireAny *x = OBJECT(ClaireAny,n);
   int i = ADR(n);
   ClaireClass *c = x->isa;
     if (c == Kernel._list || c == Kernel._set || c == Kernel._tuple)
          {int u =  POINTOADR(x) - 3;
            if (SIZE(u) < OPTIMIZE) freeObject(u);
            else freeChunk(u);
            freeObject(i);}
     else if (c == Kernel._string)
          {freeString((char *) ((ClaireImport *) x)->value);
           freeObject(i);}
     else if INHERIT(c,Kernel._cl_import)          // v3.2.40
          {(*Kernel.free_I)(n);                 // this allows to free the memory for the import
           freeObject(i);}                      // free the OID = import container
     else if (c == Kernel._array)
           freeChunk(i);
     else if (c == Kernel._symbol)
           {symbol *s = ((symbol *) x);
            ClRes->sTable[s->module_I->hash(s->name)] = NULL;
            freeObject(i);}
     else { ASSERT((INHERIT(c,Kernel._object) || INHERIT(c,Kernel._type)));
            if (SIZE(n) < OPTIMIZE) freeObject(i);
            else if (i < maxList) freeChunk(i);}}}

// the basic function to kill an object: deallocate
void kill_I_any(OID n) {ClAlloc->kill(n);}

// call the gc
void claire_gc() {ClAlloc->gc("call");}

// the gcStack now contains pointers to objects/primitive
// this function version is necessary for windows
ClaireAny *GC_OBJ_F(ClaireAny *x)                          // v3.3.36 (larger domain)
{ClAlloc->gcStack[ClAlloc->index++] = x; return x;}

// push an OID on the GC stack 
OID GC_OID(OID n)
  {if (CTAG(n) == OBJ_CODE) GC_ANY(OBJECT(ClaireObject,n));
   return n;}

void GC__OID(OID n, int m)
   {if (CTAG(n) == OBJ_CODE) GC__ANY(OBJECT(ClaireObject,n),m);}

// string
char *GC_STRING(char *s)
{if CLMEM(s)
 {int x = POINTOADR(s) - 1;         // works because the class is the string marker
  ClaireObject *y = (ClaireObject *) &Cmemory[x];
  ClaireClass *c = y->isa;
  ASSERT(c == Kernel._primitive);
  GC_ANY((ClaireAny *) &Cmemory[x]);} // is Kernel._primitive
 return s;}

void GC__STRING(char *s, int n)
{if CLMEM(s)
 {int x = POINTOADR(s) - 1;         // works because the class is the string marker
  GC__ANY((ClaireAny *) &Cmemory[x], n); // is Kernel._primitive
}}

// array
OID *GC_ARRAY(OID *a)
{int x = POINTOADR(a) - 2;            // v3.3.0  a - 2 to get the container object (and -3 to get the object index)
 GC_ANY((ClaireAny *) &Cmemory[x]);
 return a;}

// same for arrays
OID gc_error()  {  Cerror(27,0,0); return 1;}

// reserves n slots in the garbage stack
void GC_RESERVE(int n)
{ if ((ClAlloc->index + n) < ClAlloc->maxGC)
    {int i = ClAlloc->index, j = i + n + 1;
      ClAlloc->gcStack[i] = (ClaireAny *) ClAlloc->base;
      ClAlloc->base = i++;
      for (; i <= j; i++) ClAlloc->gcStack[i] = NULL;
      ClAlloc->index = j;}
  else gc_error();}

// statistics about memory management
// notice that if the verbosity is more than 4 we get a dump
void ClaireAllocation::memStat()
{list *l = memList(CFALSE);
  // printf("Loglist = %d\n",ClAlloc->logList);
 
  princ_string("Chunk allocation: "); princ_integer((*l)[1]);
  princ_string(" used cells out of "); princ_integer(maxList);
  princ_string("\nShort item allocation: "); princ_integer((*l)[2]);
  princ_string(" used cells out of "); princ_integer(maxSize - maxList);
  princ_string("\nSymbol allocation: "); princ_integer((*l)[3]);
  princ_string(" used cells out of "); princ_integer(maxHash);
  princ_string("\nWorld stack: "); princ_integer((*l)[4]);
  princ_string(" used cells out of "); princ_integer(maxHist);princ_string("\n");
  {int i, useList[28];                         // start the chunk zone analysis
   int useString = 0, useObject = 0, useOther = 0, maxFree = 0, totalList = 0;
   for (i = 1; i < logList; i++)
      {useList[i] = 0;
       if (entryList[i] != NOTHING) maxFree = i;}
   for (i = 1; i < maxList; i += Cmemory[i])
      {int p = Cmemory[i], x = Cmemory[i + 1];
       if (x != NOTHING)                       // the chunk is used
          {if (x == (int) Kernel._primitive)              // used for a string
              {useString += p;
               if (ClEnv->verbose > 4)
                  {princ_string("string(");
                   princ_string((char *) &Cmemory[i + 2]);
                   princ_string(")\n");}}
           else if (x == (int) Kernel._array)          // used for an array
              {useOther += p;
               if (ClEnv->verbose > 4)
                  {princ_string("array("); princ_integer(p); princ_string(")\n");}}
           else if (CLMEM(x) && ((ClaireAny *) x)->isa == Kernel._class)
              {useObject += p;
               if (ClEnv->verbose > 4)
                  {princ_string("object(");
                   princ_symbol(((ClaireClass *) x)->name);
                   princ_string(")\n");}}
           else {int size = log2up(p);                     // a bag (what else ?)
                 useList[size] += p;
                 totalList += p;
                 if (ClEnv->verbose > 4)
                  {princ_string("bag("); princ_integer(p); princ_string(")\n");}}}}

   princ_string("Bag: "); princ_integer(totalList);
   princ_string(" Max free bag size: "); princ_integer(1 << maxFree);
   princ_string("\nList usage: ");
   for (i = 4; i < logList; i++)
     {princ_integer(i); princ_string("="); princ_integer(useList[i]); princ_string(" ");}
   princ_string("\nString: "), princ_integer(useString);
   princ_string(" Object: "), princ_integer(useObject);
   princ_string(" Array: "), princ_integer(useOther);
   princ_string("\n");}}

// private methods
// we add a parameter (x = true -> returns the max capacity)
list *ClaireAllocation::memList(ClaireBoolean *x)
{int i = nextFree, nb = 0, usedSymb = 0, usedWorld = ClRes->iIndex, prev; // prev -> remove
 if (x == CTRUE)
     return list::alloc(4,maxList,
                          maxSize - maxList,
                          maxHash,
                          maxHist);
 while (i != NOTHING) {// printf("chasing %d -> %d \n",i,Cmemory[i + 1]);
                        //if (Cmemory[i + 1] > 3000000)
                        //  printf("Cmemory[%d + 1] = %d (from %d) \n",i,Cmemory[i + 1],prev);
                        nb++; i = Cmemory[i+1]; prev = i;}
 for (i = 1; i < maxHash; i++) if (ClRes->sTable[i] != NULL) usedSymb++;
 if (ClRes->oIndex > usedWorld) usedWorld = ClRes->oIndex;
 if (ClRes->fIndex > usedWorld) usedWorld = ClRes->fIndex;
 return list::alloc(4,usedCells,
                      firstFree - maxList - nb * OPTIMIZE,
                      usedSymb,
                      usedWorld);
}

// C API
void claire_stat() {ClAlloc->memStat();}

// query CLAIRE memory management
OID claire_mem(OID n)
{if (OWNER(n) == Kernel._boolean)
    return _oid_(ClAlloc->memList(OBJECT(ClaireBoolean,n)));
 else {if (OWNER(n) == Kernel._integer)
           {if (n == 1) printf("Badguy: %d, %d \n",Cmemory[BadGuy],Cmemory[BadGuy + 1]);
           }
        else {printf("--- the probe is %d -> adr = %d ----\n",OBJECT(ClaireAny,n),ADR(n));
               ClAlloc->probe = OBJECT(ClaireAny,n);}
       return n;}}

