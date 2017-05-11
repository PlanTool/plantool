/***********************************************************************/
/**  microCLAIRE                                       Yves Caseau     */
/**  noConsole.cpp                                                     */
/**  Copyright (C) 1994-99 Yves Caseau. All Rights Reserved.           */
/**  cf claire.h                                                       */
/***********************************************************************/

#include <claire.h>
#include <Kernel.h>
#include <Core.h>

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

extern void call_main();

/* creates a protected list of arguments */
list *C_to_Claire(int argc, char *argv[])
{int i;
 list *l;
  GC_BIND;
  l = list::empty();
  for (i=1; i<argc; i++) l->addFast(_string_(argv[i]));  // v3.2.04
  return GC_OBJECT(list,l);
}

// this is our main function (a simpler version of what is in main.cpp)
main(int argc, char *argv[])
{int k = 1, i = 7, j = 11;
  //  printf("==== call main argc = %d, argv[1] = %s ==== \n",argc,argv[1]);
  if ((argc > 3) && (equal_string("-s",argv[1]) == CTRUE))
    { i = atoi(argv[2]); j = atoi(argv[3]);  k = 4;
      if ((i < 0) || (i > 20) || (j < 0) || (j > 20)) exit(1);
      //  printf("increasing memory size by 2^%d and 2^%d \n",i,j);
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
  ClAlloc->maxHash = ((1 << (11 + i)) + 200);            // <yc> 7/98 Changed for claire3 !
  ClAlloc->hashMask = ((1 << (11 + i)) - 1);
  ClAlloc->stdOut = ClairePort::make(stdout);
  ClAlloc->stdIn = ClairePort::make(stdin);
  ClAlloc->stdErr = ClairePort::make(stdout);
  ClaireResource::run();
  ClEnv->params = C_to_Claire(argc,argv);
  call_main();
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


