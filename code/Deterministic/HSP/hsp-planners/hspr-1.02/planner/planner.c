/*
**
** Universidad Simon Bolivar, 1999 (c)
** Blai Bonet and Hector Geffner. 1999.
**
*/


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/mman.h>
#include <limits.h>
#include <ulimit.h>
#include <unistd.h>
#include <assert.h>
/*#include "planner.h"*/



/*********************************************************
**********************************************************
**
** MACROS
**
**/

#define MIN(x,y)         ((x) > (y) ? (y) : (x))
#define MAX(x,y)         ((x) > (y) ? (x) : (y))
#define MUTEX(x,y)       (_mutex[(x-1)*numberAtoms + (y-1)])

#define fatal(code)      _fatal( (code), NULL, __FILE__, __LINE__ )
#define fatal1(code,s)   _fatal( (code), (s), __FILE__, __LINE__ )

#define GBFS             1
#define ASTAR            2



/*********************************************************
**********************************************************
**
** CONSTANTS
**
**/

#define MAXPARAMETERS        20
#define MAXATOMS             10*1024
#define MAXSCHEMA            256

#define NODEHASHSIZE         5119            /* prime nearest to 5K */
#define ATOMHASHSIZE         1021            /* prime nearest to 1K */
#define BUCKETTABLESIZE      2*1024

#define SIZE                 (numberAtoms+1)
#define NODESIZE             (sizeof( node_t ) + claimSize * sizeof( atom_t ))


/* exit codes */
#define noError              00
#define noMoreMemory         10
#define maxSchema            20
#define maxAtoms             30
#define noAlgorithm          40
#define noChildren           50



/*********************************************************
**********************************************************
**
** Structures and Typedefs
**
**/

struct iatom_s
{
  int  idx;
  char name[128];
  int  parameters[MAXPARAMETERS];
  struct iatom_s *next;
} iatom_s;
typedef struct iatom_s iatom_t;


struct atom_s 
{
  char val;
} atom_s;
typedef struct atom_s atom_t;


struct node_s
{
  int cost;                               /* cost from root to this node */
  int h_plus;                             /* value of heuristic-plus */
  int h_max;                              /* value of heuristic-max */
  int fvalue;                             /* f-function value */
  int valid;                              /* valid state? */
  int operator;                           /* operator id that leads to this state */
  int bucket;                             /* bucket number of node */
  int open;                               /* true if node in OPEN list */

  struct node_s *father;                  /* link to father nodes */
  struct node_s *hashNext;                /* link into nodeHashTable */
  struct node_s *hashPrev;                /* link into nodeHashTable */
  struct node_s *bucketNext;              /* link into bucketTable */
  struct node_s *bucketPrev;              /* link into bucketTable */

  atom_t *state;                          /* current state (must be the last field)  */
} node_s;
typedef struct node_s node_t;


struct operator_s
{
  char *name;
  int valid;
  int *prec;
  int *add;
  int *del;
} operator_s;
typedef struct operator_s operator_t;


struct cost_s
{
  int level;
  int cost;
  int max;
} cost_s;
typedef struct cost_s cost_t;


struct mutex_s
{
  int x;
  int y;
  struct mutex_s *next;
  struct mutex_s *prev;
} mutex_s;
typedef struct mutex_s mutex_t;


struct mutexSet_s
{
  int x;
  int size;
  struct mutex_s *set;
  struct mutexSet_s *next;
} mutexSet_s;
typedef struct mutexSet_s mutexSet_t;


struct procRegister_s
{
  char   *procedure;
  float  diffTime;
  struct timeval startTime;
  struct timeval endTime;
  struct procRegister_s *next;
} procRegister_s;
typedef struct procRegister_s procRegister_t;



/*********************************************************
**********************************************************
**
** Global Variables
**
**/

/* fundamental global variables */
int claimSize;
int numberAtoms = 0;
int numberSchema = 0;
int numberOperators = 0;
int numberRelevantAtoms = 0;

/* for variables and domain instantiation */
int **values;
int **vars;
int initialized = 0;

/* for operator info extraction */
int operatorPrec[20], operatorPrecSize;
int operatorAdd[20], operatorAddSize;
int operatorDel[20], operatorDelSize;

/* heuristic costs */
atom_t staticState[MAXATOMS];
atom_t staticNewState[MAXATOMS];
cost_t oldCost[MAXATOMS];
cost_t cost[MAXATOMS];
int level;

/* operator access tables */
int (*preconditionHeuristicOperatorFunctionTable[MAXSCHEMA])( register int * );
int (*heuristicOperatorFunctionTable[MAXSCHEMA])( register int * );
int (*operatorFunctionTable[MAXSCHEMA])( register int *, register atom_t *, register atom_t * );

/* for relevant atoms identification */
int    *parents[MAXATOMS];
int    parentsSize[MAXATOMS];
atom_t relevantAtom[MAXATOMS];



/*********************************************************
**********************************************************
**
** Static Variables
**
**/

/* atom hash table */
static iatom_t *atomHashTable[ATOMHASHSIZE];
static iatom_t *atomHashPool = NULL;
static int atomHashClaimSize = 0;

/* node hash table */
static node_t *nodeHashTable[NODEHASHSIZE];
static int nodeHashDiameter[NODEHASHSIZE];
static int *nodeHashValues;
static int nodeHashNumElem = 0;

/* for mutexes */
static int *_mutex, *_mutex0;
static mutex_t *mutexList = NULL;
static mutexSet_t *mutexSet;
static mutexSet_t *mutexSetList = NULL;

/* operator info */
static int operatorTableSize = 0;
static operator_t *operatorTable = NULL;
static int **invPrecTable, *invPrecTableSize;
static int **invAddTable, *invAddTableSize;
static int **invDelTable, *invDelTableSize;
static int **notAdmissible, *notAdmissibleSize;

/* for node pool management */
static int pageSize;
static node_t *nodePool = NULL;
static int currentNodePool = -1;
static int nodePoolClaimSize = 0;
static int *nodePoolSize;
static char *nodePoolUsed;
static int nodePoolTableSize = 0;
static node_t **nodePoolTable;

/* for Best-First Search */
static node_t *headOPEN = NULL;
static node_t *tailOPEN = NULL;
static node_t *CLOSE = NULL;
static node_t *firstNodeInBucket[BUCKETTABLESIZE];
static node_t *lastNodeInBucket[BUCKETTABLESIZE];
static int minBucket;
static int maxBucket;

/* true initial state */
static atom_t staticInitialState[MAXATOMS];
static atom_t staticGoalState[MAXATOMS];

/* problem data */
static int expandedNodes = 0;
static int generatedNodes = 0;

/* some global parameters */
static int verbose;
static int weight;
static int algorithm;

/* procedure registration stack */
static procRegister_t *procStackTop = NULL;



/*********************************************************
**********************************************************
**
** External Variables
**
**/

extern char *_low_schemaName[];
extern char *_low_objectName[];
extern char *_low_predicateName[];
extern int   _low_goalAtoms[];



/*********************************************************
**********************************************************
**
** Function Prototypes
**
**/

void    newMutex( register int, register int, register mutex_t ** );
void    delMutex( register mutex_t * );
void    newMutexSet( register int );

void    identifyGoalParents( register int * );
void    printNode( register FILE *, register char *, register node_t * );
void    removeNodeFromOPEN( register node_t * );
node_t  *removeLastFromOPEN( register node_t * );
node_t  *removeNodeFromCLOSE( void  );

node_t  *greedyBFS( void );

void    printState( register FILE *, register atom_t * );
void    registerEntry( register char * );
int     registerExit( void );
void    flushAllRegisters( void );

void    _fatal( register int, register char *, register char *, register int );
void    printStatistics( void );

extern void setInitialState( void );
extern int  goalState( atom_t * );
extern void fillTable( void );
extern void setupHeuristic( void );



/*********************************************************
**********************************************************
**
** Operator Instantiation
**
**/

void
buildParameterList( register int *parameters, register int schema, register int *ip,
		    void (*insertOperator)( register int *, register int ),
		    int (*testOperatorPrecondition)( register int *, register int ) )
{
  register int *vp, *jp;

  if( *ip != 0 )
    {
      for( vp = values[numberSchema * (*ip - 1) + schema]; *vp != 0; ++vp )
	{
	  for( jp = &parameters[1]; jp < &parameters[MAXPARAMETERS]; ++jp )
	    if( (*jp > 0) && (*jp == *vp) )
	      break;
	  if( jp == &parameters[MAXPARAMETERS] )
	    {
	      parameters[*ip] = *vp;
	      if( (*testOperatorPrecondition)( parameters, schema ) )
		buildParameterList( parameters, schema, ip + 1, insertOperator, testOperatorPrecondition );
	    }
	}
      parameters[*ip] = 0;
    }
  else
    {
      if( (*testOperatorPrecondition)( parameters, schema ) )
	{
	  /* insert delimiting marker */
	  for( jp = parameters; *jp > 0; ++jp );
	  *jp = -1;

	  /* insert operator */
	  (*insertOperator)( parameters, schema );
	}
    }
}


void
instantiateOperators( void (*insertOperator)( register int *, register int ),
		      int (*testOperatorPrecondition)( register int *, register int ) )
{
  register int schema;
  static int parameters[MAXPARAMETERS];

  for( schema = 0; schema < numberSchema; ++schema )
    {
      memset( parameters, 0, MAXPARAMETERS * sizeof( int ) );
      parameters[0] = schema + 1;
      buildParameterList( parameters, schema, vars[schema], insertOperator, testOperatorPrecondition );
    }
}



/*********************************************************
**********************************************************
**
** Heuristics
**
**/

int
testHeuristicOperatorPrecondition( register int *parameters, register int schema )
{
  return( (*preconditionHeuristicOperatorFunctionTable[schema])( parameters ) );
}


void
insertHeuristicOperator( register int *parameters, register int schema )
{
  (*heuristicOperatorFunctionTable[schema])( parameters );
}


void
heuristicInitialization( register atom_t *state )
{
  register int i, number;

  /* initialize costs with given state */
  for( i = 1; i < MAXATOMS; ++i )
    {
      if( state[i].val == 1 )
	{
	  oldCost[i].level = 0;
	  oldCost[i].cost = 0;
	  oldCost[i].max = 0;
	}
      else
	{
	  oldCost[i].cost = INT_MAX;
	  oldCost[i].max = INT_MAX;
	}
    }
  memcpy( cost, oldCost, MAXATOMS * sizeof( cost_t ) );

  /* computation of atom's costs */
  level = 0;
  number = 1;
  while( number != 0 )
    {
      ++level;
      instantiateOperators( &insertHeuristicOperator, &testHeuristicOperatorPrecondition );
      number = memcmp( oldCost, cost, MAXATOMS * sizeof( cost_t ) );
      memcpy( oldCost, cost, MAXATOMS * sizeof( cost_t ) );
    }
}


void
heuristics( register node_t *node )
{
  register int i;
  register int atoms;

  /* initialization */
  node->h_plus = 0;
  node->h_max = 0;
  node->valid = 1;

  /* calculation for heuristic functions */
  atoms = 0;
  for( i = 1; i < SIZE; ++i )
    if( node->state[i].val == 1 )
      {
	++atoms;
	if( cost[i].cost == INT_MAX )
	  {
	    node->h_plus = -1;
	    node->h_max = -1;
	    node->valid = 0;
	    return;
	  }
	else
	  {
	    node->h_plus += cost[i].cost;
	    node->h_max = MAX( node->h_max, cost[i].max );
	  }
      }
}



/*********************************************************
**********************************************************
**
** Basic Atoms and Operators
**
**/

char *
operatorName( register int *parameters, register int schema )
{
  register int *ip;
  static char name[128];

  name[0] = '(';
  name[1] = '\0';
  strcat( name, _low_schemaName[schema] );
  for( ip = &parameters[1]; *ip != -1; ++ip )
    {
      strcat( name, " " );
      strcat( name, _low_objectName[*ip - 1] );
    }
  strcat( name, ")" );
  return( name );
}


char *
atomName( register int *parameters )
{
  register int *ip;
  static char name[128];

  name[0] = '(';
  name[1] = '\0';
  strcat( name, _low_predicateName[parameters[0] - 1] );
  for( ip = &parameters[1]; *ip != -1; ++ip )
    {
      strcat( name, " " );
      strcat( name, _low_objectName[*ip - 1] );
    }
  strcat( name, ")" );
  return( name );
}



/*********************************************************
**********************************************************
**
** Atom Hash
**
**/

unsigned 
atomHashFunction( register int *parameters )
{
  register int *ip;
  register unsigned index;

  index = 0;
  for( ip = parameters; *ip != -1; ++ip )
    index += *ip;
  return( index < ATOMHASHSIZE ? index : index % ATOMHASHSIZE );
}


iatom_t *
insertIntoAtomHash( register int *parameters )
{
  register unsigned index;
  register iatom_t *iatom;
  static int currentAtom;

  if( numberAtoms == MAXATOMS )
    fatal( maxAtoms );

  index = atomHashFunction( parameters );
  if( currentAtom >= atomHashClaimSize )
    {
      atomHashClaimSize = (atomHashClaimSize == 0 ? 1024 : 2 * atomHashClaimSize);
      atomHashPool = (iatom_t *) malloc( atomHashClaimSize * sizeof( iatom_t ) );
      if( !atomHashPool )
	fatal( noMoreMemory );
      currentAtom = 0;
    }
  iatom = &atomHashPool[currentAtom++];
  memcpy( iatom->parameters, parameters, MAXPARAMETERS * sizeof( int ) );
  strcpy( iatom->name, atomName( parameters ) );
  iatom->idx = ++numberAtoms;
  iatom->next = atomHashTable[index];
  atomHashTable[index] = iatom;
  return( iatom );
}


iatom_t *
readAtomHash( register int *parameters )
{
  register iatom_t *iatom;
  register int *ip, *jp;

  for( iatom = atomHashTable[atomHashFunction( parameters )]; iatom != NULL; iatom = iatom->next )
    {
      for( ip = parameters, jp = iatom->parameters; (*ip != -1) && (*ip == *jp); ++ip, ++jp);
      if( (*ip == -1) && (*jp == -1) )
	return( iatom );
    }

  /* if not present, insert it right now! */
  return( insertIntoAtomHash( parameters ) );
}


iatom_t *
readAtomByNumber( register int index )
{
  register int i;
  register iatom_t *iatom;

  for( i = 0; i < ATOMHASHSIZE; ++i )
    {
      for( iatom = atomHashTable[i]; (iatom != NULL) && (iatom->idx != index); iatom = iatom->next );
      if( iatom != NULL )
	break;
    }
  assert( i != ATOMHASHSIZE );
  return( iatom );
}


char *
readAtomName( register int index )
{
  return( atomName( readAtomByNumber( index )->parameters ) );
}



/*********************************************************
**********************************************************
**
** Nodes
**
**/

void
initializeMemoryMgmt( void )
{
  pageSize = sysconf( _SC_PAGESIZE );
}


node_t *
getNode( void )
{
  register int oldSize;
  register node_t *result;
  register node_t **newNodePoolTable;
  register int *newNodePoolSize;
  register char *newNodePoolUsed;

  /* check if we have enough pool areas */
  if( currentNodePool + 1 >= nodePoolTableSize )
    {
      oldSize = nodePoolTableSize;
      nodePoolTableSize = (nodePoolTableSize == 0 ? 1024 : 2 * nodePoolTableSize);
      newNodePoolTable = (node_t **) realloc( nodePoolTable, nodePoolTableSize * sizeof( node_t * ) );
      newNodePoolSize = (int *) realloc( nodePoolSize, nodePoolTableSize * sizeof( int ) );
      newNodePoolUsed = (char *) realloc( nodePoolUsed, nodePoolTableSize * sizeof( char ) );
      if( !newNodePoolTable || !newNodePoolSize || !newNodePoolUsed )
	fatal( noMoreMemory );

      /* prepare new pool areas */
      nodePoolTable = newNodePoolTable;
      nodePoolSize = newNodePoolSize;
      nodePoolUsed = newNodePoolUsed;
      memset( &nodePoolUsed[oldSize], 0, (nodePoolTableSize - oldSize) * sizeof( char ) );
    }

  /* check if we have enough space in pool area */
  if( (nodePool == NULL) || 
      ((char *)nodePool ==
       (char *)((unsigned)nodePoolTable[currentNodePool] + nodePoolSize[currentNodePool])) )
    {
      /* get new pool area */
      ++currentNodePool;
      if( nodePoolUsed[currentNodePool] == 0 )
	{
	  nodePoolClaimSize = (nodePoolClaimSize == 0 ? 1024 : (int) (1.5 * nodePoolClaimSize));
	  nodePoolSize[currentNodePool] = nodePoolClaimSize * NODESIZE;
	  fprintf( stderr, "HEAPMGMT: allocating memory for %d nodes (%d bytes)... ", 
		   nodePoolClaimSize, nodePoolSize[currentNodePool] );
	  fflush( stderr );
	  nodePoolTable[currentNodePool] = (node_t *) malloc( nodePoolSize[currentNodePool] );
	  if( !nodePoolTable[currentNodePool] )
	    fatal( noMoreMemory );
	  else
	    {
	      fprintf( stderr, "done!\n" );
	      nodePoolUsed[currentNodePool] = 1;
	    }
	}
      nodePool = &nodePoolTable[currentNodePool][0];
    }

  /* claim node from current pool */
  result = nodePool;
  result->state = (atom_t *) ((unsigned)&result->state + sizeof( node_t * ));
  nodePool = (node_t *) ((unsigned)nodePool + NODESIZE);

  /* return */
  return( result );
}


void
freeLastNodes( register int number )
{
  nodePool = (node_t *) ((unsigned)nodePool - (number * NODESIZE));
}


void
freeAllSpace( void )
{
  currentNodePool = -1;
  nodePool = NULL;
}



/*********************************************************
**********************************************************
**
** Node Hash
**
**/

int
stateCmp( register atom_t *state1, register atom_t *state2 )
{
  register atom_t *lp1, *lp2;

  assert( initialized );
  for( lp1 = state1, lp2 = state2; (lp1 < &state1[SIZE]) && (lp1->val == lp2->val); ++lp1, ++lp2 );
  return( !(lp1 == &state1[SIZE]) );
}


unsigned 
nodeHashFunction( register atom_t *state )
{
  register unsigned val = 0;
  register int *hp;
  register atom_t *ap;

  assert( initialized );
  for( ap = &state[1], hp = nodeHashValues; ap < &state[SIZE]; ++ap, ++hp )
    val += (ap->val == 1 ? *hp : 0);
  return( val % NODEHASHSIZE );
}


node_t *
readNodeHash( register atom_t *state )
{
  register unsigned index;
  register node_t *node;

  index = nodeHashFunction( state );
  for( node = nodeHashTable[index]; node != NULL; node = node->hashNext )
    if( !stateCmp( state, node->state ) )
      return( node );
  return( NULL );
}


void
insertIntoNodeHash( register node_t *node )
{
  register unsigned index;

  index = nodeHashFunction( node->state );
  node->hashNext = nodeHashTable[index];
  node->hashPrev = NULL;
  nodeHashTable[index] = node;
  if( node->hashNext != NULL )
    node->hashNext->hashPrev = node;
  ++nodeHashNumElem;
  ++(nodeHashDiameter[index]);
}


void
removeNodeFromHash( register node_t *node )
{
  register unsigned index;

  index = nodeHashFunction( node->state );
  if( nodeHashTable[index] == node )
    {
      assert( node->hashPrev == NULL );
      nodeHashTable[index] = node->hashNext;
    }
  else
    {
      assert( node->hashPrev != NULL );
      node->hashPrev->hashNext = node->hashNext;
    }

  if( node->hashNext != NULL )
    node->hashNext->hashPrev = node->hashPrev;
}


void
cleanNodeHash( void )
{
  freeAllSpace();
  memset( nodeHashDiameter, 0, NODEHASHSIZE * sizeof( int ) );
  memset( nodeHashTable, 0, NODEHASHSIZE * sizeof( node_t *) );
}


void
nodeHashStatistics( void )
{
  register int index, diameter;

  diameter = 0;
  for( index = 0; index < NODEHASHSIZE; ++index )
    if( nodeHashDiameter[index] > diameter )
      diameter = nodeHashDiameter[index];

  fprintf( stderr, "NODEHASH: nodes in hash table = %d\n", nodeHashNumElem );
  fprintf( stderr, "NODEHASH: diamter of hash table = %d\n", diameter );
}



/*********************************************************
**********************************************************
**
** Node Expansion
**
**/

int
nodeBucket( register node_t *node )
{
  register int result;

  switch( algorithm )
    {
    case GBFS:  /* GBFS: f(n) = g(n) + W*h_plus(n) */
      result = node->cost + (weight * node->h_plus);
      break;
    case ASTAR: /* ASTAR: f(n) = g(n) + h_plus(n) */
      result = node->cost + node->h_plus;
      break;
    }
  return( result );
}


void
setInitialOPEN( register node_t *node )
{
  register int bucket;

  bucket = nodeBucket( node );
  minBucket = maxBucket = bucket;
  node->bucket = bucket;
  node->bucketNext = NULL;
  node->bucketPrev = NULL;
  firstNodeInBucket[bucket] = node;
  lastNodeInBucket[bucket] = node;
  headOPEN = node;
  tailOPEN = node;
}


node_t *
getFirstOPEN( void )
{
  return( headOPEN );
}


void
removeNodeFromOPEN( register node_t *node )
{
  register int bucket;

  bucket = node->bucket;

  /* link update */
  if( node->bucketPrev != NULL )
    node->bucketPrev->bucketNext = node->bucketNext;
  if( node->bucketNext != NULL )
    node->bucketNext->bucketPrev = node->bucketPrev;

  /* bucket update */
  if( node == firstNodeInBucket[bucket] )
    {
      if( lastNodeInBucket[bucket] != node )
	firstNodeInBucket[bucket] = node->bucketNext;
      else
	{
	  firstNodeInBucket[bucket] = NULL;
	  lastNodeInBucket[bucket] = NULL;
	}
    }
  else if( node == lastNodeInBucket[bucket] )
    {
      lastNodeInBucket[bucket] = node->bucketPrev;
    }

  /* OPEN update */
  if( node == headOPEN )
    {
      headOPEN = node->bucketNext;
      if( headOPEN != NULL )
	minBucket = headOPEN->bucket;
      else
	{
	  minBucket = BUCKETTABLESIZE;
	  maxBucket = 0;
	}
    }
  if( node == tailOPEN )
    {
      tailOPEN = node->bucketPrev;
      if( tailOPEN != NULL )
	maxBucket = tailOPEN->bucket;
      else
	{
	  minBucket = BUCKETTABLESIZE;
	  maxBucket = 0;
	}
    }

  /* CLOSE update */
  node->open = 0;
  node->bucketNext = CLOSE;
  if( CLOSE != NULL )
    CLOSE->bucketPrev = node;
  CLOSE = node;
}


node_t *
removeLastFromOPEN( register node_t *father )
{
  register int bucket;
  register node_t *node;

  for( node = tailOPEN; (node != NULL) && (node->father == father); node = node->bucketPrev );
  if( node == NULL )
    return( NULL );
  else
    bucket = node->bucket;

  /* link update */
  if( node->bucketPrev != NULL )
    node->bucketPrev->bucketNext = node->bucketNext;
  if( node->bucketNext != NULL )
    node->bucketNext->bucketPrev = node->bucketPrev;

  /* bucket update */
  if( node == firstNodeInBucket[bucket] )
    {
      if( lastNodeInBucket[bucket] != node )
	firstNodeInBucket[bucket] = node->bucketNext;
      else
	{
	  firstNodeInBucket[bucket] = NULL;
	  lastNodeInBucket[bucket] = NULL;
	}
    }
  else if( node == lastNodeInBucket[bucket] )
    {
      lastNodeInBucket[bucket] = node->bucketPrev;
    }

  /* OPEN update */
  if( node == headOPEN )
    {
      headOPEN = node->bucketNext;
      if( headOPEN != NULL )
	minBucket = headOPEN->bucket;
      else
	{
	  minBucket = BUCKETTABLESIZE;
	  maxBucket = 0;
	}
    }
  if( node == tailOPEN )
    {
      tailOPEN = node->bucketPrev;
      if( tailOPEN != NULL )
	maxBucket = tailOPEN->bucket;
      else
	{
	  minBucket = BUCKETTABLESIZE;
	  maxBucket = 0;
	}
    }

  /* hash update */
  removeNodeFromHash( node );

  /* return */
  return( node );
}


node_t *
removeNodeFromCLOSE( void )
{
  register node_t *node;

  node = CLOSE;

  /* CLOSE update */
  CLOSE = CLOSE->bucketNext;
  if( CLOSE != NULL )
    CLOSE->bucketPrev = NULL;

  /* hash update */
  removeNodeFromHash( node );

  /* return */
  return( node );
}


int
nodeExpansion( register node_t *node, register node_t ***result )
{
  register int *p, *a, *d;
  register int alpha, atom, child;
  register mutex_t *mutex;
  register mutexSet_t *set;
  register node_t *tmp;

  static int initialized = 0;
  static char *operatorDisplay;
  static node_t **buffer;


  /* initialization */
  if( !initialized )
    {
      initialized = 1;
      buffer = (node_t **) malloc( numberOperators * sizeof( node_t * ) );
      operatorDisplay = (char *) malloc( numberOperators * sizeof( char ) );
      if( !buffer || !operatorDisplay )
	fatal( noMoreMemory );
    }

  /* update problem data */
  ++expandedNodes;

  /* get applicable operators using admissibility information */
  for( alpha = 0; alpha < numberOperators; ++alpha )
    operatorDisplay[alpha] = 1;

  /* check admissibility */
  for( atom = 1; atom < SIZE; ++atom )
    if( node->state[atom].val )
      for( p = notAdmissible[atom]; (p != NULL) && (*p != 0); operatorDisplay[*p++] = 0 );

  /* get some initial space */
  child = 0;
  buffer[child] = getNode();

  /* node expansion */
  for( alpha = 0; alpha < numberOperators; ++alpha )
    if( (operatorTable[alpha].valid == 1) && (operatorDisplay[alpha] == 1) )
      {
	/* preconditions: S must has some add but no del */
	for( a = operatorTable[alpha].add; (*a != 0) && (node->state[*a].val != 1); ++a );
	for( d = operatorTable[alpha].del; (*d != 0) && (node->state[*d].val == 0); ++d );
	if( (*a != 0) && (*d == 0) )
	  {
	    /* state regression */
	    memcpy( staticState, node->state, SIZE * sizeof( atom_t ) );
	    for( p = operatorTable[alpha].prec; *p != 0; staticState[*p++].val = 1 );
	    memcpy( buffer[child]->state, staticState, SIZE * sizeof( atom_t ) );
	    for( a = operatorTable[alpha].add; *a != 0; ++a )
	      {
		staticState[*a].val = 1;
		buffer[child]->state[*a].val = 0;
	      }
	    for( d = operatorTable[alpha].del; *d != 0; staticState[*d++].val = 0 );

	    /* check mutexes in the regressed and projected states */
	    for( set = mutexSetList; set != NULL; set = set->next )
	      if( staticState[set->x].val || buffer[child]->state[set->x].val )
		{
		  for( mutex = set->set; mutex != NULL; mutex = mutex->next )
		    if( (staticState[mutex->x].val && staticState[mutex->y].val) ||
			(buffer[child]->state[mutex->x].val && buffer[child]->state[mutex->y].val) )
		      break;
		  if( mutex != NULL )
		    break;
		}
	    if( set != NULL )
	      continue;

	    /* get cached node */
	    tmp = readNodeHash( buffer[child]->state );
	    /* other cases */
	    if( (tmp == NULL) ||
		(node->cost + 1 < tmp->cost) )
	      {
		++generatedNodes;

		/* check for tmp in OPEN */
		if( tmp != NULL )
		  {
		    if( verbose >= 4 )
		      {
			if( tmp->open == 0 )
			  fprintf( stderr, "reopening node %p\n", tmp );
			else
			  fprintf( stderr, "updating OPEN node %p\n", tmp );
		      }

		    removeNodeFromHash( tmp );
		    if( tmp->open == 1 )
		      {
			removeNodeFromOPEN( tmp );
		      }
		  }

		/* set node data */
		buffer[child]->operator = alpha;
		buffer[child]->father = node;
		buffer[child]->cost = node->cost + 1;
		buffer[child]->open = 1;

		/* heuristic computation */
		if( tmp == NULL )
		  {
		    heuristics( buffer[child] );
		  }
		else
		  {
		    buffer[child]->valid = 1;
		    buffer[child]->h_plus = tmp->h_plus;
		    buffer[child]->h_max = tmp->h_max;
		  }

		/* node caching and node space allocation */
		insertIntoNodeHash( buffer[child] );
		buffer[++child] = getNode();
	      }
	  }
      }

  /* free resources and return */
  freeLastNodes( 1 );
  *result = buffer;
  return( child );
}


void
insertNodeIntoBucket( register node_t *node, register int bucket )
{
  node->bucket = bucket;
  node->bucketPrev = NULL;
  node->bucketNext = firstNodeInBucket[bucket];
  
  if( firstNodeInBucket[bucket] == NULL )
    lastNodeInBucket[bucket] = node;
  else
    firstNodeInBucket[bucket]->bucketPrev = node;

  firstNodeInBucket[bucket] = node;
}


void
nodeOrdering( register node_t **buffer, register int size )
{
  register int i, bucket;
  register node_t *invalidNodes, *lastNodeInPrevBucket;

  /* bucket sorting */
  invalidNodes = NULL;
  for( i = 0; i < size; ++i )
    if( buffer[i]->valid == 1 )
      {
	/* registry used buckets */
	bucket = nodeBucket( buffer[i] );
	assert( (bucket >= 0) && (bucket < BUCKETTABLESIZE) );

	minBucket = MIN( minBucket, bucket );
	maxBucket = MAX( maxBucket, bucket );

	insertNodeIntoBucket( buffer[i], bucket );
      }
    else
      {
	buffer[i]->bucketNext = invalidNodes;
	invalidNodes = buffer[i];
      }

  /* bucket threading */
  if( firstNodeInBucket[minBucket] != NULL )
    {
      for( bucket = minBucket, lastNodeInPrevBucket = NULL; bucket <= maxBucket; ++bucket )
	if( firstNodeInBucket[bucket] != NULL )
	  {
	    if( lastNodeInPrevBucket != NULL )
	      {
		lastNodeInPrevBucket->bucketNext = firstNodeInBucket[bucket];
		firstNodeInBucket[bucket]->bucketPrev = lastNodeInPrevBucket;
	      }
	    lastNodeInPrevBucket = lastNodeInBucket[bucket];
	  }
    }

  /* compute new openList head and tail */
  headOPEN = firstNodeInBucket[minBucket];
  tailOPEN = lastNodeInBucket[maxBucket];
}


void
cleanLists( void )
{
  headOPEN = NULL;
  tailOPEN = NULL;
  CLOSE = NULL;
  memset( firstNodeInBucket, 0, BUCKETTABLESIZE * sizeof( node_t * ) );
  memset( lastNodeInBucket, 0, BUCKETTABLESIZE * sizeof( node_t * ) );
}


void
printLists( void )
{
  register node_t *node;

  fprintf( stderr, "OPEN LIST =" );
  for( node = headOPEN; node != NULL; node = node->bucketNext )
    fprintf( stderr, " %p", node );
  fprintf( stderr, "\n" );
  fprintf( stderr, "CLOSE LIST =" );
  for( node = CLOSE; node != NULL; node = node->bucketNext )
    fprintf( stderr, " %p", node );
  fprintf( stderr, "\n" );
}



/*********************************************************
**********************************************************
**
** Operator Compilation
**
**/

int
testOperatorPrecondition( register int *parameters, register int schema )
{
  return( 1 );
}


void
insertOperator( register int *parameters, register int schema )
{
  memset( operatorPrec, 0, 20 * sizeof( int ) );
  memset( operatorAdd, 0, 20 * sizeof( int ) );
  memset( operatorDel, 0, 20 * sizeof( int ) );
  if( (*operatorFunctionTable[schema])( parameters, staticState, staticNewState ) )
    {
      /* resize operatorTable */
      if( numberOperators == operatorTableSize )
	{
	  operatorTableSize = (operatorTableSize == 0 ? 16 : 2 * operatorTableSize);
	  operatorTable = 
	    (operator_t *) realloc( operatorTable, operatorTableSize * sizeof( operator_t ) );
	  if( !operatorTable )
	    fatal( noMoreMemory );
	}

      /* space allocation for precondition, add, del lists */
      operatorTable[numberOperators].prec = (int *) calloc( operatorPrecSize, sizeof( int ) );
      operatorTable[numberOperators].add = (int *) calloc( operatorAddSize, sizeof( int ) );
      operatorTable[numberOperators].del = (int *) calloc( operatorDelSize, sizeof( int ) );
      if( !operatorTable[numberOperators].prec || !operatorTable[numberOperators].add || 
	  !operatorTable[numberOperators].del )
	fatal( noMoreMemory );

      /* fill it */
      memcpy( operatorTable[numberOperators].prec, operatorPrec, operatorPrecSize * sizeof( int ) );
      memcpy( operatorTable[numberOperators].add, operatorAdd, operatorAddSize * sizeof( int ) );
      memcpy( operatorTable[numberOperators].del, operatorDel, operatorDelSize * sizeof( int ) );
      operatorTable[numberOperators].name = strdup( operatorName( parameters, schema ) );
      operatorTable[numberOperators].valid = 1;
      ++numberOperators;
    }
}


void
operatorCompilation( void )
{
  register int i, j, k, *p, *a, *d, *t;

  /* register entry */
  registerEntry( "operatorCompilation( void )" );

  /* generate reachable atoms (i.e., goal cone) */
  memset( relevantAtom, 0, MAXATOMS * sizeof( atom_t ) );
  memset( parents, 0, MAXATOMS * sizeof( int * ) );
  memset( parentsSize, 0, MAXATOMS * sizeof( int ) );

  memcpy( staticState, staticInitialState, MAXATOMS * sizeof( atom_t ) );
  heuristicInitialization( staticState );
  identifyGoalParents( _low_goalAtoms );

  /* generate ground instances of actions */
  memset( staticState, 0, MAXATOMS * sizeof( atom_t ) );
  for( i = 0; i < MAXATOMS; ++i )
    staticState[i].val = relevantAtom[i].val;
  numberOperators = 0;
  instantiateOperators( &insertOperator, &testOperatorPrecondition );

  /* compute valid operators */
  for( i = 0; i < numberOperators; ++i )
    for( p = operatorTable[i].prec; *p != 0; ++p )
      if( cost[*p].cost == INT_MAX )
	{
	  operatorTable[i].valid = 0;
	  if( verbose >= 3 )
	    fprintf( stderr, "invalidating operator %d: %s\n", i, operatorTable[i].name );
	  break;
	}

  /* space allocation */
  invPrecTableSize = (int *) calloc( SIZE, sizeof( int ) );
  invPrecTable = (int **) calloc( SIZE, sizeof( int * ) );
  invAddTableSize = (int *) calloc( SIZE, sizeof( int ) );
  invAddTable = (int **) calloc( SIZE, sizeof( int * ) );
  invDelTableSize = (int *) calloc( SIZE, sizeof( int ) );
  invDelTable = (int **) calloc( SIZE, sizeof( int * ) );
  if( !invPrecTableSize || !invAddTableSize || !invDelTableSize ||
      !invPrecTable || !invAddTable || !invDelTable )
    fatal( noMoreMemory );

  /* inverse table fill */
  for( i = 0; i < numberOperators; ++i )
    if( operatorTable[i].valid == 1 )
      {
	/* inverse table for preconditions */
	for( p = operatorTable[i].prec; *p != 0; ++p )
	  {
	    k = invPrecTableSize[*p];
	    t = invPrecTable[*p];
	    for( j = 0; (j < k) && (*(t+j) != 0) && (*(t+j) != i + 1); ++j );
	    if( (j == k) || (*(t+j) == 0) )
	      {
		if( j + 1 >= k )
		  {
		    invPrecTableSize[*p] = 
		      (invPrecTableSize[*p] == 0 ? 16 : (int)(1.5 * invPrecTableSize[*p]));
		    invPrecTable[*p] = 
		      (int *) realloc( invPrecTable[*p], invPrecTableSize[*p] * sizeof( int ) );
		    if( !invPrecTable[*p] )
		      fatal( noMoreMemory );
		    t = invPrecTable[*p];
		  }
		*(t+j) = i + 1;
		*(t+j+1) = 0;
	      }
	  }

	/* inverse table for add */
	for( a = operatorTable[i].add; *a != 0; ++a )
	  {
	    k = invAddTableSize[*a];
	    t = invAddTable[*a];
	    for( j = 0; (j < k) && (*(t+j) != 0) && (*(t+j) != i + 1); ++j );
	    if( (j == k) || (*(t+j) == 0) )
	      {
		if( j + 1 >= k )
		  {
		    invAddTableSize[*a] = 
		      (invAddTableSize[*a] == 0 ? 16 : (int)(1.5 * invAddTableSize[*a]));
		    invAddTable[*a] = 
		      (int *) realloc( invAddTable[*a], invAddTableSize[*a] * sizeof( int ) );
		    if( !invAddTable[*a] )
		      fatal( noMoreMemory );
		    t = invAddTable[*a];
		  }
		*(t+j) = i + 1;
		*(t+j+1) = 0;
	      }
	  }

	/* inverse table for del */
	for( d = operatorTable[i].del; *d != 0; ++d )
	  {
	    k = invDelTableSize[*d];
	    t = invDelTable[*d];
	    for( j = 0; (j < k) && (*(t+j) != 0) && (*(t+j) != i + 1); ++j );
	    if( (j == k) || (*(t+j) == 0) )
	      {
		if( j + 1 >= k )
		  {
		    invDelTableSize[*d] = 
		      (invDelTableSize[*d] == 0 ? 16 : (int)(1.5 * invDelTableSize[*d]));
		    invDelTable[*d] = 
		      (int *) realloc( invDelTable[*d], invDelTableSize[*d] * sizeof( int ) );
		    if( !invDelTable[*d] )
		      fatal( noMoreMemory );
		    t = invDelTable[*d];
		  }
		*(t+j) = i + 1;
		*(t+j+1) = 0;
	      }
	  }
      }

  /* register exit */
  registerExit();
}


/*
** Compute the admissibility information; i.e., for each atom x, the operator
** alpha is not admissible wrt x (alpha \in notAdmissible[x]) if and only if
** exists a precondition p of alpha such that MUTEX(p,x) and neither x or p
** belongs to add of alpha (i.e., alpha does not remove x or p).
*/
void
admissibleOperatorCompilation( void )
{
  register int x, exists, op, last;
  register int *p, *a;

  /* register entry */
  registerEntry( "admissibleOperatorCompilation( void )" );

  /* space allocation */
  notAdmissible = (int **) calloc( SIZE, sizeof( int * ) );
  notAdmissibleSize = (int *) calloc( SIZE, sizeof( int ) );
  if( !notAdmissible || !notAdmissibleSize )
    fatal( noMoreMemory );

  /* compilation */
  for( x = 1; x < SIZE; ++x )
    {
      last = 0;
      for( op = 0; op < numberOperators; ++op )
	if( operatorTable[op].valid == 1 )
	  {
	    exists = 0;
	    for( p = operatorTable[op].prec; (*p != 0) && (exists == 0); ++p )
	      if( MUTEX( x, *p ) )
		{
		  for( a = operatorTable[op].add; (*a != 0) && (*a != x) && (*a != *p); ++a );
		  exists = (*a == 0);
		}

	    if( exists == 1 )
	      {
		if( last + 1 >= notAdmissibleSize[x] )
		  {
		    notAdmissibleSize[x] = (notAdmissibleSize[x] == 0 ? 16 : (int)(2 * notAdmissibleSize[x]));
		    notAdmissible[x] = (int *) realloc( notAdmissible[x], notAdmissibleSize[x] * sizeof( int ) );
		    if( !notAdmissible[x] )
		      fatal( noMoreMemory );
		  }
		notAdmissible[x][last++] = op;
		notAdmissible[x][last] = 0;
	      }
	  }
    }

  /* register exit */
  registerExit();
}


void
addParents( register int atom, register int *parentList )
{
  register int *ip, *jp, i;

  for( ip = parentList; *ip != 0; ++ip )
    {
      for( jp = parents[atom], i = 0; (jp != 0) && (*jp != 0); ++jp, ++i )
	if( *ip == *jp )
	  break;

      if( (jp == 0) || (*jp == 0) )
	{
	  if( i > parentsSize[atom] - 2 )
	    {
	      parentsSize[atom] = (parentsSize[atom] == 0 ? 4 : 2 * parentsSize[atom]);
	      if( !(parents[atom] = (int *) realloc( parents[atom], parentsSize[atom] * sizeof( int ) )) )
		fatal( noMoreMemory );
	    }
	  parents[atom][i] = *ip;
	  parents[atom][i+1] = 0;
	}
    }
}


void
identifyGoalParents( register int *atoms )
{
  register int *ip;

  if( atoms != NULL )
    for( ip = atoms; *ip != 0; ++ip )
      if( relevantAtom[*ip].val == 0 )
	{
	  ++numberRelevantAtoms;
	  relevantAtom[*ip].val = 1;
	  identifyGoalParents( parents[*ip] );
	}
}



/*********************************************************
**********************************************************
**
** Mutex Compilation
**
**/

void
mutexCompilation( void )
{
  register int i, *p, *a, *d, *op;
  register int change, deleteCondition;
  struct mutex_s *mutex, *tmpMutex, *tmpList;

  /* register entry */
  registerEntry( "mutexCompilation( void )" );

  /* space allocation */
  _mutex = (int *) calloc( numberAtoms * numberAtoms, sizeof( int ) );
  _mutex0 = (int *) calloc( numberAtoms * numberAtoms, sizeof( int ) );
  mutexSet = (mutexSet_t *) calloc( SIZE, sizeof( mutexSet_t ) );
  if( !_mutex || !_mutex0 || !mutexSet )
    fatal( noMoreMemory );

  /* bootstrap mutex set. Briefly, add those <p,q> such that exists an operator
     op with p in ADD(op) and q in DEL(op). After this, protect each initial 
     mutex <p,q> with a mutexes <p,r> (correspondngly <q,r>) for r in PREC(op')
     where op' is an operator with q in ADD(op') (correspondingly p).
  */
  for( i = 0; i < numberOperators; ++i )
    if( operatorTable[i].valid == 1 )
      {
	for( a = operatorTable[i].add; *a != 0; ++a )
	  for( d = operatorTable[i].del; *d != 0; ++d )
	    if( !staticInitialState[*a].val || !staticInitialState[*d].val )
	      newMutex( *a, *d, &mutexList );
      }

  /* go over all mutex <p,q> and "protect" them */
  tmpList = NULL;
  for( mutex = mutexList; mutex != NULL; mutex = mutex->next )
    {
      /* add protection using operators op with p in ADD(op) */
      if( invAddTable[mutex->x] != NULL )
	for( op = invAddTable[mutex->x]; *op != 0; ++op )
	  for( p = operatorTable[(*op)-1].prec; *p != 0; ++p )
	    if( !MUTEX( mutex->y, *p ) &&
		(!staticInitialState[mutex->y].val || !staticInitialState[*p].val) )
	      newMutex( mutex->y, *p, &tmpList );

      /* add protection using operators op with q in ADD(op) */
      if( invAddTable[mutex->y] != NULL )
	for( op = invAddTable[mutex->y]; *op != 0; ++op )
	  for( p = operatorTable[(*op)-1].prec; *p != 0; ++p )
	    if( !MUTEX( mutex->x, *p ) &&
		(!staticInitialState[mutex->x].val || !staticInitialState[*p].val) )
	      newMutex( mutex->x, *p, &tmpList );
    }

  /* fusion the initial mutex set with "protectors" */
  if( tmpList != NULL )
    {
      for( mutex = tmpList; mutex->next != NULL; mutex = mutex->next );
      mutex->next = mutexList;
      mutexList = tmpList;
    }

  /* refinement: delete mutexes (p,q) that don't satisfiy one of the three conditions.
     First condition was already tested in the mutex set bootstrap, so check only
     for second, and third condition.
  */
  do {
    change = 0;
    tmpList = NULL;
    for( mutex = mutexList; mutex != NULL; mutex = tmpMutex )
      {
	tmpMutex = mutex->next;
	deleteCondition = 0;

	/* second condition: exists an operator op st p \in ADD(op) and 
	   q \not\in DEL(op) and (q \in ADD(op) or for all r \in PREC(op),
	   (r,q) is not a mutex).
	*/
	if( (deleteCondition == 0) && (invAddTable[mutex->x] != NULL) )
	  {
	    /* look for a witness */
	    for( op = invAddTable[mutex->x]; *op != 0; ++op )
	      {
		/* q \not\in DEL(op) */
		for( d = operatorTable[(*op)-1].del; (*d != 0) && (*d != mutex->y); ++d );
		if( *d != 0 )
		  continue;
		/* claim: q is not in DEL(op) */

		/* q \in ADD(op) */
		for( a = operatorTable[(*op)-1].add; (*a != 0) && (*a != mutex->y); ++a );
		if( *a != 0 )
		  break;
		/* claim: q is not in ADD(op) */

		/* for all r \in PREC(op), (r,q) is not a mutex */
		for( p = operatorTable[(*op)-1].prec; *p != 0; ++p )
		  if( MUTEX( *p, mutex->y ) )
		    break;
		if( *p == 0 )
		  break;
		/* claim: for some r \in PREC(op), (r,q) is a mutex */
	      }

	    /* delete mutex */
	    if( *op != 0 )
	      deleteCondition = 2;
	  }

	/* third condition: exists an operator op st q \in ADD(op) and 
	   p \not\in DEL(op) and (p \in ADD(op) or for all r \in PREC(op),
	   (r,p) is not a mutex).
	*/
	if( (deleteCondition == 0) && (invAddTable[mutex->y] != NULL) )
	  {
	    /* look for a witness */
	    for( op = invAddTable[mutex->y]; *op != 0; ++op )
	      {
		/* p \not\in DEL(op) */
		for( d = operatorTable[(*op)-1].del; (*d != 0) && (*d != mutex->x); ++d );
		if( *d != 0 )
		  continue;
		/* claim: p is not in DEL(op) */

		/* p \in ADD(op) */
		for( a = operatorTable[(*op)-1].add; (*a != 0) && (*a != mutex->x); ++a );
		if( *a != 0 )
		  break;
		/* claim: p is not in ADD(op) */

		/* for all r \in PREC(op), (r,p) is not a mutex */
		for( p = operatorTable[(*op)-1].prec; *p != 0; ++p )
		  if( MUTEX( *p, mutex->x ) )
		    break;
		if( *p == 0 )
		  break;
		/* claim: for some r \in PREC(op), (r,p) is a mutex */
	      }

	    /* delete mutex */
	    if( *op != 0 )
	      deleteCondition = 3;
	  }

	/* delete mutex */
	if( deleteCondition > 0 )
	  {
	    delMutex( mutex );
	    change = 1;
	  }
      }
  } while( change == 1 );

  /* print mutexes */
  if( verbose >= 4 )
    for( mutex = mutexList; mutex != NULL; mutex = mutex->next )
      {
	fprintf( stderr, "mutex <%s,", readAtomName( mutex->x ) );
	fprintf( stderr, "%s>\n", readAtomName( mutex->y ) );
      }

  /* register exit */
  registerExit();
}


void
mutexSetCompilation( void )
{
  register int i;
  register mutex_t *m, *tm;
  register mutexSet_t *s;

  /* register entry */
  registerEntry( "mutexSetCompilation( void )" );

  /* mutexSet creation */
  for( i = 1; i < SIZE; ++i )
    if( mutexSet[i].size > 0 )
      newMutexSet( i );

  /* mutexSet fill */
  for( m = mutexList; m != NULL; m = tm )
    {
      tm = m->next;
      delMutex( m );
      ++mutexSet[m->x].size;
      if( (mutexSet[m->x].size > mutexSet[m->y].size) ||
	  ((mutexSet[m->x].size == mutexSet[m->y].size) && (m->x > m->y)) )
	{
	  m->next = mutexSet[m->x].set;
	  mutexSet[m->x].set = m;
	}
      else
	{
	  m->next = mutexSet[m->y].set;
	  mutexSet[m->y].set = m;
	}
    }

  /* calculation of set sizes */
  for( s = mutexSetList; s != NULL; s = s->next )
    {
      s->size = 0;
      for( m = s->set; m != NULL; ++s->size, m = m->next );
    }
  assert( mutexList == NULL );

  /* register exit */
  registerExit();
}


void
newMutex( register int x, register int y, register mutex_t **mutexList )
{
  register mutex_t *mutex;

  if( MUTEX( x, y ) == 0 )
    {
      if( !(mutex = (mutex_t *) malloc( sizeof( mutex_t ) )) )
	fatal( noMoreMemory );
      mutex->x = x;
      mutex->y = y;
      mutex->prev = NULL;
      mutex->next = *mutexList;
      if( *mutexList != NULL )
	(*mutexList)->prev = mutex;
      (*mutexList) = mutex;
      MUTEX( x, y ) = 1;
      MUTEX( y, x ) = 1;
      ++mutexSet[x].size;
    }
}


void
delMutex( register mutex_t *mutex )
{
  if( mutex->prev != NULL )
    mutex->prev->next = mutex->next;
  else
    mutexList = mutex->next;
  
  if( mutex->next != NULL )
    mutex->next->prev = mutex->prev;
  
  MUTEX( mutex->x, mutex->y ) = 0;
  MUTEX( mutex->y, mutex->x ) = 0;
  --mutexSet[mutex->x].size;
}


void
newMutexSet( register int x )
{
  register mutexSet_t *set, *prev;

  mutexSet[x].x = x;
  mutexSet[x].set = NULL;
  mutexSet[x].next = NULL;
  for( set = mutexSetList, prev = NULL; (set != NULL) && (set->size > mutexSet[x].size);
       prev = set, set = set->next );
  if( set == NULL )
    {
      if( prev == NULL )
	mutexSetList = &mutexSet[x];
      else
	prev->next = &mutexSet[x];
    }
  else
    {
      if( prev == NULL )
	{
	  mutexSet[x].next = mutexSetList;
	  mutexSetList = &mutexSet[x];
	}
      else
	{
	  prev->next = &mutexSet[x];
	  mutexSet[x].next = set;
	}
    }
}



/*********************************************************
**********************************************************
**
** Main Initialization
**
**/

void
initialize( void )
{
  register int i;

  /* register entry */
  registerEntry( "initialize( void )" );

  /* atomHashTable initialization */
  memset( atomHashTable, 0, ATOMHASHSIZE * sizeof( iatom_t *) );

  /* initilize problem dependent structures */
  fillTable();
  setupHeuristic();

  /* check dimensionality */
  if( numberSchema >= MAXSCHEMA )
    fatal( maxSchema );

  /* initialState identification */
  memset( staticState, 0, MAXATOMS * sizeof( atom_t ) );
  setInitialState();
  memcpy( staticInitialState, staticState, MAXATOMS * sizeof( atom_t ) );

  /* operator instantiation */
  operatorCompilation();

  /* print general data */
  fprintf( stderr, "GENERAL: state size (aligned) = %d\n", NODESIZE );
  fprintf( stderr, "GENERAL: number of atoms = %d\n", numberAtoms );
  fprintf( stderr, "GENERAL: number of operators = %d\n", numberOperators );

  /* other initializations */
  mutexCompilation();
  admissibleOperatorCompilation();
  mutexSetCompilation();

  /* nodeHashTable initialization */
  if( !(nodeHashValues = (int *) malloc( SIZE * sizeof( int ) )) )
    fatal( noMoreMemory );
  for( i = 0; i < SIZE; ++i )
    nodeHashValues[i] = lrand48();
  memset( nodeHashDiameter, 0, NODEHASHSIZE * sizeof( int ) );
  memset( nodeHashTable, 0, NODEHASHSIZE * sizeof( node_t *) );
  claimSize = (numberAtoms + 1) + (4 - (numberAtoms + 1) % 4);

  /* goalState identification */
  initialized = 1;
  memset( staticState, 0, MAXATOMS * sizeof( atom_t ) );
  setInitialState();
  memcpy( staticGoalState, staticState, MAXATOMS * sizeof( atom_t ) );

  /* successful initialization */

  /* register exit */
  registerExit();
}



/*********************************************************
**********************************************************
**
** Planning
**
**/

int
checkSolution( register node_t *node )
{
  register int *atom;

  /* set initial state */
  memcpy( staticState, staticInitialState, MAXATOMS * sizeof( atom_t ) );

  /* apply operators */
  while( (node != NULL) && (node->father != NULL) )
    {
      /* check preconditions */
      for( atom = operatorTable[node->operator].prec; (*atom != 0) && (staticState[*atom].val == 1); ++atom );
      if( *atom != 0 )
	{
	  fprintf( stderr, "****** TENTATIVE SOLUTION IS NOT **********\n" );
	  return( 0 );
	}

      /* apply add-list */
      for( atom = operatorTable[node->operator].add; *atom != 0; ++atom )
	staticState[*atom].val = 1;

      /* apply del-list */
      for( atom = operatorTable[node->operator].del; *atom != 0; ++atom )
	staticState[*atom].val = 0;

      /* next operator */
      node = node->father;
    }

  /* check goal state */
  return( goalState( staticState ) );
}


int
checkProblem( void )
{
  register node_t *node;

  /* initialization */
  node = getNode();
  memcpy( node->state, staticGoalState, SIZE * sizeof( atom_t ) );
  node->operator = -1;
  node->father = NULL;
  node->cost = 0;
  heuristics( node );

  /* return */
  freeLastNodes( 1 );
  return( node->valid );
}


void
printState( register FILE *file, register atom_t *state )
{
  register int i;

  fprintf( file, "====>" );
  for( i = 1; i < SIZE; ++i )
    if( state[i].val == 1 )
      fprintf( file, " %s", readAtomName( i ) );
  fprintf( file, "\n" );
}


void
printNode( register FILE *file, register char *prefix, register node_t *node )
{
  fprintf( file, "%s: node %p (cost = %d, hplus = %d, hmax = %d)\n",
	   prefix, node, node->cost, node->h_plus, node->h_max );
  if( verbose >= 6 )
    printState( file, node->state );
}



/*********************************************************
**********************************************************
**
** Greedy Best-First Search
**
**/

int
startGreedyBFS( void )
{
  register int rv, bound;
  register node_t *result, *node;

  /* register entry */
  registerEntry( "startGreedyBFS( void )" );

  /* cleaning */
  cleanNodeHash();
  cleanLists();

  /* initialization */
  node = getNode();
  memset( firstNodeInBucket, 0, BUCKETTABLESIZE * sizeof( node_t * ) );
  memset( lastNodeInBucket, 0, BUCKETTABLESIZE * sizeof( node_t * ) );
  memcpy( node->state, staticGoalState, SIZE * sizeof( atom_t ) );
  node->operator = -1;
  node->father = NULL;
  node->cost = 0;

  heuristics( node );
  insertIntoNodeHash( node );
  setInitialOPEN( node );
  if( verbose >= 2 )
    printState( stderr, node->state );

  /* search */
  if( (result = greedyBFS()) == NULL )
    {
      rv = 0;
      fprintf( stderr, "SOLUTION: no solution found\n" );
    }
  else
    {
      rv = 1;
      bound = result->cost;
      fprintf( stderr, "SOLUTION: solution found (length = %d)\n", bound );
      if( verbose >= 1 )
	{
	  while( result != NULL )
	    {
	      if( result->father != NULL )
		fprintf( stderr, "  %s\n", operatorTable[result->operator].name );
	      result = result->father;
	    }
	}
    }

  /* register exit */
  registerExit();

  /* return */
  return( rv );
}


node_t *
greedyBFS( void )
{
  register int i, children;
  register node_t *currentNode, *nextNode;
  node_t **buffer;

  /* get first node */
  currentNode = getFirstOPEN();

  /* check for goal */
  if( (currentNode->h_plus == 0) && checkSolution( currentNode ) )
    return( currentNode );

  /* loop */
  while( currentNode != NULL )
    {
      /* print current node */
      if( verbose >= 5 )
	printNode( stderr, "CURRENT NODE", currentNode );

      /* expand node */
      children = nodeExpansion( currentNode, &buffer );

      /* print child */
      if( verbose >= 6 )
	for( i = 0; i < children; ++i )
	  if( buffer[i]->valid == 1 )
	    printNode( stderr, "child", buffer[i] );

      /* test for goal */
      for( i = 0; i < children; ++i )
	if( (buffer[i]->valid == 1) && (buffer[i]->h_plus == 0) && checkSolution( buffer[i] ) )
	  return( buffer[i] );

      /* delete currentNode from OPEN */
      removeNodeFromOPEN( currentNode );

      /* insert children into OPEN */
      nodeOrdering( buffer, children );

      /* next node: first try a probe */
      nextNode = NULL;
      if( algorithm != ASTAR )
	{
	  for( i = 0; i < children; ++i )
	    if( (buffer[i]->valid == 1) &&
		((nextNode == NULL) ||
		 (buffer[i]->h_plus < nextNode->h_plus) ||
		 ((buffer[i]->h_plus == nextNode->h_plus) && (buffer[i]->h_max < nextNode->h_max))) )
	      nextNode = buffer[i];

	  if( (nextNode != NULL) && 
	      ((nextNode->h_plus > currentNode->h_plus) ||
	      ((nextNode->h_plus == currentNode->h_plus) && (nextNode->h_max >= currentNode->h_max))) )
	    nextNode = NULL;
	}

      /* next node: if nothing, select by BFS */
      if( nextNode == NULL )
	nextNode = getFirstOPEN();

      /* select node */
      currentNode = nextNode;
    }
  return( NULL );
}



/*********************************************************
**********************************************************
**
** Registration of Entry/Exit points
**
**/

float
diffTime( register procRegister_t *proc )
{
  register float elapsedTime;

  if( proc->startTime.tv_sec == proc->endTime.tv_sec )
    elapsedTime = (float) (proc->endTime.tv_usec - proc->startTime.tv_usec) / 1000.0;
  else
    elapsedTime = (1000.0 - ((float) proc->startTime.tv_usec) / 1000.0) + 
      1000.0 * ((float) (proc->endTime.tv_sec - proc->startTime.tv_sec - 1)) + 
      ((float) proc->endTime.tv_usec / 1000.0);
  return( elapsedTime );
}


void
registerEntry( register char *procedure )
{
  register procRegister_t *proc;

  if( !(proc = (procRegister_t *) malloc( sizeof( procRegister_t ) )) )
    fatal( noMoreMemory );
  proc->procedure = procedure;
  proc->next = procStackTop;
  procStackTop = proc;
  gettimeofday( &proc->startTime, NULL );
}


int
registerExit( void )
{
  register procRegister_t *proc;

  if( procStackTop != NULL )
    {
      proc = procStackTop;
      gettimeofday( &proc->endTime, NULL );
      proc->diffTime = diffTime( proc );
      fprintf( stderr, "REGISTER: %s took %f ms\n", proc->procedure, proc->diffTime );
      procStackTop = proc->next;
      free( proc );
      return( 1 );
    }
  else
    {
      return( 0 );
    }
}


void
flushAllRegisters( void )
{
  while( registerExit() );
}



/*********************************************************
**********************************************************
**
** Main Section
**
**/

void
_fatal( register int returnCode, register char *s, register char *file, register int line )
{
  switch( returnCode )
    {
    case noMoreMemory:
      fprintf( stderr, "ERROR: error in %s:%d: no more memory\n", file, line );
      break;
    case maxAtoms:
      fprintf( stderr, "ERROR: error in %s:%d: MAXATOMS reached. Recompile\n", file, line );
      break;
    case maxSchema:
      break;
    case noAlgorithm:
      fprintf( stderr, "ERROR: error in %s:%d: No search algotihm specified\n", file, line );
      break;
    case noError:
      break;
    }
  fprintf( stderr, "ERROR: fatal error.\n" );
  printStatistics();
  flushAllRegisters();
  exit( returnCode );
}


void
readParameters( int argc, char **argv )
{
  char *progname;

  /* set some defaults */
  progname = argv[0];
  verbose = 1;
  weight = 5;
  algorithm = GBFS;

  /* parse options */
  while( argc > 1 && *(*++argv) == '-' )
    {
      switch( (*argv)[1] )
	{
	case 'a':
	  algorithm = ASTAR;
	  break;
	case 'v':
	  verbose = atoi( *++argv );
	  --argc;
	  break;
	case 'w':
	  weight = atoi( *++argv );
	  --argc;
	  break;
	default:
	  exit( -1 );
	  break;
	}
      --argc;
    }
  if( argc != 1 )
    exit( -1 );

  /* print parameters */
  fprintf( stderr, "PARAMETER: -v %d\n", verbose );
  fprintf( stderr, "PARAMETER: -w %d\n", weight );
}


void
printStatistics( void )
{
  /* print problem data & statistics */
  nodeHashStatistics();
  fprintf( stderr, "STATISTICS: number expanded nodes = %d\n", expandedNodes );
  fprintf( stderr, "STATISTICS: number generated nodes = %d\n", generatedNodes );
  fprintf( stderr, "STATISTICS: average branching factor = %f\n",
	   (float)generatedNodes / (float)expandedNodes );
}


int
main( int argc, char **argv )
{
  int rv;

  /* identification */
  fprintf( stderr, "PROBLEM: solving problem: %s\n", argv[0] );

  /* register entry */
  registerEntry( "main( int argc, char **argv )" );

  /* initialize */
  readParameters( argc, argv );
  initializeMemoryMgmt();
  initialize();

  /* check if problem has solution */
  if( checkProblem() == 1 )
    {
      switch( algorithm )
	{
	case GBFS:  /* GBFS: f(n) = g(n) + W*h_plus(n) */
	case ASTAR:
	  rv = startGreedyBFS();
	  break;
	}
    }
  else
    {
      fprintf( stderr, "SOLUTION: problem has no solution.\n" );
    }
  printStatistics();
  
  /* register exit */
  registerExit();
  fprintf( stderr, "\n" );

  return( noError );
}
