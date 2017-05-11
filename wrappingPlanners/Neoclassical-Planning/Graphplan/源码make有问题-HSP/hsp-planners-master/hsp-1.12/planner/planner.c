#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/time.h>
#include <assert.h>
#include "planner.h"

/* define's */
#define CLAIMSIZE        256

#define MIN(x,y)         ((x) > (y) ? (y) : (x))
#define MAX(x,y)         ((x) > (y) ? (x) : (y))
#define COST(p)          ((p)->value)
#define NCOST(p)         (((float)(p)->len) * 1.5 + COST( (p)->entry ))
#define DELTA(p)         (COST( (p)->father ) - COST( (p)->entry ))
#define FUNCTION(p)      (((float)(p)->len) + COST( (p)->entry ))
#define TOP              10000.0


/* struct's and typedef's */
struct hashEntry_s
{
  float value;
  struct best_s *ptr;
  struct literal_s situation[SIZE];
  struct hashEntry_s *next;
} hashEntry_s;

struct path_s
{
  char name[256];
  struct path_s *next;
} path_s;

struct best_s
{
  int len;
  int valid;
  struct hashEntry_s *entry;
  struct hashEntry_s *father;
  struct path_s *path;
  struct best_s *next;
  struct best_s *prev;
} best_s;


/* global variables */
static int threshold, loopDetection;
static int updates = 0;

static int steps;
static int maxMinutes;
static float avgBranching = 0, maxBranching = 0;
static float stepsDone = 0, ties = 0;
static struct timeval startTime;

static int hashDiameter[HASHSIZE];
static int hashValues[SIZE];
static int hashNumElem = 0;
static int heuristicValues = 0, hits = 0, offspringMaxSize;
static struct hashEntry_s  *hashTable[HASHSIZE];
static struct hashEntry_s  *hashPool;
static struct hashEntry_s  *poolBoundary;
static int hashClaimSize;

static int atomHashDiameter[ATOMHASHSIZE];
static int atomHashNumElem = 0;
static struct atomHashEntry_s  *atomHashTable[ATOMHASHSIZE];
static struct atomHashEntry_s  *atomHashPool;
static struct atomHashEntry_s  *atomPoolBoundary;
static int atomHashClaimSize;

static int nBestUpdates, nBestSize;
static struct best_s *nBestBuffer, *backBuffer, *tmpBuffer;
static struct path_s *nBestPath;

/* atoms influence */
struct heuristicOperator_s **influence[MAXATOMS];
static int influenceLimit[MAXATOMS];
extern struct heuristicOperator_s **operatorsToFire;
extern int *operatorDisplay;

/* global extern variables */
int numActions;
int **values, **vars;
int number, operatorNumber;
int operatorNumber;
int possibleHeuristicOperators, possibleHeuristicOpersWithNP;
int operatorsSize = 0, heuristicOperatorsSize = 0, heuristicOpersWithNPSize = 0;
int initialized = 0;

int (*actionTable[MAXACTIONS])( register int *, register struct literal_s *, register struct literal_s * );
struct literal_s   situation[SIZE];
struct literal_s   newSituation[SIZE];
struct operator_s  *operators = NULL;
struct heuristicOperator_s  *heuristicOperators = NULL;
struct heuristicOperator_s  **heuristicOpersWithNP = NULL;

int **parents, *parentsSize, *relevantAtoms;
int numberAtoms = 0;
int numberRelevantAtoms;
struct hashEntry_s *father;


/* prototypes */
float heuristicFunction( register struct literal_s * );
void printSituation( register FILE *, register struct literal_s *);
void insertOperator( register int *, register int );
void insertPossibleHeuristicOperator( register int *, register int );
float diffTime( void );


int
sitCmp( register struct literal_s *sit1, register struct literal_s *sit2 )
{
  register struct literal_s *sp1, *sp2;

  for( sp1 = sit1, sp2 = sit2; (sp1 < &sit1[SIZE]) && (sp1->lit == sp2->lit); ++sp1, ++sp2 );
  return( !(sp1 == &sit1[SIZE]) );
}


unsigned 
hashFunction( register struct literal_s *sit )
{
  register unsigned val = 0;
  register int *hp;
  register struct literal_s *sp;

  for( sp = sit, hp = hashValues; sp < &sit[(initialized ? numberAtoms + 1 : SIZE)]; ++sp, ++hp )
    val += (sp->lit ? *hp : 0);
  return( val % HASHSIZE );
}


struct hashEntry_s *
readHash( register struct literal_s *sit )
{
  register struct hashEntry_s *entry;

  for( entry = hashTable[hashFunction( sit )]; entry != NULL; entry = entry->next )
    {
      if( !sitCmp( sit, entry->situation ) )
	return( entry );
    }
  return( NULL );
}


struct hashEntry_s *
insertHash( register struct literal_s *sit, register float value )
{
  register unsigned index;
  register struct hashEntry_s *entry;

  if( !(entry = readHash( sit )) )
    {
      index = hashFunction( sit );
      if( hashPool >= poolBoundary )
	{
	  hashClaimSize = (hashClaimSize == 0 ? CLAIMSIZE : 2 * hashClaimSize);
	  hashPool = (struct hashEntry_s *) malloc( hashClaimSize * sizeof( struct hashEntry_s ) );
	  if( !hashPool )
	    fatal( noMoreMemory );
	  poolBoundary = &hashPool[hashClaimSize];
	}
      entry = hashPool++;
      memcpy( entry->situation, sit, SIZE * sizeof( struct literal_s ) );
      entry->ptr = NULL;
      entry->value = value;
      entry->next = hashTable[index];
      hashTable[index] = entry;
      ++hashNumElem;
      ++(hashDiameter[index]);
    }
  else
    {
      entry->value = value;
    }
  return( entry );
}


void
hashStat( void )
{
  int *p, max, total, entries;

  max = 0;
  total = 0;
  entries = 0;
  for( p = hashDiameter; p != &hashDiameter[HASHSIZE]; ++p )
    {
      total += *p;
      if( *p > max )
	max = *p;
      entries += (*p ? 1 : 0);
    }

  fprintf( stderr, "\n" );
  fprintf( stderr, "number of elements in hash table = %d\n", hashNumElem );
  fprintf( stderr, "diameter of hash table = %d\n", max );
  if( entries != 0 )
    fprintf( stderr, "average length of each hash entry = %f\n", (float) total / (float) entries );
  fprintf( stderr, "heuristic evaluations = %d\n", heuristicValues );
  fprintf( stderr, "hits on hash table = %d\n", hits );
  fprintf( stderr, "updates of entries in the hash table = %d\n", updates );
  if( stepsDone != 0 )
    {
      fprintf( stderr, "average number of ties = %f\n", ties / stepsDone );
      fprintf( stderr, "average branching factor = %f\n", avgBranching / stepsDone );
    }
  fprintf( stderr, "maximum branching factor = %f\n", maxBranching );
  fprintf( stderr, "number of atoms = %d\n", numberAtoms );
}


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


struct atomHashEntry_s *
insertAtomHash( register int *parameters )
{
  register unsigned index;
  register struct atomHashEntry_s *entry;

  if( numberAtoms == MAXATOMS )
    fatal( maxAtoms );
  
  if( initialized )
    fatal1( unexpectedAtom, atomName( parameters ) );

  index = atomHashFunction( parameters );
  if( atomHashPool >= atomPoolBoundary )
    {
      atomHashClaimSize = (atomHashClaimSize == 0 ? CLAIMSIZE : 2 * atomHashClaimSize);
      atomHashPool = (struct atomHashEntry_s *) malloc( atomHashClaimSize * sizeof( struct atomHashEntry_s ) );
      if( !atomHashPool )
	fatal( noMoreMemory );
      atomPoolBoundary = &atomHashPool[atomHashClaimSize];
    }
  entry = atomHashPool++;
  memcpy( entry->parameters, parameters, MAXPARAMETERS * sizeof( int ) );
  strcpy( entry->name, atomName( parameters ) );
  entry->idx = ++numberAtoms;
  entry->next = atomHashTable[index];
  atomHashTable[index] = entry;
  ++atomHashNumElem;
  ++(atomHashDiameter[index]);
  return( entry );
}


struct atomHashEntry_s *
readAtomHash( register int *parameters )
{
  register struct atomHashEntry_s *entry;
  register int *ip, *jp;

  for( entry = atomHashTable[atomHashFunction( parameters )]; entry != NULL; entry = entry->next )
    {
      for( ip = parameters, jp = entry->parameters; (*ip != -1) && (*ip == *jp); ++ip, ++jp);
      if( (*ip == -1) && (*jp == -1) )
	return( entry );
    }
  return( insertAtomHash( parameters ) );
}


char *
readAtomByNumber( register int index )
{
  register int i;
  register struct atomHashEntry_s *entry;

  for( i = 0; i < ATOMHASHSIZE; ++i )
    {
      for( entry = atomHashTable[i]; entry != NULL; entry = entry->next )
	if( entry->idx == index )
	  break;
      if( entry != NULL )
	break;
    }
  return( atomName( entry->parameters ) );
}


void
updateValue( register struct hashEntry_s *node, register struct hashEntry_s *bestChild, register float inc )
{
  node->value = bestChild->value + inc;
}


struct hashEntry_s * 
hashValue( register struct literal_s *sit )
{
  register struct hashEntry_s *entry;
  register float cost;

  if( !initialized )
    {
      heuristicFunction( sit );
      return( NULL );
    }
  else
    {
      if( (entry = readHash( sit )) != NULL )
	{
	  ++hits;
	}
      else
	{
	  cost = heuristicFunction( sit );
	  entry = insertHash( sit, (float) cost );
	}
      return( entry );
    }
}


float
heuristicFunction( register struct literal_s *sit )
{
  register float cost;

  ++heuristicValues;
  if( (cost = heuristicCost( sit )) == -1.0 )
    cost = TOP;
  return( cost );
}


void
addInfluence( register int prec, register struct heuristicOperator_s *operator )
{
  register int idx, oldSize;
  register struct heuristicOperator_s **ip;

  for( ip = influence[prec]; (ip != NULL) && (*ip != NULL); ++ip )
    if( *ip == operator )
      break;

  if( (ip == NULL) || (*ip == NULL) )
    {
      idx = ip - influence[prec];
      if( (ip == NULL) || (idx+1 == influenceLimit[prec]) )
	{
	  oldSize = (influenceLimit[prec] == 0 ? 4 : influenceLimit[prec]);

	  influenceLimit[prec] = (influenceLimit[prec] == 0 ? 4 : 2 * influenceLimit[prec]);
	  if( !(influence[prec] = (struct heuristicOperator_s **) 
		                     realloc( influence[prec], influenceLimit[prec] * sizeof( struct heuristicOperator_s *) )) )
	    fatal( noMoreMemory );
	  memset( &influence[prec][(!ip ? 0 : idx+1)], 0, oldSize * sizeof( struct heuristicOperator_s * ) );
	  ip = &influence[prec][idx];
	}
      *ip = operator;
    }

  /* check point */
  assert( influence[prec][influenceLimit[prec]-1] == NULL );
}


void
initialize( void )
{
  register int i, *prec;
  extern int goalAtoms[];

  /* general initialization */
  nBestBuffer = (struct best_s *) calloc( nBestSize + offspringMaxSize, sizeof( struct best_s ) );
  backBuffer = (struct best_s *) calloc( nBestSize + offspringMaxSize, sizeof( struct best_s ) );
  if( !nBestBuffer || !backBuffer )
    fatal( noMoreMemory );

  /* hashTable initialization */
  for( i = 0; i < SIZE; ++i )
    hashValues[i] = lrand48();
  memset( &hashDiameter, 0, HASHSIZE * sizeof( int ) );
  memset( &hashTable, 0, HASHSIZE * sizeof( struct hashEntry_s *) );
  hashPool = poolBoundary = NULL;
  hashClaimSize = 0;

  /* atomHashTable initialization */
  memset( &atomHashDiameter, 0, ATOMHASHSIZE * sizeof( int ) );
  memset( &atomHashTable, 0, ATOMHASHSIZE * sizeof( struct atomHashEntry_s *) );
  atomHashPool = atomPoolBoundary = NULL;
  atomHashClaimSize = 0;

  fillTable();
  setupHeuristic();

  /* check dimensionality */
  if( numActions >= MAXACTIONS )
    fatal( maxOperators );

  /* generate reachable atoms (i.e., goal cone) */
  relevantAtoms = (int *) calloc( MAXATOMS, sizeof( int ) );
  parents = (int **) calloc( MAXATOMS, sizeof( int * ) );
  parentsSize = (int *) calloc( MAXATOMS, sizeof( int ) );
  if( !relevantAtoms || !parents || !parentsSize )
    fatal( noMoreMemory );

  numberRelevantAtoms = 0;
  memset( situation, 0, SIZE * sizeof( struct literal_s ) );
  setInitialSituation();
  hashValue( situation );
  identifyGoalParents( goalAtoms );
  initialized = 1;

  /* instantiate possible operators */
  possibleHeuristicOperators = 0;
  memcpy( situation, relevantAtoms, SIZE * sizeof( struct literal_s ) );
  instantiateOperators( &insertPossibleHeuristicOperator, &heuristicStillPossible );
  operatorDisplay = (int *) calloc( possibleHeuristicOperators, sizeof( int ) );
  operatorsToFire = (struct heuristicOperator_s **) calloc( possibleHeuristicOperators + 1, sizeof( struct heuristicOperator_s * ) );
  if( !operatorDisplay || !operatorsToFire )
    fatal( noMoreMemory );

  /* detect influences */
  memset( influence, 0, MAXATOMS * sizeof( struct heuristicOperator_s ** ) );
  memset( influenceLimit, 0, MAXATOMS * sizeof( int ) );

  possibleHeuristicOpersWithNP = 0;
  for( i = 0; i < possibleHeuristicOperators; ++i )
    {
      if( heuristicOperators[i].prec[0] == 0 )
	{
	  if( possibleHeuristicOpersWithNP == heuristicOpersWithNPSize )
	    {
	      heuristicOpersWithNPSize = (heuristicOpersWithNPSize == 0 ? CLAIMSIZE : 2 * heuristicOpersWithNPSize);
	      heuristicOpersWithNP = (struct heuristicOperator_s **) realloc( heuristicOpersWithNP, heuristicOpersWithNPSize * sizeof( struct heuristicOperator_s * ) );
	      if( !heuristicOpersWithNP )
		fatal( noMoreMemory );
	    }
	  heuristicOpersWithNP[possibleHeuristicOpersWithNP++] = &heuristicOperators[i];
	}
      else
	for( prec = heuristicOperators[i].prec; *prec != 0; ++prec )
	  addInfluence( *prec, &heuristicOperators[i] );
    }
  if( possibleHeuristicOpersWithNP != 0 )
    heuristicOpersWithNP[possibleHeuristicOpersWithNP] = NULL;

  /* successful initialization */
  fprintf( stderr, "number of atoms = %d\n", numberAtoms );
  fprintf( stderr, "number of relevant atoms = %d\n", numberRelevantAtoms );
  fprintf( stderr, "number of possible heuristic operators = %d\n", possibleHeuristicOperators );
  fprintf( stderr, "successful initialization\n\n" );
}


void
resize( void )
{
  fatal( noRoomOffspring );
}


int
nBestCompare( register const void *e1, register const void *e2 )
{
  register float val, tmp;
  register struct best_s *b1, *b2;

  b1 = (struct best_s *) e1;
  b2 = (struct best_s *) e2;

  if( !b1->valid || !b2->valid )
    return( !b1->valid ? 1 : -1 );

  if( (val = DELTA( b2 )  - DELTA( b1 )) != 0.0 )
    return( val > 0.0 ? 1 : -1 );
  else if( (tmp = FUNCTION( b1 ) - FUNCTION( b2 )) != 0.0 )
    return( tmp > 0.0 ? 1 : -1 );
  else
    return( 0 );
}


void
insertPossibleHeuristicOperator( register int *parameters, register int action )
{
  register int oldSize;
  extern int preclist[], addlist[];

  if( (*heuristicActionTable[action])( parameters ) )
    {
      if( possibleHeuristicOperators == heuristicOperatorsSize )
	{
	  oldSize = (heuristicOperatorsSize == 0 ? CLAIMSIZE : heuristicOperatorsSize);
	  heuristicOperatorsSize = (heuristicOperatorsSize == 0 ? CLAIMSIZE : 2 * heuristicOperatorsSize);
	  if( !(heuristicOperators =
		(struct heuristicOperator_s *) realloc( heuristicOperators, 
							heuristicOperatorsSize * sizeof( struct heuristicOperator_s ) )) )
	    fatal( noMoreMemory );
	  memset( &heuristicOperators[possibleHeuristicOperators], 0, oldSize * sizeof( struct heuristicOperator_s ) );
	}
      strcpy( heuristicOperators[possibleHeuristicOperators].name, operatorName( parameters, action ) );
      memcpy( heuristicOperators[possibleHeuristicOperators].prec, preclist, MAXPARAMETERS * sizeof( int ) );
      memcpy( heuristicOperators[possibleHeuristicOperators].add, addlist, MAXPARAMETERS * sizeof( int ) );
      heuristicOperators[possibleHeuristicOperators].valid = 1;
      heuristicOperators[possibleHeuristicOperators].id = possibleHeuristicOperators;
      ++possibleHeuristicOperators;
    }
}


void
insertOperator( register int *parameters, register int action )
{
  register int oldSize;

  if( (*actionTable[action])( parameters, situation, newSituation ) )
    {
      /* we have a valid operator */
      if( operatorNumber == operatorsSize )
	{
	  oldSize = (operatorsSize == 0 ? CLAIMSIZE : operatorsSize);
	  operatorsSize = (operatorsSize == 0 ? CLAIMSIZE : 2 * operatorsSize);
	  if( !(operators = (struct operator_s *) realloc( operators, operatorsSize * sizeof( struct operator_s ) )) )
	    fatal( noMoreMemory );
	  memset( &operators[operatorNumber], 0, oldSize * sizeof( struct operator_s ) );
	}
      strcpy( operators[operatorNumber].name, operatorName( parameters, action ) );
      memcpy( operators[operatorNumber].parameters, parameters, MAXPARAMETERS * sizeof( int ) );
      operators[operatorNumber].function = actionTable[action];
      ++operatorNumber;
    }
}


int
stillPossible( register int *parameters, register int action )
{
  extern int (*actionPreconditionsTable[MAXACTIONS])( register int *, register struct literal_s * );
  return( (*actionPreconditionsTable[action])( parameters, situation ) );
}


void
buildParameterList( register int *parameters, register int action, register int *ip,
		    void (*insertOperator)( register int *, register int ),
		    int (*stillPossible)( register int *, register int ) )
{
  register int *vp, *jp;

  if( *ip != 0 )
    {
      for( vp = values[numActions * (*ip - 1) + action]; *vp != 0; ++vp )
	{
	  for( jp = &parameters[1]; jp < &parameters[MAXPARAMETERS]; ++jp )
	    if( (*jp > 0) && (*jp == *vp) )
	      break;
	  if( jp == &parameters[MAXPARAMETERS] )
	    {
	      parameters[*ip] = *vp;
	      if( (*stillPossible)( parameters, action ) )
		buildParameterList( parameters, action, ip + 1, insertOperator, stillPossible );
	    }
	}
      parameters[*ip] = 0;
    }
  else
    {
      if( (*stillPossible)( parameters, action ) )
	{
	  /* insert delimiting marker */
	  for( jp = parameters; *jp > 0; ++jp );
	  *jp = -1;
	  
	  /* insert operator */
	  (*insertOperator)( parameters, action );
	}
    }
}


void
instantiateOperators( void (*insertOperator)( register int *, register int ),
		      int (*stillPossible)( register int *, register int ) )
{
  register int action;
  static int parameters[MAXPARAMETERS];

  for( action = 0; action < numActions; ++action )
    {
      memset( parameters, 0, MAXPARAMETERS * sizeof( int ) );
      parameters[0] = action + 1;
      buildParameterList( parameters, action, vars[action], insertOperator, stillPossible );
    }
}


void
printList( register struct best_s *entry )
{
  if( entry != NULL )
    {
      fprintf( stderr, "%p:<%4.1f,%4.1f> ", entry->entry, DELTA( entry ), COST( entry->entry ) );
      printList( entry->next );
    }
}


int
nBestExpand( register struct best_s *first, register int num1, register int *num2 )
{
  register int i, j, k, valid;
  register struct hashEntry_s *entry;

  j = k = 0;
  operatorNumber = 0;
  memcpy( situation, first->entry->situation, SIZE * sizeof( struct literal_s ) );
  instantiateOperators( &insertOperator, &stillPossible );

  memcpy( situation, first->entry->situation, SIZE * sizeof( struct literal_s ) );
  for( i = 0; i < operatorNumber; ++i )
    {
      memcpy( newSituation, situation, SIZE * sizeof( struct literal_s ) );
      if( (*operators[i].function)( operators[i].parameters, situation, newSituation ) )
	{
	  if( j == offspringMaxSize )
	    resize();

	  /* loop detection */
	  if( loopDetection )
	    {
	      entry = readHash( newSituation );
	     if( (entry != NULL) && (entry->ptr == NULL) )
	       valid = 0;
	      else
		valid = 1;
	    }
	  nBestBuffer[num1+j].entry = hashValue( newSituation );
	  nBestBuffer[num1+j].father = first->entry;

	  if( !(nBestBuffer[num1+j].path = (struct path_s *) malloc( sizeof( struct path_s ) )) )
	    fatal( noMoreMemory );
	  strcpy( nBestBuffer[num1+j].path->name, operators[i].name );
	  nBestBuffer[num1+j].path->next = first->path;

	  nBestBuffer[num1+j].len = first->len + 1;
	  if( loopDetection && !valid )
	    nBestBuffer[num1+j].valid = 0;
	  else
	    nBestBuffer[num1+j].valid = (nBestBuffer[num1+j].entry->value == TOP ? 0 : 1);
	  nBestBuffer[num1+j].next = NULL;
	  nBestBuffer[num1+j].prev = NULL;
	  k += nBestBuffer[num1+j].valid;
	  ++j;
	}
      memcpy( situation, first->entry->situation, SIZE * sizeof( struct literal_s ) );
    }
  *num2 = j;
  return( k );
}


int
nBest( void )
{
  int num1, num2, fullBuffer;
  register int i, j, k, envion;
  register int numEntries, numChildren;
  register int greedy, impasses;
  register float elapsedTime;
  register struct best_s *first, *entry, *last;
  static struct best_s toExpand, bestChild;

  assert( nBestSize > 0 );

  /* initialization */
  memset( situation, 0, SIZE * sizeof( struct literal_s ) );
  setInitialSituation();
  toExpand.entry = hashValue( situation );
  toExpand.father = NULL;
  toExpand.path = NULL;
  toExpand.len = 0;
  toExpand.valid = 1;
  toExpand.next = NULL;
  toExpand.prev = NULL;
  first = NULL;
  last = NULL;
  numEntries = 0;
  fullBuffer = 0;

  /* check goal reacheability */
  if( COST( toExpand.entry ) == TOP )
    {
      fprintf( stderr, "error: unreacheable goal\n" );
      return( 0 );
    }

  fprintf( stderr, "initial value = %f\n", COST( toExpand.entry ) );
  steps = 0;

  /* re-initialize threshold */
  if( threshold == -1 )
    threshold = 2 * (int) COST( toExpand.entry );

  /* search */
  envion = 0;
  greedy = 1;
  impasses = 0;
  nBestPath = toExpand.path;
  while( !goalSituation( toExpand.entry->situation ) )
    {
      /* check assertion and timing */
      assert( toExpand.valid == 1 );
      elapsedTime = diffTime();
      if( elapsedTime > (float) (maxMinutes * 60 * 1000) )
	fatal( maxTime );

      /* current situation */
      father = toExpand.entry;
      num1 = numEntries;

      /* feedback */
      if( toExpand.father != NULL )
	fprintf( stderr, "id = %u:%p, cost = %d + %f, bufferSize = %d, ", 
		 hashFunction( toExpand.entry->situation ), toExpand.entry,
		 toExpand.len, COST( toExpand.entry ), 
		 numEntries );

      /* expand situation */
      numChildren = nBestExpand( &toExpand, num1, &num2 );

      /* remove duplicate children */
      for( i = 0; i < num2; ++i )
	if( nBestBuffer[num1+i].valid )
	  for( j = i+1; j < num2; ++j )
	    if( nBestBuffer[num1+i].entry == nBestBuffer[num1+j].entry )
	      {
		nBestBuffer[num1+i].valid = 0;
		--numChildren;
		break;
	      }

      /* order children and bestChild selection (random ties) */
      qsort( &nBestBuffer[num1], num2, sizeof( struct best_s ), nBestCompare );
      if( numChildren > 0 )
	{
	  for( i = 0; (i < num2) && (nBestCompare( &nBestBuffer[num1], &nBestBuffer[num1+i] ) == 0); ++i );
	  bestChild = nBestBuffer[num1 + (lrand48() % i)];
	}

      /* delete parent from buffer & check for no children */
      toExpand.valid = 0;
      if( numChildren == 0 )
	{
	  /* dead-end with no backtracks*/
	  if( first == NULL )
	    return( 0 );

	  toExpand = *first;
	  first->entry->ptr = NULL;
	  first->valid = 0;

	  first = first->next;
	  if( first != NULL )
	    first->prev = NULL;
	  else
	    last = NULL;
	  continue;
	}

      /* statistics */
      fprintf( stderr, "branching = %d, ", numChildren );
      avgBranching += numChildren;
      maxBranching = (numChildren > maxBranching ? numChildren : maxBranching);

      /* remove duplicates */
      if( !fullBuffer )
	{
	  for( i = 0; i < num2; ++i )
	    if( nBestBuffer[num1+i].valid )
	      for( entry = first; entry != NULL; entry = entry->next )
		if( nBestBuffer[num1+i].entry == entry->entry )
		  {
		    /* we keep the one with less function value */
		    if( FUNCTION( entry ) <= FUNCTION( &nBestBuffer[num1+i]) )
		      {
			nBestBuffer[num1+i].valid = 0;
			--numChildren;
			break;
		      }
		    else
		      {
			if( entry->prev == NULL )
			  {
			    first->valid = 0;
			    first = first->next;
			    if( first != NULL )
			      first->prev = NULL;
			    else
			      last = NULL;
			  }
			else
			  {
			    entry->valid = 0;
			    entry->prev->next = entry->next;
			    if( entry->next != NULL )
			      entry->next->prev = entry->prev;
			    else
			      last = entry->prev;
			  }
			entry->entry->ptr = NULL;
			--numEntries;
			break;
		      }
		  }
	}

#if 0 /* DEBUGGING */
      /* check some assertions */
      if( !fullBuffer )
	{
	  for( entry = first; entry != NULL; entry = entry->next )
	    if( entry->entry == bestChild.entry )
	      break;
	  for( i = 0; i < num2; ++i )
	    if( nBestBuffer[num1+i].valid && nBestBuffer[num1+i].entry == bestChild.entry )
	      break;
	  assert( ((entry != NULL) || (i != num2)) && ((entry == NULL) || (i == num2)) );
	}
#endif

      /* insert children into n-buffer */
      if( !fullBuffer )
	{
	  for( i = 0; i < num2; ++i )
	    if( nBestBuffer[num1+i].valid )
	      break;

	  if( i != num2 )
	    {
	      j = i;
	      for( k = j+1; k < num2; ++k )
		if( nBestBuffer[num1+k].valid )
		  {
		    nBestBuffer[num1+j].entry->ptr = &nBestBuffer[num1+j];
		    nBestBuffer[num1+j].next = &nBestBuffer[num1+k];
		    nBestBuffer[num1+k].prev = &nBestBuffer[num1+j];
		    j = k;
		  }
	      nBestBuffer[num1+i].prev = NULL;
	      nBestBuffer[num1+j].next = NULL;
	      nBestBuffer[num1+j].entry->ptr = &nBestBuffer[num1+j];

	      /* link */
	      if( last == NULL )
		{
		  first = &nBestBuffer[num1+i];
		  first->prev = NULL;
		}
	      else
		{
		  last->next = &nBestBuffer[num1+i];
		  nBestBuffer[num1+i].prev = last;
		}
	      last = &nBestBuffer[num1+j];
	    }
	}

#if 0 /* DEBUGGING */
      /* check some assertions */
      if( !fullBuffer )
	{
	  for( entry = first; entry != NULL; entry = entry->next )
	    if( entry->entry == bestChild.entry )
	      break;
	  assert( entry != NULL );
	  assert( entry->entry->ptr == entry );
	}
#endif

      /* update state variables */
      numEntries = (fullBuffer ? numEntries : (numEntries+numChildren < nBestSize ? numEntries+numChildren : nBestSize));
      fullBuffer = (fullBuffer ? 1 : (numEntries == nBestSize ? 1 : 0));

#if 0 /* OLD POSITION FOR UPDATE */
      /* update hashTable */
      if( nBestUpdates )
	{
	  updateValue( toExpand.entry, bestChild.entry, 1.0 );
	}
#endif

      /* selection */
      if( greedy )
	{
	  bestChild.valid = 0;
	  entry = (!fullBuffer ? bestChild.entry->ptr : &bestChild);
	  impasses += (DELTA( entry ) <= 0.0 ? 1 : 0);
	  if( impasses == threshold )
	    greedy = 0;
	}
      else
	{
	  entry = first;
	  greedy = 1;
	  impasses = 0;
	  
	  /* dead-end? */
	  if( entry == NULL )
	    return( 0 );
	}

      /* update hashTable */
      if( nBestUpdates )
	{
	  updateValue( toExpand.entry, bestChild.entry, 1.0 );
	}

      /* swap situations: entry->valid == 0 iff fullBuffer == 1 */
      if( entry->valid )
	{
	  if( entry->prev != NULL )
	    {
	      entry->prev->next = entry->next;
	      if( entry->next != NULL )
		entry->next->prev = entry->prev;
	    }
	  else
	    {
	      first = first->next;
	      if( first != NULL )
		first->prev = NULL;
	    }
	  --numEntries;
	}
      toExpand = *entry;
      nBestPath = entry->path;
      toExpand.valid = 1;
      entry->valid = 0;
      entry->entry->ptr = NULL;

      /* feedback */
      fprintf( stderr, "delta = %f, impasses = %d\n", DELTA( &toExpand ), impasses );

      /* swap buffers */
      i = 0;
      for( entry = first; (entry != NULL) && (i < numEntries); entry = entry->next )
	{
	  backBuffer[i] = *entry;
	  backBuffer[i].entry->ptr = &backBuffer[i];
	  backBuffer[i].next = &backBuffer[i+1];
	  if( i > 0 )
	    backBuffer[i].prev = &backBuffer[i-1];
	  ++i;
	}
      if( i > 0 )
	backBuffer[i-1].next = NULL;

      memset( &backBuffer[i], 0, (nBestSize + offspringMaxSize - i) * sizeof( struct best_s ) );
      tmpBuffer = nBestBuffer;
      nBestBuffer = backBuffer;
      backBuffer = tmpBuffer;
      first = (i > 0 ? &nBestBuffer[0] : NULL);
      last = (i > 0 ? &nBestBuffer[i-1] : NULL);

#if 0 /* DEBUGGING */
      {
	register struct best_s *before;
	
	/* check some assertions */
	for( before = NULL, entry = first; entry != NULL; before = entry, entry = entry->next )
	  assert( entry->prev == before );
	assert( before == last );
      }
#endif

      /* increment steps */
      ++steps;
      ++stepsDone;
    }

  /* return */
  return( 1 );
}


void
printSituation( register FILE *file, register struct literal_s *sit )
{
  register int i;

  fprintf( file, "\n\nsituation =\n" );
  for( i = 0; i < MAXATOMS; ++i )
    if( sit[i].lit >= 0 )
      fprintf( file, "%s\n", readAtomByNumber( i ) );
}


void
printPath( register FILE *file, register struct path_s *path )
{
  if( path != NULL )
    {
      printPath( file, path->next );
      fprintf( file, "%s\n", path->name );
    }
}


void
printHelp( char *progname )
{
  fprintf( stderr, "Solver for %s problem. Options:\n"
	   "\t-h    \tThis help.\n"
	   "\t-l    \tLoop detection (default: no).\n"
	   "\t-u    \tMake updates (default: no).\n"
	   "\t-i <n>\tSolve the problem with # max impasses = <n> (default: 2*h(s_0)).\n"
	   "\t-m <n>\tMax minutes to run (default: 30).\n"
	   "\t-N <n>\tBuffer size (default: 1000).\n"
	   "\t-r <n>\tSet the random seed to <n> (default: 100).\n", progname );
  exit( 0 );
}


float
diffTime( void )
{
  register float elapsedTime;
  struct timeval tv;

  gettimeofday( &tv, NULL );
  if( startTime.tv_sec == tv.tv_sec )
    elapsedTime = (float) (tv.tv_usec - startTime.tv_usec) / 1000.0;
  else
    elapsedTime = (1000.0 - ((float) startTime.tv_usec) / 1000.0) + 
                  1000.0 * ((float) (tv.tv_sec - startTime.tv_sec - 1)) + 
                  ((float) tv.tv_usec / 1000.0);
  return( elapsedTime );
}


void
_fatal( register int returnCode, register char *s, register char *file, register int line )
{
  register float totalTime;

  switch( returnCode )
    {
    case noMoreMemory:
      fprintf( stderr, "error in %s:%d: no more memory\n", file, line );
      break;
    case noRoomOffspring:
      fprintf( stderr, "error in %s:%d: no enough space for offspring\n", file, line );
      break;
    case maxAtoms:
      fprintf( stderr, "error in %s:%d: MAXATOMS reached. Recompile\n", file, line );
      break;
    case unexpectedAtom:
      fprintf( stderr, "error in %s:%d: unexpected atom %s\n", file, line, s );
      break;
    case maxOperators:
      fprintf( stderr, "error in %s:%d: instantiated operators exceed maximum. Recompile\n", file, line );
      break;
    case badUsage:
      fprintf( stderr, "usage: %s [-h] [-i <n>] [-l] [-u] [-m <n>] [-N <n>] [-r <n>]\n", s );
      break;
    case maxTime:
      fprintf( stderr, "error in %s:%d: max time reached\n", file, line );
      exit( returnCode );
      break;
    case noError:
      break;
    }

  fprintf( stderr, "NO SOLUTION\n" );
  fprintf( stderr, "END PLAN\n" ); 
  fprintf( stdout, "NO SOLUTION\n" );
  fprintf( stdout, "END PLAN\n" ); 

  totalTime = diffTime();
  fprintf( stderr, "elapsed time = %.2fms\n", totalTime );
  fprintf( stdout, "elapsed time = %.2fms\n", totalTime );

  exit( returnCode );
}

int oldmain(int argc, char **argv ) 
{
int rv;
  char *progname;
  long seed1;
  float totalTime;
  unsigned short seed2[3];
  extern char *problemName;

  /* set some defaults */
  progname = argv[0];
  offspringMaxSize = 256;
  nBestSize = 1000;
  nBestUpdates = 0;
  threshold = -1;  /* it will changed to 2*h(s_0) inside nBest */
  loopDetection = 1;
  maxMinutes = 30;
  seed1 = 100;
  seed2[0] = seed2[1] = seed2[2] = 100;

  /* parse options */
  while( argc > 1 && *(*++argv) == '-' )
    {
      switch( (*argv)[1] )
	{
	case 'h':
	  printHelp( progname );
	  fatal( noError );
	  break;
	case 'i':
	  threshold = atoi( *++argv );
	  --argc;
	  break;
	case 'l':
	  loopDetection = 1;
	  break;
	case 'm':
	  maxMinutes = atoi( *++argv );
	  --argc;
	  break;
	case 'u':
	  nBestUpdates = 1;
	  break;
	case 'N':
	  nBestSize = atoi( *++argv );
	  --argc;
	  break;
	case 'r':
	  seed2[0] = seed2[1] = seed2[2] = (unsigned short) atoi( *++argv );
	  seed1 = (long) seed2[0];
	  srand48( seed1 );
	  seed48( seed2 );
	  --argc;
	  break;
	default:
	  fatal1( badUsage, progname );
	  break;
	}
      --argc;
    }
  if( argc != 1 )
    fatal1( badUsage, progname );

  /* let's go for the goal */
  gettimeofday( &startTime, NULL );
  initialize();

  /* print parameters */
  fprintf( stderr, "running %s with the following args:\n", progname );
  fprintf( stderr, "\t-b = %d\n", offspringMaxSize );
  fprintf( stderr, "\t-l = %s\n", (loopDetection ? "yes" : "no") );
  fprintf( stderr, "\t-%c = %d\n", (nBestUpdates ? 'n' : 'N'), nBestSize );
  if( threshold != -1 )
    fprintf( stderr, "\t-i = %d\n", threshold );
  else
    fprintf( stderr, "\t-i = 2 * h( s_0 )\n" );
  fprintf( stderr, "\t-r = %d\n\n", seed2[0] );

  fprintf( stderr, "BEGIN PLAN\n" ); 
  fprintf( stderr, "%s\n", problemName );

  fprintf( stdout, "BEGIN PLAN\n" ); 
  fprintf( stdout, "%s\n", problemName );
  rv = nBest();

  if( rv )
    {
      fprintf( stderr, "solution found\n" );
      fprintf( stderr,  "(" );
      printPath( stderr, nBestPath );
      fprintf( stderr, ")\n" );

      fprintf( stdout, "(" );
      printPath( stdout, nBestPath );
      fprintf( stdout, ")\n" );
    }
  else
    {
      fprintf( stderr, "NO SOLUTION\n" );
      fprintf( stdout, "NO SOLUTION\n" );
    }
  fprintf( stderr, "END PLAN\n" ); 
  fprintf( stdout, "END PLAN\n" ); 

  hashStat();

  totalTime = diffTime();
  fprintf( stderr, "elapsed time = %.2fms\n", totalTime );
  fprintf( stdout, "elapsed time = %.2fms\n", totalTime );

  return( noError );

}


int main( int argc, char **argv )
{
  return oldmain(argc, argv);
}
