/*
**
** Universidad Simon Bolivar, 1999, 2000 (c)
** Blai Bonet and Hector Geffner. 1999, 2000.
**
** Contributors: Hector L. Palacios.
**
** Credits: code for h2max derived from Patrik Haslum's code.
**
** 2.0 revision log:
**
**      2/26/00 state bits packing, hsp and hspr fusion.
**      2/27/00 direct parsing of pddl files (i.e. no more C compilation, etc)
**      2/29/00 new flags "-m" for spec. restricted init. set of mutexs (default: all-pairs)
**      2/29/00 improved space management for mutexes.
**      2/29/00 Alpha version of the scheduler. New flag "-S <sched>". (its parsing not yet).
**      3/01/00 Static atoms removal. New flag "-s" for doing it.
**      3/03/00 Patch on mutex-compilation regarding staticAtoms.
**      3/04/00 Dynamic bucketTableSize.
**      3/04/00 Update printPath procedure for easier automatic extraction of plans.
**      3/04/00 Recode from scratch  heuristic computation. Now is more clean and better performance (?).
**      3/04/00 H^2_max heuristic added. H^2-plus is still pending.
**      3/05/00 Speedup of H^2-max. Improved space management for pair costs.
**      3/08/00 Use of h2max instead of mutexes, set static atoms removal as default.
**      3/08/00 Flags "-m" and "-s" removed. 
**      3/10/00 Modify insertNodeIntoBucket to allow tie breaking strategies.
**      3/10/00 Clean of parameters usage and include schedule parsing:
**                  A schedule is of the form: <option1>:<option2>:<option3>:...
**                  where each <option> is of the form [<direction>,<heuristic>,<time>] in which
**                  <direction> is either "forward" or "backward", <heuristic> is one of "h1plus", 
**                  "h1max", "h2plus", or "h2max", and <time> is the number of msecs to run the option.
**      3/10/00 AIPS00 competition output format.
**      3/13/00 Change pair's cost representation after some profiling.
**      3/17/00 Fix error when instantiating operators: always allocate non-null prec, add, del lists.
**      3/17/00 Fix some errors messages.
**      3/22/00 Fix error in computation of h2max.
**      3/25/00 PriorityQueue implementation for h2plus. 
**      3/25/00 Playing around with first versions of h2plus.
**      3/30/00 Cleanup parser
**      3/30/00 Added typing support to parser
**      4/??/00 Added simple-ADL suuport.
**      4/21/00 Change strcmp to strcasecmp so we don't need to do the patch over lexer
**      4/21/00 Start final distribution of HSP2
**
**
** Portability:
**
**      - I'm using gcc's lvalue trigraphs.
**
**
** Comments:
**
**      - lines marked with xxxx should be changed for more efficiency.
**
**/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <signal.h>
#include <time.h>
#include <sys/types.h>
#include <sys/resource.h>
#include <sys/time.h>
#include <limits.h>
#include <ulimit.h>
#include <unistd.h>
#include <fcntl.h>
#include <assert.h>

#include "parser.h"
#include "planner.h"



/*********************************************************
**********************************************************
**
** Global Variables
**
**/

/* for parsing */
char *              problemFile;
char *              domainFile;
int                 precPlusCost, precMaxCost;
char *              _low_yyfile;
int                 _low_requirements = 0;
char *              _low_problemName = NULL;
char *              _low_domainName = NULL;
char **             _low_schemaName;
char **             _low_objectName;
char **             _low_predicateName;
int                 _low_numberPredicates;
schema_t *          _low_schemaTable[MAXSCHEMA];
int                 _low_initialAtoms[ATOMSPERPACK*MAXATOMPACKS];
int                 _low_goalAtoms[ATOMSPERPACK*MAXATOMPACKS];
int                 _low_copyGoalAtoms[ATOMSPERPACK*MAXATOMPACKS];

/* fundamental global variables */
int                 numberAtomPacks;
int                 numberAtoms =  0;
int                 numberSchema = 0;
int                 numberOperators = 0;
int                 numberHOperators = 0;
int                 numberRelevantAtoms = 0;
int                 globalInitialized = 0;

/* for variables and domain instantiation */
int **              values;
int **              vars;

/* for operator info extraction */
int                 operatorPrec[1024];
int                 operatorPrecSize;
int                 operatorAdd[1024];
int                 operatorAddSize;
int                 operatorDel[1024];
int                 operatorDelSize;

/* for relevant atoms identification */
int *               parents[ATOMSPERPACK*MAXATOMPACKS];
int                 parentsSize[ATOMSPERPACK*MAXATOMPACKS];
atom_t              relevantAtom[MAXATOMPACKS];

/* for adl suboperators */
suboperator_t *     _low_suboperators;
int                 _low_groundingOperators;
int                 _low_negatedAtom[ATOMSPERPACK*MAXATOMPACKS];



/*********************************************************
**********************************************************
**
** Static Variables
**
**/

/* atom hash table */
static iatom_t *    atomHashTable[ATOMHASHSIZE];
static iatom_t *    atomHashPool = NULL;
static int          atomHashClaimSize = 0;

/* node hash table */
static node_t *     nodeHashTable[NODEHASHSIZE];
static int          nodeHashDiameter[NODEHASHSIZE];
static unsigned *   nodeHashValues;
static int          nodeHashNumElem = 0;

/* for mutexes */
static int *        _mutex;
static mutex_t *    mutexList = NULL;
static mutexSet_t * mutexSet;
static mutexSet_t * mutexSetList = NULL;
static int          numberMutexes = 0;

/* static atoms */
static int          numberStaticAtoms;
static atom_t       staticAtom[MAXATOMPACKS];
static int          staticAtomsList[ATOMSPERPACK*MAXATOMPACKS+1];
static int          staticAtomsPermutationDisplay[ATOMSPERPACK*MAXATOMPACKS];

/* operator info */
static int          operatorTableSize = 0;
static operator_t * operatorTable = NULL;
static int          HOperatorTableSize = 0;
static operator_t * HOperatorTable = NULL;
static int *        validOperators;
static int **       invPrecTable;
static int *        invPrecTableSize;
static int **       invAddTable;
static int *        invAddTableSize;
static int **       invDelTable;
static int *        invDelTableSize;
static int **       notAdmissible;
static int *        notAdmissibleSize;
static int *        operatorsWithNoPrec = NULL;
static int          operatorsWithNoPrecSize = 0;
static int *        HOperatorsWithNoPrec = NULL;
static int          HOperatorsWithNoPrecSize = 0;
static int **       HInvPrecTable;
static int *        HInvPrecTableSize;
static int **       HInvAddTable;
static int *        HInvAddTableSize;
static int **       HSubTable;
static int *        HSubTableSize;

/* for node pool management */
static int          pageSize;
static node_t *     nodePool = NULL;
static int          currentNodePool = -1;
static int          nodePoolClaimSize = 0;
static int *        nodePoolSize;
static char *       nodePoolUsed;
static int          nodePoolTableSize = 0;
static node_t **    nodePoolTable;

/* for Best-First Search */
static node_t *     headOPEN = NULL;
static node_t *     tailOPEN = NULL;
static node_t *     CLOSE = NULL;
static node_t **    firstNodeInBucket;
static node_t **    lastNodeInBucket;
static int          bucketTableSize;
static int          minBucket;
static int          maxBucket;

/* true initial state */
static atom_t       staticInitialState[MAXATOMPACKS];
static atom_t       staticGoalState[MAXATOMPACKS];

/* problem data */
static int          expandedNodes;
static int          generatedNodes;

/* for reachability analysis */
static atom_t       reachableAtom[MAXATOMPACKS];
static atom_t       oldReachable[MAXATOMPACKS];

/* states workareas */
static atom_t       staticState[MAXATOMPACKS];

/* heuristics */
static int          H2Computed = 0;
static cost_t       H1Cost[ATOMSPERPACK*MAXATOMPACKS];
static cost_t       backwardH1Cost[ATOMSPERPACK*MAXATOMPACKS];
static cost_t **    H2Cost;                            /* O(n^2) so we allocate it at runtime */

/* some global parameters */
static int          verbose;
static float        heuristicWeight;
static int          searchAlgorithm;
static int          searchHeuristic;
static int          searchDirection;
static int          mutexesAllPairs;
static int          staticCompilation; 
static char *       searchAlgorithmName[] = { "bfs", "gbfs" };
static char *       searchDirectionName[] = { "forward", "backward" };
static char *       searchHeuristicName[] = { "h1plus", "h1max", "h2plus", "h2max" };

/* schedules */
static long         nodeMemoryUsed;
static long         memoryConstraint;
static int          timeExpired;
static int          memoryExpired;
static schedule_t * globalSchedule = NULL;
static char *       scheduleString = NULL;

/* procedure registration stack */
static procRegister_t *procStackTop = NULL;



/*********************************************************
**********************************************************
**
** Function Prototypes
**
**/

void                newMutex( register int, register int, register mutex_t ** );
void                delMutex( register mutex_t * );
void                newMutexSet( register int );

void                identifyGoalParents( register int * );
void                printNode( register FILE *, register char *, register node_t * );
void                removeNodeFromOPEN( register node_t * );
node_t *            removeLastFromOPEN( register node_t * );
node_t *            removeNodeFromCLOSE( void  );

node_t *            BFS( schedule_t * );
node_t *            GBFS( schedule_t * );

void                generateHOperators( void );
char *              operatorName( register int *, register int );
char *              readAtomName( register int );
iatom_t *           readAtomByNumber( register int );
void                orderAtomList( register int *, register int );
void                printState( register FILE *, register atom_t * );
void                registerEntry( register char * );
int                 registerExit( void );
void                flushAllRegisters( void );

void                notYetImplemented( schedule_t * );
void                _fatal( register int, register char *, register char *, register int );
void                printStatistics( void );
void                setTimer( unsigned long );



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
	  if( !(_low_requirements & REQ_EQUALITY) )
	    {
	      for( jp = &parameters[1]; jp < &parameters[MAXPARAMETERS]; ++jp )
		if( (*jp > 0) && (*jp == *vp) )
		  break;
	    }
	  else
	    jp = &parameters[MAXPARAMETERS];

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
** Reachability Analysis
**
**/

int
testOperatorPrecondition( register int *parameters, register int schema )
{
  register int rv;
  extern int evaluateFormula( void *, atom_t *, int *, int );

  rv = evaluateFormula( _low_schemaTable[schema]->prec, reachableAtom, parameters, 0 );
  return( rv );
}


void
insertOperator( register int *parameters, register int schema )
{
  extern int  applyOperatorSchema( schema_t *, atom_t *, atom_t *, int * );
  extern void instantiateConditionalEffects( int, schema_t *, int * );

  if( _low_groundingOperators == 0 )
    {
      applyOperatorSchema( _low_schemaTable[schema], reachableAtom, reachableAtom, parameters );
    }
  else
    {
      if( applyOperatorSchema( _low_schemaTable[schema], reachableAtom, staticState, parameters ) )
	{
	  /* resize operatorTable */
	  if( numberOperators == operatorTableSize )
	    {
	      operatorTableSize = (operatorTableSize == 0 ? 16 : INCRATE * operatorTableSize);
	      operatorTable = 
		(operator_t*)realloc( operatorTable, operatorTableSize * sizeof( operator_t ) );
	      if( !operatorTable )
		fatal( noMoreMemory );
	    }

	  /* space allocation for precondition, add, del lists */
	  operatorTable[numberOperators].precSize = operatorPrecSize;
	  operatorTable[numberOperators].addSize = operatorAddSize;
	  operatorTable[numberOperators].delSize = operatorDelSize;
	  operatorTable[numberOperators].prec = (int*)calloc( operatorPrecSize + 1, sizeof( int ) );
	  operatorTable[numberOperators].add = (int*)calloc( operatorAddSize + 1, sizeof( int ) );
	  operatorTable[numberOperators].del = (int*)calloc( operatorDelSize + 1, sizeof( int ) );
	  if( !operatorTable[numberOperators].prec || !operatorTable[numberOperators].add || 
	      !operatorTable[numberOperators].del )
	    fatal( noMoreMemory );

	  /* order Prec, Add, and Del lists */
	  orderAtomList( operatorPrec, operatorPrecSize );
	  orderAtomList( operatorAdd, operatorAddSize );
	  orderAtomList( operatorDel, operatorDelSize );

	  /* fill it */
	  memcpy( operatorTable[numberOperators].prec, operatorPrec, (operatorPrecSize+1) * sizeof( int ) );
	  memcpy( operatorTable[numberOperators].add, operatorAdd, (operatorAddSize+1) * sizeof( int ) );
	  memcpy( operatorTable[numberOperators].del, operatorDel, (operatorDelSize+1) * sizeof( int ) );
	  operatorTable[numberOperators].name = strdup( operatorName( parameters, schema ) );
	  operatorTable[numberOperators].valid = 1;

	  /* fill operatorsWithNoPrec table */
	  if( operatorPrecSize == 0 )
	    {
	      ++operatorsWithNoPrecSize;
	      operatorsWithNoPrec =
		(int*)realloc( operatorsWithNoPrec, operatorsWithNoPrecSize * sizeof( int ) );
	      operatorsWithNoPrec[operatorsWithNoPrecSize-1] = numberOperators;
	    }

	  /* generate suboperators */
	  _low_suboperators = NULL;
	  instantiateConditionalEffects( numberOperators, _low_schemaTable[schema], parameters );
	  operatorTable[numberOperators].suboperators = _low_suboperators;

	  /* next operator */
	  ++numberOperators;
	}
    }
}


void
reachabilityAnalysis( register atom_t *state )
{
  register int i, number;

  /* basic initialization */
  _low_groundingOperators = 0;
  memset( _low_negatedAtom, 0, MAXATOMPACKS * ATOMSPERPACK * sizeof( int ) );

  /* initialize costs with given state */
  memcpy( reachableAtom, state, MAXATOMPACKS * sizeof( atom_t ) );
  memcpy( oldReachable, state, MAXATOMPACKS * sizeof( atom_t ) );

  /* computation of reachable atoms */
  number = 1;
  while( number != 0 )
    {
      instantiateOperators( &insertOperator, &testOperatorPrecondition );
      number = memcmp( oldReachable, reachableAtom, MAXATOMPACKS * sizeof( atom_t ) );
      memcpy( oldReachable, reachableAtom, MAXATOMPACKS * sizeof( atom_t ) );
    }

  /* print reachable atoms */
  if( verbose > 7 )
    {
      for( i = 1; i < SIZE_ATOMS; ++i )
	if( asserted( reachableAtom, i ) )
	  fprintf( stderr, "atom %s is reachable from initial state\n", readAtomByNumber( i )->name );
    }
}



/*********************************************************
**********************************************************
**
** Heuristics
**
**/

void
H1SetCost( register cost_t *cost, register int* set )
{
  register int *p;

  cost->max = 0;
  cost->plus = 0;
  for( p = set; *p != 0; ++p )
    {
      cost->max = MAX( H1Cost[*p].max, cost->max );
      cost->plus = PLUSSUM( H1Cost[*p].plus, cost->plus );
    }
}


void
H1Setup( register atom_t *state )
{
  register int p, *op, change;
  register unsigned long minCostMax, minCostPlus;
  cost_t tmpCost;

  /* initial costs */
  for( p = 1; p < SIZE_ATOMS; ++p )
    if( asserted( state, p ) )
      {
	H1Cost[p].max = 0;
	H1Cost[p].plus = 0;
      }
    else
      {
	H1Cost[p].max = INT_MAX;
	H1Cost[p].plus = INT_MAX;
      }

  /* use dynamic programming for computing the remaining costs */
  change = 1;
  while( change )
    {
      /* clean state */
      change = 0;

      /* single full backup for { p } */
      for( p = 1; p < SIZE_ATOMS; ++p )
	{
	  minCostMax = INT_MAX;
	  minCostPlus = INT_MAX;
	  if( !(_low_requirements & REQ_ADL) )
	    {
	      for( op = invAddTable[p]; (op != NULL) && (*op != 0); ++op )
		{
		  H1SetCost( &tmpCost, operatorTable[(*op)-1].prec );
		  minCostMax = MIN( minCostMax, tmpCost.max );
		  minCostPlus = MIN( minCostPlus, tmpCost.plus );
		}
	    }
	  else
	    {
	      for( op = HInvAddTable[p]; (op != NULL) && (*op != 0); ++op )
		{
		  H1SetCost( &tmpCost, HOperatorTable[(*op)-1].prec );
		  minCostMax = MIN( minCostMax, tmpCost.max );
		  minCostPlus = MIN( minCostPlus, tmpCost.plus );
		}
	    }
	  minCostMax = PLUSSUM( minCostMax, 1 );
	  minCostPlus = PLUSSUM( minCostPlus, 1 );

	  /* update max cost */
	  if( H1Cost[p].max > minCostMax )
	    {
	      change = 1;
	      H1Cost[p].max = minCostMax;
	    }

	  /* update plus cost */
	  if( H1Cost[p].plus > minCostPlus )
	    {
	      change = 1;
	      H1Cost[p].plus = minCostPlus;
	    }
	  assert( H1Cost[p].plus != INT_MAX || H1Cost[p].max == INT_MAX );
	}
    }

  /* print costs */
  if( verbose > 7 )
    {
      for( p = 1; p < SIZE_ATOMS; ++p )
	if( H1Cost[p].plus < INT_MAX )
	  fprintf( stderr, "cost of %s is (%lu,%lu)\n", readAtomByNumber( p )->name, 
		   H1Cost[p].plus, H1Cost[p].max );
    }
}


void
H2SetCost( register cost_t *cost, register int *set, register int *extra )
{
  register int *p, *q;

  /* go over all pairs: compute max cost & build PQ with plus costs */
  cost->max = 0;
  cost->plus = 0;
  for( p = set; *p != 0; ++p )
    for( q = p; *q != 0; ++q )
      cost->max = MAX( PAIR( *p, *q ).max, cost->max );

  if( extra != NULL )
    for( p = extra; *p != 0; ++p )
      for( q = p; *q != 0; ++q )
	cost->max = MAX( PAIR( *p, *q ).max, cost->max );
}


void
H2Setup( register atom_t *state )
{
  register int p, q, *a, *t, op, *op2, change, localChange;
  register unsigned long maxCost1, maxCost2;
  cost_t precCost;

  static int initialized = 0;
  static char *display;

  /* registry */
  registerEntry( "H2Setup()" );

  /* initialization */
  if( !initialized )
    {
      initialized = 1;
      H2Cost = (cost_t**)calloc( SIZE_ATOMS, sizeof( cost_t* ) );
      display = (char*)malloc( numberOperators * sizeof( char ) );
      if( !H2Cost || !display )
	fatal( noMoreMemory );

      for( p = 1, q = SIZE_ATOMS; p < SIZE_ATOMS; ++p, --q )
	if( (H2Cost[p] = (cost_t*)calloc( q, sizeof( cost_t ) )) == NULL )
	  fatal( noMoreMemory );
    }

  /* initial pair's costs */
  memset( display, 0, numberOperators * sizeof( char ) );
  for( p = 1; p < SIZE_ATOMS; ++p )
    for( q = p; q < SIZE_ATOMS; ++q )
      if( asserted( state, p ) && asserted( state, q) )
	{
	  PAIR( p, q ).max = 0;
	  PAIR( p, q ).plus = 0;
	}
      else
	{
	  PAIR( p, q ).max = INT_MAX;
	  PAIR( p, q ).plus = INT_MAX;
	}

  /* initially, add all operators */
  for( op = 0; op < numberOperators; ++op )
    display[op] = 1;

  /* use dynamic programming for computing the pair's costs */
  change = 1;
  while( change )
    {
      /* clean state */
      change = 0;

      /* outer loop over all operators. Inner loops over their Add/Del lists */
      for( op = 0; op < numberOperators; ++op )
	if( (operatorTable[op].valid == 1) && (display[op] == 1) )
	  {
	    /* clean display */
	    display[op] = 0;

	    /* compute the H2 value of the preconditions, and check if we are going to
	       do something useful.
	    */
	    H2SetCost( &precCost, operatorTable[op].prec, NULL );
	    maxCost1 = PLUSSUM( precCost.max, 1 );
	    if( maxCost1 == INT_MAX )
	      continue;

	    /* update for pairs { *a, *t } such that *t is in Add (this includes singleton { *a }) */
	    for( a = operatorTable[op].add; *a != 0; ++a )
	      {
		localChange = 0;
		for( t = a; *t != 0; ++t )
		  if( PAIR( *a, *t ).max > maxCost1 )
		    {
		      change = 1;
		      localChange = 1;
		      PAIR( *a, *t ).max = maxCost1;

		      for( op2 = invPrecTable[*t]; (op2 != NULL) && (*op2 != 0); ++op2 )
			display[(*op2)-1] = 1;

		      /* if singleton updated, flag all operators */
		      if( *a == *t )
			{
			  localChange = 0;
			  for( p = 0; p < numberOperators; ++p )
			    display[p] = 1;
			}
		    }

		/* set display */
		if( localChange == 1 )
		  {
		    localChange = 0;
		    for( op2 = invPrecTable[*a]; (op2 != NULL) && (*op2 != 0); ++op2 )
		      display[(*op2)-1] = 1;

		    /* also, add all operators with null preconditions */
		    for( p = 0; p < operatorsWithNoPrecSize; ++p )
		      display[operatorsWithNoPrec[p]] = 1;
		  }
	      }

	    /* update for pairs { q, *a } such that q is not in Del/Add */
	    for( q = 1; q < SIZE_ATOMS; ++q )
	      {
		/* precompute updated cost and check if we really need to do the update */
		localChange = 0;
		maxCost2 = MAX( precCost.max, PAIR( q, q ).max );
		if( maxCost2 == INT_MAX )
		  continue;

		/* ok, check that q is not affected by the operator */
		for( t = operatorTable[op].del; (t != NULL) && (*t != 0) && (*t != q); ++t);
		if( (t == NULL) || (*t == 0) )
		  for( t = operatorTable[op].add; (t != NULL) && (*t != 0) && (*t != q); ++t);
		if( (t != NULL) && (*t == 0) )
		  {
		    /* finish computing of cost */
		    for( t = operatorTable[op].prec; *t != 0; ++t )
		      maxCost2 = MAX( PAIR( q, *t ).max, maxCost2 );
		    maxCost2 = PLUSSUM( maxCost2, 1 );
		    if( maxCost2 < INT_MAX )
		      {
			/* update cost for { *a, q } */
			for( a = operatorTable[op].add; *a != 0; ++a )
			  if( PAIR( *a, q ).max > maxCost2 )
			    {
			      change = 1;
			      localChange = 1;
			      PAIR( *a, q ).max = maxCost2;

			      assert( *a != q );
			      for( op2 = invPrecTable[*a]; (op2 != NULL) && (*op2 != 0); ++op2 )
				display[(*op2)-1] = 1;
			    }
		      }
		  }

		/* set display */
		if( localChange == 1 )
		  {
		    localChange = 0;
		    for( op2 = invPrecTable[q]; (op2 != NULL) && (*op2 != 0); ++op2 )
		      display[(*op2)-1] = 1;

		    /* also, add all operators with null preconditions */
		    for( p = 0; p < operatorsWithNoPrecSize; ++p )
		      display[operatorsWithNoPrec[p]] = 1;
		  }
	      }
	  }
    }

  /* print pair's costs */
  if( verbose > 7 )
    {
      for( p = 1; p < SIZE_ATOMS; ++p )
	for( q = p; q < SIZE_ATOMS; ++q )
	  if( PAIR( p, q ).max < INT_MAX )
	    {
	      assert( PAIR( p, q ).max == PAIR( p, q ).max );
	      fprintf( stderr, "cost of { %s, %s } is (%lu,%lu)\n", readAtomByNumber( p )->name, 
		       readAtomByNumber( q )->name, PAIR( p, q ).plus, PAIR( p, q ).max );
	    }
	  else
	    {
	      fprintf( stderr, "cost of { %s, %s } is infinite\n", readAtomByNumber( p )->name, 
		       readAtomByNumber( q )->name );
	    }
    }

  /* set flag */
  H2Computed = 1;
  registerExit();
}


void
ADLH2Setup( register atom_t *state )
{
  register int p, q, *a, *t, op, *op2, change, localChange;
  register unsigned long maxCost1, maxCost2;
  cost_t precCost;

  static int initialized = 0;
  static char *display1, *display2;

  /* initialization */
  if( !initialized )
    {
      initialized = 1;
      H2Cost = (cost_t**)calloc( SIZE_ATOMS, sizeof( cost_t* ) );
      display1 = (char*)malloc( numberHOperators * sizeof( char ) );
      display2 = (char*)malloc( numberOperators * sizeof( char ) );
      if( !H2Cost || !display1 || !display2 )
	fatal( noMoreMemory );

      for( p = 1, q = SIZE_ATOMS; p < SIZE_ATOMS; ++p, --q )
	if( (H2Cost[p] = (cost_t*)malloc( q * sizeof( cost_t ) )) == NULL )
	  fatal( noMoreMemory );
    }

  /* initial pair's costs */
  memset( display1, 0, numberHOperators * sizeof( char ) );
  memset( display2, 0, numberHOperators * sizeof( char ) );
  for( p = 1; p < SIZE_ATOMS; ++p )
    for( q = p; q < SIZE_ATOMS; ++q )
      if( asserted( state, p ) && asserted( state, q ) )
	PAIR( p, q ).max = 0;
      else
	PAIR( p, q ).max = INT_MAX;

  /* initially, add all operators */
  for( op = 0; op < numberHOperators; ++op )
    display1[op] = 1;
  for( op = 0; op < numberOperators; ++op )
    display2[op] = 1;

  /* use dynamic programming for computing the pair's costs */
  change = 1;
  while( change )
    {
      /* clean state */
      change = 0;

      /* outer loop over all operators. Inner loops over their Add/Del lists */
      for( op = 0; op < numberHOperators; ++op )
	if( (HOperatorTable[op].valid == 1) && (display1[op] == 1) )
	  {
	    /* clean display */
	    display1[op] = 0;

	    /* compute the H2 value of the preconditions, and check if we are going to
	       do something useful.
	    */
	    H2SetCost( &precCost, HOperatorTable[op].prec, NULL );
	    maxCost1 = PLUSSUM( precCost.max, 1 );
	    if( maxCost1 == INT_MAX )
	      continue;

	    /* update for pairs { *a, *t } such that *t is in Add (this includes singleton { *a }) */
	    for( a = HOperatorTable[op].add; *a != 0; ++a )
	      {
		localChange = 0;
		for( t = a; *t != 0; ++t )
		  if( PAIR( *a, *t ).max > maxCost1 )
		    {
		      change = 1;
		      localChange = 1;
		      PAIR( *a, *t ).max = maxCost1;

		      for( op2 = HInvPrecTable[*t]; (op2 != NULL) && (*op2 != 0); ++op2 )
			{
			  display1[(*op2)-1] = 1;
			  display2[HOperatorTable[(*op2)-1].father] = 1;
			}

		      /* if singleton updated, flag all operators */
		      if( *a == *t )
			{
			  localChange = 0;
			  for( p = 0; p < numberHOperators; ++p )
			    display1[p] = 1;
			  for( p = 0; p < numberOperators; ++p )
			    display2[p] = 1;
			}
		    }

		/* set display */
		if( localChange == 1 )
		  {
		    localChange = 0;
		    for( op2 = HInvPrecTable[*a]; (op2 != NULL) && (*op2 != 0); ++op2 )
		      {
			display1[(*op2)-1] = 1;
			display2[HOperatorTable[(*op2)-1].father] = 1;
		      }

		    /* also, add all operators with null preconditions */
		    for( p = 0; p < HOperatorsWithNoPrecSize; ++p )
		      display1[HOperatorsWithNoPrec[p]] = 1;
		    for( p = 0; p < operatorsWithNoPrecSize; ++p )
		      display2[operatorsWithNoPrec[p]] = 1;
		  }
	      }

	    /* update for pairs { q, *a } such that q is not in Del/Add */
	    for( q = 1; q < SIZE_ATOMS; ++q )
	      {
		/* precompute updated cost and check if we really need to do the update */
		localChange = 0;
		maxCost2 = MAX( precCost.max, PAIR( q, q ).max );
		if( maxCost2 == INT_MAX )
		  continue;

		/* ok, check that q is not affected by the operator */
		for( t = HOperatorTable[op].del; (t != NULL) && (*t != 0) && (*t != q); ++t);
		if( (t == NULL) || (*t == 0) )
		  for( t = HOperatorTable[op].add; (t != NULL) && (*t != 0) && (*t != q); ++t);
		if( (t != NULL) && (*t == 0) )
		  {
		    /* finish computing of cost */
		    for( t = HOperatorTable[op].prec; *t != 0; ++t )
		      maxCost2 = MAX( PAIR( q, *t ).max, maxCost2 );
		    maxCost2 = PLUSSUM( maxCost2, 1 );
		    if( maxCost2 < INT_MAX )
		      {
			/* update cost for { *a, q } */
			for( a = HOperatorTable[op].add; *a != 0; ++a )
			  if( PAIR( *a, q ).max > maxCost2 )
			    {
			      change = 1;
			      localChange = 1;
			      PAIR( *a, q ).max = maxCost2;
			      assert( *a != q );
			      for( op2 = HInvPrecTable[*a]; (op2 != NULL) && (*op2 != 0); ++op2 )
				{
				  display1[(*op2)-1] = 1;
				  display2[HOperatorTable[(*op2)-1].father] = 1;
				}
			    }
		      }
		  }

		/* set display */
		if( localChange == 1 )
		  {
		    localChange = 0;
		    for( op2 = HInvPrecTable[q]; (op2 != NULL) && (*op2 != 0); ++op2 )
		      {
			display1[(*op2)-1] = 1;
			display2[HOperatorTable[(*op2)-1].father] = 1;
		      }

		    /* also, add all operators with null preconditions */
		    for( p = 0; p < HOperatorsWithNoPrecSize; ++p )
		      display1[HOperatorsWithNoPrec[p]] = 1;
		    for( p = 0; p < operatorsWithNoPrecSize; ++p )
		      display2[operatorsWithNoPrec[p]] = 1;
		  }
	      }
	  }

      /* now, loop over all operators for the 'parallel' action term */
      for( op = 0; op < numberOperators; ++op )
	if( (operatorTable[op].valid == 1) && (display2[op] == 1) )
	  {
	    register int *s1, *s2, *e1, *e2;

	    display2[op] = 0;
	    for( s1 = HSubTable[op]; (s1 != NULL) && (*s1 != 0); ++s1 )
	      for( s2 = s1 + 1; *s2 != 0; ++s2 )
		{
		  /* check if consistent and relevant */
		  for( e1 = HOperatorTable[(*s1)-1].add; (e1 != NULL) && (*e1 != 0); ++e1 )
		    {
		      for( e2 = HOperatorTable[(*s2)-1].del; (e2!=NULL) && (*e2!=0) && (*e2!=*e1); ++e2 );
		      if( (e1 != NULL) && (*e2 != 0) )
			break;
		    }
		  if( (e1 != NULL) && (*e1 != 0) )
		    continue;

		  for( e1 = HOperatorTable[(*s2)-1].add; (e1 != NULL) && (*e1 != 0); ++e1 )
		    {
		      for( e2 = HOperatorTable[(*s1)-1].del; (e2!=NULL) && (*e2!=0) && (*e2!=*e1); ++e2 );
		      if( (e1 != NULL) && (*e2 != 0) )
			break;
		    }
		  if( (e1 != NULL) && (*e1 != 0) )
		    continue;

		  /* precompute cost */
		  H2SetCost( &precCost, HOperatorTable[(*s1)-1].prec, HOperatorTable[(*s2)-1].prec );
		  maxCost1 = PLUSSUM( precCost.max, 1 );
		  if( maxCost1 == INT_MAX )
		    continue;

		  /* update costs */
		  for( a = HOperatorTable[(*s1)-1].add; *a != 0; ++a )
		    {
		      localChange = 0;
		      for( t = HOperatorTable[(*s2)-1].add; *t != 0; ++t )
			if( PAIR( *a, *t ).max > maxCost1 )
			  {
			    change = 1;
			    localChange = 1;
			    PAIR( *a, *t ).max = maxCost1;
			    for( op2 = HInvPrecTable[*t]; (op2 != NULL) && (*op2 != 0); ++op2 )
			      {
				display1[(*op2)-1] = 1;
				display2[HOperatorTable[(*op2)-1].father] = 1;
			      }

			    /* if singleton updated, flag all operators */
			    if( *a == *t )
			      {
				localChange = 0;
				for( p = 0; p < numberHOperators; ++p )
				  display1[p] = 1;
				for( p = 0; p < numberOperators; ++p )
				  display2[p] = 1;
			      }
			  }

		      /* set display */
		      if( localChange == 1 )
			{
			  localChange = 0;
			  for( op2 = HInvPrecTable[*a]; (op2 != NULL) && (*op2 != 0); ++op2 )
			    {
			      display1[(*op2)-1] = 1;
			      display2[HOperatorTable[(*op2)-1].father] = 1;
			    }

			  /* also, add all operators with null preconditions */
			  for( p = 0; p < HOperatorsWithNoPrecSize; ++p )
			    display1[HOperatorsWithNoPrec[p]] = 1;
			  for( p = 0; p < operatorsWithNoPrecSize; ++p )
			    display2[operatorsWithNoPrec[p]] = 1;
			}
		    }
		}
	  }
    }

  /* print pair's costs */
  if( verbose > 7 )
    {
      for( p = 1; p < SIZE_ATOMS; ++p )
	for( q = p; q < SIZE_ATOMS; ++q )
	  if( PAIR( p, q ).max < INT_MAX )
	    {
	      assert( PAIR( p, q ).max == PAIR( p, q ).max );
	      fprintf( stderr, "cost of { %s, %s } is (%lu,%lu)\n", readAtomByNumber( p )->name, 
		       readAtomByNumber( q )->name, PAIR( p, q ).plus, PAIR( p, q ).max );
	    }
	  else
	    {
	      fprintf( stderr, "cost of { %s, %s } is infinite\n", readAtomByNumber( p )->name, 
		       readAtomByNumber( q )->name );
	    }
    }

  /* set flag */
  H2Computed = 1;
}


void
runtimeH2Cost( register cost_t *cost, register int* set )
{
  register int *p, *q;

  /* go over all pairs: compute max cost & build PQ with plus costs */
  cost->max = 0;
  cost->plus = 0;
  for( p = set; *p != 0; ++p )
    for( q = p; *q != 0; ++q )
      cost->max = MAX( PAIR( *p, *q ).max, cost->max );
}


void
H2Heuristics( register cost_t *cost, register atom_t *state )
{
  register int i, *s;
  static int atomSet[ATOMSPERPACK*MAXATOMPACKS+1];

  if( !(_low_requirements & REQ_ADL) )
    {
      /* build set of atoms */
      s = atomSet;
      for( i = 1; i < SIZE_ATOMS; ++i )
	if( asserted( state, i ) )
	  *s++ = i;
      *s = 0;

      /* call H2SetCost over the set */
      runtimeH2Cost( cost, atomSet );
    }
  else
    {
      runtimeH2Cost( cost, _low_goalAtoms );
    }
}


void
heuristics( register node_t *node )
{
  register int i, *g;
  cost_t tmpCost;

  /* initialization */
  node->h1_plus = 0;
  node->h2_plus = 0;
  node->h1_max = 0;
  node->h2_max = 0;
  node->valid = 1;

  if( searchDirection == FORWARD )
    {
      /* need recompute heuristic cost from scratch */
      H1Setup( node->state );

      /* add/max the costs for atoms in goal */
      for( g = _low_goalAtoms; *g != 0; ++g )
	if( H1Cost[*g].plus == INT_MAX )
	  {
	    node->h1_plus = -1;
	    node->h1_max = -1;
	    node->h2_max = -1;
	    node->valid = 0;
	    return;
	  }
	else
	  {
	    node->h1_plus += H1Cost[*g].plus;
	    node->h1_max = MAX( node->h1_max, H1Cost[*g].max );
	  }

      /* H2 heuristics are too expensice for forward search */
      if( (_low_requirements & REQ_ADL) && (searchHeuristic == H2MAX) )
	{
	  ADLH2Setup( node->state );
	  H2Heuristics( &tmpCost, node->state );
	  node->h2_plus = tmpCost.plus;
	  node->h2_max = tmpCost.max;
	  assert( node->h2_max >= node->h1_max );
	  /*
	    assert( !(node->h1_plus == 0) || (node->h2_max == 0) );
	  */
	}
    }
  else
    {
      /* add/max the costs for atoms in node */
      for( i = 1; i < SIZE_ATOMS; ++i )
	if( asserted( node->state, i ) )
	  {
	    if( backwardH1Cost[i].plus == INT_MAX )
	      {
		node->h1_plus = -1;
		node->h1_max = -1;
		node->valid = 0;
		return;
	      }
	    else
	      {
		node->h1_plus += backwardH1Cost[i].plus;
		node->h1_max = MAX( node->h1_max, backwardH1Cost[i].max );
	      }
	  }

      /* H2 heuristics */
      if( H2Computed == 1 )
	{
	  H2Heuristics( &tmpCost, node->state );
	  node->h2_plus = tmpCost.plus;
	  node->h2_max = tmpCost.max;
	  assert( node->h2_max >= node->h1_max );
	  assert( !(node->h1_plus == 0) || (node->h2_max == 0) );
	}
    }

  /* use H2 to update valid bit */
  node->valid = node->valid && (node->h2_max < INT_MAX);
}



/*********************************************************
**********************************************************
**
** Atoms and Operators names
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
  static char name[1024];

  if( parameters[0] <= _low_numberPredicates )
    {
      name[0] = '(';
      name[1] = '\0';
      strcat( name, _low_predicateName[parameters[0] - 1] );
      for( ip = &parameters[1]; *ip != -1; ++ip )
	{
	  strcat( name, " " );
	  strcat( name, _low_objectName[*ip - 1] );
	}
      strcat( name, ")" );
    }
  else
    {
      name[0] = '\0';
      strcat( name, "(NOT-" );
      strcat( name, _low_predicateName[parameters[0] - (_low_numberPredicates + 1) - 1] );
      for( ip = &parameters[1]; *ip != -1; ++ip )
	{
	  strcat( name, " " );
	  strcat( name, _low_objectName[*ip - 1] );
	}
      strcat( name, ")" );
    }
  return( name );
}


int
atomCmp( register const void *a1, register const void *a2 )
{
  return( (*(int*)a1) - (*(int*)a2) );
}


void
orderAtomList( register int *atomList, register int size )
{
  qsort( atomList, size, sizeof( int ), &atomCmp );
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

  if( numberAtoms == ATOMSPERPACK * MAXATOMPACKS - 1 )
    fatal( maxAtoms );

  index = atomHashFunction( parameters );
  if( currentAtom >= atomHashClaimSize )
    {
      atomHashClaimSize = (atomHashClaimSize == 0 ? 1024 : INCRATE * atomHashClaimSize);
      atomHashPool = (iatom_t*)malloc( atomHashClaimSize * sizeof( iatom_t ) );
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
      nodePoolTableSize = (nodePoolTableSize == 0 ? 1024 : INCRATE * nodePoolTableSize);
      newNodePoolTable = (node_t**)realloc( nodePoolTable, nodePoolTableSize * sizeof( node_t* ) );
      newNodePoolSize = (int*)realloc( nodePoolSize, nodePoolTableSize * sizeof( int ) );
      newNodePoolUsed = (char*)realloc( nodePoolUsed, nodePoolTableSize * sizeof( char ) );
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
	  nodePoolClaimSize = (nodePoolClaimSize == 0 ? 1024 : (int) (INCRATE * nodePoolClaimSize));
	  nodePoolSize[currentNodePool] = nodePoolClaimSize * NODESIZE;
	  fprintf( stderr, "HEAPMGMT: allocating memory for %d nodes (%d bytes)... ", 
		   nodePoolClaimSize, nodePoolSize[currentNodePool] );
	  fflush( stderr );
	  nodePoolTable[currentNodePool] = (node_t*)malloc( nodePoolSize[currentNodePool] );
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
  result->state = (atom_t*) ((unsigned)&result->state + sizeof( node_t* ));
  nodePool = (node_t*) ((unsigned)nodePool + NODESIZE);

  /* update memory usage & check constraint */
  nodeMemoryUsed += NODESIZE;
  if( (memoryConstraint > 0) && ((nodeMemoryUsed) / MEGABYTE >= memoryConstraint) )
    memoryExpired = 1;

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
  nodeMemoryUsed = 0;
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

  assert( globalInitialized );
  for( lp1 = state1, lp2 = state2; (lp1 < &state1[SIZE_PACKS]) && (lp1->pack == lp2->pack); ++lp1, ++lp2 );
  return( !(lp1 == &state1[SIZE_PACKS]) );
}


unsigned 
nodeHashFunction( register atom_t *state )
{
  register int i;
  register unsigned val, *hp;

  /* xxxx: the ideal thing to do here is to loop MAXATOMPACKS times instead of SIZE_ATOMS */
  assert( globalInitialized );
  for( i = 1, hp = nodeHashValues, val = 0; i < SIZE_ATOMS; ++i, ++hp )
    val += (asserted( state, i ) ? *hp : 0);
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
  memset( nodeHashTable, 0, NODEHASHSIZE * sizeof( node_t* ) );
}


void
nodeHashStatistics( void )
{
  register int index, diameter, sum, n;

  n = 0;
  sum = 0;
  diameter = 0;
  for( index = 0; index < NODEHASHSIZE; ++index )
    {
      diameter = MAX( diameter, nodeHashDiameter[index] );
      sum += nodeHashDiameter[index];
      n += (nodeHashDiameter[index] > 0);
    }

  fprintf( stderr, "NODEHASH: nodes in hash table = %d\n", nodeHashNumElem );
  fprintf( stderr, "NODEHASH: diameter of hash table = %d\n", diameter );
  fprintf( stderr, "NODEHASH: average diameter of hash table = %f\n", (float)sum/(float)n );
}



/*********************************************************
**********************************************************
**
** Node Expansion: Common
**
**/

int
nodeBucket( register node_t *node )
{
  register int result;

  switch( searchHeuristic )
    {
    case H1PLUS: /* f(n) = g(n) + W*h1_plus(n) */
      result = node->cost + (int)(heuristicWeight * node->h1_plus);
      break;
    case H1MAX: /* f(n) = g(n) + W*h1_max(n) */
      result = node->cost + (int)(heuristicWeight * node->h1_max);
      break;
    case H2PLUS: /* f(n) = g(n) + W*h2_plus(n) */
      result = node->cost + (int)(heuristicWeight * node->h2_plus);
      break;
    case H2MAX: /* f(n) = g(n) + W*h2_max(n) */
      result = node->cost + (int)(heuristicWeight * node->h2_max);
      break;
    }
  assert( (result >= 0) && (result < bucketTableSize) ); 
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
	  minBucket = bucketTableSize;
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
	  minBucket = bucketTableSize;
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
	  minBucket = bucketTableSize;
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
	  minBucket = bucketTableSize;
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


void
insertNodeIntoBucket( register node_t *node, register int bucket )
{
  register node_t * n;

  /* look for proper place of insertion */
  for( n = firstNodeInBucket[bucket]; n != lastNodeInBucket[bucket]; n = n->bucketNext )
    if( ((searchHeuristic == H1PLUS) && (node->h2_max <= n->h2_max)) ||
	((searchHeuristic == H1MAX) && (node->h1_plus <= n->h1_plus)) ||
	((searchHeuristic == H2MAX) && (node->h1_plus <= n->h1_plus)) )
      break;

  /* insert node before n */
  node->bucket = bucket;
  node->bucketNext = n;
  node->bucketPrev = NULL;
  if( n == firstNodeInBucket[bucket] )
    {
      if( n == NULL )
	lastNodeInBucket[bucket] = node;
      else
	n->bucketPrev = node;
      firstNodeInBucket[bucket] = node;
    }
  else
    {
      assert( n != NULL );
      node->bucketPrev = n->bucketPrev;
      n->bucketPrev->bucketNext = node;
      n->bucketPrev = node;
    }
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
  if( minBucket != bucketTableSize )
    {
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
}


void
cleanLists( void )
{
  headOPEN = NULL;
  tailOPEN = NULL;
  CLOSE = NULL;
  memset( firstNodeInBucket, 0, bucketTableSize * sizeof( node_t* ) );
  memset( lastNodeInBucket, 0, bucketTableSize * sizeof( node_t* ) );
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
** Node Expansion: particulars
**
**/

int
forwardNodeExpansion( register node_t *node, register node_t ***result )
{
  register int *p, *a, *d, *alpha;
  register int child;
  register node_t *tmp;
  register suboperator_t *sub;

  static int initialized = 0;
  static node_t **buffer;

  /* initialization */
  if( !initialized )
    {
      initialized = 1;
      buffer = (node_t**)malloc( numberOperators * sizeof( node_t* ) );
      if( !buffer )
	fatal( noMoreMemory );
    }

  /* update problem data */
  ++expandedNodes;

  /* get some initial space */
  child = 0;
  buffer[child] = getNode();

  /* node expansion */
  for( alpha = validOperators; *alpha != -1; ++alpha )
    {
      /* preconditions */
      for( p = operatorTable[*alpha].prec; (*p != 0) && asserted( node->state, *p ); ++p );
      if( *p == 0 )
	{
	  /* state progression */
	  memcpy( buffer[child]->state, node->state, SIZE_PACKS * sizeof( atom_t ) );

	  /* operators effects */
	  for( a = operatorTable[*alpha].add; *a != 0; ++a )
	    set( buffer[child]->state, *a );
	  for( d = operatorTable[*alpha].del; *d != 0; ++d )
	    clear( buffer[child]->state, *d );

	  /* suboperators effects */
	  for( sub = operatorTable[*alpha].suboperators; sub != NULL; sub = sub->next )
	    {
	      for( p = sub->prec; (*p != 0) && asserted( node->state, *p ); ++p );
	      if( *p == 0 )
		{
		  for( a = sub->add; *a != 0; ++a )
		    set( buffer[child]->state, *a );
		  for( d = sub->del; *d != 0; ++d )
		    clear( buffer[child]->state, *d );
		}
	    }

	  /* get cached node */
	  tmp = readNodeHash( buffer[child]->state );
	  if( (tmp == NULL) ||
	      (node->cost + 1 < tmp->cost) )
	    {
	      ++generatedNodes;

	      /* check for tmp in OPEN */
	      if( tmp != NULL )
		{
		  if( verbose >= 6 )
		    {
		      if( tmp->open == 0 )
			fprintf( stderr, "NODE-EXPANSION: reopening node %p\n", tmp );
		      else
			fprintf( stderr, "NODE-EXPANSION: updating OPEN node %p\n", tmp );
		    }

		  /* child node heuristic information */
		  buffer[child]->valid = 1;
		  buffer[child]->h1_plus = tmp->h1_plus;
		  buffer[child]->h1_max = tmp->h1_max;

		  /* remove old node from hash and open list */
		  removeNodeFromHash( tmp );
		  if( tmp->open == 1 )
		    {
		      removeNodeFromOPEN( tmp );
		    }
		}
	      else
		{
		  heuristics( buffer[child] );
		}

	      /* set node data */
	      buffer[child]->operator = *alpha;
	      buffer[child]->father = node;
	      buffer[child]->cost = node->cost + 1;
	      buffer[child]->open = 1;

	      /* node caching and node space allocation */
	      if( buffer[child]->valid == 1 )
		{
		  insertIntoNodeHash( buffer[child] );
		  buffer[++child] = getNode();
		}
	    }
	}
    }

  /* free resources and return */
  freeLastNodes( 1 );
  *result = buffer;
  return( child );
}


int
backwardNodeExpansion( register node_t *node, register node_t ***result )
{
  register int *p, *a, *d, *alpha;
  register int i, atom, child;
  register node_t *tmp;

#if defined(USE_MUTEX)
  register mutex_t *mutex;
  register mutexSet_t *set;
#endif

  static int initialized = 0;
  static char *operatorDisplay;
  static node_t **buffer;

  /* initialization */
  if( !initialized )
    {
      initialized = 1;
      buffer = (node_t**)malloc( numberOperators * sizeof( node_t* ) );
      operatorDisplay = (char*)malloc( numberOperators * sizeof( char ) );
      if( !buffer || !operatorDisplay )
	fatal( noMoreMemory );
    }

  /* update problem data */
  ++expandedNodes;

  /* get applicable operators using admissibility information */
  for( i = 0; i < numberOperators; ++i )
    operatorDisplay[i] = 1;

  /* check admissibility */
  for( atom = 1; atom < SIZE_ATOMS; ++atom )
    if( asserted( node->state, atom ) )
      for( p = notAdmissible[atom]; (p != NULL) && (*p != 0); operatorDisplay[*p++] = 0 );

  /* get some initial space */
  child = 0;
  buffer[child] = getNode();

  /* node expansion */
  for( alpha = validOperators; *alpha != -1; ++alpha )
    if( operatorDisplay[*alpha] == 1 )
      {
	/* preconditions: S must has some add but no del */
	for( a = operatorTable[*alpha].add; (*a != 0) && !asserted( node->state, *a ); ++a );
	for( d = operatorTable[*alpha].del; (*d != 0) && !asserted( node->state, *d ); ++d );
	if( (*a != 0) && (*d == 0) )
	  {
	    /* state regression */
#if defined(USE_MUTEX)
	    memcpy( staticState, node->state, SIZE_PACKS * sizeof( atom_t ) );
#endif
	    memcpy( buffer[child]->state, node->state, SIZE_PACKS * sizeof( atom_t ) );
	    for( p = operatorTable[*alpha].prec; *p != 0; ++p )
	      {
#if defined(USE_MUTEX)
		set( staticState, *p );
#endif
		set( buffer[child]->state, *p );
	      }
	    for( a = operatorTable[*alpha].add; *a != 0; ++a )
	      {
#if defined(USE_MUTEX)
		set( staticState, *a );
#endif
		clear( buffer[child]->state, *a );
	      }
#if defined(USE_MUTEX)
	    for( d = operatorTable[*alpha].del; *d != 0; ++d )
	      clear( staticState, *d );
#endif

	    /* check if valid-bit before mutex check (less expensive) */
	    heuristics( buffer[child] );
	    if( buffer[child]->valid == 0 )
	      continue;

#if defined(USE_MUTEX)
	    /* check mutexes in the regressed and projected states */
	    for( set = mutexSetList; set != NULL; set = set->next )
	      if( asserted( staticState, set->x ) || asserted( buffer[child]->state, set->x ) )
		{
		  for( mutex = set->set; mutex != NULL; mutex = mutex->next )
		    if( (asserted( staticState, mutex->x ) && asserted( staticState, mutex->y )) ||
			(asserted( buffer[child]->state, mutex->x ) && asserted( buffer[child]->state, mutex->y )) )
		      break;
		  if( mutex != NULL )
		    break;
		}
	    if( set != NULL )
	      continue;
#endif

	    /* get cached node */
	    tmp = readNodeHash( buffer[child]->state );
	    if( (tmp == NULL) ||
		(node->cost + 1 < tmp->cost) )
	      {
		/* statistics */
		++generatedNodes;

		/* check for tmp in OPEN */
		if( tmp != NULL )
		  {
		    if( verbose >= 6 )
		      {
			if( tmp->open == 0 )
			  fprintf( stderr, "NODE-EXPANSION: reopening node %p\n", tmp );
			else
			  fprintf( stderr, "NODE-EXPANSION: updating OPEN node %p\n", tmp );
		      }

		    /* remove old node from hash and open list */
		    removeNodeFromHash( tmp );
		    if( tmp->open == 1 )
		      removeNodeFromOPEN( tmp );
		  }

		/* set node data */
		buffer[child]->operator = *alpha;
		buffer[child]->father = node;
		buffer[child]->cost = node->cost + 1;
		buffer[child]->open = 1;

		/* node caching and node space allocation */
		if( buffer[child]->valid == 1 )
		  {
		    insertIntoNodeHash( buffer[child] );
		    buffer[++child] = getNode();
		  }
	      }
	  }
      }

  /* free resources and return */
  freeLastNodes( 1 );
  *result = buffer;
  return( child );
}



/*********************************************************
**********************************************************
**
** Static Atoms Compilation
**
**/

void
staticAtomCompilation( void )
{
  register int i, j, *a, *p, res;
  register suboperator_t *sub;

  /* register entry */
  registerEntry( "staticAtomCompilation( void )" );

  p = staticAtomsList;
  numberStaticAtoms = 0;
  memset( staticAtom, 0, MAXATOMPACKS * sizeof( atom_t ) );
  for( i = 1; i < SIZE_ATOMS; ++i )
    {
      res = 1;
      for( j = 0; (j < numberOperators) && (res == 1); ++j )
	if( operatorTable[j].valid == 1 )
	  {
	    if( asserted( staticInitialState, i ) )
	      {
		for( a = operatorTable[j].del; (*a != 0) && (*a != i); ++a );
		res = res && (*a == 0);
		for( sub = operatorTable[j].suboperators; (sub != NULL) && (res == 1); sub = sub->next )
		  {
		    for( a = sub->del; (*a != 0) && (*a != i); ++a );
		    res = res && (*a == 0);
		  }
	      }
	    else
	      {
		for( a = operatorTable[j].add; (*a != 0) && (*a != i); ++a );
		res = res && (*a == 0);
		for( sub = operatorTable[j].suboperators; (sub != NULL) && (res == 1); sub = sub->next )
		  {
		    for( a = sub->add; (*a != 0) && (*a != i); ++a );
		    res = res && (*a == 0);
		  }
	      }
	  }

      if( res == 1 )
	{
	  *p++ = i;
	  set( staticAtom, i );
	  ++numberStaticAtoms;
	  if( verbose >= 5 )
	    fprintf( stderr, "static %s\n", readAtomName( i ) );
	}
    }
  *p = 0;

  /* register exit */
  registerExit();
}



/*********************************************************
**********************************************************
**
** Operator Compilation
**
**/

void
operatorCompilation( void )
{
  register int i, j, k, *p, *a, *d, *t;
  register iatom_t *entry;
  register suboperator_t *sub, *subNext, *subPrev;

  /* register entry */
  registerEntry( "operatorCompilation()" );

  /* initialization */
  memset( relevantAtom, 0, MAXATOMPACKS * sizeof( atom_t ) );
  memset( parents, 0, ATOMSPERPACK * MAXATOMPACKS * sizeof( int* ) );
  memset( parentsSize, 0, ATOMSPERPACK * MAXATOMPACKS * sizeof( int ) );
  memcpy( staticState, staticInitialState, MAXATOMPACKS * sizeof( atom_t ) );

  /* reachability analysis and goal cone */
  reachabilityAnalysis( staticInitialState );
  identifyGoalParents( _low_goalAtoms );

  /* complete initialState with negated atoms */
  if( _low_requirements & REQ_ADL )
    {
      for( a = _low_initialAtoms; *a != 0; ++a );
      for( i = 1; i < SIZE_ATOMS; ++i )
	if( !asserted( staticInitialState, i ) && (_low_negatedAtom[i] != 0 ) )
	  {
	    set( staticInitialState, _low_negatedAtom[i] );
	    *a++ = _low_negatedAtom[i];
	  }
      *a = 0;
    }

  /* generate ground instances of actions */
  numberOperators = 0;
  _low_groundingOperators = 1;
  instantiateOperators( &insertOperator, &testOperatorPrecondition );

  /* compute static atoms & move them to the tail of state */
  staticAtomCompilation();
  if( 0 && staticCompilation == 1 )
    {
      /* move them to the tail */
      for( j = numberAtoms, k = 0; (j > 0) && asserted( staticAtom, j ); --j, ++k );
      for( i = 1; i < j + 1; ++i )
	if( asserted( staticAtom, i ) )
	  {
	    staticAtomsPermutationDisplay[i] = j;
	    staticAtomsPermutationDisplay[j] = i;
	    for( --j; (j > 0) && asserted( staticAtom, j ); --j, ++k );
	  }
	else
	  {
	    staticAtomsPermutationDisplay[i] = i;
	  }

      /* update operators */
      for( i = 0; i < numberOperators; ++i )
	if( operatorTable[i].valid == 1 )
	  {
	    /* first the easy ones: add and delete lists */
	    for( a = operatorTable[i].add; *a != 0; ++a )
	      *a = staticAtomsPermutationDisplay[*a];
	    for( d = operatorTable[i].del; *d != 0; ++d )
	      *d = staticAtomsPermutationDisplay[*d];

	    /* now, the not so easy */
	    for( a = p = operatorTable[i].prec; *p != 0; ++p )
	      if( !asserted( staticAtom, *p ) )
		*a++ = staticAtomsPermutationDisplay[*p];
	      else
		--operatorTable[i].precSize;
	    *a = 0;

	    /* now, for suboperators */
	    subPrev = NULL;
	    sub = operatorTable[i].suboperators;
	    while( sub != NULL )
	      {
		subNext = sub->next;

		/* If prec has a false static atom, remove the whole suboperator */
 		for( a = p = sub->prec; *p != 0; ++p )
		  if( asserted( staticAtom, *p ) )
		    {
		      --sub->precSize;
		      if( !asserted( staticInitialState, *p ) )
			break;
		    }
		  else
		    *a++ = staticAtomsPermutationDisplay[*p];
		if( *p != 0 )
		  {
		    /* free resources */
		    free( sub->prec );
		    free( sub->add );
		    free( sub->del );
		    free( sub );

		    /* relink */
		    if( subPrev == NULL )
		      operatorTable[i].suboperators = subNext;
		    else
		      subPrev->next = subNext;
		  }
		else
		  {
		    *a = 0;
		    subPrev = sub;

		    /* add and del lists */
		    for( a = sub->add; *a != 0; ++a )
		      *a = staticAtomsPermutationDisplay[*a];
		    for( d = sub->del; *d != 0; ++d )
		      *d = staticAtomsPermutationDisplay[*d];
		  }

		/* next */
		sub = subNext;
	      }
	  }

      /* update low level and static initial and goal states */
      memset( staticInitialState, 0, MAXATOMPACKS * sizeof( atom_t ) );
      memset( staticGoalState, 0, MAXATOMPACKS * sizeof( atom_t ) );
      for( a = p = _low_initialAtoms; *p != 0; ++p )
	if( !asserted( staticAtom, *p ) )
	  {
	    set( staticInitialState, staticAtomsPermutationDisplay[*p] );
	    *a++ = staticAtomsPermutationDisplay[*p];
	  }
      *a = 0;
      for( a = p = _low_goalAtoms; *p != 0; ++p )
	if( !asserted( staticAtom, *p ) )
	  {
	    set( staticGoalState, staticAtomsPermutationDisplay[*p] );
	    *a++ = staticAtomsPermutationDisplay[*p];
	  }
      *a = 0;

      /* update atom hash */
      for( i = 0; i < ATOMHASHSIZE; ++i )
	for( entry = atomHashTable[i]; entry != NULL; entry = entry->next )
	  entry->idx = staticAtomsPermutationDisplay[entry->idx];

      /* shrink state. After this everything should be ok */
      numberAtoms -= numberStaticAtoms;
    }

  /* generate H operators (only for ADL) */
  if( _low_requirements & REQ_ADL )
    generateHOperators();

  /* space allocation */
  invPrecTableSize = (int*)calloc( SIZE_ATOMS, sizeof( int ) );
  invPrecTable = (int**)calloc( SIZE_ATOMS, sizeof( int* ) );
  invAddTableSize = (int*)calloc( SIZE_ATOMS, sizeof( int ) );
  invAddTable = (int**)calloc( SIZE_ATOMS, sizeof( int* ) );
  invDelTableSize = (int*)calloc( SIZE_ATOMS, sizeof( int ) );
  invDelTable = (int**)calloc( SIZE_ATOMS, sizeof( int* ) );
  if( !invPrecTableSize || !invAddTableSize || !invDelTableSize ||
      !invPrecTable || !invAddTable || !invDelTable )
    fatal( noMoreMemory );
  if( _low_requirements & REQ_ADL )
    {
      HInvPrecTableSize = (int*)calloc( SIZE_ATOMS, sizeof( int ) );
      HInvPrecTable = (int**)calloc( SIZE_ATOMS, sizeof( int* ) );
      HInvAddTableSize = (int*)calloc( SIZE_ATOMS, sizeof( int ) );
      HInvAddTable = (int**)calloc( SIZE_ATOMS, sizeof( int* ) );
      if( !HInvPrecTableSize || !HInvPrecTable || !HInvAddTableSize || !HInvAddTable )
	fatal( noMoreMemory );
    }

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
		      (invPrecTableSize[*p] == 0 ? 16 : (int)(INCRATE * invPrecTableSize[*p]));
		    invPrecTable[*p] = 
		      (int*)realloc( invPrecTable[*p], invPrecTableSize[*p] * sizeof( int ) );
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
		      (invAddTableSize[*a] == 0 ? 16 : (int)(INCRATE * invAddTableSize[*a]));
		    invAddTable[*a] = 
		      (int*)realloc( invAddTable[*a], invAddTableSize[*a] * sizeof( int ) );
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
		      (invDelTableSize[*d] == 0 ? 16 : (int)(INCRATE * invDelTableSize[*d]));
		    invDelTable[*d] = 
		      (int*)realloc( invDelTable[*d], invDelTableSize[*d] * sizeof( int ) );
		    if( !invDelTable[*d] )
		      fatal( noMoreMemory );
		    t = invDelTable[*d];
		  }
		*(t+j) = i + 1;
		*(t+j+1) = 0;
	      }
	  }
      }

  /* H inverse table fill */
  for( i = 0; i < numberHOperators; ++i )
    if( HOperatorTable[i].valid == 1 )
      {
	/* inverse table for preconditions */
	for( p = HOperatorTable[i].prec; *p != 0; ++p )
	  {
	    k = HInvPrecTableSize[*p];
	    t = HInvPrecTable[*p];
	    for( j = 0; (j < k) && (*(t+j) != 0) && (*(t+j) != i + 1); ++j );
	    if( (j == k) || (*(t+j) == 0) )
	      {
		if( j + 1 >= k )
		  {
		    HInvPrecTableSize[*p] = 
		      (HInvPrecTableSize[*p] == 0 ? 16 : (int)(INCRATE * HInvPrecTableSize[*p]));
		    HInvPrecTable[*p] = 
		      (int*)realloc( HInvPrecTable[*p], HInvPrecTableSize[*p] * sizeof( int ) );
		    if( !HInvPrecTable[*p] )
		      fatal( noMoreMemory );
		    t = HInvPrecTable[*p];
		  }
		*(t+j) = i + 1;
		*(t+j+1) = 0;
	      }
	  }

	/* inverse table for add */
	for( a = HOperatorTable[i].add; *a != 0; ++a )
	  {
	    k = HInvAddTableSize[*a];
	    t = HInvAddTable[*a];
	    for( j = 0; (j < k) && (*(t+j) != 0) && (*(t+j) != i + 1); ++j );
	    if( (j == k) || (*(t+j) == 0) )
	      {
		if( j + 1 >= k )
		  {
		    HInvAddTableSize[*a] = 
		      (HInvAddTableSize[*a] == 0 ? 16 : (int)(INCRATE * HInvAddTableSize[*a]));
		    HInvAddTable[*a] = 
		      (int*)realloc( HInvAddTable[*a], HInvAddTableSize[*a] * sizeof( int ) );
		    if( !HInvAddTable[*a] )
		      fatal( noMoreMemory );
		    t = HInvAddTable[*a];
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
  registerEntry( "admissibleOperatorCompilation()" );

  /* space allocation */
  notAdmissible = (int**)calloc( SIZE_ATOMS, sizeof( int* ) );
  notAdmissibleSize = (int*)calloc( SIZE_ATOMS, sizeof( int ) );
  if( !notAdmissible || !notAdmissibleSize )
    fatal( noMoreMemory );

  /* compilation */
  for( x = 1; x < SIZE_ATOMS; ++x )
    {
      last = 0;
      for( op = 0; op < numberOperators; ++op )
	if( operatorTable[op].valid == 1 )
	  {
	    exists = 0;
	    for( p = operatorTable[op].prec; (*p != 0) && (exists == 0); ++p )
#if !defined(USE_MUTEX)
	      if( PAIR( x, *p ).max == INT_MAX )
#else
	      if( MUTEX( x, *p ) )
#endif
		{
		  for( a = operatorTable[op].add; (*a != 0) && (*a != x) && (*a != *p); ++a );
		  exists = (*a == 0);
		}

	    if( exists == 1 )
	      {
		if( last + 1 >= notAdmissibleSize[x] )
		  {
		    notAdmissibleSize[x] = (notAdmissibleSize[x] == 0 ? 16 : (int)(INCRATE * notAdmissibleSize[x]));
		    notAdmissible[x] = (int*)realloc( notAdmissible[x], notAdmissibleSize[x] * sizeof( int ) );
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
	      parentsSize[atom] = (parentsSize[atom] == 0 ? 4 : INCRATE * parentsSize[atom]);
	      if( !(parents[atom] = (int*)realloc( parents[atom], parentsSize[atom] * sizeof( int ) )) )
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
      if( !asserted( relevantAtom, *ip ) )
	{
	  ++numberRelevantAtoms;
	  set( relevantAtom, *ip );
	  identifyGoalParents( parents[*ip] );
	}
}



/*********************************************************
**********************************************************
**
** H Operators
**
**/

void
generateHOperators( void )
{
  register int op, psize, asize, dsize;
  register int k, j, *s;
  register suboperator_t *sub;

  /* space allocation */
  HSubTableSize = (int*)calloc( numberOperators, sizeof( int ) );
  HSubTable = (int**)calloc( numberOperators, sizeof( int* ) );
  if( !HSubTableSize || !HSubTable )
    fatal( noMoreMemory );

  /* go! */
  for( op = 0; op < numberOperators; ++op )
    {
      /* resize HOperatorTable */
      if( numberHOperators == HOperatorTableSize )
	{
	  HOperatorTableSize = (HOperatorTableSize == 0 ? 16 : INCRATE * HOperatorTableSize);
	  HOperatorTable = 
	    (operator_t*)realloc( HOperatorTable, HOperatorTableSize * sizeof( operator_t ) );
	  if( !HOperatorTable )
	    fatal( noMoreMemory );
	}

      /* this is a copy of father */
      HOperatorTable[numberHOperators].prec = operatorTable[op].prec;
      HOperatorTable[numberHOperators].add = operatorTable[op].add;
      HOperatorTable[numberHOperators].del = operatorTable[op].del;
      HOperatorTable[numberHOperators].father = op;
      HOperatorTable[numberHOperators].valid = 1;

      /* fill operatorsWithNoPrec table */
      if( operatorTable[op].precSize == 0 )
	{
	  ++HOperatorsWithNoPrecSize;
	  HOperatorsWithNoPrec =
	    (int*)realloc( HOperatorsWithNoPrec, HOperatorsWithNoPrecSize * sizeof( int ) );
	  HOperatorsWithNoPrec[HOperatorsWithNoPrecSize-1] = numberHOperators;
	}

      /* next H operator */
      ++numberHOperators;

      /* now, the suboperators */
      for( sub = operatorTable[op].suboperators; sub != NULL; sub = sub->next )
	{
	  /* resize HOperatorTable */
	  if( numberHOperators == HOperatorTableSize )
	    {
	      HOperatorTableSize = (HOperatorTableSize == 0 ? 16 : INCRATE * HOperatorTableSize);
	      HOperatorTable = 
		(operator_t*)realloc( HOperatorTable, HOperatorTableSize * sizeof( operator_t ) );
	      if( !HOperatorTable )
		fatal( noMoreMemory );
	    }

	  /* simple fields */
	  HOperatorTable[numberHOperators].father = op;
	  HOperatorTable[numberHOperators].valid = 1;

	  /* space allocation for precondition, add, del lists */
	  psize = operatorTable[op].precSize + sub->precSize;
	  asize = operatorTable[op].addSize + sub->addSize;
	  dsize = operatorTable[op].delSize + sub->delSize;
	  HOperatorTable[numberHOperators].prec = (int*)calloc( psize + 1, sizeof( int ) );
	  HOperatorTable[numberHOperators].add = (int*)calloc( asize + 1, sizeof( int ) );
	  HOperatorTable[numberHOperators].del = (int*)calloc( dsize + 1, sizeof( int ) );
	  if( !HOperatorTable[numberHOperators].prec || !HOperatorTable[numberHOperators].add ||
	      !HOperatorTable[numberHOperators].del )
	    fatal( noMoreMemory );

	  /* fill it */
	  memcpy( HOperatorTable[numberHOperators].prec, operatorTable[op].prec,
		  operatorTable[op].precSize * sizeof( int ) );
	  memcpy( &HOperatorTable[numberHOperators].prec[operatorTable[op].precSize], sub->prec,
		  (sub->precSize + 1) * sizeof( int ) );
	  memcpy( HOperatorTable[numberHOperators].add, operatorTable[op].add,
		  operatorTable[op].addSize * sizeof( int ) );
	  memcpy( &HOperatorTable[numberHOperators].add[operatorTable[op].addSize], sub->add,
		  (sub->addSize + 1) * sizeof( int ) );
	  memcpy( HOperatorTable[numberHOperators].del, operatorTable[op].del,
		  operatorTable[op].delSize * sizeof( int ) );
	  memcpy( &HOperatorTable[numberHOperators].del[operatorTable[op].delSize], sub->del,
		  (sub->delSize + 1) * sizeof( int ) );
	  HOperatorTable[numberHOperators].valid = 1;

	  /* order Prec, Add, and Del lists */
	  orderAtomList( HOperatorTable[numberHOperators].prec, psize );
	  orderAtomList( HOperatorTable[numberHOperators].add, asize );
	  orderAtomList( HOperatorTable[numberHOperators].del, dsize );

	  /* fill operatorsWithNoPrec table */
	  if( psize == 0 )
	    {
	      ++HOperatorsWithNoPrecSize;
	      HOperatorsWithNoPrec =
		(int*)realloc( HOperatorsWithNoPrec, HOperatorsWithNoPrecSize * sizeof( int ) );
	      HOperatorsWithNoPrec[HOperatorsWithNoPrecSize-1] = numberHOperators;
	    }

	  /* fill HSub table */
	  k = HSubTableSize[op];
	  s = HSubTable[op];
	  for( j = 0; (j < k) && (*(s+j) != 0) && (*(s+j) != numberHOperators + 1); ++j );
	  if( (j == k) || (*(s+j) == 0) )
	    {
	      if( j + 1 >= k )
		{
		  HSubTableSize[op] = (HSubTableSize[op] == 0 ? 16 : (int)(INCRATE * HSubTableSize[op]));
		  HSubTable[op] = (int*)realloc( HSubTable[op], HSubTableSize[op] * sizeof( int ) );
		  if( !HSubTable[op] )
		    fatal( noMoreMemory );
		  s = HSubTable[op];
		}
	      *(s+j) = numberHOperators + 1;
	      *(s+j+1) = 0;
	    }

	  /* next H operator */
	  ++numberHOperators;
	}
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
  register int i, j, *p, *a, *d, *op;
  register int change, deleteCondition;
  struct mutex_s *mutex, *tmpMutex, *tmpList;

  /* register entry */
  registerEntry( "mutexCompilation()" );

  /* space allocation */
  _mutex = (int*)calloc( (((numberAtoms+1)*numberAtoms) >> 1), sizeof( int ) );
  mutexSet = (mutexSet_t*)calloc( SIZE_ATOMS, sizeof( mutexSet_t ) );
  if( !_mutex || !mutexSet )
    fatal( noMoreMemory );

  if( mutexesAllPairs )
    {
      /* bootstrap mutex set. Another method: add all pairs <p,q> of atoms.
       */
      for( i = 1; i < SIZE_ATOMS; ++i )
	for( j = 1; j < i; ++j )
	  {
	    /* if the staticCompilation flag is set, then all static atoms
	       were removed. In other case, don't consider them when building
	       the mutexes.
	    */
	    if( ((staticCompilation == 1) || 
		 (!asserted( staticAtom, i ) && !asserted( staticAtom, j ))) &&
		(!asserted( staticInitialState, i ) || !asserted( staticInitialState, j )) )
	      newMutex( i, j, &mutexList );
	  }
    }
  else
    {
      /* bootstrap mutex set. Briefly, add those <p,q> such that exists an operator
	 op with p in ADD(op) and q in DEL(op). After this, protect each initial 
	 mutex <p,q> with a mutexes <p,r> (correspondngly <q,r>) for r in PREC(op')
	 where op' is an operator with q in ADD(op') (correspondingly p).
      */
      for( i = 0; i < numberOperators; ++i )
	if( operatorTable[i].valid == 1 )
	  {
	    /* I have no checked the proper use of staticAtom detection for
	       this part, so don't use that information.
	    */
	    for( a = operatorTable[i].add; *a != 0; ++a )
	      for( d = operatorTable[i].del; *d != 0; ++d )
		if( !asserted( staticInitialState, *a ) || !asserted( staticInitialState, *d ) )
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
		    (!asserted( staticInitialState, mutex->y ) || !asserted( staticInitialState, *p )) )
		  newMutex( mutex->y, *p, &tmpList );

	  /* add protection using operators op with q in ADD(op) */
	  if( invAddTable[mutex->y] != NULL )
	    for( op = invAddTable[mutex->y]; *op != 0; ++op )
	      for( p = operatorTable[(*op)-1].prec; *p != 0; ++p )
		if( !MUTEX( mutex->x, *p ) &&
		    (!asserted( staticInitialState, mutex->x ) || !asserted( staticInitialState, *p )) )
		  newMutex( mutex->x, *p, &tmpList );
	}

      /* fusion the initial mutex set with "protectors" */
      if( tmpList != NULL )
	{
	  for( mutex = tmpList; mutex->next != NULL; mutex = mutex->next );
	  mutex->next = mutexList;
	  mutexList = tmpList;
	}
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
  if( verbose >= 5 )
    for( mutex = mutexList; mutex != NULL; mutex = mutex->next )
      {
	fprintf( stderr, "mutex <%s,", readAtomName( mutex->x ) );
	fprintf( stderr, "%s>\n", readAtomName( mutex->y ) );
      }

  /* print total number of mutexes */
  fprintf( stderr, "MUTEXES: number detected mutexes = %d\n", numberMutexes );

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
  registerEntry( "mutexSetCompilation()" );

  /* mutexSet creation */
  for( i = 1; i < SIZE_ATOMS; ++i )
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
      if( !(mutex = (mutex_t*)malloc( sizeof( mutex_t ) )) )
	fatal( noMoreMemory );
      mutex->x = x;
      mutex->y = y;
      mutex->prev = NULL;
      mutex->next = *mutexList;
      if( *mutexList != NULL )
	(*mutexList)->prev = mutex;
      (*mutexList) = mutex;
      MUTEX( x, y ) = 1;

      ++mutexSet[x].size;
      ++numberMutexes;
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

  --mutexSet[mutex->x].size;
  --numberMutexes;
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
  register int i, *a, *p, *op;
  extern void generateVarsAndValues( void );

  /* register entry */
  registerEntry( "initialize()" );

  /* atomHashTable initialization */
  memset( atomHashTable, 0, ATOMHASHSIZE * sizeof( iatom_t* ) );

  /* check dimensionality */
  if( numberSchema >= MAXSCHEMA )
    fatal( maxSchema );

  /* initialState and goalState identification */
  generateVarsAndValues();

  /* initial state */
  for( a = _low_initialAtoms; *a != 0; ++a )
    set( staticInitialState, *a );

  /* goal state */
  p = _low_copyGoalAtoms;
  for( a = _low_goalAtoms; *a != 0; ++a )
    {
      set( staticGoalState, *a );
      *p++ = *a;
    }
  *p = 0;

  /* operator instantiation */
  operatorCompilation();

  /* generate valid operators list */
  validOperators = (int*)malloc( (numberOperators + 1) * sizeof( int ) );
  if( !validOperators )
    fatal( noMoreMemory );
  for( op = validOperators, i = 0; i < numberOperators; *op++ = i++ );
  *op = -1;

  /* some general info */
  fprintf( stderr, "GENERAL: node size %d = %d (fixed) + %d (variable)\n", 
	   NODESIZE, sizeof( node_t ), NODESIZE - sizeof( node_t ) );
  fprintf( stderr, "GENERAL: number of relevant atoms = %d\n", numberAtoms );

  /* H1 initialization */
  H1Setup( staticInitialState );
  memcpy( backwardH1Cost, H1Cost, ATOMSPERPACK * MAXATOMPACKS * sizeof( cost_t ) );

  /* more general info */
  fprintf( stderr, "GENERAL: number of operators = %d\n", numberOperators );
  if( _low_requirements & REQ_ADL )
    fprintf( stderr, "GENERAL: number of H operators = %d\n", numberHOperators );

  /* guess an upper bound for bucketTableSize and allocate memory for proper structures */
  bucketTableSize = 0;
  for( i = 1; i < SIZE_ATOMS; ++i )
    if( backwardH1Cost[i].plus < INT_MAX )
      bucketTableSize += backwardH1Cost[i].plus;
  bucketTableSize = MAX( bucketTableSize, MINBUCKETTABLESIZE );

  firstNodeInBucket = (node_t**)malloc( bucketTableSize * sizeof( node_t* ) );
  lastNodeInBucket = (node_t**)malloc( bucketTableSize * sizeof( node_t* ) );
  if( !firstNodeInBucket || !lastNodeInBucket )
    fatal( noMoreMemory );

  /* more general info */
  fprintf( stderr, "GENERAL: number of buckets = %d\n", bucketTableSize );

  /* nodeHashTable initialization */
  if( !(nodeHashValues = (unsigned*)malloc( SIZE_ATOMS * sizeof( unsigned ) )) )
    fatal( noMoreMemory );
  for( i = 0; i < SIZE_ATOMS; ++i )
    nodeHashValues[i] = lrand48() % NODEHASHSIZE;
  memset( nodeHashDiameter, 0, NODEHASHSIZE * sizeof( int ) );
  memset( nodeHashTable, 0, NODEHASHSIZE * sizeof( node_t* ) );

  /* successful initialization */
  globalInitialized = 1;

  /* register exit */
  registerExit();
}


void
backwardSearchInitialization( register schedule_t *schedule )
{
  register int *p, *q, *op;
  static int initialized = 0;

  if( (schedule->searchDirection == BACKWARD) && (initialized == 0) )
    {
      initialized = 1;

#if !defined(USE_MUTEX)
      /* H2 initialization */
      H2Setup( staticInitialState );

      /* further operator prunning */
      for( op = validOperators; *op != -1; ++op )
	{
	  for( p = operatorTable[*op].prec; *p != 0; ++p )
	    {
	      for( q = p; *q != 0; ++q )
		if( PAIR( *p, *q ).max == INT_MAX )
		  {
		    operatorTable[*op].valid = 0;
		    if( verbose >= 3 )
		      fprintf( stderr, "invalidating operator %s (%d)\n", operatorTable[*op].name, *op );
		    break;
		  }
	      if( *q != 0 )
		break;
	    }

	  /* need to remove it? */
	  if( (p != NULL) && (*p != 0) )
	    for( p = op; *p != -1; ++p )
	      *p = *(p+1);
	}
#endif

      /* these three procedures should be called in this order! */
#if defined(USE_MUTEX)
      mutexCompilation();
#endif
      admissibleOperatorCompilation();
#if defined(USE_MUTEX)
      mutexSetCompilation();
#endif
    }
}



/*********************************************************
**********************************************************
**
** Planning
**
**/

int
goalState( register atom_t *state )
{
  register int *g;

  for( g = _low_goalAtoms; *g != 0; ++g )
    if( !asserted( state, *g ) )
      break;
  return( *g == 0 );
}


int
checkSolution( register node_t *node )
{
  register int *atom;

  if( searchDirection == FORWARD )
    {
      return( goalState( node->state ) );
    }
  else
    {
      /* set initial state */
      memcpy( staticState, staticInitialState, MAXATOMPACKS * sizeof( atom_t ) );

      /* apply operators */
      while( (node != NULL) && (node->father != NULL) )
	{
	  /* check preconditions */
	  for( atom = operatorTable[node->operator].prec; (*atom != 0) && asserted( staticState, *atom ); ++atom );
	  if( *atom != 0 )
	    {
	      fprintf( stderr, "****** TENTATIVE SOLUTION IS NOT **********\n" );
	      return( 0 );
	    }

	  /* apply add-list */
	  for( atom = operatorTable[node->operator].add; *atom != 0; ++atom )
	    set( staticState, *atom );

	  /* apply del-list */
	  for( atom = operatorTable[node->operator].del; *atom != 0; ++atom )
	    clear( staticState, *atom );

	  /* next operator */
	  node = node->father;
	}

      /* check goal state */
      return( goalState( staticState ) );
    }
}


int
checkProblem( void )
{
  register int *g1, *g2;

  /* check H1Costs for atoms in goal */
  for( g1 = _low_copyGoalAtoms; *g1 != 0; ++g1 )
    if( asserted( staticAtom, *g1 ) )
      {
	for( g2 = _low_initialAtoms; *g2 != 0; ++g2 )
	  if( staticAtomsPermutationDisplay[*g1] == *g2 )
	    break;
	if( *g2 == 0 )
	  return( 0 );
      }
    else
      {
	if( backwardH1Cost[staticAtomsPermutationDisplay[*g1]].max == INT_MAX )
	  return( 0 );
      }

  /* check H1Costs for atoms in goal */
  for( g1 = _low_goalAtoms; *g1 != 0; ++g1 )
    if( backwardH1Cost[*g1].max == INT_MAX )
      return( 0 );

  /* check H2Costs for atoms in goal */
  if( H2Computed == 1 )
    {
      for( g1 = _low_goalAtoms; *g1 != 0; ++g1 )
	for( g2 = g1; *g2 != 0; ++g2 )
	  if( PAIR( *g1, *g2 ).max == INT_MAX )
	    return( 0 );
    }

  /* problem is ok */
  return( 1 );
}


void
printOperator( register FILE *file, register int alpha )
{
  register int *p;

  fprintf( file, "operator %d \"%s\":\n", alpha, operatorTable[alpha].name );
  fprintf( file, "\tprec = " );
  for( p = operatorTable[alpha].prec; *p != 0; ++p )
    fprintf( file, "%s ", readAtomByNumber( *p )->name );
  fprintf( file, "\n\tadd = " );
  for( p = operatorTable[alpha].add; *p != 0; ++p )
    fprintf( file, "%s ", readAtomByNumber( *p )->name );
  fprintf( file, "\n\tdel = " );
  for( p = operatorTable[alpha].del; *p != 0; ++p )
    fprintf( file, "%s ", readAtomByNumber( *p )->name );
  fprintf( file, "\n" );
}


void
printState( register FILE *file, register atom_t *state )
{
  register int i;

  fprintf( file, "====>" );
  for( i = 1; i < SIZE_ATOMS; ++i )
    if( asserted( state, i ) )
      fprintf( file, " %s[%d]", readAtomName( i ), i );
  fprintf( file, "\n" );
}


void
printNode( register FILE *file, register char *prefix, register node_t *node )
{
  fprintf( file, "%s: node %p (cost = %d, h1plus = %d, h1max = %d, h2plus = %d, h2max = %d)\n",
	   prefix, node, node->cost, node->h1_plus, node->h1_max, node->h2_plus, node->h2_max );
  if( verbose >= 6 )
    {
      if( node->operator != -1 )
	fprintf( file, "operator = %s\n", operatorTable[node->operator].name );
      printState( file, node->state );
    }
}


void
printPath( register FILE *file, register node_t *node, register char *prefix, register char *suffix )
{
  if( searchDirection == FORWARD )
    {
      if( node->father != NULL )
	{
	  printPath( file, node->father, prefix, suffix );
	  fprintf( file, "%s%s%s", prefix, operatorTable[node->operator].name, suffix );
	}
    }
  else
    {
      while( node != NULL )
	{
	  if( node->father != NULL )
	    fprintf( file, "%s%s%s", prefix, operatorTable[node->operator].name, suffix );
	  node = node->father;
	}
    }
}


void
printStatistics( void )
{
  /* print problem data & statistics */
  nodeHashStatistics();
  fprintf( stderr, "STATISTICS: number expanded nodes = %d\n", expandedNodes );
  fprintf( stderr, "STATISTICS: number generated nodes = %d\n", generatedNodes );
  fprintf( stderr, "STATISTICS: average branching factor = %f\n", (float)generatedNodes / (float)expandedNodes );
}


void
cleanStatistics( void )
{
  expandedNodes = 0;
  generatedNodes = 0;
}



/*********************************************************
**********************************************************
**
** Best-First Search
**
**/

node_t *
startBFS( schedule_t *schedule )
{
  register node_t *result, *node;

  /* register entry */
  registerEntry( "startBFS()" );

  /* cleaning */
  cleanStatistics();
  cleanNodeHash();
  cleanLists();

  /* initialization */
  node = getNode();
  memset( firstNodeInBucket, 0, bucketTableSize * sizeof( node_t* ) );
  memset( lastNodeInBucket, 0, bucketTableSize * sizeof( node_t* ) );
  node->operator = -1;
  node->father = NULL;
  node->cost = 0;

  /* initial state setup */
  if( searchDirection == FORWARD )
    memcpy( node->state, staticInitialState, SIZE_PACKS * sizeof( atom_t ) );
  else
    memcpy( node->state, staticGoalState, SIZE_PACKS * sizeof( atom_t ) );

  /* insert initial node into hash/open */
  heuristics( node );
  insertIntoNodeHash( node );
  setInitialOPEN( node );
  if( verbose >= 2 )
    printState( stderr, node->state );

  /* search */
  result = BFS( schedule );

  /* register exit */
  registerExit();

  /* return */
  return( result );
}


node_t *
BFS( schedule_t *schedule )
{
  register int i, children;
  register node_t *currentNode;
  node_t **buffer;

  /* get first node */
  currentNode = getFirstOPEN();

  /* check for goal */
  if( (currentNode->h1_plus == 0) && checkSolution( currentNode ) )
    return( currentNode );

  /* initialize constraints */
  timeExpired = memoryExpired = 0;
  if( schedule->constraintType & TIME )
    setTimer( schedule->time );
  if( schedule->constraintType & MEMORY )
    memoryConstraint = schedule->memory;
  else
    memoryConstraint = -1;

  /* loop */
  while( currentNode != NULL )
    {
      /* check constraints */
      if( timeExpired || memoryExpired )
	{
	  fprintf( stderr, "CONSTRAINT: %s.\n", (timeExpired ? "time" : "memory") );
	  return( NULL );
	}

      /* basic assertion */
      assert( currentNode->valid == 1 );

      /* expand situation */
      if( searchDirection == FORWARD )
	children = forwardNodeExpansion( currentNode, &buffer );
      else
	children = backwardNodeExpansion( currentNode, &buffer );

      /* print current node */
      if( verbose >= 5 )
	{
	  printNode( stderr, "CURRENT NODE", currentNode );
	  fprintf( stderr, "number children = %d\n", children );
	  if( children == 0 )
	    {
	      fprintf( stderr, "action this state was %s\n", operatorTable[currentNode->operator].name );
	      printState( stderr, currentNode->state );
	    }
	}

      /* print child */
      if( verbose >= 6 )
	for( i = 0; i < children; ++i )
	  if( buffer[i]->valid == 1 )
	    printNode( stderr, "child", buffer[i] );

      /* test for goal */
      for( i = 0; i < children; ++i )
	if( (buffer[i]->valid == 1) && (buffer[i]->h1_plus == 0) && checkSolution( buffer[i] ) )
	  return( buffer[i] );

      /* delete currentNode from OPEN */
      removeNodeFromOPEN( currentNode );

      /* insert children into OPEN */
      nodeOrdering( buffer, children );

      /* next node */
      currentNode = getFirstOPEN();
    }
  return( NULL );
}



/*********************************************************
**********************************************************
**
** Greedy Best-First Search
**
**/

node_t *
startGBFS( schedule_t *schedule )
{
  register node_t *result, *node;

  /* register entry */
  registerEntry( "startGBFS()" );

  /* cleaning */
  cleanStatistics();
  cleanNodeHash();
  cleanLists();

  /* initialization */
  node = getNode();
  memset( firstNodeInBucket, 0, bucketTableSize * sizeof( node_t* ) );
  memset( lastNodeInBucket, 0, bucketTableSize * sizeof( node_t* ) );
  node->operator = -1;
  node->father = NULL;
  node->cost = 0;

  /* initial state setup */
  if( searchDirection == FORWARD )
    memcpy( node->state, staticInitialState, SIZE_PACKS * sizeof( atom_t ) );
  else
    memcpy( node->state, staticGoalState, SIZE_PACKS * sizeof( atom_t ) );

  /* insert initial node into hash/open */
  heuristics( node );
  insertIntoNodeHash( node );
  setInitialOPEN( node );
  if( verbose >= 2 )
    printState( stderr, node->state );

  /* search */
  result = GBFS( schedule );

  /* register exit */
  registerExit();

  /* return */
  return( result );
}


node_t *
GBFS( schedule_t *schedule )
{
  register int i, children;
  register node_t *currentNode, *nextNode;
  node_t **buffer;

  /* get first node */
  currentNode = getFirstOPEN();

  /* check for goal */
  if( (currentNode->h1_plus == 0) && checkSolution( currentNode ) )
    return( currentNode );

  /* initialize constraints */
  timeExpired = memoryExpired = 0;
  if( schedule->constraintType & TIME )
    setTimer( schedule->time );
  if( schedule->constraintType & MEMORY )
    memoryConstraint = schedule->memory;
  else
    memoryConstraint = -1;

  /* loop */
  while( currentNode != NULL )
    {
      /* check constraints */
      if( timeExpired || memoryExpired )
	{
	  fprintf( stderr, "CONSTRAINT: %s.\n", (timeExpired ? "time" : "memory") );
	  return( NULL );
	}

      /* basic assertion */
      assert( currentNode->valid == 1 );

      /* expand node */
      if( searchDirection == FORWARD )
	children = forwardNodeExpansion( currentNode, &buffer );
      else
	children = backwardNodeExpansion( currentNode, &buffer );

      /* print current node */
      if( verbose >= 5 )
	{
	  printNode( stderr, "CURRENT NODE", currentNode );
	  fprintf( stderr, "number children = %d\n", children );
	}

      /* print child */
      if( verbose >= 6 )
	for( i = 0; i < children; ++i )
	  if( buffer[i]->valid == 1 )
	    printNode( stderr, "child", buffer[i] );

      /* test for goal */
      for( i = 0; i < children; ++i )
	if( (buffer[i]->valid == 1) && (buffer[i]->h1_plus == 0) && checkSolution( buffer[i] ) )
	  return( buffer[i] );

      /* delete currentNode from OPEN */
      removeNodeFromOPEN( currentNode );

      /* insert children into OPEN */
      nodeOrdering( buffer, children );

      /* next node: first try a probe */
      nextNode = NULL;
      for( i = 0; i < children; ++i )
	if( (buffer[i]->valid == 1) &&
	    ((nextNode == NULL) ||
	     (buffer[i]->h1_plus < nextNode->h1_plus) ||
	     ((buffer[i]->h1_plus == nextNode->h1_plus) && (buffer[i]->h1_max < nextNode->h1_max))) )
	  nextNode = buffer[i];

      if( (nextNode != NULL) && 
	  ((nextNode->h1_plus > currentNode->h1_plus) ||
	   ((nextNode->h1_plus == currentNode->h1_plus) && (nextNode->h1_max >= currentNode->h1_max))) )
	nextNode = NULL;

      /* next node: if nothing, select by BFS */
      if( nextNode == NULL )
	currentNode = getFirstOPEN();
      else
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

void
registerEntry( register char *procedure )
{
  register procRegister_t *proc;

  if( !(proc = (procRegister_t*)malloc( sizeof( procRegister_t ) )) )
    fatal( noMoreMemory );
  proc->procedure = procedure;
  proc->next = procStackTop;
  procStackTop = proc;
}


int
registerExit( void )
{
  register procRegister_t *proc;
  struct rusage r_usage;

  if( procStackTop != NULL )
    {
      proc = procStackTop;
      getrusage( RUSAGE_SELF, &r_usage );
      proc->diffTime = (float)r_usage.ru_utime.tv_sec + (float)r_usage.ru_stime.tv_sec +
	(float)r_usage.ru_utime.tv_usec / (float)1000000 + (float)r_usage.ru_stime.tv_usec / (float)1000000;
      fprintf( stderr, "REGISTER: %s took %f secs\n", proc->procedure, proc->diffTime );
      procStackTop = proc->next;
      free( proc );
      return( 1 );
    }
  else
    {
      return( 0 );
    }
}


float
currentElapsedTime( void )
{
  struct rusage r_usage;

  getrusage( RUSAGE_SELF, &r_usage );
  return( (float)r_usage.ru_utime.tv_sec + (float)r_usage.ru_stime.tv_sec +
	  (float)r_usage.ru_utime.tv_usec / (float)1000000 + (float)r_usage.ru_stime.tv_usec / (float)1000000 );
}


void
flushAllRegisters( void )
{
  while( registerExit() );
}



/*********************************************************
**********************************************************
**
** Signals
**
**/

void
SIGHUPHandler( int sig )
{
  flushAllRegisters();
  exit( 1 );
}


void
timerExpirationHandler( int sig )
{
  timeExpired = 1;
}


void
setTimer( unsigned long msecs )
{
  static struct itimerval itv;

  /* clear global expirationTime flag */
  timeExpired = 0;

  /* set signal handler */
  signal( SIGVTALRM, &timerExpirationHandler );

  /* set it */
  itv.it_interval.tv_sec = 0;
  itv.it_interval.tv_usec = 0;
  itv.it_value.tv_sec = msecs/1000;
  itv.it_value.tv_usec = (msecs%1000)*1000;
  setitimer( ITIMER_VIRTUAL, &itv, NULL );
}



/*********************************************************
**********************************************************
**
** Scheduler
**
**/

node_t *
scheduler( schedule_t *schedule )
{
  register node_t *result;

  result = NULL;
  while( (result == 0) && (schedule != NULL) )
    {
      /* check implementation */
      if( (schedule->searchHeuristic == H2PLUS) || 
	  ((schedule->searchHeuristic == H2MAX) && (schedule->searchDirection == FORWARD)) ||
	  ((_low_requirements & REQ_ADL) && (schedule->searchDirection == BACKWARD)) )
	notYetImplemented( schedule );

      /* setup search parameters */
      searchAlgorithm = schedule->searchAlgorithm;
      searchDirection = schedule->searchDirection;
      searchHeuristic = schedule->searchHeuristic;

      /* additional initializations */
      backwardSearchInitialization( schedule );

      /* weak check if problem has solution */
      if( checkProblem() == 0 )
	{
	  fprintf( stderr, "SOLUTION: problem has no solution.\n" );
	  break;
	}

      /* print current schedule */
      fprintf( stderr, "SCHEDULE: %s %s with %s and W = %.1f\n",
	       searchDirectionName[searchDirection],
	       searchAlgorithmName[searchAlgorithm],
	       searchHeuristicName[searchHeuristic],
	       heuristicWeight );
      if( schedule->constraintType != 0 )
	fprintf( stderr, "SCHEDULE: constraints are time = %ld (msecs) and memory = %ld (Mbytes).\n",
		 (schedule->constraintType & TIME ? schedule->time : -1),
		 (schedule->constraintType & MEMORY ? schedule->memory : -1) );
      else
	fprintf( stderr, "SCHEDULE: unconstrained.\n" );

      /* make the search */
      switch( searchAlgorithm )
	{
	case _GBFS:
	  result = startGBFS( schedule );
	  break;
	case _BFS:
	  result = startBFS( schedule );
	  break;
	}

      /* print search results and some statistics */
      if( result == NULL )
	{
	  fprintf( stderr, "SOLUTION: no solution found\n" );
	}
      else
	{
	  fprintf( stderr, "SOLUTION: solution found (length = %d)\n", result->cost );
	  if( verbose > 0 )
	    printPath( stderr, result, "+  ", "\n" );
	}
      printStatistics();

      /* next option */
      schedule = schedule->next;
    }
  return( result );
}


/*********************************************************
**********************************************************
**
** Parameter Reading
**
**/

int
readSymbolicParameter( char *s, char **table, int tableSize )
{
  register int i;

  /* lookup table */
  for( i = 0; i < tableSize; ++i )
    if( !strcasecmp( s, table[i] ) )
      return( i );
  return( -1 );
}


void
parseSchedule( char *s )
{
  char *option, *p, *direction, *heuristic, *time;
  schedule_t *schedule, *sp;

  /* free resources */
  if( globalSchedule != NULL )
    free( globalSchedule );

  /* go over all options */
  option = strtok( s, ":" );
  while( option != NULL )
    {
      /* check option syntax */
      if( (option[0] != '[') || (option[strlen(option)-1] != ']') )
	fatal( optionSyntax );

      /* extract info */
      p = option + 1;
      direction = p;
      p = strchr( p, ',' );
      *p++ = '\0';
      heuristic = p;
      p = strchr( p, ',' );
      *p++ = '\0';
      time = p;
      p = strchr( p, ']' );
      *p++ = '\0';
      if( (direction == NULL) || (heuristic == NULL) || (time == NULL) )
	fatal( optionSyntax );

      /* build schedule */
      schedule = (schedule_t*)malloc( sizeof( schedule_t ) );
      schedule->memory = 0;
      schedule->time = atoi( time );
      schedule->constraintType = (schedule->time <= 0 ? 0 : TIME);
      schedule->searchAlgorithm = _BFS;
      schedule->searchDirection = readSymbolicParameter( direction, searchDirectionName, numberDirections );
      schedule->searchHeuristic = readSymbolicParameter( heuristic, searchHeuristicName, numberHeuristics );
      schedule->next = NULL;

      /* check parameters */
      if( (schedule->searchDirection == -1) || (schedule->searchHeuristic == -1) )
	fatal( searchParameter );

      /* insert schedule */
      for( sp = globalSchedule; (sp != NULL) && (sp->next != NULL); sp = sp->next );
      (sp == NULL ? globalSchedule : sp->next) = schedule;

      /* next option */
      option = strtok( NULL, ":" );
    }
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
      fprintf( stderr, "ERROR[%s:%d]: no more memory\n", file, line );
      break;
    case maxAtoms:
      fprintf( stderr, "ERROR: maximum atoms reached. Recompile\n" );
      break;
    case maxSchema:
      fprintf( stderr, "ERROR: maximum operator schemata reached. Recompile\n" );
      break;
    case noAlgorithm:
      fprintf( stderr, "ERROR: no search algorithm specified\n" );
      break;
    case optionSyntax:
      fprintf( stderr, "ERROR: schedule option. Format is [<direction>,<heuristic>,<time>]\n" );
      break;
    case searchParameter:
      fprintf( stderr, "ERROR: either bad search direction or heuristic.\n" );
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
usage( void )
{
  fprintf( stderr, "usage: hsp <flags>* [ <algorithm> | -S <schedule> ] <problem.pddl> <domain.pddl>\n" );
  fprintf( stderr, "where <flags> are among:\n" );
  fprintf( stderr, "   -v <level>\t\tVerbose level >= 0 (default is 1).\n" );
  fprintf( stderr, "   -w <weight>\t\tFloat to weight the heuristic component of the cost function.\n" );
  fprintf( stderr, "<algorithm> is:\n" );
  fprintf( stderr, "   -a <algorithm>\tEither 'bfs' or 'gbfs'.\n" );
  fprintf( stderr, "   -d <direction>\tEither 'forward' or 'backward'.\n" );
  fprintf( stderr, "   -h <heuristic>\tOne of 'h1plus', 'h1max', 'h2plus', 'h2max'.\n" );
  fprintf( stderr, "<schedule> is a colon separated <option> list where each option has\n" );
  fprintf( stderr, "form '[<direction>,<heuristic>,<msecs>]'. The options are performed\n" );
  fprintf( stderr, "sequentially until one finds a plan, or no more options are available\n" );
  fprintf( stderr, "(each option is attempted with the given time constraint).\n" );
  exit( -1 );
}


void
notYetImplemented( schedule_t *schedule )
{
  if( !(_low_requirements & REQ_ADL) )
    fprintf( stderr, "sorry, the option '[%s,%s]' is not yet supported.\n",
	     searchDirectionName[schedule->searchDirection], 
	     searchHeuristicName[schedule->searchHeuristic] );
  else
    fprintf( stderr, "sorry, for simple-adl it is only supported 'forward' search with 'h1max' or 'h1plus'.\n" );
  exit( -1 );
}


void
parseArguments( int argc, char **argv )
{
  char *progname;

  /* set some defaults */
  progname = argv[0];
  verbose = 1;
  mutexesAllPairs = 1;

  /* set default schedule */
  staticCompilation = 1;
  heuristicWeight = 2.0;
  searchAlgorithm = _BFS;
  searchDirection = FORWARD;
  searchHeuristic = H1PLUS;

  /* parse options */
  while( argc > 1 && *(*++argv) == '-' )
    {
      switch( (*argv)[1] )
	{
	case 'a':
	  searchAlgorithm = readSymbolicParameter( *++argv, searchAlgorithmName, numberAlgorithms );
	  --argc;
	  break;
	case 'd':
	  searchDirection = readSymbolicParameter( *++argv, searchDirectionName, numberDirections );
	  --argc;
	  break;
	case 'h':
	  searchHeuristic = readSymbolicParameter( *++argv, searchHeuristicName, numberHeuristics );
	  --argc;
	  break;
	case 'v':
	  verbose = atoi( *++argv );
	  --argc;
	  break;
	case 'w':
	  heuristicWeight = atof( *++argv );
	  --argc;
	  break;
	case 'S':
	  scheduleString = *++argv;
	  parseSchedule( strdup( scheduleString ) );
	  --argc;
	  break;
	default:
	  usage();
	  break;
	}
      --argc;
    }
  if( (argc != 3) || (searchAlgorithm == -1) || (searchDirection == -1) || (searchHeuristic == -1) )
    {
      usage();
    }
  else
    {
      problemFile = *argv++;
      domainFile = *argv;
    }

  /* set default schedule if empty one (i.e. if no "-S" parameter) */
  if( globalSchedule == NULL )
    {
      globalSchedule = (schedule_t*)malloc( sizeof( schedule_t ) );
      globalSchedule->constraintType = 0;
      globalSchedule->searchDirection = searchDirection;
      globalSchedule->searchAlgorithm = searchAlgorithm;
      globalSchedule->searchHeuristic = searchHeuristic;
      globalSchedule->next = NULL;
    }

  /* print parameters */
  fprintf( stderr, "PROBLEM: solving problem: %s %s\n", problemFile, domainFile );
  if( scheduleString != NULL )
    fprintf( stderr, "PARAMETERS: -S %s -w %f -v %d\n", scheduleString, heuristicWeight, verbose );
  else
    fprintf( stderr, "PARAMETERS: -a %s -d %s -h %s -w %f -v %d\n", 
	     searchAlgorithmName[searchAlgorithm], searchDirectionName[searchDirection],
	     searchHeuristicName[searchHeuristic], heuristicWeight, verbose );
}


int
parseProblem( void )
{
  int rv, fd, file;

  static char *files[2];
  extern int yyparse( void );
  extern int lineno;

  rv = 0;
  files[0] = problemFile;
  files[1] = domainFile;
  for( file = 0; file < 2; ++file )
    if( (fd = open( files[file], O_RDONLY )) == -1 )
      {
	perror( "ERROR: parsing files" );
	exit( -1 );
      }
    else
      {
	/* redirection of fd to stdin */
	if( file == 0 )
	  close( fileno( stdin ) );
	else
	  clearerr( stdin );
	dup( fd );
	_low_yyfile = files[file];
	lineno = 1;
	rv += yyparse();
	close( fileno( stdin ) );
	close( fd );
      }
  return( rv );
}


int
main( int argc, char **argv )
{
  int rv;
  node_t *result;

  /* set signal handler */
  signal( SIGHUP, &SIGHUPHandler );

  /* register entry */
  registerEntry( "main()" );

  /* initialize */
  parseArguments( argc, argv );

  /* parse problem */
  result = NULL;
  rv = parseProblem();
  if( rv == noError )
    {
      /* initialize planner */
      initializeMemoryMgmt();
      initialize();

      /* go! */
      result = scheduler( globalSchedule );
      rv = (result == NULL);
    }
  else
    {
      fprintf( stderr, "PARSE: parse error.\n" );
    }

  /* register exit */
  registerExit();
#if defined(COMPETITION_OUTPUT)
  if( result != NULL )
    {
      fprintf( stdout, "%s,%.4f,%d", _low_problemName, currentElapsedTime(), result->cost );
      printPath( stdout, result, ",", "" );
      fprintf( stdout, "\n" );
    }
#endif
  fprintf( stderr, "\n" );

  return( rv );
}
