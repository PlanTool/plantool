#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <assert.h>
#include "planner.h"


/* globals */
int level;
struct cost_s costs[SIZE], oldCosts[SIZE];
struct heuristicOperator_s **operatorsToFire;
int *operatorDisplay;

static int newAtoms[SIZE];
static struct heuristicOperator_s **lastOperator;


/* externs */
extern int number;
extern int numberAtoms;
extern int initialized;
extern char *actionName[];
extern char *objectName[];
extern char *predicateName[];
extern int  goalAtoms[];

extern int (*heuristicActionPreconditionsTable[MAXACTIONS])( register int * );
extern struct operator_s *initialOperators;
extern int numberRelevantAtoms;

extern float goalCost( register struct cost_s * );
extern void printSituation( FILE *, struct literal_s * );

extern struct heuristicOperator_s **influence[];
extern struct heuristicOperator_s **heuristicOpersWithNP;
extern int possibleHeuristicOperators, possibleHeuristicOpersWithNP;


int
heuristicStillPossible( register int *parameters, register int action )
{
  return( (*heuristicActionPreconditionsTable[action])( parameters ) );
}


void
heuristicInsertOperator( register int *parameters, register int action )
{
  (*heuristicActionTable[action])( parameters );
}


int
nextLevel( void )
{
  register int sum, *ip, *ap;
  register struct heuristicOperator_s **op;

  if( initialized )
    {
      /* compute operators to fire */
      operatorsToFire[0] = NULL;
      lastOperator = &operatorsToFire[0];
      memset( operatorDisplay, 0, possibleHeuristicOperators * sizeof( int ) );
      for( ap = newAtoms; *ap != 0; ++ap )
	for( op = influence[*ap]; (op != NULL) && (*op != NULL); ++op )
          if( operatorDisplay[(*op)->id] == 0 )
	    {
	      operatorDisplay[(*op)->id] = 1;
	      *lastOperator++ = *op;
	    }

      /* include all with null preconditions */
      if( possibleHeuristicOpersWithNP != 0 )
	for( op = heuristicOpersWithNP; *op != NULL; ++op )
	  *lastOperator++ = *op;
      *lastOperator = NULL;
      
      /* fire operators */
      ap = newAtoms;
      for( op = operatorsToFire; *op != NULL; ++op )
	{
	  sum = 1;
	  for( ip = (*op)->prec; *ip != 0; ++ip )
	    if( oldCosts[*ip].cost == INT_MAX )
	      break;
	    else
	      sum += oldCosts[*ip].cost;

	  if( *ip == 0 )
	    {
	      /* we have a valid operator */
	      for( ip = (*op)->add; *ip != 0; ++ip )
		if( sum < costs[*ip].cost )
		  {
		    costs[*ip].cost = sum;
		    costs[*ip].level = level + 1;
		    *ap++ = *ip;
		  }
	    }
	}
      *ap = 0;
      ++level;
      
      /* return number of fired operators */
      return( (op - operatorsToFire) - possibleHeuristicOpersWithNP );
    }
  else
    {
      ++level;
      instantiateOperators( &heuristicInsertOperator, &heuristicStillPossible );
      return( 0 );
    }
}


float
heuristicCost( register struct literal_s *sit )
{
  register float cost;
  register int i, number, *ap, bound;
  
  /* initialize costs with given situation */
  ap = newAtoms;
  bound = (initialized ? numberAtoms + 1 : MAXATOMS);
  for( i = 1; i < bound; ++i )
    {
      if( sit[i].lit == 1 )
	{
	  oldCosts[i].cost = 0;
	  *ap++ = i;
	}
      else
	oldCosts[i].cost = INT_MAX;
      oldCosts[i].level = oldCosts[i].cost;
    }
  memcpy( costs, oldCosts, (initialized ? numberAtoms + 1 : SIZE) * sizeof( struct cost_s ) );
  *ap = 0;
  
  /* compute heuristic */
  level = 0;
  number = 1;
  while( number != 0 )
    {
      number = nextLevel();
      if( !initialized )
	number = memcmp( oldCosts, costs, SIZE * sizeof( struct cost_s ) );
      memcpy( oldCosts, costs, (initialized ? numberAtoms + 1 : SIZE) * sizeof( struct cost_s ) );
    }
  
  cost = goalCost( costs );
  return( cost );
}


char *
operatorName( register int *parameters, register int action )
{
  int *ip;
  static char name[128];

  name[0] = '(';
  name[1] = '\0';
  strcat( name, actionName[action] );
  for( ip = &parameters[1]; *ip != -1; ++ip )
    {
      strcat( name, " " );
      strcat( name, objectName[*ip - 1] );
    }
  strcat( name, ")" );
  return( name );
}


char *
atomName( register int *parameters )
{
  int *ip;
  static char name[128];

  name[0] = '(';
  name[1] = '\0';
  strcat( name, predicateName[parameters[0] - 1] );
  for( ip = &parameters[1]; *ip != -1; ++ip )
    {
      strcat( name, " " );
      strcat( name, objectName[*ip - 1] );
    }
  strcat( name, ")" );
  return( name );
}


char *
buildName( register int *parameters )
{
  return( readAtomHash( parameters )->name );
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



float
goalCost( register struct cost_s *costs )
{
  register int cost, *ip;

  cost = 0;
  for( ip = goalAtoms; *ip != 0; ++ip )
    {
      if( costs[*ip].cost == INT_MAX )
        return( -1.0 );
      cost += costs[*ip].cost;
    }
  return( (float) cost );
}


void
identifyGoalParents( register int *atoms )
{
  register int *ip;

  if( atoms != NULL )
    for( ip = atoms; *ip != 0; ++ip )
      if( relevantAtoms[*ip] == 0 )
	{
	  ++numberRelevantAtoms;
	  relevantAtoms[*ip] = 1;
	  identifyGoalParents( parents[*ip] );
	}
}

