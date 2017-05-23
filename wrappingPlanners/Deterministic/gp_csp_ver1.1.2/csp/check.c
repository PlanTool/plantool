/*
 *  CPlan solves planning problems formulated as constraint satisfaction
 *  problems.
 *
 *  Copyright (C) 1999  Peter van Beek and Xinguang Chen
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation.  See the GNU General Public License
 *  for more details (see the file called Copying or contact the Free
 *  Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA).
 */

#include "gp_csp.h"
#include "../Share/global.h"
#include "../Share/csp.h"

#include "../graphplan.h"

vertex_t list_action[MAX_ACTION_NODE];

static int	check_activity_constraint();
static int	check_mutex_constraint();

static int      check_fact_mutex_constraint();

/*
 *  Check the given values against the constraint.
 */
int
check_constraint( C, r, solution )
    CSP_type    *C;
    int         r;
    int         *solution;
{
    checks += C->arity[r];

    switch( C->relation[r] ) {
    case ACTIVITY_CONSTRAINT:
      return( check_activity_constraint( C, r, solution ) );

    case MUTEX_CONSTRAINT:
      return( check_mutex_constraint( C, r, solution ) );

    case FACT_MUTEX_CONSTRAINT:
      return( check_fact_mutex_constraint( C, r, solution ) );

    default:
	printf("Error in check_constraint.\n");
	exit(6);
    }
}

static int
check_activity_constraint( C, r, solution )
    CSP_type    *C;
    int         r;
    int         *solution;
{
    int		i, j, k;
    int		a, b;

    i = C->scheme[r][0];  /* Fact in the higher level */
    j = C->scheme[r][1];  /* Fact in the lower level */

    a = solution[i];
    b = solution[j];

    /* Get the csp_val of the action assciate with this constraint */
    k = get_1st_value_from_hashtable(r);

    /* Check if the value of the higher level fact is in fact that action or not */
    if( (C->value_attribute[i][a].number == k) &&
	(C->value_attribute[j][b].type == NOTHING) )
      return 0;

    return 1;
}

static int
check_mutex_constraint( C, r, solution)
     CSP_type *C;
     int      r;
     int      *solution;
{
  int  i,j,k,l;
  int  a,b;

  i = C->scheme[r][0];  /* Fact 1 involve in the mutex */
  j = C->scheme[r][1];  /* Fact 2 involve in the mutex */

  a = solution[i];
  b = solution[j];

  k = C->value_attribute[i][a].number;
  l = C->value_attribute[j][b].number;

  if( (k > 0) && (l > 0) ) {
    if( are_mutex(list_action[k], list_action[l]) )
      return 0;
  }

  return 1;
}

static int
check_fact_mutex_constraint( C, r, solution )
     CSP_type   *C;
     int         r;
     int        *solution;
{
  int i,j;
  int a,b;

  i = C->scheme[r][0];
  j = C->scheme[r][1];

  a = solution[i];
  b = solution[j];

  if( (C->value_attribute[i][a].type != NOTHING) &&
      (C->value_attribute[j][b].type != NOTHING) )
    return 0;

  return 1;
}
