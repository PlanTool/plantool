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

/*
 * Additinal EBL to GAC-CBJ by Minh Binh Do
 */

#include "csp.h"
#include "stack.h"
#include "global.h"

#include "ebl.h"

/*
 * Three global variables to count properties discovered by the backtrack
 * routine during its execution: "checks" is the number of consistency checks
 * performed by the routine, "solution_count" is the number of solutions
 * found by the routine, and "visits" is the number of nodes of the search
 * tree visited by the routine.
 */
double  solution_count;
double  checks;
double  visits;

/*
 *  The meaning of checking is extended in GAC_CBJ, where checking[x][y] == i
 *  means that variable x was checking against variable y at level i,
 *  including forward checking and arc consistency checking.
 */
int     **checking;
int     **domains;
int     *count_uninst;
int     *domain_count;
int     *order;
BOOL    **conflicts;
BOOL    *instantiated;

/*
 * Structure to hold information for heuristics for variable ordering.
 */
static struct info {
    int index;
    int value;
} *var_order;


/* BM-EBL: Added vars for EBL */
int backtracks;
extern int cutoff_limit;
extern int ldc;
extern int auto_heu_select;
extern float maction_avg;
extern float mfact_avg;
extern int switch_vo;
/* BM: End */

/* BM: Added for ver1.1.2 */
void print_heu() {
  printf("             *****************************************************\n");
  printf("             *    MFact_Avg: %.2f, MAction_Avg: %.2f  ",
	 mfact_avg, maction_avg);
  if( ldc )
    printf(" Heu: LDC  *\n");
  else
    printf(" Heu: DLC  *\n");
  printf("             *******************************************************\n");
}


void heuristics_selection() {
  /* If too tight */
  if( (maction_avg > 100) || (mfact_avg > 10) ) {
    ldc = 1;
    print_heu();
    return;
  }

  /* If too loose (hanoi & gripper) */
  if( (mfact_avg < 2) && (mfact_avg > 0.5) ) {
    ldc = 1;
    print_heu();
    return;
  }

  /* Moderate case (close to boundaries) */
  if( mfact_avg > 5 )
    if( maction_avg > 50 ) {
      ldc = 1;
      print_heu();
      return;
    }

  /* Disputed case */
  if( mfact_avg < 0.5 ) {
    ldc = 0;
    print_heu();
    return;
  }

  if( 10*mfact_avg + maction_avg < 40 ) {
    ldc = 1;
    print_heu();
    return;
  }


  /* Very boundary case -- may need to readjust */
/*   if( (mfact_avg > 3.5) && (10*mfact_avg + maction_avg > 75) ) { */
/*     ldc = 1; */
/*     print_heu(); */
/*     return; */
/*   } */

  ldc = 0;
  print_heu();
}


/*
 * Added function by BM to switch variable ordering
 */
void switch_variable_ordering()
{
  switch_vo = 0;
  cutoff_limit = DEFAULT_CUTOFF_LIMIT;
  if( ldc == 1 )
    ldc = 0;
  else
    ldc = 1;
}

/*
 *  Dynamic variable ordering routine.
 *
 *  Choose the next variable to be instantiated by searching for the
 *  variable with the least number of values still available.
 *  Break ties by choosing the variable that participates in the most
 *  constraints (statically determined before problem-solving starts).
 *  Hidden variables always come last.
 */
static int
get_next_var( C, level )
    CSP_type    *C;
    int         level;
{
    int i, j, var, min_count, min_var, state_level;

    /*
     *  Record the number of constraints each variable
     *  participates in. Used to break ties.
     */
    if( level == 1 ) {
        for( i = 1; i <= C->n; i++ ) {
            var_order[i].value = 0;
        }
        for( i = 0; i < C->m; i++ ) {
            for( j = 0; j < C->arity[i]; j++ ) {
                var = C->scheme[i][j];
                var_order[var].value++;
            }
        }
    }

    /*
     *  All visible variables have been instantiated.
     *  Return first uninstantiated hidden variable.
     */
    if( level > C->n_visible ) {
        for( i = 1; i <= C->n; i++ ) if( !instantiated[i] ) {
            return( i );
        }
        return( 0 ); /* something has gone wrong */
    }
    /*
     *  There are uninstantiated visible variables.
     *  Find visible variable with min domain.
     */
    else {
      min_count = HUGE; /* larger than any possible domain size */
      min_var = 0;      /* guaranteed to be changed             */
      if( !ldc ) {      /* BM: Original variable ordering */
	for( i = 1; i <= C->n; i++ ) {
	  if( !instantiated[i] &&
	      C->visible[C->variable_attribute[i].type] ) {
	      
	    if( domain_count[i] < min_count ) {
	      min_count = domain_count[i];
	      min_var = i;
	    }
	    else
	      if( (domain_count[i] == min_count) &&
		  (var_order[i].value > var_order[min_var].value) ) {
		min_var = i;
	      }
	      else
		if( (domain_count[i] == min_count) &&
		    (var_order[i].value == var_order[min_var].value) &&
		    (C->variable_attribute[i].state >
		     C->variable_attribute[min_var].state) ) {
		  min_var = i;
		}
	  }
        }
      } else {    /* BM: New LDC ordering */
	state_level = -1;
        for( i = 1; i <= C->n; i++ ) {
	  if( !instantiated[i] &&
	      C->visible[C->variable_attribute[i].type] ) {

	    if( C->variable_attribute[i].state > state_level ) {
	      min_var = i;
	      state_level = C->variable_attribute[i].state;
	    }
	    else if( (C->variable_attribute[i].state == state_level) &&
		     (domain_count[i] < domain_count[min_var]) ) {
	      min_var = i;
	    }
	    else
	      if(  (C->variable_attribute[i].state == state_level) &&
		   (domain_count[i] == domain_count[min_var]) &&
		   (var_order[i].value > var_order[min_var].value) ) {
		min_var = i;
	      }
	  }
        }

      }

      return( min_var );
    }
}

/*
 * Function to restore the domain of variables which were eliminated due to the
 * instantiation of variable "i".  A variable is known to have had a value
 * deleted by looking at the "checking" matrix.
 */
static void
restore( C, current_level, current_variable )
    CSP_type    *C;
    int         current_level, current_variable;
{
    int         i, j, a;

  for( j = 1; j <= C->n; j++ ) 
    if( !instantiated[j] ) {
      if( checking[current_variable][j] == current_level ) {
         for( a = 0; a < C->domain_size[j]; a++ )
             if( domains[j][a] == current_level ) {
                domains[j][a] = 0;
                domain_count[j] ++;
                }
         for( i = 1; i <= current_level; i++ )
             if( checking[order[i]][j] == current_level )
                checking[order[i]][j] = 0;
         }
      }
}

/*
 *
 */
static void
update_constraint_counts( C, current_variable )
    CSP_type    *C;
    int         current_variable;
{
    int         i, r;

    for( i = 0; i < C->n_cons[current_variable]; i++ ) {
        r = C->index[current_variable][i];
        count_uninst[r]--;
    }
}


/*
 *
 */
static void
restore_constraint_counts( C, current_variable )
    CSP_type    *C;
    int         current_variable;
{
    int         i, r;

    for( i = 0; i < C->n_cons[current_variable]; i++ ) {
        r = C->index[current_variable][i];
        count_uninst[r]++;
    }
}


static void
union_conflicts( int x, int y )
{
  int i;
  for( i = 1; order[i] != x; i++ )
     conflicts[x][order[i]] =
         conflicts[x][order[i]] ||
         conflicts[y][order[i]];
}

static void
empty_conflicts( int from, int to )
{
  int i, j;
  for( i = from+1; i <= to; i++ )
      for( j = 1; j <= i; j++ )
          conflicts[order[i]][order[j]] = 0;
}

static void
union_checking( int x, int y )
{
  int i;
  for( i = 1; order[i] != x; i++ )
      conflicts[x][order[i]] =
          conflicts[x][order[i]] ||
          (checking[order[i]][y] != 0);
}

static void
record_a_fc_checking( CSP_type * C, int r, int x, int current_level)
{
  int i;
  for( i = 0; i < C->arity[r]; i++ )
      if( C->scheme[r][i] != x && checking[C->scheme[r][i]][x] == 0 )
         checking[C->scheme[r][i]][x] = current_level;
}

static void
record_an_ac_checking( CSP_type * C, int r, int x, int current_level)
{
  int i, j;
  for( i = 0; i < C->arity[r]; i++ ) {
      int a = C->scheme[r][i];
      if( a != x ) {
         if( instantiated[a] ) {
            if( checking[a][x] == 0 )
               checking[a][x] = current_level;
            }
         else {
            for( j = 1; j <= current_level; j++ )
                if( checking[order[j]][a] != 0 &&
                    checking[order[j]][x] == 0 )
                   checking[order[j]][x] = current_level;
            }
         }
      }
}

/*
 *  A constraint is forward checkable if one of its
 *  variables is uninstantiated.
 */
static int
forward_checkable( C, r )
    CSP_type    *C;
    int         r;
{
    return( count_uninst[r] == 1 );
}

/*
 *  A constraint is arc consistency checkable if two or
 *  more of its variables are uninstantiated. (If only
 *  one of the variables is uninstantiated it is not
 *  arc consistency checkable since the constraint has
 *  already been forward checked.)
 */
static int
arc_consistency_checkable( C, r )
    CSP_type    *C;
    int         r;
{
    return( 2 <= count_uninst[r] &&
		 count_uninst[r] <= C->checkable[C->relation[r]] );
}

/*
 *  Check a ``forward_checkable'' constraint to see which values in
 *  the domain of a future variable can be elminated due to the
 *  instantiations so far.
 */
static int
check_forward( C, r, current_level, current_variable, solution, fail )
    CSP_type    *C;
    int         r, current_level, current_variable;
    int         *solution;
    int         *fail;
{
    int i, a, future_variable;
    int delete_count = 0;

    /*
     *  Which is the uninstantiated variable?
     */
    for( i = 0; i < C->arity[r] &&
                instantiated[C->scheme[r][i]];
         i++ ) {
            ;
    }
    future_variable = C->scheme[r][i];

    for( a = 0; a < C->domain_size[future_variable]; a++ )
        if( domains[future_variable][a] == 0 ) {

            /* temporarily instantiate to check */
            solution[future_variable] = a;
            if( !check_constraint( C, r, solution ) ) {
                domains[future_variable][a] = current_level;
                domain_count[future_variable]--;
                delete_count++;
            }
        }

    if( delete_count ) {
        if( domain_count[future_variable] ) /* domain not empty */
            push( future_variable );
        record_a_fc_checking(C, r, future_variable, current_level);
    }

    *fail = future_variable;

    /* BM: Pushing the goal 
    if( domain_count[future_variable] == 0 )
      record_goal_related( future_variable, current_variable );
     BM: End */

    return( domain_count[future_variable] );
}

/*
 *  Given instantiations of some of the variables, does there exist
 *  values for the remaining variables in the scheme of the relation
 *  such that the constraint is satisfied?
 */
static int
exists( C, rel_index, solution )
    CSP_type    *C;
    int         rel_index;
    int         *solution;
{
    int         i, var, b;
    int         found;

    int         tuple[C->arity[rel_index]];
    int         min_domain[C->arity[rel_index]];

    /*
     *  Initialize the starting tuple.
     */
    for( i = 0; i < C->arity[rel_index]; i++ ) {
        var = C->scheme[rel_index][i];
        if( instantiated[var] )
            tuple[i] = solution[var];
        else {
            /* find first valid element */
            found = 0;
            for( b = 0; b < C->domain_size[var] && !found; b++ ) {
                if( domains[var][b] == 0 ) {
                    tuple[i] = min_domain[i] = b;
                    found = 1;
                    break;
                }
            }
            if( !found ) {
                return( 0 );
            }
        }
    }

    i = C->arity[rel_index]-1;
    while( i >= 0 ) {

        for( i = 0; i < C->arity[rel_index]; i++ ) {
            var = C->scheme[rel_index][i];
            solution[var] = tuple[i];
        }

        if( check_constraint( C, rel_index, solution ) ) {
            return( 1 );
        }

        /* get next tuple */
        for( i = C->arity[rel_index]-1; i >= 0; i-- ) {
            var = C->scheme[rel_index][i];
            if( !instantiated[var] ) {
                found = 0;
                var = C->scheme[rel_index][i];
                for( b = tuple[i]+1; b < C->domain_size[var]; b++ )
                    if( domains[var][b] == 0) {
                        found = 1;
                        tuple[i] = b;
                        break;
                    }
                if( found) break;
                tuple[i] = min_domain[i];
            }
        }
    }

    return( 0 );
}

/*
 *  The domain of variable v1 is to be filtered based on the given constraint.
 */
static int
revise( C, rel_index, v1, current_level, current_variable, solution, fail )
    CSP_type    *C;
    int         rel_index;
    int         v1, current_level, current_variable;
    int         *solution;
    int         *fail;
{
    int a, changed = 0, is_consistent = 0;

    /*
     *  For each element in the domain of v1, check if there
     *  exists supporting elements in the domains of the remaining
     *  variables in the scheme of the relation.
     */
    instantiated[v1] = 1;
    for( a = 0; a < C->domain_size[v1]; a++ )
        if( domains[v1][a] == 0 ) {
            solution[v1] = a;
            if( exists( C, rel_index, solution ) ) {
                is_consistent = 1;
            }
            else {
                changed = 1;
                domains[v1][a] = current_level;
                domain_count[v1]--;
            }
        }
    instantiated[v1] = 0;

    if( changed ) {
        push( v1 );
        record_an_ac_checking(C, rel_index, v1, current_level);
    }

    *fail = v1;
    return( is_consistent );
}

static int
consistent( C, current_level, current_variable, solution, fail )
    CSP_type    *C;
    int         current_level, current_variable;
    int         *solution;
    int         *fail;
{
  int         i, j, rel_index, v;

    /* BM-EBL: Added EBL checking for current variable */
    if( check_nogood(current_variable, 
		      current_level, solution) != 0 ) {
      *fail = 0;

      goto END;
    }
    /* BM: End */

    /*
     *  for each constraint in which current_variable participates...
     */
    for( j = 0; j < C->n_cons[current_variable]; j++ ) {
        rel_index = C->index[current_variable][j];
        if( forward_checkable( C, rel_index ) ) {
            if( !check_forward( C, rel_index,
                                current_level,
                                current_variable,
                                solution,
                                fail ) )
                goto END;
        }
    }

    push( current_variable );

    while( !stack_empty() ) {
        v = pop();

        /*
         *  Variable `v' has changed so need to revise any uninstantiated
         *  variables which share a constraint with `v'.
         *
         *  for each constraint in which the variable `v' participates...
         */
        for( j = 0; j < C->n_cons[v]; j++ ) {
            rel_index = C->index[v][j];
            if( arc_consistency_checkable( C, rel_index ) ) {

                /*
                 *  for each uninstantiated variable in the scheme...
                 */
                for( i = 0; i < C->arity[rel_index]; i++ ) {
                    if( C->scheme[rel_index][i] != v &&
                        !instantiated[C->scheme[rel_index][i]] ) {

                        if( !revise( C, rel_index,
                                     C->scheme[rel_index][i],
                                     current_level,
                                     current_variable,
                                     solution,
                                     fail ) )
                            goto END;

                    }
                }

            }
        }
    }
    return( 1 );

    END: while( !stack_empty() )
        v = pop();
    return( 0 );
}

/*
 *  Solve the constraint network by backtracking search interleaved
 *  with constraint propagation and conflict-based backjumping.
 */
static int
GAC_CBJ( C, solution, current_level )
    CSP_type    *C;
    int         current_level;
    int         *solution;
{
    int         i, a, current_variable;
    int         fail, jump;
    int         saved_solution_count = solution_count;

    /* BM: Added for backtrack cutoff */
    if( backtracks > cutoff_limit ) {
      printf("..... EXCESS PRESET CUTOFF_LIMIT. STOP SEARCHING !!!\n");
      if (switch_vo) {
	switch_variable_ordering();
	return 1;
      }
      return 0;
    }
    /* BM: End */

    if( current_level > C->n ) {
        solution_count++;
        process_solution( C, solution );

	/* BM: Added for memory */
	/* getchar(); */
	/* BM: End */

        return 0;
    }

    if( limit_exceeded() ) {
        return( 1 );
    }

    current_variable = get_next_var( C, current_level );
    order[current_level] = current_variable;

    update_constraint_counts( C, current_variable );

    for( a = 0; a < C->domain_size[current_variable]; a++ ) {
      /* BM-VO: ATTENTION: can plug in the value ordering here */
        if( domains[current_variable][a] == 0 ) {
            solution[current_variable] = a;
            instantiated[current_variable] = 1;
            visits++;

	    /* BM: Added for relevance-based EBL */
	    inc_nogood_relevant( current_variable, a );
	    /* BM: End */


            if( consistent( C, current_level, current_variable, solution, &fail ) ) {
               jump = GAC_CBJ( C, solution, current_level+1 );
               if( jump != current_level ) {
		 /* BM: Added for relevance-based EBL */
		 check_nogood_relevant( current_variable, solution[current_variable] );
		 /* BM: End */
					 
		 restore( C, current_level, current_variable);
		 instantiated[current_variable] = 0;
		 restore_constraint_counts(C, current_variable);

		 /* BM: Added for cutoff backtracks */
		 backtracks++;
		 /* BM: End */

		 return (jump);
	       }
	    }
            else {
	      /* BM-EBL: Added 'if' sentence for EBL  */
	      if( fail != 0 )
		union_checking(current_variable, fail);
	    }
	    /* BM: Added for relevance_based EBL */
	    check_nogood_relevant( current_variable, solution[current_variable] );
	    /* BM: End */

            restore( C, current_level, current_variable );
            instantiated[current_variable] = 0;
        }
    }

    union_checking(current_variable, current_variable);
    if( saved_solution_count == solution_count ) {
       jump = 0;
       for( i = current_level-1; i >= 1 && jump == 0; i-- )
           if( conflicts[current_variable][order[i]] )
              jump = i;
       }
    else {
       jump = current_level - 1;
    }

    if( jump >= 1 ) {
      /* BM: Record a nogood */
      record_nogood(C, current_variable, current_level, solution);
      /* BM: End */

      union_conflicts( order[jump], current_variable);
      empty_conflicts( jump, current_level );
    }

    restore_constraint_counts( C, current_variable );

    /* BM: Added for backtracks */
    backtracks++;
    /* BM: End */

    return jump;
}


/*
 *  Interface to backtracking routine.
 */
void
solve( C )
    CSP_type    *C;
{
    int         i, j;
    int         *solution;

    /*
     *  Stack for arc consistency.
     */
    create_stack( C->n+1 );

    solution = new_calloc( C->n+1, sizeof(int) );
    domains = (int **)new_calloc( C->n+1, sizeof(int *) );
    domain_count = (int *)new_calloc( C->n+1, sizeof(int));
    for( i = 1; i <= C->n; i++ ) {
        domains[i] = new_calloc( C->domain_size[i], sizeof(int) );
        for( j = 0; j < C->domain_size[i]; j++ )
            domains[i][j] = 0;
        domain_count[i] = C->domain_size[i];
    }

    checking = (int **)new_calloc( C->n+1, sizeof(int *) );
    for( i = 1; i <= C->n; i++ ) {
        checking[i] = (int *)new_calloc( C->n+1, sizeof(int) );
        for( j = 1; j <= C->n; j++ )
            checking[i][j] = 0;
    }

    conflicts = (BOOL **)new_calloc( C->n+1, sizeof(BOOL *));
    for( i = 1; i <= C->n; i++ ) {
         conflicts[i] = (BOOL *)new_calloc( C->n+1, sizeof(BOOL));
         for( j = 1; j <= C->n; j++ )
             conflicts[i][j] = 0;
    }

    instantiated = (BOOL *)new_calloc( C->n+1, sizeof(BOOL) );
    order = new_calloc( C->n+1, sizeof(int));
    count_uninst = new_calloc( C->m, sizeof(int) );
    for( i = 0; i < C->m; i++ ) {
        count_uninst[i] = C->arity[i];
    }
    var_order = (struct info *)new_calloc( C->n+1, sizeof(struct info) );

    checks = 0;
    solution_count = 0;
    visits = 0;

    /* BM: Added for cutoff backtrack */
    backtracks = 0;
    /* BM: End */

    /* BM: Added for automatic heuristics selection */
    if( auto_heu_select ) {
      heuristics_selection();
    }
    /* BM: End */ 

    GAC_CBJ( C, solution, 1 );

    destroy_stack();

    free( solution );
    for( i = 1; i <= C->n; i++ ) {
        free( domains[i] );
    }
    free( domains );
    for( i = 1; i <= C->n; i++ ) {
        free( checking[i] );
    }
    free( checking );
    for( i = 1; i <= C->n; i++ ) {
        free( conflicts[i] );
    }
    free( conflicts );

    free( instantiated );
    free( order );
    free( domain_count );
    free( count_uninst );
    free( var_order );
}

