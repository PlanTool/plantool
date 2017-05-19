
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

#include "csp.h"
#include "stack.h"
#include "global.h"

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

int     **domains;
int     *count_uninst;
int     *domain_count;
BOOL    **checking;
BOOL    *instantiated;

/*
 * Structure to hold information for heuristics for variable ordering.
 */
static struct info {
    int index;
    int value;
} *var_order;

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
    int i, j, var, min_count, min_var;

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
    int         j, a;

    for( j = 1; j <= C->n; j++ ) if (!instantiated[j]) {
        if( checking[current_variable][j] ) {
            checking[current_variable][j] = 0;
            for( a = 0; a < C->domain_size[j]; a++ )
                if( domains[j][a] == current_level ) {
                    domains[j][a] = 0;
                    domain_count[j]++;
                }
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
check_forward( C, r, current_level, current_variable, solution )
    CSP_type    *C;
    int         r, current_level, current_variable;
    int         *solution;
{
    int i, a, future_variable;
    int changed = 0;

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
            solution[future_variable] = a;
            if( !check_constraint( C, r, solution ) ) {
                changed = 1;
                domains[future_variable][a] = current_level;
                domain_count[future_variable]--;
            }
        }

    if( changed ) {
        push( future_variable );
        checking[current_variable][future_variable] = 1;
    }

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

        for( i = C->arity[rel_index]-1; i >= 0; i --) {
            var = C->scheme[rel_index][i];
            if( !instantiated[var] ) {
                found = 0;
                var = C->scheme[rel_index][i];
                for( b = tuple[i]+1; b < C->domain_size[var]; b ++)
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
revise( C, rel_index, v1, current_level, current_variable, solution )
    CSP_type    *C;
    int         rel_index;
    int         v1, current_level, current_variable;
    int         *solution;
{
    int a, changed = 0;

    /*
     *  For each element in the domain of v1, check if there
     *  exists supporting elements in the domains of the remaining
     *  variables in the scheme of the relation.
     */
    instantiated[v1] = 1;
    for( a = 0; a < C->domain_size[v1]; a++ )
        if( domains[v1][a] == 0 ) {
            solution[v1] = a;
            if( !exists( C, rel_index, solution ) ) {
                changed = 1;
                domains[v1][a] = current_level;
                domain_count[v1]--;
            }
        }
    instantiated[v1] = 0;

    if( changed ) {
        push( v1 );
        checking[current_variable][v1] = 1;
    }

    return( domain_count[v1] );
}

static int
consistent( C, current_level, current_variable, solution )
    CSP_type    *C;
    int         current_level, current_variable;
    int         *solution;
{
    int         i, j, rel_index, v;

    /*
     *  for each constraint in which current_variable participates...
     */
    for( j = 0; j < C->n_cons[current_variable]; j++ ) {
        rel_index = C->index[current_variable][j];
        if( forward_checkable( C, rel_index ) ) {
            if( !check_forward( C, rel_index,
                                current_level,
                                current_variable,
                                solution ) )
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
                                     solution ) )
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
 *  with constraint propagation and chronological backtracking.
 */
static int
GAC( C, solution, current_level )
    CSP_type    *C;
    int         current_level;
    int         *solution;
{
    int         a, current_variable;

    if( current_level > C->n ) {
        solution_count++;
        process_solution( C, solution );
        return( 1 );
    }

    if( limit_exceeded() ) {
        return( 1 );
    }

    current_variable = get_next_var( C, current_level );

    update_constraint_counts( C, current_variable );

    for( a = 0; a < C->domain_size[current_variable]; a++ ) {
        if( domains[current_variable][a] == 0 ) {

            solution[current_variable] = a;
            instantiated[current_variable] = 1;
            visits++;

            if( consistent( C, current_level, current_variable, solution ) ) {
                if( GAC( C, solution, current_level+1 ) ) {
                    return( 1 );
                }
            }

            restore( C, current_level, current_variable );
            instantiated[current_variable] = 0;
        }
    }

    restore_constraint_counts( C, current_variable );

    return( 0 );
}


/*
 *  Interface to backtracking routine.
 */
void
solve( C )
    CSP_type    *C;
{
    int         i;
    int         *solution;

    /*
     *  Stack for arc consistency.
     */
    create_stack( C->n+1 );

    solution = new_calloc( C->n+1, sizeof(int) );
    domains = (int **)new_calloc( C->n+1, sizeof(int *) );
    for( i = 1; i <= C->n; i++ ) {
        domains[i] = new_calloc( C->domain_size[i], sizeof(int) );
    }
    checking = (BOOL **)new_calloc( C->n+1, sizeof(BOOL *) );
    for( i = 1; i <= C->n; i++ ) {
        checking[i] = (BOOL *)new_calloc( C->n+1, sizeof(BOOL) );
    }
    instantiated = (BOOL *)new_calloc( C->n+1, sizeof(BOOL) );
    domain_count = new_calloc( C->n+1, sizeof(int) );
    for( i = 1; i <= C->n; i++ ) {
        domain_count[i] = C->domain_size[i];
    }
    count_uninst = new_calloc( C->m, sizeof(int) );
    for( i = 0; i < C->m; i++ ) {
        count_uninst[i] = C->arity[i];
    }
    var_order = (struct info *)new_calloc( C->n+1, sizeof(struct info) );

    checks = 0;
    solution_count = 0;
    visits = 0;
    GAC( C, solution, 1 );

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
    free( instantiated );
    free( domain_count );
    free( count_uninst );
    free( var_order );
}

