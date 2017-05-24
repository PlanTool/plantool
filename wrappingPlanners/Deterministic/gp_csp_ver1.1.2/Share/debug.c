
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
 *  Routines for debugging purposes.
 *
 *  Output can be make more understandable by printing
 *  the name of the type, rather than just the value,
 *  in print_variable and print_value.
 *
 *  Public routines:
 *	void      dump_CSP();
 *	void      dump_solution();
 *	void      verify_solution();
 */

#include <stdio.h>
#include "global.h"
#include "csp.h"


static void
print_variable( C, i )
    CSP_type    *C;
    int         i;
{
	printf( "State: %2d, %d.%d",
		    C->variable_attribute[i].state,
		    C->variable_attribute[i].type,
		    C->variable_attribute[i].number );
}

static void
print_value( C, i, a )
    CSP_type    *C;
    int         i, a;
{
	printf( "%d.%d",
		    C->value_attribute[i][a].type,
		    C->value_attribute[i][a].number );
}

void
dump_CSP( C )
    CSP_type    *C;
{
    int  i, a;
    int  count_cons[25]; /* MAGIC number */

    printf( "time = %d, C->n = %d, C->m = %d\n", C->time, C->n, C->m );
    printf( "Number of variables: %d\n", C->n );
    printf( "Domains of variables:\n" );
    for( i = 1; i <= C->n; i++ ) {
        printf( "\tx_%d has %d elements, ", i, C->domain_size[i]); 
	print_variable( C, i );
	printf( "\n" );

        for( a = 0; a < C->domain_size[i]; a++ ) {
	    printf( "\t\t" );
	    print_value( C, i, a );
	    printf( "\n" );
        }
    }

    printf( "Constraints:\n" );
    for(a = 0; a < 25; a++) {
	count_cons[a] = 0;
    }
    for( i = 0; i < C->m; i++ ) {
	if( C->relation[i] >= 25 ) {
            printf("dump_CSP: array not large enough.\n" );
            exit( 6 );
	}
	count_cons[C->relation[i]]++;
    }
    for(a = 0; a < 25; a++) if( count_cons[a] ) {
	printf( "\tConstraint %d appears %d times\n", a, count_cons[a] );
    }
}

void
dump_solution( C, solution )
    CSP_type    *C;
    int		*solution;
{
    int		i;

    printf( "Solution:\n" );
    for( i = 1; i <= C->n; i++ ) {
	print_variable( C, i );
	printf( " = " );
	print_value( C, i, solution[i] );
	printf( "\n" );
    }
}

/*
 *  Determine whether the solution that was found satisfies
 *  the constraints.
 */
void
verify_solution( C, solution )
    CSP_type    *C;
    int         *solution;
{
    int         i;

    for( i = 1; i <= C->n; i++ ) {
        if( (solution[i] < 0) || (solution[i] >= C->domain_size[i]) ) {
            printf( "Solution does not verify (x_%d: domain element %d).\n",
                i, solution[i] );
	    dump_solution( C, solution );
            dump_CSP( C );
            exit( 6 );
        }
    }

    for( i = 0; i < C->m; i++ ) {
        if( !check_constraint( C, i, solution ) ) {
            printf( "Solution does not verify (constraint %d).\n", i );
	    dump_solution( C, solution );
            dump_CSP( C );
            exit( 6 );
        }
    }
}

