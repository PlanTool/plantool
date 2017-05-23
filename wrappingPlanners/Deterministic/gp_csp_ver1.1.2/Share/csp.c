
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

#include "global.h"
#include "csp.h"

void
set_variable_attributes( C, v, type, state, number )
    CSP_type    *C;
    int         v, type, state, number;
{
    C->variable_attribute[v].type = type;
    C->variable_attribute[v].state = state;
    C->variable_attribute[v].number = number;
}

void
get_variable_attributes( C, v, type, state, number )
    CSP_type    *C;
    int         v, *type, *state, *number;
{
    *type   = C->variable_attribute[v].type;
    *state  = C->variable_attribute[v].state;
    *number = C->variable_attribute[v].number;
}

void
set_constraint( C, r, arity, type )
    CSP_type    *C;
    int         r, arity, type;
{
    static int	size_so_far;

    if( r == 0 ) {
	/*
	 *  Allocate initial space for the constraints.
	 */
	size_so_far = 4096;
	C->arity = new_calloc( 4096, sizeof(int) );
	C->scheme = (int **)new_calloc( 4096, sizeof(int *) );
	C->relation = new_calloc( 4096, sizeof(int) );
    }
    else
    if( r >= size_so_far ) {
	size_so_far += 4096;
	C->arity = (int *)realloc( C->arity, size_so_far*sizeof(int) );
	C->scheme = (int **)realloc( C->scheme, size_so_far*sizeof(int *) );
	C->relation = (int *)realloc( C->relation, size_so_far*sizeof(int) );
    }

    C->arity[r] = arity;
    C->scheme[r] = new_calloc( arity, sizeof(int) );
    C->relation[r] = type;
}

/*
 *  Construct an index for the constraints.
 */
void
construct_index( C )
    CSP_type    *C;
{
    int         a, i, var;

    /*
     *  Two passes through the schemes so that we only
     *  use exactly as much space as is needed for the index.
     */
    C->n_cons = new_calloc( C->n+1, sizeof(int) );
    for( a = 0; a < C->m; a++ ) {
	for( i = 0; i < C->arity[a]; i++ ) {
	    var = C->scheme[a][i];
	    C->n_cons[var]++;
	}
    }

    C->index = (int **)new_calloc( C->n+1, sizeof(int *) );
    for( i = 1; i <= C->n; i++ ) {
	C->index[i] = new_calloc( C->n_cons[i], sizeof(int) );
	C->n_cons[i] = 0;
    }

    for( a = 0; a < C->m; a++ ) {
	for( i = 0; i < C->arity[a]; i++ ) {
	    var = C->scheme[a][i];
	    C->index[var][C->n_cons[var]] = a;
	    C->n_cons[var]++;
	}
    }
}

void
free_CSP( C )
    CSP_type *C;
{
    int i;

    free( C->domain_size );
    free( C->variable_attribute );

    for( i = 1; i <= C->n; i++ ) {
        free( C->value_attribute[i] );
    }
    free( C->value_attribute );

    free( C->arity );

    for( i = 0; i < C->m; i++ ) {
        free( C->scheme[i] );
    }
    free( C->scheme );

    free( C->relation );
    free( C->n_cons );

    for( i = 1; i <= C->n; i++ ) {
        free( C->index[i] );
    }
    free( C->index );

    free( C );
}

int*
new_calloc( nelem, elsize )
    int  nelem, elsize;
{
    int  *p;
    void *calloc();

    p = (int *)calloc( (size_t)nelem, (size_t)elsize );
    if( p == NULL ) {
        printf( "Out of space.\n" );
        exit(6);
    }

    return( p );
}

