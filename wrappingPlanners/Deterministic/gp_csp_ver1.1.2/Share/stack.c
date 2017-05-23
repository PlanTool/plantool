
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
 *  Stack for arc consistency.
 */

#include "global.h"

static int	*stack;
static BOOL 	*on_stack;
static int	top;

void
create_stack( n )
    int		n;
{
    stack = new_calloc( n, sizeof(int) );
    on_stack = (BOOL *)new_calloc( n, sizeof(BOOL) );
    top = 0;
}

void
destroy_stack()
{
    free( stack );
    free( on_stack );
    top = 0;
}

int
stack_empty()
{
   return( top == 0 );
}

/*
 *  Push pushes a variable on the stack and marks it as being on the
 *  stack, unless the variable was already on the stack, in which case
 *  it is not added again.
 */
void
push( v )
    int		v;
{
    if( !on_stack[v] ) {
	top++;
	stack[top] = v;
	on_stack[v] = 1;
    }
}

int
pop()
{
    int		v;

    v = stack[top];
    on_stack[v] = 0;
    top--;

    return( v );
}

