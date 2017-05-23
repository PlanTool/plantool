
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
 *  Abstract data type to represent a CSP.
 *
 *    n is the number of variables; their names are 1,...,n;
 *
 *    domain_size[i] is the size of the domain for variable i, i = 1,...,n;
 *              their names are 0,...,domain_size[i]-1.
 *
 *    m is the number of constraints; their names are 0,...,m-1.
 *    arity[c] is the number of variables in constraint c, c = 0,...,m-1.
 *    scheme[c][0..arity[c]-1] is the list of variables for constraint c
 *              in ascending order.
 *    relation[c][0..max_tuple-1] is an array of constraints where each
 *              constraint is represented as a Boolean array; a 0 means
 *              the tuple is not in the relation, a 1 means it is in the
 *              relation.  (Alternatively, relation[c] could be a function
 *              pointer, or the name of a relation, as long as, given
 *              arity[c], scheme[c], relation[c] and the current
 *              partial solution, we are able to return true or false).
 *
 *    n_cons[i] is the number of constraints which involve variable i.
 *    index[i][0..n_cons[i]-1] is the index of each constraint which
 *              involves variable i.  Thus, relation[index[i][0]] ...
 *              relation[index[i][n_cons[i]-1]] are the constraints
 *              which involve variable i.
 */

typedef struct { /* attributes of variables */
    int  type;
    int  state;
    int  number;
} VARA;

typedef struct { /* attributes of values */
    int  type;
    int  number;
} VALA;

typedef struct {
    int  n;
    int  time;
    int  n_visible;
    int  visible[10];  /* MAGIC number */
    int  *domain_size;
    VARA *variable_attribute;
    VALA **value_attribute;
    int  m;
    int  *arity;
    int  **scheme;
    int  *relation;
    int  checkable[25]; /* MAGIC number */
    int  *n_cons;
    int  **index;
} CSP_type;

/*
 *  Routines available for CSPs.
 */
CSP_type  *generate(int, int);          /* generate a CSP from an instance   */
int       get_var();
void	  set_variable_attributes();
void	  get_variable_attributes();
void	  set_constraint();
void	  construct_index();
void      solve();		/* solve the CSP using backtracking  */
int       check_constraint();
void      process_solution();
void      free_CSP();		/* free dynamically allocated memory */

/*
 *  Routines available for debugging CSPs.
 */
void      dump_CSP();
void      dump_solution();
void      verify_solution();

