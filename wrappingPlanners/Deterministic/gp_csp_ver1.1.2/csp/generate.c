/**********************************************************************
    Function to generate the CSP formular from the GP Structure
***********************************************************************/

#include "gp_csp.h"
#include "../graphplan.h"
#include "../Share/global.h"
#include "../Share/csp.h"

#include <string.h>
/*
 *  Private routines.
 */
static void	set_domain();
static void	set_value_attributes();

/*
 * Global value for setting CSP values for fact variables. It will be
 * different from the value set in BB's routine
 */

int csp_value;

/*
 * Function to check if for two facts, whether of not exist a pair of actions supporting
 * them that are mutex.
 */
int
check_fact_mutex( vert1, vert2 )
     vertex_t vert1, vert2;
{
  edgelist_t edge1, edge2;

  for( edge1 = vert1->in_edges; edge1; edge1 = edge1->next ) {
    if( edge1->endpt->needed == 0 )
      continue;
    for( edge2 = vert2->in_edges; edge2; edge2 = edge2->next ) {
      if( edge2->endpt->needed == 0 )
	continue;

      if( are_mutex(edge1->endpt, edge2->endpt) )
	return 1;
    }
  }

  return 0;
}

/*
 *  Generate a CSP corresponding to a blocks world planning problem.
 */
CSP_type *
generate(  num_vars, time_step )
    int num_vars;
    int time_step;
{
    CSP_type    *C;
    int         t, v, i, j;
    int		next_con, activity_count = 0, mutex_count = 0, nvertex;

    vertex_t    vert, action, fact, v_store[MAXNODES];
    edgelist_t  edge, temp_edge;

    C = (CSP_type *) new_calloc( 1, sizeof(CSP_type) );
    C->time = time_step;
    C->n = num_vars;

    /*
     *  Allocate space for n variables.
     */
    C->domain_size = new_calloc( C->n+1, sizeof(int) );
    C->variable_attribute = (VARA *)new_calloc( C->n+1, sizeof(VARA) );
    C->value_attribute = (VALA **)new_calloc( C->n+1, sizeof(VALA *) );

    /*
     *  Record the attributes of each variable.
     *  Initialize the domain of each variable.
     */

    v = 1;
    for( t = time_step; t >= 1; t-- ) {
	/*
	 *  Try all variable at fact_table[t]
	 */

      get_next(fact_table[t],0);
      while((vert = get_next(fact_table[t], 1)) != NULL) {
	if(vert->needed == 0)
	  continue;

	/* Each fact var will have unique csp_val, which SHOULD be equal to the index
	 * number in CSP formulation.
	 */
	/*	printf("** v = %d  vert->csp_val = %d **\n", v, vert->csp_val);*/
	if( t == time_step) 
	  set_variable_attributes( C, v, BE_ACTIVE, t, vert->csp_val );
	else
	  set_variable_attributes( C, v, INACTIVE, t, vert->csp_val );
	set_domain( C, v , vert);
	v++;
      }
    }
    /*
     *  Is the variable type visible? (1) or hidden? (0)
     */
    C->visible[BE_ACTIVE] = 1;
    C->visible[INACTIVE] = 1;
    C->n_visible = num_vars;

    /*
     *  Record the arity, scheme, and relation of an
     *  ACTIVITY constraints. This constraint means that if a fact is commit to
     *  some action, then all pres of that action should have values different than
     *  NULL (NOTHING). Note that the number of variable for each
     *  constraint is varied dynamically.
     */
    next_con = 0;
    for( t = time_step; t > 1; t-- ) {
      get_next(fact_table[t],0);

      /*
       * vert: fact variable that we are considering
       * edge: vert->in_edge
       * action: edge->endpt, one action that support vert (vertex_t type)
       */
      while((vert = get_next(fact_table[t], 1)) != NULL) {
	if(vert->needed == 0)
	  continue;

	for(edge = vert->in_edges; edge; edge = edge->next) {
	  action = edge->endpt;
	  if( action->needed == 0)
	    continue;

	  for( temp_edge = action->in_edges; temp_edge; temp_edge = temp_edge->next ) {
	    if( temp_edge->endpt->needed == 0)
	      continue;

	    set_constraint( C, next_con, 2, ACTIVITY_CONSTRAINT );
	    C->scheme[next_con][0] = vert->csp_val;
	    C->scheme[next_con][1] = temp_edge->endpt->csp_val;

	    put_in_hashtable(next_con, action->csp_val);
	    activity_count++;
      	    next_con++;
	  }
	}
      }
    }


    /*
     * Constraint for mutex at the fact levels
     */
    for( t = time_step - 1; t >= 1; t--) {
      get_next(fact_table[t],0);

      while((fact = get_next(fact_table[t],1)) != NULL) {
	if(fact->needed == 0)
	  continue;
    
	/*
	 * fact: One fact that we are consider
	 * vert: Another fact that mutex with "fact"
	 */
 	for( edge = fact->exclusive; edge; edge = edge->next ) {
	  vert = edge->endpt;
	  if( (fact->csp_val > vert->csp_val) || (vert->needed == 0) )
	    continue;	  

          set_constraint( C, next_con, 2, FACT_MUTEX_CONSTRAINT );
          C->scheme[next_con][0] = fact->csp_val;
          C->scheme[next_con][1] = vert->csp_val;

	  mutex_count++;
          next_con++;
	}
      }
    }

    /*
     * Set up the constraints represent "abstract " MUTEX between actions. Two fact
     * will have constraint "MUTEX" between them if there exist a pair of actions
     * support them that are mutex.
     */
    for( t = time_step; t >= 1; t--) {
      nvertex = 0;
   
      /* For convenient, store all fact in one level in an array */
      get_next(fact_table[t],0);
      while((fact = get_next(fact_table[t],1)) != NULL) {
	if(fact->needed != 0)
	  v_store[nvertex++] = fact;
      }

      /* Check each pair of facts in the same level */
      for( i = 0; i < nvertex - 1; i++ )
	for( j = i+1; j < nvertex; j++ ) {
	  if(are_mutex( v_store[i], v_store[j] ) )
	     continue;
	  
	  if( check_fact_mutex( v_store[i], v_store[j] ) ) {
	    set_constraint( C, next_con, 2, MUTEX_CONSTRAINT );
	    C->scheme[next_con][0] = v_store[i]->csp_val;
	    C->scheme[next_con][1] = v_store[j]->csp_val;

	    mutex_count++;
	    next_con++;
	  }
	}
    }


    printf("### Total Constraints: %d . Mutex-con: %d. Act-con: %d  ######\n",
	   next_con, mutex_count, activity_count );
    C->m = next_con;

    /*
     *  When should the constraint be propagated?
     *
     *  C->checkable[r] = u means that constraint type r is propagated
     *          only when the number of uninstantiated variables in the
     *          scheme of the constraint is less than or equal to u.
     *          Constraint propagation then potentially reduces the
     *          domains of each of the uninstantiated variables.
     *  
     *  In particular,
     *    == 1    means only forward checking done on this constraint
     *    >= 2    means arc consistency checking done on this constraint
     *    == HUGE means constraint is always propagated
     */
    C->checkable[MUTEX_CONSTRAINT] = 1;
    C->checkable[ACTIVITY_CONSTRAINT] = 1;
    C->checkable[DELETE_CONSTRAINT] = HUGE;
    C->checkable[FACT_MUTEX_CONSTRAINT] = 1;

    /* Construct index for every variables */
    construct_index( C );

    /* Create the hash table for every constraints */
    construct_hash( activity_count );

    return( C );
}

static void
set_value_attributes( C, v, a, type, number )
    CSP_type    *C;
    int		type, number;
{
    C->value_attribute[v][a].type = type;
    C->value_attribute[v][a].number = number;
}

/*
 * Function to count in_edges of a fact node so we can know the domain size of 
 * that variable.
 */
int
get_domain_size(vert)
     vertex_t vert;
{
  edgelist_t edge;
  int count = 0;

  if((edge = vert->in_edges) == NULL)
    return 0;
  for(; edge; edge = edge->next)
    if( edge->endpt->needed != 0 )
      count++;

  return count;
}



/*
 *  Initialize the domain of a variable.
 *  Record the attributes of each value.
 */
static void
set_domain( C, v, vert )
    CSP_type    *C;
    int		v;
    vertex_t    vert;
{
    int		state, type, i;
    int		dsize, count = 0;
    edgelist_t  edge;

    get_variable_attributes( C, v, &type, &state, &i );

    /*
     * Set domain for goals
     */
    if( type == BE_ACTIVE ) {
      dsize = get_domain_size(vert);
      C->value_attribute[v] = (VALA *) new_calloc(dsize, sizeof(VALA));

      if( (edge = vert->in_edges) != NULL ) {
	count = 0;
	for(; edge; edge = edge->next) {
	  if(edge->endpt->needed == 0)
	    continue;

	  if( edge->endpt->is_noop )
	    set_value_attributes( C, v, count, A_NOOP, edge->endpt->csp_val);
	  else
	    set_value_attributes( C, v, count, IS_ACTION, edge->endpt->csp_val );
	  count++;
	}
      }

      C->domain_size[v] = count;
    }
	/*
	 * Set domain for variables in level lower than goal level
	 */
    else if( type == INACTIVE ) {
      dsize = get_domain_size(vert);
      C->value_attribute[v] = (VALA *) new_calloc(dsize+1, sizeof(VALA));
      set_value_attributes( C, v, 0, NOTHING, 0);

      if( (edge = vert->in_edges) != NULL ) {
	count = 1;

	for(; edge; edge = edge->next) {
	  if(edge->endpt->needed == 0)
	    continue;

	  if( edge->endpt->is_noop )
	    set_value_attributes( C, v, count, A_NOOP, edge->endpt->csp_val);
	  else
	    set_value_attributes( C, v, count, IS_ACTION, edge->endpt->csp_val);
	  count++;
	}
      }

      C->domain_size[v] = count;
    }
}
