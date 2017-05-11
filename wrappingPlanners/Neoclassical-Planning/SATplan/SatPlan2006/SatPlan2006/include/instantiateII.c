/*********************************************************************
 * (C) Copyright 1999 Albert Ludwigs University Freiburg
 *     Institute of Computer Science
 *
 * All rights reserved. Use of this software is permitted for 
 * non-commercial research purposes, and it may be copied only 
 * for that use.  All copies must include this copyright message.
 * This software is made available AS IS, and neither the authors
 * nor the  Albert Ludwigs University Freiburg make any warranty
 * about the software or its performance. 
 *********************************************************************/





/*********************************************************************
 * File: instantiateII.c
 * Description: functions for instantiating operators, second part.
 *
 *              - unify inertia in preconds with initial state
 *              - multiply remaining uninstantiated parameters
 *              - perform reachability analysis
 *              - collect relevant facts and perform final action cleanup
 *
 *
 * Author: Joerg Hoffmann 1999
 *
 *********************************************************************/ 






#include "bb.h"

#include "output.h"
#include "memory.h"

#include "instantiateI.h"
#include "instantiateII.h"

#include <stdlib.h>




extern const int UNSAT;



/* instantiation multiplying part: first unify inertia
 * preconds with initial (inertia) state, then multiply
 * remaining parameters; build a template for each legal
 * parameter combination.
 */









/* local globals for this part
 */

int linertia_preconds[MAX_VARS];
int lnum_inertia_preconds;

int lmultiply_parameters[MAX_VARS];
int lnum_multiply_parameters;

Operator *lo;
int lo_num;

Bool lused_constant[MAX_CONSTANTS];






void build_action_templates( void )

{

  int i, j, k, l, p;
  ActionTemplate *t;
  Operator *o;

  for ( i = 0; i < gnum_operators; i++ ) {
    lo = goperators[i];
    lo_num = i;

    for ( j = 0; j < gnum_constants; j++ ) {
      lused_constant[j] = FALSE;
    }

    lnum_inertia_preconds = 0;
    for ( j = 0; j < lo->num_preconds; j++ ) {
      if ( !gis_added[lo->preconds[j].predicate] &&
	   !gis_deleted[lo->preconds[j].predicate] ) {
	linertia_preconds[lnum_inertia_preconds++] = j;
      }
    }
      
    lnum_multiply_parameters = 0;
    for ( j = 0; j < lo->num_vars; j++ ) {
      for ( k = 0; k < lnum_inertia_preconds; k++ ) {
	p = lo->preconds[linertia_preconds[k]].predicate;
	for ( l = 0; l < garity[p]; l++ ) {
	  if ( lo->preconds[linertia_preconds[k]].args[l] ==
	       ENCODE_VAR( j ) ) {
	    break;
	  }
	}
	if ( l < garity[p] ) {
	  break;
	}
      }
      if ( k < lnum_inertia_preconds ) {
	continue;
      }
      lmultiply_parameters[lnum_multiply_parameters++] = j;
    }

    unify_inertia_preconds( 0 );
  }

  if ( gcmd_line.display_info == 107 ) {
    printf("\n\naction templates:");

    for ( i = 0; i < gnum_operators; i++ ) {
      printf("\n\noperator %s:", goperators[i]->name);
      for ( t = gtemplates; t; t = t->next ) {
	if ( t->op != i ) {
	  continue;
	}
	printf("\ninst: ");
	for ( j = 0; j < goperators[i]->num_vars; j++ ) {
	  if ( t->inst_table[j] < 0 ) {
	    printf("\nuninstantiated param in template! debug me, please\n\n");
	    exit( 1 );
	  }
	  printf("x%d = %s", j, gconstants[t->inst_table[j]]);
	  if ( j < goperators[i]->num_vars - 1 ) {
	    printf(", ");
	  }
	}
      }
    }
  }

  /* now remove inertia preconditions from operator schemata
   */
  for ( i = 0; i < gnum_operators; i++ ) {
    o = goperators[i];

    j = 0;
    while ( j < o->num_preconds ) {
      if ( !gis_added[o->preconds[j].predicate] &&
	   !gis_deleted[o->preconds[j].predicate] ) {
	for ( k = j; k < o->num_preconds - 1; k++ ) { 
 	  o->preconds[k].predicate = o->preconds[k+1].predicate;
	  for ( l = 0; l < garity[o->preconds[k].predicate]; l++ ) {
	    o->preconds[k].args[l] = o->preconds[k+1].args[l];
	  }
	}
	o->num_preconds--;
      } else {
	j++;
      }
    }
  }   

  if ( gcmd_line.display_info == 108 ) {
    printf("\n\ninertia free operators are:");
    for ( i = 0; i < gnum_operators; i++ ) {
      print_Operator( goperators[i] );
    }
    printf("\n\n");
  }

}



void unify_inertia_preconds( int curr_inertia )

{

  int p, i, j, k, af;
  int args[MAX_VARS];
  int affected_params[MAX_VARS];
  int num_affected_params = 0;

  if ( curr_inertia == lnum_inertia_preconds ) {
    multiply_parameters( 0 );
    return;
  }

  /* might be possible to implement this more effective by
   * collecting the arg vectors for each inertia as a preprocess
   * and setting, when instantiating a parameter, all the occurences
   * in the following inertia arg vectors to the appropriate constant
   * value.
   *
   * affected parameters are also the same on each call for a single inertia,
   * so this is unnecessary recomputing.
   */
  p = lo->preconds[linertia_preconds[curr_inertia]].predicate;
  for ( i = 0; i < garity[p]; i++ ) {
    args[i] = lo->preconds[linertia_preconds[curr_inertia]].args[i];
    if ( args[i] < 0 &&
	 lo->inst_table[DECODE_VAR( args[i] )] != -1 ) {
      args[i] = lo->inst_table[DECODE_VAR( args[i] )];
    }
    if ( args[i] < 0 ) {
      affected_params[num_affected_params++] = DECODE_VAR( args[i] );
    }
  }

  for ( i = 0; i < gnum_inertia; i++ ) {
    if ( ginertia[i].predicate != p ) {
      continue;
    }

    af = 0;
    for ( j = 0; j < garity[p]; j++ ) {
      if ( args[j] >= 0 ) {
	if ( args[j] != ginertia[i].args[j] ) {
	  break;
	} else {
	  continue;
	}
      }
      /* see if we have that constant already in instantiation;
       * if so, skip this inertia: op params are assumed different!
       */
      if ( 0 && lused_constant[ginertia[i].args[j]] ) {
	break;
      }
      /* check whether that constant has the correct type for that
       * parameter
       */
      if ( !gis_member[ginertia[i].args[j]][lo->var_types[affected_params[af]]] ) {
	break;
      }
      /* legal constant; set op parameter instantiation to it
       */
      lo->inst_table[affected_params[af++]] = ginertia[i].args[j];
      lused_constant[ginertia[i].args[j]] = TRUE;
    }
    if ( j < garity[p] ) {
      for ( k = 0; k < af; k++ ) {
	lused_constant[lo->inst_table[affected_params[k]]] = FALSE;
      }
      continue;
    }

    unify_inertia_preconds( curr_inertia + 1 );

    for ( j = 0; j < num_affected_params; j++ ) {
      lused_constant[lo->inst_table[affected_params[j]]] = FALSE;
    }
  }

  for ( i = 0; i < num_affected_params; i++ ) {
    lo->inst_table[affected_params[i]] = -1;
  }

}



void multiply_parameters( int curr_parameter )

{

  ActionTemplate *tmp;
  int i, j, t, n;

  if ( curr_parameter == lnum_multiply_parameters ) {
    tmp = new_ActionTemplate( lo_num );
    for ( i = 0; i < lo->num_vars; i++ ) {
      tmp->inst_table[i] = lo->inst_table[i];
    }
    tmp->next = gtemplates;
    gtemplates = tmp;
    gnum_templates++;
    return;
  }

  if ( curr_parameter == lnum_multiply_parameters - 1 ) {
    t = lo->var_types[lmultiply_parameters[curr_parameter]];
    n = gtype_size[t];
    for ( i = 0; i < n; i++ ) {
      if ( 0 && lused_constant[gtype_consts[t][i]] ) {
	continue;
      }
      lo->inst_table[lmultiply_parameters[curr_parameter]] = gtype_consts[t][i];

      tmp = new_ActionTemplate( lo_num );
      for ( j = 0; j < lo->num_vars; j++ ) {
	tmp->inst_table[j] = lo->inst_table[j];
      }
      tmp->next = gtemplates;
      gtemplates = tmp;
      gnum_templates++;
    }

    lo->inst_table[lmultiply_parameters[curr_parameter]] = -1;

    return;
  }

  t = lo->var_types[lmultiply_parameters[curr_parameter]];
  n = gtype_size[t];
  for ( i = 0; i < n; i++ ) {
    if ( 0 && lused_constant[gtype_consts[t][i]] ) {
      continue;
    }
    lo->inst_table[lmultiply_parameters[curr_parameter]] = gtype_consts[t][i];
    lused_constant[gtype_consts[t][i]] = TRUE;

    multiply_parameters( curr_parameter + 1 );

    lused_constant[gtype_consts[t][i]] = FALSE;   
  }

  lo->inst_table[lmultiply_parameters[curr_parameter]] = -1;

}










/* fixpoint computation for finding out which facts can be made true
 */









/* local globals for this part
 */

int_pointer lpos[MAX_PREDICATES];
int_pointer lneg[MAX_PREDICATES];
int_pointer luse[MAX_PREDICATES];
int_pointer lindex[MAX_PREDICATES];

int lp;
int largs[MAX_VARS];








void perform_reachability_analysis( void )

{

  int size, i, j, adr;
  Bool fixpoint;
  ActionTemplate *t1, *t2, *t3;
  Operator *o;
  Action *tmp, *a;

  for ( i = 0; i < gnum_predicates; i++ ) {
    size =  1;
    for ( j = 0; j < garity[i]; j++ ) {
      size *= gnum_constants;
    }

    lpos[i] = ( int_pointer ) calloc( size, sizeof( int ) );
    CHECK_PTR( lpos[i] );
    lneg[i] = ( int_pointer ) calloc( size, sizeof( int ) );
    CHECK_PTR( lneg[i] );
    luse[i] = ( int_pointer ) calloc( size, sizeof( int ) );
    CHECK_PTR( luse[i] );
    lindex[i] = ( int_pointer ) calloc( size, sizeof( int ) );
    CHECK_PTR( lindex[i] );

    for ( j = 0; j < size; j++ ) {
      lpos[i][j] = 0;
      lneg[i][j] = 0;
      luse[i][j] = 0;
      lindex[i][j] = -1;
    }
  }

  /* mark initial facts as possibly positive
   */
  for ( i = 0; i < gnum_initial; i++ ) {
    lp = ginitial[i].predicate;
    for ( j = 0; j < garity[lp]; j++ ) {
      largs[j] = ginitial[i].args[j];
    }
    lpos[lp][fact_adress()] = 1;
  }

  /* compute fixpoint
   */
  fixpoint = FALSE;
  while ( !fixpoint ) {
    fixpoint = TRUE;

    t1 = gtemplates;
    while ( t1 ) {
      o = goperators[t1->op];
      for ( i = 0; i < o->num_preconds; i++ ) {
	lp = o->preconds[i].predicate;
	for ( j = 0; j < garity[lp]; j++ ) {
	  largs[j] = GET_CONSTANT( o->preconds[i].args[j], t1 );
	}
	if ( !lpos[lp][fact_adress()] ) {
	  break;
	}
      }

      if ( i < o->num_preconds ) {
	break;
      }

      for ( i = 0; i < o->num_adds; i++ ) {
	lp = o->adds[i].predicate;
	for ( j = 0; j < garity[lp]; j++ ) {
	  largs[j] = GET_CONSTANT( o->adds[i].args[j], t1 );
	}
	adr = fact_adress();
	if ( !lpos[lp][adr] ) {
	  /* new relevant fact! (added non initial)
	   */
	  lpos[lp][adr] = 1;
	  lneg[lp][adr] = 1;
	  luse[lp][adr] = 1;
	  if ( gnum_relevant_facts == MAX_RELEVANT_FACTS ) {
	    printf("\ntoo many relevant facts! increase MAX_RELEVANT_FACTS (currently %d)\n\n",
		   MAX_RELEVANT_FACTS);
	    exit( 1 );
	  }
	  grelevant_facts[gnum_relevant_facts].predicate = lp;
	  for ( j = 0; j < garity[lp]; j++ ) {
	    grelevant_facts[gnum_relevant_facts].args[j] = largs[j];
	  }
	  lindex[lp][adr] = gnum_relevant_facts;
	  gnum_relevant_facts++;
	  fixpoint = FALSE;
	}
      }

      tmp = new_Action( t1->op );
      for ( i = 0; i < o->num_vars; i++ ) {
	tmp->inst_table[i] = t1->inst_table[i];
      }
      tmp->next = gactions;
      gactions = tmp;
      gnum_actions++;

      t2 = t1;
      t1 = t1->next;
      free_single_ActionTemplate( t2 );
    }
    gtemplates = t1;
    t3 = t1;
    if ( t1 ) t1 = t1->next;
    while ( t1 ) {
      o = goperators[t1->op];
      for ( i = 0; i < o->num_preconds; i++ ) {
	lp = o->preconds[i].predicate;
	for ( j = 0; j < garity[lp]; j++ ) {
	  largs[j] = GET_CONSTANT( o->preconds[i].args[j], t1 );
	}
	if ( !lpos[lp][fact_adress()] ) {
	  break;
	}
      }

      if ( i == o->num_preconds ) {
	for ( i = 0; i < o->num_adds; i++ ) {
	  lp = o->adds[i].predicate;
	  for ( j = 0; j < garity[lp]; j++ ) {
	    largs[j] = GET_CONSTANT( o->adds[i].args[j], t1 );
	  }
	  adr = fact_adress();
	  if ( !lpos[lp][adr] ) {
	    /* new relevant fact! (added non initial)
	     */
	    lpos[lp][adr] = 1;
	    lneg[lp][adr] = 1;
	    luse[lp][adr] = 1;
	    if ( gnum_relevant_facts == MAX_RELEVANT_FACTS ) {
	      printf("\ntoo many relevant facts! increase MAX_RELEVANT_FACTS (currently %d)\n\n",
		     MAX_RELEVANT_FACTS);
	      exit( 1 );
	    }
	    grelevant_facts[gnum_relevant_facts].predicate = lp;
	    for ( j = 0; j < garity[lp]; j++ ) {
	      grelevant_facts[gnum_relevant_facts].args[j] = largs[j];
	    }
	    lindex[lp][adr] = gnum_relevant_facts;
	    gnum_relevant_facts++;
	    fixpoint = FALSE;
	  }
	}

	tmp = new_Action( t1->op );
	for ( i = 0; i < o->num_vars; i++ ) {
	  tmp->inst_table[i] = t1->inst_table[i];
	}
	tmp->next = gactions;
	gactions = tmp;
	gnum_actions++;
	
	t3->next = t1->next;
	t2 = t1;
	t1 = t1->next;
	free_single_ActionTemplate( t2 );
      } else {
	t3 = t3->next;
	t1 = t1->next;
      }
    }
  }

  gnum_pp_facts = gnum_initial + gnum_relevant_facts;

  if ( gcmd_line.display_info == 109 ) {
    printf("\nreachability analysys came up with:");

    printf("\n\npossibly positive facts:");
    for ( i = 0; i < gnum_initial; i++ ) {
      printf("\n");
      print_Fact( &(ginitial[i]) );
    }      
    for ( i = 0; i < gnum_relevant_facts; i++ ) {
      printf("\n");
      print_Fact( &(grelevant_facts[i]) );
    }

    printf("\n\nthis yields these %d action templates:", gnum_actions);
    for ( i = 0; i < gnum_operators; i++ ) {
      printf("\n\noperator %s:", goperators[i]->name);
      for ( a = gactions; a; a = a->next ) {
	if ( a->op != i ) {
	  continue;
	}
	printf("\ntemplate: ");
	for ( j = 0; j < goperators[i]->num_vars; j++ ) {
	  printf("%s", gconstants[a->inst_table[j]]);
	  if ( j < goperators[i]->num_vars-1 ) {
	    printf(" ");
	  }
	}
      }
    }
    printf("\n\n");
  }

  /* if a goal is not possibly positive, the problem
   * is unsolvable.
   */
  for ( i = 0; i < gnum_goal; i++ ) {
    lp = ggoal[i].predicate;
    for ( j = 0; j < garity[lp]; j++ ) {
      largs[j] = ggoal[i].args[j];
    }
    if ( !lpos[lp][fact_adress()] ) {
      printf("\nproblem is unsolvable! goals can't be reached\n\n");
      output_planner_info();
      exit(UNSAT);
    }
  }
  

}



int fact_adress( void )

{

  int r = 0, b = 1, i;

  for ( i = garity[lp] - 1; i > -1; i-- ) {
    r += b * largs[i];
    b *= gnum_constants;
  }

  return r;

}










/* final sweep over domain representation, determining all
 * relevant facts and removing others
 */











void collect_relevant_facts( void )

{

  Action *a;
  Operator *o;
  int i, j, adr;


  /* mark all deleted facts; such facts, that are also pos, are relevant.
   */
  for ( a = gactions; a; a = a->next ) {
    o = goperators[a->op];

    for ( i = 0; i < o->num_dels; i++ ) {
      lp = o->dels[i].predicate;
      for ( j = 0; j < garity[lp]; j++ ) {
	largs[j] = GET_CONSTANT( o->dels[i].args[j], a );
      }
      adr = fact_adress();

      lneg[lp][adr] = 1;
      if ( lpos[lp][adr] &&
	   !luse[lp][adr] ) {
	luse[lp][adr] = 1;
	lindex[lp][adr] = gnum_relevant_facts;
	if ( gnum_relevant_facts == MAX_RELEVANT_FACTS ) {
	  printf("\nincrease MAX_RELEVANT_FACTS! (current value: %d)\n\n",
		 MAX_RELEVANT_FACTS);
	  exit( 1 );
	}
	grelevant_facts[gnum_relevant_facts].predicate = lp;
	for ( j = 0; j < garity[lp]; j++ ) {
	  grelevant_facts[gnum_relevant_facts].args[j] = largs[j];
	}
	lindex[lp][adr] = gnum_relevant_facts;
	gnum_relevant_facts++;
      }
    }
  }
      
  /* now build final action instances
   */
  for ( a = gactions; a; a = a->next ) {
    o = goperators[a->op];

    for ( i = 0; i < o->num_preconds; i++ ) {
      lp = o->preconds[i].predicate;
      for ( j = 0; j < garity[lp]; j++ ) {
	largs[j] = GET_CONSTANT( o->preconds[i].args[j], a );
      }
      adr = fact_adress();

      if ( !lneg[lp][adr] ) {
	continue;
      }

      a->preconds[a->num_preconds++] = lindex[lp][adr];
    }

    for ( i = 0; i < o->num_adds; i++ ) {
      lp = o->adds[i].predicate;
      for ( j = 0; j < garity[lp]; j++ ) {
	largs[j] = GET_CONSTANT( o->adds[i].args[j], a );
      }
      adr = fact_adress();

      if ( !lneg[lp][adr] ) {
	continue;
      }

      a->adds[a->num_adds++] = lindex[lp][adr];
    }

    for ( i = 0; i < o->num_dels; i++ ) {
      lp = o->dels[i].predicate;
      for ( j = 0; j < garity[lp]; j++ ) {
	largs[j] = GET_CONSTANT( o->dels[i].args[j], a );
      }
      adr = fact_adress();

      if ( !lpos[lp][adr] ) {
	continue;
      }

      a->dels[a->num_dels++] = lindex[lp][adr];
    }
  }

  /* build final initial and goal representation
   */
  ginitial_state.num_F = 0;
  for ( i = 0; i < gnum_initial; i++ ) {
    lp = ginitial[i].predicate;
    for ( j = 0; j < garity[lp]; j++ ) {
      largs[j] = ginitial[i].args[j];
    }
    adr = fact_adress();

    if ( !lneg[lp][adr] ) {
      continue;
    }

    ginitial_state.F[ginitial_state.num_F++] = lindex[lp][adr];
  }

  ggoal_state.num_F = 0;
  for ( i = 0; i < gnum_goal; i++ ) {
    lp = ggoal[i].predicate;
    for ( j = 0; j < garity[lp]; j++ ) {
      largs[j] = ggoal[i].args[j];
    }
    adr = fact_adress();

    if ( !lneg[lp][adr] ) {
      continue;
    }

    ggoal_state.F[ggoal_state.num_F++] = lindex[lp][adr];
  }

  if ( gcmd_line.display_info == 110 ) {
    printf("\n\nfinal domain representation:");

    printf("\n\nall actions:");
    for ( a = gactions; a; a = a->next ) {
      print_Action( a );
    }

    printf("\n\ninitial_state:");
    for ( i = 0; i < ginitial_state.num_F; i++ ) {
      printf("\n");
      print_ft_name( ginitial_state.F[i] );
    }

    printf("\n\ngoal_state:");
    for ( i = 0; i < ggoal_state.num_F; i++ ) {
      printf("\n");
      print_ft_name( ggoal_state.F[i] );
    }
  }
  
}










/* connect all add, del and pre info to connectivity graph
 */











void build_connectivity_graph( void )

{

  int i, j, n;
  Action *a;

  struct timeb tp;

  ftime( &tp );
  srandom( tp.millitm );

  gnum_ft_conn = gnum_relevant_facts;
  gnum_op_conn = gnum_actions + gnum_relevant_facts;/* include no-ops */
  gft_conn = ( FtConn * ) calloc( gnum_ft_conn, sizeof( FtConn ) );
  CHECK_PTR( gft_conn );
  gop_conn = ( OpConn * ) calloc( gnum_op_conn, sizeof( OpConn ) );
  CHECK_PTR( gop_conn );

  for ( i = 0; i < gnum_ft_conn; i++ ) {
    gft_conn[i].num_P = 0;
    gft_conn[i].num_A = 0;
    gft_conn[i].num_D = 0;

    gft_conn[i].rand = random() % STATE_HASH_SIZE;

    gft_conn[i].is_goal = FALSE;
  }
  for ( i = 0; i < ggoal_state.num_F; i++ ) {
    gft_conn[ggoal_state.F[i]].is_goal = TRUE;
  }

  for ( i = 0; i < gnum_op_conn; i++ ) {
    if ( i < gnum_actions ) {
      gop_conn[i].noop_for = -1;
      gop_conn[i].num_P = 0;
      gop_conn[i].num_A = 0;
      gop_conn[i].num_D = 0;
    } else {
      gop_conn[i].op = -1;
      gop_conn[i].noop_for = i - gnum_actions;
      gop_conn[i].num_P = 1;
      gop_conn[i].P = ( int * ) calloc( 1, sizeof( int ) );
      CHECK_PTR( gop_conn[i].P );
      gop_conn[i].P[0] =  i - gnum_actions;
      gft_conn[i - gnum_actions].num_P++;
      gop_conn[i].num_A = 1;
      gop_conn[i].A = ( int * ) calloc( 1, sizeof( int ) );
      CHECK_PTR( gop_conn[i].A );
      gop_conn[i].A[0] =  i - gnum_actions;
      gft_conn[i - gnum_actions].num_A++;
      gop_conn[i].num_D = 0;
    }
  }

  n = 0;
  for ( a = gactions; a; a = a->next ) {

    /* ops are simply copied over. a waste of time and memory,
     * but, usually there are not so many actions after
     * the fixpoint. can speed that up for optimization, but
     * keep it the clean and tidy way for the time being.
     */
    gop_conn[n].op = a->op;
    for ( i = 0; i < goperators[a->op]->num_vars; i++ ) {
      gop_conn[n].inst_table[i] = a->inst_table[i];
    }

    gop_conn[n].P = ( int * ) calloc( a->num_preconds, sizeof( int ) );
    CHECK_PTR( gop_conn[n].P );
    for ( i = 0; i < a->num_preconds; i++ ) {
      gop_conn[n].P[i] = a->preconds[i];
    }
    gop_conn[n].num_P = a->num_preconds;

    gop_conn[n].A = ( int * ) calloc( a->num_adds, sizeof( int ) );
    CHECK_PTR( gop_conn[n].A );
    for ( i = 0; i < a->num_adds; i++ ) {
      gop_conn[n].A[i] = a->adds[i];
    }
    gop_conn[n].num_A = a->num_adds;

    gop_conn[n].D = ( int * ) calloc( a->num_dels, sizeof( int ) );
    CHECK_PTR( gop_conn[n].D );
    for ( i = 0; i < a->num_dels; i++ ) {
      gop_conn[n].D[i] = a->dels[i];
    }
    gop_conn[n].num_D = a->num_dels;


    /* first sweep: only count the space we need for the fact arrays !
     */
    for ( i = 0; i < a->num_preconds; i++ ) {
      gft_conn[a->preconds[i]].num_P++;
    }
    for ( i = 0; i < a->num_adds; i++ ) {
      gft_conn[a->adds[i]].num_A++;
    }
    for ( i = 0; i < a->num_dels; i++ ) {
      gft_conn[a->dels[i]].num_D++;
    }

    n++;
  }

  for ( i = 0; i < gnum_ft_conn; i++ ) {
    gft_conn[i].P = ( int * ) calloc( gft_conn[i].num_P, sizeof( int ) );
    CHECK_PTR( gft_conn[i].P );
    gft_conn[i].num_P = 0;
    gft_conn[i].A = ( int * ) calloc( gft_conn[i].num_A, sizeof( int ) );
    CHECK_PTR( gft_conn[i].A );
    gft_conn[i].num_A = 0;
    gft_conn[i].D = ( int * ) calloc( gft_conn[i].num_D, sizeof( int ) );
    CHECK_PTR( gft_conn[i].D );
    gft_conn[i].num_D = 0;
  }

  n = 0;
  for ( a = gactions; a; a = a->next ) {
    /* second sweep: now put the op numbers into the arrays
     */
    for ( i = 0; i < a->num_preconds; i++ ) {
      gft_conn[a->preconds[i]].P[gft_conn[a->preconds[i]].num_P++] = n;
    }
    for ( i = 0; i < a->num_adds; i++ ) {
      gft_conn[a->adds[i]].A[gft_conn[a->adds[i]].num_A++] = n;
    }
    for ( i = 0; i < a->num_dels; i++ ) {
      gft_conn[a->dels[i]].D[gft_conn[a->dels[i]].num_D++] = n;
    }
    n++;
  }
  for ( ; n < gnum_op_conn; n++ ) {
    gft_conn[n - gnum_actions].P[gft_conn[n - gnum_actions].num_P++] = n;
    gft_conn[n - gnum_actions].A[gft_conn[n - gnum_actions].num_A++] = n;
  }
  


  if ( gcmd_line.display_info == 111 ) {
    printf("\n\ncreated connectivity graph as follows:");

    printf("\n\nOP ARRAY:");
    for ( i = 0; i < gnum_op_conn; i++ ) {
      printf("\n\nOP: ");
      print_op_name( i );
      printf("\n----------PRES:");
      for ( j = 0; j < gop_conn[i].num_P; j++ ) {
	printf("\n");
	print_ft_name( gop_conn[i].P[j] );
      }
      printf("\n----------ADDS:");
      for ( j = 0; j < gop_conn[i].num_A; j++ ) {
	printf("\n");
	print_ft_name( gop_conn[i].A[j] );
      }
      printf("\n----------DELS:");
      for ( j = 0; j < gop_conn[i].num_D; j++ ) {
	printf("\n");
	print_ft_name( gop_conn[i].D[j] );
      }
    }
    
    printf("\n\nFT ARRAY:");
    for ( i = 0; i < gnum_ft_conn; i++ ) {
      printf("\n\nFT: ");
      print_ft_name( i );
      printf("\n----------PRE OF:");
      for ( j = 0; j < gft_conn[i].num_P; j++ ) {
	printf("\n");
	print_op_name( gft_conn[i].P[j] );
      }
      printf("\n----------ADD BY:");
      for ( j = 0; j < gft_conn[i].num_A; j++ ) {
	printf("\n");
	print_op_name( gft_conn[i].A[j] );
      }
      printf("\n----------DEL BY:");
      for ( j = 0; j < gft_conn[i].num_D; j++ ) {
	printf("\n");
	print_op_name( gft_conn[i].D[j] );
      }
    }
  }
    
}













/* bit vectors for fast parallel lookup where needed.
 */














void insert_bit_vectors( void )

{

  int i, j, ft;

  gcword_size = sizeof( int ) * 8;

  for ( i = 0; i < gnum_ft_conn; i++ ) {
    gft_conn[i].uid_block = ( int ) i / gcword_size;
    gft_conn[i].uid_mask = 1 << ( i % gcword_size );  

    gft_conn[i].first_appearance = -1;
  }
  gnum_ft_bit = gft_conn[i-1].uid_block + 1;

  /* create goal bits
   */
  gbit_goal_state = new_BitVector( gnum_ft_bit );
  for ( i = 0; i < ggoal_state.num_F; i++ ) {
    ft = ggoal_state.F[i];
    gbit_goal_state[gft_conn[ft].uid_block] |= gft_conn[ft].uid_mask;
  }
  
  /* do op prec, add, del
   */
  for ( i = 0; i < gnum_op_conn; i++ ) {
    gop_conn[i].first_appearance = -1;
    gop_conn[i].bit_P = new_BitVector( gnum_ft_bit );
    gop_conn[i].bit_A = new_BitVector( gnum_ft_bit );
    gop_conn[i].bit_D = new_BitVector( gnum_ft_bit );
    for ( j = 0; j < gop_conn[i].num_P; j++ ) {
      ft = gop_conn[i].P[j];
      gop_conn[i].bit_P[gft_conn[ft].uid_block] |= gft_conn[ft].uid_mask;
    }
    for ( j = 0; j < gop_conn[i].num_A; j++ ) {
      ft = gop_conn[i].A[j];
      gop_conn[i].bit_A[gft_conn[ft].uid_block] |= gft_conn[ft].uid_mask;
    }
    for ( j = 0; j < gop_conn[i].num_D; j++ ) {
      ft = gop_conn[i].D[j];
      gop_conn[i].bit_D[gft_conn[ft].uid_block] |= gft_conn[ft].uid_mask;
    }
  }

  if ( gcmd_line.display_info == 112 ) {
    printf("\n\ninserted bits as follows:");

    printf("\n\nOP ARRAY:");
    for ( i = 0; i < gnum_op_conn; i++ ) {
      printf("\n\nOP %5d: ", i);
      print_op_name( i );
      printf("\n----------PRES:");
      for ( j = 0; j < gop_conn[i].num_P; j++ ) {
	printf("\n%4d - ", gop_conn[i].P[j]);
	print_ft_name( gop_conn[i].P[j] );
      }
      print_BitVector( gop_conn[i].bit_P, gnum_ft_bit );
      printf("\n----------ADDS:");
      for ( j = 0; j < gop_conn[i].num_A; j++ ) {
	printf("\n%4d - ", gop_conn[i].A[j]);
	print_ft_name( gop_conn[i].A[j] );
      }
      print_BitVector( gop_conn[i].bit_A, gnum_ft_bit );
      printf("\n----------DELS:");
      for ( j = 0; j < gop_conn[i].num_D; j++ ) {
	printf("\n%4d - ", gop_conn[i].D[j]);
	print_ft_name( gop_conn[i].D[j] );
      }
      print_BitVector( gop_conn[i].bit_D, gnum_ft_bit );
    }
    
    printf("\n\nFT ARRAY:");
    for ( i = 0; i < gnum_ft_conn; i++ ) {
      printf("\n\nFT: ");
      print_ft_name( i );
      printf("\n----------PRE OF:");
      for ( j = 0; j < gft_conn[i].num_P; j++ ) {
	printf("\n");
	print_op_name( gft_conn[i].P[j] );
      }
      printf("\n----------ADD BY:");
      for ( j = 0; j < gft_conn[i].num_A; j++ ) {
	printf("\n");
	print_op_name( gft_conn[i].A[j] );
      }
      printf("\n----------DEL BY:");
      for ( j = 0; j < gft_conn[i].num_D; j++ ) {
	printf("\n");
	print_op_name( gft_conn[i].D[j] );
      }
    }

    printf("\n\nGOAL:");
    for ( i = 0; i < ggoal_state.num_F; i++ ) {
      ft = ggoal_state.F[i];
      printf("\n%4d - ", ft);
      print_ft_name( ft );
      print_BitVector( gbit_goal_state, gnum_ft_bit );
    }
  }
    


}




