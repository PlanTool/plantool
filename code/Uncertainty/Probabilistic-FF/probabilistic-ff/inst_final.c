


/*
 * THIS SOURCE CODE IS SUPPLIED  ``AS IS'' WITHOUT WARRANTY OF ANY KIND, 
 * AND ITS AUTHOR AND THE JOURNAL OF ARTIFICIAL INTELLIGENCE RESEARCH 
 * (JAIR) AND JAIR'S PUBLISHERS AND DISTRIBUTORS, DISCLAIM ANY AND ALL 
 * WARRANTIES, INCLUDING BUT NOT LIMITED TO ANY IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE, AND
 * ANY WARRANTIES OR NON INFRINGEMENT.  THE USER ASSUMES ALL LIABILITY AND
 * RESPONSIBILITY FOR USE OF THIS SOURCE CODE, AND NEITHER THE AUTHOR NOR
 * JAIR, NOR JAIR'S PUBLISHERS AND DISTRIBUTORS, WILL BE LIABLE FOR 
 * DAMAGES OF ANY KIND RESULTING FROM ITS USE.  Without limiting the 
 * generality of the foregoing, neither the author, nor JAIR, nor JAIR's
 * publishers and distributors, warrant that the Source Code will be 
 * error-free, will operate without interruption, or will meet the needs 
 * of the user.
 */








/*********************************************************************
 * File: inst_final.c
 * Description: final domain representation functions
 *
 *
 * Author: Joerg Hoffmann 2000
 *
 *********************************************************************/ 






#include <stdlib.h>



#include "ff.h"

#include "output.h"
#include "memory.h"

#include "inst_pre.h"
#include "inst_final.h"

#include "state_transitions.h"














/********************************
 * POSSIBLY TRUE FACTS ANALYSIS *
 ********************************/








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

  int size, i, j, k, adr, num;
  Bool fixpoint;
  Facts *f;
  NormOperator *no;
  EasyTemplate *t1, *t2;
  NormEffect *ne;
  Action *tmp, *a;
  Bool *had_hard_template;
  PseudoAction *pa;
  PseudoActionEffect *pae;

  gactions = NULL;
  gnum_actions = 0;

  for ( i = 0; i < gnum_predicates; i++ ) {
    size =  1;
    for ( j = 0; j < garity[i]; j++ ) {
      size *= gnum_constants;
    }

    lpos[i] = ( int_pointer ) calloc( size, sizeof( int ) );
    lneg[i] = ( int_pointer ) calloc( size, sizeof( int ) );
    luse[i] = ( int_pointer ) calloc( size, sizeof( int ) );
    lindex[i] = ( int_pointer ) calloc( size, sizeof( int ) );

    for ( j = 0; j < size; j++ ) {
      lpos[i][j] = 0;
      lneg[i][j] = 1;/* all facts but initials are poss. negative */
      luse[i][j] = 0;
      lindex[i][j] = -1;
    }
  }

  had_hard_template = ( Bool * ) calloc( gnum_hard_templates, sizeof( Bool ) );
  for ( i = 0; i < gnum_hard_templates; i++ ) {
    had_hard_template[i] = FALSE;
  }

  /* mark initial facts as possibly positive, not poss. negative
   */
  for ( i = 0; i < gnum_predicates; i++ ) {
    lp = i;
    for ( j = 0; j < gnum_initial_predicate[i]; j++ ) {
      for ( k = 0; k < garity[i]; k++ ) {
	largs[k] = ginitial_predicate[i][j].args[k];
      }
      adr = fact_adress();
      lpos[lp][adr] = 1;
      lneg[lp][adr] = 0;
    }
    for ( j = 0; j < gnum_unknown_initial_predicate[i]; j++ ) {
      for ( k = 0; k < garity[i]; k++ ) {
	largs[k] = gunknown_initial_predicate[i][j].args[k];
      }
      adr = fact_adress();
      lpos[lp][adr] = 1;
      lneg[lp][adr] = 1;
      /* neg is already 1, see above (it is checked in inst-pre that
       * the same fact does not occur both in I and unknown-I)
       * anyway, who cares
       *
       * unknown facts are assumed relevant.
       */
      luse[lp][adr] = 1;
      if ( gnum_relevant_facts == MAX_RELEVANT_FACTS ) {
	printf("\ntoo many relevant facts! increase MAX_RELEVANT_FACTS (currently %d)\n\n",
	       MAX_RELEVANT_FACTS);
	exit( 1 );
      }
      grelevant_facts[gnum_relevant_facts].predicate = lp;
      for ( k = 0; k < garity[lp]; k++ ) {
	grelevant_facts[gnum_relevant_facts].args[k] = largs[k];
      }
      lindex[lp][adr] = gnum_relevant_facts;
      gnum_relevant_facts++;
    }
  }

  /* compute fixpoint
   */
  fixpoint = FALSE;
  while ( !fixpoint ) {
    fixpoint = TRUE;

    /* assign next layer of easy templates to possibly positive fixpoint
     */
    t1 = geasy_templates;
    while ( t1 ) {
      no = t1->op;
      for ( i = 0; i < no->num_preconds; i++ ) {
	lp = no->preconds[i].predicate;
	for ( j = 0; j < garity[lp]; j++ ) {
	  largs[j] = ( no->preconds[i].args[j] >= 0 ) ?
	    no->preconds[i].args[j] : t1->inst_table[DECODE_VAR( no->preconds[i].args[j] )];
	}
	if ( !lpos[lp][fact_adress()] ) {
	  break;
	}
      }

      if ( i < no->num_preconds ) {
	t1 = t1->next;
	continue;
      }

      num = 0;
      for ( ne = no->effects; ne; ne = ne->next ) {
	num++;
	/* currently, simply ignore effect conditions and assume
	 * they will all be made true eventually.
	 */
	for ( i = 0; i < ne->num_adds; i++ ) {
	  lp = ne->adds[i].predicate;
	  for ( j = 0; j < garity[lp]; j++ ) {
	    largs[j] = ( ne->adds[i].args[j] >= 0 ) ?
	      ne->adds[i].args[j] : t1->inst_table[DECODE_VAR( ne->adds[i].args[j] )];
	  }
	  adr = fact_adress();
	  if ( !lpos[lp][adr] ) {
	    /* new relevant fact! (added neg-initial)
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
      }

      tmp = new_Action();
      tmp->norm_operator = no;
      for ( i = 0; i < no->num_vars; i++ ) {
	tmp->inst_table[i] = t1->inst_table[i];
      }
      tmp->name = no->op->name;
      tmp->num_name_vars = no->op->number_of_real_params;
      make_name_inst_table_from_NormOperator( tmp, no, t1 );
      tmp->next = gactions;
      tmp->num_effects = num;
      gactions = tmp;
      gnum_actions++;

      t2 = t1->next;
      if ( t1->next ) {
	t1->next->prev = t1->prev;
      }
      if ( t1->prev ) {
	t1->prev->next = t1->next;
      } else {
	geasy_templates = t1->next;
      }
      free_single_EasyTemplate( t1 );
      t1 = t2;
    }

    /* now assign all hard templates that have not been transformed
     * to actions yet.
     */
    for ( i = 0; i < gnum_hard_templates; i++ ) {
      if ( had_hard_template[i] ) {
	continue;
      }
      pa = ghard_templates[i];

      for ( j = 0; j < pa->num_preconds; j++ ) {
	lp = pa->preconds[j].predicate;
	for ( k = 0; k < garity[lp]; k++ ) {
	  largs[k] = pa->preconds[j].args[k];
	}
	if ( !lpos[lp][fact_adress()] ) {
	  break;
	}
      }

      if ( j < pa->num_preconds ) {
	continue;
      }

      for ( pae = pa->effects; pae; pae = pae->next ) {
	/* currently, simply ignore effect conditions and assume
	 * they will all be made true eventually.
	 */
	for ( j = 0; j < pae->num_adds; j++ ) {
	  lp = pae->adds[j].predicate;
	  for ( k = 0; k < garity[lp]; k++ ) {
	    largs[k] = pae->adds[j].args[k];
	  }
	  adr = fact_adress();
	  if ( !lpos[lp][adr] ) {
	    /* new relevant fact! (added neg-initial)
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
	    for ( k = 0; k < garity[lp]; k++ ) {
	      grelevant_facts[gnum_relevant_facts].args[k] = largs[k];
	    }
	    lindex[lp][adr] = gnum_relevant_facts;
	    gnum_relevant_facts++;
	    fixpoint = FALSE;
	  }
	}
      }

      tmp = new_Action();
      tmp->pseudo_action = pa;
      for ( j = 0; j < pa->op->num_vars; j++ ) {
	tmp->inst_table[j] = pa->inst_table[j];
      }
      tmp->name = pa->op->name;
      tmp->num_name_vars = pa->op->number_of_real_params;
      make_name_inst_table_from_PseudoAction( tmp, pa );
      tmp->next = gactions;
      tmp->num_effects = pa->num_effects;
      gactions = tmp;
      gnum_actions++;

      had_hard_template[i] = TRUE;
    }
  }

  free( had_hard_template );

  gnum_pp_facts = gnum_initial + gnum_relevant_facts;

  if ( gcmd_line.display_info == 118 ) {
    printf("\nreachability analysys came up with:");

    printf("\n\npossibly positive facts:");
    for ( f = ginitial; f; f = f->next ) {
      printf("\n");
      print_Fact( f->fact );
    }
    for ( i = 0; i < gnum_relevant_facts; i++ ) {
      printf("\n");
      print_Fact( &(grelevant_facts[i]) );
    }

    printf("\n\nthis yields these %d action templates:", gnum_actions);
    for ( i = 0; i < gnum_operators; i++ ) {
      printf("\n\noperator %s:", goperators[i]->name);
      for ( a = gactions; a; a = a->next ) {
	if ( ( a->norm_operator && 
	       a->norm_operator->op !=  goperators[i] ) ||
	     ( a->pseudo_action &&
	       a->pseudo_action->op !=  goperators[i] ) ) {
	  continue;
	}
	printf("\ntemplate: ");
	for ( j = 0; j < goperators[i]->number_of_real_params; j++ ) {
	  printf("%s", gconstants[a->name_inst_table[j]]);
	  if ( j < goperators[i]->num_vars-1 ) {
	    printf(" ");
	  }
	}
      }
    }
    printf("\n\n");
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



void make_name_inst_table_from_NormOperator( Action *a, NormOperator *o, EasyTemplate *t )

{

  int i, r = 0, m = 0;

  for ( i = 0; i < o->op->number_of_real_params; i++ ) {
    if ( o->num_removed_vars > r &&
	 o->removed_vars[r] == i ) {
      /* this var has been removed in NormOp;
       * insert type constraint constant
       *
       * at least one there, as empty typed pars ops are removed
       */
      a->name_inst_table[i] = gtype_consts[o->type_removed_vars[r]][0];
      r++;
    } else {
      /* this par corresponds to par m  in NormOp
       */
      a->name_inst_table[i] = t->inst_table[m];
      m++;
    }
  }

}



void make_name_inst_table_from_PseudoAction( Action *a, PseudoAction *pa )

{

  int i;

  for ( i = 0; i < pa->op->number_of_real_params; i++ ) {
    a->name_inst_table[i] = pa->inst_table[i];
  }

}


















/***********************************************************
 * RELEVANCE ANALYSIS AND FINAL DOMAIN AND PROBLEM CLEANUP *
 ***********************************************************/









/* counts effects for later allocation
 */
int lnum_effects;









void collect_relevant_facts( void )

{

  Action *a;
  NormOperator *no;
  NormEffect *ne;
  int i, j, adr;
  PseudoAction *pa;
  PseudoActionEffect *pae;

  /* mark all deleted facts; such facts, that are also pos, are relevant.
   */
  for ( a = gactions; a; a = a->next ) {
    if ( a->norm_operator ) {
      no = a->norm_operator;

      for ( ne = no->effects; ne; ne = ne->next ) {
	for ( i = 0; i < ne->num_dels; i++ ) {
	  lp = ne->dels[i].predicate;
	  for ( j = 0; j < garity[lp]; j++ ) {
	    largs[j] = ( ne->dels[i].args[j] >= 0 ) ?
	      ne->dels[i].args[j] : a->inst_table[DECODE_VAR( ne->dels[i].args[j] )];
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
    } else {
      pa = a->pseudo_action;

      for ( pae = pa->effects; pae; pae = pae->next ) {
	for ( i = 0; i < pae->num_dels; i++ ) {
	  lp = pae->dels[i].predicate;
	  for ( j = 0; j < garity[lp]; j++ ) {
	    largs[j] = pae->dels[i].args[j];
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
    }
  }

  if ( gcmd_line.display_info == 119 ) {
    printf("\n\nfacts selected as relevant:\n\n");
    for ( i = 0; i < gnum_relevant_facts; i++ ) {
      printf("\n%d: ", i);
      print_Fact( &(grelevant_facts[i]) );
    }
  }

  lnum_effects = 0;

  /* first make place for initial and goal states.
   * (one artificial fact might still be added here)
   *
   * no Us in goal, no UEs in initial or goal.
   */
  ggoal_state.F = ( int * ) calloc( gnum_relevant_facts + 1, sizeof( int ) );
  ginitial_state.F = ( int * ) calloc( gnum_relevant_facts + 1, sizeof( int ) );
  ginitial_state.U = ( int * ) calloc( gnum_relevant_facts + 1, sizeof( int ) );

  create_final_goal_state();
  create_final_initial_state();
  create_final_actions();

  if ( gcmd_line.display_info == 120 ) {
    printf("\n\nfinal domain representation is:\n\n");  
    for ( i = 0; i < gnum_operators; i++ ) {
      printf("\n\n------------------operator %s-----------\n\n", goperators[i]->name);
      for ( a = gactions; a; a = a->next ) {
	if ( ( !a->norm_operator &&
	       !a->pseudo_action ) ||
	     ( a->norm_operator && 
	       a->norm_operator->op != goperators[i] ) ||
	     ( a->pseudo_action &&
	       a->pseudo_action->op != goperators[i] ) ) {
	  continue;
	}
	print_Action( a );
      }
    }
    printf("\n\n--------------------GOAL REACHED ops-----------\n\n");
    for ( a = gactions; a; a = a->next ) {
      if ( !a->norm_operator &&
	   !a->pseudo_action ) {
	print_Action( a );
      }
    }
   
    printf("\n\nfinal initial state is, known:\n\n");
    for ( i = 0; i < ginitial_state.num_F; i++ ) {
      print_ft_name( ginitial_state.F[i] );
      printf("\n");
    }
    printf("\n\nfinal initial state is, unknown:\n\n");
    for ( i = 0; i < ginitial_state.num_U; i++ ) {
      print_ft_name( ginitial_state.U[i] );
      printf("\n");
    }
    printf("\n\nfinal goal state is:\n\n");
    for ( i = 0; i < ggoal_state.num_F; i++ ) {
      print_ft_name( ggoal_state.F[i] );
      printf("\n");
    }
  }

}



void create_final_goal_state( void )

{

  WffNode *w, *ww;
  int m, i, adr;
  Action *tmp;

  set_relevants_in_wff( &ggoal );
  cleanup_wff( &ggoal );
  if ( ggoal->connective == TRU ) {
    printf("\nff: goal can be simplified to TRUE. The empty plan solves it\n\n");
    exit( 1 );
  }
  if ( ggoal->connective == FAL ) {
    printf("\nff: goal can be simplified to FALSE. No plan will solve it\n\n");
    exit( 1 );
  }

  switch ( ggoal->connective ) {
  case OR:
    printf("\nsorry -- this version of cff can't handle disjunctive goals.\n\n");
    exit( 1 );
    if ( gnum_relevant_facts == MAX_RELEVANT_FACTS ) {
      printf("\nincrease MAX_RELEVANT_FACTS! (current value: %d)\n\n",
	     MAX_RELEVANT_FACTS);
      exit( 1 );
    }
    grelevant_facts[gnum_relevant_facts].predicate = -3;
    gnum_relevant_facts++;
    for ( w = ggoal->sons; w; w = w->next ) {
      tmp = new_Action();
      if ( w->connective == AND ) {
	m = 0;
	for ( ww = w->sons; ww; ww = ww->next ) m++;
	tmp->preconds = ( int * ) calloc( m, sizeof( int ) );
	tmp->num_preconds = 0;
	for ( ww = w->sons; ww; ww = ww->next ) {
	  lp = ww->fact->predicate;
	  for ( i = 0; i < garity[lp]; i++ ) {
	    largs[i] = ww->fact->args[i];
	  }
	  adr = fact_adress();
	  tmp->preconds[tmp->num_preconds++] = lindex[lp][adr];
	}
      } else {
	tmp->preconds = ( int * ) calloc( 1, sizeof( int ) );
	tmp->num_preconds = 1;
	lp = w->fact->predicate;
	for ( i = 0; i < garity[lp]; i++ ) {
	  largs[i] = w->fact->args[i];
	}
	adr = fact_adress();
	tmp->preconds[0] = lindex[lp][adr];
      }
      tmp->effects = ( ActionEffect * ) calloc( 1, sizeof( ActionEffect ) );
      tmp->num_effects = 1;
      tmp->effects[0].conditions = NULL;
      tmp->effects[0].num_conditions = 0;
      tmp->effects[0].dels = NULL;
      tmp->effects[0].num_dels = 0;
      tmp->effects[0].adds = ( int * ) calloc( 1, sizeof( int ) );
      tmp->effects[0].adds[0] = gnum_relevant_facts - 1;
      tmp->effects[0].num_adds = 1;
      tmp->next = gactions;
      gactions = tmp;
      gnum_actions++;
      lnum_effects++;
    }
    ggoal_state.F[0] = gnum_relevant_facts - 1;
    ggoal_state.num_F = 1;
    break;
  case AND:
    for ( w = ggoal->sons; w; w = w->next ) {
      lp = w->fact->predicate;
      for ( i = 0; i < garity[lp]; i++ ) {
	largs[i] = w->fact->args[i];
      }
      adr = fact_adress();
      ggoal_state.F[ggoal_state.num_F++] = lindex[lp][adr];
    }
    break;
  case ATOM:
    ggoal_state.num_F = 1;
    lp = ggoal->fact->predicate;
    for ( i = 0; i < garity[lp]; i++ ) {
      largs[i] = ggoal->fact->args[i];
    }
    adr = fact_adress();
    ggoal_state.F[0] = lindex[lp][adr];
    break;
  default:
    printf("\n\nwon't get here: non ATOM,AND,OR in fully simplified goal\n\n");
    exit( 1 );
  }

}



void set_relevants_in_wff( WffNode **w )

{

  WffNode *i;
  int j, adr;

  switch ( (*w)->connective ) {
  case AND:
  case OR:
    for ( i = (*w)->sons; i; i = i->next ) {
      set_relevants_in_wff( &i );
    }
    break;
  case ATOM:
    /* no equalities, as fully instantiated
     */
    lp = (*w)->fact->predicate;
    for ( j = 0; j < garity[lp]; j++ ) {
      largs[j] = (*w)->fact->args[j];
    }
    adr = fact_adress();

    if ( !lneg[lp][adr] ) {
      (*w)->connective = TRU;
      free( (*w)->fact );
      (*w)->fact = NULL;
      break;
    }
    if ( !lpos[lp][adr] ) {
      (*w)->connective = FAL;
      free( (*w)->fact );
      (*w)->fact = NULL;
      break;
    }
    break;
  default:
    printf("\n\nwon't get here: non NOT,OR,AND in goal set relevants\n\n");
    exit( 1 );
  }

}



void create_final_initial_state( void )

{

  Facts *f;
  int i, adr;

  for ( f = ginitial; f; f = f->next ) {
    lp = f->fact->predicate;
    for ( i = 0; i < garity[lp]; i++ ) {
      largs[i] = f->fact->args[i];
    }
    adr = fact_adress();
    if ( !lneg[lp][adr] ) {/* non deleted ini */
      continue;
    }
    ginitial_state.F[ginitial_state.num_F++] = lindex[lp][adr];
  }

  ginitial_state.num_U = 0;
  for ( f = gunknown_initial; f; f = f->next ) {
    lp = f->fact->predicate;
    for ( i = 0; i < garity[lp]; i++ ) {
      largs[i] = f->fact->args[i];
    }
    adr = fact_adress();
    ginitial_state.U[ginitial_state.num_U++] = lindex[lp][adr];
  }
 
}



void create_final_actions( void )

{

  Action *a, *p, *t;
  NormOperator *no;
  NormEffect *ne;
  int i, j, adr;
  PseudoAction *pa;
  PseudoActionEffect *pae;

  a = gactions; p = NULL;
  while ( a ) {
    if ( a->norm_operator ) {
      /* action comes from an easy template NormOp
       */
      no = a->norm_operator;

      if ( no->num_preconds > 0 ) {
	a->preconds = ( int * ) calloc( no->num_preconds, sizeof( int ) );
      }
      a->num_preconds = 0;
      for ( i = 0; i < no->num_preconds; i++ ) {
	lp = no->preconds[i].predicate;
	for ( j = 0; j < garity[lp]; j++ ) {
	  largs[j] = ( no->preconds[i].args[j] >= 0 ) ?
	    no->preconds[i].args[j] : a->inst_table[DECODE_VAR( no->preconds[i].args[j] )];
	}
	adr = fact_adress();
	
	/* preconds are lpos in all cases due to reachability analysis
	 */
	if ( !lneg[lp][adr] ) {
	  continue;
	}
	
	a->preconds[a->num_preconds++] = lindex[lp][adr];
      }

      if ( a->num_effects > 0 ) {
	a->effects = ( ActionEffect * ) calloc( a->num_effects, sizeof( ActionEffect ) );
      }
      a->num_effects = 0;
      for ( ne = no->effects; ne; ne = ne->next ) {
	a->effects[a->num_effects].eff_p = ne->eff_p;/* july06: copy over eff prob */

	if ( ne->num_conditions > 0 ) {
	  a->effects[a->num_effects].conditions =
	    ( int * ) calloc( ne->num_conditions, sizeof( int ) );
	}
	a->effects[a->num_effects].num_conditions = 0;

	for ( i = 0; i < ne->num_conditions; i++ ) {
	  lp = ne->conditions[i].predicate;
	  for ( j = 0; j < garity[lp]; j++ ) {
	    largs[j] = ( ne->conditions[i].args[j] >= 0 ) ?
	      ne->conditions[i].args[j] : a->inst_table[DECODE_VAR( ne->conditions[i].args[j] )];
	  }
	  adr = fact_adress();
	  if ( !lpos[lp][adr] ) {/* condition not reachable: skip effect */
	    break;
	  }
	  if ( !lneg[lp][adr] ) {/* condition always true: skip it */
	    continue;
	  }
	  a->effects[a->num_effects].conditions[a->effects[a->num_effects].num_conditions++] =
	    lindex[lp][adr];
	}

	if ( i < ne->num_conditions ) {/* found unreachable condition: free condition space */
	  free( a->effects[a->num_effects].conditions );
	  continue;
	}

	/* now create the add and del effects.
	 */
	if ( ne->num_adds > 0 ) {
	  a->effects[a->num_effects].adds = ( int * ) calloc( ne->num_adds, sizeof( int ) );
	}
	a->effects[a->num_effects].num_adds = 0;
	for ( i = 0; i < ne->num_adds; i++ ) {
	  lp = ne->adds[i].predicate;
	  for ( j = 0; j < garity[lp]; j++ ) {
	    largs[j] = ( ne->adds[i].args[j] >= 0 ) ?
	      ne->adds[i].args[j] : a->inst_table[DECODE_VAR( ne->adds[i].args[j] )];
	  }
	  adr = fact_adress();

	  if ( !lneg[lp][adr] ) {/* effect always true: skip it */
	    continue;
	  }
	  
	  a->effects[a->num_effects].adds[a->effects[a->num_effects].num_adds++] = lindex[lp][adr];
	}

	if ( ne->num_dels > 0 ) {
	  a->effects[a->num_effects].dels = ( int * ) calloc( ne->num_dels, sizeof( int ) );
	}
	a->effects[a->num_effects].num_dels = 0;
	for ( i = 0; i < ne->num_dels; i++ ) {
	  lp = ne->dels[i].predicate;
	  for ( j = 0; j < garity[lp]; j++ ) {
	    largs[j] = ( ne->dels[i].args[j] >= 0 ) ?
	      ne->dels[i].args[j] : a->inst_table[DECODE_VAR( ne->dels[i].args[j] )];
	  }
	  adr = fact_adress();

	  if ( !lpos[lp][adr] ) {/* effect always false: skip it */
	    continue;
	  }

	  a->effects[a->num_effects].dels[a->effects[a->num_effects].num_dels++] = lindex[lp][adr];
	}
	if ( i < ne->num_dels ) break;

	/* this effect is OK. go to next one in NormOp.
	 */
	a->num_effects++;
	lnum_effects++;
      }
      if ( ne ) {
	/* we get here if one effect was faulty
	 */
	if ( p ) {
	  p->next = a->next;
	  t = a;
	  a = a->next;
	  free_single_Action( t );
	} else {
	  gactions = a->next;
	  t = a;
	  a = a->next;
	  free_single_Action( t );
	}
      } else {
	p = a;
	a = a->next;
      }
      continue;
    }
    if ( a->pseudo_action ) {
      /* action is result of a PseudoAction
       */
      pa = a->pseudo_action;

      if ( pa->num_preconds > 0 ) {
	a->preconds = ( int * ) calloc( pa->num_preconds, sizeof( int ) );
      }
      a->num_preconds = 0;
      for ( i = 0; i < pa->num_preconds; i++ ) {
	lp = pa->preconds[i].predicate;
	for ( j = 0; j < garity[lp]; j++ ) {
	  largs[j] = pa->preconds[i].args[j];
	}
	adr = fact_adress();
	
	/* preconds are lpos in all cases due to reachability analysis
	 */
	if ( !lneg[lp][adr] ) {
	  continue;
	}
	
	a->preconds[a->num_preconds++] = lindex[lp][adr];
      }

      if ( a->num_effects > 0 ) {
	a->effects = ( ActionEffect * ) calloc( a->num_effects, sizeof( ActionEffect ) );
      }
      a->num_effects = 0;
      for ( pae = pa->effects; pae; pae = pae->next ) {
	a->effects[a->num_effects].eff_p = pae->eff_p;/* july06: copy over eff prob */

	if ( pae->num_conditions > 0 ) {
	  a->effects[a->num_effects].conditions =
	    ( int * ) calloc( pae->num_conditions, sizeof( int ) );
	}
	a->effects[a->num_effects].num_conditions = 0;

	for ( i = 0; i < pae->num_conditions; i++ ) {
	  lp = pae->conditions[i].predicate;
	  for ( j = 0; j < garity[lp]; j++ ) {
	    largs[j] = pae->conditions[i].args[j];
	  }
	  adr = fact_adress();

	  if ( !lpos[lp][adr] ) {/* condition not reachable: skip effect */
	    break;
	  }
	  if ( !lneg[lp][adr] ) {/* condition always true: skip it */
	    continue;
	  }
	  
	  a->effects[a->num_effects].conditions[a->effects[a->num_effects].num_conditions++] =
	    lindex[lp][adr];
	}

	if ( i < pae->num_conditions ) {/* found unreachable condition: free condition space */
	  free( a->effects[a->num_effects].conditions );
	  continue;
	}

	/* now create the add and del effects.
	 */
	if ( pae->num_adds > 0 ) {
	  a->effects[a->num_effects].adds = ( int * ) calloc( pae->num_adds, sizeof( int ) );
	}
	a->effects[a->num_effects].num_adds = 0;
	for ( i = 0; i < pae->num_adds; i++ ) {
	  lp = pae->adds[i].predicate;
	  for ( j = 0; j < garity[lp]; j++ ) {
	    largs[j] = pae->adds[i].args[j];
	  }
	  adr = fact_adress();

	  if ( !lneg[lp][adr] ) {/* effect always true: skip it */
	    continue;
	  }
	  
	  a->effects[a->num_effects].adds[a->effects[a->num_effects].num_adds++] = lindex[lp][adr];
	}

	if ( pae->num_dels > 0 ) {
	  a->effects[a->num_effects].dels = ( int * ) calloc( pae->num_dels, sizeof( int ) );
	}
	a->effects[a->num_effects].num_dels = 0;
	for ( i = 0; i < pae->num_dels; i++ ) {
	  lp = pae->dels[i].predicate;
	  for ( j = 0; j < garity[lp]; j++ ) {
	    largs[j] = pae->dels[i].args[j];
	  }
	  adr = fact_adress();

	  if ( !lpos[lp][adr] ) {/* effect always false: skip it */
	    continue;
	  }
	  
	  a->effects[a->num_effects].dels[a->effects[a->num_effects].num_dels++] = lindex[lp][adr];
	}
	if ( i < pae->num_dels ) break;

	/* this effect is OK. go to next one in PseudoAction.
	 */
	a->num_effects++;
	lnum_effects++;
      }
      if ( pae ) {
	/* we get here if one effect was faulty
	 */
	if ( p ) {
	  p->next = a->next;
	  t = a;
	  a = a->next;
	  free_single_Action( t );
	} else {
	  gactions = a->next;
	  t = a;
	  a = a->next;
	  free_single_Action( t );
	}
      } else {
	p = a;
	a = a->next;
      }
      continue;
    }/* end of if clause for PseudoAction */
    /* if action was neither normop, nor pseudo action determined,
     * then it is an artificial action due to disjunctive goal
     * conditions.
     *
     * these are already in final form.
     */
    p = a;
    a = a->next;
  }/* endfor all actions ! */

}















/**************************************************
 * CONNECTIVITY GRAPH. ULTRA CLEAN REPRESENTATION *
 **************************************************/










int leck;


void build_connectivity_graph( void )

{

  int i, j, k, l, n_op, n_ef, ef, adr, ind1, ind2;
  Action *a;
  int ft;
  ActionEffect *e;
  Facts *ff, *fff;
  WffNode *iwff, *jwff;
  int ef1, ef2;
  Bool changes, ef1_implies_ef2, ef2_implies_ef1;

  int nr_srel, last_srel, num_cpt_rows;

  int chancevar, negchancevar;
  int targetfact, negtargetfact;

  int multiID;

  double lll;

  Bool *hadef;

  struct timeb tp;

  ftime( &tp );
  srandom( tp.millitm );


  num_cpt_rows = 0;
  for ( ff = gunknown_initial; ff; ff = ff->next ) {
    num_cpt_rows += ff->num_cpt;
  }

  /* gnum_ft_conn will increase below, NOT ...real...
   */
  gnum_ft_conn = gnum_relevant_facts;
  gnum_real_ft_conn = gnum_relevant_facts;
  gnum_op_conn = gnum_actions;
  /* make space for the chance vars; they will be added below.
   * twice because every chance var (one for each cpt row) will have a neg translation.
   */ 
  leck = gnum_ft_conn + 2*num_cpt_rows;
  gft_conn = ( FtConn * ) calloc( gnum_ft_conn + 2*num_cpt_rows, sizeof( FtConn ) );
  gop_conn = ( OpConn * ) calloc( gnum_op_conn, sizeof( OpConn ) );
  gef_conn = ( EfConn * ) calloc( lnum_effects, sizeof( EfConn ) );
  gnum_ef_conn = 0;
  gnum_pef_conn = 0;


  for ( i = 0; i < gnum_ft_conn; i++ ) {
    gft_conn[i].negation = -1;

    gft_conn[i].num_P = 0;
    gft_conn[i].num_PC = 0;
    gft_conn[i].num_C = 0;
    gft_conn[i].num_A = 0;
    gft_conn[i].num_D = 0;

    gft_conn[i].srelevant = FALSE;
    gft_conn[i].poss_U = FALSE;

    /* CNF member will be set below
     */

    gft_conn[i].rand = random() % BIG_INT;


    /* initialization, may change below.
     */
    gft_conn[i].num_multi = 0;
    gft_conn[i].multiID = -1;
    gft_conn[i].multi = NULL;
    gft_conn[i].had_multi = FALSE;
    gft_conn[i].realmulti = FALSE;
    gft_conn[i].parsed_weight = -1;
    gft_conn[i].is_state_var = TRUE;  
    gft_conn[i].chance_vars = NULL;
    gft_conn[i].num_chance_vars = 0;
    gft_conn[i].weighted = FALSE;
    gft_conn[i].weight_a = -1;
    gft_conn[i].weight_b = -1;
    gft_conn[i].chance_var_for = -1;
    gft_conn[i].cpt_row = -1;
  }

  /* now set up the initial equivalence array(s)
   *
   * make space for chance vars!! one equivalence per (nontrivial) cpt row
   */
  ginitial_equivalence_A = ( int * ) 
    calloc( gnum_initial_ft_equivalence + num_cpt_rows, sizeof( int ) );
  ginitial_equivalence_notA = ( int * ) 
    calloc( gnum_initial_ft_equivalence + num_cpt_rows, sizeof( int ) );
  gnum_initial_equivalence = 0;
  for ( ff = ginitial_ft_equivalence_A, 
	  fff = ginitial_ft_equivalence_notA; 
	ff; ff = ff->next, fff = fff->next ) {
    lp = ff->fact->predicate;
    for ( i = 0; i < garity[lp]; i++ ) {
      largs[i] = ff->fact->args[i];
    }
    adr = fact_adress();
    ind1 = lindex[lp][adr];
    if ( ind1 == -1 ) continue;
    ginitial_equivalence_A[gnum_initial_equivalence] = ind1;
    lp = fff->fact->predicate;
    for ( i = 0; i < garity[lp]; i++ ) {
      largs[i] = fff->fact->args[i];
    }
    adr = fact_adress();
    ind2 = lindex[lp][adr];
    if ( ind2 == -1 ) continue;
    ginitial_equivalence_notA[gnum_initial_equivalence++] = ind2;

    gft_conn[ind1].negation = ind2;
    gft_conn[ind2].negation = ind1;
  }

  for ( i = 0; i < gnum_op_conn; i++ ) {
    gop_conn[i].num_P = 0;
    gop_conn[i].num_E = 0;

    gop_conn[i].is_in_A = FALSE;
    gop_conn[i].is_in_H = FALSE;
  }

  for ( i = 0; i < lnum_effects; i++ ) {
    gef_conn[i].num_PC = 0;
    gef_conn[i].num_C = 0;
    gef_conn[i].num_A = 0;
    gef_conn[i].num_D = 0;
    gef_conn[i].num_I = 0;

    gef_conn[i].removed = FALSE;
  }


  n_op = 0;
  n_ef = 0;
  for ( a = gactions; a; a = a->next ) {

    gop_conn[n_op].action = a;

    gop_conn[n_op].P = ( int * ) calloc( a->num_preconds, sizeof( int ) );
    for ( i = 0; i < a->num_preconds; i++ ) {
	for ( j = 0; j < gop_conn[n_op].num_P; j++ ) {
	    if ( gop_conn[n_op].P[j] == a->preconds[i] ) break;
	}
	if ( j < gop_conn[n_op].num_P ) continue;
	gop_conn[n_op].P[gop_conn[n_op].num_P++] = a->preconds[i];
    }
    
    gop_conn[n_op].E = ( int * ) calloc( a->num_effects, sizeof( int ) );
    /* july06: from this loop, removed the merging of effects with the 
     * same condition -- these will be our separate outcomes!!!
     *
     */
    for ( i = 0; i < a->num_effects; i++ ) {
      e = &(a->effects[i]);
      gop_conn[n_op].E[gop_conn[n_op].num_E++] = n_ef;
      gef_conn[n_ef].op = n_op;
      gef_conn[n_ef].eff_p = e->eff_p;

      gef_conn[n_ef].PC = ( int * ) 
	calloc( e->num_conditions + a->num_preconds, sizeof( int ) );
      gef_conn[n_ef].C = ( int * ) 
	calloc( e->num_conditions, sizeof( int ) );
      for ( j = 0; j < a->num_preconds; j++ ) {
	for ( k = 0; k < gef_conn[n_ef].num_PC; k++ ) {
	  if ( gef_conn[n_ef].PC[k] == a->preconds[j] ) break;
	}
	if ( k < gef_conn[n_ef].num_PC ) continue;
	gef_conn[n_ef].PC[gef_conn[n_ef].num_PC++] = a->preconds[j];
      }
      for ( j = 0; j < e->num_conditions; j++ ) {
	for ( k = 0; k < gef_conn[n_ef].num_PC; k++ ) {
	  if ( gef_conn[n_ef].PC[k] == e->conditions[j] ) break;
	}
	if ( k == gef_conn[n_ef].num_PC ) {
	    gef_conn[n_ef].PC[gef_conn[n_ef].num_PC++] = e->conditions[j];
	}
	/* do not include preconds into the effect conds;
	 *
	 * NOTE: thus we might accept non-unary antecedents, namely
	 *       when they're redundant in that sense anyway; is there
	 *       a generalization to this?
	 */
	for ( k = 0; k < gop_conn[n_op].num_P; k++ ) {
	  if ( gop_conn[n_op].P[k] == e->conditions[j] ) break;
	}
	if ( k < gop_conn[n_op].num_P ) {
	    continue;
	}
	for ( k = 0; k < gef_conn[n_ef].num_C; k++ ) {
	  if ( gef_conn[n_ef].C[k] == e->conditions[j] ) break;
	}
	if ( k == gef_conn[n_ef].num_C ) {
	    gef_conn[n_ef].C[gef_conn[n_ef].num_C++] = e->conditions[j];
	}
      }

      gef_conn[n_ef].A = ( int * ) calloc( e->num_adds, sizeof( int ) );
      gef_conn[n_ef].D = ( int * ) calloc( e->num_dels, sizeof( int ) );
      for ( j = 0; j < e->num_adds; j++ ) {
	for ( k = 0; k < gef_conn[n_ef].num_A; k++ ) {
	  if ( gef_conn[n_ef].A[k] == e->adds[j] ) break;
	}
	if ( k < gef_conn[n_ef].num_A ) {
	  continue;
	}
	/* adding conds is useless; need this for easier implementation of 
	 * stagnating states, see repeated_states.c at end
	 */
	for ( k = 0; k < gef_conn[n_ef].num_PC; k++ ) {
	  if ( gef_conn[n_ef].PC[k] == e->adds[j] ) break;
	}
	if ( k < gef_conn[n_ef].num_PC ) continue;
	gef_conn[n_ef].A[gef_conn[n_ef].num_A++] = e->adds[j];
      }
      for ( j = 0; j < e->num_dels; j++ ) {
	for ( k = 0; k < gef_conn[n_ef].num_D; k++ ) {
	  if ( gef_conn[n_ef].D[k] == e->dels[j] ) break;
	}
	if ( k < gef_conn[n_ef].num_D ) {
	  continue;
	}
	/* this is to keep the symmetry between fts and their negation!
	 */
	if ( gft_conn[e->dels[j]].negation != -1 ) {
	  for ( k = 0; k < gef_conn[n_ef].num_PC; k++ ) {
	    if ( gef_conn[n_ef].PC[k] == gft_conn[e->dels[j]].negation ) break;
	  }
	  if ( k < gef_conn[n_ef].num_PC ) continue;
	}
	gef_conn[n_ef].D[gef_conn[n_ef].num_D++] = e->dels[j];
      }
      
      n_ef++;
      gnum_ef_conn++;
    }/* ende all a->effects */


    /* july06: removed "empty effects check" since that seems pathologically
     * irrelevant anyway, and of doubtable correctness, since "only deletes"
     * is useless only in a sub-class of domains.
     */


    /* july06: setup same AND implied effects info
     * same cond effects := alternative prob. outcomes.
     */
    if ( gop_conn[n_op].num_E > 1 ) {
      for ( i = 0; i < gop_conn[n_op].num_E; i++ ) {
	ef = gop_conn[n_op].E[i];
	gef_conn[ef].S = ( int * ) calloc( gop_conn[n_op].num_E, sizeof( int ) );
	gef_conn[ef].num_S = 0;
	gef_conn[ef].I = ( int * ) calloc( gop_conn[n_op].num_E, sizeof( int ) );
	gef_conn[ef].num_I = 0;
      }    
      /* july06: for convenience, include effect itself into the S group.
       *         in fact, keep S IDENTICAL for all efs in its group!!
       * order matters due to assignment of weight to the chance vars.
       * this way we don;t have to worry about this later when we create
       * the pef clauses -- we can just do them for any representative
       * of the S set. 
       */
      for ( i = 0; i < gop_conn[n_op].num_E; i++ ) {
	ef1 = gop_conn[n_op].E[i];
	for ( j = 0; j < gop_conn[n_op].num_E; j++ ) {
	  ef2 = gop_conn[n_op].E[j];
	  /* ef1 ==> ef2 ? */
	  ef1_implies_ef2 = TRUE;
	  for ( k = 0; k < gef_conn[ef2].num_C; k++ ) {
	    for ( l = 0; l < gef_conn[ef1].num_C; l++ ) {
	      if ( gef_conn[ef1].C[l] == gef_conn[ef2].C[k] ) break;
	    }
	    if ( l == gef_conn[ef1].num_C ) {
	      ef1_implies_ef2 = FALSE;
	      break;
	    }
	  }
	  /* ef2 ==> ef1 ? */
	  ef2_implies_ef1 = TRUE;
	  for ( k = 0; k < gef_conn[ef1].num_C; k++ ) {
	    for ( l = 0; l < gef_conn[ef2].num_C; l++ ) {
	      if ( gef_conn[ef2].C[l] == gef_conn[ef1].C[k] ) break;
	    }
	    if ( l == gef_conn[ef2].num_C ) {
	      ef2_implies_ef1 = FALSE;
	      break;
	    }
	  }
	  if ( ef1_implies_ef2 && ef2_implies_ef1 ) {
	    gef_conn[ef1].S[gef_conn[ef1].num_S++] = ef2;
	    /* july06: need this restriction later during RPG impli graph stuff
	     */
	    if ( gef_conn[ef1].num_S > MAX_OUTCOMES ) {
	      printf("\ntoo many effect outcomes. increase MAX_OUTCOMES, currently %d\n\n", 
		     MAX_OUTCOMES);
	      exit( 1 );
	    }
	    /* pff... we want to make sure that these guys all have IDENTICAL condition sets,
	     * so that in the implication graph they always choose the same open condition fact.
	     * do by this naive-stupid-fuck-it method.
	     */
	    for ( k = 0; k < gef_conn[ef1].num_C; k++ ) {
	      gef_conn[ef2].C[k] = gef_conn[ef1].C[k];
	    }
	    /* unnecessary, but... fuck it...
	     */
	    for ( k = 0; k < gef_conn[ef1].num_PC; k++ ) {
	      gef_conn[ef2].PC[k] = gef_conn[ef1].PC[k];
	    }
	    continue;
	  }
	  if ( ef1_implies_ef2 && ef1 != ef2 ) {
	    gef_conn[ef1].I[gef_conn[ef1].num_I++] = ef2;
	  }
	} /* endfor j over 2nd ef in pair, ef2 */
      } /* endfor i over 1st ef in pair, ef1 */


      /* syntax test: the Ps of each set of S effects must sum up to 1.
       */
      for ( i = 0; i < gop_conn[n_op].num_E; i++ ) {
	ef1 = gop_conn[n_op].E[i];

	lll = 0;
	for ( j = 0; j < gef_conn[ef1].num_S; j++ ) {
	  ef2 = gef_conn[ef1].S[j];

	  lll += gef_conn[ef2].eff_p;
	}
	/* this is incredible. something is wrong with these fucking
	 * doubles... I get 1.00000 != 1 if I don't do this shit here.
	 */
	lll *= 1000000;
	if ( ((int) lll) < 999999 || ((int) lll) > 1000000 ) {
	  printf("\nwarning: Ps for same-C effects of action (");
	  printf("%s", a->name ); 
	  for ( j = 0; j < a->num_name_vars; j++ ) {
	    printf(" %s", gconstants[a->name_inst_table[j]]);
	  }
	  printf(") do not seem to add up to 1 (%lf/1000000). check input.", lll);
	}
      }

    } /* endif we have more than 1 effect */




    /* count the space we need for the fact arrays !
     */
    for ( j = 0; j < gop_conn[n_op].num_P; j++ ) {
	gft_conn[gop_conn[n_op].P[j]].num_P++;
    }
    if ( gop_conn[n_op].num_E > 0 ) {
      for ( i = 0; i < gop_conn[n_op].num_E; i++ ) {
	ef = gop_conn[n_op].E[i];
	if ( gef_conn[ef].removed ) continue;
	for ( j = 0; j < gef_conn[ef].num_PC; j++ ) {
	  gft_conn[gef_conn[ef].PC[j]].num_PC++;
	}
	for ( j = 0; j < gef_conn[ef].num_C; j++ ) {
	  gft_conn[gef_conn[ef].C[j]].num_C++;
	}
 	for ( j = 0; j < gef_conn[ef].num_A; j++ ) {
	  gft_conn[gef_conn[ef].A[j]].num_A++;
	}
	for ( j = 0; j < gef_conn[ef].num_D; j++ ) {
	  gft_conn[gef_conn[ef].D[j]].num_D++;
	}
      }
    }

    n_op++;
  }/* actions loop */



  for ( i = 0; i < gnum_ft_conn; i++ ) {
    if ( gft_conn[i].num_P > 0 ) {
      gft_conn[i].P = ( int * ) calloc( gft_conn[i].num_P, sizeof( int ) );
    }
    gft_conn[i].num_P = 0;
    if ( gft_conn[i].num_PC > 0 ) {
      gft_conn[i].PC = ( int * ) calloc( gft_conn[i].num_PC, sizeof( int ) );
      gft_conn[i].srelevant = TRUE;
    }
    gft_conn[i].num_PC = 0;
    if ( gft_conn[i].num_C > 0 ) {
      gft_conn[i].C = ( int * ) calloc( gft_conn[i].num_C, sizeof( int ) );
    }
    gft_conn[i].num_C = 0;
    if ( gft_conn[i].num_A > 0 ) {
      gft_conn[i].A = ( int * ) calloc( gft_conn[i].num_A, sizeof( int ) );
    }
    gft_conn[i].num_A = 0;
    if ( gft_conn[i].num_D > 0 ) {
      gft_conn[i].D = ( int * ) calloc( gft_conn[i].num_D, sizeof( int ) );
    }
    gft_conn[i].num_D = 0;

    gft_conn[i].is_global_goal = FALSE;
  }
  for ( i = 0; i < ggoal_state.num_F; i++ ) {
    gft_conn[ggoal_state.F[i]].is_global_goal = TRUE;
    gft_conn[ggoal_state.F[i]].srelevant = TRUE;
   }

  for ( i = 0; i < gnum_op_conn; i++ ) {
    for ( j = 0; j < gop_conn[i].num_P; j++ ) {
      gft_conn[gop_conn[i].P[j]].P[gft_conn[gop_conn[i].P[j]].num_P++] = i;
    }
  }

  gmax_C = -1;
  hadef = ( Bool * ) calloc(gnum_ef_conn, sizeof( Bool ));
  for ( i = 0; i < gnum_ef_conn; i++ ) {
    hadef[i] = FALSE;
    if ( gef_conn[i].removed ) continue;

    /* july06: set the pef indices.
     */
    if ( gef_conn[i].eff_p >= 1 ) {
      gef_conn[i].pef_id = -1;
      gef_conn[i].pef_chance_id = -1;
    } else {
      gef_conn[i].pef_id = gnum_pef_conn++;
      gef_conn[i].pef_chance_id = gnum_pef_conn++;

      /* just for sanity, check that each p eff has indeed at least 2 outcomes.
       * later code (eg state trans) will rely on this without checking.
       */
      if ( gef_conn[i].num_S < 2 ) {
	printf("\nprob eff %d of ", i);
	print_op_name(gef_conn[i].op);
	printf(" with only one alternative? check input.\n\n");
	exit( 1 );
      }
    }
    
    if ( gmax_C == -1 || gef_conn[i].num_C > gmax_C ) {
      gmax_C = gef_conn[i].num_C;
    }
    for ( j = 0; j < gef_conn[i].num_PC; j++ ) {
      gft_conn[gef_conn[i].PC[j]].PC[gft_conn[gef_conn[i].PC[j]].num_PC++] = i;
    }
    for ( j = 0; j < gef_conn[i].num_C; j++ ) {
      gft_conn[gef_conn[i].C[j]].C[gft_conn[gef_conn[i].C[j]].num_C++] = i;
    }
    for ( j = 0; j < gef_conn[i].num_A; j++ ) {
      gft_conn[gef_conn[i].A[j]].A[gft_conn[gef_conn[i].A[j]].num_A++] = i;
    }
    for ( j = 0; j < gef_conn[i].num_D; j++ ) {
      gft_conn[gef_conn[i].D[j]].D[gft_conn[gef_conn[i].D[j]].num_D++] = i;
    }
  }


  /* july06: do the static global lookup table of the weights associated with
   * the pef probvars.
   */
  gpef_conn_weight_a = ( double * ) calloc(gnum_pef_conn, sizeof( double ));
  gpef_conn_weight_b = ( double * ) calloc(gnum_pef_conn, sizeof( double ));
  for ( i = 0; i < gnum_ef_conn; i++ ) {
    if ( hadef[i] ) continue;
    if ( gef_conn[i].eff_p >= 1 ) continue;

    
    /* the pefvars are all non-weighted.
     */
    for ( j = 0; j < gef_conn[i].num_S; j++ ) {
      ef = gef_conn[i].S[j];
      hadef[ef] = TRUE;

      gpef_conn_weight_a[gef_conn[ef].pef_id] = -1;
      gpef_conn_weight_b[gef_conn[ef].pef_id] = -1;
    }


    /* the weight is expressed in the corresponding pef chance vars. 
     */
    lll = 0;
    for ( j = 0; j < gef_conn[i].num_S; j++ ) {
      ef = gef_conn[i].S[j];

      gpef_conn_weight_a[gef_conn[ef].pef_chance_id] = gef_conn[ef].eff_p;
      gpef_conn_weight_b[gef_conn[ef].pef_chance_id] = 1.0-lll;

      lll += gef_conn[ef].eff_p;
    }
  }



  /* now arrange the "SA" and "SD" lists
   */
  for ( i = 0; i < gnum_ef_conn; i++ ) {
    if ( gef_conn[i].removed ) continue;

    gef_conn[i].SA = ( int * ) calloc(gef_conn[i].num_A, sizeof( int ));
    gef_conn[i].num_SA = 0;
    gef_conn[i].NSA = ( int * ) calloc(gef_conn[i].num_A, sizeof( int ));
    gef_conn[i].num_NSA = 0;
    gef_conn[i].SD = ( int * ) calloc(gef_conn[i].num_D, sizeof( int ));
    gef_conn[i].num_SD = 0;
    gef_conn[i].NSD = ( int * ) calloc(gef_conn[i].num_D, sizeof( int ));
    gef_conn[i].num_NSD = 0;

    for ( j = 0; j < gef_conn[i].num_A; j++ ) {
      for ( k = 0; k < gef_conn[i].num_S; k++ ) {
	ef = gef_conn[i].S[k];
	for ( l = 0; l < gef_conn[ef].num_A; l++ ) {
	  if ( gef_conn[ef].A[l] == gef_conn[i].A[j] ) {
	    /* found the "good" fact
	     */
	    break;
	  }
	}
	if ( l == gef_conn[ef].num_A ) {
	  /* found a "bad" eff
	   */
	  break;
	}
      }
      if ( k < gef_conn[i].num_S ) {
	/* found a "bad" eff
	 */
	gef_conn[i].NSA[gef_conn[i].num_NSA++] = gef_conn[i].A[j];
	continue;
      }
      gef_conn[i].SA[gef_conn[i].num_SA++] = gef_conn[i].A[j];
    }

    for ( j = 0; j < gef_conn[i].num_D; j++ ) {
      for ( k = 0; k < gef_conn[i].num_S; k++ ) {
	ef = gef_conn[i].S[k];
	for ( l = 0; l < gef_conn[ef].num_D; l++ ) {
	  if ( gef_conn[ef].D[l] == gef_conn[i].D[j] ) {
	    /* found the "good" fact
	     */
	    break;
	  }
	}
	if ( l == gef_conn[ef].num_D ) {
	  /* found a "bad" eff
	   */
	  break;
	}
      }
      if ( k < gef_conn[i].num_S ) {
	/* found a "bad" eff
	 */
	gef_conn[i].NSD[gef_conn[i].num_NSD++] = gef_conn[i].D[j];
	continue;
      }
      gef_conn[i].SD[gef_conn[i].num_SD++] = gef_conn[i].D[j];
    }
  }










  /* finally, do the initial state equivalence stuff etc.
   *
   * first count how many cond effs we maximally need to remember.
   */
  gmax_E = -1;
  for ( i = 0; i < gnum_op_conn; i++ ) {
    if ( gmax_E == -1 || gop_conn[i].num_E > gmax_E ) {
      gmax_E = gop_conn[i].num_E;
    }
  }



  /* say which of the equivalent fts to use in the CNFs
   *
   * we will be using the REAL facts, ie not the negation 
   * translations. guess that's easier to look at for debugging.
   */
  for ( i = 0; i < gnum_ft_conn; i++ ) {
    /* no negation there --> Ok.
     */
    if ( gft_conn[i].negation == -1 ) {
      gft_conn[i].CNF = TRUE;
      continue;
    }

    /* here, recognize real fact by having the smaller predicate nr,
     * of itself and its negation.
     */
    if ( is_real_fact( i ) ) {
      gft_conn[i].CNF = TRUE;
      continue;
    }

    gft_conn[i].CNF = FALSE;
  }






  /* and finally, the initial ORs
   *
   * NOTE: HERE, WE TRANSLATE THE ONEOF CONSTRAINTS INTO ORS!!
   * AND WE TRANSLATE THE CPTs INTO CLAUSES, introducing the chance vars on the way!
   *
   */


  /* first, count how many ORs we'll need to introduce.
   */
  j = gnum_full_or_initial;
  for ( i = 0; i < gnum_full_oneof_initial; i++ ) {
    k = 0;
    for ( iwff = gfull_oneof_initial[i]->sons; iwff; iwff = iwff->next ) k++;
    if ( k == 1 ) {
      printf("\n\nONEOF constraint is on a single fact?!\n\n");
      exit( 1 );
    }
    /* this oneof constraint has k members; need 1 non-empty, and
     * k * (k-1) / 2 exclusions ORs.
     */
    j++;
    j += k * (k-1) / 2;
  }
  for ( i = 0; i < gnum_full_multi_initial; i++ ) {
    k = 0;
    for ( iwff = gfull_multi_initial[i]->sons; iwff; iwff = iwff->next ) k++;
    if ( k == 1 ) {
      printf("\n\nMULTI constraint is on a single fact?!\n\n");
      exit( 1 );
    }
    /* this multi constraint has k members; need 1 non-empty, and
     * k * (k-1) / 2 exclusions ORs.
     */
    j++;
    j += k * (k-1) / 2;
  }
  /* max. two clauses per non-root CPT entry
   * see Sang&Beame&Kautz,AAAI'05
   */
  j += num_cpt_rows * 2;

  ginitial_or = ( int ** ) calloc( j, sizeof( int * ) );
  ginitial_or_length = ( int * ) calloc( j, sizeof( int ) );



  /* now introduce the ORs given in I, as well as those forced by ONEOF.
   */
  for ( i = 0; i < gnum_full_or_initial; i++ ) {
    k = 0;
    for ( iwff = gfull_or_initial[i]->sons; iwff; iwff = iwff->next ) k++;
    ginitial_or[i] = ( int * ) calloc( k, sizeof( int ) );

    /* note: the NOTs in the initial ORs were translated already.
     */
    ginitial_or_length[i] = 0;
    for ( iwff = gfull_or_initial[i]->sons; iwff; iwff = iwff->next ) {
      lp = iwff->fact->predicate;
      for ( j = 0; j < garity[lp]; j++ ) {
	largs[j] = iwff->fact->args[j];
      }
      adr = fact_adress();
      ind1 = lindex[lp][adr];
      if ( ind1 == -1 ) {
	printf("\n\ninitially OR-constr ft not indexed relevant?\n\n");
	exit( 1 );
      }
      ginitial_or[i][ginitial_or_length[i]++] = ind1;
    }
  }
  gnum_initial_or = gnum_full_or_initial;



  /* here come the ONEOFs
   */
  for ( i = 0; i < gnum_full_oneof_initial; i++ ) {
    k = 0;
    for ( iwff = gfull_oneof_initial[i]->sons; iwff; iwff = iwff->next ) k++;
    /* 1., the non-empty constr.
     */
    ginitial_or[gnum_initial_or] = ( int * ) calloc( k, sizeof( int ) );
    for ( iwff = gfull_oneof_initial[i]->sons; iwff; iwff = iwff->next ) {
      lp = iwff->fact->predicate;
      for ( j = 0; j < garity[lp]; j++ ) {
	largs[j] = iwff->fact->args[j];
      }
      adr = fact_adress();
      ind1 = lindex[lp][adr];
      if ( ind1 == -1 ) {
	printf("\n\ninitially ONEOF-constr ft not indexed relevant?\n\n");
	exit( 1 );
      }
      ginitial_or[gnum_initial_or][ginitial_or_length[gnum_initial_or]++] = ind1;
    }
    gnum_initial_or++;
    /* 2., the exclusion constr.
     */
    for ( iwff = gfull_oneof_initial[i]->sons; iwff->next; iwff = iwff->next ) {
      for ( jwff = iwff->next; jwff; jwff = jwff->next ) {
	lp = iwff->fact->predicate;
	for ( j = 0; j < garity[lp]; j++ ) {
	  largs[j] = iwff->fact->args[j];
	}
	adr = fact_adress();
	ind1 = lindex[lp][adr];
	if ( ind1 == -1 ) {
	  printf("\n\ninitially ONEOF-constr ft not indexed relevant?\n\n");
	  exit( 1 );
	}
	lp = jwff->fact->predicate;
	for ( j = 0; j < garity[lp]; j++ ) {
	  largs[j] = jwff->fact->args[j];
	}
	adr = fact_adress();
	ind2 = lindex[lp][adr];
	if ( ind2 == -1 ) {
	  printf("\n\ninitially ONEOF-constr ft not indexed relevant?\n\n");
	  exit( 1 );
	}
	if ( ind1 == ind2 ) {
	  printf("\n\nsame fact appears twice in a ONEOF constraint??\n\n");
	  exit( 1 );
	}
	/* really, what we want is the negated version of these facts.
	 */
	ind1 = gft_conn[ind1].negation;
	if ( ind1 == -1 ) {
	  printf("\n\nfact in ONEOF constraint not neg translated??\n\n");
	  exit( 1 );
	}
	ind2 = gft_conn[ind2].negation;
	if ( ind2 == -1 ) {
	  printf("\n\nfact in ONEOF constraint not neg translated??\n\n");
	  exit( 1 );
	}
	ginitial_or[gnum_initial_or] = ( int * ) calloc( 2, sizeof( int ) );
	ginitial_or[gnum_initial_or][0] = ind1;
	ginitial_or[gnum_initial_or][1] = ind2;
	ginitial_or_length[gnum_initial_or] = 2;
	gnum_initial_or++;
      }/* jwff */
    }/* iwff */
  }/* i over oneofs */





  /* now, introduce the CPT clauses and chance variables!
   */


  /* before we do this stuff, we assume that indimulti is true.
   */
  gindimulti = TRUE;
  if ( gnum_full_or_initial > 0 || gnum_full_oneof_initial > 0 ) {
    /* we don't want additional constraints between vars.
     */
    gindimulti = FALSE;
  }


  /* first, parse the multi-val var info and insert the oneof clauses;
   * set multiID.
   */

  multiID = 0;
  for ( i = 0; i < gnum_full_multi_initial; i++ ) {
    k = 0;
    for ( iwff = gfull_multi_initial[i]->sons; iwff; iwff = iwff->next ) k++;
    /* 1., the non-empty constr.
     */
    ginitial_or[gnum_initial_or] = ( int * ) calloc( k, sizeof( int ) );
    ginitial_or_length[gnum_initial_or] = k;
    for ( iwff = gfull_multi_initial[i]->sons; iwff; iwff = iwff->next ) {
      lp = iwff->fact->predicate;
      for ( j = 0; j < garity[lp]; j++ ) {
	largs[j] = iwff->fact->args[j];
      }
      adr = fact_adress();
      ind1 = lindex[lp][adr];
      if ( ind1 == -1 ) {
	printf("\n\ninitially MULTI-constr ft not indexed relevant?\n\n");
	exit( 1 );
      }
      /* reverse order to get back to initial state order.
       */
      ginitial_or[gnum_initial_or][--k] = ind1;
    }
    gnum_initial_or++;



    /* now the or constraint we just built contains the facts of the multi-valued var.
     * just copy that info over to the fact nodes in ftconn.
     */
    for ( j = 0; j < ginitial_or_length[gnum_initial_or-1]; j++ ) {
      ft = ginitial_or[gnum_initial_or-1][j];
      gft_conn[ft].multiID = multiID;
      gft_conn[ft].realmulti = TRUE;
      gft_conn[ft].multi = ( int * ) 
	calloc( ginitial_or_length[gnum_initial_or-1], sizeof( int ) );   
      for ( k = 0; k < ginitial_or_length[gnum_initial_or-1]; k++ ) {
	gft_conn[ft].multi[k] = ginitial_or[gnum_initial_or-1][k];
      }
      gft_conn[ft].num_multi = ginitial_or_length[gnum_initial_or-1];
      /* BTW, >= 2 in each multi.
       */
      if ( gft_conn[ft].num_multi < 2 ) {
	printf("\n\nMULTI var with less than 2 values?? check input files.\n\n");
	exit( 1 );
      }
    }



    /* 2., the exclusion constr.
     */
    for ( iwff = gfull_multi_initial[i]->sons; iwff->next; iwff = iwff->next ) {
      for ( jwff = iwff->next; jwff; jwff = jwff->next ) {
	lp = iwff->fact->predicate;
	for ( j = 0; j < garity[lp]; j++ ) {
	  largs[j] = iwff->fact->args[j];
	}
	adr = fact_adress();
	ind1 = lindex[lp][adr];
	if ( ind1 == -1 ) {
	  printf("\n\ninitially MULTI-constr ft not indexed relevant?\n\n");
	  exit( 1 );
	}
	lp = jwff->fact->predicate;
	for ( j = 0; j < garity[lp]; j++ ) {
	  largs[j] = jwff->fact->args[j];
	}
	adr = fact_adress();
	ind2 = lindex[lp][adr];
	if ( ind2 == -1 ) {
	  printf("\n\ninitially MULTI-constr ft not indexed relevant?\n\n");
	  exit( 1 );
	}
	if ( ind1 == ind2 ) {
	  printf("\n\nsame fact appears twice in a MULTI constraint??\n\n");
	  exit( 1 );
	}
	/* really, what we want is the negated version of these facts.
	 */
	ind1 = gft_conn[ind1].negation;
	if ( ind1 == -1 ) {
	  printf("\n\nfact in MULTI constraint not neg translated??\n\n");
	  exit( 1 );
	}
	ind2 = gft_conn[ind2].negation;
	if ( ind2 == -1 ) {
	  printf("\n\nfact in MULTI constraint not neg translated??\n\n");
	  exit( 1 );
	}
	ginitial_or[gnum_initial_or] = ( int * ) calloc( 2, sizeof( int ) );
	ginitial_or[gnum_initial_or][0] = ind1;
	ginitial_or[gnum_initial_or][1] = ind2;
	ginitial_or_length[gnum_initial_or] = 2;
	gnum_initial_or++;
      }/* jwff */
    }/* iwff */

    multiID++;
  }/* i over multis */
  /* do the multi info for those facts not part of a multi var.
   */
  for ( i = 0; i < gnum_ft_conn; i++ ) {
    if ( gft_conn[i].realmulti ||
	 (gft_conn[i].negation != -1 && gft_conn[gft_conn[i].negation].realmulti) ) {
      continue;
    }

    /* here, treat ft negft as "multivar"
     */
    if ( gft_conn[i].negation != -1 ) {
      /* only do if not already done by the other guy
       */
      if ( gft_conn[i].multiID == -1 ) {
	gft_conn[i].multi = ( int * ) calloc( 2, sizeof( int ) );
	gft_conn[i].multi[0] = i;
	gft_conn[i].multi[1] = gft_conn[i].negation;
	gft_conn[i].num_multi = 2;
	gft_conn[i].multiID = multiID;
	
	gft_conn[gft_conn[i].negation].multi = ( int * ) calloc( 2, sizeof( int ) );
	gft_conn[gft_conn[i].negation].multi[0] = gft_conn[i].negation;
	gft_conn[gft_conn[i].negation].multi[1] = i;
	gft_conn[gft_conn[i].negation].num_multi = 2;
	gft_conn[gft_conn[i].negation].multiID = multiID;
	multiID++;
      }
    }
  }






  /* for every unknown fact, look at its CPT
   * note: no need to do twice, ie symmetrically for
   * negations, since these have their own adapted CPTs.
   *
   * take account of the double-CPTs in the generation of the chance vars:
   * don't generate them twice, re-use if they were done previously.
   */
  /* also, regarding insertion of clauses:
   *
   * we need to do that only once since the clauses will
   * be identical after doing the CNF/negation mapping.
   *
   * let's do it for the "real" fact only!!!
   */
  for ( ff = gunknown_initial; ff; ff = ff->next ) {
    if ( ff->num_cpt < 1 ) {
      printf("\n\nunknown fact without CPT?\n\n");
      exit( 1 );
    }

    /* find target fact index, and its negation.
     */
    lp = ff->fact->predicate;
    for ( j = 0; j < garity[lp]; j++ ) {
      largs[j] = ff->fact->args[j];
    }
    adr = fact_adress();
    ind1 = lindex[lp][adr];
    if ( ind1 == -1 ) {
      printf("\n\nCPT target ft not indexed relevant?\n\n");
      exit( 1 );
    }
    targetfact = ind1;
    ind1 = gft_conn[ind1].negation;
    if ( ind1 == -1 ) {
      printf("\n\nCPT target ft not neg translated??\n\n");
      exit( 1 );
    }
    negtargetfact = ind1;


    /* special case: fact is part of a multi-valued var!
     * do all this right here, ie for entire multival val.
     */ 
    if ( gft_conn[targetfact].realmulti ) {
      /* only do sthg if we didn't process this multivar before.
       */
      if ( !gft_conn[targetfact].had_multi ) {
	do_cnf_multi_var( targetfact );
      }
      continue;
    }

    /* this case here we just skip, i.e., we do NOT
     * accurately update the information for the negated
     * variables, in this case. Is superfluous (hope so at least)
     * anyway since this stuff is not gonna influence the CNFs
     * used later.
     *
     * (in that sense, the part below for boolean vars does too
     * much work)
     */
    if ( gft_conn[negtargetfact].realmulti ) {
      continue;
    }

 

    if ( ff->num_cpt == 1 ) {
      /* easy case; just adjust the fact's weight.
       */
      gft_conn[targetfact].weighted = TRUE;
      gft_conn[targetfact].weight_a = ff->cpt_p[0];
      gft_conn[targetfact].weight_b = 1.0;

      /* for treatment as "multivar": set parsed_weight
       */
      gft_conn[targetfact].parsed_weight = ff->cpt_p[0];
      continue;
    }


    gindimulti = FALSE;


    /* CPT table with more than one entry, ie not a root node.
     * generate a chance var and two clauses for every 
     * entry where cpt_p not in {0,1.0}; generate a clause
     * for every entry where cpt_p in {0,1.0}.
     */


    /* first, make space for the indexing.
     */
    gft_conn[targetfact].chance_vars = ( int * ) calloc( ff->num_cpt, sizeof ( int ) );
    gft_conn[targetfact].num_chance_vars = ff->num_cpt;
    /* actual vars will be set by loop below.
     */


    /* NOTE: all the literals in the conds were negation-translated already!
     * so, no checking for negations in the formulas, but instead
     * looking up the indices of the negated versions.
     */

    for ( i = 0; i < ff->num_cpt; i++ ) {
      if ( ff->cpt_p[i] == 0.0 ) {
	/* insert clauses for the "real" fact only!!!
	 */
	if ( is_real_fact( targetfact ) ) {
	  k = 0;
	  for ( iwff = ff->cpt_c[i]->sons; iwff; iwff = iwff->next ) k++;
	  ginitial_or[gnum_initial_or] = ( int * ) calloc( k+1, sizeof( int ) );
	  
	  k = 0;
	  for ( iwff = ff->cpt_c[i]->sons; iwff; iwff = iwff->next ) {
	    /* find fact index
	     */
	    lp = iwff->fact->predicate;
	    for ( j = 0; j < garity[lp]; j++ ) {
	      largs[j] = iwff->fact->args[j];
	    }
	    adr = fact_adress();
	    ind1 = lindex[lp][adr];
	    if ( ind1 == -1 ) {
	      printf("\n\nCPT cond ft (perc 0) not indexed relevant?\n\n");
	      exit( 1 );
	    }
	    ind1 = gft_conn[ind1].negation;
	    if ( ind1 == -1 ) {
	      printf("\n\nCPT cond ft (perc 0) not neg translated??\n\n");
	      exit( 1 );
	    }
	    ginitial_or[gnum_initial_or][k++] = ind1;
	  }
	  
	  /* take negation since probability is 0 here:
	   * if both conds are true then fact is false.
	   */
	  ginitial_or[gnum_initial_or][k++] = negtargetfact;
	  ginitial_or_length[gnum_initial_or] = k;
	  gnum_initial_or++;
	}

	/* no chance var generated.
	 */
	gft_conn[targetfact].chance_vars[i] = -1;
	continue;
      }

      if ( ff->cpt_p[i] == 1.0 ) {
	/* insert clauses for the "real" fact only!!!
	 */
	if ( is_real_fact( targetfact ) ) {
	  k = 0;
	  for ( iwff = ff->cpt_c[i]->sons; iwff; iwff = iwff->next ) k++;
	  ginitial_or[gnum_initial_or] = ( int * ) calloc( k+1, sizeof( int ) );
	  k = 0;
	  for ( iwff = ff->cpt_c[i]->sons; iwff; iwff = iwff->next ) {
	    lp = iwff->fact->predicate;
	    for ( j = 0; j < garity[lp]; j++ ) {
	      largs[j] = iwff->fact->args[j];
	    }
	    adr = fact_adress();
	    ind1 = lindex[lp][adr];
	    if ( ind1 == -1 ) {
	      printf("\n\nCPT cond ft (perc 1.0) not indexed relevant?\n\n");
	      exit( 1 );
	    }
	    ind1 = gft_conn[ind1].negation;
	    if ( ind1 == -1 ) {
	      printf("\n\nCPT cond ft (perc 1.0) not neg translated??\n\n");
	      exit( 1 );
	    }
	    ginitial_or[gnum_initial_or][k++] = ind1;
	  }
	  
	  /* take original since probability is 1.0 here:
	   * if both conds are true then fact is true.
	   */
	  ginitial_or[gnum_initial_or][k++] = targetfact;
	  ginitial_or_length[gnum_initial_or] = k;
	  gnum_initial_or++;
	}

	/* no chance var generated.
	 */
	gft_conn[targetfact].chance_vars[i] = -1;
	continue;
      }


      /* CPT entry probability between 0 and 1.0. create a chance var.
       *
       * ie, create a chance var ONLY IF none were generated
       * already for the negation of this fact.
       * if they were, then simply take them here, in the inverse
       * manner.
       */
      if ( gft_conn[negtargetfact].num_chance_vars > 0 ) {
	negchancevar = gft_conn[negtargetfact].chance_vars[i];
	chancevar = gft_conn[negchancevar].negation;
	if ( chancevar == -1 ) {
	  printf("\n\nno negation to previously created chance var??\n\n");
	  exit( 1 );
	}
	gft_conn[targetfact].chance_vars[i] = chancevar;
      } else {
	/* Ok. let's create some stuff.
	 * the chance var says: if I am true (which is the case with my weight),
	 * then "targetfact" is also true.
	 *
	 * the negated chance var says: if I am true (which is the case with 1.0 - the other guy's weight),
	 * then "targetfact" is false (i.e. "negtargetfact" is true)
	 *
	 * 
	 * NOTE: I know this is incredibly redundant; I'm not even sure it is used
	 * later on. but it may be. computational overhead right here is ZERO.
	 */

	/* this is what we'll do.
	 * (gnum_ft_conn will be changed)
	 */
	chancevar = gnum_ft_conn;
	negchancevar = gnum_ft_conn + 1;
	
	/* the "positive" occurence.
	 */
	gft_conn[gnum_ft_conn].negation = negchancevar;
	/* it does not participate
	 * in any action in any way.
	 */
	gft_conn[gnum_ft_conn].num_P = 0;
	gft_conn[gnum_ft_conn].num_PC = 0;
	gft_conn[gnum_ft_conn].num_C = 0;
	gft_conn[gnum_ft_conn].num_A = 0;
	gft_conn[gnum_ft_conn].num_D = 0;
	gft_conn[gnum_ft_conn].srelevant = FALSE;
	/* it *will* be unknown; special case treated below.
	 */
	gft_conn[gnum_ft_conn].poss_U = TRUE;
	/* use the *positive* occurence in CNFs.
	 * this means that the chance_var_for fact is real!!!
	 * (see also below)
	 */
	if ( is_real_fact( targetfact ) ) {
	  gft_conn[gnum_ft_conn].CNF = TRUE;
	} else {
	  gft_conn[gnum_ft_conn].CNF = FALSE;
	}
	/* I guess this is quite unnecessary..
	 * we aren't gonna hash this..
	 */
	gft_conn[gnum_ft_conn].rand = random() % BIG_INT;
	gft_conn[gnum_ft_conn].num_multi = 0;
	gft_conn[gnum_ft_conn].multi = NULL;
	gft_conn[gnum_ft_conn].had_multi = FALSE;
	gft_conn[gnum_ft_conn].multiID = -1;
	gft_conn[gnum_ft_conn].realmulti = FALSE;
	gft_conn[gnum_ft_conn].parsed_weight = -1;
	/* mark as chance var and SET THE WEIGHT!!
	 */
	gft_conn[gnum_ft_conn].is_state_var = FALSE;
	gft_conn[gnum_ft_conn].chance_vars = NULL;
	gft_conn[gnum_ft_conn].num_chance_vars = 0;
	gft_conn[gnum_ft_conn].weighted = TRUE;
	gft_conn[gnum_ft_conn].weight_a = ff->cpt_p[i];
	gft_conn[gnum_ft_conn].weight_b = 1.0;
	/* this guy is used only for printing;
	 * there, always say "if I am true then this guy here is also true"
	 */
	gft_conn[gnum_ft_conn].chance_var_for = targetfact;
	gft_conn[gnum_ft_conn].cpt_row = i;
	/* one more fact.
	 */
	gnum_ft_conn++;

	/* chance var generated.
	 */
	gft_conn[targetfact].chance_vars[i] = chancevar;


	
	/* the "negative" occurence.
	 */
	gft_conn[gnum_ft_conn].negation = chancevar;
	gft_conn[gnum_ft_conn].num_P = 0;
	gft_conn[gnum_ft_conn].num_PC = 0;
	gft_conn[gnum_ft_conn].num_C = 0;
	gft_conn[gnum_ft_conn].num_A = 0;
	gft_conn[gnum_ft_conn].num_D = 0;
	gft_conn[gnum_ft_conn].srelevant = FALSE;
	gft_conn[gnum_ft_conn].poss_U = TRUE;
	/* use the *positive* occurence in CNFs.
	 * this means that the chance_var_for fact is real!!!
	 * (see also below)
	 */
	if ( is_real_fact( negtargetfact ) ) {
	  gft_conn[gnum_ft_conn].CNF = TRUE;
	} else {
	  gft_conn[gnum_ft_conn].CNF = FALSE;
	}
	gft_conn[gnum_ft_conn].rand = random() % BIG_INT;
	gft_conn[gnum_ft_conn].num_multi = 0;
	gft_conn[gnum_ft_conn].multi = NULL;
	gft_conn[gnum_ft_conn].had_multi = FALSE;
	gft_conn[gnum_ft_conn].multiID = -1;
	gft_conn[gnum_ft_conn].realmulti = FALSE;
	gft_conn[gnum_ft_conn].parsed_weight = -1;
	gft_conn[gnum_ft_conn].is_state_var = FALSE;
	gft_conn[gnum_ft_conn].chance_vars = NULL;
	gft_conn[gnum_ft_conn].num_chance_vars = 0;
	gft_conn[gnum_ft_conn].weighted = TRUE;
	gft_conn[gnum_ft_conn].weight_a = 1.0 - ff->cpt_p[i];
	gft_conn[gnum_ft_conn].weight_b = 1.0;
	/* this guy is used only for printing;
	 * there, always say "if I am true then this guy here is also true"
	 */
	gft_conn[gnum_ft_conn].chance_var_for = negtargetfact;
	gft_conn[gnum_ft_conn].cpt_row = i;
	gnum_ft_conn++;
	
	
	/* insert this new equivalence into ini state equivalences
	 * (are these actually used later? well, who knows... don't matter anyway,
	 * no overhead other than lines of code....)
	 *
	 * keep order to have real fact on the left... just for printing beauty..
	 */
	if ( is_real_fact( targetfact ) ) {
	  ginitial_equivalence_A[gnum_initial_equivalence] = chancevar;
	  ginitial_equivalence_notA[gnum_initial_equivalence++] = negchancevar;
	} else {
	  ginitial_equivalence_A[gnum_initial_equivalence] = negchancevar;
	  ginitial_equivalence_notA[gnum_initial_equivalence++] = chancevar;
	}
      }

      
      /* insert clauses for the "real" fact only!!!
       */

      if ( is_real_fact( targetfact ) ) {
	/* first clause: if all conds are true, and chance var is true,
	 * then target var is true.
	 */
	k = 0;
	for ( iwff = ff->cpt_c[i]->sons; iwff; iwff = iwff->next ) k++;
	ginitial_or[gnum_initial_or] = ( int * ) calloc( k+2, sizeof( int ) );
	k = 0;
	for ( iwff = ff->cpt_c[i]->sons; iwff; iwff = iwff->next ) {
	  lp = iwff->fact->predicate;
	  for ( j = 0; j < garity[lp]; j++ ) {
	    largs[j] = iwff->fact->args[j];
	  }
	  adr = fact_adress();
	  ind1 = lindex[lp][adr];
	  if ( ind1 == -1 ) {
	    printf("\n\nCPT cond ft (0 < perc < 1.0) not indexed relevant?\n\n");
	    exit( 1 );
	  }
	  ind1 = gft_conn[ind1].negation;
	  if ( ind1 == -1 ) {
	    printf("\n\nCPT cond ft (0 < perc < 1.0) not neg translated??\n\n");
	    exit( 1 );
	  }
	  ginitial_or[gnum_initial_or][k++] = ind1;
	}
	/* negative chance var
	 */
	ginitial_or[gnum_initial_or][k++] = negchancevar;
	/* positive target fact
	 */
	ginitial_or[gnum_initial_or][k++] = targetfact;
	ginitial_or_length[gnum_initial_or] = k;
	gnum_initial_or++;
	
	/* second clause: if all conds are true, and chance var is false,
	 * then target var is false.
	 */
	k = 0;
	for ( iwff = ff->cpt_c[i]->sons; iwff; iwff = iwff->next ) k++;
	ginitial_or[gnum_initial_or] = ( int * ) calloc( k+2, sizeof( int ) );
	k = 0;
	for ( iwff = ff->cpt_c[i]->sons; iwff; iwff = iwff->next ) {
	  lp = iwff->fact->predicate;
	  for ( j = 0; j < garity[lp]; j++ ) {
	    largs[j] = iwff->fact->args[j];
	  }
	  adr = fact_adress();
	  ind1 = lindex[lp][adr];
	  if ( ind1 == -1 ) {
	    printf("\n\nCPT cond ft (0 < perc < 1.0) not indexed relevant?\n\n");
	    exit( 1 );
	  }
	  ind1 = gft_conn[ind1].negation;
	  if ( ind1 == -1 ) {
	    printf("\n\nCPT cond ft (0 < perc < 1.0) not neg translated??\n\n");
	    exit( 1 );
	  }
	  ginitial_or[gnum_initial_or][k++] = ind1;
	}
	/* positive chance var
	 */
	ginitial_or[gnum_initial_or][k++] = chancevar;
	/* negated target fact
	 */
	ginitial_or[gnum_initial_or][k++] = negtargetfact;
	ginitial_or_length[gnum_initial_or] = k;
	gnum_initial_or++;
      } /* endif fact is the ral one and so we insert the clauses!! */
    } /* loop over all entries in a CPT */
  } /* loop over all CPTs */









  /* count max nrs of unknown that we're gonna have, in
   * RPG/anywhere, and in CNFs specifically.
   */

  /* first, count the artificial extra chance vars.
   */
  gmax_U = 0;
  gmax_CNFU = gnum_pef_conn;/* july06: each p eff can get 2 extra vars, all counted into this here */
  for ( i = gnum_real_ft_conn; i < gnum_ft_conn; i++ ) {
    if ( gft_conn[i].poss_U ) {
      gmax_U++;
      if ( gft_conn[i].CNF ) {
	gmax_CNFU++;
      }
    }
  }
  /* now, for the real stuff.
   */
  for ( i = 0; i < ginitial_state.num_U; i++ ) {
    if ( gft_conn[ginitial_state.U[i]].poss_U ) {
      printf("\n\n1 fact is unknown twice in initial state?? - %d, %d\n\n", 
	     i, ginitial_state.U[i]); print_ft_name(ginitial_state.U[i]); 
      exit( 1 );
    }
    gft_conn[ginitial_state.U[i]].poss_U = TRUE;
    gmax_U++;
    if ( !gft_conn[ginitial_state.U[i]].CNF ) continue;
    gmax_CNFU++;
  }
  /* july06: make fts poss_U that are affected by a nondet effect
   */
  for ( i = 0; i < gnum_ef_conn; i++ ) {
    if ( gef_conn[i].eff_p >= 1 ) {
      continue;
    }
    for ( j = 0; j < gef_conn[i].num_A; j++ ) {
      if ( !gft_conn[gef_conn[i].A[j]].poss_U ) {
	gft_conn[gef_conn[i].A[j]].poss_U = TRUE;
	gmax_U++;
	if ( gft_conn[gef_conn[i].A[j]].CNF ) gmax_CNFU++;
      }
    }
    for ( j = 0; j < gef_conn[i].num_D; j++ ) {
      if ( !gft_conn[gef_conn[i].D[j]].poss_U ) {
	gft_conn[gef_conn[i].D[j]].poss_U = TRUE;
	gmax_U++;
	if ( gft_conn[gef_conn[i].D[j]].CNF ) gmax_CNFU++;
      }
    }
  }


  changes = TRUE;
  while ( changes ) {
    changes = FALSE;

    /* make fts poss_U that are affected by a cond effect with
     * (curr) poss_U conds
     */
    for ( i = 0; i < gnum_ef_conn; i++ ) {
      if ( gef_conn[i].removed ) continue;
      if ( gef_conn[i].num_C == 0 ) continue;

      for ( j = 0; j < gef_conn[i].num_C; j++ ) {
	if ( gft_conn[gef_conn[i].C[j]].poss_U ) break;
      }
      if ( j == gef_conn[i].num_C ) continue;

      for ( j = 0; j < gef_conn[i].num_A; j++ ) {
	if ( !gft_conn[gef_conn[i].A[j]].poss_U ) {
	  gft_conn[gef_conn[i].A[j]].poss_U = TRUE;
	  gmax_U++;
	  changes = TRUE;
	  if ( gft_conn[gef_conn[i].A[j]].CNF ) gmax_CNFU++;
	}
      }
      for ( j = 0; j < gef_conn[i].num_D; j++ ) {
	if ( !gft_conn[gef_conn[i].D[j]].poss_U ) {
	  gft_conn[gef_conn[i].D[j]].poss_U = TRUE;
	  gmax_U++;
	  changes = TRUE;
	  if ( gft_conn[gef_conn[i].D[j]].CNF ) gmax_CNFU++;
	}
      }
    }
  }


  /* apply simple tests if domination is valid
   */
  gdomination_valid = TRUE;
  for ( i = 0; i < gnum_ef_conn; i++ ) {
    if ( gef_conn[i].num_C == 0 ) continue;
    nr_srel = 0; 
    last_srel = -1;
    for ( j = 0; j < gef_conn[i].num_D; j++ ) {
      if ( gft_conn[gef_conn[i].D[j]].srelevant ) {
	nr_srel++;
	last_srel = j;
      }
    }
    if ( nr_srel == 0 ) continue;
    if ( gef_conn[i].num_C > 1 ||
	 nr_srel > 1 ) {
      break;
    }
    if ( gef_conn[i].D[last_srel] != gef_conn[i].C[0] ) {
      break;
    }
  }
  if ( i < gnum_ef_conn ) {
    gdomination_valid = FALSE;
  }

  


  /* says if or if not we got ``evidence'',
   * i.e., clauses constraining our BN. 
   */
  gBNevidence = FALSE;
  if ( gnum_full_oneof_initial > 0 ) {
    gBNevidence = TRUE;
  }
  if ( gnum_full_or_initial > 0 ) {
    gBNevidence = TRUE;
  }

/*   if ( !gBNevidence ) { */
/*     if ( gcmd_line.display_info ) { */
/*       printf("\ninitial BN unconstrained! turning weight count off."); */
/*     } */
/*   } */


  if ( gindimulti ) {
    if ( gcmd_line.heuristic == 2 ) {
      if ( gcmd_line.display_info ) {
	printf("\ninitial BN is cross-product of independent multi-state vars! no special case treatment because -h = 2!");
      }
      gindimulti = FALSE;
    } else {
      if ( gcmd_line.simulation_wmc ) {
	if ( gcmd_line.display_info ) {
	  printf("\ninitial BN is cross-product of independent multi-state vars! enabling special case treatment!");
	}
      } else {
	if ( gcmd_line.display_info ) {
	  printf("\ninitial BN is cross-product of independent multi-state vars! no special case treatment because -X!");
	}
      }
    }
  }


  if ( gcmd_line.display_info == 121 ) {
    printf("\n\ncreated connectivity graph as follows:");

    printf("\n\n------------------OP ARRAY:-----------------------");
    for ( i = 0; i < gnum_op_conn; i++ ) {
      fflush( stdout );    
      printf("\n\nOP: ");
      print_op_name( i );
      fflush( stdout );    
      printf("\n----------PS:");
      fflush( stdout );    
      for ( j = 0; j < gop_conn[i].num_P; j++ ) {
	fflush( stdout );    
	printf("\n");
	print_ft_name( gop_conn[i].P[j] );
      }
      fflush( stdout );    
      printf("\n----------EFFS:");      fflush( stdout );    
      for ( j = 0; j < gop_conn[i].num_E; j++ ) {
	fflush( stdout );    
	printf("\neffect %d", gop_conn[i].E[j]);
      }
    }
    fflush( stdout );    
    printf("\n\n-------------------EFFECT ARRAY: %d, P %d----------------------",
	   gnum_ef_conn, gnum_pef_conn);
    for ( i = 0; i < gnum_ef_conn; i++ ) {
      printf("\n\neffect %d, P %lf pef id %d(w%lf/%lf) pefchance id %d(w%lf/%lf), of op %d: ", 
	     i, gef_conn[i].eff_p, 
	     gef_conn[i].pef_id, 
	     gef_conn[i].pef_id>=0 ? gpef_conn_weight_a[gef_conn[i].pef_id]:-2,
	     gef_conn[i].pef_id>=0 ? gpef_conn_weight_b[gef_conn[i].pef_id]:-2,
	     gef_conn[i].pef_chance_id, 
	     gef_conn[i].pef_chance_id>=0 ? gpef_conn_weight_a[gef_conn[i].pef_chance_id]:-2,
	     gef_conn[i].pef_chance_id>=0 ? gpef_conn_weight_b[gef_conn[i].pef_chance_id]:-2,
	     gef_conn[i].op);
      print_op_name( gef_conn[i].op );
      if ( gef_conn[i].removed ) {
	printf(" --- REMOVED ");
	continue;
      }
      printf("\n----------PCS:");
      for ( j = 0; j < gef_conn[i].num_PC; j++ ) {
	printf("\n");
	print_ft_name( gef_conn[i].PC[j] );
      }
      printf("\n----------CS:");
      for ( j = 0; j < gef_conn[i].num_C; j++ ) {
	printf("\n");
	print_ft_name( gef_conn[i].C[j] );
      }
      printf("\n----------ADDS:");
      for ( j = 0; j < gef_conn[i].num_A; j++ ) {
	printf("\n");
	print_ft_name( gef_conn[i].A[j] );
      }
      printf("\n----------DELS:");
      for ( j = 0; j < gef_conn[i].num_D; j++ ) {
	printf("\n");
	print_ft_name( gef_conn[i].D[j] );
      }
      printf("\n----------ALTERNATIVE OUTCOMES:");
      for ( j = 0; j < gef_conn[i].num_S; j++ ) {
	printf("\nalternative effect %d, P %lf, of op %d: ", 
	       gef_conn[i].S[j], gef_conn[gef_conn[i].S[j]].eff_p, gef_conn[gef_conn[i].S[j]].op);
	print_op_name( gef_conn[gef_conn[i].S[j]].op );
      }
      printf("\n----------SADDS:");
      for ( j = 0; j < gef_conn[i].num_SA; j++ ) {
	printf("\n");
	print_ft_name( gef_conn[i].SA[j] );
      }
      printf("\n----------NSADDS:");
      for ( j = 0; j < gef_conn[i].num_NSA; j++ ) {
	printf("\n");
	print_ft_name( gef_conn[i].NSA[j] );
      }
      printf("\n----------SDELS:");
      for ( j = 0; j < gef_conn[i].num_SD; j++ ) {
	printf("\n");
	print_ft_name( gef_conn[i].SD[j] );
      }
      printf("\n----------NSDELS:");
      for ( j = 0; j < gef_conn[i].num_NSD; j++ ) {
	printf("\n");
	print_ft_name( gef_conn[i].NSD[j] );
      }
      printf("\n----------IMPLIEDS:");
      for ( j = 0; j < gef_conn[i].num_I; j++ ) {
	printf("\nimplied effect %d of op %d: ", 
	       gef_conn[i].I[j], gef_conn[gef_conn[i].I[j]].op);
	print_op_name( gef_conn[gef_conn[i].I[j]].op );
      }
    }
    fflush( stdout );    
    
    printf("\n\n----------------------FT ARRAY:-----------------------------");
    printf("\n%d real facts, %d total facts; GOAL P: %f", 
	   gnum_real_ft_conn, gnum_ft_conn, ggoal_probability);
    for ( i = 0; i < gnum_ft_conn; i++ ) {
      printf("\n\nFT %d: ", i);
      print_ft_name( i );
      printf(" negation: %d", gft_conn[i].negation);
      printf(" poss U: %d, CNF: %d", 
	     gft_conn[i].poss_U, gft_conn[i].CNF);

      printf("\nstate var? %d, weighted? %d: %f/%f parsed %f, chance-var-for %d, cpt row %d",
	     gft_conn[i].is_state_var, 
	     gft_conn[i].weighted, gft_conn[i].weight_a, gft_conn[i].weight_b, gft_conn[i].parsed_weight,
	     gft_conn[i].chance_var_for,
	     gft_conn[i].cpt_row);
      printf("\n----------MULTI VAR %d:", gft_conn[i].multiID);
      for ( j = 0; j < gft_conn[i].num_multi; j++ ) {
	printf("\n");
	print_ft_name(gft_conn[i].multi[j]);
      }
      
      if ( gft_conn[i].num_chance_vars > 0 ) {
	printf("\n----------CHANCE VARS:");
	for ( j = 0; j < gft_conn[i].num_chance_vars; j++ ) {
	  printf("\n");
	  if ( gft_conn[i].chance_vars[j] == -1 ) {
	    printf("-1");
	  } else {
	    print_ft_name(gft_conn[i].chance_vars[j]);
	  }
	}
      }

      printf("\n----------PRECOND OF:");
      for ( j = 0; j < gft_conn[i].num_P; j++ ) {
	printf("\n");
	print_op_name( gft_conn[i].P[j] );
      }
      printf("\n----------PRE or COND OF:");
      for ( j = 0; j < gft_conn[i].num_PC; j++ ) {
	printf("\neffect %d", gft_conn[i].PC[j]);
      }
      printf("\n----------COND OF:");
      for ( j = 0; j < gft_conn[i].num_C; j++ ) {
	printf("\neffect %d", gft_conn[i].C[j]);
      }
      printf("\n----------ADD BY:");
      for ( j = 0; j < gft_conn[i].num_A; j++ ) {
	printf("\neffect %d", gft_conn[i].A[j]);
      }
      printf("\n----------DEL BY:");
      for ( j = 0; j < gft_conn[i].num_D; j++ ) {
	printf("\neffect %d", gft_conn[i].D[j]);
      }
    }
    
    printf("\n\n----------------------INITIAL EQUIVALENCES:-----------------------------");
    for ( i = 0; i < gnum_initial_equivalence; i++ ) {
      printf("\nneg ");
      print_ft_name( ginitial_equivalence_A[i] );
      printf(" <-> ");
      print_ft_name( ginitial_equivalence_notA[i] );
    }
    
    printf("\n\n----------------------INITIAL ORS:-----------------------------");
    for ( i = 0; i < gnum_initial_or; i++ ) {
      printf("\nOR: ");
      for ( j = 0; j < ginitial_or_length[i]; j++ ) {
	print_ft_name( ginitial_or[i][j] );
	printf(" ");
      }
    }
    
    printf("\n\n----------------------INITIAL ORS AS IN CNF:-----------------------------");
    for ( i = 0; i < gnum_initial_or; i++ ) {
      printf("\nOR: ");
      for ( j = 0; j < ginitial_or_length[i]; j++ ) {
	if ( gft_conn[ginitial_or[i][j]].CNF ) {
	  print_ft_name( ginitial_or[i][j] );
	  printf(" ");
	} else {
	  printf("neg ");
	  print_ft_name( gft_conn[ginitial_or[i][j]].negation );
	  printf(" ");
	}
      }
    }

    fflush( stdout );
  }
    
}



Bool is_real_fact( int index )

{

  int negindex, p, negp;

  if ( !gft_conn[index].is_state_var ) {
    /* we're supposed to ask this only for state vars, not for
     * chance vars
     */
    printf("\n\nasking ``real'' for chance var??\n\n");
    exit( 1 );
  }

  negindex = gft_conn[index].negation;
  if ( negindex == -1 ) {
    /* we're supposed to ask this only for vars with negation
     */
    printf("\n\nasking ``real'' for var without negation??\n\n");
    exit( 1 );
  }

  p = grelevant_facts[index].predicate;
  negp = grelevant_facts[negindex].predicate;

  if ( p < negp ) {
    return TRUE;
  }

  return FALSE;

}









/* do all the gftconn and CNF construction for a multi-val var
 * separate because it's gonna be LOOOONG.
 */



void do_cnf_multi_var( int inputfact )

{

  int i, j, k, l, ind1, adr, ft;
  Facts *ff;
  WffNode *iwff;
  int size = gft_conn[inputfact].num_multi;
  int rows, cond_size, test;

  int chancevar, negchancevar;
  int targetfact, negtargetfact;


  /* use max 128 conditions, ie 7 parents, and max 100 values of var.
   */

  /* this here stores the CPT for the multi-val var;
   * i-index (1st) corresponds to CPT row,
   * j-index (2nd) corresponds to index of var in multi array.
   */
  double CPT_p[128][100];
  double lll;
  /* these are the CPT row conditions, as fact indices.
   */
  int CPT_c[128][7];
  /* these are the vars as indexed inside multivar
   */
  int CPT_ft[100];


  if ( size > 100 ) {
    printf("\n\nmultivar too large! increase size in inst_final.c\n\n");
    exit( 1 );
  }


  rows = 0;
  cond_size = 0;

  /* start by collecting the multivar
   */

  for ( i = 0; i < size; i++ ) {
    CPT_ft[i] = gft_conn[inputfact].multi[i];

    /* find the corresponding CPT in unknown facts list.
     */
    for ( ff = gunknown_initial; ff; ff = ff->next ) {
      lp = ff->fact->predicate;
      for ( j = 0; j < garity[lp]; j++ ) {
	largs[j] = ff->fact->args[j];
      }
      adr = fact_adress();
      ind1 = lindex[lp][adr];
      if ( ind1 == CPT_ft[i] ) {
	break;
      }
    }




    /* conds; collect them from first element,
     * check that the others are identical.
     */
    if ( i == 0 ) {
      /* see what condition size is gonna be.
       */
      cond_size = 0;
      if ( ff->num_cpt > 1 ) {
	k = 0;
	for ( iwff = ff->cpt_c[0]->sons; iwff; iwff = iwff->next ) k++;
	cond_size = k;
      }

      rows = 0;
      for ( j = 0; j < ff->num_cpt; j++ ) {
	if ( rows == 128 ) {
	  printf("\n\ntoo many conditions! increase size in inst_final.c\n\n");
	  exit( 1 );
	}
	k = 0;
	for ( iwff = ff->cpt_c[j]->sons; iwff; iwff = iwff->next ) {
	  lp = iwff->fact->predicate;
	  for ( l = 0; l < garity[lp]; l++ ) {
	    largs[l] = iwff->fact->args[l];
	  }
	  adr = fact_adress();
	  ind1 = lindex[lp][adr];
	  if ( ind1 == -1 ) {
	    printf("\n\nCPT cond ft (0 < perc < 1.0) not indexed relevant?\n\n");
	    exit( 1 );
	  }
	  if ( k == 7 ) {
	    printf("\n\nconditions too large! increase size in inst_final.c\n\n");
	    exit( 1 );
	  }
	  CPT_c[rows][k++] = ind1;
	}
	if ( k != cond_size ) {
	  printf("\n\ndifferent condition size within CPT? check input files.\n\n");
	  exit( 1 );
	}
	rows++;
      }
    } else {
      /* make sure that these conds are identical.
       */
      test = 0;
      for ( j = 0; j < ff->num_cpt; j++ ) {
	if ( test > rows ) {
	  printf("\n\ndiff nr conditions in multi! check input files\n\n");
	  exit( 1 );
	}
	k = 0;
	for ( iwff = ff->cpt_c[j]->sons; iwff; iwff = iwff->next ) {
	  lp = iwff->fact->predicate;
	  for ( l = 0; l < garity[lp]; l++ ) {
	    largs[l] = iwff->fact->args[l];
	  }
	  adr = fact_adress();
	  ind1 = lindex[lp][adr];
	  if ( ind1 == -1 ) {
	    printf("\n\nCPT cond ft (0 < perc < 1.0) not indexed relevant?\n\n");
	    exit( 1 );
	  }
	  if ( k == 7 ) {
	    printf("\n\nconditions too large! increase size in inst_final.c\n\n");
	    exit( 1 );
	  }
	  if ( CPT_c[test][k++] != ind1 ) {
	    printf("\n\ndiff conditions in multi! check input files\n\n");
	    exit( 1 );
	  }
	}
	if ( k != cond_size ) {
	  printf("\n\ndifferent condition size within CPT? check input files.\n\n");
	  exit( 1 );
	}
	test++;
      }
      if ( test != rows ) {
	printf("\n\ndiff nr conditions in multi! check input files\n\n");
	exit( 1 );
      }
    } /* end if (first one ) */


    /* now, the probability entries!
     */
    for ( j = 0; j < ff->num_cpt; j++ ) {
      CPT_p[j][i] = ff->cpt_p[j];
    }

  } /* finished collectiong the multi-val var! */



  if ( gcmd_line.debug ) {
    /* print out multivar as collected.
     */
    printf("\nMULTIVAR:");
    for ( i = 0; i < size; i++ ) {
      printf("\n");
      print_ft_name(CPT_ft[i]);
    }
    printf("\nCPT rows %d:", rows);
    for ( i = 0; i < rows; i++ ) {
      for ( j = 0; j < cond_size; j++ ) {
	if ( is_real_fact(CPT_c[i][j]) ) {
	  printf("\n");
	  print_ft_name(CPT_c[i][j]);
	} else {
	  printf("\nneg ");
	  print_ft_name(gft_conn[CPT_c[i][j]].negation);
	}
      }
      printf("\nPs: ");
      for ( j = 0; j < size; j++ ) {
	printf("%f ", CPT_p[i][j]);
      }
      fflush(stdout);
    }
  }

  if ( cond_size > 0 ) {
    /* we don't want conditional probabilities.
     */
    gindimulti = FALSE;
  } else {
    /* collect the CPT_p into the parsed_weight
     */
    for ( i = 0; i < size; i++ ) {
      gft_conn[CPT_ft[i]].parsed_weight = CPT_p[0][i];
    }
  }


  /* Ok. We need <size>-1 chance variables (plus their negations),
   * and <size> clauses, for each row.
   */


  /* some syntax test: sum_row i cpts(i) has to be 1
   */
  for ( i = 0; i < rows; i++ ) {
    lll = 0;
    for ( j = 0; j < size; j++ ) {
      lll += CPT_p[i][j];
    }
    /* this is incredible. something is wrong with these fucking
     * doubles... I get 1.00000 != 1 if I don't do this shit here.
     */
    lll *= 1000000;
    if ( ((int) lll) < 999999 || ((int) lll) > 1000000 ) {
      printf("\nwarning: CPT row for multivar does not seem to add up to 1 (%lf/1000000). check input.",
	     lll);
    }
  }


  /* make space for chance var indices.
   */
  if ( rows > 0 ) {
    for ( j = 0; j < size; j++ ) {
      targetfact = CPT_ft[j];
      gft_conn[targetfact].chance_vars = ( int * ) calloc(rows, sizeof ( int ) );
      gft_conn[targetfact].num_chance_vars = rows;
    }
  }
  

  /* loop over combined CPT rows.
   */

  for ( i = 0; i < rows; i++ ) {

    /* loop over facts in mvar
     */
    for ( j = 0; j < size; j++ ) {
      targetfact = CPT_ft[j];
      negtargetfact = gft_conn[CPT_ft[j]].negation;
      if ( negtargetfact == -1 ) {
	printf("\n\nCPT multi target ft not neg translated??\n\n");
	exit( 1 );
      }

      if ( (0.0 < CPT_p[i][j]) && (CPT_p[i][j] < 1.0) ) {
	/* we need a chance var!
	 */
	chancevar = -1;
	negchancevar = -1;
	if ( j < size-1 ) {
	  /* create chance vars!
	   */
	  chancevar = gnum_ft_conn;
	  negchancevar = gnum_ft_conn + 1;
	  
	  /* the "positive" occurence.
	   */
	  gft_conn[gnum_ft_conn].negation = negchancevar;
	  /* it does not participate
	   * in any action in any way.
	   */
	  gft_conn[gnum_ft_conn].num_P = 0;
	  gft_conn[gnum_ft_conn].num_PC = 0;
	  gft_conn[gnum_ft_conn].num_C = 0;
	  gft_conn[gnum_ft_conn].num_A = 0;
	  gft_conn[gnum_ft_conn].num_D = 0;
	  gft_conn[gnum_ft_conn].srelevant = FALSE;
	  /* it *will* be unknown; special case treated below.
	   */
	  gft_conn[gnum_ft_conn].poss_U = TRUE;
	  /* use the *positive* occurence in CNFs.
	   * this means that the chance_var_for fact is real!!!
	   * (see also below)
	   */
	  if ( is_real_fact( targetfact ) ) {
	    gft_conn[gnum_ft_conn].CNF = TRUE;
	  } else {
	    gft_conn[gnum_ft_conn].CNF = FALSE;
	  }
	  /* I guess this is quite unnecessary..
	   * we aren't gonna hash this..
	   */
	  gft_conn[gnum_ft_conn].rand = random() % BIG_INT;
	  gft_conn[gnum_ft_conn].num_multi = 0;
	  gft_conn[gnum_ft_conn].multi = NULL;
	  gft_conn[gnum_ft_conn].had_multi = FALSE;
	  gft_conn[gnum_ft_conn].multiID = -1;
	  gft_conn[gnum_ft_conn].realmulti = FALSE;
	  gft_conn[gnum_ft_conn].parsed_weight = -1;
	  /* mark as chance var
	   */
	  gft_conn[gnum_ft_conn].is_state_var = FALSE;
	  gft_conn[gnum_ft_conn].chance_vars = NULL;
	  gft_conn[gnum_ft_conn].num_chance_vars = 0;
	  gft_conn[gnum_ft_conn].chance_var_for = targetfact;
	  gft_conn[gnum_ft_conn].cpt_row = i;
	  
	  /* set the WEIGHT. depends on earlier entries!
	   */
	  gft_conn[gnum_ft_conn].weighted = TRUE;
	  gft_conn[gnum_ft_conn].weight_b = 1.0;
	  for ( k = 0; k < j; k++ ) {
	    gft_conn[gnum_ft_conn].weight_b -= CPT_p[i][k];
	  }
	  gft_conn[gnum_ft_conn].weight_a = CPT_p[i][j];
	  
	  /* one more fact.
	   */
	  gnum_ft_conn++;
	  
	  /* chance var generated.
	   */
	  gft_conn[targetfact].chance_vars[i] = chancevar;
	  
	  
	  
	  /* the "negative" occurence.
	   */
	  gft_conn[gnum_ft_conn].negation = chancevar;
	  gft_conn[gnum_ft_conn].num_P = 0;
	  gft_conn[gnum_ft_conn].num_PC = 0;
	  gft_conn[gnum_ft_conn].num_C = 0;
	  gft_conn[gnum_ft_conn].num_A = 0;
	  gft_conn[gnum_ft_conn].num_D = 0;
	  gft_conn[gnum_ft_conn].srelevant = FALSE;
	  gft_conn[gnum_ft_conn].poss_U = TRUE;
	  /* use the *positive* occurence in CNFs.
	   * this means that the chance_var_for fact is real!!!
	   * (see also below)
	   */
	  if ( is_real_fact( negtargetfact ) ) {
	    gft_conn[gnum_ft_conn].CNF = TRUE;
	  } else {
	    gft_conn[gnum_ft_conn].CNF = FALSE;
	  }
	  gft_conn[gnum_ft_conn].rand = random() % BIG_INT;
	  gft_conn[gnum_ft_conn].num_multi = 0;
	  gft_conn[gnum_ft_conn].multi = NULL;
	  gft_conn[gnum_ft_conn].had_multi = FALSE;
	  gft_conn[gnum_ft_conn].multiID = -1;
	  gft_conn[gnum_ft_conn].realmulti = FALSE;
	  gft_conn[gnum_ft_conn].parsed_weight = -1;
	  gft_conn[gnum_ft_conn].is_state_var = FALSE;
	  gft_conn[gnum_ft_conn].chance_vars = NULL;
	  gft_conn[gnum_ft_conn].num_chance_vars = 0;
	  /* this guy is used only for printing;
	   * there, always say "if I am true then this guy here is also true"
	   */
	  gft_conn[gnum_ft_conn].chance_var_for = negtargetfact;
	  gft_conn[gnum_ft_conn].cpt_row = i;
	  /* set the WEIGHT. just modify (1-x) from other guy.
	   */
	  gft_conn[gnum_ft_conn].weighted = TRUE;
	  gft_conn[gnum_ft_conn].weight_a = 
	    gft_conn[chancevar].weight_b - gft_conn[chancevar].weight_a;
	  gft_conn[gnum_ft_conn].weight_b = gft_conn[chancevar].weight_b;
	  
	  gnum_ft_conn++;
	  
	  
	  /* insert this new equivalence into ini state equivalences
	   * (are these actually used later? well, who knows... don't matter anyway,
	   * no overhead other than lines of code....)
	   *
	   * keep order to have real fact on the left... just for printing beauty..
	   */
	  if ( is_real_fact( targetfact ) ) {
	    ginitial_equivalence_A[gnum_initial_equivalence] = chancevar;
	    ginitial_equivalence_notA[gnum_initial_equivalence++] = negchancevar;
	  } else {
	    ginitial_equivalence_A[gnum_initial_equivalence] = negchancevar;
	    ginitial_equivalence_notA[gnum_initial_equivalence++] = chancevar;
	  }
	} else {/* if up to 2nd last element of multi var */
	  gft_conn[targetfact].chance_vars[i] = -1;
	}/* up to 2nd last, ELSE last el of mvar */
	
	

	/* we got all chance vars that we will need.
	 * DO THE CLAUSE!
	 */
	
	/* first, allocate memory for it.
	 */
	ginitial_or[gnum_initial_or] = 
	  ( int * ) calloc( cond_size + j + 2, sizeof( int ) );
	
	
	/* first, negated condition.
	 */
	l = 0;
	for ( k = 0; k < cond_size; k++ ) {
	  ft = gft_conn[CPT_c[i][k]].negation;
	  if ( ft == -1 ) {
	    printf("\n\nCPT multi cond ft not neg translated??\n\n");
	    exit( 1 );
	  }
	  ginitial_or[gnum_initial_or][l++] = ft;
	}
	/* now, positive chance var, of this row, of all previous entries,
	 * i.e., facts in multivar.
	 */
	for ( k = 0; k < j; k++ ) {
	  /* this may be not there is chance at k is 0.
	   * verify that; if 0, then this one can be simply left out
	   * (positive occurence)
	   */
	  ft = gft_conn[CPT_ft[k]].chance_vars[i];
	  if ( ft == -1 ) {
	    if ( CPT_p[i][k] != 0 ) {
	      printf("\n\nCPT multi cond prev chance var p %f neq 0 not there?\n\n",
		     CPT_p[i][k]);
	      /* note: we neither have the chance var if p=1.0 at k.
	       * but then, p here must be 0 so we don;t get here.
	       */
	      exit( 1 );
	    }
	    continue;
	  }
	  ginitial_or[gnum_initial_or][l++] = ft;
	}
	/* our own negative chance var, if up to 2nd last
	 */
	if ( j < size-1 ) {
	  if ( negchancevar == -1 ) {
	    printf("\n\nCPT multi cond here neg-chance var not there?\n\n");
	    exit( 1 );
	  }
	  ginitial_or[gnum_initial_or][l++] = negchancevar;
	}
	/* positive target fact
	 */
	ginitial_or[gnum_initial_or][l++] = targetfact;
	ginitial_or_length[gnum_initial_or] = l;
	gnum_initial_or++;
      } /* non-0 or 1 probability for fact (mvar val) j */



      if ( 0.0 == CPT_p[i][j] ) {
	/* no chance var generated.
	 */
	gft_conn[targetfact].chance_vars[i] = -1;

	/* no chance here.
	 */
	ginitial_or[gnum_initial_or] = 
	  ( int * ) calloc( cond_size + 1, sizeof( int ) );
	/* negated condition.
	 */
	l = 0;
	for ( k = 0; k < cond_size; k++ ) {
	  ft = gft_conn[CPT_c[i][k]].negation;
	  if ( ft == -1 ) {
	    printf("\n\nCPT multi cond ft not neg translated??\n\n");
	    exit( 1 );
	  }
	  ginitial_or[gnum_initial_or][l++] = ft;
	}
	/* negative target fact
	 */
	ginitial_or[gnum_initial_or][l++] = negtargetfact;
	ginitial_or_length[gnum_initial_or] = l;
	gnum_initial_or++;
      }

      if ( 1.0 == CPT_p[i][j] ) {
	/* no chance var generated.
	 */
	gft_conn[targetfact].chance_vars[i] = -1;

	/* definitely.
	 */
	ginitial_or[gnum_initial_or] = 
	  ( int * ) calloc( cond_size + 1, sizeof( int ) );
	/* negated condition.
	 */
	l = 0;
	for ( k = 0; k < cond_size; k++ ) {
	  ft = gft_conn[CPT_c[i][k]].negation;
	  if ( ft == -1 ) {
	    printf("\n\nCPT multi cond ft not neg translated??\n\n");
	    exit( 1 );
	  }
	  ginitial_or[gnum_initial_or][l++] = ft;
	}
	/* positive target fact
	 */
	ginitial_or[gnum_initial_or][l++] = targetfact;
	ginitial_or_length[gnum_initial_or] = l;
	gnum_initial_or++;
      }

    } /* multi var contents j */



  } /* combined CPT rows i */


  /* mark the mvar facts as dealt with.
   */
  for ( i = 0; i < size; i++ ) {
    gft_conn[CPT_ft[i]].had_multi = TRUE;
  }

}


