/*********************************************************************

 * (C) Copyright 2008, University of Illinois, Urbana-Champaign

 *

 * All rights reserved. Use of this software is permitted ONLY for

 * non-commercial research purposes, and it may be copied only

 * for that use only. All copies must include this copyright message.

 * This software is made available AS IS, and neither the authors

 * nor the University of Illinois, make any warranty about the

 * software or its performance.

 *

 *********************************************************************/
/********************************************************************

 * File: dis_expressions.c

 * Description: modified from expressions.c in Metric-FF

 *

 * Authors:  Chih-wei Hsu and Benjamin W. Wah

 *

 *********************************************************************/
/*
# Copyright (C) 2004, Board of Trustees of the University of Illinois.
#
# The program is copyrighted by the University of Illinois, and should
# not be distributed without prior approval.  Commercialization of this 
# product requires prior licensing from the University of Illinois.
# Commercialization includes the integration of this code in part or 
# whole into a product for resale. 
#
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# Author: Y. X. Chen, C. W. Hsu, and B. W. Wah, University of Illinois
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
*/

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




/***********************************************************************
 * File: expressions.c
 * Description: functions for handling numerical expressions
 *
 *              - general utilities:
 *                comparisons between numbers etc.
 *
 *              - LNF compilation:
 *                normalization of expressions
 *                translation of subtractions
 *
 *              - LNF post-processing:
 *                summarization of effects
 *                encoding of non-minimal LNFs
 *
 * Author: Joerg Hoffmann 2001
 *
 *********************************************************************/ 








#include "dis_ff.h"

#include "dis_output.h"
#include "dis_memory.h"

#include "dis_expressions.h"
#include "lpg.h"














/*******************************************************
 * SIMPLE UTILITIES
 *******************************************************/
















dis_Bool dis_number_comparison_holds( dis_Comparator c, float l, float r )

{

  switch ( c ) {
  case LE:
    if ( l < r ) return dis_TRUE;
    break;
  case LEQ:
    if ( l <= r ) return dis_TRUE;
    break;
  case EQ:
    if ( l == r ) return dis_TRUE;
    break;
  case GEQ:
    if ( l >= r ) return dis_TRUE;
    break;
  case GE:
    if ( l > r ) return dis_TRUE;
    break;
  case IGUAL:
    /* technical for non-required fluents
     */
    return dis_TRUE;
  default:
    printf("\n\nillegal comparator %d in number comp holds", c);
    exit( 1 );
  }

  return dis_FALSE;

}





















/*******************************************************
 * MACHINERY Fdis_OR LNF TRANSFdis_ORMATION!!!!!!
 *******************************************************/























// PDDL3 seems useless
void reallocate_lnf_Action()
{
  dis_Action *a;
  dis_Actiondis_Effect *e;
  Lnfdis_ExpNode *lnf;
  int i, j, k = 0;

  for (a=dis_gactions;a;a=a->next)
  {
    for ( i = 0; i < a->num_lnf_preconds; i++ ) 
    {
      lnf = a->lnf_preconds_lh[i];
      k++;
      lnf->pF = (int *) realloc(lnf->pF, lnf->num_pF*sizeof(int));
      lnf->nF = (int *) realloc(lnf->nF, lnf->num_nF*sizeof(int));
      lnf->pC = (float *) realloc(lnf->pC, lnf->num_pF*sizeof(float));
      lnf->nC = (float *) realloc(lnf->nC, lnf->num_nF*sizeof(float));
    }
    for ( j = 0; j < a->num_effects; j++ ) 
    {
      e = &(a->effects[j]);
      for ( i = 0; i < e->num_lnf_conditions; i++ ) 
      {
        lnf = e->lnf_conditions_lh[i];
        k++;
        lnf->pF = (int *) realloc(lnf->pF, lnf->num_pF*sizeof(int));
        lnf->nF = (int *) realloc(lnf->nF, lnf->num_nF*sizeof(int));
        lnf->pC = (float *) realloc(lnf->pC, lnf->num_pF*sizeof(float));
        lnf->nC = (float *) realloc(lnf->nC, lnf->num_nF*sizeof(float));
        lnf = e->lnf_effects_rh[i];
        k++;
        lnf->pF = (int *) realloc(lnf->pF, lnf->num_pF*sizeof(int));
        lnf->nF = (int *) realloc(lnf->nF, lnf->num_nF*sizeof(int));
        lnf->pC = (float *) realloc(lnf->pC, lnf->num_pF*sizeof(float));
        lnf->nC = (float *) realloc(lnf->nC, lnf->num_nF*sizeof(float));
      }
    }
  }
}

dis_Bool dis_transform_to_LNF( void )

{

  if ( !dis_is_linear_task() ) {
    return dis_FALSE;
  }

  dis_normalize_expressions();
  if ( dis_gcmd_line.display_info == 121 ) {
    printf("\n\nnormalized expressions representation is:\n\n");
    dis_print_lnf_representation();
  }

  dis_translate_subtractions();
  if ( dis_gcmd_line.display_info == 122 ) {
    printf("\n\nLNF : translated subtractions representation is:\n\n");
    dis_print_lnf_representation();
  }

  /* LNF computed. start post-processing.
   */

  /* do same-cond effects etc. summarization here so as to have
   * as tight as possible an encoded LNF representation.
   */
   if (GpG.SearchModal != 7)
   dis_summarize_effects();
  if ( dis_gcmd_line.display_info == 123 ) {
    printf("\n\nLNF - summarized effects representation is:\n\n");
    dis_print_lnf_representation();
  }

  dis_encode_lfns_as_artificial_fluents();
  /* optimization is translated into minimizing
   * effect costs... here, determine the cost that
   * each effect has.
   *
   * returns dis_TRUE if a non-trivial optimization expression
   * could be established.
   */
  if ( dis_setup_effect_costs() ) {
    if ( dis_gcmd_line.display_info ) {
// PDDL3
//      printf("\nmetric established (normalized to minimize): ");
//      dis_print_Lnfdis_ExpNode( &dis_glnf_metric );
    }
    dis_goptimization_established = dis_TRUE;
  }
  
  if ( dis_gcmd_line.display_info == 124 ) {
    printf("\n\nencoded LNF representation is:\n\n");
    dis_print_lnf_representation();
  }
  
  reallocate_lnf_Action();

  return dis_TRUE;

}



/* simple syntax check
 */
dis_Bool dis_is_linear_task( void )

{

  dis_Action *a;
  dis_Actiondis_Effect *e;
  int i, j;

  for ( a = dis_gactions; a; a = a->next ) {
    /* preconds
     */
    for ( i = 0; i < a->num_numeric_preconds; i++ ) {
      if ( !dis_is_linear_expression( a->numeric_preconds_lh[i] ) ) {
	return dis_FALSE;
      }
      if ( !dis_is_linear_expression( a->numeric_preconds_rh[i] ) ) {
	return dis_FALSE;
      }
    }

    /* effects
     */
    for ( i = 0; i < a->num_effects; i++ ) {
      e = &(a->effects[i]);
      for ( j = 0; j < e->num_numeric_conditions; j++ ) {
	if ( !dis_is_linear_expression( e->numeric_conditions_lh[j] ) ) {
	  return dis_FALSE;
	}
	if ( !dis_is_linear_expression( e->numeric_conditions_rh[j] ) ) {
	  return dis_FALSE;
	}
      }

      if ( e->illegal ) {
	/* we don't care whether that one's ok or not-
	 * it won't be applied anyway.
	 */
	continue;
      }

      for ( j = 0; j < e->num_numeric_effects; j++ ) {
	if ( e->numeric_effects_neft[j] != INCREASE &&
	     e->numeric_effects_neft[j] != DECREASE &&
	     e->numeric_effects_neft[j] != ASSIGN ) {
	  return dis_FALSE;
	}
	if ( !dis_is_linear_expression( e->numeric_effects_rh[j] ) ) {
	  return dis_FALSE;
	}
      }
    }
  }
  

  /* goal condition also...
   */

  for ( i = 0; i < dis_gnum_numeric_goal; i++ ) {
    if ( !dis_is_linear_expression( dis_gnumeric_goal_lh[i] ) ) {
      return dis_FALSE;
    }
    if ( !dis_is_linear_expression( dis_gnumeric_goal_rh[i] ) ) {
      return dis_FALSE;
    }
  }

  if ( dis_gmetric != NULL ) {
    if ( !dis_is_linear_expression( dis_gmetric ) ) {
      if ( dis_gcmd_line.display_info ) {
	printf("\nwarning: metric is no linear expression. defaulting to plan length.");
      }
      dis_free_dis_ExpNode( dis_gmetric );
      dis_gmetric = NULL;      
    }
  }

  return dis_TRUE;

}



dis_Bool dis_is_linear_expression( dis_ExpNode *n )

{

  switch ( n->connective ) {
  case MU:
    if ( !dis_is_linear_expression( n->leftson ) ||
	 !dis_is_linear_expression( n->rightson ) ) {
      return dis_FALSE;
    }
    if ( n->leftson->connective != NUMBER &&
	 n->rightson->connective != NUMBER ) {
      return dis_FALSE;
    }
    break;
  case DI:
    if ( !dis_is_linear_expression( n->leftson ) ||
	 n->rightson->connective != NUMBER ) {
      return dis_FALSE;
    }
    break;
  case AD:
  case SU:
    if ( !dis_is_linear_expression( n->leftson ) ||
	 !dis_is_linear_expression( n->rightson ) ) {
      return dis_FALSE;
    }
    break;
  case MINUS:
    if ( !dis_is_linear_expression( n->son ) ) {
      return dis_FALSE;
    }
    break;
  case NUMBER:
  case FHEAD:
    break;
  default:
    printf("\n\nis linear exp: wrong specifier %d",
	   n->connective);
    exit( 1 );
  }

  return dis_TRUE;

}


dis_Action* pos_lnf_action(int op)
{
	return NULL;
}

void dis_print_lnf_representation( void )

{

  int i;
  dis_Action *a;

  for ( i = 0; i < dis_gnum_operators; i++ ) {
    printf("\n\n------------------operator %s-----------\n\n", dis_goperators[i]->name);
    for ( a = dis_gactions; a; a = a->next ) {
      if ( ( !a->norm_operator &&
	     !a->pseudo_action ) ||
	   ( a->norm_operator && 
	     a->norm_operator->operator != dis_goperators[i] ) ||
	   ( a->pseudo_action &&
	     a->pseudo_action->operator != dis_goperators[i] ) ) {
	continue;
      }
      dis_print_lnf_dis_Action( a );
    }
  }
  printf("\n\n--------------------GOAL REACHED ops-----------\n\n");
  for ( a = dis_gactions; a; a = a->next ) {
    if ( !a->norm_operator &&
	 !a->pseudo_action ) {
      dis_print_lnf_dis_Action( a );
    }
  }
  
  printf("\n\ninitial state is:\n\n");
  dis_print_dis_State( dis_ginitial_state );
  
  printf("\n\ngoal is:\n\n");
  for ( i = 0; i < dis_gnum_logic_goal; i++ ) {
    dis_print_ft_name( dis_glogic_goal[i] );
    printf("\n");
  }
  for ( i = 0; i < dis_gnum_lnf_goal; i++ ) {
    switch ( dis_glnf_goal_comp[i] ) {
    case GEQ:
      printf("(>= ");
      break;
    case GE:
      printf("(> ");
      break;
    default:
      printf("\nwrong comparator in lnf goal %d\n\n", dis_glnf_goal_comp[i]);
      exit( 1 );
    }
    dis_print_Lnfdis_ExpNode( dis_glnf_goal_lh[i] );
    printf(" %f", dis_glnf_goal_rh[i]);
    printf(")\n");
  }

  if ( dis_gmetric ) {
    printf("\n\nmetric is (minimize) (constant part skipped):\n");
    dis_print_Lnfdis_ExpNode( &dis_glnf_metric );
  } else {
    printf("\n\nmetric: none, i.e. plan length\n");
  }

}


















/*******************************************************
 * SUBPART I: Ndis_ORMALIZE THE dis_EXPRESSIONS
 *******************************************************/

















/* local globals.
 */

dis_Comparator exp_exp_lcomp;

int exp_lF[MAX_EXP_LNUM_F];
float exp_lC[MAX_EXP_LNUM_F];
//int exp_lF[dis_MAX_LNF_F];
//float exp_lC[dis_MAX_LNF_F];
int exp_lnum_F;

float exp_lc;











void dis_normalize_expressions( void )

{

  dis_Action *a, *p, *t;
  dis_Actiondis_Effect *e;
  int i, j, k;
  dis_Bool eq;
  Lnfdis_ExpNode *lnf;

  /* first, pre-normalize all the expressions, i.e. translate
   * divisions, and push muliplications downwards.
   */
  for ( i = 0; i < dis_gnum_numeric_goal; i++ ) {
    if ( !dis_translate_divisions( &(dis_gnumeric_goal_lh[i]) ) ) {
      printf("\n\nff: division by zero in goal. no plan will solve it.\n\n");
      exit( 1 );
    }
    dis_push_multiplications_down( &(dis_gnumeric_goal_lh[i]) );
    if ( !dis_translate_divisions( &(dis_gnumeric_goal_rh[i]) ) ) {
      printf("\n\nff: division by zero in goal. no plan will solve it.\n\n");
      exit( 1 );
    }
    dis_push_multiplications_down( &(dis_gnumeric_goal_rh[i]) );
  }

  a = dis_gactions; p = NULL;
  while ( a ) {
    for ( i = 0; i < a->num_numeric_preconds; i++ ) {
      if ( !dis_translate_divisions( &(a->numeric_preconds_lh[i]) ) ) break;
      dis_push_multiplications_down( &(a->numeric_preconds_lh[i]) );
      if ( !dis_translate_divisions( &(a->numeric_preconds_rh[i]) ) ) break;
      dis_push_multiplications_down( &(a->numeric_preconds_rh[i]) );
    }
    if ( i < a->num_numeric_preconds ) {
      if ( dis_gcmd_line.display_info ) {
	printf("\nwarning: division by zero in precond of ");
	dis_print_dis_Action_name( a );
	printf(". skipping action.");
      }
      dis_gnum_actions--;
      if ( p ) {
	p->next = a->next;
	t = a;
	a = a->next;
	t->next = dis_gtrash_actions;
	dis_gtrash_actions = t;
      } else {
	dis_gactions = a->next;
	t = a;
	a = a->next;
	t->next = dis_gtrash_actions;
	dis_gtrash_actions = t;
      }
      continue;
    }

    for ( i = 0; i < a->num_effects; i++ ) {
      e = &(a->effects[i]);

      for ( j = 0; j < e->num_numeric_conditions; j++ ) {
  	if ( !dis_translate_divisions( &(e->numeric_conditions_lh[j]) ) ) break;
	dis_push_multiplications_down( &(e->numeric_conditions_lh[j]) );
	if ( !dis_translate_divisions( &(e->numeric_conditions_rh[j]) ) ) break;
	dis_push_multiplications_down( &(e->numeric_conditions_rh[j]) );
      }
      if ( j < e->num_numeric_conditions ) break;

      if ( e->illegal ) {
	continue;
      }

      for ( j = 0; j < e->num_numeric_effects; j++ ) {
	if ( !dis_translate_divisions( &(e->numeric_effects_rh[j]) ) ) break;
	dis_push_multiplications_down( &(e->numeric_effects_rh[j]) );
      }
      if ( j < e->num_numeric_effects ) {
	if ( dis_gcmd_line.display_info ) {
	  printf("\nwarning: division by zero in effect rh of ");
	  dis_print_dis_Action_name( a );
	  printf(". marking effect as illegal.");
	}
	e->illegal = dis_TRUE;
      }
    }
    if ( i < a->num_effects ) {
      if ( dis_gcmd_line.display_info ) {
	printf("\nwarning: division by zero in effect cond of ");
	dis_print_dis_Action_name( a );
	printf(". skipping action.");
      }
      dis_gnum_actions--;
      if ( p ) {
	p->next = a->next;
	t = a;
	a = a->next;
	t->next = dis_gtrash_actions;
	dis_gtrash_actions = t;
      } else {
	dis_gactions = a->next;
	t = a;
	a = a->next;
	t->next = dis_gtrash_actions;
	dis_gtrash_actions = t;
      }
      continue;
    }
    
    p = a;
    a = a->next;
  }
  if ( dis_gmetric != NULL ) {
    if ( !dis_translate_divisions( &dis_gmetric ) ) {
      if ( dis_gcmd_line.display_info ) {
	printf("\nwarning: division by zero in metric. replaced with plan length.");
      }
      dis_free_dis_ExpNode( dis_gmetric );
      dis_gmetric = NULL;
    }
    dis_push_multiplications_down( &dis_gmetric );
  }

  /* now, collect the normalized representations of all expressions.
   */
  for ( a = dis_gactions; a; a = a->next ) {
    /* preconds
     */
    a->lnf_preconds_comp = ( dis_Comparator * ) calloc( dis_MAX_LNF_dis_COMPS, sizeof( dis_Comparator ) );
    a->lnf_preconds_lh = ( Lnfdis_ExpNode_pointer * ) calloc( dis_MAX_LNF_dis_COMPS, sizeof( Lnfdis_ExpNode_pointer ) );
    a->lnf_preconds_rh = ( float * ) calloc( dis_MAX_LNF_dis_COMPS, sizeof( float ) );
    a->num_lnf_preconds = 0;
    for ( i = 0; i < a->num_numeric_preconds; i++ ) {
      if ( a->num_lnf_preconds == dis_MAX_LNF_dis_COMPS ) {
	printf("\n\nincrease dis_MAX_LNF_dis_COMPS! currently %d\n\n", dis_MAX_LNF_dis_COMPS);
	exit( 1 );
      }
      eq = dis_FALSE;
      if ( a->numeric_preconds_comp[i] == EQ ) {
	eq = dis_TRUE;
	a->numeric_preconds_comp[i] = LEQ;
      }
      dis_put_comp_into_normalized_locals( a->numeric_preconds_comp[i],
				       a->numeric_preconds_lh[i],
				       a->numeric_preconds_rh[i] );
      a->lnf_preconds_comp[a->num_lnf_preconds] = exp_exp_lcomp;
      a->lnf_preconds_lh[a->num_lnf_preconds] = dis_new_Lnfdis_ExpNode();
      lnf = a->lnf_preconds_lh[a->num_lnf_preconds];
      for ( j = 0; j < exp_lnum_F; j++ ) {
	if ( exp_lC[j] == 0 ) continue;
	if ( exp_lC[j] > 0 ) {
	  lnf->pF[lnf->num_pF] = exp_lF[j];
	  lnf->pC[lnf->num_pF++] = exp_lC[j];
	} else {
	  lnf->nF[lnf->num_nF] = exp_lF[j];
	  lnf->nC[lnf->num_nF++] = (-1) * exp_lC[j];
	}
      }
      a->lnf_preconds_rh[a->num_lnf_preconds] = exp_lc;
      a->num_lnf_preconds++;
      if ( eq ) {
	if ( a->num_lnf_preconds == dis_MAX_LNF_dis_COMPS ) {
	  printf("\n\nincrease dis_MAX_LNF_dis_COMPS! currently %d\n\n", dis_MAX_LNF_dis_COMPS);
	  exit( 1 );
	}
	a->numeric_preconds_comp[i] = EQ;
	dis_put_comp_into_normalized_locals( GEQ,
					 a->numeric_preconds_lh[i],
					 a->numeric_preconds_rh[i] );
	a->lnf_preconds_comp[a->num_lnf_preconds] = exp_exp_lcomp;
	a->lnf_preconds_lh[a->num_lnf_preconds] = dis_new_Lnfdis_ExpNode();
	lnf = a->lnf_preconds_lh[a->num_lnf_preconds];
	for ( j = 0; j < exp_lnum_F; j++ ) {
	  if ( exp_lC[j] == 0 ) continue;
	  if ( exp_lC[j] > 0 ) {
	    lnf->pF[lnf->num_pF] = exp_lF[j];
	    lnf->pC[lnf->num_pF++] = exp_lC[j];
	  } else {
	    lnf->nF[lnf->num_nF] = exp_lF[j];
	    lnf->nC[lnf->num_nF++] = (-1) * exp_lC[j];
	  }
	}
	a->lnf_preconds_rh[a->num_lnf_preconds] = exp_lc;
	a->num_lnf_preconds++;
      }
    }

    /* effects
     */
    for ( i = 0; i < a->num_effects; i++ ) {
      e = &(a->effects[i]);

      e->lnf_conditions_comp = ( dis_Comparator * ) calloc( dis_MAX_LNF_dis_COMPS, sizeof( dis_Comparator ) );
      e->lnf_conditions_lh = ( Lnfdis_ExpNode_pointer * ) calloc( dis_MAX_LNF_dis_COMPS, sizeof( Lnfdis_ExpNode_pointer ) );
      e->lnf_conditions_rh = ( float * ) calloc( dis_MAX_LNF_dis_COMPS, sizeof( float ) );
      e->num_lnf_conditions = 0;
      for ( j = 0; j < e->num_numeric_conditions; j++ ) {
	if ( e->num_lnf_conditions == dis_MAX_LNF_dis_COMPS ) {
	  printf("\n\nincrease dis_MAX_LNF_dis_COMPS! currently %d\n\n", dis_MAX_LNF_dis_COMPS);
	  exit( 1 );
	}
	eq = dis_FALSE;
	if ( e->numeric_conditions_comp[j] == EQ ) {
	  eq = dis_TRUE;
	  e->numeric_conditions_comp[j] = LEQ;
	}
	dis_put_comp_into_normalized_locals( e->numeric_conditions_comp[j],
					 e->numeric_conditions_lh[j],
					 e->numeric_conditions_rh[j] );
	e->lnf_conditions_comp[e->num_lnf_conditions] = exp_exp_lcomp;
	e->lnf_conditions_lh[e->num_lnf_conditions] = dis_new_Lnfdis_ExpNode();
	lnf = e->lnf_conditions_lh[e->num_lnf_conditions];
	for ( k = 0; k < exp_lnum_F; k++ ) {
	  if ( exp_lC[k] == 0 ) continue;
	  if ( exp_lC[k] > 0 ) {
	    lnf->pF[lnf->num_pF] = exp_lF[k];
	    lnf->pC[lnf->num_pF++] = exp_lC[k];
	  } else {
	    lnf->nF[lnf->num_nF] = exp_lF[k];
	    lnf->nC[lnf->num_nF++] = (-1) * exp_lC[k];
	  }
	}
	e->lnf_conditions_rh[e->num_lnf_conditions] = exp_lc;
	e->num_lnf_conditions++;
	if ( eq ) {
	  if ( e->num_lnf_conditions == dis_MAX_LNF_dis_COMPS ) {
	    printf("\n\nincrease dis_MAX_LNF_dis_COMPS! currently %d\n\n", dis_MAX_LNF_dis_COMPS);
	    exit( 1 );
	  }
	  e->numeric_conditions_comp[j] = EQ;
	  dis_put_comp_into_normalized_locals( GEQ,
					   e->numeric_conditions_lh[j],
					   e->numeric_conditions_rh[j] );
	  e->lnf_conditions_comp[e->num_lnf_conditions] = exp_exp_lcomp;
	  e->lnf_conditions_lh[e->num_lnf_conditions] = dis_new_Lnfdis_ExpNode();
	  lnf = e->lnf_conditions_lh[e->num_lnf_conditions];
	  for ( k = 0; k < exp_lnum_F; k++ ) {
	    if ( exp_lC[k] == 0 ) continue;
	    if ( exp_lC[k] > 0 ) {
	      lnf->pF[lnf->num_pF] = exp_lF[k];
	      lnf->pC[lnf->num_pF++] = exp_lC[k];
	    } else {
	      lnf->nF[lnf->num_nF] = exp_lF[k];
	      lnf->nC[lnf->num_nF++] = (-1) * exp_lC[k];
	    }
	  }
	  e->lnf_conditions_rh[e->num_lnf_conditions] = exp_lc;
	  e->num_lnf_conditions++;
	}
      }

      if ( e->illegal ) {
	/* we do have the LNF to know whether the effect appears.
	 * if it does, then this one is illegal anyway, remembered
	 * in inst final due to undefined fl access.
	 *
	 * if it is LEGAL, then all fluents we're gonna find and
	 * collect below are relevant!!!
	 */
	continue;
      }
      
      e->lnf_effects_neft = ( dis_Numericdis_EffectType * ) calloc( dis_MAX_LNF_EFFS, sizeof( dis_Numericdis_EffectType ) );
      e->lnf_effects_fl = ( int * ) calloc( dis_MAX_LNF_EFFS, sizeof( int ) );
      e->lnf_effects_rh = ( Lnfdis_ExpNode_pointer * ) calloc( dis_MAX_LNF_EFFS, sizeof( Lnfdis_ExpNode_pointer ) );
      e->num_lnf_effects = 0;
      for ( j = 0; j < e->num_numeric_effects; j++ ) {
	if ( e->num_lnf_effects == dis_MAX_LNF_EFFS ) {
	  printf("\n\nincrease dis_MAX_LNF_EFFS! currently %d\n\n", dis_MAX_LNF_EFFS);
	  exit( 1 );
	}
	e->lnf_effects_neft[e->num_lnf_effects] = e->numeric_effects_neft[j];
	e->lnf_effects_fl[e->num_lnf_effects] = e->numeric_effects_fl[j];
	exp_lnum_F = 0;
	exp_lc = 0;
	if ( e->lnf_effects_neft[e->num_lnf_effects] == DECREASE ) {
	  dis_collect_normalized_locals( e->numeric_effects_rh[j], dis_FALSE );
	  e->lnf_effects_neft[e->num_lnf_effects] = INCREASE;
	} else {
	  dis_collect_normalized_locals( e->numeric_effects_rh[j], dis_TRUE );
	}
	e->lnf_effects_rh[e->num_lnf_effects] = dis_new_Lnfdis_ExpNode();
	lnf = e->lnf_effects_rh[e->num_lnf_effects];
	for ( k = 0; k < exp_lnum_F; k++ ) {
	  if ( exp_lC[k] == 0 ) continue;
	  if ( exp_lC[k] > 0 ) {
	    lnf->pF[lnf->num_pF] = exp_lF[k];
	    lnf->pC[lnf->num_pF++] = exp_lC[k];
	  } else {
	    lnf->nF[lnf->num_nF] = exp_lF[k];
	    lnf->nC[lnf->num_nF++] = (-1) * exp_lC[k];
	  }
	}
	e->lnf_effects_rh[e->num_lnf_effects]->c = exp_lc;
	e->num_lnf_effects++;
      }
    }
  }

  /* goal condition also...
   */
  dis_glnf_goal_comp = ( dis_Comparator * ) calloc( dis_MAX_LNF_dis_COMPS, sizeof( dis_Comparator ) );
  dis_glnf_goal_lh = ( Lnfdis_ExpNode_pointer * ) calloc( dis_MAX_LNF_dis_COMPS, sizeof( Lnfdis_ExpNode_pointer ) );
  dis_glnf_goal_rh = ( float * ) calloc( dis_MAX_LNF_dis_COMPS, sizeof( float ) );
  dis_gnum_lnf_goal = 0;
  for ( i = 0; i < dis_gnum_numeric_goal; i++ ) {
    if ( dis_gnum_lnf_goal == dis_MAX_LNF_dis_COMPS ) {
      printf("\n\nincrease dis_MAX_LNF_dis_COMPS! currently %d\n\n", dis_MAX_LNF_dis_COMPS);
      exit( 1 );
    }
    eq = dis_FALSE;
    if ( dis_gnumeric_goal_comp[i] == EQ ) {
      eq = dis_TRUE;
      dis_gnumeric_goal_comp[i] = LEQ;
    }
    dis_put_comp_into_normalized_locals( dis_gnumeric_goal_comp[i],
				     dis_gnumeric_goal_lh[i],
				     dis_gnumeric_goal_rh[i] );
    dis_glnf_goal_comp[dis_gnum_lnf_goal] = exp_exp_lcomp;
    dis_glnf_goal_lh[dis_gnum_lnf_goal] = dis_new_Lnfdis_ExpNode();
    lnf = dis_glnf_goal_lh[dis_gnum_lnf_goal];
    for ( j = 0; j < exp_lnum_F; j++ ) {
      if ( exp_lC[j] == 0 ) continue;
      if ( exp_lC[j] > 0 ) {
	lnf->pF[lnf->num_pF] = exp_lF[j];
	lnf->pC[lnf->num_pF++] = exp_lC[j];
      } else {
	lnf->nF[lnf->num_nF] = exp_lF[j];
	lnf->nC[lnf->num_nF++] = (-1) * exp_lC[j];
      }
    }
    dis_glnf_goal_rh[dis_gnum_lnf_goal] = exp_lc;
    dis_gnum_lnf_goal++;
    if ( eq ) {
      if ( dis_gnum_lnf_goal == dis_MAX_LNF_dis_COMPS ) {
	printf("\n\nincrease dis_MAX_LNF_dis_COMPS! currently %d\n\n", dis_MAX_LNF_dis_COMPS);
	exit( 1 );
      }
      dis_gnumeric_goal_comp[i] = EQ;
      dis_put_comp_into_normalized_locals( GEQ,
				       dis_gnumeric_goal_lh[i],
				       dis_gnumeric_goal_rh[i] );
      dis_glnf_goal_comp[dis_gnum_lnf_goal] = exp_exp_lcomp;
      dis_glnf_goal_lh[dis_gnum_lnf_goal] = dis_new_Lnfdis_ExpNode();
      lnf = dis_glnf_goal_lh[dis_gnum_lnf_goal];
      for ( j = 0; j < exp_lnum_F; j++ ) {
	if ( exp_lC[j] == 0 ) continue;
	if ( exp_lC[j] > 0 ) {
	  lnf->pF[lnf->num_pF] = exp_lF[j];
	  lnf->pC[lnf->num_pF++] = exp_lC[j];
	} else {
	  lnf->nF[lnf->num_nF] = exp_lF[j];
	  lnf->nC[lnf->num_nF++] = (-1) * exp_lC[j];
	}
      }
      dis_glnf_goal_rh[dis_gnum_lnf_goal] = exp_lc;
      dis_gnum_lnf_goal++;
    }
  }
  /* metric...
   */
  exp_lnum_F = 0;
  exp_lc = 0;
  dis_glnf_metric.num_pF = 0;
  dis_glnf_metric.num_nF = 0;
  dis_glnf_metric.c = 0;
  dis_collect_normalized_locals( dis_gmetric, dis_TRUE );
  dis_glnf_metric.pF = (int *) malloc(exp_lnum_F*sizeof(int));
  dis_glnf_metric.pC = (float *) malloc(exp_lnum_F*sizeof(int));
  dis_glnf_metric.nF = (int *) malloc(exp_lnum_F*sizeof(int));
  dis_glnf_metric.nC = (float *) malloc(exp_lnum_F*sizeof(int));
  lnf = &dis_glnf_metric;
  lnf->c = exp_lc;
  for ( j = 0; j < exp_lnum_F; j++ ) {
    if ( exp_lC[j] == 0 ) continue;
    if ( exp_lC[j] > 0 ) {
      lnf->pF[lnf->num_pF] = exp_lF[j];
      lnf->pC[lnf->num_pF++] = exp_lC[j];
    } else {
      lnf->nF[lnf->num_nF] = exp_lF[j];
      lnf->nC[lnf->num_nF++] = (-1) * exp_lC[j];
    }
  }


}



dis_Bool dis_translate_divisions( dis_ExpNode **n )

{

  dis_ExpNode *tmp;

  /* "dirty": also normalize multiplications so that the constant
   * is always on the left hand side ---
   * simplifies function below a lot.
   */
  switch ( (*n)->connective ) {
  case DI:
    /* rightson is number due to syntax check.
     */
    if ( (*n)->rightson->value == 0 ) {
      /* what needs to be done we can only decide further up.
       */
      printf("\nwarning: division by zero.");
      return dis_FALSE;
    }
    if ( !dis_translate_divisions( &((*n)->leftson) ) ) return dis_FALSE;
    (*n)->connective = MU;
    (*n)->rightson->value = 1 / (*n)->rightson->value;
    tmp = (*n)->rightson;
    (*n)->rightson = (*n)->leftson;
    (*n)->leftson = tmp;
    break;
  case MU:
    if ( !dis_translate_divisions( &((*n)->leftson) ) ) return dis_FALSE;
    if ( !dis_translate_divisions( &((*n)->rightson) ) ) return dis_FALSE;
    if ( (*n)->rightson->connective == NUMBER ) {
      tmp = (*n)->rightson;
      (*n)->rightson = (*n)->leftson;
      (*n)->leftson = tmp;      
    }
    break;
  case AD:
  case SU:
    if ( !dis_translate_divisions( &((*n)->leftson) ) ) return dis_FALSE;
    if ( !dis_translate_divisions( &((*n)->rightson) ) ) return dis_FALSE;
    break;
  case MINUS:
    if ( !dis_translate_divisions( &((*n)->son) ) ) return dis_FALSE;
    break;
  case NUMBER:
  case FHEAD:
    break;
  default:
    printf("\n\ntranslate divisions: wrong specifier %d",
	   (*n)->connective);
    exit( 1 );
  }

  return dis_TRUE;

}



void dis_push_multiplications_down( dis_ExpNode **n )

{

  dis_ExpNode *tmp1, *tmp2;

  switch ( (*n)->connective ) {
  case MU:
    /* due to syntax check, at least one of sons is number,
     * 
     * due to above, it's the left one.
     * dis_NOTE that this invariant is kept true troughout the
     * modifications done here.
     */
    if ( (*n)->rightson->connective == NUMBER ) {
      (*n)->connective = NUMBER;
      (*n)->value = (*n)->leftson->value * (*n)->rightson->value;
      dis_free_dis_ExpNode( (*n)->leftson );
      dis_free_dis_ExpNode( (*n)->rightson );
      (*n)->leftson = NULL;
      (*n)->rightson = NULL;
      break;	
    }
    if ( (*n)->rightson->connective == FHEAD ) {
      (*n)->connective = FHEAD;
      (*n)->fl = (*n)->rightson->fl;
      (*n)->c = (*n)->leftson->value;
      dis_free_dis_ExpNode( (*n)->leftson );
      dis_free_dis_ExpNode( (*n)->rightson );
      (*n)->leftson = NULL;
      (*n)->rightson = NULL;
      break;
    }
    if ( (*n)->rightson->connective == MINUS ) {
      (*n)->connective = MINUS;
      (*n)->son = (*n)->rightson;
      (*n)->son->connective = MU;
      (*n)->son->leftson = (*n)->leftson;
      (*n)->son->rightson = (*n)->rightson->son;
      (*n)->rightson = NULL;
      (*n)->leftson = NULL;
      (*n)->son->son = NULL;
      dis_push_multiplications_down( &((*n)->son) );
      break;
    }
    if ( (*n)->rightson->connective == MU ) {
      (*n)->leftson->value *= (*n)->rightson->leftson->value;
      tmp1 = (*n)->rightson->rightson;
      (*n)->rightson->rightson = NULL;
      dis_free_dis_ExpNode( (*n)->rightson );
      (*n)->rightson = tmp1;
      dis_push_multiplications_down( n );
      break;
    }
    
    /* rigthson is either AD or SU
     */
    tmp1 = dis_new_dis_ExpNode( NUMBER );
    tmp2 = dis_new_dis_ExpNode( NUMBER );
    tmp1->value = (*n)->leftson->value;
    tmp2->value = (*n)->leftson->value;
    
    (*n)->connective = (*n)->rightson->connective;
    (*n)->leftson->connective = MU;
    (*n)->rightson->connective = MU;
    (*n)->leftson->leftson = tmp1;
    (*n)->leftson->rightson = (*n)->rightson->leftson;      
    (*n)->rightson->leftson = tmp2;

    dis_push_multiplications_down( &((*n)->leftson) );
    dis_push_multiplications_down( &((*n)->rightson) );
    break;
  case AD:
  case SU:
    dis_push_multiplications_down( &((*n)->leftson) );
    dis_push_multiplications_down( &((*n)->rightson) );
    break;
  case MINUS:
    dis_push_multiplications_down( &((*n)->son) );
    break;
  case NUMBER:
  case FHEAD:
    break;
  default:
    printf("\n\ntranslate divisions: wrong specifier %d",
	   (*n)->connective);
    exit( 1 );
  }

}



void dis_put_comp_into_normalized_locals( dis_Comparator comp,
				      dis_ExpNode *lh,
				      dis_ExpNode *rh )

{

  dis_ExpNode *tmp;

  tmp = dis_new_dis_ExpNode( SU );

  /* initialisation of normalized locals
   */
  exp_lnum_F = 0;
  exp_lc = 0;

  exp_exp_lcomp = comp;

  /* if comparison is LE or LEQ, then subtract
   * left hand side from right hand side to obtain
   * new left hand side.
   */
  if ( exp_exp_lcomp == LE ) {
    tmp->leftson = rh;
    tmp->rightson = lh;
    dis_collect_normalized_locals( tmp, dis_TRUE );
    exp_exp_lcomp = GE;
    /* "subtract" the constant to get it to the right hand
     * side.
     */
    exp_lc *= (-1);
    xfree( tmp );
    return;
  }
  if ( exp_exp_lcomp == LEQ ) {
    tmp->leftson = rh;
    tmp->rightson = lh;
    dis_collect_normalized_locals( tmp, dis_TRUE );
    exp_exp_lcomp = GEQ;
    exp_lc *= (-1);
    xfree( tmp );
    return;
  }

  /* otherwise, subtract right hand side from left hand side.
   */
  tmp->leftson = lh;
  tmp->rightson = rh;
  dis_collect_normalized_locals( tmp, dis_TRUE );
  exp_lc *= (-1);
  xfree( tmp );

}



void dis_collect_normalized_locals( dis_ExpNode *n, dis_Bool positive )

{

  dis_Bool negative = positive ? dis_FALSE : dis_TRUE;
  int i;

  if ( !n ) return;

  switch ( n->connective ) {
  case AD:
    dis_collect_normalized_locals( n->leftson, positive );
    dis_collect_normalized_locals( n->rightson, positive );
    break;
  case SU:
    dis_collect_normalized_locals( n->leftson, positive );
    dis_collect_normalized_locals( n->rightson, negative );
    break;
  case MINUS:
    dis_collect_normalized_locals( n->son, negative );
    break;
  case NUMBER:
    if ( positive ) {
      exp_lc += n->value;
    } else {
      exp_lc -= n->value;
    }
    break;
  case FHEAD:
    if ( n->fl < 0 && n->fl != -2 ) {
      printf("\n\ncollecting non-relevant fluent for LNF!!\n\n");
      exit( 1 );
    }
    for ( i = 0; i < exp_lnum_F; i++ ) {
      if ( exp_lF[i] == n->fl ) break;
    }
    if ( i < exp_lnum_F ) {
      exp_lC[i] += positive ? n->c : ((-1) * n->c);
    } else { 
      if ( exp_lnum_F == MAX_EXP_LNUM_F ) {
	printf("\n\nincrease MAX_EXP_LNUM_F! currently %d\n\n", MAX_EXP_LNUM_F);
	exit( 1 );
      }
      exp_lF[exp_lnum_F] = n->fl;
      exp_lC[exp_lnum_F] = positive ? n->c : ((-1) * n->c);
      exp_lnum_F++;
    }
    break;
  default:
    printf("\n\ndis_collect_normalized_locals: wrong specifier %d",
	   n->connective);
    exit( 1 );
  }

}





















/*******************************************************
 * SUBPART II: TRANSLATE THE SUBTRACTIONS
 *******************************************************/















/* local globals.
 */

int lminus_fluent[dis_MAX_RELEVANT_FLUENTS];












void dis_translate_subtractions( void )

{

  int i, fl;

  /* minus_fluent[fl] gives the number of the fluent that
   * takes on the negative value to fl, or -1 if there is
   * no such fluent.
   */
  for ( i = 0; i < dis_MAX_RELEVANT_FLUENTS; i++ ) {
    lminus_fluent[i] = -1;
  }

  while ( dis_TRUE ) {
    /* ex fl \in nF for pre, cond, eff or goal?
     */
    if ( !dis_ex_fl_in_nF_of_pre_cond_eff_goal( &fl ) ) {
      /* no --> we are finished.
       */
      break;
    }
    if ( fl < 0 ) {
      if ( fl != -2 ) {
	printf("\n\nnon-relevant fluent in non-illegal part!\n\n");
	exit( 1 );
      } else {
	printf("\n\ntotal-time occurs negatively in metric! sorry, but I'm skipping that rubbish\n\n");
	dis_glnf_metric.num_pF = 0;
	dis_glnf_metric.num_nF = 0;
	dis_gcmd_line.optimize = dis_FALSE;
	continue;
      }
    }
    /* set the new number and name, incrementing 
     * dis_gnum_relevant_fluents, and setting
     * minus_fluent value for both directions.
     */
    dis_introduce_minus_fluent( fl );
    /* replace all occurences in effects and conds and goals
     */
    dis_replace_fl_in_nF_with_minus_fl( fl );
    /* set the initial value of the new fluent
     */
    dis_set_minus_fl_initial( fl );
    /* adjust the effects accordingly
     */
    dis_introduce_minus_fl_effects( fl );
  }

}



dis_Bool dis_ex_fl_in_nF_of_pre_cond_eff_goal( int *fl )

{

  dis_Action *a;
  dis_Actiondis_Effect *e;
  int i, j;

  for ( i = 0; i < dis_gnum_lnf_goal; i++ ) {
    if ( dis_glnf_goal_lh[i]->num_nF > 0 ) {
      *fl = dis_glnf_goal_lh[i]->nF[0];
      return dis_TRUE;
    }
  }

  for ( a = dis_gactions; a; a = a->next ) {
    for ( i = 0; i < a->num_lnf_preconds; i++ ) {
      if ( a->lnf_preconds_lh[i]->num_nF > 0 ) {
	*fl = a->lnf_preconds_lh[i]->nF[0];
	return dis_TRUE;
      }
    }

    for ( i = 0; i < a->num_effects; i++ ) {
      e = &(a->effects[i]);

      for ( j = 0; j < e->num_lnf_conditions; j++ ) {
	if ( e->lnf_conditions_lh[j]->num_nF > 0 ) {
	  *fl = e->lnf_conditions_lh[j]->nF[0];
	  return dis_TRUE;
	}
      }

      if ( e->illegal ) {
	/* we don't care if there's something in here that
	 * wants to be translated.
	 */
	continue;
      }

      for ( j = 0; j < e->num_lnf_effects; j++ ) {
	if ( e->lnf_effects_rh[j]->num_nF > 0 ) {
	  *fl = e->lnf_effects_rh[j]->nF[0];
	  return dis_TRUE;
	}
      }
    }
  }

  if ( dis_gcmd_line.optimize && dis_glnf_metric.num_nF > 0 ) {
    *fl = dis_glnf_metric.nF[0];
    return dis_TRUE;
  }

  return dis_FALSE;

}



void dis_introduce_minus_fluent( int fl )

{

  if ( dis_gnum_relevant_fluents == dis_MAX_RELEVANT_FLUENTS ) {
    printf("\ntoo many relevant fluents! increase dis_MAX_RELEVANT_FLUENTS (currently %d)\n\n",
	   dis_MAX_RELEVANT_FLUENTS);
    exit( 1 );
  }
  dis_grelevant_fluents[dis_gnum_relevant_fluents].function = -1;
  dis_grelevant_fluents_name[dis_gnum_relevant_fluents] = 
    ( char * ) calloc( strlen(dis_grelevant_fluents_name[fl])+7, sizeof( char ) );
  strcpy( dis_grelevant_fluents_name[dis_gnum_relevant_fluents], "MINUS-" );
  strcat( dis_grelevant_fluents_name[dis_gnum_relevant_fluents],
	  dis_grelevant_fluents_name[fl] );
  lminus_fluent[fl] = dis_gnum_relevant_fluents;
  lminus_fluent[dis_gnum_relevant_fluents] = fl;
  dis_gnum_relevant_fluents++;

}



void dis_replace_fl_in_nF_with_minus_fl( int fl )

{

  dis_Action *a;
  dis_Actiondis_Effect *e;
  int i, j, k, l;

  for ( i = 0; i < dis_gnum_lnf_goal; i++ ) {
    for ( j = 0; j < dis_glnf_goal_lh[i]->num_nF; j++ ) {
      if ( dis_glnf_goal_lh[i]->nF[j] == fl ) break;
    }
    if ( j == dis_glnf_goal_lh[i]->num_nF ) continue;
    /* now the jth fluent in subtraction is our translated one.
     *
     * first, put minus-fl into pF. Can't already be there
     * because we have only just introduced it.
     */
    if ( dis_glnf_goal_lh[i]->num_pF == dis_MAX_LNF_F ) {
      printf("\n\nincrease dis_MAX_LNF_F! currently %d\n\n", dis_MAX_LNF_F);
      exit( 1 );
    }
    dis_glnf_goal_lh[i]->pF[dis_glnf_goal_lh[i]->num_pF] = lminus_fluent[fl];
    dis_glnf_goal_lh[i]->pC[dis_glnf_goal_lh[i]->num_pF++] = dis_glnf_goal_lh[i]->nC[j];
    /* now remove fl from nF.
     */
    for ( k = j; k < dis_glnf_goal_lh[i]->num_nF - 1; k++ ) {
      dis_glnf_goal_lh[i]->nF[k] = dis_glnf_goal_lh[i]->nF[k+1];
      dis_glnf_goal_lh[i]->nC[k] = dis_glnf_goal_lh[i]->nC[k+1];
    }
    dis_glnf_goal_lh[i]->num_nF--;
  }

  for ( a = dis_gactions; a; a = a->next ) {
    for ( i = 0; i < a->num_lnf_preconds; i++ ) {
      for ( j = 0; j < a->lnf_preconds_lh[i]->num_nF; j++ ) {
	if ( a->lnf_preconds_lh[i]->nF[j] == fl ) break;
      }
      if ( j == a->lnf_preconds_lh[i]->num_nF ) continue;
      if ( a->lnf_preconds_lh[i]->num_pF == dis_MAX_LNF_F ) {
	printf("\n\nincrease dis_MAX_LNF_F! currently %d\n\n", dis_MAX_LNF_F);
	exit( 1 );
      }
      a->lnf_preconds_lh[i]->pF[a->lnf_preconds_lh[i]->num_pF] = lminus_fluent[fl];
      a->lnf_preconds_lh[i]->pC[a->lnf_preconds_lh[i]->num_pF++] = a->lnf_preconds_lh[i]->nC[j];
      for ( k = j; k < a->lnf_preconds_lh[i]->num_nF - 1; k++ ) {
	a->lnf_preconds_lh[i]->nF[k] = a->lnf_preconds_lh[i]->nF[k+1];
	a->lnf_preconds_lh[i]->nC[k] = a->lnf_preconds_lh[i]->nC[k+1];
      }
      a->lnf_preconds_lh[i]->num_nF--;
    }

    for ( i = 0; i < a->num_effects; i++ ) {
      e = &(a->effects[i]);

      for ( j = 0; j < e->num_lnf_conditions; j++ ) {
	for ( k = 0; k < e->lnf_conditions_lh[j]->num_nF; k++ ) {
	  if ( e->lnf_conditions_lh[j]->nF[k] == fl ) break;
	}
	if ( k == e->lnf_conditions_lh[j]->num_nF ) continue;
	if ( e->lnf_conditions_lh[j]->num_pF == dis_MAX_LNF_F ) {
	  printf("\n\nincrease dis_MAX_LNF_F! currently %d\n\n", dis_MAX_LNF_F);
	  exit( 1 );
	}
	e->lnf_conditions_lh[j]->pF[e->lnf_conditions_lh[j]->num_pF] = lminus_fluent[fl];
	e->lnf_conditions_lh[j]->pC[e->lnf_conditions_lh[j]->num_pF++] = e->lnf_conditions_lh[j]->nC[k];
	for ( l = k; l < e->lnf_conditions_lh[j]->num_nF - 1; l++ ) {
	  e->lnf_conditions_lh[j]->nF[l] = e->lnf_conditions_lh[j]->nF[l+1];
	  e->lnf_conditions_lh[j]->nC[l] = e->lnf_conditions_lh[j]->nC[l+1];
	}
	e->lnf_conditions_lh[j]->num_nF--;
      }

      if ( e->illegal ) {
	/* like before, we don't care about effects that access
	 * irrelevant fluents
	 */
	continue;
      }

      for ( j = 0; j < e->num_lnf_effects; j++ ) {
	for ( k = 0; k < e->lnf_effects_rh[j]->num_nF; k++ ) {
	  if ( e->lnf_effects_rh[j]->nF[k] == fl ) break;
	}
	if ( k == e->lnf_effects_rh[j]->num_nF ) continue;
	if ( e->lnf_effects_rh[j]->num_pF == dis_MAX_LNF_F ) {
	  printf("\n\nincrease dis_MAX_LNF_F! currently %d\n\n", dis_MAX_LNF_F);
	  exit( 1 );
	}
	e->lnf_effects_rh[j]->pF[e->lnf_effects_rh[j]->num_pF] = lminus_fluent[fl];
	e->lnf_effects_rh[j]->pC[e->lnf_effects_rh[j]->num_pF++] = e->lnf_effects_rh[j]->nC[k];
	for ( l = k; l < e->lnf_effects_rh[j]->num_nF - 1; l++ ) {
	  e->lnf_effects_rh[j]->nF[l] = e->lnf_effects_rh[j]->nF[l+1];
	  e->lnf_effects_rh[j]->nC[l] = e->lnf_effects_rh[j]->nC[l+1];
	}
	e->lnf_effects_rh[j]->num_nF--;
      }
    }
  }

  for ( j = 0; j < dis_glnf_metric.num_nF; j++ ) {
    if ( dis_glnf_metric.nF[j] == fl ) break;
  }
  if ( j < dis_glnf_metric.num_nF ) {
    if ( dis_glnf_metric.num_pF == dis_MAX_LNF_F ) {
      printf("\n\nincrease dis_MAX_LNF_F! currently %d\n\n", dis_MAX_LNF_F);
      exit( 1 );
    }
    dis_glnf_metric.pF[dis_glnf_metric.num_pF] = lminus_fluent[fl];
    dis_glnf_metric.pC[dis_glnf_metric.num_pF++] = dis_glnf_metric.nC[j];
    for ( k = j; k < dis_glnf_metric.num_nF - 1; k++ ) {
      dis_glnf_metric.nF[k] = dis_glnf_metric.nF[k+1];
      dis_glnf_metric.nC[k] = dis_glnf_metric.nC[k+1];
    }
    dis_glnf_metric.num_nF--;
  }

}



void dis_set_minus_fl_initial( int fl )

{

  if ( dis_ginitial_state.f_D[fl] ) {
    dis_ginitial_state.f_D[lminus_fluent[fl]] = dis_TRUE;
    dis_ginitial_state.f_V[lminus_fluent[fl]] =  (-1) * dis_ginitial_state.f_V[fl];
  }

}



void dis_introduce_minus_fl_effects( int fl )

{

  dis_Action *a;
  dis_Actiondis_Effect *e;
  int i, j, k, pf, nf;
  Lnfdis_ExpNode *len;

  for ( a = dis_gactions; a; a = a->next ) {
    for ( i = 0; i < a->num_effects; i++ ) {
      e = &(a->effects[i]);
      if ( e->illegal ) {
	/* no need to translate illegal effects.
	 */
	continue;
      }

      for ( j = 0; j < e->num_lnf_effects; j++ ) {
	if ( e->lnf_effects_fl[j] != fl ) {
	  continue;
	}
	/* here is an effect that affects our fl.
	 * introduce inverse effect for minus_fl,
	 * making use of all minus-fl's that are already
	 * there.
	 */
	if ( e->num_lnf_effects == dis_MAX_LNF_EFFS ) {
	  printf("\n\nincrease dis_MAX_LNF_EFFS! currently %d\n\n", dis_MAX_LNF_EFFS);
	  exit( 1 );
	}
	e->lnf_effects_neft[e->num_lnf_effects] = e->lnf_effects_neft[j];
	e->lnf_effects_fl[e->num_lnf_effects] = lminus_fluent[fl];
	e->lnf_effects_rh[e->num_lnf_effects] = dis_new_Lnfdis_ExpNode();
	len = e->lnf_effects_rh[e->num_lnf_effects];
	/* now the most "difficult" part: setup the inverted pF and nF
	 * informations.
	 *
	 * dis_NOTE: as fluent occurences are unique in original ef,
	 *       so will they be in new ef. (no len contains both
	 *       a fluent and its minus-fluent)
	 *       --> invariant is or should be that the absolute
	 *           fluents occur at most once in |pF| \cup |nF|.
	 *           holds in the beginning.  only thing we do is
	 *           we exchange in that set for some fluents the
	 *           positive with the negative version, so the
	 *           invariant is in fact preserved.
	 */
	for ( k = 0; k < e->lnf_effects_rh[j]->num_pF; k++ ) {
	  pf = e->lnf_effects_rh[j]->pF[k];
	  if ( lminus_fluent[pf] == -1 ) {
	    /* not translated yet --> insert it into nF
	     */
	    if ( len->num_nF == dis_MAX_LNF_F ) {
	      printf("\n\nincrease dis_MAX_LNF_F! currently %d\n\n", dis_MAX_LNF_F);
	      exit( 1 );
	    }
	    len->nF[len->num_nF] = pf;
	    len->nC[len->num_nF++] = e->lnf_effects_rh[j]->pC[k];
	  } else {
	    /* else, insert minus-pf into pF
	     */
	    if ( len->num_pF == dis_MAX_LNF_F ) {
	      printf("\n\nincrease dis_MAX_LNF_F! currently %d\n\n", dis_MAX_LNF_F);
	      exit( 1 );
	    }
	    len->pF[len->num_pF] = lminus_fluent[pf];
	    len->pC[len->num_pF++] = e->lnf_effects_rh[j]->pC[k];
	  }
	}
	for ( k = 0; k < e->lnf_effects_rh[j]->num_nF; k++ ) {
	  nf = e->lnf_effects_rh[j]->nF[k];
	  /* insert all of those into pF
	   */
	  if ( len->num_pF == dis_MAX_LNF_F ) {
	    printf("\n\nincrease dis_MAX_LNF_F! currently %d\n\n", dis_MAX_LNF_F);
	    exit( 1 );
	  }
	  len->pF[len->num_pF] = nf;
	  len->pC[len->num_pF++] = e->lnf_effects_rh[j]->nC[k];
	}
	/* the constant must of course be inverted.
	 */
	len->c = (-1) * e->lnf_effects_rh[j]->c;
	e->num_lnf_effects++;
      }
    }
  }

}


















/*************************************************************
 * LNF POST-PROCESSING I: SUMMARIZE EFFECTS.
 *************************************************************/



















int *lA, *lD;
int lnum_A, lnum_D;






void dis_summarize_effects( void )

{

  dis_Action *a;
  dis_Actiondis_Effect *e, *e_;
  int i, j, k, l;

  lA = ( int * ) calloc( dis_gnum_relevant_facts, sizeof( int ) );
  lD = ( int * ) calloc( dis_gnum_relevant_facts, sizeof( int ) );

  for ( a = dis_gactions; a; a = a->next ) {
    i = 0;
    while ( i < a->num_effects ) {
      e = &(a->effects[i]);
      if ( e->removed ) {
	/* this one's already handled.
	 */
	i++;
	continue;
      }

      /* first, merge the effect's own effects together. logical:
       */
      lnum_A = 0;
      for ( j = 0; j < e->num_adds; j++ ) {
	for ( k = 0; k < lnum_A; k++ ) {
	  if ( lA[k] == e->adds[j] ) break;
	}
	if ( k < lnum_A ) continue;
	lA[lnum_A++] = e->adds[j];
      }
      lnum_D = 0;
      for ( j = 0; j < e->num_dels; j++ ) {
	for ( k = 0; k < lnum_D; k++ ) {
	  if ( lD[k] == e->dels[j] ) break;
	}
	if ( k < lnum_D ) continue;
	lD[lnum_D++] = e->dels[j];
      }
      /* numerical:
       */
      j = 0;
      while ( j < e->num_lnf_effects ) {
	/* merge all effects increasing the same fluent into
	 * effect j, and remove them.
	 */
	k = j + 1;
	while ( k < e->num_lnf_effects ) {
	  if ( e->lnf_effects_fl[k] != e->lnf_effects_fl[j] ) {
	    k++;
	    continue;
	  }
	  if ( e->lnf_effects_neft[j] == ASSIGN ) {
	    if ( e->lnf_effects_neft[k] != ASSIGN ||
		 !dis_same_lnfs( e->lnf_effects_rh[j], e->lnf_effects_rh[k] ) ) {
	      e->illegal = dis_TRUE;
	      break;
	    }
	  } else {
	    if ( e->lnf_effects_neft[k] == ASSIGN ) {
	      e->illegal = dis_TRUE;
	      break;
	    }
	    dis_merge_lnfs( e->lnf_effects_rh[j], e->lnf_effects_rh[k] );
	  }
	  /* we also get here if we have two identical assigns.
	   */
	  xfree( e->lnf_effects_rh[k] );
	  for ( l = k; l < e->num_lnf_effects - 1; l++ ) {
	    e->lnf_effects_neft[l] = e->lnf_effects_neft[l+1];
	    e->lnf_effects_fl[l] = e->lnf_effects_fl[l+1];
	    e->lnf_effects_rh[l] = e->lnf_effects_rh[l+1];
	  }
	  e->num_lnf_effects--;
	}
	if ( k < e->num_lnf_effects ) {
	  /* illegal combination
	   */
	  break;
	}
	j++;
      }

      /* now merge all effects after i with same condition
       * into that.
       */
      j = i + 1;
      while ( j < a->num_effects ) {
	e_ = &(a->effects[j]);
	if ( e_->removed ) {
	  j++;
	  continue;
	}

	if ( !dis_same_condition( e, e_ ) ) {
	  j++;
	  continue;
	}
	/* no matter what happens, we can get rid of effect e_
	 */
	e_->removed = dis_TRUE;

	/* illegality is inherited in both directions.
	 */
	if ( e_->illegal ) {
	  e->illegal = dis_TRUE;
	}
	if ( e->illegal ) {
	  /* just for docu; it is removed anyway.
	   */
	  e_->illegal = dis_TRUE;
	}

	if ( !e->illegal ) {
	  /* the combined effect appears to be legal. merge it.
	   */
	  dis_merge_effects( e, e_ );
	  if ( e->illegal ) {
	    /* e might have become illegal. again, docu this.
	     */
	    e_->illegal = dis_TRUE;
	  }
	}

	j++;
      }

      /* now put the updated A and D info into e.
       *
       * have to be careful: it might be that there are
       * now too many facts and we need to re-allocate
       * e's capabilities.
       */
      if ( lnum_A > e->num_adds ) {
	xfree( e->adds );
	e->adds = ( int * ) calloc( lnum_A, sizeof( int ) );
      }
      for ( j = 0; j < lnum_A; j++ ) {
	e->adds[j] = lA[j];
      }
      e->num_adds = lnum_A;
      if ( lnum_D > e->num_dels ) {
	xfree( e->dels );
	e->dels = ( int * ) calloc( lnum_D, sizeof( int ) );
      }
      for ( j = 0; j < lnum_D; j++ ) {
	e->dels[j] = lD[j];
      }
      e->num_dels = lnum_D;

      /* increment current effects counter.
       */
      i++;
    }
  }

}



dis_Bool dis_same_condition( dis_Actiondis_Effect *e, dis_Actiondis_Effect *e_ )

{

  int i, j;

  if ( e->num_conditions != e_->num_conditions ||
       e->num_lnf_conditions != e_->num_lnf_conditions ) return dis_FALSE;

  for ( i = 0; i < e->num_conditions; i++ ) {
    for ( j = 0; j < e_->num_conditions; j++ ) {
      if ( e->conditions[i] == e_->conditions[j] ) break;
    }
    if ( j == e_->num_conditions ) break;
  }
  if ( i < e->num_conditions ) return dis_FALSE;

  for ( i = 0; i < e->num_lnf_conditions; i++ ) {
    for ( j = 0; j < e_->num_lnf_conditions; j++ ) {
      if ( e_->lnf_conditions_comp[j] != e->lnf_conditions_comp[i] ) continue;
      if ( e_->lnf_conditions_rh[j] != e->lnf_conditions_rh[i] ) continue;
      if ( !dis_same_lnfs( e_->lnf_conditions_lh[j], e->lnf_conditions_lh[i] ) ) continue;
      break;
    }
    if ( j == e_->num_lnf_conditions ) break;
  }
  if ( i < e->num_lnf_conditions ) return dis_FALSE;

  return dis_TRUE;

}



dis_Bool dis_same_lnfs( Lnfdis_ExpNode *l, Lnfdis_ExpNode *r )

{

  int i, j;

  if ( l->num_pF != r->num_pF ||
       l->c != r->c ) return dis_FALSE;

  for ( i = 0; i < l->num_pF; i++ ) {
    for ( j = 0; j < r->num_pF; j++ ) {
      if ( l->pF[i] != r->pF[j] ) continue;
      if ( l->pC[i] != r->pC[j] ) {
	/* same fluent with different weighting.
	 */
	return dis_FALSE;
      }
      break;
    }
    if ( j == r->num_pF ) break;
  }
  if ( i < l->num_pF ) return dis_FALSE;

  return dis_TRUE;

}



void dis_merge_effects( dis_Actiondis_Effect *e, dis_Actiondis_Effect *e_ )

{

  int i, j;

  /* we don't care whether adds and dels intersect:
   * they're allowed to by semantics.
   */
  for ( i = 0; i < e_->num_adds; i++ ) {
    for ( j = 0; j < lnum_A; j++ ) {
      if ( lA[j] == e_->adds[i] ) break;
    }
    if ( j < lnum_A ) continue;
    lA[lnum_A++] = e_->adds[i];
  }
  for ( i = 0; i < e_->num_dels; i++ ) {
    for ( j = 0; j < lnum_D; j++ ) {
      if ( lD[j] == e_->dels[i] ) break;
    }
    if ( j < lnum_D ) continue;
    lD[lnum_D++] = e_->dels[i];
  }

  for ( i = 0; i < e_->num_lnf_effects; i++ ) {
    for ( j = 0; j < e->num_lnf_effects; j++ ) {
      if ( e->lnf_effects_fl[j] == e_->lnf_effects_fl[i] ) break;
    }
    if ( j == e->num_lnf_effects ) {
      /* new affected fluent!
       */
      if ( e->num_lnf_effects == dis_MAX_LNF_EFFS ) {
	printf("\n\nincrease dis_MAX_LNF_EFFS! currently %d\n\n", dis_MAX_LNF_EFFS);
	exit( 1 );
      }
      e->lnf_effects_neft[e->num_lnf_effects] = e_->lnf_effects_neft[i];
      e->lnf_effects_fl[e->num_lnf_effects] = e_->lnf_effects_fl[i];
      /* we can also simply take the pointer: e_ is only marked as removed,
       * but not freed.
       */
      e->lnf_effects_rh[e->num_lnf_effects] = e_->lnf_effects_rh[i];
      e->num_lnf_effects++;
    } else {
      if ( e->lnf_effects_neft[j] == ASSIGN ) {
	if ( e_->lnf_effects_neft[i] != ASSIGN ||
	     !dis_same_lnfs( e->lnf_effects_rh[j], e_->lnf_effects_rh[i] ) ) {
	  e->illegal = dis_TRUE;
	  return;
	}
	/* identical assigns. nothing needs to be done.
	 */
      } else {
	if ( e_->lnf_effects_neft[i] == ASSIGN ) {
	  e->illegal = dis_TRUE;
	  return;
	}
	dis_merge_lnfs( e->lnf_effects_rh[j], e_->lnf_effects_rh[i] );
      }
    }
  }

}



/* merge both LNFs into the left one.
 * (only pF needed as both are already 
 * fully transformed)
 */
void dis_merge_lnfs( Lnfdis_ExpNode *l, Lnfdis_ExpNode *r )

{

  int i, j, k;

  for ( i = 0; i < r->num_pF; i++ ) {

    for ( j = 0; j < l->num_pF; j++ ) {
      if ( r->pF[i] == l->pF[j] ) break;
    }
    if ( j < l->num_pF ) {
      /* got that one in dest LNF already
       */
      l->pC[j] += r->pC[i];
      continue;
    }

    if ( lminus_fluent[r->pF[i]] != -1 ) {
      /* this one was already translated. let's see
       * if its counterpart is in the left lnf.
       */
      for ( j = 0; j < l->num_pF; j++ ) {
	if ( lminus_fluent[r->pF[i]] == l->pF[j] ) break;
      }
      if ( j < l->num_pF ) {
	/* for this, we got the inverse one!
	 */
	l->pC[j] -= r->pC[i];
	if ( l->pC[j] < 0 ) {
	  l->pF[j] = r->pF[i];
	  l->pC[j] *= (-1);
	}
	if ( l->pC[j] == 0 ) {
	  /* remove this entirely.
	   */
	  for ( k = j; k < l->num_pF - 1; k++ ) {
	    l->pF[k] = l->pF[k+1];
	    l->pC[k] = l->pC[k+1];
	  }
	  l->num_pF--;
	}
	continue;
      }
    }

    /* we got neither that nor its counterpart.
     */
    if ( l->num_pF == dis_MAX_LNF_F ) {
      printf("\n\nincrease dis_MAX_LNF_F! currently %d\n\n", dis_MAX_LNF_F);
      exit( 1 );
    }
    l->pF[l->num_pF] = r->pF[i];
    l->pC[l->num_pF++] = r->pC[i];
  }


  l->c += r->c;

}






















/*************************************************************
 * LNF POST-PROCESSING II: ENCODE NON-MINIMAL LNFs.
 *************************************************************/























void dis_encode_lfns_as_artificial_fluents( void )

{

  int i;

  /* for the artificial new ones, this will be set
   * to the respective LNF.
   */
  for ( i = 0; i < dis_MAX_RELEVANT_FLUENTS; i++ ) {
    dis_grelevant_fluents_lnf[i] = NULL;
  }

  while ( dis_TRUE ) {
    /* ex non-minimal lnf in pre, cond, eff, or goal?
     *
     * (i.e., lnf != fl + c)
     */
    if ( !dis_ex_non_minimal_lnf_in_pre_cond_goal_eff() ) {
      /* no --> we are finished.
       */
      break;
    }
    /* otherwise, the respective LNF, without the 
     * constant part, is set up in
     * exp_lF...; (local global borrowed from above);
     *
     * introduce a new artificial fluent for that
     * LNF
     */
    dis_introduce_artificial_fluent();
    /* replace all occurences in pres, conds, effs, and goals
     */
    dis_replace_non_minimal_lnf_with_artificial_fl();
  }

}



dis_Bool dis_ex_non_minimal_lnf_in_pre_cond_goal_eff( void )

{

  dis_Action *a;
  dis_Actiondis_Effect *e;
  int i, j, k;

  for ( i = 0; i < dis_gnum_lnf_goal; i++ ) {
    if ( dis_glnf_goal_lh[i]->num_pF > 1 ||
	 (dis_glnf_goal_lh[i]->num_pF == 1 && dis_glnf_goal_lh[i]->pC[0] != 1) ) {
      for ( j = 0; j < dis_glnf_goal_lh[i]->num_pF; j++ ) {
	exp_lF[j] = dis_glnf_goal_lh[i]->pF[j];
	exp_lC[j] = dis_glnf_goal_lh[i]->pC[j];
      }
      exp_lnum_F = dis_glnf_goal_lh[i]->num_pF;
      return dis_TRUE;
    }
  }

  for ( a = dis_gactions; a; a = a->next ) {
    for ( i = 0; i < a->num_lnf_preconds; i++ ) {
      if ( a->lnf_preconds_lh[i]->num_pF > 1 ||
	   (a->lnf_preconds_lh[i]->num_pF == 1 && a->lnf_preconds_lh[i]->pC[0] != 1) ) {
	for ( j = 0; j < a->lnf_preconds_lh[i]->num_pF; j++ ) {
	  exp_lF[j] = a->lnf_preconds_lh[i]->pF[j];
	  exp_lC[j] = a->lnf_preconds_lh[i]->pC[j];
	}
	exp_lnum_F = a->lnf_preconds_lh[i]->num_pF;
	return dis_TRUE;
      }
    }

    for ( i = 0; i < a->num_effects; i++ ) {
      e = &(a->effects[i]);
      if ( e->removed ) {
	/* these will not be included into conn:
	 * merged into somewhere else.
	 */
	continue;
      }

      for ( j = 0; j < e->num_lnf_conditions; j++ ) {
	if ( e->lnf_conditions_lh[j]->num_pF > 1 ||
	     (e->lnf_conditions_lh[j]->num_pF == 1 && e->lnf_conditions_lh[j]->pC[0] != 1) ) {
	  for ( k = 0; k < e->lnf_conditions_lh[j]->num_pF; k++ ) {
	    exp_lF[k] = e->lnf_conditions_lh[j]->pF[k];
	    exp_lC[k] = e->lnf_conditions_lh[j]->pC[k];
	  }
	  exp_lnum_F = e->lnf_conditions_lh[j]->num_pF;
	  return dis_TRUE;
	}
      }

      if ( e->illegal ) {
	continue;
      }

      for ( j = 0; j < e->num_lnf_effects; j++ ) {
	if ( e->lnf_effects_rh[j]->num_pF > 1 ||
	     (e->lnf_effects_rh[j]->num_pF == 1 && e->lnf_effects_rh[j]->pC[0] != 1) ) {
	  for ( k = 0; k < e->lnf_effects_rh[j]->num_pF; k++ ) {
	    exp_lF[k] = e->lnf_effects_rh[j]->pF[k];
	    exp_lC[k] = e->lnf_effects_rh[j]->pC[k];
	  }
	  exp_lnum_F = e->lnf_effects_rh[j]->num_pF;
	  return dis_TRUE;
	}
      }
    }
  }

  return dis_FALSE;

}



void dis_introduce_artificial_fluent( void )

{

  int i;

  if ( dis_gnum_relevant_fluents == dis_MAX_RELEVANT_FLUENTS ) {
    printf("\ntoo many relevant fluents! increase dis_MAX_RELEVANT_FLUENTS (currently %d)\n\n",
	   dis_MAX_RELEVANT_FLUENTS);
    exit( 1 );
  }
  dis_grelevant_fluents[dis_gnum_relevant_fluents].function = -1;

  /* no name --> is inferred in this case from _lnf
   */

  dis_grelevant_fluents_lnf[dis_gnum_relevant_fluents] = dis_new_Lnfdis_ExpNode();
  for ( i = 0; i < exp_lnum_F; i++ ) {
    dis_grelevant_fluents_lnf[dis_gnum_relevant_fluents]->pF[i] = exp_lF[i];
    dis_grelevant_fluents_lnf[dis_gnum_relevant_fluents]->pC[i] = exp_lC[i];
  }
  dis_grelevant_fluents_lnf[dis_gnum_relevant_fluents]->num_pF = exp_lnum_F;
  dis_grelevant_fluents_lnf[dis_gnum_relevant_fluents]->pF = (int *) realloc(dis_grelevant_fluents_lnf[dis_gnum_relevant_fluents]->pF, dis_grelevant_fluents_lnf[dis_gnum_relevant_fluents]->num_pF*sizeof(int));
  dis_grelevant_fluents_lnf[dis_gnum_relevant_fluents]->pC = (float *) realloc(dis_grelevant_fluents_lnf[dis_gnum_relevant_fluents]->pC, dis_grelevant_fluents_lnf[dis_gnum_relevant_fluents]->num_pF*sizeof(float));

  dis_gnum_relevant_fluents++;

}



void dis_replace_non_minimal_lnf_with_artificial_fl( void )

{

  dis_Action *a;
  dis_Actiondis_Effect *e;
  int i, j;

  for ( i = 0; i < dis_gnum_lnf_goal; i++ ) {
    if ( !dis_is_artificial_fluent( dis_glnf_goal_lh[i] ) ) {
      continue;
    }
    /* the pF here is the pF we are currently replacing.
     */
    dis_glnf_goal_lh[i]->pF[0] = dis_gnum_relevant_fluents - 1;
    dis_glnf_goal_lh[i]->pC[0] = 1;
    dis_glnf_goal_lh[i]->num_pF = 1;
  }

  for ( a = dis_gactions; a; a = a->next ) {
    for ( i = 0; i < a->num_lnf_preconds; i++ ) {
      if ( !dis_is_artificial_fluent( a->lnf_preconds_lh[i] ) ) {
	continue;
      }
      a->lnf_preconds_lh[i]->pF[0] = dis_gnum_relevant_fluents - 1;
      a->lnf_preconds_lh[i]->pC[0] = 1;
      a->lnf_preconds_lh[i]->num_pF = 1;
    }

    for ( i = 0; i < a->num_effects; i++ ) {
      e = &(a->effects[i]);
      if ( e->removed ) {
	/* these will not be included into conn:
	 * merged into somewhere else.
	 */
	continue;
      }

      for ( j = 0; j < e->num_lnf_conditions; j++ ) {
	if ( !dis_is_artificial_fluent( e->lnf_conditions_lh[j] ) ) {
	  continue;
	}
	e->lnf_conditions_lh[j]->pF[0] = dis_gnum_relevant_fluents - 1;
	e->lnf_conditions_lh[j]->pC[0] = 1;
	e->lnf_conditions_lh[j]->num_pF = 1;
      }

      if ( e->illegal ) {
	continue;
      }

      for ( j = 0; j < e->num_lnf_effects; j++ ) {
	if ( !dis_is_artificial_fluent( e->lnf_effects_rh[j] ) ) {
	  continue;
	}
	e->lnf_effects_rh[j]->pF[0] = dis_gnum_relevant_fluents - 1;
	e->lnf_effects_rh[j]->pC[0] = 1;
	e->lnf_effects_rh[j]->num_pF = 1;
      }
    }
  }

}



dis_Bool dis_is_artificial_fluent( Lnfdis_ExpNode *n )

{

  int i, j;

  if ( n->num_nF != 0 ) {
    printf("\n\nchecking non-empty nF for multiple fl!\n\n");
    exit( 1 );
  }

  if ( n->num_pF != exp_lnum_F ) {
    return dis_FALSE;
  }

  for ( i = 0; i < exp_lnum_F; i++ ) {
    for ( j = 0; j < n->num_pF; j++ ) {
      if ( n->pF[j] != exp_lF[i] ) continue;
      if ( n->pC[j] != exp_lC[i] ) {
	/* wrong constant multiplier!
	 */
	return dis_FALSE;
      }
      break;
    }
    if ( j == n->num_pF ) {
      /* didn't find this fluent i in here.
       */
      return dis_FALSE;
    }
  }

  return dis_TRUE;

}


















/*************************************************************
 * AT LAST: PREPARATIONS Fdis_OR METRIC FUNCTION
 *************************************************************/


















dis_Bool dis_setup_effect_costs( void )

{

  dis_Action *a;
  dis_Actiondis_Effect *e;
  int i, j, k, fl;
  dis_Bool non_zero = dis_FALSE, ret = dis_TRUE;

  if ( dis_glnf_metric.num_pF == 0 ) {
    /* no metric, or previously failed
     */
    if ( dis_gcmd_line.display_info ) {
   //   printf("\nno metric specified. plan length assumed.");
    }
    return dis_FALSE;
  }

  /* also in here: check if all parts of metric are defined
   * if not, then they won't ever be because we do not allow
   * assigners anyway. also, setup dis_gtt total-time multipl.
   */
  dis_gtt = 0;
  for ( i = 0; i < dis_glnf_metric.num_pF; i++ ) {
    if ( dis_glnf_metric.pF[i] == -2 ) {
      dis_gtt = dis_glnf_metric.pC[i];
      continue;
    }
    if ( !dis_ginitial_state.f_D[dis_glnf_metric.pF[i]] ) break;
  }
  if ( i < dis_glnf_metric.num_pF ) {
    if ( dis_gcmd_line.display_info ) {
      printf("\nwarning: metric undefined initially. replaced with plan length (no assigners allowed anyway).");
    }
    return dis_FALSE;
  }

  for ( a = dis_gactions; a; a = a->next ) {
    for ( i = 0; i < a->num_effects; i++ ) {
      e = &(a->effects[i]);
      e->cost = 0;

      if ( e->removed ||
	   e->illegal ) {
	continue;
      }

      for ( j = 0; j < e->num_lnf_effects; j++ ) {
	fl = e->lnf_effects_fl[j];
	for ( k = 0; k < dis_glnf_metric.num_pF; k++ ) {
	  if ( fl == dis_glnf_metric.pF[k] ) break;
	}
	if ( k == dis_glnf_metric.num_pF ) continue;

	if ( e->lnf_effects_rh[j]->num_pF > 0 ) {
	  if ( dis_gcmd_line.display_info ) {
//	    printf("\nwarning: non-constant effect on metric. metric replaced with plan length.");
	  }
	  ret = dis_FALSE;
	}
	if ( e->lnf_effects_neft[j] != INCREASE ) {
	  if ( dis_gcmd_line.display_info ) {
//	    printf("\nwarning: assign on metric. metric replaced with plan length.");
	  }
	  ret = dis_FALSE;
	}
	if ( e->lnf_effects_rh[j]->c < 0 ) {
	  if ( dis_gcmd_line.display_info ) {
//	    printf("\nwarning: change on metric in wrong direction. metric replaced with plan length.");
	  }
	  ret = dis_FALSE;
	}

	e->cost += dis_glnf_metric.pC[k] * e->lnf_effects_rh[j]->c;
	if ( e->cost > 0 ) {
	  non_zero = dis_TRUE;
	}
      }
    }
  }

/*  if ( !non_zero ) {
    if ( dis_gtt == 0 ) {
      if ( dis_gcmd_line.display_info ) {
	printf("\nwarning: trivial metric, all costs 0. metric replaced with plan length.");
      }
      return dis_FALSE;
    }
  }*/

  return ret;

}


/*************************************************************
 * AT VERY LAST: ACYCLIC := EFFS, AND STATIC FL RELEVANCE
 *************************************************************/

void dis_check_assigncycles( void )

{

  int i, j, k, c = 0;

  dis_gassign_influence = ( dis_Bool ** ) calloc( dis_gnum_real_fl_conn, sizeof( dis_Bool* ) );
  dis_gTassign_influence = ( dis_Bool ** ) calloc( dis_gnum_real_fl_conn, sizeof( dis_Bool* ) );
  for ( i = 0; i < dis_gnum_real_fl_conn; i++ ) {
    dis_gassign_influence[i] = ( dis_Bool * ) calloc( dis_gnum_real_fl_conn, sizeof( dis_Bool ) );
    dis_gTassign_influence[i] = ( dis_Bool * ) calloc( dis_gnum_real_fl_conn, sizeof( dis_Bool ) );
  }

  if ( dis_gcmd_line.display_info ) {
    printf("\n\nchecking for cyclic := effects");
  }
  for ( i = 0; i < dis_gnum_real_fl_conn; i++ ) {
    for ( j = 0; j < dis_gnum_real_fl_conn; j++ ) {
      dis_gassign_influence[i][j] = dis_i_influences_j( i, j );
      dis_gTassign_influence[i][j] = dis_i_influences_j( i, j );
    }
  }
  /* compute transitive closure on dependencies
   */
  for ( j = 0; j < dis_gnum_real_fl_conn; j++ ) {
    for ( i = 0; i < dis_gnum_real_fl_conn; i++ ) {
      if ( dis_gTassign_influence[i][j] ) {
	for ( k = 0; k < dis_gnum_real_fl_conn; k++ ) {
	  if ( dis_gTassign_influence[j][k] ) {
	    dis_gTassign_influence[i][k] = dis_TRUE;
	  }
	}
      }
    }
  }
  for ( i = 0; i < dis_gnum_real_fl_conn; i++ ) {
    if ( dis_gTassign_influence[i][i] ) {
      printf("\nnumerical variable ");
      dis_print_fl_name( i );
      printf(" lies on := propagation cycle!");
      c++;
    }
  }
  if ( c > 0 ) {
    printf("\nexit. (mneed computation not possible, RPG termination unclear)");
    printf("\n(questions to Joerg Hoffmann: hoffmann@mpi-sb.mpg.de)\n\n");
    exit( 1 );
  } else {
    printf(" --- OK.");
  }

}



dis_Bool dis_i_influences_j( int fi, int fj )

{

  int i, j, fl_;

  for ( i = 0; i < dis_gfl_conn[fj].num_AS; i++ ) {
    fl_ = dis_gfl_conn[fj].AS_fl_[i];
    if ( fl_ < 0 ) continue;
    if ( fl_ == fi ) return dis_TRUE;
    if ( !dis_gfl_conn[fl_].artificial ) continue;
    for ( j = 0; j < dis_gfl_conn[fl_].num_lnf; j++ ) {
      if ( dis_gfl_conn[fl_].lnf_F[j] == fi ) return dis_TRUE;
    }
  }

  return dis_FALSE;

}



void dis_determine_fl_relevance( void )

{

  int i, j, k, fl, fl_, ef, pc, g;
  dis_Bool **dis_influenced_by;

  /* this here contains transfers from i to j i.e. if
   * i is relevant then j is too
   */
  dis_influenced_by = ( dis_Bool ** ) calloc( dis_gnum_real_fl_conn, sizeof( dis_Bool* ) );
  for ( i = 0; i < dis_gnum_real_fl_conn; i++ ) {
    dis_influenced_by[i] = ( dis_Bool * ) calloc( dis_gnum_real_fl_conn, sizeof( dis_Bool ) );
  }
  for ( i = 0; i < dis_gnum_real_fl_conn; i++ ) {
    for ( j = 0; j < dis_gnum_real_fl_conn; j++ ) {
      dis_influenced_by[i][j] = ( dis_gassign_influence[j][i] ||
			       dis_i_inc_influences_j( j, i ) );
    }
  }
  /* transitive closure so we'll have direct access below.
   */
  for ( j = 0; j < dis_gnum_real_fl_conn; j++ ) {
    for ( i = 0; i < dis_gnum_real_fl_conn; i++ ) {
      if ( dis_influenced_by[i][j] ) {
	for ( k = 0; k < dis_gnum_real_fl_conn; k++ ) {
	  if ( dis_influenced_by[j][k] ) {
	    dis_influenced_by[i][k] = dis_TRUE;
	  }
	}
      }
    }
  }

  for ( i = 0; i < dis_gnum_real_fl_conn; i++ ) {
    dis_gfl_conn[i].relevant = dis_FALSE;
  }
  /* relevance originates in effect preconds and goals.
   */
  for ( ef = 0; ef < dis_gnum_ef_conn; ef++ ) {
    for ( pc = 0; pc < dis_gef_conn[ef].num_f_PC; pc++ ) {
      /* constraint here is gef_conn[ef].f_PC_fl[pc] >= [>] gef_conn[ef].f_PC_c[pc]
       * where lh side can be lnf expression.
       */
      fl = dis_gef_conn[ef].f_PC_fl[pc];
      if ( fl < 0 ) {
	printf("\nnegative constr lh??\n\n");
	exit( 1 );
      }
      if ( !dis_gfl_conn[fl].artificial ) {
	dis_gfl_conn[fl].relevant = dis_TRUE;
	for ( i = 0; i < dis_gnum_real_fl_conn; i++ ) {
	  if ( dis_influenced_by[fl][i] ) dis_gfl_conn[i].relevant = dis_TRUE;
	}
      } else {
	for ( i = 0; i < dis_gfl_conn[fl].num_lnf; i++ ) {
	  fl_ = dis_gfl_conn[fl].lnf_F[i];
	  dis_gfl_conn[fl_].relevant = dis_TRUE;
	  for ( j = 0; j < dis_gnum_real_fl_conn; j++ ) {
	    if ( dis_influenced_by[fl_][j] ) dis_gfl_conn[j].relevant = dis_TRUE;
	  }
	}
      }
    }
  }
  for ( g = 0; g < dis_gnum_fnumeric_goal; g++ ) {
    /* constraint here is gfnumeric_goal_fl[g] >= [>] gfnumeric_goal_c[g]
     * where lh side can be lnf expression.
     */
    fl = dis_gfnumeric_goal_fl[g];
    if ( fl < 0 ) {
      printf("\nnegative constr lh??\n\n");
      exit( 1 );
    }
    if ( !dis_gfl_conn[fl].artificial ) {
      dis_gfl_conn[fl].relevant = dis_TRUE;
      for ( i = 0; i < dis_gnum_real_fl_conn; i++ ) {
	if ( dis_influenced_by[fl][i] ) dis_gfl_conn[i].relevant = dis_TRUE;
      }
    } else {
      for ( i = 0; i < dis_gfl_conn[fl].num_lnf; i++ ) {
	fl_ = dis_gfl_conn[fl].lnf_F[i];
	dis_gfl_conn[fl_].relevant = dis_TRUE;
	for ( j = 0; j < dis_gnum_real_fl_conn; j++ ) {
	  if ( dis_influenced_by[fl_][j] ) dis_gfl_conn[j].relevant = dis_TRUE;
	}
      }
    }
  }

  if ( 0 ) {
    for ( i = 0; i < dis_gnum_real_fl_conn; i++ ) {
      printf("\n"); dis_print_fl_name( i );
      printf (" --- relevant: %d", dis_gfl_conn[i].relevant);
    }
  }

}



dis_Bool dis_i_inc_influences_j( int fi, int fj )

{

  int i, j, fl_;

  for ( i = 0; i < dis_gfl_conn[fj].num_IN; i++ ) {
    fl_ = dis_gfl_conn[fj].IN_fl_[i];
    if ( fl_ < 0 ) continue;
    if ( fl_ == fi ) return dis_TRUE;
    if ( !dis_gfl_conn[fl_].artificial ) continue;
    for ( j = 0; j < dis_gfl_conn[fl_].num_lnf; j++ ) {
      if ( dis_gfl_conn[fl_].lnf_F[j] == fi ) return dis_TRUE;
    }
  }

  return dis_FALSE;

}
