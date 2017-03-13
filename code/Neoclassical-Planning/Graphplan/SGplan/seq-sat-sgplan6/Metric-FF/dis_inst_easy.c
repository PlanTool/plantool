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

 * File: dis_inst_easy.c 

 * Description: modified from inst_easy.c in Metric-FF

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









/*********************************************************************
 * File: inst_easy.c
 * Description: functions for multiplying easy operators.
 *
 *
 * Author: Joerg Hoffmann 2000
 *
 *********************************************************************/ 









#include "dis_ff.h"
#include "lpg.h"
#include "dis_output.h"
#include "dis_memory.h"

#include "dis_expressions.h"

#include "dis_inst_pre.h"
#include "dis_inst_easy.h" 








void dis_build_easy_action_templates( void )

{

  int i, j;
  Normdis_Operator *o;
  dis_EasyTemplate *t;

  dis_cleanup_easy_domain();

  if ( dis_gcmd_line.display_info == 110 ) {
    printf("\n\ncleaned up easy operators are:\n");
    for ( i = 0; i < dis_gnum_easy_operators; i++ ) {
      dis_print_Normdis_Operator( dis_geasy_operators[i] );
    }
    fflush( stdout );
  }

  /* don't know why the following function doesn't work with negative preconditions */
  if (!GpG.is_negative)
    dis_encode_easy_unaries_as_types();

  if ( dis_gcmd_line.display_info == 111 ) {
    printf("\n\nunaries encoded easy operators are:\n");
    for ( i = 0; i < dis_gnum_easy_operators; i++ ) {
      dis_print_Normdis_Operator( dis_geasy_operators[i] );
    }
    fflush( stdout );
  }

  dis_multiply_easy_effect_parameters();

  if ( dis_gcmd_line.display_info == 112 ) {
    printf("\n\neffects multiplied easy operators are:\n");
    for ( i = 0; i < dis_gnum_easy_operators; i++ ) {
      dis_print_Normdis_Operator( dis_geasy_operators[i] );
    }
    fflush( stdout );
  }

  dis_multiply_easy_op_parameters();

  if ( dis_gcmd_line.display_info == 113 ) {
    printf("\n\ninertia free easy operators are:");
    for ( i = 0; i < dis_gnum_easy_operators; i++ ) {
      dis_print_Normdis_Operator( dis_geasy_operators[i] );
    }
    printf("\n\n");
    fflush( stdout );
  }

  if ( dis_gcmd_line.display_info == 114 ) {
    printf("\n\neasy operator templates are:\n");
    
    for ( i = 0; i < dis_gnum_easy_operators; i++ ) {
      o = dis_geasy_operators[i];

      printf("\n\n-----------operator %s:-----------", o->operator->name);
      for ( t = dis_geasy_templates; t; t = t->next ) {
	if ( t->op != o ) {
	  continue;
	}
	printf("\ninst: ");
	for ( j = 0; j < o->num_vars; j++ ) {
	  if ( t->inst_table[j] < 0 ) {
	    printf("\nuninstantiated param in template! debug me, please\n\n");
	    exit( 1 );
	  }
	  printf("x%d = %s", j, dis_gconstants[t->inst_table[j]]);
	  if ( j < o->num_vars - 1 ) {
	    printf(", ");
	  }
	}
      }
    }
    fflush( stdout );
  }

}











/*********************************
 * EASY DOMAIN CLEANUP FUNCTIONs *
 *********************************/











void dis_cleanup_easy_domain( void )

{

  int i, i1, i2, i3, i4, a;
  Normdis_Operator *o;
  Normdis_Effect *e;

  /* most likely ( for sure ? ) we do not need this function call here,
   * as empty types are recognised in translation already.
   *
   * however, who knows .. ? doesn't need any real computation time anyway.
   *
   * function DOES make sense after unaries encoding, as artificial types
   * might well be empty.
   */
  dis_handle_empty_easy_parameters();

  /* remove identical preconds and effects;
   * VERY unlikely that such will get down to here, after all
   * the formula preprocessing, but possible (?) in principle.
   * takes no computation time.
   *
   * also, remove effect conditions that are contained in the 
   * preconditions.
   */
  for ( i = 0; i < dis_gnum_easy_operators; i++ ) {
    o = dis_geasy_operators[i];

    i1 = 0;
    while ( i1 < o->num_preconds-1 ) {
      i2 = i1+1;
      while ( i2 < o->num_preconds ) {
	if ( dis_identical_fact( &(o->preconds[i1]), &(o->preconds[i2]) ) ) {
	  for ( i3 = i2; i3 < o->num_preconds-1; i3++ ) {
	    o->preconds[i3].predicate = o->preconds[i3+1].predicate;
	    for ( i4 = 0; i4 < dis_garity[o->preconds[i3].predicate]; i4++ ) {
	      o->preconds[i3].args[i4] = o->preconds[i3+1].args[i4];
	    }
	  }
	  o->num_preconds--;
	} else {
	  i2++;
	}
      }
      i1++;
    }

    for ( e = o->effects; e; e = e->next ) {
      i1 = 0;
      while ( i1 < e->num_conditions-1 ) {
	i2 = i1+1;
	while ( i2 < e->num_conditions ) {
	  if ( dis_identical_fact( &(e->conditions[i1]), &(e->conditions[i2]) ) ) {
	    for ( i3 = i2; i3 < e->num_conditions-1; i3++ ) {
	      e->conditions[i3].predicate = e->conditions[i3+1].predicate;
	      /* here, we can still have equalities. nowhere else.
	       */
	      a = ( e->conditions[i3].predicate < 0 ) ? 
		2 : dis_garity[e->conditions[i3].predicate];
	      for ( i4 = 0; i4 < a; i4++ ) {
		e->conditions[i3].args[i4] = e->conditions[i3+1].args[i4];
	      }
	    }
	    e->num_conditions--;
	  } else {
	    i2++;
	  }
	}
	i1++;
      }

      i1 = 0;
      while ( i1 < e->num_conditions ) {
	for ( i2 = 0; i2 < o->num_preconds; i2++ ) {
	  if ( dis_identical_fact( &(e->conditions[i1]), &(o->preconds[i2]) ) ) {
	    break;
	  }
	}
	if ( i2 == o->num_preconds ) {
	  i1++;
	  continue;
	}
	for ( i2 = i1; i2 < e->num_conditions-1; i2++ ) {
	  e->conditions[i2].predicate = e->conditions[i2+1].predicate;
	  for ( i3 = 0; i3 < dis_garity[e->conditions[i2].predicate]; i3++ ) {
	    e->conditions[i2].args[i3] = e->conditions[i2+1].args[i3];
	  }
	}
	e->num_conditions--;
      }  

      i1 = 0;
      while ( i1 < e->num_adds-1 ) {
	i2 = i1+1;
	while ( i2 < e->num_adds ) {
	  if ( dis_identical_fact( &(e->adds[i1]), &(e->adds[i2]) ) ) {
	    for ( i3 = i2; i3 < e->num_adds-1; i3++ ) {
	      e->adds[i3].predicate = e->adds[i3+1].predicate;
	      for ( i4 = 0; i4 < dis_garity[e->adds[i3].predicate]; i4++ ) {
		e->adds[i3].args[i4] = e->adds[i3+1].args[i4];
	      }
	    }
	    e->num_adds--;
	  } else {
	    i2++;
	  }
	}
	i1++;
      }

      i1 = 0;
      while ( i1 < e->num_dels-1 ) {
	i2 = i1+1;
	while ( i2 < e->num_dels ) {
	  if ( dis_identical_fact( &(e->dels[i1]), &(e->dels[i2]) ) ) {
	    for ( i3 = i2; i3 < e->num_dels-1; i3++ ) {
	      e->dels[i3].predicate = e->dels[i3+1].predicate;
	      for ( i4 = 0; i4 < dis_garity[e->dels[i3].predicate]; i4++ ) {
		e->dels[i3].args[i4] = e->dels[i3+1].args[i4];
	      }
	    }
	    e->num_dels--;
	  } else {
	    i2++;
	  }
	}
	i1++;
      }
    }
  }

}



dis_Bool dis_identical_fact( dis_Fact *f1, dis_Fact *f2 )

{

  int i, a;

  if ( f1->predicate != f2->predicate ) {
    return dis_FALSE;
  }

  a = ( f1->predicate < 0 ) ? 2 : dis_garity[f1->predicate];

  for ( i = 0; i < a; i++ ) {
    if ( f1->args[i] != f2->args[i] ) {
      return dis_FALSE;
    }
  }

  return dis_TRUE;

} 



/* this one needs ONLY be used after unaries encoding, as all empty types
 * are already recognised during translation, except the artificial ones,
 * of course.
 */
void dis_handle_empty_easy_parameters( void )

{

  int i, j, k;
  Normdis_Operator *o;
  Normdis_Effect *e, *tmp;

  i = 0;
  while ( i < dis_gnum_easy_operators ) {
    o = dis_geasy_operators[i];

    for ( j = 0; j < o->num_vars; j++ ) {
      if ( dis_gtype_size[o->var_types[j]] == 0 ) {
	break;
      }
    }
    if ( j < o->num_vars ) {
      dis_free_Normdis_Operator( o );
      for ( k = i; k < dis_gnum_easy_operators - 1; k++ ) {
	dis_geasy_operators[k] = dis_geasy_operators[k+1];
      }
      dis_gnum_easy_operators--;
    } else {
      i++;
    }
  }

  for ( i = 0; i < dis_gnum_easy_operators; i++ ) {
    o = dis_geasy_operators[i];
    
    e = o->effects;
    while ( e ) {
      for ( j = 0; j < e->num_vars; j++ ) {
	if ( dis_gtype_size[e->var_types[j]] == 0 ) {
	  break;
	}
      }
      if ( j < e->num_vars ) {
	if ( e->prev ) {
	  e->prev->next = e->next;
	} else {
	  o->effects = e->next;
	}
	if ( e->next ) {
	  e->next->prev = e->prev;
	}
	tmp = e->next;
	dis_free_single_Normdis_Effect( e );
	e = tmp;
      } else {
	e = e->next;
      }
    }
  }

}










/****************************
 * UNARY INERTIA INTO TYPES *
 ****************************/












void dis_encode_easy_unaries_as_types( void )

{

  Normdis_Operator *o;
  int i1, i, j, k, l, new_T, p, a;
  dis_TypeArray T;
  int num_T;
  Normdis_Effect *e;
  int intersected_type, var;

  for ( i1 = 0; i1 < dis_gnum_easy_operators; i1++ ) {
    o = dis_geasy_operators[i1];

    for ( i = 0; i < o->num_vars; i++ ) {

      T[0] = o->var_types[i];
      num_T = 1;

      j = 0;
      while ( j < o->num_preconds ) {
	p = o->preconds[j].predicate;
	if ( ( (new_T = dis_gtype_to_predicate[p]) != -1 ) &&
	     ( o->preconds[j].args[0] == dis_ENCODE_VAR( i ) ) ) {
	  if ( num_T == dis_MAX_TYPE_INTERSECTIONS ) {
	    printf("\nincrease dis_MAX_TYPE_INTERSECTIONS (currently %d)\n\n",
		   dis_MAX_TYPE_INTERSECTIONS);
	    exit( 1 );
	  }
	  /* insert new type number into ordered array T;
	   * ---- all type numbers in T are different:
	   *      new nr. is of inferred type - can't be type declared for param
	   *      precondition facts occur at most once - doubles are removed
	   *                                              during cleanup
	   */
	  for ( k = 0; k < num_T; k++ ) {
	    if ( new_T < T[k] ) {
	      break;
	    }
	  }
  	  for ( l = num_T; l > k; l-- ) {
	    T[l] = T[l-1];
	  }
	  T[k] = new_T;
	  num_T++;
	  /* now remove superfluous precondition
	   */
	  for ( k = j; k < o->num_preconds-1; k++ ) {
	    o->preconds[k].predicate = o->preconds[k+1].predicate;
	    for ( l = 0; l < dis_garity[o->preconds[k].predicate]; l++ ) {
	      o->preconds[k].args[l] = o->preconds[k+1].args[l];
	    }
	  }
	  o->num_preconds--;
	} else {
	  j++;
	}
      }

      /* if we did not hit any unary inertia concerning this parameter
       * in the preconds, skip parameter and go to next one
       */
      if ( num_T == 1 ) {
	continue;
      }

      /* now we have the ordered array of types to intersect for param i 
       * of op o in array T of size num_T;
       * if there already is this intersected type, set type of this
       * param to its number, otherwise create the new intersected type.
       */
      if ( (intersected_type = dis_find_intersected_type( T, num_T )) != -1 ) {
	/* type already there
	 */
	o->var_types[i] = intersected_type;
	continue;
      }

      /* create new type
       */
      o->var_types[i] = dis_create_intersected_type( T, num_T );
    }

    for ( e = o->effects; e; e = e->next ) {
      for ( i = 0; i < e->num_vars; i++ ) {
	T[0] = e->var_types[i];
	var = o->num_vars + i;
	num_T = 1;
	j = 0;
	while ( j < e->num_conditions ) {
	  p = e->conditions[j].predicate;
	  if ( p < 0 ) {
	    j++;
	    continue;
	  }
	  if ( ( (new_T = dis_gtype_to_predicate[p]) != -1 ) &&
	       ( e->conditions[j].args[0] == dis_ENCODE_VAR( var ) ) ) {
	    if ( num_T == dis_MAX_TYPE_INTERSECTIONS ) {
	      printf("\nincrease dis_MAX_TYPE_INTERSECTIONS (currently %d)\n\n",
		     dis_MAX_TYPE_INTERSECTIONS);
	      exit( 1 );
	    }
	    for ( k = 0; k < num_T; k++ ) {
	      if ( new_T < T[k] ) {
		break;
	      }
	    }
	    for ( l = num_T; l > k; l-- ) {
	      T[l] = T[l-1];
	    }
	    T[k] = new_T;
	    num_T++;
	    for ( k = j; k < e->num_conditions-1; k++ ) {
	      e->conditions[k].predicate = e->conditions[k+1].predicate;
	      a = ( e->conditions[k].predicate < 0 ) ?
		2 : dis_garity[e->conditions[k].predicate];
	      for ( l = 0; l < a; l++ ) {
		e->conditions[k].args[l] = e->conditions[k+1].args[l];
	      }
	    }
	    e->num_conditions--;
	  } else {
	    j++;
	  }
	}
	if ( num_T == 1 ) {
	  continue;
	}
	if ( (intersected_type = dis_find_intersected_type( T, num_T )) != -1 ) {
	  e->var_types[i] = intersected_type;
	  continue;
	}
	e->var_types[i] = dis_create_intersected_type( T, num_T );
      }
    }
  }

  dis_handle_empty_easy_parameters();

}



int dis_create_intersected_type( dis_TypeArray T, int num_T )

{

  int i, j, k, intersected_type;

  if ( dis_gnum_types == dis_MAX_TYPES ) {
    printf("\ntoo many (inferred and intersected) types! increase dis_MAX_TYPES (currently %d)\n\n",
	   dis_MAX_TYPES);
    exit( 1 );
  } 
  dis_gtype_names[dis_gnum_types] = NULL;
  dis_gtype_size[dis_gnum_types] = 0;
  for ( i = 0; i < dis_MAX_CONSTANTS; i++ ) {
    dis_gis_member[i][dis_gnum_types] = dis_FALSE;
  }
  for ( i = 0; i < num_T; i++ ) {
    dis_gintersected_types[dis_gnum_types][i] = T[i];
  }
  dis_gnum_intersected_types[dis_gnum_types] = num_T;
  intersected_type = dis_gnum_types;
  dis_gnum_types++;

  for ( j = 0; j < dis_gtype_size[T[0]]; j++ ) {
    for ( k = 1; k < num_T; k++ ) {
      if ( !dis_gis_member[dis_gtype_consts[T[0]][j]][T[k]] ) {
	break;
      }
    }
    if ( k < num_T ) {
      continue;
    }
    /* add constant to new type
     */
    if ( dis_gtype_size[intersected_type] == dis_MAX_TYPE ) {
      printf("\ntoo many consts in intersected type! increase dis_MAX_TYPE (currently %d)\n\n",
	     dis_MAX_TYPE);
      exit( 1 );
    }
    dis_gtype_consts[intersected_type][dis_gtype_size[intersected_type]++] = dis_gtype_consts[T[0]][j];
    dis_gis_member[dis_gtype_consts[T[0]][j]][intersected_type] = dis_TRUE;
  }
  
  /* now verify if the intersected type equals one of the types that we intersected.
   * this is the case, iff one of the types in T has the same size as intersected_type
   */
  for ( j = 0; j < num_T; j++ ) {
    if ( dis_gtype_size[intersected_type] != dis_gtype_size[T[j]] ) {
      continue;
    }
    /* type T[j] contains exactly the constants that we need!
     *
     * remove intersected type from table!
     */
    dis_gtype_size[intersected_type] = 0;
    for ( k = 0; k < dis_MAX_CONSTANTS; k++ ) {
      dis_gis_member[k][intersected_type] = dis_FALSE;
    }
    dis_gnum_intersected_types[intersected_type] = -1;
    dis_gnum_types--;
    intersected_type = T[j];
    break;
  }

  return intersected_type;

}



int dis_find_intersected_type( dis_TypeArray T, int num_T )

{

  int i, j;

  for ( i = 0; i < dis_gnum_types; i++ ) {
    if ( dis_gnum_intersected_types[i] == -1 ) {
      continue;
    }

    if ( dis_gnum_intersected_types[i] != num_T ) {
      continue;
    }

    for ( j = 0; j < num_T; j++ ) {
      if ( T[j] != dis_gintersected_types[i][j] ) {
	break;
      }
    }
    if ( j < num_T ) {
      continue;
    }

    return i;
  }

  return -1;

}
  













/******************************
 * MULTIPLY EFFECT PARAMETERS *
 ******************************/












/* local globals for multiplying
 */

int dis_linertia_conds[dis_MAX_VARS];
int lnum_inertia_conds;

int dis_lmultiply_parameters[dis_MAX_VARS];
int lnum_multiply_parameters;

Normdis_Operator *lo;
Normdis_Effect *le;

Normdis_Effect *lres;






void dis_multiply_easy_effect_parameters( void )

{

  int i, j, k, l, p, par;
  Normdis_Effect *e;

  for ( i = 0; i < dis_gnum_easy_operators; i++ ) {
    lo = dis_geasy_operators[i];

    lres = NULL;
    for ( e = lo->effects; e; e = e->next ) {
      le = e;

      lnum_inertia_conds = 0;
      for ( j = 0; j < e->num_conditions; j++ ) {
	for ( k = 0; k < dis_garity[e->conditions[j].predicate]; k++ ) {
	  if ( e->conditions[j].args[k] < 0 &&
	       dis_DECODE_VAR( e->conditions[j].args[k] ) < lo->num_vars ) {
	    break;
	  }
	}
	if ( k < dis_garity[e->conditions[j].predicate] ) {
	  /* only consider inertia constraining effect parameters
	   */
	  continue;
	}
	if ( !dis_gis_added[e->conditions[j].predicate] &&
	     !dis_gis_deleted[e->conditions[j].predicate] ) {
	  dis_linertia_conds[lnum_inertia_conds++] = j;
	}
      }

      lnum_multiply_parameters = 0;
      for ( j = 0; j < e->num_vars; j++ ) {
	par = lo->num_vars + j;
	for ( k = 0; k < lnum_inertia_conds; k++ ) {
	  p = e->conditions[dis_linertia_conds[k]].predicate;
	  for ( l = 0; l < dis_garity[p]; l++ ) {
	    if ( e->conditions[dis_linertia_conds[k]].args[l] ==
		 dis_ENCODE_VAR( par ) ) {
	      break;
	    }
	  }
	  if ( l < dis_garity[p] ) {
	    break;
	  }
	}
	if ( k < lnum_inertia_conds ) {
	  continue;
	}
	dis_lmultiply_parameters[lnum_multiply_parameters++] = j;
      }

      dis_unify_easy_inertia_conditions( 0 );
    }
    dis_free_Normdis_Effect( lo->effects );
    lo->effects = lres;
  }

}



void dis_unify_easy_inertia_conditions( int curr_inertia )

{

  int p, i, j, af, hh;
  int args[dis_MAX_VARS];
  int affected_params[dis_MAX_VARS];
  int num_affected_params = 0;

  if ( curr_inertia == lnum_inertia_conds ) {
    dis_multiply_easy_non_constrained_effect_parameters( 0 );
    return;
  }

  p = le->conditions[dis_linertia_conds[curr_inertia]].predicate;
  for ( i = 0; i < dis_garity[p]; i++ ) {
    args[i] = le->conditions[dis_linertia_conds[curr_inertia]].args[i];
    if ( args[i] < 0 ) {
      hh = dis_DECODE_VAR( args[i] );
      hh -= lo->num_vars;
      if ( le->inst_table[hh] != -1 ) {
	args[i] = le->inst_table[hh];
      } else {
	affected_params[num_affected_params++] = hh;
      }
    }
  }

  for ( i = 0; i < dis_gnum_initial_predicate[p]; i++ ) {
    af = 0;
    for ( j = 0; j < dis_garity[p]; j++ ) {
      if ( args[j] >= 0 ) {
	if ( args[j] != dis_ginitial_predicate[p][i].args[j] ) {
	  break;
	} else {
	  continue;
	}
      }
      le->inst_table[affected_params[af++]] = dis_ginitial_predicate[p][i].args[j];
    }
    if ( j < dis_garity[p] ) {
      continue;
    }

    dis_unify_easy_inertia_conditions( curr_inertia + 1 );
  }

  for ( i = 0; i < num_affected_params; i++ ) {
    le->inst_table[affected_params[i]] = -1;
  }

}



void dis_multiply_easy_non_constrained_effect_parameters( int curr_parameter )

{

  int t, n, i, j, k, p, par;
  Normdis_Effect *tmp;
  dis_Bool rem;

  if ( curr_parameter == lnum_multiply_parameters ) {
    /* create new effect, adjusting conds to inst, and
     * partially instantiating effects;
     *
     * add result to  lres
     */
    tmp = dis_new_Normdis_Effect2( le );

    /* instantiate param occurences
     */
    for ( i = 0; i < le->num_vars; i++ ) {
      par = lo->num_vars + i;

      /* numerical part
       */
      for ( j = 0; j < tmp->num_numeric_conditions; j++ ) {
	dis_replace_var_with_const_in_exp( &(tmp->numeric_conditions_lh[j]), 
				       par, le->inst_table[i] );
      }
      for ( j = 0; j < tmp->num_numeric_conditions; j++ ) {
	dis_replace_var_with_const_in_exp( &(tmp->numeric_conditions_rh[j]), 
				       par, le->inst_table[i] );
      }
      /* was that already enough to get numbers? if yes,
       * see whether comparison holds or not.
       */
      j = 0;
      while ( j < tmp->num_numeric_conditions ) {
	if ( tmp->numeric_conditions_lh[j]->connective == NUMBER &&
	     tmp->numeric_conditions_rh[j]->connective == NUMBER ) {
	  if ( dis_number_comparison_holds( tmp->numeric_conditions_comp[j],
					tmp->numeric_conditions_lh[j]->value,
					tmp->numeric_conditions_rh[j]->value ) ) {
	    dis_free_dis_ExpNode( tmp->numeric_conditions_lh[j] );
	    dis_free_dis_ExpNode( tmp->numeric_conditions_rh[j] );
	    for ( k = j; k < tmp->num_numeric_conditions-1; k++ ) {
	      tmp->numeric_conditions_comp[k] = tmp->numeric_conditions_comp[k+1];
	      tmp->numeric_conditions_lh[k] = tmp->numeric_conditions_lh[k+1];
	      tmp->numeric_conditions_rh[k] = tmp->numeric_conditions_rh[k+1];
	    }
	    tmp->num_numeric_conditions--;
	  } else {
	    dis_free_Normdis_Effect( tmp );
	    return;
	  }
	} else {
	  j++;
	}
      }
      for ( j = 0; j < tmp->num_numeric_effects; j++ ) {
	for ( k = 0; k < dis_garity[tmp->numeric_effects_fluent[j].function]; k++ ) {
	  if ( tmp->numeric_effects_fluent[j].args[k] == dis_ENCODE_VAR( par ) ) {
	    tmp->numeric_effects_fluent[j].args[k] = le->inst_table[i];
	  }
	}
      }
      for ( j = 0; j < tmp->num_numeric_effects; j++ ) {
	dis_replace_var_with_const_in_exp( &(tmp->numeric_effects_rh[j]), 
				       par, le->inst_table[i] );
      }

      /* logical part
       */
      for ( j = 0; j < tmp->num_conditions; j++ ) {
	for ( k = 0; k < dis_garity[tmp->conditions[j].predicate]; k++ ) {
	  if ( tmp->conditions[j].args[k] == dis_ENCODE_VAR( par ) ) {
	    tmp->conditions[j].args[k] = le->inst_table[i];
	  }
	}
      }
      for ( j = 0; j < tmp->num_adds; j++ ) {
	for ( k = 0; k < dis_garity[tmp->adds[j].predicate]; k++ ) {
	  if ( tmp->adds[j].args[k] == dis_ENCODE_VAR( par ) ) {
	    tmp->adds[j].args[k] = le->inst_table[i];
	  }
	}
      }
      for ( j = 0; j < tmp->num_dels; j++ ) {
	for ( k = 0; k < dis_garity[tmp->dels[j].predicate]; k++ ) {
	  if ( tmp->dels[j].args[k] == dis_ENCODE_VAR( par ) ) {
	    tmp->dels[j].args[k] = le->inst_table[i];
	  }
	}
      }
    }
    /* adjust conditions
     */
    i = 0;
    while ( i < tmp->num_conditions ) {
      rem = dis_FALSE;
      p = tmp->conditions[i].predicate;
      if ( !dis_gis_added[p] &&
	   !dis_gis_deleted[p] ) {
	for ( j = 0; j < dis_garity[p]; j++ ) {
	  if ( tmp->conditions[i].args[j] < 0 &&
	       dis_DECODE_VAR( tmp->conditions[i].args[j] < lo->num_vars ) ) {
	    break;
	  }
	}
	if ( j == dis_garity[p] ) {
	  /* inertia that constrain only effect params have been unified,
	   * are therefore dis_TRUE
	   */
	  rem = dis_TRUE;
	}
      }
      if ( rem ) {
	for ( j = i; j < tmp->num_conditions - 1; j++ ) {
	  tmp->conditions[j].predicate = tmp->conditions[j+1].predicate;
	  for ( k = 0; k < dis_garity[tmp->conditions[j+1].predicate]; k++ ) {
	    tmp->conditions[j].args[k] = tmp->conditions[j+1].args[k];
	  }
	}
	tmp->num_conditions--;
      } else {
	i++;
      }
    }
    /* add result to lres
     */
    if ( lres ) {
      lres->prev = tmp;
    }
    tmp->next = lres;
    lres = tmp;
    return;
  }

  t = le->var_types[dis_lmultiply_parameters[curr_parameter]];
  n = dis_gtype_size[t];

  for ( i = 0; i < n; i++ ) {
    le->inst_table[dis_lmultiply_parameters[curr_parameter]] = dis_gtype_consts[t][i];
    dis_multiply_easy_non_constrained_effect_parameters( curr_parameter + 1 );
  }

  le->inst_table[dis_lmultiply_parameters[curr_parameter]] = -1;

}



















/**************************
 * MULTIPLY OP PARAMETERS *
 **************************/



















void dis_multiply_easy_op_parameters( void )

{

  int i, j, k, l, p;
  Normdis_Operator *o;

  dis_geasy_templates = NULL;
  dis_gnum_easy_templates = 0;

  for ( i = 0; i < dis_gnum_easy_operators; i++ ) {
    lo = dis_geasy_operators[i];

    lnum_inertia_conds = 0;
    for ( j = 0; j < lo->num_preconds; j++ ) {
      if ( !dis_gis_added[lo->preconds[j].predicate] &&
	   !dis_gis_deleted[lo->preconds[j].predicate] ) {
	dis_linertia_conds[lnum_inertia_conds++] = j;
      }
    }
      
    lnum_multiply_parameters = 0;
    for ( j = 0; j < lo->num_vars; j++ ) {
      for ( k = 0; k < lnum_inertia_conds; k++ ) {
	p = lo->preconds[dis_linertia_conds[k]].predicate;
	for ( l = 0; l < dis_garity[p]; l++ ) {
	  if ( lo->preconds[dis_linertia_conds[k]].args[l] ==
	       dis_ENCODE_VAR( j ) ) {
	    break;
	  }
	}
	if ( l < dis_garity[p] ) {
	  break;
	}
      }
      if ( k < lnum_inertia_conds ) {
	continue;
      }
      dis_lmultiply_parameters[lnum_multiply_parameters++] = j;
    }

    dis_unify_easy_inertia_preconds( 0 );
  }

  /* now remove inertia preconditions from operator schemata
   */
  for ( i = 0; i < dis_gnum_easy_operators; i++ ) {
    o = dis_geasy_operators[i];

    j = 0;
    while ( j < o->num_preconds ) {
      if ( !dis_gis_added[o->preconds[j].predicate] &&
	   !dis_gis_deleted[o->preconds[j].predicate] ) {
	for ( k = j; k < o->num_preconds - 1; k++ ) { 
 	  o->preconds[k].predicate = o->preconds[k+1].predicate;
	  for ( l = 0; l < dis_garity[o->preconds[k].predicate]; l++ ) {
	    o->preconds[k].args[l] = o->preconds[k+1].args[l];
	  }
	}
	o->num_preconds--;
      } else {
	j++;
      }
    }
  }   

}



void dis_unify_easy_inertia_preconds( int curr_inertia )

{

  int p, i, j, af, hh;
  int args[dis_MAX_VARS];
  int affected_params[dis_MAX_VARS];
  int num_affected_params = 0;

  if ( curr_inertia == lnum_inertia_conds ) {
    dis_multiply_easy_non_constrained_op_parameters( 0 );
    return;
  }

  p = lo->preconds[dis_linertia_conds[curr_inertia]].predicate;
  for ( i = 0; i < dis_garity[p]; i++ ) {
    args[i] = lo->preconds[dis_linertia_conds[curr_inertia]].args[i];
    if ( args[i] < 0 ) {
      hh = dis_DECODE_VAR( args[i] );
      if ( lo->inst_table[hh] != -1 ) {
	args[i] = lo->inst_table[hh];
      } else {
	affected_params[num_affected_params++] = hh;
      }
    }
  }

  for ( i = 0; i < dis_gnum_initial_predicate[p]; i++ ) {
    af = 0;
    for ( j = 0; j < dis_garity[p]; j++ ) {
      if ( args[j] >= 0 ) {
	if ( args[j] != dis_ginitial_predicate[p][i].args[j] ) {
	  break;
	} else {
	  continue;
	}
      }
      /* check whether that constant has the correct type for that
       * parameter (can be not fulfilled due to encoding of unary inertia
       */
      if ( !dis_gis_member[dis_ginitial_predicate[p][i].args[j]][lo->var_types[affected_params[af]]] ) {
	break;
      }
      /* legal constant; set op parameter instantiation to it
       */
      lo->inst_table[affected_params[af++]] = dis_ginitial_predicate[p][i].args[j];
    }
    if ( j < dis_garity[p] ) {
      continue;
    }

    dis_unify_easy_inertia_preconds( curr_inertia + 1 );
  }

  for ( i = 0; i < num_affected_params; i++ ) {
    lo->inst_table[affected_params[i]] = -1;
  }

}



void dis_multiply_easy_non_constrained_op_parameters( int curr_parameter )

{

  dis_EasyTemplate *tmp;
  int i, j, t, n;

  if ( curr_parameter == lnum_multiply_parameters ) {
    tmp = dis_new_dis_EasyTemplate( lo );
    for ( i = 0; i < lo->num_vars; i++ ) {
      tmp->inst_table[i] = lo->inst_table[i];
    }
    tmp->next = dis_geasy_templates;
    if ( dis_geasy_templates ) {
      dis_geasy_templates->prev = tmp;
    }
    dis_geasy_templates = tmp;
    dis_gnum_easy_templates++;
    return;
  }

  if ( curr_parameter == lnum_multiply_parameters - 1 ) {
    t = lo->var_types[dis_lmultiply_parameters[curr_parameter]];
    n = dis_gtype_size[t];
    for ( i = 0; i < n; i++ ) {
      lo->inst_table[dis_lmultiply_parameters[curr_parameter]] = dis_gtype_consts[t][i];

      tmp = dis_new_dis_EasyTemplate( lo );
      for ( j = 0; j < lo->num_vars; j++ ) {
	tmp->inst_table[j] = lo->inst_table[j];
      }
      tmp->next = dis_geasy_templates;
      if ( dis_geasy_templates ) {
	dis_geasy_templates->prev = tmp;
      }
      dis_geasy_templates = tmp;
      dis_gnum_easy_templates++;
    }

    lo->inst_table[dis_lmultiply_parameters[curr_parameter]] = -1;

    return;
  }

  t = lo->var_types[dis_lmultiply_parameters[curr_parameter]];
  n = dis_gtype_size[t];
  for ( i = 0; i < n; i++ ) {
    lo->inst_table[dis_lmultiply_parameters[curr_parameter]] = dis_gtype_consts[t][i];

    dis_multiply_easy_non_constrained_op_parameters( curr_parameter + 1 );
  }

  lo->inst_table[dis_lmultiply_parameters[curr_parameter]] = -1;

}
