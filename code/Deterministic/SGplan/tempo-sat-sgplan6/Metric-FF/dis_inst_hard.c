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

 * File: dis_inst_hard.c 

 * Description: modified from inst_hard.c in Metric-FF

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
 * File: inst_hard.c
 * Description: functions for multiplying hard operators.
 *
 *
 * Author: Joerg Hoffmann 2000
 *
 *********************************************************************/ 









#include "dis_ff.h"

#include "dis_output.h"
#include "dis_memory.h"

#include "dis_expressions.h"

#include "dis_inst_pre.h"
#include "dis_inst_hard.h" 











/* used in multiplying routines
 */
int dis_linst_table[dis_MAX_VARS];
//dis_int_pointer dis_lini[dis_MAX_PREDICATES];
int **dis_lini;



void dis_build_hard_action_templates( void )

{

  int i, j, size, adr;
  Mixeddis_Operator *o;

  /* remove unused params; empty types are already recognised during
   * domain translation; have to be handled after (or while)
   * unaries encoding (if done), though.
   */
  dis_cleanup_hard_domain();

  if ( dis_gcmd_line.display_info == 115 ) {
    printf("\n\ncleaned up hard domain representation is:\n\n");
    for ( i = 0; i < dis_gnum_hard_operators; i++ ) {
      dis_print_dis_Operator( dis_ghard_operators[i] );
    }
    fflush( stdout );
  }

  /* create local table of instantiated facts that occur in the
   * initial state. for fast finding out if fact is in ini or not.
   */
  dis_lini = (int **) calloc(dis_gnum_predicates, sizeof(int *));
  
  for ( i = 0; i < dis_gnum_predicates; i++ ) {
    size = 1;
    for ( j = 0; j < dis_garity[i]; j++ ) {
        size *= dis_gtype_size[dis_gpredicates_args_type[i][j]]; 
        // size *= dis_gnum_constants;
    }
    dis_lini[i] = ( dis_int_pointer ) calloc( size, sizeof( int ) );
    for ( j = 0; j < size; j++ ) {
      dis_lini[i][j] = 0;
    }
    for ( j = 0; j < dis_gnum_initial_predicate[i]; j++ ) {
      adr = instantiated_dis_fact_adress( &dis_ginitial_predicate[i][j] );
      dis_lini[i][adr]++;
    }
  }


  /* create mixed op for each param combination
   */
  dis_multiply_hard_op_parameters();

  if ( dis_gcmd_line.display_info == 116 ) {
    printf("\n\nmixed hard domain representation is:\n\n");
    for ( o = dis_ghard_mixed_operators; o; o = o->next ) {
      dis_print_Mixeddis_Operator( o );
    }
    fflush( stdout );
  }

  /* create pseudo op for each mixed op
   */
  dis_multiply_hard_effect_parameters();
 
  if ( dis_gcmd_line.display_info == 117 ) {
    printf("\n\npseudo hard domain representation is:\n\n");
    for ( i = 0; i < dis_gnum_hard_templates; i++ ) {
      dis_print_dis_Pseudodis_Action( dis_ghard_templates[i] );
    }
    fflush( stdout );
  }
 

  for ( i = 0; i < dis_gnum_predicates; i++ )
          xfree(dis_lini[i]);    
  xfree(dis_lini);
          
}












/****************
 * CLEANUP CODE *
 ****************/












void dis_cleanup_hard_domain( void )

{

  int i, j, k, par;
  dis_Operator *o;
  dis_Effect *e;

  /* so far, only unused parameters removal
   */

  for ( i = 0; i < dis_gnum_hard_operators; i++ ) {
    o = dis_ghard_operators[i];

    j = 0;
    while ( j < o->num_vars ) {
      if ( dis_var_used_in_wff( dis_ENCODE_VAR( j ), o->preconds ) ) {
	j++;
	continue;
      }

      for ( e = o->effects; e; e = e->next ) {
	if ( dis_var_used_in_wff( dis_ENCODE_VAR( j ), e->conditions ) ) {
	  break;
	}
	if ( dis_var_used_in_literals( dis_ENCODE_VAR( j ), e->effects ) ) {
	  break;
	}
	if ( dis_var_used_in_numeric_effects( dis_ENCODE_VAR( j ), e->numeric_effects ) ) {
	  break;
	}
      }
      if ( e ) {
	j++;
	continue;
      }

      o->removed[j] = dis_TRUE;
      j++;
    }

    for ( e = o->effects; e; e = e->next ) {
      j = 0;
      while ( j < e->num_vars ) {
	par = o->num_vars + j;
	if ( dis_var_used_in_wff( dis_ENCODE_VAR( par ), e->conditions ) ) {
	  j++;
	  continue;
	}
	if ( dis_var_used_in_literals( dis_ENCODE_VAR( par ), e->effects ) ) {
	  j++;
	  continue;
	}
	if ( dis_var_used_in_numeric_effects( dis_ENCODE_VAR( par ), e->numeric_effects ) ) {
	  j++;
	  continue;
	}

	if ( e->var_names[j] ) {
	  free( e->var_names[j] );
	}
	for ( k = j; k < e->num_vars - 1; k++ ) {
	  e->var_names[k] = e->var_names[k+1];
	  e->var_names[k] = e->var_names[k+1];
	}
	e->num_vars--;
	dis_decrement_inferior_vars( par, e->conditions );
	dis_decrement_inferior_vars_in_literals( par, e->effects );
	dis_decrement_inferior_vars_in_numeric_effects( par, e->numeric_effects );
      }
    }
  }

}



dis_Bool dis_var_used_in_literals( int code_var, dis_Literal *ef )

{

  dis_Literal *l;
  int i;
  
  for ( l = ef; l; l = l->next ) {
    for ( i = 0; i < dis_garity[l->fact.predicate]; i++ ) {
      if ( l->fact.args[i] == code_var ) {
	return dis_TRUE;
      }
    }
  }

  return dis_FALSE;

}



dis_Bool dis_var_used_in_numeric_effects( int code_var, dis_Numericdis_Effect *ef )

{

  dis_Numericdis_Effect *l;
  int i;
  
  for ( l = ef; l; l = l->next ) {
    for ( i = 0; i < dis_gf_arity[l->fluent.function]; i++ ) {
      if ( l->fluent.args[i] == code_var ) {
	return dis_TRUE;
      }
    }
    if ( dis_var_used_in_exp( code_var, l->rh ) ) {
      return dis_TRUE;
    }
  }

  return dis_FALSE;

}



void dis_decrement_inferior_vars_in_literals( int var, dis_Literal *ef )

{

  dis_Literal *l;
  int i;
  
  for ( l = ef; l; l = l->next ) {
    for ( i = 0; i < dis_garity[l->fact.predicate]; i++ ) {
      if ( l->fact.args[i] >= 0 ) {
	continue;
      }
      if ( dis_DECODE_VAR( l->fact.args[i] ) > var ) {
	l->fact.args[i]++;
      }
    }
  }

}



void dis_decrement_inferior_vars_in_numeric_effects( int var, dis_Numericdis_Effect *ef )

{

  dis_Numericdis_Effect *l;
  int i;
  
  for ( l = ef; l; l = l->next ) {
    for ( i = 0; i < dis_gf_arity[l->fluent.function]; i++ ) {
      if ( l->fluent.args[i] >= 0 ) {
	continue;
      }
      if ( dis_DECODE_VAR( l->fluent.args[i] ) > var ) {
	l->fluent.args[i]++;
      }
    }
    dis_decrement_inferior_vars_in_exp( var, l->rh );
  }

}














/******************************
 * CODE THAT BUILDS MIXED OPS *
 ******************************/














void dis_multiply_hard_op_parameters( void )

{

  int i;

  dis_ghard_mixed_operators = NULL;

  for ( i = 0; i < dis_MAX_VARS; i++ ) {
    dis_linst_table[i] = -1;
  }

  for ( i = 0; i < dis_gnum_hard_operators; i++ ) {
    dis_create_hard_mixed_operators( dis_ghard_operators[i], 0 );
  }

}



void dis_create_hard_mixed_operators( dis_Operator *o, int curr_var )

{

  int t, i, m, mn;
  dis_WffNode *tmp1, *w, *ww;
  Mixeddis_Operator *tmp2;

  if ( curr_var < o->num_vars ) {
    if ( o->removed[curr_var] ) {
      /* param doesn't matter -- select any appropriate type constant
       * at least one there; otherwise, op would not have been translated.
       */
      dis_linst_table[curr_var] = dis_gtype_consts[o->var_types[curr_var]][0];
      dis_create_hard_mixed_operators( o, curr_var + 1 );
      dis_linst_table[curr_var] = -1;
      return;
    }

    t = o->var_types[curr_var];
    for ( i = 0; i < dis_gtype_size[t]; i++ ) {
      dis_linst_table[curr_var] = dis_gtype_consts[t][i];

      dis_create_hard_mixed_operators( o, curr_var + 1 );

      dis_linst_table[curr_var] = -1;
    }
    return;
  }


  tmp1 = dis_instantiate_wff( o->preconds );

  if ( tmp1->connective == dis_FAL ) {
    dis_free_dis_WffNode( tmp1 );
    return;
  }

  dis_dnf( &tmp1 );
  dis_cleanup_wff( &tmp1 );

  if ( tmp1->connective == dis_FAL ) {
    dis_free_dis_WffNode( tmp1 );
    return;
  }

  /* only debugging, REMOVE LATER
   */
  if ( dis_is_dis_dnf( tmp1 ) == -1 ) {
    printf("\n\nILLEGAL DNF %s AFTER INSTANTIATION\n\n", o->name);
    dis_print_Wff( tmp1, 0 );
    exit( 1 );
  }

  switch ( tmp1->connective ) {
  case dis_OR:
    for ( w = tmp1->sons; w; w = w->next ) {
      tmp2 = dis_new_Mixeddis_Operator( o );
      for ( i = 0; i < o->num_vars; i++ ) {
	tmp2->inst_table[i] = dis_linst_table[i];
      }
      if ( w->connective == dis_AND ) {
	m = 0;
	mn = 0;
	for ( ww = w->sons; ww; ww = ww->next ) {
	  if ( ww->connective == dis_ATOM ) m++;
	  if ( ww->connective == dis_COMP ) mn++;
	}
	tmp2->preconds = ( dis_Fact * ) calloc( m, sizeof( dis_Fact ) );
	tmp2->numeric_preconds_comp = ( dis_Comparator * ) calloc( mn, sizeof( dis_Comparator ) );
	tmp2->numeric_preconds_lh = ( dis_ExpNode_pointer * ) calloc( mn, sizeof( dis_ExpNode_pointer ) );
	tmp2->numeric_preconds_rh = ( dis_ExpNode_pointer * ) calloc( mn, sizeof( dis_ExpNode_pointer ) );
	tmp2->num_preconds = m;
	tmp2->num_numeric_preconds = mn;
	m = 0; mn = 0;
	for ( ww = w->sons; ww; ww = ww->next ) {
	  if ( ww->connective == dis_ATOM ) {
	    tmp2->preconds[m].predicate = ww->fact->predicate;
	    for ( i = 0; i < dis_garity[ww->fact->predicate]; i++ ) {
	      tmp2->preconds[m].args[i] = ww->fact->args[i];
	    }
	    m++;
	  }
	  if ( ww->connective == dis_COMP ) {
	    tmp2->numeric_preconds_comp[mn] = ww->comp;
	    tmp2->numeric_preconds_lh[mn] = dis_copy_Exp( ww->lh );
	    tmp2->numeric_preconds_rh[mn] = dis_copy_Exp( ww->rh );
	    mn++;
	  }
	}
      } else {
	if ( w->connective == dis_ATOM ) {
	  tmp2->preconds = ( dis_Fact * ) calloc( 1, sizeof( dis_Fact ) );
	  tmp2->num_preconds = 1;
	  tmp2->preconds[0].predicate = w->fact->predicate;
	  for ( i = 0; i < dis_garity[w->fact->predicate]; i++ ) {
	    tmp2->preconds[0].args[i] = w->fact->args[i];
	  }
	}
	if ( w->connective == dis_COMP ) {
	  tmp2->numeric_preconds_comp = ( dis_Comparator * ) calloc( 1, sizeof( dis_Comparator ) );
	  tmp2->numeric_preconds_lh = ( dis_ExpNode_pointer * ) calloc( 1, sizeof( dis_ExpNode_pointer ) );
	  tmp2->numeric_preconds_rh = ( dis_ExpNode_pointer * ) calloc( 1, sizeof( dis_ExpNode_pointer ) );
	  tmp2->numeric_preconds_comp[0] = w->comp;
	  tmp2->numeric_preconds_lh[0] = dis_copy_Exp( w->lh );
	  tmp2->numeric_preconds_rh[0] = dis_copy_Exp( w->rh );
	  tmp2->num_numeric_preconds = 1;
	}
      }
      tmp2->effects = dis_instantiate_dis_Effect( o->effects );
      tmp2->next = dis_ghard_mixed_operators;
      dis_ghard_mixed_operators = tmp2;
      dis_gnum_hard_mixed_operators++;
    }
    break;
  case dis_AND:
    tmp2 = dis_new_Mixeddis_Operator( o );
    for ( i = 0; i < o->num_vars; i++ ) {
      tmp2->inst_table[i] = dis_linst_table[i];
    }
    m = 0;
    mn = 0;
    for ( w = tmp1->sons; w; w = w->next ) {
      if ( w->connective == dis_ATOM ) m++;
      if ( w->connective == dis_COMP ) mn++;
    }
    tmp2->preconds = ( dis_Fact * ) calloc( m, sizeof( dis_Fact ) );
    tmp2->numeric_preconds_comp = ( dis_Comparator * ) calloc( mn, sizeof( dis_Comparator ) );
    tmp2->numeric_preconds_lh = ( dis_ExpNode_pointer * ) calloc( mn, sizeof( dis_ExpNode_pointer ) );
    tmp2->numeric_preconds_rh = ( dis_ExpNode_pointer * ) calloc( mn, sizeof( dis_ExpNode_pointer ) );
    tmp2->num_preconds = m;
    tmp2->num_numeric_preconds = mn;
    m = 0; mn = 0;
    for ( w = tmp1->sons; w; w = w->next ) {
      if ( w->connective == dis_ATOM ) {
	tmp2->preconds[m].predicate = w->fact->predicate;
	for ( i = 0; i < dis_garity[w->fact->predicate]; i++ ) {
	  tmp2->preconds[m].args[i] = w->fact->args[i];
	}
	m++;
      }
      if ( w->connective == dis_COMP ) {
	tmp2->numeric_preconds_comp[mn] = w->comp;
	tmp2->numeric_preconds_lh[mn] = dis_copy_Exp( w->lh );
	tmp2->numeric_preconds_rh[mn] = dis_copy_Exp( w->rh );
	mn++;
      }
    }
    tmp2->effects = dis_instantiate_dis_Effect( o->effects );
    tmp2->next = dis_ghard_mixed_operators;
    dis_ghard_mixed_operators = tmp2;
    dis_gnum_hard_mixed_operators++;
    break;
  case dis_ATOM:
    tmp2 = dis_new_Mixeddis_Operator( o );
    for ( i = 0; i < o->num_vars; i++ ) {
      tmp2->inst_table[i] = dis_linst_table[i];
    }
    tmp2->preconds = ( dis_Fact * ) calloc( 1, sizeof( dis_Fact ) );
    tmp2->num_preconds = 1;
    tmp2->preconds[0].predicate = tmp1->fact->predicate;
    for ( i = 0; i < dis_garity[tmp1->fact->predicate]; i++ ) {
      tmp2->preconds[0].args[i] = tmp1->fact->args[i];
    }
    tmp2->effects = dis_instantiate_dis_Effect( o->effects );
    tmp2->next = dis_ghard_mixed_operators;
    dis_ghard_mixed_operators = tmp2;
    dis_gnum_hard_mixed_operators++;
    break;
  case dis_COMP:
    tmp2 = dis_new_Mixeddis_Operator( o );
    for ( i = 0; i < o->num_vars; i++ ) {
      tmp2->inst_table[i] = dis_linst_table[i];
    }
    tmp2->numeric_preconds_comp = ( dis_Comparator * ) calloc( 1, sizeof( dis_Comparator ) );
    tmp2->numeric_preconds_lh = ( dis_ExpNode_pointer * ) calloc( 1, sizeof( dis_ExpNode_pointer ) );
    tmp2->numeric_preconds_rh = ( dis_ExpNode_pointer * ) calloc( 1, sizeof( dis_ExpNode_pointer ) );
    tmp2->numeric_preconds_comp[0] = tmp1->comp;
    tmp2->numeric_preconds_lh[0] = dis_copy_Exp( tmp1->lh );
    tmp2->numeric_preconds_rh[0] = dis_copy_Exp( tmp1->rh );
    tmp2->num_numeric_preconds = 1;
    tmp2->effects = dis_instantiate_dis_Effect( o->effects );
    tmp2->next = dis_ghard_mixed_operators;
    dis_ghard_mixed_operators = tmp2;
    dis_gnum_hard_mixed_operators++;    
    break;
  case dis_TRU:
    tmp2 = dis_new_Mixeddis_Operator( o );
    for ( i = 0; i < o->num_vars; i++ ) {
      tmp2->inst_table[i] = dis_linst_table[i];
    }
    tmp2->effects = dis_instantiate_dis_Effect( o->effects );
    tmp2->next = dis_ghard_mixed_operators;
    dis_ghard_mixed_operators = tmp2;
    dis_gnum_hard_mixed_operators++;
    break;
  default:
    printf("\n\nillegal connective %d in parsing DNF precond.\n\n",
	   tmp1->connective);
    exit( 1 );
  }

  dis_free_dis_WffNode( tmp1 );

}



dis_Effect *dis_instantiate_dis_Effect( dis_Effect *e )

{

  dis_Effect *res = NULL, *tmp, *i;
  dis_Literal *tt, *l;
  dis_Numericdis_Effect *ne, *ttt;
  int j;

  for ( i = e; i; i = i->next ) {
    tmp = dis_new_dis_Effect();

    for ( j = 0; j < i->num_vars; j++ ) {
      tmp->var_types[j] = i->var_types[j];
    }
    tmp->num_vars = i->num_vars;

    tmp->conditions = dis_instantiate_wff( i->conditions );

    if ( tmp->conditions->connective == dis_FAL ) {
      dis_free_partial_dis_Effect( tmp );
      continue;
    }

    for ( l = i->effects; l; l = l->next ) {
      tt = dis_new_dis_Literal();
      tt->negated = l->negated;
      tt->fact.predicate = l->fact.predicate;
      for ( j = 0; j < dis_garity[tt->fact.predicate]; j++ ) {
	tt->fact.args[j] = l->fact.args[j];
	if ( tt->fact.args[j] < 0 &&
	     dis_linst_table[dis_DECODE_VAR( tt->fact.args[j] )] != -1 ) {
	  tt->fact.args[j] = dis_linst_table[dis_DECODE_VAR( tt->fact.args[j] )];
	}
      }
      tt->next = tmp->effects;
      if ( tmp->effects ) {
	tmp->effects->prev = tt;
      }
      tmp->effects = tt;
    }

    for ( ne = i->numeric_effects; ne; ne = ne->next ) {
      ttt = dis_new_dis_Numericdis_Effect();
      ttt->neft = ne->neft;
      ttt->fluent.function = ne->fluent.function;
      for ( j = 0; j < dis_gf_arity[ttt->fluent.function]; j++ ) {
	ttt->fluent.args[j] = ne->fluent.args[j];
	if ( ttt->fluent.args[j] < 0 &&
	     dis_linst_table[dis_DECODE_VAR( ttt->fluent.args[j] )] != -1 ) {
	  ttt->fluent.args[j] = dis_linst_table[dis_DECODE_VAR( ttt->fluent.args[j] )];
	}
      }
      ttt->rh = dis_copy_Exp( ne->rh );
      dis_instantiate_exp( &(ttt->rh) );
      ttt->next = tmp->numeric_effects;
      if ( tmp->numeric_effects ) {
	tmp->numeric_effects->prev = ttt;
      }
      tmp->numeric_effects = ttt;
    }

    tmp->next = res;
    if ( res ) {
      res->prev = tmp;
    }
    res = tmp;
  }

  return res;

}



dis_WffNode *dis_instantiate_wff( dis_WffNode *w )

{

  dis_WffNode *res = NULL, *tmp, *i;
  int j, m, h;
  dis_Bool ok, ct;

  switch ( w->connective ) {
  case dis_AND:
    m = 0;
    i = w->sons;
    while ( i ) {
      tmp = dis_instantiate_wff( i );
      if ( tmp->connective == dis_FAL ) {
	dis_free_dis_WffNode( res );
	return tmp;
      }
      if ( tmp->connective == dis_TRU ) {
	xfree( tmp );
	i = i->next;
	continue;
      }
      tmp->next = res;
      if ( res ) {
	res->prev = tmp;
      }
      res = tmp;
      i = i->next;
      m++;
    }
    if ( m == 0 ) {
      res = dis_new_dis_WffNode( dis_TRU );
      break;
    }
    if ( m == 1 ) {
      break;
    }
    tmp = dis_new_dis_WffNode( dis_AND );
    tmp->sons = res;
    res = tmp;
    break;
  case dis_OR:
    m = 0;
    i = w->sons;
    while ( i ) {
      tmp = dis_instantiate_wff( i );
      if ( tmp->connective == dis_TRU ) {
	dis_free_dis_WffNode( res );
	return tmp;
      }
      if ( tmp->connective == dis_FAL ) {
	xfree( tmp );
	i = i->next;
	continue;
      }
      tmp->next = res;
      if ( res ) {
	res->prev = tmp;
      }
      res = tmp;
      i = i->next;
      m++;
    }
    if ( m == 0 ) {
      res = dis_new_dis_WffNode( dis_FAL );
      break;
    }
    if ( m == 1 ) {
      break;
    }
    tmp = dis_new_dis_WffNode( dis_OR );
    tmp->sons = res;
    res = tmp;
    break;
  case dis_ATOM:
    res = dis_new_dis_WffNode( dis_ATOM );
    res->fact = dis_new_dis_Fact();
    res->fact->predicate = w->fact->predicate;
    ok = dis_TRUE;
    for ( j = 0; j < dis_garity[res->fact->predicate]; j++ ) {
      h = ( w->fact->args[j] < 0 ) ?
	dis_linst_table[dis_DECODE_VAR( w->fact->args[j] )] : w->fact->args[j];
      if ( h < 0 ) {
	ok = dis_FALSE;
	res->fact->args[j] = w->fact->args[j];
      } else {
	res->fact->args[j] = h;
      }
    }
    if ( !ok ) {/* contains ef params */
      break;
    }
    if ( !dis_full_dis_possibly_negative( res->fact ) ) {
      xfree( res->fact );
      res->fact = NULL;
      res->connective = dis_TRU;
      break;
    }
    if ( !dis_full_dis_possibly_positive( res->fact ) ) {
      xfree( res->fact );
      res->fact = NULL;
      res->connective = dis_FAL;
      break;
    }
    break;
  case dis_COMP:
    res = dis_new_dis_WffNode( dis_COMP );
    res->comp = w->comp;
    res->lh = dis_copy_Exp( w->lh );
    res->rh = dis_copy_Exp( w->rh );
    dis_instantiate_exp( &(res->lh) );
    dis_instantiate_exp( &(res->rh) );
    if ( res->lh->connective != NUMBER ||
	 res->rh->connective != NUMBER ) {
      /* logical simplification only possible if both parts are numbers
       */
      break;
    }
    ct = dis_number_comparison_holds( res->comp, res->lh->value, res->rh->value );
    if ( ct ) {
      res->connective = dis_TRU;
      dis_free_dis_ExpNode( res->lh );
      res->lh = NULL;
      dis_free_dis_ExpNode( res->rh );
      res->rh = NULL;
      res->comp = -1;
    } else {
      res->connective = dis_FAL;
      dis_free_dis_ExpNode( res->lh );
      res->lh = NULL;
      dis_free_dis_ExpNode( res->rh );
      res->rh = NULL;
      res->comp = -1;
    }
    break;
  case dis_TRU:
  case dis_FAL:
    res = dis_new_dis_WffNode( w->connective );
    break;
  default:
    printf("\n\nillegal connective %d in instantiate formula\n\n",
	   w->connective);
    exit( 1 );
  }

  return res;

}



void dis_instantiate_exp( dis_ExpNode **n )

{

  int j, f, k, h;
  dis_Bool ok;

  switch ( (*n)->connective ) {
  case AD:
    dis_instantiate_exp( &((*n)->leftson) );
    dis_instantiate_exp( &((*n)->rightson) );
    if ( (*n)->leftson->connective != NUMBER ||
	 (*n)->rightson->connective != NUMBER ) {
      break;
    }
    (*n)->connective = NUMBER;
    (*n)->value = (*n)->leftson->value + (*n)->rightson->value;
    dis_free_dis_ExpNode( (*n)->leftson );
    (*n)->leftson = NULL;
    dis_free_dis_ExpNode( (*n)->rightson );
    (*n)->rightson = NULL;
    break;
  case SU:
    dis_instantiate_exp( &((*n)->leftson) );
    dis_instantiate_exp( &((*n)->rightson) );
    if ( (*n)->leftson->connective != NUMBER ||
	 (*n)->rightson->connective != NUMBER ) {
      break;
    }
    (*n)->connective = NUMBER;
    (*n)->value = (*n)->leftson->value - (*n)->rightson->value;
    dis_free_dis_ExpNode( (*n)->leftson );
    (*n)->leftson = NULL;
    dis_free_dis_ExpNode( (*n)->rightson );
    (*n)->rightson = NULL;
    break;
  case MU:
    dis_instantiate_exp( &((*n)->leftson) );
    dis_instantiate_exp( &((*n)->rightson) );
    if ( (*n)->leftson->connective != NUMBER ||
	 (*n)->rightson->connective != NUMBER ) {
      break;
    }
    (*n)->connective = NUMBER;
    (*n)->value = (*n)->leftson->value * (*n)->rightson->value;
    dis_free_dis_ExpNode( (*n)->leftson );
    (*n)->leftson = NULL;
    dis_free_dis_ExpNode( (*n)->rightson );
    (*n)->rightson = NULL;
    break;
  case DI:
    dis_instantiate_exp( &((*n)->leftson) );
    dis_instantiate_exp( &((*n)->rightson) );
    if ( (*n)->leftson->connective != NUMBER ||
	 (*n)->rightson->connective != NUMBER ) {
      break;
    }
    if ( (*n)->rightson->value == 0 ) {
      /* kind of unclean: simply leave that in here;
       * we will later determine the right thing 
       * to do with it.
       */
      break;
    }
    (*n)->connective = NUMBER;
    (*n)->value = (*n)->leftson->value / (*n)->rightson->value;
    dis_free_dis_ExpNode( (*n)->leftson );
    (*n)->leftson = NULL;
    dis_free_dis_ExpNode( (*n)->rightson );
    (*n)->rightson = NULL;
    break;
  case MINUS:
    dis_instantiate_exp( &((*n)->son) );
    if ( (*n)->son->connective != NUMBER ) break;
    (*n)->connective = NUMBER;
    (*n)->value = ((float) (-1)) * (*n)->son->value;
    dis_free_dis_ExpNode( (*n)->son );
    (*n)->son = NULL;
    break;    
  case NUMBER:
    break;
  case FHEAD:
    f = (*n)->fluent->function;
    ok = dis_TRUE;
    for ( j = 0; j < dis_gf_arity[f]; j++ ) {
      h = ( (*n)->fluent->args[j] < 0 ) ?
	dis_linst_table[dis_DECODE_VAR( (*n)->fluent->args[j] )] : (*n)->fluent->args[j];
      if ( h < 0 ) {
	ok = dis_FALSE;
      } else {
	(*n)->fluent->args[j] = h;
      }
    }
    if ( !ok ) {
      break;
    }
    /* we handle only the case where the fluent is fully instantiated,
     * static, and in the initial state.
     */
    if ( dis_gis_changed[f] ) break;
    for ( j = 0; j < dis_gnum_initial_function[f]; j++ ) {
      for ( k = 0; k < dis_gf_arity[f]; k++ ) {
	if ( dis_ginitial_function[f][j].fluent.args[k] !=
	     (*n)->fluent->args[k] ) break;
      }
      if ( k < dis_gf_arity[f] ) continue;
      (*n)->connective = NUMBER;
      (*n)->value = dis_ginitial_function[f][j].value;
      break;
    }
    break;
  default:
    printf("\n\ninst exp: wrong specifier %d",
	   (*n)->connective);
    exit( 1 );
  }

}



dis_Bool dis_full_dis_possibly_positive( dis_Fact *f )

{

  int adr;

  if ( dis_gis_added[f->predicate] ) {
    return dis_TRUE;
  }

  adr = instantiated_dis_fact_adress( f );

  if ( dis_lini[f->predicate][adr] > 0 ) {
    return dis_TRUE;
  } else {
    return dis_FALSE;
  }

}



dis_Bool dis_full_dis_possibly_negative( dis_Fact *f )

{

  int adr;

  if ( dis_gis_deleted[f->predicate] ) {
    return dis_TRUE;
  }

  adr = instantiated_dis_fact_adress( f );

  if ( dis_lini[f->predicate][adr] > 0 ) {
    return dis_FALSE;
  } else {
    return dis_TRUE;
  }

}



int instantiated_dis_fact_adress( dis_Fact *f )

{

  int r = 0, b = 1, i;

    
  for ( i = 0; i < dis_garity[f->predicate]; i++ ) {
      //r += b * const_index[f->args[i]];
      r += b * const_index[f->args[i]][dis_gpredicates_args_type[f->predicate][i]];
      b *= dis_gtype_size[dis_gpredicates_args_type[f->predicate][i]];
  } 
    
 
 /* 
  for ( i = 0; i < dis_garity[f->predicate]; i++ ) {
    r += b * f->args[i];
    b *= dis_gnum_constants;
  }*/

  return r;

}














/*********************************************************
 * CODE THAT MULTIPLIES EFFECT PARAMS --> PSEUDO ACTIONS *
 *********************************************************/















void dis_multiply_hard_effect_parameters( void )

{

  Mixeddis_Operator *o;
  dis_Pseudodis_Action *tmp;
  int i;
  dis_Effect *e;

  dis_ghard_templates = ( dis_Pseudodis_Action_pointer * ) 
    calloc( dis_gnum_hard_mixed_operators, sizeof ( dis_Pseudodis_Action_pointer ) );
  dis_gnum_hard_templates = 0;

  for ( o = dis_ghard_mixed_operators; o; o = o->next ) {
    tmp = dis_new_dis_Pseudodis_Action( o );

    for ( i = 0; i < tmp->operator->num_vars; i++ ) {
      dis_linst_table[i] = tmp->inst_table[i];
    }

    for ( e = o->effects; e; e = e->next ) {
      dis_create_hard_pseudo_effects( tmp, e, 0 );
    }

    dis_ghard_templates[dis_gnum_hard_templates++] = tmp;
  }
}



void dis_create_hard_pseudo_effects( dis_Pseudodis_Action *a, dis_Effect *e, int curr_var )

{

  int par, t, i, m, mn;
  dis_WffNode *tmp1, *w, *ww;
  dis_Pseudodis_Actiondis_Effect *tmp2;

  if ( curr_var < e->num_vars ) {
    par = a->operator->num_vars + curr_var;

    t = e->var_types[curr_var];
    for ( i = 0; i < dis_gtype_size[t]; i++ ) {
      dis_linst_table[par] = dis_gtype_consts[t][i];

      dis_create_hard_pseudo_effects( a, e, curr_var + 1 );

      dis_linst_table[par] = -1;
    }
    return;
  }

  tmp1 = dis_instantiate_wff( e->conditions );

  if ( tmp1->connective == dis_FAL ) {
    dis_free_dis_WffNode( tmp1 );
    return;
  }

  dis_dnf( &tmp1 );
  dis_cleanup_wff( &tmp1 );

  /* only debugging, REMOVE LATER
   */
  if ( dis_is_dis_dnf( tmp1 ) == -1 ) {
    printf("\n\nILLEGAL DNF %s AFTER INSTANTIATION\n\n", a->operator->name);
    dis_print_Wff( tmp1, 0 );
    exit( 1 );
  }

  switch ( tmp1->connective ) {
  case dis_OR:
    for ( w = tmp1->sons; w; w = w->next ) {
      tmp2 = dis_new_dis_Pseudodis_Actiondis_Effect();
      if ( w->connective == dis_AND ) {
	m = 0;
	mn = 0;
	for ( ww = w->sons; ww; ww = ww->next ) {
	  if ( ww->connective == dis_ATOM ) m++;
	  if ( ww->connective == dis_COMP ) mn++;
	}
	tmp2->conditions = ( dis_Fact * ) calloc( m, sizeof( dis_Fact ) );
	tmp2->numeric_conditions_comp = ( dis_Comparator * ) calloc( mn, sizeof( dis_Comparator ) );
	tmp2->numeric_conditions_lh = ( dis_ExpNode_pointer * ) calloc( mn, sizeof( dis_ExpNode_pointer ) );
	tmp2->numeric_conditions_rh = ( dis_ExpNode_pointer * ) calloc( mn, sizeof( dis_ExpNode_pointer ) );
	tmp2->num_conditions = m;
	tmp2->num_numeric_conditions = mn;
	m = 0; mn = 0;
	for ( ww = w->sons; ww; ww = ww->next ) {
	  if ( ww->connective == dis_ATOM ) {
	    tmp2->conditions[m].predicate = ww->fact->predicate;
	    for ( i = 0; i < dis_garity[ww->fact->predicate]; i++ ) {
	      tmp2->conditions[m].args[i] = ww->fact->args[i];
	    }
	    m++;
	  }
	  if ( ww->connective == dis_COMP ) {
	    tmp2->numeric_conditions_comp[mn] = ww->comp;
	    tmp2->numeric_conditions_lh[mn] = dis_copy_Exp( ww->lh );
	    tmp2->numeric_conditions_rh[mn] = dis_copy_Exp( ww->rh );
	    mn++;
	  }
	}
      } else {
	if ( w->connective == dis_ATOM ) {
	  tmp2->conditions = ( dis_Fact * ) calloc( 1, sizeof( dis_Fact ) );
	  tmp2->num_conditions = 1;
	  tmp2->conditions[0].predicate = w->fact->predicate;
	  for ( i = 0; i < dis_garity[w->fact->predicate]; i++ ) {
	    tmp2->conditions[0].args[i] = w->fact->args[i];
	  }
	}
 	if ( w->connective == dis_COMP ) {
	  tmp2->numeric_conditions_comp = ( dis_Comparator * ) calloc( 1, sizeof( dis_Comparator ) );
	  tmp2->numeric_conditions_lh = ( dis_ExpNode_pointer * ) calloc( 1, sizeof( dis_ExpNode_pointer ) );
	  tmp2->numeric_conditions_rh = ( dis_ExpNode_pointer * ) calloc( 1, sizeof( dis_ExpNode_pointer ) );
	  tmp2->numeric_conditions_comp[0] = w->comp;
	  tmp2->numeric_conditions_lh[0] = dis_copy_Exp( w->lh );
	  tmp2->numeric_conditions_rh[0] = dis_copy_Exp( w->rh );
	  tmp2->num_numeric_conditions = 1;
	}
      }
      dis_make_instantiate_literals( tmp2, e->effects );
      dis_make_instantiate_numeric_effects( tmp2, e->numeric_effects );
      tmp2->next = a->effects;
      a->effects = tmp2;
      a->num_effects++;
    }
    break;
  case dis_AND:
    tmp2 = dis_new_dis_Pseudodis_Actiondis_Effect();
    m = 0;
    mn = 0;
    for ( w = tmp1->sons; w; w = w->next ) {
      if ( w->connective == dis_ATOM ) m++;
      if ( w->connective == dis_COMP ) mn++;
    }
    tmp2->conditions = ( dis_Fact * ) calloc( m, sizeof( dis_Fact ) );
    tmp2->numeric_conditions_comp = ( dis_Comparator * ) calloc( mn, sizeof( dis_Comparator ) );
    tmp2->numeric_conditions_lh = ( dis_ExpNode_pointer * ) calloc( mn, sizeof( dis_ExpNode_pointer ) );
    tmp2->numeric_conditions_rh = ( dis_ExpNode_pointer * ) calloc( mn, sizeof( dis_ExpNode_pointer ) );
    tmp2->num_conditions = m;
    tmp2->num_numeric_conditions = mn;
    m = 0; mn = 0;
    for ( w = tmp1->sons; w; w = w->next ) {
      if ( w->connective == dis_ATOM ) {
	tmp2->conditions[m].predicate = w->fact->predicate;
	for ( i = 0; i < dis_garity[w->fact->predicate]; i++ ) {
	  tmp2->conditions[m].args[i] = w->fact->args[i];
	}
	m++;
      }
      if ( w->connective == dis_COMP ) {
	tmp2->numeric_conditions_comp[mn] = w->comp;
	tmp2->numeric_conditions_lh[mn] = dis_copy_Exp( w->lh );
	tmp2->numeric_conditions_rh[mn] = dis_copy_Exp( w->rh );
	mn++;
      }
    }
    dis_make_instantiate_literals( tmp2, e->effects );
    dis_make_instantiate_numeric_effects( tmp2, e->numeric_effects );
    tmp2->next = a->effects;
    a->effects = tmp2;
    a->num_effects++;
    break;
  case dis_ATOM:
    tmp2 = dis_new_dis_Pseudodis_Actiondis_Effect();
    tmp2->conditions = ( dis_Fact * ) calloc( 1, sizeof( dis_Fact ) );
    tmp2->num_conditions = 1;
    tmp2->conditions[0].predicate = tmp1->fact->predicate;
    for ( i = 0; i < dis_garity[tmp1->fact->predicate]; i++ ) {
      tmp2->conditions[0].args[i] = tmp1->fact->args[i];
    }
    dis_make_instantiate_literals( tmp2, e->effects );
    dis_make_instantiate_numeric_effects( tmp2, e->numeric_effects );
    tmp2->next = a->effects;
    a->effects = tmp2;
    a->num_effects++;
    break;
  case dis_COMP:
    tmp2 = dis_new_dis_Pseudodis_Actiondis_Effect();
    tmp2->numeric_conditions_comp = ( dis_Comparator * ) calloc( 1, sizeof( dis_Comparator ) );
    tmp2->numeric_conditions_lh = ( dis_ExpNode_pointer * ) calloc( 1, sizeof( dis_ExpNode_pointer ) );
    tmp2->numeric_conditions_rh = ( dis_ExpNode_pointer * ) calloc( 1, sizeof( dis_ExpNode_pointer ) );
    tmp2->numeric_conditions_comp[0] = tmp1->comp;
    tmp2->numeric_conditions_lh[0] = dis_copy_Exp( tmp1->lh );
    tmp2->numeric_conditions_rh[0] = dis_copy_Exp( tmp1->rh );
    tmp2->num_numeric_conditions = 1;
    dis_make_instantiate_literals( tmp2, e->effects );
    dis_make_instantiate_numeric_effects( tmp2, e->numeric_effects );
    tmp2->next = a->effects;
    a->effects = tmp2;
    a->num_effects++;
    break;
  case dis_TRU:
    tmp2 = dis_new_dis_Pseudodis_Actiondis_Effect();
    dis_make_instantiate_literals( tmp2, e->effects );
    dis_make_instantiate_numeric_effects( tmp2, e->numeric_effects );
    tmp2->next = a->effects;
    a->effects = tmp2;
    a->num_effects++;
    break;
  default:
    printf("\n\nillegal connective %d in parsing DNF condition.\n\n",
	   tmp1->connective);
    exit( 1 );
  }

  dis_free_dis_WffNode( tmp1 );

}
 


void dis_make_instantiate_literals( dis_Pseudodis_Actiondis_Effect *e, dis_Literal *ll )

{

  int ma = 0, md = 0, i;
  dis_Literal *l;

  for ( l = ll; l; l = l->next ) {
    if ( l->negated ) {
      md++;
    } else {
      ma++;
    }
  }

  e->adds = ( dis_Fact * ) calloc( ma, sizeof( dis_Fact ) );
  e->dels = ( dis_Fact * ) calloc( md, sizeof( dis_Fact ) );

  for ( l = ll; l; l = l->next ) {
    if ( l->negated ) {
      e->dels[e->num_dels].predicate = l->fact.predicate;
      for ( i = 0; i < dis_garity[l->fact.predicate]; i++ ) {
	e->dels[e->num_dels].args[i] = ( l->fact.args[i] < 0 ) ?
	  dis_linst_table[dis_DECODE_VAR( l->fact.args[i] )] : l->fact.args[i];
      }
      e->num_dels++;
    } else {
      e->adds[e->num_adds].predicate = l->fact.predicate;
      for ( i = 0; i < dis_garity[l->fact.predicate]; i++ ) {
	e->adds[e->num_adds].args[i] = ( l->fact.args[i] < 0 ) ?
	  dis_linst_table[dis_DECODE_VAR( l->fact.args[i] )] : l->fact.args[i];
      }
      e->num_adds++;
    }
  }

}
 


void dis_make_instantiate_numeric_effects( dis_Pseudodis_Actiondis_Effect *e, dis_Numericdis_Effect *ne )

{

  int m = 0, i;
  dis_Numericdis_Effect *n;

  for ( n = ne; n; n = n->next ) m++;

  e->numeric_effects_neft = ( dis_Numericdis_EffectType * ) calloc( m, sizeof( dis_Numericdis_EffectType ) );
  e->numeric_effects_fluent = ( dis_Fluent * ) calloc( m, sizeof( dis_Fluent ) );
  e->numeric_effects_rh = ( dis_ExpNode_pointer * ) calloc( m, sizeof( dis_ExpNode_pointer ) );
  e->num_numeric_effects = m;

  m = 0;
  for ( n = ne; n; n = n->next ) {
    e->numeric_effects_neft[m] = n->neft;
    e->numeric_effects_fluent[m].function = n->fluent.function;
    for ( i = 0; i < dis_gf_arity[n->fluent.function]; i++ ) {
      e->numeric_effects_fluent[m].args[i] = ( n->fluent.args[i] < 0 ) ?
	dis_linst_table[dis_DECODE_VAR( n->fluent.args[i] )] : n->fluent.args[i];
    }
    e->numeric_effects_rh[m] = dis_copy_Exp( n->rh );
    dis_instantiate_exp( &(e->numeric_effects_rh[m]) );
    m++;
  }

}
