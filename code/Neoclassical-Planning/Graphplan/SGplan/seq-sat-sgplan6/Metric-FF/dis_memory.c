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

 * File: dis_memory.c 

 * Description: modified from memory.c in Metric-FF

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
 * File: memory.c
 * Description: Creation and Deletion functions for all data structures.
 *
 * Author: Joerg Hoffmann
 *
 *********************************************************************/ 









#include "dis_ff.h"
#include "dis_memory.h"
#include "dis_parse.h"

#include "dis_inst_pre.h"






/**********************
 * CREATION FUNCTIONS *
 **********************/











/* parsing
 */









char *dis_new_dis_Token( int len )

{

  char *tok = ( char * ) calloc( len, sizeof( char ) );
  dis_CHECK_PTR(tok);

  return tok;

}



dis_TokenList *dis_new_dis_TokenList( void )

{

  dis_TokenList *result = ( dis_TokenList * ) calloc( 1, sizeof( dis_TokenList ) );
  dis_CHECK_PTR(result);

  result->item = NULL; 
  result->next = NULL;

  return result;

}



dis_FactList *dis_new_dis_FactList( void )

{

  dis_FactList *result = ( dis_FactList * ) calloc( 1, sizeof( dis_FactList ) );
  dis_CHECK_PTR(result);

  result->item = NULL; 
  result->next = NULL;

  return result;

}



dis_TypedList *dis_new_dis_TypedList( void )

{

  dis_TypedList *result = ( dis_TypedList * ) calloc( 1, sizeof( dis_TypedList ) );
  dis_CHECK_PTR(result);

  result->name = NULL; 
  result->type = NULL;
  result->n = -1;

  return result;

}



dis_TypedListList *dis_new_dis_TypedListList( void )

{

  dis_TypedListList *result = ( dis_TypedListList * ) calloc( 1, sizeof( dis_TypedListList ) );
  dis_CHECK_PTR(result);

  result->predicate = NULL; 
  result->args = NULL;
  result->next = NULL;

  return result;

}



dis_Parsedis_ExpNode *dis_new_dis_Parsedis_ExpNode( dis_Expdis_Connective c )

{

  dis_Parsedis_ExpNode *result = ( dis_Parsedis_ExpNode * ) calloc( 1, sizeof( dis_Parsedis_ExpNode ) );
  dis_CHECK_PTR(result);

  result->connective = c;
  result->atom = NULL;
  result->leftson = NULL;
  result->rightson = NULL;

  return result;

}



dis_PlNode *dis_new_dis_PlNode( dis_Connective c )

{

  dis_PlNode *result = ( dis_PlNode * ) calloc( 1, sizeof( dis_PlNode ) );
  dis_CHECK_PTR(result);

  result->connective = c;
  result->atom = NULL;

  result->comp = -1;
  result->neft = -1;
  result->lh = NULL;
  result->rh = NULL;
  result->value = 0;

  result->sons = NULL;
  result->next = NULL;

  return result;

}

dis_Pldis_Operator *dis_new_dis_Pldis_Operator( char *name )

{

  dis_Pldis_Operator *result = ( dis_Pldis_Operator * ) calloc( 1, sizeof( dis_Pldis_Operator ) );
  dis_CHECK_PTR(result);

  if ( name ) {
    result->name = dis_new_dis_Token(strlen(name)+1);
    dis_CHECK_PTR(result->name);
    strcpy(result->name, name);
  } else {
    result->name = NULL;
  }

  result->params = NULL;
  result->preconds = NULL;
  result->effects = NULL;
  result->number_of_real_params = 0;
  result->next = NULL;
  
  /* durative actions */
  result->duration = NULL;

  return result;

}



dis_Pldis_Operator *dis_new_axiom_op_list( void )

{

  static int count;
  char *name;
  dis_Pldis_Operator *ret;

  /* WARNING: count should not exceed 999 
   */
  count++;
  if ( count == 10000 ) {
    printf("\ntoo many axioms! look into memory.c, line 157\n\n");
    exit( 1 );
  }
  name = dis_new_dis_Token(strlen(dis_HIDDEN_STR)+strlen(dis_AXIOM_STR)+4+1);
  sprintf(name, "%s%s%4d", dis_HIDDEN_STR, dis_AXIOM_STR, count);

  ret = dis_new_dis_Pldis_Operator(name);
  free(name);

  return ret;

}














/* instantiation
 */











dis_Fact *dis_new_dis_Fact( void )

{

  dis_Fact *result = ( dis_Fact * ) calloc( 1, sizeof( dis_Fact ) );
  dis_CHECK_PTR(result);

  return result;

}



dis_Fluent *dis_new_dis_Fluent( void )

{

  dis_Fluent *result = ( dis_Fluent * ) calloc( 1, sizeof( dis_Fluent ) );
  dis_CHECK_PTR(result);

  return result;

}



dis_FluentValue *dis_new_dis_FluentValue( void )

{

  dis_FluentValue *result = ( dis_FluentValue * ) calloc( 1, sizeof( dis_FluentValue ) );
  dis_CHECK_PTR(result);

  return result;

}



dis_Facts *dis_new_dis_Facts( void )

{

  dis_Facts *result = ( dis_Facts * ) calloc( 1, sizeof( dis_Facts ) );
  dis_CHECK_PTR(result);

  result->fact = dis_new_dis_Fact();

  result->next = NULL;

  return result;

}



dis_FluentValues *dis_dis_new_dis_FluentValues( void )

{

  dis_FluentValues *result = ( dis_FluentValues * ) calloc( 1, sizeof( dis_FluentValues ) );
  dis_CHECK_PTR(result);

  result->next = NULL;

  return result;

}



dis_ExpNode *dis_new_dis_ExpNode( dis_Expdis_Connective c )

{

  dis_ExpNode *result = ( dis_ExpNode * ) calloc( 1, sizeof( dis_ExpNode ) );
  dis_CHECK_PTR(result);

  result->connective = c;
  result->fluent = NULL;
  result->fl = -2;
  result->c = 1;
  result->son = NULL;
  result->leftson = NULL;
  result->rightson = NULL;

  return result;

}



dis_WffNode *dis_new_dis_WffNode( dis_Connective c )

{

  dis_WffNode *result = ( dis_WffNode * ) calloc( 1, sizeof( dis_WffNode ) );
  dis_CHECK_PTR(result);

  result->connective = c;

  result->var = -1;
  result->var_type = -1;
  result->var_name = NULL;

  result->sons = NULL;
  result->next = NULL;
  result->prev = NULL;

  result->fact = NULL;
  result->dis_NOT_p = -1;

  result->son = NULL;

  result->comp = -1;
  result->lh = NULL;
  result->rh = NULL;
  
  result->visited = dis_FALSE;

  return result;

}



dis_Literal *dis_new_dis_Literal( void ) 

{

  dis_Literal *result = ( dis_Literal * ) calloc( 1, sizeof( dis_Literal ) );
  dis_CHECK_PTR(result);

  result->next = NULL;
  result->prev = NULL;

  return result; 

}



dis_Numericdis_Effect *dis_new_dis_Numericdis_Effect( void ) 

{

  dis_Numericdis_Effect *result = ( dis_Numericdis_Effect * ) calloc( 1, sizeof( dis_Numericdis_Effect ) );
  dis_CHECK_PTR(result);

  result->rh = NULL;

  result->next = NULL;
  result->prev = NULL;

  return result; 

}



dis_Effect *dis_new_dis_Effect( void )

{

  dis_Effect *result = ( dis_Effect * ) calloc( 1, sizeof( dis_Effect ) );
  dis_CHECK_PTR(result);

  result->num_vars = 0;

  result->conditions = NULL;

  result->effects = NULL;
  result->numeric_effects = NULL;

  result->next = NULL;
  result->prev = NULL;

  return result;

}



dis_Operator *dis_new_dis_Operator( char *name, int norp )

{

  int i;

  dis_Operator *result = ( dis_Operator * ) calloc( 1, sizeof( dis_Operator ) );
  dis_CHECK_PTR(result);

  if ( name ) {
    result->name = dis_new_dis_Token( strlen( name ) + 1 );
    dis_CHECK_PTR( result->name );
    strcpy( result->name, name );
  } else {
    result->name = NULL;
  }

  result->num_vars = 0;
  result->number_of_real_params = norp;

  for ( i = 0; i < dis_MAX_VARS; i++ ) {
    result->removed[i] = dis_FALSE;
  }

  result->preconds = NULL;

  result->effects = NULL;

  result->hard = dis_TRUE;

  return result;

}



Normdis_Effect *dis_new_Normdis_Effect1( dis_Effect *e )

{

  int i;

  Normdis_Effect *result = ( Normdis_Effect * ) calloc( 1, sizeof( Normdis_Effect ) );
  dis_CHECK_PTR(result);

  result->num_vars = e->num_vars;
  for ( i = 0; i < e->num_vars; i++ ) {
    result->var_types[i] = e->var_types[i];
    result->inst_table[i] = -1;
  }

  result->conditions = NULL;
  result->num_conditions = 0;

  result->adds = NULL;
  result->num_adds = 0;
  result->dels = NULL;
  result->num_dels = 0;

  result->numeric_conditions_comp = NULL;
  result->numeric_conditions_lh = NULL;
  result->numeric_conditions_rh = NULL;
  result->num_numeric_conditions = 0;

  result->numeric_effects_neft = NULL;
  result->numeric_effects_fluent = NULL;
  result->numeric_effects_rh = NULL;
  result->num_numeric_effects = 0;

  result->next = NULL;
  result->prev = NULL;

  return result;

}



Normdis_Effect *dis_new_Normdis_Effect2( Normdis_Effect *e )

{

  int i, j;

  Normdis_Effect *result = ( Normdis_Effect * ) calloc( 1, sizeof( Normdis_Effect ) );
  dis_CHECK_PTR(result);

  result->num_vars = 0;

  result->conditions = ( dis_Fact * ) calloc( e->num_conditions, sizeof( dis_Fact ) );
  result->num_conditions = e->num_conditions;
  for ( i = 0; i < e->num_conditions; i++ ) {
    result->conditions[i].predicate = e->conditions[i].predicate;
    for ( j = 0; j < dis_garity[e->conditions[i].predicate]; j++ ) {
      result->conditions[i].args[j] = e->conditions[i].args[j];
    }
  }
  result->adds = ( dis_Fact * ) calloc( e->num_adds, sizeof( dis_Fact ) );
  result->num_adds = e->num_adds;
  for ( i = 0; i < e->num_adds; i++ ) {
    result->adds[i].predicate = e->adds[i].predicate;
    for ( j = 0; j < dis_garity[e->adds[i].predicate]; j++ ) {
      result->adds[i].args[j] = e->adds[i].args[j];
    }
  }
  result->dels = ( dis_Fact * ) calloc( e->num_dels, sizeof( dis_Fact ) );
  result->num_dels = e->num_dels;
  for ( i = 0; i < e->num_dels; i++ ) {
    result->dels[i].predicate = e->dels[i].predicate;
    for ( j = 0; j < dis_garity[e->dels[i].predicate]; j++ ) {
      result->dels[i].args[j] = e->dels[i].args[j];
    }
  }

  result->numeric_conditions_comp = ( dis_Comparator * ) 
    calloc( e->num_numeric_conditions, sizeof( dis_Comparator ) );
  result->numeric_conditions_lh = ( dis_ExpNode_pointer * ) 
    calloc( e->num_numeric_conditions, sizeof( dis_ExpNode_pointer ) );
  result->numeric_conditions_rh = ( dis_ExpNode_pointer * ) 
    calloc( e->num_numeric_conditions, sizeof( dis_ExpNode_pointer ) );
  for ( i = 0; i < e->num_numeric_conditions; i++ ) {
    result->numeric_conditions_comp[i] = e->numeric_conditions_comp[i];
    result->numeric_conditions_lh[i] = dis_copy_Exp( e->numeric_conditions_lh[i] );
    result->numeric_conditions_rh[i] = dis_copy_Exp( e->numeric_conditions_rh[i] );
  }
  result->num_numeric_conditions = e->num_numeric_conditions;
  result->numeric_effects_neft = ( dis_Numericdis_EffectType * ) 
    calloc( e->num_numeric_effects, sizeof( dis_Numericdis_EffectType ) );
  result->numeric_effects_fluent = ( dis_Fluent * ) 
    calloc( e->num_numeric_effects, sizeof( dis_Fluent ) );
  result->numeric_effects_rh = ( dis_ExpNode_pointer * ) 
    calloc( e->num_numeric_effects, sizeof( dis_ExpNode_pointer ) );
  for ( i = 0; i < e->num_numeric_effects; i++ ) {
    result->numeric_effects_neft[i] = e->numeric_effects_neft[i];
    result->numeric_effects_fluent[i].function = e->numeric_effects_fluent[i].function;
    for ( j = 0; j < dis_gf_arity[e->numeric_effects_fluent[i].function]; j++ ) {
      result->numeric_effects_fluent[i].args[j] = e->numeric_effects_fluent[i].args[j];
    }
    result->numeric_effects_rh[i] = dis_copy_Exp( e->numeric_effects_rh[i] );
  }
  result->num_numeric_effects = e->num_numeric_effects;

  result->next = NULL;
  result->prev = NULL;

  return result;

}



Normdis_Operator *dis_new_Normdis_Operator( dis_Operator *op )

{

  int i;

  Normdis_Operator *result = ( Normdis_Operator * ) calloc( 1, sizeof( Normdis_Operator ) );
  dis_CHECK_PTR(result);

  result->operator = op;

  result->num_vars = op->num_vars;
  for ( i = 0; i < op->num_vars; i++ ) {
    result->var_types[i] = op->var_types[i];
    result->inst_table[i] = -1;
  }
  result->num_removed_vars = 0;

  result->preconds = NULL;
  result->num_preconds = 0;

  result->numeric_preconds_comp = NULL;
  result->numeric_preconds_lh = NULL;
  result->numeric_preconds_rh = NULL;
  result->num_numeric_preconds = 0;

  result->effects = NULL;

  return result;

}




dis_EasyTemplate *dis_new_dis_EasyTemplate( Normdis_Operator *op )

{

  dis_EasyTemplate *result = ( dis_EasyTemplate * ) calloc( 1, sizeof( dis_EasyTemplate ) );
  dis_CHECK_PTR(result);

  result->op = op;

  result->prev = NULL;
  result->next = NULL;

  return result;

}



Mixeddis_Operator *dis_new_Mixeddis_Operator( dis_Operator *op )

{

  Mixeddis_Operator *result = ( Mixeddis_Operator * ) calloc( 1, sizeof( Mixeddis_Operator ) );
  dis_CHECK_PTR(result);

  result->operator = op;

  result->preconds = NULL;
  result->num_preconds = 0;

  result->effects = NULL;

  return result;

}



dis_Pseudodis_Actiondis_Effect *dis_new_dis_Pseudodis_Actiondis_Effect( void )

{

  dis_Pseudodis_Actiondis_Effect *result = 
    ( dis_Pseudodis_Actiondis_Effect * ) calloc( 1, sizeof( dis_Pseudodis_Actiondis_Effect ) );
  dis_CHECK_PTR(result);

  result->conditions = NULL;
  result->num_conditions = 0;

  result->adds = NULL;
  result->num_adds = 0;
  result->dels = NULL;
  result->num_dels = 0;

  result->numeric_conditions_comp = NULL;
  result->numeric_conditions_lh = NULL;
  result->numeric_conditions_rh = NULL;
  result->num_numeric_conditions = 0;

  result->numeric_effects_neft = NULL;
  result->numeric_effects_fluent = NULL;
  result->numeric_effects_rh = NULL;
  result->num_numeric_effects = 0;

  result->next = NULL;

  return result;

}



dis_Pseudodis_Action *dis_new_dis_Pseudodis_Action( Mixeddis_Operator *op )

{

  int i;

  dis_Pseudodis_Action *result = ( dis_Pseudodis_Action * ) calloc( 1, sizeof( dis_Pseudodis_Action ) );
  dis_CHECK_PTR(result);

  result->operator = op->operator;
  for ( i = 0; i < op->operator->num_vars; i++ ) {
    result->inst_table[i] = op->inst_table[i];
  }

  result->preconds = op->preconds;
  result->num_preconds = op->num_preconds;

  result->numeric_preconds_comp = op->numeric_preconds_comp;
  result->numeric_preconds_lh = op->numeric_preconds_lh;
  result->numeric_preconds_rh = op->numeric_preconds_rh;
  result->num_numeric_preconds = op->num_numeric_preconds;

  result->effects = NULL;
  result->num_effects = 0;

  return result;

}


// PDDL3
Lnfdis_ExpNode *dis_new_Lnfdis_ExpNode( void )

{
  Lnfdis_ExpNode *result = ( Lnfdis_ExpNode * ) calloc( 1, sizeof( Lnfdis_ExpNode ) );
  dis_CHECK_PTR(result);

  result->pF = (int *) malloc(dis_MAX_LNF_F*sizeof(int));
  result->nF = (int *) malloc(dis_MAX_LNF_F*sizeof(int));
  result->pC = (float *) malloc(dis_MAX_LNF_F*sizeof(float));
  result->nC = (float *) malloc(dis_MAX_LNF_F*sizeof(float));
  result->num_pF = 0;
  result->num_nF = 0;

  result->c = 0;

  return result;

}



dis_Action *dis_new_dis_Action( void )

{

  dis_Action *result = ( dis_Action * ) calloc( 1, sizeof( dis_Action ) );
  dis_CHECK_PTR(result);

  result->norm_operator = NULL;
  result->pseudo_action = NULL;

  result->next = NULL;

  return result;

}



void dis_make_state( dis_State *pointer, int ft, int fl ) 

{

  int i;

  pointer->F = ( int * ) calloc( ft, sizeof( int ) ); 
  pointer->f_D = ( dis_Bool * ) calloc( fl, sizeof( dis_Bool ) ); 
  pointer->f_V = ( float * ) calloc( fl, sizeof( float ) );

  for ( i = 0; i < fl; i++ ) {
    pointer->f_D[i] = dis_FALSE;
  }

}



dis_EhcNode *dis_new_dis_EhcNode( void )

{

  dis_EhcNode *result = ( dis_EhcNode * ) calloc( 1, sizeof( dis_EhcNode ) );
  dis_CHECK_PTR(result);

  dis_make_state( &(result->S), dis_gnum_ft_conn, dis_gnum_fl_conn );

  result->father = NULL;
  result->next = NULL;

  return result;

}



dis_EhcHashEntry *dis_new_dis_EhcHashEntry( void )

{

  dis_EhcHashEntry *result = ( dis_EhcHashEntry * ) calloc( 1, sizeof( dis_EhcHashEntry ) );
  dis_CHECK_PTR(result);

  result->ehc_node = NULL;

  result->next = NULL;

  return result;

}



dis_PlanHashEntry *dis_new_dis_PlanHashEntry( void )

{

  dis_PlanHashEntry *result = ( dis_PlanHashEntry * ) calloc( 1, sizeof( dis_PlanHashEntry ) );
  dis_CHECK_PTR(result);

  result->next_step = NULL;

  result->next = NULL;

  return result;

}



dis_BfsNode *dis_new_dis_BfsNode( void )

{

  dis_BfsNode *result = ( dis_BfsNode * ) calloc( 1, sizeof( dis_BfsNode ) );
  dis_CHECK_PTR(result);

  result->father = NULL;

  result->next = NULL;
  result->prev = NULL;

  return result;

}



dis_BfsHashEntry *dis_new_dis_BfsHashEntry( void )

{

  dis_BfsHashEntry *result = ( dis_BfsHashEntry * ) calloc( 1, sizeof( dis_BfsHashEntry ) );
  dis_CHECK_PTR(result);

  result->bfs_node = NULL;

  result->next = NULL;

  return result;

}











/**********************
 * DELETION FUNCTIONS *
 **********************/












void dis_free_dis_TokenList( dis_TokenList *source )

{

  if ( source ) {
    dis_free_dis_TokenList( source->next );
    if ( source->item ) {
      free( source->item );
    }
    free( source );
  }

}



void dis_free_dis_FactList( dis_FactList *source )

{

  if ( source ) {
    dis_free_dis_FactList( source->next );
    dis_free_dis_TokenList( source->item );
    free( source );
  }

}



void dis_free_dis_Parsedis_ExpNode( dis_Parsedis_ExpNode *n )

{

  if ( n ) {
    dis_free_dis_TokenList( n->atom );
    dis_free_dis_Parsedis_ExpNode( n->leftson );
    dis_free_dis_Parsedis_ExpNode( n->rightson );
    free( n );
  }

}



void dis_free_dis_PlNode( dis_PlNode *node )

{
  
  if ( node ) {
    dis_free_dis_Parsedis_ExpNode( node->lh );
    dis_free_dis_Parsedis_ExpNode( node->rh );
    dis_free_dis_PlNode( node->sons );
    dis_free_dis_PlNode( node->next );
    dis_free_dis_TokenList( node->atom );
    free( node );
  }

}



void dis_free_dis_Pldis_Operator( dis_Pldis_Operator *o )

{

  if ( o ) {
    dis_free_dis_Pldis_Operator( o->next );

    if ( o->name ) {
      free( o->name );
    }
    
    dis_free_dis_FactList( o->params );
    dis_free_dis_PlNode( o->preconds );
    dis_free_dis_PlNode( o->effects );

    free( o );
  }

}



void dis_free_dis_Operator( dis_Operator *o )

{

  if ( o ) {
    /* need not free more: the only point where that happens
     * is only directly after first allocation
     */

    if ( o->name ) {
      free( o->name );
    }

    free( o );
  } 

}



void dis_free_dis_ExpNode( dis_ExpNode *n )

{

  if ( n ) {
    if ( n->fluent ) free( n->fluent );
    dis_free_dis_ExpNode( n->son );
    dis_free_dis_ExpNode( n->leftson );
    dis_free_dis_ExpNode( n->rightson );
    free( n );
  }

}



void dis_free_dis_WffNode( dis_WffNode *w )

{

  if ( w ) {
    dis_free_dis_WffNode( w->son );
    dis_free_dis_WffNode( w->sons );
    dis_free_dis_WffNode( w->next );
    if ( w->var_name ) {
      free( w->var_name );
    }
    if ( w->fact ) free( w->fact );
    dis_free_dis_ExpNode( w->lh );
    dis_free_dis_ExpNode( w->rh );
    free( w );
  }

}



void dis_free_Normdis_Effect( Normdis_Effect *e )

{

  int i;

  if ( e ) {
    dis_free_Normdis_Effect( e->next );

    if ( e->conditions ) {
      free( e->conditions );
    }
    if ( e->adds ) {
      free( e->adds );
    }
    if ( e->dels ) {
      free( e->dels );
    }

    if ( e->numeric_conditions_comp ) {
      free( e->numeric_conditions_comp );
    }
    for ( i = 0; i < e->num_numeric_conditions; i++ ) {
      dis_free_dis_ExpNode( e->numeric_conditions_lh[i] );
      dis_free_dis_ExpNode( e->numeric_conditions_rh[i] );
    }
    if ( e->numeric_conditions_lh ) {
      free( e->numeric_conditions_lh );
    }
    if ( e->numeric_conditions_rh ) {
      free( e->numeric_conditions_rh );
    }

    if ( e->numeric_effects_neft ) {
      free( e->numeric_effects_neft );
    }
    if ( e->numeric_effects_fluent ) {
      free( e->numeric_effects_fluent );
    }
    for ( i = 0; i < e->num_numeric_effects; i++ ) {
      dis_free_dis_ExpNode( e->numeric_effects_rh[i] );
    }
    if ( e->numeric_effects_rh ) {
      free( e->numeric_effects_rh );
    }

    free( e );
  }

}



void dis_free_partial_dis_Effect( dis_Effect *e )

{

  if ( e ) {
    dis_free_partial_dis_Effect( e->next );

    dis_free_dis_WffNode( e->conditions );

    free( e );
  }

}



void dis_free_Normdis_Operator( Normdis_Operator *o )

{

  int i;

  if ( o ) {

    if ( o->preconds ) {
      free( o->preconds );
    }
    if ( o->numeric_preconds_comp ) {
      free( o->numeric_preconds_comp );
    }
    for ( i = 0; i < o->num_numeric_preconds; i++ ) {
      dis_free_dis_ExpNode( o->numeric_preconds_lh[i] );
      dis_free_dis_ExpNode( o->numeric_preconds_rh[i] );
    }
    if ( o->numeric_preconds_lh ) {
      free( o->numeric_preconds_lh );
    }
    if ( o->numeric_preconds_rh ) {
      free( o->numeric_preconds_rh );
    }
    dis_free_Normdis_Effect( o->effects );

    free( o );
  }

}



void dis_free_single_Normdis_Effect( Normdis_Effect *e )

{

  int i;

  if ( e ) {
    if ( e->conditions ) {
      free( e->conditions );
    }
    if ( e->adds ) {
      free( e->adds );
    }
    if ( e->dels ) {
      free( e->dels );
    }

    if ( e->numeric_conditions_comp ) {
      free( e->numeric_conditions_comp );
    }
    for ( i = 0; i < e->num_numeric_conditions; i++ ) {
      dis_free_dis_ExpNode( e->numeric_conditions_lh[i] );
      dis_free_dis_ExpNode( e->numeric_conditions_rh[i] );
    }
    if ( e->numeric_conditions_lh ) {
      free( e->numeric_conditions_lh );
    }
    if ( e->numeric_conditions_rh ) {
      free( e->numeric_conditions_rh );
    }

    if ( e->numeric_effects_neft ) {
      free( e->numeric_effects_neft );
    }
    if ( e->numeric_effects_fluent ) {
      free( e->numeric_effects_fluent );
    }
    for ( i = 0; i < e->num_numeric_effects; i++ ) {
      dis_free_dis_ExpNode( e->numeric_effects_rh[i] );
    }
    if ( e->numeric_effects_rh ) {
      free( e->numeric_effects_rh );
    }

    free( e );
  }

}



void dis_free_single_dis_EasyTemplate( dis_EasyTemplate *t )

{

  if ( t ) {
    free( t );
  }

}



void dis_free_dis_TypedList( dis_TypedList *t )

{

  if ( t ) {
    if ( t->name ) {
      free( t->name );
      t->name = NULL;
    }
    if ( t->type ) {
      dis_free_dis_TokenList( t->type );
      t->type = NULL;
    }
    dis_free_dis_TypedList( t->next );

    free( t );
  }

}



void dis_free_dis_TypedListList( dis_TypedListList *t )

{

  if ( t ) {
    if ( t->predicate ) {
      free( t->predicate );
      t->predicate = NULL;
    }
    if ( t->args ) {
      dis_free_dis_TypedList( t->args );
      t->args = NULL;
    }
    dis_free_dis_TypedListList( t->next );

    free( t );
  }

}

dis_TypedList *copy_dis_TypedList(dis_TypedList *p)
{
  dis_TypedList *t = NULL;
  if (p)
  {
    t = dis_new_dis_TypedList();
    if (p->name)
    {
      t->name = (char *) malloc(strlen(p->name)+1);
      strcpy(t->name, p->name);
    }
    else
      t->name = NULL;
    t->type = dis_copy_dis_TokenList(p->type);
    t->n = p->n;
    t->next = copy_dis_TypedList(p->next);
  }
  return t;
}

dis_Parsedis_ExpNode *copy_dis_Parsedis_ExpNode(dis_Parsedis_ExpNode *p)
{
  dis_Parsedis_ExpNode *t = NULL;
  if (p)
  {
    t = dis_new_dis_Parsedis_ExpNode(p->connective);
    t->atom = dis_copy_dis_TokenList(p->atom);
    t->leftson = copy_dis_Parsedis_ExpNode(p->leftson);
    t->rightson = copy_dis_Parsedis_ExpNode(p->rightson);
  }
  return t;
}

dis_PlNode *copy_dis_PlNode(dis_PlNode *p)
{
  dis_PlNode *t = NULL;
  if (p)
  {
    if (p->value != -1)
    {
      t = dis_new_dis_PlNode(p->connective);
      t->parse_vars = copy_dis_TypedList(p->parse_vars);
      t->atom = dis_copy_dis_TokenList(p->atom);
      t->sons = copy_dis_PlNode(p->sons);
      t->comp = p->comp;
      t->neft = p->neft;
      t->lh = copy_dis_Parsedis_ExpNode(p->lh);
      t->rh = copy_dis_Parsedis_ExpNode(p->rh);
      t->value = p->value;  
    }
    else
      t = dis_new_dis_PlNode(dis_TRU);
    t->next = copy_dis_PlNode(p->next);
  }
  return t;
}

void xfree(void *ptr)
{
  if (ptr != NULL)
    free(ptr);
}

void dis_destroy_state(dis_State *state)
{
  xfree(state->F);
  xfree(state->f_D);
  xfree(state->f_V);
}
