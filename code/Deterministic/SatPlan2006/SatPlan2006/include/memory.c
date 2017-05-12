


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
 * File: memory.c
 * Description: Creation and Deletion functions for all data structures.
 *
 * Author: Joerg Hoffmann
 *
 *********************************************************************/ 








#include <string.h>
#include "bb.h"
#include "memory.h"









/**********************
 * CREATION FUNCTIONS *
 **********************/











/* parsing
 */









char *new_Token( int len )

{

  char *tok = ( char * ) calloc( len, sizeof( char ) );
  CHECK_PTR(tok);

  return tok;

}



TokenList *new_TokenList( void )

{

  TokenList *result = ( TokenList * ) calloc( 1, sizeof( TokenList ) );
  CHECK_PTR(result);

  result->item = NULL; 
  result->next = NULL;

  return result;

}



FactList *new_FactList( void )

{

  FactList *result = ( FactList * ) calloc( 1, sizeof( FactList ) );
  CHECK_PTR(result);

  result->item = NULL; 
  result->next = NULL;

  return result;

}



PlNode *new_PlNode( Connective c )

{

  PlNode *result = ( PlNode * ) calloc( 1, sizeof( PlNode ) );
  CHECK_PTR(result);

  result->connective = c;
  result->atom = NULL;
  result->sons = NULL;
  result->next = NULL;

  return result;

}



PlOperator *new_PlOperator( char *name )

{

  PlOperator *result = ( PlOperator * ) calloc( 1, sizeof( PlOperator ) );
  CHECK_PTR(result);

  if ( name ) {
    result->name = new_Token(strlen(name)+1);
    CHECK_PTR(result->name);
    strcpy(result->name, name);
  } else {
    result->name = NULL;
  }

  result->params = NULL;
  result->preconds = NULL;
  result->effects = NULL;
  result->number_of_real_params = 0;
  result->next = NULL;

  return result;

}



PlOperator *new_axiom_op_list( void )

{

  static int count;
  char *name;
  PlOperator *ret;

  /* WARNING: count should not exceed 999 
   */
  count++;
  if ( count == 10000 ) {
    printf("\ntoo many axioms! look into memory.c, line 157\n\n");
    exit( 1 );
  }
  name = new_Token(strlen(HIDDEN_STR)+strlen(AXIOM_STR)+4+1);
  sprintf(name, "%s%s%4d", HIDDEN_STR, AXIOM_STR, count);

  ret = new_PlOperator(name);
  free(name);

  return ret;

}



type_tree new_type_tree( char *name )

{

  type_tree act_type;
  
  if (!name) {
    return NULL;
  }

  act_type = ( type_tree ) calloc( 1, sizeof( type_tree_elt ) );
  CHECK_PTR(act_type);

  act_type->name = new_Token( strlen( name ) + 1 );
  strcpy( act_type->name, name );
  act_type->sub_types = NULL;

  return act_type;

}



type_tree_list new_type_tree_list( char *name )

{

  type_tree_list act_type_list;
  
  act_type_list = ( type_tree_list ) calloc( 1, sizeof( type_tree_list_elt ) );
  CHECK_PTR(act_type_list);

  if ( name ) {
    act_type_list->item = new_type_tree( name );
  } else {
    act_type_list->item = NULL;
  }

  act_type_list->next = NULL;
  
  return act_type_list;

}









/* instantiation
 */









Operator *new_Operator( char *name, int norp )

{

  int i;

  Operator *result = ( Operator * ) calloc( 1, sizeof( Operator ) );
  CHECK_PTR(result);

  if ( name ) {
    result->name = new_Token( strlen( name ) + 1 );
    CHECK_PTR( result->name );
    strcpy( result->name, name );
  } else {
    result->name = NULL;
  }

  result->num_vars = 0;
  result->number_of_real_params = norp;

  for ( i = 0; i < MAX_VARS; i++ ) {
    result->inst_table[i] = -1;
  }

  result->num_preconds = 0;
  result->num_adds = 0;
  result->num_dels = 0;

  result->out = FALSE;

  return result;

}



ActionTemplate *new_ActionTemplate( int op )

{

  ActionTemplate *result = ( ActionTemplate * ) calloc( 1, sizeof( ActionTemplate ) );
  CHECK_PTR(result);

  result->op = op;

  result->next = NULL;

  return result;

}

 

Action *new_Action( int op )

{

  Action *result = ( Action * ) calloc( 1, sizeof( Action ) );
  CHECK_PTR(result);

  result->op = op;

  result->num_preconds = 0;
  result->num_adds = 0;
  result->num_dels = 0;

  result->next = NULL;

  return result;

}















/* graph
 */
















BitVector *new_BitVector( int length )

{

  BitVector *result = ( BitVector * ) calloc( length, sizeof( BitVector ) );
  CHECK_PTR(result);

  memset(result, 0, length);

  return result;

}



IntList *new_IntList( int i1 )

{

  IntList *res = ( IntList * ) calloc( 1, sizeof( IntList ) );
  CHECK_PTR(res);
  
  res->i1 = i1;
  res->prev = NULL;
  res->next = NULL;

  return res;

}



IntPair *new_IntPair( int i1, int i2 )

{

  IntPair *res = ( IntPair * ) calloc( 1, sizeof( IntPair ) );
  CHECK_PTR(res);
  
  res->i1 = i1;
  res->i2 = i2;
  res->prev = NULL;
  res->next = NULL;

  return res;

}



IntBitVectorList *new_IntBitVectorList( int i1, BitVector *bv, int num_bit )

{

  int i;

  IntBitVectorList *res = ( IntBitVectorList * ) calloc( 1, sizeof( IntBitVectorList ) );
  CHECK_PTR(res);
  
  res->i1 = i1;

  res->bv = new_BitVector( num_bit );
  for ( i = 0; i < num_bit; i++ ) {
    res->bv[i] = bv[i];
  }

  res->next = NULL;

  return res;

}



FtLevelInfo *new_FtLevelInfo( void )

{

  FtLevelInfo *tmp = ( FtLevelInfo * ) calloc ( 1, sizeof( FtLevelInfo ) );
  CHECK_PTR( tmp );

  /* adders stored as list to facilitate dynamic growth
   */
  tmp->A = NULL;
  tmp->end_A = NULL;

  /* op bit vectors:
   *
   * bit_A,
   * bit_A_exclusives
   *
   * allocated in main code for better readability
   */

  /* it is precond of these at this point
   */
  tmp->P = NULL;

  tmp->bit_exclusives = new_BitVector( gnum_ft_bit );

  tmp->status = 0;

  tmp->is_goal = FALSE;
  tmp->num_A = 0;

  tmp->rplan_frac = 0;

  return tmp;

}



OpLevelInfo *new_OpLevelInfo( void )

{

  OpLevelInfo *tmp = ( OpLevelInfo * ) calloc ( 1, sizeof( OpLevelInfo ) );
  CHECK_PTR( tmp );

  tmp->bit_P_exclusives = new_BitVector( gnum_ft_bit );
  /* op bit vector:
   *
   * exclusives
   *
   * allocated in main code as size not known at time of 
   * info allocation
   */

  tmp->status = 0;

  tmp->is_in_rplan = FALSE;

  tmp->losspos = -1;
  tmp->lossneg = -1;

  tmp->rplan_frac = 0;
  tmp->forced_in = FALSE;

  return tmp;

}






























StateHashEntry *new_StateHashEntry( void )

{

  StateHashEntry *result = ( StateHashEntry * ) calloc( 1, sizeof( StateHashEntry ) );
  CHECK_PTR(result);

  result->next = NULL;

  return result;

}

  








/**********************
 * DELETION FUNCTIONS *
 **********************/












void free_TokenList( TokenList *source )

{

  if ( source ) {
    free_TokenList( source->next );
    if ( source->item ) {
      free( source->item );
    }
    free( source );
  }

}



void free_FactList( FactList *source )

{

  if ( source ) {
    free_FactList( source->next );
    free_TokenList( source->item );
    free( source );
  }

}



void free_PlNode( PlNode *node )

{
  
  if ( node ) {
    free_PlNode( node->sons );
    free_PlNode( node->next );
    free_TokenList( node->atom );
    free( node );
  }

}



void free_PlOperator( PlOperator *o )

{

  if ( o ) {
    free_PlOperator( o->next );

    if ( o->name ) {
      free( o->name );
    }
    
    free_FactList( o->params );
    free_PlNode( o->preconds );
    free_PlNode( o->effects );

    free( o );
  }

}



void free_Operator( Operator *o )

{

  if ( o ) {

    if ( o->name ) {
      free( o->name );
    }

    free( o );
  } 

}



void free_single_ActionTemplate( ActionTemplate *t )

{

  if ( t ) {
    free( t );
  }

}



void free_IntList( IntList *il )

{

  if ( il ) {
    free_IntList( il->next );
    free( il );
  }

}



void free_IntPair( IntPair *ip )

{

  if ( ip ) {
    free_IntPair( ip->next );
    free( ip );
  }

}



void free_IntBitVectorList( IntBitVectorList *ibvl )

{

  if ( ibvl ) {
    free_IntBitVectorList( ibvl->next );
    if ( !ibvl->bv ) {
      /* REMOVE THIS, LATER, FOR EFFICIENCY (HA HA) -- bet my arse it won't happen anyway.
       */
      printf("\ntrying to free an ibvl with empty bv??\n\n");
      exit( 1 );
    }
    free( ibvl->bv );
    free( ibvl );
  }

}
