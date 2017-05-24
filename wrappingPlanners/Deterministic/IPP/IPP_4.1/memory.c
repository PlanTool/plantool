



/*********************************************************************
 * File: memory.c
 * Description: Creation functions for all data structures.
 *
 * Author: Joerg Hoffmann / Frank Rittinger
 *
 *********************************************************************/ 
/*********************************************************************
 * (C) Copyright 1998 Albert Ludwigs University Freiburg
 *     Institute of Computer Science
 *
 * All rights reserved. Use of this software is permitted for 
 * non-commercial research purposes, and it may be copied only 
 * for that use.  All copies must include this copyright message.
 * This software is made available AS IS, and neither the authors
 * nor the  Albert Ludwigs University Freiburg make any warranty
 * about the software or its performance. 
 *********************************************************************/



#include "ipp.h"
#include "utilities.h"
#include "memory.h"
#include "pddl.h"







/**********************************************************************
 ********************** CREATION FUNCTIONS ****************************
 *********************************************************************/

/**********************************************************************
 * Allocates memory for a new string with length len.
 *
 * int len: length of string to allocate INCLUDING the '\0'
 * RETURNS a pointer to the allocated memory
 *********************************************************************/
char *
new_token(int len)
{
  char * tok = (char*) calloc(len, sizeof(char));
  CHECK_PTR(tok);
#ifdef MEMORY_INFO
  gmemory += len * sizeof(char);
#endif
  return tok;
}

/**********************************************************************
 * Allocates new memory for a list of strings and initializes
 * all members to NULL.
 *
 * RETURNS a pointer to the allocated memory
 *********************************************************************/
TokenList *
new_token_list(void)
{
  TokenList * result = (TokenList *) malloc(sizeof(TokenList));
  CHECK_PTR(result);
#ifdef MEMORY_INFO
  gmemory += sizeof(TokenList);
#endif
  result->item = NULL; 
  result->next = NULL;
  return result;
}

/**********************************************************************
 * Allocates new memory for a list of lists of strings and 
 * initializes all members to NULL.
 *
 * RETURNS a pointer to the allocated memory
 *********************************************************************/
FactList * 
new_fact_list(void)
{
  FactList * result = (FactList *) malloc(sizeof(FactList));
  CHECK_PTR(result);
#ifdef MEMORY_INFO
  gmemory += sizeof(FactList);
#endif
  result->item = NULL; 
  result->next = NULL;
  return result;
}

/**********************************************************************
 * Allocates new memory for a PL1 tree node.
 *
 * Connective c: The type of node, see pddl.h for a full description.
 * RETURNS a pointer to the allocated memory
 *********************************************************************/
PlNode *
new_pl_node(Connective c)
{
  PlNode * result = (PlNode *) malloc(sizeof(PlNode));
  CHECK_PTR(result);
#ifdef MEMORY_INFO
  gmemory += sizeof(PlNode);
#endif
  result->connective = c;
  result->atom = NULL;
  result->sons = NULL;
  result->next = NULL;
  return result;
}

CodeNode *new_CodeNode( Connective c )

{

  CodeNode *result = ( CodeNode * ) calloc( 1, sizeof( CodeNode ) );
  CHECK_PTR(result);
#ifdef MEMORY_INFO
  gmemory += sizeof( CodeNode );
#endif

  result->connective = c;

  result->var = 0;
  result->var_type = 0;
  result->predicate = 0;

  result->visited = FALSE;

  result->sons = NULL;
  result->next = NULL;

  return result;

}

/**********************************************************************
 * Allocates new memory for a PL1 style operator.
 *
 * char * name: The name of the operator, memory will be allocated.
 * RETURNS a pointer to the allocated memory
 *********************************************************************/
PlOperator *
new_pl_operator(char * name)
{
  PlOperator * result = (PlOperator *) malloc(sizeof(PlOperator));
  CHECK_PTR(result);
#ifdef MEMORY_INFO
  gmemory += sizeof(PlOperator);
#endif
  if (NULL != name)
    {
      result->name = new_token_list();
      result->name->item = new_token(strlen(name)+1);
      CHECK_PTR(result->name->item);
      strcpy(result->name->item, name);
    }
  else
    {
      result->name = NULL;
    }

  result->params = NULL;
  result->preconds = NULL;
  result->effects = NULL;
  result->number_of_real_params = 0;
  result->next = NULL;
  return result;
}


CodeOperator *new_CodeOperator( void )

{

  int i;

  CodeOperator * result = ( CodeOperator * ) calloc( 1, sizeof( CodeOperator ) );
  CHECK_PTR(result);
#ifdef MEMORY_INFO
  gmemory += sizeof( CodeOperator );
#endif

  result->name = NULL;

  result->num_vars = 0;
  /* achtung: var_types und inst_table bleiben uninitialisiert!
   * --> wird nicht drauf zugegriffen, immer von vorne bis num_vars
   *
   * mir egal.
   */
  for ( i = 0; i < MAX_VARS; i++ ) {
    result->inst_table[i] = -1;
  }

  result->preconds = NULL;
  result->conditionals = NULL;

  result->number_of_real_params = 0;

  result->next = NULL;

  return result;

}


/**********************************************************************
 * Allocates new memory for a PL1 style axiom operator. Creates a
 * unique name and marks it with the hidden string.
 *
 * RETURNS a pointer to the allocated memory
 *********************************************************************/
PlOperator *
new_axiom_op_list(void)
{
  static int count;
  char * name;
  PlOperator * ret;

  /* WARNING: count should not exceed 999 */
  count++;
  name = new_token(strlen(HIDDEN_STR)+strlen(AXIOM_STR)+3+1);
  sprintf(name, "%s%s%d", HIDDEN_STR, AXIOM_STR, count);

  ret = new_pl_operator(name);
  free(name);

  return ret;
}

/*
 * memory functions for graph stuff...
 */


OpNode *new_op_node( int time, char *name, Bool is_noop,
		     BitVector *pos_precond_vector,
		     BitVector *neg_precond_vector )

{

  OpNode *tmp = ( OpNode * ) calloc ( 1, sizeof( OpNode ) );
  CHECK_PTR( tmp );
#ifdef MEMORY_INFO
  ggraph_memory += sizeof( OpNode );
#endif

  if ( name ) {
    tmp->name = copy_string( name );
  } else {
    tmp->name = NULL;
  }
  tmp->index = gops_count;

  tmp->uid_block = gops_count / gcword_size;
  tmp->uid_mask = 1 << ( gops_count % gcword_size );
  gops_count++;

  tmp->preconds = NULL;
  tmp->unconditional = NULL;
  tmp->conditionals = NULL;

  tmp->is_noop = is_noop;

  tmp->info_at[time] = new_op_level_info();

  tmp->pos_precond_vector = pos_precond_vector;
  tmp->neg_precond_vector = neg_precond_vector;

  tmp->next = NULL;

  tmp->unactivated_effects = NULL;
  tmp->thread = NULL;

  return tmp;

}

EfNode *new_ef_node( int time, OpNode *op,
		     BitVector *pos_effect_vector,
		     BitVector *neg_effect_vector )

{

  EfNode *tmp = ( EfNode * ) calloc ( 1, sizeof( EfNode ) );
  CHECK_PTR( tmp );
#ifdef MEMORY_INFO
  ggraph_memory += sizeof( EfNode );
#endif

  tmp->op = op;
  tmp->first_occurence = time;

  tmp->conditions = NULL;
  tmp->effects = NULL;

  tmp->pos_effect_vector = pos_effect_vector;
  tmp->neg_effect_vector = neg_effect_vector;

  tmp->info_at[time] = new_ef_level_info();

  tmp->next = NULL;

  return tmp;

}

FtNode *new_ft_node( int time, int index, Bool positive, Bool dummy )

{

  int i;

  FtNode *tmp = ( FtNode * ) calloc ( 1, sizeof( FtNode ) );
  CHECK_PTR( tmp );
#ifdef MEMORY_INFO
  ggraph_memory += sizeof( FtNode );
#endif

  if ( !dummy ) {
    gfacts_count++;
  }
  if ( positive ) gprint_ftnum++;

  tmp->index = index;
  tmp->uid_block = index / gcword_size;
  tmp->uid_mask = 1 << ( index % gcword_size );

  tmp->positive = positive;

  tmp->adders = NULL;
  tmp->preconds = NULL;

  tmp->noop = NULL;

  tmp->info_at[time] = new_ft_level_info( tmp );
  for ( i = 0; i < time; i++ ) {
    tmp->info_at[i] = NULL;
  }

  tmp->next = NULL;

  return tmp;

}

FtEdge *new_ft_edge( FtNode *ft )

{

  FtEdge *tmp = ( FtEdge * ) calloc ( 1, sizeof( FtEdge ) );
  CHECK_PTR( tmp );
#ifdef MEMORY_INFO
  ggraph_memory += sizeof( FtEdge );
#endif

  tmp->ft = ft;
  tmp->next = NULL;

  return tmp;

}

EfEdge *new_ef_edge( EfNode *ef )

{

  EfEdge *tmp = ( EfEdge * ) calloc ( 1, sizeof( EfEdge ) );
  CHECK_PTR( tmp );
#ifdef MEMORY_INFO
  ggraph_memory += sizeof( EfEdge );
#endif

  tmp->ef = ef;
  tmp->next = NULL;

  return tmp;

}

OpEdge *new_op_edge( OpNode *op )

{

  OpEdge *tmp = ( OpEdge * ) calloc ( 1, sizeof( OpEdge ) );
  CHECK_PTR( tmp );
#ifdef MEMORY_INFO
  ggraph_memory += sizeof( OpEdge );
#endif

  tmp->op = op;
  tmp->next = NULL;

  return tmp;

}

OpLevelInfo *new_op_level_info( void )

{

  OpLevelInfo *tmp = ( OpLevelInfo * ) calloc ( 1, sizeof( OpLevelInfo ) );
  CHECK_PTR( tmp );
#ifdef MEMORY_INFO
  ggraph_memory += sizeof( OpLevelInfo );
#endif

  tmp->is_used = 0;
  tmp->exclusives = NULL;

  return tmp;

}

EfLevelInfo *new_ef_level_info( void )

{

  EfLevelInfo *tmp = ( EfLevelInfo * ) calloc ( 1, sizeof( EfLevelInfo ) );
  CHECK_PTR( tmp );
#ifdef MEMORY_INFO
  ggraph_memory += sizeof( EfLevelInfo );
#endif

  tmp->is_dummy = FALSE;

  tmp->cond_pos_exclusives = NULL;
  tmp->cond_neg_exclusives = NULL;

  return tmp;

}

FtLevelInfo *new_ft_level_info( FtNode *ft )

{

  FtLevelInfo *tmp = ( FtLevelInfo * ) calloc ( 1, sizeof( FtLevelInfo ) );
  CHECK_PTR( tmp );
#ifdef MEMORY_INFO
  ggraph_memory += sizeof( FtLevelInfo );
#endif

  tmp->adders_pointer = NULL;
  tmp->is_dummy = FALSE;

  tmp->is_goal = 0;
  tmp->is_true = 0;
  tmp->is_goal_for[0] = NULL;

  tmp->pos_exclusives = new_excl_bit_vector( gft_vector_length );
  tmp->neg_exclusives = new_excl_bit_vector( gft_vector_length );
  /* adders erst spaeter allozieren !: groesse op - vecs unbekannt */

  /* achtung! funktioniert nur, falls bitvectoren konstant lang!! */
  if ( ft->positive ) {
    (tmp->neg_exclusives)[ft->uid_block] |= ft->uid_mask;
  } else {
    (tmp->pos_exclusives)[ft->uid_block] |= ft->uid_mask;
  }

  tmp->adders = NULL;
  tmp->adders_exclusives = NULL;

  tmp->memo_start = NULL;

  return tmp;

}


OpPair *new_op_pair( OpNode *o1, OpNode *o2 )

{

  OpPair *tmp = ( OpPair * ) calloc ( 1, sizeof( OpPair ) );
  CHECK_PTR( tmp );
#ifdef MEMORY_INFO
  gexcl_memory += sizeof( OpPair );
#endif

  tmp->o1 = o1;
  tmp->o2 = o2;

  tmp->next = NULL;

  return tmp;

}
  

FtPair *new_ft_pair( FtNode *f1, FtNode *f2 )

{

  FtPair *tmp = ( FtPair * ) calloc ( 1, sizeof( FtPair ) );
  CHECK_PTR( tmp );
#ifdef MEMORY_INFO
  gexcl_memory += sizeof( FtPair );
#endif

  tmp->f1 = f1;
  tmp->f2 = f2;

  tmp->next = NULL;

  return tmp;

}
  


MemoNode *new_memo_node( int double_index, int way )

{

  MemoNode *tmp = ( MemoNode * ) calloc( 1, sizeof( MemoNode ) );
  CHECK_PTR( tmp );
#ifdef MEMORY_INFO
  gmemo_memory += sizeof( MemoNode );
#endif

  tmp->double_index = double_index;

  tmp->sons = NULL;

  tmp->min_way = way;

  /* evtl. prev = NULL
   */
  tmp->next = NULL;

  return tmp;

}


MemoNode_table *new_memo_node_table( void )

{

  MemoNode_table *tmp = ( MemoNode_table * ) calloc( 1, sizeof( MemoNode_table ) );
  CHECK_PTR( tmp );
#ifdef MEMORY_INFO
  gmemo_memory += sizeof( MemoNode_table );
#endif

  return tmp;

}



Candidate *new_candidate( void )

{

  Candidate *tmp = ( Candidate * ) calloc( 1, sizeof( Candidate ) );
  CHECK_PTR( tmp );
#ifdef MEMORY_INFO
  gwave_memory += sizeof( Candidate );
#endif

  return tmp;

}




RelevantFact *new_RelevantFact( CodeNode *n )

{

  RelevantFact *r;
  int i;

  r = ( RelevantFact * ) calloc( 1, sizeof( RelevantFact ) );

  CHECK_PTR( r );
#ifdef MEMORY_INFO
  ggraph_memory += sizeof( Candidate );
#endif

  if ( n ) {
    r->predicate = n->predicate;
    for ( i=0; i<garity[n->predicate]; i++ ) {
      r->arguments[i] = n->arguments[i];
    }
  }

  return r;

}





BitOperator *new_BitOperator( char *name )

{


  BitOperator *tmp = ( BitOperator * ) calloc( 1, sizeof( BitOperator ) );
  CHECK_PTR( tmp );
#ifdef MEMORY_INFO
  gmemo_memory += sizeof( BitOperator );
#endif

  if ( name ) {
    tmp->name = copy_string( name );
  } else {
    tmp->name = NULL;
  }
  tmp->p_preconds = new_FactInfo();
  tmp->n_preconds = new_FactInfo();
  tmp->unconditional = NULL;
  tmp->conditionals = NULL;

  tmp->next = NULL;

  return tmp;

}





Effect *new_Effect( void )

{

  Effect *tmp = ( Effect * ) calloc( 1, sizeof( Effect ) );
  CHECK_PTR( tmp );
#ifdef MEMORY_INFO
  gmemory += sizeof( Effect );
#endif

  tmp->p_conds = new_FactInfo();
  tmp->n_conds = new_FactInfo();
  tmp->p_effects = new_FactInfo();
  tmp->n_effects = new_FactInfo();

  tmp->next = NULL;
  
  return tmp;

}



FactInfo *new_FactInfo( void )

{

  FactInfo *tmp = ( FactInfo * ) calloc( 1, sizeof( FactInfo ) );
  CHECK_PTR( tmp );
#ifdef MEMORY_INFO
  gmemory += sizeof( FactInfo );
#endif

  tmp->vector = new_bit_vector( gft_vector_length );
  tmp->indices = NULL;

  return tmp;

}


  

/**********************************************************************
 * Creates a new bit vector with all bits unset.
 *
 * int length: The length of the vector in integer.
 *
 * RETURNS a new bit vector with all bits set to 0.
 *********************************************************************/
BitVector *
new_bit_vector(int length)
{
  BitVector * result = (BitVector *) calloc(length, sizeof(int));

  CHECK_PTR(result);
#ifdef MEMORY_INFO
  gmemory += length;
#endif
  memset(result, 0, length);

  return result;
}

BitVector *
new_excl_bit_vector(int length)
{
  BitVector * result = (BitVector *) calloc(length, sizeof(int));

  CHECK_PTR(result);
#ifdef MEMORY_INFO
  gexcl_memory += length;
#endif
  memset(result, 0, length);

  return result;
}


/**********************************************************************
 * Creates a new instantiated effect, and creates all necessary
 * bit vectors with the global bit vector length "gft_vector_length".
 * 
 * NOTE: gft_vector_length is global!
 *       The conditions are NOT allocated! 
 *
 * RETURNS a new instantiated effect.
 *********************************************************************/
Effect * 
new_effect()
{
  Effect * result = (Effect *) malloc(sizeof(Effect));
  CHECK_PTR(result);
#ifdef MEMORY_INFO
  gmemory += sizeof(Effect);
#endif
  result->p_conds = NULL;
  result->n_conds = NULL;
  result->p_effects = new_fact_info();
  result->n_effects = new_fact_info();
  result->next = NULL;
  return result;
}




/*********************************************************************
 * Creates a new structure that holds one integer !!
 * 
 * NOTE: The index is initialized to -1.
 *
 * int i: The value for index.
 *
 * RETURNS a pointer to the new structure.
 *********************************************************************/
Integers * 
new_integers(int i)
{
  Integers * result = (Integers *) malloc(sizeof(Integers));
  CHECK_PTR(result);
#ifdef MEMORY_INFO
  gmemory += sizeof(Integers);
#endif

  result->index = i;
  result->next = NULL;
  return result;
}




/*********************************************************************
 * Creates a new fact info structure with a bit vector with the
 * correct length. A fact info is a tuple of a bit vector and a list
 * of integers. The integers are the indices of the set bits in the
 * vector. 
 * 
 * NOTE: indices is initialized to NULL;
 *
 * RETURNS a pointer to the new structure.
 *********************************************************************/
FactInfo * 
new_fact_info()
{
  FactInfo * result = (FactInfo *) malloc(sizeof(FactInfo));
  CHECK_PTR(result);
#ifdef MEMORY_INFO
  gmemory += sizeof(FactInfo);
#endif
  
  result->vector = new_bit_vector(gft_vector_length);
  result->indices = NULL;
  return result;
}

/*********************************************************************
 * Creates a new pair of two fact info structures.
 *
 * FactInfo * pos: This represents the positive part.
 * FactInfo * neg: This represents the negaitve part.
 *
 * RETURNS a pointer to the new structure.
 *********************************************************************/
FactInfoPair * 
new_fact_info_pair(FactInfo * pos, FactInfo * neg)
{
  FactInfoPair * result = (FactInfoPair *) malloc(sizeof(FactInfoPair));
  CHECK_PTR(result);
#ifdef MEMORY_INFO
  gmemory += sizeof(FactInfoPair);
#endif
  
  result->positive = pos;
  result->negative = neg;
  return result;
}

/**********************************************************************
 ********************** DELETION FUNCTIONS ****************************
 *********************************************************************/

/*********************************************************************
 * Free a complete TokenList, but do not delete predicates and 
 * constants. It is therefore important that all variables start 
 * with a unique identifier, we use '?'.
 * 
 * TokenList * tl: The beginning of the TokenList to free.
 *********************************************************************/
void
free_token_list(TokenList * tl)
{
    if (NULL != tl) {
	free_token_list(tl->next);
	/* Now free the token, but only if it is a variable. */
	if (NULL != tl->item && '?' == *(tl->item)) {
	    free(tl->item);
	}
	free(tl); 
    }
}

/**********************************************************************
 * Free a complete FactList, but do not free any predicates or 
 * constants, i.e. only delete strings beginning with a '?'.
 *
 * FactList * list: the list that should be deleted.
 *********************************************************************/
void
free_fact_list(FactList * list)
{
  if (NULL != list)
    {
      free_fact_list(list->next);
      /* This function takes care of predicates and constants. */
      free_token_list(list->item);
      free(list);
    }
}

/*********************************************************************
 * Free a complete tree, but leave predicate names in memory.
 * 
 * PlNode * node: The root of the tree to free.
 *********************************************************************/
void 
free_tree(PlNode * node)
{
    if (NULL != node) {
	free_tree(node->sons);
	free_tree(node->next);
	/* now free the node itself */
	free_token_list(node->atom);
	free(node);
    }
}


void free_CodeNode( CodeNode *node )

{

  if ( node ) {
    free_CodeNode( node->sons );
    free_CodeNode( node->next );
#ifdef MEMORY_INFO
    gmemory -= sizeof( CodeNode );
#endif
    free( node );
  }

}


void free_CodeOperator( CodeOperator *arme_sau )

{

  if ( arme_sau ) {
    if ( arme_sau->name ) {
      free( arme_sau->name );
    }
    free_CodeNode( arme_sau->preconds );
    free_CodeNode( arme_sau->conditionals );
    /* resources are still missing. (I'll be missing you...)
     */
#ifdef MEMORY_INFO
    gmemory -= sizeof( CodeOperator );
#endif
    free( arme_sau );
  }

}


/*********************************************************************
 * Free a pl_node including its atom tokenlist, but leave predicate 
 *     names in memory.
 * 
 * PlNode * node: The node to free.
 *********************************************************************/
void 
free_pl_node(PlNode * node)
{
    if (NULL != node) {
	free_token_list(node->atom);
	free(node);
    }
}

/**********************************************************************
 *
 *********************************************************************/
void 
free_complete_token_list(TokenList * source )
{
  if ( source )
    {
      free_complete_token_list( source->next );
      if ( source->item ) {
        free( source->item );
	source->item = NULL;
      }
      free( source );
    }
}

/**********************************************************************
 *
 *********************************************************************/
void 
free_complete_fact_list(FactList * source )
{
  if ( source )
    {
      free_complete_fact_list( source->next );
      free_complete_token_list( source->item );
      free( source );
    }
}

/**********************************************************************
 * Delete a list of operators and take care of predicates and
 * constants.
 * 
 * PlOperator * op: The start of the list that is deleted.
 **********************************************************************/
void 
free_ops(PlOperator * op)
{
    if (NULL != op) {
	free_ops(op->next);
	
	free_complete_token_list(op->name);
	free_complete_fact_list(op->params);
	free_tree(op->preconds);
	free_tree(op->effects);
	free(op);
	/* Resources are still missing. */
    }
}

/**********************************************************************
 * Delete a PlOperator and take care of predicates and
 * constants.
 * 
 * PlOperator * op: The operator that is deleted.
 **********************************************************************/
void 
free_pl_op(PlOperator * op)
{
    if (NULL != op) {
	free_token_list(op->name);
	free_fact_list(op->params);
	free_tree(op->preconds);
	free_tree(op->effects);
	free(op);
	/* Resources are still missing. */
    }
}

/**********************************************************************
 *
 *********************************************************************/


void free_integers( Integers *l )

{

  if ( l != NULL ) {
    free_integers( l->next );
    free( l );
  }

}

void free_partial_effect( Effect *ef )

{

  if ( ef != NULL ) {
    free_fact_info( ef->p_conds );
    free_fact_info( ef->n_conds );
    free_integers( ef->p_effects->indices );
    free( ef->p_effects );
    free_integers( ef->n_effects->indices );
    free( ef->n_effects );
    free( ef );
  }

}

void free_partial_operator( BitOperator *op )

{

  if ( op != NULL ) {
    if ( op->name ) {
      free( op->name );
    }
    /* preconds: we need the vectors in search; only free indices and
     * structure holding parts together
     */
    free_integers( op->p_preconds->indices );
    free( op->p_preconds );
    free_integers( op->n_preconds->indices );
    free( op->n_preconds );
    if ( op->unconditional ) {
      /* unconditional is thorwn away, except for the strings
       * describing the pos and neg effects;
       * keep those for exclusions (interfere) check
       */
      free_integers( op->unconditional->p_effects->indices );
      free( op->unconditional->p_effects );
      free_integers( op->unconditional->n_effects->indices );
      free( op->unconditional->n_effects );
      free_fact_info( op->unconditional->p_conds );
      free_fact_info( op->unconditional->n_conds );
      free( op->unconditional );
    }
    /* do not touch the conditionals at all, those can be activated in 
     * later time steps, so we have to keep them still
     * (get pointed to by op_node->unactivated_effects)
     */

    free( op );
  }

}
    
void free_fact_info_pair( FactInfoPair *p )

{

  if ( p ) {
    free_fact_info( p->positive );
    free_fact_info( p->negative );
    
    free( p );
  }

}

  

/**********************************************************************
 *
 *********************************************************************/
void 
free_bit_vector(BitVector * bvec)
{
  if ( bvec ) {
    free(bvec);
  }
}


/**********************************************************************
 *
 *********************************************************************/
void 
free_effect(Effect * effect)
{
  if (NULL != effect)
    {
      free_effect(effect->next);
      free_fact_info(effect->p_conds);
      free_fact_info(effect->n_conds);
      free_fact_info(effect->p_effects);
      free_fact_info(effect->n_effects);
      free(effect);
    }
}




/**********************************************************************
 *
 *********************************************************************/
void 
free_fact_info(FactInfo * info)
{

  if ( info != NULL ) {
    free_bit_vector(info->vector);
    free_integers(info->indices);
    free( info );
  }

}



void free_BitOperator( BitOperator *op )

{

  if ( op != NULL ) {
    if ( op->name ) {
      free( op->name );
    }
    free_fact_info( op->p_preconds );
    free_fact_info( op->n_preconds );
    free_effect( op->unconditional );
    free_effect( op->conditionals );
    free( op );
  }

}



void free_complete_FactList( FactList *source )

{

  if ( source ) {
    free_complete_FactList( source->next );
    free_complete_TokenList( source->item );
    free( source );
  }

}



void free_complete_TokenList( TokenList *source )

{

  if ( source ) {
    free_complete_TokenList( source->next );
    if ( source->item ) {
      free( source->item );
    }
    free( source );
  }

}
