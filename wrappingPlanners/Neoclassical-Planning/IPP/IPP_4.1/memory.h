



/*********************************************************************
 * File: memory.h
 * Description: Creation / Deletion functions for all data structures.
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



#ifndef _MEMORY_H
#define _MEMORY_H



#include "ipp.h"




char *new_token( int len );
TokenList *new_token_list( void );
FactList *new_fact_list( void );
PlNode *new_pl_node(Connective c);
CodeNode *new_CodeNode( Connective c );
PlOperator *new_pl_operator( char *name );
CodeOperator *new_CodeOperator( void );
PlOperator *new_axiom_op_list( void );
OpNode *new_op_node( int time, char *name, Bool is_noop,
		     BitVector *pos_precond_vector,
		     BitVector *neg_precond_vector );
EfNode *new_ef_node( int time, OpNode *op,
		     BitVector *pos_effect_vector,
		     BitVector *neg_effect_vector );
FtNode *new_ft_node( int time, int index, Bool positive, Bool dummy );
OpEdge *new_op_edge( OpNode *op );
EfEdge *new_ef_edge( EfNode *ef );
FtEdge *new_ft_edge( FtNode *ft );
OpLevelInfo *new_op_level_info( void );
EfLevelInfo *new_ef_level_info( void );
FtLevelInfo *new_ft_level_info( FtNode *ft );
OpPair *new_op_pair( OpNode *o1, OpNode *o2 );
FtPair *new_ft_pair( FtNode *f1, FtNode *f2 );
MemoNode *new_memo_node( int double_index, int way );
MemoNode_table *new_memo_node_table( void );
Candidate *new_candidate( void );
RelevantFact *new_RelevantFact( CodeNode *n );
BitOperator *new_BitOperator( char *name );
Effect *new_Effect( void );
FactInfo *new_FactInfo( void );
BitVector *new_bit_vector(int length);
BitVector *new_excl_bit_vector(int length);
Effect *new_effect();
Integers *new_integers(int i);
FactInfo *new_fact_info();
FactInfoPair *new_fact_info_pair(FactInfo *pos, FactInfo *neg);



void free_token_list(TokenList *tl);void free_tree(PlNode *node);
void free_CodeNode( CodeNode *node );
void free_CodeOperator( CodeOperator *arme_sau );
void free_pl_node(PlNode *node);
void free_fact_list(FactList *list);
void free_complete_token_list(TokenList *source );
void free_complete_fact_list(FactList *source );
void free_ops(PlOperator *op);
void free_integers( Integers *l );
void free_partial_effect( Effect *ef );
void free_partial_operator( BitOperator *op );
void free_fact_info_pair( FactInfoPair *p );
void free_fact_info(FactInfo *info);
void free_pl_op(PlOperator *op);
void free_BitOperator( BitOperator *op );
void free_effect(Effect * effect);
void free_complete_FactList( FactList *source );
void free_complete_TokenList( TokenList *source );


#endif /* _MEMORY_H */
