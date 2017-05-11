

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
 * File: memory.h
 * Description: Creation / Deletion functions for all data structures.
 *
 * Author: Joerg Hoffmann / Frank Rittinger
 *
 *********************************************************************/ 






#ifndef _MEMORY_H
#define _MEMORY_H





char *new_Token( int len );
TokenList *new_TokenList( void );
FactList *new_FactList( void );
PlNode *new_PlNode( Connective c );
PlOperator *new_PlOperator( char *name );
PlOperator *new_axiom_op_list( void );
type_tree new_type_tree( char *name );
type_tree_list new_type_tree_list( char *name );



Operator *new_Operator( char *name, int norp );
ActionTemplate *new_ActionTemplate( int op );
Action *new_Action( int op );



BitVector *new_BitVector( int length );
IntList *new_IntList( int i1 );
IntPair *new_IntPair( int i1, int i2 );
IntBitVectorList *new_IntBitVectorList( int i1, BitVector *bv, int num_bit );
FtLevelInfo *new_FtLevelInfo( void );
OpLevelInfo *new_OpLevelInfo( void );



StateHashEntry *new_StateHashEntry( void );





void free_TokenList( TokenList *source );
void free_FactList( FactList *source );
void free_PlNode( PlNode *node );
void free_PlOperator( PlOperator *o );
void free_Operator( Operator *o );
void free_single_ActionTemplate( ActionTemplate *t );
void free_IntList( IntList *il );
void free_IntPair( IntPair *ip );
void free_IntBitVectorList( IntBitVectorList *ibvl );





#endif /* _MEMORY_H */
