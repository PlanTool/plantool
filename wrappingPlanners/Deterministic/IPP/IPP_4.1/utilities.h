



/*********************************************************************
 * File: utilities.h
 * Description: 
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



#ifndef _UTILITIES_H
#define _UTILITIES_H





char *copy_string( char *s );
Bool token_lists_equal( TokenList *fst, TokenList *snd );
TokenList *copy_complete_token_list( TokenList *source, TokenList **end );
void strupcase( char *from );
char *rmdash( char *s );
TokenList *copy_token_list( TokenList *in_tl );
int is_tl_element( char *s, TokenList *tl );
Bool tl_is_fl_element( TokenList *tl, FactList *fl );
FactList *copy_fact_list( FactList *in_fl );
PlNode *copy_pl_node( PlNode *node );
PlNode *deep_copy_tree( PlNode *node );
void copy_contents_of_CodeNode( CodeNode **dest, CodeNode *source );
CodeNode *copy_CodeNode( CodeNode *source );
CodeNode *deep_copy_CodeTree( CodeNode *node );
PlOperator *copy_operator( PlOperator *op );
PlOperator *copy_operator_list( PlOperator *old_op );
PlOperator *remove_empty_ops( PlOperator *ops );
Effect *copy_effects( Effect *e );
void copy_contents_of_FactInfo( FactInfo **dst, FactInfo *src );
Integers *copy_Integers( Integers *i );
BitVector *copy_bit_vector( BitVector *vec, int vec_len );




#endif /* _UTILITIES_H */
