



/*********************************************************************
 * File: output.h
 * Description: Functions for printing the data structures or 
 *              other general output.
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



#ifndef _OUTPUT_H
#define _OUTPUT_H



#include "ipp.h"



void print_factlist( FactList *list, char *sepf, char *sept );
void print_tokenlist( TokenList *list, char *sep );
void print_hidden_tokenlist( TokenList *list, char *sep );
void print_plops( PlOperator *plop );
void print_indent( int indent );
void print_plnode( PlNode *plnode, int indent );
void print_CodeNode( CodeNode *node, int indent );
void print_vector( BitVector *vec, int vec_len );
void print_plan( int time );
void print_incremental_plan( Candidate *c );
void print_fact_info( FactInfo *f_info, int vec_len );
void spec_error( char *s );
void print_plop( PlOperator *plop );
void print_CodeOperator( CodeOperator *op );
void print_BitOperator( BitOperator *o );
void print_Effect( Effect *e );
void print_FactInfo( FactInfo *f );
void print_BitVector( BitVector *vec, int vec_len );
void print_fact( short int predicate, ArgArray arguments );
void print_op_name( OpNode *op );
void print_ft_name( FtNode *ft );


#endif /* _OUTPUT_H */
