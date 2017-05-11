

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
 * File: instantiateII.h
 * Description: headers for graph
 *
 * Author: Joerg Hoffmann 2002
 *
 *********************************************************************/ 












#ifndef _GRAPH_H
#define _GRAPH_H



Bool build_graph( int *min_time );
void build_graph_evolution_step( void );
Bool apply_operator( int time, int op );
Bool are_there_non_exclusive( int time, BitVector *bits, int *F, int num_F );
Bool get_them_non_exclusive( int time, BitVector *bits, int *F, int num_F, BitVector **bit_P_exclusives );
void copy_graph_layer( int time );



void find_mutex_ops( int time );
Bool interfere( int op1, int op2 );
Bool competing_needs( int time, int op1, int op2 );

void find_mutex_fts( int time );
Bool facts_are_exclusive( int time, int ft1, int ft2 );

void MAKE_OPS_EXCLUSIVE( int time, int op1, int op2 ); 
void MAKE_OPS_UNEXCLUSIVE( int time, int op1, int op2 );
void MAKE_FTS_EXCLUSIVE( int time, int ft1, int ft2 ); 
void MAKE_FTS_UNEXCLUSIVE( int time, int ft1, int ft2 );



#endif /* _GRAPH_H */
