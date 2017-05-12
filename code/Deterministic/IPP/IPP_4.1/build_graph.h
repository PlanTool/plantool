


/*********************************************************************
 * File: build_graph.h
 * Description: headers for building the graph
 *
 * Author: Joerg Hoffmann 1998
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




#ifndef _BUILD_GRAPH_H
#define _BUILD_GRAPH_H






Bool build_graph( int *min_time );


void build_graph_evolution_step( void );


void apply_noops( int time );
Bool apply_operator( int time, BitOperator *op );
Bool apply_all_effects( int time, OpNode *op_node ); 
Bool apply_effect( int time, Effect *ef, OpNode *op_node );


void insert_potential_effects( int time );
Bool apply_potential_effects( int time, OpNode *op_node, Bool *hit );
Bool apply_potential_effect( int time, Effect *ef, OpNode *op_node );
Bool potential_applicable( int time, Effect *ef, OpNode *op_node );
void integrate_potential_effects( int time );


Bool are_there_non_exclusive( int time, FactInfo *pos, FactInfo *neg );
Bool get_them_non_exclusive( int time,
			     FactInfo *pos, FactInfo *neg,
			     BitVector **pos_exclusives, BitVector **neg_exclusives );


void insert_op_edge( OpEdge **l, OpNode *op );
void insert_ft_edge( FtEdge **l, FtNode *ft );
void insert_ef_edge( EfEdge **l, EfNode *ef );



#endif /* _BUILD_GRAPH_H */

