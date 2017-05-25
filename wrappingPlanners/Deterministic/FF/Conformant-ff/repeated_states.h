


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
 *
 * File: repeated_states.h
 *
 * Description: headers for conformant hashing routines
 *
 * Author: Joerg Hoffmann 2002
 *
 *********************************************************************/ 






#ifndef _REPEATED_STATES_H
#define _REPEATED_STATES_H



void initialize_repeated_states( void );



void hash_ehc_node( EhcNode *n );
Bool ehc_state_hashed( State *S, EhcNode *father, int op );
Bool dominates_state( State *S1, State *S2,
		      EhcNode *S1ehcnode, EhcNode *S2ehcfather,
		      BfsNode *S1bfsnode, BfsNode *S2bfsfather, 
		      int op );
int state_Fsum( State *S );
int state_Usum( State *S );
void reset_ehc_hash_entrys( void );
Bool dominates_conformant( State *S1, State *S2,
			   EhcNode *S1ehcnode, EhcNode *S2ehcfather,
			   BfsNode *S1bfsnode, BfsNode *S2bfsfather,
			   int op );
void build_domination_CNF( State *S1, State *S2,
			   EhcNode *S1ehcnode, EhcNode *S2ehcfather,
			   BfsNode *S1bfsnode, BfsNode *S2bfsfather, 
			   int op );
void build_one_part_of_domCNF( int nr, State *S, EhcNode *ehcdad, BfsNode *bfsdad, int op );
void encode_domCNF( void );
void print_rs_clause( int nr, int i );
void print_rs_encoded_clauses( void );
void insert_rs_posnoop_hitting_set_clauses( int nr, int ft, int time, 
					    State *deststate,
					    Bool *Ft, Bool *Ut );
void next_rs_posnoop_hitting_set_step( int Uindex,
				       int nr, int ft, int time,
				       State *deststate,
				       Bool *Ft, Bool *Ut );
void insert_rs_negnoop_hitting_set_clauses( int nr, int ft, int time, 
					    State *deststate,
					    Bool *Ft, Bool *Ut );
void next_rs_negnoop_hitting_set_step( int Uindex,
				       int nr, int ft, int time,
				       State *deststate,
				       Bool *Ft, Bool *Ut );



void hash_bfs_node( BfsNode *n );
Bool bfs_state_hashed( State *S, BfsNode *father, int op );



Bool stagnates( State *dest, 
		EhcNode *ehc_source, BfsNode *bfs_source,
		int endtime );
Bool single_stagnates( State *dest, 
		       EhcNode *ehc_source, BfsNode *bfs_source,
		       int endtime, int ancestornr );



Bool rs_dp_CNF( void );
Bool rs_dp( void );
Bool rs_do_unit_props( int startidx );
void rs_update_membership_lists( void );



#endif /* _REPEATED_STATES_H */

