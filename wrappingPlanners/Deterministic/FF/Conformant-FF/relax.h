


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
 * File: relax.h
 * Description: headers for relaxed ADL planning
 *
 * Author: Joerg Hoffmann 2000
 *
 *********************************************************************/ 






#ifndef _RELAX_H
#define _RELAX_H



Bool LESS( int a, int b );



Bool contains_goal( State *S, State *current_goals );
void initialize_relax( void );
void r_do_membership_lists( void );
int get_1P_and_H( State *S, State *current_goals, 
		  EhcNode *ehc_father, BfsNode *bfs_father, int S_op );
int get_1P( State *S, State *current_goals,
	    EhcNode *ehc_father, BfsNode *bfs_father, int S_op );
void get_A( State *S );



Bool build_fixpoint( State *S, int *max );
void initialize_fixpoint( State *S, 
			  EhcNode *ehc_father, BfsNode *bfs_father, int S_op );
void activate_ft( int index, int time );
void activate_ef( int index, int time );
void new_fact( int index );
void new_ef( int index );
void insert_path_implications( State *S, EhcNode *ehc_father, BfsNode *bfs_father, int S_op );
Bool append_new_U_layer( int time );
Bool more_reachable_facts( int Utime, UftNode *npp, UftNode *n );
Bool has_complementary_incoming_paths( int Utime, UftNode *n );
Bool Uleaf_disjunction_implied_by_formula( int Utime, UftNode *n );
Bool disjunction_implied_by_initial_formula( int *dis, int num_dis, Bool *is_dis );
Bool disjunction_implied_by_S_formula( int *dis, int num_dis, Bool *is_dis );
void reset_fixpoint( int time );
Bool all_goals_activated( int time ); 
void print_fixpoint_result( void );



int extract_1P( int max, Bool H_info );
int initialize_goals( int max );
void achieve_goals( int time );
void introduce_ef_PC_and_A( int time, int ef );
void pathselect( int Utime, int ef );
void introduce_path_ef_PC_and_A( int time, int ef );
void select_complementary_incoming_paths( int time, UftNode *n );
void collect_H_info( void );
void reset_search_info( void );
void select_implied_incoming_paths( int time, UftNode *n );
void get_minimal_disjunction_implied_by_initial_formula( int *dis, int num_dis, Bool *is_dis,
							 int **min_dis, int *num_min_dis, int ft );
void get_minimal_disjunction_implied_by_state_formula( int *dis, int num_dis, Bool *is_dis,
						       int **min_dis, int *num_min_dis, int ft );



Bool r_dp_CNF( void );
Bool r_dp( void );
Bool r_do_unit_props( int startidx );
void print_r_clauses( void );
void print_r_clause( int i );
int r_extend_dynamic_clauses_base( State_pointer *path, int num_path, int *path_op );
Bool r_unconditional_nondet_del_on( int ft, Bool *Ft, int op );
Bool r_unconditional_nondet_add_on( int ft, Bool *Ft, int op );



#endif /* _RELAX_H */


