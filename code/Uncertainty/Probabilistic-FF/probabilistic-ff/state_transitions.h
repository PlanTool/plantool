


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
 * File: state_transitions.h
 *
 * Description: headers for transition routines
 *
 * Author: Joerg Hoffmann 2000
 *
 *********************************************************************/ 






#ifndef _STATE_TRANSITIONS_H
#define _STATE_TRANSITIONS_H



void initialize_state_transitions( void );



Bool result_to_dest( State *dest, 
		     EhcNode *ehc_source, BfsNode *bfs_source,
		     int op );



void handle_inferred_literals( State *dest, 
			       EhcNode *ehc_source, BfsNode *bfs_source, 
			       int op,/* only for debugging .. */
			       int *check_pos, int num_cp,
			       int *check_neg, int num_cn,
			       Bool *dels, int endtime );
void extend_fixed_clauses_base( int low_state, int high_state );
int extend_dynamic_clauses_base( State *dest, 
				 EhcNode *ehc_source, BfsNode *bfs_source, 
				 int op,
				 Bool *Npp,
				 Bool firstpart );
void print_clauses( void );
void print_clause( int i );
void insert_posnoop_hitting_set_clauses( int ft, int time,
					 State *deststate,
					 Bool *Ft, Bool *Ut );
void next_posnoop_hitting_set_step( int Uindex,
				    int ft, int time,
				    State *deststate,
				    Bool *Ft, Bool *Ut );
void insert_negnoop_hitting_set_clauses( int ft, int time,
					 State *deststate,
					 Bool *Ft, Bool *Ut );
void next_negnoop_hitting_set_step( int Uindex,
				    int ft, int time,
				    State *deststate,
				    Bool *Ft, Bool *Ut );
void extend_fixed_clauses_base_encoding( int prev );
void extend_dynamic_clauses_base_encoding( int startclauses, int startcodes );
void print_encoded_clauses( void );



Bool dp_CNF( void );
Bool internal_minisat_CNF( void );



void wmc_CNF( double *a, double *b );
void simplify_ratio( double *a, double *b );
void synchronize_ratios( double *a1, double *b1, double *a2, double *b2 );
double get_gprob( double a, double b );
void external_cachet_CNF( double *a, double *b );
void print_wmc_CNF( void );



#endif /* _STATE_TRANSITIONS_H */
