


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



Bool satisfies_goal( State *S, State *current_goals );
void initialize_relax( void );
void r_do_membership_lists( void );
void r_insert_dynamic_membership( int v, Bool neg, int c );
void r_remove_member( int v, Bool neg, int c );



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
Bool Uleaf_disjunction_implied_by_formula( int Utime, UftNode *n );
Bool disjunction_implied_by_initial_formula( int *dis, int num_dis, Bool *is_dis );
Bool disjunction_implied_by_S_formula( int *dis, int num_dis, Bool *is_dis );
void reset_fixpoint( void );
Bool all_goals_activated( int time ); 
void print_fixpoint_result( void );



int extract_1P( int max, Bool H_info );
void initialize_goals( int max );
void achieve_goals( int time );
void introduce_ef_PC_and_A( int time, int ef );
void pathselect( int Utime, int ef );
void introduce_path_ef_PC_and_A( int time, int ef );
void collect_H_info( int max );
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



Bool goals_likely_enough( int time );
Bool Uleaf_disjunctions_supported_by_formula( int Utime, UftNode_pointer *goalU, int num_goalU ); 
Bool is_ancestor( int ft, 
		  int gfti, int pnode, 
		  int ***pfathers, int **num_fathers, UftNode_pointer **p);
Bool disjunctions_supported_by_initial_formula( int **dis, int *num_dis, 
						Bool **is_dis, 
						double **dis_weight,
						double *peff_weight,
						int num );
Bool disjunctions_supported_by_S_formula( int **dis, int *num_dis, 
					  Bool **is_dis, 
					  double **dis_weight,
					  double *peff_weight,
					  int num );



void preselect_P_actions( int time );
Bool select_disjunctions_supported_by_formula( int Utime, UftNode_pointer *goalU, int num_goalU,
					       double requested_weight );
void collect_support_graph( int gfti, 
			    UEdgeNode **supportgraph, int *num_supportgraph,
			    int ***pfathers, int ***pfatherefs, int **num_fathers,
			    int **leafps, int *num_leafs,
			    int **probleafps, int **probleafefs, int *num_probleafs,
			    UftNode_pointer **p, int **pt );
void propagate_support_graph( int gfti, 
			      UEdgeNode **supportgraph, int *num_supportgraph,
			      UftNode_pointer **p,
			      Bool prune_maxpeffather );
void minimize_support_graph( UftNode_pointer *goalU, int num_goalU,
			     UEdgeNode **supportgraph, int *num_supportgraph,
			     int **leafs, int **leafps, int *num_leafs, Bool **is_leaf,
			     int **probleafps, int *num_probleafs,
			     UftNode_pointer **p );
Bool support_graph_supports_goal( UftNode_pointer *goalU, int num_goalU,
				  UEdgeNode **supportgraph, int *num_supportgraph,
				  int **leafs, int **leafps, int *num_leafs, Bool **is_leaf,
				  int **probleafps, int *num_probleafs,
				  UftNode_pointer **p );
void replace_support_graph_NOOPs(int *pathops, int num_path, 
				 int **rplanops, int *num_rplanops, int num_rplan,
				 UftNode_pointer *goalU, int num_goalU,
				 UEdgeNode **supportgraph, int *num_supportgraph,
				 UftNode_pointer **p );
Bool has_prob_delete_on( int ft, int op );
void get_prob_nondelete_on( int ft, int op, int *E, int *num_E );



void r_wmc_CNF( double *a, double *b );
void r_external_cachet_CNF( double *a, double *b );
void r_print_wmc_CNF( void );



#endif /* _RELAX_H */


