/*********************************************************************

 * (C) Copyright 2008, University of Illinois, Urbana-Champaign

 *

 * All rights reserved. Use of this software is permitted ONLY for

 * non-commercial research purposes, and it may be copied only

 * for that use only. All copies must include this copyright message.

 * This software is made available AS IS, and neither the authors

 * nor the University of Illinois, make any warranty about the

 * software or its performance.

 *

 *********************************************************************/
/********************************************************************

 * File: dis_search.h 

 * Description: modified from search.h in Metric-FF

 *

 * Authors:  Chih-wei Hsu and Benjamin W. Wah

 *

 *********************************************************************/
/*
# Copyright (C) 2004, Board of Trustees of the University of Illinois.
#
# The program is copyrighted by the University of Illinois, and should
# not be distributed without prior approval.  Commercialization of this 
# product requires prior licensing from the University of Illinois.
# Commercialization includes the integration of this code in part or 
# whole into a product for resale. 
#
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# Author: Y. X. Chen, C. W. Hsu, and B. W. Wah, University of Illinois
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
*/



/*
 * THIS SOURCE CODE IS SUPPLIED  ``AS IS'' WITHOUT WARRANTY OF ANY KIND, 
 * dis_AND ITS AUTHdis_OR dis_AND THE JOURNAL OF ARTIFICIAL INTELLIGENCE RESEARCH 
 * (JAIR) dis_AND JAIR'S PUBLISHERS dis_AND DISTRIBUTdis_ORS, DISCLAIM ANY dis_AND dis_ALL 
 * WARRANTIES, INCLUDING BUT dis_NOT LIMITED TO ANY IMPLIED
 * WARRANTIES OF MERCHANTABILITY dis_AND FITNESS Fdis_OR A PARTICULAR PURPOSE, dis_AND
 * ANY WARRANTIES dis_OR NON INFRINGEMENT.  THE USER ASSUMES dis_ALL LIABILITY dis_AND
 * RESPONSIBILITY Fdis_OR USE OF THIS SOURCE CODE, dis_AND NEITHER THE AUTHdis_OR Ndis_OR
 * JAIR, Ndis_OR JAIR'S PUBLISHERS dis_AND DISTRIBUTdis_ORS, WILL BE LIABLE Fdis_OR 
 * DAMAGES OF ANY KIND RESULTING FROM ITS USE.  Without limiting the 
 * generality of the foregoing, neither the author, nor JAIR, nor JAIR's
 * publishers and distributors, warrant that the Source Code will be 
 * error-free, will operate without interruption, or will meet the needs 
 * of the user.
 */










/*********************************************************************
 *
 * File: dis_search.h
 *
 * Description: headers of routines that search the state space
 *
 *              ADL version, Enforced Hill-climbing enhanced with
 *                           Goal-adders deletion heuristic
 *
 * Author: Joerg Hoffmann 2000
 *
 *********************************************************************/ 






#ifndef _dis_SEARCH_H
#define _dis_SEARCH_H



dis_Bool dis_do_enforced_hill_climbing( void );



dis_Bool dis_search_for_better_state( dis_State *S, int h, dis_State *S_, int *h_ );
void dis_add_to_ehc_space( dis_State *S, int op, dis_EhcNode *father );
int dis_expand_first_node( int h );



void dis_hash_ehc_node( dis_EhcNode *n );
dis_Bool dis_ehc_state_hashed( dis_State *S );
dis_Bool superior_dis_ehc_state_hashed( dis_State *S );
dis_Bool dis_superior_state( dis_State *S1, dis_State *S2 );
void dis_reset_ehc_hash_entrys( void );



void dis_extract_plan_fragment( dis_State *S );
dis_PlanHashEntry *dis_hash_plan_state( dis_State *S, int step );
dis_PlanHashEntry *dis_plan_state_hashed( dis_State *S );
dis_Bool dis_same_state( dis_State *S1, dis_State *S2 );



dis_Bool dis_do_best_first_search( void );
void dis_add_to_bfs_space( dis_State *S, int op, dis_BfsNode *father );
float dis_state_cost( dis_State *S, dis_BfsNode *father );
void dis_extract_plan( dis_BfsNode *last );



void dis_hash_bfs_node( dis_BfsNode *n );
dis_Bool dis_bfs_state_hashed( dis_State *S );
dis_Bool dis_bfs_state_hashed_0( dis_State *S, float );
dis_Bool superior_dis_bfs_state_hashed( dis_State *S );
int dis_state_sum( dis_State *S );



dis_Bool dis_result_to_dest( dis_State *dest, dis_State *source, int op, int parent_op);
dis_Bool dis_result_to_dest0( dis_State *dest, dis_State *source, int op, int parent_op);
dis_Bool dis_determine_source_val( dis_State *source, int fl, float *val );
void dis_copy_dis_source_to_dest( dis_State *dest, dis_State *source );
void dis_source_to_dest( dis_State *dest, dis_State *source );
void always_true_DPop();
void dis_set_DPft_flag();

void dis_MFF_main( char* ops_file, char *fct_file );
#endif /* _SEARCH_H */
