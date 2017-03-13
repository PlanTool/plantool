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

 * File: dis_inst_final.h

 * Description: modified from inst_final.h in Metric-FF 

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
 * File: inst_final.h
 * Description: headers for final domain representation functions
 *
 *
 * Author: Joerg Hoffmann 2000
 *
 *********************************************************************/ 







#ifndef _dis_INST_FINAL_H
#define _dis_INST_FINAL_H



void dis_perform_reachability_analysis( void );
int dis_fact_adress( void );
void dis_make_name_inst_table_from_Normdis_Operator( dis_Action *a, Normdis_Operator *o, dis_EasyTemplate *t );
void dis_make_name_inst_table_from_dis_Pseudodis_Action( dis_Action *a, dis_Pseudodis_Action *pa );



void dis_collect_relevant_facts_and_fluents( void );
void dis_create_final_goal_state( void );
dis_Bool dis_set_relevants_in_wff( dis_WffNode **w );
dis_Bool dis_set_relevants_in_exp( dis_ExpNode **n );
void dis_create_final_initial_state( void );
void dis_create_final_actions( void );
void dis_instantiate_exp_by_action( dis_ExpNode **n, dis_Action *a );



void dis_build_connectivity_graph( void );



void dis_summarize_effects( void );
dis_Bool dis_same_condition( dis_Actiondis_Effect *e, dis_Actiondis_Effect *e_ );
dis_Bool dis_same_lnfs( Lnfdis_ExpNode *l, Lnfdis_ExpNode *r );
void dis_merge_effects( dis_Actiondis_Effect *e, dis_Actiondis_Effect *e_ );

int dis_tok_ft_argncmp(int f, int n, char *tok);
int dis_tok_ef_argncmp(int f, int n, char *tok);
int dis_ft_argncmp(int f, int g, int n, int m);
int dis_op_argncmp(int f, int g, int n, int m);
int dis_ft_arity(int f);
int dis_ef_arity(int f);
int dis_ef_predlen(int f);
int dis_ft_predlen(int f);
int dis_fl_predlen(int f);
char* dis_ft_argn(int f, int n);
char* dis_ef_argn(int f, int n);


#endif /* _INST_FINAL_H */
