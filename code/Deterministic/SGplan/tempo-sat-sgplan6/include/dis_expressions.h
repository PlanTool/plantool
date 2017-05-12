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

 * File: dis_expressions.h

 * Description: modified from expressions.h in Metric-FF

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
 * File: dis_expressions.h
 * Description: headers for handling numerical expressions
 *
 * Author: Joerg Hoffmann 2001
 *
 *********************************************************************/ 














#ifndef _dis_EXPRESSIONS_H
#define _dis_EXPRESSIONS_H




dis_Bool dis_number_comparison_holds( dis_Comparator c, float l, float r );



dis_Bool dis_transform_to_LNF( void );
dis_Bool dis_is_linear_task( void );
dis_Bool dis_is_linear_expression( dis_ExpNode *n );
void dis_print_lnf_representation( void );



void dis_normalize_expressions( void );
dis_Bool dis_translate_divisions( dis_ExpNode **n );
void dis_push_multiplications_down( dis_ExpNode **n );
void dis_put_comp_into_normalized_locals( dis_Comparator comp,
				      dis_ExpNode *lh,
				      dis_ExpNode *rh );
void dis_collect_normalized_locals( dis_ExpNode *n, dis_Bool positive );



void dis_translate_subtractions( void );
dis_Bool dis_ex_fl_in_nF_of_pre_cond_eff_goal( int *fl );
void dis_introduce_minus_fluent( int fl );
void dis_replace_fl_in_nF_with_minus_fl( int fl );
void dis_set_minus_fl_initial( int fl );
void dis_introduce_minus_fl_effects( int fl );



void dis_summarize_effects( void );
dis_Bool dis_same_condition( dis_Actiondis_Effect *e, dis_Actiondis_Effect *e_ );
dis_Bool dis_same_lnfs( Lnfdis_ExpNode *l, Lnfdis_ExpNode *r );
void dis_merge_effects( dis_Actiondis_Effect *e, dis_Actiondis_Effect *e_ );
void dis_merge_lnfs( Lnfdis_ExpNode *l, Lnfdis_ExpNode *r );



void dis_encode_lfns_as_artificial_fluents( void );
dis_Bool dis_ex_non_minimal_lnf_in_pre_cond_goal_eff( void );
void dis_introduce_artificial_fluent( void );
void dis_replace_non_minimal_lnf_with_artificial_fl( void );
dis_Bool dis_is_artificial_fluent( Lnfdis_ExpNode *n );



dis_Bool dis_setup_effect_costs( void );

dis_Action* pos_lnf_action(int op);

void dis_check_assigncycles( void );
dis_Bool dis_i_influences_j( int fi, int fj );
void dis_determine_fl_relevance( void );
dis_Bool dis_i_inc_influences_j( int fi, int fj );
#endif /* _dis_EXPRESSIONS_H */
