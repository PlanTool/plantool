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

 * File: dis_inst_pre.h

 * Description: modified from inst_pre.h in Metric-FF

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
 * File: inst_pre.h
 * Description: headers for instantiating operators, preprocessing part.
 *              - transform domain into integers
 *              - inertia preprocessing:
 *                  - collect inertia info
 *                  - split initial state in special arrays
 *              - Wff normalization:
 *                  - simplification
 *                  - quantifier expansion
 *                  - dis_NOT s down
 *              - negative preconditions translation
 *              - split operators into easy and hard to instantiate ones
 *
 *              - full DNF functions, only feasible for fully instantiated 
 *                formulae
 *
 * Author: Joerg Hoffmann 2000
 *
 *********************************************************************/ 











#ifndef _dis_INST_PRE_H
#define _dis_INST_PRE_H



void dis_encode_domain_in_integers( void );
void dis_collect_all_strings( void );
int dis_position_in_types_table( char *str );
int dis_position_in_constants_table( char *str );
int dis_position_in_predicates_table( char *str );
int dis_position_in_functions_table( char *str );
void dis_create_integer_representation( void );
void dis_make_dis_Fact( dis_Fact *f, dis_PlNode *n, int num_vars );
void dis_make_dis_Fluent( dis_Fluent *f, dis_TokenList *atom, int num_vars );
dis_Bool dis_is_subtype( int t1, int t2 );
dis_WffNode *dis_make_Wff( dis_PlNode *p, int num_vars );
dis_ExpNode *dis_make_dis_ExpNode( dis_Parsedis_ExpNode *p, int num_vars );
dis_Effect *dis_make_effect( dis_PlNode *p, int num_vars );



void dis_do_inertia_preprocessing_step_1( void );
void dis_collect_inertia_information( void );
void dis_split_initial_state( void );



void dis_normalize_all_wffs( void );
void dis_remove_unused_vars_in_wff( dis_WffNode **w );
void dis_decrement_inferior_vars( int var, dis_WffNode *w );
void dis_decrement_inferior_vars_in_exp( int var, dis_ExpNode *n );
dis_Bool dis_var_used_in_wff( int code_var, dis_WffNode *w );
dis_Bool dis_var_used_in_exp( int code_var, dis_ExpNode *n );
void dis_simplify_wff( dis_WffNode **w );
void dis_simplify_exp( dis_ExpNode **n );
void dis_expand_quantifiers_in_wff( dis_WffNode **w, int var, int constant );
void dis_replace_var_with_const_in_exp( dis_ExpNode **n, int var, int constant );
dis_WffNode *dis_copy_Wff( dis_WffNode *w );
dis_ExpNode *dis_copy_Exp( dis_ExpNode *n );
dis_Bool dis_possibly_positive( dis_Fact *f );
dis_Bool dis_possibly_negative( dis_Fact *f );
dis_Bool dis_matches( dis_Fact *f1, dis_Fact *f2 );
void dis_cleanup_wff( dis_WffNode **w );
void dis_detect_tautologies_in_wff( dis_WffNode **w );
dis_Bool dis_are_identical_dis_ATOMs( dis_WffNode *w1, dis_WffNode *w2 );
void dis_merge_dis_ANDs_and_dis_ORs_in_wff( dis_WffNode **w );
void dis_NOTs_down_in_wff( dis_WffNode **w );



void dis_translate_negative_preconds( void );
dis_Bool dis_translate_one_negative_cond( dis_WffNode *w );
void dis_replace_not_p_with_n_in_wff( int p, int n, dis_WffNode **w );
void dis_add_to_initial_state( int p, int n, int index );



void dis_split_domain( void );
int dis_is_dis_dnf( dis_WffNode *w );
void  dis_make_normal_effects( Normdis_Operator **nop, dis_Operator *op );



void dis_dnf( dis_WffNode **w );
void dis_ANDs_below_dis_ORs_in_wff( dis_WffNode **w );
void dis_collect_hitting_sets( dis_WffNode *dis_ORlist, int index );
void dis_merge_next_step_dis_ANDs_and_dis_ORs_in_wff( dis_WffNode **w );



#endif /* _INST_PRE_H */
