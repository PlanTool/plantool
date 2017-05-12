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

 * File: dis_inst_hard.h 

 * Description: modified from inst_hard.h in Metric-FF

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
 * File: inst_hard.h
 * Description: headers for multiplying hard operators.
 *
 *
 * Author: Joerg Hoffmann 2000
 *
 *********************************************************************/ 








#ifndef _dis_INST_HARD_H
#define _dis_INST_HARD_H



void dis_build_hard_action_templates( void );



void dis_cleanup_hard_domain( void );
dis_Bool dis_var_used_in_literals( int code_var, dis_Literal *ef );
dis_Bool dis_var_used_in_numeric_effects( int code_var, dis_Numericdis_Effect *ef );
void dis_decrement_inferior_vars_in_literals( int var, dis_Literal *ef );
void dis_decrement_inferior_vars_in_numeric_effects( int var, dis_Numericdis_Effect *ef );



void dis_multiply_hard_op_parameters( void );
void dis_create_hard_mixed_operators( dis_Operator *o, int curr_var );
dis_Effect *dis_instantiate_dis_Effect( dis_Effect *e );
dis_WffNode *dis_instantiate_wff( dis_WffNode *w );
void dis_instantiate_exp( dis_ExpNode **n );
dis_Bool dis_full_dis_possibly_positive( dis_Fact *f );
dis_Bool dis_full_dis_possibly_negative( dis_Fact *f );
int instantiated_dis_fact_adress( dis_Fact *f );



void dis_multiply_hard_effect_parameters( void );
void dis_create_hard_pseudo_effects( dis_Pseudodis_Action *a, dis_Effect *e, int curr_var );
void dis_make_instantiate_literals( dis_Pseudodis_Actiondis_Effect *e, dis_Literal *ll );
void dis_make_instantiate_numeric_effects( dis_Pseudodis_Actiondis_Effect *e, dis_Numericdis_Effect *ne );



#endif /* _INST_HARD_H */
