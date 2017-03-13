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

 * File: dis_inst_easy.h 

 * Description: modified from inst_easy.h in Metric-FF 

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
 * File: inst_easy.h
 * Description: headers for multiplying easy operators.
 *
 *
 * Author: Joerg Hoffmann 2000
 *
 *********************************************************************/ 








#ifndef _dis_INST_EASY_H
#define _dis_INST_EASY_H



void dis_build_easy_action_templates( void );



void dis_cleanup_easy_domain( void );
dis_Bool dis_identical_fact( dis_Fact *f1, dis_Fact *f2 );
void dis_handle_empty_easy_parameters( void );



void dis_encode_easy_unaries_as_types( void );
int dis_create_intersected_type( dis_TypeArray T, int num_T );
int dis_find_intersected_type( dis_TypeArray T, int num_T );



void dis_multiply_easy_effect_parameters( void );
void dis_unify_easy_inertia_conditions( int curr_inertia );
void dis_multiply_easy_non_constrained_effect_parameters( int curr_parameter );



void dis_multiply_easy_op_parameters( void );
void dis_unify_easy_inertia_preconds( int curr_inertia );
void dis_multiply_easy_non_constrained_op_parameters( int curr_parameter );



#endif /* _INST_EASY_H */
