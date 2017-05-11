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

 * File: dis_relax.h

 * Description: modified from relax.h in Metric-FF

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
 * File: dis_relax.h
 * Description: headers for relaxed ADL planning
 *
 * Author: Joerg Hoffmann 2000
 *
 *********************************************************************/ 






#ifndef _dis_RELAX_H
#define _dis_RELAX_H



dis_Bool dis_LESS( int a, int b );



int dis_get_1P( dis_State *S );
int dis_get_1P_and_A( dis_State *S );
int dis_get_1P_and_H( dis_State *S );
// Ruoyun
int CG_get_1P_and_A( dis_State *S );
int CG_get_1P_and_H( dis_State *S );
void dis_get_A( dis_State *S );
void dis_collect_A_info( void );



dis_Bool dis_build_fixpoint( dis_State *S, int *max );
dis_Bool dis_fluents_hopeless( int time );
void dis_initialize_fixpoint( dis_State *S );
void dis_determine_artificial_fl_levels( int time );
void dis_extend_fluent_levels( int time );
void dis_activate_ft( int index, int time );
void dis_activate_fl( int index, int time );
void dis_activate_ef( int index, int time );
void dis_apply_ef( int index, int time );
void dis_new_fact( int index );
void dis_new_ef( int index );
void dis_reset_fixpoint( int max );
dis_Bool dis_all_goals_activated( int time );
void dis_print_fixpoint_result( void );



int dis_extract_1P( int max );
int dis_initialize_goals( int max );
void dis_achieve_goals( int time );
void dis_enforce_artificial_goal( int fl, int time );
void dis_select_op( int time, int op );
void dis_introduce_benefits_and_enforcements( int time, int ef );
dis_Bool dis_assign_value( int ef, int at_time, int nr, float *val );
dis_Bool dis_increase_value( int ef, int at_time, int nr, float *val );
void dis_enforce_assign( int ef, int at_time, int nr );
void dis_enforce_increase( int ef, int at_time, int nr );
void dis_introduce_pc_goals( int time, int ef );
void dis_update_f_goal( int fl, int time, dis_Comparator comp, float val );
void dis_reset_search_info( void );
void dis_collect_H_info( void );


void dis_get_mneed( int fl, dis_Bool *minusinfty, float *val );
dis_Bool dis_supv( float *val, int fl, int expr, float c_ );

#endif /* _RELAX_H */


