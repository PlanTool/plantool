


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



int get_1P( State *S );
int get_1P_and_A( State *S );
int get_1P_and_H( State *S );
void get_A( State *S );
void collect_A_info( void );



Bool build_fixpoint( State *S, int *max );
Bool fluents_hopeless( int time );
void initialize_fixpoint( State *S );
void determine_artificial_fl_levels( int time );
void extend_fluent_levels( int time );
void activate_ft( int index, int time );
void activate_fl( int index, int time );
void activate_ef( int index, int time );
void apply_ef( int index, int time );
void new_fact( int index );
void new_ef( int index );
void reset_fixpoint( int max );
Bool all_goals_activated( int time );
void print_fixpoint_result( void );

void get_mneed( int fl, Bool *minusinfty, float *val );
Bool supv( float *val, int fl, int expr, float c_ );



int extract_1P( int max );
int initialize_goals( int max );
void achieve_goals( int time );
void enforce_artificial_goal( int fl, int time );
void select_op( int time, int op );
void introduce_benefits_and_enforcements( int time, int ef );
Bool assign_value( int ef, int at_time, int nr, float *val );
Bool increase_value( int ef, int at_time, int nr, float *val );
void enforce_assign( int ef, int at_time, int nr );
void enforce_increase( int ef, int at_time, int nr );
void introduce_pc_goals( int time, int ef );
void update_f_goal( int fl, int time, Comparator comp, float val );
void reset_search_info( void );
void collect_H_info( void );

int schedHeuristicPert( OpConn *schedop_conn2);
Bool scheddepend(OpConn op1,OpConn op2);

#endif /* _RELAX_H */


