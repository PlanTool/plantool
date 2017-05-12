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

 * File: dis_constraints.h

 * Authors:  Chih-wei Hsu and Benjamin W. Wah

 *

 *********************************************************************/
/*
# Copyright (C) 2006, Board of Trustees of the University of Illinois.
#
# The program is copyrighted by the University of Illinois, and should
# not be distributed without prior approval.  Commercialization of this
# product requires prior licensing from the University of Illinois.
# Commercialization includes the integration of this code in part or
# whole into a product for resale.
#
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# Author: C. W. Hsu, B. W. Wah, R. Y. Huang, and Y. X. Chen  
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
*/
#ifndef _dis_CONSTRAINTS_H
#define _dis_CONSTRAINTS_H

typedef struct _ConConn
{   
  /*
    connective: 
    weight: 100000000 means hard constraint 
    time1, time2: time values
    cond: condition of the constraint
    num: length of cond
    num1: the divider of cond for those constraints with two conditions
    fact: corresponding fact    
    fv: corresponding function
  */
  dis_Connective_Con connective;
  float weight, time1, time2;
  int *cond, num1, num, fact, fv;
} ConConn;

void dis_add_predicate(char *, dis_TypedList *);
void dis_add_function(char *);
void dis_add_initial_fv(char *);
void dis_add_constraints();
void dis_setup_grounded_constraints();
void print_dis_ConNode( dis_ConNode *plnode, int indent );
void print_dis_PrefNode( dis_PrefNode *plnode );
dis_ConNode *new_dis_ConNode( dis_Connective_Con c);
dis_PrefNode *new_dis_PrefNode( char *, char *, char *);   
int dis_check_constraints(dis_State *);
int dis_check_constraints1(dis_State *);
float dis_metric_value(dis_State *);
//void get_optimal_assignment(int *);
//void mark_unreachable_elements();
int model1000();
int model1001();
int model1002();
int model1003();
int model1004();
int model1005();
int model1006();
int model0();
void PERT1000(int, int *, float *, float *);
void PERT1004(int, int *, float *);
void PERT1005(int, int *, float *, float *);
void construct_traj();
void my_printplan(int, int *, float *);
int check_power(dis_State *, float *);
int schedule_actions(int, int *, float *, float *);
int is_applicable(dis_State *, int);
int dis_contain_fact_state(int, dis_State *);
unsigned char subgoal_inc_planning(int, int);

extern int traj_n;
extern int traj_plan[5000];
extern float traj_time[5000];
extern dis_State S0;
extern int priority_threshold, *priority;
extern float g_weight, constraint_vio;

extern int num_bottleneck_var, *bottleneck_var, *bvar_init;
extern void find_bottleneck_variables();
extern int group_id(int);

//ADD these three definition
int evaluate_combine_ff(dis_State *S);
int dis_collect_cf_H_info(dis_State *source , int h);

extern int violation_facts_num;


#endif 
