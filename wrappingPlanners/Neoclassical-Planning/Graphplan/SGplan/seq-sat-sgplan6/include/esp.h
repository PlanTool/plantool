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

 * File: esp.h

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
/*********************************************************************
 * File: esp.h
 *
 * Description: headers for esp planning
 *                                          
 * Author: Joerg Hoffmann 1999
 *
 *********************************************************************/ 




#ifndef _ESP_H
#define _ESP_H

#include "subspace.h"

extern vState known_iga_list;

int espLocalSearch (State * start_state, State * end_state, PlanAction ** plan_actions);
int distributedLocalSearch (State * start_state, State * end_state, PlanAction ** plan_actions);
int subGoalSearch (State * start_state, State * end_state, PlanAction ** plan_actions);

void save_subgraph_sol_state();
void restore_subspace_actions();
void output_esp_solution(PlanAction ** plan_actions);
void var_source_to_dest( State *dest, State *source );
int reduce_subspace_actions(State * start_state, int Gf, int mode);

void init_h1_h2();
void random_twist_goal_order(State * end_state);
void twist_goal_order(State * end_state);
void send_plan_actions(int fd, PlanAction **plan_actions);
void ff_send_plan_actions(int fd);
void ff_recv_plan_actions(int fd);
void mff_send_plan_actions(int fd);
void mff_recv_plan_actions(int fd);
void mff_send_time(int fd, float x);
void mff_recv_time(int fd);
void send_sol_state(int fd);
void recv_sol_state(int fd);
void dis_send_sol_state(int fd);
void dis_recv_sol_state(int fd);
void recv_plan_actions(int fd, PlanAction **plan_actions);
void LPG_record_esp_sol();
void FF_record_esp_sol();
void mff_record_esp_sol();
void h2_goal_order(State * end_state);
void h1_goal_order(State *start_state, State * end_state);
void init_subspace_actions();
void printv(int *v, int s, char *str);
int subGoalSearch (State * start_state, State * end_state,
                 PlanAction ** plan_actions);
Bool contain_fact_state(State *state, int f);
int reachable_rip_a_fact(State * start_state, int Gf, int ripped, int* rsa_facts, int* rsa_actions);

#endif /* _ESP_H */
