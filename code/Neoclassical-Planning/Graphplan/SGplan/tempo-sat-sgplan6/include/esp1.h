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

 * File: esp1.h

 * Authors:  Chih-wei Hsu and Benjamin W. Wah

 *

 *********************************************************************/
#ifndef _ESP1_H
#define _ESP1_H
#define MAX_OPS 128
#define MAX_QUEUE_LEN 16
extern int num_subgoal, current_subgoal, *goal_order;
extern int *num_subplan_ops, **subplan_ops;
extern float **scheduled_time, *deadline, *end_time;
extern float **lambda, **objs;
extern int **new_backtrack_nodes, num_new;
extern int num_Bnode, num_Bnode0;
extern int **Bnodes, *len_Bnodes;
extern int esploop, expandnodes;

void esp1(State *, State *);
int myPERT(int, int, int *, float *);
float localobj(int, int *, float *);
#endif
