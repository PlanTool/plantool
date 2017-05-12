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

 * File: subspace.h

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


#ifndef _SUBSPACE_H
#define _SUBSPACE_H

#include "lpg.h"
#define VSIZE 30000

#define NIM 20
#define INOUTD 3000

#define MAX_SYMM_SIZE 100
#define MAX_SYMM 300

#define MAX_SYMM_GROUP_SIZE 100
#define MAX_SYMM_GROUP 300
#define MAX_SYMM_TYPE 10


extern PlanAction ** g_plan_actions;

typedef struct _vState {
      int F[VSIZE];  
      int num_F;
} vState;

typedef struct _SymmConn {
      int f[MAX_SYMM_SIZE];  
      int size;
      int type;
} SymmConn;

typedef struct _SymmGroup {
      int g[MAX_SYMM_GROUP_SIZE];  
      int size;
} SymmGroup;

typedef struct _gentry {
      int v[INOUTD];   // the facts i precede  
      int out_d;
      int w[INOUTD];  // the facts precede i
      int in_d;
      
      int imf[NIM];
      int n_imf;
      int ima[NIM];
      int n_ima;
      int fof[NIM];
      int n_fof;      
      
      Bool ima_nailed;
        
      int inn;
      int ima_type;
      int subgoal;
} gentry;

extern SymmConn Symmetry[MAX_SYMM];
extern SymmGroup SymmetryGroup[MAX_SYMM_GROUP];
extern Bool SymmGroupConn[MAX_SYMM_GROUP][MAX_SYMM_GROUP];
extern int gnum_symm;
extern int gnum_symm_group;
 

enum block_mode {GOAL_BLOCK, IGA_GRAPH};

gentry *iga_graph;

void clear_iga_graph();
void build_in_edges(int *fv);
int build_subspace(State * start_state,  int* rsa_facts, int* rsa_actions, State *omita);
int rip_facts_subspace(State * start_state,State *gs,int* rsa_facts,int* rsa_actions) ;
int block_fact_subspace(State * start_state, int ripped, int* rsa_facts, int* rsa_actions, State * gs, int *decr_g, enum block_mode bm);        
void pure_numerical_actions();
void build_iga_graph(State * start_state,  int* rsa_facts, int* rsa_actions, 
                int *decr_g);
void modify_ft_ef_mutex();
//int block_mtxop_subspace(State * start_state, int ripped, int* rsa_facts, int* rsa_actions, int *decr_fact);
int max_out_iga_graph(State * start_state,  int* rsa_facts, int* rsa_actions,int *decr_g);
void precede_facts_of_state(State *s, vState *pf);
void zin_facts(State* start_state, vState *vf, vState *in_f);
void mxtop_subspace(State * start_state,  int* rsa_facts, int* rsa_actions, int *decr_g);
void ima_analysis(State * start_state, vState *needf, int power_level, int *rsa_facts, int *rsa_actions, int *dg);
void omita_analysis(State * start_state, int *rsa_facts, int *rsa_actions, int *decr_g);
void print_iga_graph(vState *needf);
Bool v_contain_fact_state(vState *state, int f);
Bool add_fs(State *v, int x);
void print_ft(int f);
void print_op(int a);

Bool same_action(int a, int b);
Bool in_array(int *arr, int size, int x);
void mff_bridge(State *S, State *T);
void dis_mff_bridge();
void genIMA_analysis(State * start_state, int *rsa_facts, int *rsa_actions,int *decr_g);
void build_symmetry(State * start_state, int *rsa_facts, int *rsa_actions ,int *decr_g);
int find_symm_group(int f);
int symm_goal_eval(State *dS, int goal);
Bool connected_connect(int fa, int go);

void IMA_tif_wrapper(State *start_state, State *end_state);
                            
#endif /* _SUBSPACE_H */
