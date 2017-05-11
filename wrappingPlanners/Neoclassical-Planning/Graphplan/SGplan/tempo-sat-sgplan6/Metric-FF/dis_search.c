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

 * File: dis_search.c 

 * Description: modified from search.c in Metric-FF

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
 *
 * File: search.c
 *
 * Description: implementation of routines that search the state space
 *
 *              PDDL level 2 version.
 *
 *              here: basic best-first search, using helpful actions
 *
 *
 * Author: Joerg Hoffmann 2001
 *
 *********************************************************************/ 









#include "dis_ff.h"

#include "dis_output.h"
#include "dis_memory.h"

#include "dis_expressions.h"
#include "dis_constraints.h"
#include "dis_relax.h"
#include "dis_search.h"
#include "lpg.h"
#include "output.h"
#include "stripsff.h"
#include "esp1.h"
#include <unistd.h>
typedef struct _QueueElement
{
	dis_EhcNode *ptr;
	int h;
	float obj;
} QueueElement;
QueueElement queue[MAX_QUEUE_LEN];
int queue_len = 0, level1_size;
extern int myPERT(int, int, int *, float *);
extern int current_rover;

extern int Nop, dis_ops[1000];
extern float schedule[1000], makespan, violation, max_time, total_time;
int counter = 0;

float *relax_time_fact, *relax_time_action;
int *support_action, *last_precondition;


/*****************
 * LOCAL GLOBALS *
 *****************/


int dis_ginfo=0;








/* search space for EHC
 */
dis_EhcNode *lehc_space_head, *lehc_space_end, *lehc_current_start, *lehc_current_end;



/* memory (hash table) for states that are already members
 * of the breadth - first search space in EHC
 */
dis_EhcHashEntry_pointer lehc_hash_entry[dis_EHC_HASH_SIZE];
int lnum_ehc_hash_entry[dis_EHC_HASH_SIZE];
int lchanged_ehc_entrys[dis_EHC_HASH_SIZE];
int lnum_changed_ehc_entrys;
dis_Bool lchanged_ehc_entry[dis_EHC_HASH_SIZE];



/* memory (hash table) for states that are already 
 * encountered by current serial plan
 */
dis_PlanHashEntry_pointer lplan_hash_entry[dis_PLAN_HASH_SIZE];



/* search space
 */
dis_BfsNode *lbfs_space_head, *lbfs_space_had;



/* memory (hash table) for states that are already members
 * of the best first search space
 */
dis_BfsHashEntry_pointer lbfs_hash_entry[dis_BFS_HASH_SIZE];










/********************************
 * EHC FUNCTION, Cdis_ALLED BY MAIN *
 ********************************/





dis_Bool lH;

extern int symm_constraint_eval(dis_State *dS, int goal);
extern void pro_print_state(dis_State *dS);
extern void print_op(int o);
extern void print_op_name(int o);
extern Bool connected_connect(int fa, int go);
extern int short_msof_ops(int o);


void dis_back_space_reduction(dis_State *B, int* rsa_facts, int* rsa_actions,
        dis_State *S, dis_State *newS, dis_State *start_state)
{                       
    int i,j, k, f, a, ii;
    int level;
    
    int *rsa_fluents = (int *) malloc(sizeof(int)*dis_gnum_fl_conn);
//  int *rsa_facts = (int *) malloc(sizeof(int)*dis_gnum_ft_conn);
//  int *rsa_effects = (int *) malloc(sizeof(int)*dis_gnum_ef_conn);
    
    for (i = 0; i < dis_gnum_ft_conn; i++) rsa_facts[i] = -1;
    for (i=0;i<dis_gnum_fl_conn;i++)
      rsa_fluents[i] = -1;
    S->num_F = 0;
/*    for(i=0; i<start_state->num_F; i++) {
        rsa_facts[start_state->F[i]] = 0;
    }
    for (i=0;i<dis_gnum_fl_conn;i++)
      if (start_state->f_D[i])
        rsa_fluents[i] = 0;*/
    for(i=0; i<B->num_F; i++) {
    //    if(rsa_facts[B->F[i]] < 0){
            rsa_facts[B->F[i]] = 0;
            S->F[S->num_F++] = B->F[i];
      //  }
    }
    for (i=0;i<dis_gnum_fl_conn;i++)
      S->f_D[i] = B->f_D[i];
    for (i = 0; i < dis_gnum_ef_conn; i++) {
        //rsa_actions[i] = -1;
        dis_gef_conn[i].red_level = -1;
    }
    
    level = 0;
    while(1)
    {
        i = 0;
        for (ii=0;ii<dis_gnum_fl_conn;ii++)
          if (S->f_D[ii])
            i++;
        if(S->num_F == 0 && i == 0) 
        break;
        newS->num_F = 0;
        for (i=0;i<dis_gnum_fl_conn;i++)
          newS->f_D[i] = 0;
        for (i=0;i<dis_gnum_fl_conn;i++)
        {
          if (!S->f_D[i])
            continue;
          for (j=0;j<dis_gfl_conn[i].num_AS;j++)
          {
            a = dis_gfl_conn[i].AS[j];
            if (dis_gef_conn[a].red_level >= 0)
              continue;
            dis_gef_conn[a].red_level = level;
            
                    for (k = 0; k < dis_gef_conn[a].num_PC; k++) {		
                        f = dis_gef_conn[a].PC[k];
                        if(f>=0) {
                            if(rsa_facts[f] < 0) {
                                rsa_facts[f] = level+1;
                                newS->F[newS->num_F++] = f;
                            }     
                        }
                    }       
                    
                    for (k=0;k<dis_gef_conn[a].num_f_PC;k++)
                    {
                      f = dis_gef_conn[a].f_PC_fl[k];
                      if (f >= 0)
                        if (rsa_fluents[f] < 0)
                        {
                          rsa_fluents[f] = level+1;
                          newS->f_D[f] = 1;
                        }
                    }
          }

          for (j=0;j<dis_gfl_conn[i].num_IN;j++)
          {
            if (dis_gfl_conn[i].IN_c[j] <= 0 && dis_gfl_conn[i].IN_fl_[j] == -1)
              continue;
            a = dis_gfl_conn[i].IN[j];
            if (dis_gef_conn[a].red_level >= 0)
              continue;
            dis_gef_conn[a].red_level = level;
            
                    for (k = 0; k < dis_gef_conn[a].num_PC; k++) {		
                        f = dis_gef_conn[a].PC[k];
                        if(f>=0) {
                            if(rsa_facts[f] < 0) {
                                rsa_facts[f] = level+1;
                                newS->F[newS->num_F++] = f;
                            }     
                        }
                    }       
                    
                    for (k=0;k<dis_gef_conn[a].num_f_PC;k++)
                    {
                      f = dis_gef_conn[a].f_PC_fl[k];
                      if (f >= 0)
                        if (rsa_fluents[f] < 0)
                        {
                          rsa_fluents[f] = level+1;
                          newS->f_D[f] = 1;
                        }
                    }
          }
        }
        for (ii = 0; ii < S->num_F; ii++) {
            i = S->F[ii];
        
            //printf("level %d", level); print_ft(i);
            for (j = 0; j < dis_gft_conn[i].num_A; j++) {
                a = dis_gft_conn[i].A[j];
                //if(rsa_actions[a]>=0) continue;
                if(dis_gef_conn[a].red_level>=0) continue;
                
                    if(!dis_gef_conn[a].DPop) {    
                    //    printf("level %d", level); print_op(a);
                    }
                    //rsa_actions[a] = level;
                    dis_gef_conn[a].red_level = level;
                        
                    for (k = 0; k < dis_gef_conn[a].num_PC; k++) {
                        f = dis_gef_conn[a].PC[k];
                        if(f>=0) {
                            if(rsa_facts[f] < 0) {
                                rsa_facts[f] = level+1;
                                newS->F[newS->num_F++] = f;
                            }     
                        }
                    }       
                    
                    for (k=0;k<dis_gef_conn[a].num_f_PC;k++)
                    {
                      f = dis_gef_conn[a].f_PC_fl[k];
                      if (f >= 0)
                        if (rsa_fluents[f] < 0)
                        {
                          rsa_fluents[f] = level+1;
                          newS->f_D[f] = 1;
                        }
                    }
            }           
        }     
        
        level++;
//        S->num_F = newS->num_F;
//        for(j=0; j<newS->num_F; j++) S->F[j] = newS->F[j];
        dis_source_to_dest(S, newS);
    }
/*    for (i=0;i<dis_gnum_ef_conn;i++)
    if (dis_gef_conn[i].red_level < 0)
    {
      dis_print_op_name(dis_gef_conn[i].op);
      printf("\n");
    }*/
    
  xfree(rsa_fluents);
//  xfree(rsa_facts);
//  xfree(rsa_effects);
  
}
 

/*void dis_back_space_reduction(dis_State *B, int* rsa_facts, int* rsa_actions,
        dis_State *S, dis_State *newS, dis_State *start_state)
{                       
    int i,j, k, f, a, ii;
    int level;
    
    for (i = 0; i < dis_gnum_ft_conn; i++) rsa_facts[i] = -1;
    S->num_F = 0;
    for(i=0; i<start_state->num_F; i++) {
        rsa_facts[start_state->F[i]] = 0;
    }
    for(i=0; i<B->num_F; i++) {
    //    if(rsa_facts[B->F[i]] < 0){
            rsa_facts[B->F[i]] = 0;
            S->F[S->num_F++] = B->F[i];
      //  }
    }
    for (i = 0; i < dis_gnum_ef_conn; i++) {
        //rsa_actions[i] = -1;
        dis_gef_conn[i].red_level = -1;
    }
    
    level = 0;
    while(1)
    {
        newS->num_F = 0;
       
        if(S->num_F == 0) break;
        for (ii = 0; ii < S->num_F; ii++) {
            i = S->F[ii];
        
            //printf("level %d", level); print_ft(i);
            for (j = 0; j < dis_gft_conn[i].num_A; j++) {
                a = dis_gft_conn[i].A[j];
                //if(rsa_actions[a]>=0) continue;
                if(dis_gef_conn[a].red_level>=0) continue;
                
                    if(!dis_gef_conn[a].DPop) {    
                    //    printf("level %d", level); print_op(a);
                    }
                    //rsa_actions[a] = level;
                    dis_gef_conn[a].red_level = level;
                        
                    for (k = 0; k < dis_gef_conn[a].num_PC; k++) {
                        f = dis_gef_conn[a].PC[k];
                        if(f>=0) {
                            if(rsa_facts[f] < 0) {
                                rsa_facts[f] = level+1;
                                newS->F[newS->num_F++] = f;
                            }     
                        }
                    }       
            }           
        }     
        
        level++;
        S->num_F = newS->num_F;
        for(j=0; j<newS->num_F; j++) S->F[j] = newS->F[j];
    }
}*/

void dis_goal_to_state(dis_State *S)
{
    int j;
    dis_make_state(S, dis_gnum_ft_conn, dis_gnum_fl_conn);
    S->num_F = 0;
    for(j=0; j<dis_gnum_flogic_goal; j++) {
        S->F[S->num_F++] = dis_gflogic_goal[j];
    }
    for (j=0;j<dis_gnum_fnumeric_goal;j++)
      S->f_D[dis_gfnumeric_goal_fl[j]] = 1;
}
      
int num_no_goal_state(dis_State *S)
{
    int i,j,f,n=0;
    for(j=0; j<dis_gnum_flogic_goal; j++) {
        f = dis_gflogic_goal[j];
        for(i=0; i<S->num_F; i++) {
            if(S->F[i] == f) break;
        }
        if(i==S->num_F) n++;
    }
    return n;
}

void dis_set_DPft_flag()
{
    int i,a;
    
    for(i=0; i<dis_gnum_ft_conn; i++) {
      // print_ft_name(i);printf(" ");
        dis_gft_conn[i].DPft = dis_FALSE;
        if(dis_gft_conn[i].num_A <= 0) continue;
        a = dis_gft_conn[i].A[0];
      //print_op_name(a);printf(" ");
        if(dis_gef_conn[a].DPop) dis_gft_conn[i].DPft = dis_TRUE;
      //if(dis_gft_conn[i].DPft) printf("DP\n"); else printf("not DP\n");
    }  
}

void always_true_DPop()
{
    int i,j,f;
    dis_make_state(&dis_gtrue_ft, dis_gnum_ft_conn, dis_gnum_fl_conn);
    dis_gtrue_ft.num_F = 0;
    for(i=0; i<dis_gnum_ef_conn; i++) {
        if(!dis_gef_conn[i].DPop) continue;
        if(dis_gef_conn[i].num_PC == 0 && dis_gef_conn[i].num_f_PC == 0) { 
     //      printf("always true"); print_op(i);
            for(j=0; j<dis_gef_conn[i].num_A; j++) {
                f = dis_gef_conn[i].A[j];
       //         printf("always true"); print_ft(f);
                dis_gtrue_ft.F[dis_gtrue_ft.num_F++] = f;
            }
        }
    }                           
}


void DP_fixpoint_state(dis_State *B, int* rsa_facts, int* rsa_actions,
        dis_State *S, dis_State *newS)
{                       
    int i,j, k, f, a, ii;
    int level;
            
    for(i=0; i<dis_gnum_ef_conn; i++) {
        dis_gef_conn[i].num_pcc = dis_gef_conn[i].num_PC;
    }
                     
    for (i = 0; i < dis_gnum_ft_conn; i++) rsa_facts[i] = -1;
    S->num_F = 0;
    for(i=0; i<B->num_F; i++) {
        if(dis_gft_conn[B->F[i]].DPft) continue;
        rsa_facts[B->F[i]] = 0;
        S->F[S->num_F++] = B->F[i];
    }
    // always true facts    
    for(i=0; i<dis_gtrue_ft.num_F; i++) {
        f = dis_gtrue_ft.F[i];
        for(j=0; j<S->num_F; j++) {
            if(S->F[j] == f) break;
        }
        if(j >= S->num_F) {
            rsa_facts[f] = 0;
            S->F[S->num_F++] = f;
        }
    }

    for (i = 0; i < dis_gnum_ef_conn; i++) rsa_actions[i] = -1;
    
    level = 0;
  
    while(1)
    {
        newS->num_F = 0;
       
        if(S->num_F == 0) break;
        for (ii = 0; ii < S->num_F; ii++) {
            i = S->F[ii];
        
            //printf("level %d", level); dis_print_ft_name(i); printf("\n");
            for (j = 0; j < dis_gft_conn[i].num_PC; j++) {
                a = dis_gft_conn[i].PC[j];
                if(rsa_actions[a]>=0) continue;
                if(!dis_gef_conn[a].DPop) continue;
                
                
                dis_gef_conn[a].num_pcc--;
                if(dis_gef_conn[a].num_pcc == 0) {
                            
            //        printf("\tinvoke DP"); dis_print_op_name(dis_gef_conn[a].op);
            //        printf("\n");
                    rsa_actions[a] = level;
                    for (k = 0; k < dis_gef_conn[a].num_A; k++) {
                        f = dis_gef_conn[a].A[k];
                        if(f>=0) {
                            if(rsa_facts[f] < 0) {
                                rsa_facts[f] = level+1;
                                newS->F[newS->num_F++] = f;
                            }     
                        }
                    }       
                }                   
            }           
        }     
        
        level++;
        
        S->num_F = newS->num_F;
        for(j=0; j<newS->num_F; j++) S->F[j] = newS->F[j];
    }
    
    B->num_F  = 0;
    for(i=0; i<dis_gnum_ft_conn; i++) {
        if(rsa_facts[i] < 0) continue;
        B->F[B->num_F++] = i;
    }
   
    // add always true facts
    // wrong implementation: shouldapply before
    /* 
    for(i=0; i<dis_gtrue_ft.num_F; i++) {
        f = dis_gtrue_ft.F[i];
        for(j=0; j<B->num_F; j++) {
            if(B->F[j] == f) break;
        }
        if(j >= B->num_F) {
            B->F[B->num_F++] = f;
        }
    } */  
}






dis_Bool dis_do_enforced_hill_climbing( void )

{

  dis_State S, S_, gS;
  int i, h, h_;
  
  dis_make_state( &S, dis_gnum_ft_conn, dis_gnum_fl_conn );
  dis_make_state( &S_, dis_gnum_ft_conn, dis_gnum_fl_conn );

// Chih-Wei
//  sleep(30);

/*	if (GpG.SearchModal == 3 && GpG.is_til)
		for (dis_gnum_plan_ops=0;dis_gnum_plan_ops<num_subplan_ops[current_subgoal];dis_gnum_plan_ops++)
		{
			dis_gplan_ops[dis_gnum_plan_ops] = subplan_ops[current_subgoal][dis_gnum_plan_ops];
			dis_result_to_dest(&S, &dis_ginitial_state, dis_gplan_ops[dis_gnum_plan_ops], -1);
			dis_source_to_dest(&dis_ginitial_state, &S);
		}*/

  //Y. Chen
  //printf("dis_known_iga_list\n");
  //dis_print_dis_State(dis_known_iga_list);  

 // Y. Chen 
  if(GpG.is_deripred) {
   DP_fixpoint_state(&dis_ginitial_state, work_ft, work_op, &work_S0, &work_S1); }
  //printf("init state size %d\n", dis_ginitial_state.num_F);

  if(dis_red_space == 1) 
  {
     dis_goal_to_state(&gS);  
     dis_back_space_reduction(&gS, work_ft, work_op, &work_S0, &work_S1, &dis_ginitial_state);
  }
   
  /* initialize plan hash table, search space, search hash table
   */
  for ( i = 0; i < dis_PLAN_HASH_SIZE; i++ ) {
    lplan_hash_entry[i] = NULL;
  }
  dis_hash_plan_state( &dis_ginitial_state, 0 );
  
  lehc_space_head = dis_new_dis_EhcNode();
  lehc_space_end = lehc_space_head;
  
  for ( i = 0; i < dis_EHC_HASH_SIZE; i++ ) {
    lehc_hash_entry[i] = NULL;
    lnum_ehc_hash_entry[i] = 0;
    lchanged_ehc_entry[i] = dis_FALSE;
  }
  lnum_changed_ehc_entrys = 0;
  
  
  /* start enforced Hill-climbing
   */
  dis_source_to_dest( &S, &dis_ginitial_state );
  
 if(SymmLagrangian == 1) {
    h = dis_get_1P_and_A( &S );
 } else {
    lH = dis_TRUE;
    h = dis_get_1P_and_H( &S );
 }
 
	// Chih-Wei
/*	if (GpG.SearchModal == 3 && GpG.is_til)
		if (myPERT(dis_gnum_plan_ops, 0, dis_gplan_ops, scheduled_time[current_subgoal]) != -1)
			h = dis_INFINITY;*/

  if ( h == dis_INFINITY ) {
    return dis_FALSE;
  }
  if ( h == 0 ) {
    dis_source_to_dest(&dis_mff_sol, &S);
    return dis_TRUE;
  }  
//     printf("\n\nCueing down from goal distance: %4d into depth ", h);
//     fflush(stdout);
    
    // Y. Chen
  if(SymmLagrangian==1) {
      while ( h != 0 ) {
            dis_get_1P_and_A( &S );
            lH = dis_FALSE;
            if(hc_max_exceeded) return dis_FALSE;
            if ( !dis_search_for_better_state( &S, h, &S_, &h_ ) ) {
	            return dis_FALSE;
            }
            dis_source_to_dest( &S, &S_ );
            h = h_;
       //     printf("\n                                %4d            ", h); 
    //printf("."); fflush(stdout);
            //pro_print_state(&S);
      }
  }  
  else {  
 //original 
  while ( h != 0 ) {
    if ( !dis_search_for_better_state( &S, h, &S_, &h_ ) ) {
//      printf(" --- pruning stopped --- ");
      dis_get_1P_and_A( &S );
      lH = dis_FALSE;
      
      // Y. Chen
      if(hc_max_exceeded) return dis_FALSE;
      
      if ( !dis_search_for_better_state( &S, h, &S_, &h_ ) ) {
	return dis_FALSE;
      }
      lH = dis_TRUE;
      dis_get_1P_and_H( &S_ );/* to set up dis_gH info for new start state */
    }
    dis_source_to_dest( &S, &S_ );
    h = h_;
//      printf("\n                                %4d            ", h);
//      fflush(stdout);
    //printf("."); fflush(stdout);
   }
  }

  // printf("gnum_op = %d\n", dis_gnum_plan_ops);
  
  //Y. Chen 
  dis_source_to_dest(&dis_mff_sol, &S);
  return dis_TRUE;

}













/*************************************************
 * FUNCTIONS Fdis_OR BREADTH FIRST SEARCH IN H SPACE *
 *************************************************/















dis_Bool dis_search_for_better_state( dis_State *S, int h, dis_State *S_, int *h_ )

{
	// Chih-Wei
	float obj;
	dis_EhcNode *p;

  static dis_Bool fc = dis_TRUE;
  static dis_State S__;

  int no_progress_iter=0;
  int i, j, h__, depth = 0;
  dis_EhcNode *tmp;

  hc_max_exceeded = dis_FALSE;
  
  if ( fc ) {
    dis_make_state( &S__, dis_gnum_ft_conn, dis_gnum_fl_conn );
    fc = dis_FALSE;
  }

  /* don't hash states, but search nodes.
   * this way, don't need to keep states twice in memory
   */
  tmp = dis_new_dis_EhcNode();
  dis_source_to_dest( &(tmp->S), S);
  dis_hash_ehc_node( tmp );

  lehc_current_end = lehc_space_head->next;
	// Chih-Wei  
	level1_size = 0;
	queue_len = 0;
			
	if (esploop && 0)
	{
	
/*  if ( lH ) {
    for ( i = 0; i < dis_gnum_H; i++ ) {
	if (dis_gef_conn[dis_gop_conn[dis_gH[i]].E[0]].num_A > 0 || !GpG.is_til)
      if ( dis_result_to_dest( &S__, S, dis_gH[i], -1 ) ) {
	dis_add_to_ehc_space( &S__, dis_gH[i], NULL );
	level1_size++;
      }
    }
  } else {
    for ( i = 0; i < dis_gnum_A; i++ ) {
	if (dis_gef_conn[dis_gop_conn[dis_gA[i]].E[0]].num_A > 0 || !GpG.is_til)
      if ( dis_result_to_dest( &S__, S, dis_gA[i], -1 ) ) {
	dis_add_to_ehc_space( &S__, dis_gA[i], NULL );
	level1_size++;
      }
    }

  }

  lehc_current_start = lehc_space_head->next;

  while ( dis_TRUE ) {  
    if ( lehc_current_start == lehc_current_end ) {
      dis_reset_ehc_hash_entrys();
      free( tmp );
      return dis_FALSE;
    }
   
    h__ = dis_expand_first_node( h );
	if (level1_size-- > 0)
	{
		if (h__ < h)
		{
			memcpy(subplan_ops[current_subgoal], dis_gplan_ops, sizeof(int)*dis_gnum_plan_ops);
			for (i=dis_gnum_plan_ops,p=lehc_current_start;p;p=p->father,i++)
				subplan_ops[current_subgoal][dis_gnum_plan_ops+p->depth-1] = p->op;
			myPERT(i, dis_gnum_plan_ops, subplan_ops[current_subgoal], scheduled_time[current_subgoal]);
			obj = localobj(i - dis_gnum_plan_ops, &subplan_ops[current_subgoal][dis_gnum_plan_ops], &scheduled_time[current_subgoal][dis_gnum_plan_ops]);
			for (i=0;i<queue_len;i++)
				if (obj < queue[i].obj)
					break;
			queue_len = queue_len < MAX_QUEUE_LEN ? queue_len + 1: MAX_QUEUE_LEN;
			for (j=queue_len-1;j>i;j--)
				queue[j] = queue[j-1];
			queue[i].ptr = lehc_current_start;	
			queue[i].h = h__; 
			queue[i].obj = obj;
			lehc_current_start = lehc_current_start->next;
		}
		if (level1_size == 0 && queue_len > 0)
		{
			lehc_current_start = queue[0].ptr;
			h__ = queue[0].h;
			break;
		}
	}
	else
    if ( dis_LESS( h__, h ) ) {
      break;
    }    

    no_progress_iter++;
    if(no_progress_iter>max_hc_iter) {
    //    printf("\n\nmax_hc_iter exceeded\n\n");
        hc_max_exceeded = dis_TRUE;
        return dis_FALSE; 
    }  
  }*/ 
	}	
	else
	{    
  if ( lH ) {
    for ( i = 0; i < dis_gnum_H; i++ ) {
//    if (dis_gef_conn[dis_gop_conn[dis_gH[i]].E[0]].num_A > 0 || !GpG.is_til)
      if ( dis_result_to_dest( &S__, S, dis_gH[i], -1 ) ) {
	dis_add_to_ehc_space( &S__, dis_gH[i], NULL );
      }
    }
  } else {
    for ( i = 0; i < dis_gnum_A; i++ ) {
//      if (dis_gef_conn[dis_gop_conn[dis_gA[i]].E[0]].num_A > 0 || !GpG.is_til)
      if ( dis_result_to_dest( &S__, S, dis_gA[i], -1 ) ) {
	dis_add_to_ehc_space( &S__, dis_gA[i], NULL );
      }
    }
  }

  lehc_current_start = lehc_space_head->next;

  while ( dis_TRUE ) {  
    if ( lehc_current_start == lehc_current_end ) {
      dis_reset_ehc_hash_entrys();
      xfree( tmp );
      return dis_FALSE;
    }
   
    if ( lehc_current_start->depth > depth ) {
      depth = lehc_current_start->depth;
      if ( depth > dis_gmax_search_depth ) {
	dis_gmax_search_depth = depth;
      }
    //  if ( dis_gcmd_line.display_info ) {
      
    //printf("[%d, %d]", depth, no_progress_iter);fflush( stdout );
//    printf("[%d]", depth);
//    fflush(stdout);
    //  }
    }
    h__ = dis_expand_first_node( h );
    if ( dis_LESS( h__, h ) ) {
      break;
    }    
    
    no_progress_iter++;
    if(no_progress_iter>max_hc_iter) {
//        fprintf(stderr, "max_hc_iter exceeded\n");
        hc_max_exceeded = dis_TRUE;
        return dis_FALSE; 
    }  
  } 
  }

  dis_reset_ehc_hash_entrys();
  xfree( tmp );
	
  dis_extract_plan_fragment( S );

  dis_source_to_dest( S_, &(lehc_current_start->S) );
  *h_ = h__;

  return dis_TRUE;

}



void dis_add_to_ehc_space( dis_State *S, int op, dis_EhcNode *father )

{
  float dummy;

  /* see if superior state (in terms of goal reachability)
   * is already a part of this search space
   */
  if ( superior_dis_ehc_state_hashed( S ) ) {
    return;
  }

// Chih-Wei PDDL3
  if (GpG.SearchModal == -1005 && GpG.is_durative)
    if (!check_power(S, &dummy))
      return;

  if ( !lehc_current_end ) {
    lehc_current_end = dis_new_dis_EhcNode();
    lehc_space_end->next = lehc_current_end;
    lehc_space_end = lehc_current_end;
  }

  dis_source_to_dest( &(lehc_current_end->S), S );
  lehc_current_end->op = op;
  lehc_current_end->father = father;
  if ( !father ) {
    lehc_current_end->depth = 1;
  } else {
    lehc_current_end->depth = father->depth + 1;
  }

  dis_hash_ehc_node( lehc_current_end );

  lehc_current_end = lehc_current_end->next;

}



int dis_expand_first_node( int h )

{

  static dis_Bool fc = dis_TRUE;
  static dis_State S_;

  int h_ = dis_INFINITY, i;

  if ( fc ) {
    dis_make_state( &S_, dis_gnum_ft_conn, dis_gnum_fl_conn );
    fc = dis_FALSE;
  }

  if ( lH ) {
    h_ = dis_get_1P_and_H( &(lehc_current_start->S) );
  } else {
    h_ = dis_get_1P_and_A( &(lehc_current_start->S) );
  }   

	// Chih-Wei
/*	if (GpG.SearchModal == 3 && GpG.is_til)
	{
		// check deadline feasibility
		memcpy(subplan_ops[current_subgoal], dis_gplan_ops, sizeof(int)*dis_gnum_plan_ops);
		for (i=0,ehc=lehc_current_start;ehc;ehc=ehc->father,i++)
			subplan_ops[current_subgoal][dis_gnum_plan_ops+ehc->depth-1] = ehc->op;
		if (myPERT(i + dis_gnum_plan_ops, dis_gnum_plan_ops, subplan_ops[current_subgoal], scheduled_time[current_subgoal]) != -1)
			h_ = dis_INFINITY;
	}*/

  if ( h_ == dis_INFINITY ) {
    lehc_current_start = lehc_current_start->next;
    return h_;
  }

  if ( h_ < h ) {
    return h_;
  }
    
	if (0&&esploop && queue_len == 0)
	{

/*		for (i=0;i<dis_gnum_ef_conn;i++)
			if (dis_gef_conn[i].num_A > 0)
				if (dis_result_to_dest(&S_, &(lehc_current_start->S), 
				dis_gef_conn[i].op, lehc_current_start->op))
				{
					dis_add_to_ehc_space(&S_, dis_gef_conn[i].op, lehc_current_start);
					level1_size++;
				}*/

/*  if ( lH ) {
    for ( i = 0; i < dis_gnum_H; i++ ) {
	if (dis_gef_conn[dis_gop_conn[dis_gH[i]].E[0]].num_A > 0 || !GpG.is_til)
      if ( dis_result_to_dest( &S_, &(lehc_current_start->S), dis_gH[i],
                           lehc_current_start->op  ) ) {
	dis_add_to_ehc_space( &S_, dis_gH[i], lehc_current_start );
	level1_size++;
      }
    }
  } else {
    for ( i = 0; i < dis_gnum_A; i++ ) {
	if (dis_gef_conn[dis_gop_conn[dis_gA[i]].E[0]].num_A > 0 || !GpG.is_til)
      if ( dis_result_to_dest( &S_, &(lehc_current_start->S), dis_gA[i],
                            lehc_current_start->op ) ) {
	dis_add_to_ehc_space( &S_, dis_gA[i], lehc_current_start );
	level1_size++;
      }
    }
  }*/

	}
	else
	{

  if ( lH ) {
    for ( i = 0; i < dis_gnum_H; i++ ) {
      if (dis_gef_conn[dis_gop_conn[dis_gH[i]].E[0]].num_A > 0 || !GpG.is_til)
      if ( dis_result_to_dest( &S_, &(lehc_current_start->S), dis_gH[i],
                           lehc_current_start->op  ) ) {
	dis_add_to_ehc_space( &S_, dis_gH[i], lehc_current_start );
      }
    }
  } else {
    for ( i = 0; i < dis_gnum_A; i++ ) {
      if (dis_gef_conn[dis_gop_conn[dis_gA[i]].E[0]].num_A > 0 || !GpG.is_til)
      if ( dis_result_to_dest( &S_, &(lehc_current_start->S), dis_gA[i],
                            lehc_current_start->op ) ) {
	dis_add_to_ehc_space( &S_, dis_gA[i], lehc_current_start );
      }
    }
  }
    
	}

  lehc_current_start = lehc_current_start->next;

  return h_;

}














/********************************************************
 * HASHING ALGdis_ORITHM Fdis_OR RECOGNIZING REPEATED STATES IN *
 * EHC BREADTH FIRST SEARCH                             *
 ********************************************************/














void dis_hash_ehc_node( dis_EhcNode *n )

{

  int i, sum, index;
  dis_EhcHashEntry *h, *prev = NULL;

  sum = dis_state_sum( &(n->S) );
  index = sum & dis_EHC_HASH_BITS;

  h = lehc_hash_entry[index];
  if ( !h ) {
    h = dis_new_dis_EhcHashEntry();
    h->sum = sum;
    h->ehc_node = n;
    lehc_hash_entry[index] = h;
    lnum_ehc_hash_entry[index]++;
    if ( !lchanged_ehc_entry[index] ) {
      lchanged_ehc_entrys[lnum_changed_ehc_entrys++] = index;
      lchanged_ehc_entry[index] = dis_TRUE;
    }
    return;
  }
  i = 0;
  while ( h ) {
    if ( i == lnum_ehc_hash_entry[index] ) {
      break;
    }
    i++;
    prev = h;
    h = h->next;
  }

  if ( h ) {
    /* current list end is still in allocated list of hash entrys
     */
    h->sum = sum;
    h->ehc_node = n;
    lnum_ehc_hash_entry[index]++;
    if ( !lchanged_ehc_entry[index] ) {
      lchanged_ehc_entrys[lnum_changed_ehc_entrys++] = index;
      lchanged_ehc_entry[index] = dis_TRUE;
    }
    return;
  }
  /* allocated list ended; connect a new hash entry to it.
   */
  h = dis_new_dis_EhcHashEntry();
  h->sum = sum;
  h->ehc_node = n;
  prev->next = h;
  lnum_ehc_hash_entry[index]++;
  if ( !lchanged_ehc_entry[index] ) {
    lchanged_ehc_entrys[lnum_changed_ehc_entrys++] = index;
    lchanged_ehc_entry[index] = dis_TRUE;
  }
  return;
      
}



dis_Bool dis_ehc_state_hashed( dis_State *S )

{

  int i, sum, index;
  dis_EhcHashEntry *h;

  sum = dis_state_sum( S );
  index = sum & dis_EHC_HASH_BITS;

  h = lehc_hash_entry[index];
  for ( i = 0; i < lnum_ehc_hash_entry[index]; i++ ) {
    if ( h->sum != sum ) {
      h = h->next;
      continue;
    }
    if ( dis_same_state( &(h->ehc_node->S), S ) ) {
      return dis_TRUE;
    }
    h = h->next;
  }

  return dis_FALSE;

}



dis_Bool superior_dis_ehc_state_hashed( dis_State *S )

{

  int i, sum, index;
  dis_EhcHashEntry *h;

  sum = dis_state_sum( S );
  index = sum & dis_EHC_HASH_BITS;

  h = lehc_hash_entry[index];
  for ( i = 0; i < lnum_ehc_hash_entry[index]; i++ ) {
    if ( h->sum < sum ) {
      h = h->next;
      continue;
    }
    if ( dis_superior_state( &(h->ehc_node->S), S ) ) {
      return dis_TRUE;
    }
    h = h->next;
  }

  return dis_FALSE;

}



dis_Bool dis_superior_state( dis_State *S1, dis_State *S2 ) 

{

  int i, j;

  if ( !dis_gconditional_effects ) {
  for ( i = 0; i < S2->num_F; i++ ) {
    for ( j = 0; j < S1->num_F; j++ ) {
      if ( S1->F[j] == S2->F[i] ) {
	break;
      }
    }
    if ( j == S1->num_F ) {
      return dis_FALSE;
    }
  }

    /* check whether the fluent values are superior.
     * see JAIR article for explanation / justification
     */
    for ( i = 0; i < dis_gnum_real_fl_conn; i++ ) {
      if ( !dis_gfl_conn[i].relevant ) {
        continue;
      }
       
      if ( !S2->f_D[i] ) {
        continue;
      }
       
      if ( !S1->f_D[i] ||
           S2->f_V[i] > S1->f_V[i] ) {
        return FALSE;
      }
    }  
  } else {
    if ( S2->num_F != S1->num_F ) {
      return FALSE;
    }
    for ( i = 0; i < S2->num_F; i++ ) {
      for ( j = 0; j < S1->num_F; j++ ) {
        if ( S1->F[j] == S2->F[i] ) {
          break;
        }
      }  
      if ( j == S1->num_F ) {
        return FALSE;
      }
    }  
    for ( i = 0; i < dis_gnum_real_fl_conn; i++ ) {
      if ( !dis_gfl_conn[i].relevant ) {
        continue;
      }      
      if ( S2->f_D[i] != S1->f_D[i]  ) {
        return FALSE;
      }
      if ( S2->f_V[i] != S1->f_V[i] ) {
        return FALSE;
      }
    }
  }

  return dis_TRUE;

}



void dis_reset_ehc_hash_entrys( void )

{

  int i;

  for ( i = 0; i < lnum_changed_ehc_entrys; i++ ) {
    lnum_ehc_hash_entry[lchanged_ehc_entrys[i]] = 0;
    lchanged_ehc_entry[lchanged_ehc_entrys[i]] = dis_FALSE;
  }
  lnum_changed_ehc_entrys = 0;

}













/***************************************************
 * FUNCTIONS Fdis_OR UPDATING THE CURRENT SERIAL PLAN, *
 * BASED ON SEARCH SPACE INFdis_ORMATION .             *
 *                                                 *
 * EMPLOY SOMEWHAT TEDIOUS HASHING PROCEDURE TO    *
 * AVOID REPEATED STATES IN THE PLAN               *
 ***************************************************/














void dis_extract_plan_fragment( dis_State *S )

{

  dis_EhcNode *i;
  int ops[dis_MAX_PLAN_LENGTH], num_ops;
  dis_State_pointer states[dis_MAX_PLAN_LENGTH];
  int j;
  dis_PlanHashEntry *start = NULL, *i_ph;

  num_ops = 0;
  for ( i = lehc_current_start; i; i = i->father ) {
    if ( (start = dis_plan_state_hashed( &(i->S) )) != NULL ) {
      for ( i_ph = start->next_step; i_ph; i_ph = i_ph->next_step ) {
	i_ph->step = -1;
      }
      dis_gnum_plan_ops = start->step;
      break;
    }
    if ( num_ops == dis_MAX_PLAN_LENGTH ) {
      printf("\nincrease dis_MAX_PLAN_LENGTH! currently %d\n\n",
	     dis_MAX_PLAN_LENGTH);
      exit( 1 );
    }
    states[num_ops] = &(i->S);
    ops[num_ops++] = i->op;
  }
  if ( !start ) {
    start = dis_plan_state_hashed( S );
    if ( !start ) {
      printf("\n\ncurrent start state not hashed! debug me!\n\n");
      exit( 1 );
    }
    if ( start->step == -1 ) {
      printf("\n\ncurrent start state marked removed from plan! debug me!\n\n");
      exit( 1 );
    }
  }

  for ( j = num_ops - 1; j > -1; j-- ) {
    if ( dis_gnum_plan_ops == dis_MAX_PLAN_LENGTH ) {
      printf("\nincrease dis_MAX_PLAN_LENGTH! currently %d\n\n",
	     dis_MAX_PLAN_LENGTH);
      exit( 1 );
    }
    start->next_step = dis_hash_plan_state( states[j], dis_gnum_plan_ops + 1 );
    start = start->next_step;
    dis_copy_dis_source_to_dest( &(dis_gplan_states[dis_gnum_plan_ops+1]), states[j] );
    dis_gplan_ops[dis_gnum_plan_ops++] = ops[j];
  }

	// Chih-Wei
	if (GpG.SearchModal == 3 && GpG.is_til)
		myPERT(dis_gnum_plan_ops, dis_gnum_plan_ops - num_ops, dis_gplan_ops, scheduled_time[current_subgoal]);
}




dis_PlanHashEntry *dis_hash_plan_state( dis_State *S, int step )

{

  int sum, index;
  dis_PlanHashEntry *h, *tmp;

  sum = dis_state_sum( S );
  index = sum & dis_PLAN_HASH_BITS;

  for ( h = lplan_hash_entry[index]; h; h = h->next ) {
    if ( h->sum != sum ) continue;
    if ( dis_same_state( S, &(h->S) ) ) break;
  }

  if ( h ) {
    if ( h->step != -1 ) {
      printf("\n\nreencountering a state that is already in plan! debug me\n\n");
      exit( 1 );
    }
    h->step = step;
    return h;
  }

  for ( h = lplan_hash_entry[index]; h && h->next; h = h->next );

  tmp = dis_new_dis_PlanHashEntry();
  tmp->sum = sum;
  dis_copy_dis_source_to_dest( &(tmp->S), S );
  tmp->step = step;

  if ( h ) {
    h->next = tmp;
  } else {
    lplan_hash_entry[index] = tmp;
  }

  return tmp;

}
  

 
dis_PlanHashEntry *dis_plan_state_hashed( dis_State *S )

{

  int sum, index;
  dis_PlanHashEntry *h;

  sum = dis_state_sum( S );
  index = sum & dis_PLAN_HASH_BITS;

  for ( h = lplan_hash_entry[index]; h; h = h->next ) {
    if ( h->sum != sum ) continue;
    if ( dis_same_state( S, &(h->S) ) ) break;
  }

  if ( h && h->step != -1 ) {
    return h;
  }

  return NULL;

}



dis_Bool dis_same_state( dis_State *S1, dis_State *S2 ) 

{

  int i, j;

  if ( S1->num_F != S2->num_F ) {
    return dis_FALSE;
  }

  for ( i = 0; i < S2->num_F; i++ ) {
    for ( j = 0; j < S1->num_F; j++ ) {
      if ( S1->F[j] == S2->F[i] ) {
	break;
      }
    }
    if ( j == S1->num_F ) {
      return dis_FALSE;
    }
  }

  for ( i = 0; i < dis_gnum_fl_conn; i++ ) {
    if ( S2->f_D[i] != S1->f_D[i] ||
	 (S2->f_D[i] && (S2->f_V[i] != S1->f_V[i])) ) {
      return dis_FALSE;
    }
  }

  return dis_TRUE;

}
















/************************************
 * BEST FIRST SEARCH IMPLEMENTATION *
 ************************************/














dis_Bool dis_do_best_first_search( void )

{

  dis_BfsNode *first;
  dis_State S;
  dis_State gS;
  int i, min = dis_INFINITY;
  dis_Bool start = dis_TRUE;

// Chih-Wei
//  sleep(30);
  counter = 0;
//  relax_time_fact = (float *) malloc(sizeof(float)*dis_gnum_ft_conn);   //no free
//  relax_time_action = (float *) malloc(sizeof(float)*dis_gnum_ef_conn); //no free
//  support_action = (int *) malloc(sizeof(int)*dis_gnum_ft_conn);        //no free
//  last_precondition = (int *) malloc(sizeof(int)*dis_gnum_ef_conn);     //no free
          
  dis_make_state( &S, dis_gnum_ft_conn, dis_gnum_fl_conn );
  
  // Y. Chen             
  //printf("best with gw %d hw %d\n", dis_gcmd_line.g_weight,  dis_gcmd_line.h_weight );
  //printf("init state size %d\n", dis_ginitial_state.num_F);
  if(GpG.is_deripred) {
  DP_fixpoint_state(&dis_ginitial_state, work_ft, work_op, &work_S0, &work_S1); } 
  //printf("init state size %d\n", dis_ginitial_state.num_F);
  
  // Y. Chen
  if(dis_red_space == 1) {
    dis_goal_to_state(&gS);  
    dis_back_space_reduction(&gS, work_ft, work_op, &work_S0, &work_S1, &dis_ginitial_state);
  }                                 
 

  lbfs_space_head = dis_new_dis_BfsNode();
  lbfs_space_had = NULL;

  for ( i = 0; i < dis_BFS_HASH_SIZE; i++ ) {
    lbfs_hash_entry[i] = NULL;
  }

  dis_add_to_bfs_space( &dis_ginitial_state, -1, NULL );

  while ( dis_TRUE ) {
    if ( (first = lbfs_space_head->next) == NULL ) {
      if ( dis_gcmd_line.display_info ) {
//	printf("\n\nbest first search space empty! problem proven unsolvable.\n\n");
      }
      return dis_FALSE;
    }

    lbfs_space_head->next = first->next;
    if ( first->next ) {
      first->next->prev = lbfs_space_head;
    }
    
    // Y. Chen
//    fprintf(stderr, "g = %d h = %d\n", first->g, first->h);
   
     // NOPRINT
// Chih-Wei
    if(counter>max_bfs_iter) {
//        fprintf(stderr, "\n\nmax_bfs_iter exceeded\n\n");
//        break;
        return dis_FALSE; 
    }     
   
    
    if ( dis_LESS( first->h, min ) ) {
      min = first->h;
      //printf("."); fflush(stdout);
     
      if ( start ) {
    	if ( dis_gcmd_line.display_info ) {
	   //   printf("\n\nadvancing to distance: %4d", min);
	        fflush(stdout);
	
        }
	    start = dis_FALSE;
      } else {
	    if ( dis_gcmd_line.display_info ) {
     // NOPRINT
	    //    printf("\n                       %4d", min);
            fflush(stdout);
	    }
      }
    
    }


    if ( first->h == 0 && first->int_fn < dis_INFINITY) {
      break;
    }

    for ( i = 0; i < first->num_H; i++ ) {
      if (1||dis_gef_conn[dis_gop_conn[first->H[i]].E[0]].num_A > 0 || !GpG.is_til)
      {
      if ( dis_result_to_dest( &S, &(first->S), first->H[i], first->op ) ) {
	/* we must include a check here whether the numerical part of the action
	 * is entirely fulfilled; only those actions are applied.
	 */

	dis_add_to_bfs_space( &S, first->H[i], first );
      }
      else {
        //printf("pruned");print_op_name( first->H[i]);printf("\n");
      }
      }
    }

    first->next = lbfs_space_had;
    lbfs_space_had = first;
  }

  dis_extract_plan( first );
  return dis_TRUE;

}



void dis_add_to_bfs_space( dis_State *S, int op, dis_BfsNode *father )

{

  dis_BfsNode *new, *i;
  int j, h, intg, int_fn = 0;
  float cost = 0, floatg = 0, float_fn = 0;

  if ( dis_gcmd_line.optimize && dis_goptimization_established ) {
    if ( dis_bfs_state_hashed( S ) ) {
      return;
    }
  } else {
    if ( superior_dis_bfs_state_hashed( S ) ) {
      return;
    }
  }

  if (GpG.SearchModal == -1005 && GpG.is_durative)
    if (!check_power(S, &floatg))
      return;

  Nop = 0;
// Chih-Wei PDDL3
  if (GpG.is_til || (GpG.SearchModal == -103 && GpG.is_durative) || (GpG.SearchModal == -1002 && !GpG.is_durative) || num_dur == -1)
  {
  if (father)
  {  
    dis_extract_plan(father);
    memcpy(dis_ops + Nop, dis_gplan_ops, dis_gnum_plan_ops*sizeof(int));
    Nop += dis_gnum_plan_ops;
  }  
  if (op != -1)
    dis_ops[Nop++] = op;
  }
  h = dis_get_1P_and_A( S );
  counter++;
                                          
  if ( dis_gcmd_line.optimize && dis_goptimization_established ) {
    cost = dis_gcost;
    /* dis_gtt is mulitplicator of TOTAL-dis_TIME in final metric; if no
     * total-time part in metric, it is 0
     */
    cost += h * dis_gtt;
  }

  if ( h == dis_INFINITY ) {
    return;
  }
  
  if ( father ) {
    intg = father->g + 1;
  } else {
    intg = 0;
  }

// Chih-Wei
  if ( (dis_gcmd_line.optimize && dis_goptimization_established) || num_dur == -1 || (GpG.SearchModal == -1002 && !GpG.is_durative)
    || (GpG.SearchModal == -1005 && GpG.is_durative) || GpG.is_til) 
  {
    if (GpG.SearchModal == -1005 && GpG.is_durative)
    {
      float_fn = g_weight*floatg + h;
//      fprintf(stderr, "%f %f %d\n", float_fn, floatg, h);
    }
    else
    if (num_dur == -1 || (GpG.SearchModal == -1002 && !GpG.is_durative) || GpG.is_til)
    {
      floatg = makespan;
      float_fn = g_weight*makespan + h;
    }
    else
    {
    floatg = dis_state_cost( S, father );
    float_fn = (((float) dis_gcmd_line.g_weight) * floatg) + (((float) dis_gcmd_line.h_weight) * cost);
    }
    for ( i = lbfs_space_head; i->next; i = i->next ) {
      if ( i->next->float_fn >= float_fn ) break;
    }
  } else {
    int_fn = (dis_gcmd_line.g_weight * intg) + (dis_gcmd_line.h_weight * h);
    if (GpG.SearchModal == -103)
    {
      if (h)
      float_fn = 100.0*makespan + 100.0*h;
      else
        float_fn = 0;
      int_fn = float_fn;
    }

    for ( i = lbfs_space_head; i->next; i = i->next ) {
      if ( i->next->int_fn >= int_fn) break;
    }
  }

  new = dis_new_dis_BfsNode();
  dis_copy_dis_source_to_dest( &(new->S), S );
  new->op = op;
  new->h = h;
//  if ( dis_gcmd_line.optimize && dis_goptimization_established ) {
    new->float_fn = float_fn;
//  } else {
    new->int_fn = int_fn;
//  }
  new->father = father;
  new->g = intg;

  new->H = ( int * ) calloc( dis_gnum_A, sizeof( int ) );
  for ( j = 0; j < dis_gnum_A; j++ ) {
    new->H[j] = dis_gA[j];
  }
  new->num_H = dis_gnum_A;

  new->next = i->next;
  new->prev = i;
  i->next = new;
  if ( new->next ) {
    new->next->prev = new;
  }
  
//  printf("g = %d h = %d op %d op_r_l %d\n", new->g, new->h, op, dis_gef_conn[op].red_level);
                                    
  dis_hash_bfs_node( new );

}



float dis_state_cost( dis_State *S, dis_BfsNode *father )

{

  float cost = 0;
  int i;

  for ( i = 0; i < dis_glnf_metric.num_pF; i++ ) {
    if ( dis_glnf_metric.pF[i] == -2 ) {
      /* cost is number of steps from I to S 
       */ 
      if ( father ) {
	cost += dis_gtt * (father->g + 1);
      }/* no father, no steps, no cost */
    } else {
      cost += (dis_glnf_metric.pC[i] * 
	       (S->f_V[dis_glnf_metric.pF[i]] - dis_ginitial_state.f_V[dis_glnf_metric.pF[i]]));
    }
  }

  return cost;

}



void dis_extract_plan( dis_BfsNode *last )

{

  dis_BfsNode *i;
  int ops[dis_MAX_PLAN_LENGTH], num_ops;
  int j;
    
  dis_source_to_dest(&dis_mff_sol, &(last->S));
  
  num_ops = 0;
  for ( i = last; i->op != -1; i = i->father ) {
    if ( num_ops == dis_MAX_PLAN_LENGTH ) {
      printf("\nincrease dis_MAX_PLAN_LENGTH! currently %d\n\n",
	     dis_MAX_PLAN_LENGTH);
      exit( 1 );
    }
    ops[num_ops++] = i->op;
  }

  dis_gnum_plan_ops = 0;
  for ( j = num_ops - 1; j > -1; j-- ) {
    dis_gplan_ops[dis_gnum_plan_ops++] = ops[j];
  }

}
















/************************************************************
 * HASHING ALGdis_ORITHM Fdis_OR RECOGNIZING REPEATED STATES IN BFS *
 ************************************************************/












void dis_hash_bfs_node( dis_BfsNode *n )

{

  int sum, index;
  dis_BfsHashEntry *h, *tmp;

  sum = dis_state_sum( &(n->S) );
  index = sum & dis_BFS_HASH_BITS;

  h = lbfs_hash_entry[index];
  if ( !h ) {
    h = dis_new_dis_BfsHashEntry();
    h->sum = sum;
    h->bfs_node = n;
    lbfs_hash_entry[index] = h;
    return;
  }
  for ( ; h->next; h = h->next );

  tmp = dis_new_dis_BfsHashEntry();
  tmp->sum = sum;
  tmp->bfs_node = n;
  h->next = tmp;
      
}



dis_Bool dis_bfs_state_hashed( dis_State *S )

{

  int sum, index;
  dis_BfsHashEntry *h;

  sum = dis_state_sum( S );
  index = sum & dis_BFS_HASH_BITS;

  h = lbfs_hash_entry[index];
  for ( h = lbfs_hash_entry[index]; h; h = h->next ) {
    if ( h->sum != sum ) {
      continue;
    }
    if ( dis_same_state( &(h->bfs_node->S), S ) ) {
      return dis_TRUE;
    }
  }

  return dis_FALSE;

}



dis_Bool superior_dis_bfs_state_hashed( dis_State *S )

{

  int sum, index;
  dis_BfsHashEntry *h;

  sum = dis_state_sum( S );
  index = sum & dis_BFS_HASH_BITS;

  h = lbfs_hash_entry[index];
  for ( h = lbfs_hash_entry[index]; h; h = h->next ) {
    if ( h->sum < sum ) {
      continue;
    }
    if ( dis_superior_state( &(h->bfs_node->S), S ) ) {
      return dis_TRUE;
    }
  }

  return dis_FALSE;

}



int dis_state_sum( dis_State *S )

{

  int i, sum = 0;

  for ( i = 0; i < S->num_F; i++ ) {
    sum += dis_gft_conn[S->F[i]].rand;
  }

  for ( i = 0; i < dis_gnum_real_fl_conn; i++ ) {
    if ( !dis_gfl_conn[i].relevant ) {
      continue;
    }
    if ( !S->f_D[i] ) {
      continue;
    }
    sum += dis_gfl_conn[i].rand * ( int ) S->f_V[i];
  }

  return sum;

}















/****************************
 * STATE Hdis_ANDLING FUNCTIONS *
 ****************************/



















/* state transition function; here, takes in an action whose
 * logical and numerical preconds are fulfilled, and returns dis_TRUE,
 * putting the result into *dest, iff the action has at least one
 * appearing effect and is legal, i.e. if
 * no illegal numeric effects occur.
 */
dis_Bool dis_result_to_dest( dis_State *dest, dis_State *source, int op, 
        int parent_op)
{

  static dis_Bool first_call = dis_TRUE;
  static dis_Bool *in_source, *in_dest, *in_del, *true_ef, *assigned;
  static int *del, num_del;

  int i, j, ef, fl;
  float val, source_val;
  dis_Comparator comp;

  dis_Bool one_appeared = dis_FALSE;

  if(GpG.is_deripred && op == -1) 
  {
    dis_source_to_dest(dest, source);
    DP_fixpoint_state(dest, work_ft, work_op, &work_S0, &work_S1);
    return dis_TRUE;
  }

  if (GpG.MFF_parser)
    if (priority[op] < priority_threshold)
      return dis_FALSE;
 
  if(dis_gef_conn[dis_gop_conn[op].E[0]].DPop) {
      return dis_FALSE;
  }
  if(dis_red_space) {
    if(dis_gef_conn[dis_gop_conn[op].E[0]].red_level<0) return dis_FALSE;  
  } 
 
  if(SymmLagrangian==1){
      //print_op(parent_op);
      if(!connected_connect(parent_op, op)) {
  //        printf("cutting"); print_op_name(parent_op);printf(" ");
    //     print_op_name(op); printf("\n"); 
          return dis_FALSE;         
      }
  }
 
  if ( first_call ) {
    in_source = ( dis_Bool * ) calloc( dis_gnum_ft_conn, sizeof( dis_Bool ) );
    in_dest = ( dis_Bool * ) calloc( dis_gnum_ft_conn, sizeof( dis_Bool ) );
    in_del = ( dis_Bool * ) calloc( dis_gnum_ft_conn, sizeof( dis_Bool ) );
    true_ef = ( dis_Bool * ) calloc( dis_gnum_ef_conn+5, sizeof( dis_Bool ) );
    assigned = ( dis_Bool * ) calloc( dis_gnum_fl_conn, sizeof( dis_Bool ) );
    del = ( int * ) calloc( dis_gnum_ft_conn, sizeof( int ) );
    for ( i = 0; i < dis_gnum_ft_conn; i++ ) {
      in_source[i] = dis_FALSE;
      in_dest[i] = dis_FALSE;
      in_del[i] = dis_FALSE;
    }
    for ( i = 0; i < dis_gnum_ef_conn; i++ ) {
      true_ef[i] = dis_FALSE;
    }
    for ( i = 0; i < dis_gnum_fl_conn; i++ ) {
      assigned[i] = dis_FALSE;
    }
    first_call = dis_FALSE;
  }

  /* setup true facts for effect cond evaluation
   */
  for ( i = 0; i < source->num_F; i++ ) {
    in_source[source->F[i]] = dis_TRUE;
  }

  /* evaluate effect conditions and setup deleted facts
   */
  num_del = 0;
  for ( i = 0; i < dis_gop_conn[op].num_E; i++ ) {
    ef = dis_gop_conn[op].E[i];
    /* logic cond true?
     */
    for ( j = 0; j < dis_gef_conn[ef].num_PC; j++ ) {
      if ( !in_source[dis_gef_conn[ef].PC[j]] ) break;
    }
    if ( j < dis_gef_conn[ef].num_PC ) continue;
    /* numeric cond true?
     */
    for ( j = 0; j < dis_gef_conn[ef].num_f_PC; j++ ) {
      fl = dis_gef_conn[ef].f_PC_fl[j];
      val = dis_gef_conn[ef].f_PC_c[j];
      comp = dis_gef_conn[ef].f_PC_comp[j];
      if ( !dis_determine_source_val( source, fl, &source_val ) ) {
	/* condition access to an undefined fluent!
	 */
	for ( i = 0; i < num_del; i++ ) {
	  in_del[del[i]] = dis_FALSE;
	}
	for ( i = 0; i < dis_gop_conn[op].num_E; i++ ) {
	  true_ef[i] = dis_FALSE;
	}
	for ( i = 0; i < source->num_F; i++ ) {
	  in_source[source->F[i]] = dis_FALSE;
	}
	return dis_FALSE;
      }
      if ( !dis_number_comparison_holds( comp, source_val, val ) ) break;
    }
    if ( j < dis_gef_conn[ef].num_f_PC ) continue;

    if ( dis_gef_conn[ef].illegal ) {
      /* effect always affects an undefined fluent, as we found out
       * earlier
       */
      for ( i = 0; i < source->num_F; i++ ) {
	in_source[source->F[i]] = dis_FALSE;
      }
      for ( i = 0; i < num_del; i++ ) {
	in_del[del[i]] = dis_FALSE;
      }
      for ( i = 0; i < dis_gop_conn[op].num_E; i++ ) {
	true_ef[i] = dis_FALSE;
      }
      return dis_FALSE;
    }
    true_ef[i] = dis_TRUE;
    one_appeared = dis_TRUE; 
    for ( j = 0; j < dis_gef_conn[ef].num_D; j++ ) {
      if ( in_del[dis_gef_conn[ef].D[j]] ) continue;
      in_del[dis_gef_conn[ef].D[j]] = dis_TRUE;
      del[num_del++] = dis_gef_conn[ef].D[j];
    }
  }
  if ( !one_appeared ) {
    /* no effect appeared which means that the action is either useless
     * here or its preconds are even not fulfilled (the latter
     * shouldn't happen by dis_get_A but well....)
     */
    for ( i = 0; i < source->num_F; i++ ) {
      in_source[source->F[i]] = dis_FALSE;
    }
    for ( i = 0; i < num_del; i++ ) {
      in_del[del[i]] = dis_FALSE;
    }
    for ( i = 0; i < dis_gop_conn[op].num_E; i++ ) {
      true_ef[i] = dis_FALSE;
    }
    return dis_FALSE;
  }

  /* first, see about the numeric effects - those might render
   * the op illegal here. start by copying numeric info.
   */
  for ( i = 0; i < dis_gnum_fl_conn; i++ ) {
    dest->f_D[i] = source->f_D[i];
    dest->f_V[i] = source->f_V[i];
  }

  /* illegal is an op if the result is not well-defined,
   * or if it affects an undefined fluent.
   */
  for ( i = 0; i < dis_gop_conn[op].num_E; i++ ) {
    if ( !true_ef[i] ) continue;
    ef = dis_gop_conn[op].E[i];
    for ( j = 0; j < dis_gef_conn[ef].num_AS; j++ ) {
      fl = dis_gef_conn[ef].AS_fl[j];
      if ( dis_gef_conn[ef].AS_fl_[j] == -1 ) {
	val = dis_gef_conn[ef].AS_c[j];
      } else {
	if ( !dis_determine_source_val( source, dis_gef_conn[ef].AS_fl_[j], &val ) ) {
	  /* effect rh makes use of undefined fluent!
	   */
	  for ( i = 0; i < dis_gnum_fl_conn; i++ ) {
	    assigned[i] = dis_FALSE;
	  }
	  for ( i = 0; i < source->num_F; i++ ) {
	    in_source[source->F[i]] = dis_FALSE;
	  }
	  for ( i = 0; i < num_del; i++ ) {
	    in_del[del[i]] = dis_FALSE;
	  }
	  for ( i = 0; i < dis_gop_conn[op].num_E; i++ ) {
	    true_ef[i] = dis_FALSE;
	  }
	  return dis_FALSE;
	}
	val += dis_gef_conn[ef].AS_c[j];
      }
      if ( assigned[fl] &&
	   val != dest->f_V[fl] ) {
	/* two different values assigned --> result not well-defined --> illegal!
	 */
	for ( i = 0; i < dis_gnum_fl_conn; i++ ) {
	  assigned[i] = dis_FALSE;
	}
	for ( i = 0; i < source->num_F; i++ ) {
	  in_source[source->F[i]] = dis_FALSE;
	}
	for ( i = 0; i < num_del; i++ ) {
	  in_del[del[i]] = dis_FALSE;
	}
	for ( i = 0; i < dis_gop_conn[op].num_E; i++ ) {
	  true_ef[i] = dis_FALSE;
	}
	return dis_FALSE;
      }
      dest->f_D[fl] = dis_TRUE;
      dest->f_V[fl] = val;
      assigned[fl] = dis_TRUE;
    }
    for ( j = 0; j < dis_gef_conn[ef].num_IN; j++ ) {
      fl = dis_gef_conn[ef].IN_fl[j];
      if ( assigned[fl] || 
	   !source->f_D[fl]) {
	/* assign and increase --> result not well-defined --> illegal!
	 * affects an undefined fluent --> illegal!
	 */
	for ( i = 0; i < dis_gnum_fl_conn; i++ ) {
	  assigned[i] = dis_FALSE;
	}
	for ( i = 0; i < source->num_F; i++ ) {
	  in_source[source->F[i]] = dis_FALSE;
	}
	for ( i = 0; i < num_del; i++ ) {
	  in_del[del[i]] = dis_FALSE;
	}
	for ( i = 0; i < dis_gop_conn[op].num_E; i++ ) {
	  true_ef[i] = dis_FALSE;
	}
	return dis_FALSE;
      }
      if ( dis_gef_conn[ef].IN_fl_[j] == -1 ) {
	val = dis_gef_conn[ef].IN_c[j];
      } else {
	if ( !dis_determine_source_val( source, dis_gef_conn[ef].IN_fl_[j], &val ) ) {
	  /* effect rh makes use of undefined fluent!
	   */
	  for ( i = 0; i < dis_gnum_fl_conn; i++ ) {
	    assigned[i] = dis_FALSE;
	  }
	  for ( i = 0; i < source->num_F; i++ ) {
	    in_source[source->F[i]] = dis_FALSE;
	  }
	  for ( i = 0; i < num_del; i++ ) {
	    in_del[del[i]] = dis_FALSE;
	  }
	  for ( i = 0; i < dis_gop_conn[op].num_E; i++ ) {
	    true_ef[i] = dis_FALSE;
	  }
	  return dis_FALSE;
	}
	val += dis_gef_conn[ef].IN_c[j];
      }
      dest->f_V[fl] += val;
    }
  }

  /* put all non-deleted facts from source into dest.
   * need not check for put-in facts here,
   * as initial state is made doubles-free, and invariant keeps
   * true through the transition procedure
   */
  dest->num_F = 0;
  for ( i = 0; i < source->num_F; i++ ) {
    if ( in_del[source->F[i]] ) {
      continue;
    }
    dest->F[dest->num_F++] = source->F[i];
    in_dest[source->F[i]] = dis_TRUE;
  }

  /* now add all fullfilled effect adds to dest; each fact at most once!
   */
  for ( i = 0; i < dis_gop_conn[op].num_E; i++ ) {
    if ( !true_ef[i] ) continue;
    ef = dis_gop_conn[op].E[i];
    for ( j = 0; j < dis_gef_conn[ef].num_A; j++ ) {
      if ( in_dest[dis_gef_conn[ef].A[j]] ) {
	continue;
      }
      dest->F[dest->num_F++] = dis_gef_conn[ef].A[j];
      in_dest[dis_gef_conn[ef].A[j]] = dis_TRUE;
    }
  }

  /* unset infos
   */
  for ( i = 0; i < source->num_F; i++ ) {
    in_source[source->F[i]] = dis_FALSE;
  }
  for ( i = 0; i < dest->num_F; i++ ) {
    in_dest[dest->F[i]] = dis_FALSE;
  }
  for ( i = 0; i < num_del; i++ ) {
    in_del[del[i]] = dis_FALSE;
  }
  for ( i = 0; i < dis_gop_conn[op].num_E; i++ ) {
    true_ef[i] = dis_FALSE;
  }
  for ( i = 0; i < dis_gnum_fl_conn; i++ ) {
    assigned[i] = dis_FALSE;
  }

  
  if(GpG.is_deripred) {
    DP_fixpoint_state(dest, work_ft, work_op, &work_S0, &work_S1); 
  } 
  return dis_TRUE;

}

dis_Bool dis_result_to_dest0( dis_State *dest, dis_State *source, int op, 
        int parent_op)
{

  static dis_Bool first_call = dis_TRUE;
  static dis_Bool *in_source, *in_dest, *in_del, *true_ef, *assigned;
  static int *del, num_del;

  int i, j, ef, fl;
  float val, source_val;
  dis_Comparator comp;

  dis_Bool one_appeared = dis_FALSE;

  if ( first_call ) {
    in_source = ( dis_Bool * ) calloc( dis_gnum_ft_conn, sizeof( dis_Bool ) );
    in_dest = ( dis_Bool * ) calloc( dis_gnum_ft_conn, sizeof( dis_Bool ) );
    in_del = ( dis_Bool * ) calloc( dis_gnum_ft_conn, sizeof( dis_Bool ) );
    true_ef = ( dis_Bool * ) calloc( dis_gnum_ef_conn+5, sizeof( dis_Bool ) );
    assigned = ( dis_Bool * ) calloc( dis_gnum_fl_conn, sizeof( dis_Bool ) );
    del = ( int * ) calloc( dis_gnum_ft_conn, sizeof( int ) );
    for ( i = 0; i < dis_gnum_ft_conn; i++ ) {
      in_source[i] = dis_FALSE;
      in_dest[i] = dis_FALSE;
      in_del[i] = dis_FALSE;
    }
    for ( i = 0; i < dis_gnum_ef_conn; i++ ) {
      true_ef[i] = dis_FALSE;
    }
    for ( i = 0; i < dis_gnum_fl_conn; i++ ) {
      assigned[i] = dis_FALSE;
    }
    first_call = dis_FALSE;
  }

  /* setup true facts for effect cond evaluation
   */
  for ( i = 0; i < source->num_F; i++ ) {
    in_source[source->F[i]] = dis_TRUE;
  }

  /* evaluate effect conditions and setup deleted facts
   */
  num_del = 0;
  for ( i = 0; i < dis_gop_conn[op].num_E; i++ ) {
    ef = dis_gop_conn[op].E[i];
    /* logic cond true?
     */
    for ( j = 0; j < dis_gef_conn[ef].num_PC; j++ ) {
      if ( !in_source[dis_gef_conn[ef].PC[j]] ) break;
    }
    if ( j < dis_gef_conn[ef].num_PC ) continue;
    /* numeric cond true?
     */
    for ( j = 0; j < dis_gef_conn[ef].num_f_PC; j++ ) {
      fl = dis_gef_conn[ef].f_PC_fl[j];
      val = dis_gef_conn[ef].f_PC_c[j];
      comp = dis_gef_conn[ef].f_PC_comp[j];
      if ( !dis_determine_source_val( source, fl, &source_val ) ) {
	/* condition access to an undefined fluent!
	 */
	for ( i = 0; i < num_del; i++ ) {
	  in_del[del[i]] = dis_FALSE;
	}
	for ( i = 0; i < dis_gop_conn[op].num_E; i++ ) {
	  true_ef[i] = dis_FALSE;
	}
	for ( i = 0; i < source->num_F; i++ ) {
	  in_source[source->F[i]] = dis_FALSE;
	}
	return dis_FALSE;
      }
      if ( !dis_number_comparison_holds( comp, source_val, val ) ) break;
    }
    if ( j < dis_gef_conn[ef].num_f_PC ) continue;

    if ( dis_gef_conn[ef].illegal ) {
      /* effect always affects an undefined fluent, as we found out
       * earlier
       */
      for ( i = 0; i < source->num_F; i++ ) {
	in_source[source->F[i]] = dis_FALSE;
      }
      for ( i = 0; i < num_del; i++ ) {
	in_del[del[i]] = dis_FALSE;
      }
      for ( i = 0; i < dis_gop_conn[op].num_E; i++ ) {
	true_ef[i] = dis_FALSE;
      }
      return dis_FALSE;
    }
    true_ef[i] = dis_TRUE;
    one_appeared = dis_TRUE; 
    for ( j = 0; j < dis_gef_conn[ef].num_D; j++ ) {
      if ( in_del[dis_gef_conn[ef].D[j]] ) continue;
      in_del[dis_gef_conn[ef].D[j]] = dis_TRUE;
      del[num_del++] = dis_gef_conn[ef].D[j];
    }
  }
  if ( !one_appeared ) {
    /* no effect appeared which means that the action is either useless
     * here or its preconds are even not fulfilled (the latter
     * shouldn't happen by dis_get_A but well....)
     */
    for ( i = 0; i < source->num_F; i++ ) {
      in_source[source->F[i]] = dis_FALSE;
    }
    for ( i = 0; i < num_del; i++ ) {
      in_del[del[i]] = dis_FALSE;
    }
    for ( i = 0; i < dis_gop_conn[op].num_E; i++ ) {
      true_ef[i] = dis_FALSE;
    }
    return dis_FALSE;
  }

  /* first, see about the numeric effects - those might render
   * the op illegal here. start by copying numeric info.
   */
  for ( i = 0; i < dis_gnum_fl_conn; i++ ) {
    dest->f_D[i] = source->f_D[i];
    dest->f_V[i] = source->f_V[i];
  }

  /* illegal is an op if the result is not well-defined,
   * or if it affects an undefined fluent.
   */
  for ( i = 0; i < dis_gop_conn[op].num_E; i++ ) {
    if ( !true_ef[i] ) continue;
    ef = dis_gop_conn[op].E[i];
    for ( j = 0; j < dis_gef_conn[ef].num_AS; j++ ) {
      fl = dis_gef_conn[ef].AS_fl[j];
      if ( dis_gef_conn[ef].AS_fl_[j] == -1 ) {
	val = dis_gef_conn[ef].AS_c[j];
      } else {
	if ( !dis_determine_source_val( source, dis_gef_conn[ef].AS_fl_[j], &val ) ) {
	  /* effect rh makes use of undefined fluent!
	   */
	  for ( i = 0; i < dis_gnum_fl_conn; i++ ) {
	    assigned[i] = dis_FALSE;
	  }
	  for ( i = 0; i < source->num_F; i++ ) {
	    in_source[source->F[i]] = dis_FALSE;
	  }
	  for ( i = 0; i < num_del; i++ ) {
	    in_del[del[i]] = dis_FALSE;
	  }
	  for ( i = 0; i < dis_gop_conn[op].num_E; i++ ) {
	    true_ef[i] = dis_FALSE;
	  }
	  return dis_FALSE;
	}
	val += dis_gef_conn[ef].AS_c[j];
      }
      if ( assigned[fl] &&
	   val != dest->f_V[fl] ) {
	/* two different values assigned --> result not well-defined --> illegal!
	 */
	for ( i = 0; i < dis_gnum_fl_conn; i++ ) {
	  assigned[i] = dis_FALSE;
	}
	for ( i = 0; i < source->num_F; i++ ) {
	  in_source[source->F[i]] = dis_FALSE;
	}
	for ( i = 0; i < num_del; i++ ) {
	  in_del[del[i]] = dis_FALSE;
	}
	for ( i = 0; i < dis_gop_conn[op].num_E; i++ ) {
	  true_ef[i] = dis_FALSE;
	}
	return dis_FALSE;
      }
      dest->f_D[fl] = dis_TRUE;
      dest->f_V[fl] = val;
      assigned[fl] = dis_TRUE;
    }
    for ( j = 0; j < dis_gef_conn[ef].num_IN; j++ ) {
      fl = dis_gef_conn[ef].IN_fl[j];
      if ( assigned[fl] || 
	   !source->f_D[fl]) {
	/* assign and increase --> result not well-defined --> illegal!
	 * affects an undefined fluent --> illegal!
	 */
	for ( i = 0; i < dis_gnum_fl_conn; i++ ) {
	  assigned[i] = dis_FALSE;
	}
	for ( i = 0; i < source->num_F; i++ ) {
	  in_source[source->F[i]] = dis_FALSE;
	}
	for ( i = 0; i < num_del; i++ ) {
	  in_del[del[i]] = dis_FALSE;
	}
	for ( i = 0; i < dis_gop_conn[op].num_E; i++ ) {
	  true_ef[i] = dis_FALSE;
	}
	return dis_FALSE;
      }
      if ( dis_gef_conn[ef].IN_fl_[j] == -1 ) {
	val = dis_gef_conn[ef].IN_c[j];
      } else {
	if ( !dis_determine_source_val( source, dis_gef_conn[ef].IN_fl_[j], &val ) ) {
	  /* effect rh makes use of undefined fluent!
	   */
	  for ( i = 0; i < dis_gnum_fl_conn; i++ ) {
	    assigned[i] = dis_FALSE;
	  }
	  for ( i = 0; i < source->num_F; i++ ) {
	    in_source[source->F[i]] = dis_FALSE;
	  }
	  for ( i = 0; i < num_del; i++ ) {
	    in_del[del[i]] = dis_FALSE;
	  }
	  for ( i = 0; i < dis_gop_conn[op].num_E; i++ ) {
	    true_ef[i] = dis_FALSE;
	  }
	  return dis_FALSE;
	}
	val += dis_gef_conn[ef].IN_c[j];
      }
      dest->f_V[fl] += val;
    }
  }

  /* put all non-deleted facts from source into dest.
   * need not check for put-in facts here,
   * as initial state is made doubles-free, and invariant keeps
   * true through the transition procedure
   */
  dest->num_F = 0;
  for ( i = 0; i < source->num_F; i++ ) {
    if ( in_del[source->F[i]] ) {
      continue;
    }
    dest->F[dest->num_F++] = source->F[i];
    in_dest[source->F[i]] = dis_TRUE;
  }

  /* now add all fullfilled effect adds to dest; each fact at most once!
   */
  for ( i = 0; i < dis_gop_conn[op].num_E; i++ ) {
    if ( !true_ef[i] ) continue;
    ef = dis_gop_conn[op].E[i];
    for ( j = 0; j < dis_gef_conn[ef].num_A; j++ ) {
      if ( in_dest[dis_gef_conn[ef].A[j]] ) {
	continue;
      }
      dest->F[dest->num_F++] = dis_gef_conn[ef].A[j];
      in_dest[dis_gef_conn[ef].A[j]] = dis_TRUE;
    }
  }

  /* unset infos
   */
  for ( i = 0; i < source->num_F; i++ ) {
    in_source[source->F[i]] = dis_FALSE;
  }
  for ( i = 0; i < dest->num_F; i++ ) {
    in_dest[dest->F[i]] = dis_FALSE;
  }
  for ( i = 0; i < num_del; i++ ) {
    in_del[del[i]] = dis_FALSE;
  }
  for ( i = 0; i < dis_gop_conn[op].num_E; i++ ) {
    true_ef[i] = dis_FALSE;
  }
  for ( i = 0; i < dis_gnum_fl_conn; i++ ) {
    assigned[i] = dis_FALSE;
  }

  return dis_TRUE;
}

dis_Bool dis_determine_source_val( dis_State *source, int fl, float *val )

{

  int i;

  if ( dis_gfl_conn[fl].artificial ) {
    *val = 0;
    for ( i = 0; i < dis_gfl_conn[fl].num_lnf; i++ ) {
      if ( !source->f_D[dis_gfl_conn[fl].lnf_F[i]] ) {
	return dis_FALSE;
      }
      *val += (dis_gfl_conn[fl].lnf_C[i] * source->f_V[dis_gfl_conn[fl].lnf_F[i]]);
    }
  } else {
    if ( !source->f_D[fl] ) {
      return dis_FALSE;
    }
    *val = source->f_V[fl];
  }

  return dis_TRUE;

}



void dis_copy_dis_source_to_dest( dis_State *dest, dis_State *source )

{

  int i;

  dis_make_state( dest, dis_gnum_ft_conn, dis_gnum_fl_conn );

  for ( i = 0; i < source->num_F; i++ ) {
    dest->F[i] = source->F[i];
  }
  dest->num_F = source->num_F;

  for ( i = 0; i < dis_gnum_fl_conn; i++ ) {
    dest->f_D[i] = source->f_D[i];
    dest->f_V[i] = source->f_V[i];
  }

}


void dis_source_to_dest( dis_State *dest, dis_State *source )

{

  int i;

  for ( i = 0; i < source->num_F; i++ ) {
    dest->F[i] = source->F[i];
  }
  dest->num_F = source->num_F;
   
  for ( i = 0; i < dis_gnum_fl_conn; i++ ) {
    dest->f_D[i] = source->f_D[i];
    dest->f_V[i] = source->f_V[i];
  }
}
