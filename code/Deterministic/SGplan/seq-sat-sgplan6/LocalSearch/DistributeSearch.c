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

 * File: DistributeSearch.c 

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

/*********************************************************************
 * (C) Copyright 2002  Universita' degli Studi di Brescia
 *     Dipartimento di Elettronica per l'Automazione
 *     Via Branze 38, 25123 Brescia, Italy
 *
 * All rights reserved. Use of this software is permitted ONLY for
 * non-commercial research purposes, and it may be copied only
 * for that use only. All copies must include this copyright message.
 * This software is made available AS IS, and neither the authors
 * nor the University of Brescia make any warranty about the
 * software or its performance.
 *
 *********************************************************************/






/********************************************************************
 * File: LocalSearch.c
 * Description: Local search method
 *
 *   PDDL 2.1 version without conditional and quantified effects 
 *
 * Authors: Alfonso Gerevini, Marco Lazzaroni, Alessandro Saetti, 
 *          Ivan Serina, Sergio Spinoni
 *
 *********************************************************************/




#include <values.h>
#include <math.h>
#include "lpg.h"
#include "ff.h"
#include "LpgTime.h"
#include "check.h"
#include "numeric.h"
#include "ActionSubgraph.h"
#include "H_relaxed.h"
#include "H_max.h"
#include "utilities.h"
#include "LpgOutput.h"
#include "output.h"
#include <unistd.h>
#include <sys/types.h>
#include "esp.h" 
#include "subspace.h"
#include "DistributeSearch.h"
#include "LocalSearch.h"
#include "orderings.h"
#include "subfluent.h"
#include "dis_ff.h"
#include "dis_output.h"
#include "stripsff.h"
#include "search.h"
#include "dis_search.h"
#include "dis_inst_final.h"
#include "esp1.h"
#include "dis_constraints.h"
#include "dis_producible.h"

#define MAX_SUBPATHS 100
#define MAX_SUBPATH_LEN 20
#define MAX_LAG_SUB 112
#define MIN_LAG_SUB 45

extern int* decr_g;
extern int rsa_facts[MAX_FT_NUM];
extern int rsa_actions[MAX_OP_NUM];
extern State* gsolved_state;
extern int symm_constraint_eval(dis_State *S, int goal);
extern void find_intermediate_goals(State *, int);
extern void set_dummy_fid();
extern void short_proc_sofar(int);
extern void dis_make_state( dis_State *, int, int);
extern dis_State saved_start_state;

extern char *select_inst, *select_sat;

extern int SymmConstrEvalOption;
extern Bool SymmCompart;
extern int gnum_symm_norm;
extern int dis_gnum_tif;

vState known_iga_list;
PlanAction ** g_plan_actions;
int syg_degree[MAX_SYMM_GROUP][MAX_SYMM_GROUP];


 // randomizing (optional)
void dis_reorder_saved_goal(State *saved_end_state)
{
    int gs, i, j, t;
    State S;
    
    S.num_F = saved_end_state->num_F;
    for(i=0; i<saved_end_state->num_F; i++) {
        S.F[i] = saved_end_state->F[i];
    } 
    
    gs = saved_end_state->num_F;
    for(i=0; i<gs-1; i++) {
        j= i+ (int) ((double)(gs-i)*rand()/(RAND_MAX+1.0));
        if(j!=i) {
            t = S.F[i];
            S.F[i] = S.F[j];
            S.F[j] = t;
        }
    }   
    
    for(i=0; i<saved_end_state->num_F; i++) {
        saved_end_state->F[i] = S.F[i];
    } 
}
                                
void fout_vState(vState *needf, char *s)
{
    int i;
    for(i=0; i<needf->num_F;i++) {
        printf("%s %d: %d ",s, i, needf->F[i]);
        print_ft_name(needf->F[i]);
        printf("\n");
    }
}

void dis_fout_vState(vState *needf, char *s)
{
    int i;
    for(i=0; i<needf->num_F;i++) {
        printf("%s %d: %d ",s, i, needf->F[i]);
        dis_print_ft_name(needf->F[i]);
        printf("\n");
    }
}

// 0: no order
// 1: f1 <== f2
int gam_order(int g, int f)
{
    if(!mutex_fact_achievable(f,g)) return 1; 
    return 0;
}

void zinf_gam(vState *zinf, vState *needf)
{
    int i,j,f,g;
    for(i=0; i<zinf->num_F; i++) {
        f = zinf->F[i];
        for(j=0; j<needf->num_F; j++) {
            g = needf->F[j];
            if(g ==f) continue;
    //        if( !mutex_fact_achievable(f,g) ) {
            if( gam_order(g,f) ) {
                print_ft_name(f);
                printf(" after ");
                print_ft_name(g);
                printf("\n");
            }
        }
    }
}

void push_iga_stack(vState *stack, vState *fs)
{
    int i;
    for(i=0; i<fs->num_F; i++) {    
        stack->F[stack->num_F++] = fs->F[i]; 
    } 
}

void general_IMA(State * start_state, State * end_state,
             PlanAction ** plan_actions)
{
    load_ff_gef_conn();
    genIMA_analysis(start_state, rsa_facts, rsa_actions, decr_g);

    //load_lpg_gef_conn();
    //output_esp_solution(plan_actions);    
}                                           

void test_IMA(State * start_state, State * end_state,
             PlanAction ** plan_actions)
{
    //vState needf;
    
    // 0. gef_conn stuff
    load_ff_gef_conn();
   
    if(GpG.SearchModal == 105) {
        IMA_tif_wrapper(start_state, end_state);
    }
    
    omita_analysis(start_state, rsa_facts, rsa_actions, decr_g);

    /*
    build_iga_graph(start_state, rsa_facts, rsa_actions, decr_g);
    precede_facts_of_state(&saved_ggoal_state, &needf);
    ima_analysis(start_state,  &needf, 1, decr_g, &i);
      */
    
    load_lpg_gef_conn();
    output_esp_solution(plan_actions);    
    //print_iga_graph(&needf);
}                                           


void distribute_IGA (State * start_state, State * end_state, 
     PlanAction ** plan_actions)
{
    vState needf, wf;
    vState iga_stack;
    
    // 0. gef_conn stuff
    load_ff_gef_conn();

    // 1. init
    iga_stack.num_F =0;
    
    // 1. the main loop
    while(1) {
        build_iga_graph(start_state, rsa_facts, rsa_actions, decr_g);  
        precede_facts_of_state(&saved_ggoal_state, &needf); 
        fout_vState(&needf, "needf");
    
        zin_facts(start_state, &needf, &wf);
        fout_vState(&wf, "0in_f");
  
    }
//    mxtop_subspace(start_state, rsa_facts, rsa_actions, decr_g);
//    print_iga_graph(NULL);
}

void subpath_subgoal_MFF(State * start_state, State * end_state,
                 PlanAction ** plan_actions)
{                
    int i,j,k, m;
    State saved_end_state;
    int path[MAX_SUBPATHS][MAX_SUBPATH_LEN];
    int path_len[MAX_SUBPATHS];
    int num_path;
    
    
    load_ff_gef_conn();
    if(ComputeMutex) twist_goal_order(end_state);
    source_to_dest(&saved_end_state, end_state); 
    
    var_source_to_dest(start_state, &saved_ginitial_state);
    GpG.num_esp_sol = 0; 
    dis_known_iga_list.num_F = 0;
    
    num_path = saved_end_state.num_F;
    for (j = 0; j < saved_end_state.num_F; j++)
    {
        end_state->num_F = 1;
        end_state->F[0] = saved_end_state.F[j];
        
        //printf("\n%d\n",j);
        //print_ft(saved_end_state.F[j]);
        
        mff_bridge(start_state, end_state);
        mff_record_esp_sol();
        
        for(i=0; i<GpG.num_esp_sol; i++) {
          //  print_op(GpG.esp_solution[i]);
            path[j][i] = GpG.esp_solution[i];
        }   
        path_len[j] = GpG.num_esp_sol;
        GpG.num_esp_sol=0;
    }   
    
    GpG.num_esp_sol=0;
    if(GpG.SearchModal == 1) {
        m = num_path/2;
        for(i=num_path-1; i>=m; i--) {
            for(j=0; j<5; j++) {
                GpG.esp_solution[GpG.num_esp_sol++] = path[i][j];
            }   
        }   
        
        for(i=num_path-1; i>=m; i--) {
            for(j=5; j<=7; j++) {
                GpG.esp_solution[GpG.num_esp_sol++] = path[i][j];
            }   
            if(i == num_path-1) {k=0;}
            else { k = i-m+1;}
                 
            GpG.esp_solution[GpG.num_esp_sol++] = path[k][3];
            GpG.esp_solution[GpG.num_esp_sol++] = path[i][8];
            
            for(j=path_len[k]-6; j<=path_len[k]-3; j++) {
                GpG.esp_solution[GpG.num_esp_sol++] = path[k][j];
            }   
        }   
        
        for(i=num_path-1; i>=0; i--) {
            j = path_len[i]-2;
            GpG.esp_solution[GpG.num_esp_sol++] = path[i][j];
        }   
        for(i=num_path-1; i>=0; i--) {
            j = path_len[i]-1;
            GpG.esp_solution[GpG.num_esp_sol++] = path[i][j];
        }   
        return;
    }   
    
    if(GpG.SearchModal == 2) {
        for(i=0; i<num_path; i++) {
            GpG.esp_solution[GpG.num_esp_sol++] = path[i][0];
        }   
        for(i=num_path-1; i>=0; i--) {
            for(j=1; j< path_len[i]-2; j++) {
                GpG.esp_solution[GpG.num_esp_sol++] = path[i][j];
            }   
        }       
        GpG.esp_solution[GpG.num_esp_sol++] = path[0][path_len[0]-2];
        GpG.esp_solution[GpG.num_esp_sol++] = path[0][path_len[0]-1];
        i=num_path-1;
        GpG.esp_solution[GpG.num_esp_sol++] = path[i][path_len[i]-2];
        GpG.esp_solution[GpG.num_esp_sol++] = path[i][path_len[i]-1];
        for(i=1; i<num_path-1; i++) {
            GpG.esp_solution[GpG.num_esp_sol++] = path[i][path_len[i]-2];
            GpG.esp_solution[GpG.num_esp_sol++] = path[i][path_len[i]-1];
        }   
        return;
    }   
}   
    
Bool subpath_mff_subgoal()
{
    int i,j;
    int path[MAX_SUBPATHS][MAX_SUBPATH_LEN];
    int path_len[MAX_SUBPATHS];
    int num_path;
    
    GpG.num_esp_sol = 0;
    
    max_hc_iter = 10000;
    max_bfs_iter = 30000;
    
    num_path = saved_dis_gnum_flogic_goal;
    for (j = 0; j < saved_dis_gnum_flogic_goal; j++)
    {
                       
        dis_gnum_flogic_goal = 1;
        dis_gflogic_goal[0] = saved_dis_gflogic_goal[j];
                                            
        dis_mff_bridge();                   
        mff_record_esp_sol();               
       
       /* 
        if(dis_ef_predlen(GpG.esp_solution[5])!=10) {
            dis_gnum_flogic_goal = saved_dis_gnum_flogic_goal;
            for (k = 0; k < saved_dis_gnum_flogic_goal; k++) {
                dis_gflogic_goal[k] = saved_dis_gflogic_goal[k];
            }       
            dis_source_to_dest(&dis_ginitial_state, &saved_dis_ginitial_state);
            return FALSE;
        }*/
        
        for(i=0; i<GpG.num_esp_sol; i++) {
            path[j][i] = GpG.esp_solution[i];
        }   
        path_len[j] = GpG.num_esp_sol;
        GpG.num_esp_sol=0;
    }                     
    GpG.num_esp_sol=0;    
   
        for(i=0; i<num_path; i++) {
            GpG.esp_solution[GpG.num_esp_sol++] = path[i][0];
        }   
        for(i=num_path-1; i>=0; i--) {
            for(j=1; j< path_len[i]-2; j++) {
                GpG.esp_solution[GpG.num_esp_sol++] = path[i][j];
            }   
        }       
        i=num_path-1;
        GpG.esp_solution[GpG.num_esp_sol++] = path[i][path_len[i]-2];
        for(i=0; i<num_path-1; i++) {
            GpG.esp_solution[GpG.num_esp_sol++] = path[i][path_len[i]-2];
        }   
        for(i=num_path-2; i>=0; i--) {
            GpG.esp_solution[GpG.num_esp_sol++] = path[i][path_len[i]-1];
        }   
        i=num_path-1;
        GpG.esp_solution[GpG.num_esp_sol++] = path[i][path_len[i]-1];
        return TRUE;
}       
    


int
distribute_FF (State * start_state, State * end_state,
	     PlanAction ** plan_actions)
{
	int i,j,k,tp;
	int fd[2];
	int pid;
	State saved_end_state;
   
    load_ff_gef_conn();

    GpG.total_num_subprob = 0;
    GpG.num_esp_sol = 0; 
    tp =0;
    
    //init_h1_h2();
    //random_twist_goal_order(end_state);
    twist_goal_order(end_state);
    source_to_dest(&saved_end_state, end_state); 
   
	for (j = 0; j < saved_end_state.num_F; j++)	     
	{
        GpG.total_num_subprob++;
                
        find_intermediate_goals(start_state, saved_end_state.F[j]);
        
        for(i=0; i<j; i++) end_state->F[i] = saved_end_state.F[i];
        for(i=0; i<GpG.num_sub_agenda_IMG; i++) {
          
               tp ++;  
               //printf("\nstart state ");print_state(*start_state);printf("\n");
                                
                /* end state */
                for(k=0; k<GpG.sub_agenda_IMG[i].num_F;k++) {        
                    end_state->F[j+k] =  GpG.sub_agenda_IMG[i].F[k];
                }
                end_state->num_F = j + GpG.sub_agenda_IMG[i].num_F ;
                source_to_dest(&ggoal_state, end_state); 
	            //printf("\nend state ");print_state(*end_state);printf("\n");
               
                /* -----------------------------------------------*/
                if(pipe(fd)<0) {
			        printf("pipe can't be created\n");
			        exit(1);
		        }
		        if((pid=fork()) < 0){
			        printf("fork failed\n");
			        exit(1);
		        } else if(pid == 0) {  
			        close(fd[0]);
                    /*
                    if(!do_enforced_hill_climbing (start_state, end_state)) {
                        printf(">>>>>>> Hill climbing failed! <<<<<<<\n\n"); 
                        gnum_plan_ops = -1;
                    }
                    else    
                       source_to_dest (&GpG.sol_state, &(gplan_states[gnum_plan_ops]));
                    */
                    if(!do_best_first_search ()) {
                        printf(">>>>>>> best first failed! <<<<<<<\n\n"); 
                        gnum_plan_ops = -1;
                    }
                    else    
                        source_to_dest (&GpG.sol_state, &gbfs_state);
                        
			        ff_send_plan_actions(fd[1]);
		            send_sol_state(fd[1]);
                    close(fd[1]);
			        exit(0);		     
		        } else {
			        close(fd[1]);
			        ff_recv_plan_actions(fd[0]);
                    recv_sol_state(fd[0]);
			        close(fd[0]);		
		        }
                
                if(gnum_plan_ops < 0) exit(0);
                else FF_record_esp_sol();  
                var_source_to_dest(start_state, &GpG.sol_state);
                var_source_to_dest(&ginitial_state, &GpG.sol_state);
        }
	}						

    load_lpg_gef_conn();
    output_esp_solution(plan_actions);    
    return 1;   
}

void distribute_LPG(State * start_state, State * end_state,
                 PlanAction ** plan_actions)
{
    int i,j,ns;
    int fd[2];
    int pid;    
    int sgs;
    State saved_end_state;
    
    init_h1_h2();
 
  for(ns=0; ns<1; ns++) {
    random_twist_goal_order(end_state);
    source_to_dest(&saved_end_state, end_state);
    //printf("\nrandom twisted end state ");print_state(*end_state);
    //printf("\n");

    var_source_to_dest(start_state, &saved_ginitial_state);
    GpG.num_esp_sol = 0; 
    
    for (j = 0; j < saved_end_state.num_F; j++)
    {
        GpG.total_num_subprob++;
        for(i=0; i<=j; i++) end_state->F[i] = saved_end_state.F[i];
        end_state->num_F = j+1;

    	if(pipe(fd)<0) {
            printf("pipe can't be created\n");
            exit(1);
        }   
        
        if((pid=fork()) < 0){
            printf("fork failed\n");
            exit(1);
        } else if(pid == 0) {
            close(fd[0]);            
            sgs = subGoalSearch(start_state, end_state, plan_actions);
            
            send_plan_actions(fd[1], plan_actions);
            send_sol_state(fd[1]);
            close(fd[1]);
            exit(0);     
        } else {
            close(fd[1]);
            recv_plan_actions(fd[0], plan_actions);
            recv_sol_state(fd[0]);
            close(fd[0]);       
        }   
        
        LPG_record_esp_sol();
   
        var_source_to_dest(start_state, &GpG.sol_state);
        var_source_to_dest(&ginitial_state, &GpG.sol_state);
    }   

    output_esp_solution(plan_actions);    
          
    if (GpG.maximize_plan && GpG.total_cost < 0) {
        
		printf
		  ("\nSolution number: %d\nActions:         %d\nExecution cost:  %.2f\nDuration:        %.3f\nPlan quality:    %.3f",
		   GpG.num_solutions, GpG.num_actions,
		   GpG.total_cost * (-1), GpG.total_time,
		   GpG.total_cost * GpG.orig_weight_cost * (-1) +
		   GpG.total_time * GpG.orig_weight_time);
    }
    else {
        printf
		  ("\nSolution number: %d\nActions:         %d\nExecution cost:  %.2f\nDuration:        %.3f\nPlan quality:    %.3f",
		   GpG.num_solutions, GpG.num_actions,
		   GpG.total_cost, GpG.total_time,
		   GpG.total_cost * GpG.orig_weight_cost +
		   GpG.total_time * GpG.orig_weight_time);
        
    }
  }
}

int
dfs_iga_MFF (State * start_state, State * end_state,
	     PlanAction ** plan_actions)
{
	int k;
	State saved_end_state;
    vState needf, wf;
   
    load_ff_gef_conn();
    GpG.num_esp_sol = 0; 
        
    source_to_dest(&saved_end_state, end_state); 
    dis_known_iga_list.num_F = 0;
    for(k=0; k<saved_end_state.num_F; k++) {
        dis_known_iga_list.F[dis_known_iga_list.num_F++] = saved_end_state.F[k];    
    }
   
    while(1) { 
        build_iga_graph(start_state, rsa_facts, rsa_actions, decr_g);
        precede_facts_of_state(&saved_ggoal_state, &needf);
        fout_vState(&needf, "needf");
    
        zin_facts(start_state, &needf, &wf);
        fout_vState(&wf, "0in_f");

        if(wf.num_F == 0) break;
        
        /* end state */
        end_state->num_F = 1;
        end_state->F[0] = wf.F[0];

        source_to_dest(&ggoal_state, end_state); 
        printf("\nstart state ");print_state(*start_state);printf("\n");
        printf("\nend state ");print_state(*end_state);printf("\n");

        /* -----------------------------------------------*/
        mff_bridge(start_state, end_state);
        mff_record_esp_sol();

        var_source_to_dest(start_state, &GpG.sol_state);
        var_source_to_dest(&ginitial_state, &GpG.sol_state);
    }					

    load_lpg_gef_conn();
    output_esp_solution(plan_actions);    
    return 1;   
}
int
distribute_Hibury_LPG (State * start_state, State * end_state,
	     PlanAction ** plan_actions)
{
	int i,j,k,tp;
	int fd[2];
	int pid;
	int sgs;
	State saved_end_state;
   
     

    GpG.total_num_subprob = 0;
    GpG.num_esp_sol = 0; 
    tp =0;
    
    //init_h1_h2();
    //random_twist_goal_order(end_state);
    twist_goal_order(end_state);
    source_to_dest(&saved_end_state, end_state); 
   
	for (j = 0; j < saved_end_state.num_F; j++)	     
	{
        GpG.total_num_subprob++;
               
        for(i=0; i<j; i++) end_state->F[i] = saved_end_state.F[i];
       
        while(1) { 
                
                if(contain_fact_state(&GpG.sol_state, saved_end_state.F[j])) break; 
	            
                //printf("\nstart ");print_state(*start_state);printf("\n");
	            //printf("\ngoal fact ");
                //print_ft_name(saved_end_state.F[j]);printf("\n");
                  
                find_intermediate_goals(start_state, saved_end_state.F[j]);
                if(GpG.num_IMG_facts <0) {
                    printf("dead end\n");
                    exit(0);
                }                
                
                i=GpG.num_sub_agenda_IMG/3;
                if(GpG.num_sub_agenda_IMG-1 < i) i = GpG.num_sub_agenda_IMG-1;
                                        
                
                /* end state */
                for(k=0; k<GpG.sub_agenda_IMG[i].num_F;k++) {        
                    end_state->F[j+k] =  GpG.sub_agenda_IMG[i].F[k];
                }
                end_state->num_F = j + GpG.sub_agenda_IMG[i].num_F ;
                source_to_dest(&ggoal_state, end_state); 
	            printf("\nend state ");print_state(*end_state);printf("\n");
                 
                /* -----------------------------------------------*/
                if(pipe(fd)<0) {
			        printf("pipe can't be created\n");
			        exit(1);
		        }
		        if((pid=fork()) < 0){
			        printf("fork failed\n");
			        exit(1);
		        } else if(pid == 0) {  
			        close(fd[0]);
           
                    sgs = subGoalSearch(start_state, end_state, plan_actions);
          //          printf("endsol\n"); print_state(GpG.sol_state); printf("\n");
                    send_plan_actions(fd[1], plan_actions);
                    send_sol_state(fd[1]);
                    close(fd[1]);
			        exit(0);		     
		        } else {
			        close(fd[1]);
			        recv_plan_actions(fd[0], plan_actions);
                    recv_sol_state(fd[0]);
			        close(fd[0]);		
		        }
                
                LPG_record_esp_sol();  
                var_source_to_dest(start_state, &GpG.sol_state);
                var_source_to_dest(&ginitial_state, &GpG.sol_state);
        }
	}						

    output_esp_solution(plan_actions);    
    return 1;   
}

int
distribute_Hibury (State * start_state, State * end_state,
	     PlanAction ** plan_actions)
{
	int i,j,k,tp;
	int fd[2];
	int pid;
	State saved_end_state;
   
     
    load_ff_gef_conn();

     

    GpG.total_num_subprob = 0;
    GpG.num_esp_sol = 0; 
    tp =0;
    
    //init_h1_h2();
    //random_twist_goal_order(end_state);
    twist_goal_order(end_state);
    source_to_dest(&saved_end_state, end_state); 
   
	for (j = 0; j < saved_end_state.num_F; j++)	     
	{
        GpG.total_num_subprob++;
               
        for(i=0; i<j; i++) end_state->F[i] = saved_end_state.F[i];
                                        
        if(!v_contain_fact_state(&known_iga_list ,saved_end_state.F[j])) {
            known_iga_list.F[ known_iga_list.num_F++ ] = saved_end_state.F[j]; 
        }   
        
        fout_vState(&known_iga_list, "known_iga_list");
        
        while(1) { 
                if(contain_fact_state(&GpG.sol_state, saved_end_state.F[j])) break; 
                
                find_intermediate_goals(start_state, saved_end_state.F[j]);
                if(GpG.num_IMG_facts <0) {
                    printf("dead end\n");
                    exit(0);
                }

        
	            
                /*
                printf("\nstart ");print_state(*start_state);printf("\n");
	            printf("\ngoal fact ");
                print_ft_name(saved_end_state.F[j]);printf("\n");
                  */

                i=GpG.num_sub_agenda_IMG-1;
                if(GpG.num_sub_agenda_IMG-1 < i) i = GpG.num_sub_agenda_IMG-1;
                                        
                 
                /* end state */
                for(k=0; k<GpG.sub_agenda_IMG[i].num_F;k++) {        
                    end_state->F[j+k] =  GpG.sub_agenda_IMG[i].F[k];
                }
                end_state->num_F = j + GpG.sub_agenda_IMG[i].num_F ;
                source_to_dest(&ggoal_state, end_state); 
	            printf("\nend state ");print_state(*end_state);printf("\n");
                 
                /* -----------------------------------------------*/
                if(pipe(fd)<0) {
			        printf("pipe can't be created\n");
			        exit(1);
		        }
		        if((pid=fork()) < 0){
			        printf("fork failed\n");
			        exit(1);
		        } else if(pid == 0) {  
			        close(fd[0]);
                    if(!do_best_first_search ()) {
                        printf(">>>>>>> best first failed! <<<<<<<\n\n"); 
                        gnum_plan_ops = -1;
                    }
                    else    
                     source_to_dest (&GpG.sol_state, &gbfs_state);
                   
                     
                    //printf("\ngbfs\n");print_state(gbfs_state);
                    
			        ff_send_plan_actions(fd[1]);
		            send_sol_state(fd[1]);
                    close(fd[1]);
			        exit(0);		     
		        } else {
			        close(fd[1]);
			        ff_recv_plan_actions(fd[0]);
                    recv_sol_state(fd[0]);
			        close(fd[0]);		
		        }
                if(gnum_plan_ops < 0) exit(0);
                else FF_record_esp_sol();  
                
                var_source_to_dest(start_state, &GpG.sol_state);
                var_source_to_dest(&ginitial_state, &GpG.sol_state);
        }
	}						

    load_lpg_gef_conn();
    output_esp_solution(plan_actions);    
    return 1;   
}


void distribute_ima_LPG(State * start_state, State * end_state,
                        PlanAction ** plan_actions)
{
    int j;
    // int i, nima;
    State saved_end_state;
            vState needf;
    
    init_h1_h2();
 
    random_twist_goal_order(end_state);
    source_to_dest(&saved_end_state, end_state);
    //printf("\nrandom twisted end state ");print_state(*end_state);
    //printf("\n");

    var_source_to_dest(start_state, &saved_ginitial_state);
    GpG.num_esp_sol = 0; 
    
    for (j = 0; j < saved_end_state.num_F; j++)
    {
        end_state->F[0] = saved_end_state.F[j];
        end_state->num_F = 1;
            
        load_ff_gef_conn();
        build_iga_graph(start_state, rsa_facts, rsa_actions, decr_g);
        precede_facts_of_state(end_state, &needf);
        /*ima_analysis(start_state,  &needf, 1, decr_g, &nima);
        for(i=0;i<nima;i++) {
            printf("ima %d %d ", i, decr_g[i]);
            print_op_name(decr_g[i]); printf("\n");
        }*/            
        
        /*
        if(pipe(fd)<0) {
            printf("pipe can't be created\n");
            exit(1);
        }   
        
        if((pid=fork()) < 0){
            printf("fork failed\n");
            exit(1);
        } else if(pid == 0) {
            close(fd[0]);            
    
            load_lpg_gef_conn(); 
            sgs = subGoalSearch(start_state, end_state, plan_actions);
            
            send_plan_actions(fd[1], plan_actions);
            send_sol_state(fd[1]);
            close(fd[1]);
            exit(0);     
        } else {
            close(fd[1]);
            recv_plan_actions(fd[0], plan_actions);
            recv_sol_state(fd[0]);
            close(fd[0]);       
        }   
        
        LPG_record_esp_sol();
   
        var_source_to_dest(start_state, &GpG.sol_state);
        var_source_to_dest(&ginitial_state, &GpG.sol_state);
    */
      
    }   

    return ;
    output_esp_solution(plan_actions);    
          
    if (GpG.maximize_plan && GpG.total_cost < 0) {
        
		printf
		  ("\nSolution number: %d\nActions:         %d\nExecution cost:  %.2f\nDuration:        %.3f\nPlan quality:    %.3f",
		   GpG.num_solutions, GpG.num_actions,
		   GpG.total_cost * (-1), GpG.total_time,
		   GpG.total_cost * GpG.orig_weight_cost * (-1) +
		   GpG.total_time * GpG.orig_weight_time);
    }
    else {
        printf
		  ("\nSolution number: %d\nActions:         %d\nExecution cost:  %.2f\nDuration:        %.3f\nPlan quality:    %.3f",
		   GpG.num_solutions, GpG.num_actions,
		   GpG.total_cost, GpG.total_time,
		   GpG.total_cost * GpG.orig_weight_cost +
		   GpG.total_time * GpG.orig_weight_time);
        
    }
}
int
direct_MFF_symmetry (State * start_state, State * end_state,
	     PlanAction ** plan_actions)
{
    dis_known_iga_list.num_F = 0;
    mff_bridge(start_state, end_state); 
    mff_record_esp_sol();  

    dis_print_IPC5_modal3_GpG(gcmd_line.fct_file_name);
//    dis_print_IPC4_symmetry(gcmd_line.fct_file_name);
    exit(0);
}

int
direct_MFF (State * start_state, State * end_state,
	     PlanAction ** plan_actions)
{
    dis_known_iga_list.num_F = 0;
    mff_bridge(start_state, end_state); 
    mff_record_esp_sol();  
    // if(!GpG.is_deripred) 
    if(ComputeMutex) 
    {
        load_lpg_gef_conn();
        output_esp_solution(plan_actions);    
    } else {
        dis_print_IPC4(gcmd_line.fct_file_name);
        exit(0);
    }    
    return 1;   
}
    
void subgoal_MFF(State * start_state, State * end_state,
                 PlanAction ** plan_actions)
{
    int i,j,k;
    State saved_end_state;
    
    load_ff_gef_conn();
    if(ComputeMutex) twist_goal_order(end_state);
    source_to_dest(&saved_end_state, end_state); 

    var_source_to_dest(start_state, &saved_ginitial_state);
    GpG.num_esp_sol = 0; 
    
    for (j = 0; j < saved_end_state.num_F; j++)
    {
        GpG.total_num_subprob++;
        for(i=0; i<=j; i++) end_state->F[i] = saved_end_state.F[i];
        end_state->num_F = j+1;

      //  print_ft(saved_end_state.F[j]);
        
   //     printf("start state\n"); print_state(*start_state);
     //   printf("\nend state\n"); print_state(*end_state);
        
        if(contain_fact_state(start_state, saved_end_state.F[j])) continue;
        
        // get known_iga_list
        dis_known_iga_list.num_F = 0;
        for(k=j+1; k<saved_end_state.num_F; k++) {
            dis_known_iga_list.F[dis_known_iga_list.num_F++] 
                = saved_end_state.F[k];
        }
    
        mff_bridge(start_state, end_state);
        mff_record_esp_sol();
   
        var_source_to_dest(start_state, &GpG.sol_state);
        var_source_to_dest(&ginitial_state, &GpG.sol_state);
    }   
    if((ComputeMutex)&&(!GpG.is_deripred)) {
        load_lpg_gef_conn();
        output_esp_solution(plan_actions);    
    }
}

int
distribute_MFF (State * start_state, State * end_state,
	     PlanAction ** plan_actions)
{
	int i,j,k,tp;
	State saved_end_state;
   
    load_ff_gef_conn();

    GpG.total_num_subprob = 0;
    GpG.num_esp_sol = 0; 
    tp =0;
    
    if(ComputeMutex) twist_goal_order(end_state);
    source_to_dest(&saved_end_state, end_state); 
   
	for (j = 0; j < saved_end_state.num_F; j++)	     
	{
        GpG.total_num_subprob++;
        
        if(contain_fact_state(start_state,  saved_end_state.F[j])) continue;
        find_intermediate_goals(start_state, saved_end_state.F[j]);
        
        // get known_iga_list
        dis_known_iga_list.num_F = 0;
        for(k=j+1; k<saved_end_state.num_F; k++) {
            dis_known_iga_list.F[dis_known_iga_list.num_F++] 
                = saved_end_state.F[k];
        }
        
        for(i=0; i<j; i++) end_state->F[i] = saved_end_state.F[i];
        for(i=0; i<GpG.num_sub_agenda_IMG; i++) {
          
               tp ++;  
               //printf("\nstart state ");print_state(*start_state);printf("\n");
                                
                /* end state */
                for(k=0; k<GpG.sub_agenda_IMG[i].num_F;k++) {        
                    end_state->F[j+k] =  GpG.sub_agenda_IMG[i].F[k];
                }
                end_state->num_F = j + GpG.sub_agenda_IMG[i].num_F ;
                source_to_dest(&ggoal_state, end_state); 
	            //printf("\nend state ");print_state(*end_state);printf("\n");
               
                /* -----------------------------------------------*/
                mff_bridge(start_state, end_state);
                mff_record_esp_sol();
                
                var_source_to_dest(start_state, &GpG.sol_state);
                var_source_to_dest(&ginitial_state, &GpG.sol_state);
        }
	}						

    if(!GpG.is_deripred) {
        load_lpg_gef_conn();
        output_esp_solution(plan_actions);
    }    
    return 1;   
}

int
restart_distribute_MFF (State * start_state, State * end_state,
	     PlanAction ** plan_actions)
{
	int i,j,k,tp;
	State saved_end_state, for_twist_state;
   
    load_ff_gef_conn();

    GpG.total_num_subprob = 0;
    tp =0;
    source_to_dest(&for_twist_state, end_state); 
    
    init_h1_h2();
 
  while(1) {
      
    GpG.num_esp_sol = 0; 
    source_to_dest(&saved_end_state, end_state); 
            
	for (j = 0; j < saved_end_state.num_F; j++)	     
	{
        GpG.total_num_subprob++;
        
        if(contain_fact_state(start_state,  saved_end_state.F[j])) continue;
        find_intermediate_goals(start_state, saved_end_state.F[j]);
        
        // get known_iga_list
        dis_known_iga_list.num_F = 0;
        for(k=j+1; k<saved_end_state.num_F; k++) {
            dis_known_iga_list.F[dis_known_iga_list.num_F++] 
                = saved_end_state.F[k];
        }
        
        for(i=0; i<j; i++) end_state->F[i] = saved_end_state.F[i];
        for(i=0; i<GpG.num_sub_agenda_IMG; i++) {
          
               tp ++;  
               //printf("\nstart state ");print_state(*start_state);printf("\n");
                                
                /* end state */
                for(k=0; k<GpG.sub_agenda_IMG[i].num_F;k++) {        
                    end_state->F[j+k] =  GpG.sub_agenda_IMG[i].F[k];
                }
                end_state->num_F = j + GpG.sub_agenda_IMG[i].num_F ;
                source_to_dest(&ggoal_state, end_state); 
	            //printf("\nend state ");print_state(*end_state);printf("\n");
               
                /* -----------------------------------------------*/
                mff_bridge(start_state, end_state);
                mff_record_esp_sol();

                printf("mff_bridge solved or not? gnum_op = %d\n",dis_gnum_plan_ops);
               
               if(dis_gnum_plan_ops>=0){ 
                    var_source_to_dest(start_state, &GpG.sol_state);
                    var_source_to_dest(&ginitial_state, &GpG.sol_state);
               }
        }
	}		

    if(dis_gnum_plan_ops >=0) break;    
      source_to_dest(end_state, &for_twist_state);
      random_twist_goal_order(end_state);
  }
    if(!GpG.is_deripred) {
        load_lpg_gef_conn();
        output_esp_solution(plan_actions);
    }    
    return 1;   
}

int
staged_distribute_MFF (State * start_state, State * end_state,
	     PlanAction ** plan_actions)
{
	int i,j,k,tp;
	State saved_end_state;
   
    load_ff_gef_conn();

    GpG.total_num_subprob = 0;
    GpG.num_esp_sol = 0; 
    tp =0;
    
    if(ComputeMutex) twist_goal_order(end_state);
    source_to_dest(&saved_end_state, end_state); 
   
	for (j = 0; j < saved_end_state.num_F; j++)	     
	{
        GpG.total_num_subprob++;
        
        if(contain_fact_state(start_state,  saved_end_state.F[j])) continue;
        find_intermediate_goals(start_state, saved_end_state.F[j]);
        
        // get known_iga_list
        dis_known_iga_list.num_F = 0;
        for(k=j+1; k<saved_end_state.num_F; k++) {
            dis_known_iga_list.F[dis_known_iga_list.num_F++] 
                = saved_end_state.F[k];
        }
        
        for(i=0; i<GpG.num_sub_agenda_IMG; i++) {
          
               tp ++;  
               //printf("\nstart state ");print_state(*start_state);printf("\n");
                                
                /* end state */
                for(k=0; k<GpG.sub_agenda_IMG[i].num_F;k++) {        
                    end_state->F[k] =  GpG.sub_agenda_IMG[i].F[k];
                }
                end_state->num_F =  GpG.sub_agenda_IMG[i].num_F ;
                source_to_dest(&ggoal_state, end_state); 
	            //printf("\nend state ");print_state(*end_state);printf("\n");
               
                /* -----------------------------------------------*/
                mff_bridge(start_state, end_state);
                mff_record_esp_sol();
                
                var_source_to_dest(start_state, &GpG.sol_state);
                var_source_to_dest(&ginitial_state, &GpG.sol_state);
        }
	}						

    if(!GpG.is_deripred) {
        load_lpg_gef_conn();
        output_esp_solution(plan_actions);
    }    
    return 1;   
}

void staged_subgoal_MFF(State * start_state, State * end_state,
                 PlanAction ** plan_actions)
{
    int j, k;
    State saved_end_state;
    
    load_ff_gef_conn();
    if(ComputeMutex) twist_goal_order(end_state);
    source_to_dest(&saved_end_state, end_state); 

    var_source_to_dest(start_state, &saved_ginitial_state);
    GpG.num_esp_sol = 0; 
    
    for (j = 0; j < saved_end_state.num_F; j++)
    {
        GpG.total_num_subprob++;
        end_state->F[0] = saved_end_state.F[j];
        end_state->num_F = 1;
        
   //     printf("start state\n"); print_state(*start_state);
     //   printf("\nend state\n"); print_state(*end_state);
        
        if(contain_fact_state(start_state, saved_end_state.F[j])) continue;
        
        // get known_iga_list
        dis_known_iga_list.num_F = 0;
        for(k=j+1; k<saved_end_state.num_F; k++) {
            dis_known_iga_list.F[dis_known_iga_list.num_F++] 
                = saved_end_state.F[k];
        }
    
        mff_bridge(start_state, end_state);
        mff_record_esp_sol();
   
        var_source_to_dest(start_state, &GpG.sol_state);
        var_source_to_dest(&ginitial_state, &GpG.sol_state);
    }   
    if(!GpG.is_deripred) {
        load_lpg_gef_conn();
        output_esp_solution(plan_actions);    
    }
}


Bool symm_solve_connect(State * start_state, State *solved, int goal,
                 PlanAction ** plan_actions)
{
    int i;
   State end_state;
                                 
  //  printf("====>solve connect goal"); print_ft(goal); 
    end_state.num_F = 0;
        for(i=0; i<solved->num_F; i++) end_state.F[end_state.num_F++] = solved->F[i];
        end_state.F[end_state.num_F++] = goal;

        gsolved_state = solved; 
        mff_bridge(start_state, &end_state);
        mff_record_esp_sol();
    
        if(dis_gnum_plan_ops<0) {
    //            printf("connect failed\n");
                return FALSE;
        }
        
        var_source_to_dest(start_state, &GpG.sol_state);
        var_source_to_dest(&ginitial_state, &GpG.sol_state);
  //      printf("connect succeded\n");
        return TRUE;
}
        
Bool symm_solve_subgoal(State * start_state, State *solved, int subgoal,
                 PlanAction ** plan_actions)
{
    int i,gg, goal = -100;
//    printf("---solve subgoal"); print_ft(subgoal); 
    while(1) {
        gg = symm_goal_eval(start_state, subgoal);
        if(gg<0) {printf("exit 100");exit(0);}
        for(i=0; i<SymmetryGroup[gg].size; i++) {
            goal = SymmetryGroup[gg].g[i];
            if(gft_conn[goal].num_add == gft_conn[subgoal].num_add) {
                break;
            }
        }
        if(!symm_solve_connect(start_state, solved, goal, plan_actions)) {
            return FALSE;
        }
        if(goal == subgoal) break; 
    }   
    return TRUE;
}
    
Bool reorder_symmetry_subgoal_MFF(State * start_state, State * end_state,
                 PlanAction ** plan_actions)
{
    int reo, i, j, goal;
    State saved_end_state, S, visit;
    Bool succ;

    // initialization 
    load_ff_gef_conn();
    source_to_dest(&saved_end_state, end_state); 
    
    // parameters
    dis_red_space = 0;
    max_hc_iter = 10000; 
    max_bfs_iter = 2;
    bridge_option = 0;
    SymmLagrange = 10;
   
    
    // build Symmetry 
    genIMA_analysis(start_state, rsa_facts, rsa_actions, decr_g);
    for(i=0; i<gnum_symm_group; i++) {
        for(j=0; j<gnum_symm_group; j++) {
            syg_degree[i][j] = -1;
        }
    }

    if((SymmCompart) && (gnum_symm_norm==0)) {
        return FALSE;        
    }
    SymmLagrangian = 1;
    SymmConstrEvalOption = 0;

    // constraint evaluation option
 for(reo=0; reo<10; reo++) {
                
        printf("reo %d\n", reo);
     // search                                               
    var_source_to_dest(start_state, &saved_ginitial_state);
    GpG.num_esp_sol = 0; 
    visit.num_F = 0 ;
    while(1) {
        goal = -1; S.num_F = 0;
        for(i=0; i<saved_end_state.num_F; i++) {
             if(!contain_fact_state(start_state, saved_end_state.F[i])) {
                goal = saved_end_state.F[i];
             } else {
                S.F[S.num_F++] = saved_end_state.F[i]; 
             }
        }
        if(goal<0) {
            succ = TRUE;
      //      printf("\nSolved.\n");
            break;
        }
        if(!symm_solve_subgoal(start_state, &S, goal, plan_actions)) {
            succ = FALSE;
            break;
        }
       // printf("--- subgoal solved ---");
    }  

    if(succ) {
        break;
    }
    
    dis_reorder_saved_goal(&saved_end_state);
 }
  
  if(!succ) {
      exit(0);
  }

  return TRUE;
}

Bool symmetry_subgoal_MFF(State * start_state, State * end_state,
                 PlanAction ** plan_actions)
{
    int i, j, goal;
    State saved_end_state, S, visit;
    Bool succ;
    
    /*
    // MFF construction
    dis_MFF_main(gops_file, gfct_file); 
    printf("disg %d %d\n", dis_gnum_ft_conn, dis_gnum_op_conn);
    lpg_mff_facts();
    */
    

    // initialization 
    load_ff_gef_conn();
    source_to_dest(&saved_end_state, end_state); 
    
    // parameters
    dis_red_space = 0;
    max_hc_iter = 100000; 
    max_bfs_iter = 10;
    bridge_option = 0;
    SymmLagrange = 10;
   
    // UPLOAD 11
    if(gnum_ft_conn >= 1000) {
        max_hc_iter = 30000;
    }
    
    // build Symmetry 
    genIMA_analysis(start_state, rsa_facts, rsa_actions, decr_g);
    for(i=0; i<gnum_symm_group; i++) {
        for(j=0; j<gnum_symm_group; j++) {
            syg_degree[i][j] = -1;
        }
    }

    if((SymmCompart) && (gnum_symm_norm==0)) {
        return FALSE;        
    }
    
    SymmLagrangian = 1;
        
    // constraint evaluation option
    
  for(SymmConstrEvalOption = 0; SymmConstrEvalOption <=5;
      SymmConstrEvalOption ++) {
    
    //printf("try Option = %d\n", SymmConstrEvalOption );      
    printf("\n");      
    // search                                               
    var_source_to_dest(start_state, &saved_ginitial_state);
    GpG.num_esp_sol = 0; 
    visit.num_F = 0 ;
    while(1) {
        goal = -1; S.num_F = 0;
        for(i=0; i<saved_end_state.num_F; i++) {
             if(!contain_fact_state(start_state, saved_end_state.F[i])) {
                goal = saved_end_state.F[i];
             } else {
                S.F[S.num_F++] = saved_end_state.F[i]; 
             }
        }
        if(goal<0) {
            succ = TRUE;
      //      printf("\nSolved.\n");
            break;
        }
        if(!symm_solve_subgoal(start_state, &S, goal, plan_actions)) {
            succ = FALSE;
            break;
        }
       // printf("--- subgoal solved ---");
    }  

    if(succ) {
        break;
    }
  }
  
  if(!succ) {
      exit(0);
  }

  return TRUE;
} 

/*        
mff_start_and_goal(start_state, end_state);
    print_state(*start_state);
    for(i=0; i<end_state->num_F; i++) {
     if(!contain_fact_state(start_state, end_state->F[i])) {
        symm_constraint_eval(&dis_ginitial_state, end_state->F[i]); 
        printf("\n");
     }      
    }
  */  

int 
DistributeSearch (State * start_state, State * end_state,
                 PlanAction ** plan_actions)
{
   int i, j;  
   enum dStrategy distribute_strategy;
  
   printf("\n\nStarting SGPlan search ...\n");
	fflush(stdout);
   
   //=============== Step 0: initialize ================  
   if(gcmd_line.display_info == 447) {
       printf("LPG numerical var = %d\n", gnum_comp_var);
       printf("\nDistributed Search Step 0: initialize\n");
   }
   
   // Step 0.0: checking  
   if(MAX_OP_NUM <= gnum_ef_conn) {
              printf("MAX_OP_NUM too small! set a value > %d and try again.\n", gnum_ef_conn);
              exit(1);
   }    
      
   if(MAX_FT_NUM <= gnum_ft_conn) {
       printf("MAX_FT_NUM too small! set a larger value and try again.\n");
       exit(1);
   }    
         
   // Step 0.1: modify_ft_ef_mutex
   if(ComputeMutex) modify_ft_ef_mutex();
    g_plan_actions = plan_actions;  
        
   // Step 0.2: set_dummy_fid
    srandom(seed);
    srand(seed);
   set_dummy_fid();
   
   // Step 0.3: op_in_graph stuff   // ?
   GpG.op_in_graph = (int *) calloc(gnum_ef_conn, sizeof( int ));     
   GpG.op_pure_numeric = (int *) calloc(gnum_ef_conn, sizeof( int ));
   for(i=0; i<gnum_ef_conn; i++) GpG.op_in_graph[i] = 1;
   pure_numerical_actions(); 
   
   // Step 0.4: save ginitial_state and ggoal_state before search 
   var_source_to_dest(&saved_ginitial_state, &ginitial_state);
   var_source_to_dest(&saved_ggoal_state, &ggoal_state);
   var_source_to_dest(start_state, &ginitial_state);
   var_source_to_dest(&GpG.sol_state, &ginitial_state);  
   
   // Step 0.5 init known_iga
   known_iga_list.num_F = 0; 
   
   // Step 0.6: Necessary Initialization
   if(gcmd_line.display_info == 447) {
        printf("\nDistributed Search Step 1: IGA graph analysis\n");
   }
   decr_g = ( int * ) calloc( gnum_ft_conn, sizeof( int ) );
   init_subspace_actions();
   
   // Step 0.7: strips ff initialize
   save_lpg_gef_conn();
   strips_gef_conn();
  
   // Step 1.0 :construction for mff
   
   load_ff_gef_conn();
   construct_mff();
   
   // Step 1.5 default parameters
   dis_red_space = 0;
   bridge_option = 0;
   max_bfs_iter = 10; 
   max_hc_iter = 10000; 

   //Step 2: Deciding strategy
  
   habakuk_path(gcmd_line.fct_file_name);
   
   if(GpG.SearchModal == 0) {
        test_IMA(start_state, end_state, plan_actions);
        exit(0); 
   }
   if(GpG.SearchModal == 105) {
        test_IMA(start_state, end_state, plan_actions);
        exit(0); 
   }
   if((GpG.SearchModal == 1) || (GpG.SearchModal == 2)) {
        if(GpG.SecondaryModal ==0 ) { 
            subgoal_MFF(start_state, end_state, plan_actions);
            store_sequential_plan(gcmd_line.fct_file_name);
            exit(0);
        }
        if(GpG.SecondaryModal ==1 ) { 
            subpath_subgoal_MFF(start_state, end_state, plan_actions);            
            store_sequential_plan(gcmd_line.fct_file_name);
            //   direct_MFF(start_state, end_state, plan_actions);
            exit(0);    
        }
   } 
	// Chih-Wei
   if(GpG.SearchModal == 3) {
	if (!GpG.is_til)
	{
        //if(reorder_symmetry_subgoal_MFF(start_state, end_state, plan_actions)) {
        /* LSR */
        if(symmetry_subgoal_MFF(start_state, end_state, plan_actions)) {
          dis_print_IPC5_modal3_GpG(gcmd_line.fct_file_name);
//            store_sequential_plan(gcmd_line.fct_file_name);
            exit(0);
        } else {
            max_bfs_iter = 1000000; 
            max_hc_iter = 1000000; 
            direct_MFF_symmetry(start_state, end_state, plan_actions);
        }
          exit(0);
        }
        else
        {
    
        load_lpg_gef_conn();
	notdis_init_timed_initial();
	max_hc_iter = 100000;
	max_bfs_iter = 100000;
	dis_known_iga_list.num_F = 0;
	esp1(start_state, end_state);
	store_sequential_plan(gcmd_line.fct_file_name);
        exit(0);
        }
   }
   if(GpG.SearchModal == 4) {
    if(GpG.is_deripred) {
       dis_red_space = 1;
        bridge_option = 1;
        subgoal_MFF(start_state, end_state, plan_actions);
        store_sequential_plan(gcmd_line.fct_file_name);
        exit(0);
    } else {
        direct_MFF(start_state, end_state, plan_actions);
        store_sequential_plan(gcmd_line.fct_file_name);
        exit(0);
    }
   }
  /* 
   if(GpG.SearchModal == 6) {
        subgoal_MFF(start_state, end_state, plan_actions);
        store_sequential_plan(gcmd_line.fct_file_name);
        exit(0);
   }*/ 
   
                                
   // Step 2.1: strategic deployment
   //dis_red_space = 1;  
           
   
   // Step 2.2 search    
   //direct_MFF(start_state, end_state, plan_actions);
   //subgoal_MFF(start_state, end_state, plan_actions);
   //symmetry_subgoal_MFF(start_state, end_state, plan_actions);
   //distribute_MFF(start_state, end_state, plan_actions);
   //restart_distribute_MFF(start_state, end_state, plan_actions);
   //staged_distribute_MFF(start_state, end_state, plan_actions);
   //staged_subgoal_MFF(start_state, end_state, plan_actions);
   
   
   // Step 2.3 result
   if((!ComputeMutex) || (GpG.is_deripred)) {
        store_sequential_plan(gcmd_line.fct_file_name);
        printf("\n\n");
   }
   
   // general_IMA(start_state, end_state, plan_actions);
   
   //dfs_iga_MFF(start_state, end_state, plan_actions);
   exit(0);   
  
   
   /*
   max_outd = max_out_iga_graph(&saved_ginitial_state, rsa_facts, rsa_actions, decr_g);
   if(max_outd == 0) {
        distribute_strategy = LPG_DIS;
   } else {
        distribute_strategy = FF_DIS;
   }
   */

 //  distribute_strategy = LPG_DIS;
  // distribute_strategy = HIBURY_DIS;
//        distribute_strategy = FF_DIS;
   
  
   // Step 3: Carry out the search strategy
//  distribute_IGA(start_state, end_state, plan_actions);
//    distribute_ima_LPG(start_state, end_state, plan_actions);
    //distribute_LPG(start_state, end_state, plan_actions);
                


	printf("distribute_strategy\n");  
   switch(distribute_strategy) {
        case LPG_DIS:
            printf("\nCarry out the  LPG_DIS search strategy.\n");
            GpG.esp_search = 1;
            distribute_LPG(start_state, end_state, plan_actions);
            break;  
        case FF_DIS:
            printf("\nCarry out the  FF_DIS search strategy.\n");
            GpG.esp_search = 2;
            distribute_FF(start_state, end_state, plan_actions);
            break;      
        case HIBURY_DIS:
            printf("\nCarry out the  HIBURY_DIS search strategy.\n");
            GpG.esp_search = 2;
            distribute_Hibury(start_state, end_state, plan_actions);
            break;      
   }
}

/*--------------------------------------------------------------------
 *  mff parser solver below
 *----------------------------------------------------------------*/
void print_logic_goals()
{
    int j;
    for ( j = 0; j < dis_gnum_flogic_goal; j++ ) {
        printf("\nmyGoal %d", j);
        dis_print_ft_name( dis_gflogic_goal[j] );
    }                          
    printf("\n"); 
}   
void print_fl_goals()
{
    int j;
    for ( j = 0; j < dis_gnum_fnumeric_goal; j++ ) {
        printf("\nmyGoal %d", j);
        dis_print_fl_name( dis_gfnumeric_goal_fl[j] );
        if ( dis_gfnumeric_goal_comp[j] == GEQ ) {
            printf(" >= ");
        } else {
            printf(" > ");
        }
        printf("%f", dis_gfnumeric_goal_c[j]);
    } 
    printf("\n"); 
}   

Bool mff_direct()
{
   dis_Bool found_plan;  
  
   //dis_print_dis_State(dis_ginitial_state);
   
   if ( dis_gcmd_line.ehc ) {
            found_plan = dis_do_enforced_hill_climbing();
            if ( !found_plan ) {
     //           printf("\n\nEnforced Hill-climbing failed !");
     //           printf("\nswitching to Best-first Search now.\n");
               found_plan = dis_do_best_first_search();
            }
   } else {
        found_plan = dis_do_best_first_search();
   }
    printf("\n");
    return found_plan;
}

Bool dis_all_goal_contained(dis_State *S)
{
    int i,j,f;
    for(j=0; j<saved_dis_gnum_flogic_goal; j++) {
        f = saved_dis_gflogic_goal[j];
        for(i=0; i<S->num_F; i++) {
            if(S->F[i] == f) break;
        }
        if(i==S->num_F) return FALSE;
    }
    return TRUE;
}

void mff_IGA(int fact)
{
    vState needf;
    State start, stop;
    int i;
    
   start.num_F = dis_ginitial_state.num_F;
   for(i=0; i<dis_ginitial_state.num_F; i++) {
        start.F[i] = dis_ginitial_state.F[i];
   } 
   stop.num_F = 1;
   stop.F[0] = fact;   
      
   build_iga_graph(&start, rsa_facts, rsa_actions, decr_g);  
   precede_facts_of_state(&stop, &needf); 
   
   dis_fout_vState(&needf, "needf");
}

Bool mff_subgoal_reverse()
{
   dis_Bool found_plan = FALSE;
    int j, k;
    
    GpG.num_esp_sol = 0; 
    dis_gnum_fnumeric_goal = 0;
   
    max_hc_iter = 1; 
    max_bfs_iter = 30000; 
    //dis_gcmd_line.h_weight = 8;
   
    for (j = saved_dis_gnum_flogic_goal-1; j>=0; j--)
    {
        //dis_print_dis_State(dis_ginitial_state);
       /*
        printf("\n%d ",saved_dis_gnum_flogic_goal-j);
        dis_print_ft_name(saved_dis_gflogic_goal[j]);
        printf("\n ");
        */
            

        //mff_IGA(saved_dis_gflogic_goal[j]);
        dis_gflogic_goal[saved_dis_gnum_flogic_goal-j-1] = saved_dis_gflogic_goal[j];
        dis_gnum_flogic_goal = saved_dis_gnum_flogic_goal-j;
                    
        
       //   dis_gnum_flogic_goal = 1;
       // dis_gflogic_goal[0] = saved_dis_gflogic_goal[j];
                                            
        // get known_iga_list
        dis_known_iga_list.num_F = 0;
        for(k=j+1; k<saved_dis_gnum_flogic_goal; k++) {
            dis_known_iga_list.F[dis_known_iga_list.num_F++] 
                = saved_dis_gflogic_goal[k];
        }
    
//        dis_do_best_first_search(); 
        dis_mff_bridge();
        mff_record_esp_sol();
       
        
        if(dis_gnum_plan_ops>0) { 
            dis_source_to_dest(&dis_ginitial_state, &dis_mff_sol);
        }
    }                                   
       
    //printf("\nnumeric part\n\n");
             
    for (j = 0; j < saved_dis_gnum_fnumeric_goal; j++)
    {
         
        dis_gnum_fnumeric_goal = j+1;
        dis_gfnumeric_goal_comp[j] = saved_dis_gfnumeric_goal_comp[j];
        dis_gfnumeric_goal_fl[j] = saved_dis_gfnumeric_goal_fl[j];
        dis_gfnumeric_goal_c[j] = saved_dis_gfnumeric_goal_c[j];
        //print_fl_goals();
                                    
        dis_known_iga_list.num_F = 0;
        
        dis_mff_bridge();
        mff_record_esp_sol();
   
        dis_source_to_dest(&dis_ginitial_state, &dis_mff_sol);
    }
	return found_plan;    
}
Bool mff_subgoal()
{
   dis_Bool found_plan = FALSE;
    int j, k;
    dis_State saved_dis_ginitial_statex;
    
    dis_copy_dis_source_to_dest(&saved_dis_ginitial_statex, &dis_ginitial_state);
    dis_copy_dis_source_to_dest(&dis_mff_sol, &dis_ginitial_state);
    GpG.num_esp_sol = 0; 
    dis_gnum_fnumeric_goal = 0;
   
    max_hc_iter = 10000; 
    max_bfs_iter = 30000; 
    //dis_gcmd_line.h_weight = 8;
   
    for (j = 0; j < saved_dis_gnum_flogic_goal; j++)
    {
        //dis_print_dis_State(dis_ginitial_state);
      
        /*
        printf("\n%d ",j);
        dis_print_ft_name(saved_dis_gflogic_goal[j]);
        printf("\n ");
        */

        //mff_IGA(saved_dis_gflogic_goal[j]);

        dis_gflogic_goal[j] = saved_dis_gflogic_goal[j]; 
        dis_gnum_flogic_goal = j+1;
        
       //   dis_gnum_flogic_goal = 1;
       // dis_gflogic_goal[0] = saved_dis_gflogic_goal[j];
                                            
        // get known_iga_list
        dis_known_iga_list.num_F = 0;
        for(k=j+1; k<saved_dis_gnum_flogic_goal; k++) {
            dis_known_iga_list.F[dis_known_iga_list.num_F++] 
                = saved_dis_gflogic_goal[k];
        }
   
        dis_gnum_plan_ops = 0;
        dis_mff_bridge();
        mff_record_esp_sol();
       
        if(dis_gnum_plan_ops>0) { 
            dis_source_to_dest(&dis_ginitial_state, &dis_mff_sol);
        }
    }                                   
       
    //printf("\nnumeric part\n\n");
             
    for (j = 0; j < saved_dis_gnum_fnumeric_goal; j++)
    {
         
        dis_gnum_fnumeric_goal = j+1;
        dis_gfnumeric_goal_comp[j] = saved_dis_gfnumeric_goal_comp[j];
        dis_gfnumeric_goal_fl[j] = saved_dis_gfnumeric_goal_fl[j];
        dis_gfnumeric_goal_c[j] = saved_dis_gfnumeric_goal_c[j];
        //print_fl_goals();
                                    
        dis_known_iga_list.num_F = 0;
        
        dis_mff_bridge();
        mff_record_esp_sol();
   
        dis_source_to_dest(&dis_ginitial_state, &dis_mff_sol);
    }
    if (GpG.SearchModal != 5)
    dis_copy_dis_source_to_dest(&dis_ginitial_state, &saved_dis_ginitial_statex);
    
    return found_plan;
}

int compare (const void * a, const void * b)
{
  return ( *(int*)a - *(int*)b );
}
int compare_float (const void * a, const void * b)
{
  if (*(float*)a > *(float*)b )
    return 1;
  if (*(float*)a < *(float*)b )
    return -1;
  return 0;
}
int dis_get_1P(dis_State *);

extern float *relax_time_fact, *relax_time_action;
extern int *support_action, *last_precondition;

Bool mff_subgoal_satellite()
{
  int i, j, k;
  int *saved_goal, saved_num;
  int *order, *done, *nra;
  float **distance, schedule[2000];
  float *span, *span0, *hv, UB = 1000;
  int N, **origaction, **nextaction, *node;
  dis_Action *action;
  Bool ret = 0;
  dis_State saved_dis_ginitial_state;
  char temp1[64], temp2[64];
  
  for (i=0;i<dis_gnum_ef_conn;i++)
    dis_gef_conn[i].duration = gef_conn[i].duration;
  init_timed_initial();
  saved_num = saved_dis_gnum_flogic_goal;
  saved_goal = (int *) malloc(saved_dis_gnum_flogic_goal*sizeof(int));
  memcpy(saved_goal, saved_dis_gflogic_goal, saved_dis_gnum_flogic_goal*sizeof(int));
  dis_copy_dis_source_to_dest(&saved_dis_ginitial_state, &dis_ginitial_state);

  hv = (float *) malloc(sizeof(float)*saved_num);
  span = (float *) malloc(sizeof(float)*saved_num);
  span0 = (float *) malloc(sizeof(float)*saved_num);
  order = (int *) malloc(sizeof(int)*saved_num);
  done = (int *) malloc(sizeof(int)*saved_num);
  nra = (int *) malloc(sizeof(int)*saved_num);

  for ( i = 0; i < dis_gnum_types; i++ ) 
    if (strcmp(dis_gtype_names[i], "DIRECTION") == 0)
    {
      N = dis_gtype_size[i];
      distance = (float **) malloc(sizeof(float *)*N);
      nextaction = (int **) malloc(sizeof(int *)*N);
      origaction = (int **) malloc(sizeof(int *)*N);
      node = (int *) malloc(sizeof(int)*N);
      for (j=0;j<dis_gtype_size[i];j++)
      {
        node[j] = dis_gtype_consts[i][j];  
        distance[j] = (float *) malloc(sizeof(float)*N);
        nextaction[j] = (int *) malloc(sizeof(int)*N);
        origaction[j] = (int *) malloc(sizeof(int)*N);
        distance[j][j] = dis_BIG_INT;
        nextaction[j][j] = origaction[j][j] = -1;
      }
      break;
    }
 
  for (i=0;i<dis_gnum_ef_conn;i++)
  {
    action = dis_gop_conn[dis_gef_conn[i].op].action;
    if (strcmp(action->name, "TURN_TO") != 0)
      continue;
    for (j=0;j<N;j++)
      if (node[j] == action->name_inst_table[1])
        break;
    for (k=0;k<N;k++)
      if (node[k] == action->name_inst_table[2])
        break;
    distance[j][k] = dis_gef_conn[i].duration;
    nextaction[j][k] = i;
    origaction[j][k] = i;
  }

  for (i=0;i<N;i++)
    for (j=0;j<N;j++)
      for (k=0;k<N;k++)
        if (distance[j][k] > distance[j][i] + distance[i][k])
        {
          distance[j][k] = distance[j][i] + distance[i][k];
          nextaction[j][k] = nextaction[j][i];
        }
        
/*  for (j=0;j<N;j++)
    for (k=0;k<N;k++)
      dis_gef_conn[origaction[j][k]].duration = distance[j][k];*/

  relax_time_fact = (float *) malloc(sizeof(float)*dis_gnum_ft_conn);   
  relax_time_action = (float *) malloc(sizeof(float)*dis_gnum_ef_conn); 
  support_action = (int *) malloc(sizeof(int)*dis_gnum_ft_conn);        
  last_precondition = (int *) malloc(sizeof(int)*dis_gnum_ef_conn);
/*  GpG.num_esp_sol = 0;
  max_bfs_iter = 100000;
  max_hc_iter = 100000;
  bridge_option = 0;

  dis_gnum_flogic_goal = 1;
  for (i=0;i<saved_num;i++)
  {
    dis_print_ft_name_string(saved_goal[i], temp1);
    if (strncmp(temp1+1, "POINTING", 8) == 0) 
    {
      nra[i] = UB;
      hv[i] = UB*UB*UB;
    }
    else
    {    
      k = GpG.num_esp_sol;
      dis_gflogic_goal[0] = saved_goal[i];
      dis_mff_bridge();
      if (dis_gnum_plan_ops > 0)
      {
        mff_record_esp_sol();
        if (myPERT(GpG.num_esp_sol, k, GpG.esp_solution, schedule) != -1)
        {
          fprintf(stderr, "PERT failed\n");
          exit(0);
        }
        
        nra[i] = dis_gnum_plan_ops;
        span[i] = schedule[GpG.num_esp_sol-1];
        for (j=GpG.num_esp_sol-2;j>k;j--)
          if (strncmp(dis_gop_conn[GpG.esp_solution[j]].action->name, "TAKE_IMAGE", 10) == 0)
            break;
        span0[i] = schedule[j] + dis_gef_conn[GpG.esp_solution[j]].duration;
        hv[i] = (nra[i]*UB + span[i])*UB + span0[i];
      }
      GpG.num_esp_sol = k;
    }
    fprintf(stderr, "%d %f %f\n", nra[i], span[i], span0[i]);
  }
  return ret;

  saved_dis_gnum_flogic_goal = 0;
  for (k=0;k<dis_gnum_ft_conn;k++)
  {
    dis_print_ft_name_string(k, temp1);
    if (strncmp(temp1+1, "CALIBRATED", 10) == 0)
      saved_dis_gflogic_goal[saved_dis_gnum_flogic_goal++] = k;
  }
  bridge_option = 0;
  mff_subgoal();*/
  saved_dis_gnum_flogic_goal = saved_num;
  memcpy(saved_dis_gflogic_goal, saved_goal, sizeof(int)*saved_num);
  dis_gnum_flogic_goal = saved_num;
  memcpy(dis_gflogic_goal, saved_goal, sizeof(int)*saved_num);
  GpG.SearchModal = -106;
  dis_get_1P(&dis_ginitial_state);
  
  for (j=0;j<saved_dis_gnum_flogic_goal;j++)
  {
    order[j] = j;
    dis_print_ft_name_string(saved_dis_gflogic_goal[j], temp1);
    if (strncmp(temp1+1, "POINTING", 8) == 0) 
      hv[j] = dis_BIG_INT;
    else
    {
      strncpy(temp1+1, "HAVE", 4);
      for(k=0; k<dis_gnum_ft_conn; k++) 
      {
        dis_print_ft_name_string(k, temp2);
        if (strcmp(temp1, temp2) == 0)
          break;
      }
      hv[j] = relax_time_fact[k];             
    }
    fprintf(stderr, "%f %d ", hv[j], k);
    fprintf(stderr, "%s\n", temp1);
  }
//  fprintf(stderr, "\n");
  
  GpG.SearchModal = 6;
  free(relax_time_fact);
  free(relax_time_action);
  free(support_action);
  free(last_precondition);
  return ret;

  GpG.num_esp_sol = 0;
  max_bfs_iter = 100000;
  max_hc_iter = 100000;
  bridge_option = 0;
  GpG.SearchModal = -106;

  for (i=0;i<N;i++)
  {
    free(distance[i]);
    free(nextaction[i]);
    free(origaction[i]);
  }
  free(distance);
  free(nextaction);
  free(origaction);
  free(node);
  
  free(hv);
  free(span);
  free(span0);
  free(nra);
  free(done);
  free(order);
  free(saved_goal);
  return ret;                                                                         
}

Bool mff_subgoal_order()
{
   dis_Bool found_plan = FALSE;
    int j, k;
    int *hv = (int *) malloc(sizeof(int)*saved_dis_gnum_flogic_goal);
    int *order = (int *) malloc(sizeof(int)*saved_dis_gnum_flogic_goal);
    dis_State saved_dis_ginitial_state;
    
    dis_copy_dis_source_to_dest(&saved_dis_ginitial_state, &dis_ginitial_state);
    GpG.num_esp_sol = 0; 
    dis_gnum_fnumeric_goal = 0;
    max_bfs_iter = 30000; 
    bridge_option = 0;
   
    dis_gnum_flogic_goal = 1;
    for (j=0;j<saved_dis_gnum_flogic_goal;j++)
    {
      order[j] = j;
      dis_gflogic_goal[0] = saved_dis_gflogic_goal[j];
      hv[j] = dis_get_1P(&dis_ginitial_state);
      order[j] += hv[j]*saved_dis_gnum_flogic_goal;
      fprintf(stderr, "%d ", hv[j]);
    }
    fprintf(stderr, "\n");
    qsort(order, saved_dis_gnum_flogic_goal, sizeof(int), compare);
    for (j=0;j<saved_dis_gnum_flogic_goal;j++)
      order[j] %= saved_dis_gnum_flogic_goal;

    for (j = 0; j < saved_dis_gnum_flogic_goal; j++)
    {
        dis_gflogic_goal[j] = saved_dis_gflogic_goal[order[j]]; 
        dis_gnum_flogic_goal = j+1;
        dis_known_iga_list.num_F = 0;
        for(k=j+1; k<saved_dis_gnum_flogic_goal; k++)
            dis_known_iga_list.F[dis_known_iga_list.num_F++] 
                = saved_dis_gflogic_goal[order[k]];
        max_hc_iter = hv[order[j]]*10;
    
        dis_mff_bridge();
        mff_record_esp_sol();
       
        if(dis_gnum_plan_ops>0) { 
            dis_source_to_dest(&dis_ginitial_state, &dis_mff_sol);
        }
        else
          if (j == saved_dis_gnum_flogic_goal - 1)
            exit(0);
    }                                   
       
    dis_copy_dis_source_to_dest(&dis_ginitial_state, &saved_dis_ginitial_state);
    free(order);
    free(hv);
    return found_plan;
} 

void mff_fluent_subgoal()
{
    int pre_ops[10000];    
    int num_pre_ops;
    int iter,i,j;
    State S;
    dis_State saved_dis_ginitial_state;
    float Relax=100, Relax1=18, Relax2=8;
    float *fl_relax;            
        
    fl_relax = (float*)calloc(dis_gnum_relevant_fluents, sizeof(float));
    for(i=0; i<dis_gnum_relevant_fluents; i++) fl_relax[i]= 0;
                                                        
    dis_known_iga_list.num_F = 0;
    dis_copy_dis_source_to_dest(&saved_dis_ginitial_state, &dis_ginitial_state);
             
    dis_collect_fluent(); 
   
    relax_1_easy_resource(&S);  
    for(i=0; i<S.num_F; i++) fl_relax[S.F[i]] = Relax;
    relax_2_easy_resource(&S);  
    for(i=0; i<S.num_F; i++) fl_relax[S.F[i]] = Relax2;
    relax_1_hard_resource(&S);
    for(i=0; i<S.num_F; i++) fl_relax[S.F[i]] = Relax1;
    relax_2_hard_resource(&S);
    for(i=0; i<S.num_F; i++) fl_relax[S.F[i]] = Relax2;
    relax_3_hard_resource(&S);
    for(i=0; i<S.num_F; i++) fl_relax[S.F[i]] = Relax2;
                    
 for(iter=0; iter<2; iter++) { 
    num_pre_ops=0;
    GpG.num_esp_sol = 0; 
    dis_source_to_dest(&dis_ginitial_state, &saved_dis_ginitial_state); 
                                                        
    // easy timber & stone
    relax_1_easy_resource(&S);
  
    for(i=0; i<S.num_F; i++) {
        dis_ginitial_state.f_V[S.F[i]] = fl_relax[S.F[i]];
    }                                                           

    // easy wood & coal
    relax_2_easy_resource(&S);  
    dis_gnum_flogic_goal = 0;
    for(i=0; i<S.num_F; i++) {
        dis_gnum_fnumeric_goal = 1;
        dis_gfnumeric_goal_comp[0] = GEQ;
        dis_gfnumeric_goal_fl[0] = S.F[i];
        dis_gfnumeric_goal_c[0] = 1;
            
        for(j=0; j<fl_relax[S.F[i]]; j++) {
            dis_ginitial_state.f_V[S.F[i]] = 0;
                                    
            //print_fl_goals();
            
            dis_mff_bridge();
            mff_record_esp_sol();
            dis_source_to_dest(&dis_ginitial_state, &dis_mff_sol);
        }                            
        dis_ginitial_state.f_V[S.F[i]] = 0;
    }   
    for(i=0; i<S.num_F; i++) {
        dis_ginitial_state.f_V[S.F[i]] = fl_relax[S.F[i]];
    }
    
//    return;

    
    // easy cart 
    if(fl_is_exceeding()) {   
      relax_1_easy_predicate(&S);
      for(i=0; i<S.num_F; ) {
        dis_gnum_fnumeric_goal = 0;
        dis_gnum_flogic_goal = 2;
        dis_gflogic_goal[0] = S.F[i++];
        dis_gflogic_goal[1] = S.F[i++];
            
//        print_logic_goals();
            
        dis_mff_bridge();
        mff_record_esp_sol();
        dis_source_to_dest(&dis_ginitial_state, &dis_mff_sol);
      }                               
    }

    // timber         
    relax_1_hard_resource(&S);
    dis_gnum_flogic_goal = 0;
    for(i=0; i<S.num_F; i++) {
        dis_gnum_fnumeric_goal = 1;
        dis_gfnumeric_goal_comp[0] = GEQ;
        dis_gfnumeric_goal_fl[0] = S.F[i];
        dis_gfnumeric_goal_c[0] = 1;
            
        for(j=0; j<fl_relax[S.F[i]]; j++) {
            dis_ginitial_state.f_V[S.F[i]] = 0;
                                    
            //print_fl_goals();
            
            dis_mff_bridge();
            mff_record_esp_sol();
            dis_source_to_dest(&dis_ginitial_state, &dis_mff_sol);
        }                            
        dis_ginitial_state.f_V[S.F[i]] = 0;
    }   
    for(i=0; i<S.num_F; i++) {
        dis_ginitial_state.f_V[S.F[i]] = fl_relax[S.F[i]];
    }                           
    
    // wood
    relax_2_hard_resource(&S);
    dis_gnum_flogic_goal = 0;
    for(i=0; i<S.num_F; i++) {
        dis_gnum_fnumeric_goal = 1;
        dis_gfnumeric_goal_comp[0] = GEQ;
        dis_gfnumeric_goal_fl[0] = S.F[i];
        dis_gfnumeric_goal_c[0] = 1;
            
        for(j=0; j<fl_relax[S.F[i]]; j++) {
            dis_ginitial_state.f_V[S.F[i]] = 0;
                                    
            //print_fl_goals();
            
            dis_mff_bridge();
            mff_record_esp_sol();
            dis_source_to_dest(&dis_ginitial_state, &dis_mff_sol);
        }                            
        dis_ginitial_state.f_V[S.F[i]] = 0;
    }   
    for(i=0; i<S.num_F; i++) {
        dis_ginitial_state.f_V[S.F[i]] = fl_relax[S.F[i]];
    }                                                              
    
    
    // stone
    relax_3_hard_resource(&S);
    dis_gnum_flogic_goal = 0;
    for(i=0; i<S.num_F; i++) {
        dis_gnum_fnumeric_goal = 1;
        dis_gfnumeric_goal_comp[0] = GEQ;
        dis_gfnumeric_goal_fl[0] = S.F[i];
        dis_gfnumeric_goal_c[0] = 1;
            
        for(j=0; j<fl_relax[S.F[i]]; j++) {
            dis_ginitial_state.f_V[S.F[i]] = 0;
                                    
            //print_fl_goals();
            
            dis_mff_bridge();
            mff_record_esp_sol();
            dis_source_to_dest(&dis_ginitial_state, &dis_mff_sol);
        }                            
        dis_ginitial_state.f_V[S.F[i]] = 0;
    }
    for(i=0; i<S.num_F; i++) {
        dis_ginitial_state.f_V[S.F[i]] = fl_relax[S.F[i]];
    }

    
    // save current bits
    for(i=0; i<GpG.num_esp_sol; i++){
        pre_ops[i] = GpG.esp_solution[i];    
    }
    num_pre_ops = GpG.num_esp_sol;
    
    /* 
    relax_1_hard_resource(&S);
    for(i=0; i<S.num_F; i++) {
        dis_ginitial_state.f_V[S.F[i]] = Relax;
    }   
    relax_2_hard_resource(&S);
    for(i=0; i<S.num_F; i++) {
        dis_ginitial_state.f_V[S.F[i]] = Relax;
    }                               
    relax_3_hard_resource(&S);
    for(i=0; i<S.num_F; i++) {
        dis_ginitial_state.f_V[S.F[i]] = Relax;
    } */                                                                   

    mff_subgoal(); 
    
    for(i=0; i<GpG.num_esp_sol; i++){
        pre_ops[num_pre_ops++] = GpG.esp_solution[i];    
    }
 
    for(i=0; i<dis_gnum_relevant_fluents; i++) {
    //    dis_print_fl_name( i );
    //    printf(" %f %f\n", fl_relax[i], dis_ginitial_state.f_V[i]);
        if(fl_relax[i] >0) {
            if(dis_ginitial_state.f_V[i] >1) {
                fl_relax[i] = (fl_relax[i] - dis_ginitial_state.f_V[i]) + 1;
            }                                                               
        }
    }        
 } 
    // output solution
    GpG.num_esp_sol=0;
    dis_source_to_dest(&dis_ginitial_state, &saved_dis_ginitial_state);
    relax_1_easy_resource(&S);              
    dis_gnum_flogic_goal = 0;
    for(i=0; i<S.num_F; i++) {
        dis_gnum_fnumeric_goal = 1;
        dis_gfnumeric_goal_comp[0] = GEQ;
        dis_gfnumeric_goal_fl[0] = S.F[i];
        dis_gfnumeric_goal_c[0] = 1;
            
        for(j=0; j<fl_relax[S.F[i]]-1; j++) {
            dis_ginitial_state.f_V[S.F[i]] = 0;
                                    
            //print_fl_goals();
            
            dis_mff_bridge();
            mff_record_esp_sol();
            dis_source_to_dest(&dis_ginitial_state, &dis_mff_sol);
        }                            
        dis_ginitial_state.f_V[S.F[i]] = 0;
    }   
    for(i=0; i<S.num_F; i++) {
        dis_ginitial_state.f_V[S.F[i]] = fl_relax[S.F[i]]-1;
    }
    
    // append preops
    for(i=0; i<num_pre_ops; i++) {
        GpG.esp_solution[GpG.num_esp_sol++] = pre_ops[i];
    }

  dis_source_to_dest(&dis_ginitial_state, &saved_dis_ginitial_state);
/*  for (i=0;i<GpG.num_esp_sol;i++)  
    if (dis_result_to_dest(&dis_mff_sol, &dis_ginitial_state, GpG.esp_solution[i], -1))
      dis_source_to_dest(&dis_ginitial_state, &dis_mff_sol);
    else
      exit(0);*/
}

void get_second_modal()
{
    int i;
    if(GpG.SearchModal == 6) {
        if(dis_gnum_tils>0) {
            GpG.SecondaryModal = 2;
            return;
        }
        
        for(i=0; i<dis_gnum_fl_conn; i++) {
            if(dis_fl_predlen(i) == 9) { 
                GpG.SecondaryModal = 1;
                return;
            }
        } 
        return;  
    }
}

void mff_shortest_subgoal()
{
    int i,j,k;
    
    GpG.num_esp_sol = 0; 
    dis_gnum_fnumeric_goal = 0;
  
    collect_for_shortest();
    
    for (j = 0; j < saved_dis_gnum_flogic_goal; j++)
    {
        //dis_print_dis_State(dis_ginitial_state);
             
        /*
        printf("\n%d ",j);
        dis_print_ft_name(saved_dis_gflogic_goal[j]);
        printf("\n ");
        */

        dis_gflogic_goal[j] = saved_dis_gflogic_goal[j]; 
        dis_gnum_flogic_goal = j+1;
        
        // get known_iga_list
        dis_known_iga_list.num_F = 0;
        for(k=j+1; k<saved_dis_gnum_flogic_goal; k++) {
            dis_known_iga_list.F[dis_known_iga_list.num_F++] 
                = saved_dis_gflogic_goal[k];
        }
  
        // process the sofar 
        short_proc_sofar(saved_dis_gflogic_goal[j]);
        
        // search
        dis_mff_bridge();
       
        // analyze subsolution path
        if(dis_gnum_plan_ops >0) {
            short_analyze_subpath();
            mff_record_esp_sol();
        }

        if(dis_gnum_plan_ops>=0) { 
            //dis_source_to_dest(&dis_ginitial_state, &dis_mff_sol);
            for(i=0; i<dis_gnum_plan_ops; i++) {
                dis_result_to_dest( &work_S0, &dis_ginitial_state, 
                                    dis_gplan_ops[i], -1 ) ;
                dis_source_to_dest( &dis_ginitial_state, &work_S0);
            }                                 
        } else {
            printf("exit 112");
            exit(0);
        }
    }

}

void mff_shortest_connect_subgoal()
{
    int i,j,ns=0;
    int goal;
    
    GpG.num_esp_sol = 0; 
    dis_gnum_fnumeric_goal = 0;
  
    collect_for_shortest();
    
    while(1)
    {
        //dis_print_dis_State(dis_ginitial_state);
            
        j = short_min_sofar();
        if(j==-1) continue;
        if(j==-100) break;
        
        ns++;
   
        
/*        printf("\n%d ",ns);
        dis_print_ft_name(saved_dis_gflogic_goal[j]);
        printf("\n ");
        
        dis_gflogic_goal[j] = saved_dis_gflogic_goal[j]; 
        dis_gnum_flogic_goal = j+1;
        */

        // process the sofar 
        if(short_given_connect(saved_dis_gflogic_goal[j], select_sat, select_inst)<0){
            dis_mff_bridge();
        }    
        
        // analyze subsolution path
        if(dis_gnum_plan_ops >0) {
            mff_record_esp_sol();
        }
        
        if(dis_gnum_plan_ops>=0) { 
            //dis_source_to_dest(&dis_ginitial_state, &dis_mff_sol);
            for(i=0; i<dis_gnum_plan_ops; i++) {
                dis_result_to_dest( &work_S0, &dis_ginitial_state, 
                                    dis_gplan_ops[i], -1 ) ;
                dis_source_to_dest( &dis_ginitial_state, &work_S0);
            }                                 
        } else {
            printf("exit 112");
            exit(0);
        }
    }
    dis_gnum_flogic_goal = 0;
    for (j = 0; j < saved_dis_gnum_flogic_goal; j++) {
        goal =  saved_dis_gflogic_goal[j];
        if(dis_ft_predlen(goal) == MAX_PREDLEN) continue;
        // pointing
        dis_gflogic_goal[dis_gnum_flogic_goal++] = goal; 
            
        dis_mff_bridge();
        mff_record_esp_sol();
        dis_source_to_dest(&dis_ginitial_state, &dis_mff_sol);
    }
    /*
    for (j = 0; j < saved_dis_gnum_flogic_goal; j++) {
        dis_gflogic_goal[j] = saved_dis_gflogic_goal[j]; 
        dis_gnum_flogic_goal = j+1;
        if(short_connect(saved_dis_gflogic_goal[j])<0){
            dis_mff_bridge();
        }    
        if(dis_gnum_plan_ops >0) {
            mff_record_esp_sol();
        }
        if(dis_gnum_plan_ops>=0) { 
            for(i=0; i<dis_gnum_plan_ops; i++) {
                dis_result_to_dest( &work_S0, &dis_ginitial_state, 
                                    dis_gplan_ops[i], -1 ) ;
                dis_source_to_dest( &dis_ginitial_state, &work_S0);
            }                                 
        } else {
            printf("exit 112");
            exit(0);
        }
    }*/
}

void mffDistributedSearch(char *ops_file, char *fct_file)
{
   dis_State dis_end_state; 
   int i,j = -100;
   
   dis_MFF_main(ops_file, fct_file); 
   fprintf(stderr, "\n\nStarting SGPlan search ...\n");
  
   dis_make_state(&saved_start_state, dis_gnum_ft_conn, dis_gnum_fl_conn);
   dis_source_to_dest(&saved_start_state, &dis_ginitial_state);
   dis_make_state(&dis_mff_sol, dis_gnum_ft_conn, dis_gnum_fl_conn);
   dis_make_state(&dis_end_state, dis_gnum_ft_conn, dis_gnum_fl_conn);
   dis_make_state(&dis_known_iga_list, dis_gnum_ft_conn, dis_gnum_fl_conn);
   dis_make_state(&work_S0, dis_gnum_ft_conn, dis_gnum_fl_conn);
   dis_make_state(&work_S1, dis_gnum_ft_conn, dis_gnum_fl_conn);
   work_ft = (int*) calloc(dis_gnum_ft_conn,sizeof(int));
   work_op = (int*) calloc(dis_gnum_ef_conn,sizeof(int));
   
   dis_known_iga_list.num_F = 0; 
   dis_red_space = 0;
   max_bfs_iter = 100000; 
   max_hc_iter = 100000; 
   bridge_option = 0;
   
   //mff_to_lpg();
   decr_g = ( int * ) calloc( 10000, sizeof( int ) );
  
   habakuk_path(fct_file); 
   
   // always_true_DPop();
   // dis_set_DPft_flag();
   if(GpG.SearchModal==5)
   {
//       mff_subgoal();
//       dis_print_IPC4_ffop_GpG(fct_file);
      if (!producible_main())
       mff_fluent_subgoal();
      output_solution(fct_file);
   }
   
   if(GpG.SearchModal==6)
   {
       get_second_modal();
       if( GpG.SecondaryModal == 0) {
            mff_subgoal();
            //    mff_shortest_subgoal();
            dis_print_IPC4_ffop_GpG(fct_file);
       } 
      
       if( GpG.SecondaryModal == 1) {
           i = dis_gnum_ft_conn; 
           if(i<=50) {
                mff_direct();
                dis_print_IPC4_ffop_ffsol(fct_file);      
            } else {
                mff_shortest_subgoal();                   
                dis_print_IPC4_ffop_GpG(fct_file);
            }
       }
       
       if( GpG.SecondaryModal == 2) {
           short_prune_subgoals();
           //mff_subgoal();
           //mff_shortest_subgoal();                   
           mff_shortest_connect_subgoal();
           //dis_print_PERT_GpG(fct_file);
           dis_PERT_GpG();
           short_tif();
           dis_print_sched_ffsol(fct_file);
       }
   }
  
   if(GpG.SearchModal == 106) {
       short_prune_subgoals(); 
        mff_shortest_connect_subgoal();
        dis_PERT_GpG();
        short_tif();   
        dis_print_sched_ffsol(fct_file);
       exit(0);
   } 

   if(GpG.SearchModal==7)   
   {    
        /*
       for(i=0; i<dis_gnum_op_conn;  i++) {
            dis_print_op_name(i); printf("\n");
        }*/
       
       if(!init_timed_wrapper()) {
            init_timed_initial();
//            mff_direct();
            mff_subgoal();
       } else {
            dis_gnum_op_conn -= 5;
            dis_gnum_ef_conn -= 5;
            max_hc_iter = 100; 
//            mff_direct();
            mff_subgoal();
       }
       
//       dis_print_IPC4_ffop_ffsol(fct_file);  
//       dis_print_PERT_ffsol(fct_file);  
         dis_print_Modal7tif_ffsol(fct_file);
   }                        

   if(GpG.SearchModal == 100)
   {
      // dis_red_space = 1;
        bridge_option = 1;
        mff_subgoal_reverse();
        dis_print_IPC4_ffop_GpG(fct_file);
        exit(0);
   }

    if(GpG.SearchModal == 104) {
       //mff_direct();
       //dis_print_IPC4_ffop_ffsol(fct_file);
        bridge_option = 1; 
        mff_subgoal();
       dis_print_IPC4_ffop_GpG(fct_file); 
    }
    
    if(GpG.SearchModal == 107) {
        for(i=strlen(fct_file)-1; i>=0; i--) {
            if(fct_file[i]==MIN_LAG_SUB) { j=fct_file[i+1];
       break;
         } } 
        if(j==MAX_LAG_SUB) { 
            subpath_mff_subgoal();
            dis_print_IPC4_ffop_GpG(fct_file); 
        } else {
            mff_direct();
            dis_print_IPC4_ffop_ffsol(fct_file);
        }
    }   
    
  if (GpG.SearchModal == -1000)
  {
    model1000();
    dis_print_IPC5_GpG(fct_file);
    return;
  }
  if (GpG.SearchModal == -1001)
  {
    model1001();
    dis_print_IPC5_GpG(fct_file);
    return;
  }
  if (GpG.SearchModal == -1002)
  {
    model1002();
    dis_print_IPC5_GpG(fct_file);
    return;
  }
  if (GpG.SearchModal == -1003)
  {
    model1003();
    dis_print_IPC5_GpG(fct_file);
    return;
  }
  if (GpG.SearchModal == -1004)
  {
    model1004();
    dis_print_IPC5_GpG(fct_file);
    return;
  }
  if (GpG.SearchModal == -1005 && (!GpG.is_durative || GpG.SecondaryModal))
  {
    model1005();
    dis_print_IPC5_GpG(fct_file);
    return;
  }
/*  if (GpG.SearchModal == -1006)
  {
    model1006();
    dis_print_IPC5_GpG(fct_file);
    return;
  }*/
    
    if(GpG.SearchModal < 0) {

/*        init_timed_initial();
        
        bridge_option = 0;
        max_hc_iter = 30000; 
        max_bfs_iter = 2; 
        dis_mff_bridge();
        if(dis_gnum_plan_ops>0) {
            GpG.num_esp_sol = 0; 
            mff_record_esp_sol();
            if(dis_gnum_tif==0) 
              dis_print_IPC5_GpG(fct_file);
            else 
                dis_print_less_PERT_GpG(fct_file);
            exit(0);
        }
        
        bridge_option = 1;
        max_bfs_iter = 30000; 
        dis_mff_bridge();
        if(dis_gnum_plan_ops>0) {
            GpG.num_esp_sol = 0; 
            mff_record_esp_sol();
            if(dis_gnum_tif==0)
              dis_print_IPC5_GpG(fct_file);
            else 
              dis_print_less_PERT_GpG(fct_file);
            exit(0);
        }
        
        bridge_option = 0; 
        mff_subgoal();
        if(dis_gnum_tif==0)
          dis_print_IPC5_GpG(fct_file);
        else 
          dis_print_less_PERT_GpG(fct_file);*/
        if (!GpG.is_durative && num_bottleneck_var > 0 && !GpG.is_preferences && !GpG.is_goal_utilities)
        {
          if (!psearch())
            model0();
        }
        else
          model0();
          output_solution(fct_file);
    }
}
