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

 * File: esp.c

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

/********************************************************************
 * File: esp.c
 * Description: Extended Saddle Point Search method
 *                                      
 * Author: Yixin Chen
 *
 *********************************************************************/


#include <values.h>
#include <math.h>
#include <unistd.h>

#include "lpg.h"
#include "LpgTime.h"
#include "check.h"
#include "numeric.h"
#include "ActionSubgraph.h"
#include "H_relaxed.h"
#include "H_max.h"
#include "utilities.h"
#include "LpgOutput.h"
#include "output.h"
#include "esp.h" 
#include "ff.h"
#include "orderings.h"
#include "subspace.h"
#include "interProcess.h"
#include "stripsff.h"
#include "dis_ff.h"
#include "search.h"

extern int *lch;
extern int lnum_ch;
extern Bool *lin_ch;
extern int *lDcount;
extern Bool *lin;
extern Bool **lm;
extern void fout_vState(vState *, char *);
extern void reset_plan (int);
extern void forward_noop_propagation (int, int);
extern int choose_act_fa (int);
extern void update_precond();
extern void update_precond_multilevel();
extern void update_decr_me_prec();
extern void update_decr_me_prec_multilevel();
int *h1_facts, *h2_facts;
    
int* decr_g;
int rsa_facts[MAX_FT_NUM];
int rsa_actions[MAX_OP_NUM];

void var_source_to_dest( State *dest, State *source )
{    
      int i;
   
      for ( i = 0; i < source->num_F; i++ ) {
          dest->F[i] = source->F[i];
      }

      dest->num_F = source->num_F;
      for(i=0; i<gnum_comp_var; i++) {
          dest->V[i] = source->V[i];
      }
}



void send_plan_actions(int fd, PlanAction **plan_actions)
{
	int l;
	PlanAction *temp_act;

	l=0;
	for (temp_act = *plan_actions; temp_act; temp_act = temp_act->next) l++;
	
	write(fd, (void *)&(GpG.curr_plan_length), sizeof(int));	
	write(fd, (void *)&l, sizeof(int)); 

	for (temp_act = *plan_actions; temp_act; temp_act = temp_act->next) {
		write(fd, (void *)temp_act, sizeof(PlanAction));	
	}
}

void ff_send_plan_actions(int fd)
{
    int i;
	write(fd, (void *)&gnum_plan_ops, sizeof(int)); 

	for (i= 0; i<gnum_plan_ops; i++) {
		write(fd, (void *)&(gplan_ops[i]), sizeof(int));	
	}                           
}

void ff_recv_plan_actions(int fd)
{
    int i;
	read(fd, (void *)&gnum_plan_ops, sizeof(int)); 

	for (i= 0; i<gnum_plan_ops; i++) {
		read(fd, (void *)&(gplan_ops[i]), sizeof(int));	
	}                           
}

void mff_send_plan_actions(int fd)
{
    int i;
	write(fd, (void *)&dis_gnum_plan_ops, sizeof(int)); 

	for (i= 0; i<dis_gnum_plan_ops; i++) {
		write(fd, (void *)&(dis_gplan_ops[i]), sizeof(int));	
	}                           
}

void mff_recv_plan_actions(int fd)
{
    int i;
	read(fd, (void *)&dis_gnum_plan_ops, sizeof(int)); 

	for (i= 0; i<dis_gnum_plan_ops; i++) {
		read(fd, (void *)&(dis_gplan_ops[i]), sizeof(int));	
	}                           
}

void mff_send_time(int fd, float x)
{
    write(fd, (void *)&x, sizeof(float));
}                                                          

void mff_recv_time(int fd)
{                           
    float x;
    read(fd, (void *)&x, sizeof(float)); 
    GpG.glob_dist_time += x;
}

void send_sol_state(int fd)
{
    int i;
    //write(fd, (void *)&(GpG.sol_state), sizeof(State));	
    write(fd, (void*)&(GpG.sol_state.num_F), sizeof(int));
   // printf("send nF %d\n", GpG.sol_state.num_F);
    for(i=0; i<GpG.sol_state.num_F; i++) {
        write(fd, (void*)&(GpG.sol_state.F[i]), sizeof(int));         
    }
    for(i=0; i<gnum_comp_var; i++) {
        write(fd, (void*)&(GpG.sol_state.V[i]), sizeof(float));         
    }
}        


void recv_sol_state(int fd)
{
    int i;
    //read(fd, (void *)&(GpG.sol_state), sizeof(State));	
    
    read(fd, (void*)&(GpG.sol_state.num_F), sizeof(int));
    //printf("recv nF %d\n", GpG.sol_state.num_F);
    for(i=0; i<GpG.sol_state.num_F; i++) {
        read(fd, (void*)&(GpG.sol_state.F[i]), sizeof(int));         
    }       
    for(i=0; i<gnum_comp_var; i++) {
        read(fd, (void*)&(GpG.sol_state.V[i]), sizeof(float));         
    }
}                           

void dis_send_sol_state(int fd)
{
    int i;
    
    write(fd, (void*)&(dis_mff_sol.num_F), sizeof(int));
//    printf("send nF %d\n", dis_mff_sol.num_F);
    for(i=0; i<dis_mff_sol.num_F; i++) {
        write(fd, (void*)&(dis_mff_sol.F[i]), sizeof(int));         
    }
    for(i=0; i<dis_gnum_fl_conn; i++) {
        write(fd, (void*)&(dis_mff_sol.f_D[i]), sizeof(dis_Bool));         
        write(fd, (void*)&(dis_mff_sol.f_V[i]), sizeof(float));         
    }
}        


void dis_recv_sol_state(int fd)
{
    int i;
    //read(fd, (void *)&(GpG.sol_state), sizeof(State));	
    
    read(fd, (void*)&(dis_mff_sol.num_F), sizeof(int));
  //  printf("recv nF %d\n", dis_mff_sol.num_F);
    for(i=0; i<dis_mff_sol.num_F; i++) {
        read(fd, (void*)&(dis_mff_sol.F[i]), sizeof(int));         
    }       
    for(i=0; i<dis_gnum_fl_conn; i++) {
        read(fd, (void*)&(dis_mff_sol.f_D[i]), sizeof(dis_Bool));         
        read(fd, (void*)&(dis_mff_sol.f_V[i]), sizeof(float));         
    }
}                           

void recv_plan_actions(int fd, PlanAction **plan_actions)
{
	PlanAction *plAct, *lastPa = NULL;
	int i,l, level;
	
	if (plan_actions != NULL) {
		free_gplan_actions (*plan_actions);
		*plan_actions = NULL;		
	}
				
	read(fd, (void *)&level, sizeof(int));	
	read(fd, (void *)&l, sizeof(int));

	for(i=0; i<l; i++) {
		plAct = (PlanAction *) calloc (1, sizeof (PlanAction));
		read(fd, (void *)plAct, sizeof(PlanAction));	
		if(i==0) {
			*plan_actions = plAct;
		        plAct->previous = NULL;
		} else {
			plAct->previous = lastPa;
			lastPa->next = plAct;
		}
		lastPa = plAct;
	}
	if(l>0) lastPa->next = NULL;
	
    GpG.curr_plan_length = GpG.input_plan_lenght = level;
	GpG.num_actions = l;
	GpG.gplan_actions = *plan_actions;			

    
	//printf("\nmain process: %d actions in %d level recieved.\n\n", l, level);
}

void LPG_record_esp_sol()
{
    PlanAction *temp_act;
    
    for (temp_act = GpG.gplan_actions; temp_act;
               temp_act = temp_act->next) {
      //  printf("# %d %d ",GpG.num_esp_sol,temp_act->act_pos);
        print_op_name(temp_act->act_pos); printf("\n");
        GpG.esp_solution[GpG.num_esp_sol++] = temp_act->act_pos;
    }                                          
}

void FF_record_esp_sol()
{
    int i;
 //   printf("gnum_plan_ops =%d\n", gnum_plan_ops);
    for (i = 0; i < gnum_plan_ops; i++) {
   //     printf("# %d %d ",GpG.num_esp_sol,gplan_ops[i]);
     //   print_op_name(gplan_ops[i]); printf("\n");
        GpG.esp_solution[GpG.num_esp_sol++] = gplan_ops[i];
    }       
}                                                                           

void mff_record_esp_sol()
{
    int i;
  //  printf("dis_gnum_plan_ops =%d\n", dis_gnum_plan_ops);
    for (i = 0; i < dis_gnum_plan_ops; i++) {
      /*
        printf("# %d %d ",GpG.num_esp_sol, dis_gplan_ops[i]);
        if(SymmLagrangian==1) {
            print_op_name(dis_gplan_ops[i]); 
        }
        if(GpG.SearchModal >=5 ) {
            dis_print_op_name(dis_gplan_ops[i]);
        }
        printf("\n");
        */
        GpG.esp_solution[GpG.num_esp_sol++] = dis_gplan_ops[i];
    } 
}                                                                           

void h1_goal_order(State *start_state, State * end_state)
{
    int i, nba;
    h1_facts = (int *) calloc (end_state->num_F, sizeof (int)); 
    for(i=0; i<end_state->num_F; i++) {
        nba = reduce_subspace_actions(start_state, end_state->F[i], 1);
        print_ft_name(end_state->F[i]);
        printf(" h1 = %d\n", nba);
        h1_facts[i] = nba;
    }
    for(i=0; i<gnum_ef_conn;i++) GpG.op_in_graph[i]= 1;        
}

void h2_goal_order(State * end_state)
{
    int i,j,g,o,np, min_np;
    h2_facts = (int *) calloc (end_state->num_F, sizeof (int));    
    for(i=0; i<end_state->num_F; i++) {
        g = end_state->F[i];
        min_np = 30000;
        for(j=0; j<gft_conn[g].num_A; j++) {
            o = gft_conn[g].A[j];
            np = gef_conn[o].num_PC;
            if(gef_conn[o].sf) {
                np = np + gef_conn[o].sf->num_PC_overall;
                np = np + gef_conn[o].sf->num_PC_end;
            }
            if(np < min_np) min_np = np;
        }                  
        h2_facts[i] = min_np; 
                
        print_ft_name(end_state->F[i]);
        printf(" h2 = %d\n", min_np);
    }                       
}


void twist_goal_order(State * end_state)
{
    int i,j;
    
    end_state->num_F = 0;
    for ( i = 0; i < gnum_goal_agenda; i++ ) {
        for ( j = 0; j < ggoal_agenda[i].num_F; j++ ) {
            end_state->F[(end_state->num_F)++] = ggoal_agenda[i].F[j];
        }               
    }
    source_to_dest(&ggoal_state, end_state);
}                                           
           
void init_h1_h2()
{
    h1_goal_order(&saved_ginitial_state, &saved_ggoal_state);
    h2_goal_order(&saved_ggoal_state);
}

void random_twist_goal_order(State * end_state)
{
    int *gindex;
    int i,j,t,gs, swap;
    gindex = (int *) calloc (saved_ggoal_state.num_F, sizeof (int)); 
    for(i=0; i<saved_ggoal_state.num_F; i++) {
        gindex[i] = i;
    }
    
    // randomizing (optional)
    gs = saved_ggoal_state.num_F;
    for(i=0; i<gs-1; i++) {
        j= i+ (int) ((double)(gs-i)*rand()/(RAND_MAX+1.0));
        if(j!=i) {
            t = gindex[i];    
            gindex[i] = gindex[j];
            gindex[j] = t;
        }
    }

    // order    
    for(i=0; i<gs-1; i++) {
        for(j=i+1; j<gs; j++) { 
            swap = 0;
            if(lm[gindex[j]][gindex[i]]) {
                swap = 1;    
            } else {
                if(!lm[gindex[i]][gindex[j]]) {
                    if( (h1_facts[gindex[i]] < h1_facts[gindex[j]]) ||
                       ( (h1_facts[gindex[i]] == h1_facts[gindex[j]]) &&
                         (h2_facts[gindex[i]] < h2_facts[gindex[j]]) )) 
                        swap = 1;
                }   
            }               
            
            if(swap == 1) {
                t = gindex[i];    
                gindex[i] = gindex[j];
                gindex[j] = t;
            }
        }
    }
    
    // set end_state
    
    end_state->num_F = saved_ggoal_state.num_F;
    for(i=0; i<saved_ggoal_state.num_F; i++) {
        end_state->F[i] = saved_ggoal_state.F[gindex[i]];
    } 
    source_to_dest(&ggoal_state, end_state);
}

void init_subspace_actions()
{
   int i,j;
   
   for (i = 0; i < gnum_ef_conn; i++) {
       gef_conn[i].sub_prob = 0;
       gef_conn[i]._num_pcc = gef_conn[i].num_PC;
       if (gef_conn[i].sf) {
           gef_conn[i]._num_pcc += gef_conn[i].sf->num_PC_overall; 
           gef_conn[i]._num_pcc += gef_conn[i].sf->num_PC_end;
       }
       for (j = 0; j < gef_conn[i].num_PC; j++){
           if((gef_conn[i].PC[j] < 0)
              || (gef_conn[i].PC[j] == GpG.dummy_fid)) 
               gef_conn[i]._num_pcc--;
       }        
       if(gef_conn[i].sf) {
         for (j = 0; j < gef_conn[i].sf->num_PC_overall; j++)
           if((gef_conn[i].sf->PC_overall[j] < 0)
              || (gef_conn[i].sf->PC_overall[j] == GpG.dummy_fid)) 
                gef_conn[i]._num_pcc--;
         for (j = 0; j < gef_conn[i].sf->num_PC_end; j++)
             if( (gef_conn[i].sf->PC_end[j] < 0)
              ||  (gef_conn[i].sf->PC_end[j]== GpG.dummy_fid)) 
                 gef_conn[i]._num_pcc--;
       } 
   }
}
void restore_subspace_actions()
{
   int i;
   
   for (i = 0; i < gnum_ef_conn; i++) {
       gef_conn[i].num_pcc = gef_conn[i]._num_pcc;
   }
}

void printv(int *v, int s, char *str)
{
    int i;
    printf("\n%s: ",str);
    for(i=0;i<s;i++) printf("%d ", v[i]);
    printf("\n");
}

int reachable_setupE_a_fact(State * start_state, int Gf, int ripped,                                   int* rsa_facts, int* rsa_actions)
{ 
   int i, j, k, f, a;
   int level;
   int newfacts;

   
   
    /* initialization */ 
    restore_subspace_actions();
   

   for (i = 0; i < gnum_ft_conn; i++) rsa_facts[i] = -1; 
   for(i=0; i<start_state->num_F; i++) {
       rsa_facts[start_state->F[i]] = 0;
       if(start_state->F[i] == Gf) return 1;
   }
   for (i = 0; i < gnum_ef_conn; i++) rsa_actions[i] = -1;
  
   /* set up ripped */
   setup_E( ripped );
   
   /* Forward until reach the goal */
   level = 0;
   while(1)
   {
       newfacts = 0; 
    
      /* 
       printf("\nlevel %d\n", level);
       for (i = 0; i < gnum_ft_conn; i++) 
           if(rsa_facts[i] == level){ 
                print_ft_name(i); printf("\n");
           }
       printf("\n");
       */
       
       for (i = 0; i < gnum_ft_conn; i++) {
           if(rsa_facts[i] == level) {
                 newfacts = 1;
                 for (j = 0; j < gft_conn[i].num_PC; j++) {
                    a = gft_conn[i].PC[j];
                    if(rsa_actions[a] < 0) {
                      gef_conn[a].num_pcc--;
                      if(gef_conn[a].num_pcc == 0) {
                          if(!lin[a]) {
       
                            /*                     
                            printf("mutex by ");
                            print_ft_name(ripped);
                            print_op_name(gef_conn[a].op); printf("\n");   
                            */
                            
                        }
                        else {
                         
                            /*
                         printf("add action %d ", a);
                         print_op_name(gef_conn[a].op); printf("\n");
                         */
                            
                         rsa_actions[a] = level;
                         for (k = 0; k < gef_conn[a].num_A; k++) {
                            f = gef_conn[a].A[k];
                                if(f<0) continue;
                            if(rsa_facts[f] < 0) 
                                    rsa_facts[f] = level+1; 
                            if(f == Gf) { unsetup_E(ripped); return 1;}
                         }
                         if (gef_conn[a].sf) {
                            for (k = 0; k < gef_conn[a].sf->num_A_start; k++) {
                                f = gef_conn[a].sf->A_start[k];
                                if(f<0) continue;
                                if(rsa_facts[f] < 0) 
                                        rsa_facts[f] = level+1; 
                                if(f == Gf) {  unsetup_E(ripped); return 1;}
                            }
                          }
                      
                        }  
                     
                      }
                    }
                 }
            }
       }
      
       if(newfacts != 1) break; 
       level++;
   }
   
    unsetup_E(ripped); 
   return 1;
}

// if return 0;
// that means starting from start_state, if ripped is held first,
// we cannot reach Gf without destroying ripped in the middle
// that means Gf <= ripped is reasonable
int reachable_mutex_a_fact(State * start_state, int Gf, int ripped,                                   int* rsa_facts, int* rsa_actions)
{ 
   int i, j, k, f, a;
   int level;
   int newfacts;

    /*   
    printf("\n--- Starting: "); printf(" ---");
    print_state(*start_state);
    printf("\n--- Gf: "); print_ft_name(Gf); printf(" ---\n");
   */
    /* initialization */ 
    restore_subspace_actions();
   

   for (i = 0; i < gnum_ft_conn; i++) rsa_facts[i] = -1; 
   for(i=0; i<start_state->num_F; i++) {
       rsa_facts[start_state->F[i]] = 0;
       if(start_state->F[i] == Gf) return 1;
   }
   for (i = 0; i < gnum_ef_conn; i++) rsa_actions[i] = -1;
  
   /* Forward until reach the goal */
   level = 0;
   while(1)
   {
       newfacts = 0; 
   
       /*  
       printf("\nlevel %d\n", level);
       for (i = 0; i < gnum_ft_conn; i++) 
           if(rsa_facts[i] == level){ 
                print_ft_name(i); printf("\n");
           }
       printf("\n");*/
       for (i = 0; i < gnum_ft_conn; i++) {
           if(rsa_facts[i] == level) {
                 newfacts = 1;
                 for (j = 0; j < gft_conn[i].num_PC; j++) {
                    a = gft_conn[i].PC[j];
                    if(rsa_actions[a] < 0) {
                      gef_conn[a].num_pcc--;
                      if(gef_conn[a].num_pcc == 0) {
                        if(GET_BIT (FT_EF_mutex[ripped], a)) {
                            /*
                            printf("mutex by ");
                            print_ft_name(ripped);
                            print_op_name(gef_conn[a].op); printf("\n");  */     
                        }
                        else {
                         /*
                         printf("add action %d ", a);
                         print_op_name(gef_conn[a].op); printf("\n");
                         */
                         rsa_actions[a] = level;
                         for (k = 0; k < gef_conn[a].num_A; k++) {
                            f = gef_conn[a].A[k];
                            if(f<0) continue;
                            if(rsa_facts[f] < 0) 
                                    rsa_facts[f] = level+1; 
                            if(f == Gf) { return 1;}
                         }
                         if (gef_conn[a].sf) {
                            for (k = 0; k < gef_conn[a].sf->num_A_start; k++) {
                                f = gef_conn[a].sf->A_start[k];
                                if(f<0) continue;
                                if(rsa_facts[f] < 0) 
                                        rsa_facts[f] = level+1; 
                                if(f == Gf) { return 1;}
                            }
                          }
                      
                        }  
                     
                      }
                    }
                 }
            }
       }
      
       if(newfacts != 1) break; 
       level++;
   }
   return 0;
}


// if return 0;
// that means start_state canot reach Gf without meet ripped first
// that means ripped <= Gf MUST hold
int reachable_rip_a_fact(State * start_state, int Gf, int ripped,                                   int* rsa_facts, int* rsa_actions)
{ 
   int i, j, k, f, a;
   int level;
   int newfacts;

    /*   
    printf("\n--- Starting: "); printf(" ---");
    print_state(*start_state);
    printf("\n--- Gf: "); print_ft_name(Gf); printf(" ---\n");
   */
    /* initialization */ 
    restore_subspace_actions();
   

   for (i = 0; i < gnum_ft_conn; i++) rsa_facts[i] = -1; 
   for(i=0; i<start_state->num_F; i++) {
       rsa_facts[start_state->F[i]] = 0;
       if(start_state->F[i] == Gf) return 1;
   }
   for (i = 0; i < gnum_ef_conn; i++) rsa_actions[i] = -1;
  
   /* Forward until reach the goal */
   level = 0;
   while(1)
   {
       newfacts = 0; 
   
       printf("\nlevel %d\n", level);
       for (i = 0; i < gnum_ft_conn; i++) 
           if(rsa_facts[i] == level){ 
                print_ft_name(i); printf("\n");
           }
       printf("\n");

           for (i = 0; i < gnum_ft_conn; i++) {
           if(rsa_facts[i] == level) {
                 newfacts = 1;
                 for (j = 0; j < gft_conn[i].num_PC; j++) {
                    a = gft_conn[i].PC[j];
                    if(rsa_actions[a] < 0) {
                      gef_conn[a].num_pcc--;
                      if(gef_conn[a].num_pcc == 0) {
         
                        printf("add action %d ", a);
                        print_op_name(gef_conn[a].op); printf("\n");
                        
                        rsa_actions[a] = level;
                        for (k = 0; k < gef_conn[a].num_A; k++) {
                            f = gef_conn[a].A[k];
                                if(f<0) continue;
                            if(f!=ripped)
                                if(rsa_facts[f] < 0) 
                                    rsa_facts[f] = level+1; 
                            if(f == Gf) { return 1;}
                        }
                        if (gef_conn[a].sf) {
                            for (k = 0; k < gef_conn[a].sf->num_A_start; k++) {
                                f = gef_conn[a].sf->A_start[k];
                                if(f<0) continue;
                                if(f!=ripped)
                                    if(rsa_facts[f] < 0) 
                                        rsa_facts[f] = level+1; 
                                if(f == Gf) { return 1;}
                            }
                        }
                      }  
                    }
                 }
            }
       }
      
      /* 
       n=0;
       for(i=0; i<gnum_ef_conn; i++) {
            if(rsa_actions[i]<=level)
                if(rsa_actions[i]>=0)
                    n++;
       }
       printf("level %d act %d\n", level, n);
       */
       
       if(newfacts != 1) break; 
       level++;
   }
   return 0;
}

void print_pred()
{
    int i,n;      
    for(i=0; i<GpG.num_IMG_facts; i++) {
        printf("fact %d: ",GpG.IMG[i].f);
        for(n = 0; n<GpG.IMG[i].num_pred; n++)
            printf("%d ",GpG.IMG[i].pred[n]);
        printf("\n");
    }
}

Bool in_pred(int i, int k)
{
    int n;            
    for(n = 0; n<GpG.IMG[i].num_pred; n++)
        if(k==GpG.IMG[i].pred[n]) return TRUE;
    return FALSE;
}

Bool in_follow(int i, int k)
{
    int n;            
    for(n = 0; n<GpG.IMG[i].out_degree; n++)
        if(k==GpG.IMG[i].follow[n]) return TRUE;
    return FALSE;
}

int IMG_pos(int k)
{
    int n;
    for(n=0; n<GpG.num_IMG_facts; n++)     
        if(GpG.IMG[n].f == k) return n;

    return (-1);   
}  
  
Bool order_complied_IMG()
{
    int i,j;
    for(i=0; i<GpG.num_IMG_facts-1; i++) {
        for(j=i+1; j<GpG.num_IMG_facts; j++)
            if(in_pred(i, GpG.IMG[j].f)) return FALSE;
    }
    
    return TRUE;
}

void find_intermediate_goals(State * start_state, int Gf)
{
    //int* rsa_facts;
    //int* rsa_actions;
    int i,j,f,r,k,n,swap, r1, r2, jj, nn,ff;
    Bool swap_happened, found_f, not_found_f;
    IMG_node temp;
    static int do_IMG = 0;
    State GS, nul;
    
    
    //rsa_facts = (int *) calloc (gnum_ft_conn, sizeof (int)); 
    //rsa_actions = (int *) calloc (gnum_ef_conn, sizeof (int)); 
    
    
    GpG.num_IMG_facts = 0;
    //print_state(*start_state); 
    //printf("goal ");print_ft_name(Gf); 
    printf("\nfinding IMG...\n");
 
    
/* find out IMG facts */
    /*
    if(do_IMG != 2) 
        for(i = 0; i<gnum_ft_conn; i++) {
            r = reachable_rip_a_fact (start_state, Gf, i,rsa_facts, rsa_actions);
            if(r==0) {
                GpG.IMG[GpG.num_IMG_facts++].f = i;
            }
        }                    
    */
            
   
    //more efficient implementation
    GS.num_F = 1;
    GS.F[0] = Gf;
    nul.num_F = 0;
    build_subspace(start_state, rsa_facts, rsa_actions, &nul);
    if(rsa_facts[Gf] < 0) {
        printf("Gf not included in built space\n");
        GpG.num_IMG_facts = -1;
        return;
    }    

    
    for(i=0;i<gnum_ft_conn;i++) {
        if(contain_fact_state(start_state, i)) continue;       
        if(contain_fact_state(&GS, i)) continue; 
        r = block_fact_subspace(start_state, i, rsa_facts, rsa_actions, &GS, decr_g, GOAL_BLOCK);
        if(r==0) {
            GpG.IMG[GpG.num_IMG_facts++].f = i;
        }
    }                                                                                 
    
    if(GpG.num_IMG_facts> MAX_IMG_FACTS) {
        printf("error:MAX_IMG_FACTS too small %d < %d\n", MAX_IMG_FACTS, GpG.num_IMG_facts);
        exit(1);
    }

/* do not find IMG if not found at the first time */ 
    if(do_IMG==0) { 
        if (GpG.num_IMG_facts==0) do_IMG = 2; 
        else do_IMG = 1;
    }

/* ordering constraints among IMG facts */
    for(i=0; i<GpG.num_IMG_facts; i++) {
        GpG.IMG[i].in_degree = 0; GpG.IMG[i].out_degree = 0;
    }       
    for(i=0; i<GpG.num_IMG_facts; i++) {
        GpG.IMG[i].num_pred = 0;
        for(j=0; j<GpG.num_IMG_facts; j++) {
            if(i!=j){
                /*
                r = reachable_rip_a_fact (start_state, GpG.IMG[i].f 
                    , GpG.IMG[j].f,rsa_facts, rsa_actions);   
              */

                GS.F[0] = GpG.IMG[i].f;
                r = block_fact_subspace(start_state, GpG.IMG[j].f, 
                     rsa_facts, rsa_actions, &GS, decr_g, GOAL_BLOCK);
                
                if(r == 0) {
                    GpG.IMG[i].pred[GpG.IMG[i].num_pred++] = GpG.IMG[j].f;
                    
                    GpG.IMG[i].in_degree++;
                    GpG.IMG[j].cover_follow[GpG.IMG[j].out_degree]  = FALSE;
                    GpG.IMG[j].follow[GpG.IMG[j].out_degree++]  = GpG.IMG[i].f;
                          
                    /*                  
                    printf("IMG ordering:       ");
                    //print_ft_name(GpG.IMG[j].f);
                    printf(" %d <= ", GpG.IMG[j].f);
                    //print_ft_name(GpG.IMG[i].f);
                    printf(" %d\n",GpG.IMG[i].f); 
                    */
                }
         
                /*
               // r = reachable_mutex_a_fact (start_state, GpG.IMG[i].f , GpG.IMG[j].f,rsa_facts, rsa_actions);   
                  
              //  r = reachable_setupE_a_fact (start_state, GpG.IMG[i].f, GpG.IMG[j].f,rsa_facts, rsa_actions);   
            
                if(r == 0) {
                    printf("mutex IMG ordering: ");
                    print_ft_name(GpG.IMG[i].f);
                    printf(" %d <= ", GpG.IMG[i].f);
                    print_ft_name(GpG.IMG[j].f);
                    printf(" %d\n",GpG.IMG[j].f);
                }
                */
            }   
        }                                           
    }
    
/* rank sorting of IMG facts */
    /*
    in_weight = GpG.num_IMG_facts; 
    for(i=0; i<GpG.num_IMG_facts; i++) 
        GpG.IMG[i].rank = in_weight * GpG.IMG[i].in_degree - GpG.IMG[i].out_degree;
      */
    
    print_pred();
    
    //generate an order satisfying:
    // (1) all IMG ordering constraints are preserved
    // (2) mutex ordering constraints for IMG are preserved
    // (3) zero-in-degree IMG facts are ordered before others
    // (4) with conlicts for these criteria, lower numbered have higher priority 
    for(i=0; i<GpG.num_IMG_facts-1; i++) {
        swap_happened = FALSE;
        for(j=i+1; j<GpG.num_IMG_facts; j++) {
            swap = 0;
           
            if(!in_pred(j, GpG.IMG[i].f)){ 
              if(in_pred(i,GpG.IMG[j].f)) swap = 1;
              
              else{
                if((GpG.IMG[j].in_degree == 0) &&
                    (GpG.IMG[i].in_degree != 0) ) swap = 1;
              } 
            }
            if(swap==1) {
                swap_happened = TRUE;
                 
                printf("swapping %d with %d\n", GpG.IMG[i].f,
                        GpG.IMG[j].f);
                
                temp = GpG.IMG[j];
                GpG.IMG[j] = GpG.IMG[i];
                GpG.IMG[i] = temp;
            }
        }
        if(swap_happened) i--;
    }
   
    for(i=0; i<GpG.num_IMG_facts-1; i++) {
        for(j=i+1; j<GpG.num_IMG_facts; j++) {
            swap = 0;      
            if(!in_pred(j,GpG.IMG[i].f) ) {
                    r1 = reachable_mutex_a_fact (start_state, GpG.IMG[j].f 
                      , GpG.IMG[i].f,rsa_facts, rsa_actions);
                    r2 = reachable_mutex_a_fact (start_state, GpG.IMG[i].f 
                      , GpG.IMG[j].f,rsa_facts, rsa_actions);
               
                    //r1 = mutex_fact_achievable(GpG.IMG[i].f, GpG.IMG[j].f);
                    //r2 = mutex_fact_achievable(GpG.IMG[j].f, GpG.IMG[i].f);
                    if((r1==0) && (r2!=0)) swap = 1;
            }               
            if(swap==1) {
                printf("mutex swapping %d with %d\n", GpG.IMG[i].f,
                        GpG.IMG[j].f);
                
                temp = GpG.IMG[j];
                GpG.IMG[j] = GpG.IMG[i];
                GpG.IMG[i] = temp;
                
                if(!order_complied_IMG()) {
                    temp = GpG.IMG[j];
                    GpG.IMG[j] = GpG.IMG[i];
                    GpG.IMG[i] = temp;
                }
            }
        }
    }
    
    
    printf("\nSorted IMG:\n");
    for(i=0; i<GpG.num_IMG_facts; i++) {
        printf("%d ", GpG.IMG[i].f);        
        print_ft_name(GpG.IMG[i].f); 
        printf(" in %d out %d num_pred %d rank %d\n",GpG.IMG[i].in_degree, 
                GpG.IMG[i].out_degree,GpG.IMG[i].num_pred, GpG.IMG[i].rank);
    }                            

/* generate sub-agenda for IMG */
    // add IMG facts in order to the sub-agenda
    // an IMG fact is deleted when its out-degrees are all fulfilled 
    GpG.num_sub_agenda_IMG = GpG.num_IMG_facts;
    for(i=0; i<GpG.num_IMG_facts; i++) {
        f = GpG.IMG[i].f;
        GpG.sub_agenda_IMG[i].num_F = 1;
        GpG.sub_agenda_IMG[i].F[0] = f;
       
        if(i>0){
            for(j=0; j<GpG.sub_agenda_IMG[i-1].num_F; j++) {
                k = GpG.sub_agenda_IMG[i-1].F[j];
                if(ARE_MUTEX_FT(f, k)) {
                    printf("%d not entered as it is mutex with %d\n",k,f);
                    continue;
                }
                if(!in_pred(i,k)) {
                    GpG.sub_agenda_IMG[i].F[GpG.sub_agenda_IMG[i].num_F++] = k;         
                } else {
                    n = IMG_pos(k);
                    not_found_f = FALSE;
                    for(r=0; r<GpG.IMG[n].out_degree; r++)   {
                        if(GpG.IMG[n].cover_follow[r]) continue;
                        
                        ff = GpG.IMG[n].follow[r];
                        found_f = FALSE;
                                 
                        if(ff == f) {
                                    found_f = TRUE;
                        } else {
                          if(in_follow(i, ff)) found_f = TRUE;
                          else {
                            for(jj=0; jj<GpG.sub_agenda_IMG[i-1].num_F; jj++) { 
                               if(in_follow(n,GpG.sub_agenda_IMG[i-1].F[jj]))
                               {
                                 if(ff == GpG.sub_agenda_IMG[i-1].F[jj]) {
                                    found_f = TRUE;
                                    break;
                                 }
                                 nn = IMG_pos(GpG.sub_agenda_IMG[i-1].F[jj]);
                                 if(in_follow(nn, ff)) {
                                    found_f = TRUE;
                                    break;
                                 } 
                               }
                            }
                          }
                        }
                        if(!found_f) not_found_f = TRUE;
                        else GpG.IMG[n].cover_follow[r] = TRUE; 
                    }    
                    if(not_found_f) { 
                        GpG.sub_agenda_IMG[i].F[GpG.sub_agenda_IMG[i].num_F++] = k; 
                    }
                }               
            }       
        }
    }  

    GpG.sub_agenda_IMG[GpG.num_sub_agenda_IMG].num_F = 1;
    GpG.sub_agenda_IMG[GpG.num_sub_agenda_IMG++].F[0] = Gf; 
    
   // print out the sub-agenda for IMG 
    printf("\nIMG sub-agenda is:\n");
    for(i=0; i<GpG.num_sub_agenda_IMG ; i++) {
        if ( i == 0 ) {
               printf("\nentry %3d: ", i);
        } else {
               printf("\n      %3d: ", i);
        }
         
        for ( j = 0; j < GpG.sub_agenda_IMG[i].num_F; j++ ) {
            printf("%d ", GpG.sub_agenda_IMG[i].F[j]);
          // print_ft_name( GpG.sub_agenda_IMG[i].F[j]);
          //  if ( j < GpG.sub_agenda_IMG[i].num_F - 1 ) {
          //      printf("\n           ");
          // }
        }
    }                            
    printf("\n");
   
    
    
/* clean up */
    //free(rsa_facts); 
    //free(rsa_actions);    
}                   
    

int
espLocalSearch (State * start_state, State * end_state,
	     PlanAction ** plan_actions)
{
	int i,j,k,tp;
	int fd[2];
	int pid;
	int sgs;
	State saved_end_state;
    State mips_res_state;
   
    if(GpG.esp_search == 4) serverMips();
    if(GpG.esp_search == 2) {
        save_lpg_gef_conn();
        strips_gef_conn();
        load_ff_gef_conn();
    }


    
    /* save ginitial_state and ggoal_state before search */
    var_source_to_dest(&saved_ginitial_state, &ginitial_state);
    var_source_to_dest(&saved_ggoal_state, &ggoal_state);
    var_source_to_dest(start_state, &ginitial_state);
    var_source_to_dest(&GpG.sol_state, &ginitial_state);
                                    
    // 0.5
     known_iga_list.num_F = 0; 
     
    
    /*
    for(k=0; k<ginitial_state.num_F; k++) 
        saved_ginitial_state.F[k] = ginitial_state.F[k]; 
    for(k=0; k<ggoal_state.num_F; k++) 
        saved_ggoal_state.F[k] = ggoal_state.F[k]; 
    saved_ginitial_state.num_F = ginitial_state.num_F;
    saved_ggoal_state.num_F = ggoal_state.num_F;
    for(k=0; k<gnum_comp_var; k++) {
        saved_ginitial_state.V[k] = ginitial_state.V[k];
        saved_ggoal_state.V[k] = ggoal_state.V[k];
    } */              
            
    /* initialize */
    pure_numerical_actions();
    decr_g = ( int * ) calloc( gnum_ft_conn, sizeof( int ) );


    printf("\n\n<**** ESP planning ****>\n\n");
    init_subspace_actions();
    GpG.total_num_subprob = 0;
    GpG.num_esp_sol = 0; 
    tp =0;
    /* goal ordering */
    //init_h1_h2();
    //random_twist_goal_order(end_state);
    twist_goal_order(end_state);
    source_to_dest(&saved_end_state, end_state); 
	printf("\nrandom twisted end state ");print_state(*end_state);
    printf("\n");       
   
   
    // test: iga graph 
    //build_iga_graph(&saved_ginitial_state, rsa_facts, rsa_actions, decr_g);
    //exit(0);

    
    /* esp search */
	for (j = 0; j < saved_end_state.num_F; j++)	     
	{
        GpG.total_num_subprob++;

        //if(GpG.esp_search == 1) reduce_subspace_actions(start_state,  saved_end_state.F[j], 1);
        
        
        if(!v_contain_fact_state(&known_iga_list ,saved_end_state.F[j])) {
            known_iga_list.F[ known_iga_list.num_F++ ] = saved_end_state.F[j]; 
        }   
        
        fout_vState(&known_iga_list, "known_iga_list");
                                        
        /* reduce subspace */
        switch(GpG.esp_search) {
            case 1:
            case 2:
                find_intermediate_goals(start_state, saved_end_state.F[j]);
                break;
            case 3:
                if(GpG.total_num_subprob==1)
                    find_intermediate_goals(start_state, saved_end_state.F[j]);
                else
                    find_intermediate_goals(&GpG.sol_state, saved_end_state.F[j]);
                break;
            case 4:
                find_intermediate_goals(start_state, saved_end_state.F[j]);
                break;
            default:
                printf("error of GpG.esp_search\n");
                exit(1);
        }                                                                       
        
        for(i=0; i<j; i++) end_state->F[i] = saved_end_state.F[i];
        for(i=0; i<GpG.num_sub_agenda_IMG; i++) {
          
               tp ++;  
                /* start state */
               //printf("\nstart state ");print_state(*start_state);printf("\n");
                                
                /* end state */
                for(k=0; k<GpG.sub_agenda_IMG[i].num_F;k++) {        
                    end_state->F[j+k] =  GpG.sub_agenda_IMG[i].F[k];
                }
                end_state->num_F = j + GpG.sub_agenda_IMG[i].num_F ;
                source_to_dest(&ggoal_state, end_state); 
	            //printf("\nend state ");print_state(*end_state);printf("\n");
               
		  
                /* -----------------------------------------------*/
             switch(GpG.esp_search){
             case 4:
                printf("service res = %d\n",
                    call_Mips_service(start_state, end_state, &mips_res_state));
                //print_real_state(mips_res_state);
                break;
             case 2:                        
                        
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
                 
                break;  
                
                /* -----------------------------------------------*/
             case 1:
                
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
                
                break;
 
               /* ------------------------------------------------ */
             case 3:   
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
               
                break;
                
             default: break; 
             }              
               
             /* prepare initial state */ 
             if((GpG.esp_search==1)||(GpG.esp_search==2)) {
                for(k=0; k<GpG.sol_state.num_F; k++) {
                    start_state->F[k] = GpG.sol_state.F[k];
                    ginitial_state.F[k] = GpG.sol_state.F[k]; 
                }   
                start_state->num_F = GpG.sol_state.num_F;
                ginitial_state.num_F = GpG.sol_state.num_F;
                for(k=0; k<gnum_comp_var; k++) {
                    start_state->V[k] = GpG.sol_state.V[k];
                    ginitial_state.V[k] = GpG.sol_state.V[k]; 
                }               
             }
             if(GpG.esp_search==4) {
                 var_source_to_dest(start_state, &mips_res_state);
                 var_source_to_dest(&ginitial_state, &mips_res_state);
             }
        }
	}						
    
    if(GpG.esp_search == 2) {
        load_lpg_gef_conn();
    }

    if((GpG.esp_search==1)||(GpG.esp_search==2)) {
        output_esp_solution(plan_actions);    
    }
   
    if(GpG.esp_search == 4) { 
        calloff_Mips_service(); 
        shutdownServer();	
    }
    printf("\ntotal subproblems = %d\n\n", tp);
    free(decr_g);
    return 1;   
}

int
subGoalSearch (State * start_state, State * end_state,
	     PlanAction ** plan_actions)
{
  int nulflip = 0, result =
    0, loc_init_time, tot_numtry, num_try_print, num_run = 0;

  struct tms search_start, search_end, cputime;
  float plan_time, time_elapsed;
  int optimize = 0;

  GpG.count_num_try = 0;
  GpG.curr_goal_state = end_state;


  srandom (seed);
  gnoop_conn = gft_conn;
  times (&search_start);
  loc_init_time = GpG.curr_plan_length = GpG.max_plan_length - 1;
  

  do                                                                    
    {
      tot_numtry = GpG.numtry;
      if (GpG.numrun <= 0 || GpG.numtry <= 0)
	return (-1);
      if (DEBUG0 && !DEBUG1)
//	printf ("\n\nSearching ('.' = every 50 search steps):\n");
      for (num_run = 0; num_run < GpG.numrun; num_run++)
	{


	  if (FALSE && num_run == 1)
	    {
	      printf ("\nSTART1");
	      GpG.info_search = 5;

	    }

	  tot_numtry *= GpG.inc_run;
	  GpG.curr_plan_length = loc_init_time;

	  if (DEBUG0 && !DEBUG1 && num_run > 0)
	    printf (" Restart ");
	  
      
      switch(GpG.esp_search) {
        case 1:
            null_initialize (start_state, end_state, num_run);
            break;
        case 2:
            null_initialize (start_state, end_state, num_run);
            break;
        case 3:
            if(num_run % 3 == 0)
	            stored_initialize (start_state, end_state, num_run);
            else 
                null_initialize (start_state, end_state, num_run);
             //    stored_initialize (start_state, end_state, num_run);
             break;
        case 5:
             stored_initialize (start_state, end_state, num_run);
	        //  printf("GpG.num_false_tot = %d\n", GpG.num_false_tot);
	            
             // my_print_plan_all (GpG.curr_plan_length);
             break;
        default:
            printf("GpG.esp_search = %d (should be 1-3)\n", GpG.esp_search);
            exit(1);
      }
      
      
      //action subprob in plan
      mark_action_sub_prob (GpG.curr_plan_length);
      //print_actions_in_subgraph ();
     
      
      // Y. Chen
      if(GpG.total_num_subprob <= 1)  GpG.stage_boundary_level = 0;
      else GpG.stage_boundary_level = GpG.curr_plan_length;
      //printf("\nGpG.stage_boundary_level = %d\n",  GpG.stage_boundary_level);
      
      
      if (DEBUG6)
	    my_print_plan_all (GpG.curr_plan_length);
         
	   
        
      if (DEBUG1)
	    printf ("\n\n\n\n-----SEARCH START----- ");
	  fflush (stdout);

	  GpG.num_false_tot =
	    GpG.num_false_act + GpG.num_false_fa + GpG.num_false_num_fa;

	  if (optimize)
	    set_heuristic_parameters (num_run, GpG.num_solutions);

      GpG.inf_address_level = -1000;
      GpG.cut_RFF = 0;
      
	  for (num_try = 1, num_try_print = 1; 
              ((num_try < tot_numtry) && (GpG.cut_RFF != 1));
              //((num_try < tot_numtry) );
	       num_try++, num_try_print++, GpG.count_num_try++)
	    {


	      if (num_try_print >= 50)
		{
		  if (GpG.max_cputime > 0.0)
		    {
		      times (&cputime);

		      plan_time = DeltaTime (glob_start_time, cputime);

		      if (plan_time > GpG.max_cputime)
			{
			  printf ("\n\nMax cpu time exceeded\n\n");
			  exit (0);
			}

		      time_elapsed =
			(float) ((cputime.tms_utime - start_time.tms_utime +
				  cputime.tms_stime -
				  start_time.tms_stime) / 100.0);

		      if (time_elapsed > GpG.timeout
			  && GpG.found_plan != FALSE)
			{
			  printf ("TIMEOUT\n");
			  GpG.found_plan = TIMEOUT;
			  return (num_try + num_run * GpG.numtry);
			}

		    }

		  if (GpG.num_actions > 20
		      && GpG.curr_plan_length - GpG.num_actions * 3 >
		      GpG.curr_plan_length)
		    compress_vectlevel ();

		  if (DEBUG0 && !DEBUG1)
		    printf (".");
		  num_try_print = 1;
		}

	      set_param (GpG.num_false_tot);


	      if (DEBUG2)
		printf ("\n\n\n\n\n@@@@@ Search Step: %d", num_try);
	      {
		result = choose_act_fa (GpG.curr_plan_length);

		if (result == FALSE)
		  ++nulflip;
	      }

	      GpG.num_false_tot =
		GpG.num_false_act + GpG.num_false_fa + GpG.num_false_num_fa;
	

          //print_actions_in_subgraph ();

          /* 
	      printf("GpG.num_false_tot %d\n", GpG.num_false_tot);
	      printf("GpG.curr_plan_length %d\n", GpG.curr_plan_length);	
	      */	 	
	      
	      if (GpG.num_false_tot == 0)
		tot_numtry = 500;


	      if (GpG.num_false_tot == 0)
		{
		  check_num_prec ();

		  GpG.num_false_tot =
		    GpG.num_false_act + GpG.num_false_fa +
		    GpG.num_false_num_fa;

		}
	      if (GpG.num_false_tot == 0)
		optimize = continue_to_optimize ();

	      if (GpG.num_false_tot == 0
		  && (optimize == FALSE
		      || (num_try > 100
			  && GpG.initialize_from != INIT_EMPTY_PLAN)))
		break;
	      if (GpG.curr_plan_length >= MAX_PLAN_LENGTH)
		break;



	      if (GpG.lm_multilevel)
		{

		  update_precond_multilevel ();
		  update_decr_me_prec_multilevel ();

		}
	      else
		{
		  update_precond ();
		  update_decr_me_prec ();

		}

	    }

	  if (!GpG.temporal_plan && GpG.num_false_tot == 0)
	    compress_plan ();

	  /*
	  printf("GpG.num_false_act %d\n", GpG.num_false_act);
	  printf("GpG.num_false_fa %d\n", GpG.num_false_fa);
	  printf("GpG.num_false_num_fa %d\n", GpG.num_false_num_fa);
	  */
	  
	  if (GpG.num_false_act || GpG.num_false_fa || GpG.num_false_num_fa
	      || optimize)
	    {
	      if (DEBUG0 && !DEBUG1)
		{
		  if (optimize)
		    printf (" found solution of bad quality.");

		  else
		    printf (" search limit exceeded.");
		}
	    }

	  else
	    {
	      times (&search_end);
	      GpG.do_best_first = FALSE;
	      if (GpG.inc_choice_command_line)
		GpG.inc_choice_type = GpG.inc_choice_command_line;

	      plan_time = DeltaTime (search_start, search_end);

	      times (&glob_end_time);
	      gtotal_time =
		(float) ((glob_end_time.tms_utime -
			  glob_start_time.tms_utime +
			  glob_end_time.tms_stime -
			  glob_start_time.tms_stime) / 100.0);

	      if (DEBUG0 && !DEBUG1)
		//printf (" solution found: ");
	      if (DEBUG5)
		{
		  print_num_levels_and_actions ();
		  if (GpG.temporal_plan)
		    print_temporal_plan (GpG.curr_plan_length);
		}
	    /*
            if (!GpG.noout)
		    store_adapted_temporal_plan (GpG.curr_plan_length,
					     gcmd_line.fct_file_name,
					     plan_time);
	      */
                    
          store_curr_plan (GpG.curr_plan_length, plan_actions);


        /*

          if (DEBUG0)
		if (GpG.num_solutions >= GpG.max_num_solutions || GpG.noout)
		  print_actions_in_plan ();
	      if (GpG.maximize_plan && GpG.total_cost < 0)
		printf
		  ("\nSolution number: %d\nTotal time:      %.2f\nSearch time:     %.2f\nActions:         %d\nExecution cost:  %.2f\nDuration:        %.3f\nPlan quality:    %.3f",
		   GpG.num_solutions, gtotal_time, plan_time, GpG.num_actions,
		   GpG.total_cost * (-1), GpG.total_time,
		   GpG.total_cost * GpG.orig_weight_cost * (-1) +
		   GpG.total_time * GpG.orig_weight_time);
	      else
		printf
		  ("\nSolution number: %d\nTotal time:      %.2f\nSearch time:     %.2f\nActions:         %d\nExecution cost:  %.2f\nDuration:        %.3f\nPlan quality:    %.3f",
		   GpG.num_solutions, gtotal_time, plan_time, GpG.num_actions,
		   GpG.total_cost, GpG.total_time,
		   GpG.total_cost * GpG.orig_weight_cost +
		   GpG.total_time * GpG.orig_weight_time);

	      if (!GpG.noout)
		{
		  printf ("\n     Plan file:");
		  if (GpG.out_file_name)
		    printf ("       %s_%d.SOL", gcmd_line.out_file_name,
			    GpG.num_solutions);

		  else
		    printf ("       plan_%s_%d.SOL", gcmd_line.fct_file_name,
			    GpG.num_solutions);
		} */

	      fflush (stdout);

	      if (GpG.max_num_solutions > 0
		  && GpG.num_solutions >= GpG.max_num_solutions) {
                save_subgraph_sol_state();
                return (0);
          }
        
	      break;
	    }
	}
    }
  while (GpG.optimize && GpG.num_solutions > 0);

  return (num_try + num_run * GpG.numtry);
}

void save_subgraph_sol_state()
{
    int c,j, k, temp;
    int level = GpG.curr_plan_length;
   
   // my_print_plan_all(level);
    
    c = 0;
    for (j = 0; j < gnum_ft_block; j++)
    {                    
        temp = vectlevel[level]->fact_vect[j];
        k = 32;
        while (temp)
        { 
            k--;
            if (temp & FIRST_1) {
                GpG.sol_state.F[c++] = j * 32 + k;         
            }
            /*
                printf ("\n\t %s [%d] time_f: %.2f w_is_true %d",
                print_ft_name_string (j * 32 + k, temp_name),
                j * 32 + k,
                vectlevel[level]->fact[j * 32 + k].time_f,
                vectlevel[level]->fact[j * 32 + k].w_is_true);
            */
            temp <<= 1; 
        } 
    }  
    GpG.sol_state.num_F = c;
    
    for (j = 0; j < gnum_comp_var; j++)
    {
        GpG.sol_state.V[j] = vectlevel[level]->numeric->values[j]; 
    }
}

// mode 0: forward => backward
// mode 1: backward alone

int reduce_subspace_actions(State * start_state, int Gf, int mode)
{       
   int i, j, k, f, a;
   int level, F_level, B_level;
   int* rsa_facts = NULL;
   int* rsa_actions = NULL;
   int* back_rsa_facts;
   int* back_rsa_actions;
   int newfacts, found_Gf;
   int nfa, nba, adda;
   
   /* 
   for (i = 0; i < gnum_ef_conn; i++) {
        printf ("\n\neffect %d of op %d: ", i, gef_conn[i].op);
        print_op_name (gef_conn[i].op);
        printf(" sub_prob %d   num_pcc %d\n", gef_conn[i].sub_prob,
                gef_conn[i].num_pcc);      
   }*/
 
  /* restore */ 
  for(i=0 ; i<gnum_ef_conn; i++) { 
        if(GpG.op_pure_numeric[i] == 1)
         GpG.op_in_graph[i] = 1;
        else  
         GpG.op_in_graph[i] = 0;
  }
 
 if(mode == 0) { 
   /* initialization */ 
   restore_subspace_actions();
  
   found_Gf = 0;
   rsa_facts = (int *) calloc (gnum_ft_conn, sizeof (int)); 
   rsa_actions = (int *) calloc (gnum_ef_conn, sizeof (int)); 
   for (i = 0; i < gnum_ft_conn; i++) rsa_facts[i] = -1; 
   for(i=0; i<start_state->num_F; i++) {
       rsa_facts[start_state->F[i]] = 0;
       if(start_state->F[i] == Gf) {
           found_Gf = 1;
           return 0;
       }
   }
   for (i = 0; i < gnum_ef_conn; i++) rsa_actions[i] = -1;

   /*
    printf("\n--- Starting: "); printf(" ---");
    print_state(*start_state);
    printf("\n--- Gf: "); print_ft_name(Gf); printf(" ---\n");
    */
  /*
    printv(rsa_facts, gnum_ft_conn, "rsa_facts");
    printv(rsa_actions , gnum_ef_conn, "rsa_actions"); 
   */
    
   
   /* Forward until reach the goal */
   level = 0;
   nfa = 0;
   while(found_Gf == 0)
   {
       newfacts = 0; 
      
       /*
       printf("\nlevel %d\n", level);
       for (i = 0; i < gnum_ft_conn; i++) 
           if(rsa_facts[i] == level){ 
                print_ft_name(i); printf("\n");
           }
       printf("\n");
       */

       for (i = 0; i < gnum_ft_conn; i++) {
           if(rsa_facts[i] == level) {
                 newfacts = 1;
                 for (j = 0; j < gft_conn[i].num_PC; j++) {
                    a = gft_conn[i].PC[j];
                    if(rsa_actions[a] < 0) {
                      gef_conn[a].num_pcc--;
                      if(gef_conn[a].num_pcc == 0) {
                       
                        //printf("add action %d ", a);
                        //print_op_name(gef_conn[a].op); printf("\n");
                          
                        nfa ++;
                        rsa_actions[a] = level;
                        for (k = 0; k < gef_conn[a].num_A; k++) {
                            f = gef_conn[a].A[k];
                            if(rsa_facts[f] < 0) rsa_facts[f] = level+1; 
                            if(f == Gf) found_Gf = 1;
                        }
                        if (gef_conn[a].sf) {
                            for (k = 0; k < gef_conn[a].sf->num_A_start; k++) {
                                f = gef_conn[a].sf->A_start[k];
                                if(rsa_facts[f] < 0) rsa_facts[f] = level+1; 
                                if(f == Gf) found_Gf = 1;
                            }
                        }
                      }  
                    }
                 }
            }
       }
      
       if(newfacts != 1) break; 
       level++;
   }
             
   if(found_Gf) {
    //   printf("Gf found.\n", level);
       F_level = level-1; 
     //  printf("\nForward finished in %d levels. nfa = %d/%d. ",      F_level, nfa, gnum_ef_conn);
   } 
   else {       
       printf("not found the Gf %d\n", level);
       return -1;
   }
 } 
   
/* Backward from the goal */
    // initialization
    back_rsa_facts = (int *) calloc (gnum_ft_conn, sizeof (int)); 
    back_rsa_actions = (int *) calloc (gnum_ef_conn, sizeof (int)); 
    for (i = 0; i < gnum_ft_conn; i++) back_rsa_facts[i] = -1; 
    back_rsa_facts[Gf] = 0;
    for (i = 0; i < gnum_ef_conn; i++) back_rsa_actions[i] = -1;
    nba = 0;
    
    //printf("\nBackward\n");
    // backward search
    level = 0;
    while(1) {
        newfacts = 0; 
        
        /*
        printf("\n");
        for (i = 0; i < gnum_ft_conn; i++) 
           if(back_rsa_facts[i] == level){ 
                print_ft_name(i); printf("\n");
           }
        printf("\n");
        */

        for (i = 0; i < gnum_ft_conn; i++) {
             if(back_rsa_facts[i] == level) {
                newfacts = 1; 
                for (j = 0; j < gft_conn[i].num_A; j++) { 
                    a = gft_conn[i].A[j];
   
                    adda = 0;
                    if(mode == 1) {
                       if (back_rsa_actions[a] <0) adda = 1;
                    } else {
                       if((back_rsa_actions[a] <0) && (rsa_actions[a] >=0) ) 
                           adda = 1;
                    }                            
                   
                    if(adda == 1) 
                    {
                        
                        //if((level != 0) && (rsa_actions[a] ==F_level)) continue;  
                        
                       // if(level + rsa_actions[a] > F_level) continue;  
                     
                       // printf("add action %d level %d ", a, level);
                       // print_op_name(gef_conn[a].op); printf("\n");
                        
                        GpG.op_in_graph[a] = 1;
                                                
                        nba ++; 
                        back_rsa_actions[a] = level;   
                        
                        for (k = 0; k < gef_conn[a].num_PC; k++) {
                            f = gef_conn[a].PC[k];
                            if(back_rsa_facts[f] < 0) back_rsa_facts[f]=level+1;
                        }
                        if(gef_conn[a].sf) {
                            for (k = 0; k < gef_conn[a].sf->num_PC_overall; k++){
                                f = gef_conn[a].sf->PC_overall[k];
                               if(back_rsa_facts[f] < 0) back_rsa_facts[f]=level+1;
                            }
                            for (k = 0; k < gef_conn[a].sf->num_PC_end; k++){
                               f = gef_conn[a].sf->PC_end[k];
                               if(back_rsa_facts[f] < 0) back_rsa_facts[f]=level+1;
                            }
                        }
                    }
                }                            
             }                
        }
        if ( newfacts != 1) break;
        level++;
    }    
    B_level = level; 
    printf("\nBackward finished in %d levels. nba = %d/%d. " , B_level, nba, gnum_ef_conn);
     

/* clean up and leave */
   if(mode == 0) {
        free(rsa_facts);
        free(rsa_actions);
   }
   free(back_rsa_facts); free(back_rsa_actions);   
   return nba;
}                           
  
void output_esp_solution(PlanAction ** plan_actions)
{
  int fd[2], pid, sgs;
  State * start_state, start_s;
  State * end_state, end_s; 
  int time, i, num, num_false_act, num_unsupported, j;
  FtConn *vertex_ft; 
  inform_list fa, inform_tofix, false_init_facts[MAX_FALSE];
  
  var_source_to_dest (&ginitial_state, &saved_ginitial_state);
  var_source_to_dest (&ggoal_state, &saved_ggoal_state);
  var_source_to_dest (&start_s, &saved_ginitial_state);
  var_source_to_dest (&end_s, &saved_ggoal_state);
  start_state = &start_s;
  end_state = &end_s;
  
  time = GpG.curr_plan_length = GpG.saved_fixpoint_plan_length;
  num_try = INITIALIZE_STEP;

  GpG.temporal_plan = TRUE;
 
  
 /* 
  if (1)
  {   
      time = GpG.input_plan_lenght = GpG.num_esp_sol;
      GpG.curr_plan_length = GpG.input_plan_lenght;
  }*/

  if (time + 1 < GpG.max_plan_length)
        for (i = time + 1; i < GpG.max_plan_length; i++)
                 temp_vectlevel[GpG.max_temp_vect++] = vectlevel[i];
   
  if (time >= GpG.max_plan_length) 
         for (i = GpG.max_plan_length; i <= time; i++)
                  vectlevel[i] = temp_vectlevel[--GpG.max_temp_vect];
   
     
  GpG.max_plan_length = time + 1;
  reset_plan (GpG.max_plan_length);
 
  
  num_unsupported = 0; 
  for (i = 0; i < end_state->num_F; i++)
    if (end_state->F[i] >= 0)
      {
    vertex_ft = &gft_conn[end_state->F[i]];
    CONVERT_FACT_TO_VERTEX (end_state->F[i])->lamda_prec =
      CONVERT_FACT_TO_VERTEX (end_state->F[i])->lamda_me = 1.0;
    CONVERT_FACT_TO_INFORM (end_state->F[i], time)->w_is_goal = TRUE;
    CONVERT_FACT_TO_INFORM (end_state->F[i], time)->w_is_used = TRUE;
    
    insert_unsup_fact (CONVERT_FACT_TO_INFORM (end_state->F[i], time));
    false_init_facts[num_unsupported] =
      CONVERT_FACT_TO_INFORM (end_state->F[i], time);
    num_unsupported++;
    vectlevel[time]->prec_vect[GUID_BLOCK (vertex_ft->position)] |=
      GUID_MASK (vertex_ft->position);
    vectlevel[time]->false_crit_vect[GUID_BLOCK (vertex_ft->position)] |=
      GUID_MASK (vertex_ft->position);
      
      
    backward_precond_propagation (CONVERT_FACT_TO_INFORM
                      (end_state->F[i], time));
    if (num_unsupported > MAX_GOALS)
    
      {
        printf ("\n\nipp-d: increase MAX_GOALS( preset value: %d )",
            MAX_GOALS);
        exit (1);
      } 
      } 
    else
      {
    j = -end_state->F[i];
    vectlevel[time]->numeric->w_is_goal[j]++;

    if (!is_num_prec_satisfied (j, time))
      insert_unsup_numeric_fact (j, time);
      
      }
      
  vectlevel[time]->num_prec = num_unsupported;
  GpG.num_prec = num_unsupported;
  if (GpG.temporal_plan)
    GpG.forward_time = 1;
  
    
  for (num = 0, i = 0; i < start_state->num_F; i++, num++)
  
    {
      vertex_ft = &gft_conn[start_state->F[i]];
      (fa = CONVERT_FACT_TO_INFORM (start_state->F[i], 0))->w_is_true = TRUE;
      vectlevel[0]->fact_vect[GUID_BLOCK (vertex_ft->position)] |=
    GUID_MASK (vertex_ft->position);
      if (fa->w_is_goal)
      
    { 
      vectlevel[0]->true_crit_vect[GUID_BLOCK (vertex_ft->position)] |=
        (GUID_MASK (vertex_ft->position));
      vectlevel[0]->false_crit_vect[GUID_BLOCK (vertex_ft->position)] &=
        ~(GUID_MASK (vertex_ft->position));
    }   
    
    
      forward_noop_propagation (start_state->F[i], 0);
      
      {
    vectlevel[0]->fact[start_state->F[i]].time_f = 0.0;
    forward_noop_propagation_time (&vectlevel[0]-> 
                       noop_act[start_state->F[i]]);
      }                
    } 
  
    vectlevel[0]->num_fact = num;
    num = 0;
     
    GpG.initialize_from = PLAN_ADAPT;
    if (GpG.num_solutions)  time = GpG.input_plan_lenght;
      
                  
    for (i = 0; i<GpG.num_esp_sol; i++)   
    {               
      //if (DEBUG2)
      //  printf ("\nESP composing->insert action %s  in level %d",
      //      print_op_name_string (GpG.esp_solution[i], temp_name), i);
                                                                     
      GpG.num_false_tot =
        (GpG.num_false_act + GpG.num_false_fa + GpG.num_false_num_fa);
        
      if (i >= gef_conn[GpG.esp_solution[i]].level)    
        insert_remove_action (GpG.esp_solution[i], i, C_T_INSERT_ACTION,
                  GpG.approximation_level);
                  
      GpG.num_false_tot =
        (GpG.num_false_act + GpG.num_false_fa + GpG.num_false_num_fa);
    }   
  

      
    while (GpG.num_false_act > 0)
    { 
      inform_tofix =
        CONVERT_NOOP_TO_INFORM (treated_c_l[0]->fact,
                    *treated_c_l[0]->level);
      num_false_act =
        choose_actions_treated_fact (inform_tofix, GpG.curr_plan_length);
      if (num_false_act <= 0)
        remove_treated_noop (inform_tofix);
    }
     
    GpG.num_false_tot =
        (GpG.num_false_act + GpG.num_false_fa + GpG.num_false_num_fa);
   
    //Y. Chen
    //my_print_plan_all(GpG.curr_plan_length);
    
    if(GpG.num_false_tot == 0) {
      //printf("\n\nESP solution synthized.\nnum_false_tot = %d\n", GpG.num_false_tot);             
	      
        times (&glob_end_time);
        gtotal_time =
            (float) ((glob_end_time.tms_utime -
			  glob_start_time.tms_utime +
			  glob_end_time.tms_stime -
			  glob_start_time.tms_stime) / 100.0);

        store_adapted_temporal_plan (GpG.curr_plan_length,
                gcmd_line.fct_file_name,
                gtotal_time);
    }                   
    else {
       // printf("num_false_tot %d != 0 length %d\n", GpG.num_false_tot,
        //        GpG.curr_plan_length);
     //   noInitLocalSearch(start_state, end_state, plan_actions); 
	    
        store_curr_plan (GpG.curr_plan_length, plan_actions);
                if(pipe(fd)<0) {
			        printf("pipe can't be created\n");
			        exit(1);
		        }
		        if((pid=fork()) < 0){
			        printf("fork failed\n");
			        exit(1);
		        } else if(pid == 0) {  
			        close(fd[0]);
            
                    GpG.esp_search = 5;
                    sgs = subGoalSearch(start_state, end_state, plan_actions);        
                    
         //           printf("GpG.curr_plan_length %d\n", GpG.curr_plan_length); 
                    times (&glob_end_time);
                    gtotal_time =
                (float) ((glob_end_time.tms_utime -
			  glob_start_time.tms_utime +
			  glob_end_time.tms_stime -
			  glob_start_time.tms_stime) / 100.0);
                    store_adapted_temporal_plan (GpG.curr_plan_length, gcmd_line.fct_file_name, gtotal_time);
	
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
    }   
}

Bool contain_fact_state(State *state, int f)
{
    int i;
   
   if(state == NULL) return FALSE; 
    if(!state) return FALSE; 
    for(i=0; i<state->num_F; i++) {
        if(state->F[i] == f) return TRUE;
    }
    return FALSE;
}

int
distributedLocalSearch (State * start_state, State * end_state,
	     PlanAction ** plan_actions)
{
	int j,k;
	int fd[2];
	int pid;
	int sgs;
	State saved_end_state;
    State mips_res_state;
   
    if(GpG.esp_search == 4) serverMips();
	 
    /* save ginitial_state and ggoal_state before search */
    var_source_to_dest(&saved_ginitial_state, &ginitial_state);
    var_source_to_dest(&saved_ggoal_state, &ggoal_state);
    var_source_to_dest(start_state, &ginitial_state);
    var_source_to_dest(&GpG.sol_state, &ginitial_state);
            
    /* initialize */
	printf("\n\n<**** ESP planning ****>\n\n");
    init_subspace_actions();
    GpG.total_num_subprob = 0;
    GpG.num_esp_sol = 0; 
   
    /* goal ordering */
    init_h1_h2();
    random_twist_goal_order(end_state);
    source_to_dest(&saved_end_state, end_state); 
	printf("\ntwisted end state ");print_state(*end_state);
	printf("\n");
           
        
    /* esp search */
	for (j = 0; j < saved_end_state.num_F; j++)	     
	{
        GpG.total_num_subprob++;

        while(1) {
      
          if(contain_fact_state(&GpG.sol_state, saved_end_state.F[j])) break;
          find_intermediate_goals(&GpG.sol_state, saved_end_state.F[j]);
        
          //for(i=0; i<j; i++) end_state->F[i] = saved_end_state.F[i];
        //for(i=0; i<GpG.num_sub_agenda_IMG; i++) {
            
                /* start state */
                //printf("\nstart state ");
                print_state(*start_state);printf("\n");
                                
                /* end state */
                for(k=0; k<GpG.sub_agenda_IMG[0].num_F;k++) {        
                    end_state->F[k] =  GpG.sub_agenda_IMG[0].F[k];
                }
                end_state->num_F = GpG.sub_agenda_IMG[0].num_F ;
                source_to_dest(&ggoal_state, end_state); 
	            //printf("\nend state ");print_state(*end_state);printf("\n");
               
		  
                /* -----------------------------------------------*/
             switch(GpG.esp_search){
             case 4:
                printf("service res = %d\n",
                    call_Mips_service(start_state, end_state, &mips_res_state));
                var_source_to_dest(&GpG.sol_state, &mips_res_state);
                //print_real_state(mips_res_state);
                break;
             case 2:                        
                        
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
                 
                break;  
                
                /* -----------------------------------------------*/
             case 1:
                
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
                
                break;
 
               /* ------------------------------------------------ */
             case 3:   
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
               
                break;
                
             default: break; 
             }              
               
             /* prepare initial state */ 
             if((GpG.esp_search==1)||(GpG.esp_search==2)
                ||(GpG.esp_search==4)) {
                 var_source_to_dest(start_state, &GpG.sol_state);
                 var_source_to_dest(&ginitial_state, &GpG.sol_state);
             }
        //}

        }						
    }

    if((GpG.esp_search==1)||(GpG.esp_search==2)) {
        output_esp_solution(plan_actions);    
    }
   
    if(GpG.esp_search == 4) { 
        calloff_Mips_service(); 
        shutdownServer();	
    }
    return 1;
}
