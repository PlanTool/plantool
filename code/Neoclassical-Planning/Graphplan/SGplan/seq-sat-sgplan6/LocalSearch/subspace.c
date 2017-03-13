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

 * File: subspace.c

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
#include "lpg.h"
#include "inst_final.h"
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
#include <sys/wait.h>
#include <sys/time.h>
#include <sys/resource.h>
#include "esp.h" 
#include "ff.h"
#include "orderings.h"
#include "subspace.h"
#include "dis_ff.h"
#include "dis_search.h"
#include "dis_output.h"
#include "esp1.h"

extern int *lch;
extern int lnum_ch;
extern Bool *lin_ch;
extern int *lDcount;
extern Bool *lin;
extern Bool **lm;

extern vState known_iga_list;
extern int dis_ginfo;

#define MAXNG 20
#define MAX_LEVELS 200


int *fact_supp[MAX_LEVELS];
int *_fact_supp[MAX_LEVELS];
int graph_level;

    int ggs[MAXNG];
    int ngoals;
//    State gsym[MAXNG];
extern int max_ff_iter;

SymmConn Symmetry[MAX_SYMM];
char* SymmType[MAX_SYMM_TYPE];
int symm_num_type;
SymmGroup SymmetryGroup[MAX_SYMM_GROUP];
int SymmGroupType[MAX_SYMM_GROUP][MAX_SYMM_TYPE];
Bool SymmGroupConn[MAX_SYMM_GROUP][MAX_SYMM_GROUP];
int SymmGroupNorm[MAX_SYMM_GROUP][MAX_SYMM_GROUP];
char* SymmGroupFar[MAX_SYMM_GROUP][MAX_SYMM_GROUP];
int SymmNormGl[MAX_SYMM];
int SymmNormGr[MAX_SYMM];
char* symm_stu[MAX_SYMM];
char* symm_stu_far[MAX_SYMM];
int symm_num_stu;
char* symm_logo[MAX_SYMM];
char* symm_loko[MAX_SYMM];
int symm_num_lo;
int gnum_symm=0;
int gnum_symm_group=0;
int gnum_symm_norm;
int gnum_symm_subnorm;
State* gsolved_state;
Bool SymmCompart;
int *SymmOpTypeCon;

int SymmConstrEvalOption;
extern void source_to_dest( State *, State *);
extern void print_state(State);
extern void mff_start_and_goal(State *, State *);
extern Bool do_best_first_search( void );

void v1_v2_source_to_dest( State *dest, dis_State *source )
{
      int i;
      for ( i = 0; i < source->num_F; i++ ) {
                dest->F[i] = source->F[i];
      }
      dest->num_F = source->num_F;
}


void append_state(State *v, State *w)
{
    int i;
    for(i=0; i<w->num_F; i++) {
        v->F[v->num_F++] = w->F[i];
    }
}
    
void init_permuter(int *v, int n_rol, int low)
{
    int i;
    
    if(v!=NULL) free(v);
    
    v = (int *) calloc(n_rol, sizeof(int));
    for(i=0; i<n_rol; i++) {
        v[i] = low;            
    }
}
    
Bool permuter(int action, int *v, int n_rol, int high)
{
    int i=n_rol-1;
    while(1) {
       v[i]++;
       if(v[i]<high) break;
       else {
            v[i] = 0;
            i--;
            if(i<0) return FALSE;
       }
    }
    
    return TRUE;
}
    
// require FF strips gef_conn in all these functions 

Bool actionable(State * s, int a)
{
    int i=0;
   // printf("check actionability of");print_op(a); 
    for(i=0; i<gef_conn[a].num_PC; i++) {
          if(!contain_fact_state(s, gef_conn[a].PC[i])) {
     //         printf("not actionable b/c"); print_ft(gef_conn[a].PC[i]);
              return FALSE;                                         
          }
    }
    return TRUE;
}

// at most one x;
void del_v_x(int *v, int x, int *size)
{
    int i,j;
    for(i=0;i<(*size)-1;i++) {
        if(v[i]!=x) continue;
        for(j=i;j<(*size)-1;j++) {
            v[j] = v[j+1];
        } 
        (*size)--;   
        return;
    }
}

void print_op_state(State *v, char *str)
{
    int i;
    for(i=0; i<v->num_F; i++) {
        printf("%s %d",str,i);
        print_op(v->F[i]);
    }   
}

void del_v_i(int *v, int i, int *size)
{
    int j;
    for(j=i;j<(*size)-1;j++) {    
        v[j] = v[j+1];
    } 
    (*size)--;   
}

Bool a_ima_same_prec(int a, int b)
{           
    int i,j;
    for(i=0; i<gef_conn[a].num_PC; i++) {
        print_ft_name(gef_conn[a].PC[i]); printf(" for a\n");
    }
    printf("\n");
    for(i=0; i<gef_conn[b].num_PC; i++) {
        print_ft_name(gef_conn[b].PC[i]); printf(" for b\n");
    }
    printf("\n");
        
     
    for(i=0; i<gef_conn[a].num_PC; i++) {
        for(j=0; j<gef_conn[b].num_PC; j++) {
            if(gef_conn[a].PC[i] == gef_conn[b].PC[j]) return TRUE;
        }
    }
    return FALSE;
}

Bool strong_a_ima_conflict(int a, int b)
{
    int i,j,f;
    for(i=0; i<gef_conn[a].num_D; i++) {
        f = gef_conn[a].D[i];
        for(j=0; j<gef_conn[b].num_PC; j++) {
            if( f == gef_conn[b].PC[j] ) return TRUE;
        }
    }
    for(i=0; i<gef_conn[a].num_A; i++) {
        f = gef_conn[a].A[i];
        for(j=0; j<gef_conn[b].num_PC; j++) {
            if(GET_BIT (FT_FT_mutex[f], gef_conn[b].PC[j] )) return TRUE;
        }
    }
    for(i=0; i<gef_conn[a].num_PC; i++) {
        f = gef_conn[a].PC[i];
        for(j=0; j<gef_conn[a].num_D; j++) {
            if(gef_conn[a].D[j] == f) break;
        }
        if(j<gef_conn[a].num_D) continue;
        for(j=0; j<gef_conn[b].num_PC; j++) {
            if(GET_BIT (FT_FT_mutex[f], gef_conn[b].PC[j] )) return TRUE;
        }
    }
    return FALSE;
}

Bool a_ima_conflict(int a, int b)
{
    int i,j,f;
   
   /* 
    if(a == 383) {
        for(i=0;i<gef_conn[a].num_A; i++) {
            printf("%d ", gef_conn[a].A[i]);
            print_ft_name(gef_conn[a].A[i]);
            printf("\n");
        }
    }*/
    
    for(i=0; i<gef_conn[a].num_D; i++) {
        f = gef_conn[a].D[i];
        for(j=0; j<gef_conn[b].num_PC; j++) {
            if( f == gef_conn[b].PC[j] ) return TRUE;
        }
    }
    for(i=0; i<gef_conn[a].num_A; i++) {
        f = gef_conn[a].A[i];
        for(j=0; j<gef_conn[b].num_PC; j++) {
            if(GET_BIT (FT_FT_mutex[f], gef_conn[b].PC[j] )) return TRUE;
        }
    }
    /*
    for(i=0; i<gef_conn[a].num_PC; i++) {
        f = gef_conn[a].PC[i];
        for(j=0; j<gef_conn[a].num_D; j++) {
            if(gef_conn[a].D[j] == f) break;
        }
        if(j<gef_conn[a].num_D) continue;
        for(j=0; j<gef_conn[b].num_PC; j++) {
            if(GET_BIT (FT_FT_mutex[f], gef_conn[b].PC[j] )) return TRUE;
        }
    }*/
    return FALSE;
}


Bool check_omita_ext(State *seleta, State *symfs, int* omi_select) 
{
    int i,x,a,b,k;
   
    for(k=0; k<symfs->num_F; k++) { 
        x = symfs->F[k];
           
       // printf("about fact");print_ft(x);
       
        for(i=0; i<seleta->num_F; i++) {
            a = seleta->F[i];
          //  printf("seleta");print_op(a);
            
            if(!in_array(gef_conn[a].PC, gef_conn[a].num_PC, x)) continue;
            
            b = gft_conn[x].A[omi_select[k]];
          //  printf("check_omita_ext for %d %d\n", b, a);
            if(a_ima_conflict(b, a)) {
            //    printf("check_omita_ext FALSE\n");
                return FALSE;
            }
        }
   }
    return TRUE;
}

Bool pred(int f, int g)
{
    int i;
    for(i=0; i<iga_graph[g].in_d; i++) {
        if(f == iga_graph[g].w[i]) return TRUE; 
    }
    return FALSE;
}

void p_order_needf(vState *needf)
{
    int i,j,t;
    for(i=0; i<needf->num_F-1; i++) {
        for(j=i+1; j<needf->num_F; j++) {
            if(pred(needf->F[j], needf->F[i])) {
                //printf("swap %d %d\n", needf->F[i], needf->F[j] );
                t = needf->F[i];
                needf->F[i] = needf->F[j];
                needf->F[j] = t;     
            } 
        }
    }
}

void p_order_state(State *needf)
{                   
    int i,j,t;
    for(i=0; i<needf->num_F-1; i++) {
        for(j=i+1; j<needf->num_F; j++) {
            if(pred(needf->F[j], needf->F[i])) {
                //printf("swap %d %d\n", needf->F[i], needf->F[j] );
                t = needf->F[i];
                needf->F[i] = needf->F[j];
                needf->F[j] = t;     
            } 
        }
    }
}
void fid_order_needf(vState *needf)
{
    int i,j,t;
    for(i=0; i<needf->num_F-1; i++) {
        for(j=i+1; j<needf->num_F; j++) {
            if(needf->F[i] > needf->F[j]) {
                t = needf->F[i];
                needf->F[i] = needf->F[j];
                needf->F[j] = t;     
            } 
        }
    }
}

void activate(State *s, int a)
{
    State t;
    int i,j,f;
    
    for(i=0; i<gef_conn[a].num_A;i++) {
        f = gef_conn[a].A[i];
        if(!contain_fact_state(s, f)) {
            s->F[s->num_F++] = f;
        }
    }
    
    t.num_F = 0;
    for(j=0; j<s->num_F; j++) {
        f = s->F[j];
        for(i=0; i<gef_conn[a].num_D;i++) {
            if(f == gef_conn[a].D[i]) break;
        }
        if(i==gef_conn[a].num_D) t.F[t.num_F++] = f;
    }
    
    source_to_dest(s, &t);
}

int select_action(State *S, State *v)
{
    State t;
    int i,j;
    for(i=0; i<v->num_F; i++) {
        source_to_dest(&t, S);
        activate(&t, v->F[i]);
        for(j=0; j<v->num_F; j++) {
            if(j==i) continue;
            if(!actionable(&t, v->F[j])) break;
        }
        if(j==v->num_F) return (v->F[i]);
    }
    printf("selection failed.\n");
    exit(0);
}

int type_select_action(State *S, State *v, State *va)
{
    int i,f = -1,a, mint, minf;
    mint = 11;
    for(i=0; i<va->num_F; i++) {
        f = va->F[i];
        if(iga_graph[f].ima_nailed)
        if(iga_graph[f].ima_type < mint) {
            mint = iga_graph[f].ima_type;
            minf = f;
        }        
    }
   
    
    a = iga_graph[f].ima[0];
    
    return a;
}

int strong_type_select_action(State *S, State *v, State *va)
{
    State t;
    int i,f,a = -1, mint, minf,j;
    mint = 11;
    for(i=0; i<va->num_F; i++) {
        f = va->F[i];
        if(iga_graph[f].ima_nailed)
        if(iga_graph[f].ima_type < mint) {
            mint = iga_graph[f].ima_type;
            minf = f;
        }        
    }

    for(i=0; i<va->num_F; i++) {
        f = va->F[i];
        if(iga_graph[f].ima_nailed)
          if(iga_graph[f].ima_type  == mint) {
            a = iga_graph[f].ima[0];
            source_to_dest(&t, S);
            activate(&t, a);
            for(j=0; j<v->num_F; j++) {
                if(a == v->F[j]) continue;
                if(!actionable(&t, v->F[j])) {
   //                 printf("XXXXXXXXXXXXX\n");
                    break;
                }
            }        
            if(j==v->num_F) break; 
          }        
    }
    
    
    return a;
}

Bool in_ima_list(int f, int a)
{
    int i;
    for(i=0; i<iga_graph[f].n_ima; i++) {
        if(a == iga_graph[f].ima[i]) return TRUE;
    }
    return FALSE;
}

Bool can_move(int g, int a, int f, int b)
{ 
    int p,i,j;
    for(i=0; i<iga_graph[g].n_imf; i++) {
        p = iga_graph[g].imf[i];
        if(p==f) continue;
        for(j=0; j<gft_conn[p].num_PC; j++) {
            if(a == gft_conn[p].PC[j]) 
                if(!a_ima_conflict(a, b))
                  return TRUE;
        } 
    }
    return FALSE;
}

Bool in_array(int *arr, int size, int x)
{
    int i;
    for(i=0; i<size; i++)
        if(arr[i] == x) return TRUE;
    return FALSE;
}

Bool add_fs(State *v, int x)
{
    if(!contain_fact_state(v,x)){
        v->F[v->num_F++] = x;
        return TRUE;
    }
    return FALSE;
}

void print_ft(int f)
{
    printf(" ft %d ",f); print_ft_name(f);
    printf("\n");
}

void print_op(int a)
{
    printf(" op %d ",a); print_op_name(a);
    printf("\n");
}

Bool goal_reached(State *s, int *rsa_facts)
{
    int i,f;
    Bool ret = TRUE;
    
    for(i=0; i<s->num_F; i++) {
        f = s->F[i];
        if(rsa_facts[f] < 0) {
//            printf("not reachable"); print_ft(f);
            ret = FALSE;
        }
    }
    return ret;
}


int n_more_analysis(int g, vState* needf, State *v, State *va)
{
    int n_more, i,j,k,f,a,gg,th;
    State v1;
    
    v->num_F = 0;
    n_more =0;
    
    for(i=0; i<needf->num_F;i++)  {
        f = needf->F[i];
        if((iga_graph[f].inn<0)&& (pred(f,g)||(f==g)) ) {
    //        printf("add "); print_ft(f); 
            v->F[v->num_F++] = f;
        }
    }
   
     
    while(1) {
     
        va->num_F = 0;
        v1.num_F = 0;
        for(i=0; i<v->num_F;i++)  {           
            f = v->F[i]; 
            if(iga_graph[f].ima_nailed){
              a = iga_graph[f].ima[0];
              if(add_fs(va, a)){
                     
      //          print_op(a);
                for(j=0; j<gef_conn[a].num_A; j++) {
                    gg = gef_conn[a].A[j];
        //            if(a==477) {printf("477"); print_ft(gg);}
                    if(!v_contain_fact_state(needf,gg)) continue;
                    if(contain_fact_state(v,gg)) continue;
                    if(iga_graph[gg].inn>0) continue;
          //          printf("some");print_ft(gg);
                    for(k=0; k<v->num_F;k++)  {
                        g=v->F[k];
                        if(pred(g,f) || g==f) continue;
                        if(GET_BIT (FT_FT_mutex[g], gg)) {
            //              printf("abandon");print_ft(g);
                            th = gg;
                            if(iga_graph[gg].inn<0) add_fs(&v1,th);
                           
                            add_fs(&v1,th);
                           
                            while(GET_BIT (FT_FT_mutex[g], th)){
                                if(iga_graph[th].n_fof==0) break;
                                if(iga_graph[th].inn>0) break;
                                th = iga_graph[th].fof[0];
                                add_fs(&v1,th);
              //                  printf("link");print_ft(th);
                            }       
                        }
                    }
                }
             }
            }
        }
   
        if(v1.num_F == 0) break;
        
        for(j=0; j<v1.num_F; j++) {
            f =v1.F[j];
            if(add_fs(v, f)) {
                n_more++; 
     //           printf("more");print_ft(f);
            }
            for(i=0; i<iga_graph[f].in_d; i++) {
                if(add_fs(v, iga_graph[f].w[i])) {
                    n_more++; 
     //               printf("more");print_ft(iga_graph[f].w[i]);
                }   
            }
        }
    }      
    return n_more; 
}


void lpg_bridge(State *S, State *T)
{
    int fd[2], status;    
    int pid;
    PlanAction *temp_act;    
    
    source_to_dest(&ginitial_state, S);
    source_to_dest(&ggoal_state, T);
    GpG.esp_search = 1;
    
    if(pipe(fd)<0) {
         printf("pipe can't be created\n");
         exit(1);
    }
    if((pid=fork()) < 0){
        printf("fork failed\n");
        exit(1);
    } else if(pid == 0) { 
        close(fd[0]);
       
        subGoalSearch(S, T, g_plan_actions);
       
        gnum_plan_ops = 0;
        for (temp_act = *g_plan_actions; temp_act; temp_act = temp_act->next) {
            gplan_ops[gnum_plan_ops++] = temp_act->act_pos;               
        } 
        
        ff_send_plan_actions(fd[1]);
        send_sol_state(fd[1]);       
        close(fd[1]);
        exit(0); 
    } else {
        close(fd[1]); 
        ff_recv_plan_actions(fd[0]);
        recv_sol_state(fd[0]);
        close(fd[0]);  
        wait(&status);
    }
}    

void dis_mff_bridge()
{
    int fd[2], status;    
    int pid;
    Bool fplan;
    struct rusage rusage;
    struct rlimit rlim;
    float time;
    
    rlim.rlim_cur = GpG.max_cputime - GpG.glob_dist_time + 1;
    rlim.rlim_max = GpG.max_cputime - GpG.glob_dist_time + 1;
    if(pipe(fd)<0) {
         printf("pipe can't be created\n");
         exit(1);
    }
    if((pid=fork()) < 0){
        printf("fork failed\n");
        exit(1);
    } else if(pid == 0) { 
        close(fd[0]);
        if (GpG.max_cputime > 0.0)
          setrlimit(RLIMIT_CPU, &rlim);
  
        times(&dis_dist_start);
        
        dis_gnum_plan_ops = 0;
        
        if(bridge_option == 0) {  
            if(!dis_do_enforced_hill_climbing()) {
                fplan = dis_do_best_first_search();
            } else {
                fplan = TRUE;
            }
        } else {
            if(bridge_option == 1) {
                fplan = dis_do_best_first_search();
            } else {
                printf("wrong bridge_option %d\n", bridge_option);
                exit(1);
            }
        }

        if(!fplan) {
            dis_gnum_plan_ops = -1;
        } 
        
        times(&dis_dist_end);
        time = (float) ((dis_dist_end.tms_utime -
                      dis_dist_start.tms_utime +
                      dis_dist_end.tms_stime - 
                      dis_dist_start.tms_stime) / 100.0);
        //printf("Time = %f\n", time);
         
//        mff_send_time(fd[1], time);
        mff_send_plan_actions(fd[1]);
        dis_send_sol_state(fd[1]);       
        close(fd[1]);
        exit(0); 
    } else {
        close(fd[1]); 
//        mff_recv_time(fd[0]); 
        mff_recv_plan_actions(fd[0]);
        dis_recv_sol_state(fd[0]);
        close(fd[0]);
// Chih-Wei
        wait3(&status, 0, &rusage);
        GpG.glob_dist_time = GpG.glob_dist_time + rusage.ru_utime.tv_sec + 
        0.000001*rusage.ru_utime.tv_usec + rusage.ru_stime.tv_sec + 0.000001*rusage.ru_stime.tv_usec;
        if (GpG.max_cputime > 0.0 && GpG.glob_dist_time > GpG.max_cputime)
        {
                printf ("Max cpu time exceeded\n");
                exit(0);
        }
    }
}    

void mff_bridge(State *S, State *T)
{
    // Chih-Wei
    int status;
    struct rusage rusage;
    struct rlimit rlim;
    int fd[2];    
    int pid;
    Bool fplan;
    float time;
    
    rlim.rlim_cur = GpG.max_cputime - GpG.glob_dist_time + 1;
    rlim.rlim_max = GpG.max_cputime - GpG.glob_dist_time + 1;
    if(pipe(fd)<0) {
         printf("pipe can't be created\n");
         exit(1);
    }
    if((pid=fork()) < 0){
        printf("fork failed\n");
        exit(1);
    } else if(pid == 0) { 
        close(fd[0]);
        if (GpG.max_cputime > 0.0)
          setrlimit(RLIMIT_CPU, &rlim);
    
        times(&dis_dist_start);
        
        mff_start_and_goal(S, T); 
    
	// Chih-Wei
        if(bridge_option == 0) {  
            if(!dis_do_enforced_hill_climbing()) {
            	if (GpG.SearchModal == 3 && GpG.is_til)
            	{
            		fplan = FALSE;
            	}
            	else
                fplan = dis_do_best_first_search();
            }
            else {
            	fplan = TRUE;
            }	
        } else {
            if(bridge_option == 1) {
                fplan = dis_do_best_first_search();
            } else {
                printf("wrong bridge_option %d\n", bridge_option);
                exit(1);
            }
        }

        if(!fplan) {
            dis_gnum_plan_ops = -1;
        } 
        else {    
            //printf("dms\n");dis_print_dis_State(dis_mff_sol);
            
            v1_v2_source_to_dest (&GpG.sol_state, &dis_mff_sol);        
        }
        
        times(&dis_dist_end);
        time = (float) ((dis_dist_end.tms_utime -
                      dis_dist_start.tms_utime +
                      dis_dist_end.tms_stime - 
                      dis_dist_start.tms_stime) / 100.0);
        //printf("Time = %f\n", time);
        
//        mff_send_time(fd[1], time);
        mff_send_plan_actions(fd[1]);
        send_sol_state(fd[1]);
        close(fd[1]);
        exit(0); 
    } else {
        close(fd[1]); 
//        mff_recv_time(fd[0]); 
        mff_recv_plan_actions(fd[0]);
        recv_sol_state(fd[0]);
        close(fd[0]);
// Chih-Wei
        wait3(&status, 0, &rusage);
        GpG.glob_dist_time = GpG.glob_dist_time + rusage.ru_utime.tv_sec + 
        0.000001*rusage.ru_utime.tv_usec + rusage.ru_stime.tv_sec + 0.000001*rusage.ru_stime.tv_usec;
        if (GpG.max_cputime > 0.0 && GpG.glob_dist_time > GpG.max_cputime)
        {
                printf ("Max cpu time exceeded\n");
                exit(0);
        }
    }
    
    //print_state(GpG.sol_state);
}    

void ff_bridge(State *S, State *T, int max_ff)
{
    int fd[2], status;    
    int pid;
//    int i;    
    
    source_to_dest(&ginitial_state, S);
    source_to_dest(&ggoal_state, T);
    max_ff_iter = max_ff;
    

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
        for(i=0; i<known_iga_list.num_F; i++) {
            printf("known_iga_list"); print_ft(known_iga_list.F[i]);
        }*/
       
        if(!do_best_first_search ()) {
          ///  printf(">>>>>>> best first failed! <<<<<<<\n\n"); 
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
        wait(&status);  
    }
}    


Bool same_action(int a, int b)
{
    int i;
    if(gef_conn[a].num_A != gef_conn[b].num_A) return FALSE; 
    if(gef_conn[a].num_D != gef_conn[b].num_D) return FALSE; 
    if(gef_conn[a].num_PC != gef_conn[b].num_PC) return FALSE; 

    for(i=0; i<gef_conn[a].num_A; i++) {
        if(!in_array(gef_conn[b].A, gef_conn[b].num_A, gef_conn[a].A[i]))
            return FALSE;
    }
    for(i=0; i<gef_conn[a].num_D; i++) {
        if(!in_array(gef_conn[b].D, gef_conn[b].num_D, gef_conn[a].D[i]))
            return FALSE;
    }
    for(i=0; i<gef_conn[a].num_PC; i++) {
        if(!in_array(gef_conn[b].PC, gef_conn[b].num_PC, gef_conn[a].PC[i]))
            return FALSE;
    }
    
    return TRUE;
}                                       
    
int run_depend(State *start_state, vState *needf)
{
    int index =0;
    int i,j,a,nexta;
    State v;
    State S;
    State T;
    Bool self_aid;
    
    free(iga_graph); 
    source_to_dest(&S, start_state);
    //print_state(S); printf("\n\n");
    
    v.num_F = 0;
    index =0; 
    while(1) {
       
        nexta = GpG.esp_solution[index++];
        
        if(contain_fact_state(&v, nexta)) continue;
        
        if(actionable(&S, nexta)) {
            activate(&S, nexta);     
            add_fs(&v, nexta);
           // printf("activate"); print_op(nexta);
        //    print_state(S); printf("\n\n"); 
        } else {
          
          //  printf("inactivate"); print_op(nexta);
           /* 
            while(!actionable(&S, nexta)) {
                a = full_select(&S, needf);
                if(a<0) break;
                activate(&S, a);
                add_fs(&v, a);
            }*/
           
           self_aid = FALSE; 
           for(i=index; i<GpG.num_esp_sol ; i++) {
               a = GpG.esp_solution[i];
               if(contain_fact_state(&v,a)) continue;
               source_to_dest(&T, &S);
               if(!actionable(&T, a)) continue;
               activate(&T, a);
               if(actionable(&T, nexta)) {
                    add_fs(&v, a);
           //         printf("selfaid-activate"); print_op(a);
                    activate(&S, a);
                    self_aid = TRUE;
                    break;
               }   
           }
            
           if(!self_aid) { 
            T.num_F = 0; 
            for(i=0;i<gef_conn[nexta].num_PC; i++) {
                add_fs(&T, gef_conn[nexta].PC[i]);
            }
            
           // exit(0);
        
            // prepare the known_iga_list
            known_iga_list.num_F = 0;
            for(i=index; (i<GpG.num_esp_sol) && (i<index); i++) {
                a = GpG.esp_solution[i];
                if(contain_fact_state(&v, a)) continue;
                printf("enter known_iga_list for"); print_op(a);
                for(j=0; j<gef_conn[a].num_PC; j++) {
                    known_iga_list.F[known_iga_list.num_F++] = gef_conn[a].PC[j];
                }         
            }
        
            //lpg_bridge(&S, &T);
           
            
            ff_bridge(&S, &T, 500);
            if(gnum_plan_ops < 0) {
                
                return -1;
            } else {
               for (i= 0; i<gnum_plan_ops; i++) {
                    add_fs(&v, gplan_ops[i]);
             //       printf("bridge-activate"); print_op(gplan_ops[i]);
               }
               source_to_dest(&S, &GpG.sol_state);
            }
           
            
            /*
            mff_bridge(&S, &T);
            if(dis_gnum_plan_ops < 0) {
                
                return -1;
            } else {
               for (i= 0; i<dis_gnum_plan_ops; i++) {
                    add_fs(&v, dis_gplan_ops[i]);
                    printf("bridge-activate"); print_op(dis_gplan_ops[i]);
               }
               source_to_dest(&S, &GpG.sol_state);
            }*/

           }
            
           if(actionable(&S, nexta)) { 
                activate(&S, nexta);
                add_fs(&v, nexta);
             //   printf("activate"); print_op(nexta);
           }
        }   
        
        if(index == GpG.num_esp_sol) break;  
    }
    
    GpG.num_esp_sol = 0;
    for(i=0; i<v.num_F; i++)  
        GpG.esp_solution[GpG.num_esp_sol++] = v.F[i];
   
    return 1;
}

int dependency_check(State* start_state, vState *needf)
{
    int subgoal, ng,n_more, i,j,k,f,a,g,kk,jj = 0;
    Bool bol;
    int gs[MAXNG];
    int nm[MAXNG];
    int nna[MAXNG], min_nna, not_nail;
    int nact[MAXNG];
    State v,va;
    State sol;
    
    for(i=0;i<MAXNG;i++) nact[i]=0; 
    p_order_needf(needf);
    
    //0.0 find out goals
    ng = 0;
    for(i=0; i<needf->num_F; i++) {
        f=needf->F[i];
        if(iga_graph[f].out_d == 0) {
            iga_graph[f].subgoal = ng;
            gs[ng++] = i;   
           // printf("Ending %d ",f);print_ft_name(f);printf("\n"); 
        }  
        if(ng==MAXNG) {printf("ng too small\n");exit(1);}
    }
  
    //0.1 p-order needf
    sol.num_F =0;
    
    //0.2 counting dependent iga
 while(1) {
    
    min_nna = 1000;
    k=1000;
    kk = 1000; 
    bol = TRUE;
  
    for(j=0; j<ng; j++) {
        not_nail = 0;
        g = needf->F[gs[j]];
        if(iga_graph[g].inn > 0) continue;
        bol = FALSE;
        
        for(i=0; i<iga_graph[g].in_d; i++) {
            f= iga_graph[g].w[i];
            if(!iga_graph[f].ima_nailed) not_nail++;
        }   
        nna[j] = not_nail;
        
        if( not_nail < min_nna ) {
            min_nna = not_nail;
        }   
    }          
    for(j=0; j<ng; j++) {
        g = needf->F[gs[j]];
        if(iga_graph[g].inn > 0) continue;
        if(nna[j] > min_nna) continue;
        
        //printf("Ending");print_ft(g);
        n_more = n_more_analysis(g, needf, &v, &va);
        nm[j]  =  n_more;
        //printf(" n_more = %d\n", n_more);
        
        if( n_more < k) {
            k = n_more;
        }
    }         
    for(j=0; j<ng; j++) {
        g = needf->F[gs[j]];
        if(iga_graph[g].inn > 0) continue;
        if(nm[j] > k) continue;
        if(nna[j] > min_nna) continue;
        
        //printf("Ending ind = %d", iga_graph[g].in_d);print_ft(g);
        
        if( iga_graph[g].in_d < kk) {
            kk = iga_graph[g].in_d;
            jj = j;
        }   
    }          

    
    
    
    // begining ima0 order
    /*
    for(j=0; j<ng; j++) {
        v.num_F =0;
        g = needf->F[gs[j]];
        if(iga_graph[g].inn > 0) continue;
        bol = FALSE;
        
        for(i=0; i<iga_graph[g].in_d; i++) {
            f=iga_graph[g].w[i];
            if(iga_graph[f].in_d == 0)
              if(iga_graph[f].ima_type == 0) 
                if(iga_graph[f].inn < 0)  
                {
                    add_fs(&v, iga_graph[f].ima[0]);
                }
        }
    
        printf("Ending %d ",g);print_ft_name(g);printf(" %d\n", v.num_F);
        
        if(v.num_F<k) {
            k = v.num_F;
            jj = j;
        }
    } */          
 
    
    if(bol) break;
 
    g = needf->F[gs[jj]];
    subgoal = g;
    
    //printf("Attack %d ",g);
    //print_ft_name(g);printf(" sum = %d\n", k);
                 
    n_more_analysis(g, needf, &v, &va);
    p_order_state(&v);
   
    i=0;
    jj =0; 
    while(1)
    {
        f = v.F[i];
        if((iga_graph[f].n_imf == 1) 
          && (iga_graph[f].n_fof == 1)
          && (iga_graph[f].ima_nailed)) {
            jj++;
            if(jj==3) break;
        } else {
            jj =0;
        }
        i++;
        if(i==v.num_F-1) break;
    }       
   
    i= i-2; 

    for(; i<v.num_F; i++) {
    
        f = v.F[i];
      //  printf("add"); print_ft(f);
        iga_graph[f].inn = 1;     
        
        /*
        passed = TRUE;
        for(jj=i; jj<=i+2;jj++) {
            if(jj<0) continue;
            if(jj>=v.num_F-1) break;
            if((iga_graph[f].n_imf == 1) 
             && (iga_graph[f].n_fof == 1)
             && (iga_graph[f].ima_nailed)) {
            
        } 
        
        if(passed && iga_graph[f].ima_nailed) {
         */
        
        if(iga_graph[f].ima_nailed) {
        
            a = iga_graph[f].ima[0];
 
            if(!in_array(GpG.esp_solution, GpG.num_esp_sol, a)) {
 
                GpG.esp_solution[GpG.num_esp_sol++] = a;
 
                //printf("commit"); print_op(a);
                for(j=0; j<gef_conn[a].num_A; j++) {
                    g = gef_conn[a].A[j];
                    if(g==f) continue;
                    if(v_contain_fact_state(needf, g) 
                     //       &&
                     //     ( ( iga_graph[g].n_ima <=0)||
                     //   in_array(iga_graph[g].ima, iga_graph[g].n_ima, a)) 
                    )
                    {
                        iga_graph[g].inn = 1;    
                  //      printf("co-add"); print_ft(g);
                    }
                }       
            } 
        }
        
        if(f==subgoal) break;
    }   
 }
 
 if(run_depend(start_state, needf)>0) return 1;
 else return -1; 
}

void nosame_add_action(State *va, int a)
{   
    int i;    
    for(i=0; i<va->num_F; i++) {
        if(va->F[i] == a) return;
        if(same_action(va->F[i], a)) return;
    }
    va->F[va->num_F++] = a;
}

void symmetry(int f, State *v)
{
    int g = f,i/*, k*/; /* useless Chih-Wei */
    State t;
    
    t.num_F = 0;
    v->num_F =0;
    for(i=0; i<gnum_ft_conn; i++) {
        if(GET_BIT (FT_FT_mutex[f], i)) 
           t.F[t.num_F++] = i; 
    }
  
   /* 
    max_nom = 0;
    for(i=0; i<t.num_F; i++) {
        nom = 0;
        for(j=0; j<t.num_F; j++) {
            if(GET_BIT (FT_FT_mutex[t.F[i]], t.F[j])) 
                nom++;
        }
        printf("nom %d",nom); print_ft(t.F[i]); 
        if(max_nom < nom) {
            max_nom = nom;
            k = i;
        }
    }
     */
 
    v->F[0] = f;
    v->F[1] = g;
    printf("g   "); print_ft(g); 
    v->num_F = 2;       
        
    for(i=0; i<t.num_F; i++) {
        print_ft(t.F[i]);
        if(GET_BIT (FT_FT_mutex[t.F[i]], g)) {  
            v->F[v->num_F++] = t.F[i];
        }
        else printf("pruned\n");
    }   
}

/*
void sym_mutex(vState* needf)
{
    int i,j,k,f,a,b,g,gg,kk;
    
    ngoals = 0;
    
    for(i=0; i<needf->num_F; i++) {
        f=needf->F[i];
        if(iga_graph[f].out_d == 0) {
            ggs[ngoals++] = f;
        }  
    }
   
    for(i=0; i<gnum_ft_conn; i++) iga_graph[i].subgoal = -10;

    for(i=0; i<ngoals; i++) {
        printf("\nSUBGAOL %d",i);print_ft(ggs[i]);
        symmetry(ggs[i], &(gsym[i]));
        for(j=0; j<gsym[i].num_F; j++) {
            iga_graph[gsym[i].F[j]].subgoal = i;
        }
    }
}
  */  
    
void sym_path_facts(vState* needf, State *symfs)
{
    int i,f;

    symfs->num_F = 0;
    
    for(i=0; i<needf->num_F; i++) {
        f = needf->F[i];
        if(!iga_graph[f].ima_nailed) 
        {   
            if(gft_conn[f].num_A > 6) {
              //   printf("not sym_path for"); print_ft(f);
            }
            else {
              //  printf("make sym_path for %d", gft_conn[f].num_A);print_ft(f);
                symfs->F[symfs->num_F++] = f;
            }
        }
    }   
}

void sym_path(State *symfs, State *omita, int* omi_select)
{           
   int i,x,k,a;
           
   omita->num_F = 0;
   
   for(k=0; k<symfs->num_F; k++) { 
        x = symfs->F[k];
        for(i=0; i<gft_conn[x].num_A; i++) {
            a = gft_conn[x].A[i];
            if(i != omi_select[k]) {
               // printf("omita %d sl %d",k, omi_select[k]);print_op(a);
                add_fs(omita, a); 
            }
            
        }
   }
}

void omita_analysis(State * start_state, int *rsa_facts, int *rsa_actions
        ,int *decr_g)
{
    vState needf;
    build_iga_graph(start_state, rsa_facts, rsa_actions, decr_g);
    precede_facts_of_state(&saved_ggoal_state, &needf);
    ima_analysis(start_state,  &needf, 1, rsa_facts, rsa_actions, decr_g);
}

void ima_analysis(State * start_state, vState *needf, int power_level,
      int*  rsa_facts, int* rsa_actions,int *decr_g)
{                                                
    int globk, i,j,k,f,a,g,gg,kk;
    State v, va;
    Bool gAdd, is_change, k_del, found_3;
    State S;
    State omita, symfs, ext_omita, seleta;
    int omi_select[MAXNG];
    
    GpG.num_esp_sol = 0;
    ext_omita.num_F = 0;
    seleta.num_F = 0;

    
    for(globk =0 ; globk<3; globk++) {
    
    // ***** power_level 0 *****
    for(i=0; i<needf->num_F; i++) {
        f =needf->F[i];
         
        // printf("iga %d ", f);print_ft_name(f); printf("\n");

        // get imf
        v.num_F = 0;
        for(j=0; j<iga_graph[f].in_d; j++) {
            g = iga_graph[f].w[j];
            gAdd = TRUE;
            for(k=0; k<iga_graph[f].in_d; k++) {
                if(k==j) continue;
                gg = iga_graph[f].w[k];
                if(!(iga_graph[gg].w)) continue;
                for(kk=0; kk<iga_graph[gg].in_d;kk++) {
                    if(g == iga_graph[gg].w[kk]) {
                        gAdd = FALSE;
                        break;
                    }        
                }
               if(!gAdd) break; 
            }
            if(gAdd) {
                v.F[v.num_F++] = g;
                //printf("    imf : %d ",g); print_ft_name(g); printf("\n");
            } 
        }
        //printf("  %d\n",v.num_F);
        
        iga_graph[f].n_imf = v.num_F;
        for(j=0; j<v.num_F; j++) {
            iga_graph[f].imf[j] = v.F[j];            
        } 
         
        
        // get ima
        if(v.num_F > 0){
          va.num_F = 0;
          for(j=0; j<gft_conn[f].num_A; j++) {
            a = gft_conn[f].A[j];
            gAdd = TRUE;
            for(k=0; k<v.num_F; k++) {
                for(kk = 0; kk<gef_conn[a].num_PC; kk++) {
                    if(gef_conn[a].PC[kk] == v.F[k]) break;
                }       
                if(kk == gef_conn[a].num_PC) break;
            }          
            if(k<v.num_F) gAdd = FALSE;
            if(gAdd) {
                nosame_add_action(&va, a);
            //    va.F[va.num_F++] = a;
             //   printf("    ima : %d ",a); print_op_name(a); printf("\n");
            }           
          }   
        } 
        else {
            va.num_F =0;
            for(j=0; j<gft_conn[f].num_A; j++) {
                a = gft_conn[f].A[j];
                if(actionable(start_state, a)) {
                //    va.F[va.num_F++] = a;
                    nosame_add_action(&va, a);
              //      printf("    s-ima : %d ",a); print_op_name(a); printf("\n");
                }
            }
        }

        
        iga_graph[f].n_ima = va.num_F;
        for(j=0; j<va.num_F; j++) iga_graph[f].ima[j] = va.F[j];            
        
        if(iga_graph[f].n_ima == 1) {
            iga_graph[f].ima_nailed = TRUE;
            if(iga_graph[f].n_imf == 0) iga_graph[f].ima_type = 0;
            else  iga_graph[f].ima_type = 10;
        }
        else iga_graph[f].ima_nailed = FALSE;
    }               

    
    if(power_level <1) return;

    // ***** power level 1 *****
    // * this level prunes more actions, is more powerful, 
    // * but sometimes unsafe
    // * but life is always like that, isn't it?
   // printf("\n\n");
    
    // get fof
    for(i=0;i<needf->num_F;i++) {
        f = needf->F[i];
        v.num_F = 0;
        for(j=0; j<needf->num_F; j++) {
            if(i==j) continue;
            g = needf->F[j];
            for(k=0; k<iga_graph[g].n_imf; k++) {
              if(iga_graph[g].imf)
                if(iga_graph[g].imf[k] == f) {
                    v.F[v.num_F++] = g;
                    break; 
                }               
            }
        }
        iga_graph[f].n_fof = v.num_F;
        for(j=0; j<v.num_F; j++) iga_graph[f].fof[j] = v.F[j];            
    }       
    
    
    // nail more ima 
    is_change = TRUE;
    while(is_change) {
        is_change = FALSE; 
        for(i=0; i<needf->num_F;i++) {
            f= needf->F[i];
            if(iga_graph[f].ima_nailed) continue;
            if(iga_graph[f].n_ima==0) continue;
            
           // printf("try to nail more ima"); print_ft(f);
           
            
            for(k=0; k<iga_graph[f].n_ima; k++) {
                a = iga_graph[f].ima[k];
                
                k_del = FALSE;
                
                // check fof's ima
                for(j=0; j<iga_graph[f].n_fof; j++) {
                    g = iga_graph[f].fof[j];
                    if(!iga_graph[g].ima_nailed) continue;
                    if(a_ima_conflict(a, iga_graph[g].ima[0])) { 
                       del_v_i(iga_graph[f].ima, k, &(iga_graph[f].n_ima)); 
                       k--;
                       is_change = TRUE;
                       k_del = TRUE;    
                      // printf("_fof_del ima %d ", a); print_op_name(a);
                      // printf("for fact %d ", f); print_ft_name(f); printf("\n");
                       break;
                    }
                }
                
                if(k_del) continue;
                
                // check imf's ima
                for(j=0; j<iga_graph[f].n_imf; j++) {
                    g = iga_graph[f].imf[j];
                    if(!iga_graph[g].ima_nailed) continue;
                    if(a_ima_conflict(iga_graph[g].ima[0], a)) { 
                       del_v_i(iga_graph[f].ima, k, &(iga_graph[f].n_ima)); 
                       k--;
                       is_change = TRUE;
                      // printf("_imf_del ima %d ", a); print_op_name(a);
                      // printf("for fact %d ", f); print_ft_name(f); printf("\n");
                       break;
                    }
                }
            }
       
            /* 
            for(k=0; k<iga_graph[f].n_ima; k++) {
                a = iga_graph[f].ima[k];
                // check fof's ima
                for(j=0; j<iga_graph[f].n_fof; j++) {
                    g = iga_graph[f].fof[j];
                    if(!iga_graph[g].ima_nailed) continue;
                    if(a_ima_conflict(a, iga_graph[g].ima[0])) { 
                       del_v_i(iga_graph[f].ima, k, &(iga_graph[f].n_ima)); 
                       k--;
                       is_change = TRUE;
                       printf("_fof_del ima %d ", a); print_op_name(a);
                       printf("for fact %d ", f); print_ft_name(f); printf("\n");
                       break;
                    }
                }
                if(iga_graph[f].n_ima==1) break;
            }
            if(iga_graph[f].n_ima>1) { 
              for(k=0; k<iga_graph[f].n_ima; k++) {
                a = iga_graph[f].ima[k];
                
                // check imf's ima
                for(j=0; j<iga_graph[f].n_imf; j++) {
                    g = iga_graph[f].imf[j];
                    if(!iga_graph[g].ima_nailed) continue;
                    if(a_ima_conflict(iga_graph[g].ima[0], a)) { 
                       del_v_i(iga_graph[f].ima, k, &(iga_graph[f].n_ima)); 
                       k--;
                       is_change = TRUE;
                       printf("_imf_del ima %d ", a); print_op_name(a);
                       printf("for fact %d ", f); print_ft_name(f); printf("\n");
                       break;
                    }
                }
                
                if(iga_graph[f].n_ima==1) break;
              }
            }*/
    
            if(iga_graph[f].n_ima==1) {
                iga_graph[f].ima_nailed = TRUE;
               // printf("nail more ima"); print_ft(f);
                if(iga_graph[f].n_imf == 0) iga_graph[f].ima_type = 0;
                else  iga_graph[f].ima_type = 10;
            }           
        }
    }
    
    for(i=0; i<needf->num_F; i++) {
        f = needf->F[i];
        if((iga_graph[f].in_d == 0)
           &&(iga_graph[f].n_ima > 1)) 
            if(iga_graph[f].n_fof == 1) {
            //printf("%d ",needf->F[i]);
            //print_ft_name(needf->F[i]); printf(" moving\n");
                g = iga_graph[f].fof[0];
                if(iga_graph[g].ima_nailed) {   
                    //print_op_name(iga_graph[g].ima[0]); printf("\n");
                    for(j=0; j<gft_conn[f].num_A; j++) {
                        a  = gft_conn[f].A[j];
                        //print_op_name(a); printf("\n");
                        //if(!strong_a_ima_conflict(a,iga_graph[g].ima[0])) { 
                        //if(a_ima_same_prec(a,iga_graph[g].ima[0])) {
                        if(can_move(g, a, f, iga_graph[g].ima[0])){  
                            //print_op_name(a); printf("\n");
                            iga_graph[f].n_ima=1;
                            iga_graph[f].ima[0] = a;
                            iga_graph[f].ima_nailed = TRUE;
                            iga_graph[f].ima_type = 5;
                            break;
                        }
                    }
                }
            }
    }
      
    for(i=0; i<needf->num_F; i++) { 
        if(!iga_graph[needf->F[i]].ima_nailed) {
           // printf("%d ",needf->F[i]);
           // print_ft_name(needf->F[i]); printf(" not nailed\n");
           
            f=needf->F[i];
            if(iga_graph[f].n_ima >1) {
             //   printf(" multiple\n");
              
                for(k=0; k<iga_graph[f].n_ima; k++) {
                    a = iga_graph[f].ima[k];
                    k_del = FALSE;
                
                    // check fof's ima
                    for(j=0; j<iga_graph[f].n_fof; j++) {
                        g = iga_graph[f].fof[j];
                        if(!iga_graph[g].ima_nailed) continue;
                        if(strong_a_ima_conflict(a, iga_graph[g].ima[0])) { 
                            del_v_i(iga_graph[f].ima, k, &(iga_graph[f].n_ima)); 
                            k--;
                            k_del = TRUE;    
              //              printf("strong _fof_del ima %d ", a); print_op_name(a);
              //              printf("for fact %d ", f); print_ft_name(f); printf("\n");
                            break;
                        }
                    }
                
                     if(k_del) continue;
                
                     // check imf's ima
                    for(j=0; j<iga_graph[f].n_imf; j++) {
                        g = iga_graph[f].imf[j];
                        if(!iga_graph[g].ima_nailed) continue;
                        if(strong_a_ima_conflict(iga_graph[g].ima[0], a)) { 
                            del_v_i(iga_graph[f].ima, k, &(iga_graph[f].n_ima)); 
                            k--;
                //            printf("strong _imf_del ima %d ", a); print_op_name(a);
                //            printf("for fact %d ", f); print_ft_name(f); printf("\n");
                            break;
                        }
                    }
                }
                
                if(iga_graph[f].n_ima==1) {
                  //   printf(" repaired\n");
                    iga_graph[f].ima_nailed = TRUE;
                    if(iga_graph[f].n_imf == 0) iga_graph[f].ima_type = 0;
                    else  iga_graph[f].ima_type = 10;
                } 
             
               // New , experimental
                /*            
                else {
                    iga_graph[f].ima_nailed = TRUE;
                    if(iga_graph[f].n_imf == 0) iga_graph[f].ima_type = 0;
                    else  iga_graph[f].ima_type = 10;
                } */   
            }
        }
    }
   
    printf("\n");
    // attack 0-ima
    for(i=0; i<needf->num_F; i++) {
        f = needf->F[i];
        va.num_F = 0;
        if(iga_graph[f].n_ima >0 ) continue;
            
        for(k=0;k<gft_conn[f].num_A; k++) {
                a = gft_conn[f].A[k];
                for(j=0; j<needf->num_F; j++) {
                    if(i==j) continue;
                    g = needf->F[j];
                    if(iga_graph[g].n_ima <=0 ) continue;
                    if(in_array(iga_graph[g].ima, iga_graph[g].n_ima, a))
                        nosame_add_action(&va, a); 
                }
        }
        
        if(va.num_F > 0) {
            iga_graph[f].n_ima = va.num_F;
            for(k=0; k<va.num_F; k++) iga_graph[f].ima[k] = va.F[k];
       //     printf("lieu %d ops for", va.num_F);
         //   print_ft_name(f); print_op(iga_graph[f].ima[0]);
            if(va.num_F == 1) iga_graph[f].ima_nailed = TRUE; 
        }
    }   
  
    // type 3 handling
    for(i=0; i<needf->num_F; i++) {
      f = needf->F[i];
      if(iga_graph[f].ima_type == 10) { 
        if(iga_graph[f].ima_nailed) {
            a = iga_graph[f].ima[0];    
            found_3 = FALSE;
            for(j=0; j<needf->num_F; j++) {
                if(i==j) continue;
                g = needf->F[j];
                if(iga_graph[g].ima_nailed) 
                    if(iga_graph[g].ima[0]==a) {               
                        iga_graph[g].ima_type = 3;
                        found_3 = TRUE;
                    }
            }
            if(found_3) iga_graph[f].ima_type =3;
        }
      }
    }
                
        
    //print_iga_graph(needf);     
    
    if(globk == 2) break;
    
    // sym-mutex analysis
    /*
    sym_mutex(needf); 
    for(i=0; i<needf->num_F; i++) {
        f = needf->F[i];
      //  if(!iga_graph[f].ima_nailed) {
            sym_path(f);
      //  }
    } */
    
    // hot
    
    sym_path_facts(needf, &symfs);
    for(i=0; i<symfs.num_F; i++) {
        omi_select[i] = 0;
    }  
    if(symfs.num_F == 0) break;
    
    while(1) {
        
        if(check_omita_ext(&seleta, &symfs, omi_select)) {
            sym_path(&symfs, &omita, omi_select); 
            append_state(&omita, &ext_omita);
         //   printf("size of omita = %d\n", omita.num_F);
       
            clear_iga_graph();
            build_subspace(start_state, rsa_facts, rsa_actions, &omita);
            if(goal_reached(&saved_ggoal_state, rsa_facts)) {
           //     printf("reached all goals\n");
                break;
            }
        }
        i = symfs.num_F - 1;
        while(1) {
            omi_select[i]++;
            f =  symfs.F[i];
            if(omi_select[i] < gft_conn[f].num_A) break;
            else {
                omi_select[i] = 0;
                i--;
                if(i<0) {
                    printf("single omita failed"); exit(1);
                }
            }
        } 
    }
    source_to_dest(&ext_omita, &omita); 
    seleta.num_F = 0;
    for(i=0; i<symfs.num_F; i++) {
        seleta.F[seleta.num_F++] = gft_conn[symfs.F[i]].A[omi_select[i]];    
    }        
   
    
    /* build _out edges */ 
    for(i=0;i<gnum_ft_conn;i++) {
       if(contain_fact_state(start_state, i)) continue;
       block_fact_subspace(start_state, i, rsa_facts, 
                           rsa_actions, NULL, decr_g, IGA_GRAPH);
    }   
    build_in_edges(decr_g);
    precede_facts_of_state(&saved_ggoal_state, needf);
           
            
   } //end of globk
   
     
   // print_iga_graph(needf);     
   // exit(0); 
    
   /* 
    for(i=0; i<needf->num_F; i++) { 
        if(!iga_graph[needf->F[i]].ima_nailed) continue;
        for(j=0; j<needf->num_F; j++) { 
            if(j==i) continue;
            if(pred(needf->F[i], needf->F[j])) continue;
            if(pred(needf->F[j], needf->F[i])) continue;
            if(iga_graph[needf->F[j]].ima_nailed) {
                if(a_ima_conflict(iga_graph[needf->F[i]].ima[0],
                                    iga_graph[needf->F[j]].ima[0])) 
                printf("X %d (%d) -> %d (%d)\n",needf->F[i],
                        iga_graph[needf->F[i]].ima[0], needf->F[j],
                        iga_graph[needf->F[j]].ima[0]);
            }
        }
    }*/
    
    
       
 //   while(1){
    // generate plan
    source_to_dest(&S, start_state);
    //fid_order_needf(needf);
    for(i=0; i<needf->num_F; i++) {
        iga_graph[needf->F[i]].inn = iga_graph[needf->F[i]].n_imf; 
    }
    while(1) {
        v.num_F =0;
        va.num_F = 0;
        for(i=0; i<needf->num_F; i++) {
            if(iga_graph[needf->F[i]].inn == 0) {
                if(iga_graph[needf->F[i]].ima_nailed) 
                    if(actionable(&S, iga_graph[needf->F[i]].ima[0])) {
              //      printf("candiate %d f %d a %d %d\n", v.num_F, needf->F[i], 
                //            iga_graph[needf->F[i]].ima[0], iga_graph[needf->F[i]].ima_type);
                      if(!contain_fact_state(&v, iga_graph[needf->F[i]].ima[0])) { 
                            v.F[v.num_F++] = iga_graph[needf->F[i]].ima[0];
                            va.F[va.num_F++] = needf->F[i];
                      }
                 }            
            }
        }     
       
        
      if(v.num_F==0) {
    //    printf("none\n");
        break;
    }
        
      //  a = select_action(&S, &v);
        a = type_select_action(&S, &v, &va);
    //    printf("choose action %d  from %d",a, v.num_F);print_op_name(a); printf("\n");
        GpG.esp_solution[GpG.num_esp_sol++] = a;
        
        /*
        b = (int) ((double)(v.num_F)*rand()/(RAND_MAX+1.0)); a = v.F[b];         
        printf("choose action %d %d from %d",a,b, v.num_F);print_op_name(a); printf("\n");
        */
        
        activate(&S, a);
        //print_state(S);
        
        for(i=0;i<gef_conn[a].num_A;i++) {
            f= gef_conn[a].A[i];
            if(!v_contain_fact_state(needf, f)) continue;
            
            // new
            if((iga_graph[f].n_ima>0) &&  
               !in_ima_list(f, a)) {
                //printf("a %d for fact %d ",a,f);print_ft_name(f);
                //printf("remains\n");
                continue;
            }
            if(iga_graph[f].inn >=0 ) {  
              //  printf("    achv ft %d ",f);print_ft_name(f);printf("\n");
                iga_graph[f].inn = -1;
                for(j=0; j<iga_graph[f].n_fof; j++) {
                    g = iga_graph[f].fof[j];
                    iga_graph[g].inn--;
                }
            }
        }        
    }
    
    for(i=0; i<needf->num_F; i++) 
        if(iga_graph[needf->F[i]].inn >= 0) break;
    if(i==needf->num_F) {
       // printf("solution found\n");
        return;
    }
  
    //dependency_check(start_state, needf);
    //return;
   
   while(1) { 
    
    // s1 failed, use 2
    GpG.num_esp_sol = 0;
    for(i=0; i<needf->num_F; i++) {
        iga_graph[needf->F[i]].inn = -10;
    }
  
    if(dependency_check(start_state, needf)>0) return;
    
    times (&glob_end_time);
    gtotal_time =
        (float) ((glob_end_time.tms_utime -
                  glob_start_time.tms_utime +
                  glob_end_time.tms_stime -
                  glob_start_time.tms_stime) / 100.0);
    if(gtotal_time > 1800) exit(0);
    
    sym_path_facts(needf, &symfs);
    for(i=0; i<symfs.num_F; i++) {
        omi_select[i] = 0;
    }  
    if(symfs.num_F == 0) break;
    
    while(1) {
        
        if(check_omita_ext(&seleta, &symfs, omi_select)) {
            sym_path(&symfs, &omita, omi_select); 
            append_state(&omita, &ext_omita);
           // printf("size of omita = %d\n", omita.num_F);
       
            clear_iga_graph();
            build_subspace(start_state, rsa_facts, rsa_actions, &omita);
            if(goal_reached(&saved_ggoal_state, rsa_facts)) {
             //   printf("reached all goals\n");
                break;
            }
        }
        i = symfs.num_F - 1;
        while(1) {
            omi_select[i]++;
            f =  symfs.F[i];
            if(omi_select[i] < gft_conn[f].num_A) break;
            else {
                omi_select[i] = 0;
                i--;
                if(i<0) {
                    printf("single omita failed"); exit(1);
                }
            }
        } 
    }
    source_to_dest(&ext_omita, &omita); 
    seleta.num_F = 0;
    for(i=0; i<symfs.num_F; i++) {
        seleta.F[seleta.num_F++] = gft_conn[symfs.F[i]].A[omi_select[i]];    
    }        
   
    
    /* build _out edges */ 
    for(i=0;i<gnum_ft_conn;i++) {
       if(contain_fact_state(start_state, i)) continue;
       block_fact_subspace(start_state, i, rsa_facts, 
                           rsa_actions, NULL, decr_g, IGA_GRAPH);
    }   
    build_in_edges(decr_g);
    precede_facts_of_state(&saved_ggoal_state, needf);
    for(i=0; i<needf->num_F; i++) {
        f =needf->F[i];

        // get imf
        v.num_F = 0;
        for(j=0; j<iga_graph[f].in_d; j++) {
            g = iga_graph[f].w[j];
            gAdd = TRUE;
            for(k=0; k<iga_graph[f].in_d; k++) {
                if(k==j) continue;
                gg = iga_graph[f].w[k];
                if(!(iga_graph[gg].w)) continue;
                for(kk=0; kk<iga_graph[gg].in_d;kk++) {
                    if(g == iga_graph[gg].w[kk]) {
                        gAdd = FALSE;
                        break;
                    }        
                }
               if(!gAdd) break; 
            }
            if(gAdd) {
                v.F[v.num_F++] = g;
            } 
        }
        
        iga_graph[f].n_imf = v.num_F;
        for(j=0; j<v.num_F; j++) {
            iga_graph[f].imf[j] = v.F[j];            
        } 
         
        
        // get ima
        if(v.num_F > 0){
          va.num_F = 0;
          for(j=0; j<gft_conn[f].num_A; j++) {
            a = gft_conn[f].A[j];
            gAdd = TRUE;
            for(k=0; k<v.num_F; k++) {
                for(kk = 0; kk<gef_conn[a].num_PC; kk++) {
                    if(gef_conn[a].PC[kk] == v.F[k]) break;
                }       
                if(kk == gef_conn[a].num_PC) break;
            }          
            if(k<v.num_F) gAdd = FALSE;
            if(gAdd) {
                nosame_add_action(&va, a);
            //    va.F[va.num_F++] = a;
            }           
          }   
        } 
        else {
            va.num_F =0;
            for(j=0; j<gft_conn[f].num_A; j++) {
                a = gft_conn[f].A[j];
                if(actionable(start_state, a)) {
                //    va.F[va.num_F++] = a;
                    nosame_add_action(&va, a);
                }
            }
        }

        
        iga_graph[f].n_ima = va.num_F;
        for(j=0; j<va.num_F; j++) iga_graph[f].ima[j] = va.F[j];            
        
        if(iga_graph[f].n_ima == 1) {
            iga_graph[f].ima_nailed = TRUE;
            if(iga_graph[f].n_imf == 0) iga_graph[f].ima_type = 0;
            else  iga_graph[f].ima_type = 10;
        }
        else iga_graph[f].ima_nailed = FALSE;
    }               

    
    
    // get fof
    for(i=0;i<needf->num_F;i++) {
        f = needf->F[i];
        v.num_F = 0;
        for(j=0; j<needf->num_F; j++) {
            if(i==j) continue;
            g = needf->F[j];
            for(k=0; k<iga_graph[g].n_imf; k++) {
              if(iga_graph[g].imf)
                if(iga_graph[g].imf[k] == f) {
                    v.F[v.num_F++] = g;
                    break; 
                }               
            }
        }
        iga_graph[f].n_fof = v.num_F;
        for(j=0; j<v.num_F; j++) iga_graph[f].fof[j] = v.F[j];            
    }       
    
    
    // nail more ima 
    is_change = TRUE;
    while(is_change) {
        is_change = FALSE; 
        for(i=0; i<needf->num_F;i++) {
            f= needf->F[i];
            if(iga_graph[f].ima_nailed) continue;
            if(iga_graph[f].n_ima==0) continue;
            
            // printf("try to nail more ima"); print_ft(f);
           
            
            for(k=0; k<iga_graph[f].n_ima; k++) {
                a = iga_graph[f].ima[k];
                
                k_del = FALSE;
                
                // check fof's ima
                for(j=0; j<iga_graph[f].n_fof; j++) {
                    g = iga_graph[f].fof[j];
                    if(!iga_graph[g].ima_nailed) continue;
                    if(a_ima_conflict(a, iga_graph[g].ima[0])) { 
                       del_v_i(iga_graph[f].ima, k, &(iga_graph[f].n_ima)); 
                       k--;
                       is_change = TRUE;
                       k_del = TRUE;    
               //        printf("_fof_del ima %d ", a); print_op_name(a);
                 //      printf("for fact %d ", f); print_ft_name(f); printf("\n");
                       break;
                    }
                }
                
                if(k_del) continue;
                
                // check imf's ima
                for(j=0; j<iga_graph[f].n_imf; j++) {
                    g = iga_graph[f].imf[j];
                    if(!iga_graph[g].ima_nailed) continue;
                    if(a_ima_conflict(iga_graph[g].ima[0], a)) { 
                       del_v_i(iga_graph[f].ima, k, &(iga_graph[f].n_ima)); 
                       k--;
                       is_change = TRUE;
                   //    printf("_imf_del ima %d ", a); print_op_name(a);
                   //    printf("for fact %d ", f); print_ft_name(f); printf("\n");
                       break;
                    }
                }
            }
       
            if(iga_graph[f].n_ima==1) {
                iga_graph[f].ima_nailed = TRUE;
               // printf("nail more ima"); print_ft(f);
                if(iga_graph[f].n_imf == 0) iga_graph[f].ima_type = 0;
                else  iga_graph[f].ima_type = 10;
            }           
        }
    }
    
    for(i=0; i<needf->num_F; i++) {
        f = needf->F[i];
        if((iga_graph[f].in_d == 0)
           &&(iga_graph[f].n_ima > 1)) 
            if(iga_graph[f].n_fof == 1) {
            //printf("%d ",needf->F[i]);
            //print_ft_name(needf->F[i]); printf(" moving\n");
                g = iga_graph[f].fof[0];
                if(iga_graph[g].ima_nailed) {   
                    //print_op_name(iga_graph[g].ima[0]); printf("\n");
                    for(j=0; j<gft_conn[f].num_A; j++) {
                        a  = gft_conn[f].A[j];
                        //print_op_name(a); printf("\n");
                        //if(!strong_a_ima_conflict(a,iga_graph[g].ima[0])) { 
                        //if(a_ima_same_prec(a,iga_graph[g].ima[0])) {
                        if(can_move(g, a, f, iga_graph[g].ima[0])){  
                            //print_op_name(a); printf("\n");
                            iga_graph[f].n_ima=1;
                            iga_graph[f].ima[0] = a;
                            iga_graph[f].ima_nailed = TRUE;
                            iga_graph[f].ima_type = 5;
                            break;
                        }
                    }
                }
            }
    }
      
    for(i=0; i<needf->num_F; i++) { 
        if(!iga_graph[needf->F[i]].ima_nailed) {
          //  printf("%d ",needf->F[i]);
          //  print_ft_name(needf->F[i]); printf(" not nailed\n");
           
            f=needf->F[i];
            if(iga_graph[f].n_ima >1) {
            //    printf(" multiple\n");
              
                for(k=0; k<iga_graph[f].n_ima; k++) {
                    a = iga_graph[f].ima[k];
                    k_del = FALSE;
                
                    // check fof's ima
                    for(j=0; j<iga_graph[f].n_fof; j++) {
                        g = iga_graph[f].fof[j];
                        if(!iga_graph[g].ima_nailed) continue;
                        if(strong_a_ima_conflict(a, iga_graph[g].ima[0])) { 
                            del_v_i(iga_graph[f].ima, k, &(iga_graph[f].n_ima)); 
                            k--;
                            k_del = TRUE;    
              //              printf("strong _fof_del ima %d ", a); print_op_name(a);
              //              printf("for fact %d ", f); print_ft_name(f); printf("\n");
                            break;
                        }
                    }
                
                     if(k_del) continue;
                
                     // check imf's ima
                    for(j=0; j<iga_graph[f].n_imf; j++) {
                        g = iga_graph[f].imf[j];
                        if(!iga_graph[g].ima_nailed) continue;
                        if(strong_a_ima_conflict(iga_graph[g].ima[0], a)) { 
                            del_v_i(iga_graph[f].ima, k, &(iga_graph[f].n_ima)); 
                            k--;
                //            printf("strong _imf_del ima %d ", a); print_op_name(a);
                //            printf("for fact %d ", f); print_ft_name(f); printf("\n");
                            break;
                        }
                    }
                }
                
                if(iga_graph[f].n_ima==1) {
                  //   printf(" repaired\n");
                    iga_graph[f].ima_nailed = TRUE;
                    if(iga_graph[f].n_imf == 0) iga_graph[f].ima_type = 0;
                    else  iga_graph[f].ima_type = 10;
                } 
             
            }
        }
    }
   
  //  printf("\n");
    // attack 0-ima
    for(i=0; i<needf->num_F; i++) {
        f = needf->F[i];
        va.num_F = 0;
        if(iga_graph[f].n_ima >0 ) continue;
            
        for(k=0;k<gft_conn[f].num_A; k++) {
                a = gft_conn[f].A[k];
                for(j=0; j<needf->num_F; j++) {
                    if(i==j) continue;
                    g = needf->F[j];
                    if(iga_graph[g].n_ima <=0 ) continue;
                    if(in_array(iga_graph[g].ima, iga_graph[g].n_ima, a))
                        nosame_add_action(&va, a); 
                }
        }
        
        if(va.num_F > 0) {
            iga_graph[f].n_ima = va.num_F;
            for(k=0; k<va.num_F; k++) iga_graph[f].ima[k] = va.F[k];
    //        printf("lieu %d ops for", va.num_F);
      //      print_ft_name(f); print_op(iga_graph[f].ima[0]);
            if(va.num_F == 1) iga_graph[f].ima_nailed = TRUE; 
        }
    }   
  
    // type 3 handling
    for(i=0; i<needf->num_F; i++) {
      f = needf->F[i];
      if(iga_graph[f].ima_type == 10) { 
        if(iga_graph[f].ima_nailed) {
            a = iga_graph[f].ima[0];    
            found_3 = FALSE;
            for(j=0; j<needf->num_F; j++) {
                if(i==j) continue;
                g = needf->F[j];
                if(iga_graph[g].ima_nailed) 
                    if(iga_graph[g].ima[0]==a) {               
                        iga_graph[g].ima_type = 3;
                        found_3 = TRUE;
                    }
            }
            if(found_3) iga_graph[f].ima_type =3;
        }
      }
    }
                
    
            }           
    
}               


int rip_facts_subspace(State * start_state,  State *gs, int* rsa_facts, int* rsa_actions)
{   
/*    
    int i,j,k;
    int *decr_g;
    
    build_subspace(start_state, rsa_facts, rsa_actions);

    decr_g = ( int * ) calloc( gnum_ft_conn, sizeof( int ) ); 
    
    for(i=0;i<gnum_ft_conn;i++) {
        if(contain_fact_state(start_state, i)) continue;
    
        k = block_fact_subspace(start_state, i, rsa_facts, rsa_actions, gs, decr_g);
        if(k==1) {
            printf("IMG fact: %d ", i);
            print_ft_name(i);
            printf("\n");
        }
    }                                                                                                    
    free(decr_g);    
    return 0;
  */ /* 
    for(i=0;i<gnum_ft_conn;i++) {
        restore_fact_supp();    
        if(rip_a_fact(i, Gf, rsa_facts, rsa_actions)) {
            printf("%d ", i);
            print_ft_name(i);
            printf("\n");
        }    
    }
 */   
	return 0; 	
}


void alloc_fact_supp()
{
    int i;
    for(i=0; i<MAX_LEVELS; i++){
        //fact_supp[i] =  ( int * ) calloc( gnum_ft_conn, sizeof( int ) ); 
        _fact_supp[i] =  ( int * ) calloc( gnum_ft_conn, sizeof( int ) );
    }
}                                           

void zero_fact_supp()
{
    int /*i,*/j;
   
    for (j = 0; j < gnum_ft_conn; j++) _fact_supp[0][j] = 0;
    /*
    for(i=0; i<MAX_LEVELS; i++) { 
        for (j = 0; j < gnum_ft_conn; j++) _fact_supp[i][j] = 0;
    } */                                              
}

void v_source_to_dest(vState *s, vState *t)
{
    int i;
    s->num_F = t->num_F;
    for(i=0; i<s->num_F; i++)
        s->F[i] = t->F[i];
}


void vs_source_to_dest(vState *s, State *t)
{
    int i;
    s->num_F = t->num_F;
    for(i=0; i<s->num_F; i++)
        s->F[i] = t->F[i];
}

Bool v_contain_fact_state(vState *state, int f)
{   
        int i;
        for(i=0; i<state->num_F; i++) {
            if(state->F[i] == f) return TRUE;
        }
        return FALSE;
}

void clear_iga_graph()
{
    int i;
    static int time = 0;
    if(time == 0) {
        time = 1;
        //printf("allocate iga_graph\n");
        iga_graph = (gentry*) calloc( gnum_ft_conn, sizeof(gentry));
    }
      
    for(i=0;i<gnum_ft_conn; i++) {
            iga_graph[i].out_d = 0;
            iga_graph[i].in_d = 0;
            iga_graph[i].n_imf = 0;
            iga_graph[i].n_fof = 0;
            iga_graph[i].n_ima = 0;
            iga_graph[i].ima_nailed = FALSE;
            iga_graph[i].inn = -10;
            iga_graph[i].ima_type = -10;
            iga_graph[i].subgoal = -10;
    }
}

void print_iga_graph(vState *needf)
{
    int i,j;
        
    printf("\n");
    
    for(i=0;i<gnum_ft_conn; i++) {
        
        if(needf) 
           if(!v_contain_fact_state(needf, i))
              continue; 
                
        printf("%d ", i);print_ft_name(i);
        printf("\n");
     
      /* 
        printf("     precedes :\n");
        if(iga_graph[i].v) {
            for(j=0;j<iga_graph[i].out_d; j++) {
                printf("        %d ", iga_graph[i].v[j]);
                print_ft_name(iga_graph[i].v[j]);
                printf("\n");  
            }
        }
        printf("     after :\n");
        if(iga_graph[i].w) {        
            for(j=0;j<iga_graph[i].in_d; j++) {
                printf("        %d ", iga_graph[i].w[j]);
                print_ft_name(iga_graph[i].w[j]);
                printf("\n");  
            }
        }
        */
        /*
        printf("     mtxop after :\n");
        if(iga_graph[i].mxw) {        
            for(j=0;j<iga_graph[i].in_mxd; j++) {
                printf("        %d ", iga_graph[i].mxw[j]);
                print_ft_name(iga_graph[i].mxw[j]);
                printf("\n");  
            }
        }
        */
        
        
        printf("     imf :\n");
        if(iga_graph[i].imf) {        
            for(j=0;j<iga_graph[i].n_imf; j++) {
                printf("        %d ", iga_graph[i].imf[j]);
                print_ft_name(iga_graph[i].imf[j]);
                printf("\n");  
            }
        }
        printf("     fof :\n");
        if(iga_graph[i].fof) {        
            for(j=0;j<iga_graph[i].n_fof; j++) {
                printf("        %d ", iga_graph[i].fof[j]);
                print_ft_name(iga_graph[i].fof[j]);
                printf("\n");  
            }
        }
        
        printf("     ima :\n");
        if(iga_graph[i].ima) {        
            for(j=0;j<iga_graph[i].n_ima; j++) {
                printf("        %d ", iga_graph[i].ima[j]);
                print_op_name(iga_graph[i].ima[j]);
                if(iga_graph[i].ima_nailed) printf(" NAILED %d\n", iga_graph[i].ima_type);
                printf("\n");  
            }
        }
    }
    

}


int max_out_degree()
{
    int m,i;
    m=0;
    for(i=0;i<gnum_ft_conn; i++) {
        if(iga_graph[i].out_d >m) m = iga_graph[i].out_d;
    }
    
    return m;
}

int max_out_iga_graph(State * start_state,  int* rsa_facts, int* rsa_actions, 
        int *decr_g)
{ 
    int i; 
   State nul;
   nul.num_F = 0;
    
    clear_iga_graph();
    build_subspace(start_state, rsa_facts, rsa_actions, &nul);
   printf("2\n");
   for(i=0;i<gnum_ft_conn;i++) {
       if(contain_fact_state(start_state, i)) continue;
       block_fact_subspace(start_state, i, rsa_facts, 
                           rsa_actions, NULL, decr_g, IGA_GRAPH);
   }                                         

   return max_out_degree(); 
}

void build_in_edges(int *fv)
{
    int i,j, k;
        
        
    for(i=0;i<gnum_ft_conn; i++) {
        if(iga_graph[i].v) {
            for(j=0;j<iga_graph[i].out_d; j++) {
                k = iga_graph[i].v[j];
                iga_graph[k].in_d ++;
            }
        }
    }
    for(i=0;i<gnum_ft_conn; i++) {
        fv[i] = 0;
        if(iga_graph[i].in_d > INOUTD) {
            printf("INOUTD too small");exit(1);
        }
        
        //if(iga_graph[i].in_d > 0) {
        //    iga_graph[i].w = (int*)calloc(iga_graph[i].in_d+1, sizeof(int));
        //}
    }
    for(i=0;i<gnum_ft_conn; i++) {
        if(iga_graph[i].v) {
            for(j=0;j<iga_graph[i].out_d; j++) {
                k = iga_graph[i].v[j];
                iga_graph[k].w[fv[k]++] = i;
            }
        }
    }
}

void modify_ft_ef_mutex()
{
    int i,j,a;
    for(i=0; i<gnum_ft_conn; i++) {
        for(j=0; j<gft_conn[i].num_A; j++) {
            a = gft_conn[i].A[j];
            RESET_BIT(FT_EF_mutex[i], a);
            RESET_BIT(EF_FT_mutex[a], i);
            
            /*
            if(GET_BIT (FT_EF_mutex[i], a)) {
                    printf("reset f %d a %d",i,a); 
                    print_ft_name(i);print_op_name(a); 
                    printf("\n");                   
                    RESET_BIT(FT_EF_mutex[i], a);
            }
            */
        }
    }
    
    //printf("\nft ef mutex modified\n");
}               

void test_setup_E()
{
    int i,j;

    for(i=0;i<gnum_ft_conn; i++) {
        printf("%d ", i);print_ft_name(i);printf("\n mutex :\n");
        for(j=0;j<gnum_ef_conn; j++) {
            if(GET_BIT (FT_EF_mutex[i], j)) {
                printf("    %d ", j);print_op_name(j);printf("\n");
            }
        }
        printf("\n setup_E :\n");
        setup_E( i );
        for(j=0;j<gnum_ef_conn; j++) {
        if(!lin[j]) {
            printf("    %d ", j);print_op_name(j);printf("\n");
        }
        }
        unsetup_E( i );
    }
}

void build_iga_graph(State * start_state,  int* rsa_facts, int* rsa_actions, 
        int *decr_g)
{ 
    int i; 
   State nul;
   nul.num_F = 0;
   
   /* refresh iga_graph */
   clear_iga_graph();
   
   /* build space */
   build_subspace(start_state, rsa_facts, rsa_actions, &nul);
   
       
   /* build _out edges */ 
   for(i=0;i<gnum_ft_conn;i++) {
       if(contain_fact_state(start_state, i)) continue;
       block_fact_subspace(start_state, i, rsa_facts, 
                           rsa_actions, NULL, decr_g, IGA_GRAPH);
   }   

   /* build _in edges */
   build_in_edges(decr_g);
   
   //print_iga_graph(NULL);   
    
   //test_setup_E();
}

void precede_facts_of_state(State *s, vState *pf)
{
    int j,i,f;
    pf->num_F = 0;
    
    for(i=0; i<s->num_F; i++) {
        f = s->F[i];    
        pf->F[pf->num_F++] = f;    
    }
         

    for(i=0; i<s->num_F; i++) {
        f = s->F[i];    
        for(j=0;j<iga_graph[f].in_d; j++) {
            if(!v_contain_fact_state(pf,  iga_graph[f].w[j]))
                pf->F[pf->num_F++] = iga_graph[f].w[j];    
        }   
    }
}

void zin_facts(State* start_state, vState *vf, vState *in_f)
{       
    int i,f;
    in_f->num_F = 0;
    for (i=0; i<vf->num_F; i++) {
        f = vf->F[i] ;
        if(iga_graph[f].in_d == 0)
           if(!contain_fact_state(start_state, f)) 
            in_f->F[in_f->num_F++] = f;
    }
}

/*
void mtxop_subspace(State * start_state,  int* rsa_facts, int* rsa_actions,
                    int *decr_g)
{ 
   int i; 
   for(i=0;i<gnum_ft_conn;i++) {
       if(contain_fact_state(start_state, i)) continue;
       //printf("%d ", i);print_ft_name(i);printf("\n mtxop :\n");
       block_mtxop_subspace(start_state, i, rsa_facts, 
                           rsa_actions,  decr_g);
   }     
}

void some_mtxop_subspace(State * start_state,  int* rsa_facts, int* rsa_actions,
                    int *decr_g, vState *vs)
{ 
   int i,f; 
   for(i=0;i<vs->num_F;i++) {
       f = vs->F[i];
       block_mtxop_subspace(start_state, f, rsa_facts, 
                           rsa_actions,  decr_g);
   }     
}*/

int build_subspace(State * start_state,  int* rsa_facts, int* rsa_actions, State* omita)
{  
   static int first_time = 0; 
   int i, j, k, f, a, ii;
   int level;
   vState S,newS;
  
   /* work space initialization */ 
   if(first_time == 0) {
        alloc_fact_supp();
        first_time = 1;
   }
   
   
   /* initialization */ 
   restore_subspace_actions();
   zero_fact_supp();
  
   
   for (i = 0; i < gnum_ft_conn; i++) rsa_facts[i] = -1; 
   for(i=0; i<start_state->num_F; i++) {
       //if(start_state->F[i] == Gf) return 1;
       rsa_facts[start_state->F[i]] = 0;
       _fact_supp[0][start_state->F[i]] ++;
   }
   for (i = 0; i < gnum_ef_conn; i++) rsa_actions[i] = -1;
                                                    
   /* Forward until reach the goal */
   level = 0;
 
    vs_source_to_dest(&S, start_state);
   
   while(1)
   {
       newS.num_F = 0;
      
      if(level >= MAX_LEVELS-2) {
        printf("MAX_LEVES in subspace.c too small\n");
        exit(0);
      } 
    
      /*
       printf("\nlevel %d\n", level);
       for (i = 0; i < gnum_ft_conn; i++) 
           //if(rsa_facts[i] == level){ 
           if(rsa_facts[i] >=0){ 
                print_ft_name(i); printf(" %d %d\n",i, _fact_supp[level][i]);
           }
       printf("\n");
       */

       if(S.num_F == 0) break; 
      
       for (i = 0; i < gnum_ft_conn; i++) 
           _fact_supp[level+1][i] =  _fact_supp[level][i];
       
       for (ii = 0; ii < S.num_F; ii++) {
                 
                 i = S.F[ii];
                
                 for (j = 0; j < gft_conn[i].num_PC; j++) {
                    a = gft_conn[i].PC[j];
                    if(rsa_actions[a] < 0) {
                      gef_conn[a].num_pcc--;
                      if((gef_conn[a].num_pcc == 0)
                              &&!contain_fact_state(omita, a)
                      ) {
                          /* 
                         printf("add action %d ", a);
                         print_op_name(gef_conn[a].op); printf("\n");
                         */
                         rsa_actions[a] = level;
                         
                         for (k = 0; k < gef_conn[a].num_A; k++) {
                            f = gef_conn[a].A[k];
                            if(f>=0) {
                              if(rsa_facts[f] < 0) {
                                    rsa_facts[f] = level+1; 
                                    newS.F[newS.num_F++] = f;
                              }
                              _fact_supp[level+1][f] ++;
                            }
                         }
                         if (gef_conn[a].sf) {
                            for (k = 0; k < gef_conn[a].sf->num_A_start; k++) {
                                f = gef_conn[a].sf->A_start[k];
                                if(f>=0) { 
                                   if(rsa_facts[f] < 0) { 
                                        rsa_facts[f] = level+1; 
                                        newS.F[newS.num_F++] = f;
                                   }
                                   _fact_supp[level+1][f] ++;
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
               if(rsa_actions[i]>=0){
                    //if(!v_contain_fact_state(&dela, i))
                         printf("have action %d %d ",rsa_actions[i], i);
                         print_op_name(gef_conn[i].op); printf("\n");
                        n++;
               }
       }                        
       printf("level %d act %d\n", level, n);
         */

       level++;
       v_source_to_dest(&S, &newS);
   }
   
   graph_level = level;
   //printf("graph_level = %d\n", graph_level );
   return 0;
}   

int block_fact_subspace(State * start_state, int ripped, int* rsa_facts, int* rsa_actions, State * gs, int *decr_fact, enum block_mode mode)
{
   int i,n,ii,j,f,a,g;
   int level;
   vState dels; int del_size;
   vState dela;
   
    
   if(rsa_facts[ripped]<0) {
       if(mode == GOAL_BLOCK) return 1;
        //case IGA_GRAPH: 
        iga_graph[ripped].out_d = 0;
        return 0;
   }
    
   //printf("ripped %d start from level %d\n",ripped, rsa_facts[ripped]);
   dels.num_F = 1;
   dels.F[0]  = ripped;
   level = rsa_facts[ripped];
   del_size =1;
   
   while(1) { 
   
        for(i=0; i<gnum_ft_conn; i++) decr_fact[i] = 0;
     
        /*
         for(i=0; i<dels.num_F;i++) {       
            printf("level %d dels %d ",level,dels.F[i]);
            print_ft_name(dels.F[i]); printf("\n");
         }
         */

        dela.num_F =0;
        for(ii=0; ii<dels.num_F;ii++){
            f = dels.F[ii];     
            for(i=0; i<gft_conn[f].num_PC;i++) {
                    
                a = gft_conn[f].PC[i];        
                if((rsa_actions[a] >=0) && (rsa_actions[a] <= level)){
                    if(!v_contain_fact_state(&dela, a)) {
                        dela.F[dela.num_F++] = a;
                        
                        /*
                        printf("level %d dela %d %d",level,a, rsa_actions[a]);
                        print_op_name(a); printf("\n");
                        */
                    }
                }
            }                                                       
        }
       
       /* 
        //Y. Chen new
        for(ii=0; ii<gft_conn[ripped].num_A; ii++) {
            a = gft_conn[ripped].A[ii];
                
            if((rsa_actions[a] >=0) && (rsa_actions[a] <= level)){
                if(!v_contain_fact_state(&dela, a)) {
                    dela.F[dela.num_F++] = a;
                }
            }
        }*/
        
        
        if(dela.num_F>=VSIZE) {
            printf("error:VSIZE too small\n");
            exit(1);
        }
        
        for(i=0; i<dela.num_F; i++) {
            a = dela.F[i];
            for(j=0; j<gef_conn[a].num_A; j++) {
                g = gef_conn[a].A[j];
                decr_fact[g]++ ;
       //         printf("decr %d %d\n",g,decr_fact[g]);
            }    

            if(gef_conn[a].sf) 
                for(j=0; j<gef_conn[a].sf->num_A_start; j++) {
                    g = gef_conn[a].sf->A_start[j];
                    decr_fact[g]++ ;
         //           printf("decr %d %d\n",g,decr_fact[g]);
                } 
        }   
   
       
        dels.num_F = 1;
        dels.F[0]  = ripped;
    
        for(i=0; i<gnum_ft_conn; i++) {
            if(rsa_facts[i]<0) continue;
            if(level<graph_level) {
                if((rsa_facts[i] <= level+1) 
                   &&(decr_fact[i] >= _fact_supp[level+1][i]))             {
                    if(!v_contain_fact_state(&dels, i)) dels.F[dels.num_F++] = i;    
                }    
            } else {
                if( decr_fact[i] >= _fact_supp[graph_level][i]) {
                    if(!v_contain_fact_state(&dels, i)) dels.F[dels.num_F++] = i;    
                }    
            }
        }      
       //printf("dels size = %d\n", dels.num_F);
       
        /*
       n=0;                 
       for(i=0; i<gnum_ef_conn; i++) {
           if(rsa_actions[i]<=level)
               if(rsa_actions[i]>=0)
                    //if(!v_contain_fact_state(&dela, i))
                        n++;
       }                        
       printf("level %d act %d %d\n", level, n, dela.num_F);
   */
                
       level++;
       if(level>graph_level) {
            if(dels.num_F >= del_size) break; 
       }    
   
       
       del_size = dels.num_F;
   }
/*   
   printf("dels size = %d\n", dels.num_F);
        for(i=0; i<dels.num_F;i++) {       
            printf("dels %d ",dels.F[i]);
            print_ft_name(dels.F[i]); printf("\n");
        }
        */
  // printf("dels size = %d\n", dels.num_F);
  // printf("\n");
     
    switch(mode) {
    case GOAL_BLOCK:
        for(i=0; i<gs->num_F; i++) {
            if(v_contain_fact_state(&dels, gs->F[i])) {
//                printf("blocking\n");
                return 0;
            }
        }
        return 1;
        break;
    case IGA_GRAPH:
        if(dels.num_F > 0) { 
       //iga_graph[ripped].v = (int*)calloc(dels.num_F, sizeof(int));
        n=0;
        for(i=0; i<dels.num_F; i++) {
            if(dels.F[i] != ripped)
                iga_graph[ripped].v[n++] = dels.F[i];
        }
        if(n >= INOUTD) {printf("INOUTD too small");exit(1);}
        iga_graph[ripped].out_d = n;
        }
        return 0;
        break;
    }
	return 0;   
}

Bool is_pn(int i)
{
    int j;
    char buf[3000];
        for(j=0; j<gef_conn[i].num_A; j++) {
            print_ft_name_string(gef_conn[i].A[j], buf);
            if(strcmp(buf, "(DUMMYPRED)")) return FALSE;
        }
        if(gef_conn[i].sf) {
             for (j = 0; j < gef_conn[i].sf->num_A_start; j++) {
                print_ft_name_string( gef_conn[i].sf->A_start[j], buf);
                if(strcmp(buf, "(DUMMYPRED)")) return FALSE;
             } 
        }
        return TRUE;

}
void pure_numerical_actions()
{
    int i;  
     
    for(i=0; i<gnum_ef_conn; i++) {
        if(is_pn(i)) {
            //printf("pn %d\n",i);
            GpG.op_pure_numeric[i] = 1;
        }
        else 
            GpG.op_pure_numeric[i] = 0;
    }
}

                 
void needf_imf_ima_fof(State * start_state, int *rsa_facts, int *rsa_actions, 
        int *decr_g, vState* needf )
{ 
    int  i,j,k,f,a,g,gg,kk;
    State v, va;
    Bool gAdd;
    
    
    for(i=0; i<needf->num_F; i++) {
        f =needf->F[i];
        // printf("iga %d ", f);print_ft_name(f); printf("\n");
        
        // get imf
        v.num_F = 0;
        for(j=0; j<iga_graph[f].in_d; j++) {
            g = iga_graph[f].w[j];
            gAdd = TRUE;
            for(k=0; k<iga_graph[f].in_d; k++) {
                if(k==j) continue;
                gg = iga_graph[f].w[k];
                if(!(iga_graph[gg].w)) continue;
                for(kk=0; kk<iga_graph[gg].in_d;kk++) {
                    if(g == iga_graph[gg].w[kk]) {
                        gAdd = FALSE;
                        break;
                    }        
                }
               if(!gAdd) break; 
            }
            if(gAdd) {
                v.F[v.num_F++] = g;
           //     printf("    imf : %d ",g); print_ft_name(g); printf("\n");
            } 
        }
        //printf("  %d\n",v.num_F);
        
        iga_graph[f].n_imf = v.num_F;
        for(j=0; j<v.num_F; j++) {
            iga_graph[f].imf[j] = v.F[j];            
        } 
         
        
        // get ima
        if(v.num_F > 0){
          va.num_F = 0;
          for(j=0; j<gft_conn[f].num_A; j++) {
            a = gft_conn[f].A[j];
            //printf("add for %d", f); print_op(a);
            gAdd = TRUE;
            for(k=0; k<v.num_F; k++) {
                for(kk = 0; kk<gef_conn[a].num_PC; kk++) {
                    if(gef_conn[a].PC[kk] == v.F[k]) break;
                }       
                if(kk == gef_conn[a].num_PC) break;
            }          
            if(k<v.num_F) gAdd = FALSE;
            if(gAdd) {
                nosame_add_action(&va, a);
            //    va.F[va.num_F++] = a;
            //    printf("    ima : %d ",a); print_op_name(a); printf("\n");
            }           
          }   
        } 
        else {
            va.num_F =0;
            for(j=0; j<gft_conn[f].num_A; j++) {
                a = gft_conn[f].A[j];
                if(actionable(start_state, a)) {
                //    va.F[va.num_F++] = a;
                    nosame_add_action(&va, a);
              //      printf("    s-ima : %d ",a); print_op_name(a); printf("\n");
                }
            }
        }

        
        iga_graph[f].n_ima = va.num_F;
        for(j=0; j<va.num_F; j++) iga_graph[f].ima[j] = va.F[j];            
        
        if(iga_graph[f].n_ima == 1) {
            iga_graph[f].ima_nailed = TRUE;
            if(iga_graph[f].n_imf == 0) iga_graph[f].ima_type = 0;
            else  iga_graph[f].ima_type = 10;
        }
        else iga_graph[f].ima_nailed = FALSE;
    }               
    
    // get fof
    for(i=0;i<needf->num_F;i++) {
        f = needf->F[i];
        v.num_F = 0;
        for(j=0; j<needf->num_F; j++) {
            if(i==j) continue;
            g = needf->F[j];
            for(k=0; k<iga_graph[g].n_imf; k++) {
              if(iga_graph[g].imf)
                if(iga_graph[g].imf[k] == f) {
                    v.F[v.num_F++] = g;
                    break; 
                }               
            }
        }
        iga_graph[f].n_fof = v.num_F;
        for(j=0; j<v.num_F; j++) iga_graph[f].fof[j] = v.F[j];            
    }       
}

void new_sym_path(State *symfs, State *omita, int* omi_select)
{           
   int i,x,k,a,aa;
           
   omita->num_F = 0;
   
   for(k=0; k<symfs->num_F; k++) { 
        x = symfs->F[k];
        if(iga_graph[x].n_ima > 0) {
            aa = iga_graph[x].ima[omi_select[k]];
          for(i=0; i<gft_conn[x].num_A; i++) {
            a = gft_conn[x].A[i];
            if(a != aa) {
             //   printf("omita %d sl %d",k, omi_select[k]);print_op(a);
                add_fs(omita, a); 
            }
          }
            
        }
        else{
          for(i=0; i<gft_conn[x].num_A; i++) {
            a = gft_conn[x].A[i];
            if(i != omi_select[k]) {
             //   printf("omita %d sl %d",k, omi_select[k]);print_op(a);
                add_fs(omita, a); 
            }
          }
        }
   }
}

/*
void genIMA_analysis(State * start_state, int *rsa_facts, int *rsa_actions
        ,int *decr_g)
{
    vState needf;
    int gk, i,j,k,f,g,a;
    State S;
    State omita, symfs, ext_omita, seleta;
    int omi_select[2000];
    Bool bol;
   
    
    build_iga_graph(start_state, rsa_facts, rsa_actions, decr_g);
   
    ext_omita.num_F = 0;
   
 for(gk=0; gk<3; gk++) {
        
    precede_facts_of_state(&saved_ggoal_state, &needf);
    needf_imf_ima_fof(start_state, rsa_facts, rsa_actions, decr_g, &needf);
    
    if(gk==2) break; 
    
    symfs.num_F = 0;
    for(i=0; i<needf.num_F; i++) {
        f = needf.F[i];
     //   printf("needf %d ",i); 
        if(contain_fact_state(start_state, f)) {
     //       printf("in start ");
        }else {
            if(iga_graph[f].ima_nailed) {
     //           printf("nailed ");
            } else {
                bol = TRUE;
                for(j=0; j<iga_graph[f].out_d; j++) {
                    g = iga_graph[f].v[j];
                    //print_ft(g);
                    if(!v_contain_fact_state(&needf, g)) continue;
                    if(!iga_graph[g].ima_nailed) bol = FALSE;
                }
                if(bol) {
                    symfs.F[symfs.num_F++] = f;
                   // printf("sym ");    
                } else { //printf("free ");
                }
            }    
        }
        //print_ft(f);
    }
   
    if(symfs.num_F > 0) { 
      for(i=0; i<symfs.num_F; i++)  omi_select[i] = 0;
      while(1) {
        
       // if(check_omita_ext(&seleta, &symfs, omi_select)) {
            new_sym_path(&symfs, &omita, omi_select); 
            append_state(&omita, &ext_omita);
            printf("size of omita = %d\n", omita.num_F);
       
            clear_iga_graph();
            build_subspace(start_state, rsa_facts, rsa_actions, &omita);
            if(goal_reached(&saved_ggoal_state, rsa_facts)) {
                printf("reached all goals\n");
                break;
            }
        //}
        i = symfs.num_F - 1;
        while(1) {
            omi_select[i]++;
            f =  symfs.F[i];
            //if(omi_select[i] < gft_conn[f].num_A) break;
            if(iga_graph[f].n_ima>0){
                if(omi_select[i] < iga_graph[f].n_ima) break;
            } else {
                if(omi_select[i] < gft_conn[f].num_A) break;
            }
            
            {
                omi_select[i] = 0;
                i--;
                if(i<0) {
                    printf("single omita failed"); exit(1);
                }
            }
        } 
      }
    }
    source_to_dest(&ext_omita, &omita); 
    
    
    for(i=0;i<gnum_ft_conn;i++) {
       if(contain_fact_state(start_state, i)) continue;
       block_fact_subspace(start_state, i, rsa_facts, 
                           rsa_actions, NULL, decr_g, IGA_GRAPH);
    }   
    build_in_edges(decr_g);
 }    
}*/ 

void sim_mutex_all(int fact, State *S)
{
   int j; 
   
   S->num_F = 0;
   for(j=0; j<gnum_ft_conn; j++) {
        if((ft_predlen(j)>10)&&(ft_arity(j)==1)) { 
            if(ft_argncmp(fact, j, 0, 0) == 0) {
                S->F[S->num_F++] = j;
            }
        }
   }
}
void ft_mutex_all(int fact, State *S)
{
   int j; 
   
   S->num_F = 1;
   S->F[0] = fact;
   for(j=0; j<gnum_ft_conn; j++) {
       if(GET_BIT (FT_FT_mutex[fact], j)) {
   //        printf(" m "); print_ft(j);
           S->F[S->num_F++] = j;  
       }             
   }
}

void ft_mutex_closure(int fact, State *closure, int num_allm)
{
   State S;     
   int i,j,f; 
   
   closure->num_F = 0;
   S.num_F = 1;
   S.F[0] = fact;
   for(j=0; j<gnum_ft_conn; j++) {
       if(GET_BIT (FT_FT_mutex[fact], j)) {
           S.F[S.num_F++] = j;  
       }             
   }
   
   for(i=0;i<S.num_F; i++) {
    f = S.F[i];
//    printf("  m  ");print_ft(f);

    for(j=0; j<gnum_ft_conn; j++) {
        if(GET_BIT (FT_FT_mutex[f], j)) {
  //          printf("\t\t");print_ft(j);
            if((gft_conn[j].num_add>=0) && (gft_conn[i].num_add<num_allm))
                continue;
            if(!contain_fact_state(&S, j)) break;
        }
     }
     if(j==gnum_ft_conn) {
    //    printf("in closure"); print_ft(f);
        closure->F[closure->num_F++] = f;
     } 
   } 
}


void must_pred_facts(int fact, State *M)
{
    int i;
     
    for(i=0; i<gft_conn[fact].num_A; i++) {
            
    }
} 

void ft_delete_impact(State *dS, int *rsa_facts, int *rsa_actions)
{
   int i, j, k, f, a, ii;
   int level,fact;
   vState S,newS;
  
   // prepare 
    for (i = 0; i < gnum_ft_conn; i++) rsa_facts[i] = -1;
    S.num_F = 0;
    for(i=0; i<dS->num_F; i++) { 
        fact = dS->F[i];
        rsa_facts[fact] = 0;
        S.F[S.num_F++] = fact;
    }       
    for (i = 0; i < gnum_ef_conn; i++) rsa_actions[i] = -1;
    for(i=0; i<gnum_ft_conn; i++) {
        gft_conn[i].num_add = gft_conn[i].num_A;
    }
   
    
   // iterate
   level = 0;
   while(1)
   {
      newS.num_F = 0;
      if(level >= MAX_LEVELS-2) {
        printf("MAX_LEVES in subspace.c too small\n");
        exit(0);
      } 
       if(S.num_F == 0) break; 
       
       for (ii = 0; ii < S.num_F; ii++) {
                 i = S.F[ii];
                
                 for (j = 0; j < gft_conn[i].num_PC; j++) {
                    a = gft_conn[i].PC[j];
                    if(rsa_actions[a]<0) {
                      //  printf("add");print_op(a);
                        rsa_actions[a] = level;
                         
                        for (k = 0; k < gef_conn[a].num_A; k++) {
                            f = gef_conn[a].A[k];
                            if(f>=0) {
                              if(rsa_facts[f] < 0) {
                        //           printf("del"); print_ft(f); 
                                   gft_conn[f].num_add--;
                                   if(gft_conn[f].num_add<=0) {   
                                        rsa_facts[f] = level+1; 
                                        newS.F[newS.num_F++] = f;
                                   }
                              }
                            }
                        }
                    }
                 }
       
       }
       level++;
       v_source_to_dest(&S, &newS);
   }
}

void mutex_connectivity(int begin, int end, State *dS, int *rsa_facts, 
        int *rsa_actions, State *M)
{
   int i, j, k, f, a, ii;
   int level;
   vState S,newS;
  
   // prepare 
   for (i = 0; i < gnum_ft_conn; i++) rsa_facts[i] = -1;
   rsa_facts[begin] = 0;
   S.F[0] = begin; S.num_F = 1;
   for (i = 0; i < gnum_ef_conn; i++) rsa_actions[i] = -1;
    
   // iterate
   level = 0;
   while(1)
   {
      newS.num_F = 0;
      if(level >= MAX_LEVELS-2) {
        printf("MAX_LEVES in subspace.c too small\n");
        exit(0);
      } 
      if(S.num_F == 0) break; 
       
      for (ii = 0; ii < S.num_F; ii++) {
                 i = S.F[ii];
                // printf("handle"); print_ft(i); 
                 for (j = 0; j < gft_conn[i].num_PC; j++) {
                    a = gft_conn[i].PC[j];
                    if(rsa_actions[a]<0) {
                       // printf("l %d add",level);print_op(a);
                        rsa_actions[a] = level;
                         
                        for (k = 0; k < gef_conn[a].num_A; k++) {
                            f = gef_conn[a].A[k];
                            if(contain_fact_state(M, f)) {
                              if(rsa_facts[f] < 0) {
                                   printf("\tl %d reach", level+1); print_ft(f); 
                                   rsa_facts[f] = level+1; 
                                   newS.F[newS.num_F++] = f;
                              }
                            }
                        }
                    }
                 }
       
       }
       level++;
       v_source_to_dest(&S, &newS);
   }
}
    
Bool act_connected(int f1, int f2)
{
    int i,j,a;
    for(i=0; i<gft_conn[f1].num_A; i++) {
        a = gft_conn[f1].A[i];
        for(j=0; j<gef_conn[a].num_A; j++) {
            if(f2 == gef_conn[a].A[j]) return TRUE;
        }
    }
    return FALSE;
}

        
Bool act_connect_symmetry(int f1, int g)
{
    int i,j,a,aa,f2,f3,sym,k,kk;
    //printf("act_connect"); print_ft(f1);
    for(i=0; i<gft_conn[f1].num_PC; i++) {
        a = gft_conn[f1].PC[i];
        for(j=0; j<gef_conn[a].num_A; j++) {
            f2 = gef_conn[a].A[j];
            sym = gft_conn[f2].num_add;
            if(sym >= 0) {
              if(Symmetry[sym].type == 1) {    
               // printf("\t"); print_ft(f2);
                if(f2==g) return TRUE;
              } else {
                // printf("type 0\t"); print_ft(f2);
                if(gft_conn[f2].num_MX != 3) continue;
                  for(k=0; k<gft_conn[f2].num_PC; k++) {
                    aa = gft_conn[f2].PC[k];
                    for(kk=0; kk<gef_conn[aa].num_A; kk++) {
                        f3 = gef_conn[aa].A[kk];
                        if(gft_conn[f3].num_add>=0) {
                           // printf("\t"); print_ft(f3);
                            if(f3==g) return TRUE;
                        }
                    }
                }
              }
            }
        }   
    }
    return FALSE;
}
void common_comp_nef(int f, State *S)
{
    int k,kk,a,g,j,ff,sym;
    
    S->num_F = 0;
    
    for(k=0; k<gft_conn[f].num_PC; k++) {
        a = gft_conn[f].PC[k];
        if(gef_conn[a].spec == 0) continue;
        for(kk=0; kk<gef_conn[a].num_D; kk++) {
            g = gef_conn[a].D[kk];
            if(gft_conn[g].num_MX == 3) {
            //if(gft_conn[ff].num_add<gnum_symm_norm){
                sym = gft_conn[g].num_add;
                if(sym <0) continue;
                for(j=0; j<Symmetry[sym].size; j++) {
                    ff = Symmetry[sym].f[j];
                    if(gft_conn[ff].num_MX!=3) break;
              //      if(gft_conn[ff].num_add>=gnum_symm_norm) break;
                }
                if(j==Symmetry[sym].size) {
                    add_fs(S, g);
                }
            }    
        }
    }
}

void strongest_comp_nef(int f, State *S, int rp)
{
    int k,kk,a,g,n;
    int max_n = 0;
    State P;
    
    P.num_F = 0;
    S->num_F = 0;
    for(k=0; k<gft_conn[f].num_PC; k++) {
        a = gft_conn[f].PC[k];
        for(kk=0; kk<gef_conn[a].num_D; kk++) {
            g = gef_conn[a].D[kk];    
            if(g== f) continue;
            if(gft_conn[g].num_add < 0) continue;
            if(Symmetry[gft_conn[g].num_add].size >2) continue;
            P.F[P.num_F++] = g;
        }      
    }
    for(k=0; k<P.num_F; k++) {
        g = P.F[k];
        n = 0;
        for(kk=0; kk<P.num_F; kk++) {
            if(g == P.F[kk]) n++;
        }
        if(n > max_n) { max_n = n; }
    }
    max_n = 2;
    //printf(" max_n = %d\n", max_n );
    for(k=0; k<P.num_F; k++) {
        g = P.F[k];
        n = 0;
        for(kk=0; kk<P.num_F; kk++) {
            if(g == P.F[kk]) n++;
        }
        if(n >= max_n) { 
            if(!contain_fact_state(S, g)) {
      //          printf("\t%d",n); print_ft(g);
                S->F[S->num_F++] = g;
            }    
        }
    }
}         
void strongest_comp_ef(int f, State *S, int rp)
{
    int k,kk,a,g,n;
    int max_n = 0;
    State P;
    
    P.num_F = 0;
    S->num_F = 0;
    for(k=0; k<gft_conn[f].num_PC; k++) {
        a = gft_conn[f].PC[k];
        for(kk=0; kk<gef_conn[a].num_A; kk++) {
            g = gef_conn[a].A[kk];    
            P.F[P.num_F++] = g;
        }      
    }
    for(k=0; k<P.num_F; k++) {
        g = P.F[k];
        n = 0;
        for(kk=0; kk<P.num_F; kk++) {
            if(g == P.F[kk]) n++;
        }
        if(n > max_n) { max_n = n; }
    }
    //printf(" max_n = %d\n", max_n );
    for(k=0; k<P.num_F; k++) {
        g = P.F[k];
        n = 0;
        for(kk=0; kk<P.num_F; kk++) {
            if(g == P.F[kk]) n++;
        }
        if(n == max_n) { 
            if(!contain_fact_state(S, g)) {
      //          printf("\t%d",n); print_ft(g);
                S->F[S->num_F++] = g;
            }    
        }
    }
}         
void strongest_szero_pc(int f, State *S, int rp)
{
    int k,kk,a,g,n;
    int max_n = 0;
    State P;
    
    P.num_F = 0;
    S->num_F = 0;
    for(k=0; k<gft_conn[f].num_A; k++) {
        a = gft_conn[f].A[k];
        if(gef_conn[a].spec!=0) continue;
        for(kk=0; kk<gef_conn[a].num_PC; kk++) {
            g = gef_conn[a].PC[kk];    
            P.F[P.num_F++] = g;
        }      
    }
    for(k=0; k<P.num_F; k++) {
        g = P.F[k];
        n = 0;
        for(kk=0; kk<P.num_F; kk++) {
            if(g == P.F[kk]) n++;
        }
        if(n > max_n) { max_n = n; }
    }
    //printf(" max_n = %d\n", max_n );
    for(k=0; k<P.num_F; k++) {
        g = P.F[k];
        n = 0;
        for(kk=0; kk<P.num_F; kk++) {
            if(g == P.F[kk]) n++;
        }
        if(n == max_n) { 
            if(!contain_fact_state(S, g)) {
            //    printf("\t%d",n); print_ft(g);
                S->F[S->num_F++] = g;
            }    
        }
    }
}         
void strongest_comp_pc(int f, State *S, int rp)
{
    int k,kk,a,g,n;
    int max_n = 0;
    State P;
    
    P.num_F = 0;
    S->num_F = 0;
    for(k=0; k<gft_conn[f].num_A; k++) {
        a = gft_conn[f].A[k];
        for(kk=0; kk<gef_conn[a].num_PC; kk++) {
            g = gef_conn[a].PC[kk];    
            P.F[P.num_F++] = g;
        }      
    }
    for(k=0; k<P.num_F; k++) {
        g = P.F[k];
        n = 0;
        for(kk=0; kk<P.num_F; kk++) {
            if(g == P.F[kk]) n++;
        }
        if(n > max_n) { max_n = n; }
    }
    //printf(" max_n = %d\n", max_n );
    for(k=0; k<P.num_F; k++) {
        g = P.F[k];
        n = 0;
        for(kk=0; kk<P.num_F; kk++) {
            if(g == P.F[kk]) n++;
        }
        if(n == max_n) { 
            if(!contain_fact_state(S, g)) {
            //    printf("\t%d",n); print_ft(g);
                S->F[S->num_F++] = g;
            }    
        }
    }
}         
void repetitive_pc(int f, State *S, int rp)
{
    int k,kk,a,g,n;
    int max_n = 0, rep =0;
    State P;
    
    P.num_F = 0;
    S->num_F = 0;
    for(k=0; k<gft_conn[f].num_A; k++) {
        a = gft_conn[f].A[k];
        for(kk=0; kk<gef_conn[a].num_PC; kk++) {
            g = gef_conn[a].PC[kk];    
            P.F[P.num_F++] = g;
        }      
    }
    for(k=0; k<P.num_F; k++) {
        g = P.F[k];
        n = 0;
        for(kk=0; kk<P.num_F; kk++) {
            if(g == P.F[kk]) n++;
        }
        if(n > max_n) { max_n = n; }
    }
    for(k=0; k<P.num_F; k++) {
        g = P.F[k];
        n = 0;
        for(kk=0; kk<P.num_F; kk++) {
            if(g == P.F[kk]) n++;
        }
        if((n < max_n)&&(n>rep)) { rep = n; }
    }
    printf(" max_n = %d ", max_n );
    printf(" rep = %d\n", rep );
    for(k=0; k<P.num_F; k++) {
        g = P.F[k];
        //if(gft_conn[g].num_add<0) continue;
        n = 0;
        for(kk=0; kk<P.num_F; kk++) {
            if(g == P.F[kk]) n++;
        }
        if((n>=rp)&&(n >= rep)) { 
            if(!contain_fact_state(S, g)) {
                printf("\t%d",n); print_ft(g);
                S->F[S->num_F++] = g;
            }    
        }
    }
}         
    
void get_common_pc(int f, State *S)
{
 strongest_comp_pc(f, S, 0);
//    repetitive_pc(f, S, 4);
}

void print_symm()
{
    int i,j,f;
    for(i=0;i<gnum_symm; i++) {
        printf("Symmetry %d   Type = %d   Size = %d\n",
                i, Symmetry[i].type, Symmetry[i].size);
        for(j=0; j<Symmetry[i].size;j++) {
            f = Symmetry[i].f[j];
            printf("\t"); print_ft(Symmetry[i].f[j]);
        }
    }
}
void print_symm_group()
{
    int i,j,f;
    for(i=0;i<gnum_symm_group; i++) {
        printf("Group %d\n",i);
        for(j=0; j<SymmetryGroup[i].size;j++) {
            f = SymmetryGroup[i].g[j];
            printf("\t%d ",j); print_ft(SymmetryGroup[i].g[j]);
        }
    }
}

Bool same_cpc(State *S1, State *S2)
{
    int i;
    if(S1->num_F != S2->num_F) return FALSE;
    for(i=0;i<S1->num_F; i++) {
        if(contain_fact_state(S2, S1->F[i])) {
            return TRUE;
        }
    }
    return FALSE;
}

Bool all_same_cpc(State *S1, State *S2)
{
    int i;
    if(S1->num_F != S2->num_F) return FALSE;
    for(i=0;i<S1->num_F; i++) {
        if(!contain_fact_state(S2, S1->F[i])) {
            return FALSE;
        }
    }
    return TRUE;
}
Bool common_cpc(State *S1, State *S2)
{
    int i;
    for(i=0;i<S1->num_F; i++) {
        if(contain_fact_state(S2, S1->F[i])) {
            return TRUE;
        }
    }
    return FALSE;
}
int find_symm_group(int f)
{
    int i,j;
    for(i=0; i<gnum_symm_group; i++) {
        for(j=0; j<SymmetryGroup[i].size; j++) {
            if(SymmetryGroup[i].g[j] == f) return i;
        }
    }
    return -1;
}

void spec_op_info()
{
    int i;
   
    for(i=0; i<gnum_op_conn; i++) {
        if((ef_predlen(i) ==7) || (ef_predlen(i)==8)) {
            gef_conn[i].spec = 1;
        } else {                                        
            if((ef_predlen(i) ==9) || (ef_predlen(i)==10)) {
                gef_conn[i].spec = 2;
            } else {
                gef_conn[i].spec = 0;
            }
        }

    }       
     
    /* 
    for(i=0; i<gnum_op_conn; i++) {
        if(gef_conn[i].num_PC<3) {
            gef_conn[i].spec = 0;
            continue;
        }
        if((gef_conn[i].num_PC == 4) && (gef_conn[i].num_A == 5)) {
            gef_conn[i].spec = 0;
            continue;
        }
        for(j=0; j<gef_conn[i].num_PC; j++) {
            f = gef_conn[i].PC[j];
            if(gft_conn[f].num_MX==3) {
               // printf("PC %d A %d", gft_conn[f].num_PC, gft_conn[f].num_A);
               // print_ft(f);
                if(gft_conn[f].num_PC>gft_conn[f].num_A) {
                    gef_conn[i].spec = 1;                
                } else {
                if(gft_conn[f].num_PC<gft_conn[f].num_A) {
                    gef_conn[i].spec = 2;
                } else {
                    gef_conn[i].spec = 2;
                    for(k=0; k<gef_conn[i].num_A; k++) {
                        g = gef_conn[i].A[k];
                        if(gft_conn[g].num_add>=0){ 
                            if(Symmetry[gft_conn[g].num_add].type ==1) {
                                gef_conn[i].spec = 1;
                                break;
                            } 
                        }
                    }
                }
                }
            }
       }       
    }*/

    
}

void print_op_info()
{
    int i/*, f, j*/;
    
    for(i=0; i<gnum_op_conn; i++) {
       printf("PC %d A %d D %d spec %d",
               gef_conn[i].num_PC, gef_conn[i].num_A, gef_conn[i].num_D,
               gef_conn[i].spec);
       print_op(i); 
        /*
       for(j=0; j<gef_conn[i].num_PC; j++) {
            f = gef_conn[i].PC[j];
            if(gft_conn[f].num_MX==3) {
                print_ft(f);
            }
       }       
        for(j=0; j<gef_conn[i].num_A; j++) {
            f = gef_conn[i].A[j];
            if(gft_conn[f].num_MX==3) {
                print_ft(f);
            }
       }       
        for(j=0; j<gef_conn[i].num_D; j++) {
            f = gef_conn[i].D[j];
            if(gft_conn[f].num_MX==3) {
                print_ft(f);
            }
       }  */     
    
    }
}

/*
Bool static_symm(int f)
{
    int i,j,k;
    for(i=
}*/

void mutex_ft_info()
{
    int i;
    State M_all;
    for(i=0; i<gnum_ft_conn; i++) {
      ft_mutex_all(i, &M_all);
      gft_conn[i].num_MX = M_all.num_F; 
    }
}

void print_ft_info()
{
    int i;
    for(i=0; i<gnum_ft_conn; i++) {
      printf("PC %d A %d D %d mtx %d",
               gft_conn[i].num_PC, 
               gft_conn[i].num_A, 
               gft_conn[i].num_D, 
               gft_conn[i].num_MX);
       print_ft(i); 
    }
}

int symm_norm_fact(int fact, int mode)
{
    int i,a,f,j;
    
    a = -1;
    for(i=0; i<gft_conn[fact].num_PC; i++) {
        a=gft_conn[fact].PC[i];
        if(gef_conn[a].spec == 2) {
            for(j=0; j<gef_conn[a].num_PC; j++) {
                f = gef_conn[a].PC[j];
                if(gft_conn[f].num_add >=0) {
                    if(gft_conn[f].num_add < gnum_symm_norm) {
                        return f;
                    }
                }
            }
        }
    }
    if(a<0) return -2;
    return gef_conn[a].spec - 2;
}

void dir_symm_group(int fact)
{
    int i,a,j,s = -1,d = -1,sg,dg;
    
    a = -1;
    for(i=0; i<gft_conn[fact].num_PC; i++) {
        a=gft_conn[fact].PC[i];
        if(gef_conn[a].spec == 0) {
            for(j=0; j<gef_conn[a].num_PC; j++) {
                s = gef_conn[a].PC[j];
                if(gft_conn[s].num_add >= gnum_symm_subnorm) {
                    break;
                }
            }
            for(j=0; j<gef_conn[a].num_A; j++) {
                d = gef_conn[a].A[j];
                if(gft_conn[d].num_add >= gnum_symm_subnorm) {
                    break;
                }
            }
            break;
        }
    }

    //printf("s"); print_ft(s);
    //printf("d"); print_ft(d);
    sg = find_symm_group(s);
    dg = find_symm_group(d);
    SymmGroupNorm[sg][dg] = 1;
    SymmGroupNorm[dg][sg] = 1;
    SymmGroupFar[sg][dg] = gconstants[grelevant_facts[fact].args[1]];
    SymmGroupFar[dg][sg] = gconstants[grelevant_facts[fact].args[1]];
    //printf("Far %d %d %s\n", sg, dg, SymmGroupFar[sg][dg]);
}                           

void rout_symm_group(int fact, int nnrm)
{
    int i,a,j,s,d,f, sg = -1,dg, fsymm;
    
    a = -1; s=-1; d=-1;
    fsymm = gft_conn[fact].num_add;
    for(i=0; i<gft_conn[fact].num_PC; i++) {
        a=gft_conn[fact].PC[i];
        if(gef_conn[a].spec == 2) {
            for(j=0; j<gef_conn[a].num_D; j++) {
                  f = gef_conn[a].D[j];
                  if(gft_conn[f].num_add >= gnum_symm_subnorm) {
                        if(s<0) {s=f; sg=find_symm_group(s); break; }
                        else {
                            if(sg != find_symm_group(f)) {
                                d=f;
                                //printf("action %d", ef_predlen(a)); print_op(a);
    //                            printf("s"); print_ft(s);
    //                            printf("d"); print_ft(d);
                                dg = find_symm_group(d);
                                if(ef_predlen(a) == MAX_PREDLEN){
                                    SymmNormGr[fsymm] = sg;
                                    SymmNormGl[fsymm] = dg;
                                } else {
                                    SymmNormGl[fsymm] = dg;
                                    SymmNormGr[fsymm] = sg;
                                }
                                
                                SymmGroupNorm[sg][dg] = nnrm;
                                SymmGroupNorm[dg][sg] = nnrm;
    SymmGroupFar[sg][dg] = gconstants[grelevant_facts[fact].args[0]];
    SymmGroupFar[dg][sg] = gconstants[grelevant_facts[fact].args[0]];
  //  printf("Far %d %d %s\n", sg, dg, SymmGroupFar[sg][dg]);
                                return;
                            }
                        }
                        break;
                  }
            }
        }
    }
}

void print_symm_group_norm()
{
    int i,j;
    for(i=0;i<gnum_symm_group; i++){
        for(j=0;j<gnum_symm_group; j++) {
            printf("Group %d <--> Group %d [%d]\n", i,j, SymmGroupNorm[i][j]);
        }
    }
}

void fact_connection(State *start_state)
{
    int i,f,norm,old_norm = 10000, nnrm = 10000;
    int mode=0; 
    
    for(i=0; i<start_state->num_F; i++) {
        f=start_state->F[i];
        //print_ft(f);
        if(gft_conn[f].num_add>=0) continue;
       // printf("X\n");  
        
        norm = symm_norm_fact(f, mode);
       
        //print_ft_name(f);
        //printf(" norm = %d ", norm);
        //if(norm>=0) print_ft_name(norm);
        //printf("\n");
        
        switch(mode) {
        case 0:
            if(norm == -2) { mode = 1;}
            if(norm == -1) { printf("wrong mode"); exit(0); }
            if(norm >=0) {mode = 2; old_norm = norm; nnrm = 1; }
            break;
        case 1: 
            if(norm != -2) {printf("wrong model"); exit(0);}
            dir_symm_group(f);
            mode = 0;
            break;
        case 2: 
            if(norm == -1) {nnrm++;}
            if(norm >=0) {
                if(norm!=old_norm) {printf("wrong norm"); exit(0);}
          //      printf("== nnrm = %d\n", nnrm);
                rout_symm_group(norm, nnrm);
                mode = 0;
            }
            break;
        default:
            printf("wrong default mode"); exit(0);
        }
    }
}

void action_connection()
{
    int i,f,j,x;
    
    for(i=0; i<gnum_op_conn; i++) {
       x = 0;
       for(j=0; j<gef_conn[i].num_PC; j++) {
            f = gef_conn[i].PC[j];
            if(find_symm_group(f)>=0) {
                printf("PC");print_ft(f);
                x++; break;
            }
       }       
        for(j=0; j<gef_conn[i].num_A; j++) {
            f = gef_conn[i].A[j];
            if(find_symm_group(f)>=0) {
                printf("A");print_ft(f);
                x++; break;
            }
       }       
        for(j=0; j<gef_conn[i].num_D; j++) {
            f = gef_conn[i].D[j];
            if(find_symm_group(f)>=0) {
                printf("D");print_ft(f);
                x++; break;
            }
       }     
       gef_conn[i].spec = x; 
       printf("PC %d A %d D %d spec %d",
               gef_conn[i].num_PC, gef_conn[i].num_A, gef_conn[i].num_D,
               gef_conn[i].spec);
       print_op(i);
    }
}


void normal_facts()
{
    int i, m=0;
    for(i=0; i<gnum_ft_conn; i++) {
        if(gft_conn[i].num_A > m) { m=gft_conn[i].num_A; }
    }
    
    for(i=0; i<gnum_ft_conn; i++) {
        if(gft_conn[i].num_A > m-50) { 
      printf("PC %d A %d D %d mtx %d",
               gft_conn[i].num_PC, 
               gft_conn[i].num_A, 
               gft_conn[i].num_D, 
               gft_conn[i].num_MX);
            print_ft(i);
        }
    }
}

void normal_action()
{
    int i;
    for(i=0; i<gnum_op_conn; i++) {
        if(gef_conn[i].num_PC == 2) gef_conn[i].spec = 0;
        if((gef_conn[i].num_PC == 4) && (gef_conn[i].num_A == 5)) gef_conn[i].spec = 0;
        if(gef_conn[i].num_PC == 3) gef_conn[i].spec = 1;
        if((gef_conn[i].num_PC == 4) && (gef_conn[i].num_A == 4))  gef_conn[i].spec = 1; 
       printf("PC %d A %d D %d spec %d",
               gef_conn[i].num_PC, gef_conn[i].num_A, gef_conn[i].num_D,
               gef_conn[i].spec);
       print_op(i);
    }
}
    
void compartment_facts()
{
    int i,j;
    for(i=0; i<gnum_ft_conn; i++) {
        for(j=0; j<gnum_ft_conn; j++) {
           if(i==j) continue;
           if(compartment_ops(i,j)) {
            print_ft_name(i); printf(" ");
            print_ft_name(j); printf("\n");
           } 
        }
    }
}

void symm_all_constraint_type()
{
    int k,i,j;
    Fact *f;
    symm_num_stu = 0;
    symm_num_type =0;
     for ( i = 0; i < gnum_predicates; i++ ) {
           if ( !gis_added[i] && !gis_deleted[i] ) {
                if(strlen(gpredicates[i]) != MAX_PREDLEN) continue;
                for ( j = 0; j < gnum_initial_predicate[i]; j++ ) {
                   f = &(ginitial_predicate[i][j]) ;
                   symm_stu[symm_num_stu] = gconstants[(f->args)[0]];
                   symm_stu_far[symm_num_stu] =  gconstants[(f->args)[1]];
               //    printf("%s --> %s\n", symm_stu[j], symm_stu_far[j]);
                   symm_num_stu++;
                   
                   // collect type
                   for(k=0; k<symm_num_type; k++) {
                        if(strcmp(SymmType[k],  gconstants[(f->args)[1]]) == 0)
                            break;
                   } 
                   if(k== symm_num_type) {
                        SymmType[symm_num_type++] = gconstants[(f->args)[1]];
                   }
                }                   
           }
     }     
     symm_num_lo = 0;
     for ( i = 0; i < gnum_predicates; i++ ) {
           if ( !gis_added[i] && !gis_deleted[i] ) {
                if(strlen(gpredicates[i]) != MAX_PREDLEN+3) continue;
                for ( j = 0; j < gnum_initial_predicate[i]; j++ ) {
                   f = &(ginitial_predicate[i][j]) ;
                   symm_logo[symm_num_lo] = gconstants[(f->args)[0]];
                   symm_loko[symm_num_lo] =  gconstants[(f->args)[1]];
                //   printf("%s --> %s\n", symm_logo[j], symm_loko[j]);
                   symm_num_lo++;
                }                   
           }
     }     
}
    
char* symm_constraint_type(int f){
    int i;
    for(i=0; i<symm_num_stu; i++) {
        if(tok_ft_argncmp(f,0,symm_stu[i])==0)
           return symm_stu_far[i]; 
    }
    /*printf("exit 103\n");*/ exit(0);
}

void norm_lian_symm_groups(State *S, int sg, int dg, int* lian, int* len)
{   
    int i,f = -1,g;
    char *fur;
    Bool bol;
    
    fur = SymmGroupFar[sg][dg];
    
    for(i=0; i<S->num_F; i++) {
            f=S->F[i];
            if(gft_conn[f].num_add>=0) continue;
            if(tok_ft_argncmp(f,1,fur)==0) { 
              if(ft_predlen(f)==MAX_ARITY) { 
                break;
              }
            }
    }
    g = f;
    //printf("%d", ft_predlen(g));print_ft(g);
    (*len) = 0; lian[0] = g;
    while(1) {
         bol = FALSE;
         for(i=0; i<S->num_F; i++) {
            f=S->F[i];
            if(gft_conn[f].num_add>=0) continue;
            if(f==g) continue;        
            if(ft_argncmp(f, g,1,0)==0) {
      //          printf("%d", ft_predlen(f));print_ft(f);
                bol = TRUE;
                (*len)++;
                lian[(*len)] = f;
                break;               
            }
          }  
          if(!bol) break; 
          g = f;
    }
    (*len)++;
}

void collect_symm_op_type_con()
{
    int i,j;
    
    SymmOpTypeCon = (int*)calloc(gnum_ef_conn, sizeof(int));
    
    for(i=0; i<gnum_ef_conn; i++) {
        SymmOpTypeCon[i] = 0;
    }
   
    if(SymmCompart) {
        for(i=0; i<gnum_ef_conn; i++) {
            if(ef_predlen(i) == 7) {
                for(j=0; j<symm_num_stu; j++) {
                    if(tok_ef_argncmp(i,3,symm_stu[j])==0)
                        break;
                }
                if(tok_ef_argncmp(i,5,symm_stu_far[j])!=0) {
                 //   printf("%s %s",symm_stu[j], symm_stu_far[j]);
                 //   print_op(i);   
                    SymmOpTypeCon[i] = 1;        
                }
            }    
        }
    }
}

void collect_symm_group_rank()
{
    Fact *f;
    int i,j,k,kk;
    
    for(k=0; k<gnum_symm_group; k++) {
        for(kk=0; kk<symm_num_type; kk++) {
            SymmGroupType[k][kk] = 0;
        }
    }
    for(k=0; k<gnum_symm_group; k++) {
        for ( i = 0; i < gnum_predicates; i++ ) {
           if ( !gis_added[i] && !gis_deleted[i] ) {
                if(strlen(gpredicates[i]) != 26) continue;
                for ( j = 0; j < gnum_initial_predicate[i]; j++ ) {
                   f = &(ginitial_predicate[i][j]) ;
                   if(tok_ft_argncmp(SymmetryGroup[k].g[0],1, 
                               gconstants[(f->args)[2]]) != 0) continue;
                   for(kk=0; kk<symm_num_type; kk++) {
                     if(strcmp(SymmType[kk],gconstants[(f->args)[1]]) == 0) break; 
                   }
                   if(kk>=symm_num_type) {printf("exit 105"); exit(1);}
                   SymmGroupType[k][kk] ++;
                }                   
           }
        }     
    }
   
   /* 
    for(k=0; k<gnum_symm_group; k++) {
        printf("group %d\n",k);
        for(kk=0; kk<symm_num_type; kk++) {
            printf("\t%s ---> %d\n", SymmType[kk], SymmGroupType[k][kk]);
        }                       
    }*/
}
   
void compart_type_group()
{
    int i;
    for(i=gnum_symm_norm; i<gnum_symm_subnorm;i++) {
        
    }
}

void build_symmetry(State * start_state, int *rsa_facts, int *rsa_actions
        ,int *decr_g)
{
    int i, ii, j, k = -1, kk, f, x, f0;
    State M_all; 
    int symm_index = 0, num_allm_symm = 0, num_normal_symm = 0;

    symm_all_constraint_type();

    
    for(i=0; i<gnum_ft_conn;i++) {
        gft_conn[i].num_add = -1;
    }
    for(i=0; i<gnum_ft_conn; i++) {
        if((ft_predlen(i)==6)&&(ft_arity(i)==1)) { 
        //print_ft(i);
           gft_conn[i].num_add = symm_index;
           //ft_mutex_all(i, &M_all);
           sim_mutex_all(i, &M_all);
           ii=0;
           for(j=0; j<M_all.num_F; j++) {
               f = M_all.F[j];
               if(f!=i) {
                gft_conn[f].num_add = symm_index;
                Symmetry[symm_index].f[ii++] = f;
               }
           }   
            Symmetry[symm_index].f[ii] = i;
           Symmetry[symm_index].size = M_all.num_F;
           Symmetry[symm_index].type = 0;
           symm_index++;
        }
    }
    gnum_symm_norm = symm_index;
   
    SymmCompart = FALSE; 
    for(i=0; i<gnum_ft_conn; i++) {
        if(gft_conn[i].num_add >=0) continue;
        for(j=0; j<gnum_ft_conn; j++) {
           if(i==j) continue;
           if(gft_conn[j].num_add >=0) continue;
           if(compartment_ops(i,j)) {
                SymmCompart = TRUE;
                gft_conn[i].num_add = symm_index;
                gft_conn[j].num_add = symm_index;
                Symmetry[symm_index].f[0] = i;
                Symmetry[symm_index].f[1] = j;
                Symmetry[symm_index].size = 2;
                Symmetry[symm_index].type = 0;
                symm_index++;
                break;
           } 
        }
    }
    gnum_symm_subnorm = symm_index;
    num_allm_symm = symm_index;
   // printf("gnum_symm_norm = %d, gnum_symm_subnorm = %d\n", 
    //        gnum_symm_norm, gnum_symm_subnorm); 
   
   // construct symmetry 
    for(i=0; i<gnum_ft_conn;i++) {
        if(gft_conn[i].num_add>=0) continue;
        if(ft_predlen(i) == 2) {        
                ii=0;
                gft_conn[i].num_add = symm_index;
                Symmetry[symm_index].f[ii++] = i;
                for(j=0; j<gnum_ft_conn;j++) {
                    if(gft_conn[j].num_add>=0) continue;
                    if(ft_predlen(j) == 2) {        
                    if(ft_argncmp(i,j,0,0)==0) {
                        gft_conn[j].num_add = symm_index;
                        Symmetry[symm_index].f[ii++] = j;
                    }
                    }
                }
                Symmetry[symm_index].size = ii;
                Symmetry[symm_index].type = 1;
                symm_index++;
        }
    }

    
    gnum_symm = symm_index;
    //printf("total symmetry %d\n", gnum_symm);

    // adjust symmetry type
    //mutex_ft_info();
    spec_op_info();
    
    //print_op_info(); exit(0);
    
    //print_symm(); 
    // end of symmetry construction
    
    // construct symmetry group
    
    for(i=0; i<symm_index; i++) {
        if(Symmetry[i].type>0) {
            num_normal_symm ++;
        }
    }   
    
    gnum_symm_group = 0; 
    for(i=0; i<symm_index; i++) {
        if(Symmetry[i].type>0) {
            if(gnum_symm_group < Symmetry[i].size) {
                gnum_symm_group = Symmetry[i].size;
                k = i;
            }
        }
    }
    
            
    for(j=0; j<Symmetry[k].size; j++) {
        f = Symmetry[k].f[j];
        SymmetryGroup[j].size = 1;
        SymmetryGroup[j].g[0] = f;
    }
              
    x=k;
    for(j=0; j<symm_index; j++) {
        if(Symmetry[j].type == 0) continue;
        if(j==x) continue;
        for(k=0; k<Symmetry[j].size; k++) {
            f = Symmetry[j].f[k];
            for(kk=0; kk<gnum_symm_group; kk++) {
                f0 = SymmetryGroup[kk].g[0];    
                if(ft_argncmp(f,f0,1,1)==0) {
                    SymmetryGroup[kk].g[SymmetryGroup[kk].size] = f;
                    SymmetryGroup[kk].size++;
                    break;            
                } 
            }
            if(kk>=gnum_symm_group) {
                printf("Group not decided\n");
            }
        }        
    }
    
    //print_symm_group();
    for(i=0;i<gnum_symm;i++) {
        if(Symmetry[i].type==0) continue;
        for(j=0; j<Symmetry[i].size; j++) {
            if(find_symm_group(Symmetry[i].f[j])<0) {
                f = Symmetry[i].f[j];
      //          printf("undecided ");print_ft(f);
            }
        }   
    }
   // end of symmetry group construction
    
    //print_ft_info();
    
    //action_connection();
    for(i=0; i<gnum_symm_group; i++) {
        for(j=0; j<gnum_symm_group; j++) {
            SymmGroupNorm[i][j] = 0;    
        }                        
    }
        
    fact_connection(start_state);
    //print_symm_group_norm();
    if(SymmCompart) {collect_symm_group_rank();} 
    /* 
    for(i=0; i<gnum_symm_group; i++) {
        for(j=0; j<gnum_symm_group; j++) {
            if(SymmGroupNorm[i][j] > 0)  {  
            norm_lian_symm_groups(start_state, i, j, ng, &k);
            printf("%d %d %d\n",i,j, k);
            }
        }                        
    }   
    exit(0);
    */
   
    collect_symm_op_type_con();
    return;
}

void build_connect_symm_group()
{
   int i,j,k,f,g,kk;
   Bool bol;
   
   for(i=0; i<gnum_symm_group-1; i++) {
       SymmGroupConn[i][i] = FALSE; 
       for(j=i+1; j<gnum_symm_group; j++) {
            bol = FALSE; 
            for(k=0; k<SymmetryGroup[i].size; k++) {      
                f = SymmetryGroup[i].g[k];
                //for(kk=0; kk<SymmetryGroup[j].size; kk++) {
               for(kk=0; kk<3; kk++) {
                    g = SymmetryGroup[j].g[kk];
                // print_ft_name(f);printf(" ");print_ft_name(g); printf("\n");
                  //  if(act_connect_symmetry(f,g) || act_connect_symmetry(g,f)) {
                   if(act_connect_symmetry(f,g)) {
                        bol = TRUE;
                        break;
                    }        
                }       
                if(bol) break;
             }
             SymmGroupConn[i][j] = bol;     
             SymmGroupConn[j][i] = bol;     
        }
   }     
   
   for(i=0; i<gnum_symm_group; i++) {
       for(j=0; j<gnum_symm_group; j++) {
            printf("Group %d <--> Group %d [%d]\n", i,j, SymmGroupConn[i][j]);
       }
   }
}

void genIMA_analysis(State * start_state, int *rsa_facts, int *rsa_actions
        ,int *decr_g)
{           
    build_symmetry(start_state, rsa_facts, rsa_actions, decr_g);
//    build_connect_symm_group();
}
    
void lian_symm_groups(State *S, int fact, int* lian, int* pon, int* len)
{   
    int i,f = -1,g;
    Bool bol;
    
    if(contain_fact_state(S, fact)) {
        (*len) = 0;
        return;
    }

    for(i=0; i<S->num_F; i++) {
            f=S->F[i];
            if(gft_conn[f].num_add>=0) continue;
            if(ft_argncmp(f, fact,0,0)==0) { 
                break;
            }
    }
    g = f;
    while(ft_predlen(g)!=MAX_ARITY) {
         for(i=0; i<S->num_F; i++) {
            f=S->F[i];
            if(gft_conn[f].num_add>=0) continue;
            if(f==g) continue;        
            if(ft_predlen(g)==MAX_ARITY-1) {
                if(ft_argncmp(f, g,0,0)==0) { 
                    break;         
                }
            } else {
                if(ft_argncmp(f, g,0,1)==0) { 
                    break;         
                }
            }
          }   
          g = f;
    }
//    printf("%d", ft_predlen(g));print_ft(g);
    (*len) = 0; lian[0] = g;
    if(ft_argncmp(g,fact,0,0)==0) (*pon)=(*len);
    while(1) {
         bol = FALSE;
         for(i=0; i<S->num_F; i++) {
            f=S->F[i];
            if(gft_conn[f].num_add>=0) continue;
            if(f==g) continue;        
            if(ft_argncmp(f, g,1,0)==0) {
  //              printf("%d", ft_predlen(f));print_ft(f);
                bol = TRUE;
                (*len)++;
                lian[(*len)] = f;
                if(ft_argncmp(f,fact,0,0)==0) (*pon)=(*len);
                break;               
            }
          }  
          if(!bol) break; 
          g = f;
    }
    (*len)++;
}

void lian_endpoints(State *S, int goal, int *e1, int *e2)
{
    int i,a,j,s,d = -1,f, sg = -1,dg, pon, len;
    int fact = -1;
    int lian[MAX_LENGTH];
   
    lian_symm_groups(S, goal, lian, &pon, &len);
    /*    
    printf("lian ll %d lp %d\n",len, pon);
        for(i=0;i<len; i++) {
            printf("\t"); print_ft(lian[i]);
        }
        */
  if(len>1){ 
    f = lian[0];    
    for(i=0; i<gnum_symm_norm; i++) {
        fact = Symmetry[i].f[2];
        if(ft_argncmp(f,fact,1,0)==0) {
            break;
        }
    }  
    
    a = -1; s=-1; d=-1;
    for(i=0; i<gft_conn[fact].num_PC; i++) {
        a=gft_conn[fact].PC[i];
        if(gef_conn[a].spec == 2) {
            for(j=0; j<gef_conn[a].num_D; j++) {
                  f = gef_conn[a].D[j];
                  if(gft_conn[f].num_add >= gnum_symm_subnorm) {
                        if(s<0) {s=f; sg=find_symm_group(s); break; }
                        else {
                            if(sg != find_symm_group(f)) {
                                d=f;
                                dg = find_symm_group(d);
                                (*e1) = sg;
                                (*e2) = dg;
                                return;
                            }
                        }
                        break;
                  }
            }
        }
    }
  
  } else {
    a = -1;
    fact = lian[0];
    for(i=0; i<gft_conn[fact].num_PC; i++) {
        a=gft_conn[fact].PC[i];
        //printf("%d",gef_conn[a].spec); print_op(a);
        if(gef_conn[a].spec == 0) {
            for(j=0; j<gef_conn[a].num_PC; j++) {
                s = gef_conn[a].PC[j];
                if(gft_conn[s].num_add >= gnum_symm_subnorm) {
                    break;
                }
            }
            for(j=0; j<gef_conn[a].num_A; j++) {
                d = gef_conn[a].A[j];
                if(gft_conn[d].num_add >= gnum_symm_subnorm) {
                    break;
                }
            }
            break;
        }
    }
    (*e1) = find_symm_group(s);
    (*e2) = find_symm_group(d);
  }
}
        
int symm_goal_eval(State *dS, int goal)
{
    int i,j,g, ret = -1;
    int e1, e2, sf,sgroup = -1,dgroup, dsymm;
    State S;
    int lpar[MAX_TYPES], lu[MAX_TYPES], begin, end, curr;
    Bool bol;
    
    for(i=0; i<dS->num_F; i++) {
        S.F[i] = dS->F[i];
        if(goal == S.F[i]) return -1; 
    };
    S.num_F = dS->num_F;
   // printf("\ngoal");print_ft(goal);

    dgroup = find_symm_group(goal);
    dsymm = gft_conn[goal].num_add;
    sf = -1;
    
    for(i=0; i<Symmetry[dsymm].size; i++) {
        if(contain_fact_state(&S, Symmetry[dsymm].f[i])) {
            sf = Symmetry[dsymm].f[i];
            sgroup = find_symm_group(sf);    
        }
    }
    if(sf >= 0) {
        lu[0] = sgroup; lpar[0]=-1;
        begin = 0; end =0;
        curr = 1;
        bol = FALSE;
        while(1) {
          for(i=begin; i<=end; i++) {
            g = lu[i];
            for(j=0; j<gnum_symm_group; j++) {
                if(!in_array(lu,curr, j)) {
                    if(SymmGroupNorm[g][j]>0) {
                        lpar[curr] = i;
                        lu[curr++] = j;
                        if(j == dgroup) {
                            bol = TRUE;
                            break;
                        }
                    }
                } 
            }      
            if(bol) break;          
          }        
          if(bol) break;          
          begin = end+1;
          end = curr-1;
        }
       
        i=curr-1;
        while(lpar[i]>=0) { 
         //   printf("Group %d <---> ", lu[i]);
            ret = lu[i];
            i = lpar[i];
        }  
        //printf("Group %d\n", lu[i]);  
    } else {

        lian_endpoints(&S, goal, &e1, &e2); 
        bol = FALSE;
        lu[0] = e1; lpar[0]=-1; 
        if(e1==dgroup) { curr=1; bol = TRUE;} 
        else {
            lu[1] = e2; lpar[1]=-1; curr=2;
            if(e2==dgroup) bol=TRUE;
        }
        begin = 0; end =1; 
        while(!bol) {
          for(i=begin; i<=end; i++) {
            g = lu[i];
            for(j=0; j<gnum_symm_group; j++) {
                if(!in_array(lu,curr, j)) {
                    if(SymmGroupNorm[g][j]>0) {
                        lpar[curr] = i;
                        lu[curr++] = j;
                        if(j == dgroup) {
                            bol = TRUE;
                            break;
                        }
                    }
                } 
            }      
            if(bol) break;          
          }        
          if(bol) break;          
          begin = end+1;
          end = curr-1;
        }
       
        i=curr-1;
        // printf("X-->");
        while(lpar[i]>=0) { 
           // printf("Group %d <---> ", lu[i]);
            i = lpar[i];
        } 
        //printf("Group %d\n", lu[i]);  
        ret = lu[i];
    }
       
    return ret;
}   

void pro_print_state(dis_State *dS)
{
    int i;
    State S;
    for(i=0; i<dS->num_F; i++) {
        S.F[i] = dS->F[i]; 
    }
    S.num_F = dS->num_F;
    print_state(S);
    printf("\n");
}

Bool connected_connect(int fa, int go)
{
    int lf, lg;
    if(fa<0) return TRUE;
    if(go<0) return TRUE;
    lf = ef_predlen(fa);
    lg = ef_predlen(go);
    
    if(lf==10) {
        if((lg == 8) && (op_argncmp(fa,go,0,0)==0)) return TRUE;
        else return FALSE;
    }
    if(lf==9) {
        if((lg == 7) && (op_argncmp(fa,go,0,0)==0)) return TRUE;
        else return FALSE;
    }
    return TRUE;
}       

Bool symm_stuable(int f, int g)
{
   char *foo;
   char *goo;
   int i;
    
   
   foo = symm_constraint_type(f);
   goo = symm_constraint_type(g);
   for(i=0; i<symm_num_lo; i++) {
        if(strcmp(foo, symm_logo[i])==0) {
           if(strcmp(goo, symm_loko[i])==0) {
               return TRUE;
            }
        }
   }
   //printf("non stuable"); print_ft_name(f);
   //printf(" "); print_ft_name(g); printf("\n"); 
   return FALSE;
}

int symm_stu_connectable(State *S, int g, State *curr_state, int sgroup)
{   
    int i;
    int can, lg,l, no, nr, ng;
    
    lg = strlen(symm_constraint_type(g));
    no = nr = ng = 0;

  //  printf("%s ", symm_constraint_type(g));

    /*
    if(dis_ginfo==1) {
        for(i=0; i<curr_state->num_F; i++) {
            print_ft(curr_state->F[i]);
        }
    }*/
    
    for(i=0; i<S->num_F; i++) {
        if(S->F[i] == g) continue;
        
        //printf("%s ", symm_constraint_type(S->F[i]));
        
        l = strlen(symm_constraint_type(S->F[i]));
        switch(l) {
            case 4: no++; break;
            case 5: nr++; break;
            default: ng++; break;
        } 
    }

    can = 0;
    switch(lg) {
        case 4: if(ng>0) { can = no+nr+ng; }
                else { can = no; }
                break;
        case 5: if(ng>0) { can = no+nr+ng; }
                else { can = nr;}
                break;
        default: if(ng>0) { can =no+nr+ng;}
                 else {
                    if(no>nr) can = no;
                    else can = nr;    
                 }   
                 break; 
    }        
  //  printf("%d\n", can);
    
    return can;
}

int symmti(char *s)
{
    int i;
    for(i=0; i<symm_num_type; i++) {
        if(strcmp(SymmType[i], s) == 0) return i;
    }  
    printf("wroing symmti"); exit(1); 
    return -1;          
}

int symm_typist(int *lian, int ll, int lp, int sf, Bool revs, int dgroup,State *S)
{                                               
    int can,i;
    int foo;
    int tc[MAX_SYMM_TYPE];
    
    for(i=0; i<symm_num_type; i++) tc[i] = 0;
    for(i=0; i<S->num_F; i++) {
        foo = symmti(symm_constraint_type(S->F[i]));
        tc[foo]++;                                                        
      //  if(sf<0) {printf("%d",foo);print_ft(S->F[i]);}
    }                                               

    if(sf>=0) {
        for(i=0; i<ll; i++) {
             foo = symmti(symm_constraint_type(lian[i]));
             tc[foo]++;                                                        
          //  printf("\t%d",foo);print_ft(lian[i]);
        }
        foo = symmti(symm_constraint_type(sf));
        // printf("sf\t%d",foo);print_ft(sf);
        tc[foo]++;                                                        
    } else {
        if(revs) {
            for(i=lp; i>=0; i--) {
                foo = symmti(symm_constraint_type(lian[i]));
                tc[foo]++;                                                        
      //        printf(" \t%d",foo);print_ft(lian[i]);
            }
        } else {
            for(i=lp; i<ll; i++) {
                foo = symmti(symm_constraint_type(lian[i]));
                tc[foo]++;                                                        
        //    printf(" \t%d",foo);print_ft(lian[i]);
            }
        }
    }
    
    can = 0;
    for(i=0; i<symm_num_type; i++) {
        //printf("--------- %d %d %d\n", can, tc[i], SymmGroupType[dgroup][i]);
        if(tc[i] > SymmGroupType[dgroup][i]) {
            can = can + (tc[i] - SymmGroupType[dgroup][i]);
        }
    }
    return can; 
}

int symm_no_compart_constraint_eval(dis_State *dS, int goal)
{     
    int i,j,k,a,fact = -1 , lp,ll,nedx, s = -1, d = -1;
    int sf,srcx, sgroup,dgroup, dsymm, norm,fsymm;
    State S;
    int lian[MAX_LENGTH];
    int vio = 1000;
    const int VSMALL=10, VBASE=100, VMAX=1000;
    Bool revs;      
    int layer = 0;
    
    for(i=0; i<dS->num_F; i++) {
        S.F[i] = dS->F[i]; 
    };
    S.num_F = dS->num_F;
    
    //cannot violate satisfied constraints
    for(i=0; i<gsolved_state->num_F; i++) {
            if(!contain_fact_state(&S, gsolved_state->F[i])) return VMAX;
    }
    
    //goal reached
    for(i=0; i<S.num_F; i++) {
        if(goal == S.F[i]) return 0;
    };
    
  //  printf("\ngoal");print_ft(goal);

    dgroup = find_symm_group(goal);
    dsymm = gft_conn[goal].num_add;
  //        printf("dgroup = %d\n", dgroup);
  //          printf("dsymm = %d\n", dsymm);
    sf = -1;
    for(i=0; i<Symmetry[dsymm].size; i++) {
        if(contain_fact_state(&S, Symmetry[dsymm].f[i])) {
            sf = Symmetry[dsymm].f[i];
            sgroup = find_symm_group(sf);    
//            printf("sf"); print_ft(sf);
//           printf("sgroup = %d\n", sgroup);
            break;
        }
    }
    
    if(sf>=0) {
        norm = SymmGroupNorm[sgroup][dgroup];
        if(norm == 0) {
  //          printf("%d %d %d exit 101", norm, sgroup, dgroup); 
  //          exit(1);
            return VMAX;
        }
        srcx = 0;
        for(i=0; i<SymmetryGroup[sgroup].size; i++) {
            if(contain_fact_state(&S, SymmetryGroup[sgroup].g[i])) {
                if(!contain_fact_state(gsolved_state, SymmetryGroup[sgroup].g[i])) {
                    srcx++;
                }
            }
        } 
        
        if(srcx>norm+layer) {
            vio = (norm+1)*VSMALL + (VSMALL - srcx);
        } else {
            vio = (norm-srcx+1+layer)*VBASE; 
        }
        
//        printf("srcx %d norm %d vio %d\n", srcx, norm, vio);
    }
    
    if(sf<0) {
        lian_symm_groups(&S, goal, lian, &lp, &ll);
            /*
        printf("lian ll %d lp %d\n",ll, lp);
        for(i=0;i<ll; i++) {
            printf("\t"); print_ft(lian[i]);
        }
             */
   
       if(ll>1) { 
            for(i=0; i<gnum_symm_norm; i++) {
                fact = Symmetry[i].f[2];
                if(ft_argncmp(lian[0],fact,1,0)==0) {
                    break;
                }
            } 
            fsymm = gft_conn[fact].num_add;
            if(dgroup == SymmNormGl[fsymm]) {revs=TRUE; sgroup = SymmNormGr[fsymm];}
            else {
                if(dgroup == SymmNormGr[fsymm]) {
                    sgroup = SymmNormGl[fsymm]; 
                    revs=FALSE;
                } else {
                    return VMAX; 
                } 
            }
       } else {
            fact = lian[0];
            //printf("lian0");print_ft(fact);
            for(i=0; i<gft_conn[fact].num_PC; i++) {
                a=gft_conn[fact].PC[i];
                if(gef_conn[a].spec == 0) {
                    for(j=0; j<gef_conn[a].num_PC; j++) {
                        s = gef_conn[a].PC[j];
                        if(gft_conn[s].num_add >= gnum_symm_subnorm) {
                            break;
                        }
                    }
                    for(j=0; j<gef_conn[a].num_A; j++) {
                        d = gef_conn[a].A[j];
                        if(gft_conn[d].num_add >= gnum_symm_subnorm) {
                            break;
                        }
                    }
                    break;
                }
            }
            k = find_symm_group(s);        
            if(dgroup == k) { sgroup = find_symm_group(d); } 
            else {
                if(dgroup == find_symm_group(d)) {
                    sgroup=k;
                } else {
                    return VMAX;
                }
            }
            revs=FALSE;
       }
        srcx = 0;
        for(i=0; i<SymmetryGroup[sgroup].size; i++) {
            if(contain_fact_state(&S, SymmetryGroup[sgroup].g[i])) {
                if(!contain_fact_state(gsolved_state, SymmetryGroup[sgroup].g[i])) {
                    srcx++;
                }
            }
        } 
        
        if(revs) {
            nedx = lp+1;
        } else {
            nedx = ll-lp;
        }
        
        if(srcx>=nedx+layer) {
            vio = nedx*VSMALL + (VSMALL - srcx);
            //vio = (nedx+1)*VSMALL + (VSMALL - srcx);
        } else {
            vio = (nedx-srcx+layer)*VBASE+(VBASE-nedx); 
        }
//        printf("revs %d srcx %d nedx %d vio %d\n", revs, srcx, nedx, vio);
    }
    
    return vio;
}   

int symm_compart_constraint_eval(dis_State *dS, int goal)
{     
    int i,j,k,a,fact = -1, lp,ll,nedx,s = -1,d = -1;
    int sf,srcx, sgroup,dgroup, dsymm, norm,fsymm;
    State S, src_S, dst_S;
    int lian[MAX_LENGTH];
    int vio = 10000, dstx;
    const int VSMALL=10, VBASE=100, VMAX=10000, FBASE=100, TBASE=500;
    Bool revs;            
    int layer = 0;
    int face, stuconn, typconn;
    
    for(i=0; i<dS->num_F; i++) {
        S.F[i] = dS->F[i]; 
    };
    S.num_F = dS->num_F;
    
    //cannot violate satisfied constraints
    for(i=0; i<gsolved_state->num_F; i++) {
            if(!contain_fact_state(&S, gsolved_state->F[i])) return VMAX;
    }
    
    //goal reached
    for(i=0; i<S.num_F; i++) {
        if(goal == S.F[i]) return 0;
    };
    
  //  printf("\ngoal");print_ft(goal);

    dgroup = find_symm_group(goal);
    dsymm = gft_conn[goal].num_add;
  //        printf("dgroup = %d\n", dgroup);
  //          printf("dsymm = %d\n", dsymm);
     
    dstx = 0;  dst_S.num_F = 0;
    for(i=0; i<SymmetryGroup[dgroup].size; i++) {
        if(contain_fact_state(&S, SymmetryGroup[dgroup].g[i])) {
            dst_S.F[dst_S.num_F++] = SymmetryGroup[dgroup].g[i];
            if(!contain_fact_state(gsolved_state, SymmetryGroup[dgroup].g[i])) {
                dstx++;
            }
        }
    }
    //printf("dstx= %d\n", dstx);    
    
    sf = -1;
    for(i=0; i<Symmetry[dsymm].size; i++) {
        if(contain_fact_state(&S, Symmetry[dsymm].f[i])) {
            sf = Symmetry[dsymm].f[i];
            sgroup = find_symm_group(sf);    
//            printf("sf"); print_ft(sf);
//           printf("sgroup = %d\n", sgroup);
            break;
        }
    }
    
    if(sf>=0) {
        norm = SymmGroupNorm[sgroup][dgroup];
        if(norm == 0) {
  //          printf("%d %d %d exit 101", norm, sgroup, dgroup); 
  //          exit(1);
            return VMAX;
        }
        
        norm_lian_symm_groups(&S, sgroup, dgroup, lian, &ll);
        
        for(i=0; i<gnum_symm_norm; i++) {
            fact = Symmetry[i].f[2];
            if(ft_argncmp(lian[0],fact,1,0)==0) {
                    break;
            }    
        } 
        fsymm = gft_conn[fact].num_add;   
        if(dgroup == SymmNormGl[fsymm]) {revs=TRUE;}
        else {revs=FALSE;}
        
        //printf("revs %d ll %d", revs, ll); print_ft(lian[0]);
        if(revs) {                  
            if(symm_stuable(sf, lian[ll-1])) face =0;
            else face = 1;
        } else {
            if(symm_stuable(sf, lian[0])) face =0;
            else face = 1;                                        
        }        
        
        srcx = 0;
        src_S.num_F = 0;
        for(i=0; i<SymmetryGroup[sgroup].size; i++) {
            if(contain_fact_state(&S, SymmetryGroup[sgroup].g[i])) {
                if(!contain_fact_state(gsolved_state, SymmetryGroup[sgroup].g[i])) {
                    srcx++;
                    src_S.F[src_S.num_F++] = SymmetryGroup[sgroup].g[i];
                }
            }
        } 
        stuconn = symm_stu_connectable(&src_S, sf, &S, sgroup);
        typconn = symm_typist(lian, ll, -1, sf, revs, dgroup, &dst_S);
        
        /* 
        if(USE_STU) {
           if(typconn>0) {
                vio = typconn * TBASE;
           } else { 
            if(stuconn>=norm) {
                vio = (norm+1)*VSMALL + (VSMALL - srcx) 
                    + face*FBASE + typconn * TBASE;
            } else {        
                vio = (norm-stuconn)*VBASE + face*FBASE + typconn * TBASE; 
            }                                                                      
           }
        } else {
            if(srcx>norm+layer) {
                vio = (norm+1)*VSMALL + (VSMALL - srcx) + face*FBASE;
            } else {                                
                vio = (norm-srcx+1+layer)*VBASE + face*FBASE ; 
            }                                               
        }*/
       
        switch(SymmConstrEvalOption) {  
        case 0:  
        case 3:    
            if(srcx>norm+layer) {
                vio = (norm+1)*VSMALL + (VSMALL - srcx);             
            } else {    
                vio = (norm-srcx+1+layer)*VBASE ;
            }  
            break; 
        case 1:  
        case 4:
            if(stuconn>=norm) {
                vio = (norm+1)*VSMALL + (VSMALL - srcx) 
                    + face*FBASE + typconn * TBASE + typconn * TBASE;
            } else {        
                vio = (norm-stuconn)*VBASE + face*FBASE + typconn * TBASE; 
            }                                                                      
           break;
        case 2:
        case 5:     
            if(srcx>norm+layer) {
                vio = (norm+1)*VSMALL + (VSMALL - srcx) + face*FBASE;
            } else {                                
                vio = (norm-srcx+1+layer)*VBASE + face*FBASE ; 
            }
            break;            
        }
                              
        
        
    if(dis_ginfo==1)
        printf("stuconn %d norm %d vio %d\n", stuconn, norm, vio);
    }
    
    if(sf<0) {
        lian_symm_groups(&S, goal, lian, &lp, &ll);
            /*
        printf("lian ll %d lp %d\n",ll, lp);
        for(i=0;i<ll; i++) {
            printf("\t"); print_ft(lian[i]);
        }
             */
   
       if(ll>1) { 
            for(i=0; i<gnum_symm_norm; i++) {
                fact = Symmetry[i].f[2];
                if(ft_argncmp(lian[0],fact,1,0)==0) {
                    break;
                }
            } 
            fsymm = gft_conn[fact].num_add;
            if(dgroup == SymmNormGl[fsymm]) {revs=TRUE; sgroup = SymmNormGr[fsymm];}
            else {
                if(dgroup == SymmNormGr[fsymm]) {
                    sgroup = SymmNormGl[fsymm]; 
                    revs=FALSE;
                } else {
                    return VMAX; 
                } 
            }
       } else {
            fact = lian[0];
            //printf("lian0");print_ft(fact);
            for(i=0; i<gft_conn[fact].num_PC; i++) {
                a=gft_conn[fact].PC[i];
                if(gef_conn[a].spec == 0) {
                    for(j=0; j<gef_conn[a].num_PC; j++) {
                        s = gef_conn[a].PC[j];
                        if(gft_conn[s].num_add >= gnum_symm_subnorm) {
                            break;
                        }
                    }
                    for(j=0; j<gef_conn[a].num_A; j++) {
                        d = gef_conn[a].A[j];
                        if(gft_conn[d].num_add >= gnum_symm_subnorm) {
                            break;
                        }
                    }
                    break;
                }
            }
            k = find_symm_group(s);        
            if(dgroup == k) { sgroup = find_symm_group(d); } 
            else {
                if(dgroup == find_symm_group(d)) {
                    sgroup=k;
                } else {
                    return VMAX;
                }
            }
            revs=FALSE;
       }
        srcx = 0;
        src_S.num_F = 0;
        for(i=0; i<SymmetryGroup[sgroup].size; i++) {
            if(contain_fact_state(&S, SymmetryGroup[sgroup].g[i])) {
                if(!contain_fact_state(gsolved_state, SymmetryGroup[sgroup].g[i])) {
                    srcx++;
                    src_S.F[src_S.num_F++] = SymmetryGroup[sgroup].g[i];
                }
            }
        }
        if(revs) {  
            stuconn = symm_stu_connectable(&src_S, lian[ll-1], &S, sgroup);
        } else {                                        
            stuconn = symm_stu_connectable(&src_S, lian[0], &S, sgroup);
        }   
        typconn = symm_typist(lian, ll, lp, -1, revs, dgroup, &dst_S);
        
        if(revs) {
            nedx = lp+1;
        } else {
            nedx = ll-lp;
        }
        

        /*
        if(USE_STU) {
           if(typconn >0 ) {
                vio = typconn*TBASE;
           } else {
            if(stuconn>=nedx) {
                vio = nedx*VSMALL + (VSMALL - srcx) +typconn * TBASE;
            } else {
                vio = (nedx-stuconn)*VBASE+(VBASE-nedx)+typconn * TBASE; 
            }
           }
        } else {
            if(srcx>=nedx+layer) {
                vio = nedx*VSMALL + (VSMALL - srcx)  ;
            } else {
                vio = (nedx-srcx+layer)*VBASE+(VBASE-nedx) ; 
            }
        }*/

        switch(SymmConstrEvalOption) {  
        case 0:  
        case 3:    
            if(srcx>=nedx+layer) {
                vio = nedx*VSMALL + (VSMALL - srcx) ;
            } else {
                vio = (nedx-srcx+layer)*VBASE+(VBASE-nedx) ; 
            }  
            break;
        case 1:
        case 4:
           if(typconn >0 ) {
                vio = typconn*TBASE;
           } else {
            if(stuconn>=nedx) {
                vio = nedx*VSMALL + (VSMALL - srcx) +typconn * TBASE;
            } else {
                vio = (nedx-stuconn)*VBASE+(VBASE-nedx)+typconn * TBASE; 
            }
           }
           break;
        case 2:
        case 5:
            if(srcx>=nedx+layer) {
                vio = nedx*VSMALL + (VSMALL - srcx)  ;
            } else {
                vio = (nedx-srcx+layer)*VBASE+(VBASE-nedx) ; 
            }
           break; 
        }
        

    if(dis_ginfo==1)
        printf("revs %d srcx %d nedx %d vio %d\n", revs, srcx, nedx, vio);
    }
    
    return vio;
}   

int symm_constraint_eval(dis_State *dS, int goal)
{
    if(!SymmCompart) {
         return symm_no_compart_constraint_eval(dS, goal);
    } else {
         return symm_compart_constraint_eval(dS, goal);
    }
}
                
void symm_op_constrained(dis_State *S)
{
    int i, j;
    State T,l,wop;
    
    for(i=0; i<S->num_F; i++) {
        wop.F[i] = 0;
        l.F[i] = ef_predlen(S->F[i]);
        T.F[i] = S->F[i];
    }
    T.num_F = S->num_F;
    
    for(i=0; i<S->num_F; i++) {
        if(wop.F[i]!=0) continue;
        if((l.F[i] == 7) || (l.F[i]==8)) {
            wop.F[i]=1;
            for(j=0; j<S->num_F; j++)   {
                if(i==j) continue;
                if(wop.F[j]!=0) continue;
                if(l.F[i]!=l.F[j]) continue;
                if(op_argncmp(S->F[i], S->F[j],0,0)!=0) continue; 
                wop.F[j] = -1; 
            }
            continue;
        } 
        if((l.F[i]==15) || (l.F[i]==16)) {
            wop.F[i]=1;
            for(j=0; j<S->num_F; j++)   {
                if(i==j) continue;
                if(wop.F[j]!=0) continue;
                if(l.F[i]!=l.F[j]) continue;
                if(op_argncmp(S->F[i], S->F[j],0,0)!=0) continue; 
                if(op_argncmp(S->F[i], S->F[j],1,1)!=0) continue; 
                wop.F[j] = -1; 
            }
        }
    }
    
    S->num_F = 0;
    for(i=0; i<T.num_F; i++) {
        if(wop.F[i] >=0) {
            if(SymmConstrEvalOption <3) {
                S->F[S->num_F++] = T.F[i];
            } else { 
                if(SymmOpTypeCon[T.F[i]]==0)  S->F[S->num_F++] = T.F[i];
            }
        }    
    }
}

void IMA_tif_wrapper(State *start_state, State *end_state)
{
    int i,j;
    int f, wf = -1;
    Bool found;
    
    for(i=0; i<end_state->num_F-1; i++) {
        end_state->F[i] = end_state->F[i+1];                            
    }
    end_state->num_F--;
    for(i=0; i<saved_ggoal_state.num_F-1; i++) {
        saved_ggoal_state.F[i] = saved_ggoal_state.F[i+1];                            
    }
    saved_ggoal_state.num_F--;
   
    found = FALSE;
    for(i=0; i<gnum_ef_conn; i++) {
        if(ef_arity(i) == 0) {
            continue;
        }
        for(j=0; j<gef_conn[i].num_PC; j++) {
            f = gef_conn[i].PC[j];
            if(ft_arity(f)==0) {
                if(ft_predlen(f) == 15) {
                    found = TRUE;
                    wf = f;
                    break;     
                }
            }
        }
        if(found) break;
    }
  
    
    start_state->F[start_state->num_F++] = wf;
  
}
