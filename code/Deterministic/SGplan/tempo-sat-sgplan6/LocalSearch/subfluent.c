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

 * File: subfluent.c

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
#include "utilities.h"
#include "LpgOutput.h"
#include "output.h"
#include <unistd.h>
#include "esp.h" 
#include "ff.h"
#include "orderings.h"
#include "subspace.h"
#include "stripsff.h"
#include "dis_ff.h"
#include "dis_search.h"
#include "dis_relax.h"
#include "subfluent.h"
#include "dis_inst_final.h"

#define MAX_SHORT_SAT 10
#define MAX_SHORT_D 250

extern int dis_tok_ft_argncmp(int f, int n, char *tok);
extern int dis_ft_argncmp(int f, int g, int n, int m);
extern int dis_op_argncmp(int f, int g, int n, int m);
extern int dis_ft_arity(int f);
extern int dis_ef_predlen(int f);
extern int dis_ft_predlen(int f);

RelaxFlNode fl_node[MAX_FL_NODE];
int fl_num_node;
char *fl_resource[MAX_FL_RES];
int fl_num_res;
int fl_num_field;
char *fl_round[MAX_FL_ROUND];
int fl_num_round;
int gfl_used_round;

char* short_sat[MAX_SHORT_SAT];
float sat_sofar[MAX_SHORT_SAT];
Bool sat_calloff[MAX_SHORT_SAT];
char* sat_curr_inst[MAX_SHORT_SAT];
int short_num_sat;
char* short_d[MAX_SHORT_D];
int short_num_d;
float short_d_v[MAX_SHORT_D][MAX_SHORT_D];
float shortest_d[MAX_SHORT_D];
int visit_d[MAX_SHORT_D];
int parent_d[MAX_SHORT_D];
int short_ts;
int short_rd;
int short_msof;
int short_mi;
int short_ctgt;
char* short_CA;
int avail_sat[MAX_SHORT_SAT];
char* avail_inst[MAX_SHORT_SAT];  
char *select_inst, *select_sat;

dis_TimedInitial short_c_tils[500];

int flni(char *s)
{
    int i;
    for(i=0; i<fl_num_node; i++) {
        if(strcmp(fl_node[i].s, s)==0) return i;
    }
    printf("exit 106\n"); exit(1);
}

void fl_all_constriant_type()
{
	int i,j,mo,n;
    dis_Fact *f;
    char *tok;
    fl_num_res = 0;
    fl_num_node = 0;
    fl_num_round = 0;
    mo =0;
               
    i=0;
    
    for ( j = dis_gnum_initial_predicate[i]-1; j>=0; j-- ) {
        f = &(dis_ginitial_predicate[i][j]) ;
        tok = dis_gconstants[(f->args)[0]];
        if(mo==0) {
            if(strlen(tok)>=MAX_PREDLEN-1) {
                mo=1;
            }
            else {
                fl_resource[fl_num_res++] = tok;
            //    printf("res %s\n", tok);
            }
        } 
        if(mo == 1) {
            if(strlen(tok)<MAX_PREDLEN-1) mo=2;
            else {          
                fl_node[fl_num_node++].s = tok;
       //         printf("node %s\n", tok);
            }               
        }        
        if(mo==2) {
        //    printf("round %d %s\n", fl_num_round, tok);
            fl_round[fl_num_round++] = tok;                        
        }
    }                       
   
    fl_num_field = 3;
   
    for(i=0; i<fl_num_node; i++) {
        for(j=0;j<fl_num_field; j++) {
            fl_node[i].field[j] = 0;
        }
    }
    
    for ( i = 15; i < 15+fl_num_field; i++ ) {
           if ( !dis_gis_added[i] && !dis_gis_deleted[i] ) {
                for ( j = dis_gnum_initial_predicate[i]-1; j>=0; j-- ) {
                   f = &(dis_ginitial_predicate[i][j]) ;
                   tok = dis_gconstants[(f->args)[0]];
                   n = flni(tok);
          //         printf("field %d %d %s\n", i, n, tok);
                   fl_node[n].field[i-15] = 1;
                }                   
           }
    }     
}

int fl_node_resource(int nd, int res)
{               
//    /*
    const int sd[6] = {4, 5, 0, 2, 1, 3};
    int k;
    k = (fl_num_node - nd-1);
    return 7*k+sd[res];    
//    */
    
//    const int sd[6] = {2, 1, 6, 4, 5, 3};
//    return 7*nd+sd[res]+3;
}                                   

void dis_collect_fluent()
{      
    /* 
    for(i=0; i<dis_gnum_fl_conn; i++ ) {
        printf("\n\nFL: ");      
        dis_print_fl_name( i );
        printf("\n");
    }*/
    
    fl_all_constriant_type();
 
   /* 
    for(i=0; i<gnum_ef_conn; i++) {
        dis_print_op_name(i); printf("\n");
    }
    */
   
   /* 
    for(i=0; i<fl_num_node; i++) {
        for(j=0; j<fl_num_res; j++) {
            printf("nd %d res %d ", i, j); 
            dis_print_fl_name(fl_node_resource(i,j)); printf("\n");
        }               
    }
    */
}

int ft_node_round(int nd, int r)
{
    int i;
    for(i=0; i<dis_gnum_ft_conn; i++) {
        if(dis_ft_predlen(i) == 5) {
            if(dis_tok_ft_argncmp(i, 1, fl_node[nd].s)==0) {
                if(dis_tok_ft_argncmp(i, 0, fl_round[r])==0) {
                        return i;       
                }       
            }
        }
    }
    return -1;
}
int ft_node_attri(int r)
{
    int i;
    for(i=0; i<dis_gnum_ft_conn; i++) {
        if(dis_ft_predlen(i) == 7) {
                if(dis_tok_ft_argncmp(i, 0, fl_round[r])==0) {
                        return i;       
                }       
        }
    }
	return -1;
}

void relax_1_easy_resource(State* S)
{   
    const int fr[3] = {5, 3, 0}; 
    int i,j;
   
    S->num_F = 0; 
    for(i=0; i<fl_num_node; i++) {
        for(j=1; j<fl_num_field; j++) {   
            if(fl_node[i].field[j] ==1) {
            //    dis_print_fl_name(fl_node_resource(i,fr[j])); printf(" 1 easy\n");
                S->F[S->num_F++] = fl_node_resource(i,fr[j]);
            }
        }  
    }
}

void relax_1_easy_predicate(State* S)
{   
    int i;
   
    S->num_F = 0;
    gfl_used_round = 0;
    
    for(i=0; i<fl_num_node; i++) {
        if(fl_node[i].field[2] ==1) {
            //dis_print_ft_name(ft_node_round(i,gfl_used_round)); printf(" 1 easy\n");
            //dis_print_ft_name(ft_node_attri(gfl_used_round)); printf(" 1 easy\n");
            S->F[S->num_F++] = ft_node_round(i,gfl_used_round);
            S->F[S->num_F++] = ft_node_attri(gfl_used_round);
            gfl_used_round++;                   
            if(gfl_used_round>=fl_num_round) break;
        }           
    }
}

void relax_2_easy_resource(State* S)
{   
    int i;
   
    S->num_F = 0; 
    for(i=0; i<fl_num_node; i++) {
        if(fl_node[i].field[2] == 1) {
        //    dis_print_fl_name(fl_node_resource(i,1)); printf(" 2 easy\n");
        //    dis_print_fl_name(fl_node_resource(i,2)); printf(" 2 easy\n");
            S->F[S->num_F++] = fl_node_resource(i,1);
            S->F[S->num_F++] = fl_node_resource(i,2);
        }
    }
}    

void relax_1_hard_resource(State* S)
{   
    int i;
   
    S->num_F = 0; 
    for(i=0; i<fl_num_node; i++) {
        if(fl_node[i].field[2]==0) {
        //    dis_print_fl_name(fl_node_resource(i,0)); printf(" 1 hard\n");
            S->F[S->num_F++] = fl_node_resource(i,0);
        }
    }
}    

void relax_2_hard_resource(State* S)
{   
    int i;
   
    S->num_F = 0; 
    for(i=0; i<fl_num_node; i++) {
        if(fl_node[i].field[2]==0) {
        //    dis_print_fl_name(fl_node_resource(i,1)); printf(" 2 hard\n");
            S->F[S->num_F++] = fl_node_resource(i,1);           
        }
    }
}    

void relax_3_hard_resource(State* S)
{                
    int i;
   
    S->num_F = 0; 
    for(i=0; i<fl_num_node; i++) {
        if(fl_node[i].field[1]==0) {
        //    dis_print_fl_name(fl_node_resource(i,3)); printf(" 3 hard\n");
            S->F[S->num_F++] = fl_node_resource(i,3);           
        }                                           
    }
}    

Bool fl_is_exceeding()
{
    int i, n=0;
    if(fl_num_node>=15) {
        for(i=0; i<fl_num_node; i++) {
                if(fl_node[i].field[0] ==1) {
                    n++;
                }
        }
        if(n<=1) return TRUE;
    }
    
    return FALSE;
}
    
void set_fluent_hash(FluentHash* fh, int size, char *key, float value)
{
    int i;
    for(i=0; i<size; i++) {
        if(strcmp(fh[i].key, key) == 0) {
            fh[i].value = value;
            return;
        }
    }
    printf("exit 114"); exit(1);
}
int insert_fluent_hash(FluentHash* fh, int size, char *key, float value)
{
    int i;
    for(i=0; i<size; i++) {
        if(strcmp(fh[i].key, key) == 0) return size;
    }
    fh[size].key = key;
    fh[size].value = value;
    return (size+1);
}

float key_fluent_hash(FluentHash* fh, int size, char *key)
{
    int i;
            
    for(i=0; i<size; i++) {
        if(strcmp(fh[i].key, key) == 0) {
            return fh[i].value;
        }
    }                                   
    printf("exit 113"); exit(1);
}
                        
int insert_int_hash(IntHash* fh, int size, int key, float value)
{                                                   
    int i;
    for(i=0; i<size; i++) {
        if(fh[i].key == key) return size;
    }                       
    fh[size].key = key;
    fh[size].value = value;
    return (size+1);
}

void sort_int_hash(IntHash* fh, int size)
{
    int i,j;
    IntHash x;
    
    for(i=0; i<size-1;i++) {
        for(j=i+1; j<size; j++) {
            if(fh[i].value > fh[j].value) {
                x = fh[i];
                fh[i] = fh[j];
                fh[j] = x;
            }
        }
    }
}


int short_find_sat(char *s)
{
    int i;
    for(i=0; i<short_num_sat; i++) {
        if(strcmp(short_sat[i], s) == 0) return i;
    }
    return -1;
}
int short_find_d(char *s)
{
    int i;
    for(i=0; i<short_num_d; i++) {
        if(strcmp(short_d[i], s) == 0) return i;
    }
    return -1;
}

void collect_for_shortest()
{
    int i,j,ff, ai, bi;
    int sl = -1;
    dis_Fact* f; 
    dis_Fluent *fl;
    char *a, *b;
    float v;
    
    short_num_sat = 0;
    short_num_d = 0;
   
    
    for ( i = 0; i < dis_gnum_predicates; i++ ) {
        if(strlen(dis_gpredicates[i]) != MAX_PREDLEN+1) continue;      
        for ( j = 0; j < dis_gnum_initial_predicate[i]; j++ ) {
            // f: power_avail satellite
            f = &(dis_ginitial_predicate[i][j]) ;
            short_sat[short_num_sat++] = dis_gconstants[(f->args)[0]]; 
        }
    }
    
    for(i=0; i<short_num_sat; i++) {
        sat_sofar[i] = 0.0;
        sat_calloff[i] = FALSE;
    }
    
    for(i=0; i<MAX_SHORT_D; i++) { 
        for(j=0; j<MAX_SHORT_D; j++) {
           short_d_v[i][j] = -1.0; 
        }
    }

     for ( i =  dis_gnum_functions-1; i>=0; i-- ) {
        if ( !dis_gis_changed[i] ) {
             if(strlen(dis_gfunctions[i]) == MAX_PREDLEN-1) {
                sl = i;
                break;
             }
        }   
     }
        
    for(i=0; i<dis_gnum_initial_function[sl]; i++) {
        // fl: slew_time direction direction
        fl = &( dis_ginitial_function[sl][i].fluent);
        ff = fl->function;
        a = dis_gconstants[(fl->args)[0]];
        b = dis_gconstants[(fl->args)[1]];
        v = dis_ginitial_function[sl][i].value;
        if(short_find_d(a)<0) { short_d[short_num_d++] = a; }
        if(short_find_d(b)<0) { short_d[short_num_d++] = b; }
        ai = short_find_d(a); 
        bi = short_find_d(b);
        short_d_v[ai][bi] = v;
        //printf("%d %s %d %s %f\n",ai,a,bi,b,v);
    }  
   
    for ( i = 0; i < dis_gnum_predicates; i++ ) {
        if(strlen(dis_gpredicates[i]) != MAX_PREDLEN-2) continue;
        if(dis_gnum_initial_predicate[i] <=0) continue; 
        f = &(dis_ginitial_predicate[i][0]) ;
        a = dis_gconstants[(f->args)[0]]; 
        b = dis_gconstants[(f->args)[1]];
        if(short_find_sat(a)>=0) continue;
        // short_ts: support, short_rd: on_board
        if(short_find_sat(b)<0) {short_ts=i;}
        else {short_rd=i;}
    }
    for ( i = 0; i < dis_gnum_predicates; i++ ) {
        if(strlen(dis_gpredicates[i]) >= 18) {
            // short_ctgt: calibration_target
            short_ctgt = i;
        }
    }
    
    for(i=0; i<short_num_sat; i++) {
        sat_curr_inst[i] = NULL;
    }
}                   
    
float find_shortest(char *src, char *dest, char* sat, int *spath, int* num_spath)
{
    int i,j,f, p;    
    int si;
    int begin, end, pd;
    char *a1;
    char *a2;    
    float total=0.0;
    
    if(strcmp(src, dest)==0) {
        (*num_spath) = 0;
        return 0.0;
    }
   
    for(i=0; i<short_num_d; i++) shortest_d[i] = -1;
    si = short_find_d(src);
    shortest_d[si] = 0;
    
    begin = 0; end=0;pd=1;
    visit_d[0] = si;
    parent_d[si] = -1;
    while(begin<=end) {
        for(i=begin; i<=end; i++) {
            f = visit_d[i];
            for(j=0; j<short_num_d; j++) {
                if(short_d_v[f][j] < 0) continue;
                if(shortest_d[j] < 0) {
                    shortest_d[j] = shortest_d[f] + short_d_v[f][j];
                    visit_d[pd++] = j;
                    parent_d[j] = f;
                } else {
                    if(shortest_d[f] + short_d_v[f][j] < shortest_d[j]) {
                        shortest_d[j] = shortest_d[f] + short_d_v[f][j];
                        parent_d[j] = f;
                    }
                }   
            }        
        }
        begin = end+1;
        end = pd - 1;
    }
    
    i = short_find_d(dest);
    a1 = short_d[i];
    p=i;
    i = parent_d[i];
    (*num_spath)=0;
    while(i>=0) {
        a2 = short_d[i];
        //printf("%s %s\n",a1, a2);
            
        total += short_d_v[i][p];
        
        for(j=0; j<dis_gnum_op_conn;j++) {
            if(dis_ef_predlen(j) != 7) continue;
            if(dis_tok_ef_argncmp(j, 0, sat)!=0) continue;
            if(dis_tok_ef_argncmp(j, 1, a1)!=0) continue;
            if(dis_tok_ef_argncmp(j, 2, a2)!=0) continue;
            //dis_print_op_name(j); printf("\n");
            spath[(*num_spath)++] = j;
            break;
        }
        
        a1 = a2;
        p=i;
        i = parent_d[i];
    }
   // printf("\ntotal = %f\n", total);
    return total;
}

void dis_delete_gplan_ops(int pos)
{                               
    int j;
    if(pos>=dis_gnum_plan_ops) {
        printf("\nexit 110\n"); exit(1);
    }
    for(j=pos+1; j<dis_gnum_plan_ops; j++) {
        dis_gplan_ops[j-1] = dis_gplan_ops[j];
    }
    dis_gnum_plan_ops--;
}

void dis_insert_gplan_ops(int pos, int x)
{                               
    int j;
    if(pos>dis_gnum_plan_ops) {
        printf("\nexit 111\n"); exit(1);
    }
    for(j=dis_gnum_plan_ops; j>pos; j--) {
        dis_gplan_ops[j] = dis_gplan_ops[j-1];
    }
    dis_gplan_ops[pos] = x;
    dis_gnum_plan_ops++;
}



void short_analyze_subpath()
{
    float sof;
    int i,j,k,f,a;
    char *sat, *dest, *src = NULL, *iga;
    Bool has_IGA = FALSE;
    int spath[100];
    int num_spath;
    int sat_n;
    
    /*
    for(i=0; i<dis_gnum_plan_ops; i++) {
        a = dis_gplan_ops[i];
        dis_print_op_name(a); printf("\n");
    }
    */
 
    
    // Get dest d/s
    a  = dis_gplan_ops[dis_gnum_plan_ops-1];
    // sat: satellite
    sat = dis_ef_argn(a,0);
    // dest: direction
    dest = dis_ef_argn(a,1);
//    fprintf(stderr, "sat=%s dest=%s\n", sat, dest);
    sat_n = short_find_sat(sat);
    
    // clean up
    for(i=0; i<dis_gnum_plan_ops; i++) {
        a = dis_gplan_ops[i];
        // ignore switch_on, calibrate
        if((dis_ef_predlen(a) == 9) && (dis_ef_arity(a)==2)) continue;
        // ignore switch_off, take_image
        if((dis_ef_predlen(a) == 10) && (dis_ef_arity(a)==2)) continue;
        if(strcmp(sat, dis_ef_argn(a,0)) !=0 ) {
            // printf("deleting %s %s ", dis_ef_argn(a,0), sat);dis_print_op_name(a); printf("\n");
            for(j=i+1; j<dis_gnum_plan_ops; j++) {
                dis_gplan_ops[j-1] = dis_gplan_ops[j];
            }
            dis_gnum_plan_ops--;
            i--;
        }            
    }
    
    // Get source d
    for(i=0; i<dis_ginitial_state.num_F; i++) {
        f = dis_ginitial_state.F[i];
        //dis_print_ft_name(f); printf(" %d\n", dis_ft_arity(f));
        if((dis_ft_predlen(f) == 8) && (dis_ft_arity(f)==2)) {
            if(strcmp(sat, dis_ft_argn(f, 0))==0) {
            // src: direction
                src = dis_ft_argn(f, 1);
                break;
            }    
        }
    }     
    //printf("src=%s\n",  src);
   
    // Any IGA
    for(i=0; i<dis_gnum_plan_ops; i++) {
        a = dis_gplan_ops[i];
        // a: calibrate
        if((dis_ef_predlen(a) == 9) && (dis_ef_arity(a)==3)) {
            has_IGA = TRUE;
            // iga: direction
            iga = dis_ef_argn(a, 2);
            break;
        }
    }
    
    if(has_IGA) {
   //     printf("iga=%s\n", iga);
        sof = find_shortest( src, iga, sat, spath, &num_spath);
        if(num_spath>0) {
                for(i=0; i<dis_gnum_plan_ops; i++) {
                    a = dis_gplan_ops[i];
                    if((dis_ef_predlen(a) == 9) && (dis_ef_arity(a)==3)) break;  
                    if((dis_ef_predlen(a) == 7) && (dis_ef_arity(a)==3)) {
     //                   printf("to delete "); dis_print_op_name(a); printf("\n");
                        dis_delete_gplan_ops(i);
                        i--;                    
                    }
                }               
                for(i=0; i<num_spath; i++) {
           //       printf("iga injecting "); dis_print_op_name(spath[i]); printf("\n");
                  dis_insert_gplan_ops(0,spath[i]); 
                } 
                sat_sofar[sat_n] += sof;   
        } 
         
        sof = find_shortest( iga, dest, sat, spath, &num_spath);
        if(num_spath>0) {
            for(i=0; i<dis_gnum_plan_ops; i++) {
                a = dis_gplan_ops[i];
                // calibrate
                if((dis_ef_predlen(a) == 9) && (dis_ef_arity(a)==3)) break; 
            } k=i;
            for(i=k+1; i<dis_gnum_plan_ops; i++) {
                    a = dis_gplan_ops[i];
                    // turn_to
                    if((dis_ef_predlen(a) == 7) && (dis_ef_arity(a)==3)) {
                   //     printf("to delete "); dis_print_op_name(a); printf("\n");
                        dis_delete_gplan_ops(i);
                        i--;                    
                    } 
            }                
            for(i=0; i<num_spath; i++) {
             //       printf("dst injeting "); dis_print_op_name(spath[i]); printf("\n");
                    dis_insert_gplan_ops(k+1,spath[i]);    
            }
            sat_sofar[sat_n] += sof;
        }
    } else {
            sof = find_shortest( src, dest, sat, spath, &num_spath);
            if(num_spath>0) {
                for(i=0; i<dis_gnum_plan_ops; i++) {
                    a = dis_gplan_ops[i];
                    if((dis_ef_predlen(a) == 7) && (dis_ef_arity(a)==3)) {
              //          printf("to delete "); dis_print_op_name(a); printf("\n");
                        dis_delete_gplan_ops(i);
                        i--;                    
                    }                 
                }               
                for(i=0; i<num_spath; i++) {
                //    printf("injecting "); dis_print_op_name(spath[i]); printf("\n");
                    dis_insert_gplan_ops(0,spath[i]);    
                }
                sat_sofar[sat_n] += sof;
            }
    }

/*    
     for(i=0; i<short_num_sat; i++) {
         printf("%s %f\n", short_sat[i], sat_sofar[i]);
     }
*/  
}

void short_prune_subgoals()
{
    int i,j,k,f,kk,g;
    Bool found;
    float time;
    
    if(GpG.SearchModal == 106) {
        for(i=0; i<dis_gnum_flogic_goal; i++) {
            if((dis_ft_predlen(saved_dis_gflogic_goal[i])==5)
                   && (dis_ft_arity(saved_dis_gflogic_goal[i])==0))
                    break;
        }                
        k = i;  
                                                            
        for(i=k; i<dis_gnum_flogic_goal-1; i++) {
            saved_dis_gflogic_goal[i] = saved_dis_gflogic_goal[i+1];
            dis_gflogic_goal[i] = dis_gflogic_goal[i+1];
        }   
        dis_gnum_flogic_goal--;
        saved_dis_gnum_flogic_goal--;
       
        i=dis_gnum_ef_conn-1;
        for(j=0; j<dis_gef_conn[i].num_A; j++) {
            f = dis_gef_conn[i].A[j];
           // dis_print_ft_name(f); printf("\n");
            dis_ginitial_state.F[dis_ginitial_state.num_F++] = f;
        }
       
       /* 
        j=0;
        for(i=dis_gnum_ef_conn-2; i>=0; i--) {
            if(dis_ef_arity(i)!=0) break;
            j++;
        }
        dis_gtils = (dis_TimedInitial *)calloc(2*j, sizeof(dis_TimedInitial));
         */
     
        dis_gtils = short_c_tils;   
        dis_gnum_tils = 0;
        for(i=dis_gnum_ef_conn-2; i>=0; i--) {
            if(dis_ef_arity(i)!=0) break;
            if(dis_gef_conn[i].num_A > 1) {
                for(j=0; j<dis_gef_conn[i].num_A; j++) {
                    f = dis_gef_conn[i].A[j];
                    if(dis_ft_arity(f)==0) continue;
                    found = FALSE;
                    for(k=dis_gnum_ef_conn-2; k>=0; k--) {
                        if(dis_ef_arity(k)!=0) break;
                        if(dis_gef_conn[k].num_D > 1) {
                            for(kk=0; kk<dis_gef_conn[k].num_D; kk++) {
                                g = dis_gef_conn[k].D[kk];
                                if(f==g) {
                                    found = TRUE;
                                    break;
                                }
                            }
                        }
                        if(found) break;
                    }
                    
                    if(!found) {
                        printf("exit 117"); exit(1);
                    }
                     
                    dis_gtils[dis_gnum_tils].pred = dis_grelevant_facts[f];
                    dis_gtils[dis_gnum_tils].negated = 0;
                    time = 0.0;
                    for(kk=dis_gnum_ef_conn-2; kk>=0; kk--) {
                        time += dis_gef_conn[kk].duration;
                        if(kk==i) break;
                    }
                    dis_gtils[dis_gnum_tils].time = time;
                    dis_gnum_tils++;
                    
                    dis_gtils[dis_gnum_tils].pred = dis_grelevant_facts[f];
                    dis_gtils[dis_gnum_tils].negated = 1;
                    time = 0.0;
                    for(kk=dis_gnum_ef_conn-2; kk>=0; kk--) {
                        time += dis_gef_conn[kk].duration;
                        if(kk==k) break;
                    }
                    dis_gtils[dis_gnum_tils].time = time;
                    dis_gnum_tils++;
                }
            }
        }
    }

    for(i=0; i<dis_gnum_flogic_goal; i++) {
        // sent_image
        if(dis_ft_predlen(saved_dis_gflogic_goal[i])!=MAX_PREDLEN) continue;
        for(j=0; j<dis_gnum_ft_conn; j++) {
            if(dis_ft_predlen(j)!=MAX_PREDLEN) continue;  
            if(j==saved_dis_gflogic_goal[i]) continue;
            if(dis_ft_argncmp(j, saved_dis_gflogic_goal[i],0,0)!=0) continue;
            if(dis_ft_argncmp(j, saved_dis_gflogic_goal[i],1,1)!=0) continue;
            // have_image
            saved_dis_gflogic_goal[i] = j;
            break;
        }                                   
    }

    /*    
    for(i=0; i<dis_gnum_tils; i++) {
        dis_print_dis_Fact(&(dis_gtils[i].pred));
        printf(" %d %d\n", dis_gtils[i].time, dis_gtils[i].negated);
    } */                      
}

float short_tif_tip(int o)
{                   
    int i, sl = -1;
    dis_Fluent *fl;
    char *a, *b;
    
    for ( i=0; i< dis_gnum_functions; i++ ) {
        if ( !dis_gis_changed[i] ) {
            // i: send_time
             if(strlen(dis_gfunctions[i]) == MAX_PREDLEN-1) {
                sl = i;
                break;
             }
        }   
     }
    for(i=0; i<dis_gnum_initial_function[sl]; i++) {
        fl = &( dis_ginitial_function[sl][i].fluent);
        a = dis_gconstants[(fl->args)[0]];
        b = dis_gconstants[(fl->args)[1]];
        // direction
        if(dis_tok_ef_argncmp(o, 1, a) !=0) continue;
        // mode
        if(dis_tok_ef_argncmp(o, 3, b) !=0) continue;
        return dis_ginitial_function[sl][i].value;
    }                           
	return 0;
}
void short_tif()
{
    int min_ti, i,j,k,a;
    dis_Fact* f;
    FluentHash node[2*MAX_FL_NODE];
    int num_node=0;
    IntHash subs[2*MAX_FL_NODE];
    int num_subs=0;
    float t1, t2, tip, tin, dessert;
    float min_drt, TIP_DELTA=2.5;
    
    for(i=0; i<dis_gnum_tils; i++) {
        f = &(dis_gtils[i].pred);
  //      printf("%s", dis_gconstants[(f->args)[0]]);
  //      printf(" %d %d\n", dis_gtils[i].time, dis_gtils[i].negated);
        num_node = insert_fluent_hash(node, num_node, 
                dis_gconstants[(f->args)[0]], (float)dis_gtils[i].time); 
    }                                           

    for(i=0;i<num_node;i++) {
  //      printf("%s %f\n", node[i].key, node[i].value);
    }
    
    for(i=0; i<dis_gnum_plan_ops; i++) {
        a = dis_gplan_ops[i];
        if(dis_ef_predlen(a)!=MAX_PREDLEN) continue;
        if(dis_ef_arity(a) != 4) continue;
        // a: take_image
        num_subs = insert_int_hash(subs, num_subs, a, GpG.pert_endtime[i]);
    } 
    sort_int_hash(subs, num_subs);
    
    for(j=0;j<num_subs;j++) {
        tin = subs[j].value; 
        tip = short_tif_tip(subs[j].key);
    
   
        min_drt = 10000.0; min_ti = -1;  
        for(i=0; i<dis_gnum_tils; i++) {
            f = &(dis_gtils[i].pred);
            // start time
            t1 = dis_gtils[i].time;
            i++;
            // end time
            t2 = dis_gtils[i].time;
            
            
            if(tin+tip+TIP_DELTA>=t2) continue;
            
            dessert = key_fluent_hash(node, num_node,dis_gconstants[(f->args)[0]]);
            if(dessert >=  t2) continue;
            if(dessert < t1) dessert = t1;
            if(tin<dessert) { 
                if(dessert+tip+TIP_DELTA>=t2) continue;
            }
            
            if(dessert<min_drt) {
                min_drt = dessert;
                min_ti = i-1;
            }
        }                      
        if(min_ti <0) {
       //     printf("not killed ");dis_print_op_name(subs[j].key);
       //     printf(" %f (%f %f)\n",subs[j].value, tin, tin+tip);
          //  continue;
         //   printf("\nscheduling failed\n");
            exit(0);                    
        }                               
	    
        f = &(dis_gtils[min_ti].pred);
        t1 = dis_gtils[min_ti].time;
        min_ti++;
        t2 = dis_gtils[min_ti].time;
        dessert = key_fluent_hash(node, num_node,dis_gconstants[(f->args)[0]]);
        if(dessert >=  t2) {printf("wrong"); exit(1);}
        if(dessert < t1) dessert = t1;
        if(tin<dessert) { tin = dessert; }
        if(tin+tip+TIP_DELTA>=t2) {printf("wrong"); exit(1);}
        dessert = tin+tip;    
        set_fluent_hash(node, num_node, dis_gconstants[(f->args)[0]],dessert);
            
        for(k=0; k<dis_gnum_ef_conn; k++) {
            if(dis_ef_arity(k) != 4) continue;
            if(dis_tok_ef_argncmp(k, 0, dis_gconstants[(f->args)[1]])!=0) continue;
            if(dis_tok_ef_argncmp(k, 1, dis_gconstants[(f->args)[0]])!=0) continue;
            if(dis_op_argncmp(k, subs[j].key, 2, 1)!=0) continue;    
            if(dis_op_argncmp(k, subs[j].key, 3, 3)!=0) continue;
            // send_image
                
            GpG.pert_endtime[dis_gnum_plan_ops] = dessert;
            dis_gplan_ops[dis_gnum_plan_ops] = k;
            dis_gnum_plan_ops++;
            break;
        }
        
       
       /* 
        dis_print_op_name(k);printf(" ");
        dis_print_dis_Fact(f); printf(" ");
        dis_print_op_name(subs[j].key);
        printf(" %f (%f %f) (%f %f)\n",dessert,tin, tin+tip, t1, t2);
        */
    }        
}
    
void short_proc_sofar(int g)
{
    int i,j,k,s,l;
    char *a, *b;
    dis_Fact *f, *h;
    int num_as=0;
    int ms = -1; 
    float min_sof=10000;
    
    // short_ts: support instrument mode                
    for ( j = 0; j < dis_gnum_initial_predicate[short_ts]; j++ ) {
        f = &(dis_ginitial_predicate[short_ts][j]) ;
        // a: instrument
        a = dis_gconstants[(f->args)[0]];
        // b: mode
        b = dis_gconstants[(f->args)[1]];
        // g: haveimage direction mode
        if(dis_tok_ft_argncmp(g, 1, b)==0) {
            // short_rd: on_board instrument satellite
            for(k=0; k< dis_gnum_initial_predicate[short_rd]; k++ ) {
                h = &(dis_ginitial_predicate[short_rd][k]) ;
                if(strcmp(a, dis_gconstants[(h->args)[0]])==0) {
                    // s: satellite 
                    s = short_find_sat(dis_gconstants[(h->args)[1]]);
                    for(l=0; l<num_as;l++) {
                        if(s==avail_sat[l]) break;
                    }     
                    if(l==num_as) {
                        avail_sat[num_as] = s;
                        avail_inst[num_as] = a;
                        num_as++;
                    }                       
                    break;
                }       
            }
        }                                                   
    }
   
    if(num_as==0) {
        short_msof = -1;
        return;
    } 
    
    for(i=0; i<num_as; i++) {
        s = avail_sat[i];
     //   printf("could use %s %f\n", short_sat[s], sat_sofar[s]);
        if(sat_sofar[s]<min_sof) {
            min_sof = sat_sofar[s];
            ms = s; 
            short_mi = i;
        }
    }    
    short_msof = ms;
}
    
int short_msof_ops(int o)
{
    if(short_msof<0) return 0;
    if(dis_tok_ef_argncmp(o, 0, short_sat[short_msof])==0) return 1;
    if(dis_tok_ef_argncmp(o, 1, short_sat[short_msof])==0) return 2;
    return -1;
}
    

int short_connect(int goal)
{
    int i,f = -1 ,a,k;
    int sat_n;
    char *src = NULL,*sat, *inst, *tinst = NULL, *tsat = NULL, *iga = NULL, 
    *dest, *ibon;
    Bool pi = FALSE, cl=FALSE, pa=FALSE; 
    int spath[100];
    int num_spath;
    dis_Fact* h;    
    float sof;
    
    dis_gnum_plan_ops = 0;
    
    for(i=0; i<dis_ginitial_state.num_F; i++) {
        if(goal == dis_ginitial_state.F[i]) {
            return 0;
        }
    }
    
    if(dis_ft_predlen(goal) != MAX_PREDLEN) return -1;
    
    short_proc_sofar(goal);
    
    // find src and inst, dest
    sat = short_sat[short_msof];
    inst = avail_inst[short_mi];
    sat_n = short_find_sat(sat);
    dest = dis_ft_argn(goal, 0);
    ibon = dis_ft_argn(goal, 1);
    
    // find source
    for(i=0; i<dis_ginitial_state.num_F; i++) {
        f = dis_ginitial_state.F[i];
        if((dis_ft_predlen(f) == 8) && (dis_ft_arity(f)==2)) {
            if(strcmp(sat, dis_ft_argn(f, 0))==0) {
                src = dis_ft_argn(f, 1);
                break;
            }    
        }
    }     
    
    //printf("sat %s  inst %s  dest %s src %s\n", sat, inst, dest, src);
                  
    for(i=0; i<dis_ginitial_state.num_F; i++) {
        f = dis_ginitial_state.F[i];
        if((dis_ft_predlen(f) == 8)&&(dis_ft_arity(f) == 1)) {
            if(dis_tok_ft_argncmp(f, 0,  inst)==0) {
                pi = TRUE;
            }    
        } 
        if((dis_ft_predlen(f) == 10)&&(dis_ft_arity(f) == 1)) {
            if(dis_tok_ft_argncmp(f, 0,  inst)==0) {
                cl = TRUE;
              //  printf("cl "); dis_print_ft_name(f); printf("\n");
            }    
        } 
    }
    
    if(!pi) {
        for(i=0; i<dis_ginitial_state.num_F; i++) {
            f = dis_ginitial_state.F[i];
            if((dis_ft_predlen(f) == 11)&&(dis_ft_arity(f) == 1)) {
              if(dis_tok_ft_argncmp(f, 0,  sat)==0) {
                pa = TRUE;
              }    
            } 
        }
      
        if(!pa) {
            for(i=0; i<dis_ginitial_state.num_F; i++) {
                f = dis_ginitial_state.F[i];
                if((dis_ft_predlen(f) == 8)&&(dis_ft_arity(f) == 1)) {
                    tinst = dis_ft_argn(f,0);
                    for(k=0; k< dis_gnum_initial_predicate[short_rd]; k++ ) {
                        h = &(dis_ginitial_predicate[short_rd][k]) ;
                        if(strcmp(tinst, dis_gconstants[(h->args)[0]])==0) {
                            tsat = dis_gconstants[(h->args)[1]];
                            break;
                        }       
                    }
                    if(strcmp(sat,tsat) == 0) {
                        break;
                    }
                } 
            }
         //   dis_print_ft_name(f);
         //   printf(" %s %s\n", tinst, tsat);
            for(i=0; i<dis_gft_conn[f].num_PC; i++) {
                a = dis_gft_conn[f].PC[i];
                if(dis_ef_predlen(a)!= 10) continue;
                if(dis_ef_arity(a)!=2) continue;
                if(dis_tok_ef_argncmp(a, 0, tinst)!=0) continue;
                if(dis_tok_ef_argncmp(a, 1, sat)!=0) continue;
                dis_gplan_ops[dis_gnum_plan_ops++] = a; 
                break;            
            }
        }

        for(i=0; i<dis_gnum_ef_conn; i++) {
            if(dis_ef_predlen(i)!= 9) continue;
            if(dis_ef_arity(i)!=2) continue;
            if(dis_tok_ef_argncmp(i, 0, inst)!=0) continue;
            if(dis_tok_ef_argncmp(i, 1, sat)!=0) continue;
            dis_gplan_ops[dis_gnum_plan_ops++] = i;
            break;
        }
        cl = FALSE;
    }

    if(!cl) {    
        for(k=0; k< dis_gnum_initial_predicate[short_ctgt]; k++ ) {
            h = &(dis_ginitial_predicate[short_ctgt][k]) ;
            if(strcmp(inst, dis_gconstants[(h->args)[0]])==0) {
                iga = dis_gconstants[(h->args)[1]];
                break;  
            }       
        }
       // printf("iga = %s\n", iga);
        //printf("."); fflush(stdout);
        
        sof = find_shortest( src, iga, sat, spath, &num_spath);
        for(i=num_spath-1; i>=0; i--) {
            dis_gplan_ops[dis_gnum_plan_ops++] = spath[i]; 
        }           
       
        for(i=0; i<dis_gnum_ef_conn; i++) {
            if(dis_ef_predlen(i)!= 9) continue;
            if(dis_ef_arity(i)!=3) continue;
            if(dis_tok_ef_argncmp(i, 1, inst)!=0) continue;
            if(dis_tok_ef_argncmp(i, 0, sat)!=0) continue;
            if(dis_tok_ef_argncmp(i, 2, iga)!=0) continue;
            dis_gplan_ops[dis_gnum_plan_ops++] = i;
            break;
        }
        
        src = iga;
    }

            
    sof = find_shortest( src, dest, sat, spath, &num_spath);
    for(i=num_spath-1; i>=0; i--) {
        dis_gplan_ops[dis_gnum_plan_ops++] = spath[i]; 
    }           
                                                
        for(i=0; i<dis_gnum_ef_conn; i++) {
            if(dis_ef_predlen(i)!= 10) continue;
            if(dis_ef_arity(i)!=4) continue;
            if(dis_tok_ef_argncmp(i, 2, inst)!=0) continue;
            if(dis_tok_ef_argncmp(i, 0, sat)!=0) continue;
            if(dis_tok_ef_argncmp(i, 1, dest)!=0) continue;
            if(dis_tok_ef_argncmp(i, 3, ibon)!=0) continue;
            dis_gplan_ops[dis_gnum_plan_ops++] = i;
            break;
        }
    
    // solution
    for(i=0; i<dis_gnum_plan_ops; i++) {
    //    printf("plan %d ",i); dis_print_op_name(dis_gplan_ops[i]); printf("\n");
        sat_sofar[sat_n] += dis_gef_conn[dis_gplan_ops[i]].duration;  
    }
    
    return 1;
}



int short_given_connect(int goal, char* sat, char *inst)
{
    int i,f = -1,a,k;
    int sat_n;
    char *src = NULL, *tinst = NULL, *tsat = NULL, *iga = NULL, *dest, *ibon;
    Bool pi = FALSE, cl=FALSE, pa=FALSE; 
    int spath[100]; 
    int num_spath;
    dis_Fact* h;    
    float sof;
    
    dis_gnum_plan_ops = 0;
    
    for(i=0; i<dis_ginitial_state.num_F; i++) {
        if(goal == dis_ginitial_state.F[i]) {
            return 0;
        }
    }
    
    if(dis_ft_predlen(goal) != MAX_PREDLEN) return -1;
    
    // find src and inst, dest
    sat_n = short_find_sat(sat);
    dest = dis_ft_argn(goal, 0);
    ibon = dis_ft_argn(goal, 1);
    
    // find source
    for(i=0; i<dis_ginitial_state.num_F; i++) {
        f = dis_ginitial_state.F[i];
        if((dis_ft_predlen(f) == 8) && (dis_ft_arity(f)==2)) {
            if(strcmp(sat, dis_ft_argn(f, 0))==0) {
                src = dis_ft_argn(f, 1);
                break;
            }    
        }
    }     
     
    // printf("sat %s  inst %s  dest %s src %s\n", sat, inst, dest, src);
    
    for(i=0; i<dis_ginitial_state.num_F; i++) {
        f = dis_ginitial_state.F[i];
        if((dis_ft_predlen(f) == 8)&&(dis_ft_arity(f) == 1)) {
            // power_on instrument
            if(dis_tok_ft_argncmp(f, 0,  inst)==0) {
                pi = TRUE;
            }    
        } 
        if((dis_ft_predlen(f) == 10)&&(dis_ft_arity(f) == 1)) {
            // calibrated instrument
            if(dis_tok_ft_argncmp(f, 0,  inst)==0) {
                cl = TRUE;
            //    printf("cl "); dis_print_ft_name(f); printf("\n");
            }    
        } 
    }
    
    if(!pi) {
        for(i=0; i<dis_ginitial_state.num_F; i++) {
            f = dis_ginitial_state.F[i];
            if((dis_ft_predlen(f) == 11)&&(dis_ft_arity(f) == 1)) {
              // power_avail satellite
              if(dis_tok_ft_argncmp(f, 0,  sat)==0) {
                pa = TRUE;
              }    
            } 
        }
      
        if(!pa) {
            for(i=0; i<dis_ginitial_state.num_F; i++) {
                f = dis_ginitial_state.F[i];
                // power_on instrument
                if((dis_ft_predlen(f) == 8)&&(dis_ft_arity(f) == 1)) {
                    tinst = dis_ft_argn(f,0);
                    // short_rd: on_board instrument satellite
                    for(k=0; k< dis_gnum_initial_predicate[short_rd]; k++ ) {
                        h = &(dis_ginitial_predicate[short_rd][k]) ;
                        if(strcmp(tinst, dis_gconstants[(h->args)[0]])==0) {
                            tsat = dis_gconstants[(h->args)[1]];
                            break;
                        }       
                    }
                    if(strcmp(sat,tsat) == 0) {
                        break;
                    }
                } 
            }
            //dis_print_ft_name(f);
            //printf(" %s %s\n", tinst, tsat);
            for(i=0; i<dis_gft_conn[f].num_PC; i++) {
                a = dis_gft_conn[f].PC[i];
                // a: switch_off instrument satellite
                if(dis_ef_predlen(a)!= 10) continue;
                if(dis_ef_arity(a)!=2) continue;
                if(dis_tok_ef_argncmp(a, 0, tinst)!=0) continue;
                if(dis_tok_ef_argncmp(a, 1, sat)!=0) continue;
                dis_gplan_ops[dis_gnum_plan_ops++] = a; 
                break;            
            }
        }

        for(i=0; i<dis_gnum_ef_conn; i++) {
            // i: switch_on instrument satellite
            if(dis_ef_predlen(i)!= 9) continue;
            if(dis_ef_arity(i)!=2) continue;
            if(dis_tok_ef_argncmp(i, 0, inst)!=0) continue;
            if(dis_tok_ef_argncmp(i, 1, sat)!=0) continue;
            dis_gplan_ops[dis_gnum_plan_ops++] = i;
            break;
        }
        cl = FALSE;
    }

    if(!cl) {    
        for(k=0; k< dis_gnum_initial_predicate[short_ctgt]; k++ ) {
            // h: calibration_target instrument direction
            h = &(dis_ginitial_predicate[short_ctgt][k]) ;
            if(strcmp(inst, dis_gconstants[(h->args)[0]])==0) {
                iga = dis_gconstants[(h->args)[1]];
                break;  
            }       
        }
       // printf("iga = %s\n", iga);
        //printf("."); fflush(stdout);
        
        sof = find_shortest( src, iga, sat, spath, &num_spath);
        
        
        for(i=num_spath-1; i>=0; i--) {
            dis_gplan_ops[dis_gnum_plan_ops++] = spath[i]; 
        }           
       
        for(i=0; i<dis_gnum_ef_conn; i++) {
            // i: calibrate satellite instrument direction
            if(dis_ef_predlen(i)!= 9) continue;
            if(dis_ef_arity(i)!=3) continue;
            if(dis_tok_ef_argncmp(i, 1, inst)!=0) continue;
            if(dis_tok_ef_argncmp(i, 0, sat)!=0) continue;
            if(dis_tok_ef_argncmp(i, 2, iga)!=0) continue;
            dis_gplan_ops[dis_gnum_plan_ops++] = i;
            break;
        }
        
        src = iga;
    }

            
    sof = find_shortest( src, dest, sat, spath, &num_spath);
    for(i=num_spath-1; i>=0; i--) {
        dis_gplan_ops[dis_gnum_plan_ops++] = spath[i]; 
    }           
                                                
        for(i=0; i<dis_gnum_ef_conn; i++) {
            if(dis_ef_predlen(i)!= 10) continue;
            if(dis_ef_arity(i)!=4) continue;
            if(dis_tok_ef_argncmp(i, 2, inst)!=0) continue;
            if(dis_tok_ef_argncmp(i, 0, sat)!=0) continue;
            if(dis_tok_ef_argncmp(i, 1, dest)!=0) continue;
            if(dis_tok_ef_argncmp(i, 3, ibon)!=0) continue;
            // take_image satellite direction instrument mode
            dis_gplan_ops[dis_gnum_plan_ops++] = i;
            break;
        }
    
    // solution
    for(i=0; i<dis_gnum_plan_ops; i++) {
     //   printf("plan %d ",i); dis_print_op_name(dis_gplan_ops[i]); printf("\n");
        sat_sofar[sat_n] += dis_gef_conn[dis_gplan_ops[i]].duration;  
    }
    
    return 1;
}

Bool along_inst(char* inst, char* ibon)
{
    int j;
    dis_Fact* f;
    for ( j = 0; j < dis_gnum_initial_predicate[short_ts]; j++ ) {
        f = &(dis_ginitial_predicate[short_ts][j]) ;
        if((strcmp(dis_gconstants[(f->args)[0]], inst) ==0) && 
                (strcmp(dis_gconstants[(f->args)[1]], ibon) == 0)) return TRUE;
    }       
    return FALSE;
}

int short_support(int goal)
{                   
    int i; 
    int n=0;
    char *minst;
    for(i=0; i<short_num_sat; i++) {
        minst = sat_curr_inst[i];
        if(minst == NULL) continue;
        if(along_inst(minst, dis_ft_argn(goal, 1))) {
         //   printf("--- %s %s %s\n", short_sat[i], minst, dis_ft_argn(goal, 1));
            n++;             
        }
    }
    return n;
}

int short_min_sofar()
{
    int i, j, k, goal, sup_i, mi;
    float msof = 10000.0;
    char *msat, *minst;
    dis_Fact* h;
    int sup;
    
    mi = -1;    
    for(i=0; i<short_num_sat; i++) {
    //    printf("%d %s %f\n", sat_calloff[i], short_sat[i], sat_sofar[i]); 
        if(!sat_calloff[i]) {
            if(sat_sofar[i] < msof) {
                msof = sat_sofar[i];
                mi = i;
            }
        }
    }
    if(mi<0) {
        return -100;
    }
  
    msat = short_sat[mi]; 
    minst = sat_curr_inst[mi];
   
    
    if(minst != NULL) {
        for(i=0; i<saved_dis_gnum_flogic_goal; i++) {
            goal = saved_dis_gflogic_goal[i];
            if(dis_ft_predlen(goal) != MAX_PREDLEN) continue;
            for(j=0; j<dis_ginitial_state.num_F; j++) {
                if(goal == dis_ginitial_state.F[j]) {
                    break;
                }
            }
            if(j<dis_ginitial_state.num_F) continue;
            if(along_inst(minst, dis_ft_argn(goal, 1))) {
                select_inst = minst;
                select_sat = msat;
                return i;
            }
        } 
    } 
    
       
    sup = 10000; sup_i = -1;
    for(i=0; i<saved_dis_gnum_flogic_goal; i++) {
        goal = saved_dis_gflogic_goal[i];
        // SEND_IMAGE
        if(dis_ft_predlen(goal) != MAX_PREDLEN) continue;
        for(j=0; j<dis_ginitial_state.num_F; j++) {
            if(goal == dis_ginitial_state.F[j]) {
                break;
            }
        }
        if(j<dis_ginitial_state.num_F) continue;
   
        for(k=0; k< dis_gnum_initial_predicate[short_rd]; k++ ) {
                h = &(dis_ginitial_predicate[short_rd][k]) ;
                if(strcmp(msat, dis_gconstants[(h->args)[1]])==0) {
                    if(along_inst(dis_gconstants[(h->args)[0]], dis_ft_argn(goal, 1))) {
                        
                        if(short_support(goal) < sup) {
                            sup = short_support(goal);    
                            select_inst = dis_gconstants[(h->args)[0]];
                            select_sat = msat;
                            sup_i = i; 
                        }
                        break;
                    }
                }
        }                
    }
                           
    if(sup_i >=0) { 
        sat_curr_inst[mi] = select_inst;
        return sup_i;
    } else {
        sat_calloff[mi] = TRUE;
        return -1;
    }
}
