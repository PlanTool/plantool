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

 * File: stripsff.c

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
#include "dis_memory.h"
#include "dis_output.h"
#include "dis_inst_final.h"
#include "dis_constraints.h"

s_EfConn *s_gef_conn; 
s_EfConn *lpg_gef_conn;
SpecialFacts** lpg_sf;
Bool tlpg_fff;
TIF dis_gtif[MAX_NUM_TIF];
int dis_gnum_tif=0;
int saved_dis_gnum_op_conn;
Bool dis_isw = FALSE;
extern void dis_delete_gplan_ops(int i);
int myPERT(int, int, int *, float *);

/* Chih-Wei */
extern int *penalty;
dis_State saved_start_state;
#include "tilpert.c"

void save_lpg_gef_conn()
{
    int i;
    
    if(lpg_gef_conn) free(lpg_gef_conn);
    lpg_gef_conn = (s_EfConn *) calloc (gnum_ef_conn, sizeof(s_EfConn));
    lpg_sf = (SpecialFacts**) calloc (gnum_ef_conn, sizeof(SpecialFacts*));
        
    
    for(i=0; i<gnum_ef_conn; i++) {
        lpg_gef_conn[i].num_A = gef_conn[i].num_A;
        lpg_gef_conn[i].num_D = gef_conn[i].num_D;
        lpg_gef_conn[i].num_PC = gef_conn[i].num_PC;
        lpg_gef_conn[i].A = gef_conn[i].A;
        lpg_gef_conn[i].D = gef_conn[i].D;
        lpg_gef_conn[i].PC = gef_conn[i].PC;
        lpg_sf[i] = gef_conn[i].sf;
    }
    tlpg_fff = TRUE;
}

void load_lpg_gef_conn()
{
    int i;
    if(tlpg_fff) return;
    
    if(!lpg_gef_conn) {printf("error: lpg_gef_conn null\n"); exit(1); }
        
    for(i=0; i<gnum_ef_conn; i++) {
        gef_conn[i].num_A =  lpg_gef_conn[i].num_A;
        gef_conn[i].num_D =  lpg_gef_conn[i].num_D;
        gef_conn[i].num_PC = lpg_gef_conn[i].num_PC;
        gef_conn[i].A =      lpg_gef_conn[i].A;
        gef_conn[i].D =      lpg_gef_conn[i].D;
        gef_conn[i].PC =     lpg_gef_conn[i].PC;
        gef_conn[i].sf = lpg_sf[i];
    }
    tlpg_fff = TRUE;
}

void load_ff_gef_conn()
{
    int i;
    if(!tlpg_fff) return;
    
    if(!s_gef_conn) {printf("error: s_gef_conn null\n"); exit(1); }
        
    for(i=0; i<gnum_ef_conn; i++) {
        gef_conn[i].num_A =  s_gef_conn[i].num_A;
        gef_conn[i].num_D =  s_gef_conn[i].num_D;
        gef_conn[i].num_PC = s_gef_conn[i].num_PC;
        gef_conn[i].A =      s_gef_conn[i].A;
        gef_conn[i].D =      s_gef_conn[i].D;
        gef_conn[i].PC =     s_gef_conn[i].PC;
        gef_conn[i].sf = NULL;
    }
    tlpg_fff = FALSE;
}

void mff_to_lpg()
{
    int i;

    // OpConn
    gnum_op_conn = dis_gnum_op_conn;
    gop_conn = (OpConn *)dis_gop_conn;

    // EfConn
    gnum_ef_conn = dis_gnum_ef_conn;
    gef_conn = ( EfConn * ) calloc( gnum_ef_conn, sizeof( EfConn ) );
    for(i=0; i<gnum_ef_conn; i++) {
        gef_conn[i].op = dis_gef_conn[i].op;
        gef_conn[i].num_PC = dis_gef_conn[i].num_PC;
        gef_conn[i].PC     = dis_gef_conn[i].PC;
        gef_conn[i].num_A  = dis_gef_conn[i].num_A;
        gef_conn[i].A      = dis_gef_conn[i].A;
        gef_conn[i].num_D  = dis_gef_conn[i].num_D;
        gef_conn[i].D      = dis_gef_conn[i].D;
        gef_conn[i].sf = NULL;
    }
    
    // FtConn 
    gnum_ft_conn = dis_gnum_ft_conn;
    gft_conn = ( FtConn * ) calloc( gnum_ft_conn, sizeof( FtConn ) );
    for(i=0; i<gnum_ft_conn; i++) {
       gft_conn[i].num_PC = dis_gft_conn[i].num_PC;
       gft_conn[i].PC     = dis_gft_conn[i].PC;
       gft_conn[i].num_A  = dis_gft_conn[i].num_A;
       gft_conn[i].A      = dis_gft_conn[i].A;
       gft_conn[i].num_D  = dis_gft_conn[i].num_D;
       gft_conn[i].D      = dis_gft_conn[i].D;
       gft_conn[i].rand   = dis_gft_conn[i].rand;
    } 
}


void strips_gef_conn()
{
    int i,j,f;
    Bool toAdd;
    vState vs;
    if(s_gef_conn) free(s_gef_conn);
    s_gef_conn = (s_EfConn *) calloc (gnum_ef_conn, sizeof(s_EfConn));
    
    for(i=0; i<gnum_ef_conn; i++) {
        
        // precondition
        vs.num_F = 0;
        for (j = 0; j < gef_conn[i].num_PC; j++) {
            f = gef_conn[i].PC[j];
            if(f >= 0) {
               vs.F[vs.num_F++] = f; 
            }
        }
        if (gef_conn[i].sf) {
             for (j = 0; j < gef_conn[i].sf->num_PC_overall; j++) {
                f = gef_conn[i].sf->PC_overall[j];
                if(f >= 0) {
                    vs.F[vs.num_F++] = f; 
                }
             }
             for (j = 0; j < gef_conn[i].sf->num_PC_end; j++) {
                f = gef_conn[i].sf->PC_end[j];
                if(f >= 0) {
                    vs.F[vs.num_F++] = f; 
                }
             }
        }
        
        s_gef_conn[i].num_PC = vs.num_F;
        s_gef_conn[i].PC = (int *) calloc(vs.num_F, sizeof(int) );
        for(j=0; j<vs.num_F; j++)  s_gef_conn[i].PC[j] = vs.F[j];
    
        // add effects
        vs.num_F = 0;
        for (j = 0; j < gef_conn[i].num_A; j++) {
            f = gef_conn[i].A[j];
            if(f >= 0) {
               vs.F[vs.num_F++] = f; 
            }
        }
        if (gef_conn[i].sf) {
             for (j = 0; j < gef_conn[i].sf->num_A_start; j++) {
                f = gef_conn[i].sf->A_start[j];
                if(f >= 0) {
                    vs.F[vs.num_F++] = f; 
                }
             }
        }
        
        s_gef_conn[i].num_A = vs.num_F;
        s_gef_conn[i].A = (int *) calloc(vs.num_F, sizeof(int) );
        for(j=0; j<vs.num_F; j++)  s_gef_conn[i].A[j] = vs.F[j];
     
        
        // del effects   
        vs.num_F = 0;
        for (j = 0; j < gef_conn[i].num_D; j++) {
            f = gef_conn[i].D[j];
            if(f >= 0) {
                // New 
                /*
                toAdd = TRUE;
                if(gef_conn[i].sf){
                    for(k=0; k<gef_conn[i].sf->num_A_start; k++) {
                        if(f == gef_conn[i].sf->A_start[k]) {
                            toAdd = FALSE;
                            break;
                        }    
                    }                       
                }
                if(toAdd) vs.F[vs.num_F++] = f;
                else {
                    print_ft_name(f); printf(" not del ");
                    print_op_name(i); printf("\n");
                } */
                
                toAdd = TRUE;
                
                /* window
                if(i == gnum_ef_conn - 1)   {
                  if(gef_conn[i].sf){
                    for(k=0; k<gef_conn[i].sf->num_A_start; k++) {
                        if(f == gef_conn[i].sf->A_start[k]) {
                            toAdd = FALSE;
                            break;
                        }    
                    }                       
                  }
                } */
                
                if(toAdd) vs.F[vs.num_F++] = f;
            }
        }
        if (gef_conn[i].sf) {
             for (j = 0; j < gef_conn[i].sf->num_D_start; j++) {
                f = gef_conn[i].sf->D_start[j];
                if(f >= 0) {        
                    
                    /*
                    toAdd = TRUE;
                    for (k = 0; k < gef_conn[i].num_A; k++) {
                        if(f == gef_conn[i].A[k]) {
                            toAdd = FALSE;
                            break;
                        }
                    }
                    if(toAdd) vs.F[vs.num_F++] = f;
                    else {
                        print_ft_name(f); printf(" not del ");
                        print_op_name(i); printf("\n");
                    } */
                
                    vs.F[vs.num_F++] = f;
                }
             }
        }
        
        s_gef_conn[i].num_D = vs.num_F;
        s_gef_conn[i].D = (int *) calloc(vs.num_F, sizeof(int) );
        for(j=0; j<vs.num_F; j++)  s_gef_conn[i].D[j] = vs.F[j];
     
        
  //      printf("action %d PC %d A %d D %d\n", i, s_gef_conn[i].num_PC,
   //             s_gef_conn[i].num_A, s_gef_conn[i].num_D);
    }
    
}

void construct_mff()
{
    int i;

    // flConn;
    dis_gnum_fl_conn = 0;
    dis_gnum_real_fl_conn = 0;
    
    // OpConn
    dis_gnum_op_conn = gnum_op_conn;
    dis_gop_conn = (dis_OpConn *)gop_conn;

    // EfConn
    dis_gnum_ef_conn = gnum_ef_conn;
    dis_gef_conn = ( dis_EfConn * ) calloc( dis_gnum_ef_conn, sizeof( dis_EfConn ) );
    for(i=0; i<dis_gnum_ef_conn; i++) {
        dis_gef_conn[i].op = gef_conn[i].op;
        dis_gef_conn[i].illegal = dis_FALSE;
        dis_gef_conn[i].removed = dis_FALSE;
        dis_gef_conn[i].num_f_PC = 0;
        dis_gef_conn[i].num_I = 0;
        dis_gef_conn[i].num_IN = 0;
        dis_gef_conn[i].num_AS = 0;
        dis_gef_conn[i].cost = 0.0;
        dis_gef_conn[i].duration = gef_conn[i].duration;
        dis_gef_conn[i].num_PC = gef_conn[i].num_PC;
        dis_gef_conn[i].PC     = gef_conn[i].PC;
        dis_gef_conn[i].num_A  = gef_conn[i].num_A;
        dis_gef_conn[i].A      = gef_conn[i].A;
        dis_gef_conn[i].num_D  = gef_conn[i].num_D;
        dis_gef_conn[i].D      = gef_conn[i].D;
        if(gop_conn[i].DPop == TRUE) dis_gef_conn[i].DPop = dis_TRUE;
        else  dis_gef_conn[i].DPop = dis_FALSE;
    }
        
    
    // FtConn 
    dis_gnum_ft_conn = gnum_ft_conn;
    //printf("dis_gnum_ft_conn = %d\n", dis_gnum_ft_conn);
    dis_gft_conn = ( dis_FtConn * ) calloc( dis_gnum_ft_conn, sizeof( dis_FtConn ) );
    for(i=0; i<dis_gnum_ft_conn; i++) {
       dis_gft_conn[i].num_False = 0;
       dis_gft_conn[i].num_PC = gft_conn[i].num_PC;
       dis_gft_conn[i].PC     = gft_conn[i].PC;
       dis_gft_conn[i].num_A  = gft_conn[i].num_A;
       dis_gft_conn[i].A      = gft_conn[i].A;
       dis_gft_conn[i].num_D  = gft_conn[i].num_D;
       dis_gft_conn[i].D      = gft_conn[i].D;
       dis_gft_conn[i].rand   = gft_conn[i].rand;
    } 
    
    dis_make_state(&dis_mff_sol, dis_gnum_ft_conn, dis_gnum_fl_conn);
    dis_make_state(&dis_known_iga_list, dis_gnum_ft_conn, dis_gnum_fl_conn);
    dis_make_state(&work_S0, dis_gnum_ft_conn, dis_gnum_fl_conn);
    dis_make_state(&work_S1, dis_gnum_ft_conn, dis_gnum_fl_conn);
    work_ft = (int*) calloc(dis_gnum_ft_conn,sizeof(int));
    work_op = (int*) calloc(dis_gnum_ef_conn,sizeof(int));

    always_true_DPop();
    dis_set_DPft_flag();
}

void mff_start_and_goal(State *start_state, State *end_state)
{                       
    int i;
    static int first = 1;
    
    if(first == 1) { 
        dis_make_state(&dis_ginitial_state, dis_gnum_ft_conn, dis_gnum_fl_conn);
        first = 0;
    }
   
    dis_ginitial_state.num_F = start_state->num_F;
    for(i=0; i<start_state->num_F; i++) {
        dis_ginitial_state.F[i] = start_state->F[i];
    }
    
    
    if(!(NULL == (dis_gflogic_goal))) free(dis_gflogic_goal);
    
    
    dis_gflogic_goal = ( int * ) calloc( end_state->num_F, sizeof( int ) );
    for(i=0; i<end_state->num_F; i++) {
        dis_gflogic_goal[i] = end_state->F[i];
        //printf("goal %d %d\n",i, dis_gflogic_goal[i]);
    }
    dis_gnum_flogic_goal = end_state->num_F;
    dis_gnum_numeric_goal = 0;
    dis_gnum_fnumeric_goal = 0;
    dis_gmetric  = dis_FALSE;
    dis_gnum_plan_ops = 0;
}

int mff_H(State *start_state, State *end_state )
{
    mff_start_and_goal(start_state, end_state);
    return dis_get_1P(&dis_ginitial_state);
}   

void print_gderipreds()
{
    int i,j;
    printf ("\n\n------------------OP ARRAY:-----------------------");
     for (i = 0; i < gnum_deripreds; i++)
     { 
         printf ("\n\nDERIPRED %d: ",i);
         printf ("\n---------EFF");
         print_ft(gderipreds[i].derived);
         for (j = 0; j < gderipreds[i].num_prec; j++)
         {
             printf ("prec %d", j);
             print_ft(gderipreds[i].prec[j]);
         } 
     }
}


void dis_print_IPC4( char *fact_file_name )
{
 int i, curr_plan_length = 0;
  char cNameFile[256];
  FILE *fp;
  float start_time, time;
     
  printf("\nSolution found.\n");
  
	if (gcmd_line.out_file_name[0] == 0)
	{ 
		for (i=strlen(fact_file_name)-1;i>=0;i--)
			if (fact_file_name[i] == '/' || fact_file_name[i] == '\\')
				break;
		strcpy(cNameFile, fact_file_name+i+1);
		strcat(cNameFile, ".soln");
	}
	else
		strcpy(cNameFile, gcmd_line.out_file_name);
  
  // Hoffmann
  if (gcmd_line.out_file_name[0] == 0)   
    fp = stdout;
  else
  if ((fp = fopen (cNameFile, "w")) == NULL)
    { 
      printf ("\n\n\nError opening output file: %s", cNameFile);
      MSG_ERROR (WAR_OPEN_FILE);
      return;
    }
    
   times (&glob_end_time);
   time = (float) ((glob_end_time.tms_utime -
                 glob_start_time.tms_utime +
                 glob_end_time.tms_stime -  
                 glob_start_time.tms_stime) / 100.0);
                 
   fprintf (fp, "\n; Time %.2f", time  + GpG.glob_dist_time); 
  fprintf (fp, "\n; ParsingTime %.2f",  
            gtempl_time + greach_time + grelev_time + gconn_time + gnum_time);
  fprintf (fp, "\n; NrActions %d",  dis_gnum_plan_ops);
  fprintf (fp, "\n; MakeSpan");
  fprintf (fp, "\n; MetricValue");
  fprintf (fp, "\n\n");
                    
  for (i = 0; i < dis_gnum_plan_ops; i++){
    curr_plan_length++;
    start_time = (float)i + MIN_DELTA_TIME * curr_plan_length;
    fprintf (fp, "\t %.5f:  ", start_time);
    print_file_action_name (fp, dis_gplan_ops[i]);
    fprintf (fp, "[1.000]\n");
  } 
  if (fp != stdout)
  {
  fclose (fp);
  fprintf(stderr, "Answer file is %s\n\n", cNameFile);
  }
} 

void dis_print_IPC4_ffop_GpG( char *fact_file_name )
{
 int i,j,curr_plan_length = 0, index;
  char cNameFile[256];
  FILE *fp;
  float start_time, time, curr_time =0.0, makespan;
  dis_Action *a;
// Bool last = TRUE;

  dis_State SS, SSS;
  dis_make_state(&SS,dis_gnum_ft_conn, dis_gnum_fl_conn);
  dis_source_to_dest(&SS,&dis_ginitial_state);
  dis_make_state(&SSS,dis_gnum_ft_conn, dis_gnum_fl_conn);
  dis_source_to_dest(&SSS,&dis_mff_sol);
  dis_source_to_dest(&dis_ginitial_state, &saved_start_state);
  if (GpG.is_deripred)
  {
    dis_result_to_dest(&dis_mff_sol, &dis_ginitial_state, -1, -1);
    dis_source_to_dest(&dis_ginitial_state, &dis_mff_sol);
  }
  for (i=0;i<GpG.num_esp_sol;i++)
  {
    if (dis_result_to_dest(&dis_mff_sol, &dis_ginitial_state, GpG.esp_solution[i], -1))
      dis_source_to_dest(&dis_ginitial_state, &dis_mff_sol);
    else
        exit(0);
  }
    for (j = 0; j < saved_dis_gnum_fnumeric_goal; j++)
    {
        dis_gnum_fnumeric_goal = j+1;
        dis_gfnumeric_goal_comp[j] = saved_dis_gfnumeric_goal_comp[j];
        dis_gfnumeric_goal_fl[j] = saved_dis_gfnumeric_goal_fl[j];
        dis_gfnumeric_goal_c[j] = saved_dis_gfnumeric_goal_c[j];  
    }
    for (j = 0; j < saved_dis_gnum_flogic_goal; j++)
    {
        dis_gflogic_goal[j] = saved_dis_gflogic_goal[j];
        dis_gnum_flogic_goal = j+1;
    }
    if (dis_get_1P(&dis_mff_sol) > 0)
      exit(0);
  dis_source_to_dest(&dis_ginitial_state, &SS);
//  dis_source_to_dest(&dis_mff_sol, &SSS);
  
  if (GpG.is_durative)
  {
    makespan = 0;
    for (i=0;i<GpG.num_esp_sol;i++)
      makespan += dis_gef_conn[GpG.esp_solution[i]].duration;
  }
  else
    makespan = GpG.num_esp_sol;
  
  printf("\nSolution found.\n");
	if (gcmd_line.out_file_name[0] == 0)
	{ 
		for (i=strlen(fact_file_name)-1;i>=0;i--)
			if (fact_file_name[i] == '/' || fact_file_name[i] == '\\')
				break;
		strcpy(cNameFile, fact_file_name+i+1);
		strcat(cNameFile, ".soln");
	}
	else
		strcpy(cNameFile, gcmd_line.out_file_name);
/*
  i=0;
  while(1) {
        if(fact_file_name[i] == '/') {
             if((fact_file_name[i+1] == 'p') && (fact_file_name[i+2] != 'h'))
                                 last = TRUE;
            else last = FALSE;
        }

        if(last) {
            if((fact_file_name[i] == '.')|| (fact_file_name[i] == '-')) break;
        }
        cNameFile[i] = fact_file_name[i];
        i++;
  }         
  cNameFile[i]='.';
  cNameFile[i+1]='s';
  cNameFile[i+2]='o';
  cNameFile[i+3]='l';
  cNameFile[i+4]='n';
  cNameFile[i+5]='\0';
*/
  
  // Hoffmann
  if (gcmd_line.out_file_name[0] == 0)   
    fp = stdout;
  else
  if ((fp = fopen (cNameFile, "w")) == NULL)
    { 
      printf ("\n\n\nError opening output file: %s", cNameFile);
      MSG_ERROR (WAR_OPEN_FILE);
      return;
    }
    
   times (&glob_end_time);
   time = (float) ((glob_end_time.tms_utime -
                 glob_start_time.tms_utime +
                 glob_end_time.tms_stime -  
                 glob_start_time.tms_stime) / 100.0);
                 
   fprintf (fp, "\n; Time %.2f", time + GpG.glob_dist_time); 
  fprintf (fp, "\n; ParsingTime %.2f",  
            gtempl_time + greach_time + grelev_time + gconn_time + gnum_time);
  if (dis_gmetric)
  {
      j = 0;
      for (i=0;i<GpG.num_esp_sol;i++)
      {
        index = GpG.esp_solution[i] ; 
        a = dis_gop_conn[index].action;
        if ( a->norm_operator || a->pseudo_action ) 
          j++;
      }

    fprintf (fp, "\n; NrActions %d", j);
    fprintf (fp, "\n; MakeSpan");
    fprintf (fp, "\n; MetricValue %.3f", dis_metric_value(&dis_mff_sol) + dis_gtt*makespan);
  }
  else
    if (GpG.is_durative)
    {
      j = 0;
      for (i=0;i<GpG.num_esp_sol;i++)
      {
        index = GpG.esp_solution[i] ; 
        a = dis_gop_conn[index].action;
        if ( a->norm_operator || a->pseudo_action ) 
          j++;
      }

      fprintf (fp, "\n; NrActions %d", j);
      fprintf (fp, "\n; MakeSpan");
      fprintf (fp, "\n; MetricValue %.3f", makespan);
    }
    else
    {
      j = 0;
      for (i=0;i<GpG.num_esp_sol;i++)
      {
        index = GpG.esp_solution[i] ; 
        a = dis_gop_conn[index].action;
        if ( a->norm_operator || a->pseudo_action ) 
          j++;
      }
      
      fprintf (fp, "\n; NrActions %d", j);
      fprintf (fp, "\n; MakeSpan");
      fprintf (fp, "\n; MetricValue");
    }
  fprintf (fp, "\n\n");
                    
  for (i = 0; i < GpG.num_esp_sol; i++){
    curr_plan_length++;
    
    index = GpG.esp_solution[i] ; 
    start_time = curr_time +  MIN_DELTA_TIME * curr_plan_length;
    curr_time = curr_time +  dis_gef_conn[index].duration;
    
    fprintf (fp, "\t %.5f:  ", start_time);

    a = dis_gop_conn[index].action;
  
    //printf("index %d\n", index);
    
    if ( a->norm_operator ||
              a->pseudo_action ) {
          fprintf(fp, "(%s", a->name );
          for ( j = 0; j < a->num_name_vars; j++ ) {
              fprintf(fp, " %s", dis_gconstants[a->name_inst_table[j]]);
          } 
          fprintf(fp, ")");
    } 
    //fprintf (fp, "[1.000]\n");
    fprintf (fp, "[%.5f]\n", dis_gef_conn[index].duration);
  } 
  if (fp != stdout)
  {
  fclose (fp);
  fprintf(stderr, "Answer file is %s\n\n", cNameFile);
  }
} 

// Chih-Wei PDDL3

Bool dis_num_depend_efs(int a, int b) 
{
	int i, j;
	dis_Action *ga, *gb;
	
	if (GpG.SearchModal == -1001)
	{
	  ga = dis_gop_conn[dis_gef_conn[a].op].action;
	  gb = dis_gop_conn[dis_gef_conn[b].op].action;
	  if ((strlen(ga->name) == 11 || strlen(ga->name) == 13) && strlen(gb->name) == 10)
	    return TRUE;
	  if (strlen(ga->name) == 13 && ga->name[12] == '2' && strlen(gb->name) == 13 && gb->name[12] == '1')
	    return TRUE;
	  if (strlen(ga->name) == 10 && strlen(gb->name) == 13 && gb->name[12] == '2')
	    return TRUE;
	}
	
	for (i=0;i<dis_gef_conn[a].num_PC;i++)
          for (j=0;j<dis_gef_conn[b].num_PC;j++)
            if (dis_gef_conn[a].PC[i] == dis_gef_conn[b].PC[j])
              return TRUE;
	for(i=0; i<dis_gef_conn[a].num_f_PC; i++)
		for(j=0; j<dis_gef_conn[b].num_f_PC; j++)
			if (dis_gef_conn[a].f_PC_fl[i] == dis_gef_conn[b].f_PC_fl[j]) 
            			return TRUE;
	for(i=0; i<dis_gef_conn[a].num_f_PC; i++)
		for(j=0; j<dis_gef_conn[b].num_IN; j++)
			if (dis_gef_conn[a].f_PC_fl[i] == dis_gef_conn[b].IN_fl[j])
				if (dis_gef_conn[b].IN_c[j] > 0)
	            			return TRUE;
	return FALSE; 
}

void dis_print_IPC5_GpG( char *fact_file_name )
{
  int i, j, index;
  char cNameFile[256];
  FILE *fp;
  float time, makespan;
  dis_Action *a;
  
  makespan = 0;
  if (GpG.is_durative)
  {
    if (GpG.SearchModal == -1000)
    {
      PERT1000(GpG.num_esp_sol, GpG.esp_solution, GpG.pert_endtime, GpG.duration);
      for (i=0;i<GpG.num_esp_sol;i++)
      {
        index = dis_gop_conn[GpG.esp_solution[i]].E[0];
        if (GpG.duration[i] + GpG.pert_endtime[i] > makespan)
          makespan = GpG.duration[i] + GpG.pert_endtime[i];
      }
    }
    else if (GpG.SearchModal == -1005)
    {
      PERT1005(GpG.num_esp_sol, GpG.esp_solution, GpG.pert_endtime, GpG.duration);
      for (i=0;i<GpG.num_esp_sol;i++)
      {
        index = dis_gop_conn[GpG.esp_solution[i]].E[0];
        if (GpG.duration[i] + GpG.pert_endtime[i] > makespan)
          makespan = GpG.duration[i] + GpG.pert_endtime[i];
      }
    }
    else if (GpG.SearchModal == -1004 || GpG.SearchModal == -1001)
    {
      PERT1004(GpG.num_esp_sol, GpG.esp_solution, GpG.pert_endtime);
      for (i=0;i<GpG.num_esp_sol;i++)
      {
        index = dis_gop_conn[GpG.esp_solution[i]].E[0];
        if (dis_gef_conn[index].duration + GpG.pert_endtime[i] > makespan)
          makespan = dis_gef_conn[index].duration + GpG.pert_endtime[i];
      }
    }
    else
    {
      myPERT(GpG.num_esp_sol, 0, GpG.esp_solution, GpG.pert_endtime);
      for (i=0;i<GpG.num_esp_sol;i++)
      {
        index = dis_gop_conn[GpG.esp_solution[i]].E[0];
        if (dis_gef_conn[index].duration + GpG.pert_endtime[i] > makespan)
          makespan = dis_gef_conn[index].duration + GpG.pert_endtime[i];
      }
    }
  }
  
  fprintf(stderr, "\nSolution found.\n");
  if (gcmd_line.out_file_name[0] == 0)
  { 
    for (i=strlen(fact_file_name)-1;i>=0;i--)
      if (fact_file_name[i] == '/' || fact_file_name[i] == '\\')
	break;
    strcpy(cNameFile, fact_file_name+i+1);
    strcat(cNameFile, ".soln");
  }
  else
    strcpy(cNameFile, gcmd_line.out_file_name);
  
  // Hoffmann
  if (gcmd_line.out_file_name[0] == 0)   
    fp = stdout;
  else
  if ((fp = fopen (cNameFile, "w")) == NULL)
  { 
    fprintf (stderr, "\n\n\nError opening output file: %s", cNameFile);
    MSG_ERROR (WAR_OPEN_FILE);
    return;
  }
    
  times (&glob_end_time);
  time = (float) ((glob_end_time.tms_utime -
                 glob_start_time.tms_utime +
                 glob_end_time.tms_stime -  
                 glob_start_time.tms_stime) / 100.0);
                 
  fprintf (fp, "\n; Time %.2f", time + GpG.glob_dist_time); 
  fprintf (fp, "\n; ParsingTime %.2f", gparsing_time);
  if (dis_gmetric)
  {
      j = 0;
      for (i=0;i<GpG.num_esp_sol;i++)
      {
        index = GpG.esp_solution[i] ; 
        a = dis_gop_conn[index].action;
        if ( a->norm_operator || a->pseudo_action ) 
          j++;
      }
      
    fprintf (fp, "\n; NrActions %d", j);
    fprintf (fp, "\n; MakeSpan");
    fprintf (fp, "\n; MetricValue %.3f", dis_metric_value(&dis_mff_sol) + dis_gtt*makespan);
  }
  else
    if (GpG.is_durative)
    {
      j = 0;
      for (i=0;i<GpG.num_esp_sol;i++)
      {
        index = GpG.esp_solution[i] ; 
        a = dis_gop_conn[index].action;
        if ( a->norm_operator || a->pseudo_action ) 
          j++;
      }
      
      fprintf (fp, "\n; NrActions %d", j);
      fprintf (fp, "\n; MakeSpan");
      fprintf (fp, "\n; MetricValue %.3f", makespan);
    }
    else
    {
      j = 0;
      for (i=0;i<GpG.num_esp_sol;i++)
      {
        index = GpG.esp_solution[i] ; 
        a = dis_gop_conn[index].action;
        if ( a->norm_operator || a->pseudo_action ) 
          j++;
      }
      
      fprintf (fp, "\n; NrActions %d", j);
      fprintf (fp, "\n; MakeSpan");
      fprintf (fp, "\n; MetricValue");
    }
  if (GpG.subsolver == 1)
    fprintf (fp, "\n; PlanningTechnique Modified-FF(best-first search) as the subplanner");
  else if (GpG.subsolver == 2)
    fprintf (fp, "\n; PlanningTechnique Downward as the subplanner");
  else
    fprintf (fp, "\n; PlanningTechnique Modified-FF(enforced hill-climbing search) as the subplanner");
  fprintf (fp, "\n\n");
                 
  if (GpG.is_durative)
  for (i = 0; i < GpG.num_esp_sol; i++)
  {
    index = GpG.esp_solution[i] ; 
    a = dis_gop_conn[index].action;
    if ( a->norm_operator || a->pseudo_action ) 
    {
      fprintf (fp, "%.3f: ", GpG.pert_endtime[i]);
      if (a->name[0] != '+')
        fprintf(fp, "(%s", a->name );
      else
        for (j=strlen(a->name);j>=0;j--)
          if (a->name[j] == '_')
          {
            fprintf(fp, "(%s", a->name + j + 1);
            break;
          }
      
      for ( j = 0; j < a->num_name_vars; j++ ) 
        fprintf(fp, " %s", dis_gconstants[a->name_inst_table[j]]);
      if (GpG.SearchModal == -1000 || GpG.SearchModal == -1005)
        fprintf(fp, ") [%.4f]\n", GpG.duration[i]);
      else      
        fprintf(fp, ") [%.4f]\n", dis_gef_conn[dis_gop_conn[index].E[0]].duration);
    } 
  } 
  else
  for (i = 0; i < GpG.num_esp_sol; i++)
  {
    index = GpG.esp_solution[i] ; 
    a = dis_gop_conn[index].action;
    if ( a->norm_operator || a->pseudo_action ) 
    {
      fprintf (fp, "%.3f: ", 1.001*i+0.001);
      fprintf(fp, "(%s", a->name );
      for ( j = 0; j < a->num_name_vars; j++ ) 
        fprintf(fp, " %s", dis_gconstants[a->name_inst_table[j]]);
      fprintf(fp, ") [1]\n");
    } 
  }
  if (fp != stdout)
  { 
  fclose (fp);
  fprintf(stderr, "Answer file is %s\n\n", cNameFile);
  }
} 

void dis_print_IPC5_modal3_GpG( char *fact_file_name )
{
  int i, j, index;
  char cNameFile[256];
  FILE *fp;
  float time, makespan;
  Action *a;
  
  makespan = 0;
  if (GpG.is_durative)
  {
      myPERT(GpG.num_esp_sol, 0, GpG.esp_solution, GpG.pert_endtime);
      for (i=0;i<GpG.num_esp_sol;i++)
      {
        index = dis_gop_conn[GpG.esp_solution[i]].E[0];
        if (dis_gef_conn[index].duration + GpG.pert_endtime[i] > makespan)
          makespan = gef_conn[index].duration + GpG.pert_endtime[i];
      }
  }
  
  fprintf(stderr, "\nSolution found.\n");
  if (gcmd_line.out_file_name[0] == 0)
  { 
    for (i=strlen(fact_file_name)-1;i>=0;i--)
      if (fact_file_name[i] == '/' || fact_file_name[i] == '\\')
	break;
    strcpy(cNameFile, fact_file_name+i+1);
    strcat(cNameFile, ".soln");
  }
  else
    strcpy(cNameFile, gcmd_line.out_file_name);
  
  // Hoffmann
  if (gcmd_line.out_file_name[0] == 0)   
    fp = stdout;
  else
  if ((fp = fopen (cNameFile, "w")) == NULL)
  { 
    fprintf (stderr, "\n\n\nError opening output file: %s", cNameFile);
    MSG_ERROR (WAR_OPEN_FILE);
    return;
  }
    
  times (&glob_end_time);
  time = (float) ((glob_end_time.tms_utime -
                 glob_start_time.tms_utime +
                 glob_end_time.tms_stime -  
                 glob_start_time.tms_stime) / 100.0);
                 
  fprintf (fp, "\n; Time %.2f", time + GpG.glob_dist_time); 
  fprintf (fp, "\n; ParsingTime %.2f", gparsing_time);
  if (dis_gmetric)
  {
      j = 0;
      for (i=0;i<GpG.num_esp_sol;i++)
      {
        index = GpG.esp_solution[i] ; 
        a = gop_conn[index].action;
        if ( a->norm_operator || a->pseudo_action ) 
          j++;
      }
      
    fprintf (fp, "\n; NrActions %d", j);
    fprintf (fp, "\n; MakeSpan");
    fprintf (fp, "\n; MetricValue %.3f", makespan);
  }
  else
    if (GpG.is_durative)
    {
      j = 0;
      for (i=0;i<GpG.num_esp_sol;i++)
      {
        index = GpG.esp_solution[i] ; 
        a = gop_conn[index].action;
        if ( a->norm_operator || a->pseudo_action ) 
          j++;
      }
      
      fprintf (fp, "\n; NrActions %d", j);
      fprintf (fp, "\n; MakeSpan");
      fprintf (fp, "\n; MetricValue %.3f", makespan);
    }
    else
    {
      j = 0;
      for (i=0;i<GpG.num_esp_sol;i++)
      {
        index = GpG.esp_solution[i] ; 
        a = gop_conn[index].action;
        if ( a->norm_operator || a->pseudo_action ) 
          j++;
      }
      
      fprintf (fp, "\n; NrActions %d",  j);
      fprintf (fp, "\n; MakeSpan");
      fprintf (fp, "\n; MetricValue");
    }
  if (GpG.subsolver == 1)
    fprintf (fp, "\n; PlanningTechnique Modified-FF(best-first search) as the subplanner");
  else
    fprintf (fp, "\n; PlanningTechnique Modified-FF(enforced hill-climbing search) as the subplanner");
  fprintf (fp, "\n\n");
                 
  if (GpG.is_durative)
  for (i = 0; i < GpG.num_esp_sol; i++)
  {
    index = GpG.esp_solution[i] ; 
    fprintf (fp, "%.3f: ", GpG.pert_endtime[i]);
    a = gop_conn[index].action;
    if ( a->norm_operator || a->pseudo_action ) 
    {
      if (a->name[0] != '+')
        fprintf(fp, "(%s", a->name );
      else
        for (j=strlen(a->name);j>=0;j--)
          if (a->name[j] == '_')
          {
            fprintf(fp, "(%s", a->name + j + 1);
            break;
          }
      
      for ( j = 0; j < a->num_name_vars; j++ ) 
        fprintf(fp, " %s", gconstants[a->name_inst_table[j]]);
      fprintf(fp, ") [%.4f]\n", gef_conn[dis_gop_conn[index].E[0]].duration);
    } 
  } 
  else
  for (i = 0; i < GpG.num_esp_sol; i++)
  {
    index = GpG.esp_solution[i] ; 
    fprintf (fp, "%.3f: ", 1.001*i+0.001);
    a = gop_conn[index].action;
    if ( a->norm_operator || a->pseudo_action ) 
    {
      fprintf(fp, "(%s", a->name );
      for ( j = 0; j < a->num_name_vars; j++ ) 
        fprintf(fp, " %s", gconstants[a->name_inst_table[j]]);
      fprintf(fp, ") [1]\n");
    } 
  }
  if (fp != stdout)
  { 
  fclose (fp);
  fprintf(stderr, "Answer file is %s\n\n", cNameFile);
  }
} 

void dis_print_IPC4_ffop_ffsol( char *fact_file_name )
{
 int i,j,curr_plan_length = 0, index;
  char cNameFile[256];
  FILE *fp;
  float start_time, time, curr_time=0.0, makespan;
  dis_Action *a;
//  Bool last = TRUE;
  dis_State SS, SSS;
  
  GpG.num_esp_sol = dis_gnum_plan_ops;
  memcpy(GpG.esp_solution, dis_gplan_ops, sizeof(int)*dis_gnum_plan_ops);
  dis_print_IPC4_ffop_GpG(fact_file_name);
  return;
  dis_make_state(&SS,dis_gnum_ft_conn, dis_gnum_fl_conn);
  dis_source_to_dest(&SS,&dis_ginitial_state);
  dis_make_state(&SSS,dis_gnum_ft_conn, dis_gnum_fl_conn);
  dis_source_to_dest(&SSS,&dis_mff_sol);
  dis_source_to_dest(&dis_ginitial_state, &saved_start_state);
  if (GpG.is_deripred)
  {
    dis_result_to_dest(&dis_mff_sol, &dis_ginitial_state, -1, -1);
    dis_source_to_dest(&dis_ginitial_state, &dis_mff_sol);
  }
  for (i=0;i<dis_gnum_plan_ops;i++)
  {
    if (dis_result_to_dest(&dis_mff_sol, &dis_ginitial_state, dis_gplan_ops[i], -1))
      dis_source_to_dest(&dis_ginitial_state, &dis_mff_sol);
    else
        exit(0);
  }
    for (j = 0; j < saved_dis_gnum_fnumeric_goal; j++)
    {
        dis_gnum_fnumeric_goal = j+1;
        dis_gfnumeric_goal_comp[j] = saved_dis_gfnumeric_goal_comp[j];
        dis_gfnumeric_goal_fl[j] = saved_dis_gfnumeric_goal_fl[j];
        dis_gfnumeric_goal_c[j] = saved_dis_gfnumeric_goal_c[j];  
    }
    for (j = 0; j < saved_dis_gnum_flogic_goal; j++)
    {
        dis_gflogic_goal[j] = saved_dis_gflogic_goal[j];
        dis_gnum_flogic_goal = j+1;
    }
    if (dis_get_1P(&dis_mff_sol) > 0)
      exit(0);
  dis_source_to_dest(&dis_ginitial_state, &SS);
  dis_source_to_dest(&dis_mff_sol, &SSS);

  makespan = 0;
  if (GpG.is_durative)
  for (i=0;i<dis_gnum_plan_ops;i++)
    makespan += dis_gef_conn[dis_gplan_ops[i]].duration;
  else
    makespan = dis_gnum_plan_ops;
   
  printf("\nSolution found.\n");
	if (gcmd_line.out_file_name[0] == 0)
	{ 
		for (i=strlen(fact_file_name)-1;i>=0;i--)
			if (fact_file_name[i] == '/' || fact_file_name[i] == '\\')
				break;
		strcpy(cNameFile, fact_file_name+i+1);
		strcat(cNameFile, ".soln");
	}
	else
		strcpy(cNameFile, gcmd_line.out_file_name);
/*  
  i=0;
  while(1) {
        if(fact_file_name[i] == '/') {
             if((fact_file_name[i+1] == 'p') && (fact_file_name[i+2] != 'h'))
                                 last = TRUE;
            else last = FALSE;
        }

        if(last) {
            if((fact_file_name[i] == '.')|| (fact_file_name[i] == '-')) break;
        }
        cNameFile[i] = fact_file_name[i];
        i++;
  }         
  cNameFile[i]='.';
  cNameFile[i+1]='s';
  cNameFile[i+2]='o';
  cNameFile[i+3]='l';
  cNameFile[i+4]='n';
  cNameFile[i+5]='\0';
*/
  
  // Hoffmann
  if (gcmd_line.out_file_name[0] == 0)   
    fp = stdout;
  else
  if ((fp = fopen (cNameFile, "w")) == NULL)
    { 
      printf ("\n\n\nError opening output file: %s", cNameFile);
      MSG_ERROR (WAR_OPEN_FILE);
      return;
    }
    
   times (&glob_end_time);
   time = (float) ((glob_end_time.tms_utime -
                 glob_start_time.tms_utime +
                 glob_end_time.tms_stime -  
                 glob_start_time.tms_stime) / 100.0);
                 
  // fprintf (fp, "\n; Time %.2f", time); 
   fprintf (fp, "\n; Time %.2f", time + GpG.glob_dist_time); 
  fprintf (fp, "\n; ParsingTime %.2f",  
            gtempl_time + greach_time + grelev_time + gconn_time + gnum_time);
  if (dis_gmetric)
  {
      j = 0;
      for (i=0;i<dis_gnum_plan_ops;i++)
      {
        index = dis_gplan_ops[i] ; 
        a = dis_gop_conn[index].action;
        if ( a->norm_operator || a->pseudo_action ) 
          j++;
      }
      
    fprintf (fp, "\n; NrActions %d", j);
    fprintf (fp, "\n; MakeSpan");
    fprintf (fp, "\n; MetricValue %.3f", dis_metric_value(&dis_mff_sol) + dis_gtt*makespan);
  }
  else
    if (GpG.is_durative)
    {
      j = 0;
      for (i=0;i<dis_gnum_plan_ops;i++)
      {
        index = dis_gplan_ops[i] ; 
        a = dis_gop_conn[index].action;
        if ( a->norm_operator || a->pseudo_action ) 
          j++;
      }
      
      fprintf (fp, "\n; NrActions %d", j);
      fprintf (fp, "\n; MakeSpan");
      fprintf (fp, "\n; MetricValue %.3f", makespan);
    }
    else
    {
      j = 0;
      for (i=0;i<dis_gnum_plan_ops;i++)
      {
        index = dis_gplan_ops[i] ; 
        a = dis_gop_conn[index].action;
        if ( a->norm_operator || a->pseudo_action ) 
          j++;
      }
      
      fprintf (fp, "\n; NrActions %d", j);
      fprintf (fp, "\n; MakeSpan");
      fprintf (fp, "\n; MetricValue");
    }
  fprintf (fp, "\n\n");
                    
  for (i = 0; i < dis_gnum_plan_ops; i++){
    curr_plan_length++;
 //   start_time = (float)i + MIN_DELTA_TIME * curr_plan_length;
    index = dis_gplan_ops[i] ; 
   
    start_time = curr_time +  MIN_DELTA_TIME * curr_plan_length;
    curr_time = curr_time +  dis_gef_conn[index].duration;
    
    fprintf (fp, "\t %.5f:  ", start_time);

    a = dis_gop_conn[index].action;
  
    //printf("index %d\n", index);
    
    if ( a->norm_operator ||
              a->pseudo_action ) {
          fprintf(fp, "(%s", a->name );
          for ( j = 0; j < a->num_name_vars; j++ ) {
              fprintf(fp, " %s", dis_gconstants[a->name_inst_table[j]]);
          } 
          fprintf(fp, ")");
    } 
    //fprintf (fp, "[1.000]\n");
    fprintf (fp, "[%.5f]\n", dis_gef_conn[index].duration);
  } 
  if (fp != stdout)
  {
  fclose (fp);
  fprintf(stderr, "Answer file is %s\n\n", cNameFile);
  }
} 

void lpg_mff_facts()
{
    int i,j;
    char slpg[MAX_LENGTH];
    char smff[MAX_LENGTH];
    for(i=0; i<gnum_ft_conn; i++) {
        gft_conn[i].mff = -1;
    } 
    for(i=0; i<dis_gnum_ft_conn; i++) {
        dis_gft_conn[i].lpg = -1;
    } 
    
    for(i=0; i<gnum_ft_conn; i++) {
        print_ft_name_string(i, slpg);
        printf("lpg %d %s\t",i,slpg);
        for(j=0; j<dis_gnum_ft_conn; j++) {
            if(dis_gft_conn[j].lpg>=0) continue;
            dis_print_ft_name_string(j, smff);
            if(strcmp(slpg, smff)==0) {
                gft_conn[i].mff = j;
                dis_gft_conn[j].lpg = i;
                printf("mff %d %s\n",j,smff);
            }
        }
    }
    for(i=0; i<dis_gnum_ft_conn; i++) {
        if(dis_gft_conn[i].lpg<0) {
            dis_print_ft_name_string(i, smff);
            printf("notfound mff %d %s\n",i,smff);
        }
    } 
    
    exit(0);
}


void dis_print_sched_ffsol( char *fact_file_name )
{
 int i,j,curr_plan_length = 0, index;
  char cNameFile[256];
  FILE *fp;
  float start_time, time, makespan = 0;
  dis_Action *a;
//  Bool last = TRUE;

  dis_State SS, SSS;
  dis_make_state(&SS,dis_gnum_ft_conn, dis_gnum_fl_conn);
  dis_source_to_dest(&SS,&dis_ginitial_state);
  dis_make_state(&SSS,dis_gnum_ft_conn, dis_gnum_fl_conn);
  dis_source_to_dest(&SSS,&dis_mff_sol);
  dis_source_to_dest(&dis_ginitial_state, &saved_start_state);
  if (GpG.is_deripred)
  {
    dis_result_to_dest(&dis_mff_sol, &dis_ginitial_state, -1, -1);
    dis_source_to_dest(&dis_ginitial_state, &dis_mff_sol);
  }
  for (i=0;i<dis_gnum_plan_ops;i++)
  {
    if (dis_result_to_dest(&dis_mff_sol, &dis_ginitial_state, dis_gplan_ops[i], -1))
      dis_source_to_dest(&dis_ginitial_state, &dis_mff_sol);
    else
        exit(0);
  }
    for (j = 0; j < saved_dis_gnum_fnumeric_goal; j++)
    {
        dis_gnum_fnumeric_goal = j+1;
        dis_gfnumeric_goal_comp[j] = saved_dis_gfnumeric_goal_comp[j];
        dis_gfnumeric_goal_fl[j] = saved_dis_gfnumeric_goal_fl[j];
        dis_gfnumeric_goal_c[j] = saved_dis_gfnumeric_goal_c[j];  
    }
    for (j = 0; j < saved_dis_gnum_flogic_goal; j++)
    {
        dis_gflogic_goal[j] = saved_dis_gflogic_goal[j];
        dis_gnum_flogic_goal = j+1;
    }
    if (dis_get_1P(&dis_mff_sol) > 0)
      exit(0);
  dis_source_to_dest(&dis_ginitial_state, &SS);
  dis_source_to_dest(&dis_mff_sol, &SSS);

  for (i=0;i<dis_gnum_plan_ops;i++)
  {
    if (dis_gplan_ops[i] < 0)
      continue;
    index = dis_gplan_ops[i];
    makespan = makespan < GpG.pert_endtime[i] ? GpG.pert_endtime[i] : makespan;
  }
   
  printf("\nSolution found.\n");
	if (gcmd_line.out_file_name[0] == 0)
	{ 
		for (i=strlen(fact_file_name)-1;i>=0;i--)
			if (fact_file_name[i] == '/' || fact_file_name[i] == '\\')
				break;
		strcpy(cNameFile, fact_file_name+i+1);
		strcat(cNameFile, ".soln");
	}
	else
		strcpy(cNameFile, gcmd_line.out_file_name);
/*
  i=0;
  while(1) {
        if(fact_file_name[i] == '/') {
             if((fact_file_name[i+1] == 'p') && (fact_file_name[i+2] != 'h'))
                                 last = TRUE;
            else last = FALSE;
        }

        if(last) {
            if((fact_file_name[i] == '.')|| (fact_file_name[i] == '-')) break;
        }
        cNameFile[i] = fact_file_name[i];
        i++;
  }         
  cNameFile[i]='.';
  cNameFile[i+1]='s';
  cNameFile[i+2]='o';
  cNameFile[i+3]='l';
  cNameFile[i+4]='n';
  cNameFile[i+5]='\0';
*/
  
  // Hoffmann
  if (gcmd_line.out_file_name[0] == 0)   
    fp = stdout;
  else
  if ((fp = fopen (cNameFile, "w")) == NULL)
    { 
      printf ("\n\n\nError opening output file: %s", cNameFile);
      MSG_ERROR (WAR_OPEN_FILE);
      return;
    }
    
   times (&glob_end_time);
   time = (float) ((glob_end_time.tms_utime -
                 glob_start_time.tms_utime +
                 glob_end_time.tms_stime -  
                 glob_start_time.tms_stime) / 100.0);
                 
   //fprintf (fp, "\n; Time %.2f", time); 
   fprintf (fp, "\n; Time %.2f", time + GpG.glob_dist_time); 
  fprintf (fp, "\n; ParsingTime %.2f",  
            gtempl_time + greach_time + grelev_time + gconn_time + gnum_time);
  if (dis_gmetric)
  {
      j = 0;
      for (i=0;i<dis_gnum_plan_ops;i++)
      {
        index = dis_gplan_ops[i] ; 
        a = dis_gop_conn[index].action;
        if ( a->norm_operator || a->pseudo_action ) 
          j++;
      }
      
    fprintf (fp, "\n; NrActions %d", j);
    fprintf (fp, "\n; MakeSpan");
    fprintf (fp, "\n; MetricValue %.3f", dis_metric_value(&dis_mff_sol) + dis_gtt*makespan);
  }
  else
    if (GpG.is_durative)
    {
      j = 0;
      for (i=0;i<dis_gnum_plan_ops;i++)
      {
        index = dis_gplan_ops[i] ; 
        a = dis_gop_conn[index].action;
        if ( a->norm_operator || a->pseudo_action ) 
          j++;
      }
      
      fprintf (fp, "\n; NrActions %d", j);
      fprintf (fp, "\n; MakeSpan");
      fprintf (fp, "\n; MetricValue %.3f", makespan);
    }
    else
    {
      j = 0;
      for (i=0;i<dis_gnum_plan_ops;i++)
      {
        index = dis_gplan_ops[i] ; 
        a = dis_gop_conn[index].action;
        if ( a->norm_operator || a->pseudo_action ) 
          j++;
      }
      
      fprintf (fp, "\n; NrActions %d", j);
      fprintf (fp, "\n; MakeSpan");
      fprintf (fp, "\n; MetricValue");
    }
  fprintf (fp, "\n\n");
  
  for (i = 0; i < dis_gnum_plan_ops; i++){
    curr_plan_length++;
 //   start_time = (float)i + MIN_DELTA_TIME * curr_plan_length;
    index = dis_gplan_ops[i] ; 
   
    start_time = GpG.pert_endtime[i] - dis_gef_conn[index].duration;
//        +  MIN_DELTA_TIME * curr_plan_length/2;
    
    fprintf (fp, "\t %.5f:  ", start_time);

    a = dis_gop_conn[index].action;  
  
    //printf("index %d\n", index);
    
    if ( a->norm_operator ||
              a->pseudo_action ) {
          fprintf(fp, "(%s", a->name );
          for ( j = 0; j < a->num_name_vars; j++ ) {
              fprintf(fp, " %s", dis_gconstants[a->name_inst_table[j]]);
          } 
          fprintf(fp, ")");
    } 
    //fprintf (fp, "[1.000]\n");
    fprintf (fp, "[%.5f]\n", dis_gef_conn[index].duration);
  } 
   
  if(GpG.SearchModal == 106) {
        start_time = 0.0;
        for(i=dis_gnum_ef_conn-1; i>=0; i--) {
            if(dis_ef_arity(i)!=0) break;
            if(dis_ef_predlen(i)>20) break;
            fprintf (fp, "\t %.5f:  ", start_time);
            a = dis_gop_conn[dis_gef_conn[i].op].action;  
            if ( a->norm_operator ||
              a->pseudo_action ) {
                fprintf(fp, "(%s", a->name );
                for ( j = 0; j < a->num_name_vars; j++ ) {
                    fprintf(fp, " %s", dis_gconstants[a->name_inst_table[j]]);
                } 
                fprintf(fp, ")");
    
            } 
            fprintf (fp, "[%.5f]\n", dis_gef_conn[i].duration);
            if(i!=dis_gnum_ef_conn-1) {
                start_time = start_time+dis_gef_conn[i].duration;
            }
        }
  } 
  if (fp != stdout)
  {
  fclose (fp);
  fprintf(stderr, "Answer file is %s\n\n", cNameFile);
  }
} 

void dis_print_PERT_ffsol( char *fact_file_name )
{
    dis_PERT(TRUE);
    dis_print_sched_ffsol(fact_file_name);
}

void dis_GpG_to_ffsol()
{
    int i;
    for (i = 0; i < GpG.num_esp_sol; i++){
        dis_gplan_ops[i] = GpG.esp_solution[i];
    }
    dis_gnum_plan_ops = GpG.num_esp_sol;
}

void dis_print_PERT_GpG( char *fact_file_name )
{                       
    dis_GpG_to_ffsol();
    dis_print_PERT_ffsol(fact_file_name);
}

void dis_PERT_GpG()
{
    dis_GpG_to_ffsol();
    dis_PERT(TRUE);
}

Bool init_timed_wrapper()
{
    int i;
    int n=0;
    TIF x;
    float dur_time=0.0;
   
    saved_dis_gnum_op_conn = dis_gnum_op_conn; 
    dis_isw = FALSE;
    
    dis_gnum_tif=0;
    for(i=dis_gnum_op_conn-1; i>=0; i-- ) {
        if((dis_ef_predlen(i) > MAX_PREDLEN)
           && (dis_ef_predlen(i) < 2*MAX_PREDLEN)) {
            dis_isw = TRUE;
       
         /*   
            dis_print_op_name(i); printf(" %f \n", dis_gef_conn[i].duration);
            for(j=0; j<dis_gef_conn[i].num_A; j++) {
                dis_print_ft_name(dis_gef_conn[i].A[j]); 
                printf("\n");
            }
            */
                     
           if(n==0) {
                n=1;
                dis_ginitial_state.F[dis_ginitial_state.num_F] 
                    = dis_gef_conn[i].A[0];
                dis_ginitial_state.num_F++;  
                continue;
           }
           
           if(n==1) {
                n=2;     
                x.pred = dis_gef_conn[i].A[1];
                dur_time = dur_time + dis_gef_conn[i].duration;
                x.begin = dur_time;
                dis_ginitial_state.F[dis_ginitial_state.num_F] 
                    = dis_gef_conn[i].A[1];
                dis_ginitial_state.num_F++;  
                continue;
           } 
           
           if(n==2) {
                n=1;
                dur_time = dur_time + dis_gef_conn[i].duration;
                x.end = dur_time;
                dis_gtif[dis_gnum_tif++] = x; 
                if(dis_gnum_tif > MAX_NUM_TIF) {
                    printf("exit 116"); exit(1);
                } 
                continue;
           }
        }
    }

    /*    
    for(i=0; i<dis_ginitial_state.num_F; i++) {
        dis_print_ft_name(dis_ginitial_state.F[i]);
        printf("\n");
    }*/
    
    /*
    for(i=0; i<dis_gnum_tif; i++) {
        dis_print_ft_name(dis_gtif[i].pred);
        printf(" %f %f\n", dis_gtif[i].begin, dis_gtif[i].end);
    }
    */

    return dis_isw;
}

void dis_print_sched_Modal7tif_ffsol( char *fact_file_name )
{
 int i,j,curr_plan_length = 0, index;
  char cNameFile[256];
  FILE *fp;
  float start_time, time;
  dis_Action *a;
  int makespan = 0;
  for (i=0;i<GpG.num_esp_sol;i++)
  {
    if (GpG.esp_solution[i] < 0)
      continue;
    index = GpG.esp_solution[i];
    makespan = makespan < GpG.pert_endtime[i] ? GpG.pert_endtime[i] : makespan;
  }
//  Bool last = TRUE;
  
  printf("\nSolution found.\n");
	if (gcmd_line.out_file_name[0] == 0)
	{ 
		for (i=strlen(fact_file_name)-1;i>=0;i--)
			if (fact_file_name[i] == '/' || fact_file_name[i] == '\\')
				break;
		strcpy(cNameFile, fact_file_name+i+1);
		strcat(cNameFile, ".soln");
	}
	else
		strcpy(cNameFile, gcmd_line.out_file_name);
/*
  i=0;
  while(1) {
        if(fact_file_name[i] == '/') {
             if((fact_file_name[i+1] == 'p') && (fact_file_name[i+2] != 'h'))
                                 last = TRUE;
            else last = FALSE;
        }

        if(last) {
            if((fact_file_name[i] == '.')|| (fact_file_name[i] == '-')) break;
        }
        cNameFile[i] = fact_file_name[i];
        i++;
  }         
  cNameFile[i]='.';
  cNameFile[i+1]='s';
  cNameFile[i+2]='o';
  cNameFile[i+3]='l';
  cNameFile[i+4]='n';
  cNameFile[i+5]='\0';
*/
  
  // Hoffmann
  if (gcmd_line.out_file_name[0] == 0)   
    fp = stdout;
  else
  if ((fp = fopen (cNameFile, "w")) == NULL)
    { 
      printf ("\n\n\nError opening output file: %s", cNameFile);
      MSG_ERROR (WAR_OPEN_FILE);
      return;
    }
    
   times (&glob_end_time);
   time = (float) ((glob_end_time.tms_utime -
                 glob_start_time.tms_utime +
                 glob_end_time.tms_stime -  
                 glob_start_time.tms_stime) / 100.0);
                 
   //fprintf (fp, "\n; Time %.2f", time); 
   fprintf (fp, "\n; Time %.2f", time + GpG.glob_dist_time); 
  fprintf (fp, "\n; ParsingTime %.2f",  
            gtempl_time + greach_time + grelev_time + gconn_time + gnum_time);
  fprintf (fp, "\n; NrActions %d",  dis_gnum_plan_ops);
  fprintf (fp, "\n; MakeSpan %d", makespan);
  fprintf (fp, "\n; MetricValue");
  fprintf (fp, "\n\n");
    

  
  for (i = 0; i < GpG.num_esp_sol; i++){
    if (GpG.esp_solution[i] < 0)
      continue;
    curr_plan_length++;
 //   start_time = (float)i + MIN_DELTA_TIME * curr_plan_length;
    index = GpG.esp_solution[i] ; 
   
    start_time = GpG.pert_endtime[i] - dis_gef_conn[index].duration
      /*  +  MIN_DELTA_TIME * curr_plan_length *50*/;
    
    fprintf (fp, "\t %.5f:  ", start_time);

    a = dis_gop_conn[index].action;  
  
    //printf("index %d\n", index);
    
    if ( a->norm_operator ||
              a->pseudo_action ) {
          fprintf(fp, "(%s", a->name );
          for ( j = 0; j < a->num_name_vars; j++ ) {
              fprintf(fp, " %s", dis_gconstants[a->name_inst_table[j]]);
          } 
          fprintf(fp, ")");
    } 
    //fprintf (fp, "[1.000]\n");
    fprintf (fp, "[%.5f]\n", dis_gef_conn[index].duration);
  } 

  if(dis_isw) {
        start_time = 0.0;
        for(i=0; i<5; i++) {
            index = saved_dis_gnum_op_conn - i -1;
            fprintf (fp, "\t %.5f:  ", start_time);
            a = dis_gop_conn[index].action;  
    
            if ( a->norm_operator ||
              a->pseudo_action ) {
            fprintf(fp, "(%s", a->name );
            for ( j = 0; j < a->num_name_vars; j++ ) {
                fprintf(fp, " %s", dis_gconstants[a->name_inst_table[j]]);
            } 
            fprintf(fp, ")");
            } 
            fprintf (fp, "[%.5f]\n", dis_gef_conn[index].duration);
            if(i>0) start_time += dis_gef_conn[index].duration;
        }
  }
  
  if (fp != stdout)
  {
  fclose (fp);
  fprintf(stderr, "Answer file is %s\n\n", cNameFile);
  }
} 

void dis_print_Modal7tif_ffsol( char *fact_file_name )
{
    dis_fl_pred_PERT();
    dis_print_sched_Modal7tif_ffsol(fact_file_name);
} 

void dis_print_less_PERT_ffsol( char *fact_file_name )
{
    dis_fl_pred_PERT1();
    dis_print_sched_ffsol(fact_file_name);
}

void dis_print_less_PERT_GpG( char *fact_file_name )
{                       
    dis_GpG_to_ffsol();
    dis_print_less_PERT_ffsol(fact_file_name);
}

