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

 * File: stripsff.h

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



#ifndef _STRIPSFF_H
#define _STRIPSFF_H

#define MAX_NUM_TIF 600

typedef struct _TIF {
  int action;
  int pred;
  float begin;
  float end;
}TIF;

typedef struct _s_EfConn {
          
  int *PC;
  int num_PC;
  
  int *A;
  int num_A;
  
  int *D;
  int num_D;
  
}s_EfConn;

extern s_EfConn *s_gef_conn;

void strips_gef_conn();
void save_lpg_gef_conn();
void load_lpg_gef_conn();
void load_ff_gef_conn();
void construct_mff();
void mff_to_lpg();
void mff_start_and_goal(State *start_state, State *end_State);
void print_gderipreds();
void dis_print_IPC4_symmetry( char *fact_file_name );
void dis_print_IPC4( char *fact_file_name );
void dis_print_IPC4_ffop_ffsol( char *fact_file_name );
void dis_print_PERT_ffsol( char *fact_file_name );
void dis_print_less_PERT_ffsol( char *fact_file_name );
void dis_print_sched_ffsol( char *fact_file_name );
void dis_PERT(Bool);
Bool PERT(Bool);
void dis_print_IPC4_ffop_GpG( char *fact_file_name );
void lpg_mff_facts();
void dis_print_Modal7tif_ffsol( char *fact_file_name );
void dis_print_IPC5_GpG( char *fact_file_name );
void dis_print_less_PERT_GpG( char *fact_file_name );
void dis_PERT_GpG();
void notdis_init_timed_initial();
void init_timed_initial();
Bool init_timed_wrapper();

#endif /* _STRIPSFF_H */
