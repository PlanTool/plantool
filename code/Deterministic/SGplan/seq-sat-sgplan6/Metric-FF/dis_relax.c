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

 * File: dis_relax.c 

 * Description: modified from relax.c in Metric-FF

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
 * File: relax.c
 * Description: this file handles the relaxed planning problem, i.e.,
 *              the code is responsible for the heuristic evaluation
 *              of states during search.
 *
 *              --- THE HEART PEACE OF THE FF PLANNER ! ---
 *
 *              here: linear tasks +=,-=,:= / le / le <comp> le
 *
 *
 * Author: Joerg Hoffmann 2001
 *
 *********************************************************************/ 









#include "dis_ff.h"

#include "dis_output.h"
#include "dis_memory.h"

#include "dis_expressions.h"

#include "dis_relax.h"
#include "dis_search.h"
#include "dis_constraints.h"
#include "lpg.h"


/* local globals
 */


extern ConConn *dis_gcon_conn;
extern int dis_gnum_con_conn;




/* fixpoint
 */
int *relax_lF;
int relax_lnum_F;
int *relax_lE;
int relax_lnum_E;

int *relax_lch_E;
int relax_lnum_ch_E;

int *relax_l0P_E;
int relax_lnum_0P_E;





/* 1P extraction
 */
int **relax_lgoals_at;
int *relax_lnum_goals_at;

float **relax_lf_goals_c_at;
dis_Comparator **relax_lf_goals_comp_at;

int relax_lh;

int *relax_lch_F;
int relax_lnum_ch_F;

int *relax_lused_O;
int relax_lnum_used_O;

int *relax_lin_plan_E;
int relax_lnum_in_plan_E;


/* helpful actions numerical helpers
 */
dis_Comparator *relax_relax_lHcomp;
float *relax_lHc;


extern int symm_constraint_eval(dis_State *dS, int goal);
extern void symm_op_constrained(dis_State *S);

extern int find_ef_tif(int, float *, float *);
extern int dis_find_ef_tif(int, float *, float *);
extern int myPERT(int, int, int *, float *);
extern float *relax_time_fact, *relax_time_action;
extern int *support_action, *last_precondition;
extern float violation, makespan, max_time, total_time;
extern int dis_ops[1000], Nop, dead;
extern float schedule[1000], *deadline;
dis_State current_state;
extern int get_state_distance( dis_State *);
extern void dis_collect_H_cg_info(dis_State *, int);
#define START_ACTION 1000000000

/*************************************
 * helper, for -1 == dis_INFINITY method *
 *************************************/












dis_Bool dis_LESS( int a, int b )

{

  if ( a == dis_INFINITY ) {
    return dis_FALSE;
  }

  if ( b == dis_INFINITY ) {
    return dis_TRUE;
  }

  return ( a < b ? dis_TRUE : dis_FALSE );

}














/***********************************
 * FUNCTIONS ACCESSED FROM OUTSIDE *
 ***********************************/




void my_collect_A_info(dis_State *S)
{
    int i,j,k,kk,f,g,a;
    
     static dis_Bool first_call = dis_TRUE;

  if ( first_call ) {
    dis_gA = ( int * ) calloc( dis_gnum_op_conn+5, sizeof( int ) );
    dis_gnum_A = 0;
    first_call = dis_FALSE;
  }
    for ( i = 0; i < dis_gnum_A; i++ ) {
        dis_gop_conn[dis_gA[i]].is_in_A = dis_FALSE;
    }
    dis_gnum_A = 0;
    
    for(i=0; i<S->num_F; i++) {
        f = S->F[i];
           
       /* 
        if((dis_gflogic_goal[dis_gnum_flogic_goal-1] == 169)&&(f==112)) {
                printf("Fact %d ", f);print_ft_name(f); printf("\n");
        }*/               
    
        for(j=0; j<dis_gft_conn[f].num_PC; j++) {
            a = dis_gft_conn[f].PC[j];

            /*
            if((dis_gflogic_goal[dis_gnum_flogic_goal-1] == 169)&&(f==112)) {
                printf("\t");print_op_name(a); printf("\n");
            } */                                          
            
            if (dis_gop_conn[a].is_in_A )  continue;
            for(k=0; k<dis_gef_conn[a].num_PC; k++) {
                g = dis_gef_conn[a].PC[k];
           
               /* 
                if((dis_gflogic_goal[dis_gnum_flogic_goal-1] == 169)&&(f==112)) {
                    printf("\t\t");print_ft_name(g); printf("\n");
                }*/                                                           
            
                for(kk=0; kk<S->num_F; kk++) {
                    if(S->F[kk] == g) break;
                }
                if(kk==S->num_F) break;
            }
            if(k==dis_gef_conn[a].num_PC) {
                dis_gop_conn[a].is_in_A = dis_TRUE;
                dis_gA[dis_gnum_A++] = a;
            }
        }
    }
    
    work_S0.num_F = 0;
    for(i=0; i<dis_gnum_A; i++) {
        work_S0.F[work_S0.num_F++] = dis_gA[i];
        /*
        if(dis_gflogic_goal[dis_gnum_flogic_goal-1] == 169) {
            print_op_name(work_S0.F[i]); printf("\n");
        }*/
    }
    for ( i = 0; i < dis_gnum_A; i++ ) {
        dis_gop_conn[dis_gA[i]].is_in_A = dis_FALSE;
    }
    dis_gnum_A = 0;
    
    symm_op_constrained(&work_S0);
    for(i=0; i<work_S0.num_F; i++) {
        dis_gop_conn[work_S0.F[i]].is_in_A = dis_TRUE;
        dis_gA[dis_gnum_A++] = work_S0.F[i];
    }   
    /*
     if(dis_gflogic_goal[dis_gnum_flogic_goal-1] == 169) {
        printf("\n");
        sleep(1000);
     }*/
}



/* naive implementation of the mneed computation. speedups
 * should be possible by utilizing effective data access, and
 * dynamic programming according to topological sorting wrp to
 * := dependencies.
 * 
 * wonder whether that process is time relevant at all?
 *
 * function is recursive, and will terminate because the :=
 * dependencies are cycle-free.
 */
void dis_get_mneed( int fl, Bool *minusinfty, float *val )

{

  int ef, pc, i, fl_, c, in, g, as;
  float val_, mneed_;
  Bool minusinfty_;

  /* this counts the number of hits; 0 --> mneed is -infty
   */
  c = 0;
  /* check through the actions, i.e. effects. I suppose this could be made
   * *a lot* faster by checking through the PC information of the fl;
   * additionally, we'd need fast access to the art. fluents that fl 
   * participates in.
   */
  for ( ef = 0; ef < dis_gnum_ef_conn; ef++ ) {
    /* the preconds must be supported above their required value.
     */
    for ( pc = 0;  pc < dis_gef_conn[ef].num_f_PC; pc++ ) {
      /* constraint here is gef_conn[ef].f_PC_fl[pc] >= [>] gef_conn[ef].f_PC_c[pc]
       * where lh side can be lnf expression.
       */
      fl_ = dis_gef_conn[ef].f_PC_fl[pc];
      if ( fl_ < 0 ) continue;
      if ( fl_ == fl ) {
	c++;
	val_ = dis_gef_conn[ef].f_PC_c[pc];
	if ( c == 1 || val_ > *val ) {
	  *val = val_;
	}
	continue;
      }
      if ( !dis_gfl_conn[fl_].artificial ) continue;
      for ( i = 0; i < dis_gfl_conn[fl_].num_lnf; i++ ) {
	if ( dis_gfl_conn[fl_].lnf_F[i] == fl ) {
	  /* might be that the expression can't be supported --
	   * if one of the fluents is undefined.
	   */
	  if ( dis_supv( &val_, fl, fl_, dis_gef_conn[ef].f_PC_c[pc] ) ) {
	    c++;
	    if ( c == 1 || val_ > *val ) {
	      *val = val_;
	    }
	  }
	  break;
	}
      }
    }

    /* the += effs must be supported above 0.
     */
    for ( in = 0; in < dis_gef_conn[ef].num_IN; in++ ) {
      /* += eff here is gef_conn[ef].IN_fl[in] += gef_conn[ef].IN_fl_[in] + 
       *                                          gef_conn[ef].IN_c[in]
       * where gef_conn[ef].IN_fl_[in] can be lnf expression.
       */
      /* relevance...
       */
      if ( !dis_gfl_conn[dis_gef_conn[ef].IN_fl[in]].relevant ) {
	continue;
      }
      fl_ = dis_gef_conn[ef].IN_fl_[in];
      if ( fl_ < 0 ) continue;
      if ( fl_ == fl ) {
	c++;
	val_ = (-1) * dis_gef_conn[ef].IN_c[in];
	if ( c == 1 || val_ > *val ) {
	  *val = val_;
	}
	continue;
      }
      if ( !dis_gfl_conn[fl_].artificial ) continue;
      for ( i = 0; i < dis_gfl_conn[fl_].num_lnf; i++ ) {
	if ( dis_gfl_conn[fl_].lnf_F[i] == fl ) {
	  if ( dis_supv( &val_, fl, fl_, (-1) * dis_gef_conn[ef].IN_c[in] ) ) {
	    c++;
	    if ( c == 1 || val_ > *val ) {
	      *val = val_;
	    }
	  }
	  break;
	}
      }
    }

    /* the := effs must be supported above the mneed value of the affected
     * varuable..
     */
    for ( as = 0; as < dis_gef_conn[ef].num_AS; as++ ) {
      /* := eff here is gef_conn[ef].AS_fl[as] := gef_conn[ef].AS_fl_[as] + 
       *                                          gef_conn[ef].AS_c[as]
       * where gef_conn[ef].AS_fl_[as] can be lnf expression.
       */
      /* relevance...
       */
      if ( !dis_gfl_conn[dis_gef_conn[ef].AS_fl[as]].relevant ) {
	continue;
      }
      /* after this, -infty handling is actually superfluous... or so I'd suppose...
       */
      fl_ = dis_gef_conn[ef].AS_fl_[as];
      if ( fl_ < 0 ) continue;
      if ( fl_ == fl ) {
	dis_get_mneed( dis_gef_conn[ef].AS_fl[as], &minusinfty_, &mneed_ );
	if ( !minusinfty_ ) {
	  c++;
	  val_ = mneed_ - dis_gef_conn[ef].AS_c[as];
	  if ( c == 1 || val_ > *val ) {
	    *val = val_;
	  }
	}
	continue;
      }
      if ( !dis_gfl_conn[fl_].artificial ) continue;
      for ( i = 0; i < dis_gfl_conn[fl_].num_lnf; i++ ) {
	if ( dis_gfl_conn[fl_].lnf_F[i] == fl ) {
	  dis_get_mneed( dis_gef_conn[ef].AS_fl[as], &minusinfty_, &mneed_ );
	  if ( !minusinfty_ ) {
	    if ( dis_supv( &val_, fl, fl_, mneed_ - dis_gef_conn[ef].AS_c[as] ) ) {
	      c++;
	      if ( c == 1 || val_ > *val ) {
		*val = val_;
	      }
	    }
	    break;
	  }
	}
      }
    }

  }/* end of ef loop */

  /* check through the numerical goals.
   */
  for ( g = 0; g < dis_gnum_fnumeric_goal; g++ ) {
    /* constraint here is gfnumeric_goal_fl[g] >= [>] gfnumeric_goal_c[g]
     * where lh side can be lnf expression.
     */
    fl_ = dis_gfnumeric_goal_fl[g];
    if ( fl_ < 0 ) continue;
    if ( fl_ == fl ) {
      c++;
      val_ = dis_gfnumeric_goal_c[g];
      if ( c == 1 || val_ > *val ) {
	*val = val_;
      }
    }
    if ( !dis_gfl_conn[fl_].artificial ) continue;
    for ( i = 0; i < dis_gfl_conn[fl_].num_lnf; i++ ) {
      if ( dis_gfl_conn[fl_].lnf_F[i] == fl ) {
	if ( dis_supv( &val_, fl, fl_, dis_gfnumeric_goal_c[g] ) ) {
	  c++;
	  if ( c == 1 || val_ > *val ) {
	    *val = val_;
	  }
	}
	break;
      }
    }
  }

  *minusinfty = ( c == 0 ) ? TRUE : FALSE;

}   



Bool dis_supv( float *val, int fl, int expr, float c_ )

{

  int i, pos = 0;

  *val = c_;

  for ( i = 0; i < dis_gfl_conn[expr].num_lnf; i++ ) {
    if ( dis_gfl_conn[expr].lnf_F[i] == fl ) {
      pos = i;
      continue;
    }
    if ( !dis_gmneed_start_D[dis_gfl_conn[expr].lnf_F[i]] ) {
      /* this expression contains an undef fluent -->
       * no matter how much we increase fl, it won't help
       * at all.
       */
      return FALSE;
    }
    *val -= dis_gfl_conn[expr].lnf_C[i] * 
      dis_gmneed_start_V[dis_gfl_conn[expr].lnf_F[i]];
  }

  /* this here is > 0 (hopefully -- yes, checked that)
   */
  *val /= dis_gfl_conn[expr].lnf_C[pos];

  return TRUE;

}










int dis_get_1P( dis_State *S )

{

  int max, h;
  dis_Bool solvable;
  
  if(SymmLagrangian==1) {
   // return h+SymmLagrange*symm_constraint_eval(&dis_ginitial_state, dis_gflogic_goal[dis_gnum_flogic_goal-1]);
    return symm_constraint_eval(S, 
            dis_gflogic_goal[dis_gnum_flogic_goal-1]);
  }
  
  dis_gevaluated_states++;

  solvable = dis_build_fixpoint( S, &max );

  /*
  if ( dis_gcmd_line.display_info == 126 ) {
    dis_print_fixpoint_result();
  }*/
  
  if ( solvable ) {
    h = dis_extract_1P( max );
  } else {
    h = dis_INFINITY;
    makespan = dis_BIG_INT;
    violation = dis_BIG_INT;
  }

  dis_reset_fixpoint( max );
  

  return h;

}

 

int dis_get_1P_and_H( dis_State *S )

{

  int max, h;
  dis_Bool solvable;

  dis_gevaluated_states++;

  solvable = dis_build_fixpoint( S, &max );
  
  if ( dis_gcmd_line.display_info == 126 ) {
    dis_print_fixpoint_result();
  }

  if ( solvable ) {
    h = dis_extract_1P( max );
    dis_collect_H_info();
  } else {
    h = dis_INFINITY;
    makespan = dis_BIG_INT;
    violation = dis_BIG_INT;
  }

  dis_reset_fixpoint( max );

  if(SymmLagrangian==1) {
    return symm_constraint_eval(S, 
            dis_gflogic_goal[dis_gnum_flogic_goal-1]);
  } else {
    return h;
  }
}



int dis_get_1P_and_A( dis_State *S )

{

  int max, h;
  dis_Bool solvable;

  dis_gevaluated_states++;

  /*
  if ( dis_gcmd_line.display_info == 126 ) {
    dis_print_fixpoint_result();
  }*/


  if(SymmLagrangian!=1) {
    solvable = dis_build_fixpoint( S, &max );
    if ( solvable ) {
        h = dis_extract_1P( max );
    } else {
        h = dis_INFINITY;
// Chih-Wei
    makespan = dis_BIG_INT;
    violation = dis_BIG_INT;
    }
    dis_collect_A_info();
    dis_reset_fixpoint( max );
    return h;
  }


//  if(SymmLagrangian==1) {
	else {
     //my_collect_A_info(S);
    solvable = dis_build_fixpoint( S, &max );
    if ( solvable ) {
        h = symm_constraint_eval(S,
            dis_gflogic_goal[dis_gnum_flogic_goal-1]);
    } else {                
        h = dis_INFINITY;
    }
    dis_collect_A_info();
    dis_reset_fixpoint( max );
    return h;    
  } 
}



void dis_get_A( dis_State *S )

{

  int i;

  dis_initialize_fixpoint( S );
  
  for ( i = 0; i < relax_lnum_F; i++ ) {
    dis_activate_ft( relax_lF[i], 0 );
  }
  for ( i = 0; i < relax_lnum_0P_E; i++ ) {
    if ( dis_gef_conn[relax_l0P_E[i]].in_E ) {
      continue;
    }
    dis_new_ef( relax_l0P_E[i] );
  }
  for ( i = 0; i < dis_gnum_fl_conn; i++ ) {
    dis_activate_fl( i, 0 );
  }

  dis_collect_A_info();

  /* 0 should be enough here...
   */
  dis_reset_fixpoint( 1 );

}



void dis_collect_A_info( void )

{

  static dis_Bool first_call = dis_TRUE;
    
  int i;

  if ( first_call ) {
    dis_gA = ( int * ) calloc( dis_gnum_op_conn+5, sizeof( int ) );
    dis_gnum_A = 0;
    first_call = dis_FALSE;
  }

  for ( i = 0; i < dis_gnum_A; i++ ) {
    dis_gop_conn[dis_gA[i]].is_in_A = dis_FALSE;
  }
  dis_gnum_A = 0;

  if(SymmLagrangian!=1) {
    for ( i = 0; i < relax_lnum_E; i++ ) {
        if ( dis_gef_conn[relax_lE[i]].level != 0 ) break;
        if (dis_gop_conn[dis_gef_conn[relax_lE[i]].op].is_in_A ) {
            continue;
        }
        dis_gop_conn[dis_gef_conn[relax_lE[i]].op].is_in_A = dis_TRUE;
        dis_gA[dis_gnum_A++] = dis_gef_conn[relax_lE[i]].op;
    }
  } else {
    work_S0.num_F = 0; 
    for ( i = 0; i < relax_lnum_E; i++ ) {
        if ( dis_gef_conn[relax_lE[i]].level != 0 ) break;
        if (dis_gop_conn[dis_gef_conn[relax_lE[i]].op].is_in_A ) {
         //   printf("cut"):print_op(dis_gef_conn[relax_lE[i]].op);
            continue;
        }
        work_S0.F[work_S0.num_F++] = dis_gef_conn[relax_lE[i]].op;
      //  print_op(dis_gef_conn[relax_lE[i]].op);
    }
    //printf("\n");
    symm_op_constrained(&work_S0);
    for(i=0; i<work_S0.num_F; i++) {
        dis_gop_conn[work_S0.F[i]].is_in_A = dis_TRUE;
        dis_gA[dis_gnum_A++] = work_S0.F[i];
    }
  }
}

















/*******************************
 * RELAXED FIXPOINT ON A STATE *
 *******************************/







dis_Bool dis_all_known_iga_activated( int time )
{
      int i;
      
      for ( i = 0; i < dis_known_iga_list.num_F; i++ ) {
          if ( !dis_gft_conn[dis_known_iga_list.F[i]].in_F ) {
              return dis_FALSE;
          }
      }

      for ( i = 0; i < dis_known_iga_list.num_F; i++ ) {
          if ( dis_gft_conn[ dis_known_iga_list.F[i]].level == dis_INFINITY ) {
              dis_gft_conn[ dis_known_iga_list.F[i]].level = time;
          }
      }

      return dis_TRUE;
}

dis_Bool dis_build_fixpoint( dis_State *S, int *max )

{

  int start_ft, stop_ft, start_ef, stop_ef, i, time = 0;

  dis_initialize_fixpoint( S );

  start_ft = 0;
  start_ef = 0;
  while ( dis_TRUE ) {
     
     if(SymmLagrangian==1) {
        if(time>0) break;
     }
      
    //if ( dis_all_goals_activated( time ) ) {
    if ( dis_all_known_iga_activated( time ) &&
            dis_all_goals_activated( time ) ) {
      break;
    }
    if ( start_ft == relax_lnum_F ) {
      if ( dis_fluents_hopeless( time ) ) {
	/* fixpoint, goals not reached
	 */
	*max = time;
	return dis_FALSE;
      }
    }
    /* make space if necessary, and copy over
     * info from time to time+1 for fluents
     */
    dis_extend_fluent_levels( time );
    for ( i = 0; i < dis_gnum_fl_conn; i++ ) {
      if ( dis_gfl_conn[i].def[time] ) {
	dis_gfl_conn[i].def[time+1] = dis_TRUE;
	dis_gfl_conn[i].level[time+1] = dis_gfl_conn[i].level[time];
      }
    }

    /* determine the next effect layer:
     * - activate the facts
     * - if level 0 activate the no preconds-ops
     * - activate the fluents at their <time> level
     */
    stop_ft = relax_lnum_F;
    for ( i = start_ft; i < stop_ft; i++ ) {
      dis_activate_ft( relax_lF[i], time );
    }
    if ( time == 0 ) {
      for ( i = 0; i < relax_lnum_0P_E; i++ ) {
	if ( dis_gef_conn[relax_l0P_E[i]].in_E ) {
	  continue;
	}
	dis_new_ef( relax_l0P_E[i] );
      }
    }
    for ( i = 0; i < dis_gnum_fl_conn; i++ ) {
      dis_activate_fl( i, time );
    }

    /* now say what the benefits of applying the new
     * effect layer are:
     * 
     * the facts,
     * plus the new fluent levels at time + 1.
     */
    stop_ef = relax_lnum_E;
    for ( i = start_ef; i < stop_ef; i++ ) {
      dis_activate_ef( relax_lE[i], time );
    }
    /* on top of what the new effects have added to the fluents, all effects 
     * strictly below <time> can be applied again. Their effect might be 
     * different from what they've done before, as it might depend on
     * the rh fluent fl_
     */
    if ( time > 0 ) {
      for ( i = 0; i < start_ef; i++ ) {
	dis_apply_ef( relax_lE[i], time );
      }
    }
    /* now check whether there became any assigner applicable that is
     * mightier than all the increasers put together.
     *
     * this needs only be done for the non-artificial
     * fluents as the others obviously don't get
     * assigned at all.
     */
    for ( i = 0; i < dis_gnum_real_fl_conn; i++ ) {
      if ( !dis_gfl_conn[i].curr_assigned ) {
	/* no assigner in yet
	 */
	continue;
      }
      if ( !dis_gfl_conn[i].def[time] ) {
	dis_gfl_conn[i].def[time+1] = dis_TRUE;
	dis_gfl_conn[i].level[time+1] = dis_gfl_conn[i].curr_max_assigned;
	continue;
      }
      if ( dis_gfl_conn[i].curr_max_assigned > dis_gfl_conn[i].level[time+1] ) {
	dis_gfl_conn[i].level[time+1] = dis_gfl_conn[i].curr_max_assigned;
      }
    }
    /* finally, determine the new levels of the artificial fluents
     * at the new time step.
     */
    dis_determine_artificial_fl_levels( time + 1 );

    start_ft = stop_ft;
    start_ef = stop_ef;
    time++;
  }

  *max = time;
   
  if(SymmLagrangian==1) {
    return dis_TRUE;
  } else {
  if ( dis_all_known_iga_activated( time ) &&
            dis_all_goals_activated( time ) ) {
        return dis_TRUE;
  } else {
     printf("       dis_known blocking\n");  
      return dis_FALSE;
  }
  }
}



dis_Bool dis_fluents_hopeless( int time )

{

  int i, j;
  Bool minusinfty;
  float mneed;
  
  /* if no real fluent has improved from the previous step to this one,
   * then - with facts not improving either - the process has reached a fixpoint.
   *
   * for the formal details of this, ie. the mneed values, see the JAIR article.
   */

  if ( time == 0 ) {
    /* nothing has happened yet...
     */
    return dis_FALSE;
  }

  for ( i = 0; i < dis_gnum_real_fl_conn; i++ ) {
    dis_gmneed_start_D[i] = dis_gfl_conn[i].def[time-1];
    dis_gmneed_start_V[i] = dis_gfl_conn[i].level[time-1];
  }

  for ( i = 0; i < dis_gnum_real_fl_conn; i++ ) {
    if ( !dis_gfl_conn[i].def[time-1] &&
	 !dis_gfl_conn[i].def[time] ) {
      /* this one has obviously not been made any better
       */
      continue;
    }
    if ( dis_gfl_conn[i].def[time-1] &&
	 dis_gfl_conn[i].level[time] == dis_gfl_conn[i].level[time-1] ) {
      /* this one has not improved either
       */
      continue;
    }
    dis_get_mneed( i, &minusinfty, &mneed );
    if ( dis_gcmd_line.display_info == 333 ) {
      printf("\nstart values:");
      for ( j = 0; j < dis_gnum_real_fl_conn; j++ ) {
	printf("\n"); dis_print_fl_name( j ); 
	printf(" --- %d, %f", dis_gmneed_start_D[j], dis_gmneed_start_V[j]);
      }
      printf("\nmneed "); dis_print_fl_name( i ); 
      printf(" --- %d, %f", minusinfty, mneed);
    }

    if ( minusinfty ) {
      /* this one is not needed at all
       */
      continue;
    }
    if ( dis_gfl_conn[i].def[time-1] && dis_gfl_conn[i].level[time-1] > mneed ) {
      /* here we already had a sufficient value at the last layer.
       */
      continue;
    }
    return FALSE;
  }

  return dis_TRUE;
    
}



void dis_initialize_fixpoint( dis_State *S )

{

  static dis_Bool first_call = dis_TRUE;

  int i, j, k;

  if ( first_call ) {
    relax_time_fact = (float *) malloc(sizeof(float)*dis_gnum_ft_conn);   
    relax_time_action = (float *) malloc(sizeof(float)*dis_gnum_ef_conn); 
    support_action = (int *) malloc(sizeof(int)*dis_gnum_ft_conn);        
    last_precondition = (int *) malloc(sizeof(int)*dis_gnum_ef_conn);     
         
   /* make initial space for fluent levels
     */
    dis_extend_fluent_levels( -1 );

    /* get memory for local globals
     */
    relax_lF = ( int * ) calloc( dis_gnum_ft_conn, sizeof( int ) );
    relax_lE = ( int * ) calloc( dis_gnum_ef_conn+5, sizeof( int ) );
    relax_lch_E = ( int * ) calloc( dis_gnum_ef_conn+5, sizeof( int ) );
    relax_l0P_E = ( int * ) calloc( dis_gnum_ef_conn+5, sizeof( int ) );
    
    /* initialize connectivity graph members for
     * relaxed planning
     */
    relax_lnum_0P_E = 0;
    for ( i = 0; i < dis_gnum_ef_conn; i++ ) {      
      dis_gef_conn[i].level = dis_INFINITY;    
      dis_gef_conn[i].in_E = dis_FALSE;
      dis_gef_conn[i].num_active_PCs = 0;
      dis_gef_conn[i].ch = dis_FALSE;

      dis_gef_conn[i].num_active_f_PCs = 0;
      
      if ( dis_gef_conn[i].num_PC == 0 &&
	   dis_gef_conn[i].num_f_PC == 0 &&
	   !dis_gef_conn[i].illegal ) {
	relax_l0P_E[relax_lnum_0P_E++] = i;
      }
    }
    for ( i = 0; i < dis_gnum_op_conn; i++ ) {      
      dis_gop_conn[i].is_in_A = dis_FALSE;
      dis_gop_conn[i].is_in_H = dis_FALSE;
    }
    for ( i = 0; i < dis_gnum_ft_conn; i++ ) {
      dis_gft_conn[i].level = dis_INFINITY;
      dis_gft_conn[i].in_F = dis_FALSE;
    }
    for ( i = 0; i < dis_gnum_fl_conn; i++ ) {
      dis_gfl_conn[i].curr_assigned = dis_FALSE;
    }
    first_call = dis_FALSE;
  }

  for (i=0;i<dis_gnum_ef_conn;i++)
  {
    relax_time_action[i] = dis_INFINITY;
    last_precondition[i] = -1;
  }
  for (i=0;i<dis_gnum_ft_conn;i++)
  {
    relax_time_fact[i] = dis_INFINITY;
    support_action[i] = -1;
  }

  relax_lnum_E = 0;
  relax_lnum_ch_E = 0;

  relax_lnum_F = 0;
  for ( i = 0; i < S->num_F; i++ ) {
    if ( dis_gft_conn[S->F[i]].in_F ) {
      continue;
    }

    if ((GpG.SearchModal == -103 && GpG.is_durative) || num_dur == -1 || GpG.is_til)
    {
      dis_make_state(&current_state, S->num_F, 0);
      for (current_state.num_F=0;current_state.num_F<S->num_F;current_state.num_F++)
        current_state.F[current_state.num_F] = S->F[current_state.num_F];
      if (GpG.is_til && GpG.SearchModal != -103)
      {
        j = schedule_actions(Nop, dis_ops, schedule, NULL);
        if (j)
        {
        relax_time_fact[S->F[i]] = 0;
        support_action[S->F[i]] = START_ACTION;
        for (j=0;j<Nop;j++)
          for (k=0;k<dis_gef_conn[dis_ops[j]].num_A;k++)
            if (S->F[i] == dis_gef_conn[dis_ops[j]].A[k])
              if (relax_time_fact[S->F[i]] < schedule[j] + dis_gef_conn[dis_ops[j]].duration)
              {
                relax_time_fact[S->F[i]] = schedule[j] + dis_gef_conn[dis_ops[j]].duration;
                support_action[S->F[i]] = dis_ops[j];
              }
        }
        else
        {
          relax_time_fact[S->F[i]] = dis_BIG_INT;
          support_action[S->F[i]] = START_ACTION;
        }
      }          
      else
      {
      myPERT(Nop, 0, dis_ops, schedule);
      if (GpG.SearchModal == -103)
      for (j=0;j<Nop;j++)
        schedule[j] += dis_gef_conn[dis_ops[j]].duration;
      relax_time_fact[S->F[i]] = 0;
      support_action[S->F[i]] = START_ACTION;
      if (GpG.SearchModal == -103)
      {
        for (j=0;j<Nop;j++)
          for (k=0;k<dis_gef_conn[dis_ops[j]].num_A;k++)
            if (S->F[i] == dis_gef_conn[dis_ops[j]].A[k])
              {
              if (relax_time_fact[S->F[i]] < schedule[j])
              {
                relax_time_fact[S->F[i]] = schedule[j];
                support_action[S->F[i]] = dis_ops[j];
              }
              }
      }
      else
      {
        for (j=0;j<Nop;j++)
          for (k=0;k<dis_gef_conn[dis_ops[j]].num_A;k++)
            if (S->F[i] == dis_gef_conn[dis_ops[j]].A[k])
              {
              if (relax_time_fact[S->F[i]] < schedule[j] + dis_gef_conn[dis_ops[j]].duration)
              {
                relax_time_fact[S->F[i]] = schedule[j] + dis_gef_conn[dis_ops[j]].duration;
                support_action[S->F[i]] = dis_ops[j];
              }
              }
      }
      }
    }
    dis_new_fact( S->F[i] );
  }

  /* only the real fls are ever in there
   */
  for ( i = 0; i < dis_gnum_real_fl_conn; i++ ) {
    if ( !S->f_D[i] ) {
      continue;
    }
    dis_gfl_conn[i].def[0] = dis_TRUE;
    dis_gfl_conn[i].level[0] = S->f_V[i];
  }
  /* now set the art. values from that.
   */
  dis_determine_artificial_fl_levels( 0 );

}



void dis_determine_artificial_fl_levels( int time )

{

  int i, j;
  float l;

  /* for all art. fls
   */
  for ( i = dis_gnum_real_fl_conn; i < dis_gnum_fl_conn; i++ ) {
    l = 0;
    for ( j = 0; j < dis_gfl_conn[i].num_lnf; j++ ) {
      if ( !dis_gfl_conn[dis_gfl_conn[i].lnf_F[j]].def[time] ) break;
      l += (dis_gfl_conn[i].lnf_C[j] * dis_gfl_conn[dis_gfl_conn[i].lnf_F[j]].level[time]);
    }
    if ( j < dis_gfl_conn[i].num_lnf ) {
      /* one part yet undefined.
       */
      continue;
    } else {
      dis_gfl_conn[i].def[time] = dis_TRUE;
      dis_gfl_conn[i].level[time] = l;
    }
  }

}



void dis_extend_fluent_levels( int time )

{

  static int highest_seen;

  dis_Bool *b;
  float *f1;

  int i, j;

  if ( time == -1 ) {
    highest_seen = dis_RELAXED_STEPS_DEFAULT;
    for ( i = 0; i < dis_gnum_fl_conn; i++ ) {
      dis_gfl_conn[i].def = ( dis_Bool * ) calloc( highest_seen, sizeof( dis_Bool ) );
      for ( j = 0; j < highest_seen; j++ ) {
	dis_gfl_conn[i].def[j] = dis_FALSE;
      }
      dis_gfl_conn[i].level = ( float * ) calloc( highest_seen, sizeof( float ) );
    }
    return;
  }

  if ( time + 1 < highest_seen ) return;

  b = ( dis_Bool * ) calloc( time + 1, sizeof( dis_Bool ) );
  f1 = ( float * ) calloc( time + 1, sizeof( float ) );

  highest_seen = time + 10;
  for ( i = 0; i < dis_gnum_fl_conn; i++ ) {
    for ( j = 0; j <= time; j++ ) {
      b[j] = dis_gfl_conn[i].def[j];
      f1[j] = dis_gfl_conn[i].level[j];
    }

    xfree( dis_gfl_conn[i].def );
    xfree( dis_gfl_conn[i].level );
    dis_gfl_conn[i].def = ( dis_Bool * ) calloc( highest_seen, sizeof( dis_Bool ) );
    dis_gfl_conn[i].level = ( float * ) calloc( highest_seen, sizeof( float ) );

    for ( j = 0; j <= time; j++ ) {
      dis_gfl_conn[i].def[j] = b[j];
      dis_gfl_conn[i].level[j] = f1[j];
    }
    for ( j = time + 1; j < highest_seen; j++ ) {
      dis_gfl_conn[i].def[j] = dis_FALSE;
    }
  }

  xfree( b );
  xfree( f1 );

}



void dis_activate_ft( int index, int time )

{

  int i;

  dis_gft_conn[index].level = time;

  for ( i = 0; i < dis_gft_conn[index].num_PC; i++ ) {
    /* never activate illegal effects.
     */
    if ( dis_gef_conn[dis_gft_conn[index].PC[i]].illegal ) continue;

    dis_gef_conn[dis_gft_conn[index].PC[i]].num_active_PCs++;
    if ( !dis_gef_conn[dis_gft_conn[index].PC[i]].ch ) {
      dis_gef_conn[dis_gft_conn[index].PC[i]].ch = dis_TRUE;
      relax_lch_E[relax_lnum_ch_E++] = dis_gft_conn[index].PC[i];
    }
    if ( dis_gef_conn[dis_gft_conn[index].PC[i]].num_active_PCs ==
	 dis_gef_conn[dis_gft_conn[index].PC[i]].num_PC &&
	 dis_gef_conn[dis_gft_conn[index].PC[i]].num_active_f_PCs ==
	 dis_gef_conn[dis_gft_conn[index].PC[i]].num_f_PC ) {
      dis_new_ef( dis_gft_conn[index].PC[i] );
    }
  }

}



void dis_activate_fl( int index, int time )

{

  int i, ef;

  if ( !dis_gfl_conn[index].def[time] ) return;

  for ( i = 0; i < dis_gfl_conn[index].num_PC; i++ ) {
    ef = dis_gfl_conn[index].PC[i];
    if ( dis_gef_conn[ef].illegal ) continue;

    if ( dis_gef_conn[ef].f_PC_direct_comp[index] == IGUAL ) {
      printf("\n\nprec of addressed ef does not care about fl!\n\n");
      exit( 1 );
    }
    if ( !dis_number_comparison_holds( dis_gef_conn[ef].f_PC_direct_comp[index],
				   dis_gfl_conn[index].level[time],
				   dis_gef_conn[ef].f_PC_direct_c[index] ) ) {
      /* the level is not yet high enough for this one
       */
      continue;
    }
    if ( time > 0 &&
	 dis_gfl_conn[index].def[time-1] &&
	 dis_number_comparison_holds( dis_gef_conn[ef].f_PC_direct_comp[index],
				  dis_gfl_conn[index].level[time-1],
				  dis_gef_conn[ef].f_PC_direct_c[index] ) ) {
      /* last time this was already in: that one's old!
       * do not count it!
       */
      continue;
    }
    dis_gef_conn[ef].num_active_f_PCs++;
    if ( !dis_gef_conn[ef].ch ) {
      dis_gef_conn[ef].ch = dis_TRUE;
      relax_lch_E[relax_lnum_ch_E++] = ef;
    }
    if ( dis_gef_conn[ef].num_active_PCs == dis_gef_conn[ef].num_PC &&
	 dis_gef_conn[ef].num_active_f_PCs == dis_gef_conn[ef].num_f_PC ) {
      dis_new_ef( ef );
    }
  }

}



void dis_activate_ef( int index, int time )

{

  int i, fl, res;
  float val, begin, end;

//  if ( dis_gef_conn[index].removed ) {
//    printf("\n\nactivating removed effect!!\n\n");
//    exit( 1 );
//  }
  if ( dis_gef_conn[index].illegal ) {
    printf("\n\nactivating illegal effect!!\n\n");
    exit( 1 );
  }

  if ((GpG.SearchModal == -103 && GpG.is_durative) || num_dur == -1 || GpG.is_til)
  {
    relax_time_action[index] = 0;
    for (i=0;i<dis_gef_conn[index].num_PC;i++)
      if (relax_time_action[index] < relax_time_fact[dis_gef_conn[index].PC[i]] + MIN_DELTA_TIME)
      {
        relax_time_action[index] = relax_time_fact[dis_gef_conn[index].PC[i]] + MIN_DELTA_TIME;
        last_precondition[index] = dis_gef_conn[index].PC[i];
      }
    if (GpG.SearchModal == -103 || GpG.is_til)
    {
    if (GpG.SearchModal == -103)
    res = find_ef_tif(index, &begin, &end);
    else
      res = dis_find_ef_tif(index, &begin, &end);
    if (relax_time_action[index] > end /*|| relax_time_action[index] > max_time*/)
    {
      relax_time_action[index] = dis_INFINITY;
      last_precondition[index] = -1;
      return;
    }
    if (relax_time_action[index] < begin)
      relax_time_action[index] = begin;
    }
  }

  dis_gef_conn[index].level = time;

  for ( i = 0; i < dis_gef_conn[index].num_A; i++ ) {
  if ((GpG.SearchModal == -103 && GpG.is_durative) || num_dur == -1 || GpG.is_til)
  {
    if (relax_time_fact[dis_gef_conn[index].A[i]] > relax_time_action[index] + dis_gef_conn[index].duration)
    {
      relax_time_fact[dis_gef_conn[index].A[i]] = relax_time_action[index] + dis_gef_conn[index].duration;
      support_action[dis_gef_conn[index].A[i]] = index;
    }
  }
    if ( dis_gft_conn[dis_gef_conn[index].A[i]].in_F ) {
      continue;
    }
    dis_new_fact( dis_gef_conn[index].A[i] );
  }

  for ( i = 0; i < dis_gef_conn[index].num_IN; i++ ) {
    fl = dis_gef_conn[index].IN_fl[i];
    if ( !dis_gfl_conn[fl].def[time] ) {
      /* in principle, we could skip this action here as
       * it affects a fluent that is yet undefined.
       *
       * ...seems difficult to integrate into implementation,
       * does not matter much probably (?): so we relax
       * the task further in the sense that we allow this action
       * here, only it has of course no effect on the fluent.
       */
      continue;
    }
    /* value is either the constant, or the (artificial lnf?) fl_ level
     * at <time> plus the constant.
     */
    if ( dis_gef_conn[index].IN_fl_[i] == -1 ) {
      val = dis_gef_conn[index].IN_c[i];
    } else {
      if ( !dis_gfl_conn[dis_gef_conn[index].IN_fl_[i]].def[time] ) {
	/* this one does not help us here.
	 */
	continue;
      }
      val = dis_gfl_conn[dis_gef_conn[index].IN_fl_[i]].level[time] + dis_gef_conn[index].IN_c[i];
    }
    /* we only consider the effect if it helps us.
     */
    if ( val > 0 ) {
      dis_gfl_conn[fl].level[time+1] += val;
    }
  }

  /* the assigners are remembered in a parallel sort of way...
   */
  for ( i = 0; i < dis_gef_conn[index].num_AS; i++ ) {
    fl = dis_gef_conn[index].AS_fl[i];
    if ( dis_gef_conn[index].AS_fl_[i] == -1 ) {
      val = dis_gef_conn[index].AS_c[i];
    } else {
      if ( !dis_gfl_conn[dis_gef_conn[index].AS_fl_[i]].def[time] ) {
	/* this one does not help us here.
	 */
	continue;
      }
      val = dis_gfl_conn[dis_gef_conn[index].AS_fl_[i]].level[time] + dis_gef_conn[index].AS_c[i];
    }
    if ( dis_gfl_conn[fl].curr_assigned ) {
      if ( dis_gfl_conn[fl].curr_max_assigned < val ) {
	dis_gfl_conn[fl].curr_max_assigned = val;
      }
    } else {
      dis_gfl_conn[fl].curr_assigned = dis_TRUE;
      dis_gfl_conn[fl].curr_max_assigned = val;
    }
  }

}



/* this one is used to apply effects already there
 * at later time steps - necessary because their
 * numeric effects might have changed.
 */
void dis_apply_ef( int index, int time )

{

  int i, fl;
  float val;

//  if ( dis_gef_conn[index].removed ) {
//    printf("\n\napplying removed effect!!\n\n");
//    exit( 1 );
//  }
  if ( dis_gef_conn[index].illegal ) {
    return;
  }

  /* only numerical effects matter.
   */
  for ( i = 0; i < dis_gef_conn[index].num_IN; i++ ) {
    fl = dis_gef_conn[index].IN_fl[i];
    if ( !dis_gfl_conn[fl].def[time] ) {
      continue;
    }
    if ( dis_gef_conn[index].IN_fl_[i] == -1 ) {
      val = dis_gef_conn[index].IN_c[i];
    } else {
      if ( !dis_gfl_conn[dis_gef_conn[index].IN_fl_[i]].def[time] ) {
	/* no effect here.
	 */
	continue;
      }
      val = dis_gfl_conn[dis_gef_conn[index].IN_fl_[i]].level[time] + dis_gef_conn[index].IN_c[i];
    }
    if ( val > 0 ) {
      dis_gfl_conn[fl].level[time+1] += val;
    }
  }

  for ( i = 0; i < dis_gef_conn[index].num_AS; i++ ) {
    fl = dis_gef_conn[index].AS_fl[i];
    if ( dis_gef_conn[index].AS_fl_[i] == -1 ) {
      val = dis_gef_conn[index].AS_c[i];
    } else {
      if ( !dis_gfl_conn[dis_gef_conn[index].AS_fl_[i]].def[time] ) {
	/* no effect here.
	 */
	continue;
      }
      val = dis_gfl_conn[dis_gef_conn[index].AS_fl_[i]].level[time] + dis_gef_conn[index].AS_c[i];
    }
    if ( dis_gfl_conn[fl].curr_assigned ) {
      if ( dis_gfl_conn[fl].curr_max_assigned < val ) {
	dis_gfl_conn[fl].curr_max_assigned = val;
      }
    } else {
      dis_gfl_conn[fl].curr_assigned = dis_TRUE;
      dis_gfl_conn[fl].curr_max_assigned = val;
    }
  }

}



void dis_new_fact( int index )

{

  relax_lF[relax_lnum_F++] = index;
  dis_gft_conn[index].in_F = dis_TRUE;

}



void dis_new_ef( int index )

{
  if (dis_red_space && dis_gef_conn[index].red_level < 0)
    return;
  relax_lE[relax_lnum_E++] = index;
  dis_gef_conn[index].in_E = dis_TRUE;

}



void dis_reset_fixpoint( int max )

{

  int i, j;

  for ( i = 0; i < relax_lnum_F; i++ ) {
    dis_gft_conn[relax_lF[i]].level = dis_INFINITY;
    dis_gft_conn[relax_lF[i]].in_F = dis_FALSE;
  }

  for ( i = 0; i < relax_lnum_E; i++ ) {
    dis_gef_conn[relax_lE[i]].level = dis_INFINITY;
    dis_gef_conn[relax_lE[i]].in_E = dis_FALSE;
  }

  for ( i = 0; i < relax_lnum_ch_E; i++ ) {
    dis_gef_conn[relax_lch_E[i]].num_active_PCs = 0;
    dis_gef_conn[relax_lch_E[i]].num_active_f_PCs = 0;
    dis_gef_conn[relax_lch_E[i]].ch = dis_FALSE;
  }

  for ( i = 0; i < dis_gnum_fl_conn; i++ ) {
    for ( j = 0; j <= max; j++ ) {
      dis_gfl_conn[i].def[j] = dis_FALSE;
    }
    dis_gfl_conn[i].curr_assigned = dis_FALSE;
  }

}



dis_Bool dis_all_goals_activated( int time ) 

{

  int i;

     
  for ( i = 0; i < dis_gnum_flogic_goal; i++ ) {
      if ( !dis_gft_conn[dis_gflogic_goal[i]].in_F ) {
      return dis_FALSE;
    }
  }

  for ( i = 0; i < dis_gnum_fnumeric_goal; i++ ) {
    if ( !dis_gfl_conn[dis_gfnumeric_goal_fl[i]].def[time] ) {
      return dis_FALSE;
    }
    if ( !dis_number_comparison_holds( dis_gfnumeric_goal_comp[i],
				   dis_gfl_conn[dis_gfnumeric_goal_fl[i]].level[time],
				   dis_gfnumeric_goal_c[i] ) ) {
      return dis_FALSE;
    }
  }

  for ( i = 0; i < dis_gnum_flogic_goal; i++ ) {
    if ( dis_gft_conn[dis_gflogic_goal[i]].level == dis_INFINITY ) {
      dis_gft_conn[dis_gflogic_goal[i]].level = time;
    }
  }

  return dis_TRUE;

}



void dis_print_fixpoint_result( void )

{

  int time, i;
  dis_Bool hit, hit_F, hit_E, hit_FL;

  time = 0;
  while ( 1 ) {
    hit = dis_FALSE;
    hit_F = dis_FALSE;
    hit_E = dis_FALSE;
    hit_FL = dis_FALSE;
    for ( i = 0; i < dis_gnum_ft_conn; i++ ) {
      if ( dis_gft_conn[i].level == time ) {
	hit = dis_TRUE;
	hit_F = dis_TRUE;
	break;
      }
    }
    for ( i = 0; i < dis_gnum_ef_conn; i++ ) {
      if ( dis_gef_conn[i].level == time ) {
	hit = dis_TRUE;
	hit_E = dis_TRUE;
	break;
      }
    }
    for ( i = 0; i < dis_gnum_fl_conn; i++ ) {
      if ( dis_gfl_conn[i].def[time] ) {
	hit = dis_TRUE;
	hit_FL = dis_TRUE;
	break;
      }
    }
    if ( !hit ) {
      break;
    }
 
    printf("\n\nLEVEL %d:", time);
    if ( hit_F ) {
      printf("\n\nFACTS:");
      for ( i = 0; i < dis_gnum_ft_conn; i++ ) {
	if ( dis_gft_conn[i].level == time ) {
	  printf("\n");
	  dis_print_ft_name( i );
	}
      }
    }
    if ( hit_FL ) {
      printf("\n\nFLUENTS:");
      for ( i = 0; i < dis_gnum_fl_conn; i++ ) {
	if ( dis_gfl_conn[i].def[time] ) {
	  printf("\n");
	  dis_print_fl_name( i );
	  printf(": %f", dis_gfl_conn[i].level[time]);
	} else {
	  printf("\n");
	  dis_print_fl_name( i );
	  printf(": UNDEF");
	}
      }
    }
    if ( hit_E ) {
      printf("\n\nEFS:");
      for ( i = 0; i < dis_gnum_ef_conn; i++ ) {
	if ( dis_gef_conn[i].level == time ) {
	  printf("\neffect %d to ", i);
	  dis_print_op_name( dis_gef_conn[i].op );
	}
      }
    }

    time++;
  }
  fflush( stdout );

}
    






















/**************************************
 * FIRST RELAXED PLAN (1P) dis_EXTRACTION *
 **************************************/



























int dis_extract_1P( int max )

{

  static dis_Bool first_call = dis_TRUE;
  int i, j, max_goal_level, time;

  if ( first_call ) {
    for ( i = 0; i < dis_gnum_ft_conn; i++ ) {
      dis_gft_conn[i].is_true = dis_INFINITY;
      dis_gft_conn[i].is_goal = dis_FALSE;
      dis_gft_conn[i].ch = dis_FALSE;
    }
    for ( i = 0; i < dis_gnum_op_conn; i++ ) {
      dis_gop_conn[i].is_used = dis_INFINITY;
    }
    for ( i = 0; i < dis_gnum_ef_conn; i++ ) {
      dis_gef_conn[i].in_plan = dis_INFINITY;
    }
    relax_lch_F = ( int * ) calloc( dis_gnum_ft_conn, sizeof( int ) );
    relax_lnum_ch_F = 0;
    relax_lused_O = ( int * ) calloc( dis_gnum_op_conn+5, sizeof( int ) );
    relax_lnum_used_O = 0;
    relax_lin_plan_E = ( int * ) calloc( dis_gnum_ef_conn+5, sizeof( int ) );
    relax_lnum_in_plan_E = 0;
    first_call = dis_FALSE;
  }

  dis_reset_search_info();

  max_goal_level = dis_initialize_goals( max );

  if ( dis_gcmd_line.optimize && dis_goptimization_established ) {
    /* we are optimizing cost;
     * initialize global cost of the relaxed plan.
     */
    dis_gcost = 0;
  }

  relax_lh = 0;
  for ( time = max_goal_level; time > 0; time-- ) {
    dis_achieve_goals( time );
  }	

  if (GpG.SearchModal == -1002 && !GpG.is_durative)
  {
    makespan = 0;
    for (i=0;i<Nop;i++)
      if (strlen(dis_gop_conn[dis_ops[i]].action->name) == 5)
        makespan++;
    for (i=0;i<relax_lnum_used_O;i++)
      if (strlen(dis_gop_conn[relax_lused_O[i]].action->name) == 5)
        makespan++;
    if (makespan > dead)
      relax_lh = dis_INFINITY;
  }
  if ((GpG.SearchModal == -103 && GpG.is_durative) || num_dur == -1 || GpG.is_til)
  {
    makespan = total_time = 0;
    for (i=0;i<Nop;i++)
    {
      if (makespan < schedule[i] + dis_gef_conn[dis_ops[i]].duration)
        makespan = schedule[i] + dis_gef_conn[dis_ops[i]].duration;
      total_time += dis_gef_conn[dis_ops[i]].duration;
    }
    for (i=0;i<dis_gnum_flogic_goal;i++)
    {
      if (makespan < relax_time_fact[dis_gflogic_goal[i]])
        makespan = relax_time_fact[dis_gflogic_goal[i]];
    }
    if (dis_gnum_con_conn > 0)
    {
      for (i=0;i<dis_gnum_con_conn;i++)
        if (dis_gcon_conn[i].connective == dis_WITHIN_c)
        {
          for (j=0;j<dis_gnum_flogic_goal;j++)
            if (dis_gef_conn[dis_gcon_conn[i].cond[0]].PC[0] == dis_gflogic_goal[j])
              break;
          if (j < dis_gnum_flogic_goal && relax_time_fact[dis_gflogic_goal[j]] > dis_gcon_conn[i].time1)
            break;
        }
      if (i < dis_gnum_con_conn)
        relax_lh = dis_INFINITY;
    }
    xfree(current_state.F);
  }
  return relax_lh;

}



int dis_initialize_goals( int max )

{

  static dis_Bool first_call = dis_TRUE;
  static int highest_seen;

  int i, j, max_goal_level, ft, fl;
  dis_Comparator comp;
  float val;

  if ( first_call ) {
    relax_lgoals_at = ( int ** ) calloc( dis_RELAXED_STEPS_DEFAULT, sizeof( int * ) );
    relax_lnum_goals_at = ( int * ) calloc( dis_RELAXED_STEPS_DEFAULT, sizeof( int ) );
    relax_lf_goals_c_at = ( float ** ) calloc( dis_RELAXED_STEPS_DEFAULT, sizeof( float * ) );
    relax_lf_goals_comp_at = ( dis_Comparator ** ) calloc( dis_RELAXED_STEPS_DEFAULT, sizeof( dis_Comparator * ) );
    for ( i = 0; i < dis_RELAXED_STEPS_DEFAULT; i++ ) {
      relax_lgoals_at[i] = ( int * ) calloc( dis_gnum_ft_conn, sizeof( int ) );
      relax_lf_goals_c_at[i] = ( float * ) calloc( dis_gnum_fl_conn, sizeof( float ) );
      relax_lf_goals_comp_at[i] = ( dis_Comparator * ) calloc( dis_gnum_fl_conn, sizeof( dis_Comparator ) );
    }
    highest_seen = dis_RELAXED_STEPS_DEFAULT;

    relax_relax_lHcomp = ( dis_Comparator * ) calloc( dis_gnum_fl_conn, sizeof( dis_Comparator ) );
    relax_lHc = ( float * ) calloc( dis_gnum_fl_conn, sizeof( float ) );
    first_call = dis_FALSE;
  }

  if ( max + 1 > highest_seen ) {
    for ( i = 0; i < highest_seen; i++ ) {
      xfree( relax_lgoals_at[i] );
      xfree( relax_lf_goals_c_at[i] );
      xfree( relax_lf_goals_comp_at[i] );
    }
    xfree( relax_lgoals_at );
    xfree( relax_lnum_goals_at );
    xfree( relax_lf_goals_c_at );
    xfree( relax_lf_goals_comp_at );
    highest_seen = max + 10;
    relax_lgoals_at = ( int ** ) calloc( highest_seen, sizeof( int * ) );
    relax_lnum_goals_at = ( int * ) calloc( highest_seen, sizeof( int ) );
    relax_lf_goals_c_at = ( float ** ) calloc( highest_seen, sizeof( float * ) );
    relax_lf_goals_comp_at = ( dis_Comparator ** ) calloc( highest_seen, sizeof( dis_Comparator * ) );
    for ( i = 0; i < highest_seen; i++ ) {
      relax_lgoals_at[i] = ( int * ) calloc( dis_gnum_ft_conn, sizeof( int ) );
      relax_lf_goals_c_at[i] = ( float * ) calloc( dis_gnum_fl_conn, sizeof( float ) );
      relax_lf_goals_comp_at[i] = ( dis_Comparator * ) calloc( dis_gnum_fl_conn, sizeof( dis_Comparator ) );
    }
  }

  for ( i = 0; i < max + 1; i++ ) {
    relax_lnum_goals_at[i] = 0;
    for ( j = 0; j < dis_gnum_fl_conn; j++ ) {
      relax_lf_goals_comp_at[i][j] = IGUAL;
      /* probably not necessary; igual...
       */
      relax_relax_lHcomp[j] = IGUAL;
    }
  }

  max_goal_level = 0;
  for ( i = 0; i < dis_gnum_flogic_goal; i++ ) {
    ft = dis_gflogic_goal[i];
    /* level can't be dis_INFINITY as otherwise 1P wouldn't have
     * been called.
     */
    if ( dis_gft_conn[ft].level > max_goal_level ) {
      max_goal_level = dis_gft_conn[ft].level;
    }
    relax_lgoals_at[dis_gft_conn[ft].level][relax_lnum_goals_at[dis_gft_conn[ft].level]++] = ft;
    dis_gft_conn[ft].is_goal = dis_TRUE;
    if ( !dis_gft_conn[ft].ch ) {
      relax_lch_F[relax_lnum_ch_F++] = ft;
      dis_gft_conn[ft].ch = dis_TRUE;
    }
  }

  for ( i = 0; i < dis_gnum_fnumeric_goal; i++ ) {
    fl = dis_gfnumeric_goal_fl[i];
    val = dis_gfnumeric_goal_c[i];
    comp = dis_gfnumeric_goal_comp[i];
    for ( j = 0; j <= max; j++ ) {
      if ( !dis_gfl_conn[fl].def[j] ) continue;
      if ( dis_number_comparison_holds( comp, dis_gfl_conn[fl].level[j], val ) ) break;
    }
    if ( j > max ) {
      printf("\n\nnumeric goal not reached in 1P??\n\n");
      exit( 1 );
    }
    if ( j > max_goal_level ) {
      max_goal_level = j;
    }
    relax_lf_goals_c_at[j][fl] = val;
    relax_lf_goals_comp_at[j][fl] = comp;
  }

  return max_goal_level;

}



void dis_achieve_goals( int time )

{

  int i, j, k, ft, min_p, min_e, ef = -1, p;
  float val;

  /* achieve the goals set at level time >= 1
   */
  
  if ( dis_gcmd_line.display_info == 127 ) {
    printf("\nselecting at step %3d: ", time-1);
  }

  /* before we start, we must translate the artificial goals
   * into real goals.
   */
  for ( i = dis_gnum_real_fl_conn; i < dis_gnum_fl_conn; i++ ) {
    if ( relax_lf_goals_comp_at[time][i] == IGUAL ) {
      /* this one isn't needed
       */
      continue;
    }
    dis_enforce_artificial_goal( i, time );
  }

  /* for helpful actions:
   * remember at time 1 what the goals were.
   */
  if ( time == 1 ) {
    for ( i = 0; i < dis_gnum_real_fl_conn; i++ ) {
      relax_relax_lHcomp[i] = relax_lf_goals_comp_at[time][i];
      relax_lHc[i] = relax_lf_goals_c_at[time][i];
    }
  }

  /* first, push the numeric goals at this level so far down that
   * the requirement for each of them can be fulfilled in the previous
   * level.
   */
  for ( i = 0; i < dis_gnum_real_fl_conn; i++ ) {
    if ( relax_lf_goals_comp_at[time][i] == IGUAL ) {
      continue;
    }
    if ( dis_gfl_conn[i].def[time-1] &&
	 dis_number_comparison_holds( relax_lf_goals_comp_at[time][i],
				  dis_gfl_conn[i].level[time-1],
				  relax_lf_goals_c_at[time][i] ) ) {
      /* this can be solved one step earlier.
       * propagate it downwards and mark as OK.
       */
      dis_update_f_goal( i, time-1, relax_lf_goals_comp_at[time][i], relax_lf_goals_c_at[time][i] );
      relax_lf_goals_comp_at[time][i] = IGUAL;
      continue;
    }
    /* if there is a good assigner, then take it.
     */
    for ( j = 0; j < dis_gfl_conn[i].num_AS; j++ ) {
      ef = dis_gfl_conn[i].AS[j];
      if ( !dis_LESS( dis_gef_conn[ef].level, time ) ) {
	/* we allow any effect that's already there
	 */
	continue;
      }
      if ( dis_gfl_conn[i].AS_fl_[j] != -1 &&
	   !dis_gfl_conn[dis_gfl_conn[i].AS_fl_[j]].def[time-1] ) {
	/* accesses an undefined value.
	 */
	continue;
      }
      if ( dis_gfl_conn[i].AS_fl_[j] != -1 ) {
	val = dis_gfl_conn[dis_gfl_conn[i].AS_fl_[j]].level[time-1] + dis_gfl_conn[i].AS_c[j];
      } else {
	val = dis_gfl_conn[i].AS_c[j];
      }
      if ( !dis_number_comparison_holds( relax_lf_goals_comp_at[time][i],
				     val,
				     relax_lf_goals_c_at[time][i] ) ) {
	/* that one is not strong enough.
	 */
	continue;
      }
      break;
    }
    if ( j < dis_gfl_conn[i].num_AS ) {
      /* ef is an assigner that is strong enough and already there.
       */
      if ( dis_gef_conn[ef].in_plan == time - 1 ) {
	printf("\n\nassigner already selected, nevertheless goal still there\n\n");
	exit( 1 );
      } else {
	if ( dis_gef_conn[ef].in_plan == dis_INFINITY ) {
	  relax_lin_plan_E[relax_lnum_in_plan_E++] = ef;
	}
	dis_gef_conn[ef].in_plan = time - 1;
	if ( dis_gcmd_line.optimize && dis_goptimization_established ) {
	  /* we want to execute this effect here, so we gotta live with the cost.
	   */
	  dis_gcost += dis_gef_conn[ef].cost;
	}
      }
      /* now select the resp. op at this level, if necessary
       */
      dis_select_op( time, dis_gef_conn[ef].op );
      /* now mark the benefits of that effect, introducing
       * also the fl_ level enforcement goals for each effect
       * that is useful for solving a goal at time: in particular,
       * this will be the one we have just selected.
       */      
      dis_introduce_benefits_and_enforcements( time, ef );
      /* now introduce the new goals
       */
      dis_introduce_pc_goals( time, ef );

      /* care about next fluent
       */
      continue;
    }
    /* debug...
     */
    if ( !dis_gfl_conn[i].def[time-1] ) {
      printf("\n\nall assignerss applied yet goal not fulfilled - undefined below.\n\n");
      exit( 1 );
    }



    /* no good assigner available. thus, push the goal at this level so far 
     * down that its requirement can be fulfilled in the previous level.
     */
    for ( j = 0; j < dis_gfl_conn[i].num_IN; j++ ) {
      /* go through increasers in constant quantity order top to
       * bottom (see inst_final.c);
       */
      ef = dis_gfl_conn[i].IN[j];
      if ( !dis_LESS( dis_gef_conn[ef].level, time ) ) {
	continue;
      }
      if ( dis_gfl_conn[i].IN_fl_[j] != -1 &&
	   !dis_gfl_conn[dis_gfl_conn[i].IN_fl_[j]].def[time-1] ) {
	/* accesses an undefined fluent.
	 */
	continue;
      }
      if ( dis_gfl_conn[i].IN_fl_[j] != -1 ) {
	val = dis_gfl_conn[dis_gfl_conn[i].IN_fl_[j]].level[time-1] + dis_gfl_conn[i].IN_c[j];
      } else {
	val = dis_gfl_conn[i].IN_c[j];
      }
      if ( val <= 0 ) {
	/* that one does not help us at all.
	 */
	continue;
      }
      /* if ef is already selected here, we can not use it anymore;
       * else, record it as selected.
       */
      if ( dis_gef_conn[ef].in_plan == time - 1 ) {
	continue;
      } else {
	if ( dis_gef_conn[ef].in_plan == dis_INFINITY ) {
	  relax_lin_plan_E[relax_lnum_in_plan_E++] = ef;
	}
	dis_gef_conn[ef].in_plan = time - 1;
	if ( dis_gcmd_line.optimize && dis_goptimization_established ) {
	  dis_gcost += dis_gef_conn[ef].cost;
	}
      }
      /* do the usual stuff...
       */
      dis_select_op( time, dis_gef_conn[ef].op );
      dis_introduce_benefits_and_enforcements( time, ef );
      dis_introduce_pc_goals( time, ef );
      /* stop as soon as 
       * goal can be fulfilled one step below.
       */
      if ( dis_number_comparison_holds( relax_lf_goals_comp_at[time][i],
				    dis_gfl_conn[i].level[time-1],
				    relax_lf_goals_c_at[time][i] ) ) {
	break;
      }
    }
    /* now propagate the revised goal downward, and say we are finished with
     * this one.
     */
    dis_update_f_goal( i, time-1, relax_lf_goals_comp_at[time][i], relax_lf_goals_c_at[time][i] );
    relax_lf_goals_comp_at[time][i] = IGUAL;

    /* debug...
     */
    if ( !dis_number_comparison_holds( relax_lf_goals_comp_at[time-1][i],
				   dis_gfl_conn[i].level[time-1],
				   relax_lf_goals_c_at[time-1][i] ) ) {
      printf("\n\nall increasers applied yet goal not fulfilled.\n\n");
      exit( 1 );
    }
  }/* fluents at level time */



  /* now achieve also the remaining logic goals here.
   */
  for ( i = 0; i < relax_lnum_goals_at[time]; i++ ) {
    ft = relax_lgoals_at[time][i];

    if ( dis_gft_conn[ft].is_true == time ) {
      /* fact already added by prev now selected op
       */
      continue;
    }

    min_p = dis_INFINITY;
    min_e = -1;
    for ( j = 0; j < dis_gft_conn[ft].num_A; j++ ) {
      ef = dis_gft_conn[ft].A[j];
      if ( dis_gef_conn[ef].level != time - 1 ) continue; 
      p = 0;
      for ( k = 0; k < dis_gef_conn[ef].num_PC; k++ ) {
	p += dis_gft_conn[dis_gef_conn[ef].PC[k]].level;
      }
      if ( dis_LESS( p, min_p ) ) {
	min_p = p;
	min_e = ef;
      }
    }
    ef = min_e;
    /* if ef is already selected, we can not use it anymore;
     * else, record it as selected.
     *
     * actually it can't happen here that the ef
     * is already selected as then the goal is true already.
     * nevermind.
     */
    if ( dis_gef_conn[ef].in_plan == time - 1 ) {
      continue;
    } else {
      if ( dis_gef_conn[ef].in_plan == dis_INFINITY ) {
	relax_lin_plan_E[relax_lnum_in_plan_E++] = ef;
      }
      dis_gef_conn[ef].in_plan = time - 1;
      if ( dis_gcmd_line.optimize && dis_goptimization_established ) {
	dis_gcost += dis_gef_conn[ef].cost;
      }
    }
    dis_select_op( time, dis_gef_conn[ef].op );
    dis_introduce_benefits_and_enforcements( time, ef );
    dis_introduce_pc_goals( time, ef );
  }

}



void dis_enforce_artificial_goal( int fl, int time )

{

  int i;

  if ( !dis_gfl_conn[fl].artificial ) {
    printf("\n\ntrying to enforce non-artificial goal\n\n");
    exit( 1 );
  }

  /* for now, we simply require the maximum levels of all 
   * composites.
   */
  for ( i = 0; i < dis_gfl_conn[fl].num_lnf; i++ ) {
    dis_update_f_goal( dis_gfl_conn[fl].lnf_F[i], time, GEQ,
		   dis_gfl_conn[dis_gfl_conn[fl].lnf_F[i]].level[time] );
  }
  
}



void dis_select_op( int time, int op )

{

  if ( dis_gop_conn[op].is_used != time - 1 ) {
    /* we don't have this op yet at this level
     * --> count it and record it for setting back info.
     */
    if ( dis_gop_conn[op].is_used == dis_INFINITY ) {
      relax_lused_O[relax_lnum_used_O++] = op;
    }
    dis_gop_conn[op].is_used = time - 1;
    
    if (GpG.SearchModal == 100 || 
    !dis_gef_conn[dis_gop_conn[op].E[0]].DPop)
    {
      if (GpG.MFF_parser)
      {
        if (priority[op] > -1000 )
          relax_lh = relax_lh + 1;
      }
      else
        relax_lh = relax_lh + 1;
    }

    if ( dis_gcmd_line.display_info == 127 ) {
      printf("\n                       ");
      dis_print_op_name( op );
    }
  }

}



void dis_introduce_benefits_and_enforcements( int time, int ef )

{

  int k, l, ft, fl;
  float val;

  for ( k = 0; k < dis_gef_conn[ef].num_A; k++ ) {
    ft = dis_gef_conn[ef].A[k];
    dis_gft_conn[ft].is_true = time;
    if ( !dis_gft_conn[ft].ch ) {
      relax_lch_F[relax_lnum_ch_F++] = ft;
      dis_gft_conn[ft].ch = dis_TRUE;
    }
  }

  for ( k = 0; k < dis_gef_conn[ef].num_AS; k++ ) {
    fl = dis_gef_conn[ef].AS_fl[k];
    if ( relax_lf_goals_comp_at[time][fl] == IGUAL ) {
      continue;
    }
    if ( !dis_assign_value( ef, time - 1, k, &val ) ) {
      continue;
    }
    if ( dis_number_comparison_holds( relax_lf_goals_comp_at[time][fl],
				  val,
				  relax_lf_goals_c_at[time][fl] ) ) {
      /* this effect assigns what we wanted there. enforce its
       * dependency fluent fl_
       */
      relax_lf_goals_comp_at[time][fl] = IGUAL;
      dis_enforce_assign( ef, time - 1, k );
    }
  }
  for ( k = 0; k < dis_gef_conn[ef].num_IN; k++ ) {
    fl = dis_gef_conn[ef].IN_fl[k];
    if ( relax_lf_goals_comp_at[time][fl] == IGUAL ) {
      continue;
    }
    if ( !dis_increase_value( ef, time - 1, k, &val ) ) {
      continue;
    }
    if ( val > 0 ) {
      relax_lf_goals_c_at[time][fl] -= val;
      dis_enforce_increase( ef, time - 1, k );
    }
  }

  for ( k = 0; k < dis_gef_conn[ef].num_I; k++ ) {
    if ( dis_gef_conn[dis_gef_conn[ef].I[k]].in_plan == time - 1 ) {
      continue;
    } else {
      if ( dis_gef_conn[dis_gef_conn[ef].I[k]].in_plan == dis_INFINITY ) {
	relax_lin_plan_E[relax_lnum_in_plan_E++] = dis_gef_conn[ef].I[k];
      }
      dis_gef_conn[dis_gef_conn[ef].I[k]].in_plan = time - 1;
      if ( dis_gcmd_line.optimize && dis_goptimization_established ) {
	dis_gcost += dis_gef_conn[dis_gef_conn[ef].I[k]].cost;
      }
    }
    for ( l = 0; l < dis_gef_conn[dis_gef_conn[ef].I[k]].num_A; l++ ) {
      ft = dis_gef_conn[dis_gef_conn[ef].I[k]].A[l];
      dis_gft_conn[ft].is_true = time;
      if ( !dis_gft_conn[ft].ch ) {
	relax_lch_F[relax_lnum_ch_F++] = ft;
	dis_gft_conn[ft].ch = dis_TRUE;
      }
    }
    for ( l = 0; l < dis_gef_conn[dis_gef_conn[ef].I[k]].num_AS; l++ ) {
      fl = dis_gef_conn[dis_gef_conn[ef].I[k]].AS_fl[l];
      if ( relax_lf_goals_comp_at[time][fl] == IGUAL ) {
	continue;
      }
      if ( !dis_assign_value( dis_gef_conn[ef].I[k], time - 1, l, &val ) ) {
	continue;
      }
      if ( dis_number_comparison_holds( relax_lf_goals_comp_at[time][fl],
				    val,
				    relax_lf_goals_c_at[time][fl] ) ) {
	relax_lf_goals_comp_at[time][fl] = IGUAL;
	dis_enforce_assign( dis_gef_conn[ef].I[k], time - 1, l );
      }
    }
    for ( l = 0; l < dis_gef_conn[dis_gef_conn[ef].I[k]].num_IN; l++ ) {
      fl = dis_gef_conn[dis_gef_conn[ef].I[k]].IN_fl[l];
      if ( relax_lf_goals_comp_at[time][fl] == IGUAL ) {
	continue;
      }
      if ( !dis_increase_value( dis_gef_conn[ef].I[k], time - 1, l, &val ) ) {
	continue;
      }
      if ( val > 0 ) {
	relax_lf_goals_c_at[time][fl] -= val;
	dis_enforce_increase( dis_gef_conn[ef].I[k], time - 1, l );
      }
    }
  }/* implied effects */

}



dis_Bool dis_assign_value( int ef, int at_time, int nr, float *val )

{

  if ( dis_gef_conn[ef].AS_fl_[nr] == -1 ) {
    /* no dependency.
     */
    *val = dis_gef_conn[ef].AS_c[nr];
    return dis_TRUE;
  }

  if ( !dis_gfl_conn[dis_gef_conn[ef].AS_fl_[nr]].def[at_time] ) {
    return dis_FALSE;
  }
  
  *val = dis_gfl_conn[dis_gef_conn[ef].AS_fl_[nr]].level[at_time] + dis_gef_conn[ef].AS_c[nr];
  return dis_TRUE;

}



dis_Bool dis_increase_value( int ef, int at_time, int nr, float *val )

{

  if ( dis_gef_conn[ef].IN_fl_[nr] == -1 ) {
    /* no dependency.
     */
    *val = dis_gef_conn[ef].IN_c[nr];
    return dis_TRUE;
  }

  if ( !dis_gfl_conn[dis_gef_conn[ef].IN_fl_[nr]].def[at_time] ) {
    return dis_FALSE;
  }
  
  *val = dis_gfl_conn[dis_gef_conn[ef].IN_fl_[nr]].level[at_time] + dis_gef_conn[ef].IN_c[nr];
  return dis_TRUE;

}



void dis_enforce_assign( int ef, int at_time, int nr )

{

  if ( dis_gef_conn[ef].AS_fl_[nr] == -1 ) {
    return;
  }

  if ( !dis_gfl_conn[dis_gef_conn[ef].AS_fl_[nr]].def[at_time] ) {
    printf("\n\ntrying to enforce an undefined value.\n\n");
    exit( 1 );
  }

  /* for now, simply require the maximum benefit of the effect.
   */
  dis_update_f_goal( dis_gef_conn[ef].AS_fl_[nr], at_time, GEQ,
		 dis_gfl_conn[dis_gef_conn[ef].AS_fl_[nr]].level[at_time] );

}



void dis_enforce_increase( int ef, int at_time, int nr )

{

  if ( dis_gef_conn[ef].IN_fl_[nr] == -1 ) {
    return;
  }

  if ( !dis_gfl_conn[dis_gef_conn[ef].IN_fl_[nr]].def[at_time] ) {
    printf("\n\ntrying to enforce an undefined value.\n\n");
    exit( 1 );
  }

  /* for now, simply require the maximum benefit of the effect.
   */
  dis_update_f_goal( dis_gef_conn[ef].IN_fl_[nr], at_time, GEQ,
		 dis_gfl_conn[dis_gef_conn[ef].IN_fl_[nr]].level[at_time] );

}



void dis_introduce_pc_goals( int time, int ef )

{

  int k, l, ft, fl;
  float val;
  dis_Comparator comp;

  /* now introduce the new goals
   */
  for ( k = 0; k < dis_gef_conn[ef].num_PC; k++ ) {
    ft = dis_gef_conn[ef].PC[k];
    if ( dis_gft_conn[ft].is_goal ) {
      /* this fact already is a goal
       */
      continue;
    }
    relax_lgoals_at[dis_gft_conn[ft].level][relax_lnum_goals_at[dis_gft_conn[ft].level]++] = ft;
    dis_gft_conn[ft].is_goal = dis_TRUE;
    if ( !dis_gft_conn[ft].ch ) {
      relax_lch_F[relax_lnum_ch_F++] = ft;
      dis_gft_conn[ft].ch = dis_TRUE;
    }
  }
  /* now for the numeric precs.
   */
  for ( k = 0; k < dis_gef_conn[ef].num_f_PC; k++ ) {
    fl = dis_gef_conn[ef].f_PC_fl[k];
    val = dis_gef_conn[ef].f_PC_c[k];
    comp = dis_gef_conn[ef].f_PC_comp[k];
    /* determine the first level where prec can be fulfilled.
     */
    for ( l = 0; l < time; l++ ) {
      if ( !dis_gfl_conn[fl].def[l] ) continue;
      if ( dis_number_comparison_holds( comp, dis_gfl_conn[fl].level[l], val ) ) break;
    }
    if ( l >= time ) {
      printf("\n\nnumeric prec not reached in 1P??\n\n");
      exit( 1 );
    }
    /* if new requirement is stronger than old, then insert it.
     */
    dis_update_f_goal( fl, l, comp, val );
  }

}



void dis_update_f_goal( int fl, int time, dis_Comparator comp, float val )

{

  if ( relax_lf_goals_comp_at[time][fl] == IGUAL ) {
    relax_lf_goals_comp_at[time][fl] = comp;
    relax_lf_goals_c_at[time][fl] = val;
    return;
  }

  if ( relax_lf_goals_c_at[time][fl] < val ) {
    relax_lf_goals_comp_at[time][fl] = comp;
    relax_lf_goals_c_at[time][fl] = val;
    return;
  }

  if ( relax_lf_goals_c_at[time][fl] == val &&
       comp == GE ) {
    relax_lf_goals_comp_at[time][fl] = GE;
  }
   
}



void dis_reset_search_info( void )

{

  int i;

  for ( i = 0; i < relax_lnum_ch_F; i++ ) {
    dis_gft_conn[relax_lch_F[i]].is_true = dis_INFINITY;
    dis_gft_conn[relax_lch_F[i]].is_goal = dis_FALSE;
    dis_gft_conn[relax_lch_F[i]].ch = dis_FALSE;
  }
  relax_lnum_ch_F = 0;

  for ( i = 0; i < relax_lnum_used_O; i++ ) {
    dis_gop_conn[relax_lused_O[i]].is_used = dis_INFINITY;
  }
  relax_lnum_used_O = 0;
  
  for ( i = 0; i < relax_lnum_in_plan_E; i++ ) {
    dis_gef_conn[relax_lin_plan_E[i]].in_plan = dis_INFINITY;
  }
  relax_lnum_in_plan_E = 0;

}



void dis_collect_H_info( void )

{

  static dis_Bool first_call = dis_TRUE;

  int i, j, ft, ef, op;
  float val;

  if ( first_call ) {
    dis_gH = ( int * ) calloc( dis_gnum_op_conn+5, sizeof( int ) );
    dis_gnum_H = 0;
    first_call = dis_FALSE;
  }

  for ( i = 0; i < dis_gnum_H; i++ ) {
    dis_gop_conn[dis_gH[i]].is_in_H = dis_FALSE;
  }

  /* first the logical guys
   */
  dis_gnum_H = 0;
  for ( i = relax_lnum_goals_at[1] - 1; i >= 0; i-- ) {
    ft = relax_lgoals_at[1][i];

    for ( j = 0; j < dis_gft_conn[ft].num_A; j++ ) {
      ef = dis_gft_conn[ft].A[j];
      if ( dis_gef_conn[ef].level != 0 ) {
	continue;
      }
      op = dis_gef_conn[ef].op;

      if ( dis_gop_conn[op].is_in_H ) {
	continue;
      }
      dis_gop_conn[op].is_in_H = dis_TRUE;
      dis_gH[dis_gnum_H++] = op;
    }
  }

  /* then the numerical ones.
   */
  for ( i = 0; i < dis_gnum_real_fl_conn; i++ ) {
    if ( relax_relax_lHcomp[i] == IGUAL ) {
      /* don't need this one at all.
       */
      continue;
    }
    if ( dis_gfl_conn[i].def[0] &&
	 dis_number_comparison_holds( relax_relax_lHcomp[i], dis_gfl_conn[i].level[0], relax_lHc[i] ) ) {
      /* this one's already ok initially... was only pushed down
       * through RPG.
       */
      continue;
    }
				  
    /* assigners
     */
    for ( j = 0; j < dis_gfl_conn[i].num_AS; j++ ) {
      ef = dis_gfl_conn[i].AS[j];
      op = dis_gef_conn[ef].op;
      if ( dis_gop_conn[op].is_in_H ) {
	continue;
      }

      if ( dis_gef_conn[ef].level != 0 ) {
	continue;
      }
      if ( dis_gef_conn[ef].illegal ) {
	continue;
      }
      if ( dis_gfl_conn[i].AS_fl_[j] != -1 &&
	   !dis_gfl_conn[dis_gfl_conn[i].AS_fl_[j]].def[0] ) {
	continue;
      }
      if ( dis_gfl_conn[i].AS_fl_[j] == -1 ) {
	val = dis_gfl_conn[i].AS_c[j];
      } else {
	val = dis_gfl_conn[dis_gfl_conn[i].AS_fl_[j]].level[0] + dis_gfl_conn[i].AS_c[j];
      }
      if ( !dis_number_comparison_holds( relax_relax_lHcomp[i], val, relax_lHc[i] ) ) {
	continue;
      }

      dis_gop_conn[op].is_in_H = dis_TRUE;
      dis_gH[dis_gnum_H++] = op;
    }

    if ( !dis_gfl_conn[i].def[0] ) {
      /* gotta be assigned.
       */
      continue;
    }

    /* increasers
     */
    for ( j = 0; j < dis_gfl_conn[i].num_IN; j++ ) {
      ef = dis_gfl_conn[i].IN[j];
      op = dis_gef_conn[ef].op;
      if ( dis_gop_conn[op].is_in_H ) {
	continue;
      }

      if ( dis_gef_conn[ef].level != 0 ) {
	continue;
      }
      if ( dis_gef_conn[ef].illegal ) {
	continue;
      }
      if ( dis_gfl_conn[i].IN_fl_[j] != -1 &&
	   !dis_gfl_conn[dis_gfl_conn[i].IN_fl_[j]].def[0] ) {
	continue;
      }
      if ( dis_gfl_conn[i].IN_fl_[j] == -1 ) {
	val = dis_gfl_conn[i].IN_c[j];
      } else {
	val = dis_gfl_conn[dis_gfl_conn[i].IN_fl_[j]].level[0] + dis_gfl_conn[i].IN_c[j];
      }
      if ( val <= 0 ) {
	continue;
      }

      dis_gop_conn[op].is_in_H = dis_TRUE;
      dis_gH[dis_gnum_H++] = op;
    }
  }

  if ( dis_gcmd_line.display_info == 128 ) {
    printf("\ncollected H: ");
    for ( i = 0; i < dis_gnum_H; i++ ) {
      dis_print_op_name( dis_gH[i] );
      printf("\n              ");
    }
  }

}

