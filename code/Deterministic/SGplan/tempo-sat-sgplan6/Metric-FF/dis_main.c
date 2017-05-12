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

 * File: dis_main.c 

 * Description: modified from main.c in Metric-FF

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
 * File: main.c
 * Description: The main routine for the FastForward Planner.
 *
 * Author: Joerg Hoffmann 2000
 * 
 *********************************************************************/ 








#include "dis_ff.h"

#include "dis_memory.h"
#include "dis_output.h"

#include "dis_parse.h"

#include "dis_expressions.h"

#include "dis_inst_pre.h"
#include "dis_inst_easy.h"
#include "dis_inst_hard.h"
#include "dis_inst_final.h"
#include "dis_constraints.h"
#include "dis_relax.h"
#include "dis_search.h"

#include "lpg.h"










/*
 *  ----------------------------- GLOBAL VARIABLES ----------------------------
 */












/*******************
 * GENERAL HELPERS *
 *******************/








/* used to time the different stages of the planner
 */
float dis_gtempl_time = 0, dis_greach_time = 0, dis_grelev_time = 0, dis_gconn_time = 0;
float dis_gLNF_time = 0, dis_gsearch_time = 0;


/* the command line inputs
 */
struct dis__command_line dis_gcmd_line;

/* number of states that got heuristically evaluated
 */
int dis_gevaluated_states = 0;

/* maximal depth of breadth first search
 */
int dis_gmax_search_depth = 0;





/***********
 * PARSING *
 ***********/







/* used for pddl parsing, flex only allows global variables
 */
int dis_gbracket_count;
char *dis_gproblem_name;

/* The current input line number
 */
int dis_lineno = 1;

/* The current input filename
 */
char *dis_gact_filename;

/* The pddl domain name
 */
char *dis_gdomain_name = NULL;

/* loaded, uninstantiated operators
 */
dis_Pldis_Operator *dis_gloaded_ops = NULL;

/* stores initials as fact_list 
 */
dis_PlNode *dis_gorig_initial_facts = NULL;

/* not yet preprocessed goal facts
 */
dis_PlNode *dis_gorig_goal_facts = NULL;

/* axioms as in UCPOP before being changed to ops
 */
dis_Pldis_Operator *dis_gloaded_axioms = NULL;

/* the types, as defined in the domain file
 */
dis_TypedList *dis_gparse_types = NULL;

/* the constants, as defined in domain file
 */
dis_TypedList *dis_gparse_constants = NULL;

/* the predicates and their arg types, as defined in the domain file
 */
dis_TypedListList *dis_gparse_predicates = NULL;

/* the functions and their arg types, as defined in the domain file
 */
dis_TypedListList *dis_gparse_functions = NULL;

/* the objects, declared in the problem file
 */
dis_TypedList *dis_gparse_objects = NULL;

/* the metric
 */
dis_Token dis_gparse_optimization;
dis_Parsedis_ExpNode *dis_gparse_metric = NULL;


/* connection to instantiation ( except ops, goal, initial )
 */

/* all typed objects 
 */
dis_FactList *dis_gorig_constant_list = NULL;

/* the predicates and their types
 */
dis_FactList *dis_gpredicates_and_types = NULL;

/* the functions and their types
 */
dis_FactList *dis_gfunctions_and_types = NULL;












/*****************
 * INSTANTIATING *
 *****************/









/* global arrays of constant names,
 *               type names (with their constants),
 *               predicate names,
 *               predicate aritys,
 *               defined types of predicate args
 */
dis_Token dis_gconstants[dis_MAX_CONSTANTS];
int dis_gnum_constants = 0;
dis_Token dis_gtype_names[dis_MAX_TYPES];
int dis_gtype_consts[dis_MAX_TYPES][dis_MAX_TYPE];
dis_Bool dis_gis_member[dis_MAX_CONSTANTS][dis_MAX_TYPES];
int dis_gtype_size[dis_MAX_TYPES];
int dis_gnum_types = 0;
dis_Token dis_gpredicates[dis_MAX_PREDICATES];
int dis_garity[dis_MAX_PREDICATES];
int dis_gpredicates_args_type[dis_MAX_PREDICATES][dis_MAX_ARITY];
int dis_gnum_predicates = 0;
dis_Token dis_gfunctions[dis_MAX_FUNCTIONS];
int dis_gf_arity[dis_MAX_FUNCTIONS];
int dis_gfunctions_args_type[dis_MAX_FUNCTIONS][dis_MAX_ARITY];
int dis_gnum_functions = 0;

/* derived predicates */
int dis_gnum_deripreds = 0;
/* timed initial literals */
int dis_gnum_tils = 0;
dis_TimedInitial *dis_gtils = NULL;
/* durative actions */
DuraTab *table;
int gnum_das = 0;
int **const_index;


/* the domain in integer (dis_Fact) representation
 */
dis_Operator_pointer *dis_goperators;
//dis_Operator_pointer dis_goperators[dis_MAX_OPERATdis_ORS];
int dis_gnum_operators = 0;
dis_Fact *dis_gfull_initial;
int dis_gnum_full_initial = 0;
dis_FluentValue *dis_gfull_fluents_initial;
int dis_gnum_full_fluents_initial = 0;
dis_WffNode *dis_ggoal = NULL;

dis_ExpNode *dis_gmetric = NULL;



/* stores inertia - information: is any occurence of the predicate
 * added / deleted in the uninstantiated ops ?
 */
dis_Bool dis_gis_added[dis_MAX_PREDICATES];
dis_Bool dis_gis_deleted[dis_MAX_PREDICATES];


/* for functions we *might* want to say, symmetrically, whether it is
 * increased resp. decreased at all.
 *
 * that is, however, somewhat involved because the right hand
 * sides can be arbirtray expressions, so we have no guarantee
 * that increasing really does adds to a functions value...
 *
 * thus (for the time being), we settle for "is the function changed at all?"
 */
dis_Bool dis_gis_changed[dis_MAX_FUNCTIONS];



/* splitted initial state:
 * initial non static facts,
 * initial static facts, divided into predicates
 * (will be two dimensional array, allocated directly before need)
 */
dis_Facts *dis_ginitial = NULL;
int dis_gnum_initial = 0;
dis_Fact **dis_ginitial_predicate;
int *dis_gnum_initial_predicate;

/* same thing for functions
 */
dis_FluentValues *dis_gf_initial;
int dis_gnum_f_initial = 0;
dis_FluentValue **dis_ginitial_function;
int *dis_gnum_initial_function;



/* the type numbers corresponding to any unary inertia
 */
int *dis_gtype_to_predicate;
//int dis_gtype_to_predicate[dis_MAX_PREDICATES];
int dis_gpredicate_to_type[dis_MAX_TYPES];

/* (ordered) numbers of types that new type is intersection of
 */
dis_TypeArray dis_gintersected_types[dis_MAX_TYPES];
int dis_gnum_intersected_types[dis_MAX_TYPES];



/* splitted domain: hard n easy ops
 */
dis_Operator_pointer *dis_ghard_operators;
int dis_gnum_hard_operators;
Normdis_Operator_pointer *dis_geasy_operators;
int dis_gnum_easy_operators;



/* so called Templates for easy ops: possible inertia constrained
 * instantiation constants
 */
dis_EasyTemplate *dis_geasy_templates;
int dis_gnum_easy_templates;



/* first step for hard ops: create mixed operators, with conjunctive
 * precondition and arbitrary effects
 */
Mixeddis_Operator *dis_ghard_mixed_operators;
int dis_gnum_hard_mixed_operators;



/* hard ''templates'' : pseudo actions
 */
dis_Pseudodis_Action_pointer *dis_ghard_templates;
int dis_gnum_hard_templates;



/* store the final "relevant facts"
 */
//dis_Fact dis_grelevant_facts[dis_MAX_RELEVANT_FACTS];
dis_Fact *dis_grelevant_facts;
int dis_gnum_relevant_facts = 0;
int dis_gnum_pp_facts = 0;
/* store the "relevant fluents"
 */
dis_Fluent *dis_grelevant_fluents;
//dis_Fluent dis_grelevant_fluents[dis_MAX_RELEVANT_FLUENTS];
int dis_gnum_relevant_fluents = 0;
dis_Token *dis_grelevant_fluents_name;
//dis_Token dis_grelevant_fluents_name[dis_MAX_RELEVANT_FLUENTS];
/* this is NULL for normal, and the LNF for
 * artificial fluents.
 */
Lnfdis_ExpNode_pointer *dis_grelevant_fluents_lnf;
//Lnfdis_ExpNode_pointer dis_grelevant_fluents_lnf[dis_MAX_RELEVANT_FLUENTS];



/* the final actions and problem representation
 */
dis_Action *dis_gactions = NULL;
int dis_gnum_actions;
dis_State dis_ginitial_state;
int *dis_glogic_goal = NULL;
int dis_gnum_logic_goal = 0;
dis_Comparator *dis_gnumeric_goal_comp = NULL;
dis_ExpNode_pointer *dis_gnumeric_goal_lh = NULL, *dis_gnumeric_goal_rh = NULL;
int dis_gnum_numeric_goal = 0;

/* direct numeric goal access
 */
dis_Comparator *dis_gnumeric_goal_direct_comp;
float *dis_gnumeric_goal_direct_c;



/* to avoid memory leaks; too complicated to identify
 * the exact state of the action to throw away (during construction),
 * memory gain not worth the implementation effort.
 */
dis_Action *dis_gtrash_actions = NULL;



/* additional lnf step between finalized inst and
 * conn graph
 */
dis_Comparator *dis_glnf_goal_comp = NULL;
Lnfdis_ExpNode_pointer *dis_glnf_goal_lh = NULL;
float *dis_glnf_goal_rh = NULL;
int dis_gnum_lnf_goal = 0;

Lnfdis_ExpNode dis_glnf_metric;
dis_Bool dis_goptimization_established = dis_FALSE;







/**********************
 * CONNECTIVITY GRAPH *
 **********************/







/* one ops (actions) array ...
 */
dis_OpConn *dis_gop_conn;
int dis_gnum_op_conn;



/* one effects array ...
 */
dis_EfConn *dis_gef_conn;
int dis_gnum_ef_conn;



/* one facts array.
 */
dis_FtConn *dis_gft_conn;
int dis_gnum_ft_conn;



/* and: one fluents array.
 */
dis_FlConn *dis_gfl_conn;
int dis_gnum_fl_conn;
int dis_gnum_real_fl_conn;/* number of non-artificial ones */



/* final goal is also transformed one more step.
 */
int *dis_gflogic_goal = NULL;
int dis_gnum_flogic_goal = 0;
dis_Comparator *dis_gfnumeric_goal_comp = NULL;
int *dis_gfnumeric_goal_fl = NULL;
float *dis_gfnumeric_goal_c = NULL;
int dis_gnum_fnumeric_goal = 0;

/* direct access (by relevant fluents)
 */
dis_Comparator *dis_gfnumeric_goal_direct_comp = NULL;
float *dis_gfnumeric_goal_direct_c = NULL;











/*******************
 * SEARCHING NEEDS *
 *******************/











/* applicable actions
 */
int *dis_gA;
int dis_gnum_A;



/* communication from extract 1.P. to search engine:
 * 1P action choice
 */
int *dis_gH;
int dis_gnum_H;
/* cost of relaxed plan
 */
float dis_gcost;



/* to store plan
 */
int dis_gplan_ops[dis_MAX_PLAN_LENGTH];
int dis_gnum_plan_ops = 0;



/* stores the states that the current plan goes through
 * ( for knowing where new agenda entry starts from )
 */
dis_State dis_gplan_states[dis_MAX_PLAN_LENGTH + 1];







/* dirty: multiplic. of total-time in final metric LNF
 */
float dis_gtt;

/* the mneed structures
 */
dis_Bool **dis_gassign_influence; 
dis_Bool **dis_gTassign_influence;



/* the real var input to the mneed computation.
 */
Bool *dis_gmneed_start_D; 
float *dis_gmneed_start_V;



/* does this contain conditional effects?
 * (if it does then the state hashing has to be made more
 *  cautiously)
 */
Bool dis_gconditional_effects;

/* Y. Chen new gvars
*/
dis_State dis_mff_sol;
dis_State dis_known_iga_list;

dis_State work_S0; 
dis_State work_S1;

int *work_ft;
int *work_op;

dis_State dis_gtrue_ft;
short dis_red_space;
int max_bfs_iter = 30000;
int max_hc_iter = 30000;

int bridge_option = 0;
dis_Bool hc_max_exceeded;

int *saved_dis_gflogic_goal;
int saved_dis_gnum_flogic_goal;
int saved_dis_gnum_fnumeric_goal;
dis_Comparator *saved_dis_gfnumeric_goal_comp;
int *saved_dis_gfnumeric_goal_fl;                      
float *saved_dis_gfnumeric_goal_c;     
        
int SymmLagrangian;
int SymmLagrange;

struct tms dis_dist_start, dis_dist_end;

/*
 *  ----------------------------- HEADERS Fdis_OR PARSING ----------------------------
 * ( fns defined in the scan-* files )
 */







void dis_get_fct_file_name( char *filename );
void dis_load_ops_file( char *filename );
void dis_load_fct_file( char *filename );

extern void add_new_predicates();
extern void add_negated_effects_preconds();
extern void add_new_derived_predicates();
extern void init_and_goal();






/*
 *  ----------------------------- MAIN ROUTINE ----------------------------
 */





struct tms dis_lstart, dis_lend;



dis_Bool dis_lfound_plan;

void dis_MFF_main( char* ops_file, char *fct_file )

{
  int i, j, k;
  dis_Action *a;
  Action *aa;
  struct tms start, end;
  FILE *f;
  char temp[128], dummy[8];
  float ptime;
  
  dis_PlNode *p;
  dis_Pldis_Operator *q;

  times ( &dis_lstart );

  /* parse the input files
   */

  /* start parse & instantiation timing
   */
  times( &start );
  /* domain file (ops)
   */
  GpG.is_negative = GpG.is_fluents = GpG.is_durative = GpG.is_deripred = GpG.is_til = dis_gconditional_effects = FALSE;
  GpG.is_constraints = GpG.is_preferences = GpG.is_goal_utilities = GpG.is_action_costs = GpG.is_numeric_fluents = FALSE;
  if ( dis_gcmd_line.display_info >= 1 ) {
  //  printf("\nParsing domain file");
  } 
  /* it is important for the pddl language to define the domain before 
   * reading the problem 
   */
  dis_load_ops_file( ops_file );
  /* problem file (facts)
   */  
  if ( dis_gcmd_line.display_info >= 1 ) {
    printf("\nParsing problem file"); 
  }
  dis_load_fct_file( fct_file );
  if ( dis_gcmd_line.display_info >= 1 ) {
    printf("\n");
  }

// Chih-Wei PDDL3
  dis_add_constraints(); 

  // Y. Chen ADD
    if(GpG.SearchModal == 100) {
        add_new_predicates();
        add_negated_effects_preconds();
        add_new_derived_predicates();
        init_and_goal();
    }
    
  
  GpG.is_fluents = GpG.is_numeric_fluents;
  /* This is needed to get all types.
   */
  dis_build_orig_constant_list();

  /* last step of parsing: see if it's an ADL domain!
   */
  if ( !dis_make_adl_domain() ) {
    printf("\nff: this is not an ADL problem!");
    printf("\n    can't be handled by this version.\n\n");
    exit( 1 );
  }


  /* now instantiate operators;
   */


  /**************************
   * first do PREPROCESSING * 
   **************************/

  /* start by collecting all strings and thereby encoding 
   * the domain in integers.
   */
  dis_encode_domain_in_integers();

  /* inertia preprocessing, first step:
   *   - collect inertia information
   *   - split initial state into
   *        - arrays for individual predicates
   *        - arrays for all static relations
   *        - array containing non - static relations
   */
  dis_do_inertia_preprocessing_step_1();

  /* normalize all PL1 formulae in domain description:
   * (goal, preconds and effect conditions)
   *   - simplify formula
   *   - expand quantifiers
   *   - dis_NOTs down
   */
  dis_normalize_all_wffs();

  /* translate negative preconds: introduce symmetric new predicate
   * dis_NOT-p(..) (e.g., not-in(?ob) in briefcaseworld)
   */
  dis_translate_negative_preconds();

  /* split domain in easy (disjunction of conjunctive preconds)
   * and hard (non DNF preconds) part, to apply 
   * different instantiation algorithms
   */
  dis_split_domain();

  /***********************************************
   * PREPROCESSING FINISHED                      *
   *                                             *
   * NOW MULTIPLY PARAMETERS IN EFFECTIVE MANNER *
   ***********************************************/

  dis_build_easy_action_templates();
// Chih-Wei PDDL3  
  if (dis_gnum_hard_operators)
    dis_build_hard_action_templates();

  times( &end );
  dis_TIME( dis_gtempl_time );

  times( &start );

  /* perform reachability analysis in terms of relaxed 
   * fixpoint
   */
  dis_perform_reachability_analysis();
  

  times( &end );
  dis_TIME( dis_greach_time );

  times( &start );

  /* collect the relevant facts and build final domain
   * and problem representations.
   */
  dis_collect_relevant_facts_and_fluents();

  times( &end );
  dis_TIME( dis_grelev_time );

  /* now transform problem to additive normal form,
   * if possible
   */
  times( &start );
  if ( !dis_transform_to_LNF() ) {
    printf("\n\nThis is not a linear task!\n\n");
    exit( 1 );
  }
  times( &end );
  dis_TIME( dis_gLNF_time );
  
  times( &start );

  /* now build globally accessable connectivity graph
   */
  dis_build_connectivity_graph();
    
  /* now check for acyclic := effects (in expressions.c)
   */
  dis_check_assigncycles();
  /* set the relevanc info (in expressions.c)
   */
  dis_determine_fl_relevance();

  times( &end );
  dis_TIME( dis_gconn_time );

// Chih-Wei
  times (&glob_end_time);
  gparsing_time = (float) ((glob_end_time.tms_utime -
                 glob_start_time.tms_utime +
                 glob_end_time.tms_stime -
                 glob_start_time.tms_stime) / 100.0);
  f = fopen(fct_file, "r");
  fgets(temp, 127, f);
  if (strstr(temp, "seconds"))
  {
    sscanf(temp+2, "%5s %f", dummy, &ptime);
    gparsing_time += ptime;
  }
  fclose(f);

  // Y. Chen   
  //dis_make_state(&dis_mff_sol, dis_gnum_ft_conn, dis_gnum_fl_conn);
        
  
  /***********************************************************
   * we are finally through with preprocessing and can worry *
   * bout finding a plan instead.                            *
   ***********************************************************/

  /*
  if ( dis_gcmd_line.display_info ) {
    printf("\n\nff: search configuration is ");
    if ( dis_gcmd_line.ehc ) {
      printf("EHC, if that fails then ");
    }
    printf(" best-first on %d*g(s) + %d*h(s) where\n    metric is ",
	   dis_gcmd_line.g_weight, dis_gcmd_line.h_weight);
    {
      printf(" plan length");
    }
  }*/

  times( &start );
  fflush(stdout);

// Chih-Wei PDDL3
  dis_setup_grounded_constraints();
/* Chih-Wei durative actions*/
	if (GpG.SearchModal == -2)
	{
/*	if (dis_gnum_ef_conn != gnum_ef_conn)
		printf("dis_gnum_ef_conn != gnum_ef_conn\n");*/
	for(i=0; i<dis_gnum_ef_conn; i++)
	{
		a = dis_gop_conn[dis_gef_conn[i].op].action;
		for (k=0;k<gnum_ef_conn;k++)
		{
			aa = gop_conn[gef_conn[k].op].action;
			if (strcmp(a->name, aa->name) || 
			a->num_name_vars != aa->num_name_vars)
				continue;
			for (j=0;j<a->num_name_vars;j++)
				if (strcmp(dis_gconstants[a->name_inst_table[j]], 
				gconstants[aa->name_inst_table[j]]))
					break;
			if (j == a->num_name_vars)
			{
				dis_gef_conn[i].duration = gef_conn[k].duration;
				break;
			}
		}
		if (k == gnum_ef_conn)
		{
			dis_print_op_name(dis_gef_conn[i].op);
			printf("\n");
		}
	}		
	}

  // extra copy of goals
  saved_dis_gnum_flogic_goal = dis_gnum_flogic_goal;
// Chih-Wei PDDL3
  saved_dis_gflogic_goal = (int*)calloc(dis_gnum_ft_conn, sizeof(int));
  for(i=0; i<saved_dis_gnum_flogic_goal; i++) {
    saved_dis_gflogic_goal[i] = dis_gflogic_goal[i];
  }
  saved_dis_gnum_fnumeric_goal = dis_gnum_fnumeric_goal;
  saved_dis_gfnumeric_goal_comp = (dis_Comparator *)calloc(dis_gnum_fl_conn,
                                    sizeof(dis_Comparator));
  saved_dis_gfnumeric_goal_fl = (int*)calloc(dis_gnum_fl_conn,sizeof(int));
  saved_dis_gfnumeric_goal_c = (float*)calloc(dis_gnum_fl_conn,sizeof(float));
  for(i=0; i<dis_gnum_fnumeric_goal; i++) {
    saved_dis_gfnumeric_goal_comp[i] = dis_gfnumeric_goal_comp[i];
    saved_dis_gfnumeric_goal_fl[i] = dis_gfnumeric_goal_fl[i];
    saved_dis_gfnumeric_goal_c[i] = dis_gfnumeric_goal_c[i];
  }      
  
  // Chih-Wei PDDL3
  // set DP bit
  for(i=0; i<dis_gnum_ef_conn; i++) {
    if ((dis_gop_conn[dis_gef_conn[i].op].action)->name &&
    strncmp((dis_gop_conn[dis_gef_conn[i].op].action)->name,
                "deripred", 8) == 0){
        dis_gef_conn[i].DPop = dis_TRUE;
        GpG.is_deripred = TRUE;
        
       // for(j=0; j<dis_gef_conn[i].num_PC; j++) {
       //    dis_print_ft_name(dis_gef_conn[i].PC[j]); printf(" ");
       //}
       //dis_print_ft_name(dis_gef_conn[i].A[0]); printf("\n"); 
    } else {
        dis_gef_conn[i].DPop = dis_FALSE;
    }
  }                                 
 
  if(GpG.is_deripred) {
        always_true_DPop();
        dis_set_DPft_flag();      
  }
  
  printf("\n\n"); 
  /*
  if ( dis_gcmd_line.ehc ) {
    found_plan = dis_do_enforced_hill_climbing();
    
    if ( !found_plan ) {
      printf("\n\nEnforced Hill-climbing failed !");
      printf("\nswitching to Best-first Search now.\n");
      found_plan = dis_do_best_first_search();
    }
  } else {
    found_plan = dis_do_best_first_search();
  }

  times( &end );
  dis_TIME( dis_gsearch_time );

  if ( found_plan ) {
    dis_print_plan();
  }

  dis_lfound_plan = found_plan;
  dis_output_planner_info();

  printf("\n\n");
  exit( 0 );
  */
}









/*
 *  ----------------------------- HELPING FUNCTIONS ----------------------------
 */












void dis_output_planner_info( void )

{

  printf( "\n\ntime spent: %7.2f seconds instantiating %d easy, %d hard action templates", 
	  dis_gtempl_time, dis_gnum_easy_templates, dis_gnum_hard_mixed_operators );
  printf( "\n            %7.2f seconds reachability analysis, yielding %d facts and %d actions", 
	  dis_greach_time, dis_gnum_pp_facts, dis_gnum_actions );
  printf( "\n            %7.2f seconds creating final representation with %d relevant facts, %d relevant fluents", 
	  dis_grelev_time, dis_gnum_relevant_facts, dis_gnum_relevant_fluents );
  printf( "\n            %7.2f seconds computing LNF",
	  dis_gLNF_time );
  printf( "\n            %7.2f seconds building connectivity graph",
	  dis_gconn_time );
  printf( "\n            %7.2f seconds searching, evaluating %d states, to a max depth of %d", 
	  dis_gsearch_time, dis_gevaluated_states, dis_gmax_search_depth );
  printf( "\n            %7.2f seconds total time", 
	  dis_gtempl_time + dis_greach_time + dis_grelev_time + dis_gLNF_time + dis_gconn_time + dis_gsearch_time );

  printf("\n\n");

  if ( dis_lfound_plan ) {
    dis_print_official_result();
  }

  exit( 0 );

}


FILE *out;

void dis_print_official_result( void )

{

  int i;
  char name[dis_MAX_LENGTH];

  sprintf( name, "%s.soln", dis_gcmd_line.fct_file_name );

  if ( (out = fopen( name, "w")) == NULL ) {
    printf("\n\nCan't open official output file!\n\n");
    return;
  }

  times( &dis_lend );
  fprintf(out, "Time %d\n", 
	 (int) ((dis_lend.tms_utime - dis_lstart.tms_utime + dis_lend.tms_stime - dis_lstart.tms_stime) * 10.0));

  for ( i = 0; i < dis_gnum_plan_ops; i++ ) {
    dis_print_official_op_name( dis_gplan_ops[i] );
    fprintf(out, "\n");
  }

  fclose( out );

}



void dis_print_official_op_name( int index )

{

  int i;
  dis_Action *a = dis_gop_conn[index].action;

  if ( a->norm_operator ||
       a->pseudo_action ) {
    fprintf(out, "(%s", a->name ); 
    for ( i = 0; i < a->num_name_vars; i++ ) {
      fprintf(out, " %s", dis_gconstants[a->name_inst_table[i]]);
    }
    fprintf(out, ")");
  }

}




void dis_ff_usage( void )

{

  printf("\nusage of ff:\n");

  printf("\nOPTIONS   DESCRIPTIONS\n\n");
  printf("-p <str>    path for operator and fact file\n");
  printf("-o <str>    operator file name\n");
  printf("-f <str>    fact file name\n\n");

  printf("-E          don't do enforced hill-climbing try before bestfirst\n\n");

  printf("-g <num>    set weight w_g in w_g*g(s) + w_h*h(s) [preset: %d]\n",
	 dis_gcmd_line.g_weight);
  printf("-h <num>    set weight w_h in w_g*g(s) + w_h*h(s) [preset: %d]\n\n",
	 dis_gcmd_line.h_weight);

  printf("-O          switch on optimization expression (default is plan length)\n\n");

  if ( 0 ) {
    printf("-i <num>    run-time information level( preset: 1 )\n");
    printf("      0     only times\n");
    printf("      1     problem name, planning process infos\n");
    printf("    101     parsed problem data\n");
    printf("    102     cleaned up ADL problem\n");
    printf("    103     collected string tables\n");
    printf("    104     encoded domain\n");
    printf("    105     predicates inertia info\n");
    printf("    106     splitted initial state\n");
    printf("    107     domain with Wff s normalized\n");
    printf("    108     domain with dis_NOT conds translated\n");
    printf("    109     splitted domain\n");
    printf("    110     cleaned up easy domain\n");
    printf("    111     unaries encoded easy domain\n");
    printf("    112     effects multiplied easy domain\n");
    printf("    113     inertia removed easy domain\n");
    printf("    114     easy action templates\n");
    printf("    115     cleaned up hard domain representation\n");
    printf("    116     mixed hard domain representation\n");
    printf("    117     final hard domain representation\n");
    printf("    118     reachability analysis results\n");
    printf("    119     facts selected as relevant\n");
    printf("    120     final domain and problem representations\n");
    printf("    121     normalized expressions representation\n");
    printf("    122     LNF: translated subtractions representation\n");
    printf("    123     summarized effects LNF  representation\n");
    printf("    124     encoded LNF representation\n");
    printf("    125     connectivity graph\n");
    printf("    126     fixpoint result on each evaluated state\n");
    printf("    127     1P extracted on each evaluated state\n");
    printf("    128     H set collected for each evaluated state\n");
    
    
    /*    printf("    125     False sets of goals <GAM>\n"); */
    /*    printf("    126     detected ordering constraints leq_h <GAM>\n"); */
    /*    printf("    127     the Goal Agenda <GAM>\n"); */
    
    
    
    /*   printf("    109     reachability analysis results\n"); */
    /*   printf("    110     final domain representation\n"); */
    /*   printf("    111     connectivity graph\n"); */
    /*   printf("    112     False sets of goals <GAM>\n"); */
    /*   printf("    113     detected ordering constraints leq_h <GAM>\n"); */
    /*   printf("    114     the Goal Agenda <GAM>\n"); */
    /*   printf("    115     fixpoint result on each evaluated state <1Ph>\n"); */
    /*   printf("    116     1P extracted on each evaluated state <1Ph>\n"); */
    /*   printf("    117     H set collected for each evaluated state <1Ph>\n"); */
    
    printf("\n-d <num>    switch on debugging\n\n");
  }

}



dis_Bool dis_processdis__command_line( int argc, char *argv[] )

{

  char option;

  dis_gcmd_line.display_info = 1;
  dis_gcmd_line.debug = 0;

  dis_gcmd_line.ehc = dis_TRUE;
  dis_gcmd_line.optimize = dis_FALSE;

  /* default: greedy best first search.
   */
  dis_gcmd_line.g_weight = 1;
  dis_gcmd_line.h_weight = 1;
  
  memset(dis_gcmd_line.ops_file_name, 0, dis_MAX_LENGTH);
  memset(dis_gcmd_line.fct_file_name, 0, dis_MAX_LENGTH);
  memset(dis_gcmd_line.path, 0, dis_MAX_LENGTH);

  while ( --argc && ++argv ) {
    if ( *argv[0] != '-' || strlen(*argv) != 2 ) {
      return dis_FALSE;
    }
    option = *++argv[0];
    switch ( option ) {
    case 'E':
      dis_gcmd_line.ehc = dis_FALSE;
      break;
    case 'O':
      dis_gcmd_line.optimize = dis_TRUE;
      dis_gcmd_line.ehc = dis_FALSE;
      break;      
    default:
      if ( --argc && ++argv ) {
	switch ( option ) {
	case 'p':
	  strncpy( dis_gcmd_line.path, *argv, dis_MAX_LENGTH );
	  break;
	case 'o':
	  strncpy( dis_gcmd_line.ops_file_name, *argv, dis_MAX_LENGTH );
	  break;
	case 'f':
	  strncpy( dis_gcmd_line.fct_file_name, *argv, dis_MAX_LENGTH );
	  break;
	case 'i':
	  sscanf( *argv, "%d", &dis_gcmd_line.display_info );
	  break;
	case 'd':
	  sscanf( *argv, "%d", &dis_gcmd_line.debug );
	  break;
	case 'g':
	  sscanf( *argv, "%d", &dis_gcmd_line.g_weight );
	  break;
	case 'h':
	  sscanf( *argv, "%d", &dis_gcmd_line.h_weight );
	  break;
    default:
	  printf( "\nff: unknown option: %c entered\n\n", option );
	//  return dis_FALSE;
	}
      } else {
	return dis_FALSE;
      }
    }
  }

  if ( dis_gcmd_line.ehc &&
       dis_gcmd_line.optimize ) {
    printf("\n\nff: no enforced hill-climbing when optimizing expressions.\n\n");
    return dis_FALSE;
  }

  return dis_TRUE;

}

