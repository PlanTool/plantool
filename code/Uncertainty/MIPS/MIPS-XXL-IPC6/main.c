


/*********************************************************************
 * (C) Copyright 2002 Albert Ludwigs University Freiburg
 *     Institute of Computer Science
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 * 
 *********************************************************************/

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
 * Description: The main routine for the Metric-FastForward Planner.
 *
 * Author: Joerg Hoffmann 2001 / 2002
 * 
 *********************************************************************/ 








#include "ff.h"

#include "memory.h"
#include "output.h"

#include "parse.h"

#include "expressions.h"

#include "inst_pre.h"
#include "inst_easy.h"
#include "inst_hard.h"
#include "inst_final.h"

#include "relax.h"
#include "search.h"

#include "grounded.h"
#include "external.h"
#include <unistd.h>
#include <signal.h>
#include <limits.h>




/*
 *  ----------------------------- GLOBAL VARIABLES ----------------------------
 */









/*******************
 * GENERAL HELPERS *
 *******************/



#define MAXTIME 300




/* used to time the different stages of the planner
 */
float gtempl_time = 0, greach_time = 0, grelev_time = 0, gconn_time = 0;
float gLNF_time = 0, gsearch_time = 0;
float gpert_time = 0;

/* the command line inputs
 */
struct _command_line gcmd_line;

/* number of states that got heuristically evaluated
 */
int gevaluated_states = 0;

/* maximal depth of breadth first search
 */
int gmax_search_depth = 0;


/*for pddl 2.2 and pddl 3
**/

Bool isfluents = FALSE;

Bool isDurative = FALSE;

Bool isTotalCost = FALSE;

Bool isTempTimeWindows = FALSE;

Bool isConstraints = FALSE;

Bool isPreference = FALSE;

Bool isDomainPreference = FALSE;

Bool isConstraintsPreference = FALSE; 

PlNode *pref_head = NULL;

WffNode *gpref_head = NULL;

PlNode *domain_pref_head;

PlNode *pref_constraints_head = NULL;

PlNode *pref_constraints_at_end = NULL;

PlNode *constraints_at_end = NULL;

PlNode *til_constraints;

PlNode *within_constraints;

PlNode *til_constraints_pref;

PlNode *within_constraints_pref;

WffNode *gconstraints = NULL;

WffNode *gconstraints_pref = NULL;

WffNode *gtil_constraints = NULL;

WffNode *gwithin_constraints_pref = NULL;

WffNode *gwithin_constraints = NULL;

WffNode *gtil_constraints_pref = NULL;

char automata_name[30];

float prune;

int *relevant_pref_preconds_predicate;
int gnum_pref_preconds_predicate;

TokenList_pointer *relevant_pref_predicate;
int gnum_pref_predicate;

Fact_pointer *relevant_gpref_predicate;
int gnum_gpref_predicate;

/***********
 * PARSING *
 ***********/







/* used for pddl parsing, flex only allows global variables
 */
int gbracket_count;
char *gproblem_name;

/* The current input line number
 */
int lineno = 1;

/* The current input filename
 */
char *gact_filename;

/* The pddl domain name
 */
char *gdomain_name = NULL;

/* loaded, uninstantiated operators
 */
PlOperator *gloaded_ops = NULL;

/* For timed initial literals*/
Time_Ini_Literal *Timed_Initial_Literals=NULL;

/* stores initials as fact_list 
 */
PlNode *gorig_initial_facts = NULL;

/* not yet preprocessed goal facts
 */
PlNode *gorig_goal_facts = NULL;

/* not yet preprocessed constraints facts
 */
PlNode *gorig_constraints_facts = NULL;

/* not yet preprocessed constraints facts
 */
PlNode *gorig_domain_constraints_facts = NULL;


/* axioms as in UCPOP before being changed to ops
 */
PlOperator *gloaded_axioms = NULL;

/* the types, as defined in the domain file
 */
TypedList *gparse_types = NULL;

/* the constants, as defined in domain file
 */
TypedList *gparse_constants = NULL;

/* the predicates and their arg types, as defined in the domain file
 */
TypedListList *gparse_predicates = NULL;

/* the functions and their arg types, as defined in the domain file
 */
TypedListList *gparse_functions = NULL;

/* the objects, declared in the problem file
 */
TypedList *gparse_objects = NULL;

/* the metric
 */
Token gparse_optimization;
ParseExpNode *gparse_metric = NULL;


/* connection to instantiation ( except ops, goal, initial )
 */

/* all typed objects 
 */
FactList *gorig_constant_list = NULL;

/* the predicates and their types
 */
FactList *gpredicates_and_types = NULL;

/* the functions and their types
 */
FactList *gfunctions_and_types = NULL;

/*  variable for user xml
 */
Bool breakForUser = FALSE;

XMLOperator *op_depend_op=NULL;

XMLOperator_pointer *xml_operators=NULL;

TokenListOperator_pointer *instance_Operators=NULL;


Bool graph_heuristic = FALSE;
Bool sched_heuristic = FALSE;






/*****************
 * INSTANTIATING *
 *****************/









/* global arrays of constant names,
 *               type names (with their constants),
 *               predicate names,
 *               predicate aritys,
 *               defined types of predicate args
 */
Token gconstants[MAX_CONSTANTS];
int gnum_constants = 0;
Token gtype_names[MAX_TYPES];
int gtype_consts[MAX_TYPES][MAX_TYPE];
Bool gis_member[MAX_CONSTANTS][MAX_TYPES];
int gtype_size[MAX_TYPES];
int gnum_types = 0;
Token gpredicates[MAX_PREDICATES];
int garity[MAX_PREDICATES];
int gpredicates_args_type[MAX_PREDICATES][MAX_ARITY];
int gnum_predicates = 0;
Token gfunctions[MAX_FUNCTIONS];
int gf_arity[MAX_FUNCTIONS];
int gfunctions_args_type[MAX_FUNCTIONS][MAX_ARITY];
int gnum_functions = 0;





/* the domain in integer (Fact) representation
 */
Operator_pointer goperators[MAX_OPERATORS];
int gnum_operators = 0;
Fact *gfull_initial;
int gnum_full_initial = 0;
FluentValue *gfull_fluents_initial;
int gnum_full_fluents_initial = 0;
WffNode *ggoal = NULL;

ExpNode *gmetric = NULL;



/* stores inertia - information: is any occurence of the predicate
 * added / deleted in the uninstantiated ops ?
 */
Bool gis_added[MAX_PREDICATES];
Bool gis_deleted[MAX_PREDICATES];


/* for functions we *might* want to say, symmetrically, whether it is
 * increased resp. decreased at all.
 *
 * that is, however, somewhat involved because the right hand
 * sides can be arbirtray expressions, so we have no guarantee
 * that increasing really does adds to a functions value...
 *
 * thus (for the time being), we settle for "is the function changed at all?"
 */
Bool gis_changed[MAX_FUNCTIONS];



/* splitted initial state:
 * initial non static facts,
 * initial static facts, divided into predicates
 * (will be two dimensional array, allocated directly before need)
 */
Facts *ginitial = NULL;
int gnum_initial = 0;
Fact **ginitial_predicate;
int *gnum_initial_predicate;

/* same thing for functions
 */
FluentValues *gf_initial;
int gnum_f_initial = 0;
FluentValue **ginitial_function;
int *gnum_initial_function;



/* the type numbers corresponding to any unary inertia
 */
int gtype_to_predicate[MAX_PREDICATES];
int gpredicate_to_type[MAX_TYPES];

/* (ordered) numbers of types that new type is intersection of
 */
TypeArray gintersected_types[MAX_TYPES];
int gnum_intersected_types[MAX_TYPES];



/* splitted domain: hard n easy ops
 */
Operator_pointer *ghard_operators;
int gnum_hard_operators;
NormOperator_pointer *geasy_operators;
int gnum_easy_operators;



/* so called Templates for easy ops: possible inertia constrained
 * instantiation constants
 */
EasyTemplate *geasy_templates;
int gnum_easy_templates;



/* first step for hard ops: create mixed operators, with conjunctive
 * precondition and arbitrary effects
 */
MixedOperator *ghard_mixed_operators;
int gnum_hard_mixed_operators;



/* hard ''templates'' : pseudo actions
 */
PseudoAction_pointer *ghard_templates;
int gnum_hard_templates;



/* store the final "relevant facts"
 */
Fact grelevant_facts[MAX_RELEVANT_FACTS];
int gnum_relevant_facts = 0;
int gnum_pp_facts = 0;
/* store the "relevant fluents"
 */
Fluent grelevant_fluents[MAX_RELEVANT_FLUENTS];
int gnum_relevant_fluents = 0;
Token grelevant_fluents_name[MAX_RELEVANT_FLUENTS];
/* this is NULL for normal, and the LNF for
 * artificial fluents.
 */
LnfExpNode_pointer grelevant_fluents_lnf[MAX_RELEVANT_FLUENTS];



/* the final actions and problem representation
 */
Action *gactions = NULL;
int gnum_actions;
State ginitial_state;
int *glogic_goal = NULL;
int gnum_logic_goal = 0;
Comparator *gnumeric_goal_comp = NULL;
ExpNode_pointer *gnumeric_goal_lh = NULL, *gnumeric_goal_rh = NULL;
int gnum_numeric_goal = 0;

/* direct numeric goal access
 */
Comparator *gnumeric_goal_direct_comp;
float *gnumeric_goal_direct_c;



/* to avoid memory leaks; too complicated to identify
 * the exact state of the action to throw away (during construction),
 * memory gain not worth the implementation effort.
 */
Action *gtrash_actions = NULL;



/* additional lnf step between finalized inst and
 * conn graph
 */
Comparator *glnf_goal_comp = NULL;
LnfExpNode_pointer *glnf_goal_lh = NULL;
float *glnf_goal_rh = NULL;
int gnum_lnf_goal = 0;

LnfExpNode glnf_metric;
Bool goptimization_established = FALSE;







/**********************
 * CONNECTIVITY GRAPH *
 **********************/







/* one ops (actions) array ...
 */
OpConn *gop_conn;
int gnum_op_conn;



/* one effects array ...
 */
EfConn *gef_conn;
int gnum_ef_conn;



/* one facts array.
 */
FtConn *gft_conn;
int gnum_ft_conn;



/* and: one fluents array.
 */
FlConn *gfl_conn;
int gnum_fl_conn;
int gnum_real_fl_conn;/* number of non-artificial ones */



/* final goal is also transformed one more step.
 */
int *gflogic_goal = NULL;
int gnum_flogic_goal = 0;
Comparator *gfnumeric_goal_comp = NULL;
int *gfnumeric_goal_fl = NULL;
float *gfnumeric_goal_c = NULL;
int gnum_fnumeric_goal = 0;

/* direct access (by relevant fluents)
 */
Comparator *gfnumeric_goal_direct_comp = NULL;
float *gfnumeric_goal_direct_c = NULL;











/*******************
 * SEARCHING NEEDS *
 *******************/











/* applicable actions
 */
int *gA;
int gnum_A;



/* communication from extract 1.P. to search engine:
 * 1P action choice
 */
int *gH;
int gnum_H;
/* cost of relaxed plan
 */
float gcost;



/* to store plan
 */
int gplan_ops[MAX_PLAN_LENGTH];
float gplan_ops_durations[MAX_PLAN_LENGTH];
int gnum_plan_ops = 0;



/* stores the states that the current plan goes through
 * ( for knowing where new agenda entry starts from )
 */
State gplan_states[MAX_PLAN_LENGTH + 1];







/* dirty: multiplic. of total-time in final metric LNF
 */
float gtt;







/* the mneed structures
 */
Bool **gassign_influence;
Bool **gTassign_influence;



/* the real var input to the mneed computation.
 */
Bool *gmneed_start_D;
float *gmneed_start_V;



/* does this contain conditional effects?
 * (if it does then the state hashing has to be made more
 *  cautiously)
 */
Bool gconditional_effects;











/*
 *  ----------------------------- HEADERS FOR PARSING ----------------------------
 * ( fns defined in the scan-* files )
 */







void get_fct_file_name( char *filename );
void load_ops_file( char *filename );
void load_fct_file( char *filename );


void cancel_bfs();
void make_File();

void load_xml_file();
/*
 *  ----------------------------- MAIN ROUTINE ----------------------------
 */





struct tms lstart, lend;



Bool lfound_plan;


int main( int argc, char *argv[] )

{

  /* resulting name for ops file
   */
  char ops_file[MAX_LENGTH] = "";
  /* same for fct file 
   */
  char fct_file[MAX_LENGTH] = "";
  
  struct tms start, end;

  Bool found_plan;

  times ( &lstart );

  /* command line treatment
   */
  if ( argc == 1 || ( argc == 2 && *++argv[0] == '?' ) ) {
    ff_usage();
    exit( 1 );
  }
  if ( !process_command_line( argc, argv ) ) {
    ff_usage();
    exit( 1 );
  }

  /* system("sed '/(:constraints/! s/constraints/constraint/g' $2 > dfile");*/

  /* make file names
   */

  /* one input name missing
   */
  if ( !gcmd_line.ops_file_name || 
       !gcmd_line.fct_file_name ) {
    fprintf(stdout, "\nff: two input files needed\n\n");
    ff_usage();      
    exit( 1 );
  }/* system("sed '/(:constraints/! s/constraints/constraint/g' $2 > dfile");*/
  /* add path info, complete file names will be stored in
   * ops_file and fct_file 
   */
  sprintf(ops_file, "%s%s", gcmd_line.path, gcmd_line.ops_file_name);
  sprintf(fct_file, "%s%s", gcmd_line.path, gcmd_line.fct_file_name);


  /* parse the input files
   */

  /* start parse & instantiation timing
   */
  times( &start );

 
  /* domain file (ops)
   */
  if ( gcmd_line.display_info >= 1 ) {
    printf("\nff: parsing domain file");
  } 
  /* it is important for the pddl language to define the domain before 
   * reading the problem 
   */
  load_ops_file( ops_file );
  /* problem file (facts)
   */  
  if ( gcmd_line.display_info >= 1 ) {
    printf(" ... done.\nff: parsing problem file"); 
  }
  load_fct_file( fct_file );
  if ( gcmd_line.display_info >= 1 ) {
    printf(" ... done.\n\n");
  }

  
  /*********************************************************/

   /*-- Test for Time Windows Literals Domain and problem-------*/ 

      if(isPreference){
	  if( mark_preference_in_goal(gorig_goal_facts)){
	      pref_head = extract_preference_from_goal(gorig_goal_facts);
	  }
      }

     /*add constraints domain to constraints problem
     **/
     PlNode *i_son;
       if(gorig_constraints_facts && gorig_constraints_facts->sons){

            if(gorig_domain_constraints_facts && gorig_domain_constraints_facts->sons){
		for(i_son = gorig_constraints_facts->sons; i_son->next; i_son = i_son->next );
                i_son->next = gorig_domain_constraints_facts->sons;
                  
            }        
   
       }
       else{
             if(gorig_domain_constraints_facts && gorig_domain_constraints_facts->sons){
	       gorig_constraints_facts = gorig_domain_constraints_facts;
                  
            }        

       }


   if(isConstraintsPreference){
     
       if(mark_preference_in_goal(gorig_constraints_facts)){
	       pref_constraints_head = extract_preference_from_goal(gorig_constraints_facts);
       }

        if(pref_constraints_head && pref_constraints_head->sons){
            if(mark_at_end_in_constraints(pref_constraints_head))
                 pref_constraints_at_end = extract_at_end_from_constraints(pref_constraints_head);

	    if(pref_constraints_at_end){
                if(pref_head)
		 add_at_end_constraints_to_goal(pref_head,pref_constraints_at_end);           
		else{
		   isPreference = TRUE;
		   pref_head = pref_constraints_at_end; 
		}
	    }
		            
	}
       
       /*make til_constraints_pref
       **/
       til_constraints_pref = extract_constraints_for_timed_initial_literal(pref_constraints_head);   
  
       /*make within_constraints_pref
       **/
       within_constraints_pref = extract_constraints_for_within(pref_constraints_head);
   
   }

   	if(gorig_constraints_facts && gorig_constraints_facts->sons){
            if(mark_at_end_in_constraints(gorig_constraints_facts))
                     constraints_at_end = extract_at_end_from_constraints( gorig_constraints_facts);
	    if(constraints_at_end) {
		if (gorig_goal_facts){
		 add_at_end_constraints_to_goal(gorig_goal_facts,constraints_at_end);
		} else {
		     gorig_goal_facts = 
			 constraints_at_end;
		}
	    }
	}


 /*make til_constraints
 **/
  if(isConstraints)
       if(gorig_constraints_facts)
             til_constraints = extract_constraints_for_timed_initial_literal(gorig_constraints_facts);

    /*make within_constraints
    **/
    if(isConstraints)
       if(gorig_constraints_facts)
          within_constraints = extract_constraints_for_within(gorig_constraints_facts);
     
   
    if(isTempTimeWindows)
    {
      extract_Timed_Initial_Literals(); 

      setComplementTIL();      

      split_Preconds_inPlOperator(gloaded_ops);
 
      /* print_Timed_Initial_Literals();*/
     }

    /* This is needed to get all types.
    */
    build_orig_constant_list();

  /* last step of parsing: see if it's an ADL domain!
   */
  if ( !make_adl_domain() ) {
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
  encode_domain_in_integers();


  /* inertia preprocessing, first step:
   *   - collect inertia information
   *   - split initial state into
   *        - arrays for individual predicates
   *        - arrays for all static relations
   *        - array containing non - static relations
   */
  do_inertia_preprocessing_step_1();

  /* normalize all PL1 formulae in domain description:
   * (goal, preconds and effect conditions)
   *   - simplify formula
   *   - expand quantifiers
   *   - NOTs down
   */
  normalize_all_wffs();

  /* translate negative preconds: introduce symmetric new predicate
   * NOT-p(..) (e.g., not-in(?ob) in briefcaseworld)
   */
  translate_negative_preconds();

  /* split domain in easy (disjunction of conjunctive preconds)
   * and hard (non DNF preconds) part, to apply 
   * different instantiation algorithms
   */
  split_domain();

  /*make ltl for constraints and constraints preference
  **/


  if((gcmd_line.grounded) || (gcmd_line.adl)){

    if(isConstraints || isConstraintsPreference){
    strcpy(automata_name , "r-0");

    if(isConstraints){
        if(gconstraints){
            printf("\n...Reading constraints...\n");
            constraints_to_ltl(gconstraints,gorig_constraints_facts );
            printf("\n...ltl constraints done...\n");

        }
    }

    if(isConstraintsPreference){
        if(gconstraints_pref){
            printf("\n...Reading constraints preference...\n");
            constraints_to_ltl(gconstraints_pref,pref_constraints_head );
            printf("\n...ltl constraints preference done...\n");

        }
     }

    }
   }
    

  /***********************************************
   * PREPROCESSING FINISHED                      *
   *                                             *
   * NOW MULTIPLY PARAMETERS IN EFFECTIVE MANNER *
   ***********************************************/

  build_easy_action_templates();
  build_hard_action_templates();

  times( &end );
  TIME( gtempl_time );

  times( &start );

  /* perform reachability analysis in terms of relaxed 
   * fixpoint
   */
  perform_reachability_analysis();
   
  times( &end );
  TIME( greach_time );

  times( &start );

  /* collect the relevant facts and build final domain
   * and problem representations.
   */
  collect_relevant_facts_and_fluents();

  times( &end );
  TIME( grelev_time );


  /* make grounded task
  **/

  if(gcmd_line.grounded)     
  {
    /*printf("\ngrounded task\n");*/
    if((!isfluents) && (!isDurative))
       output_grounded_STRIPS_task();
    else
       output_grounded_STRIPS_Fluents_temporal_task();

/*  free_PlNode(domain_pref_head);
    domain_pref_head = NULL;
    free_PlNode(pref_constraints_head);
    pref_constraints_head = NULL;
    free_PlNode(pref_constraints_at_end);
    pref_constraints_at_end = NULL;
    free_PlNode(constraints_at_end);
    constraints_at_end = NULL;
    free_PlNode(til_constraints);
    til_constraints = NULL;
    free_PlNode(til_constraints_pref);
    til_constraints_pref = NULL;
    free_PlNode(within_constraints);
    within_constraints = NULL;
    free_PlNode(within_constraints_pref);
    within_constraints_pref = NULL;
    free_PlNode(pref_head);
    pref_head = NULL;

    free_WffNode(gpref_head);
    gpref_head = NULL;
    free_WffNode(gconstraints);
    gconstraints = NULL;
    free_WffNode(gtil_constraints);
    gtil_constraints = NULL;
    free_WffNode(gwithin_constraints);
    gwithin_constraints = NULL;
    free_WffNode(gconstraints_pref);
    gconstraints_pref = NULL;
    free_WffNode(gtil_constraints_pref);
    gtil_constraints_pref = NULL;
    free_WffNode(gwithin_constraints_pref);
    gwithin_constraints_pref = NULL;*/

    /*printf("end of grounded task\n");*/
    exit(0);
  }
  else
      if(gcmd_line.adl)
      {
       printf("\ngrounded adl task\n");
       /* if((!isfluents) && (!isDurative))
           output_grounded_SIMPLEADL_task();
	   else*/
           output_grounded_ADL_task();
       printf("end of grounded adl task\n");
       exit(0);
      }

  /*-- end grounded task --*/




  /* now transform problem to additive normal form,
   * if possible
   */
  times( &start );
  if ( !transform_to_LNF() ) {
    printf("\n\nThis is not a linear task!\n\n");
    exit( 1 );
  }
  times( &end );
  TIME( gLNF_time );
    
  
  times( &start );

  /* now build globally accessable connectivity graph
   */
  build_connectivity_graph();


  /* now check for acyclic := effects (in expressions.c)
   */
  check_assigncycles();
  /* set the relevanc info (in expressions.c)
   */
  determine_fl_relevance();

   
  times( &end );
  TIME( gconn_time );



  /***********************************************************
   * we are finally through with preprocessing and can worry *
   * bout finding a plan instead.                            *
   ***********************************************************/

  if ( gcmd_line.display_info ) {
    printf("\n\nMIPS-XXL: search configuration is ");
    if (gcmd_line.dijkstra){
      printf("External Dijkstra.");
    }
/*
    else {
      if ( gcmd_line.ehc ) {
	printf("EHC, if that fails then ");
      }
      printf(" best-first on %d*g(s) + %d*h(s) where\n    metric is ",
	     gcmd_line.g_weight, gcmd_line.h_weight);
    }
*/
    if ( gcmd_line.optimize && goptimization_established ) {
	printf("External Branch-and-Bound.");
	printf(" optimization");
	print_LnfExpNode( &glnf_metric );
    } else {
      printf(" plan length");
    }
  }

  times( &start );

  /* TODO: Need clean up. To ensure compatibility with the old shell scripts, where -B is always
   * used with -X, the cleaning is delayed */
  if (gcmd_line.dijkstra)
      found_plan = do_external_dijkstra();
  else if (gcmd_line.branch_and_bound)
    found_plan = do_external_branch_and_bound(gcmd_line.stop_at_first_solution);
  else if (gcmd_line.external){
    if (gcmd_line.external_merge){
      do_external_merge();
      exit(0);
    }
    
    if (gcmd_line.breadth)
      found_plan = do_external_breadth_first_search();
    else
      found_plan = do_external_enforced_hill_climbing();

      
  }else{

      if ( gcmd_line.ehc ) {
	  found_plan = do_enforced_hill_climbing();
	  
	  if ( !found_plan ) {
	      printf("\n\nEnforced Hill-climbing failed !");
	      printf("\nswitching to Best-first Search now.\n");
	      found_plan = do_best_first_search();
	  }
      } else {
          if(!fopen("foundplan", "r")){
	     signal(SIGALRM, cancel_bfs);
	     if(isTempTimeWindows==FALSE)
		 alarm(MAXTIME);
	     else
		 alarm(1800);
	  }
	  found_plan = do_best_first_search();
          if(found_plan){
	      make_File();
	  }
      }
  }
  times( &end );
  TIME( gsearch_time );

  times( &start );

  if ( found_plan ) {
    int temp[MAX_PLAN_LENGTH];
    int number=0;
    int i;   
    
    for(i=0; i<gnum_plan_ops; i++) {
	if (!isAutomaton(gop_conn[gplan_ops[i]].action->name))
 	    temp[number++]= gplan_ops[i];
    }

    gnum_plan_ops = number;
    for(i=0; i<gnum_plan_ops; i++) {
	gplan_ops[i] = temp[i];
    }

    print_plan();
    
     if(isDurative==TRUE){
	 /* printf("Makespan not available\n");*/
	 if(isTempTimeWindows==FALSE)
	     printf("\n  %.2f\n", durativpert());
	 else
	     calculate_best_makespann();
     }
     else{
	 /*printf("Parallel Plan not available\n");*/
	 printf("\npert: %d\n", pert());
     }
    


    
    if(breakForUser) {

      system("lib/pollo-0.4-bin/bin/pollo.sh Depend.xml");
      printf("reached breakpoint... the user can edit the file now...\n");

        load_xml_file();
        printf("Parsing XML ... done.\n\n");
       
        if(isDurative==TRUE)
          printf("\n  %.2f", User_durative_pert());
        else
          printf("\n  %.2f", User_pert());
        
       }
  }

  times( &end );
    TIME( gpert_time );


  lfound_plan = found_plan;
  output_planner_info();

/*  printf("Executing commands ..\n");
  system("automata/add_newgoal automata/problem-merge.pddl newgoal.pddl");
  
  system("rm newgoal.pddl");
  system("mv automata/new-problem-merge.pddl automata/problem-merge.pddl");
  printf("Done\n");

 
  goto START;
  printf("\n\n");
*/
  printf ("Complete exploration!");
  exit(0);

}

void cancel_bfs(){
    printf("\n%d Seconds are over .. Cancelling the execution of BFS\n", MAXTIME);
    FILE *f;
    f = fopen("BFSCANCELED", "w");
    fclose(f);
    exit(1);
}

void make_File(){
    FILE *f;
    f = fopen("foundplan", "w");
    fclose(f);
}



/*
 *  ----------------------------- HELPING FUNCTIONS ----------------------------
 */





 


void output_planner_info( void )

{
/*
  printf( "\n\ntime spent: %7.2f seconds instantiating %d easy, %d hard action templates", 
	  gtempl_time, gnum_easy_templates, gnum_hard_mixed_operators );
  printf( "\n            %7.2f seconds reachability analysis, yielding %d facts and %d actions", 
	  greach_time, gnum_pp_facts, gnum_actions );
  printf( "\n            %7.2f seconds creating final representation with %d relevant facts, %d relevant fluents", 
	  grelev_time, gnum_relevant_facts, gnum_relevant_fluents );
  printf( "\n            %7.2f seconds computing LNF",
	  gLNF_time );
  printf( "\n            %7.2f seconds building connectivity graph",
	  gconn_time );
  printf( "\n            %7.2f seconds searching, evaluating %d states, to a max depth of %d", 
	  gsearch_time, gevaluated_states, gmax_search_depth );
  printf( "\n            %7.2f seconds total time", 
	  gtempl_time + greach_time + grelev_time + gLNF_time + gconn_time + gsearch_time + gpert_time );

  printf("\n\n");
*/
 

}



void ff_usage( void )

{

  printf("\nusage of MIPS-XXL:\n");

  printf("\nOPTIONS   DESCRIPTIONS\n\n");
  printf("-p <str>    path for operator and fact file\n");
  printf("-o <str>    operator file name\n");
  printf("-f <str>    fact file name\n\n");

  printf("-E          don't do enforced hill-climbing try before bestfirst\n\n"); 
  printf(" - External - \n");
  printf("-X          do external enforced hill-climbing\n");
  printf("-B          do external breadth-first search\n");
  printf("-k <num>    beam size for pruning; valid for both Ex-EHC and Ex-BFS\n");
  printf("-M          do external merge\n");
  printf("-D          do external Dijkstra\n");
  printf("-Z          do external branch and bound\n");
  printf("-S          Stop at the first solution\n");
  printf("---------------\n");

  printf("-g <num>    set weight w_g in w_g*g(s) + w_h*h(s) [preset: %d]\n",
	 gcmd_line.g_weight);
  printf("-h <num>    set weight w_h in w_g*g(s) + w_h*h(s) [preset: %d]\n\n",
	 gcmd_line.h_weight);

  printf("-O          switch on optimization expression (default is plan length)\n\n");
   printf("-G          output normal task\n\n");
   printf("-A          output ADL task instead of STRIPS\n\n");

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
    printf("    108     domain with NOT conds translated\n");
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



Bool process_command_line( int argc, char *argv[] )

{

  char option;

  gcmd_line.display_info = 1;
  gcmd_line.debug = 0;

  gcmd_line.ehc = TRUE;
  gcmd_line.optimize = FALSE;
  gcmd_line.adl = FALSE;
  gcmd_line.grounded = FALSE;
  gcmd_line.external = FALSE;
  gcmd_line.breadth = FALSE;
  gcmd_line.external_merge = FALSE;
  gcmd_line.beamsize = UINT_MAX;
  gcmd_line.dijkstra = FALSE;
  /* default: greedy best first search.
   */
  gcmd_line.g_weight = 1;
  gcmd_line.h_weight = 5;
  
  memset(gcmd_line.ops_file_name, 0, MAX_LENGTH);
  memset(gcmd_line.fct_file_name, 0, MAX_LENGTH);
  memset(gcmd_line.path, 0, MAX_LENGTH);

  while ( --argc && ++argv ) {
    if ( *argv[0] != '-' || strlen(*argv) != 2 ) {
      return FALSE;
    }
    option = *++argv[0];
    switch ( option ) {
	case 'E':
	    gcmd_line.ehc = FALSE;
	    break;
	case 'O':
	    gcmd_line.optimize = TRUE;
	    gcmd_line.ehc = FALSE;
	    break;
	case 'X':
	    gcmd_line.external = TRUE;
	    break;
        case 'D':
            gcmd_line.dijkstra = TRUE;
	    isTotalCost = TRUE;
	    break;
        case 'Z':
            gcmd_line.branch_and_bound = TRUE;
	    isTotalCost = TRUE;
	    break;
	case 'B':
	    gcmd_line.breadth = TRUE;
	    break;
	case 'M':
	    gcmd_line.external = TRUE;
	    gcmd_line.external_merge    = TRUE;
	    break;
	case 'S':
	    gcmd_line.stop_at_first_solution = TRUE;
	    break;
	   
    case 'A':
          gcmd_line.adl = TRUE;
      break;
    case 'G':
      gcmd_line.grounded = TRUE;
      break;
    case 'u':
      breakForUser = TRUE;
      break; 
    case 's':
      sched_heuristic = TRUE;
      break; 
    case 'r':
      graph_heuristic = TRUE;
      break;      
    default:
      if ( --argc && ++argv ) {
	switch ( option ) {
	case 'p':
	  strncpy( gcmd_line.path, *argv, MAX_LENGTH );
	  break;
	case 'o':
	  strncpy( gcmd_line.ops_file_name, *argv, MAX_LENGTH );
	  break;
	case 'f':
	  strncpy( gcmd_line.fct_file_name, *argv, MAX_LENGTH );
	  break;
        case 'k':
	  sscanf( *argv, "%d", &gcmd_line.beamsize );
	  break;
	case 'i':
	  sscanf( *argv, "%d", &gcmd_line.display_info );
	  break;
	case 'd':
	  sscanf( *argv, "%d", &gcmd_line.debug );
	  break;
	case 'g':
	  sscanf( *argv, "%d", &gcmd_line.g_weight );
	  break;
	case 'h':
	  sscanf( *argv, "%d", &gcmd_line.h_weight );
	  break;
	default:
	  printf( "\nff: unknown option: %c entered\n\n", option );
	  return FALSE;
	}
      } else {
	return FALSE;
      }
    }
  }

  if ( gcmd_line.ehc &&
       gcmd_line.optimize ) {
    printf("\n\nff: no enforced hill-climbing when optimizing expressions.\n\n");
    return FALSE;
  }

  return TRUE;

}




