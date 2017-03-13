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

 * File: main.c

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
 * File: main.c 
 * Description:  Main routines of LPG.
 *
 *   PDDL 2.1 version without conditional and quantified effects 
 *
 * Authors: Alfonso Gerevini, Marco Lazzaroni, Alessandro Saetti, 
 *          Ivan Serina, Sergio Spinoni
 *
 *********************************************************************/ 



#include <math.h>
#include <sys/time.h>
#include "lpg.h"
#include "parse.h"
#include "inst_easy.h"
#include "inst_hard.h"
#include "inst_pre.h"
#include "inst_final.h"
#include "inst_utils.h"
#include "check.h"
#include "utilities.h"
#include "numeric.h"
#include "LpgOutput.h"
#include "output.h"
#include "mutex.h"
#include "LocalSearch.h"
#include "ActionSubgraph.h"
#include "ComputeReachInf.h"
#include "search.h"
#include "relax.h"
#include "orderings.h"
#include "stripsff.h"
#include "subspace.h"
#include "dis_ff.h"

/******************************************************************************
 *                             GLOBAL VARIABLES                               *
 ******************************************************************************/

extern void mffDistributedSearch(char *, char *);
extern void DistributeSearch(State *, State *, PlanAction **);
extern char *dis_copy_dis_Token(char *);
/**
 * PARSING
 **/

/* used for pddl parsing, flex only allows global variables */
int gbracket_count;
char *gproblem_name;

/* The current input line number */
int lineno = 1;

/* The current input filename */
char *gact_filename;

/* The pddl domain name */
char *gdomain_name = NULL;

/* loaded, uninstantiated operators */
PlOperator *gloaded_ops = NULL;

PlOperator *gloaded_pl2ops = NULL;

/* constraints */
ConNode *gloaded_constraints = NULL;
dis_ConNode *dis_gloaded_constraints = NULL;
/* preferences */
PrefNode *gloaded_preferences = NULL;
dis_PrefNode *dis_gloaded_preferences = NULL;
int num_preference = 0, num_constraint = 0;
int dis_num_preference = 0, dis_num_constraint = 0;

int num_dur = 0;
/* derived predicates */
PlDP *gloaded_dps = NULL;

/* stores initials as fact_list */
PlNode *gorig_initial_facts = NULL;

/* not yet preprocessed goal facts  */

PlNode *gorig_goal_facts = NULL;

/* metric for the plan*/
PlNode *gmetric_exp = NULL;

/* axioms as in UCPOP before being changed to ops */
PlOperator *gloaded_axioms = NULL;

/* the types, as defined in the domain file */
TypedList *gparse_types = NULL;

/* the constants, as defined in domain file */
TypedList *gparse_constants = NULL;

/* the predicates and their arg types, as defined in the domain file */
TypedListList *gparse_predicates = NULL;

/* PDDL2--*/
TypedListList *gparse_functions = NULL;

/* the objects, declared in the problem file */
TypedList *gparse_objects = NULL;

/* connection to instantiation ( except ops, goal, initial ) */

/* all typed objects  */
FactList *gorig_constant_list = NULL;

/* the predicates and their types */
FactList *gpredicates_and_types = NULL;

FactList *gfunctions_and_types = NULL;



/**
 * INSTANTIATING
 **/

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
int gfunarity[MAX_FUNCTIONS];
int gfunctions_args_type[MAX_FUNCTIONS][MAX_ARITY];
int gnum_functions = 0;

/* derived predicates */
int gnum_deripreds = 0;
DerivePred* gderipreds;

/* timed initial literals */
int gnum_tils = 0;
TimedInitial *gtils = NULL, *g_tils = NULL;



/* the domain in integer (Fact) representation
 */
Operator_pointer goperators[MAX_OPERATORS];
int gnum_operators = 0;
Fact gfull_initial[MAX_INITIAL];
int gnum_full_initial = 0;


NumVar **gfullnum_initial = NULL;
int gnum_fullnum_initial = 0;
int gnum_fullnum_blocks;

int gnum_mipsnum = 0;
int gmipsnum[MAXMIPSNUM];
		
int max_num_value = 30000; //MAX_NUM_INITIAL

int gnum_comp_var = 0;
int gnum_block_compvar;
int *gis_inertial = NULL;
int goptimization_exp = -1;
int *gis_not_appliable;



WffNode *ggoal = NULL;


/* stores inertial - information: is any occurence of the predicate
 * added / deleted in the uninstantiated ops ?
 */
Bool gis_added[MAX_PREDICATES];
Bool gis_deleted[MAX_PREDICATES];



/* splitted initial state:
 * initial non static facts,
 * initial static facts, divided into predicates
 * (will be two dimensional arrays, allocated directly before need)
 */
Facts *ginitial = NULL;
int gnum_initial = 0;
Fact **ginitial_predicate;
int *gnum_initial_predicate;



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
int gnum_relevant_facts;
int gnum_pp_facts;



/* the final actions and problem representation
 */
Action *gactions;
int gnum_actions;
State ginitial_state;
State ggoal_state;
State saved_ginitial_state;
State saved_ggoal_state;

int cvar_hash_table[HASH_SIZE];
int tot = 0, compl = 0;

//char *lvar_names[MAX_VARS];
//int lvar_types[MAX_VARS];
int **l_vals = NULL; 
int **lstar_vals = NULL;
int **r_vals = NULL;
int **tested_vars = NULL;

/* for facts and mutex
*/
int *F;			/*[MAX_RELEVANT_FACTS/32+1]; */


const char *goperator_table[] = {
  "MUL_OP",
  "DIV_OP",
  "MINUS_OP",
  "UMINUS_OP",
  "PLUS_OP",

  "FIX_NUMBER",
  "VARIABLE_OP",

  "INCREASE_OP",
  "DECREASE_OP",
  "SCALE_UP_OP",
  "SCALE_DOWN_OP",
  "ASSIGN_OP",

  "LESS_THAN_OP",
  "LESS_THAN_OR_EQUAL_OP",
  "EQUAL_OP",
  "GREATER_THAN_OP",
  "GREATER_OR_EQUAL_OP",

  "MINIMIZE_OP",
  "MAXIMIZE_OP"
};




/**
 * CONNECTIVITY GRAPH
 **/


/* one ops (actions) array ... */
OpConn *gop_conn;
int gnum_op_conn;



/* one effects array ... */
EfConn *gef_conn;
int gnum_ef_conn;



/* one facts array. */
FtConn *gft_conn;
int gnum_ft_conn;

FtConn *gnoop_conn;

int gnum_ft_block;
int gnum_ef_block;




/**
 * FF SEARCHING NEEDS
 **/

/* the goal state, divided into subsets
 *  */
State *ggoal_agenda;
int gnum_goal_agenda;



/* byproduct of fixpoint: applicable actions */
int *gA;
int gnum_A;



/* communication from extract 1.P. to search engines:
 * 1P action choice */
int *gH;
int gnum_H;



/* the effects that are considered true in relaxed plan */
int *gin_plan_E;
int gnum_in_plan_E;


/* always stores (current) serial plan */
int gplan_ops[MAX_PLAN_LENGTH];
int gnum_plan_ops = 0;
int gtot_plan_ops = 0;


/* stores the states that the current plan goes through
 * ( for knowing where new agenda entry starts from ) */
State gplan_states[MAX_PLAN_LENGTH + 1];
State gbfs_state;

PlanAction *subplan_actions = NULL;



/**
 * LPG LOCAL SEARCH
 **/


int num_try;
int return_count;
unsigned int seed;


constraints_list treated_c_l[MAX_FALSE];
constraints_list unsup_fact[MAX_FALSE];
constraints_list unsup_num_fact[MAX_FALSE];

neighb_list neighb_vect[MAX_MAX_NODES];
int num_neighborhood;

/* final sort of actions in temp_vect */  
int *pos_temp_vect;//[MAX_MAX_NODES];

def_level * vectlevel[MAX_PLAN_LENGTH + 1];
def_level * temp_vectlevel[MAX_PLAN_LENGTH + 1];


inform_list *remove_act_chain; //[MAX_PLAN_LENGTH];
int ind_remove_act_chain;

/* Used for action <--> noop mutex 
 */
noop_not_in *noop_free_list; 

unsigned long tot_alloc_mem_size;

char fct_file[MAX_LENGTH];

/* Statistical data about Lagrange multipliers
*/
#ifdef __STATISTIC_LM__

 /* global variables used to compute average, total maximum value, minimum value of
   Lagrange multipliers for preconditions and mutex 
  */
 
 float average_prec_final = 0.0;
 float average_me_final = 0.0;
 float var_prec_final = 0.0;
 float var_me_final = 0.0;

 float lm_prec_min_final,lm_prec_max_final,lm_me_min_final,lm_me_max_final;
 
/*Vars used for files
 */

 FILE *file_average_prec;
 FILE *file_var_prec;
 FILE *file_average_me;
 FILE *file_var_me;

#endif // end __STATISTIC_LM__



/**
 * COMPUTE MUTEX
 **/


/* Number of set mutex and level
 */
int gnum_mutex;
int gmutex_level;
/* Total number of fact-action mutex, action-fact mutex, 
   action-action mutex, fact-fact mutex 
 */
int total_ft_ef_mutex = 0;
int total_ef_ft_mutex = 0;
int total_ef_ef_mutex = 0;
int total_ft_ft_mutex = 0;

/* fact-fact mutex matrix
 */
int **FT_FT_mutex = NULL;
/* fact-action mutex matrix
 */
int **FT_EF_mutex = NULL;
/* action-action mutex matrix
 */
int **EF_EF_mutex = NULL;
/* action-fact mutex matrix
 */
int **EF_FT_mutex = NULL;


/**
 * NUMERIC PLANNING
 **/

/* Structure for numeric vars
 */

CompositeNumVar *gcomp_var;
float  *gcomp_var_value;
float  *gcomp_var_value_before;


/**
 * TEMPORAL PLANNING
 **/

char **mat_ord;
inform_list *act_ord_vect;
int num_act_ord;
short *prop_level_index;


/**
 * CPU TIME MANAGEMENT
 **/

struct tms start_time;
struct tms glob_start_time;
struct tms glob_end_time;
float gtotal_time, gparsing_time;
char gcomm_line[MAX_LENGTH * 2];
char gops_file[MAX_LENGTH];
char gfct_file[MAX_LENGTH];
char glpg_path[MAX_LENGTH];


/**
 * MISCELLANEUS
 **/

/* used to time the different stages of the planner
 */
float gtempl_time = 0, greach_time = 0, grelev_time = 0, gconn_time = 0, 
  gnum_time = 0, gmutex_total_time = 0, gmutex_ft_time = 0, 
  gmutex_ops_time = 0, gmutex_num_time = 0;
float gsearch_time = 0;

float build_ad_time, fixpoint_time;

/* the command line inputs
 */
struct _command_line gcmd_line;

/* number of states that got heuristically evaluated
 */
int gevaluated_states = 0;

/* maximal depth of breadth first search
 */
int gmax_search_depth = 0;

char temp_name[256];

node_cost *fact_costs; //[MAX_MAX_NODES];
/* Bitvector used by remove_temp_action to find facts that 
   become TRUE after it is removed
*/
int *new_true_facts;
/* Bitvector used by remove_temp_action to find facts that 
   become FALSE after it is removed
*/
int *new_false_facts;	

/* TRUE if termination condition is reached
 */
Bool is_terminated=FALSE;

Bool ComputeMutex;

char gops_file[MAX_LENGTH];
char gfct_file[MAX_LENGTH];


/********************************************************************
 *                           HEADERS FOR PARSING                    *
 ********************************************************************/

void load_ops_file (char *filename);
void load_fct_file (char *filename);

/*****************************************************************
 *                          MAIN ROUTINE                         *
 *****************************************************************/
        
// ugly way to tell if ADL
// no time to elaborate


#define MAX_LINE_CHARS    1024
int get_requirements(char *ops_file)
{                   
	FILE *inFile;
	char lineStringBuffer[MAX_LINE_CHARS];
	int i, x = 0;
  
	GpG.gis_ADL = FALSE;
	GpG.is_deripred = FALSE;
	GpG.is_til = FALSE;
	GpG.is_durative = FALSE;
	GpG.is_fluents = FALSE;
	if ((inFile = fopen(ops_file, "r")) == NULL) 
	{
		printf("Can not open %s\n", ops_file);
		exit(1);
	}
	while (!feof(inFile) && fgets(lineStringBuffer, MAX_LINE_CHARS, inFile))
	{
		for (i=0;lineStringBuffer[i];i++)
			lineStringBuffer[i] = (char) toupper((int) lineStringBuffer[i]);
		if (strstr(lineStringBuffer, ":ADL"))
			GpG.gis_ADL = TRUE;
		if (strstr(lineStringBuffer, ":DERIVED-PREDICATES"))
			GpG.is_deripred = TRUE;
		if (strstr(lineStringBuffer, ":FLUENTS"))
			GpG.is_fluents = TRUE;
		if (strstr(lineStringBuffer, ":TIMED-INITIAL-LITERALS"))
			GpG.is_til = TRUE;
		if (strstr(lineStringBuffer, ":DURATIVE-ACTIONS"))
			GpG.is_durative = TRUE;
		if (strstr(lineStringBuffer, ":ACTION-COSTS"))
                  GpG.is_action_costs = TRUE;
		if (strstr(lineStringBuffer, ":GOAL-UTILITIES"))
                  GpG.is_goal_utilities = TRUE;
		if (strstr(lineStringBuffer, " PSR"))
			x = 1;	
	}
	fclose(inFile);
	return x;
}  
Bool ADL_ops_file (char *ops_file)
{                   
    FILE *inFile;
    char lineStringBuffer[MAX_LINE_CHARS];
    char targetString[MAX_LINE_CHARS];
    char* pStr;
        
    if ((inFile = fopen(ops_file, "r")) == NULL) {
         printf("Can not open %s\n", ops_file);
         exit(1);
    }
    while (!feof(inFile) && fgets(lineStringBuffer, MAX_LINE_CHARS, inFile)) {
        sscanf(lineStringBuffer, "%s", targetString);
        if(strcmp(targetString, "(:requirements")==0) {
            pStr = &(lineStringBuffer[16]);
            sscanf(pStr, "%s", targetString);
            //printf("%s\n",targetString);  
            if(strcmp(targetString, ":adl")==0) {
               //     printf("ADL domain!\n"); 
                    return TRUE;
            } else {
                    return FALSE;
            }
        }
    }
    return FALSE;
}  
int main (int argc, char *argv[])
{

  /* resulting name for ops file */
  char ops_file[MAX_LENGTH] = "";
  /* same for fct file */
  char fct_file[MAX_LENGTH] = "";

  char sol_file[MAX_LENGTH] = "";

  struct tms start, end;

  struct timeval tv;
  struct timezone tz;

  State current_start, current_end;
  int i, j, k;
  Bool found_plan=0;


#ifdef __EFENCE__
  extern int EF_ALLOW_MALLOC_0;
  EF_ALLOW_MALLOC_0 = 1;
#endif

printf("#\n");
printf("# (C) Copyright 2008, University of Illinois, Urbana-Champaign\n");
printf("#\n");
printf("# All rights reserved. Use of this software is permitted ONLY for\n");
printf("# non-commercial research purposes, and it may be copied only\n");
printf("# for that use only. All copies must include this copyright message.\n");
printf("# This software is made available AS IS, and neither the authors\n");
printf("# nor the University of Illinois, make any warranty about the\n");
printf("# software or its performance.\n");
printf("#\n");

  dis_gcmd_line.display_info = 1;
  dis_gcmd_line.debug = 0;
  dis_gcmd_line.ehc = dis_TRUE;
  dis_gcmd_line.optimize = dis_FALSE;
  dis_gcmd_line.g_weight = 1;
  dis_gcmd_line.h_weight = 1;
  SymmLagrangian = 0;     
  SymmLagrange = 0;     
  GpG.subsolver = 0;
  //dis_processdis__command_line(argc, argv); 
  /*
  printf("dis_gcmd_line.g_weight = %d\n", dis_gcmd_line.g_weight );
  printf("dis_gcmd_line.h_weight = %d\n", dis_gcmd_line.h_weight );
  printf("dis_gcmd_line.ehc = %d\n", dis_gcmd_line.ehc );
  printf("dis_gcmd_line.optimize = %d\n", dis_gcmd_line.optimize );
  printf("dis_gcmd_line.display_info = %d\n", dis_gcmd_line.display_info );
  printf("dis_gcmd_line.debug = %d\n", dis_gcmd_line.debug );
    */        
  
  //-------
  
  so_signal_management();

  strcpy (gcomm_line, "");
  for (i = 0; i < argc; i++)
    {
      strcat (gcomm_line, argv[i]);
      strcat (gcomm_line, " ");
    }
  get_path (*argv, glpg_path);
  initialize_preset_values ();


#ifdef __STATISTIC_LM__
  init_statistic();
#endif 

  /*Reset  hash-table
   */ 
  reset_cvar_hash_table();

  /* Initialize random seed
   */
  gettimeofday (&tv, &tz);
  seed = ((tv.tv_sec & 0177) * 1000000) + tv.tv_usec;


  /* command line treatment
   */
  if (argc == 1 || (argc == 2 && *++argv[0] == '?'))
    {
      lpg_usage ();
      exit (1);
    }
    gcmd_line.out_file_name[0] = 0;
  if (!process_command_line (argc, argv))
    {
      lpg_usage ();
      exit (1);
    }

  /* make file names
   */

  /* one input name missing
   */
  if (!gcmd_line.ops_file_name || !gcmd_line.fct_file_name)
    {
      fprintf (stdout, "\n%s: two input files needed\n\n", NAMEPRG);
      lpg_usage ();
      exit (1);
    }
  /* add path info, complete file names will be stored in
   * ops_file and fct_file 
   */
  sprintf (ops_file, "%s%s", gcmd_line.path, gcmd_line.ops_file_name);
  sprintf (fct_file, "%s%s", gcmd_line.path, gcmd_line.fct_file_name);

  strcpy (gops_file, ops_file);
  strcpy (gfct_file, fct_file);
  sprintf (sol_file, "%s%s", gcmd_line.path, gcmd_line.sol_file_name);


  /* parse the input files
   */

  /* start parse & instantiation timing
   */
  times (&glob_start_time);
  times (&start);

  /*
  //GpG.feed_MFF_LPG = TRUE;
  GpG.feed_MFF_LPG = FALSE;
  if(GpG.feed_MFF_LPG) {
        dis_MFF_main(ops_file, fct_file);
        mff_to_lpg();    
  }
  else 
  // START OF PARSING
  {*/

  /* domain file (ops)
   */

  // ADL?
  GpG.gis_ADL = ADL_ops_file(ops_file);
  i = get_requirements(ops_file);
  if((!GpG.is_deripred || i == 1 || !GpG.gis_ADL) && !GpG.is_goal_utilities && !GpG.is_action_costs) {
  if(!i || !GpG.gis_ADL) {   
   printf ("\nParsing domain file\n");
	fflush(stdout);

    /* it is important for the pddl language to define the domain before 
    * reading the problem 
    */
    load_ops_file (ops_file);
    // Y. Chen
    if(gloaded_dps) {
       // printf("\nSGPlan: Contains derived predicates\n");
      GpG.is_deripred = TRUE;
    }
    else {
      //  printf("\nSGPlan: No derived predicates\n");
      GpG.is_deripred = FALSE;
    }                 
  }
  else
  {
    gdomain_name = dis_copy_dis_Token("PSR");
    GpG.is_deripred = TRUE;
  }
  }

  /* =============  Search Modal =======================
   * Y. Chen 
   * Decide which parser to use here 
   */
                                
  search_ops_modal(); 
  //fprintf(stderr, "\nSearchModal = %d %d\n", GpG.SearchModal, GpG.SecondaryModal);
                                           
  // MFF_parser
  if((GpG.SearchModal == 5) || (GpG.SearchModal == 7) 
     || (GpG.SearchModal == 6) || (GpG.SearchModal == 100)
     || (GpG.SearchModal == 104) || (GpG.SearchModal == 106)
     || (GpG.SearchModal == 107)
     || (GpG.SearchModal == -1) || (GpG.SearchModal <= -1000)) {
	/* 
	Settlers 5
	Sattelite 6 Sattelite TIME_TIMEWINDOWS_COMPILED 106
	UMTS 7
	Psr large 100 Psr middle-compiled 104
	Promela OPTICAL_TELEGRAPH_FLUENTS PHILOSOPHERS_FLUENTS 107
	*/
        GpG.MFF_parser = TRUE;        
  } else {
        GpG.MFF_parser = FALSE;
  }
 
  // ComputeMutex       
  if(((GpG.SearchModal == 0)||(GpG.SearchModal == 105))
  || (GpG.SearchModal == 3 && GpG.is_til )
         ) {
	/*
	Airport 0 Airport TEMPORAL_TIMEWINDOWS_COMPILED 105
	Pipesworld NOTANKAGE_TEMPORAL_DEADLINES 3
	*/
	GpG.lowmemory = FALSE;
        ComputeMutex = TRUE;        
  } else {
        ComputeMutex = FALSE;
  }
  if(GpG.is_deripred) {
      ComputeMutex = FALSE;
  }
    
    
  /* ==================================================*/
 
  if(GpG.MFF_parser) {
    mffDistributedSearch(ops_file, fct_file);  
    exit(0);
  }
  
  /* ===================================================
    */
  
  transfer_PlDP_PlOperator();
        
  /*dirty trick to get another copy of gloaded_ops */
  	if (GpG.is_til)
		load_fct_file (fct_file);  
  gloaded_pl2ops = gloaded_ops;
  gloaded_ops = NULL;
  /* derived predicates */
  gloaded_dps = NULL;
  /* timed initial literals */
  gnum_tils = 0;
  g_tils = gtils = NULL;
  gdomain_name = NULL;
  gorig_initial_facts = NULL;
  gorig_goal_facts = NULL;
  gmetric_exp = NULL;
  gloaded_axioms = NULL;
  gparse_types = NULL;
  gparse_constants = NULL;
  gparse_predicates = NULL;
  gparse_functions = NULL;
  gparse_objects = NULL;
  gorig_constant_list = NULL;
  gpredicates_and_types = NULL;
  gfunctions_and_types = NULL;
  gloaded_constraints = NULL;
  gloaded_preferences = NULL;
  load_ops_file (ops_file);
  
  // Y. Chen
  transfer_PlDP_PlOperator();

  /*add dummy effect to operators without boolean effects */
  add_dummy_effects (gloaded_ops);
  add_dummy_effects (gloaded_pl2ops);
  /*counts numeric preconds and effects */
  count_num_preconds_and_effects ();
  GpG.gplan_actions = NULL;


  /* problem file (facts)
   */
  if (gcmd_line.display_info >= 1)
    {
      printf ("\nParsing problem file\n");
	fflush(stdout);
    }

  load_fct_file (fct_file);
  if (gcmd_line.display_info >= 1)
    printf ("\n\n");

  allocate_after_parser();

  /* now we have PlOperators and PlNodes */
  reduce_pddl2_to_pddl1 ();

  /* This is needed to get all types.
   */
  build_orig_constant_list ();

  /* last step of parsing: see if it's an ADL domain!
   */

  if (!make_adl_domain ())
    {
      printf ("\n%s: this is  an ADL problem!", NAMEPRG);
      printf ("\n    can't be handled by this version.\n\n");
      exit (1);
    }


  /* now instantiate operators;
   */


  /**************************
   * first do PREPROCESSING * 
   **************************/


  /* start by collecting all strings and thereby encoding 
   * the domain in integers.
   */
  encode_domain_in_integers ();

  /* inertia preprocessing, first step:
   *   - collect inertia information
   *   - split initial state into
   *        _ arrays for individual predicates
   *        - arrays for all static relations
   *        - array containing non - static relations
   */
  do_inertia_preprocessing_step_1 ();

  /* normalize all PL1 formulae in domain description:
   * (goal, preconds and effect conditions)
   *   - simplify formula
   *   - expand quantifiers
   *   - NOTs down
   */
  normalize_all_wffs ();

  /* translate negative preconds: introduce symmetric new predicate
   * NOT-p(..) (e.g., not-in(?ob) in briefcaseworld)
   */
  translate_negative_preconds ();

  /* split domain in easy (disjunction of conjunctive preconds)
   * and hard (non DNF preconds) part, to apply 
   * different instantiation algorithms
   */
  split_domain ();


  /***********************************************
   * PREPROCESSING FINISHED                      *
   *                                             *
   * NOW MULTIPLY PARAMETERS IN EFFECTIVE MANNER *
   ***********************************************/

  build_easy_action_templates ();

  build_hard_action_templates ();

  times (&end);
  TIME (gtempl_time);
  times (&start);


  check_time_and_length (0);

  // Y.Chen
  seed = 2004;
  srandom(seed);

#ifdef __MY_OUTPUT__
  printf ("\n Seed %d  \n", seed);
#endif


  /* perform reachability analysis in terms of relaxed 
   * fixpoint
   */

  perform_reachability_analysis ();

  times (&end);
  TIME (greach_time);
  times (&start);

  check_time_and_length (0);


  /* collect the relevant facts and build final domain
   * and problem representations.
   */

  collect_relevant_facts ();

  times (&end);
  TIME (grelev_time);
  times (&start);

  check_time_and_length (0);


  /* now build globally accessable connectivity graph
   */
  build_connectivity_graph ();
 
 /* 
  }// END PARSING
 */

  // Y. Chen
  set_DPop_flag();
 
  if(GpG.SearchModal != 3 && GpG.SearchModal != -2) { 
    if(!ComputeMutex) {       
        DistributeSearch(&ginitial_state, &ggoal_state, &subplan_actions);
        exit(0);
    }
  }
  
  times (&end);
  TIME (gconn_time);
  times (&start);

  check_time_and_length (0);

  /* association to gef_conn[i] a corresponding complet ploperator */
  associate_PlOperator_with_EfConn ();


  /* adding composed numeric quantities */
  add_composite_vars ();

  make_numgoal_state(GpG.numeric_goal_PlNode);

  /* make false the comparison between uninitialized numeric quantities */
  make_false_all_checks_on_not_init ();

  /* Semplification for inertial vars
   */
  propagate_inertias ();

if (GpG.SearchModal == -2)
{
	mffDistributedSearch(ops_file, fct_file);
	exit(0);
}
  if(GpG.SearchModal == 3) { 
    if(!ComputeMutex) {       
        DistributeSearch(&ginitial_state, &ggoal_state, &subplan_actions);
        exit(0);
    }
  }

  if (DEBUG0)
    if (GpG.non_strips_domain)
      {
          /*
	    if (GpG.variable_duration)
	        printf ("\n\nAction durations have been computed\n");
	    else
	     printf ("\n\nThere is no action duration to compute\n");*/
      }

  /* Set vars orig_weight_cost and orig_weight_time according with plan evaluation metric
   */
  if (goptimization_exp != -1)
    set_cost_and_time_coeffs ();

  /*
  if (DEBUG0)
    printf("\nEvaluation function weights:\n     Action duration %.2f; Action cost %.2f", GpG.orig_weight_time, GpG.orig_weight_cost);

  if (DEBUG0)
    printf ("\n\nTemporal flag: %s\n", GpG.temporal_plan ? "ON" : "OFF");
 */
    
  /* Make numeric effects structure
   */
  create_descnumeff_of_efconns ();

  /* Sets flag is_numeric for each action (efconn)
   */
  set_numeric_flag ();

  assert (gnum_comp_var < MAX_NUM_VALUE);

  /* Copy initial state in  initial_state
   */
  for (i = 0; i < gnum_comp_var; i++)
    ginitial_state.V[i] = GCOMP_VAR_VALUE(i);


  times (&end);
  TIME (gnum_time);
  times (&start);

  /* Print information about action istantiation 
   */
  print_parser_info_for_debug();
  //print_real_state(ginitial_state); 

  
    if(ComputeMutex) {
        
  //if (GpG.numrun > 0 && GpG.numtry > 0) {
  if (1) {

    if (DEBUG0 && !DEBUG1) {
   //   printf ("\nComputing mutex... ");
   //   fflush (stdout);
    }
    if (DEBUG1)
      printf ("\n\n--- COMPUTE MUTEX BETWEEN FACTS ---\n");
    
    if (GpG.accurate_cost >= 1)
      allocate_reachability_information_data();
	
    /* Comute mutex between facts    
    */
    calc_mutex (&ginitial_state);
    
    if (!are_goal_reachable_and_non_mutex ()) {
      printf ("\nThe problem is unsolvable since at the fixpoint level the goals are mutex or not reachable\n\n");
      exit (0);
    }
  }
  

  times (&end);
  TIME (gmutex_ft_time);

  if (DEBUG2)
    printf ("\n");
  if (DEBUG1)
    printf ("\n   --> Compute mutex between facts TOTAL TIME: %12.2f",gmutex_ft_time);

  times (&start);
  //if (GpG.numrun > 0 && GpG.numtry > 0) {
  if(1){  
   if (DEBUG1)
      printf ("\n\n--- COMPUTE MUTEX BETWEEN ACTIONS ---\n");
    /*Compute action-action, action_fact, fact-action mutex
     */
    calc_mutex_ops ();
  }


  times (&end);
  TIME (gmutex_ops_time);

  if (DEBUG1)
    printf ("\n   --> Compute mutex between actions TOTAL TIME: %12.2f\n",gmutex_ops_time);


  
  times (&start);

  
  //if (GpG.numrun > 0 && GpG.numtry > 0) {
  if(1){
      if (DEBUG1)
      printf ("\n\n--- COMPUTE MUTEX BETWEEN NUMERIC FACTS ---\n");
    /* Compute mutex between action with numeric effects
     */  
    if (!GpG.lowmemory)
      calc_mutex_num_efs ();
  }
  
  times (&end);
  TIME (gmutex_num_time);

  if (DEBUG1)
    printf("\n   --> Compute mutex between numeric facts TOTAL TIME: %12.2f\n",gmutex_num_time);
  if (DEBUG2)
    print_mutex_result ();
  if (DEBUG0 && !DEBUG1) {
   // printf ("done");
   // fflush (stdout);
  }

  times (&start);

  //if (DEBUG6 && !GpG.lowmemory)
    print_matrs ();
  gmutex_total_time = gmutex_ft_time + gmutex_ops_time + gmutex_num_time;
 }

  if (strlen (gcmd_line.sol_file_name) > 0)
    load_pddl2_plan (sol_file, &GpG.gplan_actions, 0);
	if (GpG.SearchModal == 3)
		ComputeMutex = FALSE;


  GpG.max_num_actions = gnum_ef_conn;
  GpG.max_num_facts = gnum_ft_conn;
  GpG.max_num_ft_block = gnum_ft_block;



  /***********************************************************
   * we are finally through with preprocessing and can worry *
   * about finding a plan instead.                            *
   ***********************************************************/


  /* another quick preprocess: approximate goal orderings and split
   * goal set into sequence of smaller sets, the goal agenda
   */
   
	// Yixin
 if(ComputeMutex) { 
    modify_ft_ef_mutex(); 
    compute_goal_agenda();
 }
  // printf ("******************\n");

  /*
   source_to_dest( &(gplan_states[0]), &ginitial_state );
   source_to_dest( &current_start, &ginitial_state );
   source_to_dest( &current_end, &(ggoal_agenda[0]) );
          
   for ( i = 0; i < gnum_goal_agenda; i++ ) {
	if ( !do_enforced_hill_climbing( &current_start, &current_end ) ) {
	       break;
       }
       source_to_dest( &current_start, &(gplan_states[gnum_plan_ops]) );
      if ( i < gnum_goal_agenda - 1 ) {
         for ( j = 0; j < ggoal_agenda[i+1].num_F; j++ ) {
	       current_end.F[current_end.num_F++] = ggoal_agenda[i+1].F[j];
         }
      }
    }
    found_plan = ( i == gnum_goal_agenda ) ? TRUE : FALSE;
  */
    

  source_to_dest (&(gplan_states[0]), &ginitial_state);
  source_to_dest (&current_start, &ginitial_state);
  source_to_dest (&current_end, &ggoal_state);

  remove_unappliable_actions ();

  if ((GpG.search_type == LOCAL && GpG.numrun > 0 && GpG.numtry > 0) ||
      GpG.search_type == DIS_SEARCH )
     {
      k = MAX (GpG.input_plan_lenght, gmutex_level);
      for (i = 0; i < k; i++)
	{
	  if (i < gmutex_level)
	    create_vectlevel (0);
	  else
	    create_vectlevel (1);
	}
      allocate_data_for_local_search();
      create_all_min_array ();

      GpG.fixpoint_plan_length = GpG.max_plan_length - 1;
      GpG.saved_fixpoint_plan_length = GpG.fixpoint_plan_length ;
      GpG.curr_goal_state =  &current_end;

    }


  if (DEBUG1) {
    printf ("\n\nTime spent for preprocessing:");
    printf ("\n Instantiating:     %7.2f seconds", gtempl_time + greach_time + grelev_time + gconn_time + gsearch_time);
    printf ("\n Mutex relations:   %7.2f seconds", gmutex_total_time);
    printf ("\n Numeric relations: %7.2f seconds", gnum_time);
  }
  if (DEBUG0) {   
    times (&glob_end_time);
    gtotal_time = (float) ((glob_end_time.tms_utime - glob_start_time.tms_utime + glob_end_time.tms_stime - glob_start_time.tms_stime) / 100.0);
  //  printf ("\nPreprocessing total time: %.2f seconds",gtotal_time);
  }
  

  /*
  printf ("\n\ninitial state is:\n\n");
  for (i = 0; i < ginitial_state.num_F; i++)
    {
      print_ft_name (current_start.F[i]);
      printf ("\n");
    }
  printf ("\n\ngoal state is:\n\n");
  for (i = 0; i < current_end.num_F; i++)
    {
      print_ft_name (current_end.F[i]);
      printf ("\n");
    }
  printf("GpG.fixpoint_plan_length = ",
          GpG.fixpoint_plan_length);
*/
#ifdef __TEST__
  for (i = 0; i < gnum_op_conn; i++)
  {
    print_op_name(i);
    printf(" -- %f \n", get_action_cost (i));
  }
#endif


  if (GpG.do_best_first == TRUE && GpG.numrun==0)  
    GpG.search_type=BEST_FIRST;
  /* Search untill it is not reached termination condition (given by the function 'is_term_condition_reached')
   */
  while(!is_terminated)
    {
      /* Different types of local search 
       */      
      switch(GpG.search_type)
	{
	  /* Local Search usually used in LPG
	   */

     case DIS_SEARCH:
         DistributeSearch(&current_start, &current_end, &subplan_actions);
         GpG.gplan_actions = subplan_actions;
         subplan_actions = NULL;
         is_terminated=TRUE;
         break;       
         
     case LOCAL:
	  /* Do Local Search
	   */
	   
        LocalSearch (&current_start, &current_end, &subplan_actions);
	  /* Store plan in GpG.gplan_actions
	   */
	  GpG.gplan_actions = subplan_actions;	 
	  subplan_actions = NULL;
	  /* Control if the termination condition is reached
	   */
	  is_terminated=TRUE;
      //is_terminated= is_term_condition_reached();
	  break;
	 
	  /* Best First Search implemented by J. Hoffmann (FF-v2.3)
	   */
	case BEST_FIRST:

      //  strips_gef_conn();
       // load_ff_gef_conn();
	  
        if (DEBUG0)
	    printf("\n\nSwitching to Best-first Search ( code from J. Hoffmann's package FF-v2.3 ) \n");
	  check_time_and_length (0);	/* con zero non controlla la lunghezza */
	  /* Return solution if reached, FALSE otherwise
	   */
	  found_plan = do_best_first_search ();
	  printf("do_best_first_search");
      
	  //if (do_enforced_hill_climbing (&current_start, &current_end))
	  //printf("do_hill");
	  
      times (&end);
	  TIME (gsearch_time);
	  
	  times (&end);
	  times (&glob_end_time);
	  gtotal_time = (float) ((glob_end_time.tms_utime - glob_start_time.tms_utime + glob_end_time.tms_stime - glob_start_time.tms_stime) / 100.0);
	  /* If a solution was found in best first search print solution
	   */
	  if (found_plan)
	    {
	      
#ifdef __MY_OUTPUT__
	      printf ("\nFFGGHH::%.2f::%d\n", gtotal_time, gnum_plan_ops);
#endif
	      store_adapted_temporal_plan_ff (gcmd_line.fct_file_name);
	      
	      
	      printf ("\nTotal time:      %.2f\nSearch time:     %.2f\nActions:         %d\nExecution cost:  %.2f\nDuration:        %.3f\nPlan quality:    %.3f", gtotal_time, gsearch_time, GpG.num_actions, GpG.total_cost, GpG.total_time,GpG.total_cost * GpG.orig_weight_cost + GpG.total_time * GpG.orig_weight_time);

	      printf ("\n     Plan file:");
	      printf ("       plan_bestfirst_%s.SOL", gcmd_line.fct_file_name);
	  
	    }
	  
	  if (DEBUG1)
	    output_planner_info ();
	  /* Control if the termination condition is reached
	   */
	  is_terminated= is_term_condition_reached();
	  break;
	  
	  /* Hill Climbing Search
	   */
	case   HILL_CLIMBING:
	  
	  if (do_enforced_hill_climbing (&current_start, &current_end))
	    source_to_dest (&current_start, &(gplan_states[gnum_plan_ops]));
	  
      printf("do_hill");
	  /* Control if the termination condition is reached
	   */
	  is_terminated= is_term_condition_reached();
	  break;
	  
	default:
	  /* Control if the termination condition is reached
	   */
	  is_terminated= is_term_condition_reached();
	  break;

	}  

	  
      if (DEBUG2)
	{
	  printf ("\n\nInitial state is:\n\n");
	  for (j = 0; j < ginitial_state.num_F; j++)
	    {
	      print_ft_name (current_start.F[j]);
	      printf ("\n");
	    }
	  printf ("\n\nGoal state is:\n\n");
	  for (j = 0; j < current_end.num_F; j++)
	    {
	      print_ft_name (current_end.F[j]);
	      printf ("\n");
	    }
	}
    }
    
  printf ("\n\n");

	      printf ("\nTotal time:      %.3f\n", gtotal_time);
  exit (0);
}
