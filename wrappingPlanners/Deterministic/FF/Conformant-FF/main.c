

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
 *              Conformant version in collaboration with Ronen Brafman.
 *
 * Author: Joerg Hoffmann 2002
 * 
 *********************************************************************/ 







#include "ff.h"

#include "memory.h"
#include "output.h"

#include "parse.h"

#include "inst_pre.h"
#include "inst_easy.h"
#include "inst_hard.h"
#include "inst_final.h"

#include "relax.h"
#include "state_transitions.h"
#include "repeated_states.h"
#include "search.h"











/*
 *  ----------------------------- GLOBAL VARIABLES ----------------------------
 */












/*******************
 * GENERAL HELPERS *
 *******************/







struct tms start, end;

/* runtime statistics etc.
 */
float gtempl_time = 0, greach_time = 0, grelev_time = 0, gconn_time = 0, gmem_time = 0;
float gsearch_time = 0, geval_time = 0, gcnf_time = 0, genc_time = 0, gsat_time = 0;
float grs_time = 0, grs_sat_time = 0, gss_time = 0, gsc_time = 0;
float gr_sat_time = 0, grp_sat_time = 0, gr_cnf_time = 0, gr_enc_time = 0, gmembership_time = 0;
int gsat_calls = 0, gcnfs = 0, grs_sat_calls = 0, gss_sat_calls = 0, gsc_sat_calls = 0;
int gr_sat_calls = 0, grp_sat_calls = 0;
int grs_comps = 0, grs_conf_comps = 0;
int grs_hits = 0, gss_hits = 0, gdp_calls = 0, gup_calls = 0;

/* the command line inputs
 */
struct _command_line gcmd_line;

/* number of states that got heuristically evaluated
 */
int gevaluated_states = 0;

/* maximal depth of breadth first search
 */
int gmax_search_depth = 0;



/* CNF statistic
 */
float *gsum_k_clauses, gsum_clauses = 0;








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

/* stores initials as fact_list 
 */
PlNode *gorig_initial_facts = NULL;

/* stores initial ors as an array of OR lists
 */
PlNode_pointer *gorig_initial_ors;
int gnum_orig_initial_ors;

/* stores initial oneofs as an array of ONEOF lists
 */
PlNode_pointer *gorig_initial_oneofs;
int gnum_orig_initial_oneofs;

/* not yet preprocessed goal facts
 */
PlNode *gorig_goal_facts = NULL;

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

/* the objects, declared in the problem file
 */
TypedList *gparse_objects = NULL;


/* connection to instantiation ( except ops, goal, initial )
 */

/* all typed objects 
 */
FactList *gorig_constant_list = NULL;

/* the predicates and their types
 */
FactList *gpredicates_and_types = NULL;












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





/* the domain in integer (Fact) representation
 */
Operator_pointer goperators[MAX_OPERATORS];
int gnum_operators = 0;
Fact *gfull_initial;
int gnum_full_initial = 0;
Fact *gfull_unknown_initial;
int gnum_full_unknown_initial;
WffNode_pointer *gfull_or_initial;
int gnum_full_or_initial;
WffNode_pointer *gfull_oneof_initial;
int gnum_full_oneof_initial;
WffNode *ggoal = NULL;




/* stores inertia - information: is any occurence of the predicate
 * added / deleted in the uninstantiated ops?
 * is any occurence of the predicate unknown?
 */
Bool gis_added[MAX_PREDICATES];
Bool gis_deleted[MAX_PREDICATES];
Bool gis_unknown[MAX_PREDICATES];



/* splitted initial state:
 * initial non static facts,
 * initial static facts, divided into predicates
 * (will be two dimensional array, allocated directly before need)
 *
 * the same mirrored for unknown facts -- "known negatives" is transferred
 * here to "known positives and unknowns"; seems more adequate for later 
 * purposes, giving access to unknowns directly.
 */
Facts *ginitial = NULL;
int gnum_initial = 0;
Facts *gunknown_initial = NULL;
int gnum_unknown_initial = 0;
Fact **ginitial_predicate;
int *gnum_initial_predicate;
Fact **gunknown_initial_predicate;
int *gnum_unknown_initial_predicate;
/* this here stores dependencies between initial variables:
 * when translating negations of an unkwown literal we need
 * to remember that the translation, while unkown, will
 * always have the respective inverse value.
 */
Facts *ginitial_ft_equivalence_A;
Facts *ginitial_ft_equivalence_notA;
int gnum_initial_ft_equivalence = 0;



/* the type numbers corresponding to any unary inertia
 */
int gtype_to_predicate[MAX_PREDICATES];
int gpredicate_to_type[MAX_TYPES];

/* (ordered) numbers of types that new type is intersection of
 */
TypeArray gintersected_types[MAX_TYPES];
int gnum_intersected_types[MAX_TYPES];



/* stores which predicate is a translation of which other one.
 */
int gtranslated_predicate_to[MAX_PREDICATES];



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



/* the final actions and problem representation
 */
Action *gactions;
int gnum_actions;
State ginitial_state;
State ggoal_state;
/* initially valid implications
 */
int *ginitial_equivalence_A;
int *ginitial_equivalence_notA;
int gnum_initial_equivalence;
/* to know how much space we need for unknown conds in states
 */
int gmax_E;
/* the initial OR constraints in final coding
 */
int **ginitial_or;
int *ginitial_or_length;
int gnum_initial_or;








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



/* max #conds. for max clauses computation.
 */
int gmax_C;



/* max U: all initial Us plus facts that are 
 * added / deleted by a conditional effect with poss_U conds.
 * (important for various memory allocations)
 */
int gmax_U;
int gmax_CNFU;

/* we get these max #s of clauses and lits.
 */
int gmax_clauses;
int gmax_rs_clauses;
int gmax_literals;









/*******************
 * SEARCHING NEEDS *
 *******************/







/* byproduct of fixpoint: applicable actions
 */
int *gA;
int gnum_A = 0;



/* communication from extract 1.P. to search engines:
 * 1P action choice
 */
int *gH;
int gnum_H = 0;



/* always stores (current) serial plan
 */
int gplan_ops[MAX_PLAN_LENGTH];
int gnum_plan_ops = 0;



/* stores the states that the current plan goes through
 */
State gplan_states[MAX_PLAN_LENGTH + 1];



/* the clauses to be communicated to the SAT solver for
 * determining inferred literals.
 */
TimedLiteral **gclauses;
int *gclause_length;
int gnum_fixed_clauses;/* up to end of gplan */
int gnum_clauses;/* beyond that, ie dynamic search fraction */ 
int gfixed_endtime = 0;

/* array; maps ft / time pair to its number in CNF encoding.
 */
int **gcodes;
int gnum_fixed_c;


/* inverse mapping, to undo changes in table.
 */
int *gcf, *gct, gnum_c;



/* statistics: count nr. of times the "disjunction minimisation" actually
 * minimised something
 */
int gremoved_lits = 0;



/* stores the current DP decisions including unit propagations.
 *
 * is for DP given in state_transitions.c!!!!!
 *
 * have to make this global as it's also accessed from repeated states --
 * when checking stagnation. I know it's ugly...
 */
int *gdecision_stack;
int gnum_decision_stack;



/* for each possible ft code, a pointer to connected dynamic list
 * of member elements, ie the clauses in which it participates,
 * positive and negative.
 *
 * used in state_transitions DP solver. global as accessed from search.c
 * in reset between ehc and bfs switch.
 */
MemberList_pointer *gpos_c_in_clause_start;
MemberList_pointer *gpos_c_in_clause_fixed;/* before here, list corresp. to fixed CNF */
MemberList_pointer *gpos_c_in_clause_end;/* before here, members of current list */
MemberList_pointer *gneg_c_in_clause_start;
MemberList_pointer *gneg_c_in_clause_fixed;
MemberList_pointer *gneg_c_in_clause_end;



/* automatically checked Bool saying if sufficient criteria for
 * "more facts are always better" holds.
 */
Bool gdomination_valid;












/*
 *  ----------------------------- HEADERS FOR PARSING ----------------------------
 * ( fns defined in the scan-* files )
 */







void get_fct_file_name( char *filename );
void load_ops_file( char *filename );
void load_fct_file( char *filename );











/*
 *  ----------------------------- MAIN ROUTINE ----------------------------
 */



char out_filename[MAX_LENGTH] = "";



struct tms lstart, lend;







int main( int argc, char *argv[] )
{
	return run(argc,argv);
	}

int run( int argc, char *argv[] )
{

  /* resulting name for ops file
   */
  char ops_file[MAX_LENGTH] = "";
  /* same for fct file 
   */
  char fct_file[MAX_LENGTH] = "";
  


  int i;
  Bool found_plan;

  times ( &lstart );
  FILE *stream;
	stream = freopen("temp_out.txt","w",stdout);
	if( stream == NULL )  
     fprintf( stdout, "error on freopen\n" );
	


  gcmd_line.display_info = 1;
  gcmd_line.debug = 0;
  gcmd_line.ehc = TRUE;
  gcmd_line.help = TRUE;
  gcmd_line.manual = FALSE;
  gcmd_line.heuristic = 1;
  gcmd_line.stagnating = TRUE;
  gcmd_line.dominating = TRUE;
  gcmd_line.breadth_bfs = FALSE;
  gcmd_line.R = FALSE;
  gcmd_line.A = FALSE;
  gcmd_line.T = FALSE;
  gcmd_line.P = FALSE;
  memset(gcmd_line.ops_file_name, 0, MAX_LENGTH);
  memset(gcmd_line.fct_file_name, 0, MAX_LENGTH);
  memset(gcmd_line.path, 0, MAX_LENGTH);

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


  /* make file names
   */

  /* one input name missing
   */
  if ( !gcmd_line.ops_file_name || 
       !gcmd_line.fct_file_name ) {
    fprintf(stdout, "\nff: two input files needed\n\n");
    ff_usage();      
    exit( 1 );
  }
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
   *        _ arrays for individual predicates
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
  collect_relevant_facts();

  times( &end );
  TIME( grelev_time );

  times( &start );

  /* now build globally accessable connectivity graph
   */
  build_connectivity_graph();

  times( &end );
  TIME( gconn_time );

  /***********************************************************
   * we are finally through with preprocessing and can worry *
   * bout finding a plan instead.                            *
   ***********************************************************/

  times( &start );

  /* 2 * #initial equivalences plus #initial OR clauses plus
   * max ops to induce * (max ef implic of op * max noop implic) plus
   * max #additional clauses for conflict check plus
   * max #additional clauses for infer clauses
   */
  gmax_clauses = 
    (2 * gnum_initial_equivalence) +
    (MAX_PLAN_LENGTH * (gmax_E + (2 * gmax_CNFU))) +
    gmax_C * 2 +
    1;
  for ( i = 0; i < gnum_initial_or; i++ ) {
    /* all ordered pairs of fts yield one binary clause.
     */
    gmax_clauses += ginitial_or_length[i] * (ginitial_or_length[i] - 1);
  }

  /* 2 * #initial equivalences plus #initial OR clauses plus
   * 2 * [as we got two cnfs glued together] max ops to induce * 
   * (max ef implic of op * max noop implic) plus
   * max #additional clauses for improvement clauses
   */
  gmax_rs_clauses = 
    (2 * gnum_initial_equivalence) +
    (2 * MAX_PLAN_LENGTH * (gmax_E + (2 * gmax_CNFU))) +
    2;
  for ( i = 0; i < gnum_initial_or; i++ ) {
    /* all ordered pairs of fts yield one binary clause.
     */
    gmax_rs_clauses += ginitial_or_length[i] * (ginitial_or_length[i] - 1);
  }

  /* max. size effect axiom
   */ 
  gmax_literals = gmax_C + 1;
  /* if all effs of maxE op contradict with the same fact then
   * the resulting noop clause is this long.
   */ 
  if ( gmax_E + 2 > gmax_literals ) {
    gmax_literals = gmax_E + 2;
  }
  /* ini OR lengths...
   */
  for ( i = 0; i < gnum_initial_or; i++ ) {
    if ( ginitial_or_length[i] > gmax_literals ) {
      gmax_literals = ginitial_or_length[i];
    }
  }

  /* make space in plan states info, and relax; don't count the time for that.
   */
  for ( i = 0; i < MAX_PLAN_LENGTH + 1; i++ ) {
    make_state( &(gplan_states[i]), gnum_ft_conn );
  }
  initialize_state_transitions();
  extend_fixed_clauses_base( 0, 0 );
  extend_fixed_clauses_base_encoding( 0 );
  initialize_relax();
  if ( gcmd_line.dominating ) {
    initialize_repeated_states();
  }
  source_to_dest( &(gplan_states[0]), &ginitial_state );

  times( &end );
  TIME( gmem_time );

  times( &start );

  if ( gcmd_line.manual ) {
    if ( gcmd_line.debug == 0 ) {
      gcmd_line.debug = 1;
    }
    manual_control();
    printf("\n\n");
    exit( 0 );
  }

  found_plan = FALSE;
  if ( gcmd_line.ehc ) {
    found_plan = do_enforced_hill_climbing( &ginitial_state, &ggoal_state );
  }

  if ( !found_plan ) {
      if ( gcmd_line.ehc ) {
	  printf("\n\nEnforced Hill-climbing failed !");
	  printf("\nswitching to Best-first Search now.\n");
      }
    gnum_plan_ops = 0;
    found_plan = do_best_first_search();
  }

  times( &end );
  TIME( gsearch_time );

  if ( found_plan ) {
    print_plan();
  }

  output_planner_info();

  printf("\n\n");
  
  fclose(stream);
 
 FILE *op,*np;
 op = fopen("temp_out.txt","rb");
 np = fopen(out_filename,"wb");
 void *buf;
 while (!feof(op)){
	 fread(&buf,1,1,op);
	 fwrite(&buf,1,1,np);
	 }
fclose(np);
fclose(op);
remove("temp_out.txt");
 exit( 0 );

}
























/*
 *  ----------------------------- HELPING FUNCTIONS ----------------------------
 */






























void output_planner_info( void )

{

  int i;

  printf( "\n\nstatistics: %7.2f seconds instantiating %d easy, %d hard action templates", 
	  gtempl_time, gnum_easy_templates, gnum_hard_mixed_operators );
  printf( "\n            %7.2f seconds reachability analysis, yielding %d facts and %d actions", 
	  greach_time, gnum_pp_facts, gnum_actions );
  printf( "\n            %7.2f seconds creating final representation with %d relevant facts (%d max U, %d CNF max U)", 
	  grelev_time, gnum_relevant_facts, gmax_U, gmax_CNFU );
  printf( "\n            %7.2f seconds building connectivity graph",
	  gconn_time );



  printf( "\n            %7.2f seconds (%7.2f pure) evaluating %d states, to a max depth of %d",
	  geval_time + gr_sat_time + grp_sat_time + gr_cnf_time + gr_enc_time, 
	  geval_time, gevaluated_states, gmax_search_depth );
  if ( gcmd_line.heuristic == 1 ) {
    printf( "\n            %7.2f seconds in DP for %d RPG ini state implication checks", 
	    gr_sat_time, gr_sat_calls );
    printf( "\n            %7.2f seconds in DP for %d RPlan extract ini state implication checks (%d lits removed)", 
	    grp_sat_time, grp_sat_calls, gremoved_lits );
  }
  if ( gcmd_line.heuristic == 2 ) {
    printf( "\n            %7.2f seconds generating, %7.2f seconds encoding Rplan S-CNFs",
	    gr_cnf_time, gr_enc_time);
    printf( "\n            %7.2f seconds in DP for %d RPG S-CNF implication checks", 
	    gr_sat_time, gr_sat_calls );
    printf( "\n            %7.2f seconds in DP for %d RPlan extract S-CNF implication checks (%d lits removed)", 
	    grp_sat_time, grp_sat_calls, gremoved_lits );
  }



  printf( "\n            %7.2f seconds generating, %7.2f seconds encoding %d state transition base CNFs",
	  gcnf_time, genc_time, gcnfs);
  printf( "\n            %7.2f seconds in DP solving %d state transition CNFs", 
	  gsat_time, gsat_calls );
  printf( "\n            %7.2f seconds checking for self-contradictions, including %d DP calls", 
	  gsc_time, gsc_sat_calls );
  if ( gcmd_line.stagnating ) {
    printf( "\n            %7.2f seconds checking for stagnating states (%d hits), including %d DP calls", 
	    gss_time, gss_hits, gss_sat_calls );
  }
  if ( gcmd_line.dominating ) {
    printf( "\n            %7.2f seconds altogether checking for dominated states making %d comparisons (%d conformant, %d hits),\n                    spending %7.2f seconds doing %d DP calls", 
	    grs_time + grs_sat_time, grs_comps, grs_conf_comps, grs_hits, grs_sat_time, grs_sat_calls );
  }
  printf( "\n            %7d total DP calls, %d total UP calls, %7.2f sec membership", 
	  gdp_calls, gup_calls, gmembership_time);
  if ( gcmd_line.debug ) {
    printf("\n                CNF statistics:");
    for ( i = 0; i < gmax_literals + 1; i++ ) {
      printf(" %d:%.2f", i, ((float) gsum_k_clauses[i])/((float) gsum_clauses));
    }
  }
  printf( "\n            %7.2f seconds for remaining searching duties",
	  gsearch_time);
  printf( "\n            %7.2f seconds total time (+ %7.2f secs for CNF memory allocation)", 
	  gtempl_time + greach_time + grelev_time + gconn_time + genc_time + gsearch_time + gsat_time + geval_time + gr_sat_time + grp_sat_time + gr_cnf_time + gr_enc_time + gcnf_time + grs_time + gss_time + grs_sat_time, gmem_time);


}



void ff_usage( void )

{

  printf("\nUsage of Conformant-FF:\n");

  printf("\nOPTIONS   DESCRIPTIONS\n\n");
  printf("-p <str>    path for operator and fact file\n");
  printf("-a <str>    path for output file\n");
  printf("-o <str>    operator file name\n");
  printf("-f <str>    fact file name\n\n");

  printf("-h <num>    heuristic function to be used (preset: %d) (explanation: see journal paper)\n", 
	 gcmd_line.heuristic);
  printf("      0     implication graph for path to s plus RPG, incomplete check for leafs implication by I\n");
  printf("      1     implication graph for path to s plus RPG, complete check for leafs implication by I\n");
  printf("      2     implication graph for RPG, complete check for leafs implication by phi(s)\n\n");

  printf("-E          EHC run OFF\n");
  printf("-H          helpful actions OFF\n\n");

  printf("-S          stagnating paths check OFF\n");
  printf("-D          full repeated (dominated) states check OFF\n\n");

  printf("-B          run Best-first search in breadth-first style\n\n");

  printf("-M          manual search control\n");
  printf("-d <num>    debug info level (preset %d)\n", gcmd_line.debug);
  printf("-R          debug relax.c\n");
  printf("-A          debug search.c\n");
  printf("-T          debug state_transitions.cpp\n");
  printf("-P          debug repeated_states.cpp\n\n");

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
    printf("    121     connectivity graph\n");
    printf("    122     fixpoint result on each evaluated state\n");
    printf("    123     1P extracted on each evaluated state\n");
    printf("    124     H set collected for each evaluated state\n");
    printf("    125     False sets of goals <GAM>\n");
    printf("    126     detected ordering constraints leq_h <GAM>\n");
    printf("    127     the Goal Agenda <GAM>\n");
  }

}



Bool process_command_line( int argc, char *argv[] )

{

  char option;

  while ( --argc && ++argv ) {
    if ( *argv[0] != '-' || strlen(*argv) != 2 ) {
      return FALSE;
    }
    option = *++argv[0];
    switch ( option ) {
    case 'M':
      gcmd_line.manual = TRUE;
      break;
    case 'E':
      gcmd_line.ehc = FALSE;
      break;
    case 'H':
      gcmd_line.help = FALSE;
      break;
    case 'S':
      gcmd_line.stagnating = FALSE;
      break;
    case 'D':
      gcmd_line.dominating = FALSE;
      break;
    case 'B':
      gcmd_line.breadth_bfs = TRUE;
      break;
    case 'R':
      gcmd_line.R = TRUE;
      break;
    case 'A':
      gcmd_line.A = TRUE;
      break;
    case 'T':
      gcmd_line.T = TRUE;
      break;
    case 'P':
      gcmd_line.P = TRUE;
      break;
    default:
      if ( --argc && ++argv ) {
	switch ( option ) {
	case 'a':
		strncpy(out_filename,*argv,MAX_LENGTH);
		break;
	case 'p':
	  strncpy( gcmd_line.path, *argv, MAX_LENGTH );
	  break;
	case 'o':
	  strncpy( gcmd_line.ops_file_name, *argv, MAX_LENGTH );
	  break;
	case 'f':
	  strncpy( gcmd_line.fct_file_name, *argv, MAX_LENGTH );
	  break;
	case 'i':
	  sscanf( *argv, "%d", &gcmd_line.display_info );
	  break;
	case 'h':
	  sscanf( *argv, "%d", &gcmd_line.heuristic );
	  break;
	case 'd':
	  sscanf( *argv, "%d", &gcmd_line.debug );
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

  if ( gcmd_line.heuristic < 0 || gcmd_line.heuristic > 3 ) {
    printf( "\nunknown heuristic function, %d\n\n", gcmd_line.heuristic );
    return FALSE;
  }
  return TRUE;

}

