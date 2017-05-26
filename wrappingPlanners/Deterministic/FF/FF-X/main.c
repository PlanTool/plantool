

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








#include "ff.h"

#include "memory.h"
#include "output.h"

#include "parse.h"

#include "inst_pre.h"
#include "inst_easy.h"
#include "inst_hard.h"
#include "inst_final.h"

#include "orderings.h"

#include "relax.h"
#include "search.h"
#include <string.h>











/*
 *  ----------------------------- GLOBAL VARIABLES ----------------------------
 */












/*******************
 * GENERAL HELPERS *
 *******************/








/* used to time the different stages of the planner
 */
float gtempl_time = 0, greach_time = 0, grelev_time = 0, gconn_time = 0;
float gsearch_time = 0;


/* the command line inputs
 */
struct _command_line gcmd_line;

/* number of states that got heuristically evaluated
 */
int gevaluated_states = 0;

/* maximal depth of breadth first search
 */
int gmax_search_depth = 0;





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
Bool gderived[MAX_PREDICATES];





/* the domain in integer (Fact) representation
 */
Operator_pointer goperators[MAX_OPERATORS];
int gnum_operators = 0;
Fact *gfull_initial;
int gnum_full_initial = 0;
WffNode *ggoal = NULL;




/* stores inertia - information: is any occurence of the predicate
 * added / deleted in the uninstantiated ops ?
 */
Bool gis_added[MAX_PREDICATES];
Bool gis_deleted[MAX_PREDICATES];



/* splitted initial state:
 * initial non static facts,
 * initial static facts, divided into predicates
 * (will be two dimensional array, allocated directly before need)
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
int gnum_relevant_facts = 0;
int gnum_pp_facts = 0;



/* the final actions and problem representation
 */
Action *gactions;
int gnum_actions;
State ginitial_state;
State ggoal_state;









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









/*******************
 * SEARCHING NEEDS *
 *******************/








/* the goal state, divided into subsets
 */
State *ggoal_agenda;
int gnum_goal_agenda;



/* byproduct of fixpoint: applicable actions
 */
int *gnaxA;
int gnum_naxA;
int *gaxA;
int gnum_axA;



/* communication from extract 1.P. to search engines:
 * 1P action choice
 */
int *gH;
int gnum_H;



/* the effects that are considered true in relaxed plan
 */
int *gin_plan_E;
int gnum_in_plan_E;



/* always stores (current) serial plan
 */
int gplan_ops[MAX_PLAN_LENGTH];
int gnum_plan_ops = 0;



/* stores the states that the current plan goes through
 * ( for knowing where new agenda entry starts from )
 */
State gplan_states[MAX_PLAN_LENGTH + 1];



int *gaxdels, gnum_axdels;

int gnum_strata;




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




int run( int argc, char *argv[] )

{
	/*
	 * *******************
	 */
	 
	

  /* resulting name for ops file
   */
  char ops_file[MAX_LENGTH] = "";
  /* same for fct file 
   */
  char fct_file[MAX_LENGTH] = "";
  
  struct tms start, end;

  int i, j;
  Bool found_plan;


  times ( &lstart );


	FILE *stream;
	stream = freopen("temp_out.txt","w",stdout);
	if( stream == NULL )  
     fprintf( stdout, "error on freopen\n" );
	

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

  gnum_strata = 1;
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

  initialize_relax();
  i = 0;
  while ( i < ginitial_state.num_F ) {
      if ( !gft_conn[ginitial_state.F[i]].axiom_add &&
	   !gft_conn[ginitial_state.F[i]].axiom_del ) {
	  i++;
	  continue;
      }
      for ( j = i; j < ginitial_state.num_F-1; j++ ) {
	  ginitial_state.F[j] = ginitial_state.F[j+1];
      }
      ginitial_state.num_F--;
  }
  do_axiom_update( &ginitial_state );

  /* make space in plan states info, and relax
   */
  for ( i = 0; i < MAX_PLAN_LENGTH + 1; i++ ) {
    make_state( &(gplan_states[i]), gnum_ft_conn );
    gplan_states[i].max_F = gnum_ft_conn;
  }

  found_plan = do_enforced_hill_climbing( &ginitial_state, &ggoal_state );

  if ( !found_plan ) {
    printf("\n\nEnforced Hill-climbing failed !");
    printf("\nswitching to Best-first Search now.\n");
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
 
 printf("*******************");
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


int main( int argc, char *argv[] )

{

  return run(argc,argv);

}













/*
 *  ----------------------------- HELPING FUNCTIONS ----------------------------
 */










FILE *out;


void output_planner_info( void )

{

  char name[MAX_LENGTH];

  printf( "\n\ntime spent: %7.2f seconds instantiating %d easy, %d hard action templates", 
	  gtempl_time, gnum_easy_templates, gnum_hard_mixed_operators );
  printf( "\n            %7.2f seconds reachability analysis, yielding %d facts and %d actions", 
	  greach_time, gnum_pp_facts, gnum_actions );
  printf( "\n            %7.2f seconds creating final representation with %d relevant facts", 
	  grelev_time, gnum_relevant_facts );
  printf( "\n            %7.2f seconds building connectivity graph",
	  gconn_time );
  printf( "\n            %7.2f seconds searching, evaluating %d states, to a max depth of %d", 
	  gsearch_time, gevaluated_states, gmax_search_depth );
  printf( "\n            %7.2f seconds total time", 
	  gtempl_time + greach_time + grelev_time + gconn_time + gsearch_time );

  printf("\n\n");

  sprintf( name, "TIME-%s", gcmd_line.fct_file_name );
  if ( (out = fopen( name, "w")) == NULL ) {
    printf("\n\nCan't open output file!\n\n");
    return;
  }
  times( &lend );
  fprintf(out, "%.2f\n", 
	 ((float) (lend.tms_utime - lstart.tms_utime + lend.tms_stime - lstart.tms_stime)) / 100.0);
  fclose( out );

  sprintf( name, "SEARCHTIME-%s", gcmd_line.fct_file_name );
  if ( (out = fopen( name, "w")) == NULL ) {
    printf("\n\nCan't open output file!\n\n");
    return;
  }
  times( &lend );
  fprintf(out, "%.2f\n", gsearch_time);
  fclose( out );


}



void print_official_result( void )

{

  char name[MAX_LENGTH];

  sprintf( name, "TIME-%s", gcmd_line.fct_file_name );

  if ( (out = fopen( name, "w")) == NULL ) {
    printf("\n\nCan't open output file!\n\n");
    return;
  }

  times( &lend );
  fprintf(out, "%.2f\n", 
	 ((float) (lend.tms_utime - lstart.tms_utime + lend.tms_stime - lstart.tms_stime)) / 100.0);

  fclose( out );

}



void print_official_op_name( int index )

{

  int i;
  Action *a = gop_conn[index].action;

  if ( a->norm_operator ||
       a->pseudo_action ) {
    fprintf(out, "(%s", a->name ); 
    for ( i = 0; i < a->num_name_vars; i++ ) {
      fprintf(out, " %s", gconstants[a->name_inst_table[i]]);
    }
    fprintf(out, ")");
  }

}



void ff_usage( void )

{

  printf("\nusage of ff:\n");

  printf("\nOPTIONS   DESCRIPTIONS\n\n");
  printf("-p <str>    path for operator and fact file\n");
  printf("-a <str>    path for output file\n");
  printf("-o <str>    operator file name\n");
  printf("-f <str>    fact file name\n\n");
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



Bool process_command_line( int argc, char *argv[] )

{

  char option;

  gcmd_line.display_info = 1;
  gcmd_line.debug = 0;
  
  memset(gcmd_line.ops_file_name, 0, MAX_LENGTH);
  memset(gcmd_line.fct_file_name, 0, MAX_LENGTH);
  memset(gcmd_line.path, 0, MAX_LENGTH);

  while ( --argc && ++argv ) {
    if ( *argv[0] != '-' || strlen(*argv) != 2 ) {
      return FALSE;
    }
    option = *++argv[0];
    switch ( option ) {
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

  return TRUE;

}

