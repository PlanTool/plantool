



/*********************************************************************
 * File: main.c
 * Description: The main routine for ipp.
 *
 * Author: Joerg Hoffmann / Frank Rittinger / Andreas Schoen
 * 
 *********************************************************************/ 
/*********************************************************************
 * (C) Copyright 1998 Albert Ludwigs University Freiburg
 *     Institute of Computer Science
 *
 * All rights reserved. Use of this software is permitted for 
 * non-commercial research purposes, and it may be copied only 
 * for that use.  All copies must include this copyright message.
 * This software is made available AS IS, and neither the authors
 * nor the  Albert Ludwigs University Freiburg make any warranty
 * about the software or its performance. 
 *********************************************************************/






#include "ipp.h"

#include "output.h"
#include "utilities.h"
#include "memory.h"

#include "pddl.h"
#include "pddl-types.h"

#include "instantiateI.h"
#include "instantiateII.h"
#include "instantiateIII.h"
#include "instantiateIV.h"

#include "build_graph.h"
#include "exclusions.h"

#include "search_plan.h"
#include "memoize.h"
#include "wave_front.h"

#include "save_graph.h"
#include "stdio.h"







/*
 *  ----------------------------- GLOBAL VARIABLES ----------------------------
 */












/*******************
 * GENERAL HELPERS *
 *******************/








/* used to time the different stages of the planner
 */
struct tms gstart, gend;
float gtotal_time = 0, gexcl_time = 0;

/* the command line inputs
 */
struct _command_line gcmd_line;


/* simple help: store names of connectives
 */
char *gconnectives[] = {"ATOM", "NOT", "AND", "OR", "ALL", "EX", "WHEN",
			 "TRU", "FAL", "EMPTY", "DUMMY", "NEUTRAL"};


/* word size of the used machine
 */
const int gcword_size = sizeof(int) * 8;


/* record memory consumption
 */
int gmemory = 0, ggraph_memory = 0, gexcl_memory = 0;
int gmemo_memory = 0, gwave_memory = 0;


/* default graph save name
 */
char gdef_save_name[MAX_LENGTH] = "graph";



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

/* to store all typed objects 
 */
FactList *gorig_constant_list = NULL;

/* type hierarchy (PDDL) 
 */
type_tree_list gglobal_type_tree_list;

/* helper for types parsing
 */
FactList *gtypes;

/* the predicates and their types as defined in the domain file
 */
FactList *gpredicates_and_types;







/*****************
 * INSTANTIATING *
 *****************/







/* global arrays of constant names,
 *               type names (with their constants),
 *               predicate names,
 *               predicate aritys,
 *               defined types of predicate args
 */
String gconstants_table[MAX_CONSTANTS_TABLE];
int gconstants_table_size = 0;
StringIntegers gtypes_table[MAX_TYPES_TABLE];
int gtype_size[MAX_TYPES_TABLE];
int gtypes_table_size = 0;
String gpredicates_table[MAX_PREDICATES_TABLE];
int garity[MAX_PREDICATES_TABLE];
int gpredicates_args_type[MAX_PREDICATES_TABLE][MAX_ARITY];
int gpredicates_table_size = 0;


/* the parsed input structures, translated into CodeNodes
 */
CodeNode *gcode_initial_state = NULL;
CodeNode *gcode_goal_state = NULL;
CodeOperator *gcode_operators = NULL;


/* helper in solving the Atomic Instantiation problem: 
 *                                    the implicit tuple tables
 *
 * ( see Technical Report 122, "Handling of Inertia in a Planning System" )
 */
int_pointer gtuples[MAX_PREDICATES_TABLE];
int gone_table_size[MAX_PREDICATES_TABLE];


/* stores inertia - information: is any occurence of the predicate
 * added / deleted in the uninstantiated ops ?
 *
 * ( see TR 122 )
 */
Bool gis_added[MAX_PREDICATES_TABLE];
Bool gis_deleted[MAX_PREDICATES_TABLE];


/* store the final "relevant facts", see TR 122
 */
RelevantFact_pointer grelevant_facts[MAX_RELEVANT_FACTS];
int gnum_relevant_facts = 0;
CodeOperator *ginst_code_operators = NULL;/* helper: first get all inst.ops */


/* standard name for inferred types ( unary inertia, see TR 122 )
 */
char gnew_types_name[MAX_LENGTH] = "INFERRED TYPE";


/* standard name for GOAL-REACHED fact, as needed for disjunctive goals
 */
char ggoal_reached_name[MAX_LENGTH] = "GOAL-REACHED";






/*************************
 * BITMAP REPRESENTATION *
 *************************/







/* the bitvector length for relevant facts
 */
int gft_vector_length;


/* final representation of ops,
 *                         initial state,
 *                         goal state
 */
BitOperator *gbit_operators = NULL;
int gnum_bit_operators = 0;
FactInfoPair *gbit_initial_state = NULL;
FactInfoPair *gbit_goal_state = NULL;










/**********************
 * BUILDING THE GRAPH *
 **********************/
 









/* 
 * dis is da graph!
 *
 * will later be allocated as an array of pointers to fact nodes
 */
FtNode_pointer *gft_table;


/* points to the first element of a global operator (ft) -node-list;
 */ 
OpNode *gall_ops_pointer = NULL;
OpNode *gprev_level_ops_pointer = NULL;
FtNode *gall_fts_pointer = NULL;
FtNode *gprev_level_fts_pointer = NULL;

OpNode *gops_with_unactivated_effects_pointer = NULL;


/* current mutexes: exclusives speedup
 */
OpPair *gop_mutex_pairs = NULL;
FtPair *gft_mutex_pairs = NULL;


/* information about current state of graph, needed for level off test
 */
unsigned int gfacts_count = 0, gexclusions_count = 0;
unsigned int gops_count = 0, gops_exclusions_count = 0;


/* for comparison: mutex number between positives
 */
int gprint_ftnum = 0, gprint_exnum = 0;


/* the present facts ordered by levels as bitvectors
 */
BitVector_pointer gpos_facts_vector_at[MAX_PLAN];
BitVector_pointer gneg_facts_vector_at[MAX_PLAN];


/* the bitvector length for ops at each graph level
 */
unsigned int gop_vector_length_at[MAX_PLAN];


/* is TRUE iff graph has levelled off.
 */
Bool gsame_as_prev_flag = FALSE;


/* stores the time step at which graph has levelled off.
 */
int gfirst_full_time = 0;










/*************
 * SEARCHING *
 *************/
 







/* current state of search: goals at levels,
 *                          same as bitvectors,
 *                          selected ops
 */
FtArray *ggoals_at;
int *gnum_goals_at;
BitVector_pointer gpos_goals_vector_at[MAX_PLAN];
BitVector_pointer gneg_goals_vector_at[MAX_PLAN];
OpArray *gops_at;
int *gnum_ops_at;


/* the wave front, currently implemented as a doubly
 * connected linear list
 */
Candidate *gwave_front_head;
Candidate *gwave_front_tail;


/* to avoid memory leak: keep a pointer on the list of
 * candidates that have been expanded and removed from
 * the wave front already
 */
Candidate *gwave_front_trash;


/* search space information: actions, noops tried,
 *                           memoization (UBTree) hits
 */
int gnum_of_actions_tried = 0, gnum_of_noops_tried = 0;
int gsimple_hits = 0, gpartial_hits = 0, gsubset_hits = 0;


/* only for communication from wave front to save graph:
 * to find out, which ops are used in the plan, we need
 * to search the list of candidates (connected by ->father)
 * that starts with the one Candidate that finally led to 
 * a plan.
 *
 * not really good implementation style, but who does really
 * care about this ?
 */
Candidate *gplan_start;






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









int run( int argc, char *argv[] )

{
  FILE *stream;

  stream = freopen("output.txt", "a+", stdout);


  /* resulting name for ops file
   */
  char ops_file[MAX_LENGTH] = "";
  /* same for fct file 
   */
  char fct_file[MAX_LENGTH] = "";
  
  int min_time = 0;
  Bool reached_goals, found_plan;

  struct tms start, end;
  float inst_time = 0, build_time = 0, search_time = 0;

  CodeOperator *o;



  /* start overall timing
   */
  times( &gstart );
  

  /* command line treatment
   */
  if ( argc == 1 || ( argc == 2 && *++argv[0] == '?' ) ) {
    ipp_usage();
    exit( USAGE_ERROR_CODE );
  }
  if ( !process_command_line( argc, argv ) ) {
    ipp_usage();
    exit( USAGE_ERROR_CODE );
  }


  /* make file names
   */

  /* one input name missing
   */
  if ( !gcmd_line.ops_file_name || 
       !gcmd_line.fct_file_name ) {
    fprintf(stdout, "\nipp: two input files needed\n\n");
    ipp_usage();      
    exit( USAGE_ERROR_CODE );
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
  if ( gcmd_line.display_info  ) {
    fprintf(OUT, "\nipp: parsing domain file");
  } 
  /* it is important for the pddl language to define the domain before 
   * reading the problem 
   */
  load_ops_file( ops_file );
  /* problem file (facts)
   */  
  if ( gcmd_line.display_info  ) {
    fprintf(OUT, " ... done.\nipp: parsing problem file"); 
  }
  load_fct_file( fct_file );
  if ( gcmd_line.display_info  ) {
    fprintf(OUT, " ... done.\n\n");
  }


  /* build type hierarchy
   */

  /* This is needed to get all types.
   */
  build_orig_constant_list(); 


  /* preprocess PlNodes so as to be able to translate them
   * into CodeNodes
   */

  /* assign unique names to variables and normalize effects
   */
  pddl_outer();


  /* collect all strings in the domain representation, store
   * them into global string arrays, and
   * collect inertia information on the way.
   * also create types and predicate args types
   */

  /* compute values of global arrays
   *
   * gconstants_table
   * gtypes_table
   * gpredicates_table
   * garity
   * gpredicates_args_type
   */
  collect_all_strings();

  /* translate Input formula into coded format;
   *
   * also do syntax checking 
   * (legal effects, conditions, quantification etc.)
   * and structure normalization
   */
  transform_PlNodes_to_CodeNodes();

  /* perform advanced domain definition check:
   * simplify input, i.e., search for formula that can be
   * trivially simplified and issue warnings, if one is found.
   *
   * then, find logical tautologies, like A and not A, and issue 
   * warnings for any such occurence.
   *
   * afterwards, continue planning on the again simplified 
   * (due to tautologies) domain description
   *
   * NOTE: the arguments to that functions only say that "yes, issue
   *       a warning wenever you find something" 
   */
  simplify_all_CodeNodes( TRUE );
  detect_all_tautologies( TRUE );
  simplify_all_CodeNodes( TRUE );


  /* prepare for instantiation process:
   * build implicit tuple tables for fast access to
   * initial state information ( see Technical Report 122 )
   *
   * also used in unary to types encoding, that's why it
   * needs to be done here already
   */
  build_predicate_tables();

  /* encode unary inertia used in op and goal state description
   * as types (see TR 122), to make instantiation
   * process a good deal faster
   *
   * afterwards, find new (maybe resulting from that) tautologies,
   * simplify description and go on.
   */
  encode_unary_inertia_in_types();
  detect_all_tautologies( FALSE );
  simplify_all_CodeNodes( FALSE );
  if ( gcmd_line.display_info == 5 ) {
    printf("\nfinal pre-instantiation initial state is:\n");
    print_CodeNode( gcode_initial_state, 0 );
    printf("\nfinal pre-instantiation goal state is:\n");
    print_CodeNode( gcode_goal_state, 0 );
    printf("\nfinal pre-instantiation operators are:");
    for ( o = gcode_operators; o; o = o->next ) {
      print_CodeOperator( o );
    }
  }

  /* the actual instantiation part, first step that actually may need
   * significant computation time: create all instances of operator
   * schemata, and transform all quantifiers (ALL/EX) into con/dis junction
   *
   * representation is simultaneously cleand up where easily possible (in
   * particular, initial + goal state are fully simplified and checked),
   * as that might well save a lot of time. 
   *
   * for better overview, the final clean up of operators, tautologies and (e.g.) 
   * empty effects removal, is done separately in spite of this, after also setting
   * relevants.
   *
   * anyone crazy enough to try can maybe gain some efficiency by
   * messing the two (three) parts of code together. GOOD LUCK !
   *
   * something more striking: I don't really know if at all, or how many,
   *                          tautologies can result from instantiating.
   *                          maybe one can spare the tautologie search
   *                          in all instances and then, also the simplification
   *                          of condition CodeNodes in final clean up
   */
  multiply_params_and_quantifiers();
  if ( gcmd_line.display_info == 6 ) {
    printf("\n1. step instantiated initial state is:\n");
    print_CodeNode( gcode_initial_state, 0 );
    printf("\n1. step instantiated goal state is:\n");
    print_CodeNode( gcode_goal_state, 0 );
    printf("\n1. step instantiated operators are:");
    for ( o = ginst_code_operators; o; o = o->next ) {
      print_CodeOperator( o );
    }
  }

  /* now we don't need the global tuple tables anymore;
   * so free their memory
   */
  free_predicate_tables();


  /* detect all facts that can, in principle, change their truth value during
   * planning. these are the facts that are relevant to the problem.
   *
   * set all other atoms to TRUE/FALSE and perform a final clean up of the 
   * actions. (initial + goal are also simplified a last time)
   */
  collect_relevant_facts();
  detect_inst_CodeOperator_tautologies();
  simplify_all_inst_CodeOperators();
  if ( gcmd_line.display_info == 8 ) {
    printf("\nfinal initial state is:\n");
    print_CodeNode( gcode_initial_state, 0 );
    printf("\nfinal goal state is:\n");
    print_CodeNode( gcode_goal_state, 0 );
    printf("\nfinal operators are:");
    for ( o = ginst_code_operators; o; o = o->next ) {
      print_CodeOperator( o );
    }
  }

  /* transform the cleaned up and simplified CodeNode ops, initial and goal
   * into bitmaps, i.e., represent each fact by its index in grelevant_facts
   * and a set of facts by the membership vector. if goal is disjunctive,
   * add a new GOAL_REACHED fact as well as one obvious op for each disjunct.
   * On the way, melt identical effects, which means that effects that have the same
   * set of preconditions are put together. The identification of an op's 
   * unconditional effects is a special case of this
   */
  generate_bitmap_representation();

  /* now we're actually finished with instantiating and can worry about
   * planning for a change
   */
  times( &end );
  TIME( inst_time );

  if ( gcmd_line.display_info ) {
    printf("\ninstantiated %d actions\n\n\n", gnum_bit_operators);
  }

  /* build the graph until goals or fixpoint are reached
   */
  min_time = gcmd_line.min_time;
  times(&start);
  reached_goals = build_graph( &min_time );
  times(&end);
  TIME( build_time );

  if ( !reached_goals ) {
    if ( min_time < MAX_PLAN ) {/* fixpoint without goals */
      if ( gcmd_line.display_info ) {
	fprintf( OUT, "\nproblem unsolvable: can't reach non exclusive goals\n\n");
	output_planner_info( inst_time, build_time - gexcl_time,
			     gexcl_time, search_time, min_time );
      }
    } else {
      if ( gcmd_line.display_info ) {/* graph oversized */
	fprintf( OUT, "\nMAX_PLAN too small( preset value: %d )\n\n", MAX_PLAN );
	output_planner_info( inst_time, build_time - gexcl_time,
			     gexcl_time, search_time, min_time );
      }
    }
    /*
    exit( 0 );
    */
    return 1;
}

  if ( gsame_as_prev_flag ) {/* we're in the fixpoint already */
    if ( gcmd_line.display_info ) {
      fprintf( OUT, "\ngraph has leveled off! wave front mechanism is taking over\n");
    }
    times(&start);
    /* after graph has leveled off, search is overtaken by the so-called wave front,
     * as introduced by Long and Fox in 
     * "The efficient Implementation of the Planning Graph in STAN", JAIR 10,1999
     */
    min_time = search_wave_front();
    times(&end);
    TIME( search_time );
    if ( gcmd_line.display_info ) {
      output_planner_info( inst_time, build_time - gexcl_time,
			   gexcl_time, search_time, min_time );
    }
    exit( 0 );
  }

  for ( ; min_time < MAX_PLAN; min_time++ ) {
    /* search and build one extra layer in alternating steps
     */

    times(&start);
    found_plan = search_plan( min_time );
    times(&end);
    TIME( search_time );

    if ( found_plan ) {
      break;
    }

    times(&start);
    build_graph_evolution_step();
    times(&end);
    TIME( build_time );
 
    if ( gsame_as_prev_flag ) {
      if ( gcmd_line.display_info ) {
	fprintf( OUT, "\ngraph has leveled off! wave front mechanism is taking over\n");
      }
      times(&start);
      min_time = search_wave_front();
      times(&end);
      TIME( search_time );
      if ( gcmd_line.display_info ) {
	output_planner_info( inst_time, build_time - gexcl_time,
			     gexcl_time, search_time, min_time );
      }
      exit( 0 );
    }

  }


  if ( gcmd_line.display_info ) {
    print_plan( min_time );
    fprintf( OUT, "\n" );
    output_planner_info( inst_time, build_time - gexcl_time,
			 gexcl_time, search_time, min_time );
    freopen("data.out","w",stdout); 
  }

  freopen("/data.out","w",stdout); 
  exit( 0 );

}

int main( int argc, char * argv[]) {

  return run(argc, argv);
}









/*
 *  ----------------------------- HELPING FUNCTIONS ----------------------------
 */












void output_planner_info( float inst_time, float build_time,
			  float excl_time, float search_time,
			  int min_time )

{

  if ( gnum_of_actions_tried > 0 ) {
    fprintf( OUT, "\n\nnumber of actions tried: %10d", 
	     gnum_of_actions_tried-gnum_of_noops_tried);
    fprintf( OUT, "\nnumber of noops tried  : %10d", gnum_of_noops_tried);
    fprintf( OUT, "\n\nhad %7d simple memoizing hits", gsimple_hits );
    fprintf( OUT, "\nhad %7d partial memoizing hits", gpartial_hits );
    fprintf( OUT, "\nhad %7d subset memoizing hits", gsubset_hits );
    fprintf( OUT, "\n" );
  }
  fprintf( OUT, "\ntime spent: %7.2f seconds instantiating %d operators", 
	   inst_time, gnum_bit_operators );
  fprintf( OUT, "\n            %7.2f seconds building graph", build_time );
  fprintf( OUT, "\n            %7.2f seconds calculating exclusions", excl_time );
  if ( gnum_of_actions_tried > 0 ) {
    fprintf( OUT, "\n            %7.2f seconds searching graph", search_time );
    fprintf( OUT, "\n            %7.2f seconds total time", 
	     inst_time + build_time + excl_time + search_time );
  }
 
  fprintf( OUT, "\n\nMemory used: %6.2f MBytes for domain representation", (float) gmemory/1000000);
  fprintf( OUT, "\n             %6.2f MBytes for graph", (float) ggraph_memory/1000000);
  fprintf( OUT, "\n             %6.2f MBytes for exclusions", (float) gexcl_memory/1000000);
  fprintf( OUT, "\n             %6.2f MBytes for memoization", (float) gmemo_memory/1000000);
  fprintf( OUT, "\n             %6.2f MBytes for wave front\n\n", (float) gwave_memory/1000000);

  printf("\n\n");

  if ( gcmd_line.write_graph ) {
    if ( gcmd_line.display_info ) {
      printf("\nwriting graph...\n\n");
    }
    SaveGraph( gcmd_line.save_name, min_time );
  }

  freopen("data.out","w",stdout); 
}





void ipp_usage( void )

{

  printf("\nusage of ipp:\n\n");
  printf("OPTIONS   DESCRIPTIONS\n\n");
  printf("-p <str>    path for operator and fact file\n");
  printf("-o <str>    operator file name\n");
  printf("-f <str>    fact file name\n\n");

  printf("-i <num>    run-time information level( preset: 1 )\n");
  printf("     0      nothing\n");
  printf("     1      info on action number, graph, search and plan\n");
  printf("     2      1 + info on problem constants, types and predicates\n");  
  printf("     3      1 + 2 + loaded operators, initial and goal state\n");
  printf("     4      1 + predicates and their inertia status\n");
  printf("     5      1 + 4 + goal state and operators with unary inertia encoded\n");
  printf("     6      1 + actions, initial and goal state after expansion of variables\n"); 
  printf("     7      1 + facts selected as relevant to the problem\n");
  printf("     8      1 + final domain representation\n");
  printf(" > 100      1 + various debugging information\n\n");


  printf("-W          write complete graph to text files after planning\n");
  printf("-w <str>    specify name for graph output files( preset: graph )\n\n");

  printf("-m <num>    build graph up to level <num> without search\n");
  printf("-S          don't do complete subset test in memoization\n\n");

}






Bool process_command_line( int argc, char *argv[] )

{

  char option;

  memset(gcmd_line.path, 0, MAX_LENGTH);
  memset(gcmd_line.ops_file_name, 0, MAX_LENGTH);
  memset(gcmd_line.fct_file_name, 0, MAX_LENGTH);

  gcmd_line.display_info = 1;

  gcmd_line.write_graph = FALSE;
  gcmd_line.save_name = gdef_save_name;

  gcmd_line.do_subset = TRUE;
  gcmd_line.min_time = 0;

  gcmd_line.debug = 0;

  while ( --argc && ++argv ) {
    if ( *argv[0] != '-' || strlen(*argv) != 2 ) {
      return FALSE;
    }
    option = *++argv[0];
    switch ( option ) {
    case 'W':
      gcmd_line.write_graph = TRUE;
      break;
    case 'S':
      gcmd_line.do_subset = FALSE;
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
	case 'i':
	  sscanf( *argv, "%d", &gcmd_line.display_info );
	  break;
	case 'w':
	  strncpy( gcmd_line.save_name, *argv, MAX_LENGTH );
	  break;
	case 'm':
	  sscanf( *argv, "%d", &gcmd_line.min_time );
	  break;
	case 'd':
	  sscanf( *argv, "%d", &gcmd_line.debug );
	  break;
	default:
	  printf( "\nipp: unknown option: %c entered\n\n", option );
	  return FALSE;
	}
      } else {
	return FALSE;
      }
    }
  }

  return TRUE;

}

