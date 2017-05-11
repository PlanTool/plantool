

/*********************************************************************
 * (C) Copyright 1999 Albert Ludwigs University Freiburg
 *     Institute of Computer Science
 *
 * All rights reserved. Use of this software is permitted for 
 * non-commercial research purposes, and it may be copied only 
 * for that use.  All copies must include this copyright message.
 * This software is made available AS IS, and neither the authors
 * nor the  Albert Ludwigs University Freiburg make any warranty
 * about the software or its performance. 
 *********************************************************************/




/*********************************************************************
 * File: main.c
 * Description: The main routine for the Branch-and-Bound Planner.
 *
 * Author: Joerg Hoffmann 2002
 *  Modified 2004 and 2006 
 *********************************************************************/ 








#include "bb.h"

#include "memory.h"
#include "output.h"

#include "parse.h"

#include "instantiateI.h"
#include "instantiateII.h"

#include "graph.h"

#include "cnfout.h"

#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <signal.h>
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
float gbuild_time = 0, gsearch_time = 0, gcnf_time = 0;


/* the command line inputs
 */
struct _command_line gcmd_line;

/* number of states that got heuristically evaluated
 */
int gevaluated_states = 0;

/* info on search spaces structure
 */
int gmax_search_depth = 0;
int gmax_search_size = 0;



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
FactList *gtypes = NULL;

/* the predicates and their types as defined in the domain file
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
int gnum_operators;
Fact gfull_initial[MAX_INITIAL];
int gnum_full_initial = 0;
Fact ggoal[MAX_STATE];
int gnum_goal = 0;






/* stores inertia - information: is any occurence of the predicate
 * added / deleted in the uninstantiated ops ?
 */
Bool gis_added[MAX_PREDICATES];
Bool gis_deleted[MAX_PREDICATES];

/* splitted initial state:
 * initial non static facts,
 * initial inertia facts of arity > 1
 */
Fact ginitial[MAX_STATE];
int gnum_initial = 0;
Fact ginertia[MAX_INITIAL];
int gnum_inertia = 0;

/* the type numbers corresponding to any unary inertia
 */
int gtype_to_predicate[MAX_PREDICATES];
int gpredicate_to_type[MAX_TYPES];

/* (ordered) numbers of types that new type is intersection of
 */
TypeArray gintersected_types[MAX_TYPES];
int gnum_intersected_types[MAX_TYPES];





/* intermediate step: store minimal action info, use it
 * to determine relevant facts, number of actions and final form
 * before creating actual actions
 */
ActionTemplate *gtemplates = NULL;
int gnum_templates = 0;




/* store the final "relevant facts"
 */
Fact grelevant_facts[MAX_RELEVANT_FACTS];
int gnum_relevant_facts = 0;
int gnum_pp_facts;


/* the (fully instantiated) domain in integer 
 * (number of relevant Fact) representation
 */
Action *gactions = NULL;
int gnum_actions;
State ginitial_state;
State ggoal_state;








/**********************
 * CONNECTIVITY GRAPH *
 **********************/






/* one fact array, ..
 */
FtConn *gft_conn;
int gnum_ft_conn;

/* one ops (actions) array
 */
OpConn *gop_conn;
int gnum_op_conn;



int gcword_size; 
int gnum_ft_bit;
int gnum_op_bit_at[MAX_GRAPH];
int gnum_ops = 0, gmax_block = 0;

/* the goals in bit-format...
 */
BitVector *gbit_goal_state;







/* some technical graph... things
 */





/* the ops not yet in the graph
 */
IntList *gout_ops = NULL;

/* global op-state resp. ft-state lists
 */ 
IntList *gin_ops = NULL;
IntList *gin_prev_ops = NULL;
IntList *gin_fts = NULL;
IntList *gin_prev_fts = NULL;

/* we want to have fast access, during constraint propagation,
 * to the fts that can (maximally) be present at the layer;
 * can easily store this by means of pointers into the gin_fts list
 * which we got anyway.
 */
IntList *gin_fts_at[MAX_GRAPH];
/* the present ops are good to have for enforcing a positive constraint:
 * we must then negatively enforce the exclusive ops. don't wanna store
 * all the exclusives in a list but at least we can skip those ops that
 * aren't even there. means as above.
 */
IntList *gin_ops_at[MAX_GRAPH];

/* current mutexes: exclusives speedup
 */
IntPair *gin_op_mutex_pairs = NULL;
IntPair *gin_ft_mutex_pairs = NULL;

/* information about current state of graph, needed for level off test
 */
int gin_ft_count = 0, gin_ft_exclusion_count = 0;
int gin_op_count = 0, gin_op_exclusion_count = 0;

/* is TRUE iff graph has levelled off.
 */
Bool gsame_as_prev_flag = FALSE;

/* stores the time step at which graph has levelled off.
 */
int gfirst_full_time = 0;











/* some search things
 */






/* flag saying that depth first has encountered a solution!
 */
Bool gfound_plan;



/* number of action constraints enforced successfully
 */
int gconstr_succ = 0;
/* same, unsuccessfully
 */
int gconstr_fail = 0;



/* search info: the currently positively enforced ops at each time
 */
int **genforced_ops_at;
int *gnum_enforced_ops_at;





/* during PG re-computation: forcing an action A in might
 * force actions below in, too - namely, if they're the
 * only achievers of a prec of A. remember these actions and
 * force them in once propagation for A has terminated.
 */
Decvar gto_be_enforced[ARRAY_SIZE];
int gnum_to_be_enforced;


/* some statistics on transitively forced in - ops
 * names refer to paper work on unit propag == rplan propag
 */
int gnum_bins = 0;
int gnum_d2ins = 0;





struct tms gstart, gend;








/* the dec vars sufficing for BBG search at the respective layers!
 * 
 * in case of solution: dec vars below that sol.
 * in case of unsat:  all dec vars expanded during search (?)
 */
Decvar gbbg_decvars[MAX_GRAPH][MAX_DECVARS];
int gnum_bbg_decvars[MAX_GRAPH];
int gnum_bbg_decs;



/* this is what bd verification works on.
 */
Decvar gbd_candidate[MAX_DECVARS];
int gnum_bd_candidate;



/* technical, for backdoor verification.
 */
Bool gsolution_found;
Bool girresolved_found;
int gbd_candidate_curr;
Bool gcontrol_strongbd;
 


/* rplan now also needed in backdoors.
 */
int **grelaxed_ops_at;
int *gnum_relaxed_ops_at;



/* the decvar Database to choose from, for 
 * enumeration
 */
Decvar gD[MAX_D_DECVARS];
int gnum_D;












/*
 *  ----------------------------- HEADERS FOR PARSING ----------------------------
 * ( fns defined in the scan-* files )
 */







void get_fct_file_name( char *filename );
void load_ops_file( char *filename );
void load_fct_file( char *filename );



struct tms start, end;








/*
 *  ----------------------------- MAIN ROUTINE ----------------------------
 */

/* start - globals and prototypes added by sjn */
const int SAT = 0;
const int UNSAT = 2;
const int UNRECOVERABLEERROR = 1;
int get_solution(FILE* fp, FILE* solnFile);
int create_solution();
extern int *lop_to_code;
extern int **lcode;
extern int *ltime_to_code;
/* end */

int lh2level, lreallevel;





int main( int argc, char *argv[] )

{
  /* resulting name for ops file
   */
  char ops_file[MAX_LENGTH] = "";
  /* same for fct file 
   */
  char fct_file[MAX_LENGTH] = "";
  
  gcmd_line.cnflayer = -1;
  gcmd_line.cnfout = -1;
  gcmd_line.solverOut = FALSE;
  gcmd_line.prune = FALSE;
  gcmd_line.binary_clause_only = 0;
  strcpy(gcmd_line.cnfFileName, "CNF");
  gcmd_line.makeCNF = 0;

  /* command line treatment
   */
  if ( argc == 1 || ( argc == 2 && *++argv[0] == '?' ) ) {
    bb_usage();
    exit( 1 );
  }
  if ( !process_command_line( argc, argv ) ) {
    printf("\n\nEXIT: bad arguments detected\n\n");
    bb_usage();    
    exit( 1 );
  }


  /* make file names
   */

  /* one input name missing
   */
  if ( !gcmd_line.ops_file_name || 
       !gcmd_line.fct_file_name ) {
    fprintf(stdout, "\nbb: two input files needed\n\n");
    bb_usage();      
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
  times( &gstart );
  /* domain file (ops)
   */
  if ( gcmd_line.display_info >= 1 ) {
    printf("\nbb: parsing domain file");
  } 
  /* it is important for the pddl language to define the domain before 
   * reading the problem 
   */
  load_ops_file( ops_file );
  /* problem file (facts)
   */  
  if ( gcmd_line.display_info >= 1 ) {
    printf(" ... done.\nbb: parsing problem file"); 
  }
  load_fct_file( fct_file );
  if ( gcmd_line.display_info >= 1 ) {
    printf(" ... done.\n\n");
  }

  /* This is needed to get all types.
   */
  build_orig_constant_list();

  /* last step of parsing: see if it's a STRIPS domain!
   */
  if ( !make_strips_domain() ) {
    printf("\n\nEXIT: bb: this is not a STRIPS problem! can't be handled by this version.\n\n");
    exit( 1 );
  }

  /* now instantiate operators; 
   */

  /* start by collecting all strings and thereby encoding 
   * the domain in integers.
   */
  encode_domain_in_integers();

  /* inertia preprocessing:
   *   - collect inertia information
   *   - split initial state into
   *        - types for unary inertia
   *        - array containing all inertia relations
   *        - array containing non - static relations
   *   - encode unary inertia in op preconds into types
   */
  do_inertia_preprocessing();

  /* unify inertia preconds with initial state,
   * then multiply remaining parameters;
   * create one ActionTemplate for each possible parameter combination
   *
   * --- the actual instantiation part, creating all possible instances ---
   */
  build_action_templates();

  times(&end);
  TIME( gtempl_time );

  times(&start);

  /* perform a simple reachabilty analysis
   * (added facts fixpoint)
   * to find out which facts can (at most) be made true in domain
   */
  perform_reachability_analysis();

  times(&end);
  TIME( greach_time );

  times(&start);

  /* as a last step, collect relevant facts (deleted ini's and
   * added non ini's) and remove others from domain
   */
  collect_relevant_facts();

  times(&end);
  TIME( grelev_time );

  times(&start);

  /* now build globally accessible connectivity graph
   */
  build_connectivity_graph();

  /* insert bit vectors for fast lookup
   *
   * (NOTE: bit vector sizes are static across graph here;
   *  (in difference to IPP) -- otherwise pre-computation 
   *  not possible as done in these parts)
   */
  insert_bit_vectors();
    

  times(&end);
  TIME( gconn_time );

  /* output CNF file */
  times(&start);
  do_cnf_output(gcmd_line.makeCNF);
  times(&end);
  TIME ( gcnf_time );
  
  output_planner_info();

  if ( gcmd_line.makeCNF != 0 ) /* output solution file */
      exit(create_solution());
  exit( SAT );
}











/*
 *  ----------------------------- HELPING FUNCTIONS ----------------------------
 */












void output_planner_info( void )

{
  printf( "\n\ntime spent: %7.2f seconds instantiating %d action templates", 
	  gtempl_time, gnum_templates );
  printf( "\n            %7.2f seconds reachability analysis, yielding %d facts and %d actions", 
	  greach_time, gnum_pp_facts, gnum_actions );
  printf( "\n            %7.2f seconds collecting %d relevant facts", 
	  grelev_time, gnum_relevant_facts );
  printf( "\n            %7.2f seconds building connectivity graph", gconn_time );
  printf( "\n            %7.2f seconds building (std) graph", gbuild_time );
  printf( "\n            %7.2f seconds CNF output time", gcnf_time );
  printf( "\n            %7.2f seconds total planner time (solving not included)\n\n", 
	  gtempl_time + greach_time + grelev_time + gconn_time + gbuild_time + gcnf_time);
}

void output_planner_info_file(FILE* fp, int numSteps) {
    fprintf(fp, "; ParsingTime %7.2f\n", gbuild_time);
    fprintf(fp, "; MakeSpan %d\n", numSteps);
}

void bb_usage( void )

{

  printf("\nusage of bb:\n");

  printf("\nOPTIONS   DESCRIPTIONS\n\n");
  printf("-p <str>    path for operator and fact file\n");
  printf("-o <str>    operator file name\n");
  printf("-f <str>    fact file name\n\n");

  printf("-l  <num>   goal layer for CNF\n\n");

  printf("-G <0 or 1> (0) create CNF output or (1) build final solution\n");
  printf("-b <str>    CNF output file name\n");
  printf("-t <0 or 1> (1) CNF output includes only unary/binary clauses - others ignored\n");
  printf("-S <str>    Input Solution File Name (only when -G 1 is used)\n");
  printf("-F <str>    Final Output Solution File Name (only when -G 1 is used)\n");
  printf("-V <str>    Variables File Name - list all variables (only when -G 1 is used)\n");

  printf("-C          CNF formula output (preset: %d); at layer <-l>\n",
	 gcmd_line.cnfout);
  printf("      0     none\n");
  printf("      1     action-based\n");
  printf("      2     gp-style action-based\n");
  printf("      3     gp-based\n");
  printf("      4     thin gp-based\n\n");




  return;

  printf("-i <num>    run-time information level( preset: 1 )\n");
  printf("      0     only times\n");
  printf("      1     problem name, planning process infos\n");
  printf("    101     cleaned up STRIPS problem\n");
  printf("    102     collected string tables\n");
  printf("    103     encoded domain\n");
  printf("    104     predicates inertia info\n");
  printf("    105     splitted initial state\n");
  printf("    106     extended types table\n");
  printf("    107     all action templates\n");
  printf("    108     inertia cleaned up operator schemata\n");
  printf("    109     reachability analysis results\n");
  printf("    110     final domain representation\n");
  printf("    111     connectivity graph\n\n");
  printf("    112     bits in connectivity graph\n\n");
  
  printf("\n-d <num>    switch on debugging\n\n");

}



Bool process_command_line( int argc, char *argv[] )

{

  char option;


  gcmd_line.display_info = 1;
  gcmd_line.debug = 0;
  gcmd_line.min_time = 0;
  
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
	case 'm':
	  sscanf( *argv, "%d", &gcmd_line.min_time );
	  break;
	case 'l':
	  sscanf( *argv, "%d", &gcmd_line.cnflayer );
	  break;
	case 'C':
	  sscanf( *argv, "%d", &gcmd_line.cnfout );
	  break;
	case 's':
	  gcmd_line.solverOut = TRUE;
	  ++argc;
	  --argv;
	  break;
        case 't':
          sscanf( *argv, "%d", &gcmd_line.binary_clause_only);
          break;
	case 'P':
	  gcmd_line.prune = TRUE;
	  ++argc;
	  --argv;
	  break;
        case 'b':
          strncpy( gcmd_line.cnfFileName, *argv, MAX_LENGTH );
          break;
        case 'G':
          sscanf( *argv, "%d", &gcmd_line.makeCNF );
          break;
        case 'S':
	  strncpy( gcmd_line.input_solution, *argv, MAX_LENGTH );
	  break;
        case 'F':
	  strncpy( gcmd_line.final_solution, *argv, MAX_LENGTH );
	  break;
        case 'V':
	  strncpy( gcmd_line.varFileName, *argv, MAX_LENGTH );
          break;
	default:
	  printf( "\nbb: unknown option: %c entered\n\n", option );
	  return FALSE;
	}
      } else {
	return FALSE;
      }
    }
  }

  if ( gcmd_line.cnfout < 0 || gcmd_line.cnfout > 4 ||
       gcmd_line.cnflayer < 1 ) {
    return FALSE;
  }
       

  return TRUE;

}

/* begin additions by sjn */
int create_solution() {
    FILE* fp;
    FILE* finalSoln;
    int result;

    /* open files for input/output */
    if ( (fp = fopen( gcmd_line.input_solution, "r" )) == NULL ) {
        printf("\n\nEXIT: can't open resulting input solution file.\n\n");
 		exit(1);
    }

    if ( (finalSoln = fopen(gcmd_line.final_solution, "w") ) == NULL ) {
        if ( EOF == fclose(fp) )
            printf("\nExit: unable to close final solution file --> resource leak\n");
        printf("\n\nEXIT: can't open file to write final solution.\n\n");
        exit(1);
    }

    /* write final solution to external file */
    result = get_solution(fp, finalSoln);
    if ( EOF == fclose(fp) ) {
        if ( EOF == fclose(finalSoln) )
            printf("\n\nEXIT: unable to close final solution file --> resource leak\n\n");
        printf("\n\nEXIT: unable to close input solution file --> resource leak\n\n");
        exit(1);
    }

    if ( EOF == fclose(finalSoln) ) {
        printf("\n\nExit: unable to close final solution file --> resource leak\n\n");
        exit(1);
    }
    return(result);
}

typedef struct _PairVals { /* simple structure to help with sorting/pruning final solution */
    int first;
    int second;
    int keep_state; /* if > 1 --> keep in final solution, else throw away */
} PairVals;

void sort_solution(PairVals* pv, int size) {
    const int done = 0;
    int pre = 0, curr = 1, max = -1;
    PairVals tmp;

    if ( curr >= size )
        return;

    /* pv should be almost (if not completely) sorted already --> 
       this algorithm isn't too bad... */
    while ( ! done ) {
        if ( pv[curr].first < pv[pre].first ) { /* swap */
            tmp.first = pv[curr].first;
            tmp.second = pv[curr].second;
            tmp.keep_state = pv[curr].keep_state;
            pv[curr].first = pv[pre].first;
            pv[curr].second = pv[pre].second;
            pv[curr].keep_state = pv[pre].keep_state;
            pv[pre].first = tmp.first;
            pv[pre].second = tmp.second;
            pv[pre].keep_state = tmp.keep_state;
            if ( pv[pre].first < max ) { /* decrement */
                if ( pre != 0 ) {
                    pre -= 1; 
                    curr -= 1;
                    continue;
                }
            }
        }
        else
            max = pv[curr].first;
        if ( ++curr >= size )
            return;
        ++pre;
    } /* while */
}

void removeArr(int* arr, int which, int size) {
    if ( which == size - 1 ) {
        arr[which] = 0;
        return;
    }

    while ( which + 1 < size ) {
        arr[which] = arr[which+1];
        if ( ++which == size - 1 ) 
            arr[which] = 0;
    } /* moving elements to lower positions of arr */    
}

void prune(PairVals* varList, int num_vars) { /* varList has been sorted on plan-levels */
    int i = 0, j = 0, k = 0, m = 0, currentValue = -1, n = 0, p = 0, breakAgain = 0;
    int ok = 1, mark = 0, ft;
    int conditions[10000];

    /* Look at initial facts; only keep actions at level 0 that can result from initial conditions */
    /* This was a hack to get plans using thin-gp-based and gp-based to work; need to find root cause still */
    /* I believe the problem is how no-ops are handled in these other encoding approaches */
    for ( k = 0; k < ginitial_state.num_F; ++k )
        conditions[++currentValue] = ginitial_state.F[k];
    i = -1;
    while ( ++i < num_vars && varList[i].first == 0 ) {
        k = 0;
        for ( n = 0; n < gop_conn[lop_to_code[varList[i].second]].num_P; ++n ) {
            for ( p = 0; p <= currentValue; ++p ) {
                if ( gop_conn[lop_to_code[varList[i].second]].P[n] == conditions[p] ) {
                    ++k;
                    break;
                }
            }
        }
        if ( k != gop_conn[lop_to_code[varList[i].second]].num_P )
            varList[i].keep_state = -1;
     }
    currentValue = -1;

    /* Search forward for steps that are unnecessary at level 0 */
    i = -1;
    while ( ++i < num_vars && varList[i].first == 0 ) {
        if ( varList[i].keep_state < 0 )
            continue;
        k = 0;
        for ( n = 0; n < gop_conn[lop_to_code[varList[i].second]].num_A; ++n ) {	    
            for ( m = 0; m <= currentValue; ++m ) {
	        if ( gop_conn[lop_to_code[varList[i].second]].A[n] == conditions[m] )
		    break; /* nothing new */
            }
            if ( m > currentValue ) {
              k = 1;
	      conditions[++currentValue] = gop_conn[lop_to_code[varList[i].second]].A[n];
            }
        }
        if ( k == 0 )
	  varList[i].keep_state = -1;
    }

    /* Iterate by level; updating conditions as steps are determined necessary; remove unnecessaries */
    i = -1; j = -1;
    while ( ++i < num_vars - 1 ) {
      while ( ++j < num_vars && varList[j].first == varList[i].first ) ;

      mark = j;
      while ( j < num_vars && varList[mark].first == varList[j].first ) {
        if ( varList[j].keep_state < 0 ) {
            ++j;
            continue;
        }
        k = 0;
        for ( n = 0; n < gop_conn[lop_to_code[varList[j].second]].num_A; ++n ) {
	  for ( m = 0; m <= currentValue; ++m ) {
	    if ( gop_conn[lop_to_code[varList[j].second]].A[n] == conditions[m] )
	      break;
          }
          if ( m > currentValue ) {
	    k = 1;
            conditions[++currentValue] = gop_conn[lop_to_code[varList[j].second]].A[n];
          }
        }
        if ( k == 1 ) { /* keep */
	  for ( n = 0; n < gop_conn[lop_to_code[varList[j].second]].num_D; ++n ) {
            for ( m = 0; m <= currentValue; ++m ) {
              if ( gop_conn[lop_to_code[varList[j].second]].D[n] == conditions[m] ) {
                removeArr(conditions, m, currentValue + 1);
                --currentValue;
	        break;
              }
            }
          }
        }
        else
          varList[j].keep_state = -1;
        ++j;
      } /* while */
      i = mark-1;
      --j;
    } /* while */

    currentValue = -1;
    i = 0; j = 0; m = 0; n = 0;
    
    /* load up all goal conditions */
    for ( i = 0; i < ggoal_state.num_F; i++ )
        conditions[++currentValue] = ggoal_state.F[i];

    /* mark ops in final layer which contribute to the goals */
    i = 0; j = 0;
    for ( i = num_vars - 1; i >= 0; --i ) {
        if ( varList[i].first != varList[num_vars - 1].first )
            break;
        else if ( varList[i].keep_state < 0 )
	    continue;

        for ( m = 0; m <= currentValue; ++m ) {
            for ( n = 0; n < gop_conn[lop_to_code[varList[i].second]].num_A; ++n ) {
                if ( gop_conn[lop_to_code[varList[i].second]].A[n] == conditions[m] ) {
                    /* match found */
                    varList[i].keep_state = 2;
                    removeArr(conditions, m, currentValue + 1);
                    --currentValue;
                    m -= 1;
                    break;
                }
            } /* for */
        } /* for */
    } /* for */

    /* collect union (unique set) of all preconditions at plan level i that truly contribute to plan */
    for ( i = num_vars - 1; i >= 0; ) {
        j = i;
        while ( (--j >= 0) && (varList[j].first == varList[i].first) ) ;


        while ( i > j ) { 
            if ( varList[i].keep_state > 1 ) { /* more preconditions to add */
                for ( k = 0; k < gop_conn[lop_to_code[varList[i].second]].num_P; ++k ) {
                    for ( p = 0; p <= currentValue; ++p ) {
                        if ( gop_conn[lop_to_code[varList[i].second]].P[k] == conditions[p] ) {
                            ok = 0; /* do not add --> already in the current list */
                            break;
                        }
                    }
                    if ( ok )
                        conditions[++currentValue] = gop_conn[lop_to_code[varList[i].second]].P[k];                    
                    ok = 1;
                }                    
            }
            --i;
        } /* while */
        if ( j >= 0 ) {
            while ( (--j >= 0) && (varList[j].first == varList[i].first) ) ;
        }

        /* look at preceding plan level and see which actions stay and which must go */
        for ( k = j + 1; k <= i && (k > -1); ++k ) {
            if ( varList[k].keep_state < 0 )
                continue;
            varList[k].keep_state = 0;
            breakAgain = 0;
            for ( m = 0; m <= currentValue; ++m ) {
                for ( n = 0; n < gop_conn[lop_to_code[varList[k].second]].num_A; ++n ) {
                    if ( gop_conn[lop_to_code[varList[k].second]].A[n] == conditions[m] ) {
                        /* match found */
                        varList[k].keep_state = 2;
                        removeArr(conditions, m, currentValue + 1);
                        if ( --currentValue < 0 ) breakAgain = 1;
                        m -= 1;
                        break;
                    }
                } /* for */
                if ( breakAgain ) break;
            } /* for */
            if ( breakAgain ) break;
        } /* for */
    } /* for */


    /* look at initial state --> finish off all necessary conditions */
    for ( m = 0; m <= currentValue; ++m ) {
        for ( k = 0; k < ginitial_state.num_F; ++k ) {
            ft = ginitial_state.F[k];
            if ( ft == conditions[m] ) {
                removeArr(conditions, m, currentValue + 1);
                if ( --currentValue < 0 )
                    return;
                m -= 1;
                break;
            }
        }
    }

    /* Ensure all preconditions of all ops kept in final solution have been satisfied */
    if ( currentValue != -1 ) {
        printf("\n\nEXIT: prune() left some preconditions open.. %d preconditions left over\n\n", ++currentValue);
        printf("NOT A SOLUTION\n\n");
        exit(1);
     }
}

int get_solution(FILE* fp, FILE* solnFile) {
    /* locals */
    long loc = 1, markEnd = -1, beg = 0;
    char c;
    long done = 0, value = 0, idx = -1, first = 1;
    char toConvert[20];
    int i = 0, j = 0;
    int counter = 0;
    int maxCounter = MAX_CNF_VARS / 2 + 1;
    int numSteps = -1;
    static PairVals toOrder[MAX_CNF_VARS / 2 + 1];    

    /* get end of file position */
    fseek(fp, beg, SEEK_END);
    loc = ftell(fp);

    while ( loc > beg ) {
        fseek(fp, loc, SEEK_SET);
        c = fgetc(fp);
        if ( isdigit(c) ) {
            markEnd = loc + 1;
            break;
        }
        --loc;
    }
    if ( loc <= beg ) {
        printf("\n\nEXIT: Improper solution format.\n\n");
        fclose(fp);
        fclose(solnFile);
        exit(1);
    }

    while ( beg < markEnd ) {        
        fseek(fp, beg, SEEK_SET);
        c = fgetc(fp);
        if ( isdigit(c) || (c == '-') ) /* # or -sign of # */
            break;
        ++beg;  
    }
    if ( beg >= markEnd ) {
        printf("\n\nEXIT: Improper solution format.\n\n");
        fclose(fp);
        fclose(solnFile);
        exit(1);
    }

    /* grab solution to CNF sentence */
    fseek(fp, beg, SEEK_SET);
    while ( beg <= markEnd ) {
        c = fgetc(fp);
        switch(c) {
            case ' ': case '\n':
                if ( first ) { /* just front garbage */
                    fseek(fp, ++beg, SEEK_SET);
                    continue;
                }
                if ( ! done ) {
                    toConvert[++idx] = '\0';
                    value = atol(toConvert);
                    if ( value > 0 && (gop_conn[lop_to_code[value]].noop_for == -1) ) {
                        /* found part of final solution */
                        toOrder[counter].first = ltime_to_code[value]; /* sort on this */
                        toOrder[counter].second = value;
		        toOrder[counter].keep_state = 0;
                        if ( ltime_to_code[value] > numSteps )
                            numSteps = ltime_to_code[value];
                        if ( ++counter > maxCounter ) {
                            printf("\n\nEXIT: Too many variables asserted????\n\n");
                            fclose(fp);
                            fclose(solnFile);
                            exit(1);
                        }     
                    } /* if */
                    idx = -1;
                    done = 1;
                } /* if */
                break;
            default:
                if ( ++idx > 19 ) {
                    printf("\n\nEXIT: wow, that's a big number - too big\n\n");
                    fclose(fp);
                    fclose(solnFile);
                    exit(1);
                }
                toConvert[idx] = c;  
                done = 0;
                first = 0;
        }; /* switch */
        fseek(fp, ++beg, SEEK_SET);
    } /* while */

    /* sort final solution */
    sort_solution(toOrder, counter);

    /* prune away actions that don't contribute */
   prune(toOrder, counter); 

    /* send final solution to file; print out any pruned steps */
    output_planner_info_file(solnFile, ++numSteps);
    for ( j = 0; j < counter; ++j ) {
      if (toOrder[j].keep_state > 1) {
        value = toOrder[j].second;
        fprintf(solnFile, "%d: (", ltime_to_code[value]);
        if ( goperators[gop_conn[lop_to_code[value]].op]->name )
	        fprintf(solnFile, "%s", goperators[gop_conn[lop_to_code[value]].op]->name);                    	
        for ( i=0; i<goperators[gop_conn[lop_to_code[value]].op]->num_vars; i++ )
		    fprintf(solnFile, " %s", gconstants[gop_conn[lop_to_code[value]].inst_table[i]]);
        fprintf(solnFile, ") [1]\n"); 
      } else {
	    printf("Pruned:  ");
        value = toOrder[j].second;
        printf("%d: (", ltime_to_code[value]);
        if ( goperators[gop_conn[lop_to_code[value]].op]->name )
	      printf("%s", goperators[gop_conn[lop_to_code[value]].op]->name);                    	
        for ( i=0; i<goperators[gop_conn[lop_to_code[value]].op]->num_vars; i++ )
	      printf(" %s", gconstants[gop_conn[lop_to_code[value]].inst_table[i]]);
        printf(") [1]\n"); 
      }
    }
    return(SAT);
}
/* end additions by sjn */
