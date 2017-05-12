

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
 * File: ff.h
 * Description: Types and structures for the FastForward planner.
 *
 *        --------- STRIPS  VERSION  v 1.0 --------------
 *
 * Author: Joerg Hoffmann 1999
 * Contact: hoffmann@informatik.uni-freiburg.de
 *
 *********************************************************************/ 








#ifndef __FF_H
#define __FF_H

#include <stdlib.h>
#include <stdio.h>
#include <strings.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/timeb.h>
#include <sys/times.h>









/*
 *  ------------------------------------ DEFINES ----------------------------
 */






#define MAX_TIME 500







/***********************
 * MEANINGLESS HELPERS *
 ***********************/




/* strcmp returns 0 if two strings are equal, which is not nice */
#define SAME 0









/****************
 * PARSING ETC. *
 ****************/









/* various defines used in parsing
 */
#define EQ_STR "EQ"
#define HIDDEN_STR "#"
#define AXIOM_STR "AXIOM"
#define NAME_STR "name\0"
#define VARIABLE_STR "variable\0"
#define STANDARD_TYPE "OBJECT\0"
#define EITHER_STR "EITHER"








/***************************
 * SOME ARBITRARY SETTINGS *
 ***************************/











/* maximal string length
 */
#define MAX_LENGTH 256 


/* marks border between connected items 
 */
#define CONNECTOR "~"


/* std size of helping arrays
 */
#define ARRAY_SIZE 10000


/* first size of goals_at array in 1P extraction
 */
#define RELAXED_STEPS_DEFAULT 10







/************************
 * INSTANTIATION LIMITS *
 ************************/






#define MAX_CONSTANTS 2000
#define MAX_PREDICATES 2400
#define MAX_TYPES 50
#define MAX_ARITY 5
#define MAX_VARS 15


#define MAX_TYPE 2000


#define MAX_INITIAL 5000


#define MAX_OPERATORS 12800


#define MAX_TYPE_INTERSECTIONS 10


#define MAX_RELEVANT_FACTS 10000/* i think this is VERY generous... */









/******************************************
 * DOMAIN STRUCTURE AND SEARCHING LIMITS *
 ******************************************/







#define MAX_OP_P 80
#define MAX_OP_A 40
#define MAX_OP_D 320





#define MAX_PLAN_LENGTH 2000


#define MAX_STATE 6400


#define MAX_SPACE 150000


#define STATE_HASH_SIZE 4096
#define STATE_HASH_BITS 4095







#define MAX_GRAPH 100

#define MAX_DECVARS 1000



/* dec D atabases might be big... only in BD analysis...
 */
#define MAX_D_DECVARS 10000












/****************
 * CODE DEFINES *
 ****************/








/* not a real 'code' define; used in relax and search to encode
 * infinite level number / plan length
 */
#ifndef INFINITY
#define INFINITY -1
#endif








/* define boolean types if not allready defined
 */
#ifndef Bool
typedef unsigned char Bool;
#ifndef TRUE /* we assume that FALSE is also not defined */
#define TRUE 1
#define FALSE 0
#endif /* TRUE */
#endif /* Bool */


/* code a param number into a negative number and vice versa
 */
#define ENCODE_VAR( val ) (val * (-1)) - 1
#define DECODE_VAR( val ) (val + 1) * (-1)

#define GET_CONSTANT( val, pointer ) ( val >= 0 ) ? val : pointer->inst_table[DECODE_VAR( val )]


/* Check allocated memory
 */
#define CHECK_PTR(p) if (NULL == (p)) { \
  fprintf(stdout, "\n\aNO MEMORY in file %s:%d\n\n", __FILE__, __LINE__); \
  printf("\nEXIT: Out of memory (Check_PTR)\n"); \
  exit(1);}


/* add elapsed time from main local time vars to specified val
 */
#define TIME( val ) val += ( float ) ( ( end.tms_utime - start.tms_utime + \
					 end.tms_stime - start.tms_stime  ) / 100.0 )


#define TIMECHECK { times( &gend ); if ( gend.tms_utime - gstart.tms_utime + gend.tms_stime - gstart.tms_stime > MAX_TIME * 100 )   { times(&end); TIME( gsearch_time ); output_planner_info(); }}









/*
 *  ------------------------------ DATA STRUCTURES ----------------------------
 */











/*******************
 * GENERAL HELPERS *
 *******************/








/* all command switches
 */
struct _command_line {

  char path[MAX_LENGTH];
  char ops_file_name[MAX_LENGTH];
  char fct_file_name[MAX_LENGTH];
  char rslt_file_name[MAX_LENGTH];
  char cnfFileName[MAX_LENGTH];
  char input_solution[MAX_LENGTH];
  char final_solution[MAX_LENGTH];
  char varFileName[MAX_LENGTH];  

  int makeCNF;
  int display_info;

  int cnfout;
  int cnflayer;
    
  int min_time;

  int debug;

  int solverOut;
  int prune;
  int timeOut;
  int binary_clause_only;
};


typedef char *Token;












/***********
 * PARSING *
 ***********/










/* A list of strings
 */
typedef struct _TokenList {

  char *item;
  struct _TokenList *next;

} TokenList;



/* list of string lists
 */
typedef struct _FactList {

  TokenList *item;
  struct _FactList *next;

} FactList;



/* This type indicates whether a node in the pddl tree stands for
 * an atomic expression, a junctor or a quantor. 
 */
typedef enum _Connective{ATOM, 
			 NOT, 
			 AND, 
			 OR, 
			 ALL, 
			 EX, 
			 WHEN} Connective;



/*
 * This is a node in the tree to parse PDDL files
 */
typedef struct _PlNode {

  /* type of the node
   */
  Connective connective;

  /* AND, OR, NOT, WHEN => NULL
   * ALL, EX            => the quantified variable with its type
   * ATOM               => the atom as predicate->param1->param2->...
   */
  TokenList *atom;

  /* (a) for AND, OR this is the list of sons(a AND b AND c...),
   * (b) for the rest this is the son, e.g. a subtree that is negated
   * (c) for WHEN, the first son is the condition and the next son
   * is the effect
   */
  struct _PlNode *sons;

  /* if you have a list of sons, they are connected by next
   */
  struct _PlNode *next;

} PlNode;


/*
 * This resembles an uninstantiated PDDL operator
 */
typedef struct _PlOperator {

  char *name;

  /* params is a list of variable/type pairs, such that:
   * factlist->item = [variable] -> [type]
   */
  FactList *params;
  PlNode *preconds;
  PlNode *effects;

  /* only important for PDDL where :VARS may be added to the param list
   * which must be hidden when writing the plan to an output file
   */
  int number_of_real_params; 

  struct _PlOperator *next;

} PlOperator;


/* the type_tree structure is used to deal with types and subclasses
 *  of types
 */
typedef struct TYPETREE_LIST *type_tree_list, type_tree_list_elt;

typedef struct TYPETREE {
  
  char *name;  /* an object type */
  type_tree_list sub_types;

} *type_tree, type_tree_elt;

struct TYPETREE_LIST {

  type_tree item;
  struct TYPETREE_LIST *next;

};









/***************** 
 * INSTANTIATION *
 *****************/











typedef int TypeArray[MAX_TYPE_INTERSECTIONS];

typedef int *int_pointer;







/* a partially instantiated fact: predicate and args
 */
typedef struct _Fact {

  int predicate, args[MAX_ARITY];

} Fact;



/* a partially instantiated operator
 */
typedef struct _Operator {

  char *name, *var_names[MAX_VARS];
  int num_vars, var_types[MAX_VARS], inst_table[MAX_VARS];
  int number_of_real_params; 

  Fact preconds[MAX_OP_P];
  int num_preconds;
  Fact adds[MAX_OP_A];
  int num_adds;
  Fact dels[MAX_OP_D];
  int num_dels;

  Bool out;

} Operator, *Operator_pointer;



/* minimal info for a fully instantiated operator;
 * yields one action when expanded
 */
typedef struct _ActionTemplate {

  int op;
  int inst_table[MAX_VARS];

  struct _ActionTemplate *next;

} ActionTemplate;



/* an instantiated operator
 */
typedef struct _Action {

  int op;
  int inst_table[MAX_VARS];

  int preconds[MAX_OP_P];
  int num_preconds;
  int adds[MAX_OP_A];
  int num_adds;
  int dels[MAX_OP_D];
  int num_dels;

  struct _Action *next;

} Action;
  













/*****************************************************
 * BASIC OP AND FT STRUCTURES FOR CONNECTIVITY GRAPH *
 *****************************************************/









typedef unsigned int BitVector, *BitVector_pointer;

typedef struct _OpLevelInfo OpLevelInfo, *OpLevelInfo_pointer;
typedef struct _FtLevelInfo FtLevelInfo, *FtLevelInfo_pointer;

typedef struct _IntList IntList;
typedef struct _IntPair IntPair;





typedef struct _OpConn {

  /* to get name
   */
  int op;
  int inst_table[MAX_VARS];

  /* noops are included just like any other op
   * (hope that this makes life easier later on...)
   */
  int noop_for;

  /* for bit vectors
   */
  int uid_block;
  unsigned int uid_mask;

  /* general connectivity info
   */
  int *P;
  BitVector *bit_P;/* use fast lookup of precs */
  int num_P;
  int *A;
  int num_A;
  int *D;
  int num_D;
  /* no vectors for A and D: only used for interference, which we
   * encode once and directly instead
   */

  /* interference *NOT* statically computed: too many actions
   * so keeping 2-dim table is too much memory even if one dim
   * is represented as bit vectors.
   *
   * instead, we keep bit vectors of adds and dels in order to
   * find interference quickly.
   */
  BitVector *bit_A;
  BitVector *bit_D;

  /* the graph info
   */
  int first_appearance;/* this does not get updated (dynamically, ops can vanish and reappear) */
  OpLevelInfo_pointer info_at[MAX_GRAPH];

} OpConn;



typedef struct _FtConn {

  /* for bit vectors
   */
  int uid_block;
  unsigned int uid_mask;

  /* general connectivity info
   */
  int *P;
  int num_P;
  int *A;
  int num_A;
  int *D;
  int num_D;

  /* for hashing
   */
  int rand;

  /* the graph info
   */
  int first_appearance;/* this does not get updated (dynamically, fts can vanish and reappear) */
  FtLevelInfo_pointer info_at[MAX_GRAPH+1];

  /* is it, or not?
   */
  Bool is_goal;

} FtConn;



struct _OpLevelInfo {

  /* here we still got an ops X ops element, but it's only
   * the nr. actually in a graph level;
   *
   * need it for fast (?) STAN-style computation of
   * fact mutex: OR together the exlusives of all adders of one fact,
   * check against adders string of other fact.
   *
   * QUESTION: what is better, keeping many potentially long vectors,
   * and performing one sweep (approx) per ft excl check, or not storing
   * all these vectors but performing an add list X add list check for every
   * ft excl check????
   */
  BitVector *bit_exclusives;
  /* facts that are exclusive of preconds; very helpful in backwards search;
   * HERE TOO??????
   */
  BitVector *bit_P_exclusives;

  /* for dynamic states
   *
   * -2 -> out (implied by other constraints)
   * -1 -> forced out
   *  0 -> neutral
   *  1 -> forced in
   */
  int status;

  /* search infos...
   *
   * is it in the relaxed plan at this time?
   */
  Bool is_in_rplan;

  float losspos, lossneg;

  float rplan_frac;
  Bool forced_in;

};



struct _FtLevelInfo {

  /* much of the adding info is due to extensive need in backward search
   * HOW'S ABOUT HERE?
   */
  IntList *A;
  /* technical, to facilitate controlling the ordering of adders
   */
  IntList *end_A;

  BitVector *bit_A;
  /* exclusives (ops) of adders
   */
  BitVector *bit_A_exclusives;

  /* keep this for fast knowing which actions use the fact as P at this layer, 
   * during constraint propagation.
   */
  IntList *P;

  /* exclusives (fts) of this fact
   */
  BitVector *bit_exclusives;

  /* for dynamic states
   *
   * -2 -> out (implied by constraints)
   * 0 -> in, i.e. as normal...
   */
  int status;

  /* search infos...
   *
   * is it a goal in the relaxed plan?
   */
  Bool is_goal;
  /* the number of adders that are currently in
   *
   * NOTE: is only computed during rplan extraction.
   *       NOT available at dynamic runtime.
   */
  int num_A;
  
  float rplan_frac;

};



/* for lists of current ops and fts
 */
struct _IntList {

  int i1;

  /* doubly connected to make removal easier
   */
  struct _IntList *prev;
  struct _IntList *next;

};



/* for lists of exclusion relations op/op resp. ft/ft --> speedup
 */
struct _IntPair {

  int i1, i2;

  /* doubly connected to make removal easier
   */
  struct _IntPair *prev;
  struct _IntPair *next;

};



/* this is real ugly, I'm aware of that; need it to remember,
 * during constraint propagation, the bit_A_exclusives vectors
 * that have changed at a level -- for undoing later on
 * 
 * appears more efficient than recomputing this information (which
 * would also be possible) when going back in the search tree
 */
typedef struct _IntBitVectorList {

  int i1;
  BitVector *bv;

  struct _IntBitVectorList *next;

} IntBitVectorList;















/****************************
 * STRUCTURES FOR SEARCHING *
 ****************************/









typedef struct _State {
  
  int F[MAX_STATE];
  int num_F;

} State;


typedef struct _SearchNode {
  
  State S;
  int op;
  int father;
  int depth;

} SearchNode;



typedef struct _StateHashEntry {

  State S;
  int sum;

  struct _StateHashEntry *next;

} StateHashEntry, *StateHashEntry_pointer;



/* for backdoors: to hold the decvars we are looking at.
 */
typedef struct _Decvar {
  
    /* the dec vars here are ops at layers
     */
    int op;
    int time;

    /* they might be in (1) or out (0);
     * returned from BBG with the solution decvars; else,
     * to indicate both values tried for unsat, it is -1.
     */
    int value;

} Decvar;
















/*
 *  -------------------------------- MAIN FN HEADERS ----------------------------
 */












void output_planner_info( void );
void bb_usage( void );
Bool process_command_line( int argc, char *argv[] );









/*
 *  ----------------------------- GLOBAL VARIABLES ----------------------------
 */












/*******************
 * GENERAL HELPERS *
 *******************/










/* used to time the different stages of the planner
 */
extern float gtempl_time, greach_time, grelev_time, gconn_time;
extern float gbuild_time, gsearch_time;

/* the command line inputs
 */
extern struct _command_line gcmd_line;

/* number of states that got heuristically evaluated
 */
extern int gevaluated_states;

/* info on search spaces structure
 */
extern int gmax_search_depth;
extern int gmax_search_size;








/***********
 * PARSING *
 ***********/











/* used for pddl parsing, flex only allows global variables
 */
extern int gbracket_count;
extern char *gproblem_name;

/* The current input line number
 */
extern int lineno;

/* The current input filename
 */
extern char *gact_filename;

/* The pddl domain name
 */
extern char *gdomain_name;

/* loaded, uninstantiated operators
 */
extern PlOperator *gloaded_ops;

/* stores initials as fact_list 
 */
extern PlNode *gorig_initial_facts;

/* not yet preprocessed goal facts
 */
extern PlNode *gorig_goal_facts;

/* to store all typed objects 
 */
extern FactList *gorig_constant_list;

/* type hierarchy (PDDL) 
 */
extern type_tree_list gglobal_type_tree_list;

/* helper for types parsing
 */
extern FactList *gtypes;

/* the predicates and their types as defined in the domain file
 */
extern FactList *gpredicates_and_types;










/*****************
 * INSTANTIATING *
 *****************/










/* global arrays of constant names,
 *               type names (with their constants),
 *               predicate names,
 *               predicate aritys,
 *               defined types of predicate args
 */
extern Token gconstants[MAX_CONSTANTS];
extern int gnum_constants;
extern Token gtype_names[MAX_TYPES];
extern int gtype_consts[MAX_TYPES][MAX_TYPE];
extern Bool gis_member[MAX_CONSTANTS][MAX_TYPES];
extern int gtype_size[MAX_TYPES];
extern int gnum_types;
extern Token gpredicates[MAX_PREDICATES];
extern int garity[MAX_PREDICATES];
extern int gpredicates_args_type[MAX_PREDICATES][MAX_ARITY];
extern int gnum_predicates;




/* the domain in integer (Fact) representation
 */
extern Operator_pointer goperators[MAX_OPERATORS];
extern int gnum_operators;
extern Fact gfull_initial[MAX_INITIAL];
extern int gnum_full_initial;
extern Fact ggoal[MAX_STATE];
extern int gnum_goal;





/* stores inertia - information: is any occurence of the predicate
 * added / deleted in the uninstantiated ops ?
 */
extern Bool gis_added[MAX_PREDICATES];
extern Bool gis_deleted[MAX_PREDICATES];

/* splitted initial state:
 * initial non static facts,
 * initial inertia facts of arity > 1
 */
extern Fact ginitial[MAX_STATE];
extern int gnum_initial;
extern Fact ginertia[MAX_INITIAL];
extern int gnum_inertia;

/* the type numbers corresponding to any unary inertia
 */
extern int gtype_to_predicate[MAX_PREDICATES];
extern int gpredicate_to_type[MAX_TYPES];

/* (ordered) numbers of types that new type is intersection of
 */
extern TypeArray gintersected_types[MAX_TYPES];
extern int gnum_intersected_types[MAX_TYPES];






/* intermediate step: store minimal action info, use it
 * to determine relevant facts, number of actions and final form
 * before creating actual actions
 */
extern ActionTemplate *gtemplates;
extern int gnum_templates;





/* store the final "relevant facts"
 */
extern Fact grelevant_facts[MAX_RELEVANT_FACTS];
extern int gnum_relevant_facts;
extern int gnum_pp_facts;

/* the (fully instantiated) domain in integer 
 * (number of relevant Fact) representation
 */
extern Action *gactions;
extern int gnum_actions;
extern State ginitial_state;
extern State ggoal_state;










/**********************
 * CONNECTIVITY GRAPH *
 **********************/






/* one fact array, ..
 */
extern FtConn *gft_conn;
extern int gnum_ft_conn;

/* one ops (actions) array
 */
extern OpConn *gop_conn;
extern int gnum_op_conn;

/* for bit vector handling
 */
extern int gcword_size; 
extern int gnum_ft_bit;
/* grows with graph to avoid memory problems
 * in the presence of many actions
 */
extern int gnum_op_bit_at[MAX_GRAPH];
extern int gnum_ops, gmax_block;

/* the goals in bit-format...
 */
extern BitVector *gbit_goal_state;








/* some technical graph... things
 */





/* the ops not yet in the graph
 */
extern IntList *gout_ops;

/* global op-state resp. ft-state lists
 */ 
extern IntList *gin_ops;
extern IntList *gin_prev_ops;
extern IntList *gin_fts;
extern IntList *gin_prev_fts;

/* we want to have fast access, during constraint propagation,
 * to the fts that can (maximally) be present at the layer;
 * can easily store this by means of pointers into the gin_fts list
 * which we got anyway.
 */
extern IntList *gin_fts_at[MAX_GRAPH];
/* the present ops are good to have for enforcing a positive constraint:
 * we must then negatively enforce the exclusive ops. don't wanna store
 * all the exclusives in a list but at least we can skip those ops that
 * aren't even there. means as above.
 */
extern IntList *gin_ops_at[MAX_GRAPH];

/* current mutexes: exclusives speedup
 */
extern IntPair *gin_op_mutex_pairs;
extern IntPair *gin_ft_mutex_pairs;

/* information about current state of graph, needed for level off test
 */
extern int gin_ft_count, gin_ft_exclusion_count;
extern int gin_op_count, gin_op_exclusion_count;

/* is TRUE iff graph has levelled off.
 */
extern Bool gsame_as_prev_flag;

/* stores the time step at which graph has levelled off.
 */
extern int gfirst_full_time;









/* some search things
 */








/* flag saying that depth first has encountered a solution!
 */
extern Bool gfound_plan;



/* number of action constraints enforced successfully
 */
extern int gconstr_succ;
/* same, unsuccessfully
 */
extern int gconstr_fail;



/* search info: the currently positively enforced ops at each time
 *
 * (to be included into relaxed plan)
 */
extern int **genforced_ops_at;
extern int *gnum_enforced_ops_at;




/* during PG re-computation: forcing an action A in might
 * force actions below in, too - namely, if they're the
 * only achievers of a prec of A. remember these actions and
 * force them in once propagation for A has terminated.
 */
extern Decvar gto_be_enforced[ARRAY_SIZE];
extern int gnum_to_be_enforced;


/* some statistics on transitively forced in - ops
 * names refer to paper work on unit propag == rplan propag
 */
extern int gnum_bins;
extern int gnum_d2ins;




extern struct tms gstart, gend;



extern struct tms start, end;






/* the dec vars sufficing for BBG search at the respective layers!
 * 
 * in case of solution: dec vars below that sol.
 * in case of unsat:  all dec vars expanded during search (?)
 */
extern Decvar gbbg_decvars[MAX_GRAPH][MAX_DECVARS];
extern int gnum_bbg_decvars[MAX_GRAPH];
extern int gnum_bbg_decs;



/* this is what bd verification works on.
 */
extern Decvar gbd_candidate[MAX_DECVARS];
extern int gnum_bd_candidate;


/* technical, for backdoor verification.
 */
extern Bool gsolution_found;
extern Bool girresolved_found;
extern int gbd_candidate_curr;
extern Bool gcontrol_strongbd;



/* rplan now also needed in backdoors.
 */
extern int **grelaxed_ops_at;
extern int *gnum_relaxed_ops_at;



/* the decvar Database to choose from, for 
 * enumeration
 */
extern Decvar gD[MAX_D_DECVARS];
extern int gnum_D;



#endif
