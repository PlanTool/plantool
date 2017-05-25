



/*********************************************************************
 * File: ipp.h
 * Description: Types and structures for the IPP planner.
 *
 * Author: Joerg Hoffmann / Frank Rittinger / Andreas Schoen
 * Contact: hoffmann@informatik.uni-freiburg.de
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






#ifndef __IPP_H
#define __IPP_H

#include <stdlib.h>
#include <stdio.h>
#include <strings.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/times.h>









/*
 *  ------------------------------------ DEFINES ----------------------------
 */











/***********************
 * MEANINGLESS HELPERS *
 ***********************/





/* if this is defined, memory consumption gets recorded
 */
#define MEMORY_INFO


/* fprintf s are parametrized with OUT 
 */
#define ERR stderr
#define OUT stdout


/* 
 * The following constants are for exit_codes for shellscripts
 */
#ifndef ERROR_CODES
#define ERROR_CODES
#define EXIT_FAILURE              1 /* for not yet classified errors */
#define NOT_SUPPORTED_ERROR_CODE  2
#define BADDOMAIN_ERROR_CODE      3
#define FCT_PARSE_ERROR_CODE      4
#define FCT_MISSING_ERROR_CODE    5
#define OPS_PARSE_ERROR_CODE      6
#define OPS_MISSING_ERROR_CODE    7
#define PARSE_ERROR_CODE          8
#define USAGE_ERROR_CODE          9
#define OTHER_ERROR_CODE         10 
#define INTERNAL_ERROR_CODE      11 /* these errors 
                                    should never happen finally */
#define TRAFO_INTERNAL_ERROR_CODE 12

#endif


/* strcmp returns 0 if two strings are equal, which is not nice */
#define SAME 0







/****************
 * PARSING ETC. *
 ****************/







/* type of quantifier, used for parsing */
#define NO_QUANT '-'
#define ALL_QUANT 'A'
#define EX_QUANT 'E'
#define ANY_PRED 0
#define EQ 1
#define EQ_STR "eq"
#define NOT_EQ 2
#define NOT_PRED '!'
#define NOT_EQ_PRED "!eq"
#define NO_SOLUTION "NO SOLUTION\n"

/* 
 * The following constants are for pl1 expression preprocessing.
 */
#define LIT_CONST      1
#define AND_CONST      2
#define OR_CONST       3
#define IMPLY_CONST    4
#define NOT_CONST      5
#define EXISTS_CONST   6 
#define FORALL_CONST   7
#define LBRACK_CONST   8
#define RBRACK_CONST   9
#define VAR_CONST     10
#define ENDNOT_CONST  11
#define HIDDEN_STR "#"
#define AXIOM_STR "AXIOM"
#define NAME_STR "name\0"
#define VARIABLE_STR "variable\0"
#define STANDARD_TYPE "OBJECT\0"
#define GOAL_OP_STR "#REACHGOAL"
#define GOAL_REACHED "#GOALREACHED"
#define EITHER_STR "EITHER"








/***************************
 * SOME ARBITRARY SETTINGS *
 ***************************/








/* maximal string length */
#define MAX_LENGTH 256 


/* marks border between connected items */
#define CONNECTOR "~"


/* der blanke wahnsinn */
#define NOOP "noop"






/************************
 * INSTANTIATION LIMITS *
 ************************/






#define MAX_CONSTANTS_TABLE 5000
#define MAX_PREDICATES_TABLE 10000
#define MAX_TYPES_TABLE 200
#define MAX_ARITY 6
#define MAX_VARS 15

#define MAX_RELEVANT_FACTS 10000/* i think this is VERY generous... */







/******************************
 * GRAPH AND SEARCHING LIMITS *
 ******************************/






#define MAX_PLAN 300
#define ARRAY_SIZE 200
#define MEMO_HASHSIZE 4
#define MEMO_HASH 3








/****************
 * CODE DEFINES *
 ****************/






/* define boolean types if not allready defined
 */
#ifndef Bool
typedef unsigned char Bool;
#ifndef TRUE /* we assume that FALSE is also not defined */
#define TRUE 1
#define FALSE 0
#endif /* TRUE */
#endif /* Bool */



/* Check allocated memory
 */
#define CHECK_PTR(p) if (NULL == (p)) { \
  fprintf(stdout, "\n\aNO MEMORY in file %s:%d\n\n", __FILE__, __LINE__); \
  exit(1);}


/* add elapsed time from main local time vars to specified val
 */
#define TIME( val ) val += ( float ) ( ( end.tms_utime - start.tms_utime + \
					 end.tms_stime - start.tms_stime  ) / 100.0 )


/* compute the adress of negative fact in fact array
 */
#define NEG_ADR( index ) gnum_relevant_facts + index











/*
 *  ------------------------------ DATA STRUCTURES ----------------------------
 */











/*******************
 * GENERAL HELPERS *
 *******************/







/* This holds all command line switches
 */
struct _command_line {

  char path[MAX_LENGTH];
  char ops_file_name[MAX_LENGTH];
  char fct_file_name[MAX_LENGTH];
  
  int display_info;
  
  Bool write_graph;
  char *save_name;

  Bool do_subset;
  int min_time;

  /* for various debugging informations
   */
  int debug;

};



/* different reasons for program termination 
 */
typedef enum {MEMORY, SYNTAX_ERROR , IO_ERROR} TermReason;


typedef unsigned int BitVector, *BitVector_pointer;


typedef struct _Integers {

  int index;
  struct _Integers *next;

} Integers;


typedef char *String;


typedef struct _StringIntegers {

  char *name;
  Integers *integers;

} StringIntegers;


typedef int *int_pointer;


typedef int IntArray[ARRAY_SIZE];







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
			 WHEN, 
			 TRU, 
			 FAL,
			 EMPTY,
			 DUMMY,
			 NEUTRAL} Connective;



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

  TokenList *name;
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








typedef int ArgArray[MAX_ARITY];


typedef struct _CodeNode {

  /* type of node
   */
  Connective connective;

  /* in quantifier nodes: number of var (in operator), type of var
   */ 
  short int var, var_type;
  /* in atoms: number of predicate (-1 is EQ)
   * number of arguments (negative: num of var-1, positive: num of object)
   */
  short int predicate;
  ArgArray arguments;

  /* speedup: in atomic inertia test, we need only check for TRU/FAL
   * properties if we have yet never visited that node or if we actually
   * change the instantiation of one of the variables in the atom
   */ 
  Bool visited;

  struct _CodeNode *sons;
  struct _CodeNode *next;

} CodeNode, *CodeNode_pointer;


typedef struct _CodeOperator {

  char *name;

  short int var_types[MAX_VARS], num_vars, inst_table[MAX_VARS];

  CodeNode *preconds;
  CodeNode *conditionals;

  /* only important for PDDL where :VARS may be added to the param list
   * which must be hidden when writing the plan to an output file
   */
  int number_of_real_params; 

  struct _CodeOperator *next;

} CodeOperator;



/* actually, this structure is not needed.
 * just for debugging and info:
 * stores (in grelevant_facts) the relevant fact to it's index
 */
typedef struct _Relevant_Fact {

  short int predicate;
  ArgArray arguments;

} RelevantFact, *RelevantFact_pointer;








/****************************
 * BITVECTOR REPRESENTATION *
 ****************************/









typedef struct _FactInfo {

  BitVector *vector;
  Integers *indices;

} FactInfo;


typedef struct _FactInfoPair {

  FactInfo *positive;
  FactInfo *negative;

} FactInfoPair;


typedef struct _Effect {

  FactInfo *p_conds;
  FactInfo *n_conds;
  FactInfo *p_effects;
  FactInfo *n_effects;
  struct _Effect *next;

} Effect;


typedef struct _BitOperator {

  char *name;
  short int num_vars, inst_table[MAX_VARS];

  FactInfo *p_preconds;
  FactInfo *n_preconds;
  Effect *unconditional;
  Effect *conditionals;

  struct _BitOperator *next;

} BitOperator;









/**********************
 * BUILDING THE GRAPH *
 **********************/




/* 
 * Literature: e.g. Technical report 88 
 *         "Extending Planning Graphs to an ADL Subset"
 *
 * more detailed description of current implementation 
 * forthcoming, see IPP - Homepage
 *
 * ... or have a look into 
 * "The efficient Implementation of the Planning Graph in STAN",
 * D.Long and M.Fox, JAIR, 10, 1999
 */





/*
 * ...some preliminary definitions, used in the graph nodes
 */
typedef struct _OpNode OpNode, *OpNode_pointer;
typedef struct _EfNode EfNode;
typedef struct _FtNode FtNode, *FtNode_pointer;
typedef struct _OpEdge OpEdge;
typedef struct _EfEdge EfEdge;
typedef struct _FtEdge FtEdge;
typedef struct _OpLevelInfo OpLevelInfo, *OpLevelInfo_pointer;
typedef struct _EfLevelInfo EfLevelInfo, *EfLevelInfo_pointer;
typedef struct _FtLevelInfo FtLevelInfo, *FtLevelInfo_pointer;


typedef FtNode_pointer FtArray[ARRAY_SIZE];
typedef OpNode_pointer OpArray[ARRAY_SIZE];



/* nodes in the memoization tree UBTree Data Structure
 */
typedef struct _MemoNode MemoNode, *MemoNode_pointer;
typedef MemoNode_pointer MemoNode_table[MEMO_HASHSIZE];




/*
 * operator representation
 */
struct _OpNode {

  char *name;
  short int num_vars, inst_table[MAX_VARS];

  int index;
  int uid_block;
  unsigned int uid_mask;

  FtEdge *preconds;/* a list of pointers to the nodes for its preconds */
  EfNode *unconditional;
  EfNode *conditionals;

  Bool is_noop;/* is it a noop ? (used in printing out the plan) */

  OpLevelInfo_pointer info_at[MAX_PLAN];/* level-dependent info */

  BitVector *pos_precond_vector;
  BitVector *neg_precond_vector;

  struct _OpNode *next;/* ops are globally stored as a list */

  Effect *unactivated_effects;
  struct _OpNode *thread;

};


/*
 * effects...
 */
struct _EfNode {

  OpNode *op;/* the op it belongs to */
  unsigned int first_occurence;/* ...of this effect in the graph */

  FtEdge *conditions;
  FtEdge *effects;

  BitVector *pos_effect_vector;
  BitVector *neg_effect_vector;

  EfLevelInfo_pointer info_at[MAX_PLAN];

  struct _EfNode *next;

};


/*
 * ...and facts.
 */
struct _FtNode {

  int index;/* index in fact hierarchy */

  int uid_block;
  unsigned int uid_mask;

  Bool positive;/* is it the positive occurence ? */

  OpNode *noop;/* to make noops - first strategy explizit */
  EfEdge *adders;/* the effects that add this fact */
  OpEdge *preconds;

  FtLevelInfo_pointer info_at[MAX_PLAN+1];/* level dependent information */

  struct _FtNode *next;

};


/* simply a list element for op-edge-lists.
 */
struct _OpEdge {

  OpNode *op;

  struct _OpEdge *next;

};


/* simply a list element for effect-edge-lists.
 */
struct _EfEdge {

  EfNode *ef;

  struct _EfEdge *next;

};


/* same for facts
 */
struct _FtEdge {

  FtNode *ft;

  struct _FtEdge *next;

};


/*
 * info for an op that changes during evolution of graph
 */
struct _OpLevelInfo {

  int is_used;

  BitVector *exclusives;

};


struct _EfLevelInfo {

  Bool is_dummy;

  BitVector *cond_pos_exclusives;
  BitVector *cond_neg_exclusives;

};


/*
 * level dependent info for facts
 */
struct _FtLevelInfo {

  EfEdge *adders_pointer;
  Bool is_dummy;

  int is_goal;
  int is_true;
  OpArray is_goal_for;

  BitVector *pos_exclusives;
  BitVector *neg_exclusives;

  BitVector *adders;
  BitVector *adders_exclusives;

  MemoNode *memo_start;

};


typedef struct _OpPair {

  OpNode *o1;
  OpNode *o2;

  struct _OpPair *next;

} OpPair;


typedef struct _FtPair {

  FtNode *f1;
  FtNode *f2;

  struct _FtPair *next;

} FtPair;








/************************
 * SEARCHING STRUCTURES *
 ************************/








/* the UBTree node structure
 *
 * ( see Technical Report 108, "A new Method to index and query Sets" )
 */
struct _MemoNode {

  int double_index;

  MemoNode_table *sons;
  
  int min_way;

  /* bei sortierung zusaetzlich *prev
   */
  struct _MemoNode *next;

};


/* a candidate node in the wave front
 *
 * ( Fox / Long: "Fast implementation of the planning graph in STAN" )
 */
typedef struct _Candidate {

  FtArray fts;

  OpArray ops;

  struct _Candidate *father;
  int depth;

  struct _Candidate *prev; 
  struct _Candidate *next;

} Candidate;











/*
 *  -------------------------------- MAIN FN HEADERS ----------------------------
 */










void output_planner_info( float inst_time, float build_time,
			  float excl_time, float search_time,
			  int min_time );
void ipp_usage( void );
Bool process_command_line( int argc, char *argv[] );









/*
 *  ----------------------------- GLOBAL VARIABLES ----------------------------
 */












/*******************
 * GENERAL HELPERS *
 *******************/






/* used to time the different stages of the planner
 */
extern struct tms gstart, gend;
extern float gtotal_time, gexcl_time;

/* the command line inputs
 */
extern struct _command_line gcmd_line;


/* simple help: store names of connectives
 */
extern char *gconnectives[];


/* word size of the used machine
 */
extern const int gcword_size;


/* record memory consumption
 */
extern int gmemory, ggraph_memory, gexcl_memory, gmemo_memory, gwave_memory;


/* default graph save name
 */
extern char gdef_save_name[MAX_LENGTH];







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

/* axioms as in UCPOP before being changed to ops
 */
extern PlOperator *gloaded_axioms;

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
extern String gconstants_table[MAX_CONSTANTS_TABLE];
extern int gconstants_table_size;
extern StringIntegers gtypes_table[MAX_TYPES_TABLE];
extern int gtype_size[MAX_TYPES_TABLE];
extern int gtypes_table_size;
extern String gpredicates_table[MAX_PREDICATES_TABLE];
extern int garity[MAX_PREDICATES_TABLE];
extern int gpredicates_args_type[MAX_PREDICATES_TABLE][MAX_ARITY];
extern int gpredicates_table_size;


/* the parsed input structures, translated into CodeNodes
 */
extern CodeNode *gcode_initial_state;
extern CodeNode *gcode_goal_state;
extern CodeOperator *gcode_operators;


/* helper in solving the Atomic Instantiation problem: 
 *                                    the implicit tuple tables
 *
 * one table size is the size of one implicit table
 * (there are 2^{arity(predicate)} such tables for each predicate)
 *
 * ( see Technical Report 122, "Handling of Inertia in a Planning System" )
 */
extern int_pointer gtuples[MAX_PREDICATES_TABLE];
extern int gone_table_size[MAX_PREDICATES_TABLE];


/* stores inertia - information: is any occurence of the predicate
 * added / deleted in the uninstantiated ops ?
 *
 * ( see TR 122 )
 */
extern Bool gis_added[MAX_PREDICATES_TABLE];
extern Bool gis_deleted[MAX_PREDICATES_TABLE];


/* store the final "relevant facts", see TR 122
 */
extern RelevantFact_pointer grelevant_facts[MAX_RELEVANT_FACTS];
extern int gnum_relevant_facts;
extern CodeOperator *ginst_code_operators;/* helper: first get all instantiated ops */


/* standard name for inferred types ( unary inertia, see TR 122 )
 */
extern char gnew_types_name[MAX_LENGTH];


/* standard name for GOAL-REACHED fact, as needed for disjunctive goals
 */
extern char ggoal_reached_name[MAX_LENGTH];








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
extern BitOperator *gbit_operators;
extern int gnum_bit_operators;
extern FactInfoPair *gbit_initial_state;
extern FactInfoPair *gbit_goal_state;










/**********************
 * BUILDING THE GRAPH *
 **********************/
 









/* 
 * dis is da graph!
 *
 * will later be allocated as an array of pointers to fact nodes
 */
extern FtNode_pointer *gft_table;


/* points to the first element of a global operator (ft) -node-list;
 */ 
extern OpNode *gall_ops_pointer;
extern OpNode *gprev_level_ops_pointer;
extern FtNode *gall_fts_pointer;
extern FtNode *gprev_level_fts_pointer;

extern OpNode *gops_with_unactivated_effects_pointer;


/* current mutexes: exclusives speedup
 */
extern OpPair *gop_mutex_pairs;
extern FtPair *gft_mutex_pairs;


/* information about current state of graph, needed for level off test
 */
extern unsigned int gfacts_count, gexclusions_count;
extern unsigned int gops_count, gops_exclusions_count;


/* for comparison: mutex number between positives
 */
extern int gprint_ftnum, gprint_exnum;


/* the present facts ordered by levels as bitvectors
 */
extern BitVector_pointer gpos_facts_vector_at[MAX_PLAN];
extern BitVector_pointer gneg_facts_vector_at[MAX_PLAN];


/* the bitvector length for ops at each graph level
 */
extern unsigned int gop_vector_length_at[MAX_PLAN];


/* is TRUE iff graph has levelled off.
 */
extern Bool gsame_as_prev_flag;


/* stores the time step at which graph has levelled off.
 */
extern int gfirst_full_time;










/*************
 * SEARCHING *
 *************/
 







/* current state of search: goals at levels,
 *                          same as bitvectors,
 *                          selectde ops
 */
extern FtArray *ggoals_at;
extern int *gnum_goals_at;
extern BitVector_pointer gpos_goals_vector_at[MAX_PLAN];
extern BitVector_pointer gneg_goals_vector_at[MAX_PLAN];
extern OpArray *gops_at;
extern int *gnum_ops_at;


/* the wave front, currently implemented as a doubly
 * connected linear list
 */
extern Candidate *gwave_front_head;
extern Candidate *gwave_front_tail;


/* to avoid memory leak: keep a pointer on the list of
 * candidates that have been expanded and removed from
 * the wave front already
 */
extern Candidate *gwave_front_trash;


/* search space information: actions, noops tried,
 *                           memoization (UBTree) hits
 */
extern int gnum_of_actions_tried, gnum_of_noops_tried;
extern int gsimple_hits, gpartial_hits, gsubset_hits;


/* only for communication from wave front to save graph:
 * to find out, which ops are used in the plan, we need
 * to search the list of candidates (connected by ->father)
 * that starts with the one Candidate that finally led to 
 * a plan.
 *
 * not really good implementation style, but who does really
 * care about this ?
 */
extern Candidate *gplan_start;

/**********************************************************/
/* SWIG Wrapping*/
int run( int argc, char * argv[] );








#endif __IPP_H
