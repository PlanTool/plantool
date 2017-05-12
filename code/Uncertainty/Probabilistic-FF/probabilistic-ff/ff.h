
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
 * File: ff.h
 * Description: Types and structures for the FastForward planner.
 *
 *        --------- ADL  VERSION  v 1.0 --------------
 *
 * Author: Joerg Hoffmann 2000
 * Contact: hoffmann@informatik.uni-freiburg.de
 *
 *********************************************************************/ 








#ifndef __FF_H
#define __FF_H






#include <stdlib.h>
#include <stdio.h>
#include <strings.h>
#include <string.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/timeb.h>
#include <sys/times.h>









/*
 *  ------------------------------------ DEFINES ----------------------------
 */















/***********************
 * MEANINGLESS HELPERS *
 ***********************/




/* strcmp returns 0 if two strings are equal, which is not nice */
#define SAME 0




/* july06: this here is the margin \delta by which we prevent 
 * non-all-outcomes prob eff propagated implication graph nodes
 * from saying that they can achieve the goal by their own right, ie
 * from having weight 1.0.
 *
 * I think that "meaningless helpers" is a good category for that .. ;-)
 */
#define DELTA 0.000001











/****************
 * PARSING ETC. *
 ****************/









/* various defines used in parsing
 */
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


/* first size of goals_at array in 1P extraction
 */
#define RELAXED_STEPS_DEFAULT 25
/* definite size of U facts array (temporal implication graph)
 */
#define RELAXED_STEPS 100
/* bound on edges in a single rplan layer
 */
#define MAX_UEDGES 200



/* size of hash table for repeated states checking
 * during EHC breadth first search
 */
#define EHC_HASH_SIZE 65536
#define EHC_HASH_BITS 65535


/* size of hash table for repeated states checking
 * in plan construction
 */
#define PLAN_HASH_SIZE 1024
#define PLAN_HASH_BITS 1023


/* size of hash table for repeated states checking
 * during BFS search
 */
#define BFS_HASH_SIZE 65536
#define BFS_HASH_BITS 65535


/* cut random values of facts off modulo this value,
 * to make state sums fit into a single integer
 */
#define BIG_INT 1500000


/* when rplan is potentially incomplete, infty is replaced with this.
 */
#define BIG_H 100000







/************************
 * INSTANTIATION LIMITS *
 ************************/








#define MAX_CONSTANTS 2000
#define MAX_PREDICATES 50
#define MAX_TYPES 50
#define MAX_ARITY 5
#define MAX_VARS 15


#define MAX_TYPE 2000


#define MAX_OPERATORS 50


/* in DNF: AND with OR - sons - collect 'hitting set':
 * one son of each OR node. 
 *
 * this here is initial max number of such son s that can be collected
 * (grows dynamically, if required)
 */
#define MAX_HITTING_SET_DEFAULT 1000


#define MAX_TYPE_INTERSECTIONS 10


#define MAX_RELEVANT_FACTS 5000






/******************************************
 * DOMAIN STRUCTURE AND SEARCHING LIMITS *
 ******************************************/







#define MAX_PLAN_LENGTH 100


/* max different outcomes of the same conditional effect;
 * used for help structure in support graph minimization
 */
#define MAX_OUTCOMES 6





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
  exit(1);}


/* add elapsed time from main local time vars to specified val
 */
#define TIME( val ) val += ( float ) ( ( end.tms_utime - start.tms_utime + \
					 end.tms_stime - start.tms_stime  ) / 100.0 )












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
  int display_info;

  int heuristic;
  Bool alloutcomepaths;
  Bool minimize;
  Bool replacenoops;

  int weightprop;
  Bool ancestorpruning;
  Bool maxpeffather;

  Bool ehc;
  Bool help;

  Bool simulation_wmc;

  Bool stagnating;
  int dominating;
  Bool breadth_bfs;

  Bool manual;
  int debug;
  Bool R;
  Bool A;
  Bool T;
  Bool P;

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



/* structure to store  typed-list-of <name>/<variable>,
 * as they are declared in PDDL files
 */
typedef struct _TypedList {

  char *name;

  /* each item in this list is the name of a type which
   * our type is the union of (EITHER - types ...)
   *
   * usually, this will default to a single-item TokenList.
   */
  TokenList *type;
  /* after first sweep, this will contain the number in type table
   */
  int n;

  struct _TypedList *next;

} TypedList;



/* only needed to parse in the predicates and their arg
 * definitions
 */
typedef struct _TypedListList {

  char *predicate;

  TypedList *args;

  struct _TypedListList *next;

} TypedListList;



/* This type indicates whether a node in the pddl tree stands for
 * an atomic expression, a junctor or a quantor. 
 */
typedef enum _Connective{TRU,
			   FAL,
			   ATOM, 
			   NOT, 
			   AND, 
			   OR, 
			   ALL, 
			   EX, 
			   WHEN,
  			   UNKNOWN,
                           CPT,
                           ONEOF,
                           MULTI} Connective;



/*
 * This is a node in the tree to parse PDDL files
 */
typedef struct _PlNode {

  /* type of the node
   */
  Connective connective;

  /* only for parsing: the var args in quantifiers
   */
  TypedList *parse_vars;

  /* AND, OR, NOT, WHEN => NULL
   * ALL, EX            => the quantified variable with its type
   * ATOM               => the atom as predicate->param1->param2->...
   *
   * CPT: the atom whose probability is specified.
   */
  TokenList *atom;

  /* note: UNKNOWN serves only as father node for CPT node.
   * inherited from cff, where actual facts where son of unknown 
   * node. no idea what that's supposed to be good for, right now,
   * but I guess I had my reasons (did I?)
   */

  /* only for CPT: the 0 < p < 1 value given in this entry; 
   */
  double cpt_p;

  /* july06: only for  WHEN: the 0 < prob <= 1 given for this cond effect.
   * (= 1 iff no prob specified)
   */
  double eff_p;


  /* (a) for AND, OR this is the list of sons(a AND b AND c...),
   * (b) for the rest this is the son, e.g. a subtree that is negated
   * (c) for WHEN, the first son is the condition and the next son
   * is the effect
   *
   * for CPT it's the condition of the entry
   * for ONEOF it's the oneof list
   * for MULTU it's the multivar-list
   */
  struct _PlNode *sons;

  /* if you have a list of sons, they are connected by next
   */
  struct _PlNode *next;

} PlNode, *PlNode_pointer;


/*
 * This resembles an uninstantiated PDDL operator
 */
typedef struct _PlOperator {

  char *name;

  /* only important for PDDL where :VARS may be added to the param list
   * which must be hidden when writing the plan to an output file
   */
  int number_of_real_params; 

  /* the params, as they are declared in domain file
   */
  TypedList *parse_params;

  /* params is a list of variable/type pairs, such that:
   * factlist->item = [variable] -> [type]
   */
  FactList *params;
  PlNode *preconds;
  PlNode *effects;

  struct _PlOperator *next;

} PlOperator;















/***************** 
 * INSTANTIATION *
 *****************/









/* helpers
 */

typedef int TypeArray[MAX_TYPE_INTERSECTIONS];

typedef int *int_pointer;




/* first step structures: parsing & preprocessing
 */

typedef struct _Fact {

  int predicate, args[MAX_ARITY];

} Fact;



typedef struct _WffNode {

  Connective connective;

  /* in ALL/EX s
   */
  int var, var_type;
  char *var_name;

  /* in AND/OR s
   */
  struct _WffNode *sons;
  /* sons are doubly connected linear list
   */
  struct _WffNode *next;
  struct _WffNode *prev;

  /* in ATOMs
   */
  Fact *fact;
  /* after translation: mark NOT-p s for efficiency
   */
  int NOT_p;

  /* in ALL/EX/NOT
   */
  struct _WffNode *son;

  /* for expansion speedup
   */
  Bool visited;

  /* no WHEN s here... use Pl Connectives anyway for simplicity
   */

} WffNode, *WffNode_pointer;



typedef struct _Facts {

  Fact *fact;

  struct _Facts *next;

  /* the CPT of the fact; will be empty if fact is known
   * (data structure only used for initial state parsing)
   */
  double *cpt_p;
  WffNode_pointer *cpt_c;
  int num_cpt;

} Facts;



typedef struct _Literal {

  Bool negated;

  Fact fact;

  struct _Literal *next;
  struct _Literal *prev;

} Literal;



typedef struct _Effect {

  int num_vars, var_types[MAX_VARS];
  char *var_names[MAX_VARS];

  WffNode *conditions;
  double eff_p;/* july06: this eff's probability! */

  Literal *effects;

  struct _Effect *next;
  struct _Effect *prev;

} Effect;



typedef struct _Operator {

  char *name, *var_names[MAX_VARS];
  int number_of_real_params; 

  int num_vars, var_types[MAX_VARS];
  Bool removed[MAX_VARS];
 
  WffNode *preconds;

  Effect *effects;

  Bool hard;

} Operator, *Operator_pointer;




/* second step: structures that keep already normalized
 * operators
 */




typedef struct _NormEffect {

  int num_vars, var_types[MAX_VARS];
  int inst_table[MAX_VARS];

  Fact *conditions;
  int num_conditions;
  double eff_p;/* july06: this eff's probability! */

  Fact *adds;
  int num_adds;
  Fact *dels;
  int num_dels;

  struct _NormEffect *prev;
  struct _NormEffect *next;

} NormEffect;



typedef struct _NormOperator {
  
  Operator *op;

  int num_vars, var_types[MAX_VARS];
  int inst_table[MAX_VARS];
  int removed_vars[MAX_VARS], num_removed_vars, type_removed_vars[MAX_VARS];

  Fact *preconds;
  int num_preconds;

  NormEffect *effects;

  Bool out;

} NormOperator, *NormOperator_pointer;
  


/* minimal info for a fully instantiated easy operator;
 * yields one action when expanded
 */
typedef struct _EasyTemplate {

  NormOperator *op;
  int inst_table[MAX_VARS];

  struct _EasyTemplate *prev;
  struct _EasyTemplate *next;

} EasyTemplate;






/* structures for hard ops
 */





/* intermediate step: structure for keeping hard ops
 * with normalized precondition, but arbitrary
 * effect conditions
 */
typedef struct _MixedOperator {
  
  Operator *op;

  int inst_table[MAX_VARS];

  Fact *preconds;
  int num_preconds;

  Effect *effects;

  struct _MixedOperator *next;

} MixedOperator;





/* last hard step: everything is action - like, except that
 * facts are not yet integer coded
 */  

typedef struct _PseudoActionEffect {

  Fact *conditions;
  int num_conditions;
  double eff_p;/* july06: this eff's probability! */

  Fact *adds;
  int num_adds;
  Fact *dels;
  int num_dels;

  struct _PseudoActionEffect *next;

} PseudoActionEffect;



typedef struct _PseudoAction {

  Operator *op;

  int inst_table[MAX_VARS];

  Fact *preconds;
  int num_preconds;

  PseudoActionEffect *effects;
  int num_effects;

} PseudoAction, *PseudoAction_pointer;




/* final domain representation structure
 */




typedef struct _ActionEffect {

  int *conditions;
  int num_conditions;
  double eff_p;/* july06: this eff's probability! */

  int *adds;
  int num_adds;
  int *dels;
  int num_dels;

} ActionEffect;



typedef struct _Action {

  NormOperator *norm_operator;
  PseudoAction *pseudo_action;

  char *name;
  int num_name_vars;
  int name_inst_table[MAX_VARS];

  int inst_table[MAX_VARS];

  int *preconds;
  int num_preconds;

  ActionEffect *effects;
  int num_effects;

  struct _Action *next;

} Action;











/*****************************************************
 * BASIC OP AND FT STRUCTURES FOR CONNECTIVITY GRAPH *
 *****************************************************/











typedef struct _OpConn {

  /* to get name
   */
  Action *action;

  /* preconds
   */
  int *P;
  int num_P;

  /* effects
   */
  int *E;
  int num_E;

  /* member for applicable actions extraction
   */
  Bool is_in_A;
  int num_active_Ps;
  Bool ch;

  /* members for 1Ph - H(S) extraction
   *
   * simple solution: static array size, in contrast to lgoals_at array in relax.c
   */
  Bool is_used_at[RELAXED_STEPS];
  Bool is_used;
  Bool is_in_H;

} OpConn;



typedef struct _EfConn {

  int op;

  /* op preconds + conds
   *
   * redundant with info below; keep that in from original FF
   * for the time being as don't know which info is more useful
   * later.
   */
  int *PC;
  int num_PC;

  /* conditions separately as we will be reasoning about these
   */
  int *C;
  int num_C;
  double eff_p;/* july06: this eff's probability! */

  int *A;
  int num_A;

  int *D;
  int num_D;

  /* july06: same C effects -- interpreted as alternative 
   * probabilistic outcomes of the action!!!
   */
  int *S;
  int num_S;

  /* july06: those guys list the common adds of all efects in this efs 
   *         S group; can be used for state trans checks:
   *         if a fact is affected by a prob effect, then it's
   *         like "normal" added/deleted iff its added/deleted by all outcomes.
   */
  int *SA;
  int num_SA;
  int *SD;
  int num_SD;
  /* ... so these guys here store the inverse! (not common effects)
   */
  int *NSA;
  int num_NSA;
  int *NSD;
  int num_NSD;





  /* implied C effects
   */
  int *I;
  int num_I;

  Bool removed;


  /* members for relaxed fixpoint computation
   */
  int level;
  Bool in_E;
  int num_active_PCs;
  Bool ch;


  /* july06: keep separate numbering of *probabilistic* effects only,
   * to save space in "codes" array
   *
   * each effect also has a chance var associated. these simply go in
   * the same numbering, ie each prob effect occupies TWO numbers.
   *
   * (really, only almost all effects -- all but one of each group of alternatives --
   * need this. fuck it.)
   */
  int pef_id;
  int pef_chance_id;

} EfConn;



typedef struct _FtConn {

  /* its negation; the resp ft number if it is translated,
   * otherwise -1
   */
  int negation;

  /* ops it is precond of -> get_A
   */
  int *P;
  int num_P;

  /* effects it is union conds, pres element of
   */
  int *PC;
  int num_PC;

  /* efs it is cond of -> figure will need this for relaxed plans
   */
  int *C;
  int num_C;

  /* efs that add or del it
   */
  int *A;
  int num_A;

  int *D;
  int num_D;

  /* members for orderings preprocessing
   */
  int *False;
  int num_False;




  /* members for CPT stuff
   */
  Bool is_state_var; /* is a state var? or a chance var */
  /* state vars are the original facts; chance vars are newly introduced
   * to implement the weighting; see Sang&Beame&Kautz,AAAI'05
   */

  int *multi, num_multi, multiID;
  /* if fact forms part of multi valued var, then 
   * this here is the array of the involved facts.
   *
   * if not, then multi == 1.
   *
   * multi-val vars are part of the interpretation of the input as
   * general bayesian network; the chance vars and weights will be
   * computed accordingly.
   *
   * for fast finding of matching multi-var facts, multiindex stores
   * an ID of the multi-var
   */
  Bool had_multi, realmulti;
  /* just to remember that we did this one
   *
   * realmulti is true for fts part of multi-sate var specified in ini
   * ( as opposed to "multistate" ft and negft)
   */
  double parsed_weight;
  /* this guy is the parsed cpt weight for a multi-var ft with empty
   * cpt condition, and -1 for all other facts.
   */
  



  int *chance_vars, num_chance_vars;
  /* array of chance vars generated for this fact (if it is a non-root state var);
   * correspond to entries in CPT table.
   *
   * needed for some indexing when generating initial state CNF.
   * if no var is generated for a CPT row (ie, probability 0 or 1)
   * then the int there is -1.
   */

  Bool weighted;
  double weight_a, weight_b;/* the weight as ratio a/b */
  /* state vars have 100 unless they're root nodes in the Bayesian network,
   * in which case their weight is their associated likelyhood.
   */
  int chance_var_for, cpt_row;
  /* if it is a chance var, this one here is the target fact/CPT row nr.
   * just for printing.
   */




  /* members for relaxed fixpoint computation
   */
  int level;
  Bool in_F;

  /* members for 1Ph extraction
   *
   * NOTE: by standard as inherited from classical FF, 
   *       a fact only ever becomes a subgoal at its 1st
   *       level in the RPG. for this we don't need an array
   *       here but only a  single Bool. keep full array structure
   *       for more flexibility, eg. making facts goals
   *       at those points in the RPG where needed
   *       (this can make a difference when, eg. some op1 just below
   *       the op2 in need of precond p, but above the 1st level of p,
   *       adds p)
   */
  Bool is_goal_at[RELAXED_STEPS];
  int is_true;
  Bool ch;

  /* search
   */
  int rand;/* for hashing */
  Bool is_global_goal;/* fast goal add finding */

  /* in goal or in eff PC
   */
  Bool srelevant;

  /* ini U or eff (add o. del) of a c eff
   */
  Bool poss_U;

  Bool CNF;

} FtConn;













/****************************
 * in RELAXATION            *
 ****************************/


















typedef struct _UftNode UftNode, *UftNode_pointer;

struct _UftNode {

  /* well, who's in here?
   */
  int ft;
  
  /* this will be false except at the layer where the fact became
   * true --> flag for recognizing such facts fast later
   * in rplan extraction.
   */
  Bool became_F;

  /* two corresponding arrays, one for
   * the respective Uft nodes one layer earlier,
   * one for the effects inducing the edges.
   */
  UftNode_pointer in_edges[MAX_UEDGES];
  int in_efs[MAX_UEDGES];
  int num_in;


  /* july06: in prob U nodes, ft will be -1 and these guys here 
   * will be set:
   */
  int Utime;/* we distinguish the application of each nondet eff at each time in the RPG */
  int ef;/* the responsible effect; edges into this node will say "-1971" is the effect (-1 is noop)*/


  /* july06: this guy here stores the dynamically created weight in backchaining.
   * don't confuse with the fixed weight of the corresponding effects,
   * which is looked up in gefconn[ef] with ef from above.
   */
  double weight;

  /* july06: this guy here stores whether or not this guy had a "1.0 - parent" ie either
   * a deterministic parent with weight 1.0, or nondet parents for all outcomes
   * of a nondet guy.
   *
   * NOTE: we could just as well store this guy (and weiggt as well, with some adaptations)
   * in a static array in Uleaf_disjunctions_supported_by_formula, relax.cpp, which is the only
   * point where they are used (currently). well, whatever...
   */
  Bool has_full_parents;


  /* this is used in the dynamic lists used to store the prob nodes in the 
   * implication graph.
   */
  struct _UftNode *next;

};



/* this is a helper for support graph minimization: the bwd chaining graph is stored
 * in terms of these accumulated edges between fact nodes. makes it more humane to
 * propagate the weights through the bugger..
 * action selection will simply go through all edges and select those that are
 * marked active.
 */
typedef struct _UEdgeNode {

  /* indices of pnodes in bwd chained graph, just to keep a close
   * correspondence... one never knows...
   */
  int pnodefrom, pnodeto;
  int t;/* t of pnode to */
  int realtime;/* the real time point along the plan that t corresponds to */

  /* a set of effects (effect outcomes, possibly) constituting this edge
   *
   * separated by different action effects!! i.e. if there is more than one
   * guy in here then that simply means that those are alternative outcomes
   * of the same effect!
   */
  int E[MAX_OUTCOMES], num_E;
  /* the accumulated prob of these efs
   */
  double P;

  /* is this currently put in the graph?
   */
  Bool active;

} UEdgeNode;



























/****************************
 * STRUCTURES FOR SEARCHING *
 ****************************/









typedef struct _State {
  
  int *F;
  int num_F;

  int max_F;

  /* the unknown facts!
   *
   * these are, in difference to the F facts above, allocated
   * statically, ie always with the max possible number
   * -- assuming that the nr of unknown facts will not be critically 
   *    high in itself.
   */
  int *U;
  int num_U;

  /* the cond effects (ie their inidices in gef_conn) with unknown antecedents 
   * of the action that leads to this state.
   *
   * allocated statically to max number of cond add (del) effects
   * of any action.
   *
   */
  int *unknown_E;
  int num_unknown_E;

} State, *State_pointer;



typedef struct _EhcNode {
  
  State S;

  /* op that leads into this state
   */
  int op;
  int depth;

  struct _EhcNode *father;
  struct _EhcNode *next;

} EhcNode;



typedef struct _EhcHashEntry {

  int Fsum, Usum;

  EhcNode *ehc_node;

  struct _EhcHashEntry *next;

} EhcHashEntry, *EhcHashEntry_pointer;



typedef struct _PlanHashEntry {

  int sum;
  State S;

  /* step is number of op that is EXECUTED in S;
   * -1 means that this state is no longer contained in plan
   */
  int step;
  struct _PlanHashEntry *next_step;

  struct _PlanHashEntry *next;

} PlanHashEntry, *PlanHashEntry_pointer;



typedef struct _BfsNode {
  
  State S;

  /* op that leads into this state
   */
  int op;
  int h;

  struct _BfsNode *father;

  struct _BfsNode *next;
  struct _BfsNode *prev;

} BfsNode;



typedef struct _BfsHashEntry {

  int Fsum, Usum;

  BfsNode *bfs_node;

  struct _BfsHashEntry *next;

} BfsHashEntry, *BfsHashEntry_pointer;




/* for state transition
 */
typedef struct _TimedLiteral {

    int literal;
    int time;

} TimedLiteral;



/* for dynamic literal-clause membership lists
 */
typedef struct _MemberList {

  int clause;

  struct _MemberList *next;

} MemberList, *MemberList_pointer;








/*
 *  -------------------------------- MAIN FN HEADERS ----------------------------
 */







void output_planner_info( void );
void ff_usage( void );
Bool process_command_line( int argc, char *argv[] );









/*
 *  ----------------------------- GLOBAL VARIABLES ----------------------------
 */












/*******************
 * GENERAL HELPERS *
 *******************/










extern struct tms start, end;

/* used to time the different stages of the planner
 */
extern float gtempl_time, greach_time, grelev_time, gconn_time, gmem_time;
extern float gsearch_time, geval_time, gcnf_time, genc_time, gsat_time;
extern float grs_time, grs_sat_time, gsc_time, gss_time;
extern float gr_sat_time, grp_sat_time, gr_cnf_time, gr_enc_time, gmembership_time;
extern float gwmc_time, gP_time;

extern float gDP_time, gDP_parsetime, gWMC_filetime; /* july06: used to communicate the time taken in DP, to whomever */

extern int gsat_calls, gcnfs, grs_sat_calls, gss_sat_calls;
extern int gr_sat_calls, grp_sat_calls, gsc_sat_calls, grs_comps;
extern int grs_hits, gss_hits, grs_conf_comps, gdp_calls, gup_calls, gwmc_up_calls;
extern int gwmc_calls, gPwmc_calls, gsim_wmc_calls;



/* the command line inputs
 */
extern struct _command_line gcmd_line;

/* number of states that got heuristically evaluated
 */
extern int gevaluated_states;

/* maximal depth of breadth first search
 */
extern int gmax_search_depth;


/* CNF statistic
 */
extern float *gsum_k_clauses, gsum_clauses;






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

/* stores initial ors as an array of OR lists
 */
extern PlNode_pointer *gorig_initial_ors;
extern int gnum_orig_initial_ors;

/* stores initial oneofs as an array of ONEOF lists
 */
extern PlNode_pointer *gorig_initial_oneofs;
extern int gnum_orig_initial_oneofs;

/* stores multis as an array of MULTI lists
 */
extern PlNode_pointer *gorig_initial_multis;
extern int gnum_orig_initial_multis;

/* not yet preprocessed goal facts
 */
extern PlNode *gorig_goal_facts;

/* the types, as defined in the domain file
 */
extern TypedList *gparse_types;

/* the constants, as defined in domain file
 */
extern TypedList *gparse_constants;

/* the predicates and their arg types, as defined in the domain file
 */
extern TypedListList *gparse_predicates;

/* the objects, declared in the problem file
 */
extern TypedList *gparse_objects;



/* connection to instantiation ( except ops, goal, initial )
 */

/* all typed objects 
 */
extern FactList *gorig_constant_list;

/* the predicates and their types
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




/* the domain in first step integer representation
 */
extern Operator_pointer goperators[MAX_OPERATORS];
extern int gnum_operators;
extern Fact *gfull_initial;
extern int gnum_full_initial;
extern Fact *gfull_unknown_initial;
extern int gnum_full_unknown_initial;
/* for each unknown initial fact nr. i,
 * we have an (j) array of size gnum_full_unknown_initial_cpt
 * with one probability entry in gfull_unknown_initial_cpt_p[i][j],
 * and one AND of facts in gfull_unknown_initial_cpt_c[i][j]
 */
extern double **gfull_unknown_initial_cpt_p;
extern WffNode_pointer **gfull_unknown_initial_cpt_c;
extern int *gnum_full_unknown_initial_cpt;
/* multiple-val variables: these 
 * will (only) be taken into account when creating the
 * initial state CNF representing the Bayesian network
 */
extern WffNode_pointer *gfull_multi_initial;
extern int gnum_full_multi_initial;
extern WffNode_pointer *gfull_or_initial;
extern int gnum_full_or_initial;
extern WffNode_pointer *gfull_oneof_initial;
extern int gnum_full_oneof_initial;
extern WffNode *ggoal;



/* stores inertia - information: is any occurence of the predicate
 * added / deleted in the uninstantiated ops?
 * is any occurence of the predicate unknown?
 */
extern Bool gis_added[MAX_PREDICATES];
extern Bool gis_deleted[MAX_PREDICATES];
extern Bool gis_unknown[MAX_PREDICATES];



/* splitted initial state:
 * initial non static facts,
 * initial static facts, divided into predicates
 * (will be two dimensional array, allocated directly before need)
 *
 * the same mirrored for unknown facts -- "known negatives" is transferred
 * here to "known positives and unknowns"; seems more adequate for later 
 * purposes, giving access to unknowns directly. note that unknowns are
 * not assumed static.
 */
extern Facts *ginitial;
extern int gnum_initial;
extern Facts *gunknown_initial;
extern int gnum_unknown_initial;
extern Fact **ginitial_predicate;
extern int *gnum_initial_predicate;
extern Fact **gunknown_initial_predicate;
extern int *gnum_unknown_initial_predicate;

/* this here stores dependencies between initial variables:
 * when translating negations of an unkwown literal we need
 * to remember that the translation, while unkown, will
 * always have the respective inverse value.
 * we remember the fts for whic this holds.
 */
extern Facts *ginitial_ft_equivalence_A;
extern Facts *ginitial_ft_equivalence_notA;
extern int gnum_initial_ft_equivalence;



/* the type numbers corresponding to any unary inertia
 */
extern int gtype_to_predicate[MAX_PREDICATES];
extern int gpredicate_to_type[MAX_TYPES];

/* (ordered) numbers of types that new type is intersection of
 */
extern TypeArray gintersected_types[MAX_TYPES];
extern int gnum_intersected_types[MAX_TYPES];


/* stores which predicate is a translation of which other one.
 */
extern int gtranslated_predicate_to[MAX_PREDICATES];


/* splitted domain: hard n easy ops
 */
extern Operator_pointer *ghard_operators;
extern int gnum_hard_operators;
extern NormOperator_pointer *geasy_operators;
extern int gnum_easy_operators;



/* so called Templates for easy ops: possible inertia constrained
 * instantiation constants
 */
extern EasyTemplate *geasy_templates;
extern int gnum_easy_templates;



/* first step for hard ops: create mixed operators, with conjunctive
 * precondition and arbitrary effects
 */
extern MixedOperator *ghard_mixed_operators;
extern int gnum_hard_mixed_operators;



/* hard ''templates'' : pseudo actions
 */
extern PseudoAction_pointer *ghard_templates;
extern int gnum_hard_templates;



/* store the final "relevant facts"
 */
extern Fact grelevant_facts[MAX_RELEVANT_FACTS];
extern int gnum_relevant_facts;
extern int gnum_pp_facts;



/* the final actions and problem representation
 */
extern Action *gactions;
extern int gnum_actions;
extern State ginitial_state;
extern State ggoal_state;
/* how sure do we want to be to have achieved the goals?
 */
extern double ggoal_probability;
/* the same, rounded to ints, for 1P p propagation and goalp == 1 questions
 */
extern int ggoal_percent;
/* to access initially valid implications: the translated facts.
 */
extern int *ginitial_equivalence_A;
extern int *ginitial_equivalence_notA;
extern int gnum_initial_equivalence;
/* to know how much space we need for unknown conds in states
 */
extern int gmax_E;
/* the initial OR constraints in final coding
 */
extern int **ginitial_or;
extern int *ginitial_or_length;
extern int gnum_initial_or;
/* says if or if not we got ``evidence'',
 * i.e., clauses constraining our BN. 
 */
extern Bool gBNevidence;
/* stores the weight of the initial network,
 * which is the same as the weight of all belief states. (?!)
 *
 * computation cheap-and-dirty: is made in relax.c upon first call to h fn
 * ie as side effect of checking goal in that state.
 */
extern double gBNa, gBNb;






/**********************
 * CONNECTIVITY GRAPH *
 **********************/





/* one ops (actions) array ...
 */
extern OpConn *gop_conn;
extern int gnum_op_conn;



/* one effects array ...
 */
extern EfConn *gef_conn;
extern int gnum_ef_conn;

/* july06: separate nr for prob effects.
 */
extern int gnum_pef_conn;

/* july06: static global lookup table of the weights associated with
 * the pef probvars: a/b
 */
extern double *gpef_conn_weight_a;
extern double *gpef_conn_weight_b;



/* one facts array.
 */
extern FtConn *gft_conn;
extern int gnum_ft_conn;
/* the nr. of non-chance vars (first .. in gft_conn)
 */
extern int gnum_real_ft_conn;



/* max #conds. for max clauses computation.
 */
extern int gmax_C;



/* max U: all initial Us plus facts that are 
 * added / deleted by a conditional effect with poss_U conds.
 * (important for various memory allocations)
 */
extern int gmax_U;
extern int gmax_CNFU;

/* we get these max #s of clauses and lits.
 */
extern int gmax_clauses;
extern int gmax_rs_clauses;
extern int gmax_literals;



/* says if or if not our initial BN is just the cross-product
 * of independent multi-state vars
 */
extern Bool gindimulti;








/*******************
 * SEARCHING NEEDS *
 *******************/








/* applicable actions
 */
extern int *gA;
extern int gnum_A;



/* communication from extract 1.P. to search engine:
 * 1P action choice
 */
extern int *gH;
extern int gnum_H;



/* always stores (current) serial plan
 */
extern int gplan_ops[MAX_PLAN_LENGTH];
extern int gnum_plan_ops;



/* stores the states that the current plan goes through
 */
extern State gplan_states[MAX_PLAN_LENGTH + 1];



/* the clauses to be communicated to the SAT solver for
 * determining inferred literals.
 */
extern TimedLiteral **gclauses;
extern int *gclause_length;
extern int gnum_fixed_clauses;
extern int gnum_clauses;
extern int gfixed_endtime;

/* array; maps ft / time pair to its number in CNF encoding.
 */
extern int **gcodes;
extern int gnum_fixed_c;


/* inverse mapping, to undo changes in table.
 */
extern int *gcf, *gct, gnum_c;

/* july06: some of the codes will be artificial, and for those we got
 * to communicate the dynamically created weight, with this guy here.
 */
extern double *gcweight;

 

/* statistics: count nr. of times the "disjunction minimisation" actually
 * minimised something
 */
extern int gremoved_lits;
extern int gPremoved_lits;



/* stores the current DP decisions including unit propagations.
 *
 * is for DP given in state_transitions.c!!!!!
 *
 * have to make this global as it's also accessed from repeated states --
 * when checking stagnation. I know it's ugly...
 *
 * july06: actually, right now, since the internal DPLL is removed, this merely is
 * a communication channel for unit literals set in specific queries.
 */
extern int *gdecision_stack;
extern int gnum_decision_stack;



/* this here used to be more important (could change value), but now is only a shorthand
 * statically computed info for asking before wmc whether or not the fact is weighted,
 * and computing its weight (a/b) if it is. only left in since it doesn't hurt
 * so why bother.
 *
 * set in initialize_state_transitions()
 */
extern double *ginitial_ft_weight;




/* automatically checked Bool saying if sufficient criteria for
 * "more facts are always better" holds.
 */
extern Bool gdomination_valid;



#endif /* __FF_H */
