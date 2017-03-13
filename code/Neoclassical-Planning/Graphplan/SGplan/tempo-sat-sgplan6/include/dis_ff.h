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

 * File: dis_ff.c 

 * Description: modified from ff.h in Metric-FF

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
 * File: ff.h
 * Description: Types and structures for the Metric-FastForward planner.
 *
 *        --------- PDDL2.1 level 2 :: VERSION  v 1.0 --------------
 *
 * Author: Joerg Hoffmann 2001
 * Contact: hoffmann@informatik.uni-freiburg.de
 *
 *********************************************************************/ 








#ifndef __dis_FF_H
#define __dis_FF_H






#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/timeb.h>
#include <sys/times.h>









/*
 *  ------------------------------------ DEFINES ----------------------------
 */











/***********************
 * MEANINGdis_LESS HELPERS *
 ***********************/




/* strcmp returns 0 if two strings are equal, which is not nice */
#define dis_SAME 0









/****************
 * PARSING ETC. *
 ****************/









/* various defines used in parsing
 */
#define dis_HIDDEN_STR "#"
#define dis_AXIOM_STR "AXIOM"
#define dis_NAME_STR "name\0"
#define dis_VARIABLE_STR "variable\0"
#define dis_STdis_ANDARD_TYPE "OBJECT\0"
#define dis_EITHER_STR "EITHER"









/***************************
 * SOME ARBITRARY SETTINGS *
 ***************************/







/* maximal string length
 */
#define dis_MAX_LENGTH 256 


/* marks border between connected items 
 */
#define dis_CONNECTdis_OR "~"


/* size of goals_at array in 1P extraction
 */
#define dis_RELAXED_STEPS_DEFAULT 25


/* size of hash table for repeated states checking
 * during EHC breadth first search
 */
#define dis_EHC_HASH_SIZE 8192
#define dis_EHC_HASH_BITS 8191


/* size of hash table for repeated states checking
 * in plan construction
 */
#define dis_PLAN_HASH_SIZE 1024
#define dis_PLAN_HASH_BITS 1023


/* size of hash table for repeated states checking
 * during BFS search
 */
#define dis_BFS_HASH_SIZE 65536
#define dis_BFS_HASH_BITS 65535


/* cut random values of facts off modulo this value,
 * to make state sums fit into a single integer
 */
#define dis_BIG_INT 1000000


/* max number of different fluents in one list of LNF
 */
#define dis_MAX_LNF_F 25
#define MAX_EXP_LNUM_F 2000

/* max number of comps in one cond / precond / goal
 */
#define dis_MAX_LNF_dis_COMPS 100


/* max number of lnf effects in one action effect
 */
#define dis_MAX_LNF_EFFS 50







/************************
 * INSTANTIATION LIMITS *
 ************************/








#define dis_MAX_CONSTANTS 65536
#define dis_MAX_PREDICATES 65536
#define dis_MAX_FUNCTIONS 65536
#define dis_MAX_TYPES 256
#define dis_MAX_ARITY 8
#define dis_MAX_VARS 9


#define dis_MAX_TYPE 2000


#define dis_MAX_OPERATdis_ORS 50000


/* in DNF: dis_AND with dis_OR - sons - collect 'hitting set':
 * one son of each dis_OR node. 
 *
 * this here is initial max number of such son s that can be collected
 * (grows dynamically, if required)
 */
#define dis_MAX_HITTING_SET_DEFAULT 1000


#define dis_MAX_TYPE_INTERSECTIONS 10


#define dis_MAX_RELEVANT_FACTS 262144
#define dis_MAX_RELEVANT_FLUENTS 65536






/******************************************
 * DOMAIN Sdis_TRUCTURE dis_AND SEARCHING LIMITS *
 ******************************************/






#define dis_MAX_STATE 10000


#define dis_MAX_PLAN_LENGTH 10000







/****************
 * CODE DEFINES *
 ****************/









/* not a real 'code' define; used in relax and search to encode
 * infinite level number / plan length
 */
#ifndef dis_INFINITY
#define dis_INFINITY 1000*dis_BIG_INT
#endif







/* define boolean types if not allready defined
 */
#ifndef dis_Bool
typedef unsigned char dis_Bool;
#ifndef dis_TRUE /* we assume that dis_FALSE is also not defined */
#define dis_TRUE 1
#define dis_FALSE 0
#endif /* dis_TRUE */
#endif /* dis_Bool */


/* code a param number into a negative number and vice versa
 */
#define dis_ENCODE_VAR( val ) (val * (-1)) - 1
#define dis_DECODE_VAR( val ) (val + 1) * (-1)
/* PDDL3.1 */
#define NUMBER_TYPE     2147483647
#define dis_GET_CONSTANT( val, pointer ) ( val >= 0 ) ? val : pointer->inst_table[dis_DECODE_VAR( val )]


/* Check allocated memory
 */
#define dis_CHECK_PTR(p) if (NULL == (p)) { \
  fprintf(stdout, "\n\aNO MEMORY in file %s:%d\n\n", __FILE__, __LINE__); \
  exit(1);}


/* add elapsed time from main local time vars to specified val
 */
#define dis_TIME( val ) val += ( float ) ( ( end.tms_utime - start.tms_utime + \
					 end.tms_stime - start.tms_stime  ) / 100.0 )












/*
 *  ------------------------------ DATA Sdis_TRUCTURES ----------------------------
 */











/*******************
 * GENERAL HELPERS *
 *******************/








/* all command switches
 */
struct dis__command_line {

  char path[dis_MAX_LENGTH];
  char ops_file_name[dis_MAX_LENGTH];
  char fct_file_name[dis_MAX_LENGTH];
  int display_info;
  int debug;

  dis_Bool optimize;
  dis_Bool ehc;

  int g_weight;
  int h_weight;

};


typedef char *dis_Token;












/***********
 * PARSING *
 ***********/










/* A list of strings
 */
typedef struct _dis_TokenList {

  char *item;
  struct _dis_TokenList *next;

} dis_TokenList;



/* list of string lists
 */
typedef struct _dis_FactList {

  dis_TokenList *item;
  struct _dis_FactList *next;

} dis_FactList;



/* structure to store  typed-list-of <name>/<variable>,
 * as they are declared in PDDL files
 */
typedef struct _dis_TypedList {

  char *name;

  /* each item in this list is the name of a type which
   * our type is the union of (EITHER - types ...)
   *
   * usually, this will default to a single-item dis_TokenList.
   */
  dis_TokenList *type;
  /* after first sweep, this will contain the number in type table
   */
  int n;

  struct _dis_TypedList *next;

} dis_TypedList;



/* only needed to parse in the predicates and their arg
 * definitions
 */
typedef struct _dis_TypedListList {

  char *predicate;

  dis_TypedList *args;

  struct _dis_TypedListList *next;

} dis_TypedListList;



typedef enum _dis_Expdis_Connective{FHEAD = 1000,
			    NUMBER,
			    MINUS,
			    AD,
                SU, 
			    MU, 
			    DI,
                /* PDDL3 */
			    VIO,
                /* durative actions */
                DURATION,
                EXPT1,
                EXPT2,
                EXPT3   } dis_Expdis_Connective;



typedef struct _dis_Parsedis_ExpNode {

  dis_Expdis_Connective connective;

  /* NULL anywhere except when node is FHEAD or NUMBER
   * (in which case it is fn name ... resp. number (int or float) as string
   */
  dis_TokenList *atom;

  /* both NULL in FHEAD;
   * in MINUS, left is son and right is NULL
   * else (binary operators), left and right operand
   */
  struct _dis_Parsedis_ExpNode *leftson, *rightson;

} dis_Parsedis_ExpNode;



/* This type indicates whether a node in the pddl tree stands for
 * an atomic expression, a junctor or a quantor. 
 */
typedef enum _dis_Connective{dis_TRU = 2000,
			 dis_FAL,
			 dis_ATOM,
			 dis_COMP,
			 dis_NEF,
			 dis_NOT, 
			 dis_AND, 
			 dis_OR, 
			 dis_ALL, 
			 dis_EX, 
			 dis_WHEN,
             /* durative actions */
             dis_AT_START_CONN,
             dis_AT_END_CONN,
             dis_OVER_ALL_CONN} dis_Connective;



typedef enum _dis_Comparator{IGUAL = 3000, /* technical if conds are array comp exp, resp float */
			 dis_IS_VIOLATED,
			 LE,
			 LEQ,
			 EQ,
			 GEQ,
			 GE} dis_Comparator;




typedef enum _dis_Numericdis_EffectType{ASSIGN = 4000,
				SCALE_UP,
				SCALE_DOWN,
				INCREASE,
				DECREASE} dis_Numericdis_EffectType;




/*
 * This is a node in the tree to parse PDDL files
 */
typedef struct _dis_PlNode {

  /* type of the node
   */
  dis_Connective connective;

  /* only for parsing: the var args in quantifiers
   */
  dis_TypedList *parse_vars;

  /* dis_AND, dis_OR, dis_NOT, dis_WHEN,
   * dis_COMP, dis_NEF         => NULL
   * dis_ALL, dis_EX            => the quantified variable with its type
   * dis_ATOM               => the atom as predicate->param1->param2->...
   */
  dis_TokenList *atom;
  /* all except dis_COMP, dis_NEF => NULL
   * dis_COMP, dis_NEF => left hand, right hand
   */
  dis_Comparator comp;
  dis_Numericdis_EffectType neft;
  dis_Parsedis_ExpNode *lh, *rh;

  /* (a) for dis_AND, dis_OR this is the list of sons(a dis_AND b dis_AND c...),
   * (b) for the rest this is the son, e.g. a subtree that is negated
   * (c) for dis_WHEN, the first son is the condition and the next son
   * is the effect
   */
  struct _dis_PlNode *sons;

  /* if you have a list of sons, they are connected by next
   */
  struct _dis_PlNode *next;
  float value;

} dis_PlNode;

/* PDDL 3 */
typedef enum _dis_Connective_Con
{
  dis_ATOM_c,
//  dis_AND_c,
  dis_AND_c_c,
//  dis_ALL_c,
  dis_ALL_c_c,
  dis_AT_END_c,
  dis_ALWAYS_c,
  dis_SOMETIME_c,
  dis_WITHIN_c,
  dis_AT_MOST_ONCE_c,
  dis_SOMETIME_AFTER_c,
  dis_SOMETIME_BEFORE_c,
  dis_ALWAYS_WITHIN_c,
  dis_HOLD_DURING_c,
  dis_HOLD_AFTER_c  
} dis_Connective_Con;

typedef struct _dis_ConNode
{
  dis_Connective_Con connective;
  dis_TypedList *parse_vars;
  float number, number2;
  dis_PlNode *sons, *sons2;
  struct _dis_ConNode *sons_c;
  struct _dis_ConNode *next;
} dis_ConNode;

typedef struct _dis_PrefNode
{
  char *name, *opname, *pname;
  dis_ConNode *body;
  dis_TypedList *args;
  struct _dis_PrefNode *next;
} dis_PrefNode;

extern int dis_num_preference, dis_num_constraint;

/*
 * This resembles an uninstantiated PDDL operator
 */
typedef struct _dis_Pldis_Operator {

  char *name;

  /* only important for PDDL where :VARS may be added to the param list
   * which must be hidden when writing the plan to an output file
   */
  int number_of_real_params; 

  /* the params, as they are declared in domain file
   */
  dis_TypedList *parse_params;

  /* params is a list of variable/type pairs, such that:
   * factlist->item = [variable] -> [type]
   */
  dis_FactList *params;
  dis_PlNode *preconds;
  dis_PlNode *effects;

  /* durative actions */
  dis_PlNode *duration;
   
  struct _dis_Pldis_Operator *next;

} dis_Pldis_Operator;















/***************** 
 * INSTANTIATION *
 *****************/









/* helpers
 */

typedef int dis_TypeArray[dis_MAX_TYPE_INTERSECTIONS];

typedef int *dis_int_pointer;




/* first step structures: parsing & preprocessing
 */

typedef struct _dis_Fact {

  int predicate, args[dis_MAX_ARITY];

} dis_Fact;



typedef struct _dis_Fluent {

  int function, args[dis_MAX_ARITY];

} dis_Fluent;



typedef struct _dis_FluentValue {

  dis_Fluent fluent;
  float value;

} dis_FluentValue;



typedef struct _dis_Facts {

  dis_Fact *fact;

  struct _dis_Facts *next;

} dis_Facts;



typedef struct _dis_FluentValues {

  dis_Fluent fluent;
  float value;

  struct _dis_FluentValues *next;

} dis_FluentValues;



typedef struct _dis_ExpNode {

  dis_Expdis_Connective connective;

  /* in FHEAD nodes, pre-processing
   */
  dis_Fluent *fluent;
  /* in FHEAD nodes after pre-processes have finished.
   * (internal number of relevant fluent, or -1 if not
   * relevant)
   */
  int fl;
  /* helper for LNF: if that fl is multiplied, this is the
   * respective constant after pre-normalization.
   */
  float c;

  /* in NUMBER nodes
   */
  float value;

  /* in MINUS nodes
   */
  struct _dis_ExpNode *son;

  /* in all others
   */
  struct _dis_ExpNode *leftson, *rightson;

} dis_ExpNode, *dis_ExpNode_pointer;



typedef struct _dis_WffNode {

  dis_Connective connective;

  /* in dis_ALL/dis_EX s
   */
  int var, var_type;
  char *var_name;

  /* in dis_AND/dis_OR s
   */
  struct _dis_WffNode *sons;
  /* sons are doubly connected linear list
   */
  struct _dis_WffNode *next;
  struct _dis_WffNode *prev;

  /* in dis_ATOMs
   */
  dis_Fact *fact;
  /* after translation: mark dis_NOT-p s for efficiency
   */
  int dis_NOT_p;

  /* in dis_ALL/dis_EX/dis_NOT
   */
  struct _dis_WffNode *son;

  /* in dis_COMP
   */
  dis_Comparator comp;
  dis_ExpNode *lh, *rh;

  /* for expansion speedup
   */
  dis_Bool visited;

  /* no dis_WHEN s here... use Pl dis_Connectives anyway for simplicity
   */

} dis_WffNode, *dis_WffNode_pointer;



typedef struct _dis_Literal {

  dis_Bool negated;

  dis_Fact fact;

  struct _dis_Literal *next;
  struct _dis_Literal *prev;

} dis_Literal;



typedef struct _dis_Numericdis_Effect {

  dis_Fluent fluent;
  dis_Numericdis_EffectType neft;

  dis_ExpNode *rh;

  struct _dis_Numericdis_Effect *next;
  struct _dis_Numericdis_Effect *prev;

} dis_Numericdis_Effect;



typedef struct _dis_Effect {

  int num_vars, var_types[dis_MAX_VARS];
  char *var_names[dis_MAX_VARS];

  dis_WffNode *conditions;

  dis_Literal *effects;
  dis_Numericdis_Effect *numeric_effects;

  struct _dis_Effect *next;
  struct _dis_Effect *prev;

} dis_Effect;



typedef struct _dis_Operator {

  char *name, *var_names[dis_MAX_VARS];
  int number_of_real_params; 

  int num_vars, var_types[dis_MAX_VARS];
  dis_Bool removed[dis_MAX_VARS];
 
  dis_WffNode *preconds;

  dis_Effect *effects;

  dis_Bool hard;

} dis_Operator, *dis_Operator_pointer;


/* timed initial literals */
typedef struct _dis_TimedInitial {
        float time;
        dis_Bool negated;
        dis_PlNode *literal;
        dis_Fact pred;
} dis_TimedInitial;
    
extern int dis_gnum_tils;
extern dis_TimedInitial *dis_gtils;

/* second step: structures that keep already normalized
 * operators
 */




typedef struct _Normdis_Effect {

  int num_vars, var_types[dis_MAX_VARS];
  int inst_table[dis_MAX_VARS];

  dis_Fact *conditions;
  int num_conditions;

  dis_Fact *adds;
  int num_adds;
  dis_Fact *dels;
  int num_dels;

  /* numerical parts: not yet normalized any further; seems that
   * normalizing requires certain additional structures +
   * transformation, and that these will better be done when 
   * the representation is fully instantiated already.
   */
  dis_Comparator *numeric_conditions_comp;
  dis_ExpNode_pointer *numeric_conditions_lh, *numeric_conditions_rh;
  int num_numeric_conditions;

  dis_Numericdis_EffectType *numeric_effects_neft;
  dis_Fluent *numeric_effects_fluent;
  dis_ExpNode_pointer *numeric_effects_rh;
  int num_numeric_effects;

  struct _Normdis_Effect *prev;
  struct _Normdis_Effect *next;

} Normdis_Effect;



typedef struct _Normdis_Operator {
  
  dis_Operator *operator;

  int num_vars, var_types[dis_MAX_VARS];
  int inst_table[dis_MAX_VARS];
  int removed_vars[dis_MAX_VARS], num_removed_vars, type_removed_vars[dis_MAX_VARS];

  dis_Fact *preconds;
  int num_preconds;
  /* numeric precondition still full scale represented, see above
   */
  dis_Comparator *numeric_preconds_comp;
  dis_ExpNode_pointer *numeric_preconds_lh, *numeric_preconds_rh;
  int num_numeric_preconds;

  Normdis_Effect *effects;

  dis_Bool out;

} Normdis_Operator, *Normdis_Operator_pointer;
  


/* minimal info for a fully instantiated easy operator;
 * yields one action when expanded
 */
typedef struct _dis_EasyTemplate {

  Normdis_Operator *op;
  int inst_table[dis_MAX_VARS];

  struct _dis_EasyTemplate *prev;
  struct _dis_EasyTemplate *next;

} dis_EasyTemplate;






/* structures for hard ops
 */





/* intermediate step: structure for keeping hard ops
 * with normalized precondition, but arbitrary
 * effect conditions
 */
typedef struct _Mixeddis_Operator {
  
  dis_Operator *operator;

  int inst_table[dis_MAX_VARS];

  dis_Fact *preconds;
  int num_preconds;
  /* numeric part, pre-normalized
   */
  dis_Comparator *numeric_preconds_comp;
  dis_ExpNode_pointer *numeric_preconds_lh, *numeric_preconds_rh;
  int num_numeric_preconds;

  dis_Effect *effects;

  struct _Mixeddis_Operator *next;

} Mixeddis_Operator;



/* last hard step: everything is action - like, except that
 * facts are not yet integer coded
 */  



typedef struct _dis_Pseudodis_Actiondis_Effect {

  dis_Fact *conditions;
  int num_conditions;

  dis_Fact *adds;
  int num_adds;
  dis_Fact *dels;
  int num_dels;


  /* and the numeric parts again...
   */
  dis_Comparator *numeric_conditions_comp;
  dis_ExpNode_pointer *numeric_conditions_lh, *numeric_conditions_rh;
  int num_numeric_conditions;

  dis_Numericdis_EffectType *numeric_effects_neft;
  dis_Fluent *numeric_effects_fluent;
  dis_ExpNode_pointer *numeric_effects_rh;
  int num_numeric_effects;

  struct _dis_Pseudodis_Actiondis_Effect *next;

} dis_Pseudodis_Actiondis_Effect;



typedef struct _dis_Pseudodis_Action {

  dis_Operator *operator;

  int inst_table[dis_MAX_VARS];

  dis_Fact *preconds;
  int num_preconds;
  /* numeric part, pre-normalized
   */
  dis_Comparator *numeric_preconds_comp;
  dis_ExpNode_pointer *numeric_preconds_lh, *numeric_preconds_rh;
  int num_numeric_preconds;

  dis_Pseudodis_Actiondis_Effect *effects;
  int num_effects;

} dis_Pseudodis_Action, *dis_Pseudodis_Action_pointer;




/* final domain representation structure
 */


// Chih-Wei PDDL3
typedef struct _Lnfdis_ExpNode {

  int *pF;
  float *pC;
  int num_pF;

  int *nF;
  float *nC;
  int num_nF;

  float c;

} Lnfdis_ExpNode, *Lnfdis_ExpNode_pointer;



typedef struct _dis_Actiondis_Effect {

  int *conditions;
  int num_conditions;

  int *adds;
  int num_adds;
  int *dels;
  int num_dels;

  /* and the numeric parts again; fluents all as fl ints;
   *
   * normalization for cond as below for pre;
   * norm. for effects by restriction of types (?),
   * right hand side float (?)
   */
  dis_Comparator *numeric_conditions_comp;
  dis_ExpNode_pointer *numeric_conditions_lh, *numeric_conditions_rh;
  int num_numeric_conditions;

  dis_Numericdis_EffectType *numeric_effects_neft;
  int *numeric_effects_fl;
  dis_ExpNode_pointer *numeric_effects_rh;
  int num_numeric_effects;

  /* LNF
   */
  dis_Comparator *lnf_conditions_comp;
  Lnfdis_ExpNode_pointer *lnf_conditions_lh;
  float *lnf_conditions_rh;
  int num_lnf_conditions;  

  dis_Numericdis_EffectType *lnf_effects_neft;
  int *lnf_effects_fl;
  Lnfdis_ExpNode_pointer *lnf_effects_rh;
  int num_lnf_effects;

  /* this is true iff the numerical part of the effects affects or accesses
   * an undefined fluent (i.e. in numeric_effects_fl or numeric_effects_rh ) 
   * --- then, if the effect appears, the action is
   * illegal.
   */
  dis_Bool illegal;

  /* helper
   */
  dis_Bool removed;

  float cost;

} dis_Actiondis_Effect;



typedef struct _dis_Action {
  
    char *name;

  Normdis_Operator *norm_operator;
  dis_Pseudodis_Action *pseudo_action;

  int num_name_vars;
  int name_inst_table[dis_MAX_VARS];

  int inst_table[dis_MAX_VARS];

  int *preconds;
  int num_preconds;
  
  dis_Actiondis_Effect *effects;
  int num_effects;

  struct _dis_Action *next;

  /* numeric part, in general format, with fluents encoded as fl ints
   *
   * also, will (?) be transformed to lh fl, rh float; then, expnodes as
   * fast accessible as specialised structures. 
   */
  dis_Comparator *numeric_preconds_comp;
  dis_ExpNode_pointer *numeric_preconds_lh, *numeric_preconds_rh;
  int num_numeric_preconds;

  /* LNF
   */
  dis_Comparator *lnf_preconds_comp;
  Lnfdis_ExpNode_pointer *lnf_preconds_lh;
  float *lnf_preconds_rh;
  int num_lnf_preconds;

} dis_Action;











/*****************************************************
 * BASIC OP dis_AND FT Sdis_TRUCTURES Fdis_OR CONNECTIVITY GRAPH *
 *****************************************************/











typedef struct _dis_OpConn {

  /* to get name
   */
  dis_Action *action;

  /* effects
   */
  int *E;
  int num_E;

  /* member for applicable actions extraction
   */
  dis_Bool is_in_A;

  /* members for 1Ph - H(S) extraction
   */
  int is_used;
  dis_Bool is_in_H;
  
  dis_Bool dummy;
} dis_OpConn;



typedef struct _dis_EfConn {

  int op;

  /* true if access to always undefined fluent, or
   * conflicting assignments.
   *
   * if that is the case then nothing except condition is set:
   * the effect is completely ignored except that
   * it renders the op unapplicable when its condition
   * is true.
   */
  dis_Bool illegal;

  /* this one means we found in conn that it is useless (empty)
   */
  dis_Bool removed;

  /* this is the cost; can be non-zero if a metric was specified
   * and established
   */
  float cost;
  /* durative actions */  
  float duration;
  
  int *PC;
  int num_PC;
  /* numeric part
   */
  dis_Comparator *f_PC_comp; /* either GEQ or GE */
  int *f_PC_fl;
  float *f_PC_c;
  int num_f_PC;
  /* array indexed by fl number, to fast know whether
   * new fluent value is high enough
   */
  dis_Comparator *f_PC_direct_comp;
  float *f_PC_direct_c;

  /* logic effects
   */
  int *A;
  int num_A;
  int *D;
  int num_D;
  /* and the numeric ones; fl_ is the encoding of the LNF
   * on the right hand side, without constant part
   * (special treatment for that as it's supposed
   *  to be the most common thing!!)
   */
  int *IN_fl;
  int *IN_fl_;
  float *IN_c;
  int num_IN;

  int *AS_fl;
  int *AS_fl_;
  float *AS_c;
  int num_AS;

  /* implied effects
   */
  int *I;
  int num_I;

  /* members for relaxed fixpoint computation
   */
  int level;
  dis_Bool in_E;
  int num_active_PCs;
  dis_Bool ch;

  /* RPG
   */
  int num_active_f_PCs;

  /* 1P; an effect can be selected several times
   * for increasing a fluent.
   */
  int in_plan;

  // Y. Chen
  dis_Bool DPop;
 
  int num_pcc; 

  int red_level;
} dis_EfConn;



typedef struct _dis_FtConn {

  /* effects it is union conds, pres element of
   */
  int *PC;
  int num_PC;

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

  /* members for relaxed fixpoint computation
   */
  int level;
  dis_Bool in_F;

  /* members for 1Ph extraction
   */
  int is_goal;
  int is_true;
  dis_Bool ch;

  /* search
   */
  int rand;/* for hashing */

  //Y.Chen
  int lpg;
    
  dis_Bool DPft;
 
} dis_FtConn;



typedef struct _dis_FlConn {

  /* effects it is union conds, pres required
   */
  int *PC;
  int num_PC;

  /* efs that inc, ass it and by which encoded fluents and constants
   */
  int *IN;
  int *IN_fl_;
  float *IN_c;
  int num_IN;

  int *AS;
  int *AS_fl_;
  float *AS_c;/* see above */
  int num_AS;

  /* is it an artificial fluent?
   */
  dis_Bool artificial;
  /* if so, then this here is the linear equation
   * it stands for
   */
  int *lnf_F;
  float *lnf_C;
  int num_lnf;

  /* the termination criterion for RPG building is based on mneed, see
   * JAIR article for definition;
   *
   * as the name suggests, we use the bool to indicate that this one is not
   * needed at all
   */
  dis_Bool mneed_is_minusinfty;
  float mneed;
  /* see JAIR; shortcut for never needed at all.
   */
  dis_Bool relevant;
  
  /* this tells us when there's no more point adding to the
   * fluent's value in RPG building
   *
   * (i.e. the max over all prec and goal requirements)
   */
  dis_Comparator max_needed_comp;
  float max_needed_c;

  /* these two tell us whether and how the fluent is required to make a rh
   * side good. needed for RPG termination. the fluent is needed in the rh
   * if:
   * 1. an IN eff is fl' += fl + c where c < 0, and max_needed_comp(fl') neq IGUAL; 
   *    needed amount: (>) |c|
   * 2. an AS eff is fl' := fl + c, and max_needed_comp(fl') neq IGUAL;
   *    needed amount: (>) max_needed_c(fl') - c
   *
   * we can only terminate RPG if all fl's that increase have reached their
   * max_need dis_AND the max_rh_need; otherwise, they might still need to be 
   * increased in order to help others!
   *
   * this way, sufficient because if all > requ met then other vars inmprove
   * sufficiently; necessary because an increasing var will eventually meet
   * the > constraints.
   */
  dis_Bool is_rh_needed;
  float max_rh_needed;

  /* the following are members handled within heuristic algorithms.
   */

  /* this are arrays saying what the max value at 
   * the levels in the RPG is, resp. whether the value
   * can be defined there at all, resp. what the increasers
   * at that level have added.
   */
  dis_Bool *def;
  float *level;

  /* for handling assigners in RPG: is an assigner in there yet,
   * and if so what is their max value?
   */
  dis_Bool curr_assigned;
  float curr_max_assigned;

  int rand;/* for hashing */
} dis_FlConn;












/****************************
 * Sdis_TRUCTURES Fdis_OR SEARCHING *
 ****************************/









typedef struct _dis_State {
  
  int *F;
  int num_F;

  dis_Bool *f_D;
  float *f_V;

} dis_State, *dis_State_pointer;



typedef struct _dis_EhcNode {
  
  dis_State S;

  int op;
  int depth;

  struct _dis_EhcNode *father;
  struct _dis_EhcNode *next;

} dis_EhcNode;



typedef struct _dis_EhcHashEntry {

  int sum;

  dis_EhcNode *ehc_node;

  struct _dis_EhcHashEntry *next;

} dis_EhcHashEntry, *dis_EhcHashEntry_pointer;



typedef struct _dis_PlanHashEntry {

  int sum;
  dis_State S;

  /* step is number of op that is dis_EXECUTED in S;
   * -1 means that this state is no longer contained in plan
   */
  int step;
  struct _dis_PlanHashEntry *next_step;

  struct _dis_PlanHashEntry *next;

} dis_PlanHashEntry, *dis_PlanHashEntry_pointer;



typedef struct _dis_BfsNode {
  
  dis_State S;

  int op;
  int g;

  /* h is plain heuristic relaxed plan steps (needed for goal state recognition)
   * int_fn is optimized function value in plansteps case
   * float_fn is optimized function value in expression case
   */
  int h;
  int int_fn;
  float float_fn;

  int *H;
  int num_H;

  struct _dis_BfsNode *father;

  struct _dis_BfsNode *next;
  struct _dis_BfsNode *prev;

} dis_BfsNode;



typedef struct _dis_BfsHashEntry {

  int sum;

  dis_BfsNode *bfs_node;

  struct _dis_BfsHashEntry *next;

} dis_BfsHashEntry, *dis_BfsHashEntry_pointer;













/*
 *  -------------------------------- MAIN FN HEADERS ----------------------------
 */













void dis_print_official_result( void );/* AIPS 2002 output routine */
void dis_print_official_op_name( int index );




void dis_output_planner_info( void );
void dis_ff_usage( void );
dis_Bool dis_processdis__command_line( int argc, char *argv[] );









/*
 *  ----------------------------- GLOBAL VARIABLES ----------------------------
 */












/*******************
 * GENERAL HELPERS *
 *******************/










/* used to time the different stages of the planner
 */
extern float dis_gtempl_time, dis_greach_time, dis_grelev_time, dis_gconn_time;
extern float dis_gLNF_time, dis_gsearch_time;

/* the command line inputs
 */
extern struct dis__command_line dis_gcmd_line;

/* number of states that got heuristically evaluated
 */
extern int dis_gevaluated_states;

/* maximal depth of breadth first search
 */
extern int dis_gmax_search_depth;









/***********
 * PARSING *
 ***********/











/* used for pddl parsing, flex only allows global variables
 */
extern int dis_gbracket_count;
extern char *dis_gproblem_name;

/* The current input line number
 */
extern int dis_lineno;

/* The current input filename
 */
extern char *dis_gact_filename;

/* The pddl domain name
 */
extern char *dis_gdomain_name;

/* loaded, uninstantiated operators
 */
extern dis_Pldis_Operator *dis_gloaded_ops;

/* PDDL 3 */
extern dis_ConNode *dis_gloaded_constraints;
extern dis_PrefNode *dis_gloaded_preferences;

/* stores initials as fact_list 
 */
extern dis_PlNode *dis_gorig_initial_facts;

/* not yet preprocessed goal facts
 */
extern dis_PlNode *dis_gorig_goal_facts;

/* the types, as defined in the domain file
 */
extern dis_TypedList *dis_gparse_types;

/* the constants, as defined in domain file
 */
extern dis_TypedList *dis_gparse_constants;

/* the predicates and their arg types, as defined in the domain file
 */
extern dis_TypedListList *dis_gparse_predicates;

/* the functions and their arg types, as defined in the domain file
 */
extern dis_TypedListList *dis_gparse_functions;

/* the objects, declared in the problem file
 */
extern dis_TypedList *dis_gparse_objects;

/* the metric
 */
extern dis_Token dis_gparse_optimization;
extern dis_Parsedis_ExpNode *dis_gparse_metric;


/* connection to instantiation ( except ops, goal, initial )
 */

/* all typed objects 
 */
extern dis_FactList *dis_gorig_constant_list;

/* the predicates and their types
 */
extern dis_FactList *dis_gpredicates_and_types;

/* the functions and their types
 */
extern dis_FactList *dis_gfunctions_and_types;














/*****************
 * INSTANTIATING *
 *****************/










/* global arrays of constant names,
 *               type names (with their constants),
 *               predicate names,
 *               predicate aritys,
 *               defined types of predicate args
 */
extern dis_Token dis_gconstants[dis_MAX_CONSTANTS];
extern int dis_gnum_constants;
extern dis_Token dis_gtype_names[dis_MAX_TYPES];
extern int dis_gtype_consts[dis_MAX_TYPES][dis_MAX_TYPE];
extern dis_Bool dis_gis_member[dis_MAX_CONSTANTS][dis_MAX_TYPES];
extern int dis_gtype_size[dis_MAX_TYPES];
extern int dis_gnum_types;
extern dis_Token dis_gpredicates[dis_MAX_PREDICATES];
extern int dis_garity[dis_MAX_PREDICATES];
extern int dis_gpredicates_args_type[dis_MAX_PREDICATES][dis_MAX_ARITY];
extern int dis_gnum_predicates;
extern dis_Token dis_gfunctions[dis_MAX_FUNCTIONS];
extern int dis_gf_arity[dis_MAX_FUNCTIONS];
extern int dis_gfunctions_args_type[dis_MAX_FUNCTIONS][dis_MAX_ARITY];
extern int dis_gnum_functions;
/* derived predicates */         
extern int dis_gnum_deripreds;
/* durative actions */
typedef struct _DuraTab
{
    char name[100];
    dis_Fluent fluent;
    int flag;  
} DuraTab; 
extern DuraTab *table;
extern int gnum_das;




/* the domain in first step integer representation
 */
//extern dis_Operator_pointer dis_goperators[dis_MAX_OPERATdis_ORS];
extern dis_Operator_pointer *dis_goperators;
extern int dis_gnum_operators;
extern dis_Fact *dis_gfull_initial;
extern int dis_gnum_full_initial;
extern dis_FluentValue *dis_gfull_fluents_initial;
extern int dis_gnum_full_fluents_initial;
extern dis_WffNode *dis_ggoal;

extern dis_ExpNode *dis_gmetric;



/* stores inertia - information: is any occurence of the predicate
 * added / deleted in the uninstantiated ops ?
 */
extern dis_Bool dis_gis_added[dis_MAX_PREDICATES];
extern dis_Bool dis_gis_deleted[dis_MAX_PREDICATES];

/* for functions we *might* want to say, symmetrically, whether it is
 * increased resp. decreased at all.
 *
 * that is, however, somewhat involved because the right hand
 * sides can be arbirtray expressions, so we have no guarantee
 * that increasing really does adds to a functions value...
 *
 * thus (for the time being), we settle for "is the function changed at all?"
 */
extern dis_Bool dis_gis_changed[dis_MAX_FUNCTIONS];



/* splitted initial state:
 * initial non static facts,
 * initial static facts, divided into predicates
 * (will be two dimensional array, allocated directly before need)
 */
extern dis_Facts *dis_ginitial;
extern int dis_gnum_initial;
extern dis_Fact **dis_ginitial_predicate;
extern int *dis_gnum_initial_predicate;

/* same thing for functions
 */
extern dis_FluentValues *dis_gf_initial;
extern int dis_gnum_f_initial;
extern dis_FluentValue **dis_ginitial_function;
extern int *dis_gnum_initial_function;



/* the type numbers corresponding to any unary inertia
 */
extern int *dis_gtype_to_predicate;
//extern int dis_gtype_to_predicate[dis_MAX_PREDICATES];
extern int dis_gpredicate_to_type[dis_MAX_TYPES];

/* (ordered) numbers of types that new type is intersection of
 */
extern dis_TypeArray dis_gintersected_types[dis_MAX_TYPES];
extern int dis_gnum_intersected_types[dis_MAX_TYPES];



/* splitted domain: hard n easy ops
 */
extern dis_Operator_pointer *dis_ghard_operators;
extern int dis_gnum_hard_operators;
extern Normdis_Operator_pointer *dis_geasy_operators;
extern int dis_gnum_easy_operators;



/* so called Templates for easy ops: possible inertia constrained
 * instantiation constants
 */
extern dis_EasyTemplate *dis_geasy_templates;
extern int dis_gnum_easy_templates;



/* first step for hard ops: create mixed operators, with conjunctive
 * precondition and arbitrary effects
 */
extern Mixeddis_Operator *dis_ghard_mixed_operators;
extern int dis_gnum_hard_mixed_operators;



/* hard ''templates'' : pseudo actions
 */
extern dis_Pseudodis_Action_pointer *dis_ghard_templates;
extern int dis_gnum_hard_templates;



/* store the final "relevant facts"
 */
extern dis_Fact *dis_grelevant_facts;
//extern dis_Fact dis_grelevant_facts[dis_MAX_RELEVANT_FACTS];
extern int dis_gnum_relevant_facts;
extern int dis_gnum_pp_facts;
/* store the "relevant fluents"
 */
extern dis_Fluent *dis_grelevant_fluents;
//extern dis_Fluent dis_grelevant_fluents[dis_MAX_RELEVANT_FLUENTS];
extern int dis_gnum_relevant_fluents;
extern dis_Token *dis_grelevant_fluents_name;
//extern dis_Token dis_grelevant_fluents_name[dis_MAX_RELEVANT_FLUENTS];
/* this is NULL for normal, and the LNF for
 * artificial fluents.
 */
extern Lnfdis_ExpNode_pointer *dis_grelevant_fluents_lnf;
//extern Lnfdis_ExpNode_pointer dis_grelevant_fluents_lnf[dis_MAX_RELEVANT_FLUENTS];



/* the final actions and problem representation
 */
extern dis_Action *dis_gactions;
extern int dis_gnum_actions;
extern dis_State dis_ginitial_state;
extern int *dis_glogic_goal;
extern int dis_gnum_logic_goal;
extern dis_Comparator *dis_gnumeric_goal_comp;
extern dis_ExpNode_pointer *dis_gnumeric_goal_lh, *dis_gnumeric_goal_rh;
extern int dis_gnum_numeric_goal;



/* to avoid memory leaks; too complicated to identify
 * the exact state of the action to throw away (during construction),
 * memory gain not worth the implementation effort.
 */
extern dis_Action *dis_gtrash_actions;



/* additional lnf step between finalized inst and
 * conn graph
 */
extern dis_Comparator *dis_glnf_goal_comp;
extern Lnfdis_ExpNode_pointer *dis_glnf_goal_lh;
extern float *dis_glnf_goal_rh;
extern int dis_gnum_lnf_goal;

extern Lnfdis_ExpNode dis_glnf_metric;
extern dis_Bool dis_goptimization_established;



/**********************
 * CONNECTIVITY GRAPH *
 **********************/





/* one ops (actions) array ...
 */
extern dis_OpConn *dis_gop_conn;
extern int dis_gnum_op_conn;



/* one effects array ...
 */
extern dis_EfConn *dis_gef_conn;
extern int dis_gnum_ef_conn;



/* one facts array.
 */
extern dis_FtConn *dis_gft_conn;
extern int dis_gnum_ft_conn;



/* and: one fluents array.
 */
extern dis_FlConn *dis_gfl_conn;
extern int dis_gnum_fl_conn;
extern int dis_gnum_real_fl_conn;/* number of non-artificial ones */



/* final goal is also transformed one more step.
 */
extern int *dis_gflogic_goal;
extern int dis_gnum_flogic_goal;
extern dis_Comparator *dis_gfnumeric_goal_comp;
extern int *dis_gfnumeric_goal_fl;
extern float *dis_gfnumeric_goal_c;
extern int dis_gnum_fnumeric_goal;

/* direct access (by relevant fluents)
 */
extern dis_Comparator *dis_gfnumeric_goal_direct_comp;
extern float *dis_gfnumeric_goal_direct_c;













/*******************
 * SEARCHING NEEDS *
 *******************/












/* applicable actions
 */
extern int *dis_gA;
extern int dis_gnum_A;



/* communication from extract 1.P. to search engine:
 * 1P action choice
 */
extern int *dis_gH;
extern int dis_gnum_H;
/* cost of relaxed plan
 */
extern float dis_gcost;



/* to store plan
 */
extern int dis_gplan_ops[dis_MAX_PLAN_LENGTH];
extern int dis_gnum_plan_ops;



/* stores the states that the current plan goes through
 */
extern dis_State dis_gplan_states[dis_MAX_PLAN_LENGTH + 1];



/* dirty: multiplic. of total-time in final metric LNF
 */
extern float dis_gtt;

/* Chih-Wei */
extern int **const_index;


/* Y. Chen new
 */
extern dis_State dis_mff_sol;
extern dis_State dis_known_iga_list;
extern int* work_ft;
extern int* work_op;
extern dis_State work_S0;
extern dis_State work_S1;
extern dis_State dis_gtrue_ft;
extern short dis_red_space; 
extern int max_bfs_iter;
extern int max_hc_iter;
extern int bridge_option;
extern dis_Bool hc_max_exceeded;

extern int *saved_dis_gflogic_goal;
extern int saved_dis_gnum_flogic_goal;
extern int saved_dis_gnum_fnumeric_goal;
extern dis_Comparator *saved_dis_gfnumeric_goal_comp;
extern int *saved_dis_gfnumeric_goal_fl;
extern float *saved_dis_gfnumeric_goal_c;

extern int SymmLagrangian;
extern int SymmLagrange;

extern struct tms dis_dist_start, dis_dist_end;
  
/* the mneed structures
 *
 * assign propagation pairs i, j, and transitive such pairs.
 */
extern dis_Bool **dis_gassign_influence;
extern dis_Bool **dis_gTassign_influence;



/* the real var input to the mneed computation.
 */
extern dis_Bool *dis_gmneed_start_D;
extern float *dis_gmneed_start_V;   



/* does this contain conditional effects?
 * (if it does then the state hashing has to be made more
 *  cautiously)
 */
extern dis_Bool dis_gconditional_effects;

#endif 
