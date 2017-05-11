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

 * File: scan-ops_pddl.y

 * Description: modified from scan-ops_pddl.y in Metric-FF

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

%{
//#define YYDEBUG 1
#ifdef YYDEBUG
  extern int yydebug=1;
#endif


#include <stdio.h>
#include <string.h> 
#include "dis_ff.h"
#include "dis_memory.h"
#include "dis_parse.h"
#include "lpg.h"
#include "dis_constraints.h"

#ifndef SCAN_ERR
#define SCAN_ERR
#define DOMDEF_dis_EXPECTED            0
#define DOMAIN_dis_EXPECTED            1
#define DOMNAME_dis_EXPECTED           2
#define LBRACKET_dis_EXPECTED          3
#define RBRACKET_dis_EXPECTED          4
#define DOMDEFS_dis_EXPECTED           5
#define REQUIREM_dis_EXPECTED          6
#define TYPEDLIST_dis_EXPECTED         7
#define LITERAL_dis_EXPECTED           8
#define PRECONDDEF_UNCdis_ORRECT       9
#define TYPEDEF_dis_EXPECTED          10
#define CONSTLIST_dis_EXPECTED        11
#define PREDDEF_dis_EXPECTED          12 
#define NAME_dis_EXPECTED             13
#define VARIABLE_dis_EXPECTED         14
#define ACTIONFUNCTdis_OR_dis_EXPECTED    15
#define dis_ATOM_Fdis_ORMULA_dis_EXPECTED     16
#define EFFECT_DEF_dis_EXPECTED       17
#define NEG_Fdis_ORMULA_dis_EXPECTED      18
#define dis_NOT_SUPPdis_ORTED             19
#define ACTION                    20
#define CONSTRAINTLIST_EXPECTED	  21
#endif


#define dis_NAME_STR "name\0"
#define dis_VARIABLE_STR "variable\0"
#define dis_STdis_ANDARD_TYPE "OBJECT\0"
 

static char *serrmsg[] = {
  "domain definition expected",
  "'domain' expected",
  "domain name expected",
  "'(' expected",
  "')' expected",
  "additional domain definitions expected",
  "requirements (e.g. ':STRIPS') expected",
  "typed list of <%s> expected",
  "literal expected",
  "uncorrect precondition definition",
  "type definition expected",
  "list of constants expected",
  "predicate definition expected",
  "<name> expected",
  "<variable> expected",
  "action functor expected",
  "atomic formula expected",
  "effect definition expected",
  "negated atomic formula expected",
  "requirement %s not dis_supported by this FF version",  
  "action definition is not correct",
  "constraint definition is not correct",
  NULL
};


void dis_opserr( int errno, char *par );


static int sact_err;
static char *sact_err_par = NULL;
static dis_Pldis_Operator *scur_op = NULL, *scur_op1 = NULL;
static dis_Bool sis_negated = dis_FALSE;
static char *preference_pointer = NULL;
static char *preference_pointer1 = NULL;
static dis_TypedList *forall_tl = NULL;
static dis_Parsedis_ExpNode *duration_exp = NULL;

void dis_opserr( int errno, char *par )

{

  sact_err = errno;

  if ( sact_err_par ) {
    free(sact_err_par);
  }
  if ( par ) {
    sact_err_par = dis_new_dis_Token(strlen(par)+1);
    strcpy(sact_err_par, par);
  } else {
    sact_err_par = NULL;
  }

}

int dis_supported( char *str )

{

  int i;
	/* derived predicates */
	/* timed initial literals */
  char * sup[] = { ":STRIPS", ":TYPING", ":NEGATIVE-PRECONDITIONS",
		   ":DISJUNCTIVE-PRECONDITIONS", ":EQUALITY", 
		   ":EXISTENTIAL-PRECONDITIONS", ":UNIVERSAL-PRECONDITIONS", 
		   ":QUANTIFIED-PRECONDITIONS", ":CONDITIONAL-EFFECTS", 
		   ":FLUENTS", ":ADL", ":DURATIVE-ACTIONS", ":DURATION-INEQUALITIES", 
		   ":DERIVED-PREDICATES", ":TIMED-INITIAL-LITERALS", /* PDDL2.2 */
		   ":PREFERENCES", 
//		   ":CONSTRAINTS", /* PDDL3.0 */
		   ":NUMERIC-FLUENTS", ":ACTION-COSTS", ":GOAL-UTILITIES", /* PDDL3.1 */
//		   ":OBJECT-FLUENTS", 
		   NULL};    

  for (i=0; NULL != sup[i]; i++) {
    if ( SAME == strcmp(sup[i], str) ) {
	switch(i)
	{
	case 19:
		GpG.is_goal_utilities = TRUE;
		break;
	case 18:
		GpG.is_action_costs = TRUE;
		break;
	}
      return dis_TRUE;
    }
  }
  return dis_FALSE;

}

%}


%start file


%union {

  char string[dis_MAX_LENGTH];
  char *pstring;
  dis_Parsedis_ExpNode *pdis_Parsedis_ExpNode;
  dis_PlNode *pdis_PlNode;
  dis_FactList *pdis_FactList;
  dis_TokenList *pdis_TokenList;
  dis_TypedList *pdis_TypedList;
  /* PDDL3.1 */
  dis_TypedListList *pdis_TypedListList;  
}


%type <pdis_PlNode> adl_effect
%type <pdis_PlNode> adl_effect_star
%type <pdis_PlNode> adl_goal_description
%type <pdis_PlNode> adl_goal_description1
%type <pdis_PlNode> adl_goal_description_star
%type <pdis_Parsedis_ExpNode> f_head
%type <pdis_Parsedis_ExpNode> f_exp
%type <pdis_TokenList> literal_term
%type <pdis_TokenList> term_star
%type <pdis_TypedList> typed_list_name
%type <pdis_TypedList> typed_list_variable
%type <pstring> term
%type <pdis_TokenList> atomic_formula_term
%type <pdis_TokenList> name_plus
%type <pstring> predicate
/* durative actions */
%type <pdis_PlNode> da_adl_effect
%type <pdis_PlNode> timed_adl_effect
%type <pdis_PlNode> timed_adl_effect_plus
%type <pdis_PlNode> timed_adl_goal_description
%type <pdis_PlNode> timed_adl_goal_description_plus
%type <pdis_PlNode> da_adl_goal_description
%type <pdis_PlNode> duration_constraint
%type <pdis_PlNode> f_assign_da
%type <pdis_Parsedis_ExpNode> f_exp_da
/*PDDL3--*/
%type <pdis_PlNode> pre_GD
%type <pdis_PlNode> pre_GD_star
%type <pdis_PlNode> pref_GD
%type <pdis_PlNode> da_GD
%type <pdis_PlNode> da_GD_star
%type <pdis_PlNode> pref_timed_GD
/*--PDDL3*/
/* PDDL3.1 */
%type <pdis_TypedListList> atomic_function_skeleton_plus

%token DEFINE_TOK
%token DOMAIN_TOK
%token REQUIREMENTS_TOK
%token TYPES_TOK
%token EITHER_TOK
%token CONSTANTS_TOK
%token PREDICATES_TOK
%token FUNCTIONS_TOK
%token ACTION_TOK
%token VARS_TOK
%token IMPLIES_TOK
%token PRECONDITION_TOK
%token PARAMETERS_TOK
%token EFFECT_TOK
%token dis_AND_TOK
%token dis_NOT_TOK
%token dis_WHEN_TOK
%token Fdis_ORdis_ALL_TOK
%token IMPLY_TOK
%token dis_OR_TOK
%token dis_EXISTS_TOK
%token LE_TOK
%token LEQ_TOK
%token EQ_TOK
%token GEQ_TOK
%token GE_TOK
%token MINUS_TOK
%token AD_TOK
%token MU_TOK
%token DI_TOK
%token ASSIGN_TOK
%token SCALE_UP_TOK
%token SCALE_DOWN_TOK
%token INCREASE_TOK
%token DECREASE_TOK
%token <string> NAME
%token <string> VARIABLE
%token <string> NUM
%token OPEN_PAREN
%token CLOSE_PAREN
/* durative actions */
%token AT_START
%token AT_END
%token OVER_ALL
%token DURATION_TOK
%token DURATIVE_ACTION_TOK
%token DURATION_VAR_TOK
%token CONDITION_TOK
%token TIME_TOK
/* derived predicates */
%token DERIVED_TOK

%left MINUS_TOK PLUS_TOK    
%left MUL_TOK DIV_TOK
%left UMINUS

%%

/**********************************************************************/
file:
{ 
  dis_opserr( DOMDEF_dis_EXPECTED, NULL ); 
}
domain_definition 
;
/* can be extended to support 'addenda' and similar stuff */


/**********************************************************************/
domain_definition : 
OPEN_PAREN  DEFINE_TOK  domain_name       
{ 
}
optional_domain_defs 
{
  if ( dis_gcmd_line.display_info >= 1 ) {
    /*printf("\ndomain '%s' defined\n", dis_gdomain_name);*/
  } 
}
;


/**********************************************************************/
domain_name :
OPEN_PAREN  DOMAIN_TOK  NAME  CLOSE_PAREN 
{ 
  dis_gdomain_name = dis_copy_dis_Token($3);
}
;


/**********************************************************************/
optional_domain_defs:
CLOSE_PAREN  /* end of domain */
|
require_def  optional_domain_defs
|
constants_def  optional_domain_defs
|
types_def  optional_domain_defs
|
predicates_def  optional_domain_defs
|
functions_def  optional_domain_defs
|
action_def  optional_domain_defs
/* durative actions */
|
durative_action_def optional_domain_defs
/* derived predicates */
|
derived_def optional_domain_defs
;


/**********************************************************************/
predicates_def :
OPEN_PAREN PREDICATES_TOK  predicates_list 
{
}
CLOSE_PAREN
{ 
}
;
/**********************************************************************/
predicates_list :
/* empty = finished */
{}
|
OPEN_PAREN  NAME typed_list_variable  CLOSE_PAREN
{

  dis_TypedListList *tll;

  if ( dis_gparse_predicates ) {
    tll = dis_gparse_predicates;
    while ( tll->next ) {
      tll = tll->next;
    }
    tll->next = dis_new_dis_TypedListList();
    tll = tll->next;
  } else {
    tll = dis_new_dis_TypedListList();
    dis_gparse_predicates = tll;
  }

  tll->predicate = dis_copy_dis_Token($2);

  tll->args = $3;

}
predicates_list
;


/**********************************************************************/
functions_def :
OPEN_PAREN FUNCTIONS_TOK  functions_list 
{
}
CLOSE_PAREN
{ 
}
;

/**********************************************************************/

functions_list :
atomic_function_skeleton_plus MINUS_TOK NAME functions_list
{
  dis_TypedListList *tll;

  if (strcmp($3, "NUMBER"))
    exit(1);
  else
  if ($1->next != NULL || strcmp($1->predicate, "TOTAL-COST")) // rule out action costs
    GpG.is_numeric_fluents = TRUE;
  
  if ( dis_gparse_functions ) 
  {
    tll = dis_gparse_functions;
    while (tll->next)
      tll = tll->next;
    tll->next = $1;
  }
  else
    dis_gparse_functions = $1;
} 
|
atomic_function_skeleton_plus 
{
  dis_TypedListList *tll;
  
//  if ($1->next != NULL || strcmp($1->predicate, "TOTAL-COST")) // rule out action costs
    GpG.is_numeric_fluents = TRUE; // default type is NUMBER 
  if ( dis_gparse_functions ) 
  {
    tll = dis_gparse_functions;
    while (tll->next)
      tll = tll->next;
    tll->next = $1;
  }
  else
    dis_gparse_functions = $1;
}
|
{}
;

atomic_function_skeleton_plus:
OPEN_PAREN  NAME typed_list_variable  CLOSE_PAREN atomic_function_skeleton_plus
{
  $$ = dis_new_dis_TypedListList();
  $$->predicate = dis_copy_dis_Token($2);
  $$->args = $3;
  $$->next = $5;
}
|
OPEN_PAREN  NAME typed_list_variable  CLOSE_PAREN
{
  $$ = dis_new_dis_TypedListList();
  $$->predicate = dis_copy_dis_Token($2);
  $$->args = $3;
  $$->next = NULL;
}
;

/**********************************************************************/
require_def:
OPEN_PAREN  REQUIREMENTS_TOK 
{ 
  dis_opserr( REQUIREM_dis_EXPECTED, NULL ); 
}
NAME
{ 
  if ( !dis_supported( $4 ) ) {
    dis_opserr( dis_NOT_SUPPdis_ORTED, $4 );
    yyerror();
  }
}
require_key_star  CLOSE_PAREN
;


/**********************************************************************/
require_key_star:
/* empty */
|
NAME
{ 
  if ( !dis_supported( $1 ) ) {
    dis_opserr( dis_NOT_SUPPdis_ORTED, $1 );
    yyerror();
  }
}
require_key_star
;


/**********************************************************************/
types_def:
OPEN_PAREN  TYPES_TOK
{ 
  dis_opserr( TYPEDEF_dis_EXPECTED, NULL ); 
}
typed_list_name  CLOSE_PAREN
{
  dis_gparse_types = $4;
}
; 


/**********************************************************************/
constants_def:
OPEN_PAREN  CONSTANTS_TOK
{ 
  dis_opserr( CONSTLIST_dis_EXPECTED, NULL ); 
}
typed_list_name  CLOSE_PAREN
{
  dis_gparse_constants = $4;
}
;

/**********************************************************************
 * actions and their optional definitions
 **********************************************************************/
action_def:
OPEN_PAREN  ACTION_TOK  
{ 
  dis_opserr( ACTION, NULL ); 
}  
NAME
{ 
  scur_op = dis_new_dis_Pldis_Operator( $4 );
  /* PDDL3 */  
  preference_pointer = NULL;
}
param_def  action_def_body  CLOSE_PAREN
{
  dis_PlNode *p;
  char temp[128]; 

  scur_op->next = dis_gloaded_ops;
  dis_gloaded_ops = scur_op; 
  scur_op = NULL;
}
;


/**********************************************************************/
param_def:
/* empty */
{ 
  scur_op->params = NULL; 
}
|
PARAMETERS_TOK  OPEN_PAREN  typed_list_variable  CLOSE_PAREN
{
  dis_TypedList *tl;
  scur_op->parse_params = $3;
  for (tl = scur_op->parse_params; tl; tl = tl->next) {
    /* to be able to distinguish params from :VARS 
     */
    scur_op->number_of_real_params++;
  }
}
;

/**********************************************************************/
action_def_body:
/* empty */
|
VARS_TOK  OPEN_PAREN  typed_list_variable  CLOSE_PAREN  action_def_body
{
  dis_TypedList *tl = NULL;

  /* add vars as parameters 
   */
  if ( scur_op->parse_params ) {
    for( tl = scur_op->parse_params; tl->next; tl = tl->next ) {
      /* empty, get to the end of list 
       */
    }
    tl->next = $3;
    tl = tl->next;
  } else {
    scur_op->parse_params = $3;
  }
}
|
/* PDDL 3 */
/* PRECONDITION_TOK  adl_goal_description */
PRECONDITION_TOK pre_GD
{ 
  scur_op->preconds = $2; 
}
action_def_body
|
EFFECT_TOK  adl_effect
{ 
  scur_op->effects = $2; 
}
action_def_body
;

pre_GD:
OPEN_PAREN CLOSE_PAREN
{
        $$ = NULL;
}
|
pref_GD
{
        $$ = $1;
}
|
OPEN_PAREN dis_AND_TOK pre_GD_star CLOSE_PAREN
{
        $$ = dis_new_dis_PlNode(dis_AND);
        $$->sons = $3;
}
|
OPEN_PAREN  Fdis_ORdis_ALL_TOK
OPEN_PAREN  typed_list_variable  CLOSE_PAREN
pre_GD  CLOSE_PAREN
{ 
  
  dis_PlNode *pln;

  pln = dis_new_dis_PlNode(dis_ALL);
  pln->parse_vars = $4; 

  $$ = pln;
  pln->sons = $6;

}
;

pre_GD_star:
/* empty */ 
{
        $$ = NULL;
}
|
pre_GD pre_GD_star
{
        if ($1)
        {
                $1->next = $2;
                $$ = $1;
        }
        else
                $$ = $2;
}
;

pref_GD:
adl_goal_description1
{
        $$ = $1;
}
;


/* derived predicates */
derived_def:
OPEN_PAREN DERIVED_TOK OPEN_PAREN predicate typed_list_variable 
CLOSE_PAREN adl_goal_description CLOSE_PAREN
{
	char temp[200];
	dis_TypedList *tl;
	dis_TokenList *t;
	GpG.is_deripred = TRUE;
	sprintf(temp, "deripred-%d>%s", ++dis_gnum_deripreds, $4);
	scur_op = dis_new_dis_Pldis_Operator(temp);

	scur_op->effects = dis_new_dis_PlNode(dis_AND);   
	scur_op->effects->parse_vars = NULL;
	scur_op->effects->next = NULL;    
	scur_op->effects->atom = NULL;
	scur_op->effects->sons = dis_new_dis_PlNode(dis_ATOM);
	scur_op->effects->sons->parse_vars = NULL;
	scur_op->effects->sons->next = NULL;
	scur_op->effects->sons->atom = dis_new_dis_TokenList();
	scur_op->effects->sons->atom->item = $4;
	scur_op->effects->sons->sons = NULL;

	t = scur_op->effects->sons->atom;
  scur_op->parse_params = $5;
  for (tl = scur_op->parse_params; tl; tl = tl->next) {
    /* to be able to distinguish params from :VARS
     */  
    scur_op->number_of_real_params++;
	t->next = dis_new_dis_TokenList();
	t->next->item = dis_new_dis_Token(strlen(tl->name)+1);
	strcpy(t->next->item, tl->name);
	t = t->next;
  }
	 	
	scur_op->preconds = $7;
	scur_op->next = dis_gloaded_ops;
	dis_gloaded_ops = scur_op; 
}
;

/**********************************************************************
 * Goal description providing full ADL.
 * RETURNS a tree with the connectives in the nodes and the atomic 
 * predicates in the leafs.
 **********************************************************************/
adl_goal_description:
adl_goal_description1
{
        $$ = $1;
}
|
OPEN_PAREN  dis_AND_TOK  adl_goal_description_star  CLOSE_PAREN
{ 
  $$ = dis_new_dis_PlNode(dis_AND);
  $$->sons = $3;
}
|
OPEN_PAREN  Fdis_ORdis_ALL_TOK 
OPEN_PAREN  typed_list_variable  CLOSE_PAREN 
adl_goal_description1  CLOSE_PAREN
{ 

  dis_PlNode *pln;

  pln = dis_new_dis_PlNode(dis_ALL);
  pln->parse_vars = $4;

  $$ = pln;
  pln->sons = $6;

}
;

adl_goal_description1:
OPEN_PAREN LE_TOK f_exp f_exp CLOSE_PAREN
{
  $$ = dis_new_dis_PlNode(dis_COMP);
  $$->comp = LE;
  $$->lh = $3;
  $$->rh = $4;
  GpG.is_numeric_fluents = TRUE;
}
|
OPEN_PAREN LEQ_TOK f_exp f_exp CLOSE_PAREN
{
  $$ = dis_new_dis_PlNode(dis_COMP);
  $$->comp = LEQ;
  $$->lh = $3;
  $$->rh = $4;
  GpG.is_numeric_fluents = TRUE;
}
|
OPEN_PAREN EQ_TOK f_exp f_exp CLOSE_PAREN
{
  $$ = dis_new_dis_PlNode(dis_COMP);
  $$->comp = EQ;
  $$->lh = $3;
  $$->rh = $4;
  GpG.is_numeric_fluents = TRUE;
}
|
OPEN_PAREN GEQ_TOK f_exp f_exp CLOSE_PAREN
{
  $$ = dis_new_dis_PlNode(dis_COMP);
  $$->comp = GEQ;
  $$->lh = $3;
  $$->rh = $4;
  GpG.is_numeric_fluents = TRUE;
}
|
OPEN_PAREN GE_TOK f_exp f_exp CLOSE_PAREN
{
  $$ = dis_new_dis_PlNode(dis_COMP);
  $$->comp = GE;
  $$->lh = $3;
  $$->rh = $4;
  GpG.is_numeric_fluents = TRUE;
}
|
literal_term
{ 
  if ( sis_negated ) {
    $$ = dis_new_dis_PlNode(dis_NOT);
    $$->sons = dis_new_dis_PlNode(dis_ATOM);
    $$->sons->atom = $1;
    sis_negated = dis_FALSE;
  } else {
    $$ = dis_new_dis_PlNode(dis_ATOM);
    $$->atom = $1;
  }
}
|
OPEN_PAREN  dis_OR_TOK  adl_goal_description_star  CLOSE_PAREN
{ 
  $$ = dis_new_dis_PlNode(dis_OR);
  $$->sons = $3;
}
|
OPEN_PAREN  dis_NOT_TOK  adl_goal_description  CLOSE_PAREN
{ 
  $$ = dis_new_dis_PlNode(dis_NOT);
  $$->sons = $3;
}
|
OPEN_PAREN  IMPLY_TOK  adl_goal_description  adl_goal_description  CLOSE_PAREN
{ 
  dis_PlNode *np = dis_new_dis_PlNode(dis_NOT);
  np->sons = $3;
  np->next = $4;

  $$ = dis_new_dis_PlNode(dis_OR);
  $$->sons = np;
}
|
OPEN_PAREN  dis_EXISTS_TOK 
OPEN_PAREN  typed_list_variable  CLOSE_PAREN 
adl_goal_description  CLOSE_PAREN
{ 

  dis_PlNode *pln;

  pln = dis_new_dis_PlNode(dis_EX);
  pln->parse_vars = $4;

  $$ = pln;
  pln->sons = $6;

}
;

/**********************************************************************/
adl_goal_description_star:
/* empty */
{
  $$ = NULL;
}
|
adl_goal_description  adl_goal_description_star
{
  $1->next = $2;
  $$ = $1;
}
;

/**********************************************************************
 * effects as allowed in pddl are saved in FF data structures
 * describes everything after the keyword :effect
 *********************************************************************/
adl_effect:
OPEN_PAREN ASSIGN_TOK f_head f_exp CLOSE_PAREN
{
  $$ = dis_new_dis_PlNode( dis_NEF );
  $$->neft = ASSIGN;
  $$->lh = $3;
  $$->rh = $4;
  GpG.is_numeric_fluents = TRUE;
}
|
OPEN_PAREN SCALE_UP_TOK f_head f_exp CLOSE_PAREN
{
  $$ = dis_new_dis_PlNode( dis_NEF );
  $$->neft = SCALE_UP;
  $$->lh = $3;
  $$->rh = $4;
  GpG.is_numeric_fluents = TRUE;
}
|
OPEN_PAREN SCALE_DOWN_TOK f_head f_exp CLOSE_PAREN
{
  $$ = dis_new_dis_PlNode( dis_NEF );
  $$->neft = SCALE_DOWN;
  $$->lh = $3;
  $$->rh = $4;
  GpG.is_numeric_fluents = TRUE;
}
|
OPEN_PAREN INCREASE_TOK f_head f_exp CLOSE_PAREN
{
  $$ = dis_new_dis_PlNode( dis_NEF );
  $$->neft = INCREASE;
  $$->lh = $3;
  $$->rh = $4;
//  if (strcmp($3->atom->item, "TOTAL-COST"))     /* rule out action-costs */
    GpG.is_numeric_fluents = TRUE;
}
|
OPEN_PAREN DECREASE_TOK f_head f_exp CLOSE_PAREN
{
  $$ = dis_new_dis_PlNode( dis_NEF );
  $$->neft = DECREASE;
  $$->lh = $3;
  $$->rh = $4;
  GpG.is_numeric_fluents = TRUE;
}
|
literal_term
{ 
  if ( sis_negated ) {
    $$ = dis_new_dis_PlNode(dis_NOT);
    $$->sons = dis_new_dis_PlNode(dis_ATOM);
    $$->sons->atom = $1;
    sis_negated = dis_FALSE;
  } else {
    $$ = dis_new_dis_PlNode(dis_ATOM);
    $$->atom = $1;
  }
}
|
OPEN_PAREN  dis_AND_TOK  adl_effect_star  CLOSE_PAREN
{ 
  $$ = dis_new_dis_PlNode(dis_AND);
  $$->sons = $3;
}
|
OPEN_PAREN  Fdis_ORdis_ALL_TOK 
OPEN_PAREN  typed_list_variable  CLOSE_PAREN 
adl_effect  CLOSE_PAREN
{ 

  dis_PlNode *pln;

  pln = dis_new_dis_PlNode(dis_ALL);
  pln->parse_vars = $4;

  $$ = pln;
  pln->sons = $6;

}
|
OPEN_PAREN  dis_WHEN_TOK  adl_goal_description  adl_effect  CLOSE_PAREN
{
  /* This will be conditional effects in FF representation, but here
   * a formula like (dis_WHEN p q) will be saved as:
   *  [dis_WHEN]
   *  [sons]
   *   /  \
   * [p]  [q]
   * That means, the first son is p, and the second one is q. 
   */
  $$ = dis_new_dis_PlNode(dis_WHEN);
  $3->next = $4;
  $$->sons = $3;
  dis_gconditional_effects = dis_TRUE;
}
;


/**********************************************************************/
adl_effect_star:
{ 
  $$ = NULL; 
}
|
adl_effect  adl_effect_star
{
  $1->next = $2;
  $$ = $1;
}
;


/**********************************************************************
 * some expressions used in many different rules
 **********************************************************************/
f_head:
OPEN_PAREN NAME term_star CLOSE_PAREN
{
  $$ = dis_new_dis_Parsedis_ExpNode( FHEAD );
  $$->atom = dis_new_dis_TokenList();
  $$->atom->item = dis_new_dis_Token( strlen($2)+1 );
  strcpy( $$->atom->item, $2 );
  $$->atom->next = $3;
}
/*|
NAME
{
  $$ = dis_new_dis_Parsedis_ExpNode( FHEAD );
  $$->atom = dis_new_dis_TokenList();
  $$->atom->item = dis_new_dis_Token( strlen($1)+1 );
  strcpy( $$->atom->item, $1 );
}*/
;



f_exp:
NUM
{ 
  $$ = dis_new_dis_Parsedis_ExpNode( NUMBER );
  $$->atom = dis_new_dis_TokenList();
  $$->atom->item = dis_new_dis_Token( strlen($1)+1 );
  strcpy( $$->atom->item, $1 );
}
|
f_head
{
  $$ = $1;
}
|
OPEN_PAREN MINUS_TOK f_exp CLOSE_PAREN
{
  $$ = dis_new_dis_Parsedis_ExpNode( MINUS );
  $$->leftson = $3;
}
|
OPEN_PAREN AD_TOK f_exp f_exp CLOSE_PAREN
{
  $$ = dis_new_dis_Parsedis_ExpNode( AD );
  $$->leftson = $3;
  $$->rightson = $4;
}
|
OPEN_PAREN MINUS_TOK f_exp f_exp CLOSE_PAREN
{
  $$ = dis_new_dis_Parsedis_ExpNode( SU );
  $$->leftson = $3;
  $$->rightson = $4;
}
|
OPEN_PAREN MU_TOK f_exp f_exp CLOSE_PAREN
{
  $$ = dis_new_dis_Parsedis_ExpNode( MU );
  $$->leftson = $3;
  $$->rightson = $4;
}
|
OPEN_PAREN DI_TOK f_exp f_exp CLOSE_PAREN
{
  $$ = dis_new_dis_Parsedis_ExpNode( DI );
  $$->leftson = $3;
  $$->rightson = $4;
}
;


/**********************************************************************/
literal_term:
OPEN_PAREN  dis_NOT_TOK  atomic_formula_term  CLOSE_PAREN
{ 
  $$ = $3;
  sis_negated = dis_TRUE;
}
|
atomic_formula_term
{
  $$ = $1;
}
;


/**********************************************************************/
atomic_formula_term:
OPEN_PAREN  predicate  term_star  CLOSE_PAREN
{ 
  $$ = dis_new_dis_TokenList();
  $$->item = $2;
  $$->next = $3;
}
|
OPEN_PAREN  EQ_TOK  term_star  CLOSE_PAREN
{
  $$ = dis_new_dis_TokenList();
  $$->item = dis_copy_dis_Token("=");
  $$->next = $3;
}
;


/**********************************************************************/
term_star:
/* empty */
{ $$ = NULL; }
|
term  term_star
{
  $$ = dis_new_dis_TokenList();
  $$->item = $1;
  $$->next = $2;
}
;


/**********************************************************************/
term:
NAME
{ 
  $$ = dis_new_dis_Token( strlen($1)+1 );
  strcpy( $$, $1 );
}
|
VARIABLE
{ 
  $$ = dis_new_dis_Token( strlen($1)+1 );
  strcpy( $$, $1 );
}
;


/**********************************************************************/
name_plus:
NAME
{
  $$ = dis_new_dis_TokenList();
  $$->item = dis_new_dis_Token( strlen($1)+1 );
  strcpy( $$->item, $1 );
}
|
NAME  name_plus
{
  $$ = dis_new_dis_TokenList();
  $$->item = dis_new_dis_Token( strlen($1)+1 );
  strcpy( $$->item, $1 );
  $$->next = $2;
}
;

/**********************************************************************/
predicate:
NAME
{ 
  $$ = dis_new_dis_Token( strlen($1)+1 );
  strcpy( $$, $1 );
}
;


/**********************************************************************/
typed_list_name:     /* returns dis_TypedList */
/* empty */
{ $$ = NULL; }
|
NAME  EITHER_TOK  name_plus  CLOSE_PAREN  typed_list_name
{ 

  $$ = dis_new_dis_TypedList();
  $$->name = dis_new_dis_Token( strlen($1)+1 );
  strcpy( $$->name, $1 );
  $$->type = $3;
  $$->next = $5;
}
|
NAME  MINUS_TOK NAME  typed_list_name   /* end of list for one type */
{
  $$ = dis_new_dis_TypedList();
  $$->name = dis_new_dis_Token( strlen($1)+1 );
  strcpy( $$->name, $1 );
  $$->type = dis_new_dis_TokenList();
  $$->type->item = dis_new_dis_Token( strlen($3)+1 );
  strcpy( $$->type->item, $3 );
  $$->next = $4;
}
|
NAME  typed_list_name        /* a list element (gets type from next one) */
{
  $$ = dis_new_dis_TypedList();
  $$->name = dis_new_dis_Token( strlen($1)+1 );
  strcpy( $$->name, $1 );
  if ( $2 ) {/* another element (already typed) is following */
    $$->type = dis_copy_dis_TokenList( $2->type );
  } else {/* no further element - it must be an untyped list */
    $$->type = dis_new_dis_TokenList();
    $$->type->item = dis_new_dis_Token( strlen(dis_STdis_ANDARD_TYPE)+1 );
    strcpy( $$->type->item, dis_STdis_ANDARD_TYPE );
  }
  $$->next = $2;
}
;


/***********************************************/
typed_list_variable:     /* returns dis_TypedList */
/* empty */
{ $$ = NULL; }
|
VARIABLE  EITHER_TOK  name_plus  CLOSE_PAREN  typed_list_variable
{ 
  $$ = dis_new_dis_TypedList();
  $$->name = dis_new_dis_Token( strlen($1)+1 );
  strcpy( $$->name, $1 );
  $$->type = $3;
  $$->next = $5;
}
|
VARIABLE  MINUS_TOK NAME  typed_list_variable   /* end of list for one type */
{
  $$ = dis_new_dis_TypedList();
  $$->name = dis_new_dis_Token( strlen($1)+1 );
  strcpy( $$->name, $1 );
  $$->type = dis_new_dis_TokenList();
  $$->type->item = dis_new_dis_Token( strlen($3)+1 );
  strcpy( $$->type->item, $3 );
  $$->next = $4;
}
|
VARIABLE  typed_list_variable        /* a list element (gets type from next one) */
{
  $$ = dis_new_dis_TypedList();
  $$->name = dis_new_dis_Token( strlen($1)+1 );
  strcpy( $$->name, $1 );
  if ( $2 ) {/* another element (already typed) is following */
    $$->type = dis_copy_dis_TokenList( $2->type );
  } else {/* no further element - it must be an untyped list */
    $$->type = dis_new_dis_TokenList();
    $$->type->item = dis_new_dis_Token( strlen(dis_STdis_ANDARD_TYPE)+1 );
    strcpy( $$->type->item, dis_STdis_ANDARD_TYPE );
  }
  $$->next = $2;
}
;

/* durative actions */
durative_action_def:
OPEN_PAREN  DURATIVE_ACTION_TOK
{                  
  dis_opserr( ACTION, NULL );
}                  
NAME               
{                  
  scur_op = dis_new_dis_Pldis_Operator( $4 );
  preference_pointer = NULL;
}                  
param_def  durative_action_def_body  CLOSE_PAREN
{                  
  char temp[128];
  dis_PlNode *p;
  
  /* an artificial effect for calculating the duration value */
  p = dis_new_dis_PlNode(dis_AND);
  p->sons = dis_new_dis_PlNode(dis_NEF);
  p->sons->neft = ASSIGN;
  p->sons->lh = dis_new_dis_Parsedis_ExpNode( FHEAD ); 
  p->sons->lh->atom = dis_new_dis_TokenList();
  p->sons->lh->atom->item = dis_copy_dis_Token("DURATIONFUNCTION");
  p->sons->rh = duration_exp;
  if (scur_op->effects->connective == dis_AND)
    p->sons->next = scur_op->effects->sons;  
  else
    p->sons->next = scur_op->effects;  
  scur_op->effects = p;

  
  scur_op->next = dis_gloaded_ops;
  dis_gloaded_ops = scur_op;
  scur_op = NULL;
  gnum_das++;
  GpG.is_durative = TRUE;
}                  
;                  
/***********************************************/
durative_action_def_body:
DURATION_TOK duration_constraint
{                  
	scur_op->duration = $2;
}
/* PDDL3 */                  
CONDITION_TOK  da_GD
{                  
	scur_op->preconds = $5;
}                  
EFFECT_TOK  da_adl_effect
{                  
	scur_op->effects = $8;
}                  
;

da_GD:
OPEN_PAREN CLOSE_PAREN
{
        $$ = NULL;
}
|
pref_timed_GD
{
        $$ = $1;
}
|
OPEN_PAREN dis_AND_TOK da_GD_star CLOSE_PAREN
{
        $$ = dis_new_dis_PlNode(dis_AND);
        $$->sons = $3;
}
|
OPEN_PAREN  Fdis_ORdis_ALL_TOK
OPEN_PAREN  typed_list_variable  CLOSE_PAREN
da_GD  CLOSE_PAREN
{ 
  
  dis_PlNode *pln;

  pln = dis_new_dis_PlNode(dis_ALL);
  pln->parse_vars = $4; 

  $$ = pln;
  pln->sons = $6;

}
;
 
da_GD_star:
/* empty */
{
        $$ = NULL;
}
|
da_GD da_GD_star
{
        if ($1)
        {
                $1->next = $2;
                $$ = $1;
        }   
        else
                $$ = $2;
}
;

pref_timed_GD:
timed_adl_goal_description
{
        $$ = $1;
}
;

da_adl_goal_description:
timed_adl_goal_description
{                   
  $$ = dis_new_dis_PlNode(dis_AND);
  $$->sons = $1;    
}                   
|                   
OPEN_PAREN dis_AND_TOK timed_adl_goal_description_plus CLOSE_PAREN
{                   
  $$ = dis_new_dis_PlNode(dis_AND);
  $$->sons = $3;    
}                   
;                   

timed_adl_goal_description_plus:
timed_adl_goal_description
{                   
  $$=$1;            
}                   
|                   
timed_adl_goal_description timed_adl_goal_description_plus
{                   
  $$ = $1;          
  $$->next = $2;    
}                   
;                   

timed_adl_goal_description:
OPEN_PAREN AT_START adl_goal_description CLOSE_PAREN
{
//	$$ = dis_new_dis_PlNode(dis_AT_START_CONN);                   
//  $$->sons = $3;
	$$ = $3;    
	$$->value = 1;
}                   
|                   
OPEN_PAREN AT_END adl_goal_description CLOSE_PAREN
{                   
//	$$ = dis_new_dis_PlNode(dis_AT_END_CONN);
//  $$->sons = $3;    
	$$ = $3;
	$$->value = 2;
}                   
|                   
OPEN_PAREN OVER_ALL adl_goal_description CLOSE_PAREN
{                   
//	$$ = dis_new_dis_PlNode(dis_OVER_ALL_CONN);
//  $$->sons = $3;    
	$$ = $3;
	$$->value = 3;
}                   
;

duration_constraint:
OPEN_PAREN EQ_TOK DURATION_VAR_TOK f_exp CLOSE_PAREN
{
  $$ = dis_new_dis_PlNode(dis_COMP);
  $$->comp = EQ;
  $$->lh = NULL;
  $$->rh = $4;
  duration_exp = $4;
}
;

da_adl_effect:
OPEN_PAREN CLOSE_PAREN
{
}
|
OPEN_PAREN dis_AND_TOK timed_adl_effect_plus CLOSE_PAREN
{                   
  $$ = dis_new_dis_PlNode(dis_AND);
  $$->sons = $3;    
}                   
|                   
timed_adl_effect    
{                   
  $$ = dis_new_dis_PlNode(dis_AND);
  $$->sons = $1;    
}                   
|                   
OPEN_PAREN  Fdis_ORdis_ALL_TOK
OPEN_PAREN  typed_list_variable  CLOSE_PAREN
da_adl_effect  CLOSE_PAREN
{                   
  dis_PlNode *pln;      

  pln = dis_new_dis_PlNode(dis_ALL);
  pln->parse_vars = $4;

  $$ = pln;         
  pln->sons = $6;   
}                   
|                   
OPEN_PAREN  dis_WHEN_TOK  da_adl_goal_description  timed_adl_effect  CLOSE_PAREN
{                   
  /* This will be conditional effects in FF representation, but here
   * a formula like (WHEN p q) will be saved as:
   *  [WHEN]        
   *  [sons]        
   *   /  \         
   * [p]  [q]       
   * That means, the first son is p, and the second one is q.
   */
  $$ = dis_new_dis_PlNode(dis_WHEN);
  $3->next = $4;    
  $$->sons = $3;   
  dis_gconditional_effects = dis_TRUE; 
}                   
|                   
OPEN_PAREN ASSIGN_TOK f_head f_exp_da CLOSE_PAREN
{
  $$ = dis_new_dis_PlNode( dis_NEF );
  $$->neft = ASSIGN;
  $$->lh = $3;
  $$->rh = $4;
  GpG.is_numeric_fluents = TRUE;
}
|
OPEN_PAREN SCALE_UP_TOK f_head f_exp_da CLOSE_PAREN
{
  $$ = dis_new_dis_PlNode( dis_NEF );
  $$->neft = SCALE_UP;
  $$->lh = $3;
  $$->rh = $4;
  GpG.is_numeric_fluents = TRUE;
}
|
OPEN_PAREN SCALE_DOWN_TOK f_head f_exp_da CLOSE_PAREN
{
  $$ = dis_new_dis_PlNode( dis_NEF );
  $$->neft = SCALE_DOWN;
  $$->lh = $3;
  $$->rh = $4;
  GpG.is_numeric_fluents = TRUE;
}
|
OPEN_PAREN INCREASE_TOK f_head f_exp_da CLOSE_PAREN
{
  $$ = dis_new_dis_PlNode( dis_NEF );
  $$->neft = INCREASE;
  $$->lh = $3;
  $$->rh = $4;
//  if (strcmp($3->atom->item, "TOTAL-COST"))     /* rule out action-costs */
    GpG.is_numeric_fluents = TRUE;
}
|
OPEN_PAREN DECREASE_TOK f_head f_exp_da CLOSE_PAREN
{
  $$ = dis_new_dis_PlNode( dis_NEF );
  $$->neft = DECREASE;
  $$->lh = $3;
  $$->rh = $4;
  GpG.is_numeric_fluents = TRUE;
}
;                   

timed_adl_effect_plus:
timed_adl_effect    
{                   
  $$ = $1;          
}                   
|                   
timed_adl_effect timed_adl_effect_plus
{                   
  $$ = $1;          
  $$->next = $2;    
}                   
;                   

timed_adl_effect:
OPEN_PAREN AT_START adl_effect CLOSE_PAREN
{
//	$$ = dis_new_dis_PlNode(dis_AT_START_CONN);
//  $$->sons = $3;
	$$ = $3;
	$$->value = 1;
}
|
OPEN_PAREN AT_END adl_effect CLOSE_PAREN
{
//	$$ = dis_new_dis_PlNode(dis_AT_END_CONN);
//  $$->sons = $3;
	$$ = $3;
	$$->value = 2;
}
|
OPEN_PAREN AT_START f_assign_da CLOSE_PAREN
{
//	$$ = dis_new_dis_PlNode(dis_AT_START_CONN);
//  $$->sons = $3;
	$$ = $3;
	$$->value = 1;
}
|
OPEN_PAREN AT_END f_assign_da CLOSE_PAREN
{
//	$$ = dis_new_dis_PlNode(dis_AT_END_CONN);
//  $$->sons = $3;
	$$ = $3;
	$$->value = 2;
}
;                   

f_assign_da:
OPEN_PAREN ASSIGN_TOK f_head f_exp_da CLOSE_PAREN
{
  $$ = dis_new_dis_PlNode( dis_NEF );
  $$->neft = ASSIGN;
  $$->lh = $3;
  $$->rh = $4;
  GpG.is_numeric_fluents = TRUE;
}
|
OPEN_PAREN SCALE_UP_TOK f_head f_exp_da CLOSE_PAREN
{
  $$ = dis_new_dis_PlNode( dis_NEF );
  $$->neft = SCALE_UP;
  $$->lh = $3;
  $$->rh = $4;
  GpG.is_numeric_fluents = TRUE;
}
|
OPEN_PAREN SCALE_DOWN_TOK f_head f_exp_da CLOSE_PAREN
{
  $$ = dis_new_dis_PlNode( dis_NEF );
  $$->neft = SCALE_DOWN;
  $$->lh = $3;
  $$->rh = $4;
  GpG.is_numeric_fluents = TRUE;
}
|
OPEN_PAREN INCREASE_TOK f_head f_exp_da CLOSE_PAREN
{
  $$ = dis_new_dis_PlNode( dis_NEF );
  $$->neft = INCREASE;
  $$->lh = $3;
  $$->rh = $4;
//  if (strcmp($3->atom->item, "TOTAL-COST"))     /* rule out action-costs */
    GpG.is_numeric_fluents = TRUE;
}
|
OPEN_PAREN DECREASE_TOK f_head f_exp_da CLOSE_PAREN
{
  $$ = dis_new_dis_PlNode( dis_NEF );
  $$->neft = DECREASE;
  $$->lh = $3;
  $$->rh = $4;
  GpG.is_numeric_fluents = TRUE;
}
;                   

f_exp_da:
OPEN_PAREN AD_TOK f_exp_da f_exp_da CLOSE_PAREN
{
  $$ = dis_new_dis_Parsedis_ExpNode( AD );
  $$->leftson = $3;
  $$->rightson = $4;
}
|
OPEN_PAREN MINUS_TOK f_exp_da f_exp_da CLOSE_PAREN
{
  $$ = dis_new_dis_Parsedis_ExpNode( SU );
  $$->leftson = $3;
  $$->rightson = $4;
}
|
OPEN_PAREN MU_TOK f_exp_da f_exp_da CLOSE_PAREN
{
  $$ = dis_new_dis_Parsedis_ExpNode( MU );
  $$->leftson = $3;
  $$->rightson = $4;
}
|
OPEN_PAREN DI_TOK f_exp_da f_exp_da CLOSE_PAREN
{
  $$ = dis_new_dis_Parsedis_ExpNode( DI );
  $$->leftson = $3;
  $$->rightson = $4;
}
|
OPEN_PAREN MINUS_TOK f_exp_da CLOSE_PAREN
{
  $$ = dis_new_dis_Parsedis_ExpNode( MINUS );
  $$->leftson = $3;
}
|
DURATION_VAR_TOK
{
	$$ = copy_dis_Parsedis_ExpNode(duration_exp);
}
|
f_exp
;

%%
#include "lex.dis_ops_pddl.c"


/**********************************************************************
 * Functions
 **********************************************************************/

/* 
 * call	bison -pops -bscan-ops scan-ops.y
 */

  


int yyerror( char *msg )

{

  fflush(stdout);
  fprintf(stderr, "\n%s: syntax error in line %d, '%s':\n", 
	  dis_gact_filename, dis_lineno, dis_ops_pddltext);

  if ( NULL != sact_err_par ) {
    fprintf(stderr, "%s %s\n", serrmsg[sact_err], sact_err_par);
  } else {
    fprintf(stderr, "%s\n", serrmsg[sact_err]);
  }

  exit( 1 );

}



void dis_load_ops_file( char *filename )

{

  FILE * fp;/* pointer to input files */
  char tmp[dis_MAX_LENGTH] = "";

  /* open operator file 
   */
  if( ( fp = fopen( filename, "r" ) ) == NULL ) {
    sprintf(tmp, "\nff: can't find operator file: %s\n\n", filename );
    perror(tmp);
    exit( 1 );
  }

  dis_gact_filename = filename;
  dis_lineno = 1; 
  dis_ops_pddlin = fp;

  yyparse();

  fclose( fp );/* and close file again */

}
