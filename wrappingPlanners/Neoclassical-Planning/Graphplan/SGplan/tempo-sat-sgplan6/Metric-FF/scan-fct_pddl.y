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

 * File: scan-fct_pddl.y 

 * Description: modified from scan-fct_pddl.y in Metric-FF

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
#include "lpg.h"
#include "dis_memory.h"
#include "dis_parse.h"
#include "dis_constraints.h"

#ifndef SCAN_ERR
#define SCAN_ERR
#define DEFINE_dis_EXPECTED            0
#define PROBLEM_dis_EXPECTED           1
#define PROBNAME_dis_EXPECTED          2
#define LBRACKET_dis_EXPECTED          3
#define RBRACKET_dis_EXPECTED          4
#define DOMDEFS_dis_EXPECTED           5
#define REQUIREM_dis_EXPECTED          6
#define TYPEDLIST_dis_EXPECTED         7
#define DOMdis_EXT_dis_EXPECTED            8
#define DOMdis_EXTNAME_dis_EXPECTED        9
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
#define SITUATION_dis_EXPECTED        20
#define SITNAME_dis_EXPECTED          21
#define BDOMAIN_dis_EXPECTED          22
#define BADDOMAIN                 23
#define INIFACTS                  24
#define GOALDEF                   25
#define ADLGOAL                   26
#define CONSTRAINTLIST_EXPECTED	  27
#endif


static char * serrmsg[] = {
  "'define' expected",
  "'problem' expected",
  "problem name expected",
  "'(' expected",
  "')' expected",
  "additional domain definitions expected",
  "requirements (e.g. ':strips') expected",
  "typed list of <%s> expected",
  "domain extension expected",
  "domain to be extented expected",
  "type definition expected",
  "list of constants expected",
  "predicate definition expected",
  "<name> expected",
  "<variable> expected",
  "action functor expected",
  "atomic formula expected",
  "effect definition expected",
  "negated atomic formula expected",
  "requirement %s not supported by this version",  
  "'situation' expected",
  "situation name expected",
  "':domain' expected",
  "this problem needs another domain file",
  "initial facts definition expected",
  "goal definition expected",
  "first order logic expression expected",
  "constraint list expected",
  NULL
};


void dis_fcterr( int errno, char *par );


static int sact_err;
static char *sact_err_par = NULL;
static dis_Bool sis_negated = dis_FALSE;
static dis_Pldis_Operator *scur_op = NULL;
static dis_TypedList *forall_tl = NULL;

void dis_fcterr( int errno, char *par ) {

  sact_err = errno;

  if ( sact_err_par ) {
    free( sact_err_par );
  }
  if ( par ) {
    sact_err_par = dis_new_dis_Token( strlen(par)+1 );
    strcpy( sact_err_par, par);
  } else {
    sact_err_par = NULL;
  }

}
%}


%start file


%union {

  char string[dis_MAX_LENGTH];
  char* pstring;
  dis_Parsedis_ExpNode *pdis_Parsedis_ExpNode;
  dis_PlNode* pdis_PlNode;
  dis_FactList* pdis_FactList;
  dis_TokenList* pdis_TokenList;
  dis_TypedList* pdis_TypedList;

}


%type <pstring> problem_name
%type <pdis_Parsedis_ExpNode> f_exp
%type <pdis_Parsedis_ExpNode> ground_f_exp
%type <pdis_Parsedis_ExpNode> plus_ground_f_exp_plus
%type <pdis_Parsedis_ExpNode> mul_ground_f_exp_plus
%type <pdis_PlNode> adl_goal_description
%type <pdis_PlNode> adl_goal_description1
%type <pdis_PlNode> adl_goal_description_star
%type <pdis_PlNode> init_el_plus
%type <pdis_PlNode> init_el
/*%type <pdis_PlNode> literal_name_plus*/
%type <pdis_PlNode> literal_name
%type <pdis_TokenList> literal_term
%type <pdis_TokenList> atomic_formula_term
%type <pdis_TokenList> term_star
%type <pstring> term
%type <pdis_TokenList> name_star
%type <pdis_TokenList> atomic_formula_name
%type <pstring> predicate
%type <pdis_TypedList> typed_list_name
%type <pdis_TypedList> typed_list_variable
%type <pdis_TokenList> name_plus
/*PDDL3--*/
%type <pdis_PlNode> pre_GD
%type <pdis_PlNode> pref_GD
%type <pdis_PlNode> pre_GD_star 
/*--PDDL3*/

%token DEFINE_TOK
%token PROBLEM_TOK
%token SITUATION_TOK
%token BSITUATION_TOK
%token OBJECTS_TOK
%token BDOMAIN_TOK
%token INIT_TOK
%token GOAL_TOK
%token METRIC_TOK
%token dis_AND_TOK
%token dis_NOT_TOK
%token <string> NAME
%token <string> VARIABLE
%token <string> NUM
%token LE_TOK
%token LEQ_TOK
%token EQ_TOK
%token GEQ_TOK
%token GE_TOK
%token MINUS_TOK
%token AD_TOK
%token MU_TOK
%token DI_TOK
%token Fdis_ORdis_ALL_TOK
%token IMPLY_TOK
%token dis_OR_TOK
%token dis_EXISTS_TOK
%token EITHER_TOK
%token OPEN_PAREN
%token CLOSE_PAREN
/* timed initial literals */
%token <string> AT_TOK
/*PDDL3--*/
%token  PREFERENCE_TOK 
%token	IS_VIOLATED_TOK
/*--PDDL3*/

%%


/**********************************************************************/
file:
/* empty */
|
problem_definition  file
;


/**********************************************************************/
problem_definition : 
OPEN_PAREN DEFINE_TOK         
{ 
  dis_fcterr( PROBNAME_dis_EXPECTED, NULL ); 
}
problem_name  problem_defs  CLOSE_PAREN                 
{  
  dis_gproblem_name = $4;
  if ( dis_gcmd_line.display_info >= 1 ) {
    printf("\n problem '%s' defined\n", dis_gproblem_name);
  }
}
;


/**********************************************************************/
problem_name :
OPEN_PAREN  PROBLEM_TOK  NAME  CLOSE_PAREN        
{ 
  $$ = dis_new_dis_Token( strlen($3)+1 );
  strcpy($$, $3);
}
;


/**********************************************************************/
base_domain_name :
OPEN_PAREN  BDOMAIN_TOK  NAME  CLOSE_PAREN
{ 
  if ( dis_SAME != strcmp($3, dis_gdomain_name) ) {
    dis_fcterr( BADDOMAIN, NULL );
    yyerror();
  }
}
;


/**********************************************************************/
problem_defs:
/* empty */
|
/* objects_def  problem_defs */
problem_defs objects_def
|
/* init_def  problem_defs */
problem_defs init_def
|
/* goal_def  problem_defs */
problem_defs goal_def
|
/* base_domain_name  problem_defs */
problem_defs base_domain_name
|
/* metric_def problem_defs */
problem_defs metric_def
;


/**********************************************************************/
objects_def:
OPEN_PAREN  OBJECTS_TOK  typed_list_name  CLOSE_PAREN
{ 
  dis_gparse_objects = $3;
}
;


/**********************************************************************/
init_def:
OPEN_PAREN  INIT_TOK
{
  dis_fcterr( INIFACTS, NULL ); 
}
init_el_plus  CLOSE_PAREN
{
  dis_gorig_initial_facts = dis_new_dis_PlNode(dis_AND);
  dis_gorig_initial_facts->sons = $4;
}
;


/**********************************************************************/
goal_def:
OPEN_PAREN  GOAL_TOK
{ 
  dis_fcterr( GOALDEF, NULL ); 
}
/* PDDL 3 */
pre_GD  CLOSE_PAREN
{
  $4->next = dis_gorig_goal_facts;
  dis_gorig_goal_facts = $4;
}
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
OPEN_PAREN  typed_list_variable
{
  forall_tl = $4;
}
CLOSE_PAREN pre_GD  CLOSE_PAREN
{
 
  dis_PlNode *pln;

  pln = dis_new_dis_PlNode(dis_ALL);
  pln->parse_vars = $4; 
  forall_tl = NULL;

  $$ = pln;
  pln->sons = $7;
 
}
;

pref_GD:
OPEN_PAREN PREFERENCE_TOK NAME adl_goal_description CLOSE_PAREN
{
        dis_PrefNode *pref;
        dis_ConNode *con; 
        char temp[128];

	GpG.is_preferences = TRUE;
	sprintf(temp, "%s%d", $3, dis_num_preference);
        pref = new_dis_PrefNode(temp, "GOALS", $3);
        con = new_dis_ConNode(dis_ATOM_c);
        con->sons = $4;
        pref->body = con;
        pref->args = copy_dis_TypedList(forall_tl);
        pref->next = dis_gloaded_preferences;
        dis_gloaded_preferences = pref; 
        dis_num_preference++;
        if (forall_tl)
        	$$ = dis_new_dis_PlNode(dis_TRU);
        else
	        $$ = NULL;
}
|
OPEN_PAREN PREFERENCE_TOK adl_goal_description CLOSE_PAREN
{
        dis_PrefNode *pref;
        dis_ConNode *con;  
        char temp[128];

	GpG.is_preferences = TRUE;
	sprintf(temp, "ANONYMOUS%d", dis_num_preference);
        pref = new_dis_PrefNode(temp, "GOALS", "ANONYMOUS");
        con = new_dis_ConNode(dis_ATOM_c);
        con->sons = $3;
        pref->body = con;
        pref->args = copy_dis_TypedList(forall_tl);
        pref->next = dis_gloaded_preferences;
        dis_gloaded_preferences = pref; 
        dis_num_preference++;
        if (forall_tl)
        	$$ = dis_new_dis_PlNode(dis_TRU);
        else
	        $$ = NULL;
}
|
adl_goal_description1
{
        $$ = $1;
}
;


pre_GD_star:
/* empty */ 
{
        $$ = NULL;
}
|
pre_GD_star pre_GD
{
	dis_PlNode *p;

/* insert into the front */	
	if ($1)
	{
		for (p=$1;p->next;p=p->next)
			;
		p->next = $2;
		$$ = $1;
	}
	else	
		$$ = $2;
}
;


/**********************************************************************/
metric_def:
OPEN_PAREN  METRIC_TOK  NAME ground_f_exp CLOSE_PAREN
{

  if ( dis_gparse_metric != NULL ) {
    printf("\n\ndouble metric specification!\n\n");
    exit( 1 );
  }

  dis_gparse_optimization = dis_copy_dis_Token($3);
  dis_gparse_metric = $4;

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
adl_goal_description  CLOSE_PAREN
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
}
|
OPEN_PAREN LEQ_TOK f_exp f_exp CLOSE_PAREN
{
  $$ = dis_new_dis_PlNode(dis_COMP);
  $$->comp = LEQ;
  $$->lh = $3;
  $$->rh = $4;
}
|
OPEN_PAREN EQ_TOK f_exp f_exp CLOSE_PAREN
{
  $$ = dis_new_dis_PlNode(dis_COMP);
  $$->comp = EQ;
  $$->lh = $3;
  $$->rh = $4;
}
|
OPEN_PAREN GEQ_TOK f_exp f_exp CLOSE_PAREN
{
  $$ = dis_new_dis_PlNode(dis_COMP);
  $$->comp = GEQ;
  $$->lh = $3;
  $$->rh = $4;
}
|
OPEN_PAREN GE_TOK f_exp f_exp CLOSE_PAREN
{
  $$ = dis_new_dis_PlNode(dis_COMP);
  $$->comp = GE;
  $$->lh = $3;
  $$->rh = $4;
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
 * initial state: combined literals and assignments
 **********************************************************************/
init_el_plus:
init_el
{
  $$ = $1;
}
|
/* init_el init_el_plus */
init_el_plus init_el
{
	dis_PlNode *p;

/* insert into the front */
	if ($1)
	{
		for (p=$1;p->next;p=p->next)
			;
		p->next = $2;
		$$ = $1;
	}
	else	
		$$ = $2;
}
;


/**********************************************************************/
init_el:
literal_name
{
  $$ = $1;
}
|
OPEN_PAREN EQ_TOK OPEN_PAREN NAME name_star CLOSE_PAREN NUM CLOSE_PAREN
{
  $$ = dis_new_dis_PlNode( dis_COMP );
  $$->comp = EQ;

  $$->lh = dis_new_dis_Parsedis_ExpNode( FHEAD );
  $$->lh->atom = dis_new_dis_TokenList();
  $$->lh->atom->item = dis_new_dis_Token( strlen($4)+1 );
  strcpy( $$->lh->atom->item, $4 );
  $$->lh->atom->next = $5;

  $$->rh = dis_new_dis_Parsedis_ExpNode( NUMBER );
  $$->rh->atom = dis_new_dis_TokenList();
  $$->rh->atom->item = dis_new_dis_Token( strlen($7)+1 );
  strcpy( $$->rh->atom->item, $7 );
}
|
OPEN_PAREN EQ_TOK NAME NUM CLOSE_PAREN
{
  $$ = dis_new_dis_PlNode( dis_COMP );
  $$->comp = EQ;

  $$->lh = dis_new_dis_Parsedis_ExpNode( FHEAD );
  $$->lh->atom = dis_new_dis_TokenList();
  $$->lh->atom->item = dis_new_dis_Token( strlen($3)+1 );
  strcpy( $$->lh->atom->item, $3 );

  $$->rh = dis_new_dis_Parsedis_ExpNode( NUMBER );
  $$->rh->atom = dis_new_dis_TokenList();
  $$->rh->atom->item = dis_new_dis_Token( strlen($4)+1 );
  strcpy( $$->rh->atom->item, $4 );
}
/* timed initial literals */
|
OPEN_PAREN AT_TOK literal_name CLOSE_PAREN
{
	int n;
	dis_TimedInitial *til;
	char name[32];
	GpG.is_til = dis_TRUE;
	if (dis_gnum_tils%128 == 0)
	{
		n = (dis_gnum_tils + 128)/128*128;
		dis_gtils = (dis_TimedInitial *) realloc(dis_gtils, sizeof(dis_TimedInitial)*n); 
	}
	til = &dis_gtils[dis_gnum_tils++];
	sscanf($2, "%s %f", name, &(til->time));
	if ($3->connective == dis_NOT)
	{
		til->negated = dis_TRUE;
		til->literal = $3->sons;
	}
	else
	{
		til->negated = dis_FALSE;
		til->literal = $3;
	}
	$$ = dis_new_dis_PlNode(dis_TRU);

	sprintf(name, "timed-initial-literals-%d", dis_gnum_tils);
	scur_op = dis_new_dis_Pldis_Operator(name);
	scur_op->effects = dis_new_dis_PlNode(dis_AND);
    scur_op->effects->parse_vars = NULL;
    scur_op->effects->sons = $3;

	scur_op->next = dis_gloaded_ops;
	dis_gloaded_ops = scur_op;
	scur_op = NULL;
}
;







/**********************************************************************
 * some expressions used in many different rules
 **********************************************************************/
f_exp:
NUM
{ 
  $$ = dis_new_dis_Parsedis_ExpNode( NUMBER );
  $$->atom = dis_new_dis_TokenList();
  $$->atom->item = dis_new_dis_Token( strlen($1)+1 );
  strcpy( $$->atom->item, $1 );
}
|
OPEN_PAREN NAME term_star CLOSE_PAREN
{
  $$ = dis_new_dis_Parsedis_ExpNode( FHEAD );
  $$->atom = dis_new_dis_TokenList();
  $$->atom->item = dis_new_dis_Token( strlen($2)+1 );
  strcpy( $$->atom->item, $2 );
  $$->atom->next = $3;
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
ground_f_exp:
NUM
{ 
  $$ = dis_new_dis_Parsedis_ExpNode( NUMBER );
  $$->atom = dis_new_dis_TokenList();
  $$->atom->item = dis_new_dis_Token( strlen($1)+1 );
  strcpy( $$->atom->item, $1 );
}
|
OPEN_PAREN NAME name_star CLOSE_PAREN
{
  $$ = dis_new_dis_Parsedis_ExpNode( FHEAD );
  $$->atom = dis_new_dis_TokenList();
  $$->atom->item = dis_new_dis_Token( strlen($2)+1 );
  strcpy( $$->atom->item, $2 );
  $$->atom->next = $3;
}
|
OPEN_PAREN MINUS_TOK ground_f_exp CLOSE_PAREN
{
  $$ = dis_new_dis_Parsedis_ExpNode( MINUS );
  $$->leftson = $3;
}
|
OPEN_PAREN AD_TOK ground_f_exp plus_ground_f_exp_plus CLOSE_PAREN
{ 
  $$ = dis_new_dis_Parsedis_ExpNode( AD );
  $$->leftson = $3;
  $$->rightson = $4;
}
|
OPEN_PAREN MU_TOK ground_f_exp mul_ground_f_exp_plus CLOSE_PAREN
{ 
  $$ = dis_new_dis_Parsedis_ExpNode( MU );
  $$->leftson = $3;
  $$->rightson = $4;
}
|
OPEN_PAREN MINUS_TOK ground_f_exp ground_f_exp CLOSE_PAREN
{
  $$ = dis_new_dis_Parsedis_ExpNode( SU );
  $$->leftson = $3;
  $$->rightson = $4;
}
|
OPEN_PAREN DI_TOK ground_f_exp ground_f_exp CLOSE_PAREN
{
  $$ = dis_new_dis_Parsedis_ExpNode( DI );
  $$->leftson = $3;
  $$->rightson = $4;
}
|
OPEN_PAREN IS_VIOLATED_TOK NAME CLOSE_PAREN
{
  $$ = dis_new_dis_Parsedis_ExpNode(VIO);
  $$->atom = dis_new_dis_TokenList();
  $$->atom->item = dis_new_dis_Token(strlen($3) + 1);
  strcpy($$->atom->item, $3);
}
;

plus_ground_f_exp_plus:
ground_f_exp
{
  $$ = $1;
}
|
plus_ground_f_exp_plus ground_f_exp
{
  $$ = dis_new_dis_Parsedis_ExpNode( AD );
  $$->leftson = $1;
  $$->rightson = $2;
}

mul_ground_f_exp_plus:
ground_f_exp
{
  $$ = $1;
}
|
mul_ground_f_exp_plus ground_f_exp
{
  $$ = dis_new_dis_Parsedis_ExpNode( MU );
  $$->leftson = $1;
  $$->rightson = $2;
}

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
  $$->item = dis_new_dis_Token( 5 );
  $$->item = "=";
  $$->next = $3;
}
;


/**********************************************************************/
term_star:
/* empty */
{
  $$ = NULL;
}
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
  $$ = dis_new_dis_Token(strlen($1) + 1);
  strcpy($$, $1);
}
|
VARIABLE
{ 
  $$ = dis_new_dis_Token(strlen($1) + 1);
  strcpy($$, $1);
}
;


/**********************************************************************/
name_plus:
NAME
{
  $$ = dis_new_dis_TokenList();
  $$->item = dis_new_dis_Token(strlen($1) + 1);
  strcpy($$->item, $1);
}
|
NAME  name_plus
{
  $$ = dis_new_dis_TokenList();
  $$->item = dis_new_dis_Token(strlen($1) + 1);
  strcpy($$->item, $1);
  $$->next = $2;
}


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




/**********************************************************************/
predicate:
NAME
{ 
  $$ = dis_new_dis_Token(strlen($1) + 1);
  strcpy($$, $1);
}
;


/**********************************************************************/
/*
literal_name_plus:
literal_name
{
  $$ = $1;
}
|
literal_name literal_name_plus
{
   $$ = $1;
   $$->next = $2;
}*/

 
/**********************************************************************/
literal_name:
OPEN_PAREN  dis_NOT_TOK  atomic_formula_name  CLOSE_PAREN
{ 
  dis_PlNode *tmp;

  tmp = dis_new_dis_PlNode(dis_ATOM);
  tmp->atom = $3;
  $$ = dis_new_dis_PlNode(dis_NOT);
  $$->sons = tmp;
}
|
atomic_formula_name
{
  $$ = dis_new_dis_PlNode(dis_ATOM);
  $$->atom = $1;
}
;


/**********************************************************************/
atomic_formula_name:
OPEN_PAREN  predicate  name_star  CLOSE_PAREN
{ 
  $$ = dis_new_dis_TokenList();
  $$->item = $2;
  $$->next = $3;
}
;


/**********************************************************************/
name_star:
/* empty */
{ $$ = NULL; }
|
NAME  name_star
{
  $$ = dis_new_dis_TokenList();
  $$->item = dis_new_dis_Token(strlen($1) + 1);
  strcpy($$->item, $1);
  $$->next = $2;
}
;


%%


#include "lex.dis_fct_pddl.c"


/**********************************************************************
 * Functions
 **********************************************************************/


/* 
 * call	bison -pfct -bscan-fct scan-fct.y
 */



int yyerror( char *msg )

{
  fflush( stdout );
  fprintf(stderr,"\n%s: syntax error in line %d, '%s':\n", 
	  dis_gact_filename, dis_lineno, dis_fct_pddltext );

  if ( sact_err_par ) {
    fprintf(stderr, "%s%s\n", serrmsg[sact_err], sact_err_par );
  } else {
    fprintf(stderr,"%s\n", serrmsg[sact_err] );
  }

  exit( 1 );

}



void dis_load_fct_file( char *filename ) 

{

  FILE *fp;/* pointer to input files */
  char tmp[dis_MAX_LENGTH] = "";

  /* open fact file 
   */
  if( ( fp = fopen( filename, "r" ) ) == NULL ) {
    sprintf(tmp, "\nff: can't find fact file: %s\n\n", filename );
    perror(tmp);
    exit ( 1 );
  }

  dis_gact_filename = filename;
  dis_lineno = 1; 
  dis_fct_pddlin = fp;

  yyparse();

  fclose( fp );/* and close file again */

}

