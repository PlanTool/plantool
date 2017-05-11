%{
#ifdef YYDEBUG
  extern int yydebug=1;
#endif


#include <stdio.h>
#include <string.h> 
#include "bb.h"
#include "memory.h"
#include "parse.h"


#ifndef SCAN_ERR
#define SCAN_ERR
#define DOMDEF_EXPECTED            0
#define DOMAIN_EXPECTED            1
#define DOMNAME_EXPECTED           2
#define LBRACKET_EXPECTED          3
#define RBRACKET_EXPECTED          4
#define DOMDEFS_EXPECTED           5
#define REQUIREM_EXPECTED          6
#define TYPEDLIST_EXPECTED         7
#define LITERAL_EXPECTED           8
#define PRECONDDEF_UNCORRECT       9
#define TYPEDEF_EXPECTED          10
#define CONSTLIST_EXPECTED        11
#define PREDDEF_EXPECTED          12 
#define NAME_EXPECTED             13
#define VARIABLE_EXPECTED         14
#define ACTIONFUNCTOR_EXPECTED    15
#define ATOM_FORMULA_EXPECTED     16
#define EFFECT_DEF_EXPECTED       17
#define NEG_FORMULA_EXPECTED      18
#define NOT_SUPPORTED             19
#define ACTION                    20
#endif


#define NAME_STR "name\0"
#define VARIABLE_STR "variable\0"
#define STANDARD_TYPE "OBJECT\0"
 

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
  "requirement %s not supported by this IPP version",  
  "action definition is not correct",
  NULL
};


void opserr( int errno, char *par );


static int sact_err;
static char *sact_err_par = NULL;
static PlOperator *scur_op = NULL;
static Bool sis_negated = FALSE;


int supported( char *str )

{

  int i;
  char * sup[] = { ":STRIPS", ":NEGATION", ":EQUALITY",":TYPING", 
		   ":CONDITIONAL-EFFECTS", ":DISJUNCTIVE-PRECONDITIONS", 
		   ":EXISTENTIAL-PRECONDITIONS", ":UNIVERSAL-PRECONDITIONS", 
		   ":QUANTIFIED-PRECONDITIONS", ":ADL",
		   NULL };     

  for (i=0; NULL != sup[i]; i++) {
    if ( SAME == strcmp(sup[i], str) ) {
      return TRUE;
    }
  }
  
  return FALSE;

}

%}


%start file


%union {

  char string[MAX_LENGTH];
  char *pstring;
  PlNode *pPlNode;
  FactList *pFactList;
  TokenList *pTokenList;

}


%type <pPlNode> adl_effect
%type <pPlNode> adl_effect_star
%type <pPlNode> adl_goal_description
%type <pPlNode> adl_goal_description_star
%type <pTokenList> literal_term
%type <pTokenList> term_star
%type <pFactList> typed_list_name
%type <pFactList> typed_list_variable
%type <pstring> term
%type <pTokenList> atomic_formula_term
%type <pTokenList> name_plus
%type <pstring> predicate

%token DEFINE_TOK
%token DOMAIN_TOK
%token REQUIREMENTS_TOK
%token TYPES_TOK
%token EITHER_TOK
%token CONSTANTS_TOK
%token PREDICATES_TOK
%token ACTION_TOK
%token VARS_TOK
%token CONTEXT_TOK
%token IMPLIES_TOK
%token PRECONDITION_TOK
%token PARAMETERS_TOK
%token EFFECT_TOK
%token AND_TOK
%token NOT_TOK
%token WHEN_TOK
%token FORALL_TOK
%token IMPLY_TOK
%token OR_TOK
%token EXISTS_TOK
%token EQUAL_TOK
%token <string> NAME
%token <string> VARIABLE
%token <string> TYPE
%token OPEN_PAREN
%token CLOSE_PAREN

%%

/**********************************************************************/
file:
{ 
  opserr( DOMDEF_EXPECTED, NULL ); 
}
domain_definition 
;
/* can be extended to support 'addenda' and similar stuff */


/**********************************************************************/
domain_definition : 
OPEN_PAREN  DEFINE_TOK  domain_name       
{ 
  /* initialize typetree 
   */
  gglobal_type_tree_list = new_type_tree_list( STANDARD_TYPE );
}
optional_domain_defs 
{
  if ( gcmd_line.display_info >= 1 ) {
    printf("\ndomain '%s' defined\n", gdomain_name);
  }
}
;


/**********************************************************************/
domain_name :
OPEN_PAREN  DOMAIN_TOK  NAME  CLOSE_PAREN 
{ 
  gdomain_name = new_Token( strlen($3)+1 );
  strcpy( gdomain_name, $3);
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
action_def  optional_domain_defs
|
predicates_def  optional_domain_defs
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

  FactList *fl, *fl1;
  TokenList *tl, *tl1;

  if ( gpredicates_and_types ) {
    fl = gpredicates_and_types;
    while ( fl->next ) {
      fl = fl->next;
    }
    fl->next = new_FactList();
    fl = fl->next;
  } else {
    fl = new_FactList();
    gpredicates_and_types = fl;
  }
  tl = new_TokenList();
  fl->item = tl;
  tl->item = new_Token( strlen( $2 ) + 1);
  strcpy( tl->item, $2 );
  fl1 = $3;
  while ( fl1 ) {
    tl1 = new_TokenList();
    tl->next = tl1;
    tl = tl1;
    tl1->item = new_Token( strlen( fl1->item->next->item ) + 1 );
    strcpy( tl1->item, fl1->item->next->item );
    fl1 = fl1->next;
  }
  free_FactList( $3 );

}
predicates_list
;


/**********************************************************************/
require_def:
OPEN_PAREN  REQUIREMENTS_TOK 
{ 
  opserr( REQUIREM_EXPECTED, NULL ); 
}
NAME
{ 
  if ( !supported( $4 ) ) {
    opserr( NOT_SUPPORTED, $4 );
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
  if ( !supported( $1 ) ) {
    opserr( NOT_SUPPORTED, $1 );
    yyerror();
  }
}
require_key_star
;


/**********************************************************************/
types_def:
OPEN_PAREN  TYPES_TOK
{ 
  opserr( TYPEDEF_EXPECTED, NULL ); 
}
typed_list_name  CLOSE_PAREN
{ 
  add_to_type_tree( $4, main_type_tree() );
  free_FactList( $4 );
}
; 


/**********************************************************************/
constants_def:
OPEN_PAREN  CONSTANTS_TOK
{ 
  opserr( CONSTLIST_EXPECTED, NULL ); 
}
typed_list_name  CLOSE_PAREN
{ 
  gorig_constant_list = $4;
}
;


/**********************************************************************
 * actions and their optional definitions
 **********************************************************************/
action_def:
OPEN_PAREN  ACTION_TOK  
{ 
  opserr( ACTION, NULL ); 
}  
NAME
{ 
  scur_op = new_PlOperator( $4 );
}
param_def  action_def_body  CLOSE_PAREN
{
  scur_op->next = gloaded_ops;
  gloaded_ops = scur_op; 
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
  FactList *f;
  scur_op->params = $3;
  for (f = scur_op->params; f; f = f->next) {
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
  FactList *f = NULL;

  /* add vars as parameters 
   */
  if ( scur_op->params ) {
    for( f = scur_op->params; f->next; f = f->next ) {
      /* empty, get to the end of list 
       */
    }
    f->next = $3;
    f = f->next;
  } else {
    scur_op->params = $3;
  }
}
|
PRECONDITION_TOK  adl_goal_description
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



/**********************************************************************
 * Goal description providing full ADL.
 * RETURNS a tree with the connectives in the nodes and the atomic 
 * predicates in the leafs.
 **********************************************************************/
adl_goal_description:
literal_term
{ 
  if ( sis_negated ) {
    $$ = new_PlNode(NOT);
    $$->sons = new_PlNode(ATOM);
    $$->sons->atom = $1;
    sis_negated = FALSE;
  } else {
    $$ = new_PlNode(ATOM);
    $$->atom = $1;
  }
}
|
OPEN_PAREN  AND_TOK  adl_goal_description_star  CLOSE_PAREN
{ 
  $$ = new_PlNode(AND);
  $$->sons = $3;
}
|
OPEN_PAREN  OR_TOK  adl_goal_description_star  CLOSE_PAREN
{ 
  $$ = new_PlNode(OR);
  $$->sons = $3;
}
|
OPEN_PAREN  NOT_TOK  adl_goal_description  CLOSE_PAREN
{ 
  $$ = new_PlNode(NOT);
  $$->sons = $3;
}
|
OPEN_PAREN  IMPLY_TOK  adl_goal_description  adl_goal_description  CLOSE_PAREN
{ 
  PlNode *np = new_PlNode(NOT);
  np->sons = $3;
  np->next = $4;

  $$ = new_PlNode(OR);
  $$->sons = np;
}
|
OPEN_PAREN  EXISTS_TOK 
OPEN_PAREN  typed_list_variable  CLOSE_PAREN 
adl_goal_description  CLOSE_PAREN
{ 
  /* The typed_list_variable returns a FactList with two-item TokenLists, 
   * the first item is the variable and the second item its type.
   * We now have to split off this FactList into a PlNode for each 
   * variable-type TokenList. 
   */
  FactList *tl = $4, *t1;
  PlNode *pln1;
  PlNode *pln2;

  pln1 = new_PlNode(EX);
  pln1->atom = tl->item;
  $$ = pln1;

  t1 = tl;
  /* every loop gives us one quantor with one variable and its type 
   */
  while ( t1->next ) {
    t1 = t1->next;

    pln2 = new_PlNode(EX);
    pln2->atom = t1->item;
    /* append the next quantor to the sons of the previous node 
     */
    pln1->sons = pln2;
    pln1 = pln2;
  }
  pln1->sons = $6;

  t1 = tl->next;
  while ( TRUE ) {
    free ( tl );
    if ( !t1 ) break;
    tl = t1;
    t1 = tl->next;
  }

}
|
OPEN_PAREN  FORALL_TOK 
OPEN_PAREN  typed_list_variable  CLOSE_PAREN 
adl_goal_description  CLOSE_PAREN
{ 
  /* This will be handled exactly like the ex-quantor case, s.a. 
   */
  FactList *tl = $4, *t1;
  PlNode *pln1;
  PlNode *pln2;
  
  pln1 = new_PlNode(ALL);
  pln1->atom = tl->item;
  $$ = pln1;

  t1 = tl;
  /* every loop gives us one quantor with one variable and its type 
   */
  while ( t1->next ) {
    t1 = t1->next;
      
    pln2 = new_PlNode(ALL);
    pln2->atom = t1->item;
    pln1->sons = pln2;
    pln1 = pln2;
  }
  pln1->sons = $6;

  t1 = tl->next;
  while ( TRUE ) {
    free ( tl );
    if ( !t1 ) break;
    tl = t1;
    t1 = tl->next;
  }

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
literal_term
{ 
  if ( sis_negated ) {
    $$ = new_PlNode(NOT);
    $$->sons = new_PlNode(ATOM);
    $$->sons->atom = $1;
    sis_negated = FALSE;
  } else {
    $$ = new_PlNode(ATOM);
    $$->atom = $1;
  }
}
|
OPEN_PAREN  AND_TOK  adl_effect_star  CLOSE_PAREN
{ 
  $$ = new_PlNode(AND);
  $$->sons = $3;
}
|
OPEN_PAREN  NOT_TOK  adl_effect  CLOSE_PAREN
{ 
  $$ = new_PlNode(NOT);
  $$->sons = $3;
}
|
OPEN_PAREN  FORALL_TOK 
OPEN_PAREN  typed_list_variable  CLOSE_PAREN 
adl_effect  CLOSE_PAREN
{ 
  /* This will be handled exactly like quantors in the goals part, s.o. 
   */
  FactList *fl = $4, *f1;
  PlNode *pln1;
  PlNode *pln2;
  
  pln1 = new_PlNode(ALL);
  pln1->atom = fl->item;
  $$ = pln1;

  f1 = fl;
  /* every loop gives us one quantor with one variable and its type 
   */
  while ( f1->next ) {
    f1 = f1->next;
    
    pln2 = new_PlNode(ALL);
    pln2->atom = f1->item;
    pln1->sons = pln2;
    pln1 = pln2;
  }
  pln1->sons = $6;

  f1 = fl->next;
  while ( TRUE ) {
    free( fl );
    if ( !f1 ) break;
    fl = f1;
    f1 = fl->next;
  }

}
|
OPEN_PAREN  WHEN_TOK  adl_goal_description  adl_effect  CLOSE_PAREN
{
  /* This will be conditional effects in FF representation, but here
   * a formula like (WHEN p q) will be saved as:
   *  [WHEN]
   *  [sons]
   *   /  \
   * [p]  [q]
   * That means, the first son is p, and the second one is q. 
   */
  $$ = new_PlNode(WHEN);
  $3->next = $4;
  $$->sons = $3;
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
literal_term:
OPEN_PAREN  NOT_TOK  atomic_formula_term  CLOSE_PAREN
{ 
  $$ = $3;
  sis_negated = TRUE;
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
  $$ = new_TokenList();
  $$->item = $2;
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
  $$ = new_TokenList();
  $$->item = $1;
  $$->next = $2;
}
;


/**********************************************************************/
term:
NAME
{ 
  $$ = new_Token( strlen($1)+1 );
  strcpy( $$, $1 );
}
|
VARIABLE
{ 
  $$ = new_Token( strlen($1)+1 );
  strcpy( $$, $1 );
}
;


/**********************************************************************/
name_plus:
NAME
{
  $$ = new_TokenList();
  $$->item = new_Token( strlen($1)+1 );
  strcpy( $$->item, $1 );
}
|
NAME  name_plus
{
  $$ = new_TokenList();
  $$->item = new_Token( strlen($1)+1 );
  strcpy( $$->item, $1 );
  $$->next = $2;
}
;

/**********************************************************************/
predicate:
NAME
{ 
  $$ = new_Token( strlen($1)+1 );
  strcpy( $$, $1 );
}
|
EQUAL_TOK
{ 
  $$ = new_Token( strlen(EQ_STR)+1 );
  strcpy( $$, EQ_STR );
}
;


/**********************************************************************/
typed_list_name:     /* returns fact_list */
/* empty */
{ $$ = NULL; }
|
NAME  EITHER_TOK  name_plus  CLOSE_PAREN  typed_list_name
{ /* this is a very, very special case... it may mean that
   * a. a parameter has one of the types in name_plus
   * b. a type is subtype of one of the following types in name_plus 
   * => for both possibilities we use the same solution:
   * build a new type name: either_name1_name2_..._namen and check
   * if a type with this name already exists. If so then NAME has this type,
   * if not then build a new type, include it in the type tree and
   * return NAME with the new type 
   * The new artificial type is not a subtype of OBJECT because its own
   * elements must already be instances of a subtype of OBJECT 
   */
  char *s;
  TokenList *t, *t1;
  type_tree tt;
  type_tree_list rootl, *st;
  
  s = new_Token( MAX_LENGTH );
  strcpy( s, EITHER_STR );
  for ( t = $3; t; t = t->next ) {
    strcat( s, CONNECTOR );
    strcat( s, t->item );
  }
  tt = NULL;
  for ( rootl = gglobal_type_tree_list; rootl; rootl = rootl->next ) {
    if ((tt = find_branch( s, rootl->item))) break;
  }
  if ( !tt ) {/* the type doesn't exist yet, so build it */
    rootl = new_type_tree_list( s );
    rootl->next = gglobal_type_tree_list;
    st = &(rootl->item->sub_types);
    for ( t = $3; t; t = t->next ) {
      if ((tt = find_branch( t->item, main_type_tree()))) {
	*st = new_type_tree_list( NULL );
	(*st)->item = tt;
	st = &((*st)->next);
      }
    }  
  }

  t = $3;
  while ( t ) {
    t1 = t->next;
    free( t->item );
    free( t );
    t = t1;
  }

  /* now do the simple stuff: return a name with a (quite complicated)
   * type 
   */
  $$ = new_FactList();
  $$->item = new_TokenList();
  $$->item->item = new_Token( strlen($1)+1 );
  strcpy( $$->item->item, $1 );
  $$->item->next = new_TokenList();
  $$->item->next->item = s;
  $$->next = $5;
}
|
NAME  TYPE  typed_list_name   /* end of list for one type */
{
  $$ = new_FactList();
  $$->item = new_TokenList();
  $$->item->item = new_Token( strlen($1)+1 );
  strcpy( $$->item->item, $1 );
  $$->item->next = new_TokenList();
  $$->item->next->item = new_Token( strlen($2)+1 );
  strcpy( $$->item->next->item, $2 );
  $$->next = $3;
}
|
NAME  typed_list_name        /* a list element (gets type from next one) */
{
  $$ = new_FactList();
  $$->item = new_TokenList();
  $$->item->item = new_Token( strlen($1)+1 );
  strcpy( $$->item->item, $1 );
  $$->item->next = new_TokenList();
  if ( $2 ) {/* another element (already typed) is following */
    char *s = $2->item->next->item;
    $$->item->next->item = new_Token(strlen(s) + 1);
    strcpy( $$->item->next->item, s ); /* same type as the next one */
    $$->next = $2;
  } else {/* no further element - it must be an untyped list */
    $$->item->next->item = new_Token( strlen(STANDARD_TYPE)+1 );
    strcpy( $$->item->next->item, STANDARD_TYPE );
    $$->next = $2;
  }
}
;


/***********************************************/
typed_list_variable:     /* returns fact_list */
/* empty */
{ $$ = NULL; }
|
VARIABLE  EITHER_TOK  name_plus  CLOSE_PAREN  typed_list_variable
{ /* this is a very, very special case... it may mean that
   * a parameter has one of the types in name_plus
   * => build a new type name: either_name1_name2_..._namen and check
   * if a type with this name already exists. If so then NAME has this type,
   * if not then build a new type, include it in the type tree and
   * return NAME with the new type 
   * The new artificial type is not a subtype of OBJECT because its own
   * elements must already be instances of a subtype of OBJECT 
   */
  char *s;
  TokenList *t, *t1;
  type_tree tt;
  type_tree_list rootl, *st;
  
  s = new_Token( MAX_LENGTH );
  strcpy( s, EITHER_STR );
  for ( t = $3; t; t = t->next ) {
    strcat( s, CONNECTOR );
    strcat( s, t->item );
  }
  tt = NULL;
  for ( rootl = gglobal_type_tree_list; rootl; rootl = rootl->next ) {
    if ((tt = find_branch( s, rootl->item))) break;
  }
  if ( !tt ) {/* the type doesn't exist yet, so build it */
    rootl = new_type_tree_list( s );
    rootl->next = gglobal_type_tree_list;
    st = &(rootl->item->sub_types);
    for ( t = $3; t; t = t->next ) {
      if ((tt = find_branch( t->item, main_type_tree()))) {
	*st = new_type_tree_list( NULL );
	(*st)->item = tt;
	st = &((*st)->next);
      }
    }  
    gglobal_type_tree_list = rootl;
  }

  t = $3;
  while ( t ) {
    t1 = t->next;
    free( t->item );
    free( t );
    t = t1;
  }

  /* now do the simple stuff: return a name with a (quite complicated)
   * type 
   */
  $$ = new_FactList();
  $$->item = new_TokenList();
  $$->item->item = new_Token( strlen($1)+1 );
  strcpy( $$->item->item, $1 );
  $$->item->next = new_TokenList();
  $$->item->next->item = s;
  $$->next = $5;
}
|
VARIABLE  TYPE  typed_list_variable   /* end of list for one type */
{
  $$ = new_FactList();
  $$->item = new_TokenList();
  $$->item->item = new_Token( strlen($1)+1 );
  strcpy( $$->item->item, $1 );
  $$->item->next = new_TokenList();
  $$->item->next->item = new_Token( strlen($2)+1 );
  strcpy( $$->item->next->item, $2 );
  $$->next = $3;
}
|
VARIABLE  typed_list_variable    /* a list element (gets type from next one) */
{
  $$ = new_FactList();
  $$->item = new_TokenList();
  $$->item->item = new_Token( strlen($1)+1 );
  strcpy( $$->item->item, $1 );
  $$->item->next = new_TokenList();
  if ( $2 ) {/* another element (already typed) is following */
    char *s = $2->item->next->item;
    int l = strlen( s );
    $$->item->next->item = new_Token( l+1 );
    strcpy( $$->item->next->item, s ); /* same type as the next one */
    $$->next = $2;
  } else {/* no further element - it must be an untyped list */
    $$->item->next->item = new_Token( strlen(STANDARD_TYPE)+1 );
    strcpy( $$->item->next->item, STANDARD_TYPE );
    $$->next = $2;
  }
}
;

%%
#include "lex.ops_pddl.c"


/**********************************************************************
 * Functions
 **********************************************************************/

/* 
 * call	bison -pops -bscan-ops scan-ops.y
 */

void opserr( int errno, char *par )

{

  sact_err = errno;

  if ( sact_err_par ) {
    free(sact_err_par);
  }
  if ( par ) {
    sact_err_par = new_Token(strlen(par)+1);
    strcpy(sact_err_par, par);
  } else {
    sact_err_par = NULL;
  }

}
  


int yyerror( char *msg )

{

  fflush(stdout);
  fprintf(stderr, "\n%s: syntax error in line %d, '%s':\n", 
	  gact_filename, lineno, yytext);

  if ( NULL != sact_err_par ) {
    fprintf(stderr, "%s %s\n", serrmsg[sact_err], sact_err_par);
  } else {
    fprintf(stderr, "%s\n", serrmsg[sact_err]);
  }

  exit( 1 );

}



void load_ops_file( char *filename )

{

  FILE * fp;/* pointer to input files */
  char tmp[MAX_LENGTH] = "";

  /* open operator file 
   */
  if( ( fp = fopen( filename, "r" ) ) == NULL ) {
    sprintf(tmp, "\nff: can't find operator file: %s\n\n", filename );
    perror(tmp);
    exit( 1 );
  }

  gact_filename = filename;
  lineno = 1; 
  yyin = fp;

  yyparse();

  fclose( fp );/* and close file again */

}
