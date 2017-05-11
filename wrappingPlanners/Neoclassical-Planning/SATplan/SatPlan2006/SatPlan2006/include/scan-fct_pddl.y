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
#define DEFINE_EXPECTED            0
#define PROBLEM_EXPECTED           1
#define PROBNAME_EXPECTED          2
#define LBRACKET_EXPECTED          3
#define RBRACKET_EXPECTED          4
#define DOMDEFS_EXPECTED           5
#define REQUIREM_EXPECTED          6
#define TYPEDLIST_EXPECTED         7
#define DOMEXT_EXPECTED            8
#define DOMEXTNAME_EXPECTED        9
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
#define SITUATION_EXPECTED        20
#define SITNAME_EXPECTED          21
#define BDOMAIN_EXPECTED          22
#define BADDOMAIN                 23
#define INIFACTS                  24
#define GOALDEF                   25
#define ADLGOAL                   26
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
  "requirement %s not supported by this IPP version",  
  "'situation' expected",
  "situation name expected",
  "':domain' expected",
  "this problem needs another domain file",
  "initial facts definition expected",
  "goal definition expected",
  "first order logic expression expected",
  NULL
};


void fcterr( int errno, char *par );


static int sact_err;
static char *sact_err_par = NULL;
static Bool sis_negated = FALSE;

%}


%start file


%union {

  char string[MAX_LENGTH];
  char* pstring;
  PlNode* pPlNode;
  FactList* pFactList;
  TokenList* pTokenList;

}


%type <pstring> problem_name
%type <pPlNode> adl_goal_description
%type <pPlNode> adl_goal_description_star
%type <pPlNode> literal_name_plus
%type <pPlNode> literal_name
%type <pTokenList> literal_term
%type <pTokenList> atomic_formula_term
%type <pTokenList> term_star
%type <pstring> term
%type <pTokenList> name_star
%type <pTokenList> atomic_formula_name
%type <pstring> predicate
%type <pFactList> typed_list_name
%type <pFactList> typed_list_variable
%type <pTokenList> name_plus

%token DEFINE_TOK
%token PROBLEM_TOK
%token SITUATION_TOK
%token BSITUATION_TOK
%token OBJECTS_TOK
%token BDOMAIN_TOK
%token INIT_TOK
%token GOAL_TOK
%token AND_TOK
%token NOT_TOK
%token <string> NAME
%token <string> VARIABLE
%token <string> TYPE
%token EQUAL_TOK
%token FORALL_TOK
%token IMPLY_TOK
%token OR_TOK
%token EXISTS_TOK
%token EITHER_TOK
%token OPEN_PAREN
%token CLOSE_PAREN

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
  fcterr( PROBNAME_EXPECTED, NULL ); 
}
problem_name  problem_defs  CLOSE_PAREN                 
{  
  gproblem_name = $4;
  if ( gcmd_line.display_info >= 1 ) {
    printf("\nproblem '%s' defined\n", gproblem_name);
  }
}
;


/**********************************************************************/
problem_name :
OPEN_PAREN  PROBLEM_TOK  NAME  CLOSE_PAREN        
{ 
  $$ = new_Token( strlen($3)+1 );
  strcpy($$, $3);
}
;


/**********************************************************************/
base_domain_name :
OPEN_PAREN  BDOMAIN_TOK  NAME  CLOSE_PAREN
{ 
  if ( SAME != strcmp($3, gdomain_name) ) {
    fcterr( BADDOMAIN, NULL );
    yyerror();
  }
}
;

/**********************************************************************/
problem_defs:
/* empty */
|
objects_def  problem_defs
|
init_def  problem_defs
|
goal_def  problem_defs
|
base_domain_name  problem_defs
;


/**********************************************************************/
objects_def:
OPEN_PAREN  OBJECTS_TOK  typed_list_name  CLOSE_PAREN
{ 
  FactList *f;
  type_tree root;
  type_tree_list ttl;

  if ( gorig_constant_list ) {
    for ( f = gorig_constant_list; f->next; f = f->next );
    f->next = $3;
  } else {
    gorig_constant_list = $3;
  }
  /* maybe new types are introduced here and must be added to the
   * type tree 
   */
  for ( f = gorig_constant_list; f; f = f->next ) {
    root = main_type_tree(); /* do not search the EITHER trees */
    if ( !find_branch( f->item->next->item, root ) ) { /* type of this constant is not known yet */
      ttl = new_type_tree_list( f->item->next->item );
      ttl->next = root->sub_types;
      root->sub_types = ttl;
    }
  }
}
;


/**********************************************************************/
init_def:
OPEN_PAREN  INIT_TOK
{
  fcterr( INIFACTS, NULL ); 
}
literal_name_plus  CLOSE_PAREN
{
  gorig_initial_facts = new_PlNode(AND);
  gorig_initial_facts->sons = $4;
}
;


/**********************************************************************/
goal_def:
OPEN_PAREN  GOAL_TOK
{ 
  fcterr( GOALDEF, NULL ); 
}
adl_goal_description  CLOSE_PAREN
{
  $4->next = gorig_goal_facts;
  gorig_goal_facts = $4;
}
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
{
  $$ = NULL;
}
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
  $$ = new_Token(strlen($1) + 1);
  strcpy($$, $1);
}
|
VARIABLE
{ 
  $$ = new_Token(strlen($1) + 1);
  strcpy($$, $1);
}
;


/**********************************************************************/
name_plus:
NAME
{
  $$ = new_TokenList();
  $$->item = new_Token(strlen($1) + 1);
  strcpy($$->item, $1);
}
|
NAME  name_plus
{
  $$ = new_TokenList();
  $$->item = new_Token(strlen($1) + 1);
  strcpy($$->item, $1);
  $$->next = $2;
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
  TokenList *t;
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
    if ((tt = find_branch( s, rootl->item ))) break;
  }

  if ( !tt ) { /* the type doesn't exist yet, so build it */
    rootl = new_type_tree_list( s );
    rootl->next = gglobal_type_tree_list;
    st = &(rootl->item->sub_types);
    for ( t = $3; t; t = t->next ) {
      if ((tt = find_branch(t->item, main_type_tree()))) {
	*st = new_type_tree_list( NULL );
	(*st)->item = tt;
	st = &((*st)->next);
      }
    }  
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
    int l = strlen( s ) + 1;
    $$->item->next->item = new_Token( l );
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
  TokenList *t;
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
  if ( !tt ) { /* the type doesn't exist yet, so build it */
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


/**********************************************************************/
predicate:
NAME
{ 
  $$ = new_Token(strlen($1) + 1);
  strcpy($$, $1);
}
|
EQUAL_TOK
{ 
  $$ = new_Token( strlen(EQ_STR)+1 );
  strcpy( $$, EQ_STR );
}
;


/**********************************************************************/
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
}
;
 
/**********************************************************************/
literal_name:
OPEN_PAREN  NOT_TOK  atomic_formula_name  CLOSE_PAREN
{ 
  PlNode *tmp;

  tmp = new_PlNode(ATOM);
  tmp->atom = $3;
  $$ = new_PlNode(NOT);
  $$->sons = tmp;
}
|
atomic_formula_name
{
  $$ = new_PlNode(ATOM);
  $$->atom = $1;
}
;


/**********************************************************************/
atomic_formula_name:
OPEN_PAREN  predicate  name_star  CLOSE_PAREN
{ 
  $$ = new_TokenList();
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
  $$ = new_TokenList();
  $$->item = new_Token(strlen($1) + 1);
  strcpy($$->item, $1);
  $$->next = $2;
}
;


%%


#include "lex.fct_pddl.c"


/**********************************************************************
 * Functions
 **********************************************************************/


/* 
 * call	bison -pfct -bscan-fct scan-fct.y
 */
void fcterr( int errno, char *par ) {

  sact_err = errno;

  if ( sact_err_par ) {
    free( sact_err_par );
  }
  if ( par ) {
    sact_err_par = new_Token( strlen(par)+1 );
    strcpy( sact_err_par, par);
  } else {
    sact_err_par = NULL;
  }

}



int yyerror( char *msg )

{
  fflush( stdout );
  fprintf(stderr,"\n%s: syntax error in line %d, '%s':\n", 
	  gact_filename, lineno, yytext );

  if ( sact_err_par ) {
    fprintf(stderr, "%s%s\n", serrmsg[sact_err], sact_err_par );
  } else {
    fprintf(stderr,"%s\n", serrmsg[sact_err] );
  }

  exit( 1 );

}



void load_fct_file( char *filename ) 

{

  FILE *fp;/* pointer to input files */
  char tmp[MAX_LENGTH] = "";

  /* open fact file 
   */
  if( ( fp = fopen( filename, "r" ) ) == NULL ) {
    sprintf(tmp, "\nff: can't find fact file: %s\n\n", filename );
    perror(tmp);
    exit ( 1 );
  }

  gact_filename = filename;
  lineno = 1; 
  yyin = fp;

  yyparse();

  fclose( fp );/* and close file again */

}

