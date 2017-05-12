%{
/*
**
** Universidad Simon Bolivar, 1999, 2000 (c)
** Blai Bonet and Hector Geffner. 1999, 2000.
**
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <limits.h>
#include <ulimit.h>
#include <fcntl.h>
#include <assert.h>

#include "parser.h"
#include "planner.h"


/* global data */
static int                domainParsed = 0;
static int                maxParameters = 0;
static int                numberObjects = 0;
static idList_t *         types = NULL;
static idList_t *         objects = NULL;
static schema_t *         schemas = NULL;
static formula_t *        predicates = NULL;
static formula_t *        initialSituation = NULL;
static formula_t *        goalSituation = NULL;
static formula_t *        instantiated;


/* prototypes */
static void     checkRequirement( int );
static char *   translate( char * );
static void     printFormula( formula_t * );
static void     printAtomicFormula( atomicFormula_t * ); 
       int      evaluateFormula( formula_t *, atom_t *, int *, int );
%}


%token   LEFTPAR RIGHTPAR NAME TWODOTS HYPHEN EITHER
%token   DEFINE DOMAIN REQUIREMENTS CONSTANTS PREDICATES
%token   QUESTION STRIPS EQUALITY CONDITIONAL
%token   DOM_AXIOMS DISJUNCTIVE ADL 
%token   E_PRECONDITIONS U_PRECONDITIONS
%token   AND NOT FORALL WHEN EQUAL
%token   ACTION PARAMETERS PRECONDITION EFFECT
%token   PROBLEM INIT GOAL LENGTH SITUATION OBJECTS
%token   SERIAL PARALLEL INTEGER
%token   TYPING TYPES


%type    <integer>          require_key
%type    <ident>            NAME name predicate variable term
%type    <idlist>           list_name typed_list_name plain_or_typed_list_name 
%type    <idlist>           list_var typed_list_var plain_or_typed_list_var
%type    <idlist>           list_term list_parameters
%type    <idlist>           type types_def
%type    <formula>          list_atomic_formula_skeleton list_literal list_atomic_formula_name
%type    <formula>          effect_formula one_effect_formula list_one_effect_formula
%type    <formula>          formula list_formula list_atomic_effect
%type    <formula>          goal_formula list_goal_formula
%type    <atomicFormula>    atomic_formula_skeleton
%type    <atomicFormula>    literal atomic_formula_term atomic_formula_name
%type    <body>             schema_body list_schema_body


%start   start


%%


start             :  list_domain_problem
                  ;

list_domain_problem
                  :  list_domain_problem problem
                  |  list_domain_problem domain
                  |  /* empty */
                  ;

domain            :  LEFTPAR DEFINE LEFTPAR DOMAIN name RIGHTPAR
                         {
			   _low_domainName = $5;
			   strcpy( _low_domainName, translate( _low_domainName ) );
			 }
                       LEFTPAR list_def
                         {
			   identifyObjects();
			   tagFormulaList( initialSituation );
			   tagFormulaList( goalSituation );
			 }
                       list_structure_def
                         {
			   schema_t *schema;
			   idList_t *var;
			   int i;

			   /* how much parameters can an schema have? */
			   for( schema = schemas; schema != NULL; schema = schema->next )
			     {
			       for( var = schema->vars, i = 0; var != NULL; var = var->next, ++i );
			       maxParameters = (maxParameters > i ? maxParameters : i);
			     }
			   generateNames();
			 }
                     RIGHTPAR
                       {
			 domainParsed = 1;
		       }
                  ;

list_def          :  list_def definition LEFTPAR
                  |  definition LEFTPAR
                  ; 

definition        :  REQUIREMENTS require_def
                  |  TYPES types_def
                  |  CONSTANTS constants_def
                  |  PREDICATES predicates_def
                  ;

require_def       :  list_require_key RIGHTPAR
                  ;

list_require_key  :  list_require_key require_key
                       {
			 checkRequirement( $2 );
		       }
                  |  require_key
                       {
			 checkRequirement( $1 );
		       }
                  ;

require_key       :  STRIPS
                       {
			 $$ = STRIPS;
		       }
                  |  TYPING
                       {
			 $$ = TYPING;
		       }
                  |  EQUALITY
                       {
			 $$ = EQUALITY;

			 /* create the equal predicate */
			 predicates = (formula_t*)malloc( sizeof( formula_t ) );
			 assert( predicates != NULL );
			 predicates->next = NULL;
			 predicates->type = ATOMIC_FORMULA;
			 predicates->u.atomic = (atomicFormula_t*)malloc( sizeof( atomicFormula_t ) );	
			 assert( predicates->u.atomic != NULL );
			 predicates->u.atomic->id = 0;
			 predicates->u.atomic->neg = 0;
			 predicates->u.atomic->equal = 1;
			 predicates->u.atomic->name = strdup( "=" );
			 predicates->u.atomic->args = NULL;
		       }
                  |  CONDITIONAL
                       {
			 $$ = CONDITIONAL;
		       }
                  |  DOM_AXIOMS
                       {
			 $$ = DOM_AXIOMS;
		       }
                  |  DISJUNCTIVE
                       {
			 $$ = DISJUNCTIVE;
		       }
                  |  ADL
                       {
			 $$ = ADL;
		       }
                  |  E_PRECONDITIONS
                       {
			 $$ = E_PRECONDITIONS;
		       }
                  |  U_PRECONDITIONS
                       {
			 $$ = U_PRECONDITIONS;
		       }
                  ;

types_def         :  plain_or_typed_list_name RIGHTPAR
                       {
			 types = $1;
		       }
                  ;

constants_def     :  plain_or_typed_list_name RIGHTPAR
                       {
			 idList_t *object;
			 if( objects == NULL )
			   objects = $1;
			 else
			   {
			     for( object = objects; object->next != NULL; object = object->next );
			     object->next = $1;
			   }
		       }
                  ;

type              :  name
                       {
			 $$ = (idList_t*) malloc( sizeof( idList_t ) );
			 assert( $$ != NULL );
			 $$->id = 0;
			 $$->name = $1;
			 $$->type = NULL;
			 $$->next = NULL;
		       }
                  |  LEFTPAR EITHER list_name RIGHTPAR
                       {
			 $$ = $3;
		       }
                  ;

plain_or_typed_list_name
                  :  typed_list_name
                  |  list_name
                  ;

plain_or_typed_list_var
                  :  typed_list_var
                  |  list_var
                  ;

typed_list_name   :  list_name HYPHEN type typed_list_name 
                       {
			 idList_t *name;
			 for( name = $1; name->next != NULL; name = name->next )
			   name->type = $3;
			 name->type = $3;
			 name->next = $4;
			 $$ = $1;
		       }
                  |  list_name HYPHEN type
                       {
			 idList_t *name;
			 for( name = $1; name != NULL; name = name->next )
			   name->type = $3;
			 $$ = $1;
		       }
                  ;

typed_list_var    :  list_var HYPHEN type typed_list_var 
                       {
			 idList_t *name;
			 for( name = $1; name->next != NULL; name = name->next )
			   name->type = $3;
			 name->type = $3;
			 name->next = $4;
			 $$ = $1;
		       }
                  |  list_var HYPHEN type
                       {
			 idList_t *name;
			 for( name = $1; name != NULL; name = name->next )
			   name->type = $3;
			 $$ = $1;
		       }
                  ;

list_name         :  name list_name
                       {
			 $$ = (idList_t*) malloc( sizeof( idList_t ) );
			 assert( $$ != NULL );
			 $$->id = 0;
			 $$->name = $1;
			 $$->type = NULL;
			 $$->next = $2;
		       }
                  |  name
                       {
			 $$ = (idList_t*) malloc( sizeof( idList_t ) );
			 assert( $$ != NULL );
			 $$->id = 0;
			 $$->name = $1;
			 $$->type = NULL;
			 $$->next = NULL;
		       }
                  ;

list_var          :  variable list_var
                       {
			 $$ = (idList_t*) malloc( sizeof( idList_t ) );
			 assert( $$ != NULL );
			 $$->id = 0;
			 $$->name = $1;
			 $$->type = NULL;
			 $$->next = $2;
		       }
                  |  /* empty */
                       {  
			 $$ = NULL;
		       }
                  ;

name              :  NAME
                  ;

variable          :  QUESTION name
                       {
			 $$ = (char*) malloc( strlen( $2 ) + 2 );
			 assert( $$ != NULL );
			 $$[0] = '\0';
			 strcat( $$, "?" );
			 strcat( $$, $2 );
			 free( $2 );
		       }
                  ;

predicates_def    :  list_atomic_formula_skeleton RIGHTPAR
                       {
			 int i;
			 formula_t *p;
			 if( predicates == NULL )
			   predicates = $1;
			 else
			   {
			     for( p = predicates; p->next != NULL; p = p->next );
			     p->next = $1;
			   }
			 for( p = predicates, i = 1; p != NULL; p = p->next )
			   p->u.atomic->id = i++;
                       }
                  ;

list_atomic_formula_skeleton 
                  :  list_atomic_formula_skeleton atomic_formula_skeleton
                       {
			 $$ = (formula_t*)malloc( sizeof( formula_t ) );
			 assert( $$ != NULL );
			 $$->type = ATOMIC_FORMULA;
			 $$->u.atomic = $2;
			 $$->next = $1;
		       }
                  |  atomic_formula_skeleton
                       {
			 $$ = (formula_t*)malloc( sizeof( formula_t ) );
			 assert( $$ != NULL );
			 $$->type = ATOMIC_FORMULA;
			 $$->u.atomic = $1;
			 $$->next = NULL;
		       }
                  ;

atomic_formula_skeleton 
                  :  LEFTPAR predicate plain_or_typed_list_var RIGHTPAR
                       {
			 $$ = (atomicFormula_t*) malloc( sizeof( atomicFormula_t ) );
			 assert( $$ != NULL );
			 $$->id = -1;
			 $$->neg = 0;
			 $$->name = $2;
			 $$->equal = 0;
			 $$->args = $3;
		       }
                  ;

predicate         :  name
                  |  EQUAL
                       {
			 $$ = "=";
		       }
                  ;

list_structure_def 
                  :  list_structure_def LEFTPAR structure_def RIGHTPAR
                  |  structure_def RIGHTPAR
                  ;

structure_def     :  schema_def
                  ;

schema_def        :  ACTION name list_schema_body
                       {
			 formula_t *formula, *tmp;
			 schema_t *schema;
			 body_t *body;

			 schema = (schema_t*) malloc( sizeof( schema_t ) );
			 assert( schema != NULL );
			 schema->name = $2;
			 schema->next = NULL;

			 for( body = $3; body != NULL; body = body->next )
			   switch( body->type )
			     {
			     case PARAMETERS:
			       schema->vars = body->data.parameters;
			       break;
			     case PRECONDITION:
			       schema->prec = body->data.precondition;
			       break;
			     case EFFECT:
			       schema->add = NULL;
			       schema->del = NULL;
			       schema->when = NULL;
			       schema->forall = NULL;
			       formula = body->data.effect;
			       while( formula != NULL )
				 {
				   tmp = formula->next;
				   switch( formula->type )
				     {
				     case ATOMIC_FORMULA:
				       if( !formula->u.atomic->neg )
					 {
					   formula->next = schema->add;
					   schema->add = formula;
					 }
				       else
					 {
					   formula->next = schema->del;
					   schema->del = formula;
					 }
				       break;
				     case WHEN_FORMULA:
				       formula->next = schema->when;
				       schema->when = formula;
				       break;
				     case FORALL_FORMULA:
				       formula->next = schema->forall;
				       schema->forall = formula;
				       break;
				     }
				   formula = tmp;
				 }
			       break;
			     }

			 /* postprocessing */
			 schemaPostProcessing( schema );

			 /* identification */
			 idMatchArgsVars( schema->prec, schema->vars );
			 idMatchArgsVars( schema->add, schema->vars );
			 idMatchArgsVars( schema->del, schema->vars );
			 idMatchArgsVars( schema->when, schema->vars );

			 /* link schemas */
			 schema->next = schemas;
			 schemas = schema;

#if 0
			 {
			   idList_t *id;
			   fprintf( stderr, "schema %s after postprocessing:\n", schema->name );
			   fprintf( stderr, "  Params = " );
			   for( id = schema->vars; id != NULL; id = id->next )
			     fprintf( stderr, "%s ", id->name );
			   fprintf( stderr, "\n  Prec = " );
			   printFormula( schema->prec );
			   fprintf( stderr, "\n  Add = " );
			   printFormula( schema->add );
			   fprintf( stderr, "\n  Del = " );
			   printFormula( schema->del );
			   fprintf( stderr, "\n  When = " );
			   printFormula( schema->when );
			   fprintf( stderr, "\n" );
			 }
#endif

			 /* insert schema into schemaTable */
			 _low_schemaTable[numberSchema++] = schema;
		       }
                  ;

list_schema_body  :  list_schema_body schema_body
                       {
			 $2->next = $1;
			 $$ = $2;
		       }
                  |  /* empty */
                       {
			 $$ = NULL;
		       }
                  ;

schema_body       :  PARAMETERS LEFTPAR list_parameters RIGHTPAR
                       {
			 $$ = (body_t*) malloc( sizeof( body_t ) );
			 assert( $$ != NULL );
			 $$->type = PARAMETERS;
			 $$->data.parameters = $3;
		       }
                  |  PRECONDITION formula
                       {
			 $$ = (body_t*) malloc( sizeof( body_t ) );
			 assert( $$ != NULL );
			 $$->type = PRECONDITION;
			 $$->data.precondition = $2;
		       }
                  |  EFFECT effect_formula
                       {
			 $$ = (body_t*) malloc( sizeof( body_t ) );
			 assert( $$ != NULL );
			 $$->type = EFFECT;
			 $$->data.effect = $2;
		       }
                  ;

list_parameters   :  plain_or_typed_list_var
                       {
			 int i;
			 idList_t *var;

			 i = 1;
			 for( var = $1; var != NULL; var = var->next )
			   var->id = i++;
			 $$ = $1;
		       }
                  ;

list_formula      :  list_formula formula
                       {
			 $2->next = $1;
			 $$ = $2;
		       }
                  |  formula
                  ;

formula           :  literal
                       {
			 $$ = (formula_t*)malloc( sizeof( formula_t ) );
			 assert( $$ != NULL );
			 $$->type = ATOMIC_FORMULA;
			 $$->u.atomic = $1;
			 $$->next = NULL;
		       }
                  |  LEFTPAR AND list_formula RIGHTPAR
                       {
			 $$ = $3;
		       }
                  ;

literal           :  atomic_formula_term
                  |  LEFTPAR NOT atomic_formula_term RIGHTPAR
                       {
			 $3->neg = 1;
			 $$ = $3;
		       }
                  ;

list_literal      :  list_literal literal
                       {
			 $$ = (formula_t*)malloc( sizeof( formula_t ) );
			 assert( $$ != NULL );
			 $$->type = ATOMIC_FORMULA;
			 $$->u.atomic = $2;
			 $$->next = $1;
		       }
                  |  literal
                       {
			 $$ = (formula_t*)malloc( sizeof( formula_t ) );
			 assert( $$ != NULL );
			 $$->type = ATOMIC_FORMULA;
			 $$->u.atomic = $1;
			 $$->next = NULL;
		       }
                  ;

list_atomic_formula_name
                  :  list_atomic_formula_name atomic_formula_name
                       {
			 $$ = (formula_t*)malloc( sizeof( formula_t ) );
			 assert( $$ != NULL );
			 $$->type = ATOMIC_FORMULA;
			 $$->u.atomic = $2;
			 $$->next = $1;
		       }
                  |  atomic_formula_name
                       {
			 $$ = (formula_t*)malloc( sizeof( formula_t ) );
			 assert( $$ != NULL );
			 $$->type = ATOMIC_FORMULA;
			 $$->u.atomic = $1;
			 $$->next = NULL;
		       }
                  ;

atomic_formula_name 
                  :  LEFTPAR predicate list_name RIGHTPAR 
                       {
			 $$ = (atomicFormula_t*) malloc( sizeof( atomicFormula_t ) );
			 assert( $$ != NULL );
			 $$->id = -1;
			 $$->neg = 0;
			 $$->equal = 0;
			 $$->name = $2;
			 $$->args = $3;
		       }
                  |  LEFTPAR predicate RIGHTPAR 
                       {
			 $$ = (atomicFormula_t*) malloc( sizeof( atomicFormula_t ) );
			 assert( $$ != NULL );
			 $$->id = -1;
			 $$->neg = 0;
			 $$->equal = 0;
			 $$->name = $2;
			 $$->args = NULL;
		       }
                  ;

atomic_formula_term 
                  :  LEFTPAR predicate list_term RIGHTPAR 
                       {
			 $$ = (atomicFormula_t*) malloc( sizeof( atomicFormula_t ) );
			 assert( $$ != NULL );
			 $$->id = -1;
			 $$->neg = 0;
			 $$->equal = 0;
			 $$->name = $2;
			 $$->args = $3;
		       }
                  ;

list_term         :  term list_term
                       {
			 $$ = (idList_t*) malloc( sizeof( idList_t ) );
			 assert( $$ != NULL );
			 $$->id = 0;
			 $$->name = $1;
			 $$->type = NULL;
			 $$->next = $2;
		       }
                  |  /* empty */
                       {  
			 $$ = NULL;
		       }
                  ;

term              :  name
                  |  variable
                  ;

effect_formula    :  one_effect_formula
                  ;

list_one_effect_formula
                  :  list_one_effect_formula one_effect_formula
                       {
			 $2->next = $1;
			 $$ = $2;
		       }
                  |  one_effect_formula
                  ;

one_effect_formula
                  :  literal
                       {
			 $$ = (formula_t*)malloc( sizeof( formula_t ) );
			 assert( $$ != NULL );
			 $$->type = ATOMIC_FORMULA;
			 $$->next = NULL;
			 $$->u.atomic = $1;
		       }
                  |  LEFTPAR AND list_one_effect_formula RIGHTPAR
                       {
			 $$ = $3;
		       }
                  |  LEFTPAR WHEN formula list_atomic_effect RIGHTPAR
                       {
			 formula_t *formula, *next;

			 $$ = (formula_t*)malloc( sizeof( formula_t ) );
			 assert( $$ != NULL );
			 $$->type = WHEN_FORMULA;
			 $$->next = NULL;
			 $$->u.when.condition = $3;
			 $$->u.when.addEffect = NULL;
			 $$->u.when.delEffect = NULL;

			 for( formula = $4; formula != NULL; formula = next )
			   {
			     next = formula->next;
			     assert( formula->type == ATOMIC_FORMULA );
			     if( formula->u.atomic->neg == 0 )
			       {
				 formula->next = $$->u.when.addEffect;
				 $$->u.when.addEffect = formula;
			       }
			     else
			       {
				 formula->next = $$->u.when.delEffect;
				 $$->u.when.delEffect = formula;
			       }
			   }
		       }
                  |  LEFTPAR FORALL LEFTPAR plain_or_typed_list_var RIGHTPAR list_atomic_effect RIGHTPAR
                       {
			 formula_t *formula, *next;

			 $$ = (formula_t*)malloc( sizeof( formula_t ) );
			 assert( $$ != NULL );
			 $$->type = FORALL_FORMULA;
			 $$->next = NULL;
			 $$->u.forall.vars = $4;
			 $$->u.forall.addEffect = NULL;
			 $$->u.forall.delEffect = NULL;
			 $$->u.forall.whenEffect = NULL;

			 for( formula = $6; formula != NULL; formula = next )
			   {
			     next = formula->next;
			     assert( formula->type == ATOMIC_FORMULA );
			     if( formula->u.atomic->neg == 0 )
			       {
				 formula->next = $$->u.forall.addEffect;
				 $$->u.forall.addEffect = formula;
			       }
			     else
			       {
				 formula->next = $$->u.forall.delEffect;
				 $$->u.forall.delEffect = formula;
			       }
			   }
		       }
                  |  LEFTPAR FORALL LEFTPAR plain_or_typed_list_var RIGHTPAR
                         LEFTPAR WHEN formula list_atomic_effect RIGHTPAR RIGHTPAR
                       {
			 formula_t *formula, *next;

			 $$ = (formula_t*)malloc( sizeof( formula_t ) );
			 assert( $$ != NULL );
			 $$->type = FORALL_FORMULA;
			 $$->next = NULL;
			 $$->u.forall.vars = $4;
			 $$->u.forall.addEffect = NULL;
			 $$->u.forall.delEffect = NULL;
			 $$->u.forall.whenEffect = (formula_t*)malloc( sizeof( formula_t ) );
			 assert( $$->u.forall.whenEffect != NULL );
			 $$->u.forall.whenEffect->type = WHEN_FORMULA;
			 $$->u.forall.whenEffect->next = NULL;
			 $$->u.forall.whenEffect->u.when.condition = $8;
			 $$->u.forall.whenEffect->u.when.addEffect = NULL;
			 $$->u.forall.whenEffect->u.when.delEffect = NULL;

			 for( formula = $9; formula != NULL; formula = next )
			   {
			     next = formula->next;
			     assert( formula->type == ATOMIC_FORMULA );
			     if( formula->u.atomic->neg == 0 )
			       {
				 formula->next = $$->u.forall.whenEffect->u.when.addEffect;
				 $$->u.forall.whenEffect->u.when.addEffect = formula;
			       }
			     else
			       {
				 formula->next = $$->u.forall.whenEffect->u.when.delEffect;
				 $$->u.forall.whenEffect->u.when.delEffect = formula;
			       }
			   }
		       }
                  ;

list_atomic_effect
                  :  literal
                       {
			 $$ = (formula_t*)malloc( sizeof( formula_t ) );
			 assert( $$ != NULL );
			 $$->type = ATOMIC_FORMULA;
			 $$->u.atomic = $1;
			 $$->next = NULL;
		       }
                  |  LEFTPAR AND list_literal RIGHTPAR
                       {
			 $$ = $3;
		       }
                  ;

goal_formula      :  literal
                       {
			 $$ = (formula_t*)malloc( sizeof( formula_t ) );
			 assert( $$ != NULL );
			 $$->type = ATOMIC_FORMULA;
			 $$->u.atomic = $1;
			 $$->next = NULL;
		       }
                  |  LEFTPAR AND list_goal_formula RIGHTPAR
                       {
			 $$ = $3;
		       }
                  |  LEFTPAR FORALL LEFTPAR plain_or_typed_list_var RIGHTPAR formula RIGHTPAR
                       {
			 $$ = (formula_t*)malloc( sizeof( formula_t ) );
			 assert( $$ != NULL );
			 $$->type = FORALL_GOAL_FORMULA;
			 $$->next = NULL;
			 $$->u.forallGoal.vars = $4;
			 $$->u.forallGoal.formula = $6;
		       }
                  ;

list_goal_formula :  list_goal_formula goal_formula
                       {
			 $2->next = $1;
			 $$ = $2;
		       }
                  |  goal_formula
                  ;


/*********************************************************************************
 ***** PROBLEM DEFINITION
 **/


problem           :  LEFTPAR DEFINE LEFTPAR PROBLEM name RIGHTPAR 
                       {
			 _low_problemName = $5;
			 strcpy( _low_problemName, translate( _low_problemName ) );
		       }
                     list_def_prob
                     RIGHTPAR
                  ;

list_def_prob     :  list_def_prob def_prob
                  |  /* empty */
                  ; 

def_prob          :  LEFTPAR REQUIREMENTS require_def
                  |  LEFTPAR SITUATION situation_def
                  |  LEFTPAR INIT init_def RIGHTPAR
                  |  LEFTPAR GOAL goal_def
                  |  LEFTPAR LENGTH length_def
                  |  LEFTPAR TWODOTS DOMAIN name RIGHTPAR
                  |  LEFTPAR OBJECTS plain_or_typed_list_name RIGHTPAR
                       {
			 idList_t *object;
			 if( objects == NULL )
			   objects = $3;
			 else
			   {
			     for( object = objects; object->next != NULL; object = object->next );
			     object->next = $3;
			   }
		       }
                  ;

situation_def     :  name RIGHTPAR
                  ;

init_def          :  list_atomic_formula_name
                       {
			 formula_t *formula;
			 initialSituation = $1;
			 for( formula = initialSituation; formula != NULL; formula = formula->next )
			   if( formula->u.atomic->neg && !(_low_requirements & REQ_ADL) )
			     fprintf( stderr, "parser([init_def]): WARNING: negative literal in initial situation\n" );
		       }
                  ;

goal_def          :  goal_formula RIGHTPAR
                       {
			 formula_t *formula, *next, *tmp;

			 goalSituation = NULL;
			 for( formula = $1; formula != NULL; formula = next )
			   {
			     next = formula->next;
			     if( formula->type == FORALL_GOAL_FORMULA )
			       {
				 instantiated = NULL;
				 instantiateFormula( formula->u.forallGoal.formula, formula->u.forallGoal.vars, NULL );
				 if( instantiated != NULL )
				   {
				     for( tmp = instantiated; tmp->next != NULL; tmp = tmp->next );
				     tmp->next = goalSituation;
				     goalSituation = instantiated;
				   }
			       }
			     else
			       {
				 formula->next = goalSituation;
				 goalSituation = formula;
			       }
			   }

			 for( formula = goalSituation; formula != NULL; formula = formula->next )
			   if( formula->u.atomic->neg && !(_low_requirements & REQ_ADL))
			     fprintf( stderr, "parser([goal_def]): WARNING: negative literal in goal situation\n" );
		       }
                  ;

length_def        :  list_length_def_aux RIGHTPAR
                  ;

list_length_def_aux 
                  : list_length_def_aux length_def_aux
                  |  /* empty */
                  ;

length_def_aux    :  LEFTPAR SERIAL number RIGHTPAR
                  |  LEFTPAR PARALLEL number RIGHTPAR
                  ;

number            :  INTEGER
                  ;

%%


int
yyerror( char *s )
{
  extern int lineno;
  fprintf( stderr, "%s:%d: %s\n", _low_yyfile, lineno, s );
  return( 0 );
}


static void
checkRequirement( int req )
{
  switch( req )
    {
    case STRIPS:
      _low_requirements = _low_requirements | REQ_STRIPS;
      break;
    case EQUALITY:
      _low_requirements = _low_requirements | REQ_EQUALITY;
      break;
    case TYPING:
      _low_requirements = _low_requirements | REQ_TYPING;
      break;
    case ADL:
      _low_requirements = _low_requirements | REQ_ADL;
      break;
    default:
      fprintf( stderr, "parser(checkRequirement): not supported requirement %d\n", req );
    }
}


static char *
translate( char *name )
{
  char *ip, *jp;
  static char buffer[1024];

  for( ip = name, jp = buffer; *ip != '\0'; ++ip, ++jp )
    *jp = (*ip == 'a' ? '-' : *ip);
  *jp = '\0';
  return( buffer );
}


static void
printAtomicFormula( atomicFormula_t *formula )
{
  register idList_t *arg;

  if( formula != NULL )
    {
      /* negation? */
      if( formula->neg == 1 )
	fprintf( stderr, "(not " );

      /* print name */
      fprintf( stderr, "(%s ", (formula->equal == 1 ? "=" : formula->name) );

      /* args */
      for( arg = formula->args; arg != NULL; arg = arg->next )
	fprintf( stderr, "%s%s", arg->name, (arg->next != NULL ? " " : ")") );

      if( formula->neg == 1 )
	fprintf( stderr, ")" );
    }
}


static void
deleteFormula( formula_t *formula )
 {
   register idList_t *var, *pvar, *nvar;

   if( formula != NULL )
     {
       switch( formula->type )
	 {
	 case ATOMIC_FORMULA:
	   free( formula->u.atomic );
	   break;
	 case WHEN_FORMULA:
	   deleteFormula( formula->u.when.condition );
	   deleteFormula( formula->u.when.addEffect );
	   deleteFormula( formula->u.when.delEffect );
	   break;
	 case FORALL_FORMULA:
	   for( pvar = NULL, var = formula->u.forall.vars; var != NULL; var = nvar )
	     {
	       nvar = var->next;
	       free( var );
	     }
	   deleteFormula( formula->u.forall.addEffect );
	   deleteFormula( formula->u.forall.delEffect );
	   deleteFormula( formula->u.forall.whenEffect );
	   break;
	 }
       free( formula );
     }
}


static void
printFormula( formula_t *formula )
 {
   register idList_t *var;

   if( formula != NULL )
     {
       switch( formula->type )
	 {
	 case ATOMIC_FORMULA:
	   printAtomicFormula( formula->u.atomic );
	   break;
	 case WHEN_FORMULA:
	   fprintf( stderr, "(when (" );
	   printFormula( formula->u.when.condition );
	   fprintf( stderr, ") " );
	   printFormula( formula->u.when.addEffect );
	   printFormula( formula->u.when.delEffect );
	   fprintf( stderr, ")" );
	   break;
	 case FORALL_FORMULA:
	   fprintf( stderr, "(forall (" );
	   for( var = formula->u.forall.vars; var != NULL; var = var->next )
	     fprintf( stderr, "%s%s", var->name, (var->next != NULL ? " " : "") );
	   fprintf( stderr, ") " );
	   printFormula( formula->u.forall.addEffect );
	   fprintf( stderr, " " );
	   printFormula( formula->u.forall.delEffect );
	   fprintf( stderr, " " );
	   printFormula( formula->u.forall.whenEffect );
	   fprintf( stderr, ")" );
	   break;
	 }
       printFormula( formula->next );
     }
}


static void
generateNames( void )
{
  register int i;
  register idList_t *object;
  register formula_t *predicate;

  for( predicate = predicates, i = 0; predicate != NULL; predicate = predicate->next, ++i );
  _low_numberPredicates = i;

  _low_schemaName = (char**) calloc( numberSchema, sizeof( char* ) );
  _low_objectName = (char**) calloc( numberObjects, sizeof( char* ) );
  _low_predicateName = (char**) calloc( _low_numberPredicates, sizeof( char* ) );

  for( i = 0; i < numberSchema; ++i )
    _low_schemaName[i] = strdup( translate( _low_schemaTable[i]->name ) );

  for( object = objects, i = 0; object != NULL; object = object->next, ++i )
    _low_objectName[i] = strdup( translate( object->name ) );

  for( predicate = predicates; predicate != NULL; predicate = predicate->next )
    _low_predicateName[predicate->u.atomic->id-1] = strdup( translate( predicate->u.atomic->name ) );
}


static void
identifyObjects( void )
{
  idList_t *object;
  int i;

  i = 1;
  for( object = objects; object != NULL; object = object->next )
    object->id = i++;
  numberObjects = i - 1;
}


static void
tagFormulaList( formula_t *formulaList )
{
  formula_t *formula, *predicate;
  idList_t *var, *object;

  for( formula = formulaList; formula != NULL; formula = formula->next )
    if( formula->type == ATOMIC_FORMULA )
      {
	for( predicate = predicates; predicate != NULL; predicate = predicate->next )
	  if( !strcasecmp( predicate->u.atomic->name, formula->u.atomic->name ) )
	    {
	      formula->u.atomic->id = predicate->u.atomic->id;
	      break;
	    }
	if( predicate == NULL )
	  {
	    fprintf( stderr, "parser(tagFormulaList): no predicate for %s\n", 
		     translate( formula->u.atomic->name ) );
	    return;
	  }

	for( var = formula->u.atomic->args; var != NULL; var = var->next )
	  {
	    for( object = objects; object != NULL; object = object->next )
	      if( !strcasecmp( object->name, var->name ) )
		{
		  var->id = object->id;
		  break;
		}
	    if( object == NULL )
	      {
		fprintf( stderr, "parser(tagFormulaList): no object for %s\n", var->name );
		return;
	      }
	  }
      }
}


static void
idMatchArgsVars( formula_t *formula, idList_t *vars )
{
  formula_t *predicate;
  idList_t *arg, *var, *object;

  /* identification */
  for( ; formula != NULL; formula = formula->next )
    switch( formula->type )
      {
      case ATOMIC_FORMULA:
	for( predicate = predicates; predicate != NULL; predicate = predicate->next )
	  if( !strcasecmp( predicate->u.atomic->name, formula->u.atomic->name ) )
	    {
	      formula->u.atomic->id = predicate->u.atomic->id;
	      formula->u.atomic->equal = predicate->u.atomic->equal;
	      break;
	    }
	if( predicate == NULL )
	  fprintf( stderr, "parser(idMatchArgsVars): no predicate for %s\n", 
		   translate( formula->u.atomic->name ) );

	for( arg = formula->u.atomic->args; arg != NULL; arg = arg->next )
	  if( arg->name[0] == '?' )
	    {
	      for( var = vars; var != NULL; var = var->next )
		if( !strcasecmp( arg->name, var->name ) )
		  {
		    arg->id = var->id;
		    break;
		  }
	      if( var == NULL )
		fprintf( stderr, "parser(idMatchArgsVars): no variable for %s\n", arg->name );
	    }
	  else
	    {
	      for( object = objects; object != NULL; object = object->next )
		if( !strcasecmp( arg->name, object->name ) )
		  {
		    arg->id = object->id;
		    break;
		  }
	      if( object == NULL )
		fprintf( stderr, "parser(idMatchArgsVars): no object for %s\n", arg->name );
	    }
	break;
      case WHEN_FORMULA:
	idMatchArgsVars( formula->u.when.condition, vars );
	idMatchArgsVars( formula->u.when.addEffect, vars );
	idMatchArgsVars( formula->u.when.delEffect, vars );
	break;
      case FORALL_FORMULA:
	idMatchArgsVars( formula->u.forall.addEffect, vars );
	idMatchArgsVars( formula->u.forall.delEffect, vars );
	idMatchArgsVars( formula->u.forall.whenEffect, vars );
	break;
      }
}


static int
isOfType( register idList_t *type1, register idList_t *type2 )
{
  register idList_t *t1, *t2, *t3;

  if( !strcasecmp( type1->name, type2->name ) )
    {
      return( 1 );
    }
  else
    {
      for( t1 = type1; t1 != NULL; t1 = t1->next )
	for( t2 = types; t2 != NULL; t2 = t2->next )
	  if( !strcasecmp( t1->name, t2->name ) )
	    for( t3 = t2->type; t3 != NULL; t3 = t3->next )
	      {
		if( isOfType( t3, type2 ) )
		  return( 1 );
	      }
      return( 0 );
    }
}


static formula_t *
copyReplaceFormula( register formula_t *formula, register instantiation_t *instantiation )
{
  register formula_t *new;
  register instantiation_t *inst;
  register idList_t *arg, *prevArg, *newArg;

  if( formula != NULL )
    {
      new = (formula_t*)malloc( sizeof( formula_t ) );
      assert( new != NULL );
      new->type = formula->type;
      switch( formula->type )
	{
	case ATOMIC_FORMULA:
	  new->u.atomic = (atomicFormula_t*)malloc( sizeof( atomicFormula_t ) );
	  assert( new->u.atomic != NULL );
	  *(new->u.atomic) = *(formula->u.atomic);
	  new->u.atomic->args = NULL;

	  /* copy/replace arguments */
	  prevArg = NULL;
	  for( arg = formula->u.atomic->args; arg != NULL; arg = arg->next )
	    {
	      newArg = (idList_t*)malloc( sizeof( idList_t ) );
	      assert( newArg != NULL );
	      for( inst = instantiation; inst != NULL; inst = inst->next )
		if( !strcasecmp( arg->name, inst->var->name ) )
		  break;
	      *newArg = *(inst == NULL ? arg : inst->val);
	      newArg->next = NULL;

	      /* link it */
	      if( prevArg == NULL )
		new->u.atomic->args = newArg;
	      else
		prevArg->next = newArg;
	      prevArg = newArg;
	    }
	  break;
	case WHEN_FORMULA:
	  new->u.when.condition = copyReplaceFormula( formula->u.when.condition, instantiation );
	  new->u.when.addEffect = copyReplaceFormula( formula->u.when.addEffect, instantiation );
	  new->u.when.delEffect = copyReplaceFormula( formula->u.when.delEffect, instantiation );
	  break;
	}

      /* next */
      new->next = copyReplaceFormula( formula->next, instantiation );
      return( new );
    }
  else
    return( NULL );
}


static void
instantiateFormula( register formula_t *effects, register idList_t *vars, register instantiation_t *instantiation )
{
  instantiation_t new;
  formula_t *newFormula;

  if( vars == NULL )
    {
      /* copy/replace effects */
      while( effects != NULL )
	{
	  newFormula = copyReplaceFormula( effects, instantiation );
	  newFormula->next = instantiated;
	  instantiated = newFormula;
	  effects = effects->next;
	}
    }
  else
    {
      new.next = instantiation;
      new.var = vars;
      for( new.val = objects; new.val != NULL; new.val = new.val->next )
	if( isOfType( new.val->type, new.var->type ) )
	  instantiateFormula( effects, vars->next, &new );
    }
}


static void
schemaPostProcessing( register schema_t *schema )
{
  register formula_t *forallFormula, *formula, *nextFormula;

  for( forallFormula = schema->forall; forallFormula != NULL; forallFormula = forallFormula->next )
    {
      assert( forallFormula->type == FORALL_FORMULA );
      instantiated = NULL;
      instantiateFormula( forallFormula->u.forall.addEffect, forallFormula->u.forall.vars, NULL );
      instantiateFormula( forallFormula->u.forall.delEffect, forallFormula->u.forall.vars, NULL );
      instantiateFormula( forallFormula->u.forall.whenEffect, forallFormula->u.forall.vars, NULL );

      for( formula = instantiated; formula != NULL; formula = nextFormula )
	{
	  assert( formula->type != FORALL_FORMULA );
	  nextFormula = formula->next;
	  switch( formula->type )
	    {
	    case ATOMIC_FORMULA:
	      if( formula->u.atomic->neg == 1 )
		{
		  formula->next = schema->del;
		  schema->del = formula;
		}
	      else
		{
		  formula->next = schema->add;
		  schema->add = formula;
		}
	      break;
	    case WHEN_FORMULA:
	      formula->next = schema->when;
	      schema->when = formula;
	      break;
	    }
	}
    }
}


static int
atomId( register atomicFormula_t *formula, register int *parameters )
{
  register int *p;
  register idList_t *arg;
  static int arguments[MAXPARAMETERS];

  /* argument extraction */
  p = arguments;
  *p++ = formula->id;
  for( arg = formula->args; arg != NULL; arg = arg->next )
    if( arg->name[0] == '?' )
      {
	assert( arg->id > 0 );
	*p++ = parameters[arg->id];
      }
    else
      *p++ = arg->id;
  *p = -1;

  /* generete atomId */
  return( readAtomHash( arguments )->idx );
}


void
generateVarsAndValues( void )
{
  register int i, j, k, *p;
  register formula_t *formula;
  register idList_t *var, *object;
  static int arguments[MAXPARAMETERS];

  /* vars  */
  vars = (int**) calloc( numberSchema, sizeof( int* ) );
  for( i = 0; i < numberSchema; ++i )
    {
      vars[i] = (int*) calloc( maxParameters + 1, sizeof( int ) );
      for( var = _low_schemaTable[i]->vars, j = 0; var != NULL; var = var->next, ++j )
	vars[i][j] = var->id;
    }

  /* values  */
  values = (int**) calloc( numberSchema * (maxParameters + 1), sizeof( int* ) );
  for( i = 0; i < numberSchema * maxParameters; ++i )
    values[i] = (int *) calloc( numberObjects + 1, sizeof( int ) );
  for( i = 0; i < numberSchema; ++i )
    for( var = _low_schemaTable[i]->vars; var != NULL; var = var->next )
      {
	if( var->type == NULL )
	  {
	    k = 0;
	    for( object = objects; object != NULL; object = object->next )
	      values[numberSchema*(var->id-1) + i][k++] = object->id;
	  }
	else
	  {
	    if( !(_low_requirements & REQ_TYPING) )
	      fprintf( stderr, "parser(generateVarsAndValues): using types without :typing requirements.\n" );
	    k = 0;
	    for( object = objects; object != NULL; object = object->next )
	      if( !strcasecmp( var->type->name, object->type->name ) || isOfType( object->type, var->type ) )
		values[numberSchema*(var->id-1) + i][k++] = object->id;
	  }
      }

  /* insert initial and goal atoms in atomHashTable */
  idMatchArgsVars( initialSituation, NULL );
  for( formula = initialSituation, i = 0; formula != NULL; formula = formula->next )
    if( !formula->u.atomic->neg )
      {
	p = arguments;
	*p++ = formula->u.atomic->id;
	for( var = formula->u.atomic->args; var != NULL; var = var->next )
	  *p++ = var->id;
	*p = -1;
	_low_initialAtoms[i++] = readAtomHash( arguments )->idx;
      }
  _low_initialAtoms[i] = 0;

  idMatchArgsVars( goalSituation, NULL );
  for( formula = goalSituation, i = 0; formula != NULL; formula = formula->next )
    if( !formula->u.atomic->neg )
      {
	p = arguments;
	*p++ = formula->u.atomic->id;
	for( var = formula->u.atomic->args; var != NULL; var = var->next )
	  *p++ = var->id;
	*p = -1;
	_low_goalAtoms[i++] = readAtomHash( arguments )->idx;
      }
  _low_goalAtoms[i] = 0;
}


static void
applyEffect( register formula_t *addList, register formula_t *delList, register formula_t *whenList,
	     register atom_t *state, register atom_t *newState, register int *parameters )
{
  register int *add, *del, idx;
  register formula_t *when;

  /* initialize list pointer */
  add = operatorAdd;
  del = operatorDel;

  /* add-list */
  for( ; addList != NULL; addList = addList->next )
    {
      /* generate atom, apply and save add effect, and add parents */
      assert( (addList->type == ATOMIC_FORMULA) && (addList->u.atomic->neg == 0) );
      idx = atomId( addList->u.atomic, parameters );
      set( newState, idx );
      addParents( idx, operatorPrec );
      *add++ = idx;

      /* negated atoms */
      if( _low_requirements & REQ_ADL )
	{
	  if( _low_negatedAtom[idx] == 0 )
	    {
	      /* generate (not-p ...) atom */
	      addList->u.atomic->id += _low_numberPredicates + 1;
	      _low_negatedAtom[idx] = atomId( addList->u.atomic, parameters );
	      addList->u.atomic->id -= _low_numberPredicates + 1;
	      assert( _low_negatedAtom[idx] != idx );
	    }
	  *del++ = _low_negatedAtom[idx];
	}
    }

  /* del-list */
  for( ; delList != NULL; delList = delList->next )
    {
      /* basic assertion */
      assert( (delList->type == ATOMIC_FORMULA) && (delList->u.atomic->neg == 1) );
      idx = atomId( delList->u.atomic, parameters );
      *del++ = idx;

      /* negated atoms */
      if( _low_requirements & REQ_ADL )
	{
	  if( _low_negatedAtom[idx] == 0 )
	    {
	      /* generate (not-p ...) atom */
	      delList->u.atomic->id += _low_numberPredicates + 1;
	      _low_negatedAtom[idx] = atomId( delList->u.atomic, parameters );
	      delList->u.atomic->id -= _low_numberPredicates + 1;
	      assert( _low_negatedAtom[idx] != idx );
	    }
	  set( newState, _low_negatedAtom[idx] );
	  addParents( _low_negatedAtom[idx], operatorPrec );
	  *add++ = _low_negatedAtom[idx];
	}
    }

  /* conditional effects */
  for( when = whenList; when != NULL; when = when->next )
    if( evaluateFormula( when->u.when.condition, state, parameters, 0 ) )
      {
	/* add-list */
	for( addList = when->u.when.addEffect; addList != NULL; addList = addList->next )
	  {
	    /* basic assertion */
	    assert( (addList->type == ATOMIC_FORMULA) && (addList->u.atomic->neg == 0) );
	    idx = atomId( addList->u.atomic, parameters );
	    set( newState, idx );
	    addParents( idx, operatorPrec );

	    /* no negated atoms for conditional add-list */
	  }

	/* del-list */
	for( delList = when->u.when.delEffect; delList != NULL; delList = delList->next )
	  {
	    /* basic assertion */
	    assert( (delList->type == ATOMIC_FORMULA) && (delList->u.atomic->neg == 1) );
	    idx = atomId( delList->u.atomic, parameters );

	    /* negated atoms */
	    if( _low_requirements & REQ_ADL )
	      {
		if( _low_negatedAtom[idx] == 0 )
		  {
		    /* generate (not-p ...) atom */
		    delList->u.atomic->id += _low_numberPredicates + 1;
		    _low_negatedAtom[idx] = atomId( delList->u.atomic, parameters );
		    delList->u.atomic->id -= _low_numberPredicates + 1;
		    assert( _low_negatedAtom[idx] != idx );
		  }
		set( newState, _low_negatedAtom[idx] );
		addParents( _low_negatedAtom[idx], operatorPrec );
	      }
	  }
      }

  /* finish lists */
  *add = 0;
  *del = 0;
  operatorAddSize = add - operatorAdd;
  operatorDelSize = del - operatorDel;
}


int
evaluateFormula( register formula_t *formula, register atom_t *state, 
		 register int *parameters, register int savePrec )
{
  register int *prec, *p, idx;
  register idList_t *arg;
  static int arguments[MAXPARAMETERS];

  /* save precondition */
  if( savePrec )
    prec = operatorPrec;

  for( ; formula != NULL; formula = formula->next )
    {
      /* basic assertion */
      assert( formula->type == ATOMIC_FORMULA );

      /* argument extraction */
      p = arguments;
      *p++ = formula->u.atomic->id;
      for( arg = formula->u.atomic->args; arg != NULL; arg = arg->next )
	if( arg->name[0] == '?' )
	  {
	    assert( arg->id > 0 );
	    if( (*p++ = parameters[arg->id]) == 0 )
	      break;
	  }
	else
	  *p++ = arg->id;
      if( arg != NULL )
	continue;
      else
	*p = -1;

      /* check formula */
      if( formula->u.atomic->equal == 1 )
	{
	  if( (formula->u.atomic->neg && (arguments[1] == arguments[2])) ||
	      (!formula->u.atomic->neg && (arguments[1] != arguments[2])) )
	    return( 0 );
	}
      else if( formula->u.atomic->neg == 0 )
	{
	  idx = readAtomHash( arguments )->idx;
	  if( !asserted( state, idx ) )
	    return( 0 );

	  /* save precondition */
	  if( savePrec )
	    *prec++ = idx;
	}
      else if( _low_requirements & REQ_ADL )
	{
	  /* generate negated atom */
	  idx = readAtomHash( arguments )->idx;
	  if( _low_negatedAtom[idx] == 0 )
	    {
	      arguments[0] += _low_numberPredicates + 1;
	      _low_negatedAtom[idx] = readAtomHash( arguments )->idx;
	      arguments[0] -= _low_numberPredicates + 1;
	      assert( _low_negatedAtom[idx] != idx );
	    }

	  /* test it if in the grounding phase */
	  if( (_low_groundingOperators == 1) && !asserted( state, _low_negatedAtom[idx] ) )
	    return( 0 );

	  /* save precondition */
	  if( savePrec )
	    *prec++ = _low_negatedAtom[idx];
	}
      else
	{
	  fprintf( stderr, "parser(evaluateFormula): WARNING: negative literal in precondition\n" );
	  return( 0 );
	}
    }

  /* save precondition */
  if( savePrec )
    {
      *prec = 0;
      operatorPrecSize = prec - operatorPrec;
    }

  return( 1 );
}


int
applyOperatorSchema( register schema_t *schema, register atom_t *state, 
		     register atom_t *newState, register int *parameters )
{
  if( !evaluateFormula( schema->prec, state, parameters, 1 ) )
    {
      return( 0 );
    }
  else
    {
      applyEffect( schema->add, schema->del, schema->when, state, newState, parameters );
      return( 1 );
    }
}


void
instantiateConditionalEffects( register int father, register schema_t *schema, register int *parameters )
{
  register int *prec, *add, *del, idx;
  register formula_t *when, *formula, *prevWhen, *nextWhen, *prevFormula, *nextFormula;
  register suboperator_t *suboperator;
  extern void orderAtomList( register int *, register int );

  /* when-list */
  prevWhen = NULL;
  for( when = schema->when; when != NULL; when = nextWhen )
    {
      assert( when->type == WHEN_FORMULA );
      nextWhen = when->next;

      /* prec-list */
      prec = operatorPrec;
      prevFormula = NULL;
      for( formula = when->u.when.condition; formula != NULL; formula = nextFormula )
	{
	  assert( formula->type == ATOMIC_FORMULA );
	  nextFormula = formula->next;

	  /* check if equality */
	  if( formula->u.atomic->equal == 1 )
	    {
	      if( (formula->u.atomic->neg && (parameters[1] == parameters[2])) ||
		  (!formula->u.atomic->neg && (parameters[1] != parameters[2])) )
		break;

	      /* delete it and relink */
	      deleteFormula( formula );
	      if( prevFormula == NULL )
		when->u.when.condition = nextFormula;
	      else
		prevFormula->next = nextFormula;
	    }
	  else
	    {
	      prevFormula = formula;

	      /* others formula */
	      idx = atomId( formula->u.atomic, parameters );
	      if( formula->u.atomic->neg == 0 )
		{
		  /* save precondition */
		  *prec++ = idx;
		}
	      else if( _low_requirements & REQ_ADL )
		{
		  /* generate (not-p ...) atom */
		  if( _low_negatedAtom[idx] == 0 )
		    {
		      formula->u.atomic->id += _low_numberPredicates + 1;
		      _low_negatedAtom[idx] = atomId( formula->u.atomic, parameters );
		      formula->u.atomic->id -= _low_numberPredicates + 1;
		      assert( _low_negatedAtom[idx] != idx );
		    }
		  *prec++ = _low_negatedAtom[idx];
		}
	      else
		{
		  fprintf( stderr, "parser(evaluateFormula): WARNING: negative literal in precondition\n" );
		}
	    }
	}

      /* check if we pass the equality conditions of the prec */
      if( formula != NULL )
	{
	  /* delete it and relink */
	  deleteFormula( when );
	  if( prevWhen == NULL )
	    schema->when = nextWhen;
	  else
	    prevWhen->next = nextWhen;
	}
      else
	{
	  *prec = 0;
	  operatorPrecSize = prec - operatorPrec;
	  prevWhen = when;

	  /* initialize lists */
	  add = operatorAdd;
	  del = operatorDel;

	  /* add-list */
	  for( formula = when->u.when.addEffect; formula != NULL; formula = formula->next )
	    {
	      /* generate atom, apply and save add effect, and add parents */
	      assert( (formula->type == ATOMIC_FORMULA) && (formula->u.atomic->neg == 0) );
	      idx = atomId( formula->u.atomic, parameters );
	      *add++ = idx;

	      /* negated atoms */
	      if( _low_requirements & REQ_ADL )
		{
		  if( _low_negatedAtom[idx] == 0 )
		    {
		      /* generate (not-p ...) atom */
		      formula->u.atomic->id += _low_numberPredicates + 1;
		      _low_negatedAtom[idx] = atomId( formula->u.atomic, parameters );
		      formula->u.atomic->id -= _low_numberPredicates + 1;
		      assert( _low_negatedAtom[idx] != idx );
		    }
		  *del++ = _low_negatedAtom[idx];
		}
	    }

	  /* del-list */
	  for( formula = when->u.when.delEffect; formula != NULL; formula = formula->next )
	    {
	      /* basic assertion */
	      assert( (formula->type == ATOMIC_FORMULA) && (formula->u.atomic->neg == 1) );
	      idx = atomId( formula->u.atomic, parameters );
	      *del++ = idx;

	      /* negated atoms */
	      if( _low_requirements & REQ_ADL )
		{
		  if( _low_negatedAtom[idx] == 0 )
		    {
		      /* generate (not-p ...) atom */
		      formula->u.atomic->id += _low_numberPredicates + 1;
		      _low_negatedAtom[idx] = atomId( formula->u.atomic, parameters );
		      formula->u.atomic->id -= _low_numberPredicates + 1;
		      assert( _low_negatedAtom[idx] != idx );
		    }
		  *add++ = _low_negatedAtom[idx];
		}
	    }

	  /* finish lists */
	  *add = 0;
	  *del = 0;
	  operatorAddSize = add - operatorAdd;
	  operatorDelSize = del - operatorDel;

	  /* save suboperator */
	  suboperator = (suboperator_t*)malloc( sizeof( suboperator_t ) );
	  assert( suboperator != NULL );
	  suboperator->prec = (int*)calloc( operatorPrecSize+1, sizeof( int ) );
	  suboperator->add = (int*)calloc( operatorAddSize+1, sizeof( int ) );
	  suboperator->del = (int*)calloc( operatorDelSize+1, sizeof( int ) );
	  assert( (suboperator->prec != NULL) && (suboperator->add != NULL) && (suboperator->del != NULL) );

	  /* order Prec, Add, and Del lists */
	  orderAtomList( operatorPrec, operatorPrecSize );
	  orderAtomList( operatorAdd, operatorAddSize );
	  orderAtomList( operatorDel, operatorDelSize );

	  /* fill it */
	  suboperator->precSize = operatorPrecSize;
	  suboperator->addSize = operatorAddSize;
	  suboperator->delSize = operatorDelSize;
	  memcpy( suboperator->prec, operatorPrec, (operatorPrecSize+1) * sizeof( int ) );
	  memcpy( suboperator->add, operatorAdd, (operatorAddSize+1) * sizeof( int ) );
	  memcpy( suboperator->del, operatorDel, (operatorDelSize+1) * sizeof( int ) );

	  /* link it */
	  suboperator->next = _low_suboperators;
	  _low_suboperators = suboperator;
	}
    }
}
