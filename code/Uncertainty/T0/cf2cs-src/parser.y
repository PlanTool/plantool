%{
/*
 *  Copyright (C) 2006 Universitat Pompeu Fabra
 *
 *  Permission is hereby granted to distribute this software for
 *  non-commercial research purposes, provided that this copyright
 *  notice is included with any such distribution.
 *
 *  THIS SOFTWARE IS PROVIDED "AS IS" WITHOUT WARRANTY OF ANY KIND,
 *  EITHER EXPRESSED OR IMPLIED, INCLUDING, BUT NOT LIMITED TO, THE
 *  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 *  PURPOSE.  THE ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE OF THE
 *  SOFTWARE IS WITH YOU.  SHOULD THE PROGRAM PROVE DEFECTIVE, YOU
 *  ASSUME THE COST OF ALL NECESSARY SERVICING, REPAIR OR CORRECTION.
 *
 *  Blai Bonet, bonet@ldc.usb.ve
 *  Hector Palacios, hector.palacios@upf.edu, hectorpal@gmail.com
 *
 */

#include <stdio.h>
#include <assert.h>
#include <iostream>

#include <map>
#include <nnf.h>

#include "parser.h"
#include "planner.h"

#ifdef USE_CLAUSE
#include "clauses.hpp"
#endif

  // Sometimes it use a lot the stack...
#define YYINITDEPTH 5000

extern int yylex(void);
using namespace std;


/*******************************************************************************
 **
 **   Global Data
 **
 ******************************************************************************/

int                _low_numberSchema = 0;
char*              _low_yyfile = NULL;
char*              _low_problemName = NULL;
char*              _low_domainName = NULL;
char**             _low_schemaName = NULL;
char**             _low_objectName = NULL;
char**             _low_predicateName = NULL;
int                _low_numberPredicates = 0;
int                _low_requirements = 0;
schema_t*          _low_schemaTable[MAXSCHEMA];
int**              _low_initialAtoms;

// operators
operator_t         _low_operator;
int                _low_groundingOperators;

// variables and domain instantiation
int**              _low_vars = NULL;
int**              _low_values = NULL;

// initial/goal formulas
formula_t*         structural = NULL;
formula_t*         initialSituation = NULL;
formula_t*         goalSituation = NULL;

// tables
typedef map<const char*,formula_t*,ltstr> str2wff_t;
str2wff_t predicates;
str2wff_t defined;
map<const char*,idList_t*,ltstr>   objects;
map<const char*,idList_t*,ltstr>   types;


/*******************************************************************************
 **
 **   Static Global Data
 **
 ******************************************************************************/

static int         domainParsed = 0;
static int         maxParameters = 0;
static int         numberObjects = 0;
static schema_t*   schemas = NULL;


/*******************************************************************************
 **
 **   Forward References
 **
 ******************************************************************************/

static void       checkRequirement( int );
static formula_t* unfoldQuantifiers( formula_t* );
static formula_t* linearizeEffect( formula_t*, formula_t*& );
       void       printFormula( FILE*, formula_t* );
       void       printFormulaPDDL( ostream& o, formula_t *formula );
       void       printInitialPDDL( ostream& out, formula_t *formula );
static nnf::node  generateEffect( nnf::Manager*, nnf::node, formula_t*, int*, lit_effect_t& );
//static void       collectEffectAtoms( formula_t*, int*, std::vector<int>& );
static void       collectEffectAtoms( formula_t*, int*, std::vector<int>&, std::vector<int> *atoms_conds = 0 );
static nnf::node  generateAtomicFormula( nnf::Manager*, formula_t *, int*, bool );
       nnf::node  generateFormula( nnf::Manager*, formula_t*, int*, bool );
static void       collectPlainFormulaAtoms( formula_t* formula, int* parameters, 
					    std::vector<int> &atoms );
static void       collectFormulaAtomsPos( formula_t*, int*, std::vector<int>& );
static void       collectFormulaAtoms( formula_t*, int*, std::vector<int>& );
static int        atomId( formula_t *literal, int* parameters );

 static void idMatchArgsVars( formula_t *formula, idList_t *vars );
 static void generateNames( void );
 static instantiation_t * buildInstantiation( idList_t *vars, idList_t *args );
 static formula_t * copyReplaceFormula( formula_t *formula, instantiation_t *inst );
 void yyerror( char const *s );


extern int        lineno;
extern int        numberAtoms;
extern iatom_t*   readAtomHash( int* );
extern std::map<int,int> inv_reachable;
extern std::map<int,int> reachable;
extern unsigned   time_shift;
extern bool observation_detected;
%}

%token   LEFTPAR RIGHTPAR NAME DEF_NAME TWODOTS HYPHEN DEFINE
%token   DOMAIN REQUIREMENTS CONSTANTS PREDICATES QUESTION 
%token   EITHER NOT AND OR FORALL WHEN EQUAL ACTION OBSERVE EQUALITY
%token   CONDITIONAL_EFFECTS NEGATIVE_PRECONDITIONS
%token   PARAMETERS PRECONDITION EFFECT 
%token   PROBLEM INIT GOAL STRUCTURAL SITUATION OBJECTS
%token   SERIAL PARALLEL INTEGER FLOAT SET
%token   TYPING STRIPS TYPES CERTAINTY KNOW UNKNOWN
%token   DEFINED FORMULA ONEOF

%type    <integer>          INTEGER
%type    <real>             FLOAT
%type    <ident>            NAME DEF_NAME name predicate variable term
%type    <integer>          require_key action_type
%type    <idlist>           type list_type typed_list_type
%type    <idlist>           list_name typed_list_name
%type    <idlist>           list_var typed_list_var plain_or_typed_list_var
%type    <idlist>           list_term list_parameters
%type    <body>             schema_body list_schema_body
%type    <body>             defined_body list_defined_body
%type    <formula>          atomic_formula formula list_formula
%type    <formula>          non_det_effect_list non_det_effect
%type    <formula>          one_effect_formula list_one_effect_formula
%type    <formula>          literal goal_formula list_literal list_atomic_effect

%start   start

%%

start             :  list_domain_problem
                  ;

list_domain_problem
                  :  list_domain_problem problem
                  |  list_domain_problem domain
                  |  /* empty */
                  ;


/*******************************************************************************
 **
 **   Domain Definition
 **
 ******************************************************************************/

domain            :  LEFTPAR DEFINE LEFTPAR DOMAIN name RIGHTPAR
                       {
			 _low_domainName = $5;
		       }
                     LEFTPAR list_def
                       {
			 idMatchArgsVars( initialSituation, NULL );
			 idMatchArgsVars( goalSituation, NULL );
		       }
                     list_structure_def
                       {
			 /* how much parameters can an schema have? */
			 for( schema_t *sch = schemas; sch != NULL; sch = sch->next )
			   {
			     int i;
			     idList_t *var;
			     for( var = sch->vars, i = 0; var != NULL; var = var->next, ++i );
			     maxParameters = MAX(maxParameters,i);
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

require_key       :  TYPING
                       {
			 $$ = TYPING;
		       }
                  | CONDITIONAL_EFFECTS 
                       {
			 $$ = CONDITIONAL_EFFECTS;
		       }
                  | NEGATIVE_PRECONDITIONS
                       {
			 $$ = NEGATIVE_PRECONDITIONS;
		       }
                  |  EQUALITY
                       {
			 $$ = EQUALITY;

			 /* create the equal predicate */
			 formula_t *pred = (formula_t*)malloc( sizeof( formula_t ) );
			 pred->type = ATOMIC_FORMULA;
			 pred->u.atomic.id = ++_low_numberPredicates;
			 pred->u.atomic.neg = 0;
			 pred->u.atomic.equal = 1;
			 pred->u.atomic.name = strdup( "=" );
			 predicates[pred->u.atomic.name] = pred;
		       }
                  |  STRIPS
                       {
			 $$ = STRIPS;
		       }
                  ;


types_def         :  plain_or_typed_list_type RIGHTPAR
                  ;

constants_def     :  plain_or_typed_list_name RIGHTPAR
                  ;

type              :  name
                       {
			 $$ = (idList_t*)malloc( sizeof( idList_t ) );
			 $$->id = 0;
			 $$->name = $1;
			 $$->type = NULL;
			 $$->next = NULL;
		       }
                  |  LEFTPAR EITHER list_type RIGHTPAR
                       {
			 $$ = $3;
		       }
                  ;

plain_or_typed_list_type
                  :  typed_list_type
                  |  list_type
                  ;

plain_or_typed_list_name
                  :  typed_list_name
                  |  list_name
                       {
			 for( idList_t *id = $1; id != NULL; id = id->next )
			   objects[id->name] = id;
		       }
                  ;

plain_or_typed_list_var
                  :  typed_list_var
                  |  list_var
                  ;

typed_list_type   :  typed_list_type list_type HYPHEN type  
                       {
			 for( idList_t *id = $2; id != NULL; id = id->next )
			   {
			     id->type = $4;
			     types[id->name] = id;
			   }
		       }
                  |  list_type HYPHEN type
                       {
			 for( idList_t *id = $1; id != NULL; id = id->next )
			   {
			     id->type = $3;
			     types[id->name] = id;
			   }
		       }
                  ;

typed_list_name   :  typed_list_name list_name HYPHEN type 
                       {
			 for( idList_t *id = $2; id != NULL; id = id->next )
			   {
			     id->type = $4;
			     objects[id->name] = id;
			     //xxxx printf( "object %s has id %d\n", id->name, id->id );
			   }
		       }
                  |  list_name HYPHEN type
                       {
			 for( idList_t *id = $1; id != NULL; id = id->next )
			   {
			     id->type = $3;
			     objects[id->name] = id;
			     //xxxx printf( "object %s has id %d\n", id->name, id->id );
			   }
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
			 for( idList_t *name = $1; name != NULL; name = name->next )
			   name->type = $3;
			 $$ = $1;
		       }
                  ;

list_type         :  list_type name 
                       {
			 $$ = (idList_t*)malloc( sizeof( idList_t ) );
			 $$->id = 0;
			 $$->name = $2;
			 $$->type = NULL;
			 $$->next = $1;
		       }
                  |  name
                       {
			 $$ = (idList_t*)malloc( sizeof( idList_t ) );
			 $$->id = 0;
			 $$->name = $1;
			 $$->type = NULL;
			 $$->next = NULL;
		       }
                  ;

list_name         :  list_name name 
                       {
			 $$ = (idList_t*)malloc( sizeof( idList_t ) );
			 $$->id = ++numberObjects;
			 $$->name = $2;
			 $$->type = NULL;
			 $$->next = $1;
			 //objects[$$->name] = $$;
		       }
                  |  name
                       {
			 $$ = (idList_t*)malloc( sizeof( idList_t ) );
			 $$->id = ++numberObjects;
			 $$->name = $1;
			 $$->type = NULL;
			 $$->next = NULL;
			 //objects[$$->name] = $$;
		       }
                  ;

list_var          :  variable list_var
                       {
			 $$ = (idList_t*)malloc( sizeof( idList_t ) );
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
			 $$ = (char*)malloc( strlen( $2 ) + 2 );
			 $$[0] = '\0';
			 strcat( $$, "?" );
			 strcat( $$, $2 );
			 free( $2 );
		       }
                  ;

predicates_def    :  list_atomic_formula_skeleton RIGHTPAR
                  ;

list_atomic_formula_skeleton 
                  :  list_atomic_formula_skeleton atomic_formula_skeleton
                  |  atomic_formula_skeleton
                  ;

atomic_formula_skeleton 
                  :  LEFTPAR predicate plain_or_typed_list_var RIGHTPAR
                       {
			 formula_t *pred = (formula_t*)malloc( sizeof( formula_t ) );
			 pred->type = ATOMIC_FORMULA;
			 pred->u.atomic.id = ++_low_numberPredicates;
			 pred->u.atomic.neg = 0;
			 pred->u.atomic.name = $2;
			 pred->u.atomic.equal = 0;
			 pred->u.atomic.args = $3;
			 predicates[pred->u.atomic.name] = pred;
			 assert( pred->u.atomic.name != 0 );
			 // AQUI!!!
			 for( str2wff_t::iterator it = predicates.begin(); 
			      it != predicates.end(); 
			      it++ )
			   assert( it->first != 0 );
			 cout << "predicates size = " << predicates.size() 
			      << " for name = " << predicates[pred->u.atomic.name]->u.atomic.name << endl;
		       }
                  ;

predicate         :  NAME
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
                  |  defined_def
                  ;

action_type       :  ACTION
                       {
			 $$ = ACTION;
		       }
                  ;


schema_def        :  action_type name list_schema_body
                       {
			 formula_t *f1, *f2, *f3;

			 schema_t *schema = (schema_t*)malloc( sizeof( schema_t ) );
			 schema->name = $2;
			 schema->vars = NULL;
			 schema->prec = NULL;
			 schema->effect = NULL;
			 schema->next = NULL;

			 bool saw_effect = false, saw_observe = false;
			 for( body_t *body = $3; body != NULL; body = body->next )
			   switch( body->type )
			     {
			     case PARAMETERS:
			       schema->vars = body->u.parameters;
			       break;
			     case PRECONDITION:
			       schema->prec = body->u.formula;
			       break;
			     case EFFECT:
			       saw_effect= true;
			       schema->is_observation = false;
			       for( f1 = body->u.formula; f1 != NULL; f1 = f1->u.flist.next )
				 {
				   f2 = (formula_t*)malloc( sizeof( formula_t ) );
				   f2->type = LIST_FORMULA;
				   f2->u.flist.formula = unfoldQuantifiers( f1->u.flist.formula );
				   f2->u.flist.formula = linearizeEffect( f2->u.flist.formula, f3 );
				   f2->u.flist.next = schema->effect;
				   schema->effect = f2;
				 }
			       break;
			     case OBSERVE:
			       saw_observe = true;
			       schema->is_observation = true;
			       observation_detected = true;
			       for( f1 = body->u.formula; f1 != NULL; f1 = f1->u.flist.next )
				 {
				   f2 = (formula_t*)malloc( sizeof( formula_t ) );
				   f2->type = LIST_FORMULA;
				   f2->u.flist.formula = unfoldQuantifiers( f1->u.flist.formula );
				   f2->u.flist.formula = linearizeEffect( f2->u.flist.formula, f3 );
				   f2->u.flist.next = schema->effect;
				   schema->effect = f2;
				 }
			       break;
			     }

			 assert( !saw_effect || !saw_observe );

			 /* if no effect, insert the empty effect */
			 if( !schema->effect )
			   {
			     f2 = (formula_t*)malloc( sizeof( formula_t ) );
			     f2->type = EMPTY_FORMULA;
			     schema->effect = (formula_t*)malloc( sizeof( formula_t ) );
			     schema->effect->type = LIST_FORMULA;
			     schema->effect->u.flist.formula = f2;
			     schema->effect->u.flist.next = NULL;
			   }

			 /* check and identify predicates and vars */
			 idMatchArgsVars( schema->prec, schema->vars );
			 idMatchArgsVars( schema->effect, schema->vars );
			 /* printSchema( stdout, schema ); printf( "\n" ); */

			 /* link schemas */
			 schema->next = schemas;
			 schemas = schema;

			 /* insert schema into schemaTable */
			 _low_schemaTable[_low_numberSchema++] = schema;
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
			 $$ = (body_t*)malloc( sizeof( body_t ) );
			 $$->type = PARAMETERS;
			 $$->u.parameters = $3;
		       }
                  |  PRECONDITION LEFTPAR AND RIGHTPAR
                       {
			 $$ = (body_t*)malloc( sizeof( body_t ) );
			 $$->type = AND;
		       }
                  |  PRECONDITION formula
                       {
			 $$ = (body_t*)malloc( sizeof( body_t ) );
			 $$->type = PRECONDITION;
			 $$->u.formula = $2;
		       }
                  |  EFFECT non_det_effect_list
                       {
			 $$ = (body_t*)malloc( sizeof( body_t ) );
			 $$->type = EFFECT;
			 $$->u.formula = $2;
		       }
                  |  OBSERVE non_det_effect_list
                       {
			 $$ = (body_t*)malloc( sizeof( body_t ) );
			 $$->type = OBSERVE;
			 $$->u.formula = $2;
		       }
                  ;

list_parameters   :  plain_or_typed_list_var
                       {
			 int i;
			 idList_t *var;
			 for( i = 1, var = $1; var != NULL; var = var->next )
			   var->id = i++;
			 $$ = $1;
		       }
                  ;

defined_def       :  DEFINED name list_defined_body
                       {
			 formula_t *def = (formula_t*)malloc( sizeof( formula_t ) );
			 def->type = DEFINED_FORMULA;
			 for( body_t *body = $3; body != NULL; body = body->next )
			   switch( body->type )
			     {
			     case PARAMETERS:
			       def->u.lambda.vars = body->u.parameters;
			       break;
			     case FORMULA:
			       def->u.lambda.formula = body->u.formula;
			       break;
			     }

			 /* check and identify predicates and vars */
			 idMatchArgsVars( def->u.lambda.formula, def->u.lambda.vars );

			 /* insert into defined table */
			 defined[$2] = def;
			 /*
			   printf( "defined %s = ", $2 );
			   printFormula( stdout, def->u.lambda.formula );
			   printf( "\n" );
			 */
		       }
                  ;

list_defined_body :  list_defined_body defined_body
                       {
			 $2->next = $1;
			 $$ = $2;
		       }
                  |  /* empty */
                       {
			 $$ = NULL;
		       }
                  ;

defined_body      :  PARAMETERS LEFTPAR list_parameters RIGHTPAR
                       {
			 $$ = (body_t*)malloc( sizeof( body_t ) );
			 $$->type = PARAMETERS;
			 $$->u.parameters = $3;
		       }
                  |  FORMULA formula
                       {
			 $$ = (body_t*)malloc( sizeof( body_t ) );
			 $$->type = FORMULA;
			 $$->u.formula = $2;
		       }
                  ;

literal           :  atomic_formula
                  |  LEFTPAR NOT atomic_formula RIGHTPAR
                       {
			 $3->u.atomic.neg = 1;
			 $$ = $3;
		       }
                  ;

formula           :  atomic_formula
                  |  LEFTPAR NOT formula RIGHTPAR
                       {
			 if( $3->type == ATOMIC_FORMULA )
			   {
			     $3->u.atomic.neg = ($3->u.atomic.neg+1)%2;
			     $$ = $3;
			   }
			 else
			   {
			     $$ = (formula_t*)malloc( sizeof( formula_t ) );
			     $$->type = NOT_FORMULA;
			     $$->u.formula = $3;
			   }
		       }
                  |  LEFTPAR UNKNOWN formula RIGHTPAR
                       {
			 $$ = (formula_t*)malloc( sizeof( formula_t ) );
			 $$->type = UNKNOWN_FORMULA;
			 $$->u.formula = $3;
		       }
                  |  LEFTPAR AND list_formula RIGHTPAR
                       {
			 $$ = (formula_t*)malloc( sizeof( formula_t ) );
			 $$->type = AND_FORMULA;
			 $$->u.formula = $3;
		       }
                  |  LEFTPAR OR list_formula RIGHTPAR
                       {
			 $$ = (formula_t*)malloc( sizeof( formula_t ) );
			 $$->type = OR_FORMULA;
			 $$->u.formula = $3;
		       }
                  |  LEFTPAR ONEOF list_formula RIGHTPAR
                       {
			 $$ = (formula_t*)malloc( sizeof( formula_t ) );
			 $$->type = ONEOF_FORMULA;
			 $$->u.formula = $3;
		       }
                  |  LEFTPAR FORALL LEFTPAR plain_or_typed_list_var RIGHTPAR formula RIGHTPAR
                       {
			 $$ = (formula_t*)malloc( sizeof( formula_t ) );
			 $$->type = FORALL_FORMULA;
			 $$->u.lambda.vars = $4;
			 $$->u.lambda.formula = $6;
		       }
                  ;

list_formula      :  list_formula formula
                       {
			 $$ = (formula_t*)malloc( sizeof( formula_t ) );
			 $$->type = LIST_FORMULA;
			 $$->u.flist.formula = $2;
			 $$->u.flist.next = $1;
		       }
                  |  formula
                       {
			 $$ = (formula_t*)malloc( sizeof( formula_t ) );
			 $$->type = LIST_FORMULA;
			 $$->u.flist.formula = $1;
			 $$->u.flist.next = NULL;
		       }
                  ;

atomic_formula    :  LEFTPAR predicate list_term RIGHTPAR 
                       {
			 $$ = (formula_t*)malloc( sizeof( formula_t ) );
			 $$->type = ATOMIC_FORMULA;
			 $$->u.atomic.id = -1;
			 $$->u.atomic.neg = 0;
			 $$->u.atomic.equal = 0;
			 $$->u.atomic.name = $2;
			 $$->u.atomic.args = $3;
		       }
                  |  LEFTPAR DEF_NAME list_term RIGHTPAR 
                       {
			 instantiation_t *inst;
			 str2wff_t::iterator it;
			 it = defined.find( $2 );
			 inst = buildInstantiation( (*it).second->u.lambda.vars, $3 );
			 $$ = copyReplaceFormula( (*it).second->u.lambda.formula, inst );
		       }
                  ;

list_term         :  term list_term
                       {
			 $$ = (idList_t*)malloc( sizeof( idList_t ) );
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

non_det_effect_list
                  :  non_det_effect_list non_det_effect
                       {
			 $$ = (formula_t*)malloc( sizeof( formula_t ) );
			 $$->type = LIST_FORMULA;
			 $$->u.flist.formula = $2;
			 $$->u.flist.next = $1;
		       }
                  |  non_det_effect
                       {
			 $$ = (formula_t*)malloc( sizeof( formula_t ) );
			 $$->type = LIST_FORMULA;
			 $$->u.flist.formula = $1;
			 $$->u.flist.next = NULL;
		       }
                  |  one_effect_formula
                       {
			 $$ = (formula_t*)malloc( sizeof( formula_t ) );
			 $$->type = LIST_FORMULA;
			 $$->u.flist.formula = $1;
			 $$->u.flist.next = NULL;
		       }
                  ;

non_det_effect
                  :  LEFTPAR one_effect_formula RIGHTPAR
                       {
			 $$ = $2;
		       }
                  |  LEFTPAR RIGHTPAR
                       {
			 $$ = (formula_t*)malloc( sizeof( formula_t ) );
			 $$->type = EMPTY_FORMULA;
		       }
                  ;

list_one_effect_formula
                  :  list_one_effect_formula one_effect_formula
                       {
			 $$ = (formula_t*)malloc( sizeof( formula_t ) );
			 $$->type = LIST_FORMULA;
			 $$->u.flist.formula = $2;
			 $$->u.flist.next = $1;
		       }
                  |  one_effect_formula
                       {
			 $$ = (formula_t*)malloc( sizeof( formula_t ) );
			 $$->type = LIST_FORMULA;
			 $$->u.flist.formula = $1;
			 $$->u.flist.next = NULL;
		       }
                  ;

one_effect_formula
                  :  literal
                  |  LEFTPAR SET literal formula RIGHTPAR
                       {
			 $$ = (formula_t*)malloc( sizeof( formula_t ) );
			 $$->type = SET_FORMULA;
			 $$->u.fset.literal = $3;
			 $$->u.fset.formula = $4;
		       }
                  |  LEFTPAR AND list_one_effect_formula RIGHTPAR
                       {
			 $$ = (formula_t*)malloc( sizeof( formula_t ) );
			 $$->type = AND_FORMULA;
			 $$->u.formula = $3;
		       }
                  |  LEFTPAR FORALL LEFTPAR plain_or_typed_list_var RIGHTPAR one_effect_formula RIGHTPAR
                       {
			 $$ = (formula_t*)malloc( sizeof( formula_t ) );
			 $$->type = FORALL_EFFECT;
			 $$->u.lambda.vars = $4;
			 $$->u.lambda.formula = $6;
		       }
                  |  LEFTPAR WHEN formula list_atomic_effect RIGHTPAR
                       {
			 $$ = (formula_t*)malloc( sizeof( formula_t ) );
			 if( $3->type != OR_FORMULA )
			   {
			     $$->type = WHEN_EFFECT;
			     $$->u.when.formula = $3;
			     $$->u.when.effect = $4;
			   }
			 else
			   {
			     $$->type = AND_FORMULA;
			     formula_t* cont = (formula_t*)malloc( sizeof( formula_t ) );
			     $$->u.formula = cont;
			     formula_t* init = $3->u.formula;
			     assert( init != NULL );
			     while( init != NULL )
			       {
				 assert( init->type == LIST_FORMULA );
				 cont->type = LIST_FORMULA;
				 // new When...
				 formula_t* newWhen = (formula_t*)malloc( sizeof( formula_t ) );
				 cont->u.flist.formula = newWhen;

				 newWhen->type = WHEN_EFFECT;
				 newWhen->u.when.formula = init->u.flist.formula;
				 newWhen->u.when.effect = copyReplaceFormula( $4, NULL );

				 init = init->u.flist.next;
				 if( init != NULL )
				   {
				     cont->u.flist.next = (formula_t*)malloc( sizeof( formula_t ) );
				     cont = cont->u.flist.next;
				   }
				 else
				   cont->u.flist.next = NULL;
			       }
			   }
		       }
                  ;

list_literal      :  list_literal literal
                       {
			 $$ = (formula_t*)malloc( sizeof( formula_t ) );
			 $$->type = LIST_FORMULA;
			 $$->u.flist.formula = $2;
			 $$->u.flist.next = $1;
		       }
                  |  literal
                       {
			 $$ = (formula_t*)malloc( sizeof( formula_t ) );
			 $$->type = LIST_FORMULA;
			 $$->u.flist.formula = $1;
			 $$->u.flist.next = NULL;
		       }
                  ;

list_atomic_effect
                  :  literal
                  |  LEFTPAR AND list_literal RIGHTPAR
                       {
			 $$ = (formula_t*)malloc( sizeof( formula_t ) );
			 $$->type = AND_FORMULA;
			 $$ = $3;
		       }
                  ;


/*******************************************************************************
 **
 **   Problem Definition
 **
 ******************************************************************************/

problem           :  LEFTPAR DEFINE LEFTPAR PROBLEM name RIGHTPAR 
                       {
			 _low_problemName = $5;
		       }
                     list_def_prob
                       {
			 idMatchArgsVars( initialSituation, NULL );
			 idMatchArgsVars( goalSituation, NULL );
			 generateNames();
		       }
                     RIGHTPAR
                  ;

list_def_prob     :  list_def_prob def_prob
                  |  /* empty */
                  ; 

def_prob          :  LEFTPAR REQUIREMENTS require_def
                  |  LEFTPAR SITUATION situation_def
                  |  LEFTPAR STRUCTURAL formula RIGHTPAR
                       {
			 structural = $3;
                       } 
                  |  LEFTPAR INIT list_formula RIGHTPAR
                       {
			 initialSituation = $3;
                       } 
                  |  LEFTPAR GOAL goal_formula RIGHTPAR
                       {
			 goalSituation = $3;
                       } 
                  |  LEFTPAR TWODOTS DOMAIN name RIGHTPAR
                  |  LEFTPAR OBJECTS plain_or_typed_list_name RIGHTPAR
                  ;

situation_def     :  name RIGHTPAR {}
                  ;

goal_formula      :  formula
                  |  LEFTPAR KNOW list_formula RIGHTPAR
		       {
			 $$ = (formula_t*)malloc( sizeof( formula_t ) );
			 $$->type = KNOW_FORMULA;
			 $$->u.formula = $3;
		       }
                  |  CERTAINTY
		       {
			 $$ = (formula_t*)malloc( sizeof( formula_t ) );
			 $$->type = CERTAINTY_FORMULA;
		       }
                  |  /* empty */
		       {
			 $$ = (formula_t*)malloc( sizeof( formula_t ) );
			 $$->type = EMPTY_FORMULA;
		       }
                  ;

%%

void
yyerror( char const *s )
{
  printf( "%s:%d: %s\n", _low_yyfile, lineno, s );
}

static void
checkRequirement( int req )
{
  switch( req )
    {
    case EQUALITY:
      _low_requirements = _low_requirements | REQ_EQUALITY;
      break;
    case TYPING:
      _low_requirements = _low_requirements | REQ_TYPING;
      break;
    case STRIPS:
      _low_requirements = _low_requirements | REQ_STRIPS;
      break;
    case CONDITIONAL_EFFECTS:
      _low_requirements = _low_requirements | REQ_CONDITIONAL_EFFECTS;
      break;
    case NEGATIVE_PRECONDITIONS:
      _low_requirements = _low_requirements | REQ_NEGATIVE_PRECONDITIONS;
      break;
    default:
      printf( "parser(checkRequirement): not supported requirement %d\n", req );
    }
}

void
printAtomicFormulaPDDL( ostream& o, formula_t *formula )
{
  if( formula != NULL )
    {
      // negation?
      if( formula->u.atomic.neg == 1 )
	o << "(not ";
      o << "(";
      // print name
      o << (formula->u.atomic.equal==1?"=":formula->u.atomic.name);

      // args
      if( formula->u.atomic.args != NULL )
	{
	  o << "_";
	  for( idList_t *arg = formula->u.atomic.args; arg != NULL; arg = arg->next )
	    o << arg->name << (arg->next != NULL ? "_" : ")");
	}
      else
	o << ")";

      if( formula->u.atomic.neg == 1 )
	o << ")";
    }
}


void
printAtomicFormulaPDDLTrans( ostream& o, formula_t *formula, string (*trans)(int) )
{
  int atom = atomId( formula, 0 );
  o << trans(formula->u.atomic.neg?-atom:atom);
}

void
printAtomicFormula( FILE *file, formula_t *formula )
{
  if( formula != NULL )
    {
      // negation?
      if( formula->u.atomic.neg == 1 )
	fprintf( file, "(not " );

      // print name
      fprintf( file, "(%s ", (formula->u.atomic.equal==1?"=":formula->u.atomic.name) );

      // args
      if( formula->u.atomic.args != NULL )
	for( idList_t *arg = formula->u.atomic.args; arg != NULL; arg = arg->next )
	  fprintf( file, "%s%s", arg->name, (arg->next != NULL ? " " : ")") );
      else
	fprintf( file, ")" );

      if( formula->u.atomic.neg == 1 )
	fprintf( file, ")" );
    }
}

void
printFormula( FILE *file, formula_t *formula )
{
  formula_t *f;
  if( formula == NULL )
    fprintf( file, "(null)" );
  else
    {
      switch( formula->type )
	{
	case ATOMIC_FORMULA:
	  printAtomicFormula( file, formula );
	  break;
	case AND_FORMULA:
	  fprintf( file, "(and " );
	  printFormula( file, formula->u.formula );
	  fprintf( file, ")" );
	  break;
	case OR_FORMULA:
	  fprintf( file, "(or " );
	  printFormula( file, formula->u.formula );
	  fprintf( file, ")" );
	  break;
	case ONEOF_FORMULA:
	  fprintf( file, "(oneof " );
	  printFormula( file, formula->u.formula );
	  fprintf( file, ")" );
	  break;
	case NOT_FORMULA:
	  fprintf( file, "(not " );
	  printFormula( file, formula->u.formula );
	  fprintf( file, ")" );
	  break;
	case UNKNOWN_FORMULA:
	  fprintf( file, "(unknown " );
	  printFormula( file, formula->u.formula );
	  fprintf( file, ")" );
	  break;
	case FORALL_FORMULA:
	case FORALL_EFFECT:
	  fprintf( file, "(forall (" );
	  for( idList_t *var = formula->u.lambda.vars; var != NULL; var = var->next )
	    fprintf( file, "%s%s", var->name, (var->next != NULL ? " " : "") );
	  fprintf( file, ") " );
	  printFormula( file, formula->u.lambda.formula );
	  fprintf( file, ")" );
	  break;
	case LIST_FORMULA:
	  fprintf( file, "[" );
	  for( f = formula; f != NULL; f = f->u.flist.next )
	    {
	      printFormula( file, f->u.flist.formula );
	      if( f->u.flist.next != NULL )
		fprintf( file, " " );
	    }
	  fprintf( file, "]" );
	  break;
	case EMPTY_FORMULA:
	  fprintf( file, ":empty:" );
	  break;
	case CERTAINTY_FORMULA:
	  fprintf( file, ":certainty:" );
	  break;
	case ADD_FORMULA:
	  fprintf( file, "(+ " );
	  printFormula( file, formula->u.formula );
	  fprintf( file, ")" );
	  break;
	case KNOW_FORMULA:
	  fprintf( file, "(know " );
	  printFormula( file, formula->u.formula );
	  fprintf( file, ")" );
	  break;
	case WHEN_EFFECT:
	  fprintf( file, "\n(when " );
	  printFormula( file, formula->u.when.formula );
	  fprintf( file, " " );
	  printFormula( file, formula->u.when.effect );
	  fprintf( file, ")" );
	  break;
	default:
	  printf( "parse(printFormula): unexpected formula type %d\n", formula->type );
	  break;
	}
    }
}


void
printFormulaPDDL( ostream& out, formula_t *formula )
{
  formula_t *f;
  if( formula == NULL )
    out << "(null)";
  else
    {
      switch( formula->type )
	{
	case ATOMIC_FORMULA:
	  printAtomicFormulaPDDL( out, formula );
	  break;
	case AND_FORMULA:
	  out << "(and ";
	  printFormulaPDDL( out, formula->u.formula );
	  out << ")";
	  break;
	case OR_FORMULA:
	  out << "(or ";
	  printFormulaPDDL( out, formula->u.formula );
	  out << ")";
	  break;
	case ONEOF_FORMULA:
	  out << "(oneof ";
	  printFormulaPDDL( out, formula->u.formula );
	  out << ")";
	  break;
	case NOT_FORMULA:
	  out << "(not ";
	  printFormulaPDDL( out, formula->u.formula );
	  out << ")";
	  break;
	case UNKNOWN_FORMULA:
	  out << "(unknown ";
	  printFormulaPDDL( out, formula->u.formula );
	  out << ")";
	  break;
	case FORALL_FORMULA:
	case FORALL_EFFECT:
	  out << "(forall (";
	  for( idList_t *var = formula->u.lambda.vars; var != NULL; var = var->next )
	    out << var->name << (var->next != NULL ? " " : "");
	  out << ") ";
	  printFormulaPDDL( out, formula->u.lambda.formula );
	  out << ")";
	  break;
	case LIST_FORMULA:
	  {
	    // out << "(and ";
	  for( f = formula; f != NULL; f = f->u.flist.next )
	    {
	      printFormulaPDDL( out, f->u.flist.formula );
	      if( f->u.flist.next != NULL )
		out << " ";
	    }
	  //out << ")";
	  }
	  break;
	case EMPTY_FORMULA:
	  out << ":empty:";
	  break;
	case CERTAINTY_FORMULA:
	  out << ":certainty:";
	  break;
	case ADD_FORMULA:
	  out << "(+ ";
	  printFormulaPDDL( out, formula->u.formula );
	  out << ")";
	  break;
	case KNOW_FORMULA:
	  out << "(know ";
	  printFormulaPDDL( out, formula->u.formula );
	  out << ")";
	  break;
	case WHEN_EFFECT:
	  out << "\n(when ";
	  printFormulaPDDL( out, formula->u.when.formula );
	  out << " ";
	  out << "(and ";
	  printFormulaPDDL( out, formula->u.when.effect );
	  out << ")";
	  out << ")";
	  break;
	default:
	  printf( "parse(printFormulaPDDL): unexpected formula type %d\n", formula->type );
	  break;
	}
    }
}

void
printFormulaPDDLTrans( ostream& out, formula_t *formula, string (*trans)(int) )
{
  formula_t *f;
  if( formula == NULL )
    out << "(null)";
  else
    {
      switch( formula->type )
	{
	case ATOMIC_FORMULA:
	  printAtomicFormulaPDDLTrans( out, formula, trans );
	  break;
	case AND_FORMULA:
	  out << "(and ";
	  printFormulaPDDLTrans( out, formula->u.formula, trans );
	  out << ")";
	  break;
	case OR_FORMULA:
	  out << "(or ";
	  printFormulaPDDLTrans( out, formula->u.formula, trans );
	  out << ")";
	  break;
	case ONEOF_FORMULA:
	  out << "(oneof ";
	  printFormulaPDDLTrans( out, formula->u.formula, trans );
	  out << ")";
	  break;
	case NOT_FORMULA:
	  out << "(not ";
	  printFormulaPDDLTrans( out, formula->u.formula, trans );
	  out << ")";
	  break;
	case UNKNOWN_FORMULA:
	  out << "(unknown ";
	  printFormulaPDDLTrans( out, formula->u.formula, trans );
	  out << ")";
	  break;
	case FORALL_FORMULA:
	case FORALL_EFFECT:
	  out << "(forall (";
	  for( idList_t *var = formula->u.lambda.vars; var != NULL; var = var->next )
	    out << var->name << (var->next != NULL ? " " : "");
	  out << ") ";
	  printFormulaPDDLTrans( out, formula->u.lambda.formula, trans );
	  out << ")";
	  break;
	case LIST_FORMULA:
	  {
	    // out << "(and ";
	  for( f = formula; f != NULL; f = f->u.flist.next )
	    {
	      printFormulaPDDLTrans( out, f->u.flist.formula, trans );
	      if( f->u.flist.next != NULL )
		out << " ";
	    }
	  //out << ")";
	  }
	  break;
	case EMPTY_FORMULA:
	  out << ":empty:";
	  break;
	case CERTAINTY_FORMULA:
	  out << ":certainty:";
	  break;
	case ADD_FORMULA:
	  out << "(+ ";
	  printFormulaPDDLTrans( out, formula->u.formula, trans );
	  out << ")";
	  break;
	case KNOW_FORMULA:
	  out << "(know ";
	  printFormulaPDDLTrans( out, formula->u.formula, trans );
	  out << ")";
	  break;
	case WHEN_EFFECT:
	  out << "\n(when ";
	  printFormulaPDDLTrans( out, formula->u.when.formula, trans );
	  out << " ";
	  out << "(and ";
	  printFormulaPDDLTrans( out, formula->u.when.effect, trans );
	  out << ")";
	  out << ")";
	  break;
	default:
	  printf( "parse(printFormulaPDDLTrans): unexpected formula type %d\n", formula->type );
	  break;
	}
    }
}

void
printInitialPDDL( ostream& out, formula_t *formula )
{
  formula_t *f;
  if( formula == NULL )
    out << "(null)";
  else
    {
      switch( formula->type )
	{
	case ATOMIC_FORMULA:
	  printAtomicFormulaPDDL( out, formula );
	  break;
	case AND_FORMULA:
	  out << "(and ";
	  printInitialPDDL( out, formula->u.formula );
	  out << ")";
	  break;
	case OR_FORMULA:
	  out << "(or ";
	  printInitialPDDL( out, formula->u.formula );
	  out << ")";
	  break;
	case ONEOF_FORMULA:
/* 	  out << "(oneof "; */
/* 	  printInitialPDDL( out, formula->u.formula ); */
/* 	  out << ")"; */
	  break;
	case NOT_FORMULA:
	  out << "(not ";
	  printInitialPDDL( out, formula->u.formula );
	  out << ")";
	  break;
	case UNKNOWN_FORMULA:
/* 	  out << "(unknown "; */
/* 	  printInitialPDDL( out, formula->u.formula ); */
/* 	  out << ")"; */
	  break;
	case FORALL_FORMULA:
	case FORALL_EFFECT:
	  out << "(forall (";
	  for( idList_t *var = formula->u.lambda.vars; var != NULL; var = var->next )
	    out << var->name << (var->next != NULL ? " " : "");
	  out << ") ";
	  printInitialPDDL( out, formula->u.lambda.formula );
	  out << ")";
	  break;
	case LIST_FORMULA:
	  {
	    // out << "(and ";
	  for( f = formula; f != NULL; f = f->u.flist.next )
	    {
	      printInitialPDDL( out, f->u.flist.formula );
	      if( f->u.flist.next != NULL )
		out << " ";
	    }
	  //out << ")";
	  }
	  break;
	case EMPTY_FORMULA:
	  out << ":empty:";
	  break;
	case CERTAINTY_FORMULA:
	  out << ":certainty:";
	  break;
	case ADD_FORMULA:
	  out << "(+ ";
	  printInitialPDDL( out, formula->u.formula );
	  out << ")";
	  break;
	case KNOW_FORMULA:
	  out << "(know ";
	  printInitialPDDL( out, formula->u.formula );
	  out << ")";
	  break;
	case WHEN_EFFECT:
	  out << "\n(when ";
	  printInitialPDDL( out, formula->u.when.formula );
	  out << " ";
	  out << "(and ";
	  printInitialPDDL( out, formula->u.when.effect );
	  out << ")";
	  out << ")";
	  break;
	default:
	  printf( "parse(printInitialPDDL): unexpected formula type %d\n", formula->type );
	  break;
	}
    }
}

bool
isSimpleFormula( formula_t* formula )
{
  if( formula == NULL ) return false;
  switch( formula->type )
    {
    case ATOMIC_FORMULA:
      return true;
      break;
    default:
      return false;
    }
}

bool
isDISJUNCTION( formula_t *formula, bool accept_or = false )
{
  if( formula == NULL )
    return false;
  else
    {
      switch( formula->type )
	{
	case ATOMIC_FORMULA:
	  return true;
	case OR_FORMULA:
	  if( !accept_or )
	    return false;
	case ONEOF_FORMULA:
	  {
	    formula_t* f = formula->u.formula;
	    bool val = true;
	    for( f = formula; f != NULL; f = f->u.flist.next )
	      val = val && isSimpleFormula( f->u.flist.formula );
	    return val;
	  }
	case UNKNOWN_FORMULA:
 	  return isSimpleFormula( formula->u.formula );

	case LIST_FORMULA:
	case AND_FORMULA:
	case NOT_FORMULA: // Not only for vars
	case FORALL_FORMULA:
	case FORALL_EFFECT:
	  return false;
	case EMPTY_FORMULA:
	case CERTAINTY_FORMULA:
	case ADD_FORMULA:
	case KNOW_FORMULA:
	case WHEN_EFFECT:
	default:
	  printf( "parse(isDISJUNCTION): unexpected formula type %d\n", formula->type );
	  exit(1);
	}
    }
}

bool
isCNF( formula_t *formula, bool accept_or = false )
{
  formula_t *f;
  if( formula == NULL )
    return false;
  else
    {
      switch( formula->type )
	{
	case LIST_FORMULA:
	  {
	    bool val = true;
	    for( f = formula; f != NULL; f = f->u.flist.next )
	      val = val && isDISJUNCTION( f->u.flist.formula, accept_or );
	    return val;
	  }
	case AND_FORMULA:
	  return isCNF( formula->u.formula, accept_or );
	case ATOMIC_FORMULA:
	case OR_FORMULA:
	case ONEOF_FORMULA:
	case UNKNOWN_FORMULA:
 	  return isDISJUNCTION( formula->u.formula, accept_or );

	case NOT_FORMULA: // Not only for vars
	case FORALL_FORMULA:
	case FORALL_EFFECT:
	  return false;
	case EMPTY_FORMULA:
	case CERTAINTY_FORMULA:
	case ADD_FORMULA:
	case KNOW_FORMULA:
	case WHEN_EFFECT:
	default:
	  printf( "parse(isCNF): unexpected formula type %d\n", formula->type );
	  exit(1);
	}
    }
}

int
getSimpleFormula( formula_t* formula )
{
  if( formula == NULL ) return 0;
  switch( formula->type )
    {
    case ATOMIC_FORMULA:
      if( !formula->u.atomic.equal )
	{
	  int atom = atomId( formula, NULL );
	  assert( atom > 0 ); // Atom doesn't exist
	  atom = (formula->u.atomic.neg?-1:1) * atom;
	  return atom;
	}
    default:
      printf( "parse(getSimpleFormula): unexpected formula type %d\n", formula->type );
      exit(1);
    }
}

// do not accept not({and|or} ..)
// neither (or ... )
static void
collectConjFormulaAtoms( formula_t* formula, int* parameters, std::set<int> &atoms )
{
  if( formula == NULL ) return;
  switch( formula->type )
    {
    case ATOMIC_FORMULA:
      if( !formula->u.atomic.equal )
	{
	  int atom = atomId( formula, parameters );
	  if( atom <= 0 ) return; // Atom doesn't exist
	  atom = (formula->u.atomic.neg?-1:1)*atom;
	  atoms.insert( atom );
	}
      break;
    case LIST_FORMULA:
      collectConjFormulaAtoms( formula->u.flist.formula, parameters, atoms );
      collectConjFormulaAtoms( formula->u.flist.next, parameters, atoms );
      break;

    case AND_FORMULA:
    case NOT_FORMULA: 
    case UNKNOWN_FORMULA:
    case ONEOF_FORMULA:
    case OR_FORMULA:
    default:
      printf( "parser(collectConjFormulaAtoms): unexpected formula type %d\n", formula->type );
      break;
    }
}

#ifdef USE_CLAUSE

void
getDISJUNCTION( formula_t *formula, clauses& c_or, clauses& c_oneof, 
		set<int>& literals, bool accept_or = false )
{
  formula_t *f;
  if( formula == NULL )
    return;
  else
    {
      switch( formula->type )
	{
	case ATOMIC_FORMULA:
	  {
	    int lit = getSimpleFormula(formula);
	    assert( lit != 0 );
	    literals.insert( lit );
	  }
	  break;
	case OR_FORMULA:
	  assert( accept_or );
	case ONEOF_FORMULA:
	  {
	    formula_t* f = formula->u.formula;
	    clause c;
	    collectConjFormulaAtoms( f, NULL, c );
	    if( formula->type == ONEOF_FORMULA )
	      c_oneof.insert(c);
	    else
	      c_or.insert(c);
	    break;
	    for( f = formula; f != NULL; f = f->u.flist.next )
	      {
		int lit = getSimpleFormula( f->u.flist.formula );
		assert( lit != 0 );
		c.insert(lit);
	      }
	  }
	  break;
	case UNKNOWN_FORMULA:
	  break;
	case LIST_FORMULA:
	  {
	    formula_t* f = formula->u.formula;
	    for( f = formula; f != NULL; f = f->u.flist.next )
	      getDISJUNCTION( f->u.flist.formula, c_or, c_oneof, literals, accept_or );
	  }
	  break;

	case AND_FORMULA:
	case NOT_FORMULA: // Not only for vars
	case FORALL_FORMULA:
	case FORALL_EFFECT:
	case EMPTY_FORMULA:
	case CERTAINTY_FORMULA:
	case ADD_FORMULA:
	case KNOW_FORMULA:
	case WHEN_EFFECT:
	default:
	  printf( "parse(getDISJUNCTION): unexpected formula type %d\n", formula->type );
	  exit(1);
	}
    }
}

void
getCNF( formula_t *formula, clauses& c_or, clauses& c_oneof, set<int>& literals, bool accept_or = false )
{
  formula_t *f;
  if( formula == NULL )
    return;
  else
    {
      switch( formula->type )
	{
	case LIST_FORMULA:
	  for( f = formula; f != NULL; f = f->u.flist.next )
	    getCNF( f->u.flist.formula, c_or, c_oneof, literals, accept_or );
	  break;
	case AND_FORMULA:
	  getCNF( formula->u.formula, c_or, c_oneof, literals, accept_or );
	  break;
	case ATOMIC_FORMULA:
	case OR_FORMULA:
	case ONEOF_FORMULA:
	case UNKNOWN_FORMULA:
	  getDISJUNCTION( formula, c_or, c_oneof, literals, accept_or );
	  break;

	case NOT_FORMULA: // Not only for vars
	case FORALL_FORMULA:
	case FORALL_EFFECT:
	case EMPTY_FORMULA:
	case CERTAINTY_FORMULA:
	case ADD_FORMULA:
	case KNOW_FORMULA:
	case WHEN_EFFECT:
	default:
	  printf( "parse(getCNF): unexpected formula type %d\n", formula->type );
	  exit(1);
	}
    }
}

#endif

void
collectFormulaUncertainty( formula_t *formula, std::vector<formula_t*>& formulas )
{
  formula_t *f;
  if( formula != NULL )
    {
      switch( formula->type )
	{
	case EMPTY_FORMULA:
	case CERTAINTY_FORMULA:
	case ATOMIC_FORMULA:
	  // discard
	  break;
	case AND_FORMULA:
	  collectFormulaUncertainty( formula->u.formula, formulas );
	  break;
	case ONEOF_FORMULA:
	case OR_FORMULA:
	  for( f = formula->u.formula; f != NULL; f = f->u.flist.next )
	    if(!isSimpleFormula( f->u.flist.formula ))
	      {
		printf( "ERROR: collectFormulaUncertainty: complex formula inside ONEOF or OR\n");
		exit(1);
	      }
	  formulas.push_back( formula );
	  break;
	case UNKNOWN_FORMULA:
	  // CFF benchmarks includes unknown for every unknown atom
	  // instead follow conformant planning competition 2006
	  formulas.push_back( formula );
	  break;
	case LIST_FORMULA:
	  for( f = formula; f != NULL; f = f->u.flist.next )
	    collectFormulaUncertainty( f->u.flist.formula, formulas );
	  break;
	case FORALL_FORMULA:
	case FORALL_EFFECT:
	case ADD_FORMULA:
	case WHEN_EFFECT:
	case KNOW_FORMULA:
	case NOT_FORMULA: // Not of atoms are atoms. Other NOT are not allowed
	default:
	  printf( "ERROR: collectFormulaUncertainty: unexpected formula type %d\n", formula->type );
	  exit(1);
	}
    }
}

void
printSchema( FILE *file, schema_t *schema )
{
  fprintf( file, "BEGIN-SCHEMA %s:\n", schema->name );
  fprintf( file, "  PARAMETERS = " );
  for( idList_t *id = schema->vars; id != NULL; id = id->next )
    fprintf( file, "%s ", id->name );
  fprintf( file, "\n  PRECONDITION = " );
  printFormula( file, schema->prec );
  fprintf( file, "\n  EFFECT = " );
  printFormula( file, schema->effect );
  fprintf( file, "\nEND-SCHEMA\n" );
}


static void
generateNames( void )
{
  str2wff_t::iterator it;
  map<const char*,idList_t*,ltstr>::iterator it2;

  _low_schemaName = (char**)calloc( _low_numberSchema, sizeof( char* ) );
  _low_objectName = (char**)calloc( numberObjects, sizeof( char* ) );
  _low_predicateName = (char**)calloc( _low_numberPredicates, sizeof( char* ) );

  for( int i = 0; i < _low_numberSchema; ++i )
    _low_schemaName[i] = strdup( _low_schemaTable[i]->name );

  for( it = predicates.begin(); it != predicates.end(); ++it )
    {
      _low_predicateName[(*it).second->u.atomic.id-1] = strdup( (*it).second->u.atomic.name );
      //xxxx printf( "name for p%d = \"%s\"\n", (*it).second->u.atomic.id, (*it).second->u.atomic.name );
    }

  for( it2 = objects.begin(); it2 != objects.end(); ++it2 )
    _low_objectName[(*it2).second->id-1] = strdup( (*it2).second->name );
}

vector<formula_t* > later_formula;
vector<idList_t *> later_object;


// Rescue unbounded predicates and objects
void
rescue_later()
{
  for( vector<formula_t*>::iterator it3 = later_formula.begin(); it3 != later_formula.end(); ++it3 )
    {
      formula_t* formula = *it3;
      str2wff_t::iterator it =
	predicates.find( formula->u.atomic.name );
      if( it == predicates.end() )
	{
	  printf( "FALTAL: parser(idMatchArgsVars): no predicate for %s\n", formula->u.atomic.name );
	  exit(1);
	}
      else
	{
	  formula->u.atomic.id = (*it).second->u.atomic.id;
	  formula->u.atomic.equal = (*it).second->u.atomic.equal;
	}	
    }
  for( vector<idList_t*>::iterator it3 = later_object.begin(); it3 != later_object.end(); ++it3 )
    {
      idList_t *arg = *it3;
      map<const char*,idList_t*,ltstr>::iterator it2 = 
	objects.find( arg->name );
      if( it2 == objects.end() )
	{
	  printf( "FATAL: parser(idMatchArgsVars): no object for %s\n", arg->name );
	  exit(1);
	}
      else
	arg->id = (*it2).second->id;
    }
}

static void
idMatchArgsVars( formula_t *formula, idList_t *vars )
{
  idList_t *arg, *var;
  str2wff_t::iterator it;
  map<const char*,idList_t*,ltstr>::iterator it2;

  if( formula != NULL )
    switch( formula->type )
      {
      case ATOMIC_FORMULA:
	for( str2wff_t::iterator it3 = predicates.begin(); 
	     it3 != predicates.end(); 
	     it3++ )
	  assert( it3->first != 0 );
	assert( formula->u.atomic.name != 0 );
	it = predicates.find( formula->u.atomic.name );
	if( it == predicates.end() )
	  {
	    printf( "parser(idMatchArgsVars): no predicate for %s\n", formula->u.atomic.name );
	    later_formula.push_back( formula );
	  }
	else
	  {
	    formula->u.atomic.id = (*it).second->u.atomic.id;
	    formula->u.atomic.equal = (*it).second->u.atomic.equal;
	  }

	for( arg = formula->u.atomic.args; arg != NULL; arg = arg->next )
	  if( arg->name[0] == '?' )
	    {
	      for( var = vars; var != NULL; var = var->next )
		if( !strcasecmp( arg->name, var->name ) )
		  {
		    arg->id = var->id;
		    break;
		  }
	      if( var == NULL )
		printf( "parser(idMatchArgsVars): no variable for %s\n", arg->name );
	    }
	  else
	    {
	      it2 = objects.find( arg->name );
	      if( it2 == objects.end() )
		{ 
		  printf( "parser(idMatchArgsVars): no object for %s\n", arg->name );
		  later_object.push_back( arg );
		  if(0){
		  cout << "Objects: ";
		  for( it2 = objects.begin(); it2 != objects.end(); ++it2 )
		    cout<< it2->second->name << " ";
		  cout << endl;
		  }
		}
	      else
		arg->id = (*it2).second->id;
	    }
	break;
      case AND_FORMULA:
      case OR_FORMULA:
      case NOT_FORMULA:
      case ADD_FORMULA:
      case KNOW_FORMULA:
      case ONEOF_FORMULA:
      case UNKNOWN_FORMULA:
	idMatchArgsVars( formula->u.formula, vars );
	break;
      case FORALL_FORMULA:
      case FORALL_EFFECT:
	// extend environment and make recursive call
	for( var = formula->u.lambda.vars; var->next != NULL; var = var->next );
	assert( (var != NULL) && (var->next == NULL) );
	var->next = vars;
	idMatchArgsVars( formula->u.lambda.formula, formula->u.lambda.vars );
	var->next = NULL;
	break;
      case LIST_FORMULA:
	idMatchArgsVars( formula->u.flist.formula, vars );
	idMatchArgsVars( formula->u.flist.next, vars );
	break;
      case EMPTY_FORMULA:
      case CERTAINTY_FORMULA:
	break;
      case SET_FORMULA:
	idMatchArgsVars( formula->u.fset.literal, vars );
	idMatchArgsVars( formula->u.fset.formula, vars );
	break;
      case WHEN_EFFECT:
	idMatchArgsVars( formula->u.when.formula, vars );
	idMatchArgsVars( formula->u.when.effect, vars );
	break;
      default:
	printf( "parse(idMatchArgsVars): unexpected formula type %d\n", formula->type );
	break;
      }
}

static bool
isOfType( idList_t *type1, idList_t *type2 )
{
  if( type1 == NULL && type2 == NULL )
    return true;
  if( type1 == NULL || type2 == NULL )
    return false;
  if( !strcasecmp( type1->name, type2->name ) )
    return( true );
  else
    {
      map<const char*,idList_t*,ltstr>::iterator it;
      for( idList_t *t1 = type1; t1 != NULL; t1 = t1->next )
	if( (it = types.find( t1->name )) != types.end() )
	  {
	      for( idList_t *t2 = (*it).second->type; t2 != NULL; t2 = t2->next )
		if( isOfType( t2, type2 ) )
		  return( true );
	    }
      return( false );
    }
}

static instantiation_t *
buildInstantiation( idList_t *vars, idList_t *args )
{
  if( !vars && !args )
    {
      return( NULL );
    }
  else if( vars && args )
    {
      instantiation_t *inst = (instantiation_t*)malloc( sizeof( instantiation_t ) );
      inst->var = vars;
      inst->val = args;
      inst->next = buildInstantiation( vars->next, args->next );
      return( inst );
    }
  else
    {
      printf( "parse(buildInstantiation): wrong number or arguments\n" );
      return( NULL );
    }
}

static formula_t *
copyReplaceAtomicFormula( formula_t *formula, instantiation_t *instantiation )
{
  instantiation_t *inst;
  idList_t *arg, *prevArg, *newArg;

  formula_t *result = (formula_t*)malloc( sizeof( formula_t ) );
  *result = *formula;
  result->u.atomic.args = NULL;

  // copy/replace arguments
  prevArg = NULL;
  for( arg = formula->u.atomic.args; arg != NULL; arg = arg->next )
    {
      newArg = (idList_t*)malloc( sizeof( idList_t ) );
      for( inst = instantiation; inst != NULL; inst = inst->next )
	if( !strcasecmp( arg->name, inst->var->name ) )
	  break;
      *newArg = *(inst==NULL?arg:inst->val);
      newArg->next = NULL;

      // link it
      if( prevArg == NULL )
	result->u.atomic.args = newArg;
      else
	prevArg->next = newArg;
      prevArg = newArg;
    }
  return( result );
}

static formula_t *
copyReplaceFormula( formula_t *formula, instantiation_t *inst )
{
  formula_t *result = NULL;
  if( formula != NULL )
    {
      switch( formula->type )
	{
	case ATOMIC_FORMULA:
	  result = copyReplaceAtomicFormula( formula, inst );
	  break;
	case AND_FORMULA:
	case OR_FORMULA:
	case NOT_FORMULA:
	case ADD_FORMULA:
	case KNOW_FORMULA:
	case ONEOF_FORMULA:
	case UNKNOWN_FORMULA:
	  result = (formula_t*)malloc( sizeof( formula_t ) );
	  *result = *formula;
	  result->u.formula = copyReplaceFormula( formula->u.formula, inst );
	  break;
	case FORALL_FORMULA:
	case FORALL_EFFECT:
	  result = (formula_t*)malloc( sizeof( formula_t ) );
	  *result = *formula;
	  result->u.lambda.formula = copyReplaceFormula( formula->u.lambda.formula, inst );
	  break;
	case LIST_FORMULA:
	  result = (formula_t*)malloc( sizeof( formula_t ) );
	  *result = *formula;
	  result->u.flist.formula = copyReplaceFormula( formula->u.flist.formula, inst );
	  result->u.flist.next = copyReplaceFormula( formula->u.flist.next, inst );
	  break;
	default:
	  result = formula;
	  break;
	}
    }
  return( result );
}

static formula_t *
instantiateFormula( formula_t* formula, idList_t* vars, instantiation_t* inst, formula_t *result )
{
  map<const char*,idList_t*,ltstr>::iterator it;
  if( vars == NULL )
    {
      formula_t *aux = (formula_t*)malloc( sizeof( formula_t ) );
      aux->type = LIST_FORMULA;
      aux->u.flist.formula = unfoldQuantifiers( copyReplaceFormula( formula, inst ) );
      aux->u.flist.next = result;
      result = aux;
    }
  else
    {
      instantiation_t newInst;
      newInst.next = inst;
      newInst.var = vars;
      for( it = objects.begin(); it != objects.end(); ++it )
	if( isOfType( (*it).second->type, vars->type ) )
	  {
	    newInst.val = (*it).second;
	    result = instantiateFormula( formula, vars->next, &newInst, result );
	  }
    }
  return( result );
}

static formula_t *
unfoldQuantifiers( formula_t *formula )
{
  formula_t *result = NULL;
  if( formula != NULL )
    {
      result = formula;
      switch( formula->type )
	{
	case ATOMIC_FORMULA:
	  break;
	case AND_FORMULA:
	case OR_FORMULA:
	case NOT_FORMULA:
	case ADD_FORMULA:
	case KNOW_FORMULA:
	case ONEOF_FORMULA:
	case UNKNOWN_FORMULA:
	  result->u.formula = unfoldQuantifiers( formula->u.formula );
	  break;
	case FORALL_FORMULA:
	case FORALL_EFFECT:
	  result = (formula_t*)malloc( sizeof( formula_t ) );
	  result->type = AND_FORMULA;
	  result->u.formula = instantiateFormula( formula->u.lambda.formula, formula->u.lambda.vars, NULL, NULL );
	  break;
	case LIST_FORMULA:
	  result->u.flist.formula = unfoldQuantifiers( result->u.flist.formula );
	  result->u.flist.next = unfoldQuantifiers( result->u.flist.next );
	  break;
	case SET_FORMULA:
	  result->u.fset.formula = unfoldQuantifiers( formula->u.fset.formula );
	  break;
	case WHEN_EFFECT:
	  result->u.when.formula = unfoldQuantifiers( formula->u.when.formula );
	  result->u.when.effect = unfoldQuantifiers( formula->u.when.effect );
	  break;
	default:
	  printf( "parse(unfoldQuantifiers): unexpected formula type %d\n", formula->type );
	  break;
	}
    }
  return( result );
}

static formula_t *
linearizeEffect( formula_t *formula, formula_t* &tail )
{
  formula_t *aux, *result;
  result = tail = NULL;
  if( formula != NULL )
    switch( formula->type )
      {
      case ATOMIC_FORMULA:
      case EMPTY_FORMULA:
      case SET_FORMULA:
      case WHEN_EFFECT:
	result = (formula_t*)malloc( sizeof( formula_t ) );
	result->type = LIST_FORMULA;
	result->u.flist.formula = formula;
	result->u.flist.next = NULL;
	tail = result;
	break;
      case AND_FORMULA:
	result = linearizeEffect( formula->u.formula, tail );
	break;
      case LIST_FORMULA:
	result = linearizeEffect( formula->u.flist.formula, aux );
	aux->u.flist.next = linearizeEffect( formula->u.flist.next, tail );
	if( tail == NULL )
	  tail = aux;
	break;
      default:
	printf( "parse(linearizeEffect): unexpected formula type %d\n", formula->type );
	break;
      }
  return( result );
}

// return -1 if parameters are not instantiated
static int
atomId( formula_t *literal, int* parameters )
{
  static int arguments[MAXPARAMETERS];

  // argument extraction
  int *p = arguments;
  *p++ = literal->u.atomic.id;
  for( idList_t *arg = literal->u.atomic.args; arg != NULL; arg = arg->next )
    if( arg->name[0] == '?' )
      {
	assert( arg->id > 0 );
	if( parameters[arg->id] <= 0 )
	  return -1; // Atom does not exist
	*p++ = parameters[arg->id];
      }
    else
      *p++ = arg->id;
  *p = -1;

  // generate atomId
  return( readAtomHash( arguments )->idx );
}


void
generateAtoms( void )
{
  formula_t *formula;
  str2wff_t::iterator it;

  for( it = predicates.begin(); it != predicates.end(); ++it )
    if( (*it).second->u.atomic.equal == 0 )
      {
	// generate instantiations and insert into atom hash
	formula = instantiateFormula( (*it).second, (*it).second->u.atomic.args, NULL, NULL );
	while( formula != NULL )
	  {
	    atomId( formula->u.flist.formula, NULL );
	    formula = formula->u.flist.next;
	  }
      }
}

void
generateVarsAndValues( void )
{
  int i, j, k;
  idList_t *var;
  map<const char*,idList_t*,ltstr>::iterator it;

  // vars
  _low_vars = (int**)calloc( _low_numberSchema, sizeof( int* ) );
  for( i = 0; i < _low_numberSchema; ++i )
    {
      _low_vars[i] = (int*)calloc( maxParameters + 1, sizeof( int ) );
      for( var = _low_schemaTable[i]->vars, j = 0; var != NULL; var = var->next, ++j )
	_low_vars[i][j] = var->id;
    }

  // values
  _low_values = (int**)calloc( _low_numberSchema * (maxParameters + 1), sizeof( int* ) );
  for( i = 0; i < _low_numberSchema * maxParameters; ++i )
    _low_values[i] = (int*)calloc( numberObjects+1, sizeof( int ) );
  for( i = 0; i < _low_numberSchema; ++i )
    for( var = _low_schemaTable[i]->vars; var != NULL; var = var->next )
      {
	k = 0;
	if( var->type == NULL )
	  {
	    for( it = objects.begin(); it != objects.end(); ++it )
	      _low_values[_low_numberSchema*(var->id-1) + i][k++] = (*it).second->id;
	  }
	else
	  {
	    if( !(_low_requirements & REQ_TYPING) )
	      printf( "parser(generateVarsAndValues): using types without :typing requirements.\n" );
	    for( it = objects.begin(); it != objects.end(); ++it )
	      if( !strcasecmp( var->type->name, (*it).second->type->name ) ||
		  isOfType( (*it).second->type, var->type ) )
		_low_values[_low_numberSchema*(var->id-1) + i][k++] = (*it).second->id;
	  }
      }
}

static nnf::node
generateEffect( nnf::Manager *man, nnf::node avar, formula_t *formula, int *parameters, lit_effect_t &frame )
{
  nnf::node res, tmp1, tmp2, tmp3;
  lit_effect_t t_frame;
  assert( (avar != (unsigned)-1) || (formula->type != WHEN_EFFECT) );

  switch( formula->type )
    {
    case LIST_FORMULA:
      res = nnf::make_value( man, true );
      for( formula_t *eff = formula; eff != 0; eff = eff->u.flist.next )
	{
	  tmp1 = generateEffect( man, avar, eff->u.flist.formula, parameters, frame );
	  tmp2 = nnf::make_and( man, res, tmp1 );
	  man->unregister_use( res );
	  man->unregister_use( tmp1 );
	  res = tmp2;
	}
      break;
    case ATOMIC_FORMULA:
      // CNF = action => lit = -action || lit
      tmp1 = generateAtomicFormula( man, formula, parameters, true );
      if( avar != (unsigned)-1 )
	res = nnf::make_or( man, avar, tmp1 );
      else
	{
	  res = tmp1;
	  man->register_use( tmp1 );
	}
      frame.push_back( std::make_pair( (formula_t*)0, std::make_pair( nnf::make_value(man,true), tmp1 ) ) );
      break;
    case SET_FORMULA:
      assert( 0 );
      break;
    case WHEN_EFFECT:
      // CNF:  action & condition => lit  <=>  -action || -condition || lit
      tmp1 = generateFormula( man, formula->u.when.formula, parameters, false );
      tmp2 = nnf::make_not( man, tmp1 );
      tmp3 = nnf::make_or( man, avar, tmp2 );
      man->unregister_use( tmp2 );
      tmp2 = generateEffect( man, (unsigned)-1, formula->u.when.effect, parameters, t_frame );
      res = nnf::make_or( man, tmp3, tmp2 );
      man->unregister_use( tmp3 );
      man->unregister_use( tmp2 );
      tmp2 = nnf::make_cnf( man, res );
      man->unregister_use( res );
      res = tmp2;
      for( size_t i = 0; i < t_frame.size(); ++i )
	{
	  man->register_use( tmp1 );
	  frame.push_back( std::make_pair( formula->u.when.formula, std::make_pair(tmp1,t_frame[i].second.second) ) );
	  man->unregister_use( t_frame[i].second.first );
	}
      man->unregister_use( tmp1 );
      break;
    default:
      res = nnf::make_value( man, true );
      printf( "parse(generateEffect): unexpected formula type %d\n", formula->type );
      break;
    }
  return( res );
}

static void
collectEffectAtoms( formula_t *formula, int *parameters, std::vector<int> &atoms, std::vector<int> *atoms_conds )
{
  switch( formula->type )
    {
    case LIST_FORMULA:
      for( formula_t *eff = formula; eff != 0; eff = eff->u.flist.next )
	collectEffectAtoms( eff->u.flist.formula, parameters, atoms, atoms_conds );
      break;
    case ATOMIC_FORMULA:
      collectFormulaAtoms( formula, parameters, atoms );
      break;
    case WHEN_EFFECT:
      if( atoms_conds )
	collectFormulaAtoms( formula->u.when.formula, parameters, *atoms_conds );
      collectEffectAtoms( formula->u.when.effect, parameters, atoms, atoms_conds );
      break;
    default:
      printf( "parse(collectEffectAtoms): unexpected formula type %d\n", formula->type );
      break;
    }
}

static void
collectUncondEffectAtoms( formula_t *formula, int *parameters, std::vector<int> &atoms )
{
  switch( formula->type )
    {
    case LIST_FORMULA:
      for( formula_t *eff = formula; eff != 0; eff = eff->u.flist.next )
	collectUncondEffectAtoms( eff->u.flist.formula, parameters, atoms );
      break;
    case ATOMIC_FORMULA:
      collectFormulaAtoms( formula, parameters, atoms );
      break;
    case AND_FORMULA:
      collectUncondEffectAtoms( formula->u.formula, parameters, atoms );
      printf( "CHECK: pass through AND in collectEffectAtoms\n" );
      break;
    case SET_FORMULA:
      assert( 0 );
      break;
    case WHEN_EFFECT:
      break;
    default:
      printf( "parse(collectEffectAtoms): unexpected formula type %d\n", formula->type );
      break;
    }
}
void
mapWhenEffects( formula_t *formula, int *parameters, std::vector<formula_t*> &clist )
{
  switch( formula->type )
    {
    case LIST_FORMULA:
      for( formula_t *eff = formula; eff != 0; eff = eff->u.flist.next )
	mapWhenEffects( eff->u.flist.formula, parameters, clist );
      break;
    case ATOMIC_FORMULA:
      break;
    case SET_FORMULA:
      assert( 0 );
      break;
    case WHEN_EFFECT:
      clist.push_back( formula->u.when.formula );
      mapWhenEffects( formula->u.when.effect, parameters, clist );
      break;
    default:
      printf("parse(mapWhenEffects): unexpected formula type %d\n", formula->type);
      break;
    }
}

int
equalityValue( formula_t *formula, int *parameters )
{
  int val[2];
  idList_t *arg[2];
  arg[0] = formula->u.atomic.args;
  arg[1] = formula->u.atomic.args->next;
  for( int i = 0; i < 2; ++i )
    {
      if( arg[i]->name[0] == '?' )
	{
	  assert( arg[i]->id > 0 );
	  if( parameters[arg[i]->id] <= 0 )
	    return -1; // Atom does not exist
	  val[i] = parameters[arg[i]->id];
	}
      else
	val[i] = arg[i]->id;
    }
  return( val[0] == val[1] );
}

static nnf::node
generateAtomicFormula( nnf::Manager *man, formula_t *formula, int *parameters, bool future )
{
  if( formula->u.atomic.equal == 1 )
    {
      bool test = equalityValue( formula, parameters );
      if( formula->u.atomic.neg ) test = !test;
      return( nnf::make_value( man, (test?true:false) ) );
    }
  else
    {
      int atom = atomId( formula, parameters );
      if( reachable.find( atom ) == reachable.end() )
	return( nnf::make_value( man, true ) ); // can only happen if formula is init
      else
	{
	  atom = reachable[atom];
	  atom = (future?atom+time_shift:atom)<<1; // present or future?
	  atom = (formula->u.atomic.neg?1+atom:atom);
	  return( nnf::make_variable( man, atom ) );
	}
    }
}

nnf::node
generateFormula( nnf::Manager *man, formula_t* formula, int* parameters, bool future )
{
  nnf::node res, tmp1, tmp2;
  if( formula == NULL ) return( nnf::make_value( man, true ) );
  switch( formula->type )
    {
    case ATOMIC_FORMULA:
      res = generateAtomicFormula( man, formula, parameters, future );
      break;
    case OR_FORMULA:
    case AND_FORMULA:
      res = nnf::make_value( man, (formula->type!=OR_FORMULA) );
      for( formula_t *f = formula->u.formula; f != NULL; f = f->u.flist.next )
	{
	  tmp1 = generateFormula( man, f->u.flist.formula, parameters, future );
	  if( formula->type == OR_FORMULA )
	    tmp2 = nnf::make_or( man, res, tmp1 );
	  else
	    tmp2 = nnf::make_and( man, res, tmp1 );
	  man->unregister_use( res );
	  man->unregister_use( tmp1 );
	  res = tmp2;
	}
      break;
    case LIST_FORMULA: // Verify it
      res = nnf::make_value( man, true );
      for( formula_t *f = formula; f != 0; f = f->u.flist.next ) 
	{
	  tmp1 = generateFormula( man, f->u.flist.formula, parameters, future );
	  tmp2 = nnf::make_and( man, res, tmp1 );
	  man->unregister_use( res );
	  man->unregister_use( tmp1 );
	  res = tmp2;
	}
      break;
    case NOT_FORMULA:
      tmp1 = generateFormula( man, formula->u.formula, parameters, future );
      res = nnf::make_not( man, tmp1 );
      man->unregister_use( tmp1 );
      break;
      // OJO: COMPLETAR!
    case ONEOF_FORMULA:
    case UNKNOWN_FORMULA:
      res = nnf::make_value( man, true );
      printf( "don't generate formula for ONEOF or UNKNOWN yet. Type %d\n", formula->type );
      break;
    default:
      res = nnf::make_value( man, true );
      printf( "parser(generateFormula): unexpected formula type %d\n", formula->type );
      //assert( 0 != 0 );
      break;
    }
  return( res );
}

static void
collectFormulaEquals( formula_t* formula, int* parameters, std::vector<formula_t*> &equals )
{
  if( formula == NULL ) return;
  switch( formula->type )
    {
    case ATOMIC_FORMULA:
      if( formula->u.atomic.equal )
	equals.push_back(formula);
      break;
    case AND_FORMULA:
      for( formula_t *f = formula->u.formula; f != NULL; f = f->u.flist.next )
	collectFormulaEquals( f->u.flist.formula, parameters, equals );
      break;
    case NOT_FORMULA:
      collectFormulaEquals( formula->u.formula, parameters, equals );
      break;
    case LIST_FORMULA:
      collectFormulaEquals( formula->u.flist.formula, parameters, equals );
      collectFormulaEquals( formula->u.flist.next, parameters, equals );
      break;
    case UNKNOWN_FORMULA:
    case OR_FORMULA:
    case ONEOF_FORMULA:
    default:
      printf( "parser(collectFormulaEquals): unexpected formula type %d\n", formula->type );
      break;
    }
}

// Collect atom already instantiated
static void
collectFormulaAtoms( formula_t* formula, int* parameters, std::vector<int> &atoms )
{
  if( formula == NULL ) return;
  switch( formula->type )
    {
    case ATOMIC_FORMULA:
      if( !formula->u.atomic.equal )
	{
	  int atom = atomId( formula, parameters );
	  if( atom <= 0 ) return; // Atom doesn't exist
	  atom = (formula->u.atomic.neg?1+(atom<<1):(atom<<1));
	  atoms.push_back( atom );
	}
      break;
    case OR_FORMULA:
    case AND_FORMULA:
    case ONEOF_FORMULA:
      for( formula_t *f = formula->u.formula; f != NULL; f = f->u.flist.next )
	collectFormulaAtoms( f->u.flist.formula, parameters, atoms );
      break;
    case UNKNOWN_FORMULA:
      collectFormulaAtoms( formula->u.formula, parameters, atoms );
      break;
    case LIST_FORMULA:
      collectFormulaAtoms( formula->u.flist.formula, parameters, atoms );
      collectFormulaAtoms( formula->u.flist.next, parameters, atoms );
      break;
    case NOT_FORMULA:
    default:
      printf( "parser(collectFormulaAtoms): unexpected formula type %d\n", formula->type );
      break;
    }
}

void
collectAllAtoms( formula_t* formula, int* parameters, std::vector<int> &atoms )
{
  if( formula == NULL ) return;
  switch( formula->type )
    {
    case LIST_FORMULA:
      for( formula_t *f = formula; f != 0; f = f->u.flist.next )
	collectAllAtoms( f->u.flist.formula, parameters, atoms );
      break;
    case ATOMIC_FORMULA:
      if( !formula->u.atomic.equal )
	{
	  int atom = atomId( formula, parameters );
	  atom = (formula->u.atomic.neg?1+(atom<<1):(atom<<1));
	  atoms.push_back( atom );
	}
      break;
    case OR_FORMULA:
    case AND_FORMULA:
    case ONEOF_FORMULA:
      for( formula_t *f = formula->u.formula; f != NULL; f = f->u.flist.next )
	collectAllAtoms( f->u.flist.formula, parameters, atoms );
      break;
    case NOT_FORMULA:
    case UNKNOWN_FORMULA:
      collectAllAtoms( formula->u.formula, parameters, atoms );
      break;
    case SET_FORMULA:
      assert( 0 );
      break;
    case WHEN_EFFECT:
      collectAllAtoms( formula->u.when.formula, parameters, atoms );
      collectAllAtoms( formula->u.when.effect, parameters, atoms );
      break;
    default:
      printf( "parser(collectAllAtoms): unexpected formula type %d\n", formula->type );
      break;
    }
}

void
generateSchema( nnf::Manager *man, nnf::node action, schema_t* schema, int* parameters )
{
  // need to generate cnfs for precondition and effects (frame will do elsewhere)
  // ASSUMPTION: avar is negative action literal 

  // CNF(prec) = prec || !action
  // ASSUMPTION: precondition formula is in positive form
  nnf::node tmp1 = generateFormula( man, schema->prec, parameters, false );
  nnf::node tmp2 = nnf::make_or( man, action, tmp1 );
  man->unregister_use( tmp1 );
  nnf::node prec = nnf::make_cnf( man, tmp2 );
  man->unregister_use( tmp2 );

  // cnf for effectstrue if action is 
  _low_operator.frame_->clear();
  nnf::node effect = generateEffect( man, action, schema->effect, parameters, *_low_operator.frame_ );
  tmp1 = nnf::make_and( man, prec, effect );
  man->unregister_use( prec );
  man->unregister_use( effect );
  _low_operator.cnf_ = tmp1;
}

// Return false can't be executed (typically, equals on precods)
// Return only atoms instatiated by parameters
bool
operatorAtoms( schema_t *schema, int *parameters, 
	       std::set<int> &prec, std::set<int> &add, std::set<int> &del,
	       std::set<int>* reachable_atoms = 0, 
	       std::set<int>* neg_prec = 0,  
	       std::set<int>* conds = 0 )
{
  std::vector<int> atoms_vec;
  collectPlainFormulaAtoms( schema->prec, parameters, atoms_vec );
  for( size_t i = 0; i < atoms_vec.size(); ++i )
    {
      if( atoms_vec[i] % 2 ) 
	{
	  if( reachable_atoms != 0 &&
	      reachable_atoms->count( atoms_vec[i] >> 1 ) )
	    return false;
	  if(neg_prec != 0) neg_prec->insert( atoms_vec[i] >> 1 );
	}
      else
	prec.insert( atoms_vec[i] >> 1 );
    }

  // Check if equals agree...
  std::vector<formula_t*> equals;
  collectFormulaEquals( schema->prec, parameters, equals );
  for( size_t i = 0; i < equals.size(); ++i )
    {
      assert( equals[i]->type = ATOMIC_FORMULA &&
	      equals[i]->u.atomic.equal == 1 );
      int test = equalityValue( equals[i], parameters );
      if( test != -1 )
	{
	  if( equals[i]->u.atomic.neg ) test = !test;
	  if( !test )
	    return false;
	}
    }
    
  atoms_vec.clear();
  vector<int> atoms_conds;
  if( conds )
    collectEffectAtoms( schema->effect, parameters, atoms_vec, &atoms_conds );    
  else
    collectEffectAtoms( schema->effect, parameters, atoms_vec );
  for( size_t i = 0; i < atoms_vec.size(); ++i )
    {
      if( atoms_vec[i] % 2 )
	del.insert( atoms_vec[i]>>1 );
      else
	add.insert( atoms_vec[i]>>1 );
    }
  if(conds)
    for( size_t i = 0; i < atoms_conds.size(); ++i )
      {
	if( atoms_conds[i] % 2 )
	  conds->insert( -int(atoms_conds[i]>>1) );
	else
	  conds->insert( atoms_conds[i]>>1 );
      }
  return true;
}



// do not accept not({and|or} ..)
// neither (or ... )
static void
collectPlainFormulaAtoms( formula_t* formula, int* parameters, std::vector<int> &atoms )
{
  if( formula == NULL ) return;
  switch( formula->type )
    {
    case ATOMIC_FORMULA:
      if( !formula->u.atomic.equal )
	{
	  int atom = atomId( formula, parameters );
	  if( atom <= 0 ) return; // Atom doesn't exist
	  atom = (formula->u.atomic.neg?1+(atom<<1):(atom<<1));
	  atoms.push_back( atom );
	}
      break;
    case AND_FORMULA:
      for( formula_t *f = formula->u.formula; f != NULL; f = f->u.flist.next )
	collectPlainFormulaAtoms( f->u.flist.formula, parameters, atoms );
      break;
    case NOT_FORMULA: 
      if ( formula->u.formula->type != ATOMIC_FORMULA )
	{
	  printf( "parser(collectPlainFormulaAtoms): not of a non atomic formula\n");
	  exit(1);
	}
      collectPlainFormulaAtoms( formula->u.formula, parameters, atoms );
      break;
    case LIST_FORMULA:
      collectPlainFormulaAtoms( formula->u.flist.formula, parameters, atoms );
      collectPlainFormulaAtoms( formula->u.flist.next, parameters, atoms );
      break;
    case UNKNOWN_FORMULA:
    case ONEOF_FORMULA:
    case OR_FORMULA:
    default:
      printf( "parser(collectPlainFormulaAtoms): unexpected formula type %d\n", formula->type );
      break;
    }
}

void
collectPlainFormulaCertainAtoms( formula_t* formula, std::set<int> &atoms )
{
  if( formula == NULL ) return;
  switch( formula->type )
    {
    case ATOMIC_FORMULA:
      if( !formula->u.atomic.equal )
	{
	  int atom = atomId( formula, 0 );
	  int a = (formula->u.atomic.neg?-int(atom):int(atom));
	  atoms.insert( a );
	}
      break;
    case AND_FORMULA:
      for( formula_t *f = formula->u.formula; f != NULL; f = f->u.flist.next )
	collectPlainFormulaCertainAtoms( f->u.flist.formula, atoms );
      break;
    case NOT_FORMULA: 
      if ( formula->u.formula->type != ATOMIC_FORMULA )
	{
	  printf( "parser(collectPlainFormulaCertainAtoms): not of a non atomic formula\n");
	  exit(1);
	}
      collectPlainFormulaCertainAtoms( formula->u.formula, atoms );
      break;
    case LIST_FORMULA:
      collectPlainFormulaCertainAtoms( formula->u.flist.formula, atoms );
      collectPlainFormulaCertainAtoms( formula->u.flist.next, atoms );
      break;
    case UNKNOWN_FORMULA:
    case ONEOF_FORMULA:
    case OR_FORMULA:
      break;
    default:
      printf( "parser(collectPlainFormulaCertainAtoms): unexpected formula type %d\n", formula->type );
      break;
    }
}

// Some atoms that appear for sure on every model (simplest case)
void
collectPlainFormulaPosAtoms( formula_t* formula, std::set<int> &atoms )
{
  if( formula == NULL ) return;
  switch( formula->type )
    {
    case ATOMIC_FORMULA:
      if( !formula->u.atomic.equal )
	{
	  int atom = atomId( formula, 0 );
	  int a = (formula->u.atomic.neg?-int(atom):int(atom));
	  atoms.insert( a );
	}
      break;
    case AND_FORMULA:
      for( formula_t *f = formula->u.formula; f != NULL; f = f->u.flist.next )
	collectPlainFormulaPosAtoms( f->u.flist.formula, atoms );
      break;
      break;
    case LIST_FORMULA:
      collectPlainFormulaPosAtoms( formula->u.flist.formula, atoms );
      collectPlainFormulaPosAtoms( formula->u.flist.next, atoms );
      break;
    case NOT_FORMULA: 
    case UNKNOWN_FORMULA:
    case OR_FORMULA:
    case ONEOF_FORMULA:
      break;
    default:
      printf( "parser(collectPlainFormulaPosAtoms): unexpected formula type %d\n", formula->type );
      break;
    }
}


bool
isComplexFormula2( formula_t* formula )
{
  bool val = false;
  if( formula == NULL ) return false;
  switch( formula->type )
    {
    case ATOMIC_FORMULA:
      return false;
      break;
    case ONEOF_FORMULA:
    case AND_FORMULA:
      for( formula_t *f = formula->u.formula; f != NULL; f = f->u.flist.next )
	val = val || isComplexFormula2( f->u.flist.formula );
      break;
    case UNKNOWN_FORMULA:
      val = val || isComplexFormula2( formula->u.formula );
    case NOT_FORMULA: 
      if ( formula->u.formula->type == ATOMIC_FORMULA )
	return false;
      else
	return true;
      break;
    case LIST_FORMULA:
      val = val || isComplexFormula2( formula->u.flist.formula );
      val = val || isComplexFormula2( formula->u.flist.next );
      break;
    case OR_FORMULA:
      return true;
    }
  return val;
}

bool
isComplexFormula( formula_t* formula )
{
  bool val = false;
  if( formula == NULL ) return false;
  switch( formula->type )
    {
    case ATOMIC_FORMULA:
      return false;
      break;
    case AND_FORMULA:
      for( formula_t *f = formula->u.formula; f != NULL; f = f->u.flist.next )
	val = val || isComplexFormula( f->u.flist.formula );
      break;
    case NOT_FORMULA: 
      if ( formula->u.formula->type == ATOMIC_FORMULA )
	return false;
      else
	return true;
      break;
    case LIST_FORMULA:
      val = val || isComplexFormula( formula->u.flist.formula );
      val = val || isComplexFormula( formula->u.flist.next );
      break;
    case UNKNOWN_FORMULA:
    case ONEOF_FORMULA:
    case OR_FORMULA:
      return true;
    }
  return val;
}

static int
collectCondEffectAtoms( formula_t *formula, int *parameters,  
			std::vector<int>& atoms, Act &act, vector<int>* r2a,
			const std::set<int>* reachable_atoms,
			const std::set<int>* static_positive_atoms = 0 )
{
  int total = 0;
  const bool debugit = false;

  switch( formula->type )
    {
    case LIST_FORMULA:
      for( formula_t *eff = formula; eff != 0; eff = eff->u.flist.next )
	total += collectCondEffectAtoms( eff->u.flist.formula, parameters, atoms, act, r2a,
					 reachable_atoms,
					 static_positive_atoms );
      break;
    case ATOMIC_FORMULA:
      break;
    case SET_FORMULA:
      assert( 0 );
      break;
    case WHEN_EFFECT:
      {
	total = 1;
	When w;
	atoms.clear();
	collectPlainFormulaAtoms( formula->u.when.formula, parameters, atoms );
	for( size_t i = 0; i < atoms.size(); ++i )
	  {
	    size_t old_a = atoms[i]>>1;
	    size_t a = 0;
	    if( r2a )
	      {
		if( (atoms[i]>>1) >= r2a->size() )
		  {
		    cout << "av = " <<  (atoms[i]>>1) 
			 << ", r2a = " << r2a->size() << endl;
		  }
		assert( (atoms[i]>>1) < r2a->size() );
		a = (*r2a)[atoms[i]>>1];
	      }
	    else
	      a = old_a;
	    if(debugit)
	      cout << "in When: considering condition atom " << (atoms[i] % 2?-1:1)*old_a 
		   << ". new value = " << a << endl;
	    if( atoms[i] % 2 )
	      {
		// Can't be made positive
		if( static_positive_atoms && static_positive_atoms->count( old_a ) > 0 )
		  {
		    if(debugit)
		      cout << "rejecting because atom " << old_a  << " is static " << endl;
		    // Isn't right
		    //act.conds.pop_back();
		    return 0;
		  }
		w.prec.insert( -a );
	      }
	    else
	      {
		// Adding non-static atoms
		if( !static_positive_atoms || static_positive_atoms->count( old_a ) == 0 ) 
		  {
		    if( reachable_atoms && reachable_atoms->count( old_a ) == 0 ) 
		      {
			if(debugit)
			  cout << "rejecting because can't reach atom " << old_a << endl;
			return 0;
		      }
		    w.prec.insert( a );
		    if(debugit)
		      cout << "Adding atom " << a << endl;
		  }
		else if(debugit)
		  cout << "rejecting because is static and positive " << old_a << endl;
	      }
	  }
	if(debugit)
	  cout << "Prec of condition size: " << w.prec.size() << endl;
	atoms.clear();
	collectPlainFormulaAtoms( formula->u.when.effect, parameters, atoms );
	  for( size_t i = 0; i < atoms.size(); ++i )
	    {
	      if( r2a && (atoms[i]>>1) >= r2a->size() )
		{
		  cout << "av = " <<  (atoms[i]>>1) 
		       << ", r2a = " << r2a->size() << endl;
		  assert( (atoms[i]>>1) < r2a->size() );
		}
	      size_t a;
	      if( r2a )
		a = (*r2a)[atoms[i]>>1];
	      else
		a = atoms[i]>>1;
	      if( atoms[i] % 2 )
		w.eff.insert( -a );
	      else
		w.eff.insert( a );
	      if(debugit)
		cout << "in When: considering effect atom " << (atoms[i] % 2?-1:1)*(atoms[i]>>1)
		     << ". new value = " << a << endl;
	    }
	atoms.clear();
	if( w.prec.size() == 0 )
	  for( set<int>::iterator e = w.eff.begin();
	       e != w.eff.end(); ++e )
	    act.eff.insert(*e);
	else
	  act.addWhen(w);
	if(debugit)
	  {
	    cout << "now action is ";
	    act.dump(cout);
	  }
      }
      break;
    default:
      printf( "parse(collectCondEffectAtoms): unexpected formula type %d\n", formula->type );
      break;
    }
  return total;
}

bool
operatorAtomsWhen( schema_t *schema, int *parameters, Act &act, vector<int>* r2a,
		   const std::set<int>& reachable_atoms,
		   const std::set<int>* static_positive_atoms = 0 )
{
  const bool debugit = false;

  std::vector<int> atoms_vec;
  collectPlainFormulaAtoms( schema->prec, parameters, atoms_vec );
  for( size_t i = 0; i < atoms_vec.size(); ++i )
    {
      int old_a = atoms_vec[i]>>1;
      size_t a = 0;
      if( r2a ) 
	{
	  if( (atoms_vec[i]>>1) >= r2a->size() )
	    {
	      cerr << "FATAL ERROR: av = " <<  (atoms_vec[i]>>1) 
		   << ", r2a = " << r2a->size() << endl;
	    }
	  assert( (atoms_vec[i]>>1) < r2a->size() );
	  a = (*r2a)[atoms_vec[i]>>1];
	}
      else
	a = old_a;
      if(debugit)
	cout << "considering atom " << (atoms_vec[i] % 2?-1:1)*old_a << ". new value = " << a << endl;
      if( atoms_vec[i] % 2 )
	{
	  // Can't be made positive
	  if( static_positive_atoms && static_positive_atoms->count( old_a ) > 0 )
	    {
	      if(debugit)
		cout << "rejecting because atom " << old_a << endl;
	      return false;
	    }

	  if( reachable_atoms.count( old_a ) != 0 )
	    { // Can be made positive, so we can't remove it
	      cerr << "FATAL ERROR: Non-static negative preconditions not supported. Final check" << endl;
	      exit(1);
	    }
	}
      else
	{
	  if( !static_positive_atoms || static_positive_atoms->count( old_a ) == 0 ) 
	    {
	      if(debugit)
		cout << "Adding atom " << a << endl;
	      act.prec.insert( a );
	    }
	}
    }
  if(debugit)
    cout << "Prec of action size: " << act.prec.size() << endl;

  atoms_vec.clear();
  collectUncondEffectAtoms( schema->effect, parameters, atoms_vec );
  for( size_t i = 0; i < atoms_vec.size(); ++i )
    {
      if( r2a && (atoms_vec[i]>>1) >= r2a->size() )
	{
	  cout << "av = " <<  (atoms_vec[i]>>1) 
	       << ", r2a = " << r2a->size() << endl;
	  assert( (atoms_vec[i]>>1) < r2a->size() );
	}

      size_t a;
      if( r2a )
	a = (*r2a)[atoms_vec[i]>>1];
      else
	a = atoms_vec[i]>>1;

      if( atoms_vec[i] % 2 )
	act.eff.insert( -a );
      else
	act.eff.insert( a );
    }
  int added = collectCondEffectAtoms( schema->effect, parameters, atoms_vec, act, r2a, 
				      &reachable_atoms, static_positive_atoms );
  if( act.eff.size() == 0 && added == 0 )
    return false; // Do nothing
  else
    return true;
}

void
operatorAtoms( schema_t *schema, int *parameters, std::set<int> &atoms )
{
  std::vector<int> atoms_vec;
  collectAllAtoms( schema->prec, parameters, atoms_vec );
  collectAllAtoms( schema->effect, parameters, atoms_vec );
  for( size_t i = 0; i < atoms_vec.size(); ++i )
    atoms.insert( atoms_vec[i] >> 1 );
}

void
formulaAtomsPos( formula_t *formula, int *parameters, int phase, std::set<int> &atoms )
{
  std::vector<int> atoms_vec;
  collectFormulaAtoms( formula, parameters, atoms_vec );
  for( size_t i = 0; i < atoms_vec.size(); ++i )
    if( (atoms_vec[i] % 2) == phase )
      atoms.insert( atoms_vec[i] >> 1 );
}

void
formulaAtoms( formula_t *formula, int *parameters, std::set<int> &atoms )
{
  std::vector<int> atoms_vec;
  collectFormulaAtoms( formula, parameters, atoms_vec );
  for( size_t i = 0; i < atoms_vec.size(); ++i )
    atoms.insert( atoms_vec[i] >> 1 );
}

void
formulaLiterals( formula_t *formula, int *parameters, std::set<int> &atoms )
{
  std::vector<int> atoms_vec;
  collectFormulaAtoms( formula, parameters, atoms_vec );
  for( size_t i = 0; i < atoms_vec.size(); ++i )
    if( atoms_vec[i] % 2) 
      atoms.insert( -(atoms_vec[i] >> 1) );
    else
      atoms.insert( atoms_vec[i] >> 1 );
}
