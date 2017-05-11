%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <assert.h>

#include "types.h"


/* defines */
#define HASHSIZE        (20*1024)


/* prototypes */
void checkRequirement( int );
char *buildPredicate( struct aformula_s *, struct instantiation_s * );
void emitPrologue( void );
void emitEpilogue( void );
void insertHash( char *, struct hashentry_s * );
char *translate( char * );

void usage( void );
void identifyObjects( void );
void tagFormulaList( struct aformula_s *formulaList );
void identifyTypes( void );
void identifyDomains( void );
void idMatchArgsVars( struct aformula_s *formula, struct idlist_s *vars );
void emitNames( void );
void emitActions( void );
void mystery( void );

int yylex();
int yyerror(const char*);

/* global data */
int domainParsed = 0;
char *problemName, *domainName = NULL;
char *yyfile, *yyparser;

int notDuplicates = 1, prunning = 0;
int maxParameters = 0, numObjects = 0;
 
int number = 0, numActions;;
FILE *xfile, *yfile, *zfile;
struct hashentry_s *hashtable[HASHSIZE];

struct idlist_s *objects = NULL;
struct action_s *actions = NULL;
struct aformula_s *predicates = NULL;
struct aformula_s *initialSituation = NULL;
struct aformula_s *goalSituation = NULL;

%}


%token   LEFTPAR RIGHTPAR NAME TWODOTS 
%token   DEFINE DOMAIN REQUIREMENTS CONSTANTS PREDICATES
%token   QUESTION STRIPS EQUALITY CONDITIONAL
%token   DOM_AXIOMS DISJUNCTIVE ADL 
%token   E_PRECONDITIONS U_PRECONDITIONS
%token   AND NOT FORALL WHEN EQUAL
%token   ACTION PARAMETERS PRECONDITION EFFECT
%token   PROBLEM INIT GOAL LENGTH SITUATION OBJECTS
%token   SERIAL PARALLEL INTEGER
%token   TYPING TYPES


%type    <integer>     require_key
%type    <ident>       NAME predicate variable term
%type    <idlist>      list_name list_var list_term list_parameters
%type    <idlist>      types_def
%type    <aformula>    atomic_formula_var list_atomic_formula_var
%type    <aformula>    atomic_formula_term
%type    <aformula>    goal_description list_goal_description effect list_effect
%type    <body>        action_body list_action_body


%start   start


%%


start             :  list_domain_problem
                  ;

list_domain_problem
                  :  list_domain_problem problem
                  |  list_domain_problem domain
                  |  /* empty */
                  ;

domain            :  LEFTPAR DEFINE LEFTPAR DOMAIN NAME RIGHTPAR
                         {
			   if( !domainName )
			     {
			       fprintf( stderr, "parsing domain: no problem domain clause (bad file order)\n" );
			       usage();
			     }
			   
			   strcpy( $5, translate( $5 ) );
			   strcpy( domainName, translate( domainName ) );
			   if( strcmp( domainName, $5 ) )
			     {
			       fprintf( stderr, "WARNING: domain names don't match (%s,%s)\n", $5, domainName );
			     }
			 }
                       LEFTPAR list_def
                         {
			   identifyObjects();
			   tagFormulaList( initialSituation );
			   tagFormulaList( goalSituation );

			   emitPrologue();
			 }
                       list_structure_def
                         {
			   struct action_s *action;
			   struct idlist_s *var;
			   int i;

			   identifyTypes();
			   identifyDomains();

			   emitNames();
			   mystery();

			   /* how much parameters can an action have? */
			   for( action = actions; action != NULL; action = action->next )
			     {
			       for( var = action->vars, i = 0; var != NULL; var = var->next, ++i );
			       maxParameters = (maxParameters > i ? maxParameters : i);
			     }

			   emitActions();
			   emitEpilogue();
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
			 predicates = (struct aformula_s *) malloc( sizeof( struct aformula_s ) );
			 assert( predicates != NULL );
			 predicates->id = 0;
			 predicates->neg = 0;
			 predicates->type = 0;
			 predicates->equal = 1;
			 predicates->func = strdup( "=" );
			 predicates->args = NULL;
			 predicates->domain = NULL;
			 predicates->next = NULL;
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

types_def         :  list_name RIGHTPAR
                       {
			 $$ = $1;
		       }
                  ;

constants_def     :  list_name RIGHTPAR
                       {
			 struct idlist_s *object;

			 if( objects == NULL )
			   objects = $1;
			 else
			   {
			     for( object = objects; object->next != NULL; object = object->next );
			     object->next = $1;
			   }
		       }
                  ;

list_name         : NAME list_name
                       {
			 $$ = (struct idlist_s *) malloc( sizeof( struct idlist_s ) );
			 assert( $$ != NULL );
			 $$->id = 0;
			 $$->name = $1;
			 $$->type = NULL;
			 $$->next = $2;
		       }
                  |  NAME
                       {  
			 $$ = (struct idlist_s *) malloc( sizeof( struct idlist_s ) );
			 assert( $$ != NULL );
			 $$->id = 0;
			 $$->name = $1;
			 $$->type = NULL;
			 $$->next = NULL;
		       }
                  ;

predicates_def    :  list_atomic_formula_var RIGHTPAR
                       {
			 struct aformula_s *predicate;
			 int i;

			 if( predicates == NULL )
			   predicates = $1;
			 else
			   {
			     for( predicate = predicates; predicates->next != NULL; predicate = predicate->next );
			     predicate->next = $1;
			   }
			    
			 i = 1;
			 for( predicate = predicates; predicate != NULL; predicate = predicate->next )
			   predicate->id = i++;
                       }
                  ;

list_atomic_formula_var 
                  :  list_atomic_formula_var atomic_formula_var
                       {
			 $2->next = $1;
			 $$ = $2;
		       }
                  |  atomic_formula_var
                  ;

atomic_formula_var 
                  :  LEFTPAR predicate list_var RIGHTPAR
                       {
			 $$ = (struct aformula_s *) malloc( sizeof( struct aformula_s ) );
			 assert( $$ != NULL );
			 $$->id = -1;
			 $$->neg = 0;
			 $$->type = 0;
			 $$->func = $2;
			 $$->equal = 0;
			 $$->args = $3;
			 $$->domain = NULL;
			 $$->next = NULL;
		       }
                  ;

list_var          :  variable list_var
                       {
			 $$ = (struct idlist_s *) malloc( sizeof( struct idlist_s ) );
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

predicate         :  NAME
                  |  EQUAL
                       {
			 $$ = "=";
		       }
                  ;

variable          :  QUESTION NAME
                       {
			 $$ = (char *) malloc( strlen( $2 ) + 2 );
			 assert( $$ != NULL );
			 $$[0] = '\0';
			 strcat( $$, "?" );
			 strcat( $$, $2 );
		       }
                  ;

list_structure_def 
                  :  list_structure_def LEFTPAR structure_def
                  |  structure_def
                  ;

structure_def     :  action_def
                  ;

action_def        :  ACTION NAME list_action_body RIGHTPAR
                       {
			 struct aformula_s *formula, *tmp;
			 struct action_s *action;
			 struct body_s *body;
			 
			 action = (struct action_s *) malloc( sizeof( struct action_s ) );
			 assert( action != NULL );
			 action->name = $2;
			 action->next = NULL;
			 
			 for( body = $3; body != NULL; body = body->next )
			   switch( body->type )
			     {
			     case PARAMETERS:
			       action->vars = body->data.parameters;
			       break;
			     case PRECONDITION:
			       action->prec = body->data.precondition;
			       break;
			     case EFFECT:
			       action->add = NULL;
			       action->del = NULL;
			       formula = body->data.effect;
			       while( formula != NULL )
				 {
				   tmp = formula->next;
				   if( !formula->neg )
				     {
				       formula->next = action->add;
				       action->add = formula;
				     }
				   else
				     {
				       formula->next = action->del;
				       action->del = formula;
				     }
				   formula = tmp;
				 }
			       break;
			     }

			 /* identification */
			 idMatchArgsVars( action->prec, action->vars );
			 idMatchArgsVars( action->add, action->vars );
			 idMatchArgsVars( action->del, action->vars );

			 /* link actions */
			 action->next = actions;
			 actions = action;
		       }
                  ;

list_action_body  :  list_action_body action_body
                       {
			 $2->next = $1;
			 $$ = $2;
		       }
                  |  /* empty */
                       {
			 $$ = NULL;
		       }
                  ;

action_body       :  PARAMETERS LEFTPAR list_parameters RIGHTPAR
                       {
			 $$ = (struct body_s *) malloc( sizeof( struct body_s ) );
			 assert( $$ != NULL );
			 $$->type = PARAMETERS;
			 $$->data.parameters = $3;
		       }
                  |  PRECONDITION goal_description
                       {
			 $$ = (struct body_s *) malloc( sizeof( struct body_s ) );
			 assert( $$ != NULL );
			 $$->type = PRECONDITION;
			 $$->data.precondition = $2;
		       }
                  |  EFFECT effect
                       {
			 $$ = (struct body_s *) malloc( sizeof( struct body_s ) );
			 assert( $$ != NULL );
			 $$->type = EFFECT;
			 $$->data.effect = $2;
		       }
                  ;

list_parameters   :  list_var
                       {
			 struct idlist_s *var;
			 int i;
			 
			 i = 1;
			 for( var = $1; var != NULL; var = var->next )
			   var->id = i++;
			 $$ = $1;
		       }
                  ;

goal_description  :  atomic_formula_term
                  |  LEFTPAR AND list_goal_description RIGHTPAR
                       {
			 $$ = $3;
		       }
                  |  LEFTPAR NOT atomic_formula_term RIGHTPAR
                       {
			 $3->neg = 1;
			 $$ = $3;
		       }
                  ;

list_goal_description 
                  :  list_goal_description goal_description
                       {
			 if( $2 != NULL )
			   {
			     $2->next = $1;
			     $$ = $2;
			   }
			 else
			   $$ = $1;
		       }
                  |  /* empty */
                       {
			 $$ = NULL;
		       }
                  ;

atomic_formula_term 
                  :  LEFTPAR predicate list_term RIGHTPAR 
                       {
			 $$ = (struct aformula_s *) malloc( sizeof( struct aformula_s ) );
			 assert( $$ != NULL );
			 $$->id = -1;
			 $$->neg = 0;
			 $$->type = 0;
			 $$->equal = 0;
			 $$->func = $2;
			 $$->args = $3;
			 $$->domain = NULL;
			 $$->next = NULL;
		       }
                  ;

list_term         :  term list_term
                       {
			 $$ = (struct idlist_s *) malloc( sizeof( struct idlist_s ) );
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

term              :  NAME
                  |  variable
                  ;

effect            :  LEFTPAR AND list_effect RIGHTPAR
                       {
			 $$ = $3;
		       }
                  |  LEFTPAR NOT atomic_formula_term RIGHTPAR
                       {
			 $3->neg = 1;
			 $$ = $3;
		       }
                  |  atomic_formula_term 
/*
                  |  LEFTPAR FORALL LEFTPAR list_var RIGHTPAR effect RIGHTPAR
                  |  LEFTPAR WHEN goal_description effect RIGHTPAR
*/
                  ;

list_effect       :  list_effect effect
                       {
			 if( $2 != NULL )
			   {
			     $2->next = $1;
			     $$ = $2;
			   }
			 else
			   $$ = $1;
		       }
                  |  /* empty */
                       {
			 $$ = NULL;
		       }
                  ;


/*********************************************************************************
 ***** PROBLEM DEFINITION
 **/


problem           :  LEFTPAR DEFINE LEFTPAR PROBLEM NAME RIGHTPAR 
                       {
			 problemName = strdup( translate( $5 ) );

			 if( domainParsed )
			   {
			     fprintf( stderr, "parsing problem: bad file order\n" );
			     usage();
			   }
		       }
                     list_def_prob
                     RIGHTPAR
                  ;

list_def_prob     :  list_def_prob def_prob
                  |  /* empty */
                  ; 

def_prob          :  LEFTPAR REQUIREMENTS require_def
                  |  LEFTPAR SITUATION situation_def
                  |  LEFTPAR INIT init_def
                  |  LEFTPAR GOAL goal_def
                  |  LEFTPAR LENGTH length_def
                  |  LEFTPAR TWODOTS DOMAIN NAME RIGHTPAR
                       {
			 domainName = $4;
		       }
                  |  LEFTPAR OBJECTS list_name RIGHTPAR
                       {
			 struct idlist_s *object;

			 if( objects == NULL )
			   objects = $3;
			 else
			   {
			     for( object = objects; object->next != NULL; object = object->next );
			     object->next = $3;
			   }
		       }
                  ;

situation_def     :  NAME RIGHTPAR
                  ;

init_def          :  list_goal_description RIGHTPAR
                       {
			 struct aformula_s *formula;

			 initialSituation = $1;
			 for( formula = initialSituation; formula != NULL; formula = formula->next )
			   if( formula->neg )
			     fprintf( stderr, "WARNING: negative literal in initial situation\n" );
		       }
                  ;

goal_def          :  goal_description RIGHTPAR
                       {
			 struct aformula_s *formula;

			 goalSituation = $1;
			 for( formula = goalSituation; formula != NULL; formula = formula->next )
			   if( formula->neg )
			     fprintf( stderr, "WARNING: negative literal in goal situation\n" );
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
yyerror( const char *s )
{
  extern int lineno;

  fprintf( stderr, "%s:%d: %s\n", yyfile, lineno, s );
  return( 0 );
}


void
usage( void )
{
  fprintf( stderr, "usage: %s <domain-pddl> <problem-pddl>\n", yyparser );
  exit( -1 );
}


int
main( int argc, char **argv )
{
  int rv, fd, file;
  extern int yyparse( void );
  extern int lineno;

  /* check usage */
  yyparser = argv[0];
  if( argc != 3 )
    usage();

  /* open output files */
  xfile = fopen( "xfile", "w+" );
  yfile = fopen( "yfile", "w+" );
  zfile = fopen( "zfile", "w+" );
  if( !xfile || !yfile || !zfile )
    {
      perror( "main: " );
      exit( -1 );
    }

  /* parse files */
  rv = 0;
  for( file = 2; file > 0; --file )
    if( (fd = open( argv[file], O_RDONLY )) == -1 )
      {
	perror( "main: " );
	exit( -1 );
      }
    else
      {
	/* redirection of fd to stdin */
	if( file == 2 )
	  close( fileno( stdin ) );
	else
	  clearerr( stdin );
	dup( fd );
	yyfile = argv[file];
	lineno = 1;
	rv += yyparse();
	close( fileno( stdin ) );
	close( fd );
      }

  /* close output files */
  fclose( xfile );
  fclose( yfile );
  fclose( zfile );

  return( rv );
}


unsigned
hashFunction( char *key )
{
  unsigned rv;
  char *p;

  for( p = key, rv = 0; *p != '\0'; rv += (unsigned) *p++ );
  return( rv % HASHSIZE );
}


struct hashentry_s *
readHash( char *key )
{
  unsigned index;
  struct hashentry_s *entry;
  
  index = hashFunction( key );
  for( entry = hashtable[index]; entry != NULL; entry = entry->next )
    if( !strcmp( key, entry->name ) )
      return( entry );

  /* Is not in hash, insert it */
  entry = (struct hashentry_s *) malloc( sizeof( hashentry_s ) );
  assert( entry != NULL );
  entry->name = strdup( key );
  entry->number = ++number;
  entry->next = NULL;
  insertHash( key, entry );
  return( entry );
}


void
insertHash( char *key, struct hashentry_s *entry )
{
  unsigned index;

  index = hashFunction( key );
  entry->next = hashtable[index];
  hashtable[index] = entry;
}


char *
buildPredicate( struct aformula_s *pred, struct instantiation_s *insts )
{
  struct idlist_s *vp;
  struct instantiation_s *ip;
  static char buf[1024];

  strcpy( buf, "(" );
  strcat( buf, pred->func );
  strcat( buf, " " );
  for( vp = pred->args; vp != NULL; vp = vp->next )
    {
      if( vp->name[0] == '?' )
	{
	  for( ip = insts; ip != NULL; ip = ip->next )
	    if( !strcmp( ip->var, vp->name ) )
	      strcat( buf, ip->val );
	}
      else
	strcat( buf, vp->name );

      if( vp->next )
	strcat( buf, " " );
    }
  strcat( buf, ")" );
  return( buf );
}


void
checkRequirement( int req )
{
  switch( req )
    {
    case STRIPS:
    case EQUALITY:
      break;
    default:
      fprintf( stderr, "Not supported requirement %d\n", req );
    }
}


void
emitPrologue( void )
{
  fprintf( xfile, "/* automatically generated code -- DO NOT EDIT BY HAND! */\n\n" );
  fprintf( xfile, "#include <stdio.h>\n" );
  fprintf( xfile, "#include <string.h>\n" );
  fprintf( xfile, "#include <limits.h>\n" );
  fprintf( xfile, "#include <assert.h>\n" );
  fprintf( xfile, "#include \"planner.h\"\n\n" );
  fprintf( xfile, "int (*heuristicActionPreconditionsTable[MAXACTIONS])( register int * );\n" );
  fprintf( xfile, "int (*heuristicActionTable[MAXACTIONS])( register int * );\n" );
  fprintf( xfile, "static int preconditions[MAXPARAMETERS];\n" );
  fprintf( xfile, "int preclist[MAXPARAMETERS], addlist[MAXPARAMETERS];\n" );
  fprintf( xfile, "extern struct cost_s costs[], oldCosts[];\n\n" );
  
  fprintf( yfile, "/* automatically generated code -- DO NOT EDIT BY HAND! */\n\n" );
  fprintf( yfile, "#include <stdio.h>\n" );
  fprintf( yfile, "#include <string.h>\n" );
  fprintf( yfile, "#include \"planner.h\"\n\n" );
  fprintf( yfile, "int (*actionPreconditionsTable[MAXACTIONS])( register int *, register struct literal_s * );\n\n" );
  
  fprintf( zfile, "/* automatically generated code -- DO NOT EDIT BY HAND! */\n\n" );
  fprintf( zfile, "#include <stdlib.h>\n" );
  fprintf( zfile, "#include <string.h>\n" );
  fprintf( zfile, "#include \"planner.h\"\n\n" );
  fprintf( zfile, "extern void fillTableAux( void );\n\n" );
}


void
emitXFileEpilogue( void )
{
  char *name;
  struct action_s *action;
  struct aformula_s *formula;
  struct hashentry_s *entry;


  /* setupHeuristic */
  fprintf( xfile, "void\n" );
  fprintf( xfile, "setupHeuristic( void )\n" );
  fprintf( xfile, "{\n" );
  for( action = actions; action != NULL; action = action->next )
    {
      fprintf( xfile, "  heuristicActionTable[%d] = &heuristicAction_%s;\n", action->id, action->name );
      fprintf( xfile, "  heuristicActionPreconditionsTable[%d] = &heuristicActionPreconditions_%s;\n", action->id, action->name );
    }
  fprintf( xfile, "}\n\n" );
  
  
  /* goalAtoms */
  fprintf( xfile, "int goalAtoms[] = { " );
  
  for( formula = goalSituation; formula != NULL; formula = formula->next )
    {
      entry = readHash( name = buildPredicate( formula, NULL ) );
      fprintf( xfile, "%d /* %s */, ", entry->number, name );
    }
  fprintf( xfile, "0 };\n\n" );
}


void
emitYFileEpilogue( void )
{
  char *pn;
  struct hashentry_s *entry;
  struct aformula_s *formula;
  struct action_s *action;


  /* generate code for initial situation */
  fprintf( yfile, "void\n" );
  fprintf( yfile, "setInitialSituation( void )\n" );
  fprintf( yfile, "{\n" );
  for( formula = initialSituation; formula != NULL; formula = formula->next )
    {
      /* we must assign a entry->number to all atoms */
      entry = readHash( pn = buildPredicate( formula, NULL ) );
      if( !formula->neg )
	{
	  entry = readHash( pn = buildPredicate( formula, NULL ) );
	  fprintf( yfile, "  situation[%d].lit = 1;\n", entry->number );
	}
    }
  fprintf( yfile, "}\n\n" );


  /* generate code for goal situation */
  fprintf( yfile, "int\n" );
  fprintf( yfile, "goalSituation( struct literal_s *sit )\n" );
  fprintf( yfile, "{\n" );
  fprintf( yfile, "  return(%s", (!goalSituation ? " 0" : " ") );
  for( formula = goalSituation; formula != NULL; formula = formula->next )
    {
      entry = readHash( pn = buildPredicate( formula, NULL ) );
      fprintf( yfile, "%ssit[%d].lit", (formula->neg ? "!" : ""), entry->number );
      if( formula->next )
	fprintf( yfile, " && " );
    }
  fprintf( yfile, " );\n" );
  fprintf( yfile, "}\n\n" );
  
  
  /* generate code for operator tables */
  fprintf( yfile, "void\n" );
  fprintf( yfile, "fillTableAux( void )\n" );
  fprintf( yfile, "{\n" );
  
  for( action = actions; action != NULL; action = action->next )
    {
      fprintf( yfile, "  actionTable[%d] = &action_%s;\n", action->id, action->name );
      fprintf( yfile, "  actionPreconditionsTable[%d] = &actionPreconditions_%s;\n", action->id, action->name );
    }
  fprintf( yfile, "}\n\n" );
}


void
insertAtomsInHash( struct aformula_s *list )
{
  struct aformula_s *formula;
  struct idlist_s *var;
  int i;

  for( formula = list; formula != NULL; formula = formula->next )
    {
      fprintf( zfile, "  p[0] = %d;  /* %s */\n", formula->id, formula->func );
      for( var = formula->args, i = 1; var != NULL; var = var->next, ++i )
	fprintf( zfile, "  p[%d] = %d;  /* %s */\n", i, var->id, var->name );
      fprintf( zfile, "  p[%d] = -1;  /* end-marker */\n", i );
      fprintf( zfile, "  readAtomHash( p );\n" );
    }
}


void
emitZFileEpilogue( void )
{
  int i, j, k;
  struct aformula_s *predicate;
  struct action_s *action;
  struct idlist_s *var, *object, *domain;


  /* fillTable */
  fprintf( zfile, "void\n" );
  fprintf( zfile, "fillTable( void )\n" );
  fprintf( zfile, "{\n" );
  fprintf( zfile, "  register int i;\n" );
  fprintf( zfile, "  static int p[MAXPARAMETERS];\n\n" );
  fprintf( zfile, "  fillTableAux();\n" );
  fprintf( zfile, "  numActions = %d;\n\n", numActions );


  /* vars  */
  fprintf( zfile, "  /* vars */\n" );
  fprintf( zfile, "  vars = (int **) calloc( numActions, sizeof( int * ) );\n" );
  fprintf( zfile, "  for( i = 0; i < numActions; ++i )\n" );
  fprintf( zfile, "    vars[i] = (int *) calloc( %d, sizeof( int ) );\n", maxParameters + 1 );
  for( action = actions, i = 0; action != NULL; action = action->next, ++i )
    for( var = action->vars, j = 0; var != NULL; var = var->next, ++j )
      fprintf( zfile, "  vars[%d][%d] = %d;\n", i, j, var->id );
  fprintf( zfile, "\n" );
  
  
  /* values  */
  fprintf( zfile, "  /* values */\n" );
  fprintf( zfile, "  values = (int **) calloc( numActions * %d, sizeof( int * ) );\n", maxParameters + 1 );
  fprintf( zfile, "  for( i = 0; i < numActions * %d; ++i )\n", maxParameters );
  fprintf( zfile, "    values[i] = (int *) calloc( %d, sizeof( int ) );\n", numObjects + 1 );
  for( action = actions, i = 0; action != NULL; action = action->next, ++i )
    for( var = action->vars, j = 0; var != NULL; var = var->next, ++j )
      for( predicate = predicates; predicate != NULL; predicate = predicate->next )
	if( !predicate->equal )
	  {
	    if( (var->type->name == NULL) || !strcmp( var->type->name, predicate->func ) )
	      {
		if( (var->type->name == NULL) || (predicate->type == 0) )
		  {
		    for( object = objects, k = 0; object != NULL; object = object->next, ++k )
		      fprintf( zfile, "  values[numActions * %d + %d][%d] = %d;\n", var->id - 1, i, k, object->id );
		  }
		else
		  {
		    for( domain = predicate->domain, k = 0; domain != NULL; domain = domain->next, ++k )
		      fprintf( zfile, "  values[numActions * %d + %d][%d] = %d;\n", var->id - 1, i, k, domain->id );
		  }
		break;
	      }
	  }
  fprintf( zfile, "\n" );
  
  
  /* insert atoms in atomHashTable: don't change order (must match parser order) */
  fprintf( zfile, "  /* insert atoms in atomHashTable */\n" );
  insertAtomsInHash( goalSituation );
  insertAtomsInHash( initialSituation );
  fprintf( zfile, "}\n\n" );
}


void
emitEpilogue( void )
{
  emitXFileEpilogue();
  emitYFileEpilogue();
  emitZFileEpilogue();
}


void
emitActionPreconditions( struct action_s *action, FILE *file )
{
  struct aformula_s *prec;
  struct idlist_s *arg;
  int i, label;

  if( file == yfile )
    {
      fprintf( file, "int\n" );
      fprintf( file, "actionPreconditions_%s( register int *parameters, register struct literal_s *sit )\n", action->name );
      fprintf( file, "{\n" );
    }
  if( file == xfile )
    {
      fprintf( file, "int\n" );
      fprintf( file, "heuristicActionPreconditions_%s( register int *parameters )\n", action->name );
      fprintf( file, "{\n" );
    }
  fprintf( file, "  register int idx;\n" );
  fprintf( file, "  static int p[MAXPARAMETERS];\n\n" );


  fprintf( file, "  /* preconditions */\n" );
  for( prec = action->prec, label = 0; prec != NULL; prec = prec->next, ++label )
    if( prec->equal )
      {
#if 1 
	/* code for equality */
	for( arg = prec->args; arg != NULL; arg = arg->next )
	  if( arg->name[0] == '?' )
	    fprintf( file, "  if( parameters[%d] == 0 )  goto label%d;\n", arg->id, label );

	fprintf( file, "  if( " );
	for( arg = prec->args; arg != NULL; arg = arg->next )
	  {
	    if( arg->name[0] == '?' )
	      fprintf( file, "parameters[%d] ", arg->id );
	    else
	      fprintf( file, "%d ", arg->id );
	    
	    if( arg->next )
	      fprintf( file, "%c= ", (prec->neg ? '=' : '!') );
	  }
	fprintf( file, ") return( 0 );\n" );
	fprintf( file, " label%d:\n\n", label );
#endif
      }
    else if( !prec->neg )
      {
	i = 1;
	fprintf( file, "  p[0] = %d;  /* %s */ \n", prec->id, prec->func );
	for( arg = prec->args; arg != NULL; arg = arg->next, ++i )
	  {
	    if( arg->name[0] == '?' )
	      {
		fprintf( file, "  if( parameters[%d] == 0 )  goto label%d;  else", arg->id, label );
		fprintf( file, "  p[%d] = parameters[%d];  /* %s */\n", i, arg->id, arg->name );
	      }
	    else
	      fprintf( file, "  p[%d] = %d;  /* %s */\n", i, arg->id, arg->name );
	  }
	fprintf( file, "  p[%d] = -1;  /* end-marker */\n", i );
	fprintf( file, "  idx = readAtomHash( p )->idx;\n" );
	if( file == yfile )
	  fprintf( file, "  if( !sit[idx].lit ) return( 0 );\n" );
	if( file == xfile )
	  fprintf( file, "  if( oldCosts[idx].cost == INT_MAX ) return( 0 );\n" );
	fprintf( file, " label%d:\n\n", label );
      }
  else
    {
      fprintf( stderr, "WARNING: negative literal in precondition of %s\n", translate( action->name ) );
    }
  fprintf( file, "  return( 1 );\n" );
  fprintf( file, "}\n\n" );
}


void
emitAction( struct action_s *action, FILE *file )
{
  struct aformula_s *formula;
  struct idlist_s *arg;
  int i;


  /* function header */
  if( file == yfile )
    {
      fprintf( file, "int\n" );
      fprintf( file, "action_%s( register int *parameters, register struct literal_s *sit, "
	       "register struct literal_s *newSit )\n", action->name );
      fprintf( file, "{\n" );
      fprintf( file, "  register int idx;\n" );
    }
  if( file == xfile )
    {
      fprintf( file, "int\n" );
      fprintf( file, "heuristicAction_%s( register int *parameters )\n", action->name );
      fprintf( file, "{\n" );
      fprintf( file, "  register int idx, sum = 1, prec = 0, add = 0;\n" );
    }
  fprintf( file, "  static int p[MAXPARAMETERS];\n\n" );


  /* code for preconditions */
  fprintf( file, "  /* preconditions */\n" );
  if( file == yfile )
    {
      fprintf( file, "  if( !actionPreconditions_%s( parameters, sit ) ) return( 0 );\n\n", action->name );
#if 1
      /* operator prunning via relevant atoms */
      fprintf( file, "  /* relevantAtoms testing */\n" );
      for( formula = action->add; formula != NULL; formula = formula->next )
	{
	  i = 1;
	  fprintf( file, "  p[0] = %d;  /* %s */ \n", formula->id, formula->func );
	  for( arg = formula->args; arg != NULL; arg = arg->next, ++i )
	    {
	      if( arg->name[0] == '?' )
		fprintf( file, "  p[%d] = parameters[%d];  /* %s */\n", i, arg->id, arg->name );
	      else
		fprintf( file, "  p[%d] = %d;  /* %s */\n", i, arg->id, arg->name );
	    }
	  fprintf( file, "  p[%d] = -1;  /* end-marker */\n", i );
	  fprintf( file, "  if( relevantAtoms[readAtomHash( p )->idx] ) goto end;\n\n" );
	}
      fprintf( file, "  return( 0 );\n" );
      fprintf( file, " end:\n\n" );
#endif
    }
  if( file == xfile )
    {
      for( formula = action->prec; formula != NULL; formula = formula->next )
	if( formula->equal )
	  {
#if 1
	    /* code for equality */
	    fprintf( file, "  if( " );
	    for( arg = formula->args; arg != NULL; arg = arg->next )
	      {
		if( arg->name[0] == '?' )
		  fprintf( file, "parameters[%d] ", arg->id );
		else
		  fprintf( file, "%d ", arg->id );
		
		if( arg->next )
		  fprintf( file, "%c= ", (formula->neg ? '=' : '!') );
	      }
	    fprintf( file, ") return( 0 );\n\n" );
#endif
	  }
      else if( !formula->neg )
	  {
	    i = 1;
	    fprintf( file, "  p[0] = %d;  /* %s */\n", formula->id, formula->func );
	    for( arg = formula->args; arg != NULL; arg = arg->next, ++i )
	      {
		if( arg->name[0] == '?' )
		  fprintf( file, "  p[%d] = parameters[%d];  /* %s */\n", i, arg->id, arg->name );
		else
		  fprintf( file, "  p[%d] = %d;  /* %s */\n", i, arg->id, arg->name );
	      }
	    fprintf( file, "  p[%d] = -1;  /* end-marker */\n", i );
	    fprintf( file, "  idx = readAtomHash( p )->idx;\n" );
	    fprintf( file, "  preclist[prec] = idx;\n" );
	    fprintf( file, "  preconditions[prec++] = idx;\n" );
	    fprintf( file, "  assert( oldCosts[idx].cost != INT_MAX );\n" );
	    fprintf( file, "  sum += oldCosts[idx].cost;\n\n" );
	  }
      else
	{
	  fprintf( stderr, "WARNING: negative literal in precondition of %s\n", translate( action->name ) );
	}
      fprintf( file, "  memset( &preclist[prec], 0, (MAXPARAMETERS - prec) * sizeof( int ) );\n\n" );
    }

  
  /* code for add-list */
  fprintf( file, "  /* add-list */\n" );
  for( formula = action->add; formula != NULL; formula = formula->next )
    {
      i = 1;
      fprintf( file, "  p[0] = %d;  /* %s */\n", formula->id, formula->func );
      for( arg = formula->args; arg != NULL; arg = arg->next, ++i )
	{
	  if( arg->name[0] == '?' )
	    fprintf( file, "  p[%d] = parameters[%d];  /* %s */\n", i, arg->id, arg->name );
	  else
	    fprintf( file, "  p[%d] = %d;  /* %s */\n", i, arg->id, arg->name );
	}
      fprintf( file, "  p[%d] = -1;  /* end-marker */\n", i );
      fprintf( file, "  idx = readAtomHash( p )->idx;\n" );
      
      if( file == yfile )
	fprintf( file, "  newSit[idx].lit = 1;\n\n" );
      if( file == xfile )
	{
	  fprintf( file, "  costs[idx].cost = (sum < costs[idx].cost ? sum : costs[idx].cost);\n" );
	  fprintf( file, "  addlist[add++] = idx;\n\n" );
#if 1
	  /* prunning via goal cone */
	  fprintf( file, "  if( !initialized ) addParents( idx, preconditions );\n\n" );
#endif
	}
    }
  if( file == xfile )
    fprintf( file, "  memset( &addlist[add], 0, (MAXPARAMETERS - add) * sizeof( int ) );\n\n" );
  
  
  /* code for del-list: not for heuristic computation */
  if( file == yfile )
    {
      fprintf( file, "  /* del-list */\n" );
      for( formula = action->del; formula != NULL; formula = formula->next )
	{
	  i = 1;
	  fprintf( file, "  p[0] = %d;  /* %s */\n", formula->id, formula->func );
	  for( arg = formula->args; arg != NULL; arg = arg->next, ++i )
	    {
	      if( arg->name[0] == '?' )
		fprintf( file, "  p[%d] = parameters[%d];  /* %s */\n", i, arg->id, arg->name );
	      else
		fprintf( file, "  p[%d] = %d;  /* %s */\n", i, arg->id, arg->name );
	    }
	  fprintf( file, "  p[%d] = -1;  /* end-marker */\n", i );
	  fprintf( file, "  idx = readAtomHash( p )->idx;\n" );
	  fprintf( file, "  newSit[idx].lit = 0;\n\n" );
	}
    }
  
  
  /* function trailer */
  fprintf( file, "  return( 1 );\n" );
  fprintf( file, "}\n\n" );
}


void
emitActions( void )
{
  struct action_s *action;
  int i;

  i = 0;
  prunning = 1;
  for( action = actions; action != NULL; action = action->next )
    {
      action->id = i++;
      emitActionPreconditions( action, xfile );
      emitAction( action, xfile );
      emitActionPreconditions( action, yfile );
      emitAction( action, yfile );
    }
  numActions = i;
}


char *
translate( char *name )
{
  char *ip, *jp;
  static char buffer[1024];

  for( ip = name, jp = buffer; *ip != '\0'; ++ip, ++jp )
    *jp = (*ip == 'a' ? '-' : *ip);
  *jp = '\0';
  return( buffer );
}


void
emitNames( void )
{
  struct action_s *action;
  struct idlist_s *object;
  struct aformula_s *predicate;
  
  fprintf( yfile, "char *problemName = \"%s\";\n", problemName );
  fprintf( yfile, "char *actionName[] = { " );
  for( action = actions; action != NULL; action = action->next )
    fprintf( yfile, "\"%s\", ", translate( action->name ) );
  fprintf( yfile, "NULL };\n\n" );
  
  fprintf( yfile, "char *objectName[] = { " );
  for( object = objects; object != NULL; object = object->next )
    fprintf( yfile, "\"%s\", ", translate( object->name ) );
  fprintf( yfile, "NULL };\n\n" );
  
  fprintf( yfile, "char *predicateName[] = { " );
  for( predicate = predicates; predicate != NULL; predicate = predicate->next )
    fprintf( yfile, "\"%s\", ", translate( predicate->func ) );
  fprintf( yfile, "NULL };\n\n" );
}


void
identifyObjects( void )
{
  struct idlist_s *object;
  int i;

  i = 1;
  for( object = objects; object != NULL; object = object->next )
    object->id = i++;
  numObjects = i - 1;
}


void
tagFormulaList( struct aformula_s *formulaList )
{
  struct aformula_s *formula, *predicate;
  struct idlist_s *var, *object;

  for( formula = formulaList; formula != NULL; formula = formula->next )
    {
      for( predicate = predicates; predicate != NULL; predicate = predicate->next )
	if( !strcmp( predicate->func, formula->func ) )
	  {
	    formula->id = predicate->id;
	    break;
	  }
      if( predicate == NULL )
	{
	  fprintf( stderr, "tagFormulaList: no predicate for %s\n", translate( formula->func ) );
	  return;
	}
	
      for( var = formula->args; var != NULL; var = var->next )
	{
	  for( object = objects; object != NULL; object = object->next )
	    if( !strcmp( object->name, var->name ) )
	      {
		var->id = object->id;
		break;
	      }
	  if( object == NULL )
	    {
	      fprintf( stderr, "tagFormulaList: no object for %s\n", var->name );
	      return;
	    }
	}
    }
}


void
identifyTypes( void )
{
  struct aformula_s *predicate, *formula;
  struct action_s *action;

  /* Strips uses predicates for types. A sound way for its identification: those predicates that
     doesn't appear in add-lists or del-lists.
  */
  for( predicate = predicates; predicate != NULL; predicate = predicate->next )
    if( !predicate->equal )
      {
	for( action = actions; action != NULL; action = action->next )
	  {
	    for( formula = action->add; formula != NULL; formula = formula->next )
	      if( !strcmp( predicate->func, formula->func ) )
		break;
	    if( formula != NULL )
	      break;
	    
	    for( formula = action->del; formula != NULL; formula = formula->next )
	      if( !strcmp( predicate->func, formula->func ) )
		break;
	    if( formula != NULL )
	      break;
	  }
	
	if( (action == NULL) )
	  {
	    predicate->type = 1;
	    for( formula = initialSituation; formula != NULL; formula = formula->next )
	      if( predicate->id == formula->id )
		++predicate->type;
#if 0
	    fprintf( stderr, "(%s ...) is a type with dim = %d\n", predicate->func, predicate->type );
#endif
	  }
      }
}


void
mystery( void )
{
  struct idlist_s *arg, *before, *var, *type, *tmp;
  struct aformula_s *predicate, *pBefore, *prec;
  struct action_s *action;
  int change;

  /* sort predicates according to dimension */
  change = 1;
  while( change )
    {
      change = 0;
      pBefore = NULL;
      for( predicate = predicates; predicate->next != NULL; pBefore = predicate, predicate = predicate->next )
	if( predicate->next->type < predicate->type )
	  {
	    change = 1;
	    if( pBefore != NULL )
	      {
		pBefore->next = predicate->next;
		predicate->next = predicate->next->next;
		pBefore->next->next = predicate;
		predicate = pBefore->next;
	      }
	    else
	      {
		predicates = predicate->next;
		predicate->next = predicate->next->next;
		predicates->next = predicate;
		predicate = predicates;
	      }
	  }
    }
#if 0
  for( predicate = predicates; predicate != NULL; predicate = predicate->next )
    fprintf( stderr, "%s = %d\n", predicate->func, predicate->type );
#endif


  /* classify arguments of actions */
  for( action = actions; action != NULL; action = action->next )
    for( var = action->vars; var != NULL; var = var->next )
      for( prec = action->prec; prec != NULL; prec = prec->next )
	if( !prec->neg && !prec->equal )
	  {
	    for( arg = prec->args; arg != NULL; arg = arg->next )
	      if( arg->id == var->id )
		{
		  tmp = (struct idlist_s *) malloc( sizeof( struct idlist_s ) );
		  assert( tmp != NULL );
		  tmp->id = 0;
		  tmp->name = prec->func;
		  tmp->type = NULL;
		  tmp->next = var->type;
		  var->type = tmp;
		  
		  for( predicate = predicates; predicate != NULL; predicate = predicate->next )
		    if( !strcmp( predicate->func, tmp->name ) )
		      {
			tmp->id = (predicate->type > 0 ? predicate->type : 10000);
			break;
		      }
		  if( predicate == NULL )
		    fprintf( stderr, "mystery: no predicate for %s\n", translate( tmp->name ) );
		  break;
		}
	    if( arg == NULL )
	      {
		tmp = (struct idlist_s *) malloc( sizeof( struct idlist_s ) );
		assert( tmp != NULL );
		tmp->id = 10000;
		tmp->name = NULL;
		tmp->type = NULL;
		tmp->next = var->type;
		var->type = tmp;
	      }
	  }


  /* sort types for each action arguments */
  for( action = actions; action != NULL; action = action->next )
    for( var = action->vars; var != NULL; var = var->next )
      {
	change = 1;
	while( change )
	  {
	    change = 0;
	    before = NULL;
	    for( type = var->type; type->next != NULL; before = type, type = type->next )
	      if( type->next->id < type->id )
		{
		  change = 1;
		  if( before != NULL )
		    {
		      before->next = type->next;
		      type->next = type->next->next;
		      before->next->next = type;
		      type = before->next;
		    }
		  else
		    {
		      var->type = type->next;
		      type->next = type->next->next;
		      var->type->next = type;
		      type = var->type;
		    }
		}
	  }
      }
#if 0
  for( action = actions; action != NULL; action = action->next )
    for( var = action->vars; var != NULL; var = var->next )
      if( var->type->name != NULL )
	fprintf( stderr, "action %s: type of var %s is %s\n", translate( action->name ), var->name, var->type->name );
  fprintf( stderr, "\n" );
#endif


  /* sort action arguments according to dimension */
  for( action = actions; action != NULL; action = action->next )
    if( action->vars != NULL )
      {
	change = 1;
	while( change )
	  {
	    change = 0;
	    before = NULL;
	    for( var = action->vars; var->next != NULL; before = var, var = var->next )
	      if( var->next->type->id < var->type->id )
		{
		  change = 1;
		  if( before != NULL )
		    {
		      before->next = var->next;
		      var->next = var->next->next;
		      before->next->next = var;
		      var = before->next;
		    }
		  else
		    {
		      action->vars = var->next;
		      var->next = var->next->next;
		      action->vars->next = var;
		      var = action->vars;
		    }
		}
	  }
      }
#if 0
  for( action = actions; action != NULL; action = action->next )
    for( var = action->vars; var != NULL; var = var->next )
      if( var->type->name != NULL )
	fprintf( stderr, "action %s: type of var %s is %s\n", translate( action->name ), var->name, var->type->name );
#endif
}


struct idlist_s *
insertDomainInList( struct idlist_s *list, struct idlist_s *new )
{
  struct idlist_s *tmp;

  for( tmp = list; tmp != NULL; tmp = tmp->next )
    if( tmp->id == new->id )
      return( list );
  
  tmp = (struct idlist_s *) malloc( sizeof( struct idlist_s ) );
  assert( tmp != NULL );
  *tmp = *new;
  tmp->next = list;
  return( tmp );
}


void
identifyDomains( void )
{
  struct aformula_s *predicate, *formula;
  struct idlist_s *arg;

  for( predicate = predicates; predicate != NULL; predicate = predicate->next )
    if( !predicate->equal )
      {
	for( formula = initialSituation; formula != NULL; formula = formula->next )
	  if( predicate->id == formula->id ) 
	    for( arg = formula->args; arg != NULL; arg = arg->next )
	      predicate->domain = insertDomainInList( predicate->domain, arg );
      }

#if 0
  for( predicate = predicates; predicate != NULL; predicate = predicate->next )
    {
      fprintf( stderr, "\ndomain of %s[%d]: ", predicate->func, predicate->id );
      for( arg = predicate->domain; arg != NULL; arg = arg->next )
	fprintf( stderr, "%s[%d] ", arg->name, arg->id );
    }
  fprintf( stderr, "\n" );
#endif
}


void
idMatchArgsVars( struct aformula_s *formula, struct idlist_s *vars )
{
  struct aformula_s *form, *predicate;
  struct idlist_s *arg, *var, *object;

  /* identification */
  for( form = formula; form != NULL; form = form->next )
    {
      for( predicate = predicates; predicate != NULL; predicate = predicate->next )
	if( !strcmp( predicate->func, form->func ) )
	  {
	    form->id = predicate->id;
	    form->equal = predicate->equal;
	    break;
	  }
      if( predicate == NULL )
	fprintf( stderr, "idMatchArgsVars: no predicate for %s\n", translate( form->func ) );
			   
      for( arg = form->args; arg != NULL; arg = arg->next )
	if( arg->name[0] == '?' )
	  {
	    for( var = vars; var != NULL; var = var->next )
	      if( !strcmp( arg->name, var->name ) )
		{
		  arg->id = var->id;
		  break;
		}
	    if( var == NULL )
	      fprintf( stderr, "idMatchArgsVars: no variable for %s\n", arg->name );
	  }
	else
	  {
	    for( object = objects; object != NULL; object = object->next )
	      if( !strcmp( arg->name, object->name ) )
		{
		  arg->id = object->id;
		  break;
		}
	    if( object == NULL )
	      fprintf( stderr, "idMatchArgsVars: no object for %s\n", arg->name );
	  }
    }
}
