%{
  /******************************************************/
  /* Strict Strips PDDL parser for BlackBox 		*/
  /******************************************************/

#include "graphplan.h"
/* #include "control.h" */

int yylex (void);
int yyerror(char * s);

extern char* yytext;
extern int yylineno;
extern int max_time;
extern char *domain_name, *problem_name;
extern op_list ops;
extern fact_list initial_facts, the_goals, constants, predicates, the_types;
extern int input_type;
extern token_list objects;

/* extern char* yytext;
 * extern char* yytext;
 * extern int yylineno;
 * extern int max_time;
 * extern char *domain_name, *problem_name;
 * extern op_list ops;
 * extern defpred_list dps;
 * extern fact_list initial_facts, the_goals, constants, predicates, the_types;
 * extern control_list controls;
 * extern int input_type;
 * extern token_list objects;
 */
op_list make_action (char *, param_list, precond_list, effect_list);

/* defpred_list make_defpred (char *, param_list, fact_list);
 * control_list make_control (char *, fact_list);
 */
void check_requirement_flag (token_list flags);
void type_object (fact_list, token_list, int);
void add_object (fact_list object);
void add_action (op_list op);
/* void add_defpred (defpred_list);
 * void add_control (control_list);
 */

%}

%union {
  char *str;
  op_list oplist;
  fact_list flist;
  token_list tlist;
}

%token DEFINE
%token DOMAIN REQUIREMENTS CONSTANTS TYPES PREDICATES 
%token PROBLEM OBJECTS INIT LENGTH PARALLEL SERIAL
%token ACTION PARAMETERS PRECONDITION EFFECT
%token AND 
%token EXISTS
%token EQ 
%token NOT GOAL
%token EITHER FLUENT
%token ID VAR 
%token SET TEST INFLUENCE 

%%

pddl : domain
       | problem
;

domain : '(' DEFINE '(' DOMAIN id ')' domain_body ')' { 
         domain_name = $<str>5; }

domain_body: domain_structure
       | domain_body domain_structure   
;

domain_structure :  require_def
       | '(' ':' CONSTANTS constlist ')' { constants = $<flist>4; }
       | '(' ':' TYPES typedlist ')' {
	 completely_free_fact_list($<flist>4); }
       | '(' ':' PREDICATES predlist ')' { predicates = $<flist>4; }
       | action { add_action($<oplist>1); }

require_def : '(' ':' REQUIREMENTS require_key ')' {
       check_requirement_flag($<tlist>4); free_token_list($<tlist>4); }

problem : '(' DEFINE '(' PROBLEM id ')' '(' ':' DOMAIN id ')' problem_body ')' 
       { problem_name = $<str>5; 
         if (strcmp(domain_name, $<str>10) != 0)
	   do_error("domain doesn't match");
	 free($<str>10);
       }

problem_body : problem_structure
       | problem_body problem_structure ;

problem_structure : require_def 
       | '(' ':' OBJECTS typedlist ')' { the_types = $<flist>4; }
       | '(' ':' INIT factlist ')' { initial_facts = $<flist>4; }
       | '(' ':' GOAL conditions ')' { the_goals = $<flist>4; }
       | '(' ':' LENGTH length_spec ')' ;

length_spec : length
       | length length_spec ;

length : '(' ':' SERIAL id ')' { free($<str>4); }
       | '(' ':' PARALLEL id ')' { max_time = atoi($<str>4);
	 free($<str>4);
       }

action : '(' ':' ACTION id parameter precondition effect ')' { 
           $<oplist>$ = make_action($<str>4, $<flist>5, $<flist>6, $<flist>7);
         }

parameter : ':' PARAMETERS parameters { $<flist>$ = $<flist>3; }

precondition : ':' PRECONDITION conditions { $<flist>$ = $<flist>3; }
effect : ':' EFFECT conditions { $<flist>$ = $<flist>3; } 

parameters : '(' ')' { $<flist>$ = NULL; }
       | '(' typedparamlist ')' { $<flist>$ = $<flist>2; }
conditions : '(' AND factlist ')' { $<flist>$ = $<flist>3; }
           | factlist { $<flist>$ = $<flist>1; }

constlist : idlist type_def { $<flist>$ = $<flist>1; add_object($<flist>1);
            type_object ($<flist>1, $<tlist>2, 0); }
        | idlist type_def constlist { $<flist>$ = $<flist>1;
	  add_object($<flist>1);
	  type_object ($<flist>1, $<tlist>2, 0);
	  fact_list_append($<flist>$, $<flist>3);
	}

typedlist : idlist { $<flist>$ = $<flist>1; add_object($<flist>1); }
        | idlist type_def { $<flist>$ = $<flist>1; add_object($<flist>1);
          type_object ($<flist>1, $<tlist>2, 1); }
        | idlist type_def typedlist { $<flist>$ = $<flist>1;
	  add_object($<flist>1);
	  type_object ($<flist>1, $<tlist>2, 1);
	  fact_list_append($<flist>$, $<flist>3);
	}

typedparamlist : paramlist { $<flist>$ = $<flist>1; }
       | paramlist type_def { $<flist>$ = $<flist>1;
         type_object ($<flist>1, $<tlist>2, 0); }
       | paramlist type_def typedparamlist { $<flist>$ = $<flist>1;
         type_object ($<flist>1, $<tlist>2, 0);
         fact_list_append($<flist>$, $<flist>3);
       }

type_def : '-' type  { $<tlist>$ = $<tlist>2; }

type : id { $<tlist>$ = str2token($<str>1); }
       | '(' EITHER types ')' { $<tlist>$ = $<tlist>3; }


types :  id { $<tlist>$ = str2token($<str>1); }
       | id types { $<tlist>$ = str2token($<str>1);
	  $<tlist>$->next = $<tlist>2;
	} 

idlist : id { $<flist>$ = token2fact(str2token($<str>1)); }
       | id idlist { $<flist>$ = token2fact(str2token($<str>1));
           $<flist>$->next = $<flist>2;
         }

paramlist : variable { $<flist>$ = token2fact(str2token($<str>1)); }
       | variable paramlist { $<flist>$ = token2fact(str2token($<str>1));
	 $<flist>$->next = $<flist>2;
       }

predlist : predicate { $<flist>$ = $<flist>1; }
       | predicate predlist { $<flist>$ = $<flist>1;
	 fact_list_append($<flist>$, $<flist>2); 
       }

predicate : '(' id ')' { $<flist>$ = token2fact(str2token($<str>2)); }
        | '(' id typedparamlist ')' { 
          $<flist>$ = token2fact(str2token($<str>2));
	  $<flist>$->body = $<flist>3;
        }

factlist : term { $<flist>$ = token2fact($<tlist>1); }
         | term factlist { $<flist>$ = token2fact($<tlist>1);
	   $<flist>$->next = $<flist>2;
	 }
         | exists { $<flist>$ = $<flist>1; }

exists :  '(' EXISTS parameters term factlist ')' {
            token_list t;
	    t = strdup2token("exists");
	    t->next = $<tlist>4;
	    $<flist>$ = token2fact(t);
	    $<flist>$->next = $<flist>3;
	    $<flist>$->body = $<flist>5;
          } 
 
term : '(' tokenlist ')' { $<tlist>$ = $<tlist>2; }
         | '(' NOT term ')' {
            $<tlist>$ = strdup2token(DELETE);
	    $<tlist>$->next = $<tlist>3;
	 }
	 | '(' GOAL term ')' {
            $<tlist>$ = strdup2token("goal");
	    $<tlist>$->next = $<tlist>3;
	 }

require_key : ':' id { $<tlist>$ = str2token($<str>2); }
       | ':' id require_key { $<tlist>$ = str2token($<str>2);
         $<tlist>$->next = $<tlist>3; }

tokenlist : variable { $<tlist>$ = str2token($<str>1); }
         | id { $<tlist>$ = str2token($<str>1); }
         | variable tokenlist { $<tlist>$ = str2token($<str>1); 
	   $<tlist>$->next = $<tlist>2; }
         | id tokenlist { $<tlist>$ = str2token($<str>1); 
	   $<tlist>$->next = $<tlist>2; }

variable : VAR { $<str>$ = bbstrdup(yytext); }

id : ID { $<str>$ = bbstrdup(yytext); }

%%

/* check only allowed requirements are used */
void check_requirement_flag (token_list flags)
{
  char s[50];

  for (; flags; flags = flags->next) {
    if (strcmp("strips", flags->item) != 0 &&
	strcmp("typing", flags->item) != 0 &&
	strcmp("equality", flags->item) != 0) {
      sprintf(s, "requirement: %s is not supported\n", flags->item);
      do_error (s);
    } 
  }
}

/* form an action(operator) */
op_list make_action (char *name, param_list params, 
		     precond_list preconds, effect_list effects)
{
  op_list op;
  op = (op_list)malloc(sizeof(op_s));
  op->name = name;
  op->params = params;
  op->preconds = preconds;
  op->effects = effects;
  set_insts(op);
  op->next = NULL;
  return op;
}

/* make a defpredicate */
/* defpred_list make_defpred (char *name, param_list params, fact_list formula)
 * {
 *   defpred_list dp;
 *   dp = (defpred_list)malloc(sizeof(defpred_s));
 *   dp->name = name;
 *   dp->params = params;
 *   dp->formula = formula;
 *   dp->next = NULL;
 *   return dp;
 * }
 */

/* control_list make_control (char *name, fact_list excludes)
 * {
 *   control_list ctrl;
 *   
 *   ctrl = (control_list)malloc(sizeof(control_s));
 *   ctrl->name = name;
 *   ctrl->excludes = excludes;
 *   ctrl->next = NULL;
 *   return ctrl;
 * }
 */

/* add objects */
void add_object (fact_list object)
{	
  token_list prev;

  if (input_type == PROBLEM_INPUT) {
    if (objects == 0) {
      prev = objects = strdup2token(object->item->item);
      object = object->next;
    }
    else {
      prev = objects;
      while (prev->next) prev = prev->next;
    }	 
    for (; object; object = object->next) {
      prev = prev->next = strdup2token(object->item->item);
    }
  }
}

/* type object */
void type_object (fact_list objects, token_list types, int retain)
{
  char *oname;
  token_list t;
  fact_list obj, next;

  for (obj = objects; obj; obj = next) {
    next = obj->next;
    oname = obj->item->item;
    if (retain) {	   	/* retain the original *no* type object */
      obj = obj->next = token2fact(strdup2token(oname));      
    }
    obj->item->next = strdup2token(types->item);
    for (t = types->next; t; t = t->next) {
      obj = obj->next = token2fact(strdup2token(oname));
      obj->item->next = strdup2token(t->item);
    }
    obj->next = next;
  }
}

void add_action (op_list op)
{
  op_list prev;

  if (ops == NULL) 
    ops = op;
  else {
    prev = ops;
    while (prev->next) prev = prev->next;
    prev->next = op;
  }
}

/* void add_defpred (defpred_list dp)
 * {
 *   if (dps == NULL) 
 *     dps = dp;
 *   else {
 *     dp->next = dps;
 *     dps = dp;
 *   } 
 * }
 * 
 * void add_control (control_list ctrl)
 * {
 *   control_list prev;
 * 
 *   if (controls == NULL) 
 *     controls = ctrl;
 *   else {
 *     prev = controls;
 *     while (prev->next) 
 *       prev = prev->next;
 *     prev->next = ctrl;
 *   } 
 * }
 */

int yyerror(char * s)
{
  fprintf(stderr,"%s\n",s);
  fprintf(stderr,"Error occurred at or near line %i\n", yylineno);
  exit(1);
}
