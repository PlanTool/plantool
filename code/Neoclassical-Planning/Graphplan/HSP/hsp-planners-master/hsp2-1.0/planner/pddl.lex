c   [a-zA-Z]
a   [a-zA-Z\_\-]
i   [0-9\-]
d   [0-9]

%{
/*
**
** Universidad Simon Bolivar, 1999, 2000 (c)
** Blai Bonet and Hector Geffner. 1999, 2000.
**
*/

#include <stdio.h>
#include <string.h>
#include <ctype.h>

#include "parser.h"
#include "tokens.h"


int lineno = 1;
int cpos = 1;
extern YYSTYPE_T yylval;
static int input();


void
skipComments (void)  
{
  int c;

  c = input();
  while( (c != '\n') && (c != EOF) )
    c = input();
  ++lineno;
}
%}


%%


"\n"                        { lineno++; cpos=1; }
"\t"                        { cpos+=8; }
" "                         { cpos++; }
";"                         { skipComments(); }

DOMAIN                      { cpos += yyleng; return( DOMAIN ); }
DEFINE                      { cpos += yyleng; return( DEFINE ); }
:STRIPS                     { cpos += yyleng; return( STRIPS ); }
:TYPING                     { cpos += yyleng; return( TYPING ); }

:EQUALITY                   { cpos += yyleng; return( EQUALITY ); }
:CONDITIONAL-EFFECTS        { cpos += yyleng; return( CONDITIONAL ); }
:REQUIREMENTS               { cpos += yyleng; return( REQUIREMENTS ); }
:CONSTANTS                  { cpos += yyleng; return( CONSTANTS ); }
:TYPES                      { cpos += yyleng; return( TYPES ); }
EITHER                      { cpos += yyleng; return( EITHER ); }
:PREDICATES                 { cpos += yyleng; return( PREDICATES ); }

:DOMAIN-AXIOMS              { cpos += yyleng; return( DOM_AXIOMS ); }
:DISJUNCTIVE-PRECONDITIONS  { cpos += yyleng; return( DISJUNCTIVE ); }
:ADL                        { cpos += yyleng; return( ADL ); }
:EXISTENTIAL-PRECONDITIONS  { cpos += yyleng; return( E_PRECONDITIONS ); }
:UNIVERSAL-PRECONDITIONS    { cpos += yyleng; return( U_PRECONDITIONS ); }

AND                         { cpos += yyleng; return( AND ); }
FORALL                      { cpos += yyleng; return( FORALL ); }
NOT                         { cpos += yyleng; return( NOT ); }
WHEN                        { cpos += yyleng; return( WHEN ); }

:ACTION                     { cpos += yyleng; return( ACTION ); }
:PARAMETERS                 { cpos += yyleng; return( PARAMETERS ); }
:PRECONDITION               { cpos += yyleng; return( PRECONDITION ); }
:EFFECT                     { cpos += yyleng; return( EFFECT ); }

PROBLEM                     { cpos += yyleng; return( PROBLEM ); }
:SITUATION                  { cpos += yyleng; return( SITUATION ); }
:OBJECTS                    { cpos += yyleng; return( OBJECTS ); }
:INIT                       { cpos += yyleng; return( INIT ); }
:GOAL                       { cpos += yyleng; return( GOAL ); }
:LENGTH                     { cpos += yyleng; return( LENGTH ); }
:SERIAL                     { cpos += yyleng; return( SERIAL ); }
:PARALLEL                   { cpos += yyleng; return( PARALLEL ); }

\(                          { cpos += yyleng; return( LEFTPAR ); }
\)                          { cpos += yyleng; return( RIGHTPAR ); }
\?                          { cpos += yyleng; return( QUESTION ); }
\=                          { cpos += yyleng; return( EQUAL ); }
\:                          { cpos += yyleng; return( TWODOTS ); }
\-                          { cpos += yyleng; return( HYPHEN ); }

{i}({d})*                   { cpos += yyleng; yylval.integer = atoi( yytext ); return( INTEGER );}
{c}({a}|{i})*               { register char *p; cpos += yyleng; yylval.ident = strdup( yytext ); 
                              for( p = yylval.ident; *p != 0; p++ ) *p = (*p == '-' ? 'a' : toupper( *p )); return( NAME ); }
.                           { cpos++; return( yytext[0] ); }


%%


int
yywrap( void )
{
  return( 1 );
}
