c   [a-zA-Z]
a   [a-zA-Z\_\-]
i   [0-9\-]
d   [0-9]

%{
#include <stdio.h>
#include <string.h>
#include <ctype.h>

#include <map>

#include "parser.h"
#include "tokens.h"

int lineno = 1;
int cpos = 1;
extern YYSTYPE_T yylval;
extern std::map<const char*,formula_t*,ltstr> defined;
static int yyinput( void );

void
skipComments (void)  
{
  int c;

  c = yyinput();
  while( (c != '\n') && (c != EOF) ) 
    c = yyinput();
  ++lineno;
}
%}

%%

"\n"                        { lineno++; cpos=1; }
"\r"                        { cpos=1; }
"\t"                        { cpos+=8; }
" "                         { cpos++; }
";"                         { skipComments(); }

DOMAIN                      { cpos += yyleng; return( DOMAIN ); }
DEFINE                      { cpos += yyleng; return( DEFINE ); }
:TYPING                     { cpos += yyleng; return( TYPING ); }
:EQUALITY                   { cpos += yyleng; return( EQUALITY ); }
:CONDITIONAL\-EFFECTS       { cpos += yyleng; return( CONDITIONAL_EFFECTS ); }
:NEGATIVE\-PRECONDITIONS    { cpos += yyleng; return( NEGATIVE_PRECONDITIONS ); } 

:REQUIREMENTS               { cpos += yyleng; return( REQUIREMENTS ); }
:CONSTANTS                  { cpos += yyleng; return( CONSTANTS ); }
:TYPES                      { cpos += yyleng; return( TYPES ); }
:STRIPS                     { cpos += yyleng; return( STRIPS ); }
EITHER                      { cpos += yyleng; return( EITHER ); }
:PREDICATES                 { cpos += yyleng; return( PREDICATES ); }

OR                          { cpos += yyleng; return( OR ); }
AND                         { cpos += yyleng; return( AND ); }
FORALL                      { cpos += yyleng; return( FORALL ); }
NOT                         { cpos += yyleng; return( NOT ); }
WHEN                        { cpos += yyleng; return( WHEN ); }

:ACTION                     { cpos += yyleng; return( ACTION ); }
:OBSERVE                    { cpos += yyleng; return( OBSERVE ); }
:PARAMETERS                 { cpos += yyleng; return( PARAMETERS ); }
:PRECONDITION               { cpos += yyleng; return( PRECONDITION ); }
:EFFECT                     { cpos += yyleng; return( EFFECT ); }
KNOW                        { cpos += yyleng; return( KNOW ); }
UNKNOWN                     { cpos += yyleng; return( UNKNOWN ); }
ONEOF                       { cpos += yyleng; return( ONEOF ); }
:CERTAINTY                  { cpos += yyleng; return( CERTAINTY ); }
:SET                        { cpos += yyleng; return( SET ); }

:FORMULA                    { cpos += yyleng; return( FORMULA ); }
:DEFINED                    { cpos += yyleng; return( DEFINED ); }

PROBLEM                     { cpos += yyleng; return( PROBLEM ); }
:SITUATION                  { cpos += yyleng; return( SITUATION ); }
:OBJECTS                    { cpos += yyleng; return( OBJECTS ); }
:STRUCTURAL                 { cpos += yyleng; return( STRUCTURAL ); }
:INIT                       { cpos += yyleng; return( INIT ); }
:GOAL                       { cpos += yyleng; return( GOAL ); }

\(                          { cpos += yyleng; return( LEFTPAR ); }
\)                          { cpos += yyleng; return( RIGHTPAR ); }
\?                          { cpos += yyleng; return( QUESTION ); }
\=                          { cpos += yyleng; return( EQUAL ); }
\:                          { cpos += yyleng; return( TWODOTS ); }
\-                          { cpos += yyleng; return( HYPHEN ); }

{i}({d})*\.({d})*           { cpos += yyleng; yylval.real = atof( yytext ); return( FLOAT );}
{i}({d})*                   { cpos += yyleng; yylval.integer = atoi( yytext ); return( INTEGER );}
{c}({a}|{i})*               { cpos += yyleng; yylval.ident = strdup( yytext );
                              for( char *p = yylval.ident; *p != 0; ++p ) *p = toupper(*p);
			      if( defined.find( yylval.ident ) != defined.end() )
				return( DEF_NAME );
			      else
				return( NAME );
                            }
.                           { cpos++; return( yytext[0] ); }

%%

int
yywrap( void )
{
  return( 1 );
}
