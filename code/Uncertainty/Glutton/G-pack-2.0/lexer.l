%{
#include "effects.h"
#include "formulas.h"
#include "terms.h"
#include "types.h"
#include "math.h"
#include <cctype>
#include <string>

struct Expression;
struct Application;

#include "parser.h"

size_t line_number;
static int make_string( char* s, int token );
static int make_number( const char* s );
%}

%option case-insensitive never-interactive nounput noyywrap

IDENT	[A-Za-z]([A-Za-z0-9\-_])*

%s COMM
%%

define				return( make_string( yytext, DEFINE ) );
domain				return( make_string( yytext, DOMAIN_TOKEN ) );
problem				return( make_string( yytext, PROBLEM ) );
:requirements			return( REQUIREMENTS );
:types				return( TYPES );
:constants			return( CONSTANTS );
:predicates			return( PREDICATES );
:functions			return( FUNCTIONS );
:strips				return( STRIPS );
:typing				return( TYPING );
:negative-preconditions		return( NEGATIVE_PRECONDITIONS );
:disjunctive-preconditions	return( DISJUNCTIVE_PRECONDITIONS );
:equality			return( EQUALITY );
:existential-preconditions	return( EXISTENTIAL_PRECONDITIONS );
:universal-preconditions	return( UNIVERSAL_PRECONDITIONS );
:quantified-preconditions	return( QUANTIFIED_PRECONDITIONS );
:conditional-effects		return( CONDITIONAL_EFFECTS );
:fluents			return( FLUENTS );
:adl				return( ADL );
:durative-actions		return( DURATIVE_ACTIONS );
:duration-inequalities		return( DURATION_INEQUALITIES );
:continuous-effects		return( CONTINUOUS_EFFECTS );
:probabilistic-effects		return( PROBABILISTIC_EFFECTS );
:rewards			return( REWARDS );
:mdp				return( MDP );
:action				return( ACTION );
:parameters			return( PARAMETERS );
:precondition			return( PRECONDITION );
:effect				return( EFFECT );
:domain				return( PDOMAIN );
:objects			return( OBJECTS );
:init				return( INIT );
:goal				return( GOAL );
;+[ \r\t]*                      BEGIN COMM;   
<COMM>\(:horizon                BEGIN 0; return( HORIZON ); 
<COMM>([^\(]|\([^:]|\(:[^h]|\(:h[^o]|\(:ho[^r]|\(:hor[^i]|\(:hori[^z]|\(:horiz[^o]|\(:horizo[^n]).*$            BEGIN 0;
:goal-reward                    return( GOAL_REWARD );
:metric				return( METRIC );
:orbit                          return( ORBIT );
:system                         return( SYSTEM );
goal-probability		return( make_string( yytext, GOAL_PROBABILITY) );
expected-cost   		return( make_string( yytext, EXPECTED_COST) );
number				return( make_string( yytext, NUMBER_TOKEN) );
object				return( make_string( yytext, OBJECT_TOKEN) );
either				return( make_string( yytext, EITHER) );
when				return( make_string( yytext, WHEN) );
not				return( make_string( yytext, NOT) );
and				return( make_string( yytext, AND) );
or				return( make_string( yytext, OR) );
imply				return( make_string( yytext, IMPLY) );
exists				return( make_string( yytext, EXISTS) );
forall				return( make_string( yytext, FORALL) );
probabilistic			return( make_string( yytext, PROBABILISTIC) );
assign				return( make_string( yytext, ASSIGN) );
scale-up			return( make_string( yytext, SCALE_UP) );
scale-down			return( make_string( yytext, SCALE_DOWN) );
increase			return( make_string( yytext, INCREASE) );
decrease			return( make_string( yytext, DECREASE) );
minimize			return( make_string( yytext, MINIMIZE) );
maximize			return( make_string( yytext, MAXIMIZE) );
{IDENT}				return( make_string( yytext, NAME) );
\?{IDENT}			return( make_string( yytext, VARIABLE) );
[0-9]*[./]?[0-9]+		return( make_number( yytext) );
\<                              return( LT );
\<=				return( LE );
>=				return( GE );
>                               return( GT );
\(                              return( LEFTPAR );
\)                              return( RIGHTPAR );
\{                              return( LEFTBRACE );
\}                              return( RIGHTBRACE );
\+                              return( PLUS );
\-                              return( HYPHEN );
\*                              return( STAR );
\/                              return( SLASH );
=                               return( EQUAL );
[ \t\r]+			/* whitespace */
\n				++line_number;
.				return( ILLEGAL_TOKEN );

%%

static int
make_string( char* s, int token )
{
  yylval.str = new std::string( s );
  return( token );
}

static int
make_number( const char* s )
{ 
  int numerator_ = 0;
  int denominator_ = 1;
  const char* si = s;
  for( ; *si != '\0' && *si != '.' && *si != '/'; ++si )
    numerator_ = 10 * numerator_ + (*si - '0');

  if( *si == '/' )
    {
      denominator_ = 0;
      for( ++si; *si != '\0'; ++si )
	denominator_ = 10*denominator_ + (*si - '0');
      if( denominator_ == 0 )
      {
        std::cout<<"ERROR: division by 0"<<std::endl;
	exit(1);
      }
      yylval.num = new double(((double)numerator_) / ((double)denominator_));
    }
  else if( *si == '.' )
    {
      int a = numerator_;
      double fr = 0;
      int j = 1;
      for( ++si; *si != '\0'; ++si, j++ )
	{
	  fr += ((*si - '0') / (pow(10, j)));
	}
      yylval.num = new double(a + fr);
    }
  else
    {
      yylval.num = new double(numerator_);
    }
  return( NUMBER );
}
