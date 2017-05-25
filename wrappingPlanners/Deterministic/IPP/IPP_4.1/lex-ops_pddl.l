%{
#include "ipp.h"
#include "utilities.h"

/* default yywrap function - always treat EOF as an EOF  */

int ops_pddlwrap() {return 1;}

%}

a [Aa]
b [Bb]
c [Cc]
d [Dd]
e [Ee]
f [Ff]
g [Gg]
h [Hh]
i [Ii]
j [Jj]
k [Kk]
l [Ll]
m [Mm]
n [Nn]
o [Oo]
p [Pp]
q [Qq]
r [Rr]
s [Ss]
t [Tt]
u [Uu]
v [Vv]
w [Ww]
x [Xx]
y [Yy]
z [Zz]

%x COMMENT OVERREAD

%%

"("  {  return(OPEN_PAREN); }

")"  {  return(CLOSE_PAREN); }

"-"  {  return('-'); }

"("[ \t]*{i}{n}"-"{p}{a}{c}{k}{a}{g}{e}  {  gbracket_count = 1;
 BEGIN OVERREAD; }

{d}{e}{f}{i}{n}{e}  {  return(DEFINE_TOK); }

{d}{o}{m}{a}{i}{n}  {  return(DOMAIN_TOK); }

"("[ \t]*":"{e}{x}{t}{e}{n}{d}{s}  {  gbracket_count = 1;
 BEGIN OVERREAD; }

":"{r}{e}{q}{u}{i}{r}{e}{m}{e}{n}{t}{s}  {  return(REQUIREMENTS_TOK); }

":"{t}{y}{p}{e}{s}  {  return(TYPES_TOK); }

":"{c}{o}{n}{s}{t}{a}{n}{t}{s}  {  return(CONSTANTS_TOK); }

"("[ \t]*":"{d}{o}{m}{a}{i}{n}"-"{v}{a}{r}{i}{a}{b}{l}{e}{s}  {  gbracket_count = 1;
 BEGIN OVERREAD; } 

":"{p}{r}{e}{d}{i}{c}{a}{t}{e}{s}  { return(PREDICATES_TOK); }

"("[ \t]*":"{t}{i}{m}{e}{l}{e}{s}{s} {  gbracket_count = 1;
 BEGIN OVERREAD; }

":"{a}{c}{t}{i}{o}{n}  {  return(ACTION_TOK); }

":"{a}{x}{i}{o}{m}  {  return(AXIOM_TOK); }

":"{p}{a}{r}{a}{m}{e}{t}{e}{r}{s}  {  return(PARAMETERS_TOK); }

":"{v}{a}{r}{s}  { return(VARS_TOK); }

":"{p}{r}{e}{c}{o}{n}{d}{i}{t}{i}{o}{n}  {  return(PRECONDITION_TOK); }

":"{c}{o}{n}{t}{e}{x}{t}  { return(CONTEXT_TOK); }

":"{e}{f}{f}{e}{c}{t}  {  return(EFFECT_TOK); }

":"{i}{m}{p}{l}{i}{e}{s}  {  return(IMPLIES_TOK); }

{a}{n}{d}  {  return(AND_TOK); }

{n}{o}{t}  {  return(NOT_TOK); }

{w}{h}{e}{n}  {  return(WHEN_TOK); }

{i}{m}{p}{l}{y} {  return(IMPLY_TOK); }

{o}{r} {  return(OR_TOK); }

{f}{o}{r}{a}{l}{l} {  return(FORALL_TOK); }

{e}{x}{i}{s}{t}{s} {  return(EXISTS_TOK); }

"="  {  return(EQUAL_TOK); }

:?[a-zA-Z][a-zA-Z0-9\-_]* { strupcase(yytext); strcpy(yylval.string, yytext); 
 return(NAME); }

\?[a-zA-Z][a-zA-Z0-9\-_\[\]]* { strupcase(yytext); strcpy(yylval.string, yytext); 
 return(VARIABLE); }

"-"[ \t]*[a-zA-Z][a-zA-Z0-9\-_\[\]]*  {
  strupcase(yytext); strcpy(yylval.string, rmdash(yytext)); return(TYPE); }

"-"[ \t]*"("[ \t]*{e}{i}{t}{h}{e}{r} { return(EITHER_TOK); }

\;(.)*\n  {  lineno++; } 
\;(.)*  {  /* this will hold only in files that end with
		   a comment but no linefeed */ } 

<COMMENT>(.^\")*\n    {  lineno++; }  ;

<INITIAL>\" { BEGIN COMMENT;}

<COMMENT>\" { BEGIN INITIAL;}

\n    {  lineno++; } 

<OVERREAD>(.^\(\))*\n  {  lineno++; }

<OVERREAD>[^\(\)]  {  }

<OVERREAD>\(  {  BEGIN OVERREAD; gbracket_count++; }

<OVERREAD>\)  {  BEGIN OVERREAD; gbracket_count--; 
  if (!gbracket_count) BEGIN INITIAL; }

. {}
%%
