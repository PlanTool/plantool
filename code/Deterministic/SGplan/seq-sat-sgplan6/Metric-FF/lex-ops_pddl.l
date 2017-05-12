/*********************************************************************

 * (C) Copyright 2008, University of Illinois, Urbana-Champaign

 *

 * All rights reserved. Use of this software is permitted ONLY for

 * non-commercial research purposes, and it may be copied only

 * for that use only. All copies must include this copyright message.

 * This software is made available AS IS, and neither the authors

 * nor the University of Illinois, make any warranty about the

 * software or its performance.

 *

 *********************************************************************/
/********************************************************************

 * File: lex-ops_pddl.l 

 * Description: modified from lex-ops_pddl.l in Metric-FF

 *

 * Authors:  Chih-wei Hsu and Benjamin W. Wah

 *

 *********************************************************************/
%{
#include "dis_ff.h"
#include "dis_parse.h"

/* default yywrap function - always treat EOF as an EOF  */
int dis_ops_pddlwrap() {return 1;}

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

{d}{e}{f}{i}{n}{e}  {  return(DEFINE_TOK); }

{d}{o}{m}{a}{i}{n}  {  return(DOMAIN_TOK); }

":"{r}{e}{q}{u}{i}{r}{e}{m}{e}{n}{t}{s}  {  return(REQUIREMENTS_TOK); }

":"{t}{y}{p}{e}{s}  {  return(TYPES_TOK); }

":"{c}{o}{n}{s}{t}{a}{n}{t}{s}  {  return(CONSTANTS_TOK); }

":"{p}{r}{e}{d}{i}{c}{a}{t}{e}{s}  { return(PREDICATES_TOK); }

":"{f}{u}{n}{c}{t}{i}{o}{n}{s}  { return(FUNCTIONS_TOK); }

":"{a}{c}{t}{i}{o}{n}  {  return(ACTION_TOK); }

":"{p}{a}{r}{a}{m}{e}{t}{e}{r}{s}  {  return(PARAMETERS_TOK); }

":"{v}{a}{r}{s}  { return(VARS_TOK); }

":"{p}{r}{e}{c}{o}{n}{d}{i}{t}{i}{o}{n}  {  return(PRECONDITION_TOK); }

":"{c}{o}{n}{d}{i}{t}{i}{o}{n}	{ return(CONDITION_TOK); }

":"{e}{f}{f}{e}{c}{t}  {  return(EFFECT_TOK); }

":"{i}{m}{p}{l}{i}{e}{s}  {  return(IMPLIES_TOK); }

":"{d}{e}{r}{i}{v}{e}{d}	{ return(DERIVED_TOK); }

{a}{n}{d}  {  return(dis_AND_TOK); }

{n}{o}{t}  {  return(dis_NOT_TOK); }

{w}{h}{e}{n}  {  return(dis_WHEN_TOK); }

{i}{m}{p}{l}{y} {  return(IMPLY_TOK); }

{o}{r} {  return(dis_OR_TOK); }

{f}{o}{r}{a}{l}{l} {  return(Fdis_ORdis_ALL_TOK); }

{e}{x}{i}{s}{t}{s} {  return(dis_EXISTS_TOK); }

"<"  {  return(LE_TOK); }

"<="  {  return(LEQ_TOK); }

"="  {  return(EQ_TOK); }

">="  {  return(GEQ_TOK); }

">"  {  return(GE_TOK); }

"-"  {  return(MINUS_TOK); }

"+"  {  return(AD_TOK); }

"*"  {  return(MU_TOK); }

"/"  {  return(DI_TOK); }

"#"{t}  { return (TIME_TOK);}

{a}{s}{s}{i}{g}{n} { return(ASSIGN_TOK); }

{s}{c}{a}{l}{e}"-"{u}{p} { return(SCALE_UP_TOK); }

{s}{c}{a}{l}{e}"-"{d}{o}{w}{n} { return(SCALE_DOWN_TOK); }

{i}{n}{c}{r}{e}{a}{s}{e} { return(INCREASE_TOK); }

{d}{e}{c}{r}{e}{a}{s}{e} { return(DECREASE_TOK); }

":"{d}{u}{r}{a}{t}{i}{o}{n}   { return (DURATION_TOK); }

"?"{d}{u}{r}{a}{t}{i}{o}{n}     {  return(DURATION_VAR_TOK); }

":"{d}{u}{r}{a}{t}{i}{v}{e}"-"{a}{c}{t}{i}{o}{n}  { return (DURATIVE_ACTION_TOK); }

{a}{t}" "{s}{t}{a}{r}{t} {return AT_START;}

{a}{t}" "{e}{n}{d} {return AT_END;}

{o}{v}{e}{r}" "{a}{l}{l} {return OVER_ALL;}


:?[a-zA-Z][a-zA-Z0-9\-_]* { dis_strupcase(yytext); strcpy(yylval.string, yytext); 
 return(NAME); }

\?[a-zA-Z][a-zA-Z0-9\-_\[\]]* { dis_strupcase(yytext); strcpy(yylval.string, yytext); 
 return(VARIABLE); }

"-"?[0-9]*[.]?[0-9]* { strcpy(yylval.string, yytext); return(NUM);}


"-"[ \t]*"("[ \t]*{e}{i}{t}{h}{e}{r} { return(EITHER_TOK); }

\;(.)*\n  {  dis_lineno++; } 
\;(.)*  {  /* this will hold only in files that end with
		   a comment but no linefeed */ } 

<COMMENT>(.^\")*\n    {  dis_lineno++; }  ;

<INITIAL>\" { BEGIN COMMENT;}

<COMMENT>\" { BEGIN INITIAL;}

\n    {  dis_lineno++; } 

<OVERREAD>(.^\(\))*\n  {  dis_lineno++; }

<OVERREAD>[^\(\)]  {  }

<OVERREAD>\(  {  BEGIN OVERREAD; dis_gbracket_count++; }

<OVERREAD>\)  {  BEGIN OVERREAD; dis_gbracket_count--; 
  if (!dis_gbracket_count) BEGIN INITIAL; }

. {}
%%
