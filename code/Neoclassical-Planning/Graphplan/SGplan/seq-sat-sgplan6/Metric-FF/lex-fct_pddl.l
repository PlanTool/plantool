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

 * File: lex-fct_pddl.l

 * Description: modified from lex-fct_pddl.l in Metric-FF

 *

 * Authors:  Chih-wei Hsu and Benjamin W. Wah

 *

 *********************************************************************/
%{
#include "dis_ff.h"
#include "dis_parse.h"
  
  /* default yywrap function - always treat EOF as an EOF  */
int dis_fct_pddlwrap() {return 1;}

int dis_gbracket_count = 0;

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

"("  { return(OPEN_PAREN); }

")"  {  return(CLOSE_PAREN); }

\([ \t]*{i}{n}"-"{p}{a}{c}{k}{a}{g}{e}  {  dis_gbracket_count = 1;
 BEGIN OVERREAD; }

\([ \t]*":"{l}{e}{n}{g}{t}{h}  {  dis_gbracket_count = 1;
 BEGIN OVERREAD; }

\([ \t]*":"{r}{e}{q}{u}{i}{r}{e}{m}{e}{n}{t}{s}  {  dis_gbracket_count = 1;
 BEGIN OVERREAD; }

{d}{e}{f}{i}{n}{e}  {  return(DEFINE_TOK); }

{p}{r}{o}{b}{l}{e}{m}  {  return(PROBLEM_TOK); }

{s}{i}{t}{u}{a}{t}{i}{o}{n}  {  return(SITUATION_TOK); }

":"{s}{i}{t}{u}{a}{t}{i}{o}{n}  {  return(BSITUATION_TOK); }

":"{o}{b}{j}{e}{c}{t}{s}  {  return(OBJECTS_TOK); }

":"{g}{o}{a}{l}  {  return(GOAL_TOK); }

":"{m}{e}{t}{r}{i}{c}  {  return(METRIC_TOK); }

":"{i}{n}{i}{t}  {  return(INIT_TOK); }

":"{d}{o}{m}{a}{i}{n}  {  return(BDOMAIN_TOK); }

{p}{r}{e}{f}{e}{r}{e}{n}{c}{e}	{ return(PREFERENCE_TOK); }
{i}{s}"-"{v}{i}{o}{l}{a}{t}{e}{d}	{ return IS_VIOLATED_TOK; }

\([ \t]*":"{e}{x}{t}{e}{n}{d}{s}  {  dis_gbracket_count = 1;
 BEGIN OVERREAD; }

{a}{t}[ \t][ \t]*[0-9][0-9]*[.]?[0-9]*  { strcpy(yylval.string, yytext );return (AT_TOK); }

{a}{n}{d}  {  return(dis_AND_TOK); }

{i}{m}{p}{l}{y} {  return(IMPLY_TOK); }

{o}{r} {  return(dis_OR_TOK); }

{f}{o}{r}{a}{l}{l} {  return(Fdis_ORdis_ALL_TOK); }

{e}{x}{i}{s}{t}{s} {  return(dis_EXISTS_TOK); }

{n}{o}{t}  {  return(dis_NOT_TOK); }

"<"  {  return(LE_TOK); }

"<="  {  return(LEQ_TOK); }

"="  {  return(EQ_TOK); }

">="  {  return(GEQ_TOK); }

">"  {  return(GE_TOK); }

"-"  {  return(MINUS_TOK); }

"+"  {  return(AD_TOK); }

"*"  {  return(MU_TOK); }

"/"  {  return(DI_TOK); }

:?[a-zA-Z][a-zA-Z0-9\-_]* { dis_strupcase( yytext );  
  strcpy(yylval.string, yytext ); return(NAME); }

\?[a-zA-Z][a-zA-Z0-9\-_\[\]]* {dis_strupcase( yytext );
 strcpy(yylval.string, yytext); return(VARIABLE); }

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

<OVERREAD>\(  {  dis_gbracket_count++; }

<OVERREAD>\)  {  dis_gbracket_count--; 
  if (!dis_gbracket_count) BEGIN INITIAL; }

. {}
%%
