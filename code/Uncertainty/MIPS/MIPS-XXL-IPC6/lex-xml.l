

/*********************************************************************
 * (C) Copyright 2002 Albert Ludwigs University Freiburg
 *     Institute of Computer Science
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 * 
 *********************************************************************/

%{
#include "ff.h"
#include "parse.h"
  
  /* default yywrap function - always treat EOF as an EOF  */
int xmlwrap() {return 1;}



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

"<"\?{x}{m}{l}" "{v}{e}{r}{s}{i}{o}{n}"="\"[1]"."[0]\"" "{e}{n}{c}{o}{d}{i}{n}{g}"="\"{i}{s}{o}\-[8][8][5][9]\-[1]\"\?">"         {  return(PROLOG1_TAG); }  
"<"\!{d}{o}{c}{t}{y}{p}{e}" "{d}{u}{r}{a}{t}{i}{v}{e}"-"{f}{f}"-"{d}{e}{p}{e}{n}{d}" "{s}{y}{s}{t}{e}{m}" "\"{d}{e}{p}{e}{n}{d}"."{d}{t}{d}\"">"  {  return(PROLOG2_TAG); }
"<"{d}{u}{r}{a}{t}{i}{v}{e}"-"{f}{f}"-"{d}{e}{p}{e}{n}{d}">"   {  return(OPEN_DURATIVE_FF_DEPEND_TAG); }
"<""/"{d}{u}{r}{a}{t}{i}{v}{e}"-"{f}{f}"-"{d}{e}{p}{e}{n}{d}">"   {  return(CLOSE_DURATIVE_FF_DEPEND_TAG); }
"<"{o}{p}{e}{r}{a}{t}{o}{r}">"  {  return(OPEN_OPERATOR_TAG); }
"<""/"{o}{p}{e}{r}{a}{t}{o}{r}">"  {  return(CLOSE_OPERATOR_TAG); }
"<"{n}{a}{m}{e}">"   {  return(OPEN_NAME_TAG); }
"<""/"{n}{a}{m}{e}">"  {  return(CLOSE_NAME_TAG); }
:?[a-zA-Z][a-zA-Z0-9\-_]* { strupcase(yytext); strcpy(yylval.string, yytext);  return(NAME); }
"<"{d}{e}{p}{e}{n}{d}">"  {  return(OPEN_DEPEND_TAG); }
"<""/"{d}{e}{p}{e}{n}{d}">"  {  return(CLOSE_DEPEND_TAG ); }
"<"{o}{n}"_"{o}{p}{e}{r}{a}{t}{o}{r}">"   { return (OPEN_ON_OPERATOR_TAG); }
"<""/"{o}{n}"_"{o}{p}{e}{r}{a}{t}{o}{r}">"  { return (CLOSE_ON_OPERATOR_TAG); }
"<"{d}{e}{p}{e}{n}{d}"_"{a}{r}{t}">"    { return  (OPEN_DEPEND_ART_TAG) ;  }  
"<""/"{d}{e}{p}{e}{n}{d}"_"{a}{r}{t}">"    { return  (CLOSE_DEPEND_ART_TAG);  }
\?[a-zA-Z][a-zA-Z0-9\-_\[\]]* { strupcase(yytext); strcpy(yylval.string, yytext); 
 return(VARIABLE); }
"("  { return(OPEN_PAREN); }

")"  {  return(CLOSE_PAREN); }

\;(.)*\n  {  lineno++; } 
\;(.)*  {  /* this will hold only in files that end with
		   a comment but no linefeed */ } 

<COMMENT>(.^\")*\n    {  lineno++; }  ;

<INITIAL>\" { BEGIN COMMENT;}

<COMMENT>\" { BEGIN INITIAL;}

\n    {  lineno++; } 

<OVERREAD>(.^\(\))*\n  {  lineno++; }

<OVERREAD>[^\(\)]  {  }

<OVERREAD>\(  {  gbracket_count++; }

<OVERREAD>\)  {  gbracket_count--; 
  if (!gbracket_count) BEGIN INITIAL; }

. {}
%%
