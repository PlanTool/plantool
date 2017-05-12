 


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
#ifdef YYDEBUG
  extern int yydebug=1;
#endif


#include <stdio.h>
#include <string.h> 
#include "ff.h"
#include "memory.h"
#include "parse.h"


#ifndef SCAN_ERR
#define SCAN_ERR
#define DEFINE_EXPECTED            0
#define PROBLEM_EXPECTED           1
#define PROBNAME_EXPECTED          2
#define LBRACKET_EXPECTED          3
#define RBRACKET_EXPECTED          4
#define DOMDEFS_EXPECTED           5
#define REQUIREM_EXPECTED          6
#define TYPEDLIST_EXPECTED         7
#define DOMEXT_EXPECTED            8
#define DOMEXTNAME_EXPECTED        9
#define TYPEDEF_EXPECTED          10
#define CONSTLIST_EXPECTED        11
#define PREDDEF_EXPECTED          12 
#define NAME_EXPECTED             13
#define VARIABLE_EXPECTED         14
#define ACTIONFUNCTOR_EXPECTED    15
#define ATOM_FORMULA_EXPECTED     16
#define EFFECT_DEF_EXPECTED       17
#define NEG_FORMULA_EXPECTED      18
#define NOT_SUPPORTED             19
#define SITUATION_EXPECTED        20
#define SITNAME_EXPECTED          21
#define BDOMAIN_EXPECTED          22
#define BADDOMAIN                 23
#define INIFACTS                  24
#define GOALDEF                   25
#define ADLGOAL                   26
#endif


static char * serrmsg[] = {
  "'define' expected",
  "'problem' expected",
  "problem name expected",
  "'(' expected",
  "')' expected",
  "additional domain definitions expected",
  "requirements (e.g. ':strips') expected",
  "typed list of <%s> expected",
  "domain extension expected",
  "domain to be extented expected",
  "type definition expected",
  "list of constants expected",
  "predicate definition expected",
  "<name> expected",
  "<variable> expected",
  "action functor expected",
  "atomic formula expected",
  "effect definition expected",
  "negated atomic formula expected",
  "requirement %s not supported by this IPP version",  
  "'situation' expected",
  "situation name expected",
  "':domain' expected",
  "this problem needs another domain file",
  "initial facts definition expected",
  "goal definition expected",
  "first order logic expression expected",
  NULL
};


/* void fcterr( int errno_, char *par ); */


static int sact_err;
static char *sact_err_par = NULL;
static Bool sis_negated = FALSE;

%}


%start file


%union {

  char string[MAX_LENGTH];
  char* pstring;
  ParseExpNode *pParseExpNode;
  PlNode* pPlNode;
  FactList* pFactList;
  TokenList* pTokenList;
  TypedList* pTypedList;
  TypedListList* pTypedListList;
  XMLOperator* pXMLOperator;
  XMLDependOnOp* pXMLDependOnOp;
}


%type <pTokenList> term_star
%type <pstring> term
%type  <pXMLDependOnOp> ON_OPERATOR
%type  <pXMLDependOnOp> DEPEND
%type <pXMLOperator> START_OPERATORS
%type <pXMLOperator> OPERATORS
%type <pXMLDependOnOp> ON_OPERATOR_STAR
%type <pTypedList> DEPEND_ART_STAR
%type <pTypedList> DEPEND_ART

%token CLOSE_DEPEND_ART_TAG
%token OPEN_DEPEND_ART_TAG
%token CLOSE_ON_OPERATOR_TAG
%token OPEN_ON_OPERATOR_TAG 
%token OPEN_DEPEND_TAG
%token CLOSE_DEPEND_TAG
%token OPEN_OPERATOR_TAG
%token CLOSE_OPERATOR_TAG
%token CLOSE_DURATIVE_FF_DEPEND_TAG
%token OPEN_DURATIVE_FF_DEPEND_TAG
%token PROLOG2_TAG
%token PROLOG1_TAG
%token OPEN_PAREN
%token CLOSE_PAREN
%token OPEN_NAME_TAG
%token CLOSE_NAME_TAG
%token <string> NAME
%token <string> VARIABLE

%%


/**********************************************************************/
file:
/* empty */
|
xml_definition  file
;


/**********************************************************************/
xml_definition : 
PROLOG1_TAG PROLOG2_TAG depend_Def
{
   printf("mit Erfolg");
}
;


/**********************************************************************/
depend_Def :
OPEN_DURATIVE_FF_DEPEND_TAG START_OPERATORS CLOSE_DURATIVE_FF_DEPEND_TAG       
{ 
  printf("mit Erfolg 2");
  op_depend_op=$2;
}
;


/**********************************************************************/
START_OPERATORS:
/* empty  */
{
   $$=NULL;
}
|
OPERATORS START_OPERATORS
{
 $1->next = $2;
 $$ = $1;
}
;
/************************************************************/
OPERATORS:
OPEN_OPERATOR_TAG OPEN_NAME_TAG OPEN_PAREN term_star CLOSE_PAREN CLOSE_NAME_TAG DEPEND  CLOSE_OPERATOR_TAG
{
   $$=new_XMLOperator();
   $$->name =  $4;
   $$->depend_on_ops=$7;
   printf("erfolg operator");
}

;


/**********************************************************************/
DEPEND:
/* empty */
{
   $$=NULL;
}
|
OPEN_DEPEND_TAG ON_OPERATOR_STAR CLOSE_DEPEND_TAG
{
$$=$2;
}
;
/**************************************************************/
ON_OPERATOR_STAR:
/* empty */
{
  $$=NULL;
}
|
ON_OPERATOR  ON_OPERATOR_STAR
{
  $1->next = $2;
  $$ = $1;
}
;
/**********************************************************************/
ON_OPERATOR:
OPEN_ON_OPERATOR_TAG OPEN_NAME_TAG OPEN_PAREN term_star CLOSE_PAREN CLOSE_NAME_TAG DEPEND_ART_STAR CLOSE_ON_OPERATOR_TAG
{
   $$=new_XMLDependOnOp();
   $$->name=$4;
   $$->depend_art=$7;
}
;
/********************************************************************/
DEPEND_ART_STAR:
/* empty */
{
   $$=NULL;
}
|
DEPEND_ART  DEPEND_ART_STAR
{
  $1->next = $2;
  $$ = $1;
}
;
/**********************************************************************/
DEPEND_ART:
OPEN_DEPEND_ART_TAG  term_star CLOSE_DEPEND_ART_TAG
{
$$=new_TypedList();
$$->type=$2;
}
;

/**********************************************************************/
term_star:
/* empty */
{ $$ = NULL; }
|
term  term_star
{
  $$ = new_TokenList();
  $$->item = $1;
  $$->next = $2;
}
;


/**********************************************************************/
term:
NAME
{ 
  $$ = new_Token( strlen($1)+1 );
  strcpy( $$, $1 );
}
|
VARIABLE
{ 
  $$ = new_Token( strlen($1)+1 );
  strcpy( $$, $1 );
}
;


/**********************************************************************/

%%


#include "lex.xml.c"


/**********************************************************************
 * Functions
 **********************************************************************/


/* 
 * call	bison -pfct -bscan-fct scan-fct.y
 */
void xmlerr( int errno_, char *par ) {

/*   sact_err = errno_; */

/*   if ( sact_err_par ) { */
/*     free( sact_err_par ); */
/*   } */
/*   if ( par ) { */
/*     sact_err_par = new_Token( strlen(par)+1 ); */
/*     strcpy( sact_err_par, par); */
/*   } else { */
/*     sact_err_par = NULL; */
/*   } */

}



int yyerror( char *msg )

{
  fflush( stdout );
  fprintf(stderr,"\n%s: syntax error in line %d, '%s':\n", 
	  "Depend.xml", lineno, xmltext );

  if ( sact_err_par ) {
    fprintf(stderr, "%s%s\n", serrmsg[sact_err], sact_err_par );
  } else {
    fprintf(stderr,"%s\n", serrmsg[sact_err] );
  }

  exit( 1 );

}



void load_xml_file() 
{
  char *filename="Depend.xml";
  FILE *fp;/* pointer to input files */
  char tmp[MAX_LENGTH] = "";

  /* open fact file 
   */
  if( ( fp = fopen( filename, "r" ) ) == NULL ) {
    sprintf(tmp, "\nff: can't find fact file: %s\n\n", filename );
    perror(tmp);
    exit ( 1 );
  }

  /*gact_filename = filename;*/
  lineno = 1; 
  xmlin = fp;

  yyparse();

  fclose( fp );/* and close file again */

}

