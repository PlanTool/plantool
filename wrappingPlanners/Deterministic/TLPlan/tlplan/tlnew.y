/* tl.y -- tlplanner parser definition

Copyright C, 1997, F. Bacchus

Log

Name		Date	Description

M Ady		970103	First Version

*/

%{

/* tl_tab.c -- tlplanner parser

Copyright C, 1997, F. Bacchus.

This file was created automatically from the file tl.y.

Do not modify this file.  Modify and rebuild tl.y instead.

*/

/* C Declarations */

#ifdef WIN32
#include <malloc.h>						/* borland and microsoft specific include! */
#endif /* WIN32 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>						/* BISON should do this for itself */

#include "tlplan.h"
#include "list.h"
#include "tlparse.h"
#include "util.h"

static void __yy_memcpy
(
	char *from,
	char *to,
	int count
);

#define YYDEBUG 1
#define YYERROR_VERBOSE 1
#define VERBOSE							/* define for verbose output */

YY_DECL;								/* yylex prototype */
/*int yylex(void); */
void yyerror(char *s);
%}

/* YACC Declarations

Notes:
	The lexical analyzer returns a token and optionally an atom (actually
	a list structure element).

	The following tokens don't actually appear in the grammar, but are
	defined here to ensure that they are unique.

	ATOM_LISTP
	ATOM_BTREEP
	ATOM_FUNCTIONP
	ATOM_FORMULAP
	ATOM_OPERATORP
	ATOM_SYMBOLINFOP
	ATOM_ISPECP

	ATOM_BADTOK -- used by yylex to flag an illegal token.
*/

%token ATOM_IDENT
%token ATOM_INTEGER
%token ATOM_FLOAT
%token ATOM_STRING
%token ATOM_INFINITY
%token ATOM_LOWER_OPEN
%token ATOM_LOWER_CLOSED
%token ATOM_UPPER_OPEN
%token ATOM_UPPER_CLOSED

%token ATOM_LISTP
%token ATOM_BTREEP
%token ATOM_FUNCTIONP
%token ATOM_FORMULAP
%token ATOM_OPERATORP
%token ATOM_SYMBOLINFOP
%token ATOM_ARRAYINFOP
%token ATOM_ISPECP

%token ATOM_BADTOK

%pure_parser

%%

/* plan */

plan:
	lists
;

/* lists */

lists:
	/* empty */
	| lists list
	{
#ifdef VERBOSE
		ErrorMessage("Parsed lists\n");
#endif /* VERBOSE */
		(*pfYYOutput)($2);				/* process the list */
	}
	| lists error
	{
		if(bInteractiveMode)
			fprintf(stdout,"OK> ");
		$$=0;
		if(yychar>0)					/* don't discard end of file */
		{
			yyclearin;					/* discard lookahead */
			yyerrok;					/* attempt to continue */
		}
	}
;

/* list

Action:
	If the list is empty, we create and return an empty list element.
	Otherwise, we promote the leading element of the list, and make the
	remaining elements its arguments.
*/

list:
	'(' ')'
	{
		$$=NewList(ATOM_LISTP,"",0);
#ifdef VERBOSE
		ErrorMessage("Parsed list\n");
#endif /* VERBOSE */
	}
	'(' list_elements
	{
		$$=NewList(ATOM_LISTP,"",$2);
#ifdef VERBOSE
		ErrorMessage("Parsed list\n");
#endif /* VERBOSE */
	}
;

/* list_elements

Action:
	We string together all of the list elements we're passed, using
	the pfNext pointer.
*/

list_elements:
	list_element ')'
	{
		$$=$1;
#ifdef VERBOSE
		ErrorMessage("Parsed list_elements, added %s\n",$2->psName);
#endif /* VERBOSE */
	}
	| more_elements list_element ')'
	{
		LISTP pl;						/* pointer to list */

		for(pl=$1;pl->plNext;pl=pl->plNext);	/* find last element */
		pl->plNext=$2;				/* append next element to list */
		$$=$1;
#ifdef VERBOSE
		ErrorMessage("Parsed list_elements, added %s\n",$2->psName);
#endif /* VERBOSE */
	}
;

/* more_elements

Action:
	We return a list of the elements.
*/

more_elements:
	list_element
	{
		$$=$1;
#ifdef VERBOSE
		ErrorMessage("Parsed more_elements, added nothing\n");
#endif /* VERBOSE */
	}
	| more_elements list_element
	{
		LISTP pl;						/* pointer to list */

		for(pl=$1;pl->plNext;pl=pl->plNext);	/* find last element */
		pl->plNext=$2;				/* append next element to list */
		$$=$1;
#ifdef VERBOSE
		ErrorMessage("Parsed more_elements, added %s\n",$2->psName);
#endif /* VERBOSE */
	}
;

/* list_element

Action:
	We return the list element itself.
*/

list_element:
	ATOM_IDENT
	{
#ifdef VERBOSE
		ErrorMessage("Parsed list_element %s\n",$1->psName);
#endif /* VERBOSE */
		$$=$1;							/* non-empty list element */
	}
	| ATOM_INTEGER
	{
#ifdef VERBOSE
		ErrorMessage("Parsed list_element %s\n",$1->psName);
#endif /* VERBOSE */
		$$=$1;							/* non-empty list element */
	}
	| ATOM_FLOAT
	{
#ifdef VERBOSE
		ErrorMessage("Parsed list_element %s\n",$1->psName);
#endif /* VERBOSE */
		$$=$1;							/* non-empty list element */
	}
	| ATOM_STRING
	{
#ifdef VERBOSE
		ErrorMessage("Parsed list_element %s\n",$1->psName);
#endif /* VERBOSE */
		$$=$1;							/* non-empty list element */
	}
	| list
	{
#ifdef VERBOSE
		ErrorMessage("Parsed list_element %s\n",$1->psName);
#endif /* VERBOSE */
		$$=$1;							/* non-empty list element */
	}
	| interval
	{
#ifdef VERBOSE
		ErrorMessage("Parsed list_element %s\n",$1->psName);
#endif /* VERBOSE */
		$$=$1;							/* non-empty list element */
	}
	| error
	{
		if(bInteractiveMode)
			fprintf(stdout,"OK> ");
		$$=0;
		if(yychar>0)					/* don't discard end of file */
		{
			yyclearin;					/* discard lookahead */
			yyerrok;					/* attempt to continue */
		}
	}
;

/* interval

Action:
	We return an interval list.
*/

interval:
	'(' left_interval_element ',' right_interval_element ')'
	{
		LISTP pl;

#ifdef VERBOSE
		ErrorMessage("Parsed interval\n");
#endif /* VERBOSE */
		pl=NewList(ATOM_IDENT,"ISPEC");
		pl->plNext=$1;					/* link tokens */
		$1->plNext=$2;
		$2->plNext=$4;
		$4->plNext=$5;
		$$=NewList(ATOM_LISTP,"",pl);	/* enclosing list */
	}
	| '[' left_interval_element ',' right_interval_element ')'
	{
		LISTP pl;

#ifdef VERBOSE
		ErrorMessage("Parsed interval\n");
#endif /* VERBOSE */
		pl=NewList(ATOM_IDENT,"ISPEC");
		pl->plNext=$1;					/* link tokens */
		$1->plNext=$2;
		$2->plNext=$4;
		$4->plNext=$5;
		$$=NewList(ATOM_LISTP,"",pl);	/* enclosing list */
	}
	| '(' left_interval_element ',' right_interval_element ']'
	{
		LISTP pl;

#ifdef VERBOSE
		ErrorMessage("Parsed interval\n");
#endif /* VERBOSE */
		pl=NewList(ATOM_IDENT,"ISPEC");
		pl->plNext=$1;					/* link tokens */
		$1->plNext=$2;
		$2->plNext=$4;
		$4->plNext=$5;
		$$=NewList(ATOM_LISTP,"",pl);	/* enclosing list */
	}
	| '[' left_interval_element ',' right_interval_element ']'
	{
		LISTP pl;

#ifdef VERBOSE
		ErrorMessage("Parsed interval\n");
#endif /* VERBOSE */
		pl=NewList(ATOM_IDENT,"ISPEC");
		pl->plNext=$1;					/* link tokens */
		$1->plNext=$2;
		$2->plNext=$4;
		$4->plNext=$5;
		$$=NewList(ATOM_LISTP,"",pl);	/* enclosing list */
	}
;

/* left_interval_element

Action:
	We return the interval element itself.
*/

left_interval_element:
	ATOM_IDENT
	{
#ifdef VERBOSE
		ErrorMessage("Parsed left interval_element %s\n",$1->psName);
#endif /* VERBOSE */
		$$=$1;							/* non-empty list element */
	}
	| ATOM_INTEGER
	{
#ifdef VERBOSE
		ErrorMessage("Parsed left interval_element %s\n",$1->psName);
#endif /* VERBOSE */
		$$=$1;							/* non-empty list element */
	}
	| ATOM_FLOAT
	{
#ifdef VERBOSE
		ErrorMessage("Parsed left interval_element %s\n",$1->psName);
#endif /* VERBOSE */
		$$=$1;							/* non-empty list element */
	}
	| list
	{
#ifdef VERBOSE
		ErrorMessage("Parsed left interval_element %s\n",$1->psName);
#endif /* VERBOSE */
		$$=$1;							/* non-empty list element */
	}
;

/* right_interval_element

Action:
	We return the interval element itself.
*/

right_interval_element:
	ATOM_IDENT
	{
#ifdef VERBOSE
		ErrorMessage("Parsed right interval element %s\n",$1->psName);
#endif /* VERBOSE */
		$$=$1;							/* non-empty list element */
	}
	| ATOM_INTEGER
	{
#ifdef VERBOSE
		ErrorMessage("Parsed right interval element %s\n",$1->psName);
#endif /* VERBOSE */
		$$=$1;							/* non-empty list element */
	}
	| ATOM_FLOAT
	{
#ifdef VERBOSE
		ErrorMessage("Parsed right interval element %s\n",$1->psName);
#endif /* VERBOSE */
		$$=$1;							/* non-empty list element */
	}
	| ATOM_INFINITY
	{
		$$=$1;
#ifdef VERBOSE
		ErrorMessage("Parsed right interval element (list)\n");
#endif /* VERBOSE */
	}
	| list
	{
#ifdef VERBOSE
		ErrorMessage("Parsed right interval element %s\n",$1->psName);
#endif /* VERBOSE */
		$$=$1;							/* non-empty list element */
	}
;

%%

