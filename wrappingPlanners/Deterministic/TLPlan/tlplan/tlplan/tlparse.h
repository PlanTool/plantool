#ifndef __TLPARSE_H
#define __TLPARSE_H

/* tlparse.h -- parser declarations

Copyright C, 1997 - 99, F. Bacchus

*/

/* global structures and definitions */

/* global data */

extern int nLineNumber;					/* input line number */
char *psCurrentFile;					/* current file name */
extern char *yytext;					/* pointer to current token in buffer */
extern char *yysol;						/* pointer to current line in buffer */
extern char *apsStringTab[STRING__MAX];	/* hashed string pointer table */

extern HASHTAB htSymbolTable;			/* symbol table header */
extern HASHTAB htStringTable;			/* string table header */
extern FILE *pfInput;					/* input file stream */
extern char *psInput;					/* input string */
extern int (*pfYYInput)(char *, int);	/* lex input function pointer */
extern void (*pfYYOutput)(LISTP);		/* yacc output function pointer */
extern CELLP pcParsedFormula;			/* formula for StringToFormula */
extern BOOL bInteractiveMode;			/* running in interactive mode */

extern LINEARPLANP plpCurrentPlan;		/* current world */

#ifdef WIN32
extern BOOL bNT;						/* we're running on Windows NT */
#endif /* WIN32 */

/* global function prototypes */

int yyparse(void);
SYMBOLP AddToSymbolTable
(
	char *psName,						/* symbol name */
	int nType,							/* symbol type */
	void *pvValue						/* symbol value */
);
void InitSymbolTable(void);
int SymbolLookup
(
	char *psName,						/* symbol name to look for */
	SYMBOLP *ppsSymbol					/* returned pointer to symbol table entry */
);
FUNCTIONP FunctionLookup
(
	char *psName						/* function name */
);
void yyerror
(
	char *ps							/* message to print */
);
int yy_input
(
	char *psBuf,
	int nMaxSize
);
int string_input
(
	char *psBuf,
	int nMaxSize
);
BOOL Reserved
(
	char *psString						/* string to check */ 
);

CELLP NewFormula
(
	int nType,							/* token type */
	char *yytext,						/* token string */
	...									/* optional argument */
);
void DoEscapes
(
	char *pDest,						/* destination buffer */
	unsigned char *pSrc					/* source quoted string */
);
void ExecuteList
(
	LISTP pl							/* command to execute */
);
void TranslateList
(
	LISTP plCommand
);
DECLSPEC void InitPlanner(void);
DECLSPEC void ClosePlanner(void);
void MarkCurrentCommand(void);

// -----------------------------------------------------------------------------

#ifdef WIN32
DECLSPEC void InitPlannerThread
(
	HWND hWnd,							// command window handle
	char *psCmdLine						// (file to load)
);
DECLSPEC BOOL WriteCommand
(
	char *psCommand
);
#endif // WIN32

DECLSPEC int CommandPuts
(
	char *ps,
	FILE *fs
);
DECLSPEC void CommandPrintf
(
	FILE *pf,
	char *psFormat,
	...
);
DECLSPEC void AbortPlanner(void);

#endif /* __TLPARSE_H */
