/* tllex.h */

/* global structures and definitions */

#undef YYSTYPE
#define YYSTYPE LISTP					/* yacc yylval type */
#undef YY_DECL							/* lex yylex type */
#define YY_DECL int yylex(YYSTYPE *plValue)

/* global function prototypes */

DECLSPEC void LoadFile
(
	char *psFile						/* file name */
);
DECLSPEC CELLP StringToFormula
(
	char *psString						/* string to convert */
);

