typedef union {
	char    *string;
	token_list	tl;
	fact_list	fl;
	op_list		ol;
} YYSTYPE;
#define	ID	258
#define	OPERATOR	259
#define	PARAMS	260
#define	PRECONDS	261
#define	AND	262
#define	ADD	263
#define	DEL	264
#define	EFFECTS	265


extern YYSTYPE yylval;
