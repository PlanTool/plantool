typedef union {
  char *str;
  op_list oplist;
  fact_list flist;
  token_list tlist;
} YYSTYPE;
#define	DEFINE	257
#define	DOMAIN	258
#define	REQUIREMENTS	259
#define	CONSTANTS	260
#define	TYPES	261
#define	PREDICATES	262
#define	PROBLEM	263
#define	OBJECTS	264
#define	INIT	265
#define	LENGTH	266
#define	PARALLEL	267
#define	SERIAL	268
#define	ACTION	269
#define	PARAMETERS	270
#define	PRECONDITION	271
#define	EFFECT	272
#define	AND	273
#define	EXISTS	274
#define	EQ	275
#define	NOT	276
#define	GOAL	277
#define	EITHER	278
#define	FLUENT	279
#define	ID	280
#define	VAR	281
#define	SET	282
#define	TEST	283
#define	INFLUENCE	284


extern YYSTYPE yylval;
