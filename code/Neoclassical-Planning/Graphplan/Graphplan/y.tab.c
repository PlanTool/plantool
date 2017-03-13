
/*  A Bison parser, made from parse.y  */

#define	ID	258
#define	OPERATOR	259
#define	PARAMS	260
#define	PRECONDS	261
#define	AND	262
#define	ADD	263
#define	DEL	264
#define	EFFECTS	265



	#include <stdio.h>
	#include <string.h>
	#include <stdlib.h>

	#include "graphplan.h"
	
	fact_list fact_list_append(fact_list,fact_list);
	token_list token_list_append(token_list,token_list);
	
int  yylex();
void yyerror(char *);
int  yyparse();

typedef union {
	char    *string;
	token_list	tl;
	fact_list	fl;
	op_list		ol;
} YYSTYPE;

#ifndef YYLTYPE
typedef
  struct yyltype
    {
      int timestamp;
      int first_line;
      int first_column;
      int last_line;
      int last_column;
      char *text;
   }
  yyltype;

#define YYLTYPE yyltype
#endif

#include <stdio.h>

#ifndef __STDC__
#define const
#endif



#define	YYFINAL		63
#define	YYFLAG		-32768
#define	YYNTBASE	13

#define YYTRANSLATE(x) ((unsigned)(x) <= 265 ? yytranslate[x] : 31)

static const char yytranslate[] = {     0,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,    11,
    12,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     1,     2,     3,     4,     5,
     6,     7,     8,     9,    10
};

static const short yyprhs[] = {     0,
     0,     2,     4,     7,     8,     9,    10,    11,    34,    38,
    41,    46,    50,    55,    60,    62,    65,    70,    72,    75,
    79,    81,    84,    86,    88,    91,    93,    95
};

static const short yyrhs[] = {    14,
     0,    15,     0,    14,    15,     0,     0,     0,     0,     0,
    11,     4,     3,    16,    21,    11,     6,    20,    17,    24,
    18,    12,    11,    10,    11,    12,    11,    23,    12,    19,
    12,    12,     0,    11,    26,    12,     0,    11,    12,     0,
    11,     5,    30,    12,     0,    11,     5,    12,     0,    11,
     8,    25,    12,     0,    11,     9,    25,    12,     0,    22,
     0,    23,    22,     0,    11,     7,    26,    12,     0,    26,
     0,    11,    12,     0,    11,    28,    12,     0,    25,     0,
    26,    25,     0,     3,     0,    27,     0,    28,    27,     0,
     3,     0,    29,     0,    30,    29,     0
};

#if YYDEBUG != 0
static const short yyrline[] = { 0,
    38,    46,    49,    57,    67,    74,    79,    86,    93,    95,
   100,   101,   105,   110,   123,   126,   134,   139,   142,   147,
   155,   160,   167,   176,   178,   186,   189,   190
};

static const char * const yytname[] = {   "$",
"error","$illegal.","ID","OPERATOR","PARAMS","PRECONDS","AND","ADD","DEL","EFFECTS",
"'('","')'","stmts","ops","op","@1","@2","@3","@4","fact_list_part",
"param_part","effect","effect_list","and_fact_list","fact","fact_list","id","id_list","bogus_id","bogus_id_list",
""
};
#endif

static const short yyr1[] = {     0,
    13,    14,    14,    16,    17,    18,    19,    15,    20,    20,
    21,    21,    22,    22,    23,    23,    24,    24,    24,    25,
    26,    26,    27,    28,    28,    29,    30,    30
};

static const short yyr2[] = {     0,
     1,     1,     2,     0,     0,     0,     0,    22,     3,     2,
     4,     3,     4,     4,     1,     2,     4,     1,     2,     3,
     1,     2,     1,     1,     2,     1,     1,     2
};

static const short yydefact[] = {     0,
     0,     1,     2,     0,     3,     4,     0,     0,     0,     0,
     0,    26,    12,    27,     0,     0,    11,    28,     0,     5,
     0,    10,    21,     0,     0,    23,    24,     0,     9,    22,
     0,     6,    18,    20,    25,     0,    19,     0,     0,     0,
    17,     0,     0,     0,     0,     0,     0,    15,     0,     0,
     0,     7,    16,     0,     0,     0,    13,    14,     0,     8,
     0,     0,     0
};

static const short yydefgoto[] = {    61,
     2,     3,     7,    25,    38,    56,    20,     9,    48,    49,
    32,    23,    24,    27,    28,    14,    15
};

static const short yypact[] = {    -7,
     2,    -7,-32768,     8,-32768,-32768,     3,    25,    20,     0,
    26,-32768,-32768,-32768,     4,    22,-32768,-32768,     7,-32768,
    31,-32768,-32768,     9,    24,-32768,-32768,     5,-32768,-32768,
    -2,-32768,    27,-32768,-32768,    27,-32768,    28,    11,    30,
-32768,    29,    32,    33,    35,    36,    16,-32768,    17,    27,
    27,-32768,-32768,    37,    38,    39,-32768,-32768,    40,-32768,
    42,    44,-32768
};

static const short yypgoto[] = {-32768,
-32768,    34,-32768,-32768,-32768,-32768,-32768,-32768,   -12,-32768,
-32768,   -24,   -23,    41,-32768,    43,-32768
};


#define	YYLAST		69


static const short yytable[] = {    30,
    26,    33,    12,     1,    36,     4,    12,    26,    30,    37,
     6,    13,    39,     8,    30,    17,    34,    21,    22,    21,
    29,    21,    41,    50,    51,    54,    55,    47,    52,    10,
    11,    16,    19,    26,    31,     5,    53,    21,    43,    40,
    42,    62,    44,    63,    45,    46,    47,     0,    57,    58,
    59,    60,     0,     0,     0,     0,     0,    18,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,    35
};

static const short yycheck[] = {    24,
     3,    25,     3,    11,     7,     4,     3,     3,    33,    12,
     3,    12,    36,    11,    39,    12,    12,    11,    12,    11,
    12,    11,    12,     8,     9,    50,    51,    11,    12,     5,
    11,     6,    11,     3,    11,     2,    49,    11,    10,    12,
    11,     0,    11,     0,    12,    11,    11,    -1,    12,    12,
    12,    12,    -1,    -1,    -1,    -1,    -1,    15,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    28
};
/* -*-C-*-  Note some compilers choke on comments on `#line' lines.  */


/* Skeleton output parser for bison,
   Copyright (C) 1984, 1989, 1990 Bob Corbett and Richard Stallman

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 1, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */


#ifndef alloca
#ifdef __GNUC__
#define alloca __builtin_alloca
#else /* Not GNU C.  */
#if (!defined (__STDC__) && defined (sparc)) || defined (__sparc__)
#include <alloca.h>
#else /* Not sparc */
#ifdef MSDOS
#include <malloc.h>
#endif /* MSDOS */
#endif /* Not sparc.  */
#endif /* Not GNU C.  */
#endif /* alloca not defined.  */

/* This is the parser code that is written into each bison parser
  when the %semantic_parser declaration is not specified in the grammar.
  It was written by Richard Stallman by simplifying the hairy parser
  used when %semantic_parser is specified.  */

/* Note: there must be only one dollar sign in this file.
   It is replaced by the list of actions, each action
   as one case of the switch.  */

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		-2
#define YYEOF		0
#define YYACCEPT	return(0)
#define YYABORT 	return(1)
#define YYERROR		goto yyerrlab1
/* Like YYERROR except do call yyerror.
   This remains here temporarily to ease the
   transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */
#define YYFAIL		goto yyerrlab
#define YYRECOVERING()  (!!yyerrstatus)
#define YYBACKUP(token, value) \
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    { yychar = (token), yylval = (value);			\
      yychar1 = YYTRANSLATE (yychar);				\
      YYPOPSTACK;						\
      goto yybackup;						\
    }								\
  else								\
    { yyerror ("syntax error: cannot back up"); YYERROR; }	\
while (0)

#define YYTERROR	1
#define YYERRCODE	256

#ifndef YYPURE
#define YYLEX		yylex()
#endif

#ifdef YYPURE
#ifdef YYLSP_NEEDED
#define YYLEX		yylex(&yylval, &yylloc)
#else
#define YYLEX		yylex(&yylval)
#endif
#endif

/* If nonreentrant, generate the variables here */

#ifndef YYPURE

int	yychar;			/*  the lookahead symbol		*/
YYSTYPE	yylval;			/*  the semantic value of the		*/
				/*  lookahead symbol			*/

#ifdef YYLSP_NEEDED
YYLTYPE yylloc;			/*  location data for the lookahead	*/
				/*  symbol				*/
#endif

int yynerrs;			/*  number of parse errors so far       */
#endif  /* not YYPURE */

#if YYDEBUG != 0
int yydebug;			/*  nonzero means print parse trace	*/
/* Since this is uninitialized, it does not stop multiple parsers
   from coexisting.  */
#endif

/*  YYINITDEPTH indicates the initial size of the parser's stacks	*/

#ifndef	YYINITDEPTH
#define YYINITDEPTH 200
#endif

/*  YYMAXDEPTH is the maximum size the stacks can grow to
    (effective only if the built-in stack extension method is used).  */

#if YYMAXDEPTH == 0
#undef YYMAXDEPTH
#endif

#ifndef YYMAXDEPTH
#define YYMAXDEPTH 10000
#endif

#ifndef __cplusplus

/* This is the most reliable way to avoid incompatibilities
   in available built-in functions on various systems.  */
static void
__yy_bcopy (from, to, count)
     char *from;
     char *to;
     int count;
{
  register char *f = from;
  register char *t = to;
  register int i = count;

  while (i-- > 0)
    *t++ = *f++;
}

#else /* __cplusplus */

/* This is the most reliable way to avoid incompatibilities
   in available built-in functions on various systems.  */
static void
__yy_bcopy (char *from, char *to, int count)
{
  register char *f = from;
  register char *t = to;
  register int i = count;

  while (i-- > 0)
    *t++ = *f++;
}

#endif


int
yyparse()
{
  register int yystate;
  register int yyn;
  register short *yyssp;
  register YYSTYPE *yyvsp;
  int yyerrstatus;	/*  number of tokens to shift before error messages enabled */
  int yychar1;		/*  lookahead token as an internal (translated) token number */

  short	yyssa[YYINITDEPTH];	/*  the state stack			*/
  YYSTYPE yyvsa[YYINITDEPTH];	/*  the semantic value stack		*/

  short *yyss = yyssa;		/*  refer to the stacks thru separate pointers */
  YYSTYPE *yyvs = yyvsa;	/*  to allow yyoverflow to reallocate them elsewhere */

#ifdef YYLSP_NEEDED
  YYLTYPE *yyls = yylsa;
  YYLTYPE *yylsp;
  YYLTYPE yylsa[YYINITDEPTH];	/*  the location stack			*/

#define YYPOPSTACK   (yyvsp--, yysp--, yylsp--)
#else
#define YYPOPSTACK   (yyvsp--, yysp--)
#endif

  int yystacksize = YYINITDEPTH;

#ifdef YYPURE
  int yychar;
  YYSTYPE yylval;
  int yynerrs;
#ifdef YYLSP_NEEDED
  YYLTYPE yylloc;
#endif
#endif

  YYSTYPE yyval;		/*  the variable used to return		*/
				/*  semantic values from the action	*/
				/*  routines				*/

  int yylen;

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Starting parse\n");
#endif

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY;		/* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.  */

  yyssp = yyss - 1;
  yyvsp = yyvs;
#ifdef YYLSP_NEEDED
  yylsp = yyls;
#endif

/* Push a new state, which is found in  yystate  .  */
/* In all cases, when you get here, the value and location stacks
   have just been pushed. so pushing a state here evens the stacks.  */
yynewstate:

  *++yyssp = yystate;

  if (yyssp >= yyss + yystacksize - 1)
    {
      /* Give user a chance to reallocate the stack */
      /* Use copies of these so that the &'s don't force the real ones into memory. */
      YYSTYPE *yyvs1 = yyvs;
      short *yyss1 = yyss;
#ifdef YYLSP_NEEDED
      YYLTYPE *yyls1 = yyls;
#endif

      /* Get the current used size of the three stacks, in elements.  */
      int size = yyssp - yyss + 1;

#ifdef yyoverflow
      /* Each stack pointer address is followed by the size of
	 the data in use in that stack, in bytes.  */
      yyoverflow("parser stack overflow",
		 &yyss1, size * sizeof (*yyssp),
		 &yyvs1, size * sizeof (*yyvsp),
#ifdef YYLSP_NEEDED
		 &yyls1, size * sizeof (*yylsp),
#endif
		 &yystacksize);

      yyss = yyss1; yyvs = yyvs1;
#ifdef YYLSP_NEEDED
      yyls = yyls1;
#endif
#else /* no yyoverflow */
      /* Extend the stack our own way.  */
      if (yystacksize >= YYMAXDEPTH)
	{
	  yyerror("parser stack overflow");
	  return 2;
	}
      yystacksize *= 2;
      if (yystacksize > YYMAXDEPTH)
	yystacksize = YYMAXDEPTH;
      yyss = (short *) alloca (yystacksize * sizeof (*yyssp));
      __yy_bcopy ((char *)yyss1, (char *)yyss, size * sizeof (*yyssp));
      yyvs = (YYSTYPE *) alloca (yystacksize * sizeof (*yyvsp));
      __yy_bcopy ((char *)yyvs1, (char *)yyvs, size * sizeof (*yyvsp));
#ifdef YYLSP_NEEDED
      yyls = (YYLTYPE *) alloca (yystacksize * sizeof (*yylsp));
      __yy_bcopy ((char *)yyls1, (char *)yyls, size * sizeof (*yylsp));
#endif
#endif /* no yyoverflow */

      yyssp = yyss + size - 1;
      yyvsp = yyvs + size - 1;
#ifdef YYLSP_NEEDED
      yylsp = yyls + size - 1;
#endif

#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Stack size increased to %d\n", yystacksize);
#endif

      if (yyssp >= yyss + yystacksize - 1)
	YYABORT;
    }

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Entering state %d\n", yystate);
#endif

 yybackup:

/* Do appropriate processing given the current state.  */
/* Read a lookahead token if we need one and don't already have one.  */
/* yyresume: */

  /* First try to decide what to do without reference to lookahead token.  */

  yyn = yypact[yystate];
  if (yyn == YYFLAG)
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* yychar is either YYEMPTY or YYEOF
     or a valid token in external form.  */

  if (yychar == YYEMPTY)
    {
#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Reading a token: ");
#endif
      yychar = YYLEX;
    }

  /* Convert token to internal form (in yychar1) for indexing tables with */

  if (yychar <= 0)		/* This means end of input. */
    {
      yychar1 = 0;
      yychar = YYEOF;		/* Don't call YYLEX any more */

#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Now at end of input.\n");
#endif
    }
  else
    {
      yychar1 = YYTRANSLATE(yychar);

#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Next token is %d (%s)\n", yychar, yytname[yychar1]);
#endif
    }

  yyn += yychar1;
  if (yyn < 0 || yyn > YYLAST || yycheck[yyn] != yychar1)
    goto yydefault;

  yyn = yytable[yyn];

  /* yyn is what to do for this token type in this state.
     Negative => reduce, -yyn is rule number.
     Positive => shift, yyn is new state.
       New state is final state => don't bother to shift,
       just return success.
     0, or most negative number => error.  */

  if (yyn < 0)
    {
      if (yyn == YYFLAG)
	goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }
  else if (yyn == 0)
    goto yyerrlab;

  if (yyn == YYFINAL)
    YYACCEPT;

  /* Shift the lookahead token.  */

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Shifting token %d (%s), ", yychar, yytname[yychar1]);
#endif

  /* Discard the token being shifted unless it is eof.  */
  if (yychar != YYEOF)
    yychar = YYEMPTY;

  *++yyvsp = yylval;
#ifdef YYLSP_NEEDED
  *++yylsp = yylloc;
#endif

  /* count tokens shifted since error; after three, turn off error status.  */
  if (yyerrstatus) yyerrstatus--;

  yystate = yyn;
  goto yynewstate;

/* Do the default action for the current state.  */
yydefault:

  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;

/* Do a reduction.  yyn is the number of a rule to reduce with.  */
yyreduce:
  yylen = yyr2[yyn];
  yyval = yyvsp[1-yylen]; /* implement default value of the action */

#if YYDEBUG != 0
  if (yydebug)
    {
      int i;

      fprintf (stderr, "Reducing via rule %d (line %d), ",
	       yyn, yyrline[yyn]);

      /* Print the symboles being reduced, and their result.  */
      for (i = yyprhs[yyn]; yyrhs[i] > 0; i++)
	fprintf (stderr, "%s ", yytname[yyrhs[i]]);
      fprintf (stderr, " -> %s\n", yytname[yyr1[yyn]]);
    }
#endif


  switch (yyn) {

case 1:
{
		extern op_list parser_return;
		parser_return = yyvsp[0].ol;
	;
    break;}
case 2:
{ yyval.ol = yyvsp[0].ol; ;
    break;}
case 3:
{
		yyvsp[0].ol->next = yyvsp[-1].ol;
		yyval.ol = yyvsp[0].ol;
	;
    break;}
case 4:
{
			/* create a new operator */
			extern op_ptr  current_op;
			current_op = (op_ptr) calloc(1,sizeof(op_s));
			current_op->name = yyvsp[0].string;
			printf("%s\n",current_op->name);
		;
    break;}
case 5:
{
					extern op_ptr current_op;
					current_op->params = yyvsp[0].fl;
				;
    break;}
case 6:
{
					extern op_ptr current_op;
					current_op->preconds = yyvsp[0].fl;
				;
    break;}
case 7:
{
					extern op_ptr current_op;
					current_op->effects = yyvsp[-1].fl;
				;
    break;}
case 8:
{
		extern op_ptr current_op;
		yyval.ol = current_op;
	;
    break;}
case 9:
{ yyval.fl = yyvsp[-1].fl; ;
    break;}
case 10:
{ yyval.fl = NULL; ;
    break;}
case 13:
{
			yyval.fl = yyvsp[-1].fl;
		;
    break;}
case 14:
{
			token_list foo;
			foo = (token_list) calloc(1,sizeof(token_list_elt));
			foo->item = malloc(4*sizeof(char));
			strcpy(foo->item,"del");
			foo->next = yyvsp[-1].fl->item;
			yyvsp[-1].fl->item = foo;
			yyval.fl = yyvsp[-1].fl;
		;
    break;}
case 15:
{ yyval.fl = yyvsp[0].fl; ;
    break;}
case 16:
{
			yyval.fl = fact_list_append(yyvsp[-1].fl,yyvsp[0].fl) ;
		;
    break;}
case 17:
{
			yyval.fl = yyvsp[-1].fl;
		;
    break;}
case 18:
{ yyval.fl = yyvsp[0].fl; ;
    break;}
case 19:
{ yyval.fl = NULL; ;
    break;}
case 20:
{
			fact_list foo;
			foo = (fact_list) calloc(1,sizeof(fact_list_elt));
			foo->item = yyvsp[-1].tl;
			yyval.fl = foo;
		;
    break;}
case 21:
{
			   yyval.fl = yyvsp[0].fl;
		   ;
    break;}
case 22:
{
			yyval.fl = fact_list_append(yyvsp[-1].fl,yyvsp[0].fl);
		;
    break;}
case 23:
{
			token_list foo;
			foo = (token_list) calloc(1,sizeof(token_list_elt));
			foo->item = yyvsp[0].string;
			yyval.tl = foo;
		;
    break;}
case 25:
{
			yyval.tl = token_list_append(yyvsp[-1].tl,yyvsp[0].tl);
		;
    break;}
case 26:
{ free(yyvsp[0].string); ;
    break;}
}
   /* the action file gets copied in in place of this dollarsign */


  yyvsp -= yylen;
  yyssp -= yylen;
#ifdef YYLSP_NEEDED
  yylsp -= yylen;
#endif

#if YYDEBUG != 0
  if (yydebug)
    {
      short *ssp1 = yyss - 1;
      fprintf (stderr, "state stack now");
      while (ssp1 != yyssp)
	fprintf (stderr, " %d", *++ssp1);
      fprintf (stderr, "\n");
    }
#endif

  *++yyvsp = yyval;

#ifdef YYLSP_NEEDED
  yylsp++;
  if (yylen == 0)
    {
      yylsp->first_line = yylloc.first_line;
      yylsp->first_column = yylloc.first_column;
      yylsp->last_line = (yylsp-1)->last_line;
      yylsp->last_column = (yylsp-1)->last_column;
      yylsp->text = 0;
    }
  else
    {
      yylsp->last_line = (yylsp+yylen-1)->last_line;
      yylsp->last_column = (yylsp+yylen-1)->last_column;
    }
#endif

  /* Now "shift" the result of the reduction.
     Determine what state that goes to,
     based on the state we popped back to
     and the rule number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTBASE] + *yyssp;
  if (yystate >= 0 && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTBASE];

  goto yynewstate;

yyerrlab:   /* here on detecting error */

  if (! yyerrstatus)
    /* If not already recovering from an error, report this error.  */
    {
      ++yynerrs;

#ifdef YYERROR_VERBOSE
      yyn = yypact[yystate];

      if (yyn > YYFLAG && yyn < YYLAST)
	{
	  int size = 0;
	  char *msg;
	  int x, count;

	  count = 0;
	  for (x = 0; x < (sizeof(yytname) / sizeof(char *)); x++)
	    if (yycheck[x + yyn] == x)
	      size += strlen(yytname[x]) + 15, count++;
	  msg = (char *) xmalloc(size + 15);
	  strcpy(msg, "parse error");

	  if (count < 5)
	    {
	      count = 0;
	      for (x = 0; x < (sizeof(yytname) / sizeof(char *)); x++)
		if (yycheck[x + yyn] == x)
		  {
		    strcat(msg, count == 0 ? ", expecting `" : " or `");
		    strcat(msg, yytname[x]);
		    strcat(msg, "'");
		    count++;
		  }
	    }
	  yyerror(msg);
	  free(msg);
	}
      else
#endif /* YYERROR_VERBOSE */
	yyerror("parse error");
    }

yyerrlab1:   /* here on error raised explicitly by an action */

  if (yyerrstatus == 3)
    {
      /* if just tried and failed to reuse lookahead token after an error, discard it.  */

      /* return failure if at end of input */
      if (yychar == YYEOF)
	YYABORT;

#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Discarding token %d (%s).\n", yychar, yytname[yychar1]);
#endif

      yychar = YYEMPTY;
    }

  /* Else will try to reuse lookahead token
     after shifting the error token.  */

  yyerrstatus = 3;		/* Each real token shifted decrements this */

  goto yyerrhandle;

yyerrdefault:  /* current state does not do anything special for the error token. */

#if 0
  /* This is wrong; only states that explicitly want error tokens
     should shift them.  */
  yyn = yydefact[yystate];  /* If its default is to accept any token, ok.  Otherwise pop it.*/
  if (yyn) goto yydefault;
#endif

yyerrpop:   /* pop the current state because it cannot handle the error token */

  if (yyssp == yyss) YYABORT;
  yyvsp--;
  yystate = *--yyssp;
#ifdef YYLSP_NEEDED
  yylsp--;
#endif

#if YYDEBUG != 0
  if (yydebug)
    {
      short *ssp1 = yyss - 1;
      fprintf (stderr, "Error: state stack now");
      while (ssp1 != yyssp)
	fprintf (stderr, " %d", *++ssp1);
      fprintf (stderr, "\n");
    }
#endif

yyerrhandle:

  yyn = yypact[yystate];
  if (yyn == YYFLAG)
    goto yyerrdefault;

  yyn += YYTERROR;
  if (yyn < 0 || yyn > YYLAST || yycheck[yyn] != YYTERROR)
    goto yyerrdefault;

  yyn = yytable[yyn];
  if (yyn < 0)
    {
      if (yyn == YYFLAG)
	goto yyerrpop;
      yyn = -yyn;
      goto yyreduce;
    }
  else if (yyn == 0)
    goto yyerrpop;

  if (yyn == YYFINAL)
    YYACCEPT;

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Shifting error token, ");
#endif

  *++yyvsp = yylval;
#ifdef YYLSP_NEEDED
  *++yylsp = yylloc;
#endif

  yystate = yyn;
  goto yynewstate;
}

