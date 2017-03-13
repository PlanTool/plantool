/* A Bison parser, made by GNU Bison 1.875.  */

/* Skeleton parser for Yacc-like parsing with Bison,
   Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */

/* As a special exception, when this file is copied by Bison into a
   Bison output file, you may use that output file without restriction.
   This special exception was added by the Free Software Foundation
   in version 1.24 of Bison.  */

/* Written by Richard Stallman by simplifying the original so called
   ``semantic'' parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Using locations.  */
#define YYLSP_NEEDED 0

/* If NAME_PREFIX is specified substitute the variables and functions
   names.  */
#define yyparse fct_pddlparse
#define yylex   fct_pddllex
#define yyerror fct_pddlerror
#define yylval  fct_pddllval
#define yychar  fct_pddlchar
#define yydebug fct_pddldebug
#define yynerrs fct_pddlnerrs


/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     DEFINE_TOK = 258,
     PROBLEM_TOK = 259,
     SITUATION_TOK = 260,
     BSITUATION_TOK = 261,
     OBJECTS_TOK = 262,
     BDOMAIN_TOK = 263,
     INIT_TOK = 264,
     GOAL_TOK = 265,
     AND_TOK = 266,
     NOT_TOK = 267,
     NAME = 268,
     VARIABLE = 269,
     TYPE = 270,
     EQUAL_TOK = 271,
     FORALL_TOK = 272,
     IMPLY_TOK = 273,
     OR_TOK = 274,
     EXISTS_TOK = 275,
     EITHER_TOK = 276,
     OPEN_PAREN = 277,
     CLOSE_PAREN = 278
   };
#endif
#define DEFINE_TOK 258
#define PROBLEM_TOK 259
#define SITUATION_TOK 260
#define BSITUATION_TOK 261
#define OBJECTS_TOK 262
#define BDOMAIN_TOK 263
#define INIT_TOK 264
#define GOAL_TOK 265
#define AND_TOK 266
#define NOT_TOK 267
#define NAME 268
#define VARIABLE 269
#define TYPE 270
#define EQUAL_TOK 271
#define FORALL_TOK 272
#define IMPLY_TOK 273
#define OR_TOK 274
#define EXISTS_TOK 275
#define EITHER_TOK 276
#define OPEN_PAREN 277
#define CLOSE_PAREN 278




/* Copy the first part of user declarations.  */
#line 1 "scan-fct_pddl.y"

#ifdef YYDEBUG
  extern int yydebug=1;
#endif

#include <stdio.h>
#include <string.h> 
#include "ipp.h"
#include "pddl.h"
#include "pddl-types.h"
#include "utilities.h"
#include "memory.h"

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

/* void fcterr( int errno, char *par ); */

static int sact_err;
static char * sact_err_par = NULL;
static Bool sis_negated = FALSE;



/* Enabling traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif

#if ! defined (YYSTYPE) && ! defined (YYSTYPE_IS_DECLARED)
#line 86 "scan-fct_pddl.y"
typedef union YYSTYPE {
  char string[MAX_LENGTH];
  char* pstring;
  PlNode* pPlNode;
  FactList* pFactList;
  TokenList* pTokenList;
} YYSTYPE;
/* Line 191 of yacc.c.  */
#line 221 "scan-fct_pddl.tab.c"
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif



/* Copy the second part of user declarations.  */


/* Line 214 of yacc.c.  */
#line 233 "scan-fct_pddl.tab.c"

#if ! defined (yyoverflow) || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# if YYSTACK_USE_ALLOCA
#  define YYSTACK_ALLOC alloca
# else
#  ifndef YYSTACK_USE_ALLOCA
#   if defined (alloca) || defined (_ALLOCA_H)
#    define YYSTACK_ALLOC alloca
#   else
#    ifdef __GNUC__
#     define YYSTACK_ALLOC __builtin_alloca
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's `empty if-body' warning. */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
# else
#  if defined (__STDC__) || defined (__cplusplus)
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   define YYSIZE_T size_t
#  endif
#  define YYSTACK_ALLOC malloc
#  define YYSTACK_FREE free
# endif
#endif /* ! defined (yyoverflow) || YYERROR_VERBOSE */


#if (! defined (yyoverflow) \
     && (! defined (__cplusplus) \
	 || (YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  short yyss;
  YYSTYPE yyvs;
  };

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (short) + sizeof (YYSTYPE))				\
      + YYSTACK_GAP_MAXIMUM)

/* Copy COUNT objects from FROM to TO.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if 1 < __GNUC__
#   define YYCOPY(To, From, Count) \
      __builtin_memcpy (To, From, (Count) * sizeof (*(From)))
#  else
#   define YYCOPY(To, From, Count)		\
      do					\
	{					\
	  register YYSIZE_T yyi;		\
	  for (yyi = 0; yyi < (Count); yyi++)	\
	    (To)[yyi] = (From)[yyi];		\
	}					\
      while (0)
#  endif
# endif

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack)					\
    do									\
      {									\
	YYSIZE_T yynewbytes;						\
	YYCOPY (&yyptr->Stack, Stack, yysize);				\
	Stack = &yyptr->Stack;						\
	yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
	yyptr += yynewbytes / sizeof (*yyptr);				\
      }									\
    while (0)

#endif

#if defined (__STDC__) || defined (__cplusplus)
   typedef signed char yysigned_char;
#else
   typedef short yysigned_char;
#endif

/* YYFINAL -- State number of the termination state. */
#define YYFINAL  5
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   104

/* YYNTOKENS -- Number of terminals. */
#define YYNTOKENS  24
/* YYNNTS -- Number of nonterminals. */
#define YYNNTS  26
/* YYNRULES -- Number of rules. */
#define YYNRULES  52
/* YYNRULES -- Number of states. */
#define YYNSTATES  110

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   278

#define YYTRANSLATE(YYX) 						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const unsigned char yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
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
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const unsigned char yyprhs[] =
{
       0,     0,     3,     4,     7,     8,    15,    20,    25,    26,
      29,    32,    35,    38,    43,    44,    50,    51,    57,    59,
      64,    69,    74,    80,    88,    96,    97,   100,   105,   107,
     112,   113,   116,   118,   120,   122,   125,   126,   132,   136,
     139,   140,   146,   150,   153,   155,   157,   159,   162,   167,
     169,   174,   175
};

/* YYRHS -- A `-1'-separated list of the rules' RHS. */
static const yysigned_char yyrhs[] =
{
      25,     0,    -1,    -1,    26,    25,    -1,    -1,    22,     3,
      27,    28,    30,    23,    -1,    22,     4,    13,    23,    -1,
      22,     8,    13,    23,    -1,    -1,    31,    30,    -1,    32,
      30,    -1,    34,    30,    -1,    29,    30,    -1,    22,     7,
      43,    23,    -1,    -1,    22,     9,    33,    46,    23,    -1,
      -1,    22,    10,    35,    36,    23,    -1,    38,    -1,    22,
      11,    37,    23,    -1,    22,    19,    37,    23,    -1,    22,
      12,    36,    23,    -1,    22,    18,    36,    36,    23,    -1,
      22,    20,    22,    44,    23,    36,    23,    -1,    22,    17,
      22,    44,    23,    36,    23,    -1,    -1,    36,    37,    -1,
      22,    12,    39,    23,    -1,    39,    -1,    22,    45,    40,
      23,    -1,    -1,    41,    40,    -1,    13,    -1,    14,    -1,
      13,    -1,    13,    42,    -1,    -1,    13,    21,    42,    23,
      43,    -1,    13,    15,    43,    -1,    13,    43,    -1,    -1,
      14,    21,    42,    23,    44,    -1,    14,    15,    44,    -1,
      14,    44,    -1,    13,    -1,    16,    -1,    47,    -1,    47,
      46,    -1,    22,    12,    48,    23,    -1,    48,    -1,    22,
      45,    49,    23,    -1,    -1,    13,    49,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const unsigned short yyrline[] =
{
       0,   135,   135,   138,   144,   143,   156,   165,   175,   178,
     180,   182,   184,   189,   224,   223,   235,   234,   249,   265,
     271,   277,   283,   293,   333,   373,   377,   388,   394,   402,
     413,   417,   427,   433,   442,   449,   461,   463,   520,   532,
     559,   561,   614,   626,   652,   658,   667,   672,   681,   691,
     700,   711,   713
};
#endif

#if YYDEBUG || YYERROR_VERBOSE
/* YYTNME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals. */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "DEFINE_TOK", "PROBLEM_TOK", 
  "SITUATION_TOK", "BSITUATION_TOK", "OBJECTS_TOK", "BDOMAIN_TOK", 
  "INIT_TOK", "GOAL_TOK", "AND_TOK", "NOT_TOK", "NAME", "VARIABLE", 
  "TYPE", "EQUAL_TOK", "FORALL_TOK", "IMPLY_TOK", "OR_TOK", "EXISTS_TOK", 
  "EITHER_TOK", "OPEN_PAREN", "CLOSE_PAREN", "$accept", "file", 
  "problem_definition", "@1", "problem_name", "base_domain_name", 
  "problem_defs", "objects_def", "init_def", "@2", "goal_def", "@3", 
  "adl_goal_description", "adl_goal_description_star", "literal_term", 
  "atomic_formula_term", "term_star", "term", "name_plus", 
  "typed_list_name", "typed_list_variable", "predicate", 
  "literal_name_plus", "literal_name", "atomic_formula_name", "name_star", 0
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[YYLEX-NUM] -- Internal token number corresponding to
   token YYLEX-NUM.  */
static const unsigned short yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const unsigned char yyr1[] =
{
       0,    24,    25,    25,    27,    26,    28,    29,    30,    30,
      30,    30,    30,    31,    33,    32,    35,    34,    36,    36,
      36,    36,    36,    36,    36,    37,    37,    38,    38,    39,
      40,    40,    41,    41,    42,    42,    43,    43,    43,    43,
      44,    44,    44,    44,    45,    45,    46,    46,    47,    47,
      48,    49,    49
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const unsigned char yyr2[] =
{
       0,     2,     0,     2,     0,     6,     4,     4,     0,     2,
       2,     2,     2,     4,     0,     5,     0,     5,     1,     4,
       4,     4,     5,     7,     7,     0,     2,     4,     1,     4,
       0,     2,     1,     1,     1,     2,     0,     5,     3,     2,
       0,     5,     3,     2,     1,     1,     1,     2,     4,     1,
       4,     0,     2
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const unsigned char yydefact[] =
{
       2,     0,     0,     2,     4,     1,     3,     0,     0,     8,
       0,     0,     8,     0,     8,     8,     8,     0,    36,     0,
      14,    16,    12,     5,     9,    10,    11,     6,    36,     0,
       0,     0,     0,    36,     0,    39,    13,     7,     0,     0,
      46,    49,     0,     0,    18,    28,    38,    34,     0,     0,
      44,    45,    51,    15,    47,    25,     0,     0,     0,    25,
       0,    30,    17,    35,    36,     0,     0,    51,     0,    25,
       0,     0,     0,    40,     0,     0,    40,    32,    33,     0,
      30,    37,    48,    52,    50,    26,    19,    21,    27,    40,
       0,     0,    20,     0,    29,    31,    40,     0,    43,     0,
      22,     0,    42,     0,     0,     0,    40,    24,    23,    41
};

/* YYDEFGOTO[NTERM-NUM]. */
static const yysigned_char yydefgoto[] =
{
      -1,     2,     3,     7,     9,    12,    13,    14,    15,    31,
      16,    32,    69,    70,    44,    45,    79,    80,    48,    29,
      90,    52,    39,    40,    41,    68
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -74
static const yysigned_char yypact[] =
{
     -14,    12,    19,   -14,   -74,   -74,   -74,     3,    39,    10,
      36,    27,    10,    29,    10,    10,    10,    30,    37,    41,
     -74,   -74,   -74,   -74,   -74,   -74,   -74,   -74,    -4,    32,
      33,    35,    38,    37,    45,   -74,   -74,   -74,    15,    40,
      35,   -74,    28,    42,   -74,   -74,   -74,    45,    43,    46,
     -74,   -74,    48,   -74,   -74,    38,    38,    49,    38,    38,
      50,    16,   -74,   -74,    37,    -3,    47,    48,    51,    38,
      52,    53,    54,    59,    38,    55,    59,   -74,   -74,    56,
      16,   -74,   -74,   -74,   -74,   -74,   -74,   -74,   -74,    -9,
      57,    58,   -74,    60,   -74,   -74,    59,    45,   -74,    38,
     -74,    38,   -74,    61,    62,    63,    59,   -74,   -74,   -74
};

/* YYPGOTO[NTERM-NUM].  */
static const yysigned_char yypgoto[] =
{
     -74,    79,   -74,   -74,   -74,   -74,     6,   -74,   -74,   -74,
     -74,   -74,   -32,   -55,   -74,     8,   -21,   -74,   -46,   -26,
     -73,    20,    64,   -74,    44,    21
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -1
static const unsigned char yytable[] =
{
      43,    63,    35,    93,    75,    89,    96,    46,     1,    28,
      50,    33,    97,    51,    85,     4,    98,    34,    22,     5,
      24,    25,    26,   102,    71,     8,    74,    49,    50,    77,
      78,    51,    11,   109,    18,    19,    20,    21,    81,    55,
      56,    50,    91,    10,    51,    57,    58,    59,    60,    17,
      28,   103,    23,    27,    30,    36,    37,    38,    47,    95,
      42,    67,    61,    53,    72,    62,    64,   104,    65,   105,
      82,    73,    76,    89,    84,    86,    87,    88,    92,    94,
      99,   100,     6,   101,   106,   107,   108,     0,    83,     0,
       0,     0,     0,    66,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    54
};

static const yysigned_char yycheck[] =
{
      32,    47,    28,    76,    59,    14,    15,    33,    22,    13,
      13,    15,    21,    16,    69,     3,    89,    21,    12,     0,
      14,    15,    16,    96,    56,    22,    58,    12,    13,    13,
      14,    16,    22,   106,     7,     8,     9,    10,    64,    11,
      12,    13,    74,     4,    16,    17,    18,    19,    20,    13,
      13,    97,    23,    23,    13,    23,    23,    22,    13,    80,
      22,    13,    42,    23,    56,    23,    23,    99,    22,   101,
      23,    22,    22,    14,    23,    23,    23,    23,    23,    23,
      23,    23,     3,    23,    23,    23,    23,    -1,    67,    -1,
      -1,    -1,    -1,    49,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    40
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const unsigned char yystos[] =
{
       0,    22,    25,    26,     3,     0,    25,    27,    22,    28,
       4,    22,    29,    30,    31,    32,    34,    13,     7,     8,
       9,    10,    30,    23,    30,    30,    30,    23,    13,    43,
      13,    33,    35,    15,    21,    43,    23,    23,    22,    46,
      47,    48,    22,    36,    38,    39,    43,    13,    42,    12,
      13,    16,    45,    23,    46,    11,    12,    17,    18,    19,
      20,    45,    23,    42,    23,    22,    48,    13,    49,    36,
      37,    36,    39,    22,    36,    37,    22,    13,    14,    40,
      41,    43,    23,    49,    23,    37,    23,    23,    23,    14,
      44,    36,    23,    44,    23,    40,    15,    21,    44,    23,
      23,    23,    44,    42,    36,    36,    23,    23,    23,    44
};

#if ! defined (YYSIZE_T) && defined (__SIZE_TYPE__)
# define YYSIZE_T __SIZE_TYPE__
#endif
#if ! defined (YYSIZE_T) && defined (size_t)
# define YYSIZE_T size_t
#endif
#if ! defined (YYSIZE_T)
# if defined (__STDC__) || defined (__cplusplus)
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# endif
#endif
#if ! defined (YYSIZE_T)
# define YYSIZE_T unsigned int
#endif

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		(-2)
#define YYEOF		0

#define YYACCEPT	goto yyacceptlab
#define YYABORT		goto yyabortlab
#define YYERROR		goto yyerrlab1

/* Like YYERROR except do call yyerror.  This remains here temporarily
   to ease the transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */

#define YYFAIL		goto yyerrlab

#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)					\
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    {								\
      yychar = (Token);						\
      yylval = (Value);						\
      yytoken = YYTRANSLATE (yychar);				\
      YYPOPSTACK;						\
      goto yybackup;						\
    }								\
  else								\
    { 								\
      yyerror ("syntax error: cannot back up");\
      YYERROR;							\
    }								\
while (0)

#define YYTERROR	1
#define YYERRCODE	256

/* YYLLOC_DEFAULT -- Compute the default location (before the actions
   are run).  */

#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)         \
  Current.first_line   = Rhs[1].first_line;      \
  Current.first_column = Rhs[1].first_column;    \
  Current.last_line    = Rhs[N].last_line;       \
  Current.last_column  = Rhs[N].last_column;
#endif

/* YYLEX -- calling `yylex' with the right arguments.  */

#ifdef YYLEX_PARAM
# define YYLEX yylex (YYLEX_PARAM)
#else
# define YYLEX yylex ()
#endif

/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)			\
do {						\
  if (yydebug)					\
    YYFPRINTF Args;				\
} while (0)

# define YYDSYMPRINT(Args)			\
do {						\
  if (yydebug)					\
    yysymprint Args;				\
} while (0)

# define YYDSYMPRINTF(Title, Token, Value, Location)		\
do {								\
  if (yydebug)							\
    {								\
      YYFPRINTF (stderr, "%s ", Title);				\
      yysymprint (stderr, 					\
                  Token, Value);	\
      YYFPRINTF (stderr, "\n");					\
    }								\
} while (0)

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (cinluded).                                                   |
`------------------------------------------------------------------*/

#if defined (__STDC__) || defined (__cplusplus)
static void
yy_stack_print (short *bottom, short *top)
#else
static void
yy_stack_print (bottom, top)
    short *bottom;
    short *top;
#endif
{
  YYFPRINTF (stderr, "Stack now");
  for (/* Nothing. */; bottom <= top; ++bottom)
    YYFPRINTF (stderr, " %d", *bottom);
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)				\
do {								\
  if (yydebug)							\
    yy_stack_print ((Bottom), (Top));				\
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

#if defined (__STDC__) || defined (__cplusplus)
static void
yy_reduce_print (int yyrule)
#else
static void
yy_reduce_print (yyrule)
    int yyrule;
#endif
{
  int yyi;
  unsigned int yylineno = yyrline[yyrule];
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %u), ",
             yyrule - 1, yylineno);
  /* Print the symbols being reduced, and their result.  */
  for (yyi = yyprhs[yyrule]; 0 <= yyrhs[yyi]; yyi++)
    YYFPRINTF (stderr, "%s ", yytname [yyrhs[yyi]]);
  YYFPRINTF (stderr, "-> %s\n", yytname [yyr1[yyrule]]);
}

# define YY_REDUCE_PRINT(Rule)		\
do {					\
  if (yydebug)				\
    yy_reduce_print (Rule);		\
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YYDSYMPRINT(Args)
# define YYDSYMPRINTF(Title, Token, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef	YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   SIZE_MAX < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#if YYMAXDEPTH == 0
# undef YYMAXDEPTH
#endif

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif



#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined (__GLIBC__) && defined (_STRING_H)
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
static YYSIZE_T
#   if defined (__STDC__) || defined (__cplusplus)
yystrlen (const char *yystr)
#   else
yystrlen (yystr)
     const char *yystr;
#   endif
{
  register const char *yys = yystr;

  while (*yys++ != '\0')
    continue;

  return yys - yystr - 1;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined (__GLIBC__) && defined (_STRING_H) && defined (_GNU_SOURCE)
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
static char *
#   if defined (__STDC__) || defined (__cplusplus)
yystpcpy (char *yydest, const char *yysrc)
#   else
yystpcpy (yydest, yysrc)
     char *yydest;
     const char *yysrc;
#   endif
{
  register char *yyd = yydest;
  register const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

#endif /* !YYERROR_VERBOSE */



#if YYDEBUG
/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

#if defined (__STDC__) || defined (__cplusplus)
static void
yysymprint (FILE *yyoutput, int yytype, YYSTYPE *yyvaluep)
#else
static void
yysymprint (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE *yyvaluep;
#endif
{
  /* Pacify ``unused variable'' warnings.  */
  (void) yyvaluep;

  if (yytype < YYNTOKENS)
    {
      YYFPRINTF (yyoutput, "token %s (", yytname[yytype]);
# ifdef YYPRINT
      YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# endif
    }
  else
    YYFPRINTF (yyoutput, "nterm %s (", yytname[yytype]);

  switch (yytype)
    {
      default:
        break;
    }
  YYFPRINTF (yyoutput, ")");
}

#endif /* ! YYDEBUG */
/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

#if defined (__STDC__) || defined (__cplusplus)
static void
yydestruct (int yytype, YYSTYPE *yyvaluep)
#else
static void
yydestruct (yytype, yyvaluep)
    int yytype;
    YYSTYPE *yyvaluep;
#endif
{
  /* Pacify ``unused variable'' warnings.  */
  (void) yyvaluep;

  switch (yytype)
    {

      default:
        break;
    }
}


/* Prevent warnings from -Wmissing-prototypes.  */

#ifdef YYPARSE_PARAM
# if defined (__STDC__) || defined (__cplusplus)
int yyparse (void *YYPARSE_PARAM);
# else
int yyparse ();
# endif
#else /* ! YYPARSE_PARAM */
#if defined (__STDC__) || defined (__cplusplus)
int yyparse (void);
#else
int yyparse ();
#endif
#endif /* ! YYPARSE_PARAM */



/* The lookahead symbol.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;

/* Number of syntax errors so far.  */
int yynerrs;



/*----------.
| yyparse.  |
`----------*/

#ifdef YYPARSE_PARAM
# if defined (__STDC__) || defined (__cplusplus)
int yyparse (void *YYPARSE_PARAM)
# else
int yyparse (YYPARSE_PARAM)
  void *YYPARSE_PARAM;
# endif
#else /* ! YYPARSE_PARAM */
#if defined (__STDC__) || defined (__cplusplus)
int
yyparse (void)
#else
int
yyparse ()

#endif
#endif
{
  
  register int yystate;
  register int yyn;
  int yyresult;
  /* Number of tokens to shift before error messages enabled.  */
  int yyerrstatus;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken = 0;

  /* Three stacks and their tools:
     `yyss': related to states,
     `yyvs': related to semantic values,
     `yyls': related to locations.

     Refer to the stacks thru separate pointers, to allow yyoverflow
     to reallocate them elsewhere.  */

  /* The state stack.  */
  short	yyssa[YYINITDEPTH];
  short *yyss = yyssa;
  register short *yyssp;

  /* The semantic value stack.  */
  YYSTYPE yyvsa[YYINITDEPTH];
  YYSTYPE *yyvs = yyvsa;
  register YYSTYPE *yyvsp;



#define YYPOPSTACK   (yyvsp--, yyssp--)

  YYSIZE_T yystacksize = YYINITDEPTH;

  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;


  /* When reducing, the number of symbols on the RHS of the reduced
     rule.  */
  int yylen;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY;		/* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */

  yyssp = yyss;
  yyvsp = yyvs;

  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed. so pushing a state here evens the stacks.
     */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
	/* Give user a chance to reallocate the stack. Use copies of
	   these so that the &'s don't force the real ones into
	   memory.  */
	YYSTYPE *yyvs1 = yyvs;
	short *yyss1 = yyss;


	/* Each stack pointer address is followed by the size of the
	   data in use in that stack, in bytes.  This used to be a
	   conditional around just the two extra args, but that might
	   be undefined if yyoverflow is a macro.  */
	yyoverflow ("parser stack overflow",
		    &yyss1, yysize * sizeof (*yyssp),
		    &yyvs1, yysize * sizeof (*yyvsp),

		    &yystacksize);

	yyss = yyss1;
	yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyoverflowlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
	goto yyoverflowlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
	yystacksize = YYMAXDEPTH;

      {
	short *yyss1 = yyss;
	union yyalloc *yyptr =
	  (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
	if (! yyptr)
	  goto yyoverflowlab;
	YYSTACK_RELOCATE (yyss);
	YYSTACK_RELOCATE (yyvs);

#  undef YYSTACK_RELOCATE
	if (yyss1 != yyssa)
	  YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;


      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
		  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
	YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

/* Do appropriate processing given the current state.  */
/* Read a lookahead token if we need one and don't already have one.  */
/* yyresume: */

  /* First try to decide what to do without reference to lookahead token.  */

  yyn = yypact[yystate];
  if (yyn == YYPACT_NINF)
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = YYLEX;
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YYDSYMPRINTF ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yyn == 0 || yyn == YYTABLE_NINF)
	goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  if (yyn == YYFINAL)
    YYACCEPT;

  /* Shift the lookahead token.  */
  YYDPRINTF ((stderr, "Shifting token %s, ", yytname[yytoken]));

  /* Discard the token being shifted unless it is eof.  */
  if (yychar != YYEOF)
    yychar = YYEMPTY;

  *++yyvsp = yylval;


  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  yystate = yyn;
  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     `$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 4:
#line 144 "scan-fct_pddl.y"
    { fcterr( PROBNAME_EXPECTED, NULL ); ;}
    break;

  case 5:
#line 146 "scan-fct_pddl.y"
    {  
  gproblem_name = yyvsp[-2].pstring;
  if ( gcmd_line.display_info ) {
    fprintf(OUT,"\nproblem '%s' defined\n", gproblem_name);
  }
;}
    break;

  case 6:
#line 157 "scan-fct_pddl.y"
    { 
  yyval.pstring = new_token( strlen(yyvsp[-1].string)+1 );
  strcpy( yyval.pstring, yyvsp[-1].string);
;}
    break;

  case 7:
#line 166 "scan-fct_pddl.y"
    { 
  if (SAME != strcmp(yyvsp[-1].string, gdomain_name))
    {
      fcterr( BADDOMAIN, NULL );
      yyerror();
    }
;}
    break;

  case 13:
#line 190 "scan-fct_pddl.y"
    { 
  FactList * f;
  type_tree root;
  type_tree_list ttl;

  if (NULL != gorig_constant_list)
    {
      for(f = gorig_constant_list; f->next; f = f->next)
	;
      f->next = yyvsp[-1].pFactList;
    }
  else
    {
      gorig_constant_list = yyvsp[-1].pFactList;
    }
  /* maybe new types are introduced here and must be added to the
     type tree */
  for (f = gorig_constant_list; f; f = f->next)
    {
      root = main_type_tree(); /* do not search the EITHER trees */
      if ( !find_branch( f->item->next->item, root ) )
	{ /* type of this constant is not known yet */
	  ttl = new_type_tree_list( f->item->next->item );
	  ttl->next = root->sub_types;
	  root->sub_types = ttl;
	}
    }
;}
    break;

  case 14:
#line 224 "scan-fct_pddl.y"
    { fcterr( INIFACTS, NULL ); ;}
    break;

  case 15:
#line 226 "scan-fct_pddl.y"
    {
  gorig_initial_facts = new_pl_node(AND);
  gorig_initial_facts->sons = yyvsp[-1].pPlNode;
;}
    break;

  case 16:
#line 235 "scan-fct_pddl.y"
    { fcterr( GOALDEF, NULL ); ;}
    break;

  case 17:
#line 237 "scan-fct_pddl.y"
    {
  yyvsp[-1].pPlNode->next = gorig_goal_facts;
  gorig_goal_facts = yyvsp[-1].pPlNode;
;}
    break;

  case 18:
#line 250 "scan-fct_pddl.y"
    { 
  if (TRUE == sis_negated)
    {
      yyval.pPlNode = new_pl_node(NOT);
      yyval.pPlNode->sons = new_pl_node(ATOM);
      yyval.pPlNode->sons->atom = yyvsp[0].pTokenList;
      sis_negated = FALSE;
    }
  else
    {
      yyval.pPlNode = new_pl_node(ATOM);
      yyval.pPlNode->atom = yyvsp[0].pTokenList;
    }
;}
    break;

  case 19:
#line 266 "scan-fct_pddl.y"
    { 
  yyval.pPlNode = new_pl_node(AND);
  yyval.pPlNode->sons = yyvsp[-1].pPlNode;
;}
    break;

  case 20:
#line 272 "scan-fct_pddl.y"
    { 
  yyval.pPlNode = new_pl_node(OR);
  yyval.pPlNode->sons = yyvsp[-1].pPlNode;
;}
    break;

  case 21:
#line 278 "scan-fct_pddl.y"
    { 
  yyval.pPlNode = new_pl_node(NOT);
  yyval.pPlNode->sons = yyvsp[-1].pPlNode;
;}
    break;

  case 22:
#line 284 "scan-fct_pddl.y"
    { 
  PlNode * np = new_pl_node(NOT);
  np->sons = yyvsp[-2].pPlNode;
  np->next = yyvsp[-1].pPlNode;

  yyval.pPlNode = new_pl_node(OR);
  yyval.pPlNode->sons = np;
;}
    break;

  case 23:
#line 296 "scan-fct_pddl.y"
    { 
  /* The typed_list_variable returns a FactList with two-item TokenLists, 
     the first item is the variable and the second item its type.
     We now have to split off this FactList into a PlNode for each 
     variable-type TokenList. */
  FactList * tl = yyvsp[-3].pFactList, *t1;
  PlNode * pln1;
  PlNode * pln2;

  pln1 = new_pl_node(EX);
  pln1->atom = tl->item;
  yyval.pPlNode = pln1;

  t1 = tl;
  /* every loop gives us one quantor with one variable and its type */
  while (NULL != t1->next)
    {
      t1 = t1->next;

      pln2 = new_pl_node(EX);
      pln2->atom = t1->item;
      /* append the next quantor to the sons of the previous node */
      pln1->sons = pln2;
      pln1 = pln2;
    }
  pln1->sons = yyvsp[-1].pPlNode;

  t1 = tl->next;
  while ( TRUE ) {
    free ( tl );
    if ( !t1 ) break;
    tl = t1;
    t1 = tl->next;
  }

;}
    break;

  case 24:
#line 336 "scan-fct_pddl.y"
    { 
  /* This will be handled exactly as the ex-quantor case, s.o. */
  FactList * tl = yyvsp[-3].pFactList, *t1;
  PlNode * pln1;
  PlNode * pln2;
  
  pln1 = new_pl_node(ALL);
  pln1->atom = tl->item;
  yyval.pPlNode = pln1;

  t1 = tl;
  /* every loop gives us one quantor with one variable and its type */
  while (NULL != t1->next)
    {
      t1 = t1->next;
      
      pln2 = new_pl_node(ALL);
      pln2->atom = t1->item;
      pln1->sons = pln2;
      pln1 = pln2;
    }
  pln1->sons = yyvsp[-1].pPlNode;

  t1 = tl->next;
  while ( TRUE ) {
    free ( tl );
    if ( !t1 ) break;
    tl = t1;
    t1 = tl->next;
  }

;}
    break;

  case 25:
#line 373 "scan-fct_pddl.y"
    {
  yyval.pPlNode = NULL;
;}
    break;

  case 26:
#line 378 "scan-fct_pddl.y"
    {
  yyvsp[-1].pPlNode->next = yyvsp[0].pPlNode;
  yyval.pPlNode = yyvsp[-1].pPlNode;
;}
    break;

  case 27:
#line 389 "scan-fct_pddl.y"
    { 
  yyval.pTokenList = yyvsp[-1].pTokenList;
  sis_negated = TRUE;
;}
    break;

  case 28:
#line 395 "scan-fct_pddl.y"
    {
  yyval.pTokenList = yyvsp[0].pTokenList;
;}
    break;

  case 29:
#line 403 "scan-fct_pddl.y"
    { 
  yyval.pTokenList = new_token_list();
  yyval.pTokenList->item = yyvsp[-2].pstring;
  yyval.pTokenList->next = yyvsp[-1].pTokenList;
;}
    break;

  case 30:
#line 413 "scan-fct_pddl.y"
    {
  yyval.pTokenList = NULL;
;}
    break;

  case 31:
#line 418 "scan-fct_pddl.y"
    {
  yyval.pTokenList = new_token_list();
  yyval.pTokenList->item = yyvsp[-1].pstring;
  yyval.pTokenList->next = yyvsp[0].pTokenList;
;}
    break;

  case 32:
#line 428 "scan-fct_pddl.y"
    { 
  yyval.pstring = new_token(strlen(yyvsp[0].string) + 1);
  strcpy(yyval.pstring, yyvsp[0].string);
;}
    break;

  case 33:
#line 434 "scan-fct_pddl.y"
    { 
  yyval.pstring = new_token(strlen(yyvsp[0].string) + 1);
  strcpy(yyval.pstring, yyvsp[0].string);
;}
    break;

  case 34:
#line 443 "scan-fct_pddl.y"
    {
  yyval.pTokenList = new_token_list();
  yyval.pTokenList->item = new_token(strlen(yyvsp[0].string) + 1);
  strcpy(yyval.pTokenList->item, yyvsp[0].string);
;}
    break;

  case 35:
#line 450 "scan-fct_pddl.y"
    {
  yyval.pTokenList = new_token_list();
  yyval.pTokenList->item = new_token(strlen(yyvsp[-1].string) + 1);
  strcpy(yyval.pTokenList->item, yyvsp[-1].string);
  yyval.pTokenList->next = yyvsp[0].pTokenList;
;}
    break;

  case 36:
#line 461 "scan-fct_pddl.y"
    { yyval.pFactList = NULL; ;}
    break;

  case 37:
#line 464 "scan-fct_pddl.y"
    { /* this is a very, very special case... it may mean that
     a. a parameter has one of the types in name_plus
     b. a type is subtype of one of the following types in name_plus 
     => for both possibilities we use the same solution:
     build a new type name: either_name1_name2_..._namen and check
     if a type with this name already exists. If so then NAME has this type,
     if not then build a new type, include it in the type tree and
     return NAME with the new type 
     The new artificial type is not a subtype of OBJECT because its own
     elements must already be instances of a subtype of OBJECT */
  char * s;
  TokenList * t;
  type_tree tt;
  type_tree_list rootl, *st;
  
  s = new_token( MAX_LENGTH );
  strcpy( s, EITHER_STR );
  for ( t = yyvsp[-2].pTokenList; t; t = t->next )
    {
      strcat( s, CONNECTOR );
      strcat( s, t->item );
    }
  tt = NULL;
  for ( rootl = gglobal_type_tree_list; rootl; rootl = rootl->next )
    {
      if ((tt = find_branch( s, rootl->item )))
	break;
    }

  if ( !tt )
    { /* the type doesn't exist yet, so build it */
      rootl = new_type_tree_list( s );
      rootl->next = gglobal_type_tree_list;
      st = &(rootl->item->sub_types);
      for ( t = yyvsp[-2].pTokenList; t; t = t->next )
	{
	  if ((tt = find_branch(t->item, main_type_tree())))
	    {
	      *st = new_type_tree_list( NULL );
	      (*st)->item = tt;
	      st = &((*st)->next);
	    }
	}  
    }

  /* now do the simple stuff: return a name with a (quite complicated)
     type */
  yyval.pFactList = new_fact_list();
  yyval.pFactList->item = new_token_list();
  yyval.pFactList->item->item = new_token( strlen(yyvsp[-4].string)+1 );
  strcpy( yyval.pFactList->item->item, yyvsp[-4].string );
  yyval.pFactList->item->next = new_token_list();
  yyval.pFactList->item->next->item = s;
  yyval.pFactList->next = yyvsp[0].pFactList;
;}
    break;

  case 38:
#line 521 "scan-fct_pddl.y"
    {
  yyval.pFactList = new_fact_list();
  yyval.pFactList->item = new_token_list();
  yyval.pFactList->item->item = new_token( strlen(yyvsp[-2].string)+1 );
  strcpy( yyval.pFactList->item->item, yyvsp[-2].string );
  yyval.pFactList->item->next = new_token_list();
  yyval.pFactList->item->next->item = new_token( strlen(yyvsp[-1].string)+1 );
  strcpy( yyval.pFactList->item->next->item, yyvsp[-1].string );
  yyval.pFactList->next = yyvsp[0].pFactList;
;}
    break;

  case 39:
#line 533 "scan-fct_pddl.y"
    {
  yyval.pFactList = new_fact_list();
  yyval.pFactList->item = new_token_list();
  yyval.pFactList->item->item = new_token( strlen(yyvsp[-1].string)+1 );
  strcpy( yyval.pFactList->item->item, yyvsp[-1].string );
  yyval.pFactList->item->next = new_token_list();
  if ( yyvsp[0].pFactList )   /* another element (already typed) is following */
    {
      char* s = yyvsp[0].pFactList->item->next->item;
      int   l = strlen( s ) + 1;
      yyval.pFactList->item->next->item = new_token( l );
      strcpy( yyval.pFactList->item->next->item, s ); /* same type as the next one */
      yyval.pFactList->next = yyvsp[0].pFactList;
    }
  else /* no further element - it must be an untyped list */
    {
      yyval.pFactList->item->next->item = new_token( strlen(STANDARD_TYPE)+1 );
      strcpy( yyval.pFactList->item->next->item, STANDARD_TYPE );
      yyval.pFactList->next = yyvsp[0].pFactList;
    }
;}
    break;

  case 40:
#line 559 "scan-fct_pddl.y"
    { yyval.pFactList = NULL; ;}
    break;

  case 41:
#line 562 "scan-fct_pddl.y"
    { /* this is a very, very special case... it may mean that
     a parameter has one of the types in name_plus
     => build a new type name: either_name1_name2_..._namen and check
     if a type with this name already exists. If so then NAME has this type,
     if not then build a new type, include it in the type tree and
     return NAME with the new type 
     The new artificial type is not a subtype of OBJECT because its own
     elements must already be instances of a subtype of OBJECT */
  char * s;
  TokenList * t;
  type_tree tt;
  type_tree_list rootl, *st;
  
  s = new_token( MAX_LENGTH );
  strcpy( s, EITHER_STR );
  for ( t = yyvsp[-2].pTokenList; t; t = t->next )
    {
      strcat( s, CONNECTOR );
      strcat( s, t->item );
    }
  tt = NULL;
  for ( rootl = gglobal_type_tree_list; rootl; rootl = rootl->next )
    if ((tt = find_branch( s, rootl->item)))
      break;
  if ( !tt )
    { /* the type doesn't exist yet, so build it */
      rootl = new_type_tree_list( s );
      rootl->next = gglobal_type_tree_list;
      st = &(rootl->item->sub_types);
      for ( t = yyvsp[-2].pTokenList; t; t = t->next )
	{
	  if ((tt = find_branch( t->item, main_type_tree())))
	    {
	      *st = new_type_tree_list( NULL );
	      (*st)->item = tt;
	      st = &((*st)->next);
	    }
	}  
      gglobal_type_tree_list = rootl;
    }

  /* now do the simple stuff: return a name with a (quite complicated)
     type */
  yyval.pFactList = new_fact_list();
  yyval.pFactList->item = new_token_list();
  yyval.pFactList->item->item = new_token( strlen(yyvsp[-4].string)+1 );
  strcpy( yyval.pFactList->item->item, yyvsp[-4].string );
  yyval.pFactList->item->next = new_token_list();
  yyval.pFactList->item->next->item = s;
  yyval.pFactList->next = yyvsp[0].pFactList;
;}
    break;

  case 42:
#line 615 "scan-fct_pddl.y"
    {
  yyval.pFactList = new_fact_list();
  yyval.pFactList->item = new_token_list();
  yyval.pFactList->item->item = new_token( strlen(yyvsp[-2].string)+1 );
  strcpy( yyval.pFactList->item->item, yyvsp[-2].string );
  yyval.pFactList->item->next = new_token_list();
  yyval.pFactList->item->next->item = new_token( strlen(yyvsp[-1].string)+1 );
  strcpy( yyval.pFactList->item->next->item, yyvsp[-1].string );
  yyval.pFactList->next = yyvsp[0].pFactList;
;}
    break;

  case 43:
#line 627 "scan-fct_pddl.y"
    {
  yyval.pFactList = new_fact_list();
  yyval.pFactList->item = new_token_list();
  yyval.pFactList->item->item = new_token( strlen(yyvsp[-1].string)+1 );
  strcpy( yyval.pFactList->item->item, yyvsp[-1].string );
  yyval.pFactList->item->next = new_token_list();
  if ( yyvsp[0].pFactList )   /* another element (already typed) is following */
    {
      char* s = yyvsp[0].pFactList->item->next->item;
      int   l = strlen( s );
      yyval.pFactList->item->next->item = new_token( l+1 );
      strcpy( yyval.pFactList->item->next->item, s ); /* same type as the next one */
      yyval.pFactList->next = yyvsp[0].pFactList;
    }
  else /* no further element - it must be an untyped list */
    {
      yyval.pFactList->item->next->item = new_token( strlen(STANDARD_TYPE)+1 );
      strcpy( yyval.pFactList->item->next->item, STANDARD_TYPE );
      yyval.pFactList->next = yyvsp[0].pFactList;
    }
;}
    break;

  case 44:
#line 653 "scan-fct_pddl.y"
    { 
  yyval.pstring = new_token(strlen(yyvsp[0].string) + 1);
  strcpy(yyval.pstring, yyvsp[0].string);
;}
    break;

  case 45:
#line 659 "scan-fct_pddl.y"
    { 
  yyval.pstring = new_token( strlen(EQ_STR)+1 );
  strcpy( yyval.pstring, EQ_STR );
;}
    break;

  case 46:
#line 668 "scan-fct_pddl.y"
    {
  yyval.pPlNode = yyvsp[0].pPlNode;
;}
    break;

  case 47:
#line 673 "scan-fct_pddl.y"
    {
   yyval.pPlNode = yyvsp[-1].pPlNode;
   yyval.pPlNode->next = yyvsp[0].pPlNode;
;}
    break;

  case 48:
#line 682 "scan-fct_pddl.y"
    { 
  PlNode * tmp;

  tmp = new_pl_node(ATOM);
  tmp->atom = yyvsp[-1].pTokenList;
  yyval.pPlNode = new_pl_node(NOT);
  yyval.pPlNode->sons = tmp;
;}
    break;

  case 49:
#line 692 "scan-fct_pddl.y"
    {
  yyval.pPlNode = new_pl_node(ATOM);
  yyval.pPlNode->atom = yyvsp[0].pTokenList;
;}
    break;

  case 50:
#line 701 "scan-fct_pddl.y"
    { 
  yyval.pTokenList = new_token_list();
  yyval.pTokenList->item = yyvsp[-2].pstring;
  yyval.pTokenList->next = yyvsp[-1].pTokenList;
;}
    break;

  case 51:
#line 711 "scan-fct_pddl.y"
    { yyval.pTokenList = NULL; ;}
    break;

  case 52:
#line 714 "scan-fct_pddl.y"
    {
  yyval.pTokenList = new_token_list();
  yyval.pTokenList->item = new_token(strlen(yyvsp[-1].string) + 1);
  strcpy(yyval.pTokenList->item, yyvsp[-1].string);
  yyval.pTokenList->next = yyvsp[0].pTokenList;
;}
    break;


    }

/* Line 991 of yacc.c.  */
#line 1786 "scan-fct_pddl.tab.c"

  yyvsp -= yylen;
  yyssp -= yylen;


  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;


  /* Now `shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*------------------------------------.
| yyerrlab -- here on detecting error |
`------------------------------------*/
yyerrlab:
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if YYERROR_VERBOSE
      yyn = yypact[yystate];

      if (YYPACT_NINF < yyn && yyn < YYLAST)
	{
	  YYSIZE_T yysize = 0;
	  int yytype = YYTRANSLATE (yychar);
	  char *yymsg;
	  int yyx, yycount;

	  yycount = 0;
	  /* Start YYX at -YYN if negative to avoid negative indexes in
	     YYCHECK.  */
	  for (yyx = yyn < 0 ? -yyn : 0;
	       yyx < (int) (sizeof (yytname) / sizeof (char *)); yyx++)
	    if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR)
	      yysize += yystrlen (yytname[yyx]) + 15, yycount++;
	  yysize += yystrlen ("syntax error, unexpected ") + 1;
	  yysize += yystrlen (yytname[yytype]);
	  yymsg = (char *) YYSTACK_ALLOC (yysize);
	  if (yymsg != 0)
	    {
	      char *yyp = yystpcpy (yymsg, "syntax error, unexpected ");
	      yyp = yystpcpy (yyp, yytname[yytype]);

	      if (yycount < 5)
		{
		  yycount = 0;
		  for (yyx = yyn < 0 ? -yyn : 0;
		       yyx < (int) (sizeof (yytname) / sizeof (char *));
		       yyx++)
		    if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR)
		      {
			const char *yyq = ! yycount ? ", expecting " : " or ";
			yyp = yystpcpy (yyp, yyq);
			yyp = yystpcpy (yyp, yytname[yyx]);
			yycount++;
		      }
		}
	      yyerror (yymsg);
	      YYSTACK_FREE (yymsg);
	    }
	  else
	    yyerror ("syntax error; also virtual memory exhausted");
	}
      else
#endif /* YYERROR_VERBOSE */
	yyerror ("syntax error");
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
	 error, discard it.  */

      /* Return failure if at end of input.  */
      if (yychar == YYEOF)
        {
	  /* Pop the error token.  */
          YYPOPSTACK;
	  /* Pop the rest of the stack.  */
	  while (yyss < yyssp)
	    {
	      YYDSYMPRINTF ("Error: popping", yystos[*yyssp], yyvsp, yylsp);
	      yydestruct (yystos[*yyssp], yyvsp);
	      YYPOPSTACK;
	    }
	  YYABORT;
        }

      YYDSYMPRINTF ("Error: discarding", yytoken, &yylval, &yylloc);
      yydestruct (yytoken, &yylval);
      yychar = YYEMPTY;

    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab2;


/*----------------------------------------------------.
| yyerrlab1 -- error raised explicitly by an action.  |
`----------------------------------------------------*/
yyerrlab1:

  /* Suppress GCC warning that yyerrlab1 is unused when no action
     invokes YYERROR.  */
#if defined (__GNUC_MINOR__) && 2093 <= (__GNUC__ * 1000 + __GNUC_MINOR__)
  __attribute__ ((__unused__))
#endif


  goto yyerrlab2;


/*---------------------------------------------------------------.
| yyerrlab2 -- pop states until the error token can be shifted.  |
`---------------------------------------------------------------*/
yyerrlab2:
  yyerrstatus = 3;	/* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (yyn != YYPACT_NINF)
	{
	  yyn += YYTERROR;
	  if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
	    {
	      yyn = yytable[yyn];
	      if (0 < yyn)
		break;
	    }
	}

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
	YYABORT;

      YYDSYMPRINTF ("Error: popping", yystos[*yyssp], yyvsp, yylsp);
      yydestruct (yystos[yystate], yyvsp);
      yyvsp--;
      yystate = *--yyssp;

      YY_STACK_PRINT (yyss, yyssp);
    }

  if (yyn == YYFINAL)
    YYACCEPT;

  YYDPRINTF ((stderr, "Shifting error token, "));

  *++yyvsp = yylval;


  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#ifndef yyoverflow
/*----------------------------------------------.
| yyoverflowlab -- parser overflow comes here.  |
`----------------------------------------------*/
yyoverflowlab:
  yyerror ("parser stack overflow");
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
  return yyresult;
}


#line 714 "scan-fct_pddl.y"


#include "lex.fct_pddl.c"

/**********************************************************************
 * Functions
 **********************************************************************/

/* 
call	bison -pfct -bscan-fct scan-fct.y
*/
void fcterr( int errno, char *par )
{
/*   sact_err = errno; */
/*   if ( sact_err_par ) */
/*     free( sact_err_par ); */
/*   if ( par ) */
/*     { */
/*       sact_err_par = new_token( strlen(par)+1 ); */
/*       strcpy( sact_err_par, par); */
/*     } */
/*   else */
/*     sact_err_par = NULL; */
}

int yyerror( char *msg )
{
  fflush( stdout );
  fprintf(stderr,"\n%s: syntax error in line %d, '%s':\n", 
	  gact_filename, lineno, yytext );
  if ( sact_err_par )
    {
      fprintf(stderr, "%s%s\n", serrmsg[sact_err], sact_err_par );
    }
  else
    fprintf(stderr,"%s\n", serrmsg[sact_err] );

  switch (sact_err)
    {
    case NOT_SUPPORTED:
      exit( NOT_SUPPORTED_ERROR_CODE );
    case BADDOMAIN:
      exit( BADDOMAIN_ERROR_CODE );
    default:
      exit( FCT_PARSE_ERROR_CODE );
     }
}


void load_fct_file( char *filename )
{
  FILE *fp;/* pointer to input files */
  char tmp[MAX_LENGTH] = "";

  /* open fact file */
  if( ( fp = fopen( filename, "r" ) ) == NULL ) 
    {
      sprintf(tmp, "\nipp: can't find fact file: %s\n\n", filename );
      perror(tmp);
      exit( FCT_MISSING_ERROR_CODE );
    }
  gact_filename = filename;
  lineno = 1; 
  yyin = fp;
  yyparse();
  fclose( fp );/* and close file again */
}

/* void  */
/* get_fct_file_name(char * filename) */
/* { */
/*   FILE *fp;/* pointer to input files */ 
/*   char tmp[MAX_LENGTH] = ""; */

/*   /* open fact file */ 
/*   if( ( fp = fopen( filename, "r" ) ) == NULL )  */
/*     { */
/*       sprintf(tmp, "\nipp: can't find fact file: %s\n\n", filename ); */
/*       perror(tmp); */
/*       output_time(IO_ERROR); */
/*       exit( FCT_MISSING_ERROR_CODE ); */
/*     } */
/*   gact_filename = filename; */
/*   lineno = 1;  */
/*   yyin = fp; */
/*   yyparse(); */
/*   fclose( fp );/* and close file again */ 
/* } */



