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
#define yyparse ops_pddlparse
#define yylex   ops_pddllex
#define yyerror ops_pddlerror
#define yylval  ops_pddllval
#define yychar  ops_pddlchar
#define yydebug ops_pddldebug
#define yynerrs ops_pddlnerrs


/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     DEFINE_TOK = 258,
     DOMAIN_TOK = 259,
     REQUIREMENTS_TOK = 260,
     TYPES_TOK = 261,
     EITHER_TOK = 262,
     CONSTANTS_TOK = 263,
     ACTION_TOK = 264,
     AXIOM_TOK = 265,
     VARS_TOK = 266,
     CONTEXT_TOK = 267,
     IMPLIES_TOK = 268,
     PRECONDITION_TOK = 269,
     PARAMETERS_TOK = 270,
     PREDICATES_TOK = 271,
     EFFECT_TOK = 272,
     AND_TOK = 273,
     NOT_TOK = 274,
     WHEN_TOK = 275,
     FORALL_TOK = 276,
     IMPLY_TOK = 277,
     OR_TOK = 278,
     EXISTS_TOK = 279,
     EQUAL_TOK = 280,
     NAME = 281,
     VARIABLE = 282,
     TYPE = 283,
     OPEN_PAREN = 284,
     CLOSE_PAREN = 285
   };
#endif
#define DEFINE_TOK 258
#define DOMAIN_TOK 259
#define REQUIREMENTS_TOK 260
#define TYPES_TOK 261
#define EITHER_TOK 262
#define CONSTANTS_TOK 263
#define ACTION_TOK 264
#define AXIOM_TOK 265
#define VARS_TOK 266
#define CONTEXT_TOK 267
#define IMPLIES_TOK 268
#define PRECONDITION_TOK 269
#define PARAMETERS_TOK 270
#define PREDICATES_TOK 271
#define EFFECT_TOK 272
#define AND_TOK 273
#define NOT_TOK 274
#define WHEN_TOK 275
#define FORALL_TOK 276
#define IMPLY_TOK 277
#define OR_TOK 278
#define EXISTS_TOK 279
#define EQUAL_TOK 280
#define NAME 281
#define VARIABLE 282
#define TYPE 283
#define OPEN_PAREN 284
#define CLOSE_PAREN 285




/* Copy the first part of user declarations.  */
#line 1 "scan-ops_pddl.y"

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
#define DOMDEF_EXPECTED            0
#define DOMAIN_EXPECTED            1
#define DOMNAME_EXPECTED           2
#define LBRACKET_EXPECTED          3
#define RBRACKET_EXPECTED          4
#define DOMDEFS_EXPECTED           5
#define REQUIREM_EXPECTED          6
#define TYPEDLIST_EXPECTED         7
#define LITERAL_EXPECTED           8
#define PRECONDDEF_UNCORRECT       9
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
#define ACTION                    20
#endif

#define NAME_STR "name\0"
#define VARIABLE_STR "variable\0"
#define STANDARD_TYPE "OBJECT\0"
 
static char * serrmsg[] = {
  "domain definition expected",
  "'domain' expected",
  "domain name expected",
  "'(' expected",
  "')' expected",
  "additional domain definitions expected",
  "requirements (e.g. ':STRIPS') expected",
  "typed list of <%s> expected",
  "literal expected",
  "uncorrect precondition definition",
  "type definition expected",
  "list of constants expected",
  "predicate definition expected",
  "<name> expected",
  "<variable> expected",
  "action functor expected",
  "atomic formula expected",
  "effect definition expected",
  "negated atomic formula expected",
  "requirement '%s' not supported by this IPP version",  
  "action definition is not correct",
  NULL
};
 
/* void opserr(int errno, char * par); */

static int sact_err;
static char * sact_err_par = NULL;
static PlOperator * scur_op = NULL;
static Bool sis_negated = FALSE;


int 
supported(char * str)
{
  int i;
  char * sup[] = { ":STRIPS", ":NEGATION", ":EQUALITY",":TYPING", 
		   ":CONDITIONAL-EFFECTS", ":DISJUNCTIVE-PRECONDITIONS", 
		   ":EXISTENTIAL-PRECONDITIONS", ":UNIVERSAL-PRECONDITIONS", 
		   ":QUANTIFIED-PRECONDITIONS", ":ADL",
		   ":DOMAIN-AXIOMS", ":SUBGOAL-THROUGH-AXIOMS",
		   NULL };     

  for (i=0; NULL != sup[i]; i++)
    {
      if (SAME == strcmp(sup[i], str))
	{
	  return TRUE;
	}
    }
  return FALSE;
}



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
#line 101 "scan-ops_pddl.y"
typedef union YYSTYPE {
  char string[MAX_LENGTH];
  char* pstring;
  PlNode* pPlNode;
  FactList* pFactList;
  TokenList* pTokenList;
} YYSTYPE;
/* Line 191 of yacc.c.  */
#line 250 "scan-ops_pddl.tab.c"
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif



/* Copy the second part of user declarations.  */


/* Line 214 of yacc.c.  */
#line 262 "scan-ops_pddl.tab.c"

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
#define YYFINAL  3
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   147

/* YYNTOKENS -- Number of terminals. */
#define YYNTOKENS  31
/* YYNNTS -- Number of nonterminals. */
#define YYNNTS  44
/* YYNRULES -- Number of rules. */
#define YYNRULES  79
/* YYNRULES -- Number of states. */
#define YYNSTATES  176

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   285

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
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const unsigned short yyprhs[] =
{
       0,     0,     3,     4,     7,     8,    14,    19,    21,    24,
      27,    30,    33,    36,    39,    40,    46,    47,    48,    55,
      56,    57,    65,    66,    67,    71,    72,    78,    79,    85,
      86,    87,    96,    97,   102,   103,   109,   110,   115,   116,
     121,   122,   123,   124,   125,   141,   143,   148,   153,   158,
     164,   172,   180,   181,   184,   186,   191,   196,   204,   210,
     211,   214,   219,   221,   226,   227,   230,   232,   234,   236,
     239,   241,   243,   244,   250,   254,   257,   258,   264,   268
};

/* YYRHS -- A `-1'-separated list of the rules' RHS. */
static const yysigned_char yyrhs[] =
{
      32,     0,    -1,    -1,    33,    34,    -1,    -1,    29,     3,
      36,    35,    37,    -1,    29,     4,    26,    30,    -1,    30,
      -1,    42,    37,    -1,    49,    37,    -1,    47,    37,    -1,
      58,    37,    -1,    51,    37,    -1,    38,    37,    -1,    -1,
      29,    16,    40,    39,    30,    -1,    -1,    -1,    29,    26,
      74,    30,    41,    40,    -1,    -1,    -1,    29,     5,    43,
      26,    44,    45,    30,    -1,    -1,    -1,    26,    46,    45,
      -1,    -1,    29,     6,    48,    73,    30,    -1,    -1,    29,
       8,    50,    73,    30,    -1,    -1,    -1,    29,     9,    52,
      26,    53,    54,    55,    30,    -1,    -1,    15,    29,    74,
      30,    -1,    -1,    11,    29,    74,    30,    55,    -1,    -1,
      14,    63,    56,    55,    -1,    -1,    17,    65,    57,    55,
      -1,    -1,    -1,    -1,    -1,    29,    10,    59,    11,    29,
      74,    30,    60,    12,    63,    61,    13,    67,    62,    30,
      -1,    67,    -1,    29,    18,    64,    30,    -1,    29,    23,
      64,    30,    -1,    29,    19,    63,    30,    -1,    29,    22,
      63,    63,    30,    -1,    29,    24,    29,    74,    30,    63,
      30,    -1,    29,    21,    29,    74,    30,    63,    30,    -1,
      -1,    63,    64,    -1,    67,    -1,    29,    18,    66,    30,
      -1,    29,    19,    65,    30,    -1,    29,    21,    29,    74,
      30,    65,    30,    -1,    29,    20,    63,    65,    30,    -1,
      -1,    65,    66,    -1,    29,    19,    68,    30,    -1,    68,
      -1,    29,    72,    69,    30,    -1,    -1,    70,    69,    -1,
      26,    -1,    27,    -1,    26,    -1,    26,    71,    -1,    26,
      -1,    25,    -1,    -1,    26,     7,    71,    30,    73,    -1,
      26,    28,    73,    -1,    26,    73,    -1,    -1,    27,     7,
      71,    30,    74,    -1,    27,    28,    74,    -1,    27,    74,
      -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const unsigned short yyrline[] =
{
       0,   157,   157,   157,   165,   164,   179,   188,   190,   192,
     194,   196,   198,   200,   209,   208,   218,   221,   220,   260,
     262,   259,   273,   277,   276,   290,   289,   301,   300,   312,
     313,   312,   326,   328,   341,   344,   365,   364,   371,   370,
     383,   388,   392,   396,   382,   432,   448,   454,   460,   466,
     476,   516,   556,   560,   572,   588,   594,   600,   636,   653,
     657,   668,   678,   686,   697,   699,   709,   715,   724,   731,
     742,   748,   758,   760,   823,   835,   861,   863,   924,   936
};
#endif

#if YYDEBUG || YYERROR_VERBOSE
/* YYTNME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals. */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "DEFINE_TOK", "DOMAIN_TOK", 
  "REQUIREMENTS_TOK", "TYPES_TOK", "EITHER_TOK", "CONSTANTS_TOK", 
  "ACTION_TOK", "AXIOM_TOK", "VARS_TOK", "CONTEXT_TOK", "IMPLIES_TOK", 
  "PRECONDITION_TOK", "PARAMETERS_TOK", "PREDICATES_TOK", "EFFECT_TOK", 
  "AND_TOK", "NOT_TOK", "WHEN_TOK", "FORALL_TOK", "IMPLY_TOK", "OR_TOK", 
  "EXISTS_TOK", "EQUAL_TOK", "NAME", "VARIABLE", "TYPE", "OPEN_PAREN", 
  "CLOSE_PAREN", "$accept", "file", "@1", "domain_definition", "@2", 
  "domain_name", "optional_domain_defs", "predicates_def", "@3", 
  "predicates_list", "@4", "require_def", "@5", "@6", "require_key_star", 
  "@7", "types_def", "@8", "constants_def", "@9", "action_def", "@10", 
  "@11", "param_def", "action_def_body", "@12", "@13", "axiom_def", "@14", 
  "@15", "@16", "@17", "adl_goal_description", 
  "adl_goal_description_star", "adl_effect", "adl_effect_star", 
  "literal_term", "atomic_formula_term", "term_star", "term", "name_plus", 
  "predicate", "typed_list_name", "typed_list_variable", 0
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[YYLEX-NUM] -- Internal token number corresponding to
   token YYLEX-NUM.  */
static const unsigned short yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const unsigned char yyr1[] =
{
       0,    31,    33,    32,    35,    34,    36,    37,    37,    37,
      37,    37,    37,    37,    39,    38,    40,    41,    40,    43,
      44,    42,    45,    46,    45,    48,    47,    50,    49,    52,
      53,    51,    54,    54,    55,    55,    56,    55,    57,    55,
      59,    60,    61,    62,    58,    63,    63,    63,    63,    63,
      63,    63,    64,    64,    65,    65,    65,    65,    65,    66,
      66,    67,    67,    68,    69,    69,    70,    70,    71,    71,
      72,    72,    73,    73,    73,    73,    74,    74,    74,    74
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const unsigned char yyr2[] =
{
       0,     2,     0,     2,     0,     5,     4,     1,     2,     2,
       2,     2,     2,     2,     0,     5,     0,     0,     6,     0,
       0,     7,     0,     0,     3,     0,     5,     0,     5,     0,
       0,     8,     0,     4,     0,     5,     0,     4,     0,     4,
       0,     0,     0,     0,    15,     1,     4,     4,     4,     5,
       7,     7,     0,     2,     1,     4,     4,     7,     5,     0,
       2,     4,     1,     4,     0,     2,     1,     1,     1,     2,
       1,     1,     0,     5,     3,     2,     0,     5,     3,     2
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const unsigned char yydefact[] =
{
       2,     0,     0,     1,     0,     3,     0,     0,     4,     0,
       0,     0,     0,     7,     5,     0,     0,     0,     0,     0,
       0,     6,    19,    25,    27,    29,    40,    16,    13,     8,
      10,     9,    12,    11,     0,    72,    72,     0,     0,     0,
      14,    20,    72,     0,     0,    30,     0,    76,     0,    22,
       0,    72,    75,    26,    28,    32,    76,    76,     0,    15,
      23,     0,    68,     0,    74,     0,    34,     0,     0,    76,
      79,    17,    22,    21,    69,    72,    76,     0,     0,     0,
       0,    41,     0,    78,    16,    24,    73,     0,    76,     0,
      36,    45,    62,     0,    38,    54,    31,     0,    76,    18,
      33,     0,    52,     0,     0,     0,    52,     0,    71,    70,
      64,    34,    59,     0,     0,     0,    34,     0,    77,    34,
      52,     0,     0,     0,    76,     0,     0,    76,    66,    67,
       0,    64,    37,    59,     0,     0,     0,    76,    39,    42,
      35,    53,    46,    48,    61,     0,     0,    47,     0,    63,
      65,    60,    55,    56,     0,     0,     0,     0,    49,     0,
      58,     0,     0,     0,     0,     0,     0,    43,    51,    50,
      57,     0,     0,     0,     0,    44
};

/* YYDEFGOTO[NTERM-NUM]. */
static const short yydefgoto[] =
{
      -1,     1,     2,     5,    10,     8,    14,    15,    48,    40,
      84,    16,    34,    49,    61,    72,    17,    35,    18,    36,
      19,    37,    55,    66,    80,   111,   116,    20,    38,    97,
     156,   172,   120,   121,   133,   134,    91,    92,   130,   131,
      63,   110,    43,    58
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -103
static const yysigned_char yypact[] =
{
    -103,    16,    -8,  -103,    36,  -103,    13,    42,  -103,    22,
       1,    23,    56,  -103,  -103,     1,     1,     1,     1,     1,
       1,  -103,  -103,  -103,  -103,  -103,  -103,    26,  -103,  -103,
    -103,  -103,  -103,  -103,    30,    32,    32,    37,    57,    41,
    -103,  -103,    -4,    40,    44,  -103,    46,    50,    48,    45,
      53,    32,  -103,  -103,  -103,    65,    50,     0,    51,  -103,
    -103,    55,    53,    58,  -103,    60,    15,    61,    53,    50,
    -103,  -103,    45,  -103,  -103,    32,    50,    64,    68,    79,
      80,  -103,    89,  -103,    26,  -103,  -103,    90,    50,    77,
    -103,  -103,  -103,    86,  -103,  -103,  -103,    75,    50,  -103,
    -103,    91,    68,    68,    93,    68,    68,    94,  -103,  -103,
      18,    15,    79,    79,    68,    95,    15,    68,  -103,    15,
      68,    96,    97,    99,    50,    68,   100,    50,  -103,  -103,
     101,    18,  -103,    79,   102,   103,    79,    50,  -103,  -103,
    -103,  -103,  -103,  -103,  -103,   104,   105,  -103,   106,  -103,
    -103,  -103,  -103,  -103,   107,   108,   112,    68,  -103,    68,
    -103,    79,   110,   111,   113,   114,   -11,  -103,  -103,  -103,
    -103,   116,   117,    25,    99,  -103
};

/* YYPGOTO[NTERM-NUM].  */
static const yysigned_char yypgoto[] =
{
    -103,  -103,  -103,  -103,  -103,  -103,    98,  -103,  -103,     6,
    -103,  -103,  -103,  -103,    70,  -103,  -103,  -103,  -103,  -103,
    -103,  -103,  -103,  -103,   -99,  -103,  -103,  -103,  -103,  -103,
    -103,  -103,   -65,   -97,   -77,   -24,   -79,  -102,    -3,  -103,
     -27,  -103,   -32,   -51
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -1
static const unsigned char yytable[] =
{
      95,   123,    94,    50,    44,    67,    70,    68,   171,   126,
      52,   123,   132,    90,   108,   109,     3,   138,    83,    64,
     140,     4,    42,   141,    51,    87,    77,    57,    69,    78,
      12,    13,    79,    95,    95,    74,   135,   101,   122,     6,
     125,    82,     7,    86,   128,   129,     9,   118,    11,   136,
     108,   109,   139,    21,    95,    39,    41,    95,    42,   154,
     146,    22,    23,    45,    24,    25,    26,    47,    46,   174,
      53,    60,    27,   145,    54,    56,   148,    57,    59,    62,
      65,    71,    95,   167,   165,    73,   155,   117,    75,    76,
      99,    81,   163,    88,   164,   102,   103,    89,   104,   105,
     106,   107,   108,   109,   112,   113,   114,   115,    93,   151,
      96,   108,   109,    28,    29,    30,    31,    32,    33,    98,
     100,   119,   124,   127,   137,   162,   142,   143,   150,   144,
     147,   149,   152,   153,   157,   158,   159,   160,   161,   166,
       0,   168,    85,   169,   170,   173,     0,   175
};

static const short yycheck[] =
{
      79,   103,    79,     7,    36,    56,    57,     7,    19,   106,
      42,   113,   111,    78,    25,    26,     0,   116,    69,    51,
     119,    29,    26,   120,    28,    76,    11,    27,    28,    14,
      29,    30,    17,   112,   113,    62,   113,    88,   103,     3,
     105,    68,    29,    75,    26,    27,     4,    98,    26,   114,
      25,    26,   117,    30,   133,    29,    26,   136,    26,   136,
     125,     5,     6,    26,     8,     9,    10,    26,    11,   171,
      30,    26,    16,   124,    30,    29,   127,    27,    30,    26,
      15,    30,   161,   162,   161,    30,   137,    12,    30,    29,
      84,    30,   157,    29,   159,    18,    19,    29,    21,    22,
      23,    24,    25,    26,    18,    19,    20,    21,    29,   133,
      30,    25,    26,    15,    16,    17,    18,    19,    20,    30,
      30,    30,    29,    29,    29,    13,    30,    30,   131,    30,
      30,    30,    30,    30,    30,    30,    30,    30,    30,    29,
      -1,    30,    72,    30,    30,    29,    -1,    30
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const unsigned char yystos[] =
{
       0,    32,    33,     0,    29,    34,     3,    29,    36,     4,
      35,    26,    29,    30,    37,    38,    42,    47,    49,    51,
      58,    30,     5,     6,     8,     9,    10,    16,    37,    37,
      37,    37,    37,    37,    43,    48,    50,    52,    59,    29,
      40,    26,    26,    73,    73,    26,    11,    26,    39,    44,
       7,    28,    73,    30,    30,    53,    29,    27,    74,    30,
      26,    45,    26,    71,    73,    15,    54,    74,     7,    28,
      74,    30,    46,    30,    71,    30,    29,    11,    14,    17,
      55,    30,    71,    74,    41,    45,    73,    74,    29,    29,
      63,    67,    68,    29,    65,    67,    30,    60,    30,    40,
      30,    74,    18,    19,    21,    22,    23,    24,    25,    26,
      72,    56,    18,    19,    20,    21,    57,    12,    74,    30,
      63,    64,    63,    68,    29,    63,    64,    29,    26,    27,
      69,    70,    55,    65,    66,    65,    63,    29,    55,    63,
      55,    64,    30,    30,    30,    74,    63,    30,    74,    30,
      69,    66,    30,    30,    65,    74,    61,    30,    30,    30,
      30,    30,    13,    63,    63,    65,    29,    67,    30,    30,
      30,    19,    62,    29,    68,    30
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
        case 2:
#line 157 "scan-ops_pddl.y"
    { opserr( DOMDEF_EXPECTED, NULL ); ;}
    break;

  case 4:
#line 165 "scan-ops_pddl.y"
    { 
  /* initialize typetree */
  gglobal_type_tree_list = new_type_tree_list( STANDARD_TYPE );
;}
    break;

  case 5:
#line 170 "scan-ops_pddl.y"
    {
  if ( gcmd_line.display_info ) {
    fprintf(OUT, "\ndomain '%s' defined\n", gdomain_name);
  }
;}
    break;

  case 6:
#line 180 "scan-ops_pddl.y"
    { 
  gdomain_name = new_token( strlen(yyvsp[-1].string)+1 );
  strcpy( gdomain_name, yyvsp[-1].string);
;}
    break;

  case 14:
#line 209 "scan-ops_pddl.y"
    {
;}
    break;

  case 15:
#line 212 "scan-ops_pddl.y"
    { 
;}
    break;

  case 16:
#line 218 "scan-ops_pddl.y"
    {;}
    break;

  case 17:
#line 221 "scan-ops_pddl.y"
    {
  FactList *fl, *fl1;
  TokenList *tl, *tl1;
  if (NULL!=gpredicates_and_types)
    {
      fl=gpredicates_and_types;
      while (NULL!=fl->next)
	{
	  fl=fl->next;
	}
      fl->next=new_fact_list();
      fl=fl->next;
    }
  else
    {
      fl=new_fact_list();
      gpredicates_and_types=fl;
    }
  tl=new_token_list();
  fl->item=tl;
  tl->item=copy_string(yyvsp[-2].string);
  fl1=yyvsp[-1].pFactList;
  while (fl1!=NULL) 
    {
      tl1=new_token_list();
      tl->next=tl1;
      tl=tl1;
      tl1->item=copy_string(fl1->item->next->item);
      fl1=fl1->next;
    }
  free_complete_FactList( yyvsp[-1].pFactList );
;}
    break;

  case 19:
#line 260 "scan-ops_pddl.y"
    { opserr( REQUIREM_EXPECTED, NULL ); ;}
    break;

  case 20:
#line 262 "scan-ops_pddl.y"
    { 
  if ( !supported( yyvsp[0].string ) )
    {
      opserr( NOT_SUPPORTED, yyvsp[0].string );
      yyerror();
    }
;}
    break;

  case 23:
#line 277 "scan-ops_pddl.y"
    { 
  if ( !supported( yyvsp[0].string ) )
    {
      opserr( NOT_SUPPORTED, yyvsp[0].string );
      yyerror();
    }
;}
    break;

  case 25:
#line 290 "scan-ops_pddl.y"
    { opserr( TYPEDEF_EXPECTED, NULL ); ;}
    break;

  case 26:
#line 292 "scan-ops_pddl.y"
    { 
  add_to_type_tree( yyvsp[-1].pFactList, main_type_tree() );
  free_complete_FactList( yyvsp[-1].pFactList );
;}
    break;

  case 27:
#line 301 "scan-ops_pddl.y"
    { opserr( CONSTLIST_EXPECTED, NULL ); ;}
    break;

  case 28:
#line 303 "scan-ops_pddl.y"
    { 
  gorig_constant_list = yyvsp[-1].pFactList;
;}
    break;

  case 29:
#line 312 "scan-ops_pddl.y"
    { opserr( ACTION, NULL ); ;}
    break;

  case 30:
#line 313 "scan-ops_pddl.y"
    { 
  scur_op = new_pl_operator( yyvsp[0].string );
;}
    break;

  case 31:
#line 317 "scan-ops_pddl.y"
    {
  scur_op->next = gloaded_ops;
  gloaded_ops = scur_op; 
;}
    break;

  case 32:
#line 326 "scan-ops_pddl.y"
    { scur_op->params = NULL; ;}
    break;

  case 33:
#line 329 "scan-ops_pddl.y"
    {
  FactList * f;
  scur_op->params = yyvsp[-1].pFactList;
  for (f = scur_op->params; f; f = f->next)
    {
      /* to be able to distinguish params from :VARS */
      scur_op->number_of_real_params++;
    }
;}
    break;

  case 35:
#line 345 "scan-ops_pddl.y"
    {
  FactList * f = NULL;

  /* add vars as parameters */
  if ( scur_op->params )
    {
      for( f = scur_op->params; f->next; f = f->next )
	{
	  /* empty, get to the end of list */
	}
      f->next = yyvsp[-2].pFactList;
      f = f->next;
    }
  else
    {
      scur_op->params = yyvsp[-2].pFactList;
    }
;}
    break;

  case 36:
#line 365 "scan-ops_pddl.y"
    { 
  scur_op->preconds = yyvsp[0].pPlNode; 
;}
    break;

  case 38:
#line 371 "scan-ops_pddl.y"
    { 
  scur_op->effects = yyvsp[0].pPlNode; 
;}
    break;

  case 40:
#line 383 "scan-ops_pddl.y"
    {
  /* returns new operator the name of which is AXIOM plus a number */
  scur_op = new_axiom_op_list(); 
;}
    break;

  case 41:
#line 388 "scan-ops_pddl.y"
    {
  scur_op->params = yyvsp[-1].pFactList;
;}
    break;

  case 42:
#line 392 "scan-ops_pddl.y"
    { 
  scur_op->preconds = yyvsp[0].pPlNode; 
;}
    break;

  case 43:
#line 396 "scan-ops_pddl.y"
    {
  PlNode * tmp;

  if (TRUE == sis_negated)
    {
      tmp = new_pl_node(NOT);
      tmp->sons = new_pl_node(ATOM);
      tmp->sons->atom = yyvsp[0].pTokenList;
      sis_negated = FALSE;
    }
  else
    {
      tmp = new_pl_node(ATOM);
      tmp->atom = yyvsp[0].pTokenList;
    }

  scur_op->effects = tmp;
;}
    break;

  case 44:
#line 415 "scan-ops_pddl.y"
    {
  /* Allowing complete "effects" is more than UCPOP and PDDL do,
     but this can easily be checked: the effect must be a single
     literal, otherwise axiom effects may become a little complicated */
  scur_op->next = gloaded_axioms;
  gloaded_axioms = scur_op;
  /* save axioms separately for now, after preprocessing they may
     be added to the other operators */
;}
    break;

  case 45:
#line 433 "scan-ops_pddl.y"
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

  case 46:
#line 449 "scan-ops_pddl.y"
    { 
  yyval.pPlNode = new_pl_node(AND);
  yyval.pPlNode->sons = yyvsp[-1].pPlNode;
;}
    break;

  case 47:
#line 455 "scan-ops_pddl.y"
    { 
  yyval.pPlNode = new_pl_node(OR);
  yyval.pPlNode->sons = yyvsp[-1].pPlNode;
;}
    break;

  case 48:
#line 461 "scan-ops_pddl.y"
    { 
  yyval.pPlNode = new_pl_node(NOT);
  yyval.pPlNode->sons = yyvsp[-1].pPlNode;
;}
    break;

  case 49:
#line 467 "scan-ops_pddl.y"
    { 
  PlNode * np = new_pl_node(NOT);
  np->sons = yyvsp[-2].pPlNode;
  np->next = yyvsp[-1].pPlNode;

  yyval.pPlNode = new_pl_node(OR);
  yyval.pPlNode->sons = np;
;}
    break;

  case 50:
#line 479 "scan-ops_pddl.y"
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

  case 51:
#line 519 "scan-ops_pddl.y"
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

  case 52:
#line 556 "scan-ops_pddl.y"
    {
  yyval.pPlNode = NULL;
;}
    break;

  case 53:
#line 561 "scan-ops_pddl.y"
    {
  yyvsp[-1].pPlNode->next = yyvsp[0].pPlNode;
  yyval.pPlNode = yyvsp[-1].pPlNode;
;}
    break;

  case 54:
#line 573 "scan-ops_pddl.y"
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

  case 55:
#line 589 "scan-ops_pddl.y"
    { 
  yyval.pPlNode = new_pl_node(AND);
  yyval.pPlNode->sons = yyvsp[-1].pPlNode;
;}
    break;

  case 56:
#line 595 "scan-ops_pddl.y"
    { 
  yyval.pPlNode = new_pl_node(NOT);
  yyval.pPlNode->sons = yyvsp[-1].pPlNode;
;}
    break;

  case 57:
#line 603 "scan-ops_pddl.y"
    { 
  /* This will be handled exactly as quantors in the goals part, s.o. */
  FactList * fl = yyvsp[-3].pFactList, *f1;
  PlNode * pln1;
  PlNode * pln2;
  
  pln1 = new_pl_node(ALL);
  pln1->atom = fl->item;
  yyval.pPlNode = pln1;

  f1 = fl;
  /* every loop gives us one quantor with one variable and its type */
  while (NULL != f1->next)
    {
      f1 = f1->next;
      
      pln2 = new_pl_node(ALL);
      pln2->atom = f1->item;
      pln1->sons = pln2;
      pln1 = pln2;
    }
  pln1->sons = yyvsp[-1].pPlNode;

  f1 = fl->next;
  while ( TRUE ) {
    free( fl );
    if ( !f1 ) break;
    fl = f1;
    f1 = fl->next;
  }

;}
    break;

  case 58:
#line 637 "scan-ops_pddl.y"
    {
  /* This will be conditional effects in IPP representation, but here
     a formula like (WHEN p q) will be saved as:
      [WHEN]
      [sons]
       /  \
     [p]  [q]
     That means, the first son is p, and the second one is q. */
  yyval.pPlNode = new_pl_node(WHEN);
  yyvsp[-2].pPlNode->next = yyvsp[-1].pPlNode;
  yyval.pPlNode->sons = yyvsp[-2].pPlNode;
;}
    break;

  case 59:
#line 653 "scan-ops_pddl.y"
    { 
  yyval.pPlNode = NULL; 
;}
    break;

  case 60:
#line 658 "scan-ops_pddl.y"
    {
  yyvsp[-1].pPlNode->next = yyvsp[0].pPlNode;
  yyval.pPlNode = yyvsp[-1].pPlNode;
;}
    break;

  case 61:
#line 669 "scan-ops_pddl.y"
    { 
  yyval.pTokenList = yyvsp[-1].pTokenList;
  sis_negated = TRUE;
/*   $$ = new_token_list(); */
/*   $$->item = new_token( strlen($3->item)+2 ); */
/*   sprintf($$->item, "!%s", $3->item); */
/*   $$->next = $3->next; */
;}
    break;

  case 62:
#line 679 "scan-ops_pddl.y"
    {
  yyval.pTokenList = yyvsp[0].pTokenList;
;}
    break;

  case 63:
#line 687 "scan-ops_pddl.y"
    { 
  yyval.pTokenList = new_token_list();
  yyval.pTokenList->item = yyvsp[-2].pstring;
  yyval.pTokenList->next = yyvsp[-1].pTokenList;
;}
    break;

  case 64:
#line 697 "scan-ops_pddl.y"
    { yyval.pTokenList = NULL; ;}
    break;

  case 65:
#line 700 "scan-ops_pddl.y"
    {
  yyval.pTokenList = new_token_list();
  yyval.pTokenList->item = yyvsp[-1].pstring;
  yyval.pTokenList->next = yyvsp[0].pTokenList;
;}
    break;

  case 66:
#line 710 "scan-ops_pddl.y"
    { 
  yyval.pstring = new_token( strlen(yyvsp[0].string)+1 );
  strcpy( yyval.pstring, yyvsp[0].string );
;}
    break;

  case 67:
#line 716 "scan-ops_pddl.y"
    { 
  yyval.pstring = new_token( strlen(yyvsp[0].string)+1 );
  strcpy( yyval.pstring, yyvsp[0].string );
;}
    break;

  case 68:
#line 725 "scan-ops_pddl.y"
    {
  yyval.pTokenList = new_token_list();
  yyval.pTokenList->item = new_token( strlen(yyvsp[0].string)+1 );
  strcpy( yyval.pTokenList->item, yyvsp[0].string );
;}
    break;

  case 69:
#line 732 "scan-ops_pddl.y"
    {
  yyval.pTokenList = new_token_list();
  yyval.pTokenList->item = new_token( strlen(yyvsp[-1].string)+1 );
  strcpy( yyval.pTokenList->item, yyvsp[-1].string );
  yyval.pTokenList->next = yyvsp[0].pTokenList;
;}
    break;

  case 70:
#line 743 "scan-ops_pddl.y"
    { 
  yyval.pstring = new_token( strlen(yyvsp[0].string)+1 );
  strcpy( yyval.pstring, yyvsp[0].string );
;}
    break;

  case 71:
#line 749 "scan-ops_pddl.y"
    { 
  yyval.pstring = new_token( strlen(EQ_STR)+1 );
  strcpy( yyval.pstring, EQ_STR );
;}
    break;

  case 72:
#line 758 "scan-ops_pddl.y"
    { yyval.pFactList = NULL; ;}
    break;

  case 73:
#line 761 "scan-ops_pddl.y"
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
  TokenList *t, *t1;
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
    }


  t = yyvsp[-2].pTokenList;
  while ( t ) {
    t1 = t->next;
    free( t->item );
    free( t );
    t = t1;
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

  case 74:
#line 824 "scan-ops_pddl.y"
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

  case 75:
#line 836 "scan-ops_pddl.y"
    {
  yyval.pFactList = new_fact_list();
  yyval.pFactList->item = new_token_list();
  yyval.pFactList->item->item = new_token( strlen(yyvsp[-1].string)+1 );
  strcpy( yyval.pFactList->item->item, yyvsp[-1].string );
  yyval.pFactList->item->next = new_token_list();
  if ( yyvsp[0].pFactList )   /* another element (already typed) is following */
    {
      char * s = yyvsp[0].pFactList->item->next->item;
      yyval.pFactList->item->next->item = new_token(strlen(s) + 1);
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

  case 76:
#line 861 "scan-ops_pddl.y"
    { yyval.pFactList = NULL; ;}
    break;

  case 77:
#line 864 "scan-ops_pddl.y"
    { /* this is a very, very special case... it may mean that
     a parameter has one of the types in name_plus
     => build a new type name: either_name1_name2_..._namen and check
     if a type with this name already exists. If so then NAME has this type,
     if not then build a new type, include it in the type tree and
     return NAME with the new type 
     The new artificial type is not a subtype of OBJECT because its own
     elements must already be instances of a subtype of OBJECT */
  char * s;
  TokenList *t, *t1;
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

  t = yyvsp[-2].pTokenList;
  while ( t ) {
    t1 = t->next;
    free( t->item );
    free( t );
    t = t1;
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

  case 78:
#line 925 "scan-ops_pddl.y"
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

  case 79:
#line 937 "scan-ops_pddl.y"
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


    }

/* Line 991 of yacc.c.  */
#line 2103 "scan-ops_pddl.tab.c"

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


#line 937 "scan-ops_pddl.y"

#include "lex.ops_pddl.c"

/**********************************************************************
 * Functions
 **********************************************************************/

/* 
call	bison -pops -bscan-ops scan-ops.y
*/

void 
opserr(int errno, char * par)
{
/*   sact_err = errno; */
/*   if (NULL != sact_err_par) */
/*     { */
/*       free(sact_err_par); */
/*     } */
/*   if (NULL != par) */
/*     { */
/*       sact_err_par = new_token(strlen(par)+1); */
/*       strcpy(sact_err_par, par); */
/*     } */
/*   else */
/*     { */
/*       sact_err_par = NULL; */
/*     } */
}
  
int 
yyerror(char * msg)
{
  fflush(stdout);
  fprintf(stderr, "\n%s: syntax error in line %d, '%s':\n", 
	  gact_filename, lineno, yytext);
  if (NULL != sact_err_par)
    {
      fprintf(stderr, "%s %s\n", serrmsg[sact_err], sact_err_par);
    }
  else
    {
      fprintf(stderr, "%s\n", serrmsg[sact_err]);
    }
  switch (sact_err)
    {
    case NOT_SUPPORTED:
      exit( NOT_SUPPORTED_ERROR_CODE );
    default:
      exit( OPS_PARSE_ERROR_CODE );
     }
}


void 
load_ops_file(char * filename)
{
  FILE * fp;/* pointer to input files */
  char tmp[MAX_LENGTH] = "";

  /* open operator file */
  if( ( fp = fopen( filename, "r" ) ) == NULL ) 
    {
      sprintf(tmp, "\nipp: can't find operator file: %s\n\n", filename );
      perror(tmp);
      exit( OPS_MISSING_ERROR_CODE );
    }
  gact_filename = filename;
  lineno = 1; 
  yyin = fp;
  yyparse();
  fclose( fp );/* and close file again */
}


