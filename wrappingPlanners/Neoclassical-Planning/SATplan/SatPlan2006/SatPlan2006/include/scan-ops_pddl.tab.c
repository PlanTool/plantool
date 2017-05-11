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
     PREDICATES_TOK = 264,
     ACTION_TOK = 265,
     VARS_TOK = 266,
     CONTEXT_TOK = 267,
     IMPLIES_TOK = 268,
     PRECONDITION_TOK = 269,
     PARAMETERS_TOK = 270,
     EFFECT_TOK = 271,
     AND_TOK = 272,
     NOT_TOK = 273,
     WHEN_TOK = 274,
     FORALL_TOK = 275,
     IMPLY_TOK = 276,
     OR_TOK = 277,
     EXISTS_TOK = 278,
     EQUAL_TOK = 279,
     NAME = 280,
     VARIABLE = 281,
     TYPE = 282,
     OPEN_PAREN = 283,
     CLOSE_PAREN = 284
   };
#endif
#define DEFINE_TOK 258
#define DOMAIN_TOK 259
#define REQUIREMENTS_TOK 260
#define TYPES_TOK 261
#define EITHER_TOK 262
#define CONSTANTS_TOK 263
#define PREDICATES_TOK 264
#define ACTION_TOK 265
#define VARS_TOK 266
#define CONTEXT_TOK 267
#define IMPLIES_TOK 268
#define PRECONDITION_TOK 269
#define PARAMETERS_TOK 270
#define EFFECT_TOK 271
#define AND_TOK 272
#define NOT_TOK 273
#define WHEN_TOK 274
#define FORALL_TOK 275
#define IMPLY_TOK 276
#define OR_TOK 277
#define EXISTS_TOK 278
#define EQUAL_TOK 279
#define NAME 280
#define VARIABLE 281
#define TYPE 282
#define OPEN_PAREN 283
#define CLOSE_PAREN 284




/* Copy the first part of user declarations.  */
#line 1 "scan-ops_pddl.y"

#ifdef YYDEBUG
  extern int yydebug=1;
#endif


#include <stdio.h>
#include <string.h> 
#include "bb.h"
#include "memory.h"
#include "parse.h"


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
 

static char *serrmsg[] = {
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
  "requirement %s not supported by this IPP version",  
  "action definition is not correct",
  NULL
};


void opserr( int errno, char *par );


static int sact_err;
static char *sact_err_par = NULL;
static PlOperator *scur_op = NULL;
static Bool sis_negated = FALSE;


int supported( char *str )

{

  int i;
  char * sup[] = { ":STRIPS", ":NEGATION", ":EQUALITY",":TYPING", 
		   ":CONDITIONAL-EFFECTS", ":DISJUNCTIVE-PRECONDITIONS", 
		   ":EXISTENTIAL-PRECONDITIONS", ":UNIVERSAL-PRECONDITIONS", 
		   ":QUANTIFIED-PRECONDITIONS", ":ADL",
		   NULL };     

  for (i=0; NULL != sup[i]; i++) {
    if ( SAME == strcmp(sup[i], str) ) {
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
#line 107 "scan-ops_pddl.y"
typedef union YYSTYPE {

  char string[MAX_LENGTH];
  char *pstring;
  PlNode *pPlNode;
  FactList *pFactList;
  TokenList *pTokenList;

} YYSTYPE;
/* Line 191 of yacc.c.  */
#line 254 "scan-ops_pddl.tab.c"
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif



/* Copy the second part of user declarations.  */


/* Line 214 of yacc.c.  */
#line 266 "scan-ops_pddl.tab.c"

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
#define YYLAST   126

/* YYNTOKENS -- Number of terminals. */
#define YYNTOKENS  30
/* YYNNTS -- Number of nonterminals. */
#define YYNNTS  39
/* YYNRULES -- Number of rules. */
#define YYNRULES  73
/* YYNRULES -- Number of states. */
#define YYNSTATES  156

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   284

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
      25,    26,    27,    28,    29
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const unsigned char yyprhs[] =
{
       0,     0,     3,     4,     7,     8,    14,    19,    21,    24,
      27,    30,    33,    36,    37,    43,    44,    45,    52,    53,
      54,    62,    63,    64,    68,    69,    75,    76,    82,    83,
      84,    93,    94,    99,   100,   106,   107,   112,   113,   118,
     120,   125,   130,   135,   141,   149,   157,   158,   161,   163,
     168,   173,   181,   187,   188,   191,   196,   198,   203,   204,
     207,   209,   211,   213,   216,   218,   220,   221,   227,   231,
     234,   235,   241,   245
};

/* YYRHS -- A `-1'-separated list of the rules' RHS. */
static const yysigned_char yyrhs[] =
{
      31,     0,    -1,    -1,    32,    33,    -1,    -1,    28,     3,
      35,    34,    36,    -1,    28,     4,    25,    29,    -1,    29,
      -1,    41,    36,    -1,    48,    36,    -1,    46,    36,    -1,
      50,    36,    -1,    37,    36,    -1,    -1,    28,     9,    39,
      38,    29,    -1,    -1,    -1,    28,    25,    68,    29,    40,
      39,    -1,    -1,    -1,    28,     5,    42,    25,    43,    44,
      29,    -1,    -1,    -1,    25,    45,    44,    -1,    -1,    28,
       6,    47,    67,    29,    -1,    -1,    28,     8,    49,    67,
      29,    -1,    -1,    -1,    28,    10,    51,    25,    52,    53,
      54,    29,    -1,    -1,    15,    28,    68,    29,    -1,    -1,
      11,    28,    68,    29,    54,    -1,    -1,    14,    57,    55,
      54,    -1,    -1,    16,    59,    56,    54,    -1,    61,    -1,
      28,    17,    58,    29,    -1,    28,    22,    58,    29,    -1,
      28,    18,    57,    29,    -1,    28,    21,    57,    57,    29,
      -1,    28,    23,    28,    68,    29,    57,    29,    -1,    28,
      20,    28,    68,    29,    57,    29,    -1,    -1,    57,    58,
      -1,    61,    -1,    28,    17,    60,    29,    -1,    28,    18,
      59,    29,    -1,    28,    20,    28,    68,    29,    59,    29,
      -1,    28,    19,    57,    59,    29,    -1,    -1,    59,    60,
      -1,    28,    18,    62,    29,    -1,    62,    -1,    28,    66,
      63,    29,    -1,    -1,    64,    63,    -1,    25,    -1,    26,
      -1,    25,    -1,    25,    65,    -1,    25,    -1,    24,    -1,
      -1,    25,     7,    65,    29,    67,    -1,    25,    27,    67,
      -1,    25,    67,    -1,    -1,    26,     7,    65,    29,    68,
      -1,    26,    27,    68,    -1,    26,    68,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const unsigned short yyrline[] =
{
       0,   163,   163,   163,   174,   173,   190,   200,   202,   204,
     206,   208,   210,   217,   216,   226,   229,   228,   268,   272,
     267,   283,   287,   286,   300,   299,   314,   313,   329,   333,
     328,   347,   351,   364,   367,   386,   385,   392,   391,   406,
     419,   425,   431,   437,   447,   489,   531,   535,   549,   562,
     568,   574,   611,   630,   634,   646,   652,   661,   673,   675,
     686,   692,   702,   709,   720,   726,   737,   739,   799,   811,
     835,   837,   896,   908
};
#endif

#if YYDEBUG || YYERROR_VERBOSE
/* YYTNME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals. */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "DEFINE_TOK", "DOMAIN_TOK", 
  "REQUIREMENTS_TOK", "TYPES_TOK", "EITHER_TOK", "CONSTANTS_TOK", 
  "PREDICATES_TOK", "ACTION_TOK", "VARS_TOK", "CONTEXT_TOK", 
  "IMPLIES_TOK", "PRECONDITION_TOK", "PARAMETERS_TOK", "EFFECT_TOK", 
  "AND_TOK", "NOT_TOK", "WHEN_TOK", "FORALL_TOK", "IMPLY_TOK", "OR_TOK", 
  "EXISTS_TOK", "EQUAL_TOK", "NAME", "VARIABLE", "TYPE", "OPEN_PAREN", 
  "CLOSE_PAREN", "$accept", "file", "@1", "domain_definition", "@2", 
  "domain_name", "optional_domain_defs", "predicates_def", "@3", 
  "predicates_list", "@4", "require_def", "@5", "@6", "require_key_star", 
  "@7", "types_def", "@8", "constants_def", "@9", "action_def", "@10", 
  "@11", "param_def", "action_def_body", "@12", "@13", 
  "adl_goal_description", "adl_goal_description_star", "adl_effect", 
  "adl_effect_star", "literal_term", "atomic_formula_term", "term_star", 
  "term", "name_plus", "predicate", "typed_list_name", 
  "typed_list_variable", 0
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[YYLEX-NUM] -- Internal token number corresponding to
   token YYLEX-NUM.  */
static const unsigned short yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const unsigned char yyr1[] =
{
       0,    30,    32,    31,    34,    33,    35,    36,    36,    36,
      36,    36,    36,    38,    37,    39,    40,    39,    42,    43,
      41,    44,    45,    44,    47,    46,    49,    48,    51,    52,
      50,    53,    53,    54,    54,    55,    54,    56,    54,    57,
      57,    57,    57,    57,    57,    57,    58,    58,    59,    59,
      59,    59,    59,    60,    60,    61,    61,    62,    63,    63,
      64,    64,    65,    65,    66,    66,    67,    67,    67,    67,
      68,    68,    68,    68
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const unsigned char yyr2[] =
{
       0,     2,     0,     2,     0,     5,     4,     1,     2,     2,
       2,     2,     2,     0,     5,     0,     0,     6,     0,     0,
       7,     0,     0,     3,     0,     5,     0,     5,     0,     0,
       8,     0,     4,     0,     5,     0,     4,     0,     4,     1,
       4,     4,     4,     5,     7,     7,     0,     2,     1,     4,
       4,     7,     5,     0,     2,     4,     1,     4,     0,     2,
       1,     1,     1,     2,     1,     1,     0,     5,     3,     2,
       0,     5,     3,     2
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const unsigned char yydefact[] =
{
       2,     0,     0,     1,     0,     3,     0,     0,     4,     0,
       0,     0,     0,     7,     5,     0,     0,     0,     0,     0,
       6,    18,    24,    26,    15,    28,    12,     8,    10,     9,
      11,     0,    66,    66,     0,    13,     0,    19,    66,     0,
       0,    70,     0,    29,    21,     0,    66,    69,    25,    27,
      70,     0,    14,    31,    22,     0,    62,     0,    68,     0,
      70,    73,    16,     0,    33,    21,    20,    63,    66,     0,
      72,    15,    70,     0,     0,     0,     0,    23,    67,    70,
      17,     0,    70,     0,    35,    39,    56,     0,    37,    48,
      30,    71,    32,     0,    46,     0,     0,     0,    46,     0,
      65,    64,    58,    33,    53,     0,     0,     0,    33,    33,
      46,     0,     0,     0,    70,     0,     0,    70,    60,    61,
       0,    58,    36,    53,     0,     0,     0,    70,    38,    34,
      47,    40,    42,    55,     0,     0,    41,     0,    57,    59,
      54,    49,    50,     0,     0,     0,    43,     0,    52,     0,
       0,     0,     0,    45,    44,    51
};

/* YYDEFGOTO[NTERM-NUM]. */
static const yysigned_char yydefgoto[] =
{
      -1,     1,     2,     5,    10,     8,    14,    15,    42,    35,
      71,    16,    31,    44,    55,    65,    17,    32,    18,    33,
      19,    36,    53,    64,    76,   103,   108,   110,   111,   123,
     124,    85,    86,   120,   121,    57,   102,    39,    51
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -96
static const yysigned_char yypact[] =
{
     -96,    11,   -12,   -96,    23,   -96,     8,    43,   -96,    25,
      -1,    19,    51,   -96,   -96,    -1,    -1,    -1,    -1,    -1,
     -96,   -96,   -96,   -96,    26,   -96,   -96,   -96,   -96,   -96,
     -96,    33,    37,    37,    38,   -96,    49,   -96,    -4,    22,
      53,    54,    55,   -96,    66,    67,    37,   -96,   -96,   -96,
      -2,    64,   -96,    79,   -96,    68,    67,    69,   -96,    67,
      54,   -96,   -96,    72,    65,    66,   -96,   -96,    37,    70,
     -96,    26,    54,    73,    74,    75,    76,   -96,   -96,    54,
     -96,    77,    54,    48,   -96,   -96,   -96,    21,   -96,   -96,
     -96,   -96,   -96,    78,    74,    74,    80,    74,    74,    81,
     -96,   -96,    17,    65,    75,    75,    74,    82,    65,    65,
      74,    83,    84,    85,    54,    74,    86,    54,   -96,   -96,
      87,    17,   -96,    75,    88,    89,    75,    54,   -96,   -96,
     -96,   -96,   -96,   -96,    90,    91,   -96,    92,   -96,   -96,
     -96,   -96,   -96,    93,    94,    74,   -96,    74,   -96,    75,
      95,    96,    97,   -96,   -96,   -96
};

/* YYPGOTO[NTERM-NUM].  */
static const yysigned_char yypgoto[] =
{
     -96,   -96,   -96,   -96,   -96,   -96,    71,   -96,   -96,    24,
     -96,   -96,   -96,   -96,    31,   -96,   -96,   -96,   -96,   -96,
     -96,   -96,   -96,   -96,   -95,   -96,   -96,   -62,   -92,   -71,
     -19,   -74,   -86,   -10,   -96,   -39,   -96,   -31,   -50
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -1
static const unsigned char yytable[] =
{
      61,    89,    40,    45,    88,    59,   116,    47,   122,   113,
      70,     3,    84,   128,   129,    58,     4,    67,   130,   113,
      69,    38,    81,    46,    50,    60,     6,    12,    13,    91,
      89,    89,    93,   112,   125,   115,     7,    78,   104,   105,
     106,   107,   118,   119,   126,   100,   101,     9,    20,    89,
      11,    48,    89,   135,    34,   143,    21,    22,    37,    23,
      24,    25,    38,    41,   134,    94,    95,   137,    96,    97,
      98,    99,   100,   101,    43,    89,    73,   144,   152,    74,
      50,    75,    49,   150,    52,   151,    26,    27,    28,    29,
      30,    54,    56,    62,    63,    80,    77,    66,    68,    79,
      72,    82,    83,    87,   140,    90,    92,   109,   114,   117,
     127,   139,   131,   132,   133,   136,   138,   141,   142,   145,
     146,   147,   148,   149,   153,   154,   155
};

static const unsigned char yycheck[] =
{
      50,    75,    33,     7,    75,     7,    98,    38,   103,    95,
      60,     0,    74,   108,   109,    46,    28,    56,   110,   105,
      59,    25,    72,    27,    26,    27,     3,    28,    29,    79,
     104,   105,    82,    95,   105,    97,    28,    68,    17,    18,
      19,    20,    25,    26,   106,    24,    25,     4,    29,   123,
      25,    29,   126,   115,    28,   126,     5,     6,    25,     8,
       9,    10,    25,    25,   114,    17,    18,   117,    20,    21,
      22,    23,    24,    25,    25,   149,    11,   127,   149,    14,
      26,    16,    29,   145,    29,   147,    15,    16,    17,    18,
      19,    25,    25,    29,    15,    71,    65,    29,    29,    29,
      28,    28,    28,    28,   123,    29,    29,    29,    28,    28,
      28,   121,    29,    29,    29,    29,    29,    29,    29,    29,
      29,    29,    29,    29,    29,    29,    29
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const unsigned char yystos[] =
{
       0,    31,    32,     0,    28,    33,     3,    28,    35,     4,
      34,    25,    28,    29,    36,    37,    41,    46,    48,    50,
      29,     5,     6,     8,     9,    10,    36,    36,    36,    36,
      36,    42,    47,    49,    28,    39,    51,    25,    25,    67,
      67,    25,    38,    25,    43,     7,    27,    67,    29,    29,
      26,    68,    29,    52,    25,    44,    25,    65,    67,     7,
      27,    68,    29,    15,    53,    45,    29,    65,    29,    65,
      68,    40,    28,    11,    14,    16,    54,    44,    67,    29,
      39,    68,    28,    28,    57,    61,    62,    28,    59,    61,
      29,    68,    29,    68,    17,    18,    20,    21,    22,    23,
      24,    25,    66,    55,    17,    18,    19,    20,    56,    29,
      57,    58,    57,    62,    28,    57,    58,    28,    25,    26,
      63,    64,    54,    59,    60,    59,    57,    28,    54,    54,
      58,    29,    29,    29,    68,    57,    29,    68,    29,    63,
      60,    29,    29,    59,    68,    29,    29,    29,    29,    29,
      57,    57,    59,    29,    29,    29
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
#line 163 "scan-ops_pddl.y"
    { 
  opserr( DOMDEF_EXPECTED, NULL ); 
;}
    break;

  case 4:
#line 174 "scan-ops_pddl.y"
    { 
  /* initialize typetree 
   */
  gglobal_type_tree_list = new_type_tree_list( STANDARD_TYPE );
;}
    break;

  case 5:
#line 180 "scan-ops_pddl.y"
    {
  if ( gcmd_line.display_info >= 1 ) {
    printf("\ndomain '%s' defined\n", gdomain_name);
  }
;}
    break;

  case 6:
#line 191 "scan-ops_pddl.y"
    { 
  gdomain_name = new_Token( strlen(yyvsp[-1].string)+1 );
  strcpy( gdomain_name, yyvsp[-1].string);
;}
    break;

  case 13:
#line 217 "scan-ops_pddl.y"
    {
;}
    break;

  case 14:
#line 220 "scan-ops_pddl.y"
    { 
;}
    break;

  case 15:
#line 226 "scan-ops_pddl.y"
    {;}
    break;

  case 16:
#line 229 "scan-ops_pddl.y"
    {

  FactList *fl, *fl1;
  TokenList *tl, *tl1;

  if ( gpredicates_and_types ) {
    fl = gpredicates_and_types;
    while ( fl->next ) {
      fl = fl->next;
    }
    fl->next = new_FactList();
    fl = fl->next;
  } else {
    fl = new_FactList();
    gpredicates_and_types = fl;
  }
  tl = new_TokenList();
  fl->item = tl;
  tl->item = new_Token( strlen( yyvsp[-2].string ) + 1);
  strcpy( tl->item, yyvsp[-2].string );
  fl1 = yyvsp[-1].pFactList;
  while ( fl1 ) {
    tl1 = new_TokenList();
    tl->next = tl1;
    tl = tl1;
    tl1->item = new_Token( strlen( fl1->item->next->item ) + 1 );
    strcpy( tl1->item, fl1->item->next->item );
    fl1 = fl1->next;
  }
  free_FactList( yyvsp[-1].pFactList );

;}
    break;

  case 18:
#line 268 "scan-ops_pddl.y"
    { 
  opserr( REQUIREM_EXPECTED, NULL ); 
;}
    break;

  case 19:
#line 272 "scan-ops_pddl.y"
    { 
  if ( !supported( yyvsp[0].string ) ) {
    opserr( NOT_SUPPORTED, yyvsp[0].string );
    yyerror();
  }
;}
    break;

  case 22:
#line 287 "scan-ops_pddl.y"
    { 
  if ( !supported( yyvsp[0].string ) ) {
    opserr( NOT_SUPPORTED, yyvsp[0].string );
    yyerror();
  }
;}
    break;

  case 24:
#line 300 "scan-ops_pddl.y"
    { 
  opserr( TYPEDEF_EXPECTED, NULL ); 
;}
    break;

  case 25:
#line 304 "scan-ops_pddl.y"
    { 
  add_to_type_tree( yyvsp[-1].pFactList, main_type_tree() );
  free_FactList( yyvsp[-1].pFactList );
;}
    break;

  case 26:
#line 314 "scan-ops_pddl.y"
    { 
  opserr( CONSTLIST_EXPECTED, NULL ); 
;}
    break;

  case 27:
#line 318 "scan-ops_pddl.y"
    { 
  gorig_constant_list = yyvsp[-1].pFactList;
;}
    break;

  case 28:
#line 329 "scan-ops_pddl.y"
    { 
  opserr( ACTION, NULL ); 
;}
    break;

  case 29:
#line 333 "scan-ops_pddl.y"
    { 
  scur_op = new_PlOperator( yyvsp[0].string );
;}
    break;

  case 30:
#line 337 "scan-ops_pddl.y"
    {
  scur_op->next = gloaded_ops;
  gloaded_ops = scur_op; 
;}
    break;

  case 31:
#line 347 "scan-ops_pddl.y"
    { 
  scur_op->params = NULL; 
;}
    break;

  case 32:
#line 352 "scan-ops_pddl.y"
    {
  FactList *f;
  scur_op->params = yyvsp[-1].pFactList;
  for (f = scur_op->params; f; f = f->next) {
    /* to be able to distinguish params from :VARS 
     */
    scur_op->number_of_real_params++;
  }
;}
    break;

  case 34:
#line 368 "scan-ops_pddl.y"
    {
  FactList *f = NULL;

  /* add vars as parameters 
   */
  if ( scur_op->params ) {
    for( f = scur_op->params; f->next; f = f->next ) {
      /* empty, get to the end of list 
       */
    }
    f->next = yyvsp[-2].pFactList;
    f = f->next;
  } else {
    scur_op->params = yyvsp[-2].pFactList;
  }
;}
    break;

  case 35:
#line 386 "scan-ops_pddl.y"
    { 
  scur_op->preconds = yyvsp[0].pPlNode; 
;}
    break;

  case 37:
#line 392 "scan-ops_pddl.y"
    { 
  scur_op->effects = yyvsp[0].pPlNode; 
;}
    break;

  case 39:
#line 407 "scan-ops_pddl.y"
    { 
  if ( sis_negated ) {
    yyval.pPlNode = new_PlNode(NOT);
    yyval.pPlNode->sons = new_PlNode(ATOM);
    yyval.pPlNode->sons->atom = yyvsp[0].pTokenList;
    sis_negated = FALSE;
  } else {
    yyval.pPlNode = new_PlNode(ATOM);
    yyval.pPlNode->atom = yyvsp[0].pTokenList;
  }
;}
    break;

  case 40:
#line 420 "scan-ops_pddl.y"
    { 
  yyval.pPlNode = new_PlNode(AND);
  yyval.pPlNode->sons = yyvsp[-1].pPlNode;
;}
    break;

  case 41:
#line 426 "scan-ops_pddl.y"
    { 
  yyval.pPlNode = new_PlNode(OR);
  yyval.pPlNode->sons = yyvsp[-1].pPlNode;
;}
    break;

  case 42:
#line 432 "scan-ops_pddl.y"
    { 
  yyval.pPlNode = new_PlNode(NOT);
  yyval.pPlNode->sons = yyvsp[-1].pPlNode;
;}
    break;

  case 43:
#line 438 "scan-ops_pddl.y"
    { 
  PlNode *np = new_PlNode(NOT);
  np->sons = yyvsp[-2].pPlNode;
  np->next = yyvsp[-1].pPlNode;

  yyval.pPlNode = new_PlNode(OR);
  yyval.pPlNode->sons = np;
;}
    break;

  case 44:
#line 450 "scan-ops_pddl.y"
    { 
  /* The typed_list_variable returns a FactList with two-item TokenLists, 
   * the first item is the variable and the second item its type.
   * We now have to split off this FactList into a PlNode for each 
   * variable-type TokenList. 
   */
  FactList *tl = yyvsp[-3].pFactList, *t1;
  PlNode *pln1;
  PlNode *pln2;

  pln1 = new_PlNode(EX);
  pln1->atom = tl->item;
  yyval.pPlNode = pln1;

  t1 = tl;
  /* every loop gives us one quantor with one variable and its type 
   */
  while ( t1->next ) {
    t1 = t1->next;

    pln2 = new_PlNode(EX);
    pln2->atom = t1->item;
    /* append the next quantor to the sons of the previous node 
     */
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

  case 45:
#line 492 "scan-ops_pddl.y"
    { 
  /* This will be handled exactly like the ex-quantor case, s.a. 
   */
  FactList *tl = yyvsp[-3].pFactList, *t1;
  PlNode *pln1;
  PlNode *pln2;
  
  pln1 = new_PlNode(ALL);
  pln1->atom = tl->item;
  yyval.pPlNode = pln1;

  t1 = tl;
  /* every loop gives us one quantor with one variable and its type 
   */
  while ( t1->next ) {
    t1 = t1->next;
      
    pln2 = new_PlNode(ALL);
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

  case 46:
#line 531 "scan-ops_pddl.y"
    {
  yyval.pPlNode = NULL;
;}
    break;

  case 47:
#line 536 "scan-ops_pddl.y"
    {
  yyvsp[-1].pPlNode->next = yyvsp[0].pPlNode;
  yyval.pPlNode = yyvsp[-1].pPlNode;
;}
    break;

  case 48:
#line 550 "scan-ops_pddl.y"
    { 
  if ( sis_negated ) {
    yyval.pPlNode = new_PlNode(NOT);
    yyval.pPlNode->sons = new_PlNode(ATOM);
    yyval.pPlNode->sons->atom = yyvsp[0].pTokenList;
    sis_negated = FALSE;
  } else {
    yyval.pPlNode = new_PlNode(ATOM);
    yyval.pPlNode->atom = yyvsp[0].pTokenList;
  }
;}
    break;

  case 49:
#line 563 "scan-ops_pddl.y"
    { 
  yyval.pPlNode = new_PlNode(AND);
  yyval.pPlNode->sons = yyvsp[-1].pPlNode;
;}
    break;

  case 50:
#line 569 "scan-ops_pddl.y"
    { 
  yyval.pPlNode = new_PlNode(NOT);
  yyval.pPlNode->sons = yyvsp[-1].pPlNode;
;}
    break;

  case 51:
#line 577 "scan-ops_pddl.y"
    { 
  /* This will be handled exactly like quantors in the goals part, s.o. 
   */
  FactList *fl = yyvsp[-3].pFactList, *f1;
  PlNode *pln1;
  PlNode *pln2;
  
  pln1 = new_PlNode(ALL);
  pln1->atom = fl->item;
  yyval.pPlNode = pln1;

  f1 = fl;
  /* every loop gives us one quantor with one variable and its type 
   */
  while ( f1->next ) {
    f1 = f1->next;
    
    pln2 = new_PlNode(ALL);
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

  case 52:
#line 612 "scan-ops_pddl.y"
    {
  /* This will be conditional effects in FF representation, but here
   * a formula like (WHEN p q) will be saved as:
   *  [WHEN]
   *  [sons]
   *   /  \
   * [p]  [q]
   * That means, the first son is p, and the second one is q. 
   */
  yyval.pPlNode = new_PlNode(WHEN);
  yyvsp[-2].pPlNode->next = yyvsp[-1].pPlNode;
  yyval.pPlNode->sons = yyvsp[-2].pPlNode;
;}
    break;

  case 53:
#line 630 "scan-ops_pddl.y"
    { 
  yyval.pPlNode = NULL; 
;}
    break;

  case 54:
#line 635 "scan-ops_pddl.y"
    {
  yyvsp[-1].pPlNode->next = yyvsp[0].pPlNode;
  yyval.pPlNode = yyvsp[-1].pPlNode;
;}
    break;

  case 55:
#line 647 "scan-ops_pddl.y"
    { 
  yyval.pTokenList = yyvsp[-1].pTokenList;
  sis_negated = TRUE;
;}
    break;

  case 56:
#line 653 "scan-ops_pddl.y"
    {
  yyval.pTokenList = yyvsp[0].pTokenList;
;}
    break;

  case 57:
#line 662 "scan-ops_pddl.y"
    { 
  yyval.pTokenList = new_TokenList();
  yyval.pTokenList->item = yyvsp[-2].pstring;
  yyval.pTokenList->next = yyvsp[-1].pTokenList;
;}
    break;

  case 58:
#line 673 "scan-ops_pddl.y"
    { yyval.pTokenList = NULL; ;}
    break;

  case 59:
#line 676 "scan-ops_pddl.y"
    {
  yyval.pTokenList = new_TokenList();
  yyval.pTokenList->item = yyvsp[-1].pstring;
  yyval.pTokenList->next = yyvsp[0].pTokenList;
;}
    break;

  case 60:
#line 687 "scan-ops_pddl.y"
    { 
  yyval.pstring = new_Token( strlen(yyvsp[0].string)+1 );
  strcpy( yyval.pstring, yyvsp[0].string );
;}
    break;

  case 61:
#line 693 "scan-ops_pddl.y"
    { 
  yyval.pstring = new_Token( strlen(yyvsp[0].string)+1 );
  strcpy( yyval.pstring, yyvsp[0].string );
;}
    break;

  case 62:
#line 703 "scan-ops_pddl.y"
    {
  yyval.pTokenList = new_TokenList();
  yyval.pTokenList->item = new_Token( strlen(yyvsp[0].string)+1 );
  strcpy( yyval.pTokenList->item, yyvsp[0].string );
;}
    break;

  case 63:
#line 710 "scan-ops_pddl.y"
    {
  yyval.pTokenList = new_TokenList();
  yyval.pTokenList->item = new_Token( strlen(yyvsp[-1].string)+1 );
  strcpy( yyval.pTokenList->item, yyvsp[-1].string );
  yyval.pTokenList->next = yyvsp[0].pTokenList;
;}
    break;

  case 64:
#line 721 "scan-ops_pddl.y"
    { 
  yyval.pstring = new_Token( strlen(yyvsp[0].string)+1 );
  strcpy( yyval.pstring, yyvsp[0].string );
;}
    break;

  case 65:
#line 727 "scan-ops_pddl.y"
    { 
  yyval.pstring = new_Token( strlen(EQ_STR)+1 );
  strcpy( yyval.pstring, EQ_STR );
;}
    break;

  case 66:
#line 737 "scan-ops_pddl.y"
    { yyval.pFactList = NULL; ;}
    break;

  case 67:
#line 740 "scan-ops_pddl.y"
    { /* this is a very, very special case... it may mean that
   * a. a parameter has one of the types in name_plus
   * b. a type is subtype of one of the following types in name_plus 
   * => for both possibilities we use the same solution:
   * build a new type name: either_name1_name2_..._namen and check
   * if a type with this name already exists. If so then NAME has this type,
   * if not then build a new type, include it in the type tree and
   * return NAME with the new type 
   * The new artificial type is not a subtype of OBJECT because its own
   * elements must already be instances of a subtype of OBJECT 
   */
  char *s;
  TokenList *t, *t1;
  type_tree tt;
  type_tree_list rootl, *st;
  
  s = new_Token( MAX_LENGTH );
  strcpy( s, EITHER_STR );
  for ( t = yyvsp[-2].pTokenList; t; t = t->next ) {
    strcat( s, CONNECTOR );
    strcat( s, t->item );
  }
  tt = NULL;
  for ( rootl = gglobal_type_tree_list; rootl; rootl = rootl->next ) {
    if ((tt = find_branch( s, rootl->item))) break;
  }
  if ( !tt ) {/* the type doesn't exist yet, so build it */
    rootl = new_type_tree_list( s );
    rootl->next = gglobal_type_tree_list;
    st = &(rootl->item->sub_types);
    for ( t = yyvsp[-2].pTokenList; t; t = t->next ) {
      if ((tt = find_branch( t->item, main_type_tree()))) {
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
   * type 
   */
  yyval.pFactList = new_FactList();
  yyval.pFactList->item = new_TokenList();
  yyval.pFactList->item->item = new_Token( strlen(yyvsp[-4].string)+1 );
  strcpy( yyval.pFactList->item->item, yyvsp[-4].string );
  yyval.pFactList->item->next = new_TokenList();
  yyval.pFactList->item->next->item = s;
  yyval.pFactList->next = yyvsp[0].pFactList;
;}
    break;

  case 68:
#line 800 "scan-ops_pddl.y"
    {
  yyval.pFactList = new_FactList();
  yyval.pFactList->item = new_TokenList();
  yyval.pFactList->item->item = new_Token( strlen(yyvsp[-2].string)+1 );
  strcpy( yyval.pFactList->item->item, yyvsp[-2].string );
  yyval.pFactList->item->next = new_TokenList();
  yyval.pFactList->item->next->item = new_Token( strlen(yyvsp[-1].string)+1 );
  strcpy( yyval.pFactList->item->next->item, yyvsp[-1].string );
  yyval.pFactList->next = yyvsp[0].pFactList;
;}
    break;

  case 69:
#line 812 "scan-ops_pddl.y"
    {
  yyval.pFactList = new_FactList();
  yyval.pFactList->item = new_TokenList();
  yyval.pFactList->item->item = new_Token( strlen(yyvsp[-1].string)+1 );
  strcpy( yyval.pFactList->item->item, yyvsp[-1].string );
  yyval.pFactList->item->next = new_TokenList();
  if ( yyvsp[0].pFactList ) {/* another element (already typed) is following */
    char *s = yyvsp[0].pFactList->item->next->item;
    yyval.pFactList->item->next->item = new_Token(strlen(s) + 1);
    strcpy( yyval.pFactList->item->next->item, s ); /* same type as the next one */
    yyval.pFactList->next = yyvsp[0].pFactList;
  } else {/* no further element - it must be an untyped list */
    yyval.pFactList->item->next->item = new_Token( strlen(STANDARD_TYPE)+1 );
    strcpy( yyval.pFactList->item->next->item, STANDARD_TYPE );
    yyval.pFactList->next = yyvsp[0].pFactList;
  }
;}
    break;

  case 70:
#line 835 "scan-ops_pddl.y"
    { yyval.pFactList = NULL; ;}
    break;

  case 71:
#line 838 "scan-ops_pddl.y"
    { /* this is a very, very special case... it may mean that
   * a parameter has one of the types in name_plus
   * => build a new type name: either_name1_name2_..._namen and check
   * if a type with this name already exists. If so then NAME has this type,
   * if not then build a new type, include it in the type tree and
   * return NAME with the new type 
   * The new artificial type is not a subtype of OBJECT because its own
   * elements must already be instances of a subtype of OBJECT 
   */
  char *s;
  TokenList *t, *t1;
  type_tree tt;
  type_tree_list rootl, *st;
  
  s = new_Token( MAX_LENGTH );
  strcpy( s, EITHER_STR );
  for ( t = yyvsp[-2].pTokenList; t; t = t->next ) {
    strcat( s, CONNECTOR );
    strcat( s, t->item );
  }
  tt = NULL;
  for ( rootl = gglobal_type_tree_list; rootl; rootl = rootl->next ) {
    if ((tt = find_branch( s, rootl->item))) break;
  }
  if ( !tt ) {/* the type doesn't exist yet, so build it */
    rootl = new_type_tree_list( s );
    rootl->next = gglobal_type_tree_list;
    st = &(rootl->item->sub_types);
    for ( t = yyvsp[-2].pTokenList; t; t = t->next ) {
      if ((tt = find_branch( t->item, main_type_tree()))) {
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
   * type 
   */
  yyval.pFactList = new_FactList();
  yyval.pFactList->item = new_TokenList();
  yyval.pFactList->item->item = new_Token( strlen(yyvsp[-4].string)+1 );
  strcpy( yyval.pFactList->item->item, yyvsp[-4].string );
  yyval.pFactList->item->next = new_TokenList();
  yyval.pFactList->item->next->item = s;
  yyval.pFactList->next = yyvsp[0].pFactList;
;}
    break;

  case 72:
#line 897 "scan-ops_pddl.y"
    {
  yyval.pFactList = new_FactList();
  yyval.pFactList->item = new_TokenList();
  yyval.pFactList->item->item = new_Token( strlen(yyvsp[-2].string)+1 );
  strcpy( yyval.pFactList->item->item, yyvsp[-2].string );
  yyval.pFactList->item->next = new_TokenList();
  yyval.pFactList->item->next->item = new_Token( strlen(yyvsp[-1].string)+1 );
  strcpy( yyval.pFactList->item->next->item, yyvsp[-1].string );
  yyval.pFactList->next = yyvsp[0].pFactList;
;}
    break;

  case 73:
#line 909 "scan-ops_pddl.y"
    {
  yyval.pFactList = new_FactList();
  yyval.pFactList->item = new_TokenList();
  yyval.pFactList->item->item = new_Token( strlen(yyvsp[-1].string)+1 );
  strcpy( yyval.pFactList->item->item, yyvsp[-1].string );
  yyval.pFactList->item->next = new_TokenList();
  if ( yyvsp[0].pFactList ) {/* another element (already typed) is following */
    char *s = yyvsp[0].pFactList->item->next->item;
    int l = strlen( s );
    yyval.pFactList->item->next->item = new_Token( l+1 );
    strcpy( yyval.pFactList->item->next->item, s ); /* same type as the next one */
    yyval.pFactList->next = yyvsp[0].pFactList;
  } else {/* no further element - it must be an untyped list */
    yyval.pFactList->item->next->item = new_Token( strlen(STANDARD_TYPE)+1 );
    strcpy( yyval.pFactList->item->next->item, STANDARD_TYPE );
    yyval.pFactList->next = yyvsp[0].pFactList;
  }
;}
    break;


    }

/* Line 991 of yacc.c.  */
#line 2027 "scan-ops_pddl.tab.c"

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
#if defined (__GNUC_MINOR__) && 2093 <= (__GNUC__ * 1000 + __GNUC_MINOR__) \
    && !defined __cplusplus
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


#line 929 "scan-ops_pddl.y"

#include "lex.ops_pddl.c"


/**********************************************************************
 * Functions
 **********************************************************************/

/* 
 * call	bison -pops -bscan-ops scan-ops.y
 */

void opserr( int errno, char *par )

{

  sact_err = errno;

  if ( sact_err_par ) {
    free(sact_err_par);
  }
  if ( par ) {
    sact_err_par = new_Token(strlen(par)+1);
    strcpy(sact_err_par, par);
  } else {
    sact_err_par = NULL;
  }

}
  


int yyerror( char *msg )

{

  fflush(stdout);
  fprintf(stderr, "\n%s: syntax error in line %d, '%s':\n", 
	  gact_filename, lineno, yytext);

  if ( NULL != sact_err_par ) {
    fprintf(stderr, "%s %s\n", serrmsg[sact_err], sact_err_par);
  } else {
    fprintf(stderr, "%s\n", serrmsg[sact_err]);
  }

  exit( 1 );

}



void load_ops_file( char *filename )

{

  FILE * fp;/* pointer to input files */
  char tmp[MAX_LENGTH] = "";

  /* open operator file 
   */
  if( ( fp = fopen( filename, "r" ) ) == NULL ) {
    sprintf(tmp, "\nff: can't find operator file: %s\n\n", filename );
    perror(tmp);
    exit( 1 );
  }

  gact_filename = filename;
  lineno = 1; 
  yyin = fp;

  yyparse();

  fclose( fp );/* and close file again */

}


