/* A Bison parser, made by GNU Bison 3.0.2.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2013 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "3.0.2"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1




/* Copy the first part of user declarations.  */
#line 1 "parser.y" /* yacc.c:339  */

/*
**
** Universidad Simon Bolivar, 1999, 2000 (c)
** Blai Bonet and Hector Geffner. 1999, 2000.
**
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <limits.h>
#include <ulimit.h>
#include <fcntl.h>
#include <assert.h>

#include "parser.h"
#include "planner.h"


/* global data */
static int                domainParsed = 0;
static int                maxParameters = 0;
static int                numberObjects = 0;
static idList_t *         types = NULL;
static idList_t *         objects = NULL;
static schema_t *         schemas = NULL;
static formula_t *        predicates = NULL;
static formula_t *        initialSituation = NULL;
static formula_t *        goalSituation = NULL;
static formula_t *        instantiated;


/* prototypes */
static void     checkRequirement( int );
static char *   translate( char * );
static void     printFormula( formula_t * );
static void     printAtomicFormula( atomicFormula_t * ); 
       int      evaluateFormula( formula_t *, atom_t *, int *, int );

static void     schemaPostProcessing( register schema_t * );
static void     instantiateFormula( register formula_t *, register idList_t *, register instantiation_t * );
static void     idMatchArgsVars( formula_t *, idList_t * );
static void     tagFormulaList( formula_t * );
static void     generateNames( void );
static void     identifyObjects( void );
       int      yylex( void );
       int      yyerror( char * );

#line 120 "parser.tab.c" /* yacc.c:339  */

# ifndef YY_NULLPTR
#  if defined __cplusplus && 201103L <= __cplusplus
#   define YY_NULLPTR nullptr
#  else
#   define YY_NULLPTR 0
#  endif
# endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif

/* In a future release of Bison, this section will be replaced
   by #include "parser.tab.h".  */
#ifndef YY_YY_PARSER_TAB_H_INCLUDED
# define YY_YY_PARSER_TAB_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 1
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    LEFTPAR = 258,
    RIGHTPAR = 259,
    NAME = 260,
    TWODOTS = 261,
    HYPHEN = 262,
    EITHER = 263,
    DEFINE = 264,
    DOMAIN = 265,
    REQUIREMENTS = 266,
    CONSTANTS = 267,
    PREDICATES = 268,
    QUESTION = 269,
    STRIPS = 270,
    EQUALITY = 271,
    CONDITIONAL = 272,
    DOM_AXIOMS = 273,
    DISJUNCTIVE = 274,
    ADL = 275,
    E_PRECONDITIONS = 276,
    U_PRECONDITIONS = 277,
    AND = 278,
    NOT = 279,
    FORALL = 280,
    WHEN = 281,
    EQUAL = 282,
    ACTION = 283,
    PARAMETERS = 284,
    PRECONDITION = 285,
    EFFECT = 286,
    PROBLEM = 287,
    INIT = 288,
    GOAL = 289,
    LENGTH = 290,
    SITUATION = 291,
    OBJECTS = 292,
    SERIAL = 293,
    PARALLEL = 294,
    INTEGER = 295,
    TYPING = 296,
    TYPES = 297
  };
#endif

/* Value type.  */


extern YYSTYPE yylval;

int yyparse (void);

#endif /* !YY_YY_PARSER_TAB_H_INCLUDED  */

/* Copy the second part of user declarations.  */

#line 209 "parser.tab.c" /* yacc.c:358  */

#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 yytype_uint8;
#else
typedef unsigned char yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 yytype_uint16;
#else
typedef unsigned short int yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short int yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif

#ifndef YY_ATTRIBUTE
# if (defined __GNUC__                                               \
      && (2 < __GNUC__ || (__GNUC__ == 2 && 96 <= __GNUC_MINOR__)))  \
     || defined __SUNPRO_C && 0x5110 <= __SUNPRO_C
#  define YY_ATTRIBUTE(Spec) __attribute__(Spec)
# else
#  define YY_ATTRIBUTE(Spec) /* empty */
# endif
#endif

#ifndef YY_ATTRIBUTE_PURE
# define YY_ATTRIBUTE_PURE   YY_ATTRIBUTE ((__pure__))
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# define YY_ATTRIBUTE_UNUSED YY_ATTRIBUTE ((__unused__))
#endif

#if !defined _Noreturn \
     && (!defined __STDC_VERSION__ || __STDC_VERSION__ < 201112)
# if defined _MSC_VER && 1200 <= _MSC_VER
#  define _Noreturn __declspec (noreturn)
# else
#  define _Noreturn YY_ATTRIBUTE ((__noreturn__))
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(E) ((void) (E))
#else
# define YYUSE(E) /* empty */
#endif

#if defined __GNUC__ && 407 <= __GNUC__ * 100 + __GNUC_MINOR__
/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN \
    _Pragma ("GCC diagnostic push") \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")\
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# define YY_IGNORE_MAYBE_UNINITIALIZED_END \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif


#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
             && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYSIZE_T yynewbytes;                                            \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / sizeof (*yyptr);                          \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, (Count) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYSIZE_T yyi;                         \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  3
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   213

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  43
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  59
/* YYNRULES -- Number of rules.  */
#define YYNRULES  115
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  216

/* YYTRANSLATE[YYX] -- Symbol number corresponding to YYX as returned
   by yylex, with out-of-bounds checking.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   297

#define YYTRANSLATE(YYX)                                                \
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, without out-of-bounds checking.  */
static const yytype_uint8 yytranslate[] =
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
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,    89,    89,    93,    94,    95,    99,   104,   110,    98,
     129,   130,   133,   134,   135,   136,   139,   142,   146,   152,
     156,   160,   177,   181,   185,   189,   193,   197,   203,   209,
     222,   231,   238,   239,   243,   244,   247,   256,   265,   274,
     283,   293,   298,   308,   313,   316,   327,   344,   352,   363,
     375,   376,   383,   384,   387,   390,   484,   490,   495,   502,
     509,   518,   530,   535,   538,   546,   552,   553,   560,   568,
     579,   587,   598,   608,   621,   633,   643,   648,   649,   652,
     656,   661,   665,   673,   677,   682,   710,   739,   778,   786,
     790,   797,   805,   809,   820,   825,   835,   834,   843,   844,
     847,   848,   849,   850,   851,   852,   853,   866,   869,   879,
     911,   915,   916,   919,   920,   923
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || 0
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "LEFTPAR", "RIGHTPAR", "NAME", "TWODOTS",
  "HYPHEN", "EITHER", "DEFINE", "DOMAIN", "REQUIREMENTS", "CONSTANTS",
  "PREDICATES", "QUESTION", "STRIPS", "EQUALITY", "CONDITIONAL",
  "DOM_AXIOMS", "DISJUNCTIVE", "ADL", "E_PRECONDITIONS", "U_PRECONDITIONS",
  "AND", "NOT", "FORALL", "WHEN", "EQUAL", "ACTION", "PARAMETERS",
  "PRECONDITION", "EFFECT", "PROBLEM", "INIT", "GOAL", "LENGTH",
  "SITUATION", "OBJECTS", "SERIAL", "PARALLEL", "INTEGER", "TYPING",
  "TYPES", "$accept", "start", "list_domain_problem", "domain", "$@1",
  "$@2", "$@3", "list_def", "definition", "require_def",
  "list_require_key", "require_key", "types_def", "constants_def", "type",
  "plain_or_typed_list_name", "plain_or_typed_list_var", "typed_list_name",
  "typed_list_var", "list_name", "list_var", "name", "variable",
  "predicates_def", "list_atomic_formula_skeleton",
  "atomic_formula_skeleton", "predicate", "list_structure_def",
  "structure_def", "schema_def", "list_schema_body", "schema_body",
  "list_parameters", "list_formula", "formula", "literal", "list_literal",
  "list_atomic_formula_name", "atomic_formula_name", "atomic_formula_term",
  "list_term", "term", "effect_formula", "list_one_effect_formula",
  "one_effect_formula", "list_atomic_effect", "goal_formula",
  "list_goal_formula", "problem", "$@4", "list_def_prob", "def_prob",
  "situation_def", "init_def", "goal_def", "length_def",
  "list_length_def_aux", "length_def_aux", "number", YY_NULLPTR
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[NUM] -- (External) token number corresponding to the
   (internal) symbol number NUM (which must be that of a token).  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
     295,   296,   297
};
# endif

#define YYPACT_NINF -186

#define yypact_value_is_default(Yystate) \
  (!!((Yystate) == (-186)))

#define YYTABLE_NINF -44

#define yytable_value_is_error(Yytable_value) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
    -186,    33,    48,  -186,    91,  -186,  -186,   103,     6,   138,
     138,  -186,   130,   136,  -186,  -186,   141,  -186,    -2,    99,
     140,   138,   142,   138,    -2,   143,    59,  -186,  -186,  -186,
    -186,  -186,  -186,  -186,  -186,  -186,  -186,  -186,  -186,     3,
    -186,  -186,   144,  -186,   145,   138,    36,  -186,   109,  -186,
    -186,   146,   119,   148,  -186,   153,   140,   150,   161,  -186,
     138,   138,  -186,  -186,  -186,    68,  -186,  -186,  -186,   151,
    -186,  -186,  -186,   138,   163,   164,  -186,  -186,   138,  -186,
      36,   150,  -186,   165,    55,  -186,  -186,   166,  -186,  -186,
     112,   167,  -186,   168,   159,    70,  -186,   138,   169,  -186,
     170,   151,  -186,   119,   171,  -186,   172,    93,  -186,  -186,
     161,   175,   176,    69,  -186,    84,  -186,  -186,  -186,  -186,
     138,  -186,   145,  -186,  -186,    68,  -186,    80,   178,  -186,
    -186,  -186,   179,  -186,   121,    36,   180,   151,  -186,  -186,
     181,    69,   134,   134,   182,    50,   177,   184,   185,  -186,
    -186,  -186,  -186,  -186,  -186,   186,  -186,  -186,  -186,   187,
     188,  -186,  -186,   170,   151,    81,  -186,  -186,    23,  -186,
    -186,  -186,   184,  -186,  -186,  -186,   189,   184,   123,   191,
     184,   192,  -186,   126,  -186,  -186,   129,  -186,   151,   194,
    -186,  -186,  -186,  -186,  -186,   195,    94,  -186,   196,   198,
     133,  -186,    61,   199,     8,  -186,  -186,   135,   184,  -186,
    -186,  -186,   194,   200,   201,  -186
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       5,     0,     2,     1,     0,     4,     3,     0,     0,     0,
       0,    44,     0,     0,     6,    96,     0,    99,     0,     0,
       0,    41,     0,    41,     7,     0,     0,    97,    98,    19,
      21,    22,    23,    24,    25,    26,    27,    20,    12,     0,
      18,    14,     0,    32,    33,    41,     0,    15,     0,    48,
      13,     0,     0,     0,    11,     0,     0,     0,     0,   112,
       0,    41,    16,    17,    29,     0,    40,    51,    50,    43,
      46,    47,    28,     0,     8,     0,    54,    10,     0,   100,
       0,   108,    71,     0,     0,    91,    66,     0,   103,   104,
       0,     0,   101,     0,     0,    37,    30,     0,     0,    34,
      35,    43,    57,     0,     0,    53,     0,     0,    70,   102,
       0,     0,     0,    76,   109,     0,   110,   111,   107,   106,
      41,    36,     0,    45,    49,     0,    42,    55,     0,     9,
     105,    73,     0,    95,     0,     0,     0,    43,    77,    78,
       0,    76,     0,     0,     0,    39,     0,     0,     0,    56,
      52,    72,    92,    94,    67,     0,    74,    75,   115,     0,
       0,    31,    38,     0,    43,     0,    59,    64,     0,    82,
      60,    79,     0,   113,   114,    61,     0,     0,     0,     0,
       0,     0,    58,     0,    63,    84,     0,    81,    43,     0,
      93,    65,    62,    83,    80,     0,     0,    88,     0,     0,
       0,    85,     0,     0,     0,    90,    69,     0,     0,    86,
      89,    68,     0,     0,     0,    87
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -186,  -186,  -186,  -186,  -186,  -186,  -186,  -186,   174,   139,
    -186,   173,  -186,  -186,    64,   -18,  -135,   107,    62,   -39,
    -100,    -6,   -83,  -186,  -186,   158,   -38,  -186,   105,  -186,
    -186,  -186,  -186,  -186,  -146,   -58,  -186,  -186,   128,   100,
      72,  -186,  -186,  -186,  -117,  -185,   -98,  -186,  -186,  -186,
    -186,  -186,  -186,  -186,  -186,  -186,  -186,  -186,    67
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     1,     2,     5,    16,    52,   104,    24,    25,    38,
      39,    40,    50,    41,    95,    42,    98,    43,    99,    44,
     100,    68,   101,    47,    48,    49,   113,    74,    75,    76,
     127,   149,   176,   183,   166,   167,   207,    81,    82,    86,
     140,   141,   170,   186,   171,   198,    87,   134,     6,    17,
      19,    28,    92,    83,    88,    89,    90,   117,   159
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      85,   126,   155,    12,    13,    51,    66,    62,    69,    20,
      21,    22,   133,    11,   203,    45,     9,    45,    29,    30,
      31,    32,    33,    34,    35,    36,   181,   213,    11,   175,
     139,   184,   111,     3,   189,    67,   153,   192,    10,    45,
      23,    11,   107,    93,    37,   163,   178,   111,   179,   180,
      67,     4,    85,   195,    91,    45,   122,   -43,   139,    96,
      11,   187,   212,    67,    97,    55,    11,   102,   132,   194,
      56,    94,   106,    11,    11,    11,    85,   -41,   110,   111,
     112,   144,    67,    97,   200,   111,    11,   208,    67,    45,
     169,   123,    57,    58,    59,    60,    61,   131,    11,    11,
       7,    45,    26,    27,   177,   111,     8,   138,    67,   146,
     147,   148,    46,    70,    45,   115,   116,   200,   111,    96,
     169,    67,   142,   143,    84,   152,   168,   185,   169,   165,
     191,   197,   168,   193,    14,   138,   204,   205,   204,   210,
      15,   197,   206,    11,    18,    46,    54,    73,    64,   211,
      72,    77,    65,    80,   197,    29,    30,    31,    32,    33,
      34,    35,    36,    78,    84,    97,   103,   120,   105,   109,
     114,   118,   119,   124,   158,   129,   130,   125,   135,   137,
     164,    37,   150,   151,   154,   156,   161,   165,   168,   145,
     172,   173,   174,   182,   188,    79,   190,   196,    53,   199,
     201,   202,   121,   209,   214,   215,    71,   162,   128,   108,
     160,   136,    63,   157
};

static const yytype_uint8 yycheck[] =
{
      58,   101,   137,     9,    10,    23,    45,     4,    46,    11,
      12,    13,   110,     5,   199,    21,    10,    23,    15,    16,
      17,    18,    19,    20,    21,    22,   172,   212,     5,   164,
     113,   177,    24,     0,   180,    27,   134,   183,    32,    45,
      42,     5,    80,    61,    41,   145,    23,    24,    25,    26,
      27,     3,   110,   188,    60,    61,    95,     7,   141,    65,
       5,   178,   208,    27,    14,     6,     5,    73,   107,   186,
      11,     3,    78,     5,     5,     5,   134,     7,    23,    24,
      25,   120,    27,    14,    23,    24,     5,    26,    27,    95,
     148,    97,    33,    34,    35,    36,    37,     4,     5,     5,
       9,   107,     3,     4,    23,    24,     3,   113,    27,    29,
      30,    31,     3,     4,   120,     3,     4,    23,    24,   125,
     178,    27,    38,    39,     3,     4,     3,     4,   186,     3,
       4,   189,     3,     4,     4,   141,     3,     4,     3,     4,
       4,   199,   200,     5,     3,     3,     3,    28,     4,   207,
       4,     3,     7,     3,   212,    15,    16,    17,    18,    19,
      20,    21,    22,    10,     3,    14,     3,     8,     4,     4,
       4,     4,     4,     4,    40,     4,     4,     7,     3,     3,
       3,    41,     4,     4,     4,     4,     4,     3,     3,   125,
       4,     4,     4,     4,     3,    56,     4,     3,    24,     4,
       4,     3,    95,     4,     4,     4,    48,   145,   103,    81,
     143,   111,    39,   141
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,    44,    45,     0,     3,    46,    91,     9,     3,    10,
      32,     5,    64,    64,     4,     4,    47,    92,     3,    93,
      11,    12,    13,    42,    50,    51,     3,     4,    94,    15,
      16,    17,    18,    19,    20,    21,    22,    41,    52,    53,
      54,    56,    58,    60,    62,    64,     3,    66,    67,    68,
      55,    58,    48,    51,     3,     6,    11,    33,    34,    35,
      36,    37,     4,    54,     4,     7,    62,    27,    64,    69,
       4,    68,     4,    28,    70,    71,    72,     3,    10,    52,
       3,    80,    81,    96,     3,    78,    82,    89,    97,    98,
      99,    64,    95,    58,     3,    57,    64,    14,    59,    61,
      63,    65,    64,     3,    49,     4,    64,    69,    81,     4,
      23,    24,    25,    69,     4,     3,     4,   100,     4,     4,
       8,    60,    62,    64,     4,     7,    63,    73,    71,     4,
       4,     4,    62,    89,    90,     3,    82,     3,    64,    65,
      83,    84,    38,    39,    62,    57,    29,    30,    31,    74,
       4,     4,     4,    89,     4,    59,     4,    83,    40,   101,
     101,     4,    61,    63,     3,     3,    77,    78,     3,    78,
      85,    87,     4,     4,     4,    59,    75,    23,    23,    25,
      26,    77,     4,    76,    77,     4,    86,    87,     3,    77,
       4,     4,    77,     4,    87,    59,     3,    78,    88,     4,
      23,     4,     3,    88,     3,     4,    78,    79,    26,     4,
       4,    78,    77,    88,     4,     4
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    43,    44,    45,    45,    45,    47,    48,    49,    46,
      50,    50,    51,    51,    51,    51,    52,    53,    53,    54,
      54,    54,    54,    54,    54,    54,    54,    54,    55,    56,
      57,    57,    58,    58,    59,    59,    60,    60,    61,    61,
      62,    62,    63,    63,    64,    65,    66,    67,    67,    68,
      69,    69,    70,    70,    71,    72,    73,    73,    74,    74,
      74,    75,    76,    76,    77,    77,    78,    78,    79,    79,
      80,    80,    81,    81,    82,    83,    83,    84,    84,    85,
      86,    86,    87,    87,    87,    87,    87,    87,    88,    88,
      88,    89,    89,    89,    90,    90,    92,    91,    93,    93,
      94,    94,    94,    94,    94,    94,    94,    95,    96,    97,
      98,    99,    99,   100,   100,   101
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     1,     2,     2,     0,     0,     0,     0,    13,
       3,     2,     2,     2,     2,     2,     2,     2,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     2,     2,
       1,     4,     1,     1,     1,     1,     4,     3,     4,     3,
       2,     0,     2,     0,     1,     2,     2,     2,     1,     4,
       1,     1,     4,     2,     1,     3,     2,     0,     4,     2,
       2,     1,     2,     1,     1,     4,     1,     4,     2,     1,
       2,     1,     4,     3,     4,     2,     0,     1,     1,     1,
       2,     1,     1,     4,     3,     5,     7,    11,     1,     4,
       3,     1,     4,     7,     2,     1,     0,     9,     2,     0,
       3,     3,     4,     3,     3,     5,     4,     2,     1,     2,
       2,     2,     0,     4,     4,     1
};


#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)
#define YYEMPTY         (-2)
#define YYEOF           0

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                  \
do                                                              \
  if (yychar == YYEMPTY)                                        \
    {                                                           \
      yychar = (Token);                                         \
      yylval = (Value);                                         \
      YYPOPSTACK (yylen);                                       \
      yystate = *yyssp;                                         \
      goto yybackup;                                            \
    }                                                           \
  else                                                          \
    {                                                           \
      yyerror (YY_("syntax error: cannot back up")); \
      YYERROR;                                                  \
    }                                                           \
while (0)

/* Error token number */
#define YYTERROR        1
#define YYERRCODE       256



/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)

/* This macro is provided for backward compatibility. */
#ifndef YY_LOCATION_PRINT
# define YY_LOCATION_PRINT(File, Loc) ((void) 0)
#endif


# define YY_SYMBOL_PRINT(Title, Type, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Type, Value); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*----------------------------------------.
| Print this symbol's value on YYOUTPUT.  |
`----------------------------------------*/

static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
{
  FILE *yyo = yyoutput;
  YYUSE (yyo);
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# endif
  YYUSE (yytype);
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
{
  YYFPRINTF (yyoutput, "%s %s (",
             yytype < YYNTOKENS ? "token" : "nterm", yytname[yytype]);

  yy_symbol_value_print (yyoutput, yytype, yyvaluep);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yytype_int16 *yybottom, yytype_int16 *yytop)
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yytype_int16 *yyssp, YYSTYPE *yyvsp, int yyrule)
{
  unsigned long int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       yystos[yyssp[yyi + 1 - yynrhs]],
                       &(yyvsp[(yyi + 1) - (yynrhs)])
                                              );
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, Rule); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif


#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
static YYSIZE_T
yystrlen (const char *yystr)
{
  YYSIZE_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
static char *
yystpcpy (char *yydest, const char *yysrc)
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYSIZE_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
        switch (*++yyp)
          {
          case '\'':
          case ',':
            goto do_not_strip_quotes;

          case '\\':
            if (*++yyp != '\\')
              goto do_not_strip_quotes;
            /* Fall through.  */
          default:
            if (yyres)
              yyres[yyn] = *yyp;
            yyn++;
            break;

          case '"':
            if (yyres)
              yyres[yyn] = '\0';
            return yyn;
          }
    do_not_strip_quotes: ;
    }

  if (! yyres)
    return yystrlen (yystr);

  return yystpcpy (yyres, yystr) - yyres;
}
# endif

/* Copy into *YYMSG, which is of size *YYMSG_ALLOC, an error message
   about the unexpected token YYTOKEN for the state stack whose top is
   YYSSP.

   Return 0 if *YYMSG was successfully written.  Return 1 if *YYMSG is
   not large enough to hold the message.  In that case, also set
   *YYMSG_ALLOC to the required number of bytes.  Return 2 if the
   required number of bytes is too large to store.  */
static int
yysyntax_error (YYSIZE_T *yymsg_alloc, char **yymsg,
                yytype_int16 *yyssp, int yytoken)
{
  YYSIZE_T yysize0 = yytnamerr (YY_NULLPTR, yytname[yytoken]);
  YYSIZE_T yysize = yysize0;
  enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
  /* Internationalized format string. */
  const char *yyformat = YY_NULLPTR;
  /* Arguments of yyformat. */
  char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
  /* Number of reported tokens (one for the "unexpected", one per
     "expected"). */
  int yycount = 0;

  /* There are many possibilities here to consider:
     - If this state is a consistent state with a default action, then
       the only way this function was invoked is if the default action
       is an error action.  In that case, don't check for expected
       tokens because there are none.
     - The only way there can be no lookahead present (in yychar) is if
       this state is a consistent state with a default action.  Thus,
       detecting the absence of a lookahead is sufficient to determine
       that there is no unexpected or expected token to report.  In that
       case, just report a simple "syntax error".
     - Don't assume there isn't a lookahead just because this state is a
       consistent state with a default action.  There might have been a
       previous inconsistent state, consistent state with a non-default
       action, or user semantic action that manipulated yychar.
     - Of course, the expected token list depends on states to have
       correct lookahead information, and it depends on the parser not
       to perform extra reductions after fetching a lookahead from the
       scanner and before detecting a syntax error.  Thus, state merging
       (from LALR or IELR) and default reductions corrupt the expected
       token list.  However, the list is correct for canonical LR with
       one exception: it will still contain any token that will not be
       accepted due to an error action in a later state.
  */
  if (yytoken != YYEMPTY)
    {
      int yyn = yypact[*yyssp];
      yyarg[yycount++] = yytname[yytoken];
      if (!yypact_value_is_default (yyn))
        {
          /* Start YYX at -YYN if negative to avoid negative indexes in
             YYCHECK.  In other words, skip the first -YYN actions for
             this state because they are default actions.  */
          int yyxbegin = yyn < 0 ? -yyn : 0;
          /* Stay within bounds of both yycheck and yytname.  */
          int yychecklim = YYLAST - yyn + 1;
          int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
          int yyx;

          for (yyx = yyxbegin; yyx < yyxend; ++yyx)
            if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR
                && !yytable_value_is_error (yytable[yyx + yyn]))
              {
                if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
                  {
                    yycount = 1;
                    yysize = yysize0;
                    break;
                  }
                yyarg[yycount++] = yytname[yyx];
                {
                  YYSIZE_T yysize1 = yysize + yytnamerr (YY_NULLPTR, yytname[yyx]);
                  if (! (yysize <= yysize1
                         && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
                    return 2;
                  yysize = yysize1;
                }
              }
        }
    }

  switch (yycount)
    {
# define YYCASE_(N, S)                      \
      case N:                               \
        yyformat = S;                       \
      break
      YYCASE_(0, YY_("syntax error"));
      YYCASE_(1, YY_("syntax error, unexpected %s"));
      YYCASE_(2, YY_("syntax error, unexpected %s, expecting %s"));
      YYCASE_(3, YY_("syntax error, unexpected %s, expecting %s or %s"));
      YYCASE_(4, YY_("syntax error, unexpected %s, expecting %s or %s or %s"));
      YYCASE_(5, YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s"));
# undef YYCASE_
    }

  {
    YYSIZE_T yysize1 = yysize + yystrlen (yyformat);
    if (! (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
      return 2;
    yysize = yysize1;
  }

  if (*yymsg_alloc < yysize)
    {
      *yymsg_alloc = 2 * yysize;
      if (! (yysize <= *yymsg_alloc
             && *yymsg_alloc <= YYSTACK_ALLOC_MAXIMUM))
        *yymsg_alloc = YYSTACK_ALLOC_MAXIMUM;
      return 1;
    }

  /* Avoid sprintf, as that infringes on the user's name space.
     Don't have undefined behavior even if the translation
     produced a string with the wrong number of "%s"s.  */
  {
    char *yyp = *yymsg;
    int yyi = 0;
    while ((*yyp = *yyformat) != '\0')
      if (*yyp == '%' && yyformat[1] == 's' && yyi < yycount)
        {
          yyp += yytnamerr (yyp, yyarg[yyi++]);
          yyformat += 2;
        }
      else
        {
          yyp++;
          yyformat++;
        }
  }
  return 0;
}
#endif /* YYERROR_VERBOSE */

/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep)
{
  YYUSE (yyvaluep);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YYUSE (yytype);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}




/* The lookahead symbol.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;
/* Number of syntax errors so far.  */
int yynerrs;


/*----------.
| yyparse.  |
`----------*/

int
yyparse (void)
{
    int yystate;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       'yyss': related to states.
       'yyvs': related to semantic values.

       Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* The state stack.  */
    yytype_int16 yyssa[YYINITDEPTH];
    yytype_int16 *yyss;
    yytype_int16 *yyssp;

    /* The semantic value stack.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs;
    YYSTYPE *yyvsp;

    YYSIZE_T yystacksize;

  int yyn;
  int yyresult;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken = 0;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;

#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  yyssp = yyss = yyssa;
  yyvsp = yyvs = yyvsa;
  yystacksize = YYINITDEPTH;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY; /* Cause a token to be read.  */
  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        YYSTYPE *yyvs1 = yyvs;
        yytype_int16 *yyss1 = yyss;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * sizeof (*yyssp),
                    &yyvs1, yysize * sizeof (*yyvsp),
                    &yystacksize);

        yyss = yyss1;
        yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yytype_int16 *yyss1 = yyss;
        union yyalloc *yyptr =
          (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
        if (! yyptr)
          goto yyexhaustedlab;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
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

  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = yylex ();
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token.  */
  yychar = YYEMPTY;

  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

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
     '$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 6:
#line 99 "parser.y" /* yacc.c:1646  */
    {
			   _low_domainName = (yyvsp[-1].ident);
			   strcpy( _low_domainName, translate( _low_domainName ) );
			 }
#line 1453 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 7:
#line 104 "parser.y" /* yacc.c:1646  */
    {
			   identifyObjects();
			   tagFormulaList( initialSituation );
			   tagFormulaList( goalSituation );
			 }
#line 1463 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 8:
#line 110 "parser.y" /* yacc.c:1646  */
    {
			   schema_t *schema;
			   idList_t *var;
			   int i;

			   /* how much parameters can an schema have? */
			   for( schema = schemas; schema != NULL; schema = schema->next )
			     {
			       for( var = schema->vars, i = 0; var != NULL; var = var->next, ++i );
			       maxParameters = (maxParameters > i ? maxParameters : i);
			     }
			   generateNames();
			 }
#line 1481 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 9:
#line 124 "parser.y" /* yacc.c:1646  */
    {
			 domainParsed = 1;
		       }
#line 1489 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 17:
#line 143 "parser.y" /* yacc.c:1646  */
    {
			 checkRequirement( (yyvsp[0].integer) );
		       }
#line 1497 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 18:
#line 147 "parser.y" /* yacc.c:1646  */
    {
			 checkRequirement( (yyvsp[0].integer) );
		       }
#line 1505 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 19:
#line 153 "parser.y" /* yacc.c:1646  */
    {
			 (yyval.integer) = STRIPS;
		       }
#line 1513 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 20:
#line 157 "parser.y" /* yacc.c:1646  */
    {
			 (yyval.integer) = TYPING;
		       }
#line 1521 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 21:
#line 161 "parser.y" /* yacc.c:1646  */
    {
			 (yyval.integer) = EQUALITY;

			 /* create the equal predicate */
			 predicates = (formula_t*)malloc( sizeof( formula_t ) );
			 assert( predicates != NULL );
			 predicates->next = NULL;
			 predicates->type = ATOMIC_FORMULA;
			 predicates->u.atomic = (atomicFormula_t*)malloc( sizeof( atomicFormula_t ) );	
			 assert( predicates->u.atomic != NULL );
			 predicates->u.atomic->id = 0;
			 predicates->u.atomic->neg = 0;
			 predicates->u.atomic->equal = 1;
			 predicates->u.atomic->name = strdup( "=" );
			 predicates->u.atomic->args = NULL;
		       }
#line 1542 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 22:
#line 178 "parser.y" /* yacc.c:1646  */
    {
			 (yyval.integer) = CONDITIONAL;
		       }
#line 1550 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 23:
#line 182 "parser.y" /* yacc.c:1646  */
    {
			 (yyval.integer) = DOM_AXIOMS;
		       }
#line 1558 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 24:
#line 186 "parser.y" /* yacc.c:1646  */
    {
			 (yyval.integer) = DISJUNCTIVE;
		       }
#line 1566 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 25:
#line 190 "parser.y" /* yacc.c:1646  */
    {
			 (yyval.integer) = ADL;
		       }
#line 1574 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 26:
#line 194 "parser.y" /* yacc.c:1646  */
    {
			 (yyval.integer) = E_PRECONDITIONS;
		       }
#line 1582 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 27:
#line 198 "parser.y" /* yacc.c:1646  */
    {
			 (yyval.integer) = U_PRECONDITIONS;
		       }
#line 1590 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 28:
#line 204 "parser.y" /* yacc.c:1646  */
    {
			 types = (yyvsp[-1].idlist);
		       }
#line 1598 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 29:
#line 210 "parser.y" /* yacc.c:1646  */
    {
			 idList_t *object;
			 if( objects == NULL )
			   objects = (yyvsp[-1].idlist);
			 else
			   {
			     for( object = objects; object->next != NULL; object = object->next );
			     object->next = (yyvsp[-1].idlist);
			   }
		       }
#line 1613 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 30:
#line 223 "parser.y" /* yacc.c:1646  */
    {
			 (yyval.idlist) = (idList_t*) malloc( sizeof( idList_t ) );
			 assert( (yyval.idlist) != NULL );
			 (yyval.idlist)->id = 0;
			 (yyval.idlist)->name = (yyvsp[0].ident);
			 (yyval.idlist)->type = NULL;
			 (yyval.idlist)->next = NULL;
		       }
#line 1626 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 31:
#line 232 "parser.y" /* yacc.c:1646  */
    {
			 (yyval.idlist) = (yyvsp[-1].idlist);
		       }
#line 1634 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 36:
#line 248 "parser.y" /* yacc.c:1646  */
    {
			 idList_t *name;
			 for( name = (yyvsp[-3].idlist); name->next != NULL; name = name->next )
			   name->type = (yyvsp[-1].idlist);
			 name->type = (yyvsp[-1].idlist);
			 name->next = (yyvsp[0].idlist);
			 (yyval.idlist) = (yyvsp[-3].idlist);
		       }
#line 1647 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 37:
#line 257 "parser.y" /* yacc.c:1646  */
    {
			 idList_t *name;
			 for( name = (yyvsp[-2].idlist); name != NULL; name = name->next )
			   name->type = (yyvsp[0].idlist);
			 (yyval.idlist) = (yyvsp[-2].idlist);
		       }
#line 1658 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 38:
#line 266 "parser.y" /* yacc.c:1646  */
    {
			 idList_t *name;
			 for( name = (yyvsp[-3].idlist); name->next != NULL; name = name->next )
			   name->type = (yyvsp[-1].idlist);
			 name->type = (yyvsp[-1].idlist);
			 name->next = (yyvsp[0].idlist);
			 (yyval.idlist) = (yyvsp[-3].idlist);
		       }
#line 1671 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 39:
#line 275 "parser.y" /* yacc.c:1646  */
    {
			 idList_t *name;
			 for( name = (yyvsp[-2].idlist); name != NULL; name = name->next )
			   name->type = (yyvsp[0].idlist);
			 (yyval.idlist) = (yyvsp[-2].idlist);
		       }
#line 1682 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 40:
#line 284 "parser.y" /* yacc.c:1646  */
    {
			 (yyval.idlist) = (idList_t*) malloc( sizeof( idList_t ) );
			 assert( (yyval.idlist) != NULL );
			 (yyval.idlist)->id = 0;
			 (yyval.idlist)->name = (yyvsp[-1].ident);
			 (yyval.idlist)->type = NULL;
			 (yyval.idlist)->next = (yyvsp[0].idlist);
		       }
#line 1695 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 41:
#line 293 "parser.y" /* yacc.c:1646  */
    {
			 (yyval.idlist) = NULL;
		       }
#line 1703 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 42:
#line 299 "parser.y" /* yacc.c:1646  */
    {
			 (yyval.idlist) = (idList_t*) malloc( sizeof( idList_t ) );
			 assert( (yyval.idlist) != NULL );
			 (yyval.idlist)->id = 0;
			 (yyval.idlist)->name = (yyvsp[-1].ident);
			 (yyval.idlist)->type = NULL;
			 (yyval.idlist)->next = (yyvsp[0].idlist);
		       }
#line 1716 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 43:
#line 308 "parser.y" /* yacc.c:1646  */
    {  
			 (yyval.idlist) = NULL;
		       }
#line 1724 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 45:
#line 317 "parser.y" /* yacc.c:1646  */
    {
			 (yyval.ident) = (char*) malloc( strlen( (yyvsp[0].ident) ) + 2 );
			 assert( (yyval.ident) != NULL );
			 (yyval.ident)[0] = '\0';
			 strcat( (yyval.ident), "?" );
			 strcat( (yyval.ident), (yyvsp[0].ident) );
			 free( (yyvsp[0].ident) );
		       }
#line 1737 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 46:
#line 328 "parser.y" /* yacc.c:1646  */
    {
			 int i;
			 formula_t *p;
			 if( predicates == NULL )
			   predicates = (yyvsp[-1].formula);
			 else
			   {
			     for( p = predicates; p->next != NULL; p = p->next );
			     p->next = (yyvsp[-1].formula);
			   }
			 for( p = predicates, i = 1; p != NULL; p = p->next )
			   p->u.atomic->id = i++;
                       }
#line 1755 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 47:
#line 345 "parser.y" /* yacc.c:1646  */
    {
			 (yyval.formula) = (formula_t*)malloc( sizeof( formula_t ) );
			 assert( (yyval.formula) != NULL );
			 (yyval.formula)->type = ATOMIC_FORMULA;
			 (yyval.formula)->u.atomic = (yyvsp[0].atomicFormula);
			 (yyval.formula)->next = (yyvsp[-1].formula);
		       }
#line 1767 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 48:
#line 353 "parser.y" /* yacc.c:1646  */
    {
			 (yyval.formula) = (formula_t*)malloc( sizeof( formula_t ) );
			 assert( (yyval.formula) != NULL );
			 (yyval.formula)->type = ATOMIC_FORMULA;
			 (yyval.formula)->u.atomic = (yyvsp[0].atomicFormula);
			 (yyval.formula)->next = NULL;
		       }
#line 1779 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 49:
#line 364 "parser.y" /* yacc.c:1646  */
    {
			 (yyval.atomicFormula) = (atomicFormula_t*) malloc( sizeof( atomicFormula_t ) );
			 assert( (yyval.atomicFormula) != NULL );
			 (yyval.atomicFormula)->id = -1;
			 (yyval.atomicFormula)->neg = 0;
			 (yyval.atomicFormula)->name = (yyvsp[-2].ident);
			 (yyval.atomicFormula)->equal = 0;
			 (yyval.atomicFormula)->args = (yyvsp[-1].idlist);
		       }
#line 1793 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 51:
#line 377 "parser.y" /* yacc.c:1646  */
    {
			 (yyval.ident) = "=";
		       }
#line 1801 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 55:
#line 391 "parser.y" /* yacc.c:1646  */
    {
			 formula_t *formula, *tmp;
			 schema_t *schema;
			 body_t *body;

			 schema = (schema_t*) malloc( sizeof( schema_t ) );
			 assert( schema != NULL );
			 schema->name = (yyvsp[-1].ident);
			 schema->next = NULL;

			 for( body = (yyvsp[0].body); body != NULL; body = body->next )
			   switch( body->type )
			     {
			     case PARAMETERS:
			       schema->vars = body->data.parameters;
			       break;
			     case PRECONDITION:
			       schema->prec = body->data.precondition;
			       break;
			     case EFFECT:
			       schema->add = NULL;
			       schema->del = NULL;
			       schema->when = NULL;
			       schema->forall = NULL;
			       formula = body->data.effect;
			       while( formula != NULL )
				 {
				   tmp = formula->next;
				   switch( formula->type )
				     {
				     case ATOMIC_FORMULA:
				       if( !formula->u.atomic->neg )
					 {
					   formula->next = schema->add;
					   schema->add = formula;
					 }
				       else
					 {
					   formula->next = schema->del;
					   schema->del = formula;
					 }
				       break;
				     case WHEN_FORMULA:
				       formula->next = schema->when;
				       schema->when = formula;
				       break;
				     case FORALL_FORMULA:
				       formula->next = schema->forall;
				       schema->forall = formula;
				       break;
				     }
				   formula = tmp;
				 }
			       break;
			     }

			 /* postprocessing */
			 schemaPostProcessing( schema );

			 /* identification */
			 idMatchArgsVars( schema->prec, schema->vars );
			 idMatchArgsVars( schema->add, schema->vars );
			 idMatchArgsVars( schema->del, schema->vars );
			 idMatchArgsVars( schema->when, schema->vars );

			 /* link schemas */
			 schema->next = schemas;
			 schemas = schema;

#if 0
			 {
			   idList_t *id;
			   fprintf( stderr, "schema %s after postprocessing:\n", schema->name );
			   fprintf( stderr, "  Params = " );
			   for( id = schema->vars; id != NULL; id = id->next )
			     fprintf( stderr, "%s ", id->name );
			   fprintf( stderr, "\n  Prec = " );
			   printFormula( schema->prec );
			   fprintf( stderr, "\n  Add = " );
			   printFormula( schema->add );
			   fprintf( stderr, "\n  Del = " );
			   printFormula( schema->del );
			   fprintf( stderr, "\n  When = " );
			   printFormula( schema->when );
			   fprintf( stderr, "\n" );
			 }
#endif

			 /* insert schema into schemaTable */
			 _low_schemaTable[numberSchema++] = schema;
		       }
#line 1897 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 56:
#line 485 "parser.y" /* yacc.c:1646  */
    {
			 (yyvsp[0].body)->next = (yyvsp[-1].body);
			 (yyval.body) = (yyvsp[0].body);
		       }
#line 1906 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 57:
#line 490 "parser.y" /* yacc.c:1646  */
    {
			 (yyval.body) = NULL;
		       }
#line 1914 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 58:
#line 496 "parser.y" /* yacc.c:1646  */
    {
			 (yyval.body) = (body_t*) malloc( sizeof( body_t ) );
			 assert( (yyval.body) != NULL );
			 (yyval.body)->type = PARAMETERS;
			 (yyval.body)->data.parameters = (yyvsp[-1].idlist);
		       }
#line 1925 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 59:
#line 503 "parser.y" /* yacc.c:1646  */
    {
			 (yyval.body) = (body_t*) malloc( sizeof( body_t ) );
			 assert( (yyval.body) != NULL );
			 (yyval.body)->type = PRECONDITION;
			 (yyval.body)->data.precondition = (yyvsp[0].formula);
		       }
#line 1936 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 60:
#line 510 "parser.y" /* yacc.c:1646  */
    {
			 (yyval.body) = (body_t*) malloc( sizeof( body_t ) );
			 assert( (yyval.body) != NULL );
			 (yyval.body)->type = EFFECT;
			 (yyval.body)->data.effect = (yyvsp[0].formula);
		       }
#line 1947 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 61:
#line 519 "parser.y" /* yacc.c:1646  */
    {
			 int i;
			 idList_t *var;

			 i = 1;
			 for( var = (yyvsp[0].idlist); var != NULL; var = var->next )
			   var->id = i++;
			 (yyval.idlist) = (yyvsp[0].idlist);
		       }
#line 1961 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 62:
#line 531 "parser.y" /* yacc.c:1646  */
    {
			 (yyvsp[0].formula)->next = (yyvsp[-1].formula);
			 (yyval.formula) = (yyvsp[0].formula);
		       }
#line 1970 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 64:
#line 539 "parser.y" /* yacc.c:1646  */
    {
			 (yyval.formula) = (formula_t*)malloc( sizeof( formula_t ) );
			 assert( (yyval.formula) != NULL );
			 (yyval.formula)->type = ATOMIC_FORMULA;
			 (yyval.formula)->u.atomic = (yyvsp[0].atomicFormula);
			 (yyval.formula)->next = NULL;
		       }
#line 1982 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 65:
#line 547 "parser.y" /* yacc.c:1646  */
    {
			 (yyval.formula) = (yyvsp[-1].formula);
		       }
#line 1990 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 67:
#line 554 "parser.y" /* yacc.c:1646  */
    {
			 (yyvsp[-1].atomicFormula)->neg = 1;
			 (yyval.atomicFormula) = (yyvsp[-1].atomicFormula);
		       }
#line 1999 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 68:
#line 561 "parser.y" /* yacc.c:1646  */
    {
			 (yyval.formula) = (formula_t*)malloc( sizeof( formula_t ) );
			 assert( (yyval.formula) != NULL );
			 (yyval.formula)->type = ATOMIC_FORMULA;
			 (yyval.formula)->u.atomic = (yyvsp[0].atomicFormula);
			 (yyval.formula)->next = (yyvsp[-1].formula);
		       }
#line 2011 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 69:
#line 569 "parser.y" /* yacc.c:1646  */
    {
			 (yyval.formula) = (formula_t*)malloc( sizeof( formula_t ) );
			 assert( (yyval.formula) != NULL );
			 (yyval.formula)->type = ATOMIC_FORMULA;
			 (yyval.formula)->u.atomic = (yyvsp[0].atomicFormula);
			 (yyval.formula)->next = NULL;
		       }
#line 2023 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 70:
#line 580 "parser.y" /* yacc.c:1646  */
    {
			 (yyval.formula) = (formula_t*)malloc( sizeof( formula_t ) );
			 assert( (yyval.formula) != NULL );
			 (yyval.formula)->type = ATOMIC_FORMULA;
			 (yyval.formula)->u.atomic = (yyvsp[0].atomicFormula);
			 (yyval.formula)->next = (yyvsp[-1].formula);
		       }
#line 2035 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 71:
#line 588 "parser.y" /* yacc.c:1646  */
    {
			 (yyval.formula) = (formula_t*)malloc( sizeof( formula_t ) );
			 assert( (yyval.formula) != NULL );
			 (yyval.formula)->type = ATOMIC_FORMULA;
			 (yyval.formula)->u.atomic = (yyvsp[0].atomicFormula);
			 (yyval.formula)->next = NULL;
		       }
#line 2047 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 72:
#line 599 "parser.y" /* yacc.c:1646  */
    {
			 (yyval.atomicFormula) = (atomicFormula_t*) malloc( sizeof( atomicFormula_t ) );
			 assert( (yyval.atomicFormula) != NULL );
			 (yyval.atomicFormula)->id = -1;
			 (yyval.atomicFormula)->neg = 0;
			 (yyval.atomicFormula)->equal = 0;
			 (yyval.atomicFormula)->name = (yyvsp[-2].ident);
			 (yyval.atomicFormula)->args = (yyvsp[-1].idlist);
		       }
#line 2061 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 73:
#line 609 "parser.y" /* yacc.c:1646  */
    {
			 (yyval.atomicFormula) = (atomicFormula_t*) malloc( sizeof( atomicFormula_t ) );
			 assert( (yyval.atomicFormula) != NULL );
			 (yyval.atomicFormula)->id = -1;
			 (yyval.atomicFormula)->neg = 0;
			 (yyval.atomicFormula)->equal = 0;
			 (yyval.atomicFormula)->name = (yyvsp[-1].ident);
			 (yyval.atomicFormula)->args = NULL;
		       }
#line 2075 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 74:
#line 622 "parser.y" /* yacc.c:1646  */
    {
			 (yyval.atomicFormula) = (atomicFormula_t*) malloc( sizeof( atomicFormula_t ) );
			 assert( (yyval.atomicFormula) != NULL );
			 (yyval.atomicFormula)->id = -1;
			 (yyval.atomicFormula)->neg = 0;
			 (yyval.atomicFormula)->equal = 0;
			 (yyval.atomicFormula)->name = (yyvsp[-2].ident);
			 (yyval.atomicFormula)->args = (yyvsp[-1].idlist);
		       }
#line 2089 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 75:
#line 634 "parser.y" /* yacc.c:1646  */
    {
			 (yyval.idlist) = (idList_t*) malloc( sizeof( idList_t ) );
			 assert( (yyval.idlist) != NULL );
			 (yyval.idlist)->id = 0;
			 (yyval.idlist)->name = (yyvsp[-1].ident);
			 (yyval.idlist)->type = NULL;
			 (yyval.idlist)->next = (yyvsp[0].idlist);
		       }
#line 2102 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 76:
#line 643 "parser.y" /* yacc.c:1646  */
    {  
			 (yyval.idlist) = NULL;
		       }
#line 2110 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 80:
#line 657 "parser.y" /* yacc.c:1646  */
    {
			 (yyvsp[0].formula)->next = (yyvsp[-1].formula);
			 (yyval.formula) = (yyvsp[0].formula);
		       }
#line 2119 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 82:
#line 666 "parser.y" /* yacc.c:1646  */
    {
			 (yyval.formula) = (formula_t*)malloc( sizeof( formula_t ) );
			 assert( (yyval.formula) != NULL );
			 (yyval.formula)->type = ATOMIC_FORMULA;
			 (yyval.formula)->next = NULL;
			 (yyval.formula)->u.atomic = (yyvsp[0].atomicFormula);
		       }
#line 2131 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 83:
#line 674 "parser.y" /* yacc.c:1646  */
    {
			 (yyval.formula) = (yyvsp[-1].formula);
		       }
#line 2139 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 84:
#line 678 "parser.y" /* yacc.c:1646  */
    {
                         //printf("EMPTY EFFECT\n");
			 (yyval.formula) = NULL;
		       }
#line 2148 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 85:
#line 683 "parser.y" /* yacc.c:1646  */
    {
			 formula_t *formula, *next;

			 (yyval.formula) = (formula_t*)malloc( sizeof( formula_t ) );
			 assert( (yyval.formula) != NULL );
			 (yyval.formula)->type = WHEN_FORMULA;
			 (yyval.formula)->next = NULL;
			 (yyval.formula)->u.when.condition = (yyvsp[-2].formula);
			 (yyval.formula)->u.when.addEffect = NULL;
			 (yyval.formula)->u.when.delEffect = NULL;

			 for( formula = (yyvsp[-1].formula); formula != NULL; formula = next )
			   {
			     next = formula->next;
			     assert( formula->type == ATOMIC_FORMULA );
			     if( formula->u.atomic->neg == 0 )
			       {
				 formula->next = (yyval.formula)->u.when.addEffect;
				 (yyval.formula)->u.when.addEffect = formula;
			       }
			     else
			       {
				 formula->next = (yyval.formula)->u.when.delEffect;
				 (yyval.formula)->u.when.delEffect = formula;
			       }
			   }
		       }
#line 2180 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 86:
#line 711 "parser.y" /* yacc.c:1646  */
    {
			 formula_t *formula, *next;

			 (yyval.formula) = (formula_t*)malloc( sizeof( formula_t ) );
			 assert( (yyval.formula) != NULL );
			 (yyval.formula)->type = FORALL_FORMULA;
			 (yyval.formula)->next = NULL;
			 (yyval.formula)->u.forall.vars = (yyvsp[-3].idlist);
			 (yyval.formula)->u.forall.addEffect = NULL;
			 (yyval.formula)->u.forall.delEffect = NULL;
			 (yyval.formula)->u.forall.whenEffect = NULL;

			 for( formula = (yyvsp[-1].formula); formula != NULL; formula = next )
			   {
			     next = formula->next;
			     assert( formula->type == ATOMIC_FORMULA );
			     if( formula->u.atomic->neg == 0 )
			       {
				 formula->next = (yyval.formula)->u.forall.addEffect;
				 (yyval.formula)->u.forall.addEffect = formula;
			       }
			     else
			       {
				 formula->next = (yyval.formula)->u.forall.delEffect;
				 (yyval.formula)->u.forall.delEffect = formula;
			       }
			   }
		       }
#line 2213 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 87:
#line 741 "parser.y" /* yacc.c:1646  */
    {
			 formula_t *formula, *next;

			 (yyval.formula) = (formula_t*)malloc( sizeof( formula_t ) );
			 assert( (yyval.formula) != NULL );
			 (yyval.formula)->type = FORALL_FORMULA;
			 (yyval.formula)->next = NULL;
			 (yyval.formula)->u.forall.vars = (yyvsp[-7].idlist);
			 (yyval.formula)->u.forall.addEffect = NULL;
			 (yyval.formula)->u.forall.delEffect = NULL;
			 (yyval.formula)->u.forall.whenEffect = (formula_t*)malloc( sizeof( formula_t ) );
			 assert( (yyval.formula)->u.forall.whenEffect != NULL );
			 (yyval.formula)->u.forall.whenEffect->type = WHEN_FORMULA;
			 (yyval.formula)->u.forall.whenEffect->next = NULL;
			 (yyval.formula)->u.forall.whenEffect->u.when.condition = (yyvsp[-3].formula);
			 (yyval.formula)->u.forall.whenEffect->u.when.addEffect = NULL;
			 (yyval.formula)->u.forall.whenEffect->u.when.delEffect = NULL;

			 for( formula = (yyvsp[-2].formula); formula != NULL; formula = next )
			   {
			     next = formula->next;
			     assert( formula->type == ATOMIC_FORMULA );
			     if( formula->u.atomic->neg == 0 )
			       {
				 formula->next = (yyval.formula)->u.forall.whenEffect->u.when.addEffect;
				 (yyval.formula)->u.forall.whenEffect->u.when.addEffect = formula;
			       }
			     else
			       {
				 formula->next = (yyval.formula)->u.forall.whenEffect->u.when.delEffect;
				 (yyval.formula)->u.forall.whenEffect->u.when.delEffect = formula;
			       }
			   }
		       }
#line 2252 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 88:
#line 779 "parser.y" /* yacc.c:1646  */
    {
			 (yyval.formula) = (formula_t*)malloc( sizeof( formula_t ) );
			 assert( (yyval.formula) != NULL );
			 (yyval.formula)->type = ATOMIC_FORMULA;
			 (yyval.formula)->u.atomic = (yyvsp[0].atomicFormula);
			 (yyval.formula)->next = NULL;
		       }
#line 2264 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 89:
#line 787 "parser.y" /* yacc.c:1646  */
    {
			 (yyval.formula) = (yyvsp[-1].formula);
		       }
#line 2272 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 90:
#line 791 "parser.y" /* yacc.c:1646  */
    {
                         //printf("EMPTY EFFECT\n");
			 (yyval.formula) = NULL;
		       }
#line 2281 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 91:
#line 798 "parser.y" /* yacc.c:1646  */
    {
			 (yyval.formula) = (formula_t*)malloc( sizeof( formula_t ) );
			 assert( (yyval.formula) != NULL );
			 (yyval.formula)->type = ATOMIC_FORMULA;
			 (yyval.formula)->u.atomic = (yyvsp[0].atomicFormula);
			 (yyval.formula)->next = NULL;
		       }
#line 2293 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 92:
#line 806 "parser.y" /* yacc.c:1646  */
    {
			 (yyval.formula) = (yyvsp[-1].formula);
		       }
#line 2301 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 93:
#line 810 "parser.y" /* yacc.c:1646  */
    {
			 (yyval.formula) = (formula_t*)malloc( sizeof( formula_t ) );
			 assert( (yyval.formula) != NULL );
			 (yyval.formula)->type = FORALL_GOAL_FORMULA;
			 (yyval.formula)->next = NULL;
			 (yyval.formula)->u.forallGoal.vars = (yyvsp[-3].idlist);
			 (yyval.formula)->u.forallGoal.formula = (yyvsp[-1].formula);
		       }
#line 2314 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 94:
#line 821 "parser.y" /* yacc.c:1646  */
    {
			 (yyvsp[0].formula)->next = (yyvsp[-1].formula);
			 (yyval.formula) = (yyvsp[0].formula);
		       }
#line 2323 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 96:
#line 835 "parser.y" /* yacc.c:1646  */
    {
			 _low_problemName = (yyvsp[-1].ident);
			 strcpy( _low_problemName, translate( _low_problemName ) );
		       }
#line 2332 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 106:
#line 854 "parser.y" /* yacc.c:1646  */
    {
			 idList_t *object;
			 if( objects == NULL )
			   objects = (yyvsp[-1].idlist);
			 else
			   {
			     for( object = objects; object->next != NULL; object = object->next );
			     object->next = (yyvsp[-1].idlist);
			   }
		       }
#line 2347 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 108:
#line 870 "parser.y" /* yacc.c:1646  */
    {
			 formula_t *formula;
			 initialSituation = (yyvsp[0].formula);
			 for( formula = initialSituation; formula != NULL; formula = formula->next )
			   if( formula->u.atomic->neg && !(_low_requirements & REQ_ADL) )
			     fprintf( stderr, "parser([init_def]): WARNING: negative literal in initial situation\n" );
		       }
#line 2359 "parser.tab.c" /* yacc.c:1646  */
    break;

  case 109:
#line 880 "parser.y" /* yacc.c:1646  */
    {
			 formula_t *formula, *next, *tmp;

			 goalSituation = NULL;
			 for( formula = (yyvsp[-1].formula); formula != NULL; formula = next )
			   {
			     next = formula->next;
			     if( formula->type == FORALL_GOAL_FORMULA )
			       {
				 instantiated = NULL;
				 instantiateFormula( formula->u.forallGoal.formula, formula->u.forallGoal.vars, NULL );
				 if( instantiated != NULL )
				   {
				     for( tmp = instantiated; tmp->next != NULL; tmp = tmp->next );
				     tmp->next = goalSituation;
				     goalSituation = instantiated;
				   }
			       }
			     else
			       {
				 formula->next = goalSituation;
				 goalSituation = formula;
			       }
			   }

			 for( formula = goalSituation; formula != NULL; formula = formula->next )
			   if( formula->u.atomic->neg && !(_low_requirements & REQ_ADL))
			     fprintf( stderr, "parser([goal_def]): WARNING: negative literal in goal situation\n" );
		       }
#line 2393 "parser.tab.c" /* yacc.c:1646  */
    break;


#line 2397 "parser.tab.c" /* yacc.c:1646  */
      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYEMPTY : YYTRANSLATE (yychar);

  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
# define YYSYNTAX_ERROR yysyntax_error (&yymsg_alloc, &yymsg, \
                                        yyssp, yytoken)
      {
        char const *yymsgp = YY_("syntax error");
        int yysyntax_error_status;
        yysyntax_error_status = YYSYNTAX_ERROR;
        if (yysyntax_error_status == 0)
          yymsgp = yymsg;
        else if (yysyntax_error_status == 1)
          {
            if (yymsg != yymsgbuf)
              YYSTACK_FREE (yymsg);
            yymsg = (char *) YYSTACK_ALLOC (yymsg_alloc);
            if (!yymsg)
              {
                yymsg = yymsgbuf;
                yymsg_alloc = sizeof yymsgbuf;
                yysyntax_error_status = 2;
              }
            else
              {
                yysyntax_error_status = YYSYNTAX_ERROR;
                yymsgp = yymsg;
              }
          }
        yyerror (yymsgp);
        if (yysyntax_error_status == 2)
          goto yyexhaustedlab;
      }
# undef YYSYNTAX_ERROR
#endif
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
         error, discard it.  */

      if (yychar <= YYEOF)
        {
          /* Return failure if at end of input.  */
          if (yychar == YYEOF)
            YYABORT;
        }
      else
        {
          yydestruct ("Error: discarding",
                      yytoken, &yylval);
          yychar = YYEMPTY;
        }
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label yyerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto yyerrorlab;

  /* Do not reclaim the symbols of the rule whose action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
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


      yydestruct ("Error: popping",
                  yystos[yystate], yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

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

#if !defined yyoverflow || YYERROR_VERBOSE
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  yystos[*yyssp], yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  return yyresult;
}
#line 926 "parser.y" /* yacc.c:1906  */



int
yyerror( char *s )
{
  extern int lineno;
  fprintf( stderr, "%s:%d: %s\n", _low_yyfile, lineno, s );
  return( 0 );
}


static void
checkRequirement( int req )
{
  switch( req )
    {
    case STRIPS:
      _low_requirements = _low_requirements | REQ_STRIPS;
      break;
    case EQUALITY:
      _low_requirements = _low_requirements | REQ_EQUALITY;
      break;
    case TYPING:
      _low_requirements = _low_requirements | REQ_TYPING;
      break;
    case ADL:
      _low_requirements = _low_requirements | REQ_ADL;
      break;
    default:
      fprintf( stderr, "parser(checkRequirement): not supported requirement %d\n", req );
    }
}


static char *
translate( char *name )
{
  char *ip, *jp;
  static char buffer[1024];

  for( ip = name, jp = buffer; *ip != '\0'; ++ip, ++jp )
    *jp = (*ip == 'a' ? '-' : *ip);
  *jp = '\0';
  return( buffer );
}


static void
printAtomicFormula( atomicFormula_t *formula )
{
  register idList_t *arg;

  if( formula != NULL )
    {
      /* negation? */
      if( formula->neg == 1 )
	fprintf( stderr, "(not " );

      /* print name */
      fprintf( stderr, "(%s ", (formula->equal == 1 ? "=" : formula->name) );

      /* args */
      for( arg = formula->args; arg != NULL; arg = arg->next )
	fprintf( stderr, "%s%s", arg->name, (arg->next != NULL ? " " : ")") );

      if( formula->neg == 1 )
	fprintf( stderr, ")" );
    }
}


static void
deleteFormula( formula_t *formula )
 {
   register idList_t *var, *pvar, *nvar;

   if( formula != NULL )
     {
       switch( formula->type )
	 {
	 case ATOMIC_FORMULA:
	   free( formula->u.atomic );
	   break;
	 case WHEN_FORMULA:
	   deleteFormula( formula->u.when.condition );
	   deleteFormula( formula->u.when.addEffect );
	   deleteFormula( formula->u.when.delEffect );
	   break;
	 case FORALL_FORMULA:
	   for( pvar = NULL, var = formula->u.forall.vars; var != NULL; var = nvar )
	     {
	       nvar = var->next;
	       free( var );
	     }
	   deleteFormula( formula->u.forall.addEffect );
	   deleteFormula( formula->u.forall.delEffect );
	   deleteFormula( formula->u.forall.whenEffect );
	   break;
	 }
       free( formula );
     }
}


static void
printFormula( formula_t *formula )
 {
   register idList_t *var;

   if( formula != NULL )
     {
       switch( formula->type )
	 {
	 case ATOMIC_FORMULA:
	   printAtomicFormula( formula->u.atomic );
	   break;
	 case WHEN_FORMULA:
	   fprintf( stderr, "(when (" );
	   printFormula( formula->u.when.condition );
	   fprintf( stderr, ") " );
	   printFormula( formula->u.when.addEffect );
	   printFormula( formula->u.when.delEffect );
	   fprintf( stderr, ")" );
	   break;
	 case FORALL_FORMULA:
	   fprintf( stderr, "(forall (" );
	   for( var = formula->u.forall.vars; var != NULL; var = var->next )
	     fprintf( stderr, "%s%s", var->name, (var->next != NULL ? " " : "") );
	   fprintf( stderr, ") " );
	   printFormula( formula->u.forall.addEffect );
	   fprintf( stderr, " " );
	   printFormula( formula->u.forall.delEffect );
	   fprintf( stderr, " " );
	   printFormula( formula->u.forall.whenEffect );
	   fprintf( stderr, ")" );
	   break;
	 }
       printFormula( formula->next );
     }
}


static void
generateNames( void )
{
  register int i;
  register idList_t *object;
  register formula_t *predicate;

  for( predicate = predicates, i = 0; predicate != NULL; predicate = predicate->next, ++i );
  _low_numberPredicates = i;

  _low_schemaName = (char**) calloc( numberSchema, sizeof( char* ) );
  _low_objectName = (char**) calloc( numberObjects, sizeof( char* ) );
  _low_predicateName = (char**) calloc( _low_numberPredicates, sizeof( char* ) );

  for( i = 0; i < numberSchema; ++i )
    _low_schemaName[i] = strdup( translate( _low_schemaTable[i]->name ) );

  for( object = objects, i = 0; object != NULL; object = object->next, ++i )
    _low_objectName[i] = strdup( translate( object->name ) );

  for( predicate = predicates; predicate != NULL; predicate = predicate->next )
    _low_predicateName[predicate->u.atomic->id-1] = strdup( translate( predicate->u.atomic->name ) );
}


static void
identifyObjects( void )
{
  idList_t *object;
  int i;

  i = 1;
  for( object = objects; object != NULL; object = object->next )
    object->id = i++;
  numberObjects = i - 1;
}


static void
tagFormulaList( formula_t *formulaList )
{
  formula_t *formula, *predicate;
  idList_t *var, *object;

  for( formula = formulaList; formula != NULL; formula = formula->next )
    if( formula->type == ATOMIC_FORMULA )
      {
	for( predicate = predicates; predicate != NULL; predicate = predicate->next )
	  if( !strcasecmp( predicate->u.atomic->name, formula->u.atomic->name ) )
	    {
	      formula->u.atomic->id = predicate->u.atomic->id;
	      break;
	    }
	if( predicate == NULL )
	  {
	    fprintf( stderr, "parser(tagFormulaList): no predicate for %s\n", 
		     translate( formula->u.atomic->name ) );
	    return;
	  }

	for( var = formula->u.atomic->args; var != NULL; var = var->next )
	  {
	    for( object = objects; object != NULL; object = object->next )
	      if( !strcasecmp( object->name, var->name ) )
		{
		  var->id = object->id;
		  break;
		}
	    if( object == NULL )
	      {
		fprintf( stderr, "parser(tagFormulaList): no object for %s\n", var->name );
		return;
	      }
	  }
      }
}


static void
idMatchArgsVars( formula_t *formula, idList_t *vars )
{
  formula_t *predicate;
  idList_t *arg, *var, *object;

  /* identification */
  for( ; formula != NULL; formula = formula->next )
    switch( formula->type )
      {
      case ATOMIC_FORMULA:
	for( predicate = predicates; predicate != NULL; predicate = predicate->next )
	  if( !strcasecmp( predicate->u.atomic->name, formula->u.atomic->name ) )
	    {
	      formula->u.atomic->id = predicate->u.atomic->id;
	      formula->u.atomic->equal = predicate->u.atomic->equal;
	      break;
	    }
	if( predicate == NULL )
	  fprintf( stderr, "parser(idMatchArgsVars): no predicate for %s\n", 
		   translate( formula->u.atomic->name ) );

	for( arg = formula->u.atomic->args; arg != NULL; arg = arg->next )
	  if( arg->name[0] == '?' )
	    {
	      for( var = vars; var != NULL; var = var->next )
		if( !strcasecmp( arg->name, var->name ) )
		  {
		    arg->id = var->id;
		    break;
		  }
	      if( var == NULL )
		fprintf( stderr, "parser(idMatchArgsVars): no variable for %s\n", arg->name );
	    }
	  else
	    {
	      for( object = objects; object != NULL; object = object->next )
		if( !strcasecmp( arg->name, object->name ) )
		  {
		    arg->id = object->id;
		    break;
		  }
	      if( object == NULL )
		fprintf( stderr, "parser(idMatchArgsVars): no object for %s\n", arg->name );
	    }
	break;
      case WHEN_FORMULA:
	idMatchArgsVars( formula->u.when.condition, vars );
	idMatchArgsVars( formula->u.when.addEffect, vars );
	idMatchArgsVars( formula->u.when.delEffect, vars );
	break;
      case FORALL_FORMULA:
	idMatchArgsVars( formula->u.forall.addEffect, vars );
	idMatchArgsVars( formula->u.forall.delEffect, vars );
	idMatchArgsVars( formula->u.forall.whenEffect, vars );
	break;
      }
}


static int
isOfType( register idList_t *type1, register idList_t *type2 )
{
  register idList_t *t1, *t2, *t3;

  if( !strcasecmp( type1->name, type2->name ) )
    {
      return( 1 );
    }
  else
    {
      for( t1 = type1; t1 != NULL; t1 = t1->next )
	for( t2 = types; t2 != NULL; t2 = t2->next )
	  if( !strcasecmp( t1->name, t2->name ) )
	    for( t3 = t2->type; t3 != NULL; t3 = t3->next )
	      {
		if( isOfType( t3, type2 ) )
		  return( 1 );
	      }
      return( 0 );
    }
}


static formula_t *
copyReplaceFormula( register formula_t *formula, register instantiation_t *instantiation )
{
  register formula_t *new;
  register instantiation_t *inst;
  register idList_t *arg, *prevArg, *newArg;

  if( formula != NULL )
    {
      new = (formula_t*)malloc( sizeof( formula_t ) );
      assert( new != NULL );
      new->type = formula->type;
      switch( formula->type )
	{
	case ATOMIC_FORMULA:
	  new->u.atomic = (atomicFormula_t*)malloc( sizeof( atomicFormula_t ) );
	  assert( new->u.atomic != NULL );
	  *(new->u.atomic) = *(formula->u.atomic);
	  new->u.atomic->args = NULL;

	  /* copy/replace arguments */
	  prevArg = NULL;
	  for( arg = formula->u.atomic->args; arg != NULL; arg = arg->next )
	    {
	      newArg = (idList_t*)malloc( sizeof( idList_t ) );
	      assert( newArg != NULL );
	      for( inst = instantiation; inst != NULL; inst = inst->next )
		if( !strcasecmp( arg->name, inst->var->name ) )
		  break;
	      *newArg = *(inst == NULL ? arg : inst->val);
	      newArg->next = NULL;

	      /* link it */
	      if( prevArg == NULL )
		new->u.atomic->args = newArg;
	      else
		prevArg->next = newArg;
	      prevArg = newArg;
	    }
	  break;
	case WHEN_FORMULA:
	  new->u.when.condition = copyReplaceFormula( formula->u.when.condition, instantiation );
	  new->u.when.addEffect = copyReplaceFormula( formula->u.when.addEffect, instantiation );
	  new->u.when.delEffect = copyReplaceFormula( formula->u.when.delEffect, instantiation );
	  break;
	}

      /* next */
      new->next = copyReplaceFormula( formula->next, instantiation );
      return( new );
    }
  else
    return( NULL );
}


static void
instantiateFormula( register formula_t *effects, register idList_t *vars, register instantiation_t *instantiation )
{
  instantiation_t new;
  formula_t *newFormula;

  if( vars == NULL )
    {
      /* copy/replace effects */
      while( effects != NULL )
	{
	  newFormula = copyReplaceFormula( effects, instantiation );
	  newFormula->next = instantiated;
	  instantiated = newFormula;
	  effects = effects->next;
	}
    }
  else
    {
      new.next = instantiation;
      new.var = vars;
      for( new.val = objects; new.val != NULL; new.val = new.val->next )
	if( isOfType( new.val->type, new.var->type ) )
	  instantiateFormula( effects, vars->next, &new );
    }
}


static void
schemaPostProcessing( register schema_t *schema )
{
  register formula_t *forallFormula, *formula, *nextFormula;

  for( forallFormula = schema->forall; forallFormula != NULL; forallFormula = forallFormula->next )
    {
      assert( forallFormula->type == FORALL_FORMULA );
      instantiated = NULL;
      instantiateFormula( forallFormula->u.forall.addEffect, forallFormula->u.forall.vars, NULL );
      instantiateFormula( forallFormula->u.forall.delEffect, forallFormula->u.forall.vars, NULL );
      instantiateFormula( forallFormula->u.forall.whenEffect, forallFormula->u.forall.vars, NULL );

      for( formula = instantiated; formula != NULL; formula = nextFormula )
	{
	  assert( formula->type != FORALL_FORMULA );
	  nextFormula = formula->next;
	  switch( formula->type )
	    {
	    case ATOMIC_FORMULA:
	      if( formula->u.atomic->neg == 1 )
		{
		  formula->next = schema->del;
		  schema->del = formula;
		}
	      else
		{
		  formula->next = schema->add;
		  schema->add = formula;
		}
	      break;
	    case WHEN_FORMULA:
	      formula->next = schema->when;
	      schema->when = formula;
	      break;
	    }
	}
    }
}


static int
atomId( register atomicFormula_t *formula, register int *parameters )
{
  register int *p;
  register idList_t *arg;
  static int arguments[MAXPARAMETERS];

  /* argument extraction */
  p = arguments;
  *p++ = formula->id;
  for( arg = formula->args; arg != NULL; arg = arg->next )
    if( arg->name[0] == '?' )
      {
	assert( arg->id > 0 );
	*p++ = parameters[arg->id];
      }
    else
      *p++ = arg->id;
  *p = -1;

  /* generete atomId */
  return( readAtomHash( arguments )->idx );
}


void
generateVarsAndValues( void )
{
  register int i, j, k, *p;
  register formula_t *formula;
  register idList_t *var, *object;
  static int arguments[MAXPARAMETERS];

  /* vars  */
  vars = (int**) calloc( numberSchema, sizeof( int* ) );
  for( i = 0; i < numberSchema; ++i )
    {
      vars[i] = (int*) calloc( maxParameters + 1, sizeof( int ) );
      for( var = _low_schemaTable[i]->vars, j = 0; var != NULL; var = var->next, ++j )
	vars[i][j] = var->id;
    }

  /* values  */
  values = (int**) calloc( numberSchema * (maxParameters + 1), sizeof( int* ) );
  for( i = 0; i < numberSchema * maxParameters; ++i )
    values[i] = (int *) calloc( numberObjects + 1, sizeof( int ) );
  for( i = 0; i < numberSchema; ++i )
    for( var = _low_schemaTable[i]->vars; var != NULL; var = var->next )
      {
	if( var->type == NULL )
	  {
	    k = 0;
	    for( object = objects; object != NULL; object = object->next )
	      values[numberSchema*(var->id-1) + i][k++] = object->id;
	  }
	else
	  {
	    if( !(_low_requirements & REQ_TYPING) )
	      fprintf( stderr, "parser(generateVarsAndValues): using types without :typing requirements.\n" );
	    k = 0;
	    for( object = objects; object != NULL; object = object->next )
	      if( !strcasecmp( var->type->name, object->type->name ) || isOfType( object->type, var->type ) )
		values[numberSchema*(var->id-1) + i][k++] = object->id;
	  }
      }

  /* insert initial and goal atoms in atomHashTable */
  idMatchArgsVars( initialSituation, NULL );
  for( formula = initialSituation, i = 0; formula != NULL; formula = formula->next )
    if( !formula->u.atomic->neg )
      {
	p = arguments;
	*p++ = formula->u.atomic->id;
	for( var = formula->u.atomic->args; var != NULL; var = var->next )
	  *p++ = var->id;
	*p = -1;
	_low_initialAtoms[i++] = readAtomHash( arguments )->idx;
      }
  _low_initialAtoms[i] = 0;

  idMatchArgsVars( goalSituation, NULL );
  for( formula = goalSituation, i = 0; formula != NULL; formula = formula->next )
    if( !formula->u.atomic->neg )
      {
	p = arguments;
	*p++ = formula->u.atomic->id;
	for( var = formula->u.atomic->args; var != NULL; var = var->next )
	  *p++ = var->id;
	*p = -1;
	_low_goalAtoms[i++] = readAtomHash( arguments )->idx;
      }
  _low_goalAtoms[i] = 0;
}


static void
applyEffect( register formula_t *addList, register formula_t *delList, register formula_t *whenList,
	     register atom_t *state, register atom_t *newState, register int *parameters )
{
  register int *add, *del, idx;
  register formula_t *when;

  /* initialize list pointer */
  add = operatorAdd;
  del = operatorDel;

  /* add-list */
  for( ; addList != NULL; addList = addList->next )
    {
      /* generate atom, apply and save add effect, and add parents */
      assert( (addList->type == ATOMIC_FORMULA) && (addList->u.atomic->neg == 0) );
      idx = atomId( addList->u.atomic, parameters );
      set( newState, idx );
      addParents( idx, operatorPrec );
      *add++ = idx;

      /* negated atoms */
      if( _low_requirements & REQ_ADL )
	{
	  if( _low_negatedAtom[idx] == 0 )
	    {
	      /* generate (not-p ...) atom */
	      addList->u.atomic->id += _low_numberPredicates + 1;
	      _low_negatedAtom[idx] = atomId( addList->u.atomic, parameters );
	      addList->u.atomic->id -= _low_numberPredicates + 1;
	      assert( _low_negatedAtom[idx] != idx );
	    }
	  *del++ = _low_negatedAtom[idx];
	}
    }

  /* del-list */
  for( ; delList != NULL; delList = delList->next )
    {
      /* basic assertion */
      assert( (delList->type == ATOMIC_FORMULA) && (delList->u.atomic->neg == 1) );
      idx = atomId( delList->u.atomic, parameters );
      *del++ = idx;

      /* negated atoms */
      if( _low_requirements & REQ_ADL )
	{
	  if( _low_negatedAtom[idx] == 0 )
	    {
	      /* generate (not-p ...) atom */
	      delList->u.atomic->id += _low_numberPredicates + 1;
	      _low_negatedAtom[idx] = atomId( delList->u.atomic, parameters );
	      delList->u.atomic->id -= _low_numberPredicates + 1;
	      assert( _low_negatedAtom[idx] != idx );
	    }
	  set( newState, _low_negatedAtom[idx] );
	  addParents( _low_negatedAtom[idx], operatorPrec );
	  *add++ = _low_negatedAtom[idx];
	}
    }

  /* conditional effects */
  for( when = whenList; when != NULL; when = when->next )
    if( evaluateFormula( when->u.when.condition, state, parameters, 0 ) )
      {
	/* add-list */
	for( addList = when->u.when.addEffect; addList != NULL; addList = addList->next )
	  {
	    /* basic assertion */
	    assert( (addList->type == ATOMIC_FORMULA) && (addList->u.atomic->neg == 0) );
	    idx = atomId( addList->u.atomic, parameters );
	    set( newState, idx );
	    addParents( idx, operatorPrec );

	    /* no negated atoms for conditional add-list */
	  }

	/* del-list */
	for( delList = when->u.when.delEffect; delList != NULL; delList = delList->next )
	  {
	    /* basic assertion */
	    assert( (delList->type == ATOMIC_FORMULA) && (delList->u.atomic->neg == 1) );
	    idx = atomId( delList->u.atomic, parameters );

	    /* negated atoms */
	    if( _low_requirements & REQ_ADL )
	      {
		if( _low_negatedAtom[idx] == 0 )
		  {
		    /* generate (not-p ...) atom */
		    delList->u.atomic->id += _low_numberPredicates + 1;
		    _low_negatedAtom[idx] = atomId( delList->u.atomic, parameters );
		    delList->u.atomic->id -= _low_numberPredicates + 1;
		    assert( _low_negatedAtom[idx] != idx );
		  }
		set( newState, _low_negatedAtom[idx] );
		addParents( _low_negatedAtom[idx], operatorPrec );
	      }
	  }
      }

  /* finish lists */
  *add = 0;
  *del = 0;
  operatorAddSize = add - operatorAdd;
  operatorDelSize = del - operatorDel;
}


int
evaluateFormula( register formula_t *formula, register atom_t *state, 
		 register int *parameters, register int savePrec )
{
  register int *prec = NULL, *p, idx;
  register idList_t *arg;
  static int arguments[MAXPARAMETERS];

  /* save precondition */
  if( savePrec )
    prec = operatorPrec;

  for( ; formula != NULL; formula = formula->next )
    {
      /* basic assertion */
      assert( formula->type == ATOMIC_FORMULA );

      /* argument extraction */
      p = arguments;
      *p++ = formula->u.atomic->id;
      for( arg = formula->u.atomic->args; arg != NULL; arg = arg->next )
	if( arg->name[0] == '?' )
	  {
	    assert( arg->id > 0 );
	    if( (*p++ = parameters[arg->id]) == 0 )
	      break;
	  }
	else
	  *p++ = arg->id;
      if( arg != NULL )
	continue;
      else
	*p = -1;

      /* check formula */
      if( formula->u.atomic->equal == 1 )
	{
	  if( (formula->u.atomic->neg && (arguments[1] == arguments[2])) ||
	      (!formula->u.atomic->neg && (arguments[1] != arguments[2])) )
	    return( 0 );
	}
      else if( formula->u.atomic->neg == 0 )
	{
	  idx = readAtomHash( arguments )->idx;
	  if( !asserted( state, idx ) )
	    return( 0 );

	  /* save precondition */
	  if( savePrec )
	    *prec++ = idx;
	}
      else if( _low_requirements & REQ_ADL )
	{
	  /* generate negated atom */
	  idx = readAtomHash( arguments )->idx;
	  if( _low_negatedAtom[idx] == 0 )
	    {
	      arguments[0] += _low_numberPredicates + 1;
	      _low_negatedAtom[idx] = readAtomHash( arguments )->idx;
	      arguments[0] -= _low_numberPredicates + 1;
	      assert( _low_negatedAtom[idx] != idx );
	    }

	  /* test it if in the grounding phase */
	  if( (_low_groundingOperators == 1) && !asserted( state, _low_negatedAtom[idx] ) )
	    return( 0 );

	  /* save precondition */
	  if( savePrec )
	    *prec++ = _low_negatedAtom[idx];
	}
      else
	{
	  fprintf( stderr, "parser(evaluateFormula): WARNING: negative literal in precondition\n" );
	  return( 0 );
	}
    }

  /* save precondition */
  if( savePrec )
    {
      *prec = 0;
      operatorPrecSize = prec - operatorPrec;
    }

  return( 1 );
}


int
applyOperatorSchema( register schema_t *schema, register atom_t *state, 
		     register atom_t *newState, register int *parameters )
{
  if( !evaluateFormula( schema->prec, state, parameters, 1 ) )
    {
      return( 0 );
    }
  else
    {
      applyEffect( schema->add, schema->del, schema->when, state, newState, parameters );
      return( 1 );
    }
}


void
instantiateConditionalEffects( register int father, register schema_t *schema, register int *parameters )
{
  register int *prec, *add, *del, idx;
  register formula_t *when, *formula, *prevWhen, *nextWhen, *prevFormula, *nextFormula;
  register suboperator_t *suboperator;
  extern void orderAtomList( register int *, register int );

  /* when-list */
  prevWhen = NULL;
  for( when = schema->when; when != NULL; when = nextWhen )
    {
      assert( when->type == WHEN_FORMULA );
      nextWhen = when->next;

      /* prec-list */
      prec = operatorPrec;
      prevFormula = NULL;
      for( formula = when->u.when.condition; formula != NULL; formula = nextFormula )
	{
	  assert( formula->type == ATOMIC_FORMULA );
	  nextFormula = formula->next;

	  /* check if equality */
	  if( formula->u.atomic->equal == 1 )
	    {
	      if( (formula->u.atomic->neg && (parameters[1] == parameters[2])) ||
		  (!formula->u.atomic->neg && (parameters[1] != parameters[2])) )
		break;

	      /* delete it and relink */
	      deleteFormula( formula );
	      if( prevFormula == NULL )
		when->u.when.condition = nextFormula;
	      else
		prevFormula->next = nextFormula;
	    }
	  else
	    {
	      prevFormula = formula;

	      /* others formula */
	      idx = atomId( formula->u.atomic, parameters );
	      if( formula->u.atomic->neg == 0 )
		{
		  /* save precondition */
		  *prec++ = idx;
		}
	      else if( _low_requirements & REQ_ADL )
		{
		  /* generate (not-p ...) atom */
		  if( _low_negatedAtom[idx] == 0 )
		    {
		      formula->u.atomic->id += _low_numberPredicates + 1;
		      _low_negatedAtom[idx] = atomId( formula->u.atomic, parameters );
		      formula->u.atomic->id -= _low_numberPredicates + 1;
		      assert( _low_negatedAtom[idx] != idx );
		    }
		  *prec++ = _low_negatedAtom[idx];
		}
	      else
		{
		  fprintf( stderr, "parser(instantiateConditionalEffects): WARNING: negative literal in precondition\n" );
		}
	    }
	}

      /* check if we pass the equality conditions of the prec */
      if( formula != NULL )
	{
	  /* delete it and relink */
	  deleteFormula( when );
	  if( prevWhen == NULL )
	    schema->when = nextWhen;
	  else
	    prevWhen->next = nextWhen;
	}
      else
	{
	  *prec = 0;
	  operatorPrecSize = prec - operatorPrec;
	  prevWhen = when;

	  /* initialize lists */
	  add = operatorAdd;
	  del = operatorDel;

	  /* add-list */
	  for( formula = when->u.when.addEffect; formula != NULL; formula = formula->next )
	    {
	      /* generate atom, apply and save add effect, and add parents */
	      assert( (formula->type == ATOMIC_FORMULA) && (formula->u.atomic->neg == 0) );
	      idx = atomId( formula->u.atomic, parameters );
	      *add++ = idx;

	      /* negated atoms */
	      if( _low_requirements & REQ_ADL )
		{
		  if( _low_negatedAtom[idx] == 0 )
		    {
		      /* generate (not-p ...) atom */
		      formula->u.atomic->id += _low_numberPredicates + 1;
		      _low_negatedAtom[idx] = atomId( formula->u.atomic, parameters );
		      formula->u.atomic->id -= _low_numberPredicates + 1;
		      assert( _low_negatedAtom[idx] != idx );
		    }
		  *del++ = _low_negatedAtom[idx];
		}
	    }

	  /* del-list */
	  for( formula = when->u.when.delEffect; formula != NULL; formula = formula->next )
	    {
	      /* basic assertion */
	      assert( (formula->type == ATOMIC_FORMULA) && (formula->u.atomic->neg == 1) );
	      idx = atomId( formula->u.atomic, parameters );
	      *del++ = idx;

	      /* negated atoms */
	      if( _low_requirements & REQ_ADL )
		{
		  if( _low_negatedAtom[idx] == 0 )
		    {
		      /* generate (not-p ...) atom */
		      formula->u.atomic->id += _low_numberPredicates + 1;
		      _low_negatedAtom[idx] = atomId( formula->u.atomic, parameters );
		      formula->u.atomic->id -= _low_numberPredicates + 1;
		      assert( _low_negatedAtom[idx] != idx );
		    }
		  *add++ = _low_negatedAtom[idx];
		}
	    }

	  /* finish lists */
	  *add = 0;
	  *del = 0;
	  operatorAddSize = add - operatorAdd;
	  operatorDelSize = del - operatorDel;

	  /* save suboperator */
	  suboperator = (suboperator_t*)malloc( sizeof( suboperator_t ) );
	  assert( suboperator != NULL );
	  suboperator->prec = (int*)calloc( operatorPrecSize+1, sizeof( int ) );
	  suboperator->add = (int*)calloc( operatorAddSize+1, sizeof( int ) );
	  suboperator->del = (int*)calloc( operatorDelSize+1, sizeof( int ) );
	  assert( (suboperator->prec != NULL) && (suboperator->add != NULL) && (suboperator->del != NULL) );

	  /* order Prec, Add, and Del lists */
	  orderAtomList( operatorPrec, operatorPrecSize );
	  orderAtomList( operatorAdd, operatorAddSize );
	  orderAtomList( operatorDel, operatorDelSize );

	  /* fill it */
	  suboperator->precSize = operatorPrecSize;
	  suboperator->addSize = operatorAddSize;
	  suboperator->delSize = operatorDelSize;
	  memcpy( suboperator->prec, operatorPrec, (operatorPrecSize+1) * sizeof( int ) );
	  memcpy( suboperator->add, operatorAdd, (operatorAddSize+1) * sizeof( int ) );
	  memcpy( suboperator->del, operatorDel, (operatorDelSize+1) * sizeof( int ) );

	  /* link it */
	  suboperator->next = _low_suboperators;
	  _low_suboperators = suboperator;
	}
    }
}
