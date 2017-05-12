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
** Universidad Simon Bolivar, 1999 (c)
** Blai Bonet and Hector Geffner. 1999.
**
*/


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <assert.h>

#include "types.h"


/* defines */
#define HASHSIZE        (20*1024)


/* prototypes */
void checkRequirement( int );
char *buildPredicate( struct aformula_s *, struct instantiation_s * );
void emitPrologue( void );
void emitEpilogue( void );
void insertHash( char *, struct hashentry_s * );
char *translate( char * );

void usage( void );
void identifyObjects( void );
void tagFormulaList( struct aformula_s *formulaList );
void identifyTypes( void );
void identifyDomains( void );
void idMatchArgsVars( struct aformula_s *formula, struct idlist_s *vars );
void emitNames( void );
void emitActions( void );
void mystery( void );

int yylex();
int yyerror(const char*);


/* global data */
int domainParsed = 0;
char *problemName, *domainName = NULL;
char *yyfile, *yyparser;

int notDuplicates = 1, prunning = 0;
int maxParameters = 0, numObjects = 0;
 
int number = 0, numberSchema;;
FILE *xfile, *yfile, *zfile;
struct hashentry_s *hashtable[HASHSIZE];

struct idlist_s *objects = NULL;
struct action_s *actions = NULL;
struct aformula_s *predicates = NULL;
struct aformula_s *initialSituation = NULL;
struct aformula_s *goalSituation = NULL;


#line 134 "y.tab.c" /* yacc.c:339  */

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
   by #include "y.tab.h".  */
#ifndef YY_YY_Y_TAB_H_INCLUDED
# define YY_YY_Y_TAB_H_INCLUDED
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
    DEFINE = 262,
    DOMAIN = 263,
    REQUIREMENTS = 264,
    CONSTANTS = 265,
    PREDICATES = 266,
    QUESTION = 267,
    STRIPS = 268,
    EQUALITY = 269,
    CONDITIONAL = 270,
    DOM_AXIOMS = 271,
    DISJUNCTIVE = 272,
    ADL = 273,
    E_PRECONDITIONS = 274,
    U_PRECONDITIONS = 275,
    AND = 276,
    NOT = 277,
    FORALL = 278,
    WHEN = 279,
    EQUAL = 280,
    ACTION = 281,
    PARAMETERS = 282,
    PRECONDITION = 283,
    EFFECT = 284,
    PROBLEM = 285,
    INIT = 286,
    GOAL = 287,
    LENGTH = 288,
    SITUATION = 289,
    OBJECTS = 290,
    SERIAL = 291,
    PARALLEL = 292,
    INTEGER = 293,
    TYPING = 294,
    TYPES = 295
  };
#endif
/* Tokens.  */
#define LEFTPAR 258
#define RIGHTPAR 259
#define NAME 260
#define TWODOTS 261
#define DEFINE 262
#define DOMAIN 263
#define REQUIREMENTS 264
#define CONSTANTS 265
#define PREDICATES 266
#define QUESTION 267
#define STRIPS 268
#define EQUALITY 269
#define CONDITIONAL 270
#define DOM_AXIOMS 271
#define DISJUNCTIVE 272
#define ADL 273
#define E_PRECONDITIONS 274
#define U_PRECONDITIONS 275
#define AND 276
#define NOT 277
#define FORALL 278
#define WHEN 279
#define EQUAL 280
#define ACTION 281
#define PARAMETERS 282
#define PRECONDITION 283
#define EFFECT 284
#define PROBLEM 285
#define INIT 286
#define GOAL 287
#define LENGTH 288
#define SITUATION 289
#define OBJECTS 290
#define SERIAL 291
#define PARALLEL 292
#define INTEGER 293
#define TYPING 294
#define TYPES 295

/* Value type.  */


extern YYSTYPE yylval;

int yyparse (void);

#endif /* !YY_YY_Y_TAB_H_INCLUDED  */

/* Copy the second part of user declarations.  */

#line 260 "y.tab.c" /* yacc.c:358  */

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
#define YYLAST   127

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  41
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  45
/* YYNRULES -- Number of rules.  */
#define YYNRULES  85
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  150

/* YYTRANSLATE[YYX] -- Symbol number corresponding to YYX as returned
   by yylex, with out-of-bounds checking.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   295

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
      35,    36,    37,    38,    39,    40
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,    98,    98,   102,   103,   104,   108,   123,   131,   107,
     158,   159,   162,   163,   164,   165,   168,   171,   175,   181,
     185,   189,   205,   209,   213,   217,   221,   225,   231,   237,
     251,   260,   271,   291,   296,   300,   315,   325,   330,   331,
     337,   348,   349,   352,   355,   408,   414,   419,   426,   433,
     442,   454,   455,   459,   467,   478,   484,   499,   509,   514,
     515,   518,   522,   527,   534,   545,   557,   556,   570,   571,
     574,   575,   576,   577,   578,   579,   583,   597,   600,   611,
     622,   626,   627,   630,   631,   634
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || 0
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "LEFTPAR", "RIGHTPAR", "NAME", "TWODOTS",
  "DEFINE", "DOMAIN", "REQUIREMENTS", "CONSTANTS", "PREDICATES",
  "QUESTION", "STRIPS", "EQUALITY", "CONDITIONAL", "DOM_AXIOMS",
  "DISJUNCTIVE", "ADL", "E_PRECONDITIONS", "U_PRECONDITIONS", "AND", "NOT",
  "FORALL", "WHEN", "EQUAL", "ACTION", "PARAMETERS", "PRECONDITION",
  "EFFECT", "PROBLEM", "INIT", "GOAL", "LENGTH", "SITUATION", "OBJECTS",
  "SERIAL", "PARALLEL", "INTEGER", "TYPING", "TYPES", "$accept", "start",
  "list_domain_problem", "domain", "$@1", "$@2", "$@3", "list_def",
  "definition", "require_def", "list_require_key", "require_key",
  "types_def", "constants_def", "list_name", "predicates_def",
  "list_atomic_formula_var", "atomic_formula_var", "list_var", "predicate",
  "variable", "list_structure_def", "structure_def", "action_def",
  "list_action_body", "action_body", "list_parameters", "goal_description",
  "list_goal_description", "atomic_formula_term", "list_term", "term",
  "effect", "list_effect", "problem", "$@4", "list_def_prob", "def_prob",
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
     295
};
# endif

#define YYPACT_NINF -97

#define yypact_value_is_default(Yystate) \
  (!!((Yystate) == (-97)))

#define YYTABLE_NINF -1

#define yytable_value_is_error(Yytable_value) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int8 yypact[] =
{
     -97,    12,    14,   -97,    17,   -97,   -97,    45,    -4,    33,
      49,    15,    74,   -97,   -97,    76,   -97,    -3,     7,    43,
      78,    82,    78,    -3,    83,     9,   -97,   -97,   -97,   -97,
     -97,   -97,   -97,   -97,   -97,   -97,   -97,   -97,    16,   -97,
      78,   -97,    84,     0,   -97,    10,   -97,   -97,    85,    61,
      87,   -97,    86,    43,   -97,    88,   -97,    90,    78,   -97,
     -97,   -97,   -97,   -97,   -97,    80,   -97,   -97,   -97,    91,
      94,   -97,   -97,   -97,    93,   -97,    19,   -97,    48,    89,
     -97,   -97,   -97,    63,    95,   -97,    96,    97,    99,    80,
     -97,    61,   100,   101,   -97,   -97,   -97,    98,     4,   -97,
      35,   -97,   -97,   -97,   -97,   -97,   -97,   -97,    23,   -97,
     -97,   -97,    71,     0,   102,   -97,   -97,   103,     4,    70,
      70,   -97,   106,    88,   107,   -97,   -97,   -97,   -97,   -97,
     -97,   108,   109,    80,   -97,    59,   -97,   -97,   -97,   -97,
     -97,   110,   -97,    98,   -97,    73,   111,   -97,   -97,   -97
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       5,     0,     2,     1,     0,     4,     3,     0,     0,     0,
       0,     0,     0,     6,    66,     0,    69,     0,     0,     0,
       0,     0,     0,     7,     0,     0,    67,    68,    19,    21,
      22,    23,    24,    25,    26,    27,    20,    12,     0,    18,
      31,    14,     0,     0,    15,     0,    34,    13,     0,     0,
       0,    11,     0,     0,    55,     0,    82,     0,     0,    16,
      17,    30,    29,    38,    39,    37,    32,    33,    28,     0,
       8,    42,    43,    10,     0,    70,     0,    72,     0,     0,
      51,    73,    74,     0,     0,    71,     0,     0,     0,    37,
      46,     0,     0,     0,    78,    54,    55,     0,    58,    79,
       0,    80,    81,    77,    76,    40,    35,    36,     0,    41,
       9,    75,     0,     0,     0,    59,    60,     0,    58,     0,
       0,    44,     0,     0,     0,    45,    52,    53,    56,    57,
      85,     0,     0,    37,    48,     0,    63,    49,    83,    84,
      50,     0,    65,     0,    47,     0,     0,    61,    64,    62
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int8 yypgoto[] =
{
     -97,   -97,   -97,   -97,   -97,   -97,   -97,   -97,   104,    58,
     -97,    79,   -97,   -97,   -19,   -97,   -97,    75,   -87,    81,
     -53,   -97,    25,   -97,   -97,   -97,   -97,   -55,    22,   -96,
       1,   -97,   -24,   -97,   -97,   -97,   -97,   -97,   -97,   -97,
     -97,   -97,   -97,   -97,     2
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     1,     2,     5,    15,    49,    92,    23,    24,    37,
      38,    39,    47,    41,    42,    44,    45,    46,    88,    98,
      89,    70,    71,    72,   108,   125,   141,    95,    76,    80,
     117,   118,   137,   145,     6,    16,    18,    27,    85,    77,
      81,    82,    83,   102,   131
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_uint8 yytable[] =
{
      79,   114,   107,    48,     9,    63,    19,    20,    21,   115,
      25,    26,     3,    43,    66,    52,    87,     4,    53,    13,
      59,    61,    78,    94,     7,    64,    10,   121,   136,    28,
      29,    30,    31,    32,    33,    34,    35,    22,    11,    86,
      54,    55,    56,    57,    58,   116,   140,   146,     8,   136,
     122,   123,   124,    63,    12,    36,    28,    29,    30,    31,
      32,    33,    34,    35,    63,   116,   100,   101,   134,    96,
      97,   119,   120,    64,    78,   126,   135,   147,    14,    17,
     142,   143,    36,    40,    64,    43,    51,    69,    62,    68,
      73,    78,    87,    99,    74,    84,    90,    91,    93,   103,
     104,   113,   105,   106,   110,   111,   127,   128,   130,   133,
     135,    75,   138,   139,   144,   149,   109,    60,   112,   129,
      67,   148,   132,     0,    65,     0,     0,    50
};

static const yytype_int16 yycheck[] =
{
      55,    97,    89,    22,     8,     5,     9,    10,    11,     5,
       3,     4,     0,     3,     4,     6,    12,     3,     9,     4,
       4,    40,     3,     4,     7,    25,    30,     4,   124,    13,
      14,    15,    16,    17,    18,    19,    20,    40,     5,    58,
      31,    32,    33,    34,    35,    98,   133,   143,     3,   145,
      27,    28,    29,     5,     5,    39,    13,    14,    15,    16,
      17,    18,    19,    20,     5,   118,     3,     4,   123,    21,
      22,    36,    37,    25,     3,     4,     3,     4,     4,     3,
      21,    22,    39,     5,    25,     3,     3,    26,     4,     4,
       3,     3,    12,     4,     8,     5,     5,     3,     5,     4,
       4,     3,     5,     4,     4,     4,     4,     4,    38,     3,
       3,    53,     4,     4,     4,     4,    91,    38,    96,   118,
      45,   145,   120,    -1,    43,    -1,    -1,    23
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,    42,    43,     0,     3,    44,    75,     7,     3,     8,
      30,     5,     5,     4,     4,    45,    76,     3,    77,     9,
      10,    11,    40,    48,    49,     3,     4,    78,    13,    14,
      15,    16,    17,    18,    19,    20,    39,    50,    51,    52,
       5,    54,    55,     3,    56,    57,    58,    53,    55,    46,
      49,     3,     6,     9,    31,    32,    33,    34,    35,     4,
      52,    55,     4,     5,    25,    60,     4,    58,     4,    26,
      62,    63,    64,     3,     8,    50,    69,    80,     3,    68,
      70,    81,    82,    83,     5,    79,    55,    12,    59,    61,
       5,     3,    47,     5,     4,    68,    21,    22,    60,     4,
       3,     4,    84,     4,     4,     5,     4,    59,    65,    63,
       4,     4,    69,     3,    70,     5,    61,    71,    72,    36,
      37,     4,    27,    28,    29,    66,     4,     4,     4,    71,
      38,    85,    85,     3,    68,     3,    70,    73,     4,     4,
      59,    67,    21,    22,     4,    74,    70,     4,    73,     4
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    41,    42,    43,    43,    43,    45,    46,    47,    44,
      48,    48,    49,    49,    49,    49,    50,    51,    51,    52,
      52,    52,    52,    52,    52,    52,    52,    52,    53,    54,
      55,    55,    56,    57,    57,    58,    59,    59,    60,    60,
      61,    62,    62,    63,    64,    65,    65,    66,    66,    66,
      67,    68,    68,    68,    69,    69,    70,    71,    71,    72,
      72,    73,    73,    73,    74,    74,    76,    75,    77,    77,
      78,    78,    78,    78,    78,    78,    78,    79,    80,    81,
      82,    83,    83,    84,    84,    85
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     1,     2,     2,     0,     0,     0,     0,    13,
       3,     2,     2,     2,     2,     2,     2,     2,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     2,     2,
       2,     1,     2,     2,     1,     4,     2,     0,     1,     1,
       2,     3,     1,     1,     4,     2,     0,     4,     2,     2,
       1,     1,     4,     4,     2,     0,     4,     2,     0,     1,
       1,     4,     4,     1,     2,     0,     0,     9,     2,     0,
       3,     3,     3,     3,     3,     5,     4,     2,     2,     2,
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
#line 108 "parser.y" /* yacc.c:1646  */
    {
			   if( !domainName )
			     {
			       fprintf( stderr, "parsing domain: no problem domain clause (bad file order)\n" );
			       usage();
			     }
			   
			   strcpy( (yyvsp[-1].ident), translate( (yyvsp[-1].ident) ) );
			   strcpy( domainName, translate( domainName ) );
			   if( strcmp( domainName, (yyvsp[-1].ident) ) )
			     {
			       fprintf( stderr, "WARNING: domain names don't match (%s,%s)\n", (yyvsp[-1].ident), domainName );
			     }
			 }
#line 1459 "y.tab.c" /* yacc.c:1646  */
    break;

  case 7:
#line 123 "parser.y" /* yacc.c:1646  */
    {
			   identifyObjects();
			   tagFormulaList( initialSituation );
			   tagFormulaList( goalSituation );

			   emitPrologue();
			 }
#line 1471 "y.tab.c" /* yacc.c:1646  */
    break;

  case 8:
#line 131 "parser.y" /* yacc.c:1646  */
    {
			   struct action_s *action;
			   struct idlist_s *var;
			   int i;

			   identifyTypes();
			   identifyDomains();

			   emitNames();
			   mystery();

			   /* how much parameters can an action have? */
			   for( action = actions; action != NULL; action = action->next )
			     {
			       for( var = action->vars, i = 0; var != NULL; var = var->next, ++i );
			       maxParameters = (maxParameters > i ? maxParameters : i);
			     }

			   emitActions();
			   emitEpilogue();
			 }
#line 1497 "y.tab.c" /* yacc.c:1646  */
    break;

  case 9:
#line 153 "parser.y" /* yacc.c:1646  */
    {
			 domainParsed = 1;
		       }
#line 1505 "y.tab.c" /* yacc.c:1646  */
    break;

  case 17:
#line 172 "parser.y" /* yacc.c:1646  */
    {
			 checkRequirement( (yyvsp[0].integer) );
		       }
#line 1513 "y.tab.c" /* yacc.c:1646  */
    break;

  case 18:
#line 176 "parser.y" /* yacc.c:1646  */
    {
			 checkRequirement( (yyvsp[0].integer) );
		       }
#line 1521 "y.tab.c" /* yacc.c:1646  */
    break;

  case 19:
#line 182 "parser.y" /* yacc.c:1646  */
    {
			 (yyval.integer) = STRIPS;
		       }
#line 1529 "y.tab.c" /* yacc.c:1646  */
    break;

  case 20:
#line 186 "parser.y" /* yacc.c:1646  */
    {
			 (yyval.integer) = TYPING;
		       }
#line 1537 "y.tab.c" /* yacc.c:1646  */
    break;

  case 21:
#line 190 "parser.y" /* yacc.c:1646  */
    {
			 (yyval.integer) = EQUALITY;

			 /* create the equal predicate */
			 predicates = (struct aformula_s *) malloc( sizeof( struct aformula_s ) );
			 assert( predicates != NULL );
			 predicates->id = 0;
			 predicates->neg = 0;
			 predicates->type = 0;
			 predicates->equal = 1;
			 predicates->func = strdup( "=" );
			 predicates->args = NULL;
			 predicates->domain = NULL;
			 predicates->next = NULL;
		       }
#line 1557 "y.tab.c" /* yacc.c:1646  */
    break;

  case 22:
#line 206 "parser.y" /* yacc.c:1646  */
    {
			 (yyval.integer) = CONDITIONAL;
		       }
#line 1565 "y.tab.c" /* yacc.c:1646  */
    break;

  case 23:
#line 210 "parser.y" /* yacc.c:1646  */
    {
			 (yyval.integer) = DOM_AXIOMS;
		       }
#line 1573 "y.tab.c" /* yacc.c:1646  */
    break;

  case 24:
#line 214 "parser.y" /* yacc.c:1646  */
    {
			 (yyval.integer) = DISJUNCTIVE;
		       }
#line 1581 "y.tab.c" /* yacc.c:1646  */
    break;

  case 25:
#line 218 "parser.y" /* yacc.c:1646  */
    {
			 (yyval.integer) = ADL;
		       }
#line 1589 "y.tab.c" /* yacc.c:1646  */
    break;

  case 26:
#line 222 "parser.y" /* yacc.c:1646  */
    {
			 (yyval.integer) = E_PRECONDITIONS;
		       }
#line 1597 "y.tab.c" /* yacc.c:1646  */
    break;

  case 27:
#line 226 "parser.y" /* yacc.c:1646  */
    {
			 (yyval.integer) = U_PRECONDITIONS;
		       }
#line 1605 "y.tab.c" /* yacc.c:1646  */
    break;

  case 28:
#line 232 "parser.y" /* yacc.c:1646  */
    {
			 (yyval.idlist) = (yyvsp[-1].idlist);
		       }
#line 1613 "y.tab.c" /* yacc.c:1646  */
    break;

  case 29:
#line 238 "parser.y" /* yacc.c:1646  */
    {
			 struct idlist_s *object;

			 if( objects == NULL )
			   objects = (yyvsp[-1].idlist);
			 else
			   {
			     for( object = objects; object->next != NULL; object = object->next );
			     object->next = (yyvsp[-1].idlist);
			   }
		       }
#line 1629 "y.tab.c" /* yacc.c:1646  */
    break;

  case 30:
#line 252 "parser.y" /* yacc.c:1646  */
    {
			 (yyval.idlist) = (struct idlist_s *) malloc( sizeof( struct idlist_s ) );
			 assert( (yyval.idlist) != NULL );
			 (yyval.idlist)->id = 0;
			 (yyval.idlist)->name = (yyvsp[-1].ident);
			 (yyval.idlist)->type = NULL;
			 (yyval.idlist)->next = (yyvsp[0].idlist);
		       }
#line 1642 "y.tab.c" /* yacc.c:1646  */
    break;

  case 31:
#line 261 "parser.y" /* yacc.c:1646  */
    {  
			 (yyval.idlist) = (struct idlist_s *) malloc( sizeof( struct idlist_s ) );
			 assert( (yyval.idlist) != NULL );
			 (yyval.idlist)->id = 0;
			 (yyval.idlist)->name = (yyvsp[0].ident);
			 (yyval.idlist)->type = NULL;
			 (yyval.idlist)->next = NULL;
		       }
#line 1655 "y.tab.c" /* yacc.c:1646  */
    break;

  case 32:
#line 272 "parser.y" /* yacc.c:1646  */
    {
			 struct aformula_s *predicate;
			 int i;

			 if( predicates == NULL )
			   predicates = (yyvsp[-1].aformula);
			 else
			   {
			     for( predicate = predicates; predicates->next != NULL; predicate = predicate->next );
			     predicate->next = (yyvsp[-1].aformula);
			   }
			    
			 i = 1;
			 for( predicate = predicates; predicate != NULL; predicate = predicate->next )
			   predicate->id = i++;
                       }
#line 1676 "y.tab.c" /* yacc.c:1646  */
    break;

  case 33:
#line 292 "parser.y" /* yacc.c:1646  */
    {
			 (yyvsp[0].aformula)->next = (yyvsp[-1].aformula);
			 (yyval.aformula) = (yyvsp[0].aformula);
		       }
#line 1685 "y.tab.c" /* yacc.c:1646  */
    break;

  case 35:
#line 301 "parser.y" /* yacc.c:1646  */
    {
			 (yyval.aformula) = (struct aformula_s *) malloc( sizeof( struct aformula_s ) );
			 assert( (yyval.aformula) != NULL );
			 (yyval.aformula)->id = -1;
			 (yyval.aformula)->neg = 0;
			 (yyval.aformula)->type = 0;
			 (yyval.aformula)->func = (yyvsp[-2].ident);
			 (yyval.aformula)->equal = 0;
			 (yyval.aformula)->args = (yyvsp[-1].idlist);
			 (yyval.aformula)->domain = NULL;
			 (yyval.aformula)->next = NULL;
		       }
#line 1702 "y.tab.c" /* yacc.c:1646  */
    break;

  case 36:
#line 316 "parser.y" /* yacc.c:1646  */
    {
			 (yyval.idlist) = (struct idlist_s *) malloc( sizeof( struct idlist_s ) );
			 assert( (yyval.idlist) != NULL );
			 (yyval.idlist)->id = 0;
			 (yyval.idlist)->name = (yyvsp[-1].ident);
			 (yyval.idlist)->type = NULL;
			 (yyval.idlist)->next = (yyvsp[0].idlist);
		       }
#line 1715 "y.tab.c" /* yacc.c:1646  */
    break;

  case 37:
#line 325 "parser.y" /* yacc.c:1646  */
    {  
			 (yyval.idlist) = NULL;
		       }
#line 1723 "y.tab.c" /* yacc.c:1646  */
    break;

  case 39:
#line 332 "parser.y" /* yacc.c:1646  */
    {
			 (yyval.ident) = "=";
		       }
#line 1731 "y.tab.c" /* yacc.c:1646  */
    break;

  case 40:
#line 338 "parser.y" /* yacc.c:1646  */
    {
			 (yyval.ident) = (char *) malloc( strlen( (yyvsp[0].ident) ) + 2 );
			 assert( (yyval.ident) != NULL );
			 (yyval.ident)[0] = '\0';
			 strcat( (yyval.ident), "?" );
			 strcat( (yyval.ident), (yyvsp[0].ident) );
		       }
#line 1743 "y.tab.c" /* yacc.c:1646  */
    break;

  case 44:
#line 356 "parser.y" /* yacc.c:1646  */
    {
			 struct aformula_s *formula, *tmp;
			 struct action_s *action;
			 struct body_s *body;
			 
			 action = (struct action_s *) malloc( sizeof( struct action_s ) );
			 assert( action != NULL );
			 action->name = (yyvsp[-2].ident);
			 action->next = NULL;
			 
			 for( body = (yyvsp[-1].body); body != NULL; body = body->next )
			   switch( body->type )
			     {
			     case PARAMETERS:
			       action->vars = body->data.parameters;
			       break;
			     case PRECONDITION:
			       action->prec = body->data.precondition;
			       break;
			     case EFFECT:
			       action->add = NULL;
			       action->del = NULL;
			       formula = body->data.effect;
			       while( formula != NULL )
				 {
				   tmp = formula->next;
				   if( !formula->neg )
				     {
				       formula->next = action->add;
				       action->add = formula;
				     }
				   else
				     {
				       formula->next = action->del;
				       action->del = formula;
				     }
				   formula = tmp;
				 }
			       break;
			     }

			 /* identification */
			 idMatchArgsVars( action->prec, action->vars );
			 idMatchArgsVars( action->add, action->vars );
			 idMatchArgsVars( action->del, action->vars );

			 /* link actions */
			 action->next = actions;
			 actions = action;
		       }
#line 1798 "y.tab.c" /* yacc.c:1646  */
    break;

  case 45:
#line 409 "parser.y" /* yacc.c:1646  */
    {
			 (yyvsp[0].body)->next = (yyvsp[-1].body);
			 (yyval.body) = (yyvsp[0].body);
		       }
#line 1807 "y.tab.c" /* yacc.c:1646  */
    break;

  case 46:
#line 414 "parser.y" /* yacc.c:1646  */
    {
			 (yyval.body) = NULL;
		       }
#line 1815 "y.tab.c" /* yacc.c:1646  */
    break;

  case 47:
#line 420 "parser.y" /* yacc.c:1646  */
    {
			 (yyval.body) = (struct body_s *) malloc( sizeof( struct body_s ) );
			 assert( (yyval.body) != NULL );
			 (yyval.body)->type = PARAMETERS;
			 (yyval.body)->data.parameters = (yyvsp[-1].idlist);
		       }
#line 1826 "y.tab.c" /* yacc.c:1646  */
    break;

  case 48:
#line 427 "parser.y" /* yacc.c:1646  */
    {
			 (yyval.body) = (struct body_s *) malloc( sizeof( struct body_s ) );
			 assert( (yyval.body) != NULL );
			 (yyval.body)->type = PRECONDITION;
			 (yyval.body)->data.precondition = (yyvsp[0].aformula);
		       }
#line 1837 "y.tab.c" /* yacc.c:1646  */
    break;

  case 49:
#line 434 "parser.y" /* yacc.c:1646  */
    {
			 (yyval.body) = (struct body_s *) malloc( sizeof( struct body_s ) );
			 assert( (yyval.body) != NULL );
			 (yyval.body)->type = EFFECT;
			 (yyval.body)->data.effect = (yyvsp[0].aformula);
		       }
#line 1848 "y.tab.c" /* yacc.c:1646  */
    break;

  case 50:
#line 443 "parser.y" /* yacc.c:1646  */
    {
			 struct idlist_s *var;
			 int i;
			 
			 i = 1;
			 for( var = (yyvsp[0].idlist); var != NULL; var = var->next )
			   var->id = i++;
			 (yyval.idlist) = (yyvsp[0].idlist);
		       }
#line 1862 "y.tab.c" /* yacc.c:1646  */
    break;

  case 52:
#line 456 "parser.y" /* yacc.c:1646  */
    {
			 (yyval.aformula) = (yyvsp[-1].aformula);
		       }
#line 1870 "y.tab.c" /* yacc.c:1646  */
    break;

  case 53:
#line 460 "parser.y" /* yacc.c:1646  */
    {
			 (yyvsp[-1].aformula)->neg = 1;
			 (yyval.aformula) = (yyvsp[-1].aformula);
		       }
#line 1879 "y.tab.c" /* yacc.c:1646  */
    break;

  case 54:
#line 468 "parser.y" /* yacc.c:1646  */
    {
			 if( (yyvsp[0].aformula) != NULL )
			   {
			     (yyvsp[0].aformula)->next = (yyvsp[-1].aformula);
			     (yyval.aformula) = (yyvsp[0].aformula);
			   }
			 else
			   (yyval.aformula) = (yyvsp[-1].aformula);
		       }
#line 1893 "y.tab.c" /* yacc.c:1646  */
    break;

  case 55:
#line 478 "parser.y" /* yacc.c:1646  */
    {
			 (yyval.aformula) = NULL;
		       }
#line 1901 "y.tab.c" /* yacc.c:1646  */
    break;

  case 56:
#line 485 "parser.y" /* yacc.c:1646  */
    {
			 (yyval.aformula) = (struct aformula_s *) malloc( sizeof( struct aformula_s ) );
			 assert( (yyval.aformula) != NULL );
			 (yyval.aformula)->id = -1;
			 (yyval.aformula)->neg = 0;
			 (yyval.aformula)->type = 0;
			 (yyval.aformula)->equal = 0;
			 (yyval.aformula)->func = (yyvsp[-2].ident);
			 (yyval.aformula)->args = (yyvsp[-1].idlist);
			 (yyval.aformula)->domain = NULL;
			 (yyval.aformula)->next = NULL;
		       }
#line 1918 "y.tab.c" /* yacc.c:1646  */
    break;

  case 57:
#line 500 "parser.y" /* yacc.c:1646  */
    {
			 (yyval.idlist) = (struct idlist_s *) malloc( sizeof( struct idlist_s ) );
			 assert( (yyval.idlist) != NULL );
			 (yyval.idlist)->id = 0;
			 (yyval.idlist)->name = (yyvsp[-1].ident);
			 (yyval.idlist)->type = NULL;
			 (yyval.idlist)->next = (yyvsp[0].idlist);
		       }
#line 1931 "y.tab.c" /* yacc.c:1646  */
    break;

  case 58:
#line 509 "parser.y" /* yacc.c:1646  */
    {  
			 (yyval.idlist) = NULL;
		       }
#line 1939 "y.tab.c" /* yacc.c:1646  */
    break;

  case 61:
#line 519 "parser.y" /* yacc.c:1646  */
    {
			 (yyval.aformula) = (yyvsp[-1].aformula);
		       }
#line 1947 "y.tab.c" /* yacc.c:1646  */
    break;

  case 62:
#line 523 "parser.y" /* yacc.c:1646  */
    {
			 (yyvsp[-1].aformula)->neg = 1;
			 (yyval.aformula) = (yyvsp[-1].aformula);
		       }
#line 1956 "y.tab.c" /* yacc.c:1646  */
    break;

  case 64:
#line 535 "parser.y" /* yacc.c:1646  */
    {
			 if( (yyvsp[0].aformula) != NULL )
			   {
			     (yyvsp[0].aformula)->next = (yyvsp[-1].aformula);
			     (yyval.aformula) = (yyvsp[0].aformula);
			   }
			 else
			   (yyval.aformula) = (yyvsp[-1].aformula);
		       }
#line 1970 "y.tab.c" /* yacc.c:1646  */
    break;

  case 65:
#line 545 "parser.y" /* yacc.c:1646  */
    {
			 (yyval.aformula) = NULL;
		       }
#line 1978 "y.tab.c" /* yacc.c:1646  */
    break;

  case 66:
#line 557 "parser.y" /* yacc.c:1646  */
    {
			 problemName = strdup( translate( (yyvsp[-1].ident) ) );

			 if( domainParsed )
			   {
			     fprintf( stderr, "parsing problem: bad file order\n" );
			     usage();
			   }
		       }
#line 1992 "y.tab.c" /* yacc.c:1646  */
    break;

  case 75:
#line 580 "parser.y" /* yacc.c:1646  */
    {
			 domainName = (yyvsp[-1].ident);
		       }
#line 2000 "y.tab.c" /* yacc.c:1646  */
    break;

  case 76:
#line 584 "parser.y" /* yacc.c:1646  */
    {
			 struct idlist_s *object;

			 if( objects == NULL )
			   objects = (yyvsp[-1].idlist);
			 else
			   {
			     for( object = objects; object->next != NULL; object = object->next );
			     object->next = (yyvsp[-1].idlist);
			   }
		       }
#line 2016 "y.tab.c" /* yacc.c:1646  */
    break;

  case 78:
#line 601 "parser.y" /* yacc.c:1646  */
    {
			 struct aformula_s *formula;

			 initialSituation = (yyvsp[-1].aformula);
			 for( formula = initialSituation; formula != NULL; formula = formula->next )
			   if( formula->neg )
			     fprintf( stderr, "WARNING: negative literal in initial situation\n" );
		       }
#line 2029 "y.tab.c" /* yacc.c:1646  */
    break;

  case 79:
#line 612 "parser.y" /* yacc.c:1646  */
    {
			 struct aformula_s *formula;

			 goalSituation = (yyvsp[-1].aformula);
			 for( formula = goalSituation; formula != NULL; formula = formula->next )
			   if( formula->neg )
			     fprintf( stderr, "WARNING: negative literal in goal situation\n" );
		       }
#line 2042 "y.tab.c" /* yacc.c:1646  */
    break;


#line 2046 "y.tab.c" /* yacc.c:1646  */
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
#line 637 "parser.y" /* yacc.c:1906  */



int
yyerror( const char *s )
{
  extern int lineno;

  fprintf( stderr, "%s:%d: %s\n", yyfile, lineno, s );
  return( 0 );
}


void
usage( void )
{
  fprintf( stderr, "usage: %s <domain-pddl> <problem-pddl>\n", yyparser );
  exit( -1 );
}


int
main( int argc, char **argv )
{
  int rv, fd, file;
  extern int yyparse( void );
  extern int lineno;

  /* check usage */
  yyparser = argv[0];
  if( argc != 3 )
    usage();

  /* open output files */
  xfile = fopen( "xfile", "w+" );
  yfile = fopen( "yfile", "w+" );
  zfile = fopen( "zfile", "w+" );
  if( !xfile || !yfile || !zfile )
    {
      perror( "main: " );
      exit( -1 );
    }

  /* parse files */
  rv = 0;
  for( file = 2; file > 0; --file )
    if( (fd = open( argv[file], O_RDONLY )) == -1 )
      {
	perror( "main: " );
	exit( -1 );
      }
    else
      {
	/* redirection of fd to stdin */
	if( file == 2 )
	  close( fileno( stdin ) );
	else
	  clearerr( stdin );
	dup( fd );
	yyfile = argv[file];
	lineno = 1;
	rv += yyparse();
	close( fileno( stdin ) );
	close( fd );
      }

  /* close output files */
  fclose( xfile );
  fclose( yfile );
  fclose( zfile );

  return( rv );
}


unsigned
hashFunction( char *key )
{
  unsigned rv;
  char *p;

  for( p = key, rv = 0; *p != '\0'; rv += (unsigned) *p++ );
  return( rv % HASHSIZE );
}


struct hashentry_s *
readHash( char *key )
{
  unsigned index;
  struct hashentry_s *entry;
  
  index = hashFunction( key );
  for( entry = hashtable[index]; entry != NULL; entry = entry->next )
    if( !strcmp( key, entry->name ) )
      return( entry );

  /* Is not in hash, insert it */
  entry = (struct hashentry_s *) malloc( sizeof( hashentry_s ) );
  assert( entry != NULL );
  entry->name = strdup( key );
  entry->number = ++number;
  entry->next = NULL;
  insertHash( key, entry );
  return( entry );
}


void
insertHash( char *key, struct hashentry_s *entry )
{
  unsigned index;

  index = hashFunction( key );
  entry->next = hashtable[index];
  hashtable[index] = entry;
}


char *
buildPredicate( struct aformula_s *pred, struct instantiation_s *insts )
{
  struct idlist_s *vp;
  struct instantiation_s *ip;
  static char buf[1024];

  strcpy( buf, "(" );
  strcat( buf, pred->func );
  strcat( buf, " " );
  for( vp = pred->args; vp != NULL; vp = vp->next )
    {
      if( vp->name[0] == '?' )
	{
	  for( ip = insts; ip != NULL; ip = ip->next )
	    if( !strcmp( ip->var, vp->name ) )
	      strcat( buf, ip->val );
	}
      else
	strcat( buf, vp->name );

      if( vp->next )
	strcat( buf, " " );
    }
  strcat( buf, ")" );
  return( buf );
}


void
checkRequirement( int req )
{
  switch( req )
    {
    case STRIPS:
    case EQUALITY:
      break;
    default:
      fprintf( stderr, "Not supported requirement %d\n", req );
    }
}


void
emitPrologue( void )
{
  fprintf( xfile, "/* automatically generated code -- DO NOT EDIT BY HAND! */\n\n" );
  fprintf( xfile, "#include <stdio.h>\n" );
  fprintf( xfile, "#include <string.h>\n" );
  fprintf( xfile, "#include <limits.h>\n" );
  fprintf( xfile, "#include <assert.h>\n" );
  fprintf( xfile, "#include \"planner.h\"\n\n" );
  fprintf( xfile, "static int preconditions[MAXPARAMETERS];\n" );
  fprintf( xfile, "int preclist[MAXPARAMETERS], addlist[MAXPARAMETERS];\n" );
  fprintf( xfile, "extern cost_t cost[], oldCost[];\n\n" );
  
  fprintf( yfile, "/* automatically generated code -- DO NOT EDIT BY HAND! */\n\n" );
  fprintf( yfile, "#include <stdio.h>\n" );
  fprintf( yfile, "#include <string.h>\n" );
  fprintf( yfile, "#include \"planner.h\"\n\n" );
  fprintf( yfile, "int (*actionPreconditionsTable[MAXSCHEMA])( register int *, register atom_t * );\n\n" );
  
  fprintf( zfile, "/* automatically generated code -- DO NOT EDIT BY HAND! */\n\n" );
  fprintf( zfile, "#include <stdlib.h>\n" );
  fprintf( zfile, "#include <string.h>\n" );
  fprintf( zfile, "#include \"planner.h\"\n\n" );
  fprintf( zfile, "extern void fillTableAux( void );\n\n" );
}


void
emitXFileEpilogue( void )
{
  char *name;
  struct action_s *action;
  struct aformula_s *formula;
  struct hashentry_s *entry;


  /* setupHeuristic */
  fprintf( xfile, "void\n" );
  fprintf( xfile, "setupHeuristic( void )\n" );
  fprintf( xfile, "{\n" );
  for( action = actions; action != NULL; action = action->next )
    {
      fprintf( xfile, "  heuristicOperatorFunctionTable[%d] = &heuristicAction_%s;\n", action->id, action->name );
      fprintf( xfile, "  preconditionHeuristicOperatorFunctionTable[%d] = &heuristicActionPreconditions_%s;\n", action->id, action->name );
    }
  fprintf( xfile, "}\n\n" );
  
  
  /* goalAtoms */
  fprintf( xfile, "int _low_goalAtoms[] = { " );
  
  for( formula = goalSituation; formula != NULL; formula = formula->next )
    {
      entry = readHash( name = buildPredicate( formula, NULL ) );
      fprintf( xfile, "%d /* %s */, ", entry->number, name );
    }
  fprintf( xfile, "0 };\n\n" );
}


void
emitYFileEpilogue( void )
{
  char *pn;
  struct hashentry_s *entry;
  struct aformula_s *formula;
  struct action_s *action;


  /* generate code for initial situation */
  fprintf( yfile, "void\n" );
  fprintf( yfile, "setInitialState( void )\n" );
  fprintf( yfile, "{\n" );
  fprintf( yfile, "  if( !initialized )\n" );
  fprintf( yfile, "    {\n" );

  for( formula = initialSituation; formula != NULL; formula = formula->next )
    if( !formula->neg )
      {
	/* we must assign a entry->number to all atoms */
	entry = readHash( pn = buildPredicate( formula, NULL ) );
	fprintf( yfile, "      staticState[%d].val = 1;\n", entry->number );
      }

  fprintf( yfile, "    }\n" );
  fprintf( yfile, "  else\n" );
  fprintf( yfile, "    {\n" );

  for( formula = goalSituation; formula != NULL; formula = formula->next )
    if( !formula->neg )
      {
	entry = readHash( pn = buildPredicate( formula, NULL ) );
	fprintf( yfile, "      staticState[%d].val = 1;\n", entry->number );
      }
  
  fprintf( yfile, "    }\n" );
  fprintf( yfile, "}\n\n" );


  /* generate code for goal situation */
  fprintf( yfile, "int\n" );
  fprintf( yfile, "goalState( atom_t *state )\n" );
  fprintf( yfile, "{\n" );
  fprintf( yfile, "  return(%s", (!goalSituation ? " 0" : " ") );

  for( formula = goalSituation; formula != NULL; formula = formula->next )
    {
      entry = readHash( pn = buildPredicate( formula, NULL ) );
      fprintf( yfile, "%sstate[%d].val", (formula->neg ? "!" : ""), entry->number );
      if( formula->next )
	fprintf( yfile, " && " );
    }
  
  fprintf( yfile, " );\n" );
  fprintf( yfile, "}\n\n" );
  
  
  /* generate code for operator tables */
  fprintf( yfile, "void\n" );
  fprintf( yfile, "fillTableAux( void )\n" );
  fprintf( yfile, "{\n" );
  
  for( action = actions; action != NULL; action = action->next )
    {
      fprintf( yfile, "  operatorFunctionTable[%d] = &action_%s;\n", action->id, action->name );
    }
  fprintf( yfile, "}\n\n" );
}


void
insertAtomsInHash( struct aformula_s *list )
{
  struct aformula_s *formula;
  struct idlist_s *var;
  int i;

  for( formula = list; formula != NULL; formula = formula->next )
    {
      fprintf( zfile, "  p[0] = %d;  /* %s */\n", formula->id, formula->func );
      for( var = formula->args, i = 1; var != NULL; var = var->next, ++i )
	fprintf( zfile, "  p[%d] = %d;  /* %s */\n", i, var->id, var->name );
      fprintf( zfile, "  p[%d] = -1;  /* end-marker */\n", i );
      fprintf( zfile, "  readAtomHash( p );\n" );
    }
}


void
emitZFileEpilogue( void )
{
  int i, j, k;
  struct aformula_s *predicate;
  struct action_s *action;
  struct idlist_s *var, *object, *domain;


  /* fillTable */
  fprintf( zfile, "void\n" );
  fprintf( zfile, "fillTable( void )\n" );
  fprintf( zfile, "{\n" );
  fprintf( zfile, "  register int i;\n" );
  fprintf( zfile, "  static int p[MAXPARAMETERS];\n\n" );
  fprintf( zfile, "  fillTableAux();\n" );
  fprintf( zfile, "  numberSchema = %d;\n\n", numberSchema );


  /* vars  */
  fprintf( zfile, "  /* vars */\n" );
  fprintf( zfile, "  vars = (int **) calloc( numberSchema, sizeof( int * ) );\n" );
  fprintf( zfile, "  for( i = 0; i < numberSchema; ++i )\n" );
  fprintf( zfile, "    vars[i] = (int *) calloc( %d, sizeof( int ) );\n", maxParameters + 1 );
  for( action = actions, i = 0; action != NULL; action = action->next, ++i )
    for( var = action->vars, j = 0; var != NULL; var = var->next, ++j )
      fprintf( zfile, "  vars[%d][%d] = %d;\n", i, j, var->id );
  fprintf( zfile, "\n" );
  
  
  /* values  */
  fprintf( zfile, "  /* values */\n" );
  fprintf( zfile, "  values = (int **) calloc( numberSchema * %d, sizeof( int * ) );\n", maxParameters + 1 );
  fprintf( zfile, "  for( i = 0; i < numberSchema * %d; ++i )\n", maxParameters );
  fprintf( zfile, "    values[i] = (int *) calloc( %d, sizeof( int ) );\n", numObjects + 1 );
  for( action = actions, i = 0; action != NULL; action = action->next, ++i )
    for( var = action->vars, j = 0; var != NULL; var = var->next, ++j )
      for( predicate = predicates; predicate != NULL; predicate = predicate->next )
	if( !predicate->equal )
	  {
	    if( (var->type->name == NULL) || !strcmp( var->type->name, predicate->func ) )
	      {
		if( (var->type->name == NULL) || (predicate->type == 0) )
		  {
		    for( object = objects, k = 0; object != NULL; object = object->next, ++k )
		      fprintf( zfile, "  values[numberSchema * %d + %d][%d] = %d;\n", var->id - 1, i, k, object->id );
		  }
		else
		  {
		    for( domain = predicate->domain, k = 0; domain != NULL; domain = domain->next, ++k )
		      fprintf( zfile, "  values[numberSchema * %d + %d][%d] = %d;\n", var->id - 1, i, k, domain->id );
		  }
		break;
	      }
	  }
  fprintf( zfile, "\n" );
  
  
  /* insert atoms in atomHashTable: don't change order (must match parser order) */
  fprintf( zfile, "  /* insert atoms in atomHashTable */\n" );
  insertAtomsInHash( goalSituation );
  insertAtomsInHash( initialSituation );
  fprintf( zfile, "}\n\n" );
}


void
emitEpilogue( void )
{
  emitXFileEpilogue();
  emitYFileEpilogue();
  emitZFileEpilogue();
}


void
emitAtom( struct aformula_s *formula, FILE *file )
{
  struct idlist_s *arg;
  int i;

  i = 1;
  fprintf( file, "  p[0] = %d;  /* %s */\n", formula->id, formula->func );
  for( arg = formula->args; arg != NULL; arg = arg->next, ++i )
    {
      if( arg->name[0] == '?' )
	fprintf( file, "  p[%d] = parameters[%d];  /* %s */\n", i, arg->id, arg->name );
      else
	fprintf( file, "  p[%d] = %d;  /* %s */\n", i, arg->id, arg->name );
    }
  fprintf( file, "  p[%d] = -1;  /* end-marker */\n", i );
  fprintf( file, "  idx = readAtomHash( p )->idx;\n" );
}


void
emitActionPreconditions( struct action_s *action, FILE *file )
{
  struct aformula_s *prec;
  struct idlist_s *arg;
  int i, label;

  fprintf( file, "int\n" );
  fprintf( file, "actionPreconditions_%s( register int *parameters, register atom_t *state )\n", action->name );
  fprintf( file, "{\n" );
  fprintf( file, "  register int idx, *pr;\n" );
  fprintf( file, "  static int p[MAXPARAMETERS];\n\n" );
  
  
  fprintf( file, "  /* preconditions */\n" );
  fprintf( file, "  pr = operatorPrec;\n" );
  for( prec = action->prec, label = 0; prec != NULL; prec = prec->next, ++label )
    if( prec->equal )
      {
#if 1 
	/* code for equality */
	for( arg = prec->args; arg != NULL; arg = arg->next )
	  if( arg->name[0] == '?' )
	    fprintf( file, "  if( parameters[%d] == 0 )  goto label%d;\n", arg->id, label );
	
	fprintf( file, "  if( " );
	for( arg = prec->args; arg != NULL; arg = arg->next )
	  {
	    if( arg->name[0] == '?' )
	      fprintf( file, "parameters[%d] ", arg->id );
	    else
	      fprintf( file, "%d ", arg->id );
	    
	    if( arg->next )
	      fprintf( file, "%c= ", (prec->neg ? '=' : '!') );
	  }
	fprintf( file, ") return( 0 );\n" );
	fprintf( file, " label%d:\n\n", label );
#endif
      }
    else if( !prec->neg )
      {
	i = 1;
	fprintf( file, "  p[0] = %d;  /* %s */ \n", prec->id, prec->func );
	for( arg = prec->args; arg != NULL; arg = arg->next, ++i )
	  {
	    if( arg->name[0] == '?' )
	      {
		fprintf( file, "  if( parameters[%d] == 0 )  goto label%d;  else", arg->id, label );
		fprintf( file, "  p[%d] = parameters[%d];  /* %s */\n", i, arg->id, arg->name );
	      }
	    else
	      fprintf( file, "  p[%d] = %d;  /* %s */\n", i, arg->id, arg->name );
	  }
	fprintf( file, "  p[%d] = -1;  /* end-marker */\n", i );
	fprintf( file, "  idx = readAtomHash( p )->idx;\n" );
	fprintf( file, "  if( !state[idx].val ) return( 0 );\n" );
	fprintf( file, "  *pr++ = idx;\n" );
	fprintf( file, " label%d:\n\n", label );
      }
    else
      {
	fprintf( stderr, "WARNING: negative literal in precondition of %s\n", translate( action->name ) );
      }

  /* trailer */
  fprintf( file, "  *pr++ = 0;\n" );
  fprintf( file, "  operatorPrecSize = pr - operatorPrec;\n" );
  fprintf( file, "  return( 1 );\n" );
  fprintf( file, "}\n\n" );
}


void
emitHeuristicActionPreconditions( struct action_s *action, FILE *file )
{
  struct aformula_s *prec;
  struct idlist_s *arg;
  int i, label;

  fprintf( file, "int\n" );
  fprintf( file, "heuristicActionPreconditions_%s( register int *parameters )\n", action->name );
  fprintf( file, "{\n" );
  fprintf( file, "  register int idx;\n" );
  fprintf( file, "  static int p[MAXPARAMETERS];\n\n" );
  
  
  fprintf( file, "  /* preconditions */\n" );
  for( prec = action->prec, label = 0; prec != NULL; prec = prec->next, ++label )
    if( prec->equal )
      {
#if 1 
	/* code for equality */
	for( arg = prec->args; arg != NULL; arg = arg->next )
	  if( arg->name[0] == '?' )
	    fprintf( file, "  if( parameters[%d] == 0 )  goto label%d;\n", arg->id, label );

	fprintf( file, "  if( " );
	for( arg = prec->args; arg != NULL; arg = arg->next )
	  {
	    if( arg->name[0] == '?' )
	      fprintf( file, "parameters[%d] ", arg->id );
	    else
	      fprintf( file, "%d ", arg->id );
	    
	    if( arg->next )
	      fprintf( file, "%c= ", (prec->neg ? '=' : '!') );
	  }
	fprintf( file, ") return( 0 );\n" );
	fprintf( file, " label%d:\n\n", label );
#endif
      }
    else if( !prec->neg )
      {
	i = 1;
	fprintf( file, "  p[0] = %d;  /* %s */ \n", prec->id, prec->func );
	for( arg = prec->args; arg != NULL; arg = arg->next, ++i )
	  {
	    if( arg->name[0] == '?' )
	      {
		fprintf( file, "  if( parameters[%d] == 0 )  goto label%d;  else", arg->id, label );
		fprintf( file, "  p[%d] = parameters[%d];  /* %s */\n", i, arg->id, arg->name );
	      }
	    else
	      fprintf( file, "  p[%d] = %d;  /* %s */\n", i, arg->id, arg->name );
	  }
	fprintf( file, "  p[%d] = -1;  /* end-marker */\n", i );
	fprintf( file, "  idx = readAtomHash( p )->idx;\n" );
	fprintf( file, "  if( oldCost[idx].cost == INT_MAX ) return( 0 );\n" );
	fprintf( file, " label%d:\n\n", label );
      }
    else
      {
	fprintf( stderr, "WARNING: negative literal in precondition of %s\n", translate( action->name ) );
      }

  /* trailer */
  fprintf( file, "  return( 1 );\n" );
  fprintf( file, "}\n\n" );
}


void
emitAction( struct action_s *action, FILE *file )
{
  struct aformula_s *formula;
  struct idlist_s *arg;
  int i;


  /* function header */
  fprintf( file, "int\n" );
  fprintf( file, "action_%s( register int *parameters, register atom_t *state, "
	   "register atom_t *newState )\n", action->name );
  fprintf( file, "{\n" );
  fprintf( file, "  register int idx, *a, *d;\n" );
  fprintf( file, "  static int p[MAXPARAMETERS];\n\n" );


  /* code for preconditions */
  fprintf( file, "  /* preconditions */\n" );
  fprintf( file, "  if( !actionPreconditions_%s( parameters, state ) ) return( 0 );\n\n", action->name );

#if 1
  /* operator prunning via relevant atoms */
  fprintf( file, "  /* relevantAtoms testing */\n" );
  for( formula = action->add; formula != NULL; formula = formula->next )
    {
      i = 1;
      fprintf( file, "  p[0] = %d;  /* %s */ \n", formula->id, formula->func );
      for( arg = formula->args; arg != NULL; arg = arg->next, ++i )
	{
	  if( arg->name[0] == '?' )
	    fprintf( file, "  p[%d] = parameters[%d];  /* %s */\n", i, arg->id, arg->name );
	  else
	    fprintf( file, "  p[%d] = %d;  /* %s */\n", i, arg->id, arg->name );
	}
      fprintf( file, "  p[%d] = -1;  /* end-marker */\n", i );
      fprintf( file, "  if( relevantAtom[readAtomHash( p )->idx].val ) goto end;\n\n" );
    }
  fprintf( file, "  return( 0 );\n" );
  fprintf( file, " end:\n\n" );
#endif

  
  /* code for add-list */
  fprintf( file, "  /* add-list */\n" );
  fprintf( file, "  a = operatorAdd;\n" );
  for( formula = action->add; formula != NULL; formula = formula->next )
    {
      emitAtom( formula, file );
      fprintf( file, "  *a++ = idx;\n" );
      fprintf( file, "  newState[idx].val = 1;\n" );
      if( formula->next != NULL )
	fprintf( file, "\n" );
    }
  fprintf( file, "  *a++ = 0;\n" );
  fprintf( file, "  operatorAddSize = a - operatorAdd;\n\n" );

  
  /* code for del-list */
  fprintf( file, "  /* del-list */\n" );
  fprintf( file, "  d = operatorDel;\n" );
  for( formula = action->del; formula != NULL; formula = formula->next )
    {
      emitAtom( formula, file );
      fprintf( file, "  *d++ = idx;\n" );
      fprintf( file, "  newState[idx].val = 0;\n\n" );
      if( formula->next != NULL )
	fprintf( file, "\n" );
    }
  fprintf( file, "  *d++ = 0;\n" );
  fprintf( file, "  operatorDelSize = d - operatorDel;\n\n" );
  
  
  /* function trailer */
  fprintf( file, "  return( 1 );\n" );
  fprintf( file, "}\n\n" );
}


void
emitHeuristicAction( struct action_s *action, FILE *file )
{
  struct aformula_s *formula;
  struct idlist_s *arg;


  /* function header */
  fprintf( file, "int\n" );
  fprintf( file, "heuristicAction_%s( register int *parameters )\n", action->name );
  fprintf( file, "{\n" );
  fprintf( file, "  register int idx, sum = 0, max = 0, prec = 0, add = 0;\n" );
  fprintf( file, "  static int p[MAXPARAMETERS];\n\n" );


  /* code for preconditions */
  fprintf( file, "  /* preconditions */\n" );
  for( formula = action->prec; formula != NULL; formula = formula->next )
    if( formula->equal )
      {
#if 1
	/* code for equality */
	fprintf( file, "  if( " );
	for( arg = formula->args; arg != NULL; arg = arg->next )
	  {
	    if( arg->name[0] == '?' )
	      fprintf( file, "parameters[%d] ", arg->id );
	    else
	      fprintf( file, "%d ", arg->id );
	    
	    if( arg->next )
	      fprintf( file, "%c= ", (formula->neg ? '=' : '!') );
	  }
	fprintf( file, ") return( 0 );\n\n" );
#endif
      }
    else if( !formula->neg )
      {
	emitAtom( formula, file );
	fprintf( file, "  preclist[prec] = idx;\n" );
	fprintf( file, "  preconditions[prec++] = idx;\n" );
	fprintf( file, "  assert( oldCost[idx].cost != INT_MAX );\n" );
	fprintf( file, "  sum += oldCost[idx].cost;\n" );
	fprintf( file, "  assert( oldCost[idx].max != INT_MAX );\n" );
	fprintf( file, "  max = (oldCost[idx].max > max ? oldCost[idx].max : max);\n\n" );
      }
    else
      {
	fprintf( stderr, "WARNING: negative literal in precondition of %s\n", translate( action->name ) );
      }
  fprintf( file, "  memset( &preclist[prec], 0, (MAXPARAMETERS - prec) * sizeof( int ) );\n\n" );

  
  /* code for add-list */
  fprintf( file, "  /* add-list */\n" );
  for( formula = action->add; formula != NULL; formula = formula->next )
    {
      emitAtom( formula, file );
      fprintf( file, "  if( sum + 1 < cost[idx].cost )\n" );
      fprintf( file, "    {\n" );
      fprintf( file, "      cost[idx].cost = sum + 1;\n" );
      fprintf( file, "      cost[idx].level = level;\n" );
      fprintf( file, "    }\n" );
      fprintf( file, "  if( max + 1 < cost[idx].max )\n" );
      fprintf( file, "    {\n" );
      fprintf( file, "      cost[idx].max = max + 1;\n" );
      fprintf( file, "    }\n" );
      fprintf( file, "  addlist[add++] = idx;\n\n" );

#if 1
      /* prunning via goal cone */
      fprintf( file, "  if( !initialized ) addParents( idx, preconditions );\n\n" );
#endif
    }
  fprintf( file, "  memset( &addlist[add], 0, (MAXPARAMETERS - add) * sizeof( int ) );\n\n" );
  
  
  /* code for del-list: not for heuristic computation */

  
  /* function trailer */
  fprintf( file, "  return( 1 );\n" );
  fprintf( file, "}\n\n" );
}


void
emitActions( void )
{
  struct action_s *action;
  int i;

  i = 0;
  prunning = 1;
  for( action = actions; action != NULL; action = action->next )
    {
      action->id = i++;
      emitHeuristicActionPreconditions( action, xfile );
      emitHeuristicAction( action, xfile );
      emitActionPreconditions( action, yfile );
      emitAction( action, yfile );
    }
  numberSchema = i;
}


char *
translate( char *name )
{
  char *ip, *jp;
  static char buffer[1024];

  for( ip = name, jp = buffer; *ip != '\0'; ++ip, ++jp )
    *jp = (*ip == 'a' ? '-' : *ip);
  *jp = '\0';
  return( buffer );
}


void
emitNames( void )
{
  struct action_s *action;
  struct idlist_s *object;
  struct aformula_s *predicate;
  
  fprintf( yfile, "char *_low_problemName = \"%s\";\n", problemName );
  fprintf( yfile, "char *_low_schemaName[] = { " );
  for( action = actions; action != NULL; action = action->next )
    fprintf( yfile, "\"%s\", ", translate( action->name ) );
  fprintf( yfile, "NULL };\n\n" );
  
  fprintf( yfile, "char *_low_objectName[] = { " );
  for( object = objects; object != NULL; object = object->next )
    fprintf( yfile, "\"%s\", ", translate( object->name ) );
  fprintf( yfile, "NULL };\n\n" );
  
  fprintf( yfile, "char *_low_predicateName[] = { " );
  for( predicate = predicates; predicate != NULL; predicate = predicate->next )
    fprintf( yfile, "\"%s\", ", translate( predicate->func ) );
  fprintf( yfile, "NULL };\n\n" );
}


void
identifyObjects( void )
{
  struct idlist_s *object;
  int i;

  i = 1;
  for( object = objects; object != NULL; object = object->next )
    object->id = i++;
  numObjects = i - 1;
}


void
tagFormulaList( struct aformula_s *formulaList )
{
  struct aformula_s *formula, *predicate;
  struct idlist_s *var, *object;

  for( formula = formulaList; formula != NULL; formula = formula->next )
    {
      for( predicate = predicates; predicate != NULL; predicate = predicate->next )
	if( !strcmp( predicate->func, formula->func ) )
	  {
	    formula->id = predicate->id;
	    break;
	  }
      if( predicate == NULL )
	{
	  fprintf( stderr, "tagFormulaList: no predicate for %s\n", translate( formula->func ) );
	  return;
	}
	
      for( var = formula->args; var != NULL; var = var->next )
	{
	  for( object = objects; object != NULL; object = object->next )
	    if( !strcmp( object->name, var->name ) )
	      {
		var->id = object->id;
		break;
	      }
	  if( object == NULL )
	    {
	      fprintf( stderr, "tagFormulaList: no object for %s\n", var->name );
	      return;
	    }
	}
    }
}


void
identifyTypes( void )
{
  struct aformula_s *predicate, *formula;
  struct action_s *action;

  /* Strips uses predicates for types. A sound way for its identification: those predicates that
     doesn't appear in add-lists or del-lists.
  */
  for( predicate = predicates; predicate != NULL; predicate = predicate->next )
    if( !predicate->equal )
      {
	for( action = actions; action != NULL; action = action->next )
	  {
	    for( formula = action->add; formula != NULL; formula = formula->next )
	      if( !strcmp( predicate->func, formula->func ) )
		break;
	    if( formula != NULL )
	      break;
	    
	    for( formula = action->del; formula != NULL; formula = formula->next )
	      if( !strcmp( predicate->func, formula->func ) )
		break;
	    if( formula != NULL )
	      break;
	  }
	
	if( (action == NULL) )
	  {
	    predicate->type = 1;
	    for( formula = initialSituation; formula != NULL; formula = formula->next )
	      if( predicate->id == formula->id )
		++predicate->type;
#if 0
	    fprintf( stderr, "(%s ...) is a type with dim = %d\n", predicate->func, predicate->type );
#endif
	  }
      }
}


void
mystery( void )
{
  struct idlist_s *arg, *before, *var, *type, *tmp;
  struct aformula_s *predicate, *pBefore, *prec;
  struct action_s *action;
  int change;

  /* sort predicates according to dimension */
  change = 1;
  while( change )
    {
      change = 0;
      pBefore = NULL;
      for( predicate = predicates; predicate->next != NULL; pBefore = predicate, predicate = predicate->next )
	if( predicate->next->type < predicate->type )
	  {
	    change = 1;
	    if( pBefore != NULL )
	      {
		pBefore->next = predicate->next;
		predicate->next = predicate->next->next;
		pBefore->next->next = predicate;
		predicate = pBefore->next;
	      }
	    else
	      {
		predicates = predicate->next;
		predicate->next = predicate->next->next;
		predicates->next = predicate;
		predicate = predicates;
	      }
	  }
    }
#if 0
  for( predicate = predicates; predicate != NULL; predicate = predicate->next )
    fprintf( stderr, "%s = %d\n", predicate->func, predicate->type );
#endif


  /* classify arguments of actions */
  for( action = actions; action != NULL; action = action->next )
    for( var = action->vars; var != NULL; var = var->next )
      for( prec = action->prec; prec != NULL; prec = prec->next )
	if( !prec->neg && !prec->equal )
	  {
	    for( arg = prec->args; arg != NULL; arg = arg->next )
	      if( arg->id == var->id )
		{
		  tmp = (struct idlist_s *) malloc( sizeof( struct idlist_s ) );
		  assert( tmp != NULL );
		  tmp->id = 0;
		  tmp->name = prec->func;
		  tmp->type = NULL;
		  tmp->next = var->type;
		  var->type = tmp;
		  
		  for( predicate = predicates; predicate != NULL; predicate = predicate->next )
		    if( !strcmp( predicate->func, tmp->name ) )
		      {
			tmp->id = (predicate->type > 0 ? predicate->type : 10000);
			break;
		      }
		  if( predicate == NULL )
		    fprintf( stderr, "mystery: no predicate for %s\n", translate( tmp->name ) );
		  break;
		}
	    if( arg == NULL )
	      {
		tmp = (struct idlist_s *) malloc( sizeof( struct idlist_s ) );
		assert( tmp != NULL );
		tmp->id = 10000;
		tmp->name = NULL;
		tmp->type = NULL;
		tmp->next = var->type;
		var->type = tmp;
	      }
	  }


  /* sort types for each action arguments */
  for( action = actions; action != NULL; action = action->next )
    for( var = action->vars; var != NULL; var = var->next )
      {
	change = 1;
	while( change )
	  {
	    change = 0;
	    before = NULL;
	    for( type = var->type; type->next != NULL; before = type, type = type->next )
	      if( type->next->id < type->id )
		{
		  change = 1;
		  if( before != NULL )
		    {
		      before->next = type->next;
		      type->next = type->next->next;
		      before->next->next = type;
		      type = before->next;
		    }
		  else
		    {
		      var->type = type->next;
		      type->next = type->next->next;
		      var->type->next = type;
		      type = var->type;
		    }
		}
	  }
      }
#if 0
  for( action = actions; action != NULL; action = action->next )
    for( var = action->vars; var != NULL; var = var->next )
      if( var->type->name != NULL )
	fprintf( stderr, "action %s: type of var %s is %s\n", translate( action->name ), var->name, var->type->name );
  fprintf( stderr, "\n" );
#endif


  /* sort action arguments according to dimension */
  for( action = actions; action != NULL; action = action->next )
    if( action->vars != NULL )
      {
	change = 1;
	while( change )
	  {
	    change = 0;
	    before = NULL;
	    for( var = action->vars; var->next != NULL; before = var, var = var->next )
	      if( var->next->type->id < var->type->id )
		{
		  change = 1;
		  if( before != NULL )
		    {
		      before->next = var->next;
		      var->next = var->next->next;
		      before->next->next = var;
		      var = before->next;
		    }
		  else
		    {
		      action->vars = var->next;
		      var->next = var->next->next;
		      action->vars->next = var;
		      var = action->vars;
		    }
		}
	  }
      }
#if 0
  for( action = actions; action != NULL; action = action->next )
    for( var = action->vars; var != NULL; var = var->next )
      if( var->type->name != NULL )
	fprintf( stderr, "action %s: type of var %s is %s\n", translate( action->name ), var->name, var->type->name );
#endif
}


struct idlist_s *
insertDomainInList( struct idlist_s *list, struct idlist_s *new )
{
  struct idlist_s *tmp;

  for( tmp = list; tmp != NULL; tmp = tmp->next )
    if( tmp->id == new->id )
      return( list );
  
  tmp = (struct idlist_s *) malloc( sizeof( struct idlist_s ) );
  assert( tmp != NULL );
  *tmp = *new;
  tmp->next = list;
  return( tmp );
}


void
identifyDomains( void )
{
  struct aformula_s *predicate, *formula;
  struct idlist_s *arg;

  for( predicate = predicates; predicate != NULL; predicate = predicate->next )
    if( !predicate->equal )
      {
	for( formula = initialSituation; formula != NULL; formula = formula->next )
	  if( predicate->id == formula->id ) 
	    for( arg = formula->args; arg != NULL; arg = arg->next )
	      predicate->domain = insertDomainInList( predicate->domain, arg );
      }

#if 0
  for( predicate = predicates; predicate != NULL; predicate = predicate->next )
    {
      fprintf( stderr, "\ndomain of %s[%d]: ", predicate->func, predicate->id );
      for( arg = predicate->domain; arg != NULL; arg = arg->next )
	fprintf( stderr, "%s[%d] ", arg->name, arg->id );
    }
  fprintf( stderr, "\n" );
#endif
}


void
idMatchArgsVars( struct aformula_s *formula, struct idlist_s *vars )
{
  struct aformula_s *form, *predicate;
  struct idlist_s *arg, *var, *object;

  /* identification */
  for( form = formula; form != NULL; form = form->next )
    {
      for( predicate = predicates; predicate != NULL; predicate = predicate->next )
	if( !strcmp( predicate->func, form->func ) )
	  {
	    form->id = predicate->id;
	    form->equal = predicate->equal;
	    break;
	  }
      if( predicate == NULL )
	fprintf( stderr, "idMatchArgsVars: no predicate for %s\n", translate( form->func ) );
			   
      for( arg = form->args; arg != NULL; arg = arg->next )
	if( arg->name[0] == '?' )
	  {
	    for( var = vars; var != NULL; var = var->next )
	      if( !strcmp( arg->name, var->name ) )
		{
		  arg->id = var->id;
		  break;
		}
	    if( var == NULL )
	      fprintf( stderr, "idMatchArgsVars: no variable for %s\n", arg->name );
	  }
	else
	  {
	    for( object = objects; object != NULL; object = object->next )
	      if( !strcmp( arg->name, object->name ) )
		{
		  arg->id = object->id;
		  break;
		}
	    if( object == NULL )
	      fprintf( stderr, "idMatchArgsVars: no object for %s\n", arg->name );
	  }
    }
}
