/* A Bison parser, made by GNU Bison 3.0.4.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015 Free Software Foundation, Inc.

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
#define YYBISON_VERSION "3.0.4"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1


/* Substitute the variable and function names.  */
#define yyparse         dis_fct_pddlparse
#define yylex           dis_fct_pddllex
#define yyerror         dis_fct_pddlerror
#define yydebug         dis_fct_pddldebug
#define yynerrs         dis_fct_pddlnerrs

#define yylval          dis_fct_pddllval
#define yychar          dis_fct_pddlchar

/* Copy the first part of user declarations.  */
#line 49 "scan-fct_pddl.y" /* yacc.c:339  */

//#define YYDEBUG 1
#ifdef YYDEBUG
  extern int yydebug=1;
#endif


#include <stdio.h>
#include <string.h> 
#include "dis_ff.h"
#include "lpg.h"
#include "dis_memory.h"
#include "dis_parse.h"
#include "dis_constraints.h"

#ifndef SCAN_ERR
#define SCAN_ERR
#define DEFINE_dis_EXPECTED            0
#define PROBLEM_dis_EXPECTED           1
#define PROBNAME_dis_EXPECTED          2
#define LBRACKET_dis_EXPECTED          3
#define RBRACKET_dis_EXPECTED          4
#define DOMDEFS_dis_EXPECTED           5
#define REQUIREM_dis_EXPECTED          6
#define TYPEDLIST_dis_EXPECTED         7
#define DOMdis_EXT_dis_EXPECTED            8
#define DOMdis_EXTNAME_dis_EXPECTED        9
#define TYPEDEF_dis_EXPECTED          10
#define CONSTLIST_dis_EXPECTED        11
#define PREDDEF_dis_EXPECTED          12 
#define NAME_dis_EXPECTED             13
#define VARIABLE_dis_EXPECTED         14
#define ACTIONFUNCTdis_OR_dis_EXPECTED    15
#define dis_ATOM_Fdis_ORMULA_dis_EXPECTED     16
#define EFFECT_DEF_dis_EXPECTED       17
#define NEG_Fdis_ORMULA_dis_EXPECTED      18
#define dis_NOT_SUPPdis_ORTED             19
#define SITUATION_dis_EXPECTED        20
#define SITNAME_dis_EXPECTED          21
#define BDOMAIN_dis_EXPECTED          22
#define BADDOMAIN                 23
#define INIFACTS                  24
#define GOALDEF                   25
#define ADLGOAL                   26
#define CONSTRAINTLIST_EXPECTED	  27
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
  "requirement %s not supported by this version",  
  "'situation' expected",
  "situation name expected",
  "':domain' expected",
  "this problem needs another domain file",
  "initial facts definition expected",
  "goal definition expected",
  "first order logic expression expected",
  "constraint list expected",
  NULL
};


void dis_fcterr( int errno, char *par );


static int sact_err;
static char *sact_err_par = NULL;
static dis_Bool sis_negated = dis_FALSE;
static dis_Pldis_Operator *scur_op = NULL;
static dis_TypedList *forall_tl = NULL;

void dis_fcterr( int errno, char *par ) {

  sact_err = errno;

  if ( sact_err_par ) {
    free( sact_err_par );
  }
  if ( par ) {
    sact_err_par = dis_new_dis_Token( strlen(par)+1 );
    strcpy( sact_err_par, par);
  } else {
    sact_err_par = NULL;
  }

}

#line 181 "scan-fct_pddl.tab.c" /* yacc.c:339  */

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


/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int dis_fct_pddldebug;
#endif

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    DEFINE_TOK = 258,
    PROBLEM_TOK = 259,
    SITUATION_TOK = 260,
    BSITUATION_TOK = 261,
    OBJECTS_TOK = 262,
    BDOMAIN_TOK = 263,
    INIT_TOK = 264,
    GOAL_TOK = 265,
    METRIC_TOK = 266,
    dis_AND_TOK = 267,
    dis_NOT_TOK = 268,
    NAME = 269,
    VARIABLE = 270,
    NUM = 271,
    LE_TOK = 272,
    LEQ_TOK = 273,
    EQ_TOK = 274,
    GEQ_TOK = 275,
    GE_TOK = 276,
    MINUS_TOK = 277,
    AD_TOK = 278,
    MU_TOK = 279,
    DI_TOK = 280,
    Fdis_ORdis_ALL_TOK = 281,
    IMPLY_TOK = 282,
    dis_OR_TOK = 283,
    dis_EXISTS_TOK = 284,
    EITHER_TOK = 285,
    OPEN_PAREN = 286,
    CLOSE_PAREN = 287,
    AT_TOK = 288,
    PREFERENCE_TOK = 289,
    IS_VIOLATED_TOK = 290
  };
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED

union YYSTYPE
{
#line 160 "scan-fct_pddl.y" /* yacc.c:355  */


  char string[dis_MAX_LENGTH];
  char* pstring;
  dis_Parsedis_ExpNode *pdis_Parsedis_ExpNode;
  dis_PlNode* pdis_PlNode;
  dis_FactList* pdis_FactList;
  dis_TokenList* pdis_TokenList;
  dis_TypedList* pdis_TypedList;


#line 266 "scan-fct_pddl.tab.c" /* yacc.c:355  */
};

typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE dis_fct_pddllval;

int dis_fct_pddlparse (void);



/* Copy the second part of user declarations.  */

#line 283 "scan-fct_pddl.tab.c" /* yacc.c:358  */

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
#define YYFINAL  5
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   248

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  36
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  37
/* YYNRULES -- Number of rules.  */
#define YYNRULES  93
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  222

/* YYTRANSLATE[YYX] -- Symbol number corresponding to YYX as returned
   by yylex, with out-of-bounds checking.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   290

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
      35
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,   242,   242,   245,   252,   251,   267,   277,   288,   292,
     295,   298,   301,   304,   310,   320,   319,   334,   333,   346,
     351,   356,   364,   362,   383,   405,   427,   436,   440,   460,
     481,   486,   492,   509,   517,   525,   533,   541,   549,   562,
     568,   574,   584,   607,   611,   630,   636,   656,   661,   678,
     695,   742,   750,   759,   765,   772,   779,   786,   797,   805,
     814,   820,   827,   834,   841,   848,   858,   863,   871,   876,
     885,   891,   900,   907,   920,   924,   935,   941,   951,   958,
     970,   972,   981,   992,  1012,  1014,  1023,  1034,  1055,  1080,
    1090,  1100,  1112,  1114
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || 0
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "DEFINE_TOK", "PROBLEM_TOK",
  "SITUATION_TOK", "BSITUATION_TOK", "OBJECTS_TOK", "BDOMAIN_TOK",
  "INIT_TOK", "GOAL_TOK", "METRIC_TOK", "dis_AND_TOK", "dis_NOT_TOK",
  "NAME", "VARIABLE", "NUM", "LE_TOK", "LEQ_TOK", "EQ_TOK", "GEQ_TOK",
  "GE_TOK", "MINUS_TOK", "AD_TOK", "MU_TOK", "DI_TOK",
  "Fdis_ORdis_ALL_TOK", "IMPLY_TOK", "dis_OR_TOK", "dis_EXISTS_TOK",
  "EITHER_TOK", "OPEN_PAREN", "CLOSE_PAREN", "AT_TOK", "PREFERENCE_TOK",
  "IS_VIOLATED_TOK", "$accept", "file", "problem_definition", "$@1",
  "problem_name", "base_domain_name", "problem_defs", "objects_def",
  "init_def", "$@2", "goal_def", "$@3", "pre_GD", "$@4", "pref_GD",
  "pre_GD_star", "metric_def", "adl_goal_description",
  "adl_goal_description1", "adl_goal_description_star", "init_el_plus",
  "init_el", "f_exp", "ground_f_exp", "plus_ground_f_exp_plus",
  "mul_ground_f_exp_plus", "literal_term", "atomic_formula_term",
  "term_star", "term", "name_plus", "typed_list_name",
  "typed_list_variable", "predicate", "literal_name",
  "atomic_formula_name", "name_star", YY_NULLPTR
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
     285,   286,   287,   288,   289,   290
};
# endif

#define YYPACT_NINF -112

#define yypact_value_is_default(Yystate) \
  (!!((Yystate) == (-112)))

#define YYTABLE_NINF -1

#define yytable_value_is_error(Yytable_value) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
      12,    49,    76,    12,  -112,  -112,  -112,    42,    80,  -112,
      85,     2,    58,    14,  -112,  -112,  -112,  -112,  -112,  -112,
    -112,   105,   117,  -112,  -112,   125,    53,   108,   110,    77,
     123,    37,   134,   141,  -112,  -112,  -112,    18,    34,  -112,
    -112,  -112,   109,   124,  -112,  -112,  -112,  -112,  -112,     4,
     130,   105,   141,   131,   126,  -112,    16,   133,   151,  -112,
    -112,  -112,   135,    66,    66,    46,    66,    66,   136,   135,
     135,   137,  -112,    32,    96,  -112,   151,    37,    37,    37,
      37,   155,  -112,  -112,  -112,   105,   156,   139,   157,   158,
     104,   142,   151,   144,   102,   132,   145,  -112,   146,  -112,
      71,    66,    66,  -112,  -112,    66,   147,    96,    66,    66,
     165,   135,   135,   149,   165,   135,   150,   152,   153,    56,
      37,    37,    37,   154,  -112,  -112,   159,   151,  -112,  -112,
    -112,  -112,  -112,   135,   161,  -112,  -112,    96,    66,    66,
      66,    66,   162,   163,   164,  -112,  -112,   166,   167,    94,
    -112,   168,  -112,  -112,   169,   170,  -112,  -112,  -112,  -112,
     171,  -112,    70,  -112,    73,   172,  -112,  -112,   173,   174,
     165,   175,    75,    66,    66,    66,  -112,  -112,  -112,  -112,
    -112,   176,   141,  -112,   177,  -112,   135,  -112,  -112,  -112,
    -112,  -112,  -112,  -112,   181,  -112,   178,  -112,  -112,   179,
     180,   182,   183,   165,   184,   123,   185,   186,   135,  -112,
    -112,  -112,  -112,  -112,   165,   187,  -112,  -112,   188,  -112,
    -112,  -112
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       2,     0,     0,     2,     4,     1,     3,     0,     0,     8,
       0,     0,     0,     0,     5,    12,     9,    10,    11,    13,
       6,    80,     0,    15,    17,     0,    80,     0,     0,     0,
       0,     0,     0,     0,    83,    14,     7,     0,     0,    45,
      47,    90,     0,     0,    20,    26,    38,    71,    58,     0,
       0,    80,    78,     0,     0,    88,     0,     0,    92,    16,
      46,    27,     0,     0,     0,    74,     0,     0,     0,     0,
      43,     0,    19,     0,    74,    18,    92,     0,     0,     0,
       0,     0,    29,    82,    79,    80,     0,     0,     0,     0,
       0,     0,    92,     0,     0,     0,     0,    30,     0,    51,
       0,     0,     0,    76,    77,     0,     0,    74,     0,     0,
      84,     0,    43,     0,    84,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    81,    89,     0,    92,    50,    93,
      91,    21,    28,    43,     0,    40,    70,    74,     0,     0,
       0,     0,     0,     0,     0,    73,    75,     0,     0,    84,
      22,     0,    44,    39,     0,     0,    25,    72,    59,    60,
       0,    66,     0,    68,     0,     0,    65,    49,     0,     0,
      84,     0,     0,     0,     0,     0,    33,    34,    35,    36,
      37,     0,     0,    87,     0,    41,     0,    24,    63,    61,
      67,    62,    69,    64,     0,    31,     0,    52,    53,     0,
       0,     0,     0,    84,     0,     0,     0,     0,     0,    55,
      54,    56,    57,    86,    84,     0,    42,    48,     0,    85,
      23,    32
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -112,   190,  -112,  -112,  -112,  -112,  -112,  -112,  -112,  -112,
    -112,  -112,   -85,  -112,  -112,  -112,  -112,   -61,   -30,   -93,
    -112,   189,   -60,   -64,  -112,  -112,  -112,   121,   -63,  -112,
     -50,   -16,  -111,   -25,   191,   192,   -56
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     2,     3,     7,     9,    15,    11,    16,    17,    29,
      18,    30,    43,   184,    44,    94,    19,   112,    97,   113,
      38,    39,   101,    50,   162,   164,    46,    47,   106,   107,
      53,    27,   150,    58,    40,    41,    93
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_uint8 yytable[] =
{
      45,    96,    84,   154,   102,   105,   108,   109,   111,   132,
      34,   117,   116,   119,   120,   121,   122,    74,    76,   152,
     118,    21,    22,    23,    24,    25,    77,    78,    79,    80,
      88,    54,    55,    13,    14,    83,   129,    56,   183,    81,
     169,   142,   143,     1,   146,   144,   115,    89,   147,   148,
     151,    57,     4,    48,   155,   160,   161,   163,   165,   196,
     103,   104,    99,    95,    45,    37,    59,    26,    49,   124,
      74,   168,    48,     8,   171,    32,     5,   100,   172,   173,
     174,   175,    99,    33,    10,   137,    48,    49,   159,    48,
      20,    99,   213,   138,   139,   140,   141,   100,   190,    12,
     192,    49,   189,   219,    49,   191,   100,   198,    37,   149,
     103,   104,   199,   200,   201,   202,   181,    54,    55,    26,
     215,    61,    62,    55,   182,   206,    63,    64,    65,    66,
      67,    28,   204,    42,   131,    68,    69,    70,    71,    31,
      35,    72,    36,    73,   133,    62,    55,   218,    51,    63,
      64,    65,    66,    67,    42,    52,    75,    86,   134,    69,
      70,    71,    82,    85,    90,    92,    95,   110,   114,   123,
      55,   125,   127,   126,   128,    45,   130,   135,   136,   145,
     149,   153,   156,    98,   157,   158,   166,     0,     0,     0,
     203,   167,   170,     6,   176,   177,   178,   207,   179,   180,
     185,   186,   187,   188,   193,   194,   195,   197,     0,   205,
     208,   209,   210,     0,   211,   212,   214,   216,   217,   220,
     221,     0,     0,     0,     0,     0,     0,    60,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    87,     0,    91
};

static const yytype_int16 yycheck[] =
{
      30,    62,    52,   114,    64,    65,    66,    67,    69,    94,
      26,    74,    73,    77,    78,    79,    80,    42,    14,   112,
      76,     7,     8,     9,    10,    11,    22,    23,    24,    25,
      14,    13,    14,    31,    32,    51,    92,    19,   149,    35,
     133,   101,   102,    31,   107,   105,    14,    31,   108,   109,
     111,    33,     3,    16,   115,   119,   120,   121,   122,   170,
      14,    15,    16,    31,    94,    31,    32,    14,    31,    85,
      95,   127,    16,    31,   137,    22,     0,    31,   138,   139,
     140,   141,    16,    30,     4,    14,    16,    31,    32,    16,
      32,    16,   203,    22,    23,    24,    25,    31,   162,    14,
     164,    31,    32,   214,    31,    32,    31,    32,    31,    15,
      14,    15,   172,   173,   174,   175,    22,    13,    14,    14,
     205,    12,    13,    14,    30,   186,    17,    18,    19,    20,
      21,    14,   182,    31,    32,    26,    27,    28,    29,    14,
      32,    32,    32,    34,    12,    13,    14,   208,    14,    17,
      18,    19,    20,    21,    31,    14,    32,    31,    26,    27,
      28,    29,    32,    32,    31,    14,    31,    31,    31,    14,
      14,    32,    14,    16,    32,   205,    32,    32,    32,    32,
      15,    32,    32,    62,    32,    32,    32,    -1,    -1,    -1,
      14,    32,    31,     3,    32,    32,    32,    16,    32,    32,
      32,    32,    32,    32,    32,    32,    32,    32,    -1,    32,
      32,    32,    32,    -1,    32,    32,    32,    32,    32,    32,
      32,    -1,    -1,    -1,    -1,    -1,    -1,    38,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    54,    -1,    57
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,    31,    37,    38,     3,     0,    37,    39,    31,    40,
       4,    42,    14,    31,    32,    41,    43,    44,    46,    52,
      32,     7,     8,     9,    10,    11,    14,    67,    14,    45,
      47,    14,    22,    30,    67,    32,    32,    31,    56,    57,
      70,    71,    31,    48,    50,    54,    62,    63,    16,    31,
      59,    14,    14,    66,    13,    14,    19,    33,    69,    32,
      57,    12,    13,    17,    18,    19,    20,    21,    26,    27,
      28,    29,    32,    34,    69,    32,    14,    22,    23,    24,
      25,    35,    32,    67,    66,    32,    31,    71,    14,    31,
      31,    70,    14,    72,    51,    31,    53,    54,    63,    16,
      31,    58,    58,    14,    15,    58,    64,    65,    58,    58,
      31,    53,    53,    55,    31,    14,    53,    64,    72,    59,
      59,    59,    59,    14,    67,    32,    16,    14,    32,    72,
      32,    32,    48,    12,    26,    32,    32,    14,    22,    23,
      24,    25,    58,    58,    58,    32,    64,    58,    58,    15,
      68,    53,    55,    32,    68,    53,    32,    32,    32,    32,
      59,    59,    60,    59,    61,    59,    32,    32,    72,    55,
      31,    64,    58,    58,    58,    58,    32,    32,    32,    32,
      32,    22,    30,    68,    49,    32,    32,    32,    32,    32,
      59,    32,    59,    32,    32,    32,    68,    32,    32,    58,
      58,    58,    58,    14,    66,    32,    53,    16,    32,    32,
      32,    32,    32,    68,    32,    48,    32,    32,    53,    68,
      32,    32
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    36,    37,    37,    39,    38,    40,    41,    42,    42,
      42,    42,    42,    42,    43,    45,    44,    47,    46,    48,
      48,    48,    49,    48,    50,    50,    50,    51,    51,    52,
      53,    53,    53,    54,    54,    54,    54,    54,    54,    54,
      54,    54,    54,    55,    55,    56,    56,    57,    57,    57,
      57,    58,    58,    58,    58,    58,    58,    58,    59,    59,
      59,    59,    59,    59,    59,    59,    60,    60,    61,    61,
      62,    62,    63,    63,    64,    64,    65,    65,    66,    66,
      67,    67,    67,    67,    68,    68,    68,    68,    69,    70,
      70,    71,    72,    72
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     0,     2,     0,     6,     4,     4,     0,     2,
       2,     2,     2,     2,     4,     0,     5,     0,     5,     2,
       1,     4,     0,     8,     5,     4,     1,     0,     2,     5,
       1,     4,     7,     5,     5,     5,     5,     5,     1,     4,
       4,     5,     7,     0,     2,     1,     2,     1,     8,     5,
       4,     1,     4,     4,     5,     5,     5,     5,     1,     4,
       4,     5,     5,     5,     5,     4,     1,     2,     1,     2,
       4,     1,     4,     4,     0,     2,     1,     1,     1,     2,
       0,     5,     4,     2,     0,     5,     4,     2,     1,     4,
       1,     4,     0,     2
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
        case 4:
#line 252 "scan-fct_pddl.y" /* yacc.c:1646  */
    { 
  dis_fcterr( PROBNAME_dis_EXPECTED, NULL ); 
}
#line 1519 "scan-fct_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 5:
#line 256 "scan-fct_pddl.y" /* yacc.c:1646  */
    {  
  dis_gproblem_name = (yyvsp[-2].pstring);
  if ( dis_gcmd_line.display_info >= 1 ) {
    printf("\n problem '%s' defined\n", dis_gproblem_name);
  }
}
#line 1530 "scan-fct_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 6:
#line 268 "scan-fct_pddl.y" /* yacc.c:1646  */
    { 
  (yyval.pstring) = dis_new_dis_Token( strlen((yyvsp[-1].string))+1 );
  strcpy((yyval.pstring), (yyvsp[-1].string));
}
#line 1539 "scan-fct_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 7:
#line 278 "scan-fct_pddl.y" /* yacc.c:1646  */
    { 
  if ( dis_SAME != strcmp((yyvsp[-1].string), dis_gdomain_name) ) {
    dis_fcterr( BADDOMAIN, NULL );
    yyerror();
  }
}
#line 1550 "scan-fct_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 14:
#line 311 "scan-fct_pddl.y" /* yacc.c:1646  */
    { 
  dis_gparse_objects = (yyvsp[-1].pdis_TypedList);
}
#line 1558 "scan-fct_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 15:
#line 320 "scan-fct_pddl.y" /* yacc.c:1646  */
    {
  dis_fcterr( INIFACTS, NULL ); 
}
#line 1566 "scan-fct_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 16:
#line 324 "scan-fct_pddl.y" /* yacc.c:1646  */
    {
  dis_gorig_initial_facts = dis_new_dis_PlNode(dis_AND);
  dis_gorig_initial_facts->sons = (yyvsp[-1].pdis_PlNode);
}
#line 1575 "scan-fct_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 17:
#line 334 "scan-fct_pddl.y" /* yacc.c:1646  */
    { 
  dis_fcterr( GOALDEF, NULL ); 
}
#line 1583 "scan-fct_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 18:
#line 339 "scan-fct_pddl.y" /* yacc.c:1646  */
    {
  (yyvsp[-1].pdis_PlNode)->next = dis_gorig_goal_facts;
  dis_gorig_goal_facts = (yyvsp[-1].pdis_PlNode);
}
#line 1592 "scan-fct_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 19:
#line 347 "scan-fct_pddl.y" /* yacc.c:1646  */
    {
        (yyval.pdis_PlNode) = NULL;
}
#line 1600 "scan-fct_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 20:
#line 352 "scan-fct_pddl.y" /* yacc.c:1646  */
    {
        (yyval.pdis_PlNode) = (yyvsp[0].pdis_PlNode);
}
#line 1608 "scan-fct_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 21:
#line 357 "scan-fct_pddl.y" /* yacc.c:1646  */
    {
        (yyval.pdis_PlNode) = dis_new_dis_PlNode(dis_AND);
        (yyval.pdis_PlNode)->sons = (yyvsp[-1].pdis_PlNode);
}
#line 1617 "scan-fct_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 22:
#line 364 "scan-fct_pddl.y" /* yacc.c:1646  */
    {
  forall_tl = (yyvsp[0].pdis_TypedList);
}
#line 1625 "scan-fct_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 23:
#line 368 "scan-fct_pddl.y" /* yacc.c:1646  */
    {
 
  dis_PlNode *pln;

  pln = dis_new_dis_PlNode(dis_ALL);
  pln->parse_vars = (yyvsp[-4].pdis_TypedList); 
  forall_tl = NULL;

  (yyval.pdis_PlNode) = pln;
  pln->sons = (yyvsp[-1].pdis_PlNode);
 
}
#line 1642 "scan-fct_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 24:
#line 384 "scan-fct_pddl.y" /* yacc.c:1646  */
    {
        dis_PrefNode *pref;
        dis_ConNode *con; 
        char temp[128];

	GpG.is_preferences = TRUE;
	sprintf(temp, "%s%d", (yyvsp[-2].string), dis_num_preference);
        pref = new_dis_PrefNode(temp, "GOALS", (yyvsp[-2].string));
        con = new_dis_ConNode(dis_ATOM_c);
        con->sons = (yyvsp[-1].pdis_PlNode);
        pref->body = con;
        pref->args = copy_dis_TypedList(forall_tl);
        pref->next = dis_gloaded_preferences;
        dis_gloaded_preferences = pref; 
        dis_num_preference++;
        if (forall_tl)
        	(yyval.pdis_PlNode) = dis_new_dis_PlNode(dis_TRU);
        else
	        (yyval.pdis_PlNode) = NULL;
}
#line 1667 "scan-fct_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 25:
#line 406 "scan-fct_pddl.y" /* yacc.c:1646  */
    {
        dis_PrefNode *pref;
        dis_ConNode *con;  
        char temp[128];

	GpG.is_preferences = TRUE;
	sprintf(temp, "ANONYMOUS%d", dis_num_preference);
        pref = new_dis_PrefNode(temp, "GOALS", "ANONYMOUS");
        con = new_dis_ConNode(dis_ATOM_c);
        con->sons = (yyvsp[-1].pdis_PlNode);
        pref->body = con;
        pref->args = copy_dis_TypedList(forall_tl);
        pref->next = dis_gloaded_preferences;
        dis_gloaded_preferences = pref; 
        dis_num_preference++;
        if (forall_tl)
        	(yyval.pdis_PlNode) = dis_new_dis_PlNode(dis_TRU);
        else
	        (yyval.pdis_PlNode) = NULL;
}
#line 1692 "scan-fct_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 26:
#line 428 "scan-fct_pddl.y" /* yacc.c:1646  */
    {
        (yyval.pdis_PlNode) = (yyvsp[0].pdis_PlNode);
}
#line 1700 "scan-fct_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 27:
#line 436 "scan-fct_pddl.y" /* yacc.c:1646  */
    {
        (yyval.pdis_PlNode) = NULL;
}
#line 1708 "scan-fct_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 28:
#line 441 "scan-fct_pddl.y" /* yacc.c:1646  */
    {
	dis_PlNode *p;

/* insert into the front */	
	if ((yyvsp[-1].pdis_PlNode))
	{
		for (p=(yyvsp[-1].pdis_PlNode);p->next;p=p->next)
			;
		p->next = (yyvsp[0].pdis_PlNode);
		(yyval.pdis_PlNode) = (yyvsp[-1].pdis_PlNode);
	}
	else	
		(yyval.pdis_PlNode) = (yyvsp[0].pdis_PlNode);
}
#line 1727 "scan-fct_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 29:
#line 461 "scan-fct_pddl.y" /* yacc.c:1646  */
    {

  if ( dis_gparse_metric != NULL ) {
    printf("\n\ndouble metric specification!\n\n");
    exit( 1 );
  }

  dis_gparse_optimization = dis_copy_dis_Token((yyvsp[-2].string));
  dis_gparse_metric = (yyvsp[-1].pdis_Parsedis_ExpNode);

}
#line 1743 "scan-fct_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 30:
#line 482 "scan-fct_pddl.y" /* yacc.c:1646  */
    {
        (yyval.pdis_PlNode) = (yyvsp[0].pdis_PlNode);
}
#line 1751 "scan-fct_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 31:
#line 487 "scan-fct_pddl.y" /* yacc.c:1646  */
    { 
  (yyval.pdis_PlNode) = dis_new_dis_PlNode(dis_AND);
  (yyval.pdis_PlNode)->sons = (yyvsp[-1].pdis_PlNode);
}
#line 1760 "scan-fct_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 32:
#line 495 "scan-fct_pddl.y" /* yacc.c:1646  */
    { 

  dis_PlNode *pln;

  pln = dis_new_dis_PlNode(dis_ALL);
  pln->parse_vars = (yyvsp[-3].pdis_TypedList);

  (yyval.pdis_PlNode) = pln;
  pln->sons = (yyvsp[-1].pdis_PlNode);

}
#line 1776 "scan-fct_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 33:
#line 510 "scan-fct_pddl.y" /* yacc.c:1646  */
    {
  (yyval.pdis_PlNode) = dis_new_dis_PlNode(dis_COMP);
  (yyval.pdis_PlNode)->comp = LE;
  (yyval.pdis_PlNode)->lh = (yyvsp[-2].pdis_Parsedis_ExpNode);
  (yyval.pdis_PlNode)->rh = (yyvsp[-1].pdis_Parsedis_ExpNode);
}
#line 1787 "scan-fct_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 34:
#line 518 "scan-fct_pddl.y" /* yacc.c:1646  */
    {
  (yyval.pdis_PlNode) = dis_new_dis_PlNode(dis_COMP);
  (yyval.pdis_PlNode)->comp = LEQ;
  (yyval.pdis_PlNode)->lh = (yyvsp[-2].pdis_Parsedis_ExpNode);
  (yyval.pdis_PlNode)->rh = (yyvsp[-1].pdis_Parsedis_ExpNode);
}
#line 1798 "scan-fct_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 35:
#line 526 "scan-fct_pddl.y" /* yacc.c:1646  */
    {
  (yyval.pdis_PlNode) = dis_new_dis_PlNode(dis_COMP);
  (yyval.pdis_PlNode)->comp = EQ;
  (yyval.pdis_PlNode)->lh = (yyvsp[-2].pdis_Parsedis_ExpNode);
  (yyval.pdis_PlNode)->rh = (yyvsp[-1].pdis_Parsedis_ExpNode);
}
#line 1809 "scan-fct_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 36:
#line 534 "scan-fct_pddl.y" /* yacc.c:1646  */
    {
  (yyval.pdis_PlNode) = dis_new_dis_PlNode(dis_COMP);
  (yyval.pdis_PlNode)->comp = GEQ;
  (yyval.pdis_PlNode)->lh = (yyvsp[-2].pdis_Parsedis_ExpNode);
  (yyval.pdis_PlNode)->rh = (yyvsp[-1].pdis_Parsedis_ExpNode);
}
#line 1820 "scan-fct_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 37:
#line 542 "scan-fct_pddl.y" /* yacc.c:1646  */
    {
  (yyval.pdis_PlNode) = dis_new_dis_PlNode(dis_COMP);
  (yyval.pdis_PlNode)->comp = GE;
  (yyval.pdis_PlNode)->lh = (yyvsp[-2].pdis_Parsedis_ExpNode);
  (yyval.pdis_PlNode)->rh = (yyvsp[-1].pdis_Parsedis_ExpNode);
}
#line 1831 "scan-fct_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 38:
#line 550 "scan-fct_pddl.y" /* yacc.c:1646  */
    { 
  if ( sis_negated ) {
    (yyval.pdis_PlNode) = dis_new_dis_PlNode(dis_NOT);
    (yyval.pdis_PlNode)->sons = dis_new_dis_PlNode(dis_ATOM);
    (yyval.pdis_PlNode)->sons->atom = (yyvsp[0].pdis_TokenList);
    sis_negated = dis_FALSE;
  } else {
    (yyval.pdis_PlNode) = dis_new_dis_PlNode(dis_ATOM);
    (yyval.pdis_PlNode)->atom = (yyvsp[0].pdis_TokenList);
  }
}
#line 1847 "scan-fct_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 39:
#line 563 "scan-fct_pddl.y" /* yacc.c:1646  */
    { 
  (yyval.pdis_PlNode) = dis_new_dis_PlNode(dis_OR);
  (yyval.pdis_PlNode)->sons = (yyvsp[-1].pdis_PlNode);
}
#line 1856 "scan-fct_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 40:
#line 569 "scan-fct_pddl.y" /* yacc.c:1646  */
    { 
  (yyval.pdis_PlNode) = dis_new_dis_PlNode(dis_NOT);
  (yyval.pdis_PlNode)->sons = (yyvsp[-1].pdis_PlNode);
}
#line 1865 "scan-fct_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 41:
#line 575 "scan-fct_pddl.y" /* yacc.c:1646  */
    { 
  dis_PlNode *np = dis_new_dis_PlNode(dis_NOT);
  np->sons = (yyvsp[-2].pdis_PlNode);
  np->next = (yyvsp[-1].pdis_PlNode);

  (yyval.pdis_PlNode) = dis_new_dis_PlNode(dis_OR);
  (yyval.pdis_PlNode)->sons = np;
}
#line 1878 "scan-fct_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 42:
#line 587 "scan-fct_pddl.y" /* yacc.c:1646  */
    { 

  dis_PlNode *pln;

  pln = dis_new_dis_PlNode(dis_EX);
  pln->parse_vars = (yyvsp[-3].pdis_TypedList);

  (yyval.pdis_PlNode) = pln;
  pln->sons = (yyvsp[-1].pdis_PlNode);

}
#line 1894 "scan-fct_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 43:
#line 607 "scan-fct_pddl.y" /* yacc.c:1646  */
    {
  (yyval.pdis_PlNode) = NULL;
}
#line 1902 "scan-fct_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 44:
#line 612 "scan-fct_pddl.y" /* yacc.c:1646  */
    {
  (yyvsp[-1].pdis_PlNode)->next = (yyvsp[0].pdis_PlNode);
  (yyval.pdis_PlNode) = (yyvsp[-1].pdis_PlNode);
}
#line 1911 "scan-fct_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 45:
#line 631 "scan-fct_pddl.y" /* yacc.c:1646  */
    {
  (yyval.pdis_PlNode) = (yyvsp[0].pdis_PlNode);
}
#line 1919 "scan-fct_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 46:
#line 637 "scan-fct_pddl.y" /* yacc.c:1646  */
    {
	dis_PlNode *p;

/* insert into the front */
	if ((yyvsp[-1].pdis_PlNode))
	{
		for (p=(yyvsp[-1].pdis_PlNode);p->next;p=p->next)
			;
		p->next = (yyvsp[0].pdis_PlNode);
		(yyval.pdis_PlNode) = (yyvsp[-1].pdis_PlNode);
	}
	else	
		(yyval.pdis_PlNode) = (yyvsp[0].pdis_PlNode);
}
#line 1938 "scan-fct_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 47:
#line 657 "scan-fct_pddl.y" /* yacc.c:1646  */
    {
  (yyval.pdis_PlNode) = (yyvsp[0].pdis_PlNode);
}
#line 1946 "scan-fct_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 48:
#line 662 "scan-fct_pddl.y" /* yacc.c:1646  */
    {
  (yyval.pdis_PlNode) = dis_new_dis_PlNode( dis_COMP );
  (yyval.pdis_PlNode)->comp = EQ;

  (yyval.pdis_PlNode)->lh = dis_new_dis_Parsedis_ExpNode( FHEAD );
  (yyval.pdis_PlNode)->lh->atom = dis_new_dis_TokenList();
  (yyval.pdis_PlNode)->lh->atom->item = dis_new_dis_Token( strlen((yyvsp[-4].string))+1 );
  strcpy( (yyval.pdis_PlNode)->lh->atom->item, (yyvsp[-4].string) );
  (yyval.pdis_PlNode)->lh->atom->next = (yyvsp[-3].pdis_TokenList);

  (yyval.pdis_PlNode)->rh = dis_new_dis_Parsedis_ExpNode( NUMBER );
  (yyval.pdis_PlNode)->rh->atom = dis_new_dis_TokenList();
  (yyval.pdis_PlNode)->rh->atom->item = dis_new_dis_Token( strlen((yyvsp[-1].string))+1 );
  strcpy( (yyval.pdis_PlNode)->rh->atom->item, (yyvsp[-1].string) );
}
#line 1966 "scan-fct_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 49:
#line 679 "scan-fct_pddl.y" /* yacc.c:1646  */
    {
  (yyval.pdis_PlNode) = dis_new_dis_PlNode( dis_COMP );
  (yyval.pdis_PlNode)->comp = EQ;

  (yyval.pdis_PlNode)->lh = dis_new_dis_Parsedis_ExpNode( FHEAD );
  (yyval.pdis_PlNode)->lh->atom = dis_new_dis_TokenList();
  (yyval.pdis_PlNode)->lh->atom->item = dis_new_dis_Token( strlen((yyvsp[-2].string))+1 );
  strcpy( (yyval.pdis_PlNode)->lh->atom->item, (yyvsp[-2].string) );

  (yyval.pdis_PlNode)->rh = dis_new_dis_Parsedis_ExpNode( NUMBER );
  (yyval.pdis_PlNode)->rh->atom = dis_new_dis_TokenList();
  (yyval.pdis_PlNode)->rh->atom->item = dis_new_dis_Token( strlen((yyvsp[-1].string))+1 );
  strcpy( (yyval.pdis_PlNode)->rh->atom->item, (yyvsp[-1].string) );
}
#line 1985 "scan-fct_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 50:
#line 696 "scan-fct_pddl.y" /* yacc.c:1646  */
    {
	int n;
	dis_TimedInitial *til;
	char name[32];
	GpG.is_til = dis_TRUE;
	if (dis_gnum_tils%128 == 0)
	{
		n = (dis_gnum_tils + 128)/128*128;
		dis_gtils = (dis_TimedInitial *) realloc(dis_gtils, sizeof(dis_TimedInitial)*n); 
	}
	til = &dis_gtils[dis_gnum_tils++];
	sscanf((yyvsp[-2].string), "%s %f", name, &(til->time));
	if ((yyvsp[-1].pdis_PlNode)->connective == dis_NOT)
	{
		til->negated = dis_TRUE;
		til->literal = (yyvsp[-1].pdis_PlNode)->sons;
	}
	else
	{
		til->negated = dis_FALSE;
		til->literal = (yyvsp[-1].pdis_PlNode);
	}
	(yyval.pdis_PlNode) = dis_new_dis_PlNode(dis_TRU);

	sprintf(name, "timed-initial-literals-%d", dis_gnum_tils);
	scur_op = dis_new_dis_Pldis_Operator(name);
	scur_op->effects = dis_new_dis_PlNode(dis_AND);
    scur_op->effects->parse_vars = NULL;
    scur_op->effects->sons = (yyvsp[-1].pdis_PlNode);

	scur_op->next = dis_gloaded_ops;
	dis_gloaded_ops = scur_op;
	scur_op = NULL;
}
#line 2024 "scan-fct_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 51:
#line 743 "scan-fct_pddl.y" /* yacc.c:1646  */
    { 
  (yyval.pdis_Parsedis_ExpNode) = dis_new_dis_Parsedis_ExpNode( NUMBER );
  (yyval.pdis_Parsedis_ExpNode)->atom = dis_new_dis_TokenList();
  (yyval.pdis_Parsedis_ExpNode)->atom->item = dis_new_dis_Token( strlen((yyvsp[0].string))+1 );
  strcpy( (yyval.pdis_Parsedis_ExpNode)->atom->item, (yyvsp[0].string) );
}
#line 2035 "scan-fct_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 52:
#line 751 "scan-fct_pddl.y" /* yacc.c:1646  */
    {
  (yyval.pdis_Parsedis_ExpNode) = dis_new_dis_Parsedis_ExpNode( FHEAD );
  (yyval.pdis_Parsedis_ExpNode)->atom = dis_new_dis_TokenList();
  (yyval.pdis_Parsedis_ExpNode)->atom->item = dis_new_dis_Token( strlen((yyvsp[-2].string))+1 );
  strcpy( (yyval.pdis_Parsedis_ExpNode)->atom->item, (yyvsp[-2].string) );
  (yyval.pdis_Parsedis_ExpNode)->atom->next = (yyvsp[-1].pdis_TokenList);
}
#line 2047 "scan-fct_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 53:
#line 760 "scan-fct_pddl.y" /* yacc.c:1646  */
    {
  (yyval.pdis_Parsedis_ExpNode) = dis_new_dis_Parsedis_ExpNode( MINUS );
  (yyval.pdis_Parsedis_ExpNode)->leftson = (yyvsp[-1].pdis_Parsedis_ExpNode);
}
#line 2056 "scan-fct_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 54:
#line 766 "scan-fct_pddl.y" /* yacc.c:1646  */
    {
  (yyval.pdis_Parsedis_ExpNode) = dis_new_dis_Parsedis_ExpNode( AD );
  (yyval.pdis_Parsedis_ExpNode)->leftson = (yyvsp[-2].pdis_Parsedis_ExpNode);
  (yyval.pdis_Parsedis_ExpNode)->rightson = (yyvsp[-1].pdis_Parsedis_ExpNode);
}
#line 2066 "scan-fct_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 55:
#line 773 "scan-fct_pddl.y" /* yacc.c:1646  */
    {
  (yyval.pdis_Parsedis_ExpNode) = dis_new_dis_Parsedis_ExpNode( SU );
  (yyval.pdis_Parsedis_ExpNode)->leftson = (yyvsp[-2].pdis_Parsedis_ExpNode);
  (yyval.pdis_Parsedis_ExpNode)->rightson = (yyvsp[-1].pdis_Parsedis_ExpNode);
}
#line 2076 "scan-fct_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 56:
#line 780 "scan-fct_pddl.y" /* yacc.c:1646  */
    {
  (yyval.pdis_Parsedis_ExpNode) = dis_new_dis_Parsedis_ExpNode( MU );
  (yyval.pdis_Parsedis_ExpNode)->leftson = (yyvsp[-2].pdis_Parsedis_ExpNode);
  (yyval.pdis_Parsedis_ExpNode)->rightson = (yyvsp[-1].pdis_Parsedis_ExpNode);
}
#line 2086 "scan-fct_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 57:
#line 787 "scan-fct_pddl.y" /* yacc.c:1646  */
    {
  (yyval.pdis_Parsedis_ExpNode) = dis_new_dis_Parsedis_ExpNode( DI );
  (yyval.pdis_Parsedis_ExpNode)->leftson = (yyvsp[-2].pdis_Parsedis_ExpNode);
  (yyval.pdis_Parsedis_ExpNode)->rightson = (yyvsp[-1].pdis_Parsedis_ExpNode);
}
#line 2096 "scan-fct_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 58:
#line 798 "scan-fct_pddl.y" /* yacc.c:1646  */
    { 
  (yyval.pdis_Parsedis_ExpNode) = dis_new_dis_Parsedis_ExpNode( NUMBER );
  (yyval.pdis_Parsedis_ExpNode)->atom = dis_new_dis_TokenList();
  (yyval.pdis_Parsedis_ExpNode)->atom->item = dis_new_dis_Token( strlen((yyvsp[0].string))+1 );
  strcpy( (yyval.pdis_Parsedis_ExpNode)->atom->item, (yyvsp[0].string) );
}
#line 2107 "scan-fct_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 59:
#line 806 "scan-fct_pddl.y" /* yacc.c:1646  */
    {
  (yyval.pdis_Parsedis_ExpNode) = dis_new_dis_Parsedis_ExpNode( FHEAD );
  (yyval.pdis_Parsedis_ExpNode)->atom = dis_new_dis_TokenList();
  (yyval.pdis_Parsedis_ExpNode)->atom->item = dis_new_dis_Token( strlen((yyvsp[-2].string))+1 );
  strcpy( (yyval.pdis_Parsedis_ExpNode)->atom->item, (yyvsp[-2].string) );
  (yyval.pdis_Parsedis_ExpNode)->atom->next = (yyvsp[-1].pdis_TokenList);
}
#line 2119 "scan-fct_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 60:
#line 815 "scan-fct_pddl.y" /* yacc.c:1646  */
    {
  (yyval.pdis_Parsedis_ExpNode) = dis_new_dis_Parsedis_ExpNode( MINUS );
  (yyval.pdis_Parsedis_ExpNode)->leftson = (yyvsp[-1].pdis_Parsedis_ExpNode);
}
#line 2128 "scan-fct_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 61:
#line 821 "scan-fct_pddl.y" /* yacc.c:1646  */
    { 
  (yyval.pdis_Parsedis_ExpNode) = dis_new_dis_Parsedis_ExpNode( AD );
  (yyval.pdis_Parsedis_ExpNode)->leftson = (yyvsp[-2].pdis_Parsedis_ExpNode);
  (yyval.pdis_Parsedis_ExpNode)->rightson = (yyvsp[-1].pdis_Parsedis_ExpNode);
}
#line 2138 "scan-fct_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 62:
#line 828 "scan-fct_pddl.y" /* yacc.c:1646  */
    { 
  (yyval.pdis_Parsedis_ExpNode) = dis_new_dis_Parsedis_ExpNode( MU );
  (yyval.pdis_Parsedis_ExpNode)->leftson = (yyvsp[-2].pdis_Parsedis_ExpNode);
  (yyval.pdis_Parsedis_ExpNode)->rightson = (yyvsp[-1].pdis_Parsedis_ExpNode);
}
#line 2148 "scan-fct_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 63:
#line 835 "scan-fct_pddl.y" /* yacc.c:1646  */
    {
  (yyval.pdis_Parsedis_ExpNode) = dis_new_dis_Parsedis_ExpNode( SU );
  (yyval.pdis_Parsedis_ExpNode)->leftson = (yyvsp[-2].pdis_Parsedis_ExpNode);
  (yyval.pdis_Parsedis_ExpNode)->rightson = (yyvsp[-1].pdis_Parsedis_ExpNode);
}
#line 2158 "scan-fct_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 64:
#line 842 "scan-fct_pddl.y" /* yacc.c:1646  */
    {
  (yyval.pdis_Parsedis_ExpNode) = dis_new_dis_Parsedis_ExpNode( DI );
  (yyval.pdis_Parsedis_ExpNode)->leftson = (yyvsp[-2].pdis_Parsedis_ExpNode);
  (yyval.pdis_Parsedis_ExpNode)->rightson = (yyvsp[-1].pdis_Parsedis_ExpNode);
}
#line 2168 "scan-fct_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 65:
#line 849 "scan-fct_pddl.y" /* yacc.c:1646  */
    {
  (yyval.pdis_Parsedis_ExpNode) = dis_new_dis_Parsedis_ExpNode(VIO);
  (yyval.pdis_Parsedis_ExpNode)->atom = dis_new_dis_TokenList();
  (yyval.pdis_Parsedis_ExpNode)->atom->item = dis_new_dis_Token(strlen((yyvsp[-1].string)) + 1);
  strcpy((yyval.pdis_Parsedis_ExpNode)->atom->item, (yyvsp[-1].string));
}
#line 2179 "scan-fct_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 66:
#line 859 "scan-fct_pddl.y" /* yacc.c:1646  */
    {
  (yyval.pdis_Parsedis_ExpNode) = (yyvsp[0].pdis_Parsedis_ExpNode);
}
#line 2187 "scan-fct_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 67:
#line 864 "scan-fct_pddl.y" /* yacc.c:1646  */
    {
  (yyval.pdis_Parsedis_ExpNode) = dis_new_dis_Parsedis_ExpNode( AD );
  (yyval.pdis_Parsedis_ExpNode)->leftson = (yyvsp[-1].pdis_Parsedis_ExpNode);
  (yyval.pdis_Parsedis_ExpNode)->rightson = (yyvsp[0].pdis_Parsedis_ExpNode);
}
#line 2197 "scan-fct_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 68:
#line 872 "scan-fct_pddl.y" /* yacc.c:1646  */
    {
  (yyval.pdis_Parsedis_ExpNode) = (yyvsp[0].pdis_Parsedis_ExpNode);
}
#line 2205 "scan-fct_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 69:
#line 877 "scan-fct_pddl.y" /* yacc.c:1646  */
    {
  (yyval.pdis_Parsedis_ExpNode) = dis_new_dis_Parsedis_ExpNode( MU );
  (yyval.pdis_Parsedis_ExpNode)->leftson = (yyvsp[-1].pdis_Parsedis_ExpNode);
  (yyval.pdis_Parsedis_ExpNode)->rightson = (yyvsp[0].pdis_Parsedis_ExpNode);
}
#line 2215 "scan-fct_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 70:
#line 886 "scan-fct_pddl.y" /* yacc.c:1646  */
    { 
  (yyval.pdis_TokenList) = (yyvsp[-1].pdis_TokenList);
  sis_negated = dis_TRUE;
}
#line 2224 "scan-fct_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 71:
#line 892 "scan-fct_pddl.y" /* yacc.c:1646  */
    {
  (yyval.pdis_TokenList) = (yyvsp[0].pdis_TokenList);
}
#line 2232 "scan-fct_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 72:
#line 901 "scan-fct_pddl.y" /* yacc.c:1646  */
    { 
  (yyval.pdis_TokenList) = dis_new_dis_TokenList();
  (yyval.pdis_TokenList)->item = (yyvsp[-2].pstring);
  (yyval.pdis_TokenList)->next = (yyvsp[-1].pdis_TokenList);
}
#line 2242 "scan-fct_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 73:
#line 908 "scan-fct_pddl.y" /* yacc.c:1646  */
    {
  (yyval.pdis_TokenList) = dis_new_dis_TokenList();
  (yyval.pdis_TokenList)->item = dis_new_dis_Token( 5 );
  (yyval.pdis_TokenList)->item = "=";
  (yyval.pdis_TokenList)->next = (yyvsp[-1].pdis_TokenList);
}
#line 2253 "scan-fct_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 74:
#line 920 "scan-fct_pddl.y" /* yacc.c:1646  */
    {
  (yyval.pdis_TokenList) = NULL;
}
#line 2261 "scan-fct_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 75:
#line 925 "scan-fct_pddl.y" /* yacc.c:1646  */
    {
  (yyval.pdis_TokenList) = dis_new_dis_TokenList();
  (yyval.pdis_TokenList)->item = (yyvsp[-1].pstring);
  (yyval.pdis_TokenList)->next = (yyvsp[0].pdis_TokenList);
}
#line 2271 "scan-fct_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 76:
#line 936 "scan-fct_pddl.y" /* yacc.c:1646  */
    { 
  (yyval.pstring) = dis_new_dis_Token(strlen((yyvsp[0].string)) + 1);
  strcpy((yyval.pstring), (yyvsp[0].string));
}
#line 2280 "scan-fct_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 77:
#line 942 "scan-fct_pddl.y" /* yacc.c:1646  */
    { 
  (yyval.pstring) = dis_new_dis_Token(strlen((yyvsp[0].string)) + 1);
  strcpy((yyval.pstring), (yyvsp[0].string));
}
#line 2289 "scan-fct_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 78:
#line 952 "scan-fct_pddl.y" /* yacc.c:1646  */
    {
  (yyval.pdis_TokenList) = dis_new_dis_TokenList();
  (yyval.pdis_TokenList)->item = dis_new_dis_Token(strlen((yyvsp[0].string)) + 1);
  strcpy((yyval.pdis_TokenList)->item, (yyvsp[0].string));
}
#line 2299 "scan-fct_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 79:
#line 959 "scan-fct_pddl.y" /* yacc.c:1646  */
    {
  (yyval.pdis_TokenList) = dis_new_dis_TokenList();
  (yyval.pdis_TokenList)->item = dis_new_dis_Token(strlen((yyvsp[-1].string)) + 1);
  strcpy((yyval.pdis_TokenList)->item, (yyvsp[-1].string));
  (yyval.pdis_TokenList)->next = (yyvsp[0].pdis_TokenList);
}
#line 2310 "scan-fct_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 80:
#line 970 "scan-fct_pddl.y" /* yacc.c:1646  */
    { (yyval.pdis_TypedList) = NULL; }
#line 2316 "scan-fct_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 81:
#line 973 "scan-fct_pddl.y" /* yacc.c:1646  */
    { 
  (yyval.pdis_TypedList) = dis_new_dis_TypedList();
  (yyval.pdis_TypedList)->name = dis_new_dis_Token( strlen((yyvsp[-4].string))+1 );
  strcpy( (yyval.pdis_TypedList)->name, (yyvsp[-4].string) );
  (yyval.pdis_TypedList)->type = (yyvsp[-2].pdis_TokenList);
  (yyval.pdis_TypedList)->next = (yyvsp[0].pdis_TypedList);
}
#line 2328 "scan-fct_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 82:
#line 982 "scan-fct_pddl.y" /* yacc.c:1646  */
    {
  (yyval.pdis_TypedList) = dis_new_dis_TypedList();
  (yyval.pdis_TypedList)->name = dis_new_dis_Token( strlen((yyvsp[-3].string))+1 );
  strcpy( (yyval.pdis_TypedList)->name, (yyvsp[-3].string) );
  (yyval.pdis_TypedList)->type = dis_new_dis_TokenList();
  (yyval.pdis_TypedList)->type->item = dis_new_dis_Token( strlen((yyvsp[-1].string))+1 );
  strcpy( (yyval.pdis_TypedList)->type->item, (yyvsp[-1].string) );
  (yyval.pdis_TypedList)->next = (yyvsp[0].pdis_TypedList);
}
#line 2342 "scan-fct_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 83:
#line 993 "scan-fct_pddl.y" /* yacc.c:1646  */
    {
  (yyval.pdis_TypedList) = dis_new_dis_TypedList();
  (yyval.pdis_TypedList)->name = dis_new_dis_Token( strlen((yyvsp[-1].string))+1 );
  strcpy( (yyval.pdis_TypedList)->name, (yyvsp[-1].string) );
  if ( (yyvsp[0].pdis_TypedList) ) {/* another element (already typed) is following */
    (yyval.pdis_TypedList)->type = dis_copy_dis_TokenList( (yyvsp[0].pdis_TypedList)->type );
  } else {/* no further element - it must be an untyped list */
    (yyval.pdis_TypedList)->type = dis_new_dis_TokenList();
    (yyval.pdis_TypedList)->type->item = dis_new_dis_Token( strlen(dis_STdis_ANDARD_TYPE)+1 );
    strcpy( (yyval.pdis_TypedList)->type->item, dis_STdis_ANDARD_TYPE );
  }
  (yyval.pdis_TypedList)->next = (yyvsp[0].pdis_TypedList);
}
#line 2360 "scan-fct_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 84:
#line 1012 "scan-fct_pddl.y" /* yacc.c:1646  */
    { (yyval.pdis_TypedList) = NULL; }
#line 2366 "scan-fct_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 85:
#line 1015 "scan-fct_pddl.y" /* yacc.c:1646  */
    { 
  (yyval.pdis_TypedList) = dis_new_dis_TypedList();
  (yyval.pdis_TypedList)->name = dis_new_dis_Token( strlen((yyvsp[-4].string))+1 );
  strcpy( (yyval.pdis_TypedList)->name, (yyvsp[-4].string) );
  (yyval.pdis_TypedList)->type = (yyvsp[-2].pdis_TokenList);
  (yyval.pdis_TypedList)->next = (yyvsp[0].pdis_TypedList);
}
#line 2378 "scan-fct_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 86:
#line 1024 "scan-fct_pddl.y" /* yacc.c:1646  */
    {
  (yyval.pdis_TypedList) = dis_new_dis_TypedList();
  (yyval.pdis_TypedList)->name = dis_new_dis_Token( strlen((yyvsp[-3].string))+1 );
  strcpy( (yyval.pdis_TypedList)->name, (yyvsp[-3].string) );
  (yyval.pdis_TypedList)->type = dis_new_dis_TokenList();
  (yyval.pdis_TypedList)->type->item = dis_new_dis_Token( strlen((yyvsp[-1].string))+1 );
  strcpy( (yyval.pdis_TypedList)->type->item, (yyvsp[-1].string) );
  (yyval.pdis_TypedList)->next = (yyvsp[0].pdis_TypedList);
}
#line 2392 "scan-fct_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 87:
#line 1035 "scan-fct_pddl.y" /* yacc.c:1646  */
    {
  (yyval.pdis_TypedList) = dis_new_dis_TypedList();
  (yyval.pdis_TypedList)->name = dis_new_dis_Token( strlen((yyvsp[-1].string))+1 );
  strcpy( (yyval.pdis_TypedList)->name, (yyvsp[-1].string) );
  if ( (yyvsp[0].pdis_TypedList) ) {/* another element (already typed) is following */
    (yyval.pdis_TypedList)->type = dis_copy_dis_TokenList( (yyvsp[0].pdis_TypedList)->type );
  } else {/* no further element - it must be an untyped list */
    (yyval.pdis_TypedList)->type = dis_new_dis_TokenList();
    (yyval.pdis_TypedList)->type->item = dis_new_dis_Token( strlen(dis_STdis_ANDARD_TYPE)+1 );
    strcpy( (yyval.pdis_TypedList)->type->item, dis_STdis_ANDARD_TYPE );
  }
  (yyval.pdis_TypedList)->next = (yyvsp[0].pdis_TypedList);
}
#line 2410 "scan-fct_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 88:
#line 1056 "scan-fct_pddl.y" /* yacc.c:1646  */
    { 
  (yyval.pstring) = dis_new_dis_Token(strlen((yyvsp[0].string)) + 1);
  strcpy((yyval.pstring), (yyvsp[0].string));
}
#line 2419 "scan-fct_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 89:
#line 1081 "scan-fct_pddl.y" /* yacc.c:1646  */
    { 
  dis_PlNode *tmp;

  tmp = dis_new_dis_PlNode(dis_ATOM);
  tmp->atom = (yyvsp[-1].pdis_TokenList);
  (yyval.pdis_PlNode) = dis_new_dis_PlNode(dis_NOT);
  (yyval.pdis_PlNode)->sons = tmp;
}
#line 2432 "scan-fct_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 90:
#line 1091 "scan-fct_pddl.y" /* yacc.c:1646  */
    {
  (yyval.pdis_PlNode) = dis_new_dis_PlNode(dis_ATOM);
  (yyval.pdis_PlNode)->atom = (yyvsp[0].pdis_TokenList);
}
#line 2441 "scan-fct_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 91:
#line 1101 "scan-fct_pddl.y" /* yacc.c:1646  */
    { 
  (yyval.pdis_TokenList) = dis_new_dis_TokenList();
  (yyval.pdis_TokenList)->item = (yyvsp[-2].pstring);
  (yyval.pdis_TokenList)->next = (yyvsp[-1].pdis_TokenList);
}
#line 2451 "scan-fct_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 92:
#line 1112 "scan-fct_pddl.y" /* yacc.c:1646  */
    { (yyval.pdis_TokenList) = NULL; }
#line 2457 "scan-fct_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 93:
#line 1115 "scan-fct_pddl.y" /* yacc.c:1646  */
    {
  (yyval.pdis_TokenList) = dis_new_dis_TokenList();
  (yyval.pdis_TokenList)->item = dis_new_dis_Token(strlen((yyvsp[-1].string)) + 1);
  strcpy((yyval.pdis_TokenList)->item, (yyvsp[-1].string));
  (yyval.pdis_TokenList)->next = (yyvsp[0].pdis_TokenList);
}
#line 2468 "scan-fct_pddl.tab.c" /* yacc.c:1646  */
    break;


#line 2472 "scan-fct_pddl.tab.c" /* yacc.c:1646  */
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
#line 1124 "scan-fct_pddl.y" /* yacc.c:1906  */



#include "lex.dis_fct_pddl.c"


/**********************************************************************
 * Functions
 **********************************************************************/


/* 
 * call	bison -pfct -bscan-fct scan-fct.y
 */



int yyerror( char *msg )

{
  fflush( stdout );
  fprintf(stderr,"\n%s: syntax error in line %d, '%s':\n", 
	  dis_gact_filename, dis_lineno, dis_fct_pddltext );

  if ( sact_err_par ) {
    fprintf(stderr, "%s%s\n", serrmsg[sact_err], sact_err_par );
  } else {
    fprintf(stderr,"%s\n", serrmsg[sact_err] );
  }

  exit( 1 );

}



void dis_load_fct_file( char *filename ) 

{

  FILE *fp;/* pointer to input files */
  char tmp[dis_MAX_LENGTH] = "";

  /* open fact file 
   */
  if( ( fp = fopen( filename, "r" ) ) == NULL ) {
    sprintf(tmp, "\nff: can't find fact file: %s\n\n", filename );
    perror(tmp);
    exit ( 1 );
  }

  dis_gact_filename = filename;
  dis_lineno = 1; 
  dis_fct_pddlin = fp;

  yyparse();

  fclose( fp );/* and close file again */

}

