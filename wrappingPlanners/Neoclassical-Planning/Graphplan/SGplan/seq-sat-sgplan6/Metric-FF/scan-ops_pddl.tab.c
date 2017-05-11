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
#define yyparse         dis_ops_pddlparse
#define yylex           dis_ops_pddllex
#define yyerror         dis_ops_pddlerror
#define yydebug         dis_ops_pddldebug
#define yynerrs         dis_ops_pddlnerrs

#define yylval          dis_ops_pddllval
#define yychar          dis_ops_pddlchar

/* Copy the first part of user declarations.  */
#line 49 "scan-ops_pddl.y" /* yacc.c:339  */

//#define YYDEBUG 1
#ifdef YYDEBUG
  extern int yydebug=1;
#endif


#include <stdio.h>
#include <string.h> 
#include "dis_ff.h"
#include "dis_memory.h"
#include "dis_parse.h"
#include "lpg.h"
#include "dis_constraints.h"

#ifndef SCAN_ERR
#define SCAN_ERR
#define DOMDEF_dis_EXPECTED            0
#define DOMAIN_dis_EXPECTED            1
#define DOMNAME_dis_EXPECTED           2
#define LBRACKET_dis_EXPECTED          3
#define RBRACKET_dis_EXPECTED          4
#define DOMDEFS_dis_EXPECTED           5
#define REQUIREM_dis_EXPECTED          6
#define TYPEDLIST_dis_EXPECTED         7
#define LITERAL_dis_EXPECTED           8
#define PRECONDDEF_UNCdis_ORRECT       9
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
#define ACTION                    20
#define CONSTRAINTLIST_EXPECTED	  21
#endif


#define dis_NAME_STR "name\0"
#define dis_VARIABLE_STR "variable\0"
#define dis_STdis_ANDARD_TYPE "OBJECT\0"
 

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
  "requirement %s not dis_supported by this FF version",  
  "action definition is not correct",
  "constraint definition is not correct",
  NULL
};


void dis_opserr( int errno, char *par );


static int sact_err;
static char *sact_err_par = NULL;
static dis_Pldis_Operator *scur_op = NULL, *scur_op1 = NULL;
static dis_Bool sis_negated = dis_FALSE;
static char *preference_pointer = NULL;
static char *preference_pointer1 = NULL;
static dis_TypedList *forall_tl = NULL;
static dis_Parsedis_ExpNode *duration_exp = NULL;

void dis_opserr( int errno, char *par )

{

  sact_err = errno;

  if ( sact_err_par ) {
    free(sact_err_par);
  }
  if ( par ) {
    sact_err_par = dis_new_dis_Token(strlen(par)+1);
    strcpy(sact_err_par, par);
  } else {
    sact_err_par = NULL;
  }

}

int dis_supported( char *str )

{

  int i;
	/* derived predicates */
	/* timed initial literals */
  char * sup[] = { ":STRIPS", ":TYPING", ":NEGATIVE-PRECONDITIONS",
		   ":DISJUNCTIVE-PRECONDITIONS", ":EQUALITY", 
		   ":EXISTENTIAL-PRECONDITIONS", ":UNIVERSAL-PRECONDITIONS", 
		   ":QUANTIFIED-PRECONDITIONS", ":CONDITIONAL-EFFECTS", 
		   ":FLUENTS", ":ADL", ":DURATIVE-ACTIONS", ":DURATION-INEQUALITIES", 
		   ":DERIVED-PREDICATES", ":TIMED-INITIAL-LITERALS", /* PDDL2.2 */
		   ":PREFERENCES", 
//		   ":CONSTRAINTS", /* PDDL3.0 */
		   ":NUMERIC-FLUENTS", ":ACTION-COSTS", ":GOAL-UTILITIES", /* PDDL3.1 */
//		   ":OBJECT-FLUENTS", 
		   NULL};    

  for (i=0; NULL != sup[i]; i++) {
    if ( SAME == strcmp(sup[i], str) ) {
	switch(i)
	{
	case 19:
		GpG.is_goal_utilities = TRUE;
		break;
	case 18:
		GpG.is_action_costs = TRUE;
		break;
	}
      return dis_TRUE;
    }
  }
  return dis_FALSE;

}


#line 217 "scan-ops_pddl.tab.c" /* yacc.c:339  */

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
extern int dis_ops_pddldebug;
#endif

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    DEFINE_TOK = 258,
    DOMAIN_TOK = 259,
    REQUIREMENTS_TOK = 260,
    TYPES_TOK = 261,
    EITHER_TOK = 262,
    CONSTANTS_TOK = 263,
    PREDICATES_TOK = 264,
    FUNCTIONS_TOK = 265,
    ACTION_TOK = 266,
    VARS_TOK = 267,
    IMPLIES_TOK = 268,
    PRECONDITION_TOK = 269,
    PARAMETERS_TOK = 270,
    EFFECT_TOK = 271,
    dis_AND_TOK = 272,
    dis_NOT_TOK = 273,
    dis_WHEN_TOK = 274,
    Fdis_ORdis_ALL_TOK = 275,
    IMPLY_TOK = 276,
    dis_OR_TOK = 277,
    dis_EXISTS_TOK = 278,
    LE_TOK = 279,
    LEQ_TOK = 280,
    EQ_TOK = 281,
    GEQ_TOK = 282,
    GE_TOK = 283,
    MINUS_TOK = 284,
    AD_TOK = 285,
    MU_TOK = 286,
    DI_TOK = 287,
    ASSIGN_TOK = 288,
    SCALE_UP_TOK = 289,
    SCALE_DOWN_TOK = 290,
    INCREASE_TOK = 291,
    DECREASE_TOK = 292,
    NAME = 293,
    VARIABLE = 294,
    NUM = 295,
    OPEN_PAREN = 296,
    CLOSE_PAREN = 297,
    AT_START = 298,
    AT_END = 299,
    OVER_ALL = 300,
    DURATION_TOK = 301,
    DURATIVE_ACTION_TOK = 302,
    DURATION_VAR_TOK = 303,
    CONDITION_TOK = 304,
    TIME_TOK = 305,
    DERIVED_TOK = 306,
    PLUS_TOK = 307,
    MUL_TOK = 308,
    DIV_TOK = 309,
    UMINUS = 310
  };
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED

union YYSTYPE
{
#line 196 "scan-ops_pddl.y" /* yacc.c:355  */


  char string[dis_MAX_LENGTH];
  char *pstring;
  dis_Parsedis_ExpNode *pdis_Parsedis_ExpNode;
  dis_PlNode *pdis_PlNode;
  dis_FactList *pdis_FactList;
  dis_TokenList *pdis_TokenList;
  dis_TypedList *pdis_TypedList;
  /* PDDL3.1 */
  dis_TypedListList *pdis_TypedListList;  

#line 323 "scan-ops_pddl.tab.c" /* yacc.c:355  */
};

typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE dis_ops_pddllval;

int dis_ops_pddlparse (void);



/* Copy the second part of user declarations.  */

#line 340 "scan-ops_pddl.tab.c" /* yacc.c:358  */

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
#define YYLAST   420

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  56
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  68
/* YYNRULES -- Number of rules.  */
#define YYNRULES  158
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  418

/* YYTRANSLATE[YYX] -- Symbol number corresponding to YYX as returned
   by yylex, with out-of-bounds checking.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   310

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
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,   306,   306,   306,   317,   316,   330,   339,   341,   343,
     345,   347,   349,   351,   354,   357,   364,   363,   373,   376,
     375,   404,   403,   414,   435,   452,   456,   464,   476,   480,
     475,   491,   495,   494,   508,   507,   521,   520,   535,   539,
     534,   559,   563,   576,   579,   600,   599,   606,   605,   613,
     618,   623,   629,   647,   651,   664,   673,   718,   723,   729,
     746,   755,   764,   773,   782,   791,   804,   810,   816,   826,
     845,   849,   861,   870,   879,   888,   898,   907,   920,   926,
     941,   961,   965,   977,   998,  1006,  1011,  1017,  1024,  1031,
    1038,  1049,  1055,  1064,  1071,  1083,  1085,  1096,  1102,  1112,
    1119,  1130,  1141,  1143,  1153,  1164,  1184,  1186,  1195,  1206,
    1225,  1229,  1224,  1263,  1268,  1262,  1278,  1283,  1288,  1294,
    1312,  1316,  1329,  1336,  1342,  1350,  1355,  1363,  1371,  1379,
    1389,  1400,  1404,  1410,  1416,  1429,  1445,  1454,  1463,  1472,
    1482,  1493,  1498,  1506,  1514,  1522,  1530,  1540,  1549,  1558,
    1567,  1577,  1588,  1595,  1602,  1609,  1616,  1622,  1627
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || 0
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "DEFINE_TOK", "DOMAIN_TOK",
  "REQUIREMENTS_TOK", "TYPES_TOK", "EITHER_TOK", "CONSTANTS_TOK",
  "PREDICATES_TOK", "FUNCTIONS_TOK", "ACTION_TOK", "VARS_TOK",
  "IMPLIES_TOK", "PRECONDITION_TOK", "PARAMETERS_TOK", "EFFECT_TOK",
  "dis_AND_TOK", "dis_NOT_TOK", "dis_WHEN_TOK", "Fdis_ORdis_ALL_TOK",
  "IMPLY_TOK", "dis_OR_TOK", "dis_EXISTS_TOK", "LE_TOK", "LEQ_TOK",
  "EQ_TOK", "GEQ_TOK", "GE_TOK", "MINUS_TOK", "AD_TOK", "MU_TOK", "DI_TOK",
  "ASSIGN_TOK", "SCALE_UP_TOK", "SCALE_DOWN_TOK", "INCREASE_TOK",
  "DECREASE_TOK", "NAME", "VARIABLE", "NUM", "OPEN_PAREN", "CLOSE_PAREN",
  "AT_START", "AT_END", "OVER_ALL", "DURATION_TOK", "DURATIVE_ACTION_TOK",
  "DURATION_VAR_TOK", "CONDITION_TOK", "TIME_TOK", "DERIVED_TOK",
  "PLUS_TOK", "MUL_TOK", "DIV_TOK", "UMINUS", "$accept", "file", "$@1",
  "domain_definition", "$@2", "domain_name", "optional_domain_defs",
  "predicates_def", "$@3", "predicates_list", "$@4", "functions_def",
  "$@5", "functions_list", "atomic_function_skeleton_plus", "require_def",
  "$@6", "$@7", "require_key_star", "$@8", "types_def", "$@9",
  "constants_def", "$@10", "action_def", "$@11", "$@12", "param_def",
  "action_def_body", "$@13", "$@14", "pre_GD", "pre_GD_star", "pref_GD",
  "derived_def", "adl_goal_description", "adl_goal_description1",
  "adl_goal_description_star", "adl_effect", "adl_effect_star", "f_head",
  "f_exp", "literal_term", "atomic_formula_term", "term_star", "term",
  "name_plus", "predicate", "typed_list_name", "typed_list_variable",
  "durative_action_def", "$@15", "$@16", "durative_action_def_body",
  "$@17", "$@18", "da_GD", "da_GD_star", "pref_timed_GD",
  "da_adl_goal_description", "timed_adl_goal_description_plus",
  "timed_adl_goal_description", "duration_constraint", "da_adl_effect",
  "timed_adl_effect_plus", "timed_adl_effect", "f_assign_da", "f_exp_da", YY_NULLPTR
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
     295,   296,   297,   298,   299,   300,   301,   302,   303,   304,
     305,   306,   307,   308,   309,   310
};
# endif

#define YYPACT_NINF -292

#define yypact_value_is_default(Yystate) \
  (!!((Yystate) == (-292)))

#define YYTABLE_NINF -1

#define yytable_value_is_error(Yytable_value) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
    -292,    23,     4,  -292,    64,  -292,    28,    69,  -292,    47,
      -3,    48,    21,  -292,  -292,    -3,    -3,    -3,    -3,    -3,
      -3,    -3,    -3,  -292,  -292,  -292,  -292,    57,    71,  -292,
    -292,    73,  -292,  -292,  -292,  -292,  -292,  -292,  -292,  -292,
      82,   103,   103,   117,  -292,   131,  -292,   141,   137,   138,
     140,  -292,    37,   112,   148,   153,   161,   153,   168,   175,
    -292,  -292,  -292,   153,   176,   179,   180,  -292,  -292,  -292,
      42,   177,  -292,   178,  -292,    71,   164,   164,   181,  -292,
     182,   179,   188,   103,   179,   194,  -292,  -292,    71,  -292,
     196,    89,   187,   197,   176,  -292,  -292,   103,  -292,   202,
     153,    57,  -292,   153,   198,   209,   210,   217,   212,   218,
     125,   219,  -292,  -292,  -292,  -292,  -292,   153,  -292,  -292,
     221,   153,   253,  -292,  -292,  -292,   266,  -292,  -292,  -292,
     226,  -292,  -292,   197,   197,   224,   197,   197,   228,     1,
       1,   158,     1,     1,    12,  -292,  -292,  -292,   225,   209,
     241,  -292,    89,   210,   246,   197,   247,    12,   248,   248,
     248,   248,   248,    89,   242,   244,   197,   252,   254,   255,
     153,   197,   256,   153,  -292,   156,  -292,     1,     1,  -292,
    -292,     1,   267,    12,     1,     1,   268,    89,   209,   269,
     153,  -292,   210,   270,     2,   255,   210,   153,   275,     1,
       1,     1,     1,     1,  -292,     1,   274,  -292,  -292,  -292,
    -292,   276,   277,  -292,   278,     1,     1,     1,     1,    12,
     286,   287,   304,  -292,  -292,   306,   307,  -292,  -292,  -292,
    -292,   308,  -292,  -292,   310,   311,   312,   313,   314,   315,
     316,   317,    79,  -292,  -292,  -292,   319,  -292,   197,    90,
       1,     1,     1,   320,  -292,  -292,  -292,  -292,  -292,   209,
    -292,   210,  -292,  -292,  -292,  -292,  -292,  -292,   274,   322,
    -292,   197,   197,   197,   300,   309,   324,   325,  -292,   326,
     327,   328,   329,  -292,   330,   331,   274,   332,   153,   333,
     334,   335,   337,  -292,  -292,  -292,  -292,  -292,  -292,  -292,
    -292,  -292,  -292,   338,  -292,  -292,  -292,   192,  -292,  -292,
     274,   340,   341,   342,   248,   248,   248,   248,   248,  -292,
     343,   343,   344,    95,   345,   340,    66,   340,  -292,   153,
      67,    67,    67,    67,    67,   288,   346,   347,   348,   349,
    -292,  -292,  -292,   351,   352,   353,   211,  -292,  -292,   354,
     355,   356,   357,   358,   248,   248,   248,   248,   248,  -292,
    -292,  -292,  -292,   116,   359,   351,  -292,   337,    67,    67,
      67,    67,  -292,  -292,  -292,  -292,  -292,    67,    67,    67,
      67,    67,  -292,  -292,   360,    90,    77,     1,    67,     1,
      67,     1,    67,   312,   361,   313,   362,   314,   363,   315,
     364,   316,   365,  -292,  -292,   366,   367,   368,   369,  -292,
    -292,  -292,  -292,  -292,  -292,  -292,  -292,  -292
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       2,     0,     0,     1,     0,     3,     0,     0,     4,     0,
       0,     0,     0,     7,     5,     0,     0,     0,     0,     0,
       0,     0,     0,     6,    28,    34,    36,    18,    25,    38,
     110,     0,    11,    12,     8,    10,     9,    13,    15,    14,
       0,   102,   102,     0,    16,     0,    21,    24,     0,     0,
       0,    29,   102,     0,     0,   106,     0,   106,     0,     0,
      39,   111,   101,   106,    31,     0,     0,   105,    35,    37,
     106,     0,    17,     0,    22,    25,    41,    41,     0,    32,
       0,    99,     0,   102,     0,     0,   109,    19,    27,    23,
       0,    43,     0,     0,    31,    30,   100,   102,   104,     0,
     106,    18,    26,   106,     0,     0,     0,     0,     0,     0,
       0,     0,    57,    65,    92,    33,   103,   106,   108,    20,
       0,   106,     0,    45,    50,    55,     0,    47,    77,    40,
       0,   113,   112,    70,     0,     0,     0,    70,     0,     0,
       0,    95,     0,     0,    95,    56,   107,    42,     0,    53,
       0,    49,    43,    81,     0,     0,     0,    95,     0,     0,
       0,     0,     0,    43,     0,     0,    70,     0,     0,     0,
     106,     0,     0,   106,    84,     0,    85,     0,     0,    97,
      98,     0,     0,    95,     0,     0,     0,    43,    53,     0,
     106,    46,    81,     0,     0,     0,     0,   106,     0,     0,
       0,     0,     0,     0,    48,     0,     0,    71,    58,    67,
      91,     0,     0,    66,     0,     0,     0,     0,     0,    95,
       0,     0,     0,    94,    96,     0,     0,    93,    44,    54,
      51,     0,    82,    78,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   114,   117,   122,     0,    68,     0,     0,
       0,     0,     0,     0,    60,    61,    62,    63,    64,     0,
      80,     0,    72,    73,    74,    75,    76,   130,   120,     0,
     116,     0,     0,     0,     0,     0,     0,     0,    86,     0,
       0,     0,     0,    83,     0,     0,   120,     0,   106,     0,
       0,     0,     0,    59,    69,    88,    87,    89,    90,    52,
      79,   121,   118,     0,   127,   128,   129,     0,   115,   133,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   131,
       0,     0,     0,     0,     0,   141,     0,     0,   123,   106,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     119,   132,   142,     0,     0,     0,     0,   157,   158,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   143,
     145,   144,   146,     0,     0,   125,   135,     0,     0,     0,
       0,     0,   136,   137,   138,   139,   140,     0,     0,     0,
       0,     0,   124,   126,     0,   158,     0,   158,     0,   158,
       0,   158,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   134,   156,     0,     0,     0,     0,   147,
     148,   149,   150,   151,   153,   152,   154,   155
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -292,  -292,  -292,  -292,  -292,  -292,   323,  -292,  -292,   216,
    -292,  -292,  -292,   289,   263,  -292,  -292,  -292,   271,  -292,
    -292,  -292,  -292,  -292,  -292,  -292,  -292,   284,  -128,  -292,
    -292,  -101,   191,  -292,  -292,   -71,  -102,  -132,   -99,   193,
    -150,  -123,  -105,   -97,  -130,  -292,   -48,   370,   -27,   -57,
    -292,  -292,  -292,  -292,  -292,  -292,  -204,   107,  -292,  -292,
      49,  -291,  -292,    45,    88,  -223,    94,  -197
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     1,     2,     5,    10,     8,    14,    15,    56,    44,
     101,    16,    58,    46,    47,    17,    40,    64,    80,    94,
      18,    41,    19,    42,    20,    48,    76,    91,   107,   152,
     163,   188,   189,   124,    21,   166,   112,   167,   192,   193,
     176,   348,   113,   114,   182,   183,    82,   144,    53,    71,
      22,    49,    77,   109,   165,   274,   286,   287,   244,   327,
     364,   245,   131,   308,   324,   309,   337,   349
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_uint16 yytable[] =
{
      73,   128,   243,   125,   123,   172,    78,   127,   199,   200,
     201,   202,   203,    86,   186,    54,   177,   178,   181,   184,
     185,   328,   111,     3,   191,    67,    24,    25,   157,    26,
      27,    28,    29,    96,   207,   204,    99,   169,    12,    13,
      62,   174,   175,   118,    65,     4,   120,   125,   128,    84,
     179,   180,   365,   224,   220,   221,    98,   195,   222,   228,
     146,   225,   226,   168,   148,   171,    66,     6,    30,     7,
     116,    85,    31,     9,   365,    52,   236,   237,   238,   239,
     240,    70,   241,   343,   196,    11,   125,   128,   325,   253,
      23,   128,   249,   250,   251,   252,   268,   234,    43,   269,
     212,   104,   325,   105,   344,   106,   322,   174,   346,   271,
     272,   273,    45,   211,    50,   347,   214,   174,   346,   404,
      51,   270,   271,   272,   273,   347,   279,   280,   281,   282,
     174,   175,   278,   231,   350,   351,   352,   353,   320,   321,
     235,    52,   133,   134,   276,   135,   136,   137,   138,   139,
     140,   141,   142,   143,    68,    55,   128,   125,   284,   271,
     272,   273,   285,    62,   330,   331,   332,   333,   334,    57,
      59,   386,   388,   390,   392,    60,    61,   277,    62,    90,
     394,   396,   398,   400,   402,   215,   216,   217,   218,   405,
      69,   406,    70,   407,   219,   408,   179,   180,   174,   175,
     289,   290,   291,    72,   377,   378,   379,   380,   381,   311,
      74,   312,   313,    75,    79,   128,   128,    81,    83,    87,
      88,   336,   338,    93,    95,   314,   315,   316,   317,   318,
      97,   303,   100,   108,   319,   320,   321,   103,   110,   121,
     368,   369,   370,   371,   117,   385,   387,   389,   391,   219,
     122,   126,   164,   130,   393,   395,   397,   399,   401,   129,
     132,   145,   279,   147,   280,   170,   281,   187,   282,   173,
     149,   134,   345,   150,   136,   137,   138,   139,   140,   141,
     142,   143,   190,   153,   154,   155,   156,   194,   197,   198,
     205,    62,   157,   206,   208,   151,   209,   210,   213,   158,
     159,   160,   161,   162,    62,   153,   154,   155,   156,   223,
     227,   230,   233,   219,   157,   242,   292,   119,   246,   247,
     248,   354,   355,   356,   357,   358,    62,   134,   254,   255,
     136,   137,   138,   139,   140,   141,   142,   143,    32,    33,
      34,    35,    36,    37,    38,    39,   256,    62,   257,   258,
     259,   102,   260,   261,   262,   263,   264,   265,   266,   267,
     275,    92,   283,   288,    89,   115,   293,   294,   295,   296,
     297,   298,   299,   300,   302,   304,   305,   306,   307,   229,
     310,   323,   326,   329,   335,   232,   340,   341,   359,   360,
     361,   362,   363,   301,   366,   367,   372,   373,   374,   375,
     376,   382,   403,   409,   410,   411,   412,   413,   414,   415,
     416,   417,   384,   342,   383,   339,     0,     0,     0,     0,
      63
};

static const yytype_int16 yycheck[] =
{
      57,   106,   206,   105,   105,   137,    63,   106,   158,   159,
     160,   161,   162,    70,   144,    42,   139,   140,   141,   142,
     143,   312,    93,     0,   152,    52,     5,     6,    26,     8,
       9,    10,    11,    81,   166,   163,    84,   134,    41,    42,
      38,    40,    41,   100,     7,    41,   103,   149,   153,     7,
      38,    39,   343,   183,   177,   178,    83,   154,   181,   187,
     117,   184,   185,   134,   121,   136,    29,     3,    47,    41,
      97,    29,    51,     4,   365,    38,   199,   200,   201,   202,
     203,    39,   205,    17,   155,    38,   188,   192,   311,   219,
      42,   196,   215,   216,   217,   218,    17,   196,    41,    20,
     171,    12,   325,    14,   327,    16,   310,    40,    41,    43,
      44,    45,    41,   170,    41,    48,   173,    40,    41,    42,
      38,    42,    43,    44,    45,    48,   249,   250,   251,   252,
      40,    41,    42,   190,   331,   332,   333,   334,    43,    44,
     197,    38,    17,    18,   246,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    42,    38,   261,   259,   259,    43,
      44,    45,   261,    38,   314,   315,   316,   317,   318,    38,
      29,   368,   369,   370,   371,    38,    38,   248,    38,    15,
     377,   378,   379,   380,   381,    29,    30,    31,    32,   386,
      42,   388,    39,   390,    38,   392,    38,    39,    40,    41,
     271,   272,   273,    42,   354,   355,   356,   357,   358,    17,
      42,    19,    20,    38,    38,   320,   321,    38,    38,    42,
      42,   320,   321,    42,    42,    33,    34,    35,    36,    37,
      42,   288,    38,    46,    42,    43,    44,    41,    41,    41,
      29,    30,    31,    32,    42,   368,   369,   370,   371,    38,
      41,    41,    26,    41,   377,   378,   379,   380,   381,    42,
      42,    42,   385,    42,   387,    41,   389,    42,   391,    41,
      17,    18,   329,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    41,    17,    18,    19,    20,    41,    41,    41,
      48,    38,    26,    49,    42,    42,    42,    42,    42,    33,
      34,    35,    36,    37,    38,    17,    18,    19,    20,    42,
      42,    42,    42,    38,    26,    41,    16,   101,    42,    42,
      42,    33,    34,    35,    36,    37,    38,    18,    42,    42,
      21,    22,    23,    24,    25,    26,    27,    28,    15,    16,
      17,    18,    19,    20,    21,    22,    42,    38,    42,    42,
      42,    88,    42,    42,    42,    42,    42,    42,    42,    42,
      41,    77,    42,    41,    75,    94,    42,    42,    42,    42,
      42,    42,    42,    42,    42,    42,    42,    42,    41,   188,
      42,    41,    41,    41,    41,   192,    42,    42,    42,    42,
      42,    42,    41,   286,    42,    42,    42,    42,    42,    42,
      42,    42,    42,    42,    42,    42,    42,    42,    42,    42,
      42,    42,   367,   325,   365,   321,    -1,    -1,    -1,    -1,
      50
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,    57,    58,     0,    41,    59,     3,    41,    61,     4,
      60,    38,    41,    42,    62,    63,    67,    71,    76,    78,
      80,    90,   106,    42,     5,     6,     8,     9,    10,    11,
      47,    51,    62,    62,    62,    62,    62,    62,    62,    62,
      72,    77,    79,    41,    65,    41,    69,    70,    81,   107,
      41,    38,    38,   104,   104,    38,    64,    38,    68,    29,
      38,    38,    38,   103,    73,     7,    29,   104,    42,    42,
      39,   105,    42,   105,    42,    38,    82,   108,   105,    38,
      74,    38,   102,    38,     7,    29,   105,    42,    42,    69,
      15,    83,    83,    42,    75,    42,   102,    42,   104,   102,
      38,    66,    70,    41,    12,    14,    16,    84,    46,   109,
      41,    91,    92,    98,    99,    74,   104,    42,   105,    65,
     105,    41,    41,    87,    89,    92,    41,    94,    98,    42,
      41,   118,    42,    17,    18,    20,    21,    22,    23,    24,
      25,    26,    27,    28,   103,    42,   105,    42,   105,    17,
      20,    42,    85,    17,    18,    19,    20,    26,    33,    34,
      35,    36,    37,    86,    26,   110,    91,    93,    91,    99,
      41,    91,    93,    41,    40,    41,    96,    97,    97,    38,
      39,    97,   100,   101,    97,    97,   100,    42,    87,    88,
      41,    84,    94,    95,    41,    99,    91,    41,    41,    96,
      96,    96,    96,    96,    84,    48,    49,    93,    42,    42,
      42,   105,    91,    42,   105,    29,    30,    31,    32,    38,
      97,    97,    97,    42,   100,    97,    97,    42,    84,    88,
      42,   105,    95,    42,    94,   105,    97,    97,    97,    97,
      97,    97,    41,   112,   114,   117,    42,    42,    42,    97,
      97,    97,    97,   100,    42,    42,    42,    42,    42,    42,
      42,    42,    42,    42,    42,    42,    42,    42,    17,    20,
      42,    43,    44,    45,   111,    41,    92,    91,    42,    97,
      97,    97,    97,    42,    87,    94,   112,   113,    41,    91,
      91,    91,    16,    42,    42,    42,    42,    42,    42,    42,
      42,   113,    42,   105,    42,    42,    42,    41,   119,   121,
      42,    17,    19,    20,    33,    34,    35,    36,    37,    42,
      43,    44,   112,    41,   120,   121,    41,   115,   117,    41,
      96,    96,    96,    96,    96,    41,    94,   122,    94,   122,
      42,    42,   120,    17,   121,   105,    41,    48,    97,   123,
     123,   123,   123,   123,    33,    34,    35,    36,    37,    42,
      42,    42,    42,    41,   116,   117,    42,    42,    29,    30,
      31,    32,    42,    42,    42,    42,    42,    96,    96,    96,
      96,    96,    42,   116,   119,    97,   123,    97,   123,    97,
     123,    97,   123,    97,   123,    97,   123,    97,   123,    97,
     123,    97,   123,    42,    42,   123,   123,   123,   123,    42,
      42,    42,    42,    42,    42,    42,    42,    42
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    56,    58,    57,    60,    59,    61,    62,    62,    62,
      62,    62,    62,    62,    62,    62,    64,    63,    65,    66,
      65,    68,    67,    69,    69,    69,    70,    70,    72,    73,
      71,    74,    75,    74,    77,    76,    79,    78,    81,    82,
      80,    83,    83,    84,    84,    85,    84,    86,    84,    87,
      87,    87,    87,    88,    88,    89,    90,    91,    91,    91,
      92,    92,    92,    92,    92,    92,    92,    92,    92,    92,
      93,    93,    94,    94,    94,    94,    94,    94,    94,    94,
      94,    95,    95,    96,    97,    97,    97,    97,    97,    97,
      97,    98,    98,    99,    99,   100,   100,   101,   101,   102,
     102,   103,   104,   104,   104,   104,   105,   105,   105,   105,
     107,   108,   106,   110,   111,   109,   112,   112,   112,   112,
     113,   113,   114,   115,   115,   116,   116,   117,   117,   117,
     118,   119,   119,   119,   119,   119,   119,   119,   119,   119,
     119,   120,   120,   121,   121,   121,   121,   122,   122,   122,
     122,   122,   123,   123,   123,   123,   123,   123,   123
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     0,     2,     0,     5,     4,     1,     2,     2,
       2,     2,     2,     2,     2,     2,     0,     5,     0,     0,
       6,     0,     5,     4,     1,     0,     5,     4,     0,     0,
       7,     0,     0,     3,     0,     5,     0,     5,     0,     0,
       8,     0,     4,     0,     5,     0,     4,     0,     4,     2,
       1,     4,     7,     0,     2,     1,     8,     1,     4,     7,
       5,     5,     5,     5,     5,     1,     4,     4,     5,     7,
       0,     2,     5,     5,     5,     5,     5,     1,     4,     7,
       5,     0,     2,     4,     1,     1,     4,     5,     5,     5,
       5,     4,     1,     4,     4,     0,     2,     1,     1,     1,
       2,     1,     0,     5,     4,     2,     0,     5,     4,     2,
       0,     0,     8,     0,     0,     8,     2,     1,     4,     7,
       0,     2,     1,     1,     4,     1,     2,     4,     4,     4,
       5,     2,     4,     1,     7,     5,     5,     5,     5,     5,
       5,     1,     2,     4,     4,     4,     4,     5,     5,     5,
       5,     5,     5,     5,     5,     5,     4,     1,     1
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
        case 2:
#line 306 "scan-ops_pddl.y" /* yacc.c:1646  */
    { 
  dis_opserr( DOMDEF_dis_EXPECTED, NULL ); 
}
#line 1707 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 4:
#line 317 "scan-ops_pddl.y" /* yacc.c:1646  */
    { 
}
#line 1714 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 5:
#line 320 "scan-ops_pddl.y" /* yacc.c:1646  */
    {
  if ( dis_gcmd_line.display_info >= 1 ) {
    /*printf("\ndomain '%s' defined\n", dis_gdomain_name);*/
  } 
}
#line 1724 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 6:
#line 331 "scan-ops_pddl.y" /* yacc.c:1646  */
    { 
  dis_gdomain_name = dis_copy_dis_Token((yyvsp[-1].string));
}
#line 1732 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 16:
#line 364 "scan-ops_pddl.y" /* yacc.c:1646  */
    {
}
#line 1739 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 17:
#line 367 "scan-ops_pddl.y" /* yacc.c:1646  */
    { 
}
#line 1746 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 18:
#line 373 "scan-ops_pddl.y" /* yacc.c:1646  */
    {}
#line 1752 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 19:
#line 376 "scan-ops_pddl.y" /* yacc.c:1646  */
    {

  dis_TypedListList *tll;

  if ( dis_gparse_predicates ) {
    tll = dis_gparse_predicates;
    while ( tll->next ) {
      tll = tll->next;
    }
    tll->next = dis_new_dis_TypedListList();
    tll = tll->next;
  } else {
    tll = dis_new_dis_TypedListList();
    dis_gparse_predicates = tll;
  }

  tll->predicate = dis_copy_dis_Token((yyvsp[-2].string));

  tll->args = (yyvsp[-1].pdis_TypedList);

}
#line 1778 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 21:
#line 404 "scan-ops_pddl.y" /* yacc.c:1646  */
    {
}
#line 1785 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 22:
#line 407 "scan-ops_pddl.y" /* yacc.c:1646  */
    { 
}
#line 1792 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 23:
#line 415 "scan-ops_pddl.y" /* yacc.c:1646  */
    {
  dis_TypedListList *tll;

  if (strcmp((yyvsp[-1].string), "NUMBER"))
    exit(1);
  else
  if ((yyvsp[-3].pdis_TypedListList)->next != NULL || strcmp((yyvsp[-3].pdis_TypedListList)->predicate, "TOTAL-COST")) // rule out action costs
    GpG.is_numeric_fluents = TRUE;
  
  if ( dis_gparse_functions ) 
  {
    tll = dis_gparse_functions;
    while (tll->next)
      tll = tll->next;
    tll->next = (yyvsp[-3].pdis_TypedListList);
  }
  else
    dis_gparse_functions = (yyvsp[-3].pdis_TypedListList);
}
#line 1816 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 24:
#line 436 "scan-ops_pddl.y" /* yacc.c:1646  */
    {
  dis_TypedListList *tll;
  
//  if ($1->next != NULL || strcmp($1->predicate, "TOTAL-COST")) // rule out action costs
    GpG.is_numeric_fluents = TRUE; // default type is NUMBER 
  if ( dis_gparse_functions ) 
  {
    tll = dis_gparse_functions;
    while (tll->next)
      tll = tll->next;
    tll->next = (yyvsp[0].pdis_TypedListList);
  }
  else
    dis_gparse_functions = (yyvsp[0].pdis_TypedListList);
}
#line 1836 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 25:
#line 452 "scan-ops_pddl.y" /* yacc.c:1646  */
    {}
#line 1842 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 26:
#line 457 "scan-ops_pddl.y" /* yacc.c:1646  */
    {
  (yyval.pdis_TypedListList) = dis_new_dis_TypedListList();
  (yyval.pdis_TypedListList)->predicate = dis_copy_dis_Token((yyvsp[-3].string));
  (yyval.pdis_TypedListList)->args = (yyvsp[-2].pdis_TypedList);
  (yyval.pdis_TypedListList)->next = (yyvsp[0].pdis_TypedListList);
}
#line 1853 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 27:
#line 465 "scan-ops_pddl.y" /* yacc.c:1646  */
    {
  (yyval.pdis_TypedListList) = dis_new_dis_TypedListList();
  (yyval.pdis_TypedListList)->predicate = dis_copy_dis_Token((yyvsp[-2].string));
  (yyval.pdis_TypedListList)->args = (yyvsp[-1].pdis_TypedList);
  (yyval.pdis_TypedListList)->next = NULL;
}
#line 1864 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 28:
#line 476 "scan-ops_pddl.y" /* yacc.c:1646  */
    { 
  dis_opserr( REQUIREM_dis_EXPECTED, NULL ); 
}
#line 1872 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 29:
#line 480 "scan-ops_pddl.y" /* yacc.c:1646  */
    { 
  if ( !dis_supported( (yyvsp[0].string) ) ) {
    dis_opserr( dis_NOT_SUPPdis_ORTED, (yyvsp[0].string) );
    yyerror();
  }
}
#line 1883 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 32:
#line 495 "scan-ops_pddl.y" /* yacc.c:1646  */
    { 
  if ( !dis_supported( (yyvsp[0].string) ) ) {
    dis_opserr( dis_NOT_SUPPdis_ORTED, (yyvsp[0].string) );
    yyerror();
  }
}
#line 1894 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 34:
#line 508 "scan-ops_pddl.y" /* yacc.c:1646  */
    { 
  dis_opserr( TYPEDEF_dis_EXPECTED, NULL ); 
}
#line 1902 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 35:
#line 512 "scan-ops_pddl.y" /* yacc.c:1646  */
    {
  dis_gparse_types = (yyvsp[-1].pdis_TypedList);
}
#line 1910 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 36:
#line 521 "scan-ops_pddl.y" /* yacc.c:1646  */
    { 
  dis_opserr( CONSTLIST_dis_EXPECTED, NULL ); 
}
#line 1918 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 37:
#line 525 "scan-ops_pddl.y" /* yacc.c:1646  */
    {
  dis_gparse_constants = (yyvsp[-1].pdis_TypedList);
}
#line 1926 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 38:
#line 535 "scan-ops_pddl.y" /* yacc.c:1646  */
    { 
  dis_opserr( ACTION, NULL ); 
}
#line 1934 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 39:
#line 539 "scan-ops_pddl.y" /* yacc.c:1646  */
    { 
  scur_op = dis_new_dis_Pldis_Operator( (yyvsp[0].string) );
  /* PDDL3 */  
  preference_pointer = NULL;
}
#line 1944 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 40:
#line 545 "scan-ops_pddl.y" /* yacc.c:1646  */
    {
  dis_PlNode *p;
  char temp[128]; 

  scur_op->next = dis_gloaded_ops;
  dis_gloaded_ops = scur_op; 
  scur_op = NULL;
}
#line 1957 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 41:
#line 559 "scan-ops_pddl.y" /* yacc.c:1646  */
    { 
  scur_op->params = NULL; 
}
#line 1965 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 42:
#line 564 "scan-ops_pddl.y" /* yacc.c:1646  */
    {
  dis_TypedList *tl;
  scur_op->parse_params = (yyvsp[-1].pdis_TypedList);
  for (tl = scur_op->parse_params; tl; tl = tl->next) {
    /* to be able to distinguish params from :VARS 
     */
    scur_op->number_of_real_params++;
  }
}
#line 1979 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 44:
#line 580 "scan-ops_pddl.y" /* yacc.c:1646  */
    {
  dis_TypedList *tl = NULL;

  /* add vars as parameters 
   */
  if ( scur_op->parse_params ) {
    for( tl = scur_op->parse_params; tl->next; tl = tl->next ) {
      /* empty, get to the end of list 
       */
    }
    tl->next = (yyvsp[-2].pdis_TypedList);
    tl = tl->next;
  } else {
    scur_op->parse_params = (yyvsp[-2].pdis_TypedList);
  }
}
#line 2000 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 45:
#line 600 "scan-ops_pddl.y" /* yacc.c:1646  */
    { 
  scur_op->preconds = (yyvsp[0].pdis_PlNode); 
}
#line 2008 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 47:
#line 606 "scan-ops_pddl.y" /* yacc.c:1646  */
    { 
  scur_op->effects = (yyvsp[0].pdis_PlNode); 
}
#line 2016 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 49:
#line 614 "scan-ops_pddl.y" /* yacc.c:1646  */
    {
        (yyval.pdis_PlNode) = NULL;
}
#line 2024 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 50:
#line 619 "scan-ops_pddl.y" /* yacc.c:1646  */
    {
        (yyval.pdis_PlNode) = (yyvsp[0].pdis_PlNode);
}
#line 2032 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 51:
#line 624 "scan-ops_pddl.y" /* yacc.c:1646  */
    {
        (yyval.pdis_PlNode) = dis_new_dis_PlNode(dis_AND);
        (yyval.pdis_PlNode)->sons = (yyvsp[-1].pdis_PlNode);
}
#line 2041 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 52:
#line 632 "scan-ops_pddl.y" /* yacc.c:1646  */
    { 
  
  dis_PlNode *pln;

  pln = dis_new_dis_PlNode(dis_ALL);
  pln->parse_vars = (yyvsp[-3].pdis_TypedList); 

  (yyval.pdis_PlNode) = pln;
  pln->sons = (yyvsp[-1].pdis_PlNode);

}
#line 2057 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 53:
#line 647 "scan-ops_pddl.y" /* yacc.c:1646  */
    {
        (yyval.pdis_PlNode) = NULL;
}
#line 2065 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 54:
#line 652 "scan-ops_pddl.y" /* yacc.c:1646  */
    {
        if ((yyvsp[-1].pdis_PlNode))
        {
                (yyvsp[-1].pdis_PlNode)->next = (yyvsp[0].pdis_PlNode);
                (yyval.pdis_PlNode) = (yyvsp[-1].pdis_PlNode);
        }
        else
                (yyval.pdis_PlNode) = (yyvsp[0].pdis_PlNode);
}
#line 2079 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 55:
#line 665 "scan-ops_pddl.y" /* yacc.c:1646  */
    {
        (yyval.pdis_PlNode) = (yyvsp[0].pdis_PlNode);
}
#line 2087 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 56:
#line 675 "scan-ops_pddl.y" /* yacc.c:1646  */
    {
	char temp[200];
	dis_TypedList *tl;
	dis_TokenList *t;
	GpG.is_deripred = TRUE;
	sprintf(temp, "deripred-%d>%s", ++dis_gnum_deripreds, (yyvsp[-4].pstring));
	scur_op = dis_new_dis_Pldis_Operator(temp);

	scur_op->effects = dis_new_dis_PlNode(dis_AND);   
	scur_op->effects->parse_vars = NULL;
	scur_op->effects->next = NULL;    
	scur_op->effects->atom = NULL;
	scur_op->effects->sons = dis_new_dis_PlNode(dis_ATOM);
	scur_op->effects->sons->parse_vars = NULL;
	scur_op->effects->sons->next = NULL;
	scur_op->effects->sons->atom = dis_new_dis_TokenList();
	scur_op->effects->sons->atom->item = (yyvsp[-4].pstring);
	scur_op->effects->sons->sons = NULL;

	t = scur_op->effects->sons->atom;
  scur_op->parse_params = (yyvsp[-3].pdis_TypedList);
  for (tl = scur_op->parse_params; tl; tl = tl->next) {
    /* to be able to distinguish params from :VARS
     */  
    scur_op->number_of_real_params++;
	t->next = dis_new_dis_TokenList();
	t->next->item = dis_new_dis_Token(strlen(tl->name)+1);
	strcpy(t->next->item, tl->name);
	t = t->next;
  }
	 	
	scur_op->preconds = (yyvsp[-1].pdis_PlNode);
	scur_op->next = dis_gloaded_ops;
	dis_gloaded_ops = scur_op; 
}
#line 2127 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 57:
#line 719 "scan-ops_pddl.y" /* yacc.c:1646  */
    {
        (yyval.pdis_PlNode) = (yyvsp[0].pdis_PlNode);
}
#line 2135 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 58:
#line 724 "scan-ops_pddl.y" /* yacc.c:1646  */
    { 
  (yyval.pdis_PlNode) = dis_new_dis_PlNode(dis_AND);
  (yyval.pdis_PlNode)->sons = (yyvsp[-1].pdis_PlNode);
}
#line 2144 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 59:
#line 732 "scan-ops_pddl.y" /* yacc.c:1646  */
    { 

  dis_PlNode *pln;

  pln = dis_new_dis_PlNode(dis_ALL);
  pln->parse_vars = (yyvsp[-3].pdis_TypedList);

  (yyval.pdis_PlNode) = pln;
  pln->sons = (yyvsp[-1].pdis_PlNode);

}
#line 2160 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 60:
#line 747 "scan-ops_pddl.y" /* yacc.c:1646  */
    {
  (yyval.pdis_PlNode) = dis_new_dis_PlNode(dis_COMP);
  (yyval.pdis_PlNode)->comp = LE;
  (yyval.pdis_PlNode)->lh = (yyvsp[-2].pdis_Parsedis_ExpNode);
  (yyval.pdis_PlNode)->rh = (yyvsp[-1].pdis_Parsedis_ExpNode);
  GpG.is_numeric_fluents = TRUE;
}
#line 2172 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 61:
#line 756 "scan-ops_pddl.y" /* yacc.c:1646  */
    {
  (yyval.pdis_PlNode) = dis_new_dis_PlNode(dis_COMP);
  (yyval.pdis_PlNode)->comp = LEQ;
  (yyval.pdis_PlNode)->lh = (yyvsp[-2].pdis_Parsedis_ExpNode);
  (yyval.pdis_PlNode)->rh = (yyvsp[-1].pdis_Parsedis_ExpNode);
  GpG.is_numeric_fluents = TRUE;
}
#line 2184 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 62:
#line 765 "scan-ops_pddl.y" /* yacc.c:1646  */
    {
  (yyval.pdis_PlNode) = dis_new_dis_PlNode(dis_COMP);
  (yyval.pdis_PlNode)->comp = EQ;
  (yyval.pdis_PlNode)->lh = (yyvsp[-2].pdis_Parsedis_ExpNode);
  (yyval.pdis_PlNode)->rh = (yyvsp[-1].pdis_Parsedis_ExpNode);
  GpG.is_numeric_fluents = TRUE;
}
#line 2196 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 63:
#line 774 "scan-ops_pddl.y" /* yacc.c:1646  */
    {
  (yyval.pdis_PlNode) = dis_new_dis_PlNode(dis_COMP);
  (yyval.pdis_PlNode)->comp = GEQ;
  (yyval.pdis_PlNode)->lh = (yyvsp[-2].pdis_Parsedis_ExpNode);
  (yyval.pdis_PlNode)->rh = (yyvsp[-1].pdis_Parsedis_ExpNode);
  GpG.is_numeric_fluents = TRUE;
}
#line 2208 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 64:
#line 783 "scan-ops_pddl.y" /* yacc.c:1646  */
    {
  (yyval.pdis_PlNode) = dis_new_dis_PlNode(dis_COMP);
  (yyval.pdis_PlNode)->comp = GE;
  (yyval.pdis_PlNode)->lh = (yyvsp[-2].pdis_Parsedis_ExpNode);
  (yyval.pdis_PlNode)->rh = (yyvsp[-1].pdis_Parsedis_ExpNode);
  GpG.is_numeric_fluents = TRUE;
}
#line 2220 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 65:
#line 792 "scan-ops_pddl.y" /* yacc.c:1646  */
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
#line 2236 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 66:
#line 805 "scan-ops_pddl.y" /* yacc.c:1646  */
    { 
  (yyval.pdis_PlNode) = dis_new_dis_PlNode(dis_OR);
  (yyval.pdis_PlNode)->sons = (yyvsp[-1].pdis_PlNode);
}
#line 2245 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 67:
#line 811 "scan-ops_pddl.y" /* yacc.c:1646  */
    { 
  (yyval.pdis_PlNode) = dis_new_dis_PlNode(dis_NOT);
  (yyval.pdis_PlNode)->sons = (yyvsp[-1].pdis_PlNode);
}
#line 2254 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 68:
#line 817 "scan-ops_pddl.y" /* yacc.c:1646  */
    { 
  dis_PlNode *np = dis_new_dis_PlNode(dis_NOT);
  np->sons = (yyvsp[-2].pdis_PlNode);
  np->next = (yyvsp[-1].pdis_PlNode);

  (yyval.pdis_PlNode) = dis_new_dis_PlNode(dis_OR);
  (yyval.pdis_PlNode)->sons = np;
}
#line 2267 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 69:
#line 829 "scan-ops_pddl.y" /* yacc.c:1646  */
    { 

  dis_PlNode *pln;

  pln = dis_new_dis_PlNode(dis_EX);
  pln->parse_vars = (yyvsp[-3].pdis_TypedList);

  (yyval.pdis_PlNode) = pln;
  pln->sons = (yyvsp[-1].pdis_PlNode);

}
#line 2283 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 70:
#line 845 "scan-ops_pddl.y" /* yacc.c:1646  */
    {
  (yyval.pdis_PlNode) = NULL;
}
#line 2291 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 71:
#line 850 "scan-ops_pddl.y" /* yacc.c:1646  */
    {
  (yyvsp[-1].pdis_PlNode)->next = (yyvsp[0].pdis_PlNode);
  (yyval.pdis_PlNode) = (yyvsp[-1].pdis_PlNode);
}
#line 2300 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 72:
#line 862 "scan-ops_pddl.y" /* yacc.c:1646  */
    {
  (yyval.pdis_PlNode) = dis_new_dis_PlNode( dis_NEF );
  (yyval.pdis_PlNode)->neft = ASSIGN;
  (yyval.pdis_PlNode)->lh = (yyvsp[-2].pdis_Parsedis_ExpNode);
  (yyval.pdis_PlNode)->rh = (yyvsp[-1].pdis_Parsedis_ExpNode);
  GpG.is_numeric_fluents = TRUE;
}
#line 2312 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 73:
#line 871 "scan-ops_pddl.y" /* yacc.c:1646  */
    {
  (yyval.pdis_PlNode) = dis_new_dis_PlNode( dis_NEF );
  (yyval.pdis_PlNode)->neft = SCALE_UP;
  (yyval.pdis_PlNode)->lh = (yyvsp[-2].pdis_Parsedis_ExpNode);
  (yyval.pdis_PlNode)->rh = (yyvsp[-1].pdis_Parsedis_ExpNode);
  GpG.is_numeric_fluents = TRUE;
}
#line 2324 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 74:
#line 880 "scan-ops_pddl.y" /* yacc.c:1646  */
    {
  (yyval.pdis_PlNode) = dis_new_dis_PlNode( dis_NEF );
  (yyval.pdis_PlNode)->neft = SCALE_DOWN;
  (yyval.pdis_PlNode)->lh = (yyvsp[-2].pdis_Parsedis_ExpNode);
  (yyval.pdis_PlNode)->rh = (yyvsp[-1].pdis_Parsedis_ExpNode);
  GpG.is_numeric_fluents = TRUE;
}
#line 2336 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 75:
#line 889 "scan-ops_pddl.y" /* yacc.c:1646  */
    {
  (yyval.pdis_PlNode) = dis_new_dis_PlNode( dis_NEF );
  (yyval.pdis_PlNode)->neft = INCREASE;
  (yyval.pdis_PlNode)->lh = (yyvsp[-2].pdis_Parsedis_ExpNode);
  (yyval.pdis_PlNode)->rh = (yyvsp[-1].pdis_Parsedis_ExpNode);
//  if (strcmp($3->atom->item, "TOTAL-COST"))     /* rule out action-costs */
    GpG.is_numeric_fluents = TRUE;
}
#line 2349 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 76:
#line 899 "scan-ops_pddl.y" /* yacc.c:1646  */
    {
  (yyval.pdis_PlNode) = dis_new_dis_PlNode( dis_NEF );
  (yyval.pdis_PlNode)->neft = DECREASE;
  (yyval.pdis_PlNode)->lh = (yyvsp[-2].pdis_Parsedis_ExpNode);
  (yyval.pdis_PlNode)->rh = (yyvsp[-1].pdis_Parsedis_ExpNode);
  GpG.is_numeric_fluents = TRUE;
}
#line 2361 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 77:
#line 908 "scan-ops_pddl.y" /* yacc.c:1646  */
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
#line 2377 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 78:
#line 921 "scan-ops_pddl.y" /* yacc.c:1646  */
    { 
  (yyval.pdis_PlNode) = dis_new_dis_PlNode(dis_AND);
  (yyval.pdis_PlNode)->sons = (yyvsp[-1].pdis_PlNode);
}
#line 2386 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 79:
#line 929 "scan-ops_pddl.y" /* yacc.c:1646  */
    { 

  dis_PlNode *pln;

  pln = dis_new_dis_PlNode(dis_ALL);
  pln->parse_vars = (yyvsp[-3].pdis_TypedList);

  (yyval.pdis_PlNode) = pln;
  pln->sons = (yyvsp[-1].pdis_PlNode);

}
#line 2402 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 80:
#line 942 "scan-ops_pddl.y" /* yacc.c:1646  */
    {
  /* This will be conditional effects in FF representation, but here
   * a formula like (dis_WHEN p q) will be saved as:
   *  [dis_WHEN]
   *  [sons]
   *   /  \
   * [p]  [q]
   * That means, the first son is p, and the second one is q. 
   */
  (yyval.pdis_PlNode) = dis_new_dis_PlNode(dis_WHEN);
  (yyvsp[-2].pdis_PlNode)->next = (yyvsp[-1].pdis_PlNode);
  (yyval.pdis_PlNode)->sons = (yyvsp[-2].pdis_PlNode);
  dis_gconditional_effects = dis_TRUE;
}
#line 2421 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 81:
#line 961 "scan-ops_pddl.y" /* yacc.c:1646  */
    { 
  (yyval.pdis_PlNode) = NULL; 
}
#line 2429 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 82:
#line 966 "scan-ops_pddl.y" /* yacc.c:1646  */
    {
  (yyvsp[-1].pdis_PlNode)->next = (yyvsp[0].pdis_PlNode);
  (yyval.pdis_PlNode) = (yyvsp[-1].pdis_PlNode);
}
#line 2438 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 83:
#line 978 "scan-ops_pddl.y" /* yacc.c:1646  */
    {
  (yyval.pdis_Parsedis_ExpNode) = dis_new_dis_Parsedis_ExpNode( FHEAD );
  (yyval.pdis_Parsedis_ExpNode)->atom = dis_new_dis_TokenList();
  (yyval.pdis_Parsedis_ExpNode)->atom->item = dis_new_dis_Token( strlen((yyvsp[-2].string))+1 );
  strcpy( (yyval.pdis_Parsedis_ExpNode)->atom->item, (yyvsp[-2].string) );
  (yyval.pdis_Parsedis_ExpNode)->atom->next = (yyvsp[-1].pdis_TokenList);
}
#line 2450 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 84:
#line 999 "scan-ops_pddl.y" /* yacc.c:1646  */
    { 
  (yyval.pdis_Parsedis_ExpNode) = dis_new_dis_Parsedis_ExpNode( NUMBER );
  (yyval.pdis_Parsedis_ExpNode)->atom = dis_new_dis_TokenList();
  (yyval.pdis_Parsedis_ExpNode)->atom->item = dis_new_dis_Token( strlen((yyvsp[0].string))+1 );
  strcpy( (yyval.pdis_Parsedis_ExpNode)->atom->item, (yyvsp[0].string) );
}
#line 2461 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 85:
#line 1007 "scan-ops_pddl.y" /* yacc.c:1646  */
    {
  (yyval.pdis_Parsedis_ExpNode) = (yyvsp[0].pdis_Parsedis_ExpNode);
}
#line 2469 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 86:
#line 1012 "scan-ops_pddl.y" /* yacc.c:1646  */
    {
  (yyval.pdis_Parsedis_ExpNode) = dis_new_dis_Parsedis_ExpNode( MINUS );
  (yyval.pdis_Parsedis_ExpNode)->leftson = (yyvsp[-1].pdis_Parsedis_ExpNode);
}
#line 2478 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 87:
#line 1018 "scan-ops_pddl.y" /* yacc.c:1646  */
    {
  (yyval.pdis_Parsedis_ExpNode) = dis_new_dis_Parsedis_ExpNode( AD );
  (yyval.pdis_Parsedis_ExpNode)->leftson = (yyvsp[-2].pdis_Parsedis_ExpNode);
  (yyval.pdis_Parsedis_ExpNode)->rightson = (yyvsp[-1].pdis_Parsedis_ExpNode);
}
#line 2488 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 88:
#line 1025 "scan-ops_pddl.y" /* yacc.c:1646  */
    {
  (yyval.pdis_Parsedis_ExpNode) = dis_new_dis_Parsedis_ExpNode( SU );
  (yyval.pdis_Parsedis_ExpNode)->leftson = (yyvsp[-2].pdis_Parsedis_ExpNode);
  (yyval.pdis_Parsedis_ExpNode)->rightson = (yyvsp[-1].pdis_Parsedis_ExpNode);
}
#line 2498 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 89:
#line 1032 "scan-ops_pddl.y" /* yacc.c:1646  */
    {
  (yyval.pdis_Parsedis_ExpNode) = dis_new_dis_Parsedis_ExpNode( MU );
  (yyval.pdis_Parsedis_ExpNode)->leftson = (yyvsp[-2].pdis_Parsedis_ExpNode);
  (yyval.pdis_Parsedis_ExpNode)->rightson = (yyvsp[-1].pdis_Parsedis_ExpNode);
}
#line 2508 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 90:
#line 1039 "scan-ops_pddl.y" /* yacc.c:1646  */
    {
  (yyval.pdis_Parsedis_ExpNode) = dis_new_dis_Parsedis_ExpNode( DI );
  (yyval.pdis_Parsedis_ExpNode)->leftson = (yyvsp[-2].pdis_Parsedis_ExpNode);
  (yyval.pdis_Parsedis_ExpNode)->rightson = (yyvsp[-1].pdis_Parsedis_ExpNode);
}
#line 2518 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 91:
#line 1050 "scan-ops_pddl.y" /* yacc.c:1646  */
    { 
  (yyval.pdis_TokenList) = (yyvsp[-1].pdis_TokenList);
  sis_negated = dis_TRUE;
}
#line 2527 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 92:
#line 1056 "scan-ops_pddl.y" /* yacc.c:1646  */
    {
  (yyval.pdis_TokenList) = (yyvsp[0].pdis_TokenList);
}
#line 2535 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 93:
#line 1065 "scan-ops_pddl.y" /* yacc.c:1646  */
    { 
  (yyval.pdis_TokenList) = dis_new_dis_TokenList();
  (yyval.pdis_TokenList)->item = (yyvsp[-2].pstring);
  (yyval.pdis_TokenList)->next = (yyvsp[-1].pdis_TokenList);
}
#line 2545 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 94:
#line 1072 "scan-ops_pddl.y" /* yacc.c:1646  */
    {
  (yyval.pdis_TokenList) = dis_new_dis_TokenList();
  (yyval.pdis_TokenList)->item = dis_copy_dis_Token("=");
  (yyval.pdis_TokenList)->next = (yyvsp[-1].pdis_TokenList);
}
#line 2555 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 95:
#line 1083 "scan-ops_pddl.y" /* yacc.c:1646  */
    { (yyval.pdis_TokenList) = NULL; }
#line 2561 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 96:
#line 1086 "scan-ops_pddl.y" /* yacc.c:1646  */
    {
  (yyval.pdis_TokenList) = dis_new_dis_TokenList();
  (yyval.pdis_TokenList)->item = (yyvsp[-1].pstring);
  (yyval.pdis_TokenList)->next = (yyvsp[0].pdis_TokenList);
}
#line 2571 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 97:
#line 1097 "scan-ops_pddl.y" /* yacc.c:1646  */
    { 
  (yyval.pstring) = dis_new_dis_Token( strlen((yyvsp[0].string))+1 );
  strcpy( (yyval.pstring), (yyvsp[0].string) );
}
#line 2580 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 98:
#line 1103 "scan-ops_pddl.y" /* yacc.c:1646  */
    { 
  (yyval.pstring) = dis_new_dis_Token( strlen((yyvsp[0].string))+1 );
  strcpy( (yyval.pstring), (yyvsp[0].string) );
}
#line 2589 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 99:
#line 1113 "scan-ops_pddl.y" /* yacc.c:1646  */
    {
  (yyval.pdis_TokenList) = dis_new_dis_TokenList();
  (yyval.pdis_TokenList)->item = dis_new_dis_Token( strlen((yyvsp[0].string))+1 );
  strcpy( (yyval.pdis_TokenList)->item, (yyvsp[0].string) );
}
#line 2599 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 100:
#line 1120 "scan-ops_pddl.y" /* yacc.c:1646  */
    {
  (yyval.pdis_TokenList) = dis_new_dis_TokenList();
  (yyval.pdis_TokenList)->item = dis_new_dis_Token( strlen((yyvsp[-1].string))+1 );
  strcpy( (yyval.pdis_TokenList)->item, (yyvsp[-1].string) );
  (yyval.pdis_TokenList)->next = (yyvsp[0].pdis_TokenList);
}
#line 2610 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 101:
#line 1131 "scan-ops_pddl.y" /* yacc.c:1646  */
    { 
  (yyval.pstring) = dis_new_dis_Token( strlen((yyvsp[0].string))+1 );
  strcpy( (yyval.pstring), (yyvsp[0].string) );
}
#line 2619 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 102:
#line 1141 "scan-ops_pddl.y" /* yacc.c:1646  */
    { (yyval.pdis_TypedList) = NULL; }
#line 2625 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 103:
#line 1144 "scan-ops_pddl.y" /* yacc.c:1646  */
    { 

  (yyval.pdis_TypedList) = dis_new_dis_TypedList();
  (yyval.pdis_TypedList)->name = dis_new_dis_Token( strlen((yyvsp[-4].string))+1 );
  strcpy( (yyval.pdis_TypedList)->name, (yyvsp[-4].string) );
  (yyval.pdis_TypedList)->type = (yyvsp[-2].pdis_TokenList);
  (yyval.pdis_TypedList)->next = (yyvsp[0].pdis_TypedList);
}
#line 2638 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 104:
#line 1154 "scan-ops_pddl.y" /* yacc.c:1646  */
    {
  (yyval.pdis_TypedList) = dis_new_dis_TypedList();
  (yyval.pdis_TypedList)->name = dis_new_dis_Token( strlen((yyvsp[-3].string))+1 );
  strcpy( (yyval.pdis_TypedList)->name, (yyvsp[-3].string) );
  (yyval.pdis_TypedList)->type = dis_new_dis_TokenList();
  (yyval.pdis_TypedList)->type->item = dis_new_dis_Token( strlen((yyvsp[-1].string))+1 );
  strcpy( (yyval.pdis_TypedList)->type->item, (yyvsp[-1].string) );
  (yyval.pdis_TypedList)->next = (yyvsp[0].pdis_TypedList);
}
#line 2652 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 105:
#line 1165 "scan-ops_pddl.y" /* yacc.c:1646  */
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
#line 2670 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 106:
#line 1184 "scan-ops_pddl.y" /* yacc.c:1646  */
    { (yyval.pdis_TypedList) = NULL; }
#line 2676 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 107:
#line 1187 "scan-ops_pddl.y" /* yacc.c:1646  */
    { 
  (yyval.pdis_TypedList) = dis_new_dis_TypedList();
  (yyval.pdis_TypedList)->name = dis_new_dis_Token( strlen((yyvsp[-4].string))+1 );
  strcpy( (yyval.pdis_TypedList)->name, (yyvsp[-4].string) );
  (yyval.pdis_TypedList)->type = (yyvsp[-2].pdis_TokenList);
  (yyval.pdis_TypedList)->next = (yyvsp[0].pdis_TypedList);
}
#line 2688 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 108:
#line 1196 "scan-ops_pddl.y" /* yacc.c:1646  */
    {
  (yyval.pdis_TypedList) = dis_new_dis_TypedList();
  (yyval.pdis_TypedList)->name = dis_new_dis_Token( strlen((yyvsp[-3].string))+1 );
  strcpy( (yyval.pdis_TypedList)->name, (yyvsp[-3].string) );
  (yyval.pdis_TypedList)->type = dis_new_dis_TokenList();
  (yyval.pdis_TypedList)->type->item = dis_new_dis_Token( strlen((yyvsp[-1].string))+1 );
  strcpy( (yyval.pdis_TypedList)->type->item, (yyvsp[-1].string) );
  (yyval.pdis_TypedList)->next = (yyvsp[0].pdis_TypedList);
}
#line 2702 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 109:
#line 1207 "scan-ops_pddl.y" /* yacc.c:1646  */
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
#line 2720 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 110:
#line 1225 "scan-ops_pddl.y" /* yacc.c:1646  */
    {                  
  dis_opserr( ACTION, NULL );
}
#line 2728 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 111:
#line 1229 "scan-ops_pddl.y" /* yacc.c:1646  */
    {                  
  scur_op = dis_new_dis_Pldis_Operator( (yyvsp[0].string) );
  preference_pointer = NULL;
}
#line 2737 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 112:
#line 1234 "scan-ops_pddl.y" /* yacc.c:1646  */
    {                  
  char temp[128];
  dis_PlNode *p;
  
  /* an artificial effect for calculating the duration value */
  p = dis_new_dis_PlNode(dis_AND);
  p->sons = dis_new_dis_PlNode(dis_NEF);
  p->sons->neft = ASSIGN;
  p->sons->lh = dis_new_dis_Parsedis_ExpNode( FHEAD ); 
  p->sons->lh->atom = dis_new_dis_TokenList();
  p->sons->lh->atom->item = dis_copy_dis_Token("DURATIONFUNCTION");
  p->sons->rh = duration_exp;
  if (scur_op->effects->connective == dis_AND)
    p->sons->next = scur_op->effects->sons;  
  else
    p->sons->next = scur_op->effects;  
  scur_op->effects = p;

  
  scur_op->next = dis_gloaded_ops;
  dis_gloaded_ops = scur_op;
  scur_op = NULL;
  gnum_das++;
  GpG.is_durative = TRUE;
}
#line 2767 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 113:
#line 1263 "scan-ops_pddl.y" /* yacc.c:1646  */
    {                  
	scur_op->duration = (yyvsp[0].pdis_PlNode);
}
#line 2775 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 114:
#line 1268 "scan-ops_pddl.y" /* yacc.c:1646  */
    {                  
	scur_op->preconds = (yyvsp[0].pdis_PlNode);
}
#line 2783 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 115:
#line 1272 "scan-ops_pddl.y" /* yacc.c:1646  */
    {                  
	scur_op->effects = (yyvsp[0].pdis_PlNode);
}
#line 2791 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 116:
#line 1279 "scan-ops_pddl.y" /* yacc.c:1646  */
    {
        (yyval.pdis_PlNode) = NULL;
}
#line 2799 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 117:
#line 1284 "scan-ops_pddl.y" /* yacc.c:1646  */
    {
        (yyval.pdis_PlNode) = (yyvsp[0].pdis_PlNode);
}
#line 2807 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 118:
#line 1289 "scan-ops_pddl.y" /* yacc.c:1646  */
    {
        (yyval.pdis_PlNode) = dis_new_dis_PlNode(dis_AND);
        (yyval.pdis_PlNode)->sons = (yyvsp[-1].pdis_PlNode);
}
#line 2816 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 119:
#line 1297 "scan-ops_pddl.y" /* yacc.c:1646  */
    { 
  
  dis_PlNode *pln;

  pln = dis_new_dis_PlNode(dis_ALL);
  pln->parse_vars = (yyvsp[-3].pdis_TypedList); 

  (yyval.pdis_PlNode) = pln;
  pln->sons = (yyvsp[-1].pdis_PlNode);

}
#line 2832 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 120:
#line 1312 "scan-ops_pddl.y" /* yacc.c:1646  */
    {
        (yyval.pdis_PlNode) = NULL;
}
#line 2840 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 121:
#line 1317 "scan-ops_pddl.y" /* yacc.c:1646  */
    {
        if ((yyvsp[-1].pdis_PlNode))
        {
                (yyvsp[-1].pdis_PlNode)->next = (yyvsp[0].pdis_PlNode);
                (yyval.pdis_PlNode) = (yyvsp[-1].pdis_PlNode);
        }   
        else
                (yyval.pdis_PlNode) = (yyvsp[0].pdis_PlNode);
}
#line 2854 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 122:
#line 1330 "scan-ops_pddl.y" /* yacc.c:1646  */
    {
        (yyval.pdis_PlNode) = (yyvsp[0].pdis_PlNode);
}
#line 2862 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 123:
#line 1337 "scan-ops_pddl.y" /* yacc.c:1646  */
    {                   
  (yyval.pdis_PlNode) = dis_new_dis_PlNode(dis_AND);
  (yyval.pdis_PlNode)->sons = (yyvsp[0].pdis_PlNode);    
}
#line 2871 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 124:
#line 1343 "scan-ops_pddl.y" /* yacc.c:1646  */
    {                   
  (yyval.pdis_PlNode) = dis_new_dis_PlNode(dis_AND);
  (yyval.pdis_PlNode)->sons = (yyvsp[-1].pdis_PlNode);    
}
#line 2880 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 125:
#line 1351 "scan-ops_pddl.y" /* yacc.c:1646  */
    {                   
  (yyval.pdis_PlNode)=(yyvsp[0].pdis_PlNode);            
}
#line 2888 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 126:
#line 1356 "scan-ops_pddl.y" /* yacc.c:1646  */
    {                   
  (yyval.pdis_PlNode) = (yyvsp[-1].pdis_PlNode);          
  (yyval.pdis_PlNode)->next = (yyvsp[0].pdis_PlNode);    
}
#line 2897 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 127:
#line 1364 "scan-ops_pddl.y" /* yacc.c:1646  */
    {
//	$$ = dis_new_dis_PlNode(dis_AT_START_CONN);                   
//  $$->sons = $3;
	(yyval.pdis_PlNode) = (yyvsp[-1].pdis_PlNode);    
	(yyval.pdis_PlNode)->value = 1;
}
#line 2908 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 128:
#line 1372 "scan-ops_pddl.y" /* yacc.c:1646  */
    {                   
//	$$ = dis_new_dis_PlNode(dis_AT_END_CONN);
//  $$->sons = $3;    
	(yyval.pdis_PlNode) = (yyvsp[-1].pdis_PlNode);
	(yyval.pdis_PlNode)->value = 2;
}
#line 2919 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 129:
#line 1380 "scan-ops_pddl.y" /* yacc.c:1646  */
    {                   
//	$$ = dis_new_dis_PlNode(dis_OVER_ALL_CONN);
//  $$->sons = $3;    
	(yyval.pdis_PlNode) = (yyvsp[-1].pdis_PlNode);
	(yyval.pdis_PlNode)->value = 3;
}
#line 2930 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 130:
#line 1390 "scan-ops_pddl.y" /* yacc.c:1646  */
    {
  (yyval.pdis_PlNode) = dis_new_dis_PlNode(dis_COMP);
  (yyval.pdis_PlNode)->comp = EQ;
  (yyval.pdis_PlNode)->lh = NULL;
  (yyval.pdis_PlNode)->rh = (yyvsp[-1].pdis_Parsedis_ExpNode);
  duration_exp = (yyvsp[-1].pdis_Parsedis_ExpNode);
}
#line 2942 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 131:
#line 1401 "scan-ops_pddl.y" /* yacc.c:1646  */
    {
}
#line 2949 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 132:
#line 1405 "scan-ops_pddl.y" /* yacc.c:1646  */
    {                   
  (yyval.pdis_PlNode) = dis_new_dis_PlNode(dis_AND);
  (yyval.pdis_PlNode)->sons = (yyvsp[-1].pdis_PlNode);    
}
#line 2958 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 133:
#line 1411 "scan-ops_pddl.y" /* yacc.c:1646  */
    {                   
  (yyval.pdis_PlNode) = dis_new_dis_PlNode(dis_AND);
  (yyval.pdis_PlNode)->sons = (yyvsp[0].pdis_PlNode);    
}
#line 2967 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 134:
#line 1419 "scan-ops_pddl.y" /* yacc.c:1646  */
    {                   
  dis_PlNode *pln;      

  pln = dis_new_dis_PlNode(dis_ALL);
  pln->parse_vars = (yyvsp[-3].pdis_TypedList);

  (yyval.pdis_PlNode) = pln;         
  pln->sons = (yyvsp[-1].pdis_PlNode);   
}
#line 2981 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 135:
#line 1430 "scan-ops_pddl.y" /* yacc.c:1646  */
    {                   
  /* This will be conditional effects in FF representation, but here
   * a formula like (WHEN p q) will be saved as:
   *  [WHEN]        
   *  [sons]        
   *   /  \         
   * [p]  [q]       
   * That means, the first son is p, and the second one is q.
   */
  (yyval.pdis_PlNode) = dis_new_dis_PlNode(dis_WHEN);
  (yyvsp[-2].pdis_PlNode)->next = (yyvsp[-1].pdis_PlNode);    
  (yyval.pdis_PlNode)->sons = (yyvsp[-2].pdis_PlNode);   
  dis_gconditional_effects = dis_TRUE; 
}
#line 3000 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 136:
#line 1446 "scan-ops_pddl.y" /* yacc.c:1646  */
    {
  (yyval.pdis_PlNode) = dis_new_dis_PlNode( dis_NEF );
  (yyval.pdis_PlNode)->neft = ASSIGN;
  (yyval.pdis_PlNode)->lh = (yyvsp[-2].pdis_Parsedis_ExpNode);
  (yyval.pdis_PlNode)->rh = (yyvsp[-1].pdis_Parsedis_ExpNode);
  GpG.is_numeric_fluents = TRUE;
}
#line 3012 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 137:
#line 1455 "scan-ops_pddl.y" /* yacc.c:1646  */
    {
  (yyval.pdis_PlNode) = dis_new_dis_PlNode( dis_NEF );
  (yyval.pdis_PlNode)->neft = SCALE_UP;
  (yyval.pdis_PlNode)->lh = (yyvsp[-2].pdis_Parsedis_ExpNode);
  (yyval.pdis_PlNode)->rh = (yyvsp[-1].pdis_Parsedis_ExpNode);
  GpG.is_numeric_fluents = TRUE;
}
#line 3024 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 138:
#line 1464 "scan-ops_pddl.y" /* yacc.c:1646  */
    {
  (yyval.pdis_PlNode) = dis_new_dis_PlNode( dis_NEF );
  (yyval.pdis_PlNode)->neft = SCALE_DOWN;
  (yyval.pdis_PlNode)->lh = (yyvsp[-2].pdis_Parsedis_ExpNode);
  (yyval.pdis_PlNode)->rh = (yyvsp[-1].pdis_Parsedis_ExpNode);
  GpG.is_numeric_fluents = TRUE;
}
#line 3036 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 139:
#line 1473 "scan-ops_pddl.y" /* yacc.c:1646  */
    {
  (yyval.pdis_PlNode) = dis_new_dis_PlNode( dis_NEF );
  (yyval.pdis_PlNode)->neft = INCREASE;
  (yyval.pdis_PlNode)->lh = (yyvsp[-2].pdis_Parsedis_ExpNode);
  (yyval.pdis_PlNode)->rh = (yyvsp[-1].pdis_Parsedis_ExpNode);
//  if (strcmp($3->atom->item, "TOTAL-COST"))     /* rule out action-costs */
    GpG.is_numeric_fluents = TRUE;
}
#line 3049 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 140:
#line 1483 "scan-ops_pddl.y" /* yacc.c:1646  */
    {
  (yyval.pdis_PlNode) = dis_new_dis_PlNode( dis_NEF );
  (yyval.pdis_PlNode)->neft = DECREASE;
  (yyval.pdis_PlNode)->lh = (yyvsp[-2].pdis_Parsedis_ExpNode);
  (yyval.pdis_PlNode)->rh = (yyvsp[-1].pdis_Parsedis_ExpNode);
  GpG.is_numeric_fluents = TRUE;
}
#line 3061 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 141:
#line 1494 "scan-ops_pddl.y" /* yacc.c:1646  */
    {                   
  (yyval.pdis_PlNode) = (yyvsp[0].pdis_PlNode);          
}
#line 3069 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 142:
#line 1499 "scan-ops_pddl.y" /* yacc.c:1646  */
    {                   
  (yyval.pdis_PlNode) = (yyvsp[-1].pdis_PlNode);          
  (yyval.pdis_PlNode)->next = (yyvsp[0].pdis_PlNode);    
}
#line 3078 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 143:
#line 1507 "scan-ops_pddl.y" /* yacc.c:1646  */
    {
//	$$ = dis_new_dis_PlNode(dis_AT_START_CONN);
//  $$->sons = $3;
	(yyval.pdis_PlNode) = (yyvsp[-1].pdis_PlNode);
	(yyval.pdis_PlNode)->value = 1;
}
#line 3089 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 144:
#line 1515 "scan-ops_pddl.y" /* yacc.c:1646  */
    {
//	$$ = dis_new_dis_PlNode(dis_AT_END_CONN);
//  $$->sons = $3;
	(yyval.pdis_PlNode) = (yyvsp[-1].pdis_PlNode);
	(yyval.pdis_PlNode)->value = 2;
}
#line 3100 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 145:
#line 1523 "scan-ops_pddl.y" /* yacc.c:1646  */
    {
//	$$ = dis_new_dis_PlNode(dis_AT_START_CONN);
//  $$->sons = $3;
	(yyval.pdis_PlNode) = (yyvsp[-1].pdis_PlNode);
	(yyval.pdis_PlNode)->value = 1;
}
#line 3111 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 146:
#line 1531 "scan-ops_pddl.y" /* yacc.c:1646  */
    {
//	$$ = dis_new_dis_PlNode(dis_AT_END_CONN);
//  $$->sons = $3;
	(yyval.pdis_PlNode) = (yyvsp[-1].pdis_PlNode);
	(yyval.pdis_PlNode)->value = 2;
}
#line 3122 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 147:
#line 1541 "scan-ops_pddl.y" /* yacc.c:1646  */
    {
  (yyval.pdis_PlNode) = dis_new_dis_PlNode( dis_NEF );
  (yyval.pdis_PlNode)->neft = ASSIGN;
  (yyval.pdis_PlNode)->lh = (yyvsp[-2].pdis_Parsedis_ExpNode);
  (yyval.pdis_PlNode)->rh = (yyvsp[-1].pdis_Parsedis_ExpNode);
  GpG.is_numeric_fluents = TRUE;
}
#line 3134 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 148:
#line 1550 "scan-ops_pddl.y" /* yacc.c:1646  */
    {
  (yyval.pdis_PlNode) = dis_new_dis_PlNode( dis_NEF );
  (yyval.pdis_PlNode)->neft = SCALE_UP;
  (yyval.pdis_PlNode)->lh = (yyvsp[-2].pdis_Parsedis_ExpNode);
  (yyval.pdis_PlNode)->rh = (yyvsp[-1].pdis_Parsedis_ExpNode);
  GpG.is_numeric_fluents = TRUE;
}
#line 3146 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 149:
#line 1559 "scan-ops_pddl.y" /* yacc.c:1646  */
    {
  (yyval.pdis_PlNode) = dis_new_dis_PlNode( dis_NEF );
  (yyval.pdis_PlNode)->neft = SCALE_DOWN;
  (yyval.pdis_PlNode)->lh = (yyvsp[-2].pdis_Parsedis_ExpNode);
  (yyval.pdis_PlNode)->rh = (yyvsp[-1].pdis_Parsedis_ExpNode);
  GpG.is_numeric_fluents = TRUE;
}
#line 3158 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 150:
#line 1568 "scan-ops_pddl.y" /* yacc.c:1646  */
    {
  (yyval.pdis_PlNode) = dis_new_dis_PlNode( dis_NEF );
  (yyval.pdis_PlNode)->neft = INCREASE;
  (yyval.pdis_PlNode)->lh = (yyvsp[-2].pdis_Parsedis_ExpNode);
  (yyval.pdis_PlNode)->rh = (yyvsp[-1].pdis_Parsedis_ExpNode);
//  if (strcmp($3->atom->item, "TOTAL-COST"))     /* rule out action-costs */
    GpG.is_numeric_fluents = TRUE;
}
#line 3171 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 151:
#line 1578 "scan-ops_pddl.y" /* yacc.c:1646  */
    {
  (yyval.pdis_PlNode) = dis_new_dis_PlNode( dis_NEF );
  (yyval.pdis_PlNode)->neft = DECREASE;
  (yyval.pdis_PlNode)->lh = (yyvsp[-2].pdis_Parsedis_ExpNode);
  (yyval.pdis_PlNode)->rh = (yyvsp[-1].pdis_Parsedis_ExpNode);
  GpG.is_numeric_fluents = TRUE;
}
#line 3183 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 152:
#line 1589 "scan-ops_pddl.y" /* yacc.c:1646  */
    {
  (yyval.pdis_Parsedis_ExpNode) = dis_new_dis_Parsedis_ExpNode( AD );
  (yyval.pdis_Parsedis_ExpNode)->leftson = (yyvsp[-2].pdis_Parsedis_ExpNode);
  (yyval.pdis_Parsedis_ExpNode)->rightson = (yyvsp[-1].pdis_Parsedis_ExpNode);
}
#line 3193 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 153:
#line 1596 "scan-ops_pddl.y" /* yacc.c:1646  */
    {
  (yyval.pdis_Parsedis_ExpNode) = dis_new_dis_Parsedis_ExpNode( SU );
  (yyval.pdis_Parsedis_ExpNode)->leftson = (yyvsp[-2].pdis_Parsedis_ExpNode);
  (yyval.pdis_Parsedis_ExpNode)->rightson = (yyvsp[-1].pdis_Parsedis_ExpNode);
}
#line 3203 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 154:
#line 1603 "scan-ops_pddl.y" /* yacc.c:1646  */
    {
  (yyval.pdis_Parsedis_ExpNode) = dis_new_dis_Parsedis_ExpNode( MU );
  (yyval.pdis_Parsedis_ExpNode)->leftson = (yyvsp[-2].pdis_Parsedis_ExpNode);
  (yyval.pdis_Parsedis_ExpNode)->rightson = (yyvsp[-1].pdis_Parsedis_ExpNode);
}
#line 3213 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 155:
#line 1610 "scan-ops_pddl.y" /* yacc.c:1646  */
    {
  (yyval.pdis_Parsedis_ExpNode) = dis_new_dis_Parsedis_ExpNode( DI );
  (yyval.pdis_Parsedis_ExpNode)->leftson = (yyvsp[-2].pdis_Parsedis_ExpNode);
  (yyval.pdis_Parsedis_ExpNode)->rightson = (yyvsp[-1].pdis_Parsedis_ExpNode);
}
#line 3223 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 156:
#line 1617 "scan-ops_pddl.y" /* yacc.c:1646  */
    {
  (yyval.pdis_Parsedis_ExpNode) = dis_new_dis_Parsedis_ExpNode( MINUS );
  (yyval.pdis_Parsedis_ExpNode)->leftson = (yyvsp[-1].pdis_Parsedis_ExpNode);
}
#line 3232 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 157:
#line 1623 "scan-ops_pddl.y" /* yacc.c:1646  */
    {
	(yyval.pdis_Parsedis_ExpNode) = copy_dis_Parsedis_ExpNode(duration_exp);
}
#line 3240 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;


#line 3244 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
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
#line 1630 "scan-ops_pddl.y" /* yacc.c:1906  */

#include "lex.dis_ops_pddl.c"


/**********************************************************************
 * Functions
 **********************************************************************/

/* 
 * call	bison -pops -bscan-ops scan-ops.y
 */

  


int yyerror( char *msg )

{

  fflush(stdout);
  fprintf(stderr, "\n%s: syntax error in line %d, '%s':\n", 
	  dis_gact_filename, dis_lineno, dis_ops_pddltext);

  if ( NULL != sact_err_par ) {
    fprintf(stderr, "%s %s\n", serrmsg[sact_err], sact_err_par);
  } else {
    fprintf(stderr, "%s\n", serrmsg[sact_err]);
  }

  exit( 1 );

}



void dis_load_ops_file( char *filename )

{

  FILE * fp;/* pointer to input files */
  char tmp[dis_MAX_LENGTH] = "";

  /* open operator file 
   */
  if( ( fp = fopen( filename, "r" ) ) == NULL ) {
    sprintf(tmp, "\nff: can't find operator file: %s\n\n", filename );
    perror(tmp);
    exit( 1 );
  }

  dis_gact_filename = filename;
  dis_lineno = 1; 
  dis_ops_pddlin = fp;

  yyparse();

  fclose( fp );/* and close file again */

}
