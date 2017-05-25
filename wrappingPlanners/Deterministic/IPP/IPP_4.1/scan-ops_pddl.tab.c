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
#define yyparse         ops_pddlparse
#define yylex           ops_pddllex
#define yyerror         ops_pddlerror
#define yydebug         ops_pddldebug
#define yynerrs         ops_pddlnerrs

#define yylval          ops_pddllval
#define yychar          ops_pddlchar

/* Copy the first part of user declarations.  */
#line 1 "scan-ops_pddl.y" /* yacc.c:339  */

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


#line 172 "scan-ops_pddl.tab.c" /* yacc.c:339  */

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
extern int ops_pddldebug;
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

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED

union YYSTYPE
{
#line 101 "scan-ops_pddl.y" /* yacc.c:355  */

  char string[MAX_LENGTH];
  char* pstring;
  PlNode* pPlNode;
  FactList* pFactList;
  TokenList* pTokenList;

#line 248 "scan-ops_pddl.tab.c" /* yacc.c:355  */
};

typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE ops_pddllval;

int ops_pddlparse (void);



/* Copy the second part of user declarations.  */

#line 265 "scan-ops_pddl.tab.c" /* yacc.c:358  */

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
#define YYLAST   147

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  31
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  44
/* YYNRULES -- Number of rules.  */
#define YYNRULES  79
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  176

/* YYTRANSLATE[YYX] -- Symbol number corresponding to YYX as returned
   by yylex, with out-of-bounds checking.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   285

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
      25,    26,    27,    28,    29,    30
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
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

#if YYDEBUG || YYERROR_VERBOSE || 0
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "DEFINE_TOK", "DOMAIN_TOK",
  "REQUIREMENTS_TOK", "TYPES_TOK", "EITHER_TOK", "CONSTANTS_TOK",
  "ACTION_TOK", "AXIOM_TOK", "VARS_TOK", "CONTEXT_TOK", "IMPLIES_TOK",
  "PRECONDITION_TOK", "PARAMETERS_TOK", "PREDICATES_TOK", "EFFECT_TOK",
  "AND_TOK", "NOT_TOK", "WHEN_TOK", "FORALL_TOK", "IMPLY_TOK", "OR_TOK",
  "EXISTS_TOK", "EQUAL_TOK", "NAME", "VARIABLE", "TYPE", "OPEN_PAREN",
  "CLOSE_PAREN", "$accept", "file", "$@1", "domain_definition", "$@2",
  "domain_name", "optional_domain_defs", "predicates_def", "$@3",
  "predicates_list", "$@4", "require_def", "$@5", "$@6",
  "require_key_star", "$@7", "types_def", "$@8", "constants_def", "$@9",
  "action_def", "$@10", "$@11", "param_def", "action_def_body", "$@12",
  "$@13", "axiom_def", "$@14", "$@15", "$@16", "$@17",
  "adl_goal_description", "adl_goal_description_star", "adl_effect",
  "adl_effect_star", "literal_term", "atomic_formula_term", "term_star",
  "term", "name_plus", "predicate", "typed_list_name",
  "typed_list_variable", YY_NULLPTR
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
     285
};
# endif

#define YYPACT_NINF -103

#define yypact_value_is_default(Yystate) \
  (!!((Yystate) == (-103)))

#define YYTABLE_NINF -1

#define yytable_value_is_error(Yytable_value) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int8 yypact[] =
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

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_uint8 yydefact[] =
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

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int8 yypgoto[] =
{
    -103,  -103,  -103,  -103,  -103,  -103,    98,  -103,  -103,     6,
    -103,  -103,  -103,  -103,    70,  -103,  -103,  -103,  -103,  -103,
    -103,  -103,  -103,  -103,   -99,  -103,  -103,  -103,  -103,  -103,
    -103,  -103,   -65,   -97,   -77,   -24,   -79,  -102,    -3,  -103,
     -27,  -103,   -32,   -51
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     1,     2,     5,    10,     8,    14,    15,    48,    40,
      84,    16,    34,    49,    61,    72,    17,    35,    18,    36,
      19,    37,    55,    66,    80,   111,   116,    20,    38,    97,
     156,   172,   120,   121,   133,   134,    91,    92,   130,   131,
      63,   110,    43,    58
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_uint8 yytable[] =
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

static const yytype_int16 yycheck[] =
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
static const yytype_uint8 yystos[] =
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

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
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

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
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
#line 157 "scan-ops_pddl.y" /* yacc.c:1646  */
    { opserr( DOMDEF_EXPECTED, NULL ); }
#line 1458 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 4:
#line 165 "scan-ops_pddl.y" /* yacc.c:1646  */
    { 
  /* initialize typetree */
  gglobal_type_tree_list = new_type_tree_list( STANDARD_TYPE );
}
#line 1467 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 5:
#line 170 "scan-ops_pddl.y" /* yacc.c:1646  */
    {
  if ( gcmd_line.display_info ) {
    fprintf(OUT, "\ndomain '%s' defined\n", gdomain_name);
  }
}
#line 1477 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 6:
#line 180 "scan-ops_pddl.y" /* yacc.c:1646  */
    { 
  gdomain_name = new_token( strlen((yyvsp[-1].string))+1 );
  strcpy( gdomain_name, (yyvsp[-1].string));
}
#line 1486 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 14:
#line 209 "scan-ops_pddl.y" /* yacc.c:1646  */
    {
}
#line 1493 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 15:
#line 212 "scan-ops_pddl.y" /* yacc.c:1646  */
    { 
}
#line 1500 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 16:
#line 218 "scan-ops_pddl.y" /* yacc.c:1646  */
    {}
#line 1506 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 17:
#line 221 "scan-ops_pddl.y" /* yacc.c:1646  */
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
  tl->item=copy_string((yyvsp[-2].string));
  fl1=(yyvsp[-1].pFactList);
  while (fl1!=NULL) 
    {
      tl1=new_token_list();
      tl->next=tl1;
      tl=tl1;
      tl1->item=copy_string(fl1->item->next->item);
      fl1=fl1->next;
    }
  free_complete_FactList( (yyvsp[-1].pFactList) );
}
#line 1543 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 19:
#line 260 "scan-ops_pddl.y" /* yacc.c:1646  */
    { opserr( REQUIREM_EXPECTED, NULL ); }
#line 1549 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 20:
#line 262 "scan-ops_pddl.y" /* yacc.c:1646  */
    { 
  if ( !supported( (yyvsp[0].string) ) )
    {
      opserr( NOT_SUPPORTED, (yyvsp[0].string) );
      yyerror();
    }
}
#line 1561 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 23:
#line 277 "scan-ops_pddl.y" /* yacc.c:1646  */
    { 
  if ( !supported( (yyvsp[0].string) ) )
    {
      opserr( NOT_SUPPORTED, (yyvsp[0].string) );
      yyerror();
    }
}
#line 1573 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 25:
#line 290 "scan-ops_pddl.y" /* yacc.c:1646  */
    { opserr( TYPEDEF_EXPECTED, NULL ); }
#line 1579 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 26:
#line 292 "scan-ops_pddl.y" /* yacc.c:1646  */
    { 
  add_to_type_tree( (yyvsp[-1].pFactList), main_type_tree() );
  free_complete_FactList( (yyvsp[-1].pFactList) );
}
#line 1588 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 27:
#line 301 "scan-ops_pddl.y" /* yacc.c:1646  */
    { opserr( CONSTLIST_EXPECTED, NULL ); }
#line 1594 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 28:
#line 303 "scan-ops_pddl.y" /* yacc.c:1646  */
    { 
  gorig_constant_list = (yyvsp[-1].pFactList);
}
#line 1602 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 29:
#line 312 "scan-ops_pddl.y" /* yacc.c:1646  */
    { opserr( ACTION, NULL ); }
#line 1608 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 30:
#line 313 "scan-ops_pddl.y" /* yacc.c:1646  */
    { 
  scur_op = new_pl_operator( (yyvsp[0].string) );
}
#line 1616 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 31:
#line 317 "scan-ops_pddl.y" /* yacc.c:1646  */
    {
  scur_op->next = gloaded_ops;
  gloaded_ops = scur_op; 
}
#line 1625 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 32:
#line 326 "scan-ops_pddl.y" /* yacc.c:1646  */
    { scur_op->params = NULL; }
#line 1631 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 33:
#line 329 "scan-ops_pddl.y" /* yacc.c:1646  */
    {
  FactList * f;
  scur_op->params = (yyvsp[-1].pFactList);
  for (f = scur_op->params; f; f = f->next)
    {
      /* to be able to distinguish params from :VARS */
      scur_op->number_of_real_params++;
    }
}
#line 1645 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 35:
#line 345 "scan-ops_pddl.y" /* yacc.c:1646  */
    {
  FactList * f = NULL;

  /* add vars as parameters */
  if ( scur_op->params )
    {
      for( f = scur_op->params; f->next; f = f->next )
	{
	  /* empty, get to the end of list */
	}
      f->next = (yyvsp[-2].pFactList);
      f = f->next;
    }
  else
    {
      scur_op->params = (yyvsp[-2].pFactList);
    }
}
#line 1668 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 36:
#line 365 "scan-ops_pddl.y" /* yacc.c:1646  */
    { 
  scur_op->preconds = (yyvsp[0].pPlNode); 
}
#line 1676 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 38:
#line 371 "scan-ops_pddl.y" /* yacc.c:1646  */
    { 
  scur_op->effects = (yyvsp[0].pPlNode); 
}
#line 1684 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 40:
#line 383 "scan-ops_pddl.y" /* yacc.c:1646  */
    {
  /* returns new operator the name of which is AXIOM plus a number */
  scur_op = new_axiom_op_list(); 
}
#line 1693 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 41:
#line 388 "scan-ops_pddl.y" /* yacc.c:1646  */
    {
  scur_op->params = (yyvsp[-1].pFactList);
}
#line 1701 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 42:
#line 392 "scan-ops_pddl.y" /* yacc.c:1646  */
    { 
  scur_op->preconds = (yyvsp[0].pPlNode); 
}
#line 1709 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 43:
#line 396 "scan-ops_pddl.y" /* yacc.c:1646  */
    {
  PlNode * tmp;

  if (TRUE == sis_negated)
    {
      tmp = new_pl_node(NOT);
      tmp->sons = new_pl_node(ATOM);
      tmp->sons->atom = (yyvsp[0].pTokenList);
      sis_negated = FALSE;
    }
  else
    {
      tmp = new_pl_node(ATOM);
      tmp->atom = (yyvsp[0].pTokenList);
    }

  scur_op->effects = tmp;
}
#line 1732 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 44:
#line 415 "scan-ops_pddl.y" /* yacc.c:1646  */
    {
  /* Allowing complete "effects" is more than UCPOP and PDDL do,
     but this can easily be checked: the effect must be a single
     literal, otherwise axiom effects may become a little complicated */
  scur_op->next = gloaded_axioms;
  gloaded_axioms = scur_op;
  /* save axioms separately for now, after preprocessing they may
     be added to the other operators */
}
#line 1746 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 45:
#line 433 "scan-ops_pddl.y" /* yacc.c:1646  */
    { 
  if (TRUE == sis_negated)
    {
      (yyval.pPlNode) = new_pl_node(NOT);
      (yyval.pPlNode)->sons = new_pl_node(ATOM);
      (yyval.pPlNode)->sons->atom = (yyvsp[0].pTokenList);
      sis_negated = FALSE;
    }
  else
    {
      (yyval.pPlNode) = new_pl_node(ATOM);
      (yyval.pPlNode)->atom = (yyvsp[0].pTokenList);
    }
}
#line 1765 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 46:
#line 449 "scan-ops_pddl.y" /* yacc.c:1646  */
    { 
  (yyval.pPlNode) = new_pl_node(AND);
  (yyval.pPlNode)->sons = (yyvsp[-1].pPlNode);
}
#line 1774 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 47:
#line 455 "scan-ops_pddl.y" /* yacc.c:1646  */
    { 
  (yyval.pPlNode) = new_pl_node(OR);
  (yyval.pPlNode)->sons = (yyvsp[-1].pPlNode);
}
#line 1783 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 48:
#line 461 "scan-ops_pddl.y" /* yacc.c:1646  */
    { 
  (yyval.pPlNode) = new_pl_node(NOT);
  (yyval.pPlNode)->sons = (yyvsp[-1].pPlNode);
}
#line 1792 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 49:
#line 467 "scan-ops_pddl.y" /* yacc.c:1646  */
    { 
  PlNode * np = new_pl_node(NOT);
  np->sons = (yyvsp[-2].pPlNode);
  np->next = (yyvsp[-1].pPlNode);

  (yyval.pPlNode) = new_pl_node(OR);
  (yyval.pPlNode)->sons = np;
}
#line 1805 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 50:
#line 479 "scan-ops_pddl.y" /* yacc.c:1646  */
    { 
  /* The typed_list_variable returns a FactList with two-item TokenLists, 
     the first item is the variable and the second item its type.
     We now have to split off this FactList into a PlNode for each 
     variable-type TokenList. */
  FactList * tl = (yyvsp[-3].pFactList), *t1;
  PlNode * pln1;
  PlNode * pln2;

  pln1 = new_pl_node(EX);
  pln1->atom = tl->item;
  (yyval.pPlNode) = pln1;

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
  pln1->sons = (yyvsp[-1].pPlNode);

  t1 = tl->next;
  while ( TRUE ) {
    free ( tl );
    if ( !t1 ) break;
    tl = t1;
    t1 = tl->next;
  }

}
#line 1846 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 51:
#line 519 "scan-ops_pddl.y" /* yacc.c:1646  */
    { 
  /* This will be handled exactly as the ex-quantor case, s.o. */
  FactList * tl = (yyvsp[-3].pFactList), *t1;
  PlNode * pln1;
  PlNode * pln2;
  
  pln1 = new_pl_node(ALL);
  pln1->atom = tl->item;
  (yyval.pPlNode) = pln1;

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
  pln1->sons = (yyvsp[-1].pPlNode);

  t1 = tl->next;
  while ( TRUE ) {
    free ( tl );
    if ( !t1 ) break;
    tl = t1;
    t1 = tl->next;
  }

}
#line 1883 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 52:
#line 556 "scan-ops_pddl.y" /* yacc.c:1646  */
    {
  (yyval.pPlNode) = NULL;
}
#line 1891 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 53:
#line 561 "scan-ops_pddl.y" /* yacc.c:1646  */
    {
  (yyvsp[-1].pPlNode)->next = (yyvsp[0].pPlNode);
  (yyval.pPlNode) = (yyvsp[-1].pPlNode);
}
#line 1900 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 54:
#line 573 "scan-ops_pddl.y" /* yacc.c:1646  */
    { 
  if (TRUE == sis_negated)
    {
      (yyval.pPlNode) = new_pl_node(NOT);
      (yyval.pPlNode)->sons = new_pl_node(ATOM);
      (yyval.pPlNode)->sons->atom = (yyvsp[0].pTokenList);
      sis_negated = FALSE;
    }
  else
    {
      (yyval.pPlNode) = new_pl_node(ATOM);
      (yyval.pPlNode)->atom = (yyvsp[0].pTokenList);
    }
}
#line 1919 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 55:
#line 589 "scan-ops_pddl.y" /* yacc.c:1646  */
    { 
  (yyval.pPlNode) = new_pl_node(AND);
  (yyval.pPlNode)->sons = (yyvsp[-1].pPlNode);
}
#line 1928 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 56:
#line 595 "scan-ops_pddl.y" /* yacc.c:1646  */
    { 
  (yyval.pPlNode) = new_pl_node(NOT);
  (yyval.pPlNode)->sons = (yyvsp[-1].pPlNode);
}
#line 1937 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 57:
#line 603 "scan-ops_pddl.y" /* yacc.c:1646  */
    { 
  /* This will be handled exactly as quantors in the goals part, s.o. */
  FactList * fl = (yyvsp[-3].pFactList), *f1;
  PlNode * pln1;
  PlNode * pln2;
  
  pln1 = new_pl_node(ALL);
  pln1->atom = fl->item;
  (yyval.pPlNode) = pln1;

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
  pln1->sons = (yyvsp[-1].pPlNode);

  f1 = fl->next;
  while ( TRUE ) {
    free( fl );
    if ( !f1 ) break;
    fl = f1;
    f1 = fl->next;
  }

}
#line 1974 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 58:
#line 637 "scan-ops_pddl.y" /* yacc.c:1646  */
    {
  /* This will be conditional effects in IPP representation, but here
     a formula like (WHEN p q) will be saved as:
      [WHEN]
      [sons]
       /  \
     [p]  [q]
     That means, the first son is p, and the second one is q. */
  (yyval.pPlNode) = new_pl_node(WHEN);
  (yyvsp[-2].pPlNode)->next = (yyvsp[-1].pPlNode);
  (yyval.pPlNode)->sons = (yyvsp[-2].pPlNode);
}
#line 1991 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 59:
#line 653 "scan-ops_pddl.y" /* yacc.c:1646  */
    { 
  (yyval.pPlNode) = NULL; 
}
#line 1999 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 60:
#line 658 "scan-ops_pddl.y" /* yacc.c:1646  */
    {
  (yyvsp[-1].pPlNode)->next = (yyvsp[0].pPlNode);
  (yyval.pPlNode) = (yyvsp[-1].pPlNode);
}
#line 2008 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 61:
#line 669 "scan-ops_pddl.y" /* yacc.c:1646  */
    { 
  (yyval.pTokenList) = (yyvsp[-1].pTokenList);
  sis_negated = TRUE;
/*   $$ = new_token_list(); */
/*   $$->item = new_token( strlen($3->item)+2 ); */
/*   sprintf($$->item, "!%s", $3->item); */
/*   $$->next = $3->next; */
}
#line 2021 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 62:
#line 679 "scan-ops_pddl.y" /* yacc.c:1646  */
    {
  (yyval.pTokenList) = (yyvsp[0].pTokenList);
}
#line 2029 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 63:
#line 687 "scan-ops_pddl.y" /* yacc.c:1646  */
    { 
  (yyval.pTokenList) = new_token_list();
  (yyval.pTokenList)->item = (yyvsp[-2].pstring);
  (yyval.pTokenList)->next = (yyvsp[-1].pTokenList);
}
#line 2039 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 64:
#line 697 "scan-ops_pddl.y" /* yacc.c:1646  */
    { (yyval.pTokenList) = NULL; }
#line 2045 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 65:
#line 700 "scan-ops_pddl.y" /* yacc.c:1646  */
    {
  (yyval.pTokenList) = new_token_list();
  (yyval.pTokenList)->item = (yyvsp[-1].pstring);
  (yyval.pTokenList)->next = (yyvsp[0].pTokenList);
}
#line 2055 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 66:
#line 710 "scan-ops_pddl.y" /* yacc.c:1646  */
    { 
  (yyval.pstring) = new_token( strlen((yyvsp[0].string))+1 );
  strcpy( (yyval.pstring), (yyvsp[0].string) );
}
#line 2064 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 67:
#line 716 "scan-ops_pddl.y" /* yacc.c:1646  */
    { 
  (yyval.pstring) = new_token( strlen((yyvsp[0].string))+1 );
  strcpy( (yyval.pstring), (yyvsp[0].string) );
}
#line 2073 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 68:
#line 725 "scan-ops_pddl.y" /* yacc.c:1646  */
    {
  (yyval.pTokenList) = new_token_list();
  (yyval.pTokenList)->item = new_token( strlen((yyvsp[0].string))+1 );
  strcpy( (yyval.pTokenList)->item, (yyvsp[0].string) );
}
#line 2083 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 69:
#line 732 "scan-ops_pddl.y" /* yacc.c:1646  */
    {
  (yyval.pTokenList) = new_token_list();
  (yyval.pTokenList)->item = new_token( strlen((yyvsp[-1].string))+1 );
  strcpy( (yyval.pTokenList)->item, (yyvsp[-1].string) );
  (yyval.pTokenList)->next = (yyvsp[0].pTokenList);
}
#line 2094 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 70:
#line 743 "scan-ops_pddl.y" /* yacc.c:1646  */
    { 
  (yyval.pstring) = new_token( strlen((yyvsp[0].string))+1 );
  strcpy( (yyval.pstring), (yyvsp[0].string) );
}
#line 2103 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 71:
#line 749 "scan-ops_pddl.y" /* yacc.c:1646  */
    { 
  (yyval.pstring) = new_token( strlen(EQ_STR)+1 );
  strcpy( (yyval.pstring), EQ_STR );
}
#line 2112 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 72:
#line 758 "scan-ops_pddl.y" /* yacc.c:1646  */
    { (yyval.pFactList) = NULL; }
#line 2118 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 73:
#line 761 "scan-ops_pddl.y" /* yacc.c:1646  */
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
  for ( t = (yyvsp[-2].pTokenList); t; t = t->next )
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
      for ( t = (yyvsp[-2].pTokenList); t; t = t->next )
	{
	  if ((tt = find_branch( t->item, main_type_tree())))
	    {
	      *st = new_type_tree_list( NULL );
	      (*st)->item = tt;
	      st = &((*st)->next);
	    }
	}  
    }


  t = (yyvsp[-2].pTokenList);
  while ( t ) {
    t1 = t->next;
    free( t->item );
    free( t );
    t = t1;
  }

  /* now do the simple stuff: return a name with a (quite complicated)
     type */
  (yyval.pFactList) = new_fact_list();
  (yyval.pFactList)->item = new_token_list();
  (yyval.pFactList)->item->item = new_token( strlen((yyvsp[-4].string))+1 );
  strcpy( (yyval.pFactList)->item->item, (yyvsp[-4].string) );
  (yyval.pFactList)->item->next = new_token_list();
  (yyval.pFactList)->item->next->item = s;
  (yyval.pFactList)->next = (yyvsp[0].pFactList);
}
#line 2184 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 74:
#line 824 "scan-ops_pddl.y" /* yacc.c:1646  */
    {
  (yyval.pFactList) = new_fact_list();
  (yyval.pFactList)->item = new_token_list();
  (yyval.pFactList)->item->item = new_token( strlen((yyvsp[-2].string))+1 );
  strcpy( (yyval.pFactList)->item->item, (yyvsp[-2].string) );
  (yyval.pFactList)->item->next = new_token_list();
  (yyval.pFactList)->item->next->item = new_token( strlen((yyvsp[-1].string))+1 );
  strcpy( (yyval.pFactList)->item->next->item, (yyvsp[-1].string) );
  (yyval.pFactList)->next = (yyvsp[0].pFactList);
}
#line 2199 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 75:
#line 836 "scan-ops_pddl.y" /* yacc.c:1646  */
    {
  (yyval.pFactList) = new_fact_list();
  (yyval.pFactList)->item = new_token_list();
  (yyval.pFactList)->item->item = new_token( strlen((yyvsp[-1].string))+1 );
  strcpy( (yyval.pFactList)->item->item, (yyvsp[-1].string) );
  (yyval.pFactList)->item->next = new_token_list();
  if ( (yyvsp[0].pFactList) )   /* another element (already typed) is following */
    {
      char * s = (yyvsp[0].pFactList)->item->next->item;
      (yyval.pFactList)->item->next->item = new_token(strlen(s) + 1);
      strcpy( (yyval.pFactList)->item->next->item, s ); /* same type as the next one */
      (yyval.pFactList)->next = (yyvsp[0].pFactList);
    }
  else /* no further element - it must be an untyped list */
    {
      (yyval.pFactList)->item->next->item = new_token( strlen(STANDARD_TYPE)+1 );
      strcpy( (yyval.pFactList)->item->next->item, STANDARD_TYPE );
      (yyval.pFactList)->next = (yyvsp[0].pFactList);
    }
}
#line 2224 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 76:
#line 861 "scan-ops_pddl.y" /* yacc.c:1646  */
    { (yyval.pFactList) = NULL; }
#line 2230 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 77:
#line 864 "scan-ops_pddl.y" /* yacc.c:1646  */
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
  for ( t = (yyvsp[-2].pTokenList); t; t = t->next )
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
      for ( t = (yyvsp[-2].pTokenList); t; t = t->next )
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

  t = (yyvsp[-2].pTokenList);
  while ( t ) {
    t1 = t->next;
    free( t->item );
    free( t );
    t = t1;
  }

  /* now do the simple stuff: return a name with a (quite complicated)
     type */
  (yyval.pFactList) = new_fact_list();
  (yyval.pFactList)->item = new_token_list();
  (yyval.pFactList)->item->item = new_token( strlen((yyvsp[-4].string))+1 );
  strcpy( (yyval.pFactList)->item->item, (yyvsp[-4].string) );
  (yyval.pFactList)->item->next = new_token_list();
  (yyval.pFactList)->item->next->item = s;
  (yyval.pFactList)->next = (yyvsp[0].pFactList);
}
#line 2294 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 78:
#line 925 "scan-ops_pddl.y" /* yacc.c:1646  */
    {
  (yyval.pFactList) = new_fact_list();
  (yyval.pFactList)->item = new_token_list();
  (yyval.pFactList)->item->item = new_token( strlen((yyvsp[-2].string))+1 );
  strcpy( (yyval.pFactList)->item->item, (yyvsp[-2].string) );
  (yyval.pFactList)->item->next = new_token_list();
  (yyval.pFactList)->item->next->item = new_token( strlen((yyvsp[-1].string))+1 );
  strcpy( (yyval.pFactList)->item->next->item, (yyvsp[-1].string) );
  (yyval.pFactList)->next = (yyvsp[0].pFactList);
}
#line 2309 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;

  case 79:
#line 937 "scan-ops_pddl.y" /* yacc.c:1646  */
    {
  (yyval.pFactList) = new_fact_list();
  (yyval.pFactList)->item = new_token_list();
  (yyval.pFactList)->item->item = new_token( strlen((yyvsp[-1].string))+1 );
  strcpy( (yyval.pFactList)->item->item, (yyvsp[-1].string) );
  (yyval.pFactList)->item->next = new_token_list();
  if ( (yyvsp[0].pFactList) )   /* another element (already typed) is following */
    {
      char* s = (yyvsp[0].pFactList)->item->next->item;
      int   l = strlen( s );
      (yyval.pFactList)->item->next->item = new_token( l+1 );
      strcpy( (yyval.pFactList)->item->next->item, s ); /* same type as the next one */
      (yyval.pFactList)->next = (yyvsp[0].pFactList);
    }
  else /* no further element - it must be an untyped list */
    {
      (yyval.pFactList)->item->next->item = new_token( strlen(STANDARD_TYPE)+1 );
      strcpy( (yyval.pFactList)->item->next->item, STANDARD_TYPE );
      (yyval.pFactList)->next = (yyvsp[0].pFactList);
    }
}
#line 2335 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
    break;


#line 2339 "scan-ops_pddl.tab.c" /* yacc.c:1646  */
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
#line 960 "scan-ops_pddl.y" /* yacc.c:1906  */

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
