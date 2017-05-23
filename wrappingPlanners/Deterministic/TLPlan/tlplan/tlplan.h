/* tlplan.h

Copyright C, 1997 - 2002, F. Bacchus

Temporal Logic Planner Main Routine

Log

Name		Date	Description

M Ady		970118	First Version

*/

#ifndef __TLPLAN_H
#define __TLPLAN_H

/* Windows types (defined here for now) */

#define FALSE 0
#define TRUE 1
#define NOT_FALSE 2						/* special constant for current values */
#define BOOL int
#define BOOLP int *
#define DWORD unsigned int

#ifdef WIN32
#define DECLSPECX __declspec(dllexport)
#ifdef BUILDING_TLLIB
#define DECLSPEC __declspec(dllexport)
#else
#define DECLSPEC __declspec(dllimport)
#endif
#else
#define DECLSPECX
#define DECLSPEC
#define _MAX_PATH 256
#endif

/* global definitions */

#define MAX(a,b) (((a)>(b))?(a):(b))
#define MIN(a,b) (((a)<(b))?(a):(b))

#undef NULL
#define NULL 0

#define SYMTABSIZE 200					/* initial size of symbol hash table */
#define STRINGTABSIZE 800				/* initial size of string hash table */
#define MAXSYMBOLS 20000					/* maximum number of symbol-info entries */
#define MAXMACROOPERATORS 10			/* maximum number of macro-info entries */

/* pddl support */

#define PDDL_UNSUPPORTED		0		/* don't do special PDDL processing */
#define PDDL_IGNORE_METRIC		1		/* PDDL processing without metric support */
#define PDDL_PRIORITY_METRIC	2		/* PDDL processing with priority metric support */
#define PDDL_COST_METRIC		3		/* PDDL processing with costs metric support */

/* plan dispositions */

#define PLAN_ALIVE		0				/* plan is on open list */
#define PLAN_PRUNED		1				/* plan pruned by control formula */
#define PLAN_FILTERED	2				/* plan discarded by filter routine */
#define PLAN_LIMIT		3				/* plan unsearched due to depth limit */


#define HSP_COST_FROM_IGNORED_OPERATOR  -2  /* (arbitrary) cost assigned to added/deleted fluents when coming from ignored actions in the HSP heuristic. MUST BE NEGATIVE. */


/* instrumentation macros */

#define INSTRUMENT 0

#if INSTRUMENT
#include "gate.h"
#define ENTER(a,b) EnterGate(a,b)
#define EXIT(a) ExitGate(a)
#else
#define ENTER(a,b)
#define EXIT(a)
#endif /* INSTRUMENT */

/* timing macros */

#define TIMING 0

#if TIMING
#define StartTimer(x) \
	if(bTimingStatistics) \
		x[0]=GetInternalRunTime()
#define StopTimer(x) \
	if(bTimingStatistics) \
		x[1]+=GetInternalRunTime()-x[0];
#else // TIMING
#define StartTimer(x)
#define StopTimer(x)
#endif // TIMING

#define IdentName(pf) ((pf)->psName?(pf)->psName:SaveName(pf))
#define GetName(pf,ac) ((pf)->psName?(pf)->psName:MakeName(pf,ac))

/* pre-hashed strings (add new entries to end of table!) */

#define STRING_LPAREN						0
#define STRING_RPAREN						1
#define STRING_ASSIGN						2
#define STRING_LT							3
#define STRING_LE							4
#define STRING_EQ							5
#define STRING_GT							6
#define STRING_GE							7
#define STRING_ASTAR						8
#define STRING_ACTION_PRIORITY				9
#define STRING_ADD							10
#define STRING_ALWAYS						11
#define STRING_AND							12
#define STRING_ATEMPORAL					13
#define STRING_BEST_ACTION					14
#define STRING_CLASSIC						15
#define STRING_COST							16
#define STRING_DEL							17
#define STRING_DELTA						18
#define STRING_DOMAIN_GLOBAL				19
#define STRING_DURATION						20
#define STRING_EVENTUALLY					21
#define STRING_EXISTSX						22
#define STRING_EXISTS						23
#define STRING_EXTENDED						24
#define STRING_FALSE						25
#define STRING_FORALL						26
#define STRING_FUNCTION						27
#define STRING_GENERATOR					28
#define STRING_GOAL							29
#define STRING_IF_THEN_ELSE					30
#define STRING_IMPLIES						31
#define STRING_ISPEC						32
#define STRING_NEXT							33
#define STRING_NO_PLAN						34
#define STRING_NOT							35
#define STRING_OPTIMAL_COST					36
#define STRING_OR							37
#define STRING_PLAN_COST					38
#define STRING_PLAN_LENGTH					39
#define STRING_SET_PLAN_NAME				40
#define STRING_DEPTH_BEST_FIRST				41
#define STRING_PRE							42
#define STRING_PREDICATE					43
#define STRING_PREVIOUS						44
#define STRING_PRINT						45
#define STRING_PRINT_WORLD					46
#define STRING_PRIORITY						47
#define STRING_RESOURCES					48
#define STRING_SEARCH_GLOBAL				49
#define STRING_OPERATOR						50
#define STRING_DEF_DEFINED_GENERATOR		51
#define STRING_GET_GOAL_ADDENDUM			52
#define STRING_T_ALWAYS						53
#define STRING_T_EVENTUALLY					54
#define STRING_T_UNTIL						55
#define STRING_TEMPORAL						56
#define STRING_TRUE							57
#define STRING_UNKNOWN						58
#define STRING_UNTIL						59
#define STRING_WORLD_GLOBAL					60
#define STRING_WORLD_HEURISTIC_RANK			61
#define STRING_WORLD_NUMBER					62
#define STRING_XOR							63
#define STRING_BREADTH_FIRST				64
#define STRING_BREADTH_FIRST_PRIORITY		65
#define STRING_BEST_FIRST					66
#define STRING_DEPTH_FIRST					67
#define STRING_DEPTH_FIRST_PRIORITY			68
#define STRING_DEPTH_FIRST_NO_BACKTRACKING	69
#define STRING_PLUS							70
#define STRING_MINUS						71
#define STRING_STAR							72
#define STRING_SLASH						73
#define STRING_MOD							74
#define STRING_MAX							75
#define STRING_MIN							76
#define STRING_EXPT							77
#define STRING_SQRT							78
#define STRING_ABS							79
#define STRING_EXP							80
#define STRING_LOG							81
#define STRING_RAND							82
#define STRING_SEED							83
#define STRING_ROUND						84
#define STRING_INT							85
#define STRING_FLOOR						86
#define STRING_CEIL							87
#define STRING_POS_INT						88
#define STRING_LT_POS_INT					89
#define STRING_IS_BETWEEN					90
#define STRING_PERMUTE						91
#define STRING_EXIT							92
#define STRING_RANDOM						93
#define STRING_CLEAR_WORLD_SYMBOLS			94
#define STRING_DECLARE_DEFINED_SYMBOLS		95
#define STRING_DECLARE_DESCRIBED_SYMBOLS	96
#define STRING_DECLARE_EXTERNAL_SYMBOLS		97
#define STRING_DECLARE_GLOBAL_VARIABLES		98
#define STRING_MACRO						99
#define STRING_DEFINE						100
#define STRING_DEF_ADL_OPERATOR				101
#define STRING_DEF_DEFINED_FUNCTION			102
#define STRING_DEF_DEFINED_PREDICATE		103
#define STRING_DEF_DOMAIN					104
#define STRING_DEF_STRIPS_OPERATOR			105
#define STRING_GET_HEURISTIC_FN				106
#define STRING_PRINT_WORLD_LIST				107
#define STRING_GET_PRIORITY_FN				108
#define STRING_GET_SEARCH_LIMIT				109
#define STRING_GET_SEARCH_STRATEGY			110
#define STRING_GET_TL_CONTROL				111
#define STRING_GET_TRACE_LEVEL				112
#define STRING_LIST_DOMAINS					113
#define STRING_LOAD_DOMAIN					114
#define STRING_LOAD_FILE					115
#define STRING_PLAN							116
#define STRING_PRINT_PLAN_LIST				117
#define STRING_RESET_DOMAINS				118
#define STRING_RESET_HEURISTIC_FN			119
#define STRING_RESET_PRINT_WORLD_FN			120
#define STRING_RESET_PRIORITY_FN			121
#define STRING_RESET_SEARCH_LIMIT			122
#define STRING_RESET_SEARCH_STRATEGY		123
#define STRING_RESET_TL_CONTROL				124
#define STRING_SET_CONTROL					125
#define STRING_SET_HEURISTIC_FN				126
#define STRING_SET_INITIAL_FACTS			127
#define STRING_SET_INITIAL_WORLD			128
#define STRING_SET_INITIALIZATION_SEQUENCE	129
#define STRING_SET_GOAL						130
#define STRING_SET_GOAL_TYPE				131
#define STRING_SET_PRINT_WORLD_FN			132
#define STRING_SET_PRIORITY_FN				133
#define STRING_SET_SEARCH_LIMIT				134
#define STRING_SET_SEARCH_STRATEGY			135
#define STRING_SET_STATISTICS_FILE			136
#define STRING_SET_TL_CONTROL				137
#define STRING_SET_TRACE_LEVEL				138
#define STRING_VERBOSE_OFF					139
#define STRING_VERBOSE_ON					140
#define STRING_ACTION_NAME					141
#define STRING_ACTION_COST					142
#define STRING_ACTION_DURATION				143
#define STRING_LOCAL_VARS					144
#define STRING_SELECT_INITIAL_WORLD			145
#define STRING_SELECT_FINAL_WORLD			146
#define STRING_SELECT_NEXT_WORLD			147
#define STRING_SELECT_PREVIOUS_WORLD		148
#define STRING_CURRENT						149
#define STRING_WRITE						150
#define STRING_APPEND						151
#define STRING_READ							152
#define STRING_OPEN_FILE					153
#define STRING_CLOSE_FILE					154
#define STRING_FIBONACCI					155
#define STRING_MS							156
#define STRING_ISAAC						157
#define STRING_SET_RNG						158
#define STRING_RESET_RNG					159
#define STRING_GET_RNG						160
#define STRING_DEF_DEFINED_MACRO			161
#define STRING_ASSIGN_APPEND				162
#define STRING_REDIRECT						163
#define STRING_PLAN_DURATION				164
#define STRING_PLAN_STATUS					165
#define STRING_WORLDS_GENERATED				166
#define STRING_WORLDS_SEARCHED				167
#define STRING_WORLDS_PRUNED				168
#define STRING_WORLDS_DISCARDED				169
#define STRING_WORLDS_UNEXAMINED			170
#define STRING_PLAN_CPU_TIME				171
#define STRING_SET_SEARCH_DEPTH_LIMIT		172
#define STRING_GET_SEARCH_DEPTH_LIMIT		173
#define STRING_SEARCH_MAX_DEPTH				174
#define STRING_SET_SEARCH_HEURISTIC_LIMIT	175
#define STRING_GET_SEARCH_HEURISTIC_LIMIT	176
#define STRING_SEARCH_MAX_HEURISTIC			177
#define STRING_RESET_SEARCH_DEPTH_LIMIT		178
#define STRING_RESET_SEARCH_HEURISTIC_LIMIT	179
#define STRING_OK							180
#define STRING_GET_CPU_TIME					181
#define STRING_HEURISTIC_FN					182
#define	STRING_SET_GOAL_ADDENDUM			183
#define STRING_RESET_GOAL_ADDENDUM			184
#define STRING_GET_PLAN_NAME				185
#define STRING_RESET_PLAN_NAME				186
#define STRING_BINDING						187
#define STRING_MODIFY_WORLD					188
#define STRING_USER_ABORT					189
#define STRING_ENABLE						190
#define STRING_DISABLE						191
#define STRING_CYCLE_CHECKING				192
#define STRING_TIMING_STATISTICS			193
#define STRING_NO_CYCLE_CHECK				194
#define STRING_IN_THE_SET					195
#define STRING_BACKTRACKING					196
#define STRING_PRUNING_ALL_SUCCESSORS		197
#define STRING_CONCURRENT_PLANNING			198
#define STRING_WAIT_FOR_NEXT_EVENT			199
#define STRING_DELAYED_ACTION				200
#define STRING_GLOBAL_DELAYED_ACTION		201
#define STRING_LOAD_PLAN					202
#define STRING_MAKE_LITERAL					203
#define STRING_SET_GOAL_SEQUENCE			204
#define STRING_SET_GOAL_FORMULA				205
#define STRING_CURRENT_TIME					206
#define STRING_INHIBIT_DELAYED_ACTION		207
#define STRING_PLUS_EQ						208
#define STRING_MINUS_EQ						209
#define STRING_PRINT_DELTA_TIME				210
#define STRING_REWRITABLE					211
#define STRING_PDDL_SUPPORT					212
#define STRING_DECLARE_MACRO_OPERATORS		213
#define STRING_CLEAR_OPERATORS				214
#define STRING_NEAREST_FIRST				215
#define STRING_DECLARE_ELIDED_OPERATORS		216
#define STRING_LOAD_PDDL_PROBLEM			217
#define STRING_CLEAR_EVENT_QUEUE			218
#define STRING_CLOSEST_FIRST				219
#define STRING_UPDATE_WORLD					220
#define STRING_LOWEST_FIRST					221
#define STRING_PRINT_PDDL_PLAN				222
#define STRING_REACHABLE_EVENT				223
#define STRING_ALL_PAIRS_SHORTEST_PATH		224
#define STRING_NEAREST_FIRST_EX				225
#define STRING_CLOSEST_FIRST_EX				226
#define STRING_CONDITIONAL_EXP				227
#define STRING_SDEL				228
#define STRING_GOAL_DISTANCE				229
#define STRING_SET_METRIC_FN                    230
#define STRING_SET_PREFERENCES                  231
#define STRING_SET_POSTACTION_SEQUENCE          232
#define STRING_SET_GOAL_MULTIPLIER              233
#define STRING_SET_PREFERENCE_MULTIPLIER        234
#define STRING_SET_METRIC_MULTIPLIER            235
#define STRING_SET_OPTIMISTIC_METRIC_MULTIPLIER            236
#define STRING_SET_OPTIMISTIC_METRIC_FN                    237
#define STRING_SET_HEURISTIC_EXPONENT                      238
#define STRING_SET_METRIC_DISCOUNT                         239
#define STRING_SET_PLAN_TIMEOUT                            240
#define STRING_PRINT_PDDL3_PLAN                            241
#define STRING_WORLD_COUNTER                            242
#define STRING_SET_COUNT_SAT_PREFS_FN                   243
#define STRING_GET_BEST_RELAXED_METRIC                   244
#define STRING_SET_BEST_METRIC_MULTIPLIER               245
#define STRING_SET_DISCOUNTED_METRIC_MULTIPLIER               246
#define STRING_CLOSEDLIST_LENGTH                        247
#define STRING_GOAL_DISTANCE_NEG				248
#define STRING_SET_MERGE_FN_TYPE				249
#define STRING_SET_QUALPREFERENCE_VALUE_FN			250
#define STRING_SET_QUALPREFERENCE_NUMBER				251
#define STRING_NUMERIC                                          252
#define STRING_QUALPREFERENCE_VALUE_FIRST                    253
#define STRING_QUALPREFERENCE_EASY_FIRST                     254
#define STRING_GET_BEST_PREFERENCE_VALUE                     255
#define STRING_GOAL_DISTANCE_QUALPREFERENCES                     256
#define STRING_GOAL_QUALPREFERENCE_VALUE_FIRST                    257
#define STRING_GOAL_QUALPREFERENCE_EASY_FIRST                     258
#define STRING_SET_BDF_PREFERENCE      259
#define STRING_NO_RELAXING             260
#define	STRING_DEF_ADL_H_OPERATOR      261
#define	STRING_HSP_IGNORE      262
#define STRING_UNREL_EFFECTS   263
#define STRING_UNOT            264
#define STRING_GET_NO_WORLD_FIXED_POINTS    265 
#define STRING_GET_NO_REAL_FIXED_POINTS 266 
#define STRING_GOAL_DISTANCE_HSP_UNRELEFFS 267
#define STRING_GOAL_DISTANCE_FF_UNRELEFFS 268
#define STRING_VERBOSE_SHOW_EXPANDED_PLANS_ON 269
#define STRING_VERBOSE_SHOW_EXPANDED_PLANS_OFF 270
#define STRING_VERBOSE_SHOW_EXPANDED_WORLDS_ON 271
#define STRING_VERBOSE_SHOW_EXPANDED_WORLDS_OFF 272
#define STRING_VERBOSE_FF_SHOW_RELAXED_PLANS_ON 273
#define STRING_VERBOSE_FF_SHOW_RELAXED_PLANS_OFF 274
#define STRING_VERBOSE_FF_SHOW_RELAXED_WORLDS_ON 275
#define STRING_VERBOSE_FF_SHOW_RELAXED_WORLDS_OFF 276
#define STRING_BEST_FIRST_DELAYED_HEURISTIC 277
#define STRING_BEST_FIRST_HELPFUL_LIST 278
#define STRING_SET_HELPFUL_LIST_PRIORITY 279
#define STRING_SET_DISCOUNTED_METRIC_PRIORITY 280
#define STRING_SET_OPTIMISTIC_METRIC_PRIORITY 281
#define STRING_SET_BEST_RELAXED_METRIC_PRIORITY 282
#define STRING_SET_PREFERENCE_DISTANCE_PRIORITY 283
#define STRING_SET_METRIC_PRIORITY 284
#define STRING_GOAL_DISTANCE_FF_PDDL_PREFS 285
#define STRING_SET_TOTAL_PREFS_FN 286
#define	STRING_SET_HEURISTIC_PRESERVED_PROPERTY 287
#define STRING_WATCH_HEURISTIC_PRESERVE 288
#define STRING_SET_GOAL_OCCLUSION_MULTIPLIER 289
#define STRING_SET_PRECOND_OCCLUSION_MULTIPLIER 290
#define STRING_BEST_FIRST_PREFER_LONGER 291
#define STRING_PRINT_STRING 292
#define STRING_FF_RELPLAN_SIMPLIFICATION_ON 293
#define STRING_FF_RELPLAN_SIMPLIFICATION_OFF 294


#define STRING__MAX		   			295	/* 1 more than the last string constant */

/* forward references (ansi) */

struct Binding;
struct LinearPlan;

/* global structures */

typedef union LitVal					/* literal value */
{
  char *psString;						/* string pointer */
  int nInteger;						/* integer value */
  double dfFloat;						/* float value */
  struct List *plList;				/* list pointer */
  struct BTree *pbtTree;				/* tree pointer */
  struct HBTree *phbtTree;                        /* h-tree pointer */
  struct ISpec *piISpec;				/* interval pointer */
  struct Symbol *psSymbol;			/* symbol table pointer */
  struct SymbolInfo *psiSymbolInfo;	/* symbol information table pointer */
  struct ArrayInfo *paiArrayInfo;		/* array value pointer */
  struct Cell *pcFormula;				/* pointer to formula */
  struct Cell *(*pfFunction)(struct Cell *);	/* function pointer */
  struct Operator *poOperator;		/* pointer to operator */
  struct OperInstance *poiOperInstance;                /* an occurrence of an operator's instantiation at a certain depth */
  struct FFActionInstance *pffActionInstance;
}LITVAL, *LITVALP;

typedef struct OperInstance {
  int nOperNumber;             /* operator number  */
  int nOperInstance;           /* number of operator instantiation  */
  int nDepth;                  /* depth of the action */
  double dHspCost;                /* HSP cost incurred for this instanca */  
  struct List  *pcSupFacts;            /* supporting fact */
  struct Binding *pbBindings;  /* operator bindings */
  struct Cell *pcActionInstance;    /* actual instance as a formula (only used in verbose modes) */
  struct List *plAddedFacts;   /* List of added facts */
  struct List *plFlipUndo; /* Stores maximum depth an added fact was added before 
			      this action added it.  Used in case of backtracking */
  struct List *plFlipUndoMin; /* Stores minimum depth an added fact was added before 
				 this action added it.  Used in case of backtracking */

  int nAddedFacts;  /* number of added facts */

} OPERINSTANCE, *OPERINSTANCEP;

struct FFActionInstance
{
  int nFFOperInstance; /* number of operator instantiation that added this fact */
  struct List *plSupFacts; /* supporting facts */
  struct Binding *pbBindings;    /* bindings for this instance */
  int nFFDepth; 
};

typedef struct Cell
{
	struct Cell *pcNext;				/* pointer to next */
	struct Formula *pfForm;				/* pointer to value */
}CELL, *CELLP;


typedef struct List						/* list element (atom) */
{
	struct List *plNext;				/* pointer to next */
	char *psName;						/* string pointer */
	int nType;							/* atomic type */
	union LitVal uValue;				/* literal value */
}LIST, *LISTP;

typedef struct HCell                /* a cell for heuristic information */
{
  struct HCell *pcNext;
  double hsp_cost;


  OPERINSTANCEP poiOperInstance;

  //  LISTP pcFFSupFacts;           /* supporting facts (for FF heuristic) */
 
  
  // struct Binding * pbBindings;    /* bindings for this instance */
  // int nFFOperNumber; /* number of operator that added this fact */
  //int nFFOperInstance; /* number of operator instantiation that added this fact */
  //int nFFDepth; 
  // CELLP pcFFActionInstance;
} HCELL, *HCELLP;


typedef struct Formula					/* formula or term */
{
	char *psName;						/* name */
	int nType;							/* atomic type of value */
        int nHashPos;                                   /* Position in the hash table */
	union LitVal uValue;				/* function or literal value */
	struct Cell *pcVars;				/* pointer to free variable list */
	struct Cell *pcGenLit;				/* generator literal or bindings list*/
	struct Cell *pcArgs;				/* pointer to argument list */
	struct Action *paAction;			/* pointer to action table */
}FORMULA, *FORMULAP;

typedef struct Symbol					/* symbol table entries */
{
	char *psName;						/* symbol name */
	int nType;							/* symbol type */
	union LitVal uValue;				/* symbol value */
}SYMBOL, *SYMBOLP;

typedef struct Binding					/* variable binding */
{
	struct Binding *pbNext;				/* pointer to next binding */
	struct GContext *pgcContext;		/* pointer to associated context block */
	CELLP pcVar;						/* bound variable */
	CELLP pcVal;						/* its value */
}BINDING, *BINDINGP;

typedef struct BTree					/* binary tree structure */
{
	struct BTree *pbtNext;				/* pointer to next (for list operations) */
	CELLP pcKey;						/* search key (list of predicate or function arguments) */
	CELLP pcValue;						/* function value (functions only) */
	struct BTree *pbtLeft;				/* pointer to less than */
	struct BTree *pbtRight;				/* pointer to greater than */
	struct BTree **apbtOwner;			/* pointer to owning world (not parent!) */
}BTREE, *BTREEP;

typedef struct HBTree					/* binary tree structure for computing heuristics*/
{
	struct HBTree *pbtNext;				/* pointer to next (for list operations) */
	CELLP pcKey;						/* search key (list of predicate or function arguments) */
	CELLP pcValue;						/* function value (functions only) */
	struct HBTree *pbtLeft;				/* pointer to less than */
	struct HBTree *pbtRight;				/* pointer to greater than */
	struct HBTree **apbtOwner;			/* pointer to owning world (not parent!) */
        double hsp_cost; /* cost associated to this fact, a la HSP */
  //        LISTP pcFFSupFacts;           /* supporting facts (for FF heuristic) */
  //        int nFFOperNumber;            /* number of operator that added this fact */
  //        int nFFOperInstance;          /* number of operator instantiation that added this fact */
  //        int nFFDepth;                 /* depth at which this operator was added */
  //        BINDINGP pbBindings;          /* bindings of the action that added this fact */
        int nIndex;                       /* index number  of the tree that owns this node */
        OPERINSTANCEP poiOperInstance;
        struct HBTree *aphbtComplement;      /* a pointer to the complement fact ( */
        LISTP plActionList;           /* list of actions that have added this fact (only used for occlusion computation (for the moment)) */
        BOOL bNotOccluded;            /* whether or not this node is occluded */
        int nDepthAdded;              /* depth at which the node was last added: only used for occlusion computation */
        BOOL bFFSearched;             /* whether or not this node has been used for search expansion */ 
        BOOL bAddedByRelaxed;   /* whether or not this node is added by the relaxed plan so far */
        int nDepthAddedByRelaxed; /* depth at which the node was last added, used during relaxed plan extraction */
        int nMinDepthAddedByRelaxed; /* depth at which the node was first added, used during relaxed plan extraction */
        int nDepthAddedOcc;  /* depth last added (used for occlusion computation) */
        int nOccCounted; /* this node has been counted as occluded */
        int nDepthOccCounted; /* depth at which this node has been counted as occluded */
  LISTP plActionListRelPlan; /* list of from the relaxed plan that have added this fact */
  int nSimpCount; /* counter used for simplification */
} HBTREE, *HBTREEP;



/* generator contexts */

typedef struct GContext					/* prototypical generator context block */
{
	void (*pfMark)(struct GContext *);	/* pointer to context marking function */
}GCONTEXT, *GCONTEXTP;

typedef struct IContext					/* integer generator context block */
{
	void (*pfMark)(struct GContext *);	/* pointer to context marking function */
	int nValue;							/* integer value */
}ICONTEXT, *ICONTEXTP;

typedef struct LContext					/* is-between context */
{
	void (*pfMark)(struct GContext *);	/* pointer to context marking function */
	int nNext;							/* next value to enumerate */
	int nLast;							/* last value to enumerate */
}LCONTEXT, *LCONTEXTP;

typedef struct NFContext				/* nearest-first context */
{
	void (*pfMark)(struct GContext *);	/* pointer to context marking function */
	BTREEP pbtEdge;						/* edge predicate btree pointer */
	CELLP pcVarVertex;					/* vertex variable */
	CELLP pcVarDistance;				/* distance variable */

	int *pnVertexIndices;				/* vertex index array (name sorted order) */
	CELLP *ppcVertices;					/* vertex pointer array (allocation order) */
	int *pnDistances;					/* distance (edge-count) array (allocation order) */
	int *pnQueue;						/* queue array (breadth first order) */
	int *pnNext;						/* pointer to next available element in queue array */
	int *pnFree;						/* pointer to next free element location in queue array */
	int nVertices;						/* number of unique vertices seen so far */
}NFCONTEXT, *NFCONTEXTP;

typedef struct NFXContext				/* nearest-first-ex context */
{
	void (*pfMark)(struct GContext *);	/* pointer to context marking function */
	CELLP pcLocal;						/* local variables */
	CELLP pcEdge;						/* edge predicate */
	CELLP pcVarVertex;					/* vertex variable */
	CELLP pcVarDist;					/* edge-count variable */

	int *pnVertexIndices;				/* vertex index array (name sorted order) */
	CELLP *ppcVertices;					/* vertex pointer array (allocation order) */
	int *pnEdgeCounts;					/* edge-count array (allocation order) */
	int *pnQueue;						/* queue array (breadth first order) */
	int *pnNext;						/* pointer to next available element in queue array */
	int *pnFree;						/* pointer to next free element location in queue array */
	int nVertices;						/* number of unique vertices seen so far */
}NFXCONTEXT, *NFXCONTEXTP;

typedef struct CFContext				/* closest-first context */
{
	void (*pfMark)(struct GContext *);	/* pointer to context marking function */
	BTREEP pbtEdge;						/* edge predicate btree pointer */
	BTREEP pbtCost;						/* cost function btree pointer */
	CELLP pcVarVertex;					/* vertex variable */
	CELLP pcVarCost;					/* cost variable */
	CELLP pcVarDist;					/* (optional) edge-count variable */

	int *pnVertexIndices;				/* vertex index array (name sorted order) */
	CELLP *ppcVertices;					/* vertex pointer array (allocation order) */
	int *pnEdgeCounts;					/* edge-count array (allocation order) */
	double *pdfCosts;					/* total cost array (allocation order) */
	int *pnHeap;						/* heap array (heap order) */
	int nHeapLength;					/* size of heap array */
	int nHeapCount;						/* number of nodes stored in heap */
	int nVertices;						/* number of unique vertices seen so far */
}CFCONTEXT, *CFCONTEXTP;

typedef struct CFXContext				/* closest-first-ex context */
{
	void (*pfMark)(struct GContext *);	/* pointer to context marking function */
	CELLP pcLocal;						/* local variables */
	CELLP pcEdge;						/* edge predicate */
	CELLP pcCost;						/* cost function */
	CELLP pcVarVertex;					/* vertex variable */
	CELLP pcVarCost;					/* cost variable */
	CELLP pcVarDist;					/* (optional) edge-count variable */

	int *pnVertexIndices;				/* vertex index array (name sorted order) */
	CELLP *ppcVertices;					/* vertex pointer array (allocation order) */
	int *pnEdgeCounts;					/* edge-count array (allocation order) */
	double *pdfCosts;					/* total cost array (allocation order) */
	int *pnHeap;						/* heap array (heap order) */
	int nHeapLength;					/* size of heap array */
	int nHeapCount;						/* number of nodes stored in heap */
	int nVertices;						/* number of unique vertices seen so far */
}CFXCONTEXT, *CFXCONTEXTP;

typedef struct LFContext				/* lowest-first context */
{
	void (*pfMark)(struct GContext *);	/* pointer to context marking function */
	CELLP pcVarList;					/* pointer to variables list */
	int nCount;							/* cardinality of function */
	int nNext;							/* number of elements generated so far */
	BTREEP apbtIndirect[1];				/* array of pointers to btrees */
}LFCONTEXT, *LFCONTEXTP;

typedef struct APSPContext				/* all-pairs-shortest-path context */
{
	void (*pfMark)(struct GContext *);	/* pointer to context marking function */
	CELLP pcVarVertex1;					/* vertex1 variable */
	CELLP pcVarVertex2;					/* vertex2 variable */
	CELLP pcVarCost;					/* cost variable */
	CELLP pcVarNext;					/* next vertex variable */

	int nCount;							/* number of vertices */
	BTREEP *apbtVertices;				/* vertex pointer array (allocation order) */
	double *pdfCosts;					/* total cost array (allocation order squared) */
	int *pnNext;						/* next vertex array (allocation order squared) */

	int i;								/* enumeration row index */
	int j;								/* enumeration column index */
}APSPCONTEXT, *APSPCONTEXTP;

typedef struct BTContext				// context for BTreeGeneratorH
{
	void (*pfMark)(struct GContext *);	/* pointer to context marking function */
	struct BTContext *pbtcNext;			/* pointer to next node */
	BTREEP pbtTree;						/* pointer to btree node */
}BTCONTEXT, *BTCONTEXTP;

typedef struct BTOContext				// context for BTreeGeneratorO
{
	void (*pfMark)(struct GContext *);	/* pointer to context marking function */
	BTREEP pbtNext;						/* pointer to next node */
	BTREEP pbtRoot;						/* pointer to root node */
}BTOCONTEXT, *BTOCONTEXTP;

typedef struct PredContext				// context for described predicates
{
	void (*pfMark)(struct GContext *);	/* pointer to context marking function */
	CELLP pcArgs;						/* evaluated terms */
	void *pvContext;					/* context for BTreeGenerator */
}PREDCONTEXT, *PREDCONTEXTP;

/* symbol-info types */

#define EVAL_UNKNOWN		0
#define EVAL_DESCRIBED		1
#define EVAL_DEFINED		2
#define EVAL_EXTERNAL		3

#define SYMBOL_UNKNOWN		0
#define SYMBOL_PREDICATE	1
#define SYMBOL_FUNCTION		2
#define SYMBOL_GENERATOR	3
#define SYMBOL_MACRO		4

/* global variable types */

#define VARIABLE_UNKNOWN	0
#define DOMAIN_GLOBAL		1
#define SEARCH_GLOBAL		2
#define WORLD_GLOBAL		3

typedef struct ArrayInfo
{
	char *psName;						/* array name */
        int nHashPos;                                           /* position of psName in hash table */
	int nDimension;						/* number of dimensions */
	int nCount;							/* number of array elements */
	int *pnLimits;						/* dimensions */
	int *pnMultipliers;					/* offset multipliers */
	CELLP *ppcArray;					/* array of pointers to values */
}ARRAYINFO, *ARRAYINFOP;

// We implement action-promises by flagging a linear-plan with a null
// apbtWorld pointer.  We coerce and point to the action-promise bindings list
// with nSignature, and we point to the operator modify-world formula 
// with pcActionName.

typedef struct LinearPlan
{
	struct LinearPlan *plpNext;			/* pointer to next plan (in a list) */
	struct LinearPlan *plpParent;		/* pointer to parent plan */
	CELLP pcTLForm;						/* our current control formula */
	CELLP pcCCForm;						/* our current condition formula */
	double dfTime;						/* total time to get to this state */
	double dfDuration;					/* PDDL duration */
	double dfCost;						/* total cost to get to this state */
	int nLength;						/* total number of actions to get to this state */
	int nWorldNumber;					/* instance number of this world */
	BTREEP *apbtWorld;					/* resulting world */
        HBTREEP *aphbtWorld;                                    /* pointer to world used by heuristics */
	CELLP pcActionName;					/* name of action that got us here */
	CELLP pcPromise;					/* action promise (modify-world) formula */
	double dfActionDuration;			/* action duration */
        double dfActionCost;				/* action cost */
	double dfActionPriority;			/* action priority */
	unsigned int nSignature;			/* world signature (extra) */
	double dfHeuristic;					/* heuristic cost (extra) */
	double dfPriority;					/* world priority (extra) */
        double dfBestRelMetric;                                 /* best relaxed metric */
        double dfBestPreferenceValue;
        double *afDistancesToPrefValues;
        double *afHeuristicVector;   /* a vector of heuristic values in addition to dfHeuristic */ 
        int nHeuristicVectorSize;
        LISTP plHelpfulActions;   /* List of Helpful Actions */
        BOOL bIsPreferredNode;    /* Whether or not this node was produced by a helpful action */

}LINEARPLAN, *LINEARPLANP;

typedef struct SearchResult
{
	LINEARPLANP plpState;				/* final state */
	char *psPlanStatus;					/* plan status string */
	int nWorldsGenerated;				/* number of worlds generated */
	int nWorldsSearched;				/* number of worlds searched */
	int nWorldsPruned;					/* number of worlds pruned */
	int nWorldsDiscarded;				/* number of worlds discarded */
	int nWorldsUnexamined;				/* number of worlds unexamined */
	int nSearchMaxDepth;				/* maximum search depth */
	double dfSearchMaxHeuristic;		/* maximum heuristic cost */
	double dfPlanCPUTime;				/* plan search time */
//-------
	int nPlanLength;					/* plan length (depth) */
	double dfPlanCost;					/* plan heuristic cost */
//-------
	int nOpenLength;					/* worlds not yet expanded */
	int nClosedLength;					/* worlds expanded */
//-------
        double dfPlanMetric;                                     /* metric of the plan */             

}SEARCHRESULT, *SEARCHRESULTP;

typedef struct ISpec
{
	int bLowerOpen:1;					/* lower bound is open */
	int bLowerInfinite:1;				/* lower bound is -infinity (for symmetry) */
	int bUpperOpen:1;					/* upper bound is open */
	int bUpperInfinite:1;				/* upper bound is +infinity */
	double dfLower;						/* lower bound */
	double dfUpper;						/* upper bound */
}ISPEC, *ISPECP;

typedef struct Action					/* formula action table entry */
{	
  char *psName;						/* name */
        BOOL (*pfEval)						/* evaluator */
		(struct Cell *, struct LinearPlan *, struct Binding *);
	BOOL (*pfIdle)						/* idle routine */
		(struct Cell *, struct LinearPlan *, struct Binding *);
	struct Cell *(*pfProgress)			/* progressor */
		(struct Cell *, struct LinearPlan *, struct Binding *, BOOL *);
	int (*pfCurrent)					/* current value evaluator */
		(struct Cell *, struct LinearPlan *, struct Binding *);
	BOOL (*pfGenerator)					/* binding generator */
		(struct Cell *, void **, struct Cell *, struct LinearPlan *, struct Binding *);
	struct Cell *(*pfCompute)			/* function evaluator */
		(struct Cell *, struct LinearPlan *, struct Binding *);
	struct Cell *(*pfMakeCCForm)		// current condition
		(BOOL, struct Cell *, BOOL *);
}ACTION, *ACTIONP;

typedef struct SymbolInfo
{
	char *psName;						/* symbol name */
        int nHashPos;                                           /* position of psName in Hash table */
	int nSymbolType;					/* predicate, function, generator, unknown */
	int nEvalType;						/* described, defined, computed, unknown */
	int nArity;							/* number of arguments */
	void (*pfFunction)(void);			/* function pointer */
	CELLP pcFormula;					/* formula to evaluate (optional) */
	ACTIONP paAction;					/* action table */
	BOOL bRewritable;					/* predicate is rewritable */
}SYMBOLINFO, *SYMBOLINFOP;

/* hash status codes */

#define HASH__SUCCESS		0
#define HASH__TABLE_FULL	-1
#define HASH__DUPLICATE_KEY	-2
#define HASH__NOT_FOUND		-3
#define HASH__NO_MEMORY		-4

typedef struct HashTab
{
	void **pavTable;					/* hash table address */
	int nSize;							/* actual size of hash table, records, prime! */
	int nRecLen;						/* record length */
	int nLimit;							/* maximum number of records (<= nSize) */
	int nCount;							/* current number of records */
}HASHTAB, *HASHTABP;

/* -----------------------------------------------------------------------------

 (a) Data structure for operators. An operator is an action template,
		i.e., it contains unbound variables.
		Operators are stored in a structure with the following fields.

	Name:
		a formula of the form (name var1 ... varn)
		When var1 ... varn are instantiated this becomes the name of
		the action generated by the operator.
	Successor:
		successor formula
	Duration:
		the duration of the action.  defaults to 1.
	Cost:
		cost of action/operator. Can be used
		by search routines that search for optimizing solution in
		situations where each action has varying costs.  Defaults to 1.
	Priority:
		search priority.  This can be used in search to prioritize the
		successors of a world (e.g., in depthFirst search we can
		achieve a oneStep best first)
*/

typedef struct Operator
{
	struct Operator *poNext;			/* pointer to next operator */
	CELLP pcName;						/* operator name and arguments */
	CELLP pcSuccessor;					/* successor formula */
	CELLP pcDuration;					/* MTL duration (defaults to 1) */
	CELLP pcCost;						/* cost of action (defaults to 1) */
	CELLP pcPriority;					/* search order priority (defaults to 0) */
        BOOL bHSPIgnore;                                        /* whether or not we ignore this action in the hsp heuristic */
        BOOL bUnrelEff;                                         /* whether or not all the effects of this action are unrelaxed fluents */
} OPERATOR, *OPERATORP;

typedef struct MacroOperator			/* macro operator declaration */
{
	struct MacroOperator *pmoNext;		/* pointer to next */
	char *psName;						/* operator name */
	char *psDomain;						/* expansion domain path */
	CELLP pcGoal;						/* goal formula */
}MACROOPERATOR, *MACROOPERATORP;

typedef struct ElidedOperator			/* elided operator declaration */
{
	struct ElidedOperator *peoNext;		/* pointer to next */
	char *psName;						/* operator name */
}ELIDEDOPERATOR, *ELIDEDOPERATORP;

typedef BOOL (*EVALP)(CELLP, LINEARPLANP, BINDINGP);
typedef struct Cell *(*COMPUTEP)(CELLP, LINEARPLANP, BINDINGP);
typedef void (*COMMANDP)(struct List *);
typedef struct Cell *(*FUNCTIONP)(struct Cell *);
typedef BOOL (*GENERATORP)(struct Cell *, void **, struct Cell *,
	struct LinearPlan *, struct Binding *);

typedef char *(*SEARCHP)				/* search engine prototype */
(
	BOOL (*)(LINEARPLANP),
	LINEARPLANP (*)(CELLP, CELLP, LINEARPLANP, BINDINGP),
	BOOL (*)(LINEARPLANP, LINEARPLANP),
	CELLP,
	CELLP,
	int,
	BINDINGP pbBindings
);

typedef int (*MERGEP)
(
   LINEARPLANP,
   LINEARPLANP
);

typedef struct TimeStat					/* timing statistics */
{
	double adfProgress[2];				/* total time progressing */
	double adfProgressAtomic[2];		/* total time progressing atomics */
	double adfSuccessor[2];				/* total time generating successors */
	double adfHeuristic[2];				/* total time calculating heuristic functions */
	double adfPriority[2];				/* total time calculating priority functions */
	double adfBTreeLookUp[2];			/* total time searching btrees */
	double adfBTreeModify[2];			/* total time modifying btrees */
	double adfCycleCheck[2];			/* total time cycle checking */
	double adfUpdateOpenClose[2];		/* total time updating open/closed lists */
	double adfPruneOpenList[2];			/* total time pruning open list */
	double adfGarbageCollect[2];		/* total time collecting garbage */
	double adfMemoryAllocation[2];		/* total time allocating memory */
}TIMESTAT, *TIMESTATP;


extern char *slogFile;

#endif /* __TLPLAN_H */
