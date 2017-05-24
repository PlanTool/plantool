/* tlparse.c -- tlplan symbol table, parser and lexical analyzer support routines

Copyright C, 1996 - 2001  F. Bacchus

*/

#include <assert.h>
#include <setjmp.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef WIN32
#include <windows.h>
#endif /* WIN32 */

#include "tlplan.h"
#include "adl.h"
#include "btree.h"
#include "hbtree.h"
#include "crchash.h"
#include "domain.h"
#include "eval.h"
#include "formula.h"
#include "hash.h"
#include "iface.h"
#include "makeform.h"
#include "oper.h"
#include "plan.h"
#include "search.h"
#include "strips.h"
#include "tllex.h"
#include "tlparse.h"
#include "tl_tab.h"
#include "user.h"
#include "util.h"
#include "world.h"
#include "zone.h"
#include "strhash.h"

/* local structures and definitions */

typedef struct STab
{
	int nIndex;							/* table index */
	char *psString;						/* string to hash */
}STAB, *STABP;

/* local data */

/* To add a new keyword to the language:

1.  Define a STRING_XXX symbol.
	See tlplan.h.

2.  Add a new entry to asStringTable.
	See tlparse.c.

3.  If necessary, add a new ACTION table entry.
	See formula.c.

4.  If necessary, add entries to ListToFormula (in two locations) according to arity of keyword.  
	See formula.c.

5.  If necessary add an InitActionName entry to InitPlanner.
	See tlparse.c
  
6.  If necessary, add the appropriate evaluation and/or computation etc. routines.  
	See Eval.c, Eval1.c, Compute.c, Progress.c, and/or Idle.c.

*/

static STAB asStringTable[]=			/* be sure to add strings to the end */
{
	{STRING_LPAREN,"("},
	{STRING_RPAREN,")"},
	{STRING_ASSIGN,":="},
	{STRING_LT,"<"},
	{STRING_LE,"<="},
	{STRING_EQ,"="},
	{STRING_GT,">"},
	{STRING_GE,">="},
	{STRING_ASTAR,"a*"},
	{STRING_ACTION_PRIORITY,"action-priority"},
	{STRING_ADD,"add"},
	{STRING_ALWAYS,"always"},
	{STRING_AND,"and"},
	{STRING_ATEMPORAL,"atemporal"},
	{STRING_BEST_ACTION,"best-action"},
	{STRING_CLASSIC,"classic"},
	{STRING_COST,"cost"},
	{STRING_DEL,"del"},
	{STRING_DELTA,"delta"},
	{STRING_DOMAIN_GLOBAL,"domain-global"},
	{STRING_DURATION,"duration"},
	{STRING_EVENTUALLY,"eventually"},
	{STRING_EXISTSX,"exists!"},
	{STRING_EXISTS,"exists"},
	{STRING_EXTENDED,"extended"},
	{STRING_FALSE,"false"},
	{STRING_FORALL,"forall"},
	{STRING_FUNCTION,"function"},
	{STRING_GENERATOR,"generator"},
	{STRING_GOAL,"goal"},
	{STRING_IF_THEN_ELSE,"if-then-else"},
	{STRING_IMPLIES,"implies"},
	{STRING_ISPEC,"ispec"},
	{STRING_NEXT,"next"},
	{STRING_NO_PLAN,"no-plan"},
	{STRING_NOT,"not"},
	{STRING_OPTIMAL_COST,"optimal-cost"},
	{STRING_OR,"or"},
	{STRING_PLAN_COST,"plan-cost"},
	{STRING_PLAN_LENGTH,"plan-length"},
	{STRING_SET_PLAN_NAME,"set-plan-name"},
	{STRING_DEPTH_BEST_FIRST,"depth-best-first"},
	{STRING_PRE,"pre"},
	{STRING_PREDICATE,"predicate"},
	{STRING_PREVIOUS,"previous"},
	{STRING_PRINT,"print"},
	{STRING_PRINT_WORLD,"print-world"},
	{STRING_PRIORITY,"priority"},
	{STRING_RESOURCES,"resources"},
	{STRING_SEARCH_GLOBAL,"search-global"},
	{STRING_OPERATOR,"operator"},
	{STRING_DEF_DEFINED_GENERATOR,"def-defined-generator"},
	{STRING_GET_GOAL_ADDENDUM,"get-goal-addendum"},
	{STRING_T_ALWAYS,"t-always"},
	{STRING_T_EVENTUALLY,"t-eventually"},
	{STRING_T_UNTIL,"t-until"},
	{STRING_TEMPORAL,"temporal"},
	{STRING_TRUE,"true"},
	{STRING_UNKNOWN,"unknown"},
	{STRING_UNTIL,"until"},
	{STRING_WORLD_GLOBAL,"world-global"},
	{STRING_WORLD_HEURISTIC_RANK,"world-heuristic-rank"},
	{STRING_WORLD_NUMBER,"world-number"},
	{STRING_XOR,"xor"},
	{STRING_BREADTH_FIRST,"breadth-first"},
	{STRING_BREADTH_FIRST_PRIORITY,"breadth-first-priority"},
	{STRING_BEST_FIRST,"best-first"},
	{STRING_DEPTH_FIRST,"depth-first"},
	{STRING_DEPTH_FIRST_PRIORITY,"depth-first-priority"},
	{STRING_DEPTH_FIRST_NO_BACKTRACKING,"depth-first-no-backtracking"},
	{STRING_PLUS,"+"},
	{STRING_MINUS,"-"},
	{STRING_STAR,"*"},
	{STRING_SLASH,"/"},
	{STRING_MOD,"mod"},
	{STRING_MAX,"max"},
	{STRING_MIN,"min"},
	{STRING_EXPT,"expt"},
	{STRING_SQRT,"sqrt"},
	{STRING_ABS,"abs"},
	{STRING_EXP,"exp"},
	{STRING_LOG,"log"},
	{STRING_RAND,"rand"},
	{STRING_SEED,"seed"},
	{STRING_ROUND,"round"},
	{STRING_INT,"int"},
	{STRING_FLOOR,"floor"},
	{STRING_CEIL,"ceil"},
	{STRING_POS_INT,"pos-int"},
	{STRING_LT_POS_INT,"<-pos-int"},
	{STRING_IS_BETWEEN,"is-between"},
	{STRING_PERMUTE,"permute"},
	{STRING_EXIT,"exit"},
	{STRING_RANDOM,"random"},
	{STRING_CLEAR_WORLD_SYMBOLS,"clear-world-symbols"},
	{STRING_DECLARE_DEFINED_SYMBOLS,"declare-defined-symbols"},
	{STRING_DECLARE_DESCRIBED_SYMBOLS,"declare-described-symbols"},
	{STRING_DECLARE_EXTERNAL_SYMBOLS,"declare-external-symbols"},
	{STRING_DECLARE_GLOBAL_VARIABLES,"declare-global-variables"},
	{STRING_MACRO,"macro"},
	{STRING_DEFINE,"define"},
	{STRING_DEF_ADL_OPERATOR,"def-adl-operator"},
	{STRING_DEF_DEFINED_FUNCTION,"def-defined-function"},
	{STRING_DEF_DEFINED_PREDICATE,"def-defined-predicate"},
	{STRING_DEF_DOMAIN,"def-domain"},
	{STRING_DEF_STRIPS_OPERATOR,"def-strips-operator"},
	{STRING_GET_HEURISTIC_FN,"get-heuristic-fn"},
	{STRING_PRINT_WORLD_LIST,"print-world-list"},
	{STRING_GET_PRIORITY_FN,"get-priority-fn"},
	{STRING_GET_SEARCH_LIMIT,"get-search-limit"},
	{STRING_GET_SEARCH_STRATEGY,"get-search-strategy"},
	{STRING_GET_TL_CONTROL,"get-tl-control"},
	{STRING_GET_TRACE_LEVEL,"get-trace-level"},
	{STRING_LIST_DOMAINS,"list-domains"},
	{STRING_LOAD_DOMAIN,"load-domain"},
	{STRING_LOAD_FILE,"load-file"},
	{STRING_PLAN,"plan"},
	{STRING_PRINT_PLAN_LIST,"print-plan-list"},
	{STRING_RESET_DOMAINS,"reset-domains"},
	{STRING_RESET_HEURISTIC_FN,"reset-heuristic-fn"},
	{STRING_RESET_PRINT_WORLD_FN,"reset-print-world-fn"},
	{STRING_RESET_PRIORITY_FN,"reset-priority-fn"},
	{STRING_RESET_SEARCH_LIMIT,"reset-search-limit"},
	{STRING_RESET_SEARCH_STRATEGY,"reset-search-strategy"},
	{STRING_RESET_TL_CONTROL,"reset-tl-control"},
	{STRING_SET_CONTROL,"set-control"},
	{STRING_SET_HEURISTIC_FN,"set-heuristic-fn"},
	{STRING_SET_INITIAL_FACTS,"set-initial-facts"},
	{STRING_SET_INITIAL_WORLD,"set-initial-world"},
	{STRING_SET_INITIALIZATION_SEQUENCE,"set-initialization-sequence"},
	{STRING_SET_GOAL,"set-goal"},
	{STRING_SET_GOAL_TYPE,"set-goal-type"},
	{STRING_SET_PRINT_WORLD_FN,"set-print-world-fn"},
	{STRING_SET_PRIORITY_FN,"set-priority-fn"},
	{STRING_SET_SEARCH_LIMIT,"set-search-limit"},
	{STRING_SET_SEARCH_STRATEGY,"set-search-strategy"},
	{STRING_SET_STATISTICS_FILE,"set-statistics-file"},
	{STRING_SET_TL_CONTROL,"set-tl-control"},
	{STRING_SET_TRACE_LEVEL,"set-trace-level"},
	{STRING_VERBOSE_OFF,"verbose-off"},
	{STRING_VERBOSE_ON,"verbose-on"},
	{STRING_ACTION_NAME,"action-name"},
	{STRING_ACTION_COST,"action-cost"},
	{STRING_ACTION_DURATION,"action-duration"},
	{STRING_LOCAL_VARS,"local-vars"},
	{STRING_SELECT_INITIAL_WORLD,"select-initial-world"},
	{STRING_SELECT_FINAL_WORLD,"select-final-world"},
	{STRING_SELECT_NEXT_WORLD,"select-next-world"},
	{STRING_SELECT_PREVIOUS_WORLD,"select-previous-world"},
	{STRING_CURRENT,"current"},
	{STRING_WRITE,"write"},
	{STRING_APPEND,"append"},
	{STRING_READ,"read"},
	{STRING_OPEN_FILE,"open-file"},
	{STRING_CLOSE_FILE,"close-file"},
	{STRING_FIBONACCI,"fibonacci"},
	{STRING_MS,"ms"},
	{STRING_ISAAC,"isaac"},
	{STRING_SET_RNG,"set-rng"},
	{STRING_RESET_RNG,"reset-rng"},
	{STRING_GET_RNG,"get-rng"},
	{STRING_DEF_DEFINED_MACRO,"def-defined-macro"},
	{STRING_ASSIGN_APPEND,":<<"},
	{STRING_REDIRECT,"redirect"},
	{STRING_PLAN_DURATION,"plan-duration"},
	{STRING_PLAN_STATUS,"plan-status"},
	{STRING_WORLDS_GENERATED,"worlds-generated"},
	{STRING_WORLDS_SEARCHED,"worlds-searched"},
	{STRING_WORLDS_PRUNED,"worlds-pruned"},
	{STRING_WORLDS_DISCARDED,"worlds-discarded"},
	{STRING_WORLDS_UNEXAMINED,"worlds-unexamined"},
	{STRING_PLAN_CPU_TIME,"plan-cpu-time"},
	{STRING_SET_SEARCH_DEPTH_LIMIT,"set-search-depth-limit"},
	{STRING_GET_SEARCH_DEPTH_LIMIT,"get-search-depth-limit"},
	{STRING_SEARCH_MAX_DEPTH,"search-max-depth"},
	{STRING_SET_SEARCH_HEURISTIC_LIMIT,"set-search-heuristic-limit"},
	{STRING_GET_SEARCH_HEURISTIC_LIMIT,"get-search-heuristic-limit"},
	{STRING_SEARCH_MAX_HEURISTIC,"search-max-heuristic"},
	{STRING_RESET_SEARCH_DEPTH_LIMIT,"reset-search-depth-limit"},
	{STRING_RESET_SEARCH_HEURISTIC_LIMIT,"reset-search-heuristic-limit"},
	{STRING_OK,"ok"},
	{STRING_GET_CPU_TIME,"get-cpu-time"},
	{STRING_HEURISTIC_FN,"heuristic-fn"},
	{STRING_SET_GOAL_ADDENDUM,"set-goal-addendum"},
	{STRING_RESET_GOAL_ADDENDUM,"reset-goal-addendum"},
	{STRING_GET_PLAN_NAME,"get-plan-name"},
	{STRING_RESET_PLAN_NAME,"reset-plan-name"},
	{STRING_BINDING,"binding"},
	{STRING_MODIFY_WORLD,"modify-world"},
	{STRING_USER_ABORT,"user-abort"},
	{STRING_ENABLE,"enable"},
	{STRING_DISABLE,"disable"},
	{STRING_CYCLE_CHECKING,"cycle-checking"},
	{STRING_TIMING_STATISTICS,"timing-statistics"},
	{STRING_NO_CYCLE_CHECK,"no-cycle-check"},
	{STRING_IN_THE_SET,"in-the-set"},
	{STRING_BACKTRACKING,"backtracking"},
	{STRING_PRUNING_ALL_SUCCESSORS,"pruning-all-successors"},
	{STRING_CONCURRENT_PLANNING,"concurrent-planning"},
	{STRING_WAIT_FOR_NEXT_EVENT,"wait-for-next-event"},
	{STRING_DELAYED_ACTION,"delayed-action"},
	{STRING_GLOBAL_DELAYED_ACTION,"global-delayed-action"},
	{STRING_LOAD_PLAN,"load-plan"},
	{STRING_MAKE_LITERAL,"make-literal"},
	{STRING_SET_GOAL_SEQUENCE,"set-goal-sequence"},
	{STRING_SET_GOAL_FORMULA,"set-goal-formula"},
	{STRING_CURRENT_TIME,"current-time"},
	{STRING_INHIBIT_DELAYED_ACTION,"inhibit-delayed-action"},
	{STRING_PLUS_EQ,"+="},
	{STRING_MINUS_EQ,"-="},
	{STRING_PRINT_DELTA_TIME,"print-delta-time"},
	{STRING_REWRITABLE,"rewritable"},
	{STRING_PDDL_SUPPORT,"pddl-support"},
	{STRING_DECLARE_MACRO_OPERATORS,"declare-macro-operators"},
	{STRING_CLEAR_OPERATORS,"clear-operators"},
	{STRING_NEAREST_FIRST,"nearest-first"},
	{STRING_DECLARE_ELIDED_OPERATORS,"declare-elided-operators"},
	{STRING_LOAD_PDDL_PROBLEM,"load-pddl-problem"},
	{STRING_CLEAR_EVENT_QUEUE,"clear-event-queue"},
	{STRING_CLOSEST_FIRST,"closest-first"},
	{STRING_UPDATE_WORLD,"update-world"},
	{STRING_LOWEST_FIRST,"lowest-first"},
	{STRING_PRINT_PDDL_PLAN,"print-pddl-plan"},
	{STRING_REACHABLE_EVENT,"reachable-event"},
	{STRING_ALL_PAIRS_SHORTEST_PATH,"all-pairs-shortest-path"},
	{STRING_NEAREST_FIRST_EX,"nearest-first-ex"},
	{STRING_CLOSEST_FIRST_EX,"closest-first-ex"},
	{STRING_CONDITIONAL_EXP,"conditional-exp"},
	{STRING_SDEL,"sdel"},
	{STRING_GOAL_DISTANCE,"goal-distance"},
	{STRING_SET_METRIC_FN,"set-metric-fn"},
	{STRING_SET_PREFERENCES,"set-preferences"},
	{STRING_SET_POSTACTION_SEQUENCE,"set-post-action-sequence"},
	{STRING_SET_GOAL_MULTIPLIER,"set-goal-multiplier"},
	{STRING_SET_PREFERENCE_MULTIPLIER,"set-preference-multiplier"},
	{STRING_SET_METRIC_MULTIPLIER,"set-metric-multiplier"},
	{STRING_SET_OPTIMISTIC_METRIC_MULTIPLIER,"set-optimistic-metric-multiplier"},
	{STRING_SET_OPTIMISTIC_METRIC_FN,"set-optimistic-metric-fn"},
	{STRING_SET_HEURISTIC_EXPONENT,"set-heuristic-exponent"},
	{STRING_SET_METRIC_DISCOUNT,"set-metric-discount"},
	{STRING_SET_PLAN_TIMEOUT,"set-plan-timeout"},
	{STRING_PRINT_PDDL3_PLAN,"print-pddl3-plan"},
	{STRING_WORLD_COUNTER,"world-counter"},
	{STRING_SET_COUNT_SAT_PREFS_FN,"set-count-sat-prefs-fn"},
	{STRING_GET_BEST_RELAXED_METRIC,"get-best-relaxed-metric"},
	{STRING_SET_BEST_METRIC_MULTIPLIER,"set-best-metric-multiplier"},
	{STRING_SET_DISCOUNTED_METRIC_MULTIPLIER,"set-discounted-metric-multiplier"},
	{STRING_CLOSEDLIST_LENGTH,"closedlist-length"},
	{STRING_GOAL_DISTANCE_NEG,"goal-distance-neg"},
	{STRING_SET_MERGE_FN_TYPE,"set-merge-fn-type"},
	{STRING_SET_QUALPREFERENCE_VALUE_FN,"set-qualpreference-value-fn"},
	{STRING_SET_QUALPREFERENCE_NUMBER,"set-qualpreference-number"},
	{STRING_NUMERIC,"numeric"},
	{STRING_QUALPREFERENCE_VALUE_FIRST,"qualpreference-value-first"},
	{STRING_QUALPREFERENCE_EASY_FIRST,"qualpreference-easy-first"},
	{STRING_GET_BEST_PREFERENCE_VALUE,"get-best-preference-value"},
	{STRING_GOAL_DISTANCE_QUALPREFERENCES,"goal-distance-qualpreferences"},
	{STRING_GOAL_QUALPREFERENCE_VALUE_FIRST,"goal-qualpreference-value-first"},
	{STRING_GOAL_QUALPREFERENCE_EASY_FIRST,"goal-qualpreference-easy-first"},
	{STRING_SET_BDF_PREFERENCE,"set-bdf-preference"},
	{STRING_NO_RELAXING,"no-relaxing"},
	{STRING_DEF_ADL_H_OPERATOR,"def-adl-h-operator"},
	{STRING_HSP_IGNORE,"hsp-ignore"},
	{STRING_UNREL_EFFECTS,"only-unrelaxed-effects"},
	{STRING_UNOT,"unot"},
	{STRING_GET_NO_WORLD_FIXED_POINTS,"get-no-world-fixed-points"},
	{STRING_GET_NO_REAL_FIXED_POINTS,"get-no-real-fixed-points"},
	{STRING_GOAL_DISTANCE_HSP_UNRELEFFS,"goal-distance-hsp-unreleffs"},
	{STRING_GOAL_DISTANCE_FF_UNRELEFFS,"goal-distance-ff-unreleffs"},
	{STRING_VERBOSE_SHOW_EXPANDED_PLANS_ON,"verbose-show-expanded-plans-on"},
	{STRING_VERBOSE_SHOW_EXPANDED_PLANS_OFF,"verbose-show-expanded-plans-off"},
	{STRING_VERBOSE_SHOW_EXPANDED_WORLDS_ON,"verbose-show-expanded-worlds-on"},
	{STRING_VERBOSE_SHOW_EXPANDED_WORLDS_OFF,"verbose-show-expanded-worlds-off"},
	{STRING_VERBOSE_FF_SHOW_RELAXED_PLANS_ON,"verbose-ff-show-relaxed-plans-on"},
	{STRING_VERBOSE_FF_SHOW_RELAXED_PLANS_OFF,"verbose-ff-show-relaxed-plans-off"},
	{STRING_VERBOSE_FF_SHOW_RELAXED_WORLDS_ON,"verbose-ff-show-relaxed-worlds-on"},
	{STRING_VERBOSE_FF_SHOW_RELAXED_WORLDS_OFF,"verbose-ff-show-relaxed-worlds-off"},
	{STRING_BEST_FIRST_DELAYED_HEURISTIC,"best-first-delayed-heuristic"},
	{STRING_BEST_FIRST_HELPFUL_LIST,"best-first-helpful-list"},
	{STRING_SET_HELPFUL_LIST_PRIORITY,"set-helpful-list-priority"},
	{STRING_SET_DISCOUNTED_METRIC_PRIORITY,"set-discounted-metric-priority"},
	{STRING_SET_OPTIMISTIC_METRIC_PRIORITY,"set-optimistic-metric-priority"},
	{STRING_SET_BEST_RELAXED_METRIC_PRIORITY,"set-best-relaxed-metric-priority"},
	{STRING_SET_PREFERENCE_DISTANCE_PRIORITY,"set-preference-distance-priority"},
	{STRING_SET_METRIC_PRIORITY,"set-metric-priority"},
	{STRING_GOAL_DISTANCE_FF_PDDL_PREFS,"goal-distance-ff-pddl-prefs"},
	{STRING_SET_TOTAL_PREFS_FN,"set-total-prefs-fn"},
	{STRING_SET_HEURISTIC_PRESERVED_PROPERTY,"set-heuristic-preserved-property"},
	{STRING_WATCH_HEURISTIC_PRESERVE,"watch-heuristic-preserve"},
	{STRING_SET_GOAL_OCCLUSION_MULTIPLIER,"set-goal-occlusion-multiplier"},
	{STRING_SET_PRECOND_OCCLUSION_MULTIPLIER,"set-precondition-occlusion-multiplier"},
	{STRING_BEST_FIRST_PREFER_LONGER,"best-first-prefer-longer"},
	{STRING_PRINT_STRING,"print-string"},
	{STRING_FF_RELPLAN_SIMPLIFICATION_ON,"ff-relplan-simplification-on"}, 
	{STRING_FF_RELPLAN_SIMPLIFICATION_OFF,"ff-relplan-simplification-off"}
};

/* global data ----------------------------------------------------------------- */

int nLineNumber;						/* input line number */
char *psCurrentFile;					/* current file name */
char *yysol;							/* buffer location of current line */
char *apsStringTab[STRING__MAX];		/* hashed string pointer table */

HASHTAB htSymbolTable;					/* symbol table header */
HASHTAB htStringTable;					/* string table header */

FILE *pfInput;							/* input file stream */
char *psInput;							/* input string */
int (*pfYYInput)(char *, int)=yy_input;	/* lex input function pointer */
void (*pfYYOutput)(LISTP)=ExecuteList;	/* yacc output function pointer */
CELLP pcParsedFormula;					/* formula for StringToFormula */
BOOL bInteractiveMode=TRUE;				/* running in interactive mode */

CELLP pcCurrentCommand;					/* current command formula */
LINEARPLANP plpCurrentPlan;				/* current plan */
#ifdef WIN32
BOOL bNT;								/* this is Windows NT */
#endif /* WIN32 */

/* local function prototypes */

//static int TabColumn(unsigned char *ps, int nPos);
static void InitStringTable(void);
#ifdef WIN32
static char *ReadCommand
(
	char *pcBuffer,
	int nMaxSize,
	FILE *pfInput
);
#endif // WIN32

/* InitPlanner -----------------------------------------------------------------

Description:
	Initialize dynamic memory and command tables.
*/

DECLSPEC void InitPlanner(void)
{
	/* setup zoned memory */

  // nZoneLimit= MIN( (int)(0.75*SizeOfMemory()), 1e9-1.1e8); /* never exceed 1GB of memory */ 
  nZoneLimit= 1e9-1.1e8; /* never exceed 1GB of memory */ 
  //  nZoneLimit= MIN( (int)(0.75*SizeOfMemory()), 250000000); /* never exceed 250MB of memory */
  // nZoneLimit = 250000000;  
  //  nZoneLimit = 2000000;  
  printf("SizeOfMemory=%d\n",nZoneLimit);
    InitZone(&zScratch);
	InitZone(&zPermanent);
	SetZone(&zPermanent);

#ifdef WIN32

    /* Detect platform */
 
	{
		OSVERSIONINFO osvi; 
		osvi.dwOSVersionInfoSize=sizeof(OSVERSIONINFO); 
 
		GetVersionEx(&osvi); 
		if(osvi.dwPlatformId==VER_PLATFORM_WIN32_NT)
			bNT=TRUE;
		else
			bNT=FALSE;
	} 

#endif /* WIN32 */
	
	/* initialize the string table */

	//	if(HashCreate(&htStringTable,STRINGTABSIZE,sizeof(char *))==HASH__NO_MEMORY)
	//{
	//	ErrorMessage("Failed to allocate string hash table, exiting.\n");
	//	exit(1);
	//}
	
	if( StrHashCreate()==HASH__NO_MEMORY ) {
	    ErrorMessage("Failed to allocate string hash table, exiting.\n");
	    exit(1);
	  }
	
	
	InitStringTable();
	
	/* initialize the symbol table */

	if(HashCreate(&htSymbolTable,SYMTABSIZE,sizeof(SYMBOL))==HASH__NO_MEMORY)
	{
		ErrorMessage("Failed to allocate symbol hash table, exiting.\n");
		exit(1);
	}
	InitSymbolTable();

	/* initialize the btree stack */

	InitBTreeStack();

	/* initialize the hbtree stack */

	HInitBTreeStack();

	/* initialize CRC hash table */

	CRCHashCreate();

	/* default world */

	plpCurrentPlan=MakeWorldAction(MakeWorld(),NULL,0,0,0);

	InitFileHandles();					// set up default file handles
	ResetTLControl();					// (always (true))
	EvalResetDomains(NULL,NULL,NULL);	// release the domain list
	bImmediateExit=FALSE;				// clear previous exit flag

	InitActionName(&aTrueAction);
	InitActionName(&aFalseAction);
	InitActionName(&aNotAction);
	InitActionName(&aUNotAction);
	InitActionName(&aAndAction);
	InitActionName(&aOrAction);
	InitActionName(&aXorAction);
	InitActionName(&aImpliesAction);
	InitActionName(&aIfThenElseAction);
	InitActionName(&aEqAction);
	InitActionName(&aForAllAction);
	InitActionName(&aExistsAction);
	InitActionName(&aExistsXAction);
	InitActionName(&aAlwaysAction);
	InitActionName(&aEventuallyAction);
	InitActionName(&aNextAction);
	InitActionName(&aUntilAction);
	InitActionName(&aTAlwaysAction);
	InitActionName(&aTEventuallyAction);
	InitActionName(&aTUntilAction);
	InitActionName(&aBindingAction);
	InitActionName(&aDeltaAction);
	InitActionName(&aAddAction);
	InitActionName(&aDelAction);
	InitActionName(&aSDelAction);
	InitActionName(&aPrintWorldAction);
	InitActionName(&aPrintWorldListAction);
	InitActionName(&aPrintAction);
	InitActionName(&aSetPlanNameAction);
	InitActionName(&aResetPlanNameAction);
	InitActionName(&aGetPlanNameAction);
	InitActionName(&aExitAction);
	InitActionName(&aAssignAction);
	InitActionName(&aAssignAppendAction);
	InitActionName(&aSearchGlobalInitializationAction);
	InitActionName(&aGoalAction);
	InitActionName(&aCurrentAction);
	InitActionName(&aPreviousAction);
	InitActionName(&aPermuteAction);
	InitActionName(&aArrayAction);
	InitActionName(&aAStarAction);
	InitActionName(&aOptimalCostAction);
	InitActionName(&aBestActionAction);
	InitActionName(&aPlanLengthAction);
	InitActionName(&aPlanCostAction);
	InitActionName(&aPlanDurationAction);
	InitActionName(&aActionNameAction);
	InitActionName(&aActionCostAction);
	InitActionName(&aActionDurationAction);
	InitActionName(&aActionPriorityAction);
	InitActionName(&aWorldNumberAction);
	InitActionName(&aWorldCounterAction);
	InitActionName(&aPreAction);
	InitActionName(&aVarGenAction);
	InitActionName(&aLiteralAction);
	InitActionName(&aIdentAction);
	InitActionName(&aFloatAction);
	InitActionName(&aIntegerAction);
	InitActionName(&aStringAction);
	InitActionName(&aISpecAction);
	InitActionName(&aDefPredicateAction);
	InitActionName(&aDefGeneratorAction);
	InitActionName(&aDefFunctionAction);
	InitActionName(&aDefMacroAction);
	InitActionName(&aModifyWorldAction);
	InitActionName(&aOperatorAction);
	InitActionName(&aDummyAction);
	InitActionName(&aClearWorldSymbolsAction);
	InitActionName(&aDeclareDefinedSymbolsAction);
	InitActionName(&aDeclareDescribedSymbolsAction);
	InitActionName(&aDeclareExternalSymbolsAction);
	InitActionName(&aDeclareGlobalVariablesAction);
	InitActionName(&aDeclareMacroOperatorsAction);
	InitActionName(&aDefineAction);
	InitActionName(&aDefADLOperatorAction);
	InitActionName(&aDefADLHOperatorAction);
	InitActionName(&aDefDefinedFunctionAction);
	InitActionName(&aDefDefinedGeneratorAction);
	InitActionName(&aDefDefinedMacroAction);
	InitActionName(&aDefDefinedPredicateAction);
	InitActionName(&aDefDomainAction);
	InitActionName(&aDefStripsOperatorAction);
	InitActionName(&aGetHeuristicFnAction);
	InitActionName(&aHeuristicFnAction);
	InitActionName(&aGetPriorityFnAction);
	InitActionName(&aGetSearchLimitAction);
	InitActionName(&aGetSearchStrategyAction);
	InitActionName(&aGetTLControlAction);
	InitActionName(&aGetGoalAddendumAction);
	InitActionName(&aGetTraceLevelAction);
	InitActionName(&aListDomainsAction);
	InitActionName(&aLoadDomainAction);
	InitActionName(&aLoadFileAction);
	InitActionName(&aLoadPlanAction);
	InitActionName(&aPlanAction);
	InitActionName(&aPrintPlanListAction);
	InitActionName(&aResetDomainsAction);
	InitActionName(&aResetHeuristicFnAction);
	InitActionName(&aResetPrintWorldFnAction);
	InitActionName(&aResetPriorityFnAction);
	InitActionName(&aResetSearchLimitAction);
	InitActionName(&aResetSearchStrategyAction);
	InitActionName(&aResetTLControlAction);
	InitActionName(&aSetControlAction);
	InitActionName(&aEnableAction);
	InitActionName(&aDisableAction);
	InitActionName(&aSetGoalAddendumAction);
	InitActionName(&aResetGoalAddendumAction);
	InitActionName(&aSetHeuristicFnAction);
	InitActionName(&aSetMetricFnAction);
	InitActionName(&aSetOptimisticMetricFnAction);
	InitActionName(&aSetMetricMultiplierAction);
	InitActionName(&aSetOptimisticMetricMultiplierAction);
	InitActionName(&aSetGoalMultiplierAction);
	InitActionName(&aSetPreferenceMultiplierAction);
	InitActionName(&aSetGoalOcclusionMultiplierAction);
	InitActionName(&aSetPrecondOcclusionMultiplierAction);
	InitActionName(&aSetOptimisticMetricFnAction);
	InitActionName(&aSetInitialFactsAction);
	InitActionName(&aSetInitialWorldAction);
	InitActionName(&aSetInitializationSequenceAction);
	InitActionName(&aSetGoalAction);
	InitActionName(&aSetPreferencesAction);
	InitActionName(&aSetGoalTypeAction);
	InitActionName(&aSetPrintWorldFnAction);
	InitActionName(&aSetPriorityFnAction);
	InitActionName(&aSetSearchLimitAction);
	InitActionName(&aSetSearchStrategyAction);
	InitActionName(&aSetStatisticsFileAction);
	InitActionName(&aSetTLControlAction);
	InitActionName(&aSetTraceLevelAction);
	InitActionName(&aVerboseOffAction);
	InitActionName(&aVerboseOnAction);
	InitActionName(&aLtAction);
	InitActionName(&aLeAction);
	InitActionName(&aGtAction);
	InitActionName(&aGeAction);
	InitActionName(&aSeedAction);
	InitActionName(&aRandAction);
	InitActionName(&aRandomAction);
	InitActionName(&aExpAction);
	InitActionName(&aLogAction);
	InitActionName(&aRoundAction);
	InitActionName(&aIntAction);
	InitActionName(&aFloorAction);
	InitActionName(&aCeilAction);
	InitActionName(&aPlusAction);
	InitActionName(&aMinusAction);
	InitActionName(&aMultiplyAction);
	InitActionName(&aDivideAction);
	InitActionName(&aModAction);
	InitActionName(&aMaxAction);
	InitActionName(&aMinAction);
	InitActionName(&aConditionalExpAction);
	InitActionName(&aExptAction);
	InitActionName(&aSqrtAction);
	InitActionName(&aAbsAction);
	InitActionName(&aPosIntAction);
	InitActionName(&aLtPosIntAction);
	InitActionName(&aIsBetweenAction);
	InitActionName(&aInTheSetAction);
	InitActionName(&aSelectInitialWorldAction);
	InitActionName(&aSelectFinalWorldAction);
	InitActionName(&aSelectNextWorldAction);
	InitActionName(&aSelectPreviousWorldAction);
	InitActionName(&aCloseFileAction);
	InitActionName(&aRedirectAction);
	InitActionName(&aOpenFileAction);
	InitActionName(&aPrintStringAction);
	InitActionName(&aSetRNGAction);
	InitActionName(&aResetRNGAction);
	InitActionName(&aGetRNGAction);
	InitActionName(&aPlanStatusAction);
	InitActionName(&aWorldsGeneratedAction);
	InitActionName(&aWorldsSearchedAction);
	InitActionName(&aWorldsPrunedAction);
	InitActionName(&aWorldsDiscardedAction);
	InitActionName(&aWorldsUnexaminedAction);
	InitActionName(&aPlanCPUTimeAction);
	InitActionName(&aGetCPUTimeAction);
	InitActionName(&aSetSearchDepthLimitAction);
	InitActionName(&aResetSearchDepthLimitAction);
	InitActionName(&aGetSearchDepthLimitAction);
	InitActionName(&aSearchMaxDepthAction);
	InitActionName(&aSetSearchHeuristicLimitAction);
	InitActionName(&aResetSearchHeuristicLimitAction);
	InitActionName(&aGetSearchHeuristicLimitAction);
	InitActionName(&aSearchMaxHeuristicAction);
	InitActionName(&aDelayedActionAction);
	InitActionName(&aGlobalDelayedActionAction);
	InitActionName(&aWaitForNextEventAction);
	InitActionName(&aMakeLiteralAction);
	InitActionName(&aSetGoalSequenceAction);
	InitActionName(&aSetGoalFormulaAction);
	InitActionName(&aCurrentTimeAction);
	InitActionName(&aInhibitDelayedActionAction);
	InitActionName(&aPlusEqAction);
	InitActionName(&aMinusEqAction);
	InitActionName(&aClearOperatorsAction);
	InitActionName(&aNearestFirstAction);
	InitActionName(&aDeclareElidedOperatorsAction);
	InitActionName(&aLoadPddlProblemAction);
	InitActionName(&aClearEventQueueAction);
	InitActionName(&aClosestFirstAction);
	InitActionName(&aUpdateWorldAction);
	InitActionName(&aLowestFirstAction);
	InitActionName(&aPrintPddlPlanAction);
	InitActionName(&aReachableEventAction);
	InitActionName(&aAllPairsShortestPathAction);
	InitActionName(&aNearestFirstExAction);
	InitActionName(&aClosestFirstExAction);
	InitActionName(&aGoalDistanceAction);
	InitActionName(&aGoalDistanceNegAction);
	InitActionName(&aSetHeuristicExponentAction);
	InitActionName(&aSetMetricDiscountAction);
	InitActionName(&aSetPlanTimeoutAction);
	InitActionName(&aPrintPddl3PlanAction);
	InitActionName(&aSetCountSatPrefsFnAction);
	InitActionName(&aSetTotalPrefsFnAction);
	InitActionName(&aGetBestRelaxedMetricAction);
	InitActionName(&aGetNoWorldFixedPointsAction);
	InitActionName(&aGetNoRealFixedPointsAction);
	InitActionName(&aSetDiscountedMetricMultiplierAction);
	InitActionName(&aSetBestMetricMultiplierAction);
	InitActionName(&aClosedlistLengthAction);
	
	InitActionName(&aSetQualpreferenceNumberAction);
	InitActionName(&aGoalDistanceQualpreferencesAction);
	
	InitActionName(&aVerboseShowExpandedPlansOffAction);
	InitActionName(&aVerboseShowExpandedPlansOnAction);
	InitActionName(&aVerboseShowExpandedWorldsOffAction);
	InitActionName(&aVerboseShowExpandedWorldsOnAction);
	InitActionName(&aVerboseFFShowRelaxedPlansOffAction);
	InitActionName(&aVerboseFFShowRelaxedPlansOnAction);
	InitActionName(&aVerboseFFShowRelaxedWorldsOffAction);
	InitActionName(&aVerboseFFShowRelaxedWorldsOnAction);
	InitActionName(&aSetHelpfulListPriority);
	InitActionName(&aSetDiscountedMetricPriorityAction);
	InitActionName(&aSetOptimisticMetricPriorityAction);
	InitActionName(&aSetBestRelaxedMetricPriorityAction);
	InitActionName(&aSetPreferenceDistancePriorityAction);
	InitActionName(&aSetMetricPriorityAction);
	InitActionName(&aSetMetricMultiplierAction);
	InitActionName(&aGoalDistanceFFPDDLPrefs);


	InitTrueForm();
	InitFalseForm();
}

/* InitStringTable

Description:
	Add the quoted strings to the string hash table.
*/

static void InitStringTable(void)
{
	STABP ps;							/* string table pointer */
	int i;								/* loop index */
	BOOL bErrorFlag;					/* error flag */
	int nPosition;

	ENTER("InitStringTable",FALSE);
	assert(STRING__MAX==sizeof(asStringTable)/sizeof(STAB));
	HashClear(&htStringTable);			/* clear the string table */
	ps=asStringTable;
	bErrorFlag=FALSE;
	for(i=0;i<sizeof(asStringTable)/sizeof(STAB);i++)
	{
		if(asStringTable[i].nIndex!=i)
		{
			ErrorMessage("Bad string table index for string \"%s\"\n",
				asStringTable[i].psString);
			bErrorFlag=TRUE;
		}
		apsStringTab[i]=IdentAllocAndPos(asStringTable[i].psString,&nPosition);		
		ps++;
	}
	if(bErrorFlag)
	{
		EXIT("InitStringTable");
		exit(1);
	}
	EXIT("InitStringTable");
}

/* InitSymbolTable

Description:
	Add the command strings to the symbol hash table.
*/

void InitSymbolTable(void)
{
	ENTER("InitSymbolTable",FALSE);
	HashClear(&htSymbolTable);			/* clear the symbol table */
	EXIT("InitSymbolTable");
}
										
/* ClosePlanner

Description:
	Display planner statistics (before exiting).
    Close open files etc.
*/

DECLSPEC void ClosePlanner(void)
{
	/* display hash table statistics */

	CommandPrintf(stdout,"\nExiting\n");
	CommandPrintf(stdout,"Symbol table usage: %d out of %d entries used.\n",
		HashCount(&htSymbolTable),htSymbolTable.nLimit);
	CommandPrintf(stdout,"String table usage: %d total strings on an open hash table with %d entries. %d entries colliding\n",
		      StrHashGetCount(),StrHashGetTableSize(),StrHashGetCollisions());
	//	StrHashDump();
	if(pfTraceStream!=stderr)
		fclose(pfTraceStream);
	CloseLibraries();
	CloseAllFiles();
}

/* ExecuteList

Description:
	Assume the parsed list is a command and attempt to execute it.
*/

void ExecuteList
(
	LISTP plCommand
)
{
	LISTP pl;
	CELLP pc;
	BOOL bStatus;

	ENTER("ExecuteList",FALSE);
	if(plCommand->nType!=ATOM_LISTP)
	{
		ErrorMessage("ExecuteList: Ill formed list\n");
		EXIT("ExecuteList");
		return;
	}
	pl=plCommand->uValue.plList;
	if(pl)								/* in case of error */
	{
		pcCurrentCommand=ListToFormula(plCommand);
		if(pcCurrentCommand)
		{
			if(pcCurrentCommand->pfForm->paAction->pfEval!=NoEvaluator)
			{
				bStatus=(*pcCurrentCommand->pfForm->paAction->pfEval)
					(pcCurrentCommand,plpCurrentPlan,pbGlobalVariables);
				if(bInteractiveMode)
					CommandPrintf(stdout,bStatus?"True\n":"False\n");
			}
			else if(pcCurrentCommand->pfForm->paAction->pfCompute!=NoComputor)
			{
				pc=(*pcCurrentCommand->pfForm->paAction->pfCompute)
					(pcCurrentCommand,plpCurrentPlan,pbGlobalVariables);
				if(bInteractiveMode)
					PrintFormulaList(stdout,pc);
			}
			else /* if(pcCurrentCommand->pfForm->paAction==&aDummyAction) */
			{
				ErrorMessage("ExecuteList: Unknown command %s\n",pl->psName);
			}
		}
		if(bInteractiveMode&&!bImmediateExit)
			CommandPrintf(stdout,"OK> ");
	}
	pcCurrentCommand=NULL;
	EXIT("ExecuteList");
}

/* TranslateList

Description:
	Assume the parsed list is a formula and attempt to translate it.
*/

void TranslateList
(
	LISTP plCommand
)
{
	ENTER("TranslateList",FALSE);
	if(plCommand->nType!=ATOM_LISTP)
	{
		ErrorMessage("TranslateList: Ill formed list\n");
		EXIT("TranslateList");
		return;
	}
	pcParsedFormula=ListToFormula(plCommand);
	EXIT("TranslateList");
}

/* AddToSymbolTable

Description:
	Allocate a symbol table entry, and fill it in.
*/

SYMBOLP AddToSymbolTable
(
	char *psName,						/* symbol name */
	int nType,							/* symbol type */
	void *pvValue						/* symbol value */
)
{
	SYMBOLP psSymbol;
	int nError;
	int nPosition;

	ENTER("AddToSymbolTable",FALSE);
	nError=HashInsert(&htSymbolTable,psName,(void **)&psSymbol);
	if(nError!=HASH__SUCCESS)
	{
		ErrorMessage("Failed to store symbol %s\n",psName);
		HashError(stderr,nError);
		EXIT("AddToSymbolTable");
		exit(1);
	}
	psSymbol->psName=StrAllocAndPos(psName,&nPosition);
	psSymbol->nType=nType;
	switch(nType)
	{
		case ATOM_STRING:				/* string name is value */
			psSymbol->uValue.psString=psName;
			break;
		case ATOM_INTEGER:
			psSymbol->uValue.nInteger=(int)pvValue;
			break;
		case ATOM_LISTP:
			psSymbol->uValue.plList=(LISTP)pvValue;
			break;
		case ATOM_BTREEP:
			psSymbol->uValue.pbtTree=(BTREEP)pvValue;
			break;
		case ATOM_FUNCTIONP:
			psSymbol->uValue.pfFunction=(CELLP(*)(CELLP))pvValue;
			break;
		case ATOM_SYMBOLINFOP:
			psSymbol->uValue.psiSymbolInfo=(SYMBOLINFOP)pvValue;
			break;
		case ATOM_FORMULAP:
			psSymbol->uValue.pcFormula=(CELLP)pvValue;
			break;
		default:
			ErrorMessage("AddToSymbolTable:  Unexpected type %d\n",nType);
	}
	EXIT("AddToSymbolTable");
	return psSymbol;
}

/* SymbolLookup

Description:
	Search for a string in the symbol hash table.
Returns:
	TRUE on success.
*/

int SymbolLookup
(
	char *psName,						/* symbol name to look for */
	SYMBOLP *ppsSymbol					/* returned pointer to symbol table entry */
)
{
	int nStatus;

	ENTER("SymbolLookup",TRUE);
	nStatus=HashSearch(&htSymbolTable,psName,(void **)ppsSymbol);
	if(nStatus!=HASH__SUCCESS)
		*ppsSymbol=NULL;
	EXIT("SymbolLookup");
	return nStatus==HASH__SUCCESS;
}

/* yyerror

Description:
	Parser error handler.  This routine is called by yyparse().
*/

void yyerror
(
	char *ps							/* message to print */
)
{
	ENTER("yyerror",TRUE);
	ErrorMessage("%s\n",ps);
	EXIT("yyerror");
}

/* yy_input

Description:
	Fetch input for yylex.
*/

#define YY_NULL 0

int yy_input
(
	char *psBuf,
	int nMaxSize
)
{
	int nLength;

	ENTER("yy_input",FALSE);
	if(bImmediateExit)
	{
		EXIT("yy_input");
		return(YY_NULL);
	}
#ifdef WIN32
	if(!pfInput)
	{
		if(!ReadCommand(psBuf,nMaxSize,pfInput))
			return(YY_NULL);
	}
	else
	{
		if(!fgets(psBuf,nMaxSize,pfInput))
		{
			if(!feof(pfInput))
			{
				perror("Failed to read input file");
				exit(1);
			}
			EXIT("yy_input");
			return(YY_NULL);
		}
	}
#else // WIN32

	if(!fgets(psBuf,nMaxSize,pfInput))
	{
		if(!feof(pfInput))
		{
			perror("Failed to read input file");
			exit(1);
		}
		EXIT("yy_input");
		return(YY_NULL);
	}
#endif // WIN32	
	yysol=psBuf;						/* save pointer to start of line */
	nLength=strlen(psBuf);
	EXIT("yy_input");
	return(nLength);
}

/* string_input

Description:
	Pass a string to yylex.
*/

int string_input
(
	char *psBuf,
	int nMaxSize
)
{
	int nLength;
	char *ps;
	int i;

	ENTER("string_input",FALSE);
	if(!*psInput)
	{
		EXIT("string_input");
		return(YY_NULL);
	}
	ps=psBuf;
	for(i=0;i<nMaxSize-1;i++)
	{
		if(!*psInput)
			break;
		*ps++=*psInput++;
	}
	*ps=NULL;
	yysol=psBuf;						/* save pointer to start of line */
	nLength=ps-psBuf;
	EXIT("string_input");
	return(nLength);
}

/* Reserved

Description:
	Check a string to see if it's reserved.
Note:
	We compare strings the hard way since our argument may have come from
	the parser.
*/

BOOL Reserved
(
	char *psString						/* string to check */ 
)
{
	int i;
	
	for(i=0;i<sizeof(asStringTable)/sizeof(STAB);i++)
	{
		if(stricmp(psString,asStringTable[i].psString)==0)
			return TRUE;
	}
	return FALSE;
}

/* NewFormula

Description:
	Allocate and fill in a Formula structure.
	The value of the Formula is yytext.
Note:
	This routine is not used and it's broken...  
	It doesn't use the hash tables to allocate strings.
*/

//CELLP NewFormula
//(
//	int nType,							/* token type */
//	char *yytext,						/* token string */
//	...									/* optional argument */
//)
//{
//	int nLength;						/* string length */
//	char *ps;							/* string pointer */
//	CELLP pc;							/* formula pointer */
//	SYMBOLP psSymbol;
//	va_list pa;							/* argument pointer */
//
//	ENTER("NewFormula",FALSE);
//	nLength=strlen((char *)yytext);
//	if(nType==ATOM_STRING)
//	{
//		ps=(char *)MemAlloc(nLength-1);
//		DoEscapes(ps,(unsigned char *)yytext);
//	}
//	else
//	{
//		ps=(char *)MemAlloc(nLength+1);
//		strcpy(ps,(const char *)yytext);
//	}
//	pc=(CELLP)MemAlloc(sizeof(CELL));
//	pc->pfForm=(FORMULAP)MemAlloc(sizeof(FORMULA));
//	pc->pfForm->nType=nType;
//	pc->pfForm->psName=ps;
//	switch(nType)
//	{
//		case ATOM_IDENT:
//			SymbolLookup(ps,&psSymbol);
//			if(psSymbol)
//			{
//				pc->pfForm->uValue=psSymbol->uValue;
//				pc->pfForm->nType=psSymbol->nType;
//			}
//			else
//				pc->pfForm->nType=ATOM_IDENT;
//			break;
//		case ATOM_INTEGER:
//			pc->pfForm->uValue.nInteger=atoi(ps);
//			break;
//		case ATOM_FLOAT:
//			pc->pfForm->uValue.dfFloat=atof(ps);
//			break;
//		case ATOM_STRING:
//			pc->pfForm->uValue.psString=ps;
//			break;
//		case ATOM_FORMULAP:
//			va_start(pa,yytext);
//			pc->pfForm->uValue.pcFormula=va_arg(pa,CELLP);
//			va_end(pa);
//			break;
//		default:
//			ErrorMessage("Unexpected token type: %d\n",nType);
//	}
///*	pf->nColumn=TabColumn(yysol,yytext-yysol)+(nType==ATOM_STRING?1:0); */
//	EXIT("NewFormula");
//	return(pc);
//}

/* Local Functions ------------------------------------------------------------- */

/* TabColumn

Description:
	Calculate the column location of a token, given its position
	in a string which may contain tabs.
*/

//#define TABSIZE 4
//
//static int TabColumn
//(
//	unsigned char *ps,					/* pointer to string */
//	int nPos							/* position of token */
//)
//{
//	unsigned char *ps1;
//	int i,j;
//
//	ENTER("TabColumn",FALSE);
//	i=j=0;
//	for(ps1=ps;j<nPos;ps1++)
//	{
//		if(*ps1=='\t')
//			i+=TABSIZE-(i%TABSIZE);
//		else
//			i++;
//		j++;
//	}
//	EXIT("TabColumn");
//	return(i);
//}

/* DoEscapes

Description:
	This routine processes a string, stripping off the leading and
	trailing quotes, and interpreting the standard C escapes.
Note:
	This routine never increases the size of the input string.
*/

void DoEscapes
(
	char *pDest,							/* destination buffer */
	unsigned char *pSrc						/* source quoted string */
)
{
	unsigned char *ps;
	char *pd;

	ENTER("DoEscapes",FALSE);
	for(ps=pSrc,pd=pDest;*ps;)
	{
		switch(*ps)
		{
			case '"':					/* ignore quotes */
				ps++;
				break;
			case '\\':					/* escape */
				ps++;
				switch(*ps++)
				{
					case 't':
						*pd++=0x09;
						break;
					case 'n':
						*pd++=0x0A;
						break;
					case 'v':
						*pd++=0x0B;
						break;
					case 'f':
						*pd++=0x0C;
						break;
					case 'r':
						*pd++=0x0D;
						break;
					default:
						*pd=*(ps-1);
				}
				break;
			default:
				*pd++=*ps++;
		}
	}
	*pd=0;								/* terminate string */
	EXIT("DoEscapes");
}

/* MarkCurrentCommand

Description:
	Save the current command.
*/

void MarkCurrentCommand(void)
{
	if(pcCurrentCommand)
		MarkFormula(pcCurrentCommand);
	if(plpCurrentPlan)
		MarkLinearPlan(plpCurrentPlan);
}

#ifdef WIN32

// Command Window Interface ----------------------------------------------------

static HWND hWndCommand;				// command window handle
static HANDLE hReadDoneSemaphore;		// command read buffer semaphore handle
static HANDLE hWriteDoneSemaphore;		// command write buffer semaphore handle
static HANDLE hCommandThread;			// command thread handle
static char acCommandBuffer[512];		// command transfer buffer
static jmp_buf jbOuterJump;				// local longjmp context

static void WINAPI PlannerThreadProc
(
	char *psCmdLine
);

/* InitPlannerThread

Description:
	Run the planner thread and initialize its global resources.
*/

DECLSPEC void InitPlannerThread
(
	HWND hWnd,							// command window handle
	char *psCmdLine						// passed command line
)
{
	DWORD dwThreadId;

	hWndCommand=hWnd;					// save command window handle
	if(!hReadDoneSemaphore)
		hReadDoneSemaphore=CreateSemaphore(0,1,1,"ReadDoneSemaphore");
	else
		ReleaseSemaphore(hReadDoneSemaphore,1,0);
	if(!hWriteDoneSemaphore)
		hWriteDoneSemaphore=CreateSemaphore(0,0,1,"WriteDoneSemaphore");
	else
		WaitForSingleObject(hWriteDoneSemaphore,0);
	hCommandThread=CreateThread(0,0,
		(unsigned long (__stdcall *)(void *))PlannerThreadProc,psCmdLine,0,&dwThreadId);
}	

/* PlannerThreadProc

Description:
	Simulate the planner thread.
*/

static void WINAPI PlannerThreadProc
(
	char *psCmdLine
)
{
	pjbCurrentJump=&jbOuterJump;
	if(!setjmp(jbOuterJump))			/* set up for quick exit to interpreter */
	{
		InitPlanner();					/* initialize planner */
		if(psCmdLine)
			LoadFile(psCmdLine);		/* load the domain file */
	}
	if(!bImmediateExit)
		LoadFile((char *)-1);			/* windows interpreter */
	ClosePlanner();						/* print statistics */
	hCommandThread=0;
	SendMessage(hWndCommand,WM_CLOSE,0,0);
}

/* ReadCommand

Description:
	Fetch a command from the command window (input interface).
*/

static char *ReadCommand
(
	char *pcBuffer,
	int nMaxSize,
	FILE *pfInput
)
{
	int nLength;

	WaitForSingleObject(hWriteDoneSemaphore,INFINITE);
	nLength=strlen(acCommandBuffer);
	if(nLength+2>=nMaxSize)
	{
		ErrorMessage("Command line too long\n");
		ReleaseSemaphore(hReadDoneSemaphore,1,0);
		return 0;
	}
	strcpy(pcBuffer,acCommandBuffer);
	strcpy(pcBuffer+nLength,"\n");
	ReleaseSemaphore(hReadDoneSemaphore,1,0);
	return pcBuffer;
}

/* WriteCommand

Description:
	Pass a command to the planner (window interface).
	If the command includes a leading prompt string, it is stripped from the buffer.
*/

DECLSPEC BOOL WriteCommand
(
	char *psCommand
)
{
	if(WaitForSingleObject(hReadDoneSemaphore,100)==WAIT_TIMEOUT)
		return FALSE;
	strcpy(acCommandBuffer,psCommand);
	ReleaseSemaphore(hWriteDoneSemaphore,1,0);
	return TRUE;
}

#endif // WIN32

/* CommandPuts

Description:
	Write a string to a file or the command window.
*/

DECLSPEC int CommandPuts
(
	char *ps,
	FILE *fs
)
{
#ifdef WIN32
	char acBuffer[512];
	char *pc1,*pc2;

	if(fs!=stdout&&fs!=stderr)			// don't redirect file output
		return fputs(ps,fs);
	if(hWndCommand)						// stdout and stderr go to command window if tlwin 
	{
		// insert a carriage return before every linefeed

		for(pc1=ps,pc2=acBuffer;*pc1;*pc2++=*pc1++)
		{
			if(*pc1=='\n')
				*pc2++='\r';
		}
		*pc2=0;
		SendMessage(hWndCommand,WM_USER+45,0,(LPARAM)acBuffer);
		return 0;
	}
#endif // WIN32
	return fputs(ps,fs);
}

/* CommandPrintf

Description:
	Format and write a string to a file or the command window.
*/

DECLSPEC void CommandPrintf
(
	FILE *pf,
	char *psFormat,
	...
)
{
	va_list pa;							/* argument pointer */
	char acBuffer[512];

	va_start(pa,psFormat);
	vsprintf(acBuffer,psFormat,pa);
	va_end(pa);
	CommandPuts(acBuffer,pf);
}

/* AbortPlanner

Description:
	Abort the planner at the next opportunity.
Notes:
	bUserAbort is handled atomically by the code, so it doesn't need to
	be protected with semaphores.
*/

DECLSPEC void AbortPlanner(void)
{
	bUserAbort=TRUE;
}
