/* to fix:

Mark true formulas by return type and atoms by type:
	Function, Predicate, Generator, Macro
	Integer, Float, String, Ident
Mark dummy formulas with "unknown" and check to make sure none get all the way
	through formula compilation.

ListToFormula should be ListToFormulaList!  All calling routines need to be
	able to handle returned sequences of formulas!  The specialty compilers
	don't handle sequences of formulas correctly.  They need to be recoded.
*/

/* formula.c -- Formula Manipulation Routines

Copyright C, 1997 - 2001, Fahiem Bacchus

*/

#include <setjmp.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef WIN32 
#include <windows.h>
#endif /* WIN32 */

#include "tlplan.h"
#include "adl.h"
#include "cc.h"
#include "compute.h"
#include "current.h"
#include "domain.h"
#include "eval.h"
#include "formula.h"
#include "idle.h"
#include "iface.h"
#include "internal.h"
#include "list.h"
#include "makeform.h"
#include "makegen.h"
#include "progress.h"
#include "queue.h"
#include "strips.h"
#include "tlparse.h"
#include "tl_tab.h"
#include "util.h"
#include "var.h"
#include "world.h"
#include "zone.h"

/* argument types */

#define ARG_UNKNOWN		0
#define ARG_BOOL		1
#define ARG_TERM		2
#define ARG_DESCRIBED	3

/* local function prototypes */

static BOOL ListToVarGen
(
	LISTP plList,						/* input list */
	CELLP *pcVars,						/* output variables list */
	CELLP *pcGen						/* output generator */
);
static void ArgListCheck
(
	CELLP pcFormula,					/* formula to check */ 
	int nType							/* expected argument type */	
);
static CELLP CompileQForm
(
	ACTIONP paAction,					/* action pointer */
	CELLP pcVariables,					/* free variables */
	CELLP pcGenLit,						/* generator formula (such-that clause) */
	CELLP pcFormula
);
static CELLP CompileArrayForm
(
	char *psName,						/* array name */
	CELLP pcArgs
);
static CELLP CompileISpecForm
(
	CELLP pcArg1,
	CELLP pcArg2,
	CELLP pcArg3,
	CELLP pcArg4
);
static CELLP CompilePreForm
(
	CELLP pcVarGens,					/* vargen list */
	CELLP pcFormula 					/* precondition formula */
);
static CELLP CompileVarGenForm
(
	CELLP pcVariables,					/* free variables */
	CELLP pcGenLit						/* generator formula (such-that clause) */
);
static void MarkForm
(
	FORMULAP pfFormula					/* input formula */
);
static void FormSizeOf
(
	FORMULAP pf,						/* input formula */
	int *pnSize
);
static BOOL ListToVars
(
	LISTP plList,						/* input list */
	CELLP *pcVars,						/* output variables list */
	int *pnVars
);
void PrintFlatTerm
(
	FILE *pfStream,						/* output file stream */
	CELLP pcFormula						/* term to display */
);

/* local data */

BOOL bADLPre;							/* adl preconditions flag */

/* Action Tables ------------------------------------------------------------ */

/* Entries are in eval, idle, progress, generator, compute order.  */

ACTION aTrueAction=
{
	"true",
	EvalTrue,
	EvalTrue,
	ProgressTrue,
	CurrentTrue,
	NoGenerator,
	NoComputor,
	MakeCCTrueForm
};
ACTION aFalseAction=
{
	"false",
	EvalFalse,	
	EvalFalse,
	ProgressFalse,
	CurrentFalse,
	NoGenerator,
	NoComputor,
	MakeCCFalseForm
};
ACTION aNotAction=
{
	"not",
	EvalNot,
	IdleNot,
	ProgressNot,	
	CurrentNot,	
	NoGenerator,
	NoComputor,
	MakeCCNotForm
};
ACTION aAndAction=
{
	"and",
	EvalAnd,
	IdleAnd,
	ProgressAnd,
	CurrentAnd,
	NoGenerator,
	NoComputor,
	MakeCCAndForm
};
ACTION aOrAction=
{
	"or",
	EvalOr,
	IdleOr,
	ProgressOr,
	CurrentOr,
	NoGenerator,
	NoComputor,
	MakeCCOrForm
};
ACTION aXorAction=
{
	"xor",
	EvalXor,
	IdleXor,
	ProgressXor,
	CurrentXor,
	NoGenerator,
	NoComputor,
	MakeCCXorForm
};
ACTION aImpliesAction=
{
	"implies",
	EvalImplies,
	IdleImplies,
	ProgressImplies,
	CurrentImplies,
	NoGenerator,
	NoComputor,
	MakeCCImpliesForm
};
ACTION aIfThenElseAction=
{
	"if-then-else",
	EvalIfThenElse,
	IdleIfThenElse,
	ProgressIfThenElse,
	CurrentIfThenElse,
	NoGenerator,
	NoComputor,
	MakeCCIfThenElseForm
};
ACTION aEqAction=
{
	"=",
	EvalEq,
	EvalEq,
	ProgressAtomic,
	CurrentAtomic,
	GenerateEq,
	NoComputor,
	MakeCCAtomicForm
};
ACTION aPlusEqAction=
{
	"+=",
	NoEvaluator,
	NoIdler,
	ProgressAtomic,
	CurrentAtomic,
	NoGenerator,
	NoComputor,
	MakeCCAtomicForm
};
ACTION aMinusEqAction=
{
	"-=",
	NoEvaluator,
	NoIdler,
	ProgressAtomic,
	CurrentAtomic,
	NoGenerator,
	NoComputor,
	MakeCCAtomicForm
};
ACTION aForAllAction=
{
	"forall",
	EvalForAll,
	IdleForAll,
	ProgressForAll,
	CurrentForAll,
	NoGenerator,
	NoComputor,
	MakeCCForAllForm
};
ACTION aExistsAction=
{
	"exists",
	EvalExists,
	IdleExists,	
	ProgressExists,
	CurrentExists,
	NoGenerator,
	NoComputor,
	MakeCCExistsForm
};
ACTION aExistsXAction=
{
	"exists!",
	EvalExistsX,
	IdleExistsX,
	ProgressExistsX,
	CurrentExistsX,
	NoGenerator,
	NoComputor,
	MakeCCExistsXForm
};

ACTION aAlwaysAction=
{
	"always",
	NoEvaluator,
	IdleIgnoreOp,
	ProgressAlways,
	CurrentAlways,
	NoGenerator,
	NoComputor,
	MakeCCAlwaysForm
};
ACTION aEventuallyAction=
{
	"eventually",
	NoEvaluator,
	IdleIgnoreOp,
	ProgressEventually,
	CurrentEventually,
	NoGenerator,
	NoComputor,
	MakeCCEventuallyForm
};
ACTION aNextAction=
{
	"next",
	NoEvaluator,
	IdleIgnoreOp,
	ProgressNext,
	CurrentNext,
	NoGenerator,
	NoComputor,
	MakeCCNextForm
};
ACTION aUntilAction=
{
	"until",
	NoEvaluator,
	IdleUntil,
	ProgressUntil,
	CurrentUntil,
	NoGenerator,
	NoComputor,
	MakeCCUntilForm
};
ACTION aTAlwaysAction=
{
	"t-always",
	NoEvaluator,
	IdleTAlways,
	ProgressTAlways,
	CurrentTAlways,
	NoGenerator,
	NoComputor,
	CCNotImplemented
};
ACTION aTEventuallyAction=
{
	"t-eventually",
	NoEvaluator,
	IdleTEventually,
	ProgressTEventually,
	CurrentTEventually,
	NoGenerator,
	NoComputor,
	CCNotImplemented
};
ACTION aTUntilAction=
{
	"t-until",
	NoEvaluator,
	IdleTUntil,	
	ProgressTUntil,
	CurrentTUntil,
	NoGenerator,
	NoComputor,
	CCNotImplemented
};
ACTION aBindingAction=
{
	"binding",
	NoEvaluator,
	IdleBinding,
	ProgressBinding,
	CurrentBinding,
	NoGenerator,
	NoComputor,
	MakeCCBindingForm
};
ACTION aDeltaAction=
{
	"delta",
	NoEvaluator,
	IdleDelta,
	ProgressDelta,
	CurrentDelta,
	NoGenerator,
	NoComputor,
	CCNotImplemented
};

ACTION aAddAction=
{
	"add",
	EvalAdd,
	NoIdler,
	NoProgressor,
	NoCurrentor,
	NoGenerator,
	NoComputor,
	NoCCForm
};
ACTION aDelAction=
{
	"del",
	EvalDel,
	NoIdler,
	NoProgressor,
	NoCurrentor,
	NoGenerator,
	NoComputor,
	NoCCForm
};

ACTION aPrintWorldAction=
{
	"print-world",
	EvalPrintWorld,
	EvalPrintWorld,
	ProgressAtomic,
	CurrentAtomic,
	NoGenerator,
	NoComputor,
	MakeCCAtomicForm
};
ACTION aPrintDeltaTimeAction=
{
	"print-delta-time",
	EvalPrintDeltaTime,
	EvalPrintDeltaTime,
	ProgressAtomic,
	CurrentAtomic,
	NoGenerator,
	NoComputor,
	MakeCCAtomicForm
};
ACTION aPrintWorldListAction=
{
	"print-world-list",
	EvalPrintWorldList,
	EvalPrintWorldList,
	ProgressAtomic,
	CurrentAtomic,
	NoGenerator,
	NoComputor,
	MakeCCAtomicForm
};

ACTION aPrintAction=
{
	"print",
	EvalPrint,
	EvalPrint,
	ProgressAtomic,
	CurrentAtomic,
	NoGenerator,
	NoComputor,
	MakeCCAtomicForm
};
ACTION aSetPlanNameAction=
{
	"set-plan-name",
	EvalSetPlanName,
	EvalSetPlanName,
	ProgressAtomic,
	CurrentAtomic,
	NoGenerator,
	NoComputor,
	MakeCCAtomicForm
};
ACTION aResetPlanNameAction=
{
	"reset-plan-name",
	EvalResetPlanName,
	EvalResetPlanName,
	ProgressAtomic,
	CurrentAtomic,
	NoGenerator,
	NoComputor,
	MakeCCAtomicForm
};
ACTION aGetPlanNameAction=
{
	"set-plan-name",
	NoEvaluator,
	NoIdler,
	NoProgressor,
	NoCurrentor,
	NoGenerator,
	ComputeGetPlanName,
	NoCCForm
};
ACTION aExitAction=
{
	"exit",
	EvalExit,
	EvalExit,
	ProgressAtomic,
	CurrentAtomic,
	NoGenerator,
	NoComputor,
	MakeCCAtomicForm
};
ACTION aAssignAction=
{
	":=",
	EvalAssign,
	EvalAssign,
	ProgressAtomic,
	CurrentAtomic,
	NoGenerator,
	NoComputor,
	MakeCCAtomicForm
};
ACTION aAssignAppendAction=
{
	":<<",
	EvalAssignAppend,
	EvalAssignAppend,
	ProgressAtomic,
	CurrentAtomic,
	NoGenerator,
	NoComputor,
	MakeCCAtomicForm
};
ACTION aSearchGlobalInitializationAction=
{
	"search-global-initialization",
	EvalSearchGlobalInitialization,
	EvalSearchGlobalInitialization,
	ProgressAtomic,
	CurrentAtomic,
	NoGenerator,
	NoComputor,
	MakeCCAtomicForm
};

ACTION aGoalAction=
{
	"goal",
	EvalGoal,
	EvalGoal,
	ProgressGoal,
	CurrentGoal,
	GenerateGoal,
	ComputeGoal,
	MakeCCAtomicForm
};
ACTION aCurrentAction=
{
	"current",
	EvalCurrent,
	EvalCurrent,
	ProgressCurrent,
	CurrentCurrent,
	GenerateCurrent,
	ComputeCurrent,
	MakeCCAtomicForm
};
ACTION aPreviousAction=
{
	"previous",
	EvalPrevious,
	EvalPrevious,
	ProgressPrevious,
	CurrentPrevious,
	GeneratePrevious,
	ComputePrevious,
	MakeCCAtomicForm
};
ACTION aPermuteAction=
{
	"permute",
	NoEvaluator,
	NoIdler,
	NoProgressor,
	NoCurrentor,
	GeneratePermute,
	NoComputor,
	NoCCForm
};

ACTION aArrayAction=
{
	NULL,
	NoEvaluator,
	NoIdler,
	NoProgressor,
	NoCurrentor,
	NoGenerator,
	ComputeArray,
	NoCCForm
};
ACTION aAStarAction=
{
	"a*",
	NoEvaluator,
	NoIdler,
	NoProgressor,
	NoCurrentor,
	NoGenerator,
	ComputeAStar,
	NoCCForm
};
ACTION aOptimalCostAction=
{
	"optimal-cost",
	NoEvaluator,
	NoIdler,
	NoProgressor,
	NoCurrentor,
	NoGenerator,
	ComputeOptimalCost,
	NoCCForm
};
ACTION aBestActionAction=
{
	"best-action",
	NoEvaluator,
	NoIdler,
	NoProgressor,
	NoCurrentor,
	NoGenerator,
	ComputeBestAction,
	NoCCForm
};
ACTION aPlanLengthAction=
{
	"plan-length",
	NoEvaluator,
	NoIdler,
	NoProgressor,
	NoCurrentor,
	NoGenerator,
	ComputePlanLength,
	NoCCForm
};
ACTION aPlanCostAction=
{
	"plan-cost",
	NoEvaluator,
	NoIdler,
	NoProgressor,
	NoCurrentor,
	NoGenerator,
	ComputePlanCost,
	NoCCForm
};
ACTION aPlanDurationAction=
{
	"plan-duration",
	NoEvaluator,
	NoIdler,
	NoProgressor,
	NoCurrentor,
	NoGenerator,
	ComputePlanDuration,
	NoCCForm
};
ACTION aActionNameAction=
{
	"action-name",
	NoEvaluator,
	NoIdler,
	NoProgressor,
	NoCurrentor,
	NoGenerator,
	ComputeActionName,
	NoCCForm
};
ACTION aActionCostAction=
{
	"action-cost",
	NoEvaluator,
	NoIdler,
	NoProgressor,
	NoCurrentor,
	NoGenerator,
	ComputeActionCost,
	NoCCForm
};
ACTION aActionDurationAction=
{
	"action-duration",
	NoEvaluator,
	NoIdler,
	NoProgressor,
	NoCurrentor,
	NoGenerator,
	ComputeActionDuration,
	NoCCForm
};
ACTION aActionPriorityAction=
{
	"action-priority",
	NoEvaluator,
	NoIdler,
	NoProgressor,
	NoCurrentor,
	NoGenerator,
	ComputeActionPriority,
	NoCCForm
};
ACTION aWorldNumberAction=
{
	"world-number",
	NoEvaluator,
	NoIdler,
	NoProgressor,
	NoCurrentor,
	NoGenerator,
	ComputeWorldNumber,
	NoCCForm
};
//ACTION aWorldHeuristicRankAction=
//{
//	"world-heuristic-rank",
//	NoEvaluator,
//	NoIdler,
//	NoProgressor,
//	NoCurrentor,
//	NoGenerator,
//	ComputeWorldHeuristicRank,
//	NoCCForm
//};

ACTION aPreAction=						/* dummy action */
{
	"pre",
	NoEvaluator,
	NoIdler,
	NoProgressor,
	NoCurrentor,
	NoGenerator,
	NoComputor,
	NoCCForm
};
ACTION aVarGenAction=
{
	"vargen",
	NoEvaluator,
	NoIdler,
	NoProgressor,
	NoCurrentor,
	NoGenerator,
	NoComputor,
	NoCCForm
};

ACTION aLiteralAction=
{
	NULL,
	NoEvaluator,
	NoIdler,
	NoProgressor,
	NoCurrentor,
	NoGenerator,
	ComputeLiteral,
	NoCCForm
};
ACTION aIdentAction=
{
	NULL,
	NoEvaluator,
	NoIdler,
	NoProgressor,
	NoCurrentor,
	NoGenerator,
	ComputeVariable,
	NoCCForm
};
ACTION aFloatAction=
{
	NULL,
	NoEvaluator,
	NoIdler,
	NoProgressor,
	NoCurrentor,
	NoGenerator,
	ComputeLiteral,
	NoCCForm
};
ACTION aIntegerAction=
{
	NULL,
	NoEvaluator,
	NoIdler,
	NoProgressor,
	NoCurrentor,
	NoGenerator,
	ComputeLiteral,
	NoCCForm
};
ACTION aStringAction=
{
	NULL,
	NoEvaluator,
	NoIdler,
	NoProgressor,
	NoCurrentor,
	NoGenerator,
	ComputeLiteral,
	NoCCForm
};
ACTION aISpecAction=
{
	NULL,
	NoEvaluator,
	NoIdler,
	NoProgressor,
	NoCurrentor,
	NoGenerator,
	ComputeLiteral,
	NoCCForm
};

ACTION aDefPredicateAction=
{
	NULL,
	EvalDefPredicate,
	EvalDefPredicate,
	ProgressAtomic,	
	CurrentAtomic,
	NoGenerator,
	NoComputor,
	MakeCCAtomicForm
};
ACTION aDefGeneratorAction=
{
	NULL,
	NoEvaluator,
	NoIdler,
	NoProgressor,	
	NoCurrentor,
	GenerateDefGenerator,
	NoComputor,
	NoCCForm
};
ACTION aDefFunctionAction=
{
	NULL,
	NoEvaluator,
	NoIdler,
	NoProgressor,
	NoCurrentor,
	NoGenerator,
	ComputeDefFunction,
	NoCCForm
};
ACTION aDefMacroAction=
{
	NULL,
	NoEvaluator,
	NoIdler,
	NoProgressor,
	NoCurrentor,
	NoGenerator,
//	ComputeDefMacro
	ComputeDefFunction,
	NoCCForm
};

ACTION aModifyWorldAction=
{
	"modify-world",
	EvalModifyWorld,
	EvalModifyWorld,
	ProgressModifyWorld,
	CurrentModifyWorld,
	NoGenerator,
	NoComputor,
	MakeCCAtomicForm
};
ACTION aOperatorAction=
{
	NULL,
	NoEvaluator,
	NoIdler,
	NoProgressor,
	NoCurrentor,
	NoGenerator,
	NoComputor,
	NoCCForm
};


ACTION aDummyAction=					/* dummy formulas -- available only in the right context */
{
	NULL,
	NoEvaluator,
	NoIdler,
	NoProgressor,
	NoCurrentor,
	NoGenerator,
	NoComputor,
	NoCCForm
};

/* former commands */

ACTION aClearWorldSymbolsAction=
{
	"clear-world-symbols",
	EvalClearWorldSymbols,
	EvalClearWorldSymbols,
	ProgressAtomic,
	CurrentAtomic,
	NoGenerator,
	NoComputor,
	MakeCCAtomicForm
};
ACTION aClearOperatorsAction=
{
	"clear-operators",
	EvalClearOperators,
	EvalClearOperators,
	ProgressAtomic,
	CurrentAtomic,
	NoGenerator,
	NoComputor,
	MakeCCAtomicForm
};
ACTION aDeclareDefinedSymbolsAction=
{
	"declare-defined-symbols",
	EvalDeclareDefinedSymbols,
	EvalDeclareDefinedSymbols,
	ProgressAtomic,
	CurrentAtomic,
	NoGenerator,
	NoComputor,
	MakeCCAtomicForm
};
ACTION aDeclareDescribedSymbolsAction=
{
	"declare-described-symbols",
	EvalDeclareDescribedSymbols,
	EvalDeclareDescribedSymbols,
	ProgressAtomic,
	CurrentAtomic,
	NoGenerator,
	NoComputor,
	MakeCCAtomicForm
};
ACTION aDeclareExternalSymbolsAction=
{
	"declare-external-symbols",
	EvalDeclareExternalSymbols,
	EvalDeclareExternalSymbols,	
	ProgressAtomic,
	CurrentAtomic,
	NoGenerator,
	NoComputor,
	MakeCCAtomicForm
};
ACTION aDeclareGlobalVariablesAction=
{
	"declare-global-variables",
	EvalDeclareGlobalVariables,
	EvalDeclareGlobalVariables,
	ProgressAtomic,
	CurrentAtomic,
	NoGenerator,
	NoComputor,
	MakeCCAtomicForm
};
ACTION aDeclareMacroOperatorsAction=
{
	"declare-macro-operators",
	EvalDeclareMacroOperators,
	EvalDeclareMacroOperators,
	ProgressAtomic,
	CurrentAtomic,
	NoGenerator,
	NoComputor,
	MakeCCAtomicForm
};
ACTION aDeclareElidedOperatorsAction=
{
	"declare-elided-operators",
	EvalDeclareElidedOperators,
	EvalDeclareElidedOperators,
	ProgressAtomic,
	CurrentAtomic,
	NoGenerator,
	NoComputor,
	MakeCCAtomicForm
};
ACTION aDefineAction=
{
	"define",
	EvalDefine,
	EvalDefine,
	ProgressAtomic,
	CurrentAtomic,
	NoGenerator,
	NoComputor,
	MakeCCAtomicForm
};
ACTION aDefADLOperatorAction=
{
	"def-adl-operator",
	EvalDefADLOperator,
	EvalDefADLOperator,
	ProgressAtomic,
	CurrentAtomic,
	NoGenerator,
	NoComputor,
	MakeCCAtomicForm
};
ACTION aDefDefinedFunctionAction=
{
	"def-defined-function",
	EvalDefDefinedFunction,
	EvalDefDefinedFunction,
	ProgressAtomic,
	CurrentAtomic,
	NoGenerator,
	NoComputor,
	MakeCCAtomicForm
};
ACTION aDefDefinedGeneratorAction=
{
	"def-defined-generator",
	EvalDefDefinedGenerator,
	EvalDefDefinedGenerator,
	ProgressAtomic,
	CurrentAtomic,
	NoGenerator,
	NoComputor,
	MakeCCAtomicForm
};
ACTION aDefDefinedMacroAction=
{
	"def-defined-macro",
	EvalDefDefinedMacro,
	EvalDefDefinedMacro,
	ProgressAtomic,
	CurrentAtomic,
	NoGenerator,
	NoComputor,
	MakeCCAtomicForm
};
ACTION aDefDefinedPredicateAction=
{
	"def-defined-predicate",
	EvalDefDefinedPredicate,
	EvalDefDefinedPredicate,
	ProgressAtomic,
	CurrentAtomic,
	NoGenerator,
	NoComputor,
	MakeCCAtomicForm
};
ACTION aDefDomainAction=
{
	"def-domain",
	EvalDefDomain,
	EvalDefDomain,
	ProgressAtomic,
	CurrentAtomic,
	NoGenerator,
	NoComputor,
	MakeCCAtomicForm
};
ACTION aDefStripsOperatorAction=
{
	"def-strips-operator",
	EvalDefStripsOperator,
	EvalDefStripsOperator,
	ProgressAtomic,
	CurrentAtomic,
	NoGenerator,
	NoComputor,
	MakeCCAtomicForm
};
ACTION aGetHeuristicFnAction=
{
	"get-heuristic-fn",
	NoEvaluator,
	NoIdler,
	NoProgressor,
	NoCurrentor,
	NoGenerator,
	ComputeGetHeuristicFn,
	NoCCForm
};
ACTION aHeuristicFnAction=
{
	"heuristic-fn",
	NoEvaluator,
	NoIdler,
	NoProgressor,
	NoCurrentor,
	NoGenerator,
	ComputeHeuristicFn,
	NoCCForm
};
ACTION aGetPriorityFnAction=
{
	"get-priority-fn",
	NoEvaluator,
	NoIdler,
	NoProgressor,
	NoCurrentor,
	NoGenerator,
	ComputeGetPriorityFn,
	NoCCForm
};
ACTION aGetSearchLimitAction=
{
	"get-search-limit",
	NoEvaluator,
	NoIdler,
	NoProgressor,
	NoCurrentor,
	NoGenerator,
	ComputeGetSearchLimit,
	NoCCForm
};
ACTION aGetSearchStrategyAction=
{
	"get-search-strategy",
	NoEvaluator,
	NoIdler,
	NoProgressor,
	NoCurrentor,
	NoGenerator,
	ComputeGetSearchStrategy,
	NoCCForm
};
ACTION aGetTLControlAction=
{
	"get-tl-control",
	NoEvaluator,
	NoIdler,
	NoProgressor,
	NoCurrentor,
	NoGenerator,
	ComputeGetTLControl,
	NoCCForm
};
ACTION aGetGoalAddendumAction=
{
	"get-goal-addendum",
	NoEvaluator,
	NoIdler,
	NoProgressor,
	NoCurrentor,
	NoGenerator,
	ComputeGetGoalAddendum,
	NoCCForm
};
ACTION aGetTraceLevelAction=
{
	"get-trace-level",
	NoEvaluator,
	NoIdler,
	NoProgressor,
	NoCurrentor,
	NoGenerator,
	ComputeGetTraceLevel,
	NoCCForm
};
ACTION aListDomainsAction=
{
	"list-domains",
	EvalListDomains,
	EvalListDomains,
	ProgressAtomic,
	CurrentAtomic,
	NoGenerator,
	NoComputor,
	MakeCCAtomicForm
};
ACTION aLoadDomainAction=
{
	"load-domain",
	EvalLoadDomain,
	EvalLoadDomain,
	ProgressAtomic,
	CurrentAtomic,
	NoGenerator,
	NoComputor,
	MakeCCAtomicForm
};
ACTION aLoadFileAction=
{
	"load-file",
	EvalLoadFile,
	EvalLoadFile,
	ProgressAtomic,
	CurrentAtomic,
	NoGenerator,
	NoComputor,
	MakeCCAtomicForm
};
ACTION aLoadPddlProblemAction=
{
	"load-pddl-problem",
	EvalLoadPddlProblem,
	EvalLoadPddlProblem,
	ProgressAtomic,
	CurrentAtomic,
	NoGenerator,
	NoComputor,
	MakeCCAtomicForm
};
ACTION aLoadPlanAction=
{
	"load-plan",
	EvalLoadPlan,
	EvalLoadPlan,
	ProgressAtomic,
	CurrentAtomic,
	NoGenerator,
	NoComputor,
	MakeCCAtomicForm
};
ACTION aPlanAction=
{
	"plan",
	EvalPlan,
	NoIdler,
	NoProgressor,
	NoCurrentor,
	NoGenerator,
	NoComputor,
	MakeCCAtomicForm
};
ACTION aClearEventQueueAction=
{
	"clear-event-queue",
	EvalClearEventQueue,
	NoIdler,
	NoProgressor,
	NoCurrentor,
	NoGenerator,
	NoComputor,
	MakeCCAtomicForm
};
ACTION aPrintPlanListAction=
{
	"print-plan-list",
	EvalPrintPlanList,
	EvalPrintPlanList,
	ProgressAtomic,
	CurrentAtomic,
	NoGenerator,
	NoComputor,
	MakeCCAtomicForm
};
ACTION aPrintPddlPlanAction=
{
	"print-pddl-plan",
	EvalPrintPddlPlan,
	EvalPrintPddlPlan,
	ProgressAtomic,
	CurrentAtomic,
	NoGenerator,
	NoComputor,
	MakeCCAtomicForm
};
ACTION aResetDomainsAction=
{
	"reset-domains",
	EvalResetDomains,
	EvalResetDomains,
	ProgressAtomic,
	CurrentAtomic,
	NoGenerator,
	NoComputor,
	MakeCCAtomicForm
};
ACTION aResetHeuristicFnAction=
{
	"reset-heuristic-fn",
	EvalResetHeuristicFn,
	EvalResetHeuristicFn,
	ProgressAtomic,
	CurrentAtomic,
	NoGenerator,
	NoComputor,
	MakeCCAtomicForm
};
ACTION aResetPrintWorldFnAction=
{
	"reset-print-world-fn",
	EvalResetPrintWorldFn,
	EvalResetPrintWorldFn,
	ProgressAtomic,
	CurrentAtomic,
	NoGenerator,
	NoComputor,
	MakeCCAtomicForm
};
ACTION aResetPriorityFnAction=
{
	"reset-priority-fn",
	EvalResetPriorityFn,
	EvalResetPriorityFn,
	ProgressAtomic,
	CurrentAtomic,
	NoGenerator,
	NoComputor,
	MakeCCAtomicForm
};
ACTION aResetSearchLimitAction=
{
	"reset-search-limit",
	EvalResetSearchLimit,
	EvalResetSearchLimit,
	ProgressAtomic,
	CurrentAtomic,
	NoGenerator,
	NoComputor,
	MakeCCAtomicForm
};
ACTION aResetSearchStrategyAction=
{
	"reset-search-strategy",
	EvalResetSearchStrategy,
	EvalResetSearchStrategy,
	ProgressAtomic,
	CurrentAtomic,
	NoGenerator,
	NoComputor,
	MakeCCAtomicForm
};
ACTION aResetTLControlAction=
{
	"reset-tl-control",
	EvalResetTLControl,
	EvalResetTLControl,
	ProgressAtomic,
	CurrentAtomic,
	NoGenerator,
	NoComputor,
	MakeCCAtomicForm
};
ACTION aSetControlAction=
{
	"set-control",
	EvalSetControl,
	EvalSetControl,
	ProgressAtomic,
	CurrentAtomic,
	NoGenerator,
	NoComputor,
	MakeCCAtomicForm
};
ACTION aEnableAction=
{
	"enable",
	EvalEnable,
	EvalEnable,
	ProgressAtomic,
	CurrentAtomic,
	NoGenerator,
	NoComputor,
	MakeCCAtomicForm
};
ACTION aDisableAction=
{
	"disable",
	EvalDisable,
	EvalDisable,
	ProgressAtomic,
	CurrentAtomic,
	NoGenerator,
	NoComputor,
	MakeCCAtomicForm
};
ACTION aSetGoalAddendumAction=
{
	"set-goal-addendum",
	EvalSetGoalAddendum,
	EvalSetGoalAddendum,
	ProgressAtomic,
	CurrentAtomic,
	NoGenerator,
	NoComputor,
	MakeCCAtomicForm
};
ACTION aResetGoalAddendumAction=
{
	"reset-goal-addendum",
	EvalResetGoalAddendum,
	EvalResetGoalAddendum,
	ProgressAtomic,
	CurrentAtomic,
	NoGenerator,
	NoComputor,
	MakeCCAtomicForm
};
ACTION aSetHeuristicFnAction=
{
	"set-heuristic-fn",
	EvalSetHeuristicFn,
	EvalSetHeuristicFn,
	ProgressAtomic,
	CurrentAtomic,
	NoGenerator,
	NoComputor,
	MakeCCAtomicForm
};
ACTION aSetInitialFactsAction=
{
	"set-initial-facts",
	EvalSetInitialFacts,
	EvalSetInitialFacts,
	ProgressAtomic,
	CurrentAtomic,
	NoGenerator,
	NoComputor,
	MakeCCAtomicForm
};
ACTION aSetInitialWorldAction=
{
	"set-initial-world",
	EvalSetInitialWorld,
	EvalSetInitialWorld,
	ProgressAtomic,
	CurrentAtomic,
	NoGenerator,
	NoComputor,
	MakeCCAtomicForm
};
ACTION aSetInitializationSequenceAction=
{
	"set-initialization-sequence",
	EvalSetInitializationSequence,
	EvalSetInitializationSequence,
	ProgressAtomic,
	CurrentAtomic,
	NoGenerator,
	NoComputor,
	MakeCCAtomicForm
};
ACTION aSetGoalAction=
{
	"set-goal",
	EvalSetGoal,
	EvalSetGoal,
	ProgressAtomic,
	CurrentAtomic,
	NoGenerator,
	NoComputor,
	MakeCCAtomicForm
};
ACTION aSetGoalSequenceAction=
{
	"set-goal-sequence",
	EvalSetGoalSequence,
	EvalSetGoalSequence,
	ProgressAtomic,
	CurrentAtomic,
	NoGenerator,
	NoComputor,
	MakeCCAtomicForm
};
ACTION aSetGoalFormulaAction=
{
	"set-goal-formula",
	EvalSetGoalFormula,
	EvalSetGoalFormula,
	ProgressAtomic,
	CurrentAtomic,
	NoGenerator,
	NoComputor,
	MakeCCAtomicForm
};
ACTION aSetGoalTypeAction=
{
	"set-goal-type",
	EvalSetGoalType,
	EvalSetGoalType,
	ProgressAtomic,
	CurrentAtomic,
	NoGenerator,
	NoComputor,
	MakeCCAtomicForm
};
ACTION aSetPrintWorldFnAction=
{
	"set-print-world-fn",
	EvalSetPrintWorldFn,
	EvalSetPrintWorldFn,
	ProgressAtomic,
	CurrentAtomic,
	NoGenerator,
	NoComputor,
	MakeCCAtomicForm
};
ACTION aSetPriorityFnAction=
{
	"set-priority-fn",
	EvalSetPriorityFn,
	EvalSetPriorityFn,
	ProgressAtomic,
	CurrentAtomic,
	NoGenerator,
	NoComputor,
	MakeCCAtomicForm
};
ACTION aSetSearchLimitAction=
{
	"set-search-limit",
	EvalSetSearchLimit,
	EvalSetSearchLimit,
	ProgressAtomic,
	CurrentAtomic,
	NoGenerator,
	NoComputor,
	MakeCCAtomicForm
};
ACTION aSetSearchStrategyAction=
{
	"set-search-strategy",
	EvalSetSearchStrategy,
	EvalSetSearchStrategy,
	ProgressAtomic,
	CurrentAtomic,
	NoGenerator,
	NoComputor,
	MakeCCAtomicForm
};
ACTION aSetStatisticsFileAction=
{
	"set-statistics-file",
	EvalSetStatisticsFile,
	EvalSetStatisticsFile,
	ProgressAtomic,
	CurrentAtomic,
	NoGenerator,
	NoComputor,
	MakeCCAtomicForm
};
ACTION aSetTLControlAction=
{
	"set-tl-control",
	EvalSetTLControl,
	EvalSetTLControl,
	ProgressAtomic,
	CurrentAtomic,
	NoGenerator,
	NoComputor,
	MakeCCAtomicForm
};
ACTION aSetTraceLevelAction=
{
	"set-trace-level",
	EvalSetTraceLevel,
	EvalSetTraceLevel,
	ProgressAtomic,
	CurrentAtomic,
	NoGenerator,
	NoComputor,
	MakeCCAtomicForm
};
ACTION aVerboseOffAction=
{
	"verbose-off",
	EvalVerboseOff,
	EvalVerboseOff,
	ProgressAtomic,
	CurrentAtomic,
	NoGenerator,
	NoComputor,
	MakeCCAtomicForm
};
ACTION aVerboseOnAction=
{
	"verbose-on",
	EvalVerboseOn,
	EvalVerboseOn,
	ProgressAtomic,
	CurrentAtomic,
	NoGenerator,
	NoComputor,
	MakeCCAtomicForm
};
ACTION aUpdateWorldAction=
{
	"update-world",
	EvalUpdateWorld,
	EvalUpdateWorld,
	ProgressAtomic,
	CurrentAtomic,
	NoGenerator,
	NoComputor,
	MakeCCAtomicForm
};

/* former internal symbols */

ACTION aLtAction=
{
	"<",
	EvalLt,
	EvalLt,
	ProgressAtomic,
	CurrentAtomic,
	GenerateLt,
	NoComputor,
	MakeCCAtomicForm
};
ACTION aLeAction=
{
	"<=",
	EvalLe,
	EvalLe,
	ProgressAtomic,
	CurrentAtomic,
	GenerateLe,
	NoComputor,
	MakeCCAtomicForm
};
ACTION aGtAction=
{
	">",
	EvalGt,
	EvalGt,
	ProgressAtomic,
	CurrentAtomic,
	GenerateGt,
	NoComputor,
	MakeCCAtomicForm
};
ACTION aGeAction=
{
	">=",
	EvalGe,
	EvalGe,
	ProgressAtomic,
	CurrentAtomic,
	GenerateGe,
	NoComputor,
	MakeCCAtomicForm
};
ACTION aSeedAction=
{
	"seed",
	EvalSeed,
	EvalSeed,
	ProgressAtomic,
	CurrentAtomic,
	NoGenerator,
	NoComputor,
	MakeCCAtomicForm
};

ACTION aRandAction=
{
	"rand",
	NoEvaluator,
	NoIdler,
	NoProgressor,
	NoCurrentor,
	NoGenerator,
	ComputeRand,
	NoCCForm
};
ACTION aRandomAction=
{
	"random",
	NoEvaluator,
	NoIdler,
	NoProgressor,
	NoCurrentor,
	NoGenerator,
	ComputeRandom,
	NoCCForm
};
ACTION aExpAction=
{
	"exp",
	NoEvaluator,
	NoIdler,
	NoProgressor,
	NoCurrentor,
	NoGenerator,
	ComputeExp,
	NoCCForm
};
ACTION aLogAction=
{
	"log",
	NoEvaluator,
	NoIdler,
	NoProgressor,
	NoCurrentor,
	NoGenerator,
	ComputeLog,
	NoCCForm
};
ACTION aRoundAction=
{
	"round",
	NoEvaluator,
	NoIdler,
	NoProgressor,
	NoCurrentor,
	NoGenerator,
	ComputeRound,
	NoCCForm
};
ACTION aIntAction=
{
	"int",
	NoEvaluator,
	NoIdler,
	NoProgressor,
	NoCurrentor,
	NoGenerator,
	ComputeInt,
	NoCCForm
};
ACTION aFloorAction=
{
	"floor",
	NoEvaluator,
	NoIdler,
	NoProgressor,
	NoCurrentor,
	NoGenerator,
	ComputeFloor,
	NoCCForm
};
ACTION aCeilAction=
{
	"ceil",
	NoEvaluator,
	NoIdler,
	NoProgressor,
	NoCurrentor,
	NoGenerator,
	ComputeCeil,
	NoCCForm
};
ACTION aPlusAction=
{
	"+",
	NoEvaluator,
	NoIdler,
	NoProgressor,
	NoCurrentor,
	NoGenerator,
	ComputePlus,
	NoCCForm
};
ACTION aMinusAction=
{
	"-",
	NoEvaluator,
	NoIdler,
	NoProgressor,
	NoCurrentor,
	NoGenerator,
	ComputeMinus,
	NoCCForm
};
ACTION aMultiplyAction=
{
	"*",
	NoEvaluator,
	NoIdler,
	NoProgressor,
	NoCurrentor,
	NoGenerator,
	ComputeMultiply,
	NoCCForm
};
ACTION aDivideAction=
{
	"/",
	NoEvaluator,
	NoIdler,
	NoProgressor,
	NoCurrentor,
	NoGenerator,
	ComputeDivide,
	NoCCForm
};
ACTION aModAction=
{
	"mod",
	NoEvaluator,
	NoIdler,
	NoProgressor,
	NoCurrentor,
	NoGenerator,
	ComputeMod,
	NoCCForm
};
ACTION aMaxAction=
{
	"max",
	NoEvaluator,
	NoIdler,
	NoProgressor,
	NoCurrentor,
	NoGenerator,
	ComputeMax,
	NoCCForm
};
ACTION aMinAction=
{
	"min",
	NoEvaluator,
	NoIdler,
	NoProgressor,
	NoCurrentor,
	NoGenerator,
	ComputeMin,
	NoCCForm
};
ACTION aExptAction=
{
	"expt",
	NoEvaluator,
	NoIdler,
	NoProgressor,
	NoCurrentor,
	NoGenerator,
	ComputeExpt,
	NoCCForm
};
ACTION aSqrtAction=
{
	"sqrt",
	NoEvaluator,
	NoIdler,
	NoProgressor,
	NoCurrentor,
	NoGenerator,
	ComputeSqrt,
	NoCCForm
};
ACTION aAbsAction=
{
	"abs",
	NoEvaluator,
	NoIdler,
	NoProgressor,
	NoCurrentor,
	NoGenerator,
	ComputeAbs,
	NoCCForm
};
		
ACTION aPosIntAction=
{
	"pos-int",
	NoEvaluator,
	NoIdler,
	NoProgressor,
	NoCurrentor,
	GeneratePosInt,
	NoComputor,
	NoCCForm
};
ACTION aLtPosIntAction=
{
	"<-pos-int",
	NoEvaluator,
	NoIdler,
	NoProgressor,
	NoCurrentor,
	GenerateLtPosInt,
	NoComputor,
	NoCCForm
};
ACTION aIsBetweenAction=
{
	"is-between",
	EvalIsBetween,
	EvalIsBetween,
	ProgressAtomic,
	CurrentAtomic,
	GenerateIsBetween,
	NoComputor,
	MakeCCAtomicForm
};
ACTION aNearestFirstAction=
{
	"nearest-first",
	NoEvaluator,
	NoIdler,
	NoProgressor,
	NoCurrentor,
	GenerateNearestFirst,
	NoComputor,
	NoCCForm
};
ACTION aNearestFirstExAction=
{
	"nearest-first-ex",
	NoEvaluator,
	NoIdler,
	NoProgressor,
	NoCurrentor,
	GenerateNearestFirstEx,
	NoComputor,
	NoCCForm
};
ACTION aClosestFirstAction=
{
	"closest-first",
	NoEvaluator,
	NoIdler,
	NoProgressor,
	NoCurrentor,
	GenerateClosestFirst,
	NoComputor,
	NoCCForm
};
ACTION aClosestFirstExAction=
{
	"closest-first-ex",
	NoEvaluator,
	NoIdler,
	NoProgressor,
	NoCurrentor,
	GenerateClosestFirstEx,
	NoComputor,
	NoCCForm
};
ACTION aLowestFirstAction=
{
	"lowest-first",
	NoEvaluator,
	NoIdler,
	NoProgressor,
	NoCurrentor,
	GenerateLowestFirst,
	NoComputor,
	NoCCForm
};
ACTION aAllPairsShortestPathAction=
{
	"all-pairs-shortest-path",
	NoEvaluator,
	NoIdler,
	NoProgressor,
	NoCurrentor,
	GenerateAllPairsShortestPath,
	NoComputor,
	NoCCForm
};
//ACTION aFewestEdgesPathAction=
//{
//	"fewest-edges-path",
//	NoEvaluator,
//	NoIdler,
//	NoProgressor,
//	NoCurrentor,
//	GenerateFewestEdgesPath,
//	NoComputor,
//	NoCCForm
//};
//ACTION aLowestCostPathAction=
//{
//	"lowest-cost-path",
//	NoEvaluator,
//	NoIdler,
//	NoProgressor,
//	NoCurrentor,
//	GenerateLowestCostPath,
//	NoComputor,
//	NoCCForm
//};
ACTION aInTheSetAction=
{
	"in-the-set",
	EvalInTheSet,
	EvalInTheSet,
	ProgressAtomic,
	CurrentAtomic,
	GenerateInTheSet,
	NoComputor,
	MakeCCAtomicForm
};

ACTION aSelectInitialWorldAction=
{
	"select-initial-world",
	EvalSelectInitialWorld,
	EvalSelectInitialWorld,
	ProgressAtomic,
	CurrentAtomic,
	NoGenerator,
	NoComputor,
	MakeCCAtomicForm
};
ACTION aSelectFinalWorldAction=
{
	"select-final-world",
	EvalSelectFinalWorld,
	EvalSelectFinalWorld,
	ProgressAtomic,
	CurrentAtomic,
	NoGenerator,
	NoComputor,
	MakeCCAtomicForm
};
ACTION aSelectNextWorldAction=
{
	"select-next-world",
	EvalSelectNextWorld,
	EvalSelectNextWorld,
	ProgressAtomic,
	CurrentAtomic,
	NoGenerator,
	NoComputor,
	MakeCCAtomicForm
};
ACTION aSelectPreviousWorldAction=
{
	"select-previous-world",
	EvalSelectPreviousWorld,
	EvalSelectPreviousWorld,
	ProgressAtomic,
	CurrentAtomic,
	NoGenerator,
	NoComputor,
	MakeCCAtomicForm
};
ACTION aCloseFileAction=
{
	"close-file",
	EvalCloseFile,
	EvalCloseFile,
	ProgressAtomic,
	CurrentAtomic,
	NoGenerator,
	NoComputor,
	MakeCCAtomicForm
};
ACTION aRedirectAction=
{
	"redirect",
	EvalRedirect,
	EvalRedirect,
	ProgressAtomic,
	CurrentAtomic,
	NoGenerator,
	NoComputor,
	MakeCCAtomicForm
};
ACTION aOpenFileAction=
{
	"open-file",
	NoEvaluator,
	NoIdler,
	NoProgressor,
	NoCurrentor,
	NoGenerator,
	ComputeOpenFile,
	NoCCForm
};
ACTION aMakeLiteralAction=
{
	"make-literal",
	NoEvaluator,
	NoIdler,
	NoProgressor,
	NoCurrentor,
	NoGenerator,
	ComputeMakeLiteral,
	NoCCForm
};
ACTION aSetRNGAction=
{
	"set-rng",
	EvalSetRNG,
	EvalSetRNG,
	ProgressAtomic,
	CurrentAtomic,
	NoGenerator,
	NoComputor,
	MakeCCAtomicForm
};
ACTION aResetRNGAction=
{
	"set-rng",
	EvalResetRNG,
	EvalResetRNG,
	ProgressAtomic,
	CurrentAtomic,
	NoGenerator,
	NoComputor,
	MakeCCAtomicForm
};
ACTION aGetRNGAction=
{
	"get-rng",
	NoEvaluator,
	NoIdler,
	NoProgressor,
	NoCurrentor,
	NoGenerator,
	ComputeGetRNG,
	NoCCForm
};
ACTION aPlanStatusAction=
{
	"plan-status",
	NoEvaluator,
	NoIdler,
	NoProgressor,
	NoCurrentor,
	NoGenerator,
	ComputePlanStatus,
	NoCCForm
};
ACTION aWorldsGeneratedAction=
{
	"worlds-generated",
	NoEvaluator,
	NoIdler,
	NoProgressor,
	NoCurrentor,
	NoGenerator,
	ComputeWorldsGenerated,
	NoCCForm
};
ACTION aWorldsSearchedAction=
{
	"worlds-searched",
	NoEvaluator,
	NoIdler,
	NoProgressor,
	NoCurrentor,
	NoGenerator,
	ComputeWorldsSearched,
	NoCCForm
};
ACTION aWorldsPrunedAction=
{
	"worlds-pruned",
	NoEvaluator,
	NoIdler,
	NoProgressor,
	NoCurrentor,
	NoGenerator,
	ComputeWorldsPruned,
	NoCCForm
};
ACTION aWorldsDiscardedAction=
{
	"worlds-discarded",
	NoEvaluator,
	NoIdler,
	NoProgressor,
	NoCurrentor,
	NoGenerator,
	ComputeWorldsDiscarded,
	NoCCForm
};
ACTION aWorldsUnexaminedAction=
{
	"worlds-unexamined",
	NoEvaluator,
	NoIdler,
	NoProgressor,
	NoCurrentor,
	NoGenerator,
	ComputeWorldsUnexamined,
	NoCCForm
};
ACTION aPlanCPUTimeAction=
{
	"plan-cpu-time",
	NoEvaluator,
	NoIdler,
	NoProgressor,
	NoCurrentor,
	NoGenerator,
	ComputePlanCPUTime,
	NoCCForm
};
ACTION aGetCPUTimeAction=
{
	"get-cpu-time",
	NoEvaluator,
	NoIdler,
	NoProgressor,
	NoCurrentor,
	NoGenerator,
	ComputeGetCPUTime,
	NoCCForm
};
ACTION aSetSearchDepthLimitAction=
{
	"set-search-depth-limit",
	EvalSetSearchDepthLimit,
	EvalSetSearchDepthLimit,
	ProgressAtomic,
	CurrentAtomic,
	NoGenerator,
	NoComputor,
	MakeCCAtomicForm
};
ACTION aResetSearchDepthLimitAction=
{
	"reset-search-depth-limit",
	EvalResetSearchDepthLimit,
	EvalResetSearchDepthLimit,
	ProgressAtomic,
	CurrentAtomic,
	NoGenerator,
	NoComputor,
	MakeCCAtomicForm
};
ACTION aGetSearchDepthLimitAction=
{
	"get-search-depth-limit",
	NoEvaluator,
	NoIdler,
	NoProgressor,
	NoCurrentor,
	NoGenerator,
	ComputeGetSearchDepthLimit,
	NoCCForm
};
ACTION aSearchMaxDepthAction=
{
	"search-max-depth",
	NoEvaluator,
	NoIdler,
	NoProgressor,
	NoCurrentor,
	NoGenerator,
	ComputeSearchMaxDepth,
	NoCCForm
};
ACTION aSetSearchHeuristicLimitAction=
{
	"set-search-heuristic-limit",
	EvalSetSearchHeuristicLimit,
	EvalSetSearchHeuristicLimit,
	ProgressAtomic,
	CurrentAtomic,
	NoGenerator,
	NoComputor,
	MakeCCAtomicForm
};
ACTION aResetSearchHeuristicLimitAction=
{
	"reset-search-heuristic-limit",
	EvalResetSearchHeuristicLimit,
	EvalResetSearchHeuristicLimit,
	ProgressAtomic,
	CurrentAtomic,
	NoGenerator,
	NoComputor,
	MakeCCAtomicForm
};
ACTION aGetSearchHeuristicLimitAction=
{
	"get-search-heuristic-limit",
	NoEvaluator,
	NoIdler,
	NoProgressor,
	NoCurrentor,
	NoGenerator,
	ComputeGetSearchHeuristicLimit,
	NoCCForm
};
ACTION aSearchMaxHeuristicAction=
{
	"search-max-heuristic",
	NoEvaluator,
	NoIdler,
	NoProgressor,
	NoCurrentor,
	NoGenerator,
	ComputeSearchMaxHeuristic,
	NoCCForm
};
ACTION aDelayedActionAction=
{
	"delayed-action",
	EvalDelayedAction,
	EvalDelayedAction,
	ProgressAtomic,
	CurrentAtomic,
	NoGenerator,
	NoComputor,
	MakeCCAtomicForm
};
ACTION aGlobalDelayedActionAction=
{
	"global-delayed-action",
	EvalGlobalDelayedAction,
	EvalGlobalDelayedAction,
	ProgressAtomic,
	CurrentAtomic,
	NoGenerator,
	NoComputor,
	MakeCCAtomicForm
};
ACTION aWaitForNextEventAction=
{
	"wait-for-next-event",
	EvalWaitForNextEvent,
	EvalWaitForNextEvent,
	ProgressAtomic,
	CurrentAtomic,
	NoGenerator,
	NoComputor,
	MakeCCAtomicForm
};
ACTION aReachableEventAction=
{
	"reachable-event",
	EvalReachableEvent,
	EvalReachableEvent,
	ProgressAtomic,
	CurrentAtomic,
	NoGenerator,
	NoComputor,
	MakeCCAtomicForm
};
ACTION aCurrentTimeAction=
{
	"current-time",
	NoEvaluator,
	NoIdler,
	NoProgressor,
	NoCurrentor,
	NoGenerator,
	ComputeCurrentTime,
	NoCCForm
};
ACTION aInhibitDelayedActionAction=
{
	"inhibit-delayed-action",
	EvalInhibitDelayedAction,
	EvalInhibitDelayedAction,
	ProgressAtomic,
	CurrentAtomic,
	NoGenerator,
	NoComputor,
	MakeCCAtomicForm
};

// InitActionName

// Description:
//	Call IdentAlloc to get the hash address of an action name.
//	The hash table entry replaces the name string.
//	We must call InitActionName once for each action with a name

void InitActionName
(
	ACTIONP pa 
)
{
	ENTER("InitActionName",FALSE);
	pa->psName=IdentAlloc(pa->psName);
	EXIT("InitActionName");
}

/* ListToFormula ---------------------------------------------------------------

Description:
	Convert a list into the corresponding formula.
	This routine is primarily used to convert user list input.
Notes:
	Due to macro expansion, this routine may return a sequence of formulas.
Bugs:
	The quantified formulas and other special case formulas don't handle
	recursion for macros correctly yet!
*/

CELLP ListToFormula
(
	LISTP plList
)
{
	LISTP pl;
	CELLP pc;
	LISTP pl1,pl2,pl3,pl4;
	CELLP pc1,pc2,pc3,pc4;
	CELLP pcStart,pcEnd;				/* list head */
	CELLP *ppc;
	FORMULAP pf;
	int nArgs;
	int nVars;

	ENTER("ListToFormula",FALSE);
	if(!plList)							/* handle null formula gracefully */
	{
		EXIT("ListToFormula");
		return NULL;
	}

	/* if we have an enclosed list, this is probably a formula */

	if(plList->nType==ATOM_LISTP)
	{
		pl=plList->uValue.plList;

		/* an empty formula is illegal */

		if(!pl)
		{
			ErrorMessage("ListToFormula: Empty (sub)formula.\n");
			EXIT("ListToFormula");
			return NULL;
		}
		
		/* handle a defined number or string */

		if(plList->psName&&(pl->nType==ATOM_FLOAT||
			pl->nType==ATOM_INTEGER||pl->nType==ATOM_STRING))
		{
			pc1=(CELLP)MemAlloc(sizeof(CELL));
			pc1->pfForm=(FORMULAP)MemAlloc(sizeof(FORMULA));
			pc1->pfForm->nType=pl->nType;
			if(plList->nType==ATOM_STRING)
			{
				pc1->pfForm->psName=StrAlloc(pl->psName);
				pc1->pfForm->uValue.psString=pc1->pfForm->psName;
			}
			else
			{
				pc1->pfForm->psName=IdentAlloc(pl->psName);
				pc1->pfForm->uValue=pl->uValue;
			}
			EXIT("ListToFormula");
			return pc1;
		}

		if(pl->nType!=ATOM_IDENT&&pl->nType!=ATOM_FUNCTIONP&&
			pl->nType!=ATOM_SYMBOLINFOP)
		{
			ErrorMessage("ListToFormula: First element of list must be an identifier\n");
			EXIT("ListToFormula");
			return NULL;
		}

		/* convert formulas with no arguments */

		if(StringEqQ(pl->psName,apsStringTab[STRING_TRUE])||
			StringEqQ(pl->psName,apsStringTab[STRING_FALSE])||
			StringEqQ(pl->psName,apsStringTab[STRING_OPTIMAL_COST])||
			StringEqQ(pl->psName,apsStringTab[STRING_BEST_ACTION])||
			StringEqQ(pl->psName,apsStringTab[STRING_RESET_PRINT_WORLD_FN])||
			StringEqQ(pl->psName,apsStringTab[STRING_PLAN_LENGTH])||
			StringEqQ(pl->psName,apsStringTab[STRING_PLAN_COST])||
			StringEqQ(pl->psName,apsStringTab[STRING_PLAN_DURATION])||
			StringEqQ(pl->psName,apsStringTab[STRING_ACTION_NAME])||
			StringEqQ(pl->psName,apsStringTab[STRING_ACTION_COST])||
			StringEqQ(pl->psName,apsStringTab[STRING_ACTION_DURATION])||
			StringEqQ(pl->psName,apsStringTab[STRING_ACTION_PRIORITY])||
			StringEqQ(pl->psName,apsStringTab[STRING_WORLD_NUMBER])||
			StringEqQ(pl->psName,apsStringTab[STRING_WORLD_HEURISTIC_RANK])||
			StringEqQ(pl->psName,apsStringTab[STRING_PLAN])||
			StringEqQ(pl->psName,apsStringTab[STRING_CLEAR_EVENT_QUEUE])||
			StringEqQ(pl->psName,apsStringTab[STRING_VERBOSE_ON])||
			StringEqQ(pl->psName,apsStringTab[STRING_VERBOSE_OFF])||
			StringEqQ(pl->psName,apsStringTab[STRING_UPDATE_WORLD])||
			StringEqQ(pl->psName,apsStringTab[STRING_GET_TRACE_LEVEL])||
			StringEqQ(pl->psName,apsStringTab[STRING_GET_SEARCH_STRATEGY])||
			StringEqQ(pl->psName,apsStringTab[STRING_GET_SEARCH_LIMIT])||
			StringEqQ(pl->psName,apsStringTab[STRING_GET_TL_CONTROL])||
			StringEqQ(pl->psName,apsStringTab[STRING_RESET_TL_CONTROL])||
			StringEqQ(pl->psName,apsStringTab[STRING_GET_HEURISTIC_FN])||
			StringEqQ(pl->psName,apsStringTab[STRING_HEURISTIC_FN])||
			StringEqQ(pl->psName,apsStringTab[STRING_RESET_HEURISTIC_FN])||
			StringEqQ(pl->psName,apsStringTab[STRING_GET_GOAL_ADDENDUM])||
			StringEqQ(pl->psName,apsStringTab[STRING_RESET_GOAL_ADDENDUM])||
			StringEqQ(pl->psName,apsStringTab[STRING_GET_PRIORITY_FN])||
			StringEqQ(pl->psName,apsStringTab[STRING_RESET_PRIORITY_FN])||
			StringEqQ(pl->psName,apsStringTab[STRING_RESET_SEARCH_LIMIT])||
			StringEqQ(pl->psName,apsStringTab[STRING_RESET_DOMAINS])||
			StringEqQ(pl->psName,apsStringTab[STRING_GET_PLAN_NAME])||
			StringEqQ(pl->psName,apsStringTab[STRING_RESET_PLAN_NAME])||
			StringEqQ(pl->psName,apsStringTab[STRING_RESET_RNG])||
			StringEqQ(pl->psName,apsStringTab[STRING_GET_RNG])||
			StringEqQ(pl->psName,apsStringTab[STRING_LIST_DOMAINS])||
			StringEqQ(pl->psName,apsStringTab[STRING_RAND])||
			StringEqQ(pl->psName,apsStringTab[STRING_CLEAR_WORLD_SYMBOLS])||
			StringEqQ(pl->psName,apsStringTab[STRING_CLEAR_OPERATORS])||
			StringEqQ(pl->psName,apsStringTab[STRING_SELECT_INITIAL_WORLD])||
			StringEqQ(pl->psName,apsStringTab[STRING_SELECT_FINAL_WORLD])||
			StringEqQ(pl->psName,apsStringTab[STRING_SELECT_NEXT_WORLD])||
			StringEqQ(pl->psName,apsStringTab[STRING_SELECT_PREVIOUS_WORLD])||
			StringEqQ(pl->psName,apsStringTab[STRING_PLAN_STATUS])||
			StringEqQ(pl->psName,apsStringTab[STRING_WORLDS_GENERATED])||
			StringEqQ(pl->psName,apsStringTab[STRING_WORLDS_SEARCHED])||
			StringEqQ(pl->psName,apsStringTab[STRING_WORLDS_PRUNED])||
			StringEqQ(pl->psName,apsStringTab[STRING_WORLDS_DISCARDED])||
			StringEqQ(pl->psName,apsStringTab[STRING_WORLDS_UNEXAMINED])||
			StringEqQ(pl->psName,apsStringTab[STRING_PLAN_CPU_TIME])||
			StringEqQ(pl->psName,apsStringTab[STRING_GET_CPU_TIME])||
			StringEqQ(pl->psName,apsStringTab[STRING_GET_SEARCH_DEPTH_LIMIT])||
			StringEqQ(pl->psName,apsStringTab[STRING_SEARCH_MAX_DEPTH])||
			StringEqQ(pl->psName,apsStringTab[STRING_GET_SEARCH_HEURISTIC_LIMIT])||
			StringEqQ(pl->psName,apsStringTab[STRING_SEARCH_MAX_HEURISTIC])||
			StringEqQ(pl->psName,apsStringTab[STRING_WAIT_FOR_NEXT_EVENT])||
			StringEqQ(pl->psName,apsStringTab[STRING_REACHABLE_EVENT])||
			StringEqQ(pl->psName,apsStringTab[STRING_EXIT])||
			StringEqQ(pl->psName,apsStringTab[STRING_SET_GOAL_FORMULA])||
			StringEqQ(pl->psName,apsStringTab[STRING_PRINT_PDDL_PLAN])||
			StringEqQ(pl->psName,apsStringTab[STRING_CURRENT_TIME]))
		{
			pcStart=NULL;				/* initialize listhead */
			pcEnd=(CELLP)&pcStart;
			nArgs=0;
			for(pl1=pl->plNext;pl1;pl1=pl1->plNext)
			{
				for(pc1=ListToFormula(pl1);pc1;pc1=pc1->pcNext)
				{
					pcEnd=pcEnd->pcNext=pc1;
					nArgs++;
				}
			}
			if(nArgs)
			{
				ErrorMessage("ListToFormula:  Too many arguments to %s formula\n",
					pl->psName);
				EXIT("ListToFormula");
				return NULL;
			}
			if(StringEqQ(pl->psName,apsStringTab[STRING_TRUE]))
				pc=CompileArgForm(&aTrueAction,NULL);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_FALSE]))
				pc=CompileArgForm(&aFalseAction,NULL);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_OPTIMAL_COST]))
				pc=CompileArgForm(&aOptimalCostAction,NULL);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_BEST_ACTION]))
				pc=CompileArgForm(&aBestActionAction,NULL);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_PLAN_LENGTH]))
				pc=CompileArgForm(&aPlanLengthAction,NULL);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_RESET_PRINT_WORLD_FN]))
				pc=CompileArgForm(&aResetPrintWorldFnAction,NULL);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_PLAN_COST]))
				pc=CompileArgForm(&aPlanCostAction,NULL);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_PLAN_DURATION]))
				pc=CompileArgForm(&aPlanDurationAction,NULL);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_ACTION_NAME]))
				pc=CompileArgForm(&aActionNameAction,NULL);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_ACTION_COST]))
				pc=CompileArgForm(&aActionCostAction,NULL);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_ACTION_DURATION]))
				pc=CompileArgForm(&aActionDurationAction,NULL);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_ACTION_PRIORITY]))
				pc=CompileArgForm(&aActionPriorityAction,NULL);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_WORLD_NUMBER]))
				pc=CompileArgForm(&aWorldNumberAction,NULL);
//			else if(StringEqQ(pl->psName,apsStringTab[STRING_WORLD_HEURISTIC_RANK]))
//				pc=CompileArgForm(&aWorldHeuristicRankAction,NULL);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_PLAN]))
				pc=CompileArgForm(&aPlanAction,NULL);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_CLEAR_EVENT_QUEUE]))
				pc=CompileArgForm(&aClearEventQueueAction,NULL);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_VERBOSE_OFF]))
				pc=CompileArgForm(&aVerboseOffAction,NULL);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_VERBOSE_ON]))
				pc=CompileArgForm(&aVerboseOnAction,NULL);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_UPDATE_WORLD]))
				pc=CompileArgForm(&aUpdateWorldAction,NULL);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_GET_TRACE_LEVEL]))
				pc=CompileArgForm(&aGetTraceLevelAction,NULL);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_GET_SEARCH_STRATEGY]))
				pc=CompileArgForm(&aGetSearchStrategyAction,NULL);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_GET_SEARCH_LIMIT]))
				pc=CompileArgForm(&aGetSearchLimitAction,NULL);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_GET_TL_CONTROL]))
				pc=CompileArgForm(&aGetTLControlAction,NULL);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_GET_GOAL_ADDENDUM]))
				pc=CompileArgForm(&aGetGoalAddendumAction,NULL);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_RESET_TL_CONTROL]))
				pc=CompileArgForm(&aResetTLControlAction,NULL);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_GET_HEURISTIC_FN]))
				pc=CompileArgForm(&aGetHeuristicFnAction,NULL);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_HEURISTIC_FN]))
				pc=CompileArgForm(&aHeuristicFnAction,NULL);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_RESET_HEURISTIC_FN]))
				pc=CompileArgForm(&aResetHeuristicFnAction,NULL);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_RESET_GOAL_ADDENDUM]))
				pc=CompileArgForm(&aResetGoalAddendumAction,NULL);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_GET_PRIORITY_FN]))
				pc=CompileArgForm(&aGetPriorityFnAction,NULL);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_RESET_PRIORITY_FN]))
				pc=CompileArgForm(&aResetPriorityFnAction,NULL);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_RESET_SEARCH_LIMIT]))
				pc=CompileArgForm(&aResetSearchLimitAction,NULL);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_RESET_DOMAINS]))
				pc=CompileArgForm(&aResetDomainsAction,NULL);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_GET_PLAN_NAME]))
				pc=CompileArgForm(&aGetPlanNameAction,NULL);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_RESET_PLAN_NAME]))
				pc=CompileArgForm(&aResetPlanNameAction,NULL);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_RESET_RNG]))
				pc=CompileArgForm(&aResetRNGAction,NULL);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_GET_RNG]))
				pc=CompileArgForm(&aGetRNGAction,NULL);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_LIST_DOMAINS]))
				pc=CompileArgForm(&aListDomainsAction,NULL);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_RAND]))
				pc=CompileArgForm(&aRandAction,NULL);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_CLEAR_WORLD_SYMBOLS]))
				pc=CompileArgForm(&aClearWorldSymbolsAction,NULL);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_CLEAR_OPERATORS]))
				pc=CompileArgForm(&aClearOperatorsAction,NULL);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_SELECT_INITIAL_WORLD]))
				pc=CompileArgForm(&aSelectInitialWorldAction,NULL);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_SELECT_FINAL_WORLD]))
				pc=CompileArgForm(&aSelectFinalWorldAction,NULL);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_SELECT_NEXT_WORLD]))
				pc=CompileArgForm(&aSelectNextWorldAction,NULL);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_SELECT_PREVIOUS_WORLD]))
				pc=CompileArgForm(&aSelectPreviousWorldAction,NULL);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_PLAN_STATUS]))
				pc=CompileArgForm(&aPlanStatusAction,NULL);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_WORLDS_GENERATED]))
				pc=CompileArgForm(&aWorldsGeneratedAction,NULL);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_WORLDS_SEARCHED]))
				pc=CompileArgForm(&aWorldsSearchedAction,NULL);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_WORLDS_PRUNED]))
				pc=CompileArgForm(&aWorldsPrunedAction,NULL);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_WORLDS_DISCARDED]))
				pc=CompileArgForm(&aWorldsDiscardedAction,NULL);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_WORLDS_UNEXAMINED]))
				pc=CompileArgForm(&aWorldsUnexaminedAction,NULL);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_PLAN_CPU_TIME]))
				pc=CompileArgForm(&aPlanCPUTimeAction,NULL);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_GET_CPU_TIME]))
				pc=CompileArgForm(&aGetCPUTimeAction,NULL);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_GET_SEARCH_DEPTH_LIMIT]))
				pc=CompileArgForm(&aGetSearchDepthLimitAction,NULL);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_RESET_SEARCH_DEPTH_LIMIT]))
				pc=CompileArgForm(&aResetSearchDepthLimitAction,NULL);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_SEARCH_MAX_DEPTH]))
				pc=CompileArgForm(&aSearchMaxDepthAction,NULL);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_GET_SEARCH_HEURISTIC_LIMIT]))
				pc=CompileArgForm(&aGetSearchHeuristicLimitAction,NULL);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_RESET_SEARCH_HEURISTIC_LIMIT]))
				pc=CompileArgForm(&aResetSearchHeuristicLimitAction,NULL);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_SEARCH_MAX_HEURISTIC]))
				pc=CompileArgForm(&aSearchMaxHeuristicAction,NULL);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_WAIT_FOR_NEXT_EVENT]))
				pc=CompileArgForm(&aWaitForNextEventAction,NULL);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_REACHABLE_EVENT]))
				pc=CompileArgForm(&aReachableEventAction,NULL);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_EXIT]))
				pc=CompileArgForm(&aExitAction,NULL);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_SET_GOAL_FORMULA]))
				pc=CompileArgForm(&aSetGoalFormulaAction,NULL);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_CURRENT_TIME]))
				pc=CompileArgForm(&aCurrentTimeAction,NULL);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_PRINT_PDDL_PLAN]))
				pc=CompileArgForm(&aPrintPddlPlanAction,NULL);
			else
			{
				ErrorMessage("Internal error\n");
				EXIT("ListToFormula");
				return NULL;
			}
			EXIT("ListToFormula");
			return pc;
		}

		/* convert formulas with one argument */

		if(StringEqQ(pl->psName,apsStringTab[STRING_NOT])||
			StringEqQ(pl->psName,apsStringTab[STRING_GOAL])||
			StringEqQ(pl->psName,apsStringTab[STRING_CURRENT])||
			StringEqQ(pl->psName,apsStringTab[STRING_PREVIOUS])||
			StringEqQ(pl->psName,apsStringTab[STRING_PERMUTE])||
			StringEqQ(pl->psName,apsStringTab[STRING_DELTA])||
			StringEqQ(pl->psName,apsStringTab[STRING_ALWAYS])||
			StringEqQ(pl->psName,apsStringTab[STRING_EVENTUALLY])||
			StringEqQ(pl->psName,apsStringTab[STRING_NEXT])||
			StringEqQ(pl->psName,apsStringTab[STRING_SET_SEARCH_LIMIT])||
			StringEqQ(pl->psName,apsStringTab[STRING_LOAD_DOMAIN])||
			StringEqQ(pl->psName,apsStringTab[STRING_LOAD_FILE])||
			StringEqQ(pl->psName,apsStringTab[STRING_LOAD_PDDL_PROBLEM])||
			StringEqQ(pl->psName,apsStringTab[STRING_SET_GOAL_TYPE])||
			StringEqQ(pl->psName,apsStringTab[STRING_SET_CONTROL])||
			StringEqQ(pl->psName,apsStringTab[STRING_ENABLE])||
			StringEqQ(pl->psName,apsStringTab[STRING_DISABLE])||
			StringEqQ(pl->psName,apsStringTab[STRING_SET_GOAL_ADDENDUM])||
			StringEqQ(pl->psName,apsStringTab[STRING_SET_RNG])||
			StringEqQ(pl->psName,apsStringTab[STRING_SET_STATISTICS_FILE])||
			StringEqQ(pl->psName,apsStringTab[STRING_SET_HEURISTIC_FN])||
			StringEqQ(pl->psName,apsStringTab[STRING_SET_PRIORITY_FN])||
			StringEqQ(pl->psName,apsStringTab[STRING_SET_TRACE_LEVEL])||
			StringEqQ(pl->psName,apsStringTab[STRING_SET_TL_CONTROL])||
			StringEqQ(pl->psName,apsStringTab[STRING_SET_SEARCH_STRATEGY])||
			StringEqQ(pl->psName,apsStringTab[STRING_SET_PRINT_WORLD_FN])||
			StringEqQ(pl->psName,apsStringTab[STRING_PRINT_PLAN_LIST])||
			StringEqQ(pl->psName,apsStringTab[STRING_EXP])||
			StringEqQ(pl->psName,apsStringTab[STRING_LOG])||
			StringEqQ(pl->psName,apsStringTab[STRING_SEED])||
			StringEqQ(pl->psName,apsStringTab[STRING_ROUND])||
			StringEqQ(pl->psName,apsStringTab[STRING_INT])||
			StringEqQ(pl->psName,apsStringTab[STRING_FLOOR])||
			StringEqQ(pl->psName,apsStringTab[STRING_CEIL])||
			StringEqQ(pl->psName,apsStringTab[STRING_POS_INT])||
			StringEqQ(pl->psName,apsStringTab[STRING_CLOSE_FILE])||
			StringEqQ(pl->psName,apsStringTab[STRING_SET_SEARCH_DEPTH_LIMIT])||
			StringEqQ(pl->psName,apsStringTab[STRING_SET_SEARCH_HEURISTIC_LIMIT])||
			StringEqQ(pl->psName,apsStringTab[STRING_PRINT_WORLD])||
			StringEqQ(pl->psName,apsStringTab[STRING_PRINT_WORLD_LIST])||
			StringEqQ(pl->psName,apsStringTab[STRING_ASTAR]))
		{
			pcStart=NULL;				/* initialize listhead */
			pcEnd=(CELLP)&pcStart;
			nArgs=0;
			for(pl1=pl->plNext;pl1;pl1=pl1->plNext)
			{
				for(pc1=ListToFormula(pl1);pc1;pc1=pc1->pcNext)
				{
					pcEnd=pcEnd->pcNext=pc1;
					nArgs++;
				}
			}
			if(nArgs!=1)
			{
				ErrorMessage("ListToFormula:  Too %s arguments to %s formula\n",
					nArgs<1?"few":"many",pl->psName);
				EXIT("ListToFormula");
				return NULL;
			}
			if(StringEqQ(pl->psName,apsStringTab[STRING_NOT]))
				pc=CompileArgForm(&aNotAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_GOAL]))
				pc=CompileArgForm(&aGoalAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_CURRENT]))
				pc=CompileArgForm(&aCurrentAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_PREVIOUS]))
				pc=CompileArgForm(&aPreviousAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_PERMUTE]))
				pc=CompileArgForm(&aPermuteAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_DELTA]))
				pc=CompileArgForm(&aDeltaAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_ALWAYS]))
				pc=CompileArgForm(&aAlwaysAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_EVENTUALLY]))
				pc=CompileArgForm(&aEventuallyAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_NEXT]))
				pc=CompileArgForm(&aNextAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_SET_GOAL_TYPE]))
				pc=CompileArgForm(&aSetGoalTypeAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_SET_CONTROL]))
				pc=CompileArgForm(&aSetControlAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_ENABLE]))
				pc=CompileArgForm(&aEnableAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_DISABLE]))
				pc=CompileArgForm(&aDisableAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_SET_GOAL_ADDENDUM]))
				pc=CompileArgForm(&aSetGoalAddendumAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_SET_RNG]))
				pc=CompileArgForm(&aSetRNGAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_SET_SEARCH_LIMIT]))
				pc=CompileArgForm(&aSetSearchLimitAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_LOAD_DOMAIN]))
				pc=CompileArgForm(&aLoadDomainAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_LOAD_FILE]))
				pc=CompileArgForm(&aLoadFileAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_LOAD_PDDL_PROBLEM]))
				pc=CompileArgForm(&aLoadPddlProblemAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_SET_STATISTICS_FILE]))
				pc=CompileArgForm(&aSetStatisticsFileAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_SET_HEURISTIC_FN]))
				pc=CompileArgForm(&aSetHeuristicFnAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_SET_PRIORITY_FN]))
				pc=CompileArgForm(&aSetPriorityFnAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_SET_TRACE_LEVEL]))
				pc=CompileArgForm(&aSetTraceLevelAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_SET_TL_CONTROL]))
				pc=CompileArgForm(&aSetTLControlAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_SET_SEARCH_STRATEGY]))
				pc=CompileArgForm(&aSetSearchStrategyAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_SET_PRINT_WORLD_FN]))
				pc=CompileArgForm(&aSetPrintWorldFnAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_PRINT_PLAN_LIST]))
				pc=CompileArgForm(&aPrintPlanListAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_EXP]))
				pc=CompileArgForm(&aExpAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_LOG]))
				pc=CompileArgForm(&aLogAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_SEED]))
				pc=CompileArgForm(&aSeedAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_ROUND]))
				pc=CompileArgForm(&aRoundAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_INT]))
				pc=CompileArgForm(&aIntAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_FLOOR]))
				pc=CompileArgForm(&aFloorAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_CEIL]))
				pc=CompileArgForm(&aCeilAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_POS_INT]))
				pc=CompileArgForm(&aPosIntAction,NULL);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_CLOSE_FILE]))
				pc=CompileArgForm(&aCloseFileAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_SET_SEARCH_DEPTH_LIMIT]))
				pc=CompileArgForm(&aSetSearchDepthLimitAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_SET_SEARCH_HEURISTIC_LIMIT]))
				pc=CompileArgForm(&aSetSearchHeuristicLimitAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_PRINT_WORLD]))
				pc=CompileArgForm(&aPrintWorldAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_PRINT_WORLD_LIST]))
				pc=CompileArgForm(&aPrintWorldListAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_ASTAR]))
				pc=CompileArgForm(&aAStarAction,pcStart);
			else
			{
				ErrorMessage("Internal error\n");
				EXIT("ListToFormula");
				return NULL;
			}
			EXIT("ListToFormula");
			return pc;
		}

		/* convert formulas with two arguments */

		if(StringEqQ(pl->psName,apsStringTab[STRING_IMPLIES])||
			StringEqQ(pl->psName,apsStringTab[STRING_EQ])||
			StringEqQ(pl->psName,apsStringTab[STRING_ASSIGN])||
			StringEqQ(pl->psName,apsStringTab[STRING_OPEN_FILE])||
			StringEqQ(pl->psName,apsStringTab[STRING_LT])||
			StringEqQ(pl->psName,apsStringTab[STRING_LE])||
			StringEqQ(pl->psName,apsStringTab[STRING_GT])||
			StringEqQ(pl->psName,apsStringTab[STRING_GE])||
			StringEqQ(pl->psName,apsStringTab[STRING_T_ALWAYS])||
			StringEqQ(pl->psName,apsStringTab[STRING_T_EVENTUALLY])||
			StringEqQ(pl->psName,apsStringTab[STRING_RANDOM])||
			StringEqQ(pl->psName,apsStringTab[STRING_REDIRECT])||
			StringEqQ(pl->psName,apsStringTab[STRING_LT_POS_INT])||
			StringEqQ(pl->psName,apsStringTab[STRING_UNTIL])||
			StringEqQ(pl->psName,apsStringTab[STRING_INHIBIT_DELAYED_ACTION])||
			StringEqQ(pl->psName,apsStringTab[STRING_PLUS_EQ])||
			StringEqQ(pl->psName,apsStringTab[STRING_MINUS_EQ]))
		{
			pcStart=NULL;				/* initialize listhead */
			pcEnd=(CELLP)&pcStart;
			nArgs=0;
			for(pl1=pl->plNext;pl1;pl1=pl1->plNext)
			{
				for(pc1=ListToFormula(pl1);pc1;pc1=pc1->pcNext)
				{
					pcEnd=pcEnd->pcNext=pc1;
					nArgs++;
				}
			}
			if(nArgs!=2)
			{
				ErrorMessage("ListToFormula:  Too %s arguments to %s formula\n",
					nArgs<2?"few":"many",pl->psName);
				EXIT("ListToFormula");
				return NULL;
			}
			if(StringEqQ(pl->psName,apsStringTab[STRING_IMPLIES]))
				pc=CompileArgForm(&aImpliesAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_EQ]))
				pc=CompileArgForm(&aEqAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_LT]))
				pc=CompileArgForm(&aLtAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_LE]))
				pc=CompileArgForm(&aLeAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_GT]))
				pc=CompileArgForm(&aGtAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_GE]))
				pc=CompileArgForm(&aGeAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_ASSIGN]))
				pc=CompileArgForm(&aAssignAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_OPEN_FILE]))
				pc=CompileArgForm(&aOpenFileAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_T_ALWAYS]))
				pc=CompileArgForm(&aTAlwaysAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_T_EVENTUALLY]))
				pc=CompileArgForm(&aTEventuallyAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_RANDOM]))
				pc=CompileArgForm(&aRandomAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_REDIRECT]))
				pc=CompileArgForm(&aRedirectAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_LT_POS_INT]))
				pc=CompileArgForm(&aLtPosIntAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_UNTIL]))
				pc=CompileArgForm(&aUntilAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_INHIBIT_DELAYED_ACTION]))
				pc=CompileArgForm(&aInhibitDelayedActionAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_PLUS_EQ]))
				pc=CompileArgForm(&aPlusEqAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_MINUS_EQ]))
				pc=CompileArgForm(&aMinusEqAction,pcStart);
			else
			{
				ErrorMessage("Internal error\n");
				EXIT("ListToFormula");
				return NULL;
			}
			EXIT("ListToFormula");
			return pc;
		}
		
		/* convert formulas with three arguments */

		if(StringEqQ(pl->psName,apsStringTab[STRING_IF_THEN_ELSE])||
			StringEqQ(pl->psName,apsStringTab[STRING_DELAYED_ACTION])||
			StringEqQ(pl->psName,apsStringTab[STRING_GLOBAL_DELAYED_ACTION])||
			StringEqQ(pl->psName,apsStringTab[STRING_T_UNTIL]))
		{
			pcStart=NULL;				/* initialize listhead */
			pcEnd=(CELLP)&pcStart;
			nArgs=0;
			for(pl1=pl->plNext;pl1;pl1=pl1->plNext)
			{
				for(pc1=ListToFormula(pl1);pc1;pc1=pc1->pcNext)
				{
					pcEnd=pcEnd->pcNext=pc1;
					nArgs++;
				}
			}
			if(nArgs!=3)
			{
				ErrorMessage("ListToFormula:  Too %s arguments to %s formula\n",
					nArgs<3?"few":"many",pl->psName);
				EXIT("ListToFormula");
				return NULL;
			}
			if(StringEqQ(pl->psName,apsStringTab[STRING_IF_THEN_ELSE]))
				pc=CompileArgForm(&aIfThenElseAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_DELAYED_ACTION]))
				pc=CompileArgForm(&aDelayedActionAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_GLOBAL_DELAYED_ACTION]))
				pc=CompileArgForm(&aGlobalDelayedActionAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_T_UNTIL]))
				pc=CompileArgForm(&aTUntilAction,pcStart);
			else
			{
				ErrorMessage("Internal error\n");
				EXIT("ListToFormula");
				return NULL;
			}
			EXIT("ListToFormula");
			return pc;
		}

		/* convert formulas with zero or more arguments */

		if(StringEqQ(pl->psName,apsStringTab[STRING_SET_GOAL])||
			StringEqQ(pl->psName,apsStringTab[STRING_SET_INITIAL_WORLD])||
			StringEqQ(pl->psName,apsStringTab[STRING_PRINT_DELTA_TIME]))
		{
			pcStart=NULL;				/* initialize listhead */
			pcEnd=(CELLP)&pcStart;
			nArgs=0;
			for(pl1=pl->plNext;pl1;pl1=pl1->plNext)
			{
				for(pc1=ListToFormula(pl1);pc1;pc1=pc1->pcNext)
				{
					pcEnd=pcEnd->pcNext=pc1;
					nArgs++;
				}
			}
			if(StringEqQ(pl->psName,apsStringTab[STRING_SET_GOAL]))
				pc=CompileArgForm(&aSetGoalAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_SET_INITIAL_WORLD]))
				pc=CompileArgForm(&aSetInitialWorldAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_PRINT_DELTA_TIME]))
				pc=CompileArgForm(&aPrintDeltaTimeAction,pcStart);
			else
			{
				pc=NULL;				// keep compiler happy
				ErrorMessage("Internal error\n");
			}
			EXIT("ListToFormula");
			return pc;
		}

		/* convert formulas with one or more arguments */

		if(StringEqQ(pl->psName,apsStringTab[STRING_AND])||
			StringEqQ(pl->psName,apsStringTab[STRING_OR])||
			StringEqQ(pl->psName,apsStringTab[STRING_XOR])||
			StringEqQ(pl->psName,apsStringTab[STRING_ADD])||
			StringEqQ(pl->psName,apsStringTab[STRING_DEL])||
			StringEqQ(pl->psName,apsStringTab[STRING_PRINT])||
			StringEqQ(pl->psName,apsStringTab[STRING_DEF_DOMAIN])||
			StringEqQ(pl->psName,apsStringTab[STRING_DECLARE_DESCRIBED_SYMBOLS])||
			StringEqQ(pl->psName,apsStringTab[STRING_DECLARE_DEFINED_SYMBOLS])||
			StringEqQ(pl->psName,apsStringTab[STRING_DECLARE_EXTERNAL_SYMBOLS])||
			StringEqQ(pl->psName,apsStringTab[STRING_DECLARE_GLOBAL_VARIABLES])||
			StringEqQ(pl->psName,apsStringTab[STRING_DECLARE_MACRO_OPERATORS])||
			StringEqQ(pl->psName,apsStringTab[STRING_DECLARE_ELIDED_OPERATORS])||
			StringEqQ(pl->psName,apsStringTab[STRING_DEFINE])||
			StringEqQ(pl->psName,apsStringTab[STRING_ASSIGN_APPEND])||
			StringEqQ(pl->psName,apsStringTab[STRING_DEF_DEFINED_PREDICATE])||
			StringEqQ(pl->psName,apsStringTab[STRING_DEF_DEFINED_FUNCTION])||
			StringEqQ(pl->psName,apsStringTab[STRING_DEF_DEFINED_GENERATOR])||
			StringEqQ(pl->psName,apsStringTab[STRING_DEF_DEFINED_MACRO])||
			StringEqQ(pl->psName,apsStringTab[STRING_DEF_ADL_OPERATOR])||
			StringEqQ(pl->psName,apsStringTab[STRING_DEF_STRIPS_OPERATOR])||
			StringEqQ(pl->psName,apsStringTab[STRING_SET_INITIAL_FACTS])||
			StringEqQ(pl->psName,apsStringTab[STRING_SET_INITIALIZATION_SEQUENCE])||
			StringEqQ(pl->psName,apsStringTab[STRING_SET_GOAL_SEQUENCE])||
			StringEqQ(pl->psName,apsStringTab[STRING_IS_BETWEEN])||
			StringEqQ(pl->psName,apsStringTab[STRING_NEAREST_FIRST])||
			StringEqQ(pl->psName,apsStringTab[STRING_NEAREST_FIRST_EX])||
			StringEqQ(pl->psName,apsStringTab[STRING_CLOSEST_FIRST])||
			StringEqQ(pl->psName,apsStringTab[STRING_CLOSEST_FIRST_EX])||
			StringEqQ(pl->psName,apsStringTab[STRING_LOWEST_FIRST])||
			StringEqQ(pl->psName,apsStringTab[STRING_ALL_PAIRS_SHORTEST_PATH])||
			StringEqQ(pl->psName,apsStringTab[STRING_PLUS])||
			StringEqQ(pl->psName,apsStringTab[STRING_MINUS])||
			StringEqQ(pl->psName,apsStringTab[STRING_STAR])||
			StringEqQ(pl->psName,apsStringTab[STRING_SLASH])||
			StringEqQ(pl->psName,apsStringTab[STRING_MOD])||
			StringEqQ(pl->psName,apsStringTab[STRING_MAX])||
			StringEqQ(pl->psName,apsStringTab[STRING_MIN])||
			StringEqQ(pl->psName,apsStringTab[STRING_EXPT])||
			StringEqQ(pl->psName,apsStringTab[STRING_SQRT])||
			StringEqQ(pl->psName,apsStringTab[STRING_ABS])||
			StringEqQ(pl->psName,apsStringTab[STRING_SET_PLAN_NAME])||
			StringEqQ(pl->psName,apsStringTab[STRING_LOAD_PLAN])||
			StringEqQ(pl->psName,apsStringTab[STRING_MAKE_LITERAL]))
		{
			bADLPre=StringEqQ(pl->psName,apsStringTab[STRING_DEF_ADL_OPERATOR]);
			
			pcStart=NULL;				/* initialize listhead */
			pcEnd=(CELLP)&pcStart;
			nArgs=0;
			for(pl1=pl->plNext;pl1;pl1=pl1->plNext)
			{
				for(pc1=ListToFormula(pl1);pc1;pc1=pc1->pcNext)
				{
					pcEnd=pcEnd->pcNext=pc1;
					nArgs++;
				}
			}
			if(!nArgs)
			{
				ErrorMessage("ListToFormula:  Too few arguments to %s formula\n",
					pl->psName);
				EXIT("ListToFormula");
				return NULL;
			}
			if(StringEqQ(pl->psName,apsStringTab[STRING_AND]))
				pc=CompileArgForm(&aAndAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_OR]))
				pc=CompileArgForm(&aOrAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_XOR]))
				pc=CompileArgForm(&aXorAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_ADD]))
				pc=CompileArgForm(&aAddAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_DEL]))
				pc=CompileArgForm(&aDelAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_PRINT]))
				pc=CompileArgForm(&aPrintAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_DEF_DOMAIN]))
				pc=CompileArgForm(&aDefDomainAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_DECLARE_DESCRIBED_SYMBOLS]))
				pc=CompileArgForm(&aDeclareDescribedSymbolsAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_DECLARE_DEFINED_SYMBOLS]))
				pc=CompileArgForm(&aDeclareDefinedSymbolsAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_DECLARE_EXTERNAL_SYMBOLS]))
				pc=CompileArgForm(&aDeclareExternalSymbolsAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_DECLARE_GLOBAL_VARIABLES]))
				pc=CompileArgForm(&aDeclareGlobalVariablesAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_DECLARE_MACRO_OPERATORS]))
				pc=CompileArgForm(&aDeclareMacroOperatorsAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_DECLARE_ELIDED_OPERATORS]))
				pc=CompileArgForm(&aDeclareElidedOperatorsAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_DEFINE]))
				pc=CompileArgForm(&aDefineAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_DEF_DEFINED_PREDICATE]))
				pc=CompileArgForm(&aDefDefinedPredicateAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_DEF_DEFINED_FUNCTION]))
				pc=CompileArgForm(&aDefDefinedFunctionAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_DEF_DEFINED_GENERATOR]))
				pc=CompileArgForm(&aDefDefinedGeneratorAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_DEF_DEFINED_MACRO]))
				pc=CompileArgForm(&aDefDefinedMacroAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_ASSIGN_APPEND]))
				pc=CompileArgForm(&aAssignAppendAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_DEF_ADL_OPERATOR]))
			{
				pc=CompileArgForm(&aDefADLOperatorAction,pcStart);
				bADLPre=FALSE;
			}
			else if(StringEqQ(pl->psName,apsStringTab[STRING_DEF_STRIPS_OPERATOR]))
				pc=CompileArgForm(&aDefStripsOperatorAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_SET_GOAL]))
				pc=CompileArgForm(&aSetGoalAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_SET_INITIAL_FACTS]))
				pc=CompileArgForm(&aSetInitialFactsAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_SET_INITIAL_WORLD]))
				pc=CompileArgForm(&aSetInitialWorldAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_SET_INITIALIZATION_SEQUENCE]))
				pc=CompileArgForm(&aSetInitializationSequenceAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_SET_GOAL_SEQUENCE]))
				pc=CompileArgForm(&aSetGoalSequenceAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_SET_PLAN_NAME]))
				pc=CompileArgForm(&aSetPlanNameAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_IS_BETWEEN]))
				pc=CompileArgForm(&aIsBetweenAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_NEAREST_FIRST]))
				pc=CompileArgForm(&aNearestFirstAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_NEAREST_FIRST_EX]))
				pc=CompileArgForm(&aNearestFirstExAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_CLOSEST_FIRST]))
				pc=CompileArgForm(&aClosestFirstAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_CLOSEST_FIRST_EX]))
				pc=CompileArgForm(&aClosestFirstExAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_LOWEST_FIRST]))
				pc=CompileArgForm(&aLowestFirstAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_ALL_PAIRS_SHORTEST_PATH]))
				pc=CompileArgForm(&aAllPairsShortestPathAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_PLUS]))
				pc=CompileArgForm(&aPlusAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_MINUS]))
				pc=CompileArgForm(&aMinusAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_STAR]))
				pc=CompileArgForm(&aMultiplyAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_SLASH]))
				pc=CompileArgForm(&aDivideAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_MOD]))
				pc=CompileArgForm(&aModAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_MAX]))
				pc=CompileArgForm(&aMaxAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_MIN]))
				pc=CompileArgForm(&aMinAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_EXPT]))
				pc=CompileArgForm(&aExptAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_SQRT]))
				pc=CompileArgForm(&aSqrtAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_ABS]))
				pc=CompileArgForm(&aAbsAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_LOAD_PLAN]))
				pc=CompileArgForm(&aLoadPlanAction,pcStart);
			else if(StringEqQ(pl->psName,apsStringTab[STRING_MAKE_LITERAL]))
				pc=CompileArgForm(&aMakeLiteralAction,pcStart);
			else
			{
				pc=NULL;				// keep compiler happy
				ErrorMessage("Internal error\n");
			}
			EXIT("ListToFormula");
			return pc;
		}
		if(StringEqQ(pl->psName,apsStringTab[STRING_IN_THE_SET]))
		{
			pc=NULL;
			pl1=pl->plNext;
			if(pl1->plNext)			/* if variables present */
			{
				if(!ListToVars(pl1,&pc1,&nVars))
				{
					EXIT("ListToFormula");
					return NULL;
				}
			}
			else
			{
				ErrorMessage("ListToFormula:  No variables or values in %s formula\n",
					pl->psName);
				EXIT("ListToFormula");
				return NULL;
			}

			pcStart=NULL;				/* initialize listhead */
			pcEnd=(CELLP)&pcStart;
			nArgs=0;
			for(pl1=pl1->plNext;pl1;pl1=pl1->plNext)
			{
				pcEnd=pcEnd->pcNext=ListToFormula(pl1);
				nArgs++;
			}
			if(!nArgs)
			{
				ErrorMessage("ListToFormula:  No values in %s formula\n",
					pl->psName);
				EXIT("ListToFormula");
				return NULL;
			}
			if(nArgs!=nArgs/nVars*nVars)
			{
				ErrorMessage("ListToFormula:  Value count not a multiple of "
					"variable count in %s formula\n",pl->psName);
				EXIT("ListToFormula");
				return NULL;
			}
			
			pc=CompileQForm(&aInTheSetAction,pc1,NULL,NULL);
			pc->pfForm->pcArgs=pcStart;
			ArgListCheck(pc,ARG_BOOL);
			EXIT("ListToFormula");
			return pc;
		}

		/* convert quantified formulas */

		if(StringEqQ(pl->psName,apsStringTab[STRING_EXISTS])||
			StringEqQ(pl->psName,apsStringTab[STRING_EXISTSX])||
			StringEqQ(pl->psName,apsStringTab[STRING_FORALL]))
		{
			pc=NULL;
			ppc=&pc;
			for(pl1=pl->plNext;pl1;pl1=pl1->plNext->plNext)
			{
				if(pl1->plNext)			/* if vargen present */
				{
					if(!ListToVarGen(pl1,&pc1,&pc2))
					{
						EXIT("ListToFormula");
						return NULL;
					}
					if(StringEqQ(pl->psName,apsStringTab[STRING_EXISTS]))
					{
						*ppc=CompileQForm(&aExistsAction,pc1,pc2,NULL);
						ArgListCheck(*ppc,ARG_BOOL);
					}
					else if(StringEqQ(pl->psName,apsStringTab[STRING_EXISTSX]))
					{
						*ppc=CompileQForm(&aExistsXAction,pc1,pc2,NULL);
						ArgListCheck(*ppc,ARG_BOOL);
					}
					else if(StringEqQ(pl->psName,apsStringTab[STRING_FORALL]))
					{
						*ppc=CompileQForm(&aForAllAction,pc1,pc2,NULL);
						ArgListCheck(*ppc,ARG_BOOL);
					}
					else
						ErrorMessage("Internal error\n");
				}
				else
				{
					pc3=ListToFormula(pl1);	/* formula */
					if(!pc)				/* if no vargen present */
					{
						ErrorMessage("ListToFormula:  No variables or generator in %s formula\n",
							pl->psName);
						EXIT("ListToFormula");
						return NULL;
					}
					*ppc=pc3;
					break;
				}
				ppc=&(*ppc)->pfForm->pcArgs;
			}
			EXIT("ListToFormula");
			return pc;
		}

		/* convert adl pre formula */

		if(StringEqQ(pl->psName,apsStringTab[STRING_PRE]))
		{
			if(bADLPre)				/* must be ADL preconditions */
			{
				pcStart=NULL;
				pcEnd=(CELLP)&pcStart;
				pc3=NULL;
				for(pl1=pl->plNext;pl1;pl1=pl1->plNext->plNext)
				{
					if(pl1->plNext)		/* if vargen present */
					{
						if(!ListToVarGen(pl1,&pc1,&pc2))
						{
							EXIT("ListToFormula");
							return NULL;
						}
						pcEnd=pcEnd->pcNext=CompileVarGenForm(pc1,pc2);
						ArgListCheck(pcEnd,ARG_UNKNOWN);
					}
					else
					{
						pc3=ListToFormula(pl1);	/* formula */
						break;
					}
				}
				pc=CompilePreForm(pcStart,pc3);
				ArgListCheck(pc,ARG_UNKNOWN);
				EXIT("ListToFormula");
				return pc;
			}
			else						/* must be strips preconditions */
			{
				pcStart=NULL;			/* initialize listhead */
				pcEnd=(CELLP)&pcStart;
				for(pl1=pl->plNext;pl1;pl1=pl1->plNext)
				{
					for(pc1=ListToFormula(pl1);pc1;pc1=pc1->pcNext)
						pcEnd=pcEnd->pcNext=pc1;
				}
				pc=CompileDummyForm(pl->psName,pcStart);
			}
		}

		/* convert interval formulas */

		if(StringEqQ(pl->psName,apsStringTab[STRING_ISPEC]))
		{
			pl1=pl->plNext;
			if(!pl1)
			{
				ErrorMessage("ListToFormula:  Too few arguments to %s formula\n",
					pl->psName);
				EXIT("ListToFormula");
				return NULL;
			}
			pl2=pl1->plNext;
			if(!pl2)
			{
				ErrorMessage("ListToFormula:  Too few arguments to %s formula\n",
					pl->psName);
				EXIT("ListToFormula");
				return NULL;
			}
			pl3=pl2->plNext;
			if(!pl3)
			{
				ErrorMessage("ListToFormula:  Too few arguments to %s formula\n",
					pl->psName);
				EXIT("ListToFormula");
				return NULL;
			}
			pl4=pl3->plNext;
			if(!pl4)
			{
				ErrorMessage("ListToFormula:  Too few arguments to %s formula\n",
					pl->psName);
				EXIT("ListToFormula");
				return NULL;
			}
			if(pl4->plNext)
			{
				ErrorMessage("ListToFormula:  Too many arguments to %s formula\n",
					pl->psName);
				EXIT("ListToFormula");
				return NULL;
			}
			pc1=ListToFormula(pl1);
			pc2=ListToFormula(pl2);
			pc3=ListToFormula(pl3);
			pc4=ListToFormula(pl4);
			if(pc1&&pc2&&pc3&&pc4)
			{
				pc=CompileISpecForm(pc1,pc2,pc3,pc4);
				ArgListCheck(pc,ARG_UNKNOWN);
			}
			else
				pc=NULL;
			EXIT("ListToFormula");
			return pc;
		}

		/* handle array */

		if(*pl->psName=='?')
		{
			if(!pl->plNext)
			{
				ErrorMessage("ListToFormula:  Array %s requires indices/dimensions\n",
					pl->psName);
				EXIT("ListToFormula");
				return NULL;
			}
			pcStart=NULL;				/* initialize listhead */
			pcEnd=(CELLP)&pcStart;
			for(pl1=pl->plNext;pl1;pl1=pl1->plNext)
			{
				pc1=ListToFormula(pl1);
				if(pc1)					/* discard errors */
					pcEnd=pcEnd->pcNext=pc1;
			}
			pc=CompileArrayForm(pl->psName,pcStart);
			ArgListCheck(pc,ARG_TERM);
			EXIT("ListToFormula");
			return pc;
		}
		
		/* convert symbol-info routines */

		if(pl->nType==ATOM_SYMBOLINFOP)
		{
			pcStart=NULL;				/* initialize listhead */
			pcEnd=(CELLP)&pcStart;
			for(pl1=pl->plNext;pl1;pl1=pl1->plNext)
			{
				for(pc1=ListToFormula(pl1);pc1;pc1=pc1->pcNext)
					pcEnd=pcEnd->pcNext=pc1;
			}
			pc=MakeSymbolInfoForm(FALSE,pl,pcStart);
			ArgListCheck(pc,ARG_UNKNOWN);
			EXIT("ListToFormula");
			return pc;
		}

		/* unknown symbol -- must be a definition or a keyword */

		pcStart=NULL;				/* initialize listhead */
		pcEnd=(CELLP)&pcStart;
		for(pl1=pl->plNext;pl1;pl1=pl1->plNext)
		{
			for(pc1=ListToFormula(pl1);pc1;pc1=pc1->pcNext)
				pcEnd=pcEnd->pcNext=pc1;
		}
		pc=CompileDummyForm(pl->psName,pcStart);
		EXIT("ListToFormula");
		return pc;
	}

	/* this must be a free standing identifier, number, or string */

	pc1=(CELLP)MemAlloc(sizeof(CELL));
	pc1->pfForm=pf=(FORMULAP)MemAlloc(sizeof(FORMULA));
	pf->nType=plList->nType;
	if(plList->nType==ATOM_STRING)
	{
		pf->psName=StrAlloc(plList->psName);
		pf->uValue.psString=pf->psName;
	}
	else
	{
		pf->psName=IdentAlloc(plList->psName);
		pf->uValue=plList->uValue;
	}
	pf->paAction=&aLiteralAction;
	EXIT("ListToFormula");
	return pc1;
}

/* ListToVarGen

Description:
	Parse a variables generator pair for a quantified formula.
*/

static BOOL ListToVarGen
(
	LISTP plList,						/* input list */
	CELLP *pcVars,						/* output variables list */
	CELLP *pcGen						/* output generator */
)
{
	LISTP pl2,pl4;
	CELLP pc;
	CELLP pcStart,pcEnd;

	if(!plList)							/* variables */
	{
		ErrorMessage("ListToVarGen:  No quantifier variables in %s formula\n",
			plList->psName);
		EXIT("ListToVarGen");
		return FALSE;
	}
	if(plList->nType!=ATOM_LISTP)
	{
		ErrorMessage("ListToVarGen:  Quantifier variables must be in a list\n");
		EXIT("ListToVarGen");
		return FALSE;
	}
	pcStart=NULL;						/* initialize variables listhead */
	pcEnd=(CELLP)&pcStart;
	for(pl4=plList->uValue.plList;pl4;pl4=pl4->plNext)
	{
		if(pl4->nType!=ATOM_IDENT)
		{
			ErrorMessage("ListToVarGen:  Invalid variable \"%s\" in quantifier variable list\n",
				pl4->psName);
			EXIT("ListToVarGen");
			return FALSE;
		}
		if(pl4->psName[0]!='?')
		{
			ErrorMessage("ListToVarGen:  Non variable \"%s\" in quantifier variable list\n",
				pl4->psName);
			EXIT("ListToVarGen");
			return FALSE;
		}
		pc=ListToFormula(pl4);
		if(pc)							/* discard errors */
			pcEnd=pcEnd->pcNext=pc;
	}
	pl2=plList->plNext;
	if(!pl2)							/* generator */
	{
		ErrorMessage("ListToVarGen:  Too few arguments to quantifier formula\n");
		EXIT("ListToVarGen");
		return FALSE;
	}
	*pcVars=pcStart;
	*pcGen=ListToFormula(pl2);			/* generator */
	if(!*pcVars||!*pcGen)
	{
		EXIT("ListToVarGen");
		return FALSE;
	}
	EXIT("ListToVarGen");
	return TRUE;
}

/* ListToVars

Description:
	Parse a variables list.
*/

static BOOL ListToVars
(
	LISTP plList,						/* input list */
	CELLP *pcVars,						/* output variables list */
	int *pnVars							/* output variable count */
)
{
	LISTP pl4;
	CELLP pc;
	CELLP pcStart,pcEnd;

	*pnVars=0;
	if(!plList)							/* variables */
	{
		ErrorMessage("ListToVars:  No variables in %s formula\n",
			plList->psName);
		EXIT("ListToVars");
		return FALSE;
	}
	if(plList->nType!=ATOM_LISTP)
	{
		ErrorMessage("ListToVars:  Variables must be in a list\n");
		EXIT("ListToVars");
		return FALSE;
	}
	pcStart=NULL;						/* initialize variables listhead */
	pcEnd=(CELLP)&pcStart;
	for(pl4=plList->uValue.plList;pl4;pl4=pl4->plNext)
	{
		if(pl4->nType!=ATOM_IDENT)
		{
			ErrorMessage("ListToVars:  Invalid variable in %s variable list\n",
				plList->psName);
			EXIT("ListToVars");
			return FALSE;
		}
		if(pl4->psName[0]!='?')
		{
			ErrorMessage("ListToVars:  Non variable in %s variable list\n",
				plList->psName);
			EXIT("ListToVars");
			return FALSE;
		}
		pc=ListToFormula(pl4);
		if(pc)							/* discard errors */
			pcEnd=pcEnd->pcNext=pc;
		(*pnVars)++;
	}
	*pcVars=pcStart;
	if(!*pcVars)
	{
		EXIT("ListToVars");
		return FALSE;
	}
	EXIT("ListToVars");
	return TRUE;
}

/* CopyCellList

Description:
	Create a copy of a cell list.
*/

DECLSPEC CELLP CopyCellList
(
	CELLP pcList						/* input cell list */
)
{
	CELLP pcStart;						/* pointer to result */
	CELLP pcEnd;						/* pointer end of result */
	CELLP pc;							/* pointer to input cell */

	ENTER("CopyCellList",TRUE);
	pcStart=NULL;						/* initialize list head */
	pcEnd=(CELLP)&pcStart;

	for(pc=pcList;pc;pc=pc->pcNext)
	{
		pcEnd=pcEnd->pcNext=(CELLP)MemAlloc(sizeof(CELL));
		pcEnd->pfForm=pc->pfForm;
	}
	EXIT("CopyCellList");
	return pcStart;
}

DECLSPEC CELLP CopyCellListReturnEndCell
(
	CELLP pcList,						/* input cell list */
	CELLP *ppcLastCellofCopy			/* set a pointer to last cell in copy */
)
{
	CELLP pcStart;						/* pointer to result */
	CELLP pcEnd;						/* pointer end of result */
	CELLP pc;							/* pointer to input cell */

	ENTER("CopyCellList",TRUE);
	pcStart=NULL;						/* initialize list head */
	pcEnd=(CELLP)&pcStart;

	for(pc=pcList;pc;pc=pc->pcNext)
	{
		pcEnd=pcEnd->pcNext=(CELLP)MemAlloc(sizeof(CELL));
		pcEnd->pfForm=pc->pfForm;
	}
	*ppcLastCellofCopy=pcStart?pcEnd:NULL;
	EXIT("CopyCellList");
	
	return pcStart;
}

DECLSPEC CELLP CopyPrefixCellList
(
	CELLP pcList,						/* input cell list */
	CELLP pcFirstPastPrefix,			/* First cell past prefix */
	CELLP *ppcLastCellofCopy			/* Set a pointer to last cell in copy */
)
{
	CELLP pcStart;						/* pointer to result */
	CELLP pcEnd;						/* pointer end of result */
	CELLP pc;

	ENTER("CopyPrefixCellList",TRUE);
	pcStart=NULL;						/* initialize list head */
	pcEnd=(CELLP)&pcStart;

	for(pc=pcList;pc!=pcFirstPastPrefix;pc=pc->pcNext)
	{
		pcEnd=pcEnd->pcNext=(CELLP)MemAlloc(sizeof(CELL));
		pcEnd->pfForm=pc->pfForm;
	}
	*ppcLastCellofCopy=pcStart?pcEnd:NULL;
	EXIT("CopyPrefixCellList");
	return pcStart;
}

/* CopyCell

Description:
	Create a copy of a cell.
*/

DECLSPEC CELLP CopyCell
(
	CELLP pcCell						/* input cell */
)
{
	CELLP pc;							/* pointer to input cell */

	ENTER("CopyCell",TRUE);
	pc=NULL;
	if(pcCell)
	{
		pc=(CELLP)MemAlloc(sizeof(CELL));
		pc->pfForm=pcCell->pfForm;
	}
	EXIT("CopyCell");
	return pc;
}

/* CopyCell2

Description:
	Create a copy of a cell.  Use the provided cell.
*/

DECLSPEC CELLP CopyCell2
(
	CELLP pc,							/* cell to overwrite */
	CELLP pcCell						/* input cell */
)
{
	pc->pcNext=NULL;
	pc->pfForm=pcCell?pcCell->pfForm:NULL;
	return pc;
}

/* CopyFormulaList

Description:
	Create a copy of a formula list.
Notes:
	Names are no longer copied!
*/

DECLSPEC CELLP CopyFormulaList
(
	CELLP pcList						/* input formula list */
)
{
	CELLP pcStart;						/* pointer to result */
	CELLP pcEnd;						/* pointer end of result */
	CELLP pc;							/* pointer to input formula */
	FORMULAP pf,pf1;					/* formula pointers */

	ENTER("CopyFormulaList",TRUE);
	pcStart=NULL;						/* initialize list head */
	pcEnd=(CELLP)&pcStart;

	for(pc=pcList;pc;pc=pc->pcNext)
	{
		pf1=pc->pfForm;
		pcEnd=pcEnd->pcNext=(CELLP)MemAlloc(sizeof(CELL));
		pcEnd->pfForm=pf=(FORMULAP)CopyAlloc(pf1,sizeof(FORMULA));
//		pf->psName=pf1->psName;
		switch(pf1->nType)
		{
			case ATOM_LISTP:
				pf->uValue.plList=CopyList(pf1->uValue.plList);
				break;
			case ATOM_ISPECP:
				pf->uValue.piISpec=(ISPECP)CopyAlloc(pf1->uValue.piISpec,sizeof(ISPEC));
				break;
		}
		if(pf1->pcVars)
			pf->pcVars=CopyFormulaList(pf1->pcVars);
		if(pf1->pcGenLit)
			pf->pcGenLit=CopyFormulaList(pf1->pcGenLit);
		if(pf1->pcArgs)
			pf->pcArgs=CopyFormulaList(pf1->pcArgs);
	}
	EXIT("CopyFormulaList");
	return pcStart;
}

/* CopyFormula

Description:
	Create a copy of a single a formula.
	If the formula is actually a list of formulas, only the first element
	is copied.
Notes:
	Names are no longer copied!  (They're already in the hash table, and
	we mark that separately.)
*/

DECLSPEC CELLP CopyFormula
(
	CELLP pcFormula					/* input formula */
)
{
	CELLP pc;						/* pointer to result */
	FORMULAP pf,pf1;

	ENTER("CopyFormula",TRUE);
	if(!pcFormula)
	{
		EXIT("CopyFormula");
		return NULL;
	}
	pf1=pcFormula->pfForm;
	pc=(CELLP)MemAlloc(sizeof(CELL));
	pc->pfForm=pf=(FORMULAP)CopyAlloc(pf1,sizeof(FORMULA));
//	pf->psName=pf1->psName;
	switch(pf1->nType)
	{
		case ATOM_LISTP:
			pf->uValue.plList=CopyList(pf1->uValue.plList);
			break;
		case ATOM_ISPECP:
			pf->uValue.piISpec=(ISPECP)CopyAlloc(pf1->uValue.piISpec,sizeof(ISPEC));
			break;
	}
	if(pf1->pcVars)
		pf->pcVars=CopyFormulaList(pf1->pcVars);
	if(pf1->pcGenLit)
		pf->pcGenLit=CopyFormulaList(pf1->pcGenLit);
	if(pf1->pcArgs)
		pf->pcArgs=CopyFormulaList(pf1->pcArgs);
	EXIT("CopyFormula");
	return pc;
}

/* MarkFormulaList

Description:
	Mark an entire list of formulas (for garbage collection).
*/

void MarkFormulaList
(
	CELLP pcFormula					/* input formula */
)
{
	CELLP pc;						/* pointer to list */

	ENTER("MarkFormulaList",TRUE);
	if(!pcFormula||ZoneMarkedQ(pcFormula))
	{
		EXIT("MarkFormulaList");
		return;
	}
	
	for(pc=pcFormula;pc;pc=pc->pcNext)
	{
		MarkForm(pc->pfForm);
		ZoneMark(pc->pfForm);
		ZoneMark(pc);
	}
	EXIT("MarkFormulaList");
}

/* MarkFormula

Description:
	Mark a single formula (for garbage collection).
*/

void MarkFormula
(
	CELLP pcFormula					/* input formula */
)
{
	ENTER("MarkFormula",TRUE);
	if(pcFormula)
	{
		if(ZoneMarkedQ(pcFormula))
		{
			EXIT("MarkFormulaList");
			return;
		}

		MarkForm(pcFormula->pfForm);
		ZoneMark(pcFormula->pfForm);
		ZoneMark(pcFormula);
	}
	EXIT("MarkFormula");
}

/* MarkForm

Description:
	Mark all the things pointed to by a single formula.
*/

static void MarkForm
(
	FORMULAP pf							/* input formula */
)
{
	ARRAYINFOP pai;
	CELLP *ppc;							/* array pointer */
	int i;

	ENTER("MarkForm",TRUE);
	if(pf)
	{
//		if(pf->psName)
//			ZoneMark(pf->psName);
		switch(pf->nType)
		{
			case ATOM_ISPECP:
				ZoneMark(pf->uValue.piISpec);
				break;
			case ATOM_OPERATORP:
				ZoneMark(pf->uValue.poOperator);
				break;
			case ATOM_ARRAYINFOP:
				pai=pf->uValue.paiArrayInfo;
				ZoneMark(pai);
//				ZoneMark(pai->psName);
				ZoneMark(pai->pnLimits);
				ZoneMark(pai->pnMultipliers);
				ZoneMark(pai->ppcArray);
				ppc=pai->ppcArray;
				for(i=0;i<pai->nCount;i++)
					MarkFormula(*ppc++);
				break;
		}
		if(pf->pcVars)
			MarkFormulaList(pf->pcVars);
		if(pf->pcGenLit)
			MarkFormulaList(pf->pcGenLit);	/* pcGenLit can point to local variables! */
		if(pf->pcArgs)
			MarkFormulaList(pf->pcArgs);
	}
	EXIT("MarkForm");
}

/* FormulaListSizeOf -----------------------------------------------------------

Description:
	Calculate the incremental size of an entire list of formulas.
	Marked memory is ignored.
*/

void FormulaListSizeOf
(
	CELLP pcFormula,				/* input formula */
	int *pnSize
)
{
	CELLP pc;						/* pointer to list */

	ENTER("FormulaListSizeOf",TRUE);
	for(pc=pcFormula;pc;pc=pc->pcNext)
	{
		if(!ZoneMarkedQ(pc))
			*pnSize+=ZoneSizeOf(pc);
		if(!ZoneMarkedQ(pc->pfForm))
			*pnSize+=ZoneSizeOf(pc->pfForm);
		FormSizeOf(pc->pfForm,pnSize);
		MarkForm(pc->pfForm);
		ZoneMark(pc->pfForm);
		ZoneMark(pc);
	}
	EXIT("FormulaListSizeOf");
}

/* FormulaSizeOf

Description:
	Calculate the incremental size of a formula.
	Marked memory is ignored.
*/

void FormulaSizeOf
(
	CELLP pcFormula,				/* input formula */
	int *pnSize
)
{
	ENTER("FormulaSizeOf",TRUE);
	if(pcFormula)
	{
		if(!ZoneMarkedQ(pcFormula))
			*pnSize+=ZoneSizeOf(pcFormula);
		if(!ZoneMarkedQ(pcFormula->pfForm))
			*pnSize+=ZoneSizeOf(pcFormula->pfForm);
		FormSizeOf(pcFormula->pfForm,pnSize);
		MarkForm(pcFormula->pfForm);
		ZoneMark(pcFormula->pfForm);
		ZoneMark(pcFormula);
	}
	EXIT("FormulaSizeOf");
}

/* FormSizeOf

Description:
	Calculate the incremental size of a form.
	Marked memory is ignored.
*/

static void FormSizeOf
(
	FORMULAP pf,						/* input formula */
	int *pnSize
)
{
	ARRAYINFOP pai;
	CELLP *ppc;							/* array pointer */
	int i;

	ENTER("FormSizeOf",TRUE);
	if(pf)
	{
		switch(pf->nType)
		{
			case ATOM_ISPECP:
				if(!ZoneMarkedQ(pf->uValue.piISpec))
				{
					*pnSize+=ZoneSizeOf(pf->uValue.piISpec);
					ZoneMark(pf->uValue.piISpec);
				}
				break;
			case ATOM_OPERATORP:
				if(!ZoneMarkedQ(pf->uValue.poOperator))
				{
					*pnSize+=ZoneSizeOf(pf->uValue.poOperator);
					ZoneMark(pf->uValue.poOperator);
				}
				break;
			case ATOM_ARRAYINFOP:
				pai=pf->uValue.paiArrayInfo;
				if(!ZoneMarkedQ(pai))
				{
					*pnSize+=ZoneSizeOf(pai);
					ZoneMark(pai);
				}
				if(!ZoneMarkedQ(pai->pnLimits))
				{
					*pnSize+=ZoneSizeOf(pai->pnLimits);
					ZoneMark(pai->pnLimits);
				}
				if(!ZoneMarkedQ(pai->pnMultipliers))
				{
					*pnSize+=ZoneSizeOf(pai->pnMultipliers);
					ZoneMark(pai->pnMultipliers);
				}
				if(!ZoneMarkedQ(pai->ppcArray))
				{
					*pnSize+=ZoneSizeOf(pai->ppcArray);
					ZoneMark(pai->ppcArray);
				}
				ppc=pai->ppcArray;
				for(i=0;i<pai->nCount;i++)
				{
					FormulaSizeOf(*ppc,pnSize);
					MarkFormula(*ppc++);
				}
				break;
		}
		if(pf->pcVars)
		{
			FormulaListSizeOf(pf->pcVars,pnSize);
			MarkFormulaList(pf->pcVars);
		}
		if(pf->pcGenLit)
		{
			FormulaListSizeOf(pf->pcGenLit,pnSize);	/* pcGenLit can point to local variables! */
			MarkFormulaList(pf->pcGenLit);	/* pcGenLit can point to local variables! */
		}
		if(pf->pcArgs)
		{
			FormulaListSizeOf(pf->pcArgs,pnSize);
			MarkFormulaList(pf->pcArgs);
		}
	}
	EXIT("FormSizeOf");
}

/* AppendFormula --------------------------------------------------------------

Description:
	Append a formula list onto the end of another.
Notes:
	The prefix list is optionally copied!
*/

CELLP AppendFormula
(
	BOOL bCopy,							/* copy the prefix list */
	CELLP pcFirst,						/* prefix list */
	CELLP pcSecond						/* suffix list */
)
{
	CELLP pcStart;						/* pointer to result */
	CELLP pcEnd;						/* pointer end of result */
	CELLP pc;

	ENTER("AppendFormula",TRUE);
	if(!pcFirst)
    	return pcSecond;
	if(bCopy)
	{
		pcStart=NULL;						/* initialize list head */
		pcEnd=(CELLP)&pcStart;

		for(pc=pcFirst;pc;pc=pc->pcNext)
		{
			pcEnd=pcEnd->pcNext=(CELLP)MemAlloc(sizeof(CELL));
			pcEnd->pfForm=pc->pfForm;
		}
	}
	else
	{
		pcStart=pcFirst;
		for(pcEnd=pcFirst;pcEnd->pcNext;pcEnd=pcEnd->pcNext);	/* find end of first list */
	}
	pcEnd->pcNext=pcSecond;
	EXIT("AppendFormula");
	return pcStart;
}

/* ReverseFormulaList

Description:
	Reverse a formula list.
Notes:
	The formula is not copied!
*/

CELLP ReverseFormulaList
(
	CELLP pcList
)
{
	CELLP pc1,pc2,pc3;

	pc3=NULL;
	ENTER("ReverseFormulaList",TRUE);
	for(pc1=pcList;pc1;pc1=pc2)
	{
		pc2=pc1->pcNext;
		pc1->pcNext=pc3;
		pc3=pc1;
	}
	EXIT("ReverseFormulaList");
	return pc3;
}

/* FormulaDifference

Description:
	Calculate the set difference of two lists of formulas.
Note:
	This routine only works on the top level!
Scheme:

*/

CELLP FormulaDifference
(
	CELLP pcFormula1,					/* input formula (minuend) */
	CELLP pcFormula2					/* input formula (subtrahend) */
)
{
	CELLP pcStart,pcEnd;				/* result listhead */
	CELLP pc1,pc2;						/* pointer to formula */

	ENTER("FormulaDifference",TRUE);
	pcStart=NULL;						/* initialize listhead */
	pcEnd=(CELLP)&pcStart;

	if(!pcFormula1)
		pcStart=NULL;
	else if(!pcFormula2)
		pcStart=CopyCellList(pcFormula1);
	else
	{
		for(pc1=pcFormula1;pc1;pc1=pc1->pcNext)
		{
			for(pc2=pcFormula2;pc2;pc2=pc2->pcNext)
			{
				if(FormulaEqQ(pc1,pc2))
					break;
			}
			if(!pc2)					/* if no match */
				pcEnd=pcEnd->pcNext=CopyCell(pc1);
		}
	}
	EXIT("FormulaDifference");
	return pcStart;
}

/* FormulaIntersection

Description:
	Calculate the set intersection of two lists.
Scheme:

(define (intersection list1 list2)
  (letrec ((iter
		(lambda(list1 result)
		  (cond ((null? list1) result)
			((memq (first list1) list2)
			 (iter (rest list1) (cons (first list1) result)))
			(else (iter (rest list1) result))))))
	(if (or (null? list1) (null? list2))
	'()
	(iter list1 '()))))
*/

CELLP FormulaIntersection
(
	CELLP pcList1,						/* input list */
	CELLP pcList2						/* input list */
)
{
	CELLP pc1,pc2;
	CELLP pcStart;
	CELLP pcEnd;

	ENTER("FormulaIntersection",TRUE);
	pcStart=NULL;						/* initialize list head */
	pcEnd=(CELLP)&pcStart;

	for(pc1=pcList1;pc1;pc1=pc1->pcNext)
	{
		for(pc2=pcList2;pc2;pc2=pc2->pcNext)
		{
			if(FormulaEqQ(pc1,pc2))
			{
				pcEnd=pcEnd->pcNext=CopyCell(pc2);
				break;
			}
		}
	}
	EXIT("FormulaIntersection");
	return pcStart;
}

/* FormulaIntersects

Description:
	Determine if two lists of formulas have anything in common.
*/

BOOL FormulaIntersects
(
	CELLP pcList1,						/* input list */
	CELLP pcList2						/* input list */
)
{
	CELLP pc1,pc2;

	ENTER("FormulaIntersects",TRUE);

	for(pc1=pcList1;pc1;pc1=pc1->pcNext)
		for(pc2=pcList2;pc2;pc2=pc2->pcNext)
			if(FormulaEqQ(pc1,pc2))
			{
				EXIT("FormulaIntersects");
				return TRUE;
			}
	EXIT("FormulaIntersects");
	return FALSE;
}

/* FormulaUnion

Description:
	Calculate the set union of two formula lists.
Note:
	We assume that neither list has any duplicate elements.
	The returned list is a copy!
*/

CELLP FormulaUnion
(
	CELLP pcList1,					/* input list */
	CELLP pcList2					/* input list */
)
{
	CELLP pc1,pc2;
	CELLP pcStart;

	ENTER("FormulaUnion",TRUE);
	if(!pcList1)
		pcStart=pcList2;
	else if(!pcList2)
//		pcStart=CopyFormulaList(pcList1);
		pcStart=CopyCellList(pcList1);
	else
	{
		pcStart=pcList2;
		for(pc1=pcList1;pc1;pc1=pc1->pcNext)
		{
			for(pc2=pcList2;pc2;pc2=pc2->pcNext)
			{
				if(FormulaEqQ(pc1,pc2))
					break;
			}
			if(!pc2)
			{
				pc2=CopyCell(pc1);
				pc2->pcNext=pcStart;
				pcStart=pc2;
			}
		}
	}
	EXIT("FormulaUnion");
	return pcStart;
}

/* FormulaListEqQ

Description:
	Return TRUE iff pcFormula1 has the same structure and contents as pcFormula2.
*/

BOOL FormulaListEqQ
(
	CELLP pcFormula1,
	CELLP pcFormula2
)
{
	CELLP pc1,pc2;					/* list pointers */

	ENTER("FormulaListEqQ",TRUE);
	for
	(
		pc1=pcFormula1,pc2=pcFormula2;
		pc1&&pc2;
		pc1=pc1->pcNext,pc2=pc2->pcNext
	)
	{
		if(!FormulaEqQ(pc1,pc2))
		{
			EXIT("FormulaListEqQ");
			return FALSE;
		}
	}
	EXIT("FormulaListEqQ");
	return !(pc1||pc2);					/* formulas are equal if both are exhausted */
}

/* FormulaEqQ

Description:
	Return TRUE iff First(pcFormula1) has the same structure and contents as
	First(pcFormula2).
Notes:
	For numeric values, we compare the values numerically, to allow for integer
	and float mismatches.  For everything else, we compare the literal names.
	We also compare the genlit and variables and argument lists.
*/

BOOL FormulaEqQ
(
	CELLP pcFormula1,
	CELLP pcFormula2
)
{
	ENTER("FormulaEqQ",TRUE);

	//FB check pointer equality first.

	if(pcFormula1==pcFormula2||pcFormula1->pfForm==pcFormula2->pfForm)
	  return TRUE;

	if(!pcFormula1&&!pcFormula2)
	{
		EXIT("FormulaEqQ");
		return TRUE;
	}
	if(!pcFormula1||!pcFormula2)
	{
		EXIT("FormulaEqQ");
		return FALSE;
	}

	/* treat numeric equality special */

	if(pcFormula1->pfForm->nType==ATOM_INTEGER)
	{
		if(pcFormula2->pfForm->nType==ATOM_INTEGER)
		{
			EXIT("FormulaEqQ");
			return pcFormula1->pfForm->uValue.nInteger==pcFormula2->pfForm->uValue.nInteger;
		}
		else if(pcFormula2->pfForm->nType==ATOM_FLOAT)
		{
			EXIT("FormulaEqQ");
			return pcFormula1->pfForm->uValue.nInteger==pcFormula2->pfForm->uValue.dfFloat;
		}
		else
		{
			EXIT("FormulaEqQ");
			return FALSE;
		}
	}
	else if(pcFormula1->pfForm->nType==ATOM_FLOAT)
	{
		if(pcFormula2->pfForm->nType==ATOM_INTEGER)
		{
			EXIT("FormulaEqQ");
			return pcFormula1->pfForm->uValue.dfFloat==pcFormula2->pfForm->uValue.nInteger;
		}
		else if(pcFormula2->pfForm->nType==ATOM_FLOAT)
		{
			EXIT("FormulaEqQ");
			return pcFormula1->pfForm->uValue.dfFloat==pcFormula2->pfForm->uValue.dfFloat;
		}
		else
		{
			EXIT("FormulaEqQ");
			return FALSE;
		}
	}
	else if(pcFormula1->pfForm->nType==ATOM_STRING)
	{
		if(!StringEqQ(pcFormula1->pfForm->psName,pcFormula2->pfForm->psName))
		{
			EXIT("FormulaEqQ");
			return FALSE;
		}
	}
	else
	{
		/* other things */

		if(!StringEqQ(IdentName(pcFormula1->pfForm),IdentName(pcFormula2->pfForm)))
		{
			EXIT("FormulaEqQ");
			return FALSE;
		}
	}

	if(pcFormula1->pfForm->pcVars)
	{
		if(!FormulaListEqQ(pcFormula1->pfForm->pcVars,pcFormula2->pfForm->pcVars))
		{
			EXIT("FormulaEqQ");
			return FALSE;
		}
	}
	else if(pcFormula2->pfForm->pcVars)
	{
		EXIT("FormulaEqQ");
		return FALSE;
	}

	if(pcFormula1->pfForm->pcGenLit)
	{
		if(!FormulaListEqQ(pcFormula1->pfForm->pcGenLit,pcFormula2->pfForm->pcGenLit))
		{
			EXIT("FormulaEqQ");
			return FALSE;
		}
	}
	else if(pcFormula2->pfForm->pcGenLit)
	{
		EXIT("FormulaEqQ");
		return FALSE;
	}

	if(pcFormula1->pfForm->pcArgs)
	{
		if(!FormulaListEqQ(pcFormula1->pfForm->pcArgs,pcFormula2->pfForm->pcArgs))
		{
			EXIT("FormulaEqQ");
			return FALSE;
		}
	}
	else if(pcFormula2->pfForm->pcArgs)
	{
		EXIT("FormulaEqQ");
		return FALSE;
	}

	EXIT("FormulaEqQ");
	return TRUE;					/* formulas are equal */
}

/* FormulaMemQ

Description:
	Locate a subformula within a list of formulas.
Note:
	This routine works with "first" of subformula only!
*/

CELLP FormulaMemQ
(
	CELLP pcSubFormula,			/* search key */
	CELLP pcFormula				/* search domain */
)
{
	CELLP pc;

	ENTER("FormulaMemQ",TRUE);
	for(pc=pcFormula;pc;pc=pc->pcNext)
		if(FormulaEqQ(pcSubFormula,pc))
			break;
	EXIT("FormulaMemQ");
	return pc;
}

/* Formula Print Routines ------------------------------------------------------ */

/* PrintFormulaList

Description:
	Print a list of formulas in lisp format.
*/

DECLSPEC void PrintFormulaList
(
	FILE *pfStream,						/* output file stream */
	CELLP pcFormula						/* (list of) formulas to display */
)
{
	CELLP pc;							/* formula pointer */

	ENTER("PrintFormulaList",TRUE);
	if(pcFormula->pcNext)
	{
		CommandPuts("(",pfStream);
		for(pc=pcFormula;pc;pc=pc->pcNext)
		{
			PrintFormula(pfStream,pc,1);
			if(pc->pcNext)
				CommandPrintf(pfStream," ");
		}
		CommandPuts(")\n",pfStream);
	}
	else
		PrintFormula(pfStream,pcFormula,0);
	EXIT("PrintFormulaList");
}

/* PrintFormula

Description:
	Print a formula in lisp format.
*/

static char *psBlanks="                                                                                ";

DECLSPEC void PrintFormula
(
	FILE *pfStream,						/* output file stream */
	CELLP pcFormula,					/* formula to display */
	int nLevel							/* recursion level (initialize to 0) */
)
{
	CELLP pc;							/* formula pointer */
	int nCol;							/* current column location */

	ENTER("PrintFormula",TRUE);
	if(nLevel)
		CommandPuts("\n",pfStream);
	CommandPrintf(pfStream,"%.*s(",2*nLevel,psBlanks);
	
	nCol=2*nLevel+1;
	if(pcFormula)
	{
		PrintTerm(pfStream,pcFormula,nLevel,nCol);
		nCol=2*(nLevel+1);
		if(pcFormula->pfForm->pcVars)
		{
			CommandPrintf(pfStream,"\n%.*s(",2*(nLevel+1),psBlanks);
			nCol++;
			for(pc=pcFormula->pfForm->pcVars;pc;pc=pc->pcNext)
			{
				nCol=PrintTerm(pfStream,pc,nLevel+1,nCol);
				if(pc->pcNext)
				{
					CommandPrintf(pfStream," ");
					nCol++;
				}
			}
			CommandPrintf(pfStream,")");
		}
		nCol=2*(nLevel+1);
		for(pc=pcFormula->pfForm->pcGenLit;pc;pc=pc->pcNext)
		{
			CommandPrintf(pfStream," ");
			nCol++;
			if(pc->pfForm->pcArgs||pc->pfForm->pcVars||pc->pfForm->pcGenLit)
				PrintFormula(pfStream,pc,nLevel+1);
			else
			{
				CommandPrintf(pfStream,"(");
				nCol=PrintTerm(pfStream,pc,nLevel+1,nCol);
				CommandPrintf(pfStream,")");
			}
		}
		nCol=2*(nLevel+1);
		for(pc=pcFormula->pfForm->pcArgs;pc;pc=pc->pcNext)
		{
			CommandPrintf(pfStream," ");
			nCol++;
			if(pc->pfForm->pcArgs||pc->pfForm->pcVars||pc->pfForm->pcGenLit)
				PrintFormula(pfStream,pc,nLevel+1);
			else
				nCol=PrintTerm(pfStream,pc,nLevel+1,nCol);
		}
	}
	CommandPrintf(pfStream,")");
	if(!nLevel)
		CommandPrintf(pfStream,"\n");
	EXIT("PrintFormula");
}

/* PrintTerm

Description:
	Print a single term of a formula.
*/

int PrintTerm
(
	FILE *pfStream,						/* output file stream */
	CELLP pcFormula,					/* term to display */
	int nLevel,							/* recursion level (initialize to 0) */
	int nCol							/* current column */
)
{
	int nLength;						/* string length */
	char ac[40];						/* string buffer */
	char *ps;							/* string pointer */
	ENTER("PrintTerm",TRUE);
	switch(pcFormula->pfForm->nType)
	{
		case ATOM_STRING:
			ps=GetName(pcFormula->pfForm,ac);
			nLength=strlen(ps)+2;
			if(nCol>2*(nLevel+1)&&nCol+nLength>80)
			{
				CommandPrintf(pfStream,"\n%.*s",2*(nLevel+1),psBlanks);
				nCol=2*(nLevel+1);
			}
			CommandPrintf(pfStream,"\"%s\"",ps);
			nCol+=nLength;
			break;
		case ATOM_IDENT:
		case ATOM_INFINITY:
		case ATOM_ISPECP:
		case ATOM_FUNCTIONP:
		case ATOM_SYMBOLINFOP:
		case ATOM_INTEGER:
		case ATOM_FLOAT:
			ps=GetName(pcFormula->pfForm,ac);
			nLength=strlen(ps);
			if(nCol>2*(nLevel+1)&&nCol+nLength>80)
			{
				CommandPrintf(pfStream,"\n%.*s",2*(nLevel+1),psBlanks);
				nCol=2*(nLevel+1);
			}
			CommandPrintf(pfStream,"%s",ps);
			nCol+=nLength;
			break;
		case ATOM_OPERATORP:
			{
				OPERATORP po;
				po=pcFormula->pfForm->uValue.poOperator;
				PrintFormula(pfStream,po->pcName,nLevel);
			}
			break;
		case ATOM_LISTP:
			PrintList(pfStream,pcFormula->pfForm->uValue.plList,nLevel+1);
			break;
		default:
			ErrorMessage("PrintTerm:  Unsupported atom type %d\n",
				pcFormula->pfForm->nType);
	}
	EXIT("PrintTerm");
	return nCol;
}

/* PrintFlatFormula

Description:
	Print a formula in lisp format.
	Keep everything on a single line.
*/

DECLSPEC void PrintFlatFormula
(
	FILE *pfStream,						/* output file stream */
	CELLP pcFormula						/* formula to display */
)
{
	CELLP pc;							/* formula pointer */

	ENTER("PrintFlatFormula",TRUE);
	
	CommandPrintf(pfStream,"(");
	if(pcFormula)
	{
		PrintFlatTerm(pfStream,pcFormula);
		if(pcFormula->pfForm->pcVars)
		{
			CommandPrintf(pfStream," (");
			for(pc=pcFormula->pfForm->pcVars;pc;pc=pc->pcNext)
			{
				PrintFlatTerm(pfStream,pc);
				if(pc->pcNext)
					CommandPrintf(pfStream," ");
			}
			CommandPrintf(pfStream,")");
		}
		for(pc=pcFormula->pfForm->pcGenLit;pc;pc=pc->pcNext)
		{
			CommandPrintf(pfStream," ");
			if(pc->pfForm->pcArgs||pc->pfForm->pcVars||pc->pfForm->pcGenLit)
				PrintFlatFormula(pfStream,pc);
			else
			{
				CommandPrintf(pfStream,"(");
				PrintFlatTerm(pfStream,pc);
				CommandPrintf(pfStream,")");
			}
		}
		for(pc=pcFormula->pfForm->pcArgs;pc;pc=pc->pcNext)
		{
			CommandPrintf(pfStream," ");
			if(pc->pfForm->pcArgs||pc->pfForm->pcVars||pc->pfForm->pcGenLit)
				PrintFlatFormula(pfStream,pc);
			else
				PrintFlatTerm(pfStream,pc);
		}
	}
	CommandPrintf(pfStream,")");
	EXIT("PrintFlatFormula");
}

/* PrintFlatTerm

Description:
	Print a single term of a formula.
	Keep everything on the same line.
*/

void PrintFlatTerm
(
	FILE *pfStream,						/* output file stream */
	CELLP pcFormula						/* term to display */
)
{
	char ac[40];						/* string buffer */
	char *ps;							/* string pointer */
	ENTER("PrintTerm",TRUE);
	switch(pcFormula->pfForm->nType)
	{
		case ATOM_STRING:
			ps=GetName(pcFormula->pfForm,ac);
			CommandPrintf(pfStream,"\"%s\"",ps);
			break;
		case ATOM_IDENT:
		case ATOM_INFINITY:
		case ATOM_ISPECP:
		case ATOM_FUNCTIONP:
		case ATOM_SYMBOLINFOP:
		case ATOM_INTEGER:
		case ATOM_FLOAT:
			ps=GetName(pcFormula->pfForm,ac);
			CommandPrintf(pfStream,"%s",ps);
			break;
		case ATOM_OPERATORP:
			{
				OPERATORP po;
				po=pcFormula->pfForm->uValue.poOperator;
				PrintFlatFormula(pfStream,po->pcName);
			}
			break;
//		case ATOM_LISTP:
//			PrintList(pfStream,pcFormula->pfForm->uValue.plList,1);
//			break;
		default:
			ErrorMessage("PrintFlatTerm:  Unsupported atom type %d\n",
				pcFormula->pfForm->nType);
	}
	EXIT("PrintFlatTerm");
}

/* FormulaToInteger ------------------------------------------------------------

Description:
	Convert a "term" formula into an integer by evaluating it and rounding
	if necessary.
*/

DECLSPEC BOOL FormulaToInteger
(
	CELLP pcTerm,						/* term to evaluate */
	LINEARPLANP plpLinearPlan,			/* current plan */
	BINDINGP pbBindings,				/* current bindings */
	int *pnValue						/* result */
)
{
	CELLP pcValue;

	pcValue=ComputeTerm(pcTerm,plpLinearPlan,pbBindings);
	if(!pcValue)
		TermError("formula-to-integer",pcTerm,pbBindings);
	switch(pcValue->pfForm->nType)
	{
		case ATOM_INTEGER:
			*pnValue=pcValue->pfForm->uValue.nInteger;
			break;
		case ATOM_FLOAT:
			if(pcValue->pfForm->uValue.dfFloat<0.0)
				*pnValue=(int)(pcValue->pfForm->uValue.dfFloat-0.5);
			else
				*pnValue=(int)(pcValue->pfForm->uValue.dfFloat+0.5);
			break;
		default:
			ErrorMessage("FormulaToInteger:  Argument must be numeric\n");
			return FALSE;
	}
	return TRUE;
}

/* FormulaToDouble

Description:
	Convert a "term" formula into a double by evaluating it.
*/

DECLSPEC BOOL FormulaToDouble
(
	CELLP pcTerm,						/* term to evaluate */
	LINEARPLANP plpLinearPlan,			/* current plan */
	BINDINGP pbBindings,				/* current bindings */
	double *pdfValue					/* result */
)
{
	CELLP pcValue;

	pcValue=ComputeTerm(pcTerm,plpLinearPlan,pbBindings);
	if(!pcValue)
	{
		TermError("formula-to-double",pcTerm,pbBindings);
		return FALSE;
	}
	switch(pcValue->pfForm->nType)
	{
		case ATOM_INTEGER:
			*pdfValue=(double)pcValue->pfForm->uValue.nInteger;
			break;
		case ATOM_FLOAT:
			*pdfValue=pcValue->pfForm->uValue.dfFloat;
			break;
		default:
			ErrorMessage("FormulaToDouble:  Argument must be numeric\n");
			return FALSE;
	}
	return TRUE;
}

/* FormulaToString

Description:
	Convert a "term" formula into a string by evaluating it.
*/

DECLSPEC BOOL FormulaToString
(
	CELLP pcTerm,						/* term to evaluate */
	LINEARPLANP plpLinearPlan,			/* current plan */
	BINDINGP pbBindings,				/* current bindings */
	char **ppsValue						/* result */
)
{
	CELLP pcValue;

	pcValue=ComputeTerm(pcTerm,plpLinearPlan,pbBindings);
	if(!pcValue)
		TermError("formula-to-string",pcTerm,pbBindings);
	switch(pcValue->pfForm->nType)
	{
		case ATOM_STRING:
			*ppsValue=pcValue->pfForm->uValue.psString;
			break;
		default:
			ErrorMessage("FormulaToString:  Argument must be a string\n");
			return FALSE;
	}
	return TRUE;
}

/* ArgListCheck ----------------------------------------------------------------

Description:
	Type check an argument list.
*/

static void ArgListCheck
(
	CELLP pcFormula,					/* formula to check */ 
	int nType							/* expected argument type */	
)
{
	CELLP pc,pc1;
	int n;
	char ac[40];

	switch(nType)
	{
		case ARG_BOOL:
			n=0;
			for(pc=pcFormula->pfForm->pcArgs;pc;pc=pc->pcNext)
			{
				n++;
				if(!pc->pfForm->paAction->pfEval&&!pc->pfForm->paAction->pfProgress)			/* if non-predicate */
					ErrorMessage("%s argument %d, %s: expected predicate or boolean expression\n",
						GetName(pcFormula->pfForm,ac),n,GetName(pc->pfForm,ac));
			}
			n=0;
			for(pc=pcFormula->pfForm->pcVars;pc;pc=pc->pcNext)
			{
				n++;
				if(!VarQ(pc))			/* if non-variable */
					ErrorMessage("%s variable %d, %s: invalid variable name\n",
						GetName(pcFormula->pfForm,ac),n,GetName(pc->pfForm,ac));
			}
			break;
		case ARG_TERM:
			n=0;
			for(pc=pcFormula->pfForm->pcArgs;pc;pc=pc->pcNext)
			{
				n++;
				if(pc->pfForm->nType!=ATOM_IDENT&&
					pc->pfForm->nType!=ATOM_INTEGER&&
					pc->pfForm->nType!=ATOM_FLOAT&&
					pc->pfForm->nType!=ATOM_STRING&&
					!pc->pfForm->paAction->pfCompute)
					ErrorMessage("%s argument %d, %s: expected term\n",
						GetName(pcFormula->pfForm,ac),n,GetName(pc->pfForm,ac));
			}
			break;
		case ARG_DESCRIBED:
			n=0;
			for(pc=pcFormula->pfForm->pcArgs;pc;pc=pc->pcNext)
			{
				n++;
				if(StringEqQ(IdentName(pc->pfForm),apsStringTab[STRING_EQ]))	/* if function equate */
				{
					pc1=pc->pfForm->pcArgs;
					if(pc1->pfForm->nType!=ATOM_SYMBOLINFOP||
						!DescribedQ(pc1->pfForm->uValue.psiSymbolInfo)||
						!FunctionQ(pc1->pfForm->uValue.psiSymbolInfo))
						ErrorMessage("%s argument %d, %s: expected described function\n",
							GetName(pcFormula->pfForm,ac),n,GetName(pc1->pfForm,ac));
				}
				else if(pc->pfForm->nType!=ATOM_SYMBOLINFOP||
					!DescribedQ(pc->pfForm->uValue.psiSymbolInfo))
					ErrorMessage("%s argument %d, %s: expected described symbol\n",
						GetName(pcFormula->pfForm,ac),n,GetName(pc->pfForm,ac));
			}
			break;
		case ARG_UNKNOWN:
			break;						/* comment out break for debug */
		default:
			ErrorMessage("%s arguments unchecked\n",GetName(pcFormula->pfForm,ac));
	}
}

/* SaveName

Description:
	Create and fill in the name for a formula that doesn't have one.
	The resulting name is linked into the ident hash table if it doesn't exist.
*/

DECLSPEC char *SaveName
(
	FORMULAP pf 
)
{
	char acBuffer[40];
	char *ps;

	switch(pf->nType)
	{
		case ATOM_FLOAT:
			sprintf(acBuffer,"%f",pf->uValue.dfFloat);
			for(ps=acBuffer+strlen(acBuffer);ps>acBuffer&&*ps=='0';--ps);
				ps[1]=0;				/* delete trailing zeros */
			pf->psName=IdentAlloc(acBuffer);
			break;
		case ATOM_INTEGER:
			sprintf(acBuffer,"%d",pf->uValue.nInteger);
			pf->psName=IdentAlloc(acBuffer);
			break;
		default:
			ErrorMessage("MakeName:  Unexpected atomic type: %d\n",pf->nType);
	}
	return pf->psName;
}

/* MakeName

Description:
	Create and fill in the name for a formula that doesn't have one.
	The name is not linked into the ident hash table.
*/

DECLSPEC char *MakeName
(
	FORMULAP pf,
	char acBuffer[40]
)
{
	char *ps;

	switch(pf->nType)
	{
		case ATOM_FLOAT:
			sprintf(acBuffer,"%f",pf->uValue.dfFloat);
			for(ps=acBuffer+strlen(acBuffer);ps>acBuffer&&*ps=='0';--ps);
				ps[1]=0;				/* delete trailing zeros */
			break;
		case ATOM_INTEGER:
			sprintf(acBuffer,"%d",pf->uValue.nInteger);
			break;
		default:
			ErrorMessage("MakeName:  Unexpected atomic type: %d\n",pf->nType);
	}
	return acBuffer;
}

/* Compile Routines --------------------------------------------------------- */

/* CompileArgForm

Description:
	Compile a formula with 0 or more arguments.
	The argument count is not checked (it should already be done).
*/

DECLSPEC CELLP CompileArgForm
(
	ACTIONP paAction,					/* action pointer */
	CELLP pcArgs
)
{
	CELLP pc;
	FORMULAP pf;

	ENTER("CompileArgForm",FALSE);
	pc=(CELLP)MemAlloc(sizeof(CELL));
	pc->pfForm=pf=(FORMULAP)MemAlloc(sizeof(FORMULA));
	pf->nType=ATOM_IDENT;
	pf->paAction=paAction;
	pf->psName=IdentAlloc(paAction->psName);
	pf->pcArgs=pcArgs;
	EXIT("CompileArgForm");
	return pc;
}

/* CompileQForm

Description:
	Compile a quantified formula.
*/

static CELLP CompileQForm
(
	ACTIONP paAction,					/* action pointer */
	CELLP pcVariables,					/* free variables */
	CELLP pcGenLit,						/* generator formula (such-that clause) */
	CELLP pcFormula
)
{
	CELLP pc;
	FORMULAP pf;

	ENTER("CompileQForm",FALSE);
	pc=(CELLP)MemAlloc(sizeof(CELL));
	pc->pfForm=pf=(FORMULAP)MemAlloc(sizeof(FORMULA));
	pf->nType=ATOM_IDENT;
	pf->paAction=paAction;
	pf->psName=IdentAlloc(paAction->psName);

	pf->pcVars=pcVariables;
	pf->pcGenLit=pcGenLit;
	if(pcGenLit)
		pcGenLit->pcNext=NULL;
	pf->pcArgs=pcFormula;
	if(pcFormula)
		pcFormula->pcNext=NULL;

	EXIT("CompileQForm");
	return pc;
}

/* CompileArrayForm

Description:
	Compile an array formula
*/

static CELLP CompileArrayForm
(
	char *psName,						/* array name */
	CELLP pcArgs
)
{
	CELLP pc;
	FORMULAP pf;

	ENTER("CompileArrayForm",FALSE);
	pc=(CELLP)MemAlloc(sizeof(CELL));
	pc->pfForm=pf=(FORMULAP)MemAlloc(sizeof(FORMULA));
	pf->nType=ATOM_IDENT;
	pf->paAction=&aArrayAction;
	pf->psName=IdentAlloc(psName);
	pf->pcArgs=pcArgs;
	EXIT("CompileArrayForm");
	return pc;
}

/* CompileDummyForm

Description:
	Compile a dummy formula.
	We didn't recognize the initial token, so we assume that some enclosing
	formula treats the token as a keyword.
*/

DECLSPEC CELLP CompileDummyForm
(
	char *psName,						/* name of initial token */
	CELLP pcArgs
)
{
	CELLP pc;
	FORMULAP pf;

	ENTER("CompileDummyForm",FALSE);
	pc=(CELLP)MemAlloc(sizeof(CELL));
	pc->pfForm=pf=(FORMULAP)MemAlloc(sizeof(FORMULA));
	pf->nType=ATOM_IDENT;
	pf->paAction=&aDummyAction;
	pf->psName=IdentAlloc(psName);
	pf->pcArgs=pcArgs;
	EXIT("CompileDummyForm");
	return pc;
}

/* CompileISpecForm

Description:
	Compile an ISpec (interval) formula.
*/

static CELLP CompileISpecForm
(
	CELLP pcArg1,
	CELLP pcArg2,
	CELLP pcArg3,
	CELLP pcArg4
)
{
	CELLP pc;
	FORMULAP pf;
	ISPECP pi;
	char acBuffer[60];
	char ac[40];						// string buffer

	ENTER("CompileISpecForm",FALSE);
	pi=(ISPECP)MemAlloc(sizeof(ISPEC));
	if(StringEqQ(IdentName(pcArg1->pfForm),apsStringTab[STRING_LPAREN]))
		pi->bLowerOpen=TRUE;
	switch(pcArg2->pfForm->nType)
	{
		case ATOM_INTEGER:
			pi->dfLower=(double)pcArg2->pfForm->uValue.nInteger;
			break;
		case ATOM_FLOAT:
			pi->dfLower=pcArg2->pfForm->uValue.dfFloat;
			break;
		case ATOM_INFINITY:
			pi->bLowerInfinite=TRUE;
			break;
		default:
			ErrorMessage("CompileISpecForm:  Unexpected lower limit type %d\n",
				pcArg2->pfForm->nType);
	}
	switch(pcArg3->pfForm->nType)
	{
		case ATOM_INTEGER:
			pi->dfUpper=(double)pcArg3->pfForm->uValue.nInteger;
			break;
		case ATOM_FLOAT:
			pi->dfUpper=pcArg3->pfForm->uValue.dfFloat;
			break;
		case ATOM_INFINITY:
			pi->bUpperInfinite=TRUE;
			break;
		default:
			ErrorMessage("CompileISpecForm:  Unexpected upper limit type %d\n",
				pcArg3->pfForm->nType);
	}
	if(StringEqQ(IdentName(pcArg4->pfForm),apsStringTab[STRING_RPAREN]))
		pi->bUpperOpen=TRUE;

	if(!pi->bLowerInfinite&&!pi->bUpperInfinite&&pi->dfLower>pi->dfUpper)
		ErrorMessage("CompileISpecForm:  Interval limits out of order: (not (< %s %s))\n",
			GetName(pcArg2->pfForm,ac),GetName(pcArg3->pfForm,ac));

	sprintf(acBuffer,"%s%s,%s%s",
		GetName(pcArg1->pfForm,ac),GetName(pcArg2->pfForm,ac),
		GetName(pcArg3->pfForm,ac),GetName(pcArg4->pfForm,ac));

	pc=(CELLP)MemAlloc(sizeof(CELL));
	pc->pfForm=pf=(FORMULAP)MemAlloc(sizeof(FORMULA));
	pf->nType=ATOM_ISPECP;
	pf->paAction=&aISpecAction;
	pf->psName=IdentAlloc(acBuffer);
	pf->uValue.piISpec=pi;
	EXIT("CompileISpecForm");
	return pc;
}

/* CompileVarGenForm

Description:
	Compile an ADL VARGEN formula.
Notes:
	A VARGEN formula provides a temporary means to keep variable/generator
	pairs associated.
	The VARGEN formulas will be discarded during ADL precondition optimization.
	pfVariables can be a list!
*/

static CELLP CompileVarGenForm
(
	CELLP pcVariables,					/* free variables */
	CELLP pcGenLit						/* generator formula (such-that clause) */
)
{
	CELLP pc;
	FORMULAP pf;

	ENTER("CompileVarGenForm",FALSE);
	pc=(CELLP)MemAlloc(sizeof(CELL));
	pc->pfForm=pf=(FORMULAP)MemAlloc(sizeof(FORMULA));
	pf->nType=ATOM_IDENT;
	pf->paAction=&aVarGenAction;
	pf->psName=IdentAlloc(pf->paAction->psName);

	pf->pcVars=pcVariables;
	pf->pcGenLit=pcGenLit;
	pcGenLit->pcNext=NULL;

	EXIT("CompileVarGenForm");
	return pc;
}

/* CompilePreForm

Description:
	Compile an ADL PRE formula.
Notes:
	Unlike the normal quantified formulas, we will optimize the ADL
	preconditions if there is more than one vargen.  We store the list
	containing all of the vargens as variables.
	The PRE formula will be discarded during ADL precondition optimization.
*/

static CELLP CompilePreForm
(
	CELLP pcVarGens,					/* vargen list */
	CELLP pcFormula 					/* precondition formula */
)
{
	CELLP pc;
	FORMULAP pf;

	ENTER("CompilePreForm",FALSE);
	pc=(CELLP)MemAlloc(sizeof(CELL));
	pc->pfForm=pf=(FORMULAP)MemAlloc(sizeof(FORMULA));
	pf->nType=ATOM_IDENT;
	pf->paAction=&aPreAction;
	pf->psName=IdentAlloc(pf->paAction->psName);

	pf->pcVars=pcVarGens;
	pf->pcArgs=pcFormula;
	if(pcFormula)
		pcFormula->pcNext=NULL;

	EXIT("CompilePreForm");
	return pc;
}

/* ZoneCopyFormulaList

Description:
	Copy of a formula list to the permanent zone.
*/

void ZoneCopyFormulaList
(
	CELLP pcList						/* input formula list */
)
{
	CELLP pc,pc1;
	FORMULAP pf;

	ENTER("ZoneCopyFormulaList",TRUE);

	for(pc=pcList;pc;pc=pc1)
	{
		pc1=pc->pcNext;					/* ZoneCopy clobbers pcNext field */
		ZoneCopy(pc);
		pf=pc->pfForm;
		ZoneCopy(pf);
		if(pf->psName)
			ZoneCopy(pf->psName);
		switch(pf->nType)
		{
			case ATOM_LISTP:
				if(pf->uValue.plList)
					ZoneCopyList(pf->uValue.plList);
				break;
			case ATOM_ISPECP:
				if(pf->uValue.piISpec)
					ZoneCopy(pf->uValue.piISpec);
				break;
		}
		if(pf->pcVars)
			ZoneCopyFormulaList(pf->pcVars);
		if(pf->pcGenLit)
			ZoneCopyFormulaList(pf->pcGenLit);
		if(pf->pcArgs)
			ZoneCopyFormulaList(pf->pcArgs);
	}
	EXIT("ZoneCopyFormulaList");
}

/* ZoneCopyFormula

Description:
	Copy a single a formula to the permanent zone.
	If the formula is actually a list of formulas, only the first element
	is copied.
*/

void ZoneCopyFormula
(
	CELLP pcFormula					/* input formula */
)
{
	FORMULAP pf;

	ENTER("ZoneCopyFormula",TRUE);
	if(!pcFormula)
	{
		EXIT("ZoneCopyFormula");
		return;
	}
	ZoneCopy(pcFormula);
	pf=pcFormula->pfForm;
	ZoneCopy(pf);
	if(pf->psName)
		ZoneCopy(pf->psName);
	switch(pf->nType)
	{
		case ATOM_LISTP:
			if(pf->uValue.plList)
				ZoneCopyList(pf->uValue.plList);
			break;
		case ATOM_ISPECP:
			if(pf->uValue.piISpec)
				ZoneCopy(pf->uValue.piISpec);
			break;
	}
	if(pf->pcVars)
		ZoneCopyFormulaList(pf->pcVars);
	if(pf->pcGenLit)
		ZoneCopyFormulaList(pf->pcGenLit);
	if(pf->pcArgs)
		ZoneCopyFormulaList(pf->pcArgs);
	EXIT("ZoneCopyFormula");
}

/* ZoneRelocFormulaList

Description:
	Relocate all of the pointers of a formula list.
*/

void ZoneRelocFormulaList
(
	CELLP pcList						/* input formula list */
)
{
	CELLP pc,pc1;
	FORMULAP pf;

	ENTER("ZoneRelocFormulaList",TRUE);

	for(pc=pcList;pc;pc=pc1)
	{
		pc1=pc->pcNext;					/* ZoneReloc clobbers pcNext */
		if(pc->pcNext)
			ZoneReloc((void **)&pc->pcNext);
		ZoneReloc((void **)&pc->pfForm);
		pf=pc->pfForm;
		if(pf->psName)
			ZoneReloc((void **)&pf->psName);
		switch(pf->nType)
		{
			case ATOM_LISTP:
				if(pf->uValue.plList)
				{
					ZoneReloc((void **)&pf->uValue.plList);
					ZoneRelocList(pf->uValue.plList);
				}
				break;
			case ATOM_ISPECP:
				if(pf->uValue.piISpec)
					ZoneReloc((void **)&pf->uValue.piISpec);
				break;
		}
		if(pf->pcVars)
		{
			ZoneReloc((void **)&pf->pcVars);
			ZoneRelocFormulaList(pf->pcVars);
		}
		if(pf->pcGenLit)
		{
			ZoneReloc((void **)&pf->pcGenLit);
			ZoneRelocFormulaList(pf->pcGenLit);
		}
		if(pf->pcArgs)
		{
			ZoneReloc((void **)&pf->pcArgs);
			ZoneRelocFormulaList(pf->pcArgs);
		}
	}
	EXIT("ZoneRelocFormulaList");
}

/* ZoneRelocFormula

Description:
	Relocate all the pointers of a single a formula.
	If the formula is actually a list of formulas, only the first element
	is relocated.
*/

void ZoneRelocFormula
(
	CELLP pcFormula					/* input formula */
)
{
	FORMULAP pf;

	ENTER("ZoneRelocFormula",TRUE);
	if(!pcFormula)
	{
		EXIT("ZoneRelocFormula");
		return;
	}
	if(pcFormula->pcNext)
		ZoneReloc((void **)&pcFormula->pcNext);
	ZoneReloc((void **)&pcFormula->pfForm);
	pf=pcFormula->pfForm;
	if(pf->psName)
		ZoneReloc((void **)&pf->psName);
	switch(pf->nType)
	{
		case ATOM_LISTP:
			if(pf->uValue.plList)
			{
				ZoneReloc((void **)&pf->uValue.plList);
				ZoneRelocList(pf->uValue.plList);
			}
			break;
		case ATOM_ISPECP:
			if(pf->uValue.piISpec)
				ZoneReloc((void **)&pf->uValue.piISpec);
			break;
	}
	if(pf->pcVars)
	{
		ZoneReloc((void **)&pf->pcVars);
		ZoneRelocFormulaList(pf->pcVars);
	}
	if(pf->pcGenLit)
	{
		ZoneReloc((void **)&pf->pcGenLit);
		ZoneRelocFormulaList(pf->pcGenLit);
	}
	if(pf->pcArgs)
	{
		ZoneReloc((void **)&pf->pcArgs);
		ZoneRelocFormulaList(pf->pcArgs);
	}
	EXIT("ZoneRelocFormula");
}
