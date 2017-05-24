/* eval.h */

#ifndef __EVAL_H
#define __EVAL_H

#define STDOUT 0
#define STDIN 1
#define STDERR 2
#define STDLOG 3
#define MINFILE 3
#define MAXFILE 32

typedef struct FileTable
{
	FILE *pfFile;						/* file handle */
	BOOL bStdio;						/* stdio flag */
	char cStatus;						/* open status */
}FILETABLE, *FILETABLEP;

/* global data */

extern DECLSPEC char *psPlanName;		/* current problem name */
extern DECLSPEC BOOL bImmediateExit;	/* immediate exit flag */

extern FILETABLE aftFileTable[MAXFILE];	/* file handle table */

extern int nSearchDepthLimit;			/* search depth limit */
extern double dfSearchHeuristicLimit;	/* search heuristic limit */

/* global function prototypes -------------------------------------------------- */

/* formula predicates */

BOOL TrueFormQ
(
	CELLP pcFormula
);
BOOL FalseFormQ
(
	CELLP pcFormula
);
BOOL AndFormQ
(
	CELLP pcFormula					/* formula */
);
BOOL OrFormQ
(
	CELLP pcFormula
);
BOOL XorFormQ
(
	CELLP pcFormula
);
BOOL NotFormQ
(
	CELLP pcFormula
);
BOOL ImpliesFormQ
(
	CELLP pcFormula
);
BOOL GoalFormQ
(
	CELLP pcFormula
);
BOOL PreviousFormQ
(
	CELLP pcFormula
);
BOOL EqFormQ
(
	CELLP pcForm						/* formula */
);
BOOL PlusEqFormQ
(
	CELLP pcForm						/* formula */
);
BOOL MinusEqFormQ
(
	CELLP pcForm						/* formula */
);
BOOL AtomicFormQ
(
	CELLP pcFormula
);
BOOL FnLitQ
(
	CELLP pcLit						/* literal */
);
BOOL AddFormQ
(
	CELLP pcFormula					/* formula */
);
BOOL DelFormQ
(
	CELLP pcFormula					/* formula */
);
BOOL ForAllFormQ
(
	CELLP pcFormula
);
BOOL ExistsFormQ
(
	CELLP pcFormula
);
BOOL ExistsXFormQ
(
	CELLP pcFormula
);
BOOL BindingFormQ
(
	CELLP pcFormula
);
BOOL EvalExit
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);

/* Formula evaluators ---------------------------------------------------------- */

BOOL EvalEq
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalDefPredicate
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalAnd
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalAndRelaxed_H
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalAndRelaxed_FF
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalOr
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalXor
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalImplies
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalIfThenElse
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalIfThenElse_RESET
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);

BOOL EvalNot
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalNotRelaxed
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalNotRelaxedNeg
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalTrue
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalFalse
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalForAll
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalExistsX
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalExists
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalForAllNoGenerator
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalExistsXNoGenerator
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalExistsNoGenerator
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalForAll_H
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalForAll_FF
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalForAll_HNoReset
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalExistsX_H
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalExists_H
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalGoal
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalCurrent
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalPrevious
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalAssign
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalAssignAppend
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalSearchGlobalInitialization
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalAtomic
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalPrint
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalPlan
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalSetPlanName
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalResetPlanName
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalPrintWorld
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalPrintWorldList
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);

/* bindings -------------------------------------------------------------------- */

DECLSPEC BINDINGP ExtendBindings
(
	CELLP pcVars,					/* new variables */
	CELLP pcVals,					/* new values */
	BINDINGP pbBindings					/* old bindings */
);
DECLSPEC CELLP LookupVar
(
	CELLP pbVar,
	BINDINGP pbBindings
);
CELLP GetBindingFormula
(
	CELLP pcBindingForm
);
CELLP GetBindingVars
(
	CELLP pcBindingForm
);
CELLP GetBindingVals
(
	CELLP pcBindingForm
);
DECLSPEC void SetVarX
(
	CELLP pcVar,
	CELLP pcVal,
	BINDINGP pbBindings
);
DECLSPEC void AppendVarX
(
	CELLP pcVar,						/* variable to append to */
	CELLP pcVals,						/* list of values */
	BINDINGP pbBindings
);
DECLSPEC void SetArrayX
(
	CELLP pcArray,
	CELLP pcVal,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);

/* Miscellaneous --------------------------------------------------------------- */

CELLP GetDeltaFormula
(
	CELLP pcDeltaForm
);
CELLP GetBFISpec
(
	CELLP pcBFFormula
);
CELLP GetBFFormulas
(
	CELLP pcBFFormula
);
char *GetFnName
(
	CELLP pcLit						/* literal formula */
);
CELLP GetFnLitGen
(
	CELLP pcGenLit					/* literal generator */
);

//-----------------

BOOL EvalSetSearchLimit
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalResetSearchLimit
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalSetSearchStrategy
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalSetMergeFnType
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalResetSearchStrategy
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalSetControl
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalEnable
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalDisable
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalSetPriorityFn
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalResetPriorityFn
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalSetHeuristicFn
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalSetMetricFn
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalSetHeuristicPreservedProperty
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalSetQualpreferenceValueFn
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalSetQualpreferenceNumber
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalSetCountSatPrefsFn
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalSetTotalPrefsFn
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalSetOptimisticMetricFn
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalSetMetricMultiplier
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalSetBestMetricMultiplier
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalSetDiscountedMetricMultiplier
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalSetMetricDiscount
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalSetDiscountedMetricMultiplier
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalSetMetricPriority
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalSetDiscountedMetricPriority
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalSetPreferenceDistancePriority
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalSetOptimisticMetricPriority
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalSetBestRelaxedMetricPriority
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalSetHelpfulListPriority
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalSetHeuristicExponent
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalSetOptimisticMetricMultiplier
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalSetGoalMultiplier
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalSetPlanTimeout
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalSetPreferenceMultiplier
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalSetGoalOcclusionMultiplier
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalSetPrecondOcclusionMultiplier
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalResetHeuristicFn
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalSetGoalAddendum
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalResetGoalAddendum
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalSetGoalType
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalSetGoal
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalSetPreferences
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalSetGoalSequence
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalSetGoalFormula
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalSetInitialWorld
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalSetInitialFacts
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalSetInitializationSequence
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalSetPostActionSequence
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalSetBDFPreference
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalLoadPlan
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalSetTLControl
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalResetTLControl
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalResetDomains
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalDefDomain
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalListDomains
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalLoadDomain
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalSetPrintWorldFn
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalResetPrintWorldFn
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalVerboseOn
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalVerboseOff
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalFFRelPlanSimpOn
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalFFRelPlanSimpOff
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalVerboseShowExpandedPlansOn
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalVerboseShowExpandedPlansOff
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalVerboseShowExpandedWorldsOn
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalVerboseShowExpandedWorldsOff
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalVerboseFFShowRelaxedWorldsOn
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalVerboseFFShowRelaxedWorldsOff
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalVerboseFFShowRelaxedPlansOn
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalVerboseFFShowRelaxedPlansOff
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalSetTraceLevel
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalDefine
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalLoadFile
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalLoadPddlProblem
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalPrintPddlPlan
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalPrintPddl3Plan
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalSetStatisticsFile
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalClearWorldSymbols
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalClearOperators
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalClearEventQueue
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalDeclareDescribedSymbols
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalDeclareDefinedSymbols
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalDeclareInternalSymbols
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalDeclareGlobalVariables
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalDeclareMacroOperators
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalDeclareElidedOperators
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalDefDefinedPredicate
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalDefDefinedFunction
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalDefDefinedGenerator
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalDefDefinedMacro
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalDeclareExternalSymbols
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalPrintPlanList
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalSelectInitialWorld
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalSelectFinalWorld
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalSelectNextWorld
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalSelectPreviousWorld
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);

BOOL EvalCloseFile
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalRedirect
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
void CloseAllFiles(void);
void InitFileHandles(void);
DECLSPEC FILE *HandleToFileStream
(
	int nFile							/* file handle */
);
BOOL EvalSetRNG
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
CELLP ComputeGetRNG
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalResetRNG
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalSetSearchDepthLimit
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalResetSearchDepthLimit
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalSetSearchHeuristicLimit
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalResetSearchHeuristicLimit
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalPrintDeltaTime
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL PrintPlanList
(
	LINEARPLANP plpLinearPlan,
	FILE *pfFile
);
BINDINGP CopyBindingsShared
(
	BINDINGP pbShare,					/* bindings to share */
	BINDINGP pbCopy						/* bindings to copy */
);

#endif /* __EVAL_H */
