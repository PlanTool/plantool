/* formula.h */

#ifndef __FORMULA_H
#define __FORMULA_H

/* global data */

extern ACTION aTrueAction;
extern ACTION aFalseAction;
extern ACTION aNotAction;
extern ACTION aUNotAction;
extern ACTION aAndAction;
extern ACTION aOrAction;
extern ACTION aXorAction;
extern ACTION aImpliesAction;
extern ACTION aIfThenElseAction;
extern ACTION aEqAction;
extern ACTION aForAllAction;
extern ACTION aForAllActionNoReset;
extern ACTION aExistsAction;
extern ACTION aExistsXAction;
extern ACTION aPreAction;
extern ACTION aVarGenAction;
extern ACTION aAddAction;
extern ACTION aDelAction;
extern ACTION aSDelAction;
extern ACTION aPrintAction;
extern ACTION aPlanNameAction;
extern ACTION aArrayAction;
extern ACTION aAlwaysAction;
extern ACTION aEventuallyAction;
extern ACTION aNextAction;
extern ACTION aUntilAction;
extern ACTION aTAlwaysAction;
extern ACTION aTEventuallyAction;
extern ACTION aTUntilAction;
extern ACTION aGoalAction;
extern ACTION aPreviousAction;
extern ACTION aPermuteAction;
extern ACTION aBindingAction;
extern ACTION aDeltaAction;
extern ACTION aAssignAction;
extern ACTION aSearchGlobalInitializationAction;
extern ACTION aAStarAction;
extern ACTION aOptimalCostAction;
extern ACTION aBestActionAction;
extern ACTION aPrintWorldAction;
extern ACTION aPrintWorldListAction;
extern ACTION aSetPlanNameAction;
extern ACTION aSetPlanTimeoutAction;
extern ACTION aResetPlanNameAction;
extern ACTION aGetPlanNameAction;
extern ACTION aAssignAppendAction;
extern ACTION aCurrentAction;
extern ACTION aPlanDurationAction;
extern ACTION aClearWorldSymbolsAction;
extern ACTION aClearOperatorsAction;
extern ACTION aClearEventQueueAction;
extern ACTION aReachableEventAction;
extern ACTION aDeclareGlobalVariablesAction;
extern ACTION aDeclareMacroOperatorsAction;
extern ACTION aDefADLOperatorAction;
extern ACTION aDefADLHOperatorAction;
extern ACTION aDefDefinedFunctionAction;
extern ACTION aDefDefinedGeneratorAction;
extern ACTION aDefDefinedMacroAction;
extern ACTION aDefDefinedPredicateAction;
extern ACTION aDefStripsOperatorAction;
extern ACTION aHeuristicFnAction;
extern ACTION aGetTLControlAction;
extern ACTION aGetGoalAddendumAction;
extern ACTION aLoadPlanAction;
extern ACTION aPrintPlanListAction;
extern ACTION aResetSearchStrategyAction;
extern ACTION aResetTLControlAction;
extern ACTION aEnableAction;
extern ACTION aDisableAction;
extern ACTION aSetGoalAddendumAction;
extern ACTION aResetGoalAddendumAction;
extern ACTION aSetInitialFactsAction;
extern ACTION aSetTLControlAction;
extern ACTION aLtAction;
extern ACTION aLeAction;
extern ACTION aGtAction;
extern ACTION aGeAction;
extern ACTION aSeedAction;
extern ACTION aRandAction;
extern ACTION aRandomAction;
extern ACTION aExpAction;
extern ACTION aLogAction;
extern ACTION aRoundAction;
extern ACTION aIntAction;
extern ACTION aFloorAction;
extern ACTION aCeilAction;
extern ACTION aPlusAction;
extern ACTION aMinusAction;
extern ACTION aMultiplyAction;
extern ACTION aDivideAction;
extern ACTION aModAction;
extern ACTION aMaxAction;
extern ACTION aMinAction;
extern ACTION aExptAction;
extern ACTION aSqrtAction;
extern ACTION aAbsAction;
extern ACTION aPosIntAction;
extern ACTION aLtPosIntAction;
extern ACTION aIsBetweenAction;
extern ACTION aSelectInitialWorldAction;
extern ACTION aSelectFinalWorldAction;
extern ACTION aSelectNextWorldAction;
extern ACTION aSelectPreviousWorldAction;
extern ACTION aCloseFileAction;
extern ACTION aRedirectAction;
extern ACTION aOpenFileAction;
extern ACTION aPrintStringAction;
extern ACTION aSetRNGAction;
extern ACTION aResetRNGAction;
extern ACTION aGetRNGAction;
extern ACTION aPlanStatusAction;
extern ACTION aWorldsGeneratedAction;
extern ACTION aWorldsSearchedAction;
extern ACTION aWorldsPrunedAction;
extern ACTION aWorldsDiscardedAction;
extern ACTION aWorldsUnexaminedAction;
extern ACTION aPlanCPUTimeAction;
extern ACTION aGetCPUTimeAction;
extern ACTION aGetBestRelaxedMetricAction;
extern ACTION aGetBestPreferenceValueAction;
extern ACTION aGetNoWorldFixedPointsAction;
extern ACTION aGetNoRealFixedPointsAction;
extern ACTION aSetSearchDepthLimitAction;
extern ACTION aResetSearchDepthLimitAction;
extern ACTION aGetSearchDepthLimitAction;
extern ACTION aSearchMaxDepthAction;
extern ACTION aSetSearchHeuristicLimitAction;
extern ACTION aResetSearchHeuristicLimitAction;
extern ACTION aGetSearchHeuristicLimitAction;
extern ACTION aSearchMaxHeuristicAction;
extern ACTION aWaitForNextEventAction;
extern ACTION aReachableEventAction;
extern ACTION aPlanLengthAction;
extern ACTION aPlanCostAction;
extern ACTION aActionPriorityAction;
extern ACTION aWorldNumberAction;
extern ACTION aWorldCounterAction;
extern ACTION aClosedlistLengthAction;
extern ACTION aWorldHeuristicRankAction;
extern ACTION aLiteralAction;
extern ACTION aIdentAction;
extern ACTION aFloatAction;
extern ACTION aIntegerAction;
extern ACTION aStringAction;
extern ACTION aISpecAction;
extern ACTION aModifyWorldAction;
extern ACTION aOperatorAction;
extern ACTION aDefPredicateAction;
extern ACTION aDefGeneratorAction;
extern ACTION aDefFunctionAction;
extern ACTION aDefMacroAction;
extern ACTION aExitAction;
extern ACTION aDummyAction;
extern ACTION aDeclareDescribedSymbolsAction;
extern ACTION aDeclareDefinedSymbolsAction;
extern ACTION aDeclareExternalSymbolsAction;
extern ACTION aDeclareInternalSymbolsAction;
extern ACTION aDefDomainAction;
extern ACTION aSetGoalAction;
extern ACTION aSetPreferencesAction;
extern ACTION aDefineAction;
extern ACTION aSetSearchLimitAction;
extern ACTION aSetGoalTypeAction;
extern ACTION aSetControlAction;
extern ACTION aLoadDomainAction;
extern ACTION aLoadFileAction;
extern ACTION aLoadPddlProblemAction;
extern ACTION aSetStatisticsFileAction;
extern ACTION aSetHeuristicsFnAction;
extern ACTION aSetPriorityFnAction;
extern ACTION aSetTraceLevelAction;
extern ACTION aSetSearchStrategyAction;
extern ACTION aSetMergeFnTypeAction;
extern ACTION aSetPrintWorldFnAction;
extern ACTION aResetPrintWorldFnAction;
extern ACTION aPlanAction;
extern ACTION aVerboseOnAction;
extern ACTION aVerboseOffAction;
extern ACTION aGetTraceLevelAction;
extern ACTION aGetSearchLimitAction;
extern ACTION aGetHeuristicFnAction;
extern ACTION aGetPriorityFnAction;
extern ACTION aGetSearchStrategyAction;
extern ACTION aResetSearchLimitAction;
extern ACTION aResetDomainsAction;
extern ACTION aListDomainsAction;
extern ACTION aResetHeuristicFnAction;
extern ACTION aResetPriorityFnAction;
extern ACTION aActionNameAction;
extern ACTION aActionCostAction;
extern ACTION aActionDurationAction;
extern ACTION aSetHeuristicFnAction;
extern ACTION aSetMetricFnAction;
extern ACTION aSetHeuristicPreservedPropertyAction;
extern ACTION aSetQualpreferenceValueFnAction;
extern ACTION aSetQualpreferenceNumberAction;
extern ACTION aSetCountSatPrefsFnAction;
extern ACTION aSetOptimisticMetricFnAction;
extern ACTION aSetPreferenceMultiplierAction;
extern ACTION aSetGoalOcclusionMultiplierAction;
extern ACTION aSetPrecondOcclusionMultiplierAction;
extern ACTION aSetMetricMultiplierAction;
extern ACTION aSetDiscountedMetricMultiplierAction;
extern ACTION aSetBestMetricMultiplierAction;
extern ACTION aSetOptimisticMetricMultiplierAction;
extern ACTION aSetGoalMultiplierAction;
extern ACTION aSetHeuristicExponentAction;
extern ACTION aSetMetricDiscountAction;
extern ACTION aInTheSetAction;
extern ACTION aDelayedActionAction;
extern ACTION aGlobalDelayedActionAction;
extern ACTION aSetInitializationSequenceAction;
extern ACTION aSetInitialWorldAction;
extern ACTION aMakeLiteralAction;
extern ACTION aSetGoalSequenceAction;
extern ACTION aSetGoalFormulaAction;
extern ACTION aCurrentTimeAction;
extern ACTION aInhibitDelayedActionAction;
extern ACTION aPlusEqAction;
extern ACTION aMinusEqAction;
extern ACTION aPrintDeltaTimeAction;
extern ACTION aNearestFirstAction;
extern ACTION aNearestFirstExAction;
extern ACTION aClosestFirstAction;
extern ACTION aClosestFirstExAction;
extern ACTION aLowestFirstAction;
extern ACTION aAllPairsShortestPathAction;
extern ACTION aPrintPddlPlanAction;
extern ACTION aPrintPddl3PlanAction;
extern ACTION aDeclareElidedOperatorsAction;
extern ACTION aUpdateWorldAction;
extern ACTION aConditionalExpAction;
extern ACTION aGoalDistanceAction;
extern ACTION aGoalDistanceHspUnreleffsAction;
extern ACTION aGoalDistanceFFUnreleffsAction;
extern ACTION aGoalDistanceQualpreferencesAction;
extern ACTION aGoalDistanceNegAction;
extern ACTION aSetBDFPreferenceAction;
extern ACTION aVerboseShowExpandedPlansOffAction;
extern ACTION aVerboseShowExpandedPlansOnAction;
extern ACTION aVerboseShowExpandedWorldsOffAction;
extern ACTION aVerboseShowExpandedWorldsOnAction;
extern ACTION aVerboseFFShowRelaxedPlansOffAction;
extern ACTION aVerboseFFShowRelaxedPlansOnAction;
extern ACTION aVerboseFFShowRelaxedWorldsOffAction;
extern ACTION aVerboseFFShowRelaxedWorldsOnAction;
extern ACTION aSetHelpfulListPriority;
extern ACTION aSetDiscountedMetricPriorityAction;
extern ACTION aSetOptimisticMetricPriorityAction;
extern ACTION aSetBestRelaxedMetricPriorityAction;
extern ACTION aSetPreferenceDistancePriorityAction;
extern ACTION aSetMetricPriorityAction;
extern ACTION aSetMetricMultiplierAction;
extern ACTION aGoalDistanceFFPDDLPrefs;
extern ACTION aSetTotalPrefsFnAction;



/* global function prototypes */

CELLP ListToFormula
(
	LISTP plList
);
DECLSPEC CELLP CopyCellList
(
	CELLP pcList						/* input cell list */
);
DECLSPEC CELLP CopyCellListReturnEndCell
(
	CELLP pcList,						/* input cell list */
	CELLP *ppcLastCellofCopy            /* return point ot last cell of copy */
);
DECLSPEC CELLP CopyPrefixCellList
(
	CELLP pcList,						/* input cell list */
	CELLP pcFirstPastPrefix,            /* First cell past prefix */
	CELLP *ppcLastCellofCopy            /* return point ot last cell of copy */
);
DECLSPEC CELLP CopyCell
(
	CELLP pcCell						/* input cell */
);
DECLSPEC CELLP CopyCell2
(
	CELLP pc,							/* cell to overwrite */
	CELLP pcCell						/* input cell */
);
DECLSPEC CELLP CopyFormulaList
(
	CELLP pcFormula						/* input formula */
);
DECLSPEC CELLP CopyFormula
(
	CELLP pcFormula						/* input formula */
);
void MarkFormulaList
(
	CELLP pcFormula						/* input formula */
);
void MarkFormula
(
	CELLP pcFormula						/* input formula */
);	
BOOL FormulaEqQ
(
	CELLP pcFormula1,
	CELLP pcFormula2
);
BOOL FormulaListEqQ
(
	CELLP pcFormula1,
	CELLP pcFormula2
);
CELLP FormulaDifference
(
	CELLP pcFormula1,					/* input formula (minuend) */
	CELLP pcFormula2					/* input formula (subtrahend) */
);
CELLP FormulaIntersection
(
	CELLP pcList1,						/* input list */
	CELLP pcList2						/* input list */
);
BOOL FormulaIntersects
(
	CELLP pcList1,						/* input list */
	CELLP pcList2						/* input list */
);
CELLP FormulaUnion
(
	CELLP pcList1,						/* input list */
	CELLP pcList2						/* input list */
);
CELLP FormulaMemQ
(
	CELLP pcSubFormula,					/* search key */
	CELLP pcFormula						/* search domain */
);
CELLP AppendFormula
(
	BOOL bCopy,							/* copy prefix list */
	CELLP pcFirst,						/* prefix list */
	CELLP pcSecond						/* suffix list */
);
CELLP ReverseFormulaList
(
	CELLP pcList
);
DECLSPEC void PrintFormula
(
	FILE *pfStream,						/* output file stream */
	CELLP pcFormula,					/* formula to display */
	int nLevel							/* recursion level (initialize to 0) */
);
DECLSPEC void PrintFlatFormula
(
	FILE *pfStream,						/* output file stream */
	CELLP pcFormula						/* formula to display */
);
DECLSPEC void PrintFormulaList
(
	FILE *pfStream,						/* output file stream */
	CELLP pcFormula						/* (list of) formulas to display */
);
CELLP ListToFormula
(
	LISTP plList						/* list to convert */
);

DECLSPEC CELLP CompileArgForm
(
	ACTIONP paAction,					/* action pointer */
	CELLP pcArgs
);
DECLSPEC CELLP CompileDummyForm
(
	char *psName,						/* name of leading token */
	CELLP pcArgs
);
DECLSPEC BOOL FormulaToInteger
(
	CELLP pcTerm,						/* term to evaluate */
	LINEARPLANP plpLinearPlan,			/* current plan */
	BINDINGP pbBindings,				/* current bindings */
	int *pnValue						/* result */
);
DECLSPEC BOOL FormulaToDouble
(
	CELLP pcTerm,						/* term to evaluate */
	LINEARPLANP plpLinearPlan,			/* current plan */
	BINDINGP pbBindings,				/* current bindings */
	double *pdfValue					/* result */
);
DECLSPEC BOOL FormulaToString
(
	CELLP pcTerm,						/* term to evaluate */
	LINEARPLANP plpLinearPlan,			/* current plan */
	BINDINGP pbBindings,				/* current bindings */
	char **psValue						/* result */
);
DECLSPEC void RefPrintFormula
(
	FILE *pfStream,						/* output file stream */
	CELLP pcFormula,					/* formula to display */
	int nLevel							/* recursion level (initialize to 0) */
);
DECLSPEC char *SaveName
(
	FORMULAP pf 
);
DECLSPEC char *MakeName
(
	FORMULAP pf,
	char acBuffer[40]
);
int PrintTerm
(
	FILE *pfStream,						/* output file stream */
	CELLP pcFormula,					/* term to display */
	int nLevel,							/* recursion level (initialize to 0) */
	int nCol							/* current column */
);
void ZoneCopyFormulaList
(
	CELLP pcList						/* input formula list */
);
void ZoneCopyFormula
(
	CELLP pcFormula						/* input formula */
);
void ZoneRelocFormulaList
(
	CELLP pcList						/* input formula list */
);
void ZoneRelocFormula
(
	CELLP pcFormula						/* input formula */
);
void FormulaSizeOf
(
	CELLP pcFormula,					/* input formula */
	int *pnSize
);
void FormulaListSizeOf
(
	CELLP pcFormula,					/* input formula */
	int *pnSize
);
void InitActionName
(
	ACTIONP pa 
);

#endif /* __FORMULA_H */
