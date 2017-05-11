/* Compute.h -- compute function support */

/* global data */

//extern int nAELength;					/* array element length */

/* global function prototypes */

DECLSPEC CELLP ComputeTerms
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
/* FAHIEM DEC 27th 2000. A no copy version of compute terms
   This version must be passed a place to store its computed values.*/
DECLSPEC BOOL ComputeTermsNoCopy
(
	CELLP pcFormula,
	CELLP pcNewVals,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
DECLSPEC CELLP ComputeTerm
(
	CELLP pcTerm,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
CELLP ComputeDefFunction
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
CELLP ComputeDefMacro
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
CELLP ComputeDescMacro
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
CELLP ComputeLiteral
(
	CELLP pcLiteral,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
CELLP ComputeVariable
(
	CELLP pcVariable,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
CELLP ComputeArray
(
	CELLP pcArray,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL ArrayIndex
(
	CELLP pcArray,						/* array reference */
	CELLP pcBinding,					/* array binding */
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings,
	int *nOffset						/* returned offset */
);
CELLP ComputeGoal
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
CELLP ComputeCurrent
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
CELLP ComputePrevious
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
CELLP ComputeOptimalCost
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
CELLP ComputeBestAction
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
CELLP ComputeAStar
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
CELLP ComputePlanLength
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
CELLP ComputePlanCost
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
CELLP ComputePlanDuration
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
CELLP ComputeActionName
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
CELLP ComputeActionCost
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
CELLP ComputeActionDuration
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
CELLP ComputeActionPriority
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
CELLP ComputeWorldNumber
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
CELLP ComputeWorldCounter
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
CELLP ComputeClosedlistLength
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
CELLP ComputeWorldHeuristicRank
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
CELLP ComputeGetPlanName
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
CELLP ComputePlanStatus
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
CELLP ComputeWorldsGenerated
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
CELLP ComputeWorldsSearched
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
CELLP ComputeWorldsPruned
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
CELLP ComputeWorldsDiscarded
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
CELLP ComputeWorldsUnexamined
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
CELLP ComputePlanCPUTime
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
CELLP ComputeGetCPUTime
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
CELLP ComputeGetBestRelaxedMetric
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
CELLP ComputeGetNumberWorldFixedPoints
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
CELLP ComputeGetNumberRealFixedPoints
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
CELLP ComputeGetBestPreferenceValue
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
CELLP ComputeGetSearchDepthLimit
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
CELLP ComputeGetSearchHeuristicLimit
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
CELLP ComputeSearchMaxHeuristic
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
CELLP ComputeSearchMaxDepth
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);




CELLP ExpandArrays
(
	CELLP pcVars,						/* local variable list */
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
CELLP MakeArray
(
	CELLP pcFormula,					/* formula to convert */
	CELLP pcValue,						/* atomic initialization value */
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);

//-----------------------------

CELLP ComputeGetSearchLimit
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
CELLP ComputeGetSearchStrategy
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
CELLP ComputeGetPriorityFn
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
CELLP ComputeGetHeuristicFn
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
CELLP ComputeHeuristicFn
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
CELLP ComputeMetricFn
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
CELLP ComputeGetTLControl
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
CELLP ComputeGetGoalAddendum
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
CELLP ComputeGetTraceLevel
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
CELLP ComputeOpenFile
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
CELLP ComputePrintString
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
CELLP ComputeMakeLiteral
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);

