/* make.h */

/* global data */

extern CELLP pcTrue;
extern CELLP pcFalse;

/* global function prototypes */

CELLP MakeTrueForm(void);
void InitTrueForm(void);
CELLP MakeFalseForm(void);
void InitFalseForm(void);

CELLP MakeNotForm
(
	BOOL bCopy,							/* copy arguments */
	CELLP pcArgs
);
DECLSPEC CELLP MakeAndForm
(
	BOOL bCopy,							/* copy arguments */
	CELLP pcArgs
);
CELLP MakeOrForm
(
	BOOL bCopy,							/* copy arguments */
	CELLP pcArgs
);
CELLP MakeXorForm
(
	BOOL bCopy,							/* copy arguments */
	CELLP pcArgs
);
CELLP MakeImpliesForm
(
	BOOL bCopy,							/* copy arguments */
	CELLP pcArg1,
	CELLP pcArg2
);
CELLP MakeIfThenElseForm
(
	BOOL bCopy,							/* copy arguments */
	CELLP pcArg1,
	CELLP pcArg2,
	CELLP pcArg3
);
CELLP MakeInTheSetForm
(
	BOOL bCopy,							/* copy arguments */
	CELLP pcVars,
	CELLP pcVals
);
CELLP MakeForAllForm
(
	BOOL bCopy,							/* copy arguments */
	CELLP pcVariables,
	CELLP pcGenerator,
	CELLP pcFormula
);
CELLP MakeExistsForm
(
	BOOL bCopy,							/* copy arguments */
	CELLP pcVariables,
	CELLP pcGenerator,
	CELLP pcFormula
);
CELLP MakeExistsXForm
(
	BOOL bCopy,							/* copy arguments */
	CELLP pcVariables,
	CELLP pcGenerator,
	CELLP pcFormula
);
CELLP MakeBindingForm
(
	BOOL bCopy,							/* copy arguments */
	CELLP pcVars,
	CELLP pcVals,
	CELLP pcFormula
);
CELLP MakeDeltaForm
(
	BOOL bCopy,							/* copy arguments */
	CELLP pcArgs
);

DECLSPEC CELLP MakeAddForm
(
	BOOL bCopy,							/* copy arguments */
	CELLP pcArgs
);

DECLSPEC CELLP MakeIntegerForm
(
	int nValue
);
DECLSPEC CELLP MakeFloatForm
(
	double dfValue
);
DECLSPEC CELLP MakeStringForm
(
	char *psValue						/* string value */
);
DECLSPEC CELLP MakeLiteralForm
(
	int nType,							/* atomic type */
	LITVAL lValue						/* literal value */
);

DECLSPEC CELLP MakeSymbolInfoForm
(
	BOOL bCopy,							/* copy arguments */
	LISTP plName,						/* name */
	CELLP pcArgs						/* list of arguments */
);

CELLP MakeSearchGlobalInitializationForm
(
	BOOL bCopy,							/* copy arguments */
	CELLP pcArg1,
	CELLP pcArg2
);
CELLP MakeOptimalCostForm(void);
CELLP MakeBestActionForm(void);

/* routines called from user libraries -------------------------------------- */

DECLSPEC CELLP MakeEqForm
(
	BOOL bCopy,							/* copy arguments */
	CELLP pcArg1,
	CELLP pcArg2
);
DECLSPEC CELLP MakeGlobalDelayedActionForm
(
	BOOL bCopy,							/* copy arguments */
	CELLP pcArg1,						/* delay */
	CELLP pcArg2,						/* tag formula */
	CELLP pcArg3						/* action formula */
);
DECLSPEC CELLP MakeSetInitializationSequenceForm
(
	BOOL bCopy,							/* copy arguments */
	CELLP pcArgs
);
DECLSPEC CELLP MakeSetInitialWorldForm
(
	BOOL bCopy,							/* copy arguments */
	CELLP pcArgs
);
