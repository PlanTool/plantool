// color.h

DECLSPEC void PColorFormula
(
	FILE *pfStream,						/* output file stream */
	CELLP pcFormula,					/* formula to display */
	LINEARPLANP plpLinearPlan,			/* linear plan */
	BINDINGP pbBindings,				/* bindings list */
	int nLevel							/* recursion level (initialize to 0) */
);
DECLSPEC void PColorFormulaList
(
	FILE *pfStream,						/* output file stream */
	CELLP pcFormula,					/* (list of) formulas to display */
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);

DECLSPEC void EColorFormula
(
	FILE *pfStream,						/* output file stream */
	CELLP pcFormula,					/* formula to display */
	LINEARPLANP plpLinearPlan,			/* linear plan */
	BINDINGP pbBindings,				/* bindings list */
	int nLevel							/* recursion level (initialize to 0) */
);
DECLSPEC void EColorFormulaList
(
	FILE *pfStream,						/* output file stream */
	CELLP pcFormula,					/* (list of) formulas to display */
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);

