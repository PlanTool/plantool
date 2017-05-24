/* var.h */

/* global function prototypes */

CELLP VariablesIn
(
	CELLP pcList						/* list of formulas */
);
BOOL VarQ
(
	CELLP pcVar
);
BOOL VariableFreeQ
(
	CELLP pcFormula
);

