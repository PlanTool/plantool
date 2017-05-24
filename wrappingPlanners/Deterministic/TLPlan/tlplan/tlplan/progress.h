/* progress.h */

#ifndef __PROGRESS_H
#define __PROGRESS_H

/* global function prototypes */

CELLP ProgressNext
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings,
	BOOLP pImmutable
);
CELLP ProgressEventually
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings,
	BOOLP pImmutable
);
CELLP ProgressAlways
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings,
	BOOLP pImmutable
);
CELLP ProgressAnd
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings,
	BOOLP pImmutable
);
CELLP ProgressOr
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings,
	BOOLP pImmutable
);
CELLP ProgressXor
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings,
	BOOLP pImmutable
);
CELLP ProgressImplies
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings,
	BOOLP pImmutable
);
CELLP ProgressIfThenElse
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings,
	BOOLP pImmutable
);
CELLP ProgressNot
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings,
	BOOLP pImmutable
);
CELLP ProgressTrue
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings,
	BOOLP pImmutable
);
CELLP ProgressFalse
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings,
	BOOLP pImmutable
);
CELLP ProgressTUntil
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings,
	BOOLP pImmutable
);
CELLP ProgressTEventually
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings,
	BOOLP pImmutable
);
CELLP ProgressTAlways
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings,
	BOOLP pImmutable
);
CELLP ProgressDelta
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings,
	BOOLP pImmutable
);
CELLP ProgressForAll
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings,
	BOOLP pImmutable
);
CELLP ProgressExists
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings,
	BOOLP pImmutable
);
CELLP ProgressExistsX
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings,
	BOOLP pImmutable
);
CELLP ProgressBinding
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings,
	BOOLP pImmutable
);
CELLP ProgressGoal
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings,
	BOOLP pImmutable
);
CELLP ProgressCurrent
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings,
	BOOLP pImmutable
);
CELLP ProgressPrevious
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings,
	BOOLP pImmutable
);
CELLP ProgressUntil
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP Bindings,
	BOOLP pImmutable
);
CELLP ProgressAtomic
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings, 
	BOOLP pImmutable
);

#endif /* __PROGRESS_H */
