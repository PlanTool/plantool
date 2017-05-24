/* interval.h */

#ifndef __INTERVAL_H
#define __INTERVAL_H

/* global structures and definitions */

/* global function prototypes */

CELLP LShiftForm
(
	CELLP pcFormula,					/* ispec formula to shift */
	double dfDelta						/* time to shift */
);
//ISPECP ISpecLShift
//(
//	ISPECP piISpec,
//	double dfDelta
//);
BOOL ISpecLt0
(
	ISPECP piIspec
);
BOOL ISpec0In
(
	ISPECP piISpec
);

#endif /* __INTERVAL_H */
