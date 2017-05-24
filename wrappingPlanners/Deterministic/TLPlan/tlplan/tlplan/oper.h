/* oper.h */

#ifndef __OPER_H
#define __OPER_H

/* global data */

extern OPERATORP poOperators;			/* list of domain's operators */
extern MACROOPERATORP pmoMacroOperators;	/* list of domain's macro operators */
extern MACROOPERATORP pmoMacroOperEnd;	/* pointer to end of macro operator list */
extern ELIDEDOPERATORP peoElidedOperators;	/* list of domain's elided operators */
extern ELIDEDOPERATORP peoElidedOperEnd;	/* pointer to end of elided operator list */

/* global function prototypes */

void ClearOps(void);
OPERATORP CopyOperator
(
	OPERATORP po
);
void MarkOperator
(
	OPERATORP po
);
OPERATORP MakeOperator
(
	CELLP pcName,						/* operator name */
	CELLP pcUpdate,						/* update formula */
	CELLP pcDuration,					/* duration */
	CELLP pcCost,						/* cost */
	CELLP pcPriority					/* priority */
);
MACROOPERATORP AddMacroOperator
(
	char *psName,						/* operator name */
	char *psDomain,						/* domain file path */
	CELLP pcGoal						/* macro goal formula */
);
void MarkMacroOperator
(
	MACROOPERATORP pmo
);
void FreeMacroOperators(void);
ELIDEDOPERATORP AddElidedOperator
(
	char *psName						/* operator name */
);
void MarkElidedOperator
(
	ELIDEDOPERATORP peo
);
void FreeElidedOperators(void);


#endif /* __OPER_H */
