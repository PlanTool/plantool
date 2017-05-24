/* queue.h --  Concurrent Time Queue Support */

extern double dfDelayedActionTime;
extern double dfWaitEventTime;
int nDelayedAction;
int nWaitEvent;

// global function prototypes

void ClearQueue(void);
BOOL EvalDelayedAction
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalGlobalDelayedAction
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalWaitForNextEvent
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
void MarkTimeQueue(void);
void DumpQueue(void);
void DumpQueueEvents(LINEARPLANP plpLinearPlan);
void QueueDelete
(
	LINEARPLANP plp
);
BOOL EvalInhibitDelayedAction
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
CELLP ComputeCurrentTime
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BOOL EvalReachableEvent
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
);
BINDINGP CopyBindings
(
	BINDINGP pbBindings
);
