/* gate.h -- instrumentation routines */

#ifndef __GATE_H
#define __GATE_H

extern BOOL bRunTime;

void EnterGate
(
	char *psName,						/* name of current routine */
	BOOL bRunFlag						/* run time flag */
);
void ExitGate
(
	char *psName						/* name of current routine */
);

#endif /* __GATE_H */
