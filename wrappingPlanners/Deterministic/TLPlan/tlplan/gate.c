/* gate.c -- instrumentation routines

Copyright C, 1996 - 99  F. Bacchus

*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef WIN32
#include <windows.h>
#endif /* WIN32 */

#include "tlplan.h"
#include "gate.h"
#include "tlparse.h"
#include "util.h"
#include "zone.h"

typedef struct NameStack
{
	struct NameStack *pnNext;			/* pointer to next */
	char *psName;						/* name of routine */
}NAMESTACK, *PNAMESTACK;

/* global data */

BOOL bRunTime;							/* current run/compile state of planner */

/* local data */

static PNAMESTACK pnNameStack;

/* EnterGate

Description:
	Push a name onto the name stack, check for compile time routines being
	called at run time.
*/

void EnterGate
(
	char *psName,						/* name of current routine */
	BOOL bRunFlag						/* run time flag */
)
{
	char *ps;
	PNAMESTACK pn;

	if(bRunTime&&!bRunFlag)
		CommandPrintf(stderr,"Compile time %s called at run time from %s\n",
			psName,pnNameStack->psName);
/*	ps=(char *)malloc(strlen(psName)+1); */
	ps=(char *)ZoneAlloc(strlen(psName)+1);
	strcpy(ps,psName);
/*	pn=(PNAMESTACK)malloc(sizeof(NAMESTACK)); */
	pn=(PNAMESTACK)ZoneAlloc(sizeof(NAMESTACK));
	pn->pnNext=pnNameStack;
	pn->psName=ps;
	pnNameStack=pn;
}

/* ExitGate

Description:
	Pop a name off of the name stack, report any problems.
*/

void ExitGate
(
	char *psName
)
{
	PNAMESTACK pn;

	if(strcmp(psName,pnNameStack->psName))
		CommandPrintf(stderr,"Exiting %s from %s\n",psName,pnNameStack->psName);
	if(!pnNameStack)
	{
		CommandPrintf(stderr,"Name stack underflow\n");
		return;
	}
	pn=pnNameStack;
	pnNameStack=pn->pnNext;
	Free(pn->psName);
	Free(pn);
}
