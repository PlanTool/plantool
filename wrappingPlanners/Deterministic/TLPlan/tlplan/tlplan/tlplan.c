/* tlplan.c

Copyright C, 1997 - 2001, F. Bacchus

Temporal Logic Planner Main Routine

Log

Name		Date	Description

M Ady		970118	First Version

*/

#include <setjmp.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef WIN32
#include <windows.h>
#endif // WIN32

#include "tlplan.h"
#include "eval.h"
#include "iface.h"
#include "tllex.h"
#include "tlparse.h"

/* local data */

static jmp_buf jbOuterJump;				/* local longjmp context */

/* global data */

/* local function prototypes */

/* main

Description:
	Open the input file, parse it, and invoke the planner.
*/

int main
(
	int argc,							/* argument count */
	char **argv							/* argument vector */
)
{
	ENTER("main",FALSE);
	if(argc>1)
	{
		if(argc>2||strcmp(argv[1],"?")==0)
		{
			CommandPrintf(stderr,"Usage: tlplan <domains.tlp>\n");
			exit(1);
		}
	}
	pjbCurrentJump=&jbOuterJump;
	if(!setjmp(jbOuterJump))			/* set up for quick exit to interpreter */
	{
		InitPlanner();					/* initialize planner */
		if(argc>1)
			LoadFile(argv[1]);			/* load the domain file */
	}
	if(!bImmediateExit)
		LoadFile(NULL);					/* interactive interpreter */
	ClosePlanner();						/* print statistics */
	EXIT("main");
	return(0);
}


