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
char *slogFile; // log file 

/* global data */

/* local function prototypes */

/* main

Description:
	Open the input file, parse it, and invoke the planner.
*/


void usage_tlplan() {
  CommandPrintf(stderr,"Usage: tlplan [-l <logfile>] [<domains.tlp>]\n");
  exit(1);
}

int main
(
	int argc,							/* argument count */
	char **argv							/* argument vector */
)
{
	char *filename=NULL;
	ENTER("main",FALSE);
	slogFile="tlplan.log";

	if (argc==2) {
	  if (!strcmp(argv[1],"?") || !strcmp(argv[1],"-h"))
	    usage_tlplan();
	  else filename=argv[1];
	}
	else if (argc>2) {
	  if (!strcmp(argv[1],"-l")) {
	    slogFile=argv[2];
	    if (argc>3)
	      filename=argv[3];
	  } else if (!strcmp(argv[2],"-l") && argc>3) {
	    slogFile=argv[3];
	    filename=argv[1];
	  } else 
	    usage_tlplan();
	}
	  
	
	pjbCurrentJump=&jbOuterJump;
	if(!setjmp(jbOuterJump))			/* set up for quick exit to interpreter */
	{
		InitPlanner();					/* initialize planner */
		if(filename)
			LoadFile(filename);			/* load the domain file */
	}
	if(!bImmediateExit)
		LoadFile(NULL);					/* interactive interpreter */
	ClosePlanner();						/* print statistics */
	EXIT("main");
	return(0);
}


