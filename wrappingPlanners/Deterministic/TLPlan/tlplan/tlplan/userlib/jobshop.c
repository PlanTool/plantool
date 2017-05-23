// jobshop.c

#include <setjmp.h>
#include <stdio.h>
#include <stdlib.h>

#ifdef WIN32
#include <windows.h>
#endif // WIN32

#include "../tlplan.h"
#include "../compute.h"
#include "../eval.h"
#include "../formula.h"
#include "../iface.h"
#include "../list.h"
#include "../makeform.h"
#include "../makegen.h"
#include "../tllex.h"
#include "../tlparse.h"
#include "../tl_tab.h"
#include "../util.h"
#include "../world.h"

/* RandomJobShopworld

Description:
	Random world generator for job shop.
Notes:
	This routine will generate random job shop problems.
	Parameters are silently clamped to valid values:

	(random-jobshop-world <seed> <machines> <jobs> <job-acts> <max-duration>)

	seed -- [0,32767]
	machines -- [1...
	jobs -- [1...
	job-acts -- [1...)
	max-duration -- [1...)

*/

DECLSPECX CELLP RandomJobShopWorld
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc,pc1,pc2;					/* formula pointers */
	CELLP pcStart,pcEnd;				/* literal list head */
	char acBuffer[128];					/* string buffer */
	int i,j,k;							/* array indices */
	int nSeed;							/* world number */
	int nMachines;						/* number of machines */
	int nJobs;							/* number of jobs */
	int nJobActs;						/* number of job actions */
	int nMaxDuration;					/* maximum job-act duration */

	/* get the world number */
	
	pc=pcFormula->pfForm->pcArgs;
	if(!FormulaToInteger(pc,plpLinearPlan,pbBindings,&nSeed))
		return NULL;					/* message already printed */
	if(nSeed<0)
		nSeed=0;
	else if(nSeed>32767)
		nSeed=32767;

	/* get the number of machines */
	
	pc=pc->pcNext;
	if(!FormulaToInteger(pc,plpLinearPlan,pbBindings,&nMachines))
		return NULL;					/* message already printed */
	if(nMachines<1)
		nMachines=1;

	/* get the number of jobs */
	
	pc=pc->pcNext;
	if(!FormulaToInteger(pc,plpLinearPlan,pbBindings,&nJobs))
		return NULL;					/* message already printed */
	if(nJobs<1)
		nJobs=1;

	/* get the number of job actions */
	
	pc=pc->pcNext;
	if(!FormulaToInteger(pc,plpLinearPlan,pbBindings,&nJobActs))
		return NULL;					/* message already printed */
	if(nJobActs<1)
		nJobActs=1;

	/* get the maximum job action duration */
	
	pc=pc->pcNext;
	if(!FormulaToInteger(pc,plpLinearPlan,pbBindings,&nMaxDuration))
		return NULL;					/* message already printed */
	if(nMaxDuration<1)
		nMaxDuration=1;

	/* fill in the plan name */
	
	sprintf(acBuffer,"Job Shop %d (M%d, J%d, JA%d, MD%d)",
		nSeed,nMachines,nJobs,nJobActs,nMaxDuration);
	psPlanName=StrAlloc(acBuffer);

	/* generate the literals */

	pcStart=NULL;						/* initialize literal list head */
	pcEnd=(CELLP)&pcStart;

	pc1=MakeIntegerForm(0);
	for(i=0;i<nMachines;i++)
	{
		pc=MakeIntegerForm(i+1);

		// "(machine %d)"
		
		pcEnd=pcEnd->pcNext=MakeSymbolInfoForm(FALSE,
			NewList(ATOM_IDENT,"machine"),pc);

		// "(= (machine-next-free %d) 0)"

		pcEnd=pcEnd->pcNext=
			MakeEqForm(FALSE,MakeSymbolInfoForm(FALSE,
			NewList(ATOM_IDENT,"machine-next-free"),pc),pc1);
	}

	pc1=MakeIntegerForm(nJobActs);
	for(i=0;i<nJobs;i++)
	{
		pc=MakeIntegerForm(i+1);

		// "(job %d)"
		
		pcEnd=pcEnd->pcNext=MakeSymbolInfoForm(FALSE,
			NewList(ATOM_IDENT,"job"),pc);

		// "(= (num-acts %d) %d)"

		pcEnd=pcEnd->pcNext=
			MakeEqForm(FALSE,MakeSymbolInfoForm(FALSE,
			NewList(ATOM_IDENT,"num-acts"),pc),pc1);

		nSeed=nSeed*214013+2531011;		/* 0x00034DFD & 0x00269EC3 */
		k=((nSeed>>16)&0x00007FFF)%nMaxDuration+1;
		
		// "(= (job-est %d) %d)"

		pcEnd=pcEnd->pcNext=
			MakeEqForm(FALSE,MakeSymbolInfoForm(FALSE,NewList(ATOM_IDENT,"job-est"),pc),
			MakeIntegerForm(k));

		for(j=0;j<nJobActs;j++)
		{
			pc=CopyCellList(pc);
			pc2=MakeIntegerForm(j+1);
			pc->pcNext=pc2;

			// "(= (jobact-duration %d %d) %d)"

			nSeed=nSeed*214013+2531011;		/* 0x00034DFD & 0x00269EC3 */
			k=((nSeed>>16)&0x00007FFF)%nMaxDuration+1;

			pcEnd=pcEnd->pcNext=
				MakeEqForm(FALSE,MakeSymbolInfoForm(FALSE,
				NewList(ATOM_IDENT,"jobact-duration"),pc),MakeIntegerForm(k));

			// "(= (jobact-machine %d %d) %d)"

			nSeed=nSeed*214013+2531011;		/* 0x00034DFD & 0x00269EC3 */
			k=((nSeed>>16)&0x00007FFF)%nMachines+1;

			pcEnd=pcEnd->pcNext=
				MakeEqForm(FALSE,MakeSymbolInfoForm(FALSE,
				NewList(ATOM_IDENT,"jobact-machine"),pc),MakeIntegerForm(k));
		}
	}
	{
		FILE *ofs;

		ofs=fopen("foo.tmp","w");
		PrintFormulaList(ofs,pcStart);
		fclose(ofs);
	}
	return pcStart;
}

