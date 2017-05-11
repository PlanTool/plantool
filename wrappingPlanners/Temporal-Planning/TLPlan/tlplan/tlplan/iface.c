/* iface.c

Copyright C, 1996 - 2001  F. Bacchus

Interface
	There are a few straggling functions here, from the old command interface.
	These routines should be put elsewhere and this module should be retired.
*/

#include <setjmp.h>
#include <stdio.h>

#include "tlplan.h"
#include "iface.h"
#include "formula.h"
#include "util.h"

/* Utility Routines ------------------------------------------------------------ */

/* TermError

Description:
	Display a term evaluation error.  
	A term is undefined, or mis-defined.
	Print a message, display a formula and jump back to the previous longjmp location.
*/

DECLSPEC void TermError
(
	char *psMessage,					/* message string */
	CELLP pcFormula,					/* formula to display */
	BINDINGP pbBindings					/* bindings list */
)
{
	Message("Error, undefined or uninitialized term in %s\n",psMessage);
	if(pcFormula)
	{
		PrintFormula(stderr,pcFormula,0);
		PrintFormula(pfTraceStream,pcFormula,0);
		if(pbBindings)
			DumpBindings(pbBindings);
	}
	if(pjbCurrentJump==&jbTLPlannerJump)
		longjmp(*pjbCurrentJump,1);			/* tear down stack */
}

/* NoEvaluator

Description:
	The code has attempted to "evaluate" something that can't be evaluated.
	Print a message, display a formula and jump back to the previous longjmp location.
*/

BOOL NoEvaluator
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	char ac[40];

	Message("Error, %s cannot be evaluated.\n",GetName(pcFormula->pfForm,ac));
	PrintFormula(stderr,pcFormula,0);
	PrintFormula(pfTraceStream,pcFormula,0);
	if(pjbCurrentJump==&jbTLPlannerJump)
		longjmp(*pjbCurrentJump,1);			/* tear down stack */
	return TRUE;
}

/* NoIdler

Description:
	The code has attempted to "evaluate" something that can't be evaluated.
	Print a message, display a formula and jump back to the previous longjmp location.
*/

BOOL NoIdler
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	char ac[40];						// string buffer

	Message("Error, %s cannot be idled.\n",GetName(pcFormula->pfForm,ac));
	PrintFormula(stderr,pcFormula,0);
	PrintFormula(pfTraceStream,pcFormula,0);
	if(pjbCurrentJump==&jbTLPlannerJump)
		longjmp(*pjbCurrentJump,1);			/* tear down stack */
	return TRUE;
}

/* NoProgressor

Description:
	The code has attempted to "progress" something that can't be progressed.
	Print a message, display a formula and jump back to the previous longjmp location.
*/

CELLP NoProgressor
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings,
	BOOLP pImmutable
)
{
	char ac[40];						// string buffer

	Message("Error, %s cannot be progressed.\n",GetName(pcFormula->pfForm,ac));
	PrintFormula(stderr,pcFormula,0);
	PrintFormula(pfTraceStream,pcFormula,0);
	if(pjbCurrentJump==&jbTLPlannerJump)
		longjmp(*pjbCurrentJump,1);			/* tear down stack */
	return NULL;
}

/* NoCurrentor

Description:
	The code has attempted to "Current" something that can't be Currented.
	Print a message, display a formula and jump back to the previous longjmp location.
*/

int NoCurrentor
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	char ac[40];						// string buffer

	Message("Error, Cannot calculate the current value of %s.\n",GetName(pcFormula->pfForm,ac));
	PrintFormula(stderr,pcFormula,0);
	PrintFormula(pfTraceStream,pcFormula,0);
	if(pjbCurrentJump==&jbTLPlannerJump)
		longjmp(*pjbCurrentJump,1);			/* tear down stack */
	return NULL;
}

/* NoComputor

Description:
	The code has attempted to "compute" something that can't be computed.
	Print a message, display a formula and jump back to the previous longjmp location.
*/

CELLP NoComputor
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	char ac[40];						// string buffer

	Message("Error, %s cannot be computed.\n",GetName(pcFormula->pfForm,ac));
	PrintFormula(stderr,pcFormula,0);
	PrintFormula(pfTraceStream,pcFormula,0);
	if(pjbCurrentJump==&jbTLPlannerJump)
		longjmp(*pjbCurrentJump,1);			/* tear down stack */
	return NULL;
}

/* NoGenerator

Description:
	The code has attempted to "generate" something that can't be generated.
	Print a message, display a formula and jump back to the previous longjmp location.
*/

BOOL NoGenerator
(
	CELLP pcGenLit,
	void **ppvContext,
	CELLP pcVars,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	char ac[40];						// string buffer

	Message("Error, %s cannot be used as a generator.\n",GetName(pcGenLit->pfForm,ac));
	PrintFormula(stderr,pcGenLit,0);
	PrintFormula(pfTraceStream,pcGenLit,0);
	if(pjbCurrentJump==&jbTLPlannerJump)
		longjmp(*pjbCurrentJump,1);		/* tear down stack */
	return FALSE;						/* terminate generator immediately */
}

/* NoCCForm

Description:
	The code has attempted to "CC" something that can't be CCed.
	Print a message, display a formula and jump back to the previous longjmp location.
*/

CELLP NoCCForm
(
	BOOL bType,
	CELLP pcForm,
	BOOL *pbImmutable
)
{
	char ac[40];						// string buffer

	Message("Error, Cannot generate the current condition formula of %s.\n",GetName(pcForm->pfForm,ac));
	PrintFormula(stderr,pcForm,0);
	PrintFormula(pfTraceStream,pcForm,0);
	if(pjbCurrentJump==&jbTLPlannerJump)
		longjmp(*pjbCurrentJump,1);			/* tear down stack */
	*pbImmutable=FALSE;
	return NULL;
}

/* CCImplemented

Description:
	The code has attempted to "CC" something that hasn't been implemented.
	Print a message, display a formula and jump back to the previous longjmp location.
*/

CELLP CCNotImplemented
(
	BOOL bType,
	CELLP pcForm,
	BOOL *pbImmutable
)
{
	char ac[40];						// string buffer
	
	Message("Error, %s hasn't been implemented in the current condition algorithm.\n",GetName(pcForm->pfForm,ac));
	PrintFormula(stderr,pcForm,0);
	PrintFormula(pfTraceStream,pcForm,0);
	if(pjbCurrentJump==&jbTLPlannerJump)
		longjmp(*pjbCurrentJump,1);			/* tear down stack */
	*pbImmutable=FALSE;
	return NULL;
}

