/* cdf.c

Description:
	Inverse cumulative distribution functions.
	Calls cdf functions in "netlib" dcdflib.c
*/

#include <stdio.h>

#include "../tlplan.h"
#include "../formula.h"
#include "../makeform.h"

#include "dcdflib.h"

/* ICDFBeta

Description:
	Returns the beta parameter at which the beta cdf has the specified value.
Call:
	(icdf-beta p a b)
Where:
	p is cdf value (integral) [0, 1]
	a is first parameter [0, +infinity]
	b is second parameter [0, +infinity]
Returns
	upper limit of beta cdf [0, 1]
*/

DECLSPECX CELLP ICDFBeta
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;
	int which;
	double p;
	double q;
	double x;
	double y;
	double a;
	double b;
	int status;
	double bound;

	pc=pcFormula->pfForm->pcArgs;
	if(!FormulaToDouble(pc,plpLinearPlan,pbBindings,&p))
		return NULL;					/* message already printed */
	pc=pc->pcNext;	
	if(!FormulaToDouble(pc,plpLinearPlan,pbBindings,&a))
		return NULL;					/* message already printed */
	pc=pc->pcNext;	
	if(!FormulaToDouble(pc,plpLinearPlan,pbBindings,&b))
		return NULL;					/* message already printed */

	which=2;
	q=1.0-p;

	cdfbet(&which,&p,&q,&x,&y,&a,&b,&status,&bound);
	if(!status)
	{
		pc=MakeFloatForm(x);
		return pc;
	}
	return NULL;
}

/* ICDFBinomial

Description:
	Returns the number of successes at which the binomial cdf has the specified value.
Call:
	(icdf-binomial p xn prob)
Where:
	p is cdf value (integral) [0, 1]
	xn is number of trials (0, +infinity)
	prob is probability on each trial [0,1].
Returns
	number of successes observed [0, XN].
*/

DECLSPECX CELLP ICDFBinomial
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;
	int which;
	double p;
	double q;
	double s;
	double xn;
	double pr;
	double ompr;
	int status;
	double bound;

	pc=pcFormula->pfForm->pcArgs;
	if(!FormulaToDouble(pc,plpLinearPlan,pbBindings,&p))
		return NULL;					/* message already printed */
	pc=pc->pcNext;	
	if(!FormulaToDouble(pc,plpLinearPlan,pbBindings,&xn))
		return NULL;					/* message already printed */
	pc=pc->pcNext;	
	if(!FormulaToDouble(pc,plpLinearPlan,pbBindings,&pr))
		return NULL;					/* message already printed */

	which=2;
	q=1.0-p;
	ompr=1.0-pr;

	cdfbin(&which,&p,&q,&s,&xn,&pr,&ompr,&status,&bound);
	if(!status)
	{
		pc=MakeFloatForm(s);
		return pc;
	}
	return NULL;
}

/* ICDFChiSquare

Description:
	Returns the chi-square parameter at which the chi-square cdf has the specified value.

Call:
	(icdf-chi-square p df)
Where:
	p is cdf (integral) [0, 1]
	df is degrees of freedom (0, +infinity)
Returns
	upper limit of chi-square cdf [0, +infinity).
*/

DECLSPECX CELLP ICDFChiSquare
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;
	int which;
	double p;
	double q;
	double x;
	double df;
	int status;
	double bound;

	pc=pcFormula->pfForm->pcArgs;
	if(!FormulaToDouble(pc,plpLinearPlan,pbBindings,&p))
		return NULL;					/* message already printed */
	pc=pc->pcNext;	
	if(!FormulaToDouble(pc,plpLinearPlan,pbBindings,&df))
		return NULL;					/* message already printed */

	which=2;
	q=1.0-p;

	cdfchi(&which,&p,&q,&x,&df,&status,&bound);
	if(!status)
	{
		pc=MakeFloatForm(x);
		return pc;
	}
	return NULL;
}

/* ICDFNonCentraChiSquare

Description:
	Returns the chi-square parameter at which the non-central chi-square cdf has the specified value.
Call:
	(icdf-non-central-chi-square p df pnon)
Where:
	p is cdf (integral) [0, 1-1E-16)
	df is degrees of freedom (0, +infinity)
	pnon is non-centrality [0, +infinity)
Returns
	upper limit of non-central chi-square cdf [0,1E300].
*/

DECLSPECX CELLP ICDFNonCentralChiSquare
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;
	int which;
	double p;
	double q;
	double x;
	double df;
	double pnon;
	int status;
	double bound;

	pc=pcFormula->pfForm->pcArgs;
	if(!FormulaToDouble(pc,plpLinearPlan,pbBindings,&p))
		return NULL;					/* message already printed */
	pc=pc->pcNext;	
	if(!FormulaToDouble(pc,plpLinearPlan,pbBindings,&df))
		return NULL;					/* message already printed */
	pc=pc->pcNext;	
	if(!FormulaToDouble(pc,plpLinearPlan,pbBindings,&pnon))
		return NULL;					/* message already printed */

	which=2;
	q=1.0-p;

	cdfchn(&which,&p,&q,&x,&df,&pnon,&status,&bound);
	if(!status)
	{
		pc=MakeFloatForm(x);
		return pc;
	}
	return NULL;
}

/* ICDFF

Description:
	Returns the f parameter at which the f cdf has the specified value.
Call:
	(icdf-f p dfn dfd)
Where:
	p is cdf (integral) [0, 1]
	dfn is degrees of freedom of numerator (0, +infinity)
	dfd is degrees of freedom of denominator (0, +infinity)
Returns
	upper limit of f cdf [0,1E300].
*/

DECLSPECX CELLP ICDFF
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;
	int which;
	double p;
	double q;
	double f;
	double dfn;
	double dfd;
	int status;
	double bound;

	pc=pcFormula->pfForm->pcArgs;
	if(!FormulaToDouble(pc,plpLinearPlan,pbBindings,&p))
		return NULL;					/* message already printed */
	pc=pc->pcNext;	
	if(!FormulaToDouble(pc,plpLinearPlan,pbBindings,&dfn))
		return NULL;					/* message already printed */
	pc=pc->pcNext;	
	if(!FormulaToDouble(pc,plpLinearPlan,pbBindings,&dfd))
		return NULL;					/* message already printed */

	which=2;
	q=1.0-p;

	cdff(&which,&p,&q,&f,&dfn,&dfd,&status,&bound);
	if(!status)
	{
		pc=MakeFloatForm(f);
		return pc;
	}
	return NULL;
}

/* ICDFNonCentralF

Description:
	Returns the f parameter at which the non-central f cdf has the specified value.
Call:
	(icdf-non-central-f p dfn dfd pnon
Where:
	p is cdf (integral) [0,1-1E-16)
	dfn is degrees of freedom of numerator (0, +infinity)
	dfd is degrees of freedom of denominator (0, +infinity)
	pnon is non-centrality [0,infinity)
Returns
	upper limit of non-central f cdf [0,1E300].
*/

DECLSPECX CELLP ICDFNonCentralF
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;
	int which;
	double p;
	double q;
	double f;
	double dfn;
	double dfd;
	double pnon;
	int status;
	double bound;

	pc=pcFormula->pfForm->pcArgs;
	if(!FormulaToDouble(pc,plpLinearPlan,pbBindings,&p))
		return NULL;					/* message already printed */
	pc=pc->pcNext;	
	if(!FormulaToDouble(pc,plpLinearPlan,pbBindings,&dfn))
		return NULL;					/* message already printed */
	pc=pc->pcNext;	
	if(!FormulaToDouble(pc,plpLinearPlan,pbBindings,&dfd))
		return NULL;					/* message already printed */
	pc=pc->pcNext;	
	if(!FormulaToDouble(pc,plpLinearPlan,pbBindings,&pnon))
		return NULL;					/* message already printed */

	which=2;
	q=1.0-p;

	cdffnc(&which,&p,&q,&f,&dfn,&dfd,&pnon,&status,&bound);
	if(!status)
	{
		pc=MakeFloatForm(f);
		return pc;
	}
	return NULL;
}

/* CDFGamma

Description:
	Returns the sample value at which the beta cdf has the specified value.
Call:
	(cdf-gamma p shape scale)
Where:
	p is cdf (integral) [0, 1]
	shape is shape parameter (0, +infinity)
	scale is scale parameter (0, +infinity)
Returns
	upper limit of gamma cdf [0,1E300].
*/

DECLSPECX CELLP ICDFGamma
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;
	int which;
	double p;
	double q;
	double x;
	double shape;
	double scale;
	int status;
	double bound;

	pc=pcFormula->pfForm->pcArgs;
	if(!FormulaToDouble(pc,plpLinearPlan,pbBindings,&p))
		return NULL;					/* message already printed */
	pc=pc->pcNext;	
	if(!FormulaToDouble(pc,plpLinearPlan,pbBindings,&shape))
		return NULL;					/* message already printed */
	pc=pc->pcNext;	
	if(!FormulaToDouble(pc,plpLinearPlan,pbBindings,&scale))
		return NULL;					/* message already printed */

	which=2;
	q=1.0-p;

	cdfgam(&which,&p,&q,&x,&shape,&scale,&status,&bound);
	if(!status)
	{
		pc=MakeFloatForm(x);
		return pc;
	}
	return NULL;
}

/* ICDFNegativeBinomial

Description:
	Returns the sample value at which the negative binomial cdf has the specified value.
Call:
	(icdf-negative-binomial p n prob)
Where:
	p is cdf value (integral) [0,1]
	n is number of successes [0, +infinity)
	prob is probability on each trial [0,1].
Returns
	number of failures observed before nth success [0, 1E300].
*/

DECLSPECX CELLP ICDFNegativeBinomial
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;
	int which;
	double p;
	double q;
	double s;
	double xn;
	double pr;
	double ompr;
	int status;
	double bound;

	pc=pcFormula->pfForm->pcArgs;
	if(!FormulaToDouble(pc,plpLinearPlan,pbBindings,&p))
		return NULL;					/* message already printed */
	pc=pc->pcNext;	
	if(!FormulaToDouble(pc,plpLinearPlan,pbBindings,&xn))
		return NULL;					/* message already printed */
	pc=pc->pcNext;	
	if(!FormulaToDouble(pc,plpLinearPlan,pbBindings,&pr))
		return NULL;					/* message already printed */

	which=2;
	q=1.0-p;
	ompr=1.0-pr;

	cdfnbn(&which,&p,&q,&s,&xn,&pr,&ompr,&status,&bound);
	if(!status)
	{
		pc=MakeFloatForm(s);
		return pc;
	}
	return NULL;
}

/* ICDFNormal

Description:
	Returns the sample value at which the normal cdf has the specified value.
Call:
	(icdf-normal p mean sd)
Where:
	p is cdf value (integral) (0,1]
	mean is the center of the distribution (-infinity, +infinity)
	sd is the standard deviation (0, +infinity)
Returns
	upper limit of normal cdf (-infinity, +infinity)
*/

DECLSPECX CELLP ICDFNormal
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;
	int which;
	double p;
	double q;
	double x;
	double mean;
	double sd;
	int status;
	double bound;

	pc=pcFormula->pfForm->pcArgs;
	if(!FormulaToDouble(pc,plpLinearPlan,pbBindings,&p))
		return NULL;					/* message already printed */
	pc=pc->pcNext;	
	if(!FormulaToDouble(pc,plpLinearPlan,pbBindings,&mean))
		return NULL;					/* message already printed */
	pc=pc->pcNext;	
	if(!FormulaToDouble(pc,plpLinearPlan,pbBindings,&sd))
		return NULL;					/* message already printed */

	which=2;
	q=1.0-p;

	cdfnor(&which,&p,&q,&x,&mean,&sd,&status,&bound);
	if(!status)
	{
		pc=MakeFloatForm(x);
		return pc;
	}
	return NULL;
}

/* ICDFPoisson

Description:
	Returns the beta parameter at which the beta cdf has the specified value.

Call:
	(icdf-poisson p mean);
Where:
	p is cdf value (integral) [0, 1]
	mean is the mean of the distribution [0, +infinity)
Returns
	upper limit of poison cdf [0,1E300]
*/

DECLSPECX CELLP ICDFPoison
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;
	int which;
	double p;
	double q;
	double s;
	double mean;
	int status;
	double bound;

	pc=pcFormula->pfForm->pcArgs;
	if(!FormulaToDouble(pc,plpLinearPlan,pbBindings,&p))
		return NULL;					/* message already printed */
	pc=pc->pcNext;	
	if(!FormulaToDouble(pc,plpLinearPlan,pbBindings,&mean))
		return NULL;					/* message already printed */

	which=2;
	q=1.0-p;

	cdfpoi(&which,&p,&q,&s,&mean,&status,&bound);
	if(!status)
	{
		pc=MakeFloatForm(s);
		return pc;
	}
	return NULL;
}

/* ICDFStudentsT

Description:
	Returns the t parameter at which the student's t cdf has the specified value.
Call:
	(icdf-students-t p df)
Where:
	p is cdf (integral) (0,1]
	df is degrees of freedom [1e-300, 1E10]
Returns
	upper limit of student's t cdf [-1E300, 1E300]
*/

DECLSPECX CELLP ICDFStudentsT
(
	CELLP pcFormula,
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	CELLP pc;
	int which;
	double p;
	double q;
	double t;
	double df;
	int status;
	double bound;

	pc=pcFormula->pfForm->pcArgs;
	if(!FormulaToDouble(pc,plpLinearPlan,pbBindings,&p))
		return NULL;					/* message already printed */
	pc=pc->pcNext;	
	if(!FormulaToDouble(pc,plpLinearPlan,pbBindings,&df))
		return NULL;					/* message already printed */

	which=2;
	q=1.0-p;

	cdft(&which,&p,&q,&t,&df,&status,&bound);
	if(!status)
	{
		pc=MakeFloatForm(t);
		return pc;
	}
	return NULL;
}
