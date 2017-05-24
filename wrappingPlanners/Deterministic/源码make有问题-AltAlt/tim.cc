/* TIM main file.

* $Revision: 1.15 $
* $Date: 1999/07/13 12:51:44 $

*/

#include "States.h"
#include "TimInterface.h"
#include "Rules.h"
#include "alldefs.h"
#include <sys/time.h>

struct timeval tstart,tstart1,tend;

extern "C" domptr yyparse();
extern "C" void printresults();
extern "C" void printop();
extern "C" domptr topdom;
extern "C" predlist allpreds;
extern "C" char * pname;
extern "C" char * dname;
extern "C" FILE * yyin;
extern "C" void initialiseParser();


int subtype(int * x,int * y)
{
	return (((x[0] & y[0]) == x[0]) && ((x[1] & y[1]) == x[1]));
};

/* Procedure used to output the argument types for operators. */

void outargs(ostream & o,propptr vs)
{
  int argnum = 1;
   for(;vs;vs=vs->as)
     {
       o << "x" << argnum++ << ":";

	int y = 0;

	for(int j = 0;j<numTypes;j++)
	{
		if(subtype(vs->nm->type,alltypes[j].getTypeVec()))
		{
			if(y) o << " U ";
			o << "T" << j;
			y = 1;
		};
	};
	if(vs->as) o << ",";	

     };
};

/* A resource is rigid if it is state valued for some property it defines and
that state-valued property appears no more than once in any of the associated 
state space states. */

int rigidFR(char * nm,int a)
{
	char prp[strlen(nm)+2];
	strcpy(prp,nm);
	prp[strlen(nm)+1] = '\0';
	int i;

	for(i = 1;i<=a;i++)
	{
		prp[strlen(nm)] = (char) (48 + i);
		char c = symtab.enter(prp);
		int x = cnv(c);
		x = state_seeds.statefor(x);


		if(x==0 || x >= numRStates) break;
		x = allstates[x].rigidity(c);
		if(x==1) break;
	};

	return i<=a;
};

/* This code is for outputting the domain invariants. It uses the fr field in predicates
which is set during the parsing phase. The rigidFR call is used to check whether the
invariant can be a strict equality. Note that we can simply report fixed resources of
size 1, since the balancing ensures that there cannot be anyway to lose that single
proposition (or some variation of it). */

void showDomainInvariants(ostream & cout)
{
	for(predlist ps = allpreds;ps;ps=ps->next)
		if(ps->fr>0)
		{
			int c = 0;
			for(plptr fs = topdom->initial_state;fs;fs=fs->rest)
				if(!strcmp(fs->prop->nm->varname,ps->name)) c++;

			cout << "|{";
			if(ps->numargs>1) cout << "(";
			for(int i = 0;i<ps->numargs;i++)
			{	cout << "x"<<i;
				if(i+1 < ps->numargs) cout << ",";
			};
			if(ps->numargs>1) cout << ")";
			cout << ": " << ps->name << "(";
			for(int i = 0;i<ps->numargs;i++)
			{	cout << "x"<<i;
				if(i+1 < ps->numargs) cout << ",";
			};
			cout << ")}| ";

			if(c > 1 && !rigidFR(ps->name,ps->numargs)) cout << "<";

			if(c==0)
			   for(tpptr obs = topdom->obs;obs;obs=obs->tps)
				if(obs->type->varname == ps->name) c++;


			cout << "= " << c << "\n";
		};
};


#ifdef GUI
int tim(ostream & cin,ostream & cout,int argc,char * argv[])
#endif
#ifndef GUI
int main(int argc,char * argv[])
#endif
{
	cout << "TIM $Revision: 1.15 $: Type Inference Mechanism\n\n"
	<< "Support module for STAN: State Analysis Planner\n\n"
	<< "D. Long and M. Fox, University of Durham\n\n\n"
	<< "Reading domain file: " << argv[1] << "\n";

	gettimeofday(&tstart,0);
	gettimeofday(&tstart1,0);

	yyin = fopen(argv[1],"r");


	if(!yyin)
	{
		cerr << "Operator file not found: " << argv[1] << "\n";
		exit(1);
	};

	initialiseParser();

	yyparse();

	cout << "Reading problem file: " << argv[2] << "\n";

	yyin = fopen(argv[2],"r");

	if(!yyin)
	{
		cerr << "Problem file not found!\n";
		exit(1);
	};

	yyparse();

	gettimeofday(&tend,0);
	long t1 = (tend.tv_sec - tstart.tv_sec) * 1000 + (tend.tv_usec-tstart.tv_usec)/1000;

	gettimeofday(&tstart,0);

	stan(cout);


	gettimeofday(&tend,0);
	long t2 = (tend.tv_sec - tstart.tv_sec) * 1000 + (tend.tv_usec-tstart.tv_usec)/1000;
	gettimeofday(&tstart,0);



	for(rules rs = allrules;rs;rs=rs->next)
		rs->rule->addToState();

	cout << "\nTIM: Domain analysis complete for " << dname << "\n\nTIM: TYPES:\n\n";

	for(int i = 0;i<numTypes;i++)
		cout << alltypes[i] << "\n";

	cout << "\n\nTIM: STATE INVARIANTS:\n\n";

	for(int i = 0;i < numStates;i++)
		if(allstates[i].isState()) cout << allstates[i] << "\n";

	cout << "\n\nTIM: DOMAIN INVARIANTS:\n\n";

	showDomainInvariants(cout);

	cout << "\n\nTIM: ATTRIBUTE SPACES:\n\n";

	for(int i = 1;i < numStates;i++)
		if(!allstates[i].isState()) cout << allstates[i] << "\n";


	cout << "\n\nTIM: OPERATOR PARAMETER RESTRICTIONS:\n\n";

	for(oplistptr ops = topdom->ops;ops;ops = ops->ops)
	{
		cout << ops->op->thename->nm->varname << "(";

		outargs(cout,ops->op->thename->as);

		cout << ")\n";
					
	};


	cout << "\n\nTIM: ADDITIONAL STATE INVARIANTS, USING SUB-STATE ANALYSIS:\n\n";

	for(int i = 1;i < numRStates;i++)
	{
#ifdef TIM_DEBUG
		allstates[i].showall();
		cout << "State " << i << " might have " << numSubStates(allstates[i]) << " sub-states\n";
#endif

		analyseSubStates(cout,allstates[i]);

	};

	if(specialStates || specialSubStates)
	{
		cout << "\n\nTIM: FINAL COLLECTION OF INVARIANTS FROM SPECIAL SPACES:\n\n";

		for(;specialStates;specialStates = specialStates->next)
			cout << *(specialStates->state);

		for(;specialSubStates;specialSubStates = specialSubStates->next)
			cout << *(specialSubStates->state);

	};


	gettimeofday(&tend,0);
	cout << "\nParse time: " << t1 << "millisecs\nAnalysis time: " << t2 << "millisecs\nFinal analysis and output time: " << (tend.tv_sec - tstart.tv_sec) * 1000 + (tend.tv_usec-tstart.tv_usec)/1000 << "millisecs\n";

	cout << "\nTotal time: " << (tend.tv_sec - tstart1.tv_sec) * 1000 + (tend.tv_usec-tstart1.tv_usec)/1000 << "millisecs\n\n";

	return 0;

};


