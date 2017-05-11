/*
* $Revision: 1.3 $
* $Date: 1999/07/09 21:35:39 $
*/

#include "TimInterface.h"
#include "BasicTim.h"
#include "Rules.h"


#include <fstream.h>

void initialiseTIM()
{
	symtab.wipe();
	allobs = 0;
	numRStates = 0;
	state_seeds.initialise();
	attribute_seeds.initialise();
	allrules = alltransrules = alldecrules = 0;
	statetab.wipe();
	gotLoop = 0;
	allstates = 0;
	numStates = 0;
	specialStates = 0;
	numTypes = 0;
	alltypes = 0;
	substates = 0;
};



void stan(ostream & cout)
{
	initialiseTIM();

	for(oplistptr ops = topdom->ops;ops;ops = ops->ops)
	{	analyseOp(ops->op);
	};

	state_seeds.markStates();
	markAttSpaces();

#ifdef STANSHOW
	for(rules rs = allrules;rs;rs = rs->next)
		rs->rule->showrule();
	cout << "Transrules\n";
	for(rules rs = alltransrules;rs;rs = rs->next)
		rs->rule->showrule();

	state_seeds.showStSeeds();
	cout << "Attribute sets\n";
	attribute_seeds.showAtSeeds();
#endif

	state_seeds.setupStates();
	attribute_seeds.adjustAttributes(state_seeds);

	for(tpptr obs = topdom->obs;obs;obs=obs->tps)
		analyseOb(obs->type);

#ifdef STANSHOW
	for(int i = 1;i<numStates;i++)
		allstates[i].showall();
#endif

	for(int i = 1;i<numStates;i++)
		allstates[i].extend();

#ifdef STANSHOW
	for(int i = 1;i<numStates;i++)
		allstates[i].showall();

	orderings();
#endif

	setalltypes();

#ifdef STANSHOW
	for(objectlist os = allobs;os;os=os->next)
		cout << *(os->obj) << "\n";

	for(int i = 0;i<numTypes;i++)
		for(int j = 0;j<numTypes;j++)
			if(i != j && derivedType(alltypes[i],alltypes[j]))
				cout << "Type " << j << " derived from type " << i << "\n";
#endif

	distributeDecreasingRules();

	statics();

#ifdef STANSHOW
	cout << "\n";
	printrels(topdom->statics);
	cout << "\n";
	print(topdom->ops);
#endif

	setSymmetries();

#ifdef SYMMETRYOUT
	cout << "There are " << object(0,0).howManySymObs() << " symmetric objects\n";

for(objectlist os = allobs;os;os=os->next)
	cout << *(os->obj) << " " << os->obj->getSymposn() << "/" << os->obj->getGroupsize() << "\n";

#endif
#ifdef STANSHOW
	for(objectlist os = allobs;os;os=os->next)
		cout << *(os->obj) << "\n";
#endif

//	goalStateAnalysis();

};

void timcall()
{
	for(rules rs = allrules;rs;rs=rs->next)
		rs->rule->addToState();

	ofstream fout("/dev/null");

	for(int i = 1;i < numRStates;i++)
	{
#ifdef TIM_DEBUG
		allstates[i].showall();
		cout << "State " << i << " might have " << numSubStates(allstates[i]) << " sub-states\n";
#endif

		analyseSubStates(fout,allstates[i]);
	};

};



