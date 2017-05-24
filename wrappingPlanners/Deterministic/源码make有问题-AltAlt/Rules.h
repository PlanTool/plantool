/*
* $Revision: 1.3 $
* $Date: 1999/07/09 21:45:02 $
*/

#ifndef __RULES
#define __RULES

#include "SymTab.h"
#include "BasicTim.h"
#include "switches.h"
#include "alldefs.h"

class subStates;

class stateSeeds {
private:
	char stseeds[256];
	int used[256];
	int marked[256];
	int realState[256];

	int deref(char);

public:

	stateSeeds()
	{
		for(int i = 0;i<256;i++)
		{	stseeds[i] = (char) i;
			used[i] = 0;
			marked[i] = 0;
			realState[i] = 0;
		};
	};
	void unite(char,char *);
	void showStSeeds();
	void showAtSeeds();
	void setupStates();
	int countStates();
	int statefor(int);
	void markAttribute(int i){marked[i] = 1;};
	int shouldUse(int i){return used[i]&&marked[i];};
	void adjustAttributes(stateSeeds);
	int isMarked(int i){return marked[i];};
	void unmarkAll()
	{
		for(int i = 0;i<256;i++)
			marked[i] = 0;
	};
	void markStates();
	int isState(int i){return realState[i];};
	void notRealState(int);
	void markAttsReSeeded()
	{
		for(int i =0 ;i<256;i++) realState[i] = 1;

		for(int i=0;i<256;i++)
		{	if (marked[i]) 
				notRealState(deref(stseeds[i]));
		};
	};	
	void markReducer(int i){realState[i] = 1;};
	int isReducer(int i){return realState[i];};
	void initialise()
	{
		for(int i = 0;i<256;i++)
		{	stseeds[i] = (char) i;
			used[i] = 0;
			marked[i] = 0;
			realState[i] = 0;
		};
	};
	void setupReseededs(subStates *,int);	
};



extern stateSeeds state_seeds;
extern stateSeeds attribute_seeds;

class transrule {
private:
	char * left, * right, *enablers;
	opptr producer;
	tpptr varName;

public:	
	transrule(char * l,char * r,char * e,opptr op,tpptr vn) : 
		producer(op), varName(vn)
	{
		left = new char[strlen(l)+1];
		strcpy(left,l);
		sortsym(left);
		right = new char[strlen(r)+1];
		strcpy(right,r);

		enablers = new char[strlen(e)];

		unsigned int i;
		for(i = 0;i<=strlen(e);i++)
		{
			if(l[0] == e[i]) break;

			enablers[i] = e[i];
		};
		for(i=i+1;i<=strlen(e);i++)
			enablers[i-1] = e[i];

		enablers[i] = '\0';

		state_seeds.unite(left[0],left);
		state_seeds.unite(left[0],right);
		attribute_seeds.unite(left[0],left);
		attribute_seeds.unite(left[0],right);
	};
	transrule(char * r,char * e,opptr op,tpptr vn) : 
		producer(op), varName(vn)
	{
		left = new char[1];
		left[0] = '\0';
		right = new char[strlen(r)+1];
		strcpy(right,r);
			
		enablers = new char[strlen(e)+1];
		strcpy(enablers,e);

		attribute_seeds.unite(right[0],right);

		for(int i = 0;r[i] != '\0';i++)
			attribute_seeds.markAttribute(cnv(r[i]));


	};
	transrule(opptr op,tpptr vn,char * l,char * e) : 
		producer(op), varName(vn)
	{
		right = new char[1];
		right[0] = '\0';
		left = new char[strlen(l)+1];
		strcpy(left,l);
			
		enablers = new char[strlen(e)+1];
		strcpy(enablers,e);

		attribute_seeds.unite(left[0],left);

		for(int i = 0;l[i] != '\0';i++)
			attribute_seeds.markAttribute(cnv(l[i]));


	};
	void showrule();
	void showrule(ostream &);
	char tryrule(char *);
	char trySubRule(char *);
	int ordered(int,int);
	int enabled();
	void addToState();
	int addToState(stateSeeds&);
	void addToAttState();
	char * getEnablers(){return enablers;};
	int isEnabled(int);
	int isAttRule();
	int splitRule(char);
	int isRelevant(char c){return left[0] == c;};
	char * getFirstProp()
	{
		if(left[0] != '\0') return symtab.getsym(left[0]);
		return symtab.getsym(right[0]);
	};
	char * implicit_getFirstProp(){if(!(left[0]=='\0')){return symtab.getsym(left[0]);} else {return symtab.getsym(right[0]);};};
	void unite(stateSeeds& sts)
	{
		if(left[0]!='\0')
		{
			sts.unite(left[0],left);
			sts.unite(left[0],right);
			if (right[0]=='\0') sts.markAttribute(left[0]);
		}
		else
		{	sts.unite(right[0],right);
			sts.markAttribute(right[0]);
		};
	};
	tpptr getvarName(){return varName;};

	int onlyStaticEnablers();
};


class rulelistnode {
public:
	transrule * rule;
	rulelistnode * next;

	rulelistnode(transrule * r,rulelistnode * n) : rule(r), next(n) {};
};

typedef rulelistnode * rules;



extern rules allrules;
extern rules alltransrules;
extern rules alldecrules;
extern int numStates;
extern int numRStates;
 
#endif
