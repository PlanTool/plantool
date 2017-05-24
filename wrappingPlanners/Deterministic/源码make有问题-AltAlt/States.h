/*
* $Revision: 1.3 $
* $Date: 1999/06/25 10:30:09 $
*/

#ifndef __STATES
#define __STATES

#include "SymTab.h"
#include "BasicTim.h"
#include "switches.h"
#include "alldefs.h"
#include "Rules.h"

extern int types[];








class statelistnode;
class States;

void markAttSpaces();
void analyseOb(pptr);
void setalltypes(); 
void distributeDecreasingRules();
void statics();
void setSymmetries();


class oState {
private:
	char state;
	char reduced;
	int parent;
	int superset;
	int subset;
	int initial;

	static symbol_table reducedState;

public:	oState(char,int,int,int);
	void show();
	int addnewstates(States *,rules,statelistnode *);
	char * getState(){return statetab.getsym(state);};
	char * getRedState(){return reducedState.getsym(reduced);};
	int isSuper(){return superset;};
	void markSuper(){superset = 1;};
	void markSub(){subset = 1;};
	int isSub(){return subset;};
	int isInitial(){return initial;};

};

class statelistnode {
public:
	oState os;
	statelistnode * next;

	statelistnode(oState o,statelistnode * n) : os(o), next(n) {};
};

typedef statelistnode * statelist;
extern int gotLoop;

class States {
protected:
	int stSeedSet;
	int doneAttributes;
	oblist objects;
	statelist states;
	rules trules;
	int realState;

	void addobs(oblist);
	int checkAttributes()
	{
		if(doneAttributes > gotLoop){ return 1;};
		return doneAttributes==-1;
	};

public:
	States(int ss,int rs) : stSeedSet(ss), doneAttributes(0), objects(0), 
					states(0), trules(0), realState(rs) {};
	States() : stSeedSet(0), doneAttributes(0), objects(0), states(0), trules(0), realState(0) {};
	void addstate(int,char *,object *,char *);
	void addReseededState(symbol_table&,char*);
	void showall();
	void extend();
	void addRule(transrule *);
	void doAttributes();
	oblist copyObs();
	oblist filterObs(oblist);
	int isState(){return realState;};
	int orbit(){int x = 0; for(statelist xs = states;xs;xs=xs->next) x++; return x;};
	oblist getObs(){return objects;};
	int contains(char *);
	int myState(){return stSeedSet;};
	void reanalyseOb(int,int,object *);
	void reanalyseOb(object *,char *,int);
	virtual rules getRules() {return trules;};
	int rigidity(char);
	void setObs(oblist os) {objects = os;};

	char * checkForAtt(oState *,char *);

	friend ostream & operator <<(ostream &,States &);

	int onlyStaticEnablers();
};

extern States * allstates;
extern symbol_table tempstates; 
extern OType * alltypes; 


extern char * * collectionpt;
extern int * cps;



class StListNode {
public:
	States * state;
	StListNode * next;

	StListNode(States * st,StListNode * n) : state(st), next(n) {};
};

typedef StListNode * StList;

extern StList specialStates;

class subStates : public States {
private:
	rules srules;
	int type;

public:

	subStates() : States(), srules(0), type(0) {realState = 1;};
	subStates(int x,int r,int tp) : States(x,r), srules(0), type(tp) {};
	void setType(int t){type = t;};
	void setUpRules(States);
	void re_seed_substate(ostream&);
	void extend();
	void markAtt(){realState = 1;};
	rules getRules(){return srules;};

	friend ostream & operator <<(ostream &,subStates &);
};

class SubStListNode {
public:
	subStates * state;
	SubStListNode * next;

	SubStListNode(subStates * st,SubStListNode * n) : state(st), next(n) {};
};

typedef SubStListNode * SubStList;

extern SubStList specialSubStates;

extern OType * alltypes;
extern rules allrules;
extern rules alltransrules;
extern rules alldecrules;

extern subStates * substates;

int isOfType(int * theobtp,int * thevartp);

void analyseSubStates(ostream &,States);

int numSubStates(States);

propptr where(object * ob,plptr ps,char * atrel);
void orderings();

#endif
