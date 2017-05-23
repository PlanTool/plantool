/*
* $Revision: 1.5 $
* $Date: 1999/07/11 22:24:53 $


*/

#ifndef __BASICTIM
#define __BASICTIM

#include <string.h>
#include <stream.h>
#include <stdio.h>
#include "switches.h"
#include "SymTab.h"

#include "alldefs.h"

extern "C" domptr yyparse();
extern "C" void printrels(plptr);
extern "C" void print(oplistptr);

extern "C" predlist allpreds;
extern "C" domptr topdom;
extern "C" void printop(opptr);
extern "C" void printrels(plptr);
extern "C" void printpropn(propptr);
extern "C" plptr allextraprops;



#define MOVEOP 1
#define LOADOP 2
#define UNLOADOP 3
#define ILOADOP 4
#define IUNLOADOP 5

extern "C" predlist allpreds;
extern "C" domptr topdom;

void typestream(ostream &,int);
int subset(char*,char*);
int checkMatch(propptr,propptr);
int unaryPred(char);
void setdiff(char*,char*,char*);
void identityInvariant(ostream &,char,int,int);
int ofType(int*,int);
int arg_in(int,char*,propptr);
int argnum(char*,tpptr);

predlist getPred(char*);
propptr getInrel(plptr,tpptr,predlist);
tpptr getLocName(tpptr,plptr,predlist,char,tpptr);
tpptr findArg(tpptr,propptr);
class object;

class objectlistnode {
public:	object * obj;
	objectlistnode * next;

	objectlistnode(object * o,objectlistnode * n) : obj(o), next(n) {};
};

class object {
private:
	char * name;
	int type[2];
	int Type;

	int inOp;
	plptr initState;
	int numInits;
	plptr goalState;
	int numGoals;

	objectlistnode * symmetryGroup;
	int symGNum;
#ifdef SYMMETRY
	int symIndex;
	int groupsize;
	int symposn;
#endif
	static int symGps;
#ifdef SYMMETRY
	static int numSymObs;
#endif
	void setSym(objectlistnode * s) 
	{ 	symmetryGroup = s; symGNum = symGps;
	};

public:
	object(char * nm,int u) : name(nm), Type(-1), inOp(u), initState(0),
				numInits(0), goalState(0), numGoals(0),
				symmetryGroup(0), symGNum(0)
#ifdef SYMMETRY
						, symIndex(0),groupsize(1), symposn(1)
#endif
					 {type[0] = 0; type[1] = 0;};
	void assertType(int);
	int * getTypeVec() { return type;};
	void setType(int t) {Type = t;};
	int getType() {return Type;};
	char * getName(){return name;};
	friend ostream & operator<<(ostream &,const object &);

	void addInitProp(propptr p) {	plptr tmp = new proplist; 
					tmp->rest = initState;
					tmp->prop = p;
					initState = tmp;
					numInits++;};
	void addGoalProp(propptr p) {	plptr tmp = new proplist; 
					tmp->rest = goalState;
					tmp->prop = p;
					goalState = tmp;
					numGoals++;};
	plptr getInits() {return initState;};
	int getNumInits() {return numInits;};
	plptr getGoals() {return goalState;};
	int getNumGoals() {return numGoals;};
	int isInOp(){return inOp;};
	void addSym(object * o) 
		{symmetryGroup = new objectlistnode(o,symmetryGroup);
#ifdef SYMMETRY
		o->symIndex = ++numSymObs;
		o->symposn = ++groupsize;
#endif
		};
	objectlistnode * getSym() { return symmetryGroup;};
	void setSyms()
		{
#ifdef SYMMETRY
		if(symmetryGroup) symIndex = ++numSymObs;

		for(objectlistnode * os = symmetryGroup;os;os=os->next)
		{	os->obj->setSym(symmetryGroup);
			os->obj->groupsize = groupsize;
		};
#endif
		 symGNum = symGps++;};
	int symmetric(object *);
	void reduceName(){name[0] = '\0';};
	void restoreName(char n){name[0] = n;};
	int howManyGps(){return symGps;};
	int gpNum(){return symGNum;};
#ifdef SYMMETRY
	int howManySymObs(){return numSymObs;};
	int getSymIndex(){return symIndex;};
	int getSymposn(){return symposn;};
	int getGroupsize(){return groupsize;};
#endif
};


typedef objectlistnode * objectlist;

extern objectlist allobs;

class OType {
private:
	int Type;
	int type[2];
	objectlist members;

	oplistptr catalyst;
	int islocation;
	int isPortable;
	int catcount;

public:
	OType(int t) : Type(t), members(0), islocation(0), isPortable(0),
		catalyst(0), catcount(0)
	{	type[0] = 0;
		type[1] = 0;
	};
	void addobject(object * o)
	{
		members = new objectlistnode(o,members);
		type[0] = o->getTypeVec()[0];
		type[1] = o->getTypeVec()[1];
	};
	OType() {};
	int getType(){return Type;};
	int * getTypeVec(){return type;};
	objectlist getObs() {return members;};


	friend ostream & operator << (ostream &,OType &);

};


class oblistnode {
public:
	char * name;
	object * theobject;
	oblistnode * next;

	oblistnode(char * nm,object * obj,oblistnode * n) : name(nm), theobject(obj), next(n) {};
};

typedef oblistnode * oblist;

int indexfor(propptr,oblist);

void sortsym(char *);

extern int numTypes;
extern OType * alltypes;



void analyseOp(opptr); 
int derivedType(OType &,OType &);
predcell * findPred(char *);

#endif 
