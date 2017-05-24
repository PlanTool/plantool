/*
* $Revision: 1.6 $
* $Date: 1999/07/11 22:24:53 $

*/

#include "BasicTim.h"
#include "SymTab.h"
#include <fstream.h>



int numTypes;


char * * collectionpt;
int * cps;



int object::symGps = 1;

/* This function replaces well_typed from instantiation.cc. We should put it in stan.h.
*/

int isOfType(int * theobtp,int * thevartp)
{
	return (((theobtp[0] & thevartp[0]) == thevartp[0]) && 
		((theobtp[1] & thevartp[1]) == thevartp[1]));
};


#ifdef SYMMETRY
int object::numSymObs = 0;
#endif

void sortsym(char * cs)
{
	while(cs[0] != '\0')
	{
		for(char * xs = cs+1;xs[0] != '\0';xs++)
		{
			if(xs[0] < cs[0]) 
			{
				char c = cs[0];
				cs[0] = xs[0];
				xs[0] = c;
			};
		};
		cs++;
	};
};




   

int
checkMatch(propptr pre,propptr del)
{
	for(;pre;pre = pre->as)
	{
		if(strcmp(pre->nm->varname,del->nm->varname)) return 0;

		del=del->as;
	};

	return del==0;
};


int indexfor(propptr p,oblist ls)
{
	int count = 0;
	for(;ls;ls=ls->next)
	{
		if(!strcmp(ls->theobject->getName(),p->nm->varname))
			return count;
		count++;
	};
	return -1;
};




predlist getPred(char * p)
{
	int x = strlen(p)-1;
	char pos = p[x];

	p[x] = '\0';

	predlist ps;

	for(ps = allpreds;ps;ps=ps->next)
		if(!strcmp(ps->name,p)) break;

	p[x] = pos;

	return ps;
};


int
unaryPred(char c)
{	return (getPred(symtab.getsym(c))->numargs==1);
};





int subset(char * xs,char * ys)
{
	int onepassed = 0;

	for(;xs[0] != '\0';xs++)
	{
		for(;ys[0] != '\0';ys++)
			if(ys[0] >= xs[0]){ break;}else{ onepassed = 1;};
		if(xs[0] != ys[0]){break;}
		else{ys++;};
	};
	return xs[0] == '\0' && (ys[0] != '\0' || onepassed);
};



		


int
argnum(char* nm,tpptr vs)
{	for(int i = 0;1;i++)
	{	if (!strcmp(nm,vs->type->varname))
			return i;
		vs=vs->tps;
	};
};

int
arg_in(int pos,char* sv,propptr pre)
{	if(pos==0)
		return !strcmp(sv,pre->as->nm->varname);
	return !strcmp(sv,pre->as->as->nm->varname);
};


void setdiff(char * buff,char * sup,char * sub)
{
	while(sup[0] != '\0')
	{
		while(sup[0] != sub[0])
		{
			buff[0] = sup[0];
			buff++;
			sup++;
		};

		sup++;
		sub++;
	};
};




ostream & operator<<(ostream & cc,const object & o)
{
	cc << o.name << " (" << o.Type << ")";
	return cc;
};

void
object::assertType(int v)
{
	v--;
	int x = v /32;
	unsigned int y = 1 << (v % 32);
	type[x] |= y;

};


typedef objectlistnode * objectlist;

objectlist allobs = 0;


ostream & operator << (ostream & o,OType & x)
{
	o << "Type T"<<x.Type<< " = {";
	for(objectlist xs = x.members;xs;xs=xs->next)
	{	o << xs->obj->getName();
		if(xs->next) o << ",";
	};
	o << "}";
	return o;
};





void
setalltypes()
{
	numTypes = 0;

	for(objectlist obs=allobs;obs;obs=obs->next)
	{
		if(obs->obj->getType() < 0)
		{
			obs->obj->setType(numTypes);
			int * tt = obs->obj->getTypeVec();
			for(objectlist tmp = obs->next;tmp;tmp=tmp->next)
				if(tmp->obj->getType()<0 && 
					tt[0] == tmp->obj->getTypeVec()[0] && 
					tt[1] == tmp->obj->getTypeVec()[1])
						tmp->obj->setType(numTypes);
			numTypes++;
		};
	};

	alltypes = new OType[numTypes];

	for(int i = 0;i<numTypes;i++)
		alltypes[i] = OType(i);

	
	for(objectlist obs=allobs;obs;obs=obs->next)
		alltypes[obs->obj->getType()].addobject(obs->obj);

};

int 
derivedType(OType & a,OType & b)
{
	return ((((a.getTypeVec())[0] & (b.getTypeVec())[0]) == (a.getTypeVec())[0]) &&
		((a.getTypeVec()[1] & b.getTypeVec()[1]) == a.getTypeVec()[1]));
};




int ofType(int * vec,int st)
{
	return vec[--st/32] & (1 << (st % 32));
};



void setSymmetries()
{
	for(objectlist os = allobs;os;os=os->next)
	  if(!os->obj->getSym())
	  {
		for(objectlist ps = os->next;ps;ps=ps->next)
			if(os->obj->symmetric(ps->obj))
				os->obj->addSym(ps->obj);
		os->obj->setSyms();
	  };
};

int
object::symmetric(object * o)
{
	if(numInits != o->numInits || numGoals != o->numGoals || inOp || o->inOp)
		return 0;

	char n1 = name[0];
	char n2 = o->getName()[0];

	name[0] = '\0';
	o->reduceName();

	plptr ps;

	for(ps = o->getInits();ps;ps=ps->rest)
	{
		plptr os = initState;
		for(;os;os=os->rest)
			if(checkMatch(ps->prop,os->prop))
				break;
		if(!os) break;
	};

	if(ps) 
	{
		name[0] = n1;
		o->restoreName(n2);
		return 0;
	};

	for(ps = o->getGoals();ps;ps=ps->rest)
	{
		plptr os = goalState;
		for(;os;os=os->rest)
			if(checkMatch(ps->prop,os->prop))
				break;
		if(!os)	break;
	};

	name[0] = n1;
	o->restoreName(n2);

#ifdef SYMSHOW
	if(!ps) cout << name << " and " << o->getName() << " are symmetric\n";
#endif

	return !ps;


};

void typestream(ostream & o,int sSS)
{
	int x = 0;

	for(int i = 0;i<numTypes;i++)
		if(ofType(alltypes[i].getTypeVec(),sSS))
		{	if(x) o << " U ";
			o << "T" << i;
			x = 1;
		};

	if(!x) o << "(empty)";
};





void identityInvariant(ostream & o,char c,int nc,int ss)
{

	char * prp = symtab.getsym(c);
	int x = strlen(prp)-1;
	char pos = prp[x];

	int ipos = (int) (pos - '0');
	predlist ps = getPred(prp);
	int na = ps->numargs;

	if(na>1)
	{
	   o << "FORALL x:";
	   typestream(o,ss);
	   o << ". ";

	   if(nc==1)
	   {
		for(int y = 1;y<na;y++)
		o << "FORALL y"<<y<<". FORALL z"<<y<<". ";

		o << ps->name << "(";

		int z = 1;
		for(int y = 1;y <= na;y++)
		{	if(y != ipos) 
			{	o << "y" << z++;}
			else
			{	o << "x";};
			if(y<na){ o << ","; } 
			else { o << ")";};
		};

		o << " AND ";

		o << ps->name << "(";

		z = 1;
		for(int y = 1;y <= na;y++)
		{	if(y != ipos) 
			{	o << "z" << z++;}
			else
			{	o << "x";};
			if(y<na){ o << ","; } 
			else { o << ")";};
		};

		o << " => ";
		for(int y = 1;y < na;y++)
		{	o << "y"<<y<< " = z"<<y;
			if(y+1 < na) o << " AND ";
		};
		o << "\n";
	    }
	    else
	    {
		for(int y = 1;y<na;y++)
		 for(int z = 0;z<=nc;z++)
		  o << "FORALL y"<<z<<"_"<<y<<". ";

		for(int yy = 0;yy<=nc;yy++)
		{
		  o << ps->name << "(";

		  int z = 1;
		  for(int y = 1;y <= na;y++)
		  {	if(y != ipos) 
			{	o << "y" <<yy<<"_"<< z++;}
			else
			{	o << "x";};
			if(y<na){ o << ","; } 
			else { o << ")";};
		  };
		  if(yy<nc) o << " AND ";
		};
		o << " => ";
		for(int yy1 = 0;yy1<nc;yy1++)
		{
		  for(int yy2 = yy1+1;yy2<=nc;yy2++)
		  {
			for(int y = 1;y<na;y++)
			{
				o << "y"<<yy1<<"_"<<y<< "=y"<<yy2<<"_"<<y;
				if(y+1<na) o << " AND ";
			};
			if(yy2<nc) o << " OR ";
		  };
		  if(yy1+1 < nc) o << " OR ";
		};
		o << "\n";
	    };


	};
};


int isSubType(int * t1,int * t2)
{
	return ((t1[0] & t2[0]) == t2[0]) && ((t1[1] & t2[1]) == t2[1]);
};




predcell * findPred(char * nm)
{
	predlist ps;
	for(ps = allpreds;strcmp(ps->name,nm);ps=ps->next);
	return ps;
};
