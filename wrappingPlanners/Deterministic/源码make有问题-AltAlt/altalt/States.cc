/*
* $Revision: 1.7 $
* $Date: 1999/07/09 21:48:29 $
*/

#include "SymTab.h"
#include "BasicTim.h"
#include "States.h"

OType * alltypes;
symbol_table tempstates;

States * allstates;
int gotLoop = 0;



StList specialStates = 0;
SubStList specialSubStates = 0;
subStates * substates;


void markAttSpaces()
{
	for(int i = 0;i<256;i++)
		if(attribute_seeds.isMarked(i)) state_seeds.notRealState(i);
};


void processRules(char c)
{
	rules trailer = 0;

	for(rules rs = allrules;rs;rs=rs->next)
		if(rs->rule->splitRule(c))
		{
			if(trailer) 
			{
				trailer->next = rs->next;
// We don't want to delete the rule because it could still be 
// referenced.
//				delete rs->rule;
				delete rs;
				rs = trailer;
			}
			else
			{
				allrules = rs->next;
// Same thing here.
//				delete rs->rule;
				delete rs;
				rs = allrules;
			};
		}
		else
		{
			trailer = rs;
		};
};

void analyseOb(pptr tobj)
{
	propptr args;
	int x;
	char property[50];
	char * obj = tobj->varname;

//	attribute_seeds.unmarkAll();

	allobs = new objectlistnode(new object(obj,tobj->used),allobs);

	for(int i = 0;i<numStates;i++)
		cps[i] = 0;


	for(plptr fs = topdom->initial_state;fs;fs=fs->rest)
	{
		x = 1;

		for(args = fs->prop->as;args;args = args->as)
		{
			if(strcmp(args->nm->varname,obj)==0)
			{	if(args->nm->varname != obj) 	
				{
					delete [] (args->nm->varname);
					args->nm->varname = obj;
				};

				allobs->obj->addInitProp(fs->prop);
				break;
			};
			x++;
		};


		if(args)
		{

			strcpy(property,fs->prop->nm->varname);
			property[strlen(fs->prop->nm->varname)+1] = '\0';
			property[strlen(fs->prop->nm->varname)] = 48+(char) x;
			char v = symtab.enter(property);
			x = cnv(v);
//			attribute_seeds.markAttribute(x);
			x = state_seeds.statefor(x);


			if(x) collectionpt[x][cps[x]++] = v;
		};


	};


	for(x = 0;x<numStates;x++)
	{
		collectionpt[x][cps[x]] = '\0';
		allstates[x].addstate(x,obj,allobs->obj,collectionpt[x]);

// NEW LINE FOR GOAL STATE ANALYSIS

		cps[x] = 0;
	};

	for(plptr fs = topdom->goal_state;fs;fs=fs->rest)
	{
// NEW LINE FOR GOAL STATE ANALYSIS

		x = 1;

		for(args = fs->prop->as;args;args = args->as)
		{
			if(strcmp(args->nm->varname,obj)==0)
			{	if(args->nm->varname != obj) 	
				{
					delete [] (args->nm->varname);
					args->nm->varname = obj;
				};
				allobs->obj->addGoalProp(fs->prop);
				break;
			};
// NEW LINE FOR GOAL STATE ANALYSIS
			x++;
		};

// NEW CODE FOR GOAL STATE ANALYSIS

		if(args)
		{

			strcpy(property,fs->prop->nm->varname);
			property[strlen(fs->prop->nm->varname)+1] = '\0';
			property[strlen(fs->prop->nm->varname)] = 48+(char) x;
			char v = symtab.enter(property);
			x = cnv(v);
			x = state_seeds.statefor(x);


			if(x) collectionpt[x][cps[x]++] = v;
		};
	};

#ifdef STANSHOW
	for(x = 1;x<numRStates;x++)
	{
		if(cps[x]==0 && ofType(allobs->obj->getTypeVec(),x))
		{
			cout << obj << " has " << x << " state unspecified\n";
			allstates[x].showall(); cout << "\n";};
	};
#endif


};

void goalStateAnalysis()
{
	propptr args;
	int x;
	char property[50];

	for(objectlist os = allobs;os;os=os->next)
	{

		for(int i = 0;i<numStates;i++)
			cps[i] = 0;

		for(plptr gs = os->obj->getGoals();gs;gs=gs->rest)
		{
			x = 1;
			for(args = gs->prop->as;args;args = args->as)
			{
				if(strcmp(args->nm->varname,os->obj->getName())==0) break;

				x++;
			};

			if(args)
			{

				strcpy(property,gs->prop->nm->varname);
				property[strlen(gs->prop->nm->varname)+1] = '\0';
				property[strlen(gs->prop->nm->varname)] = 48+(char) x;
				char v = symtab.enter(property);
				x = cnv(v);
				x = state_seeds.statefor(x);


				if(x) cps[x]++;
			};
		};

		for(x = 1;x<numRStates;x++)
		{
			if(cps[x]==0 && ofType(os->obj->getTypeVec(),x))
			{	cout << os->obj->getName() << " has " << x << " state unspecified\n";
				allstates[x].showall(); cout << "\n";};
		};
	};
};


	

int ordered(int i,int j)
{
	for(rules rs = allrules;rs;rs=rs->next)
	{
		if(rs->rule->ordered(i,j)) return 1;
	};
	return 0;
};

void orderings()
{
	for(int i = 1;i<numStates-1;i++)
		for(int j = i+1;j<numStates;j++)
		{
			if(ordered(i,j))
				cout << "State " << i << " < State " << j << "\n";

			if(ordered(j,i))
				cout << "State " << j << " < State " << i << "\n";
		};
};

void existStream(ostream & o,char * prp)
{
	int last = strlen(prp)-1;
	char pos = prp[last];
	int ipos = (int) (pos-'0');

	predlist ps = getPred(prp);

	int na = ps->numargs;

	int arg = 1;
	char argC = '1';

	for(int i = 1;i<=na;i++)
	{
		if(i != ipos)
		{
			o << "Exists y"<<arg++ << ":";
			prp[last] = argC;
			char c = symtab.enter(prp);
			typestream(o,state_seeds.statefor(cnv(c)));
			o << ". ";
		};
		argC++;
	};
	prp[last] = pos;

	o << ps->name << "(";

	int z = 1;
	for(int y = 1;y <= na;y++)
	{	if(y != ipos) 
		{	o << "y" << z++;}
		else
		{	o << "x";
		};
		if(y<na){ o << ","; } 
		else { o << ")";};
	};

};

void outState(ostream & o,char * st)
{
	char cs[strlen(st)+1];

	int j = 1;

	cs[0] = st[0];

	for(unsigned int i = 1;i<strlen(st);i++)
		if(st[i] != st[i-1]) cs[j++] = st[i];
	
	cs[j] = '\0';

	if(strlen(cs)>1) o << "(";

	for(unsigned int i = 0;i<strlen(cs);i++)
	{
		existStream(o,symtab.getsym(cs[i]));
		if(i+1 < strlen(cs)) o << " AND ";
	};
	if(strlen(cs)>1) o << ")";
};

ostream & operator <<(ostream & o,States & s)
{
#ifdef TIM_DEBUG
	s.showall();
	for(rules rs = s.trules;rs;rs = rs->next)
		rs->rule->showrule();
#endif
 if(s.objects)
 {
  if(s.isState())
  {

	for(int i = 1;i<256;i++)
	  if(state_seeds.statefor(i) == s.stSeedSet)
	  {
		int xc = 0;

		for(statelist ss = s.states;ss;ss=ss->next)
		{
			int lc = 0;
			char * cs = ss->os.getState();
			for(unsigned int j = 0;j<strlen(cs);j++)
				if(cs[j] == (char) i) lc++;
			xc = lc>xc?lc:xc;
		};

		identityInvariant(o,(char) i,xc,s.stSeedSet);
	  };


	o << "FORALL x:";
	typestream(o,s.stSeedSet);

	o << ". (";
	int xc = 0;
	for(statelist ss = s.states;ss;ss=ss->next)
	{
	    if(!ss->os.isSuper())
	    {	if(xc) o << " OR ";
		xc++;

		outState(o,ss->os.getState());
	    };

	};
	o << ")\n";

	for(statelist ss = s.states;ss->next;ss=ss->next)
	{
	  if(!ss->os.isSuper())
	  {
		for(statelist ss1 = ss->next;ss1;ss1=ss1->next)
		{
		   if(!ss1->os.isSuper())
		   {
		     	if(ss->os.isSub() && ss1->os.isSub())
		     	{	statelist ss2;
				for(ss2 = s.states;ss2;ss2=ss2->next)
					if(ss2->os.isSuper() 
							&&
						subset(ss1->os.getRedState(),
								ss2->os.getState())
							&& 
					   	subset(ss->os.getRedState(),
								ss2->os.getState()))
							break;
				if(ss2) break;
			};

			o << "FORALL x:";
			typestream(o,s.stSeedSet);

			o << ". NOT (";

			outState(o,ss->os.getState());
	
			o << " AND ";

			outState(o,ss1->os.getState());

			o << ")\n";
		    };
		};
	    };
	};		
   }
   else
   {
     if(s.stSeedSet < numRStates)
     {
	o << "Objects, x, in ";
	typestream(o,s.stSeedSet);
	o << " can have property: ";
	for(int i = 0;i<256;i++)
		if(state_seeds.statefor(i) == s.stSeedSet)
		{	existStream(o,symtab.getsym((char) i));
			o << "; ";
		};

     }
     else
     {
	o << "Objects, x, in ";
	typestream(o,s.stSeedSet);
	o << " all have property: ";
	for(int i = 0;i<256;i++)
		if(state_seeds.statefor(i) == s.stSeedSet)
		{	existStream(o,symtab.getsym((char) i));
			o << "; ";
		};

     };
   };



  };

   return o;
};


void
analyseOp(opptr op)
{
	char addels[100];
	char precels[100];
	char enablers[100];
	char sym[100];

	int a, p, e;

	propptr args;
	char apos;

	for(tpptr vs = op->vars;vs;vs=vs->tps)
	{
		a = 0;
		p = 0;
		e = 0;
		for(plptr dels = op->dels;dels;dels = dels->rest)
		{
			apos = '0';
			for(args = dels->prop->as;args;args=args->as)
			{	apos++;
				plptr pres;
				if(strcmp(vs->type->varname,args->nm->varname)==0)
				{  for(pres = op->precs;pres;pres = pres->rest)
					if(checkMatch(pres->prop,dels->prop))
					{
						strcpy(sym,pres->prop->nm->varname);
						sym[strlen(pres->prop->nm->varname)] = apos;
						sym[strlen(pres->prop->nm->varname)+1] = '\0';
						precels[p++] = symtab.enter(sym);
						break;
					};
				};
			};
		};


		for(plptr ps = op->precs;ps;ps=ps->rest)
		{
			apos = '0';
			for(args = ps->prop->as;args;args=args->as)
			{
				apos++;
				if(strcmp(vs->type->varname,args->nm->varname)==0)
				{
					strcpy(sym,ps->prop->nm->varname);
					sym[strlen(ps->prop->nm->varname)] = apos;
					sym[strlen(ps->prop->nm->varname)+1] = '\0';
					enablers[e++] = symtab.enter(sym);
					break;
				};
			};
		};



		enablers[e] = '\0';
		
		if(p)
		{

			for(plptr adds = op->adds;adds;adds = adds->rest)
			{
				apos = '0';
				for(args = adds->prop->as;args;args=args->as)
				{	apos++;
					if(strcmp(vs->type->varname,args->nm->varname)==0)
					{
						strcpy(sym,adds->prop->nm->varname);
						sym[strlen(adds->prop->nm->varname)] = apos;
						sym[strlen(adds->prop->nm->varname)+1] = '\0';
						addels[a++] = symtab.enter(sym);


/* If the newly found add element is matched by a precel element then we have to
split off the rule a->a, and remove the a element from the precels */

						for(int i = 0;i<p;i++)
							if(precels[i] == addels[a-1])
							{
								char tmp[2] = {precels[i], '\0'};
								allrules = new rulelistnode(new transrule(tmp,tmp,enablers,op,vs),allrules);
								a--;
								for(int j = i;j<p;j++)
									precels[j] = precels[j+1];
								p--;
								break;
							};




						break;
					};
				};
			};

			if(p&&a)
			{
				precels[p] = '\0';
				addels[a] = '\0';

				transrule * t = new transrule(precels,addels,enablers,op,vs);
				allrules = new rulelistnode(t,allrules);
			}
			else {if(p)
			{
				char en[100];
				char dd[2];
				dd[1] = '\0';
				precels[p] = '\0';
				

				for(int k = 0;k<p;k++)
				{
/* DEAL WITH REDUCING RULES */
					attribute_seeds.markAttribute(cnv(precels[k]));

					int i = 0;
					for(;i<e;i++)
					{
						if(enablers[i] == precels[k]) break;
						en[i] = enablers[i];
					};

					strcpy(en+i,enablers+i+1);

					sortsym(en);
					dd[0] = precels[k];
					alldecrules = new rulelistnode(new transrule(op,vs,dd,en),alldecrules);

					i = cnv(precels[k]);
					attribute_seeds.markReducer(i);

#ifdef STANSHOW
					cout << "Decreasing rule: \n";
					alldecrules->rule->showrule();
#endif

#ifdef STANSHOW
					symtab.showsym(precels[k]);
					cout << " ";
#endif
				};
#ifdef STANSHOW
				cout << "are attributes\n";
#endif
			}
			else if(a)
			{
				char tmp[2] = {'\0','\0'};
				for(int i = 0;i<a;i++)
				{	tmp[0] = addels[i];
					alltransrules = new rulelistnode(new transrule(tmp,enablers,op,vs),alltransrules);
#ifdef STANSHOW
					cout << "Rule from ";
					printop(op);
					alltransrules->rule->showrule();
#endif
				};
			};};

		}
		else
		{

			for(plptr adds = op->adds;adds;adds = adds->rest)
			{
				apos = '0';
				for(args = adds->prop->as;args;args=args->as)
				{	apos++;
					if(strcmp(vs->type->varname,args->nm->varname)==0)
					{
						strcpy(sym,adds->prop->nm->varname);
						sym[strlen(adds->prop->nm->varname)] = apos;
						sym[strlen(adds->prop->nm->varname)+1] = '\0';
						addels[0] = symtab.enter(sym);
						addels[1] = '\0';

						int i;
						for(i = 0;i<e;i++)
						   if(addels[0]==enablers[i]) break;

						if(i==e)
						{  alltransrules = new rulelistnode(new transrule(addels,enablers,op,vs),alltransrules);
#ifdef STANSHOW
						   cout << "Rule from ";
						   printop(op);
						   alltransrules->rule->showrule();
#endif
						};
						

						break;
					};
				};
			};
		};

	};
};





symbol_table oState::reducedState;

oState::oState(char c,int p,int s,int init) : state(c), parent(p), initial(init)
{	
	if(s == -1){ subset = 1; superset = 0;}
	else{	subset = 0;
		superset = s;};


	char * st = statetab.getsym(c);
	char cs[strlen(st)+1];

	int j = 1;

	cs[0] = st[0];

	for(unsigned int i = 1;i<strlen(st);i++)
		if(st[i] != st[i-1]) cs[j++] = st[i];
	
	cs[j] = '\0';

	reduced = reducedState.enter(cs);

};

void
oState::show()
{
	char * cs = statetab.getsym(state);

	int i = 1;
	cout << "{";
	symtab.showsym(cs[0]);

	while(cs[i] != '\0')
	{
		cout << ", ";
		symtab.showsym(cs[i]);
		i++;
	};
	cout << "}";

	if(superset) cout << "(S) ";
	if(subset) cout << "[s]";
	if(initial) cout << "*";
};



int
States::contains(char * cs)
{

	int x = 0;
	for(statelist ss = states;ss;ss=ss->next)
	{
		if(subset(ss->os.getRedState(),cs)){x = 1;ss->os.markSub();}
		else
		{
			if(!ss->os.isSuper())
			{
				char ds[strlen(cs)+1];
				int j = 1;
				ds[0] = cs[0];
				for(unsigned int i = 1;i<strlen(cs);i++)
					if(cs[i] != cs[i-1]) ds[j++] = cs[i];
				ds[j] = '\0'; 
				if(subset(ds,ss->os.getState()))
				{	ss->os.markSuper();
					x = -1;};
			};
		};
	};
	return x;
};


char *
States::checkForAtt(oState * optr,char * st)
{
#ifdef TIM_DEBUG
	cout << "Checking\n";
	cout << "\n";
	char * cs = st;
	int i = 1;
	cout << "{";
	symtab.showsym(cs[0]);

	while(cs[i] != '\0')
	{
		cout << ", ";
		symtab.showsym(cs[i]);
		i++;
	};
	cout << "}\n";
#endif

	statelist ss = states;
	statelist ss1 = 0;

	for(;&(ss->os) != optr;ss=ss->next)
		if(ss->os.isInitial()) ss1 = ss;

	if(ss->os.isInitial()) ss1 = ss;

	ss = ss1;

	for(;ss && (ss == ss1 || !ss->os.isInitial());ss=ss->next)
	{
#ifdef TIM_DEBUG
		ss->os.show(); cout << "\n";
		if(ss->next) cout << "Next one: " << ss->next->os.isInitial() << " " << (&(ss->os) == optr) << "\n";
#endif
		if(subset(ss->os.getState(),st)){ realState = 0; return ss->os.getState();};
	};
	return 0;
};


void
States::addRule(transrule * r)
{
	trules = new rulelistnode(r,trules);
};

void
States::showall()
{
	cout << "\nState " << stSeedSet << "\n";
	for(oblist os = objects;os;os=os->next)
		cout << os->name << " ";
	cout << "\n";
	for(statelist s = states;s;s=s->next)
	{
		s->os.show();
		cout << " ";
	};
	cout << "\n";
};

int 
States::rigidity(char c)
{
	if(!realState) return 0;

	statelist s = states;
	for(;s;s=s->next)
	{
		int xc = 0;
		for(char * cs = s->os.getState();cs[0] != '\0';cs++)
			if(cs[0] == c) xc++;

		if(xc>1) break;
	};

	return s==0;
};


int
States::onlyStaticEnablers()
{
	rules rs;
	for(rs = trules;rs;rs=rs->next)
		if(!rs->rule->onlyStaticEnablers()) break;

	return !rs;

};

char restart[40];

int
oState::addnewstates(States* ste,rules rles,statelistnode * src)
{
	for(rules rls = rles;rls;rls=rls->next)
	{
		char c = rls->rule->trySubRule(statetab.getsym(state));
		if(c != '\0')
		{
		  int x = ste->contains(statetab.getsym(c));

		  if(x==1)
		  { 	char * st  = ste->checkForAtt(this,statetab.getsym(c));
			if(st)
			{	int d = strlen(statetab.getsym(c)) - strlen(st);
								
				setdiff(restart,statetab.getsym(c),st);

				for(int i = 0; i < d; i++)
				{
					if(i==0 || restart[i] != restart[i-1])
					{	
#ifdef STANSHOW
						cout << symtab.getsym(restart[i]) << " is attribute\n";
#endif
						processRules(restart[i]);
					};

				};

#ifdef TIM_DEBUG
				for(rules rs = allrules;rs;rs=rs->next)
					rs->rule->showrule();
				for(rules rs = alltransrules;rs;rs=rs->next)
					rs->rule->showrule();
#endif

		  		return d;

			};
		    };

	 	  src->next = new statelistnode(oState(c,parent,x,0),src->next);
		};
	};

	return 0;
};

void 
States::reanalyseOb(object * ob,char * bs,int cb)
{
	char property[100];
	char istate[100];
	int ist = 0;

	for(plptr is = ob->getInits();is;is=is->rest)
	{
		int arg = 1;
		for(propptr args = is->prop->as;args;args=args->as)
		{
			if(args->nm->varname == ob->getName()) break;
			arg++;
		};
		strcpy(property,is->prop->nm->varname);
		property[strlen(is->prop->nm->varname)+1] = '\0';
		property[strlen(is->prop->nm->varname)] = 48 + (char) arg;

		char v = symtab.enter(property);
		int vv = cnv(v);

		for(int i = 0;i<cb;i++)
			if(v == bs[i]) vv = 0;

		if(vv && state_seeds.statefor(vv) == stSeedSet) 
			istate[ist++] = v;
	};

	istate[ist] = '\0';

	if(ist>0)
	{
		sortsym(istate);
		char c = tempstates.newenter(istate);

		if(c != '\0')
		{
		  c = statetab.enter(istate);
		  oState os(c,stSeedSet,contains(istate),1);
		  states = new statelistnode(os,states);
		};
		  
		objects = new oblistnode(ob->getName(),ob,objects);
		
	};

};


void
States::extend()
{
    if(realState)
    {	int x = 1;
	statelist orig = states;
	oblist origobs = objects;
	int cbad = 0;
	char allbad[50];

	while(x)
	{
		x = 0;

		for(statelist sts = states;sts;sts=sts->next)
		{
			x = sts->os.addnewstates(this,allrules,sts);
// Can't use trules instead of allrules here for some reason. Have the rules
// not been distributed yet?

			if(x) break;
		};

		if(x)
		{
			for(int i = 0;i<x;i++)
				allbad[cbad++] = restart[i];

			if(orig != states) 
			{	while(states)
				{
					statelist tmp = states;
					states = states->next;
					delete tmp;
				};
			}
			else
			{
				states = 0;
			};

			if(origobs != objects)
			{	while(objects)
				{
					oblist tmp = objects;
					objects = objects -> next;
					delete tmp;
				};
			}
			else
			{
				objects = 0;
			};

			tempstates.wipe();

			for(oblist os = origobs;os;os=os->next)
				reanalyseOb(os->theobject,allbad,cbad);
		};
	};

	if(orig != states)
	{
		States * nstptr = new States(stSeedSet,1);
		nstptr->objects = objects;
		objects = origobs;
		nstptr->states = states;
		states = orig;
		
		specialStates = new StListNode(nstptr,specialStates);

#ifdef TIM_DEBUG
		cout << "Added new state from:\n";
		showall();
		cout << "\nwhich is:\n";
		specialStates->state->showall();
#endif

	};
    };
	
    gotLoop = 0;

    if(!doneAttributes)
		doAttributes();
};


void
States::doAttributes()
{
  while(doneAttributes <= gotLoop)
  {
	doneAttributes = gotLoop+1;



	for(rules rs = trules;rs;rs=rs->next)
	{
		char * xs = rs->rule->getEnablers();
		if(xs[0] != '\0')
		{	for(;xs[0] != '\0';xs++)
		  	 if(!allstates[state_seeds.statefor(cnv(xs[0]))].checkAttributes())
				allstates[state_seeds.statefor(cnv(xs[0]))].doAttributes();

			oblist os = allstates[state_seeds.statefor(cnv(rs->rule->getEnablers()[0]))].copyObs();

			for(xs = rs->rule->getEnablers()+1;xs[0] != '\0';xs++)
			   os = allstates[state_seeds.statefor(cnv(xs[0]))].filterObs(os);	

			addobs(os);
		}
		else
		{
			addobs(allstates[0].copyObs());
		};
	};
  };

/* This setting avoids reentry into a loop which is now completely analysed */

  doneAttributes = -1;

};


oblist
States::copyObs()
{
	oblist nos = 0;

	for(oblist os = objects;os;os=os->next)
		nos = new oblistnode(os->name,os->theobject,nos);

	return nos;
};


oblist
States::filterObs(oblist os)
{
	oblist prev = 0;
	oblist ns = os;
	oblist tmp;

	while(os)
	{
		for(tmp = objects;tmp;tmp=tmp->next)
			if(tmp->name==os->name)
				 break;

		if(tmp)
		{
			if(prev)
			{	prev->next = os;
				prev = os;
				os = os->next;
			}
			else
			{	ns = os;
				prev = ns;
				os = os->next;
			};
		}
		else
		{
			if(prev)
			{
				prev->next = os->next;
			}
			else
			{
				ns = os->next;
			};

			tmp = os;
			os = os->next;
			delete tmp;
		};
	};

	return ns;
};

void
States::addobs(oblist os)
{
	int inc = 0;
	while(os)
	{
		oblist ps;
		for(ps = objects;ps;ps=ps->next)
			if(ps->name == os->name) break;
		if(!ps)
		{
			ps = os->next;
			os->next = objects;
			objects = os;
			os->theobject->assertType(stSeedSet);
			os = ps;
			inc = 1;
		}
		else
		{
			ps = os->next;

			delete os;
			os = ps;
		};
	};
	gotLoop += inc;
};



void
States::addstate(int x,char * ob,object * obj,char * cs)
{
	if(x)
	{

		if(strlen(cs)>0)
		{
			sortsym(cs);
		        char c = statetab.newenter(cs);

			if(c != '\0')
			{
			  oState os(c,stSeedSet,contains(cs),1);
		 	  states = new statelistnode(os,states);

			  tempstates.enter(cs);

			};
			objects = new oblistnode(ob,obj,objects);
			obj->assertType(x);

		}
/*		else
		{ if(trules)
			for(rules trs = trules;trs;trs=trs->next)
				if(trs->rule->enabled())
				{	objects = new oblistnode(ob,obj,objects);
					obj->assertType(x);
					break;
				};
		};
*/
	}
	else
	{
		objects = new oblistnode(ob,obj,objects);
	};

};


void
States::addReseededState(symbol_table& tmpstatetab,char * cs)
{
	sortsym(cs);
	char c = tmpstatetab.newenter(cs);

	if(c != '\0')
	{
		statetab.newenter(cs);
	 	oState os(c,stSeedSet,contains(cs),1);
		states = new statelistnode(os,states);
	};
};


void
subStates::extend()
{
    if(realState)
    {	int x = 1;
	statelist orig = states;
	oblist origobs = objects;
	int cbad = 0;
	char allbad[50];

	while(x)
	{
		x = 0;

		for(statelist sts = states;sts;sts=sts->next)
		{	x = sts->os.addnewstates(this,srules,sts);

			if(x) break;
		};

		if(x)
		{
			for(int i = 0;i<x;i++)
				allbad[cbad++] = restart[i];

			if(orig != states) 
			{	while(states)
				{
					statelist tmp = states;
					states = states->next;
					delete tmp;
				};
			}
			else
			{
				states = 0;
			};

			if(origobs != objects)
			{	while(objects)
				{
					oblist tmp = objects;
					objects = objects -> next;
					delete tmp;
				};
			}
			else
			{
				objects = 0;
			};

			tempstates.wipe();

			for(oblist os = origobs;os;os=os->next)
				reanalyseOb(os->theobject,allbad,cbad);
		};
	};

	if(orig != states)
	{
		subStates * nstptr = new subStates();
		nstptr->objects = objects;
		objects = origobs;
		nstptr->states = states;
		states = orig;
		
		specialSubStates = new SubStListNode(nstptr,specialSubStates);

#ifdef TIM_DEBUG
		cout << "Added new state from:\n";
		showall();
		cout << "\nwhich is:\n";
		specialSubStates->state->showall();
#endif

	};
    };
	
    gotLoop = 0;
};

/* CHECK WHAT HAPPENS TO DECREASING RULES: IF THEY ARE SIMPLY DISCARDED
THEN WE HAVE A PROBLEM IN DETERMINING WHICH ARE THE ATTRIBUTE ELEMENTS 
IN THE ATTRIBUTE SPACE - PERHAPS WE CAN DO IT BY EXAMINING THE ATTRIBUTE SEEDS
TO SEE WHICH ELEMENTS ARE EXPLICITLY MARKED AS ATTRIBUTES? */

/* CURRENTLY WE IGNORE THIS PROBLEM! */


void
subStates::setUpRules(States p)
{
	for(rules rs = p.getRules();rs;rs=rs->next)
	{
		if(rs->rule->isEnabled(type))
			if(rs->rule->isAttRule())
			{
				trules = new rulelistnode(rs->rule,trules);
				realState = 0;
			}
			else
			{
				srules = new rulelistnode(rs->rule,srules);
			};
	};

};




void 
States::reanalyseOb(int tp,int sbst,object * ob)
{
	char property[100];
	char istate[100];
	int ist = 0;

	for(plptr is = ob->getInits();is;is=is->rest)
	{
		int arg = 1;
		for(propptr args = is->prop->as;args;args=args->as)
		{
			if(args->nm->varname == ob->getName()) break;
			arg++;
		};
		strcpy(property,is->prop->nm->varname);
		property[strlen(is->prop->nm->varname)+1] = '\0';
		property[strlen(is->prop->nm->varname)] = 48 + (char) arg;

		char v = symtab.enter(property);
		int vv = cnv(v);
		if(state_seeds.statefor(vv) == stSeedSet) 
		{	istate[ist++] = v;
			if(attribute_seeds.isReducer(vv)) 
				for(rules rs = alldecrules;rs;rs=rs->next)
					if(rs->rule->isRelevant(v) &&
						rs->rule->isEnabled(tp))
						substates[sbst].markAtt();
		};
	};

	istate[ist] = '\0';


	if(ist>0)
	{
		sortsym(istate);
		char c = tempstates.newenter(istate);
		if(c != '\0')
		{
		  c = statetab.enter(istate);
		  oState os(c,stSeedSet,substates[sbst].contains(istate),1);
		  substates[sbst].states = new statelistnode(os,substates[sbst].states);
		};
		  
		substates[sbst].objects = new oblistnode(ob->getName(),ob,substates[sbst].objects);
		
	};

	substates[sbst].stSeedSet = stSeedSet;
	substates[sbst].setType(tp);

};

void analyseSubState(ostream & o,States s,int tp,int sbst)
{
	tempstates.wipe();

	for(objectlist os = alltypes[tp].getObs();os;os=os->next)
	{
		if(os->obj->getType() == tp)
			s.reanalyseOb(tp,sbst,os->obj);
	};

	substates[sbst].setUpRules(s);
	substates[sbst].extend();

	o << substates[sbst] << "\n";
};

void analyseSubStates(ostream & o,States s)
{
	int nss = numSubStates(s);

	if(nss>1)
	{
		substates = new subStates[nss];

		for(int i = 0;i<nss;i++)
		{	analyseSubState(o,s,types[i],i);
			substates[i].re_seed_substate(o);
		};
	};
};

/* Re_seed_substate takes the substate built by analyseSubState and does reuniting on the rules to see whether further splitting can be done. This might result in 2 or more new subspaces for the single type associated with the substate. */

void 
subStates::re_seed_substate(ostream& o)
{
	stateSeeds sts;
	for(rules rs=srules;rs;rs=rs->next)
	{
		rs->rule->unite(sts);

	};
	for(rules rs=trules;rs;rs=rs->next)
	{
		rs->rule->unite(sts);
	};

	int n = sts.countStates();
	if (n==1) return;
	subStates * subs = new subStates[n+1];
	sts.setupReseededs(subs,type);
	for(rules rs=srules;rs;rs=rs->next)
	{
		subs[rs->rule->addToState(sts)].addRule(rs->rule);
	};
	for(rules rs=trules;rs;rs=rs->next)
	{
		subs[rs->rule->addToState(sts)].addRule(rs->rule);
	};
	
/* This allocates the rules to the subspaces. Now we filter the initial
state properties through the used properties for each subspace */
	
	for(int i = 1;i<n+1;i++)
	{
		subs[i].srules = subs[i].States::getRules();
		subs[i].setObs(objects);
	};

/* Now the objects have been added to the new subspaces. The next step is to add the initial conditions to the spaces. */

	tempstates.wipe();
	char ** collpt = new char*[n+1];
	int * collptcounts = new int[n+1];
	for(int i=1;i<n+1;i++)
	{
		collpt[i] = new char[30];

/* 30 is the max number of properties associated with an object in the initial state (by assumption!) */	
	};
	for(statelist ptr=states;ptr;ptr=ptr->next)
	{
		if(ptr->os.isInitial())
		{
			for(int i = 1;i<n+1;i++)
			{
				collptcounts[i] = 0;
			};

			for(char* cptr=ptr->os.getState();*cptr!='\0';cptr++)
			{
				int indx = sts.statefor(cnv(*cptr));
				if (indx)
				{	collpt[indx][collptcounts[indx]++] = *cptr;
				};
			};

			for(int i = 1;i<n+1;i++)
			{
				if(collptcounts[i])
				{
					collpt[i][collptcounts[i]] = '\0';
					subs[i].addReseededState(tempstates,collpt[i]);


					collptcounts[i] = 0;
				};
			};
		};
	};

/* Now tidy up memory claims. */

	for(int i = 1;i<n+1;i++)	delete [] collpt[i];
	delete [] collpt;
	delete [] collptcounts;
	

/* Now have to go through these states we have built doing the extension and final analysis. */

	for(int i =1;i<n+1;i++)
	{	
		subs[i].extend();
		o << subs[i];
	};
	
	delete [] subs;				


			
};

void statics()
{
	char tmp[50];

/* THIS LINE IS USED TO CORRECT THE init BY REMOVING THE DUMMY 
PROPOSITIONS INTRODUCED WHEN OPERATOR CONSTANTS WERE ABSTRACTED. 
NB: THE MEMORY THEY OCCUPY IS CURRENTLY NOT RECLAIMED!! */

	topdom->initial_state = allextraprops;


	for(plptr fs = topdom->initial_state;fs;fs=fs->rest)
	{
		strcpy(tmp,fs->prop->nm->varname);
		tmp[strlen(tmp)+1] = '\0';
		tmp[strlen(tmp)] = '1';
		char c = symtab.enter(tmp);
		if(state_seeds.statefor(cnv(c))>=numRStates)
		{
			plptr tmpp = new proplist;
			tmpp->prop = fs->prop;
			tmpp->rest = topdom->statics;
			topdom->statics = tmpp; 
		};
	};

	for(oplistptr ops = topdom->ops;ops;ops=ops->ops)
	{	plptr prev = 0;

/* THIS LINE TO READJUST OPERATORS BY REMOVING THE STATICS INTRODUCED IN
ABSTRACTION OF CONSTANTS */

		if(ops->op->precs == ops->op->statics) ops->op->precs = 0;

		plptr oldstatics = ops->op->statics;

		for(plptr ps = ops->op->precs;ps;)
		{

/* THIS LINE TO READJUST OPERATORS BY REMOVING THE STATICS INTRODUCED IN
ABSTRACTION OF CONSTANTS */

			if(ps->rest == oldstatics) ps->rest = 0;

			char c;
			strcpy(tmp,ps->prop->nm->varname);
			tmp[strlen(tmp)+1] = '\0';
			int pos = strlen(tmp);

			char a = '0';
			for(propptr qs = ps->prop->as;qs;qs=qs->as)
			{
				tmp[pos] = ++a;
				c = symtab.enter(tmp);
				int v = state_seeds.statefor(cnv(c));
				if(v)
				{	v--;
					int x = v /32;
					unsigned int y = 1 << (v % 32);
					qs->nm->type[x] |= y;
				};

/* The following line added 8/7/99 to mark parameters which appear in 
preconditions. */


				qs->nm->isInPre = 1;
			};

			tmp[pos] = a;
			c = symtab.enter(tmp);


			if(state_seeds.statefor(cnv(c))>=numRStates)
			{
			   if(prev)
			   {	prev->rest = ps->rest;
				ps->rest = ops->op->statics;
				ops->op->statics = ps;
				ps = prev->rest;
			   }
			   else
			   {
				ops->op->precs = ps->rest;
				ps->rest = ops->op->statics;
				ops->op->statics = ps;
				ps = ops->op->precs;
			   };
			}
			else
			{
				prev = ps;
				ps=ps->rest;
			};
		};
	};
};

/* SOME NEW STUFF FOR STATE SPLITTING ANALYSIS */

void subIdentityInvariant(ostream & o,char c,int nc,int tp)
{

	char * prp = symtab.getsym(c);
	int x = strlen(prp)-1;
	char pos = prp[x];

	int ipos = (int) (pos - '0');
	predlist ps = getPred(prp);
	int na = ps->numargs;

	if(na>1)
	{
	   o << "FORALL x:T"<<tp <<". ";

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


ostream & operator <<(ostream & o,subStates & s)
{
#ifdef TIM_DEBUG
	s.showall();
	for(rules rs = s.trules;rs;rs = rs->next)
		rs->rule->showrule();

	o << "More rules\n";

	for(rules rs = s.srules;rs;rs = rs->next)
		rs->rule->showrule();
#endif
  if(s.realState)
  {

	for(int i = 1;i<256;i++)
	  if(state_seeds.statefor(i) == s.stSeedSet)
	  {
		int xc = 0;

		for(statelist ss = s.states;ss;ss=ss->next)
		{
			int lc = 0;
			char * cs = ss->os.getState();
			for(unsigned int j = 0;j<strlen(cs);j++)
				if(cs[j] == (char) i) lc++;
			xc = lc>xc?lc:xc;
		};

		if(xc) subIdentityInvariant(o,(char) i,xc,s.type);
	  };






	o << "FORALL x:T" << s.type << ". (";

	int xc = 0;
	for(statelist ss = s.states;ss;ss=ss->next)
	{
	    if(!ss->os.isSuper())
	    {	if(xc) o << " OR ";
		xc++;

		outState(o,ss->os.getState());
	    };

	};
	o << ")\n";

	for(statelist ss = s.states;ss->next;ss=ss->next)
	{
	  if(!ss->os.isSuper())
	  {
		for(statelist ss1 = ss->next;ss1;ss1=ss1->next)
		{
		   if(!ss1->os.isSuper())
		   {
		     	if(ss->os.isSub() && ss1->os.isSub())
		     	{	statelist ss2;
				for(ss2 = s.states;ss2;ss2=ss2->next)
					if(ss2->os.isSuper() 
							&&
						subset(ss1->os.getRedState(),
								ss2->os.getState())
							&& 
					   	subset(ss->os.getRedState(),
								ss2->os.getState()))
							break;
				if(ss2) break;
			};

			o << "FORALL x:T" << s.type << ". NOT (";

			outState(o,ss->os.getState());
	
			o << " AND ";

			outState(o,ss1->os.getState());

			o << ")\n";
		    };
		};
	    };
	};		
   };

 /* SKIP ATTRIBUTE SPACES ON SUBSPACE ANALYSIS: NOT ENOUGH TO SAY 

  else
   {
     if(s.stSeedSet < numRStates)
     {
	o << "Objects, x, in ";
	typestream(o,s.stSeedSet);
	o << " can have property: ";
	for(int i = 0;i<256;i++)
		if(state_seeds.statefor(i) == s.stSeedSet)
		{	existStream(o,symtab.getsym((char) i));
			o << "; ";
		};

     }
     else
     {
	o << "Objects, x, in ";
	typestream(o,s.stSeedSet);
	o << " all have property: ";
	for(int i = 0;i<256;i++)
		if(state_seeds.statefor(i) == s.stSeedSet)
		{	existStream(o,symtab.getsym((char) i));
			o << "; ";
		};

     };
   };


*/





   return o;
};


void distributeDecreasingRules()
{
	for(rules ds = alldecrules;ds;ds=ds->next)
		ds->rule->addToState();
};


int types[100];
int ct = 0;





int numSubStates(States x)
{
	ct = 0;

	for(oblist os = x.getObs();os;os=os->next)
	{
		int i;
		for(i = 0;i<ct;i++)
			if(os->theobject->getType() == types[i]) break;

		if(i == ct) types[ct++] = os->theobject->getType();

	};

	return ct;
};


/* END OF SUBSTATE ANALYSIS */
