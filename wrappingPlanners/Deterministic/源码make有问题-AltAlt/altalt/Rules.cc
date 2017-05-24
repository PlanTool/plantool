/*
* $Revision: 1.3 $
* $Date: 1999/07/09 21:45:02 $
*/

#include "SymTab.h"
#include "BasicTim.h"
#include "Rules.h" 
#include "switches.h"
#include "States.h"



rules allrules = 0;
rules alltransrules = 0;
rules alldecrules = 0;

char
transrule::tryrule(char * st)
{
/*
	showrule();
	cout << " for ";
	for(int i = 0;i < strlen(st);i++)
	{	symtab.showsym(st[i]); 
		cout << " ";
	};
	cout << "\n";
*/

	char tmp[100];
	int i = 0;
	unsigned int cnt = 0;

	char * y = left;

	for(char * x = st;x[0] != '\0';x++)
	{
		for(;y[0] != '\0';y++)
			if(y[0] >= x[0]) break;
		if(y[0] == x[0])
		{
			cnt++;
			y++;
		}
		else
		{
			tmp[i++] = x[0];
		};
	};

	if(strlen(left) <= cnt)
	{
		for(unsigned int j = 0;j<=strlen(right);j++)
			tmp[i++] = right[j];

		sortsym(tmp);

		return statetab.newenter(tmp);
	};
	return '\0';
};



int staticProp(char c)
{
	return allstates[state_seeds.statefor(cnv(c))].getRules() == 0;
};


int
transrule::onlyStaticEnablers()
{
	showrule();

	char * c;
	for(c = enablers;*c != '\0';c++)
		if(!staticProp(*c)) break;

	return *c=='\0';
};

void
transrule::addToState()
{
	if(right[0] != '\0')
	{
		allstates[state_seeds.statefor(cnv(right[0]))].addRule(this);
#ifdef STANSHOW
		showrule();
		cout << "added to state " << state_seeds.statefor(cnv(right[0])) << "\n";
#endif
	}
	else
	{
		allstates[state_seeds.statefor(cnv(left[0]))].addRule(this);
#ifdef STANSHOW
		showrule();
		cout << "added to attribute-space " << state_seeds.statefor(cnv(left[0])) << "\n";
#endif
	};
};


int
transrule::addToState(stateSeeds& stsds)
{
	if(right[0] != '\0')
	{
		return stsds.statefor(cnv(right[0]));
#ifdef STANSHOW
		showrule();
		cout << "added to state " << stsds.statefor(cnv(right[0])) << "\n";
#endif
	}
	else
	{
		return stsds.statefor(cnv(left[0]));
#ifdef STANSHOW
		showrule();
		cout << "added to attribute-space " << stsds.statefor(cnv(left[0])) << "\n";
#endif
	};
};


void
transrule::addToAttState()
{
	if(attribute_seeds.isMarked(cnv(right[0])))
	{
		allstates[state_seeds.statefor(cnv(right[0]))].addRule(this);
#ifdef STANSHOW
		showrule();
		cout << "added to state " << state_seeds.statefor(cnv(right[0])) << "\n";
#endif
	};
};

int
transrule::enabled()
{
	char * es = enablers;
	while(es[0] != '\0')
	{
		if(!attribute_seeds.isMarked(cnv(es[0]))) break;
		es++;
	};
	return es[0] == '\0';
};

int
transrule::ordered(int i,int j)
{
	char c = left[0];

	if(enablers[0] == '\0' || state_seeds.statefor(cnv(c)) != i) return 0;

	for(char * es = enablers;es[0] != '\0';es++)
	{
		c = es[0];
		if(state_seeds.statefor(cnv(c)) == j) return 1;
	};

	return 0;
};

int satisfiesType(int sn,int tp)
{
	return ofType(alltypes[tp].getTypeVec(),sn);
};


int
transrule::isEnabled(int tp)
{

/* I SUPPOSE THAT IT WOULD BE MORE SENSIBLE TO TOPOLOGICALLY SORT THE
TYPES AND WORK THROUGH THE DEPENDENCIES. NOT DOING SO COULD MEAN THAT
RULES ARE ENABLED WHICH WOULD NOT BE IF WE HAD A FINER TYPE DISTINCTION
WITHIN THE ENABLING ATTRIBUTE/STATE SPACES. */

	for(unsigned int i = 0;i<strlen(enablers);i++)
	{	
		int x = cnv(enablers[i]);
		if(!satisfiesType(state_seeds.statefor(x),tp)) return 0;
	};

	return 1;
};
char
transrule::trySubRule(char * st)
{
/*
	showrule();
	cout << " for ";
	for(int i = 0;i < strlen(st);i++)
	{	symtab.showsym(st[i]); 
		cout << " ";
	};
	cout << "\n";
*/

	char tmp[100];
	int i = 0;
	unsigned int cnt = 0;

	char * y = left;

	for(char * x = st;x[0] != '\0';x++)
	{
		for(;y[0] != '\0';y++)
			if(y[0] >= x[0]) break;
		if(y[0] == x[0])
		{
			cnt++;
			y++;
		}
		else
		{
			tmp[i++] = x[0];
		};
	};

	if(strlen(left) <= cnt)
	{
		for(unsigned int j = 0;j<=strlen(right);j++)
			tmp[i++] = right[j];

		sortsym(tmp);

		if(tempstates.newenter(tmp) != '\0') 
		{
			return statetab.enter(tmp);
		}
		else
		{
			return '\0';
		};
	};
	return '\0';
};


int 
transrule::isAttRule()
{
	return left[0] == '\0' || right[0] == '\0';
};

void
transrule::showrule()
{
	showrule(cout);
};


void
transrule::showrule(ostream & cout)
{
	cout << symtab.getsym(left[0]);

	for(unsigned int i = 1;i<strlen(left);i++)
	{	cout << ", " << symtab.getsym(left[i]);
	};
	cout << " -> " << symtab.getsym(right[0]);

	for(unsigned int i = 1;i<strlen(right);i++)
	{
		cout << ", " << symtab.getsym(right[i]);
	};

	if(enablers[0] != '\0')
	{
		cout << " <= (" << symtab.getsym(enablers[0]);
		for(unsigned int i = 1;i<strlen(enablers);i++)
		{
			cout << ", " << symtab.getsym(enablers[i]);
		};
		cout << ")";
	};


	cout << "\n";
};

int
transrule::splitRule(char c)
{
	int splitter = 0;
	int ret = 0;

	for(unsigned int i = 0;i<strlen(left);i++)
		if(left[i] == c)
		{	splitter = 1;
			for(unsigned int j = i;j < strlen(left);j++)
				left[j] = left[j+1];
			i--;
	   	};

	if(splitter)
	{
		char * nens = new char[strlen(enablers)+2];
		strcpy(nens,enablers);
		nens[strlen(enablers)] = c;
		nens[strlen(enablers)+1] = '\0';

		sortsym(nens);

		delete [] enablers;
		enablers = nens;

	};

	splitter = 0;

	for(unsigned int i = 0;i<strlen(right);i++)
	   if(right[i] == c)
	   {	splitter = 1;
		for(unsigned int j = i;j < strlen(right);j++)
			right[j] = right[j+1];
		i--;
	   };
	if(splitter)
	{
		char a[2];
		a[1] = '\0';

		if(!strlen(left))
		{
			for(unsigned int i = 0;i<strlen(right);i++)
			{
			   a[0] = right[i];
			   alltransrules = new rulelistnode(new transrule(a,enablers,producer,varName),alltransrules);
			};

			ret = 1;
		};

		a[0] = c;

		char es[strlen(enablers)+strlen(left)+1];
		int j = 0;
		for(unsigned int i = 0;i<strlen(enablers);i++)
			if(enablers[i] != c) es[j++] = enablers[i];
		for(unsigned int i = 0;i<=strlen(left);i++)
			if(left[i] != c) es[j++] = left[i];

		alltransrules = new rulelistnode(new transrule(a,es,producer,varName),alltransrules);

	};

	return ret;
};

void
stateSeeds::adjustAttributes(stateSeeds ss)
{
	for(rules rs = alltransrules;rs;rs=rs->next)
		rs->rule->addToAttState();
};

void stateSeeds::markStates()
{
	for(int i = 0;i<256;i++)
		if(used[i] && deref(stseeds[i]) == i)
			realState[i]++;
};

void stateSeeds::notRealState(int i)
{
	realState[deref(stseeds[i])] = 0; 
};

void
stateSeeds::showStSeeds()
{
	for(int i = 0;i<256;i++)
	{
		if(used[i] && deref(stseeds[i]) == i)
		{
			cout << "\n{";
			symtab.showsym((char) i);

			for(int j = 0;j<256;j++)
			{
				if(i != j && deref(stseeds[j])==i)
				{	cout << ", ";
 					symtab.showsym((char) j);

				};
			};
			cout << "}";
		};
	};
	cout << "\n";
};
void
stateSeeds::showAtSeeds()
{
	for(int i = 0;i<256;i++)
	{
		if(used[i] && deref(stseeds[i]) == i)
		{
			cout << "\n{";
			int done = 0;
			if(marked[i])
			{
			 	symtab.showsym((char) i);
				done = 1;
			};

			for(int j = 0;j<256;j++)
			{
				if(i != j && deref(stseeds[j])==i && marked[j])
				{	if(done) cout << ", ";
 					symtab.showsym((char) j);
					done = 1;
				};
			};
			cout << "}";
		};
	};
	cout << "\n";
};

int
stateSeeds::deref(char c)
{
	int a = cnv(c);
	int y = cnv(stseeds[a]);

	while(a != y)
	{
		a = y;
		y = cnv(stseeds[a]);
	};
	return a;
};


void
stateSeeds::unite(char x,char * xs)
{
	int i = 0;
	int y = deref(x);

#ifdef STANSHOW1
	cout << "Uniting... ";
	symtab.showsym((char) y);
	cout << " with ";
#endif

	used[y] = 1;

	while(xs[i] != '\0')
	{
#ifdef STANSHOW1
		symtab.showsym(xs[i]);
		cout << " ";
#endif
		used[deref(xs[i])] = 1;
		stseeds[deref(xs[i])] = y;
		stseeds[cnv(xs[i])] = y;
		used[cnv(xs[i])] = 1;
		i++;
	};
#ifdef STANSHOW1
	cout << "\n";
	showStSeeds();
#endif
};

int numStates;
int numRStates;

int 
stateSeeds::countStates()
{
	int nstates = 0;

	for(int i = 0;i<256;i++)	
		if(used[i] && deref(stseeds[i]) == i)
			nstates++;

	return nstates;
};


void 
stateSeeds::setupStates()
{
	numStates = 1;

	for(int i = 0;i<256;i++)	
		if(used[i] && deref(stseeds[i]) == i)
			numStates++;

	for(int i = 0;i<256;i++)
		if(!used[i] && attribute_seeds.shouldUse(i))
			numStates++;

	numRStates = numStates;


	for(predlist ps = allpreds;ps;ps=ps->next)
		if(!ps->ar) numStates += ps->numargs;

	allstates = new States[numStates];
	int x = 1;

	for(int i = 0;i<256;i++)	
		if(used[i] && deref(stseeds[i]) == i)
		{
			allstates[x] = States(x,realState[i]);

			for(int j = 0;j<256;j++)
				if(used[j] && deref(stseeds[j])==i)
					used[j] = x;

			x++;
		};

	for(int i = 0;i<256;i++)
		if(!used[i] && attribute_seeds.shouldUse(i))
		{
			allstates[x] = States(x,0);

#ifdef STANSHOW
			cout << "Set up state for: ";
			symtab.showsym(cnv(i));
			cout << "\n";
#endif
			used[i] = x++;
		};


	char tmp[50];

	for(predlist ps = allpreds;ps;ps=ps->next)
	  if(!ps->ar)
	  {
		strcpy(tmp,ps->name);
		tmp[strlen(ps->name)+1] = '\0';

		char a = '0';

		for(int i = 0;i<ps->numargs;i++)
		{
			a++;
			allstates[x] = States(x,0);

			tmp[strlen(ps->name)] = a;
			char c = symtab.enter(tmp);
			if(!used[cnv(c)]) used[cnv(c)] = x++;
			else numStates--;
		};
	};


	collectionpt = new char *[numStates];
	cps = new int[numStates];

	for(int i = 0;i<numStates;i++)
		collectionpt[i] = new char[30];
};

/* Note that in the following function we set x = 1 to avoid confusion
between a 0 setting in the used array with the 0th element in the sequence of subspaces. */

void
stateSeeds::setupReseededs(subStates * subs,int type)
{
	markAttsReSeeded();

	int x = 1;
	for(int i = 0;i<256;i++)	
		if(used[i] && deref(stseeds[i]) == i)
		{

/* Recall that deref gets us the sentinel element for the state seeds set.
It is this that will be marked as an attribute space if appropriate. */

			subs[x] = subStates(x,realState[deref(stseeds[i])],type);

			for(int j = 0;j<256;j++)
				if(used[j] && deref(stseeds[j])==i)
					used[j] = x;

			x++;
		};
};



int
stateSeeds::statefor(int x)
{
	return used[x];
};

stateSeeds state_seeds;
stateSeeds attribute_seeds;
