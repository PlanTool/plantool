/* 
* $Revision: 3.12 $
* $Date: 1999/07/11 22:28:39 $

instantiation.cc */

/* This code contains the code that build instantiations of operator
schemas. It relies on the type information constructed by the TIM
system (confusingly supplied here in the stan.h/.cc files!) to set up
the basis for instantiation. It also uses statics and (not (= ...))
preconditions to reduce unnecessary instantiation. The AIPS version
of STAN also used a special filter on relevant objects, but this work
is still in early development and not included here. */

#include <stream.h>
#include "alldefs.h"
#include "globals.h"
#include "facts.h"
#include "actions.h"
#include "TimInterface.h"
#include "instantiation.h"
#include "switches.h"
//#define DEBUG2

void make_action(opptr op)
{
	action * newact = new action(op);
	available_acts = new action_list_node(newact,available_acts);
};


char *
catstrs(propptr p)
{
	int x = 0;
	int b = 0;

	for(propptr q = p;q>0;q=q->as)
		x += strlen(q->nm->varname)+1;

	char * s = new char[x+1];

	x=0;

	for(char * q = p->nm->varname;*q != '\0';q++)
			s[x++] = *q;
	s[x] = '1';
	s[x+1] = '\0';
	buff[b++] = symtab.enter(s);
	s[x++] = '(';
	p = p->as;

	for(;p>0;p=p->as)
	{	for(char * q = p->nm->varname;*q != '\0';q++)
			s[x++] = *q;
		s[x++] = ',';


		buff[b++] = cnv(1+p->nm->sgnum);



	};
	s[x-1] = ')';
	s[x] = '\0';
	buff[b] = '\0';
	
	return s;
};

/* This function produces the standard PDDL output form of the actions - 
we prefer the version produced by catstrs above, which is not LISP-like,
but more natural. */

#ifdef PDDLOUTPUT

char *
catstrsPDDL(propptr p)
{
	int x = 0;

	for(propptr q = p;q>0;q=q->as)
		x += strlen(q->nm->varname)+1;

	char * s = new char[x+1];

	x=0;

	for(char * q = p->nm->varname;*q != '\0';q++)
			s[x++] = *q;
	s[x++] = ' ';
	p = p->as;

	for(;p>0;p=p->as)
	{	for(char * q = p->nm->varname;*q != '\0';q++)
			s[x++] = *q;
		s[x++] = ' ';



	};
	s[x-1] = '\0';
	
	return s;
};

#endif

void build(opptr,objectlist);

/* Checking for (not (= ...)) constraints. */

int legal_ob(char * theob,uneqlist theuneqs)
{
	for(;theuneqs>0;theuneqs=theuneqs->next)
		if(strcmp(theuneqs->uneq->type->varname,theob)==0)
			return FALSE;
	return TRUE;
};

/* Type constraints. */

int well_typed(int * theobtp,int * thevartp)
{
#ifdef DEBUG2
	cout << "The object: " << theobtp[0] << " The var: " << thevartp[0] << "\n";
#endif
	return (((theobtp[0] & thevartp[0]) == thevartp[0]) && 
		((theobtp[1] & thevartp[1]) == thevartp[1]));
};

/* Recursive selection of objects for instantiation of objects. */

void choose(opptr op,objectlist relobs,objectlist obs)
{
	while(relobs > 0)
	{
#ifdef DEBUG2
		cout << "Considering " << relobs->obj->getName() << " for " << op->vars->type->varname << "\n";
#endif

		if(well_typed(relobs->obj->getTypeVec(),op->vars->type->type) && 
					legal_ob(relobs->obj->getName(),op->vars->uneqs)
					)
		{	
			op->vars->type->varname = relobs->obj->getName();

			op->vars->type->sgnum = relobs->obj->gpNum();

#ifdef SYMMETRY
			op->vars->type->sindex = relobs->obj->getSymIndex();
#endif

			tpptr oldparams = op->vars;
			op->vars = op->vars->tps;
#ifdef DEBUG2
			printop(op);
#endif
			build(op,obs);
			op->vars = oldparams;
		};
		relobs = relobs->next;
	};
};

/* Used in an earlier version of STAN when types were supplied explicitly. 
This was discontinued when STAN was entered for the strips track of the AIPS
competition, since types are not part of strips. This decision led to the
whole of the development of TIM.... */

tpptr match_type(char * tp,tpptr obs)
{
	while((obs > 0) && (strcmp(tp,obs->type->type_name)!=0)) obs=obs->tps;
	return obs;
};

/* The main instantiation function. */

void build(opptr op,objectlist obs)
{
	if(op->vars == 0)
	{
		acnt++;
		make_action(op);
	}
	else
	{
		if(!op->vars->type->used)
		{
			choose(op,obs,obs);
		}
		else
		{
			if(legal_ob(op->vars->type->varname,op->vars->uneqs))
			{
				tpptr oldparam = op->vars;
				op->vars = op->vars->tps;
#ifdef DEBUG2
				printop(op);
#endif
				build(op,obs);
				op->vars = oldparam;
			}
#ifdef DEBUG2
			else
			{
				cout << "ILLEGAL!\n";
				printop(op);
			};
#endif
		};
	};

};


/* This code is used to start the instantiation process with static 
conditions. Parameters that are set during this process are then not
touched subsequently. */

void buildStatics(opptr,objectlist);

void instantiateFromStatic(opptr op,plptr statc,objectlist obs)
{
	if(statc->prop->as && 
		 !strcmp(statc->prop->as->nm->type_name,"101"))
	{
		statc->prop->as->nm->used = 1;
		buildStatics(op,obs);
	}
	else
	{
	    for(plptr sts = findPred(statc->prop->nm->varname)->initially;sts;sts=sts->rest)
		if(!strcmp(sts->prop->nm->varname,statc->prop->nm->varname))
		{
			propptr args1 = sts->prop->as;
			propptr args;
			for(args = statc->prop->as;args;args=args->as)
			{
				if(args1->nm->varname[0] == '\0') break;

				if(!args->nm->used)
				{
					objectlist os = obs;
					for(;strcmp(os->obj->getName(),args1->nm->varname);os=os->next);
					

					args->nm->varname = args1->nm->varname;


					args->nm->sgnum = os->obj->gpNum(); 

#ifdef SYMMETRY
					args->nm->sindex = os->obj->getSymIndex();
#endif


				}
				else
				{	if(strcmp(args->nm->varname,args1->nm->varname))
						break;
				};
				args->nm->used++;
				args1 = args1->as;
			};

			if(!args) buildStatics(op,obs);
			args1 = args;

			for(args = statc->prop->as;args != args1;args=args->as)
				args->nm->used--;

		};
	};
};

void buildStatics(opptr op,objectlist obs)
{
	if(op->statics == 0)
	{
#ifdef DEBUG2
		cout << "No more statics in: ";
		printop(op);
#endif
		build(op,obs);
	}
	else
	{
		plptr statc = op->statics;
#ifdef DEBUG2
		cout << "About to instantiate from a static: ";
		printop(op);
#endif
		op->statics = statc->rest;
		instantiateFromStatic(op,statc,obs);
		op->statics = statc;
	};
};

/* This controls the whole process of instantiating the actions. */

void build_all_actions(oplistptr ops,objectlist obs)
{
	acnt = 0;
	no_acts = 0;
	for(;ops>0;ops=ops->ops)
	{
#ifdef VERBOSE
		cout << acnt << " actions\n";
#endif
		buildStatics(ops->op,obs);
	};
#ifdef VERBOSE
	cout << acnt << " actions built\n";
#endif
};

/* Now remove actions which aren't needed.... */

void filter_actions()
{
	action_list f = available_acts;
	action_list b = 0;

	for(;f>0;f=f->next)
	{
		if(!f->act->all_precs_achievable())
		{
#ifdef DEBUG
			cout << "Removing " << f->act->get_name() << "\n";
#endif
			if(b)
			{	b->next=f->next;
			}
			else
			{
				available_acts = f->next;
			};
		}
		else
		{
			b = f;
		};
	};
};

/* The appRate function decides at what rate an action can be applied (that is,
how many times per level. This is useful to decide how many goals of a given
type can be achieved in a level, which in turn guides the plan length. For example,
in the TSP on a fully connected graph, the appRate function works out that only
one move can be made per level, and so only one new visited location can be 
achieved per level, so the whole plan must be as long as the size of the graph.
This pre-empts the need for unnecessary search during graph construction. */

int appRate(char * nm,opptr op)
{
	int p,c,ar1;

	int ar=0;
	plptr pp;
#ifdef APPRATE
	cout << "Apprate check\n";
#endif
	for(predlist ps = allpreds;ps;ps=ps->next)
	{
#ifdef APPRATE
		cout << "Checking apprate for " << ps->name << "\n";
#endif
		p=0;
		c=0;

		for(pp=op->precs;pp;pp=pp->rest)
			if(strcmp(pp->prop->nm->varname,ps->name)==0)
				p++;

		if(p)
			for(pp=op->dels;pp;pp=pp->rest)
				if(strcmp(pp->prop->nm->varname,ps->name)==0)
					c++;

		if(c) 
		{	ar1 = (ps->fr-1-p+c)/c;
			ar = ar?(ar<=ar1?ar:ar1):ar1;
		};
#ifdef APPRATE
		cout << p << " " << c << " " << ar << "\n";
#endif
	};

	if(ar)
	{
		p = 0;

		for(pp = op->adds;pp;pp=pp->rest)
			if(strcmp(pp->prop->nm->varname,nm)==0)
				p++;

			
		return p*ar;
	}
	else
	{
		return MAX_FACTS;
	};
};
	
