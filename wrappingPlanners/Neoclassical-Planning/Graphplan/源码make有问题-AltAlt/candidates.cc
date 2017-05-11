/*
* $Revision: 3.1 $
* $Date: 1999/01/06 10:15:43 $


candidates.cc 

This file defines the wave-front machinery. This is based on the observation that
once we are at the fix-point of a graph there is nothing new in subsequent
layers. This means that the search between the current layer and the layer just
after the fix-point is repeating exactly the work done in the search from the
preceding layer to the fix-point. The repeated work is not only costly in time,
but also generates bad goal sets which have to be memoised and layer information
which must be stored, all of which is costly in space. Graph-plan becomes an
iterated depth-first search of the search space after the fix point, which 
regenerates all of the early parts of the search space on each increase to the
iterator. The wavefront machinery avoids this by maintaining a record of the
goal sets which remain to be examined from the fix-point: the wavefront. This 
collection is just the bad-goal sets generated at the layer just before the
fix-point in the preceding search. Once this observation is exploited, it 
also becomes clear that the termination property is simply the exhaustion of these
candidate sets: there is only a finite number of possible goal sets to consider
from the fix-point. This all means that there is a lot of freedom once we reach 
the fix-point - we can try candidates in any order we like, without being restricted
to the iterated depth-first search of the usual Graphplan. We have found that 
candidates whose generated search goes far down into the graph are often the ones
which will ultimately yield a plan, so we focus on these primarily. Other heuristics
are possible - we have experimented a bit, but there is no doubt more that could be
done here. Our current machinery is fully described in JAIR volume 10, 1999.*/

#include "switches.h"
#include "candidates.h"
#include "facts.h"
#include "actions.h"
#include "globals.h"
#include "search.h"


/* A candidate maintains its own collection of goals and the plan to this point.
The former is the candidate set itself, while the latter is just for reporting the
plan at the end. This is a shared list structure, so quite efficient, but it would
be nice to compress it further. Similarly, the goal set could be stored as a 
vector to save space if some extra machinery were put in place. */

/* Constructor for first candidate. */

candidate::candidate(token_list gls,int ln)
{

        plan_size = ln;
        stored_acts_at[level_out+1] = 0;
	stored_goals = 0;
	for(;gls>0;gls=gls->next)
		stored_goals = new token_list_node(gls->info,stored_goals);

token_list tmp = 0, tmp1;
while(stored_goals)
{
	tmp1 = stored_goals;
	stored_goals=stored_goals->next;
	tmp1->next = tmp;
	tmp = tmp1;
};
stored_goals = tmp;

#ifdef COUNTCANDS
	numCs++;
#endif

#ifdef SYMMETRY
	myBrokenSym = brokenSym;
#endif

};

/* Constructor for subsequent candidates. */

candidate::candidate(token_list gls,action_list a[],int ln)
{  
	for(int i = level_out+2;i<=ln;i++)
		stored_acts_at[i] = a[i];
        action_list x = 0;
	for(action_list y = a[level_out+1];y>0;y=y->next)
	  x = new action_list_node(y->act,x);
	stored_acts_at[level_out+1] = x;
	plan_size = ln;
	stored_goals = 0;


	for(;gls>0;gls=gls->next)
	{
		stored_goals = new token_list_node(gls->info,stored_goals);
	};

token_list tmp = 0, tmp1;
while(stored_goals)
{
	tmp1 = stored_goals;
	stored_goals = stored_goals->next;
	tmp1->next = tmp;
	tmp = tmp1;
};
stored_goals = tmp;

#ifdef COUNTCANDS
	numCs++;
#endif

#ifdef SYMMETRY
	myBrokenSym = new int[numAllSymObs];
	for(int i = 0;i<numAllSymObs;i++) myBrokenSym[i] = brokenSym[i];
#endif


};


ostream & operator<<(ostream & o,const candidate & c)
{
	o << "Candidate at level: " << c.plan_size << ":\n";
	for(int i = level_out+1;i<=c.plan_size;i++)
	{
		o << i << ": ";
		for(action_list al = c.stored_acts_at[i];al>0;al=al->next)
			o << al->act->get_name() << "\n";
	};
	o << "Goals: ";
	for(token_list gls = c.stored_goals;gls>0;gls=gls->next)
		o << gls->info->get_name() << "\n";

	return o;
};


candidate_list_node * cands = 0;
candidate_list_node * lastcand = 0;


candidate_list_node * add_candidate(token_list goals,action_list a[],candidate_list_node * cands)
{

	if (lastcand)
	{
	 	lastcand->next= new candidate_list_node(goals,a,0);
		lastcand = lastcand->next;
		return cands;
	}
	else
	{
		lastcand = new candidate_list_node(goals,a,0);
		return lastcand;
	};


	
};

/* Extension of a candidate is simply the process of setting up its goal
set at the wavefront and searching as usual. The machinery for recording
the plan so far and extending it is a bit clumsy - surely could be improved. */

int
candidate::extend()
{
        for(int i = level_out+1;i<=plan_size;i++)
	   committed_acts_at[i+1] = stored_acts_at[i];


#ifdef SYMMETRY
	brokenSym = myBrokenSym;
#endif

#ifdef CANDIDATESOUTPUT
	cout << "Extending: " << *this << "\n";
#endif

	only_mark_dominations(level_out+1,stored_goals);
//	tried[level_out+1] = 0;
//	maxtried[level_out+1]=0;

#ifdef SYMMETRY
	for(int i = 0;i<=numSymGps[level_out+1];i++)
		for(int k = 0;k<symSizes[i];k++)
			triedGps[level_out+1][i][k] = '\0';
#endif

	for(int i = 0;i<exvecsizef;i++)
		gcs[i] = 0;

#ifdef ACTIONSETMINIMAL
/* To allow action_set_minimal test to mark top level goals in situations
in which the test fails. See search.cc. */

	goals_at[level_out+1] = stored_goals;
#endif

	if(search_plan(level_out+1,stored_goals))
	{
	  return TRUE;
	};


	for(;stored_goals;stored_goals = stored_goals->next)
		if(stored_goals->info->is_dominated(level_out+1))
			stored_goals->info->get_noop()->untry_noop(level_out+1,stored_goals->info);
	   
	return FALSE;
};

