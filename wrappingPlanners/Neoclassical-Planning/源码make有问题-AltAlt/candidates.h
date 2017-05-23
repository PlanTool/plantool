/* 
* $Revision: 3.1 $
* $Date: 1999/01/06 10:15:43 $



candidates.h */

#ifndef __CANDIDATES
#define __CANDIDATES

#include "switches.h"
#include "facts.h"
#include "actions.h"
#include "globals.h"
#include "search.h"
#include "badgoals.h"
#include <stream.h>

class candidate {
private:
	action_list stored_acts_at[MAX_PLAN];
	token_list stored_goals;

#ifdef SYMMETRY
	int * myBrokenSym;
#endif

public:
	int plan_size;
	candidate(token_list,action_list [],int);
	candidate(token_list,int);
	friend ostream & operator<<(ostream &,const candidate &);
	int extend();

	~candidate()
	{
#ifdef COUNTCANDS
		numCs--;
#endif

#ifdef SYMMETRY
		delete [] myBrokenSym;
#endif
	};

#ifdef CANDSTATS
	heritage * past;
	int totalpast;
	int best;
#endif

};

class candidate_list_node {
public:
	candidate cand;
	candidate_list_node * next;
	candidate_list_node(token_list gls,candidate_list_node * nxt) 
	  : cand(gls,top_level), next(nxt) {}
	candidate_list_node(token_list gls,action_list a[],candidate_list_node * nxt)
		: cand(gls,a,top_level), next(nxt) 
	{
#ifndef EBLOFFATWF
		already_seen.insertall(gls);		
#endif
	}
};

extern candidate_list_node * cands;
extern candidate_list_node * lastcand;


candidate_list_node * add_candidate(token_list,action_list *,candidate_list_node *);

#endif
