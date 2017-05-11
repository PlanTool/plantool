/* 
* $Revision: 3.1 $
* $Date: 1999/01/06 10:15:43 $



facts.cc - This file defines all the operations on facts, fact layers
and the associated structures. */

#include "facts.h"
#include "actions.h"
#include "alldefs.h"
#include "stan.h"
#include <string.h>
#include <stream.h>
#include <stdio.h>

fact_level_info * flevel[MAX_PLAN];

void
hash_entry::set_noop(int n,action * achvr)
{
	mynoop = n;
	achievec[achvr->expos()] |= achvr->exmask();
};

void
hash_entry::add_achiever(int ln,action * achvr)
{
	allachievers = new action_list_node(achvr,allachievers);
	achievec[achvr->expos()] |= achvr->exmask();
};

void
hash_entry::add_consumer(action * cnsmr)
{
	consumers[cnsmr->expos()] |= cnsmr->exmask();
};

/* The hash-table management functions - usual thing. */

hashtable::hashtable()
{
	no_facts = 0;
	for(int i = 0;i < MAX_HASH;i++) hasht[i] = 0;
};



hash_entry *
hashtable::insert(char * name)
{
	int x = hash(name);
	int y = x % MAX_HASH;

	for(hash_entry * he = hasht[y];he > 0;he=he->getnext())
		if(he->has_name(x,name)){ delete [] name; return he;};
	hasht[y] = new hash_entry(name,x,hasht[y]);
	return hasht[y];
};


/* Facts: called hash_entry structures for historical reasons. A type alias is
defined to allow them to be called facts, too. */

hash_entry::hash_entry(char * nm,int h,hash_entry * he)
{
	next = he;
	name = nm;	/* NOTE: Initialisation in this case is always
				from a "new" allocated string. However, 
				this does mean that if the string is not
				required (being a duplicate) we end up
				wasting the space, unless we delete in insert.
				This is perhaps rather an odd way round. */


	hashval = h;

	achieved = MAX_PLAN+1;
	achievable = FALSE;
	modifiable = FALSE;
	deleters = 0;

	for(int i = 0;i<EXVECSIZE;i++)
		achievec[i] = 0;

	mynoop = MAX_ACTIONS;		// This will store the index of the noop
					// once we have one.


	mySym = new char[strlen(buff)+1];	// Symmetry code is still underutilised
	strcpy(mySym,buff);			// but should eventually be used to
						// exploit symmetry in the domain.

	precount = 0;				// precount is used to count the
						// number of times the fact is used
						// as a precondition. This is also
						// a development idea....



	allachievers = 0;

};



/* The dominates function is used in goal-ordering to check whether one
fact is ordered with respect to another. */

int
hash_entry::dominates(int ln,hash_entry * h)
{
//	return flevel[ln][fact_entry].get_submissives()[h->expos()] & h->exmask();

/* This mechanism is too strong: it forces an ordering between symmetric goals
which prevents their simultaneous achievement in cases where that would be possible.

However, modifying it to check that the apprate is 1 ensures it is only used to
force an ordering when the goals will have to be ordered anyway and a fixed
ordering will be preferable. This is too restricted a use of the symmetry 
machinery but it helps in TSP, for example.
*/
	if(flevel[ln][fact_entry].get_submissives()[h->expos()] & h->exmask()) return TRUE;

	if(flevel[ln][h->fact_entry].get_submissives()[expos()] & exmask()) return FALSE;

	return apprate[frnum]==1 && strcmp(mySym,h->mySym)==0 && has_noop(ln) && fact_entry > h->fact_entry;
};

void
hash_entry::initialise(int ln,int v)
{
	int i;

	fact_entry = v;
	achieved=ln;

	exvec_pos = v / 32;
	exvec_mask = 1 << (v % 32);

	for(action_list ds = deleters;ds > 0;ds = ds->next)
		ds->act->add_del_info(exvec_pos,exvec_mask);

	for(i=0;i<EXVECSIZE;i++)
		achievec[i] = 0;

	for(i=1;i<rCount;i++)
		if(name[strlen(resnms[i])] == '(' && 		
			strncmp(resnms[i],name,strlen(resnms[i]))==0)
		{	
			frnum = i;
			break;
		};

	if(i==rCount) frnum = 0;

	resval = ln>0;

};


/* The NOREUSE machinery was originally developed based on a mistake! It is 
a useful heuristic to use for some domains, but is not generally completeness
preserving, so usually linked out. */

#ifdef NOREUSE
int * no_reuse_vec[MAX_PLAN];

void
hash_entry::please_exclude(int ln)
{
	no_reuse_vec[ln][exvec_pos] |= exvec_mask;
	no_reuse_vec[ln-1][exvec_pos] |= exvec_mask;
};

void
hash_entry::dont_exclude(int ln)
{
	no_reuse_vec[ln][exvec_pos] ^= exvec_mask;
	no_reuse_vec[ln-1][exvec_pos] ^= exvec_mask;
};
#endif




/* Goal ordering is calculated with this function. */

int
hash_entry::ordered_by(int * acexvec)
{
	if(mynoop < MAX_ACTIONS)
	{
		int ep = action_table[mynoop]->expos();
		unsigned int em = action_table[mynoop]->exmask();
		int ov = acexvec[ep];
		acexvec[ep] |= em;

		for(int i = 0;i<exvecsizea;i++)
			if(((achievec[i] & acexvec[i]) ^ achievec[i]) != 0) 
			{	acexvec[ep] = ov;
				return FALSE;
			};
		acexvec[ep] = ov;
		return TRUE;
	}
	else
	{
		return excluded_by(acexvec);
	};
};



/* Checking for continued achievability during search. */

int
hash_entry::still_achievable(int ln)
{
	if(has_noop(ln) && !get_noop()->already_excluded(ln)) return TRUE;
	for(action_list achs = achievers(ln);achs>0;achs=achs->next)
		if(!achs->act->already_excluded(ln)) return TRUE;
	return FALSE;
};


/* Fact flevel information: this information corresponds to the fact
layers in a plan graph - it is stored in arrays associated with the
corresponding facts. */


void 
fact_level_info::copy(fact_level_info & pl)
{
	exvec = pl.exvec;
	used = 0;
	excluded = 0;
	achieved = 0;

	achievers = pl.achievers;
	consumers = pl.consumers;

	domination = pl.domination;
	dominated = pl.dominated;

};

void 
fact_level_info::initialise(action_list_node * acs)
{
	achievers = acs;
	consumers = 0;
	for(int i = 0;i<EXVECSIZE;i++)
	{
		exvec[i] = 0;
		domination[i] = 0;
	};

	used = 0;

	achieved = 0;
	excluded = 0;

};

void
fact_level_info::set_up(fact_level_info & f)
{
	for(int i = 0;i<EXVECSIZE; i++)
	{
		exvec[i] = 0;
		domination[i] = 0;
	};

	used = 0;
	excluded = 0;

	achievers = f.achievers;
//	consumers = f.consumers;

	achieved = 0;

};

void create_fact_layer(int ln,int ftl)
{
	flevel[ln] = new fact_level_info[ftl];

	for(int i=0;i<ftl;i++)
	{
		flevel[ln][i].initialise(fact_table[i]->get_allachievers());
	};
};
	
hashtable all_facts;
