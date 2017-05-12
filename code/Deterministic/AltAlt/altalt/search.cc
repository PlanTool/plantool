/* 
* $Revision: 3.1 $
* $Date: 1999/01/06 10:15:43 $



search.cc */


/* This is pretty much a standard implementation of Graphplan's search
machinery. The main difference is the implementation of Rao Kambhampati's EBL/DDB machinery.*/

#include "globals.h"
#include "facts.h"
#include "actions.h"
#include "badgoals.h"
#include "candidates.h"
#include "search.h"
#include "construction.h"
#include "searchhsp.h"


/* RS: Added by RS */
/* ALTALT */
//This function retrieves valid actions given the level of the graph.
void retrieveValidActions(int ln)
{
  int i;
  int index=0;

  for(i=0;i<acts_at[ln];i++)
    {
      if(!action_table[i]->is_noop())
	{
	  finalAcTable[index]=action_table[i];
	  index++;
	}

    }
   finacts_at[0]=index;

}
/* RS: End */

int action_set_minimal(int ln)
{
	/* The action set is minimal if every action is uniquely responsible
		for at least one goal. This means that we need to go through
		the add lists of each action that has been selected and see
		that at least one of these effects has an is_used count of 1 */

	/* Note that there are alternatives - we could consider achievers when
		goals are marked, to ensure that they are still uniquely 
		achieving at least one goal. This would require considering all
		the achievers of goals that are reachieved. Not clear what is
		best here. */

	token_list achs;
	int is_action = 0;

	for(action_list acts = committed_acts_at[ln];acts > 0;acts=acts->next)
	{
		if(acts->act->is_noop()) continue;

		is_action = 1;

		for(achs=acts->act->get_adds();achs>0;achs=achs->next)
			if(achs->info->is_done(ln)==1 && (achs->info->get_when()==ln || !achs->info->is_used(ln-1))) 
				break;
		if(achs==0) return FALSE;
	};
	return is_action;
};



/* This function is used to record the effects of goal orderings. Goals which
must be achieved earlier than other goals in the same set are automatically
achieved using a noop. There is also a check to see whether the chain of
ordered goals is too long to fit in between the start of the graph and the current
layer. */

inline int
mark_dominations(int ln,token_list gls)
{
#ifdef DOMOFF
		return TRUE;
#endif

//	cout << "-----\n";

	hash_entry * tmp;

	num_goals = 0;

	for(;gls;gls=gls->next)
	{
		gls->info->no_domination(ln);
		oGoals[num_goals] = gls->info;
		oGoals[num_goals]->domval = num_goals++;
//		cout << gls->info->get_name() << "\n";
	};

	for(int i = 0;i<num_goals;i++)
	{
		for(int j = i+1;j<num_goals;j++)
			if(oGoals[i]->dominates(ln,oGoals[j]))
			{	oGoals[i]->mark_dominates(ln);
				if(!oGoals[j]->is_dominated(ln))
				{
					oGoals[j]->get_noop()->try_noop(ln,oGoals[j]);
					oGoals[j]->mark_dominated(ln);

// cout << oGoals[j]->get_name() << " dominated at " << ln << "\n";
				};

				if(oGoals[j]->domval != oGoals[i]->domval)
				{
//cout << "Swapping " << oGoals[i]->get_name() << " and " << oGoals[j]->get_name() << "\n";
					int newdom = max(oGoals[i]->domval,oGoals[j]->domval);
					oGoals[i]->domval = newdom;
					oGoals[j]->domval = newdom;
					tmp = oGoals[i];
					oGoals[i] = oGoals[j];
					oGoals[j] = tmp;
					j = i;
				};

			}
			else
			{
				if(!oGoals[i]->is_dominated(ln) && oGoals[j]->dominates(ln,oGoals[i]))
				{	oGoals[j]->mark_dominates(ln);
					oGoals[i]->mark_dominated(ln);
					oGoals[i]->get_noop()->try_noop(ln,oGoals[i]);

// cout << oGoals[i]->get_name() << " dominated at " << ln << "\n";

				};
			};

		gdval[i] = oGoals[i]->get_when();
		for(int j = 0;j<i;j++)
			if(gdval[j] && gdval[j] >= gdval[i] && 
					oGoals[i]->dominates(ln,oGoals[j]))
				gdval[i] = gdval[j]+1;

		if(gdval[i]>ln)
			return FALSE;

	};			

	return TRUE;
};


/* Similar function, but doesn't care about the chain length: this is because
we know the goal set has already been tested for this earlier. */

void
only_mark_dominations(int ln,token_list gls)
{
#ifdef DOMOFF
	return;
#endif
	hash_entry * tmp;

	num_goals = 0;

	for(;gls;gls=gls->next)
	{
		gls->info->no_domination(ln);
		oGoals[num_goals] = gls->info;
		oGoals[num_goals]->domval = num_goals++;

	};

	for(int i = 0;i<num_goals;i++)
	{
		for(int j = i+1;j<num_goals;j++)
			if(oGoals[i]->dominates(ln,oGoals[j]))
			{	oGoals[i]->mark_dominates(ln);
				if(!oGoals[j]->is_dominated(ln))
				{
					oGoals[j]->get_noop()->try_noop(ln,oGoals[j]);
					oGoals[j]->mark_dominated(ln);

// cout << oGoals[j]->get_name() << " dominated at " << ln << "\n";

				};

				if(oGoals[j]->domval != oGoals[i]->domval)
				{
//cout << "Swapping " << oGoals[i]->get_name() << " and " << oGoals[j]->get_name() << "\n";
					int newdom = max(oGoals[i]->domval,oGoals[j]->domval);
					oGoals[i]->domval = newdom;
					oGoals[j]->domval = newdom;
					tmp = oGoals[i];
					oGoals[i] = oGoals[j];
					oGoals[j] = tmp;
					j = i;
				};
			}
			else
			{
				if(!oGoals[i]->is_dominated(ln) && oGoals[j]->dominates(ln,oGoals[i]))
				{	oGoals[j]->mark_dominates(ln);
					oGoals[i]->mark_dominated(ln);
					oGoals[i]->get_noop()->try_noop(ln,oGoals[i]);

// cout << oGoals[i]->get_name() << " dominated at " << ln << "\n";

				};
			};

	};
};


void accumulate_goals(int ln)
{
	goals_at[ln] = 0;

/*	for(int i=facts_at[ln]-1;i-1;i--)*/
	for(int i = 0;i<facts_at[ln];i++)
		if(fact_table[i]->is_used(ln))
		{
/*			int c = 0;
			for(action_list as = fact_table[i]->get_achievers(ln);as;as=as->next)
				c++;
			cout << c << " ";
*/
			goals_at[ln] = new token_list_node(fact_table[i],goals_at[ln]);
		};
//		cout << "\n";

};

void release_goals(token_list gs)
{
	token_list tmp;

	while(gs)
	{
		tmp = gs;
		gs = gs->next;
		delete tmp;
	};
};

/* The main search code... */

int tried[MAX_PLAN];
int maxtried[MAX_PLAN];


int search_plan(int ln,token_list goals)
{
	/* Search for a plan from level ln:
		For each goal in turn:
			look for an achiever;
			check it's valid together with achievers so far;
			...
	*/

//if(marked_bad_goal_set(ln-1,goals)) cout << "Trying again from " << ln << "\n";

#ifdef SEARCHDEBUG
	cout << "Searching at level: "<<ln << " with goals: \n";
	for(token_list gs = goals;gs>0;gs=gs->next)
	{
		cout << "\t" << gs->info->get_name();
		if(gs->info->is_dominated(ln)) cout << "*";
		cout << "\n";
	};

#endif

	if(ln==0) return TRUE; /* If we have reached the last layer */


	if(goals == 0)
	{
		/* Then we are ready to drop back a layer */

#ifdef SEARCHDEBUG
		cout << "Goals done at this level\n";
#endif

#ifdef ACTIONSETMINIMAL
/* This code seems to interact badly with the EBL/DDB stuff. Could be that we
need to refine the marking of goals to ensure we restrict the conflict set. Another
idea would be to get action_set_minimal to hack the plan in the case when there
are redundant actions. In the all noop case we should do something different.*/

		if(!action_set_minimal(ln)) 
		{
//cout << "The critical bit\n";
			for(goals = goals_at[ln];goals;goals=goals->next)
			{	//cout << "Marking " << goals->info->get_name() << "\n";
				gcs[goals->info->expos()] |= goals->info->exmask();
			};
			return FALSE;
		};

#ifdef SEARCHDEBUG
		cout << "Checked for minimal action set\n";
#endif

#endif

#ifdef REPORTACTS
		cout << "Action set at level: " << ln << ":\n";
		for(action_list as = committed_acts_at[ln];as;as=as->next)
			cout << "\t" << as->act->get_name() << "\n";
#endif

//		tried[ln-1] = 0;
//		maxtried[ln-1] = 0;

		accumulate_goals(ln-1);

		if(marked_bad_goal_set(ln-1,goals_at[ln-1])) 
		{
			int rgcs[EXVECSIZE];
			for(int i = 0;i<exvecsizef;i++)
				rgcs[i] = 0;
			for(token_list gs = goals_at[ln-1];gs;gs=gs->next)
				if(gcs[gs->info->expos()] & gs->info->exmask())
					rgcs[gs->info->getwhat_for(ln-1)->expos()] |= gs->info->getwhat_for(ln-1)->exmask();

			for(int i = 0;i<exvecsizef;i++)
				gcs[i] = rgcs[i];

			release_goals(goals_at[ln-1]);
			return FALSE;
		};

#ifdef SEARCHDEBUG
		cout << "Come through bad goal set test\n";
#endif

		if(!mark_dominations(ln-1,goals_at[ln-1])) 
		{
			int rgcs[EXVECSIZE];
			for(int i = 0;i<exvecsizef;i++)
				{rgcs[i] = 0;
				gcs[i] = 0;};		
			for(token_list gs = goals_at[ln-1];gs;gs=gs->next)
			{
		 	  	if(gs->info->is_dominated(ln-1))
					gs->info->get_noop()->untry_noop(ln-1,gs->info);
				if(gs->info->is_in_domination(ln-1))
				{
				 gcs[gs->info->expos()] |= gs->info->exmask();
				 rgcs[gs->info->getwhat_for(ln-1)->expos()] |= gs->info->getwhat_for(ln-1)->exmask();};
			};

			mark_bad_goal_set(ln-1,goals_at[ln-1]);

			release_goals(goals_at[ln-1]);
			for(int i=0;i<exvecsizef;i++)
				gcs[i] = rgcs[i];			
			return FALSE;
		};

		if(search_plan(ln-1,goals_at[ln-1]))
		{
			return TRUE;
		}
		else
		{
#ifdef SYMMETRY
			for(int i = 0;i<=numSymGps[ln-1];i++)
				for(int k = 0;k<symSizes[i];k++)
					triedGps[ln-1][i][k] = '\0';
#endif

			mark_bad_goal_set(ln-1,goals_at[ln-1]);
			int rgcs[EXVECSIZE];
			for(int i = 0;i<exvecsizef;i++)
				rgcs[i] = 0;
			

			for(token_list gs = goals_at[ln-1];gs;gs=gs->next)
			{	if(gs->info->is_dominated(ln-1))
				{
					gs->info->get_noop()->untry_noop(ln-1,gs->info);
				rgcs[gs->info->getwhat_for(ln-1)->expos()] |= gs->info->getwhat_for(ln-1)->exmask();}
				else
				{


				if((gcs[gs->info->expos()] & gs->info->exmask()) || gs->info->is_in_domination(ln-1))
					rgcs[gs->info->getwhat_for(ln-1)->expos()] |= gs->info->getwhat_for(ln-1)->exmask();
				
					
				};
			};
			release_goals(goals_at[ln-1]);
			for(int i=0;i<exvecsizef;i++)
				gcs[i] = rgcs[i];
			return FALSE;
		};
		
	}
	else
	{
		/* Otherwise we have to find an operator for the
			next goal */
//	        tried[ln]++;
//		maxtried[ln]=tried[ln]>maxtried[ln]?tried[ln]:maxtried[ln];

		int conflictset[EXVECSIZE];
		for(int i = 0;i < exvecsizef;i++)
			conflictset[i] = 0;
		
		fact * the_goal = goals->info;
		conflictset[the_goal->expos()] = the_goal->exmask();

		if(the_goal->is_done(ln)){
#ifdef SEARCHDEBUG 
		cout << the_goal->get_name() << " is already done\n";
#endif
 			return search_plan(ln,goals->next);};
		fact * supp;
		if(the_goal->has_noop(ln))
		{
		    supp = the_goal->get_noop()->already_excluded(ln);
		    if (!supp)
		     {
#ifdef SEARCHDEBUG
			cout << "Considering noop for " << the_goal->get_name() << "\n";
#endif

			supp = the_goal->get_noop()->tryop(ln,goals->next,the_goal);
			if(!supp)
			{
				if(search_plan(ln,goals->next))
				{
				       return TRUE;
				}
				else
				{
					if(
#ifdef DDBOFFATWF
						ln==level_out+1 || 
#endif

						(gcs[the_goal->expos()] & the_goal->exmask()))
					{	
						for(int i = 0;i < exvecsizef;i++)
							conflictset[i] |= gcs[i];
						the_goal->get_noop()->untry(ln,the_goal->getnum());
					}
					else
					{	
						the_goal->get_noop()->untry(ln,the_goal->getnum());
						return FALSE;
					};

				};
			}
			else
			{
//				maxtried[ln] = MAX_GOALS;
				conflictset[supp->expos()] |= supp->exmask();
			};
		     }
		     else
			{	conflictset[supp->expos()] |= supp->exmask();
			};
		      			
		};

#ifdef NOREUSE
		the_goal->please_exclude(ln);
#endif

		for(action_list achs = the_goal->achievers(ln);achs > 0;achs=achs->next)
		{	/* Try each action - check its preconditions are OK
				and that it isn't already excluded by other
				action choices */





#ifdef SEARCHDEBUG
			cout << "Considering " << achs->act->get_name() << "\n";
#endif
			supp = achs->act->already_excluded(ln);
			if(supp) 
			{	conflictset[supp->expos()] |= supp->exmask();
				continue;
			};
#ifdef SEARCHDEBUG
			cout << "Trying it\n";
#endif

#ifdef SYMMETRY
			if(achs->act->unusable(ln)) continue;
#endif

			supp = achs->act->tryop(ln,goals->next,the_goal);
			if(supp)
			{	// maxtried[ln] = MAX_GOALS;
				conflictset[supp->expos()] |= supp->exmask();
				continue;
			};


			if(search_plan(ln,goals->next))
				return TRUE;
			if(
#ifdef DDBOFFATWF
				ln==level_out+1 || 
#endif
				(gcs[the_goal->expos()] & the_goal->exmask()))
			{	
				for(int i = 0;i < exvecsizef;i++)
					conflictset[i] |= gcs[i];
				achs->act->untry(ln,the_goal->getnum());
			}
			else
			{	
				achs->act->untry(ln,the_goal->getnum());
#ifdef SEARCHDEBUG
			cout << "Untried " << achs->act->get_name() << "\n";
#endif


				return FALSE;
			};

#ifdef SEARCHDEBUG
			cout << "Untried " << achs->act->get_name() << "\n";
#endif


		};
//		tried[ln]--;
#ifdef NOREUSE
		the_goal->dont_exclude(ln);

#endif

#ifdef SYMMETRYCONTEXT
/* Recall goals are stored and tried in decreasing fe order. */

		for(int i = 1;i<=numSymGps[ln];i++)
			if(triedGps[ln][i] <= 1+the_goal->getnum()) 
				triedGps[ln][i] = 0;

#endif


		for(int i = 0;i < exvecsizef;i++)
			gcs[i] = conflictset[i];

	};

#ifdef SEARCHDEBUG
	cout << "No plan at level " << ln << "\n";
#endif

	/* Nothing doing so we failed */

	return FALSE;

};


/* Initialise... */

void start_search()
{
	int i;
	for(i=0;i<MAX_PLAN;i++)
	{
		goals_at[i] = 0;
		committed_acts_at[i] = 0;
	};

#ifdef NOREUSE
	no_reuse_vec[0] = new int[EXVECSIZE];
	no_reuse_vec[1] = new int[EXVECSIZE];

	for(i=0;i<EXVECSIZE;i++)
	{
		no_reuse_vec[0][i] = 0;
		no_reuse_vec[1][i] = 0;
	};
#endif
	for(i = 0;i<EXVECSIZE;i++)
		gcs[i] = 0;
};


/* Start it off... */

int search(int ln,token_list goals)
{
	top_level = ln;

#ifdef ACTIONSETMINIMAL
/* This is just to make sure the goals are all accessible for marking if the
action_set_minimal test identifies a problem. Also see candidates.cc. */
	goals_at[ln] = goals;
#endif

	if(is_bad_goal_set(ln,goals)) return FALSE;
	cout<<"Passing bad goal set..."<<endl;
        for(int i = 0;i<exvecsizef;i++)
		gcs[i] = 0;
	if(!mark_dominations(ln,goals))
		{
	
			for(token_list gls = goals;gls;gls=gls->next)
			{
		 	  	if(gls->info->is_dominated(ln))
					gls->info->get_noop()->untry_noop(ln,gls->info);
				if(gls->info->is_in_domination(ln))
					gcs[gls->info->expos()] |= gls->info->exmask();
			};
			mark_bad_goal_set(ln,goals);	
			return FALSE;
		};
        cout<<"if not mark dominations..."<<endl; 
//	tried[ln] = 0;
//	maxtried[ln] = 0;
	 //I commented this ... This is the original search from stan...
         	
     	if(search_plan(ln,goals)) return TRUE;
#ifdef SEARCHDEBUG
	cout << "Marking bad goals\n";
#endif
	mark_bad_goal_set(ln,goals);
#ifdef SEARCHDEBUG
	cout << "Bad goals marked\n";
#endif

 	for(;goals;goals=goals->next)
		if(goals->info->is_dominated(ln))
			goals->info->get_noop()->untry_noop(ln,goals->info);
       
#ifdef SYMMETRY
	for(int i = 0;i<=numSymGps[ln];i++)
		for(int k = 0;k<symSizes[i];k++)
			triedGps[ln][i][k] = '\0';

#endif

	return FALSE;

};


/* The main invocation point for the search process. */

int find_plan(token_list initst,token_list goals)
{
        
        start_search();

	int level = build_initial_segment(initst,goals);
          
        if(level<0) return -1;

/* the goals at the top level are not sorted in spike order, so before search they must be sorted. Accumulate goals does this. We mark all the goals, accumulate them (sort them) then unmark them and release original goal set to reclaim the space. The sort
ed list is then the top level goal set. */

	for(token_list gs = goals;gs;gs=gs->next)
	{
		gs->info->mark_goal(level,0);
	};

	accumulate_goals(level);
		
	for(token_list gs = goals;gs;gs=gs->next)
	{
		gs->info->unmark_goal(level);
	};

	release_goals(goals);
	goals = goals_at[level];
          
        retrieveValidActions(level);
	if(GLEVEL==0) cout<<"Graph built until levels off..."<<endl;
        else cout<<"Graph built until level: "<<level<<endl;
/*RS: Added by RS*/     	
/* ALTALT */
//It needs to extend the planning graph until levels off, unless specified other thing in the command line 
if(WHICHSEARCH!=HSPBASED)         
        while(!search(level,goals))
	{
 cout<<"Building another segment of the graph.."<<endl;
	  level++;
		build_layer(level);
#ifndef WFOFF		 
		    if(facts_at[level] == facts_at[level-1] && 
			mutex_at[level] == mutex_at[level-1])
			{level_out = level-1;break;};
		 
#endif

	};
        //Calculating graph time construction.   
	if(WHICHSEARCH==HSPBASED)
        {
	  gettimeofday(&pg_tend,0);
          totalgp=(pg_tend.tv_sec - pg_tstart.tv_sec) * 1000 + (pg_tend.tv_usec-pg_tstart.tv_usec)/1000;
	  gettimeofday(&hsp_tstart,0);
	  //Initializing time data structure to start regression search phase.
	  cout<<"Initialize HSP search..."<<"\n";          
          return startGreedyBFS(level,initst,goals);
        } 
     /* RS: End */       

	if(level_out)
	{
#ifdef VERBOSE
		cout << "Leveled at " << level_out << "\n";
#endif

		reset(goals);

		cands = new candidate_list_node(goals,cands);
		lastcand = cands;

		hits = 0;
		oldhits = 0;

		while(TRUE)
		{
			candidate_list_node * cs = cands;
			candidate_list_node * oldcs = 0;
			cands = 0;
			lastcand = 0;
			top_level = level;



			while(cs)
			{	
				if(cs->cand.extend())
				{
#ifdef FINALSTATS
					final_level_count = level;
#endif

					return level;
				};


				oldcs = cs;
				cs = cs->next;
				delete oldcs;

			};

			if(hits==oldhits) return -1;
			oldhits = hits;

			level++;
#ifdef VERBOSE
			cout << "Layer " << level << "\n";
#endif

		};
	};

#ifdef FINALSTATS
	final_level_count = level;
#endif

	return level;
};


