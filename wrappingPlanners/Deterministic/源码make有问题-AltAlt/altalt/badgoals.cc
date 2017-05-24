/* 
* $Revision: 3.1 $
* $Date: 1999/01/06 10:15:43 $

badgoals.cc */

#include "search.h"
#include "badgoals.h"
#include "facts.h"
#include "candidates.h"

bgTrie bad_goals[MAX_PLAN];
bgTrie already_seen;


bgTrie::~bgTrie()
{
	if(root) delete root;
};

void bgTrie::reset(token_list goals)
{
	if(bad_goals[level_out].root) 
	{
		delete bad_goals[level_out].root;
		bad_goals[level_out].root = 0;
	};
#ifdef BADDEBUG
			cout << "Marking bad goals during reset at " << level_out << "\n";
#endif

	bad_goals[level_out].insert(level_out,goals);

#ifndef EBLOFFATWF
	already_seen.insertall(goals);
#endif

};

void reset(token_list goals)
{
	bad_goals[level_out].reset(goals);
};

void bgTrie::show()
{
	cout << "[";
	for(goalcell * x = root;x;x = x->getbranch().root)
	{
		cout << fact_table[x->getval()]->get_name() << " ";
		x->getchain().show();
	};
	cout << "]";
};

int bgTrie::insert(int ln,token_list tl)
{	if(tl)
        {
	  if (
#ifdef EBLOFFATWF
		ln == level_out || 
#endif
		(gcs[tl->info->expos()] & tl->info->exmask())
				|| tl->info->is_in_domination(ln))
	    {
	        if(!root)
		{
			root = new goalcell(tl->info->getnum());
#ifdef BADDEBUG
			cout << tl->info->get_name() << " ";
#endif
			return root->setnewlength(root->getchain().insert(ln,tl->next));
		}
		else
		{	if(tl->info->getnum()==root->getval())
			{	
#ifdef BADDEBUG
			cout << tl->info->get_name() << " ";
#endif
				return root->setlength(root->getchain().insert(ln,tl->next));
			}
			else
			{	return root->getbranch().insert(ln,tl);
			};
		};
	   }
	   else
	     {
	        return insert(ln,tl->next);
	     };
 	}
	else {
#ifdef BADDEBUG
		 cout << "\n";
#endif
	  return 0;};
};
	
int bgTrie::insertall(token_list tl)
{	if(tl)
        {
	        if(!root)
		{
			root = new goalcell(tl->info->getnum());
#ifdef BADDEBUG
			cout << tl->info->get_name() << " ";
#endif
			return root->setnewlength(root->getchain().insertall(tl->next));
		}
		else
		{	if(tl->info->getnum()==root->getval())
			{	
#ifdef BADDEBUG
			cout << tl->info->get_name() << " ";
#endif
				return root->setlength(root->getchain().insertall(tl->next));
			}
			else
			{	return root->getbranch().insertall(tl);
			};
		};
 	}
	else {
#ifdef BADDEBUG
		  cout << "\n";
#endif
	  return 0;};
};



int bgTrie::find(token_list tl,int tll)
{
	if(!tl || !root)
		return FALSE;

	if(root->getval()==tl->info->getnum() && root->getlength() <= tll)
	{


		gcs[tl->info->expos()] |= tl->info->exmask();

		if (root->getlength() == 1)
			return TRUE;

		if (root->getchain().find(tl->next,tll-1))
			return TRUE;
		else 
		{
			gcs[tl->info->expos()] ^= tl->info->exmask();
			return root->getbranch().find(tl->next,tll-1);
		};
	}
	else
	{	
		return (root->getbranch().find(tl,tll) || 
			 (root->getval() < tl->info->getnum() && find(tl->next,tll-1)));
// NOTE: THE < IN THE LAST LINE IS DEPENDENT ON THE REVERSE GOAL ORDERING
// DEFINED IN SEARCH.CC ACCUMULATE_GOALS: SWITCH TO > OTHERWISE.
	
	};
};

int bgTrie::findit(token_list tl,int tll)
{
	if(!tl || !root)
		return FALSE;

	if(root->getval()==tl->info->getnum() && root->getlength() <= tll)
	{

		if (root->getlength() == 1)
			return TRUE;

		if (root->getchain().findit(tl->next,tll-1))
			return TRUE;
		else 
		{
			return root->getbranch().findit(tl->next,tll-1);
		};
	}
	else
	{	
		return (root->getbranch().findit(tl,tll));
// || 
//			 (root->getval() < tl->info->getnum() && findit(tl->next,tll-1)));
// NOTE: THE < IN THE LAST LINE IS DEPENDENT ON THE REVERSE GOAL ORDERING
// DEFINED IN SEARCH.CC ACCUMULATE_GOALS: SWITCH TO > OTHERWISE.
	
	};
};


int marked_bad_goal_set(int ln,token_list goals)
{
	/* First check fixed resources */

	int x;
	int size_of;

#ifdef CANDSTATS
	newlowest(ln+1);
#endif


	if(frCount>0 || rStart>0)
	{ 
		for(x=0;x<rCount;x++)
		{
			fixedres[x] = 0;
			appres[x] = 0;
		};

		for(token_list gs = goals;gs;gs=gs->next)
		{
			fixedres[gs->info->getfrnum()]++;
			appres[gs->info->getfrnum()] += gs->info->getresval();
		};

		if(frCount)
			for(x=1;x<frCount;x++)
				if(fixedres[x]>fixedreslim[x])
				{
#ifdef FIXEDRES
					cout << "Rejecting (fr): ";
					for(token_list gs = goals;gs;gs = gs->next)
						cout << gs->info->get_name() << " ";
					cout << "at level "<< ln << "\n";
#endif
					for(token_list gs = goals;gs;gs = gs->next)
						gcs[gs->info->expos()] |= gs->info->exmask();
					return TRUE;
				};

		if(rStart)
			for(x=rStart;x<rCount;x++)
				if(appres[x] > apprate[x]*ln)
				{
#ifdef FIXEDRES
					cout << "Rejecting (ar): ";
					for(token_list gs = goals;gs;gs = gs->next)
						cout << gs->info->get_name() << " ";
					cout << "at level "<< ln << "\n";
#endif


					if(ln==level_out) 
					{
						size_of = 0;

						for(token_list gs = goals;gs>0;gs=gs->next)
						{	
								size_of++;
						};
#ifdef EBLOFFATWF
						if(!bad_goals[ln].find(goals,size_of))
#endif
#ifndef EBLOFFATWF
						if(!already_seen.findit(goals,size_of))
#endif
						{

#ifdef BADDEBUG
	cout << "(1) Creating candidate at level: " << ln << "\n";
#endif	
	
#ifdef EBLOFFATWF
				 		bad_goals[ln].insert(ln,goals);
#endif
							hits++;

							cands = add_candidate(goals,committed_acts_at,cands);
						};
					};

					for(token_list gs = goals;gs;gs = gs->next)
						gcs[gs->info->expos()] |= gs->info->exmask();
					return TRUE;
				};
	};

	/* Create a vector of goals, compress - then check the 
		vectors in the hash table. */


	size_of = 0;
	for(token_list gs = goals;gs;gs=gs->next)
	{	
		size_of++;
	};

#ifdef EBLOFFATWF
	return bad_goals[ln].find(goals, size_of);
#endif

#ifndef EBLOFFATWF
	if(bad_goals[ln].find(goals, size_of))
	{
		if(level_out==ln && !already_seen.findit(goals, size_of))
		{
#ifdef BADDEBUG
			cout << "(2) Creating candidate at level: " << ln << "\n";
#endif
			hits++;
			cands = add_candidate(goals,committed_acts_at,cands);
		};

		return TRUE;			
	}
	else
	{
		return FALSE;
	};
#endif
};

int is_bad_goal_set(int ln,token_list goals)
{
	/* First check fixed resources */

	int size_of;
	int x;

	if(frCount>0 || rStart>0)
	{ 
		for(x=0;x<rCount;x++)
		{
			fixedres[x] = 0;
			appres[x] = 0;
		};

		for(token_list gs = goals;gs;gs=gs->next)
		{
			fixedres[gs->info->getfrnum()]++;
			appres[gs->info->getfrnum()] += gs->info->getresval();
		};

		if(frCount)
			for(x=1;x<frCount;x++)
				if(fixedres[x]>fixedreslim[x])
				{
#ifdef FIXEDRES
					cout << "Rejecting (fr): ";
					for(token_list gs = goals;gs;gs = gs->next)
						cout << gs->info->get_name() << " ";
					cout << "at level "<< ln << "\n";
#endif
					return TRUE;
				};

		if(rStart)
			for(x=rStart;x<rCount;x++)
				if(appres[x] > apprate[x]*ln)
				{
#ifdef FIXEDRES
					cout << "Rejecting (ar): ";
					for(token_list gs = goals;gs;gs = gs->next)
						cout << gs->info->get_name() << " ";
					cout << "at level "<< ln << "\n";
#endif
					if(ln==level_out) 
					{
						size_of = 0;


						for(token_list gs = goals;gs>0;gs=gs->next)
						{
							size_of++;
						};
#ifdef EBLOFFATWF
						if(!bad_goals[ln].find(goals,size_of))
#endif
#ifndef EBLOFFATWF
						if(!already_seen.findit(goals,size_of))
#endif

						{
#ifdef BADDEBUG
			cout << "Marking bad goals (res) at " << ln << "\n";
#endif

#ifdef EBLOFFATWF
							bad_goals[ln].insert(ln,goals);
#endif
							hits++;

							cands = add_candidate(goals,committed_acts_at,cands);

//							cout << (cands->cand) << "\n";


						};
					};

					return TRUE;
				};
	};

	return FALSE;

};




/* Once a bad goal set is found, we can record it with this function. */


void mark_bad_goal_set(int ln,token_list goals)
{
#ifdef BADSTATS
	badstats[ln]++;
#endif


#ifdef BADDEBUG
	cout << "Marking bad goals at level " << ln << ": \n";
#endif

	bad_goals[ln].insert(ln,goals);


	int size_of = 0;
	for(token_list gls = goals;gls;gls=gls->next)
		size_of++;

	if(ln==level_out 
#ifndef EBLOFFATWF
		&& !already_seen.findit(goals,size_of)
#endif
		)
	{

#ifdef BADDEBUG
		cout << "(3) Creating candidate at level: " << ln << "\n";
#endif

		hits++;
		cands = add_candidate(goals,committed_acts_at,cands);

	};

};
