/* 
* $Revision: 3.1 $
* $Date: 1999/01/06 10:15:43 $


construction.cc

This file defines the main functions involved in construction of the plan
graph, apart from the individual structure management functions for facts
and actions.

Highlights are the build_layer and build_initial_segement functions
responsible for overseeing most of the process. Apart from that, there
is a fair bit of mutex checking stuff, which is often pretty expensive. */

#include "globals.h"
#include "facts.h"
#include "actions.h"
#include "construction.h"


int changedActs[EXVECSIZE];

/* Start up the graph */

void set_initial_facts(token_list initstate)
{
	int fact_num = 0;

	for( ;initstate > 0;initstate=initstate->next) 
	{
		fact_table[fact_num] = initstate->info;
		initstate->info->initialise(0,fact_num++);

	         /*RS: Added by RS*/
		/* ALTALT */
		/* Heuristic values of the initial propositions are initialized to 0*/
		initstate->info->setSumH(0);
		initstate->info->setMaxV(0);
                /* RS: End */
		
		no_facts++;
#ifdef PDDLOUTPUT	
		if(no_facts>MAX_FACTS) memerr();
#endif
		exvecsizef = 1+(no_facts-1)/32;
	};

	flevel[0] = new fact_level_info[fact_num];
	for(int i = 0;i<fact_num;i++)
		flevel[0][i].initialise(0);
        
	facts_at[0] = fact_num;
	acts_at[0] = 0;
	mutex_at[0] = 0;
};

void start_layer(int ln)
{

	create_action_layer(ln,acts_at[ln-1],acts_at[ln]);
	create_fact_layer(ln,facts_at[ln]);

#ifdef NOREUSE
	no_reuse_vec[ln+1] = new int[EXVECSIZE];

	for(i=0;i<EXVECSIZE;i++)
		no_reuse_vec[ln+1][i] = 0;
#endif

#ifdef SYMMETRY
	triedGps[ln] = new char*[matchedSymGps+1];
	numSymGps[ln] = matchedSymGps;
	for(int i = 0;i<matchedSymGps+1;i++)
	{	triedGps[ln][i] = new char[symSizes[i]];
		for(int k = 0;k<symSizes[i];k++)
			triedGps[ln][i][k] = '\0';
	};
#endif


};

/* Creation of noops for new facts at preceding layer. Note the
treatment of special case when ln==1, since facts_at[-1] is 
undefined. */

void doNoops(int ln)
{
	int i = ln==1?0:facts_at[ln-2];
	for(;i<facts_at[ln-1];i++)
	{	action_table[acts_at[ln]] = new action(acts_at[ln],ln,fact_table[i]);
		acts_at[ln]++;
	};
};

void set_mutex_actions_serial(int ln);
void set_mutex_actions(int);
void set_mutex_facts(int);

/* So here is one of the key functions. It only bothers to check
actions which are still outstanding - once applied an action is 
taken out of the list. If applicable, an action is enacted. Then
all mutex checking is carried out. */

void build_layer(int ln)
{
	action_list candidate_acts = available_acts;
	action_list previous_act = 0;
	action * candidate_act;

	facts_at[ln] = facts_at[ln-1];
	acts_at[ln] = acts_at[ln-1];
	mutex_at[ln] = 0;

#ifdef DEBUG
	cout << "About to build a layer\n";
#endif

#ifdef DEBUG1
	cout << "Layer " << ln << " started\n";
#endif

	doNoops(ln);
#ifdef DEBUG
	cout << "Noops complete\n";
#endif
	for(;candidate_acts>0;candidate_acts = candidate_acts->next)
	{
		candidate_act = candidate_acts->act;
		if(candidate_act->applicable(ln))
		{
			candidate_act->enact(ln);

			if(previous_act)
			{
				previous_act->next = candidate_acts->next;
				delete candidate_acts;
				candidate_acts = previous_act;
			}
			else
			{
				available_acts = candidate_acts->next;
			};
		}
		else
		{
			previous_act = candidate_acts;
		};
	};

	start_layer(ln);

	if(SERIAL)
	  set_mutex_actions_serial(ln);
	else
	  set_mutex_actions(ln);
	
        set_mutex_facts(ln);

#ifdef DEBUG1  
	cout << "\n\nLayer: "<<ln<<"\n";
	for(int i = 0; i < facts_at[ln];i++)
		cout << "Fact: "<<fact_table[i]->get_name() << "  Sum Value:  "<<fact_table[i]->getSumH()<<"  Level value: "<<fact_table[i]->get_when()<<"\n";
	cout << "\n";
	
#endif 

#ifdef VERBOSE
	cout << "Level " << ln << ":\tFacts: " << facts_at[ln] << " Actions: " << acts_at[ln] << " Mutex pairs: " << mutex_at[ln] << "\n";
#endif

};

void check_still_mutex_pres(int ln,action * a,action_list b)
{
	/* If we keep a vector showing the preconditions of an action then
	we can check this one by or-ing together all the exclusion vectors
	for the preconditions of a and then looking for a hit with a precondition
	of b. The checkvec stores precisely this information. */

	action * bb = b->act;

	if(!bb->check_mutx_vec(a->checkvec(ln)))
	{
		a->reset_mutex(ln,bb);
		bb->reset_mutex(ln,a);
		//cout<<"!In still mutex "<<a->get_name()<<endl;
		delete b;
	}
	else
	{
		a->link_exclude(ln,b);
		bb->add_exclude(ln,a);
	};

};

/* Temporary mutex actions are those mutex only because of mutex preconditions,
which is not necessarily a persistent feature. Of course, we could exploit the
information coming out of the state-analysis which says which facts are 
permanently mutex and then work out which actions are permanently mutex as
a result. This would be worth doing for speed up. */

void check_temp_mutex(int ln,action * a,action * b)
{
	if(b->check_mutx_vec(a->checkvec(ln)))
	{
		a->set_mutex(ln,b);
		b->set_mutex(ln,a);
       	 //cout<<"Setting dyn. mutex action at level "<<ln<<" ACT: "<<a->get_name()<<" with: "<<b->get_name()<<endl;
#ifdef DEBUG
		cout << a->get_name() << " and " << b->get_name() << " mutex at level " << ln << "\n";
#endif

	};

};

/* Checking for interaction between pres and posts for two actions. */

int check_PAD(action * a, action * b)
{
	return a->checkPAD(b);

/*
	for(int i=0;i<exvecsizef;i++)
		if(((a->adds_vec()[i] | a->precs_vec()[i]) & b->dels_vec()[i]) ||
		   ((b->adds_vec()[i] | b->precs_vec()[i]) & a->dels_vec()[i]))
			return TRUE;
	return FALSE;
*/
};

/* Check whether two actions invert one-another. For example, put a on b from
c and put a on c from b invert one-another and so it is pointless using one
to achieve the preconditions of the other at the preceding layer. This is
something that happens surprisingly often - most actions have a direct inverse,
and the search will always try using this inverse to achieve the preconditions
of the original action. Spotting this and stopping it is well worth-while. */

int check_inversion(action * a, action * b)
{
	for(int i = 0;i<exvecsizef;i++)
		if((a->adds_vec()[i] ^ b->dels_vec()[i]) | (a->dels_vec()[i] ^ b->adds_vec()[i])) return FALSE;

	return TRUE;
};


int check_perm_mutex(int ln,action * a, action * b)
{
	/* If we use a vector for adds and dels then we can check this
	easily too. 

	Note that the delete vectors must be updated when a new fact is
	added which is deleted by an action that is already active. */

//	if(check_adds_dels(a,b) || check_precs_dels(a,b))
  
        if(a->checkPAD(b))
	{
		a->set_perm_mutex(ln,b);
		b->set_perm_mutex(ln,a);

#ifdef DEBUG
		cout << a->get_name() << " and " << b->get_name() << " permanently mutex\n";
#endif


		if(check_inversion(a,b))
		{
#ifdef INVERSIONS
		 	cout << a->get_name() << " and " << b->get_name() <<
						" mutual inverses\n";
#endif

			a->set_inversion(ln,b);
			b->set_inversion(ln,a);
		};

		return TRUE;
	};
	return FALSE;
};


/*RS:Added by RS */
/* ALTALT */
/* Forcing two actions become mutex at level ln */
int force_perm_mutex(int ln,action * a, action * b)
{

  //Forcing two actions to become mutex, This is done for the SERIAL GRAPH.  
  if(!a->is_noop()&&!b->is_noop())
    {  
        a->set_perm_mutex(ln,b);
      	b->set_perm_mutex(ln,a);
	if(check_inversion(a,b))
	{
	  a->set_inversion(ln,b);
	  b->set_inversion(ln,a);
	};
       	return TRUE;
    }
  else
    {
      if(a->checkPAD(b))
	{
		a->set_perm_mutex(ln,b);
		b->set_perm_mutex(ln,a);

#ifdef DEBUG
		cout << a->get_name() << " and " << b->get_name() << " permanently mutex\n";
#endif

		if(check_inversion(a,b))
		{
#ifdef INVERSIONS
		 	cout << a->get_name() << " and " << b->get_name() <<
						" mutual inverses\n";
#endif
			a->set_inversion(ln,b);
			b->set_inversion(ln,a);
		};

		return TRUE;
	};
    };
    return FALSE;
};

/*RS Added */
/* ALTALT */
/* Making planning graph to be a SERIAL planning graph */
/* A Serial planning graph forces every pair of actions (exception Noops) become */
/* pairwise mutexes with each other, such that only one action can be executed at a time*/
void set_mutex_actions_serial(int ln)
{
	int i,j;
  	for(i = 0;i<acts_at[ln-1];i++)
 	{
	  if(changedActs[action_table[i]->expos()] & action_table[i]->exmask())
		{
		  action_list as = action_table[i]->get_tmp_excludeds();
		  action_list bs;
		  while(as)
		    {
 			bs = as->next;
			if(action_table[i]->is_noop()||as->act->is_noop())
			{
			  if(as->act->get_entry()>i &&
				(changedActs[action_table[i]->expos()] &
					action_table[i]->exmask()))
			    {
				action_table[i]->check_still_mutex_pres(ln,as);
			    }
			  else
			    {
				if(changedActs[as->act->expos()] & as->act->exmask())
				{
					delete as;
				}
				else
				{
					action_table[i]->link_exclude(ln,as);
				};
			    };
			}
			as = bs;
		    };
		}
	      else
	       {
		 action_table[i]->set_excludeds(ln);
	       };
 	};


/* Now check new actions against all others: all conditions to be checked */
/* If there are some actions that still neeed to be checked, just mark them mutexes */		
	    for(;i<acts_at[ln];i++)
		for(j=0;j<i;j++)
	             if(!force_perm_mutex(ln,action_table[i],action_table[j]))
			check_temp_mutex(ln,action_table[i],action_table[j]);
	
	for(i=0;i<acts_at[ln];i++)
		action_table[i]->set_excs(ln);
	
};

/*RS END */


void set_mutex_actions(int ln)
{
	/* Two actions are mutex if: 1) They have conflicting add/del effects.
				     2) They have conflicting del/pres.
				     3) They have mutex pres.
		Note that two actions cannot be mutex if they were not mutex 
		in the preceding layer, if they are old actions.

		Note also that reasons 1 and 2 are permanent. This means we
		can divide reasons for mutex into two groups and only retest
		mutex preconditions again.
	*/
	int i,j;

	/* First check old actions - only condition 3 needs checking */

	/*RS: Added by RS*/
  	for(i = 0;i<acts_at[ln-1];i++)
 	{
 	   if(changedActs[action_table[i]->expos()] & action_table[i]->exmask())
 	   {
 		action_list as = action_table[i]->get_tmp_excludeds();
 		action_list bs;
 		while(as)
 		{
 			bs = as->next;
			if(as->act->get_entry()>i &&
				(changedActs[action_table[i]->expos()] &
					action_table[i]->exmask()))
			{
				action_table[i]->check_still_mutex_pres(ln,as);
			}
			else
			{
				if(changedActs[as->act->expos()] & as->act->exmask())
				{
					delete as;
				}
				else
				{
					action_table[i]->link_exclude(ln,as);
				};
			};

			as = bs;
		};
 	    }
 	    else
 	    {
 		action_table[i]->set_excludeds(ln);
 	    };
 	};


	/* Now check new actions against all others: all conditions to be checked */

	    for(;i<acts_at[ln];i++)
		for(j=0;j<i;j++)
	           if(!check_perm_mutex(ln,action_table[i],action_table[j]))
			check_temp_mutex(ln,action_table[i],action_table[j]);

	for(i=0;i<acts_at[ln];i++)
		action_table[i]->set_excs(ln);
};

void set_mutex(int ln,fact * f,fact * g)
{
	f->set_mutex(g,ln,g->expos(),g->exmask());
	g->set_mutex(f,ln,f->expos(),f->exmask());

#ifdef DEBUG
	cout << "Setting "<<f->get_name()<< " and "<<g->get_name()<< " mutex at level " << ln<<"\n";
#endif
	mutex_at[ln]++;

};

/* Goal ordering: see whether the achievers of one fact all undo another fact.
If so, then there is no point in achieving the latter fact first. This orders
these facts. This calculation is done layer-by-layer, and allows us to insist
on certain facts being done by noops in a layer, and also to spot plan-length
restrictions in some cases. */

void check_ordered_facts(int ln,fact * f,fact * g)
{
#ifdef DOMINATION
	cout << "Checking " << f->get_name() << " " << g->get_name() << "\n";
#endif

	action_list ach,ach1;

	for(ach = f->achievers(ln);ach;ach=ach->next)
	{
		for(ach1 = g->achievers(ln);ach1;ach1=ach1->next)
			if(!(ach->act->excludeds(ln)[ach1->act->expos()] & ach1->act->exmask()))
				break;
		if(ach1) break;
	};

	if(!ach)
	{

		if(f->has_noop(ln))
		{

#ifdef DOMINATION
		cout << f->get_name() << " has noop\n";
#endif
			if(g->ordered_by(f->get_noop()->excludeds(ln)))
			{
#ifdef DOMINATION
				cout << g->get_name() << " < " << f->get_name() << " at " << ln << "\n";
#endif
				f->add_dominate(ln,g);
			};
		};

		if(g->has_noop(ln))
		{

#ifdef DOMINATION
		cout << g->get_name() << " has noop\n";
#endif
			if(f->ordered_by(g->get_noop()->excludeds(ln)))
			{
#ifdef DOMINATION
				cout << f->get_name() << " < " << g->get_name() << " at " << ln << "\n";
#endif
				g->add_dominate(ln,f);
			};
		};
#ifdef DOMINATION
	}
	else
	{
		cout << "OK because: " << ach->act->get_name() << " and " << ach1->act->get_name() << "\n";
#endif
	};
};




int check_mutex_facts(int ln,fact * f,fact * g)
{
	action_list ach = f->achievers(ln);

	int acexvec[exvecsizea];

	if(f->has_noop(ln))
	{
		for(int i = 0;i<exvecsizea;i++)
			acexvec[i] = f->get_noop()->excludeds(ln)[i];
	}
	else
	{
		if(ach)
		{
			for(int i = 0;i<exvecsizea;i++)
				acexvec[i] = ach->act->excludeds(ln)[i];

			ach = ach->next;
		}
		else
		{
			for(int i = 0;i<exvecsizea;i++)
				acexvec[i] = 0;
		};
	};

	for(;ach;ach=ach->next)
		for(int i = 0;i<exvecsizea;i++)
			acexvec[i] &= ach->act->excludeds(ln)[i];

	if(g->excluded_by(acexvec))
	{
		set_mutex(ln,f,g);

#ifdef MUTEXFACTSDEBUG
	cout << "Mutex at level " << ln << ": " << f->get_name() << " " << g->get_name() << "\n";
	cout << "Achievers of "<<f->get_name() << " are:\n";
	for(ach = f->achievers(ln);ach>0;ach=ach->next)
		cout << "\t"<< ach->act->get_name()<<"\n";
	cout << "Achievers of "<<g->get_name() << " are:\n";
	for(ach = g->achievers(ln);ach>0;ach=ach->next)
		cout << "\t"<< ach->act->get_name()<<"\n";
#endif
		return TRUE;
	}
	else{
		if(f->has_noop(ln) && g->excluded_by(f->get_noop()->excludeds(ln)))
				 check_ordered_facts(ln,f,g);

#ifdef MUTEXFACTSDEBUG
cout << g->get_name() << " ok with " << f->get_name() << "\n";

#endif

		return FALSE;

	};
};


void set_mutex_facts(int ln)
{
	/* Two facts are mutex if: 1) They are only achieved by mutex actions.
		Note that two facts will never be mutex unless they were mutex
		at the preceding layer, if they are old facts.

		If two facts are mutex because all their achievers are permanently
		mutually exclusive and there are no new achievers then they remain
		mutually exclusive. For the moment we'll not try to deal with this
		possible efficiency gain.
	*/

	int i,j;

	for(i=0;i<exvecsizea;i++)
		changedActs[i] = 0;

	for(i=0;i<facts_at[ln-1];i++)
		for(j=i+1;j<facts_at[ln-1];j++)
			if(fact_table[i]->is_mutex(ln-1,fact_table[j]->expos(),fact_table[j]->exmask()))
			  {
				if(!check_mutex_facts(ln,fact_table[i],fact_table[j]))
				{
					int * xi = fact_table[i]->get_consumers();
					int * xj = fact_table[j]->get_consumers();
	
					for(int k = 0;k<exvecsizea;k++)
						changedActs[k] |= (xi[k] | xj[k]);
				};

#ifdef DEBUG
				cout << "Checked old mutex facts: " << fact_table[i]->get_name() << " and " << fact_table[j]->get_name() << "\n";
#endif
			      }
			else
			{
				if(fact_table[i]->dominates(ln-1,fact_table[j]) ||
				   fact_table[j]->dominates(ln-1,fact_table[i]) ||
				   !fact_table[i]->has_noop(ln-1) || !fact_table[j]->has_noop(ln-1))
					check_ordered_facts(ln,fact_table[i],fact_table[j]);
			};

	for(;i<facts_at[ln];i++)
		for(j=0;j<i;j++)
			check_mutex_facts(ln,fact_table[i],fact_table[j]);
};

/* Check for all goals. This code is rather aged and could do with some
tidying up to exploit the vectors better: we could construct a vector
of goals and a vector of their or-ed mutex vectors, and then and these
together to check for mutex pairs. Non-urgent, since this is hardly a
costly function. */

int all_possible(int ln,token_list goals)
{
	token_list g2;

	for(;goals > 0;goals = goals->next)
		if(goals->info->is_achieved(ln))
		{
			for(g2=goals->next;g2>0;g2=g2->next)
			if(!g2->info->is_achieved(ln) || goals->info->is_mutex(ln,g2->info->expos(),g2->info->exmask()))
			{
#ifdef DEBUG
				if(g2->info->is_achieved(ln))
				{
					cout << "Level " << ln << ": " << goals->info->get_name() << " and " << g2->info->get_name() << " mutex\n";
				}
				else
				{
					cout << "Level " << ln << ": " << g2->info->get_name() << " unachieved\n";
				};
#endif
				return FALSE;
			};
		}
		else
		{
#ifdef DEBUG
			cout << "Unachieved goal at level " << ln << ": " << goals->info->get_name()<<"\n";
#endif
			return FALSE;
		};

#ifdef DEBUG
	cout << "Goals all possible at " << ln << "\n";
#endif

	return TRUE;
};

/* A major coordinator: build the graph up until all goals are achievable
and non-pairwise mutex. */

int build_initial_segment(token_list initst,token_list goals)
{
	set_initial_facts(initst);
	setvec();
	int level = 0;
	while(!all_possible(level,goals))
	{
		level++;
		build_layer(level);
                	
#ifdef DEBUG
		cout << facts_at[level] << " " << facts_at[level-1]<<"\n"
			<< mutex_at[level] << " " << mutex_at[level-1]<<"\n";
#endif	
		if(facts_at[level]==facts_at[level-1]&&acts_at[level]==acts_at[level-1]&& mutex_at[level]==mutex_at[level-1])
		  {
		     return -1;
		  }
		
	};


        /* RS: Added by RS*/
        graphbuilt=TRUE; 
         
#ifdef FINALSTATS
	initial_length = level;
#endif

#ifdef VERBOSE
	cout << "Initial layer complete at: " << level << "\n";
#endif

#ifdef HSPSEARCH
	    cout<<"Expanding the graph until levels off ..."<<endl;
	    if(!NOLEVOFF)
	      {
		if(level<=GLEVEL)
		  {
		    while(level<GLEVEL)
		      {
			level++;
			build_layer(level);           
		      }
		  }
		else
		  {
 
		    while(!level_out)
		      {
			level++;
			build_layer(level);
               
			if(facts_at[level]==facts_at[level-1]&& mutex_at[level]==mutex_at[level-1])
			  {
			    level_out=level-1;
			    break;
			  }
		      }
		  }
	      }
#endif
/*RS: End */
	return level;
};



void copy_layer(int ln)
{
	facts_at[ln] = facts_at[ln-1];
	acts_at[ln] = acts_at[ln-1];
	mutex_at[ln] = mutex_at[ln-1];

#ifdef NOREUSE
	no_reuse_vec[ln+1] = new unsigned long[EXVECSIZE];

	for(int i=0;i<EXVECSIZE;i++)
		no_reuse_vec[ln+1][i] = 0;
#endif

#ifdef DEBUG
	cout << "About to copy a layer\n";
#endif
	for(int i=0;i<facts_at[ln];i++)
		fact_table[i]->copy(ln);
	for(int i = 0;i<acts_at[ln];i++)
		action_table[i]->copy(ln);

#ifdef DEBUG1
	cout << "Layer " << ln << " copied over\n";
#endif

#ifdef DEBUG
	cout << "\n\nLayer: "<<ln<<"\n";
	for(int i = 0; i < facts_at[ln];i++)
		cout << fact_table[i]->get_name() << "\n";
	for(int i = 0; i < acts_at[ln];i++)
		cout << action_table[i]->get_name() << "\n";
#endif

};
