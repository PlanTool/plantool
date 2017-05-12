/* 
* $Revision: 3.1 $
* $Date: 1999/01/06 10:15:43 $

actions.cc - The file for action data structure implementations. 
*/

#include "facts.h"
#include "actions.h"
#include "globals.h"
#include "stan.h"

/* This variable is used in constructing the or-ed vector of the mutual
exclusions of precondition facts for an action before it is known to be
applicable. It gets transferred into the appropriate layer checkvec if
the action is applicable. Subsequent layer checkvecs are generated using
the setvec function for op_level_info. */

action_list_node * freelist = 0;
action_list_node * freep = 0;
action_list_node * freelim = 0;

int dummycell;
int chkvec[EXVECSIZE];

op_level_info * level[MAX_PLAN];

#ifdef SYMMETRY

int numSymGps[MAX_PLAN];
char ** triedGps[MAX_PLAN];
char * foundSymGps[MAX_ACTIONS];
action * foundSymGpsRep[MAX_ACTIONS];
int totalSymGps = 0;
int matchedSymGps = 0;
#endif

/* RS: Added */
/* ALTALT */
/* Determining if current fact is in the list of propositions */
int in_list(token_list props,fact * elem){
	token_list iter;
	for(iter=props;iter>0;iter=iter->next)
		if(iter->info->getnum()==elem->getnum())
		     return TRUE;
   return FALSE;
}
/* END */

void setvec()
{
	for(int i = 0;i<EXVECSIZE;i++)
		chkvec[i] = 0;
};

void create_action_layer(int ln,int apl,int atl)
{
	level[ln] = new op_level_info[atl];

	int i;
	for(i = 0;i<apl;i++)
	{
		level[ln][i].start_layer(level[ln-1][i]);
		level[ln][i].setvec(action_table[i]->get_precs(),ln);
		action_table[i]->move_tmp_excludeds(ln-1);
	};

	for(;i<atl;i++)
	{
		level[ln][i].set_up();
		level[ln][i].setvec(action_table[i]->get_precs(),ln);
	};
};

/* Noop constructor. */

action::action(int al,int ln,fact * f)
{
	name = f->get_name();
	noop=TRUE;
	preconditions = new token_list_node(f,0);
	add_list = preconditions;
	del_list = 0;

	act_entry = al;

	/* ALTALT */
	// Setting distance-based value for action */
	levelact=ln;

	exvec_pos = al / 32;
	exvec_mask = 1 << (al % 32);

	for(int i=0;i<EXVECSIZE;i++) 
	{
		precvec[i] = 0;
		addvec[i] = 0;
		delvec[i] = 0;
	};

	precvec[f->expos()] = f->exmask();
	addvec[f->expos()] = f->exmask();

	f->set_noop(al,this);
	f->add_consumer(this);

//	level[ln].set_up();
//	level[ln].setvec(preconditions,ln);

	act_entry = al;

	no_acts++;
#ifdef PDDLOUTPUT	
	if(no_acts>MAX_ACTIONS) memerr();
#endif

	exvecsizea = (1+((no_acts-1)/32));

#ifdef SYMMETRY

	numSymParams = 0;

	symLabel = 0;
#endif

};


/* Actually applying an action - that is, adding it to a layer. */

void
action::enact(int ln)
{
	token_list ae,pc,prec;
	
	/*RS: Added by RS*/
	int sumv;
	int maxv;
        /*RS: END */  
       	int otherachieved=TRUE;

#ifdef DEBUG
	cout << name << " being applied at level " << ln << "\n";
#endif

	//ADDED 
	levelact=ln;


	act_entry = acts_at[ln];
	action_table[acts_at[ln]++] = this;

	exvec_pos = act_entry / 32;
	exvec_mask = 1 << (act_entry % 32);


//	level[ln].set_up();
//	level[ln].copyvec();

	no_acts++;
#ifdef PDDLOUTPUT	
	if(no_acts>MAX_ACTIONS) memerr();
#endif

	exvecsizea = (1+((no_acts-1)/32));
	
	/*RS: Added by RS*/
	/* ALTALT */
	/*Calculating distance-based values given preconditions of action*/ 
	sumv=0;
        maxv=0;
        for(prec=preconditions;prec>0;prec=prec->next)
	{
	    sumv=sumv+prec->info->getSumH();
            maxv=max(maxv,prec->info->getMaxV());
	}
        sumv+=1;
	/*RS: End*/
	
	for(ae = add_list;ae>0;ae=ae->next)
	{
		/*RS: Added by RS*/
		otherachieved=TRUE;
		/*RS: End */
		
		if(!ae->info->is_achieved(ln))
		{	fact_table[facts_at[ln]] = ae->info;
			ae->info->initialise(ln,facts_at[ln]++);
                        
                    	/*RS: Added by RS*/ 
			/* ALTALT */
			/*The value of the new proposition is the SUM of the values of its preconditions.*/
			/* Setting distance-based values for propositions */
			ae->info->setSumH(sumv);  
                        /*The value of the new proposition is the MAX of its preconditions...*/
			/* Setting distance-based values for propositions */
			ae->info->setMaxV(ln);
                        otherachieved=FALSE;
			/*RS: End */
			
			no_facts++;
			
			
#ifdef PDDLOUTPUT	
			if(no_facts>MAX_FACTS) memerr();
#endif
			exvecsizef = (1+(no_facts-1)/32);
		};
		    /*RS: Added by RS */
		    /* ALTALT */
		    /* If other action gave the proposition just update its value */
		     if(otherachieved) 
                     {
		         int minsum=min(sumv,ae->info->getSumH());
                         ae->info->setSumH(minsum);
			 int maxcurrent=max(maxv,ae->info->getMaxV());
			 ae->info->setMaxV(maxcurrent);
		     }
                         
		    /*RS: End */
		    
			ae->info->add_achiever(ln,this);
			addvec[ae->info->expos()] |= ae->info->exmask();
	};

	for(pc = preconditions;pc>0;pc=pc->next)
	{
		pc->info->add_consumer(this);
	};

	for(ae = del_list;ae>0;ae=ae->next)
		if(ae->info->is_achieved(ln))
			delvec[ae->info->expos()] |= ae->info->exmask();


#ifdef SYMMETRY
	if(numSymParams>0)
	{
		int i;
		for(i = 0;i<totalSymGps;i++)
		{
			if(!strcmp(symGp,foundSymGps[i]))
			{
				if(i >= matchedSymGps)
				{
					char * tmp = foundSymGps[i];
					foundSymGps[i] = foundSymGps[matchedSymGps];
					foundSymGps[matchedSymGps] = tmp;
					symLabel = ++matchedSymGps;
					int running_product = 1;
					for(int k = 0;k<numSymParams;k++)
						running_product *= symDomobs[symParams[k]]->getGroupsize()+1;
					symSizes[symLabel] = running_product;

					foundSymGpsRep[i]->symLabel = matchedSymGps;
					foundSymGpsRep[i] = foundSymGpsRep[matchedSymGps-1];
				}
				else
				{
					symLabel = i+1;
	
				};
				break;
			};
		};
		if(i == totalSymGps)
		{
			foundSymGps[totalSymGps] = symGp;
			foundSymGpsRep[totalSymGps++] = this;
		};

#ifdef SYMMETRYOUT
		if(symLabel) 
			cout << name << " is in symmetry group " << symLabel << "\n";
#endif
	};
#endif
};


/* Testing whether an action can be applied at a layer. */

int
action::applicable(int ln)
{
	token_list pc;

#ifdef DEBUG
	cout << "Checking " << name << "\n";
#endif

	for(pc = preconditions;pc>0;pc=pc->next)
		if(!pc->info->is_achieved(ln-1)) break;

	if(pc) { 
#ifdef DEBUG
			cout << "False precondition: "<<pc->info->get_name()<<"\n";
#endif 
			return FALSE;};


	for(int i = 0;i<exvecsizef;i++)
		chkvec[i] = 0;

	for(pc = preconditions;pc>0;pc=pc->next)
	{	precvec[pc->info->expos()] |= pc->info->exmask();

		for(int i = 0;i<exvecsizef;i++)
			chkvec[i] |= pc->info->get_vec(ln-1)[i];
	};

	for(int i = 0;i<exvecsizef;i++)
		if(chkvec[i] & precvec[i]) return FALSE;

	return TRUE;

};

/*RS: Added by RS */
/* ALTALT */
/* Checking if an action is applicable for regressing a state */
/* An action is applicable if and only if: Adds(A) intersection current state<>0 AND DELS(A) intersection current state=0 */
int
action::applicable_regression(token_list state)
{
	token_list adds,dels;
        int relevant=FALSE;
        //Relevance...    
        //cout<<"Action considered: "<<name<<"\n";
	for(adds=add_list;adds>0;adds=adds->next)
	  {
	    //cout<<"Fact add list:"<<adds->info->get_name()<<" value "<<adds->info->getnum()<<"\n";
	    if(in_list(state,adds->info)) {relevant=TRUE; break;};
	  }	

	if(!relevant) return FALSE;

        //Consistency...
	for(dels=del_list;dels>0;dels=dels->next)
		if(in_list(state,dels->info)) return FALSE;
	
	return TRUE;	
};
/* RS: End */

void
action::try_noop(int ln,fact * goal)
{
#ifdef BADSTATS
	calls_at[ln]++;
#endif
	if(!level[ln][act_entry].is_used())
	{
		level[ln][act_entry].mark_all_exclusives(act_entry,goal);

		goal->mark_goal(ln-1,goal);

		goal->mark_achieved(ln);

	};

	level[ln][act_entry].mark_used();
};



fact * 
action::tryop(int ln,token_list goals, fact * the_goal)
{
	/* This function adds the action to the current level of the plan */

	/* To do: 	mark excluded actions;
		  	add action to committed_acts_at list;
		  	mark achieved effects;
			mark preconditions;
			add previously unmarked preconditions to
				goals_at list;
	*/
#ifdef BADSTATS
	calls_at[ln]++;
#endif
#ifdef CHECKSTATS
	checks[ln][act_entry]++;
#endif

#ifdef NOREUSE
	if(!noop)
	{
		for(int i = 0;i<exvecsizef;i++)
			if(precvec[i] & no_reuse_vec[ln][i]) 
			{	
#ifdef BADSTATS
				fail2s_at[ln]++;
#endif

#ifdef EXCLUSIONS
				cout << name << " excluded at " << ln << "\n";
				for(int j = 0;j<facts_at[ln-1];j++)
					if(no_reuse_vec[ln][fact_table[j]->expos()] & fact_table[j]->exmask() & precvec[i]) cout << "\t" << fact_table[j]->get_name() << "\n";
#endif
				return FALSE;
			};
	};
#endif

#ifdef TRYDEBUG
	action_list excs;
#endif
	token_list fs;

	level[ln][act_entry].mark_all_exclusives(act_entry,the_goal);


	for(;goals>0;goals=goals->next)
		if(!goals->info->still_achievable(ln))
		{
#ifdef TRYDEBUG
			cout << "On action " << name << " at level " << ln << " " << goals->info->get_name() << " no longer achievable\n";

			cout << "Actions already used at this level:\n";
			for(action_list cas = committed_acts_at[ln];cas>0;cas=cas->next)
				cout << "\t" << cas->act->get_name() << "\n";

			cout << "Actions at previous level:\n";
			for(action_list cas = committed_acts_at[ln+1];cas>0;cas=cas->next)
				cout << "\t" << cas->act->get_name() << "\n";

			cout << "Excluded actions at this level:\n";

			for(int i = 0;i<acts_at[ln];i++)
				if(action_table[i]->already_excluded(ln))
					cout << "\t" << action_table[i]->get_name() << "\n";

			cout << "Actions excluded by THIS action:\n";
	for(excs = level[ln][act_entry].get_excludeds();excs>0;excs=excs->next)
		cout << "\t" << excs->act->get_name()<<"\n";
			cout << "\nAnd permanently:\n";
	for(excs = level[ln][act_entry].get_pexcludeds();excs>0;excs=excs->next)
		cout << "\t" << excs->act->get_name() << "\n";

			cout << "Achiever choices for shot goal:\n";
			for(action_list achs = goals->info->achievers(ln);achs>0;achs=achs->next)
				cout << "\t" << achs->act->get_name() << "\n";

#endif

			level[ln][act_entry].unmark_all_exclusives(act_entry+1);
#ifdef BADSTATS
			fail2s_at[ln]++;
#endif
			return goals->info;
		};


	for(fs = preconditions;fs>0;fs=fs->next)
	        fs->info->mark_goal(ln-1,the_goal);

	for(fs = add_list;fs>0;fs=fs->next)
		fs->info->mark_achieved(ln);

	committed_acts_at[ln] = new action_list_node(this,committed_acts_at[ln]);

#ifdef SYMMETRY
	for(int i = 0;i<numSymParams;i++)
		brokenSym[symParams[i]]++;
#endif


	return 0;

};


void 
action::untry_noop(int ln,fact * gl)
{
	level[ln][act_entry].unmark_used();

	if(!level[ln][act_entry].is_used())
	{
		gl->unmark_goal(ln-1);
		gl->unmark_achieved(ln);
		level[ln][act_entry].unmark_all_exclusives(act_entry+1);

	};
};

void 
action::untry(int ln,int fe)
{
	/* Reverse the sequence for trying the operator in the first place */

#ifdef SYMMETRY
	if(symLabel)
	{	int isbroken = 1;
		int running_total = 0;

		for(int i = 0;i<numSymParams;i++)
		{	brokenSym[symParams[i]]--;
			isbroken = isbroken && brokenSym[symParams[i]];
			running_total += brokenSym[symParams[i]]?indexes[i]:0;
		};

		if(!isbroken) 
		{//cout << name << " untried at " << ln << ": " << running_total << "\n";
			triedGps[ln][symLabel][running_total] = '1';

		};
	
	};
#endif


	action_list excs;
	token_list fs;

	excs = committed_acts_at[ln];
	committed_acts_at[ln] = committed_acts_at[ln]->next;
	delete excs;

	for(fs = preconditions;fs>0;fs=fs->next)
		fs->info->unmark_goal(ln-1);

	for(fs = add_list;fs>0;fs=fs->next)
		fs->info->unmark_achieved(ln);

	level[ln][act_entry].unmark_all_exclusives(act_entry+1);


};

/* The primary constructor. */

action::action(opptr op)
{
	for(int i=0;i<EXVECSIZE;i++)
	{
		precvec[i] = 0;
		addvec[i] = 0;
		delvec[i] = 0;
	};

#ifdef PDDLOUTPUT
	name = catstrsPDDL(op->thename);
#endif
#ifndef PDDLOUTPUT
	name = catstrs(op->thename);
#endif
	noop = FALSE;

#ifdef DEBUG
	cout << "Created " << name << "\n";
#endif

	plptr fs;

	preconditions = 0;
	add_list = 0;
	del_list = 0;

	for(fs=op->precs;fs>0;fs=fs->rest)
		preconditions = new token_list_node(all_facts.insert(catstrs(fs->prop)),preconditions);

	for(fs=op->adds;fs>0;fs=fs->rest)
	{
		add_list = new token_list_node(all_facts.insert(catstrs(fs->prop)),add_list);
		add_list->info->set_achievable();
		add_list->info->set_modifiable();
	};

	for(fs=op->dels;fs>0;fs=fs->rest)
	{	del_list = new token_list_node(all_facts.insert(catstrs(fs->prop)),del_list);
		del_list->info->add_deleter(this);
		del_list->info->set_modifiable();
	};

#ifdef SYMMETRY

	numSymParams = 0;
	int cnt = 0;

	for(tpptr vs = op->vars_shadow;vs;vs=vs->tps)
	{
#ifdef TECHSYMMETRYOUT
		cout << ":: " << vs->type->sindex << " ";
#endif
		if(vs->type->sindex) numSymParams++;
		cnt++;
	};
#ifdef TECHSYMMETRYOUT
		cout << "\n";
#endif
	if(numSymParams)
	{
		symParams = new int[numSymParams];
		symGp = new char[strlen(op->thename->nm->varname)+cnt+1];
		strcpy(symGp,op->thename->nm->varname);

		cnt = strlen(op->thename->nm->varname);
		indexes = new int[numSymParams];
		numSymParams = 0;
		int running_product = 1;
//cout << name << " ";
		for(tpptr vs = op->vars_shadow;vs;vs=vs->tps)
		{
			if(vs->type->sindex) 
			{	symParams[numSymParams] = vs->type->sindex;
				indexes[numSymParams] = running_product * symDomobs[vs->type->sindex]->getSymposn();
//cout << indexes[numSymParams] << " ";
				running_product = running_product * (symDomobs[vs->type->sindex]->getGroupsize()+1);
				numSymParams++;
			};
			symGp[cnt++] = (char) vs->type->sgnum;
		};
// cout << "\n";
		symGp[cnt] = '\0';

#ifdef TECHSYMMETRYOUT
		cout << name << " has symGp " << symGp << "\n";
#endif
	};

	symLabel = 0;
#endif
		
};

/* Used for filtering actions and static preconditions. Note that there is
a memory leak here - unused preconditions should be deleted to reclaim memory,
rather than just linked out of the list. */

int
action::all_precs_achievable()
{
	token_list qs = 0;

	for(token_list ps = preconditions;ps>0;ps=ps->next)
	{
		if(!ps->info->is_achievable()) return FALSE;
		if(!ps->info->is_modifiable())
		{
#ifdef DEBUG
			cout << "Removing precondition " << ps->info->get_name() << " from " << name << "\n";
#endif
			if(qs)
			{
				qs->next = ps->next;
			}
			else
			{
				preconditions = ps->next;
			};
		}
		else
		{
			qs = ps;
		};
	};

	return TRUE;
};


/* These functions are all the layer-dependent functions for actions, working
on op_level_info structures. */

void
op_level_info::copy(op_level_info & pl)
{
	used = 0;
	excluded = 0;

	nexcs = pl.nexcs;
	npexcs = 0;
	ninvs = 0;

	exvec = pl.exvec;
	excludeds = pl.excludeds;
	permexcludeds = pl.permexcludeds;
	inversions = pl.inversions;

};

void
op_level_info::set_excs(int ln)
{

	nexcs += npexcs;

	if(ln>0) nexcs += ninvs;

	excs = new pint[nexcs];

	int i = 0;
	for(action_list e = excludeds;e>0;e = e->next)
		excs[i++] = e->act->get_excluded_marker(ln);
	for(action_list e = permexcludeds;e>0;e = e->next)
		excs[i++] = e->act->get_excluded_marker(ln);

	if(ln>0)
		for(action_list e = inversions;e;e=e->next)
			excs[i++] = e->act->get_excluded_marker(ln-1);



};

void 
op_level_info::start_layer(op_level_info & pl)
{
	used = 0;
	excluded = 0;
	excludeds = 0;
	nexcs = 0;

	npexcs = pl.npexcs;
	ninvs = pl.ninvs;

	permexcludeds = pl.permexcludeds;
	for(int i = 0;i<EXVECSIZE;i++)
		exvec[i] = pl.exvec[i];

	inversions = pl.inversions;
};

void
op_level_info::set_up()
{
	for(int i = 0;i<EXVECSIZE;i++)
		exvec[i] = 0;

	used = 0;
	excluded = 0;

	excludeds = 0;
	permexcludeds = 0;
	inversions = 0;

	nexcs = 0;
	npexcs = 0;
	ninvs = 0;
};

void
op_level_info::setvec(token_list pcs,int ln)
{
	int i = 0;
	for(;pcs > 0;pcs = pcs->next)
		for(i = 0;i<exvecsizef;i++)
			checkVec[i] |= pcs->info->get_vec(ln-1)[i];
	for(;i<EXVECSIZE;i++)
		checkVec[i] = 0;
};





