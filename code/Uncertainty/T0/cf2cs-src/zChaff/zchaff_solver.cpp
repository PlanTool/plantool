/*********************************************************************
 Copyright 2000-2004, Princeton University.  All rights reserved. 
 By using this software the USER indicates that he or she has read, 
 understood and will comply with the following:

 --- Princeton University hereby grants USER nonexclusive permission 
 to use, copy and/or modify this software for internal, noncommercial,
 research purposes only. Any distribution, including commercial sale 
 or license, of this software, copies of the software, its associated 
 documentation and/or modifications of either is strictly prohibited 
 without the prior consent of Princeton University.  Title to copyright
 to this software and its associated documentation shall at all times 
 remain with Princeton University.  Appropriate copyright notice shall 
 be placed on all software copies, and a complete copy of this notice 
 shall be included in all copies of the associated documentation.  
 No right is  granted to use in advertising, publicity or otherwise 
 any trademark, service mark, or the name of Princeton University. 


 --- This software and any associated documentation is provided "as is" 

 PRINCETON UNIVERSITY MAKES NO REPRESENTATIONS OR WARRANTIES, EXPRESS 
 OR IMPLIED, INCLUDING THOSE OF MERCHANTABILITY OR FITNESS FOR A 
 PARTICULAR PURPOSE, OR THAT  USE OF THE SOFTWARE, MODIFICATIONS, OR 
 ASSOCIATED DOCUMENTATION WILL NOT INFRINGE ANY PATENTS, COPYRIGHTS, 
 TRADEMARKS OR OTHER INTELLECTUAL PROPERTY RIGHTS OF A THIRD PARTY.  

 Princeton University shall not be liable under any circumstances for 
 any direct, indirect, special, incidental, or consequential damages 
 with respect to any claim by USER or any third party on account of 
 or arising from the use, or inability to use, this software or its 
 associated documentation, even if Princeton University has been advised
 of the possibility of those damages.
*********************************************************************/

#include <cstdio>
#include <cmath>
#include <algorithm>
#include <fstream>
#include "zchaff_solver.h"
//typedef double DOUBLE;
//  extern "C" DOUBLE PLED_FindMaxFlow(int nvtxs, int nedges,
//  				   int *edge_from, int *edge_to, 
//  				   DOUBLE *ewts, int *part);

//#define VERIFY_ON

#ifdef VERIFY_ON
ofstream verify_out("resolve_trace");
#endif

void CSolver::re_init_stats(void)
{
    _stats.is_mem_out		= false;
    _stats.outcome		= UNDETERMINED;

    _stats.next_restart 	= _params.restart.first_restart;
    _stats.restart_incr 	= _params.restart.backtrack_incr;
    _stats.next_cls_deletion 	= _params.cls_deletion.interval;
    _stats.next_var_score_decay = _params.decision.decay_period;

    _stats.current_randomness 	= _params.decision.base_randomness;

    _stats.total_bubble_move 	= 0;
    _stats.num_decisions 	= 0;
    _stats.num_decisions_stack_conf	= 0;
    _stats.num_decisions_vsids	= 0;
    _stats.num_decisions_shrinking=0;
    _stats.restarts_since_vsids	= 0;
    _stats.num_backtracks	= 0;
    _stats.max_dlevel 			= 0;
    _stats.num_implications   		= 0;
    _stats.num_restarts			= 0;
    _stats.num_del_orig_cls		= 0;
    _stats.num_shrinkings               = 0;
    _stats.start_cpu_time	= get_cpu_time();
    _stats.finish_cpu_time	= 0;
    _stats.random_seed		= 0;
    _stats.sum_sizes_iter = 0;
    _stats.sum_squares_iter =0;
    _stats.num_conf_cls_iter =0;
}

void CSolver::init_stats(void) 
{
    re_init_stats();

    _stats.been_reset		= true;
    _stats.outcome 		= UNDETERMINED;

    _stats.num_free_variables 	= 0;
    _stats.num_free_branch_vars = 0;
}

void CSolver::init_parameters(void)
{
    _params.verbosity				= 0;
    _params.time_limit				= 3600 * 48;
    _params.shrinking.size                      = 95;
    _params.shrinking.enable                    = true;

    _params.decision.strategy 			= 0;
    _params.decision.restart_randomness		= 0;	
    _params.decision.base_randomness		= 0;
    _params.decision.decay_period		= 40; 
    _params.decision.bubble_init_step 		= 0x400;

    _params.cls_deletion.enable 		= true ;
    _params.cls_deletion.head_activity          = 500;//250;//60
    _params.cls_deletion.tail_activity          = 10;//35;//7
    _params.cls_deletion.head_num_lits          = 6;//6;//8
    _params.cls_deletion.tail_num_lits          = 45;//45;//42
    _params.cls_deletion.tail_vs_head           = 16;
    _params.cls_deletion.interval	        = 600;
    

    _params.restart.enable			= true;
    _params.restart.interval			= 700;
    _params.restart.first_restart		= 10*_params.restart.interval;
    _params.restart.backtrack_incr		= _params.restart.interval;
    _params.restart.backtrack_incr_incr 	= 0;
}

CSolver::CSolver(void) {
    init_parameters();
    init_stats();
    _dlevel			= 0;		
    _force_terminate		= false;
    _implication_id		= 0;
    _num_marked			= 0;
    _num_in_new_cl		= 0;
    _outside_constraint_hook	= NULL;
    _sat_hook	                = NULL;
}

CSolver::~CSolver()
{
    while (!_assignment_stack.empty()) {
	delete _assignment_stack.back();
	_assignment_stack.pop_back();
    }
}

void CSolver::set_time_limit(float t) 
{ 
    _params.time_limit = t; 
}

float CSolver::elapsed_cpu_time(void) 
{
    return get_cpu_time() - _stats.start_cpu_time;
}

float CSolver::cpu_run_time() 
{ 
    return (_stats.finish_cpu_time - _stats.start_cpu_time);
}

void CSolver::set_variable_number(int n) 
{
    assert (num_variables() == 0);
    CDatabase::set_variable_number(n);
    _stats.num_free_variables	= num_variables();
    while (_assignment_stack.size() <= num_variables())
	_assignment_stack.push_back(new vector<int>);
    assert (_assignment_stack.size() == num_variables() + 1);
}

int CSolver::add_variable(void) 
{
    int num = CDatabase::add_variable();
    ++_stats.num_free_variables;
    while (_assignment_stack.size() <= num_variables())
	_assignment_stack.push_back(new vector<int>);
    assert (_assignment_stack.size() == num_variables() + 1);
    return num;
}

void CSolver::set_mem_limit(int s)
{ 
    CDatabase::set_mem_limit(s); 
}

void CSolver::set_decision_strategy(int s) 
{
    _params.decision.strategy = s; 
}

void CSolver::set_randomness(int n) 
{
    _params.decision.base_randomness = n; 
}

void CSolver::set_random_seed(int seed) {
    srand (seed);
}

void CSolver::enable_cls_deletion(bool allow) {
    _params.cls_deletion.enable=allow;
}

void CSolver::add_hook( HookFunPtrT fun, int interval) 
{
    pair<HookFunPtrT, int> a(fun, interval);
    _hooks.push_back(pair<int, pair<HookFunPtrT, int> > (0, a));
}

void CSolver::run_periodic_functions(void)
{
    //a. restart
    if (_params.restart.enable && _stats.num_backtracks > _stats.next_restart && _shrinking_cls.empty()) {
        _stats.next_restart = _stats.num_backtracks + _stats.restart_incr;
	delete_unrelevant_clauses();
        if(_stats.num_restarts%5==1)     
            compact_lit_pool();
	restart(); 
        cout<<"\rDecision: "<<_assignment_stack[0]->size()<<"/"<<num_variables();
        cout<<"\tTime: "<<get_cpu_time()-_stats.start_cpu_time<<"/"<<_params.time_limit<<flush;
    }
    //b. decay variable score	         
    if (_stats.num_backtracks > _stats.next_var_score_decay) {
	_stats.next_var_score_decay = _stats.num_backtracks + _params.decision.decay_period;
	decay_variable_score();
    }
    //d. run hook functions
    for (unsigned i=0; i< _hooks.size(); ++i) {
	pair<int,pair<HookFunPtrT, int> > & hook = _hooks[i];
	if (_stats.num_decisions >= hook.first) {
	    hook.first += hook.second.second;
	    hook.second.first((void *) this);
	}
    }
} 
    

void CSolver::init_solve(void)
{
    CDatabase::init_stats();
    re_init_stats();
    _stats.been_reset 		= false;

    assert (_conflicts.empty());
    assert (_conflict_lits.empty());
    assert (_num_marked == 0);
    assert (_num_in_new_cl == 0);
    assert (_dlevel == 0);
	
    for (unsigned i=0, sz = variables().size(); i< sz; ++i) {
	variable(i).score(0) = variable(i).lits_count(0);
	variable(i).score(1) = variable(i).lits_count(1);
    }

    _ordered_vars.resize( num_variables());
    update_var_score();
    int seed = 0;//time(NULL);
    //seed=1066564017;
    if(_stats.random_seed==0)
	_stats.random_seed=seed;
    else
	seed=_stats.random_seed;
    srand(seed);
    top_unsat_cls=clauses().end();
    top_unsat_cls--;
    _shrinking_benefit=0;
    _shrinking_cls.clear();
    _shrinking_conf_cls_length=0;
    DBG2(dump());
}

void CSolver::set_var_value(int v, int value, ClauseIdx ante, int dl)
{
    assert (value == 0 || value == 1);
    DBG2(dump());
    CHECK(verify_integrity());
    DBG1 (cout << "Setting\t" << (value>0?"+":"-") << v 
	  << " at " << dlevel() << " because " << ante<< endl;);
    CVariable & var = variable(v);
    assert (var.value() == UNKNOWN);
    var.set_dlevel(dl);
    var.set_value(value);
    var.antecedent() = ante;
    var.assgn_stack_pos() = _assignment_stack[dl]->size();
    _assignment_stack[dl]->push_back(v*2+!value);

    if (dl == dlevel())
	set_var_value_current_dl(v, value);
    else 
	set_var_value_not_current_dl(v, value);
	
    ++_stats.num_implications ;
    if (var.is_branchable()) 
	--num_free_variables();
}

void CSolver::set_var_value_current_dl(int v, int value)
{
    vector<CLitPoolElement *> & watchs = variable(v).watched(value);
    for (vector <CLitPoolElement *>::iterator itr = watchs.begin(); itr != watchs.end(); ++itr) {
	ClauseIdx cl_idx;
	CLitPoolElement * other_watched = *itr;
	CLitPoolElement * watched = *itr;
	int dir = watched->direction();
	CLitPoolElement * ptr = watched;
	while(1) {
	    ptr += dir;
	    if (ptr->val() <= 0) { //reached one end of the clause
		if (dir == 1) 	//reached the right end, i.e. spacing element is the cl_id
		    cl_idx = ptr->get_clause_index();
		if (dir == watched->direction()) { //we haven't go both directions.
		    ptr = watched;	
		    dir = -dir;			//change direction, go the other way
		    continue;
		}
                //otherwise, we have already go through the whole clause
		int the_value = literal_value (*other_watched);
		if (the_value == 0) //a conflict
		    _conflicts.push_back(cl_idx);
		else if ( the_value != 1) //i.e. unknown
		    queue_implication (other_watched->s_var(), cl_idx, dlevel());
		break;
	    }
	    if (ptr->is_watched()) {	//literal is the other watched lit, skip it.
		other_watched = ptr;
		continue;
	    }
	    if (literal_value(*ptr) == 0) // literal value is 0, keep going
		continue;
	    //now the literal's value is either 1 or unknown, watch it instead
	    int v1 = ptr->var_index();
	    int sign = ptr->var_sign();
	    variable(v1).watched(sign).push_back(ptr);
	    ptr->set_watch(dir);
	    //remove the original watched literal from watched list
	    watched->unwatch();
	    *itr = watchs.back(); //copy the last element in it's place
	    watchs.pop_back();	//remove the last element
	    --itr;		//do this so with don't skip one during traversal
	    break;
	}
    }
}

void CSolver::set_var_value_not_current_dl(int v, int value)
{
//the only difference between this one and the last function
//is that because the assignment var is not at current_dl, 
//(that means, < current_dl), it's possible some variable 
//have a dlevel higher than the assigned one. So, we need 
//to make sure that watched literal has the highest dlevel if
//we can't find other unassigned or value 1 literals
//also, it's possible we need to readjust decision levels of 
//some variables implied.
    ClauseIdx cl_idx;
    CLitPoolElement * watched, * other_watched, * ptr, * max_ptr = NULL;
    int dir,max_dl;
    vector<CLitPoolElement *> & watchs = variable(v).watched(value);
    for (vector <CLitPoolElement *>::iterator itr = watchs.begin(); 
	 itr != watchs.end(); ++itr) {
	max_dl = -1;
	watched = *itr;
	dir = watched->direction();
	ptr = watched;
	while(1) {
	    ptr += dir;
	    if (ptr->val() <= 0) {
		if (dir == 1) 
		    cl_idx = ptr->get_clause_index();
		if (dir == watched->direction()) {
		    ptr = watched;
		    dir = -dir;
		    continue;
		}
		CLitPoolElement * current_watched = watched;
		if (variable(watched->var_index()).dlevel() < max_dl) {
		    int v1 = max_ptr->var_index();
		    int sign = max_ptr->var_sign();
		    variable(v1).watched(sign).push_back(max_ptr);
		    max_ptr->set_watch(dir);
		    watched->unwatch();
		    *itr = watchs.back();
		    watchs.pop_back();
		    --itr;
		    current_watched = max_ptr;
		}
		int max = variable(current_watched->var_index()).dlevel();
		int the_value = literal_value (*other_watched);
		if (the_value == 0) {
		    //it's a conflict, but we will not put into _conflicts
		    //instead, make it a implication so it will be resolved 
		    //in deduce()
		    assert (variable(other_watched->var_index()).dlevel() >= max);
//  		    cout << "Queueing Conflict for " << other_watched->var_index() << " orig DL: " 
//  			 << variable(other_watched->var_index()).dlevel() << " current DL: " 
//  			 << max << endl;
		    queue_implication(other_watched->s_var(), cl_idx, max);
		}
		else if ( the_value == 1) {
		    int v1 = other_watched->var_index();
		    if (variable(v1).dlevel() > max)
			queue_implication(other_watched->s_var(), cl_idx, max);
		}
		else 
		    queue_implication (other_watched->s_var(), cl_idx, max);
		break;
	    }
	    if (ptr->is_watched()) {
		other_watched = ptr;
		continue;
	    }
	    if (literal_value(*ptr) == 0) {
				//keep track of the 0 literal with max decision level
		int ptr_dl = variable(ptr->var_index()).dlevel();
		if ( ptr_dl > max_dl) {
		    max_dl = ptr_dl;
		    max_ptr = ptr;
		}
		continue;
	    }
	    //now it's value is either 1 or unknown
	    int v1 = ptr->var_index();
	    int sign = ptr->var_sign();
	    variable(v1).watched(sign).push_back(ptr);
	    watched->unwatch();
	    ptr->set_watch(dir);

	    *itr = watchs.back();
	    watchs.pop_back();
	    --itr;
	    break;
	}
    }
}

void CSolver::unset_var_value(int v)
{
    if (v == 0) return;
    DBG1(cout <<"Unset var " << (variable(v).value()?"+":"-") << v << endl;);
    CVariable & var = variable(v);
    var.set_value(UNKNOWN);
    var.set_antecedent(NULL_CLAUSE);
    var.set_dlevel( -1);
    var.assgn_stack_pos() = -1;

    if (var.is_branchable()) {
	++num_free_variables();
	if (var.var_score_pos() < _max_score_pos)
	    _max_score_pos = var.var_score_pos();
    }
}

int CSolver::find_max_clause_dlevel(ClauseIdx cl)
{
//if cl has single literal, then the dlevel for it should be 0
//thus, we need to set initial max_level = 0 instead
//of -1 because if clause has single literal, then the
//loop will found no literal with value UNKNOWN
    int max_level = 0;
    if (cl == NULL_CLAUSE) 
	return dlevel();
    for (unsigned i=0, sz= clause(cl).num_lits(); i<sz;  ++i) {
	int var_idx =((clause(cl).literals())[i]).var_index();
	if (variable(var_idx).value() != UNKNOWN) {
	    if (max_level < variable(var_idx).dlevel())
		max_level = variable(var_idx).dlevel();
	}
    }
    return max_level; 
}

void CSolver::dump_assignment_stack(ostream & os ) {
    os << "Assignment Stack:  ";
    for (int i=0; i<= dlevel(); ++i) {
	os << "(" <<i << ":";
	for (unsigned j=0; j<(*_assignment_stack[i]).size(); ++j )
	    os << ((*_assignment_stack[i])[j]&0x1?"-":"+")
		 << ((*_assignment_stack[i])[j] >> 1) << " ";
	os << ") " << endl;
    }
    os << endl;
}

void CSolver::dump_implication_queue(ostream & os) 
{
    _implication_queue.dump(os);
}

void CSolver::delete_clause_group (int gid)
{
    assert( is_gid_allocated(gid) );
    if (_stats.been_reset==false)
	reset(); //if delete some clause, then implication queue are invalidated
    for (vector<CClause>::iterator itr1 = clauses().begin(); 
	 itr1 != clauses().end(); ++itr1) {
	CClause & cl = * itr1;
	if (cl.status() != DELETED_CL) {
	    if (cl.gid(gid) == true) {
		mark_clause_deleted(cl);
	    }
	}
    }
    //delete the index from variables
    for (vector<CVariable>::iterator itr = variables().begin(); 
	 itr != variables().end(); ++ itr) {
	for (unsigned i=0; i<2; ++i) { //for each phase
	    //delete the lit index from the vars
#ifdef KEEP_LIT_CLAUSES
	    vector<ClauseIdx> & lit_clauses = (*itr).lit_clause(i);
	    for (vector<ClauseIdx>::iterator itr1 = lit_clauses.begin();
		 itr1 != lit_clauses.end(); ++ itr1) 
		if ( clause(*itr1).status() == DELETED_CL ) {
		    *itr1 = lit_clauses.back();
		    lit_clauses.pop_back();
		    -- itr1;
		}
#endif
	    //delete the watched index from the vars
	    vector<CLitPoolElement *> & watched = (*itr).watched(i);
	    for (vector<CLitPoolElement *>::iterator itr1 = watched.begin(); 
		 itr1 != watched.end(); ++itr1) 
		if ( (*itr1)->val() <= 0) {
		    *itr1 = watched.back();
		    watched.pop_back();
		    --itr1;
		}
	}
    }
    free_gid(gid);
}

void CSolver::reset(void)
{
    if (_stats.been_reset)
    	return;
    if (num_variables()==0) return;
    back_track(0);
    _conflicts.clear();
    while (!_implication_queue.empty())
	_implication_queue.pop();

    _stats.is_solver_started 	= false;
    _stats.outcome		= UNDETERMINED;
    _stats.been_reset 		= true;
}

void CSolver::delete_unrelevant_clauses(void)
{
    /*
    if (CDatabase::_stats.mem_used_up) {
	CDatabase::_stats.mem_used_up = false;
	if (++CDatabase::_stats.mem_used_up_counts < 5) {
	    DBG1(
		cout << "Forced to be more aggressive in clause deletion. " << endl;
		cout <<"MaxUnrel: " << _params.cls_deletion.max_unrelevance 
		<< "  MinLenDel: " << _params.cls_deletion.min_num_lits
		<< "  MaxLenCL : " << _params.cls_deletion.max_conf_cls_size << endl;
		);
	}
    }*/

    unsigned original_del_cls = num_deleted_clauses();
    int num_conf_cls=num_clauses()-init_num_clauses()+num_del_orig_cls();
    int head_count=num_conf_cls/_params.cls_deletion.tail_vs_head;
    int count=0;
    DBG2 (dump());
    for (vector<CClause>::iterator itr1=clauses().begin(); itr1 != clauses().end()-1; ++itr1) {
	CClause & cl = * itr1;
        bool cls_sat_at_dl_0=false;	
	if(cl.status()!=DELETED_CL){ 
	    for (int i=0, sz=cl.num_lits(); i<sz; ++i){
	        if (literal_value(cl.literal(i)) == 1 && variable(cl.literal(i).var_index()).dlevel()==0) {
        	    cls_sat_at_dl_0=true;
		    break;
	        }
	    }
	    if(cls_sat_at_dl_0 ){
		if(cl.status()==ORIGINAL_CL || cl.status()==CONFLICT_CL){
		    // deleting ORIGINAL clauses can change the initial starting value for VSIDS..
		    // deleting CONFLICT clauses will avoid future calls to is_satisfied on these clauses	
		    int val_0_lits = 0, val_1_lits = 0, unknown_lits = 0;
		    for (unsigned i=0; i< cl.num_lits(); ++i) {
		        int lit_value = literal_value (cl.literal(i));
		        if (lit_value == 0 )
		   	    ++val_0_lits;
			if (lit_value == 1) 
			    ++val_1_lits;
			if (lit_value == UNKNOWN) 
		            ++unknown_lits; 	
			if (unknown_lits+val_1_lits > 1 ) { 
			    mark_clause_deleted(cl);
			    break;
			}			
                    }
		    continue;	// if i didn't delete it now, i wont later either
		}
		
	    }
	}
        if (cl.status()!=CONFLICT_CL ) {
            continue;
        }
        
        count++;
	int max_activity=_params.cls_deletion.head_activity-(_params.cls_deletion.head_activity-_params.cls_deletion.tail_activity)*count/num_conf_cls;
	int max_conf_cls_size;//=_params.cls_deletion.head_num_lits+(_params.cls_deletion.tail_num_lits-_params.cls_deletion.head_num_lits)*count/num_conf_cls;
	
		
	if(head_count>0){
	    //max_activity=_params.cls_deletion.head_activity;
	    max_conf_cls_size=_params.cls_deletion.head_num_lits;
            head_count--;
	}
	else{
	    //max_activity=_params.cls_deletion.tail_activity;
	    max_conf_cls_size=_params.cls_deletion.tail_num_lits;
	}
        /*
        //WHY?
        if(cl.activity()<0)
            cl.activity()+=55;
        //if(cl.activity())
            cl.activity()--;
	*/
	if(cl.activity()>max_activity)	
	    continue;

      	int val_0_lits = 0, val_1_lits = 0, unknown_lits = 0,lit_value;
	for (unsigned i=0; i< cl.num_lits(); ++i) {
	    lit_value = literal_value (cl.literal(i));
	    if (lit_value == 0 ) 
		++val_0_lits;
	    else if (lit_value == 1) 
		++val_1_lits;
	    else
		++unknown_lits;
	    if ((unknown_lits>max_conf_cls_size)){
		if((unknown_lits+val_1_lits > 1 )) { 
		    mark_clause_deleted(cl);
		    DBG1 (cout << "Deleting Large clause: " << cl << endl;);
		}
	        else{
		// IF THIS ASSERTION FAILS, and we are just after restart, this tells us a var ought to be assigned
		    assert((dlevel()!=0) || ( unknown_lits!=0 && unknown_lits!=1));
		//    cout<<" SKIP - "<<unknown_lits<<' '<<val_1_lits<<endl;	
	        }
		break;
            }
        }
    }
    // if none were recently marked for deletion...
    if (original_del_cls == num_deleted_clauses()) 
        return;
    
    //delete the index from variables
    for (vector<CVariable>::iterator itr = variables().begin(); 
	 itr != variables().end(); ++ itr) {
	for (unsigned i=0; i<2; ++i) { //for each phase
	    //delete the lit index from the vars
#ifdef KEEP_LIT_CLAUSES
	    vector<ClauseIdx> & lit_clauses = (*itr).lit_clause(i);
	    for (vector<ClauseIdx>::iterator itr1 = lit_clauses.begin();
		 itr1 != lit_clauses.end(); ++ itr1) 
		if ( clause(*itr1).status() == DELETED_CL ) {
		    *itr1 = lit_clauses.back();
		    lit_clauses.pop_back();
		    -- itr1;
		}
#endif
	    //delete the watched index from the vars
	    vector<CLitPoolElement *> & watched = (*itr).watched(i);
	    for (vector<CLitPoolElement *>::iterator itr1 = watched.begin(); 
		 itr1 != watched.end(); ++itr1) {
		if ( (*itr1)->val() <= 0) {
		    *itr1 = watched.back();
		    watched.pop_back();
		    --itr1;
		}
	    }
	}
    }

    for (unsigned i=1; i<variables().size(); ++i) {
	if(variable(i).dlevel()!=0){
	    variable(i).score(0) = variable(i).lits_count(0);
	    variable(i).score(1) = variable(i).lits_count(1);

	    if(variable(i).lits_count(0)==0 && (variable(i).value()==UNKNOWN) ){
	        queue_implication(2*i+1,NULL_CLAUSE,0);
	    }
 	    else if(variable(i).lits_count(1)==0 && (variable(i).value()==UNKNOWN) ){
	        queue_implication(2*i,NULL_CLAUSE,0);
	    }
	}
	else{
	    variable(i).score(0)=variable(i).score(1)=0;	
	}
    }
    update_var_score();
    DBG1(cout << "Deleting " << num_deleted_clauses() - original_del_cls << " Clause ";
	 cout << " and " << num_deleted_literals() - original_del_lits << " Literals " << endl;);
    DBG2 (dump());
}
//============================================================================================
bool CSolver::time_out(void) 
{
    return (get_cpu_time() - _stats.start_cpu_time> _params.time_limit);
}

void CSolver::adjust_variable_order(int * lits, int n_lits) //note lits are signed vars, not CLitPoolElements
{
    for (int i=0; i<n_lits; ++i) {
	int var_idx = lits[i]>>1;
	int var_sign = lits[i]&0x1;
	CVariable & var = variable(var_idx);
	assert (var.value() != UNKNOWN);
	int orig_score = var.score();
	variable(var_idx).score(lits[i]&0x01)++;
	int new_score = var.score();
	if (orig_score == new_score) 
	    continue;
	int pos = var.var_score_pos();
	int orig_pos = pos;
	assert (_ordered_vars[pos].first == & var);
	assert (_ordered_vars[pos].second == orig_score);
	int bubble_step = _params.decision.bubble_init_step;
	for (pos = orig_pos ; pos >= 0; pos -= bubble_step)
	    if (_ordered_vars[pos].second >= new_score) break;
	pos += bubble_step;
	for ( bubble_step = bubble_step >> 1; bubble_step > 0; bubble_step = bubble_step >> 1) {
	    if ( pos - bubble_step >= 0) {
		if (_ordered_vars[pos - bubble_step].second < new_score)
		    pos -= bubble_step;
	    }
	}
	//now found the position, do a swap
	_ordered_vars[orig_pos] = _ordered_vars[pos];
	_ordered_vars[orig_pos].first->set_var_score_pos(orig_pos);

	_ordered_vars[pos].first = & var;
	_ordered_vars[pos].second = new_score;
	_ordered_vars[pos].first->set_var_score_pos(pos);

	_stats.total_bubble_move += orig_pos - pos;
    }
    CHECK(
	for (unsigned i=0; i< _ordered_vars.size(); ++i) {
	    assert (_ordered_vars[i].second == _ordered_vars[i].first->score());
	    assert (_ordered_vars[i].first->var_score_pos() == i);
	    assert (i==0 || _ordered_vars[i].second <= _ordered_vars[i-1].second );
	    assert (_ordered_vars[i].first->value() != UNKNOWN || _max_score_pos <= i);
	});

}

void CSolver::decay_variable_score(void) 
{
    unsigned int i, sz;
    for(i=1, sz = variables().size(); i<sz; ++i) {
	CVariable & var = variable(i);
	var.score(0) = var.score(0)/2;//4
	var.score(1) = var.score(1)/2;//4
    }
    for (i=0, sz = _ordered_vars.size(); i<sz; ++i) {
	_ordered_vars[i].second = _ordered_vars[i].first->score();
    }
}

bool CSolver::decide_next_branch(void)
{
    if(dlevel()>0)
        assert(_assignment_stack[dlevel()]->size()>0);
    //cout<<_assignment_stack[dlevel()]->size()<<endl;
    CHECK(verify_integrity());
    if (!_implication_queue.empty()) {
	//some hook function did a decision, so skip my own decision making.
	//if the front of implication queue is 0, that means it's finished
	//because var index start from 1, so 2 *vid + sign won't be 0. 
	//else it's a valid decision.
	return (_implication_queue.front().lit != 0);
    }
    int s_var=0;
    if(_params.shrinking.enable){
        while(!_shrinking_cls.empty()){
	    s_var=_shrinking_cls.begin()->second;
            _shrinking_cls.erase(_shrinking_cls.begin());
	    if(variable(s_var>>1).value()==UNKNOWN){
                _stats.num_decisions++;
                _stats.num_decisions_shrinking++;
	        ++dlevel();
	        queue_implication(s_var^0x01,NULL_CLAUSE,dlevel());
                return true;
	    }
        }
    }
//    add_outside_clauses();
    if (_outside_constraint_hook != NULL)
	_outside_constraint_hook(this);

    if (!_implication_queue.empty())
	return (_implication_queue.front().lit != 0);
    
    ++_stats.num_decisions;
   if (num_free_variables() == 0) //no more free vars
	return false;
     
    bool cls_sat;
    int i,sz,var_idx,score,max_score=-1;
    
    for (;top_unsat_cls->status()!=ORIGINAL_CL;top_unsat_cls--) {
        CClause &cl=*top_unsat_cls;
        if(cl.status()!=CONFLICT_CL)
	    continue;
        cls_sat=false;
	if(cl.sat_lit_idx()<cl.num_lits()&&literal_value(cl.literal(cl.sat_lit_idx()))==1)
            cls_sat=true;
	if(!cls_sat){	
	    for (i=0,sz=cl.num_lits(); i<sz; ++i){
	        if (literal_value(cl.literal(i)) == 1 ) {
        	    cls_sat=true;
		    cl.sat_lit_idx()=i;
		    break;
	    	}
	    }
	}
	if(!cls_sat){
            for (i=0, sz= cl.num_lits(); i<sz; ++i){
	        var_idx=cl.literal(i).var_index();
	        score=0;
	        if (variable(var_idx).value() == UNKNOWN){
                    score=variable(var_idx).score();
		    if(score>max_score){
		        max_score=score;
		        s_var=2*var_idx;
		    }    
		}
            }
	    break;
	}
    }
    if(max_score!=-1){    
        ++dlevel();
        if (dlevel() > _stats.max_dlevel) 
	    _stats.max_dlevel = dlevel();
	CVariable &v=variable(s_var>>1);		
        if(v.score(0)<v.score(1))
    	    s_var+=1;
        else if(v.score(0)==v.score(1)){
            if(v.two_lits_count(0)>v.two_lits_count(1))
                s_var+=1;
            else if(v.two_lits_count(0)==v.two_lits_count(1))
                s_var+=rand()%2;
        }
	assert(s_var >=2 );					
        queue_implication(s_var, NULL_CLAUSE, dlevel());
	++_stats.num_decisions_stack_conf;
        return true;
    }
    _stats.restarts_since_vsids=0;    
   DBG2(
	cout << "Ordered Vars ";
	for (unsigned i=0; i< _ordered_vars.size(); ++i)
	cout << (_ordered_vars[i].first - & variables()[0]) << " ";
	cout << endl;
	);

    CHECK(
	int free = 0;
	for (unsigned i=0; i< _ordered_vars.size(); ++i) {
	    assert (_ordered_vars[i].second == _ordered_vars[i].first->score());
	    assert (_ordered_vars[i].first->var_score_pos() == i);
	    assert (i==0 || _ordered_vars[i].second <= _ordered_vars[i-1].second );
	    assert (_ordered_vars[i].first->value() != UNKNOWN || _max_score_pos <= i);
	    if (_ordered_vars[i].first->value() == UNKNOWN) ++free;
	}
	assert (free == num_free_variables());
	);

    for (unsigned i=_max_score_pos; i<_ordered_vars.size(); ++i) {
	CVariable & var = *_ordered_vars[i].first;
	if (var.value()==UNKNOWN && var.is_branchable()) {
	    //move th max score position pointer
	    _max_score_pos = i;
	    //make some randomness happen
	    if (--_stats.current_randomness < _params.decision.base_randomness)
		_stats.current_randomness = _params.decision.base_randomness;

	    int randomness = _stats.current_randomness;
	    if (randomness >= num_free_variables())
		randomness = num_free_variables() - 1;
	    int skip =rand()%(1+randomness);
	    int index = i;
	    while (skip > 0) {
		++index;
		if (_ordered_vars[index].first->value()==UNKNOWN && 
		    _ordered_vars[index].first->is_branchable())
		    --skip;
	    }
	    CVariable * ptr = _ordered_vars[index].first;
	    assert (ptr->value() == UNKNOWN && ptr->is_branchable());
	    int sign=0;
//	    sign = ptr->score(0) > ptr->score(1) ? 0 : 1;
	    if(ptr->score(0) < ptr->score(1))
		sign += 1;
            else if(ptr->score(0) == ptr->score(1)){
                if(ptr->two_lits_count(0)>ptr->two_lits_count(1))
                   sign+=1;
                else if(ptr->two_lits_count(0)==ptr->two_lits_count(1))
                   sign+=rand()%2;
            }
//	    sign = (random() > (RAND_MAX >> 1))? 0: 1;
	    int var_idx = ptr - &(*variables().begin());
	    s_var = var_idx + var_idx + sign;
	    break;
	}
    }

    assert (s_var >= 2); //there must be a free var somewhere
    ++dlevel();
    if (dlevel() > _stats.max_dlevel) 
	_stats.max_dlevel = dlevel();
    ++_stats.num_decisions_vsids;
    DBG0 (cout << "**Decision " << _stats.num_decisions << " at Level " << dlevel() ;
	  cout <<": " << s_var << "\ti.e. " << (s_var&0x1?"-":" ") ;
	  cout <<(s_var>>1)  << endl; );
    _implication_id = 0;
    queue_implication(s_var, NULL_CLAUSE, dlevel());
    return true;
}

int CSolver::preprocess(void) 
{
    unsigned int i, sz;
    assert(dlevel() == 0);
    //1. detect all the unused variables
    vector<int> un_used;
    for (i=1, sz=variables().size(); i<sz; ++i) {
	CVariable & v = variable(i);
	if (v.lits_count(0) == 0 && v.lits_count(1) == 0) {
	    un_used.push_back(i);
	    queue_implication(i+i, NULL_CLAUSE, 0);
	    int r = deduce();
	    assert (r == NO_CONFLICT);
	}
    }
    if (_params.verbosity>1 && un_used.size() > 0) {
	cout << un_used.size()<< " Variables are defined but not used " << endl;
	if (_params.verbosity > 2) {
	    for (unsigned i=0; i< un_used.size(); ++i)
		cout << un_used[i] << " ";
	    cout << endl;
	}
    }

    //2. detect all variables with only one phase occuring (i.e. pure literals)
    vector<int> uni_phased;
    for (i=1, sz=variables().size(); i<sz; ++i) {
	CVariable & v = variable(i);
	if (v.value() != UNKNOWN)
	    continue;
	if (v.lits_count(0) == 0){ //no positive phased lits.
	    queue_implication( i+i+1, NULL_CLAUSE, 0);
	    uni_phased.push_back(-i);
	}
	else if (v.lits_count(1) == 0){ //no negative phased lits.
	    queue_implication( i+i, NULL_CLAUSE, 0);
	    uni_phased.push_back(i);
	}
    }
    if (_params.verbosity>1 && uni_phased.size() > 0) {
	cout << uni_phased.size()<< " Variables only appear in one phase." << endl;
	if (_params.verbosity > 2) {
	    for (i=0; i< uni_phased.size(); ++i)
		cout << uni_phased[i] << " ";
	    cout <<endl;
	}
    }
    //3. Unit clauses
    for (i=0, sz=clauses().size(); i<sz; ++i) {
	if (clause(i).status()!=DELETED_CL)
	    if (clause(i).num_lits() == 1){ //unit clause
		if (variable(clause(i).literal(0).var_index()).value() == UNKNOWN)
		    queue_implication(clause(i).literal(0).s_var(), i, 0);
	    }
    }
    if(deduce()==CONFLICT) {
    	cout << " CONFLICT during preprocess " <<endl;
#ifdef VERIFY_ON
	for (unsigned i=1; i< variables().size(); ++i) {
	    if (variable(i).value() != UNKNOWN) {
		assert (variable(i).dlevel() <= 0); 
		int ante = variable(i).antecedent();
		int ante_id = 0;
		if (ante >= 0) {
		    ante_id = clause(ante).id();
		    verify_out << "VAR: " << i 
			       << " L: " << variable(i).assgn_stack_pos()
			       << " V: " << variable(i).value() 
			       << " A: " << ante_id 
			       << " Lits:";
		    for (unsigned j=0; j< clause(ante).num_lits(); ++j)
			verify_out <<" " <<  clause(ante).literal(j).s_var();
		    verify_out << endl;
		}
	    }
	}
	verify_out << "CONF: " << clause(_conflicts[0]).id() << " ==";
	for (unsigned i=0; i< clause(_conflicts[0]).num_lits(); ++i) {
	    int svar = clause(_conflicts[0]).literal(i).s_var();
	    verify_out << " " << svar;
	}
#endif
	return CONFLICT;
    }
    
    cout<<_assignment_stack[0]->size()<<" vars set during preprocess; "<<endl;
    return NO_CONFLICT;    
}

void CSolver::mark_var_unbranchable(int vid)
{
    
    if (variable(vid).is_branchable()==true) {
	variable(vid).disable_branch();
	if (variable(vid).value()==UNKNOWN)
	    --num_free_variables();
    }
}

void CSolver::mark_var_branchable(int vid)
{
    CVariable & var = variable(vid);
    if (var.is_branchable()==false) {
	var.enable_branch();
	if (var.value() == UNKNOWN) {
	    ++ num_free_variables();
	    if (var.var_score_pos() < _max_score_pos)
		_max_score_pos = var.var_score_pos();
	}
    }
}

ClauseIdx CSolver::add_orig_clause (int * lits, int n_lits, int gid)
{
    int cid = add_clause_with_gid(lits, n_lits, gid);
    if (cid >= 0){
	clause(cid).set_status(ORIGINAL_CL);
	clause(cid).activity()=0; // just to initialize
    }	
    return cid;
}

ClauseIdx CSolver::add_clause_with_gid (int * lits, int n_lits, int gid)
{
    assert (dlevel() >= 0);
    unsigned gflag;
    if (gid == PERMANENT_GID ) 
	gflag = 0;
    else if (gid == VOLATILE_GID)
	gflag = (~0x0);
    else {
	assert (gid <= WORD_WIDTH && gid > 0);
	gflag = (1 << (gid- 1));
    }
    ClauseIdx cid = add_clause( lits, n_lits, gflag);
    if (cid < 0) {
	_stats.is_mem_out = true;
	_stats.outcome = MEM_OUT;
	return cid;
    }
    return cid;
}

ClauseIdx CSolver::add_conflict_clause (int * lits, int n_lits, int gflag)
{
    ClauseIdx cid = add_clause(lits, n_lits, gflag);

#if 0
    cout << "conflict clause " << cid << " = [";
    for( int i = 0; i < n_lits; ++i ) cout << " " << (lits[i]%2?'-':'+') << (lits[i]>>1);
      cout << " ], at dlevel = " << _dlevel << endl;
#endif

    if (cid >= 0) {
	clause(cid).set_status(CONFLICT_CL);
	clause(cid).activity()=0;
    }
    else {
	_stats.is_mem_out = true;
	_stats.outcome = MEM_OUT;
    }
    return cid;
}

void CSolver::real_solve(void)
{
    while(_stats.outcome == UNDETERMINED) {
	run_periodic_functions();
	if (decide_next_branch()) {
	    while (deduce()==CONFLICT) { 
	        int blevel;
		blevel = analyze_conflicts();
		if (blevel < 0) {
		    _stats.outcome = UNSATISFIABLE;
		    return;
		}
	    }
	}
	else {
            if(_sat_hook != NULL && _sat_hook(this))
                continue;
	    _stats.outcome = SATISFIABLE;
	    return;
	}
	if (time_out()) { 
	    _stats.outcome = TIME_OUT;
	    return; 
	}
	if (_force_terminate) { 
	    _stats.outcome = ABORTED;
	    return; 
	}
	if (_stats.is_mem_out) {
	    _stats.outcome = MEM_OUT;
	    return; 
	}	    
    }
}

int CSolver::solve(void)
{
//    DBG1(dump());
    if (_stats.outcome == UNDETERMINED) {
	init_solve();
	//preprocess 
	DBG1(dump_assignment_stack(););
	if(preprocess()==CONFLICT) 
	    _stats.outcome = UNSATISFIABLE;
	else //the real search
	    real_solve();
        cout<<endl;
	_stats.finish_cpu_time = get_cpu_time();
    }
    int result = _stats.outcome;
//    _stats.outcome = UNDETERMINED;
    return result;
}

void CSolver::back_track(int blevel)
{
    DBG1(cout << "Back track to " << blevel <<" ,currently in "<< dlevel() << " level" << endl;);
    DBG2 (dump());
    CHECK(verify_integrity());
    assert(blevel <= dlevel());
    for (int i=dlevel(); i>= blevel; --i) {
	vector<int> & assignments = *_assignment_stack[i];
	for (int j=assignments.size()-1 ; j>=0; --j) 
	    unset_var_value(assignments[j]>>1);
	assignments.clear();
    }
    dlevel() = blevel - 1;
    if (dlevel() < 0 ) 
	dlevel() = 0;
    ++_stats.num_backtracks;
    DBG2 (dump());
    CHECK(verify_integrity());
}

int CSolver::deduce(void) 
{
    while (!_implication_queue.empty() && _conflicts.size()==0) {
	DBG2(dump_implication_queue(););
	const CImplication & imp = _implication_queue.front();
	int lit = imp.lit;
	int vid = lit>>1;
	int dl = imp.dlevel;
	ClauseIdx cl = imp.antecedent;
	_implication_queue.pop();
	CVariable & var = variable(vid);
	if ( var.value() == UNKNOWN) { // an implication
	    set_var_value(vid, !(lit&0x1), cl, dl);
	    //var.assgn_stack_pos() = _assignment_stack[dl]->size();
	    //_assignment_stack[dl]->push_back(lit);
	    CHECK(verify_integrity());
	}
	else if (var.value() == (unsigned)(lit&0x1) ) { //a conflict
	    //note: literal & 0x1 == 1 means the literal is in negative phase
	    //when a conflict occure at not current dlevel, we need to backtrack
	    //to resolve the problem.
	    //conflict analysis will only work if the conflict occure at 
	    //the top level (current dlevel)
	    if (dl == dlevel()) {
		_conflicts.push_back(cl);
		continue;
	    }
	    else {
		assert (dl < dlevel());
		int orig_dl = var.dlevel();
		vector<CImplication> still_valid;
		if (orig_dl < dl) //this means other implication will take care of it.
		    continue;
		if (orig_dl > dl) {
		    //in this case, backtrack to orig_dl level, and do the implication need to be done
		    while (!_implication_queue.empty()) {
			if (_implication_queue.front().dlevel < orig_dl ) 
			    still_valid.push_back(_implication_queue.front());
			_implication_queue.pop();
		    }
		    for (unsigned i=0; i< still_valid.size(); ++i) 
			_implication_queue.push(still_valid[i]);
		    back_track(orig_dl);
		    set_var_value(vid, !(lit&0x1), cl, dl);
		    //var.assgn_stack_pos() = _assignment_stack[dl]->size();
		    //_assignment_stack[dl]->push_back(lit);
		}
		else { 
		    assert (orig_dl == dl);
		    //in this case, backtrack to that dlevel and it's a conflict clause
		    back_track(orig_dl + 1);
		    _conflicts.push_back(cl);
		}
	    }
	}
	else {
	    //so the variable have been assigned before
	    if(var.antecedent()!=NULL_CLAUSE&&clause(cl).num_lits() < clause(var.antecedent()).num_lits()) 
	    	var.antecedent()=cl;
	    if (var.dlevel() <= dl) continue;
	    int old_dl = var.dlevel();
	    int old_pos = var.assgn_stack_pos();
	    assert ((*_assignment_stack[old_dl])[old_pos] == lit);
	    (*_assignment_stack[old_dl])[old_pos] = 0; //insert a bubble
	    unset_var_value(vid);
	    set_var_value(vid, !(lit& 0x1), cl, dl);
	    //var.assgn_stack_pos() = _assignment_stack[dl]->size();
	    //_assignment_stack[dl]->push_back(lit);
	}
    }
    //if loop exited because of a conflict, we need to clean implication queue
    while(!_implication_queue.empty()) 
	_implication_queue.pop();
    return (_conflicts.size()?CONFLICT:NO_CONFLICT);
}


void CSolver::verify_integrity(void) 
{	
    for (unsigned i=1; i< variables().size(); ++ i) {
	if (variable(i).value() != UNKNOWN) {
	    int pos = variable(i).assgn_stack_pos();
	    int value = variable(i).value();
	    int dlevel = variable(i).dlevel();
	    assert ((*_assignment_stack[dlevel])[pos] == (int) (i+i+1-value));
	}
    }
    for (unsigned i=0; i< clauses().size(); ++i) {
	if (clause(i).status() == DELETED_CL)
	    continue;
	CClause & cl = clause(i);
	int num_0 = 0;
	int num_1 = 0;
	int num_unknown = 0;
	int watched[2]; 
	int watch_index = 0;
	watched[1] = watched[0] = 0;
	for (unsigned j=0; j< cl.num_lits(); ++j) {
	    CLitPoolElement lit = cl.literal(j);
	    int vid = lit.var_index();
	    if (variable(vid).value() == UNKNOWN)
		++ num_unknown;
	    else {
		if (literal_value(lit) == 0)
		    ++ num_0;
		else 
		    ++ num_1;
	    }
	    if (lit.is_watched()) {
		watched[watch_index] = lit.s_var();
		++watch_index;
	    }
	}
	if (watch_index==0) {
	    assert ( cl.num_lits() == 1);
	    continue;
	}
	assert (watch_index == 2);
	for (unsigned j=0; j< cl.num_lits(); ++j) {
	    CLitPoolElement lit = cl.literal(j);
	    int vid1 = (watched[0]>>1);
	    if (variable(vid1).value() == (unsigned)(watched[0] & 0x1)) {
		if (!lit.is_watched()) {
		    assert (literal_value(lit) == 0);
		    assert (variable(lit.var_index()).dlevel() <= variable(vid1).dlevel());
		}
	    }
	    int vid2 = (watched[1]>>1);
	    if (variable(vid2).value() == (unsigned)(watched[1] & 0x1)) {
		if (!lit.is_watched()) {
		    assert (literal_value(lit) == 0);
		    assert (variable(lit.var_index()).dlevel() <= variable(vid1).dlevel());
		}
	    }
	}
    }
}

void CSolver::mark_vars_of_dl(vector<int> & lits, int dl)
//given a vector of lits (all are "in_new_clause", which is generated by
//last round of mark_vars_at_level, mark_vars_of_dl will mark those which dlevel = dl,
//and put the rest into _conflict_lits
{
    assert (_num_marked == 0);
    DBG1(
	cout << "The Lits involved are : " << endl;
	for (unsigned i=0; i< lits.size(); ++i) {
	    cout << "V: " << (lits[i] & 0x1 ? "-":"+") << (lits[i] >> 1) << ": " 
		 << variable(lits[i]>>1);
	}
	cout << endl << endl;
	);
    
    for (vector<int>::iterator itr = lits.begin(); itr != lits.end(); ++ itr) {
	int v = (*itr)>>1;
	assert (variable(v).new_cl_phase() != UNKNOWN);
	if (variable(v).dlevel() == dl) {
	    -- _num_in_new_cl;
	    variable(v).set_new_cl_phase(UNKNOWN);
	    assert (!variable(v).is_marked());
	    variable(v).set_marked();
	    ++ _num_marked;
	}
	else {
	    assert (variable(v).dlevel() < dl);
	    _conflict_lits.push_back((*itr));
	}
    }
}

void CSolver::mark_vars_at_level(ClauseIdx cl, int var_idx, int dl)
{
    assert (_resolvents.empty() || var_idx != -1);
#ifdef VERIFY_ON	
    _resolvents.push_back(clause(cl).id());
#endif
    for (CLitPoolElement * itr = clause(cl).literals(); (*itr).val() > 0 ; ++ itr) {
	int v = (*itr).var_index();
	if (v == var_idx) 
	    continue;
	else if (variable(v).dlevel() == dl) {
	    if (!variable(v).is_marked()) {
		variable(v).set_marked();
		++ _num_marked;
                if(_mark_increase_score){
                    int tmp=itr->s_var();
                    adjust_variable_order(&tmp,1);
                }
	    }
	}
	else {
	    assert (variable(v).dlevel() < dl);
	    if (variable(v).new_cl_phase() == UNKNOWN){ //it's not in the new cl
                if(variable(v).dlevel()){
		    ++ _num_in_new_cl;
		    variable(v).set_new_cl_phase((*itr).var_sign());
		    _conflict_lits.push_back((*itr).s_var());
                }
	    }
	    else //if this variable is already in the new clause, it must have the same phase 
		assert(variable(v).new_cl_phase() == (*itr).var_sign());
	}
    }
}

int CSolver::analyze_conflicts(void) {
    assert (_conflicts.size() > 0);
    assert(_conflict_lits.size() == 0); 
    assert (_implication_queue.empty());
    assert (_num_marked == 0);
    DBG1(dump_assignment_stack());
    CHECK(verify_integrity());

    if (dlevel() == 0){ //already at level 0. Conflict means unsat.
#ifdef VERIFY_ON
	for (unsigned i=1; i< variables().size(); ++i) {
	    if (variable(i).value() != UNKNOWN) {
		assert (variable(i).dlevel() <= 0); 
		int ante = variable(i).antecedent();
		int ante_id = 0;
		if (ante >= 0) {
		    ante_id = clause(ante).id();
		    assert(clause(ante).status()!=DELETED_CL);
		    verify_out << "VAR: " << i 
			       << " L: " << variable(i).assgn_stack_pos()
			       << " V: " << variable(i).value() 
			       << " A: " << ante_id 
			       << " Lits:";
		    for (unsigned j=0; j< clause(ante).num_lits(); ++j)
			verify_out << " " << clause(ante).literal(j).s_var();
		    verify_out << endl;
		}
	    }
	}
	ClauseIdx shortest;
	shortest=_conflicts.back();
	int len=(int) clause(_conflicts.back()).num_lits();
	while(!_conflicts.empty()){	
		if((int) clause(_conflicts.back()).num_lits() < len ){
			shortest=_conflicts.back();
			len=clause(_conflicts.back()).num_lits();
		}	
		_conflicts.pop_back();
	}
	verify_out << "CONF: " << clause(shortest).id() << " ==";
	for (unsigned i=0; i< clause(shortest).num_lits(); ++i) {
	    int svar = clause(shortest).literal(i).s_var();
	    verify_out << " " << svar;
	}
	verify_out<<endl;
#endif	
	_conflicts.clear();
	back_track(0);
	return -1;
    }
    
    int result;
//    int result = conflict_analysis_firstUIP_resolve_based()
    result = conflict_analysis_firstUIP();
//    int result = conflict_analysis_allUIP();
//    int result = conflict_analysis_decisions_only();
    DBG1( dump_assignment_stack(););
    return result;
}


//when all the literals involved are in _conflict_lits
//call this function to finish the adding clause and backtrack

int CSolver::finish_add_conf_clause(int gflag)
    
{
    unsigned int i,sz;
    int back_dl = 0;
    int unit_lit = -1;
    ClauseIdx added_cl = add_conflict_clause(&(*_conflict_lits.begin()), _conflict_lits.size(), gflag);
    if (added_cl < 0 ) { //memory out.
	_stats.is_mem_out = true;
	_conflicts.clear();
	assert (_implication_queue.empty());
	return 1; 
    }
    int new_cls_len=clause(added_cl).num_lits();
    _stats.sum_sizes_iter += new_cls_len;
    _stats.sum_squares_iter += new_cls_len*new_cls_len;
    _stats.num_conf_cls_iter += 1;
    
    top_unsat_cls=clauses().end();
    top_unsat_cls--;
#ifdef VERIFY_ON
    verify_out << "CL: " <<  clause(added_cl).id() << " <=";
    for (unsigned i=0; i< _resolvents.size(); ++i)
	verify_out << " " <<  _resolvents[i];
    verify_out << endl;
    _resolvents.clear();
#endif

    adjust_variable_order (&(*_conflict_lits.begin()), _conflict_lits.size());
    
    DBG0( cout << "**Add Clause " <<added_cl<< " : "<<clause(added_cl) << endl; );
    if(_params.shrinking.enable){
        if(_shrinking_cls.size())
            _shrinking_cls.clear();
        if(_shrinking_conf_cls_length){
            int benefit=_shrinking_conf_cls_length-_conflict_lits.size();
            _shrinking_benefit+=benefit;
            _shrinking_conf_cls_length=0;
            _recent_shrinkings.push(benefit);
            if(_recent_shrinkings.size()>20){
                _shrinking_benefit-=_recent_shrinkings.front();
                _recent_shrinkings.pop();
            }
        }
        if(_conflict_lits.size()>_params.shrinking.size){
            _shrinking_cls.clear();
            for(i=0,sz=_conflict_lits.size();i<sz;i++)
                _shrinking_cls.insert(pair<int,int>(variable(_conflict_lits[i]>>1).dlevel(),_conflict_lits[i]));
            int prev_dl=_shrinking_cls.begin()->first;
            multimap<int,int>::iterator mmi,next;
            mmi=_shrinking_cls.end();
            mmi--;
            int last_dl=mmi->first;
            bool found_break=false;
            for(mmi=_shrinking_cls.begin();mmi!=_shrinking_cls.end()&&mmi->first!=last_dl;){
                if(mmi->first-prev_dl>2){
                    found_break=true;
                    break;
                }
                prev_dl=mmi->first;
                next=mmi;
                next++;
                _shrinking_cls.erase(mmi);
                mmi=next;
            }
            if(found_break&&_shrinking_cls.size()>0&&prev_dl<dlevel()-1){
                _shrinking_conf_cls_length=_conflict_lits.size();
                _stats.num_shrinkings++;
                back_dl=prev_dl;
                back_track(back_dl+1);
                _conflicts.clear();
                _resolvents.clear();
                _num_in_new_cl=0;
                for(i=0,sz=_conflict_lits.size();i<sz;i++)
    	            variable(_conflict_lits[i]>>1).set_new_cl_phase(UNKNOWN);
                _conflict_lits.clear();
                if(_stats.num_shrinkings%20==0&&_recent_shrinkings.size()==20){
                    if(_shrinking_benefit>800)
                       _params.shrinking.size-=5;
                    else if(_shrinking_benefit<600)
                       _params.shrinking.size+=10;
                }
                
                return back_dl;
            }
        }
    }
    for (i=0; i< clause(added_cl).num_lits(); ++i) {
	int vid = clause(added_cl).literal(i).var_index();
	int sign =clause(added_cl).literal(i).var_sign();
	assert (variable(vid).value() != UNKNOWN);
	assert (literal_value(clause(added_cl).literal(i)) == 0);
	int dl = variable(vid).dlevel();
	if ( dl < dlevel()) {
	    if (dl > back_dl) 
		back_dl = dl;
	}
	else {
	    assert (unit_lit == -1);
	    unit_lit = vid + vid + sign; 
	}
    }
    if(back_dl==0) {
    	_stats.next_restart = _stats.num_backtracks + _stats.restart_incr;
    	_stats.next_cls_deletion =_stats.num_backtracks + _params.cls_deletion.interval;
    }

    back_track(back_dl + 1);
    queue_implication(unit_lit, added_cl, back_dl);
	    
    for (i=1; i< _conflicts.size(); ++i) //after resolve the first conflict, others must also be resolved
	assert(!is_conflicting(_conflicts[i]));
    _conflicts.clear();

    while (_conflict_lits.size()) {
	int svar = _conflict_lits.back();
	_conflict_lits.pop_back();
	CVariable & var = variable(svar >> 1);
	assert (var.new_cl_phase() == (unsigned)(svar & 0x1));
	-- _num_in_new_cl;
	var.set_new_cl_phase(UNKNOWN);
    }
    assert (_num_in_new_cl == 0);
    DBG0(cout << "Conflict Analasis: conflict at level: " << back_dl + 1;
	 cout << "  Assertion Clause is " << added_cl<< endl; );

    return back_dl;
}

int CSolver::conflict_analysis_grasp (void)
{
    fatal(_POSITION_, "Not implemented in this version");
    return 0;
}

int CSolver::conflict_analysis_decisions_only (void)
{
    //For experiment purpose only, don't use it 
    unsigned int i;
    ClauseIdx cl = _conflicts[0];
    int gflag = clause(cl).gflag();
    vector<int> involved_lits;
    for (i=0; i < clause(cl).num_lits(); ++i) {
	int svar = clause(cl).literal(i).s_var();
	++ _num_marked;
	variable(svar>>1).set_marked();
	involved_lits.push_back(svar);
    }
    for (i=0; i<involved_lits.size(); ++i) {
	int vid = involved_lits[i] >> 1;
	if (variable(vid).get_antecedent() != NULL_CLAUSE) {
	    //it's not a decision variable
	    CClause & c = clause(variable(vid).get_antecedent());
	    gflag |= c.gflag();
	    for (unsigned j=0; j< c.num_lits(); ++j) {
		int lit = c.literal(j).s_var();
		if (variable(lit>>1).is_marked()==false) {
		    ++ _num_marked;
		    variable(lit>>1).set_marked();
		    involved_lits.push_back(lit);
		}
	    }
	}
    }
    for (vector<int>::iterator itr = involved_lits.begin(); 
	 itr != involved_lits.end(); ++itr) {
	int svar = *itr;
	if (variable(svar>>1).get_antecedent() == NULL_CLAUSE) {
	    ++ _num_in_new_cl;
	    variable(svar>>1).set_new_cl_phase(svar & 0x1);
	    _conflict_lits.push_back(svar);
	}
	assert (variable(svar>>1).is_marked());
	-- _num_marked;
	variable(svar>>1).clear_marked();
    }
    assert (_num_marked == 0);
		    		
    return finish_add_conf_clause(gflag);
}

int CSolver::conflict_analysis_allUIP (void)
{
    //For experiment purpose only, don't use it 
    vector<int> real_conflict_lits;

    ClauseIdx cl = _conflicts[0];
    DBG0(cout <<"Conflict clause: " << cl << " : " << clause(cl) << endl;);
    mark_vars_at_current_dlevel (cl, -1 /*var*/);
    unsigned gflag = clause(cl).gflag();
    vector <int> & assignments = *_assignment_stack[dlevel()]; //current dl must be the conflict cl.
    for (int i=assignments.size()-1; i >= 0; --i) { //now add conflict lits, and unassign vars
	int assigned = assignments[i];
	if (variable(assigned>>1).is_marked()) {  
	    //this variable is involved in the conflict clause or it's antecedent
	    variable(assigned>>1).clear_marked();
	    -- _num_marked; 
	    ClauseIdx ante_cl = variables()[assigned>>1].get_antecedent();
	    if ( _num_marked == 0 ) { 
				//the first UIP encountered
		real_conflict_lits.push_back(assigned ^ 0x1); //this is the flipped lit
		break;
	    }
	    else {
		assert ( ante_cl != NULL_CLAUSE );
		gflag |= clause(ante_cl).gflag();
		mark_vars_at_current_dlevel(ante_cl, assigned>>1/*var*/);
	    }
	}
    }
    //at this point, real_conflict_lits has the firstUIP, _conflict_lits has the vars involved in other dlevel
    vector<int> current;
    for (int dl = dlevel() - 1; dl > 0; --dl) {
	DBG1(cout << "Processing Dlevel " << dl << endl; );
	current.swap(_conflict_lits);
	_conflict_lits.clear();
	assert (_num_marked == 0);
	mark_vars_of_dl(current, dl);
	if (_num_marked == 0) //conflict does not involve this level
	    continue;
	vector <int> & assignments = *_assignment_stack[dl]; 
	for (int i=assignments.size()-1; i >= 0; --i) { //now add conflict lits, and unassign vars
	    int assigned = assignments[i];
	    if (variable(assigned>>1).is_marked()) {  
				//this variable is involved in the conflict clause or it's antecedent
		variable(assigned>>1).clear_marked();
		-- _num_marked; 
		ClauseIdx ante_cl = variables()[assigned>>1].get_antecedent();
		if ( _num_marked == 0 ) { 
		    //UIP encountered
		    real_conflict_lits.push_back(assigned^0x1);  // add this assignment
		    DBG1(cout << "Put " << (assigned&0x1 ? "+":"-") << (assigned >> 1) << "in conflict clause" << endl;);
		    break; //go on to the next level
		}
		else {
		    assert ( ante_cl != NULL_CLAUSE );
		    gflag |= clause(ante_cl).gflag();
		    mark_vars_at_level(ante_cl, assigned>>1/*var*/, dl);
		}
	    }	    
	}
    }
    assert (_num_marked == 0);
    for (unsigned j=0; j< real_conflict_lits.size(); ++j ) {
	int svar = real_conflict_lits[j];
	assert (variable(svar>>1).new_cl_phase() == UNKNOWN);
	variable(svar>>1).set_new_cl_phase(svar & 0x1);
	_conflict_lits.push_back(svar);
	++ _num_in_new_cl;
    }
    return finish_add_conf_clause(gflag);
}

int CSolver::conflict_analysis_lastUIP(void)
{
    //For experiment purpose only, don't use it 
    ClauseIdx cl = _conflicts[0];
    mark_vars_at_current_dlevel (cl, -1 /*var*/);
    unsigned gflag = clause(cl).gflag();
    vector <int> & assignments = *_assignment_stack[dlevel()]; //current dl must be the conflict cl.
    for (int i=assignments.size()-1; i >= 0; --i) { //now add conflict lits, and unassign vars
	int assigned = assignments[i];
	if (variable(assigned>>1).is_marked()) {  
	    //this variable is involved in the conflict clause or it's antecedent
	    variable(assigned>>1).clear_marked();
	    -- _num_marked; 
	    ClauseIdx ante_cl = variables()[assigned>>1].get_antecedent();
	    if ( ante_cl == NULL_CLAUSE ) { //last UIP is the decision varialbe
		assert (i == 0);	     
		assert (_num_marked == 0); //last UIP is still a UIP
		assert (variable(assigned>>1).new_cl_phase() == UNKNOWN);
		_conflict_lits.push_back(assigned^0x1);  // add this assignment's reverse, e.g. UIP
		++ _num_in_new_cl;
		variable(assigned>>1).set_new_cl_phase((assigned^0x1)&0x1);
	    }
	    else {
		assert ( ante_cl != NULL_CLAUSE );
		gflag |= clause(ante_cl).gflag();
		mark_vars_at_current_dlevel(ante_cl, assigned>>1/*var*/);
	    }
	}
    }
    return finish_add_conf_clause(gflag);
}

int CSolver::conflict_analysis_firstUIP (void) 
{
    int min_conf_id=_conflicts[0];
    int min_conf_length=-1;
    ClauseIdx cl;
    unsigned gflag; 
    _mark_increase_score=false;
    if(_conflicts.size()>1){
        for(vector<ClauseIdx>::iterator ci=_conflicts.begin();ci!=_conflicts.end();ci++){
            assert (_num_in_new_cl==0);
            assert (dlevel() > 0);
            cl = *ci;
            clause(cl).activity()++;
            DBG0(cout <<"Conflict clause: " << cl << " : " << clause(cl) << endl;);
            mark_vars_at_current_dlevel (cl, -1 /*var*/);
            gflag = clause(cl).gflag();
            vector <int> & assignments = *_assignment_stack[dlevel()]; //current dl must be the conflict cl.
            for (int i=assignments.size()-1; i >= 0; --i) { //now add conflict lits, and unassign vars
                int assigned = assignments[i];
        	    if (variable(assigned>>1).is_marked()) {  
        	        //this variable is involved in the conflict clause or it's antecedent
        	        variable(assigned>>1).clear_marked();
        	        --_num_marked; 
        	        ClauseIdx ante_cl = variables()[assigned>>1].get_antecedent();
        	        if ( _num_marked == 0 ) { 
        		    //the first UIP encountered, conclude add clause
        		    assert (variable(assigned>>1).new_cl_phase() == UNKNOWN);
        		    _conflict_lits.push_back(assigned^0x1);  // add this assignment's reverse, e.g. UIP
        		    ++ _num_in_new_cl;
        		    variable(assigned>>1).set_new_cl_phase((assigned^0x1)&0x1);
        		    break; //if do this, only one clause will be added.
        	        }
        	        else {
        		    assert ( ante_cl != NULL_CLAUSE );
        		    gflag |= clause(ante_cl).gflag();
        		    mark_vars_at_current_dlevel(ante_cl, assigned>>1/*var*/);
                        clause(ante_cl).activity()++;
        	        }
                }
            }
            if(min_conf_length==-1||_conflict_lits.size()<min_conf_length){
                min_conf_length=_conflict_lits.size();
                min_conf_id=cl;
            }
            //cout<<"current is "_conflict_lits.size()<<" Min is "<<_min_conf_clause.size()<<endl;
            
            for(vector<int>::iterator vi=_conflict_lits.begin();vi!=_conflict_lits.end();vi++) {
       	        int s_var = *vi;
    	        CVariable & var = variable(s_var >> 1);
    	        assert (var.new_cl_phase() == (unsigned)(s_var & 0x1));
    	        var.set_new_cl_phase(UNKNOWN);
            }
            _num_in_new_cl=0;
            _conflict_lits.clear();
            _resolvents.clear();
        }
    }
    assert(_num_marked==0);
    cl = min_conf_id;
    clause(cl).activity()+=5;
    DBG0(cout <<"Conflict clause: " << cl << " : " << clause(cl) << endl;);
    _mark_increase_score=true;
    mark_vars_at_current_dlevel (cl, -1 /*var*/);
    gflag = clause(cl).gflag();
    vector <int> & assignments = *_assignment_stack[dlevel()]; //current dl must be the conflict cl.
    for (int i=assignments.size()-1; i >= 0; --i) { //now add conflict lits, and unassign vars
        int assigned = assignments[i];
	if (variable(assigned>>1).is_marked()) {  
	    //this variable is involved in the conflict clause or it's antecedent
	    variable(assigned>>1).clear_marked();
	    --_num_marked; 
	    ClauseIdx ante_cl = variables()[assigned>>1].get_antecedent();
	    if ( _num_marked == 0 ) { 
		//the first UIP encountered, conclude add clause
		assert (variable(assigned>>1).new_cl_phase() == UNKNOWN);
		_conflict_lits.push_back(assigned^0x1);  // add this assignment's reverse, e.g. UIP
		++ _num_in_new_cl;
		variable(assigned>>1).set_new_cl_phase((assigned^0x1)&0x1);
		break; //if do this, only one clause will be added.
	    }
	    else{
		assert ( ante_cl != NULL_CLAUSE );
		gflag |= clause(ante_cl).gflag();
		mark_vars_at_current_dlevel(ante_cl, assigned>>1/*var*/);
                clause(ante_cl).activity()+=5;
            }
        }
    }
    return finish_add_conf_clause(gflag);
}		

void CSolver::print_cls(ostream & os)
{
    for (unsigned i=0; i< clauses().size(); ++i) {
	CClause & cl = clause(i);
	if (cl.status() == DELETED_CL)
	    continue;
	if (cl.status() == ORIGINAL_CL)
	    os <<"0 ";
	else {
	    assert (cl.status() == CONFLICT_CL);
	    os << "A ";
	}
	for (int i=1; i< 33; ++i) 
	    os <<( cl.gid(i) ? 1:0);
	os << "\t";
	for (unsigned j=0; j< cl.num_lits(); ++j)
	    os << (cl.literal(j).var_sign() ? "-":"") << cl.literal(j).var_index() << " ";
	os <<"0" <<  endl;
	    
    }
}


int CSolver::mem_usage(void) 
{
    int mem_dbase = CDatabase::mem_usage();
    int mem_assignment = 0;
    for (int i=0; i< _stats.max_dlevel; ++i)
	mem_assignment += _assignment_stack[i]->capacity() * sizeof(int);
    mem_assignment += sizeof(vector<int>)* _assignment_stack.size();
    return mem_dbase + mem_assignment;
}

void CSolver::clean_up_dbase(void)
{
    assert (dlevel() == 0);
    assert (_stats.is_solver_started == false);

    int mem_before = mem_usage();
    //1. remove all the learned clauses

    for (vector<CClause>::iterator itr1=clauses().begin(); itr1 != clauses().end()-1; ++itr1) {
	CClause & cl = * itr1;
	if(cl.status()!=ORIGINAL_CL)
            mark_clause_deleted(cl);
    }
    //delete_unrelevant_clauses() is specialized using berkmin deletion strategy.

    //2. free up the mem for the vectors if possible
    for (unsigned i=0; i< variables().size(); ++i) {
	for (int j=0; j< 2; ++j ) { //both phase
	    vector<CLitPoolElement *> watched;
	    vector<CLitPoolElement *> & old_watched = variable(i).watched(j);
	    watched.reserve(old_watched.size());
	    for (vector<CLitPoolElement *>::iterator itr = old_watched.begin(); 
		 itr != old_watched.end(); ++itr)
		watched.push_back(*itr);
	    //because watched is a temp mem allocation, it will get deleted
	    //out of the scope, but by swap it with the old_watched, the contents are reserved.
	    old_watched.swap(watched); 
#ifdef KEEP_LIT_CLAUSES
	    vector<int> lits_cls;
	    vector<int> & old_lits_cls = variable(i).lit_clause(j);
	    lits_cls.reserve(old_lits_cls.size());
	    for (vector<int>::iterator itr = old_lits_cls.begin(); itr != old_lits_cls.end(); ++itr)
		lits_cls.push_back(*itr);
	    old_lits_cls.swap(lits_cls); 
#endif
	}
    } 
    int mem_after = mem_usage();
    if (_params.verbosity > 0) 
	cout << "Database Cleaned, releasing (approximately) " 
	     << mem_before - mem_after << " Bytes" << endl;
}

void CSolver::update_var_score(void) {
    unsigned int i,sz;
    for (i=1, sz=variables().size(); i<sz; ++i) {
	_ordered_vars[i-1].first = & variable(i);
	_ordered_vars[i-1].second = variable(i).score();
    }
    DBG2(
	cout << "Before Update ";
	for (int i=0; i< _ordered_vars.size(); ++i)
	cout << (_ordered_vars[i].first - & variables()[0]) << "(" << _ordered_vars[i].first->score() << ") ";
	cout << endl;
	);
    ::stable_sort(_ordered_vars.begin(), _ordered_vars.end(), cmp_var_stat);
    DBG2(
	cout << "After Update";
	for (int i=0; i< _ordered_vars.size(); ++i)
	cout << (_ordered_vars[i].first - & variables()[0]) << "(" << _ordered_vars[i].first->score() << ") ";
	cout << endl;
	);
    for (i=0, sz= _ordered_vars.size(); i<sz; ++i) 
	_ordered_vars[i].first->set_var_score_pos(i);
    _max_score_pos = 0;
}
	
void CSolver::restart (void){

    _stats.num_restarts += 1;
    if (_params.verbosity > 1 ) 
	cout << "Restarting ... " << endl;

//	To exploit the distribution, 
//	RESTART WITH A NEW INDEPENDENT SEED.. umm how do i do that?
//	
//	here are some choices -> choice of decision variables randomly from a set
	

    if(dlevel()>0)
	back_track(1);

    _stats.restarts_since_vsids +=1;

    assert(dlevel()==0);
/*
    if (dlevel() > 1) {
	if(rand()%2){
		cout << "Backtrack to dlevel 1" <<" from " << dlevel()<< endl;
		back_track(1); //backtrack to level 0. restart.	
	}
	else{
		cout << "Backtrack to  dlevel "<<dlevel()/2 <<" from " << dlevel()<< endl;
		back_track(dlevel()/2);	
	}
    }
*/
    
}

void CSolver::set_up_resolve(ClauseIdx conf_cl, set<CVariable *, cmp_var_assgn_pos> & conf_lits)
{
    assert (_conflict_lits.empty());
    assert (conf_lits.empty());
    CClause & cl = clause(conf_cl);
    for (CLitPoolElement * lit = cl.first_lit(); lit->is_literal(); ++lit) 
	conf_lits.insert(& variable(lit->var_index()));
}

unsigned CSolver::resolve_one_lit(set<CVariable *, cmp_var_assgn_pos> & conf_lits)
{
    assert (!conf_lits.empty());
    CVariable * vptr = (* conf_lits.begin());
    unsigned vid = vptr - &variable(0);
    conf_lits.erase(conf_lits.begin());
    int cl_idx = vptr->antecedent();
    assert (cl_idx != NULL_CLAUSE);
    CClause & cl = clause(cl_idx);
    for (CLitPoolElement * lit = cl.first_lit(); lit->is_literal(); ++lit) {
	if (lit->var_index() != vid) {
	    conf_lits.insert(&variable(lit->var_index()));
	}
    }
    return cl.gflag();
}

bool CSolver::is_uip_reached(set<CVariable *, cmp_var_assgn_pos> & conf_lits)
{
    assert (!conf_lits.empty());
    if (conf_lits.size() == 1) return true;
    set<CVariable *, cmp_var_assgn_pos>::iterator itr = conf_lits.begin();
    CVariable * v1 = *itr;
    ++itr;
    CVariable * v2 = *itr;
    assert (v1->dlevel() >= v2->dlevel());
    if (v1->dlevel() != v2->dlevel())
	return true;
    return false;
}

int CSolver::conflict_analysis_firstUIP_resolve_based(void)
{
    assert (dlevel() > 0);
    ClauseIdx cl = _conflicts[0];
    unsigned gflag = clause(cl).gflag();
    set<CVariable *, cmp_var_assgn_pos> conf_lits;
    set_up_resolve(cl, conf_lits);
    while(!is_uip_reached(conf_lits))
	gflag |= resolve_one_lit(conf_lits);
    vector<int> cls_lits;
    for (set<CVariable *, cmp_var_assgn_pos>::iterator itr = conf_lits.begin();
	 itr != conf_lits.end(); ++itr) {
	int vid = (*itr) - &variable(0);
	int sign = variable(vid).value();
	cls_lits.push_back(vid + vid + sign);
    }
    ClauseIdx added_cl = add_conflict_clause(&(*cls_lits.begin()), cls_lits.size(), gflag);
    if (added_cl < 0 ) { //memory out.
	_stats.is_mem_out = true;
	_conflicts.clear();
	assert (_implication_queue.empty());
	return 1; 
    }
    adjust_variable_order (&(*cls_lits.begin()), cls_lits.size());
    int back_dl = 0;
    int unit_lit = cls_lits[0];
    if (cls_lits.size() > 1) {
	back_dl = variable(cls_lits[1]>>1).dlevel();
    }
    DBG0( cout << "**Add Clause " <<added_cl<< " : "<<clause(added_cl) << endl; );
    back_track(back_dl + 1);
    queue_implication(unit_lit, added_cl, back_dl);
	    
    for (unsigned i=1; i< _conflicts.size(); ++i) //after resolve the first conflict, others must also be resolved
	assert(!is_conflicting(_conflicts[i]));
    _conflicts.clear();
    DBG0(cout << "Conflict Analasis: conflict at level: " << back_dl + 1;
	 cout << "  Assertion Clause is " << added_cl<< endl; );
    return back_dl;
}

//this function can be called within a solving process. i.e. not after
//solve() terminate
int CSolver::add_clause_incr(int * lits, int num_lits, int gid)
{
    unsigned gflag;
    if (gid == PERMANENT_GID ) 
	gflag = 0;
    else if (gid == VOLATILE_GID)
	gflag = ~0x0;
    else {
	assert (gid <= WORD_WIDTH && gid > 0);
	gflag = (1 << (gid- 1));
    }

    int cl = add_clause(lits, num_lits, gflag);
    if (cl < 0) return -1;
    clause(cl).set_status(ORIGINAL_CL);

#if 0
    cout << "new clause " << cl << " = [";
    for( int i = 0; i < num_lits; ++i ) cout << " " << (lits[i]%2?'-':'+') << (lits[i]>>1);
      cout << " ], at dlevel = " << _dlevel << endl;
#endif

    if(clause(cl).num_lits()==1){
        int var_idx=clause(cl).literal(0).var_index();
        if(literal_value(clause(cl).literal(0))==0&&variable(var_idx).dlevel()==0){
            back_track(0);
            if (preprocess()==CONFLICT)
            	_stats.outcome = UNSATISFIABLE;
        }
        else{
            if(dlevel()>0)
                back_track(1);      
            queue_implication(clause(cl).literal(0).s_var(),cl,0);
        }
        return cl;
    }
    int max_level = 0;
    int max_level2 = 0;
    int unit_lit = 0;
    int unknown_count = 0;
    int num_sat = 0;
    int sat_dlevel,max_lit;
    int i, sz;
    bool already_sat=false;

    for (i=0, sz= clause(cl).num_lits(); i<sz; ++i) {
        int var_idx=lits[i]>>1;
        int value=variable(var_idx).value();
        if (value == UNKNOWN) continue;
        if (variable(var_idx).dlevel() == 0 
                && variable(var_idx).antecedent() == -1 
                && literal_value(clause(cl).literal(i)) == 0) {
            back_track(0);
            if (preprocess()==CONFLICT)
            	_stats.outcome = UNSATISFIABLE;
            return cl;
        }
    }
    for (i=0, sz= clause(cl).num_lits(); unknown_count<2&& i<sz; ++i) {
        int var_idx=lits[i]>>1;
        int value=variable(var_idx).value();
        if(value==UNKNOWN){
            unit_lit=clause(cl).literal(i).s_var();
            unknown_count++;
        }
        else{
            int dl=variable(var_idx).dlevel();
            if(dl >= max_level){
                max_level2=max_level;
                max_level=dl;
                max_lit=clause(cl).literal(i).s_var();
            }
            else if(dl > max_level2)
                max_level2=dl;
            int effect=literal_value(clause(cl).literal(i));
            if(effect==1){
                already_sat=true;
                num_sat++;
                sat_dlevel=dl;
            }
        }
    }
    if(unknown_count==0){
        if(already_sat){
            if(num_sat==1&&sat_dlevel==max_level&&max_level>max_level2){
                back_track(max_level2+1);
                queue_implication(max_lit,cl,max_level2);
            }
        }
        else{
            assert(is_conflicting(cl));
            if(max_level>max_level2){
                back_track(max_level2+1);
                queue_implication(max_lit,cl,max_level2);
            }
            else{
                back_track(max_level);
                if (max_level==0 && preprocess()==CONFLICT)
                    _stats.outcome = UNSATISFIABLE;
            }
        }
    }
    else if(unknown_count==1){
        if(!already_sat){
            if(max_level<dlevel())
                back_track(max_level+1);
            queue_implication(unit_lit, cl, max_level);
        }
    }
    return cl;
}

int CSolver::conflict_analysis_mincut (void) 
{
    assert (1 && "Not available, need a mincut library PLED");
    return 0;
//      ClauseIdx cl = _conflicts[0];
//      int asrt = -1;
//      DBG0(cout <<"Conflict clause: " << cl << " : " << clause(cl) << endl;);
//      mark_vars_at_current_dlevel (cl, -1 /*var*/);
//      unsigned gflag = clause(cl).gflag();
//      ClauseIdx added_cl;
//      vector <int> & assignments = *_assignment_stack[dlevel()]; //current dl must be the conflict cl.
//      for (int i=assignments.size()-1; i >= 0; --i) { //now add conflict lits, and unassign vars
//  	int assigned = assignments[i];
//  	if (variable(assigned>>1).is_marked()) {  
//  	    //this variable is involved in the conflict clause or it's antecedent
//  	    variable(assigned>>1).clear_marked();
//  	    -- _num_marked; 
//  	    ClauseIdx ante_cl = variables()[assigned>>1].get_antecedent();
//  	    if ( _num_marked == 0 ) { 
//  		//the first UIP encountered, conclude add clause
//  		assert (variable(assigned>>1).new_cl_phase() == UNKNOWN);
//  		asrt = assigned^0x1; //this is the assert literal
//  		break; //if do this, only one clause will be added.
//  	    }
//  	    else {
//  		assert ( ante_cl != NULL_CLAUSE );
//  		gflag |= clause(ante_cl).gflag();
//  		mark_vars_at_current_dlevel(ante_cl, assigned>>1/*var*/);
//  	    }
//  	}
//      }
//      assert (asrt != -1);
//      if (_conflict_lits.size() == 0) {
//  	_conflict_lits.push_back(asrt);
//  	variable(asrt>>1).set_new_cl_phase( asrt & 0x1);
//  	++ _num_in_new_cl;
//  	return finish_add_conf_clause(gflag);
//      }
//      int nvtxs ;
//      int nedges;
//      vector<int> edge_from;
//      vector<int> edge_to;
//      vector<DOUBLE> ewts;
//      vector<int> part;
//      vector<int> st;
//      vector<int> var_map;
//      var_map.resize(variables().size());
//      int source_size = _conflict_lits.size();
//      int sink_size = 0;
//      for (int i=0; i< _conflict_lits.size(); ++i) {
//  	int svar = _conflict_lits[i];
//  	st.push_back(svar  );
//  	variable(svar>>1).set_marked();
//  	variable(svar>>1).set_new_cl_phase(UNKNOWN);
//  	var_map[svar>>1] = i;
//      }
//      _num_in_new_cl -= _conflict_lits.size();
//      assert (_num_in_new_cl == 0);
//      assert (_num_marked == 0);
//      _num_marked += source_size;
//      _conflict_lits.clear();
//      for (int i=0; i< st.size(); ++i) {
//  	int vid = st[i]>>1;
//  	int sign = st[i] & 0x1;
//  	part.resize(part.size() + 2); //add two vetex
//  	if (i < source_size) 
//  	    part[i+i] = 0;
//  	else 
//  	    part[i+i] = 3;
//  	//split the node
//  	edge_from.push_back(i+i);
//  	edge_to.push_back(i+i+1);
//  	ewts.push_back(1);
//  	//add other edges
//  	ClauseIdx ante = variable(vid).get_antecedent();
//  	if (ante != NULL_CLAUSE && clause(ante).num_lits() > 1) {
//  	    part[i+i+1] = 3;
//  	    CClause & cl = clause(ante);
//  	    for (int j=0; j< cl.num_lits(); ++j) {
//  		int svar = cl.literal(j).s_var();
//  		if (variable(svar>>1).is_marked() == false) {
//  		    variable(svar>>1).set_marked();
//  		    ++ _num_marked;
//  		    var_map[svar>>1] = st.size();
//  		    st.push_back(svar);
//  		}
//  		if (svar != st[i]) {
//  		    edge_from.push_back(i+i+1);
//  		    edge_to.push_back(var_map[svar>>1] << 1);
//  		    ewts.push_back(variables().size());
//  		}
//  	    }
//  	}
//  	else { //decision variable
//  	    part[i+i+1] = 1; //it's a sink
//  	    ++ sink_size;
//  	}
//      }
//      assert (st.size() == part.size()/2);
//      assert (edge_from.size() == edge_to.size());
//      int flow = (int) PLED_FindMaxFlow(part.size(), edge_from.size(),
//  				      edge_from.begin(), edge_to.begin(), 
//  				      ewts.begin(), part.begin());
//      assert (flow <= source_size);
//      for (int i=0; i< edge_from.size(); ++i) {
//  	if (part[edge_from[i]] == 0 && part[edge_to[i]] == 1) {
//  	    assert (edge_from[i] == edge_to[i] -1 );
//  	    int svar = st[edge_from[i]>>1];
//  	    _conflict_lits.push_back(svar);
//  	    variable(svar>>1).set_new_cl_phase(svar & 0x1);
//  	    ++ _num_in_new_cl;
//  	    -- flow;
//  	}
//      }
//      assert (flow == 0);
//      _conflict_lits.push_back(asrt);
//      variable(asrt>>1).set_new_cl_phase( asrt & 0x1);
//      ++ _num_in_new_cl;
//      for (int i=0; i< st.size(); ++i) {
//  	assert (variable(st[i]>>1).is_marked());
//  	variable(st[i]>>1).clear_marked();
//      -- _num_marked;
//      }
//      assert (_num_marked == 0);
//      return finish_add_conf_clause(gflag); 
}		
 

