/*********************************************************************
 Copyright 2000-2003, Princeton University.  All rights reserved. 
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
#include <algorithm>
#include <fstream>
#include "zchaff_solver.h"
#include "stdio.h"
//typedef double DOUBLE;
//  extern "C" DOUBLE PLED_FindMaxFlow(int nvtxs, int nedges,
//  				   int *edge_from, int *edge_to, 
//  				   DOUBLE *ewts, int *part);
//#define VERIFY_ON

#ifdef VERIFY_ON
ofstream verify_out("resolve_trace");
#endif
#ifdef DEBUG_OUT
ofstream solution_file("solutions.txt");
#endif

void CSolver::re_init_stats(void)
{
    _stats.is_mem_out		= false;
    _stats.outcome			= UNKNOWN;
    _stats.next_restart 	= _params.restart.first_restart;
    _stats.restart_incr 	= _params.restart.backtrack_incr;
    _stats.next_cls_deletion 	= _params.cls_deletion.interval; // 600(new), 5000(old)
    _stats.next_var_score_decay = _params.decision.decay_period;
    _stats.current_randomness 	= _params.decision.base_randomness;
    _stats.total_bubble_move 	= 0;

    _stats.num_decisions 	= 0;
#ifdef BIG_NUM
	mpz_init(_stats.num_solutions);
	mpz_set_ui(_stats.num_solutions, 0);
#else
	_stats.num_solutions 	= 0;		// added by sang
#endif
	_stats.num_total_components	= 0;
	_stats.num_split_components = 0;
	_stats.num_not_split_components = 0;
	_stats.num_trivial_components = 0;
	_stats.first_split_level	= 100000; // set to max value
	_stats.num_changed_components = 0;	// added by sang
	_stats.num_cross_implications = 0;	// added by sang
	_stats.num_adjusted_components = 0;	// added by sang
    _stats.num_unsatisfied_clause = 0;  // added by sang
	_stats.num_original_clause = 0;		// added by sang
	_stats.num_unit_clause	= 0;		// added by sang
	_stats.num_active_added_conf_clauses = 0; // added by sang
    _stats.num_backtracks	= 0;
    _stats.max_dlevel 		= 0;
	_stats.num_del_orig_cls		= 0;	// added in zchaff2004
    _stats.num_implications   	= 0;
    _stats.start_cpu_time	= get_cpu_time();
    _stats.finish_cpu_time	= 0;
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
    _params.time_limit				= 3600 * 48;		//2 days
    _params.decision.strategy 			= 0;
    _params.decision.restart_randomness		= 0;	
    _params.decision.base_randomness		= 0;
    _params.decision.decay_period		= 512; // 1024 //0x100;
    _params.decision.bubble_init_step 		= 0x400;
    _params.cls_deletion.enable 		= true ;
	// new parameters for clause deletion in zchaff2004
	_params.cls_deletion.enable 		= true ;
    _params.cls_deletion.head_activity          = 500;//250;//60
    _params.cls_deletion.tail_activity          = 10;//35;//7
    _params.cls_deletion.head_num_lits          = 6;//6;//8
    _params.cls_deletion.tail_num_lits          = 45;//45;//42
    _params.cls_deletion.tail_vs_head           = 16;
    _params.cls_deletion.interval				= 1500; //600;
	// old parameters for clause deletion
    //_params.cls_deletion.interval		= 5000;
    //_params.cls_deletion.max_unrelevance	= 20;
    //_params.cls_deletion.min_num_lits		= 100;
    //_params.cls_deletion.max_conf_cls_size	= 5000;
    _params.restart.enable			= false;	
    _params.restart.first_restart		= 5000;
    _params.restart.backtrack_incr		= 1500;		
    _params.restart.backtrack_incr_incr 	= 200;
}

CSolver::CSolver(void) {
	//for (int i = 0; i < LEVEL_MAX; i++)
	//	level_total_components[i] = level_new_components[i] 
	//	= level_trivial[i] = level_conflicts[i] = level_hits[i] = 0;
    init_parameters();
    init_stats();
	//backtrack_tag = false;	// for debug
	//set_tag = true;		// for debug
	//flag2 = true;			// for debug
	//flag = true;			// for debug
	original_BN_nodes = 0;	// initialized to 0, which means the CNF is not a BN by default
	far_back_track_enabled = false;
	static_heuristic = false;
	cross_enabled = true;	// false -> true
	adjust_enabled = true;	// false -> true
	backtrack_factor = 2;
	candidates.resize(10, 0);
	num_conflicts	= 0;
	num_sat			= 0;
	num_no_components = 0;
    _dlevel			= 0;		
    _force_terminate	= false;
    _implication_id		= 0;
    _num_marked			= 0;
    _num_in_new_cl		= 0;
	//num_no_components	= 0;
	num_far_backtrack	= 0;
    _outside_constraint_hook	= NULL;
	_uni_phased_size	= 0;		// added by sang
	bound				= 1000;		// added by sang
	_gid				= 0;		// added by sang
#ifdef APPROXIMATE_HASHING
#ifndef BIG_NUM
	cache_size			= 5 * 1024 * 1024;	// 5M
	oldest_entry		= 1024 * 1024;	// that's 1M, can be set by -o option
	clean_limit			= 20 * oldest_entry;
#else
	cache_size			= 3 * 1024 * 1024;	// 3M
	oldest_entry		= 1024 * 500;	// that's 500K, can be set by -o option
	clean_limit			= 20 * oldest_entry;
#endif
#else
	cache_size			= 1024 * 1024;	// 1M, can be set by -c option
	oldest_entry		= 50 * 1024;	// that's 50K, can be set by -o option
	clean_limit			= 20 * oldest_entry;
#endif
	max_entry			= 1024 * 1024 * 1024;	// that's 1G
	max_distance		= MAX_DISTANCE;
	//dynamic_heuristic	= false;	// true -> false
	//dynamic_heuristic	= SUM_DEGREE;	// which is -h 3
	dynamic_heuristic	= VSADS;	// which is -h 5
	quiet				= false;
	//approximation		= false;
	max_num_learned_clause = 10*1024*1024;	// 10M
	//hashtable = new CHashTable(cache_size);
	srand(time(NULL));
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

void CSolver::set_BN_node(int BN_nodes)
{
	original_BN_nodes = BN_nodes;
}

void CSolver::set_var_weight(vector<double> * weight)
{
	vector<double> var_weight = * weight;
	for (int i = variables().size()-1; i >= 1; --i)
	{
		CVariable & var = variable(i);
		//if (var_weight[i] > -0.5)	// initialized to -1, > -0.5 means set to some positive value
		var.set_weight(var_weight[i]);
		//else if (original_BN_nodes == 0)	// not a BN CNF, all var weights should be set to 0.5
		//	var.set_weight(0.5);

		if (var.pos_weight + var.neg_weight < 1.5)
			var.internal = false;	// add a tag for a component with only interal nodes
		else
			var.internal = true;	// an internal node has a weight sum of 2
	}
}

void CSolver::set_cache_size(unsigned size)
{
	cache_size = size;
}

void CSolver::set_max_entry(unsigned entry)
{
	max_entry = entry;
}

void CSolver::set_oldest_entry(unsigned oldest)
{
	if (clean_limit == oldest_entry * 20)	// that means -l not set
		clean_limit	= 20 * oldest;	
	oldest_entry = oldest;
}

void CSolver::set_clean_limit(unsigned limit)
{
	clean_limit = limit;
}

void CSolver::set_max_distance(unsigned dist)
{
	max_distance = dist;
}

void CSolver::set_dynamic_heuristic(int dynamic)
{
	switch(dynamic)
	{
		case 0: dynamic_heuristic = DEGREE;
				break;
		case 1: dynamic_heuristic = DEGREE_SUM;
				break;
		case 2: dynamic_heuristic = SUM;
				break;
		case 3: dynamic_heuristic = SUM_DEGREE;
				break;
		case 4: dynamic_heuristic = VSIDS;
				break;
		case 5: dynamic_heuristic = VSADS;
				break;
		case 6: dynamic_heuristic = UNIT_PROP;
				break;
		case 7: dynamic_heuristic = BERKMIN;
				break;
	}
}

//void CSolver::set_approximation(bool app)
//{
//	approximation = app;
//}

void CSolver::set_max_num_learned_clause(unsigned max_num)
{
	max_num_learned_clause = max_num;
}

void CSolver::set_backtrack_factor(double bfactor)
{
	backtrack_factor = bfactor;
}


void CSolver::set_vgo(vector< set<int> > variable_group_ordering)
{
	vgo = variable_group_ordering;
}


void CSolver::set_static_heuristic(bool stat)
{
	static_heuristic = stat;
}


void CSolver::set_far_back_track_flag(bool far)
{
	far_back_track_enabled = far;
}


void CSolver::assign_static_score()
{
	for (int i = vgo.size() - 1; i >= 0; --i)
	{
		set <int> & group = vgo[i];
		for (set<int>::iterator itr = group.begin(); itr != group.end(); ++itr)
			variable(*itr).set_static_score(i);		// the smaller, the better
	}
	cout << "Assigning static scores done" << endl;
	
	//for (int i = 1; i < variables().size(); i++)
	//	cout << variable(i)._static_score << " ";
}


void CSolver::set_cross_flag(bool cross)
{
	cross_enabled = cross;
}

void CSolver::set_adjust_flag(bool adjust)
{
	adjust_enabled = adjust;
}

void CSolver::set_quiet_flag(bool q)
{
	quiet = q;
	//hashtable->quiet = q;
}


double CSolver::elapsed_cpu_time(void) 
{
    return get_cpu_time() - _stats.start_cpu_time;
}

double CSolver::cpu_run_time() // changed by sang, from float to double
{ 
    return (_stats.finish_cpu_time - _stats.start_cpu_time);
}

void CSolver::set_variable_number(int n) 
{ 	
    assert (num_variables() == 0);
    CDatabase::set_variable_number(n);
    _stats.num_free_variables	= num_variables();
	//_component_infor_stack.resize(n);
    while (_assignment_stack.size() <= num_variables())
	{
		_assignment_stack.push_back(new vector<int>);
#ifdef NON_ACTIVE_STACK
		_non_active_free_var_stack.push_back(new vector<int>);
#endif
#ifdef TOCOMPONENTS
		//_component_infor_stack.push_back(new <Component_information>);
		_num_implication_stack.push_back(0);
		//_clause_component_stack.push_back(new vector<unsigned>);
		_branch_infor_stack.push_back(new branch_information);
		_branch_infor_stack.back()->gid = _branch_infor_stack.back()->num_new_children = 0; // initialize!
		_branch_infor_stack.back()->num_children_of_changed = 0;
#else
#ifdef FORMULA_CACHING	// added by sang
		//_formula_stack.push_back(new Component_value_pair);
		//_formula_stack.back()->value = -1;	// initialize formula sat probability to -1
		_num_implication_stack.push_back(0);
		//_hash_index_stack.push_back(new Hashindex);
		//_hash_index_stack.back()->index = 0;
#ifdef APPROXIMATE_HASHING
		_hash_index_stack.back()->secondary_index = 0;
#endif
#endif
#endif
	}
    assert (_assignment_stack.size() == num_variables() + 1);
}

int CSolver::add_variable(void) 
{
    int num = CDatabase::add_variable();
    ++_stats.num_free_variables;
    while (_assignment_stack.size() <= num_variables())
	{
		_assignment_stack.push_back(new vector<int>);
#ifdef NON_ACTIVE_STACK
		_non_active_free_var_stack.push_back(new vector<int>);
#endif

#ifdef TOCOMPONENTS
		_num_implication_stack.push_back(0);
		//_component_infor_stack.push_back(new vector<Component_information>);
		//_clause_component_stack.push_back(new vector<unsigned>);
		_branch_infor_stack.push_back(new branch_information);
		_branch_infor_stack.back()->gid = _branch_infor_stack.back()->num_new_children = 0;	// initialize!
		_branch_infor_stack.back()->num_children_of_changed = 0;
#else
#ifdef FORMULA_CACHING	// added by sang
		_formula_stack.push_back(new Component_value_pair);	
		_formula_stack.back()->value = -1;	// initialize formula sat probability to -1
		_num_implication_stack.push_back(0);
		_hash_index_stack.push_back(new Hashindex);
		_hash_index_stack.back()->index = 0;
#ifdef APPROXIMATE_HASHING
		_hash_index_stack.back()->secondary_index = 0;
#endif
#endif // FORMULA_CACHING
#endif // TOCOMPONENTS
	}
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

void CSolver::enable_cls_deletion(bool allow) 
{
    _params.cls_deletion.enable = allow; 
}

void CSolver::set_cls_del_interval(int n)
{
    _params.cls_deletion.interval = n; 
}

// old functions for clause deletion
/*
void CSolver::set_max_unrelevance(int n ) 
{
    _params.cls_deletion.max_unrelevance = n; 
}

void CSolver::set_min_num_clause_lits_for_delete(int n) 
{
    _params.cls_deletion.min_num_lits = n; 
}

void CSolver::set_max_conflict_clause_length(int l) 
{
    _params.cls_deletion.max_conf_cls_size = l; 
}*/

void CSolver::set_randomness(int n) 
{
    _params.decision.base_randomness = n; 
}

void CSolver::set_random_seed(int seed) {
    srand (seed);
}

void CSolver::add_hook( HookFunPtrT fun, int interval) 
{
    pair<HookFunPtrT, int> a(fun, interval);
    _hooks.push_back(pair<int, pair<HookFunPtrT, int> > (0, a));
}

void CSolver::run_periodic_functions(void)
{
    //a. clause deletion
    /*if ( _params.cls_deletion.enable &&
	 _stats.num_backtracks > _stats.next_cls_deletion) {
	_stats.next_cls_deletion = _stats.num_backtracks + _params.cls_deletion.interval;
	delete_unrelevant_clauses(); 
    }*/

	if (_stats.num_active_added_conf_clauses > _stats.next_cls_deletion)// if there are too many learned clauses, remove some
	{
		_stats.next_cls_deletion += _params.cls_deletion.interval;	// check and delete clauses every 5000 clauses added
		
		/*cout << "before delete clauses: " << endl;
		dump_assignment_stack();
		dump_implication_queue();
		dump_cross_implications();
		dump_branchable_component_stack();
		dump_branch_infor_stack();
		dump_components(_components);*/
		
		delete_unrelevant_clauses(); 

		/*cout << "after delete clauses: " << endl;
		dump_assignment_stack();
		dump_implication_queue();
		dump_cross_implications();
		dump_branchable_component_stack();
		dump_branch_infor_stack();
		dump_components(_components);*/
	}

	 /*
    //b. restart
    if (_params.restart.enable && 
	_stats.num_backtracks > _stats.next_restart) {
	_stats.next_restart = _stats.num_backtracks + _stats.restart_incr;
	_stats.restart_incr += _params.restart.backtrack_incr_incr;
	restart();
    } 
	*/
    //c. update var stats for decision
    if (_stats.num_decisions > _stats.next_var_score_decay) 
	{
		_stats.next_var_score_decay = _stats.num_decisions + _params.decision.decay_period;
		//cout << "before decaying: num_conflicts = " << num_conflicts
		//	 << ", num_sat = " << num_sat << endl;
		//num_conflicts = num_conflicts >> 2;	
		//num_sat = num_sat >> 2;
		//cout << "after decaying: num_conflicts = " << num_conflicts
		//	 << ", num_sat = " << num_sat << endl;
		if (dynamic_heuristic >= 4)		// heuristic 0-3 are sum/degree based, no need to decay scores
			decay_variable_score();
    }
	/*
    //d. run hook functions
    for (unsigned i=0; i< _hooks.size(); ++i) {
	pair<int,pair<HookFunPtrT, int> > & hook = _hooks[i];
	if (_stats.num_decisions >= hook.first) {
	    hook.first += hook.second.second;
	    hook.second.first((void *) this);
	}
    }
	*/
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
    //for (unsigned i=0, sz = variables().size(); i< sz; ++i) {
	if (dynamic_heuristic == VSIDS)
		for (unsigned i=1, sz = variables().size(); i < sz; ++i) 
		{
			variable(i).score(0) = variable(i).lits_count(0);
			variable(i).score(1) = variable(i).lits_count(1);
		}
	else if (dynamic_heuristic == VSADS)
		for (unsigned i=1, sz = variables().size(); i < sz; ++i) 
			variable(i).score(0) = variable(i).score(1) = 0;

    //_ordered_vars.resize( num_variables());
    //update_var_score();
    //DBG2(dump());
}

void CSolver::set_var_value(int v, int value, ClauseIdx ante, int dl)
{
	assert (dl == dlevel());	// added by sang
    assert (value == 0 || value == 1);
    DBG2(dump());
    //CHECK(verify_integrity());
    DBG1 (cout << "Setting\t" << (value>0?"+":"-") << v 
	  << " at " << dlevel() << " because " << ante<< endl;);
    CVariable & var = _variables[v];
    assert (var.value() == UNKNOWN);

	//{	//  this implication might affect some other unbranched components
	/*	vector<int> & var_set = * _component_infor_stack[_branch_infor_stack[dlevel()]->gid]->var_set;
		if (!binary_search(var_set.begin(), var_set.end(), v))
		{
			//return;	// cross implications turned off
			//if (flag)
				cout << "current branching component " << _branch_infor_stack[dlevel()]->gid
					 << ", cross implication of var " << v
					 << ", decisions = " << _stats.num_decisions << endl;
			//--_num_implication_stack[dlevel()]; 
			// put the cross-component-implication in the list for later check
			cross_implication_list.push_back(new cross_implication);
			cross_implication_list.back()->vid = v;
			cross_implication_list.back()->level = dlevel();
			cross_implication_list.back()->marked = false;
			return;	// cross implications turned off
		}
		else 
			++_num_implication_stack[dlevel()]; 
	*/
	//}
	//else 
	//	++_num_implication_stack[dlevel()]; 

    var.set_dlevel(dl);
    var.set_value(value);
    var.antecedent() = ante;
#ifdef KEEP_LIT_CLAUSES		// added by sang, update counter_one for "half" clauses
/*#ifdef DEBUG_OUT
	cout << "set var " << v << ": " << value << endl;
	cout << "var " << v << ", pos: " << endl << "(";
	for (int j=0; j< var.lit_clause(0).size(); ++j)
	    cout << var.lit_clause(0)[j] << "  " ;
	cout << ")" << endl;
	cout << "var " << v << ", neg: " << endl << "(";
	for (int j=0; j< var.lit_clause(1).size(); ++j)
	    cout << var.lit_clause(1)[j] << "  " ;
	cout << ")" << endl;
	//cout << "before update all counter_one" << endl;
	//for (int k=0; k<_stats.num_original_clause; ++k)
	//	cout << "clause " << k << "'s counter_one: " 
	//		 << clause(k).counter_one() << endl;
#endif */
	int sz;
	if (value == 1)
	{
		sz = var.lit_clause(0).size();
		for (int i=0; i< sz; ++i)
		{
			//cout << "increment counter_one of clause " 
			//	 << var.lit_clause(0)[i] << endl;
			(clause(var.lit_clause(0)[i]).counter_one())++;
			//if (clause(var.lit_clause(0)[i]).counter_one() == 1)
			//	_stats.num_unsatisfied_clause--;
		}
	}
    else // value == 0
	{
		sz = var.lit_clause(1).size();
		for (int i=0; i< sz; ++i)
		{
			//cout << "increment counter_one of clause " 
			//	 << var.lit_clause(1)[i] << endl;
			(clause(var.lit_clause(1)[i]).counter_one())++;
			//if (clause(var.lit_clause(1)[i]).counter_one() == 1)
			//	_stats.num_unsatisfied_clause--;
		}
	}
#ifdef DEBUG_OUT
	/*cout << "after update all counter_one" << endl;
	for (int k=0; k<_stats.num_original_clause; ++k)
		cout << "clause " << k << "'s counter_one: " 
			 << clause(k).counter_one() << endl;*/
#endif
#endif
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
	CLitPoolElement * other_watched;
	CLitPoolElement * watched = *itr;

	//watched = watched + watchs.size() - 1;
	//cout << "watched clause index: " << watched->get_clause_index() << endl;
#ifdef DISABLE_CLASUES
	if (clause( watched->find_clause_index()).counter_one() > 1) // make the watched lit possible to move to 1
		continue;	// added by sang, seems not good, because of added conflict clauses
#endif
	
	int dir = watched->direction(); 
	CLitPoolElement * ptr = watched;
	//if (v == 329)
	//	cout << "got before while" << endl;

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
		//otherwise, we have already gone through the whole clause
		int the_value = literal_value (*other_watched);
		if (the_value == 0) //a conflict
		    _conflicts.push_back(cl_idx);
		else if ( the_value != 1) //i.e. unknown
		{
			int unit = other_watched->s_var() >> 1;
			//vector<int> & var_set = * _component_infor_stack[_branch_infor_stack[dlevel()]->gid]->var_set;
			queue_implication(other_watched->s_var(), cl_idx, dlevel());
			/*if (binary_search(var_set.begin(), var_set.end(), unit))
				queue_implication(other_watched->s_var(), cl_idx, dlevel());
			else if (cross_enabled)					
			{
				//if (flag)
				//cout << "branching component " << _branch_infor_stack[dlevel()]->gid
				//	 << ", cross implication of var " << (other_watched->s_var() >> 1) 
				//	 << ", decisions = " << _stats.num_decisions << endl;
				bool found = false;	
				// check if this cross-implication has already been generated at this level
				for (int i = cross_implication_list.size() - 1; i >= 0 ; --i)
				{
					if (cross_implication_list[i]->level != dlevel())
						break;
					if (cross_implication_list[i]->vid == unit)
					{
						found = true;
						break;
					}
				}
				if (!found)		// don't add the duplicate cross implication!
				{
					--_num_implication_stack[dlevel()]; 
					++_stats.num_cross_implications;
					// put the cross-component-implication in the list for later check
					cross_implication_list.push_back(new cross_implication);
					cross_implication_list.back()->vid = unit;
					cross_implication_list.back()->level = dlevel();
					cross_implication_list.back()->marked = false;
					queue_implication(other_watched->s_var(), cl_idx, dlevel());
				}
			}*/				
		}
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

#ifdef DISABLE_CLASUES	// added by sang
	if (clause( watched->find_clause_index()).counter_one())
		continue;	// added by sang, seems not good, because of added conflict clauses
#endif

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
#ifdef KEEP_LIT_CLAUSES
	int sz;
	if (var.value() == 1) 
	{
		sz = var.lit_clause(0).size();
		for (int i=0; i< sz; ++i)
		{
			clause(var.lit_clause(0)[i]).counter_one()--;
			//if (clause(var.lit_clause(0)[i]).counter_one() == 0)
#ifdef DISABLE_CLASUES
			//if (var.lit_clause(0)[i] < _stats.num_original_clause) // newly added
#endif
			//		_stats.num_unsatisfied_clause++;
		}
	}
	else // var.value() == 0
	{
		sz = var.lit_clause(1).size();
		for (int i=0; i< sz; ++i)
		{
			clause(var.lit_clause(1)[i]).counter_one()--;
			//if (clause(var.lit_clause(1)[i]).counter_one() == 0)
#ifdef DISABLE_CLASUES
			//if (var.lit_clause(1)[i] < _stats.num_original_clause) // newly added
#endif
		     //  _stats.num_unsatisfied_clause++;
		}
	}
#endif
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
	if (_variables[var_idx].value() != UNKNOWN) {
	    if (max_level < _variables[var_idx].dlevel())
		max_level =  _variables[var_idx].dlevel();
	}
    }
    return max_level; 
}

void CSolver::dump_assignment_stack(ostream & os ) {
    cout << "Assignment Stack:  " << endl;
    for (int i=0; i<= dlevel(); ++i) 
	{
		cout << "(" <<i << ":";
		if (_assignment_stack[i]->size() > 0)
		{
			cout << ((*_assignment_stack[i])[0]&0x1?"-":"+")
				 << ((*_assignment_stack[i])[0] >> 1);
			if (variable((*_assignment_stack[i])[0] >> 1).tried_both())
			cout << "* ";
		else 
			cout << " ";
		}
		for (unsigned j=1; j<(_assignment_stack[i])->size(); ++j )
			cout << ((*_assignment_stack[i])[j]&0x1?"-":"+")
				 << ((*_assignment_stack[i])[j] >> 1) << " ";
		cout << ") ";
    }
    cout << endl;
}

void CSolver::dump_implication_queue(ostream & os) 
{
    _implication_queue.dump(os);
	cout << endl;
}

void CSolver::delete_clause_group (int gid)
{
    assert( is_gid_allocated(gid) );
    //if (_stats.been_reset==false)
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
	for (int i=0; i<2; ++i) { //for each phase
	    //delete the lit index from the vars
/*#ifdef KEEP_LIT_CLAUSES
	    vector<ClauseIdx> & lit_clauses = (*itr).lit_clause(i);
	    for (vector<ClauseIdx>::iterator itr1 = lit_clauses.begin();
		 itr1 != lit_clauses.end(); ++ itr1) 
		if ( clause(*itr1).status() == DELETED_CL ) {
		    *itr1 = lit_clauses.back();
		    lit_clauses.pop_back();
		    -- itr1;
		}
#endif*/
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
    //_gid = 0; // reset _gid to 0!
    //cout << "reset _gid to 0!" << endl;
}

void CSolver::reset(void)
{
    if (_stats.been_reset)
    	return;
    if (num_variables()==0) return;
    //back_track(0);	commented out by sang
    //_conflicts.clear();commented out by sang
    while (!_implication_queue.empty())
	_implication_queue.pop();
    _stats.is_solver_started 	= false;
    _stats.outcome		= UNDETERMINED;
    _stats.been_reset 		= true;
}


// old version 
/*
void CSolver::delete_unrelevant_clauses(void)
{
    if (CDatabase::_stats.mem_used_up) {
	CDatabase::_stats.mem_used_up = false;
	if (++CDatabase::_stats.mem_used_up_counts < 5) {
	    _params.cls_deletion.max_unrelevance = (int) (_params.cls_deletion.max_unrelevance * 0.8);
	    if (_params.cls_deletion.max_unrelevance < 4)
		_params.cls_deletion.max_unrelevance = 4;
	    _params.cls_deletion.min_num_lits = (int) (_params.cls_deletion.min_num_lits* 0.8);
	    if (_params.cls_deletion.min_num_lits < 10)
		_params.cls_deletion.min_num_lits = 10;
	    _params.cls_deletion.max_conf_cls_size = (int) (_params.cls_deletion.max_conf_cls_size*0.8);
	    if (_params.cls_deletion.max_conf_cls_size < 50 )
		_params.cls_deletion.max_conf_cls_size = 50;
	    DBG1(
		cout << "Forced to be more aggressive in clause deletion. " << endl;
		cout <<"MaxUnrel: " << _params.cls_deletion.max_unrelevance 
		<< "  MinLenDel: " << _params.cls_deletion.min_num_lits
		<< "  MaxLenCL : " << _params.cls_deletion.max_conf_cls_size << endl;
		);
	}
    }
    unsigned original_del_cls = num_deleted_clauses();
//    int original_del_lits = num_deleted_literals();
    DBG2 (dump());
    for (vector<CClause>::iterator itr1 = clauses().begin(); 
	 itr1 != clauses().end(); ++itr1) {
	CClause & cl = * itr1;
	if (cl.status()!=CONFLICT_CL || cl.num_lits() < _params.cls_deletion.min_num_lits ) continue;
	int val_0_lits = 0, val_1_lits = 0, unknown_lits = 0;
	for (unsigned i=0; i< cl.num_lits(); ++i) {
	    int lit_value = literal_value (cl.literal(i));
	    if (lit_value == 0 ) 
		++val_0_lits;
	    else if (lit_value == 1) 
		++val_1_lits;
	    else 
		++unknown_lits;
	    if (unknown_lits + val_1_lits > (int)_params.cls_deletion.max_unrelevance) {
		mark_clause_deleted(cl);
		DBG1(cout << "Deleting Unrelevant clause: " << cl << endl;);
		break;
	    }
	    if (cl.num_lits() > _params.cls_deletion.max_conf_cls_size && 
		(unknown_lits+val_1_lits > 1) ) { //to make sure it's not generating an implication
				//and it's not an antecedent for other var assignment
		mark_clause_deleted(cl);
		DBG1 (cout << "Deleting Large clause: " << cl << endl;);\
		break;
	    }
	}
    }
    if (original_del_cls == num_deleted_clauses()) return;
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
    // update_var_score(); commented out by sang
    DBG1(cout << "Deleting " << num_deleted_clauses() - original_del_cls << " Clause ";
	 cout << " and " << num_deleted_literals() - original_del_lits << " Literals " << endl;);
    DBG2 (dump());
}
*/

// new version
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
    //for (vector<CClause>::iterator itr1=clauses().begin(); itr1 != clauses().end()-1; ++itr1) {
	for (int k = _stats.num_original_clause; k < clauses().size(); k++)	{ // changed by sang
	//CClause & cl = * itr1;
	CClause & cl = clause(k);
	//assert(cl.status()!=ORIGINAL_CL);
	//if(cl.status()==ORIGINAL_CL)
	//	continue;
    //bool cls_sat_at_dl_0=false;	
	if(cl.status()!=DELETED_CL){ 
	    /*for (int i=0, sz=cl.num_lits(); i<sz; ++i){
	        if (literal_value(cl.literal(i)) == 1 && variable(cl.literal(i).var_index()).dlevel()==0) {
        	    cls_sat_at_dl_0=true;
		    break;
	        }
	    }*/
	    //if(cls_sat_at_dl_0 ){
		if (cl.counter_one()) {	// changed by sang
		//if(cl.status()==ORIGINAL_CL || cl.status()==CONFLICT_CL){
		if(cl.status()==CONFLICT_CL){	// changed by sang, ORIGINAL clauses should not be deleted
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

    /*for (unsigned i=1; i<variables().size(); ++i) 
	{
		if(variable(i).dlevel()!=0)
		{
			variable(i).score(0) = variable(i).lits_count(0);
			variable(i).score(1) = variable(i).lits_count(1);
		    if(variable(i).lits_count(0)==0 && (variable(i).value()==UNKNOWN) )
			{
				queue_implication(2*i+1,NULL_CLAUSE,0);
			}
 			else if(variable(i).lits_count(1)==0 && (variable(i).value()==UNKNOWN) )
			{
				queue_implication(2*i,NULL_CLAUSE,0);
			}
		}
		else
		{
			variable(i).score(0)=variable(i).score(1)=0;	
		}
    }*/
    // update_var_score(); commented out by sang
    DBG1(cout << "Deleting " << num_deleted_clauses() - original_del_cls << " Clause ";
	 cout << " and " << num_deleted_literals() - original_del_lits << " Literals " << endl;);
    DBG2 (dump());
}
//============================================================================================


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

	assert (var.value() != UNKNOWN);	// commented out by sang
	//int orig_score = var.score();
	++ var.score(var_sign);
	}
/*
	int new_score = var.score();
	if (orig_score == new_score) 
	    continue;
	int pos = var.var_score_pos();
	int orig_pos = pos;

	assert (_ordered_vars[pos].first == & var);
	assert (_ordered_vars[pos].second == orig_score);

	//next we bubble up the position, because the score can 
	//increase by at most 1, this is valid.
/* this is a linear search */
//  	while (pos > 0) {
//  	    if (_ordered_vars[pos-1].second >= new_score)
//  		break;
//  	    else {
//  		_ordered_vars[pos].second = _ordered_vars[pos-1].second;
//  		_ordered_vars[pos].first = _ordered_vars[pos-1].first;
//  		_ordered_vars[pos].first->set_var_score_pos(pos);
//  		--pos;
//  	    }
//  	}
//  	_stats.total_bubble_move += orig_pos - pos;
//  	_ordered_vars[pos].first = & var;
//  	_ordered_vars[pos].second = new_score;
//  	_ordered_vars[pos].first->set_var_score_pos(pos);
/* this is a binary search */
/*	int bubble_step = _params.decision.bubble_init_step;
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
*/
}
void CSolver::decay_variable_score(void) 
{
    unsigned int i, sz;
    for(i=1, sz = variables().size(); i<sz; ++i) {
	CVariable & var = variable(i);
	//var.score(0) = var.score(0)/2;
	//var.score(1) = var.score(1)/2;
	var.score(0) = var.score(0) >> 1;
	var.score(1) = var.score(1) >> 1;
    }
    for (i=0, sz = _ordered_vars.size(); i<sz; ++i) {
	_ordered_vars[i].second = _ordered_vars[i].first->score();
/*  	assert (i==0 || _ordered_vars[i].second <= _ordered_vars[i-1].second );
	assert ( _ordered_vars[i].first->value() != UNKNOWN || 
	!_ordered_vars[i].first->is_branchable() || 
	_max_score_pos <= i); */
    }
}
bool CSolver::decide_next_branch(void)
{
	//cout << "in decide_next_branch(void)" << endl;
	//check_integrity();
    // CHECK(verify_integrity());
    if (!_implication_queue.empty()) {
	//some hook function did a decision, so skip my own decision making.
	//if the front of implication queue is 0, that means it's finished
	//because var index start from 1, so 2 *vid + sign won't be 0. 
	//else it's a valid decision.
	return (_implication_queue.front().lit != 0);
    }
    if (_outside_constraint_hook != NULL)
	_outside_constraint_hook(this);
    if (!_implication_queue.empty())
		return (_implication_queue.front().lit != 0);
// formula-caching, added by sang -- begin
	unsigned hash_index = 0;
	//unsigned secondary_index = 0;
#ifdef APPROXIMATE_HASHING
	unsigned long secondary_index = 0;
#endif
#ifndef BIG_NUM
	long double satprob = -3;	// double -> long double
#endif
	int gid = _branch_infor_stack[dlevel()]->gid;
	assert (gid >= 0);
	//cout << "decision " << _stats.num_decisions << " at level " << dlevel() << endl;
	//dump_assignment_stack();
	vector <int> & assignments = * _assignment_stack[dlevel()];
	if (!assignments.empty())	// for changed components, it is empty!
	{
		vector<int> & var_set = * _component_infor_stack[gid]->var_set;
		// for each implication at this level, check if it is cross-component (not belonging to current component)
		for (vector<int>::iterator vitr = assignments.begin(); vitr != assignments.end(); vitr++)
		{
			if (binary_search(var_set.begin(), var_set.end(), (* vitr) >> 1))
			{
				++_num_implication_stack[dlevel()];		// not a cross-component-implication
				variable((* vitr) >> 1).cross_flag = false;
			}
			else
			{
				//cout << "before removing cross implications: " << endl;
				//for (int j = 0; j < assignments.size(); j++)
				//	cout << ((assignments[j] & 0x1) ? "-" : "" ) << (assignments[j] >> 1) << " ";
				//cout << endl;
				//cout << "cross_enabled = " << cross_enabled << endl;
				++_stats.num_cross_implications;
				if (this->cross_enabled)
				{
					variable((* vitr) >> 1).cross_flag = true;
					// put the cross-component-implication in the list for later check
					cross_implication_list.push_back(new cross_implication);
					cross_implication_list.back()->vid = ((* vitr) >> 1);
					cross_implication_list.back()->level = dlevel();
					cross_implication_list.back()->marked = false;
				}
				else
				{
					//cout << "after removing cross implication " << ((* vitr) & 0x1 ? "-" : "") 
					//	<< ((* vitr) >> 1) << ": " << endl;
					unset_var_value((* vitr) >> 1);
					assignments.erase(vitr);
					--vitr;					
					//for (int j = 0; j < assignments.size(); j++)
					//	cout << ((assignments[j] & 0x1) ? "-" : "" ) << (assignments[j] >> 1) << " ";
					//cout << endl;
				}
			}	// end if
		}	// end for
	}	// end if
	//if (dlevel() >= 7 
	//	&& variable((*_assignment_stack[4])[0] >> 1).tried_both() && _assignment_stack[4]->size() == 2
	//	&& _assignment_stack[5]->size() == 2 && _assignment_stack[7]->size() == 3)
	//if (_stats.num_active_added_conf_clauses >= 8000)
	//	cout << "decision " << _stats.num_decisions << " at level " << dlevel() << endl;
	//if (_stats.num_decisions >=   270607) //&& variable((*_assignment_stack[10])[0] >> 1).tried_both())
	//	flag = true;
	/*if (flag)	// for debug
	{
		cout << "decision " << _stats.num_decisions << " at level " << dlevel() 
			 << ", in decide_next_branch, branch_component = " << gid << endl;
		cout << "component " << gid << " is " << (_component_infor_stack[gid]->changed? "changed":"unchanged") << endl;
		dump_assignment_stack();
		dump_branch_infor_stack();
		dump_branchable_component_stack();
	}*/
	int new_components = 0;
	if (_stats.num_unsatisfied_clause > 0)	// if residual formula is not SAT, do component detection
		new_components = extract_new_components(_components, dlevel() + 1, gid);
	if (_component_infor_stack[gid]->changed)	// check if the current branching component has been changed
	{
		_branch_infor_stack[dlevel()]->num_children_of_changed += new_components;	// = -> +=
		// adjust level of children of changed component
		for (int i = 0; i < new_components; ++i)
			_component_infor_stack[_component_infor_stack.size() - 1 - i]->level--;
		_branch_infor_stack[dlevel()]->changed_components.push_back(gid);
	}
	else
		_branch_infor_stack[dlevel()]->num_new_children = new_components;
	/*if (flag)
	{
		cout << "at level " << dlevel() << ", in decide_next_branch, after extract components:" << endl;
		//dump_assignment_stack();
		cout << "new components: " << new_components << endl;
		dump_cross_implications();
		if (new_components > 0)
		{
			//dump_assignment_stack();
			//dump_branch_infor_stack();
			dump_components(_components);
			dump_cross_implications();
		}
	}*/
	int backlevel = dlevel();
	int backup = new_components;	// save new_components

	if (new_components == 0) // no component left or (one component left with one clause)
	{
#ifdef BIG_NUM
		satprob.zero_flag = false;
		mpz_set_ui(satprob.numerator, 1);
		satprob.denominator = 0;
		backlevel = percolate_up(satprob, gid);
#else
		backlevel = percolate_up(1, gid);
#endif	
		//if (flag)
		//	cout << "percolate_up(1, " << gid << ")" << " to level " << backlevel << endl;
	}
	else 
	{
		_stats.num_total_components += new_components;
		//if (dlevel() < LEVEL_MAX)
		//	level_total_components[dlevel()] += new_components;
		//else
		//	level_total_components[LEVEL_MAX] += new_components;
			
		if (new_components > 1)
		{
			//if (dlevel() < LEVEL_MAX)
			//	level_new_components[dlevel()] += new_components - 1;
			//else
			//	level_new_components[LEVEL_MAX] += new_components - 1;
			_stats.num_split_components++;
			if (_stats.first_split_level > dlevel())
				_stats.first_split_level = dlevel();
		}
		else
			_stats.num_not_split_components++;
		//int backup = new_components;	// save new_components
		// move itr to point to the first component created by last branch
		Components::iterator itr = _components.end();
		int pos = _component_infor_stack.size() - new_components - 1;
		for (; new_components > 0; --itr, --new_components);
		new_components = backup;	// restore new_components
		//if (flag)
		//	cout << "processing each new components:" << endl;
		for (; new_components > 0; ++itr, --new_components)
		{
			//if (flag)
			//	cout << "processing component " << pos + 1 << endl;
			//	check each new component to see if its value is trival, if so, percolate up
			if (_component_infor_stack[++pos]->num_clause == 1)
			{				
				_stats.num_trivial_components++;
				//if (dlevel() < LEVEL_MAX)
				//	level_trivial[dlevel()]++;
				//else
				//	level_trivial[LEVEL_MAX]++;
				--backup;	// this component not pushed into branchable_component_stack
				//if (flag)
				//	cout << "component " << pos << " has only one clause" << endl;
#ifdef BIG_NUM
				//int size = (*itr)->comp_val_pair->f[0]->size();
				int size = _component_infor_stack[pos]->var_set->size();
				satprob.zero_flag = false;
				satprob.denominator = size;
				mpz_set_ui(satprob.numerator, (1 << size) - 1);	// (2^n - 1)/(2^n)
#else	
				satprob = 1;
				//vector<int> & literals = *(*itr)->comp_val_pair->f[0];
				vector<int> & literals = (*itr)->comp_val_pair->f;
				for(int i = literals.size() - 2; i >= 0; --i)	// skip the last element 0, a terminating symbol
				{
					if (!variable(literals[i] >> 1).internal)	// no need to handle internal nodes that weight 1
					if (literals[i] & 0x1)
						satprob = satprob * variable(literals[i] >> 1).pos_weight; // negtive literal
					else
						satprob = satprob * variable(literals[i] >> 1).neg_weight; // positive literal
				}
				if (satprob > 0.9999999999)	// that means sat_prob == 1, all vars have weight 1, internal node
					satprob = 0;
				// there is only one new component, which is _component_infor_stack.back()
				//int sz = (*itr)->comp_val_pair->f[0]->size();
				/*int sz = _component_infor_stack[pos]->var_set->size();
				for(int i =0; i < sz; ++i)
				satprob = satprob * 0.5;*/
				satprob = 1 - satprob;	// satprob = 1 - (1/(2^sz))
#endif
				//if (flag)
				//	cout << "percolate up (" << satprob << ", " << gid << ")" << endl;
				backlevel = percolate_up(satprob, gid);	// percolate up this SAT value
				//formula & f = (* itr)->comp_val_pair->f;
				//while (!f.empty())
				//{
				//	delete(f.back());
				//	f.pop_back();
				//}
#ifdef BIG_NUM
#endif
				//cout << "before removing component " << pos << endl;
				//dump_components(_components);
				delete _component_infor_stack[pos]->var_set;
				_component_infor_stack[pos]->active = false; // this component already sat, set not active
				delete (*itr)->comp_val_pair;
				delete (*itr);
				_components.erase(itr, ++itr);
				--itr;	// restore itr;
				//cout << "after removing component " << pos << endl;
				//dump_components(_components);
			}	//	end if
			//	check each new component to see if it is already cached, if so, percolate up
#ifdef APPROXIMATE_HASHING
			else if (hashtable->in_hash_table((*itr)->comp_val_pair->f, hash_index, secondary_index, satprob))
#else
			else if (hashtable->in_hash_table((*itr)->comp_val_pair->f, hash_index, satprob))
#endif // APPROXIMATE_HASHING
			{
				//if (dlevel() < LEVEL_MAX)
				//	level_hits[dlevel()]++;
				//else
				//	level_hits[LEVEL_MAX]++;
				// the component is already in cache, remove it from the component list
				//_component_infor_stack[(*itr)->index]->active = false;
				--backup;	// this component not pushed into branchable_component_stack
				_component_infor_stack[pos]->active = false;
				// delete the formula because it is already in cache
				//formula & f = (* itr)->comp_val_pair->f;
				//while (!f.empty())
				//{
				//	delete(f.back());
				//	f.pop_back();
				//}
				delete (*itr)->comp_val_pair;
				delete _component_infor_stack[pos]->var_set;
				delete (*itr);
				_components.erase(itr, ++itr);	// remove it because it's in cache
				--itr;	// restore itr;
				//sat_prob = cached_value;		// problem here!
				//if (flag)
				//	cout << "_component_infor_stack[" << pos << "] already in cache[" 
				//		 << hash_index << "], with value = " << satprob 
				//		 << ", component " << pos << " set to inactive and removed from _components" << endl;
#ifdef BIG_NUM
				int temp = satprob.zero_flag;
				backlevel = percolate_up(satprob, gid);
				satprob.zero_flag = temp;
#else
				backlevel = percolate_up(satprob, gid);
#endif
				// gid is the branch component, not the current component that is cached already
				//dump_components(_components);
#ifdef BIG_NUM
				if (satprob.zero_flag)
#else
				if (satprob < -0.5)
#endif
					// this component is unsat, according to the cached value -1
					if (backlevel <= 0)
					{
					//	//cout << "there is a UNSAT component at level " << backlevel 
					//	//	 << ", that means the original formula cannot be satisfied" << endl;
						return false;
					}
					else
					{
						//num_conflicts++;
						// should flip the decision var at upperlevel, and flip the deepest unflipped decision var
						// need to backtrack to a level >= backlevel && its decision var not tried both
						if (_component_infor_stack[_branch_infor_stack[backlevel]->gid]->changed)
						{
							//if (flag)
							//	cout << "children of changed component being hit" << endl;
							--backlevel;	// can't backtrack to a dummy level, so --backlevel
						}
						int last_branch;
						int vid;
						if (backlevel > 0)
						{
							last_branch = (*_assignment_stack[backlevel])[0];
							vid = last_branch >> 1;
							bool tried_both = variable(vid).tried_both();
					
							while (tried_both && (--backlevel > 0))
							{
								last_branch = (*_assignment_stack[backlevel])[0];
								vid = last_branch >> 1;
								tried_both = variable(vid).tried_both();
							}	// end while
						}
						if ((backlevel <= 0))// variables on level 0 can never be flipped
						{	
						// backtracked to level 0, still no decision variable can be flipped, the whole search space done
							//if (flag)
							//	cout << "variables on level 0 can't be flipped, whole search space done" << endl;
							return false;	// whole search space done
						}	// end if
						//dump_assignment_stack();
						//if (flag)
						//	cout << "UNSAT components " << gid << ", backtrack to level " << backlevel << endl
						//		 << "last_branch " << ((last_branch & 0x1) ? '-' : '+') << vid
						//		 << " flipped to " << (((last_branch ^ 0x1) & 0x1) ? '-' : '+') << vid << endl;
						int branch_compoent = _branch_infor_stack[backlevel]->gid;
						back_track(backlevel);		//	backtrack to the proper level
						// note: dlevel()==backlevel-1 now! so need to increment dlevel() in the following
		
						while (!_implication_queue.empty())
							_implication_queue.pop();
						queue_implication(last_branch ^ 0x1, NULL_CLAUSE, ++dlevel()); // flip last_branch
						variable(vid).set_tried_both(true);
						_branch_infor_stack[dlevel()]->gid = branch_compoent;
						return true;
					}	// end else (backlevel <= _uni_phased_size)
			} // end if in_hash_table
			else 
			{
				// store the hash index of the current component, to avoid future re-computation
				_component_infor_stack[pos]->hash_index = hash_index;
#ifdef APPROXIMATE_HASHING
				_component_infor_stack[pos]->secondary_index = secondary_index;
				// remove all clauses of the formula!
				//formula & f = (* itr)->comp_val_pair->f;
				//while (!f.empty())
				//{
				//	delete(f.back());
				//	f.pop_back();
				//}
				//formula temp;	// temp is empty
				//f.swap(temp);	// re-claim all memory of f, temp will be re-claimed when exiting this function
				//cout << "after swap: f.capacity() = " << f.capacity() << ", f.size() = " << f.size() << endl;
#endif
				_branchable_component_stack.push_back(pos);	// add the new component to branchable stack
			}
		}	// end for each component
	}	//	end else if (new_components == 0)
// formula-caching, added by sang -- end
#ifdef BIG_NUM
	//cout << (&(*satprob.numerator)) << " cleared in decide_next_branch, decision = " << _stats.num_decisions << endl;
	//mpz_clear(satprob.numerator);
#endif

	if (adjust_enabled && backup > 1)
	// means >1 new components pushed into _branchable_component_stack, need to adjust branching order
	{	// selection sort
		//cout << "at decision " << _stats.num_decisions << ", start scanning _branchable_component_stack: ";
		//cout << "new_components pushed in = " << backup << endl;
		//dump_branchable_component_stack();
		int comp_i;
		int comp_j;
		Component_information * ptr;
		for(int i = _branchable_component_stack.size() - backup; i < _branchable_component_stack.size() - 1; ++i)
			for (int j = i + 1; j < _branchable_component_stack.size(); ++j)
			{
				comp_i = _branchable_component_stack[i];
				comp_j = _branchable_component_stack[j];

				if (_component_infor_stack[comp_i]->var_set->size()		// branch small(var_set) component first
					< _component_infor_stack[comp_j]->var_set->size())
				//if (_component_infor_stack[comp_i]->num_clause < _component_infor_stack[comp_j]->num_clause)
				//if (_component_infor_stack[comp_i]->num_clause/_component_infor_stack[comp_i]->var_set->size()
				//	> _component_infor_stack[comp_j]->num_clause/_component_infor_stack[comp_j]->var_set->size())
				{
					//cout << "component " << comp_i << "(" << _component_infor_stack[comp_i]->num_clause 
					//	 << "/" << _component_infor_stack[comp_i]->var_set->size()
					//	 << ") and component " << comp_j 
					//	 << "(" << _component_infor_stack[comp_j]->num_clause 
					//	 << "/" << _component_infor_stack[comp_j]->var_set->size()
					//	 << ") need to be swapped "<< endl;

					//_branchable_component_stack[i] = comp_j;
					//_branchable_component_stack[j] = comp_i;

					++_stats.num_adjusted_components;
					(*_component_infor_stack[comp_i]->comp_itr)->index = comp_j;
					(*_component_infor_stack[comp_j]->comp_itr)->index = comp_i;

					ptr = _component_infor_stack[comp_i];
					_component_infor_stack[comp_i] = _component_infor_stack[comp_j];
					_component_infor_stack[comp_j] = ptr;
				}
			}
		//cout << "after processing _branchable_component_stack: " << endl;
		//dump_branchable_component_stack();
	}

	int s_var = 0;
	int branch_component = -1;

	if (!_branchable_component_stack.empty())
	{
		// an active component found, choose its largest-degree-var as decision variable
		branch_component = _branchable_component_stack.back();
		_branchable_component_stack.pop_back();		// pop the branch component from stack
		Component_information * comp_infor = _component_infor_stack[branch_component];
		
		if (cross_enabled)
		{
			vector<int> & var_set = * comp_infor->var_set;
			//if (flag)
			//{
			//	dump_cross_implications();
			//	cout << "at level " << dlevel() << ", component " << branch_component << "'s var_set: ";
			//	for (int i = 0; i < var_set.size(); ++i)
			//		cout << var_set[i] << ", ";
			//	cout << endl;
			//}
		for (int i = cross_implication_list.size() - 1; i >= 0; --i) // check if this component has been changed
		{
			//if (flag)
			//	cout << "looking for " << cross_implication_list[i]->vid << " in component " <<  branch_component << endl;
			if (comp_infor->level > cross_implication_list[i]->level)	// bug removed: >= -> >
			{
				break;		// cross_component_implication cannot change components created below it
			}
			if (cross_implication_list[i]->marked)
			{
				continue;
			}
			if (binary_search(var_set.begin(), var_set.end(), cross_implication_list[i]->vid))
			{
				comp_infor->changed = true;

				if (!variable(cross_implication_list[i]->vid >> 1).internal)
				if (cross_implication_list[i]->vid & 0x1)
					comp_infor->cross_implication_weight *= variable(cross_implication_list[i]->vid >> 1).neg_weight;
				else
					comp_infor->cross_implication_weight *= variable(cross_implication_list[i]->vid >> 1).pos_weight;

				++comp_infor->num_cross_implications;	// need to count how many cross-implications refering to this component!
				cross_implication_list[i]->marked = true;
				//break;	// break means not to count
			}
			//else if (flag)
			//	cout << "not found" << endl;
		}
		if (_component_infor_stack[branch_component]->changed)
		{
			++_stats.num_changed_components;
			//if (flag)
			//	cout << "at level " << dlevel() << " with " << _stats.num_decisions << " decisions, "
			//		 << "component " << branch_component << " changed by "
			//		 << _component_infor_stack[branch_component]->num_cross_implications 
			//		 << " cross_implications" << endl;
			if (_assignment_stack[dlevel()]->size() > 0)	// increment dlevel() only when last component unchanged
			{
				//if (flag)
				//	cout << "_assignment_stack[" << dlevel() << "]->size = " 
				//		 << _assignment_stack[dlevel()]->size() << ", ++dlevel()" << endl;
				++dlevel();	// bug! if tow components changed in a row, could cause problems
			}
			_branch_infor_stack[dlevel()]->gid = branch_component;
			// need to put this changed component into its parent's branched_child_list
			_component_infor_stack[comp_infor->ancestor_index]->branched_child_list.push_back(branch_component);
			return true;
		}
		}	// end if (cross_enabled)
		
		s_var = comp_infor->largest_svar;	// choose the largest_degree var to branch
		_component_infor_stack[comp_infor->ancestor_index]->branched_child_list.push_back(branch_component);
		//if (flag)
		//	cout << "at level " << dlevel()+1 << ", decision made, s_var: " << s_var << " = "
		//		 << ((s_var & 0x1) ? '-' : '+') << (s_var >>1) 
		//		 << ", which belongs to component " << branch_component << endl;
	}
	if (s_var == 0)
	{
		assert(_stats.num_unsatisfied_clause == 0);		// must be no UNSAT clauses left
		//if (flag)
		//	cout << "entered branch_component == -1, "
		//	 << "no active component at this level" << endl;
		// need to backtrack and find a varialbe not_tried_both and flip it
		//++num_sat;
		++num_no_components;
		if (dlevel() == 0)
			return false;		// can't do anything in this case
		backlevel = dlevel();
		if (_component_infor_stack[_branch_infor_stack[backlevel]->gid]->changed)
			--backlevel;	// can't backtrack to a dummy level, so --backlevel
    	int last_branch = (*_assignment_stack[backlevel])[0];
		int vid = last_branch >> 1;
		bool tried_both = variable(vid).tried_both();

		while (tried_both && (--backlevel > 0))
		{
			last_branch = (*_assignment_stack[backlevel])[0];
			vid = last_branch >> 1;
			tried_both = variable(vid).tried_both();
		}	// end while
		if ((backlevel == 0)) // variables on level 0 can never be flipped
		//|| ((_stats.num_solutions == 0) && (backlevel <= _uni_phased_size)))	
		{	
		// backtracked to level 0, still no decision variable can be flipped, the whole search space done
			//if (flag)
			//	cout << "variables on level 0 can't be flipped, whole search space done" << endl;
		//	else 
		//	{
		//		cout << "Since no solution found so far, no need to flip uni_phased variables"
		//		<< endl << "Halted, won't backtrack to level " << backlevel << endl;
		//	}
			return false;	// whole search space done
		}	// end if
		back_track(backlevel);		//	backtrack to the proper level
		//if (flag)
		//	cout << "back_track to level " << backlevel << endl;
		// note: dlevel()==backlevel-1 now! so need to increment dlevel() in the following
		while (!_implication_queue.empty())
			_implication_queue.pop();
		queue_implication(last_branch ^ 0x1, NULL_CLAUSE, ++dlevel()); // flip last_branch
		variable(vid).set_tried_both(true);
		return true;
	}	// end if (s_var <= 2)
	assert(s_var >= 2); // commented out by sang

	if (! _component_infor_stack[_branch_infor_stack[dlevel()]->gid]->changed)
	{
		//if (flag)
		//	cout << "at level " << dlevel() << ", branching component " 
		//		 << _branch_infor_stack[dlevel()]->gid << " unchanged, ++dlevel" << endl;
		++dlevel();		// check if the parent of current component has been changed by cross implications
	}
	//else
	//	_branch_infor_stack[dlevel()]->num_new_children = 0;// clear it, it might not be 0 because of changed comp

	++_stats.num_decisions;	// a real decision made here
	//cout << "num of decisions: " << _stats.num_decisions << endl;
    if (dlevel() > _stats.max_dlevel) 
	_stats.max_dlevel = dlevel();
    DBG0 (cout << "**Decision " << _stats.num_decisions << " at Level " << dlevel() ;
	  cout <<": " << s_var << "\ti.e. " << (s_var&0x1?"-":" ") ;
	  cout <<(s_var>>1)  << endl; );
    _implication_id = 0;
	/*if (variable(s_var>>1).tried_both())
	{
		cout << "num of decisions: " << _stats.num_decisions << endl;
		cout << "variable " << (s_var>>1) << " should not have been tried both when first being set!";
		dump_assignment_stack();
		dump_branchable_component_stack();
		dump_cross_implications();
		dump_branch_infor_stack();
		dump_components(_components);	
		exit(0);
	}*/
    queue_implication(s_var, NULL_CLAUSE, dlevel());
	_branch_infor_stack[dlevel()]->gid = branch_component; // added by sang
	assert(branch_component >= 0);
    return true;
}

int CSolver::preprocess(void) 
{
	touched.resize(num_variables() + 1, 0);
	var_score.resize((num_variables() + 1) << 1, 0);
	score_sum.resize((num_variables() + 1) << 1, 0);
	clause_comp.resize(num_clauses(), 0);
	//print_cls();
    int i, sz;
    assert(dlevel() == 0);
    _stats.num_unsatisfied_clause = num_clauses(); // added by sang
	_stats.num_original_clause = num_clauses(); // added by sang
    //1. detect all the unused variables
    /*vector<int> un_used;
    for (i=1, sz=variables().size(); i<sz; ++i) {
	CVariable & v = variable(i);
	if (v.lits_count(0) == 0 && v.lits_count(1) == 0) {
	    un_used.push_back(i);
	    queue_implication(i+i, NULL_CLAUSE, 0);
	    //int r = deduce();	// commented out by sang
	    //assert (r == NO_CONFLICT);
	}
    }
    if (_params.verbosity>1 && un_used.size() > 0) {
	cout << un_used.size()<< " Variables are defined but not used " << endl;
	if (_params.verbosity > 2) {
	    for (unsigned i=0; i< un_used.size(); ++i)
		cout << un_used[i] << " ";
	    cout << endl;
	}
    }*/
	//	added by sang ---- begin
//#ifdef STATIC_ORDERING
	/*if (!dynamic_heuristic)
		for (int v = num_variables(); v > 0; --v)
		{
			CVariable & var = variable(v);
			int degree[2]; // degree[0]==pos_degree, degree[1]==neg_degree
			degree[0] = var.lit_clause(0).size();
			degree[1] = var.lit_clause(1).size();

			if (degree[0] < degree[1])
			{
				var.largest_degree = degree[1];
				var.largest_svar = (v << 1) + 1;
			}
			else 
			{
				var.largest_degree = degree[0];
				var.largest_svar = v << 1;
			}
		//cout << v << ": largest_degree = " << var.largest_degree << ", argest_svar = " << var.largest_svar << endl;
		}	// end for
	*/
//#endif // end STATTIC_ORDERING
	//	added by sang ---- end

	// set _clause_component_stack[0] to an all-0 vector
	_branch_infor_stack[0]->gid = 0;
	//_clause_component_stack[0]->resize(_stats.num_original_clause);
	//vector <unsigned> & cl = * _clause_component_stack[0];
	//for (int i = 0; i < _stats.num_original_clause; ++i)
	//	cl[i] = 0;
	_component_infor_stack.push_back(new Component_information); // _component_infor_stack[0]
	 //_component_infor_stack[0]->var_set = new set<int>;
	_component_infor_stack[0]->var_set = new vector<int>;
	vector<int> & var_set = * _component_infor_stack[0]->var_set;
	int var_size = num_variables();
	var_set.resize(var_size);
	for (int v = 1; v <= var_size; ++v)
		var_set[v-1] = v;
	//	var_set.insert(v);
	
    //3. Unit clauses
    for (i=0, sz=clauses().size(); i<sz; ++i) 
	{
	if (clause(i).status()!=DELETED_CL)
	    if (clause(i).num_lits() == 1)
		{ //unit clause
			// cout << " unit clause: " << endl; // added by sang
			// clause(i).dump(cout);
		if (variable(clause(i).literal(0).var_index()).value() == UNKNOWN)
		{
		    // _stats.num_unsatisfied_clause--; // added by sang, removed
			//cout << "initial num_unsatisfied_clause: " << _stats.num_unsatisfied_clause << endl;
		    queue_implication(clause(i).literal(0).s_var(), i, 0);
			//++_stats.num_unit_clause;	// added by sang
		}	// end if
	    }	// end if
    }	// end for
    if(deduce()==CONFLICT) {
#ifdef VERIFY_ON
	for (i=1; i< variables().size(); ++i) {
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
		}	// end if
	    }	// end for
	}		// end if
	verify_out << "CONF: " << clause(_conflicts[0]).id() << " ==";
	for (i=0; i< clause(_conflicts[0]).num_lits(); ++i) {
	    int svar = clause(_conflicts[0]).literal(i).s_var();
	    verify_out << " " << svar;
	}
#endif
	//cout << "UNSAT! " << endl;
	//exit(0);
	return CONFLICT;
    }
	_stats.num_unit_clause = _assignment_stack[0]->size(); // - un_used.size();
	if (!quiet)
		cout << "Number of unit clauses\t\t\t" << _stats.num_unit_clause << endl;
/*
	if (_stats.num_unsatisfied_clause == 0)
	{
#ifdef BIG_NUM
		mpz_set_ui(_component_infor_stack[0]->left_sat_prob.numerator, 1);
		_component_infor_stack[0]->left_sat_prob.zero_flag = false;
#else
		_component_infor_stack[0]->left_sat_prob = 1;
#endif
		return SAT;
	}
	if (_stats.num_unsatisfied_clause == 1)
#ifdef BIG_NUM
		for (int i = clauses().size()-1; i >= 0; --i)
			if (clause(i).counter_one() == 0)
			{
				int size = 0;
				for (int j = clause(i).num_lits() - 1; j >= 0; --j)
					if (variable(clause(i).literal(j).var_index()).value() == UNKNOWN)
						size++;
				_component_infor_stack[0]->left_sat_prob.denominator = size;
				unsigned long exp_size = 1 << size;
				mpz_set_ui(_component_infor_stack[0]->left_sat_prob.numerator, --exp_size);// (2^n - 1)/(2^n)
			}
#else
	{
		_component_infor_stack[0]->left_sat_prob = 1;
		for (int i = clauses().size()-1; i >= 0; --i)
			if (clause(i).counter_one() == 0)
			{
				for (int j = clause(i).num_lits() - 1; j >= 0; --j)
					if (variable(clause(i).literal(j).var_index()).value() == UNKNOWN)
						_component_infor_stack[0]->left_sat_prob /= 2;
				_component_infor_stack[0]->left_sat_prob = 1 - _component_infor_stack[0]->left_sat_prob;
				//cout << "Final Probability = " << _component_infor_stack[0]->left_sat_prob << endl;
				return SAT;
			}
	}
	_component_infor_stack[0]->sat_probability = -3;
	_component_infor_stack[0]->left_sat_prob = -3;
	_component_infor_stack[0]->right_sat_prob = -3;
#endif
*/
	_component_infor_stack[0]->sat_probability = -3;
	_component_infor_stack[0]->left_sat_prob = -3;
	_component_infor_stack[0]->right_sat_prob = -3;
	_component_infor_stack[0]->active = true;
	_component_infor_stack[0]->level = 0;	// used in far_back_track
	_component_infor_stack[0]->left_branch_done = false;
	_component_infor_stack[0]->changed = false;
	_component_infor_stack[0]->num_cached_children = 0;

	//cout << "None uni_phased VAR assigned" << endl;
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
    int cid = add_clause_with_gid(lits, n_lits, gid, true);
    if (cid >= 0)
	clause(cid).set_status(ORIGINAL_CL);
	clause(cid).activity()=0; // just to initialize, from zchaff2004
    return cid;
}

ClauseIdx CSolver::add_clause_with_gid (int * lits, int n_lits, int gid, bool original)
{	// function signature changed by sang, original added
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
    ClauseIdx cid = add_clause(lits, n_lits, gflag, original); // changed by sang
    if (cid < 0) {
	_stats.is_mem_out = true;
	_stats.outcome = MEM_OUT;
	return cid;
    }
    return cid;
}
ClauseIdx CSolver::add_conflict_clause (int * lits, int n_lits, int gflag)
{
	ClauseIdx cid;

	// if (_stats.num_active_added_conf_clauses % 100 == 0)
	//	cout << "Active added conflict clauses: " 

	//a. clause deletion
	//if (_stats.num_active_added_conf_clauses > _stats.next_cls_deletion 
	//	&& _params.cls_deletion.enable) {
	//_stats.next_cls_deletion = _stats.num_active_added_conf_clauses + _params.cls_deletion.interval;
	//delete_unrelevant_clauses(); 
    //}
	//num_conflicts++;
	if (_stats.num_active_added_conf_clauses >= bound)
	{
		if (!quiet)
			cout << "Added conflict clauses: " << _stats.num_active_added_conf_clauses << endl;
		bound = bound + 1000;
		//if (_stats.num_active_added_conf_clauses > _stats.next_cls_deletion)// if there are too many learned clauses, remove some
		//{
		//	_stats.next_cls_deletion += 5000;	// check and delete clauses every 5000 clauses added
		//	delete_unrelevant_clauses(); 
		//}
		//dump_components(_components);
		//char c = getchar();
	}
#ifdef STOP_ADDED_CLAUSE
	//cout << "_stats.num_active_added_conf_clauses: " 
	//	 << _stats.num_active_added_conf_clauses << endl;
	if (_stats.num_active_added_conf_clauses >= ADDED_CL_MAX -1) // added by sang
	{
		cout << "too many added clauses, removed clasues with gid = " << _gid << endl;
#ifdef ENABLE_DEL_CLAUSE
		//cout << "clauses with gid = " << _gid << " removed! " << endl;
		//cout << "before removing, mem-usage: " << mem_usage();
		//cout << ", lit_pool_utilization: " << lit_pool_utilization() << endl;
		delete_clause_group(_gid);	// too many clauses, removed clasues with gid 1
		//cout << "after removing, before compact, mem-usage: " << mem_usage();
		//cout << ", lit_pool_utilization: " << lit_pool_utilization() << endl;
		compact_lit_pool();
		//cout << "after removing, after compact, mem-usage: " << mem_usage();
		//cout << ", lit_pool_utilization: " << lit_pool_utilization() << endl;
		_gid = 0; // reset_gid to 0!
		_stats.num_active_added_conf_clauses = GID_1;
#else
		cout << "Stop adding clauses!" << endl;
#endif
		//_stats.num_active_added_conf_clauses = GID_1;
		_gid = alloc_gid(); // need to allocate gid here!
		cid = add_clause_with_gid(lits, n_lits, _gid, false); // set _gid, added by sang
		_stats.num_active_added_conf_clauses++;
	}
	else if (_stats.num_active_added_conf_clauses >= GID_1)
	{
		if (_gid == 0)
		{
			_gid = alloc_gid();
//#ifdef DEBUG_OUT
			cout << "set _gid from 0 to " << _gid << endl;
//#endif
		}
		cid = add_clause_with_gid(lits, n_lits, _gid, false); // set _gid, added by sang
		_stats.num_active_added_conf_clauses++;
//#ifdef DEBUG_OUT
		if (cid % 100 == 0)
		{
			cout << "clause " << cid << " added, set gid = " << _gid << endl;
			//cout << "mem-usage: " << mem_usage() << endl;
			//cout << "lit_pool_utilization: " << lit_pool_utilization() << endl;
//#endif
		}
	}
	else 
	{
		cid = add_clause( lits, n_lits, gflag);
		_stats.num_active_added_conf_clauses++;
	}
#else // not defined STOP_ADDE_DCLAUSE
	cid = add_clause( lits, n_lits, gflag);
	_stats.num_active_added_conf_clauses++;
#endif // STOP_ADDED_CLAUSE
    if (cid >= 0) {
	clause(cid).set_status(CONFLICT_CL);
	clause(cid).activity()=0;	// from zchaff2004
    }
    else {
    	cout << "mem_out set true in add_conflict_clause() due to cid < 0" << endl;
	_stats.is_mem_out = true;
	_stats.outcome = MEM_OUT;
    }
    return cid;
}

void CSolver::real_solve(void)
{
/* #ifdef KEEP_LIT_CLAUSES
	for (int i=1; i<= num_variables(); ++i)
	{
		cout << "var " << i << ", " << endl;
		variable(i).dump(cout);		// added by sang
	}
#endif */

    while(1) {
	run_periodic_functions();
	if (decide_next_branch()) 
	{
	    while (deduce()==CONFLICT) 
		{ 
			int blevel = analyze_conflicts();
			if (blevel < 0)
			{
				if (_stats.num_solutions > 0)
					_stats.outcome = SATISFIABLE;
				else
					_stats.outcome = UNSATISFIABLE;
				return;
			}	// end if
	    }	// end while
	}	// end if
	else {
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
	    cout << "memory out in real_solve(), mem-usage: " << mem_usage() << endl; // added by sang
	    return; 
	}	    
    }
}

int CSolver::solve(void)
{
    if (_stats.outcome == UNDETERMINED) {
	init_solve();
	hashtable = new CHashTable(cache_size, max_entry, oldest_entry, clean_limit);	// added by sang
	hashtable->quiet = quiet;	// control output

	//preprocess 
	DBG1(dump_assignment_stack(););

	_stats.outcome = preprocess();
	if(_stats.outcome == CONFLICT) 
	    _stats.outcome = UNSATISFIABLE;
	else if (_stats.outcome == SAT)//the real search
		_stats.outcome = SATISFIABLE;
	else	// NO_CONFLICT
	{
		if (static_heuristic)
			assign_static_score();
		//dump_components(_components);
	    real_solve();
		_stats.finish_cpu_time = get_cpu_time();
	}
    }
    int result = _stats.outcome;
	// _stats.outcome = UNDETERMINED;
	// cout << endl << "Seeking for all solutions......" << endl;
	//dump_assignment_stack();
	//dump_components(_components);
	if (!quiet)
	{
		//cout << "num_no_components\t\t\t" << num_no_components << endl;
		//cout << "num_conflicts\t\t\t\t" << num_conflicts << endl;
		//cout << "num_sat\t\t\t\t\t" << num_sat << endl;

		//cout << "level\t" << "total\t\t" << "new\t\t" << "hits\t\t" << "trivial\t\t" << "conflicts" << endl;
		//for (int i = 0; i < (_stats.max_dlevel < LEVEL_MAX ? _stats.max_dlevel : LEVEL_MAX); i++)
		//	cout << i << '\t' << level_total_components[i] << "\t\t" << level_new_components[i] << "\t\t"
		//		 << level_hits[i] << "\t\t" << level_trivial[i] << "\t\t" << level_conflicts[i] << endl;


		cout << endl << "Learning\t\t\t\tON" << endl;
#ifdef APPROXIMATE_HASHING
		cout << "Approximate hashing\t\t\tON" << endl;
#else
		cout << "Approximate hashing\t\t\tOFF" << endl;
#endif

#ifdef BIG_NUM
		cout << "Multiple precision\t\t\tON" << endl;
#else
		cout << "Multiple precision\t\t\tOFF" << endl;
#endif

		if (static_heuristic)
			cout << "Static ordering\t\t\t\tON" << endl;
		else
			cout << "Static ordering\t\t\t\tOFF" << endl;
		{
			cout << "Dynamic heuristic\t\t\t";
			switch(dynamic_heuristic)
			{
				case DEGREE:		cout << "DEGREE";
									break;
				case DEGREE_SUM:	cout << "DEGREE_SUM";
									break;
				case SUM:			cout << "SUM";
									break;
				case SUM_DEGREE:	cout << "SUM_DEGREE";
									break;
				case VSIDS:			cout << "VSIDS";
									break;
				case VSADS:			cout << "VSADS";
									break;
				case UNIT_PROP:		cout << "UNIT_PROP";
									break;
				case BERKMIN:		cout << "BERKMIN";
									break;
			}
		}
		cout << endl << endl;

		cout << "Cache table size\t\t\t" << hashtable->hashtable_size << endl
		 //<< "Max_entry allowed\t\t\t" << hashtable->max_entry << endl
		 << "Oldest age allowed\t\t\t" << hashtable->oldest << endl
		 << "Max cache entries\t\t\t" << hashtable->clean_limit << endl
		 << "Added cache entries\t\t\t" << hashtable->active_entries << endl
		 << "Removed cache entries\t\t\t" << hashtable->num_removed_entries << endl
		 << "Number of cache hits\t\t\t" << hashtable->num_hit << endl
		 << "Number of pos hits\t\t\t" << hashtable->pos_hit << endl
		 << "Number of neg hits\t\t\t" << hashtable->neg_hit << endl
		 << "Number of collisions\t\t\t" << hashtable->num_collisions << endl;
		 //<< "Number of literals\t\t\t" << hashtable->num_literals << endl
		 //<< "Cache memory usage\t\t\t" << hashtable->mem_usage << endl
		 //<< "Content memory usage\t\t\t" << hashtable->content_usage << endl
		 //<< "Literal memory usage\t\t\t" << hashtable->literal_usage << endl
		 //<< "MAX_DISTANCE\t\t\t\t" << max_distance << endl;
	}
		long double final_prob = 1;
		long double num_solutions;
		unsigned i = 0;

		if (result == UNSATISFIABLE && _stats.num_decisions == 0)
		{
			_stats.sat_prob = 0;
#ifdef BIG_NUM
			mpz_set_ui(_stats.num_solutions, 0);
#else
			_stats.num_solutions = 0;
#endif
		}
		else
		{
			//cout << "SAT!" << endl;
			//exit(0);
#ifdef BIG_NUM
		if (!_component_infor_stack[0]->left_sat_prob.zero_flag)
		{
			int j = 0;
			if (_branch_infor_stack[0]->num_new_children < 1)
			{
				//cout << "_branch_infor_stack[0]->num_new_children < 1, j = " << _stats.num_unit_clause << endl;
				//j = _stats.num_unit_clause;
				j = _assignment_stack[0]->size();
			}
			else if (_stats.num_unit_clause > 0)	// the original formula contains severl components, value already divided in percolate_up
			{
				//cout << "j = 1, _stats.num_unit_clause = " << _stats.num_unit_clause << endl;
				j = 1;			
			}
			
			mpz_mul_2exp(_stats.num_solutions,
					_component_infor_stack[0]->left_sat_prob.numerator,
					num_variables() - _component_infor_stack[0]->left_sat_prob.denominator
					- j);
			//cout << "Number of solutions\t\t\t";
			//mpz_out_str (stdout, 10, _component_infor_stack[0]->left_sat_prob.numerator);
			//cout << endl;
		}
		else if (!_component_infor_stack[1]->sat_probability.zero_flag)
		{
			mpz_mul_2exp(_stats.num_solutions,
					_component_infor_stack[1]->sat_probability.numerator,
					num_variables() - _component_infor_stack[1]->sat_probability.denominator);
			//cout << "Number of solutions\t\t\t";
			//mpz_out_str (stdout, 10, _component_infor_stack[1]->sat_probability.numerator);
			//cout << endl;
		}
		//else cout << "Number of solutions\t\t\t" << 0 << endl;		
		}	// end else
#else
		if (_component_infor_stack[0]->left_sat_prob > 0)
			final_prob = _component_infor_stack[0]->left_sat_prob;
		else 
			final_prob = _component_infor_stack[1]->sat_probability;
		//cout << "final_prob of component 0 = " << final_prob << endl;
		//cout << "_assignment_stack[0]->size() = " << _assignment_stack[0]->size() << endl;
		//dump_assignment_stack();
		//dump_components(_components);

		/*if (_branch_infor_stack[0]->num_new_children < 1)
			//for (int j = _stats.num_unit_clause; j > 0; --j)	
			for (int j = _assignment_stack[0]->size(); j > 0; --j)
				final_prob *= 0.5;
		else 
			//if (_stats.num_unit_clause > 0)	// the original formula contains severl components, value already divided in percolate_up
			if (_assignment_stack[0]->size() > 0)
				final_prob *= 0.5;
		*/

		//cout << "Satisfying probability\t\t\t" << (final_prob < 0 ? 0 : final_prob) << endl;
		//cout << "number of free variables: " << (num_variables() - _assignment_stack[0]->size()) << endl;
		//for	(num_solutions = final_prob; i < num_variables() - _stats.num_unit_clause; ++i)
		if (final_prob < 0)
		{
			final_prob = 0;
			num_solutions = 0;
		}
		else 
			for(num_solutions = final_prob; i < num_variables(); ++i)
				num_solutions = num_solutions + num_solutions;
		//cout << "Number of solutions\t\t\t" << num_solutions << endl;
		_stats.sat_prob = final_prob;
		_stats.num_solutions = num_solutions;
		}	// end else
#endif
	if (!quiet)
	{
		cout <<  endl;
		cout << "Cross component implications\t\t" << (cross_enabled?"ON":"OFF") << endl;
		cout << "Number of cross implications\t\t" << _stats.num_cross_implications << endl;
		cout << "Backtrack factor\t\t\t" << backtrack_factor << endl;
		cout << "Number of far backtrack\t\t\t" << num_far_backtrack << endl;

		cout << endl;
		cout << "Number of total components\t\t" << _stats.num_total_components << endl;
		cout << "Number of split components\t\t" << _stats.num_split_components << endl;
		cout << "Number of non-split components\t\t" << _stats.num_not_split_components << endl;
		cout << "Number of SAT residual formula\t\t" << num_no_components << endl;
		cout << "Number of trivial components\t\t" << _stats.num_trivial_components << endl;
		cout << "Number of changed components\t\t" << _stats.num_changed_components << endl;
		cout << "Number of adjusted components\t\t" << _stats.num_adjusted_components << endl;
		cout << "First component split level\t\t" << _stats.first_split_level << endl;
	}
    return result;
}


void CSolver::far_back_track(int blevel)
{
	/*if (flag)
	{
		cout << "in far_back_track, decision " << _stats.num_decisions << " at level " 
			 << dlevel() << ", blevel = " << blevel << endl;
		dump_assignment_stack();
		dump_branch_infor_stack();
		dump_branchable_component_stack();
		dump_components(_components);
	}*/

	while(!cross_implication_list.empty())	// remove all obsolete cross implications
		if (cross_implication_list.back()->level >= blevel)
		{
			delete cross_implication_list.back();
			cross_implication_list.pop_back();
		}
		else
			break;

	while(!_branchable_component_stack.empty())
	{
		if (_component_infor_stack[_branchable_component_stack.back()]->level >= blevel)
			_branchable_component_stack.pop_back();
		else
			break;
	}

    //for (int i = dlevel(); i >= blevel-1 && i > 0; --i) 
	for (int i = dlevel(); i >= blevel-1; --i) 
	{
		vector<int> & assignments = *_assignment_stack[i];
		Component_information * comp_infor = _component_infor_stack[_branch_infor_stack[i]->gid];

		if (comp_infor->active && comp_infor->level <= blevel)	// only deal with active components
		{
			//_branch_infor_stack.push_back(_branch_infor_stack[i]);	// put it in _branch_infor_stack again
			if (comp_infor->level < blevel && comp_infor->active && i != blevel - 1)
			{
				_branchable_component_stack.push_back(_branch_infor_stack[i]->gid);	// bug fixed!
				//if (flag)
				//	cout << "component " << _branch_infor_stack[i]->gid 
				//		 << " pushed back to _branchable_component_stack" << endl;
			}

#ifndef BIG_NUM
			if (i > 0 && variable(assignments[0] >> 1).tried_both())	// i > 0 added
				comp_infor->right_sat_prob = -3;	// backtrack right branch
			else
			{
				comp_infor->left_branch_done = false;	// backtrack left branch
				comp_infor->left_sat_prob = -3;
			}
#else
			if (i > 0 && variable(assignments[0] >> 1).tried_both())	// i > 0 added
				comp_infor->right_sat_prob.zero_flag = 1;
				//mpz_set_ui(comp_infor->right_sat_prob.numerator, 0);	// backtrack right branch
			else
			{
				comp_infor->left_branch_done = false;	// backtrack left branch
				comp_infor->left_sat_prob.zero_flag = 1;
				//mpz_set_ui(comp_infor->left_sat_prob.numerator, 0);
			}
#endif
//#ifndef BIG_NUM
//			if (!comp_infor->left_branch_done)
//				comp_infor->left_sat_prob = -3;
//			else
//				comp_infor->right_sat_prob = -3;
//#endif
			comp_infor->changed = false;
			comp_infor->num_children = 0;
			comp_infor->num_cached_children = 0;
			comp_infor->num_cross_implications = 0;
			comp_infor->largest_sum = 0;
			if (i > 0)
			{
				comp_infor->branched_child_list.clear();
				(*comp_infor->comp_itr)->comp_val_pair->cached_child_list.clear();
			}
		}

		while (!_branch_infor_stack[i+1]->changed_components.empty())
		{
			int top = _branch_infor_stack[i+1]->changed_components.back();
			_branch_infor_stack[i+1]->changed_components.pop_back();
			Component_information * changed_component = _component_infor_stack[top];

			if (changed_component->level < blevel && changed_component->active)
			{
				_branchable_component_stack.push_back(top);
				//if (flag)
				//	cout << "changed component " << _branch_infor_stack[i]->gid 
				//		 << " pushed back to _branchable_component_stack" << endl;
#ifndef BIG_NUM
				changed_component->right_sat_prob = changed_component->left_sat_prob 
												  = changed_component->sat_probability = -3;
#else
				changed_component->left_sat_prob.zero_flag = 1;
				changed_component->right_sat_prob.zero_flag = 1;
				changed_component->sat_probability.zero_flag = 1;
				//mpz_set_ui(changed_component->left_sat_prob.numerator, 0);
				//mpz_set_ui(changed_component->right_sat_prob.numerator, 0);
				//mpz_set_ui(changed_component->sat_probability.numerator, 0);
#endif
				changed_component->left_branch_done = false;
				changed_component->changed = false;
				changed_component->num_children = 0;
				changed_component->num_cached_children = 0;
				changed_component->num_cross_implications = 0;
				changed_component->largest_sum = 0;
				if (i > 0)
				{
					changed_component->branched_child_list.clear();
					(*changed_component->comp_itr)->comp_val_pair->cached_child_list.clear();
				}
			}
		}

		_num_implication_stack[i] = 0; // reset number of implications of the level, added by sang
		//if (variable(assignments[0] >> 1).tried_both())	// added by sang
		if (i > blevel - 1)
		{
			_branch_infor_stack[i]->gid = 0;	// this cannot be reset unless the decision var tried_both
			if (assignments.size() > 0)	// because for changed components, assignment.size() == 0
			{
				variable(assignments[0] >> 1).set_tried_both(false);
				for (int j = assignments.size() - 1 ; j >= 0; --j)
				{
					//variable((assignments[j] >> 1)).set_tried_both(false);
					unset_var_value(assignments[j] >> 1);
				}
				assignments.clear();
			}
		}
		
		int k = _branch_infor_stack[i]->num_new_children + _branch_infor_stack[i+1]->num_children_of_changed;
		/*if (flag)
		{
			cout << "when backtracking level " << i << ", " << k << " components to remove" << endl;
			cout << "_branch_infor_stack[i]->num_new_children = " << _branch_infor_stack[i]->num_new_children
				 << ", _branch_infor_stack[i+1]->num_children_of_changed = " 
				 << _branch_infor_stack[i+1]->num_children_of_changed << endl;
		}*/
		_branch_infor_stack[i+1]->num_children_of_changed = 0;	// reset it!
		// remove all new components created by that decision and all children of the possibly changed component
		// (if so, extract_new_components detected all children of that changed component)
		for (; k > 0; --k)
		{
			// release memory allocated to the components created at the current level
			// the number of components of the current level is stored in the last level
			//assert (!_component_infor_stack.back()->active); // only delete inactive components
			if (_component_infor_stack.back()->active)
			{
				//if (flag)
				//	cout << "removing active component " << _component_infor_stack.size()-1 
				//		 << " from component_infor_stack" << endl;
				Components::iterator & itr = _component_infor_stack.back()->comp_itr;
				// remove cached children of the current component first
				vector<child_to_remove *> & child_list = (*itr)->comp_val_pair->cached_child_list;
				for (int i = child_list.size()-1; i >= 0; --i)
				{
					hashtable->removeChildren(* child_list[i]);
				} // end for
				child_list.clear();
				_component_infor_stack.back()->num_cached_children = 0;

				/*vector<int> & cloned_child_list = _component_infor_stack.back()->branched_child_list;
				if (!cloned_child_list.empty())
				{
					cout << "removing branched children! " << endl;
					dump_assignment_stack();
					dump_branch_infor_stack();
					dump_branchable_component_stack();
					dump_components(_components);

					remove_branched_children(cloned_child_list);
				}
				cloned_child_list.clear();*/

				// delete the formula because it is useless
				if ((* itr)->comp_val_pair)
				{
#ifndef APPROXIMATE_HASHING
					//formula &f = (* itr)->comp_val_pair->f;
					//cout << "before pop_back(), f.capacity() = " << f.capacity() << endl;
					//while (!f.empty())
					//{
					//	delete(f.back());
					//	f.pop_back();
					//}
#endif
					vector<child_to_remove *> & child_list = (*itr)->comp_val_pair->cached_child_list;
					while (!child_list.empty())
					{
						delete child_list.back();
						child_list.pop_back();
					}
					delete _component_infor_stack.back()->var_set;
					delete (*itr)->comp_val_pair;
				}
				delete(*itr);
				_components.erase(itr, ++itr);
			}	// end if active
			//else if (flag)
			//	cout << "removing inactive component " << _component_infor_stack.size()-1 
			//		 << " from component_infor_stack" << endl;
			delete _component_infor_stack.back();
			_component_infor_stack.pop_back();		
		}	// end for
		_branch_infor_stack[i]->num_new_children = 0; // this must be reset each time backtracking
		_branch_infor_stack[i + 1]->num_children_of_changed = 0;	// bug removed! (i -> i+1)
		_branch_infor_stack[i + 1]->changed_components.clear();
		_branch_infor_stack[i + 1]->gid = 0; // this must be reset each time backtracking
	}	// end for

    dlevel() = blevel - 1;
    if (dlevel() < 0 ) 
	dlevel() = 0;
    ++_stats.num_backtracks;
}



void CSolver::back_track(int blevel)
{
	//dump_assignment_stack();
	//cout << "in backtrack, decisions = " << _stats.num_decisions << endl;
	/*(if (backtrack_tag)
	{
		cout << "in backtrack, level = " << dlevel() << ", decisions = " << _stats.num_decisions << endl;
		dump_assignment_stack();
		dump_branch_infor_stack();
		dump_components(_components);
		backtrack_tag = false;
	}*/
    DBG1(cout << "Back track to " << blevel <<" ,currently in "<< dlevel() << " level" << endl;);
    DBG2 (dump());
    //CHECK(verify_integrity());
    assert(blevel <= dlevel());
	assert (_component_infor_stack[_branch_infor_stack[blevel]->gid]->left_branch_done); // left must be done
	assert (!_component_infor_stack[_branch_infor_stack[blevel]->gid]->changed);	// must not be changed

	while(!cross_implication_list.empty())	// remove all obsolete cross implications
		if (cross_implication_list.back()->level >= blevel)
		{
			delete cross_implication_list.back();
			cross_implication_list.pop_back();
		}
		else
			break;

	// reset the branch component at blevel
	_component_infor_stack[_branch_infor_stack[blevel]->gid]->num_cached_children = 0;
	_component_infor_stack[_branch_infor_stack[blevel]->gid]->branched_child_list.clear();

    for (int i=dlevel(); i>= blevel; --i) 
	{
		_num_implication_stack[i] = 0; // reset number of implications of the level, added by sang
		vector<int> & assignments = *_assignment_stack[i];
		if (assignments.size() > 0)
		{
			if (variable(assignments[0] >> 1).tried_both())	// added by sang
			{
				_branch_infor_stack[i]->gid = 0;	// this cannot be reset unless the decision var tried_both
				variable((assignments[0] >> 1)).set_tried_both(false); // added by sang
			}
			for (int j = assignments.size() - 1 ; j >= 0; --j)
			{
				//variable((assignments[j] >> 1)).set_tried_both(false); // added by sang
				unset_var_value(assignments[j] >> 1);
				variable(assignments[j] >> 1).cross_flag = false;
			}
			assignments.clear();
		}
		
		int k = _branch_infor_stack[i]->num_new_children + _branch_infor_stack[i+1]->num_children_of_changed;
		/*if (flag)
		{
			cout << "when backtracking level " << i << ", " << k << " components to remove" << endl;
			cout << "_branch_infor_stack[i]->num_new_children = " << _branch_infor_stack[i]->num_new_children
				 << ", _branch_infor_stack[i+1]->num_children_of_changed = " 
				 << _branch_infor_stack[i+1]->num_children_of_changed << endl;
		}*/
		_branch_infor_stack[i+1]->num_children_of_changed = 0;	// reset it!
		// remove all new components created by that decision and all children of the possibly changed component
		// (if so, extract_new_components detected all children of that changed component)
		for (; k > 0; --k)
		{
			// release memory allocated to the components created at the current level
			// the number of components of the current level is stored in the last level
			//assert (!_component_infor_stack.back()->active); // only delete inactive components
			if (_component_infor_stack.back()->active)
			{
#ifdef BIG_NUM
				//mpz_clear(_component_infor_stack.back()->sat_probability.numerator);
				//mpz_clear(_component_infor_stack.back()->left_sat_prob.numerator);
				//mpz_clear(_component_infor_stack.back()->right_sat_prob.numerator);
#endif
				if (!_branchable_component_stack.empty())
					if (_component_infor_stack.size() - 1 == _branchable_component_stack.back())
					{
						_branchable_component_stack.pop_back();	// update _branchable_component_stack
						//cout << "matched! component " << _component_infor_stack.size() - 1 
						//	 << " removed from _branchable_component_stack " << endl;
						//dump_branchable_component_stack();
						//char c = getchar();
					}
				Components::iterator & itr = _component_infor_stack.back()->comp_itr;
				// delete the formula because it is useless
				if ((* itr)->comp_val_pair)
				{
#ifndef APPROXIMATE_HASHING
					//formula &f = (* itr)->comp_val_pair->f;
					//cout << "before pop_back(), f.capacity() = " << f.capacity() << endl;
					//while (!f.empty())
					//{
					//	delete(f.back());
					//	f.pop_back();
					//}
#endif
#ifdef BIG_NUM
				//cout << &(* itr)->comp_val_pair->value.numerator 
				//	 << " cleared in backtrack, decision = " << _stats.num_decisions << endl;
				//mpz_clear((* itr)->comp_val_pair->value.numerator);
#endif
					vector<child_to_remove *> & child_list = (*itr)->comp_val_pair->cached_child_list;
					while (!child_list.empty())
					{
						delete child_list.back();
						child_list.pop_back();
					}
					delete _component_infor_stack.back()->var_set;
					delete (*itr)->comp_val_pair;
				}
				delete(*itr);
				_components.erase(itr, ++itr);
			}	// end if active
			delete _component_infor_stack.back();
			_component_infor_stack.pop_back();		
		}	// end for
		_branch_infor_stack[i]->num_new_children = 0; // this must be reset each time backtracking
		_branch_infor_stack[i + 1]->num_children_of_changed = 0;	// bug removed! (i -> i+1)
		_branch_infor_stack[i + 1]->changed_components.clear();
		_branch_infor_stack[i + 1]->gid = 0; // this must be reset each time backtracking
	}	// end for
    dlevel() = blevel - 1;
    if (dlevel() < 0 ) 
	dlevel() = 0;
    ++_stats.num_backtracks;
    DBG2 (dump());
    //CHECK(verify_integrity());
}

int CSolver::deduce(void) 
{
    while (!_implication_queue.empty() && _conflicts.size()==0) {
	DBG2(dump_implication_queue(););
	const CImplication & imp = _implication_queue.front();
	int lit = imp.lit;
	int vid = lit>>1;
	int dl = imp.dlevel;
	//++_num_implication_stack[dl]; // added by sang
	ClauseIdx cl = imp.antecedent;
	_implication_queue.pop();
	CVariable & var = variable(vid);
	if (var.value() == UNKNOWN) { // an implication
	    set_var_value(vid, !(lit&0x1), cl, dl);
	    var.assgn_stack_pos() = _assignment_stack[dl]->size();
	    _assignment_stack[dl]->push_back(lit);
		// increment number of implications in this level
		//vector<int> & var_set = * _component_infor_stack[_branch_infor_stack[dl]->gid]->var_set;
		//if (binary_search(var_set.begin(), var_set.end(), vid))
		//++_num_implication_stack[dl]; // added by sang
	    //CHECK(verify_integrity());
	} // end if (var.value() == UNKNOWN)
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
		    var.assgn_stack_pos() = _assignment_stack[dl]->size();
		    _assignment_stack[dl]->push_back(lit);
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
	    if (var.dlevel() <= dl) continue;
	    int old_dl = var.dlevel();
	    int old_pos = var.assgn_stack_pos();
	    assert ((*_assignment_stack[old_dl])[old_pos] == lit);
	    (*_assignment_stack[old_dl])[old_pos] = 0; //insert a bubble
	    unset_var_value(vid);
	    set_var_value(vid, !(lit& 0x1), cl, dl);
	    var.assgn_stack_pos() = _assignment_stack[dl]->size();
	    _assignment_stack[dl]->push_back(lit);
	}
    }	// end while
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

// old version
/*
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
	    }
	}
	else {
	    assert (variable(v).dlevel() < dl);
	    if (variable(v).new_cl_phase() == UNKNOWN){ //it's not in the new cl
		++ _num_in_new_cl;
		variable(v).set_new_cl_phase((*itr).var_sign());
		_conflict_lits.push_back((*itr).s_var());
	    }
	    else //if this variable is already in the new clause, it must have the same phase 
		assert(variable(v).new_cl_phase() == (*itr).var_sign());
	}
    }
}*/


// new version from zchaff2004
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
	//if (dlevel() < LEVEL_MAX)
	//	level_conflicts[dlevel()]++;
	//else
	//	level_conflicts[LEVEL_MAX]++;
    if (dlevel() != 0)
		assert (_conflicts.size() > 0);
	//else assert(_stats.num_solutions > 0);
    assert(_conflict_lits.size() == 0); 
    assert (_implication_queue.empty());
    assert (_num_marked == 0);
    DBG1(dump_assignment_stack());
    //CHECK(verify_integrity());
    if (dlevel() == 0){ //already at level 0. Conflict means unsat.
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
			verify_out << " " << clause(ante).literal(j).s_var();
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
	_conflicts.clear();
	// back_track(0); // commented out by sang
	return -1;
    }
//    int result = conflict_analysis_firstUIP_resolve_based();
//	Added by sang--begin

//#ifdef LEARNING_OFF changed by sang
/*#ifdef STOP_ADDED_CLAUSE
	if (_stats.num_active_added_conf_clauses < ADDED_CL_MAX)
	{
		int result = conflict_analysis_firstUIP(); // commented out by sang
		DBG1( dump_assignment_stack(););	// commented out by sang
		return result;	// commented out by sang
	}
	cout << "NO more adding conflict clauses!" << endl;
#ifdef DEBUG_OUT
	cout << endl << "output status in analyze_conflicts(): " << endl;
	cout << "conflict size: " << _conflicts.size() << endl;
	cout << "conflict clauses are: ";
	for (int k=0;k<_conflicts.size();++k)
		cout << _conflicts[k];
#endif
	ClauseIdx cl = _conflicts[0];
    // assert(is_conflict(cl));
    int back_level = find_max_clause_dlevel(cl);
    vector <int> & assignments = *_assignment_stack[back_level];
    int last_branch = assignments[0];

#ifdef DEBUG_OUT
	cout << endl << "conflict[0] = " << cl << endl;
	cout << "backtrack to level " << back_level << endl;
#endif
	if ((back_level == 0) ||	// variables on level 0 can never be flipped
		((_stats.num_solutions == 0) && (back_level <= _uni_phased_size)))
	{ 	//	changed by sang
    	// variables <= level uni_phased.size() are uni_phased variables 
		// or inunit clauses that cannot be flipped
		cout << "there are " << _uni_phased_size << " uni_phased variables" << endl;
		if (back_level == 0)
			cout << "variables on level 0 can't be flipped, whole search space done" << endl;
		else 
		{
			cout << "Since no solution found so far, no need to flip uni_phased variables" 
				 << endl << "Halted, won't backtrack to level " << back_level << endl;
		}
	_conflicts.clear();
	//back_track(0); // commented out by sang
	return -1;
	}
    back_track(back_level);
	// flip the last branched variable that results the conflict
    queue_implication(last_branch ^ 0x1, NULL_CLAUSE, ++dlevel()); 
    _conflicts.clear();
    CHECK( dump_assignment_stack(););
    return back_level; 
#else */

	if (_stats.num_active_added_conf_clauses < max_num_learned_clause)
	{
		int result = conflict_analysis_firstUIP(); // commented out by sang
		DBG1( dump_assignment_stack(););		// commented out by sang
		return result;
	}
	else	// should stop adding learned clauses
	{
		//cout << "_stats.num_active_added_conf_clauses = " << _stats.num_active_added_conf_clauses;
		//cout << " >= max_num_learned_clause " << max_num_learned_clause << endl;
		//char c = getchar();
		int gid = _branch_infor_stack[dlevel()]->gid;	// get the current branch component
#ifdef BIG_NUM
		BigNum sat_prob(0, 0, true);	// initialized to 1
		int back_dl = percolate_up(sat_prob, gid);	// pass up UNSAT to higher level components
#else
		int back_dl = percolate_up(-1, gid);	// pass up UNSAT to higher level components
#endif
		//if (flag)
		//	cout << "percolate_up(-1, " << gid << ")" << endl;
		if (back_dl <= 0)
			return -1;

		int last_branch = (*_assignment_stack[back_dl])[0];
		int vid = last_branch >> 1;
		bool tried_both = variable(vid).tried_both();

		while (tried_both && (--back_dl >= 1))
		{
			last_branch = (*_assignment_stack[back_dl])[0];
			vid = last_branch >> 1;
			tried_both = variable(vid).tried_both();
		}

		if (back_dl == 0)
		{
			//if (flag)
			//	cout << "variables on level 0 can't be flipped, whole search space done" << endl;
			//_conflicts.clear();
			//CHECK( dump_assignment_stack(););
			return -1;
		}

		//if (flag)
		//	cout << "backtrack to level " << back_dl << endl;

		back_track(back_dl);		//	backtrack to the proper level
		// notice: dlevel()==back_dl-1 now! so need to increment dlevel() in the following
		while (!_implication_queue.empty())
			_implication_queue.pop();
		queue_implication(last_branch ^ 0x1, NULL_CLAUSE, ++dlevel()); // flip last_branch
		variable(vid).set_tried_both(true);	// tried both now, set the flag

		_conflicts.clear();
		return back_dl;
	}	// end else (_stats.num_active_added_conf_clauses < max_num_learned_clause)
	
	//	Added by sang--end

// #endif //STOP_ADDED_CLAUSE
}
//when all the literals involved are in _conflict_lits
//call this function to finish the adding clause and backtrack
int CSolver::finish_add_conf_clause(int gflag)
{
    unsigned int i;
    int back_dl = 0;
	int second_max = 0;	// the second max variable level of the learned clause
    int unit_lit = -1;

	//cout << "in finish_add_conflict_clause: " << endl;
	ClauseIdx added_cl = 
		add_conflict_clause(&(*_conflict_lits.begin()), _conflict_lits.size(), gflag);
	//cout << "num_clause: " << clauses().size() << endl;
    if (added_cl < 0 ) { //memory out.
	_stats.is_mem_out = true;
	_conflicts.clear();
	assert (_implication_queue.empty());
	return 1; 
    }    
#ifdef VERIFY_ON
    verify_out << "CL: " <<  clause(added_cl).id() << " <=";
    for (unsigned i=0; i< _resolvents.size(); ++i)
	verify_out << " " <<  _resolvents[i];
    verify_out << endl;
    _resolvents.clear();
#endif
	if (dynamic_heuristic > 3)
		adjust_variable_order (&(*_conflict_lits.begin()), _conflict_lits.size()); // removed by sang
    DBG0( cout << "**Add Clause " <<added_cl<< " : "<<clause(added_cl) << endl; );
//#ifdef DEBUG_OUT
	/*if (flag)//|| backtrack_tag)
	{
		cout << "************************" << endl
			 << "in finish_add_conf_clause: " << endl;
		//cout << "num of added clauses: " << _stats.num_active_added_conf_clauses << endl;
		//if (added_cl > 131000)
		cout << "Added Clause " <<added_cl<< " : "<<clause(added_cl); 
		dump_assignment_stack();
		cout << "************************" << endl;
		//dump_branch_infor_stack();
		//if (backtrack_tag)
		//{
		//	cout << "this branch UNSAT" << endl;
		//	exit(0);
		//}
	}*/
//#endif
    for (i=0; i< clause(added_cl).num_lits(); ++i) {
	int vid = clause(added_cl).literal(i).var_index();
	int sign =clause(added_cl).literal(i).var_sign();
	assert (variable(vid).value() != UNKNOWN);
	assert (literal_value(clause(added_cl).literal(i)) == 0);

	int dl = variable(vid).dlevel();
	if ( dl < dlevel()) {	
	    if (dl > second_max)	//	naming changed by sang 
		second_max = dl;
	}
	else {
	    assert (unit_lit == -1);
	    unit_lit = vid + vid + sign; 
	}	// end else 
	}	// end for

	back_dl = dlevel();	// back_dl may be changed by the following statments in TOCOMPONENTS
	int gid = _branch_infor_stack[dlevel()]->gid;	// get the current branch component
	//if (flag)
	//{
	//	cout << "before percolate_up(" << -1 << ", " << gid << ")," << endl;
	//	dump_components(_components);
	//}
#ifdef BIG_NUM
	//BigNum satprob(0, 0, true);	// initialized to 0 with zeroflag set true
	satprob.zero_flag = true;
	//mpz_set_ui(satprob.numerator, 0);
	//satprob.denominator = 0;
	back_dl = percolate_up(satprob, gid);	// pass up UNSAT to higher level components
#else
	back_dl = percolate_up(-1, gid);	// pass up UNSAT to higher level components
#endif
	/*if (flag)
	{
		cout << "after percolate_up(" << -1 << ", " << gid << ")," << endl;
	//	cout << "after percolate_up, back_dl = " << back_dl << ", dlevel() = " << dlevel() << endl;
	//	cout << "after percolate_up(-1, " << gid << "):" << endl;
		dump_components(_components);
	}*/

	/*if (_stats.num_solutions == 0 && back_dl <= _uni_phased_size)
	{
		//cout << "there is an UNSAT component at level " << back_dl 
		//	 << ", that means the original formula cannot be satisfied" << endl;
		return -1;
	}*/
	// added by sang -- begin
	if (back_dl <= 0)
		return -1;
	int last_branch = (*_assignment_stack[back_dl])[0];
	int vid = last_branch >> 1;
	bool tried_both = variable(vid).tried_both();
	//if (flag)
	//{
	//	cout << "tried_both = " << tried_both << endl;
	//	dump_assignment_stack();
	//}

#ifdef FORMULA_CACHING
#ifdef TOCOMPONENTS	// TOCOMPONENTS defined
	while (tried_both && (--back_dl >= 1))
	{
		last_branch = (*_assignment_stack[back_dl])[0];
		vid = last_branch >> 1;
		tried_both = variable(vid).tried_both();
	}
#endif	// end ifndef TOCOMPONENTS
#else	// NO FORMULA_CACHING
	while (tried_both && (--back_dl >= 1))
	{
		last_branch = (*_assignment_stack[back_dl])[0];
		vid = last_branch >> 1;
		tried_both = variable(vid).tried_both();
	}
#endif  // end NO FORMULA_CACHING
	if ((back_dl == 0)) // ||	// variables on level 0 can never be flipped
		//((_stats.num_solutions == 0) && (back_dl <= _uni_phased_size)))
	{	// backtracked to level 0, still no decision variable, can't be flipped, the whole search space done
		//if (flag)
		//	cout << "variables on level 0 can't be flipped, whole search space done" << endl;
		_conflicts.clear();
		return -1;
	}

	while (!_implication_queue.empty())
		_implication_queue.pop();

	//if (flag)
	//	cout << "in finish_add_conflict_clause, back_dl = " << back_dl << ", second_max = " << second_max << endl;
	//if (flag)
	//		cout << "dlevel = " << dlevel() << ", back_dl = " << back_dl << ", second_max = " << second_max << endl;
	//if (back_dl > second_max + 1)	// in this case, add the learned implication
	if (back_dl > second_max)	// in this case, add the learned implication
	{
		//if (far_back_track_enabled && num_conflicts > (num_sat << 1))	hybrid-1
		//if (far_back_track_enabled && num_conflicts > (num_sat << 1))	// hybrid-2
		if (far_back_track_enabled)
		{
			int blevel = 0;
			for (int j = back_dl - 1; j >= second_max; --j)
			{
				if (_component_infor_stack[_branch_infor_stack[j+1]->gid]->ancestor_index 
					!= _branch_infor_stack[j]->gid)
				{
					blevel = j + 1;
					break;
				}
				// search for a back_level where the decision has been flipped already
				if (j > 0 && variable((*_assignment_stack[j])[0] >> 1).tried_both())
				//if (j > 0 && _component_infor_stack[_branch_infor_stack[j]->gid]->left_branch_done)
					//&& _component_infor_stack[_branch_infor_stack[j]->gid]->active
					//&& _component_infor_stack[_branch_infor_stack[j]->gid]->left_sat_prob > -0.5)
				{
					//if (j + 1 <= back_dl - 1)
					//	blevel = j + 1;
					blevel = j;
					break;
				}
			}
			if (blevel == back_dl)
			{
				back_track(back_dl);	//	backtrack to the proper level
				// notice: dlevel()==back_dl-1 now! so need to increment dlevel() in the following
				queue_implication(last_branch ^ 0x1, NULL_CLAUSE, ++dlevel()); // flip last_branch
				variable(vid).set_tried_both(true);	// tried both now, set the flag
				queue_implication(unit_lit, added_cl, back_dl); // the implication is related to the branch component
			}
			else 
			{
				if (blevel == 0)
					blevel = second_max;
				//if (flag)
				//	cout << "blevel = " << blevel << ", unit_lit = " << unit_lit << endl;
				//last_branch = (*_assignment_stack[blevel])[0];
				far_back_track(blevel + 1);	//	backtrack to the proper level
				num_far_backtrack++;
				queue_implication(unit_lit, added_cl, blevel); // the implication is related to the branch component
			}
		}
		// if there are too many added conflict clauses, use far_back_track, otherwise back_track
		else if (_stats.num_active_added_conf_clauses >= _stats.num_decisions * backtrack_factor)
		//if (true) //(num_no_components == 0)
		/*	// far_back_track to a level between second_max+1 and back_dl if possible
		{
			int blevel = 0;
			for (int j = second_max + 1; j < back_dl; ++j)
			{
				// search for a back_level where the decision has been flipped already
				if (variable((*_assignment_stack[j])[0] >> 1).tried_both())
				{
					blevel = j;
					break;
				}
			}
			//if (flag)
			//	cout << "blevel = " << blevel << endl;
			if (blevel > 0)	
			{
				//last_branch = (*_assignment_stack[blevel])[0];
				far_back_track(blevel + 1);	//	backtrack to the proper level
				//variable(last_branch).set_tried_both(true);	// tried both now, set the flag
				//queue_implication(last_branch ^ 0x1, NULL_CLAUSE, ++dlevel()); // flip last_branch
				//if (back_dl > second_max)
				queue_implication(unit_lit, added_cl, blevel); // the implication is related to the branch component
			}
			else
		*/
			{	// far backtracking -b
				far_back_track(second_max+1);	//	backtrack to the proper level
				num_far_backtrack++;
				// notice: dlevel()==back_dl-1 now! so need to increment dlevel() in the following
				//queue_implication(last_branch ^ 0x1, NULL_CLAUSE, ++dlevel()); // flip last_branch
				//variable(vid).set_tried_both(true);	// tried both now, set the flag
				queue_implication(unit_lit, added_cl, second_max); // the implication is related to the branch component
			}
		//}
		else	// normal backtracking
		{
			back_track(back_dl);	//	backtrack to the proper level
			// notice: dlevel()==back_dl-1 now! so need to increment dlevel() in the following
			queue_implication(last_branch ^ 0x1, NULL_CLAUSE, ++dlevel()); // flip last_branch
			variable(vid).set_tried_both(true);	// tried both now, set the flag
			queue_implication(unit_lit, added_cl, back_dl); // the implication is related to the branch component
		}
	}
	else	// normal backtracking
	{
		back_track(back_dl);	//	backtrack to the proper level
		queue_implication(last_branch ^ 0x1, NULL_CLAUSE, ++dlevel()); // flip last_branch
		//if (back_dl > second_max)
		//	queue_implication(unit_lit, added_cl, back_dl); // the implication is related to the branch component
		variable(vid).set_tried_both(true);	// tried both now, set the flag
	}

	/*if (flag)
	{
		if (back_dl > second_max)
			cout << "after far_back_track to level " << second_max << " : " << endl;
		else
			cout << "after back_track to level " << back_dl << " : " << endl;
		dump_assignment_stack();
		dump_implication_queue();
		dump_components(_components);
	}*/

	_conflicts.clear();
    //CHECK( dump_assignment_stack(););
    // added by sang -- end
    for (i=1; i< _conflicts.size(); ++i) //after resolve the first conflict, others must also be resolved
	assert(!is_conflicting(_conflicts[i]));
    //_conflicts.clear();
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
  printf("\n\nNot implemented in this version\n\n");
  exit( 1 );

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

// old version
/*
int CSolver::conflict_analysis_firstUIP (void) 
{
    assert (dlevel() > 0);
    ClauseIdx cl = _conflicts[0];
    DBG0(cout <<"Conflict clause: " << cl << " : " << clause(cl) << endl;);
#ifdef DEBUG_OUT
	cout <<"Conflict clause: " << cl << " : " << clause(cl) << endl; // added by sang
#endif
    mark_vars_at_current_dlevel (cl, -1); //var)
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
		mark_vars_at_current_dlevel(ante_cl, assigned>>1); // var;
	    }
	}
    }
    return finish_add_conf_clause(gflag);
}*/		

// new version from zchaff2004
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



int CSolver::conflict_analysis_mincut (void) 
{
    assert (1 && "Not available, need a mincut library PLED");
    return 0;
}		

void CSolver::print_cls(ostream & os)
{
	cout << "printing clauses at level " << dlevel() << endl;
	os << endl << "************************************" << endl; // added by sang
    for (unsigned i=0; i< clauses().size(); ++i) 
	{
		cout << i << " : (";
		CClause & cl = clause(i);
		if (cl.status() == DELETED_CL)
		    continue;
		if (cl.status() == ORIGINAL_CL)
			os <<"O ";
		else {
			//assert (cl.status() == CONFLICT_CL);
			if (cl.status() == CONFLICT_CL)
	    		os << "Added ";
			else cout << cl.status() << " ";
		}
	/*for (int i=1; i< 33; ++i)	// commented out by sang
		if (cl.gid(i))
			cout << "  gid = " << i << endl; // added by sang
		//else cout << "  gid = 0" << endl;	 // added by sang
	*/ 
	// for (int i=1; i< 33; ++i)	// commented out by sang
	    // os <<( cl.gid(i) ? 1:0); // commented out by sang
	// os << "\t";					// commented out by sang
		for (unsigned j=0; j< cl.num_lits(); ++j) {
		// can be simplified to:
		// if (var.value() != (unsigned)(lit&0x1))	// added by sang
		//if (variable(cl.literal(j).var_index()).value() == UNKNOWN) // added by sang 
			os << (cl.literal(j).var_sign() ? "-":"") << cl.literal(j).var_index() << " ";
			/*if ((cl.literal(j).var_sign() == 0) &&						// added by sang 
			(variable(cl.literal(j).var_index()).value() == 1))
			cout << " S ";
			if ((cl.literal(j).var_sign() == 1) &&						// added by sang 
			(variable(cl.literal(j).var_index()).value() == 0))
			cout << " S ";*/
		}	// end for j
	os <<")" <<  endl;
	//cout << endl;	// added by sang
    }	// end for i
	os << "************************************" << endl; // added by sang
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

// old version
/*void CSolver::clean_up_dbase(void)
{
    assert (dlevel() == 0);
    assert (_stats.is_solver_started == false);
    int mem_before = mem_usage();

    //1. remove all the learned clauses
    int old_unrelevance = _params.cls_deletion.max_unrelevance;
    int old_minlits = _params.cls_deletion.min_num_lits;
    int old_cls_size = _params.cls_deletion.max_conf_cls_size;

    _params.cls_deletion.max_unrelevance = 0;
    _params.cls_deletion.min_num_lits = 0;
    _params.cls_deletion.max_conf_cls_size = 0;
    delete_unrelevant_clauses();
    _params.cls_deletion.max_unrelevance = old_unrelevance;
    _params.cls_deletion.min_num_lits = old_minlits;
    _params.cls_deletion.max_conf_cls_size = old_cls_size;

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
	    for (vector<int>::iterator itr = old_lits_cls.begin(); 
			itr != old_lits_cls.end(); ++itr)	// corrected by sang
			lits_cls.push_back(*itr);
	    old_lits_cls.swap(lits_cls); 
#endif
	}
    } 
    int mem_after = mem_usage();
    if (_params.verbosity > 0) 
	cout << "Database Cleaned, releasing (approximately) " 
	     << mem_before - mem_after << " Bytes" << endl;
}*/

// new version from zchaff2004
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
	    for (vector<int>::iterator itr = old_lits_cls.begin(); 
			itr != old_lits_cls.end(); ++itr)	// corrected by sang
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
    if (_params.verbosity > 1 ) 
	cout << "Restarting ... " << endl;
    if (dlevel() > 1) {
	//clean up the original var_stats.
/*	for (int i=1; i<variables().size(); ++i) {
	variable(i).score(0) = variable(i).lits_count(0);
	variable(i).score(1) = variable(i).lits_count(1);
	}
	update_var_score(); */
	back_track(1); //backtrack to level 0. restart.	
    }
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
//experimental, may be buggy
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
    int max_level = 0;
    int unit_lit = 0;
    int i, sz;
    for (i=0, sz= clause(cl).num_lits(); i<sz;  ++i) {
	int value =literal_value(clause(cl).literal(i));
	if (value != 0) {
	    if (unit_lit == 0)
		unit_lit = clause(cl).literals()[i].s_var();
	    else {//unit !=0, another non_zero, so it's neither unit nor conflict
		unit_lit = 0;
		break;
	    }
	}
	else {
	    int var_idx =((clause(cl).literals())[i]).var_index();
	    if (max_level < variable(var_idx).dlevel())
		max_level =  variable(var_idx).dlevel();
	}
    }
    if (unit_lit != 0) {
	int vid = (unit_lit >> 1);
	if (variable(vid).value() == UNKNOWN || variable(vid).dlevel() > max_level)
	    queue_implication(unit_lit, cl, max_level);
	DBG1(cout << "Additional Clause cause implication" << endl;);
    }
    else if (i >= sz) {
	//get through all the literals and unit still = 0, that means conflict
	//int d_lit = (*_assignment_stack[max_level])[0];
	back_track(max_level);
	assert (!is_conflicting(cl));
	assert (!is_satisfied(cl));
	int unit = find_unit_literal(cl);
	if (unit == 0){ //not a unit clause
	    //no need to do anything
	}
	else 
	    queue_implication(unit, cl, find_max_clause_dlevel(cl));
	DBG1(cout << "Additional Clause cause implication" << endl;);
    }
    return cl;
}

int CSolver::extract_new_components(Components & head, int level, unsigned gid)
// extract components from clauses of component gid in the last level
// gid is the index to the global Component_infor_stack, which can be used to identify a component uniquely
// copy other component pointers from last level
// and store the head pointer in head, return number of components
{
	int num_components = 0;
	int comp_infor_stack_pos = _component_infor_stack.size();	// index point of the current component in _component_infor_stack
	int sum_score = 0;
	int sum = 0;
	bool larger;
	CVariable & var = variable(0);
	
	++detection_seq;	// sequence number of the current component detection
	//cout << "detectio_seq == " << detection_seq << endl;
	vector<int> & var_set = * _component_infor_stack[gid]->var_set;
	assert (!var_set.empty());
	int sz = var_set.size();
	//cout << "size = " << sz << endl;

	//for (set<int>::iterator itr = var_set.begin(); itr != var_set.end(); ++itr)
	for (int idx = 0; idx < sz; ++ idx)
	{
		//int vid = (*itr); // current var in the component
		int vid = var_set[idx];
		if(touched[vid] == 1 || variable(vid).value() != UNKNOWN)
			continue;	// this var is already touched
		bool useful_var = false;
		CVariable & cur_var = variable(vid);
		for (int k = 0; k < 2; ++k)
		{
			int var_touched_sz = cur_var.lit_clause(k).size();	
			for (int i=0; i< var_touched_sz; ++i)
				if (!clause(cur_var.lit_clause(k)[i]).counter_one())
				{
					useful_var = true;
					break;
				}
			if (useful_var)
				break;
		}
		if (!useful_var)
			continue;
		// this clause is  untouched
		head.push_back(new Component_item);
		head.back()->comp_val_pair = new Component_value_pair;	// allocated memory for the pair
		++num_components;	// increment the counter

		//cout << "before set head.back()->index, comp_infor_stack_pos = " << comp_infor_stack_pos
		//	 << ", num_components = " << num_components << endl;
		head.back()->index = comp_infor_stack_pos + num_components - 1;	//  set the index to _component_infor_stack
		//cout << "after set head.back()->index, it is " << head.back()->index << endl;
		Component_information * comp_infor = new Component_information;
		Components::iterator itr = head.end();	// reset the iterator
		//comp_infor->tag = backtrack_tag;
		comp_infor->comp_itr = --itr;
		comp_infor->level = level;
		comp_infor->active = true;
		comp_infor->left_branch_done = false;
		comp_infor->changed = false;
#ifndef BIG_NUM
		comp_infor->sat_probability = -3;	// initialize to -3
		comp_infor->left_sat_prob = -3;
		comp_infor->right_sat_prob = -3;
#endif
		comp_infor->ancestor_index = gid;
		comp_infor->num_children = 0;
		comp_infor->largest_svar = 100000;
		comp_infor->largest_degree = 0;
		comp_infor->static_score = 100000;
		//comp_infor->largest_distance = 0;
		//comp_infor->var_set = new set<int>;
		comp_infor->var_set = new vector<int>;
		comp_infor->num_cached_children = 0;
		comp_infor->num_cross_implications = 0;
		comp_infor->cross_implication_weight = 1;	// added, 7/29/05
		comp_infor->largest_sum = 0;
		comp_infor->num_clause = 0;

		_component_infor_stack.push_back(comp_infor);	// store comp_infor in the global stack
		formula & comp= head.back()->comp_val_pair->f;	// comp = formula
	
		vector<int> & variables = * comp_infor->var_set;

		touched_var_stack.push_back(vid);
		touched[vid] = 1;	// set the touched flag for variable vid

		while (!touched_var_stack.empty())
		{
			int v = touched_var_stack.back();	// get the last touched var, and then remove it from stack
			touched_var_stack.pop_back();
			variables.push_back(v);
			//cout << "touched_var_stack :" << endl;
			//dump_touched_var_stack();
			//char c = getchar();
			CVariable & var = variable(v);
			int degree[2]; // degree[0]==pos_degree, degree[1]==neg_degree
			degree[0] = degree[1] = 0;	

			for (int k = 0; k < 2; ++k)
			{
				int var_touched_sz = var.lit_clause(k).size();
				
				for (int i = 0; i < var_touched_sz; ++i)
				{
					int cl_num = var.lit_clause(k)[i];
					CClause & cla = clause(cl_num);
					if (cla.counter_one())
					{
						//cout << "clause " << cl_num << " already satisfied" << endl;
						continue;	// this clause is either not a member of this clause-component group or satisfied
					}
					if (clause_comp[cl_num] == detection_seq)
					{
						//cout << "clause " << cl_num << " already processed" << endl;
						//if (dynamic_heuristic)
						//if (clause_comp[cl_num] == _component_infor_stack.size() - 1) // marked by current component
						++degree[k];
						continue;
					}
					clause_comp[cl_num] = detection_seq;	// set detection sequence number
					//else push the clause into formula comp
					//comp.push_back(new vector <int>);	// allocate memory for a clause
					//if (dynamic_heuristic)
					++degree[k];	// increment degree-counter
					
					int cla_sz = cla.num_lits();
					int num_unkown = 0;
					for (int j = 0; j < cla_sz; ++j)
					{
						int idx = cla.literal(j).var_index();
						if (variable(idx).value() == UNKNOWN)
						{
							//comp.back()->push_back(cla.literal(j).var_sign() ? -idx : idx);
							//comp.back()->push_back(cla.literal(j).s_var());	// changed by sang
							comp.push_back(cla.literal(j).s_var());	
							num_unkown++;
							//cout << "in clause " << var.lit_clause(k)[i] << ", "
							//cout << "literal " << (cla.literal(j).var_sign() ? idx : idx)
							//	 << " pushed into clause of component" << endl;
							//char c = getchar();
							//if (!variable(idx).is_touched())
							if (touched[idx] == 0)
							{
								touched_var_stack.push_back(idx);
								touched[idx] = 1;
								//variable(idx).touched();
								//cout << "in clause " << var.lit_clause(k)[i]
								//	 << ", variable " << idx << " is not touched, pushed into var_stack" << endl;
								//char c = getchar();
							}	// end if
						}	// end if
					}	// end for j
					if ((dynamic_heuristic == UNIT_PROP || dynamic_heuristic == BERKMIN) && num_unkown == 2)
					{
						//cout << "binary clause found: ("
						//	 << comp[comp.size() - 2] << ", " << comp.back() << ")" << endl;
						//cout << "var_score.size() = " << var_score.size() << endl;							
						var_score[comp[comp.size() - 2]]++;
						var_score[comp.back()]++;
					}
					comp.push_back(0);	// add 0 as a terminating symbol for a clause
					comp_infor->num_clause++;
					
				}	// end for i
			}	// end for k

			sum = degree[0] + degree[1];
			larger = (degree[0] < degree[1]);

			//if (!static_heuristic || comp_infor->largest_degree == variable(v)._static_score)
			if (!static_heuristic)
			{
				switch(dynamic_heuristic)
				{
				case DEGREE:	
							if (comp_infor->largest_degree < degree[larger]
								|| ((comp_infor->largest_degree == degree[larger]) 
								&& (comp_infor->largest_svar > (v << 1))))
							{
								comp_infor->largest_degree = degree[larger];
								comp_infor->largest_svar = (v << 1) + larger;
							}
							break;
				case DEGREE_SUM:	
							if ((comp_infor->largest_degree < degree[larger])
								|| (comp_infor->largest_degree == degree[larger] 
								&& comp_infor->largest_sum < sum))
							{
								comp_infor->largest_sum = sum;
								comp_infor->largest_svar = (v << 1) + larger;
								comp_infor->largest_degree = degree[larger];
							}
							break;
				case SUM:
							if (comp_infor->largest_sum < sum)
							{
								comp_infor->largest_sum = sum;
								comp_infor->largest_svar = (v << 1) + larger;
								comp_infor->largest_degree = degree[larger];
							}
							break;
				case SUM_DEGREE:
							if ((comp_infor->largest_sum < sum)
								|| (comp_infor->largest_sum == sum && comp_infor->largest_degree < degree[larger]))
							{
								comp_infor->largest_sum = sum;
								comp_infor->largest_svar = (v << 1) + larger;
								comp_infor->largest_degree = degree[larger];
							}
							break;
				case VSIDS: 
							var = variable(v);
							sum_score = var.score(0) + var.score(1);
							if ((comp_infor->largest_sum < sum_score)
								|| (comp_infor->largest_sum == sum_score 
								&& comp_infor->largest_degree < var.score())
								|| comp_infor->largest_sum == 0)
							{
								//cout << "var " << v << " selected as new s_var" 
								//	 << ", var " << (comp_infor->largest_svar >> 1) << " replaced" << endl; 
								comp_infor->largest_sum = sum_score;
								comp_infor->largest_degree = var.score();
								if (var.score(0) >= var.score(1))
								{
									comp_infor->largest_svar = v << 1;
									//comp_infor->largest_degree = var.score(0);
								}
								else
								{
									comp_infor->largest_svar = (v << 1) + 1;
									//comp_infor->largest_degree = var.score(1);
								}
							}
							break;
				case UNIT_PROP:	;
				case BERKMIN:	;
				case VSADS:	
							var = variable(v);
							int shifted_sum = sum >> 1;
							//if (shifted_sum == 0)
							//	shifted_sum = 1;
							/*int vsids_sum = var.score(0) + var.score(1);
							if (vsids_sum > 0)
								sum_score = vsids_sum;
							else
								sum_score = 1;*/
							sum_score = shifted_sum + var.score(0) + var.score(1);	// define half-sum socre here
							//sum_score = (sum >> 2) + var.score(0) + var.score(1);	// define half-sum socre here
							//sum_score = sum + var.score(0) + var.score(1);	// define sum socre here
							//var_score[v] = sum_score;
							if ((comp_infor->largest_sum < sum_score)
								|| (comp_infor->largest_sum == sum_score 
								&& comp_infor->largest_degree < sum))
							{
								comp_infor->largest_sum = sum_score;
								comp_infor->largest_degree = sum;
								comp_infor->largest_svar = (v << 1) + (var.score(0) >= var.score(1) ? 0 : 1);
							}
				}	// end switch
			}	
			else if (comp_infor->static_score > variable(v)._static_score)
			// the smaller the better static_score is
			{
				comp_infor->static_score = variable(v)._static_score;
				switch(dynamic_heuristic)
				{
				case DEGREE:								
							comp_infor->largest_degree = degree[larger];
							comp_infor->largest_svar = (v << 1) + larger;
							break;
				case DEGREE_SUM:	
							comp_infor->largest_sum = sum;
							comp_infor->largest_svar = (v << 1) + larger;
							comp_infor->largest_degree = degree[larger];
							break;
				case SUM:
							comp_infor->largest_sum = sum;
							comp_infor->largest_svar = (v << 1) + larger;
							comp_infor->largest_degree = degree[larger];
							break;
				case SUM_DEGREE:
							comp_infor->largest_sum = sum;
							comp_infor->largest_svar = (v << 1) + larger;
							comp_infor->largest_degree = degree[larger];
							break;
				case VSIDS: 
							var = variable(v);
							sum_score = var.score(0) + var.score(1);
							comp_infor->largest_sum = sum_score;
							comp_infor->largest_degree = var.score();
							if (var.score(0) >= var.score(1))
							{
								comp_infor->largest_svar = v << 1;
								//comp_infor->largest_degree = var.score(0);
							}
							else
							{
								comp_infor->largest_svar = (v << 1) + 1;
								//comp_infor->largest_degree = var.score(1);
							}
							break;
				case UNIT_PROP:	;
				case BERKMIN:	;
				case VSADS:	
							var = variable(v);
							int shifted_sum = sum >> 1;
							//if (shifted_sum == 0)
							//	shifted_sum = 1;
							/*int vsids_sum = var.score(0) + var.score(1);
							if (vsids_sum > 0)
								sum_score = vsids_sum;
							else
								sum_score = 1;*/
							sum_score = shifted_sum + var.score(0) + var.score(1);	// define half-sum socre here
							//sum_score = (sum >> 2) + var.score(0) + var.score(1);	// define half-sum socre here
							//sum_score = sum + var.score(0) + var.score(1);	// define sum socre here
							//var_score[v] = sum_score;
							comp_infor->largest_sum = sum_score;
							comp_infor->largest_degree = sum;
							comp_infor->largest_svar = (v << 1) + (var.score(0) >= var.score(1) ? 0 : 1);
				}	// end switch
			}	// end else if
			else if (comp_infor->static_score == variable(v)._static_score)
			// the smaller the better static_score is
			{
				switch(dynamic_heuristic)
				{
				case DEGREE:	
							if (comp_infor->largest_degree < degree[larger]
								|| ((comp_infor->largest_degree == degree[larger]) 
								&& (comp_infor->largest_svar > (v << 1))))
							{
								comp_infor->largest_degree = degree[larger];
								comp_infor->largest_svar = (v << 1) + larger;
							}
							break;
				case DEGREE_SUM:	
							if ((comp_infor->largest_degree < degree[larger])
								|| (comp_infor->largest_degree == degree[larger] 
								&& comp_infor->largest_sum < sum))
							{
								comp_infor->largest_sum = sum;
								comp_infor->largest_svar = (v << 1) + larger;
								comp_infor->largest_degree = degree[larger];
							}
							break;
				case SUM:
							if (comp_infor->largest_sum < sum)
							{
								comp_infor->largest_sum = sum;
								comp_infor->largest_svar = (v << 1) + larger;
								comp_infor->largest_degree = degree[larger];
							}
							break;
				case SUM_DEGREE:
							if ((comp_infor->largest_sum < sum)
								|| (comp_infor->largest_sum == sum && comp_infor->largest_degree < degree[larger]))
							{
								comp_infor->largest_sum = sum;
								comp_infor->largest_svar = (v << 1) + larger;
								comp_infor->largest_degree = degree[larger];
							}
							break;
				case VSIDS: 
							var = variable(v);
							sum_score = var.score(0) + var.score(1);
							if ((comp_infor->largest_sum < sum_score)
								|| (comp_infor->largest_sum == sum_score 
								&& comp_infor->largest_degree < var.score())
								|| comp_infor->largest_sum == 0)
							{
								//cout << "var " << v << " selected as new s_var" 
								//	 << ", var " << (comp_infor->largest_svar >> 1) << " replaced" << endl; 
								comp_infor->largest_sum = sum_score;
								comp_infor->largest_degree = var.score();
								if (var.score(0) >= var.score(1))
								{
									comp_infor->largest_svar = v << 1;
									//comp_infor->largest_degree = var.score(0);
								}
								else
								{
									comp_infor->largest_svar = (v << 1) + 1;
									//comp_infor->largest_degree = var.score(1);
								}
							}
							break;
				case UNIT_PROP:	;
				case BERKMIN:	;
				case VSADS:	
							var = variable(v);
							int shifted_sum = sum >> 1;
							//if (shifted_sum == 0)
							//	shifted_sum = 1;
							/*int vsids_sum = var.score(0) + var.score(1);
							if (vsids_sum > 0)
								sum_score = vsids_sum;
							else
								sum_score = 1;*/
							sum_score = shifted_sum + var.score(0) + var.score(1);	// define half-sum socre here
							//sum_score = (sum >> 2) + var.score(0) + var.score(1);	// define half-sum socre here
							//sum_score = sum + var.score(0) + var.score(1);	// define sum socre here
							//var_score[v] = sum_score;
							if ((comp_infor->largest_sum < sum_score)
								|| (comp_infor->largest_sum == sum_score 
								&& comp_infor->largest_degree < sum))
							{
								comp_infor->largest_sum = sum_score;
								comp_infor->largest_degree = sum;
								comp_infor->largest_svar = (v << 1) + (var.score(0) >= var.score(1) ? 0 : 1);
							}
				}	// end switch
			}	// end else if (!static_heuristic)

			/*else if (comp_infor->static_score > variable(v)._static_score)	
			// the smaller the better static_score is
				{
					comp_infor->static_score = variable(v)._static_score;
					comp_infor->largest_degree = degree[larger];
					comp_infor->largest_sum = sum;
					comp_infor->largest_svar = (v << 1) + larger;
				}
			*/
		}	// end while
		//hashtable->print_formula(comp);
		//comp_infor->num_clause = comp.size();	// comp = formula
		//hashtable->quicksort(head.back()->comp_val_pair->f, 0, head.back()->comp_val_pair->f.size() - 1);
		sort(variables.begin(), variables.end());
		
		/*if (dynamic_heuristic == VSADS)	// VSADS-rand
		{
			vector <int> best_scores;
			for (int k = var_set.size() - 1; k >= 0; k--)
				if (var_score[var_set[k]] >= comp_infor->largest_sum * 0.85)
					best_scores.push_back(var_set[k]);
			int decision = best_scores[rand() % best_scores.size()];
			comp_infor->largest_svar = (decision << 1);
				+ (variable(decision).score(0) >= variable(decision).score(1) ? 0 : 1);
		}
		else */
		
		if (dynamic_heuristic == UNIT_PROP)
		{
			//vector<int> vars = *comp_infor->var_set;
			//for (int k = vars.size() - 1; k >= 0; k--)
			//	var_score[vars[k] << 1] = var_score[(vars[k] << 1) + 1] = 0;
			for (int i = 0; i < 10; i++)
				candidates[i] = 0;
			int up_score;
			int largest_score = 0;
			int largest_svar = 0;
			int tail = -1;
			int current = 0;

			//for (int i = comp_infor->num_clause - 1; i >= 0; --i)
			//	if (comp[i]->size() == 2)	// a binary clause
			//	{
			//		//cout << "binary clause: " << (*comp[i])[0] << ", " << (*comp[i])[1] << endl;
			//		var_score[(*comp[i])[0]]++;
			//		var_score[(*comp[i])[1]]++;
			//	}

			for (int k = variables.size() - 1; k >= 0; k--)
			{
				if (comp_infor->static_score < variable(variables[k])._static_score)	// bug fixed
					continue; // static score worse than the best, aborted
					
				int j = (variables[k] << 1) + 1;
				up_score = var_score[j] + var_score[j - 1];
				if (up_score > 0)
				{
					var_score[j] = var_score[j] * var_score[j - 1] + up_score;
					if (tail == -1)
					{
						tail = 0;
						candidates[0] = j;
					}
					else if (var_score[j] > var_score[candidates[tail]])	// bug
					{
						tail = (tail + 1) > 9 ? 9 : (tail + 1);
						current = tail;
						while (current >= 0)
						{
							if (var_score[j] > var_score[candidates[current]])
								current--;
							else
							{
								current++;
								break;
							}
						}
						if (current < 0)
							current = 0;
						for (int i = tail; i > current; --i)
							candidates[i] = candidates[i - 1];
						candidates[current] = j;
					}	// end else if (var_score[j] > var_score[candidates[tail]])
					else if (tail < 9)
						candidates[++tail] = j;
				}	// end if (up_score > 0)
			}	// end for j
			//for (int i = 0; i <= tail; ++i)
			//	cout << "(" << candidates[i] << "," << var_score[candidates[i]] << ") ";
			//cout << endl;
			//cout << "**********************" << endl;
			if (tail == 0)	// only one var with score > 0, choose it
				comp_infor->largest_svar = candidates[0] 
											- (var_score[candidates[0]] > var_score[candidates[0] - 1] ? 0 : 1);
			else if (tail > 0)
			{
				vector<int> * assignments;
				bool noconflict = true;
				// do real unit propagation to determine actual scores of candidates
				for (int i = 0; i <= tail; i++)
				{
					assert(candidates[i] - 1 == candidates[i] ^ 0x1);
					queue_implication(candidates[i], NULL_CLAUSE, ++dlevel()); // made a decision
					if (deduce() == CONFLICT)	// favor var that leads to a conflict
					{
						candidates[0] = candidates[i];
						_conflicts.clear();
						assignments = _assignment_stack[dlevel()];
						for (int j = assignments->size() - 1 ; j >= 0; --j)
							unset_var_value((*assignments)[j] >> 1);
						assignments->clear();
						while(!_implication_queue.empty())
							_implication_queue.pop();
						--dlevel();
						noconflict = false;
						break;
					}
					assignments = _assignment_stack[dlevel()];
					var_score[candidates[i]] = assignments->size() - 1;
					for (int j = assignments->size() - 1 ; j >= 0; --j)
						unset_var_value((*assignments)[j] >> 1);
					assignments->clear();
					while(!_implication_queue.empty())
						_implication_queue.pop();
					//--dlevel();
					
					queue_implication(candidates[i] - 1, NULL_CLAUSE, dlevel()); // flip the decision
					if (deduce() == CONFLICT)	// favor var that leads to a conflict
					{
						candidates[0] = candidates[i] - 1;
						_conflicts.clear();
						assignments = _assignment_stack[dlevel()];
						for (int j = assignments->size() - 1 ; j >= 0; --j)
							unset_var_value((*assignments)[j] >> 1);
						assignments->clear();
						while(!_implication_queue.empty())
							_implication_queue.pop();
						--dlevel();
						noconflict = false;
						break;
					}
					assignments = _assignment_stack[dlevel()];
					var_score[candidates[i] - 1] = assignments->size() - 1;
					for (int j = assignments->size() - 1 ; j >= 0; --j)
						unset_var_value((*assignments)[j] >> 1);
					assignments->clear();
					while(!_implication_queue.empty())
						_implication_queue.pop();
					--dlevel();
				}	// end for i
				if (noconflict)
				{
					int finalist[10];
					int counter = 0;
					double thresh_hold;
					largest_score = 0;

					for (int i = 0; i <= tail; i++)
					{
						up_score = var_score[candidates[i]] * var_score[candidates[i]-1] 
									+ var_score[candidates[i]] + var_score[candidates[i]-1];
						var_score[candidates[i] - 1] = 
												var_score[candidates[i]] > var_score[candidates[i] - 1] ? 0 : 1;
						var_score[candidates[i]] = up_score;
						if (up_score > largest_score)
							{
								largest_score = up_score;
								finalist[0] = candidates[i];
							}
					}

					//cout << "updated scores of candidates: " << endl;
					//for (int i = 0; i <= tail; ++i)
					//	cout << "(" << candidates[i] << "," << var_score[candidates[i]] << ") ";
					//cout << endl;

					thresh_hold = largest_score * 0.9;
					//cout << "thresh_hold = " << thresh_hold << endl;
					for (int i = 0; i <= tail; i++)
						if (var_score[candidates[i]] > thresh_hold && candidates[i] != finalist[0])
						{
							finalist[++counter] = candidates[i];
							//cout << "finalist[" << counter << "] set to "  << candidates[i] << endl;
						}

					//cout << "finalist" << ", counter = " << counter << ", " << "tail = " << tail << endl;
					//for (int i = 0; i <= counter; i++)
					//	cout << "(" << finalist[i] << "," << var_score[finalist[i]] << ") ";
					//cout << endl;
					//if (candidates[0] > 0)
					//assert(var_score[finalist[0]] == largest_score);
					//assert(var_score[finalist[0]-1] == 0 || var_score[finalist[0]-1] == 1);
					counter = 0;	// set counter to 0 means randomization on var-selection off
					if (counter == 0)
						comp_infor->largest_svar = finalist[0] - var_score[finalist[0] - 1]; 
					else	// randomly choose a var from top 10% scores
					{
						counter = rand() % (counter+1);
						comp_infor->largest_svar = finalist[counter] - var_score[finalist[counter] - 1];
					}

					//					- (var_score[candidates[0]] > var_score[candidates[0] - 1] ? 0 : 1);
				}
				else
				{
					comp_infor->largest_svar = candidates[0];
					//cout << "conflict on " << candidates[0] << endl;
				}
			}	// end else if (tail > 0)
			//else  // no var instanciation will result unit-prop, pick one randomly
			//{
			//	comp_infor->largest_svar = vars[rand() % vars.size()] << 1;
			//}
			for (int k = variables.size() - 1; k >= 0; k--)	// reset scores
				var_score[variables[k] << 1] = var_score[(variables[k] << 1) + 1] = 0;
		}	 // end if (dynamic_heuristic == UNIT_PROP)
		else if (dynamic_heuristic == BERKMIN)
		{
			//vector<int> vars = *comp_infor->var_set;
			//for (int k = vars.size() - 1; k >= 0; k--)
			//{
			//	int temp = vars[k] << 1;
			//	var_score[temp] = var_score[temp + 1] = score_sum[temp] = score_sum[temp+1] = 0;
			//}

			int up_score;
			int largest_score = 0;
			int largest_svar = 0;

			//for (int i = comp_infor->num_clause - 1; i >= 0; --i)	// first pass, compute var_score
			//	if (comp[i]->size() == 2)	// a binary clause
			//	{
			//		//cout << "binary clause: " << (*comp[i])[0] << ", " << (*comp[i])[1] << endl;
			//		var_score[(*comp[i])[0]]++;
			//		var_score[(*comp[i])[1]]++;
			//	}
			//score_sum = var_score;	// copy vector
			//if (flag2)
			//{
			//	cout << "variable scores after detecting binary clauses: " << endl;
			//	for (int k = 0; k < variables.size(); k++)
			//		cout << "(" << variables[k] << ":" << (var_score[variables[k] << 1]) << ") "
			//			 << "(-" << variables[k] << ":" << (var_score[(variables[k] << 1) + 1]) << ") ";
			//	cout << endl;
			//}
			for (int k = variables.size() - 1; k >= 0; k--)
			{
				int temp = variables[k] << 1;
				//score_sum[temp] = var_score[temp+1];	// bug?
				//score_sum[temp+1] = var_score[temp];
				score_sum[temp] = var_score[temp];	// changed
				score_sum[temp+1] = var_score[temp+1];
			}
			//if (flag)
			//{
			//	cout << "score_sum: " << endl;
			//	for (int k = 0; k < score_sum.size(); k++)
			//		cout << score_sum[k] << " ";
			//	cout << endl;
			//}

			//for (int i = comp_infor->num_clause - 1; i >= 0; --i)	// second pass, compute largest_score
			//	if (comp[i]->size() == 2)	// a binary clause
			int num_literal = 0;
			/*for (int i = 0; i < comp.size(); i++)
			{
				if (comp[i] != 0)
					num_literal++;
				else
				{
					if (num_literal == 2)
					{
						// cout << "binary clause found " << endl;
						// a binary clause will change two var's score
						score_sum[comp[i - 1] ^ 0x1] += var_score[comp[i - 2] ^ 0x1]; 
						score_sum[comp[i - 2] ^ 0x1] += var_score[comp[i - 1] ^ 0x1];
						up_score = score_sum[comp[i - 1] ^ 0x1] + score_sum[comp[i - 1]];
						if (up_score > largest_score)
						{
							largest_score = up_score;
							largest_svar = score_sum[comp[i - 1] ^ 0x1] > score_sum[comp[i - 1]]? 
											comp[i - 1] ^ 0x1 : comp[i - 1];
						}
						up_score = score_sum[comp[i - 2] ^ 0x1] + score_sum[comp[i - 2]];
						if (up_score > largest_score)
						{
							largest_score = up_score;
							largest_svar = score_sum[comp[i - 2] ^ 0x1] > score_sum[comp[i - 2]]? 
										  comp[i - 2] ^ 0x1 : comp[i - 2];
						}
					}	// end if
					num_literal = 0;
				}	// end else
			}	// end for i
			*/
			for (int i = comp.size() - 2; i >= 0 ; i--)	// comp ends with 0, and starts with a literal
			{
				if (comp[i] != 0) // && i != 0)
					num_literal++;
				else
				{
					if (num_literal == 2)
					{
						//if (flag)
						//	cout << "binary clause found: (" 
						//		 << (comp[i + 1] & 0x1 ? "-":"") << (comp[i + 1] >> 1)
						//		 << "," << (comp[i + 2] & 0x1 ? "-":"") << (comp[i + 2] >> 1) << ")" << endl;
						// a binary clause will change two var's score
						score_sum[comp[i + 1] ^ 0x1] += var_score[comp[i + 2] ^ 0x1]; // that's correct! not a bug
						score_sum[comp[i + 2] ^ 0x1] += var_score[comp[i + 1] ^ 0x1];
						up_score = score_sum[comp[i + 1] ^ 0x1] + score_sum[comp[i + 1]];
						if (up_score > largest_score && comp_infor->static_score 
							== variable(comp[i + 1] >> 1)._static_score)
						{
							largest_score = up_score;
							largest_svar = score_sum[comp[i + 1] ^ 0x1] <= score_sum[comp[i + 1]]? 
											comp[i + 1] ^ 0x1 : comp[i + 1];
						}
						up_score = score_sum[comp[i + 2] ^ 0x1] + score_sum[comp[i + 2]];
						if (up_score > largest_score && comp_infor->static_score 
							== variable(comp[i + 2] >> 1)._static_score)
						{
							largest_score = up_score;
							largest_svar = score_sum[comp[i + 2] ^ 0x1] <= score_sum[comp[i + 2]]? 
										  comp[i + 2] ^ 0x1 : comp[i + 2];
						}
					}	// end if
					num_literal = 0;
				}	// end else
			}	// end for i
			if (num_literal == 2)	// make up for the first clause which could be binary too
			//if (comp[2] == 0)
			{
				//if (flag)
				//	cout << "binary clause found: (" 
				//		 << (comp[0] & 0x1 ? "-":"") << (comp[0] >> 1)
				//		 << "," << (comp[1] & 0x1 ? "-":"") << (comp[1] >> 1) << ")" << endl;
				// a binary clause will change two var's score
				score_sum[comp[0] ^ 0x1] += var_score[comp[1] ^ 0x1]; 
				score_sum[comp[1] ^ 0x1] += var_score[comp[0] ^ 0x1];
				up_score = score_sum[comp[0] ^ 0x1] + score_sum[comp[0]];
				if (up_score > largest_score && comp_infor->static_score 
					== variable(comp[0] >> 1)._static_score)
				{
					largest_score = up_score;
					largest_svar = score_sum[comp[0] ^ 0x1] <= score_sum[comp[0]]? 
									comp[0] ^ 0x1 : comp[0];
				}
				up_score = score_sum[comp[1] ^ 0x1] + score_sum[comp[1]];
				if (up_score > largest_score && comp_infor->static_score 
					== variable(comp[1] >> 1)._static_score)
				{
					largest_score = up_score;
					largest_svar = score_sum[comp[1] ^ 0x1] <= score_sum[comp[1]]? 
								  comp[1] ^ 0x1 : comp[1];
				}
			}	// end if
			if (largest_svar > 0)
			{
				comp_infor->largest_svar = largest_svar;
				//if (flag)
				//	cout << "largest_svar " << largest_svar << " has score = " << largest_score << endl;
			}
			for (int k = variables.size() - 1; k >= 0; k--)	// reset scores
			{
				var_score[variables[k] << 1] = var_score[(variables[k] << 1) + 1] = 0;
				//score_sum[variables[k] << 1] = score_sum[(variables[k] << 1) + 1] = 0;
			}
		}	// end else if (dynamic_heuristic == BERKMIN)
	} // end for idx

	 // keep track of num of children of a breaking component
	_component_infor_stack[gid]->num_children = num_components;
	//_branch_infor_stack[level - 1]->num_new_children = num_components;
	for (int idx = 0; idx < sz; ++ idx)
		touched[var_set[idx]] = 0;	// reset all values possibly changed back to 0
	//cout << "new components: " << num_components << endl;
	return  num_components;
}


void CSolver::dump_touched_var_stack()	// added by sang
{
	for (unsigned i = 0; i < touched_var_stack.size(); ++i)
		cout << touched_var_stack[i] << " ";
	cout << endl << "****************************************************" << endl;
	return;
}

void CSolver::dump_branchable_component_stack()	// added by sang
{
	cout << "_branchable_component_stack: " << endl;
	for (unsigned i = 0; i < _branchable_component_stack.size(); ++i)
		cout << _branchable_component_stack[i] << ", ";
	cout << endl << "****************************************************" << endl;
	return;
}

#ifndef BIG_NUM
void CSolver::dump_components(Components & head)	// added by sang
{
	//int comp_index = 1;
	//if (head.empty())
	//{
	//	cout << "empty components" << endl << "****************************************************" << endl;
	//	return;
	//}	
	cout << endl << "****************************************************" << endl;
	// component 0 is not in the component list!
	cout << "_component_infor_stack[0]: " << endl;
	cout << "sat_probability = " << _component_infor_stack[0]->sat_probability
		 << ", left_sat_prob = " << _component_infor_stack[0]->left_sat_prob
		 << ", right_sat_prob = " << _component_infor_stack[0]->right_sat_prob << endl
		 << (_component_infor_stack[0]->active ? "active " : ", inactive ") 
		 << ", num_children = " << _component_infor_stack[0]->num_children
		 << ", num_cached_children = " << _component_infor_stack[0]->num_cached_children << endl;
	if (!_component_infor_stack[0]->var_set->empty())
		cout << ", var_set.size() = " << _component_infor_stack[0]->var_set->size() << endl;
	else
		cout << ", empty var_set!" << endl;
	cout << "****************************************************" << endl;

	for (Components::iterator itr = head.begin(); itr != head.end(); ++itr)
	{
		if ((* itr)->comp_val_pair)	// the component pointed by itr is not NULL, (* itr)== Component_item *
		{
			cout << "component " << (* itr)->index << ", created at level " 
				 << _component_infor_stack[(* itr)->index]->level
				 << ", size = " << _component_infor_stack[(* itr)->index]->var_set->size()
				 //<< ", size = " << _component_infor_stack[(* itr)->index]->num_clause
				 //<< (* itr)->comp_val_pair->f.size() 
				 << (_component_infor_stack[(* itr)->index]->active ? ", active " : ", inactive ") 
				 << (_component_infor_stack[(* itr)->index]->changed ? ", changed " : ", unchanged ") 
				 << ", hash_index = " << _component_infor_stack[(* itr) ->index]->hash_index 
				 << ", left_done = " << _component_infor_stack[(* itr) ->index]->left_branch_done
				 <<	", sat_prob = " << _component_infor_stack[(* itr)->index]->sat_probability 
				 << ", left = " << _component_infor_stack[(* itr)->index]->left_sat_prob
				 << ", right = " << _component_infor_stack[(* itr)->index]->right_sat_prob
				 << ", ancestor = " << _component_infor_stack[(* itr)->index]->ancestor_index 
				 << ", num_children = " << _component_infor_stack[(* itr)->index]->num_children
				 << ", largest_svar = " << _component_infor_stack[(* itr)->index]->largest_svar 
				 << " has degree = " << _component_infor_stack[(* itr)->index]->largest_degree;
				 //<< ", largest_distance = " << _component_infor_stack[(* itr)->index]->largest_distance << endl;
			
			/*vector<int> & var_set = * _component_infor_stack[(* itr)->index]->var_set;
			cout << endl << "var_set: ";
			for (vector<int>::iterator itra = var_set.begin(); itra != var_set.end(); ++itra)
				cout << (*itra) << ", ";
			cout << endl;*/
			cout << ", num_cached_children = " 
				 << _component_infor_stack[(* itr)->index]->num_cached_children << endl;
			if (_component_infor_stack[(* itr)->index]->num_cross_implications > 0)
				cout << "changed by " << _component_infor_stack[(* itr)->index]->num_cross_implications
					 << " cross implications" << endl;
			vector<child_to_remove *> & child_list = (*itr)->comp_val_pair->cached_child_list;
			vector<int> & branched_child_list = _component_infor_stack[(* itr)->index]->branched_child_list;

			for (int k = child_list.size() - 1, j = 0; k >= 0; --k)
			{
				cout << "cached child " << ++j << " : " 
					 << " hash_index = " << child_list[k]->hash_index
					 << " seq_num = " << child_list[k]->sequence_number << endl;
			}
			//if (!branched_child_list.empty())
			//	cout << "branched_child_list.size() = " << branched_child_list.size() << endl;
			for (int k = branched_child_list.size() - 1, j = 0; k >= 0; --k)
			{
				cout << "branched child " << ++j << " : " 
					 << " hash_index = " << _component_infor_stack[branched_child_list[k]]->hash_index << endl;
			}

			//if (_component_infor_stack[(* itr)->index]->num_children == 0)
			//	hashtable->print_formula((* itr)->comp_val_pair->f);
		}	// end if
		cout << "****************************************************" << endl;
	}	// end for
}
#else
void CSolver::dump_components(Components & head)	// added by sang
{
	//int comp_index = 1;
	if (head.empty())
	{
		cout << "empty components" << endl << "****************************************************" << endl;
		return;
	}
	
	cout << endl << "****************************************************" << endl;
	cout << "_component_infor_stack[0]: " << endl
		 << "size = " << num_clauses()
		 << ", zero_flag = " << _component_infor_stack[0]->sat_probability.zero_flag << " "
		 << ", sat_probability = ";
	mpz_out_str(stdout, 10, _component_infor_stack[0]->sat_probability.numerator);
	cout << " / 2^" << _component_infor_stack[0]->sat_probability.denominator
		 << ", left_sat_prob = "; 
	mpz_out_str(stdout, 10, _component_infor_stack[0]->left_sat_prob.numerator);
	cout << " / 2^" << _component_infor_stack[0]->left_sat_prob.denominator << endl
		 << "right_sat_prob = ";
	mpz_out_str(stdout, 10, _component_infor_stack[0]->right_sat_prob.numerator);
	cout << " / 2^" << _component_infor_stack[0]->right_sat_prob.denominator
		 //<< (_component_infor_stack[0]->active ? "active " : ", inactive ") 
		 << ", num_children = " << _component_infor_stack[0]->num_children << endl;
	cout << "****************************************************" << endl;

	for (Components::iterator itr = head.begin(); itr != head.end(); ++itr)
	{
		if ((* itr)->comp_val_pair)	// the component pointed by itr is not NULL, (* itr)== Component_item *
		{
			double temp = mpz_get_d(_component_infor_stack[(* itr)->index]->sat_probability.numerator);
				for (int k = 0; k < _component_infor_stack[(* itr)->index]->sat_probability.denominator; ++k)
					temp *= 0.5;
			double left = mpz_get_d(_component_infor_stack[(* itr)->index]->left_sat_prob.numerator);
				for (int k = 0; k < _component_infor_stack[(* itr)->index]->left_sat_prob.denominator; ++k)
					left *= 0.5;
			cout << "component " << (* itr)->index << ", created at level " 
				 << _component_infor_stack[(* itr)->index]->level
				 << ", size = " << (* itr)->comp_val_pair->f.size() 
				 << (_component_infor_stack[(* itr)->index]->active ? ", active " : ", inactive ") 
				 << ", hash_index = " << _component_infor_stack[(* itr) ->index]->hash_index << endl
				 << "left_done = " << _component_infor_stack[(* itr)->index]->left_branch_done
 				 << ", zero_flag = " << _component_infor_stack[(* itr)->index]->sat_probability.zero_flag
				 << ", left_zero = " << _component_infor_stack[(* itr)->index]->left_sat_prob.zero_flag
				 << ", right_zero = " << _component_infor_stack[(* itr)->index]->right_sat_prob.zero_flag << endl
				 <<	", sat_prob = " << temp
			//mpz_out_str(stdout, 10, _component_infor_stack[(* itr)->index]->sat_probability.numerator); 
			//cout << " / 2^" << _component_infor_stack[(* itr)->index]->sat_probability.denominator
				 << ", left = " << left
			//mpz_out_str(stdout, 10, _component_infor_stack[(* itr)->index]->left_sat_prob.numerator);
			//cout << " / 2^" << _component_infor_stack[(* itr)->index]->left_sat_prob.denominator
				 << ", right = "; 
			mpz_out_str(stdout, 10, _component_infor_stack[(* itr)->index]->right_sat_prob.numerator);
			cout << " / 2^" << _component_infor_stack[(* itr)->index]->right_sat_prob.denominator
				 << ", ancestor = " << _component_infor_stack[(* itr)->index]->ancestor_index 
				 << ", num_children = " << _component_infor_stack[(* itr)->index]->num_children
				 << "largest_svar = " << _component_infor_stack[(* itr)->index]->largest_svar 
				 << " has degree = " << _component_infor_stack[(* itr)->index]->largest_degree;
				 //<< ", largest_distance = " << _component_infor_stack[(* itr)->index]->largest_distance << endl;
			
			//set<int> & var_set = * _component_infor_stack[(* itr)->index]->var_set;
			//cout << "var_set: ";
			//for (set<int>::iterator itra = var_set.begin(); itra != var_set.end(); ++itra)
			//cout << (*itra) << ", ";
			//cout << endl;

			cout << "num_cached_children = " << _component_infor_stack[(* itr)->index]->num_cached_children << endl;

			vector<child_to_remove *> & child_list = (*itr)->comp_val_pair->cached_child_list;
			vector<int> & branched_child_list = _component_infor_stack[(* itr)->index]->branched_child_list;

			for (int k = child_list.size() - 1, j = 0; k >= 0; --k)
			{
				cout << "cached child " << ++j << " : " 
					 << " hash_index = " << child_list[k]->hash_index
					 << " seq_num = " << child_list[k]->sequence_number << endl;
			}
			for (int k = branched_child_list.size() - 1, j = 0; k >= 0; --k)
			{
				cout << "branched child " << ++j << " : " 
					 << " hash_index = " << _component_infor_stack[branched_child_list[k]]->hash_index << endl;
			}

			//if (_component_infor_stack[(* itr)->index]->num_children == 0)
			//	hashtable->print_formula((* itr)->comp_val_pair->f);
		}	// end if
		cout << "****************************************************" << endl;
	}	// end for
}
#endif



#ifdef BIG_NUM
int CSolver::percolate_up(BigNum & sat_prob, int gid)
// return the level that percolate_up finally stops at
{
	assert (_component_infor_stack[gid]->active);
	int last_branch_comp = gid;
	int branch_level = dlevel();
	int backlevel = dlevel();	// set a max value
	//long double sat_prob = sat_probability;
	unsigned hash_index = 0;
	bool left_branch_done = false;
	bool right_branch_done = false;
	//left_branch_done = (_component_infor_stack[last_branch_comp]->sat_probability > -2);
	left_branch_done = _component_infor_stack[last_branch_comp]->left_branch_done;
	right_branch_done = (_component_infor_stack[last_branch_comp]->num_children == 1);

	if (_component_infor_stack[last_branch_comp]->changed)
	{
		if (sat_prob.zero_flag)
		{
			left_branch_done = right_branch_done = true;	// there is only one branch for changed components
			_component_infor_stack[last_branch_comp]->sat_probability.zero_flag = true;	// one branch UNSAT means whole UNSAT
		}
		else 
		{
			if (!_component_infor_stack[last_branch_comp]->sat_probability.zero_flag)
				//_component_infor_stack[last_branch_comp]->sat_probability *= sat_prob;
				BigNum::mul(_component_infor_stack[last_branch_comp]->sat_probability, 
							_component_infor_stack[last_branch_comp]->sat_probability, sat_prob);
			else
				//_component_infor_stack[last_branch_comp]->sat_probability = sat_prob;
			{
				mpz_set(_component_infor_stack[last_branch_comp]->sat_probability.numerator, 
						sat_prob.numerator);
				_component_infor_stack[last_branch_comp]->sat_probability.denominator 
					= sat_prob.denominator;
				_component_infor_stack[last_branch_comp]->sat_probability.zero_flag 
					= sat_prob.zero_flag;
			}
			// condition if (_component_infor_stack[last_branch_comp]->num_children == 0) added
			if (_component_infor_stack[last_branch_comp]->num_children == 0)
				right_branch_done = true;
			if (right_branch_done)
			{
				left_branch_done = true;	// only one branch, right_done means left_done too, both done
				/*mpz_set(sat_prob.numerator, _component_infor_stack[last_branch_comp]->sat_probability.numerator);
				sat_prob.zero_flag = _component_infor_stack[last_branch_comp]->sat_probability.zero_flag;
				sat_prob.denominator = _component_infor_stack[last_branch_comp]->sat_probability.denominator;*/
				_component_infor_stack[last_branch_comp]->sat_probability.denominator += 
								_component_infor_stack[last_branch_comp]->num_cross_implications;
				//sat_prob.denominator += _component_infor_stack[last_branch_comp]->num_cross_implications;
				//for (int i = 0; i < _component_infor_stack[last_branch_comp]->num_cross_implications; ++i)
				//	_component_infor_stack[last_branch_comp]->sat_probability *= 0.5;
				//if (flag)
				//	cout << "changed component " << last_branch_comp << " divided by "
				//		 << "2^" << _component_infor_stack[last_branch_comp]->num_cross_implications << endl;
			}
		}	// end else
	}	
	else if (sat_prob.zero_flag)
		{
		// sat_prob < 0 means an UNSAT component encountered, that branch must be set done, no matter
		// other components of that branch done or not
			if (left_branch_done)
				right_branch_done = true;	// right branch UNSAT
			else // could be removed, because it will backtrack to this level
			{	// left branch UNSAT, left_branch_done = true; 
				_component_infor_stack[last_branch_comp]->left_sat_prob.zero_flag = true;// means left_branch UNSAT
				_component_infor_stack[last_branch_comp]->left_branch_done = true;
				//if (flag)
				//	cout << "backlevel = " << backlevel << " returned from percolate_up" << endl;
				return backlevel; 			
			}
		}	// end if
	else if (_component_infor_stack[last_branch_comp]->num_children == 0)
	//if (_component_infor_stack[last_branch_comp]->num_children == 0)	// else removed, for this must be checked
	{
		right_branch_done = true;
		if (!left_branch_done)
			_component_infor_stack[last_branch_comp]->left_branch_done = true;
	}	// end else if

	if (_component_infor_stack[last_branch_comp]->num_children > 0)	// don't decrement 0 to -1 !
		--_component_infor_stack[last_branch_comp]->num_children;

	while(left_branch_done && right_branch_done)
	{	
		assert (_component_infor_stack[last_branch_comp]-> active);
		Component_information & comp_infor = * _component_infor_stack[last_branch_comp];
		if (!_component_infor_stack[last_branch_comp]->changed)
		{
			if (!sat_prob.zero_flag)	//	sat_prob > 0, right branch SAT
			{
				if (!comp_infor.right_sat_prob.zero_flag) // multiply the component's right_sat_prob if it is > 0
					BigNum::mul(comp_infor.right_sat_prob, comp_infor.right_sat_prob, sat_prob);
			
				else	// copy sat_prob to comp_infor.right_sat_prob
				{
					mpz_set(comp_infor.right_sat_prob.numerator, sat_prob.numerator);
					comp_infor.right_sat_prob.denominator = sat_prob.denominator;
					comp_infor.right_sat_prob.zero_flag = sat_prob.zero_flag;
				}
				int num_implications = _num_implication_stack[branch_level] - 1; // bug, comp_infor.level -1 changed!
				comp_infor.right_sat_prob.denominator += num_implications;	// probability divided by 2 for num_implications times

				if (!comp_infor.left_sat_prob.zero_flag)
				{
					BigNum::add(sat_prob, comp_infor.left_sat_prob, comp_infor.right_sat_prob);
					//BigNum::add(comp_infor.sat_probability, comp_infor.left_sat_prob, comp_infor.right_sat_prob);
					++sat_prob.denominator;	// * 0.5
					//++comp_infor.sat_probability.denominator;	// * 0.5
				}
				else	// left branch UNSAT
				{
					//mpz_set(comp_infor.sat_probability.numerator, comp_infor.right_sat_prob.numerator);
					mpz_set(sat_prob.numerator, comp_infor.right_sat_prob.numerator);
					sat_prob.denominator = comp_infor.right_sat_prob.denominator + 1;
					sat_prob.zero_flag = false; // left UNSAT & right SAT
				}
			}
			else // right branch UNSAT
			{
				if (!comp_infor.left_sat_prob.zero_flag)
				{
					//if (flag)
					//	cout << "component " << last_branch_comp << " right UNSAT, left SAT" << endl;
					//mpz_set(comp_infor.sat_probability.numerator, comp_infor.left_sat_prob.numerator); // ignore UNSAT right branch (-1)
					mpz_set(sat_prob.numerator, comp_infor.left_sat_prob.numerator); // ignore UNSAT right branch (-1)
					sat_prob.denominator = comp_infor.left_sat_prob.denominator + 1;
					sat_prob.zero_flag = false;	// left SAT
				}
				else
				{
					//if (flag)
					//	cout << "component " << last_branch_comp << " both UNSAT" << endl;
					//comp_infor.sat_probability.zero_flag = true;	// both left and right UNSAT
					backlevel = comp_infor.level - 1;	// at backlevel, the decison results an UNSAT component\
					cout << "changed component " << last_branch_comp << " UNSAT " << endl;
				}
			}
		}
		else if (sat_prob.zero_flag)	// last_branch_comp changed!
		{
			//comp_infor.sat_probability.zero_flag = true;	// both left and right UNSAT
			backlevel = comp_infor.level - 1;	// at backlevel, the decison results an UNSAT component
		}
		if (_component_infor_stack[last_branch_comp]->changed)
		{
			//	sat_prob = comp_infor.sat_probability;		// save sat_prob, pass it to its parent
			sat_prob.denominator = comp_infor.sat_probability.denominator;
			//sat_prob.zero_flag = comp_infor.sat_probability.zero_flag;
			mpz_set(sat_prob.numerator, comp_infor.sat_probability.numerator);
		}

		Components::iterator itr = comp_infor.comp_itr;
		Component_value_pair * & comp_val_pair = (* itr)->comp_val_pair;

		hash_index = comp_infor.hash_index;
#ifdef APPROXIMATE_HASHING
		comp_val_pair->secondary_index = comp_infor.secondary_index;
#endif
		//comp_val_pair->value = sat_prob; // save sat_prob for caching // bug removed!
		mpz_set(comp_val_pair->value.numerator, sat_prob.numerator); // save sat_prob for caching
		comp_val_pair->value.denominator = sat_prob.denominator;
		comp_val_pair->value.zero_flag =  sat_prob.zero_flag;

		int parent_index = comp_infor.ancestor_index;
		assert (_component_infor_stack[parent_index]->active);
		bool last_child;
		//if (flag)
		//	cout << "component " << last_branch_comp << " parent_index = " << parent_index << endl;

		if (sat_prob.zero_flag)
		{
			//if (flag)
			//	cout << "component " << last_branch_comp << " sat_prob = 0" << endl;
			Component_information * & parent = _component_infor_stack[parent_index];
			vector<child_to_remove *> & child_list = (* parent->comp_itr)->comp_val_pair->cached_child_list;

			for (; parent->num_cached_children > 0; --parent->num_cached_children)
			{
				if (! hashtable->removeChildren(* child_list.back()))
					cout << "Removing a child failed" << endl;
				else
				{
					//delete child_list.back();
					child_list.pop_back();
				}
			} // end for
			vector<int> cloned_child_list = parent->branched_child_list;
			last_child = (_component_infor_stack[parent_index]->num_children ==
						_component_infor_stack[parent_index]->branched_child_list.size());
			cloned_child_list.pop_back();	// .back() is the current component itself
			if (!cloned_child_list.empty())
			{
				/*if (flag)
				{
					cout << "removing branched siblings of component " << last_branch_comp << " triggered " << endl;
					cout << "num_siblings: " << cloned_child_list.size() << endl;
					//dump_components(_components);
				}*/
				remove_branched_children(cloned_child_list);
			}
			parent->branched_child_list.clear();
			parent->branched_child_list.push_back(last_branch_comp);
		}
		if (!sat_prob.zero_flag  || last_child)
		{
			if (!hashtable->set_hash_table_entry(hash_index, * comp_val_pair))
				delete comp_val_pair;

			if (comp_infor.ancestor_index > 0)
			{
				Components::iterator parent = _component_infor_stack[parent_index]->comp_itr;
				(* parent)->comp_val_pair->cached_child_list.push_back(new child_to_remove);
				child_to_remove* & child = (* parent)->comp_val_pair->cached_child_list.back();
				child->hash_index = hash_index;
				child->sequence_number = hashtable->active_entries - 1;
				// one more children cached
				++_component_infor_stack[parent_index]->num_cached_children;

				vector <int> & branched_list = _component_infor_stack[parent_index]->branched_child_list;
				for(vector<int>::iterator branched_itr = branched_list.begin();
					branched_itr != branched_list.end(); ++branched_itr)
					if ((* branched_itr) == last_branch_comp)
					{
						branched_list.erase(branched_itr);
						break;
					}
				}
			comp_val_pair = NULL; // set to NULL so that it won't be removed in back_track
		}
		else // remove all the cached children of the current component
		{
			//if (flag)
			//	cout << "value not cached, and remove siblings" << endl;
			vector<child_to_remove *> & child_list = (*itr)->comp_val_pair->cached_child_list;
			for (int i = child_list.size()-1; i >= 0; --i)
			{
				//if (flag)
				//	cout << "removing cached children of component " << last_branch_comp << endl;
				hashtable->removeChildren(* child_list[i]);
			} // end for
			child_list.clear();
			comp_infor.num_cached_children = 0;
				
			vector<int> & cloned_child_list = comp_infor.branched_child_list;
			if (!cloned_child_list.empty())
			{
				remove_branched_children(cloned_child_list);
			}
			cloned_child_list.clear();
			// clear its parent->branched_child_list
			_component_infor_stack[comp_infor.ancestor_index]->branched_child_list.clear();
		}	// end else

		_component_infor_stack[last_branch_comp]->active = false;
		delete comp_infor.var_set;
		delete comp_val_pair;
		delete (*itr);
		_components.erase(itr, ++itr); // erase the cached component from _components

		branch_level = _component_infor_stack[last_branch_comp]->level - 1;//bug removed
		last_branch_comp = comp_infor.ancestor_index;

		left_branch_done = (_component_infor_stack[last_branch_comp]->left_branch_done);
		right_branch_done = (--_component_infor_stack[last_branch_comp]->num_children == 0);

		if ((last_branch_comp == 0)) //|| (backlevel <= _uni_phased_size && _stats.num_solutions == 0))
		{
			if (right_branch_done)
			{
				//cout << "last_branch_comp == 0, exit" << endl;
				//for (int j = _stats.num_unit_clause - 1; j > 0; --j)	// newly added
				//	sat_prob = sat_prob / 2;
				//if (_stats.num_unit_clause > 1)								// bug removed, need this condition!
				//	sat_prob.denominator += _stats.num_unit_clause - 1;		// newly added
				if (_assignment_stack[0]->size() > 1)								// bug removed, need this condition!
					sat_prob.denominator += _assignment_stack[0]->size() - 1;		// newly added

				if (!_component_infor_stack[0]->left_sat_prob.zero_flag)
					BigNum::mul(_component_infor_stack[0]->left_sat_prob,
								_component_infor_stack[0]->left_sat_prob, sat_prob);
				else
				{
					mpz_set(_component_infor_stack[0]->left_sat_prob.numerator, sat_prob.numerator);
					_component_infor_stack[0]->left_sat_prob.denominator = sat_prob.denominator;
					_component_infor_stack[0]->left_sat_prob.zero_flag = sat_prob.zero_flag;
				}
				return -1;
			}
		}
		if (!_component_infor_stack[last_branch_comp]->changed)
		{
			if (sat_prob.zero_flag)
			// sat_prob < 0 means an UNSAT component encountered, that branch must be set done, no matter
			// other components of that branch done or not
				if (left_branch_done)
					right_branch_done = true;	// both branch UNSAT
				else 
				{
					_component_infor_stack[last_branch_comp]->left_sat_prob.zero_flag = true; // 
					_component_infor_stack[last_branch_comp]->left_branch_done = true;// left branch found UNSAT
					return backlevel;
				}
		}
		else	// this component changed!
		{
			if (sat_prob.zero_flag)	// the only branch of a changed component is UNSAT, which means whole comp UNSAT
			{
				_component_infor_stack[last_branch_comp]->sat_probability.zero_flag = true;
				left_branch_done = right_branch_done = true;
				//cout << "changed component " << last_branch_comp << " UNSAT " << endl;
			}
			else
			{
				if (!_component_infor_stack[last_branch_comp]->sat_probability.zero_flag)
				//_component_infor_stack[last_branch_comp]->sat_probability *= sat_prob;
					BigNum::mul(_component_infor_stack[last_branch_comp]->sat_probability, 
							_component_infor_stack[last_branch_comp]->sat_probability, sat_prob);
				else //_component_infor_stack[last_branch_comp]->sat_probability = sat_prob;
				{
					mpz_set(_component_infor_stack[last_branch_comp]->sat_probability.numerator, 
							sat_prob.numerator);
					_component_infor_stack[last_branch_comp]->sat_probability.denominator 
						= sat_prob.denominator;
					_component_infor_stack[last_branch_comp]->sat_probability.zero_flag 
						= sat_prob.zero_flag;
				}
				if (right_branch_done)
				{
					left_branch_done = true; // only one branch, right_done(SAT) means left_done too, both done
					_component_infor_stack[last_branch_comp]->sat_probability.denominator += 
						_component_infor_stack[last_branch_comp]->num_cross_implications;
					//for (int i = 0; i < _component_infor_stack[last_branch_comp]->num_cross_implications; ++i)
					//	_component_infor_stack[last_branch_comp]->sat_probability *= 0.5;	// fixed by adding this! 
					//cout << "changed component " << last_branch_comp << " divided by "
					//	 << "2^" << _component_infor_stack[last_branch_comp]->num_cross_implications << endl;
				}
			}
		}
	} // end while
	// at this point, the component must be !left_branch done or !right_branch_done
	Component_information & comp_inf = * _component_infor_stack[last_branch_comp];
	assert (!sat_prob.zero_flag); // since we started with something SAT, the probability is always > 0
	//if (sat_prob > 0)
	if (!_component_infor_stack[last_branch_comp]->changed)
	{
		if (left_branch_done)
		{
			//if (sat_prob > 0) // this condition must be true to quit the while loop
			if (!comp_inf.right_sat_prob.zero_flag) // some other SAT component of right branch done
				BigNum::mul(comp_inf.right_sat_prob, comp_inf.right_sat_prob, sat_prob);
			else
			{	// no other component of right branch done
				mpz_set(comp_inf.right_sat_prob.numerator, sat_prob.numerator);
				comp_inf.right_sat_prob.denominator = sat_prob.denominator;
				comp_inf.right_sat_prob.zero_flag = false; 
			}
		}
		else // left_branch not done yet
		{
			if (!comp_inf.left_sat_prob.zero_flag) // some other SAT component of right branch done
				BigNum::mul(comp_inf.left_sat_prob, comp_inf.left_sat_prob, sat_prob);
			else
			{
				// no other left branch component done yet
				mpz_set(comp_inf.left_sat_prob.numerator, sat_prob.numerator);
				comp_inf.left_sat_prob.denominator = sat_prob.denominator;
				comp_inf.left_sat_prob.zero_flag = false;
			}
			if (right_branch_done)	// tricky here:
			// right_branch_done actually means num_children == 0 means left_branch_done
			{
				comp_inf.left_branch_done = true; // comp_inf.sat_probability > -2 means left_branch_done
				int num_implications = _num_implication_stack[branch_level] - 1; // bug, com_inf.level -1 changed
				comp_inf.left_sat_prob.denominator += num_implications;	// probability divided by 2 for num_implications times
			}	// end if 
		}	// end else
	}
	//cout << &sat_prob.numerator << " cleared, decision = " << _stats.num_decisions << endl;
	//mpz_clear(sat_prob.numerator);
	return backlevel;	// this is the level where some component is UNSAT
}
#else	// BIG_NUM undefined
int CSolver::percolate_up(long double sat_prob, int gid)
// return the level that percolate_up finally stops at
{
	assert (_component_infor_stack[gid]->active);
	/*if (flag)
	{
		cout << "in percolate_up, sat_prob = " << sat_prob << ", gid = " << gid << endl;
		dump_components(_components);
	}*/
	int last_branch_comp = gid;
	int branch_level = dlevel();
	int backlevel = dlevel();	// set a max value
	unsigned hash_index = 0;
	bool left_branch_done = _component_infor_stack[last_branch_comp]->left_branch_done;
	bool right_branch_done = (_component_infor_stack[last_branch_comp]->num_children == 1);

	//if (flag)
	//	cout << "left_done = " << left_branch_done << " right_done = " <<  right_branch_done 
	//		 << ", changed = " << _component_infor_stack[last_branch_comp]->changed << endl;

	if (_component_infor_stack[last_branch_comp]->changed)
	{
		//assert (sat_prob > 0);	
		// changed component must not result UNSAT in one step, but it can be a cache hit! bug removed
		//if (sat_prob < 0)
		//{
		//	cout << "assert (sat_prob > 0) fails with sat_prob = " << sat_prob
		//		 << ", at decision " << _stats.num_decisions << endl;
		//	exit(0);
		//}
		if (sat_prob < -0.5)
		{
			left_branch_done = right_branch_done = true;	// there is only one branch for changed components
			_component_infor_stack[last_branch_comp]->sat_probability = -1;	// one branch UNSAT means whole UNSAT
		}
		else 
		{
			if (_component_infor_stack[last_branch_comp]->sat_probability > -0.5)
				_component_infor_stack[last_branch_comp]->sat_probability *= sat_prob;
			else
				_component_infor_stack[last_branch_comp]->sat_probability = sat_prob;
			if (_component_infor_stack[last_branch_comp]->num_children == 0)
				right_branch_done = true;
			if (right_branch_done)
			{
				_component_infor_stack[last_branch_comp]->left_branch_done = true;
				left_branch_done = true;	// only one branch, right_done means left_done too, both done
				//for (int i = 0; i < _component_infor_stack[last_branch_comp]->num_cross_implications; ++i)
				//	_component_infor_stack[last_branch_comp]->sat_probability *= 0.5;
					_component_infor_stack[last_branch_comp]->sat_probability *= 
						_component_infor_stack[last_branch_comp]->cross_implication_weight;
				//if (flag)
				//	cout << "changed component " << last_branch_comp << " divided by "
				//		 << "2^" << _component_infor_stack[last_branch_comp]->num_cross_implications << endl;
			}
		}
	}
	else if (sat_prob < -0.5)
		{
		// sat_prob < 0 means an UNSAT component encountered, that branch must be set done, no matter
		// other components of that branch done or not
			if (left_branch_done)
				right_branch_done = true;	// right branch UNSAT
			else // could be removed, because it will backtrack to this level
			{	// left branch UNSAT, left_branch_done = true; 
				_component_infor_stack[last_branch_comp]->left_sat_prob = -1;	// means left_branch UNSAT
				_component_infor_stack[last_branch_comp]->left_branch_done = true;
				//if (flag) 
				//	cout << "returned in percolate_up, backlevel = " << backlevel << endl;
				return backlevel; 			
			}
		}	// end if
	else if (_component_infor_stack[last_branch_comp]->num_children == 0)
	//if (_component_infor_stack[last_branch_comp]->num_children == 0)	// else removed, for this must be checked
	{
		right_branch_done = true;
		//if (!left_branch_done)
		_component_infor_stack[last_branch_comp]->left_branch_done = true;
	}	// end else if

	if (_component_infor_stack[last_branch_comp]->num_children > 0)	// don't decrement 0 to -1 !
		--_component_infor_stack[last_branch_comp]->num_children;

	while(left_branch_done && right_branch_done)
	{	
		//if (flag) 
		//	cout << "entered while, sat_prob = " << sat_prob << ", last_branch = " << last_branch_comp << endl;

		assert (_component_infor_stack[last_branch_comp]-> active);
		Component_information & comp_infor = * _component_infor_stack[last_branch_comp];

		if (!_component_infor_stack[last_branch_comp]->changed)
		{
			if (sat_prob > -0.5)
			{
				if (comp_infor.right_sat_prob > -0.5)
					// multiply the component's right_sat_prob if it is > 0
					comp_infor.right_sat_prob *= sat_prob;
				else
					comp_infor.right_sat_prob = sat_prob;

				//int num_implications = _num_implication_stack[branch_level] - 1; // bug, comp_infor.level -1 changed!
				//for (int i = 0; i < num_implications; ++i)
				//	comp_infor.right_sat_prob *= 0.5;	// probability divided by 2 for num_implications times

				vector<int> & literals = * _assignment_stack[branch_level];
				for(int i = literals.size() - 1; i >= 0; --i)
				{
					if (!variable(literals[i] >> 1).cross_flag
						&& !variable(literals[i] >> 1).internal)
					if (literals[i] & 0x1)
						comp_infor.right_sat_prob *= variable(literals[i] >> 1).neg_weight; // negtive literal
					else
						comp_infor.right_sat_prob *= variable(literals[i] >> 1).pos_weight; // positive literal
						//	sat_prob = sat_prob * 0.5;
				}

				if (comp_infor.left_sat_prob > -0.5)
					comp_infor.sat_probability = comp_infor.left_sat_prob + comp_infor.right_sat_prob;
					//comp_infor.sat_probability = (comp_infor.left_sat_prob + comp_infor.right_sat_prob) * 0.5;
				else	// left branch UNSAT
					comp_infor.sat_probability = comp_infor.right_sat_prob;
					//comp_infor.sat_probability = comp_infor.right_sat_prob * 0.5;
			}
			else // right branch UNSAT
			{
				if (comp_infor.left_sat_prob > -0.5)
					comp_infor.sat_probability = comp_infor.left_sat_prob;	// 0.5 removed due to counting!
					//comp_infor.sat_probability = comp_infor.left_sat_prob * 0.5; // ignore UNSAT right branch (-1)
				else
				{
					comp_infor.sat_probability = -1;	// both left and right UNSAT
					backlevel = comp_infor.level - 1;	// at backlevel, the decison results an UNSAT component
				}
			}
		}	// end if (!_component_infor_stack[last_branch_comp]->changed)
		else if (sat_prob < -0.5)	// last_branch_comp changed!
		{
			comp_infor.sat_probability = -1;	// the only branch UNSAT
			//backlevel = _component_infor_stack[comp_infor.ancestor_index]->level;	// bug removed, no -1
			backlevel = comp_infor.level - 1;	// bug in the above removed
			//cout << "changed component " << last_branch_comp << " UNSAT " << endl;
		}

		sat_prob = comp_infor.sat_probability;		// save sat_prob, pass it to its parent

		//if (_component_infor_stack[last_branch_comp]->tag)
		//{
		//	cout << "level " << (_component_infor_stack[last_branch_comp]->level-1)
		//		 << ", component " << last_branch_comp
		//		 << "(" << _component_infor_stack[last_branch_comp]->num_clause
		//		 << ") : " << sat_prob << endl;
		//}

		Components::iterator itr = comp_infor.comp_itr;
		Component_value_pair * & comp_val_pair = (* itr)->comp_val_pair;

		hash_index = comp_infor.hash_index;
		comp_val_pair->value = sat_prob; // save sat_prob for caching
#ifdef APPROXIMATE_HASHING
		comp_val_pair->secondary_index = comp_infor.secondary_index;
#endif
		int parent_index = comp_infor.ancestor_index;
		//if (flag) 
		//	cout << "parent = " << parent_index << endl;
		assert (_component_infor_stack[parent_index]->active);

		bool last_child;
		if (sat_prob < -0.5 && parent_index > 0) // remove siblings of this UNSAT component
		{
			Component_information * & parent = _component_infor_stack[parent_index];
			vector<child_to_remove *> & child_list = (* parent->comp_itr)->comp_val_pair->cached_child_list;

			for (; parent->num_cached_children > 0; --parent->num_cached_children)
			{
				if (! hashtable->removeChildren(* child_list.back()))
					cout << "Removing a child failed" << endl;
				else
				{
					//delete child_list.back();
					child_list.pop_back();
				}
			} // end for
			vector<int> cloned_child_list = parent->branched_child_list;
			last_child = (_component_infor_stack[parent_index]->num_children ==
							_component_infor_stack[parent_index]->branched_child_list.size());
			//if (flag) cout << "last_child = " << last_child << endl;
			cloned_child_list.pop_back();	// .back() is the current component itself
			if (!cloned_child_list.empty())
			{
				//if (flag)
				//{
				//	cout << "removing branched siblings of component " << last_branch_comp << " triggered " << endl;
				//	cout << "num_siblings: " << cloned_child_list.size() << endl;
					//dump_components(_components);
				//}
				remove_branched_children(cloned_child_list);
			}
			parent->branched_child_list.clear();
			parent->branched_child_list.push_back(last_branch_comp);
		}
		if (sat_prob > -0.5 || last_child)
		{
			//if (flag)
			//{
			//	cout << "component " << last_branch_comp << " cached at " 
			//		 << hash_index << ", with sat_prob = " << sat_prob << endl;
			//}
			/*if (_component_infor_stack[last_branch_comp]->tag)
			{
				cout << "level " << (_component_infor_stack[last_branch_comp]->level-1)
					 << ", component " << last_branch_comp
					 //<< "(" << _component_infor_stack[last_branch_comp]->num_clause
					 << "(" << _component_infor_stack[last_branch_comp]->var_set->size()
					 << ", " << (_component_infor_stack[last_branch_comp]->largest_svar >> 1)
					 << ") : " << sat_prob << endl;
			}*/
			if (!hashtable->set_hash_table_entry(hash_index, * comp_val_pair))	// cache it
				delete comp_val_pair;	// if already cached, delete it
			if (comp_infor.ancestor_index > 0)
			{
				Components::iterator parent = _component_infor_stack[parent_index]->comp_itr;
				(* parent)->comp_val_pair->cached_child_list.push_back(new child_to_remove);
				child_to_remove* & child = (* parent)->comp_val_pair->cached_child_list.back();
				child->hash_index = hash_index;
				child->sequence_number = hashtable->active_entries - 1;
				// one more children cached
				++_component_infor_stack[parent_index]->num_cached_children;

				// since last child is now cached (in cached_child_list), removed it from branched_child_list
				vector <int> & branched_list = _component_infor_stack[parent_index]->branched_child_list;
				for(vector<int>::iterator branched_itr = branched_list.begin();
					branched_itr != branched_list.end(); ++branched_itr)
					if ((* branched_itr) == last_branch_comp)
					{
						branched_list.erase(branched_itr);
						break;
					}
			}
#ifdef APPROXIMATE_HASHING
			if (!comp_val_pair->f.empty())	// for some reason, not all clauses are deleted in decide_next_branch
			{
				formula temp;	// temp is empty
				comp_val_pair->f.swap(temp);	// re-claim all memory of f, temp will be re-claimed when exiting this function
			}
#endif
			comp_val_pair = NULL; // set to NULL so that it won't be removed in back_track
		}
		else // remove all the cached children of the current component
		{
			//if (flag) cout << "component " << last_branch_comp << " is not the last child, -1 not cached" << endl;
			vector<child_to_remove *> & child_list = (*itr)->comp_val_pair->cached_child_list;
			for (int i = child_list.size()-1; i >= 0; --i)
			{
				//if (flag)
				//	cout << "removing cached children of component " << last_branch_comp << endl;
				hashtable->removeChildren(* child_list[i]);
			} // end for
			child_list.clear();
			comp_infor.num_cached_children = 0;
				
			vector<int> & cloned_child_list = comp_infor.branched_child_list;
			if (!cloned_child_list.empty())
			{
				remove_branched_children(cloned_child_list);
			}
			cloned_child_list.clear();
			// clear its parent->branched_child_list
			_component_infor_stack[comp_infor.ancestor_index]->branched_child_list.clear();
		}	// end else

		_component_infor_stack[last_branch_comp]->active = false;
		delete comp_infor.var_set;
		delete comp_val_pair;
		delete (*itr);
		_components.erase(itr, ++itr); // erase the cached component from _components

		branch_level = _component_infor_stack[last_branch_comp]->level - 1;//bug removed
		last_branch_comp = comp_infor.ancestor_index;

		left_branch_done = (_component_infor_stack[last_branch_comp]->left_branch_done);
		right_branch_done = (--_component_infor_stack[last_branch_comp]->num_children == 0);

		if ((last_branch_comp == 0)) //|| (backlevel <= _uni_phased_size && _stats.num_solutions == 0))
		{
			if (right_branch_done)	// right_branch_done means left done, for component 0 only has left branch
			{
				//if (flag)
				//	cout << "last_branch_comp == 0, exit" << endl;				
				//for (int j = _stats.num_unit_clause - 1; j > 0; --j)	// newly added  
				vector<int> & literals = * _assignment_stack[0];
				for(int i = literals.size() - 1; i >= 0; --i)
				{
					if (literals[i] & 0x1)
						sat_prob *= variable(literals[i] >> 1).neg_weight; // negtive literal
					else
						sat_prob *= variable(literals[i] >> 1).pos_weight; // positive literal
				}
				//for (int j = _assignment_stack[0]->size() - 1; j > 0; --j)
				//	sat_prob = sat_prob *= 0.5;
				if (_component_infor_stack[0]->left_sat_prob > 0)
					_component_infor_stack[0]->left_sat_prob *= sat_prob;
				else
					_component_infor_stack[0]->left_sat_prob = sat_prob;
				//if (flag)
				//	cout << "component 0 has sat_prob = " << _component_infor_stack[0]->left_sat_prob << endl;
				return -1;
			}
		}
		if (!_component_infor_stack[last_branch_comp]->changed)
		{
			if (sat_prob < -0.5)
			// sat_prob < 0 means an UNSAT component encountered, that branch must be set done, no matter
			// other components of that branch done or not
				if (left_branch_done)
					right_branch_done = true;	// both branch UNSAT
				else 
				{
					_component_infor_stack[last_branch_comp]->left_sat_prob = -1; // 
					_component_infor_stack[last_branch_comp]->left_branch_done = true;// left branch found UNSAT
					//if (flag) cout << "returned in percolate_up, backlevel = " << backlevel << endl;
					return backlevel;
				}
		}
		else	// this component changed!
		{
			if (sat_prob < -0.5)	// the only branch of a changed component is UNSAT, which means whole comp UNSAT
			{
				_component_infor_stack[last_branch_comp]->sat_probability = -1;
				left_branch_done = right_branch_done = true;
				//cout << "changed component " << last_branch_comp << " UNSAT " << endl;
			}
			else
			{
				if (_component_infor_stack[last_branch_comp]->sat_probability > -0.5)
					_component_infor_stack[last_branch_comp]->sat_probability *= sat_prob;
				else
					_component_infor_stack[last_branch_comp]->sat_probability = sat_prob;
				if (right_branch_done)
				{
					left_branch_done = true; // only one branch, right_done(SAT) means left_done too, both done
					_component_infor_stack[last_branch_comp]->sat_probability *= 
						_component_infor_stack[last_branch_comp]->cross_implication_weight;
					//for (int i = 0; i < _component_infor_stack[last_branch_comp]->num_cross_implications; ++i)
						//_component_infor_stack[last_branch_comp]->sat_probability *= 0.5;	// fixed by adding this! 
					//cout << "changed component " << last_branch_comp << " divided by "
					//	 << "2^" << _component_infor_stack[last_branch_comp]->num_cross_implications << endl;
				}
			}
		}
	} // end while
	// at this point, the component must be !left_branch done or !right_branch_done
	Component_information & comp_inf = * _component_infor_stack[last_branch_comp];
	assert (sat_prob > -0.5); // since we started with something SAT, the probability is always > 0
	/*if (sat_prob < 0)
	{
		cout << "assert (sat_prob > 0); failed, decision = " << _stats.num_decisions << endl;
	}*/
	if (!_component_infor_stack[last_branch_comp]->changed)
	{
		if (left_branch_done)
		{
			//if (sat_prob > 0) // this condition must be true to quit the while loop
			if (comp_inf.right_sat_prob > -0.5) // some other SAT component of right branch done
				comp_inf.right_sat_prob = comp_inf.right_sat_prob * sat_prob;
			else 
				comp_inf.right_sat_prob = sat_prob; // no other component of right branch done					
		}
		else // left_branch not done yet
		{
			if (comp_inf.left_sat_prob > -0.5) // some other SAT component of right branch done
				comp_inf.left_sat_prob = comp_inf.left_sat_prob * sat_prob;
			else
				comp_inf.left_sat_prob = sat_prob;	// no other left branch component done yet
			if (right_branch_done)	// tricky here:
			// right_branch_done actually means num_children == 0 means left_branch_done
			{
				comp_inf.left_branch_done = true; // comp_inf.sat_probability > -2 means left_branch_done
				vector<int> & literals = * _assignment_stack[branch_level];
				for(int i = literals.size() - 1; i >= 0; --i)
				{
					if (!variable(literals[i] >> 1).cross_flag)
					if (literals[i] & 0x1)
						comp_inf.left_sat_prob *= variable(literals[i] >> 1).neg_weight; // negtive literal
					else
						comp_inf.left_sat_prob *= variable(literals[i] >> 1).pos_weight; // positive literal
						//	sat_prob = sat_prob * 0.5;
				}
				//int num_implications = _num_implication_stack[branch_level] - 1; // bug, com_inf.level -1 changed
				//for (int i = 0; i < num_implications; ++i)
				//	comp_inf.left_sat_prob *= 0.5;	// probability divided by 2 for num_implications times
			}	// end if 
		}	// end else
	}
	/*else if (last_branch_comp != gid)	// need to set sat_probability if the current component is different
	{
		if (comp_inf.sat_probability > 0)
			comp_inf.sat_probability *= sat_prob;
		else
			comp_inf.sat_probability = sat_prob;
	}*/
	/*if (flag) 
	{
		cout << "returned in percolate_up, backlevel = " << backlevel << endl;
		dump_components(_components);
	}*/
	return backlevel;	// this is the level where some component is UNSAT
}
#endif	// end ifdef BIG_NUM


void CSolver::dump_branch_infor_stack(void)
{
	cout << "branch_infor_stack" << endl;
	for (int i = 0; i <= dlevel(); ++i)
	{
		cout << "(" << i << ": " << _branch_infor_stack[i]->gid //<< ") ";
			 << ", " << _branch_infor_stack[i]->num_new_children 
			 << ", " <<  _branch_infor_stack[i]->num_children_of_changed << ") ";
		//cout << "level " << i << ": gid = " << _branch_infor_stack[i]->gid
			 //<< ", num_new_children = " << _branch_infor_stack[i]->num_new_children << endl;
	}
	cout << endl;
	return;
}


void CSolver::dump_cross_implications(void)
{
	cout << "cross_implications: ";
	for (int i = 0; i < cross_implication_list.size(); ++i)
	{
		cout << "(" << cross_implication_list[i]->level
			 << ", " << cross_implication_list[i]->vid
			 << ", " << cross_implication_list[i]->marked << ") ";
	}
	cout << endl;
	return;
}


void CSolver::remove_branched_children(vector<int> & branched_child_list)
{
	vector<int> & branched_child_stack = branched_child_list;
	Component_information * current;
	int top_child;

	while(!branched_child_stack.empty())
	{
		top_child = branched_child_stack.back();
		branched_child_stack.pop_back();
		current = _component_infor_stack[top_child];
		vector <child_to_remove *> & cached_child_list = 
													(* current->comp_itr)->comp_val_pair->cached_child_list;		 
		for (int i = cached_child_list.size()-1; i >= 0; --i)
		{
			//cout << "num_decisions = " << _stats.num_decisions << endl;
			//hashtable->removeChildren(* cached_child_list.back());	// bug removed!
			hashtable->removeChildren(* cached_child_list[i]);	// remove all cached children
		}	// end for
		cached_child_list.clear();

		// DFS removing all branched children
		for (int j = current->branched_child_list.size()-1; j >= 0; --j)
		{
			branched_child_stack.push_back(current->branched_child_list[j]);
		}
		current->branched_child_list.clear();
	} // end while
	return;
}





bool CSolver::check_integrity()
{
	for (int i = dlevel(); i > 0; --i)
	{
		if (!(*_assignment_stack[i]).empty())
		{
			CVariable & var = variable((*_assignment_stack[i])[0]>>1);
			int v = _branch_infor_stack[i]->gid;
			assert(!_component_infor_stack[v]->changed);
			if (var.tried_both())
			{
				assert(_component_infor_stack[v]->left_branch_done);
				//if (!_component_infor_stack[v]->left_branch_done)
				//{
				//	cout << "assert component " << v 
				//		 << " left_done failed at decision = " << _stats.num_decisions << endl;
				//	exit(0);
				//}
			}
		}
	}
	return true;
}




	







