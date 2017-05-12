/*********************************************************************
 Copyright 2000-2001, Princeton University.  All rights reserved. 
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
 any trademark,  service mark, or the name of Princeton University. 


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


#define LEVEL_MAX 500

#ifndef __SAT_SOLVER__
#define __SAT_SOLVER__

#include <set>
#include <stack>
#include <list>
#include <algorithm>

using namespace std;

#include "zchaff_version.h"
#include "zchaff_dbase.h"
#include "hash.h"	//	added by sang
//#include "big_num.h"

#ifndef _SAT_STATUS_
#define _SAT_STATUS_
enum SAT_StatusT {
    UNDETERMINED,
    UNSATISFIABLE,
    SATISFIABLE,
    TIME_OUT,
    MEM_OUT,
    ABORTED
};
#endif

enum SAT_DeductionT {
    CONFLICT,
    NO_CONFLICT,
	SAT
};


enum HEURISTICS {
    DEGREE,		// Dynamic largest individual sum
    DEGREE_SUM,
	SUM,		// Dynamic largest combined sum
	SUM_DEGREE,
	VSIDS,		// Variable State Independent Decaying Sum
	VSADS,		// Variable State Aware Decaying Sum
	UNIT_PROP,	// Unit propagation counting based
	BERKMIN		// berkmin-like approximate BCP-based
};


class CSolver;

typedef void(*HookFunPtrT)(void *) ;
typedef void(*OutsideConstraintHookPtrT)(CSolver * solver);

/**Struct**********************************************************************

  Synopsis    [Sat solver parameters ]

  Description []

  SeeAlso     []

******************************************************************************/
struct CSolverParameters {
    float 	time_limit;
    int		verbosity;

    struct {
	int	strategy;
	int	restart_randomness;	
	int	base_randomness;
	int	bubble_init_step;
	int	decay_period;
    } decision;

	// old version
    /*struct {
	bool		enable;
	unsigned  	interval;
	unsigned	max_unrelevance;
	unsigned 	min_num_lits;
	unsigned 	max_conf_cls_size;
    } cls_deletion;*/

	// new version
	struct {
	bool		enable;
	unsigned  	interval;
    unsigned        head_activity;
    unsigned        tail_activity;
    unsigned        head_num_lits;
    unsigned        tail_num_lits;
    int             tail_vs_head;
    } cls_deletion;

    struct {
	bool	enable;
	int 	first_restart;
	int 	backtrack_incr;		
	int	backtrack_incr_incr;
    } restart;
};
/**Struct**********************************************************************

  Synopsis    [Sat solver statistics ]

  Description []

  SeeAlso     []

******************************************************************************/
struct CSolverStats {
    bool	been_reset;		//when delete clause in incremental solving, must reset.
    bool 	is_solver_started;	
    int 	outcome;
    bool	is_mem_out;		//this flag will be set if memory out

    double 	start_cpu_time;    	
    double 	finish_cpu_time;

    int		current_randomness;

    int		next_restart;
    int		restart_incr;
    int		next_cls_deletion;
    int		next_var_score_decay;
    int 	num_free_variables;
    int		num_free_branch_vars;

    long64 	total_bubble_move;
    long64 num_decisions;
#ifdef BIG_NUM
	mpz_t num_solutions;			//	added by sang
#else
	long double num_solutions;
#endif
	long double sat_prob;
	unsigned num_cross_implications;
	int		num_changed_components;
	int		num_adjusted_components;
	unsigned num_total_components;
	unsigned num_split_components;
	unsigned num_not_split_components;
	unsigned num_trivial_components;
	int		first_split_level;

	int		num_unsatisfied_clause; //	added by sang
	int		num_original_clause;	//	added by sang
	int		num_unit_clause;		//	added by sang
    unsigned num_active_added_conf_clauses;  //	added by sang
    unsigned num_backtracks;
    int 	max_dlevel;
	int		num_del_orig_cls;	// added in zchaff2004
    long64 	num_implications;
};
/**Class**********************************************************************

  Synopsis    [Sat Solver]

  Description [This class contains the process and datastructrues to solve
               the Sat problem.]

  SeeAlso     []

******************************************************************************/
inline bool cmp_var_stat(const pair<CVariable *,int> & v1, 
			    const pair<CVariable *,int> & v2) 
{	
    if (v1.second >= v2.second) return true;
    return false;
};

struct cmp_var_assgn_pos {
    bool operator () (CVariable * v1, CVariable * v2)
	{
	    if (v1->dlevel() > v2->dlevel())
		return true;
	    else if (v1->dlevel() < v2->dlevel())
		return false;
	    else if (v1->assgn_stack_pos() > v2->assgn_stack_pos()) 
		return true;
	    return false;
	}
};

struct CImplication
{
    int lit;
    int antecedent;
    int dlevel;
};

struct  ImplicationQueue:queue<CImplication> 
{
    void dump(ostream & os) {
	queue<CImplication> temp(*this); //have to make a copy, since queue can only be accessed destructively
	os << "Implication Queue Previous: " ;
	while(!temp.empty()) {
	    CImplication a = temp.front();
	    os << "(" << ((a.lit&1)?"-":"+") << (a.lit>>1) 
	       << "@" << a.dlevel << ":" << a.antecedent << ")  ";
	    temp.pop();
	}
    }
};

//	added by sang -- begin
struct Hashindex
{
	unsigned index;
#ifdef APPROXIMATE_HASHING
	//unsigned secondary_index;
	unsigned long secondary_index;
#endif
};

struct Component_item
{
	Component_value_pair * comp_val_pair; 
	int	index;		// index to the gloabal structure: vector<Component_information>
};
typedef list<Component_item *> Components;	// list of pointers to components, main structure at each level

struct Component_information
{
	//bool tag;						//	for debug
	bool changed;					//	changed due to corss-component implications
	Components::iterator comp_itr;	//	pointer to the component associated with this information struct
	int num_clause;					//	number of clauses of the component
	int num_cross_implications;		//	#cross_implications that have changed this component
	double cross_implication_weight;//	the multiplication of weight of all cross-implications refering to this component 
	unsigned ancestor_index;		//	index to the Component_information struct of the component, which generates 
									//	the first appearance of this component
	int level;						//	the level where this component is created
	bool active;					//	if this component is active
	bool left_branch_done;			//	if the left branch of this component has been done
#ifdef BIG_NUM
	BigNum sat_probability;	//	the sat probability of this component
	BigNum left_sat_prob;		//	the sat probability of the left branch
	BigNum right_sat_prob;		//	the sat probability of the right branch
#else
	long double sat_probability;	//	the sat probability of this component
	long double left_sat_prob;		//	the sat probability of the left branch
	long double right_sat_prob;		//	the sat probability of the right branch
#endif
	unsigned num_children;			//	number of components generated by instanciating a var in the current component
	unsigned num_cached_children;	//	for removing siblings of an unsat child
	int largest_svar;				//	the s_var with largest degree in this component
	int largest_degree;				//	the degree of s_var (either pos_degree or neg_degree)
	int largest_sum;
	int static_score;
	//unsigned largest_distance;		//	the largest distance to a cached component
	vector <int> * var_set;				//	the set for storing all varariables in the component
	vector<int> branched_child_list;// branched but uncached childen of a component
	unsigned hash_index;			//	the hash index of this componet
	//unsigned secondary_index;		//	the secondary hash index of this componet
#ifdef APPROXIMATE_HASHING
	unsigned long secondary_index;
#endif
};

struct branch_information
{
	unsigned gid;				//	the component number involved with the next level decision
	int num_new_children;	//	the number of newly created components by branching on the last level component
	int num_children_of_changed;	// children of a changed component which is detected at that level
	vector<int> changed_components;	// changed component list at that level
};

struct cross_implication	// the cross_component_implication involving var vid created at level
{
	int vid;
	int level;
	bool marked;
};

//	added by sang -- end

class CSolver:public CDatabase
{
//friend void CHashTable::print_formula(formula &);	// added by sang
//	friend class CHashTable;						// added by sang
protected:
	//int counter;
	//bool backtrack_tag;							// for debug
	//bool set_tag;									// for debug
	//bool flag;									// for debug
	//bool flag2;
	//int				hit_sizeA[100];
	//int				hit_sizeB[100];
	//int				level_total_components[LEVEL_MAX+1];
	//int				level_new_components[LEVEL_MAX+1];
	//int				level_trivial[LEVEL_MAX+1];
	//int				level_conflicts[LEVEL_MAX+1];
	//int				level_hits[LEVEL_MAX+1];
	int				original_BN_nodes;				// for BN solving
	int				num_conflicts;
	int				num_sat;
	int				num_no_components;
	bool            _mark_increase_score;			// from zchaff2004
	vector<int>		var_score;						// for decision making
	vector<int>		score_sum;						// for decision making
	vector<int>		candidates;						// for decision making
	vector<set <int> > vgo;							// for decision making, static
	bool			static_heuristic;
	bool			cross_enabled;
	bool			adjust_enabled;
	bool			far_back_track_enabled;
	float			backtrack_factor;
	vector <cross_implication *> cross_implication_list; // list for storing all cross component implication
	unsigned		detection_seq;
	unsigned		num_cross;
	CHashTable *	hashtable;						// added by sang
    int				_id;							//the id of the solver, in case we need to distinguish
    bool			_force_terminate;				//forced to time out by outside caller
    CSolverParameters 		_params;				//parameters for the solver 
    CSolverStats     		_stats;					//statistics and states of the current run

    int 			_dlevel;						//current decision elvel
	int				_uni_phased_size;				// added by sang, # of uni_phased variables
	unsigned		bound;
	//int				num_back_track;
	//int				_num_branchable_components;		// for decision making
	int				backtrack_bound;
	int				num_far_backtrack;
	int				_gid;							// added by sang
	//vector <Component_value_pair *> _formula_stack;	// added by sang
	//vector <Hashindex *> _hash_index_stack;			// added by sang
	vector <unsigned>		_branchable_component_stack;// for making decisions
	vector <int>		_num_implication_stack;		// added by sang
	vector<Component_information *> _component_infor_stack; // global array of component information
	//vector <Components *> _components_stack;		// added by sang
	//vector <int>	_gid_stack;						// stack of branching components
	vector <branch_information *> _branch_infor_stack;// stack of branch information
	vector <int>	touched_var_stack;				// added by sang
	vector<int>		touched;
	vector<int>		clause_comp;
	//vector <vector <unsigned> *>	_clause_component_stack;// added by sang
	//formula			f_no_unit_clauses;				// added by sang
	Components		components_no_unit_clauses;		// added by sang
	Components		_components;					// global array of components

    vector<vector<int> *> 	_assignment_stack;
    long64			_implication_id;
    ImplicationQueue		_implication_queue;
    
    vector<pair<int,pair< HookFunPtrT, int> > > _hooks;	//hook function run after certain number of decisions
    OutsideConstraintHookPtrT	_outside_constraint_hook;

//for hash table	added by sang
	bool extract_formula_from_clauses(formula &); //get formula of the current level
	//int extract_components_from_clauses(Components &, int);
	int extract_new_components(Components &, int, unsigned);
	int find_component(CVariable &, int); // find out the component corresponding to the variable
#ifdef BIG_NUM
	BigNum satprob;	// initialized to 1
	int percolate_up(BigNum &, int);
#else
	int percolate_up(long double, int);
#endif
	//int remove_cached_components(Components &);	
// int to_components(formula &);	// divide the formula into components, return number of components

//these are for decision making
    int				_max_score_pos;		//index the unassigned var with max score
    vector<pair<CVariable*,int> >_ordered_vars;		//pair's first pointing to the var, second is the score. 
	vector<vector<int> *>	_non_active_free_var_stack;	// added by sang
	bool	is_active(int);		// check if a var is active in the residual formula, added by sang
	void dump_non_active_free_var_stack(void);
	void assign_static_score();

//these are for conflict analysis
    int 		_num_marked;		//used when constructing learned clauses
    int 		_num_in_new_cl;		//used when constructing learned clauses
    vector<ClauseIdx> 	_conflicts;		//the conflicting clauses		       
    vector<int> 	_conflict_lits; 	//used when constructing learned clause
    vector<int> 	_resolvents;
protected:
    void re_init_stats(void);
    void init_stats(void);
    void init_parameters(void);
    void init_solve(void);			
    void real_solve(void);
    void restart (void);
    int preprocess(void);
    int deduce(void);
    void run_periodic_functions (void);

//for decision making
    bool decide_next_branch(void);
    void decay_variable_score(void) ;
    void adjust_variable_order(int * lits, int n_lits);
    void update_var_score(void);

//for preprocessing
    int simplify(void);
    int add_probe_clause(vector<int> lits);
    int do_deduction(void);
    void make_equivalent(vector<pair<int, int> > & equiv);
    void get_rl_candidate(vector<vector<ClauseIdx> > * candidate);
    int probe(vector<pair<int,int> > & equiv_svar_pair, bool & modified);
    int probe_one_variable_assignment( vector<ClauseIdx>& cls);
    int justify_one_clause(ClauseIdx cls_idx) ;

//for conflict analysis
    ClauseIdx add_conflict_clause (int * lits, int n_lits, int gflag);
    int analyze_conflicts(void);
    ClauseIdx finish_add_conf_clause(int gflag);
    int conflict_analysis_grasp (void);
    int conflict_analysis_firstUIP (void);
    int conflict_analysis_lastUIP(void);
    int conflict_analysis_allUIP (void);
    int conflict_analysis_mincut (void);
    int conflict_analysis_decisions_only (void);
    void mark_vars_at_level(ClauseIdx cl, int var_idx, int dl);
    void mark_vars_of_dl(vector<int> & lits, int dl);
    void mark_vars_at_current_dlevel(ClauseIdx cl, int var_idx) {
	mark_vars_at_level(cl, var_idx, dlevel());
    }
    int find_max_clause_dlevel(ClauseIdx cl);	//the max dlevel of all the assigned lits in this clause
    void back_track(int level);
	void far_back_track(int level);		//	added by sang

//for another way of doing conflict analysis
    void set_up_resolve(ClauseIdx conf_cl, set<CVariable *, cmp_var_assgn_pos> & conf_lits);
    unsigned resolve_one_lit(set<CVariable *, cmp_var_assgn_pos> & conf_lits);
    bool is_uip_reached(set<CVariable *, cmp_var_assgn_pos> & conf_lits);
    int conflict_analysis_firstUIP_resolve_based(void);

//for bcp 
    void set_var_value(int var, int value, ClauseIdx ante, int dl);
    void set_var_value_not_current_dl(int v, int value);
    void set_var_value_current_dl(int v, int value);
    void unset_var_value(int var);

//misc functions
    bool time_out(void);
    void delete_unrelevant_clauses(void);
    ClauseIdx add_clause_with_gid (int * lits, int n_lits, int gid = 0, bool original = false);

public:
//constructors and destructors
    CSolver();
    ~CSolver();

	unsigned		cache_size;						// added by sang
	unsigned		max_entry;						// the max number of active cache entries
	unsigned		oldest_entry;					// the oldest entry in cache
	unsigned		clean_limit;					// if > limit, clean_cache will be triggered
	unsigned		max_distance;					// added by sang
	//bool			dynamic_heuristic;				// true means dynamic largest degree, false means static
	HEURISTICS		dynamic_heuristic;				
	bool			approximation;					// if true, use approximate hash
	bool			quiet;
	unsigned		max_num_learned_clause;			// for shutting down clause-learning

//member access function
    void set_time_limit(float t); 
    void set_mem_limit(int s);
    void set_decision_strategy(int s) ;
    void set_preprocess_strategy(int s); 
    void enable_cls_deletion(bool allow); 
    void set_cls_del_interval(int n);
    void set_max_unrelevance(int n );
    void set_min_num_clause_lits_for_delete(int n) ;
    void set_max_conflict_clause_length(int l) ;
    void set_allow_multiple_conflict( bool b) ;
    void set_allow_multiple_conflict_clause( bool b) ;
    void set_randomness(int n) ;
    void set_random_seed(int seed);
    void set_variable_number(int n);

	// some functions added by sang
	//void set_var_weight(vector<double> &);
	void set_BN_node(int);
	void set_var_weight(vector<double> *);
	void set_vgo(vector< set<int> >);
	void set_static_heuristic(bool);
	void set_cache_size(unsigned);
	void set_max_entry(unsigned);
	void set_max_distance(unsigned);
	void set_dynamic_heuristic(int);
	void set_approximation(bool);
	void set_max_num_learned_clause(unsigned);
	void set_backtrack_factor(double);
	void set_oldest_entry(unsigned);
	void set_clean_limit(unsigned);
	void set_cross_flag(bool);
	void set_adjust_flag(bool);
	void set_far_back_track_flag(bool);
	void set_quiet_flag(bool);

    int add_variable(void) ;
    void mark_var_branchable(int vid);
    void mark_var_unbranchable(int vid);

    int & dlevel() 		{ return _dlevel; }
    int outcome () 		{ return _stats.outcome; }
    //int num_decisions() 	{ return _stats.num_decisions; }
	long64 num_decisions() 	{ return _stats.num_decisions; }
    int & num_free_variables() 	{ return _stats.num_free_variables; }
    int max_dlevel() 		{ return _stats.max_dlevel; }
    long64 num_implications()	{ return _stats.num_implications; }
    long64 total_bubble_move() 	{ return _stats.total_bubble_move; }
	long double satisfying_prob() {return _stats.sat_prob; }
#ifdef BIG_NUM
	long double num_solutions() 
	{
		cout << "Number of solutions\t\t\t";
		mpz_out_str (stdout, 10, _stats.num_solutions);
		cout << endl;
		return mpz_get_d(_stats.num_solutions); 
	}
	bool gmp() {return true;}
#else
	long double num_solutions() {return _stats.num_solutions; }
	bool gmp() {return false;}
#endif

    char * version(void)	{ return __ZCHAFF_VERSION__; }
    
    double elapsed_cpu_time();// changed from float to double
    double cpu_run_time() ;	// changed from float to double
    int estimate_mem_usage() {	return CDatabase::estimate_mem_usage(); }
    int mem_usage(void); 

    void queue_implication (int lit, ClauseIdx ante_clause, int dlevel) {
	DBG1 (cout << "\t\t\t\t\t\tQueueing " << (lit&0x01?" -":" +") << (lit>>1) ;
	      cout << "at " << dlevel << " because of  " << ante_clause << endl; );
	CImplication i;
	i.lit = lit;
	i.antecedent = ante_clause;
  	i.dlevel = dlevel;
	_implication_queue.push(i);
    }

//top level function
    int id(void) {return _id; }
    void set_id(int i) { _id = i;}
    void force_terminate (void) { _force_terminate = true;}
//for incremental SAT
    int add_clause_incr(int * lits, int n_lits, int gid = 0);
    void make_decision(int lit) {
	++ dlevel();
	queue_implication(lit, NULL_CLAUSE, dlevel());
    }

    void add_hook( HookFunPtrT fun, int interval);
    void add_outside_constraint_hook (OutsideConstraintHookPtrT fun) {_outside_constraint_hook = fun;}

    void verify_integrity(void);
	bool check_integrity(void);		// added by sang
    void delete_clause_group(int gid);
    void reset (void);
    int	solve(void);
    ClauseIdx add_orig_clause (int * lits, int n_lits, int gid = 0);
    void clean_up_dbase(void);
    void dump_assignment_stack(ostream & os = cout);
    void dump_implication_queue(ostream & os = cout);

    void print_cls(ostream & os= cout);
    void dump(ostream & os = cout ) {
	CDatabase::dump(os);
	dump_assignment_stack(os);
    }
	void dump_touched_var_stack();	// added by sang
	void dump_components(Components &);	// added by sang
	void dump_component_infor_stack();	// added by sang
	void dump_branch_infor_stack();	// added by sang
	void dump_branchable_component_stack(); // added by sang
	void dump_cross_implications();	// added by sang
	void dump_var_related_clauses(int, int); // added by sang
    void add_outside_clauses(void);

	void remove_branched_children(vector<int> &); // added by sang
};
#endif
