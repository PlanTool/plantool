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



#ifndef __SAT_SOLVER__
#define __SAT_SOLVER__

#include <set>
#include <stack>
#include <map>

using namespace std;

#include "zchaff_version.h"
#include "zchaff_dbase.h"

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
    NO_CONFLICT
};

class CSolver;

typedef void(*HookFunPtrT)(void *) ;
typedef void(*OutsideConstraintHookPtrT)(CSolver * solver);
typedef bool(*SatHookPtrT)(CSolver * solver);

/**Struct**********************************************************************

  Synopsis    [Sat solver parameters ]

  Description []

  SeeAlso     []

******************************************************************************/
struct CSolverParameters {
    float 	time_limit;
    int		verbosity;

    struct {
        int size;
        int enable;
    } shrinking;

    struct {
	int 	strategy;
	int	restart_randomness;	
	int	base_randomness;
	int	bubble_init_step;
	int	decay_period;
    } decision;

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
        int     interval;
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
    int 	num_decisions;
    int 	num_decisions_stack_conf;
    int 	num_decisions_vsids;
    int 	num_decisions_shrinking;
    int         num_shrinkings;
    int 	restarts_since_vsids;
    int		num_backtracks;
    int 	max_dlevel;
    int 	random_seed;
    long64 	num_implications;
    int num_restarts;
    int num_del_orig_cls;
    int sum_sizes_iter;
    double sum_squares_iter;
    int num_conf_cls_iter;
    int num_fda_iter;
    
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
//    if (v1.second >= v2.second) return true;
//    if ((v1.first)->score(0)+(v1.first)->score(1) >= (v2.first)->score(0)+(v2.first)->score(1)) return true;
//    if ((v1.first)->score() >= (v2.first)->score()) return true;
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

//  struct CImplication
//  {
//      long64 id;
//      int lit;
//      int dlevel;
//      int antecedent;
//  };

//  struct cmp_implication {
//      bool operator() (const CImplication & i1, const CImplication & i2) {
//  	if (i1.dlevel > i2.dlevel) 
//  	    return true;
//  	else if (i1.dlevel < i2.dlevel)
//  	    return false;
//  	else if (i1.id > i2.id)
//  	    return true;
//  	return false;
//      }
//  };

//  struct ImplicationQueue: priority_queue<CImplication, vector<CImplication>, cmp_implication> 
//  {
//      CImplication front(void) {
//  	return top();
//      }
//      void dump(ostream & os) {
//  	priority_queue<CImplication, vector<CImplication>, cmp_implication> temp(&this);
//  	os << "Implication Queue Previous: " ;
//  	while(!temp.empty()) {
//  	    CImplication a = temp.front();
//  	    os << "(" << ((a.lit&1)?"-":"+") << (a.lit>>1) 
//  	       << "@" << a.dlevel << ":" << a.antecedent << ")  ";
//  	    temp.pop();
//  	}
//  	os << endl;
//      }
//  };

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

class CSolver:public CDatabase
{
protected:
    int				_id;			//the id of the solver, in case we need to distinguish
    bool			_force_terminate;	//forced to time out by outside caller
    CSolverParameters 		_params;		//parameters for the solver 
    CSolverStats     		_stats;			//statistics and states of the current run

    int 			_dlevel;		//current decision elvel
    vector<vector<int> *> 	_assignment_stack;
    queue<int>                  _recent_shrinkings;
    int                         _shrinking_benefit;
    int                         _shrinking_conf_cls_length;
    bool                        _mark_increase_score;
    long64			_implication_id;
    ImplicationQueue		_implication_queue;
    
    vector<pair<int,pair< HookFunPtrT, int> > > _hooks;	//hook function run after certain number of decisions
    OutsideConstraintHookPtrT	_outside_constraint_hook;
    SatHookPtrT	_sat_hook; //hook function run after a satisfiable solution found, return true to continue solving and false to terminate as satisfiable
//these are for decision making
    int				_max_score_pos;		//index the unassigned var with max score
    vector<pair<CVariable*,int> >_ordered_vars;		//pair's first pointing to the var, second is the score. 

//these are for conflict analysis
    int 		_num_marked;		//used when constructing learned clauses
    int 		_num_in_new_cl;		//used when constructing learned clauses
    vector<ClauseIdx> 	_conflicts;		//the conflicting clauses		       
    vector<int> 	_conflict_lits; 	//used when constructing learned clause
    vector<int> 	_resolvents;
    multimap<int,int>   _shrinking_cls;
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
    int analyze_conflicts();
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
    ClauseIdx add_clause_with_gid (int * lits, int n_lits, int gid = 0);

public:
//constructors and destructors
    CSolver();
    ~CSolver();
//member access function
    void set_time_limit(float t); 
    void set_mem_limit(int s);
    void set_decision_strategy(int s) ;
    void set_preprocess_strategy(int s); 
    void enable_cls_deletion(bool allow); 
    void set_allow_multiple_conflict( bool b) ;
    void set_allow_multiple_conflict_clause( bool b) ;
    void set_randomness(int n) ;
    void set_random_seed(int seed);

    void set_variable_number(int n);
    int add_variable(void) ;
    void mark_var_branchable(int vid);
    void mark_var_unbranchable(int vid);

    int & dlevel() 		{ return _dlevel; }
    int outcome () 		{ return _stats.outcome; }
    int num_decisions() 	{ return _stats.num_decisions; }
    int num_decisions_stack_conf() 
    	{ return _stats.num_decisions_stack_conf; }
    int num_decisions_vsids()
     	{ return _stats.num_decisions_vsids; }
    int num_decisions_shrinking()
     	{ return _stats.num_decisions_shrinking; }
    int num_shrinkings()
     	{ return _stats.num_shrinkings; }
    int & num_free_variables() 	{ return _stats.num_free_variables; }
    int max_dlevel() 		{ return _stats.max_dlevel; }
    int random_seed()		{ return _stats.random_seed; }
    long64 num_implications()	{ return _stats.num_implications; }
    long64 total_bubble_move() 	{ return _stats.total_bubble_move; }

    char * version(void)	{ return __ZCHAFF_VERSION__; }
    
    float elapsed_cpu_time();
    float cpu_run_time() ;
    int estimate_mem_usage() {	return CDatabase::estimate_mem_usage(); }
    int mem_usage(void); 

//      void queue_implication (int lit, ClauseIdx ante_clause, int dlevel) {
//  	DBG1 (cout << "\t\t\t\t\t\tQueueing " << (lit&0x01?" -":" +") << (lit>>1) ;
//  	      cout << "at " << dlevel << " because of  " << ante_clause << endl; );
//  	CImplication i;
//  	i.lit = lit;
//  	i.id = ++_implication_id;
//  	i.antecedent = ante_clause;
//  	i.dlevel = dlevel;
//  	_implication_queue.push(i);
//      }

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
    void unset_force_terminate (void) { _force_terminate = false;}

//for incremental SAT
    int add_clause_incr(int * lits, int n_lits, int gid = 0);
    void make_decision(int lit) {
	++ dlevel();
	queue_implication(lit, NULL_CLAUSE, dlevel());
    }

    void add_hook( HookFunPtrT fun, int interval);
    void add_outside_constraint_hook (OutsideConstraintHookPtrT fun) {_outside_constraint_hook = fun;}
    void add_sat_hook (SatHookPtrT fun) {_sat_hook = fun;}

    void verify_integrity(void);
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
    void add_outside_clauses(void);
};
#endif
