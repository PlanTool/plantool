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
#ifndef __ZCHAFF_DATABASE__
#define __ZCHAFF_DATABASE__

#include <queue>
#include <set>
#include <stdlib.h>
#include "zchaff_base.h"

#define STARTUP_LIT_POOL_SIZE 0x8000

/**Struct**********************************************************************

  Synopsis    [Definition of the statistics of clause database]

  Description []

  SeeAlso     [CDatabase]

******************************************************************************/

struct CDatabaseStats {
    unsigned 	mem_used_up_counts;
    bool 	mem_used_up;
    unsigned 	init_num_clauses;
    unsigned 	init_num_literals;
    unsigned 	num_added_clauses;
    long64 	num_added_literals;
    unsigned 	num_deleted_clauses;
    unsigned    num_del_orig_cls;
    long64 	num_deleted_literals;
    unsigned 	num_compact;
    unsigned	num_enlarge;
};

/**Struct**********************************************************************

  Synopsis    [Definition of the parameters of clause database]

  Description []

  SeeAlso     [CDatabase]

******************************************************************************/


struct CDatabaseParams {
    int 	mem_limit;
};
/**Class**********************************************************************

  Synopsis    [Definition of clause database ]

  Description [Clause Database is the place where the information of the 
               SAT problem are stored. it is a parent class of CSolver ]

  SeeAlso     [CSolver]

******************************************************************************/
class CDatabase
{
protected:
    CDatabaseStats 	_stats;

    CDatabaseParams	_params;

    unsigned 		_allocated_gid;		//the gids that have already been allocated

    //for efficiency, the memeory management of lit pool is done by the solver
    CLitPoolElement * _lit_pool_start;		//the begin of the lit vector
    CLitPoolElement * _lit_pool_finish;		//the tail of the used lit vector
    CLitPoolElement * _lit_pool_end_storage;   	//the storage end of lit vector


    vector<CVariable> 	_variables; 		//note: first element is not used

    vector<CClause> 	_clauses;

    set<ClauseIdx> 	_unused_clause_idx;
    
    vector<CClause>::iterator 	top_unsat_cls;	
protected:
//constructors & destructors
    CDatabase() ;

    ~CDatabase();

    void init_stats(void) {
	_stats.mem_used_up_counts 	= 0;
	_stats.mem_used_up 		= false;
	_stats.init_num_clauses 	= num_clauses();
	_stats.init_num_literals 	= num_literals();
	_stats.num_deleted_clauses 	= 0;
	_stats.num_del_orig_cls 	= 0;
	_stats.num_deleted_literals 	= 0;
	_stats.num_enlarge		= 0;
	_stats.num_compact		= 0;
    }
//lit pool naming convention follows STL Vector
    CLitPoolElement * lit_pool_begin(void) ;

    CLitPoolElement * lit_pool_end(void) ;

    void lit_pool_incr_size( int size) ;

    void lit_pool_push_back(int value) ; 

    int lit_pool_size(void) ;

    int lit_pool_free_space(void) ;

    double lit_pool_utilization(void);

    CLitPoolElement & lit_pool(int i) ;

//functions on lit_pool
    void output_lit_pool_stats(void);

    bool enlarge_lit_pool(void);	//when allocated memeory runs out, do a reallocation

    void compact_lit_pool(void);	//garbage collection

    unsigned literal_value (CLitPoolElement l) { //note: it will return 0 or 1 or other , 
	                                         //here "other" may not equal UNKNOWN
	return (variable(l.var_index()).value() ^ l.var_sign()); 
    }

    unsigned svar_value (int svar) {		//note: it will return 0 or 1 or other , 
	                                         //here "other" may not equal UNKNOWN
	return (variable(svar>>1).value() ^ (svar&0x1)); 
    }
//clause properties
    void mark_clause_deleted(CClause & cl);

    int find_unit_literal(ClauseIdx cl);	//if not unit clause, return 0.

    bool is_conflicting(ClauseIdx cl); 		//e.g. all literals assigned value 0

    bool is_unit (ClauseIdx cl);

    bool is_satisfied(ClauseIdx cl);		//e.g. at least one literal has value 1

//others
    ClauseIdx get_free_clause_idx(void);

public:

//member access function
    vector<CVariable>& variables(void) { 
	return _variables; 
    }

    CVariable & variable(int idx) { 
	return _variables[idx]; 
    }

    vector<CClause>& clauses(void) { 
	return _clauses; 
    }

    CClause & clause(ClauseIdx idx) { 
	return _clauses[idx]; 
    }

    CDatabaseStats & stats(void) {
	return _stats;
    }

    void set_mem_limit(int n) {
	_params.mem_limit = n;
    }

//clause group management
    ClauseIdx add_clause (int * lits, int n_lits, int gflag = 0);

    int alloc_gid (void); 

    void free_gid (int gid);

    int get_volatile_gid(void) {return -1;}

    int	get_permanent_gid(void) { return 0; }

    bool is_gid_allocated(int gid);

    int merge_clause_group(int g1, int g2);

//some stats
    unsigned & init_num_clauses()	{ return _stats.init_num_clauses; }

    unsigned & init_num_literals ()	{ return _stats.init_num_literals; }

    unsigned & num_added_clauses ()	{ return _stats.num_added_clauses; }

    long64  & num_added_literals() 	{ return _stats.num_added_literals; }

    unsigned & num_deleted_clauses() 	{ return _stats.num_deleted_clauses; }

    unsigned & num_del_orig_cls()       { return _stats.num_del_orig_cls; }

    long64 & num_deleted_literals() 	{ return _stats.num_deleted_literals; }

    unsigned num_variables(void) 	{ return variables().size() - 1; }

    unsigned num_clauses(void) 		{ return _clauses.size() - _unused_clause_idx.size();}

    unsigned num_literals(void)		{ return _stats.num_added_literals - _stats.num_deleted_literals;}

    unsigned num_mem_compacts(void)	{ return _stats.num_compact; }

    unsigned num_mem_enlarges(void) 	{ return _stats.num_enlarge; }

//functions 
    unsigned estimate_mem_usage (void);

    unsigned mem_usage(void) ;

    void set_variable_number(int n) { 
	variables().resize(n + 1) ; 
    }
    int add_variable(void) {
	variables().resize( variables().size() + 1);
	return variables().size() - 1;
    }
//dump functions
    void detail_dump_cl(ClauseIdx cl_idx, ostream & os = cout);

    void dump(ostream & os = cout);

    friend ostream & operator << ( ostream & os, CDatabase & db) { 
	db.dump(os); 
	return os;
    }
};

#endif











