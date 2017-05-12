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


#ifndef __BASIC_CLASSES__
#define __BASIC_CLASSES__

#include <assert.h>
#include <iostream>
#include <vector>
#include <list>
#include "zchaff_header.h"
//#define BIG_NUM
#ifdef BIG_NUM
	#include "big_num.h"
#endif

//#define APPROXIMATE_HASHING
#define KEEP_LIT_CLAUSES	// added by sang
#define FORMULA_CACHING		// added by sang
//#define LINEAR_HASH		// added by sang
#define KEEP_GOING			// for finding all solutions, added by sang
//#define LEARNING_OFF		// for turning off clause-learning, added by sang
//#define DEBUG_OUT			// output debug information added by sang
//#define CACHE_SIZE			100000 // for hashtable size
#define TOCOMPONENTS		// added by sang
#define LARGEST_DEGREE		// decision heuristic
//#define STATIC_ORDERING		// decision heuristic
#define MAX_DISTANCE	0	// minimum distance to a cached component for a component to be cached
//#define CHOOSE_ACTIVE_VAR	// in a decision, only choose active var in the residual components
//#define NON_ACTIVE_STACK	// store non-active vars in a stack to avoid future decision on them
//#define DISABLE_CLASUES		// disable clauses that are not in the active components
//#define SORT_CLAUSES_IN_FORMULA
#define CUT_OFF 10			// for quicksort, added by sang
//#define STOP_ADDED_CLAUSE	// added by sang
//#define ENABLE_DEL_CLAUSE	// added by sang
//#define ADDED_CL_MAX 50000// max number of added conflict clauses
//#define GID_1		3000	// gid =1 for clauses > GID_1
#define MAX_HASH_ENTRIES  238435399 // 8388593	// 1M*2	// added by sang
#define UNKNOWN   	2
#define TRIED_BOTH	3		//	added by sang

#define NULL_CLAUSE  	-1
#define VOLATILE_GID   -1
#define	PERMANENT_GID 	0

//typedef vector<vector <int> *> formula; // define the formula type, added by sang
typedef vector <int> formula; // changed at 2/22/2005

struct child_to_remove
{
	unsigned hash_index;
	unsigned sequence_number;
};

struct Component_value_pair 
{	
#ifdef APPROXIMATE_HASHING
	unsigned long secondary_index;
#endif

#ifdef BIG_NUM
	BigNum value;	// the exact satisfying probability
#else
	long double value;	// the probabily of f to be satisfied
#endif
	
	formula f;		// real clauses of a formula
	unsigned sequence_number;
	Component_value_pair * next;
	//Component_value_pair * prev;	// double-linked list
	vector<child_to_remove *> cached_child_list;	// the child components to remove
};

typedef int ClauseIdx; //used to refer a clause. Because of dynamic 
// allocation of vector storage, no pointer is allowered

#ifndef _CLS_STATUS_
#define _CLS_STATUS_
enum CLAUSE_STATUS
{
    ORIGINAL_CL,
    CONFLICT_CL,	// ADDED_CL
    DELETED_CL,
    PROBE_CL,
    UNKNOWN_CL,
	//SATISFIED_ORIGINAL_CL,	// added by sang
	//SATISFIED_ADDED_CL		// added by sang
};
#endif

/**Class**********************************************************************

  Synopsis    [Definition of a literal]

  Description [A literal is a variable with phase. Two things specify a literal: 
               its "sign", and its variable index. 

	       Each clause that has more than 1 literal contains two special literals.
	       They are being "watched". A literal is marked with 2 bits: 
	       00->not watched; 11->watched, direction = 1;  01->watched, dir = -1; 
	       10 is not valid. These two bits occupy the least significant bits
	       of the literal. 

	       Each literal is represented by a 32 bit signed integer. The higher 29
	       bits represent the variable index. At most 2**28 varialbes are
	       allowed. If the sign of this integer is negative, it means that it is
	       not a valid literal. It could be a clause index or a deleted literal 
	       pool element. The 3rd least significant bit is used to mark its sign. 
	       0->positive, 1->negative. 

	       The literals are collected in a storage space called literal
	       pool. An element in a literal pool can be a literal or a special
	       spacing element to indicate the termination of a clause. The 
	       spacing elements has negative value of the clause index.]

  SeeAlso     [CDatabase, CClause]

******************************************************************************/

class CLitPoolElement
{
protected:
    int32 _val;

public:
    //constructors & destructors
    CLitPoolElement(void):_val(0)	{}

    ~CLitPoolElement() 			{}

    //member access function
    int & val(void) {
	return _val;
    }

    int s_var(void) { //stands for signed variable, i.e. 2*var_idx + sign
	return _val>>2;
    }

    //unsigned var_index(void) {
	int var_index(void) {
	return _val>>3; 
    }

    unsigned var_sign(void) { 
	return ( (_val>> 2)& 0x1); 
    }

    void set (int s_var) {
	_val = (s_var << 2);
    }

    void set(int vid, int sign) { 
	_val = (((vid<<1) + sign)<< 2); 
    }

    //followings are for manipulate watched literals
    int direction (void) {
	return ((_val&0x03) - 2);
    }

    bool is_watched(void) {
	return ((_val&0x03) != 0);
    }

    void unwatch(void) {
	_val = _val & (~0x3);
    }

    void set_watch(int dir) {
	_val = _val + dir + 2;
    }

    //following are used for spacing (e.g. indicate clause's end)
    bool is_literal(void) {
	return _val > 0;
    }

    void set_clause_index(int cl_idx) {
	_val = - cl_idx;
    }

    ClauseIdx get_clause_index(void) {
	assert (_val <= 0);
	return -_val; 
    }

    //misc functions
    unsigned find_clause_index(void) {
	CLitPoolElement * ptr;
	for (ptr = this; ptr->is_literal(); ++ ptr);
	return ptr->get_clause_index();
    }

    //every class should have a dump function and a self check function
    void dump(ostream & os= cout);

    friend ostream & operator << ( ostream & os, CLitPoolElement & l) { 
	l.dump(os); 
	return os;
    }
};


/**Class**********************************************************************

  Synopsis    [Definition of a clause]

  Description [A clause is consisted of a certain number of literals. 
               All literals are collected in a single large vector, called
	       literal pool. Each clause has a pointer to the beginning position
	       of it's literals in the pool. 
	       
	       Zchaff support incremental SAT. Clauses can be added or deleted
	       from the database during search. To accomodate this feature, some 
	       modifications are needed.

	       Clauses can be generated during search by conflict driven analysis.
	       Conflict clauses are generated by a resolution process. 
	       Therefore, if after one search, some clauses got deleted, then 
	       some of the learned conflict clause may be invalidated. To maintain
	       the integrity of the clause database, it is necessary to keep track
	       of the clauses that are involved in the resolution process for a
	       certain conflict clause so that when those clauses are deleted,
	       the conflict clause should also be deleted. 
	       
	       The scheme we implement is similar to the scheme described in:
	       Ofer Strichman, Pruning techniques for the SAT-based Bounded Model 
	       Checking Problems, in Proc. 11th Advanced Research Working Conference on 
	       Correct Hardware Design and Verification Methods (CHARME'01)
	       ]

  SeeAlso     [CDatabase]

******************************************************************************/

class CClause
{
protected:
    CLitPoolElement *	_first_lit;	//pointer to the first literal in literal pool
    unsigned 		_num_lits ;	//number of literals
    CLAUSE_STATUS 	_status	: 3;	//indicate if this clause has been deleted or not
    unsigned		_id	: 29;	//the unique ID of a clause
    unsigned		_gflag;		//the clause group id flag, maximum allow WORD_WIDTH groups
	int             _activity;	// added in zchaff2004
	unsigned		_counter_one; // counting number of 1's in the clause, added by sang
	//int				_component_index;	// added by sang
	// pointer to the component that contains this clause
public:
//constructors & destructors
    CClause(void){
	_status = UNKNOWN_CL;
	_counter_one = 0;	// added by sang
	//_component_index = 0;	// added by sang
    }

    ~CClause() {}

//initialization & clear up
    void init(CLitPoolElement * head, unsigned num_lits, unsigned gflag) { //initialization of a clause
	_first_lit = head;
	_num_lits = num_lits;
	_gflag = gflag;
    }

//member access function

	int & activity(void){	// added in zchaff2004
        return _activity;
    }

	unsigned & counter_one(void) {
		return _counter_one;
	}		// added by sang

	//int & component_index(void) {
	//	return _component_index;
	//}		// added by sang

    CLitPoolElement * literals(void) 	{ 	//literals()[i] is it's the i-th literal
	return _first_lit; 
    }	

    CLitPoolElement & literal(int idx) 	{ 	//return the idx-th literal
	return *(_first_lit + idx); 
    }

    CLitPoolElement * & first_lit(void) {	//use it only if you want to modify _first_lit
	return _first_lit; 
    }

    unsigned & num_lits(void) { 
	return _num_lits; 
    }

    unsigned id(void) {
	return _id;
    }

    void set_id(int id) {
	_id = id;
    }

    CLAUSE_STATUS status(void) { 
	return _status; 
    }

    void set_status(CLAUSE_STATUS st) { 
	_status = st;
    }

//manipulate the group flag
    unsigned & gflag(void) {
	return _gflag;
    }

    bool gid (int i) {
	assert (i>=1 && i<= WORD_WIDTH);
	return (( _gflag & (1<<(i-1))) != 0);
    }

    void set_gid (int i) {
	assert (i>=1 && i<= WORD_WIDTH);
	_gflag |= (1<<(i-1));
    }

    void clear_gid(int i) {
	assert (i>=1 && i<= WORD_WIDTH);
	_gflag &= ~(1<<(i-1));
    }

//misc function
    bool self_check(void);

    void dump(ostream & os = cout); 

    friend ostream & operator << ( ostream & os, CClause & cl) { 
	cl.dump(os); 
	return os;
    }
};


/**Class**********************************************************************

  Synopsis    [Definition of a variable]

  Description [CVariable contains the necessary information for a variable.]

  SeeAlso     [CDatabase]

******************************************************************************/
class CVariable 
{
public:
	double pos_weight;		// weight of positive form
	double neg_weight;		// weight of negative form
	bool internal;			// a var can be exteral (no parent, or added at some node) or internal (with parent(s))
	bool cross_flag;		// a var can be cross-component implication or not

protected:
    unsigned _value		: 2;	//it can take 3 values, 0, 1 and UNKNOWN
						
    bool _marked		: 1;	//used in conflict analysis.
	bool _tried_both;	// added by sang
	bool _touched;		// added by sang
    
    unsigned _new_cl_phase	: 2;	//it can take 3 value 0: pos phase, 
    //1: neg phase, UNKNOWN : not in new clause;
    //used to keep track of literals appearing
    //in newly added clause so that a. each
    //variable can only appearing in one phase
    //b. same literal won't appear more than once.
    bool _enable_branch 	: 1;	//if this variable is enabled in branch selection

    int _implied_sign		: 1;	//when a var is implied, here is the sign (1->negative, 0->positive)

    ClauseIdx _antecedent	;   	//used in conflict analysis. 

    int _dlevel; 			//decision level this variable being assigned

    int _assgn_stack_pos;	;	//the position where it is in the assignment stack

    int _lits_count[2];			//how many literals are there with this variable. (two phases)

    vector<CLitPoolElement *> _watched[2];	//watched literals of this var. 0: pos phase, 1: neg phase

#ifdef KEEP_LIT_CLAUSES
    vector<ClauseIdx> _lit_clauses[2];	//this will keep track of ALL the appearance of the variable in clauses
    //note this will increase the database size by upto a factor of 2
#endif
    int _scores[2];			//the score used for decision making

    int _var_score_pos;			//keep track of this variable's position in the sorted score array

public:
//constructors & destructors
	int	largest_degree;		// added by sang	
	int largest_svar;		// added by sang
	int _static_score;

    CVariable(void) {
	init();
	_lits_count[0] = _lits_count[1] = 0;
    }

    ~CVariable() {}

    void init(void) {
	_static_score = 100000;	// set a max socre
	_tried_both = false;	//	added by sang
	_touched = false;		//	added by sang
	pos_weight = 1;			//	added by sang, for weighted counting
	neg_weight = 1;			//	added by sang
	internal = false;
	cross_flag = false;

	_value = UNKNOWN; 
	_antecedent=NULL_CLAUSE; 
	_marked = false;
	_dlevel = -1; 
	_assgn_stack_pos = -1;
	_new_cl_phase = UNKNOWN;	
	_scores[0] = _scores[1] = 0;
	_enable_branch = true;
    }
//member access function
    int & score(int i) 	{ 
	return _scores[i]; 
    }

    int score(void)	{ 
//	return 1;	//this will make a fixed order branch heuristic
	// return score(0)>score(1)?score(0):score(1); 
	return score(0) + score(1);		// changed from max individual score to combined sum score
    }

	void set_weight(double weight)
	{
		if (weight < -0.5)	// -1 means an internal node with weight pos-1 and neg-1
			pos_weight = neg_weight = 1;
		else
		{
			pos_weight = weight;
			neg_weight = 1 - weight;
		}
	}

    int & var_score_pos(void) {
	return _var_score_pos; 
    }
    
    void set_var_score_pos(int pos) {
	_var_score_pos = pos;
    }

    unsigned value(void) { 
	return _value;
    }

    void set_value(unsigned v) {
	_value = v;
    }

	// added by sang -- begin

	void set_static_score(int i)
	{
		_static_score = i;
	}

	void set_tried_both(bool t) {
		_tried_both = t;	
	}

	bool tried_both(void) {
		return _tried_both;	
	}

	void touched(void) {
		_touched = true;
	}

	void untouched(void) {
		_touched = false;
	}

	bool is_touched(void) {
		return _touched;
	}
	// added by sang -- end

    int & dlevel(void) { 
	return _dlevel;
    }

    int get_dlevel(void) {
	return _dlevel;
    }

    void set_dlevel(int dl) {
	_dlevel = dl;
    }

    int & assgn_stack_pos(void) {
	return _assgn_stack_pos;
    }

    int & lits_count(int i) { 
	return _lits_count[i];
    }

    bool is_marked(void) { 
	return _marked; 
    }    
    
    int get_implied_sign(void) {
	return _implied_sign;
    }
    
    void set_implied_sign(int sign) {
	_implied_sign = sign;
    }

    unsigned new_cl_phase(void) { 
	return _new_cl_phase; 
    } 

    void set_new_cl_phase(unsigned phase) { 
	_new_cl_phase = phase; 
    }

    void set_marked(void) { 
	_marked = true; 
    }

    void clear_marked(void) { 
	_marked = false; 
    }

    ClauseIdx & antecedent(void) { 
	return _antecedent; 
    }
    
    ClauseIdx get_antecedent(void) { 
	return _antecedent; 
    }
    
    void set_antecedent(ClauseIdx cl) {
	_antecedent = cl;
    }

    vector<CLitPoolElement *> & watched(int i) { 
	return _watched[i]; 
    }

    void enable_branch(void) {
	_enable_branch = true;
    }

    void disable_branch(void) {
	_enable_branch = false;
    }

    bool is_branchable(void) {
	return _enable_branch;
    }
#ifdef KEEP_LIT_CLAUSES
    vector<ClauseIdx> & lit_clause(int i) {
	return _lit_clauses[i]; 
    }
#endif    

//misc function
    bool self_check(void);

    void  dump(ostream & os=cout);

    friend ostream & operator << ( ostream & os, CVariable & v) { 
	v.dump(os); 
	return os;
    }
};

#endif














