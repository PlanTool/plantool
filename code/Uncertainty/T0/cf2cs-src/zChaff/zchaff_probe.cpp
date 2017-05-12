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
#include <hash_set>
#include "zchaff_solver.h"

struct pair_int_equal {
    bool operator () (pair<int,int> a, pair<int,int> b) const {
	return (a.first==b.first && a.second == b.second);
    }
};

struct pair_int_comp {
    bool operator () (pair<int,int> a, pair<int,int> b) const {
	if (a.first < b.first) 
	    return true;
	else if (a.first == b.first && a.second < b.second)
	    return true;
	return false;
    }
};

struct pair_int_hash_fun {
    size_t operator () (const pair<int, int> a) const {
	return (a.first + (a.second << 16));
    }
};


typedef hash_set<pair<int,int>, pair_int_hash_fun, pair_int_equal> PairHash;

inline static void hash_insert( PairHash & table,
		int a, int b)
{
    table.insert(pair<int,int> (a, b));
}

inline static void hash_remove(PairHash & table,
		int a, int b)
{
    table.erase(pair<int,int> (a, b));
}

inline static bool hash_contain(PairHash & table,
		int a, int b)
{
    return (table.find(pair<int,int>(a,b))) != table.end();
}

static void dump_vector(vector<int> & list) {
    for (int i=0; i< list.size(); ++i)
	cout << list[i] << "  ";
    cout << endl;
}

static void get_common(vector<int> & temp1, vector<int> & common) 
//will put the common vars of temp1 and common into common, suppose common
//is already sorted, in is not
{
    ::sort(temp1.begin(), temp1.end());
    vector<int> temp2;
    common.swap(temp2);
    vector<int>::iterator itr1, itr2;
    for (itr1=temp1.begin(), itr2= temp2.begin(); itr1 != temp1.end() && itr2 != temp2.end(); ) {
	if ( *itr1 < *itr2) 
	    ++itr1;
	else if (*itr1 > *itr2)
	    ++itr2;
	else {
	    common.push_back(*itr1);
	    ++itr1; ++itr2;
	}
    }
}

int CSolver::add_probe_clause(vector<int> lits)
{
    int cl = add_clause(lits.begin(), lits.size(), VOLATILE_GID);
    clause(cl).status() = PROBE_CL;
}

int CSolver::do_deduction(void)
//after make a implication, try to go to
//a state which is consistant. So, the dlevel
//will remain the same if no conflict occured
//else it will be smaller than current.
{
    while (deduce() == CONFLICT) {
	if (analyze_conflicts() < 0)
	    break;
    }
    return dlevel();
}


int CSolver::simplify(void)
{
    vector<pair<int,int> > equiv_svar_pair;
    assert(dlevel() == 0);
    bool modified = true;
//      while (modified) {
//  	modified = false;
	if (probe(equiv_svar_pair, modified)==CONFLICT)
	    return CONFLICT;
	if (!equiv_svar_pair.empty()) {
	    modified = true;
	    make_equivalent(equiv_svar_pair);
	}
//    }
    return NO_CONFLICT;
}

void CSolver::make_equivalent(vector<pair<int, int> > & equiv)
{
//      if (equiv.size() == 0) return;
//      vector<vector<int> * > equ_class;
//      hash_map<int, int> var_to_class;
//      //1.initially, everything map to itself.
//      for(int i=0; i< variables().size(); ++i) {
//  	equ_clause.push_back(new vector<int>);
//  	equ_clauss.back().push_back(i+i);
//  	var_to_class[i] = i+i;	
//      }
//      //2. make equivalent
//      for (int i=0; i< equiv.size(); ++ i) {
//  	int a = equiv[i].first;
//  	int b = equiv[i].second;
//  	int va = a >>1;
//  	int sa = a & 0x1;
//  	int vb = b >>1;
//  	int sb = b & 0x1;
//  	int eq_a = var_to_class[a >> 1];
//  	int eq_b = var_to_class[b >> 1];
//  	if ((eq_a>>a) == (eq_b>>1)) {
//  	    int signa = (eq_a & 0x1) ^ sa;
//  	    int singb = (eq_b & 0x1) ^ sb;
//  		assert (signa == singb);
//  	}
//  	else {
//  	    int s = (eq_a & 0x1)^(eq_b & 0x1)^sa^sb ;
//  	    vector<int> & class_a = *equ_class[eq_a>>1];
//  	    vector<int> & class_b = *equ_class[eq_b>>1];
//  	    assert (class_a.size() > 0);
//  	    assert (class_b.size() > 0);
//  	    for (int j=0; j< class_b.size(); ++j) {
//  		int m = class_b[j];
//  		assert (var_to_class[m>>1] == (eq_b >> 1));
//  		var_to_class[m>>1] == eq_a ^ s ^ (m & 0x1);
//  		class_a.push_back(m ^ s);
//  	    }
//  	    class_b.clear();
//  	}
//      }
    //3. update var map
    equiv.clear();
}

void CSolver::get_rl_candidate(vector<vector<ClauseIdx> > * candidate)
{
    candidate[0].resize(variables().size());
    candidate[1].resize(variables().size());
    if (_params.preprocess.do_recursive_learning) {
	for (int i=0; i< clauses().size(); ++i) {
	    if (clause(i).status() == DELETED_CL || clause(i).status() == PROBE_CL) 
		continue;
	    CClause & cl = clause(i);
	    int num_lits = cl.num_lits();
	    int free_lits = 0;
	    int j;
	    for ( j=0; j< num_lits; ++j) {
		int lit_value = literal_value(cl.literal(j));
		if (lit_value ==0)
		    continue;
		else if (lit_value==1)
		    break;
		else
		    ++ free_lits;
	    }
	    if (j < num_lits || //i.e. break occured, clause SAT
		free_lits > _params.preprocess.max_rl_length)
		continue;
	    assert (free_lits > 1);
	    for (j=0; j< num_lits; ++j) {
		int vidx = cl.literal(j).var_index();
		int phase = cl.literal(j).var_sign();
		if (variable(vidx).value() == UNKNOWN ) {
		    candidate[phase][vidx].push_back(i);
		}
	    }
	}
    }
}

int CSolver::probe(vector<pair<int,int> > & equiv_svar_pair, bool & modified) 
{
    assert (dlevel() == 0);

    //use candidate to store the candidate clause for recursive learning
    vector<vector<ClauseIdx> > candidate[2];
    get_rl_candidate( candidate);
    //implication table store relationship a=>b. We need a < b; 
    PairHash implication_table;
    //put all the decision variables into "lits", this is a reason 
    //for the implications, though may not be the best one.
    vector<int> lits;
    for (int i=1; i<= dlevel(); ++i) 
	lits.push_back((*_assignment_stack[i])[0] ^ 0x1);
    //now do the real probing
    for (int i=1; i<= num_variables(); ++i) {
	if (variable(i).value() != UNKNOWN)
	    continue;
	DBG0(cout << "Probing VID : " << i << endl;);
	for (int phase = 0; phase < 2; ++phase) {
	    int svar = i + i + phase;
	    ++dlevel();
	    int dl = dlevel();
	    queue_implication(svar, NULL_CLAUSE);
	    vector<ClauseIdx> & rl_candidates = candidate[!phase][i];	
	    DBG0(  cout << "Probe " << i << " with value " << !phase << endl; );
	    probe_one_variable_assignment(rl_candidates);
	    if (dlevel() != dl) //that means some conflict happened inside, 
		if (dlevel() == -1) 
		    return CONFLICT;	
		else{
		    modified = true;
		    break;
		}
	    //dlevel() == dl, so the assigmentstack has all the implications;
	    vector<int> & assign = *_assignment_stack[dlevel()];
	    assert(assign[0] == svar);
	    for (int j=1, sz = assign.size(); j< sz; ++j) {
		int a, b;
		int v = assign[j]>> 1;
		int sign = assign[j]&0x1;
		if (v > i) {
		    a = svar;
		    b = assign[j] ;
		}
		else {
		    a = assign[j] ^ 0x1;
		    b = svar ^ 0x1;
		}
		if (!hash_contain(implication_table, a, b)) {
		    hash_insert(implication_table, a, b);
		    if (hash_contain(implication_table, a^0x1, b)) {
			DBG0(cout << "Probe infered " << b  << " must be true " << endl;);
			//here only add an "artificial" clause because we know it's dlevel 0, 
			//and this must be true globaly in the general case, we need to add 
			//"reasons" for this, it's pretty expensive though.
			lits.push_back( b);
			int cl = add_probe_clause(lits);
			queue_implication(b, cl);
			lits.pop_back();
		    }
		    else if (hash_contain(implication_table, a, b^0x1)) {
			DBG0(cout << "Probe infered " << (a^0x1)  << " must be true " << endl;);
			lits.push_back( a^0x1);
			int cl = add_probe_clause(lits);
			queue_implication(a^0x1, cl);
			lits.pop_back();
		    }
		    else if (hash_contain(implication_table, a^0x1, b^0x1)) {
			//because a < b, always b being represented by a. 
			equiv_svar_pair.push_back(pair<int,int>(a, b));
			DBG0(
			    cout << "Found " << a << " <==> " << b << endl;
			    if ((a&0x1)==(b&0x1) )	
			    cout << "i.e. " << a/2 <<"<===>" <<b/2 << endl;
			    else 
			    cout << "i.e. " << a/2 <<"<===>-" << b/2 << endl;);
		    }
		}
	    }
	    back_track(dl);
	    if (!_implication_queue.empty()) {
		modified = true;
		do_deduction();
		assert (dlevel() == 0); //we know there should be no conflict
	    }
	}
    }
    return NO_CONFLICT;
}

int CSolver::probe_one_variable_assignment( vector<ClauseIdx>& cls)
{
    assert (dlevel() == 1);
    int dl = dlevel();
    do_deduction();
    if (dl != dlevel())
	return CONFLICT;
    if (_params.preprocess.do_recursive_learning) {
	for (int i=0; i<cls.size(); ++i) {
	    justify_one_clause(cls[i]);
	    if (dlevel() != dl) 
		return CONFLICT;
	}
    }
    return NO_CONFLICT;
}



int CSolver::justify_one_clause(ClauseIdx cls_idx) 
//this will probe free variables in the clause one by one
{    
    CClause & cl = clause(cls_idx);
    vector<int> need_to_probe;
    for (int i=0; i< cl.num_lits(); ++i) {
	int litvalue = literal_value(cl.literal(i));
	if (litvalue==1)
	    return NO_CONFLICT;
	else if (litvalue != 0)
	    need_to_probe.push_back(cl.literal(i).s_var());
    }
    int num_free_lits = need_to_probe.size();
    assert (num_free_lits > 1);

    DBG1(cout << "Justify "; detail_dump_cl(cls_idx); cout <<endl;);
    //1. The first free lit
    ++ dlevel();
    int dl = dlevel();
    queue_implication(need_to_probe[0], NULL_CLAUSE);
    do_deduction();
    if (dlevel() != dl) //conflict happened
	return CONFLICT;
    //common will keep the common implications
    vector<int> common(*_assignment_stack[dlevel()]);
    //undo the implication
    back_track(dlevel());
    ::sort(common.begin(), common.end());
    //2. The rest free lit
    for (int i=1; i< num_free_lits; ++i) {
	++ dlevel();
	queue_implication(need_to_probe[i], NULL_CLAUSE);
	do_deduction();
	if (dlevel() != dl) //conflict happened
	    return CONFLICT;
	//get the common implication
	vector<int> temp1(*_assignment_stack[dlevel()]);
	get_common(temp1, common);
	back_track(dlevel());
	if (common.empty()) return NO_CONFLICT;
    }
    assert (!common.empty());
    DBG0(	cout << "Recursive learning found "; 
		dump_vector(common); 
		cout << endl;);
    vector<int> cls_lits;
    //this is the reason, though not a good one, for the implication
    for (int i=1; i<= dlevel(); ++i) 
	cls_lits.push_back( (*_assignment_stack[i])[0] ^ 0x1);
    //add the reason clause, and make the implication
    for (int i=0; i< common.size(); ++i) {
	cls_lits.push_back(common[i]);
	int cl = add_probe_clause(cls_lits);
	queue_implication(common[i], cl); 
	cls_lits.pop_back();
    }
    int result = deduce();
    assert(result != CONFLICT);
    return NO_CONFLICT;
}






//  int CSolver::simplify() 
//  {
//      vector<pair<int,int> > equiv_svar_pair;
//      if (probe(equiv_svar_pair)==CONFLICT) return CONFLICT;
//      assert (_simplified_inst==NULL);
//      _simplified_inst = new CSolver;

//      _var_map.clear();
//      for (int i=0; i<= variables().size(); ++i)
//  	_var_map.push_back(i); //e.g. each var map to itself
//      for (int i=0; i<= variables().size(); ++i)
//  	if (variable(i).value() != UNKNOWN) 
//  	    _var_map[i] = 0;
//      for (int i=0; i< equiv_svar_pair.size(); ++i) {
//  	int va = equiv_svar_pair[i].first>>1;
//  	int sa = equiv_svar_pair[i].first & 0x1;
//  	int vb = equiv_svar_pair[i].second>>1;
//  	int sb = equiv_svar_pair[i].second & 0x1;
//  	if (sa == sb) _var_map[vb] = va;
//  	else _var_map[vb] = -va;
//      }
//      for (bool modified=true; modified; ) {
//  	modified = false;
//  	for (int i=1; i< variables().size(); ++i) {
//  	    int equiv = _var_map[i];
//  	    if (_var_map[abs(equiv)] != abs(equiv)) {
//  		int equiv_equiv = _var_map[abs(equiv)];
//  		if (equiv>0 )
//  		    _var_map[i] = equiv_equiv;
//  		else 
//  		    _var_map[i] = -equiv_equiv;
//  		modified = true;
//  	    }
//  	}
//  	for (int i=0; i< variables().size(); ++i) {
//  	    cout << i << "(" << _var_map[i] << ")  ";
//  	}
//  	cout << endl << endl;
//      }
//      int new_idx = 0;
//      for (int i=0; i< variables().size(); ++i) {
//  	if (_var_map[i] == i) {
//  	    _var_map[i] = new_idx;
//  	    ++new_idx;
//  	}
//  	else {
//  	    assert (_var_map[i] < i && _var_map[i] > -i);
//  	    if (_var_map[i] < 0)
//  		_var_map[i] = - _var_map[-_var_map[i]];
//  	    else 
//  		_var_map[i] = _var_map[_var_map[i]];
//  	}
//      }
//      _simplified_inst->set_variable_number(new_idx);
//      for (int i=0; i< clauses().size(); ++i) {
//  	set<int> lits;
//  	set<int> vars;
//  	CClause & cl = clause(i);
//  	if (cl.status() != IN_USE) continue;
//  	int j;
//  	for ( j=0; j< cl.num_lits(); ++j) {
//  	    if (literal_value(cl.literal(j))==1) break;
//  	    if (literal_value(cl.literal(j))==0) continue;
//  	    int vidx = cl.literal(j).var_index();
//  	    int vsign = cl.literal(j).var_sign();
//  	    int actual_vidx = _var_map[vidx];
//  	    int actual_vsign = 0;
//  	    if (actual_vidx < 0) {
//  		actual_vidx = - actual_vidx;
//  		actual_vsign = 1;
//  	    }
//  	    actual_vsign = vsign ^ actual_vsign;
//  	    lits.insert((actual_vidx<<1) + actual_vsign);
//  	    vars.insert(actual_vidx);
//  	}
//  	if (j < cl.num_lits() || lits.size() != vars.size()) continue;
//  	vector<int> temp;
//  	for (set<int>::iterator itr = lits.begin(); itr != lits.end(); ++itr) 
//  	    temp.push_back(*itr);
//  	_simplified_inst->add_clause(temp.begin(), temp.size());
//      }
//      ofstream out("out.cnf");
//      out << "p cnf " << new_idx-1 << "  " << _simplified_inst->num_clauses() << endl;
//      for (int i=0; i< _simplified_inst->num_clauses(); ++i) {
//  	CClause & cl = _simplified_inst->clause(i);
//  	for (int j=0; j< cl.num_lits(); ++j) {
//  	    if (cl.literal(j).var_sign())
//  		out << "-";
//  	    out << cl.literal(j).var_index() << " ";
//    	}
//  	out << "0" << endl;
//      }
//      out.close();
//      cout <<"Found equivalence, so exit, output to out.cnf" << endl;
//      exit(0);
//      return NO_CONFLICT;
//  }

//  int CSolver::probe(vector<pair<int,int> > & equiv_svar_pair) 
//  {
//      int dl = dlevel();

//      vector<vector<ClauseIdx> > candidate[2];

//      //use candidate to store the candidate clause for recursive learning
//      candidate[0].resize(variables().size());
//      candidate[1].resize(variables().size());

//      if (_params.do_recursive_learning) {
//  	for (int i=0; i< clauses().size(); ++i) {
//  	    if (clause(i).status() == DELETED_CL) 
//  		continue;
//  	    CClause & cl = clause(i);
//  	    int num_lits = cl.num_lits();
//  	    int free_lits = 0;
//  	    int j;
//  	    for ( j=0; j< num_lits; ++j) {
//  		if (literal_value(cl.literal(j))==0)
//  		    continue;
//  		else if (literal_value(cl.literal(j))==1)
//  		    break;
//  		else
//  		    ++ free_lits;
//  	    }
//  	    if (j < num_lits || //i.e. break occured, clause SAT
//  		free_lits > _params.max_recursive_learning_cl_length)
//  		continue;
//  	    for (j=0; j< num_lits; ++j) {
//  		int vidx = cl.literal(j).var_index();
//  		int phase = cl.literal(j).var_sign();
//  		if (variable(vidx).value() == UNKNOWN ) {
//  		    candidate[phase][vidx].push_back(i);
//  		}
//  	    }
//  	}
//      }
//      //implication table store relationship a=>b. We need a < b; 
//      PairHash implication_table;
//      for (int i=1; i<= num_variables(); ++i) {
//  	cout << "Probing VID : " << i << endl;
//  	assert (dlevel()==dl);
//  	if (variable(i).value() != UNKNOWN) 
//  	    continue;
//  	for (int phase = 0; phase < 2; ++phase) {
//  	    vector<ClauseIdx> & probe_candidates = candidate[!phase][i];	
//  	    if (probe_one_variable_assignment(i, 
//  					      phase,
//  					      implication_table, 
//  					      equiv_svar_pair,
//  					      probe_candidates)==CONFLICT) {
//  		if (dlevel()==dl) {
//  		    phase = -1;
//  		}
//  		else {
//  		    if (dlevel() < 0)
//  			return CONFLICT;
//  		    dl = dlevel(); //make it the new probing level;
//  		    implication_table.clear();
//  		    i=0; //probe every var again, because we reached a new dlevel
//  		    break;
//  		}
//  	    }
//  	}
//      }
//      return NO_CONFLICT;
//  }

//  int CSolver::probe_one_variable_assignment(int vidx, int phase, 
//  					   PairHash& implication_table,
//  					   vector<pair<int,int> > & equiv_svar_pair,
//  					   vector<ClauseIdx>& probe_cl_candidates)
//  {
//      CHECK(  cout << "Probe " << vidx << " with value " << !phase << endl; );
//      assert( _implication_queue.empty());
//      ++dlevel();
//      int dl = dlevel();
//      int probe_lit = vidx+vidx+phase;
//      queue_implication(probe_lit , NULL_CLAUSE);
//      bool is_conflict= false;
//      while (deduce() == CONFLICT) {
//  	is_conflict = true;
//  	if (analyze_conflicts() <= 0)
//  	    break; //no need to deduce ananymore.
//      }
//      if (is_conflict==true) 
//  	return CONFLICT;

//      if (_params.do_recursive_learning) {
//  	vector<ClauseIdx> cls = probe_cl_candidates;
//  	for (int i=0; i<cls.size(); ++i) {
//  	    assert (dlevel()==dl);
//  	    if (justify_one_clause(cls[i])==CONFLICT) {
//  		if (dlevel() < dl) 
//  		    return CONFLICT;
//  		else {
//  		    assert (dl == dlevel());
//  		    i = -1; //redo the probe
//  		}
//  	    }
//  	}
//      }
//      vector<int> assign(*_assignment_stack[dlevel()]);
//      back_track(dlevel()); //now undo the whole probe

//      assert( _implication_queue.empty());
//      for (int j=1; j< assign.size(); ++j) {
//  	int a, b; //we will add a=>b into implication_table, maintain a < b;
//  	int v = assign[j]>> 1;
//  	int sign = assign[j]&0x1;
//  	if (v > vidx) {
//  	    a = probe_lit;
//  	    b = assign[j] ;
//  	}
//  	else {
//  	    a = assign[j] ^ 0x1;
//  	    b = probe_lit ^ 0x1;
//  	}
//    	if (!hash_contain(implication_table, a, b)) {
//  	    hash_insert(implication_table, a, b);
//  	    if (hash_contain(implication_table, a^0x1, b)) {
//  		DBG0(cout << "Probe infered " << b  << " must be true " << endl;);
//  		vector<int> lits;
//  		for (int i=1; i< dlevel(); ++i)
//  		    lits.push_back( (*_assignment_stack[i])[0] ^ 0x1);
//  		lits.push_back(b);
//  		int cl = add_clause(lits.begin(), lits.size());
//  		queue_implication(b, cl);
//  	    }
//  	    else if (hash_contain(implication_table, a^0x1, b^0x1)) {
//  		equiv_svar_pair.push_back(pair<int,int>(a, b));
//  		DBG0(
//  		    cout << "Found " << a << " <==> " << b << endl;
//  		    if ((a&0x1)==(b&0x1) )	
//  		    cout << "i.e. " << a/2 <<"<===>" <<b/2 << endl;
//  		    else 
//  		    cout << "i.e. " << a/2 <<"<===>-" << b/2 << endl;
//  		    );
//  	    }
//  	}
//      }
//      if (!_implication_queue.empty()) {
//  	int result = deduce();
//  	assert (result == NO_CONFLICT);
//      }
//      return NO_CONFLICT;
//  }
//  //  		cout << "Probe infered " << a << " <==> " << b << endl;

//  //  		 b can be replaced by a, i.e. a<=>b
//  //  	    vector<int> lits;
//  //  	    for (int i=1; i< dlevel(); ++i)
//  //  		lits.push_back( (*_assignment_stack[i])[0] ^ 0x1);
//  //  	    lits.push_back(b);
//  //  	    lits.push_back(a^0x1);
//  //  	    add_clause(lits.begin(), lits.size());
//  //  	    lits.pop_back();
//  //  	    lits.pop_back();
//  //  	    lits.push_back(b^0x1);
//  //  	    lits.push_back(a);
//  //  	    add_clause(lits.begin(), lits.size());
//  //  	    }
//  //  	}}
	


//  bool CSolver::get_reason_for_implication(int a, 
//  					 int b, 
//  					 int dl,
//  					 vector<pair<int, int> >& trace, //first ==> literal, second ==> ante
//  					 vector<int>& lits)
//  //if we have a==>b, with the assignment stack at this dlevel in "trace"
//  //find the clause which will have this implication info, the lits will
//  //have a and b in it, and all the other literals (dlevel < dl)
//  //trace should has b in it, and a as the first entry.
//  {
//      assert (_num_marked == 0);
//      assert (trace[0].first == a);
//      //1.first set b to be marked
//      variable(b>>1).set_marked();
//      //2.reverse traverse the assignment stack
//      for (int i=trace.size() - 1; i>0 ; ++i) {
//  	int vid = trace[i].first>>1;	
//  	CVariable & var = variable(vid);
//  	if (!var.is_marked())
//  	    continue;
//  	else {
//  	    var.clear_marked();
//  	    -- _num_marked;
//  	    int ante = trace[i].second;
//  	    for (CLitPoolElement * itr = clause(ante).literals(); (*itr).val() > 0 ; ++ itr) {
//  		int v = (*itr).var_index();
//  		if (v == vid) 
//  		    continue;
//  		else if (variable(v).dlevel()<dl && variable(v).value() != UNKNOWN) {
//  		    if (variable(v).in_new_cl() == -1){ //it's not in the new cl
//  			variable(v).set_in_new_cl((*itr).var_sign());
//  			lits.push_back((*itr).s_var());
//  		    }
//  		}
//  		else{
//  		    assert (variable(v).dlevel()==dl || variable(v).value()== UNKNOWN);
//  		    if (!variable(v).is_marked()) {
//  			variable(v).set_marked();
//  			++ _num_marked;
//  		    }
//  		}
//  	    }
//  	}
//      }
//      //3. should have single marked vars, clean up
//      assert (_num_marked == 1);
//      assert (variable(a>>1).is_marked());    
//      variable(a>>1).clear_marked();
//      -- _num_marked;
//  }

//  int CSolver::do_probe() 
//  {
//      switch(_params.probe_strength) {
//      case 0:
//  	return NO_CONFLICT;
//  	break;
//      case 1:
//  	return probe_n_vars(_params.num_probe_vars);
//  	break;
//      case 2:
//  	probe_vars_level_2();
//  	break;
//      case 3:
//  	probe_vars_level_2();
//  	break;
//      }	
//  }

//  void CSolver::get_cl_lits_from_reason_lits(vector<int> & reason_lits, vector<int> & cl_lits)
//  {
//      for (int k=0, sz1=reason_lits.size(); k< sz1; ++k) {
//  	int vid = reason_lits[k]>>1;
//  	int sign = reason_lits[k] & 0x1;
//  	if (variable(vid).in_new_cl()==-1) {
//  	    cl_lits.push_back(cl_lits);
//  	    variable(vid).set_in_new_cl(sign);
//  	}
//  	else 
//  	    assert(variable(vid).in_new_cl() == sign);
//      }
//      for (int j=0, sz=cl_lits.size()-1; j<sz; ++j)
//  	variable(cl_lits>>1).set_in_new_cl(-1);
//  }

//  int CSolver::probe_n_vars(int num_probes) 
//  {
//      if (num_probes > num_free_variables() )
//  	num_probes = num_free_variables();
	
//      for (int i=_max_score_pos; num_probes >0;  ++i) {
//  	CVariable & var = *_var_order[i].first;
//  	if (var.value()==UNKNOWN) {
//  	    int vidx = _var_order[i].first - variables().begin();
	    
//  	    vector<pair<int,int> > trace_0 = _implication_cache[vidx + vidx];
//  	    assert (trace_0.size() == 0);
//  	    if (probe_one_var(vidx, 0) == CONFLICT)
//  		return CONFLICT;
//  	    get_current_dl_implication_trace(trace_0);
//  	    backtrack(dlevel());

//  	    vector<pair<int,int> > trace_1 = _implication_cache[vidx + vidx + 1];
//  	    assert (trace_1.size() == 0);
//  	    if (probe_one_var(vidx, 1) == CONFLICT)
//  		return CONFLICT;
//  	    get_current_dl_implication_trace(trace_1);
//  	    backtrack(dlevel());
//  	    vector<int> common, temp1;
//  	    for (int j=0, sz=trace_0.size(); j< sz; ++j)
//  		common.push_back(trace_0.first);
//  	    for (int j=0, sz=trace_1.size(); j< sz; ++j)
//  		temp1.push_back(trace_1.first);
//  	    ::sort(common.begin(), common.end());
//  	    get_common(temp1, common);
//  	    if (!common.empty()) {
//  		for (int j=0, sz=common.size(); j< sz; ++j) {
//  		    vector<int> reason_lits;
//  		    int a = vidx + vidx;
//  		    int b = common[j];
//  		    get_reason_for_implication(a, b, dlevel()+1, trace_0, reason_lits);
//  		    int a = vidx + vidx + 1;
//  		    get_reason_for_implication(a, b, dlevel()+1, trace_1, reason_lits);
//  		    vector<int> cl_lits;
//  		    get_cl_lits_from_reason_lits(reason_lits, cl_lits);
//  		    cl_lits.push_back(common[j]);
//  		}
//  		int cl = add_clause(cl_lits.begin(), lits.size());
//  		queue_implication(common[j], cl);
//  	    }
//  	--num_probes;
//  	}
//      }
//      int result = deduce();
//      assert (result != CONFLICT);
//      return NO_CONFLICT;
//  }

//  int CSolver::probe_one_var(int vidx, int phase)
//  {
//      CHECK(  cout << "Probe " << vidx << " with value " << !phase << endl; );
//      assert( _implication_queue.empty());
//      ++dlevel();
//      int dl = dlevel();
//      int probe_lit = vidx+vidx+phase;
//      queue_implication(probe_lit , NULL_CLAUSE);
//      bool is_conflict= false;
//      while (deduce() == CONFLICT) {
//  	is_conflict = true;
//  	if (analyze_conflicts() <= 0)
//  	    break; //no need to deduce ananymore.
//      }
//      if (is_conflict==true) 
//  	return CONFLICT;
//  }

//  void CSolver::get_current_dl_implication_trace(vector<pair<int, int> > & traces)
//  {
//      int dlevel = dlevel();
//      vector<int> & assign = *_assignment_stack[dlevel];
//      assert (traces.empty());
//      assert (!assign.empty());
//      for (int i=0, sz = assign.size(); i< sz; ++i) {
//  	int svar = assign[i];
//  	int vid = svar>> 1;
//  	traces.push_back(pair<int, int>(svar, variable(vid).get_antecedence()));
//      }
//  }

//  static void get_common(vector<int> & temp1, vector<int> & common) 
//  //will put the common vars of temp1 and common into common, suppose common
//  //is already sorted, in is not
//  {
//      ::sort(temp1.begin(), temp1.end());
//      vector<int> temp2;
//      common.swap(temp2);
//      vector<int>::iterator itr1, itr2;
//      for (itr1=temp1.begin(), itr2= temp2.begin(); itr1 != temp1.end() && itr2 != temp2.end(); ) {
//  	if ( *itr1 < *itr2) 
//  	    ++itr1;
//  	else if (*itr1 > *itr2)
//  	    ++itr2;
//  	else {
//  	    common.push_back(*itr1);
//  	    ++itr1; ++itr2;
//  	}
//      }
//  }
//  int CSolver::simple_justify_one_clause(ClauseIdx cls_idx)
//  //this will not probe free vars in the clause, instead, use the pre-probed trace
//  //must have all free vars probed first.
//  {
//      CClause & cl = clause(cls_idx);
//      vector<int> free_lits;
//      for (int i=0; i< cl.num_lits(); ++i) {
//  	int litvalue = literal_value(cl.literal(i));
//  	if (litvalue==1)
//  	    return; //clause is already sat
//  	else if (litvalue != 0)
//  	    free_lits.push_back(cl.literal(i).s_var());
//      }
//      assert (free_lits.size() > 1);
//      vector<int> common;
//      vector<int> & trace = _implication_cache[free_lits[0]];
//      for (int i=0, sz=trace.size(); i< sz; ++i)
//  	common.push_back(trace[i].first);
//      ::sort(common.begin(), common.end());
//      for (int i=1, sz = free_lits.size(); i< sz; ++i) {
//  	vector<int> temp1;	
//  	vector<int> & trace = _implication_cache[free_lits[i]];
//  	for (int j=0, sz=trace.size(); j< sz; ++j)
//  	    temp1.push_back(trace[j].first);
//  	get_common(temp1, common);
//  	//find the common implications of temp1 and temp2
//  	if (common.empty()) return;
//      }
//      CHECK(cout << "Recursive learning found "; 
//  	  dump_vector(common); 
//  	  cout << endl;);
//      for (int i=0; i< common.size(); ++i) {
//  	vector<int> reason_lits;
//  	for (int j=0; j< num_free_lits; ++j) {
//  	    int a = free_lits[j];
//  	    int b = common[i];
//  	    get_reason_for_implication(a, b, dlevel()+1, _implication_cache[a], reason_lits);
//  	}
//  	vector<int> cl_lits;
//  	get_cl_lits_from_reason_lits(reason_lits, cl_lits);
//  	cl_lits.push_back(common[i]);
//  	int cl = add_clause(cl_lits.begin(), cl_lits.size());
//  	CHECK( detail_dump_cl(cl); cout << endl; );
//  	queue_implication(common[i], cl);
//      }
//      int result = deduce();
//      assert(result != CONFLICT);
//  }
				       

//  int CSolver::justify_one_clause(ClauseIdx cls_idx) 
//  //this will probe free variables in the clause one by one
//  {    
//      CClause & cl = clause(cls_idx);
//      vector<int> need_to_probe;
//      for (int i=0; i< cl.num_lits(); ++i) {
//  	int litvalue = literal_value(cl.literal(i));
//  	if (litvalue==1)
//  	    return NO_CONFLICT;
//  	else if (litvalue != 0)
//  	    need_to_probe.push_back(cl.literal(i).s_var());
//      }
//      CHECK(cout << "Justify "; detail_dump_cl(cls_idx); cout <<endl;);

//      int num_free_lits = need_to_probe.size();
//      assert (num_free_lits > 1);
//      //this is used to keep track of the "reasons" when add clause
//      vector<pair<int,int> > implication_trace[num_free_lits];   

//      //1. The first free lit
//      ++ dlevel();
//      queue_implication(need_to_probe[0], NULL_CLAUSE);
//      bool is_conflict= false;
//      while (deduce() == CONFLICT) {
//  	is_conflict = true;
//  	if (analyze_conflicts() <= 0)
//  	    break; //no need to deduce ananymore.
//      }
//      if (is_conflict==true) 
//  	return CONFLICT;
//      //store the trace
//      get_current_dl_implication_trace(implication_trace[0]);
//      //common will keep the common implications
//      vector<int> common(*_assignment_stack[dlevel()]);
//      //undo the implication
//      back_track(dlevel());
//      ::sort(common.begin(), common.end());
//      //2. The rest free lit
//      for (int i=1; i< num_free_lits; ++i) {
//  	++ dlevel();
//  	queue_implication(need_to_probe[i], NULL_CLAUSE);
//  	bool is_conflict= false;
//  	while (deduce() == CONFLICT) {
//  	    is_conflict = true;
//  	    if (analyze_conflicts() <= 0)
//  		break; //no need to deduce ananymore.
//  	}
//  	if (is_conflict==true) 
//  	    return CONFLICT;
//  	//store the trace
//  	get_current_dl_implication_trace(implication_trace[i]);
//  	//get the common implication
//  	vector<int> temp1(*_assignment_stack[dlevel()]);
//  	get_common(temp1, common);
//  	back_track(dlevel());
//  	if (common.empty()) return NO_CONFLICT;
//      }
//      assert (!common.empty());
//      CHECK(	cout << "Recursive learning found "; 
//  		dump_vector(common); 
//  		cout << endl;);
//      for (int i=0; i< common.size(); ++i) {
//  	vector<int> reason_lits;
//  	for (int j=0; j< num_free_lits; ++j) {
//  	    int a = need_to_probe[j];
//  	    int b = common[i];
//  	    get_reason_for_implication(a, b, dlevel()+1, implication_trace[j], reason_lits);
//  	}
//  	vector<int> cl_lits;
//  	get_cl_lits_from_reason_lits(reason_lits, cl_lits);
//  	cl_lits.push_back(common[i]);
//  	int cl = add_clause(cl_lits.begin(), cl_lits.size());
//  	CHECK( detail_dump_cl(cl); cout << endl; );
//  	queue_implication(common[i], cl);
//      }
//      int result = deduce();
//      assert(result != CONFLICT);
//      return NO_CONFLICT;
//  }
