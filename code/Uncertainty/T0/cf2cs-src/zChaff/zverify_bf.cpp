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

#include <sys/time.h>
#include <sys/resource.h>
#include <unistd.h>
#include <cstdio>
#include <cstdlib>
#include <vector>
#include <set>
#include <iostream>
#include <fstream>
#include <assert.h>

#include "mystl_hash.h"

using namespace std;

const int MAX_BUFF_SIZE = 1024* 1024 * 4;
const int CLS_COUNT_ARRAY_SIZE = 1024 * 1024;
const int WORD_LEN = 64000;

#define UNKNOWN 2

#define MEM_LIMIT 800000

int _peak_mem;

//================================================================================

double get_cpu_time()
{
    double res;
    struct rusage usage;

    getrusage(RUSAGE_SELF, &usage);

    res = usage.ru_utime.tv_usec + usage.ru_stime.tv_usec;
    res *= 1e-6;
    res += usage.ru_utime.tv_sec + usage.ru_stime.tv_sec; 
	
    return res;
}

void get_line(ifstream &fs, vector<char> &buf)
{
    buf.clear();
    buf.reserve(4096);
    while(!fs.eof()) {
        char ch = fs.get();
        if(ch == '\n' || ch == '\377')
	    break;
        if(ch == '\r') 
	    continue;
        buf.push_back(ch);
    }
    buf.push_back('\0');
    return;
}

int get_token (char * & lp, char * token)
{
    char * wp = token;
    while (*lp && ((*lp == ' ') || (*lp == '\t'))) {
	lp++;
    }
    while (*lp && (*lp != ' ') && (*lp != '\t') && (*lp != '\n')) {
	*(wp++) = *(lp++);
    }
    *wp = '\0';                                 // terminate string
    return wp - token; 
}

int get_mem_usage(void) 
{
    FILE * fp;
    char buffer[128];
    char token[128];
    char filename[128];

    int pid = getpid();
    sprintf(filename, "/proc/%i/status", pid);
    if ( (fp = fopen (filename, "r")) == NULL) {
	cerr << "Can't open Proc file, are you sure you are using Linux?" << endl;
	exit(1);
    }
    while(!feof(fp)) {
	fgets(buffer, 1024, fp);
	char * ptr = buffer;
	get_token(ptr, token);
	if (strcmp(token, "VmSize:")==0) {
	    get_token(ptr, token);
	    fclose(fp);
	    return atoi(token);
	}
    }
    cerr << "Error in get memeory usage" << endl;
    exit(1);
    return 0;
}

void check_mem_out(void)
{
    int mem = get_mem_usage();
    if (mem > MEM_LIMIT) {
	cerr << "Mem out" << endl;
	exit(1);
    }
    if (mem > _peak_mem)
	_peak_mem = mem;
}

int my_a2i(char * str)
{
    int result = 0;
    bool neg = false;
    if (str[0] == '-') {
	neg = true;
	++ str;
    }
    else if (str[0] == '+')
	++ str;
    for (unsigned i=0, l = strlen(str); i< l; ++i) {
	int d = str[i] - '0';
	if (d < 0 || d > 9) {
	    cerr << "Abort: Unable to change " << str << " into a number " << endl;
	    exit(1);
	}
	result = result * 10 + d;
    }
    if (neg) 
	result = -result;
    return result;
}

//================================================================================
class CClause 
{
public:    
    int id;
    vector<int> literals;
    int num_fanouts;
    int num_used;
public:
    CClause(void) {
	num_fanouts = 0;
	num_used = 0;
    }
    ~CClause(){}
};

class CVariable
{
public:
    short value;
    short in_clause_phase; 
    int antecedent;
    int level ;
    bool is_needed;
    CVariable(void) { 
	value = UNKNOWN; 
	is_needed = false;
	antecedent = -1; 
	in_clause_phase = UNKNOWN;
	level = -1;
    }
};

struct cmp_var_level 
{
    bool operator () (CVariable * v1, CVariable * v2)
	{
	    if (v1->level > v2->level)
		return true;
	    else if (v1->level < v2->level)
		return false;
	    else if ( (int)v1 > (int)v2)
		return true;
	    return false;
	}
};

class CDatabase 
{
private:
    bool _output_core;
    char * _counter_filename;
    fstream _counter_stream;
    ifstream _trace_stream;
    FILE * _tmp_rsource_fp;
    
    int	_num_init_clauses;
    int _current_clause_count;
    int _max_clause_count;
    int _init_clause_count;
    int _current_clause_id;
    int _max_clause_id;
    hash_map<int, CClause * > _clauses;
    vector<CVariable> _variables;
    int _conf_id;
    vector<int> _conf_clause;
    vector<int> _empty_r_source;
public:
    CDatabase(void) {
	_counter_filename = NULL;
	_output_core = false;
	_tmp_rsource_fp = NULL;
    }
    ~CDatabase() {
	if (_counter_filename != NULL)
	    remove(_counter_filename); 
    }
    int & num_init_clauses(void) { return _num_init_clauses; }

    vector<CVariable> & variables(void) {return _variables; }

    hash_map<int, CClause *> & clauses(void)  {return  _clauses; }

    void init(char * cnf_file, char * trace_file);
    void read_cnf(char * cnf_file);
    void first_pass(void);
    void second_pass(void);
    bool real_verify(void);
    bool verify(void);
    void add_clause_by_literals(int cl_id, vector<int> & lits);
    void add_clause_by_resolvents(int cl_id, vector<int> & resolvents);
    void set_clause_fanout(int cl_id, int n);
    void set_var_number(int nvar);
    int recursive_find_level(int vid);

    void set_init_cls_number (int n) {
	_num_init_clauses = n;
    }

    int lit_value(int svar) { 
	assert (_variables[svar>>1].value != UNKNOWN);
	return _variables[svar>>1].value ^ (svar&0x1);
    }
    
    FILE * reverse_file(FILE * fp_in);
    void print_file(FILE * fp);
    void calculate_unsat_core(void);
    void set_output_core(bool foo) {
	_output_core = foo;
	if (foo == true) {
	    assert (_tmp_rsource_fp == NULL);
	    _tmp_rsource_fp = tmpfile();
	    if (_tmp_rsource_fp == NULL) {
		cerr << "Can't Open Temp File " << endl;
		exit(1);
	    }
	}
    }
   
    void dump(void);    
};

bool CDatabase::verify(void) 
{
    cout << "Current Mem Usage\t\t\t" << get_mem_usage() << endl;
    cout << "Begin First Pass, calculating fanout... " << endl;
    first_pass();
    cout << "Finished First Pass, Mem Usage \t\t" << get_mem_usage() << endl;
    cout << "Init Num. Clause\t\t\t" << _num_init_clauses << endl;
    cout << "Total Num. Clauses\t\t\t" << _max_clause_id << endl;
    second_pass();
    cout << "Finished Second Pass, Mem Usage \t" << get_mem_usage() << endl;
    cout << "Init Clause Count\t\t\t" << _init_clause_count << endl;
    cout << "Current Clause Count\t\t\t" << _current_clause_count << endl;
    assert ((unsigned)_current_clause_count == _clauses.size());
    cout << "Max Clause Count\t\t\t" << _max_clause_count << endl;
    int r = real_verify();    
    cout << "Final Mem Usage \t\t\t" << get_mem_usage() << endl;
    return r;
}

void CDatabase::set_clause_fanout(int cl_id, int n)
{
    assert (_clauses.find(cl_id) != _clauses.end());
    CClause * cl = _clauses[cl_id];
    cl->num_fanouts = n;
    if (n==0) { // no fanout, don't need it anyway
	if (!(_output_core && cl_id < num_init_clauses())) { 
	    delete cl;
	    _clauses.erase(cl_id);
	    -- _current_clause_count;
	}
    }
}

void CDatabase::add_clause_by_literals(int cl_id, vector<int> & lits)
{
    CClause * cl = new CClause;
    cl->id = cl_id;
    cl->literals = lits;
    _clauses[cl_id] = cl;
    ++ _current_clause_count;
    if (_current_clause_count > _max_clause_count)
	_max_clause_count = _current_clause_count;
    if (_current_clause_id % 10 == 0)
	check_mem_out();
}

void CDatabase::add_clause_by_resolvents(int cl_id, vector<int> & resolvents)
{
    vector<int> literals;

    int cl1_id = resolvents[0];
    assert (cl1_id < cl_id);
    assert (_clauses.find(cl1_id) != _clauses.end());
    CClause & cl1 = *_clauses[cl1_id];
    for (unsigned i=0; i< cl1.literals.size(); ++i) {
	int lit = cl1.literals[i];
	int vid = (lit>>0x1);
	int sign = (lit&0x1);
	assert (_variables[vid].in_clause_phase == UNKNOWN);
	_variables[vid].in_clause_phase = sign;
	literals.push_back(lit);
    }
        
    for (unsigned i=1; i< resolvents.size(); ++i) {
	int distance = 0;
	int cl1_id = resolvents[i];
	assert (cl1_id < cl_id);
	assert (_clauses.find(cl1_id) != _clauses.end());
	CClause & cl1 = *_clauses[cl1_id];
	
	for (unsigned j=0; j< cl1.literals.size(); ++j) {
	    int lit = cl1.literals[j];
	    int vid = (lit>>0x1);
	    int sign = (lit&0x1);
	    if (_variables[vid].in_clause_phase == UNKNOWN) {
		_variables[vid].in_clause_phase = sign;
		literals.push_back(lit);
	    }
	    else if (_variables[vid].in_clause_phase != sign) {
				//distance 1 literal
		++distance;
		_variables[vid].in_clause_phase = UNKNOWN;
	    }
	}
	if (distance != 1) {
	    cerr << "Resolve between two clauses with distance larger than 1" << endl;
	    cerr << "The resulting clause is " << cl_id << endl;
	    cerr << "Starting clause is " << resolvents[0] << endl;
	    cerr << "One of the clause involved is " << cl1_id << endl;
	    exit(1);
	}
    }

    for (unsigned i=0; i< resolvents.size(); ++i) {
	int key = resolvents[i];
	CClause * cl = _clauses[key];
	++ cl->num_used;
	if (cl->num_used == cl->num_fanouts) {
	    if (!(_output_core && key < num_init_clauses())) { 
		delete cl;
		_clauses.erase(key);
		-- _current_clause_count;
	    }
	}
    }

    CClause * cl = new CClause;
    for (unsigned i=0; i< literals.size(); ++i) {
	int lit = literals[i];
	int vid = (lit>>0x1);
	int sign = (lit&0x1);
	if (_variables[vid].in_clause_phase == UNKNOWN) 
	    continue;
	assert (_variables[vid].in_clause_phase == sign);
	_variables[vid].in_clause_phase = UNKNOWN;
	cl->literals.push_back(lit);
    }
    cl->id = cl_id;
    _clauses[cl_id] = cl;

    ++ _current_clause_count;
    if (_current_clause_count > _max_clause_count) 
	_max_clause_count = _current_clause_count;
    if (_current_clause_id % 10 == 0)
	check_mem_out();
}

void CDatabase::init(char * cnf_file, char * trace_file)
{
    _num_init_clauses = 0;
    _current_clause_count = 0;
    _max_clause_count = 0;
    _conf_id	= -1;
    _current_clause_id = 0;
    _counter_filename = strdup(tmpnam(NULL));
    cout << "Temp file is " << _counter_filename << endl;
    _counter_stream.open(_counter_filename, ios::in | ios::out | ios::trunc);	
    if (!_counter_stream.is_open()) {
	cerr << "Abort: Can't open a temp file " << endl;
	exit(1);
    }

    _trace_stream.open(trace_file, fstream::in);
    if (!_trace_stream.is_open()) {
	cerr << "Abort: Can't open the trace file " << endl;
	exit(1);
    }
    read_cnf(cnf_file);
}

void CDatabase::read_cnf (char * filename)
{
    ifstream in_file(filename);
    if (!in_file) {
	cerr << "Can't open input CNF file " << filename << endl;
	exit(1);
    }

    vector<char> buffer;
    vector<int> literals;
    bool header_encountered = false;
    char token[WORD_LEN];	
    while (!in_file.eof()) {
	get_line(in_file, buffer);
	char * ptr = &(*buffer.begin());
	if (get_token(ptr, token)) {
	    if (strcmp(token, "c")==0)
		continue;
	    else if (strcmp(token, "p")==0) {
		assert (literals.empty());
		assert (header_encountered == false);
		get_token(ptr, token);
		if (strcmp(token, "cnf") != 0) {
		    cerr << "Format Error, p cnf NumVar NumCls " << endl;
		    exit(1);
		}
		get_token(ptr, token);
		int nvar = my_a2i(token);
		set_var_number(nvar);
		get_token(ptr, token);
		int ncls = my_a2i(token);
		set_init_cls_number(ncls);
		header_encountered = true;
		continue;
	    }
	    else {
		int lit = my_a2i(token);
		if (lit != 0) {
		    if (lit > 0) 
			literals.push_back(lit + lit);
		    else 
			literals.push_back(1- lit - lit);
		}
		else {
		    add_clause_by_literals(_current_clause_id, literals);
		    ++ _current_clause_id;
		    literals.clear();
		}
	    }
	}
	while (get_token(ptr, token)) {
	    int lit = my_a2i(token);
	    if (lit != 0) {
		if (lit > 0) 
		    literals.push_back(lit + lit);
		else 
		    literals.push_back(1- lit - lit);
	    }
	    else {
		add_clause_by_literals(_current_clause_id, literals);
		++ _current_clause_id;
		literals.clear();
	    }
	}
    }
    if (!literals.empty()) {
	cerr << "Trailing numbers without termination " << endl;
	exit(1);
    }
    if (clauses().size() != (unsigned)num_init_clauses()) 
	cerr << "WARNING : Clause count inconsistant with the header " << endl;
    cout << "Successfully read " << num_init_clauses() << " Clauses " << endl;
}

void CDatabase::first_pass(void)
{
    _max_clause_id = 0;
    int * fanout_count;
    fanout_count = new int [CLS_COUNT_ARRAY_SIZE];
    bool first = true;
    int base = 0;
    char token[WORD_LEN];
    vector<char> buffer;

    _counter_stream.seekg(0, ios::beg);
    int iteration = 0;
    while (first || _max_clause_id >  base) {
	cout << "Iteration    " << iteration++ << endl;
	_trace_stream.seekg(0,ios::beg);
	_trace_stream.clear();

	for (int i=0; i< CLS_COUNT_ARRAY_SIZE; ++i) 
	    fanout_count[i] = 0;

	while (!_trace_stream.eof()) {
	    get_line(_trace_stream, buffer);
	    char * ptr = &(*buffer.begin());
	    get_token(ptr, token);
	    if (strcmp (token, "CL:") == 0) {
		vector<int> resolvents;

		get_token(ptr, token);
		int cl_id = my_a2i(token);
		if (cl_id > _max_clause_id -1 ) 
		    _max_clause_id = cl_id + 1;
		get_token(ptr, token);
		assert (strcmp(token, "<=") == 0);

		while (get_token(ptr, token)) {
		    int r = my_a2i(token);
		    resolvents.push_back(r);
		}
		for (unsigned i=0; i< resolvents.size(); ++i) {
		    int cl_id = resolvents[i];
		    int index = cl_id - base;
		    if (index >= 0 && index < CLS_COUNT_ARRAY_SIZE) //in the window
			++ fanout_count[index];
		}
	    }
	    else if (strcmp (token, "VAR:") == 0) {
		get_token(ptr,token);
		int vid = my_a2i(token);

		get_token(ptr,token);
		assert (strcmp(token, "L:") == 0);
		get_token(ptr,token); //skip the level

		get_token(ptr,token);
		assert (strcmp(token, "V:") == 0);
		get_token(ptr,token);
		int value = my_a2i(token);
		assert (value == 1 || value == 0);

		get_token(ptr,token);
		assert (strcmp(token, "A:") == 0);
		get_token(ptr,token);
		int ante = my_a2i(token);

		get_token(ptr,token);
		assert (strcmp(token, "Lits:") == 0);

		_variables[vid].value = value;
		_variables[vid].antecedent = ante;
		int index = ante - base;
		if (index >= 0 && index < CLS_COUNT_ARRAY_SIZE) //in the window
		    ++ fanout_count[index];
	    }
	    else if (strcmp (token, "CONF:") == 0) {
		get_token(ptr,token);
		_conf_id = my_a2i(token);
		get_token(ptr,token);
		assert (strcmp(token, "==") == 0);

		int index = _conf_id - base;
		if (index >= 0 && index < CLS_COUNT_ARRAY_SIZE) //in the window
		    ++ fanout_count[index];

		while (get_token(ptr, token) && first) {
		    int lit = my_a2i(token);
		    assert (lit > 0);
		    assert ((unsigned)(lit>>1) < _variables.size());
		    _conf_clause.push_back(lit);
		}
	    }
	}
	int top;
	if (_max_clause_id <  base + CLS_COUNT_ARRAY_SIZE)
	    top = _max_clause_id;
	else 
	    top = base + CLS_COUNT_ARRAY_SIZE;
	for (int i=base; i< top; ++i)
	    _counter_stream << fanout_count[i-base] << endl;

	first = false;
	base += CLS_COUNT_ARRAY_SIZE;
    }	
    delete [] fanout_count;
}

void CDatabase::second_pass(void)
{
    int n_fanout;
    vector<char> buffer;
    char token[WORD_LEN];

    _trace_stream.seekg(0,ios::beg);
    _trace_stream.clear();

    _counter_stream.seekg(0, ios::beg);
    _counter_stream.clear();

    assert (_current_clause_id == _num_init_clauses);
    
    for (int i=0; i< _current_clause_id; ++i) {
	_counter_stream >> n_fanout;
	set_clause_fanout(i, n_fanout);
    }
    _init_clause_count = _current_clause_count;
    
    for (int i=_current_clause_id; i< _max_clause_id; ++i) {
	_counter_stream >> n_fanout;
	while (!_trace_stream.eof()) {
	    get_line(_trace_stream, buffer);
	    char * ptr = &(*buffer.begin());
	    get_token(ptr, token);
	    if (strcmp (token, "CL:") == 0) {
		vector<int> resolvents;

		get_token(ptr, token);
		int cl_id = my_a2i(token);
		assert (cl_id == i);

		get_token(ptr, token);
		assert (strcmp(token, "<=") == 0);

		while (get_token(ptr, token)) {
		    int r = my_a2i(token);
		    resolvents.push_back(r);
		}
		
		if (_output_core) {
		    int storage[resolvents.size() + 2];
		    storage[0] = - cl_id;
		    storage[1] = - resolvents.size();
		    for (unsigned j=0; j< resolvents.size(); ++j)
			storage[j+2] = resolvents[j];
		    fwrite(storage, sizeof(int), resolvents.size() + 2, _tmp_rsource_fp);
		}
		
		add_clause_by_resolvents(cl_id, resolvents);
		_current_clause_id = cl_id + 1;
		set_clause_fanout(cl_id, n_fanout);
		break;
	    }
	}	
    }
//    assert (_counter_stream.eof());
}


void CDatabase::dump(void) 
{
//      cout << "p cnf " << _variables.size() - 1 << " " << _num_init_clauses << endl;
//      for (unsigned i=0; i< _clauses.size(); ++i) {
//  	for (unsigned j=0; j< _clauses[i].literals.size(); ++j ) {
//  	    int lit = _clauses[i].literals[j];
//  	    cout << ((lit & 0x1)?"-":"") << (lit>>1) << " ";
//  	}
//  	cout << "0" << endl;
//      }
}

int CDatabase::recursive_find_level(int vid)
{
    int ante = _variables[vid].antecedent;
    assert (_variables[vid].value != UNKNOWN);
    assert (_variables[vid].antecedent != -1);
    assert (_clauses.find(ante) != _clauses.end());;
    if (_variables[vid].level != -1) 
	return _variables[vid].level;
    int level = -1;
    CClause & cl = *_clauses[ante];
    for (unsigned i=0; i<cl.literals.size(); ++i) {
	int v = (cl.literals[i] >> 1);
	int s = (cl.literals[i] & 0x1);
	if (v == vid) {
	    assert(_variables[v].value != s);
	    continue;
	}
	else {
	    assert(_variables[v].value == s);
	    int l = recursive_find_level(v);
	    if (level < l )
		level = l;
	}
    }
    _variables[vid].level = level + 1;
    return level + 1;
}

void CDatabase::set_var_number(int nvar) 
{ 
    _variables.resize(nvar + 1);
    for (unsigned i=0; i < _variables.size(); ++i) {
	_variables[i].value = UNKNOWN;
	_variables[i].in_clause_phase = UNKNOWN;
    }
}

bool CDatabase::real_verify(void)
{
    cout << "Levelize variables... ";
    for (unsigned i=1; i< _variables.size(); ++i) {
	int cl_id = _variables[i].antecedent;
	if (_variables[i].value != UNKNOWN &&  cl_id != -1) {
	    int level = recursive_find_level(i);
//	    cout << "Var: " << i << " level " << level << endl;
	}
    }
    cout << "finished"<< endl;
    //Can we construct an empty clause? 
    cout << "Begin Resolution... " ;
    set<CVariable *, cmp_var_level> clause_lits;
    assert (_clauses.find(_conf_id) != _clauses.end());
    _empty_r_source.push_back(_conf_id);
    CClause & conf_cl = *_clauses[_conf_id];
    for (unsigned i=0; i< conf_cl.literals.size(); ++i) {
	assert (lit_value(conf_cl.literals[i]) == 0);
	int vid = (conf_cl.literals[i] >> 1);
	clause_lits.insert(&_variables[vid]);
    }	
    assert (clause_lits.size() == conf_cl.literals.size());

    while(!clause_lits.empty()) {
//  	for (set<CVariable *, cmp_var_level>::iterator itr = clause_lits.begin();
//  	     itr != clause_lits.end(); ++itr) {
//  	    int vid = (*itr) - &_variables[0];
//  	    cout << vid << "(" << (*itr)->level << ") ";
//  	}
//  	cout << endl;

	int vid = (*clause_lits.begin() - &_variables[0]);
	int ante = _variables[vid].antecedent;
	if (ante == -1) {
	    cerr << "Variable " << vid << " has an NULL antecedent ";
	    exit(1);
	}
	clause_lits.erase(clause_lits.begin());
	_variables[vid].in_clause_phase = 1;
	assert (_clauses.find(ante) != _clauses.end());
	CClause & cl = *_clauses[ante];
	_empty_r_source.push_back(ante);
	int distance = 0;
	for (unsigned i=0; i< cl.literals.size(); ++i) {
	    int l = cl.literals[i];
	    int v = (l>>1);
	    assert (_variables[v].value != UNKNOWN);
	    if (lit_value(l) == 1) {
		if (vid != v) {
		    cerr << "The antecedent of the variable is not really an antecedent " << endl;
		    exit(1);
		}
		else 
		    ++distance;
	    }
	    else
		clause_lits.insert(&_variables[v]);
	}
	assert (distance == 1);
    }
    cout << " Empty clause generated. " << endl;
    if (_output_core) 
	calculate_unsat_core();
    return true;
}

FILE * CDatabase::reverse_file(FILE * fp_in)
{
    //currently fp_in point to the end of the input file
    assert (((unsigned)MAX_BUFF_SIZE > _variables.size() * 2) && 
	    "Buff must be able to contain at least 2 biggest clauses"); 
    int * read_buff;
    read_buff = (int *) malloc((MAX_BUFF_SIZE + _variables.size())*sizeof(int));
    int * write_buff;
    write_buff = (int*) malloc((MAX_BUFF_SIZE + _variables.size()) * sizeof(int));

    long file_size = ftell(fp_in)/sizeof(int);
    assert (ftell(fp_in)%sizeof(int) == 0);
    int num_trunks = file_size/MAX_BUFF_SIZE;

    FILE * fp = tmpfile();
    int last_remain = 0;
    cout << "Reverse Trunk ";
    for (int i=0; i< num_trunks; ++i) {
	cout << i << " ";
	cout.flush();
	fseek(fp_in, -MAX_BUFF_SIZE*sizeof(int), SEEK_CUR);
	int r = fread(read_buff, sizeof(int), MAX_BUFF_SIZE, fp_in);
	assert (r == MAX_BUFF_SIZE);
	fseek(fp_in, -MAX_BUFF_SIZE*sizeof(int), SEEK_CUR);
	int write_idx = 0;
	for (int index=MAX_BUFF_SIZE - 1 + last_remain; index>= 1; --index) {
	    if (read_buff[index] < 0) {
		assert (read_buff[index -1] < 0);
		int num_lits = -read_buff[index];
		-- index;
		for (int j=0; j< num_lits + 2; ++j)
		    write_buff[write_idx ++] = read_buff[j + index];
	    }
	}
	fwrite(write_buff, sizeof(int), write_idx, fp);
	last_remain = 0;
	for (int j = 0; j < MAX_BUFF_SIZE; ++j) {
	    if (read_buff[j] < 0 && read_buff[j+1] < 0)
		break;
	    else 
		read_buff[j + MAX_BUFF_SIZE] = read_buff[j];
	    ++ last_remain;
	}
    }
    cout <<" Last " << endl;
    //the last trunk
    int last_trunk_size = file_size%MAX_BUFF_SIZE;
    int last_trunk_begin = MAX_BUFF_SIZE - last_trunk_size;
    assert (ftell(fp_in) == (long) (last_trunk_size * sizeof(int)));
    fseek(fp_in, -last_trunk_size*sizeof(int), SEEK_CUR);
    fread(read_buff + last_trunk_begin, sizeof(int), last_trunk_size, fp_in);
    int index; 
    int write_idx = 0;

    for (index = MAX_BUFF_SIZE-1 + last_remain; index >= last_trunk_begin + 1; --index) {
	if (read_buff[index] < 0) {
	    assert (read_buff[index -1] < 0);
	    int num_lits = -read_buff[index];
	    -- index;
	    for (int j=0; j< num_lits + 2; ++j)
		write_buff[write_idx ++] = read_buff[j + index];
	}	
    }
    fwrite(write_buff, sizeof(int), write_idx, fp);
    assert (read_buff[last_trunk_begin] < 0 &&
	    read_buff[last_trunk_begin+1] < 0);
    free(read_buff);
    free(write_buff);
    return fp;
}

void CDatabase::print_file(FILE * fp)
{
    ofstream out("out_file");
    int inputs[_variables.size()];
    int info[2];
    int pos = ftell(fp);
    rewind(fp);
    fread(info, sizeof(int), 2, fp);
    while (!feof(fp)) {
	assert (info[0] < 0);
	assert (info[1] < 0);
	unsigned num_in = -info[1];
	assert ( num_in < _variables.size());
	fread(inputs, sizeof(int), num_in, fp);
	out << "CL : " << -info[0] << " Num: "<< num_in << " :";
	for (unsigned i=0; i< num_in; ++i)
	    out << inputs[i] << " ";
	out << endl;
	fread(info, sizeof(int), 2, fp);
    }	
    fseek (fp, pos, SEEK_SET);
}

void CDatabase::calculate_unsat_core(void)
{
    cout << "Begin Output Unsat Core ... ";
    FILE * r_source_fp;
//      FILE * clause_fp;
//      clause_fp = reverse_file(_tmp_clause_fp);
//      close(_tmp_clause_fp);
//      rewind(clause_fp);

    r_source_fp = reverse_file(_tmp_rsource_fp);
    fclose(_tmp_rsource_fp);
    rewind(r_source_fp);
    hash_set<int> involved;
    for (unsigned i=0; i< _empty_r_source.size(); ++i)
	involved.insert(_empty_r_source[i]);

    int r_source[_variables.size()];
    int info[2];
    
    fread(info, sizeof(int), 2, r_source_fp);
    while (!feof(r_source_fp)) {
	assert (info[0] < 0);
	assert (info[1] < 0);
	int cl_id = -info[0];
	unsigned num_srcs = -info[1];
	assert ( num_srcs < _variables.size());
	fread(r_source, sizeof(int), num_srcs, r_source_fp);
	if (involved.find(cl_id) != involved.end()) {
	    involved.erase(cl_id);
	    for (unsigned i=0; i< num_srcs; ++i) {
		int s= r_source[i];
		assert (s < cl_id);
		involved.insert(s);
	    }
	}
	fread(info, sizeof(int), 2, r_source_fp);
    }	
    fclose(r_source_fp);

    int needed_cls_count = 0;
    int needed_var_count = 0;
    for (int i=0; i< num_init_clauses(); ++i) {
	if (involved.find(i) != involved.end()) {
	    assert (_clauses.find(i) != _clauses.end());
	    ++ needed_cls_count;
	    CClause & cl = * _clauses[i];
	    for (unsigned j=0; j< cl.literals.size(); ++j) {
		int vid = (cl.literals[j] >> 1);
		if (_variables[vid].is_needed == false) {
		    ++ needed_var_count;
		    _variables[vid].is_needed = true;
		}
	    }
	}
    }
    cout << "Original Num. Clauses:\t\t\t" << num_init_clauses() << endl;
    cout << "Needed Clauses to Construct Empty:\t"<< needed_cls_count << endl;
    cout << "Total Variable count:\t\t\t" << _variables.size()-1 << endl;
    cout << "Variables involved in Empty:\t\t" << needed_var_count << endl;
    ofstream dump("unsat_core.cnf");
    dump << "c Variables Not Involved: ";
    unsigned int k=0;
    for (unsigned i=1; i< _variables.size(); ++i) {
	if (_variables[i].is_needed == false) {
	    if (k%20 == 0) 
		dump << endl << "c ";
	    ++k;
	    dump << i << " ";
	}
    }
    dump << endl;
    dump << "p cnf " << _variables.size()-1 << " " << needed_cls_count << endl;
    for (int i=0; i< num_init_clauses(); ++i) {
	if (involved.find(i) != involved.end()) {
	    CClause & cl = * _clauses[i];
	    dump << "c Original Cls ID: " << i << endl;
	    for (unsigned j=0; j< cl.literals.size(); ++j)
		dump << ((cl.literals[j] & 0x1)?" -":" ") << (cl.literals[j] >> 1);
	    dump << " 0" << endl;
	}
    }
}


int main(int argc, char * * argv)
{
    cout << "ZVer SAT Solver Verifier" << endl;
    cout << "Copyright Princeton University, 2002-2004. All Right Reseverd." << endl;
    if (argc != 3 && argc != 4) {
	cout << "Usage: verify CNF_File Dump_File [-core]" << endl;
	cout << "-core : output the unsat core of the instance " << endl;
	cout << endl;
	exit(1);
    }
    _peak_mem = get_mem_usage();
    CDatabase dbase;
    double begin_time = get_cpu_time();
    dbase.init(argv[1], argv[2]);

    if (argc == 4) {
	if (strcmp(argv[3], "-core")!=0) {
	    cerr << "The third argument must be -core" << endl;
	    exit(1);
	}
	dbase.set_output_core(true);
    }
    else 
	dbase.set_output_core(false);

    bool status = dbase.verify();

    cout << "Total Runtime\t\t\t\t" << get_cpu_time() - begin_time << endl;
    cout << "Peak Mem Usage\t\t\t\t" << _peak_mem << endl;
    if (status == true) 
	cout << "Verify Successful " << endl;
    else 
	cout << "Failed to verify the result " << endl;

    return (0);
}










