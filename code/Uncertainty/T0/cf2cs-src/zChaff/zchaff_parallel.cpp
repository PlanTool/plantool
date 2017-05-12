#include <pthread.h>
#include <vector.h>
#include <stdio.h>
#include <stdlib.h>
#include <iostream.h>
#include <fstream.h>
#include <set.h>
#include "zchaff_solver.h"

#define MAX_LINE_LENGTH 64000
#define MAX_WORD_LENGTH 128

#define DBG(x)	

class CLemma {
public:
    int _id;
    unsigned _access_flag;
    vector<int> _lits;
};

int _num_threads;
pthread_t * _threads;
CSolver * _solvers;

pthread_mutex_t _lemma_lock;
int _lemma_id;
vector<CLemma *> _lemma_pool;

pthread_mutex_t _inited_lock;
int _num_inited;
pthread_cond_t _all_inited_signal;

pthread_mutex_t	_finished_lock;
int _finished_id;
pthread_cond_t _finished_signal;

int _num_variables;
int _time_limit;
vector<vector<int> *> _orig_clauses;

unsigned _full_flag;

void init(void)
{
    pthread_mutex_init(& _lemma_lock, NULL);
    pthread_mutex_init(& _inited_lock, NULL);
    pthread_mutex_init(& _finished_lock, NULL);
    pthread_cond_init( & _all_inited_signal, NULL);
    pthread_cond_init( & _finished_signal, NULL);
}

void add_lemma(CSolver * solver, int * literals, int num_lits, int cid)
{
    int id;
    CLemma * lemma = new CLemma;
    lemma->_lits.resize(num_lits);
    for (int i=0; i< num_lits; ++i)
	lemma->_lits[i] = literals[i];
    lemma->_access_flag = (_full_flag & (~(1<<solver->id())));
    pthread_mutex_lock(&_lemma_lock);
    lemma->_id = _lemma_id ++;
    _lemma_pool.push_back(lemma);
    DBG (cout << "Solver "<< solver->id() << " Add Lemma ID: " << lemma->_id << "  : ";
	 for (int i=0; i< num_lits; ++i)
	     cout << ((literals[i]&1)?"-":"+") << (literals[i]>>1) << " ";
	 cout << endl; );
    pthread_mutex_unlock(&_lemma_lock);
}

//  ofstream os0("lemma_dump0");
//  ofstream os1("lemma_dump1");

void learn_lemma(CSolver * solver)
{
    vector<int> literals;
    pthread_mutex_lock(&_lemma_lock);
    for (int i=0; i< _lemma_pool.size(); ++i) {
	CLemma * lemma = _lemma_pool[i];
	if ((lemma->_access_flag & (1 << solver->id())) != 0) {
	    literals = lemma->_lits;
	    lemma->_access_flag = (lemma->_access_flag & (~(1<<solver->id())));
	    DBG (cout << "Solver "<< solver->id() << " learning Lemma ID: " << lemma->_id << endl;);
	    
//  	    if (solver->id() == 0) {
//  		for (int k=0; k< lemma->_lits.size(); ++k)
//  		    os0 << ((lemma->_lits[k]&1)?"-":"") << (lemma->_lits[k]>>1) << " ";
//  		os0 <<" 0" <<  endl;
//  	    }
//  	    else {
//  		for (int k=0; k< lemma->_lits.size(); ++k)
//  		    os1 << ((lemma->_lits[k]&1)?"-":"") << (lemma->_lits[k]>>1) << " ";
//  		os1 <<" 0" <<  endl;
//  	    }

	    if (lemma->_access_flag == 0) {
		DBG (cout << "Solver "<< solver->id() << " removing Lemmma ID: " << lemma->_id << endl;);
		_lemma_pool[i] = _lemma_pool.back();
		_lemma_pool.pop_back();
	    }
	    break;
	}
    }
    pthread_mutex_unlock(&_lemma_lock);
    if (literals.size())
	solver->add_clause_incr(literals.begin(), literals.size(),0);
}

void * run_sat_solver( void * input_solver) 
{
    //a. set the original clauses
    CSolver * solver = (CSolver *) input_solver;
    DBG( int id = solver->id();
	 cout << "Solver " << id << " begin reading clauses " << endl;);
    solver->set_variable_number(_num_variables);
    for (int i=0; i< _orig_clauses.size(); ++i) {
	solver->add_orig_clause(_orig_clauses[i]->begin(), _orig_clauses[i]->size());
    }
    DBG( cout << "Solver " << id << " finished reading clauses " << endl;);
    //b. check if we can signal the master that it's fine to release the
    //memory occupied by original clauses
    pthread_mutex_lock( &_inited_lock );
    ++ _num_inited;
    if (_num_inited == _num_threads) {
	pthread_cond_signal( & _all_inited_signal);
	DBG ( cout << "Solver " << id << " Signal init all finished " << endl; );
    }
    pthread_mutex_unlock( & _inited_lock);
    //c. set the handler, the hook function and begin the solve
    solver->add_conflict_hook(add_lemma);
    solver->add_outside_constraint_hook(learn_lemma);
    solver->set_time_limit(_time_limit);
    //d. Run the solver
    DBG ( cout << "Solver " << id << " begins solve " << endl;);
    solver->solve();
    DBG ( cout << "Solver " << id << " ends solve " << endl;);
    //d. the solver function exit, so report that solving is finished
    pthread_mutex_lock( &_finished_lock);
    if (_finished_id == -1) {
	_finished_id = solver->id();
	DBG ( cout << "Solver " << id << " signal finished searching" << endl;);
	pthread_cond_signal( & _finished_signal);
    }
    pthread_mutex_unlock( & _finished_lock);
    //e. exit the routine
    pthread_exit(NULL);
}

int sat_solve(int n)
{
    int rc;
    _num_threads = n;
    _threads = (pthread_t *) malloc ( sizeof (pthread_t) * n);
    _solvers = new CSolver[n];
    _num_inited = 0;
    _finished_id = -1;
    _lemma_id = 0;
    _full_flag = 0;
    for (int i=0; i< n; ++i) 	
	_full_flag |= (1 << i);
    DBG(cout << "Master begin creating threads " <<endl;);
    cout <<"Self is " <<  pthread_self() << endl;
    for (int i=0; i< n; ++i) {
	_solvers[i].set_id(i);
	rc = pthread_create( & _threads[i], NULL, run_sat_solver, (void *)&(_solvers[i]));
	cout <<"Self is " <<  pthread_self() << "New is " << _threads[i] <<  endl;
	if (rc) {
	    cerr << "Can't create thread "<< i << "  Error Code: "<< rc << endl;
	    exit(-1);
	}
	DBG( cout << "Master created thread " << i << endl;);
    }

    pthread_mutex_lock(& _inited_lock);
    if (_num_inited != _num_threads) {
	DBG ( cout << "Master wating for all thread to be initialized..... " << endl; );
	pthread_cond_wait( & _all_inited_signal, & _inited_lock);
    }
    pthread_mutex_unlock( & _inited_lock);
    DBG ( cout << "Master receive signal that all thread are initialized, clean up " << endl; );
    for (int i=0; i< _orig_clauses.size(); ++i)
	delete _orig_clauses[i];
    
    pthread_mutex_lock(& _finished_lock);
    if (_finished_id == -1) {
	DBG ( cout << "Master wating for threads to finish..... " << endl; );
	pthread_cond_wait(& _finished_signal,&_finished_lock);
    }
    DBG ( cout << "Master receive signal that one threads have finished" << endl; );
    for (int i=0; i< n; ++i) {
	if (i == _finished_id)
	    continue;
	else {
	    DBG( cout << "Master terminate solver " << i << endl; );
	    _solvers[i].force_time_out();
	}
    }
    pthread_mutex_unlock( &_finished_lock);
    for (int i=0; i< n; ++i) 
	pthread_join(_threads[i], NULL);
    DBG ( cout << "Master found all threads exited, exit itself " << endl;);
    return _solvers[_finished_id].outcome();
}

void read_cnf(char * filename )
{
    char line_buffer[MAX_LINE_LENGTH];
    char word_buffer[MAX_WORD_LENGTH];
    set<int> clause_vars;
    set<int> clause_lits;

    ifstream inp (filename, ios::in);
    if (!inp) {
	cerr << "Can't open input file" << endl;
	exit(1);
    }
    while (inp.getline(line_buffer, MAX_LINE_LENGTH)) {
	if (inp.fail()) {
	    cerr << "Too large an input line. Unable to continue..." << endl;
	    exit(2);
	}
	if (line_buffer[0] == 'c') { continue; }
	else if (line_buffer[0] == 'p') {
	    int var_num;
	    int cl_num;

	    int arg = sscanf (line_buffer, "p cnf %d %d", &var_num, &cl_num);
	    if( arg < 2 ) {
		cerr << "Unable to read number of variables and clauses" << endl;
		exit(3);
	    }
	    _num_variables = var_num;
	}
	else {                             // Clause definition or continuation
	    char *lp = line_buffer;
	    do {
		char *wp = word_buffer;
		while (*lp && ((*lp == ' ') || (*lp == '\t'))) {
		    lp++;
		}
		while (*lp && (*lp != ' ') && (*lp != '\t') && (*lp != '\n')) {
		    *(wp++) = *(lp++);
		}
		*wp = '\0';                                 // terminate string

		if (strlen(word_buffer) != 0) {     // check if number is there
		    int var_idx = atoi (word_buffer);
		    int sign = 0;

		    if( var_idx != 0) {
			if( var_idx < 0)  { var_idx = -var_idx; sign = 1; }
			clause_vars.insert(var_idx);
			clause_lits.insert( (var_idx << 1) + sign);
		    } 	
		    else {
			//add this clause
			if (clause_vars.size() != 0 && (clause_vars.size() == clause_lits.size())) { //yeah, can add this clause
			    vector<int> * temp = new vector<int>;
			    for (set<int>::iterator itr = clause_lits.begin();
				 itr != clause_lits.end(); ++itr)
				temp->push_back (*itr);
			    _orig_clauses.push_back(temp);
			    
			}
			else {} //it contain var of both polarity, so is automatically satisfied, just skip it
			clause_lits.clear();
			clause_vars.clear();
		    }
		}
	    }
	    while (*lp);
	}
    }
//    assert (clause_vars.size() == 0); 	//some benchmark has no 0 in the last clause
    if (clause_lits.size() && clause_vars.size()==clause_lits.size() ) {
	vector<int> * temp = new vector<int>;
	for (set<int>::iterator itr = clause_lits.begin();
	     itr != clause_lits.end(); ++itr)
	    temp->push_back (*itr);
	_orig_clauses.push_back(temp);
    }
    clause_lits.clear();
    clause_vars.clear();
}

void handle_result(int outcome, char * filename )
{
    CSolver & sv = _solvers[_finished_id];
    cout << "Instance " << filename << endl;
    switch(outcome) {
    case SATISFIABLE:
	cout << "Instance satisfiable" << endl;
//following lines will print out a solution if a solution exist
//      for (int i=1; i<= _num_variables; ++i) {
//          switch(sv.variable(i).value()) {
//          case -1:
//              cout <<"("<< i<<")"; break;
//          case 0:
//              cout << "-" << i; break;
//          case 1:
//              cout << i ; break;
//          default:
//              cerr << "Unknown variable value state"<< endl;
//              exit(4);
//          }
//          cout << " ";
//      }
//          cout << endl;
	break;
    case UNSATISFIABLE:
	cout << "Instance unsatisfiable" << endl;
	break;
    case TIME_OUT:
	cout << "Time out, unable to determine the satisfiablility of the instance" << endl;
	break;
    case MEM_OUT:
	cout << "Memory out, unable to determine the satisfiablility of the instance" << endl;
	break;
    }
    cout << "Original Num Variables\t\t\t" << _num_variables << endl;
    cout << "Original Num Clauses\t\t\t" << sv.init_num_clauses() << endl;
    cout << "Original Num Literals\t\t\t" << sv.init_num_literals() << endl << endl;
    for (int i=0; i< _num_threads; ++i ) {
	CSolver & solver = _solvers[i];
	char * result = "UNKNOWN";
	switch (solver.outcome()) {
	case SATISFIABLE:
	    result  = "SAT";
	    break;
	case UNSATISFIABLE:
	    result  = "UNSAT";
	    break;
	case TIME_OUT:
	    result  = "ABORT : TIME OUT"; 
	    break;
	case MEM_OUT:
	    result  = "ABORT : MEM OUT"; 
	    break;
	default:
	    cerr << "Unknown outcome" << endl;
	    exit (5);
	}	
	cout << "Thread  " << i  << ":" << result << endl;
	cout << "\tMax Decision Level\t\t\t" << solver.max_dlevel() << endl;
	cout << "\tNum. of Decisions\t\t\t" << solver.num_decisions() << endl;
	cout<<_stats.num_decisions_stack_conf <<" stack_conf ; ";
	cout<<_stats.num_decisions_stack_orig <<" stack_orig ; ";
	cout<<_stats.num_decisions_vsids <<" vsids ;";
 	cout << "\tAdded Conflict Clauses\t\t\t" << solver.num_added_clauses() - solver.init_num_clauses()<< endl;
	cout << "\tAdded Conflict Literals\t\t\t" << solver.num_added_literals() - solver.init_num_literals()<< endl;
	cout << "\tDeleted Unrelevant clause\t\t" << solver.num_deleted_clauses() <<endl;
	cout << "\tDeleted Unrelevant literals\t\t" << solver.num_deleted_literals() <<endl;
	cout << "\tNumber of Implication\t\t\t" << solver.num_implications()<< endl;
	//other statistics comes here
	cout << "\tTotal Run Time\t\t\t\t" << solver.cpu_run_time() << endl << endl;
    }
}

void verify_solution(CSolver * solver)	
{
    for (int i=0; i< solver->init_num_clauses(); ++i) {
	int j;
	for (j=0; j< solver->clause(i).num_lits(); ++j) {
	    int lit = solver->clause(i).literal(i).s_var();
	    int vid = lit >> 1;
	    int sign = (lit & 1);
	    if (solver->variable(vid).value() == !sign)
		break;
	}
	if (j >= solver->clause(i).num_lits())
  	    cerr << "Verify Satisfiable solution failed, please file a bug report, thanks. " << endl;
  	    exit(1);
    }
    cout << "Verify Solution successful " << endl;
}    

void clean_up(void)
{
    pthread_mutex_destroy(& _lemma_lock);
    pthread_mutex_destroy(& _inited_lock);
    pthread_mutex_destroy(& _finished_lock);
    pthread_cond_destroy( & _all_inited_signal);
    pthread_cond_destroy( & _finished_signal);
    free (_threads);
    delete [] _solvers;
    for (int i=0; i< _lemma_pool.size(); ++i) 
	delete _lemma_pool[i];
}

int main(int argc, char * * argv) 
{
    int num_thread;
    if (argc < 3) {
	cerr << "Z-Chaff: Parallel Accelerated Sat solver from Princeton. " << endl;
	cerr << "Copyright 2001-2004, Princeton Univ." << endl << endl;;
	cerr << "Usage: "<< argv[0] << " cnf_file num_threads [time_limit]" << endl;
	return 2;
    }
    cout <<"Solving " << argv[1] << " ......" << endl;
    if (argc == 3) {
	read_cnf (argv[1] );
	num_thread = atoi(argv[2]);
	_time_limit = 3600 * 24;
    }
    else {
	read_cnf (argv[1] );
	num_thread = atoi(argv[2]);
	_time_limit = atoi(argv[3]);
    }
    int result = sat_solve(num_thread);
    if (result == SATISFIABLE) 
	verify_solution(& _solvers[_finished_id]);
    handle_result (result,  argv[1]);
    clean_up();
    pthread_exit(NULL);
}
