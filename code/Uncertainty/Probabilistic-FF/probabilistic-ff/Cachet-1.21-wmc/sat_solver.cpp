/* =========FOR INTERNAL USE ONLY. NO DISTRIBUTION PLEASE ========== */

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
#include <iostream>
#include <fstream>
#include <cstdlib>
#include <cstdio>
#include <set>
#include <vector>
#include "SAT.h"

using namespace std;

#define MAX_LINE_LENGTH    65536
#define MAX_WORD_LENGTH    64

//This cnf parser function is based on the GRASP code by Joao Marques Silva
void read_cnf(SAT_Manager mng, char * filename )
{
    char line_buffer[MAX_LINE_LENGTH];
    char word_buffer[MAX_WORD_LENGTH];
    set<int> clause_vars;
    set<int> clause_lits;
	vector <double> var_weight;
    int line_num = 0;
	bool beforeP = true;

    ifstream inp (filename, ios::in);
    if (!inp) {
	cerr << "Can't open input file" << endl;
	exit(1);
    }
    while (inp.getline(line_buffer, MAX_LINE_LENGTH)) {
	++ line_num;
	if (line_buffer[0] == 'c') { 
	    if (beforeP)
		{
			int original_nodes;
			char * BN = new char[1000];
			char * temp = new char[1000];
			//sscanf (line_buffer, "c %d %s", &original_nodes, BN);
			sscanf (line_buffer, "c %s %s %s %s %s %d", temp, temp, temp, BN, temp, &original_nodes);
			//if (BN[0] == 'B' && BN[1] == 'N')
			if (strcmp(BN, "expanded") == 0)
				SAT_SetBNNodes(mng, original_nodes);
				//cout << "got BN, " << original_nodes << " nodes" << endl;
		}
		else
			continue; 
	}
	else if (line_buffer[0] == 'p') {
		beforeP = false;
	    int var_num;
	    int cl_num;

	    int arg = sscanf (line_buffer, "p cnf %d %d", &var_num, &cl_num);
	    if( arg < 2 ) {
		cerr << "Unable to read number of variables and clauses"
		     << "at line " << line_num << endl;
		exit(3);
	    }
	    SAT_SetNumVariables(mng, var_num); // first element not used.
		var_weight.resize(var_num + 1);		// for weighted counting
		for (int i = 1; i < var_weight.size(); ++i)	// all weight 0.5 by default
			var_weight[i] = 0.5;
	}
	else if (line_buffer[0] == 'w')		// added by sang
	{
		char *lp = line_buffer;
	    char *wp = word_buffer;

		++lp;	// skip w and all spaces after it
		while (*lp && ((*lp == ' ') || (*lp == '\t'))) 
		{
		    lp++;
		}

		// get the var_idx
		while (*lp && (*lp != ' ') && (*lp != '\t') && (*lp != '\n')) 
		{
		    *(wp++) = *(lp++);
		}
		*wp = '\0';                                 // terminate string
		//assert (strlen(word_buffer) != 0);
		int var_idx = atoi (word_buffer);

		// skip all spaces after var_idx
		while (*lp && ((*lp == ' ') || (*lp == '\t'))) 
		{
		    lp++;
		}

		// get the weight
		wp = word_buffer;
		while (*lp && (*lp != ' ') && (*lp != '\t') && (*lp != '\n')) 
		{
		    *(wp++) = *(lp++);
		}
		*wp = '\0';                                 // terminate string
		//assert (strlen(word_buffer) != 0);
		double weight = atof (word_buffer);

		// set the weight of var_idx
		var_weight[var_idx] = weight;
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
			    vector <int> temp;
			    for (set<int>::iterator itr = clause_lits.begin();
				 itr != clause_lits.end(); ++itr)
				{
					temp.push_back (*itr);
					/*// sort the literals	// insertion sort added by sang
					int j = temp.size() - 1;
					for (; j > 0 && *itr < temp[j -1]; --j)
						temp[j] = temp[j - 1];
					if (j < temp.size() - 1)
					{
						temp[j] = * itr;
						cout << "sorted" << endl;
					}*/	// removed, because literals(svar) are already sorted in set
				}
				
			    SAT_AddClause(mng, & temp.begin()[0], temp.size() );
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
    if (!inp.eof()) {
	cerr << "Input line " << line_num <<  " too long. Unable to continue..." << endl;
	exit(2);
    }
//    assert (clause_vars.size() == 0); 	//some benchmark has no 0 in the last clause
    if (clause_lits.size() && clause_vars.size()==clause_lits.size() ) {
	vector <int> temp;
	for (set<int>::iterator itr = clause_lits.begin();
	     itr != clause_lits.end(); ++itr)
	    temp.push_back (*itr);
	SAT_AddClause(mng, & temp.begin()[0], temp.size() );
    }
    clause_lits.clear();
    clause_vars.clear();

	SAT_SetVarWeight(mng, & var_weight);
}


void handle_result(SAT_Manager mng, int outcome, char * filename, bool quiet)
{
  FILE *A;
    char * result = "UNKNOWN";

    double rrr, rtime;

    if ( (A = fopen("A","w")) == NULL ) {
      printf("\n\nCachet: can't open answer file\n\n");
      exit( 1 );
    }
    
    switch (outcome) {
    case SATISFIABLE:
	//cout << "Instance satisfiable" << endl;
//following lines will print out a solution if a solution exist
	/* for (int i=1, sz = SAT_NumVariables(mng); i<= sz; ++i) {
	    switch(SAT_GetVarAsgnment(mng, i)) {
	    case -1:	
		cout <<"("<< i<<")"; break;
	    case 0:
		cout << "-" << i; break;
	    case 1:
		cout << i ; break;
	    default:
		cerr << "Unknown variable value state"<< endl;
		exit(4);
	    }
	    cout << " ";
	} */
	result  = "SAT";
	if ( !quiet )
	cout << endl;
	break;
    case UNSATISFIABLE:
	result  = "UNSAT";
	if ( !quiet )
	cout << endl;
	//cout << "Instance unsatisfiable" << endl << endl;
	break;
    case TIME_OUT:
	result  = "ABORT : TIME OUT"; 
	cout << "Time out, unable to determing the satisfiablility of the instance";
	if ( !quiet )
	cout << endl;
	break;
    case MEM_OUT:
	result  = "ABORT : MEM OUT"; 
	cout << "Memory out, unable to determing the satisfiablility of the instance";
	if ( !quiet )
	cout << endl;
	break;
    default:
	cerr << "Unknown outcome" << endl;
	exit (5);
    }	
    rtime = ((double) SAT_GetCPUTime(mng));
    if (!quiet)
	{
    cout << "Number of Decisions\t\t\t" << SAT_NumDecisions(mng)<< endl;	// order changed
	cout << "Max Decision Level\t\t\t" << SAT_MaxDLevel(mng) << endl;	// order changed
    cout << "Number of Variables\t\t\t" << SAT_NumVariables(mng) << endl;
    cout << "Original Num Clauses\t\t\t" << SAT_InitNumClauses(mng) << endl;
    cout << "Original Num Literals\t\t\t" << SAT_InitNumLiterals(mng) << endl;
    cout << "Added Conflict Clauses\t\t\t" << SAT_NumAddedClauses(mng)- SAT_InitNumClauses(mng)<< endl;
    cout << "Added Conflict Literals\t\t\t" << SAT_NumAddedLiterals(mng) - SAT_InitNumLiterals(mng) << endl;
    cout << "Deleted Unrelevant clauses\t\t" << SAT_NumDeletedClauses(mng) <<endl;
    cout << "Deleted Unrelevant literals\t\t" << SAT_NumDeletedLiterals(mng) <<endl;
    cout << "Number of Implications\t\t\t" << SAT_NumImplications(mng)<< endl;
    //other statistics comes here
    cout << "Total Run Time\t\t\t\t" << rtime << endl << endl;
	}
//    cout << "RESULT:\t" << filename << " " << result << " RunTime: " << SAT_GetCPUTime(mng)<< endl;
	//if (result  == "UNSAT")
	//	cout  << "UNSAT" << endl;
	//else 
    rrr = SAT_SatProb(mng);
	if (SAT_GMP(mng))
		{
			long double solutions = SAT_NumSolutions(mng);
			if (solutions > 1000000 && !quiet)
				cout << "In scientific number form\t\t" << solutions << endl;
		}
	else 
	{
	  if (!quiet) {
	    cout << "Satisfying probability\t\t\t" << rrr << endl;
	    cout << "Number of solutions\t\t\t" << SAT_NumSolutions(mng) << endl;
	  }
	}
	if ( !quiet )
	  cout << endl << endl;

	fprintf(A,"%d %lf %lf\n", outcome, rrr, rtime);
	fclose( A );

}
void output_status(SAT_Manager mng)
{
    cout << "Dec: " << SAT_NumDecisions(mng)<< "\t ";
    cout << "AddCl: " << SAT_NumAddedClauses(mng) <<"\t";
    cout << "AddLit: " << SAT_NumAddedLiterals(mng)<<"\t";
    cout << "DelCl: " << SAT_NumDeletedClauses(mng) <<"\t";
    cout << "DelLit: " << SAT_NumDeletedLiterals(mng)<<"\t";
    cout << "NumImp: " << SAT_NumImplications(mng) <<"\t";
    cout << "AveBubbleMove: " << SAT_AverageBubbleMove(mng) <<"\t";
    //other statistics comes here
    cout << "RunTime:" << SAT_GetElapsedCPUTime(mng) << endl;
}

void verify_solution(SAT_Manager mng)
{
    int num_verified = 0;
    for ( int cl_idx = SAT_GetFirstClause (mng); cl_idx >= 0; 
	  cl_idx = SAT_GetNextClause(mng, cl_idx)) {
	int len = SAT_GetClauseNumLits(mng, cl_idx);
	int * lits = new int[len+1];
	SAT_GetClauseLits( mng, cl_idx, lits);
	int i;
	for (i=0; i< len; ++i) {
	    int v_idx = lits[i] >> 1;
	    int sign = lits[i] & 0x1;
	    int var_value = SAT_GetVarAsgnment( mng, v_idx);
	    if( (var_value == 1 && sign == 0) ||
		(var_value == 0 && sign == 1) ) break;
	}
	if (i >= len) {
	    cerr << "Verify Satisfiable solution failed, please file a bug report, thanks. " << endl;
	    exit(6);
	}
	delete [] lits;
	++ num_verified;
    }
    cout << num_verified << " Clauses are true, Verify Solution successful. ";
}

int main(int argc, char ** argv)
{

    SAT_Manager mng = SAT_InitManager();
	bool quiet = false;	// for output control
	bool static_heuristic = false;
    if (argc < 2) {
	cout << "cachet version 1.21, July 25, 2005" << endl
		 << "copyright 2005, University of Washington" << endl
		 << "incorporating code from zchaff, copyright 2004, Princeton University" << endl
		 << "Usage: "<< argv[0] << " cnf_file [-t time_limit]" 
		 << " [-c cache_size]" 
		 << " [-o oldest_cache_entry]" 
		 << " [-l cache_clean_limit]" 
		 << " [-h heuristic_selection]"
		 << " [-b backtrack_factor]"
		 << " [-f far_backtrack_enabled]"
		 //<< " [-r cross_implication_off] "
		 << " [-a adjusting_component_ordering_off] "
		 << " [-n max_num_learned_clause] " 
		 << " [-q quiet] " << endl;
	return 2;
    }
// 	cout << "cachet version 1.21, July 25, 2005" << endl
// 		 << "copyright 2005, University of Washington" << endl
// 		 << "incorporating code from zchaff, copyright 2004, Princeton University" << endl;
//     cout <<"Solving " << argv[1] << " ......" << endl;
    //if (argc == 2) {
	//read_cnf (mng, argv[1] );
    //}
    //else {
	//read_cnf (mng, argv[1] );
	//SAT_SetTimeLimit(mng, atoi(argv[2]));
    //}

	read_cnf (mng, argv[1] );
	int current = 1, option, static_argv_index;
	while (++current < argc)
	{
		switch (argv[current][1])
		{
			case 't':	SAT_SetTimeLimit(mng, atoi(argv[++current]));
						break;
			case 'c':	SAT_SetCacheSize(mng, atoi(argv[++current]));
						break;
			//case 'e':	SAT_SetMaxEntry(mng, atoi(argv[++current]));
			//			break;
			//case 'm':	SAT_SetMaxDistance(mng, atoi(argv[++current]));
			//			break;
			//case 'd':	SAT_SetDynamicHeuristic(mng, true);
			//			break;
			case 's':	SAT_SetStaticHeuristic(mng, true);	// false -> true
						static_heuristic = true;
						static_argv_index = ++current;	// record the place of static ordering input file
						break;
			case 'h':	option = atoi(argv[++current]);
						if (option < 0 || option > 7)
						{
							cout << "invalid heuristic selection, must be between 0 and 7" << endl;
							exit(0);
						}
						SAT_SetDynamicHeuristic(mng, option);
						break;
			case 'n':	SAT_SetMaxNumLearnedClause(mng, atoi(argv[++current]));
						break;
			case 'o':	SAT_SetOldestEntry(mng, atoi(argv[++current]));
						break;
			case 'l':	SAT_SetCleanLimit(mng, atoi(argv[++current]));
						break;
			case 'r':	SAT_SetCrossFlag(mng, false);	// true -> false
						break;
			case 'a':	SAT_SetAdjustFlag(mng, false);	// true -> false
						break;
			case 'b':	SAT_SetBacktrackFactor(mng, atof(argv[++current]));
						break;
			case 'f':	SAT_SetFarBacktrackFlag(mng, true);
						break;
			case 'q':	SAT_SetQuietFlag(mng, true);	// true -> false
						quiet = true;
						break;
			default:	cout << "unkonwn option! " << argv[current] << endl;
						exit(0);
		}	// end switch
	}

	if (static_heuristic)
	{
		ifstream static_ordering_input(argv[static_argv_index]);
		int var;
		vector<set <int> > static_scores;
		static_scores.push_back(*(new set<int>));
		while (static_ordering_input >> var)
		{
			if (var == 0)
			{
				static_scores.push_back(*(new set<int>));	// start a new group, 0 terminates a variable group
			}
			else
			{
				static_scores.back().insert(var);		// the smaller, the better;
			}
		}
		SAT_SetVGO(mng, static_scores);
		static_ordering_input.close();
	}	// end if (static_heuristic)

    int result = SAT_Solve(mng);
	// cout << "Number of solutions: " << _stats.num_solutions << endl;
#ifndef KEEP_GOING
//    if (result == SATISFIABLE)
//		verify_solution(mng);
#endif
    handle_result (mng, result,  argv[1], quiet);


    return 0;
}
