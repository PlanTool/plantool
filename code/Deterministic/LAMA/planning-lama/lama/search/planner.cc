/*********************************************************************
 * Author: Malte Helmert (helmert@informatik.uni-freiburg.de)
 * (C) Copyright 2003-2004 Malte Helmert
 * Modified by: Silvia Richter (silvia.richter@nicta.com.au),
 *              Matthias Westphal (westpham@informatik.uni-freiburg.de)             
 * (C) Copyright 2008 NICTA and Matthias Westphal
 *
 * This file is part of LAMA.
 *
 * LAMA is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 3
 * of the license, or (at your option) any later version.
 *
 * LAMA is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, see <http://www.gnu.org/licenses/>.
 *
 *********************************************************************/

#include "best_first_search.h"
#include "wa_star_search.h"
#include "ff_heuristic.h"
#include "globals.h"
#include "operator.h"
#include "landmarks_graph.h"
#include "landmarks_graph_rpg_sasp.h"
#include "landmarks_count_heuristic.h"

#include <cassert>
#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <sys/times.h>
#include <climits>

using namespace std;


int save_plan(const vector<const Operator *> &plan, const string& filename, int iteration);

void print_heuristics_used(bool ff_heuristic, bool ff_preferred_operators, 
			   bool landmarks_heuristic, 
			   bool landmarks_heuristic_preferred_operators);

int main(int argc, const char **argv) {
    struct tms start, search_start, search_end;
    struct tms landmarks_generation_start, landmarks_generation_end;
    times(&start);
    bool poly_time_method = false;
    string plan_filename = "sas_plan";
    
    bool ff_heuristic = false, ff_preferred_operators = false;
    bool landmarks_heuristic = false, landmarks_preferred_operators = false;
    bool reasonable_orders = true;
    bool iterative_search = false;

    enum {wa_star, bfs} search_type = bfs;
    if(argc < 2 || argc > 3) {
	std::cout << "Usage: \"search options [outputfile]\"\n";
    }
    else {
	for(const char *c = argv[1]; *c != 0; c++) {
	    if(*c == 'f') {
		ff_heuristic = true;
	    } else if(*c == 'F') {
		ff_preferred_operators = true;
            } else if(*c == 'l') {
                landmarks_heuristic = true; 
            } else if(*c == 'L') {
                landmarks_preferred_operators = true; 
	    } else if(*c == 'w') {
                search_type = wa_star;
	    } else if(*c == 'i') {
                iterative_search = true;
	    } else {
		cerr << "Unknown option: " << *c << endl;
		return 1;
	    }
	}
	if(argc == 3)
	    plan_filename = argv[2];
    }
    if(!ff_heuristic && !landmarks_heuristic) {
	cerr << "Error: you must select at least one heuristic!" << endl
	     << "If you are unsure, choose options \"fFlL\"." << endl;
	return 2;
    }
    cin >> poly_time_method;
    if(poly_time_method) {
	cout << "Poly-time method not implemented in this branch." << endl;
	cout << "Starting normal solver." << endl;
    }

    // Read input and generate landmarks
    bool generate_landmarks = false;
    g_lgraph = NULL; 
    g_lm_heur = NULL;
    if(landmarks_heuristic || landmarks_preferred_operators) 
	generate_landmarks = true;
    times(&landmarks_generation_start);
    read_everything(cin, generate_landmarks, reasonable_orders);
    // dump_everything();
    times(&landmarks_generation_end);
    int landmarks_generation_ms = (landmarks_generation_end.tms_utime - 
				   landmarks_generation_start.tms_utime) * 10;
    if(g_lgraph != NULL) {
	cout << "Landmarks generation time: " << landmarks_generation_ms / 1000.0 
	     << " seconds" << endl;
    }

    // Check whether landmarks were found, if not switch to FF-heuristic.
    if(generate_landmarks && g_lgraph->number_of_landmarks() == 0) {
	cout << "No landmarks found. This should only happen if task is unsolvable." << endl;
	if(landmarks_heuristic) {
	    cout << "Disabling landmarks count heuristic." << endl;
	    landmarks_heuristic = false;
	}

	if(!ff_heuristic) {
	    cout << "Using FF heuristic with preferred operators." << endl;
	    ff_heuristic = true;
	    ff_preferred_operators = true;
	}
    }

    int iteration_no = 0;
    bool solution_found = false;
    int wa_star_weights[] = {10, 5, 3, 2, 1, -1};
    int wastar_bound = -1;
    g_ff_heur = NULL;
    int wastar_weight = wa_star_weights[0];
    bool reducing_weight = true;
    do{
	iteration_no++;
	cout << "Search iteration " << iteration_no << endl;
	if(reducing_weight && wa_star_weights[iteration_no - 1] != -1)
	    wastar_weight = wa_star_weights[iteration_no - 1];
	else {
	    cout << "No more new weight, weight is " << wastar_weight << endl;
	    reducing_weight = false;
	}
	// Initialize search engine and heuristics (this is cheap and we want to vary search type
	// and heuristics, so we initialize freshly in each iteration)
	BestFirstSearchEngine* engine; 
	if(search_type == wa_star)
		// Parameters of WAStar are 1) weight for heuristic, 2) upper bound on solution
		// cost (this cuts of search branches if the cost of a node exceeds the bound), 
		// use -1 for none.
	    engine = new WAStarSearchEngine(wastar_weight, wastar_bound);  
	else
	    engine = new BestFirstSearchEngine;

	print_heuristics_used(ff_heuristic, ff_preferred_operators, 
			      landmarks_heuristic, landmarks_preferred_operators);
	if(landmarks_heuristic || landmarks_preferred_operators) {
	    if(landmarks_preferred_operators)
		if(!g_ff_heur)
		    g_ff_heur = new FFHeuristic;
	    g_lm_heur = new LandmarksCountHeuristic(
		*g_lgraph, *engine, landmarks_preferred_operators, g_ff_heur);
	    engine->add_heuristic(g_lm_heur, landmarks_heuristic,
				  landmarks_preferred_operators);
	}
	if(ff_heuristic || ff_preferred_operators) {
	    if(!g_ff_heur)
		g_ff_heur = new FFHeuristic;
	    engine->add_heuristic(g_ff_heur, ff_heuristic,
				  ff_preferred_operators);
	} 

	// Search
	times(&search_start);
	engine->search();
	times(&search_end);
	int plan_cost = INT_MAX;
	if(engine->found_solution())
	    plan_cost = save_plan(engine->get_plan(), plan_filename, iteration_no);

	engine->statistics();

	int search_ms = (search_end.tms_utime - search_start.tms_utime) * 10;
	cout << "Search time: " << search_ms / 1000.0 << " seconds" << endl;
	int total_ms = (search_end.tms_utime - start.tms_utime) * 10;
	cout << "Total time: " << total_ms / 1000.0 << " seconds" << endl;
	solution_found |= engine->found_solution();

	if(!engine->found_solution())
	    iterative_search = false;

	// Set new parameters for next search
	search_type = wa_star;
	wastar_bound = plan_cost;
	if(wastar_weight <= 2) { // make search less greedy
	    ff_preferred_operators = false;
	    landmarks_preferred_operators = false;
	}

	// If the heuristic weight was already 0, we can only search for better solutions
	// by decreasing the bound (note: this could be improved by making WA* expand 
	// all fringe states, but seems to have little importance).
	if(wastar_weight == 0) {
	    wastar_bound--;
	}

    }
    while(iterative_search);

    return solution_found ? 0 : 1; 
}

int save_plan(const vector<const Operator *> &plan, const string& filename, int iteration) {
    ofstream outfile;
    int plan_cost = 0;
    bool separate_outfiles = true; // IPC conditions, change to false for a single outfile.
    if(separate_outfiles) {
	// Write a separat output file for each plan found by iterative search
	stringstream it_no;
	it_no << iteration;
	outfile.open((filename + "." + it_no.str()).c_str(), ios::out);
    }
    else {
	// Write newest plan always to same output file
	outfile.open(filename.c_str(), ios::out);
    }
    for(int i = 0; i < plan.size(); i++) {
	int action_cost =  plan[i]->get_cost();
	if(g_use_metric)
	    action_cost--; // Note: action costs have all been increased by 1 to deal with 0-cost actions
	plan_cost += action_cost;
	if(!g_use_metric)
	    cout << plan[i]->get_name() << endl;
	else 
	    cout << plan[i]->get_name() << " (" 
		 << action_cost << ")" << endl;
	outfile << "(" << plan[i]->get_name() << ")" << endl;
    }
    outfile.close();
    if(!g_use_metric)
	cout << "Plan length: " << plan.size() << " step(s)." << endl;
    else 
	cout << "Plan length: " << plan.size() << " step(s), cost: " 
	     << plan_cost << "." << endl;
    return plan_cost;
}

void print_heuristics_used(bool ff_heuristic, bool ff_preferred_operators, 
			   bool landmarks_heuristic, 
			   bool landmarks_preferred_operators) {
    cout << "Using the following heuristic(s):" << endl;
    if(ff_heuristic) {
	cout << "FF heuristic ";
	if(ff_preferred_operators)
	    cout << "with preferred operators";
	cout << endl;
    }
    if(landmarks_heuristic) {
	cout << "Landmark heuristic ";
	if(landmarks_preferred_operators)
	    cout << "with preferred operators";
	cout << endl;
    }
}
