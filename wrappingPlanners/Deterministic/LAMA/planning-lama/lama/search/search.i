%module search

%include <argcargv.i>

%{
// The header where functions are declared
#include "best_first_search.h"
#include "wa_star_search.h"
#include "ff_heuristic.h"
#include "globals.h"
#include "operator.h"
#include "landmarks_graph.h"
#include "landmarks_graph_rpg_sasp.h"
#include "landmarks_count_heuristic.h"
%}

%apply (int ARGC, char **ARGV) { (int argc, char *argv[]) }

int replacemain( int argc, char *argv[] ); // The function we want to wrap