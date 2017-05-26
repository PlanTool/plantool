%module preprocess

%include <argcargv.i>

%{
#include "helper_functions.h"
#include "successor_generator.h"
#include "causal_graph.h"
#include "domain_transition_graph.h"
#include "state.h"
#include "operator.h"
#include "axiom.h"
#include "variable.h"
%}

%apply (int ARGC, char **ARGV) { (int argc, char *argv[]) }

int oldmain(); // The function we want to wrap