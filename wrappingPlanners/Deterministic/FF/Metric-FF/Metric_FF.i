//Metric_FF.i
%module Metric_FF

%include <argcargv.i>

%{
#include "ff.h" // The header where functions are declared
%}

%apply (int ARGC, char **ARGV) { (int argc, char *argv[]) }

int run( int argc, char *argv[] ); // The function we want to wrap
