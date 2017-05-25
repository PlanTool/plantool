%module ipp

%include <argcargv.i>

%{
#include "ipp.h"
%}

%apply (int ARGC, char **ARGV) { (int argc, char *argv[]) }

int run( int argc, char *argv[] );
