%module graphplan

%include <argcargv.i>

%{
#include "graphplan.h"
%}

%apply (int ARGC, char **ARGV) { (int argc, char *argv[]) }

int oldmain( int argc, char *argv[] );
