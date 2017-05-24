// FF_X.i
%module FF_X

%include <argcargv.i>

%{
#include "ff.h" // The header where functions are declared
%}

%apply (int ARGC, char **ARGV) { (int argc, char *argv[]) }

int FF_X( int argc, char *argv[] ); // The function we want to wrap
