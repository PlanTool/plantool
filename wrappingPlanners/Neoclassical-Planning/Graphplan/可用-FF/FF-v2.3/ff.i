%module ff

%include <argcargv.i>

%{
#include "ff.h"
%}

%apply (int ARGC, char **ARGV) { (int argc, char *argv[]) }

%include "ff.h"
int oldmain( int argc, char * argv [] );