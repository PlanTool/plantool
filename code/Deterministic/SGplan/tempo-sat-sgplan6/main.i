%module sgplan

%include <argcargv.i>

%apply (int ARGC, char **ARGV) { (int argc, char *argv[]) }

%{
#define SWIG_FILE_WITH_INIT
#include "main.h"
%}

extern int run( int argc, char *argv[] );
