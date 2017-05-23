%module ipp

%include <argcargv.i>

%{
#include "ipp.h"
%}

%apply (int ARGC, char **ARGV) { (int argc, char *argv[]) }

int oldmain( int argc, char *argv[] );
