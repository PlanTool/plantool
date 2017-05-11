%module csp

%include <argcargv.i>

%{
#include "csp.h"
  %}

%apply (int ARGC, char **ARGV) { (int argc, char *argv[]) }

int oldmain( int argc, char *argv[] );
