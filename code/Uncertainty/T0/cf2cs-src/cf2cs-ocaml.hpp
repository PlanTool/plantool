#include <vector>
#include <string>
#include <iostream>

#include "nnf.h"
#include "planner.h"

void usage( std::ostream &os );
void parseArg( int argc, char **argv );
void do_cf2cs();
void prepare_action( Act& a );

#define EXTERNC extern

