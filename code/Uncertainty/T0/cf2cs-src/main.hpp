#include <vector>
#include "planner.h"

#define FOR(type,it,obj)				\
  for( type it = obj.begin(); it != obj.end(); ++it)

#define FOR_P(type,it,obj)				\
  for( type it = obj->begin(); it != obj->end(); ++it)

#define FOR_0(it,obj)				\
  for( size_t it = 0; it < obj.size(); it++)

#define FOR_1(it,obj)				\
  for( size_t it = 1; it < obj.size(); it++)

EXTERNC int             verbose, verbosity_level;
EXTERNC std::set<int> static_atoms;
EXTERNC bool observation_detected;
EXTERNC bool use_oneof_or;
EXTERNC std::vector<string> atoms;
EXTERNC size_t natoms;
EXTERNC std::vector<int> r2a;
EXTERNC std::vector<Act> actions;
EXTERNC std::vector<Act> observations;

EXTERNC std::set<int> all_atoms;
EXTERNC std::set<int> goal_literals;
EXTERNC std::set<int> precs_literals;

// // Std::Sets used inside t0c.cc
EXTERNC std::set<int> unknown; // lits original mentioned in oneof, or at Init 

EXTERNC std::set<int> simple_unknown; // Unknown not in disj, so of the form p || -p
EXTERNC std::set<int> true_literals; // Literals true at Init, after unit resolution
EXTERNC std::set<int> disj_orig_lits; // Literals at disj_orig
EXTERNC std::set<int> oneof_lits; // Literals at oneof


EXTERNC std::vector<std::set<int> > disj_orig; // Or at Init
EXTERNC std::vector<std::set<int> > disj; // Or + Oneof + exclusion, after unit resolution
EXTERNC std::vector<std::set<int> > oneof; // Real oneof (with trivial p || -p)

EXTERNC std::vector<std::set<int> > g_or; // Goal clauses

EXTERNC const string dummy_pred;
EXTERNC int dummy_pred_n;

void put_spaces( char *s );
void printOperatorGrounded( int *parameters, int schema );
string int2str( int i );
char * operatorName2( int *parameters, int schema );
string set2str( const set<int>& s );
string name_l_old( int lit );
string set_atoms2str( const set<int>& s );
