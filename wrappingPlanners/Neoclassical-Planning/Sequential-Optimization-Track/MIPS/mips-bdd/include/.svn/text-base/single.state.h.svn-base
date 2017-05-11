// ***********************************************************
// 
//  Book:       Heuristic Search
// 
//  Authors:    S.Edelkamp, S.Schroedl
// 
//  See file README for information on using and copying 
//  this software.
// 
//  Project:    Mips - model checking integrated planning
//              system
// 
//  Module:     mips\include\single.state.h
//  Authors:    S.Edelkamp, M.Helmert
// 
//  The propositional state to be expanded. Together with the
//  bitvector the handle to the interface and the 
//  g and h values are provided. Moreover for retrieving
//  the solution path the index of the grounded operator
//  is appended.
// ***********************************************************

#ifndef _STATE_H_
#define _STATE_H_

class Predicate;
class Domain;
class Action;
class FactMap;
class BitArray;

#include <vector>
#include <list>
#include <string>

using namespace std;

class State;

typedef struct StateType {
  State *the_state;
  double diff;             // weight of edge for backward traversal
  struct StateType *link;
} StateType;

class State {

 protected:
  FactMap* factMap;
  BitArray* bitvector;

  // caution: no encapsulation of the following variables
 public:
  
  float time;               // duration
  int g;                 // generating path length
  int h;                 // expected goal distance in number of edges
  int action;            // executed action to generate this state
  State* pred;           // predecessor in solution
  //  StateType *brother;    // neighbor for backward traversal 
  //  double dist;           // pdb lookup value

  // member functions

  BitArray* getVector() // get bitvector representation of state
    { return bitvector; } 
  int size();          // bit vector size of state
  bool get(int fact);  // is fact present in current state

  // methods
  
  State(FactMap& fMap);  // construct dummy state
  State(vector<int>& facts, FactMap& fMap); 
  // construct state given int vector of atoms
  State(const State& copy);  // copy state
  ~State();                  // delete state
  State(BitArray& init, FactMap& fMap); // construct state with bit vector
  virtual int hash(int max);
  // compute hash value for hash table of size max
  void project(int* group, bool* isActive);
  // project state according to provided mask
  void enlarge(int* group, bool* isActive);
  // enlarge state upto provided mask
  string toString();  // output state for debugging
  string visString(); // output for visualizer
};

#endif
