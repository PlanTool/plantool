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
//  Module:     mips\include\single.ida.h
//  Authors:    S.Edelkamp, M.Helmert
// 
//  Korf's proposal for linear space implementation of A* algorithm.
//  Bounded depth-first search traversals for set of states with 
//  f-values smaller than thershold thresh, next theshold is fstar.
//  Set of visited states are maintained in Reinefeld and Marsland's 
//  transposition table. Refinement Partial IDA* (cf. Edelkamp and
//  Meyer, KI 2001) store set of visited states in bit-state hash 
//  table to increase coverage to detect duplicates.
//
// ***********************************************************

#ifndef __IDA_STAR_TT_H__
#define __IDA_STAR_TT_H__

#include <vector>
#include <list>

using namespace std;

#include <single.search.h>

class FactMap;
class State;
class BitArray;


class IDAStar: public Search {
  int index, start;             // to generated successor set
  int fstar, f;                 // next threshold and state evaluation func
  int found, hval, hval2, hval3; // different hashindices
  float WH;                     // weightening of h-value in evaluation func
  int MAXF; // maximum f value for exploration, sentinal for next threshold
  BitArray* bst;                // bit-state hash table 

  State* state, *succState;     // current and list of successor states

  State* IDAsearch(int thresh, State* state); 
  // one bounded depth-first iteration step
 public:
  IDAStar(FactMap& fMap);       // initializes data structures
  int search(Timer & t, State*& finalState);
  // inherited search procedure executes main loop
};

#endif
