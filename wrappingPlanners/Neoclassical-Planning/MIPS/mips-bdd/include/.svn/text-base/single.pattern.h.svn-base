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
//  Module:     mips\include\single.pattern.h
//  Authors:    S.Edelkamp, M.Helmert
// 
// explicit pattern database implementation, invoked by 
//  single pdb and opposes symbolic implementation with 
//  bdds
//
// ***********************************************************

#ifndef _SINGLE_PATTERN_H
#define _SINGLE_PATTERN_H

#include <vector>
#include <list>

using namespace std;

#include <bdd.h>
#include <util.queue.h>
#include <util.bitarray.h>
#include <single.heuristic.h>


class FactMap;
class Operator;
class HashTable;

class Pattern {
 protected:
  HashTable* fHashTable;   // full Hashtable for search
  int factCount;           // number of facts to deal with

  BitArray processedFacts; // bitarray to store sets of facts efficiently
  BitArray enqueuedFacts;  // keeps track of elements in the queue
  BitArray zeroFacts;      // for resetting a vector 
  BitArray markedFacts;    // to avoid recomputations
  BitArray stateFacts;     // intermediate vector

  FactMap& factMap;       // mapping from all facts to used ones
//  Operator** operators;   // set of operators in the space
//  int oSize;              // upper bound on the previous
  Operator** redOps;      // set of reduced operators in the space
  int redOpSize;          // upper bound on the previous

  Queue<int>* queue;      // queue for complete breadth first search
  short* HT;              // perfect hash table, storing dist. to goal

  int expansions;         // maintain number of expansions for statistics
  int generations;        // maintain number of expansions for statistics
  double stateSpace;      // size of state space, double to avoid int overflow
  double* multipliers;    // offsets for perfect perfect hash function address
  
  State* initState;       // abstract initial state
  State* goalState;       // abstract goal state
  State* succState;       // one successor state 
  State* enlargedState;   // enlarge state to current abstraction level
  State* state;           // currently expanded state
  int* FactArray;         // int representation of propositional bit vector
  int fSize;              // size of the above array
  
  int max;
  int* group;             // constant time access to group of atom
  int* position;          // constant time access of postition in group
  bool* isActive, *goalActive; // mask array to indicate current abstraction

  // methods

  void generateFacts(int hval);        
  // subprocedure to extent underspecified state
  void insert(BitArray& state, int g); // inserts one encountered state 
  int perfectHash(int* FactArray, int fSize); 
  // perfect hash retrieval
  int enlargeState(BitArray& state, int i);
 public:
  Pattern(FactMap& fMap,State* init, State* goal, Operator** operate, 
      int osize, bool* active, bool* goalactive, int* gr, int* pos);
  


  virtual double BFS();
  // construction of explicit pattern database, called by constructor
  int size();            
  // size of explicit pattern database
  bdd getBddHeuristic();
  virtual int heuristic(int* FactArray, int fSize);
  // retrieval of hash function value
  string toString(); // output routine
  
  //made public from protected
    int oSize;              // upper bound on the previous
      Operator** operators;   // set of operators in the space
};
  

#endif
