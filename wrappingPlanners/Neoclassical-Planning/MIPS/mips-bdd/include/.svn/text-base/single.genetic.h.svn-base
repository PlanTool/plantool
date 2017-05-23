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

#ifndef _SINGLE_GEN_H
#define _SINGLE_GEN_H

#include <vector>
#include <list>

using namespace std;

#include <util.queue.h>
#include <util.bitarray.h>
#include <single.heuristic.h>
// aus gruenden, welche durchaus als mysterioes zu bezeichnen sind treten bei einkommentieren seltsamste fehler auf...
//#include <out.group.h>

// GALIB
#include <ga/GASimpleGA.h>      // we're going to use the simple GA
#include <ga/GA2DBinStrGenome.h> // and the 2D binary string genome
#include <ga/std_stream.h>

class FactMap;
class Operator;
class HashTable;

class GEN {
 protected:
// ours:
  int popSize;			// first dimension:  static populationsize
  double* fitnessValues;
  int temppatternSize;  // second dimension: number of Patterns
  int groupSize;		// third dimension:	 static number of groups
  bool*** population;	// Here they are...
  int *populationSizes;// Number of Patterns in Invidiuums

// classical:
  HashTable* fHashTable;   // full Hashtable for search
  int factCount;           // number of facts to deal with
  BitArray processedFacts; // bitarray to store sets of facts efficiently
  BitArray enqueuedFacts;  // keeps track of elements in the queue
  BitArray zeroFacts;      // for resetting a vector
  BitArray markedFacts;    // to avoid recomputations
  BitArray stateFacts;     // intermediate vector
//  Operator** operators;   // set of operators in the space
//  int oSize;              // upper bound on the previous
  Operator** redOps;      // set of reduced operators in the space	// !!!
  int redOpSize;          // upper bound on the previous			// !!!
  Queue<int>* queue;      // queue for complete breadth first search // !!!
  short* HT;              // perfect hash table, storing dist. to goal // !!!
  int expansions;         // maintain number of expansions for statistics
  int generations;        // maintain number of expansions for statistics
  double stateSpace;      // size of state space, double to avoid int overflow	// !!!
  double* multipliers;    // offsets for perfect perfect hash function address	// !!!
//  State* initState;       // abstract initial state
  State* succState;       // one successor state	// !!!
  State* enlargedState;   // enlarge state to current abstraction level	// !!!
  State* state;           // currently expanded state
  int* FactArray;         // int representation of propositional bit vector
  int fSize;              // size of the above array
  int max;
//  bool* isActive, *goalActive; // mask array to indicate current abstraction
  int* indicateSelection;
  double selectnr;

  // methods

 public:
  GEN(FactMap& fMap,State* init, State* goal, Operator** operate,
      int osize, int* gr, int* pos, bool* goalActive);
  static float Objective(GAGenome &);
  void evaluiere();
  bool** transfer;
  int transferoffset;
  int getGroupSize() {return groupSize;}
  FactMap& factMap;       // mapping from all facts to used ones
  int* group;             // constant time access to group of atom
  int* position;          // constant time access of postition in group
  State* goalState;       // abstract goal state
  vector<int> abb;
  int abbild(int i) {return abb[i];}
  Operator** operators;   // set of operators in the space
  State* initState;       // abstract initial state
  int oSize;              // upper bound on the previous
  bool* isActive, *goalActive; // mask array to indicate current abstraction
};


#endif
