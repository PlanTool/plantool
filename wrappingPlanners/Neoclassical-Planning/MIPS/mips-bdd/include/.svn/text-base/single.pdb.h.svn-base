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
//  Module:     mips\include\single.pdb.h
//  Authors:    S.Edelkamp, M.Helmert
// 
// Pattern database heuristic with automated computation of
// different abstractions. Each pattern database computed
// either explicitly or symbolically.
//
// ***********************************************************

#ifndef _SINGLE_PDB_H
#define _SINGLE_PDB_H

#include <vector>
#include <list>

using namespace std;

#include <util.queue.h>
#include <single.heuristic.h>
// #include <bdd.h>


class State;
class FactMap;
class Operator;
class HashTable;
class Mem;
class Pattern;
class Group;

class PDB : public Heuristic {
    // enum {TIMEOUT,BACKTRACK,OVERFLOW,FALSE,TRUE};
  int factCount;           // number of facts to deal with
  Pattern** P;             // array of pattern data bases
  int abstraction;         // number of abstractions
  int stateSpace;          // (expected) size of state to initialize arrays
  int* multipliers;        // hash function

  State* initState;        // overall initial state
  State* goalState;        // overall goal state
  
  int max;
  int* group;              // group decompostion of facts
  int* position;           // position of fact in fact group
  bool** isActive, *goalActive;  
  // computed partition into different abstractions
  // in every abstraction a list of active groups is selected
  double* groupSizes;      // array of group sizes of adic representation
  int encodingSizeClassic();		// binary encoding size of propositional part
  int encodingSizeGenetic();			// with genetic algorithms
  int encodingSize();

  int* start;        // improvement of relaxed plan heuristics 
  int** graph;       // the graph of dependencies
  int* outdeg;       // the maintained list of out-degrees
  int* indeg;        // the maintained list of in-degrees
  int sum;
  int maximum;

 public:
  
  PDB(FactMap& fMap, State* initState, State* goalState,    
      Operator** operate, int osize,           
      Operator*** preactions, int* asize);
  // constructor  

  int getValue(int i, bdd in);
  bdd getBdd(int i, int j) ;
  bdd getBdd(int index, bdd in);

  int getValue(bdd in) ;

  bool isSum(int i, int j, int sum) { 
      return i+j == sum; 
  }
  bool isSum(int i, int j, int k, int sum) { 
      return i+j+k == sum; 
  }
  bool isSum(int i, int j, int k, int l, int sum) { 
      return i+j+k+l == sum; 
  }
  bool isSum(int i, int j, int k, int l, int m, int sum) { 
      return i+j+k+l+m == sum; 
  }
  bool isMax(int i, int j, int maxim) { 
      return (i > j) ? j == maxim : i == maxim; 
  }
  bool isMax(int i, int j, int k, int maxim) { 
      int maximum = (i > j) ? i : j;
      return k > maximum ? k == maxim : maximum == maxim; 
  }
  bool isMax(int i, int j, int k, int l, int maxim) { 
      int maximum = (i > j) ? i : j;
      maximum = (k > maximum) ? k : maximum;
      return (l > maximum) ? l == maxim : maximum == maxim; 
  }
  bool isMax(int i, int j, int k, int l, int m, int maxim) {
      int maximum = (i > j) ? i : j;
      maximum = (k > maximum) ? k : maximum;
      maximum = (l > maximum) ? l : maximum;
      return (m > maximum) ? m == maxim : maximum == maxim; 
  }
  
  virtual int getRelaxedPlan(Operator**& operQueue);
  // not implemented for this heuristic yet
  virtual double estimate(State* state) { return 0.0; }
  // dummy for real time estimate
  bdd getBddHeuristic();
  // returns monolithical heuristic, not in use when partitioning it
  bdd evaluate(bdd states, bool pre);

  int simulate(State* from, int nr, Operator** ops);
  // improvement of relaxed planning heuristic by relaxed
  // plan critics. increases base on topological sort in 
  // fact groups projections
  int simulate(int fact, int nr, Operator** ops, int g);
  // subprocedure of the above
  int similar(int f1, int f2);
  // subprocedure of the above
  int estimateOne(bdd states);

  int calculate(State* from, State* to);
  // interface to overall search algroithm
  int improve(State* from, State* to, int*& matches);
  // improvement of relaxed planning heuristic by 
  int exploreSpace(Timer& globalTimer, int timebound);
  int size();
  // total of pattern database sizes
  string toString(); 
  // output routine to display abstractions 
};
  

#endif
