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
//  Module:     mips\include\single.heuristic.h
//  Authors:    S.Edelkamp, M.Helmert
// 
//  Base class of all heuristic functions, also specifying
//  important successor generation function. More of  
//  90 % of implementation is not in use, since source 
//  correspond to idea of partial order reduction in explicit
//  state model checking based on Peleds ample set.
//  cf. Lluch-Lafuente, Edelkamp, Leue, SPIN 2002
//  Core problems: in planning fact group projections are
//  not asynchronous and very few transitions are local.
//  Note that partial order reduction retains completeness
//  but looses optimality
//
// ***********************************************************

#ifndef _SINGLE_HEURISTIC_H
#define _SINGLE_HEURISTIC_H

#include <vector>
#include <list>
#include <string>

using namespace std;

#include <util.queue.h>
#include <util.bitarray.h>

class State;
class Operator;
class FactMap;


class Heuristic {

 protected:
  FactMap& factMap;       // handle to interface
  State* goalState;       // the goal state to determine visibility for PO
  int* FactArray;         // int array of facts, converted from bit vector
  int fSize;              // size of the above
  int* OpArray;           // all grounded operators  
  int opSize;             // number of grounded operators

  int depth;

  // all operator application, enableness and dependency computations
  // based
  BitArray** goals;       // set of facts in given depth  
  BitArray *relevantOps;  
  BitArray *zeroOps;
  BitArray *resultOps;
  BitArray *dependOps;

  Operator** operators;   // set of operators in the space
  int oSize;              // upper bound on the previous
  Operator*** preActions; // precomputed set of operators wrt to preconditions
  int* aSize;             // maintains the respective upper bounds

 public: 
  Operator** relevantQueue; // queue of relevant operators for expansion
  int operRelevant;         // size of relevant queue
  Heuristic(FactMap& fMap,    
        Operator** operate, int osize,           
        Operator*** preactions, int* asize);

  // abstract processes interface of heuristic search engines

  virtual int calculate(State* from, State* to) = 0;
  // return estimate
  virtual int expand(State* state, State**& succs, bool complete);
  // compute successor set
  virtual int getRelaxedPlan(Operator**& relaxedPlan) = 0;
  // return relaxed plan
  virtual double estimate(State* state) = 0;
  // return
  virtual string toString() = 0;
  // output of heuristic estimate
};

#endif
