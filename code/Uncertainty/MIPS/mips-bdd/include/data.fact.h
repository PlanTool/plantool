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
//  Module:     mips\include\data.fact.h
//  Authors:    S.Edelkamp, M.Helmert
// 
// Grounded representation of one instantiated predicate or
//  function, converting its index to its descriptor, mainly 
//  used for output proposes
//
// ***********************************************************

#ifndef _DATA_FACT_H
#define _DATA_FACT_H

#include <string>
#include <vector>
#include <map>
using namespace std;

class LispEntity;
class Domain;
class Predicate;

class Fact {
  int id;                      // the unique identificator 
  bool isPredicate(Domain &d); // 1 for predicates, 0 for functions
  int ispred;                  // see above

public:
  Fact(Domain &d, LispEntity &le);     // constructor during parsing time
  Predicate &getPredicate(Domain &d);  // very slow retrieval of predicate
  explicit Fact(int factId) : id(factId) {} // sets id
  int toInt()                  {return id;} // returns id
  string toString(Domain &d);     // outputs representation of fact
};

#endif
