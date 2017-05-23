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
//  Module:     mips\include\data.predicate.h
//  Authors:    S.Edelkamp, M.Helmert
// 
// ***********************************************************

#ifndef _DATA_PREDICATE_H
#define _DATA_PREDICATE_H

#include <string>
#include <vector>
#include <map>

using namespace std;

#include <util.bitarray.h>

class LispEntity;
class SymbolicFact;

/** ungrounded predicate specification, similar to predicate
 */

class Predicate {
  typedef map<string, string> TypeMap;
  Predicate(const Predicate &copy); // prevent copying
  string name;
  int baseId;
  int parameterCount;
  bool settable, clearable;
 
public:
  vector<BitArray> projections;
  vector<SymbolicFact *> preconditions;

  Predicate(string nm, int parNo, int factLowerBound);
  Predicate(LispEntity &le, int factLowerBound);

  int getFactLowerBound() {return baseId;}
  int getFactUpperBound(int objectCount);
  int getParameterCount() {return parameterCount;}
  string getName()        {return name;}
  string toString();

  void markSettable()  {settable = true;}
  void markClearable() {clearable = true;}
  bool isConstant()    {return !settable && !clearable;}

  unsigned long getProjection(int arg, int obj) {
    return projections[arg].get(obj);
  }
  void setProjection(int arg, int obj) {
    projections[arg].set(obj);
  }
  vector<SymbolicFact *> &getPreconditions() {return preconditions;}
};

#endif
