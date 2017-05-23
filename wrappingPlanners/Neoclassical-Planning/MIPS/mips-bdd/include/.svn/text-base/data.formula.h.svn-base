#ifndef _DATA_FORMULA_H
#define _DATA_FORMULA_H

#include <string>
#include <vector>
#include <map>
#include <bdd.h>
using namespace std;

class Action;
class TypeEngine;
class Domain;
class LispEntity;
class SymbolicFact;

class Formula {
  Formula(const Formula &copy); // prevent copying
  Action& action;    // handle to action 
  Domain& domain;    // handle to doamin

  string Condition;   // string representation of operator *,-,/,
  string Operator;   // string representation of operator *,-,/,
  string Value;      // string representation of value
  SymbolicFact* sFact;  // symbolic fact
  bdd Binary;
  LispEntity* sString;  // symbolic fact
  Formula* Left;     // formula for left tree
  Formula* Right;    // formula of right tree
  
public:
  Formula(Action *a, map<string, int> &funcTable,
      Domain &d, LispEntity &le);
  Formula(map<string, int> &funcTable, Domain &d, LispEntity &le);
  ~Formula();
  int countLeaves();
  int maxint();
  int minint();
  void printBdd();
  bdd getBinary() { return Binary; }
  void makeBdd(int index, vector<bdd*> v, int addIndex, bdd addBdd, int mint); 
  void makeBdd(vector<bdd*> v);
  string sat(int upperBound);
  LispEntity toLisp();
  string toString();
};

#endif
