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
//  Module:     mips\include\data.action.h
//  Authors:    S.Edelkamp, M.Helmert
// 
// ***********************************************************

#ifndef _DATA_ACTION_H
#define _DATA_ACTION_H

#include <algorithm>
#include <string>
#include <vector>
#include <map>
using namespace std;
#include<data.instantiation.h>

class LispEntity;
class Domain;
class SymbolicFact;
class Formula;

/** symbolic action representation, intermediate data structure
    instantiations kept as a list
*/

class Action {
  enum {NORMAL,WHEN,FORALL} LABEL;  // separate whens-constructs from others
  Action(const Action &copy); // prevent copying
  Domain& domain;             // handle to domain
  string name;                // action name
  int label;                  // additional label
  float duration;             // time of fixed time action 
  int parameterCount;         // number of parameters
  bool derived;
  bool durative;           

  vector <pair <int,SymbolicFact * > > preAdd, preDel, effAdd, effDel;
  // propositional part
  vector<Instantiation> instantiations;
  // list of instantiations
  vector<vector <pair <int,SymbolicFact * > > > 
      cPreAdd, cPreDel, cEffAdd, cEffDel;
  map<string, pair<int,Formula*> > pref;

  void scanForConstantPredicates(map<string,int>& parameters, 
                                 vector<pair <int,LispEntity> >& le,
                                 vector<pair <int,LispEntity> >& preAddList);
  // scan to eliminate constant predicates, extends parameter list
  // does tackle one quantifier, general enough for benchmark problems
  vector<vector<SymbolicFact *> > preByMaxPar;
          // non-unary preconditions with a given maximum argument number
  vector<vector<int> > preconditionCount;
  vector<vector<int> > validArguments;
  vector<int> split(string instance);
public:
  Action(Domain &d);
  Action(Domain &d, LispEntity &le);
  ~Action();

  string getName()   {return name;}
  string toString();
  string getfullString();

  bool getDerived() { return derived; }
  bool getDurative() { return durative; }

  void logInstantiation(const Instantiation& inst) {
    instantiations.push_back(inst);
  }
  float getDuration() { return duration;}

  vector<pair <int,SymbolicFact *> >& getAddPreconditions()  {return preAdd;}
  vector<pair <int,SymbolicFact *> >& getDelPreconditions()  {return preDel;}
  vector<pair <int,SymbolicFact *> >& getAddEffects()        {return effAdd;}
  vector<pair <int,SymbolicFact *> >& getDelEffects()        {return effDel;}
  map<string, pair<int,Formula*> >& getPrefs() {return pref;}

  vector<vector<pair <int,SymbolicFact *> > >& getConditionalsAddPreconditions()  {return cPreAdd;}
  vector<vector<pair <int,SymbolicFact *> > >& getConditionalsDelPreconditions()  {return cPreDel;}
  vector<vector<pair <int,SymbolicFact *> > >& getConditionalsAddEffects()        {return cEffAdd;}
  vector<vector<pair <int,SymbolicFact *> > >& getConditionalsDelEffects()        {return cEffDel;}

  vector<SymbolicFact *> &getPreconditionsByMaxPar(int maxPar) {
    return preByMaxPar[maxPar];
  }

  vector<Instantiation> &getInstantiations() {
    return instantiations;
  }

};

#endif
