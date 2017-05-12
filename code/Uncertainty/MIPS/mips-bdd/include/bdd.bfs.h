
#ifndef _BDD_BFS_H
#define _BDD_BFS_H

#include <vector>
#include <list>

using namespace std;

#include <single.search.h>
#include <bdd.h>
#include <bdd.arithmetics.h>

class FactMap;

class BddBfs: public Search {

  Arithmetics arithmetics;          
  // to perform arithmetic opations with bdds
  bdd trans, init, goal, applicable, derived; 
  // transition function, initial and goal state
  // applicable operators in nondeterministic setting
  bdd preVariables; // pre-image variables
  bdd derivedVariables; // derived variables
  bdd negatedVariables; // derived variables
  bdd normalVariables; 
  bdd effVariables; // image variables
  bdd actVariables; // action variables
  bddPair* preEff;  // replacement pre-image into image variables
  bddPair* effPre;  // replacement image into pre-image variables
  bddPair* preOrig;  // replacement pre-image into orig-image variables
  bddPair* effOrig;  // replacement image into orig-image variables
  bddPair* origPre;  // replacement pre-image from orig-image variables
  bddPair* origEff;  // replacement image from orig-image variables


  bdd objective; // objective function 
  int index; // index of variables in objective function
  bdd upper; // upper bound in index variables

  bdd inc, add, max, greater, formula;
  vector<bdd> topi;     
  vector<bdd> flopi;
  vector<bdd> deri;     
  // for disjunctive paritioning, 
  // splitted transition function resuls are combined in 
  // balanced binary tree
  vector<bdd> transi;   // splitted transition relation
  vector<bdd> derivedi;   // splitted derived relation

  bdd preHeuristic, effHeuristic; 
  // the monolithical heuristic estimate, in case of partition PDBs
  // maintained in bdd.pattern
  
  vector <vector <bdd> >& hold;
  vector <vector <pair <string,bdd> > >& prefHold;
  vector <vector <bdd> >& within;
  vector <vector <pair <string,bdd> > >& prefWithin;

  bdd  
    preHeurVariables, effHeurVariables, preWeightVariables,
    preMeritVariables, effMeritVariables, allVariables;
  bddPair
    *weightPair, *meritPair, *heurPair;

  int bound;
  vector<bdd> rel;
  bdd isin;

  int    // indices
    preHeurIndex, effHeurIndex, preWeightIndex,
    preMeritIndex, effMeritIndex;
 
  virtual string getTime(State* at) { return string(""); } 

  bdd computePatternHull(bdd stateset, bool forward);
  bool searchStep(bdd from, bdd& back, bdd &reach, bdd& varset,
          bddPair *rename, vector<bdd> &bddVec,
          bdd &meet, Timer &t, int &time);
  bool heuristicStep(bdd front, bdd back, bdd &reach, bdd varset,
             bddPair *rename, vector<bdd> &bddVec,
             bdd &meet, Timer &t, int &time);
  void buildTransition();
  void solutionPrint(vector<bdd> &forward, vector<bdd> &backward,
             bdd meet, int quality, Timer &globalTimer);
  bdd solutionStep(bdd &current, bdd next, bool forward);
  bdd computePreImage(bdd& states);
  bdd pruneOutgoing(bdd stateActionTable);
  bdd pruneUnconnected(bdd stateActionTable);
  int cyclic(Timer& globalTimer, State*& finalState);
  int deterministic(Timer& globalTimer, State*& finalState);
  int fgSearch(Timer& globalTimer, State*& finalState);
  int patternconstruct(Timer& globalTimer);
  int Bdda(Timer& globalTimer, State*& finalState);
  int BranchandBound(Timer& globalTimer, State*& finalState);
  int nondeterministic(Timer& globalTimer, State*& finalState);
  void outputStates(list<bdd>& states, int quality, Timer & globalTimer);
  int fixpoint();
  bdd backwardStep(bdd current, bool forward);  

 public:
  BddBfs(FactMap& fMap);
  int search(Timer & t, State*& finalState);
};

#endif
