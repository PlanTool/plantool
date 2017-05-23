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
//  Module:     mips\include\bdd.arithmetics.h
//  Authors:    S.Edelkamp, M.Helmert
// 
// Since buddy-package allows no arithmetic operation, we   
// impelemented them. Mainly needed to code BDD-A* algorithm
//
// ***********************************************************

#ifndef _ARITHMETICS_H
#define _ARITHMETICS_H

#include <vector>
using namespace std;

#include "bdd.h"  // buddy package

class FactMap;


class Arithmetics {
  int MAX_ADD;      // the maximal range for arithmetics [0..MAX_ADD]
  int               // indices to address bdd groups
    maxIndex, greaterIndex, formulaIndex, incIndex, addIndex, mvaddIndex;
  bdd               // bdd for arithmetic operations, computed only once
    bddMax, bddGreater, bddFormula, bddInc, bddAdd, mvbddAdd, bddHeuristic;
  int               //
    preHeurIndex, effHeurIndex,preWeightIndex, 
    effWeightIndex,preMeritIndex, effMeritIndex;
  bdd               // set of variables for quantification
    preHeurVar, effHeurVar, preWeightVar, 
    effWeightVar, preMeritVar, effMeritVar, allVars;
  
  bddPair *renamePair, *renamePair2; 
  // change variable sets to the one needed
 
  vector<bdd> fluents; // array of bdds for atoms, for HSP-heuristic 
  vector<int> vdepth;  // array for depth for HSP-heuristic
  vector<int> facts;   // array of indices for HSP-heuristic

 public:
  Arithmetics(); 
  void getIndices(int* pre, int* eff, vector<int>& indices, bool inverse);

  int getPreHeurIndex() { return preHeurIndex; }
  int getEffHeurIndex() { return effHeurIndex; }
  int getPreWeightIndex() { return preWeightIndex; }
  int getEffWeightIndex() { return effWeightIndex; }
  int getPreMeritIndex() { return preMeritIndex; }
  int getEffMeritIndex() { return effMeritIndex; }

  bdd getPreHeurVar() { return preHeurVar; }
  bdd getEffHeurVar() { return effHeurVar; }
  bdd getPreWeightVar() { return preWeightVar; }
  bdd getEffWeightVar() { return effWeightVar; }
  bdd getPreMeritVar() { return preMeritVar; }
  bdd getEffMeritVar() { return effMeritVar; }
  bdd getAllVars() { return allVars; }

  bdd getPreHeuristic();
  bdd getEffHeuristic();
  
  bdd buildFormula();    // formula function for BDD-A* algorithm
  int Max(bdd& max);     // maximum of values for admissible HSP
  int Greater(bdd& greater); // comparison relation, based on add
  int Greater(bdd& greater,int maxvalue);
  int Increment(bdd& inc);   
  int Increment(bdd& inc,int maxvalue);   
  // increment inc(a,b) corr. to a+1 = b, enumerated
  int Addition(bdd& add);    
  int Addition(bdd& add,int maxvalue);    
  // addition add(a,b,c) corr. to a+b = c, recursivly based on inc
  int Formula(bdd& formula);  
  // formula(f',f,h,h',w) corr to f' = f + h' - h + w, build on add
  
  
};

#endif
