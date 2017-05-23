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
//  Module:     mips\include\bdd.pattern.h
//  Authors:    S.Edelkamp, M.Helmert
// 
// ***********************************************************

#ifndef _BDD_PATTERN_H
#define _BDD_PATTERN_H

#include <vector>
#include <list>

using namespace std;

#include <util.queue.h>
#include <util.bitarray.h>
#include <single.pattern.h>
#include <bdd.h>

class FactMap;
class Operator;
class FactGroup;
class Group;

class BddPattern : public Pattern {
  bdd trans, init, goal, applicable;
  bdd flop;
  bdd preVariables;
  bdd effVariables;
  bdd actVariables;
  bddPair* preEff;
  bddPair* effPre;

  bdd state, heur, eval;
  int groupsize;
  bool* visited;
  vector<bdd> bddVector;
  vector<bdd> bddPreVector;
  vector<bdd> fddVar;
  vector<bdd> topi;
  vector<bdd> transi;
  vector<bdd> flopi;

  void buildTransition();

 public:
  BddPattern(FactMap& fMap,State* init, State* goal, Operator** operate, 
	     int osize, bool* active, bool* goalactive, int* gr, int* pos);
  virtual double BFS();
  int size();
  bdd getHeuristic() { return heur; }
  vector<bdd> getBddVector() { return bddPreVector; }
  int heuristic(int* FactArray, int fSize);
  string toString();
};
  

#endif
