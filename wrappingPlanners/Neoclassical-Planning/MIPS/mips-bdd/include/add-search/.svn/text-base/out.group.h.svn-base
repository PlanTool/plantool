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
//  Module:     mips\include\out.group.h
//  Authors:    S.Edelkamp, M.Helmert
// 
// grounded group of mutual exclusive facts to minimize state
// encoding with one dummy true fact if necessary. 
// See Edelkamp and Helmert ECP-99 paper: partition of fact into 
// groups bases on state invariances. including operators
// that modify states groups can be viewed as graphs projection
// of the entire problem space graph, which suggest to draw
// connections to automata based model checking, where finite
// state automata span the overall space. implementation now also 
// features symmetry detection based on representative objects for 
// fact groups
//
// ***********************************************************

#ifndef _GROUP_
#define _GROUP_

class BitArray;
class FactMap;

#include <vector>
#include <list>

using namespace std;


class FactMap;   // forward class declaration
class Operator;

class Group {
  int number;        // group number for self reference
  int factsize;      // number of facts
  int objsize;       // number of representative objects  
  int ressize;       // number of resources to be checked
  int base;          // temporary variable for symmetry detection
  int param;         // temporary variable counting number of words in string
  int value;         // yet another variable for symmetry detection
  int currState;     // the fact that is true in current state
  int goalState;     // the fact that is true in goal state
  int ** dist;       // improvement for relaxed plans (not in use)
  FactMap& factMap;             // handle to refer to interface
  int none;                     // omitted "true" fact

 public:

  // caution: all following variables are public and not encapsulated
  //          this has not been changed due to fear of efficiency lacks

  int* fact;         // array of facts 
  int symmsize;      // number of symmetric groups
  int* symm;         // list of groups that symmetric by object swap
  int* object;       // list of static object representatives for detect symm
  int* resource;     // list of resources to be checked in case of symmetry
  Group* succ;       // link of list of successor groups to be checked for symm
  BitArray** bitvector; // bitvector representation 
  BitArray* mask;       // another bitvector 
  BitArray* affectedOp; // a bitvector of operators that alter one fact
  

  Group(int nr, int base, vector<int>& facts, bool none, 
    vector<int>& symm, vector<int>& objects, 
    vector<int>& resources, FactMap& fMap);
  // instantiates group with set of fact, symmetries, objects and resources

  Group(const Group& copy);           // copy constructor

  // member variables

  int getFirst() { return fact[0]; }       // get one representative fact 
  int getObject() { return object[0]; }    // get one representative object
  int getNumber() { return number; }
  int setParam(int p) { param = p; }
  int setValue(int v) { value = v; }
  int resourceSize() { return ressize; }
  int symmSize() { return symmsize; }
  int objSize() { return objsize; }
  int size() { return factsize + none; }
  int getFactSize() { return factsize; }

  // methods

  vector<string> split(string instance);
  // split function similar to the one in util.tools  
  string replace(string other, string obj1, string obj2);
  // replace obj1 with obj2 in other   
  bool opmatch(Group* other);
  // is operator affected
  void setGoalState(int position) { goalState = position; }
  // set goal fact for fast match query
  int getGoalState() { return goalState; }
  // get goal fact for fast query
  void setCurrState(int position) { currState = position; }
  // set current fact for fast match query
  int getCurrState() { return currState; }
  // get current fact for fast query
  void initStates() { goalState = currState = -1; }

  bool contains(Operator* oper);            // O(1) operator containment
  int omitted() { return none; }            // has group omitted "true" fact
  void reduceSymmetries();                  // reduce symetries based on marks
  void affects(vector <Operator*>& operators); 
  string toString();                        // output in string
  string getString();
};

#endif
