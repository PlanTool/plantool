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
// groups bases on state invariances. 
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
  FactMap& factMap;             // handle to refer to interface
  int none;                     // omitted "true" fact
  bdd groupPreBdd, groupEffBdd; // bdd representations of entire group

 public:

  // caution: all following variables are public and not encapsulated
  //          this has not been changed due to fear of efficiency lacks

  int* fact;         // array of facts 

  Group(int nr, vector<int>& facts, bool none, FactMap& fMap);
  // instantiates group with set of fact, symmetries, objects and resources

  Group(const Group& copy);           // copy constructor

  // member variables

  bdd getPreBdd() { return groupPreBdd; }  // pre image bdd representation
  bdd getEffBdd() { return groupEffBdd; }  // image bdd representation
  void setBdd();

  int getFirst() { return fact[0]; }       // get one representative fact 
  int getNumber() { return number; }
  int getFactSize() { return factsize; }
  int size() { return factsize + none; }
  int omitted() { return none; }            // has group omitted "true" fact
  string toString();                        // output in string
};

#endif
