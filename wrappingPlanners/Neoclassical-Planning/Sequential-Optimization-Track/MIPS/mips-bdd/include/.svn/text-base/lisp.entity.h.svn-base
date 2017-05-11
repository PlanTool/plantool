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
//  Module:     mips\include\lisp.entity.h
//  Authors:    S.Edelkamp, M.Helmert
// 
//  Lisp entity is the parsed structure of the pddl input.
//  A leaf entity is a string, compound entities are
//  given by vectors 
// ***********************************************************

#ifndef _LISP_ENTITY_H
#define _LISP_ENTITY_H

#include <vector>
#include <string>
#include <iostream>

using namespace std;


class LispEntity {
  bool atom;
  string value;             // this should be a union, but that is
  vector<LispEntity> list;  // not allowed for copy-constructor types
public:
  explicit LispEntity(string val) : // constructs leaf entity 
    atom(true), value(val) {}
  explicit LispEntity(const vector<LispEntity> &l) : // constructs compound
    atom(false), list(l) {}
  ~LispEntity() {}                 // destructor
  float eval();
  bool isOperator() {              // is entity arithmetic atom
    return isString() && 
      (value == "*" || value == "-" || value == "+" || value == "/");
  }
  bool isString() {return atom;}   // is entity a leaf
  bool isList() {return !atom;}    // is entity a compound
  bool isBoolean();                // contains entity a boolean statement
  string& getString();             // get entity contents as a string
  string coreString();
  LispEntity getFormula();         // get entity contents as a formula 
  vector<LispEntity> &getList();   // split list in vector of entities
  vector<LispEntity> getAndList(); // split list, omitting leading and
  string toString();               // output entity
};

#endif
