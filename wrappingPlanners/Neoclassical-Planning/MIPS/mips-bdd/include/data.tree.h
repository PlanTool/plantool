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
//  Module:     mips\include\data.tree.h
//  Authors:    S.Edelkamp, M.Helmert
// 
// Recursive mixed boolean and arithmetic tree with either variable
// indices or constants at leaves. Bounding and restricting numerical
//  variables is not in current use
// ***********************************************************

#ifndef _DATA_TREE_H
#define _DATA_TREE_H

#include <string>
#include <vector>
#include <map>
using namespace std;

class Domain;
class FactMap;
class Operator;


class Tree {
  Operator* oper;   
  int variable;     // index of resource variable for leaf node
  Domain& domain;   // handle to domain specification
  double value;     // value of leaf node if constant
  bool bvalue;      // set if current node is constant
  bool boolset;     // set if current node is boolean and, or, not
  bool valset;      // set if current node is constant
  Number number;
  int op;           // index of operator (+,-,/,*,<=,...)
  Tree* Left;       // left branch of arithmetic tree    
  Tree* Right;      // right branch of arithmetic tree 
  int depth;
   
public:
  void init(int nor); // initializing node nor = number of resource

  int* container;    // faster access function for leaf variables
  int counter;       // number of leaf variables

  Tree(double r, Domain& dom);   // constructor for floating-point constant
  Tree(int i, Domain& dom);      // constructor for integer constant
  Tree(int oper, Tree* left, Tree* right, Domain& dom);
  // constructor for internal node
  Tree(const Tree& tree);
  ~Tree();
  Tree* insert(Tree* subtree, int i);
  // insert subtree in current tree

  bool smaller(Tree* other);
  // variable bounding
  bool equals(Tree* t2);
  // test for equality

  Tree* replace(int effvar, int effop, Tree* effbody);

  void setOperator(Operator* o) { oper = o; }
  Operator* getOperator() { return oper; }

  int getDepth() { return depth; }
  int initDepth() { depth = -1; } 
  int setDepth(int i) { depth = i; } 
  int count(); 
  // return number of variables in tree
  bool isTrivial();
  // tree is leaf
  bool isValue();
  // tree is constant
  bool isBoolean();
  // current node is Boolean connector
  bool isEqual();


  // relaxed exploration to bound variable domains
  bool isRelaxedGreater();
  bool isRelaxedSmaller();
  bool isRelaxedSharp();  
  bool relaxedTest(double* list);

  double getConstant();  // return one leaf constant value (assuming one)
  int getVariable();     // return index of constant variable (assuming one)
  double getValue() { return value; } // get constant value
  double eval();             // evaluate tree value
  bool test(double* list);   // instantiate leaves and test if outcome is true
  double eval(double* list); // instantiate leaves and return value

  // restrict to current interval to bound and instantiate resource variables
  void restrict(double*& clist1, double*& clist2,
        bool right,double value);
  void restrict(double* list1, double* list2,
        double*& clist1, double*& clist2);

  // further routines to bound and instantiate resource variables
  bool test(double* list1, double* list2);
  double eval_min(double* list1, double* list2);
  double eval_max(double* list1, double* list2);

  void simplify();     // simplify tree by recursive executing ops on constants
  bool contains(int r);  // does tree contain resource with index r
  void convert(map<int,int>& id); 
  // convert variable indices due to compression map
  bool hasConstants() const;
  // return true if there is a leave that is constant
  string toString(FactMap& fMap); // extended output
  string getString();             // reduced output
};

#endif
