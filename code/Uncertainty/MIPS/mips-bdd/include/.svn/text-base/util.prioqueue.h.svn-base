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
//  Module:     mips\include\util.prioqueue.h
//  Authors:    S.Edelkamp, M.Helmert
// 
//  Dial priority queue implementation based on buckets of 
//  doubly linked elements. Used in A* if algorithm deals 
//  with integer g-, h-, and f- values only. Operations insert, 
//  delete and decrease key are in O(1), while
//  deleteMin takes time O(C) with 
//  C = max { w(u,v) - h(v) -h(u) |  e = (v,w) }.
//  C bounded by small constant in practice, yielding
//  linear time priority queue impelentation.
//
// ***********************************************************

#ifndef __PRIO_QUEUE_H__
#define __PRIO_QUEUE_H__

#include <stdio.h>                                    // standard I/O library
#include <stdlib.h>
#include <assert.h>

class State;


struct PrioElem;

typedef struct PrioElem{
  State *the_state;       // the contained state as a reference
  struct PrioElem *prev;  // doubly linkage
  struct PrioElem *next;
}PrioElem;

class PrioQueue {

public:
  PrioElem** open; // index of first element of OPEN with given f value.
  int bestf;       // f value of best nodes on OPEN. 
  int maxf;        // maximal f value. 

  PrioQueue(int _maxf);
  ~PrioQueue(); 
  void init();
  bool insert (State* s, int f); 
  // insert a state in the queue with given f value. 
  void close (State* s, int f); 
  // remove a state from the queue with given f value. 
  void decreaseKey(State*s, int oldf, int newf); 
  // Change priority of a state. 
  State* deleteMin();
};


#endif
