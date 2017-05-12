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
//  Module:     mips\include\util.queue.h
//  Authors:    S.Edelkamp, M.Helmert
// 
//  General queue implementation in a array ring structure of bounded size.
//  All access operations in O(1). The entries of the queue are of
//  class T, the template parameter. Entire implementation in this file.
//  The source refers to Heilmans data structure and algorithm book.
//
// ***********************************************************

#ifndef QUEUE_H
#define QUEUE_H

// #include <iostream>
#include <stdlib.h>

typedef enum {enque, deque} Q_opType;


template<class T>
class Queue {
private:
  int head, tail, size;
  Q_opType lastOp;
  T* element; 
 
public: 
  Queue(int sz): size(sz), head(0), tail(size-1), lastOp(deque) { 
    element = new T[sz]; 
    //for(int i=0;i<sz;i++) element[i] = 0; 
  }
  ~Queue() { delete [] element; }
  void makeEmpty() { head = 0; tail = size-1; lastOp = deque; }
  bool empty() { return (head == (tail+1) % size); }
  int enqueue(const T& elem) {
    if ((head == (tail+1) % size) && (lastOp == enque)) { 
      // cout << "Queue full \n"; 
      return -1; } 
    lastOp = enque;
    tail = (tail + 1) % size;
    element[tail] = elem; 
    return 1;
  }

  T get_head() { return element[head];  }
  T get_tail() { return element[tail];  }
  T dequeue() {
    if ((head == (tail+1) % size) && (lastOp == enque)) 
    { // cout << "Queue empty \n"; 
	exit(1); } 
    T temp = element[head];
    lastOp = deque;
    head = (head + 1) % size;
    return temp;  }

  void print() const { 
    int x=head;
    // cout << "\nh:" << head << " t:" << tail << " ";
    // while (x % size != (tail+1) % size) { 
    //  cout << element[(x % size)] << " "; x++; }
    // cout << "\n"; 
}
  
};


#endif
