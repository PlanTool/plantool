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
//  Module:     mips\include\util.tools.h
//  Authors:    S.Edelkamp, M.Helmert
// 
// Set of tools as ordinary c-funtions for entire project
// and contains classes for a time, a display bar and exception.
//
// ***********************************************************

#ifndef _TOOLS_H
#define _TOOLS_H

#include <string>
#include <vector>
#include <iostream>
#include <ctime>
#include <cstdio>
#include <algorithm>
#include <iomanip>

using namespace std;


extern void toDecimalString(int val, char *dest);
// conversion int into charlist
extern string toString(int val);
// conversion int into string
extern string substempty(string s);
// conversion blank into slash
extern string toString(unsigned int val);
// conversion unsigned int into string
extern string toString(double val);
// conversion double into string
extern int pow(int x, int y);
// integral power computation x^y
extern int log2(int n); 
// computes log2 by bit-shifts, rounds up
extern vector<vector<int> > getPerms(vector<int> vec);
// computes all permutation of a vector
extern void error(string str);    
// throws StringException
extern int absolut(int a);
// computes absolut value of integer 
extern double absolut(double a);
// computes absolut value of double
extern vector<int> fromTo(int from, int to, int step = 1);
// generate vector <from, from + step, from + 2 * step, ..., to - 1>
extern vector<int> firstTuple(int size);
// generate tuples (0,...,0), (0,...,1), ..., (0,...,max-1), (0,...,1,0)
extern void nextTuple(vector<int> &vec, int maxCount);
// next tuple computation
extern bool lastTuple(const vector<int> &vec, int maxCount);
// last tuple computation
extern vector<string> splitting(string instance);
// splits string into parts

/** class timer is used to measure performance in seconds and
    milliseconds. timer can be stopped and restarted returning
    the passed time interval length. total time returns time span
    from first start.
*/ 

class Timer {
  time_t startTime;
  time_t lastTime;
  long startClock;
  long lastClock;
public:
  struct TimeSpan {
    TimeSpan(long s, long c) : seconds(s), clocks(c) {}
    string toString();
    long seconds;
    long clocks;
  };
  Timer();
  TimeSpan stop();    // stop and restart timer; return last time span
  TimeSpan total();   // return time span from first start to now
};

ostream &operator<<(ostream &os, Timer::TimeSpan span);

/** progress bar display dots to view progress in exploration
 */

class ProgressBar {
  ostream &stream;
  int total;
  int dotsPrinted;
  int totalDots;

  int threshold;
  void calcThreshold();
public:
  ProgressBar(ostream &s, int tot, int dots = 20);
  void display(int count);
  void done();
};

/** simple string container, entire implementation */

class StringException {
  string text;
public:
  StringException(string str) : text(str) {}
  string toString() {return text;}
};

extern ostream &operator<<(ostream &os, StringException e);



#endif
