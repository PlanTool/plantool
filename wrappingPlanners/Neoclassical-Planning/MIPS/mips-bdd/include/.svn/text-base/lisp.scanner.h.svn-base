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
//  Module:     mips\include\lisp.scanner.h
//  Authors:    S.Edelkamp, M.Helmert
// 
// Lisp scanner reads file until next token and is invoked by 
// the parser. Reading methods omits existing comments */
//
// ***********************************************************

#ifndef _LISP_SCANNER_H
#define _LISP_SCANNER_H

#include <vector>
#include <string>
#include <iostream>

using namespace std;

class LispScanner {
  istream *stream;   // file to be analyzed in form of a stream
  string fileName;   // file name of file to be analyzed
  int lineNo;        // current line number

  int currToken, nextToken;  // maintain pair of token for 1 step lookahead 
  string currId, nextId;     // identification for pair of token

  void readToken();          // reads one tokan, supresses comments
public:
  enum {LEFT, RIGHT, ID, END};     // brackets, id and terminal
  LispScanner(string fileName);    // constructor called by file name
  ~LispScanner();                  // destructor   
  int peekToken();                 // lookahead of token
  int getToken();                  // retrieve next token
  string getValue();               // retrieve value of token in form of string
  void error(string str);          // error handling
};

#endif
