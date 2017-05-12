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
//  Module:     mips\include\lisp.parser.h
//  Authors:    S.Edelkamp, M.Helmert
// 
//  Lisp-parser parses either the domain or the problem pddl
//  file. For both cases it frequently calls the lisp scanner. 
//  to parse the input into lisp entity
//
// ***********************************************************

#ifndef _LISP_PARSER_H
#define _LISP_PARSER_H

#include <vector>
#include <string>
#include <iostream>

class LispEntity;
class LispScanner;

using namespace std;


class LispParser {
  LispEntity parseEntity(LispScanner &s);
public:
  LispParser() {}                       // constructor
  ~LispParser() {}                      // destructor
  LispEntity parseFile(string fileNm);  // parses a given pddl file
};

#endif
