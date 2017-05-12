#ifndef HEURISTICS_H
#define HEURISTICS_H

#include "hash.h"
#include "problems.h"

class atomList_t;
class atomListHash_t;
class atomListHashEntry_t;
class actionList_t;
class problem_t;
class state_t;
class hash_t;
class algorithm_t;
class retraseHeuristic_t;
class hash_FH_t;

/*******************************************************************************
 *
 * heuristic (abstract class)
 *
 ******************************************************************************/

/*
  Implements the notion of a heuristic function, a mapping from states to real
  numbers. The value assigned by a heuristic to a state can be viewed as an
  estimate of that state's optimal value.
*/
class heuristic_t
{
protected:
  const problem_t &problem_;

public:
  heuristic_t( const problem_t &problem ) : problem_(problem) { }
  virtual ~heuristic_t() { }

  /*
    Computes a heuristic value for the given state.
  */
  virtual double value( const state_t &state ) const = 0;

  /*
    Computes a heuristic value for the given state. "Hash" is the state's 
    hash value.
  */
  virtual double value ( const state_t &state, unsigned hash ) const = 0;

  /*
    Prints various statistics about the heuristic's operation.
  */
  virtual void statistics( std::ostream &os ) const = 0;
};


/*******************************************************************************
 *
 * LaSH (Largest Solved Horizon) heuristic
 *
 ******************************************************************************/

/*
  LaSH heuristic is applicable to finite-horizon MDPs. It estimates value h(s,t)
  for a state s with t steps to go as (t-t') * maxR + V(s, t'), where maxR is 
  the maximum reward any action can yield in any state, t' is a number of 
  steps-to-go, t' < t, for which some value of s is already available, and 
  V(s,t) is the current estimate of the value of state s at t' steps-to-go.

  The intuision behind this heuristic is that finite-horizon MDPs are typically
  solved in a bottom-up fashion, i.e., by first computing V*(s, 1) for most or all 
  of the states, then V(s, 2), and so on. That is, the computation proceeds from
  smaller values of steps-to-go to larger ones. Therefore, when LaSH tries to 
  compute h(s,t), an estimate of s's value for some smaller number of 
  steps-to-go, t', is likely already available. Moreover, if V(s, t') is 
  admissible, then so is h(s, t). In theory, in the case of lr2tdp algorithm, 
  the LaSH estimates are admissible. However, due to the approximations made by
  our implementation of lr2tdp, the estimates given by this implementation may, 
  in general, occasionally 
  be inadmissible.
*/
class lashHeuristic_t : public heuristic_t
{
  size_t stepsToGo_;
  const hash_FH_t* hash_table_;

public:
  lashHeuristic_t( const problem_t &problem );
  virtual ~lashHeuristic_t();
  virtual double value( const state_t &state ) const;
  virtual double value( const state_t &state, unsigned hash ) const;
  virtual void statistics( std::ostream &os ) const { }
  void set_stepsToGo(size_t stepsToGo) {  stepsToGo_ = stepsToGo; }
  void set_hash(const hash_FH_t* hash_table) {  hash_table_ = hash_table; }
};


#endif // HEURISTICS_H
