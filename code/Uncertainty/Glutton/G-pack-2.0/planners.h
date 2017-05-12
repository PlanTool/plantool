#ifndef PLANNERS_H
#define PLANNERS_H

#include "algorithms.h"
#include "actions.h"
#include <iostream>

class problem_t;
class hash_t;
class heuristic_t;


/*******************************************************************************
 *
 * planner (abstract class)
 *
 ******************************************************************************/

/*
  The planner class and its children are just wrappers around planning algorithm
  implementations.
*/

class planner_t
{
protected:
  const problem_t &problem_;
  heuristic_t &heur_;
  hash_t *hash_table_;
  algorithm_t *algorithm_;

public:
  planner_t( const problem_t &problem, heuristic_t &heur )
    : problem_(problem), heur_(heur), hash_table_(0), algorithm_(0) { }
  virtual ~planner_t() { }

  virtual void initRound( void ) { }
  virtual void endRound( void ) { }
  virtual void reset_state( void ) { algorithm_->reset_state(); }
  virtual const action_t* decideAction( const state_t &state )
    {
      return( algorithm_->next( state ) );
    }
  
  void print( std::ostream &os, const action_t &action ) const
    {
      action.print( os );
    }

  virtual void statistics( std::ostream &os, int level ) const = 0;
};


/*******************************************************************************
 *
 * planner RANDOM
 *
 ******************************************************************************/

class plannerRANDOM_t : public planner_t
{
public:
  plannerRANDOM_t( const problem_t &problem, heuristic_t &heur )
    : planner_t(problem,heur)
    {
      algorithm_ = new random_t( problem );
    }

  virtual ~plannerRANDOM_t()
    {
      delete algorithm_;
    }

  virtual void statistics( std::ostream &os, int level ) const { }
};


/*******************************************************************************
 *
 * planner LR2TDP
 *
 ******************************************************************************/

class plannerLR2TDP_t : public planner_t
{
  hash_FH_t *hash_table_FH_;

public:
  plannerLR2TDP_t( const problem_t &problem, heuristic_t &heur );
  virtual ~plannerLR2TDP_t();
  
  virtual void statistics( std::ostream &os, int level ) const;
};


/*******************************************************************************
 *
 * planner GLUTTON
 *
 ******************************************************************************/

class plannerGLUTTON_t : public planner_t
{
  hash_FH_t *hash_table_FH_;

public:
  plannerGLUTTON_t( const problem_t &problem, heuristic_t &heur );
  virtual ~plannerGLUTTON_t();

  virtual void statistics( std::ostream &os, int level ) const;
};


/*******************************************************************************
 *
 * planner GOURMAND
 *
 ******************************************************************************/

class plannerGOURMAND_t : public planner_t
{
  hash_FH_t *hash_table_FH_;

public:
  plannerGOURMAND_t( const problem_t &problem, heuristic_t &heur );
  virtual ~plannerGOURMAND_t();
  
  virtual void statistics( std::ostream &os, int level ) const;
};

#endif // PLANNERS_H
