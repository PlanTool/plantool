#include "global.h"
#include "hash.h"
#include "planners.h"
#include "states.h"
#include <assert.h>


/*******************************************************************************
 *
 * planner LR2TDP
 *
 ******************************************************************************/


plannerLR2TDP_t::plannerLR2TDP_t(const problem_t &problem, heuristic_t &heur)
  : planner_t(problem,heur)
{
  hash_table_FH_ = new hash_FH_t( problem, gpt::initial_hash_size, heur_, problem.horizon() );
  algorithm_ = new lr2tdp_t( problem, *hash_table_FH_ );
}


plannerLR2TDP_t::~plannerLR2TDP_t()
{
  delete algorithm_;
  delete hash_table_FH_;
}


void
plannerLR2TDP_t::statistics( std::ostream &os, int level ) const
{
  if( level >= 300 )
    hash_table_FH_->print( os, problem_ );
}


/*******************************************************************************
 *
 * planner GLUTTON
 *
 ******************************************************************************/


plannerGLUTTON_t::plannerGLUTTON_t(const problem_t &problem, heuristic_t &heur)
  : planner_t(problem,heur)
{
  hash_table_FH_ = new hash_FH_t( problem, gpt::initial_hash_size, heur_, 
				                            problem.horizon() );
  algorithm_ = new glutton_t( problem, *hash_table_FH_ );
}


plannerGLUTTON_t::~plannerGLUTTON_t()
{
  delete algorithm_;
  delete hash_table_FH_;
}


void
plannerGLUTTON_t::statistics( std::ostream &os, int level ) const
{
  if( level >= 300 )
    hash_table_FH_->print( os, problem_ );
}


/*******************************************************************************
 *
 * planner GOURMAND
 *
 ******************************************************************************/


plannerGOURMAND_t::plannerGOURMAND_t(const problem_t &problem, 
				                              heuristic_t &heur)
  : planner_t(problem,heur)
{
  hash_table_FH_ = new hash_FH_t( problem, gpt::initial_hash_size, heur_, 
				                            problem.horizon() );
  algorithm_ = new gourmand_t( problem, *hash_table_FH_ );
}


plannerGOURMAND_t::~plannerGOURMAND_t()
{
  delete algorithm_;
  delete hash_table_FH_;
}


void
plannerGOURMAND_t::statistics( std::ostream &os, int level ) const
{
  if( level >= 300 )
    hash_table_FH_->print( os, problem_ );
}





