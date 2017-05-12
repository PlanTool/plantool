#include "problems.h"
#include "hash.h"
#include "heuristics.h"



/*******************************************************************************
 *
 * LaSH heuristic
 *
 ******************************************************************************/

lashHeuristic_t::lashHeuristic_t( const problem_t &problem )
  : heuristic_t(problem), stepsToGo_(0), hash_table_(NULL)
{
  if( gpt::verbosity >= 500 )
    std::cout << "<zero>: new" << std::endl;
}

lashHeuristic_t::~lashHeuristic_t()
{
  if( gpt::verbosity >= 500 )
    std::cout << "<zero>: deleted" << std::endl;
}


double 
lashHeuristic_t::value( const state_t &state ) const
{ 
  return value(state, hash_table_->hash_value(state));
}

double 
lashHeuristic_t::value( const state_t &state, unsigned hash ) const
{
  // Go through all values of steps-to-go, from the current one to 1, and check
  // if a value estimate for the given state s is available for any of them. 
  for (size_t s = stepsToGo_ - 1; s >= 1; s--)
    {
      hashEntry_t<double> *node = hash_table_->find(state, hash, s);
      
      if (node != NULL)
	{
	  return (node->value() + problem_.max_reward() * (stepsToGo_ - s));
	}
    }
  
  // If not, we need to estimate the value of s with just one step to go.
  return( problem_.max_reward() * (stepsToGo_ -1) 
	  + problem_.eval_all_actions(state) ); 
}


