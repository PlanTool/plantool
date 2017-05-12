#include "hash.h"
#include "actions.h"
#include "problems.h"
#include "queue.h"
#include "states.h"
#include "algorithms.h"

#include <limits>
#include <values.h>
#include <iostream>
#include <math.h>
#include <deque>
#include <set>










/*******************************************************************************
 *
 * lr2tdp
 *
 ******************************************************************************/

lr2tdp_t::lr2tdp_t( const problem_t &problem, hash_FH_t &hash_table )
  : algorithm_t(problem), hash_table_(&hash_table), 
    steps_to_go_(problem.horizon())
{
  // Initialize the display_ variable
  display_ = new std::pair<state_t*,double>[DISP_INT_SIZE];

  for( size_t i = 0; i < DISP_INT_SIZE; ++i )
    display_[i].first = new state_t;

  // Initialize the initial state
  std::pair<state_t*,double> *initial 
                               = new std::pair<state_t*, double>[DISP_INT_SIZE];

  for( size_t i = 0; i < DISP_INT_SIZE; ++i )
    initial[i].first = new state_t;
  problem_->initial_states( initial );

  init_state_ = *initial[0].first;

  for( size_t i = 1; i < DISP_INT_SIZE; ++i )
    delete initial[i].first;

  delete [] initial;

  // Initialize other member variables
  rounds_remaining_ = gpt::num_rounds;
  times(&start_time_);
}


lr2tdp_t::~lr2tdp_t()
{
  for( size_t i = 0; i < DISP_INT_SIZE; ++i )
    {
      delete display_[i].first;
    }
 
  delete [] display_;
}


void 
lr2tdp_t::solve( hashEntry_t<double> *node, size_t stepsRemaining ) 
{
  // Keep running trials until the given state is solved or until we run out of
  // time.
  while( !(node->bits() & SOLVED) )
    {
      tms timenow;
      times(&timenow);

      if (elapsed_time(start_time_, timenow) > timeout_)      
	{
	  std::cout<<"Out of time! Terminating lrtdp early."<<std::endl;
	  break;
	}

      if (gpt::out_of_memory)
	{
	  std::cout<<"Out of memory! Terminating lrtdp early."<<std::endl;
	  break;
	}
	
      trial( node, stepsRemaining );
    }
}


void 
lr2tdp_t::solve_reverse_iter(const state_t &state, size_t stepsRemaining ) 
{
  // Solve the given state for the number of steps-to-go equal to 1, 2, etc.
  // until you either run out of time or solve it for the number of steps-to-go
  // equal to stepsRemaining
  for (size_t i = 1; i <= stepsRemaining; i++)
    {
      tms timenow;
      times(&timenow);

      // If we are out of time or space, stop now.
      if (elapsed_time(start_time_, timenow) > timeout_)      
	{
	  std::cout<<"Out of time! Terminating lr2tdp early."<<std::endl;
	  break;
	}

      if (gpt::out_of_memory)
	{
	  std::cout<<"Out of memory! Terminating lr2tdp early."<<std::endl;
	  break;
	}

      if (!hash_table_->initialized(i))
	{
	  hash_table_->initialize(i);
	}	    

      hashEntry_t<double> *node = hash_table_->get(state, i);

      if (!(node->bits() & SOLVED)) solve( node, i );

      if (node->bits() & SOLVED) 
	{
	  std::cout<<"SOLVED FOR "<<i<<" STEPS REMAINING"<<std::endl;
	}
      else
	{
	  std::cout<<"SOLVED FOR "<<i - 1<<" STEPS REMAINING"<<std::endl;
	}
    }	
}


void
lr2tdp_t::trial( hashEntry_t<double> *node, size_t stepsRemaining )
{
  // Will hold Q-values of different actions.
  double value;

  // Will hold states visited by the trial, with the most recently visited one
  // on top.
  std::deque<std::pair<hashEntry_t<double>*, 
		                          std::pair<unsigned, size_t> > > Stack;

  if( gpt::verbosity >= 450 )
    std::cout << "<lr2tdp>: trial begins" << std::endl;
  
  // Initialize the current state of the trial to be the given state.
  state_t *current = new state_t( *node->state() );
  unsigned hash = hash_table_->hash_value(*current);


  // Number of steps in the trial so far.
  size_t numIter = 0;

  if( gpt::verbosity >= 5 )
    std::cout<<"Solving for steps-to-go equal to "<<stepsRemaining<<std::endl;
 
  // Run the trial for at most the target number of steps.
  for ( ;stepsRemaining > 0; stepsRemaining--)
    {
      // If we are out of time space, stop now.
      if (gpt::out_of_memory)
	{
	  std::cout<<"Out of memory! Terminating trial early."<<std::endl;
	  break;
	}

      if( gpt::verbosity >= 450 )
	{
	  std::cout << "<lr2tdp>:   node ";
	  node->state()->print( std::cout );
	  std::cout << " = " << node->value() << std::endl;
	}

      // If the trial runs into an already solved state, stop it -- there
      // is no point in exploring past that state.
      if( (node->bits() & SOLVED) ) 
	{
	  break;
	}
      
      // Otherwise, memorize the state just visited, its hash value, and the 
      // number of steps-to-go at which we visited it.
      Stack.push_front( std::pair<hashEntry_t<double>*, std::pair<unsigned, 
	  size_t> >(node, std::pair<unsigned, size_t>(hash, stepsRemaining) ) );
      
      // At this point, we need to figure out the action that currently looks
      // best in the state we just visited.
      int action = 0;
      hashEntry_t<succDataContainer*> *succ_entry = NULL;
      
      // If there is more than one step-to-go...
      if (stepsRemaining > 1)
	{
	  // Retrieve the info about the current state's successors under all
	  // actions, and use this info to find the apparent best action and
	  // its value.
	  succ_entry = hash_table_->get_from_succ_hash(*current, hash);
	  action = hash_table_->bestAction( *current, hash, stepsRemaining, 
					          *problem_, value, succ_entry);
	  assert(action != -1);

	  // Tell the successor hash table that we are using the successors 
	  // information for the current state, so that this hash table doesn't
	  // delete it.
	  succ_entry->set_bits(DELETE_PROTECTED);
	}
      else
	{
	  // If there is just one step-to-go, we can find the value of the best
	  // action in a much cheaper way, without generating the current
	  // state's successors, instead looking only at all actions' immediate
	  // rewards.
	  value = problem_->eval_all_actions( *current );
	}
      
      // Update the value of the current state.
      node->update( value );
      numIter++;

      // If there is more that one step-to-go, transition to another state
      // via the apparent best action in the current state, as determined above.
      if (stepsRemaining > 1)
	{
	  // Sample a successor from the already pre-sampled set of successors
	  *current = succ_entry->value()->sample(action);

	  // At this point, we don't need the info about the current state's 
	  // successors anymore. Delete it or release hold on it, as appropriate
	  if (succ_entry != NULL)
	    {
	      if (!succ_entry->value()->owned_by_hash())
		{
		  delete succ_entry->value();
		  delete succ_entry;
		  succ_entry = NULL;
		}
	      else
		{
		  succ_entry->reset_bits(DELETE_PROTECTED);
		}    
	    }

	  problem_->complete_state( *current );

	  // Finally, transition to the sampled successor.
	  hash = hash_table_->hash_value(*current);
	  node = hash_table_->get(*current, hash, stepsRemaining - 1 );
	  assert( current->make_check() );
	}

      if( gpt::verbosity >= 450 )
	{
	  std::cout << "<lr2tdp>:   action = ";
	  problem_->actionsT()[action]->print( std::cout );
	  std::cout << std::endl;
	}
    }

  delete current;

  // The trial itself is finished. Now we need to check whether the state value
  // updates made during the trial have caused any of the states to get solved.
  // If so, we will mark these states as solved and will avoid updating their 
  // values in the future.
  if( gpt::verbosity >= 450 )
    std::cout << "<lr2tdp>: trial end." << std::endl;
  
  // Analyze each state the trial visited in reverse order (most recently 
  // visited first).
  while( !Stack.empty() )
    {
      tms timenow;
      times(&timenow);

      // If we are out of time or space, stop now.
      if (elapsed_time(start_time_, timenow) > timeout_)      
	{
	  std::cout<<"Out of time! Terminating post-trial checks early."
		   <<std::endl;
	  break;
	}

      if (gpt::out_of_memory)
	{
	  std::cout<<"Out of memory! Terminating post-trial checks early."
		   <<std::endl;
	  break;
	}

      std::pair<hashEntry_t<double>*, std::pair<unsigned, size_t> > temp 
	                                                        = Stack.front();
      Stack.pop_front();

      // If the state hasn't been marked as solved previously, check it 
      if( !(temp.first->bits() & SOLVED) )
	{
	  // If one of the visited states is not solved. we don't need to
	  // check the rest of the stack. A state can only be solved if all
	  // of its successors under the action we used in that state are 
	  // solved. Since any state in the stack is a successor of all
	  // states below it in the stack, if this state isn't solved then
	  // neither are those below it.
	  if( !checkSolved( temp.first, temp.second.first, 
			    temp.second.second))
	    {
	      break;
	    }
	}
    }
  
  for( ; !Stack.empty(); Stack.pop_front() );

  if( gpt::verbosity >= 5 )
    std::cout<<"Trial ended after "<<numIter<<" iterations"<<std::endl;
}



size_t
hashEntry_pair_hash::operator()(std::pair<hashEntry_t<double>*, 
				         std::pair<unsigned, size_t> > he) const
{
  return he.second.first;
}


/*
  To check whether a state is solved, we need to verify that an value update
  changes its value by no more than epsilon and, recursively, that all of this
  state's successors under the apparent best action are solved as well. This
  method does exactly that, but implements the described strategy in an 
  iterative manner.
*/
bool
lr2tdp_t::checkSolved( hashEntry_t<double> *node, unsigned hash, 
		                                          size_t stepsRemaining)
{
  double value;

  // A queue of triplets of the form (state, state's hash value, steps-to-go)
  // We will use this variable to store the triplets corresponding to states 
  // that still need to be checked. (We could store only the states themselves 
  // along with the number of steps-to-go,i.e., without their hash values, but 
  //this would make the code significantly slover.
  std::deque<std::pair<hashEntry_t<double>*,std::pair<unsigned,size_t> > > open;

  // A set containing the set triplets as the queue above. We need it to be
  // able to efficiently check whether a stat is already present in the queue.
  hashing::hash_set<std::pair<hashEntry_t<double>*,std::pair<unsigned,size_t> >,
            hashEntry_pair_hash,  std::equal_to< std::pair<hashEntry_t<double>*,
					  std::pair<unsigned, size_t> > > > aux;
  open.clear();
  aux.clear();

  // Initialize both the queue and the set by putting the state to be checked
  // in them.
  open.push_back(std::pair<hashEntry_t<double>*, 
		  std::pair<unsigned, size_t> >(node, 
			   std::pair<unsigned, size_t>(hash,  stepsRemaining)));
  aux.insert(std::pair<hashEntry_t<double>*, 
	          std::pair<unsigned, size_t> >(node, 
			   std::pair<unsigned, size_t>(hash,  stepsRemaining)));
  bool rv = true;
  std::pair<hashEntry_t<double>*, std::pair<unsigned, size_t> >temp;

  // Repeat the following until there are no more states to check
  while( !open.empty() )
    {
      tms timenow;
      times(&timenow);

      // If we are out of time, stop now.
      if (elapsed_time(start_time_, timenow) > timeout_)      
	{
	  std::cout<<"Out of time! Terminating convergence checks early."
		   <<std::endl;
	  break;
	}

      if (gpt::out_of_memory)
	{
	  std::cout<<"Out of memory! Terminating convergence checks early."
		   <<std::endl;
	  break;
	}

      // Analyze the state at the front of the queue.
      temp = open.front();
      open.pop_front();
      int action = 0;
      hashEntry_t<succDataContainer*> *succ_entry = NULL;
      succDataContainer* succ = NULL; 

      // If the state is being analyzed for more than one step-to-go, find the
      // apparent best action in it, the action's value, and the state's 
      // successors under that action.
      if (temp.second.second > 1)
	{
	  succ_entry = hash_table_->get_from_succ_hash(*temp.first->state(), 
						             temp.second.first);
	  action = hash_table_->bestAction(*temp.first->state(), 
	   temp.second.first, temp.second.second, *problem_, value, succ_entry);
	  succ_entry->set_bits(DELETE_PROTECTED);
	}
      else
	{
	  // Otherwise (if there is only one step-to-go), find the value of the
	  // best action by looking at actions' immediate rewards.
	  value =  problem_->eval_all_actions( *temp.first->state() );
	}

      assert(action != -1);
     
      // Now, check whether the state's old value and the value of the apparent
      // best action in it (i.e., the state's new value) are within a fraction 
      // of epsilon of each other.
      double biggerVal = MAX(value, temp.first->value());
      double smallerVal = MIN(value, temp.first->value());
      
      if (((biggerVal >= 0 && (1-gpt::epsilon) * biggerVal > smallerVal) 
	|| (biggerVal < 0 && (1+gpt::epsilon) * biggerVal > smallerVal)))
	{
          // If not, the currently analyzed state isn't solved. 
	  rv = false;
	}
	  
      // Note that if the check above fails we could stop here and return. 
      // However, for efficiency reasons, we will examine the successors of this
      // state anyways and, most importantly, update those that aren't solved
      // yet. Counterintuitively, this "extra work" speeds up convergence and
      // detection of solved states across in the long run.

      // If the state is being analyzed for more than one step-to-go, check
      // whether its immediate successors under the apparent best action (i.e.,
      // its "children") have converged.
      if (temp.second.second > 1)
	{
	  bool bAllChildrenConv = true;
	  assert(succ_entry != NULL);
	  succ = succ_entry->value();
	  
	  for (succDataContainer::succHash_t::const_iterator it 
	        = succ->succ_begin(action); it != succ->succ_end(action); it++)
	    {
	      tms timenow;
	      times(&timenow);
	      
	      // If we are out of time, stop now.
	      if (elapsed_time(start_time_, timenow) > timeout_)      
		{
                  std::cout<<"Terminating convergence checks early"<<std::endl;
		  break;
		}
	      
	      hashEntry_t<double> *entry = hash_table_->get(*(it.state()), 
	                              it.hash_value(), temp.second.second - 1);
	      
	      if( !(entry->bits() & SOLVED) )
		{
                  // If any of the children aren't solved, and if they aren't
                  // in our queue yet, attach them to the back of the queue 
                  // for subsequent analysis.
		  bAllChildrenConv = false;
		  std::pair<hashEntry_t<double>*, std::pair<unsigned, size_t> > 
		    temp_cand = std::pair<hashEntry_t<double>*, 
		        std::pair<unsigned, size_t> >(entry, std::pair<unsigned,
			      size_t>(it.hash_value(), temp.second.second - 1));

		  if (aux.find( temp_cand  ) == aux.end()) 
		    {
		      open.push_front(temp_cand );
		      aux.insert( temp_cand );
		    }
		}
            }

	  // If all the immediate successors of the state currently being 
	  // analyzed turn out to have been solved, updating this state once
	  // again is all takes to make it solved as well. Do so, and mark it
	  // as solved.
	  if (bAllChildrenConv)
	    {
	      temp.first->update(value);
	      temp.first->set_bits( SOLVED );
	    }
	}
      else
	{
          // If the state is being analyzed for one step-to-go, then it gets
          // solved after just one update (which assigns to this state the 
          // immediate reward of the highest-reward action in it.). Do this
          // update, and mark the state as solved.
	  temp.first->update(value);
	  temp.first->set_bits( SOLVED );
	}

      // Delete or release the information about the successors of the 
      // currently analyzed state, we don't need it anymore for now.
      if (succ_entry != NULL)
	{
	  if (!succ->owned_by_hash())
	    {
	      delete succ_entry;
	      delete succ;
	      succ_entry = NULL;
	      succ = NULL;
	    }
	  else
	    {
	      succ_entry->reset_bits(DELETE_PROTECTED);
	    }    
	}
    }
;
  return( rv );
}


void 
lr2tdp_t::reset_state()  
{
  rounds_remaining_ = gpt::num_rounds;
  gpt::solved_completely = true;
  tms old_start_time = start_time_;
  times(&start_time_);
  timeout_ = gpt::timeout - elapsed_time(old_start_time, start_time_);
  hash_table_->cleanup();
}


const action_t *
lr2tdp_t::next( const state_t &state )
{
  tms timenow;

  // Solve the given state if it's not solved yet and if there is time.
  times(&timenow);

  if ((!hash_table_->initialized( steps_to_go_ ) 
      || !hash_table_->find( state, steps_to_go_) 
      || !(hash_table_->find( state, steps_to_go_)->bits() & SOLVED))
      && elapsed_time(start_time_, timenow) <= timeout_
      && !gpt::out_of_memory)
    {
      solve_reverse_iter( state, steps_to_go_);
    }

  if (steps_to_go_ == problem_->horizon() 
      && hash_table_->initialized(steps_to_go_))
    {
      hashEntry_t<double> *node = hash_table_->find(state, steps_to_go_);

      if (node != NULL && node->bits() & SOLVED)
	{
	  gpt::solved_completely = true;
	}
    }

  int action = 0;
  double value = 0;

  // If the requested state has been solved, return the best action in it.
  if (hash_table_->initialized( steps_to_go_ ) 
      && hash_table_->find( state, steps_to_go_) 
      && (hash_table_->find( state, steps_to_go_)->bits() & SOLVED))
    {
      action = hash_table_->bestAction( state, steps_to_go_, *problem_, value );
    }
  else 
    {
      // Otherwise, return an action chosen uniformly at random. 
      action = floor(drand48()*problem_->actionsT().size());
    }
 
  // Update our counters for the number of steps-to-go remaining in the current
  // policy execution round and for the number of remaining policy execution 
  // rounds.
  steps_to_go_--;
    
  if (steps_to_go_ == 0)
    {
      rounds_remaining_--;
      steps_to_go_ = problem_->horizon();
    }

  return( problem_->actionsT()[action] );
}



/*******************************************************************************
 *
 * Extension of lr2tdp for IPPC
 *
 ******************************************************************************/

lr2tdp_extension_for_IPPC_t::lr2tdp_extension_for_IPPC_t( 
			       const problem_t &problem, hash_FH_t &hash_table )
  : lr2tdp_t(problem, hash_table), first_action_(true),
    policy_to_use_(RAW_WITH_MYOPIC_REPLACEMENT)
{
  timeout_ = gpt::timeout;
  times(&start_time_);
  // Initialize the app variable that will help us keep track of the policy's
  // reward.
  std::pair<Function,bool> fp 
                       = problem_->domain().functions().find_function("reward");

  // The reward variable _must_ be present
  assert(fp.second == true);
  app_ = &Application::make_application(fp.first, TermList());
  evaluate_primitive_cyclic_policies();
  
  // Account for the time spent performing the initialization tasks.
  tms old_start_time = start_time_;
  times(&start_time_);
  timeout_ = MAX(0, timeout_ - elapsed_time(old_start_time, start_time_));
}


double
lr2tdp_extension_for_IPPC_t::simulate(int defaultPolicy)
{
  ValueMap vm;
  vm[app_] = 0;
  state_t *current = NULL;

  if (defaultPolicy == RANDOM)
    {
      std::cout<<"******** TRYING A RANDOM POLICY **************"<<std::endl;
    }
  else if (defaultPolicy == RAW_WITHOUT_MYOPIC_REPLACEMENT)
    {
      std::cout<<"******** TRYING THE RAW POLICY PRODUCED BY THE PLANNER "
	       <<"**************"<<std::endl;
    }
  else if (defaultPolicy == RAW_WITH_MYOPIC_REPLACEMENT)
    {
      std::cout<<"******** TRYING THE RAW POLICY WITH REPLACEMENT OF MYOPIC "
	       <<"ACTIONS **************"<<std::endl;
    }
  else
    {
      std::cout<<"******** TRYING THE PRIMITIVE CYCLIC POLICY FOR ACTION "
	       <<problem_->actionsT()[defaultPolicy]->name()
               <<" **************"
	       <<std::endl;
    }

  for (size_t r = 0; r < gpt::num_rounds; r++)
    {
      current = new state_t(init_state_);

      for (size_t stepsToGo = problem_->horizon(); stepsToGo > 0; stepsToGo--)
	{
	  // Initially, assign an invalid value to the action variable.
	  size_t a = -1;

	  if (defaultPolicy == RAW_WITHOUT_MYOPIC_REPLACEMENT)
	    {
	      a = next_sim(*current, stepsToGo, false);
	    }
	  else if (defaultPolicy == RAW_WITH_MYOPIC_REPLACEMENT)
	    {
	      a = next_sim(*current, stepsToGo, true);
	    }
	  else
	    {
	      a = (defaultPolicy > RANDOM ? defaultPolicy
		   : floor(drand48()*problem_->actionsT().size()));
	    }			   
			  
	  problem_->actionsT()[a]->affect( *current, &vm );
	}

      delete current;
    }
    
  return (vm[app_] /  ((double) gpt::num_rounds));
}


int 
lr2tdp_extension_for_IPPC_t::next_sim(const state_t &state, size_t stepsToGo, 
				                             bool replaceMyopic)
{
  double value = 0;
  int action = 0;
  size_t longestAvHor = stepsToGo;
  bool bNoneAvailable = true;

  // Check if state has been solved for any number of steps-to-go > 1
  // up to stepsToGo.
  if (!hash_table_->initialized(stepsToGo) 
      || !hash_table_->find(state, stepsToGo) 
      || !(hash_table_->get(state, stepsToGo)->bits() & SOLVED)) 
    {
      hashEntry_t<double> *node = NULL;

      for (size_t j = stepsToGo - 1; j > 0; j--)
	{
	  if (hash_table_->initialized(j))
	    {
	      node = hash_table_->find(state, j);

	      if (node && (node->bits() & SOLVED))
		{
		  longestAvHor = j;
		  bNoneAvailable = false;
		  break;
		}
	    }
	}
    }
  else
    {
      bNoneAvailable = false;
    }

  // If it has, return the action for the largest number of steps-to-go
  // for which s has been solved, or the best primitive cyclic policy's
  // action
  if (!bNoneAvailable)
    {
      if (longestAvHor > 1 || (longestAvHor == 1 && !replaceMyopic))
	{
	  action = hash_table_->bestAction( state, longestAvHor, *problem_, 
					                                value );
	}
      else 
	{
	  action = (best_cyclic_policy_ >= 0 ? 
	     best_cyclic_policy_: floor(drand48()*problem_->actionsT().size()));
	}
    }
  else
    {
      // Otherwise, simply return the best primitive cyclic policy's
      // action (this action may need to be chosen randomly if the 
      // best primitive cyclic policy is random.
      if (best_cyclic_policy_ >= 0)
	{
	  action = best_cyclic_policy_;
	}
      else
	{
	  action = floor(drand48()*problem_->actionsT().size());
	}
    }

  assert((action >= 0) && (action < (int)problem_->actionsT().size()));
  return action;
}


void
lr2tdp_extension_for_IPPC_t::evaluate_primitive_cyclic_policies()
{
  // Determine the best primitive cyclic policy
  best_cyclic_policy_value_ = -1000000000;
  best_cyclic_policy_ = RANDOM;

  for (int defaultPolicy = RANDOM; 
            defaultPolicy < ((int)problem_->actionsT().size()); defaultPolicy++)
    {
      double val = simulate(defaultPolicy);

      if (val > best_cyclic_policy_value_)
	{
	  best_cyclic_policy_value_ = val;
	  best_cyclic_policy_ = defaultPolicy;
	}

      std::cout<<"The primitive cyclic policy for action "
	       <<(defaultPolicy == -1 ? "random" 
		  : problem_->actionsT()[defaultPolicy]->name())
	       <<" has an expected reward of "<<val<<std::endl;
    } 

  gpt::best_default_policy_reward = best_cyclic_policy_value_;
}



/*******************************************************************************
 *
 * glutton
 *
 ******************************************************************************/

glutton_t::glutton_t( const problem_t &problem, hash_FH_t &hash_table )
   : lr2tdp_extension_for_IPPC_t(problem, hash_table)
{ }


void 
glutton_t::reset_state()  
{
  lr2tdp_t::reset_state();
  first_action_ = true;
  
  if (gpt::use_best_default_policy)
    {
      policy_to_use_ = best_cyclic_policy_;
    }
}


const action_t *
glutton_t::next( const state_t &state )
{
  std::cout<<" # steps-to-go: "<<steps_to_go_<<std::endl;
  tms timenow;

  // If we want to use a planner-produced policy(and by default this is what we
  // want to use) as opposed to a primitive cyclic one, invoke LR^2TDP to solve
  // the given state.
  if (policy_to_use_ == RAW_WITH_MYOPIC_REPLACEMENT 
      || policy_to_use_ == RAW_WITHOUT_MYOPIC_REPLACEMENT)
    {
      times(&timenow);

      if ((!hash_table_->initialized(steps_to_go_) 
	   || !hash_table_->find(state, steps_to_go_) 
	   || !(hash_table_->find(state, steps_to_go_)->bits() & SOLVED))
	  && elapsed_time(start_time_, timenow) <= timeout_ 
	  && !gpt::out_of_memory)
	{
	  solve_reverse_iter(state, steps_to_go_);
	}

      if (steps_to_go_ == problem_->horizon() 
	  && hash_table_->initialized(steps_to_go_))
	{
	  hashEntry_t<double> *node = hash_table_->find(state, steps_to_go_);

	  if (node != NULL && node->bits() & SOLVED)
	    {
	      gpt::solved_completely = true;
	    }
	}
    }

  // If this is the first invocation of this method, the above call to LR^2TDP
  // caused a lot of offline planning that attempted to solve not just the 
  // requested state but the entire problem. Figure out what we should do 
  // next...
  if (first_action_)
    {
      first_action_ = false;

      // Whatever solution we got at this point, it may be only partial, and 
      // may also be of a low quality. At this point, we consider several 
      // alternative policies (see the documentation for lr2tdp::simulate(.) and
      // lr2tdp::next_sim(.), some derived from the raw one produced by LR^2TDP
      // and some not, that may turn out to be better than the raw LR^2TDP 
      // output. We evaluate them by simulation and in the future will use the
      // best one according to their empirical expected reward.      
      double valOfNormalPolicy = simulate(RAW_WITHOUT_MYOPIC_REPLACEMENT);
      std::cout<<"VALUE OF THE RAW POLICY: "<<valOfNormalPolicy<<std::endl;
      double valOfSubstPolicy = simulate(RAW_WITH_MYOPIC_REPLACEMENT);
      std::cout<<"VALUE OF THE RAW POLICY WITH REPLACEMENT OF MYOPIC ACTIONS: "
	       <<valOfSubstPolicy<<std::endl;
      std::cout<<"VALUE OF THE BEST PRIMITIVE CYCLIC POLICY: "
	       <<best_cyclic_policy_value_<<std::endl;

      if (valOfNormalPolicy >= valOfSubstPolicy 
	  && valOfNormalPolicy >= best_cyclic_policy_value_)
	{
	  policy_to_use_ = RAW_WITHOUT_MYOPIC_REPLACEMENT;
	  std::cout<<"***** WILL USE THE RAW POLICY *****"<<std::endl;
	}
      else if (valOfSubstPolicy >= valOfNormalPolicy 
	       && valOfSubstPolicy >= best_cyclic_policy_value_)
	{
	  policy_to_use_ = RAW_WITH_MYOPIC_REPLACEMENT;
	  std::cout<<"***** WILL USE THE RAW POLICY WITH REPLACEMENT OF MYOPIC "
		   <<"ACTIONS *****"<<std::endl;
	}
      else
	{
	  policy_to_use_ = best_cyclic_policy_;
	  std::cout<<"***** WILL USE A PRIMITIVE CYCLIC POLICY *****"
		   <<std::endl;
	}
    }

  // The best policy (among the alternatives we consider) has been determined
  // during the first invocation of this method, and we will use that policy
  // during the first one and all subsequent invocations to select an action
  // for the requested state.
  int action = 0;

  if (policy_to_use_ == RANDOM)
    {
      action = floor(drand48()*problem_->actionsT().size());
    }
  else if (policy_to_use_ >= 0) // primitive cyclic 
    {
      action = best_cyclic_policy_;
    }
  else if (policy_to_use_ == RAW_WITH_MYOPIC_REPLACEMENT)
    {
      action = next_sim(state, steps_to_go_, true);
    }
  else // RAW_WITHOUT_MYOPIC_REPLACEMENT
    {
      action = next_sim(state, steps_to_go_, false);
    }

  assert(action >= 0 && action <= (int)problem_->actionsT().size());
  
  // Update our counters for the number of steps-to-go remaining in the current
  // policy execution round and for the number of remaining policy execution 
  // rounds.
  steps_to_go_--;

  if (steps_to_go_ == 0)
    {
      rounds_remaining_--;
      steps_to_go_ = problem_->horizon();
    }

  return( problem_->actionsT()[action] );
}



/*******************************************************************************
 *
 * gourmand
 *
 ******************************************************************************/

gourmand_t::gourmand_t( const problem_t &problem, hash_FH_t &hash_table )
  : lr2tdp_extension_for_IPPC_t(problem, hash_table)
{
  time_alloc_for_lookahead_ = new double[problem_->horizon() + 1];
  exp_time_for_lookahead_ = new double[problem_->horizon() + 1];
  num_samples_ = new size_t[problem_->horizon() + 1];
  largest_target_ = 1;

  // Initialize the arrays hold the amount of time allocated to decision epochs
  // at various steps-to-go, the empirical expected amount of time required to
  // solve a state for a given number of steps-to-go, and the numbers of 
  // samples these estimates are based on.
  for (size_t i = 1; i <= problem_->horizon(); i++)
    {
      // Initially, the total time to solve a problem gets divided equally 
      // among all the decision epochs.
      time_alloc_for_lookahead_[i] = ((double) timeout_) 
	/ (problem_->horizon() * gpt::num_rounds);
      exp_time_for_lookahead_[i] = timeout_;
      num_samples_[i] = 0;
    }
}


gourmand_t::~gourmand_t()
{
  if( gpt::verbosity >= 500 )
    std::cout << "<gourmand>: deleted" << std::endl;	 
}


void 
gourmand_t::realloc_time(double timeDelta, size_t stepsRemaining)
{
  for (size_t j = 1; j <= problem_->horizon(); j++)
    {
      int numCandidatesForRaise = 
	(rounds_remaining_ - 1) * (problem_->horizon()) + stepsRemaining - 1;

      if (numCandidatesForRaise > 0)
	{
	  time_alloc_for_lookahead_[j] = MAX(0.01, 
	       time_alloc_for_lookahead_[j] 
	       + (timeDelta) / numCandidatesForRaise);
	}  
    }
}


void 
gourmand_t::update_comp_time_stats(double new_sample, size_t stepsRemaining)
{
  // Update the running average for the time it takes to solve for the given
  // number of steps-to-go with the new measurement.
  exp_time_for_lookahead_[stepsRemaining] = 
    (((double) num_samples_[stepsRemaining]) 
    / (num_samples_[stepsRemaining] + 1))
    * exp_time_for_lookahead_[stepsRemaining] 
    + new_sample / (num_samples_[stepsRemaining] + 1);
  num_samples_[stepsRemaining]++;

  // This makes sure that we gradually forget the old samples.
  if (num_samples_[stepsRemaining] > SAMPLE_BUFFER_SIZE)
    {
      num_samples_[stepsRemaining] = SAMPLE_BUFFER_SIZE;
    } 
}


void 
gourmand_t::solve_reverse_iter(const state_t &state, size_t stepsRemaining ) 
{
  times(&start_time_);
  size_t target = stepsRemaining;
  bool attempting_largest_target = false;

  // First, we figure out when to stop computing an action for the current
  // decision epoch.

  // If this is the first decision epoch of the first policy execution round..
  if (first_action_ && rounds_remaining_ == gpt::num_rounds)
    {
      // ... then set the amount of time for this decision epoch to be 
      // the total number of execution rounds times the amount of time 
      // initially allocated to a decision epoch during which the agent 
      // has H steps to go (where H is the MDP's horizon). The intuition here
      // is that, since each policy execution attempt starts in the same 
      // state, we should take all the time allocated to this decision
      // epoch in each of the policy execution attempts and use it up all at
      // once. Hopefully, this will let us come up with a better action
      // for this state.
      timeout_ = gpt::num_rounds * time_alloc_for_lookahead_[stepsRemaining];
    }
  else // this is not the first decision epoch of the first round
    {
      // An obvious termination condition would be to allocate
      // some amount of time to planning for this decision epoch and stop when
      // either this amount of time runs out or when we solve the current 
      // state s for the current number of steps-to-go t. However,  this 
      // approach has a drawback. When the allocated time runs out, chances 
      // are that we will have solved s for some number of steps to go t' < t,
      // and are in the process of solving it for steps-to-go = t' + 1. The 
      // issue is that, since s hasn't been solved completely for t'+ 1
      // steps-to-go, we will want to return an optimal action for s for 
      // t' steps-to-go. Thus, the time spent from the moment we solve 
      // s for t' until we have to stop is essentially wasted.
      //
      // Therefore, we use a less wasteful termination condition. We allocate
      // some amount of time to plan for the current decision epoch, and 
      // estimate the largest number of steps-to-go t' <= t for which we 
      // *should* be able to solve the current state s. We terminate 
      // the computation for the current decition epoch when we either solve
      // s for t' steps-to-go or run out of the allocated time. The intuition
      // is that, if our estimate of t' is good, we will rarely run out of
      // allocated time when solving for t'. More often than not, we will
      // solve for t' and, without wasting any more time, proceed to the next
      // decision epoch.
      //
      // To implement the above idea, we need to have a mechanism that
      // allocates time to every decision epoch and that figures out, based
      // on the allocated time and data about how long it takes, on average,
      // to solve a state for different numbers of steps-to-go, the target
      // number of steps-to-go t' for which we can reasonably expect to solve
      // the current state within the allocated time. The code below does
      // both of these things. 
	

      if (stepsRemaining == problem_->horizon())
	{
	  // If this is the beginning of an execution round past the first
	  // one, we have already spent a lot of time solving for the
	  // initial state during the first execution round, so just use
	  // up any leftover time assigned to this decision epoch.
	  timeout_ = time_alloc_for_lookahead_[stepsRemaining];
	  target = stepsRemaining;
	}
      else
	{
	  // If this isn't the first decision epoch or an execution round,
	  // we need to find the the largest number of steps-to-go for 
	  // which we should be able to solve the current state.  
	  for (size_t k = 1; k <= stepsRemaining; k++)
	    {
	      // If the time allocated to this decision epoch is smaller than
	      // the average amount of time it takes to solve for k 
	      // steps-to-go...
	      if (time_alloc_for_lookahead_[stepsRemaining] 
		  < exp_time_for_lookahead_[k])
		{
		  // Then first check if the estimate for the amount of time
		  // to solve for k steps-to-go (i.e., 
		  // exp_time_for_lookahead_[k]) is reliable. Essentially,
		  // an estimate is reliable only if we have solved at least 
		  // one state for k steps-to-go previously and recorded the 
		  // amount of time it took. Unfortunately, for large numbers
		  // of steps-to-go we don't have these data, since we may 
		  // have never had a chance to solve any state for that 
		  // many steps-to-go. In particular, there is a largest 
		  // number of steps-to-go, denoted at largest_target_, for
		  // which we have solved a state so far. If k is larger than
		  // that number, then exp_time_for_lookahead_[k] is just some
		  // very large value that we used to initialize our 
		  // exp_time_for_lookahead_ estimates and is effectively 
		  // meaning less. In general, we don't want to try to solve
		  // a state for such k, since we don't know how long this
		  // will take. However, if k is just one more than 
		  // largest_target_, we make an exception and give it a try.
		  if (k - 1 == largest_target_)
		    {
		      attempting_largest_target = true;
		      target = stepsRemaining;
		      timeout_ = MAX(time_alloc_for_lookahead_[stepsRemaining], 
				     3 * exp_time_for_lookahead_[k-1]);
		      break;
		    }
		  else if (3 * time_alloc_for_lookahead_[stepsRemaining] 
			   > exp_time_for_lookahead_[k])              
		    {
		      // Otherwise, if solving for k takes an amount of time
		      // within some small factor of the allocated amount, go
		      // k steps-to-go. This fudge factor is needed because 
		      // solving for k steps-to-go may occasionally take 
		      // slightly more or less time than we have estimated. 
		      target = MAX(k, 1);
		      timeout_ = 3 * time_alloc_for_lookahead_[stepsRemaining];
		    }
		  else
		    {
		      // Finally, if none of the above holds, the previous
		      // number of steps-to-go that we analyzed, k-1, must 
		      // have been the largest one we can realistically hope 
		      // to solve for within allocated time, so go for k-1.
		      target = MAX(k-1, 1);
		      timeout_ = MAX(time_alloc_for_lookahead_[stepsRemaining], 
				     3 * exp_time_for_lookahead_[k-1]);
		      break;
		    }
		}
	      else if (k == stepsRemaining)
		{
		  // If we got to k = stepsRemaining, go for stepsRemaining
		  target = MAX(k, 1);
		  timeout_ = MAX(time_alloc_for_lookahead_[stepsRemaining], 
				 3 * exp_time_for_lookahead_[k]);
		  break;
		}
	    }  
	}
    }

  // Now that we have figured out when to stop computing an action for the
  // curremt decision epoch, let's try actually computing it.

  double actualCompTime = 0;
  double allocatedCompTime  = 0;
  tms timenow;

  // First, check whether the current state has already been solved for the 
  // current number of steps-to-go
  hashEntry_t<double> *node = NULL;

  if (hash_table_->initialized(stepsRemaining))
    {
      node = hash_table_->get(state, stepsRemaining);
    }

  if(node != NULL && node->bits() & SOLVED) 
    {
      // If it has already been solved, allocate the computation time we haven't
      // used in this decision epoch to the future decision epochs. 
      times(&timenow);
      actualCompTime = elapsed_time(start_time_, timenow);
      allocatedCompTime = time_alloc_for_lookahead_[stepsRemaining];
      realloc_time(allocatedCompTime - actualCompTime, stepsRemaining);
      return;
    }

  // Otherwise, figure out the largest number of steps-to-go smaller than
  // the current number of steps-to-go for which the current state has been
  // solved.
  size_t max_solved_steps_to_go = 0;

  for (size_t i = stepsRemaining - 1; i >= 1; i--)
    {
      if (hash_table_->initialized(i))
	{
	  node = hash_table_->get(state, i);
	}

      if (node != NULL && node->bits() & SOLVED)
	{
	  max_solved_steps_to_go = i;
	  break;
	}
    }

  // If this number exceeds the target number of steps-to-go that we planned to
  // solve for in this decision epoch, we will simply try to solve as much past
  // the target as possible, until we either run out of time allocated for this
  // decision epoch or solve the current state for the current number of 
  // steps-to-go, whichever comes earliest. So, set the target to the current
  // number of steps-to-go.
  if (max_solved_steps_to_go >= target)
    {
      timeout_ = time_alloc_for_lookahead_[stepsRemaining];
      target = stepsRemaining;
    }

  // Now, compute the optimal action in the current state for starting from 
  // 1 past the largerst number of steps-to-go for which it has already been
  // solved to the target number, or until you run out of time allocated to the
  // current decision epoch. Essentially, we will be using reverse iterative 
  // deepening (in the same manner as LR^2TDP), but we will also time how long
  // solving for each number of steps-to-go takes us and store these stats.
  for (size_t i = max_solved_steps_to_go + 1; i <= target; i++)
    {
      if (!hash_table_->initialized(i))
	{
	  hash_table_->initialize(i);
	}

      node = hash_table_->get(state, i);

      if( !(node->bits() & SOLVED) ) 
	{
	  solve( node, i );
	}

      if (node->bits() & SOLVED) 
	{
	  // If this is the first decision epoch of the first policy execution
	  // round, we need to initialize the stats of how long it takes, on 
	  // average, to solve a state for a particular number of states-to-go.
	  if (first_action_ && rounds_remaining_ == gpt::num_rounds)
	    {
	      times(&timenow);
	      actualCompTime = elapsed_time(start_time_, timenow);
	      allocatedCompTime = time_alloc_for_lookahead_[i];

	      // If the amount of time it took to solve for steps-to-go = i is
	      // smaller than we thought (and allocated to the current decision
	      // epoch)...
	      if (actualCompTime < allocatedCompTime)
		{
		  // ... then, for future decision epochs with this number of 
		  // steps-to-go,allocate the amount of time we just measured. 
		  // Due to the possible measurement imprecision, this number 
		  // may turn out to be 0; in this case, set it to some small 
		  // positive number, e.g., 0.01.
		  time_alloc_for_lookahead_[i] = MAX(actualCompTime, 0.01);

		  // Also, distribute the unused amount of time initially 
		  // allocated to the current decision epoch equally among 
		  // the future decision epochs.
		  if (i < problem_->horizon())
		    {
		      for (size_t j = i + 1; j <= problem_->horizon(); j++)
			{
			  time_alloc_for_lookahead_[j] += 
			    (allocatedCompTime - actualCompTime) 
			    / (problem_->horizon() - i);
			}
		    }
		}

	      // Update the estimate of the time it take to solve a state for 
	      // steos-to-go = i.
	      update_comp_time_stats(actualCompTime, i);

	      // Update the largest number of steps-to-go for which we have 
	      // solved any state so far.
	      largest_target_ = i;

	      // Since this is the first decision epoch of the first policy
	      // execution round, allocate to it the total amount of time
	      // allocated for all the decision epochs for 
	      // steps-to-go = horizon. The intuition here is that in all these
	      // decision epochs, we are trying to solve the same state, the
	      // initial one, for the same number of steps-to-go, equal to the 
	      // horizon of the problem. Therefore, it makes sense to use up
	      // all the time allocated to such decision epochs now in order
	      // to compute as good of an action for this state as possible
	      // early on. 
	      timeout_ = gpt::num_rounds 
		* time_alloc_for_lookahead_[stepsRemaining];
	    }

	  // If we managed to solve the current state for the target number 
	  // of steps-to-go that we have set for this decision epoch, update
	  // our estimate for the amount of time it takes to solve a state for
	  // this number of steps-to-go and reallocate the unused computation 
	  // time for this decision epoch to future decision epochs. 
	  if (i == target)
	    {
	      times(&timenow);
	      actualCompTime = elapsed_time(start_time_, timenow);
	      allocatedCompTime = time_alloc_for_lookahead_[stepsRemaining];
	      realloc_time(allocatedCompTime - actualCompTime, stepsRemaining);
	      update_comp_time_stats(actualCompTime, i);
	    }
	}
      else // if we tried, but didn't manage to solve for steps-to-go = i...
	{
	  // This means that the amount of time we allocated to this decision
	  // epoch wasn't enough to solve the curernt state for the number
	  // of steps-to-go that we set as target for this decision epoch.
	  // In other words, we underestimated the amount of time it takes
	  // to solve for steps-to-gp = target.
	  times(&timenow);
	  actualCompTime = elapsed_time(start_time_, timenow);
	  allocatedCompTime = time_alloc_for_lookahead_[stepsRemaining];
	  realloc_time(allocatedCompTime - actualCompTime, stepsRemaining);

	  // Since our estimate for the amount of time to solve for 
	  // steps-to-go = target turned out to be erring on the low side,
	  // increase this estimate by updating it with some number
	  // greater than the amount of time we just used.
	  if (!attempting_largest_target && !gpt::out_of_memory)
	    {
	      update_comp_time_stats(3 * actualCompTime, target);
	    }
	  else if (attempting_largest_target && i - 1 < largest_target_ 
		   && !gpt::out_of_memory)
	    {
	      update_comp_time_stats(3 * actualCompTime, largest_target_);
	    }

	  break;
	}
    }

  // If this was the first decision epoch of the first policy execution round,
  // it has used up all of the time allocated so far to the decision epochs
  // for steps-to-go = horizon for all of the policy execution rounds.
  // Therefore, set the time allocated to such decision epochs in the remaining
  // policy execution rounds to 0 (it may increase later on if other decision
  // epochs take less time than we currently expect).
  if  (first_action_ && rounds_remaining_ == gpt::num_rounds)
    {
      std::cout<<"\n\n******INITIAL TIME ALLOCATION*******"<<std::endl;

      for (size_t i = 1; i <= problem_->horizon(); i++)
	{
	  std::cout<<i<<" steps-to-go: "<<time_alloc_for_lookahead_[i]<<"s"<<std::endl;
	}

      std::cout<<"************************************\n\n"<<std::endl;

      time_alloc_for_lookahead_[stepsRemaining] = 0;
    }

  first_action_ = false;
}


void 
gourmand_t::reset_state()  
{
  lr2tdp_t::reset_state();
  first_action_ = true;  

  for (size_t i = 1; i <= problem_->horizon(); i++)
    {
      time_alloc_for_lookahead_[i] = ((double) timeout_) 
	/ (problem_->horizon() * gpt::num_rounds);
    }

  if (gpt::use_best_default_policy)
    {
      policy_to_use_ = best_cyclic_policy_;
    }
}


const action_t *
gourmand_t::next( const state_t &state )
{
  std::cout<<" # steps-to-go: "<<steps_to_go_<<std::endl;

  // If we want to use a planner-produced policy(and by default this is what we
  // want to use) as opposed to a primitive cyclic one, invoke LR^2TDP to solve
  // the given state.
  if (policy_to_use_ == RAW_WITH_MYOPIC_REPLACEMENT 
      || policy_to_use_ == RAW_WITHOUT_MYOPIC_REPLACEMENT)
    {
      if ((!hash_table_->initialized(steps_to_go_) 
	   || !hash_table_->find(state, steps_to_go_) 
	   || !(hash_table_->find(state, steps_to_go_)->bits() & SOLVED))
	  && !gpt::out_of_memory)
	{
	  solve_reverse_iter(state, steps_to_go_);
	}

      if (steps_to_go_ == problem_->horizon() 
	  && hash_table_->initialized(steps_to_go_))
	{
	  hashEntry_t<double> *node = hash_table_->find(state, steps_to_go_);

	  if (node != NULL && node->bits() & SOLVED)
	    {
	      gpt::solved_completely = true;
	    }
	}
    }

  int action = 0;

  if (policy_to_use_ == RANDOM)
    {
      action = floor(drand48()*problem_->actionsT().size());
    }
  else if (policy_to_use_ >= 0) // primitive cyclic 
    {
      action = best_cyclic_policy_;
    }
  else if (policy_to_use_ == RAW_WITH_MYOPIC_REPLACEMENT)
    {
      action = next_sim(state, steps_to_go_, true);
    }
  else // RAW_WITHOUT_MYOPIC_REPLACEMENT
    {
      action = next_sim(state, steps_to_go_, false);
    }
    
  assert(action >= 0 && action <= (int)problem_->actionsT().size());

  // Update our counters for the number of steps-to-go remaining in the current
  // policy execution round and for the number of remaining policy execution 
  // rounds.
  steps_to_go_--;

  if (steps_to_go_ == 0)
    {
      rounds_remaining_--;
      steps_to_go_ = problem_->horizon();
    }

  return( problem_->actionsT()[action] );
}
