#ifndef ALGORITHMS_H
#define ALGORITHMS_H


#include "global.h"
#include "hash.h"
#include <assert.h>
#include <math.h>
#include <deque>
#include <set>

class action_t;
class hash_t;
class state_t;
class problem_t;
class heuristic_t;
class Action;


/*******************************************************************************
 *
 * algorithm (abstract class)
 *
 ******************************************************************************/

class algorithm_t
{
protected:
  const problem_t *problem_;


public:

  algorithm_t() : problem_(0) { }
  algorithm_t( const problem_t &problem ) : problem_(&problem) { }
  virtual ~algorithm_t() { }
  virtual void reset_state() { }
  virtual const action_t *next( const state_t &state ) = 0;
};


/*******************************************************************************
 *
 * random
 *
 ******************************************************************************/

/*
  Implements an algorithm that picks an action at random in any state. This
  policy is sometimes useful as a baseline for less trivial algorithms.
*/
class random_t : public algorithm_t
{
public:

  random_t( const problem_t &problem ) : algorithm_t(problem) { }
  virtual ~random_t() { }
  virtual const action_t *next( const state_t &state )
    {
      std::set<const action_t*> actions;
      actionList_t::const_iterator ai;
      std::set<const action_t*>::const_iterator it;

      for( ai = problem_->actionsT().begin(); ai != problem_->actionsT().end();
	   ++ai )
	{
	  if( (*ai)->enabled( state ) )
	    actions.insert( *ai );
	}

      assert( actions.size() > 0 );
      unsigned action = lrand48() % actions.size();
      for( it = actions.begin(); it != actions.end(); ++it )
	if( action-- == 0 ) return( *it );
      return( 0 );
    }
};


/*******************************************************************************
 *
 * lr2tdp
 *
 ******************************************************************************/

struct hashEntry_pair_hash
{
  size_t operator()(std::pair<hashEntry_t<double>*, 
		                        std::pair<unsigned, size_t> > he) const;
}; 


/*
  Implements the  Reverse Iterative Deepening LRTDP algorithm (called LR^2TDP). 
  For more details, please refer to Kolobov, Dai, Mausam, Weld, "Reverse 
  Iterative Deepening for Finite-Horizon MDPs with Large Branching Factors", 
  ICAPS-2012.
*/
class lr2tdp_t : public algorithm_t
{
protected:

  // A hash table that maps augmented states (tuples of the form (s, t), where
  // s is a state and t is the number of steps-to-go till the end of the 
  // process) to values. When the planner halts, these values approximately
  // represent the maximum reward one can obtain from a given state over a given
  // number of steps-to-go.
  hash_FH_t *hash_table_;

  // The number of steps-to-go remaining till the end of the current policy
  // execution round. Initially, it should be set to the problem's horizon,
  // and the planner will keep track of it from then on, resetting it to
  // the horizon value whenever a new policy execution round starts.
  size_t steps_to_go_;

  // The time when the planner is initialized.
  tms start_time_;

  // The amount of time the planner has for completing the next computational
  // step. Initially, it is set to the user-specified amount of time for 
  // solving the given problem.
  double timeout_;

  // The number of policy execution rounds remaining. Initially, it should
  // be set to gpt::num_rounds.
  size_t rounds_remaining_;

  // An array of state-value pairs. Needed for intermediate policy 
  // computations.
  std::pair<state_t*,double> *display_;

  // The initial state from which the planner should find a policy.
  state_t init_state_;

  /*
    Computes the (near-) optimal value of the given state for the given 
    number of steps-to-go using the standard LRTDP algorithm, as long as there 
    is enough time.
  */
  void solve( hashEntry_t<double> *node, size_t stepsRemaining );
  
  /*
    Runs an LRTDP trial starting at the given step for stepsRemaining steps.
    Each trial consists in choosing the greedy best action in the current state,
    updating the value of the current state, sampling a successor under that 
    action, transitioning to that successor, and repeating the above steps
    stepsRemaining times.
  */
  void trial( hashEntry_t<double> *node, size_t stepsRemaining );

  /*
    Checks whether the given action (whose hash value according to hash_FH_t is
    also provided) has been solved for the given number of steps-to-go, i.e., 
    whose value can be changed only by an insignificant amount by any future 
    updates. Once a state has been solved, its value doesn't need to be updated 
    any more, which means that we are done computing a (near-) optimal
    action for it.
  */
  bool checkSolved( hashEntry_t<double> *node, unsigned hash, 
		                                         size_t stepsRemaining);


public:
  
  lr2tdp_t( const problem_t &problem, hash_FH_t &hash_table );
  virtual ~lr2tdp_t();

  /*
    Computes the (near-) optimal value of the given state for the given 
    number of steps-to-go using the reverse iterative deepening LRTDP (LR^2TDP)
    algorithm, as long as there is enough time. In particular, it first solves
    the given state for 1 step to go using ordinary LRTDP (via the solve(.) 
    method, then for 2 steps to go using the 1-step-to-go solution whenever 
    appropriate, then for 3 steps to go, and so on until it either runs out of 
    time of solves the given state for stepsRemaining. See Kolobov, Dai, Mausam,
    Weld, "Reverse Iterative Deepening for Finite-Horizon MDPs with Large 
    Branching Factors", ICAPS-2012 for more details.
  */
  virtual void solve_reverse_iter(const state_t &state, size_t stepsRemaining );

  /* 
     Resets the planner's internal state: clears the hash tables, resets the 
     number of policy execution rounds, etc.
  */
  virtual void reset_state();

  /*
    Returns an action to be executed in the given state. The lr2tdp_t class 
    internally keeps track of the number of steps-to-go, and this method 
    assumes that an action is being requested for that number of steps-to-go.

    This method is used primarily by the client code (client.h and client.cc) 
    when the client receives a request for an action for a particular state 
    from the server. The method assumes that the very first request from the
    server is for an action to execute in the initial state with the number of
    steps-to-go equal to the horizon. When this first request comes 
    and the method is called for the first time, it runs LR^2TDP in an
    offline mode. That is, during the first call to the "next" method, the
    planner tries to solve not just the requested (initial) state, but the 
    entire problem. Thus, calling this method for the first time causes a lot of
    planning to happen, while subsequent calls involve hardly any planning and 
    mostly just return actions determined during the offline planning stage.
  */
  virtual const action_t *next( const state_t &state );
};



/*******************************************************************************
 *
 * Extension of lr2tdp for IPPC
 *
 ******************************************************************************/

/* 
   Contains several variables and methods useful for any extension of LR^2TDP 
   that is meant to compete under the IPPC-2011 conditions.
*/

class lr2tdp_extension_for_IPPC_t : public lr2tdp_t
{
protected:

  // A variable that helps keep track of the reward yielded by various action
  // sequences.
  const Application *app_;

  // A flag indicating whether the planner has already been asked to provide
  // an action for a state. Once this happens, the flag is permanently set to
  // false.
  bool first_action_;

  // The index of the best primitive cyclic policy. See the documentation for
  // lr2tdp::simulate(.) and lr2tdp::next_sim(.) for more info.
  int best_cyclic_policy_;

  // The value of the best primitive cyclic policy. See the documentation for
  // simulate(.) and next_sim(.) methods for more info.
  double best_cyclic_policy_value_;

  // The index of the policy that should be used. 
  int policy_to_use_;

  // Indexes of various default policies considered by the planner.
  static const int RANDOM = -1;
  static const int RAW_WITH_MYOPIC_REPLACEMENT = RANDOM - 1;
  static const int RAW_WITHOUT_MYOPIC_REPLACEMENT 
                                              = RAW_WITH_MYOPIC_REPLACEMENT - 1;

  /*
    Simulates and returns an estimate for the expected reward of a default
    policy starting in a given state with stepsRemaining steps-to-go.

    Since lr2tdp extenstions are expected to make many approximations while 
    computing a policy for a problem and to operate under a time limit, 
    the policies they produce may be partial (i.e., may not specify an action
    for every state). Moreover, if they are given little time to solve a large
    problem, the resulting policy may also be of a low quality. In all these
    cases, it makes sense to compare the planner's raw resulting policy to some
    easy modifications of that policy and to a set of so-called primitive
    cyclic policies, in order to check if any of these alternatives is better 
    than the planner's raw output. The "simulate" method helps in doing this by 
    assessing these alternative policies' quality, in terms of expected
    reward.

    The input parameter defaultPolicy describes the policy that needs to be 
    evaluated. If 0 <= defaultPolicy < gpt::num_actions, then the policy to be 
    evaluatedis the so-called primitive cyclic policy for the action whose 
    number is given by defaultPolicy (recall that any action can be accessed by
    its index in the problem_->actionsT()[] array). A primitive cyclic 
    policy for an action a simply chooses a for execution in every state, i.e.,
    forces the agent to "do the same thing" wherever the agent is. Despite
    being very naive, such policies are sometimes a good option to fall back
    on. For an explanation of why this can happen see Kolobov, Dai, Mausam,
    Weld, "Reverse Iterative Deepening for Finite-Horizon MDPs with Large 
    Branching Factors", ICAPS-2012.

    The defaultPolicy parameter can take on three more values -- RANDOM,
    RAW_WITH_MYOPIC_REPLACEMENT, and RAW_WITHOUT_MYOPIC_REPLACEMENT. RANDOM
    corresponds to the policy that picks an action in any state uniformly
    at random, and the other two correspond to modifications of the planner's 
    raw output policies described in the documentation for the 
    lr2tdp_extension_for_IPPC_t::next_sim method.
  */
  double simulate (int defaultPolicy);

  /*
    Given a partial policy computed by lr2tdp, chooses an action in a given
    state s for a given number of steps-to-go t even if the original partial
    policy doesn't know what to do there.

    First, it checks whether the pair (s, t) has been solved by lr2tdp, i.e.,
    lr2tdp's partial policy knows an action for this augmented state. If so,
    next_sim returns that action. Otherwise, it checks whether the partial
    policy knows an action for any augmented state of the form (s, t'), t'< t, 
    i.e., an action for state s with a smaller number of steps-to-go than 
    requested. If so, next_sim determines the largest t' > 1 s.t. an action for
    (s, t') is known, and returns that action.

    If lr2tdp's partial policy knows an action only for (s, 1) and if the 
    replaceMyopic flag is set, next_sim returns the action corresponding 
    to the best primitive cyclic policy (see the documentation for 
    the simulate(.) method). Otherwise, it returns the partial policy's optimal
    action for (s, 1) (i.e., a myopically best action).

    Finally, if lr2tdp's doesn't know an action for state s for any number 
    of steps to go, next_sim also returns the action corresponding to the best 
    primitive cyclic policy (see the documentation for the simulate(.) method). 

    WARNING: the best primitive cyclic policy must be determined (i.e., 
    the best_cyclic_policy_ must be assigned a meaningful value) _before_ 
    next_sim is called. See the documentation for simulate(.) for 
    an explanation of how to determine the best primitive cyclic policy.
  */
  int next_sim(const state_t &state, size_t stepsToGo, bool replaceMyopic);

  /* 
     Evaluates the set of all primitive cyclic policies and sets 
     best_cyclic_policy_ to the index of the best such policy and 
     best_cyclic_policy_value_ -- to that policy's empirical expected reward.
  */
  void evaluate_primitive_cyclic_policies();


public:

  lr2tdp_extension_for_IPPC_t(const problem_t &problem, hash_FH_t &hash_table);
  virtual ~lr2tdp_extension_for_IPPC_t() { }
  virtual const action_t *next( const state_t &state ) = 0;
};



/*******************************************************************************
 *
 * glutton
 *
 ******************************************************************************/

/*
  Implements Glutton, a solver for finite-horizon MDPs based on Reverse 
  Iterative Deepening LRTDP (called LR^2TDP). For more details, please refer to
  Kolobov, Dai, Mausam, Weld, "Reverse Iterative Deepening for Finite-Horizon 
  MDPs with Large Branching Factors", ICAPS-2012.
*/
class glutton_t : public lr2tdp_extension_for_IPPC_t
{
public:
  
  glutton_t( const problem_t &problem, hash_FH_t &hash_table );
  virtual ~glutton_t() { }

  /*
    See the documentation for lr2tdp_t::reset_state().
  */
  virtual void reset_state();
  
  /*
    Returns an action to be executed in the given state. The Glutton class 
    internally keeps track of the number of steps-to-go, and this method 
    assumes that an action is being requested for that number of steps-to-go.

    This method is used primarily by the client code (client.h and client.cc) 
    when the client receives a request for an action for a particular state 
    from the server. The method assumes that the very first request from the
    server is for an action to execute in the initial state with the number of
    steps-to-go equal to the horizon. When this first request comes 
    and the method is called for the first time, it invokes the planner in an
    offline mode. That is, during the first call to the "next" method, the
    planner tries to solve not just the requested (initial) state, but the 
    entire problem. Thus, calling this method for the first time causes a lot of
    planning to happen, while subsequent calls involve hardly any planning and 
    mostly just return actions determined during the offline planning stage.
  */
  virtual const action_t *next( const state_t &state );
};



/*******************************************************************************
 *
 * gourmand
 *
 ******************************************************************************/


/*
  Implements Gourmand, a solver for finite-horizon MDPs based on Reverse 
  Iterative Deepening LRTDP (called LR^2TDP). Gourmand is a further development
  of another LR^2TDP-based planner, Glutton. The main difference between the 
  two is that Gourmand in an online solver, while Glutton is an offline one.
  For more details, please refer to Kolobov, Mausam, Weld, "LRTDP vs. UCT for 
  Online Probabilistic Planning", AAAI-2012.
*/ 
class gourmand_t : public lr2tdp_extension_for_IPPC_t
{
  
  // An array holding, for each number of steps-to-go from 1 to the problem's
  // horizon, the amount of planning time allocated to a decision epoch with
  // the corresponding number of steps-to-go. E.g., it specifies the amount of
  // computation time the planner can use to decide on an action in a state 
  // with t steps-to-go.
  double *time_alloc_for_lookahead_;

  // An array holding the empirical expected amounts of time that solving 
  // a state with t steps-to-go typically takes, 1 <= t <= horizon.
  double *exp_time_for_lookahead_;

  // An array holding the numbers of samples that the estimates in 
  // exp_time_for_lookahead_ are based on.
  size_t *num_samples_;

  // The largest number of steps-to-go for which any state has been solved so
  // far.
  size_t largest_target_;

  // The parameter that determines how quickly the planner "forgets" samples
  // used to compute the estimates in numSamples_. See the 
  // update_comp_time_stats(.) method for more details.
  static const size_t SAMPLE_BUFFER_SIZE = 20;

  /*
    Reallocates the given chunk of time equally among the future decision steps.
    Suppose, for instance, that there are still R policy execution rounds 
    remaining, including the current one, and the MDP we are solving has 
    a horizon of H. Suppose also that in the current policy execution round, 
    the agent still has M steps to go. This means that there are 
    (R - 1) * H + (M - 1) decision steps to plan for, after the current one. 
    Therefore, the amount of time already allocated to each of these steps will
    increase by extraTime / ((R - 1) * H + (M - 1)) after time reallocation.

    Note that timeDelta can be negative, in which case realloc_time uniformly 
    cuts down the amount of time available to the remaining decision epochs.
  */
  void realloc_time(double timeDelta, size_t stepsRemaining);

  /*
    Updates the statistics in exp_time_for_lookahead_ for a given number of 
    steps-to-go with a new time measurement sample.
  */
  void update_comp_time_stats(double new_sample, size_t stepsRemaining);
  
public:


  gourmand_t( const problem_t &problem, hash_FH_t &hash_table );
  virtual ~gourmand_t();

  /*
    See the documentation for lr2tdp_t::reset_state().
  */
  virtual void reset_state();

  /*
    Computes an action in the given state in the same way as 
    lr2tdp_t::solve_reverse_iter(), but also collects information about
    how long it takes to solve states for different numbers of steps-to-go
    and uses this information to allocate computation time to varios decision
    epochs.
  */
  virtual void solve_reverse_iter(const state_t &state, size_t stepsRemaining );

  /*
    Returns an action to be executed in the given state. The Gourmand class 
    internally keeps track of the number of steps-to-go, and this method 
    assumes that an action is being requested for that number of steps-to-go.

    Unlike Glutton, Gourmand is an online planner. This means that, when asked
    for an action in a state s with t steps-to-go, it computes an acton for 
    this state and maybe a few others, but doesn't specifically attempt to 
    solve the entire MDP (which Glutton does).
  */
  virtual const action_t *next( const state_t &state );
};

#endif // ALGORITHMS_H
