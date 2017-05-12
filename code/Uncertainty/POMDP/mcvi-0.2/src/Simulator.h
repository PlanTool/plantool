#ifndef __SIMULATOR_H
#define __SIMULATOR_H

#include "State.h"
#include "PolicyGraph.h"

class Model;
class PolicyGraph;
class RandStream;

/**
   @class Simulator
   @brief Run a single or multiple simulation using the policy graph.
   @details Runs single simulation using the policy graph and return the
   rewards or run multiple simulations and return the average reward.

   @author Wee Sun Lee
   @date 25 August 2009
*/
class Simulator
{
public:
  /**
     @param[in] model MDP model
     @param[in] policy Policy graph
     @param[in] maxMacroActLength Length of simulation
  */
  Simulator(Model& model, PolicyGraph& policy, long maxMacroActLength): model(model), policy(policy), maxMacroActLength(maxMacroActLength) {};

  /**
     Runs a single simulation from \a startState. Stores simulation trace.
     @param[in] length Simulation length
     @param[out] sumReward Sum of undiscounted reward
     @param[out] sumDiscounted Sum of discounted reward
     @param[in] trace_fn Filename to record trace
     @param[in] startState Initial state of simulation
     @param[in*] randStream Stream of random numbers
     @param[in] rootIndex Which starting state in the policy graph to use
  */
  void runSingle(long length, double* sumReward, double* sumDiscounted, std::string trace_fn, State startState,  RandStream* randStream, long rootIndex = 0);

  /**
     Runs a single simulation from \a startState.
     Does not store simulation trace.
     @param[in] length Simulation length
     @param[out] sumDiscounted Sum of discounted reward
     @param[in] startState Initial state of simulation
     @param[in] currNode starting controller node
     @param[in*] randStream Stream of random numbers
  */
  void runSingle(long length, double* sumDiscounted, State startState, PolicyGraph::Node &currNode, RandStream* randStream);

private:
  Model& model;
  PolicyGraph& policy;
  long maxMacroActLength;

};

#endif //__SIMULATOR_H
