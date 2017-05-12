#ifndef __PARTICLESBELIEFSET_H
#define __PARTICLESBELIEFSET_H

#include <set>
#include "BeliefSet.h"

class ParticlesBelief;

/**
   @class ParticlesBeliefSet
   @brief Set of beliefs where each belief is an unweighted particles representation of belief, suitable for MDP
   @details Stores beliefs that are represented as vector of particles. Also
   generate next belief given current belief, action and observation.

   @warning nextBelief may end up with an empty vector. Current implementation
   only calls it on action and observations that does not end up with
   empty vector.

   @author Wee Sun Lee
   @date 27 August 2009
*/
class ParticlesBeliefSet: public BeliefSet
{
  public:

    /**
       @param[in] model Model of the world
       @param[in] randSource Source of random number
       @param[in] numRandStreams How many initial starting points to sample
       from the current belief
       @param[in] maxMacroActLength Maximum simulation length allowed for a
       macro action. For robustness.
    */
    ParticlesBeliefSet();
    ~ParticlesBeliefSet();

    long numBeliefs() { return pBSet.size(); };

    std::pair<Belief*, bool> insert(Belief *belief);

    std::set<ParticlesBelief*, beliefComp> pBSet;
};

#endif //__PARTICLESBELIEFSET_H
