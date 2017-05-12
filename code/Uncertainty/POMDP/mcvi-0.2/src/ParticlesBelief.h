#ifndef __PARTICLESBELIEF_H
#define __PARTICLESBELIEF_H

#include <vector>
#include "Belief.h"

class RandStream;
class RandSource;

/**
   @class ParticlesBelief
   @brief Set of particles representing belief - suitable for MDP but not POMDP
   @details Represent belief using a set of sampled stated. The particles are
   not weighted, so is suitble for MDP but probably not POMDP.

   @author Wee Sun Lee
   @date 22 August 2009
*/

class ParticlesBelief : public Belief
{
  public:
    ParticlesBelief(BeliefNode* beliefNode): Belief(beliefNode) {}
    ~ParticlesBelief();

    Particle sample(RandStream& randStream) const;
    Particle sample(long index, RandStream& randStream) const {
        return getParticle(index);
    }
    State average() const;
    Belief* nextBelief(const Action& action, const Obs& obs, bool useSameRandSeed) const;

    /**
       Get the particle with the given index
    */
    Particle getParticle(long index) const;

    /**
       Compute the cummulative sum for sampling using binary search
    */
    void compute_cum_sum();

    /**
       Create a belief from a single state
    */
    static Belief *beliefFromState(const State& st, const Obs& obs,  long pathLength);

    /**
       Create a belief from a set of DIFFERENT states
    */
    static Belief *beliefFromStateSet(const std::vector<State>& st, const Obs& obs, const std::vector<long>& pathLength);

    static void initStatic(RandSource* randSourceV, long numRandStreamsV, long maxMacroActLengthV);

    /**
       Compute the effective sample size
       @return ESS
       @param [in] a set of particles
    */
    static double ESS(std::vector<Particle>& sample);

    // A belief is represented by a set of particles
    std::vector<Particle> belief;
    // The cummulative sum associated with the vector of particles
    std::vector<double> cum_sum;

    static RandSource* randSource;
    static long numRandStreams;
    static long maxMacroActLength;
    static double ESSthreshold;
    // When do we need to use the binary search compared to the normal
    // getParticle
    static double approxSample;

    class ParticlesBeliefIterator:
            public BeliefItImpl
    {
      public:
        ParticlesBeliefIterator(std::vector<Particle> const* belief, int num_particles, RandStream& randStream);
        ParticlesBeliefIterator(std::vector<Particle> const* belief, int num_particles);
        ParticlesBeliefIterator(ParticlesBeliefIterator const& other);
        ~ParticlesBeliefIterator() {}

        ParticlesBeliefIterator* clone();
        void operator++();
        bool operator!=(Belief::BeliefItImpl const& right);
        Particle const& operator*();
        Particle const* getPointer();

//      private:
        std::vector<Particle> const* belief_;
        int num_particles_;
        int current_particle_;
        int cum_index_;
        double cum_weight_;
        /*const*/ double weight_interval_;
        double sample_weight_;
        Particle new_particle_;

    };

    const_iterator begin(int num_particles) const;
    const_iterator begin(int num_particles, RandStream& randStream) const;
    const_iterator end() const;
};

inline Particle ParticlesBelief::getParticle(long index) const
{
    return belief[index % belief.size()];
}

#endif //__PARTICLESBELIEF_H
