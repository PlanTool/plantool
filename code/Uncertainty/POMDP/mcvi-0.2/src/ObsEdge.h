#ifndef __OBSEDGE_H
#define __OBSEDGE_H

#include <cmath>
#include <vector>
#include "Obs.h"
#include "Particle.h"
#include "PolicyGraph.h"
#include "Utils.h"

class Bounds;
class Simulator;

class ObsEdge {
  public:
    ObsEdge(Obs const obs, Bounds* bounds)
            : obs(obs), bounds(bounds),
              count(0),
              bestPolicyNode(NULL),
              bestPolicyVal(NegInf),
              nextBelief(NULL),
              lastUpdated(Never)
    {
        cachedParticles = new ParticleStore();
        cachedParticles->currSum = 0;
        cachedParticles->particles.clear();
    }

    ObsEdge()
            : count(0),
              bestPolicyNode(NULL),
              bestPolicyVal(NegInf),
              nextBelief(NULL),
              lastUpdated(Never)
    {
        cachedParticles = new ParticleStore();
        cachedParticles->currSum = 0;
        cachedParticles->particles.clear();
    }

    ~ObsEdge()
    {
        // probably have to switch to smart pointer (reference counting) in the future
        clearParticles();
    }

    void backup();
    void backupFromPolicyGraph();
    void backupFromNextBelief();
    void addPolicyNodes();
    void findBestInitPolicy();
    double findInitUpper();

    void addParticle(const State& state, long pathLength, double immediateReward);
    void clearParticles();
    void addNode(PolicyGraph::Node* node);

    static void initStatic(Simulator* simulator);

    // The observation that generated this ObsEdge
    Obs const obs;
    Bounds* bounds;

    double upper;
    double lower;
    // store number of particles since we will delete all particles
    // after back up
    long count;
    std::vector<PolicyGraph::Node* > nodes;
    PolicyGraph::Node* bestPolicyNode;
    double bestPolicyVal;
    Belief* nextBelief; // next child belief
    ParticleStore *cachedParticles; // particles stored, used for backup
    long lastUpdated;

    static Simulator* simulator;
};

#endif
