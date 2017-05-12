#ifndef __BELIEFFOREST_H
#define __BELIEFFOREST_H

#include <map>
#include <queue>
#include "BeliefSet.h"

class Obs;
class ObsEdge;
class Model;
class Belief;
class PolicyGraph;
class RandSource;
class Bounds;

/**
   @class BeliefForest
   @brief Belief forest construction and search
   @details Routines for searching a belief forest and constructing a
   joint policy graph for the forest.

   @author Wee Sun Lee
   @date 26 October 2009
*/
class BeliefForest
{
  public:
    BeliefForest(Model& model, BeliefSet& beliefSet, std::vector<Belief *>& roots, PolicyGraph& policyGraph, Bounds& bounds, RandSource& randSource, long numRandBackupStreams, long numRandBeliefStreams);

    void search(double targetGap, unsigned maxTime, double targetMultiplier, long displayInterval);

  private:
    Model& model;
    BeliefSet& beliefSet;
    std::vector<Belief *> roots;
    PolicyGraph& policyGraph;
    Bounds& bounds;
    RandSource& randSource;
    long numRandBackupStreams;
    long numRandBeliefStreams;

    std::priority_queue<std::pair<double,long> > rootQueue;
    std::vector<Belief *> beliefStack;
    std::vector<long> counts;

    void expandNodes(double target, long currRoot);
    std::map<Obs,ObsEdge>::iterator findBestObs(Belief *currNode, double target, double& excessUncertainty);
    void backUpNodes(long currRoot);
};

#endif // __BELIEFFOREST_H
