#ifndef __BELIEFTREE_H
#define __BELIEFTREE_H

#include <map>
#include <string>
#include <vector>

class Obs;
class ObsEdge;
class Model;
class Belief;
class BeliefSet;
class PolicyGraph;
class RandSource;
class Bounds;

/**
   @class BeliefTree
   @brief Belief tree construction and search
   @details Routines for searching the belief tree and constructing the policy
   graph.

   @author Wee Sun Lee
   @date 26 October 2009
*/
class BeliefTree
{
  public:
    BeliefTree(Model& model, BeliefSet& beliefSet, Belief *root, PolicyGraph& policyGraph, Bounds& bounds, RandSource& randSource, long numRandBackupStreams, long numRandBeliefStreams);
    ~BeliefTree() {}

    void search(double targetGap, unsigned maxTime, double targetMultiplier, long displayInterval, std::string policy_filename);

  private:
    Model& model;
    BeliefSet& beliefSet;
    Belief *root;
    PolicyGraph& policyGraph;
    Bounds& bounds;
    RandSource& randSource;
    long numRandBackupStreams;
    long numRandBeliefStreams;

    std::vector<Belief *> beliefStack;

    void expandNodes(double target);
    std::map<Obs,ObsEdge>::iterator findBestObs(Belief *currNode, double target, double& excessUncertainty);
    void backUpNodes();
};

#endif // __BELIEFTREE_H
