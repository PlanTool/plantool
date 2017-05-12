#ifndef __ACTNODE_H
#define __ACTNODE_H

#include "Action.h"
#include <map>

class ObsEdge;
class Belief;
class Bounds;

class ActNode
{
  public:
    ActNode(Action const& action, Belief const& belief, Bounds* bounds);

    /**
       Back up on this ActNode
    */
    void backup();

    void generateMacroObsPartitions();

    /**
       Sample observations and create corresponding ObsEdge.

       Each ObsEdge will have a number of particles corresponding to
       the probability of getting the observation.
    */
    void generateObsPartitions();

    /**
       Remove all the generated particles from all the ObsEdge
    */
    void clearObsPartitions();

    // The label of this ActNode
    Action const action;
    // The belief representation of the Belief father of this ActNode
    Belief const& belief;
    Bounds* bounds;
    // The average lower (upper) bounds compute by dividing the total
    // lower (upper) bounds of all ObsEdge children by the number of
    // such edges
    double avgLower, avgUpper;
    // The randSeed for generateing ObsEdge, so if we redo the
    // generation, it will be the same
    unsigned long randSeed;
    long lastUpdated;
    std::map<Obs, ObsEdge> obsChildren;
};

#endif
