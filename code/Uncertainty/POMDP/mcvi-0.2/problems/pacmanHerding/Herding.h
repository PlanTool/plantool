#ifndef __HERDING_H
#define __HERDING_H

#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include "Obs.h"
#include "Model.h"
#include "HerdingProblem.h"
#include <map>

/**
   @class Herding
   @brief Collaborative Search and Capture (NIPS 2011)
   @author Wee Sun Lee
   @date 23 Sept 2009
*/
class Herding : public Model
{
  public:
    Herding(HerdingProblem const& problem, bool useMacro = false);

    void setInitialBelief(Belief* root);

    double sample(State const& currState, Action const& action, State* nextState, Obs* obs, RandStream* randStream );

    double sample(State const& currState, Action const& macroAct, long controllerState, State* nextState, long* nextControllerState, Obs* obs, RandStream* randStream );

    /**
       Greedy policy
    */
    double initPolicy(State const& currState, Action const& initAction, long controllerState, State* nextState, long* nextControllerState, Obs* obs, RandStream* randStream );

    double upperBound(State const& state);

    double getMaxReward() { return Caught;};

    double getMinReward() { return MovementCost;};

    bool allowableAct(Belief const& belief, Action const& action);

    double getObsProb(Action const& act, State const& nextState, Obs const& obs);

    double beliefTransition(State const& currState, Action* action, State* nextState, Obs* obs){return 0.0;}
    /**
       Macro state computed by
    */
    long getObsGrpFromState(State const& state);

    inline long getObsGrpFromObs(Obs const& obs) { return obs.obs[1]; }

    inline obsType getObsType(Obs const& obs) { return (obs.obs[0]==LoopObs? LoopObs : (obs.obs[0]==TermObs? TermObs: OtherObs)) ; }

    inline void setObsType(Obs* obs, obsType type) { obs->obs[0] = type; }

    inline bool isTermState(State const& state) { return (state[0] == TermState);}

    /**
       Reads in problem parameters from file.
       @warning No error checking currently done - file format need to be exactly right.
       - Line 1: number of observation variables
       - Line 2: prob of random move
       - Line 3: num of ghosts
       - Line 4: width then height of grid
       - Line 5: x then y initial coordinate of agent 1
       - Line 6: x then y initial coordinate of agent 2
       - Line 7 to 7 + num of ghosts: x then y initial coordinate of ghost
       - After: grid of the world. Each grid location is an integer
       separated by space. Wall is represented by 0. Each accessible location
       is represented by a positive integer indicating its region. Each
       intersection must form its own region. The width of each region is one
       but length is not limited. The two ends of a region must be terminated
       with intersections. The entire grid must be surrounded by walls. The
       bottom left of the grid is (0,0) while the top right is
       (width-1, height-1).
    */
    static void readProblem(std::string filename, HerdingProblem* problem);

    /**
       Output an ascii visual display of the state.
    */
    void displayState(State state, long type);

    /**
       Write the macrostate group mapping
    */
    void writeMapping(std::string filename);

    /**
       Read the macrostate group mapping
    */
    void readMapping(std::string filename);

  private:
    bool useMacro;
    long xSize, ySize; // horizontal and vertical dimensions of the world
    long numRegionPerAgent;
    long numGhosts;
    long numStateVars;
    double probRandom;
    std::vector<std::vector <long> > grid; // raster scanned grid
    std::vector<regionType> rType; // junction or not
    std::vector<std::vector<long> > constraints;
    std::vector<long> ghostMStateMap;
    long ghostMStateCt;
    long agentMStateCt;

    // vector indexed by macrostate grp containing
    // macrostate grp on the east, south, west, north of a macro state grp
    std::vector<std::vector <long> > connectivity;

    // Find allowable actions in each macrostate
    void findConstraints(std::vector<std::vector<long> >* constraints);

    // for computing shortest path
    std::vector<std::vector <long> > gridNodeLabel; // label of accessible locs
    long numAccessibleLocs; // how many accessible locations in the grid
    std::vector<std::vector <long> > shortestPath; // shortest path matrix
    std::vector<bool> found; // storing whether node seen before in BFS
    // discounted reward as a function of number of steps
    std::vector<double> discountedReward;
    void calcShortestPath(long i, long j); // single source shortest path
    void calcShortestPathMatrix(); // compute all pairs shortest path
    long computeGhostIndex(State const& state);
};

#endif // __HERDING_H
