#ifndef __UNDERWATERMODEL_H
#define __UNDERWATERMODEL_H

#include <iostream>
#include <fstream>
#include <map>
#include <utility>
#include <string>
#include <vector>
#include "Obs.h"
#include "Model.h"
#include "UnderwaterProblem.h"
#include <map>

/**
   Constants and types used in the model. Take care in making any changes
   as they affect most of the pacman code.
*/
const double TerminalReward = 10000; //reach the goal
const double HitRockPenalty = -2000; //penalty for hitting a rock
const double MovePenalty = -1; //penalty for direct movement
const double DiagMovePenalty = -1.414; //penalty for diagnol movement

const long ObsOutView = -1; //null observation
const long NumObsVars = 3; // only observe x, y coordinate
const long NumMacroActions = 6; //n, s, e, ne, se, move to goal
const long NumActions = 5; //n, s, e, ne, se
const long NumInitPolicies = 1;
enum actions {n, s, e, ne, se, stay}; // Action names
enum stateVar {rx, ry, local};
const long RelativeDirX[] = {0, 0, 1, 1, 1, 0}; // Relative x movement for each aciton
const long RelativeDirY[] = {-1, 1, 0, -1, 1, 0}; // Relative y movement for each aciton

//const long TermState = LONG_MIN; // indicates state is at terminal state


/**
   @class UnderwaterModel
   @brief Underwater navigation example
   @author Lim Zhan Wei
   @date 30 Nov 2010
*/
class UnderwaterModel : public Model
{
  public:
    UnderwaterModel(UnderwaterProblem const& problem, bool useMacro = true);

    double sample(State const& currState, Action const& act, State* nextState, Obs* obs, RandStream* randStream );

    double sample(State const& currState, Action const& macroAct, long controllerState, State* nextState, long* nextControllerState, Obs* obs, RandStream* randStream );

    double initPolicy(State const& currState, Action const& act, long controllerState, State* nextState, long* nextControllerState, Obs* obs, RandStream* randStream );

    double upperBound(State const& state);

    double getMaxReward() { return TerminalReward;}

    //return the probability of receiving observation, given next state and action
    double getObsProb(Action const& act, State const& nextstate, Obs const& obs);

    //return the next belief vector (via nextState) given current belief, obs and action
    double beliefTransition(State const& currState, long act, State* nextState, Obs* obs);

    double getMinReward() { return HitRockPenalty;}

    bool allowableAct(Belief const& belief, Action const& action );

    long getObsGrpFromState(State const& state) {return 0;}

    inline long getObsGrpFromObs(Obs const& obs) {return 0;}

    inline obsType getObsType(Obs const& obs) { return (obs.obs[0]==LoopObs? LoopObs : (obs.obs[0]==TermObs? TermObs: OtherObs)) ; }

    inline void setObsType(Obs* obs, obsType type) { obs->obs[0] = type; }

    //get approximate flat state representation from current state
    State getFlatState(State const& state);

    inline bool isTermState(State const& state) { return state[rx] == -1;}
    /**
       Reads in problem parameters from file.
       @warning No error checking currently done - file format need to be exactly right.
       - Line 1: width then height of grid
       - Line 2: number of rocks
       - Line 3: discount factor
       - Line 4 to 4+number of rocks: x then y coordinate of rock
       - Line 4 + number of rocks + 1: x then y coordinate of initial rover position
    */
    static void readProblem(std::string filename, UnderwaterProblem* problem);


  private:
    bool useMacro;
    long xSize, ySize; // horizontal and vertical dimensions of the world
    UnderwaterProblem const& problem;

    std::vector< std::vector<char> > navMap;

    long numStates;
    double power(double x, long i);
    std::pair<long, long> findNearestGoal(long x, long y);
    std::pair<long, long> stateToCoord(long stateNo){return std::make_pair(stateNo/xSize, stateNo % xSize);}
    long coordToState(std::pair<long, long> coord){return coord.second*xSize + coord.first;}

    Belief* root;
};

#endif // __UNDERWATERMODEL_H
