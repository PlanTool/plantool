/**
   This problem is different from the problem in the published paper
   "Monte Carlo Value Iteration for Continuous-State POMDPs". The
   results are not directly comparable.
*/

#ifndef __CORRIDORMODEL_H
#define __CORRIDORMODEL_H

#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include "Model.h"
#include <map>

/**
   Constants and types used in corridor model.
*/
const double Discount = 0.95;

const int NumDoors = 4;
//Position of doors, the first door is good door
//Note that door should inside the interval
// [-CorridorEndLength+CorridorEndLength, CorridorLength-CorridorEndLength]
const double DoorPositions[] = {3, -12, -5, 9};
const double DoorRadius = 1.0;
const double CorridorLength = 21;
const double CorridorEndLength = 5;
const double Noise = 0.01;
const double EnterReward = 1;
const double WrongPenalty = -2;

const double MovementCost = 0; // Cost of any other actions
const long NumStateVars = 2; // enter success, coordinates of rover
const long NumObsVars = 1;
enum directions {ActLeft, ActRight, ActEnter}; // Action names
enum observations {ObsNothing, ObsCrash, ObsDoor, ObsLeftEnd, ObsRightEnd};

const long NumActs = 3; // Number of actions

//const long TermObs = -1;
const long TermState = -1; // indicates state is at terminal state

// Same as actions but repeat action till macro states change.
const long NumMacroActs = 0;
const long NumInitPolicies = 1;

class ParticlesBelief;

/**
   @class CorridorModel
   @brief Continuous Corridor Problem
   @details ...
   @author Haoyu Bai
   @date 14 Mar 2010
*/
class CorridorModel : public Model
{
  public:
    CorridorModel(int numParticles = -1);

    double sample(State const& currState, Action const& action, State* nextState, Obs* obs, RandStream* randStream );

    //XXX No macro act
    double sample(State const& currState, Action const& macroAction, long controllerState, State* nextState, long* nextControllerState, Obs* obs, RandStream* randStream );

    /**
       Greedy policy that selects the action that moves left
    */
    double initPolicy(State const& currState, Action const& initAction, long controllerState, State* nextState, long* nextControllerState, Obs* obs, RandStream* randStream );

    State sampleInitState() const;

    /* actually do not need to be real prob, any kind of weight is ok */
    double getObsProb(Action const& action, State const& state, Obs const& obs);

    double upperBound(State const& state);

    double getMaxReward() { return EnterReward;};

    double getMinReward() { return WrongPenalty;};

    bool allowableAct(Belief const& belief, Action const& action);

    //inline long getObsGrpFromObs(Obs const& obs) { return obs[1]; };

    //XXX hmm, what TermObs mean?
    inline obsType getObsType(Obs const& obs) { return OtherObs; };

    inline void setObsType(Obs* obs, obsType type) {
        //obs[0] = type; //XXX
    } ;

    inline bool isTermState(State const& state) { return (static_cast<long>(state[0]) == TermState);};

    /**
       Output an ascii visual display of the state.
    */
    void displayState(State state, long type = 0);

  private:
    double probRandom;
    int numParticles;
};

#endif // __PACMANMODEL_H
