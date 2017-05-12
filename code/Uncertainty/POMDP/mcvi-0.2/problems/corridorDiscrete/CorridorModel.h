#ifndef __CORRIDORMODEL_H
#define __CORRIDORMODEL_H

#include "Model.h"


const double Discount = 0.95;

const int NumDoors = 4;
const double Noise = 0.1;
const double EnterReward = 10;
const double WrongPenalty = -2;

const double MovementCost = -0.5;
const long NumStateVars = 2;
const long NumObsVars = 1;
// The 2nd door from the left is the correct door
enum directions {ActLeft, ActRight, ActEnter};
enum observations {ObsNothing, ObsWrongDoor, ObsLeftEnd, ObsRightEnd};

const long NumActs = 3;

const long TermState = -1;

const long NumMacroActs = 0;
const long NumInitPolicies = 1;

class ParticlesBelief;

class CorridorModel : public Model
{
  public:
    CorridorModel(int numParticles = -1);

    double sample(State const& currState, Action const& action, State* nextState, Obs* obs, RandStream* randStream);

    double sample(State const& currState, Action const& macroAction, long controllerState, State* nextState, long* nextControllerState, Obs* obs, RandStream* randStream);

    double initPolicy(State const& currState, Action const& initAction, long controllerState, State* nextState, long* nextControllerState, Obs* obs, RandStream* randStream);

    State sampleInitState() const;

    double getObsProb(Action const& action, State const& state, Obs const& obs);

    double upperBound(State const& state);

    double getMaxReward() { return EnterReward; }
    double getMinReward() { return WrongPenalty; }

    bool allowableAct(Belief const& belief, Action const& action);

    inline obsType getObsType(Obs const& obs) { return OtherObs; }

    inline void setObsType(Obs* obs, obsType type) {}

    inline bool isTermState(State const& state) {
        return (static_cast<long>(state[0]) == TermState);
    }

  private:
    double probRandom;
    int numParticles;
};

#endif
