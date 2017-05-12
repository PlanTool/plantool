#include "CorridorModel.h"
#include "Include.h"
#include "ParticlesBelief.h"
#include <cassert>
#include <cmath>
#include <cstdlib>
#include <deque>
#include <iostream>

using namespace std;

double uniform_deviate(int seed) {
    return seed * (1.0 / (RAND_MAX + 1.0) );
}

int rand_range(int low, int high) {
    return low + uniform_deviate(rand()) * (high-low);
}

#define NOISY(v) ((v) ^ (randStream->getf() < Noise))

CorridorModel::CorridorModel(int numParticles):
        Model(NumStateVars, NumObsVars, NumActs, NumMacroActs, NumInitPolicies, Discount),
        numParticles(numParticles)
{}

bool CorridorModel::allowableAct(Belief const& belief, Action const& action) {
    if (action.type == Macro) return false;

    return true;
}

double CorridorModel::sample(State const& currState, Action const& action, State* nextState, Obs* obs, RandStream* randStream) {
    bool debug = false;

    double reward = MovementCost;
    if (currState[0] < 0) {
        obs->obs[0] = TermObs;
        *nextState = currState;
        // if (action.getActNumUser() == ActEnter)
        // return WrongPenalty;
        // else return -1000;
        return -1000;
    }

    if (action.getActNumUser() == ActEnter) {
        if (currState[1] == 1) {
            (*nextState)[0] = TermState;
            (*nextState)[1] = currState[1];
            obs->obs[0] = TermObs;
            reward = EnterReward;
        } else {
            *nextState = currState;
            obs->obs[0] = ObsWrongDoor;
            reward = WrongPenalty;
        }
    } else {
        bool moveDir = (action.getActNumUser() == ActLeft);
        long nxtIndex;

        if NOISY(moveDir) nxtIndex = -1;
        else nxtIndex = 1;
        // if (moveDir) nxtIndex = -1;
        // else nxtIndex = 1;

        long nxtPos = currState[1] + nxtIndex;
        if (nxtPos < 0 || nxtPos > 3) nxtPos -= nxtPos / abs(nxtPos);

        obs->obs[0] = ObsNothing;
        if (nxtPos == 0) {
            obs->obs[0] = ObsLeftEnd;
        } else if (nxtPos == 3) {
            obs->obs[0] = ObsRightEnd;
        }

        (*nextState)[1] = nxtPos;
        (*nextState)[0] = 0;
    }

    if (debug) {
        cout<<"Model::sample\n";
        cout<<currState[1]<<" "<<action.getActNumUser()<<" "<<(*nextState)[1]<<" "<<obs->obs[0]<<"\n";
        cout<<"Leaving Model::sample\n";
    }

    return reward;
}

double CorridorModel::sample(State const& currState, Action const& macroAct, long controllerState, State* nextState, long* nextControllerState, Obs* obs, RandStream* randStream) {
    assert(false);
    return 0;
}

double CorridorModel::initPolicy(State const& currState, Action const& initAction, long controllerState, State* nextState, long* nextControllerState, Obs* dummy, RandStream* randStream) {
    Obs obs(vector<long>(getNumObsVar(), 0));
    if (currState[0] > 0) {
        *nextState = currState;
        return 0;
    }

    // move left
    return sample(currState, Action(Act,ActLeft), nextState, &obs, randStream);
}

State CorridorModel::sampleInitState() const {
    double p = 3;
    State st(getNumStateVar(), 0);
    st[1] = p;
    return st;
}

double CorridorModel::upperBound(State const& state) {
    double minSteps = fabs(state[1] - 1);
    double reward = 0, coef = 1;
    for (int i=1; i<minSteps; i++) {
        reward += coef * MovementCost;
        coef *= discount;
    }
    reward += coef * EnterReward;
    return reward;
}

double CorridorModel::getObsProb(Action const& action, State const& nextState, Obs const& obs) {
    bool debug = false;

    double nxtPos = nextState[1];

    if (debug) {
        cout<<"ActNum "<<action.getActNumUser()<<"\n";
        cout<<"State "<<nextState[1]<<"\n";
        cout<<"Obs "<<obs.obs[0]<<"\n";
    }

    if (obs.obs[0] == TermObs) {
        if (nextState[0] == TermState) return 1.0;
        else return 0.0;
    } else if (obs.obs[0] == ObsLeftEnd) {
        if (nxtPos == 0) return 1.0;
        else return 0.0;
    } else if (obs.obs[0] == ObsRightEnd) {
        if (nxtPos == 3) return 1.0;
        else return 0.0;
    } else if (obs.obs[0] == ObsWrongDoor) {
        if (nxtPos != 1) return 1.0;
        else return 0.0;
    } else {
        //ObsNothing
        if (nxtPos == 0 || nxtPos == 3) return 0.0;
        return 1.0;
    }
}
