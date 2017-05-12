#include <cassert>
#include <cmath>
#include <cstdlib>
#include <deque>
#include "CorridorModel.h"
#include "Include.h"
#include "ParticlesBelief.h"
#include "Utils.h"

#include <iostream>

using namespace std;

int indoor(double pos) {
    int i=0;
    for(i=0;i<NumDoors;++i)
        if(fabs(DoorPositions[i]-pos)<=DoorRadius)
            return i;
    return -1;
}

inline vector<double> sameValueVector(int size, double value)
{
    vector<double> result;
    for (int i = 0; i < size; ++i)
        result.push_back(value);

    return result;
}

void CorridorModel::initRewardModel()
{
    vector<Gauss> left;
    left.push_back(Gauss(-21,1));
    left.push_back(Gauss(-19,1));
    left.push_back(Gauss(-17,1));
    GaussMixture move_left = GaussMixture(left, sameValueVector(3,-2));

    vector<Gauss> right;
    right.push_back(Gauss(21,1));
    right.push_back(Gauss(19,1));
    right.push_back(Gauss(17,1));
    GaussMixture move_right = GaussMixture(right, sameValueVector(3,-2));

    vector<Gauss> door;
    vector<double> door_weights;
    door_weights.push_back(-10);
    door_weights.push_back(2);
    door_weights.push_back(-10);
    door.push_back(Gauss(-25,250));
    door.push_back(Gauss(3,3));
    door.push_back(Gauss(25,250));
    GaussMixture enter = GaussMixture(door, door_weights);

    RewardModel[0] = move_left;
    RewardModel[1] = move_right;
    RewardModel[2] = enter;

    MaxReward = enter.evaluate(3) / (1 - getDiscount());
    MinReward = move_left.evaluate(-19) / (1 - getDiscount());
}

void CorridorModel::initObsModel()
{
    vector<GaussMixture> gauss_mixture;

    double variance = 1.6;
    // left end
    vector<Gauss> left;
    left.push_back(Gauss(-21, variance));
    left.push_back(Gauss(-19, variance));
    left.push_back(Gauss(-17, variance));
    left.push_back(Gauss(-15, variance));
    left.push_back(Gauss(-13, variance));
    gauss_mixture.push_back(GaussMixture(left, sameValueVector(5,1)));

    // right end
    vector<Gauss> right;
    right.push_back(Gauss(21, variance));
    right.push_back(Gauss(19, variance));
    right.push_back(Gauss(17, variance));
    right.push_back(Gauss(15, variance));
    right.push_back(Gauss(13, variance));
    gauss_mixture.push_back(GaussMixture(right, sameValueVector(5,1)));

    // door
    vector<Gauss> door;
    door.push_back(Gauss(-11, variance));
    door.push_back(Gauss(-5, variance));
    door.push_back(Gauss(3, variance));
    door.push_back(Gauss(9, variance));
    gauss_mixture.push_back(GaussMixture(door, sameValueVector(4,1)));

    // corridor
    vector<Gauss> corridor;
    corridor.push_back(Gauss(-9, variance));
    corridor.push_back(Gauss(-7, variance));
    corridor.push_back(Gauss(-3, variance));
    corridor.push_back(Gauss(-1, variance));
    corridor.push_back(Gauss(1, variance));
    corridor.push_back(Gauss(5, variance));
    corridor.push_back(Gauss(7, variance));
    corridor.push_back(Gauss(11, variance));
    gauss_mixture.push_back(GaussMixture(corridor, sameValueVector(8,1)));

    ObsModel.setMixture(gauss_mixture, sameValueVector(4,2));
}

void CorridorModel::initInitState()
{
    vector<Gauss> gauss;
    gauss.push_back(Gauss(-15,30));
    gauss.push_back(Gauss(-5,30));
    gauss.push_back(Gauss(5,30));
    gauss.push_back(Gauss(15,30));
    InitState = GaussMixture(gauss, sameValueVector(4,1));
}

CorridorModel::CorridorModel(int numParticles):
        Model(NumStateVars, NumObsVars, NumActs,
              NumMacroActs, NumInitPolicies, Discount),
        numParticles(numParticles)
{
    MovementModel[0] = Gauss(-ActionScale,MovementVariance);
    MovementModel[1] = Gauss(ActionScale,MovementVariance);
    MovementModel[2] = Gauss(0,MovementVariance);

    initRewardModel();
    initObsModel();

    initInitState();
}

bool CorridorModel::allowableAct(Belief const& belief, Action const& action)
{
    if (action.type == Macro) return false;

    return true;
}

double CorridorModel::sample(State const& currState, Action const& action, State* nextState, Obs* obs, RandStream* randStream  )
{
    int act_index = action.getActNumUser();

    double reward = MovementCost;
    double pos = currState[1];

    if (act_index == ActEnter) {
        if (RewardModel[act_index].evaluate(currState[1]) >= 0) {
            (*nextState)[0] = 0;
            (*nextState)[1] = currState[1];
        } else {
            (*nextState) = currState;
            (*nextState)[1] += MovementModel[act_index].sample(randStream);
            obs->obs[0] = ObsModel.sample((*nextState)[1], randStream);
        }
    } else {
        assert(act_index == ActLeft || act_index == ActRight);
        double nxtPos;

        nxtPos = pos + MovementModel[act_index].sample(randStream);

        if (nxtPos<-CorridorLength) {
            // left end
            nxtPos = -CorridorLength;
        } else if (nxtPos>CorridorLength) {
            // right end
            nxtPos = CorridorLength;
        }

        (*nextState)[1] = nxtPos;
        (*nextState)[0] = 0;

        obs->obs[0] = ObsModel.sample(nxtPos, randStream);
    }

    assert(obs->obs[0] < 4);

    return reward + RewardModel[act_index].evaluate(currState[1]);
}

double CorridorModel::sample(State const& currState, Action const& macroAction, long controllerState, State* nextState, long* nextControllerState, Obs* obs, RandStream* randStream)
{
    // should never reach here
    assert(false);
    return 0;
}


double CorridorModel::initPolicy(State const& currState, Action const& initAction, long controllerState, State* nextState, long* nextControllerState, Obs* dummy, RandStream* randStream)
{
    Obs obs(vector<long>(getNumObsVar(), 0));
    if (currState[0] < 0){
        (*nextState) = currState;
        return 0;
    };

    return sample(currState, Action(Act,ActEnter), nextState, &obs, randStream);
}

State CorridorModel::sampleInitState() const
{
    State st(getNumStateVar(), 0);
    st[1] = InitState.sample();
    return st;
}

/* Should be a tight upper bound, right? */
double CorridorModel::upperBound(State const& state)
{
    double minSteps = fabs(state[1] - DoorPositions[0]);
    double reward = pow(discount, minSteps) * MaxReward;
    return reward;
}

double CorridorModel::getObsProb(Action const& action, State const& nextState, Obs const& obs)
{
    assert(0 <= obs.obs[0] && obs.obs[0] < 4);
    return ObsModel.gauss_mixture[obs.obs[0]].evaluate(nextState[1]);
}

void CorridorModel::displayState(State state, long type)
{
    cout << state[1] << endl;
}
