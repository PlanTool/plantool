#ifndef __SOLVER_H
#define __SOLVER_H

#include <cstdlib>
#include <ctime>
#include <iostream>
#include <sstream>
#include "State.h"

class Obs;
class Model;
class Belief;
class BeliefSet;

class Solver {
  public:
    Solver(): maxTime(3600), discount(0.95), targetPrecision(0.1),
              numBackUpStreams(100), numNextBeliefStreams(100),
              maxSimulLength(100), maxMacroActLength(100),
              iterDeepMult(0.95),
              useMacro(1), seed(0), displayInterval(60)
    {}

    void input(int argc, char **argv, int noRequiredArgs);

    /**
       @param[in] currModel The model
       @param[in] initialBeliefStates Initial states
       @param[in] pathLength Pathlength of each state

       Assume the initial observation is {0, -1, 0...}.
       Assume a uniform distribution on the initialBeliefStates. Doesn't need to implement the sampleInitState function in the Model.
    */
    void solve(Model& currModel, std::vector<State> &initialBeliefStates, std::vector<long> &pathLength);

    /**
       @param[in] currModel The model
       @param[in] initialBeliefStates Initial states
       @param[in] obs The initial observation
       @param[in] pathLength Pathlength of each state

       Assume a uniform distribution on the initialBeliefStates. Doesn't need to implement the sampleInitState function in the Model.
    */
    void solve(Model& currModel, std::vector<State> &initialBeliefStates, Obs& obs, std::vector<long> &pathLength);

    /**
       @param[in] currModel The model
       @param[in] initialBeliefStates Initial states
       @param[in] pathLength Pathlength of all states

       Assume the initial observation is {0, -1, 0...}.
       Assume a uniform distribution on the initialBeliefStates. Doesn't need to implement the sampleInitState function in the Model.
    */
    void solve(Model& currModel, State& initialBeliefState, long pathLength);

    /**
       @param[in] currModel The model
       @param[in] initialBeliefStates Initial states
       @param[in] obs The initial observation
       @param[in] pathLength Pathlength of all states

       Assume a uniform distribution on the initialBeliefStates. Doesn't need to implement the sampleInitState function in the Model.
    */
    void solve(Model& currModel, State& initialBeliefState, Obs& obs, long pathLength);

    /**
       @param[in] currModel The model
       @param[in] initialObs The initial observation

       Assume the BeliefSet is empty.
       Need to implement the sampleInitState in the Model.
    */
    void solve(Model& currModel, Obs& initialObs);

    /**
       @param[in] currModel The model
       @param[in] currSet The initial BeliefSet
       @param[in] initialObs The initial observation

       Need to implement the sampleInitState in the Model.
    */
    void solve(Model& currModel, BeliefSet& currSet, Obs& initialObs);

    /**
       @param[in] currModel The model
       @param[in] currSet The initial BeliefSet
       @param[in] root The initial belief

       Need to implement the sampleInitState in the Model.
    */
    void solve(Model& currModel, BeliefSet& currSet, Belief* root);

    std::ostringstream message;
    std::string policy_file;
    unsigned maxTime;
    double discount;
    double targetPrecision;
    long numBackUpStreams;
    long numNextBeliefStreams;
    long maxSimulLength;
    long maxMacroActLength;
    double iterDeepMult;
    long useMacro;
    unsigned seed;
    long displayInterval;
};

#endif
