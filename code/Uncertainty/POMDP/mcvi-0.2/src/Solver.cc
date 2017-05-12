#include "Solver.h"
#include "Obs.h"
#include "ObsEdge.h"
#include "Model.h"
#include "Belief.h"
#include "BeliefNode.h"
#include "BeliefSet.h"
#include "BeliefTree.h"
#include "PolicyGraph.h"
#include "Bounds.h"
#include "RandSource.h"
#include "Simulator.h"
#include "ParticlesBelief.h"
#include "ParticlesBeliefSet.h"

using namespace std;

void Solver::input(int argc, char **argv, int noRequiredArgs)
{
    message << "Usage:\n"
            << "  -o policyfile\n"
            << "  -p targetPrecision (default: 0.1)\n"
            << "  -t maxtime (default: 3600s)\n"
            << "  -l maxSimulLength (default: 100 steps)\n"
            << "  -c maxMacroActLength (default: 100 steps)\n"
            << "  -b numRandomStreamsForBackUp (default: 100)\n"
            << "  -n numRandomStreamsForNextBelief (default: 100)\n"
            << "  -d discountFactor (default: 0.95)\n"
            << "  -i depthMultiplier (default: 0.95)\n"
            << "  -u useMacro (default: 1)\n"
            << "  -s randNumSeed (default: 0, uses time)\n"
            << "  -v displayInterval (default: 60)\n";

    if (argc < noRequiredArgs) {
        cout << message.str() << endl;
        exit(1);
    }

    for (long i=1; i < argc; i++) {
        if (argv[i][0] != '-') {
            cout << message.str() << endl;
            exit(1);
        }
        i++;
        switch(argv[i-1][1]) {
            case 'o':
                policy_file = argv[i];
                break;
            case 't':
                maxTime = atoi(argv[i]);
                break;
            case 'l':
                maxSimulLength = atoi(argv[i]);
                break;
            case 'c':
                maxMacroActLength = atoi(argv[i]);
                break;
            case 'b':
                numBackUpStreams = atoi(argv[i]);
                break;
            case 'u':
                useMacro = atoi(argv[i]);
                break;
            case 'n':
                numNextBeliefStreams = atoi(argv[i]);
                break;
            case 'd':
                discount = atof(argv[i]);
                break;
            case 'p':
                targetPrecision = atof(argv[i]);
                break;
            case 'i':
                iterDeepMult = atof(argv[i]);
                break;
            case 's':
                seed = (unsigned) atoi(argv[i]);
                break;
            case 'v':
                displayInterval = atoi(argv[i]);
                break;
            default: break;
        }
    }
}

void Solver::solve(Model& currModel, vector<State> &initialBeliefStates, vector<long> &pathLength)
{
    Obs obs(vector<long>(currModel.getNumObsVar(),0));
    obs.obs[1] = -1;

    this->solve(currModel, initialBeliefStates, obs, pathLength);
}

void Solver::solve(Model& currModel, vector<State> &initialBeliefStates, Obs& obs, vector<long> &pathLength)
{
    ParticlesBeliefSet currSet;

    Action::initStatic(&currModel);
    Belief* root = ParticlesBelief::beliefFromStateSet(initialBeliefStates,obs,pathLength);

    this->solve(currModel, currSet, root);
}

void Solver::solve(Model& currModel, State& initialBeliefState, long pathLength)
{
    Obs obs(vector<long>(currModel.getNumObsVar(),0));
    obs.obs[1] = -1;

    this->solve(currModel, initialBeliefState, obs, pathLength);
}

void Solver::solve(Model& currModel, State& initialBeliefState, Obs& initialObs, long pathLength)
{
    ParticlesBeliefSet currSet;
    Action::initStatic(&currModel);
    Belief* root = ParticlesBelief::beliefFromState(initialBeliefState,
                                                    initialObs,
                                                    pathLength);

    this->solve(currModel, currSet, root);
}

void Solver::solve(Model& currModel, Obs& rootObs)
{
    ParticlesBeliefSet currSet;
    this->solve(currModel, currSet, rootObs);
}

void Solver::solve(Model& currModel, BeliefSet& currSet, Obs& rootObs)
{
    Action::initStatic(&currModel);
    ParticlesBelief* rootBelief = new ParticlesBelief(new BeliefNode(rootObs));
    double w = 1.0 / numNextBeliefStreams;
    for (int i = 0; i < numNextBeliefStreams; i++) {
        State st = currModel.sampleInitState();
        Particle temp(st, 0, w);
        rootBelief->belief.push_back(temp);
    }

    this->solve(currModel, currSet, rootBelief);
}

void Solver::solve(Model& currModel, BeliefSet& currSet, Belief* root)
{
    // Current code may not work if the following is not true
    if (numBackUpStreams > numNextBeliefStreams) {
        cout << "Number of next belief stream need to be larger than or equal to backup streams\n";
        exit(1);
    }

    if (seed == 0)
        srand ( time(NULL) );
    else
        srand ( seed);

    // Set random source, which stores streams of random numbers to be reused
    RandSource currRandSource(numNextBeliefStreams);

    BeliefNode::initStatic(&currModel);
    ParticlesBelief::initStatic(&currRandSource,numNextBeliefStreams,maxMacroActLength);

    PolicyGraph policyGraph(currModel, 1, currModel.getNumObsVar());
    Simulator currSim(currModel, policyGraph, maxSimulLength);

    ObsEdge::initStatic(&currSim);

    Bounds bounds(currModel, policyGraph, currRandSource, numBackUpStreams, maxSimulLength, maxMacroActLength);
    BeliefTree currTree(currModel, currSet, root, policyGraph, bounds, currRandSource, numBackUpStreams, numNextBeliefStreams);
    currTree.search(targetPrecision, maxTime, iterDeepMult, displayInterval, policy_file);

    policyGraph.write(policy_file);
}

// void Solver::initStatic(Model& model, Simulator& simulator,
//                         RandSource& randSouce,
//                         PolicyGraph& policyGraph,
//                         long maxSimulLength,
//                         long numBackUpStreams,
//                         logn maxMacroActLength)
// {
//     Model &CommonParams::model = model;
//     Simulator &CommonParams::simulator = simulator;
//     RandSource &CommonParams::randSource = currRandSource;
//     PolicyGraph &CommonParams::policyGraph = policyGraph;
//     const long CommonParams::maxSimulLength = maxSimulLength;
//     const long CommonParams::numBackUpStreams = numBackUpStreams;
//     const long CommonParams::maxMacroActLength = maxMacroActLength;
// }
