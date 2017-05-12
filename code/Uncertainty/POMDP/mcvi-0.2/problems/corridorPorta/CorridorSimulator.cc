#include "CorridorModel.h"
#include "BeliefTree.h"
#include "ParticlesBeliefSet.h"
#include "RandSource.h"
#include "PolicyGraph.h"
#include "Bounds.h"
#include "Simulator.h"
#include <sstream>
#include <iostream>
#include <cstdlib>
#include <ctime>

using namespace std;

int main(int argc, char **argv)
{
    ostringstream message;
    long maxSimulLength = 100;
    long numTrials = 1;
    unsigned seed=0;
    long display = 0;

    message << "Usage:\n"
            << "  -o policyfile\n"
            << "  -l maxSimulLength (default: 100 steps)\n"
            << "  -n numTrials (default: 1)\n"
            << "  -x display (1 for show, 0 for not, -1 for csv, default: 0)\n"
            << "  -s randNumSeed (default: 0, uses time)\n";

    if (argc == 1){
        cout << message.str() << endl;
        exit(1);
    }

    string policy_file;
    for (long i=1; i<argc; i++) {
        if (argv[i][0] != '-') {
            cout << message.str() << endl;
            exit(1);
        }
        i++;
        switch(argv[i-1][1]) {
        case 'o':
            policy_file = argv[i];
            break;
        case 'l':
            maxSimulLength = atoi(argv[i]);
            break;
        case 'n':
            numTrials = atoi(argv[i]);
            break;
        case 'x':
            display = atoi(argv[i]);
            break;
        case 's':
            seed = (unsigned) atoi(argv[i]);
            break;
        default:
            cout << message.str() << endl;
            exit(1);
        }
    }

    if (seed == 0)
        srand ( time(NULL) );
    else
        srand ( seed);

    CorridorModel currModel;

    RandSource currRandSource(numTrials);

    RandStream randStream;
    randStream.initseed(currRandSource.get());

    PolicyGraph policyGraph(currModel, 1, currModel.getNumObsVar());
    policyGraph.read(policy_file);

    Simulator currSim(currModel, policyGraph, maxSimulLength);
    double avgReward, avgDiscounted;
    vector<State > trace;
    if (numTrials == 1){
        currSim.runSingle(maxSimulLength, &avgReward, &avgDiscounted, "trace.out", currModel.sampleInitState(), &randStream);
        if (display != -1)
            cout << "Average Reward: " << avgReward << "   Average Discounted Reward: " << avgDiscounted << "\n";

        if (display == 1) cin.get();
    }else{
        double sumReward = 0;
        double sumDiscounted = 0;
        for (long i= 0; i<numTrials; i++){
            currSim.runSingle(maxSimulLength, &avgReward, &avgDiscounted, "trace.out", currModel.sampleInitState(), &randStream);
            sumReward += avgReward;
            sumDiscounted += avgDiscounted;
        }
        cout << "Average Reward: " << sumReward/numTrials << "   Average Discounted Reward: " << sumDiscounted/numTrials << "\n";
    }

};
