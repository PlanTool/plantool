#include "Herding.h"
#include "HerdingProblem.h"
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
#include <cmath>

using namespace std;

int main(int argc, char **argv)
{
    ostringstream message;
    double discount = 0.95;
    long maxSimulLength = 100;
    long numTrials = 1;
    unsigned seed=0;
    long display = 0;

    message << "Usage:\n"
            << "  -m mapfile\n"
            << "  -o policyfile\n"
            << "  -g macrostategroupfile\n"
            << "  -l maxSimulLength (default: 100 steps)\n"
            << "  -d discountFactor (default: 0.95)\n"
            << "  -n numTrials (default: 1)\n"
            << "  -x display (1 for show, 0 for not, -1 for csv, default: 0)\n"
            << "  -s randNumSeed (default: 0, uses time)\n";

    if (argc == 1){
        cout << message.str() << endl;
        exit(1);
    }

    string map_file, policy_file, macroStateGroup_file;
    for (long i=1; i<argc; i++) {
        if (argv[i][0] != '-') {
            cout << message.str() << endl;
            exit(1);
        }
        i++;
        switch(argv[i-1][1]) {
            case 'm':
                map_file = argv[i];
                break;
            case 'o':
                policy_file = argv[i];
                break;
            case 'g':
                macroStateGroup_file = argv[i];
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
            case 'd':
                discount = atof(argv[i]);
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

    HerdingProblem currProblem;
    currProblem.discount = discount;

    Herding::readProblem(map_file, &currProblem);

    Herding currModel(currProblem);
    currModel.readMapping(macroStateGroup_file);

    RandSource currRandSource(numTrials);

    RandStream randStream;
    randStream.initseed(currRandSource.get());

    PolicyGraph policyGraph(currModel, 1, currModel.getNumObsVar());
    policyGraph.read(policy_file);

    Simulator currSim(currModel, policyGraph, maxSimulLength);
    double avgReward, avgDiscounted;
    // vector<State > trace;
    // vector<PolicyGraph::TypeActPair> actTrace;
    string str = "tracefile";

    if (numTrials == 1){
        currSim.runSingle(maxSimulLength, &avgReward, &avgDiscounted, str, currProblem.initState, &randStream);
        if (display != -1)
            cout << "Average Reward: " << avgReward << "   Average Discounted Reward: " << avgDiscounted << "\n";

        if (display == 1) cin.get();

        // for (long i=0; i< trace.size(); i++){
        //   if (display == 1) system("clear");
        //   currModel.displayState(trace[i],display);
        //   if (display == 1) cin.get();
        // }

    }else{
        double sumReward = 0;
        double sumDiscounted = 0;
        vector<double> rewardRecord;
        vector<double> discountedRecord;

        for (long i= 0; i<numTrials; i++){
            currSim.runSingle(maxSimulLength, &avgReward, &avgDiscounted, str, currProblem.initState, &randStream);
            sumReward += avgReward;
            sumDiscounted += avgDiscounted;
            rewardRecord.push_back(avgReward);
            discountedRecord.push_back(avgDiscounted);
        }

        double avgReward  = sumReward/numTrials;
        double avgDiscounted = sumDiscounted/numTrials;

        double totalVarRew = 0;
        double totalVarDiscounted = 0;

        for (long i= 0; i<numTrials; i++){
            double currVarRew = pow(rewardRecord[i] - avgReward, 2);
            totalVarRew += currVarRew / numTrials;
            double currVarDiscounted = pow(discountedRecord[i] - avgDiscounted, 2);
            totalVarDiscounted += currVarDiscounted / numTrials;
        }
        totalVarRew = sqrt(totalVarRew);
        double confIntervalRew = 1.96 * totalVarRew / sqrt((double) numTrials);
        totalVarDiscounted = sqrt(totalVarDiscounted);
        double confIntervalDiscounted= 1.96 * totalVarDiscounted / sqrt((double) numTrials);

        cout << "Average Reward: " << avgReward << " +/-" << confIntervalRew <<" Average Discounted Reward: " << avgDiscounted <<" +/-"<< confIntervalDiscounted << endl;


        /*
          double sumReward = 0;
          double sumDiscounted = 0;
          for (long i= 0; i<numTrials; i++){
          currSim.runSingle(maxSimulLength, avgReward, avgDiscounted, currProblem.initState, trace, actTrace, false, currRandSource);
          sumReward += avgReward;
          sumDiscounted += avgDiscounted;
          if (display == 1)
          if (avgReward != currProblem.numGhosts)
	  cout << "[Fail to get all] Average Reward: " << avgReward << "   Average Discounted: " << avgDiscounted << "\n";
          }

          cout << "Average Reward: " << sumReward/numTrials << "   Average Discounted Reward: " << sumDiscounted/numTrials << "\n";*/
    }

}
