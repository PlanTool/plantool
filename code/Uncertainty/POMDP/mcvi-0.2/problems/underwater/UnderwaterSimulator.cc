#include "UnderwaterModel.h"
#include "UnderwaterProblem.h"
#include "BeliefTree.h"
#include "ParticlesBeliefSet.h"
#include "RandSource.h"
#include "PolicyGraph.h"
#include "Bounds.h"
#include "Simulator.h"
#include <sstream>
#include <iostream>
#include <cstdlib>
#include <cmath>
#include <ctime>

using namespace std;

int main(int argc, char **argv)
{
    ostringstream message;
    double discount = 0.95;
    long maxSimulLength = 100;
    long numTestStreams = 100;
    long numTrials = 1;
    unsigned seed=0;
    unsigned simpleOutput = 0;
    long display = 0;

    message << "Usage:\n"
            << "  -m mapfile\n"
            << "  -o policyfile\n"
            << "  -l maxSimulLength (default: 100 steps)\n"
            << "  -t numTestStreams (default: 100 steps)\n"
            << "  -d discountFactor (default: 0.95)\n"
            << "  -n numTrials (default: 1)\n"
            << "  -x display (1 for show, 0 for not, -1 for csv, default: 0)\n"
            << "  -p output result in two column format (discounted reward, average reward), for simple graph plotting\n"
            << "  -s randNumSeed (default: 0, uses time)\n";

    if (argc == 1){
	cout << message.str() << endl;
	exit(1);
    }

    string map_file, policy_file;
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
            case 'l':
                maxSimulLength = atoi(argv[i]);
                break;
            case 't':
                numTestStreams = atoi(argv[i]);
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
            case 'p':
                simpleOutput = (unsigned) atoi(argv[i]);
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

    UnderwaterProblem currProblem;
    currProblem.discount = discount;

    UnderwaterModel::readProblem(map_file, &currProblem);
    UnderwaterModel currModel(currProblem, true);

    RandSource currRandSource(numTrials);

    PolicyGraph policyGraph(currModel, 1, currModel.getNumObsVar());
    policyGraph.read(policy_file);

    Simulator currSim(currModel, policyGraph, maxSimulLength);
    double avgReward, avgDiscounted;
    // vector<State > trace;
    // vector<PolicyGraph::TypeActPair> actTrace;
    string str = "tracefile";

    RandStream randStream;
    randStream.initseed(currRandSource.get());

    if (numTrials == 1){
        for(int k = 0;k < int(currProblem.initialBeliefStates.size()); k++){
            //trace.clear();
            //actTrace.clear();
            currSim.runSingle(maxSimulLength, &avgReward, &avgDiscounted, str, currProblem.initialBeliefStates[k], &randStream);
            cout << "Average Reward: " << avgReward << "   Average Discounted Reward: " << avgDiscounted << "\n";

            // ofstream traceFileStream;
            // stringstream filenameStream;
            // filenameStream << "simout" << k << ".txt";
            // traceFileStream.open(filenameStream.str().c_str());
            // if(!traceFileStream.is_open()){
            //   	cerr << "Cannot open file to write simulation trace" << endl;
            //   	exit(EXIT_FAILURE);
            // }

            // for (long i=0; i< trace.size(); i++){
            //   	for(long j=0;j<trace[i].size();j++) traceFileStream << trace[i][j] << " ";
            //   	traceFileStream << endl;
            //   	traceFileStream << actTrace[i].first << " " << actTrace[i].second << endl;
            // }
            // traceFileStream.close();
        }
    } else {
	double sumReward = 0;
	double sumDiscounted = 0;
	vector<double> rewardRecord;
	vector<double> discountedRecord;

	for (long i= 0; i<numTrials; i++){
            long init =  currRandSource.get() % currProblem.initialBeliefStates.size();
            currSim.runSingle(maxSimulLength, &avgReward, &avgDiscounted, str, currProblem.initialBeliefStates[init], &randStream);
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

	if(simpleOutput)
            cout << avgDiscounted << " " <<  avgReward << "\n";
	else
            cout << "Average Reward: " << avgReward << " (95\% conf " << confIntervalRew <<") Average Discounted Reward: " << avgDiscounted <<" (95\% conf "<< confIntervalDiscounted << ")\n";

    }
};
