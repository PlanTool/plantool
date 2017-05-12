#include <cfloat>
#include <cmath>
#include <cstdlib>
#include <ctime>
#include <iostream>
#include <fstream>
#include "ValueIteration.h"

using namespace std;

void ValueIteration::doValueIteration(std::vector<std::vector<double> >& rewardMatrix, std::vector<std::vector<std::vector<std::pair<long,double> > > >& transMatrix, double targetPrecision, long displayInterval)
{
    time_t start, curr;
    double timeSoFar = 0;

    values.resize(numStates);
    actions.resize(numStates);

    time(&start);
    time(&curr);

    vector<vector<double> > tempValues;

    tempValues.resize(2);
    for (long i=0; i<2; i++){
        tempValues[i].resize(numStates,0);
    }

    double currChange = Inf;
    long currIndex = 0, nextIndex = 1;

    while (currChange > targetPrecision){
        // for display
        double temp = difftime(curr,start);
        if (temp - timeSoFar >= displayInterval){
            timeSoFar = temp;
            cout << "time: " << temp << " Diff: " << currChange << "\n";
        }

        for (long i=0; i<numStates; i++){
            double bestValue = NegInf;
            long bestAction = 0;
            for (long j = 0; j < numActions; j++){
                double currValue = rewardMatrix[i][j];
                for (long k = 0; k < (long)transMatrix[i][j].size(); k++){
                    long nextState = transMatrix[i][j][k].first;
                    double prob = transMatrix[i][j][k].second;
                    currValue +=  discount * prob * tempValues[nextIndex][nextState];
                }
                if (currValue > bestValue){
                    bestValue = currValue;
                    bestAction = j;
                }
            }
            tempValues[currIndex][i] = bestValue;
            actions[i] = bestAction;
        }
        currChange = 0;
        for (long i = 0; i < numStates; i++){
            if (fabs(tempValues[currIndex][i]- tempValues[nextIndex][i])> currChange)
                currChange = fabs(tempValues[currIndex][i]- tempValues[nextIndex][i]);
        }

        currIndex = nextIndex;
        nextIndex = 1-nextIndex;
    }

    for (long i =0; i< numStates; i++){
        values[i] = tempValues[nextIndex][i];
    }

    cout << "time: " << difftime(curr,start) << " Diff: " << currChange << "\n";
}

void ValueIteration::write(std::string filename)
{
    ofstream fp;
    fp.open(filename.c_str());
    if (!fp.is_open()){
        cerr << "Fail to open " << filename << "\n";
        exit(EXIT_FAILURE);
    }
    fp << numStates << "\n";
    for (long i=0; i < numStates; i++){
        fp << actions[i] << "\n";
    }
    fp.close();
}

void ValueIteration::read(std::string filename)
{
    ifstream fp;
    fp.open(filename.c_str(), ios::in);
    if (!fp.is_open()){
        cerr << "Fail to open " << filename << "\n";
        exit(EXIT_FAILURE);
    }

    fp >> numStates;
    actions.resize(numStates,0);
    for (long i=0; i< numStates; i++){
        fp >> actions[i];
    }
    fp.close();
}
