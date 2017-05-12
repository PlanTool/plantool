#include "UnderwaterModel.h"
#include "UnderwaterProblem.h"
#include "Solver.h"
#include "Action.h"
#include "ParticlesBelief.h"
#include <sstream>
#include <iostream>
#include <cstdlib>
#include <ctime>

using namespace std;

int main(int argc, char **argv)
{
    ostringstream message;

    message << "-m <mapfile> as the first arguments\n";

    if (argc < 2 || argv[1][0] != '-' || argv[1][1] != 'm') {
        cout << message.str() << "\n";
        exit(1);
    }
    string map_file = argv[2];

    Solver solver;

    solver.input(argc,argv,4);

    UnderwaterProblem currProblem;
    currProblem.discount = solver.discount;

    // HUY note - 1. Read problem from file
    UnderwaterModel::readProblem(map_file, &currProblem);
    bool useMacroBool = true;
    if (solver.useMacro != 1)
        useMacroBool = false;

    // HUY note - 2. Initialize UnderwaterModel
    UnderwaterModel currModel(currProblem, useMacroBool);

    vector<long> pathLength(currProblem.initialBeliefStates.size(), 0);
    Obs initialObs(vector<long>(currModel.getNumObsVar(),0));
    initialObs.obs[1] = -1;

    solver.solve(currModel, currProblem.initialBeliefStates, initialObs, pathLength);
}
