#include "CorridorModel.h"
#include "Action.h"
#include "ParticlesBeliefSet.h"
#include "Controller.h"
#include "Obs.h"
#include "RandSource.h"
#include "BeliefNode.h"
#include "ParticlesBelief.h"
#include <iostream>

using namespace std;

int main(int argc, char **argv)
{
    CorridorModel currModel(500);
    RandSource currRandSource(500);

    Obs initialObs(vector<long>(currModel.getNumObsVar(),0));
    initialObs.obs[1] = -1;

    PolicyGraph policyGraph(currModel, 1, currModel.getNumObsVar());
    policyGraph.read("policy");

    Controller control(policyGraph, currModel, initialObs, 100, 100, &currRandSource, true);

    cout<<"Action      : 0 (left), 1 (right), 2 (enter)\n";
    cout<<"Observation : -1 (terminate), 0 (nothing), 1 (wrong-door), 2 (left-end), 3 (right-end)\n";

    cout<<"First action (dummy observation) : ";
    cout<<control.nextAction(Obs(vector<long>(1,-1))).getActNumUser()<<endl;
    cout<<"Observe nothing : ";
    cout<<control.nextAction(Obs(vector<long>(1,0))).getActNumUser()<<endl;
    cout<<"Observe right-end : ";
    cout<<control.nextAction(Obs(vector<long>(1,3))).getActNumUser()<<endl;
    cout<<"Observe nothing : ";
    cout<<control.nextAction(Obs(vector<long>(1,0))).getActNumUser()<<endl;
    cout<<"Observe nothing : ";
    cout<<control.nextAction(Obs(vector<long>(1,0))).getActNumUser()<<endl;
    cout<<"Observe termination : ";
    cout<<control.nextAction(Obs(vector<long>(1,-1))).getActNumUser()<<endl;
}
