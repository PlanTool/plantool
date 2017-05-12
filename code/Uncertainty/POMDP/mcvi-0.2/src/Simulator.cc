#include <iostream>
#include <fstream>
#include "Simulator.h"
#include "Model.h"
#include "RandSource.h"

using namespace std;

void Simulator::runSingle(long length, double* sumReward,
                          double* sumDiscounted, string trace_fn,
                          State startState, RandStream* randStream,
                          long rootIndex)
{
    ofstream fp;
    fp.open(trace_fn.c_str());
    if (!fp.is_open()){
        cerr << "Fail to open " << trace_fn << "\n";
        exit(EXIT_FAILURE);
    }

    // Variables for computing rewards
    double currReward;
    *sumReward = *sumDiscounted = 0;
    double currDiscount = 1;

    // States
    State* currState = new State(startState); // initialize
    State* nextState = new State(model.getNumStateVar(),0); // allocate space

    // Macro action
    long currMacroActState = InitMacroActState, nextMacroActState;
    long numSelfLoops = 1;

    // To keep track of policy graph state
    Obs obs(vector<long>(model.getNumObsVar(),0)); // observation
    PolicyGraph::Node *currGraphNode = policy.getRoot(rootIndex);
    PolicyGraph::Node *nextGraphNode;

    Action* action = &(policy.getAction(currGraphNode));

    // Run simulation
    for (long t=0; t < length; t++){
        for(State::iterator it=currState->begin(); it != currState->end(); ++it)
            fp << *it << " ";
        fp<<endl;
        fp << action->type << " " << action->getActNumUser() << endl;

        // Check for terminal state
        if (model.isTermState(*currState)){
            //fp<<"success"<<endl;
            break;
        }

        if (action->type == Initial){ // check node type
            currReward = model.initPolicy(*currState, *action, currMacroActState, nextState, &nextMacroActState, &obs, randStream);
            currMacroActState = nextMacroActState;
        }
        else if (action->type == Macro){
            currReward = model.sample(*currState, *action, currMacroActState, nextState, &nextMacroActState, &obs, randStream);
            currMacroActState = nextMacroActState;

            // exit macro action
            if (model.getObsType(obs) == OtherObs){
                nextGraphNode = policy.getNextState(currGraphNode, obs);
                currGraphNode = nextGraphNode;
                currMacroActState = InitMacroActState;
                numSelfLoops = 1;
            } else if (numSelfLoops == maxMacroActLength){
                nextGraphNode = policy.getNextState(currGraphNode, obs);
                currGraphNode = nextGraphNode;
                currMacroActState = InitMacroActState;
                numSelfLoops = 1;
            } else{
                numSelfLoops++;
            }
        }
        else{
            currReward = model.sample(*currState, *action, nextState, &obs, randStream);
            nextGraphNode = policy.getNextState(currGraphNode, obs);
            currGraphNode = nextGraphNode;
        }
        *sumReward += currReward;
        *sumDiscounted += currDiscount * currReward;
        currDiscount *= model.getDiscount();

        // swap 2 pointers
        State* temp;
        temp = currState;
        currState = nextState;
        nextState = temp;

        action = &(policy.getAction(currGraphNode));
    }

    delete currState;
    delete nextState;
}

void Simulator::runSingle(long length, double* sumDiscounted,
                          State startState, PolicyGraph::Node &currNode,
                          RandStream* randStream)
{
    // Variables for computing rewards
    double currReward;
    *sumDiscounted = 0;
    double currDiscount = 1;

    // States
    State* currState = new State(startState); // initialize
    State* nextState = new State(model.getNumStateVar(),0); // allocate space

    // Macro action
    long currMacroActState = InitMacroActState, nextMacroActState;
    long numSelfLoops = 1;

    // To keep track of policy graph state
    Obs obs; // observation
    obs.obs.resize(model.getNumObsVar(),0);

    PolicyGraph::Node* currGraphNode = &currNode, *nextGraphNode;

    // Initialize simulation

    Action* action = &(policy.getAction(currGraphNode));
    // Run simulation
    for (long t=0; t < length; t++){

        // Check for terminal state
        if (model.isTermState(*currState)){
            break;
        }

        if (action->type == Initial){ // check node type
            currReward = model.initPolicy(*currState, *action, currMacroActState, nextState, &nextMacroActState, &obs, randStream);
            currMacroActState = nextMacroActState;
        }
        else if (action->type == Macro){
            currReward = model.sample(*currState, *action, currMacroActState, nextState, &nextMacroActState, &obs, randStream);
            currMacroActState = nextMacroActState;

            // exit macro action
            if (model.getObsType(obs) == OtherObs){
                nextGraphNode = policy.getNextState(currGraphNode, obs);
                currGraphNode = nextGraphNode;
                currMacroActState = InitMacroActState;
                numSelfLoops = 1;
            } else if (numSelfLoops == maxMacroActLength){
                nextGraphNode = policy.getNextState(currGraphNode, obs);
                currGraphNode = nextGraphNode;
                currMacroActState = InitMacroActState;
                numSelfLoops = 1;
            } else{
                numSelfLoops++;
            }
        }
        else{
            currReward = model.sample(*currState, *action, nextState, &obs, randStream);
            nextGraphNode = policy.getNextState(currGraphNode, obs);
            currGraphNode = nextGraphNode;
        }
        *sumDiscounted += currDiscount * currReward;
        currDiscount *= model.getDiscount();

        State* temp;
        temp = currState;
        currState = nextState;
        nextState = temp;

        action = &(policy.getAction(currGraphNode));
    }

    delete currState;
    delete nextState;
}
