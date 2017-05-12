#include "Controller.h"
#include "Model.h"
#include "Include.h"
#include "BeliefNode.h"
#include "ParticlesBelief.h"
#include <iostream>

using namespace std;

Controller::Controller(PolicyGraph& policy, Model& model, Obs& rootObs, int numNextBeliefStreams, int maxMacroActLength, RandSource* randSource, bool trackBelief):
        policy(policy),
        firstAction(true),
        trackBelief(trackBelief),
        staleBelief(false)
{
    ParticlesBelief* rootBelief = new ParticlesBelief(new BeliefNode(rootObs));
    double w = 1.0 / numNextBeliefStreams;
    for (int i = 0; i < numNextBeliefStreams; ++i) {
        State st = model.sampleInitState();
        Particle temp(st, 0, w);
        rootBelief->belief.push_back(temp);
    }

    init(model, rootBelief, randSource, numNextBeliefStreams, maxMacroActLength);
}

Controller::Controller(PolicyGraph& policy, Model& model, Belief* belief, RandSource* randSource, bool trackBelief):
        policy(policy),
        firstAction(true),
        trackBelief(trackBelief),
        staleBelief(false)
{
    // dummy value for the last 2 params
    init(model, belief, randSource, -1, -1);
}

void Controller::init(Model& model, Belief* belief, RandSource* randSource, int numNextBeliefStreams, int maxMacroActLength) {
    currBel = belief;

    Action::initStatic(&model);

    BeliefNode::initStatic(&model);
    ParticlesBelief::initStatic(randSource, numNextBeliefStreams, maxMacroActLength);
    currGraphNode = policy.getRoot(0);
}

Action const& Controller::nextAction(Obs const& obs, int dummy)
{
    if (firstAction) {
        // If this is the first action then ignore the observation
        firstAction = false;
        return policy.getAction(currGraphNode);
    }

    Action& action = policy.getAction(currGraphNode);
    PolicyGraph::Node *nextGraphNode = policy.getNextState(currGraphNode, obs);
    currGraphNode = nextGraphNode;

    // Update the belief
    if (trackBelief) {
        Belief* nextBelief = NULL;
        if (!staleBelief) {
            nextBelief = currBel->nextBelief(action, obs, false);
            if (nextBelief == NULL) {
                cout<<"\nCannot track the belief from now on!!\n";
                cout<<"You will get a stale belief if you call currBelief()\n\n";
                staleBelief = true;
            } else {
                delete currBel;
                currBel = nextBelief;
            }
        }
    }

    return policy.getAction(currGraphNode);
}

BeliefDefine Controller::currBelief() const
{
    BeliefDefine result;
    result.belief = currBel;
    result.stale = staleBelief;

    return result;
}
