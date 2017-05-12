#include <iostream>
#include "Bounds.h"
#include "Action.h"
#include "ActNode.h"
#include "Obs.h"
#include "ObsEdge.h"
#include "Model.h"
#include "Belief.h"
#include "BeliefNode.h"
#include "PolicyGraph.h"
#include "RandSource.h"

using namespace std;

void Bounds::updateBestActions(Belief& belief)
{
    bool debug = false;

    if (debug) {
        cout<<"Bounds::updateBestActions\n";
    }

    BeliefNode& beliefNode = *(belief.beliefNode);

    // find best bounds at the belief
    beliefNode.lBound = NegInf;
    beliefNode.uBound = NegInf;

    beliefNode.bestLBoundAct.setActNum(-1);
    beliefNode.bestUBoundAct.setActNum(-1);

    for (long i = model.getNumInitPolicies();
         i < model.getNumInitPolicies() + model.getNumMacroActs() + model.getNumActs();
         i++)
        if (beliefNode.actNodes[i] != NULL) {
            if (debug) {
                cout<<"Bounds::updateBestActions "<<i<<" => "<<beliefNode.actNodes[i]->avgLower<<"\n";
            }

            if (beliefNode.lBound < beliefNode.actNodes[i]->avgLower){
                beliefNode.lBound = beliefNode.actNodes[i]->avgLower;

                if (debug) {
                    cout<<"Bounds::updateBestActions Change bestLBoundAct to "<<i<<"\n";
                }
                beliefNode.bestLBoundAct.setActNum(i);
                beliefNode.lastUpdated = beliefNode.actNodes[i]->lastUpdated;
            }

            if (debug) {
                cout<<"Bounds::updateBestActions "<<i<<" => "<<beliefNode.actNodes[i]->avgUpper<<"\n";
            }

            if (beliefNode.uBound < beliefNode.actNodes[i]->avgUpper){
                beliefNode.uBound = beliefNode.actNodes[i]->avgUpper;

                if (debug) {
                    cout<<"Bounds::updateBestActions Change bestUBoundAct to "<<i<<"\n";
                }
                beliefNode.bestUBoundAct.setActNum(i);
            }
        }

    if (debug) {
        cout<<"Leaving Bounds::updateBestActions\n";
    }
}

void Bounds::backUp(Belief& belief)
{
    bool debug = false;

    // backup all the actions first
    // if new belief, check init policies
    BeliefNode& beliefNode = *(belief.beliefNode);

    if (debug) {
        cout<<"Bounds::backUp\n";
    }

    if (beliefNode.lastUpdated == Never) {
        initBeliefForBackUp(belief);
        backUpInitPolicies(belief);
    }
    backUpMacroActions(belief);
    backUpActions(belief);

    updateBestActions(belief);

    // construct new policy node and try to insert
    PolicyGraph::Node *tempNode = new PolicyGraph::Node(beliefNode.bestLBoundAct);

    long actIndex = beliefNode.bestLBoundAct.actNum;

    if (debug) {
        cout<<beliefNode.actNodes[actIndex]->avgLower<<"\n";
        cout<<"beliefNode.actNodes[beliefNode.bestLBoundAct.actNum]->obsChildren.size() = "<<(beliefNode.actNodes[actIndex]->obsChildren.size())<<"\n";
    }

    // construct policy node observation edges
    for (map<Obs,ObsEdge>::iterator iter = beliefNode.actNodes[actIndex]->obsChildren.begin();
         iter != beliefNode.actNodes[actIndex]->obsChildren.end();
         ++iter){
        if (debug) {
            cout<<"Bounds::backUp temp"<<"\n";
        }

        PolicyGraph::Edge tempEdge(model.getNumObsVar());
        tempEdge.obs = iter->first;
        tempEdge.nextNode = iter->second.bestPolicyNode;
        tempNode->edges.push_back(tempEdge);
    }

    //insert into graph
    pair<PolicyGraph::Node *, bool> outcome = policyGraph.insert(tempNode, model.getObsGrpFromObs(beliefNode.obs));
    // delete if node already present
    if (!outcome.second){
        delete tempNode;
    }
    beliefNode.bestPolicyNode = outcome.first;
//    beliefNode.lastUpdated = policyGraph.getSize(model.getObsGrpFromObs(beliefNode.obs));

    if (debug) {
        cout<<"Leaving backUp\n";
    }
}

void Bounds::backUpInitPolicies(Belief& belief)
{
    bool debug = false;

    if (debug) {
        cout<<"Bounds::backUpInitPolicies\n";
    }

    BeliefNode& beliefNode = *(belief.beliefNode);

    for (long i = 0; i < model.getNumInitPolicies(); i++){
        if (beliefNode.actNodes[i] == NULL)
            beliefNode.actNodes[i] = new ActNode(*(new Action(i)),
                                                 belief,
                                                 this);
        double policyValue = 0;

        vector<Particle> particles;
        // run simulation
        for (Belief::const_iterator it = belief.begin(numRandStreams);
             it != belief.end(); ++it) {
            particles.push_back(*it);
        }

        for (long j = 0; j < numRandStreams; j++) {
            RandStream randStream;
            randStream.initseed(randSource.getStream(j).get());
            Particle const& currParticle = particles[j];
            State const* currState = &currParticle.state;
            State nextState(model.getNumStateVar(),0);

            long currMacroActState = InitMacroActState;
            long nextMacroActState;

            Obs obs(vector<long>(model.getNumObsVar(),0));

            double currDiscount = 1;
            double sumDiscounted = 0;
            double currReward;
            for (long k = 0; k < maxSimulLength; k++){
                // Check for terminal state
                if (model.isTermState(*currState)){
                    break;
                }
                currReward = model.initPolicy(*currState, Action(i), currMacroActState, &nextState, &nextMacroActState, &obs, &randStream);
                currMacroActState = nextMacroActState;
                sumDiscounted += currDiscount * currReward;
                currDiscount *= model.getDiscount();
                currState = &nextState;
            }
            double currWeight = power(model.getDiscount(),currParticle.pathLength);
            policyValue += currWeight*sumDiscounted;
        }

        // compute averages
        beliefNode.actNodes[i]->avgLower = beliefNode.actNodes[i]->avgUpper = policyValue/numRandStreams;

        // construct observation edge
        Obs tempObs(vector<long>(model.getNumObsVar(),0));
        model.setObsType(&tempObs,LoopObs);
        pair<map<Obs,ObsEdge>::iterator, bool> ret =
                beliefNode.actNodes[i]->obsChildren.insert(pair<Obs,ObsEdge >(tempObs,ObsEdge(tempObs, this)));
        ObsEdge& insertedEdge = ret.first->second;
        insertedEdge.lower = insertedEdge.upper = policyValue;
        insertedEdge.bestPolicyNode = policyGraph.getInitPolicy(i);
    }

    if (debug) {
        cout<<"Leaving Bounds::backUpInitPolicies\n";
    }
}

void Bounds::backUpMacroActions(Belief& belief)
{
    bool debug = false;

    if (debug) {
        cout<<"Bounds::backUpMacroAction\n";
    }

    BeliefNode& beliefNode = *(belief.beliefNode);

    Obs obs(vector<long>(model.getNumObsVar(), 0));
    #pragma omp parallel for schedule(guided)
    for (long i = model.getNumInitPolicies();
         i < model.getNumInitPolicies() + model.getNumMacroActs();
         i++) {
        if (model.allowableAct(belief, Action(i))) {
                if (beliefNode.actNodes[i] == NULL)
                    beliefNode.actNodes[i] = new ActNode(*(new Action(i)),
                                                         belief,
                                                         this);
                beliefNode.actNodes[i]->generateMacroObsPartitions();
                beliefNode.actNodes[i]->backup();
                beliefNode.actNodes[i]->clearObsPartitions();
        }
    }
}

void Bounds::backUpActions(Belief& belief)
{
    bool debug = false;

    if (debug) {
        cout<<"Bounds::backUpActions\n";
    }

    BeliefNode& beliefNode = *(belief.beliefNode);

    //long currMacroActState, nextMacroActState;
    Obs obs(vector<long>(model.getNumObsVar(),0));

    #pragma omp parallel for schedule(guided)
    for (long i = model.getNumInitPolicies() + model.getNumMacroActs();
         i < model.getNumInitPolicies() + model.getNumMacroActs() + model.getNumActs();
         i++){
        if (model.allowableAct(belief, Action(i))){
            if (beliefNode.actNodes[i] == NULL)
                beliefNode.actNodes[i] = new ActNode(*(new Action(i)),
                                                     belief,
                                                     this);
            // generate partitions of states at observation children of this action
            beliefNode.actNodes[i]->generateObsPartitions();
            beliefNode.actNodes[i]->backup();
            beliefNode.actNodes[i]->clearObsPartitions();
        }
    }

    if (debug) {
        cout<<"Leaving Bounds::backUpActions\n";
    }
}

void Bounds::buildActNodes(Belief& belief)
{
    BeliefNode& beliefNode = *(belief.beliefNode);
    for (long i = 0; i < model.getNumInitPolicies() + model.getNumMacroActs() + model.getNumActs(); i++) {
        Action* act = new Action(i);
        beliefNode.actNodes.push_back(new ActNode(*act, belief, this));
    }
}

void Bounds::initBeliefForBackUp(Belief& belief)
{
    BeliefNode& beliefNode = *(belief.beliefNode);

    buildActNodes(belief);
    beliefNode.lBound = beliefNode.uBound = NegInf;
}
