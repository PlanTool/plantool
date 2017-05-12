#include <ctime>
#include <cmath>
#include <iostream>
#include "BeliefForest.h"
#include "ActNode.h"
#include "Obs.h"
#include "ObsEdge.h"
#include "Model.h"
#include "Belief.h"
#include "BeliefNode.h"
#include "BeliefSet.h"
#include "PolicyGraph.h"
#include "RandSource.h"
#include "Bounds.h"

using namespace std;

BeliefForest::BeliefForest(Model& model, BeliefSet& beliefSet,
                           std::vector<Belief *>& roots,
                           PolicyGraph& policyGraph, Bounds& bounds,
                           RandSource& randSource, long numRandBackupStreams,
                           long numRandBeliefStreams):
        model(model), beliefSet(beliefSet), roots(roots),
        policyGraph(policyGraph), bounds(bounds), randSource(randSource),
        numRandBackupStreams(numRandBackupStreams),
        numRandBeliefStreams(numRandBeliefStreams)
{
    for (long i = 0; i < (long)roots.size(); i++){
        beliefSet.insert(roots[i]);
        roots[i]->beliefNode->uBound = model.getMaxReward()/(1-model.getDiscount());
        roots[i]->beliefNode->lBound = model.getMinReward()/(1-model.getDiscount());
        double gap = roots[i]->beliefNode->uBound - roots[i]->beliefNode->lBound;
        rootQueue.push(pair<double,long>(gap,i));
    }
    counts.resize(roots.size(),1); // pseudo count of 1
}

void BeliefForest::search(double targetGap, unsigned maxTime, double targetMultiplier, long displayInterval)
{
    time_t start, curr;
    double timeSoFar = 0;

    time(&start);
    time(&curr);
    cout << "\n";

    double maxGap = rootQueue.top().first;
    long maxGapRoot = rootQueue.top().second;

    while (maxGap > 0){
        // for display
        double temp = difftime(curr,start);
        if (temp - timeSoFar >= displayInterval){
            timeSoFar = temp;
            cout << "time: " << difftime(curr,start) << " WeightedGap: " << maxGap << " uBound: " << roots[maxGapRoot]->beliefNode->uBound << " lBound: " << roots[maxGapRoot]->beliefNode->lBound << " diff: " << roots[maxGapRoot]->beliefNode->uBound - roots[maxGapRoot]->beliefNode->lBound << " numBeliefs: " << beliefSet.numBeliefs() << " numPolicyNodes: " << policyGraph.getNumPolicyNodes() << "\n";
        }


        double currTarget = (roots[maxGapRoot]->beliefNode->uBound - roots[maxGapRoot]->beliefNode->lBound) * targetMultiplier;

        expandNodes(currTarget,maxGapRoot);
        backUpNodes(maxGapRoot);

        counts[maxGapRoot]++;
        double newGap = roots[maxGapRoot]->beliefNode->uBound - roots[maxGapRoot]->beliefNode->lBound;
        if (newGap < targetGap)
            newGap = 0;
        else
            newGap /= sqrt(counts[maxGapRoot]);
        rootQueue.pop();
        rootQueue.push(pair<double,long>(newGap,maxGapRoot));


        time(&curr);
        if (difftime(curr,start) > maxTime)
            break;

        maxGap = rootQueue.top().first;
        maxGapRoot = rootQueue.top().second;
    }

    maxGap = rootQueue.top().first;
    maxGapRoot = rootQueue.top().second;

    cout << "time: " << difftime(curr,start) << " WeightedGap: " << maxGap << " uBound: " << roots[maxGapRoot]->beliefNode->uBound << " lBound: " << roots[maxGapRoot]->beliefNode->lBound << " diff: " << roots[maxGapRoot]->beliefNode->uBound - roots[maxGapRoot]->beliefNode->lBound << " numBeliefs: " << beliefSet.numBeliefs() << " numPolicyNodes: " << policyGraph.getNumPolicyNodes() << "\n";

}

void BeliefForest::expandNodes(double target, long currRoot)
{
    double currTarget = target;
    Belief *currNode = roots[currRoot];
    double excessUncertainty = 0.0;

    beliefStack.push_back(currNode);
    bounds.backUp(*currNode);
    map<Obs,ObsEdge>::iterator iter = findBestObs(currNode, currTarget, excessUncertainty);
    int i= 0;
    while (excessUncertainty > 0){
        Belief *nextNode = iter->second.nextBelief;
        if (nextNode == NULL){
            nextNode = currNode->nextBelief(currNode->beliefNode->bestUBoundAct,
                                            iter->first);
            pair<Belief *,bool> ret = beliefSet.insert(nextNode);
            if (ret.second == false){
                delete nextNode;
            }
            iter->second.nextBelief = ret.first;
            nextNode = ret.first;
        }
        currNode = nextNode;
        beliefStack.push_back(currNode);
        bounds.backUp(*currNode);
        iter = findBestObs(currNode, currTarget, excessUncertainty);
        i++;
    }

}

inline map<Obs,ObsEdge>::iterator BeliefForest::findBestObs(Belief *currNode, double target, double& excessUncertainty)
{
    map<Obs,ObsEdge>::iterator iter = currNode->beliefNode->actNodes[currNode->beliefNode->bestUBoundAct.actNum]->obsChildren.begin();
    double currBest = -FLT_MAX;
    map<Obs,ObsEdge>::iterator currBestObs;
    while (iter !=  currNode->beliefNode->actNodes[currNode->beliefNode->bestUBoundAct.actNum]->obsChildren.end()){
        long count = iter->second.count;
        double diff = (iter->second.upper-iter->second.lower)/count - target;
        if (count*diff > currBest){
            currBest = count*diff;
            currBestObs = iter;
            excessUncertainty = diff;
        }
        ++iter;
    }
    return currBestObs;
}

inline void BeliefForest::backUpNodes(long currRoot)
{
    while (beliefStack.size() != 0){
        bounds.backUp(*beliefStack.back());
        beliefStack.pop_back();
    }
    policyGraph.updateInitNode(roots[currRoot]->beliefNode->bestPolicyNode,currRoot);
}
