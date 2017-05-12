#include <cstdlib>
#include <iostream>
#include <fstream>
#include "PolicyGraph.h"

using namespace std;

typedef std::map<long, PolicyGraph::uniquePolicySet >::iterator MSPit;

bool PolicyGraph::nodeComparator::operator()(const PolicyGraph::Node *node1, const PolicyGraph::Node *node2) const
{
    int comp = Action::compare(node1->action, node2->action);
    if (comp)
        return (comp == 1);

    if (node1->edges.size() < node2->edges.size())
        return true;
    else if (node2->edges.size() < node1->edges.size())
        return false;
    for (long i = 0; i < (long)node1->edges.size(); i++){
        for (long j = 0; j < (long)node1->edges[i].obs.obs.size(); j++) {
            if (node1->edges[i].obs.obs[j] < node2->edges[i].obs.obs[j])
                return true;
            else if (node2->edges[i].obs.obs[j] < node1->edges[i].obs.obs[j])
                return false;
        }
        if (node1->edges[i].nextNode < node2->edges[i].nextNode)
            return true;
        else if (node2->edges[i].nextNode < node1->edges[i].nextNode)
            return false;
    }

    return false;
}

PolicyGraph::PolicyGraph(Model& model, long numInitPolicies, long numObsVar, long numRoots): numInitPolicies(numInitPolicies), numObsVar(numObsVar), numRoots(numRoots)
{
    Action::initStatic(&model);
    // insert all the initial policies
    for (long i = 0; i < numInitPolicies; i++){
        PolicyGraph::Node *tempNode = new PolicyGraph::Node(Action(i));
        PolicyGraph::Edge tempEdge(numObsVar);
        tempEdge.obs.obs[0] = LoopObs;
        tempEdge.nextNode = tempNode; // loop back
        tempNode->edges.push_back(tempEdge);
        allNodes.push_back(tempNode); // put in list of all nodes
        // put in set of nodes
        nodeSet.insert(pair<PolicyGraph::Node *,int>(tempNode,i));
        initialPolicies.push_back(tempNode); // put into list of init policies
    }
    initActionNode.resize(numRoots);
    for (long i = 0; i < numRoots; i++){
        initActionNode[i] = initialPolicies[0]; // initialize roots
    }
}

PolicyGraph::~PolicyGraph()
{
    clean();
}

void PolicyGraph::clean()
{
    for (long i=0; i < (long)allNodes.size(); i++){
        delete allNodes[i];
    }
    allNodes.clear();
    initialPolicies.clear();
    obsPolicies.clear();
    nodeSet.clear();
}

// Binary search to find edge corresponding to \obs
// Returns first init policy if fail
PolicyGraph::Node *PolicyGraph::find(const Obs& obs, const std::vector<Edge>& edges)
{
    long low = 0, high = edges.size()-1;
    while (high >= low){
        long mid = (high+low) >> 1;
        int comp = obs.compare(edges[mid].obs);
        if (comp == 0) return edges[mid].nextNode;
        else if (comp == -1)
            high = mid-1;
        else
            low = mid+1;
    }
    return getInitPolicy(0);
}

pair<PolicyGraph::Node *, bool> PolicyGraph::insert(PolicyGraph::Node *n, long obsGrp)
{
    map<Node *, long, nodeComparator>::iterator it = nodeSet.find(n);
    MSPit iter = obsPolicies.find(obsGrp);
    if (iter == obsPolicies.end()){
        PolicyGraph::uniquePolicySet tempSet;
        pair<MSPit, bool> ret = obsPolicies.insert(pair<long, PolicyGraph::uniquePolicySet>(obsGrp,tempSet));
        iter = ret.first;
    }
    if (it != nodeSet.end()){
        if (iter->second.policySet.find(it->second) == iter->second.policySet.end()){
            iter->second.policyVec.push_back(it->first);
            iter->second.policySet.insert(it->second);
        }
        return pair<PolicyGraph::Node *, bool>(it->first,false);
    } else {
        iter->second.policyVec.push_back(n);
        iter->second.policySet.insert(allNodes.size());
        nodeSet.insert(pair<PolicyGraph::Node *, int>(n, allNodes.size()));
        allNodes.push_back(n);
        return pair<PolicyGraph::Node *, bool>(n,true);
    }
}

void PolicyGraph::write(std::string filename)
{
    ofstream fp;
    fp.open(filename.c_str());
    if (!fp.is_open()){
        cerr << "Fail to open " << filename << "\n";
        exit(EXIT_FAILURE);
    }
    fp << allNodes.size() << "\n";
    fp << obsPolicies.size() << "\n";
    fp << numRoots << "\n";
    for (long i = 0; i < numRoots; i++){
        map<Node *, long, nodeComparator>::iterator temp = nodeSet.find(initActionNode[i]);
        fp << temp->second << " ";
    }
    fp << "\n";
    for (long i = 0; i < (long)allNodes.size(); i++){
        fp << allNodes[i]->action.type << " " << allNodes[i]->action.getActNumUser() << " ";
        fp << allNodes[i]->edges.size() << " ";
        for (long j = 0; j < (long)allNodes[i]->edges.size(); j++){
            map<Node *, long, nodeComparator>::iterator temp = nodeSet.find(allNodes[i]->edges[j].nextNode);
            for (long k = 0; k < numObsVar; k++)
                fp <<  allNodes[i]->edges[j].obs.obs[k] << " ";
            fp << temp->second << " ";
        }
        fp << "\n";
    }

    for (MSPit iter = obsPolicies.begin(); iter != obsPolicies.end(); ++iter){
        fp << iter->first << " ";
        fp <<  iter->second.policyVec.size() << " ";
        for (long j = 0; j < (long)iter->second.policyVec.size(); j++){
            map<Node *, long, nodeComparator>::iterator temp = nodeSet.find(iter->second.policyVec[j]);
            fp <<  temp->second << " ";
        }
        fp << "\n";
    }

    fp.close();
}

void PolicyGraph::read(std::string filename)
{
    ifstream fp;
    fp.open(filename.c_str(), ios::in);
    if (!fp.is_open()){
        cerr << "Fail to open " << filename << "\n";
        exit(EXIT_FAILURE);
    }
    long numNodes, numObsGrps;
    fp >> numNodes;
    fp >> numObsGrps;
    clean(); // clear any old stuff

    // alloc memory first
    for (long i = 0; i< numNodes; i++){
        PolicyGraph::Node * tempNode = new Node(Action(-1));
        allNodes.push_back(tempNode);
    }

    // root
    fp >> numRoots;
    initActionNode.resize(numRoots);
    long tempIndex;
    for (long i = 0; i < numRoots; i++){
        fp >> tempIndex;
        initActionNode[i] = allNodes[tempIndex];
    }

    long temptype;
    long actNum;
    for (long i = 0; i < numNodes; i++){
        fp >> temptype;
        fp >> actNum;
        allNodes[i]->action.setActNumUser((actType)temptype, actNum);
        long numChildren;
        fp >> numChildren;
        for (long j = 0; j < numChildren; j++){
            PolicyGraph::Edge tempEdge(numObsVar);
            for (long k = 0; k < numObsVar; k++)
                fp >> tempEdge.obs.obs[k];
            long next;
            fp >> next;
            tempEdge.nextNode = allNodes[next];
            allNodes[i]->edges.push_back(tempEdge);
        }
        nodeSet.insert(pair<PolicyGraph::Node *,long>(allNodes[i],i));
    }

    for (long i = 0; i < numInitPolicies; i++){
        initialPolicies.push_back(allNodes[i]);
    }

    for (long i = 0; i < numObsGrps; i++){
        long currObsGrp;
        fp >> currObsGrp;
        long numMSNodes;
        fp >> numMSNodes;
        PolicyGraph::uniquePolicySet tempSet;
        for (long j = 0; j < numMSNodes; j++){
            long tempNodeNum;
            fp >> tempNodeNum;
            tempSet.policySet.insert(tempNodeNum);
            tempSet.policyVec.push_back(allNodes[tempNodeNum]);
        }
        obsPolicies.insert(pair<long, uniquePolicySet >(currObsGrp, tempSet));
    }
    fp.close();
}
