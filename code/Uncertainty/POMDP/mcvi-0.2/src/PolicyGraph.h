#ifndef __POLICYGRAPH_H
#define __POLICYGRAPH_H

#include <vector>
#include <map>
#include <set>
#include <string>
#include "Obs.h"
#include "Action.h"

/**
   @class PolicyGraph
   @brief Data structure for policy graph.
   @details A policy graph consist of three types of nodes: action nodes,
   macro action nodes and initial policy nodes. Action nodes denotes only
   a single action, macro action nodes denote a subcontroller which has
   control until some terminating observation returns control to the
   policy graph while initial policy nodes are sink nodes that never return
   control to the policy graph. The node information consist
   of the type, action and a set of labeled edges, labeled with observations,
   pointing to next policy nodes.
   When running simulations, if an observation is not in the
   possible set of labels corresponding to the edges of the current
   policy graph node, and a neighbouring observation is not found,
   control is passed on to the first initial policy
   (init policy with index 0), hence a good first initial policy will make
   the policy considerably more robust.

   The policy graph is designed for models where the initial belief is known,
   so has a root that always returns the same initial action.

   In addition, the policy graph nodes are grouped according to the
   observation group of in which they are created.
   This allows policy search to be
   done only those with the same observation group, if desired.
   The nodes are always
   maintained in order of insert, so evaluations only need to be done for
   new nodes if old ones have been evaluated before.

   @author Wee Sun Lee
   @date 21 August 2009
*/
class PolicyGraph
{
  public:
    struct Node; // described below

    /**
       Policy graph edge labeled by observation \a obs and pointing to the next
       policy graph node.
    */
    struct Edge
    {
        Obs obs;
        Node *nextNode;

        Edge(long numObsVar)
        {
            obs.obs.resize(numObsVar,0);
        }
    };


    /**
       Policy graph node. Node \a type takes values initial, macro, act.
       The variable \a action is the index passed to the model for appropriate
       action. Edges should be maintained sorted according to observation to
       allow binary search to operate correctly.
    */
    struct Node
    {
        Action action;
        std::vector<Edge> edges;

        Node(Action action): action(action) {}
    };


    struct uniquePolicySet
    {
        std::set<long> policySet;
        std::vector<Node *>  policyVec;
    };


    /**
       Initializes the policy graph.
       @param [in] numInitPolicies Number of initial policies in the model that
       uses the policy graph.
       @param [in] numObsVar Number of variables in the observation vector
       @param [in] numRoots Number of roots for the graph
    */
    PolicyGraph(Model& model, long numInitPolicies, long numObsVar, long numRoots = 1);

    ~PolicyGraph();

    /**
       Returns the ith root of the graph
    */
    inline Node *getRoot(long i=0) { return initActionNode[i]; };

    inline long getNodeIndex(Node *ptr)
    {
        return nodeSet.find(ptr)->second;
    }

    /**
       Returns the action type and action index given the current controller
       state upon receiving macro state and observation.
    */
    inline Action& getAction(Node *ptr)
    {
        return ptr->action;
    }

    inline Action& getAction(long i)
    {
        return allNodes[i]->action;
    }

    /**
       Returns the next graph node given the current graph node and observation.
    */
    inline Node *getNextState(Node *ptr, const Obs& obs)
    {
        return find(obs,ptr->edges);
    }

    inline long getNextNodeIndex(long curr, Obs& obs)
    {
        return nodeSet.find(find(obs,allNodes[curr]->edges))->second;
    }

    /**
       Change the initial graph node to a different one.
    */
    inline void updateInitNode(Node *ptr, long i = 0)
    {
        initActionNode[i] = ptr;
    }

    /**
       Insert a node to the \a obs group of the graph.
       @return a pair of node pointer and a variable indicating whether
       the node is already in the graph. If the node is already in the graph,
       the second variable will return false, and the first variable will contain
       a pointer to the node already in the graph, else the second variable
       will return true and the first variable will contain the pointer just
       inserted. If insertion fails, responsibility to destroy the object
       lies with the caller. Nodes successfully inserted will be destroyed
       when the policy graph is destroyed.
    */
    std::pair<Node *, bool> insert(Node *n, long obsGrp);

    /**
       Initial policies are maintained separately from other graph nodes
       as they are sink nodes. this routine returns the i-th initial policy.
    */
    inline Node *getInitPolicy(long i)
    {
        return initialPolicies[i];
    }

    /**
       @return pointer to node
       @param [in] obsGrp Macro state group that the node belongs to
       @param [in] index The index within the group. The index is ordered
       according to insertion time.
    */
    inline Node *getPolicy(long obsGrp, long index)
    {
        std::map<long, uniquePolicySet >::iterator iter = obsPolicies.find(obsGrp);
        return iter->second.policyVec[index];
    }

    /**
       @return the size of the group of nodes corresponding to \a obs
    */
    inline long getSize(long obsGrp)
    {
        std::map<long, uniquePolicySet >::iterator iter = obsPolicies.find(obsGrp);
        if (iter == obsPolicies.end())
            return 0;
        else
            return iter->second.policyVec.size();
    }

    /**
       @return the total number of policy nodes.
    */

    inline long getNumPolicyNodes()
    {
        return allNodes.size();
    }

    /**
       Write out the policy graph to \a filename
    */
    void write(std::string filename);

    /**
       Reads in what has be written out with \a write
    */
    void read(std::string filename);

  private:
    // This function object compares Node objects. Used in map.
    class nodeComparator
    {
      public:
        bool operator()(const Node *node1, const Node *node2) const;
    };

    long numInitPolicies; // total number of initial policies
    long numObsVar;
    long numRoots;
    std::vector<Node *>initActionNode; // root

    // map keyed by obs groups, where each entry is an ordered
    // vector of node pointers, ordered by insertion time
    std::map<long, uniquePolicySet > obsPolicies;

    // Used for making sure that node is unique and as reverse pointer back
    // to the pointer's location in \a allNodes
    std::map<Node *, long, nodeComparator> nodeSet;

    // Provides access to the initial policies
    std::vector<Node *> initialPolicies;

    // Store all the nodes. Nodes are labeled with their index in this vector.
    std::vector<Node *> allNodes;

    // Given a sorted vector of edges, find the edge corresponding to \a obs
    // and return the edge's next node
    Node *find(const Obs& obs, const std::vector<Edge>& edges);

    // Clean up allocated memory
    void clean();

    // Compare observations: return -1 if first < second, 0 if first == second
    // and 1 if first > second
};
#endif // __POLICYGRAPH_H
