/**
   @brief An implementation of the controller
   @author Le Trong Dao
   @date 2012-02-05
**/

#ifndef __CONTROLLER_H
#define __CONTROLLER_H

// Some macros to adapt to the interface
#define ActionDefine Action const&
#define ObsDefine Obs const&

class Model;
class Belief;
class Action;
class Obs;

struct BeliefDefine {
    Belief const* belief;
    bool stale;
};

#include "ControllerInterface.h"
#include "PolicyGraph.h"
#include "RandSource.h"

// This class will maintain an internal belief and specify the next
// action given the current observation
class Controller: public ControllerInterface
{
  public:
    /**
       @param[in] policy The policy graph the Controller will work on
       @param[in] model The model of the problem
       @param[in] belief The initial belief
       @param[in] randSource The stream of randomness
       @param[in] trackBelief Whether we wants the Controller to track our belief
     */
    Controller(PolicyGraph& policy, Model& model, Belief* belief, RandSource* randSource, bool trackBelief = false);

    /**
       We use Particle Filter to track the belief in this case

       @param[in] policy The policy graph the Controller will work on
       @param[in] model The model of the problem
       @param[in] obs The initial observation
       @param[in] numNextBeliefStreams How many particles in the belief
       @param[in] maxMacroActLength How long can a macro action runs
       @param[in] randSource The stream of randomness
       @param[in] trackBelief Whether we wants the Controller to track our belief
     */
    Controller(PolicyGraph& policy, Model& model, Obs& rootObs, int numNextBeliefStreams, int maxMacroActLength, RandSource* randSource, bool trackBelief = false);

    /**
       @param[in] obs The observation
       @param[] please ignore this, this is for the APPL package

       Return the action given an observation
    */
    ActionDefine nextAction(ObsDefine obs, int dummy = -1);
    /**
       Return the current belief
    */
    BeliefDefine currBelief() const;

  private:
    void init(Model& model, Belief* belief, RandSource* randSource, int numNextBeliefStreams, int maxMacroActLength);

    PolicyGraph& policy;
    PolicyGraph::Node *currGraphNode;
    Belief* currBel;
    bool firstAction, trackBelief, staleBelief;
};

#endif
