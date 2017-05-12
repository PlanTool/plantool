#ifndef __MODEL_H
#define __MODEL_H

#include <cfloat>
#include <vector>
#include "State.h"
#include "Utils.h"

class Belief;
class Action;
class Obs;
class RandStream;

const long InitMacroActState = 0; // initial controller state for macro actions
// must take the value InitMacroActState

enum obsType{TermObs=-1, LoopObs, OtherObs}; // observation types

/**
   @class Model
   @brief Base class for MDP models.
   @details Extend this class to implement a new model.
   See examples in the problems subdirectory.
   States are represented as a vector of doubles.
   Observations are represented as a vector of integers.

   The following needs to be provided or implemented:
   - Discount factor
   - Number of actions, number of macro actions and number of initial policies.
   - Number of state variables, observation variables.
   - A sampler for actions: implements the basic dynamics of the model
   - A sampler for macro actions: uses basic actions to create abstract actions
   - A sampler for initial policies: initializes the solver and provides
   robust fallback.
   - Upper bound for the value of the state: heuristic to help search more
   efficiently, the tighter the bound the better
   - Maximum, minimum of reward in any state
   - Function that receives an observation and returns whether it is in
   a macroaction loop and to set the value of observations to indicate it
   is in a loop
   - Function to indicate whether a state is a terminal state
   - A function restricting allowable actions given an observation: allows
   search to be restricted based on observations
   - A function to group observations together, so that search for policies
   are restricted to within an observation group to improve efficiency

   Take note of the following global definitions that are defined in this file.
   - typedef std::vector<double> State; // shorthand for defining state
   - const long InitMacroActState = 0; // initial controller state for macro actions must take the value InitMacroActState
   - enum obsType{LoopObs, TermObs, OtherObs}; // observation types
   - enum actType{initial, macro, act}; // types of actions

   @author Wee Sun Lee
   @date 8 July 2009,
   updated 25 October 2009

*/
class Model
{
  public:

    /**
       Provides initialization for all parameters.
       @param[in] numStateVar Number of state variables
       @param[in] numObsVar Number of observation variables.
       @param[in] numActs Number of possible actions.
       @param[in] numMacroActs Number of possible macro actions.
       @param[in] numInitPolicies Number of initial policies
       @param[in] discount Discount factor for the MDP problem.
    */
    Model(long numStateVar, long numObsVar, long numActs, long numMacroActs,
          long numInitPolicies, double discount):
            numStateVar(numStateVar), numObsVar(numObsVar),
            numActs(numActs), numMacroActs(numMacroActs),
            numInitPolicies(numInitPolicies),
            discount(discount)
    {}

    /**
       Return the initial belief
    */
    virtual State sampleInitState() const { return State(getNumStateVar(), 0); };

    /**
       Restricts allowable actions given an observation
       Override this function if restriction is required.
    */
    virtual bool allowableAct(Belief const& belief, Action const& act) = 0;

    /**
       Action sampler.
       @return Reward of doing \a act in \a currState.
       @param[in] currState Current state
       @param[in] act Index of action to perform
       @param[out] nextState Sampled next state. This is random and changes from invocation to invocation.
       @param[out] obs Observation
       @param[in] randSource Source of random numbers
    */
    virtual double sample(State const& currState, Action const& act, State* nextState, Obs* obs, RandStream* randStream) = 0;

    /**
       Macro action sampler. Macro actions are called at every time step just
       like actions but they may (or may not) have internal controller states.
       When they are initially called, the internal state is set by the solver
       to \a InitMacroActionState. Internal states can be maintained
       internally, or can be passed to the next time the routine is called
       through \a nextControllerState (or both). Observation \a obs is used to
       indicate whether the macro action has terminated, through \a getObsType.

       @return Reward of doing \a macroAct in \a currState and \a controllerState
       @param[in] currState Current state.
       @param[in] macroAct Index of macro action to perform.
       @param[in] controllerState Resume macro action from this controller
       state. Macro action should be written to start from
       controllerState value \a InitMacroActionState and should be
       indexed relative to it.
       @param[out] nextState Sampled next state.
       @param[out] nextControllerState Controller transitions to this state. To be used for the next controller action.
       @param[out] obs Observation
       @param[in] randSource Source of random numbers
    */
    virtual double sample(State const& currState, Action const& macroAct, long controllerState, State* nextState, long* nextControllerState, Obs* obs, RandStream* randStream) = 0;

    /**
       Initial policy sampler. These are the initial policies when the solver
       starts solving. They should be somewhat robust as the controller will
       default to initial policies eventually. Also, when an the controller
       sees an observation it has never seen before from the particular
       controller state, it defaults to the initial policy with
       \a policyIndex 0, so that particular policy should be particularly
       robust. As in macro actions, initial policies may have internal states,
       maintained internally or passed to the next time the routine is called
       using \a nextControllerState. Unlike macro actions, initial actions
       keeps running forever, so there is no observation to indicate that it
       has terminated.

       @return Reward of doing policy \a policyIndex in \a currState
       and \a controllerState
       @param[in] currState Current state.
       @param[in] policyIndex Index of initial policy to perform.
       @param[in] controllerState Resume policy from this controller
       state. Initial policies should be written to start from
       controllerState value \a InitMacroActionState and should be
       indexed relative to it.
       @param[out] nextState Sampled next state.
       @param[out] nextControllerState Controller transitions to this state. To be used for the next controller action.
       @param[in] randSource Source of random numbers
    */
    virtual double initPolicy(State const& currState, Action const& initAction, long controllerState, State* nextState, long* nextControllerState, Obs* obs, RandStream* randStream) = 0;

    /**
       Upper bound to the value of \a state. The value of a state is
       the expected sum of discounted reward of running a simulation from
       the state.
    */
    virtual double upperBound(State const& state) = 0;

    /**
     * Probability for seeing observation obs when arrived state nextState.
     */
    virtual double getObsProb(Action const& act, State const& nextState, Obs const& obs) = 0;

    /**
       Maximum value that the reward can take in any state.
    */
    virtual double getMaxReward() = 0;

    /**
       Minimum value that the reward can take in any state.
    */
    virtual double getMinReward() = 0;

    /**
       Returns the observation type: LoopObs, TermObs or OtherObs.
    */
    virtual obsType getObsType(Obs const& obs) = 0;

    /**
       Sets the type of the observation: LoopObs, TermObs or OtherObs.
    */
    virtual void setObsType(Obs* obs, obsType type) = 0;

    /**
       @return Whether this state a terminal state
    */
    virtual bool isTermState(State const& state) = 0;

    /**
       If implemented, this is used to restrict the search for policies during
       backup to within the observation group in order to speed up search.
       The assumption is that if an observation belongs to a different group,
       the partial policies within the other group are unlikely to be
       useful to this group.
    */
    virtual long getObsGrpFromObs(Obs const& obs) const { return 0; }

    /**
       @return The number of state variables
    */
    inline long getNumStateVar() const { return numStateVar; }

    /**
       @return The number of observation variables
    */
    inline long getNumObsVar() const { return numObsVar; }

    /**
       @return Discount value for the problem.
    */
    inline double getDiscount() const { return discount; }

    /**
       @return Number of base actions in the model.
    */
    inline long getNumActs() const { return numActs; }

    /**
       @return Number of macro actions in the model.
    */
    inline long getNumMacroActs() const { return numMacroActs; }

    /**
       @return Number of initial policies in the model
    */
    inline long getNumInitPolicies() const { return numInitPolicies; }

    /**
       This function object compares states and observations. Used in map.
    */

#if 0
    // HUY - ADD comparator for vector<double>
    class vecDoubleComparator {
      public:

        bool operator()(std::vector<double> const& v1, std::vector<double> const& v2) const
        {
            for (size_t i = 0; i < v1.size(); i++){
                // HUY
                if (v1[i] < v2[i] - 1e-5) return true;
                if (v2[i] < v1[i] - 1e-5) return false;
            }
            return false;
        };
    };
#endif

    /**
       Destructor
    */
    virtual ~Model() {}

  private:
    long numStateVar; // number of state variables
    long numObsVar; // number of obseration variables
    long numActs; // number of actions
    long numMacroActs; // number of macroactions
    long numInitPolicies; //number of initial policies

  protected:
    double discount; // discount factor for MDP

};

#endif // __MODEL_H
