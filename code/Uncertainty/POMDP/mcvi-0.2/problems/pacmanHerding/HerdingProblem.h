
#ifndef __HERDINGPROBLEM_H
#define __HERDINGPROBLEM_H

#include "State.h"

/**
   Constants and types used in pacman model. Take care in making any changes
   as they affect most of the pacman code.
*/
const double Caught = 10; // Reward for catching ghost
const double MovementCost = 0; // Cost of any other actions
enum regionType{junction, nonjunction}; // Region type
enum stateVars{a1x, a1y, a2x, a2y, gx, gy}; // State variable names
enum directions {east, south, west, north, unchanged}; // Action names
const long RelativeDirX[] = {1, 0, -1, 0, 0}; // Relative x movement for each aciton
const long RelativeDirY[] = {0, -1, 0, 1, 0}; // Relative y movement for each aciton

const long NumActsPerAgent = 5; // Number of actions for each agent
// Number of possible actions (two agents).
const long NumActs = 25;

// Same as actions but repeat action till macro states change.
const long NumMacroActs = 25;
const long NumInitPolicies = 1;
const long GhostCtQuant = 5;
const long GhostCtLevels = 6;
const long TermState = -1; // indicates state is at terminal state

/**
   @brief This structure is used to read in parameters to be passed to HerdingModel.
   @param discount Discount factor for MDP
   @param xSize Horizontal dimension of grid.
   @param ySize Vertical dimension of grid. The bottom left corner of the grid
   is (0,0) while the top right corner is (\a xSize-1, \a ySize-1)
   @param initState Initial state. Vector of six dimension: x coord of agent 1,
   y coord of agent 1, x coord of agent 2, y coord of agent 2, x coord of ghost,
   y coord of ghost.
   @param grid Each region is represented by an integer. The wall is represented by
   zero and not considered a region. Every intersection must form its own region.
   Every region is only one cell wide and must be terminated by intersections on
   both ends. See example files.
   @param numRegionPerAgent Number of regions in the grid, excluding walls.
   @param numMacroStateGrps Total number of macro states for the problem. Macro states encodes the regions the agents are in. If there are 10 regions,
   (agent 1 region, agent 2 region, ghost region), we will get 1000 macro states.

   - Five possible actions: move in 4 directions or stay in place.
   - If move into a wall, agent will stay in place.
   - Ghost currently moves randomly, but not allowed to move to position where an
   agent currently occupy.
   - Action encoding used is agent_1_action * NumActsPerAgent + agent_2_action
   - Actions have no additional observations.
   - Macro actions all repeat the same action until termination conditions
   are met.
*/
struct HerdingProblem
{
    double discount;
    long xSize;
    long ySize;
    long numGhosts;
    long numStateVars;
    State initState;
    std::vector<std::vector <long> > grid;
    long numRegionPerAgent;
    double probRandom;
    long numObsVars;
};

#endif // __HERDINGPROBLEM_H
