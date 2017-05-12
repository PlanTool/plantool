Monte Carlo Value Iteration (MCVI) for POMDP
--------------------------------------------

MCVI is a C++ implementation of the MCVI algorithm [1]. It takes as
input a POMDP model (coded in C++) and produce a policy file. It also
contains a simple simulator for evaluating the quality of the computed
policy.

For bug reports and suggestions, please email <motion@comp.nus.edu.sg>.

[1] Monte Carlo Value Iteration for continuous-state
POMDPs. Proc. Int. Workshop on the Algorithmic Foundations of Robotics
(WAFR) (2011), pp. 175-191.

[2] Unmanned Aircraft Collision Avoidance using Continuous-State
POMDPs. Proc. Robotics: Science and Systems, 2011.

[3]Monte Carlo Value Iteration with Macro-Actions. Proc. Neural
Information Processing Systems (NIPS), 2011.

========================================================================
TABLE OF CONTENTS
========================================================================

* Requirements
* Quick start
* Package contents
* Notice


========================================================================
REQUIREMENTS
========================================================================

Operating systems:        Linux

Tested compilers:         gcc/g++ 4.1.2 under RedHat 5
                          gcc/g++ 4.4.6 under Ubuntu-Linux
                          gcc/g++ 4.5.3 under Ubuntu-Linux
                          gcc/g++ 4.6.1 under Ubuntu-Linux

* General
  For gcc/g++ tool chain, GNU make is required for building.

========================================================================
QUICK START
========================================================================

* At the top-level directory, type the commands:

    gunzip mcvi.tar.gz
    tar -xvf mcvi.tar
    cd mcvi/problems/<problem>
    make

A total of 2 executables are generated in the current directory.
  "Solver" computes a policy.
  "Simulator" is the policy simulator.

Please read the argument options for each problems. Below, we will run
corridorPorta as an example. Texts in quotes (`) are output of the
command.

- Try solving corridorPorta problem. Type the command:
    ./Solver

    `Usage:
        -o policyfile
        -p targetPrecision (default: 0.1)
        -t maxtime (default: 3600s)
        -l maxSimulLength (default: 100 steps)
        -b numRandomStreamsForBackUp (default: 100)
        -n numRandomStreamsForNextBelief (default: 100)
        -d discountFactor (default: 0.95)
        -i depthMultiplier (default: 0.95)
        -u useMacro (default: 1)
        -s randNumSeed (default: 0, uses time)
        -v displayInterval (default: 60)`

  So we need to supply policy file name for output

- Type:
    ./Solver -o policy

- Try simulating a policy. Type:
    ./Simulator -o policy -n <number of times to run simulation>

Most of the examples will take more than 1 hour to converge, you can
set the duration by the "-t" option.

"Warning: no next belief" ===> See Notice

========================================================================
PROBLEM EXAMPLES
========================================================================

Each problem is located in a subdirectory of the "problems" directory.

To model a problem, you need to extend the Model class.
See examples in the problems directory. There are 5 toy problems now:
1/ CorridorDiscrete
2/ Corridor (continuous)
3/ CorridorPorta (from Porta et al., "Point-based value iteration for
continuous POMDPs", J. Machine Learning Research, 2006)
4/ Underwater (from Kurniawati et al., "SARSOP: Efficient point-based
POMDP planning by approximating optimally reachable belief spaces", Proc. Robotics:
Science & Systems, 2008)
5/ PacmanHerding (Collaborative Search and Capture, from Lim et al.,
"Monte Carlo Value Iteration with Macro-Actions", NIPS 2011)

The minimum configuration consists of a Model and a main function
which calls the Solver.


========================================================================
CREATE NEW PROBLEMS
========================================================================

To create a new problem, we need to extend the Model (Model.h) and
create a main function which calls Solver's functions (Solver.h). Take
a look at the functions which Solver provides. There are several
functions to provide convenience when dealing with simple problems as
well as functions that are required when more customizations are
needed.

Simulator (Simulator.h) provides two functions and you can cater to
your needs.

The simplest setting are illustrated in CorridorDiscrete problem when
we only provide a Model, a Solver which calls the `solve` function of
Solver, and a Simulator which only calculates the average (discounted)
reward.

========================================================================
CONTROLLER
========================================================================

The Controller class (Controller.h) provides a convenient abstraction to make
use of MCVI's policy to control external systems (for e.g. physical robots).
Given an observation from the system, it returns the corresponding control
action according to the policy provided to it.

An example of an implementation of a Controller is given in corridorDiscrete.

========================================================================
HOW TO COMPILE
========================================================================
There are 2 ways to compile:
1/ Go into the src/ folder and compile the MCVI into a library (in
build/) and use the library to compile your Solver, Simulator, and
Controller

2/ Include the common.mk file in the src/ folder, and set the
variables appropriately in your Makefile. See examples in the toy
problems.

========================================================================
PACKAGE CONTENTS
========================================================================

README.txt                      This file
src/Action                      Representation of an action
src/Obs                         Representation of observation
src/ActNode                     Representation of an Action node
src/ObsEdge                     Representation of an Observation edge
src/Particle                    Representation of a particle
src/State                       Representation of a state
src/Belief                      Functions of a Belief
src/BeliefNode                  Data storage for a Belief
src/BeliefTree                  A tree with one root node and several
                                belief nodes
src/BeliefForest                A collection of Belief Trees
src/BeliefSet                   A set of Beliefs with insertion method
                                to prevent duplications
src/ParticlesBelief             Implementation of particle belief
src/ParticlesBeliefSet          Implementation of particle belief set
src/PolicyGraph                 Representation of policy graph
src/Solver                      Simple Solver with necessary options
src/Simulator                   Simple Simulator with necessary
                                options
src/Controller                  Controller class which supports nextBelief(obs)
src/Utils                       Some simple utilities
src/common.mk                   Make file
src/ValueIteration              Simple value iteration (not used, for
                                testing purpose only)
src/History                     Representation of History (has not
                                been used, provided for interface
                                purpose)
problems/                       Sample problems

To modify the solver, you need to change the code in the src
directory. The structure is as follow:
+ Model
+ BeliefTree which will be searched on
+ Belief which represent a node in the tree
+ BeliefNode which store the data for a belief
+ ActNode which represent an action we take from a belief
+ ObsEdge is an edge of an ActNode which represent a probable
observation
+ PolicyGraph is the output graph

Other files are implementations of the above structures.

========================================================================
NOTICE
========================================================================

"Warning: no next belief" means that the program sampled a "bad"
observation and there is no probable next belief. This is not a
problem as the program will go back and regenerate the observations if
it runs out of next beliefs. You can also try to increase the number
of particles to see if that helps.

The function signatures maybe changed without notice. For example,
there are plans to change references to non-const objects to
pointer. Those changes will be documented and maintained consistently
over the files and examples.
