FPG
===

Contents:

1. Overview
2. Installation
3. Contributors


1. Overview
-----------

The Factored Policy-Gradient planner (FPG) is a probabilistic
planner. It views the planning problem as an optimization problem and
solves it using a stochastic gradient ascent (through the OLpomdp
algorithm). The search is performed directly in policy space, the
policy being "factored" with one sub-policy (in the form of a linear
network) per action.

This version of FPG currently handles:
 - concurrent actions (several actions can be triggered
 simultaneously),
 - discrete resources, and
 - uncertain task durations (the model providing a probability
 distribution over real values).


2. Installation
---------------

2.1 Dependencies:

    REQUIRED:
    qt4:
    libpg: Library providing policy-gradient algorithms
           (comes with its own dependencies)

2.2 Instructions:

2.2.1 Go to the source/planner/ directory
2.2.2 Copy Makefile.default to Makefile
2.2.3 Edit Makefile to set appropriate paths and defines for optional packages.
2.2.4 Go to the (source/planner/)standAlone/ directory
2.2.5 Copy Makefile.default to Makefile
2.2.6 Edit Makefile to set appropriate paths and defines for optional packages.
2.2.7 Run make to compile FPG (will have the name "brazilplan")
2.2.8 Run FPG on an airline problem with:
      ./brazilplan ../../../examples/caseStudies/airline.xml



3. Contributors
---------------

Project leader, FPG: Douglas Aberdeen (doug.aberdeen@gmail.com)
Brazil interface:    Owen Thomas (owen.thomas@nicta.com.au)
Debugging:	     Olivier Buffet (olivier.buffet@loria.fr)
