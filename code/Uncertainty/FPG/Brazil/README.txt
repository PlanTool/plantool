Brazil (& FPG)
==============

Contents:

1. Overview
2. Installation
3. Contributors


1. Overview
-----------

1.1 FPG:

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


1.2 Brazil:

Brazil is a graphical interface serving two main purposes:

 - helping the user to define probabilistic planning domains with
 resources and uncertain task durations, and

 - providing a way to visualize the planner's outcome through:

   - an interactive GANTT chart which shows the most probable
   execution by default, with the possibility for the user to see what
   happens if a task fails/succeeds or finishes earlier/later, and

   - simulation results shown on a graph (one point per execution)
   with the color indicating success (green) or failure (red), and the
   position depending on the duration/resource level/...

Brazil is here linked to FPG because it has been designed as a GUI for
FPG, but other planners could be used.


2. Installation
---------------

2.1 Dependencies:

    REQUIRED:
    qt4:
    libpg: Library providing policy-gradient algorithms
	   (comes with its own dependencies)

2.2 Installation (Brazil + FPG)

2.2.1 Go to the source/ directory
2.2.2 Copy  config.xml.default to config.xml
2.2.3 Copy  Brazil.pro.default to Brazil.pro
2.2.4 Edit Brazil.pro to set appropriate paths and defines for optional packages.
2.2.5 Run qmake
2.2.6 Run make to compile "Brazil"
2.2.7 Run ./Brazil (example files can be found in Brazil/examples/caseStudies/ )

2.3 Installation (FPG alone)

2.3.1 Go to the source/planner/ directory
2.3.2 Copy Makefile.default to Makefile
2.3.3 Edit Makefile to set appropriate paths and defines for optional packages.
2.3.4 Go to the (source/planner/)standAlone/ directory
2.3.5 Copy Makefile.default to Makefile
2.3.6 Edit Makefile to set appropriate paths and defines for optional packages.
2.3.7 Run make to compile "fpg"
2.3.8 Run FPG on an airline problem with:
      ./fpg ../../../examples/caseStudies/airline.xml



3. Contributors
---------------

Project leader, FPG: Douglas Aberdeen (doug.aberdeen@gmail.com)
Brazil interface:    Owen Thomas (owen.thomas@nicta.com.au)
Bug detection:	     Olivier Buffet (olivier.buffet@loria.fr)
