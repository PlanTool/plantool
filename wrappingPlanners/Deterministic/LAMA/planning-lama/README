LAMA 

Version: May 4, 2011 (updated translator compared to Jan and Aug 2010
and April 2010, bug fix in FF heuristic since Dec 2010)

License:
LAMA was written by Silvia Richter and Matthias Westphal,
copyright (c) 2008 NICTA and Matthias Westphal.
Distributed under the GNU General Public License (GPL,
see separate licence file).
This program uses source code of the Fast Downward planner,
(c) 2003-2004 by Malte Helmert and Silvia Richter. Permission
to redistribute this code under the terms of the GPL has been
granted.

Note of Caution: Please be aware that LAMA has bugs. In particular the
component responsible for parsing and translating the PDDL input has
known problems. However, for all classical (i.e. non-numerical, no
preferences etc.) tasks from past international planning competitions,
formulations exist that LAMA can parse. E.g.: for Airport, use the
STRIPS formulation. For Freecell (IPC 2002), the untyped variant. For
Philosophers, Optical Telegraph and PSR, use the ADL variant with
derived predicates. For Mprime and Assembly, domain files are included
in this distribution in the "bench-patch" directory which correct
known bugs of the competition files (Mprime) or features not supported
by LAMA (Assembly).

If you encounter further problems, please email me at
silvia.richter@nicta.com.au. The lama/translate directory also
contains a patch that can be applied to skip the invariant generation
step in the translator component. This eliminates many problems and
may be helpful e.g. if you are interested in using the framework of
LAMA with a heuristic other than the landmark heuristic.

Lastly, I would be happy to know if you are using LAMA. Just drop me a
short email at the above address. Then I can also inform you about bug
fixes and new versions.

The following description is adapted from the Fast Downward README
(thanks to Malte Helmert).

Structure
=========

LAMA runs in three separate phases: translation, knowledge
compilation, and search. These are partitioned into three separate
programs.  The three directories "translate", "preprocess" and
"search" contain these programs.

Documentation
=============

1. A comprehensive description of LAMA can be found in the JAIR
   article "The LAMA Planner: Guiding Cost-Based Anytime Planning with
   Landmarks" by Silvia Richter and Matthias Westphal (2010), which is
   included in the "doc" directory under "lama-jair10.pdf". A brief
   description of the planner is furthermore given in "lama-short.pdf"
   in the same directory.

2. The AIJ article "Concise finite-domain representations for PDDL
   planning tasks" by Malte Helmert (2009) describes the translation
   component in detail.  The description is somewhat idealized, as the
   actual implementation has some limitations in dealing with some ADL
   features. Still, the article provides a fairly good description of
   what the translator does (or should do, at any rate).

3. "sas-format.txt" in the "doc" directory is a description of the
   translator output format.  You will only need this if you want to
   use SAS+ tasks/multi-valued planning tasks within your own planner.

4. "pre-format.txt" in the "doc" directory is a description of the
   output format of the knowledge compilation component
   ("preprocess"). You will only need this if you want to use the
   preprocessed multi-valued planning task information within your own
   planner.

Build Instructions
==================

Parts of the planner are implemented in C++, and parts are implemented
in Python.

The C++ code was only tested under g++ and uses hash tables from the
original STL which are part of the g++ libraries but not of the C++
standard. So if you want to make the planner run under a different C++
environment, you will need to adjust the parts of the planner that use
these features.

Statically compiled executables are provided with this distribution.
To recompile the code on a standard Unix-ish system, just run "build"
in this directory to build the C++ components of the planner:

    # ./build

The executables will be placed in the respective directories. (If this
does not work, simply compile and link all the C++ source files you
find in each directory.)

The Python code is interpreted and thus does not need compilation.
However, you will need a Python interpreter, version 2.5 or newer, to
run it. If you are on a non-Unix system, you will probably need to
install Python by following the instructions at
http://www.python.org/. If you are on a Unix-ish system, check if the
correct version of Python is installed already by typing "python2.5"
in a shell. If this does not result in an error message, you are fine.
If you do get an error, but have a *newer* version of Python installed
(such as Python 2.5), you can also run the translator under this newer
version. In this case, you will need to change the first line of
translate/translate.py to match your system setup. The translator will
*not* work with Python 2.4 or older. If you only have an older version
of Python (or none) installed, go ahead and install a newer one with
your Unix system's package manager (e.g. with "apt-get install
python2.5" on a Debian system). Different versions of Python can
peacefully coexist, so this should not wreck your system setup.

Running the Planner
===================

To run the planner, you can use the script "plan" in this
directory with the following arguments:

   # ./plan <domain_path> <problem_path> <result_path>

   This run script contains the settings used for LAMA during IPC-6.
   For other possible arguments modify the run script accordingly.
   The planner understands four options which can be combined in any
   way:	

   l:  Use the landmark heuristic.
   L:  Use preferred operators of the landmark heuristic.
   f:  Use the FF heuristic.
   F:  Use helpful actions ("preferred operators" of the FF
       heuristic).

   At least one heuristic ("l" or "f") must be used; on average, using
   all features of the planner ("lLfF") appears to produce the best
   results.

   The planner can be told to run in iterated mode (keep searching for
   better solutions) using option "i". Note that in this case, the
   planner may never stop (unless it can exhaust the search space) and
   needs to be aborted at some point.
   
   By default, the first solution is found using best-first search,
   later search iterations use weighted A*. In order to use
   weighted A* for the first iteration as well, use option "w".

Or, you can run the three steps of LAMA separately (e.g. if you want
to re-use output from earlier translation/preprocessing steps):

First, run

   # translate/translate.py domain.pddl problem.pddl

   The translator will will write its result to a file called
   "output.sas", which serves as an input to the next phase, knowledge
   compilation. The translator also writes a file called
   "test.groups", which is some sort of translation key (see
   "sas-format.txt" in the documentation directory mentioned above).
   This second file is not needed by the planner, but might help you
   understand what the translated task looks like. It also writes a
   file called "all.groups" which is needed by the landmark heuristic.

Second, run:

   # preprocess/preprocess < output.sas

   This will run the knowledge compilation component, writing its
   output to the file aptly named "output".

Finally, run:

   # search/search <args>  < output

   This runs the search component of the planner. On success, it will
   write a file called "sas_plan" containing the plan.

Questions and Feedback
======================

Please feel free to e-mail us at silvia.richter@nicta.com.au if you
have any questions, encounter bugs, or would like to discuss any
issues regarding the planner.

Have fun,

Silvia Richter & Matthias Westphal
