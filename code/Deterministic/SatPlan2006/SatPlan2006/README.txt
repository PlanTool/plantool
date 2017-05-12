================================================================================
Last Edited April 23, 2006 by Shane J. Neph, University of Washington

-We assume a directory structure where the satplan source code is located in  
 folder 'include'.  We assume one subfolder in include: bin.  'include' is
 located below the folder containing this README.txt and the top-level Makefile.
 
 This structure is only important to the top-level Makefile.  You can change it
 [the structure] however you like so long as you update that Makefile.


[How to get started:]
-Open the top-level Makefile and set the BIN variable to the directory where
 you would like the satplan binary (and others) placed.
-Type 'make install' at the command line.
-Make sure you look over the 'License Issues' section below for potential
 hiccups.


[Quick info about satplan:]
-Shows user input requirements upon any bad input (including no input args)
-Requires 2 arguments always (<problem> & <domain>)
-Has a bunch of optional inputs.  All input types are listed upon bad input.
-Some of the potentially unobvious inputs and gotchas include:
  -cnf 
    use when you want to keep the WFF your solver takes a crack at.  We
    also place a lot of information here: time, date, solver used, rev,
    variable encodings, etc.  The file is placed in your current working
    directory.
  -encoding
    can take values 1 through 4 with 4 being the default.  1 was used during
    the 2004 competition; 4 was used in 2006.
    1 = action-based
    2 = gpstyle-action-based
    3 = gp-based
    4 = thin-bp-based

    Different encodings combined with different solvers results can result
    in a large time range to solve a particular problem.
  -level
    try to find solution using this plan level only - no more, no fewer
  -listoptions 
    lists all detailed input options for a specific solver, as specified by 
    -solver.  These options are likely to go well beyond the options offered by 
    satplan itself.  The options are specific to the chosen -solver.  This 
    option is not useful when using arbitrary solvers wrapped in perl scripts.
  -listsolvers
    displays satplan Rev and all registered -solver's at your disposal.
    Note that arbitrary solvers wrapped in perl scripts are not listed 
    here, but are also available to you.
  -path
    allows you to specify where the <domain> and <problem> files are located.
    If unspecified, the current working directory is used.  If the current
    working directory does not work out, then the PATH environmental variable
    is explored.  -domain and -problem is each considered independently relative
    to either the current working directory or to PATH as the case may be.
    When -path is specified, then each of -domain and -path is considered
    relative to where -path leaves off.
  -options [ ... ] 
    place detailed options you want to send directly to your chosen solver, 
    specified by -solver, in between the [ ]'s.  This is closely related to 
    the options listed when -listoptions is used except that it is not 
    constrained to only built-in solvers.  You are welcome to pass extra
    information to a general solver wrapped in a perl script using this option.
  -solution 
    You can change the name of the solution file to whatever you like with this 
    option.  The file is placed in the current working directory.  
  -timeout
    How long should a solver spend on a given CNF file with the given rand()
    number?  Upon timing out, another rand() will be chosen. This will occur up
    to -restarts times.  After all -restarts are exhausted, satplan will
    increase the plan level by 1 (assuming -level is not used, in which case
    satplan would now exit; and assuming -globaltime hasn't been exceeded)

*make sure you read over "gotchas" and "License issues" below*


[defaults]
-When unspecified, the following defaults are used:
  -cnf CNF_ + pid# (removed by satplan if not manually overridden)
  -bcheck 0 (not very useful)
  -cnfonly 0
  -encoding 1 (may not be the best default)
  -globalmemory 1Gb
  -globaltime 30min
  -level (unused)
  -maxlevel 1000
  -options [ ] (unused)
  -path (current working directory then PATH on failure)
  -restart 1
  -seed 89
  -solution (-problem's value name + .soln at the end)
  -solver (default order: siege, minisat; berkmin561, jerusat1.3, then satz-rand)
  -timeout 1800s
  -verbose 1 (ON)

  There are also defaults defined for each -solver available.  The user always
  has the option of overriding all of these by using -options [ ... ] at the
  command prompt.  No user-input checking is performed by satplan on the 
  [ ... ] portion of -options when in use.  The user is directly specifying
  his/her chosen options to the SAT solver itself so satplan is taken out
  of the error checking loop.  The solver may ignore bad input or it may
  give a bad return value (or issue a signal).  In the latter case, satplan 
  will interpret this as a "Aborted Solver".  Keep in mind that options from 
  the satplan command line, such as -seed, which would normally be fed to the 
  sat solver are *not* used when -options [ ... ] is used --> you are really 
  talking directly to the solver and satplan provides no defaults.


[in regards to the -cnf option]
-When the -cnf option is used, several comments are added to the CNF file.
  These comments include all variable encodings from the bb executable (bb is
  the portion of satplan responsible for building the CNF encoding).  Also, 
  other various info is added to the specified WFF file - date, time, revisions, 
  user-input options, etc.  When -cnf is not used, the resulting CNF file is 
  deleted at the end of satplan.
 
  The siege_v4 SAT solver cannot deal with the extra comments in the specified
  CNF file.  For this reason, all comments are added to the specified file
  *after* the -solver had its crack at it.  siege will work fine in satplan, 
  only the final CNF file output would be unsuitable for siege.  To be very
  clear about this, siege is safe to use within satplan (there are no problems).
  It is a great solver, so specify it as your -solver if you want to.

  Clearly, adding all variable information to the WFF takes some time so
  satplan will run longer when -cnf is used.


[in regards to the -path, -domain and -problem options] 
 You can choose to ignore the -path option and define each of -domain and
 -problem using absolute or relative paths.  If unused, -path defaults to
 the current working directory.  Obviously, if domain and problem files
 are located in the current working directory, you can just specify the file
 names for those 2 inputs.

 The second choice is similar to the first.  You can define -path as a
 relative path to the current working directory, or as an absolute path.
 Then, -domain and -problem can be specified as relative paths from where 
 -path leaves off.

 You cannot specify -path and then specify -domain or -problem as absolute
 paths.  -path is prepended to the values of -domain and -problem.

 You can also set the PATH environmental variable instead of using -path.


[Maintenance:]
-There is a file in the include/ directory named SolverRevisions.txt.  As its 
 name suggests, this is where revision information for the solver binaries 
 should go.  It could have been hardcoded, but a new version of a solver could
 be released that fixes some obscure bug that has no affect on us.  Someone may
 not find the energy needed to open the source code and update it if the new
 solver works with the existing code.  I hope SolverRevisions.txt makes such 
 things a more-likely-to-be-updated feature for satplan.  

 In addition, if you decide to add a script wrapper to a new arbitrary solver, 
 then you can specify the revision of the underlying solver in this file.  For 
 example, if there was a perl script named SiegeWrapper.pl which called the 
 siege_v4 binary, then you could place the following in SolverRevisions.txt:
 
 SiegeWrapper.pl = 4.0

 When the -cnf option is used, the solver revision would be recorded.  Without
 this update to SolverRevisions.txt, you get "Unknown" for the solver revision.


[Licence Issues:]
-Some solvers may have licenses which prohibit us from re-distributing binaries.
 In these cases, notes will exist here on what you will need to do to get that
 solver on your machine so that satplan can use it.
 satplan itself can be used freely.  Its use is only restricted by the licenses
 of the underlying solvers called by satplan that you specify.

-One such solver is BerkMin561.  This is an excellent solver and it is 
 recommended that you do the following, assuming you will not be using satplan
 beyond the license agreement for BerMin561:
 -visit http://www-cad.eecs.berkeley.edu/~kenmcmil/smv/dld2.html
 -review the license agreement.  If you agree, then provide the necessary
  information and download the binary.
 -run the BerkMin561 program once *directly* (not through satplan) and, again,
  assuming you do agree, agree to the license agreement.  Now a file 
  license.txt will be constructed.
 -move license.txt and the binary to the include/bin/ subdirectory.  Make sure
  the binary is named: berkmin61-linux.  If not, then rename it.  That's it.

 Note: BerkMin561 is "compiled-into" satplan.  satplan is ready to call the
       binary if it exists.  If the binary does not exist on your system,
       there is no problem.  In this case, if you choose berkmin561 as your
       solver, satplan will tell you that the solver does not exist.  If you
       do not specify berkmin561, satplan will run as expected, with the 
       chosen solver or the default solver if unspecified.


[gotchas:]
-It is important that SolverRevisions.txt is up to date (see [Maintenance:]).

-Some solvers may be listed under the -listsolvers option that are not installed
 on your system (see [License issues:]).  If you try to use one of these ghost
 solvers, satplan will report a "No binary or script found" message only.  In
 this case, choose a different solver or get the binary.  If the default solver
 is used, satplan will ignore any solvers that do not exist on your system.

-If you download the BerkMin561 solver (see License Issues:) but do not move
 license.txt to the include/bin/ subdirectory as specified, when you specify that
 solver, the satplan program will hang when it calls berkmin561-linux.

-If a user chooses to use -options [ ... ] and does not use a seed value, or if 
 a solver does not take a random seed or does not use one internally, then the 
 -restarts option should be set to 1.  Otherwise, upon -timeout, you will run a 
 sat solver in the same exact initial state up to -restart times before moving 
 to the next plan level.

-siege is not safe to use under multiple users or multiple processes in the
 same wd.  If this safety is needed, use the -solver SiegeWrapper.pl option.
 We do not have the source code for siege, so we cannot fix it.

-siege will not work if the input cnf file name contains 5 or more consecutive
 digits.  For this reason, we convert the CNF_<pid>'s pid portion to ascii
 alphas (still as unique as the pid value).  Everything works fine, but you
 cannot call -solver siege and -cnf mycnfname11111_ok, or any other file name
 with 5 or more consecutive digits.  We do not have the source code, so we
 cannot fix this issue.


[errors:]
-On very large problems, some #define's in the code responsible for building
 the CNF encoding may need to be adjusted.  The messages given look something
 like "too many cnf vars. increase MAX_CNF_VARS" or 
 "too many clauses. increase MAX_CLAUSES."  There are several of items like 
 this.  If you decide to increase these #define's, they are located in the
 bb.h file in the include directory.  You would then need to re-make install.


[arbitrary solvers:]
-Information regarding how to add new solvers to satplan via perl scripts or to
 the main code directly is contained in README-solvers.txt, which is itself
 located in the include/ folder.
================================================================================
