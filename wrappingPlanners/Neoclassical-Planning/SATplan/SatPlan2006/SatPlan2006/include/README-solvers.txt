09/02/04, Shane J. Neph, University of Washington
====================================================================================================
You can add a new solver to satplan's arsenal by creating a perl script wrapper for it.
Alternatively, you could add the solver to satplan itself (assuming you have the source files) in 
C++.

For C++:
Inherit and implement the SolverInterface.h protected interface.  For the exact details on how to
do that, look at some of the solvers already implemented (as well as comments, of course).  Also, 
you can gain some insight from reading the perl section below.  I won't provide much detail
here, but you will need to make sure to make both a header and implementation file for your solver.
Your solver will need to register itself with the generic factory in its implementation file -see
'Modern C++ Design' by Andrei Alexandrescu (Addison Wesley) for a description of how the factory 
works in general.
You will need to add your implementation file to the appropriate 'Makefile' so it can be compiled
and linked in.

For either C++ or perl, you will need to place your binary or script in the include/bin directory
====================================================================================================




================================================================================
The requirements for a solver wrapped in a perl script (script) are as follows:


(1) You are responsible for calling the underlying binary solver and formatting
     the output as outlined below.

(2) Both the script and the underlying binary must exist in the 
     /include/bin/ directory.

(3) Make sure the script cleans up any temporary files that are generated, as
    files left over from previous runs could cause bad outputs later!

(4) We will pass the script arguments in the following order:
    (a) CNF problem file that your solver should try to solve
    (b) The directory where you script (and underlying binary) are located.
    (c) File name to output your final results to.
    (d) Any remaining arguments will be specific options specified by the user
         directed at your specific solver.  satplan will not perform any error
         handling on these parameters, but will pass them directly to your
         script.  The user can do this by using the -options [ X ] option at
         the command line.  You can choose to use these (or ignore them) as
         you wish.

(5) The final solution file must contain the following information upon SAT:
     The word SATISFIABLE in capital letters followed by at least 1 newline,
      and then the solution: a list of distinct integers with any number of  
      spaces and newlines separating the integers.  Further, (ignoring sign) 
      the number of and values of the integers must exactly match the 
      variables in the CNF problem file.  The listed integers should be 
      the asserted values in your solution only.  For example, if your
      solver concludes that -2 should be asserted and 2 should be given
      the value of zero, then you should use -2 in the final solution file,
      and 2 should not be used.

     Example (in file named as the third argument passed to your script):
 
     SATISFIABLE
     -1  -2  3  -4  5  6  7
     8  9  -10

    In reality, satplan will allow you to have information above and below 
     these requirements.  This may be dangerous if you have the words
     SATISFIABLE, UNSATISFIABLE or ERROR anywhere else though - I recommend
     against any other information - the resulting file will be deleted by
     satplan anyway.

(6) The final solution file must contain the following information upon UNSAT:
    The word UNSATISFIABLE in capital letters followed by any number of 
     newlines or spaces.

     Example (in file named as the third argument passed to your script):
    
    UNSATISFIABLE

(7) The final solution file must contain the following information upon any 
    error encountered (solver abort, bad user input that is not ignored, etc.):
     The word ERROR in capital letters followed by any number of
     newlines or spaces.

     Example (in file named as the third argument passed to your script):
  
     ERROR

(8) Upon any error, in addition to (7), use "exit 2" to leave the script.

(9) Upon SATISFIABLE and UNSATISFIABLE, return with value 0 or 1 respectively.


============
Other Items:
(1) The value of -seed is not passed to your scripted solver when used as an
     option to satplan.  The user has the option of using -options [ X ] and 
     passing a seed to your solver directly if your solver allows for such
     input.  The -seed option is ignored when using a "general solver".

(2) The user should input the exact name of the perl script file following the
    -solver option.  Example:

     -solver SiegeWrapper.pl

(3) We check to ensure that the number of variables in the WFF matches the
     number of variable assignments in any arbitrary solver's solution.  This
     check may take up to a couple of seconds on very large problems.  So,
     an arbitrary solver wrapped in a perl script will be slightly slower
     than a "built in" satplan solver on very large problems.

============
Examples:
(1) SiegeWrapper.pl
  siege is already a built-in solver available from the command line, but
   SiegeWrapper.pl was also made for illustrative purposes.  siege_v4 creates
   a file called siege.results.  That file does not conform to the requirements
   here, so SiegeWrapper.pl takes siege.results and produces a file named
   as the 3rd arg passed in, which does conform to these requirements.  The 
   script also cleans up by deleting siege.results before returning.

   -solver siege // option uses built-in call to siege_v4 executable

   -solver SiegeWrapper.pl // option uses script to call siege_v4 executable

================================================================================
