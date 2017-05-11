// Files included
#include "Input.h"
#include "SolverInterface.h"
#include "Timer.h"


/*============================================================================//
    SolverInterface.cpp: Implementation of SolverInterface.

    Implementation: Shane J. Neph, June 2004, University of Washington

    Special Note:  Using "ulimit -t" option below for system call to solver.
      The time limit works as expected with the exception that the return
      value does not indicate a timeout (WIFSIGNALED() returns false).  There
      does not appear to be signal handlers which ignore the timeouts and return 
      with a special exit code either.  Not sure what's going on with that.  I
      inserted an instance of a Timer to catch/enforce timeouts so everything
      works as needed (the timeout *does* actually occur in the system call).
//============================================================================*/

void SolverInterface::checkRtn(int rtn) {
    if ( WIFSIGNALED(rtn) ) {
        int tmpRtn = WTERMSIG(rtn);
        Assert<CE::UserAbort>(tmpRtn != SIGQUIT, "user abort");
        Assert<CE::UserAbort>(tmpRtn != SIGINT, "user abort");
        std::string err = "memory or time limit exceeded";
        Assert<CE::TimeOut>(tmpRtn != SIGXCPU, "timeout");
        Assert<CE::MemoryLimit>(tmpRtn != SIGXFSZ, "memory exhausted");
        Assert<CE::MemoryLimit>(tmpRtn != SIGSEGV, "memory exhausted");
        err = convert<std::string>(tmpRtn);
        err = "Unchecked Signal Generated (# " + err + ") ...aborting";
        Assert<AbortedMission>(false, err);
    }
}

//=============
// RunSolver()
//=============
void SolverInterface::RunSolver(const Input& input) {
    toRemove_ = cleanFiles(); // get cleanup information: used upon destruction
    RenameSolution(""); // remove any file left from previous run

    // Define 'toRun' --> call to SAT solver
    std::string toRun;
    if ( input.ProgramOptions().empty() )
        toRun = getExeNameAndArgs(input.ExeDirectory(), input.CNF(), 
                                  input.Rand());
    else
        toRun = getExeNameAndArgs(input.ExeDirectory(), input.CNF(), 
                                  input.ProgramOptions());

    std::string time = "ulimit -t ";
    time += convert<std::string>(input.LevelTimeOut());
    toRun = time + std::string(";") + toRun;

    // Run solver and check status information
    Timer timer;
    timer.Start();
    int rtn = std::system(toRun.c_str());
    checkRtn(rtn);
    timer.Stop();
    Assert<CE::TimeOut>(timer.UserTime() <= input.LevelTimeOut(), "timeout");

    // Let solver evaluate return value
    bool isOK = evaluateReturn(WEXITSTATUS(rtn));
    Assert<AbortedMission>(isOK, "Aborted Solver");
}

