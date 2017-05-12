// Macro guard
#ifndef GENERAL_SOLVER_CSE473_H
#define GENERAL_SOLVER_CSE473_H

// Files included
#include "SolverInterface.h"
#include "StringAlgorithms.h"


/*============================================================================//
    GeneralSolver.h: header file - implementation of SolverInterface for
     a general solver, wrapped in a perl script, complying to the rules set up
     by SatPlan.

    Implementation: Shane J. Neph, June 2004, University of Washington
//============================================================================*/

struct GeneralSolver : public SolverInterface {
    static std::string Name()
        { return("internaluse_generalsolver"); }

    GeneralSolver() : tmpFile_("generalsolver.stats"), exeName_(""),
                      results_("generalsolver.results"), numVars_(0) { /* */ }
    virtual ~GeneralSolver()
        { /* */ }

private:
    virtual std::vector<std::string> cleanFiles() const {
        std::vector<std::string> toRtn;
        toRtn.push_back(getTempFile());
        toRtn.push_back(getSolutionName());
        return(toRtn);
    }
    virtual bool evaluateReturn(int rtnVal);
    virtual std::string exe() const { return(exeName_); }
    virtual std::string getExeNameAndArgs(const std::string& dir,
                                          const std::string& wffFileName,
                                          long rand) {
        numVars_ = getNumberVariables(wffFileName);

        /* rand ignored with general (unknown) solvers */
        std::string toRtn = exe() + " " + wffFileName + " " + dir;
        toRtn += (" " + getTempFile());
        return("perl " + dir + toRtn);
    }
    virtual std::string getExeNameAndArgs(const std::string& dir,
                                          const std::string& wffFileName,
                                          const std::string& options) {
        numVars_ = getNumberVariables(wffFileName);

        std::string toRtn = exe() + " " + wffFileName + " " + dir;
        toRtn += (getTempFile() + " " + options);
        return("perl " + dir + toRtn);
    }
    virtual std::string getSolutionName() const { 
        return(results_); 
    }
    virtual bool isSat(const std::vector<std::string>& vec);
    virtual void listOptions(std::ostream& os);
    virtual std::string name() const { return(Name()); }
    virtual int runSolver(const std::string& exe) {
        std::string toRun = "perl " + exe; // script
        int rtn = std::system(toRun.c_str()); // run solver
        return(WEXITSTATUS(rtn));
    }
    virtual void setExe(const std::string& newExe) { exeName_ = newExe; }
    virtual void setUnique(const std::string& original) {
        tmpFile_ += original;
        results_ += original;
    }

private:
    long getNumberVariables(const std::string&);
    std::string getTempFile() const {
        return(tmpFile_);
    }

private:
    std::string tmpFile_, exeName_, results_;
    long numVars_;
};


#endif // GENERAL_SOLVER_CSE473_H
