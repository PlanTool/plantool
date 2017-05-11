// Macro guard
#ifndef MINISAT_SOLVER_CSE473_H
#define MINISAT_SOLVER_CSE473_H

// Files included
#include "SolverInterface.h"


/*============================================================================//
    MiniSat.h: header file - implementation of SolverInterface for
     minisat.

    Implementation: Shane J. Neph, Feb 2006, University of Washington
//============================================================================*/

struct MiniSatSolver : public SolverInterface {
    static std::string ExeName()
        { return("SatELiteGTI"); }

    static std::string Name()
        { return("minisat"); }

    MiniSatSolver() : tmpFile_("minisolver.stats"),
                      results_("minisolver.results")
        { /* */ }

    virtual ~MiniSatSolver()
        { /* */ }

private:
    virtual std::vector<std::string> cleanFiles() const {
        std::vector<std::string> toRtn;
        toRtn.push_back(getTempFile());
        toRtn.push_back(getSolutionName());
        return(toRtn);
    }
    virtual bool evaluateReturn(int rtnVal);
    virtual std::string exe() const { return(ExeName() + " "); }
    virtual std::string getExeNameAndArgs(const std::string& dir,
                                          const std::string& wffFileName,
                                          long rand) {
        std::string toRtn = exe() + " " + wffFileName;
        toRtn += (" > " + getTempFile());
        return(dir + toRtn);
    }
    virtual std::string getExeNameAndArgs(const std::string& dir,
                                          const std::string& wffFileName,
                                          const std::string& options) {
        std::string toRtn = exe() + " " + wffFileName;
        toRtn += (" " + options);
        toRtn += (" > " + getTempFile());
        return(dir + toRtn);
    }
    virtual std::string getSolutionName() const { 
        return(results_); 
    }
    virtual bool isSat(const std::vector<std::string>& vec);
    virtual void listOptions(std::ostream& os, const std::string& dir);
    virtual std::string name() const { return(Name()); }
    virtual void setUnique(const std::string& original) {
        tmpFile_ += original;
        results_ += original;
    }

private:
    std::string getTempFile() const {
        return(tmpFile_);
    }

private:
    std::string tmpFile_, results_;
};


#endif // MINISAT_SOLVER_CSE473_H
