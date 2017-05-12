// Macro guard
#ifndef SATZRAND_SOLVER_CSE473_H
#define SATZRAND_SOLVER_CSE473_H

// Files included
#include "SolverInterface.h"


/*============================================================================//
    SatzRandSolver.h: header file - implementation of SolverInterface for
     satz-rand.

    Implementation: Shane J. Neph, June 2004, University of Washington
//============================================================================*/

struct SatzRandSolver : public SolverInterface {
    static std::string ExeName()
        { return("satz-rand"); }

    static std::string Name()
        { return("satz-rand"); }

    SatzRandSolver() : tmpFile_("satzrand.stats"), results_("satzrand.results")
        { /* */ }

    virtual ~SatzRandSolver()
        { /* */ }

private:
    virtual std::vector<std::string> cleanFiles() const {
        std::vector<std::string> toRtn;
        toRtn.push_back("record");
        toRtn.push_back(tmpFile_);
        toRtn.push_back(getSolutionName());
        return(toRtn);
    }
    virtual bool evaluateReturn(int rtnVal);
    virtual std::string exe() const { return(ExeName() + " "); }
    virtual std::string getExeNameAndArgs(const std::string& dir,
                                          const std::string& fileName,
                                          long rand) {
        std::string toRtn = exe() + fileName;
        toRtn += std::string(" -seed ") + convert<std::string>(rand);
        toRtn += " -out " + getSolutionName() + " >> ";
        toRtn += tmpFile_;
        return(dir + toRtn);
    }
    virtual std::string getExeNameAndArgs(const std::string& dir,
                                          const std::string& wffFileName,
                                          const std::string& options) {
        std::string toRtn = exe() + wffFileName;
        toRtn += (" " + options);
        toRtn += (" -out " + getSolutionName() + " >> ");
        toRtn += tmpFile_;
        return(dir + toRtn);
    }
    virtual std::string getSolutionName() const { return(results_); }
    virtual bool isSat(const std::vector<std::string>& vec);
    virtual void listOptions(std::ostream& os, const std::string& dir);
    virtual std::string name() const { return(Name()); }
    virtual void setUnique(const std::string& original) {
        tmpFile_ += original;
        results_ += original;
    }

private:
    std::string tmpFile_, results_;
};


#endif // SATZRAND_SOLVER_CSE473_H
