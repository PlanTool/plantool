// Macro guard
#ifndef BERKMIN561_SOLVER_CSE473_H
#define BERKMIN561_SOLVER_CSE473_H

// Files included
#include "SolverInterface.h"


/*============================================================================//
    BerkMin561.h: header file - implementation of SolverInterface for
     BerkMin561.

    Implementation: Shane J. Neph, June 2004, University of Washington
//============================================================================*/

struct BerkMin561Solver : public SolverInterface {
    static std::string ExeName()
        { return("berkmin561-linux"); }

    static std::string LicenseName()
        { return("license.txt"); }

    static std::string Name()
        { return("berkmin561"); }

    BerkMin561Solver() : tmpFile_("berkmin561.stats"),
                         results_("berkin.results")
        { /* */ }

    virtual ~BerkMin561Solver()
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
        std::string toRtn = exe() + wffFileName;
        toRtn += (" > " + getTempFile());
        return(dir + toRtn);
    }
    virtual std::string getExeNameAndArgs(const std::string& dir,
                                          const std::string& wffFileName,
                                          const std::string& options) {
        std::string toRtn = exe() + wffFileName;
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


#endif // BERKMIN561_SOLVER_CSE473_H
