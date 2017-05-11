// Macro guard
#ifndef SIEGE_SOLVER_CSE473_H
#define SIEGE_SOLVER_CSE473_H

// Files included
#include "SolverInterface.h"


/*============================================================================//
    SiegeSolver.h: header file - implementation of SolverInterface for Siege

    Implementation: Shane J. Neph, June 2004, University of Washington
//============================================================================*/

struct SiegeSolver : public SolverInterface {
    static std::string ExeName()
        { return("siege_v4"); }

    static std::string Name()
        { return("siege"); }

    virtual ~SiegeSolver()
        { /* */ }

    SiegeSolver() : tmpFile_("siege.results") { /* */ }

private:
    virtual std::vector<std::string> cleanFiles() const {
        std::vector<std::string> toRtn;
        toRtn.push_back(getSolutionName());
        return(toRtn);
    }
    std::string exe() const { return(ExeName() + " "); }
    virtual std::string getExeNameAndArgs(const std::string& dir,
                                          const std::string& wffFileName,
                                          long rand) {
        return(dir + exe() + wffFileName + " " + convert<std::string>(rand));
    }
    virtual std::string getExeNameAndArgs(const std::string& dir,
                                          const std::string& wffFileName,
                                          const std::string& options) {
        // rand() used in overloaded version; no other real options
        return(dir + exe() + wffFileName);
    }

    virtual std::string getSolutionName() const { return(tmpFile_); }
    virtual bool isSat(const std::vector<std::string>& vec);
    virtual std::string name() const { return(Name()); }
    virtual void setUnique(const std::string& original) {
        /* solver outputs to its own file --> can't do anything here */
    }

private:
    std::string tmpFile_;
};


#endif // SIEGE_SOLVER_CSE473_H
