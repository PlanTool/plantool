// Macro Guard
#ifndef SOLVER_CSE473_H
#define SOLVER_CSE473_H

// Files included
#include "Assertion.h"
#include "ByLine.h"
#include "CommonErrors.h"
#include "Conversion.h"
#include "Exceptions.h"
#include "StandardFiles.h"

// unix-specific headers
#include <unistd.h>
#include <sys/wait.h>

/*============================================================================//
    SolverInterface.h: header file

    Purpose is to define a general interface for arbitrary sat solvers.  Note
     that the "Template Method" was used here (not to be confused with C++
     templates) --> Define a non-virtual interface and let solvers implement
     the protected/private virtual interface --> most changes can be 
     implemented without affecting the user of SolverInterface.

    Implementation: Shane J. Neph, June 2004, University of Washington
//============================================================================*/

struct Input; // forward declaration

struct SolverInterface {
    typedef Exc::Exception<111> AbortedMission;

    //================
    // GetRevisions()
    //================
    static std::vector<std::string> GetRevisions(const std::string& dir) {
        std::string loc = dir + "SolverRevisions.txt";
        std::ifstream ifile(loc.c_str());
        std::vector<std::string> toRtn;
        if ( !ifile )
            return(toRtn);
        std::istream_iterator<ByLine> in(ifile), eof;
        std::copy(in, eof, std::back_inserter(toRtn));
        return(toRtn);
    }

    //=========
    // IsSat()
    //=========
    bool IsSat(const std::string& file) {
        std::ifstream ifile(file.c_str());
        Assert<CE::FileError>(ifile, "No " + file + " file found...");
        std::vector<std::string> vec;
        std::copy(std::istream_iterator<ByLine>(ifile),
                  std::istream_iterator<ByLine>(),
                  std::back_inserter(vec));
        ifile.close();         
        return(isSat(vec));
    }

    //===============
    // ListOptions()
    //===============
    std::ostream& ListOptions(std::ostream& os, const std::string& dir) {
        listOptions(os, dir);
        return(os);
    }

    //==================
    // RenameSolution()
    //==================
    void RenameSolution(const std::string& newFileName) {
        std::string curName = getSolutionName();
        int rtn = 0;
        if ( newFileName.empty() ) { // remove
            std::remove(curName.c_str());
            return;
        }
        else // rename
            rtn = std::rename(curName.c_str(), newFileName.c_str());
        Assert<CE::FileError>(!rtn, "Can't rename - No File: " + curName);
    }

    //=============
    // RunSolver()
    //=============
    void RunSolver(const Input& input);

    //==============
    // SetExeName()
    //==============
    void SetExeName(const std::string& newName) { 
        setExe(newName); 
    }

    //=============
    // SetUnique()
    //=============
    void SetUnique(const std::string& original) {
        setUnique(original);
    }

    virtual ~SolverInterface() {
        std::vector<std::string>::iterator i = toRemove_.begin();
        while ( i != toRemove_.end() ) {
            std::remove(i->c_str());
            ++i;
        }
    }


protected: // virtuals
    /*
     When overridden, evaluateReturn() gives a solver an opportunity to look
      over its own results prior to IsSat() being called externally.  Note
      that evaluateReturn()'s purpose is to return true if 'rtnVal'
      represents *either* sat or unsat.  Return false on error code only.
    */

    virtual std::vector<std::string> cleanFiles() const = 0;
    virtual bool evaluateReturn(int rtnVal) { return(0 == rtnVal); }
    virtual std::string exe() const = 0;
    virtual std::string getExeNameAndArgs(const std::string& dir,
                                          const std::string& wffFileName,
                                          const std::string& options) = 0;
    virtual std::string getExeNameAndArgs(const std::string& dir,
                                          const std::string& wffFileName,
                                          long rand) = 0;
    virtual std::string getSolutionName() const = 0;
    virtual bool isSat(const std::vector<std::string>& vec) = 0;
    virtual void listOptions(std::ostream& os, const std::string& dir) {
        os << "no additonal options for this solver" << std::endl;
    }
    virtual std::string name() const = 0;
    virtual int runSolver(const std::string& exe) {
        int rtn = std::system(exe.c_str()); // run solver
        return(WEXITSTATUS(rtn));
    }
    virtual void setExe(const std::string& newExe) { /* do nothing by def */ }
    virtual void setUnique(const std::string& original) = 0;

private:
    void checkRtn(int rtn);

private:
    std::string dir_;
    std::vector<std::string> toRemove_;
};

#endif // SOLVER_CSE473_H
