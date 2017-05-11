// Macro guard
#ifndef CNFENCODING_CSE473_H
#define CNFENCODING_CSE473_H

// Files included
#include "Assertion.h"
#include "CommonErrors.h"
#include "Input.h"
#include "StandardFiles.h"

// Unix-specific headers
#include <sys/wait.h>

/*============================================================================//
    CNFEncoding.h: header file for simple CNFEncoding UDT

    Purpose is to manage calls to bb executable.

    Implementation: Shane J. Neph, June 2004, University of Washington
//============================================================================*/


struct CNFEncoding {
    enum Status { SAT = 0, UNSAT = 2, ERROR = -1 };

    // Constructor
    CNFEncoding(const Input& input) : input_(input), phase1_(true), bCheck_(true) {
        // make unique names based on pid#
        tmpFile_ = "solveroutput." + convert<std::string>(getpid());
        finalFile_ = "original." + convert<std::string>(getpid());
    }

    // Public Interface
    std::string FinalSolutionFile() const {
        return(finalFile_);
    }

    std::string TempSolutionFile() const {
        return(tmpFile_);
    }

    Status Run(long planLevel) {
        std::string args;
        std::string domain = input_.Domain();
        std::string prob = input_.Problem();
	std::string style = convert<std::string>(input_.Encoding());
        if ( !domain.empty() && domain[0] != '/' )
            domain = input_.Path() + domain;
        if ( !prob.empty() && prob[0] != '/' )
            prob = input_.Path() + prob;
        args = input_.ExeDirectory();
        args += "bb -o " + domain;
        args += " -f " + prob + " -C " + style + " -b ";
        args += input_.CNF() + " -d ";
        args += (input_.KeepCNF() ? "1" : "0");
        args += " -l " + convert<std::string>(planLevel) + " ";
        args += (phase1_ ? "-G 0" : "-G 1");

        if ( !phase1_ ) {
            args += " -S " + TempSolutionFile();
            args += " -F " + FinalSolutionFile();
            args += " -t 0";
        }
        else {
            args += (bCheck_ ? " -t 1" : " -t 0");
        }

        if ( input_.KeepCNF() )
            args += (" -V " + input_.VarFile());

        int sysCall = system(args.c_str());
        static std::string err = "Memory limit exceeded: bb system call";
        static std::string timeerr = "Time limit exceeded: bb system call";
        static std::string bberr = "bb internal error";
        static std::string allerr = err + ", OR " + timeerr + ", OR " + bberr;

        if ( WIFSIGNALED(sysCall) ) {
            int tmp = WTERMSIG(sysCall);
            Assert<CE::UserAbort>(tmp != SIGQUIT, "user abort");
            Assert<CE::UserAbort>(tmp != SIGINT, "user abort");
            Assert<CE::MemoryLimit>(SIGXFSZ != tmp, err);
            Assert<CE::MemoryLimit>(SIGSEGV != tmp, err);
            Assert<CE::TimeOut>(SIGXCPU != tmp, timeerr);
            typedef CE::ProgramError PE;
            Assert<PE>(false, "Unchecked Signal Generated...aborting");
        }

        if ( UNSAT == WEXITSTATUS(sysCall) )
            return(UNSAT);
        else if ( SAT == WEXITSTATUS(sysCall) ) {
            // SAT is sometimes returned even when a resource is exhausted."
            Assert<CE::MemoryLimit>(!outofMemoryorTime(), allerr);
            return(SAT);
        }
        Assert<CE::MemoryLimit>(!outofMemoryorTime(), allerr);
        return(ERROR);
    }
  
    void SetPhase1(bool bCheck=false) { bCheck_ = bCheck; phase1_ = true; }
    void SetPhase2() { phase1_ = false; }

private:
    bool outofMemoryorTime() const {
        // See if CNF file was created: if not, assume memory problem
        std::ifstream ifile(input_.CNF().c_str());
        return(!ifile);
    }


private:
    Input input_;
    bool phase1_, bCheck_;
    std::string finalFile_, tmpFile_;
};


#endif // CNFENCODING_CSE473_H
