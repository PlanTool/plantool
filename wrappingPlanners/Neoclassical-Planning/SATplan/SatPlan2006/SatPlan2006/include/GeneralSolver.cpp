// Files included
#include "Assertion.h"
#include "ByLine.h"
#include "CommonErrors.h"
#include "Exceptions.h"
#include "GeneralSolver.h"
#include "SolverTraits.h"
#include "StringAlgorithms.h"


/*============================================================================//
    GeneralSolver.cpp: Implementation of the GeneralSolver.  

    Implementation: Shane J. Neph, June 2004, University of Washington
//============================================================================*/


namespace {
    typedef CE::ProgramError PE;

    SolverInterface* Create() { 
        return(new GeneralSolver()); 
    }

    struct Tmp {
        static bool func() {
            // register GeneralSolver as a possible solver
            bool reg = SolverTraits::SolverFactoryType::Instance()->Register(
                                               GeneralSolver::Name(), Create);
            Assert<PE>(reg, "Creation of " + GeneralSolver::Name());
            return(reg);
        }
    };
    static bool reg = Tmp::func();

    std::string unsat = "UNSATISFIABLE";
    std::string sat = "SATISFIABLE";
    std::string error = "ERROR";

    long maxNum(const std::vector<std::string>& vec) {
        /* make sure everything in vec is a number! */
        long toRtn = -100000;
        std::vector<std::string>::const_iterator i = vec.begin(), j = vec.end();
        while ( i != j ) {
            long tmp = convert<long>(*i);
            if ( tmp < 0 ) tmp = -tmp;
            if ( tmp > toRtn ) toRtn = tmp;
            ++i;
        }
        return(toRtn);
    }

    std::vector<std::string> getNums(const std::string& in) {
        std::vector<std::string> toRtn = StringAlgs::SplitString(in, ' ');
        std::vector<std::string>::iterator i = toRtn.begin(), j = toRtn.end();
        while ( i != j ) {
            if ( !StringAlgs::IsInteger(*i) ) {
                toRtn.clear();
                break;
            }
            ++i;
        }
        return(toRtn);
    }
} // end unnamed namespace


//==================
// evaluateReturn()
//==================
bool GeneralSolver::evaluateReturn(int rtnVal) {
    std::ifstream ifile(getTempFile().c_str());
    if ( !ifile ) return(false);
    std::istream_iterator<ByLine> in(ifile), eof;
    long absMax = -1;

    // Copy file content to 'lst' --> see if !sat and take appropriate action
    std::list<std::string> lst;
    std::copy(in, eof, std::back_inserter(lst));
    std::vector<std::string> cpy(lst.begin(), lst.end());

    // Set up file with real solution or 'unsat'
    using StringAlgs::SplitString;
    std::ofstream out(getSolutionName().c_str());
    Assert<CE::FileError>(out, "can't open: " + getSolutionName());
    if ( !isSat(cpy) ) // make file for unsat
        out << unsat << std::endl;
    else { // sat!
        out << sat << std::endl;
        std::ostream_iterator<std::string> ofile(out, "\n");
        std::list<std::string>::iterator i = lst.begin(), j;
        std::string::size_type npos = std::string::npos;
        bool done = false;
        while ( i != lst.end() ) {
            std::string next = StringAlgs::Trim(*i);
            if ( !next.empty() && (next.find(sat) != npos) ) {
                done = true;
                j = i;
                bool justNumbers = false;
                while ( ++j != lst.end() ) {
                    next = StringAlgs::Trim(*j);
                    std::vector<std::string> allNumbers = getNums(next);
                    long max = maxNum(allNumbers);
                    if ( max > absMax )
                        absMax = max;
                    if ( !allNumbers.empty() ) {
                        std::copy(allNumbers.begin(), allNumbers.end(), ofile);
                        justNumbers = true;
                    }
                    else if ( justNumbers )
                        break;
                } // while
            }
            if ( done ) break;
            ++i;
        } // while

        // Perform sanity checks
        std::string err = "No variable assignments found in solution file!";
        Assert<CE::FileError>(absMax > 0, err);

        err = "Solution file had only " + convert<std::string>(absMax);
        err += " variable assignments, but WFF had ";
        err += convert<std::string>(numVars_);
        err += " variables...Invalid Solution";
        Assert<CE::FileError>(absMax == numVars_, err);
    }
    return((0 == rtnVal) || (1 == rtnVal)); // 0 sat; 1 unsat
}

//======================
// getNumberVariables()
//======================
long GeneralSolver::getNumberVariables(const std::string& wff) {
     std::ifstream infile(wff.c_str());
     Assert<CE::FileError>(infile, "unable to open: " + wff);
     std::istream_iterator<ByLine> in(infile), eof;
     std::list<std::string> lst;
     std::copy(in, eof, std::back_inserter(lst));
     std::list<std::string>::iterator i = lst.begin(), j = lst.end();
     long absMax = -1;
     while ( i != j ) {
         long max = maxNum(getNums(*i));
         if ( max > absMax )
             absMax = max;
         ++i;
     }
     return(absMax);
}

//=========
// isSat()
//=========
bool GeneralSolver::isSat(const std::vector<std::string>& vec) {
    typedef SolverInterface::AbortedMission AM;
    std::vector<std::string>::const_iterator i = vec.begin(), j = vec.end();
    while ( i != j ) {
        if ( i->find(unsat) != std::string::npos )
            return(false);
        else if ( i->find(sat) != std::string::npos )
            return(true);
        Assert<AM>(i->find(error) == std::string::npos, "Solver Aborted");
        ++i;
    }
    bool toThrow = false;
    Assert<AM>(toThrow, "Improper solution file format...assumed Solver Aborted");
    return(false);
}

//===============
// listOptions()
//===============
void GeneralSolver::listOptions(std::ostream& os) {
    try {
        SolverInterface::runSolver(exe());
    } catch(...) { /* */ }
}

