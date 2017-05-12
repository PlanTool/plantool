// Files included
#include "Assertion.h"
#include "BerkMin561.h"
#include "ByLine.h"
#include "CommonErrors.h"
#include "Exceptions.h"
#include "SolverTraits.h"
#include "StringAlgorithms.h"


/*============================================================================//
    BerkMin561.cpp: Implementation of BerkMin561Solver.  Also, registration
     of BerkMin561 as possible sat solver happens here.

    Implementation: Shane J. Neph, June 2004, University of Washington
//============================================================================*/


namespace {
    typedef CE::ProgramError PE;

    SolverInterface* Create() { 
        return(new BerkMin561Solver); 
    }

    struct Tmp {
        static bool func() {
            // register BerkMin561Solver as a possible solver
            bool reg = SolverTraits::SolverFactoryType::Instance()->Register(
                                            BerkMin561Solver::Name(), Create);
            Assert<PE>(reg, "Creation of " + BerkMin561Solver::Name());
            return(reg);
        }
    };
    static bool reg = Tmp::func();

    std::string unsat = "UNSATISFIABLE";
    std::string sat = "SATISFIABLE";
} // end unnamed namespace


bool BerkMin561Solver::evaluateReturn(int rtnVal) {
    std::ifstream ifile(getTempFile().c_str());
    if ( !ifile ) return(false);
    std::istream_iterator<ByLine> in(ifile), eof;

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
        std::list<std::string>::iterator i = lst.begin();
        std::string::size_type npos = std::string::npos;
        while ( i != lst.end() ) {
            std::string next = StringAlgs::Trim(*i);
            if ( !next.empty() && (next.find("solution") != npos) ) {
                out << next;
                lst.erase(i++); // don't send this info to std::cout
                --i;
            }
            ++i;
        } // while
        // Copy file content to std::cout
        std::copy(lst.begin(), lst.end(), 
                  std::ostream_iterator<std::string>(std::cout, "\n"));
    }
    return((10 == rtnVal) || (20 == rtnVal)); // 10 sat; 20 unsat
}


bool BerkMin561Solver::isSat(const std::vector<std::string>& vec) {
    std::vector<std::string>::const_iterator i = vec.begin(), j = vec.end();
    while ( i != j ) {
        if ( i->find(unsat) != std::string::npos )
            return(false);
        ++i;
    }
    return(true);
}

void BerkMin561Solver::listOptions(std::ostream& os, const std::string& dir) {
    try {
        SolverInterface::runSolver(dir + exe());
    } catch(...) { /* */ }
}
