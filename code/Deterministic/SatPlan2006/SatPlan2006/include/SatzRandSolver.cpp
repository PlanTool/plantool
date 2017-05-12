// Files included
#include "Assertion.h"
#include "ByLine.h"
#include "CommonErrors.h"
#include "Exceptions.h"
#include "SatzRandSolver.h"
#include "SolverTraits.h"
#include "StringAlgorithms.h"


/*============================================================================//
    SatzRandSolver.cpp: Implementation of SatzRandSolver.h.  Registration of
     SatzRandSolver as possible sat solver happens here.

    Implementation: Shane J. Neph, June 2004, University of Washington
//============================================================================*/


namespace {
    typedef CE::ProgramError PE;

    SolverInterface* Create() { 
        return(new SatzRandSolver); 
    }

    struct Tmp {
        static bool func() {
            // register SatzRandSolver as a possible solver
            bool reg = SolverTraits::SolverFactoryType::Instance()->Register(
                                              SatzRandSolver::Name(), Create);
            Assert<PE>(reg, "Creation of " + SatzRandSolver::Name());
            return(reg);
        }
    };
    static bool reg = Tmp::func();

    std::string unsat = "unsatisfiable";
} // end unnamed namespace


//==================
// evaluateReturn()
//==================
bool SatzRandSolver::evaluateReturn(int rtnVal) {
    // SatzRandSolver does not write to file on UNSAT --> take care of here
    std::ifstream ifile(tmpFile_.c_str());
    if ( !ifile ) return(false);
    std::istream_iterator<ByLine> in(ifile), eof;

    // Copy file content to 'vec' --> see if !sat and take appropriate action
    std::vector<std::string> vec;
    std::copy(in, eof, std::back_inserter(vec));

    // Copy file content to std::cout
    std::copy(vec.begin(), vec.end(), 
              std::ostream_iterator<std::string>(std::cout, "\n"));

    if ( !isSat(vec) ) { // make file for isSat()
        std::ofstream out(getSolutionName().c_str());
        out << unsat << std::endl;
    }
    return(SolverInterface::evaluateReturn(rtnVal));
}

//=========
// isSat()
//=========
bool SatzRandSolver::isSat(const std::vector<std::string>& vec) {
    std::vector<std::string>::const_iterator i = vec.begin(), j = vec.end();
    while ( i != j ) {
        if ( i->find(unsat) != std::string::npos )
            return(false);
        ++i;
    }
    return(true);
}

//===============
// listOptions()
//===============
void SatzRandSolver::listOptions(std::ostream& os, const std::string& dir) {
    try {
        SolverInterface::runSolver(dir + exe());
    } catch(...) { /* */ }
}
