// Files included
#include "Assertion.h"
#include "CommonErrors.h"
#include "Exceptions.h"
#include "SiegeSolver.h"
#include "SolverTraits.h"
#include "StringAlgorithms.h"


/*============================================================================//
    SiegeSolver.cpp: Implementation of SiegeSolver.  Also, registration of
     SiegeSolver as possible sat solver happens here.

    Implementation: Shane J. Neph, June 2004, University of Washington
//============================================================================*/


namespace {
    typedef CE::ProgramError PE;

    SolverInterface* Create() { 
        return(new SiegeSolver); 
    }

    struct Tmp {
        static bool func() {
            // register SiegeSolver as a possible solver
            bool reg = SolverTraits::SolverFactoryType::Instance()->Register(
                                                 SiegeSolver::Name(), Create);
            Assert<PE>(reg, "Creation of " + SiegeSolver::Name());
            return(reg);
        }
    };
    static bool reg = Tmp::func();
} // end unnamed namespace


//=========
// isSat()
//=========
bool SiegeSolver::isSat(const std::vector<std::string>& vec) {

    /* no extra work needed for results from siege */
    std::vector<std::string>::const_reverse_iterator iter = vec.rbegin();
    while ( iter != vec.rend() ) {
        if ( iter->find("unsatisfiable") != std::string::npos )
            return(false);
        ++iter;
    }
    return(true);
}


