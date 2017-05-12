// Files included
#include "Assertion.h"
#include "ByLine.h"
#include "CommonErrors.h"
#include "Exceptions.h"
#include "Jerusat-1.3.h"
#include "SolverTraits.h"
#include "StringAlgorithms.h"


/*============================================================================//
    Jerusat-1.3.cpp: Implementation of Jerusat_1_3Solver.  Also, registration
     of Jerusat1.3 as possible sat solver happens here.

    Implementation: Shane J. Neph, June 2004, University of Washington
//============================================================================*/


namespace {
    typedef CE::ProgramError PE;

    SolverInterface* Create() { 
        return(new Jerusat_1_3Solver); 
    }

    struct Tmp {
        static bool func() {
            // register Jerusat_1_3Solver as a possible solver
            bool reg = SolverTraits::SolverFactoryType::Instance()->Register(
                                            Jerusat_1_3Solver::Name(), Create);
            Assert<PE>(reg, "Creation of " + Jerusat_1_3Solver::Name());
            return(reg);
        }
    };
    static bool reg = Tmp::func();

    std::string unsat = "UNSATISFIABLE";
    std::string sat = "SATISFIABLE";
} // end unnamed namespace

//==================
// evaluateReturn()
//==================
bool Jerusat_1_3Solver::evaluateReturn(int rtnVal) {
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
    if ( !isSat(cpy) ) { // make file for unsat
        std::ofstream out(getSolutionName().c_str());
        out << unsat << std::endl;
    } else { // sat!
        std::list<std::string>::iterator i = lst.begin();
        while ( i != lst.end() ) {
            std::string next = StringAlgs::Trim(*i);
            if ( !next.empty() && next[0] == 'v' ) {
                std::vector<std::string> split = SplitString(next, ' ');                
                std::vector<std::string>::iterator end = split.end();
                if ( StringAlgs::Trim(*(--end)) == "0" ) // end marker
                    next = next.substr(0, next.size() - 1);
                out << next.substr(1);
                lst.erase(i++); // don't send this info to std::cout
                --i;
            }
            ++i;
        } // while
    }

    // Copy file content to std::cout
    std::copy(lst.begin(), lst.end(), 
              std::ostream_iterator<std::string>(std::cout, "\n"));

    return((10 == rtnVal) || (20 == rtnVal)); // 10 sat; 20 unsat
}

//=========
// isSat()
//=========
bool Jerusat_1_3Solver::isSat(const std::vector<std::string>& vec) {
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
void Jerusat_1_3Solver::listOptions(std::ostream& os, const std::string& dir) {
    os << "[-maxClsLen X (:integer)]"                << std::endl;
    os << "[-clsDensity X (:integer)]"               << std::endl;
    os << "[-qsNum X (:integer)]"                    << std::endl;
    os << "[-maxInterval X (:double)]"               << std::endl;
    os << "[-ifGetFirst X (:integer)]"               << std::endl;
    os << "[-ifLittleRand X (:integer)]"             << std::endl;
    os << "[-ifSortInImply X (:integer)]"            << std::endl;
    os << "[-ifPrepush X (:integer)]"                << std::endl;
    os << "[-restartAft X (:integer)]"               << std::endl;
    os << "[-restartAdd X (:integer)]"               << std::endl;
    os << "[-ifClsOnRestart X (:integer)]"           << std::endl;
    os << "[-maxMinHeur X (:double)]"                << std::endl;
    os << "[-ifLessSign X (:integer)]"               << std::endl;
    os << "[-notCheckConfsFrom X (:double)]"         << std::endl;
    os << "[-ifAddClsAftConfChk X (:integer)]"       << std::endl;
    os << "[-maxConfLen X (:integer)]"               << std::endl;
    os << "[-ifAdd1UipClause X (:integer)]"          << std::endl;
    os << "[-max1UIPLenToRec X (:integer)]"          << std::endl;
    os << "[-incrHeur1UIP X (:integer)]"             << std::endl;
    os << "[-cutOff X (:integer)]"                   << std::endl;
    os << "[-restartsStrategy X (:integer)]"         << std::endl;
    os << "[-restartDelayTimes X (:integer)]"        << std::endl;
    os << "[-restartsMaxClssToRec X (:integer)]"     << std::endl;
    os << "[-ifGetFirstBackAftRestart X (:integer)]" << std::endl;
}
