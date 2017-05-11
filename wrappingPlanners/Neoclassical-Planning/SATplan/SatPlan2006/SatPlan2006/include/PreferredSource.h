// Macro Guard
#ifndef PREFERRED_SOURCE_CSE473_H
#define PREFERRED_SOURCE_CSE473_H

// Files included
#include "BerkMin561.h"
#include "ByLine.h"
#include "Jerusat-1.3.h"
#include "MiniSatSolver.h"
#include "SatzRandSolver.h"
#include "SiegeSolver.h"
#include "SolverInterface.h"
#include "SolverTraits.h"


/*============================================================================//
    PreferredSource.h: header file for PreferredSource UDT

    Purpose is to select a default solver --> check to ensure the binary 
     actually exists.  Note that this kind of defeats the purpose of using a
     generic Factory in SolverTraits (solvers register themselves), but we 
     cannot be sure that an actual executable exists.  Also, a compiled-in
     solver wrapper does not necessarily need to be a "preferred source".

    Implementation: Shane J. Neph, June 2004, University of Washington
//============================================================================*/




struct PreferredSource {
    typedef SolverTraits::FactoryType FT;

    static bool Exists(const std::string& dir, const std::string& solver) {
        if ( solver == SiegeSolver::Name() )
            return(exists(dir, SiegeSolver::ExeName()));
        if ( solver == MiniSatSolver::Name() )
            return(exists(dir, MiniSatSolver::ExeName()));
        if ( solver == Jerusat_1_3Solver::Name() )
            return(exists(dir, Jerusat_1_3Solver::ExeName()));
        if ( solver == SatzRandSolver::Name() )
            return(exists(dir, SatzRandSolver::ExeName()));
        if ( solver == BerkMin561Solver::Name() ) { // Licence Agreement
            bool berk = exists(dir, BerkMin561Solver::ExeName());
            if ( berk ) {
                std::string lic = BerkMin561Solver::LicenseName();
                berk = exists(dir, lic);
                if ( berk ) {
                    std::ofstream ofile(lic.c_str());
                    std::ifstream infile((dir + lic).c_str());
                    if ( !(infile && ofile) ) return(false);
                    std::istream_iterator<ByLine> in(infile), eof;
                    std::ostream_iterator<std::string> out(ofile, "\n");
                    std::copy(in, eof, out);
                }
            }
            return(berk);
        }
        return(exists(dir, solver)); // look for solver name directly
    }

    static std::string SelectPreferred(const std::string& dir) {
        if ( exists(dir, SiegeSolver::ExeName()) ) // 1st choice
            return(SiegeSolver::Name());
        if ( exists(dir, MiniSatSolver::ExeName()) ) // 2nd choice
            return(MiniSatSolver::Name());
        if ( exists(dir, BerkMin561Solver::ExeName()) ) { // 3rd choice
            bool berk = exists(dir, BerkMin561Solver::ExeName());
            if ( berk ) {
                std::string lic = BerkMin561Solver::LicenseName();
                berk = exists(dir, lic);
                if ( berk ) {
                    std::ofstream ofile(lic.c_str());
                    std::ifstream infile((dir + lic).c_str());
                    if ( infile && ofile ) {
                        std::istream_iterator<ByLine> in(infile), eof;
                        std::ostream_iterator<std::string> out(ofile, "\n");
                        std::copy(in, eof, out);
                        return(BerkMin561Solver::Name());
                    }
                }
            }
        }
        if ( exists(dir, Jerusat_1_3Solver::ExeName()) ) // 4th choice
            return(Jerusat_1_3Solver::Name());
        if ( exists(dir, SatzRandSolver::ExeName()) ) // 5th choice
            return(SatzRandSolver::Name());
        return("");
    }

private:
    static bool exists(const std::string& dir, const std::string& file) {
        std::string wholePath = file;
        if ( dir != "./" )
            wholePath = dir + wholePath;
        std::ifstream test(wholePath.c_str());
        if ( !test )
            return(false);
        std::vector<std::string> toCheck;
        std::copy(std::istream_iterator<ByLine>(test),
                  std::istream_iterator<ByLine>(),
                  std::back_inserter(toCheck));
        return(!toCheck.empty());
    }
};

#endif // PREFERRED_SOURCE_CSE473_H

