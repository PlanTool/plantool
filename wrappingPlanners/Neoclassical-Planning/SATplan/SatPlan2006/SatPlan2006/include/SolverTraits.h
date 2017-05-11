// Macro Guard
#ifndef CSE473_SOLVER_TRAITS_H
#define CSE473_SOLVER_TRAITS_H

// Files included
#include "Factory.h"
#include "SingletonType.h"
#include "StandardFiles.h"
#include "SolverInterface.h"

/*============================================================================//
    SolverTraits.h: typedefs common to all sat solvers

    Implementation: Shane J. Neph, June 2004, University of Washington
//============================================================================*/


class SolverTraits {
public:
    typedef Factory<SolverInterface, std::string> FactoryType;
    typedef SingletonType<FactoryType> SolverFactoryType;
protected:
    ~SolverTraits() { /* */ }
};

#endif  // CSE473_SOLVER_TRAITS_H
