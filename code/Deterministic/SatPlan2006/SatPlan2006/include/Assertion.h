// Macro Guard
#ifndef CSE473_ASSERTIONS_H
#define CSE473_ASSERTIONS_H

// Files included
#include "StandardFiles.h"


/*============================================================================//
    Assertion.h: header file for Assert<ExceptionType> UDT

    Purpose is mostly syntactic sugar.  This class allows for runtime assertions
     to be made.

    Implementation: Shane J. Neph, June 2004, University of Washington
//============================================================================*/


template <typename ExceptionType>
struct Assert {
    explicit Assert(bool toAssert) {
        if ( ! toAssert ) {
            ExceptionType exc;
            throw(exc);
        }
    } // Assert(Overload1)
 
    Assert(bool toAssert, const std::string& info) {
        if ( ! toAssert ) 
            throw(ExceptionType(info));
    } // Assert(Overload2)

    Assert(bool toAssert, const std::string& info1, const std::string& info2) {
        if ( ! toAssert )
            throw(ExceptionType(info1, info2));
    } // Assert(Overload3)
};


#endif // CSE473_ASSERTIONS_H
