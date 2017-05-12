#ifndef EXCEPTIONS_CSE473_H
#define EXCEPTIONS_CSE473_H

#include "StandardFiles.h"


/*============================================================================//
    Exceptions.h: header file for Exception<int> UDT

    Purpose is to allow user to quickly define new exceptions within a
     hierarchy, with some built-in functionality (e.g.; GetError()).  
     Exception<> is located within the Exc:: namespace.

    Implementation: Shane J. Neph, June 2004, University of Washington
//============================================================================*/


namespace Exc {

// Forward Declarations
template <int ID>
struct Exception;

// Explicit Specializations
template <>
struct Exception<0> {
    Exception() : info_("Unknown Exception") { /* */ }
    explicit Exception(const std::string info) : info_(info) { /* */ }
    Exception(const std::string& info1, const std::string& info2) 
        : info_(info1 + "\n" + info2) { /* */ }

    std::string GetMessage() const { return(info_); }

private:
    std::string info_;
};

// Typedefs
typedef Exception<0> BaseException;

// Definitions
template <int ID>
struct Exception : BaseException {
    Exception() : BaseException("Unknown Exception") { /* */ }
    explicit Exception(const std::string info) : BaseException(info) { /* */ }
    Exception(const std::string& info1, const std::string& info2) 
        : BaseException(info1, info2) { /* */ }
};

} // namespace Exc

#endif // EXCEPTIONS_CSE473_H
