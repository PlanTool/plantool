#ifndef COMMON_ERRORS_CSE473_H
#define COMMON_ERRORS_CSE473_H

// Files included
#include "Exceptions.h"


/*============================================================================//
    CommonErrors.h: central location to typedef common errors used by app.
     Everything is defined in the CE namespace.

    Implementation: Shane J. Neph, June 2004, University of Washington
//============================================================================*/

namespace CE {

    // Type definitions
    typedef Exc::Exception<-1>  BadArg;
    typedef Exc::Exception<-2>  ErrorOccurred;
    typedef Exc::Exception<-3>  FileError;
    typedef Exc::Exception<-4>  FileFormatError;
    typedef Exc::Exception<-5>  Help;
    typedef Exc::Exception<-6>  InfiniteLoop;
    typedef Exc::Exception<-7>  LogicError;
    typedef Exc::Exception<-8>  MemoryLimit;
    typedef Exc::Exception<-9>  ProgramError;
    typedef Exc::Exception<-10> RunTimeError;
    typedef Exc::Exception<-11> TimeOut;
    typedef Exc::Exception<-12> UndefinedArg;
    typedef Exc::Exception<-13> UnknownArg;
    typedef Exc::Exception<-14> UnknownErr;
    typedef Exc::Exception<-15> Usage;
    typedef Exc::Exception<-16> UserAbort;

}

#endif // COMMON_ERRORS_CSE473_H
