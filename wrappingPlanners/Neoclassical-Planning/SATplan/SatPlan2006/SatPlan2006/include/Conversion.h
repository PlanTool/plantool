// Macro Guard
#ifndef CONVERSION_CSE473_H
#define CONVERSION_CSE473_H

/*============================================================================//
    Conversion.h: header file for conversion<Type> function.

    Purpose is syntactic sugar.  Allow user to convert a type into another type
     using a notation similar to static_cast<>.  Currently wired to only
     support types convertible to/from a string via a stringstream object.

    Implementation: Shane J. Neph, June 2004, University of Washington
//============================================================================*/


//==============
// convert<,>()
//==============
template <typename RtnType, typename From>
extern RtnType convert(From from);


#include "Conversion.template"  // g++ workaround, no support for 'export'


#endif  // CONVERSION_CSE473_H
