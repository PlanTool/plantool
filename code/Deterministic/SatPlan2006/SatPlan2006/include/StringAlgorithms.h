// Macro Guard
#ifndef STRINGALGORITHMS_H
#define STRINGALGORITHMS_H

// Files included
#include "StandardFiles.h"

/*============================================================================//
    StringAlgorithms.h: header file for several string functions

    Purpose is to give user some common string manipulation algorithms.
    Located within StringAlgs namespace.

    Implementation: Shane J. Neph, June 2004, University of Washington
//============================================================================*/


namespace StringAlgs {

//=============
// IsInteger()
//=============
extern bool IsInteger(const std::string& num);

//=====================
// RemoveAllNewlines()
//=====================
extern void RemoveNewlines(std::string& s);

//===================
// RemoveBackSpace()
//===================
extern void RemoveBackSpace(std::string& s);

//========================
// RemoveFrontBackSpace()
//========================
extern void RemoveFrontBackSpace(std::string& s);

//====================
// RemoveFrontSpace()
//====================
extern void RemoveFrontSpace(std::string& s);

//==============
// RemoveTabs()
//==============
extern void RemoveTabs(std::string& s);

//===============
// SplitString() 
//===============
extern std::vector<std::string> SplitString(const std::string& s, char delim);

//==============
// StartsWith()
//==============
extern bool StartsWith(const std::string& str, const std::string& toStart);

//========
// Trim()
//========
extern std::string Trim(const std::string& str);

//=============
// Uppercase()
//=============
extern std::string Uppercase(const std::string& str);

} // namespace StringAlgs


#endif // STRINGALGORITHMS_H
