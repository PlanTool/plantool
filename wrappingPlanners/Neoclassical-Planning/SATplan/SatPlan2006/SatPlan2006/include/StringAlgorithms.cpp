// Files included
#include "StringAlgorithms.h"


/*============================================================================//
    StringAlgorithms.cpp: Implementation File for StringAlgorithms.h

    Several simple std::string-related algorithms

    Implementation: Shane J. Neph, June 2004, University of Washington
//============================================================================*/


namespace StringAlgs {

//=============
// IsInteger()
//=============
bool IsInteger(const std::string& num) {
    if ( num.empty() ) return(false);
    return(num.find_first_not_of("-0123456789") == std::string::npos);
}

//===================
// RemoveBackSpace()
//===================
void RemoveBackSpace(std::string& s) {
    bool done = false;
    while ( ! done ) {
        if ( s.empty() ) break;
        if ( s[s.size()-1] == ' ' ) {
            if ( s.size() > 1 )
                s = s.substr(0, s.size()-1);
            else
                s = "";
        }
        else 
            break;
    } // while
}

//====================
// RemoveFrontSpace()
//====================
void RemoveFrontSpace(std::string& s) {
    bool done = false;
    while ( ! done ) {
        if ( s.empty() ) 
            break;
        if ( s[0] == ' ' ) 
            s = s.substr(1);
        else 
            break;
    } // while
}

//========================
// RemoveFrontBackSpace()
//========================
void RemoveFrontBackSpace(std::string& s) {
    RemoveBackSpace(s);
    RemoveFrontSpace(s);
}

//=====================
// RemoveAllNewlines()
//=====================
void RemoveAllNewlines(std::string& s) {    
    std::size_t idx = 0;
    while ( idx < s.size() ) {        
        if ( s.empty() ) break;
        if ( s[idx] == '\n' ) { 
            s = s.substr(0, idx) + s.substr(idx + 1);
            --idx;
        }
        ++idx;
    }
}

//==============
// RemoveTabs()
//==============
void RemoveTabs(std::string& s) {
    bool done = false;
    std::string::size_type idx = 0;
    while ( ! done ) {
        if ( s.empty() ) break;
        if ( s[idx] == '\t' ) {
            s = s.substr(0, idx) + s.substr(idx+1);
            idx = 0;
        }
        if ( ++idx == s.size() ) break;
    } // while
}

//===============
// SplitString() 
//===============
std::vector<std::string> SplitString(const std::string& s, char delim) {
    // Split string around each delim instance
    std::vector<std::string> toRtn;
    if ( s.empty() )
        return(toRtn);
    
    std::size_t j = 0, i = 0;
    for ( ; i < s.size(); ) {
        if ( s[i] == delim ) {
            if ( 0 != i ) 
                toRtn.push_back(s.substr(j, i-j));
            j = ++i;         
        }
        else
            ++i;
    } // for
    if ( j != i )
        toRtn.push_back(s.substr(j));
 
    // Remove leading/trailing spaces, any tabs and any newlines
    std::vector<std::string>::iterator k = toRtn.begin();
    while ( k != toRtn.end() ) {
        RemoveTabs(*k);
        RemoveFrontBackSpace(*k);
        RemoveAllNewlines(*k);
        if ( ! k->empty() )
            ++k;
        else {
            toRtn.erase(k);
            k = toRtn.begin();
        }
    } // while
    return(toRtn);
}

//==============
// StartsWith()
//==============
bool StartsWith(const std::string& str, const std::string& toStart) {    
    if ( str.size() < toStart.size() )
        return(false);
    for ( std::size_t i = 0; i < toStart.size(); ++i ) {
        if ( tolower(toStart[i]) != tolower(str[i]) )
            return(false);
    }
    return(true);
}

//========
// Trim()
//========
std::string Trim(const std::string& s) {
    std::string copy(s);
    RemoveBackSpace(copy);
    RemoveFrontSpace(copy);
    return(copy);
}

//=============
// Uppercase()
//=============
std::string Uppercase(const std::string& str) {
    long size = static_cast<long>(str.size());
    std::string toRtn;
    for ( long idx = 0; idx < size; ++idx ) 
        toRtn += toupper(str[idx]);
    return(toRtn);
}

} // namespace StringAlgs
