// Macro guard
#ifndef BYLINE_CSE473_H
#define BYLINE_CSE473_H

// Files included
#include "StandardFiles.h"

/*============================================================================//
    ByLine.h: Implementation of a tool to be used with an istream_iterator.
     Allows an entire file to be copied into a container line by line rather
     than by the usual (any) whitespace delimmitters.

    Implementation: Shane J. Neph, June 2004, University of Washington
//============================================================================*/

struct ByLine : std::string {
    friend std::istream& operator>>(std::istream &is, ByLine &toGet) {
        std::getline(is, toGet);
        return is;
    }
};


#endif // BYLINE_CSE473_H
