// Macro Guard
#ifndef CSE473_FACTORY_H
#define CSE473_FACTORY_H

// Files included
#include "Assertion.h"
#include "CommonErrors.h"
#include "Exceptions.h"
#include "StandardFiles.h"


//=============================================================
// Factory design idea "borrowed" from 'Modern C++ Design'
// by Andrei Alexandrescu and modified slightly from his 
// implementation in the Loki Library.
//=============================================================

/*============================================================================//
    Factory.h: Generic abstract factory    

    Implementation: Shane J. Neph, June 2004, University of Washington
//============================================================================*/


template
    <
    typename AbstractProduct,
    typename IdentifierType,
    typename ProductCreator = AbstractProduct* (*)()
    >
struct Factory {
    typedef IdentifierType IDType;

    bool IsRegistered(const IdentifierType& id) {
        return(associations_.find(id) != associations_.end());
    }

    bool Register(const IdentifierType& id, ProductCreator creator) {
        return(associations_.insert(typename AssocMap::value_type(id, creator)).second);
    }

    bool Unregister(const IdentifierType& id) {
        return(associations_.erase(id) == 1);
    }

    std::set<IdentifierType> GetIDs() const {
        std::set<IdentifierType> toRtn;
        typename AssocMap::const_iterator i = associations_.begin();
        while ( i != associations_.end() ) {
            toRtn.insert(i->first);
            ++i;
        }
        return(toRtn);
    }

    AbstractProduct* CreateObject(const IdentifierType& id) {
        typename AssocMap::const_iterator i = associations_.find(id);
        Assert<CE::BadArg>(i != associations_.end(), "Factory Class");
        return((i->second)());  // Create an instance and return
    }
private:
    typedef std::map<IdentifierType, ProductCreator> AssocMap;
    AssocMap associations_;
};

#endif // CSE473_FACTORY_H
