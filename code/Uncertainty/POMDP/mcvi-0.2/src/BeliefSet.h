#ifndef __BELIEFSET_H
#define __BELIEFSET_H

#include "Belief.h"

/**
   @class BeliefSet
   @brief Interface for storing belief
   @details Provides interface for data structure for storing beliefs.
   Inherit from this class for specific types of belief representations.

   @author Wee Sun Lee
   @date 27 August 2009
*/
class BeliefSet
{
  public:

    /**
       Insert pointer to a belief \a belief.
       @return the inserted pointer and true if successful. Returns the pointer
       to stored belief and false, if a good enough representation already exist
       in storage. If inserted, the object will be destroyed when BeliefSet
       is destroyed. If not inserted, it is the responsibility of calling
       routine to destroy the object.
    */
    virtual std::pair<Belief *,bool> insert(Belief *belief) = 0;

    /**
       @return number of stored beliefs
    */
    virtual long numBeliefs() = 0;

    /**
       Destructor. Should destroy the objects in storage.
    */
    virtual ~BeliefSet() {}

     class beliefComp {
      public:
        bool operator()(const Belief *b1, const Belief *b2) const;
    };
};

#endif // __BELIEFSET_H
