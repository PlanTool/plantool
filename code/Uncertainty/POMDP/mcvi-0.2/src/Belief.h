#ifndef __BELIEF_H
#define __BELIEF_H

#include <cfloat>
#include <vector>
#include <map>
#include <list>
#include "State.h"
#include "Particle.h"
#include "Utils.h"

class Action;
class Obs;
class RandStream;
struct BeliefNode;

/**
   @class Belief
   @brief Interface for actual belief
   @provide sample(), nextBelief(Action&,Obs&)
   @details Used as node in the belief tree. Actual belief not
   represented here, inherit from this class to implement different
   types of beliefs.

   @author Wee Sun Lee
   @date 22 August 2009
*/

/**
   Same distribution belief will be checked by BeliefSet.h
   implementation. Make sure your belief is comparable.
*/

class BeliefIterator;

class Belief
{
  public:

    /**
       Initialize the beliefNode associated with this belief
       @param [in] beliefNode The beliefNode
    */
    Belief(BeliefNode* beliefNode): beliefNode(beliefNode) {}

    /**
       A pure virtual destructor that does nothing. The job of delete
       the pointer is transfered to the derived class. The implementation
       need to define the destructor for Belief first.
    */
    virtual ~Belief() {}

    /**
       Sample from this belief. Need to be implemented by classes
       that inherit from this class for specific belief types
       @return a state sampled from this belief
       @param [in] randSource Source of random number
    */
    virtual Particle sample(RandStream& randStream) const = 0;

    /**
       A hack to get the same particle every time
    */
    virtual Particle sample(long index, RandStream& randStream) const = 0;

    virtual State average() const = 0;

    /**
       Compute the next belief given an action and an observation.
       @param [in] act The action
       @param [in] obs The observation
    */
    virtual Belief* nextBelief(const Action& act, const Obs& obs, bool useSameRandSeed = true) const = 0;

    BeliefNode* beliefNode;

    class BeliefItImpl
    {
      public:
        virtual BeliefItImpl* clone() = 0;
        virtual void operator++() = 0;
        virtual bool operator!=(BeliefItImpl const& right) = 0;
        virtual Particle const& operator*() = 0;
        virtual Particle const* getPointer() = 0;
        virtual ~BeliefItImpl() {}
    };

    class Iterator:
            public std::iterator<std::forward_iterator_tag, Particle>
    {
        BeliefItImpl* impl_;
      public:
        Iterator(): impl_() {}
        Iterator(BeliefItImpl* impl): impl_(impl) {}
        Iterator(Iterator const& right): impl_(right.impl_->clone()) {}
        ~Iterator() { delete impl_; }

        bool operator!=(Iterator const& right)
        {
            return *impl_ != *(right.impl_);
        }

        Iterator& operator=(Iterator const& right)
        {
            delete impl_;
            impl_ = right.impl_->clone();
            return *this;
        }

        Iterator& operator++()
        {
            ++(*impl_);
            return *this;
        }

        Particle const& operator*()
        {
            return *(*impl_);
        }

        Particle const* operator->()
        {
            return impl_->getPointer();
        }
    };

    typedef Iterator const_iterator;

    virtual const_iterator begin(int num_particles) const = 0;
    virtual const_iterator begin(int num_particles, RandStream& randStream) const = 0;
    virtual const_iterator end() const = 0;
};


#endif //__BELIEF_H
