/*
 * $Id: BrazilState.cpp 107 2006-07-27 03:31:12Z owen $ 
 * 
 * This file is part of the Brazil Planner. Copyright NICTA 2006.
 *
 * This file is commercial in confidence. You should no be looking at
 * it unless you are an employee of NICTA, a student who has signed
 * their IP to NICTA, or you have signed an NDA covering the Brazil
 * source code. If you are not one of these people we will poke out
 * your eyes with a gerkhin while forcing you to sing the Brazilian
 * national anthem.
 * 
 */

#ifndef _BrazilStateUpdater__h
#define _BrazilStateUpdater__h

#include"DOMDuration.h"
#include"DOMAtomicEffectVisitor.h"

#include"BrazilStateHelpers.h"

/**
 * Purpose of this class is to do the high level modification of the state.
 * Unlike BrazilStateHelpers the routines in this class will change the
 * predicates, resources, and the event queue. These routines are called
 * from the BrazilSimulator only. Again, these routines could be part of 
 * a monolithic state class, but this is a little saner.
 */
class BrazilStateUpdater : public BrazilStateHelpers {


 public:

    typedef std::map<int, DOMAction*> Eligible;
    typedef Eligible::const_iterator EligibleCIt;

 private:

    /** 
     * Keep a copy of the domain handy
     */
    DOMDomain* domain;

    /**
     * Keep track of what's eligble. use a map so that we can 
     * map actions quickly to their numerical agent number. This will
     * be important with thousands of actions.
     */
    Eligible eligible;

    /** 
     * We know about maximum plan duration 
     */
    time_t maxDuration;

    bool updateEligible();
    bool processBatchOfEvents(bool median);
    void processEvent(BrazilEvent* e, bool median);
    bool processStartAction(DOMAction* a, int actionCopy, bool median);
    void processEndAction(DOMAction* a, int actionCopy, bool median);
    void processStartOutcome(DOMOutcome* o, int actionCopy, bool median);
    void processEndOutcome(DOMOutcome*);
    void processEffect(DOMEffectSet* e);

    /**
     * Necassary to treat functions versus predicates properly 
     * when processing effects. Note the method of passing in
     * the pointer to BrazilState::this via the 
     * BrazilStateUpdater constructor.
     */
    class EffectVisitor: public DOMAtomicEffectVisitor {
	BrazilStateHelpers* state;
    public:
	EffectVisitor(BrazilStateHelpers* bs) : state(bs) {};
	virtual ~EffectVisitor() {};
	virtual bool visitFunctionEffect(DOMFunctionEffect*);
	virtual bool visitPredicateEffect(DOMPredicateEffect*);
    } effectVisitor;


    
 public:

    BrazilStateUpdater(DOMDomain* d, time_t md);
	
    BrazilStateUpdater& operator=(const BrazilState& srcState);
    
    bool processEvents(bool median);
	
    void startAction(DOMAction* a);

    /** 
     * Allow simulator access to the list of currently eligible tasks
     * @return a map of eligible action indicies to actions
     */
    Eligible& getEligible() { return eligible; }

    bool replayEffects(time_t upTo);

};
#endif
