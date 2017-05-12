#include <cstdlib>
#include <cassert>
#include <cmath>
#include <iostream>
#include "ParticlesBeliefSet.h"
#include "ParticlesBelief.h"

using namespace std;

ParticlesBeliefSet::ParticlesBeliefSet() {}

/**
   Destroys stored beliefs.
*/
ParticlesBeliefSet::~ParticlesBeliefSet()
{
    for (std::set<ParticlesBelief*, beliefComp>::iterator it = pBSet.begin();
         it != pBSet.end(); ++it)
        delete *it;
    pBSet.clear();
}


bool BeliefSet::beliefComp::operator()(const Belief* belief1, const Belief* belief2) const
{
    const ParticlesBelief
            *b1 = safe_cast<const ParticlesBelief*>(belief1),
            *b2 = safe_cast<const ParticlesBelief*>(belief2);
    // HUY's note - belief is a vector of particle, which consists of state and pathlength
    if (b1->belief.size() < b2->belief.size()) return true;
    else if (b1->belief.size() > b2->belief.size()) return false;

    for (long i = 0; i < (long)b1->belief.size(); i++){
        if (b1->belief[i].pathLength < b2->belief[i].pathLength) return true;
        else if (b1->belief[i].pathLength > b2->belief[i].pathLength) return false;
        for (long j=0; j < (long)b1->belief[i].state.size(); j++){
            if ( b1->belief[i].state[j] <  b2->belief[i].state[j]) return true;
            else if ( b1->belief[i].state[j] >  b2->belief[i].state[j]) return false;
        }
    }
    return false;
}

pair<Belief*,bool> ParticlesBeliefSet::insert(Belief* belief)
{
    ParticlesBelief *ptr = safe_cast<ParticlesBelief *>(belief);
    if (ptr == NULL){
        cerr << "Illegally casting into ParticlesBelief \n";
        exit(EXIT_FAILURE);
    }

    pair<set<ParticlesBelief*, BeliefSet::beliefComp>::iterator, bool> ret = pBSet.insert(ptr);

    return pair<Belief*, bool>(*ret.first,ret.second);
}
