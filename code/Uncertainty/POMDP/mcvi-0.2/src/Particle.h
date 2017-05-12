#ifndef __PARTICLE_H
#define __PARTICLE_H

#include <vector>
#include "State.h"

struct Particle
{
    State state;
    double weight;
    long pathLength;

    Particle(State st, long pL, double weight): state(st),
                                                weight(weight),
                                                pathLength(pL) {}
    Particle() {}
};

struct ParticleStore
{
    // Total sum of all immediate reward (from calling model.sample())
    // of all particle
    double currSum;
    std::vector<Particle> particles;
};

#endif
