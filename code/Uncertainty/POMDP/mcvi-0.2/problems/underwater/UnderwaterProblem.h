
#ifndef __UNDERWATERPROBLEM_H
#define __UNDERWATERPROBLEM_H

#include "Model.h"
#include <utility>
#include <set>

/**
    TODO: write a good description
*/
struct UnderwaterProblem
{
    double discount;
    long xSize;
    long ySize;
    std::vector< std::vector<char> > map;
    State initialBelief;
    std::set<std::pair<long, long> > destinations;
    std::vector<State> initialBeliefStates;
};

#endif // __ROCKSAMPLEPROBLEM_H





