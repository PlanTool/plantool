#ifndef _DISTANCES_H_
#define _DISTANCES_H_

#include "ff.h"
#include "utility.h"

void compute_a_distances(void );
void make_minimal_state(State *S, int effect);
Bool edeletes(int effect, int fact);
float get_goal_distance(int effect);

#endif
