#ifndef _PLAN_RECONSTRUCT
#define _PLAN_RECONSTRUCT


#include "ff.h"
#include "bucket.h"
#include "search.h"

#define MAX_H 1000


Bool reconstruct_plan(Bucket *b, State *goal, State *pred, int pred_g, int pred_h);
Bool reconstruct_plan_dijkstra(Bucket* b, State *goal, State *pred_goal, int pred_g, int pred_h);
#endif
