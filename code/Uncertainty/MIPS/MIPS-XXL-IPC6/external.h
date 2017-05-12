#ifndef _EXTERNAL_H
#define _EXTERNAL_H

#include "ff.h"

#define BUCKETSIZE 1000000
#define PLAN_BUCKETSIZE 20000
#define BUCKET_NOT_IN_USE -10
#define LOCALITY 2000
#define EARLY_MERGE_LOCALITY 2

/*
 * After that many flushes "early merge" should start
 */
#define EARLY_MERGE_BUFFERS 400

/*
 * This value should be inferred from the problem and not hard-coded.
 */
#define MAX_EDGE_COST 10;


Bool do_external_breadth_first_search(void);
Bool do_external_enforced_hill_climbing(void);
void do_external_merge(void);
Bool do_external_dijkstra(void);

Bool do_external_branch_and_bound(Bool);

void write_goal(State *goal, State *pred, int depth);
float goal_cost(State *goal, State *pred, int depth);
Bool is_goal(State *goal);


#endif
