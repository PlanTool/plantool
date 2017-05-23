#ifndef __JUSTIFY_H_
#define __JUSTIFY_H_

#define NECESSARY 3
#define ADDED	  4
#define PRUNED    5
#define USELESS   -2

int remove_action (edgelist_t *pruned_act, edgelist_t *changed_fact, 
		   vertex_t act, int stime);
void restore_action (edgelist_t *pruned_act, edgelist_t *changed_fact, 
		     int maxtime);
void justify_plan (int maxtime);
void justify_operator (vertex_t sv, int starttime, int maxtime);
void mark_goals_as_necessary (int maxtime);


#endif
