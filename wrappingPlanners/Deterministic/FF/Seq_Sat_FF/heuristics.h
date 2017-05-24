#ifndef _LABEL_HEURISTIC_H_
#define _LABEL_HEURISTIC_H_

#include <string.h>
#include "ff.h"
#include "list_set.h"

typedef set label;
typedef set_pointer label_pointer;

/* functions that actually compute stuff */
/* if collect_helpful, gH etc. will be set for use by search. */
/* applicable actions in gA etc. will be set in both cases. */
float eval(State *start, int *num_acts, Bool collect_helpful);

void compute_labels(State *start);
void compute_hadd(State *start);
void compute_landmarks(State *start);

/* - gets the actions from helpful_actions
 * - sets is_goal for facts  
 * - orders them according to the ff h
 * - sets the global vars
 */
void set_helpful_actions(State *current_goals);

/* stuff stolen shamelessly from FF */

void reset_search_info_label( void );
void collect_H_info_label( void );
void achieve_goals_label( int time );
/*void initialize_goals_label( int max , State *current_goals);*/
/* no goal agenda in Metric FF so just use global goal array */
void initialize_goals_label( int max);

/* other */

Bool relaxed_plan_contains_effect(int effect);

void print_action_label(label_pointer lp);
void print_tsp_label(label_pointer lp);
void print_whole_label(int fact_index);
void print_landmark(int fact_index);
void print_all_landmarks();

#endif
