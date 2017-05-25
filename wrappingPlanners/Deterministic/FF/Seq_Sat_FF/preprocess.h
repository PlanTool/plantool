#ifndef _PREPROCESS_H_
#define _PREPROCESS_H_

#include "ff.h"

void calculate_h2(State *);
void print_h2_pairs(void);
void print_h2_singles(void);
void print_h2_info(void);

float pairwise_eval(int *goal_facts, int num_goal_facts);
float get_pair_value(int f1, int f2);

#endif
