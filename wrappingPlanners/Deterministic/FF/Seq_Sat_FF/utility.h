#ifndef _UTILITY_H_
#define _UTILITY_H_

#include "ff.h"

Bool LESS( float a, float b );
float PLUS( float a, float b );
float MAX( float a, float b);
float MIN( float a, float b);
float get_cost(int effect);
float get_implied_cost(int effect);
void print_state( State *S );
Bool effect_deletes_fact(int effect, int fact);
Bool effect_adds_fact(int effect, int fact);
void add_fact( State *S, int fact );
void remove_fact( State *S, int fact );
Bool array_contains_int(int *array, int num_elems, int key);
Bool ROUGHLY_EQUAL( float a, float b);

#endif
