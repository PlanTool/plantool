#include <string.h>
#include <math.h>

#include "utility.h"
#include "ff.h"
#include "output.h"

extern Bool artificial_gtt;

Bool LESS( float a, float b ) {

  if ( a == INFINITY ) {
    return FALSE;
  }

  if ( b == INFINITY ) {
    return TRUE;
  }

  return ( a < b ? TRUE : FALSE );
}

float MAX( float a, float b) {

  if( (a == INFINITY) || (b == INFINITY)) return INFINITY;
  if( a > b ) return a;
  return b;
}

Bool ROUGHLY_EQUAL( float a, float b) {

  return (abs(a - b) < EPSILON);
}

float MIN( float a, float b) {
  
  if(a == INFINITY) return b;
  if(b == INFINITY) return a;
  if(a < b) return a;
  return b;

}

float PLUS( float a, float b )

{

  if (( a == INFINITY ) || ( b == INFINITY )) {
    return INFINITY;
  }
 
  return ( a + b );
}

float get_cost(int effect) {
  /* if we have artificial_gtt, this implies that gtt
   * was 0 anyway. */
  if ( artificial_gtt ) {
    return gop_conn[gef_conn[effect].op].cost;
  }
  else {
    return gop_conn[gef_conn[effect].op].cost + gtt;
  }
}

void print_state( State *S )

{

  int i;

  printf("\n{");

  for ( i = 0; i < S->num_F; i++ ) {
    print_ft_name( S->F[i] );
    printf(", ");
  }
  printf("}\n");

}

Bool effect_deletes_fact(int effect, int fact) {
  int i;
  for(i = 0; i < gef_conn[effect].num_D; i++) {
    if(gef_conn[effect].D[i] == fact)
      return TRUE;
  }
  return FALSE;
}

Bool effect_adds_fact(int effect, int fact) {
  int i;
  for(i = 0; i < gef_conn[effect].num_A; i++) {
    if(gef_conn[effect].A[i] == fact)
      return TRUE;
  }
  return FALSE;
}

void add_fact(State *S, int fact) {

  int i;

  for(i = 0; i < S->num_F; i++) {
    if (S->F[i] == fact) {
      return;
    }
  }

  S->F[S->num_F++] = fact;
}

void remove_fact(State *S, int fact) {
  
  int i, j;

  for(i = 0; i < S->num_F; i++) {
    if (S->F[i] == fact) {
      for (j = i; j + 1 < S->num_F; j++) {
	S->F[j] = S->F[j + 1];
      }
      S->num_F--;
      return;
    }
  }
}

Bool array_contains_int(int *array, int num_elems, int key) {

  int i;

  for(i = 0; i < num_elems; i++) {
    if(array[i] == key)
      return TRUE;
  }
  return FALSE;
}
