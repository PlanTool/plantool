#ifndef _LIST_SET_H_
#define _LIST_SET_H_

#include <string.h>
#include "ff.h"

typedef struct _node {
  int val;
  struct _node *next;
} node, *node_pointer;

typedef struct _list_set {
  int n_elems;
  node_pointer first;
} list_set, *list_set_pointer;

#define SET_SIZE sizeof(list_set)
#define NODE_SIZE sizeof(node)

typedef list_set set;
typedef list_set_pointer set_pointer;

void set_empty(set_pointer);
void init_set(set_pointer);
void copy_set(set_pointer, set_pointer);
void set_union(set_pointer, set_pointer);
void set_intersection(set_pointer, set_pointer);
void add_elem(set_pointer, int);
void remove_elem(set_pointer, int );
Bool contains_elem(set_pointer, int);
Bool remove_one_and_return(set_pointer, int*);
int num_elems(set_pointer);

/* return num_elements, put a malloced array
 * in the second parameter
 */

int get_as_array(set_pointer, int**); 

void print_set(set_pointer);

void init_iterator(list_set_pointer set, void **track);
int get_val(list_set_pointer set, void **current);

#endif 
