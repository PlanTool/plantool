#include "list_set.h"

void free_nodes(node_pointer np) {
  if(np->next) {
    free_nodes(np->next);
  }
  free(np);
}

void set_empty(list_set_pointer set) {

  if(set->first) {
    free_nodes(set->first);
  }
  set->n_elems = 0;
  set->first = NULL;
}

void init_set(list_set_pointer set) {
  set->n_elems = 0;
  set->first = NULL;
}

void copy_set(list_set_pointer from, list_set_pointer to) {

  node_pointer from_np = from->first, *to_np;

  set_empty(to);

  to_np = &(to->first);

  while (from_np) {
    *to_np = malloc(NODE_SIZE);
    (*to_np)->val = from_np->val;
    (*to_np)->next = NULL;
    to_np = &((*to_np)->next);
    from_np = from_np->next;
  }

  to->n_elems = from->n_elems;
}

void add_elem(list_set_pointer set, int val) {

  node_pointer prev = NULL, current = set->first, temp;

  while (current && (current->val < val)) {
    prev = current;
    current = current->next;
  }

  if(current) {
    if(current->val == val) {
      return;
    }
    if(!prev) {
      temp = malloc(NODE_SIZE);
      temp->next = set->first;
      set->first = temp;
      temp->val = val;
      set->n_elems++;
      return;
    }
  }

  if(!prev) {
    set->first = malloc(NODE_SIZE);
    set->first->val = val;
    set->first->next = NULL;
    set->n_elems++;
  }
  
  else {
    prev->next = malloc(NODE_SIZE);
    prev->next->next = current;
    prev->next->val = val;
    set->n_elems++;
  }
}

void set_union(list_set_pointer result, list_set_pointer operand) {

  node_pointer op_current = operand->first, temp;
  node_pointer *result_current = &(result->first);
  
  while(op_current) {

    /* here we need val from op to go before current result val */
    if(!(*result_current) || ((*result_current)->val > op_current->val)) {
      temp = malloc(NODE_SIZE);
      temp->val = op_current->val;
      temp->next = *result_current;
      *result_current = temp;
      result_current = &(temp->next);
      op_current = op_current->next;
      result->n_elems++;
    }
    else if ((*result_current)->val == op_current->val) {
      op_current = op_current->next;
    }
    else {
      result_current = &((*result_current)->next);
    }
  }
}

void set_intersection(list_set_pointer result, list_set_pointer operand) {

  node_pointer op_current = operand->first, temp;
  node_pointer *result_current = &(result->first);

  while(*result_current) {

    if(op_current) {
      if(op_current->val <= (*result_current)->val) {
	if(op_current->val == (*result_current)->val) {
	  result_current = &((*result_current)->next);
	}
	op_current = op_current->next;
      }
      else { 
	temp = (*result_current)->next;
	free(*result_current);
	*result_current = temp;
	result->n_elems--;
      }
    }

    else {
      temp = (*result_current)->next;
      free(*result_current);
      *result_current = temp;
      result->n_elems--;
    }
  }
}


void remove_elem(list_set_pointer set, int val) {

  node_pointer prev = NULL, current = NULL;
  
  if(!set->first) return;

  current = set->first;
  
  while(current && (current->val < val)) {
    prev = current;
    current = current->next;
  }

  if(!current || (current->val != val)) {
    return;
  }

  else {
    if(prev) {
      prev->next =  current->next;
    }
    else {
      set->first = current->next;
    }
    free(current);
    set->n_elems--;
  }
}

Bool contains_elem(list_set_pointer set, int val) {
  
  node_pointer np = set->first;

  while(np && (np->val < val)) {
    np = np->next;
  }

  if(np && (np->val == val)) {
    return TRUE;
  }

  return FALSE;
}

int num_elems(list_set_pointer set) {
  return set->n_elems;
}

void print_set(list_set_pointer set) {

  printf("num elems = %d:\t", set->n_elems);

  node_pointer np = set->first;

  while(np) {
    printf("%d, ", np->val);
    np = np->next;
  }
  printf("\n");
}

Bool remove_one_and_return(list_set_pointer set, int *rv) {
  
  if(!set->first) {
    return FALSE;
  }
  else{
    *rv = set->first->val;
    set->first = set->first->next;
    set->n_elems--;
    return TRUE;
  }
}

int get_as_array(list_set_pointer set, int **result) {
  
  int *rv = calloc(set->n_elems, sizeof(int));
  int i;

  *result = rv;

  node_pointer temp = set->first;

  for(i = 0; temp; i++) {
    rv[i] = temp->val;
    temp = temp->next;
  }
  
  return set->n_elems;  
}

void init_iterator(list_set_pointer set, void **track) {
  *track = set->first;
}

int get_val(list_set_pointer set, void **current) {
  int rv = ((node_pointer)*current)->val;
  *current = ((node_pointer)*current)->next;
  return rv;
}
