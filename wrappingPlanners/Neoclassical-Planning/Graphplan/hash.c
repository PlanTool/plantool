/*********************** Graphplan **************************************
  (C) Copyright 1995 Avrim Blum and Merrick Furst

  All rights reserved. Use of this software is permitted for non-commercial
  research purposes, and it may be copied only for that use.  All copies must
  include this copyright message.  This software is made available AS IS, and
  neither the authors nor CMU make any warranty about the software or its 
  performance. 
*************************************************************************/

#include "graphplan.h"

/****This file contains a lot of stuff related to creation of the graph****/

/************* hash table functions ***************
 *
 * A hash table has type hashtable_t and is an array of lists of type
 * element_t.  A element_t is a pointer to a element_s structure.
 * A vertex_s structure contains a "name" field that is also the key.
 *
 * Hashing Method:  (key mod BIGPRIME) mod HSIZE.
 * Use result of first mod to do quick comparisons...
 *
 * lookup returns a pointer to the element_s structure or NULL.
 * insert creates a new element_s structure and inserts the name, returning
 *   the pointer.  Allocates new space for the name.
 * An element_s is the same as a vertex_s.  Same for element_t and vertex_t
 */
int num_created;
int hash(char *key, int size);

#define BIGPRIME 8000977
/* extern int NUMINTS; */

/**FOR BETTER PRINTING: remember to take out!!!!**/
/* #define HSIZE1 1                             */
#define HSIZE1 HSIZE

int hash(char *key, int size)
{
  int h;
  for(h=0; *key != '\0'; key++)  h = (h*256 + (*key)) % size;
  return h;
}
element_s * lookup_from_table(hashtable_t htable, char *key)
{
  element_t l;
  int hval = hash(key, BIGPRIME);
  int index = hval % HSIZE1;
  for(l = htable[index]; l != NULL; l = l->next) {
    if (hval == l->hashval && strcmp(key,l->name) == SAME) return l;
  }
  return NULL;
}

/* inserts token list but ONLY IF IT'S NOT THERE ALREADY. Returns ptr. */
element_t insert_token_list(hashtable_t htable, token_list t)
{
  char str[100];
  element_t retval;
  instantiate_into_string(t, NULL, str,1);
  if ((retval = lookup_from_table(htable,str)) == NULL) 
    retval = insert_into_table(htable, str);
  return retval;
}

element_t insert_into_table(hashtable_t htable, char *key)
{
  element_t l;
  int hval = hash(key, BIGPRIME);
  int index = hval % HSIZE1;
  l = (element_t) calloc(1, sizeof(element_s));
  l->name = (char *) calloc(1 + strlen(key), sizeof(char));
  strcpy(l->name, key);
  l->hashval = hval;
/*   l->exclusive_vect = (int *) calloc(NUMINTS, sizeof(int)); */
  l->next = htable[index];
  htable[index] = l;
  ++num_created;  /**global, use for info purposes **/
  return l;
}


/* if flag=0, just reset. Otherwise,
 * go from where you left off last (use static vars to hold state).
 * Return NULL if no more.
 * NOTE: this is DANGEROUS the way it's written since can't interleave
 * with different hash tables, etc.  Should really have flag be state.
 */
element_t get_next(hashtable_t h, int flag)
{
  static int i;
  static element_t current;
  element_t temp;
  if (flag==0) {i = 0; current = h[0]; return NULL;}
  while(1) {
    if (current != NULL) {
      temp = current;
      current = current->next;
      return temp;
    } else {
      if ((++i) >= HSIZE1) return NULL;
      current = h[i];
    }
  }
}

