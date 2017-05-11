/************************************************************************************
   hash.c: Contains functions to manage the hash table
************************************************************************************/

#include "gp_csp.h"
#include "../Share/global.h"


hash_item *hash_table;
item_t hash_list;


/*
 * Function to construct the real hash_table as array from the linked_list
 * that is dynamically created in generate()
 */
void
construct_hash(num_entry )
     int num_entry;
{
  item_t temp;

  /* Create the hash table and put in the entries */
  hash_table = (hash_item *) new_calloc( num_entry+1, sizeof(hash_item) );

  temp = hash_list;
  while( temp ) {
    hash_table[temp->index].first = temp->first;
    temp = temp->next;
  } 

  /* Free the linked list */
  while( hash_list ) {
    temp = hash_list;
    hash_list = hash_list->next;
    free(temp);
  }
}

/*
 * Function to free the linked_list intermediate structure after build the
 * real hashtable
 */

void
free_hash( )
{
  free(hash_table);
}

/*
 * Function to put the new concate the new item into the linked_list
 */
void
put_in_hashtable(index, value1)
     int index;
     int value1;
{
  static item_t tail;
  item_t temp;

  temp = (item_t) new_calloc(1, sizeof(item_s));
  temp->index = index;
  temp->first = value1;

  if( index == 0 ) {
    hash_list = temp;
    tail = hash_list;
  } else {
    tail->next = temp;
    tail = tail->next;
  }
}

/*
 * Functions to return the first, or second value of the hash entry from the table
 */
int
get_1st_value_from_hashtable(index)
     int index;
{
  return hash_table[index].first;
}
