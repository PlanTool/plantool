/* External Priority queue                       *
 * based on Dial's representation for fixed size *
 * maximum edge cost                             */

#ifndef _EXT_PRIO_QUEUE_H
#define _EXT_PRIO_QUEUE_H

#include <stdlib.h>
#include <limits.h>
#include "ff.h"
#include "memory.h"
#include "search.h"
#include "file.h"
#include "bucket.h"
#include "duplicates_removal.h"

typedef struct _Dial_PQ {
  int  inserted;          /* total states inserted    */
  int  expanded;          /* total states expanded    */
  int  duplicates;        /* total duplicates         */
  Bool active;            /* true = index is valid    */
  int  total_bfs_levels;  /* BFS levels due to 0 cost */
  int  current_bfs_level; /* Current BFS Level        */
  int  index_bucket_pool; /* index in bucket pool     */
  int  duplicates_within;
  int  duplicates_top_layers;
} Dial_PQ;
 
typedef struct _external_prio_queue {
  Bucket** buckets;  /* the pool of buckets */
  Dial_PQ *dial;     /* |dial| >= |buckets| */ 
  int current_index; /* current index in dial */
		     /* buckets[dial[i].index_bucket_pool] has the states with cost = i */
  DR *dr;            /* it is better to keep dr here */
  int size;          /* size = |dial| */
  int no_of_buckets; /* no_of_buckets = |buckets| */
} external_prio_queue;

/* PQ management */
Bool allocate_pq(external_prio_queue *pq, int pq_size, int no_of_buckets );
Bool free_pq(external_prio_queue *pq);
void init_dial_struct(external_prio_queue *pq, int index);

/* PQ handles */
State* del_min(external_prio_queue *pq);
Bool insert_in_pq(external_prio_queue *pq, State* S, State *father, int operator);
int get_empty_bucket_index(external_prio_queue *pq);

#endif
