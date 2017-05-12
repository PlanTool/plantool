#include "external_prio_queue.h"
#include "external.h"
#include "duplicates_removal.h"
#include "bucket.h"

/*#define DEBUG_PQ*/
/* PQ management */

static Bool first_insert = TRUE;

/*
 * It estimates the size of a single bucket. 
 * use make_state to get a state size
 */
int estimate_bucket_size(int no_of_buckets){
  return BUCKETSIZE;
}

void init_dial_struct(external_prio_queue *pq, int index){
  (pq->dial[index]).inserted = 0;          /* total states inserted    */
  (pq->dial[index]).expanded = 0;          /* total states expanded    */
  (pq->dial[index]).duplicates = 0;        /* total duplicates         */
  (pq->dial[index]).active = 0;            /* true = index is valid    */
  (pq->dial[index]).total_bfs_levels = 0;  /* BFS levels due to 0 cost */
  (pq->dial[index]).current_bfs_level = 0; /* Current BFS Level        */
  (pq->dial[index]).index_bucket_pool = pq->no_of_buckets; /* index in bucket pool     */
  (pq->dial[index]).duplicates_within = 0;
  (pq->dial[index]).duplicates_top_layers = 0;
}

Bool allocate_pq(external_prio_queue *pq, int pq_size, int no_of_buckets ){
  pq->size = pq_size;
  pq->no_of_buckets = no_of_buckets;
  pq->current_index = 0;
  printf("Allocating the priority queue of size %d and bucket pool of size %d ", pq->size,
	 pq->no_of_buckets);
  pq->dial    = (Dial_PQ*)calloc(pq->size, sizeof(Dial_PQ));
  int i;
  for (i = 0; i < pq->size; i++){
    init_dial_struct(pq, i);
  }
  pq->buckets = (Bucket**)calloc(pq->no_of_buckets, sizeof(Bucket**));
  int bucket_size = estimate_bucket_size(pq->no_of_buckets);
  for (i = 0; i < pq->no_of_buckets; i++){
    pq->buckets[i] = ( Bucket * ) calloc( 1, sizeof( Bucket ) ); 
    allocate_bucket(pq->buckets[i], bucket_size);

    pq->buckets[i]->size = 0;
    allocate_states_in_bucket(pq->buckets[i]);
    initialize_bucket(pq->buckets[i], BUCKET_NOT_IN_USE, 0, TRUE);
    printf ("[%d] ", i);
  } 
  printf(".. done \n");

  printf("Allocating the duplicates removal module ... ");
  pq->dr =  malloc(sizeof(DR));
  init_DuplicatesRemoval(pq->dr);
  printf("done\n");

  return TRUE;
}


Bool free_pq(external_prio_queue *pq){
  return TRUE;
}

int get_empty_bucket_index(external_prio_queue *pq){

  int i, empty_bucket_index = pq->no_of_buckets;

  for (i=0; i< pq->no_of_buckets; i++){
    /* printf("B[%d] => %d; ", i, (pq->buckets[i])->g);*/
    if ((pq->buckets[i])->g == BUCKET_NOT_IN_USE){
      empty_bucket_index = i;
      break;
    }
  }
   if (empty_bucket_index == pq->no_of_buckets){
    printf("FATAL ERROR! Recompile with a bigger bucket pool (> %d)!\n",pq->no_of_buckets);
    exit(0);
  }
  return empty_bucket_index;
}


unsigned int remove_duplicates(external_prio_queue *pq, int g,int  h){

  /* run a duplicates removal module here */
  unsigned int elements_written = 
    mergeAndRemoveDuplicates(pq->dr, g, h, 1,
			     &pq->dial[g].duplicates_within);
  
  elements_written = 
    subtractFilesOfAboveLayers(pq->dr, g, h, LOCALITY, 
			       &pq->dial[g].duplicates_top_layers, 0, g);
  /*
    print_statistics(depth, read_bucket->inserted, 
    write_bucket->inserted, write_bucket->duplicates, 
    *duplicates_within, 
    *duplicates_toplayers, expanded, upper_bound, 
    goal_depth, elements_written);*/
 
  return elements_written;
}

/* 
 * Only for debuging. Prints the status of the priortiy queue. 
 */
void print_PQ(external_prio_queue *pq){
  int i;
  for (i=0; i< pq->current_index+10; i++){
    printf("%d => %d ;; ", i, 
	   (pq->dial[i]).index_bucket_pool);
    if (i % 10 == 0) printf("\n");

  }
  printf("\n\n");

}

/* PQ handles */
State* del_min(external_prio_queue *pq){

  /* if the bucket is in use, flush it and remove duplicates from it */
#ifdef DEBUG_PQ
  printf("Trying to get the state from current_index = %d; dial[%d].index = %d .. ", 
	 pq->current_index,  pq->current_index,  
	 (pq->dial[pq->current_index]).index_bucket_pool);
#endif
  State* state = get_next(pq->buckets[(pq->dial[pq->current_index]).index_bucket_pool]);
  if    (state){
#ifdef DEBUG_PQ
    printf("success!\n");
#endif
    return state;
  }

#ifdef DEBUG_PQ
    printf("failed!\n");
#endif
  /* state = 0 => bucket is empty. Make it usable again */
  initialize_bucket(pq->buckets[(pq->dial[pq->current_index]).index_bucket_pool], 
    BUCKET_NOT_IN_USE, 0, TRUE);
  (pq->dial[pq->current_index]).index_bucket_pool = pq->no_of_buckets;
  
#ifdef DEBUG_PQ
  printf("Made the bucket usable again\n");
#endif
  int g = 0, empty_bucket_index;
  unsigned int elements_written;
  
  /* Search the next bucket that is non-empty*/
  for (g = pq->current_index+1; g < pq->size; g++){
    if (pq->dial[g].inserted > 0){
      /* is there a bucket corresponding to this g-value ? */
      if (pq->dial[g].index_bucket_pool < pq->no_of_buckets){
	  flush_bucket(pq->buckets[(pq->dial[g]).index_bucket_pool]);
	  empty_bucket_index = (pq->dial[g]).index_bucket_pool;
      }
      else {
	empty_bucket_index = get_empty_bucket_index(pq);
      }
      
      elements_written = remove_duplicates(pq, g, 0);
      
      if (elements_written > 0){
	initialize_bucket(pq->buckets[empty_bucket_index], g, 0, FALSE);
	(pq->dial[g]).index_bucket_pool = empty_bucket_index;
	/*printf("Breaking the loop at g=%d \n", g);*/
	break;
      }
    }
  }

  
  if (g == pq->size)
    state = (State*) NULL;
  else {
    /*printf("deleting min from g=%d \n", g);*/
    pq->current_index = g;
    state = get_next(pq->buckets[(pq->dial[pq->current_index]).index_bucket_pool]);  
  }
  return state;
}

Bool insert_in_pq(external_prio_queue *pq, State* S, State *father, int operator){

  int bucket_index = (pq->dial[(int)S->cost]).index_bucket_pool;
  
  if (bucket_index == pq->no_of_buckets){
    bucket_index = get_empty_bucket_index(pq);
    (pq->dial[(int)S->cost]).index_bucket_pool = bucket_index;
    initialize_bucket(pq->buckets[(pq->dial[(int)S->cost]).index_bucket_pool], 
		      (int)S->cost, 0, TRUE);
  }

  insert_in_bucket(pq->buckets[(pq->dial[(int)S->cost]).index_bucket_pool], S, father, operator);

  (pq->dial[(int)S->cost]).inserted++;

#ifdef DEBUG_PQ
  printf("Total elements inserted in bucket[%d] for cost %d = %d\n",
	 pq->dial[(int)S->cost].index_bucket_pool,   (int)S->cost,
	 (pq->dial[(int)S->cost]).inserted);
#endif
  if(first_insert){
    flush_bucket(pq->buckets[(pq->dial[(int)S->cost]).index_bucket_pool]);
    initialize_bucket(pq->buckets[(pq->dial[(int)S->cost]).index_bucket_pool], 
		      (int)S->cost, 0, TRUE);
    first_insert = FALSE;
  }
  return TRUE;
}

