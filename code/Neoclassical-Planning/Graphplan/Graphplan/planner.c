/*********************** Graphplan **************************************
  (C) Copyright 1995 Avrim Blum and Merrick Furst

  All rights reserved. Use of this software is permitted for non-commercial
  research purposes, and it may be copied only for that use.  All copies must
  include this copyright message.  This software is made available AS IS, and
  neither the authors nor CMU make any warranty about the software or its 
  performance. 
*************************************************************************/


/***** planner.c: do the planning part.*********************************/


/*************************************************************************

  About completeness check.  The basic idea of the completeness check
  is as follows.  Say the graph has leveled off at level N.  Let S be
  the the collection of unsolvable goal-sets we have stored at level N
  (in "bad_table").  Notice two things.  First, any plan to achieve
  our true goals must have at some point achieved some goal-set in S.
  Second, if a time step has passed in which S has not grown larger,
  then the collection of goal-sets at level N+1 is the same as S (this
  is because the graph has level off, so you can think of a time step
  on which we fail as shifting everything to the right).  That means
  that in order to achieve some goal set in S, we must previously have
  already achieved some other goal set in S, implying there is an
  infinite loop and the problem is not solvable.


***************************************************************************/
  

#include "graphplan.h"

extern hashtable_t *fact_table, *op_table;  /* the arrays of hash tables */
extern int do_memo, do_subsets, do_lower, xviewing, do_buildup, do_completeness, do_greedy, do_heuristic;
extern fact_list the_goals;

/*stuff for completeness */
extern int same_as_prev_flag, first_full_time, num_hashes[2];

int setup(int maxtime);
void insert_bad(goal_arr a, int num, int time);
void insert_good(goal_arr a, int num, int time);
int lookup_bad(goal_arr a, int num, int time);
int lookup_good(goal_arr a, int num, int time);
int subset_lookup(goal_arr a, int num, int time);
int PARTIALsubset_lookup(goal_arr a, int num, int time); /*using THIS one now*/
int can_hope_to_solve(goal_arr g, int length, int time);
int build_up_to_plan(int num_goals, int time);
int constrict(int num_goals, int time);
int basicplan(int cindex, int nindex, int time);
int runbasicplan(int numgoals, int time);
void mark_exclusive(vertex_t v);
void unmark_exclusive(vertex_t v);

int hash_inserts = 0, hash_hits = 0, hash_ctr = 0, goodhash_hits = 0,
  hash_numctr = 0, subset_hits = 0;
extern int bvec_len;
int number_of_actions_tried = 0;
int for_printing;  /* if 1, then leave "used" flags in, else don't */
int global_maxtime;  /* global variable containing maxtime */
/*****use for storing current goals*****/
goal_arr *goals_at;


int do_plan(int maxtime)
{
  int num_goals, i, result;
  num_goals = setup(maxtime);
  global_maxtime = maxtime;
  printf("goals at time %d:\n  ",maxtime+1);
  for(i=0; i < num_goals; ++i) printf("%s ",goals_at[maxtime][i]->name);
  printf("\n\n");
  if (do_lower && !can_hope_to_solve(goals_at[maxtime],num_goals,maxtime)) {
    if (do_memo) insert_bad(goals_at[maxtime], num_goals, maxtime);
    return 0;
  }
  for_printing = 1;
  if (do_buildup) result = build_up_to_plan(num_goals, maxtime);
  else if (do_greedy) result = constrict(num_goals, maxtime);
  else result = runbasicplan(num_goals, maxtime);
  if (result == 0 && do_memo)insert_bad(goals_at[maxtime], num_goals, maxtime);
  if (result == 0 && xviewing == 1) { /* clean up the viewer */
    for(i=0; i < num_goals; ++i) {
      draw_fact(goals_at[maxtime][i],maxtime,0);
    }
  }
  return result;
}


/* Print out info.  The best match to a "state-space step" in something like
   Prodigy is somewhere between the number of set-creation steps and the
   number of actions tried. */
void print_plan(int maxtime)
{
  int i;
  vertex_t v;
  for(i=0; i < maxtime; ++i) {
    get_next(op_table[i],0);
    while((v = get_next(op_table[i],1)) != NULL) {
      if (v->used && !IS_NOOP(v)) printf("%d %s\n",i+1,v->name);
    }
  }
  if (do_memo) { /* print out some useful info */
    printf("%d entries in hash table, ",hash_inserts);
    if (hash_inserts == 0) printf("\n");
    else printf("%d hash hits, avg set size %d.\n",
		hash_hits, hash_numctr/hash_ctr);
    printf("%d total set-creation steps (entries + hits + plan length - 1).\n",
	   hash_inserts + hash_hits + maxtime - 1);
  }
  if (do_subsets) printf("%d subset hits.\n", subset_hits);
  if (do_greedy) printf("%d hits on doable sets\n", goodhash_hits);
  printf("%d actions tried\n", number_of_actions_tried);
}


/******Routines for checking proactively if we can stop already **********/

/* when you make a decision, immediately
   mark each op/noop as not doable, by doing MARK(v) (which adds 1 to
   v->cant_do).  Test by doing IS_MARKED(v).
   */
#define MARK(v) ((v)->cant_do += 1)
#define UNMARK(v) ((v)->cant_do -= 1)
#define IS_MARKED(v) ((v)->cant_do > 0)

void mark_exclusive(vertex_t v)
{
  edgelist_t e;
  for(e = v->exclusive; e; e = e->next) MARK(e->endpt);
}

void unmark_exclusive(vertex_t v)
{
  edgelist_t e;
  for(e = v->exclusive; e; e = e->next) UNMARK(e->endpt);
}

/**** see if remaining goals still possible **/
int goals_still_possible(int index, int time)
{
  int i;
  edgelist_t e;
  for(i=0; i < index; ++i) {
    for(e = goals_at[time][i]->in_edges;e && IS_MARKED(e->endpt);e = e->next);
    if (e == NULL) return 0;  /* all were marked */
  }
  return 1;
}

/* this is a minimality check: give up right now if there are any
   operators being used at the current level that can be removed while
   keeping it a legal plan.  Note: we are guaranteed that all minimal sets
   of operators will be tried.

   Note: we're given the goal set in the PREVIOUS time....

  Note: At one point had in planner that won't consider any op that 
  adds a previous goal.  Reason: either already tried it (so cannot be good) 
  or else should be tried with prev goal in mind so that you don't get
  unnecessarily-many goals in goals_at[time-1]. The problem is you could get
  a situation where there are only two ways to make goal #1 true.  One makes
  goal #2 true and the other makes goal #3 true, both of which are later in
  the list than goal #1.
*/
int action_set_is_minimal(int index, int time)
{
  int i;
  edgelist_t e, e2;
  vertex_t op;
  for(i=0; i < index; ++i) {
    for(e = goals_at[time][i]->out_edges; e; e = e->next) {
      if (!(op = e->endpt)->used) continue;  /* not an op we're using */
      for(e2 = op->out_edges; e2; e2 = e2->next) {
	if (!e2->endpt->used) continue;  /* not one of our goals */
	if (e2->endpt->is_true == 1) break; /* op is needed */
      }
      if (e2 == NULL) { /* Hey, the op wasn't needed after all! */
	return 0;
      }
    }
  }
  return 1;
}

/* helper for setting up */
int runbasicplan(int numgoals, int time)
{
  int i, result;
  for(i=0; i < numgoals; ++i) goals_at[time][i]->used = 1;
  result = basicplan(numgoals-1, 0, time);
  for(i=0; i < numgoals; ++i) goals_at[time][i]->used = 0;
  return result;
}

/***********End of checking routines************************************/


/*********** THIS IS THE MAIN RECURSIVE BACKWARD CHAINING SEARCH **********
   This is a recursive planner.  Two interesting things it does are 
   (1) use the mutual-exclusivity relations, and (2) go layer by layer.

   Idea: Given an array of goals at some time, for each way of generating
   these goals from facts in previous time step that doesn't violate
   exclusivity constraints, try running recursively.

   Returns 1 if doable and 0 if not.

   Arguments:
    cindex tells us the index of the Current goal we're working on.
    nindex is the next free index at the previous level(the one we're creating)
      We use this to put preconditions of our ops into.
    time is the current time.

   This examines the goals one by one (starting with the one of highest
   index in the goals_at[time] array) and for each, looks at all ways of
   creating it (that aren't exclusive of previous commitments).  For each 
   such way, we first check to see if some future goal has been "cut off",
   and if not, we put the preconditions into goals_at[time-1][nindex].

   Note: NOOP is the first one in list of ways to make a goal true. 
   This routine relies on the exclusivity relations between ops (and noops) 
   being in the graph.  I.e., it does no other (redundant) checking to 
   make sure plan is LEGAL.
   
   The planner is proactive in the sense that it checks to see if remaining 
   goals in the list are still possible. (this is what is meant above by 
   "cut off")
   
   We tried using the heuristic of having the next goal be the
   one with the fewest ways of making it true (rather than the next one
   in the list) but it didn't generally help much.
   */
int basicplan(int cindex, int nindex, int time)
{
  int i, result, was_used;
  vertex_t v, op;
  edgelist_t e, edge2;
  if (time <= 0) return 1;  /* we're done */

  if (cindex < 0) { /* go on to the previous time step */
    if (DEBUG_FLAG) {
      printf("goals at time %d:\n  ",time);
      for(i=0; i < nindex; ++i) printf("%s ",goals_at[time-1][i]->name);
      printf("\n\n");
    }
    if (do_memo) {
      if (lookup_bad(goals_at[time-1], nindex, time-1)) return 0;
      if (do_greedy && lookup_good(goals_at[time-1], nindex, time-1)) return 1;
      if (do_subsets) {  /* option to see if a subset is inside */
	if(PARTIALsubset_lookup(goals_at[time-1], nindex, time-1)) {
	  insert_bad(goals_at[time-1],nindex, time-1);/*find faster next time*/
	  return 0;
	}
      }
    }

    /** do minimality check **/
    if (!action_set_is_minimal(nindex, time-1)) {
      printf("found non-minimal action set, time %d\n",time);
      return 0;
    }

    /** if desired, do lower bound check **/
    if (do_lower && !can_hope_to_solve(goals_at[time-1],nindex, time-1)) {
      insert_bad(goals_at[time-1],nindex, time-1);
      return 0;
    }
    /* do greedy check */
    if (do_greedy) result = constrict(nindex, time-1);
    else result = basicplan(nindex-1, 0, time-1);
    if (result == 0 && do_memo) insert_bad(goals_at[time-1], nindex, time-1);
    return result;
  }

  /* Let v be the next goal, skipping over any that may just by luck have
     been made true by the operators we've already committed to */
  while((v = goals_at[time][cindex])->is_true) {
    --cindex;
    if (cindex < 0) return basicplan(-1, nindex, time);
  }

  /* Try the ops and noops */
  for(e = v->in_edges; e; e = e->next) {
    op = e->endpt;
    if (IS_MARKED(op)) continue; /* exclusive of something used*/

    /* ok, do it. */
    mark_exclusive(op);
    /* check to see if causes problems for future goals */
    if (!goals_still_possible(cindex,time)) {
      unmark_exclusive(op);
      continue;
    }
    op->used++;

    /* THIS SORT-OF CORRESPONDS TO STATE-SPACE STEPS */
    if (!IS_NOOP(op)) ++number_of_actions_tried;

    if (xviewing) draw_op(op, time-1, 1, 1); /* XVIEWING */

    for(edge2 = op->in_edges; edge2; edge2 = edge2->next) {
      was_used = edge2->endpt->used++;
      if (!was_used) goals_at[time-1][nindex++] = edge2->endpt;
      if (nindex > MAXGOALS) do_error("MAXGOALS too small");
    }
    for(edge2 = op->out_edges; edge2; edge2 = edge2->next)
      ++edge2->endpt->is_true;
     
    /* now recurse */
    if (DEBUG_FLAG > 1) {
      if (IS_NOOP(op)) printf("trying %s from prev time step.\n",v->name);
      else printf("Trying %d %s to achieve %s.\n",time, op->name, v->name);
    }
    result = basicplan(cindex-1, nindex, time);
    
    /* undo */
    for(edge2 = op->out_edges; edge2; edge2 = edge2->next)
      --edge2->endpt->is_true;
    for(edge2 = op->in_edges; edge2; edge2 = edge2->next) {
      was_used = --edge2->endpt->used;
      if (!was_used) goals_at[time-1][--nindex] = NULL;
    }
    unmark_exclusive(op);
    if (result == 1 && for_printing) return 1;   /* DONE */

    op->used--;
    if (xviewing) draw_op(op, time-1, 0, 1);  /* XVIEWING */

    if (result == 1) return 1;                   /* DONE */
  }
  /* otherwise, return 0 */
  return 0;
}




/**************Here are a couple more helpers/heuristics*****************
 **************that can be turned on by using options.*****************/


/* This routine takes a goal array, its length, and a time and 
   returns 0 if there is just no hope of being able to solve the goals.
   
   Currently, it does the following reasoning: if it can find a subset
   of the goals whose excl_in_this_step form a clique of size s, then
   the time had better be at least s.
 */
int can_hope_to_solve(goal_arr g, int length, int time)
{
  int i, yesind, noind, maxval, maxind;
  vertex_t tempv;
  edgelist_t edge;
  goal_arr arr;
  static int *count, *marked_as_nbr, flag = 0;

  if (flag == 0) {  /* doing this in case MAXGOALS is not a constant */
    count = (int *) malloc(MAXGOALS*sizeof(int));
    marked_as_nbr = (int *) malloc(MAXGOALS*sizeof(int));
    flag = 1;
  }

  /* if time = 0, special case: just return yes */
  if (time==0) return 1;

  /* copy over goal array and initialize: give each vertex the index in arr*/
  for(i=0; i < length; ++i) { 
    arr[i] = g[i]; 
    count[i] = 0;
    arr[i]->junk = i; /* using junk ptr to hold this info */
  }
  yesind = 0;  /* "potential" ones for clique between yes and no indices */
  noind = length;

  /* set up counts */
  for(i=0; i < length; ++i) {
    for(edge = arr[i]->excl_in_this_step; edge; edge = edge->next) {
      if (arr[edge->endpt->junk] == edge->endpt) ++count[i];
    }
  }

  /* get clique based on greedy strategy: pick node with highest degree and
     put in. then of all nbrs, put in node of next highest degree (into the
     set of vertices still under consideration), etc. 
   */
  while(yesind < noind) {
    /* find highest degree (quit if not big enough) and put it into yes pile*/
    for(i=yesind, maxval = 0; i < noind; ++i) {
      if (count[i] > maxval) {maxval = count[i]; maxind = i;}
    }
    if (maxval < time) return 1;
    tempv = arr[yesind]; /* the lowest in "potential" set */
    arr[yesind] = arr[maxind];
    arr[maxind] = tempv;
    arr[yesind]->junk = yesind;
    arr[maxind]->junk = maxind;
    yesind++;

    /* begin thinking all are non-neighbors */
    for(i=yesind; i < noind; ++i) marked_as_nbr[i]=0;

    /* mark as no longer potential all the non-neighbors, and decrease counts*/
    /* first, mark all neighbors in "potential" set*/
    for(edge = arr[yesind-1]->excl_in_this_step; edge; edge = edge->next) {
      tempv = edge->endpt;
      if (tempv->junk < noind && arr[tempv->junk] == tempv)
	marked_as_nbr[tempv->junk] = 1;
    }
    /* now move all non-neighbors into "no" region, and for each one, decrease
       counts */
    for(i=yesind; i < noind; ++i) {
      if (!marked_as_nbr[i]) {
	tempv = arr[noind-1];
	arr[noind-1] = arr[i];
	arr[noind-1]->junk = noind-1;
	arr[i] = tempv;
	arr[i]->junk = i;
	count[i] = count[noind-1];
	count[noind-1] = 0;        /* might as well zero it out */
	--noind;                   /* decrease noindex pointer */
	for(edge = arr[noind]->excl_in_this_step; edge; edge = edge->next) {
	  if (arr[edge->endpt->junk] == edge->endpt)
	    --count[edge->endpt->junk];
	}
      }
    }

  }
  if (yesind > time) {
    if (DEBUG_FLAG) printf("Can't hope to solve by time %d.\n",time);
    return 0;
  }
  else return 1;
}


/* Build up incrementally to a plan for entire set.  Start with first goal,
   and if successful go to a plan for the first two, etc.  Store record
   so that if at time t we succeed on first i goals, but not first i+1, then
   at time t+1 we start with i+1. */
int build_up_to_plan(int num_goals, int time)
{
  static int i = 0;
  int j,success, oldfp = for_printing;
  for(;i < num_goals; ++i) {
    if (i != num_goals-1) {
      printf("trying first %d goals\n",i+1);
      for_printing = 0;
    } else for_printing = oldfp;
    if (xviewing) {      /* XVIEWING */
      reset_viewer(time); 
      for(j=0; j <=i; ++j) draw_fact(goals_at[time][j], time, 1);
    }
    success = runbasicplan(i+1,time);
    if (!success) { 
      printf("failed\n"); 
      if (do_memo) insert_bad(goals_at[time], num_goals, time);
      for_printing = oldfp;
      return 0; 
    }
  }
  return 1;
}

/* Just for experimentation..... */
/* try greedily constricting a set until find minimal one causing failure. Do
   "hold one out" for each possibility
 */
int constrict(int num_goals, int time)
{
  goal_arr storage;
  vertex_t temp, temp2;
  int i, j, num_current, result, oldfp;

  for(i=0; i < num_goals; ++i) storage[i] = goals_at[time][i];
  if (runbasicplan(num_goals, time)) return 1; /* success */

  oldfp = for_printing;
  for_printing = 0;
  for(num_current = num_goals-1; num_current > 0; --num_current) {
    temp = goals_at[time][num_current];
    for(j = num_current-1; j >= 0; --j) {
      result = runbasicplan(num_current, time);
      if (result == 0) {
	if (do_memo) insert_bad(goals_at[time], num_current, time);
	break; /* failed on subset */
      }
      insert_good(goals_at[time], num_current, time);
      temp2 = goals_at[time][j];
      goals_at[time][j] = temp;
      temp = temp2;
    }
    if (j < 0) break;  /* all succeeded, so return right away */
  }
 
  for(i=0; i < num_goals; ++i) goals_at[time][i] = storage[i];
  for_printing = oldfp;
  return 0;
}
/***************End of options code***************************************/



/*******************Hashing/memoizing routines********************/
/*****************************************************************/
/* These are the functions for memoizing.  Idea: store in a hash table
   the sets of goals that have been seen so far.
   Since we're just doing lookup, we might as well use a big table.

   For hash table, look at rand1 values. Just add the 
   values to hash since in fact you WANT to have lists that are same 
   as sets but different as sequences to collide.  Will also store the actual
   sum, since it's likely will only have equal sums when really are same
   sets.

   NEW: store sets as bit-vectors of length MAXNODES. That way can compare
   with just a few equality or bitwise AND tests. (also need to check they
   are at the same time)

 */

#define P_HSIZE 8192  /* planner hash table size */
#define P_HASH 8191 /* 111111111111  */
#define BVEC_LEN ((MAXNODES - 1)/32 + 1)

/* store sets as bit-vectors: two sets equal if same vector at same time */
typedef unsigned int *bit_vector;

typedef struct SET_ELT {
  bit_vector item;
  int num;
  long sum;
  int time;
  struct SET_ELT *next;
  struct SET_ELT *thread;
} *hash_entry;

typedef hash_entry set_table[P_HSIZE];
set_table bad_table, good_table;
hash_entry *global_list;  /* global_list[i] is a hash_entry */
int max_time_seen = -1;

/* sum instead of xor since doesn't seem to matter */
long sum_arr(goal_arr a, int num)
{
  int i;
  unsigned long sum;
  for(i=0,sum=0; i < num; i++)  sum = sum + a[i]->rand1;
  return sum;
}

/* turn a goal array into a bit vector. Assume there is space in v. */
/* NOTE: if levels are equal, then uid_*'s should be equal too */
void make_bit_vector(goal_arr a, int len, bit_vector v)
{
  int i;
  vertex_t w;
  for(i=0; i < bvec_len; ++i) v[i] = 0;  /* zero out first */
  for(i=0; i < len; ++i) {
    w = a[i];
    v[w->uid_block] = v[w->uid_block] | w->uid_mask;
  }
}

/* make more space for global list */
void fix_global_list(int time)
{
  hash_entry *temp;
  int i;
  temp = global_list;
  global_list = (hash_entry *) calloc(time+1, sizeof(hash_entry));
  for(i=0; i <= max_time_seen; ++i) global_list[i] = temp[i];
  if (temp) free(temp);
  max_time_seen = time;
}


/***Doing the arbitrary subset case ***/
/***This is currently not being used, but to use it, just replace calls
    to PARTIALsubset_lookup by calls to this.  This routine actually
    does help a lot sometimes, but for it to be generally effective, we
    need a good data structure to acess the subsets more quickly ***/

int subset_lookup(goal_arr a, int num, int time)
{
  static bit_vector query;
  static int flag = 0;
  int j;
  hash_entry elt;
  if (flag == 0) {  /* (since MAXNODES not constant) */
    query = (bit_vector) malloc(BVEC_LEN*sizeof(int));
    flag = 1;
  }
  if (time > max_time_seen) fix_global_list(time); /* make more space */
    
  make_bit_vector(a, num, query);   /* "query" is what we actually check*/
  for(elt = global_list[time]; elt; elt = elt->thread) {
    for(j=0; j < bvec_len; ++j) {
      if ((query[j] | elt->item[j]) != query[j]) break;
    }
    if (j == bvec_len) { /* same */
      ++subset_hits; /* just for info */
      return 1;
    }
  }
  return 0;
}


/***** Since it seems hard to find if an arbitrary subset is in the
  hash table, let's just see if a set of size one smaller is in the 
  hash table.
  ****/
int PARTIALsubset_lookup(goal_arr a, int num, int time)
{
  long sum, current;
  static bit_vector v;
  static int flag = 0;
  int i, j, index, vind;
  hash_entry elt;
  if (flag == 0) {
    v = (bit_vector) malloc(BVEC_LEN*sizeof(int));
    flag = 1;
  }
  sum = sum_arr(a, num);
  make_bit_vector(a, num, v);
  /* try removing one */
  for(i=0; i < num; ++i) {
    current = sum - a[i]->rand1;
    vind = a[i]->uid_block;
    v[vind] = v[vind] ^ a[i]->uid_mask;   /* flip that bit */
    index = (int) (current & P_HASH);
    /* if (index < 0) index += P_HSIZE; */
    for(elt = bad_table[index]; elt; elt = elt->next) {
      if (elt->sum != current) continue;  /* not same */
      if (elt->time != time) continue;    /* not same */
      /* if get here, probably the same, but need to verify. */
      for(j=0; j < bvec_len; ++j) {
	if (v[j] != elt->item[j]) break;
      }
      if (j == bvec_len) { /* what we expect */
	++subset_hits;
	return 1;
      }
    }
    v[vind] = v[vind] ^ a[i]->uid_mask;   /* flip that bit back */
  }
  return 0;
}


/* doing lookup with verification.  Other option is to hash both
   random values and to use other hash for probabilistic verification

   storing as bit vectors so can check equality easily
 */
int set_lookup(set_table table, goal_arr a, int num, int time)
{
  long sum;
  static bit_vector query;
  static int flag = 0;
  int index, i;
  hash_entry elt;

  if (flag == 0) {  /* (since MAXNODES not constant) */
    query = (bit_vector) malloc(BVEC_LEN*sizeof(int));
    flag = 1;
  }
  ++hash_ctr;  /* just for info */
  hash_numctr += num; /* just for info */
  sum = sum_arr(a, num);
  index = (int) (sum & P_HASH);  /* BITWISE AND */
  /*  if (index < 0) index += P_HSIZE; */
  make_bit_vector(a, num, query);   /* "query" is what we actually check*/
  for(elt = table[index]; elt; elt = elt->next) {
    if (elt->sum != sum) continue;   /* not same */
    if (elt->time != time) continue; /* not same */
    /* if get here, probably the same, but need to verify */
    for(i=0; i < bvec_len; ++i) {
      if (query[i] != elt->item[i]) break;
    }
    if (i == bvec_len) return 1;
  }
  return 0;
}

int lookup_bad(goal_arr a, int num, int time)
{
  int val = set_lookup(bad_table, a, num, time);
  if (val == 1) {
    if (DEBUG_FLAG) printf("hash table hit.\n");
    ++hash_hits; /* just for info */
  }
  return val;
}

int lookup_good(goal_arr a, int num, int time)
{
  int val = set_lookup(good_table, a, num, time);
  if (val == 1) {
    if (DEBUG_FLAG) printf("good hash table hit.\n");
    ++goodhash_hits; /* just for info */
  }
  return val;
}


/* insert into table.  Return pointer */
hash_entry set_insert(set_table table, goal_arr a, int num, int time)
{
  long sum;
  int index;
  static int mem_flag = 0;
  hash_entry elt;
  sum = sum_arr(a, num);
  index = (int) (sum & P_HASH);
  /* if (index < 0) index += P_HSIZE; */
  elt = (struct SET_ELT *) malloc(sizeof(struct SET_ELT));
  if (elt == NULL) { /* out of memory */
    if (mem_flag == 0)
      ++mem_flag, printf("no memory at insert %d\n",hash_inserts);
    return NULL;
  }
  elt->num = num;
  elt->sum = sum;
  elt->time = time;
  elt->item = (bit_vector) malloc(BVEC_LEN*sizeof(int));
  make_bit_vector(a, num, elt->item);
  elt->next = table[index];
  table[index] = elt;
  return elt;
}

void insert_bad(goal_arr a, int num, int time)
{
  int i;
  static goal_arr just_for_debugging;
  hash_entry elt = set_insert(bad_table, a, num, time);
  if (do_subsets) {
    if (time > max_time_seen) fix_global_list(time);
    elt->thread = global_list[time];
    global_list[time] = elt;
  }
  ++hash_inserts;  /* global: just for info */
  if (do_completeness && same_as_prev_flag) { /* info for completeness test */ 
    if (time == first_full_time) ++num_hashes[1];
    if (time == first_full_time + 1) { /* JUST FOR DEBUGGING */
      for(i=0;i<num;++i) just_for_debugging[i] = a[i]->prev_time;
      if (!set_lookup(bad_table, just_for_debugging,num,time-1)){
	/* SHOULD NEVER HAPPEN */
	printf("Error! set in table at time %d not in at time %d.\n",
	       first_full_time+1, first_full_time);
	for(i=0;i<num;++i) printf("%s ",a[i]->name);
	printf("\n");
	exit(1);
      }
    }
  }
}

void insert_good(goal_arr a, int num, int time)
{
  set_insert(good_table, a, num, time);
}

/******************** set up ****************************/
void handle_events(void);
void do_graph_created(void);

/* set up goal array "goals_at" and mark the initial facts as "is_true".
   Return number of goals.  Note: possibly might be some nodes in 
   fact_table[maxtime] that aren't goals.  
 */
int setup(int maxtime)
{
  vertex_t v;
  fact_list temp;
  char str[100];
  int i = 0, j;
  /* set up initial facts */
  get_next(fact_table[0],0);
  while((v = get_next(fact_table[0],1)) != NULL) v->is_true = 1;

  if (xviewing) reset_viewer(maxtime);  /* XVIEWING */
  if (xviewing == 2) {  /* draw whole graph */
    for(j=0; j < maxtime+1; ++j) {
      /* facts */
      get_next(fact_table[j],0);
      while((v = get_next(fact_table[j],1)) != NULL) draw_fact(v, j, 1);
      handle_events();
      /* ops */
      if (j == maxtime) break;
      get_next(op_table[j],0);
      while((v = get_next(op_table[j],1)) != NULL) 
	if (v->out_edges) draw_op(v, j, 1, 0);
    }
  }

  /* set up goal array */
  if (goals_at != NULL) free(goals_at);
  goals_at = (goal_arr *) calloc(maxtime+1,sizeof(goal_arr));
  for(temp=the_goals; temp; temp = temp->next) {
    instantiate_into_string(temp->item, NULL, str,1);
    if ((v = lookup_from_table(fact_table[maxtime],str)) == NULL) 
      do_error("goal not found in output");
    goals_at[maxtime][i++] = v;
    if (xviewing) draw_fact(v, maxtime, 1);  /* XVIEWING */
    if (i > MAXGOALS) do_error("MAXGOALS too small.");
  }
  return i;
}






