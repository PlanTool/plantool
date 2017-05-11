/*********************** Graphplan **************************************
  (C) Copyright 1995 Avrim Blum and Merrick Furst

  All rights reserved. Use of this software is permitted for non-commercial
  research purposes, and it may be copied only for that use.  All copies must
  include this copyright message.  This software is made available AS IS, and
  neither the authors nor CMU make any warranty about the software or its 
  performance. 
*************************************************************************/

#include "graphplan.h"
#include <sys/time.h>

/* The code consists of the following main parts.

1. Reading in the operator and fact files.  Note: we take 3 formats.
In the non-prodigy formats, everything up to first left paren is a
comment. Note: there is no special understanding of "not"s: these are
just viewed as another word.  In the non-prodigy formats, "(del blah
blah)" means to delete "(blah blah)".

Note: the code is lex.yy.c and y.tab.{c,h} is only for reading 
prodigy-style files.

2. Creating the graph.  

The layered graph will have facts-at-time-1 as the first layer,
then ops-at-time-1, then facts-at-time-2, etc.  Each layer is stored
in a hash table of vertex_s structures.  These structures have a
"name" field which is the key, and the hash functions return a pointer
to the structure.  (The reason for doing it this way is that sometimes
we want to access a vertex by using its name.) The graph is created in
a forward pass until all goals appear and none are listed as being
mutually exclusive.

  Creating the graph involves the following main steps.

  A. Performing instantiations.  The code for this part is a bit
messy, but conceputally, it's just performing the task: given a list
of facts and an operator, find all the ways in which that operator can
be instantiated so that its preconditions are in that list.  What is
confusing is that we have two different ways of storing the names of
things.  One way is as a token list (easier for instantiations) and
the other way is as a string with words connected by underscores (how
they are stored in the graph).

  B. Calculating mutual exclusions.  These propagate and will be used
  to speed up the later search. We will also use this to make the
  graph smaller (in particular, if an operator needs two facts that
  are mutually exclusive, then we don't put it in the graph.)

  Also, once the graph has "leveled off" (2 adjacent fact layers have
  the same number of facts and the same number of mutual exclusions)
  then all future layers will be the same.  So, we can just copy and
  don't need to instantiate any more.

3. Doing the search.  This is just a simple backward chaining, done
  level-by level to make most use of the exclusions.  Unsolvable goal
  sets at a given time step are stored in a hash table so that we can
  fail more quickly next time.  Most of the flags to the user have to
  do with this part of the planning.  E.g., We can do a lowerbounding
  check based on the principle that if there are 10 goals no two of
  which can be made true in the same time step then we will need at
  least 10 steps.  Or, we can check subsets of a current goal set to
  see if any of them has been stored in the unsolvable sets hash
  table.

  Right now, there are no operator-ordering or goal-ordering heuristics,
  except that NOOPs are always tried first.  I.e., If there are several
  ways of making fact F true at time T, and one such way is to have F be 
  true at time T-1, then that way is tried first.  This seems to make
  plans look nicer.

*/

/* GLOBAL VARIABLES */

char junk[100]; 
fact_list the_types;           /* list of all the different type decls */
fact_list initial_facts, the_goals; /* initial facts and goals */
hashtable_t *fact_table, *op_table;  /* the arrays of hash tables */
hashtable_t useful;  /* USED for "do_irrel": CHECK USEFUL FACTS. */
hashtable_t types_table;  /* holds the types */

extern int hash_hits, hash_inserts; /* entries in plan hash table */

/* these are defaults */
int MAXNODES = 256;  /* default MAX number of nodes at any given time step. */
int DEBUG_FLAG = 0, do_memo = 0, do_subsets=0,
    do_noexcl = 0, do_buildup = 0, do_greedy = 0;
int oldstyle = 0;  /* type of ops file being read in */
int do_irrel = 0, going_backwards = 0;/* use for finding irrelevant features */
int do_lower = 0;  /* try to immediately  prove a set is notpossibly doable */
int xviewing = 0;  /* graphical animation */
int do_completeness = 1; /* do completeness check */

/* other flags, counters, etc. */
int same_as_prev_flag = 0;  /* graph layer is same as previous time step */
int first_full_time, num_hashes[2] = {-1,0}; /* for checking completeness */
/* num_hashes[0] = previous count, and num_hashes[1] = current count */
extern int number_of_recursive_calls, number_of_actions_tried;
int bvec_len = 1;  /* max facts in a time step / 32 */
int num_goals;

int instrs(void)
{
  printf("command line args.  Use any of: \n \
          -h               for this list \n \
          -o <op file>     to specify operator file\n \
          -f <fact file>   to specify fact file\n \
          -t <integer>     to specify a fixed number of time steps\n \
          -i <info level>  to specify info level 1 or 2 (default is 0)\n \
          -O <option list> to specify options you want\n \
          -M <integer>     to specify alternative max nodes in a time step\n \
                            (default is 256)\n \
          -d               give default values to everything not specified\n\
\n   for example: graphplan -o fixit_ops -f fixit_facts1 -O IL -d \n\
          \n\n");
  return 0;
}


int oldmain(int argc, char *argv[])
{
  op_list ops;
  int i,max_time=0, auto_stop = 0, old_num_created, givedef=0;
  FILE *fp;
  char opfile[50], factfile[50], option[10];
  struct timeval start, end;

  if (argc == 1) {
    printf("Welcome to graphplan. 'graphplan -h' gives help on command-line args.\n\n");
  }
  option[0] = opfile[0] = factfile[0] = '\0';
  DEBUG_FLAG = -1;
  for(i = 1; i < argc; ++i) {
    if (argv[i][0] != '-') continue; 
    if (argv[i][1] == 'h') {instrs(); exit(0);}
    if (argv[i][1] == 'x') xviewing = 1;
    else if (argv[i][1] == 'd') givedef = 1;
    else if (i == argc - 1) do_error("command line args not in proper format");
    else if (argv[i][1] == 'o') strcpy(opfile,argv[i+1]);
    else if (argv[i][1] == 'f') strcpy(factfile,argv[i+1]);
    else if (argv[i][1] == 't') sscanf(argv[i+1],"%d",&max_time);
    else if (argv[i][1] == 'i') sscanf(argv[i+1],"%d",&DEBUG_FLAG);
    else if (argv[i][1] == 'O') strcpy(option,argv[i+1]);
    else if (argv[i][1] == 'M') sscanf(argv[i+1],"%d",&MAXNODES);
  }

  if (MAXNODES > MAXMAXNODES)
    do_error("Sorry, can't handle that large MAXNODES");


  /*LOAD IN OPS AND FACTS FILES. If ends in ".lisp", assume prodigy style*/
  if (!opfile[0]) {
    printf("give file name for operators: ");
    gets(opfile);
  }
  if ((fp = fopen(opfile,"r")) == NULL) do_error("cant load operator file");
  if (strcmp(opfile+max((strlen(opfile)-strlen(LISPEXT)),0),LISPEXT) == SAME)
    ops = load_prodigy_ops(fp);
  else
    ops = load_ops(fp);
  if (!ops) do_error("illegal ops file");

  if (!factfile[0]) {
    printf("give file name for initial facts: ");
    gets(factfile); 
  }
  if ((fp = fopen(factfile,"r")) == NULL) do_error("cant load facts file");
  the_types = load_types(fp);  /* load in types */

  load_fact_list(fp, &initial_facts);  /* load in actual facts */
  read_item(fp,junk); /* left paren */
  read_item(fp,junk); /* "effects" */
  num_goals = load_fact_list(fp, &the_goals);  /* load in the goals */
  if (num_goals > MAXGOALS) do_error("MAXGOALS too small");
  printf("facts loaded.\n");

/*  if (!givedef && !xviewing) {
 *    printf("X animation? (<CR> for no): ");
 *    gets(junk);
 *    if (*junk == 'y') xviewing = 1;
 * }
 */
  if (xviewing) setup_viewer(); 

  if (givedef && max_time == 0) auto_stop = 1;   /* NUMBER OF TIME STEPS */
  else if (max_time == 0) {
    printf("number of time steps, or <CR> for automatic: ");
    gets(junk);
    if (sscanf(junk,"%d",&max_time) != 1) {
      auto_stop = 1;
      max_time = 0;
    }
  }

  if (givedef && DEBUG_FLAG == -1) DEBUG_FLAG = 0; /* INFORMATION TO USER */
  else if (DEBUG_FLAG == -1) {
    printf("Info type: (2 = max, 0 or <CR> = min): ");
    gets(junk);
    if (*junk == '2') DEBUG_FLAG = 2;
    else if (*junk == '1') DEBUG_FLAG = 1;
    else DEBUG_FLAG = 0;
  }

  /* OTHER OPTIONS */
  if (option[0] == '\0' && !givedef) {
    printf("Other: 'I' = look for irrelevants\n");
    printf("       'L' = Lower bound time needed by counting steps\n");
    printf("       'B' = Build up to goals\n");
/*    printf("       'H' = Try a heuristic ordering of subgoals\n");  */
    printf("       'E' = Don't do mutual exclusions (for testing)\n");
    printf("       'S' = examine subsets:      ");
    gets(option);
  }
  do_memo = 1;  /* ALWAYS DOING MEMOIZING */
  for(i=0; option[i] != '\0'; ++i) {
    if (option[i] == 'S') {do_subsets = 1; do_completeness = 0;}
    if (option[i] == 'I') do_irrel = 1;
    if (option[i] == 'B') do_buildup = 1;
    if (option[i] == 'G') do_greedy = 1; /* Just for experimentation */
    if (option[i] == 'L') {
      do_lower = 1; do_completeness = 0;
    }
    if (option[i] == 'E') do_noexcl = 1;
  }
  if (do_completeness == 0) printf("turning off completeness check.\n");
  /* BEGIN TIMING */
  gettimeofday(&start,0);

  /* DO PRE-PREPROCESSING IF DESIRED */
  if (do_irrel) initial_facts = useful_facts(ops, initial_facts);

  /* MAKE GRAPH AND START PLANNING */
  max_time = create_graph(ops, initial_facts, max_time); /* treat 0 as auto*/
  if (max_time == -1) {
    if (same_as_prev_flag)
      printf("Problem not solvable: can't even reach non-mutex goals.\n");
    else 
      printf("Can't get there from here in allotted time.\n");
  } else if (!auto_stop) {
    if (DEBUG_FLAG > 0) print_info(max_time);
    if (do_plan(max_time)) print_plan(max_time);
    else print_cant_do(max_time);
  } else {
    for(; max_time <= max_auto; max_time++) {
      if (DEBUG_FLAG > 0) print_info(max_time);
      /* try doing plan */
      if (do_plan(max_time)) {
	print_plan(max_time);
	break;
      } else {
	if (max_time == max_auto) print_cant_do(max_time);
	else printf("Can't solve in %d steps\n",max_time);
	if (same_as_prev_flag) { /* check for true unsolvability */
	  if (num_hashes[0] == num_hashes[1] && do_completeness) {
	    printf("Problem not solvable.\n");
	    break;
	  } else {
	    num_hashes[0] = num_hashes[1];
	  }
	}
      }
      /* create next level (unless at very end) */
      if (max_time != max_auto) {
	old_num_created = num_created;
	create_graph_layer(ops);
	printf("%d new nodes added.\n",num_created - old_num_created);
      }
    }
  }
  gettimeofday(&end,0);
  if (end.tv_sec - start.tv_sec < 60)
    fprintf(stdout,"  %.2f secs\n",end.tv_sec + end.tv_usec/1000000.0 - 
	    (start.tv_sec + start.tv_usec/1000000.0));
  else
    fprintf(stdout,"  %d min, %d seconds\n",(end.tv_sec - start.tv_sec) / 60,
	    (end.tv_sec - start.tv_sec)%60);

  if (xviewing) {
    do_final_viewing();
  }
  return 0;
}

int main( int argc, char * argv[]) {
  return oldmain(argc, argv);
}


/* For printing, removes vertices that could never be reached by planner */
void remove_unneeded_vertices(int time)
{
  fact_list temp;
  vertex_t vert, goal;
  edgelist_t edge;
  char str[100];

  /* begin by marking goals as needed (even required) at the end*/
  for(temp=the_goals; temp; temp = temp->next) {
    instantiate_into_string(temp->item, NULL, str,1);
    if ((vert = lookup_from_table(fact_table[time],str)) == NULL) 
      do_error("goal not found in R.U.V.");
    vert->needed = 2;
  }
  /* now work backwards from end.  An operator is needed if it has a 
   * needed add-effect and no add-effects exclusive of a required fact. 
   * NOOPs are needed if their effect is needed
   * A fact is needed if it has an edge into
   * a needed operator or NOOP (if it's needed in a later time step). */
  for(--time; time >= 0; --time) {
    /* do ops */
    get_next(op_table[time],0);                   /* INIT */
    while((vert = get_next(op_table[time],1)) != NULL) {
      vert->needed = 0;
      if (IS_NOOP(vert)) {
	if (vert->out_edges->endpt->needed) vert->needed = 1;
      } else {
	for(edge = vert->out_edges; edge; edge = edge->next)
	  if(edge->endpt->needed) vert->needed = 1;
      }
    }

    /*do facts */
    get_next(fact_table[time],0);                 /* INIT */
    while((vert = get_next(fact_table[time],1)) != NULL) {
      vert->needed = 0;
      for(edge = vert->out_edges; edge; edge = edge->next) { /* incl' NOOP */
	if(edge->endpt->needed) {
	  vert->needed = 1; break;
	}
      }
      goal = lookup_from_table(fact_table[time+1],vert->name);  /*kill this??*/
      if (goal->needed == 2 && goal->in_edges->next == NULL) { /*pass by NOOP*/
	vert->needed = 2;
	if (DEBUG_FLAG>2) printf("%s required, time %d\n",vert->name,time);
      }
    }
  }
}


/***********creating the graph*************************************
 ******************************************************************/


/* this routine makes a layer of noops.  Assumes next fact already in table.
 */
void make_noop_layer(int time)
{
  vertex_t noop,v,w;
  get_next(fact_table[time],0);  /**INIT**/
  while((v = get_next(fact_table[time],1)) != NULL) {
    w = v->next_time;
    noop = insert_into_table(op_table[time],make_noop_string(v->name));
    noop->is_noop = 1;  /* SAY THAT IT'S A NOOP */
    noop->in_edges = insert_edge(noop->in_edges, v);
    v->out_edges = insert_edge(v->out_edges,noop);
    noop->out_edges = insert_edge(noop->out_edges,w);
    w->in_edges = insert_edge(w->in_edges,noop);
  }
}

/***********create the graph************************************
* Given a list of ops, a list of initial facts and a length 
* of time, we create the graph.
*************************************/


/* This routine creates a single layer: op_table[time] and fact_table[time+1].
   Assumes is always called on consecutive time steps.  To make sure
   I don't forget this, "time" is a static variable.
 */
void create_graph_layer(op_list olist)
{
  static int time = 0;
  int i;
  static pair fact_summary, old_fact_summary;
  fact_list flist;
  op_ptr op;
  vertex_t v,w;
  if (time == 0) fact_summary.first = fact_summary.second = 0;
  if (same_as_prev_flag) make_copy(time);
  else {
    /* first copy facts over to next time step */
    get_next(fact_table[time],0);  /**INIT**/
    while((v = get_next(fact_table[time],1)) != NULL) {
      w = insert_into_table(fact_table[time+1],v->name);
      w->prev_time = v;
      v->next_time = w;
    }
    /* get fact list from the hash table (INEFFICIENT!) */
    flist = make_list_from_htable(fact_table[time]);
    /* do ops */
    for(op = olist; op != NULL; op = op->next)
      do_operator(fact_table[time], flist, op, op->preconds, time);
    completely_free_fact_list(flist);

    /* make the noop layer.  NOTE: doing it HERE so that the noops will be
     the first ones in the list.  Makes it easier for the planner to
     try them FIRST. */
    make_noop_layer(time);

    /* set up uids and rands*/
    get_next(op_table[time], 0);  /* INIT */
    for(i=0;  (v = get_next(op_table[time],1)) != NULL; ++i) {
      if (i >= MAXNODES) do_error("Too many ops. Need to increase MAXNODES");
      set_uid(v,i);
    }
    get_next(fact_table[time+1],0);  /**INIT**/
    for(i=0; (v = get_next(fact_table[time+1],1)) != NULL; ++i) {
      if (i >= MAXNODES) do_error("Too many ops. Need to increase MAXNODES");
      v->rand1 = random(); /* these are used in memoizing */
      set_uid(v,i);        /* everybody needs a unique ID */
    }

    /* find mutually exclusive ops */
    find_all_mutex_ops(op_table, time);

    /* find mutually exclusive facts */
    old_fact_summary = fact_summary;  /* remember state of last time step */
    fact_summary = find_mutex_facts(fact_table, time+1);
    if (do_lower) find_currently_mutex_facts();  /* NEW */
    if (fact_summary.first == old_fact_summary.first &&
	fact_summary.second == old_fact_summary.second) {
      same_as_prev_flag=1;
      first_full_time = time; /* for checking completeness */
    }
  }
  /* setting bit vector length */
  if (bvec_len * 32 < fact_summary.first) bvec_len = 1 + fact_summary.first/32;
  printf("time: %d, %d facts and %d exclusive pairs.\n",
	 time+1, fact_summary.first, fact_summary.second);

  ++time;  /* INCREMENT TIME... */
}

/* If maxtime=0, then treat this as auto.  Keep going until "can_stop"
 * Returns max time used (if not called with 0, just same argument)
 * Returns -1 if can't get there from here (i.e., goals not in graph or
 * not mutex)
 */
int create_graph(op_list olist, fact_list flist, int maxtime)
{
  int time = 0, i, autostop = !(maxtime);
  vertex_t v;

  num_created = 0;  /* GLOBAL: how many nodes created */
  if (autostop) maxtime = max_auto;

  /* allocate space */
  fact_table = (hashtable_t *) calloc(maxtime+1,sizeof(hashtable_t));
  op_table = (hashtable_t *) calloc(maxtime,sizeof(hashtable_t));
  
  /* load in initial facts. */
  for(i=0; flist != NULL; flist = flist->next) {
    v = insert_token_list(fact_table[0],flist->item);
    v->rand1 = random();
    set_uid(v,i++);
  }
  /* make the graph */
  for(time=0; time < maxtime; ++time) { 
    /* for auto case, see if can stop */
    if (autostop && can_stop(time)) {
      printf("Goals first reachable in %d steps.\n",time);
      break;
    }
    if (autostop && same_as_prev_flag) break; /* Will never reach goals */
    create_graph_layer(olist);   /* CREATE LAYER OF GRAPH */
  }

  printf("%d nodes created.\n", num_created);
  if (can_stop(time)) return time;
  else return -1;
}

edgelist_t insert_edge_at_end(edgelist_t e, vertex_t v);

/*****copy op_table[time-1] to op_table[time] and fact_table[time] to 
  fact_table[time+1]. (not exactly copies of course since need to point to
  objects at subsequent time step.)   This is done in the case that 
  fact_table[time] was EXACTLY like fact_table[time-1], meaning that there
  were both the same number of ops and the same number of mutex's.

  Of course, rands are different.

  Also, the ordering of the exclusive lists gets reversed, but this shouldn't
  affect anything.
  *****/
void make_copy(int time)
{
  vertex_t u,v,w;
  edgelist_t e;

  /* first do the ops and connect to their preconditions */
  get_next(op_table[time-1],0);
  while((v = get_next(op_table[time-1],1)) != NULL) {
    w = insert_into_table(op_table[time],v->name);
    w->prev_time = v;
    v->next_time = w;
    w->uid_mask = v->uid_mask;
    w->uid_block = v->uid_block;
    w->is_noop = v->is_noop;

    /* in-edges of op */
    for(e = v->in_edges; e; e = e->next) {
      u = e->endpt->next_time;  /* the fact for OUR in-edge */
      w->in_edges = insert_edge_at_end(w->in_edges, u);
      u->out_edges = insert_edge_at_end(u->out_edges, w);
    }
  }

  /* now do facts and connect them up.  Keep order the same */
  get_next(fact_table[time],0);
  while((v = get_next(fact_table[time],1)) != NULL) {
    w = insert_into_table(fact_table[time+1],v->name);
    w->prev_time = v;
    v->next_time = w;
    w->rand1 = random();
    w->uid_mask = v->uid_mask;
    w->uid_block = v->uid_block;

    /* in-edges */
    for(e = v->in_edges; e; e = e->next) {
      u = e->endpt->next_time;  /* the op for OUR in-edge */
      w->in_edges = insert_edge_at_end(w->in_edges, u);
      u->out_edges = insert_edge_at_end(u->out_edges, w);
    }
  }
 
  /* now do del_edges, exclusives (don't really need del_list) */
  get_next(fact_table[time+1],0);
  while((v = get_next(fact_table[time+1],1)) != NULL) {
    for(e = v->prev_time->exclusive; e; e = e->next) {
      v->exclusive = insert_edge(v->exclusive,e->endpt->next_time);
    }
    for(e = v->prev_time->excl_in_this_step; e; e = e->next) {
      v->excl_in_this_step = 
	insert_edge(v->excl_in_this_step,e->endpt->next_time);
    }
    for(e = v->prev_time->del_edges; e; e = e->next) {
      v->del_edges = insert_edge(v->del_edges,e->endpt->next_time);
    }
  }
  get_next(op_table[time],0);
  while((v = get_next(op_table[time],1)) != NULL) {
    for(e = v->prev_time->exclusive; e; e = e->next) {
      v->exclusive = insert_edge(v->exclusive,e->endpt->next_time);
    }
    for(e = v->prev_time->del_edges; e; e = e->next) {
      v->del_edges = insert_edge(v->del_edges,e->endpt->next_time);
    }
  }
}


/* for auto case.  See if can stop expanding graph.  Can stop if (A)
 * all goals are reachable, and (B) they're not mutex of each other.
 */
int can_stop(int time)
{
  static int flag = 0;
  fact_list temp;
  char str[100];
  static vertex_t *v;   /* doing it this way since MAXGOALS not a constant */
  int num=0, i, j;
  if (flag == 0) { /* set up v */
    v = (vertex_t *) malloc(MAXGOALS*sizeof(vertex_t));
    flag = 1;
  }

  for(temp = the_goals; temp; temp = temp->next) {
    instantiate_into_string(temp->item, NULL, str,1);
    if ((v[num++] = lookup_from_table(fact_table[time],str)) == NULL) 
      return 0;
  }
  /* reached all goals, but check to make sure none are exclusive. */
  for(i=0; i < num; ++i)
    for(j=i+1; j < num; ++j)
      if (are_mutex(v[i],v[j])) {
	fprintf(stderr,"Goals reachable at %d steps but mutually exclusive.\n",
	       time);
	return 0;
      }
  return 1;
}


/***********LOAD IN OPERATOR AND FACT FILES*****************************/
/*******fn prototypes needed for load_ops********/
op_list load_ops_recursive(FILE *fp);
int find_catalysts(op_ptr op, int flag);
void set_insts(op_ptr op);
/************************************************/


/* reads in operators into a big list and returns it. A hash mark "#"
   means old-style.  That means that preconditions are assumed to be
   deleted unless they are in the add-effects.  A precondition
   that wasn't deleted was called a "catalyst" in the old version.
*/
op_list load_ops(FILE *fp)
{
  int c;
  /* start with # if old style */
  if ((c = getc(fp)) == '#') oldstyle = 1;
  else oldstyle = 0;
  ungetc(c, fp);
  return load_ops_recursive(fp);
}

extern	FILE * yyin;
int yyparse();
int lineno;
op_list parser_return;
op_list current_op;

op_list load_prodigy_ops(FILE *fp)
{
  op_list op;
  yyin = fp;
  yyparse();
  /* now go down ops */
  for(op = parser_return; op; op = op->next) {
    /* set instantiation list */
    set_insts(op);
  }
  return parser_return;
}



op_list load_ops_recursive(FILE *fp)
{
  op_list op; char str[100];
  int result;
  /* first read up to left paren */
  do {
    result = read_item(fp,junk);
  } while ((result != LEFT_PAREN) && (result != EOF));
  if (result == EOF) return NULL;

  read_item(fp,junk); /* this should be OPR */

  if (read_item(fp,str) != OK) do_error("error reading operator name");
  printf("%s\n",str);
  op = (op_list) malloc(sizeof(op_s));
  op->name = (char *) calloc(strlen(str) + 1, sizeof(char));
  strcpy(op->name, str);

  /* read in parameters */
  if (read_item(fp,junk) != LEFT_PAREN)
    do_error("expecting left paren before parameters");
  read_item(fp,junk);  /* PARAMS */
  load_fact_list(fp, &(op->params));

  /* read in preconditions. */
  if (read_item(fp,junk) != LEFT_PAREN)
    do_error("expecting left paren before preconds");
  read_item(fp,junk);  /* PRECOND */
  load_fact_list(fp, &(op->preconds));

  /* read in effects */
  if (read_item(fp,junk) != LEFT_PAREN)
    do_error("expecting left paren before preconds");
  read_item(fp,junk);  /* EFFECT */
  load_fact_list(fp, &(op->effects));
  read_item(fp,junk);  /* the final right parenthesis */

  /* Because the input is "old-style", need to put in deletes for the
     preconditions deleted and remove effects for the catalysts */
  if (oldstyle) find_catalysts(op,0);
  else if (find_catalysts(op,1) == 1) { /** just to verify **/
    printf("I notice that %s is adding one of its preconditions.\n",
	    op->name);
    printf("This means you are probably using the old-style format\n");
    printf("If so, just put a hash mark '#' as the first character\n");
    printf("in your operator file and I will read it in correctly.\n");
    printf("If not, then I don't know what it means to add your ");
    printf("precondition.\n\n");
    exit(0);
  }
  /* set instantiation list */
  set_insts(op);
  op->next = load_ops_recursive(fp);
  return op;
}

/* check down two lists. If precond is in the effect list, then remove it.
 * If precond is not, then put in a new delete-effect.
 * This is done in a really slow way, but it's only done once per operator...
 * This will go away once we change the input format
 */
effect_list remove_catal_effects(token_list p, effect_list e)
{
  if (e == NULL) return NULL;
  else if (equal_facts(e->item, p))
    return remove_catal_effects(p,e->next); /* should free storage...*/
  else {
    e->next = remove_catal_effects(p, e->next);
    return e;
  }
}
/* if flag=1, then just return 1 if a catalyst exists and 0 otherwise,
 * but don't do any work.
 */
int find_catalysts(op_ptr op, int flag)
{
  precond_list p; 
  effect_list e, new_e;
  token_list t;

  /* first, put in the deletes */
  for(p = op->preconds; p; p = p->next) {
    for(e = op->effects; e; e = e->next) {
      if (equal_facts(p->item, e->item)) break;
    }
    if (flag == 1) {
      if (e != NULL) return 1; /* found catalyst */
      else continue;
    }
    if (e == NULL) { /* didn't do break, so precondition is deleted */
      new_e = (effect_list) calloc(1, sizeof(fact_list_elt));
      t = (token_list) calloc(1, sizeof(token_list_elt));
      t->item = (char *) calloc(strlen(DELETE)+1,sizeof(char));
      strcpy(t->item, DELETE);
      t->next = p->item;  /* ok for them to point to same storage */
      new_e->item = t;
      new_e->next = op->effects;
      op->effects = new_e;
    }
  }
  if (flag == 1) return 0; /* found no catalysts */
  /* Now, get rid of the copy's */
  for(p = op->preconds; p; p = p->next)
    op->effects = remove_catal_effects(p->item, op->effects);
  return 0;
}

int really_is_var(char *str) 
{ 
  return (str[strlen(str)-1] == '>');
}

/****have instantiation list associated to op.  Set all the var-parts.*****/
/** Changing this to allow the parameters to have some variables mentioned
 ** more than once.  Just makes a list of all of the different ones.
 ** This goes in reverse order. (Gets re-reversed when op-string is created)
 **/
void set_insts(op_ptr op)
{
  param_list pptr;
  token_list tptr;
  instantiation_list iptr;
  op->insts = NULL;
  for(pptr = op->params; pptr != NULL; pptr = pptr->next) {
    for(tptr = pptr->item; tptr; tptr = tptr->next) {
      if (!is_var(tptr->item)) continue;
      if (!really_is_var(tptr->item)) {
	sprintf(junk,"Badly-formed op: %s",op->name);
	do_error(junk);
      }
      /* it's a variable.  Check to make sure it's a new one */
      for(iptr = op->insts; iptr; iptr = iptr->next) {
	if (strcmp(iptr->var_part, tptr->item) == SAME) break;
      }
      if (iptr != NULL) continue;  /* did break above: not new */

      op->insts = insert_inst(tptr->item, NULL, op->insts);
    }
  }
}

token_list load_tokens(FILE *fp);

/* returns number of facts loaded */
int load_fact_list(FILE *fp, fact_list *fptr)
{
  token_list tlist;
  /* if not left paren, then we're done. */
  if (read_item(fp, junk) != LEFT_PAREN) {
    *fptr = NULL;
    return 0;
  }
  if ((tlist = load_tokens(fp)) == NULL) do_error("bad input file");
  *fptr = (fact_list) malloc(sizeof(fact_list_elt));
  (*fptr)->item = tlist;
  return 1 + load_fact_list(fp, &( (*fptr)->next ));
}

/* special for loading in types: ugly.  Should change format.
   Reads up to and including next left paren.  Also, sets up
   types_table */
fact_list load_types(FILE *fp)
{
  char temp_token[100];
  token_list t; fact_list f;

  while(read_item(fp, junk) != LEFT_PAREN); /* get left paren */
  read_item(fp, temp_token); /* first in list: see if it is "preconds" */
  if (strcmp(temp_token, PRECOND) == SAME) return NULL; /* at end */
  t = (token_list) malloc(sizeof(token_list_elt));
  t->item = (char *) calloc(1+strlen(temp_token),sizeof(char));
  strcpy(t->item, temp_token);
  t->next = load_tokens(fp);
  insert_token_list(types_table, t);
  f = (fact_list) malloc(sizeof(fact_list_elt));
  f->item = t; 
  f->next = load_types(fp);
  return f;
}

/* if next word is a right paren, returns NULL. Otherwise, read in tokens up
   to right parenthesis and return the list of tokens.  */
token_list load_tokens(FILE *fp)
{
  char temp_token[100];
  int result;
  token_list_elt *current;
  result = read_item(fp, temp_token);
  if (result == RIGHT_PAREN) return NULL;
  current = (token_list) malloc(sizeof(token_list_elt));
  current->item = (char *) calloc(1 + strlen(temp_token),sizeof(char));
  strcpy(current->item, temp_token);
  current->next = load_tokens(fp);
  return current;
}

/*************************END OF STUFF FOR LOADING IN OPS,FACTS**********/
/************************************************************************/



/********* ROUTINES FOR TAKING THE OPS AND A LIST OF FACTS AND ***********
 *********     INSTANTIATING TO CREATE NODES IN THE GRAPH      ***********/

/* These routines create nodes in the graph by instantiating operators  */
/* This part is kindof messy, but conceptually it's not bad            */

/* see if match different var names to same constant */
int illegal_match(instantiation_list insts)
{
  instantiation_list i,j;
  for(i = insts; i; i = i->next)
    for(j = i->next; j; j = j->next)
      if (strcmp(i->var_part,j->var_part) != SAME &&
	  strcmp(i->const_part,j->const_part) == SAME)
	return 1;
  return 0;
}

/****************here's the important one for making the graph*************/
/* this takes in an operator and an instantiation list, and a time.
   Inserts the operators and new facts and connects them appropriately.

   Inserts op into op_table[time]. attaches edges to fact_table[i] and
   fact_table[i+1].

   If the op can be avoided due to preconds being mutually exclusive, then
   op is still created, but no effects made. So, will be cleaned up
   on next pass.
 */
void make_graph_piece(op_ptr op, int time)
{
  vertex_t op_vert, fact_vert, fv2;
  effect_list elist; precond_list plist, p2;
  char str[100], str2[100];
  string_list temp;

  /* get op name */
  make_op_string(op, str);
  /* NEW: need to make sure that vars of different names match to different
     constants.  Otherwise can get into trouble. */
  if (illegal_match(op->insts)) {
    if (DEBUG_FLAG > 1) printf("discarding %s.\n",str);
    return;
  }

  /* first, insert the operator and get a pointer to it */
  op_vert = insert_into_table(op_table[time], str);

  
  /* if op can be avoided then return now */
  for(plist = op->preconds; plist; plist = plist->next) {
    instantiate_into_string(plist->item, op->insts, str,1);
    fact_vert = lookup_from_table(fact_table[time], str);
    for(p2 = plist->next; p2; p2 = p2->next) {
      instantiate_into_string(p2->item, op->insts, str2,1);
      fv2 = lookup_from_table(fact_table[time], str2);
      if (fact_vert == NULL || fv2 == NULL) {      /* wasn't there */
	do_error("didn't find precondition. Shouldn't happen");
      }
      if (are_mutex(fact_vert, fv2)) { /* DONT NEED TO PUT OP IN */
	if (DEBUG_FLAG) printf("Avoiding %s, time %d\n",op_vert->name, time);
	return;
      }
    }
  }

  /* set up the connections to the preconditions */
  for(plist = op->preconds; plist; plist = plist->next) {
    instantiate_into_string(plist->item, op->insts, str,1);
    fact_vert = lookup_from_table(fact_table[time], str);
    op_vert->in_edges = insert_edge(op_vert->in_edges, fact_vert);
    fact_vert->out_edges = insert_edge(fact_vert->out_edges, op_vert);
  }
      
  /* set up the connections to effects. Now allowing deletes */
  for(elist = op->effects; elist; elist = elist->next) {
    if (strcmp(elist->item->item, DELETE) != SAME) {
      instantiate_into_string(elist->item, op->insts, str,1);
      fact_vert = lookup_from_table(fact_table[time+1], str);
      if (fact_vert == NULL) {                     /* wasn't there */
	if (do_irrel) {  /* in this case, check to see if it's useful first */
	  if (lookup_from_table(useful,str) == NULL) {
	    if (DEBUG_FLAG > 2)
	      printf("%s not useful. Not putting into graph at time %d.\n",
		     str, time+1);
	    continue;
	  }
	}
	fact_vert = insert_into_table(fact_table[time+1],str);
      }
      op_vert->out_edges = insert_edge(op_vert->out_edges, fact_vert);
      fact_vert->in_edges = insert_edge(fact_vert->in_edges, op_vert);
    } else {
      /* just put into del list.  Don't try creating new node */
      instantiate_into_string(elist->item->next, op->insts, str,1);
      temp = (string_list) malloc(sizeof(token_list_elt));
      temp->item = (char *) calloc(strlen(str) + 1, sizeof(char));
      strcpy(temp->item, str);
      temp->next = op_vert->del_list;
      op_vert->del_list = temp;
    }
  }
}


/*** do one of the operators ***/
/* The do_operator_rec routine can be used to do forwards or backwards, 
   but is written using variable names as if going in forwards direction.

   This routine takes the list of facts true at current time, a ptr to an
   operator, a list of preconditions to go, and some instantiations.
   It calls make_graph_piece for each new node that needs to be created.

   --> Also takes in hash table.  For speedup...

   When d_o_rec matches, it calls itself recursively with
   flag=1 and replacing current facts by the types list, to make sure that
   those get matched.  Should now handle correctly the case where there
   is a parameter that is not one of the preconditions.
*/
void do_operator_rec(hashtable_t, fact_list, op_ptr, precond_list, int, int);

void do_operator(hashtable_t htable, fact_list current_facts, op_ptr op, 
		 precond_list p, int time)
{
  do_operator_rec(htable, current_facts,op,p,time,0);
}

/* calls make_graph_piece unless "going backwards", in which case calls
   list_useful_preconds. */
void list_useful_preconds(op_ptr op, int time);
void selector(op_ptr op, int time)
{
  if (!going_backwards) make_graph_piece(op,time);
  else list_useful_preconds(op,time);
}


void do_operator_rec(hashtable_t htable, fact_list current_facts, op_ptr op, 
                     precond_list precs_to_go, int time, int flag)
{
  fact_list factptr;
  token_list prec;
  int result;
  int i, old_matched[MAX_TOKENS]; /* 0 if const_part is NULL. Else 1 */
  instantiation_list iptr;
  char str[100];

  if (precs_to_go == NULL) { 
    /* if flag is 1, that means we're done. Otherwise, call recursively 
        * using the_types. */
    if (flag == 1) selector(op,time);
    else do_operator_rec(types_table, the_types, op, op->params, time, 1);
    return;
  }
  for(i=0, iptr = op->insts; iptr!= NULL; iptr = iptr->next, i++) {
    if (iptr->const_part == NULL) old_matched[i]=0;
    else old_matched[i] = 1;
  }
  prec =  precs_to_go->item;

  /* first see if it's fully instantiated.  If so, it's lots faster */
  if (instantiate_into_string(prec, op->insts, str,0)) { /* YAY! */
    if (lookup_from_table(htable, str))           /* found it */
      do_operator_rec(htable, current_facts, op, precs_to_go->next, time,flag);
    return;
  }
  /* now we know that it is NOT fully instantiated */

  for(factptr = current_facts; factptr != NULL; factptr = factptr->next) {
    result = compare_and_instantiate(prec, factptr->item, op->insts);
    if (result == NO) continue;  /* No match.  Most common */
    if (result == YES) do_error("bug in do_operator_rec");
    do_operator_rec(htable, current_facts, op,  precs_to_go->next, time, flag);
    /* now, un-instantiate the new ones */
    for(i=0,iptr=op->insts; iptr!=NULL; iptr = iptr->next, i++)
      if (old_matched[i] == 0) iptr->const_part = NULL;
  }
}

/* this routine takes an uninstantiated pattern, a fact, and a list of
   partially made instantiations.
   Sees if the precondition matches the fact given the
   instantiations so far.  Returns YES if exact match NO if no exact match
   and NEW_INSTS if new instantiations were needed.
 */
int compare_and_instantiate(token_list patt, token_list fact,
                            instantiation_list insts)
{
  token_list p,f;          /* p,f,iptr are pointers to go down the lists */
  instantiation_list iptr;
  int temp_arr[MAX_TOKENS]; /* temp_arr[i] = NO or NEW_INSTS */
  int i=0,result = YES;  /* change result to NEW_INSTS when add instantiatons*/

  /* Check the constant parts.  If any fail to match,  we can return.  */
  for(p = patt, f = fact; p && f; p = p->next, f = f->next)
    if (is_const(p->item) && !equal_tokens(p->item,f->item)) return NO;

  /* check they're the same length */
  if (p || f) return NO;

  /* initialize temp_arr */
  for(i=0,iptr=insts; iptr !=NULL; i++,iptr=iptr->next) temp_arr[i] = NO;

  for(p = patt, f = fact; p && f; p = p->next, f = f->next) {
    if (is_const(p->item)) continue;    /* we already know this matches */
    for(i=0,iptr = insts; iptr != NULL; i++, iptr = iptr->next)
      if (equal_tokens(p->item,iptr->var_part)) {  /* matched */
        if (iptr->const_part == NULL) {          /* need new inst */
          iptr->const_part = f->item;            /* so, do it */
          temp_arr[i] = NEW_INSTS;               /* and remember it */
          result = NEW_INSTS;                    /* and change result */
        } else if (!equal_tokens(f->item,iptr->const_part)) { /* not equal*/
          result=NO;
	}
	break;
      }
    if (iptr == NULL) {
      sprintf(junk,"Badly formed operator: '%s' not a parameter",p->item);
      do_error(junk);
    }
    if (result == NO) break;
  }

  /* if result is NO, then reset insts */
  if (result == NO)
    for(i=0,iptr=insts; iptr !=NULL; i++,iptr=iptr->next) {
      if (temp_arr[i] == NEW_INSTS) iptr->const_part = NULL;
    }
  return result;
}

/*************************END OF OP->PART_OF_GRAPH section***************/




/***************TRY TO GET RID OF IRRELEVANT FEATURES, IF WE'RE ASKED TO***/
/* ------------------------------------------ 
   Get rid of irrelevant features by going backwards from goal to see
   all the possible things that might be useful.
   Should give this a parameter that says how long to spend before giving up,
   but for now, let's ignore that issue.

   Idea: start with hashtable containing all goals, marked as un-visited
   (used == 0) and at iteration 0 (is_true == 0).  Go through all goals
   at current iteration and for each one, mark as used and then for each op, 
   see if the goal is an add-effect of the op. If so, put into the 
   hashtable all the possible preconditions for the op (use the-types) that 
   aren't already there, and set their is_true to the current time +1.

   Keep going until everything is used.

   Don't free up storage: keep around the table for use in keeping graph
   pruned.

 */

static int num_facts_searched = 0; /* how many looked at. For info. */

/** prototypes **/
fact_list delete_useless(fact_list f);
int main_search_loop(int time, op_list ops);
int const_parts_match(token_list patt, token_list fact);
void init_useful(void);
int add_fact(fact_list f);

fact_list useful_facts(op_list ops, fact_list f)
{
  int time = 0;
  going_backwards = 1;
  printf("beginning pre-preprocessing...\n");
  init_useful();  /* set up the hash table */
  while(main_search_loop(time++, ops))
    {printf("time %d\n",time);} /* if (time == 2) exit(0);}  */
  printf("found %d possibly useful facts.\n",num_facts_searched);
  going_backwards = 0;
  f = delete_useless(f);
  printf("...done pre-preprocessing.\n");
  return f;
}

fact_list delete_useless(fact_list f)
{
  char str[100];
  if (f == NULL) return f;
  instantiate_into_string(f->item, NULL, str,1);
  if (lookup_from_table(useful, str)) {
    f->next = delete_useless(f->next);
  } else {
    printf("Noticed fact %s not useful.\n",str);
    f = delete_useless(f->next);  /* should really free storage here...*/
  }
  return f;
}

void list_useful_preconds(op_ptr op, int time)
{
  precond_list plist;
  char str[100];
  vertex_t v;
  if (illegal_match(op->insts)) return;  /* not a legal match of stuff */
  for(plist = op->preconds; plist; plist = plist->next) {
    instantiate_into_string(plist->item, op->insts, str,1);
    if (lookup_from_table(useful, str)) continue; /* don't insert */
    v = insert_into_table(useful, str);
/*    printf("%s\n",str); */
    v->is_true = time;
  }
}

/* returns 0 if don't need to continue. Returns 1 if did something. */
int main_search_loop(int time, op_list ops)
{
  vertex_t v;
  op_ptr op;
  effect_list e;
  int return_val = 0;
  fact_list_elt effect, goal;
  effect.next = goal.next = NULL;

  get_next(useful,0);
  while((v = get_next(useful,1)) != NULL) {
    if (v->is_true != time || v->used) continue;
    v->used = 1;  /* mark as used */
    ++num_facts_searched;
    goal.item = token_list_from_string(v->name);/* put into token list format*/

    for(op = ops; op; op = op->next) {
      for(e = op->effects; e; e = e->next) {
	if(!add_fact(e)) continue;  /* only want to look at adds */
	       /* for speed, first check const parts */
	if(!const_parts_match(e->item,goal.item)) continue;
	effect.item = e->item;  /* set up one-element list */
	return_val = 1;
	do_operator(useful, &goal, op, &effect, time+1);
/*	printf("%d. Did %s for goal %s\n",num_facts_searched,op->name, v->name); */
      }
    }
  }
  return return_val;
}

/* given an effect(patt) and a goal(fact). See if constant parts match */
int const_parts_match(token_list patt, token_list fact)
{
  token_list p,f;
  for(p = patt, f = fact; p && f; p = p->next, f = f->next)
    if (is_const(p->item) && !equal_tokens(p->item,f->item)) return 0;
  if (p || f) return 0;  /* different lengths */
  return 1;
}

void init_useful(void)
{
  fact_list temp;
  for(temp = the_goals; temp; temp = temp->next)
    insert_token_list(useful, temp->item);
}


/* is the first fact an effect that's added? */
int add_fact(fact_list f)
{
  if (strcmp(f->item->item, DELETE) == SAME) return 0;
  else return 1;
}

