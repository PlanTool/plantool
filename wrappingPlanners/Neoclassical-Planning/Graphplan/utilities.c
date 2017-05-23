/*********************** Graphplan **************************************
  (C) Copyright 1995 Avrim Blum and Merrick Furst

  All rights reserved. Use of this software is permitted for non-commercial
  research purposes, and it may be copied only for that use.  All copies must
  include this copyright message.  This software is made available AS IS, and
  neither the authors nor CMU make any warranty about the software or its 
  performance. 
*************************************************************************/


#include "graphplan.h"
#ifndef FALSE
#define FALSE 0
#endif
extern hashtable_t *fact_table, *op_table;  /* the arrays of hash tables */
extern int hash_hits, hash_inserts, hash_numctr, hash_ctr;
extern int do_noexcl;
char templine[100]; /* global var that can use to store lines read in */

fact_list fact_list_from_vertex_list(vertex_t vlist);


/***given a string, makes a new one with NOOP in front***
  Returns new storage.
*/
char *make_noop_string(char *str)
{
  char *newp;
  newp = (char *) calloc(strlen(str) + strlen(NOOP) + 2, sizeof(char));
  sprintf(newp,"%s_%s",NOOP,str);
  return newp;
}

/*set uid_mask and uid_block*/
void set_uid(vertex_t v, int id)
{
  v->uid_block = id/32;
  v->uid_mask = 1 << (id % 32);
}


edgelist_t insert_edge(edgelist_t e, vertex_t v)
{
  edgelist_t newedge = (edgelist_t) calloc(1,sizeof(edgelist_s));
  newedge->next = e;
  newedge->endpt = v;
  return newedge;
}

/* like above, but insert at end of list */
edgelist_t insert_edge_at_end(edgelist_t e, vertex_t v)
{
  edgelist_t newedge;
  if (e == NULL) {
    newedge = (edgelist_t) calloc(1,sizeof(edgelist_s));
    newedge->endpt = v;
    return newedge;
  } else {
    e->next = insert_edge_at_end(e->next, v);
    return e;
  }
}


/* makes a fact list from the hash table */
fact_list make_list_from_htable(hashtable_t h)
{
  int i;
  fact_list the_facts = NULL,current, p;
  for(i=0; i < HSIZE; ++i)
    if (h[i] != NULL) {
      current = fact_list_from_vertex_list(h[i]);
      for(p = current;  p->next; p = p->next);
      p->next = the_facts;
      the_facts = current;
    }
  return( the_facts );
}

void completely_free_fact_list(fact_list l)
{
  fact_list temp;
  while(l) { temp = l->next; free_token_list(l->item); free(l); l = temp;}
}

void free_token_list(token_list l)
{
  token_list temp;
  while(l) {   temp = l->next; free(l->item); free(l); l = temp;  }
}


/* makes string list into fact list. */
fact_list fact_list_from_vertex_list(vertex_t vlist)
{
  fact_list retval = NULL, f;
  char *str;
  for(; vlist; vlist = vlist->next) {
    str = vlist->name;
    f = (fact_list) malloc(sizeof(fact_list_elt));
    f->next = retval;
    retval = f;
    f->item = token_list_from_string(str);
  }
  return retval;
}

token_list token_list_from_string(char str[])
{
  char *p = str, *q, temp[50];
  token_list_elt dummy, *current;
  current = &dummy;
  while (*p) {
    current->next = (token_list) malloc(sizeof(token_list_elt));
    current = current->next;
    for(q = temp; (*p != '\0') && (*p != CONNECTOR); *q++ = *p++);
    if (*p == CONNECTOR) ++p;
    *q = '\0';
    current->item = (char *) calloc(1 + strlen(temp),sizeof(char));
    strcpy(current->item, temp);
  }
  current->next = NULL;
  return( dummy.next );
}



/* insert into instantiation list */
instantiation_list insert_inst(char *v, char *c, instantiation_list i)
{
  instantiation_list temp=(instantiation_list) malloc(sizeof(instantiation_s));
  temp->var_part = v;
  temp->const_part = c;
  temp->next = i;
  return( temp );
}


/* Returns instantiate token list in a string (last argument) Uses
   CONNECTOR to connect the tokens together. 
   if "failflag" is 0 then Return 1 if ok, 0 if failure.
   Otherwise, exit on failure.
   */
int instantiate_into_string(token_list l, instantiation_list inst,char str[],
			    int failflag)
{
  instantiation_list i;
  char *p;
  token_list t = l;

  for(p = str; t; t = t->next) {
    if (is_const(t->item))  /* just copy over */
      { strcpy(p,t->item); p+=strlen(t->item); *p++ = CONNECTOR; }
    else {
      for(i=inst; i; i = i->next)
        if (strcmp(i->var_part, t->item) == SAME) {
	  if (i->const_part == NULL) {i = NULL; break;}
          strcpy(p,i->const_part);p+=strlen(i->const_part);*p++ = CONNECTOR;
          break;
        }
      if (!i) {
	if (failflag) do_error("instantiation failed");
	return 0;
      }
    }
  }
  *(--p) = '\0';
  return 1;
}

/* "whitespace" is blank or paren */
int is_stopper(int c)
{
  return (isspace(c) || (c == ')') || (c == '('));
}

/* read the next "item".  An item is either a string of non-parentesis,
   non-blank characters, or a parenthesis.  In the former case, put into
   str and return OK. In the latter case, return LEFT_PAREN or RIGHT_PAREN
   appropriately.  Return EOF on end-of-file.

   if character is CONNECTOR, change it to REPLACEMENT
 */
#define REPLACEMENT '.'
int read_item(FILE *fp, char *str)
{
  int letter;
  /* first read in any blankspace and check for EOF */
  while(isspace(letter = getc(fp)));
  if (letter == EOF) return EOF;

  /* check if parenthesis */
  if (letter == '(') return LEFT_PAREN;
  if (letter == ')') return RIGHT_PAREN;
  
  ungetc((char) letter, fp);
  while(!is_stopper(*str++ = getc(fp)))    /* read word in */
    if (*(str-1) == CONNECTOR) *(str-1) = REPLACEMENT;  /* change '_'to'.'*/
  ungetc(*--str, fp);                      /* went one too far */
  *str = '\0';                             /* end it nicely */
  return OK;
}

/* string to describe an instantiated operator: re-reverse order */
void rec_make_op_string(op_ptr op,instantiation_list insts, char *str)
{
  if (insts == NULL) return;
  rec_make_op_string(op, insts->next, str);
  if (insts->const_part == NULL) 
    {fprintf(stderr,"op %s, ",op->name); do_error("bad instantiation");}
  sprintf(str + strlen(str), "_%s", insts->const_part);
}
/* string to describe an instantiated operator */
void make_op_string(op_ptr op, char *str)
{
  sprintf(str, "%s",op->name);
  rec_make_op_string(op, op->insts, str + strlen(str));
}

void do_error(char *s)
{
  fprintf(stderr,"%s\n",s); exit( 1 );
}

int equal_facts(token_list f1, token_list f2)
{
  for(; f1 && f2; f1 = f1->next, f2 = f2->next)
    if (!equal_tokens(f1->item,f2->item)) return 0;  /* different tokens*/
  if (f1 || f2) return 0;  /* different lengths */
  return 1;
}


/************ ROUTINES FOR DOING MUTUAL EXCLUSION RELATIONS ************
 ************ --SHOULD PROBABLY BE IN A DIFFERENT FILE      ************/

/* Note: the first fact layer doesn't have uids set, but then again, they
   aren't exlusive either */
int are_mutex(vertex_t v1, vertex_t v2)
{
  return ((v1->exclusive_vect[v2->uid_block]) & (v2->uid_mask));
}

int are_mutex_in_this_step(vertex_t v1, vertex_t v2)
{
  return ((v1->excl_in_this_step_vect[v2->uid_block]) & (v2->uid_mask));
}


/* Given time in op hash table, finds all mutually exclusive ops.
 */
void find_all_mutex_ops(hashtable_t harr[], int time)
{
  vertex_t v;
  get_next(harr[time],0); /* initialize */
  while((v = get_next(harr[time],1)) != NULL)
    find_mutex_ops(v, time);
  return;
}

/* make exclusive if they aren't already */
void do_exclusive(vertex_t v, vertex_t v2)
{
  if (are_mutex(v,v2)) return;
  v->exclusive_vect[v2->uid_block] |= v2->uid_mask;
  v2->exclusive_vect[v->uid_block] |= v->uid_mask;
  v->exclusive = insert_edge(v->exclusive, v2);
  v2->exclusive = insert_edge(v2->exclusive, v);
}
	

/* Given a vertex representing an operator, it finds other mutually-exclusive
 * ops. 
 *
 * The method is: (1) if a precond p is mutually exclusive with another fact 
 * q s.t. q is a precond for another op then the ops are also mutually 
 * exclusive. 
 *
 * LOCAL PART: If the operator deletes something that is either 
 * someone else's precondition or someone else's effect, then the two ops 
 * are exclusive.  THE REASON FOR THIS is (I) if you delete a precond then 
 * impossible to do both, and (II) if you delete an effect, then the order of 
 * these ops will matter, though this only is a problem in domains where an op
 * might add a fact that was already there (otherwise these two can't possibly 
 * happen at the same time anyway).
 *
 * NEW: Make noop exclusive of any other action which adds its effect. The
 * Reason is that you need never include both in a plan.
 *
 * ALSO: put into del_edges.
 *
 * For speedup: for type(1) only call do_exclusive for vertices v2 
 * for which v<v2 as ptrs
 */
void find_mutex_ops(vertex_t v, int time)
{
  string_list slist;
  edgelist_t temp1, temp2, temp3;
  vertex_t v2, nextfact, prevfact;

  /* Do type (1) */
  for(temp1 = v->in_edges; temp1; temp1 = temp1->next) {
    for(temp2 = temp1->endpt->exclusive; temp2; temp2 = temp2->next) {
      for(temp3 = temp2->endpt->out_edges; temp3; temp3 = temp3->next) {
	v2 = temp3->endpt;  /* this is the op to check */
	if ((int) v < (int) v2) do_exclusive(v, v2);    /* Do it */
      }
    }
  }
  
  /* LOCAL PART: */
  for(slist = v->del_list; slist; slist = slist->next) {
    nextfact = lookup_from_table(fact_table[time+1], slist->item);
    if (nextfact == NULL) {
      if (DEBUG_FLAG > 1) printf("%s deleting nonexistent fact\n",v->name);
      continue;
    }
    prevfact = nextfact->prev_time;
    /* do preconditions */
    if (prevfact) {
      for(temp2 = prevfact->out_edges; temp2; temp2 = temp2->next) {
	if (v != temp2->endpt) {
	  do_exclusive(v, temp2->endpt);
	}
      }
    }
    /* now effects */
    for(temp2 = nextfact->in_edges; temp2; temp2 = temp2->next) {
      if (v != temp2->endpt) {
	do_exclusive(v, temp2->endpt);
      }
    }
    /* now put into del_edges (and put info into FACT too, for DELETES)*/
    v->del_edges = insert_edge(v->del_edges, nextfact);
    nextfact->del_edges = insert_edge(nextfact->del_edges,v);
  }
  /* free storage of delete list */
  while(v->del_list) {
    slist = v->del_list;
    v->del_list = v->del_list->next;
    free(slist);
  }
  /* SPECIAL for NOOPs */
  if (v->is_noop) {
    for(temp2 = v->out_edges->endpt->in_edges; temp2; temp2 = temp2->next) {
      if (v != temp2->endpt) 
	do_exclusive(v, temp2->endpt);
    }
  }
}

/* Given two vertices representing facts, are they mutually exclusive?
 * just see if all ops generating p are exclusive of all ops generating q.
 */
int are_facts_exclusive(vertex_t p, vertex_t q)
{
  edgelist_t temp1, temp2;
  if (do_noexcl) return 0; /* option to turn off exclusivity */
  for(temp1 = p->in_edges; temp1; temp1 = temp1->next) {
    for(temp2 = q->in_edges; temp2; temp2 = temp2->next) {
      if (!are_mutex(temp1->endpt, temp2->endpt)) return 0;
    }
  }
  if (DEBUG_FLAG>2) printf("Facts %s and %s mutually exclusive\n",
			   p->name, q->name);
  return 1;
}



/* Given time in fact hash table, finds all mutually exclusive facts.
 * Assumes there are no more than MAXNODES facts at this time step.
 * Returns number of facts and number of exclusive pairs.
 *
 * Note: varr is used in find_currently_mutex_facts too.
 *
 */
static vertex_t *varr;
pair find_mutex_facts(hashtable_t harr[], int time)
{
  static int flag = 0;
  pair retval;
  edgelist_t e;
  vertex_t v;
  int i, j, fcount=0, ecount=0;
  if (flag == 0) {  /* doing this since MAXNODES is not a constant */
    varr = (vertex_t *) malloc(MAXNODES*sizeof(vertex_t));
    flag = 1;
  }
  get_next(harr[time],0); /* initialize */
  for(i=0; (varr[i] = get_next(harr[time],1)) != NULL; ++i);
  for(i=0; varr[i] != NULL; ++i) {
    ++fcount;
    /* if it's NEW, then check against everybody */
    if (!varr[i]->prev_time) {
      for(j=0; varr[j] != NULL; ++j) {
	if (!varr[j]->prev_time && j <= i) continue; /* already checked */
	if (are_facts_exclusive(varr[i], varr[j])) {
	  ++ecount;
	  do_exclusive(varr[i], varr[j]);
	}
      }
    } else { /* OLD, so only check previous exclusives */
      for(e = varr[i]->prev_time->exclusive; e; e = e->next) {
	v = e->endpt->next_time;
	if ((int) v < (int) varr[i]) continue;  /* get in other direction */
	if (are_facts_exclusive(varr[i], v)) {
	  ++ecount;
	  do_exclusive(varr[i], v);
	}
      }
    }
  }
  retval.first = fcount;
  retval.second = ecount;
  return retval;
}

/* This routine looks at a pairs of facts at a given time step such that 
   neither fact is in the initial state, and returns 1 if they cannot both 
   be created by ops in this time step.

   Note: be sure to skip over the noops here.  
*/
int are_facts_excl_in_this_step(vertex_t v1, vertex_t v2)
{
  edgelist_t e1, e2;
  for(e1 = v1->in_edges; e1; e1 = e1->next) {
    if (IS_NOOP(e1->endpt)) continue;
    for(e2 = v2->in_edges; e2; e2 = e2->next) {
      if (IS_NOOP(e2->endpt)) continue;
      if (!are_mutex(e1->endpt, e2->endpt)) return 0;
    }
  }
  return 1;
}

/* make them excl in this step */
void do_excl_in_this_step(vertex_t v1, vertex_t v2)
{
  v1->excl_in_this_step_vect[v2->uid_block] |= v2->uid_mask;
  v2->excl_in_this_step_vect[v1->uid_block] |= v1->uid_mask;
  v1->excl_in_this_step = insert_edge(v1->excl_in_this_step, v2);
  v2->excl_in_this_step = insert_edge(v2->excl_in_this_step, v1);
}
	
/* This routine looks at all pairs of facts a given time step and for each
   pair such that neither fact is in the initial state marks the two facts as
   "excl_in_this_step" if they cannot both be created by ops in this time step.

   Using same storage as find_mutex_facts, so CALL THIS ONE AFTER
   CALLING find_mutex_facts.
 */
void find_currently_mutex_facts()
{
  edgelist_t e;
  vertex_t v2;
  int i, j;
    
  /* initialization of varr done already in find_mutex_facts */

  for(i=0; varr[i] != NULL; ++i) {
    if (lookup_from_table(fact_table[0],varr[i]->name)) continue;

    /* if it's NEW, then check against everybody */
    if (!varr[i]->prev_time) {
      for(j=0; varr[j] != NULL; ++j) {
	if (!varr[j]->prev_time && j <= i) continue; /* already checked */
	if (lookup_from_table(fact_table[0],varr[j]->name)) continue;
	if (are_facts_excl_in_this_step(varr[i], varr[j]))
	  do_excl_in_this_step(varr[i], varr[j]);
      }
    } else {               /* OLD, so only check previous exclusives */
      for(e = varr[i]->prev_time->excl_in_this_step; e; e = e->next) {
	v2 = e->endpt->next_time;
	if ((int) v2 < (int) varr[i]) continue;  /* get in other direction */
	if (are_facts_excl_in_this_step(varr[i], v2))
	  do_excl_in_this_step(varr[i], v2);
      }
    }
  }
}
/*************************END MUTUAL EXCLUSION PART**********************/





/************ ROUTINES FOR PRINTING OUT INFORMATION TO USER**************/
void print_info_piece(hashtable_t t, int flag)
{
  vertex_t vert;
  edgelist_t edge;
  get_next(t, 0); /* get ready */
  while((vert = get_next(t, 1)) != NULL) {
    if (vert->needed == 0 || IS_NOOP(vert)) continue;
    printf("%s\n",vert->name);
    if (flag>=2 && ((edge = vert->exclusive) != NULL)) {
      printf("   exclusive: "); /* now, includes noops */
      for(; edge; edge = edge->next)
	if (edge->endpt->needed) printf("%s ",edge->endpt->name);
      printf("\n");
    }
  }
}

/* for printing out info to user */
void print_info(int len)
{
  int i;
printf("Printing graph. For clarity, ignoring parts unreachable by planner\n");
  remove_unneeded_vertices(len);  /**just for printing **/

  for(i=0; i < len; ++i) {
    printf("\nTime: %d\n",i+1);
    print_info_piece(op_table[i], DEBUG_FLAG);
    if (DEBUG_FLAG >= 2) {
      printf("\nFacts: \n");
      print_info_piece(fact_table[i+1], 2); /* PRINT FACTS*/
    }
  }
}


/************ more random utilities ***************/
void read_initial_comments(FILE *fp)
{
  int c;
  while((c = getc(fp)) != '(');  /* read up to first left paren */
}

int allocs_in_use;

void my_free(void * p) {
    allocs_in_use--;
    free(p);
}
void * my_alloc(int size) {
    void * p = malloc(size);
    allocs_in_use++;
    if (p == NULL) {
        fprintf(stderr, "Ran out of space.  Requested size=%d.\n", size);
        exit(1);
    }
    return p;
}

void print_alloc(void)
{ printf("allocs - frees = %d\n",allocs_in_use); }


void yyerror(char *x)
{
	printf("%s\n",x);
}

fact_list fact_list_append(fact_list f1, fact_list f2)
{
	if (f2 == NULL) return f1;

	if (f1 == NULL) return f2;

	f1->next = fact_list_append(f1->next, f2);

	return f1;
}


token_list token_list_append(token_list f1, token_list f2)
{
	if (f2 == NULL) return f1;

	if (f1 == NULL) return f2;

	f1->next = token_list_append(f1->next, f2);

	return f1;
}
void print_cant_do(int time)
{
  fprintf(stderr,"Can't solve in %d steps\n",time);
  printf("%d entries in hash table and %d hits.\n",hash_inserts, hash_hits);
  printf("   avg set size %d\n", hash_numctr/hash_ctr);
}
