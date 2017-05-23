/***** ltl2ba : generalized.c *****/

/* Written by Denis Oddoux, LIAFA, France                                 */
/* Copyright (c) 2001  Denis Oddoux                                       */
/*                                                                        */
/* This program is free software; you can redistribute it and/or modify   */
/* it under the terms of the GNU General Public License as published by   */
/* the Free Software Foundation; either version 2 of the License, or      */
/* (at your option) any later version.                                    */
/*                                                                        */
/* This program is distributed in the hope that it will be useful,        */
/* but WITHOUT ANY WARRANTY; without even the implied warranty of         */
/* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the          */
/* GNU General Public License for more details.                           */
/*                                                                        */
/* You should have received a copy of the GNU General Public License      */
/* along with this program; if not, write to the Free Software            */
/* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA*/
/*                                                                        */
/* Based on the translation algorithm by Gastin and Oddoux,               */
/* presented at the CAV Conference, held in 2001, Paris, France 2001.     */
/* Send bug-reports and/or questions to: Denis.Oddoux@liafa.jussieu.fr    */
/* or to Denis Oddoux                                                     */
/*       LIAFA, UMR 7089, case 7014                                       */
/*       Universite Paris 7                                               */
/*       2, place Jussieu                                                 */
/*       F-75251 Paris Cedex 05                                           */
/*       FRANCE                                                           */    

#include "ltl2ba.h"

/********************************************************************\
|*              Structures and shared variables                     *|
\********************************************************************/

extern FILE *tl_out;
extern ATrans **transition;
extern struct tms t_debut, t_fin;
extern int tl_verbose, tl_stats, tl_simp_diff, tl_simp_fly, tl_fjtofj,
  tl_simp_scc, *final_set, node_id;
extern char **sym_table;

GState *gstack, *gremoved, *gstates, **init;
GScc *scc_stack;
int init_size = 0, gstate_id = 1, gstate_count = 0, gtrans_count = 0;
int *fin, *final, rank, scc_id, *bad_scc;

void print_generalized();

/********************************************************************\
|*        Simplification of the generalized Buchi automaton         *|
\********************************************************************/

void free_gstate(GState *s) /* frees a state and its transitions */
{
  free_gtrans(s->trans->nxt, s->trans, 1);
  tfree(s->nodes_set);
  tfree(s);
}

GState *remove_gstate(GState *s, GState *s1) /* removes a state */
{
  GState *prv = s->prv;
  s->prv->nxt = s->nxt;
  s->nxt->prv = s->prv;
  free_gtrans(s->trans->nxt, s->trans, 0);
  s->trans = (GTrans *)0;
  tfree(s->nodes_set);
  s->nodes_set = 0;
  s->nxt = gremoved->nxt;
  gremoved->nxt = s;
  s->prv = s1;
  for(s1 = gremoved->nxt; s1 != gremoved; s1 = s1->nxt)
    if(s1->prv == s)
      s1->prv = s->prv;
  return prv;
} 

void copy_gtrans(GTrans *from, GTrans *to) /* copies a transition */
{
  to->to = from->to;
  copy_set(from->pos,   to->pos,   1);
  copy_set(from->neg,   to->neg,   1);
  copy_set(from->final, to->final, 0);
}

int same_final(GState *a, GTrans *s, GState *b, GTrans *t, int use_scc) {
  return (same_sets(s->final, t->final, 0) || 
	  (use_scc && 
	   ((a->incoming != s->to->incoming) ||
	    (b->incoming != t->to->incoming) ||
	    in_set(bad_scc, a->incoming) ||
	    in_set(bad_scc, b->incoming))));
}


int same_gtrans(GState *a, GTrans *s, GState *b, GTrans *t, int use_scc) 
{ /* returns 1 if the transitions are identical */
  if((s->to != t->to) ||
     ! same_sets(s->pos,   t->pos,   1) ||
     ! same_sets(s->neg,   t->neg,   1))
    return 0; /* transitions differ */
  if(same_final(a, s, b, t, 0))
    return 3; /* same transitions exactly : any state can be removed */
  if(!use_scc)
    return 0; /* transitions differ */
  if(a->incoming == b->incoming)
    return 3; /* any state can be removed */
  if(a->incoming > b->incoming) /* b can be a son of a */
    return 2; /* do not remove state b */
  return 1; /* do not remove state a */
}

int simplify_gtrans() /* simplifies the transitions */
{
  int changed = 0;
  GState *s;
  GTrans *t;

  if(tl_stats) times(&t_debut);

  for(s = gstates->nxt; s != gstates; s = s->nxt)
    for(t = s->trans->nxt; t != s->trans;) {
      GTrans *t1 = s->trans->nxt;
      copy_gtrans(t, s->trans);
      while ((t == t1) || (t1->to != t->to) ||
	     !included_set(t1->pos, t->pos, 1) ||
	     !included_set(t1->neg, t->neg, 1) ||
	     !same_final(s, t, s, t1, tl_simp_scc))
	t1 = t1->nxt;
      if(t1 != s->trans) {
	GTrans *free = t->nxt;
	t->to = free->to;
	copy_set(free->pos, t->pos, 1);
	copy_set(free->neg, t->neg, 1);
	copy_set(free->final, t->final, 0);
	t->nxt   = free->nxt;
	if(free == s->trans) s->trans = t;
	free_gtrans(free, 0, 0);
	changed++;
      }
      else
	t = t->nxt;
    }

  if(tl_stats) {
    times(&t_fin);
    fprintf(tl_out, "\nSimplification of the generalized Buchi automaton - transitions: %.2fs");
    fprintf(tl_out, "\n%i transitions removed\n", 
	   ((float)(t_fin.tms_utime - t_debut.tms_utime))/100, changed);
  }

  return changed;
}

void retarget_all_gtrans()
{             /* redirects transitions before removing a state from the automaton */
  GState *s;
  GTrans *t;
  int i;
  for (i = 0; i < init_size; i++)
    if (init[i] && !init[i]->trans) /* init[i] has been removed */
      init[i] = init[i]->prv;
  for (s = gstates->nxt; s != gstates; s = s->nxt)
    for (t = s->trans->nxt; t != s->trans; )
      if (!t->to->trans) { /* t->to has been removed */
	t->to = t->to->prv;
	if(!t->to) { /* t->to has no transitions */
	  GTrans *free = t->nxt;
	  t->to = free->to;
	  copy_set(free->pos, t->pos, 1);
	  copy_set(free->neg, t->neg, 1);
	  copy_set(free->final, t->final, 0);
	  t->nxt   = free->nxt;
	  if(free == s->trans) s->trans = t;
	  free_gtrans(free, 0, 0);
	}
	else
	  t = t->nxt;
      }
      else
	t = t->nxt;
  while(gremoved->nxt != gremoved) { /* clean the 'removed' list */
    s = gremoved->nxt;
    gremoved->nxt = gremoved->nxt->nxt;
    if(s->nodes_set) tfree(s->nodes_set);
    tfree(s);
  }
}

int all_gtrans_match(GState *a, GState *b, int use_scc) 
{ /* decides if the states are equivalent */
  GTrans *s, *t;
  int i;
  int which_remove = 3; /* any state can be removed */
  for (s = a->trans->nxt; s != a->trans; s = s->nxt) { 
                                /* all transitions from a appear in b */
    copy_gtrans(s, b->trans);
    t = b->trans->nxt;
    while(! (i = same_gtrans(a, s, b, t, use_scc)))/* = and not == */
      t = t->nxt;
    if(t == b->trans) return 0;
    which_remove &= i;
    
  }
  for (t = b->trans->nxt; t != b->trans; t = t->nxt) { 
                                /* all transitions from b appear in a */
    copy_gtrans(t, a->trans);
    s = a->trans->nxt;
    while(! (i = same_gtrans(a, s, b, t, use_scc)))/* = and not == */
      s = s->nxt;
    if(s == a->trans) return 0;
    which_remove &= i;
  }
  return which_remove;
}

int simplify_gstates() /* eliminates redundant states */
{
  int changed = 0;
  GState *s, *s1;
  int i;

  if(tl_stats) times(&t_debut);

  for(s = gstates->nxt; s != gstates; s = s->nxt) {
    if(s->trans == s->trans->nxt) { /* s has no transitions */
      s = remove_gstate(s, (GState *)0);
      changed++;
      continue;
    }
    gstates->trans = s->trans;
    s1 = s->nxt;
    while(!(i = all_gtrans_match(s, s1, tl_simp_scc))) /* = and not == */
      s1 = s1->nxt;
    if(s1 != gstates) { /* s and s1 are equivalent */
      if(i & 2) s = remove_gstate(s, s1);
      else remove_gstate(s1, s);
      changed++;
    }
  }
  retarget_all_gtrans();

  if(tl_stats) {
    times(&t_fin);
    fprintf(tl_out, "\nSimplification of the generalized Buchi automaton - states: %.2fs");
    fprintf(tl_out, "\n%i states removed\n", 
	   ((float)(t_fin.tms_utime - t_debut.tms_utime))/100, changed);
  }

  return changed;
}

int gdfs(GState *s) {
  GTrans *t;
  GScc *c;
  GScc *scc = (GScc *)tl_emalloc(sizeof(GScc));
  scc->gstate = s;
  scc->rank = rank;
  scc->theta = rank++;
  scc->nxt = scc_stack;
  scc_stack = scc;

  s->incoming = 1;

  for (t = s->trans->nxt; t != s->trans; t = t->nxt) {
    if (t->to->incoming == 0) {
      int result = gdfs(t->to);
      scc->theta = min(scc->theta, result);
    }
    else {
      for(c = scc_stack->nxt; c != 0; c = c->nxt)
	if(c->gstate == t->to) {
	  scc->theta = min(scc->theta, c->rank);
	  break;
	}
    }
  }
  if(scc->rank == scc->theta) {
    while(scc_stack != scc) {
      scc_stack->gstate->incoming = scc_id;
      scc_stack = scc_stack->nxt;
    }
    scc->gstate->incoming = scc_id++;
    scc_stack = scc->nxt;
  }
  return scc->theta;
}

void simplify_gscc() {
  GState *s;
  GTrans *t;
  int i, **scc_final;
  rank = 1;
  scc_stack = 0;
  scc_id = 1;

  if(gstates == gstates->nxt) return;

  for(s = gstates->nxt; s != gstates; s = s->nxt)
    s->incoming = 0; /* state color = white */

  for(i = 0; i < init_size; i++)
    if(init[i] && init[i]->incoming == 0)
      gdfs(init[i]);

  scc_final = (int **)tl_emalloc(scc_id * sizeof(int *));
  for(i = 0; i < scc_id; i++)
    scc_final[i] = make_set(-1,0);

  for(s = gstates->nxt; s != gstates; s = s->nxt)
    if(s->incoming == 0)
      s = remove_gstate(s, 0);
    else
      for (t = s->trans->nxt; t != s->trans; t = t->nxt)
	if(t->to->incoming == s->incoming)
	  merge_sets(scc_final[s->incoming], t->final, 0);

  clear_set(bad_scc, 0);

  for(i = 0; i < scc_id; i++)
    if(!included_set(final_set, scc_final[i], 0))
       add_set(bad_scc, i);

  for(i = 0; i < scc_id; i++)
    tfree(scc_final[i]);
  tfree(scc_final);
}

/********************************************************************\
|*        Generation of the generalized Buchi automaton             *|
\********************************************************************/

int is_final(int *from, ATrans *at, int i) /*is the transition final for i ?*/
{
  ATrans *t;
  int in_to;
  if((tl_fjtofj && !in_set(at->to, i)) ||
    (!tl_fjtofj && !in_set(from,  i))) return 1;
  in_to = in_set(at->to, i);
  rem_set(at->to, i);
  for(t = transition[i]; t; t = t->nxt)
    if(included_set(t->to, at->to, 0) &&
       included_set(t->pos, at->pos, 1) &&
       included_set(t->neg, at->neg, 1)) {
      if(in_to) add_set(at->to, i);
      return 1;
    }
  if(in_to) add_set(at->to, i);
  return 0;
}

GState *find_gstate(int *set, GState *s) 
{ /* finds the corresponding state, or creates it */

  if(same_sets(set, s->nodes_set, 0)) return s; /* same state */

  s = gstack->nxt; /* in the stack */
  gstack->nodes_set = set;
  while(!same_sets(set, s->nodes_set, 0))
    s = s->nxt;
  if(s != gstack) return s;

  s = gstates->nxt; /* in the solved states */
  gstates->nodes_set = set;
  while(!same_sets(set, s->nodes_set, 0))
    s = s->nxt;
  if(s != gstates) return s;

  s = gremoved->nxt; /* in the removed states */
  gremoved->nodes_set = set;
  while(!same_sets(set, s->nodes_set, 0))
    s = s->nxt;
  if(s != gremoved) return s;

  s = (GState *)tl_emalloc(sizeof(GState)); /* creates a new state */
  s->id = (empty_set(set, 0)) ? 0 : gstate_id++;
  s->incoming = 0;
  s->nodes_set = dup_set(set, 0);
  s->trans = emalloc_gtrans(); /* sentinel */
  s->trans->nxt = s->trans;
  s->nxt = gstack->nxt;
  gstack->nxt = s;
  return s;
}

void make_gtrans(GState *s) { /* creates all the transitions from a state */
  int i, *list, state_trans = 0, trans_exist = 1;
  GState *s1;
  GTrans *t;
  ATrans *t1, *free;
  AProd *prod = (AProd *)tl_emalloc(sizeof(AProd)); /* initialization */
  prod->nxt = prod;
  prod->prv = prod;
  prod->prod = emalloc_atrans();
  clear_set(prod->prod->to,  0);
  clear_set(prod->prod->pos, 1);
  clear_set(prod->prod->neg, 1);
  prod->trans = prod->prod;
  prod->trans->nxt = prod->prod;
  list = list_set(s->nodes_set, 0);

  for(i = 1; i < list[0]; i++) {
    AProd *p = (AProd *)tl_emalloc(sizeof(AProd));
    p->astate = list[i];
    p->trans = transition[list[i]];
    if(!p->trans) trans_exist = 0;
    p->prod = merge_trans(prod->nxt->prod, p->trans);
    p->nxt = prod->nxt;
    p->prv = prod;
    p->nxt->prv = p;
    p->prv->nxt = p;
  }

  while(trans_exist) { /* calculates all the transitions */
    AProd *p = prod->nxt;
    t1 = p->prod;
    if(t1) { /* solves the current transition */
      GTrans *trans, *t2;
      clear_set(fin, 0);
      for(i = 1; i < final[0]; i++)
	if(is_final(s->nodes_set, t1, final[i]))
	  add_set(fin, final[i]);
      for(t2 = s->trans->nxt; t2 != s->trans;) {
	if(tl_simp_fly &&
	   included_set(t1->to, t2->to->nodes_set, 0) &&
	   included_set(t1->pos, t2->pos, 1) &&
	   included_set(t1->neg, t2->neg, 1) &&
	   same_sets(fin, t2->final, 0)) { /* t2 is redondant */
	  GTrans *free = t2->nxt;
	  t2->to->incoming--;
	  t2->to = free->to;
	  copy_set(free->pos, t2->pos, 1);
	  copy_set(free->neg, t2->neg, 1);
	  copy_set(free->final, t2->final, 0);
	  t2->nxt   = free->nxt;
	  if(free == s->trans) s->trans = t2;
	  free_gtrans(free, 0, 0);
	  state_trans--;
	}
	else if(tl_simp_fly &&
		included_set(t2->to->nodes_set, t1->to, 0) &&
		included_set(t2->pos, t1->pos, 1) &&
		included_set(t2->neg, t1->neg, 1) &&
		same_sets(t2->final, fin, 0)) {/* t1 is redondant */
	  break;
	}
	else {
	  t2 = t2->nxt;
	}
      }
      if(t2 == s->trans) { /* adds the transition */
	trans = emalloc_gtrans();
	trans->to = find_gstate(t1->to, s);
	trans->to->incoming++;
	copy_set(t1->pos, trans->pos, 1);
	copy_set(t1->neg, trans->neg, 1);
	copy_set(fin,   trans->final, 0);
	trans->nxt = s->trans->nxt;
	s->trans->nxt = trans;
	state_trans++;
      }
    }
    if(!p->trans)
      break;
    while(!p->trans->nxt) /* calculates the next transition */
      p = p->nxt;
    if(p == prod)
      break;
    p->trans = p->trans->nxt;
    do_merge_trans(&(p->prod), p->nxt->prod, p->trans);
    p = p->prv;
    while(p != prod) {
      p->trans = transition[p->astate];
      do_merge_trans(&(p->prod), p->nxt->prod, p->trans);
      p = p->prv;
    }
  }
  
  tfree(list); /* free memory */
  while(prod->nxt != prod) {
    AProd *p = prod->nxt;
    prod->nxt = p->nxt;
    free_atrans(p->prod, 0);
    tfree(p);
  }
  free_atrans(prod->prod, 0);
  tfree(prod);

  if(tl_simp_fly) {
    if(s->trans == s->trans->nxt) { /* s has no transitions */
      free_gtrans(s->trans->nxt, s->trans, 1);
      s->trans = (GTrans *)0;
      s->prv = (GState *)0;
      s->nxt = gremoved->nxt;
      gremoved->nxt = s;
      for(s1 = gremoved->nxt; s1 != gremoved; s1 = s1->nxt)
	if(s1->prv == s)
	s1->prv = (GState *)0;
      return;
    }
    
    gstates->trans = s->trans;
    s1 = gstates->nxt;
    while(!all_gtrans_match(s, s1, 0))
      s1 = s1->nxt;
    if(s1 != gstates) { /* s and s1 are equivalent */
      free_gtrans(s->trans->nxt, s->trans, 1);
      s->trans = (GTrans *)0;
      s->prv = s1;
      s->nxt = gremoved->nxt;
      gremoved->nxt = s;
      for(s1 = gremoved->nxt; s1 != gremoved; s1 = s1->nxt)
	if(s1->prv == s)
	  s1->prv = s->prv;
      return;
    }
  }

  s->nxt = gstates->nxt; /* adds the current state to 'gstates' */
  s->prv = gstates;
  s->nxt->prv = s;
  gstates->nxt = s;
  gtrans_count += state_trans;
  gstate_count++;
}

/********************************************************************\
|*            Display of the generalized Buchi automaton            *|
\********************************************************************/

void reverse_print_generalized(GState *s) /* dumps the generalized Buchi automaton */
{
  GTrans *t;
  if(s == gstates) return;

  reverse_print_generalized(s->nxt); /* begins with the last state */

  fprintf(tl_out, "state %i (", s->id);
  print_set(s->nodes_set, 0);
  fprintf(tl_out, ") : %i\n", s->incoming);
  for(t = s->trans->nxt; t != s->trans; t = t->nxt) {
    if (empty_set(t->pos, 1) && empty_set(t->neg, 1))
      fprintf(tl_out, "1");
    print_set(t->pos, 1);
    if (!empty_set(t->pos, 1) && !empty_set(t->neg, 1)) fprintf(tl_out, " & ");
    print_set(t->neg, 2);
    fprintf(tl_out, " -> %i : ", t->to->id);
    print_set(t->final, 0);
    fprintf(tl_out, "\n");
  }
}

void print_generalized() { /* prints intial states and calls 'reverse_print' */
  int i;
  fprintf(tl_out, "init :\n");
  for(i = 0; i < init_size; i++)
    if(init[i])
      fprintf(tl_out, "%i\n", init[i]->id);
  reverse_print_generalized(gstates->nxt);
}

/********************************************************************\
|*                       Main method                                *|
\********************************************************************/

void mk_generalized() 
{ /* generates a generalized Buchi automaton from the alternating automaton */
  ATrans *t;
  GState *s;
  int i;

  fin = new_set(0);
  bad_scc = new_set(0);
  final = list_set(final_set, 0);

  if(tl_stats) times(&t_debut);

  gstack        = (GState *)tl_emalloc(sizeof(GState)); /* sentinel */
  gstack->nxt   = gstack;
  gremoved      = (GState *)tl_emalloc(sizeof(GState)); /* sentinel */
  gremoved->nxt = gremoved;
  gstates       = (GState *)tl_emalloc(sizeof(GState)); /* sentinel */
  gstates->nxt  = gstates;
  gstates->prv  = gstates;

  for(t = transition[0]; t; t = t->nxt) { /* puts initial states in the stack */
    s = (GState *)tl_emalloc(sizeof(GState));
    s->id = (empty_set(t->to, 0)) ? 0 : gstate_id++;
    s->incoming = 1;
    s->nodes_set = dup_set(t->to, 0);
    s->trans = emalloc_gtrans(); /* sentinel */
    s->trans->nxt = s->trans;
    s->nxt = gstack->nxt;
    gstack->nxt = s;
    init_size++;
  }

  if(init_size) init = (GState **)tl_emalloc(init_size * sizeof(GState *));
  init_size = 0;
  for(s = gstack->nxt; s != gstack; s = s->nxt)
    init[init_size++] = s;

  while(gstack->nxt != gstack) { /* solves all states in the stack until it is empty */
    s = gstack->nxt;
    gstack->nxt = gstack->nxt->nxt;
    if(!s->incoming) {
      free_gstate(s);
      continue;
    }
    make_gtrans(s);
  }

  retarget_all_gtrans();

  if(tl_stats) {
    times(&t_fin);
    fprintf(tl_out, "\nBuilding of the generalized Buchi automaton : %.2fs");
    fprintf(tl_out, "%i states, %i transitions\n", 
	    ((float)(t_fin.tms_utime - t_debut.tms_utime))/100, 
	    gstate_count, gtrans_count);
  }

  tfree(gstack);
  /*for(i = 0; i < node_id; i++) /* frees the data from the alternating automaton */
  /*free_atrans(transition[i], 1);*/
  free_all_atrans();
  tfree(transition);

  if(tl_verbose) {
    fprintf(tl_out, "\nGeneralized Buchi automaton before simplification\n");
    print_generalized();
  }

  if(tl_simp_diff) {
    if (tl_simp_scc) simplify_gscc();
    simplify_gtrans();
    if (tl_simp_scc) simplify_gscc();
    while(simplify_gstates()) { /* simplifies as much as possible */
      if (tl_simp_scc) simplify_gscc();
      simplify_gtrans();
      if (tl_simp_scc) simplify_gscc();
    }
    
    if(tl_verbose) {
      fprintf(tl_out, "\nGeneralized Buchi automaton after simplification\n");
      print_generalized();
    }
  }
}
  
