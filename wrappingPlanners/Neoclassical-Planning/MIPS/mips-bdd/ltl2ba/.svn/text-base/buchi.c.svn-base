/***** ltl2ba : buchi.c *****/

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

extern GState **init, *gstates;
extern struct tms t_debut, t_fin;
extern int tl_verbose, tl_stats, tl_simp_diff, tl_simp_fly, tl_simp_scc,
  tl_preference, init_size, *final;
extern void put_uform(void);

extern FILE *tl_out;	

extern char automata_name[200];

BState *bstack, *bstates, *bremoved;
BScc *scc_stack;
int accept, bstate_count = 0, btrans_count = 0, rank;

/********************************************************************\
|*        Simplification of the generalized Buchi automaton         *|
\********************************************************************/

char* truncate(char* str) {
  int i = 0;
  int j = 0;
  int l = 0;
  while (str[l] != '\0') {
      l++;
  }

  char* ret = tl_emalloc(sizeof(char) * l);
  while (str[i+1] != '\0') {
      if (str[i] == 'p' && str[i+1] == '-')
	  j = i;
      ret[i] = str[i];
      i++;
  }

  i=j;
  while (i<= l-2) {
       ret[i] = str[i+2];
    i++;
  }
  return ret;
}

void free_bstate(BState *s) /* frees a state and its transitions */
{
  free_btrans(s->trans->nxt, s->trans, 1);
  tfree(s);
}

BState *remove_bstate(BState *s, BState *s1) /* removes a state */
{
  BState *prv = s->prv;
  s->prv->nxt = s->nxt;
  s->nxt->prv = s->prv;
  free_btrans(s->trans->nxt, s->trans, 0);
  s->trans = (BTrans *)0;
  s->nxt = bremoved->nxt;
  bremoved->nxt = s;
  s->prv = s1;
  for(s1 = bremoved->nxt; s1 != bremoved; s1 = s1->nxt)
    if(s1->prv == s)
      s1->prv = s->prv;
  return prv;
} 

void copy_btrans(BTrans *from, BTrans *to) {
  to->to    = from->to;
  copy_set(from->pos, to->pos, 1);
  copy_set(from->neg, to->neg, 1);
}

int simplify_btrans() /* simplifies the transitions */
{
  BState *s;
  BTrans *t, *t1;
  int changed = 0;

  if(tl_stats) times(&t_debut);

  for (s = bstates->nxt; s != bstates; s = s->nxt)
    for (t = s->trans->nxt; t != s->trans;) {
      t1 = s->trans->nxt;
      copy_btrans(t, s->trans);
      while((t == t1) || (t->to != t1->to) ||
	    !included_set(t1->pos, t->pos, 1) ||
	    !included_set(t1->neg, t->neg, 1))
	t1 = t1->nxt;
      if(t1 != s->trans) {
	BTrans *free = t->nxt;
	t->to    = free->to;
	copy_set(free->pos, t->pos, 1);
	copy_set(free->neg, t->neg, 1);
	t->nxt   = free->nxt;
	if(free == s->trans) s->trans = t;
	free_btrans(free, 0, 0);
	changed++;
      }
      else
	t = t->nxt;
    }

  if(tl_stats) {
    times(&t_fin);
    fprintf(tl_out, "\nSimplification of the Buchi automaton - transitions: %.2fs");
    fprintf(tl_out, "\n%i transitions removed\n", 
	   ((float)(t_fin.tms_utime - t_debut.tms_utime))/100, changed);

  }
  return changed;
}

int same_btrans(BTrans *s, BTrans *t) /* returns 1 if the transitions are identical */
{
  return((s->to == t->to) &&
	 same_sets(s->pos, t->pos, 1) &&
	 same_sets(s->neg, t->neg, 1));
}

void remove_btrans(BState *to) 
{             /* redirects transitions before removing a state from the automaton */
  BState *s;
  BTrans *t;
  int i;
  for (s = bstates->nxt; s != bstates; s = s->nxt)
    for (t = s->trans->nxt; t != s->trans; t = t->nxt)
      if (t->to == to) { /* transition to a state with no transitions */
	BTrans *free = t->nxt;
	t->to = free->to;
	copy_set(free->pos, t->pos, 1);
	copy_set(free->neg, t->neg, 1);
	t->nxt   = free->nxt;
	if(free == s->trans) s->trans = t;
	free_btrans(free, 0, 0);
      }
}

void retarget_all_btrans()
{             /* redirects transitions before removing a state from the automaton */
  BState *s;
  BTrans *t;
  for (s = bstates->nxt; s != bstates; s = s->nxt)
    for (t = s->trans->nxt; t != s->trans; t = t->nxt)
      if (!t->to->trans) { /* t->to has been removed */
	t->to = t->to->prv;
	if(!t->to) { /* t->to has no transitions */
	  BTrans *free = t->nxt;
	  t->to = free->to;
	  copy_set(free->pos, t->pos, 1);
	  copy_set(free->neg, t->neg, 1);
	  t->nxt   = free->nxt;
	  if(free == s->trans) s->trans = t;
	  free_btrans(free, 0, 0);
	}
      }
  while(bremoved->nxt != bremoved) { /* clean the 'removed' list */
    s = bremoved->nxt;
    bremoved->nxt = bremoved->nxt->nxt;
    tfree(s);
  }
}

int all_btrans_match(BState *a, BState *b) /* decides if the states are equivalent */
{	
  BTrans *s, *t;
  if (((a->final == accept) || (b->final == accept)) &&
      (a->final + b->final != 2 * accept) && 
      a->incoming >=0 && b->incoming >=0)
    return 0; /* the states have to be both final or both non final */

  for (s = a->trans->nxt; s != a->trans; s = s->nxt) { 
                                /* all transitions from a appear in b */
    copy_btrans(s, b->trans);
    t = b->trans->nxt;
    while(!same_btrans(s, t))
      t = t->nxt;
    if(t == b->trans) return 0;
  }
  for (s = b->trans->nxt; s != b->trans; s = s->nxt) { 
                                /* all transitions from b appear in a */
    copy_btrans(s, a->trans);
    t = a->trans->nxt;
    while(!same_btrans(s, t))
      t = t->nxt;
    if(t == a->trans) return 0;
  }
  return 1;
}

int simplify_bstates() /* eliminates redundant states */
{
  BState *s, *s1;
  int changed = 0;

  if(tl_stats) times(&t_debut);

  for (s = bstates->nxt; s != bstates; s = s->nxt) {
    if(s->trans == s->trans->nxt) { /* s has no transitions */
      s = remove_bstate(s, (BState *)0);
      changed++;
      continue;
    }
    bstates->trans = s->trans;
    bstates->final = s->final;
    s1 = s->nxt;
    while(!all_btrans_match(s, s1))
      s1 = s1->nxt;
    if(s1 != bstates) { /* s and s1 are equivalent */
      if(s1->incoming == -1)
	s1->final = s->final; /* get the good final condition */
      s = remove_bstate(s, s1);
      changed++;
    }
  }
  retarget_all_btrans();

  if(tl_stats) {
    times(&t_fin);
    fprintf(tl_out, "\nSimplification of the Buchi automaton - states: %.2fs");
    fprintf(tl_out, "\n%i states removed\n", 
	   ((float)(t_fin.tms_utime - t_debut.tms_utime))/100, changed);
  }

  return changed;
}

int bdfs(BState *s) {
  BTrans *t;
  BScc *c;
  BScc *scc = (BScc *)tl_emalloc(sizeof(BScc));
  scc->bstate = s;
  scc->rank = rank;
  scc->theta = rank++;
  scc->nxt = scc_stack;
  scc_stack = scc;

  s->incoming = 1;

  for (t = s->trans->nxt; t != s->trans; t = t->nxt) {
    if (t->to->incoming == 0) {
      int result = bdfs(t->to);
      scc->theta = min(scc->theta, result);
    }
    else {
      for(c = scc_stack->nxt; c != 0; c = c->nxt)
	if(c->bstate == t->to) {
	  scc->theta = min(scc->theta, c->rank);
	  break;
	}
    }
  }
  if(scc->rank == scc->theta) {
    if(scc_stack == scc) { /* s is alone in a scc */
      s->incoming = -1;
      for (t = s->trans->nxt; t != s->trans; t = t->nxt)
	if (t->to == s)
	  s->incoming = 1;
    }
    scc_stack = scc->nxt;
  }
  return scc->theta;
}

void simplify_bscc() {
  BState *s;
  rank = 1;
  scc_stack = 0;

  if(bstates == bstates->nxt) return;

  for(s = bstates->nxt; s != bstates; s = s->nxt)
    s->incoming = 0; /* state color = white */

  bdfs(bstates->prv);

  for(s = bstates->nxt; s != bstates; s = s->nxt)
    if(s->incoming == 0)
      remove_bstate(s, 0);
}




/********************************************************************\
|*              Generation of the Buchi automaton                   *|
\********************************************************************/

BState *find_bstate(GState **state, int final, BState *s)
{                       /* finds the corresponding state, or creates it */
  if((s->gstate == *state) && (s->final == final)) return s; /* same state */

  s = bstack->nxt; /* in the stack */
  bstack->gstate = *state;
  bstack->final = final;
  while(!(s->gstate == *state) || !(s->final == final))
    s = s->nxt;
  if(s != bstack) return s;

  s = bstates->nxt; /* in the solved states */
  bstates->gstate = *state;
  bstates->final = final;
  while(!(s->gstate == *state) || !(s->final == final))
    s = s->nxt;
  if(s != bstates) return s;

  s = bremoved->nxt; /* in the removed states */
  bremoved->gstate = *state;
  bremoved->final = final;
  while(!(s->gstate == *state) || !(s->final == final))
    s = s->nxt;
  if(s != bremoved) return s;

  s = (BState *)tl_emalloc(sizeof(BState)); /* creates a new state */
  s->gstate = *state;
  s->id = (*state)->id;
  s->incoming = 0;
  s->final = final;
  s->trans = emalloc_btrans(); /* sentinel */
  s->trans->nxt = s->trans;
  s->nxt = bstack->nxt;
  bstack->nxt = s;
  return s;
}

int next_final(int *set, int fin) /* computes the 'final' value */
{
  if((fin != accept) && in_set(set, final[fin + 1]))
    return next_final(set, fin + 1);
  return fin;
}

void make_btrans(BState *s) /* creates all the transitions from a state */
{
  int state_trans = 0;
  GTrans *t;
  BTrans *t1;
  BState *s1;
  if(s->gstate->trans)
    for(t = s->gstate->trans->nxt; t != s->gstate->trans; t = t->nxt) {
      int fin = next_final(t->final, (s->final == accept) ? 0 : s->final);
      BState *to = find_bstate(&t->to, fin, s);
      
      for(t1 = s->trans->nxt; t1 != s->trans;) {
	if(tl_simp_fly && 
	   (to == t1->to) &&
	   included_set(t->pos, t1->pos, 1) &&
	   included_set(t->neg, t1->neg, 1)) { /* t1 is redondant */
	  BTrans *free = t1->nxt;
	  t1->to->incoming--;
	  t1->to = free->to;
	  copy_set(free->pos, t1->pos, 1);
	  copy_set(free->neg, t1->neg, 1);
	  t1->nxt   = free->nxt;
	  if(free == s->trans) s->trans = t1;
	  free_btrans(free, 0, 0);
	  state_trans--;
	}
	else if(tl_simp_fly &&
		(t1->to == to ) &&
		included_set(t1->pos, t->pos, 1) &&
		included_set(t1->neg, t->neg, 1)) /* t is redondant */
	  break;
	else
	  t1 = t1->nxt;
      }
      if(t1 == s->trans) {
	BTrans *trans = emalloc_btrans();
	trans->to = to;
	trans->to->incoming++;
	copy_set(t->pos, trans->pos, 1);
	copy_set(t->neg, trans->neg, 1);
	trans->nxt = s->trans->nxt;
	s->trans->nxt = trans;
	state_trans++;
      }
    }
  
  if(tl_simp_fly) {
    if(s->trans == s->trans->nxt) { /* s has no transitions */
      free_btrans(s->trans->nxt, s->trans, 1);
      s->trans = (BTrans *)0;
      s->prv = (BState *)0;
      s->nxt = bremoved->nxt;
      bremoved->nxt = s;
      for(s1 = bremoved->nxt; s1 != bremoved; s1 = s1->nxt)
	if(s1->prv == s)
	  s1->prv = (BState *)0;
      return;
    }
    bstates->trans = s->trans;
    bstates->final = s->final;
    s1 = bstates->nxt;
    while(!all_btrans_match(s, s1))
      s1 = s1->nxt;
    if(s1 != bstates) { /* s and s1 are equivalent */
      free_btrans(s->trans->nxt, s->trans, 1);
      s->trans = (BTrans *)0;
      s->prv = s1;
      s->nxt = bremoved->nxt;
      bremoved->nxt = s;
      for(s1 = bremoved->nxt; s1 != bremoved; s1 = s1->nxt)
	if(s1->prv == s)
	  s1->prv = s->prv;
      return;
    }
  }
  s->nxt = bstates->nxt; /* adds the current state to 'bstates' */
  s->prv = bstates;
  s->nxt->prv = s;
  bstates->nxt = s;
  btrans_count += state_trans;
  bstate_count++;
}



/********************************************************************\
|*                  Display of the Buchi automaton                  *|
\********************************************************************/

void print_buchi(BState *s) /* dumps the Buchi automaton */
{
  BTrans *t;
  char from[200];
  char to[200];
  if(s == bstates) {
      if (tl_preference) {
	if(!isdurative){

	  fprintf(tl_out, "(:action skip-preference-automaton-%s\n",automata_name);
	  fprintf(tl_out, ":precondition\n");
	  fprintf(tl_out, "\t(and\n");
	  fprintf(tl_out, "\t\t(sync-automaton-%s)\n",automata_name);
	  fprintf(tl_out, "\t)\n");
	  fprintf(tl_out, ":effect\n");
	  fprintf(tl_out, "\t(and \n");
	  fprintf(tl_out, "\t\t(not (sync-automaton-%s))\n",automata_name);
	  fprintf(tl_out, "\t\t(sync-next)\n");
	  fprintf(tl_out, "\t\t(is-violated-%s) \n",truncate(automata_name));
	  fprintf(tl_out, "\t\t(not (is-satisfied-%s)) \n",truncate(automata_name));
	  fprintf(tl_out, "\t\t(dead-%s) \n",truncate(automata_name));
	  fprintf(tl_out, "\t) \n");
	  fprintf(tl_out, ")\n");
 }
        else{
          fprintf(tl_out, "(:durative-action skip-preference-automaton-%s\n",automata_name);
          fprintf(tl_out, ":duration (= ?duration 0 )\n");
	  fprintf(tl_out, ":condition\n");
	  fprintf(tl_out, "\t(and\n");
	  fprintf(tl_out, "\t\t(over all (sync-automaton-%s))\n",automata_name);
	  fprintf(tl_out, "\t)\n");
	  fprintf(tl_out, ":effect\n");
	  fprintf(tl_out, "\t(and \n");
	  fprintf(tl_out, "\t\t(at start (dead-%s)) \n",truncate(automata_name));
	  fprintf(tl_out, "\t\t(at start (not (sync-automaton-%s)))\n",automata_name);
	  fprintf(tl_out, "\t\t(at end (sync-next))\n");
	  fprintf(tl_out, "\t\t(at end (is-violated-%s)) \n",truncate(automata_name));
	  fprintf(tl_out, "\t\t(at end (not (is-satisfied-%s))) \n",truncate(automata_name));
	  fprintf(tl_out, "\t) \n");
	  fprintf(tl_out, ")\n");
	}
      }
      return;
  }

  print_buchi(s->nxt); /* begins with the last state */

  if(s->id == -1) {
    sprintf(from, "%s-init",automata_name);
  }
  else {
    if(s->final == accept) {
      sprintf(from, "%s-accept",automata_name);
    }
    else {
      sprintf(from, "%s-T%i",automata_name,s->final);
    }
    sprintf(from, "%s-%i",from, s->id);
  }

  for(t = s->trans->nxt; t != s->trans; t = t->nxt) {
   if(!isdurative)
      fprintf(tl_out, "(:action sync-trans-%s-",from);
   else
      fprintf(tl_out, "(:durative-action sync-trans-%s-",from);
    if(t->to->id == -1) {
      fprintf(tl_out, "%s-init\n",automata_name);
      sprintf(to, "%s-init",automata_name);
    }
    else {
      if(t->to->final == accept) {
	fprintf(tl_out, "%s-accept",automata_name);
	sprintf(to,"%s-accept",automata_name);
      }
      else {
	fprintf(tl_out, "%s-T%i",automata_name,t->to->final);
	sprintf(to, "%s-T%i",automata_name,t->to->final);
      }
      fprintf(tl_out, "-%i\n", t->to->id);
      sprintf(to, "%s-%i",to, t->to->id);
    }
/*    fprintf(tl_out, ":parameters ()\n"); */
    if(isdurative)
        fprintf(tl_out, ":duration (= ?duration 0 )\n");
    if(!isdurative)
        fprintf(tl_out, ":precondition\n");
    else
        fprintf(tl_out, ":condition\n");
    fprintf(tl_out, "\t(and\n");
    if(!isdurative){
/*	if (tl_preference)
	    fprintf(tl_out, "\t\t(alive-%s)\n",truncate(automata_name));
*/
        fprintf(tl_out, "\t\t(at-%s)\n",from);
        fprintf(tl_out, "\t\t(sync-automaton-%s)\n",automata_name);
        fprintf(tl_out, "\t\t");
        print_set(t->pos, 1);
        if (!empty_set(t->pos, 1) && !empty_set(t->neg, 1)) fprintf(tl_out, "\n");
        print_set(t->neg, 2);
    }
    else{
/*
	if (tl_preference)
	    fprintf(tl_out, "\t\t(at start (alive-%s))\n",truncate(automata_name));
*/
        fprintf(tl_out, "\t\t(at start (at-%s))\n",from);
        fprintf(tl_out, "\t\t(over all (sync-automaton-%s))\n",automata_name);
        fprintf(tl_out, "\t\t");
        print_set(t->pos, 1);
        if (!empty_set(t->pos, 1) && !empty_set(t->neg, 1)) fprintf(tl_out, "\n");
        print_set(t->neg, 2);
	/* for within ltl exp
	**/
	if (s->final != t->to->final) {
            if(t->to->final == accept) {
		if(strstr(automata_name, "within")){
		    char s[30];
                    strcpy(s,give_the_fact_name(automata_name) );
                   if (tl_preference)
                       fprintf(tl_out, "\n\t\t(over all (%s))\n",s);
                   else
                       fprintf(tl_out, "\n\t\t(over all (w%s))\n",s);
	    }
	   }
	 }
    }
    fprintf(tl_out, "\n\t)\n");
    fprintf(tl_out, ":effect\n");
    fprintf(tl_out, "\t(and \n");
    if(!isdurative){
       if (s->final != t->to->final) {
         if(t->to->final == accept) {
	     if (tl_preference) {
		 fprintf(tl_out, "\t\t(not (is-violated-%s)) \n",truncate(automata_name));
		 fprintf(tl_out, "\t\t(is-satisfied-%s) \n",truncate(automata_name));
	     }
	     else 
		 fprintf(tl_out, "\t\t(accepting-%s)\n",automata_name);
         } else {
	     if (tl_preference) {
		 fprintf(tl_out, "\t\t(is-violated-%s) \n",truncate(automata_name));
		 fprintf(tl_out, "\t\t(not (is-satisfied-%s)) \n",truncate(automata_name));
	     }
	     else 
		 fprintf(tl_out, "\t\t(not (accepting-%s))\n",automata_name);
         }
       }
       if (s->id != t->to->id) {
         fprintf(tl_out, "\t\t(not (at-%s))\n",from);
         fprintf(tl_out, "\t\t(at-%s)\n",to);
       }
       fprintf(tl_out, "\t\t(not (sync-automaton-%s))\n",automata_name);
       fprintf(tl_out, "\t\t(sync-next)\n");
       }
     else
        {
       if (s->final != t->to->final) {
         if(t->to->final == accept) {
	     if (tl_preference) {
	     fprintf(tl_out, "\t\t(at end (not (is-violated-%s))) \n",truncate(automata_name));
	     fprintf(tl_out, "\t\t(at end (is-satisfied-%s)) \n",truncate(automata_name));
	     }
	   else 
	       fprintf(tl_out, "\t\t(at start (accepting-%s))\n",automata_name);
         } else {
	     if (tl_preference) {
	       fprintf(tl_out, "\t\t(at end (is-violated-%s)) \n",truncate(automata_name));

	       fprintf(tl_out, "\t\t(at end (not (is-satsified-%s))) \n",truncate(automata_name));
	     }
	   else 
	       fprintf(tl_out, "\t\t(at start (not (accepting-%s)))\n",automata_name);
         }
       }
       if (s->id != t->to->id) {
         fprintf(tl_out, "\t\t(at start (not (at-%s)))\n",from);
         fprintf(tl_out, "\t\t(at start (at-%s))\n",to);
       }
       fprintf(tl_out, "\t\t(at start (not (sync-automaton-%s)))\n",automata_name);
       fprintf(tl_out, "\t\t(at end (sync-next))\n");
       }
    fprintf(tl_out, "\t)\n");
    fprintf(tl_out, ")\n");
  }
}

void print_spin_buchi() {

  BState *s = bstates->prv;
  char from[200];

  fprintf(tl_out, "\n(:init\n");
/*  if (tl_preference)
      fprintf(tl_out, "\t(alive-%s)\n",truncate(automata_name));
*/
  fprintf(tl_out, "\t(at-%s-init)\n",automata_name);
  
  if(s->final == accept) { 
      if (tl_preference)
	  fprintf(tl_out, "\t(is-satisfied-%s)\n",truncate(automata_name));
	 /* fprintf(tl_out, "\t(= (IS_VIOLATED_%s) 0)\n",automata_name); */
    else 
	fprintf(tl_out, "\t(accepting-%s)\n",automata_name);
  }
  else 
      if (tl_preference) {
	  fprintf(tl_out, "\t(is-violated-%s)\n",truncate(automata_name));
      }
  fprintf(tl_out,")\n");

  fprintf(tl_out, "\n(:goal\n"); 
  if (!tl_preference) 
      fprintf(tl_out, "\n\t(accepting-%s)\n)\n",automata_name);
  /*else
      fprintf(tl_out, "\n\t(preference %s)\n)\n",truncate(automata_name));
  */
  fprintf(tl_out, "\n)\n"); 

  fprintf(tl_out, "(:partition \n");
  if (!tl_preference) {
      fprintf(tl_out, "   (:fact-group-acc-%s \n",automata_name);
      fprintf(tl_out, "\t (accepting-%s)\n",automata_name);
      fprintf(tl_out, "\t none-of-those\n");
      fprintf(tl_out, "   ) \n");
  }
/*
  else {
      fprintf(tl_out, "   (:fact-group-alive-%s \n",automata_name);
      fprintf(tl_out, "\t (alive-%s)\n",truncate(automata_name));
      fprintf(tl_out, "\t none-of-those\n");
      fprintf(tl_out, "   ) \n");
  }
*/

/*
  fprintf(tl_out, "(:fact-group-sync-%s \n",automata_name);
  fprintf(tl_out, "\t(sync-automaton-%s)\n",automata_name);
  fprintf(tl_out, "\t none-of-those\n");
  fprintf(tl_out, ") \n");
*/
  fprintf(tl_out, "(:fact-group-state-%s \n",automata_name);
  for(s = bstates->prv; s != bstates; s = s->prv) {
    if(s->id == -1) {
      fprintf(tl_out, "\t (dead-%s)\n",truncate(automata_name));
      sprintf(from, "%s-init",automata_name);
    }
    else {
      if(s->final == accept) {
	sprintf(from, "%s-accept",automata_name);
      }
      else {
	sprintf(from, "%s-T%i",automata_name,s->final);
      }
      sprintf(from, "%s-%i",from, s->id);
    }

    fprintf(tl_out, "\t(at-%s)\n",from);
  }
/*  fprintf(tl_out, "\t none-of-those\n"); */
  fprintf(tl_out, ") \n");

  if (tl_preference) {
      fprintf(tl_out, "(:fact-group-pref-%s \n",automata_name);
      fprintf(tl_out, "\t(is-violated-%s)\n",truncate(automata_name));
      fprintf(tl_out, "\t(is-satisfied-%s)\n",truncate(automata_name));
      fprintf(tl_out, ") \n");
  }
  fprintf(tl_out, ")\n\n");


  fprintf(tl_out, "(:predicates \n");
  if (!tl_preference) {
    fprintf(tl_out, "\t(accepting-%s)\n",automata_name);
  }
  else
      fprintf(tl_out, "\t(dead-%s)\n",truncate(automata_name));

  fprintf(tl_out, "\t(sync-automaton-%s)\n",automata_name);

  for(s = bstates->prv; s != bstates; s = s->prv) {
    if(s->id == -1) {
      sprintf(from, "%s-init",automata_name);
    }
    else {
      if(s->final == accept) {
	sprintf(from, "%s-accept",automata_name);
      }
      else {
	sprintf(from, "%s-T%i",automata_name,s->final);
      }
      sprintf(from, "%s-%i",from, s->id);
    }

    fprintf(tl_out, "\t(at-%s)\n",from);
  }
  if (tl_preference) {
    fprintf(tl_out, "\t(is-violated-%s)\n",truncate(automata_name));
    fprintf(tl_out, "\t(is-satisfied-%s)\n",truncate(automata_name));
  }
  fprintf(tl_out, ")\n",from);

  print_buchi(bstates->nxt); 
  return;
  BTrans *t;
  /*  BState *s; */
  int accept_all = 0, init_count = 0;
  if(bstates->nxt == bstates) { /* empty automaton */
    fprintf(tl_out, "never {    /* ");
    put_uform();
    fprintf(tl_out, " */\n");
    fprintf(tl_out, "T0_init:\n");
    fprintf(tl_out, "\tfalse;\n");
    fprintf(tl_out, "}\n");
    return;
  }
  if(bstates->nxt->nxt == bstates && bstates->nxt->id == 0) { /* true */
    fprintf(tl_out, "never {    /* ");
    put_uform();
    fprintf(tl_out, " */\n");
    fprintf(tl_out, "accept_init:\n");
    fprintf(tl_out, "\tif\n");
    fprintf(tl_out, "\t:: (1) -> goto accept_init\n");
    fprintf(tl_out, "\tfi;\n");
    fprintf(tl_out, "}\n");
    return;
  }

  fprintf(tl_out, "never { /* ");
  put_uform();
  fprintf(tl_out, " */\n");
  for(s = bstates->prv; s != bstates; s = s->prv) {
    if(s->id == 0) { /* accept_all at the end */
      accept_all = 1;
      continue;
    }
    if(s->final == accept)
      fprintf(tl_out, "accept_");
    else fprintf(tl_out, "T%i_", s->final);
    if(s->id == -1)
      fprintf(tl_out, "init:\n");
    else fprintf(tl_out, "S%i:\n", s->id);
    if(s->trans->nxt == s->trans) {
      fprintf(tl_out, "\tfalse;\n");
      continue;
    }
    fprintf(tl_out, "\tif\n");
    for(t = s->trans->nxt; t != s->trans; t = t->nxt) {
      BTrans *t1;
      fprintf(tl_out, "\t:: (");
      spin_print_set(t->pos, t->neg);
      for(t1 = t; t1->nxt != s->trans; )
	if (t1->nxt->to->id == t->to->id &&
	    t1->nxt->to->final == t->to->final) {
	  fprintf(tl_out, ") || (");
	  spin_print_set(t1->nxt->pos, t1->nxt->neg);
	  t1->nxt = t1->nxt->nxt;
	}
	else  t1 = t1->nxt;
      fprintf(tl_out, ") -> goto ");
      if(t->to->final == accept)
	fprintf(tl_out, "accept_");
      else fprintf(tl_out, "T%i_", t->to->final);
      if(t->to->id == 0)
	fprintf(tl_out, "all\n");
      else if(t->to->id == -1)
	fprintf(tl_out, "init\n");
      else fprintf(tl_out, "S%i\n", t->to->id);
    }
    fprintf(tl_out, "\tfi;\n");
  }
  if(accept_all) {
    fprintf(tl_out, "accept_all:\n");
    fprintf(tl_out, "\tskip\n");
  }
  fprintf(tl_out, "}\n");
}

/********************************************************************\
|*                       Main method                                *|
\********************************************************************/

void mk_buchi() 
{/* generates a Buchi automaton from the generalized Buchi automaton */
  int i;
  BState *s = (BState *)tl_emalloc(sizeof(BState));
  GTrans *t;
  BTrans *t1;
  accept = final[0] - 1;
  
  if(tl_stats) times(&t_debut);

  bstack        = (BState *)tl_emalloc(sizeof(BState)); /* sentinel */
  bstack->nxt   = bstack;
  bremoved      = (BState *)tl_emalloc(sizeof(BState)); /* sentinel */
  bremoved->nxt = bremoved;
  bstates       = (BState *)tl_emalloc(sizeof(BState)); /* sentinel */
  bstates->nxt  = s;
  bstates->prv  = s;

  s->nxt        = bstates; /* creates (unique) inital state */
  s->prv        = bstates;
  s->id = -1;
  s->incoming = 1;
  s->final = 0;
  s->gstate = 0;
  s->trans = emalloc_btrans(); /* sentinel */
  s->trans->nxt = s->trans;
  for(i = 0; i < init_size; i++) 
    if(init[i])
      for(t = init[i]->trans->nxt; t != init[i]->trans; t = t->nxt) {
	int fin = next_final(t->final, 0);
	BState *to = find_bstate(&t->to, fin, s);
	for(t1 = s->trans->nxt; t1 != s->trans;) {
	  if(tl_simp_fly && 
	     (to == t1->to) &&
	     included_set(t->pos, t1->pos, 1) &&
	     included_set(t->neg, t1->neg, 1)) { /* t1 is redondant */
	    BTrans *free = t1->nxt;
	    t1->to->incoming--;
	    t1->to = free->to;
	    copy_set(free->pos, t1->pos, 1);
	    copy_set(free->neg, t1->neg, 1);
	    t1->nxt   = free->nxt;
	    if(free == s->trans) s->trans = t1;
	    free_btrans(free, 0, 0);
	  }
	else if(tl_simp_fly &&
		(t1->to == to ) &&
		included_set(t1->pos, t->pos, 1) &&
		included_set(t1->neg, t->neg, 1)) /* t is redondant */
	  break;
	  else
	    t1 = t1->nxt;
	}
	if(t1 == s->trans) {
	  BTrans *trans = emalloc_btrans();
	  trans->to = to;
	  trans->to->incoming++;
	  copy_set(t->pos, trans->pos, 1);
	  copy_set(t->neg, trans->neg, 1);
	  trans->nxt = s->trans->nxt;
	  s->trans->nxt = trans;
	}
      }
  
  while(bstack->nxt != bstack) { /* solves all states in the stack until it is empty */
    s = bstack->nxt;
    bstack->nxt = bstack->nxt->nxt;
    if(!s->incoming) {
      free_bstate(s);
      continue;
    }
    make_btrans(s);
  }

  retarget_all_btrans();

  if(tl_stats) {
    times(&t_fin);
    fprintf(tl_out, "\nBuilding of the Buchi automaton : %.2fs");
    fprintf(tl_out, "\n%i states, %i transitions\n", 
	    ((float)(t_fin.tms_utime - t_debut.tms_utime))/100, 
	    bstate_count, btrans_count);
  }

  if(tl_verbose) {
    fprintf(tl_out, "\nBuchi automaton before simplification\n");
    print_buchi(bstates->nxt);
    if(bstates == bstates->nxt) 
      fprintf(tl_out, "empty automaton, refuses all words\n");  
  }

  if(tl_simp_diff) {
    simplify_btrans();
    if(tl_simp_scc) simplify_bscc();
    while(simplify_bstates()) { /* simplifies as much as possible */
      simplify_btrans();
      if(tl_simp_scc) simplify_bscc();
    }
    
    if(tl_verbose) {
      fprintf(tl_out, "\nBuchi automaton after simplification\n");
      print_buchi(bstates->nxt);
      if(bstates == bstates->nxt) 
	fprintf(tl_out, "empty automaton, refuses all words\n");
      fprintf(tl_out, "\n");
    }
  }

  print_spin_buchi();
}
