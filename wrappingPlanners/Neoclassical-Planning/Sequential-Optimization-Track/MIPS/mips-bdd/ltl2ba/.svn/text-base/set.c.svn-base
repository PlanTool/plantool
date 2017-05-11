/***** ltl2ba : set.c *****/

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

extern FILE *tl_out;
extern int node_size, sym_size;
extern char **sym_table;

int mod = 8 * sizeof(int);

/* sym = 1 for symbol sets, 0 for nodes sets */

int *new_set(int sym) /* creates a new set */
{
  return (int *)tl_emalloc((sym?sym_size:node_size) * sizeof(int));
}

int *clear_set(int *l, int sym) /* clears the set */
{
  int i;
  for(i = 0; i < (sym?sym_size:node_size); i++) {
    l[i] = 0;
  }
  return l;
}

int *make_set(int n, int sym) /* creates the set {n}, or the empty set if n = -1 */
{
  int *l = clear_set(new_set(sym), sym);
  if(n == -1) return l;
  l[n/mod] = 1 << (n%mod);
  return l;
}

void copy_set(int *from, int *to, int sym) /* copies a set */
{
  int i;
  for(i = 0; i < (sym?sym_size:node_size); i++)
    to[i] = from[i];
}

int *dup_set(int *l, int sym) /* duplicates a set */
{
  int i, *m = new_set(sym);
  for(i = 0; i < (sym?sym_size:node_size); i++)
    m[i] = l[i];
  return m;
}
  
void merge_sets(int *l1, int *l2, int sym) /* puts the union of the two sets in l1 */
{
  int i;
  for(i = 0; i < (sym?sym_size:node_size); i++)
    l1[i] = l1[i] | l2[i];
}

void do_merge_sets(int *l, int *l1, int *l2, int sym) /* makes the union of two sets */
{
  int i;
  for(i = 0; i < (sym?sym_size:node_size); i++)
    l[i] = l1[i] | l2[i];
}

int *intersect_sets(int *l1, int *l2, int sym) /* makes the intersection of two sets */
{
  int i, *l = new_set(sym);
  for(i = 0; i < (sym?sym_size:node_size); i++)
    l[i] = l1[i] & l2[i];
  return l;
}

int empty_intersect_sets(int *l1, int *l2, int sym) /* tests intersection of two sets */
{
  int i, test = 0;
  for(i = 0; i < (sym?sym_size:node_size); i++)
    test |= l1[i] & l2[i];
  return !test;
}

void add_set(int *l, int n) /* adds an element to a set */
{
  l[n/mod] |= 1 << (n%mod);
}

void rem_set(int *l, int n) /* removes an element from a set */
{
  l[n/mod] &= (-1 - (1 << (n%mod)));
}

void spin_print_set(int *pos, int *neg) /* prints the content of a set for spin */
{
  int i, j, start = 1;
  for(i = 0; i < sym_size; i++) 
    for(j = 0; j < mod; j++) {
      if(pos[i] & (1 << j)) {
	if(!start)
	  fprintf(tl_out, " && ");
	fprintf(tl_out, "%s", sym_table[mod * i + j]);
	start = 0;
      }
      if(neg[i] & (1 << j)) {
	if(!start)
	  fprintf(tl_out, " && ");
	fprintf(tl_out, "!%s", sym_table[mod * i + j]);
	start = 0;
      }
    }
  if(start)
    fprintf(tl_out, "1");
}


char* exchange(char* str) {
  int i = 0;
  while (str[i] != '\0') {
       if (str[i] == '_') 
	 str[i] = '-';
/*
       if ('a' <= str[i] && str[i] < 'z'){
	   str[i] = str[i] - 'a' + 'A';
       }
*/
    i++;
  }
/*
    if ( str[0] == 'A' && str[1] == 'T') {
      str[0] = 'a';  str[1] = 't';
    }
*/

  return str;
}



char *give_the_fact_name(char* str) {

  int i = 0;
  int j = 0;
  char *t, *t1;
  char *str2;

  while (str[i] != '\0') {
      if(i>1)
          break;
      i++;
  }

   while (str[i]!='w'){
      str2[j++] = str[i++];
   }
   str2[j] = '\0';

     t = strchr(str,'f');

 

     sprintf(t1,"%s%s",str2,t );
    

  return t1;
}





char *give_the_pref_automaton_name(char* str) {

  int i = 0;
  int j = 0;
  char *str2, *t;


   while (str[i] != '\0') {
      if(i>1)
          break;
      i++;
  }

   while (str[i]!='-'){
      str2[j++] = str[i++];
   }
   str2[j] = '\0';

  sprintf(t,"%s",str2 );

  return t;
}






void print_set(int *l, int sym) /* prints the content of a set */
{
  int i, j, start = 1;;
  if(!sym) fprintf(tl_out, "{");
  for(i = 0; i < (sym?sym_size:node_size); i++) 
    for(j = 0; j < mod; j++)
      if(l[i] & (1 << j)) {
	switch(sym) {
	case 0:
	  if(!start)
	    fprintf(tl_out, ",");
	  fprintf(tl_out, "%i", mod * i + j);
	  break;
	case 1:
	  if(!start)
	    fprintf(tl_out, " ");
           if(!isdurative)
                fprintf(tl_out, "(%s)", exchange(sym_table[mod * i + j]));
           else
	        fprintf(tl_out, "(over all (%s))", exchange(sym_table[mod * i + j]));
	  break;
	case 2:
	  if(!start)
	    fprintf(tl_out, " ");
          if(!isdurative)
	    fprintf(tl_out, "(not (%s))", exchange(sym_table[mod * i + j]));
	  else
             fprintf(tl_out, "(over all (not (%s)))", exchange(sym_table[mod * i + j]));
	}
	start = 0;
      }
  if(!sym) {
    fprintf(tl_out, "}");
  }
}

int empty_set(int *l, int sym) /* tests if a set is the empty set */
{
  int i, test = 0;
  for(i = 0; i < (sym?sym_size:node_size); i++)
    test |= l[i];
  return !test;
}

int same_sets(int *l1, int *l2, int sym) /* tests if two sets are identical */
{
  int i, test = 1;
  for(i = 0; i < (sym?sym_size:node_size); i++)
    test &= (l1[i] == l2[i]);
  return test;
}

int included_set(int *l1, int *l2, int sym) 
{                    /* tests if the first set is included in the second one */
  int i, test = 0;
  for(i = 0; i < (sym?sym_size:node_size); i++)
    test |= (l1[i] & ~l2[i]);
  return !test;
}

int in_set(int *l, int n) /* tests if an element is in a set */
{
  return(l[n/mod] & (1 << (n%mod)));
}

int *list_set(int *l, int sym) /* transforms a set into a list */
{
  int i, j, size = 1, *list;
  for(i = 0; i < (sym?sym_size:node_size); i++)
    for(j = 0; j < mod; j++) 
      if(l[i] & (1 << j))
	size++;
  list = (int *)tl_emalloc(size * sizeof(int));
  list[0] = size;
  size = 1;
  for(i = 0; i < (sym?sym_size:node_size); i++)
    for(j = 0; j < mod; j++) 
      if(l[i] & (1 << j))
	list[size++] = mod * i + j;
  return list;
}
  
