
/*
 *  CPlan solves planning problems formulated as constraint satisfaction
 *  problems.
 *
 *  Copyright (C) 1999  Peter van Beek and Xinguang Chen
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation.  See the GNU General Public License
 *  for more details (see the file called Copying or contact the Free
 *  Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA).
 */

#include "gp_csp.h"
#include "../graphplan.h"
#include "../Share/global.h"
#include "../Share/csp.h"
#include "../Share/ebl.h"
#include "../justify.h"

int printed_value[50][20];
int number_printed[50];

extern vertex_t list_action[];
extern int nogood_stat[];
extern int check_nogood_stat[];
extern int irrelevant_nogood;

void
initialize_print()
{
  int i;

  for( i = 0; i < 50; i++ )
    number_printed[i] = 0;
}

void
printed(time, number)
     int time;
     int number;
{
  printed_value[time][number_printed[time]] = number;
  number_printed[time]++;
}

int
is_printed(time, number)
     int time;
     int number;
{
  int i;
  for(i = 0; i < number_printed[time]; i++)
    if( number == printed_value[time][i] )
      return 1;

  return 0;
}

void
pretty_print( name , time, index)
     char *name;
     int time;
     int index;
{
  int flag = 0, i;
  
  printf("%d.%d ", time, index);
  for( i = 0; i < strlen(name); i++ ) {
    if( name[i] != '_' )
      printf("%c", name[i]);
    else {
      if( flag == 0 ) {
	printf("%c ", '(');
	flag = 1;
      }
      else
	printf("%c ", ',');
    }
  }
  printf(" )\n");
}

void
process_solution( C, solution )
    CSP_type    *C;
    int		*solution;
{
    int		t;
    int		v1, count = 1;
    VALA	a1;
    vertex_t    vert;


    printf("\n------------------------------------------------------\n");
    printf("          Solution by GAC_CBJ_EBL solver");
    printf("\n------------------------------------------------------\n");

    verify_solution( C, solution );
    initialize_print();

    for( t = 1; t <= C->time; t++ ) {
	get_next(fact_table[t], 0);
        while( (vert = get_next(fact_table[t], 1)) != NULL) {
	  if( vert->needed == 0 )
	    continue;
	  v1 = vert->csp_val;
	  a1 = C->value_attribute[v1][solution[v1]];

	  if( (a1.type == IS_ACTION) && (!is_printed(t, a1.number))) {
	    /*	    printf("  %6d   ", list_action[a1.number]->csp_val); */
	    list_action[a1.number]->used = 1;
	    pretty_print( list_action[a1.number]->name , t, count);
	    count++;
	    printed(t, a1.number);
	  }

	  if( a1.type == A_NOOP )
	    list_action[a1.number]->used = 1;
        }
    }

    /* Justify rountine from Blackbox */
    printf("\n------------------------------------------------------\n");
    printf("          Solution after justify");
    printf("\n------------------------------------------------------\n");
    /* BM: Justify the final plan */
    justify_plan( C->time );
    count = 1;
    for( t=0; t < C->time; t++ ) {
      get_next(op_table[t], 0);
      while( (vert = get_next(op_table[t], 1)) != NULL ) {
	if( (vert->used == 0) || IS_NOOP(vert) )
	  continue;

	pretty_print( vert->name, t+1, count );
	count++;
      }
    }
    printf("--------------------------------------------------------\n");

}

