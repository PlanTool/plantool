

/*********************************************************************
 * (C) Copyright 1999 Albert Ludwigs University Freiburg
 *     Institute of Computer Science
 *
 * All rights reserved. Use of this software is permitted for 
 * non-commercial research purposes, and it may be copied only 
 * for that use.  All copies must include this copyright message.
 * This software is made available AS IS, and neither the authors
 * nor the  Albert Ludwigs University Freiburg make any warranty
 * about the software or its performance. 
 *********************************************************************/







/*********************************************************************
 * File: instantiateI.c
 * Description: functions for instantiating operators, first part.
 *              - transform domain into integers
 *              - domain cleanup functions
 *              - inertia preprocessing:
 *                  - collect inertia info
 *                  - split initial state in special arrays
 *                  - encode unary inertia as types
 *
 * Author: Joerg Hoffmann 1999
 *
 *********************************************************************/ 








#include <string.h>
#include "bb.h"

#include "output.h"
#include "memory.h"

#include "instantiateI.h"











/*******************************************************
 * TRANSFORM DOMAIN INTO INTEGER (FACT) REPRESENTATION *
 *******************************************************/










void encode_domain_in_integers( void )

{

  int i,j;

  collect_all_strings();

  if ( gcmd_line.display_info == 102 ) {
    printf("\nconstant table:");
    for ( i = 0; i < gnum_constants; i++ ) {
      printf("\n%d --> %s", i, gconstants[i]);
    }

    printf("\n\ntypes table:");
    for ( i = 0; i < gnum_types; i++ ) {
      printf("\n%d --> %s: ", i, gtype_names[i]);
      for ( j = 0; j < gtype_size[i]; j++ ) {
	printf("%d ", gtype_consts[i][j]);
      }
    }

    printf("\n\npredicates table:");
    for ( i = 0; i < gnum_predicates; i++ ) {
      printf("\n%3d --> %s: ", i, gpredicates[i]);
      for ( j = 0; j < garity[i]; j++ ) {
	printf("%s ", gtype_names[gpredicates_args_type[i][j]]);
      }
    }
    printf("\n\n");
  }


  create_integer_representation();

  cleanup_domain();

  if ( gcmd_line.display_info == 103 ) {
    printf("\n\ncoded initial state is:");
    for ( i = 0; i < gnum_full_initial; i++ ) {
      printf("\n");
      print_Fact( &(gfull_initial[i]) );
    }

    printf("\n\ncoded goal state is:");
    for ( i = 0; i < gnum_goal; i++ ) {
      printf("\n");
      print_Fact( &(ggoal[i]) );
    }

    printf("\n\ncoded operators are:");
    for ( i = 0; i < gnum_operators; i++ ) {
      print_Operator( goperators[i] );
    }
    printf("\n\n");
  }

}



void collect_all_strings( void )

{

  FactList *f;
  TokenList *t;
  int p_num, type_num, c_num, ar;
  int i;

  for ( f = gorig_constant_list; f; f = f->next ) {
    if ( (type_num = position_in_types_table( f->item->next->item )) == -1 ) {
      if ( gnum_types == MAX_TYPES ) {
	printf("\ntoo many types! increase MAX_TYPES (currently %d)\n\n",
	       MAX_TYPES);
	exit( 1 );
      }
      gtype_names[gnum_types] = new_Token( strlen( f->item->next->item ) + 1 );
      strcpy( gtype_names[gnum_types], f->item->next->item );
      gtype_size[gnum_types] = 0;
      for ( i = 0; i < MAX_CONSTANTS; i++ ) {
	gis_member[i][gnum_types] = FALSE;
      }
      type_num = gnum_types++;
    }

    if ( (c_num = position_in_constants_table( f->item->item )) == -1 ) {
      if ( gnum_constants == MAX_CONSTANTS ) {
	printf("\ntoo many constants! increase MAX_CONSTANTS (currently %d)\n\n",
	       MAX_CONSTANTS);
	exit( 1 );
      }
      gconstants[gnum_constants] = new_Token( strlen( f->item->item ) + 1 );
      strcpy( gconstants[gnum_constants], f->item->item );
      c_num = gnum_constants++;
    }
    
    if ( !gis_member[c_num][type_num] ) {
      if ( gtype_size[type_num] == MAX_TYPE ) {
	printf("\ntoo many consts in type %s! increase MAX_TYPE (currently %d)\n\n",
	       gtype_names[type_num], MAX_TYPE);
	exit( 1 );
      }     
      gtype_consts[type_num][gtype_size[type_num]++] = c_num;
      gis_member[c_num][type_num] = TRUE;
    }
  }

  for ( f = gpredicates_and_types; f; f = f->next ) {
    if ( (p_num = position_in_predicates_table( f->item->item )) != -1 ) {
      printf("\npredicate %s declared twice!\n\n", f->item->item);
      exit( 1 );
    }
    if ( gnum_predicates == MAX_PREDICATES ) {
      printf("\ntoo many predicates! increase MAX_PREDICATES (currently %d)\n\n",
	     MAX_PREDICATES);
      exit( 1 );
    }
    gpredicates[gnum_predicates] = new_Token( strlen( f->item->item ) + 1 );
    strcpy( gpredicates[gnum_predicates], f->item->item );
    ar = 0;
    for ( t = f->item->next; t; t = t->next ) {
      if ( (type_num = position_in_types_table( t->item )) == -1 ) {
	printf("\nwarning: predicate %s uses unknown or empty type %s\n\n", 
	       f->item->item, t->item);
      }
      if ( ar == MAX_ARITY ) {
	printf("\narity of %s to high! increase MAX_ARITY (currently %d)\n\n",
	       gpredicates[gnum_predicates], MAX_ARITY);
	exit( 1 );
      }
      gpredicates_args_type[gnum_predicates][ar++] = type_num;
    }
    garity[gnum_predicates++] = ar;
  }

  free_FactList( gorig_constant_list );
  free_FactList( gpredicates_and_types );
  free_FactList( gtypes );

}



int position_in_types_table( char *str )

{

  int i;

  for ( i=0; i<gnum_types; i++ ) {
    if ( str == gtype_names[i] || 
	 (strcmp( str, gtype_names[i] ) == SAME) ) {
      break;
    }
  }

  return ( i == gnum_types ) ? -1 : i;

}



int position_in_constants_table( char *str )

{

  int i;

  for ( i=0; i<gnum_constants; i++ ) {
    if ( str == gconstants[i] || 
	 strcmp( str, gconstants[i] ) == SAME ) {
      break;
    }
  }

  return ( i == gnum_constants ) ? -1 : i;

}



int position_in_predicates_table( char *str )

{

  int i;

  for ( i=0; i<gnum_predicates; i++ ) {
    if ( str == gpredicates[i] || 
	 strcmp( str, gpredicates[i] ) == SAME ) {
      break;
    }
  }

  return ( i == gnum_predicates ) ? -1 : i;

}



void create_integer_representation( void )

{

  PlNode *n;
  PlOperator *o;
  Operator *tmp;
  FactList *ff;
  int type_num, i;

  if ( gorig_initial_facts ) {
    for ( n = gorig_initial_facts->sons; n; n = n->next ) {
      if ( gnum_full_initial == MAX_INITIAL ) {
	printf("\ntoo many initial facts! increase MAX_INITIAL (currently %d)\n\n",
	       MAX_INITIAL);
	exit( 1 );
      }
      make_Fact( &(gfull_initial[gnum_full_initial]), n, NULL );
      if ( gfull_initial[gnum_full_initial].predicate == -1 ) {
	printf("\nequality in initial state! check input files.\n\n");
	exit( 1 );
      }
      gnum_full_initial++;
    }
    free_PlNode( gorig_initial_facts );
  }

  if ( gorig_goal_facts ) {
    for ( n = gorig_goal_facts->sons; n; n = n->next ) {
      if ( gnum_goal == MAX_STATE ) {
	printf("\ntoo many goal facts! increase MAX_STATE (currently %d)\n\n",
	       MAX_STATE);
	exit( 1 );
      }
      make_Fact( &(ggoal[gnum_goal]), n, NULL );
      if ( ggoal[gnum_goal].predicate == -1 ) {
	printf("\nequality in goal state! check input files.\n\n");
	exit( 1 );
      }
      gnum_goal++;
    }
    free_PlNode( gorig_goal_facts );
  }

  for ( i = 0; i < MAX_TYPES; i++ ) {
    gpredicate_to_type[i] = -1;
    gnum_intersected_types[i] = -1;
  }

  for ( o = gloaded_ops; o; o = o->next ) {
    tmp = new_Operator( o->name, o->number_of_real_params );

    for ( ff = o->params; ff; ff = ff->next ) {
      if ( (type_num = position_in_types_table( ff->item->next->item )) == -1 ) {
	printf("\nwarning: parameter %s of op %s has unknown or empty type. skipping op",
	       ff->item->item, ff->item->next->item);
	break;
      }
      if ( tmp->num_vars == MAX_VARS ) {
	printf("\ntoo many parameters! increase MAX_VARS (currently %d)\n\n",
	       MAX_VARS);
	exit( 1 );
      }
      tmp->var_names[tmp->num_vars] = ff->item->item; 
      tmp->var_types[tmp->num_vars++] = type_num;
    }
    if ( ff ) {
      free_Operator( tmp );
      continue;
    }

    if ( o->preconds ) {
      for ( n = o->preconds->sons; n; n = n->next ) {
	if ( tmp->num_preconds == MAX_OP_P ) {
	  printf("\ntoo many preconds! increase MAX_OP_P (currently %d)\n\n",
		 MAX_OP_P);
	  exit( 1 );
	}
	make_Fact( &((tmp->preconds)[tmp->num_preconds]), n, tmp );
	tmp->num_preconds++;
      }
    }

    if ( o->effects ) {
      for ( n = o->effects->sons; n; n = n->next ) {
	if ( n->connective == ATOM ) {
	  if ( tmp->num_adds == MAX_OP_A ) {
	    printf("\ntoo many added facts! increase MAX_OP_A (currently %d)\n\n",
		   MAX_OP_A);
	    exit( 1 );
	  }
	  make_Fact( &((tmp->adds)[tmp->num_adds]), n, tmp );
	  if ( (tmp->adds)[tmp->num_adds].predicate == -1 ) {
	    printf("\nequality in effect of op %s! check input files.\n\n",
		   o->name);
	    exit( 1 );
	  }
	  tmp->num_adds++;
	} else {/* n->connective == NOT */
	  if ( tmp->num_dels == MAX_OP_D ) {
	    printf("\ntoo many deleted facts! increase MAX_OP_D (currently %d)\n\n",
		   MAX_OP_D);
	    exit( 1 );
	  }
	  make_Fact( &((tmp->dels)[tmp->num_dels]), n->sons, tmp );
	  if ( (tmp->dels)[tmp->num_dels].predicate == -1 ) {
	    printf("\nequality in effect of op %s! check input files.\n\n",
		   o->name);
	    exit( 1 );
	  }
	  tmp->num_dels++;
	}
      }
    }

    if ( gnum_operators == MAX_OPERATORS ) {
      printf("\ntoo many operators! increase MAX_OPERATORS (currently %d)\n\n",
	     MAX_OPERATORS);
      exit( 1 );
    }
    goperators[gnum_operators++] = tmp;
  }

  free_PlOperator( gloaded_ops );

}



void make_Fact( Fact *f, PlNode *n, Operator *o )

{

  int m, i;
  TokenList *t;

  if ( !n->atom ) {
    printf("\nillegal (empty) atom used in domain. check input files\n\n");
    exit( 1 );
  }

  if ( strcmp( n->atom->item, EQ_STR ) == SAME ) {
    f->predicate = -1;
  } else {
    f->predicate = position_in_predicates_table( n->atom->item );
    if ( f->predicate == -1 ) {
      printf("\nundeclared predicate %s used in domain definition\n\n",
	     n->atom->item);
      exit( 1 );
    }
  }

  m = 0;
  for ( t = n->atom->next; t; t = t->next ) {
    if ( t->item[0] == '?' ) {
      if ( !o ) {
	printf("\natom in initial or goal state uses variable\n\n");
	exit( 1 );
      }
      for ( i=0; i<o->num_vars; i++ ) {
	if ( o->var_names[i] == t->item ||
	     strcmp( o->var_names[i], t->item ) == SAME ) {
	  break;
	}
      }
      if ( i == o->num_vars ) {
	printf("\nunknown variable %s in literal %s (op %s). check input files\n\n",
	       t->item, n->atom->item, o->name);
	exit( 1 );
      }
      if ( f->predicate != -1 &&
	   o->var_types[i] != gpredicates_args_type[f->predicate][m] &&
	   !is_subtype( o->var_types[i], gpredicates_args_type[f->predicate][m] ) ) {
	printf("\ntype of var %s of op %s doesnt match type of arg %d of predicate %s\n\n",
	       o->var_names[i], o->name, m, gpredicates[f->predicate]);
	exit( 1 );
      }
      f->args[m] = ENCODE_VAR( i );
    } else {
      if ( (f->args[m] = 
	    position_in_constants_table( t->item )) == -1 ) {
	printf("\nunknown constant %s in literal %s. check input files\n\n",
	       t->item, n->atom->item);
	exit( 1 );
      }
    }
    m++;
  }
  if ( f->predicate == -1 ) {
    if ( m != 2 ) {
      printf("\nfound eq - predicate with %d arguments. check input files\n\n",
	     m);
      exit( 1 );
    }
  } else {
    if ( m != garity[f->predicate] ) {
      printf("\npredicate %s is declared to have %d arguments. check input files\n\n",
	     gpredicates[f->predicate],
	     garity[f->predicate]);
      exit( 1 );
    }
  }

}



Bool is_subtype( int t1, int t2 )

{

  int i;

  for ( i = 0; i < gtype_size[t1]; i++ ) {
    if ( !gis_member[gtype_consts[t1][i]][t2] ) {
      return FALSE;
    }
  }

  return TRUE;

}



void cleanup_domain( void )

{

  /* for the time being, 
   *
   *     - removes ops that have no effects
   *     - removes params that are equality constrained
   *       ( replace them by first param )
   *     - removes parameters that are not used
   */

  Operator *o;
  int i, i1, i2, i3, sw;


  /* mark ops that do not have any effects
   */
  for ( i = 0; i < gnum_operators; i++ ) {
    if ( goperators[i]->num_adds + goperators[i]->num_dels == 0 ) {
      printf("\nwarning: op %s has no effects. skipping it.",
	     goperators[i]->name);
      goperators[i]->out = TRUE;
    }
  }


  /* remove equality constraints
   */
  for ( i = 0; i < gnum_operators; i++ ) {
    o = goperators[i];
    i1 = 0;
    while ( i1 < o->num_preconds ) {
      if ( o->preconds[i1].predicate == -1 ) {
	printf("\nwarning: found equality in precondition.");
	/* was hacker style before, relying on true boolean value coming as
	 * integer one in C; making that explicit now.
	 */
	sw = 0;
	if ( o->preconds[i1].args[0] < 0 ) {
	  sw++;
	}
	if ( o->preconds[i1].args[1] < 0 ) {
	  sw++;
	}
	switch ( sw ) {
	case 2:
	  if ( o->preconds[i1].args[0] == o->preconds[i1].args[1] ) {
	    printf("\n         ... identical parameters.");
	    break;
	  }
	  /* replace higher with lower --> higher gets removed in next cleanup step!
	   */
	  printf("\n         ... replacing upper parameter with lower parameter.");
	  replace_var_entries( o, o->preconds[i1].args[0], o->preconds[i1].args[1] );
	  break;
	case 1:
	  /* replace param with constant
	   */
	  printf("\n         ... replacing parameter with constant.");
	  replace_var_entries( o, o->preconds[i1].args[0], o->preconds[i1].args[1] );
	  break;
	case 0:
	  /* two consts; different --> op out, same --> nothing to do
	   */
	  if ( o->preconds[i1].args[0] != o->preconds[i1].args[1] ) {
	    printf("\n         ... different constants! removing op %s.",
		   o->name);
	    o->out = TRUE;
	  } else {
	    printf("\n         ... identical constants.");
	  }    
	  break;
	}
	/* equality precond gets removed anyway.
	 */
	printf("\n         ... removing equality precondition.");
	for ( i2 = i1; i2 < o->num_preconds-1; i2++ ) {
	  o->preconds[i2].predicate = o->preconds[i2+1].predicate;
	  for ( i3 = 0; i3 < garity[o->preconds[i2].predicate]; i3++ ) {
	    o->preconds[i2].args[i3] = o->preconds[i2+1].args[i3];
	  }
	}
	o->num_preconds--;
      } else {
	i1++;
      }
    }
  }


  /* remove ops that are marked as out
   */
  i = 0;
  while ( i < gnum_operators ) {
    if ( goperators[i]->out ) {
      free_Operator( goperators[i] );
      for ( i1 = i; i1 < gnum_operators-1; i1++ ) {
	goperators[i1] = goperators[i1+1];
      }
      gnum_operators--;
    } else {
      i++;
    }
  }


  /* remove doubly occuring facts (preconds can be defined or resulting from
   * equality preprocessing, effects are always defined so)
   */
  for ( i = 0 ; i <gnum_operators; i++ ) {
    remove_identical_preconds_and_effects( goperators[i] );
  }


  /* remove unused parameters; separated from rest of cleanup
   * because those can result from encoding unary inertia as
   * types; the rest is done once and for all
   */
  remove_unused_parameters();

}



void replace_var_entries( Operator *o, int p0, int p1 )

{

  int i, j, pIN, pOUT;

  if ( p0 >=0 || p1 >= 0 ) {/* one parameter, one constant */

    pIN = p0 >= 0 ? p0 : p1;
    pOUT = p0 >= 0 ? p1 : p0;

  } else {/* two parameters */    

    if ( p0 == p1 ) {/* identical */
      return;
    }

    /* lower parameter stays in; value is > due to encoding of variables
     */
    pIN = p0 > p1 ? p0 : p1;
    pOUT = p0 > p1 ? p1 : p0;

  }
  
  for ( i = 0; i < o->num_preconds; i++ ) {
    for ( j = 0; j < garity[o->preconds[i].predicate]; j++ ) {
      if ( o->preconds[i].args[j] == pOUT ) {
	o->preconds[i].args[j] = pIN;
      }
    }
  }
  for ( i = 0; i < o->num_adds; i++ ) {
    for ( j = 0; j < garity[o->adds[i].predicate]; j++ ) {
      if ( o->adds[i].args[j] == pOUT ) {
	o->adds[i].args[j] = pIN;
      }
    }
  }
  for ( i = 0; i < o->num_dels; i++ ) {
    for ( j = 0; j < garity[o->dels[i].predicate]; j++ ) {
      if ( o->dels[i].args[j] == pOUT ) {
	o->dels[i].args[j] = pIN;
      }
    }
  }

}



void remove_identical_preconds_and_effects( Operator *o )

{

  int i, j, k, l;

  i = 0;
  while ( i < o->num_preconds-1 ) {
    j = i+1;
    while ( j < o->num_preconds ) {
      if ( identical_fact( &(o->preconds[i]), &(o->preconds[j]) ) ) {
	for ( k = j; k < o->num_preconds-1; k++ ) {
	  o->preconds[k].predicate = o->preconds[k+1].predicate;
	  for ( l = 0; l < garity[o->preconds[k].predicate]; l++ ) {
	    o->preconds[k].args[l] = o->preconds[k+1].args[l];
	  }
	}
	o->num_preconds--;
      } else {
	j++;
      }
    }
    i++;
  }   

  i = 0;
  while ( i < o->num_adds-1 ) {
    j = i+1;
    while ( j < o->num_adds ) {
      if ( identical_fact( &(o->adds[i]), &(o->adds[j]) ) ) {
	for ( k = j; k < o->num_adds-1; k++ ) {
	  o->adds[k].predicate = o->adds[k+1].predicate;
	  for ( l = 0; l < garity[o->adds[k].predicate]; l++ ) {
	    o->adds[k].args[l] = o->adds[k+1].args[l];
	  }
	}
	o->num_adds--;
      } else {
	j++;
      }
    }
    i++;
  }   

  i = 0;
  while ( i < o->num_dels-1 ) {
    j = i+1;
    while ( j < o->num_dels ) {
      if ( identical_fact( &(o->dels[i]), &(o->dels[j]) ) ) {
	for ( k = j; k < o->num_dels-1; k++ ) {
	  o->dels[k].predicate = o->dels[k+1].predicate;
	  for ( l = 0; l < garity[o->dels[k].predicate]; l++ ) {
	    o->dels[k].args[l] = o->dels[k+1].args[l];
	  }
	}
	o->num_dels--;
      } else {
	j++;
      }
    }
    i++;
  }   

}



Bool identical_fact( Fact *f1, Fact *f2 )

{

  int i;

  if ( f1->predicate != f2->predicate ) {
    return FALSE;
  }

  for ( i = 0; i < garity[f1->predicate]; i++ ) {
    if ( f1->args[i] != f2->args[i] ) {
      return FALSE;
    }
  }

  return TRUE;

} 



void remove_unused_parameters( void )

{

  Operator *o;
  Bool used[MAX_VARS];
  int i, i1, i2, i3;

  for ( i = 0; i < gnum_operators; i++ ) {
    o = goperators[i];
    for ( i1 = 0; i1 < MAX_VARS; i1++ ) {
      used[i1] = FALSE;
    }
    
    for ( i1 = 0; i1 < o->num_preconds; i1++ ) {
      for ( i2 = 0; i2 < garity[o->preconds[i1].predicate]; i2++ ) {
	if ( o->preconds[i1].args[i2] < 0 ) {
	  used[DECODE_VAR( o->preconds[i1].args[i2] )] = TRUE;
	}
      }
    }
    for ( i1 = 0; i1 < o->num_adds; i1++ ) {
      for ( i2 = 0; i2 < garity[o->adds[i1].predicate]; i2++ ) {
	if ( o->adds[i1].args[i2] < 0 ) {
	  used[DECODE_VAR( o->adds[i1].args[i2])] = TRUE;
	}
      }
    }
    for ( i1 = 0; i1 < o->num_dels; i1++ ) {
      for ( i2 = 0; i2 < garity[o->dels[i1].predicate]; i2++ ) {
	if ( o->dels[i1].args[i2] < 0 ) {
	  used[DECODE_VAR( o->dels[i1].args[i2])] = TRUE;
	}
      }
    }

    i1 = 0;
    i3 = 0;
    while ( i1 < o->num_vars ) {
      if ( used[i1] ) {
	i1++;
      } else {
	printf("\nwarning: parameter x%d of op %s is not used. skipping it.",
	       i3, o->name);
	for ( i2 = i1; i2 < o->num_vars-1; i2++ ) {
	  o->var_types[i2] = o->var_types[i2+1];
	  used[i2] = used[i2+1];
	}
	decrement_var_entries( o, i1 );
	o->num_vars--;
      }
      i3++;
    }
  }

}



void decrement_var_entries( Operator *o, int start )

{

  int st = ENCODE_VAR( start ), i, j;

  for ( i = 0; i < o->num_preconds; i++ ) {
    for ( j = 0; j < garity[o->preconds[i].predicate]; j++ ) {
      if ( o->preconds[i].args[j] < st ) {
	o->preconds[i].args[j]++;
      }
    }
  }
  for ( i = 0; i < o->num_adds; i++ ) {
    for ( j = 0; j < garity[o->adds[i].predicate]; j++ ) {
      if ( o->adds[i].args[j] < st ) {
	o->adds[i].args[j]++;
      }
    }
  }
  for ( i = 0; i < o->num_dels; i++ ) {
    for ( j = 0; j < garity[o->dels[i].predicate]; j++ ) {
      if ( o->dels[i].args[j] < st ) {
	o->dels[i].args[j]++;
      }
    }
  }

}












/**********************************
 * PREPROCESS INERTIA INFORMATION *
 **********************************/











void do_inertia_preprocessing( void )

{

  int i, j;

  collect_inertia_information();

  if ( gcmd_line.display_info == 104 ) {
    printf("\n\npredicates inertia info:");
    for ( i = 0; i < gnum_predicates; i++ ) {
      printf("\n%3d --> %s: ", i, gpredicates[i]);
      printf(" is %s, %s",
	     gis_added[i] ? "ADDED" : "NOT ADDED",
	     gis_deleted[i] ? "DELETED" : "NOT DELETED");
    }
    printf("\n\n");
  }


  split_initial_state();

  if ( gcmd_line.display_info == 105 ) {
    printf("\n\nfull initial state was:");
    for ( i = 0; i < gnum_full_initial; i++ ) {
      printf("\n");
      print_Fact( &(gfull_initial[i]) );
      if ( garity[gfull_initial[i].predicate] == 1 ) {
	printf(" --> obj. number %d", gfull_initial[i].args[0]);
      }
    }

    printf("\n\nsplitted initial state is:");

    printf("\n\nextended types table:");
    for ( i = 0; i < gnum_types; i++ ) {
      printf("\n%d --> ", i);
      if ( gpredicate_to_type[i] == -1 ) {
	printf("%s ", gtype_names[i]);
      } else {
	printf("UNARY INERTIA TYPE (%s) ", gpredicates[gpredicate_to_type[i]]);
      }
      for ( j = 0; j < gtype_size[i]; j++ ) {
	printf("%d ", gtype_consts[i][j]);
      }
    }

    printf("\n\nnon static initial state:");
    for ( i = 0; i < gnum_initial; i++ ) {
      printf("\n");
      print_Fact( &(ginitial[i]) );
    }

    printf("\n\nstatic initial state:");
    for ( i = 0; i < gnum_inertia; i++ ) {
      printf("\n");
      print_Fact( &(ginertia[i]) );
    }
    printf("\n\n");
  }


  encode_unary_inertia_as_types();
  remove_ops_with_empty_parameter_types();
  remove_unused_parameters();

  if ( gcmd_line.display_info == 106 ) {
    printf("\n\nfull initial state was:");
    for ( i = 0; i < gnum_full_initial; i++ ) {
      printf("\n");
      print_Fact( &(gfull_initial[i]) );
      if ( garity[gfull_initial[i].predicate] == 1 ) {
	printf(" --> obj. number %d", gfull_initial[i].args[0]);
      }
    }

    printf("\n\nintersections extended types table:");
    for ( i = 0; i < gnum_types; i++ ) {
      printf("\n%d --> ", i);
      if ( gpredicate_to_type[i] == -1 ) {
	if ( gnum_intersected_types[i] == -1 ) {
	  printf("%s: ", gtype_names[i]);
	} else {
	  printf("INTERSECTED TYPE (");
	  for ( j = 0; j < gnum_intersected_types[i]; j++ ) {
	    if ( gpredicate_to_type[gintersected_types[i][j]] == -1 ) {
	      printf("%s", gtype_names[gintersected_types[i][j]]);
	    } else {
	      printf("UNARY INERTIA TYPE (%s)", 
		     gpredicates[gpredicate_to_type[gintersected_types[i][j]]]);
	    }
	    if ( j < gnum_intersected_types[i] - 1 ) {
	      printf(" and ");
	    }
	  }
	  printf("): ");
	}
      } else {
	printf("UNARY INERTIA TYPE (%s): ", gpredicates[gpredicate_to_type[i]]);
      }
      for ( j = 0; j < gtype_size[i]; j++ ) {
	printf("%d ", gtype_consts[i][j]);
      }
    }

    printf("\n\nops with unary inertia preconds encoded:");
    for ( i = 0; i < gnum_operators; i++ ) {
      print_Operator( goperators[i] );
    }
    printf("\n\n");
  }

}



void collect_inertia_information( void )

{

  int i, j;

  for ( i = 0; i < gnum_predicates; i++ ) {
    gis_added[i] = FALSE;
    gis_deleted[i] = FALSE;
  }

  for ( i = 0; i < gnum_operators; i++ ) {
    for ( j = 0; j < goperators[i]->num_adds; j++ ) {
      gis_added[goperators[i]->adds[j].predicate] = TRUE;
    }
    for ( j = 0; j < goperators[i]->num_dels; j++ ) {
      gis_deleted[goperators[i]->dels[j].predicate] = TRUE;
    }
  }

}



void split_initial_state( void )

{

  int i, j, p, t;

  for ( i = 0; i < MAX_PREDICATES; i++ ) {
    gtype_to_predicate[i] = -1;
  }
  for ( i = 0; i < MAX_TYPES; i++ ) {
    gpredicate_to_type[i] = -1;
  }

  for ( i = 0; i < gnum_full_initial; i++ ) {
    p = gfull_initial[i].predicate;
    if ( !gis_added[p] &&
	 !gis_deleted[p] ) {
      if ( garity[p] == 1 ) {
	if ( (t = gtype_to_predicate[p]) == -1 ) {
	  if ( gnum_types == MAX_TYPES ) {
	    printf("\ntoo many (inferred) types! increase MAX_TYPES (currently %d)\n\n",
		   MAX_TYPES);
	    exit( 1 );
	  } 
	  gtype_to_predicate[p] = gnum_types;
	  gpredicate_to_type[gnum_types] = p;
	  gtype_names[gnum_types] = NULL;
	  gtype_size[gnum_types] = 0;
	  for ( j = 0; j < MAX_CONSTANTS; j++ ) {
	    gis_member[j][gnum_types] = FALSE;
	  }
	  t = gnum_types;
	  gnum_types++;
	}
	if ( gtype_size[t] == MAX_TYPE ) {
	  printf("\ntoo many consts in type %s! increase MAX_TYPE (currently %d)\n\n",
		 gtype_names[t], MAX_TYPE);
	  exit( 1 );
	}
	if ( !gis_member[gfull_initial[i].args[0]][gpredicates_args_type[p][0]] ) {
	  printf("\ntype mismatch in initial state! %s as arg 0 of %s\n\n",
		 gconstants[gfull_initial[i].args[0]], gpredicates[p]);
	  exit( 1 );
	}
	gtype_consts[t][gtype_size[t]++] = gfull_initial[i].args[0];
	gis_member[gfull_initial[i].args[0]][t] = TRUE;
      } else {
	if ( gnum_inertia == MAX_INITIAL ) {
	  printf("\ntoo many inertia! increase MAX_INITIAL (currently %d)\n\n",
		 MAX_INITIAL);
	  exit( 1 );
	}
	ginertia[gnum_inertia].predicate = p;
	for ( j = 0; j < garity[p]; j++ ) {
	  if ( !gis_member[gfull_initial[i].args[j]][gpredicates_args_type[p][j]] ) {
	    printf("\ntype mismatch in initial state! %s as arg %d of %s\n\n",
		   gconstants[gfull_initial[i].args[j]], j, gpredicates[p]);
	    exit( 1 );
	  }
	  ginertia[gnum_inertia].args[j] = gfull_initial[i].args[j];
	}
	gnum_inertia++;
      }
      continue;
    }
    if ( gnum_initial == MAX_STATE ) {
      printf("\ntoo many non static initials! increase MAX_STATE (currently %d)\n\n",
	     MAX_STATE);
      exit( 1 );
    }
    ginitial[gnum_initial].predicate = p;
    for ( j = 0; j < garity[p]; j++ ) {
      if ( !gis_member[gfull_initial[i].args[j]][gpredicates_args_type[p][j]] ) {
	printf("\ntype mismatch in initial state! %s as arg %d of %s\n\n",
	       gconstants[gfull_initial[i].args[j]], j, gpredicates[p]);
	exit( 1 );
      }
      ginitial[gnum_initial].args[j] = gfull_initial[i].args[j];
    }
    gnum_initial++;
  }

}



void encode_unary_inertia_as_types( void )

{

  Operator *o;
  int i1, i, j, k, l, new_T, p;
  TypeArray T;
  int num_T;

  int intersected_type;

  for ( i = 0; i < MAX_TYPES; i++ ) {
    gnum_intersected_types[i] = -1;
  }

  for ( i1 = 0; i1 < gnum_operators; i1++ ) {
    o = goperators[i1];

    for ( i = 0; i < o->num_vars; i++ ) {

      T[0] = o->var_types[i];
      num_T = 1;

      j = 0;
      while ( j < o->num_preconds ) {
	p = o->preconds[j].predicate;
	if ( ( (new_T = gtype_to_predicate[p]) != -1 ) &&
	     ( o->preconds[j].args[0] == ENCODE_VAR( i ) ) ) {
	  if ( num_T == MAX_TYPE_INTERSECTIONS ) {
	    printf("\nincrease MAX_TYPE_INTERSECTIONS (currently %d)\n\n",
		   MAX_TYPE_INTERSECTIONS);
	    exit( 1 );
	  }
	  /* insert new type number into ordered array T;
	   * ---- all type numbers in T are different:
	   *      new nr. is of inferred type - can't be type declared for param
	   *      precondition facts occur at most once - doubles are removed
	   *                                              during cleanup
	   */
	  for ( k = 0; k < num_T; k++ ) {
	    if ( new_T < T[k] ) {
	      break;
	    }
	  }
	  for ( l = num_T; l > k; l-- ) {
	    T[l] = T[l-1];
	  }
	  T[k] = new_T;
	  num_T++;
	  /* now remove superfluous precondition
	   */
	  for ( k = j; k < o->num_preconds-1; k++ ) {
	    o->preconds[k].predicate = o->preconds[k+1].predicate;
	    for ( l = 0; l < garity[o->preconds[k].predicate]; l++ ) {
	      o->preconds[k].args[l] = o->preconds[k+1].args[l];
	    }
	  }
	  o->num_preconds--;
	} else {
	  j++;
	}
      }

      /* if we did not hit any unary inertia concerning this parameter
       * in the preconds, skip parameter and go to next one
       */
      if ( num_T == 1 ) {
	continue;
      }

      /* now we have the ordered array of types to intersect for param i 
       * of op o in array T of size num_T;
       * if there already is this intersected type, set type of this
       * param to its number, otherwise create the new intersected type.
       */
      if ( (intersected_type = find_intersected_type( T, num_T )) != -1 ) {
	/* type already there
	 */
	o->var_types[i] = intersected_type;
	continue;
      }
      /* have to create new type
       */
      if ( gnum_types == MAX_TYPES ) {
	printf("\ntoo many (inferred and intersected) types! increase MAX_TYPES (currently %d)\n\n",
	       MAX_TYPES);
	exit( 1 );
      } 
      gtype_names[gnum_types] = NULL;
      gtype_size[gnum_types] = 0;
      for ( j = 0; j < MAX_CONSTANTS; j++ ) {
	gis_member[j][gnum_types] = FALSE;
      }
      for ( j = 0; j < num_T; j++ ) {
	gintersected_types[gnum_types][j] = T[j];
      }
      gnum_intersected_types[gnum_types] = num_T;
      intersected_type = gnum_types;
      o->var_types[i] = intersected_type;
      gnum_types++;

      for ( j = 0; j < gtype_size[T[0]]; j++ ) {
	for ( k = 1; k < num_T; k++ ) {
	  if ( !gis_member[gtype_consts[T[0]][j]][T[k]] ) {
	    break;
	  }
	}
	if ( k < num_T ) {
	  continue;
	}
	/* add constant to new type
	 */
	if ( gtype_size[intersected_type] == MAX_TYPE ) {
	  printf("\ntoo many consts in type %s! increase MAX_TYPE (currently %d)\n\n",
		 gtype_names[intersected_type], MAX_TYPE);
	  exit( 1 );
	}
	gtype_consts[intersected_type][gtype_size[intersected_type]++] = gtype_consts[T[0]][j];
	gis_member[gtype_consts[T[0]][j]][intersected_type] = TRUE;
      }
      
      /* now verify if the intersected type equals one of the types that we intersected.
       * this is the case, iff one of the types in T has the same size as intersected_type
       */
      for ( j = 0; j < num_T; j++ ) {
	if ( gtype_size[intersected_type] != gtype_size[T[j]] ) {
	  continue;
	}
	/* type T[j] contains exactly the constants that we need!
	 *
	 * remove intersected type from table!
	 */
	gtype_size[intersected_type] = 0;
	for ( k = 0; k < MAX_CONSTANTS; k++ ) {
	  gis_member[k][intersected_type] = FALSE;
	}
	gnum_intersected_types[intersected_type] = -1;
	gnum_types--;
	o->var_types[i] = T[j];
	break;
      }
    }
  }

}



int find_intersected_type( TypeArray T, int num_T )

{

  int i, j;

  for ( i = 0; i < gnum_types; i++ ) {
    if ( gnum_intersected_types[i] == -1 ) {
      continue;
    }

    if ( gnum_intersected_types[i] != num_T ) {
      continue;
    }

    for ( j = 0; j < num_T; j++ ) {
      if ( T[j] != gintersected_types[i][j] ) {
	break;
      }
    }
    if ( j < num_T ) {
      continue;
    }

    return i;
  }

  return -1;

}
  


void remove_ops_with_empty_parameter_types( void )

{

  Operator *o;
  int i, j;

  /* mark ops that have a parameter with empty type
   */
  for (  i = 0; i < gnum_operators; i++ ) {
    o = goperators[i];
    for ( j = 0; j < o->num_vars; j++ ) {
      if ( gtype_size[o->var_types[j]] == 0 ) {
	printf("\nwarning: parameter x%d of op %s has become empty type. skipping op.",
	       j, o->name);
	o->out = TRUE;
	break;
      }
    }
  }

  /* remove ops that are marked as out
   */
  i = 0;
  while ( i < gnum_operators ) {
    if ( goperators[i]->out ) {
      free_Operator( goperators[i] );
      for ( j = i; j < gnum_operators-1; j++ ) {
	goperators[j] = goperators[j+1];
      }
      gnum_operators--;
    } else {
      i++;
    }
  }

}
