




/*********************************************************************
 * File: instantiateI.c
 * Description: code for instantiating operators, first part.
 *              - transforms PlNodes to CodeNodes
 *              - performs syntax check
 *              - builds gobal tuple tables
 *              - encodes unary inertia as types
 *
 * Author: Joerg Hoffmann 1999
 *
 *********************************************************************/ 
/*********************************************************************
 * (C) Copyright 1998 Albert Ludwigs University Freiburg
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
 *
 * one thing that I'd like to say about the instantiation code is this:
 *
 *      it might look terrible to have four big files of C code just
 *      to instantiate the operators, and in fact, it is.
 *
 *      on the other hand, this is only the result of the terrible
 *      things that the domain designer is allowed to define in 
 *      full scale PDDL.
 *
 *      though the code produces a very tight domain representation
 *      in all cases, see Technical Report 122 
 *      "Handling of Inertia in a Planning System" on the IPP homepage,
 *      there are quite some cases where it could be made far more
 *      efficient. At the very least, if one is facing a simple STRIPS
 *      domain, it's better to skip the whole thing and use a specialised
 *      algorithm which will be faster than this code by factors in the order
 *      of hundreds...
 *
 **********************************************************************/








#include "ipp.h"

#include "output.h"
#include "utilities.h"
#include "memory.h"

#include "instantiateI.h"



  




/*
 *  ----------------------------- SET UP GLOBAL STRING ARRAYS ----------------------------
 */









void collect_all_strings( void )

{

  FactList *f;
  TokenList *t;
  int p_num, type_num, c_num, i, j, ar;
  Integers *tmp, *g;

  for ( f = gorig_constant_list; f; f = f->next ) {
    if ( (type_num = position_in_types_table( f->item->next->item )) == -1 ) {
      if ( gtypes_table_size == MAX_TYPES_TABLE ) {
	printf("\ntoo many types! increase MAX_TYPES_TABLE (currently %d)\n\n",
	       MAX_TYPES_TABLE);
	exit( 1 );
      }
      gtypes_table[gtypes_table_size].name = copy_string( f->item->next->item );
      gtypes_table[gtypes_table_size].integers = NULL;
      type_num = gtypes_table_size++;
    }

    if ( (c_num = position_in_constants_table( f->item->item )) == -1 ) {
      if ( gconstants_table_size == MAX_CONSTANTS_TABLE ) {
	printf("\ntoo many constants! increase MAX_CONSTANTS_TABLE (currently %d)\n\n",
	       MAX_CONSTANTS_TABLE);
	exit( 1 );
      }
      gconstants_table[gconstants_table_size] = copy_string( f->item->item );
      c_num = gconstants_table_size++;
    }
    
    tmp = new_integers( c_num );
    for ( g = gtypes_table[type_num].integers; g; g = g->next ) {
      if ( g->index == c_num ) break;
    }
    if ( !g ) {
      tmp->next = gtypes_table[type_num].integers;
      gtypes_table[type_num].integers = tmp;
    }
  }

  for ( f = gpredicates_and_types; f; f = f->next ) {
    if ( (p_num = position_in_predicates_table( f->item->item )) != -1 ) {
      printf("\npredicate %s declared twice!\n\n", f->item->item);
      exit(BADDOMAIN_ERROR_CODE);
    }
    if ( gpredicates_table_size == MAX_PREDICATES_TABLE ) {
      printf("\ntoo many predicates! increase MAX_PREDICATES_TABLE (currently %d)\n\n",
	     MAX_PREDICATES_TABLE);
      exit( 1 );
    }
    gpredicates_table[gpredicates_table_size] = copy_string( f->item->item );
    ar = 0;
    for ( t = f->item->next; t; t = t->next ) {
      if ( (type_num = position_in_types_table( t->item )) == -1 ) {
	printf("\nwarning: predicate %s uses unknown or empty type %s\n\n", 
	       f->item->item, t->item);
      }
      gpredicates_args_type[gpredicates_table_size][ar++] = type_num;
    }
    garity[gpredicates_table_size++] = ar;
  }

  free_complete_fact_list( gorig_constant_list );
  free_complete_fact_list( gpredicates_and_types );

  if ( gcmd_line.display_info == 2 ||
       gcmd_line.display_info == 3 ) {
    printf("\nconstant table:");
    for ( i = 0; i < gconstants_table_size; i++ ) {
      printf("\n%d --> %s", i, gconstants_table[i]);
    }
    printf("\n\ntypes table:");
    for ( i = 0; i < gtypes_table_size; i++ ) {
      printf("\n%d --> %s: ", i, gtypes_table[i].name);
      for ( tmp=gtypes_table[i].integers; tmp; tmp=tmp->next ) {
	printf("%d ", tmp->index);
      }
    }
    printf("\n\npredicates table:");
    for ( i = 0; i < gpredicates_table_size; i++ ) {
      printf("\n%d --> %s: ", i, gpredicates_table[i]);
      for ( j = 0; j < garity[i]; j++ ) {
	printf("%s ", gtypes_table[gpredicates_args_type[i][j]].name);
      }
    }
    printf("\n\n");
  }

}



int position_in_types_table( char *str )

{

  int i;

  for ( i=0; i<gtypes_table_size; i++ ) {
    if ( str == gtypes_table[i].name || 
	 (strcmp( str, gtypes_table[i].name ) == SAME) ) {
      break;
    }
  }

  return ( i == gtypes_table_size ) ? -1 : i;

}



int position_in_constants_table( char *str )

{

  int i;

  for ( i=0; i<gconstants_table_size; i++ ) {
    if ( str == gconstants_table[i] || 
	 strcmp( str, gconstants_table[i] ) == SAME ) {
      break;
    }
  }

  return ( i == gconstants_table_size ) ? -1 : i;

}



int position_in_predicates_table( char *str )

{

  int i;

  for ( i=0; i<gpredicates_table_size; i++ ) {
    if ( str == gpredicates_table[i] || 
	 strcmp( str, gpredicates_table[i] ) == SAME ) {
      break;
    }
  }

  return ( i == gpredicates_table_size ) ? -1 : i;

}



  






/*
 *  ----------------------- TRANSLATE TO CODE NODES + SYNTAX CHECK ------------------------
 */









char *lvar_str[MAX_VARS];
int lvar_types[MAX_VARS];








void transform_PlNodes_to_CodeNodes( void )

{

  PlOperator *op;
  CodeOperator *tmp, *j;

  if ( !normalize_initial( &gorig_initial_facts ) ) {
    printf("\nillegal initial state!\n\n");
    exit(BADDOMAIN_ERROR_CODE);
  }
  if ( !is_legal_condition( gorig_goal_facts ) ) {
    printf("\nillegal goal state!\n\n");
    exit(BADDOMAIN_ERROR_CODE);
  }
  for ( op = gloaded_ops; op; op = op->next ) {
    if ( !is_legal_condition( op->preconds ) ) {
      printf("\nop %s has illegal precondition\n\n", op->name->item);
      exit(BADDOMAIN_ERROR_CODE);
    }
    if ( !normalize_effects( &op->effects ) ) {
      printf("\nop %s has illegal effects\n\n", op->name->item);
      exit(BADDOMAIN_ERROR_CODE);
    }
  }

  gcode_initial_state = transform_Pl_to_Code( gorig_initial_facts, 0 );
  gcode_goal_state = transform_Pl_to_Code( gorig_goal_facts, 0 );
  for ( op = gloaded_ops; op; op = op->next ) {
    tmp = transform_PlOp_to_CodeOp( op );
    if ( tmp != NULL ) {
      tmp->next = gcode_operators;
      gcode_operators = tmp;
    }
  }

  if ( gcmd_line.display_info == 3 ) {
    printf("\ntranslation to CodeNodes complete.");
    printf("\ncoded initial state is:\n");
    print_CodeNode( gcode_initial_state, 0 );
    printf("\ncoded goal state is:\n");
    print_CodeNode( gcode_goal_state, 0 );
    printf("\ncoded operators are:\n");
    for ( j = gcode_operators; j; j = j->next ) {
      print_CodeOperator( j );
    }
  }

}
  


Bool normalize_initial( PlNode **n )

{

  PlNode *i, *tmp;
  TokenList *t;

  if ( !(*n) ) {
    return TRUE;
  }

  if ( (*n)->connective != AND ) {
    tmp = copy_pl_node( *n );
    tmp->sons = (*n)->sons;
    tmp->next = (*n)->next;
    (*n)->connective = AND;
    (*n)->sons = tmp;
    (*n)->next = NULL;

    return normalize_initial( n );
  }

  for ( i = (*n)->sons; i; i=i->next ) {
    if ( i->connective != ATOM ) {
      printf("\nnon ATOM in initial state");
      return FALSE;
    }

    if ( i->sons != NULL ) {
      printf("\ninitial atom has sons");
      return FALSE;
    }

    if ( (strcmp( i->atom->item, "EQ" ) == SAME) ||
	 (strcmp( i->atom->item, "eq" ) == SAME) ) {
      printf("\neq predicate in initial state");
      return FALSE;
    }

    for ( t = i->atom->next; t; t = t->next ) {
      if ( t->item[0] == '?' ) {
	printf("\nvariable in initial state");
	return FALSE;
      }
    }
  }

  return TRUE;

}



Bool is_legal_condition( PlNode *n )

{

  PlNode *i;

  if ( !n ) {
    return TRUE;
  }

  switch ( n->connective ) {
  case ATOM:
    if ( n->sons != NULL ) {
      printf("\nATOM has son");
      return FALSE;
    }
    return TRUE;
  case ALL:
  case EX:
  case NOT:
    if ( !(n->sons) ) {
      printf("\n%s without sons",
	     gconnectives[n->connective]);
    }
    if ( !is_legal_condition( n->sons ) ) {
      return FALSE;
    }
    return TRUE;
  case AND:
  case OR:
    if ( !(n->sons) ) {
      printf("\n%s without sons",
	     gconnectives[n->connective]);
      return FALSE;
    }
    for ( i=n->sons; i; i = i->next ) {
      if ( !is_legal_condition( i ) ) {
	return FALSE;
      }
    }
    return TRUE;
  case TRU:
  case FAL:
    if ( n->sons ) {
      printf("\n%s with sons", 
	     gconnectives[n->connective]);
      return FALSE;
    }
    return TRUE;
  default:
    printf("\nillegal connective in condition: %s", 
	   gconnectives[n->connective]);
    return FALSE;
  }

  return TRUE;

}



Bool normalize_effects( PlNode **n )

{

  PlNode *i, *j, *tmp;

  if ( !(*n) ) {
    return TRUE;
  }

  if ( (*n)->connective != AND ) {
    tmp = copy_pl_node( *n );
    tmp->sons = (*n)->sons;
    tmp->next = (*n)->next;
    (*n)->connective = AND;
    (*n)->sons = tmp;
    (*n)->next = NULL;

    return normalize_effects( n );
  }

  for ( i = (*n)->sons; i; i = i->next ) {
    for ( j = i; j; j = j->sons ) {
      if ( j->connective == WHEN ) {
	break;
      }
      if ( j->connective != ALL ) {
	printf("\nnon ALL before WHEN");
	return FALSE;
      }
    }
    if ( !j ) {
      printf("\neffect without WHEN");
      return FALSE;
    }
    if ( !j->sons ) {
      printf("\neffect has neither condition nor postconds");
      return FALSE;
    }
    if ( !j->sons->next ) {
      printf("\neffect has empty postconds");
      return FALSE;
    }
    if ( !is_legal_condition( j->sons ) ) {
      printf("\neffect condition illegal");
      return FALSE;
    }
    if ( !normalize_effect( &(j->sons->next) ) ) {
      printf("\neffect illegal");
      return FALSE;
    }
  }

  return TRUE;

}



Bool normalize_effect( PlNode **n )

{

  PlNode *i, *tmp;

  if ( !(*n) ) {
    return FALSE;
  }

  if ( (*n)->connective != AND ) {
    tmp = copy_pl_node( *n );
    tmp->sons = (*n)->sons;
    tmp->next = (*n)->next;
    (*n)->connective = AND;
    (*n)->sons = tmp;
    (*n)->next = NULL;

    return normalize_effect( n );
  }

  for ( i = (*n)->sons; i; i = i->next ) {
    if ( i->connective != ATOM ) {
      if ( i->connective != NOT ) {
	printf("\nnon literal in effect");
	return FALSE;
      }
      if ( !i->sons ) {
	printf("NOT without son");
	return FALSE;
      }
      if ( i->sons->connective != ATOM ) {
	printf("\nnon literal in effect");
	return FALSE;
      }
      if ( i->sons->sons ||
	   i->sons->next ) {
	printf("\nnegated atom with sons/next");
	return FALSE;
      }
      if ( (strcmp( i->sons->atom->item, "EQ" ) == SAME) ||
	   (strcmp( i->sons->atom->item, "eq" ) == SAME) ) {
	printf("\neq predicate in effect");
	return FALSE;
      }
    } else {
      if ( (strcmp( i->atom->item, "EQ" ) == SAME) ||
	   (strcmp( i->atom->item, "eq" ) == SAME) ) {
	printf("\neq predicate in effect");
	return FALSE;
      }
      if ( i->sons ) {
	printf("\natom with sons");
	return FALSE;
      }
    }
  }
   
  return TRUE;

}
   


CodeOperator *transform_PlOp_to_CodeOp( PlOperator *op )

{

  FactList *f;
  int num_vars = 0;

  CodeOperator *tmp = new_CodeOperator();

  tmp->name = copy_string( op->name->item );
  tmp->number_of_real_params = op->number_of_real_params;

  for ( f = op->params; f; f = f->next ) {
    if ( num_vars == MAX_VARS ) {
      printf("\ntoo many vars. increase MAX_VARS (currently %d)\n\n",
	     MAX_VARS);
      exit( 1 );
    }
    lvar_str[num_vars] = f->item->item;
    if ( (lvar_types[num_vars] =
	  position_in_types_table( f->item->next->item )) == -1 ) {
      if ( gcmd_line.display_info ) {
	printf("\nwarning: parameter %s of op %s has unknown or empty type %s.skipping op",
	       f->item->item, tmp->name, f->item->next->item);
      }
      free_CodeOperator( tmp );
      return NULL;
    }
    tmp->var_types[tmp->num_vars++] = lvar_types[num_vars];
    num_vars++;
  }

  tmp->preconds = transform_Pl_to_Code( op->preconds, num_vars );
  tmp->conditionals = transform_Pl_to_Code( op->effects, num_vars );

  return tmp;

}



CodeNode *transform_Pl_to_Code( PlNode *p, int num_vars )

{

  CodeNode *res = NULL, *tmp;
  PlNode *nn;
  TokenList *t;
  int i, n;

  if ( !p ) {
    return NULL;
  }

  res = new_CodeNode( p->connective );

  switch ( p->connective ) {

  case ALL:
  case EX:
    if ( num_vars == MAX_VARS ) {
      printf("\ntoo many vars. increase MAX_VARS (currently %d)\n\n",
	     MAX_VARS);
      exit( 1 );
    }
    lvar_str[num_vars] = p->atom->item;
    if ( (lvar_types[num_vars] =
	  position_in_types_table( p->atom->next->item )) == -1 ) {
      if ( gcmd_line.display_info ) {
	printf("\nwarning: quantified var %s has unknown or empty type %s. simplifying...",
	       p->atom->item, p->atom->next->item);
      }
      if ( p->connective == ALL ) {
	res->connective = TRU;
	res->next = transform_Pl_to_Code( p->next, num_vars );    
	break;	
      } else {
	res->connective = FAL;
	res->next = transform_Pl_to_Code( p->next, num_vars );    
	break;	
      }
    }
    res->var = num_vars;
    res->var_type = lvar_types[num_vars];
    res->sons = transform_Pl_to_Code( p->sons, num_vars+1 );
    break;

  case ATOM:
    if ( strcmp( p->atom->item, "EQ" ) == SAME ||
	 strcmp( p->atom->item, "eq" ) == SAME ) {
      res->predicate = -1;
    } else {
      res->predicate = position_in_predicates_table( p->atom->item );
      if ( res->predicate == -1 ) {
	printf("\nundeclared predicate %s used in domain definition\n\n",
	       p->atom->item);
	exit( 1 );
      }
    }
    n = 0;
    for ( t = p->atom->next; t; t = t->next ) {
      if ( t->item[0] == '?' ) {
	for ( i=0; i<num_vars; i++ ) {
	  if ( strcmp( lvar_str[i], t->item ) == SAME ) break;
	}
	if ( i == num_vars ) {
	  printf("\nunknown variable %s in literal %s. check input files\n\n",
		 t->item, p->atom->item);
	  exit( 1 );
	}
	if ( res->predicate != -1 &&
	     !is_subtype( lvar_types[i], gpredicates_args_type[res->predicate][n] ) ) {
	  printf("\ntype of var %s doesnt match type of arg %d of predicate %s\n\n",
		 lvar_str[i], n, gpredicates_table[res->predicate]);
	  exit( 1 );
	}
	res->arguments[n] = ((-1) * i) - 1;
      } else {
	if ( (res->arguments[n] = 
	      position_in_constants_table( t->item )) == -1 ) {
	  printf("\nunknown constant %s in literal %s. check input files\n\n",
		 t->item, p->atom->item);
	  exit( 1 );
	}
      }
      n++;
    }
    if ( res->predicate == -1 ) {
      if ( n != 2 ) {
	printf("\nfound eq - predicate with %d arguments. check input files\n\n",
	       n);
	exit( 1 );
      }
    } else {
      if ( n != garity[res->predicate] ) {
	printf("\npredicate %s is declaredto have %d arguments. check input files\n\n",
	       gpredicates_table[res->predicate],
	       garity[res->predicate]);
	exit( 1 );
      }
    }
    break;
  case AND:
  case OR:
    for ( nn = p->sons; nn; nn = nn->next ) {
      tmp = transform_Pl_to_Code( nn, num_vars );
      tmp->next = res->sons;
      res->sons = tmp;
    }
    break;
  case NOT:
    res->sons = transform_Pl_to_Code( p->sons, num_vars );
    break;
  case WHEN:
    res->sons = transform_Pl_to_Code( p->sons, num_vars );
    res->sons->next = transform_Pl_to_Code( p->sons->next, num_vars );
    break;
  case TRU:
  case FAL:
    break;
  default:
    printf("\nshouldn't come here. translation of %s\n\n",
	   gconnectives[p->connective]);
  }

  return res;

}



Bool is_subtype( int t1, int t2 )

{

  Integers *in, *in2;

  for ( in = gtypes_table[t1].integers; in; in = in->next ) {
    for ( in2 = gtypes_table[t2].integers; in2; in2 = in2->next ) {
      if ( in->index == in2->index ) {
	break;
      }
    }
    if ( !in2 ) {
      return FALSE;
    }
  }

  return TRUE;

}













/*
 *  ----------------------- BUILD IMPLICIT TUPLE TABLES ----------------------------
 *
 */




int lmaxdom;








void build_predicate_tables( void )

{

  int i, j, k, size, num_sets;
  int start, end, set_size;
  CodeNode *p = gcode_initial_state, *curr;
  ArgArray set;
  Integers *in;

  char *str[] = { "INERTIA", "POSITIVE INERTIA", 
		  "NEGATIVE INERTIA", "FLUENT" };

  setup_added_deleted_info();

  if ( gcmd_line.display_info == 4 ||
       gcmd_line.display_info == 5 ) {
    printf("\npredicates info:");
    for ( i = 0; i < gpredicates_table_size; i++ ) {
      printf("\n%d --> %s (arity %d): is %s added and %s deleted (TR 122: %s)", 
	     i, 
	     gpredicates_table[i],
	     garity[i],
	     gis_added[i] ? "" : "NOT",
	     gis_deleted[i] ? "" : "NOT",
	     str[gis_added[i]*2+gis_deleted[i]]);
    }
    printf("\n\n");
  }

  for ( i=0; i<gtypes_table_size; i++ ) {
    size = 0;
    for ( in = gtypes_table[i].integers; in; in = in->next ) {
      size++;
    }
    gtype_size[i] = size;
  }

  lmaxdom = gconstants_table_size;

  for ( i=0; i<gpredicates_table_size; i++ ) {
    gone_table_size[i] = a_to_the_power_of_b( lmaxdom, garity[i] );
  }
    
  for ( i=0; i<gpredicates_table_size; i++ ) {
    num_sets = a_to_the_power_of_b( 2, garity[i] );
    size = gone_table_size[i] * num_sets;

    gtuples[i] = ( int_pointer ) calloc( size, sizeof( int ) );

    for ( j=0; j<garity[i]; j++ ) {
      set[j] = 0;
    }
    set_size = 0;
    start = 0;
    while ( TRUE ) {
      end = start + a_to_the_power_of_b( lmaxdom, set_size );
      for ( k = start; k < end; k++ ) { 
	gtuples[i][k] = 0;
      }

      set_size = increment_set( garity[i], &set );
      if ( set_size == 0 ) {
	break;
      }
      start += gone_table_size[i];
    }
  }

  if ( p ) {
    for ( curr = p->sons; curr; curr = curr->next ) {
      increment_tuples( curr );
    }
  }

  if ( gcmd_line.display_info == 101 ) {
    printf("\nTUPLE TABLES");
    for ( i=0; i<gpredicates_table_size; i++ ) {
      printf("\nPREDICATE %s", gpredicates_table[i]);

      for ( j=0; j<garity[i]; j++ ) {
	set[j] = 0;
      }
      set_size = 0;
      start = 0;
      while ( TRUE ) {
	printf("\nTable to set: ");
	for ( j = 0; j < garity[i]; j++ ) {
	  printf(" %d", set[j]);
	}
	end = start + a_to_the_power_of_b( lmaxdom, set_size );
	for ( k = start; k < end; k++ ) { 
	  printf("\n%d", gtuples[i][k]);
	}
	
	set_size = increment_set( garity[i], &set );
	if ( set_size == 0 ) {
	  break;
	}
	start += gone_table_size[i];
      }
    }
  }

}



void setup_added_deleted_info( void )

{

  CodeOperator *i;
  CodeNode *j, *k;
  int l;

  for ( l = 0; l < MAX_PREDICATES_TABLE; l++ ) {
    gis_added[l] = FALSE;
    gis_deleted[l] = FALSE;
  }

  for ( i = gcode_operators; i; i = i->next ) {
    for ( j = i->conditionals->sons; j; j = j->next ) {
      for ( k = j; k; k = k->sons ) {
	if ( k->connective == WHEN ) {
	  break;
	}
      }
      extract_added_deleted_info( k->sons->next );
    }
  }

}



void extract_added_deleted_info( CodeNode *n )

{

  CodeNode *i;

  for ( i = n->sons; i; i = i->next ) {
    if ( i->connective == NOT ) {
      gis_deleted[i->sons->predicate] = TRUE;
    } else {
      gis_added[i->predicate] = TRUE;
    }
  }

}



void increment_tuples( CodeNode *n )

{

  int a = n->predicate;
  int set_size;
  int j, basis, adr;
  ArgArray set;

  for ( j=0; j<garity[a]; j++ ) {
    set[j] = 0;
  }
  set_size = 0;
  basis = 0;
  while ( TRUE ) {
    adr = basis +
      inner_table_adr_sethelper( garity[a], set, n->arguments );
    gtuples[a][adr]++;

    set_size = increment_set( garity[a], &set );
    if ( set_size == 0 ) {
      break;
    }
    basis += gone_table_size[a];
  }

}



Bool possibly_positive( int predicate, ArgArray arguments )

{

  int adr;

  if ( gis_added[predicate] ) {
    return TRUE;
  }

  adr = set_number( garity[predicate], arguments ) * gone_table_size[predicate];
  adr += inner_table_adr( garity[predicate], arguments );

  if ( gtuples[predicate][adr] > 0 ) {
    return TRUE;
  } else {
    return FALSE;
  }
    
}



Bool possibly_negative( int predicate, ArgArray arguments )

{

  int adr;

  if ( gis_deleted[predicate] ) {
    return TRUE;
  }

  adr = set_number( garity[predicate], arguments ) * gone_table_size[predicate];
  adr += inner_table_adr( garity[predicate], arguments );

  if ( gtuples[predicate][adr] < 
       max_unifying_tuples( predicate, arguments ) ) {
    return TRUE;
  } else {
    return FALSE;
  }

}



int a_to_the_power_of_b( int a, int b )

{

  int r = 1, i;
  
  for ( i = 0; i < b; i++ ) {
    r *= a;
  }

  return r;

}



int increment_set( int n, ArgArray *set )

{

  int r = 0, i;

  for ( i = 0; i < n; i++ ) {
    if ( (*set)[i] == 0 ) {
      (*set)[i] = 1;
      break;
    }
    (*set)[i] = 0;
  }

  for ( ; i < n; i++ ) {
    if ( (*set)[i] == 1 ) {
      r++;
    }
  }

  return r;

}



int inner_table_adr_sethelper( int n, ArgArray set, ArgArray args )

{

  int i, r, value;

  i = 0;
  value = 1;
  r = 0;
  while ( i < n ) {
    if ( set[i] == 1 ) {
      r += args[i] * value;
      value *= lmaxdom;
    }
    i++;
  }

  return r;

}



int inner_table_adr( int n, ArgArray args )

{

  int i, r, value;

  i = 0;
  value = 1;
  r = 0;
  while ( i < n ) {
    if ( args[i] >= 0 ) {
      r += args[i] * value;
      value *= lmaxdom;
    }
    i++;
  }

  return r;

}



int set_number( int n, ArgArray args )

{

  int r, i, value;

  r = 0;
  value = 1;
  for ( i = 0; i < n; i++ ) {
    if ( args[i] >= 0 ) {
      r += value;
    }
    value *= 2;
  }

  return r;

}



int max_unifying_tuples( int p, ArgArray args )

{

  int n = garity[p], i, r = 1;

  for ( i = 0; i < n; i++ ) {
    if ( args[i] < 0 ) {
      r *= gtype_size[gpredicates_args_type[p][i]];
    }
  }

  return r;

}



void free_predicate_tables( void )

{

  int i;

  for ( i = 0; i < gpredicates_table_size; i++ ) {
    free( gtuples[i] );
  }

}











/*
 *  ----------------------- ENCODE UNARY INERTIA IN TYPES --------------------------
 */








CodeNode *ltmpI;








void encode_unary_inertia_in_types( void )

{

  CodeOperator *o, *o2 = NULL, *o3;
  Integers *tmp, *tt;
  int i, l;
  ArgArray a;
  short int unary;
  CodeNode *n, *nn, *jj, *next;

  encode_unaries_under_CodeNode( &(gcode_goal_state) );

  for ( o = gcode_operators; o; o = o->next ) {
    encode_unaries_under_CodeNode( &(o->preconds) );
    encode_unaries_under_CodeNode( &(o->conditionals) );
  }
  for ( o = gcode_operators; o; o = o->next ) {
    /* normalize prefixes of conditional effects!
     *
     * these can right now have the form of general binary trees,
     * where the inner nodes of grade 2 are AND nodes, while the 
     * nodes with grade one are ALL nodes; the leaves are all WHENs
     */
    n = o->conditionals;
    if ( !n ) {
      continue;
    }
    nn = n->sons;
    while ( nn ) {
      collect_all_effect_prefix_paths( nn );
      free_CodeNode( nn->sons );
      nn->connective = ltmpI->connective;
      if ( nn->connective == ALL ) {
	nn->var = ltmpI->var;
	nn->var_type = ltmpI->var_type;
      }
      nn->sons = ltmpI->sons;
      next = nn->next;
      nn->next = ltmpI->next;
      for ( jj = nn; jj->next; jj = jj->next );
      jj->next = next;
      nn = next;
      free( ltmpI );
    }
  }

  o = gcode_operators;
  while ( o ) {
    i = 0;
    
    while( i<o->num_vars ) {
      if ( (unary = var_used_in_unary_under( i, o->preconds )) != -1 ) {
	o2 = new_CodeOperator();
	o2->name = copy_string( o->name );
	o2->number_of_real_params = o->number_of_real_params;
	o2->num_vars = o->num_vars;
	for ( l=0; l<o->num_vars; l++ ) {
	  o2->var_types[l] = o->var_types[l];
	}
	o2->preconds = deep_copy_CodeTree( o->preconds );
	o2->conditionals = deep_copy_CodeTree( o->conditionals );
	for ( o3 = o; o3->next; o3 = o3->next );
	o3->next = o2;
	
	/* set types of var i
	 * in o: typ \cap ini(unary)
	 * in o2: typ \setminus ini(unary)
	 */
	if ( gtypes_table_size == MAX_TYPES_TABLE ) {
	  printf("\ntoo many types (+inferred types)!increase MAX_TYPES_TABLE (currently %d)\n\n",
		 MAX_TYPES_TABLE);
	  exit( 1 );
	}
	gtypes_table[gtypes_table_size].name = gnew_types_name;
	gtypes_table[gtypes_table_size].integers = NULL;
	for ( tt = gtypes_table[o->var_types[i]].integers; tt; tt = tt->next ) {
	  a[0] = tt->index;
	  if ( possibly_positive( unary, a ) ) {
	    tmp = new_integers( tt->index );
	    tmp->next = gtypes_table[gtypes_table_size].integers;
	    gtypes_table[gtypes_table_size].integers = tmp;
	  }
	}
	o->var_types[i] = gtypes_table_size;

	gtypes_table_size++;
	if ( gtypes_table_size == MAX_TYPES_TABLE ) {
	  printf("\ntoo many types (+inferred types)! increase MAX_TYPES_TABLE (currently %d)\n\n",
		 MAX_TYPES_TABLE);
	  exit( 1 );
	}
	gtypes_table[gtypes_table_size].name = gnew_types_name;
	gtypes_table[gtypes_table_size].integers = NULL;
	for ( tt = gtypes_table[o2->var_types[i]].integers; tt; tt = tt->next ) {
	  a[0] = tt->index;
	  if ( !possibly_positive( unary, a ) ) {
	    tmp = new_integers( tt->index );
	    tmp->next = gtypes_table[gtypes_table_size].integers;
	    gtypes_table[gtypes_table_size].integers = tmp;
	  }
	}
	o2->var_types[i] = gtypes_table_size;
	gtypes_table_size++;
	
	/* in o->preconds: unary(i) = TRUE
	 * in o2->preconds: unary(i) = FALSE
	 */
	replace_unary_var_occurences( unary, i, TRU, &(o->preconds) );
	replace_unary_var_occurences( unary, i, FAL, &(o2->preconds) );
	/* same in conditionals
	 */
	replace_unary_var_occurences( unary, i, TRU, &(o->conditionals) );
	replace_unary_var_occurences( unary, i, FAL, &(o2->conditionals) );
	continue;
      }


      if ( (unary = var_used_in_unary_under( i, o->conditionals )) != -1 ) {
	o2 = new_CodeOperator();
	o2->name = copy_string( o->name );
	o2->number_of_real_params = o->number_of_real_params;
	o2->num_vars = o->num_vars;
	for ( l=0; l<o->num_vars; l++ ) {
	  o2->var_types[l] = o->var_types[l];
	  o2->inst_table[l] = o->inst_table[l];
	}
	o2->preconds = deep_copy_CodeTree( o->preconds );
	o2->conditionals = deep_copy_CodeTree( o->conditionals );
	for ( o3 = o; o3->next; o3 = o3->next );
	o3->next = o2;
	
	if ( gtypes_table_size == MAX_TYPES_TABLE ) {
	  printf("\ntoo many types (+inferred types)! increase MAX_TYPES_TABLE (currently %d)\n\n",
		 MAX_TYPES_TABLE);
	  exit( 1 );
	}
	gtypes_table[gtypes_table_size].name = gnew_types_name;
	gtypes_table[gtypes_table_size].integers = NULL;
	for ( tt = gtypes_table[o->var_types[i]].integers; tt; tt = tt->next ) {
	  a[0] = tt->index;
	  if ( possibly_positive( unary, a ) ) {
	    tmp = new_integers( tt->index );
	    tmp->next = gtypes_table[gtypes_table_size].integers;
	    gtypes_table[gtypes_table_size].integers = tmp;
	  }
	}
	o->var_types[i] = gtypes_table_size;

	gtypes_table_size++;
	if ( gtypes_table_size == MAX_TYPES_TABLE ) {
	  printf("\ntoo many types (+inferred types)! increase MAX_TYPES_TABLE (currently %d)\n\n",
		 MAX_TYPES_TABLE);
	  exit( 1 );
	}
	gtypes_table[gtypes_table_size].name = gnew_types_name;
	gtypes_table[gtypes_table_size].integers = NULL;
	for ( tt = gtypes_table[o2->var_types[i]].integers; tt; tt = tt->next ) {
	  a[0] = tt->index;
	  if ( !possibly_positive( unary, a ) ) {
	    tmp = new_integers( tt->index );
	    tmp->next = gtypes_table[gtypes_table_size].integers;
	    gtypes_table[gtypes_table_size].integers = tmp;
	  }
	}
	o2->var_types[i] = gtypes_table_size;
	gtypes_table_size++;
	
	replace_unary_var_occurences( unary, i, TRU, &(o->preconds) );
	replace_unary_var_occurences( unary, i, FAL, &(o2->preconds) );
	replace_unary_var_occurences( unary, i, TRU, &(o->conditionals) );
	replace_unary_var_occurences( unary, i, FAL, &(o2->conditionals) );
	continue;
      }
      i++;
    }
    o = o->next;
  }

  if ( gcmd_line.display_info == 101 ) {
    printf("\ngoal state with unaries encoded is:\n");
    print_CodeNode( gcode_goal_state, 0 );
    printf("\noperators with unaries encoded are:");
    for ( o = gcode_operators; o; o = o->next ) {
      print_CodeOperator( o );
    }
  }

}



void encode_unaries_under_CodeNode( CodeNode **n )

{

  CodeNode *son1, *son2, *tmp;
  int old_type;
  ArgArray a;
  Integers *tt, *ttmp;
  short int unary;

  if ( !(*n) ) {
    return;
  }

  encode_unaries_under_CodeNode( &((*n)->sons) );
  encode_unaries_under_CodeNode( &((*n)->next) );

  if ( (*n)->connective == ALL ) {

    if ( (unary = var_used_in_unary_under( (*n)->var, (*n)->sons )) != -1 ) {
      son1 = new_CodeNode( ALL );
      son2 = new_CodeNode( ALL );
      tmp = deep_copy_CodeTree( (*n)->sons );
      son1->sons = (*n)->sons;
      son2->sons = tmp;
      son1->var = (*n)->var;
      son2->var = (*n)->var;
      old_type = (*n)->var_type;
      (*n)->connective = AND;
      (*n)->var = 0;
      (*n)->var_type = 0;
      (*n)->sons = son1;
      son1->next = son2;
      son2->next = NULL;
      if ( gtypes_table_size == MAX_TYPES_TABLE ) {
	printf("\ntoo many types (+inferred types)! increase MAX_TYPES_TABLE (currently %d)\n\n",
	       MAX_TYPES_TABLE);
	exit( 1 );
      }
      gtypes_table[gtypes_table_size].name = gnew_types_name;
      gtypes_table[gtypes_table_size].integers = NULL;
      for ( tt = gtypes_table[old_type].integers; tt; tt = tt->next ) {
	a[0] = tt->index;
	if ( possibly_positive( unary, a ) ) {
	  ttmp = new_integers( tt->index );
	  ttmp->next = gtypes_table[gtypes_table_size].integers;
	  gtypes_table[gtypes_table_size].integers = ttmp;
	}
      }
      son1->var_type = gtypes_table_size;
      gtypes_table_size++;
      
      if ( gtypes_table_size == MAX_TYPES_TABLE ) {
	printf("\ntoo many types (+inferred types)! increase MAX_TYPES_TABLE (currently %d)\n\n",
	       MAX_TYPES_TABLE);
	exit( 1 );
      }
      gtypes_table[gtypes_table_size].name = gnew_types_name;
      gtypes_table[gtypes_table_size].integers = NULL;
      for ( tt = gtypes_table[old_type].integers; tt; tt = tt->next ) {
	a[0] = tt->index;
	if ( !possibly_positive( unary, a ) ) {
	  ttmp = new_integers( tt->index );
	  ttmp->next = gtypes_table[gtypes_table_size].integers;
	  gtypes_table[gtypes_table_size].integers = ttmp;
	}
      }
      son2->var_type = gtypes_table_size;
      gtypes_table_size++;

      replace_unary_var_occurences( unary, son1->var, TRU, &(son1->sons) );
      replace_unary_var_occurences( unary, son2->var, FAL, &(son2->sons) );
      
      encode_unaries_under_CodeNode( &(son1) );
      encode_unaries_under_CodeNode( &(son2) );
    }

  }


  if ( (*n)->connective == EX ) {

    if ( (unary = var_used_in_unary_under( (*n)->var, (*n)->sons )) != -1 ) {
      son1 = new_CodeNode( EX );
      son2 = new_CodeNode( EX );
      tmp = deep_copy_CodeTree( (*n)->sons );
      son1->sons = (*n)->sons;
      son2->sons = tmp;
      son1->var = (*n)->var;
      son2->var = (*n)->var;
      old_type = (*n)->var_type;
      (*n)->connective = OR;
      (*n)->var = 0;
      (*n)->var_type = 0;
      (*n)->sons = son1;
      son1->next = son2;
      son2->next = NULL;
      if ( gtypes_table_size == MAX_TYPES_TABLE ) {
	printf("\ntoo many types (+inferred types)! increase MAX_TYPES_TABLE (currently %d)\n\n",
	       MAX_TYPES_TABLE);
	exit( 1 );
      }
      gtypes_table[gtypes_table_size].name = gnew_types_name;
      gtypes_table[gtypes_table_size].integers = NULL;
      for ( tt = gtypes_table[old_type].integers; tt; tt = tt->next ) {
	a[0] = tt->index;
	if ( possibly_positive( unary, a ) ) {
	  ttmp = new_integers( tt->index );
	  ttmp->next = gtypes_table[gtypes_table_size].integers;
	  gtypes_table[gtypes_table_size].integers = ttmp;
	}
      }
      son1->var_type = gtypes_table_size;
      gtypes_table_size++;
      
      if ( gtypes_table_size == MAX_TYPES_TABLE ) {
	printf("\ntoo many types (+inferred types)! increase MAX_TYPES_TABLE (currently %d)\n\n",
	       MAX_TYPES_TABLE);
	exit( 1 );
      }
      gtypes_table[gtypes_table_size].name = gnew_types_name;
      gtypes_table[gtypes_table_size].integers = NULL;
      for ( tt = gtypes_table[old_type].integers; tt; tt = tt->next ) {
	a[0] = tt->index;
	if ( !possibly_positive( unary, a ) ) {
	  ttmp = new_integers( tt->index );
	  ttmp->next = gtypes_table[gtypes_table_size].integers;
	  gtypes_table[gtypes_table_size].integers = ttmp;
	}
      }
      son2->var_type = gtypes_table_size;
      gtypes_table_size++;

      replace_unary_var_occurences( unary, son1->var, TRU, &(son1->sons) );
      replace_unary_var_occurences( unary, son2->var, FAL, &(son2->sons) );
      
      encode_unaries_under_CodeNode( &(son1) );
      encode_unaries_under_CodeNode( &(son2) );
    }

  }

}



int var_used_in_unary_under( int var, CodeNode *n )

{

  short int unary;

  if ( !n ) {
    return -1;
  }

  if ( (unary = var_used_in_unary_under( var, n->next )) != -1 ) {
    return unary;
  }
  if ( (unary = var_used_in_unary_under( var, n->sons )) != -1 ) {
    return unary;
  }

  if ( n->connective != ATOM ||
       n->predicate == -1 ) {
    return -1;
  }

  if ( garity[n->predicate] == 1 &&
       n->arguments[0] == ((-1)*var)-1 &&
       !gis_added[n->predicate] &&
       !gis_deleted[n->predicate] ) {
    return n->predicate;
  } else {
    return -1;
  }

}



void replace_unary_var_occurences( short int unary, int var, 
				   Connective val, CodeNode **n )

{

  if ( !(*n) ) {
    return;
  }

  replace_unary_var_occurences( unary, var, val, &((*n)->next) );
  replace_unary_var_occurences( unary, var, val, &((*n)->sons) );

  if ( (*n)->connective != ATOM ) {
    return;
  }

  if ( (*n)->predicate == unary &&
       (*n)->arguments[0] == ((-1)*var)-1 ) {
    (*n)->connective = val;
    free_CodeNode( (*n)->sons );
    (*n)->sons = NULL;
  }

}



void collect_all_effect_prefix_paths( CodeNode *n )

{

  ltmpI = NULL;

  recursive_collect_all_effect_prefix_paths( n, NULL );

}



void recursive_collect_all_effect_prefix_paths( CodeNode *n, 
						CodeNode *history )

{

  CodeNode *tmp, *nn, *new_history, *new_eff, *new_when;

  if ( n->connective == ALL ) {
    tmp = new_CodeNode( ALL );
    tmp->var = n->var;
    tmp->var_type = n->var_type;
    /* don't know if that's necessary
     *
     * doesn't matter, as it's on domain description level
     */
    new_history = deep_copy_CodeTree( history );
    if ( !new_history ) {
      new_history = tmp;
    } else {
      for ( nn = new_history; nn->sons; nn = nn->sons );
      nn->sons = tmp;
    }
    recursive_collect_all_effect_prefix_paths( n->sons, new_history );
    free_CodeNode( new_history );
    return;
  }

  if ( n->connective == AND ) {
    if ( !(n->sons) ||
	 !(n->sons->next) ||
	 n->sons->next->next ) {
      printf("\neffect prefix AND node not binary ...!?\n\n");
      exit( 1 );
    }
    recursive_collect_all_effect_prefix_paths( n->sons, history );
    recursive_collect_all_effect_prefix_paths( n->sons->next, history );
    return;
  }

  if ( n->connective == WHEN ) {
    new_history = deep_copy_CodeTree( history );
    new_when = new_CodeNode( WHEN );
    new_eff = deep_copy_CodeTree( n->sons );
    new_when->sons = new_eff;
    if ( !new_history ) {
      new_history = new_when;
    } else {
      for ( nn = new_history; nn->sons; nn = nn->sons );
      nn->sons = new_when;
    }
    new_history->next = ltmpI;
    ltmpI = new_history;
    return;
  }

  printf("\nnon AND, ALL, WHEN in binary prefix tree ...?!\n\n");
  exit( 1 );

}



