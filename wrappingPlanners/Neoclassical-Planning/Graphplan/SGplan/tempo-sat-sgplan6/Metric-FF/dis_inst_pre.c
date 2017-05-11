/*********************************************************************

 * (C) Copyright 2008, University of Illinois, Urbana-Champaign

 *

 * All rights reserved. Use of this software is permitted ONLY for

 * non-commercial research purposes, and it may be copied only

 * for that use only. All copies must include this copyright message.

 * This software is made available AS IS, and neither the authors

 * nor the University of Illinois, make any warranty about the

 * software or its performance.

 *

 *********************************************************************/
/********************************************************************

 * File: dis_inst_pre.c 

 * Description: modified from inst_pre.c in Metric-FF

 *

 * Authors:  Chih-wei Hsu and Benjamin W. Wah

 *

 *********************************************************************/
/*
# Copyright (C) 2004, Board of Trustees of the University of Illinois.
#
# The program is copyrighted by the University of Illinois, and should
# not be distributed without prior approval.  Commercialization of this 
# product requires prior licensing from the University of Illinois.
# Commercialization includes the integration of this code in part or 
# whole into a product for resale. 
#
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# Author: Y. X. Chen, C. W. Hsu, and B. W. Wah, University of Illinois
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
*/


/*
 * THIS SOURCE CODE IS SUPPLIED  ``AS IS'' WITHOUT WARRANTY OF ANY KIND, 
 * AND ITS AUTHOR AND THE JOURNAL OF ARTIFICIAL INTELLIGENCE RESEARCH 
 * (JAIR) AND JAIR'S PUBLISHERS AND DISTRIBUTORS, DISCLAIM ANY AND ALL 
 * WARRANTIES, INCLUDING BUT NOT LIMITED TO ANY IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE, AND
 * ANY WARRANTIES OR NON INFRINGEMENT.  THE USER ASSUMES ALL LIABILITY AND
 * RESPONSIBILITY FOR USE OF THIS SOURCE CODE, AND NEITHER THE AUTHOR NOR
 * JAIR, NOR JAIR'S PUBLISHERS AND DISTRIBUTORS, WILL BE LIABLE FOR 
 * DAMAGES OF ANY KIND RESULTING FROM ITS USE.  Without limiting the 
 * generality of the foregoing, neither the author, nor JAIR, nor JAIR's
 * publishers and distributors, warrant that the Source Code will be 
 * error-free, will operate without interruption, or will meet the needs 
 * of the user.
 */








/*********************************************************************
 * File: inst_pre.c
 * Description: functions for instantiating operators, preprocessing part.
 *              - transform domain into integers
 *              - inertia preprocessing:
 *                  - collect inertia info
 *                  - split initial state in special arrays
 *              - Wff normalization:
 *                  - simplification
 *                  - quantifier expansion
 *                  - NOT s down
 *              - negative preconditions translation
 *              - split operators into easy and hard to instantiate
 *
 *              - full DNF functions, only feasible for fully instantiated 
 *                formulae
 *
 * Author: Joerg Hoffmann 2000
 *
 *********************************************************************/ 









#include "dis_ff.h"
#include "lpg.h"

#include "dis_output.h"
#include "dis_memory.h"
#include "dis_parse.h"
#include "dis_expressions.h"

#include "dis_inst_pre.h"














extern void calc_invariants();
extern void print_out_invariant_result();

/*******************************************************
 * TRANSFdis_ORM DOMAIN INTO INTEGER (FACT) REPRESENTATION *
 *******************************************************/









char *dis_lvar_names[dis_MAX_VARS];
int dis_lvar_types[dis_MAX_VARS];








void dis_encode_domain_in_integers( void )

{

  int i,j, *type_count;

  dis_collect_all_strings();

  type_count = (int *) calloc(dis_gnum_types, sizeof(int));
  const_index = (int **) calloc(dis_gnum_constants, sizeof(int*));
  for (i=0;i<dis_gnum_constants;i++)
        const_index[i] = (int *) calloc(dis_gnum_types, sizeof(int));
   for (j=0;j<dis_gnum_types;j++)
   {
       type_count[j] = 0;
       for (i=0;i<dis_gnum_constants;i++)
           if (dis_gis_member[i][j] == dis_TRUE)
           {
               const_index[i][j] = type_count[j];
               type_count[j]++;
           }
   }
          
 /* 
  for (i=0;i<dis_gnum_constants;i++)
      for (j=2;j<dis_gnum_types;j++)
          if (dis_gis_member[i][j] == dis_TRUE)
          {
              const_index[i] = type_count[j];
              type_count[j]++; 
              break;
          }  */ 
  xfree(type_count);
  dis_goperators = (dis_Operator_pointer *) calloc(dis_MAX_OPERATdis_ORS, 
          sizeof(dis_Operator_pointer));
  dis_gtype_to_predicate = calloc(dis_MAX_PREDICATES, sizeof(int));
  
  
  if ( dis_gcmd_line.display_info == 103 ) {
    printf("\nconstant table:");
    for ( i = 0; i < dis_gnum_constants; i++ ) {
      printf("\n%d --> %s", i, dis_gconstants[i]);
    }

    printf("\n\ntypes table:");
    for ( i = 0; i < dis_gnum_types; i++ ) {
      printf("\n%d --> %s: ", i, dis_gtype_names[i]);
      for ( j = 0; j < dis_gtype_size[i]; j++ ) {
	printf("%d ", dis_gtype_consts[i][j]);
      }
    }

    printf("\n\npredicates table:");
    for ( i = 0; i < dis_gnum_predicates; i++ ) {
      printf("\n%3d --> %s: ", i, dis_gpredicates[i]);
      for ( j = 0; j < dis_garity[i]; j++ ) {
	printf("%s ", dis_gtype_names[dis_gpredicates_args_type[i][j]]);
      }
    }

    printf("\n\nfunctions table:");
    for ( i = 0; i < dis_gnum_functions; i++ ) {
      printf("\n%3d --> %s: ", i, dis_gfunctions[i]);
      for ( j = 0; j < dis_gf_arity[i]; j++ ) {
	printf("%s ", dis_gtype_names[dis_gfunctions_args_type[i][j]]);
      }
    }
    printf("\n\n");
  }

  dis_create_integer_representation();
    
  if ( dis_gcmd_line.display_info == 104 ) {
    printf("\n\nfirst step initial state is:");
    for ( i = 0; i < dis_gnum_full_initial; i++ ) {
      printf("\n");
      dis_print_dis_Fact( &(dis_gfull_initial[i]) );
    }
    printf("\n\nfirst step fluent initial state is:");
    for ( i = 0; i < dis_gnum_full_fluents_initial; i++ ) {
      printf("\n");
      dis_print_dis_Fluent( &(dis_gfull_fluents_initial[i].fluent) );
      printf(": %f", dis_gfull_fluents_initial[i].value);
    }
    
    printf("\n\nfirst step operators are:");
    for ( i = 0; i < dis_gnum_operators; i++ ) {
      dis_print_dis_Operator( dis_goperators[i] );
    }
    printf("\n\n");
    
    printf("\n\nfirst step goal is:\n");
    dis_print_Wff( dis_ggoal, 0 );
    fflush( stdout );

    printf("\n\nfirst step metric is: (normalized to minimize)\n");
    dis_print_dis_ExpNode( dis_gmetric );
    fflush( stdout );
  }

  if (dis_gnum_predicates > 1 && dis_gnum_types > 1)
  {
    calc_invariants();
//    print_out_invariant_result();    
  }
}



void dis_collect_all_strings( void )

{

  dis_FactList *f;
  dis_TokenList *t;
  int p_num, type_num, c_num, ar;
  int i;

  /* first are types and their objects. for = we make sure that there
   * is one type that contains all objects.
   */
  dis_gtype_names[0] = dis_copy_dis_Token("ARTIFICIAL-ALL-OBJECTS");
  dis_gtype_size[0] = 0;
  for ( i = 0; i < dis_MAX_CONSTANTS; i++ ) {
    dis_gis_member[i][0] = dis_FALSE;
  }
  dis_gnum_types = 1;

  for ( f = dis_gorig_constant_list; f; f = f->next ) {
    if ( (type_num = dis_position_in_types_table( f->item->next->item )) == -1 ) {
      if ( dis_gnum_types == dis_MAX_TYPES ) {
	printf("\ntoo many types! increase dis_MAX_TYPES (currently %d)\n\n",
	       dis_MAX_TYPES);
	exit( 1 );
      }
      dis_gtype_names[dis_gnum_types] = dis_new_dis_Token( strlen( f->item->next->item ) + 1 );
      strcpy( dis_gtype_names[dis_gnum_types], f->item->next->item );
      dis_gtype_size[dis_gnum_types] = 0;
      for ( i = 0; i < dis_MAX_CONSTANTS; i++ ) {
	dis_gis_member[i][dis_gnum_types] = dis_FALSE;
      }
      type_num = dis_gnum_types++;
    }

    if ( (c_num = dis_position_in_constants_table( f->item->item )) == -1 ) {
      if ( dis_gnum_constants == dis_MAX_CONSTANTS ) {
	printf("\ntoo many constants! increase dis_MAX_CONSTANTS (currently %d)\n\n",
	       dis_MAX_CONSTANTS);
	exit( 1 );
      }
      dis_gconstants[dis_gnum_constants] = dis_new_dis_Token( strlen( f->item->item ) + 1 );
      strcpy( dis_gconstants[dis_gnum_constants], f->item->item );
      c_num = dis_gnum_constants++;

      /* all constants into 0-type.
       */
      if ( dis_gtype_size[0] == dis_MAX_TYPE ) {
	printf("\ntoo many consts in type %s! increase dis_MAX_TYPE (currently %d)\n\n",
	       dis_gtype_names[0], dis_MAX_TYPE);
	exit( 1 );
      }     
      dis_gtype_consts[0][dis_gtype_size[0]++] = c_num;
      dis_gis_member[c_num][0] = dis_TRUE;
    }
    
    if ( !dis_gis_member[c_num][type_num] ) {
      if ( dis_gtype_size[type_num] == dis_MAX_TYPE ) {
	printf("\ntoo many consts in type %s! increase dis_MAX_TYPE (currently %d)\n\n",
	       dis_gtype_names[type_num], dis_MAX_TYPE);
	exit( 1 );
      }     
      dis_gtype_consts[type_num][dis_gtype_size[type_num]++] = c_num;
      dis_gis_member[c_num][type_num] = dis_TRUE;
    }
  }
  dis_add_types();

  /* next are predicates; first of all, create in-built predicate =
   */
  dis_gpredicates[0] = dis_copy_dis_Token("=");
  dis_gpredicates_args_type[0][0] = 0;/* all objects type */
  dis_gpredicates_args_type[0][1] = 0;
  dis_garity[0] = 2;
  dis_gnum_predicates = 1;

  for ( f = dis_gpredicates_and_types; f; f = f->next ) {
    if ( (p_num = dis_position_in_predicates_table( f->item->item )) != -1 ) {
      printf("\npredicate %s declared twice!\n\n", f->item->item);
      exit( 1 );
    }
    if ( dis_gnum_predicates == dis_MAX_PREDICATES ) {
      printf("\ntoo many predicates! increase dis_MAX_PREDICATES (currently %d)\n\n",
	     dis_MAX_PREDICATES);
      exit( 1 );
    }
    dis_gpredicates[dis_gnum_predicates] = dis_new_dis_Token( strlen( f->item->item ) + 1 );
    strcpy( dis_gpredicates[dis_gnum_predicates], f->item->item );
    ar = 0;
    for ( t = f->item->next; t; t = t->next ) {
      if ( (type_num = dis_position_in_types_table( t->item )) == -1 ) {
	printf("\npredicate %s is declared to use unknown or empty type %s\n\n", 
	       f->item->item, t->item);
	exit( 1 );
      }
      if ( ar == dis_MAX_ARITY ) {
	printf("\narity of %s to high! increase dis_MAX_ARITY (currently %d)\n\n",
	       dis_gpredicates[dis_gnum_predicates], dis_MAX_ARITY);
	exit( 1 );
      }
      dis_gpredicates_args_type[dis_gnum_predicates][ar++] = type_num;
    }
    dis_garity[dis_gnum_predicates++] = ar;
  }


  /* next are functions; first of all, create in-built function total-time
   * for sole use in metric
   */
  dis_gfunctions[0] = dis_copy_dis_Token("TOTAL-TIME");
  dis_gf_arity[0] = 0;
  if (GpG.is_durative)
  {
    dis_gfunctions[1] = dis_copy_dis_Token("DURATIONFUNCTION");
    dis_gf_arity[1] = 0;
    dis_gnum_functions = 2;
  }
  else
    dis_gnum_functions = 1;

  for ( f = dis_gfunctions_and_types; f; f = f->next ) {
    if ( (p_num = dis_position_in_functions_table( f->item->item )) != -1 ) {
      printf("\nfunction %s declared twice!\n\n", f->item->item);
      exit( 1 );
    }
    if ( dis_gnum_functions == dis_MAX_FUNCTIONS ) {
      printf("\ntoo many functions! increase dis_MAX_FUNCTIONS (currently %d)\n\n",
	     dis_MAX_FUNCTIONS);
      exit( 1 );
    }
    dis_gfunctions[dis_gnum_functions] = dis_new_dis_Token( strlen( f->item->item ) + 1 );
    strcpy( dis_gfunctions[dis_gnum_functions], f->item->item );
    ar = 0;
    for ( t = f->item->next; t; t = t->next ) {
      if ( (type_num = dis_position_in_types_table( t->item )) == -1 ) {
	printf("\nfunction %s is declared to use unknown or empty type %s\n\n", 
	       f->item->item, t->item);
	exit( 1 );
      }
      if ( ar == dis_MAX_ARITY ) {
	printf("\narity of %s to high! increase dis_MAX_ARITY (currently %d)\n\n",
	       dis_gfunctions[dis_gnum_functions], dis_MAX_ARITY);
	exit( 1 );
      }
      dis_gfunctions_args_type[dis_gnum_functions][ar++] = type_num;
    }
    dis_gf_arity[dis_gnum_functions++] = ar;
  }

  dis_free_dis_FactList( dis_gorig_constant_list );
  dis_free_dis_FactList( dis_gpredicates_and_types );
  dis_free_dis_FactList( dis_gfunctions_and_types );

}



int dis_position_in_types_table( char *str )

{

  int i;

  /* start at 1 because 0 is our artificial one
   */
  for ( i = 1; i < dis_gnum_types; i++ ) {
    if ( str == dis_gtype_names[i] || 
	 (strcmp( str, dis_gtype_names[i] ) == dis_SAME) ) {
      break;
    }
  }

  return ( i == dis_gnum_types ) ? -1 : i;

}



int dis_position_in_constants_table( char *str )

{

  int i;

  for ( i=0; i<dis_gnum_constants; i++ ) {
    if ( str == dis_gconstants[i] || 
	 strcmp( str, dis_gconstants[i] ) == dis_SAME ) {
      break;
    }
  }

  return ( i == dis_gnum_constants ) ? -1 : i;

}



int dis_position_in_predicates_table( char *str )

{

  int i;

  for ( i = 0; i < dis_gnum_predicates; i++ ) {
    if ( str == dis_gpredicates[i] || 
	 strcmp( str, dis_gpredicates[i] ) == dis_SAME ) {
      break;
    }
  }

  return ( i == dis_gnum_predicates ) ? -1 : i;

}



int dis_position_in_functions_table( char *str )

{

  int i;

  for ( i=0; i<dis_gnum_functions; i++ ) {
    if ( str == dis_gfunctions[i] || 
	 strcmp( str, dis_gfunctions[i] ) == dis_SAME ) {
      break;
    }
  }

  return ( i == dis_gnum_functions ) ? -1 : i;

}



void dis_create_integer_representation( void )

{

  dis_PlNode *n, *nn;
  dis_Pldis_Operator *o;
  dis_Operator *tmp;
  dis_FactList *ff;
  int type_num, i = 0, sum;
  dis_ExpNode *t;
  
  /* durative actions delete*/
  float v;
  int j = 0, k;
  dis_TokenList *arg;
	table = (DuraTab *) malloc(sizeof(DuraTab)*gnum_das);

  if ( dis_gorig_initial_facts ) {
    sum = 0;
    for ( n = dis_gorig_initial_facts->sons; n; n = n->next ) sum++;
    sum += dis_gnum_constants;/* space for equalities */
    dis_gfull_initial = ( dis_Fact * ) calloc( sum, sizeof( dis_Fact ) );
    dis_gfull_fluents_initial = ( dis_FluentValue * ) 
      calloc( sum, sizeof( dis_FluentValue ));

	for ( n = dis_gorig_initial_facts->sons; n; n = n->next ) {
      if ( n->connective == dis_ATOM ) {
	dis_make_dis_Fact( &(dis_gfull_initial[dis_gnum_full_initial]), n, 0 );
	if ( dis_gfull_initial[dis_gnum_full_initial].predicate == 0 ) {
	  printf("\nequality in initial state! check input files.\n\n");
	  exit( 1 );
	}
	dis_gnum_full_initial++;
      } else
      if (n->connective != dis_TRU)
      {
	/* a fluent value assignment
	 */
	dis_make_dis_Fluent( &(dis_gfull_fluents_initial[dis_gnum_full_fluents_initial].fluent), 
		     n->lh->atom, 0 );
	dis_gfull_fluents_initial[dis_gnum_full_fluents_initial].value =
	  ( float ) strtod( n->rh->atom->item, NULL);
	dis_gnum_full_fluents_initial++;
      }
    }
    dis_free_dis_PlNode( dis_gorig_initial_facts );
  }

  /* now insert all our artificial equality constraints into initial state.
   */
  for ( i = 0; i < dis_gnum_constants; i++ ) {
    dis_gfull_initial[dis_gnum_full_initial].predicate = 0;
    dis_gfull_initial[dis_gnum_full_initial].args[0] = i;
    dis_gfull_initial[dis_gnum_full_initial].args[1] = i;
    dis_gnum_full_initial++;
  }
  /* FINITO. the rest of equality handling will fully
   * automatically be done by the rest of the machinery.
   */

  dis_ggoal = dis_make_Wff( dis_gorig_goal_facts, 0 );

  if ( dis_gparse_metric != NULL ) {
    if ( !dis_gcmd_line.optimize&&0 ) {
      if ( dis_gcmd_line.display_info ) {
	printf("\n\nno optimization required. skipping criterion.\n\n");
      }
    } else {
      dis_gmetric = dis_make_dis_ExpNode( dis_gparse_metric, 0 );
      if ( strcmp( dis_gparse_optimization, "MINIMIZE" ) != dis_SAME &&
	   strcmp( dis_gparse_optimization, "MAXIMIZE" ) != dis_SAME ) {
	if ( dis_gcmd_line.display_info ) {
	  printf("\n\nunknown optimization method %s. check input files\n\n", 
		 dis_gparse_optimization);
	}
	exit( 1 );
      }
      if ( strcmp( dis_gparse_optimization, "MAXIMIZE" ) == dis_SAME ) {
	t = dis_new_dis_ExpNode( MINUS );
	t->son = dis_gmetric;
	dis_gmetric = t;
      }
    }
  }

  for ( o = dis_gloaded_ops; o; o = o->next ) {
    tmp = dis_new_dis_Operator( o->name, o->number_of_real_params );

    for ( ff = o->params; ff; ff = ff->next ) {
      if ( (type_num = dis_position_in_types_table( ff->item->next->item )) == -1 ) {
	printf("\nwarning: parameter %s of op %s has unknown or empty type %s. skipping op",
	       ff->item->item, o->name, ff->item->next->item);
	break;
      }
      if ( tmp->num_vars == dis_MAX_VARS ) {
	printf("\ntoo many parameters! increase dis_MAX_VARS (currently %d)\n\n",
	       dis_MAX_VARS);
	exit( 1 );
      }
      for ( i = 0; i < tmp->num_vars; i++ ) {
	if ( tmp->var_names[i] == ff->item->item ||
	     strcmp( tmp->var_names[i], ff->item->item ) == dis_SAME ) {
	  printf("\nwarning: operator %s parameter %s overwrites previous declaration\n\n",
		 tmp->name, ff->item->item);
	}
      }
      tmp->var_names[tmp->num_vars] = dis_new_dis_Token( strlen( ff->item->item ) + 1 );
      strcpy( tmp->var_names[tmp->num_vars], ff->item->item );
      tmp->var_types[tmp->num_vars++] = type_num;
    }
    if ( ff ) {
      dis_free_dis_Operator( tmp );
      continue;
    }

    for ( i = 0; i < tmp->num_vars; i++ ) {
      dis_lvar_types[i] = tmp->var_types[i];
      dis_lvar_names[i] = tmp->var_names[i];
    }

	/* durative actions */
  if (GpG.SearchModal != -2)
    if (o->duration)
    {
      strcpy(table[j].name, o->name);
      if (o->duration->rh->connective == NUMBER)
      {
        v = atof(o->duration->rh->atom->item);
	memcpy(table[j].fluent.args, &v, sizeof(v));
	table[j].fluent.function = -1;
      }
      else
	if (o->duration->rh->connective == FHEAD)
	{
          table[j].fluent.function = dis_position_in_functions_table
          (o->duration->rh->atom->item);
          k = 0;
          for (arg=o->duration->rh->atom->next;arg;arg=arg->next)
          {
            for (i=0;i<tmp->num_vars;i++)
              if (strcmp(arg->item, tmp->var_names[i]) == dis_SAME)
                break;
            table[j].fluent.args[k++] = i;
          }
          table[j].flag = 0;
        }
        else
          if (GpG.SearchModal == -1000 || GpG.SearchModal == -1006)
          {
            table[j].fluent.function = dis_position_in_functions_table
            (o->duration->rh->rightson->atom->item);
            k = 0;
            for (arg=o->duration->rh->rightson->atom->next;arg;arg=arg->next)
            {
              for (i=0;i<tmp->num_vars;i++)
                if (strcmp(arg->item, tmp->var_names[i]) == dis_SAME)
                  break;
              table[j].fluent.args[k++] = i;
            }
            if (GpG.SearchModal == -1000)
              table[j].flag = 1;
            else	
              table[j].flag = 0;
          }
          else
            table[j].fluent.function = -1;
      j++;				 
    }
	/*
        else
	{
		v = 0;
		memcpy(table[j].fluent.args, &v, sizeof(v));
		table[j].fluent.function = -1;
	}*/
    
    tmp->preconds = dis_make_Wff( o->preconds, tmp->num_vars );

    if ( o->effects ) {
      /* in dis_make_effect, make sure that no one afects equality.
       */
      nn = o->effects->sons;
      while ( nn &&
	      (tmp->effects = dis_make_effect( nn, tmp->num_vars )) == NULL ) {
	nn = nn->next;
      }
      if ( nn ) {
	for ( n = nn->next; n; n = n->next ) {
	  if ( (tmp->effects->prev = dis_make_effect( n, tmp->num_vars )) == NULL ) {
	    continue;
	  }
	  tmp->effects->prev->next = tmp->effects;
	  tmp->effects = tmp->effects->prev;
	}
      }
    }

    if ( dis_gnum_operators == dis_MAX_OPERATdis_ORS ) {
      printf("\ntoo many operators! increase dis_MAX_OPERATdis_ORS (currently %d)\n\n",
	     dis_MAX_OPERATdis_ORS);
      exit( 1 );
    }
    dis_goperators[dis_gnum_operators++] = tmp;
  }

  if ( 0 ) {
    /* currently not in use; leads to free memory reads and
     * free memory frees (no memory leaks), which are hard to explain.
     *
     * almost no memory consumption anyway.
     */
    dis_free_dis_Pldis_Operator( dis_gloaded_ops );
  }

	/* timed initial literals */
	dis_gtils = (dis_TimedInitial *) realloc(dis_gtils, sizeof(dis_TimedInitial)*dis_gnum_tils);
	for (i=0;i<dis_gnum_tils;i++)
		dis_make_dis_Fact(&(dis_gtils[i].pred), dis_gtils[i].literal, 0);

}



void dis_make_dis_Fact( dis_Fact *f, dis_PlNode *n, int num_vars )

{

  int m, i;
  dis_TokenList *t;
  
  if ( !n->atom ) {
    /* can't happen after previous syntax check. Oh well, whatever...
     */
    printf("\nillegal (empty) atom used in domain. check input files\n\n");
    exit( 1 );
  }

  f->predicate = dis_position_in_predicates_table( n->atom->item );
  if ( f->predicate == -1 ) {
    printf("\nundeclared predicate %s used in domain definition\n\n",
	   n->atom->item);
    exit( 1 );
  }
  m = 0;
  for ( t = n->atom->next; t; t = t->next ) {
    if ( t->item[0] == '?' ) {
      for ( i=num_vars-1; i>-1; i-- ) {
	/* downwards, to always get most recent declaration/quantification
	 * of that variable
	 */
	if ( dis_lvar_names[i] == t->item ||
	     strcmp( dis_lvar_names[i], t->item ) == dis_SAME ) {
	  break;
	}
      }
      if ( i == -1 ) {
	printf("\nundeclared variable %s in literal %s. check input files\n\n",
	       t->item, n->atom->item);
	exit( 1 );
      }
      if ( dis_lvar_types[i] != dis_gpredicates_args_type[f->predicate][m] &&
	   !dis_is_subtype( dis_lvar_types[i], dis_gpredicates_args_type[f->predicate][m] ) ) {
	printf("\ntype of var %s does not match type of arg %d of predicate %s\n\n",
	       dis_lvar_names[i], m, dis_gpredicates[f->predicate]);
	exit( 1 );
      }
      f->args[m] = dis_ENCODE_VAR( i );
    } else {
      if ( (f->args[m] = 
	    dis_position_in_constants_table( t->item )) == -1 ) {
	printf("\nunknown constant %s in literal %s. check input files\n\n",
	       t->item, n->atom->item);
	exit( 1 );
      }
      if ( !dis_gis_member[f->args[m]][dis_gpredicates_args_type[f->predicate][m]] ) {
	printf("\ntype mismatch: constant %s as arg %d of %s. check input files\n\n",
	       dis_gconstants[f->args[m]], m, dis_gpredicates[f->predicate]);
	exit( 1 );
      }
    }
    m++;
  }
  if ( m != dis_garity[f->predicate] ) {
    printf("\npredicate %s is declared to have %d (not %d) arguments. check input files\n\n",
	   dis_gpredicates[f->predicate],
	   dis_garity[f->predicate], m);
    exit( 1 );
  }

}



void dis_make_dis_Fluent( dis_Fluent *f, dis_TokenList *atom, int num_vars )

{

  int m, i;
  dis_TokenList *t;

  if ( !atom ) {
    /* can't happen after previous syntax check. Oh well, whatever...
     */
    printf("\nillegal (empty) atom used in domain. check input files\n\n");
    exit( 1 );
  }

  f->function = dis_position_in_functions_table( atom->item );
  if ( f->function == -1 ) {
    if (atom->item[0] != '_')
    printf("\nundeclared function %s used in domain definition\n\n",
	   atom->item);
    return;
    exit( 1 );
  }

  m = 0;
  for ( t = atom->next; t; t = t->next ) {
    if ( t->item[0] == '?' ) {
      for ( i=num_vars-1; i>-1; i-- ) {
	/* downwards, to always get most recent declaration/quantification
	 * of that variable
	 */
	if ( dis_lvar_names[i] == t->item ||
	     strcmp( dis_lvar_names[i], t->item ) == dis_SAME ) {
	  break;
	}
      }
      if ( i == -1 ) {
	printf("\nundeclared variable %s in function %s. check input files\n\n",
	       t->item, atom->item);
	exit( 1 );
      }
      if ( dis_lvar_types[i] != dis_gfunctions_args_type[f->function][m] &&
	   !dis_is_subtype( dis_lvar_types[i], dis_gfunctions_args_type[f->function][m] ) ) {
	printf("\ntype of var %s does not match type of arg %d of function %s\n\n",
	       dis_lvar_names[i], m, dis_gfunctions[f->function]);
	exit( 1 );
      }
      f->args[m] = dis_ENCODE_VAR( i );
    } else {
      if ( (f->args[m] = 
	    dis_position_in_constants_table( t->item )) == -1 ) {
	printf("\nunknown constant %s in function %s. check input files\n\n",
	       t->item, atom->item);
	exit( 1 );
      }
      if ( !dis_gis_member[f->args[m]][dis_gfunctions_args_type[f->function][m]] ) {
	printf("\ntype mismatch: constant %s as arg %d of %s. check input files\n\n",
	       dis_gconstants[f->args[m]], m, dis_gfunctions[f->function]);
	exit( 1 );
      }
    }
    m++;
  }

  if ( m != dis_gf_arity[f->function] ) {
    printf("\nfunction %s is declared to have %d (not %d) arguments. check input files\n\n",
	   dis_gfunctions[f->function],
	   dis_gf_arity[f->function], m);
    exit( 1 );
  }

}



dis_Bool dis_is_subtype( int t1, int t2 )

{

  int i;

  for ( i = 0; i < dis_gtype_size[t1]; i++ ) {
    if ( !dis_gis_member[dis_gtype_consts[t1][i]][t2] ) {
      return dis_FALSE;
    }
  }

  return dis_TRUE;

}



dis_WffNode *dis_make_Wff( dis_PlNode *p, int num_vars )

{

  dis_WffNode *tmp;
  int i, t;
  dis_PlNode *n;

  if ( !p ) {
    tmp = NULL;
    return tmp;
  }

  tmp = dis_new_dis_WffNode( p->connective );
  switch ( p->connective ) {
  case dis_ALL:
  case dis_EX:
    for ( i = 0; i < num_vars; i++ ) {
      if ( dis_lvar_names[i] == p->atom->item ||
	   strcmp( dis_lvar_names[i], p->atom->item ) == dis_SAME ) {
	printf("\nwarning: var quantification of %s overwrites previous declaration\n\n",
	       p->atom->item);
      }
    }
    if ( (t = dis_position_in_types_table( p->atom->next->item )) == -1 ) {
      printf("\nwarning: quantified var %s has unknown or empty type %s. simplifying.\n\n",
	     p->atom->item, p->atom->next->item);
      tmp->connective = ( p->connective == dis_EX ) ? dis_FAL : dis_TRU;
      break;
    }
    tmp->var = num_vars;
    tmp->var_type = t;
    tmp->var_name = dis_new_dis_Token( strlen( p->atom->item ) + 1 );
    strcpy( tmp->var_name, p->atom->item );
    dis_lvar_names[num_vars] = p->atom->item;
    dis_lvar_types[num_vars] = t;
    tmp->son = dis_make_Wff( p->sons, num_vars + 1 );
    break;
  case dis_AND:
  case dis_OR:
    if ( !p->sons ) {
      printf("\nwarning: empty con/disjunction in domain definition. simplifying.\n\n");
      tmp->connective = ( p->connective == dis_OR ) ? dis_FAL : dis_TRU;
      break;
    }
    tmp->sons = dis_make_Wff( p->sons, num_vars );
    for ( n = p->sons->next; n; n = n->next ) {
      tmp->sons->prev = dis_make_Wff( n, num_vars );
      tmp->sons->prev->next = tmp->sons;
      tmp->sons = tmp->sons->prev;
    }
    break;
  case dis_NOT:
    tmp->son = dis_make_Wff( p->sons, num_vars );
    break;
  case dis_ATOM:
    tmp->fact = dis_new_dis_Fact();
    dis_make_dis_Fact( tmp->fact, p, num_vars );
    break;
  case dis_TRU:
  case dis_FAL:
    break;
  case dis_COMP:
    tmp->comp = p->comp;
    tmp->lh = dis_make_dis_ExpNode( p->lh, num_vars );
    tmp->rh = dis_make_dis_ExpNode( p->rh, num_vars );
    break;
  default:
    printf("\nforbidden connective %d in Pl Wff. must be a bug somewhere...\n\n",
	   p->connective);
    exit( 1 );
  }

  return tmp;

}



dis_ExpNode *dis_make_dis_ExpNode( dis_Parsedis_ExpNode *p, int num_vars )

{

  dis_ExpNode *tmp;
  char temp[128];

  if ( !p ) {
    tmp = NULL;
    return tmp;
  }

  tmp = dis_new_dis_ExpNode( p->connective );
  switch ( p->connective ) {
  case AD:
  case SU:
  case MU:
  case DI:
    tmp->leftson = dis_make_dis_ExpNode( p->leftson, num_vars );
    tmp->rightson = dis_make_dis_ExpNode( p->rightson, num_vars );
    break;
  case MINUS:
    tmp->son = dis_make_dis_ExpNode( p->leftson, num_vars );
    break;
  case NUMBER:
    tmp->value = ( float ) strtod( p->atom->item, NULL );
    break;
  case FHEAD:
    tmp->fluent = dis_new_dis_Fluent();
    dis_make_dis_Fluent( tmp->fluent, p->atom, num_vars );
    break;
  case VIO:
    tmp->fluent = dis_new_dis_Fluent();
    sprintf(temp, "_%s_VIO", p->atom->item);
    xfree(p->atom->item);
    p->atom->item = dis_copy_dis_Token(temp);
    dis_make_dis_Fluent( tmp->fluent, p->atom, num_vars );
    if (tmp->fluent->function == -1)
    {
      tmp->connective = NUMBER;
      tmp->value = 0;
    }
    else
      tmp->connective = FHEAD;
    break;
  default:
    printf("\n\nmake expnode: wrong specifier %d",
	   p->connective);
    exit( 1 );
  }

  return tmp;

}



dis_Effect *dis_make_effect( dis_PlNode *p, int num_vars )

{

  dis_Effect *tmp = dis_new_dis_Effect();
  dis_PlNode *n, *m;
  int t, i;

  for ( n = p; n && n->connective == dis_ALL; n = n->sons ) { 
    if ( (t = dis_position_in_types_table( n->atom->next->item )) == -1 ) {
      printf("\nwarning: effect parameter %s has unknown or empty type %s. skipping effect.\n\n", 
	     n->atom->item, n->atom->next->item); 
      return NULL; 
    } 
    for ( i = 0; i < num_vars + tmp->num_vars; i++ ) {
      if ( dis_lvar_names[i] == n->atom->item ||
	   strcmp( dis_lvar_names[i], n->atom->item ) == dis_SAME ) {
	printf("\nwarning: effect parameter %s overwrites previous declaration\n\n",
	       n->atom->item);
      }
    }
    dis_lvar_types[num_vars + tmp->num_vars] = t; 
    dis_lvar_names[num_vars + tmp->num_vars] = n->atom->item; 
    tmp->var_names[tmp->num_vars] = dis_new_dis_Token( strlen( n->atom->item ) + 1 ); 
    strcpy( tmp->var_names[tmp->num_vars], n->atom->item );
    tmp->var_types[tmp->num_vars++] = t; 
  }

  if ( !n || n->connective != dis_WHEN ) {
    printf("\nnon dis_WHEN %d at end of effect parameters. debug me\n\n",
	   n->connective);
    exit( 1 );
  }

  tmp->conditions = dis_make_Wff( n->sons, num_vars + tmp->num_vars );

  if ( n->sons->next->connective != dis_AND ) {
    printf("\nnon dis_AND %d in front of literal effect list. debug me\n\n",
	   n->sons->next->connective);
    exit( 1 );
  }
  if ( !n->sons->next->sons ) {
    return tmp;
  }
  for ( m = n->sons->next->sons; m; m = m->next ) {
    if ( m->connective == dis_NEF ) {
      if ( tmp->numeric_effects != NULL ) {
	tmp->numeric_effects->prev = dis_new_dis_Numericdis_Effect();
	dis_make_dis_Fluent( &(tmp->numeric_effects->prev->fluent),
		     m->lh->atom, num_vars + tmp->num_vars );
	tmp->numeric_effects->prev->neft = m->neft;
	tmp->numeric_effects->prev->rh = dis_make_dis_ExpNode( m->rh, num_vars + tmp->num_vars );

	tmp->numeric_effects->prev->next = tmp->numeric_effects;
	tmp->numeric_effects = tmp->numeric_effects->prev;
      } else {
	tmp->numeric_effects = dis_new_dis_Numericdis_Effect();
	dis_make_dis_Fluent( &(tmp->numeric_effects->fluent),
		     m->lh->atom, num_vars + tmp->num_vars );
	tmp->numeric_effects->neft = m->neft;
	tmp->numeric_effects->rh = dis_make_dis_ExpNode( m->rh, num_vars + tmp->num_vars );
      }
    } else {
      if ( tmp->effects != NULL ) {
	tmp->effects->prev = dis_new_dis_Literal();
	if ( m->connective == dis_NOT ) {
	  tmp->effects->prev->negated = dis_TRUE;
	  dis_make_dis_Fact( &(tmp->effects->prev->fact), m->sons, num_vars + tmp->num_vars );
	  if ( (tmp->effects->prev->fact).predicate == 0 ) {
	    printf("\n\nequality in effect! check input files!\n\n");
	    exit( 1 );
	  }
	} else {
	  tmp->effects->prev->negated = dis_FALSE;
	  dis_make_dis_Fact( &(tmp->effects->prev->fact), m, num_vars + tmp->num_vars );
	  if ( (tmp->effects->prev->fact).predicate == 0 ) {
	    printf("\n\nequality in effect! check input files!\n\n");
	    exit( 1 );
	  }
	}
	tmp->effects->prev->next = tmp->effects;
	tmp->effects = tmp->effects->prev;
      } else {
	tmp->effects = dis_new_dis_Literal();
	if ( m->connective == dis_NOT ) {
	  tmp->effects->negated = dis_TRUE;
	  dis_make_dis_Fact( &(tmp->effects->fact), m->sons, num_vars + tmp->num_vars );
	  if ( (tmp->effects->fact).predicate == 0 ) {
	    printf("\n\nequality in effect! check input files!\n\n");
	    exit( 1 );
	  }
	} else {
	  tmp->effects->negated = dis_FALSE;
	  dis_make_dis_Fact( &(tmp->effects->fact), m, num_vars + tmp->num_vars );
	  if ( (tmp->effects->fact).predicate == 0 ) {
	    printf("\n\nequality in effect! check input files!\n\n");
	    exit( 1 );
	  }
	}
      }
    }
  }

  return tmp;

}











/*************************
 * INERTIA PREPROCESSING *
 *************************/











void dis_do_inertia_preprocessing_step_1( void )

{

  int i, j;
  dis_Facts *f;
  dis_FluentValues *ff;
  

  dis_collect_inertia_information();

  if ( dis_gcmd_line.display_info == 105 ) {
    printf("\n\npredicates inertia info:");
    for ( i = 0; i < dis_gnum_predicates; i++ ) {
      printf("\n%3d --> %s: ", i, dis_gpredicates[i]);
      printf(" is %s, %s",
	     dis_gis_added[i] ? "ADDED" : "NOT ADDED",
	     dis_gis_deleted[i] ? "DELETED" : "NOT DELETED");
    }
    printf("\n\nfunctions inertia info:");
    for ( i = 0; i < dis_gnum_functions; i++ ) {
      printf("\n%3d --> %s: ", i, dis_gfunctions[i]);
      printf(" is %s",
	     dis_gis_changed[i] ? "CHANGED" : "NOT CHANGED");
    }
    printf("\n\n");
  }

  dis_split_initial_state();

  if ( dis_gcmd_line.display_info == 106 ) {
    printf("\n\nsplitted initial state is:");
    printf("\nindividual predicates:");
    for ( i = 0; i < dis_gnum_predicates; i++ ) {
      printf("\n\n%s:", dis_gpredicates[i]);
      if ( !dis_gis_added[i] &&
	   !dis_gis_deleted[i] ) {
	printf(" ---  STATIC");
      }
      for ( j = 0; j < dis_gnum_initial_predicate[i]; j++ ) {
	printf("\n");
	dis_print_dis_Fact( &(dis_ginitial_predicate[i][j]) );
      }
    }
    printf("\n\nnon static part:");
    for ( f = dis_ginitial; f; f = f->next ) {
      printf("\n");
      dis_print_dis_Fact( f->fact );
    }

    printf("\n\nextended types table:");
    for ( i = 0; i < dis_gnum_types; i++ ) {
      printf("\n%d --> ", i);
      if ( dis_gpredicate_to_type[i] == -1 ) {
	printf("%s ", dis_gtype_names[i]);
      } else {
	printf("UNARY INERTIA TYPE (%s) ", dis_gpredicates[dis_gpredicate_to_type[i]]);
      }
      for ( j = 0; j < dis_gtype_size[i]; j++ ) {
	printf("%d ", dis_gtype_consts[i][j]);
      }
    }

    printf("\nindividual functions:");
    for ( i = 0; i < dis_gnum_functions; i++ ) {
      printf("\n\n%s:", dis_gfunctions[i]);
      if ( !dis_gis_changed[i] ) {
	printf(" ---  STATIC");
      }
      for ( j = 0; j < dis_gnum_initial_function[i]; j++ ) {
	printf("\n");
	dis_print_dis_Fluent( &(dis_ginitial_function[i][j].fluent) );
	printf(": %f", dis_ginitial_function[i][j].value);
     }
    }
    printf("\n\nnon static part:");
    for ( ff = dis_gf_initial; ff; ff = ff->next ) {
      printf("\n");
      dis_print_dis_Fluent( &(ff->fluent) );
      printf(": %f", ff->value);
    }
  }

}



void dis_collect_inertia_information( void )

{

  int i;
  dis_Effect *e;
  dis_Literal *l;
  dis_Numericdis_Effect *ne;

  for ( i = 0; i < dis_gnum_predicates; i++ ) {
    if (dis_gpredicates[i][0] == '_')
    {
      dis_gis_added[i] = dis_TRUE;
      dis_gis_deleted[i] = dis_TRUE;
    }
    else
    {
      dis_gis_added[i] = dis_FALSE;
      dis_gis_deleted[i] = dis_FALSE;
    }
  }
  for ( i = 0; i < dis_gnum_functions; i++ ) {
    if (dis_gfunctions[i][0] == '_' || (i == 1 && GpG.is_durative) || i == 0)
      dis_gis_changed[i] = dis_TRUE;
    else
      dis_gis_changed[i] = dis_FALSE;
  }

  for ( i = 0; i < dis_gnum_operators; i++ ) {
    for ( e = dis_goperators[i]->effects; e; e = e->next ) {
      for ( l = e->effects; l; l = l->next ) {
	if ( l->negated ) {
	  dis_gis_deleted[l->fact.predicate] = dis_TRUE;
	} else {
	  dis_gis_added[l->fact.predicate] = dis_TRUE;
	}
      }
      for ( ne = e->numeric_effects; ne; ne = ne->next ) {
	dis_gis_changed[ne->fluent.function] = dis_TRUE;
      }
    }
  }

}



void dis_split_initial_state( void )

{

  int i, j, p, t;
  dis_Facts *tmp;
  dis_FluentValues *ftmp;
    
  //for ( i = 0; i < dis_MAX_PREDICATES; i++ ) {
  for ( i = 0; i < dis_gnum_predicates; i++ ) {
      dis_gtype_to_predicate[i] = -1;
  }
  for ( i = 0; i < dis_MAX_TYPES; i++ ) {
    dis_gpredicate_to_type[i] = -1;
  }

  for ( i = 0; i < dis_gnum_predicates; i++ ) {
    if ( !dis_gis_added[i] &&
	 !dis_gis_deleted[i] &&
	 dis_garity[i] == 1 ) {
      if ( dis_gnum_types == dis_MAX_TYPES ) {
	printf("\ntoo many (inferred) types! increase dis_MAX_TYPES (currently %d)\n\n",
	       dis_MAX_TYPES);
	exit( 1 );
      }
      dis_gtype_to_predicate[i] = dis_gnum_types;
      dis_gpredicate_to_type[dis_gnum_types] = i;
      dis_gtype_names[dis_gnum_types] = NULL;
      dis_gtype_size[dis_gnum_types] = 0;
      for ( j = 0; j < dis_MAX_CONSTANTS; j++ ) {
	dis_gis_member[j][dis_gnum_types] = dis_FALSE;
      }
      dis_gnum_types++;
    }
  }
     

  /* double size of predicates table as each predicate might need
   * to be translated to dis_NOT-p
   */
  dis_ginitial_predicate = ( dis_Fact ** ) calloc( dis_gnum_predicates * 2, sizeof( dis_Fact * ) );
  dis_gnum_initial_predicate = ( int * ) calloc( dis_gnum_predicates * 2, sizeof( int ) );
  for ( i = 0; i < dis_gnum_predicates * 2; i++ ) {
    dis_gnum_initial_predicate[i] = 0;
  }
  for ( i = 0; i < dis_gnum_full_initial; i++ ) {
    p = dis_gfull_initial[i].predicate;
    dis_gnum_initial_predicate[p]++;
  }
  for ( i = 0; i < dis_gnum_predicates; i++ ) {
    dis_ginitial_predicate[i] = ( dis_Fact * ) calloc( dis_gnum_initial_predicate[i], sizeof( dis_Fact ) );
    dis_gnum_initial_predicate[i] = 0;
  }
  dis_ginitial = NULL;
  dis_gnum_initial = 0;

  for ( i = 0; i < dis_gnum_full_initial; i++ ) {
    p = dis_gfull_initial[i].predicate;
    dis_ginitial_predicate[p][dis_gnum_initial_predicate[p]].predicate = p;
    for ( j = 0; j < dis_garity[p]; j++ ) {
      dis_ginitial_predicate[p][dis_gnum_initial_predicate[p]].args[j] = dis_gfull_initial[i].args[j];
    }
    dis_gnum_initial_predicate[p]++;
    if ( dis_gis_added[p] ||
	 dis_gis_deleted[p] ) {
      tmp = dis_new_dis_Facts();
      tmp->fact->predicate = p;
      for ( j = 0; j < dis_garity[p]; j++ ) {
	tmp->fact->args[j] = dis_gfull_initial[i].args[j];
      }
      tmp->next = dis_ginitial;
      dis_ginitial = tmp;
      dis_gnum_initial++;
    } else {
      if ( dis_garity[p] == 1 ) {
	t = dis_gtype_to_predicate[p];
	if ( dis_gtype_size[t] == dis_MAX_TYPE ) {
	  printf("\ntoo many consts in type %s! increase dis_MAX_TYPE (currently %d)\n\n",
		 dis_gtype_names[t], dis_MAX_TYPE);
	  exit( 1 );
	}
	if ( !dis_gis_member[dis_gfull_initial[i].args[0]][dis_gpredicates_args_type[p][0]] ) {
	  printf("\ntype mismatch in initial state! %s as arg 0 of %s\n\n",
		 dis_gconstants[dis_gfull_initial[i].args[0]], dis_gpredicates[p]);
	  exit( 1 );
	}
	dis_gtype_consts[t][dis_gtype_size[t]++] = dis_gfull_initial[i].args[0];
	dis_gis_member[dis_gfull_initial[i].args[0]][t] = dis_TRUE;
      }
    }
  }

  dis_ginitial_function = ( dis_FluentValue ** ) 
    calloc( dis_gnum_functions, sizeof( dis_FluentValue * ) );
  dis_gnum_initial_function = ( int * ) calloc( dis_gnum_functions, sizeof( int ) );
  for ( i = 0; i < dis_gnum_functions; i++ ) {
    dis_gnum_initial_function[i] = 0;
  }
  for ( i = 0; i < dis_gnum_full_fluents_initial; i++ ) {
    p = dis_gfull_fluents_initial[i].fluent.function;
    dis_gnum_initial_function[p]++;
  }
  for ( i = 0; i < dis_gnum_functions; i++ ) {
    dis_ginitial_function[i] = ( dis_FluentValue * ) 
      calloc( dis_gnum_initial_function[i], sizeof( dis_FluentValue ) );
    dis_gnum_initial_function[i] = 0;
  }
  dis_gf_initial = NULL;
  dis_gnum_f_initial = 0;

  for ( i = 0; i < dis_gnum_full_fluents_initial; i++ ) {
    p = dis_gfull_fluents_initial[i].fluent.function;
    dis_ginitial_function[p][dis_gnum_initial_function[p]].fluent.function = p;
    for ( j = 0; j < dis_gf_arity[p]; j++ ) {
      dis_ginitial_function[p][dis_gnum_initial_function[p]].fluent.args[j] = 
	dis_gfull_fluents_initial[i].fluent.args[j];
    }
    dis_ginitial_function[p][dis_gnum_initial_function[p]].value =
      dis_gfull_fluents_initial[i].value;
    dis_gnum_initial_function[p]++;
    if ( dis_gis_changed[p] ) {
      ftmp = dis_dis_new_dis_FluentValues();
      ftmp->fluent.function = p;
      for ( j = 0; j < dis_gf_arity[p]; j++ ) {
	ftmp->fluent.args[j] = dis_gfull_fluents_initial[i].fluent.args[j];
      }
      ftmp->value = dis_gfull_fluents_initial[i].value;
      ftmp->next = dis_gf_initial;
      dis_gf_initial = ftmp;
      dis_gnum_f_initial++;
    }
  }

}











/******************************
 * Ndis_ORMALIZE dis_ALL PL1 Fdis_ORMULAE *
 ******************************/












void dis_normalize_all_wffs( void )

{

  int i;
  dis_Effect *e;

  dis_simplify_wff( &dis_ggoal );
  dis_remove_unused_vars_in_wff( &dis_ggoal );
  dis_expand_quantifiers_in_wff( &dis_ggoal, -1, -1 );
  dis_NOTs_down_in_wff( &dis_ggoal );
  dis_cleanup_wff( &dis_ggoal );

/*  if ( dis_ggoal->connective == dis_TRU ) {
    printf("\nff: goal can be simplified to dis_TRUE. The empty plan solves it\n\n");
    dis_gnum_plan_ops = 0;
    dis_print_official_result();
    exit( 1 );
  }*/
  if ( dis_ggoal->connective == dis_FAL ) {
    printf("\nnormalize ff: goal can be simplified to dis_FALSE. No plan will solve it\n\n");
    exit( 1 );
  }

  /* put goal into DNF right away: fully instantiated already
   */
  dis_dnf( &dis_ggoal );
  dis_cleanup_wff( &dis_ggoal );

  /* all we can do here is simplify if that's possible.
   */
  if ( dis_gmetric != NULL ) {
    dis_simplify_exp( &dis_gmetric );
  }


  for ( i = 0; i < dis_gnum_operators; i++ ) {
    dis_simplify_wff( &(dis_goperators[i]->preconds) );
    dis_remove_unused_vars_in_wff( &(dis_goperators[i]->preconds) );
    dis_expand_quantifiers_in_wff( &(dis_goperators[i]->preconds), -1, -1 );
    dis_NOTs_down_in_wff( &(dis_goperators[i]->preconds) );
    dis_cleanup_wff( &(dis_goperators[i]->preconds) );

    for ( e = dis_goperators[i]->effects; e; e = e->next ) {
      dis_simplify_wff( &(e->conditions) );
      dis_remove_unused_vars_in_wff( &(e->conditions) );
      dis_expand_quantifiers_in_wff( &(e->conditions), -1, -1 );
      dis_NOTs_down_in_wff( &(e->conditions) );
      dis_cleanup_wff( &(e->conditions) );
    }
  }

  if ( dis_gcmd_line.display_info == 107 ) {
    printf("\n\ndomain with normalized PL1 formula:");

    printf("\n\noperators are:");
    for ( i = 0; i < dis_gnum_operators; i++ ) {
      dis_print_dis_Operator( dis_goperators[i] );
    }
    printf("\n\n");

    printf("\n\ngoal is:\n");
    dis_print_Wff( dis_ggoal, 0 );

    if ( dis_gmetric ) {
      printf("\n\nmetric is (minimize):\n");
      dis_print_dis_ExpNode( dis_gmetric );
    } else {
      printf("\n\nmetric: none, i.e. plan length\n");
    }
  }

}



void dis_remove_unused_vars_in_wff( dis_WffNode **w )

{

  dis_WffNode *tmp;
  dis_WffNode *i;

  switch ( (*w)->connective ) {
  case dis_ALL:
  case dis_EX:
    dis_remove_unused_vars_in_wff( &((*w)->son) );
    if ( !dis_var_used_in_wff( dis_ENCODE_VAR( (*w)->var ), (*w)->son ) ) {
      dis_decrement_inferior_vars((*w)->var, (*w)->son );
      (*w)->connective = (*w)->son->connective;
      (*w)->var = (*w)->son->var;
      (*w)->var_type = (*w)->son->var_type;
      if ( (*w)->var_name ) {
	free( (*w)->var_name );
      }
      (*w)->var_name = (*w)->son->var_name;
      (*w)->sons = (*w)->son->sons;
      if ( (*w)->fact ) {
	free( (*w)->fact );
      }
      (*w)->fact = (*w)->son->fact;
      (*w)->comp = (*w)->son->comp;
      if ( (*w)->lh ) dis_free_dis_ExpNode( (*w)->lh );
      if ( (*w)->rh ) dis_free_dis_ExpNode( (*w)->rh );
      (*w)->lh = (*w)->son->lh;
      (*w)->rh = (*w)->son->rh;

      tmp = (*w)->son;
      (*w)->son = (*w)->son->son;
      xfree( tmp );
    }
    break;
  case dis_AND:
  case dis_OR:
    for ( i = (*w)->sons; i; i = i->next ) {
      dis_remove_unused_vars_in_wff( &i );
    }
    break;
  case dis_NOT:
    dis_remove_unused_vars_in_wff( &((*w)->son) );
    break;
  case dis_COMP:
  case dis_ATOM:
  case dis_TRU:
  case dis_FAL:
    break;
  default:
    printf("\nwon't get here: remove var, non logical %d\n\n",
	   (*w)->connective);
    exit( 1 );
  }

}



dis_Bool dis_var_used_in_wff( int code_var, dis_WffNode *w )

{

  dis_WffNode *i;
  int j;

  switch ( w->connective ) {
  case dis_ALL:
  case dis_EX:
    return dis_var_used_in_wff( code_var, w->son );
  case dis_AND:
  case dis_OR:
    for ( i = w->sons; i; i = i->next ) {
      if ( dis_var_used_in_wff( code_var, i ) ) {
	return dis_TRUE;
      }
    }
    return dis_FALSE;
  case dis_NOT:
    return dis_var_used_in_wff( code_var, w->son );
  case dis_ATOM:
    for ( j = 0; j < dis_garity[w->fact->predicate]; j++ ) {
      if ( w->fact->args[j] >= 0 ) {
	continue;
      }
      if ( w->fact->args[j] == code_var ) {
	return dis_TRUE;
      }
    }
    return dis_FALSE;
  case dis_COMP:
    if ( dis_var_used_in_exp( code_var, w->lh ) ) {
      return dis_TRUE;
    }
    if ( dis_var_used_in_exp( code_var, w->rh ) ) {
      return dis_TRUE;
    }
    return dis_FALSE;
  case dis_TRU:
  case dis_FAL:
    return dis_FALSE;
  default:
    printf("\nwon't get here: var used ?, non logical %d\n\n",
	   w->connective);
    exit( 1 );
  }


}



dis_Bool dis_var_used_in_exp( int code_var, dis_ExpNode *n )

{

  int i;

  switch ( n->connective ) {
  case AD:
  case SU:
  case MU:
  case DI:
    if ( dis_var_used_in_exp( code_var, n->leftson ) ||
	 dis_var_used_in_exp( code_var, n->rightson ) ) {
      return dis_TRUE;
    }
    return dis_FALSE;
  case MINUS:
    if ( dis_var_used_in_exp( code_var, n->son ) ) {
      return dis_TRUE;
    }
    return dis_FALSE;
  case NUMBER:
    return dis_FALSE;
  case FHEAD:
    if ( n->fluent ) {
      for ( i = 0; i < dis_gf_arity[n->fluent->function]; i++ ) {
	if ( n->fluent->args[i] >= 0 ) {
	  continue;
	}
	if ( n->fluent->args[i] == code_var ) {
	  return dis_TRUE;
	}
      }
    } else {
      /* in the case that this is called from ahead, where fluents
       * have been replaced with their identifiers
       */
    }
    return dis_FALSE;
  default:
    printf("\n\nvar used in expnode: wrong specifier %d",
	   n->connective);
    exit( 1 );
  }

}



void dis_decrement_inferior_vars( int var, dis_WffNode *w )

{

  dis_WffNode *i;
  int j;

  switch ( w->connective ) {
  case dis_ALL:
  case dis_EX:
    w->var--;
    dis_decrement_inferior_vars( var, w->son );
    break;
  case dis_AND:
  case dis_OR:
    for ( i = w->sons; i; i = i->next ) {
      dis_decrement_inferior_vars( var, i );
    }
    break;
  case dis_NOT:
    dis_decrement_inferior_vars( var, w->son );
    break;
  case dis_ATOM:
    for ( j = 0; j < dis_garity[w->fact->predicate]; j++ ) {
      if ( w->fact->args[j] >= 0 ) {
	continue;
      }
      if ( dis_DECODE_VAR( w->fact->args[j] ) > var ) {
	w->fact->args[j]++;
      }
    }
    break;
  case dis_COMP:
    dis_decrement_inferior_vars_in_exp( var, w->lh );
    dis_decrement_inferior_vars_in_exp( var, w->rh );
    break;
  case dis_TRU:
  case dis_FAL:
    break;
  default:
    printf("\nwon't get here: decrement, non logical %d\n\n",
	   w->connective);
    exit( 1 );
  }

}



void dis_decrement_inferior_vars_in_exp( int var, dis_ExpNode *n )

{

  int j;

  switch ( n->connective ) {
  case AD:
  case SU:
  case MU:
  case DI:
    dis_decrement_inferior_vars_in_exp( var, n->leftson );
    dis_decrement_inferior_vars_in_exp( var, n->rightson );
    break;
  case MINUS:
    dis_decrement_inferior_vars_in_exp( var, n->son );
    break;
  case NUMBER:
    break;
  case FHEAD:
    if ( n->fluent ) {
      for ( j = 0; j < dis_gf_arity[n->fluent->function]; j++ ) {
	if ( n->fluent->args[j] >= 0 ) {
	  continue;
	}
	if ( dis_DECODE_VAR( n->fluent->args[j] ) > var ) {
	  n->fluent->args[j]++;
	}
      }
    } else {
      /* in the case that this is called from ahead, where fluents
       * have been replaced with their identifiers
       */
    }
    break;
  default:
    printf("\n\ndecr inf vars in expnode: wrong specifier %d",
	   n->connective);
    exit( 1 );
  }

}



void dis_simplify_wff( dis_WffNode **w )

{

  dis_WffNode *i, *tmp;
  int m;
  dis_Bool ct;

  switch ( (*w)->connective ) {
  case dis_ALL:
  case dis_EX:
    dis_simplify_wff( &((*w)->son) );
    if ( (*w)->son->connective == dis_TRU ||
	 (*w)->son->connective == dis_FAL ) {
      (*w)->connective = (*w)->son->connective;
      if ((*w)->son)
      free( (*w)->son );
      (*w)->son = NULL;
      if ( (*w)->var_name ) {
	free( (*w)->var_name );
      }
    }
    break;
  case dis_AND:
    m = 0;
    i = (*w)->sons;
    while ( i ) {
      dis_simplify_wff( &i );
      if ( i->connective == dis_FAL ) {
	(*w)->connective = dis_FAL;
	dis_free_dis_WffNode( (*w)->sons );
	(*w)->sons = NULL;
	return;
      }
      if ( i->connective == dis_TRU ) {
	if ( i->prev ) {
	  i->prev->next = i->next;
	} else {
	  (*w)->sons = i->next;
	}
	if ( i->next ) {
	  i->next->prev = i->prev;
	}
	tmp = i;
	i = i->next;
	xfree( tmp );
	continue;
      }
      i = i->next;
      m++;
    }
    if ( m == 0 ) {
      (*w)->connective = dis_TRU;
      dis_free_dis_WffNode( (*w)->sons );
      (*w)->sons = NULL;
    }
    if ( m == 1 ) {
      (*w)->connective = (*w)->sons->connective;
      (*w)->var = (*w)->sons->var;
      (*w)->var_type = (*w)->sons->var_type;
      if ( (*w)->var_name ) {
	free( (*w)->var_name );
      }
      (*w)->var_name = (*w)->sons->var_name;
      (*w)->son = (*w)->sons->son;
      if ( (*w)->fact ) {
	free( (*w)->fact );
      }
      (*w)->fact = (*w)->sons->fact;
      (*w)->comp = (*w)->sons->comp;
      if ( (*w)->lh ) dis_free_dis_ExpNode( (*w)->lh );
      if ( (*w)->rh ) dis_free_dis_ExpNode( (*w)->rh );
      (*w)->lh = (*w)->sons->lh;
      (*w)->rh = (*w)->sons->rh;

      tmp = (*w)->sons;
      (*w)->sons = (*w)->sons->sons;
      xfree( tmp );
    }
    break;
  case dis_OR:
    m = 0;
    i = (*w)->sons;
    while ( i ) {
      dis_simplify_wff( &i );
      if ( i->connective == dis_TRU ) {
	(*w)->connective = dis_TRU;
	dis_free_dis_WffNode( (*w)->sons );
	(*w)->sons = NULL;
	return;
      }
      if ( i->connective == dis_FAL ) {
	if ( i->prev ) {
	  i->prev->next = i->next;
	} else {
	  (*w)->sons = i->next;
	}
	if ( i->next ) {
	  i->next->prev = i->prev;
	}
	tmp = i;
	i = i->next;
	xfree( tmp );
	continue;
      }
      i = i->next;
      m++;
    }
    if ( m == 0 ) {
      (*w)->connective = dis_FAL;
      dis_free_dis_WffNode( (*w)->sons );
      (*w)->sons = NULL;
    }
    if ( m == 1 ) {
      (*w)->connective = (*w)->sons->connective;
      (*w)->var = (*w)->sons->var;
      (*w)->var_type = (*w)->sons->var_type;
      if ( (*w)->var_name ) {
	free( (*w)->var_name );
      }
      (*w)->var_name = (*w)->sons->var_name;
      (*w)->son = (*w)->sons->son;
      if ( (*w)->fact ) {
	free( (*w)->fact );
      }
      (*w)->fact = (*w)->sons->fact;
      (*w)->comp = (*w)->sons->comp;
      if ( (*w)->lh ) dis_free_dis_ExpNode( (*w)->lh );
      if ( (*w)->rh ) dis_free_dis_ExpNode( (*w)->rh );
      (*w)->lh = (*w)->sons->lh;
      (*w)->rh = (*w)->sons->rh;

      tmp = (*w)->sons;
      (*w)->sons = (*w)->sons->sons;
      xfree( tmp );
    }
    break;
  case dis_NOT:
    dis_simplify_wff( &((*w)->son) );
    if ( (*w)->son->connective == dis_TRU ||
	 (*w)->son->connective == dis_FAL ) {
      (*w)->connective = ( (*w)->son->connective == dis_TRU ) ? dis_FAL : dis_TRU;
      xfree( (*w)->son );
      (*w)->son = NULL;
    }
    break;
  case dis_ATOM:
    if ( (*w)->visited ) {
      /* already seen and not changed
       */
      break;
    }
    if ( !dis_possibly_negative( (*w)->fact ) ) {
      (*w)->connective = dis_TRU;
      xfree( (*w)->fact );
      (*w)->fact = NULL;
      break;
    }
    if ( !dis_possibly_positive( (*w)->fact ) ) {
      (*w)->connective = dis_FAL;
      xfree( (*w)->fact );
      (*w)->fact = NULL;
      break;
    }
    (*w)->visited = dis_TRUE;
    break;
  case dis_COMP:
    dis_simplify_exp( &((*w)->lh) );
    dis_simplify_exp( &((*w)->rh) );
    if ( (*w)->lh->connective != NUMBER ||
	 (*w)->rh->connective != NUMBER ) {
      /* logical simplification only possible if both parts are numbers
       */
      break;
    }
    ct = dis_number_comparison_holds( (*w)->comp, (*w)->lh->value, (*w)->rh->value );
    if ( ct ) {
      (*w)->connective = dis_TRU;
      dis_free_dis_ExpNode( (*w)->lh );
      (*w)->lh = NULL;
      dis_free_dis_ExpNode( (*w)->rh );
      (*w)->rh = NULL;
      (*w)->comp = -1;
      break;
    } else {
      (*w)->connective = dis_FAL;
      dis_free_dis_ExpNode( (*w)->lh );
      (*w)->lh = NULL;
      dis_free_dis_ExpNode( (*w)->rh );
      (*w)->rh = NULL;
      (*w)->comp = -1;
      break;
    }
    break;
  case dis_TRU:
  case dis_FAL:
    break;
  default:
    printf("\nwon't get here: simplify, non logical %d\n\n",
	   (*w)->connective);
    exit( 1 );
  }

}



void dis_simplify_exp( dis_ExpNode **n )

{

  int j, f, k;

  switch ( (*n)->connective ) {
  case AD:
    dis_simplify_exp( &((*n)->leftson) );
    dis_simplify_exp( &((*n)->rightson) );
    if ( (*n)->leftson->connective != NUMBER ||
	 (*n)->rightson->connective != NUMBER ) {
      break;
    }
    (*n)->connective = NUMBER;
    (*n)->value = (*n)->leftson->value + (*n)->rightson->value;
    dis_free_dis_ExpNode( (*n)->leftson );
    (*n)->leftson = NULL;
    dis_free_dis_ExpNode( (*n)->rightson );
    (*n)->rightson = NULL;
    break;
  case SU:
    dis_simplify_exp( &((*n)->leftson) );
    dis_simplify_exp( &((*n)->rightson) );
    if ( (*n)->leftson->connective != NUMBER ||
	 (*n)->rightson->connective != NUMBER ) {
      break;
    }
    (*n)->connective = NUMBER;
    (*n)->value = (*n)->leftson->value - (*n)->rightson->value;
    dis_free_dis_ExpNode( (*n)->leftson );
    (*n)->leftson = NULL;
    dis_free_dis_ExpNode( (*n)->rightson );
    (*n)->rightson = NULL;
    break;
  case MU:
    dis_simplify_exp( &((*n)->leftson) );
    dis_simplify_exp( &((*n)->rightson) );
    if ( (*n)->leftson->connective != NUMBER ||
	 (*n)->rightson->connective != NUMBER ) {
      break;
    }
    (*n)->connective = NUMBER;
    (*n)->value = (*n)->leftson->value * (*n)->rightson->value;
    dis_free_dis_ExpNode( (*n)->leftson );
    (*n)->leftson = NULL;
    dis_free_dis_ExpNode( (*n)->rightson );
    (*n)->rightson = NULL;
    break;
  case DI:
    dis_simplify_exp( &((*n)->leftson) );
    dis_simplify_exp( &((*n)->rightson) );
    if ( (*n)->leftson->connective != NUMBER ||
	 (*n)->rightson->connective != NUMBER ) {
      break;
    }
    if ( (*n)->rightson->value == 0 ) {
      /* kind of unclean: simply leave that in here;
       * we will later determine the right thing 
       * to do with it.
       */
      break;
    }
    (*n)->connective = NUMBER;
    (*n)->value = (*n)->leftson->value / (*n)->rightson->value;
    dis_free_dis_ExpNode( (*n)->leftson );
    (*n)->leftson = NULL;
    dis_free_dis_ExpNode( (*n)->rightson );
    (*n)->rightson = NULL;
    break;
  case MINUS:
    dis_simplify_exp( &((*n)->son) );
    if ( (*n)->son->connective != NUMBER ) break;
    (*n)->connective = NUMBER;
    (*n)->value = ((float) (-1)) * (*n)->son->value;
    dis_free_dis_ExpNode( (*n)->son );
    (*n)->son = NULL;
    break;    
  case NUMBER:
    break;
  case FHEAD:
    if ( !(*n)->fluent ) {
      /* in the case that this is called from ahead, where fluents
       * have been replaced with their identifiers
       */
      break;
    }
    f = (*n)->fluent->function;
    for ( j = 0; j < dis_gf_arity[f]; j++ ) {
      if ( (*n)->fluent->args[j] < 0 ) {
	break;
      }
    }
    if ( j < dis_gf_arity[f] ) {
      break;
    }
    /* we handle only the case where the fluent is fully instantiated,
     * static, and in the initial state.
     */
    if ( dis_gis_changed[f] ) break;
    for ( j = 0; j < dis_gnum_initial_function[f]; j++ ) {
      for ( k = 0; k < dis_gf_arity[f]; k++ ) {
	if ( dis_ginitial_function[f][j].fluent.args[k] !=
	     (*n)->fluent->args[k] ) break;
      }
      if ( k < dis_gf_arity[f] ) continue;
      (*n)->connective = NUMBER;
      (*n)->value = dis_ginitial_function[f][j].value;
      break;
    }
    break;
  default:
    printf("\n\nsimplify expnode: wrong specifier %d",
	   (*n)->connective);
    exit( 1 );
  }

}



void dis_expand_quantifiers_in_wff( dis_WffNode **w, int var, int constant )

{

  dis_WffNode *r = NULL, *tmp, *i;
  int j, l;
  dis_Bool change, ct;

  if ( !(*w) ) {
    return;
  }

  switch ( (*w)->connective ) {
  case dis_ALL:
  case dis_EX:
    if ( var != -1 ) {/* depth first: upper node is active */
      dis_expand_quantifiers_in_wff( &((*w)->son), var, constant );
      return;
    }

    (*w)->connective = ( (*w)->connective == dis_ALL ) ? dis_AND : dis_OR;
    for ( j = 0; j < dis_gtype_size[(*w)->var_type]; j++ ) {
      tmp = dis_copy_Wff( (*w)->son );
      dis_expand_quantifiers_in_wff( &tmp, (*w)->var, dis_gtype_consts[(*w)->var_type][j] );
      tmp->next = r;
      if ( r ) {
	r->prev = tmp;
      }
      r = tmp;
    }

    dis_free_dis_WffNode( (*w)->son );
    (*w)->sons = r;
    (*w)->var = -1;
    (*w)->var_type = -1;
    if ( (*w)->var_name ) {
      free( (*w)->var_name );
    }
    (*w)->var_name = NULL;

    /* now make all sons expand their quantifiers
     */
    for ( i = (*w)->sons; i; i = i->next ) {
      dis_expand_quantifiers_in_wff( &i, -1, -1 );
    }
    break;
  case dis_AND:
  case dis_OR:
    for ( i = (*w)->sons; i; i = i->next ) {
      dis_expand_quantifiers_in_wff( &i, var, constant );
    }
    break;
  case dis_NOT:
    dis_expand_quantifiers_in_wff( &((*w)->son), var, constant );
    break;
  case dis_ATOM:
    if ( var == -1 ) {
      break;
    }

    change = dis_FALSE;
    for ( l = 0; l < dis_garity[(*w)->fact->predicate]; l++ ) {
      if ( (*w)->fact->args[l] == dis_ENCODE_VAR( var ) ) {
	(*w)->fact->args[l] = constant;
	change = dis_TRUE;
      }
    }
    if ( !change && (*w)->visited ) {
      /* we did not change anything and we've already seen that node
       * --> it cant be simplified
       */
      break;
    }
    if ( !dis_possibly_negative( (*w)->fact ) ) {
      (*w)->connective = dis_TRU;
      xfree( (*w)->fact );
      (*w)->fact = NULL;
      break;
    }
    if ( !dis_possibly_positive( (*w)->fact ) ) {
      (*w)->connective = dis_FAL;
      xfree( (*w)->fact );
      (*w)->fact = NULL;
      break;
    }
    (*w)->visited = dis_TRUE;
    break;
  case dis_COMP:
    if ( var == -1 ) {
      break;
    }

    dis_replace_var_with_const_in_exp( &((*w)->lh), var, constant );
    dis_replace_var_with_const_in_exp( &((*w)->rh), var, constant );
    if ( (*w)->lh->connective != NUMBER ||
	 (*w)->rh->connective != NUMBER ) {
      /* logical simplification only possible if both parts are numbers
       */
      break;
    }
    ct = dis_number_comparison_holds( (*w)->comp, (*w)->lh->value, (*w)->rh->value );
    if ( ct ) {
      (*w)->connective = dis_TRU;
      dis_free_dis_ExpNode( (*w)->lh );
      (*w)->lh = NULL;
      dis_free_dis_ExpNode( (*w)->rh );
      (*w)->rh = NULL;
      (*w)->comp = -1;
      break;
    } else {
      (*w)->connective = dis_FAL;
      dis_free_dis_ExpNode( (*w)->lh );
      (*w)->lh = NULL;
      dis_free_dis_ExpNode( (*w)->rh );
      (*w)->rh = NULL;
      (*w)->comp = -1;
      break;
    }
    break;
  case dis_TRU:
  case dis_FAL:
    break;
  default:
    printf("\nwon't get here: expansion, non logical %d\n\n",
	   (*w)->connective);
    exit( 1 );
  }

}



void dis_replace_var_with_const_in_exp( dis_ExpNode **n, int var, int constant )

{

  int j, f, k;

  switch ( (*n)->connective ) {
  case AD:
    dis_replace_var_with_const_in_exp( &((*n)->leftson), var, constant );
    dis_replace_var_with_const_in_exp( &((*n)->rightson), var, constant );
    if ( (*n)->leftson->connective != NUMBER ||
	 (*n)->rightson->connective != NUMBER ) {
      break;
    }
    (*n)->connective = NUMBER;
    (*n)->value = (*n)->leftson->value + (*n)->rightson->value;
    dis_free_dis_ExpNode( (*n)->leftson );
    (*n)->leftson = NULL;
    dis_free_dis_ExpNode( (*n)->rightson );
    (*n)->rightson = NULL;
    break;
  case SU:
    dis_replace_var_with_const_in_exp( &((*n)->leftson), var, constant );
    dis_replace_var_with_const_in_exp( &((*n)->rightson), var, constant );
    if ( (*n)->leftson->connective != NUMBER ||
	 (*n)->rightson->connective != NUMBER ) {
      break;
    }
    (*n)->connective = NUMBER;
    (*n)->value = (*n)->leftson->value - (*n)->rightson->value;
    dis_free_dis_ExpNode( (*n)->leftson );
    (*n)->leftson = NULL;
    dis_free_dis_ExpNode( (*n)->rightson );
    (*n)->rightson = NULL;
    break;
  case MU:
    dis_replace_var_with_const_in_exp( &((*n)->leftson), var, constant );
    dis_replace_var_with_const_in_exp( &((*n)->rightson), var, constant );
    if ( (*n)->leftson->connective != NUMBER ||
	 (*n)->rightson->connective != NUMBER ) {
      break;
    }
    (*n)->connective = NUMBER;
    (*n)->value = (*n)->leftson->value * (*n)->rightson->value;
    dis_free_dis_ExpNode( (*n)->leftson );
    (*n)->leftson = NULL;
    dis_free_dis_ExpNode( (*n)->rightson );
    (*n)->rightson = NULL;
    break;
  case DI:
    dis_replace_var_with_const_in_exp( &((*n)->leftson), var, constant );
    dis_replace_var_with_const_in_exp( &((*n)->rightson), var, constant );
    if ( (*n)->leftson->connective != NUMBER ||
	 (*n)->rightson->connective != NUMBER ) {
      break;
    }
    if ( (*n)->rightson->value == 0 ) {
      /* kind of unclean: simply leave that in here;
       * we will later determine the right thing 
       * to do with it.
       */
      break;
    }
    (*n)->connective = NUMBER;
    (*n)->value = (*n)->leftson->value / (*n)->rightson->value;
    dis_free_dis_ExpNode( (*n)->leftson );
    (*n)->leftson = NULL;
    dis_free_dis_ExpNode( (*n)->rightson );
    (*n)->rightson = NULL;
    break;
  case MINUS:
    dis_replace_var_with_const_in_exp( &((*n)->son), var, constant );
    if ( (*n)->son->connective != NUMBER ) break;
    (*n)->connective = NUMBER;
    (*n)->value = ((float) (-1)) * (*n)->son->value;
    dis_free_dis_ExpNode( (*n)->son );
    (*n)->son = NULL;
    break;    
  case NUMBER:
    break;
  case FHEAD:
    if ( !(*n)->fluent ) {
      /* in the case that this is called from ahead, where fluents
       * have been replaced with their identifiers
       */
      break;
    }
    f = (*n)->fluent->function;
    for ( j = 0; j < dis_gf_arity[f]; j++ ) {
      if ( (*n)->fluent->args[j] == dis_ENCODE_VAR( var ) ) {
	(*n)->fluent->args[j] = constant;
      }
    }
    for ( j = 0; j < dis_gf_arity[f]; j++ ) {
      if ( (*n)->fluent->args[j] < 0 ) {
	break;
      }
    }
    if ( j < dis_gf_arity[f] ) {
      break;
    }
    /* we handle only the case where the fluent is fully instantiated,
     * static, and in the initial state.
     */
    if ( dis_gis_changed[f] ) break;
    for ( j = 0; j < dis_gnum_initial_function[f]; j++ ) {
      for ( k = 0; k < dis_gf_arity[f]; k++ ) {
	if ( dis_ginitial_function[f][j].fluent.args[k] !=
	     (*n)->fluent->args[k] ) break;
      }
      if ( k < dis_gf_arity[f] ) continue;
      (*n)->connective = NUMBER;
      (*n)->value = dis_ginitial_function[f][j].value;
      break;
    }
    break;
  default:
    printf("\n\nreplace var with const in expnode: wrong specifier %d",
	   (*n)->connective);
    exit( 1 );
  }

}



dis_WffNode *dis_copy_Wff( dis_WffNode *w )

{

  dis_WffNode *tmp, *tmp2, *i;
  int j;

  tmp = dis_new_dis_WffNode( w->connective );

  switch ( w->connective ) {
  case dis_ALL:
  case dis_EX:
    tmp->var = w->var;
    tmp->var_type = w->var_type;
    tmp->son = dis_copy_Wff( w->son );
    break;
  case dis_AND:
  case dis_OR:
    for ( i = w->sons; i; i = i->next ) {
      tmp2 = dis_copy_Wff( i );
      if ( tmp->sons ) {
	tmp->sons->prev = tmp2;
      }
      tmp2->next = tmp->sons;
      tmp->sons = tmp2;
    }
    break;
  case dis_NOT:
    tmp->son = dis_copy_Wff( w->son );
    break;
  case dis_ATOM:
    tmp->fact = dis_new_dis_Fact();
    tmp->fact->predicate = w->fact->predicate;
    for ( j = 0; j < dis_garity[w->fact->predicate]; j++ ) {
      tmp->fact->args[j] = w->fact->args[j];
    }
    tmp->visited = w->visited;
    break;
  case dis_COMP:
    tmp->comp = w->comp;
    tmp->lh = dis_copy_Exp( w->lh );
    tmp->rh = dis_copy_Exp( w->rh );
    break;
  case dis_TRU:
  case dis_FAL:
    break;
  default:
    printf("\nwon't get here: copy, non logical %d\n\n",
	   w->connective);
    exit( 1 );
  }

  return tmp;

}



dis_ExpNode *dis_copy_Exp( dis_ExpNode *n )

{

  dis_ExpNode *tmp;
  int i;

  tmp = dis_new_dis_ExpNode( n->connective );

  switch ( n->connective ) {
  case AD:
  case SU:
  case MU:
  case DI:
    tmp->leftson = dis_copy_Exp( n->leftson );
    tmp->rightson = dis_copy_Exp( n->rightson );
    break;
  case MINUS:
    tmp->son = dis_copy_Exp( n->son );
    break;
  case NUMBER:
    tmp->value = n->value;
    break;
  case FHEAD:
    if ( n->fluent ) {
      tmp->fluent = dis_new_dis_Fluent();
      tmp->fluent->function = n->fluent->function;
      for ( i = 0; i < dis_gf_arity[tmp->fluent->function]; i++ ) {
	tmp->fluent->args[i] = n->fluent->args[i];
      }
    } else {
      /* in the case that this is called from ahead, where fluents
       * have been replaced with their identifiers
       */
      tmp->fl = n->fl;
    }
    break;
  default:
    printf("\n\ncopy expnode: wrong specifier %d",
	   n->connective);
    exit( 1 );
  }

  return tmp;

}



dis_Bool dis_possibly_positive( dis_Fact *f )

{

  int i;

  if ( dis_gis_added[f->predicate] ) {
    return dis_TRUE;
  }

  for ( i = 0; i < dis_gnum_initial_predicate[f->predicate]; i++ ) {
    if ( dis_matches( f, &(dis_ginitial_predicate[f->predicate][i]) ) ) {
      return dis_TRUE;
    }
  }

  return dis_FALSE;

}



dis_Bool dis_possibly_negative( dis_Fact *f )

{

  int i;

  if ( dis_gis_deleted[f->predicate] ) {
    return dis_TRUE;
  }

  for ( i = 0; i < dis_garity[f->predicate]; i++ ) {
    if ( f->args[i] < 0 ) {
      return dis_TRUE;
    }
  }

  for ( i = 0; i < dis_gnum_initial_predicate[f->predicate]; i++ ) {
    if ( dis_matches( f, &(dis_ginitial_predicate[f->predicate][i]) ) ) {
      return dis_FALSE;
    }
  }

  return dis_TRUE;

}



dis_Bool dis_matches( dis_Fact *f1, dis_Fact *f2 )

{

  int i;

  for ( i = 0; i < dis_garity[f1->predicate]; i++ ) {
    if ( f1->args[i] >= 0 ) {
      if ( f2->args[i] >= 0 &&
	   f1->args[i] != f2->args[i] ) {
	return dis_FALSE;
      }
    }
  }

  return dis_TRUE;

}



void dis_cleanup_wff( dis_WffNode **w )

{

  dis_merge_dis_ANDs_and_dis_ORs_in_wff( w );
  dis_detect_tautologies_in_wff( w );
  dis_simplify_wff( w );
  dis_detect_tautologies_in_wff( w );
  dis_merge_dis_ANDs_and_dis_ORs_in_wff( w );

}



void dis_detect_tautologies_in_wff( dis_WffNode **w )

{

  dis_WffNode *i, *j, *tmp;

  switch ( (*w)->connective ) {
  case dis_ALL:
  case dis_EX:
    dis_detect_tautologies_in_wff( &((*w)->son) );
    break;
  case dis_AND:
  case dis_OR:
    for ( i = (*w)->sons; i; i = i->next ) {
      dis_detect_tautologies_in_wff( &i );
    }
    for ( i = (*w)->sons; i && i->next; i = i->next ) {
      j = i->next;
      while ( j ) {
	if ( dis_are_identical_dis_ATOMs( i, j ) ) {
	  j->prev->next = j->next;
	  if ( j->next ) {
	    j->next->prev = j->prev;
	  }
	  tmp = j;
	  j = j->next;
	  if ( tmp->fact ) {
	    free( tmp->fact );
	  }
	  xfree( tmp );
	  continue;
	}
	if ( i->connective == dis_NOT &&
	     dis_are_identical_dis_ATOMs( i->son, j ) ) {
	  (*w)->connective = ( (*w)->connective == dis_AND ) ? dis_FAL : dis_TRU;
	  dis_free_dis_WffNode( (*w)->son );
	  (*w)->son = NULL;
	  return;
	}
	if ( j->connective == dis_NOT &&
	     dis_are_identical_dis_ATOMs( i, j->son ) ) {
	  (*w)->connective = ( (*w)->connective == dis_AND ) ? dis_FAL : dis_TRU;
	  dis_free_dis_WffNode( (*w)->son );
	  (*w)->son = NULL;
	  return;
	}
	j = j->next;
      }
    }
    break;
  case dis_NOT:
    dis_detect_tautologies_in_wff( &((*w)->son) );
    break;
  case dis_ATOM:
  case dis_COMP:
  case dis_TRU:
  case dis_FAL:
    break;
  default:
    printf("\nwon't get here: tautologies, non logical %d\n\n",
	   (*w)->connective);
    exit( 1 );
  }

}



dis_Bool dis_are_identical_dis_ATOMs( dis_WffNode *w1, dis_WffNode *w2 )

{

  int i;

  if ( w1->connective != dis_ATOM ||
       w2->connective != dis_ATOM ) {
    return dis_FALSE;
  }

  if ( w1->fact->predicate != w2->fact->predicate ) {
    return dis_FALSE;
  }

  for ( i = 0; i < dis_garity[w1->fact->predicate]; i++ ) {
    if ( w1->fact->args[i] != w2->fact->args[i] ) {
      return dis_FALSE;
    }
  }

  return dis_TRUE;

}



void dis_merge_dis_ANDs_and_dis_ORs_in_wff( dis_WffNode **w )

{

  dis_WffNode *i, *j, *tmp;

  switch ( (*w)->connective ) {
  case dis_ALL:
  case dis_EX:
    dis_merge_dis_ANDs_and_dis_ORs_in_wff( &((*w)->son) );
    break;
  case dis_AND:
  case dis_OR:
    i = (*w)->sons;
    while ( i ) {
      dis_merge_dis_ANDs_and_dis_ORs_in_wff( &i );
      if ( i->connective == (*w)->connective ) {
	if ( !(i->sons) ) {
	  if ( i->next ) {
	    i->next->prev = i->prev;
	  }
	  if ( i->prev ) {
	    i->prev->next = i->next;
	  } else {
	    (*w)->sons = i->next;
	  }
	  tmp = i;
	  i = i->next;
	  xfree( tmp );
	  continue;
	}
	for ( j = i->sons; j->next; j = j->next );
	j->next = i->next;
	if ( i->next ) {
	  i->next->prev = j;
	}
	if ( i->prev ) {
	  i->prev->next = i->sons;
	  i->sons->prev = i->prev;
	} else {
	  (*w)->sons = i->sons;
	}
	tmp = i;
	i = i->next;
	xfree( tmp );
	continue;
      }
      i = i->next;
    }
    break;
  case dis_NOT:
    dis_merge_dis_ANDs_and_dis_ORs_in_wff( &((*w)->son) );
    break;
  case dis_COMP:
  case dis_ATOM:
  case dis_TRU:
  case dis_FAL:
    break;
  default:
    printf("\nwon't get here: merge, non logical %d\n\n",
	   (*w)->connective);
    exit( 1 );
  }

}



void dis_NOTs_down_in_wff( dis_WffNode **w )

{

  dis_WffNode *tmp1, *tmp2, *i;

  switch ( (*w)->connective ) {
  case dis_ALL:
  case dis_EX:
    printf("\ntrying to put nots down in quantified formula! debug me\n\n");
    exit( 1 );
    break;
  case dis_AND:
  case dis_OR:
    for ( i = (*w)->sons; i; i = i->next ) {
      dis_NOTs_down_in_wff( &i ); 
    }
    break;
  case dis_NOT:
    if ( (*w)->son->connective == dis_NOT ) {
      (*w)->connective = (*w)->son->son->connective;
      (*w)->fact = (*w)->son->son->fact;
      (*w)->comp = (*w)->son->son->comp;
      (*w)->lh = (*w)->son->son->lh;
      (*w)->rh = (*w)->son->son->rh;
      tmp1 = (*w)->son;
      tmp2 = (*w)->son->son;
      (*w)->son = (*w)->son->son->son;
      /* don't need to remember (*w)->son->son->next: this is empty because
       * otherwise the resp. father, (*w)->son, would have been an
       * dis_AND or dis_OR
       */
      xfree( tmp1 );
      xfree( tmp2 );
      dis_NOTs_down_in_wff( w );
      break;
    }
    if ( (*w)->son->connective == dis_AND ||
	 (*w)->son->connective == dis_OR ) {
      (*w)->connective = ( (*w)->son->connective == dis_AND ) ? dis_OR : dis_AND;
      (*w)->sons = (*w)->son->sons;
      xfree( (*w)->son );
      (*w)->son = NULL;
      for ( i = (*w)->sons; i; i = i->next ) {
	tmp1 = dis_new_dis_WffNode( i->connective );
	tmp1->son = i->son;
	tmp1->sons = i->sons;
	tmp1->fact = i->fact;
	tmp1->comp = i->comp;
	tmp1->lh = i->lh;
	tmp1->rh = i->rh;
	i->connective = dis_NOT;
	i->son = tmp1;
	i->sons = NULL;
	i->fact = NULL;
	i->comp = -1;
	i->lh = NULL;
	i->rh = NULL;
	dis_NOTs_down_in_wff( &i );
      }
      break;
    }
    if ( (*w)->son->connective == dis_COMP ) {
      if ( (*w)->son->comp != EQ ) {
	(*w)->connective = dis_COMP;
	(*w)->lh = (*w)->son->lh;
	(*w)->rh = (*w)->son->rh;
	switch ( (*w)->son->comp ) {
	case LE:
	  (*w)->comp = GEQ;
	  break;
	case LEQ:
	  (*w)->comp = GE;
	  break;
	case GEQ:
	  (*w)->comp = LE;
	  break;
	case GE:
	  (*w)->comp = LEQ;
	  break;
	default:
	  printf("\n\nillegal comparator not EQ %d in nots down", 
		 (*w)->son->comp);
	  exit( 1 );
	}
	xfree( (*w)->son );
	(*w)->son = NULL;
      } else {
	(*w)->connective = dis_OR;
	(*w)->sons = (*w)->son;
	(*w)->son = NULL;
	(*w)->sons->comp = LE;
	tmp1 = dis_new_dis_WffNode( dis_COMP );
	tmp1->lh = dis_copy_Exp( (*w)->sons->lh );
	tmp1->rh = dis_copy_Exp( (*w)->sons->rh );
	tmp1->comp = GE;
	tmp1->prev = (*w)->sons;
	(*w)->sons->next = tmp1;
      }
    }
    break;
  case dis_COMP:
  case dis_ATOM:
  case dis_TRU:
  case dis_FAL:
    break;
  default:
    printf("\nwon't get here: nots down, non logical %d\n\n",
	   (*w)->connective);
    exit( 1 );
  }


}











/****************************************************
 * NEGATIVE PRE- dis_AND EFFECT- CONDITIONS TRANSLATION *
 ****************************************************/








int lconsts[dis_MAX_ARITY];








void dis_translate_negative_preconds( void )

{

  int i, j;
  dis_Effect *e;
  dis_Facts *f;
  dis_FluentValues *ff;

  while ( dis_translate_one_negative_cond( dis_ggoal ) );
  
  for ( i = 0 ; i < dis_gnum_operators; i++ ) {
    while ( dis_translate_one_negative_cond( dis_goperators[i]->preconds ) );

    for ( e = dis_goperators[i]->effects; e; e = e->next ) {
      while ( dis_translate_one_negative_cond( e->conditions ) );
    }
  }

  if ( dis_gcmd_line.display_info == 108 ) {
    printf("\n\ndomain with translated negative conds:");

    printf("\n\noperators are:");
    for ( i = 0; i < dis_gnum_operators; i++ ) {
      dis_print_dis_Operator( dis_goperators[i] );
    }
    printf("\n\n");

    printf("\ninitial state is:\n");
    for ( f = dis_ginitial; f; f = f->next ) {
      printf("\n");
      dis_print_dis_Fact( f->fact );
    }
    printf("\n");
    for ( ff = dis_gf_initial; ff; ff = ff->next ) {
      printf("\n");
      dis_print_dis_Fluent( &(ff->fluent) );
      printf(": %f", ff->value);
    }
    printf("\n\n");

    printf("\n\nindividual predicates:\n");
    for ( i = 0; i < dis_gnum_predicates; i++ ) {
      printf("\n\n%s:", dis_gpredicates[i]);
      if ( !dis_gis_added[i] &&
	   !dis_gis_deleted[i] ) {
	printf(" ---  STATIC");
      }
      for ( j = 0; j < dis_gnum_initial_predicate[i]; j++ ) {
	printf("\n");
	dis_print_dis_Fact( &(dis_ginitial_predicate[i][j]) );
      }
    }
    printf("\n\nindividual functions:");
    for ( i = 0; i < dis_gnum_functions; i++ ) {
      printf("\n\n%s:", dis_gfunctions[i]);
      if ( !dis_gis_changed[i] ) {
	printf(" ---  STATIC");
      }
      for ( j = 0; j < dis_gnum_initial_function[i]; j++ ) {
	printf("\n");
	dis_print_dis_Fluent( &(dis_ginitial_function[i][j].fluent) );
	printf(": %f", dis_ginitial_function[i][j].value);
     }
    }
    printf("\n\n");

    printf("\n\ngoal is:\n");
    dis_print_Wff( dis_ggoal, 0 );
    printf("\n\n");
  }

}



dis_Bool dis_translate_one_negative_cond( dis_WffNode *w )

{

  dis_WffNode *i;
  int p, j, k, m;
  dis_Effect *e;
  dis_Literal *l, *tmp;

  switch ( w->connective ) {
  case dis_ALL:
  case dis_EX:
    printf("\ntranslating dis_NOT in quantified formula! debug me\n\n");
    exit( 1 );
  case dis_AND:
  case dis_OR:
    for ( i = w->sons; i; i = i->next ) {
      if ( dis_translate_one_negative_cond( i ) ) {
	return dis_TRUE;
      }
    }
    return dis_FALSE;
  case dis_NOT:
    if ( w->son->fact->predicate == -1 ) {
      return dis_FALSE;
    }
    break;
  case dis_COMP:
  case dis_ATOM:
  case dis_TRU:
  case dis_FAL:
    return dis_FALSE;
  default:
    printf("\nwon't get here: translate one neg cond, non logical %d\n\n",
	   w->connective);
    exit( 1 );
  }

  GpG.is_negative = TRUE;
  if ( dis_gnum_predicates == dis_MAX_PREDICATES ) {
    printf("\ntoo many predicates in translation! increase dis_MAX_PREDICATES (currently %d)\n\n",
	   dis_MAX_PREDICATES);
    exit( 1 );
  }
  p = w->son->fact->predicate;
 // dis_gpredicates[dis_gnum_predicates] = dis_new_dis_Token( strlen( dis_gpredicates[p] ) + 5 );
  dis_gpredicates[dis_gnum_predicates] = 
      dis_new_dis_Token( strlen( dis_gpredicates[p] ) + strlen("dis_NOT-") + 1);
  sprintf( dis_gpredicates[dis_gnum_predicates], "dis_NOT-%s", dis_gpredicates[p] );
  dis_garity[dis_gnum_predicates] = dis_garity[p];
  for ( j = 0; j < dis_garity[p]; j++ ) {
    dis_gpredicates_args_type[dis_gnum_predicates][j] = 
      dis_gpredicates_args_type[p][j];
  }
  dis_gis_added[dis_gnum_predicates] = dis_FALSE;
  dis_gis_deleted[dis_gnum_predicates] = dis_FALSE;
  m = 1;
  for ( j = 0; j < dis_garity[dis_gnum_predicates]; j++ ) {
    m *= dis_gtype_size[dis_gpredicates_args_type[dis_gnum_predicates][j]];
  }
  dis_ginitial_predicate[dis_gnum_predicates] = ( dis_Fact * ) calloc( m, sizeof( dis_Fact ) );
  dis_gnum_predicates++;


  dis_replace_not_p_with_n_in_wff( p, dis_gnum_predicates - 1, &dis_ggoal );

  for ( j = 0; j < dis_gnum_operators; j++ ) {
    dis_replace_not_p_with_n_in_wff( p, dis_gnum_predicates - 1, 
				 &(dis_goperators[j]->preconds) );

    for ( e = dis_goperators[j]->effects; e; e = e->next ) {
      dis_replace_not_p_with_n_in_wff( p, dis_gnum_predicates - 1, 
				   &(e->conditions) );
      for ( l = e->effects; l; l = l->next ) {
	if ( l->fact.predicate != p ) {
	  continue;
	}
	tmp = dis_new_dis_Literal();
	if ( l->negated ) {
	  tmp->negated = dis_FALSE;
	  dis_gis_added[dis_gnum_predicates - 1] = dis_TRUE;
	} else {
	  tmp->negated = dis_TRUE;
	  dis_gis_deleted[dis_gnum_predicates - 1] = dis_TRUE;
	}
	tmp->fact.predicate = dis_gnum_predicates - 1;
	for ( k = 0; k < dis_garity[p]; k++ ) {
	  tmp->fact.args[k] = l->fact.args[k];
	  }
	if ( l->prev ) {
	  tmp->prev = l->prev;
	  tmp->prev->next = tmp;
	} else {
	  e->effects = tmp;
	}
	tmp->next = l;
	l->prev = tmp;
      }
    }
  }

  dis_add_to_initial_state( p, dis_gnum_predicates - 1, 0 );

  return dis_TRUE;

}



void dis_replace_not_p_with_n_in_wff( int p, int n, dis_WffNode **w )

{

  dis_WffNode *i;

  switch ( (*w)->connective ) {
  case dis_ALL:
  case dis_EX:
    printf("\nreplacing p with dis_NOT-p in quantified formula! debug me\n\n");
    exit( 1 );
  case dis_AND:
  case dis_OR:
    for ( i = (*w)->sons; i; i = i->next ) {
      dis_replace_not_p_with_n_in_wff( p, n, &i );
    }
    break;
  case dis_NOT:
    if ( (*w)->son->fact->predicate == p ) {
      (*w)->connective = dis_ATOM;
      (*w)->dis_NOT_p = p;
      (*w)->fact = (*w)->son->fact;
      (*w)->fact->predicate = n;
      xfree( (*w)->son );
      (*w)->son = NULL;
    }
    break;
  case dis_ATOM:
  case dis_COMP:
  case dis_TRU:
  case dis_FAL:
    break;
  default:
    printf("\nwon't get here: replace p with dis_NOT-p, non logical %d\n\n",
	   (*w)->connective);
    exit( 1 );
  }

}



void dis_add_to_initial_state( int p, int n, int index )

{

  int i, j;
  dis_Facts *tmp;

  if ( index == dis_garity[p] ) {
    /* see if contrary fact is there in ini
     */
    for ( i = 0; i < dis_gnum_initial_predicate[p]; i++ ) {
      for ( j = 0; j < dis_garity[p]; j++ ) {
	if ( dis_ginitial_predicate[p][i].args[j] != lconsts[j] ) {
	  break;
	}
      }
      if ( j == dis_garity[p] ) {
	break;
      }
    }
    if ( i < dis_gnum_initial_predicate[p] ) {
      return;
    }

    /* no: add new fact to ini
     */
    dis_ginitial_predicate[n][dis_gnum_initial_predicate[n]].predicate = n;
    for ( i = 0; i < dis_garity[n]; i++ ) {
      dis_ginitial_predicate[n][dis_gnum_initial_predicate[n]].args[i] = lconsts[i];
    }
    dis_gnum_initial_predicate[n]++;

    if ( !dis_gis_added[n] &&
	 !dis_gis_deleted[n] ) {
      return;
    }

    tmp = dis_new_dis_Facts();
    tmp->fact->predicate = n;
    for ( i = 0; i < dis_garity[p]; i++ ) {
      tmp->fact->args[i] = lconsts[i];
    }
    tmp->next = dis_ginitial;
    dis_ginitial = tmp;
    dis_gnum_initial++;
    return;
  }

  for ( i = 0; i < dis_gtype_size[dis_gpredicates_args_type[p][index]]; i++ ) {
    lconsts[index] = dis_gtype_consts[dis_gpredicates_args_type[p][index]][i];
    dis_add_to_initial_state( p, n, index + 1 );
  }

}











/*******************************************************************
 * SPLIT DOMAIN IN PREPARATION Fdis_OR SEPARATE INSTANTIATION ROUTINES *
 *******************************************************************/










void dis_split_domain( void )

{

  int i, j, m, s = 0, mn;
  dis_Effect *e;
  dis_WffNode *w, *ww, *www;
  Normdis_Operator *tmp_op;
  dis_Fact *tmp_ft;

  dis_goperators = (dis_Operator_pointer *) realloc(dis_goperators, 
              dis_gnum_operators*sizeof(dis_Operator_pointer));
  dis_gtype_to_predicate = realloc(dis_gtype_to_predicate, 
          dis_gnum_predicates*sizeof(int));
  
  for ( i = 0; i < dis_MAX_TYPES; i++ ) {
    dis_gnum_intersected_types[i] = -1;
  }

  for ( i = 0; i < dis_gnum_operators; i++ ) {
    if ( (m = dis_is_dis_dnf( dis_goperators[i]->preconds )) != -1 ) {
      for ( e = dis_goperators[i]->effects; e; e = e->next ) {
	if ( dis_is_dis_dnf( e->conditions ) == -1 ) {
	  break;
	}
      }
      if ( !e ) {
	dis_goperators[i]->hard = dis_FALSE;
	s += m;
      }
    }   
     /* mysterious bug, Hoffmann?*/
      if (strlen(dis_goperators[i]->name) > 11 &&      
              strcasecmp(dis_goperators[i]->name+11, "NOT-AFFECTED") == 0)
                dis_goperators[i]->hard = dis_TRUE;
  }

    dis_ghard_operators = ( dis_Operator_pointer * ) calloc( dis_gnum_operators, 
            sizeof( dis_Operator ) );  
//  dis_ghard_operators = ( dis_Operator_pointer * ) calloc( dis_MAX_OPERATdis_ORS, sizeof( dis_Operator ) );
  dis_gnum_hard_operators = 0; 
  dis_geasy_operators = ( Normdis_Operator_pointer * ) calloc( s, sizeof( Normdis_Operator_pointer ) );
  dis_gnum_easy_operators = 0;

  for ( i = 0; i < dis_gnum_operators; i++ ) {
    if ( dis_goperators[i]->hard ) {
      dis_ghard_operators[dis_gnum_hard_operators++] = dis_goperators[i];
      continue;
    }
    w = dis_goperators[i]->preconds;
    switch ( w->connective ) {
    case dis_OR:
      for ( ww = w->sons; ww; ww = ww->next ) {
	tmp_op = dis_new_Normdis_Operator( dis_goperators[i] );
	if ( ww->connective == dis_AND ) {
	  m = 0;
	  mn = 0;
	  for ( www = ww->sons; www; www = www->next ) {
	    if ( www->connective == dis_ATOM ) m++;
	    if ( www->connective == dis_COMP ) mn++;
	  }
	  tmp_op->preconds = ( dis_Fact * ) calloc( m, sizeof( dis_Fact ) );
	  tmp_op->numeric_preconds_comp = ( dis_Comparator * ) calloc( mn, sizeof( dis_Comparator ) );
	  tmp_op->numeric_preconds_lh = ( dis_ExpNode_pointer * ) calloc( mn, sizeof( dis_ExpNode_pointer ) );
	  tmp_op->numeric_preconds_rh = ( dis_ExpNode_pointer * ) calloc( mn, sizeof( dis_ExpNode_pointer ) );
	  for ( www = ww->sons; www; www = www->next ) {
	    if ( www->connective == dis_ATOM ) {
	      tmp_ft = &(tmp_op->preconds[tmp_op->num_preconds]);
	      tmp_ft->predicate = www->fact->predicate;
	      for ( j = 0; j < dis_garity[tmp_ft->predicate]; j++ ) {
		tmp_ft->args[j] = www->fact->args[j];
	      }
	      tmp_op->num_preconds++;
	    }
	    if ( www->connective == dis_COMP ) {
	      tmp_op->numeric_preconds_comp[tmp_op->num_numeric_preconds] = www->comp;
	      tmp_op->numeric_preconds_lh[tmp_op->num_numeric_preconds] = dis_copy_Exp( www->lh );
	      tmp_op->numeric_preconds_rh[tmp_op->num_numeric_preconds] = dis_copy_Exp( www->rh );
	      tmp_op->num_numeric_preconds++;
	    }
	  }
	} else {
	  if ( ww->connective == dis_ATOM ) {
	    tmp_op->preconds = ( dis_Fact * ) calloc( 1, sizeof( dis_Fact ) );
	    tmp_ft = &(tmp_op->preconds[0]);
	    tmp_ft->predicate = ww->fact->predicate;
	    for ( j = 0; j < dis_garity[tmp_ft->predicate]; j++ ) {
	      tmp_ft->args[j] = ww->fact->args[j];
	    }
	    tmp_op->num_preconds = 1;
	  }
	  if ( ww->connective == dis_COMP ) {
	    tmp_op->numeric_preconds_comp = ( dis_Comparator * ) calloc( 1, sizeof( dis_Comparator ) );
	    tmp_op->numeric_preconds_lh = ( dis_ExpNode_pointer * ) calloc( 1, sizeof( dis_ExpNode_pointer ) );
	    tmp_op->numeric_preconds_rh = ( dis_ExpNode_pointer * ) calloc( 1, sizeof( dis_ExpNode_pointer ) );
	    tmp_op->numeric_preconds_comp[0] = ww->comp;
	    tmp_op->numeric_preconds_lh[0] = dis_copy_Exp( ww->lh );
	    tmp_op->numeric_preconds_rh[0] = dis_copy_Exp( ww->rh );
	    tmp_op->num_numeric_preconds = 1;
	  }
	}
	dis_make_normal_effects( &tmp_op, dis_goperators[i] );
	dis_geasy_operators[dis_gnum_easy_operators++] = tmp_op;
      }
      break;
    case dis_AND:
      tmp_op = dis_new_Normdis_Operator( dis_goperators[i] );
      m = 0; 
      mn = 0;
      for ( ww = w->sons; ww; ww = ww->next ) {
	if ( ww->connective == dis_ATOM ) m++;
	if ( ww->connective == dis_COMP ) mn++;
      }
      tmp_op->preconds = ( dis_Fact * ) calloc( m, sizeof( dis_Fact ) );
      tmp_op->numeric_preconds_comp = ( dis_Comparator * ) calloc( mn, sizeof( dis_Comparator ) );
      tmp_op->numeric_preconds_lh = ( dis_ExpNode_pointer * ) calloc( mn, sizeof( dis_ExpNode_pointer ) );
      tmp_op->numeric_preconds_rh = ( dis_ExpNode_pointer * ) calloc( mn, sizeof( dis_ExpNode_pointer ) );
      for ( ww = w->sons; ww; ww = ww->next ) {
	if ( ww->connective == dis_ATOM ) {
	  tmp_ft = &(tmp_op->preconds[tmp_op->num_preconds]);
	  tmp_ft->predicate = ww->fact->predicate;
	  for ( j = 0; j < dis_garity[tmp_ft->predicate]; j++ ) {
	    tmp_ft->args[j] = ww->fact->args[j];
	  }
	  tmp_op->num_preconds++;
	}
	if ( ww->connective == dis_COMP ) {
	  tmp_op->numeric_preconds_comp[tmp_op->num_numeric_preconds] = ww->comp;
	  tmp_op->numeric_preconds_lh[tmp_op->num_numeric_preconds] = dis_copy_Exp( ww->lh );
	  tmp_op->numeric_preconds_rh[tmp_op->num_numeric_preconds] = dis_copy_Exp( ww->rh );
	  tmp_op->num_numeric_preconds++;
	}
      }
      dis_make_normal_effects( &tmp_op, dis_goperators[i] );
      dis_geasy_operators[dis_gnum_easy_operators++] = tmp_op;
      break;
    case dis_ATOM:
      tmp_op = dis_new_Normdis_Operator( dis_goperators[i] );
      tmp_op->preconds = ( dis_Fact * ) calloc( 1, sizeof( dis_Fact ) );
      tmp_ft = &(tmp_op->preconds[0]);
      tmp_ft->predicate = w->fact->predicate;
      for ( j = 0; j < dis_garity[tmp_ft->predicate]; j++ ) {
	tmp_ft->args[j] = w->fact->args[j];
      }
      tmp_op->num_preconds = 1;
      dis_make_normal_effects( &tmp_op, dis_goperators[i] );
      dis_geasy_operators[dis_gnum_easy_operators++] = tmp_op;
      break;
    case dis_COMP:
      tmp_op = dis_new_Normdis_Operator( dis_goperators[i] );
      tmp_op->numeric_preconds_comp = ( dis_Comparator * ) calloc( 1, sizeof( dis_Comparator ) );
      tmp_op->numeric_preconds_lh = ( dis_ExpNode_pointer * ) calloc( 1, sizeof( dis_ExpNode_pointer ) );
      tmp_op->numeric_preconds_rh = ( dis_ExpNode_pointer * ) calloc( 1, sizeof( dis_ExpNode_pointer ) );
      tmp_op->numeric_preconds_comp[0] = w->comp;
      tmp_op->numeric_preconds_lh[0] = dis_copy_Exp( w->lh );
      tmp_op->numeric_preconds_rh[0] = dis_copy_Exp( w->rh );
      tmp_op->num_numeric_preconds = 1;
      dis_make_normal_effects( &tmp_op, dis_goperators[i] );
      dis_geasy_operators[dis_gnum_easy_operators++] = tmp_op;
      break;
    case dis_TRU:
      tmp_op = dis_new_Normdis_Operator( dis_goperators[i] );
      dis_make_normal_effects( &tmp_op, dis_goperators[i] );
      dis_geasy_operators[dis_gnum_easy_operators++] = tmp_op;
      break;
    case dis_FAL:
      break;
    default:
      printf("\nwon't get here: non dis_OR, dis_AND, dis_ATOM, dis_TRUE, dis_FALSE in dis_dnf. debug me\n\n");
      exit( 1 );
    }
  }

  if ( dis_gcmd_line.display_info == 109 ) {
    printf("\n\nsplitted operators are:\n");
    
    printf("\nEASY:\n");
    for ( i = 0; i < dis_gnum_easy_operators; i++ ) {
      dis_print_Normdis_Operator( dis_geasy_operators[i] );
    }

    printf("\n\n\nHARD:\n");
    for ( i = 0; i < dis_gnum_hard_operators; i++ ) {
      dis_print_dis_Operator( dis_ghard_operators[i] );
    }
  } 

//   xfree(dis_gfull_initial);
//   xfree(dis_gfull_fluents_initial);       
}



int dis_is_dis_dnf( dis_WffNode *w )

{

  dis_WffNode *i;
  int s = 0;

  switch ( w->connective ) {
  case dis_ALL:
  case dis_EX:
    printf("\nchecking quantifier for dis_dnf. debug me\n\n");
    exit( 1 );
  case dis_AND:
    for ( i = w->sons; i; i = i->next ) {
      if ( i->connective == dis_ATOM ||
	   i->connective == dis_COMP ) {
	continue;
      }
      return -1;
    }
    return 1;
  case dis_OR:
    for ( i = w->sons; i; i = i->next ) {
      s++;
      if ( i->connective == dis_ATOM ||
	   i->connective == dis_COMP ||
	   ( i->connective == dis_AND &&
	     dis_is_dis_dnf( i ) != -1 ) ) {
	continue;
      }
      return -1;
    }
    return s;
  case dis_NOT:
    printf("\n\ndis_NOT in presimplified formula. debug me\n\n");
    exit( 1 );
  case dis_ATOM:
  case dis_COMP:
  case dis_TRU:
  case dis_FAL:
    return 1;
  default:
    printf("\nwon't get here: check dis_dnf, conn %d\n\n",
	   w->connective);
    exit( 1 );
  }

}



void dis_make_normal_effects( Normdis_Operator **nop, dis_Operator *op )

{

  dis_Effect *e;
  Normdis_Effect *tmp_ef;
  dis_WffNode *w, *ww, *www;
  int j, m, ma, md, mn;
  dis_Literal *l;
  dis_Numericdis_Effect *ll;
  dis_Fact *tmp_ft;
  dis_Fluent *tmp_fl;

  for ( e = op->effects; e; e = e->next ) {
    w = e->conditions;
    switch ( w->connective ) {
    case dis_OR:
      for ( ww = w->sons; ww; ww = ww->next ) {
	tmp_ef = dis_new_Normdis_Effect1( e );
	if ( ww->connective == dis_AND ) {
	  m = 0;
	  mn = 0;
	  for ( www = ww->sons; www; www = www->next ) {
	    if ( www->connective == dis_ATOM ) m++;
	    if ( www->connective == dis_COMP ) mn++;
	  }
	  tmp_ef->conditions = ( dis_Fact * ) calloc( m, sizeof( dis_Fact ) );
	  tmp_ef->numeric_conditions_comp = ( dis_Comparator * ) calloc( mn, sizeof( dis_Comparator ) );
	  tmp_ef->numeric_conditions_lh = ( dis_ExpNode_pointer * ) calloc( mn, sizeof( dis_ExpNode_pointer ) );
	  tmp_ef->numeric_conditions_rh = ( dis_ExpNode_pointer * ) calloc( mn, sizeof( dis_ExpNode_pointer ) );
	  for ( www = ww->sons; www; www = www->next ) {
	    if ( www->connective == dis_ATOM ) {
	      tmp_ft = &(tmp_ef->conditions[tmp_ef->num_conditions]);
	      tmp_ft->predicate = www->fact->predicate;
	      for ( j = 0; j < dis_garity[tmp_ft->predicate]; j++ ) {
		tmp_ft->args[j] = www->fact->args[j];
	      }
	      tmp_ef->num_conditions++;
	    }
	    if ( www->connective == dis_COMP ) {
	      tmp_ef->numeric_conditions_comp[tmp_ef->num_numeric_conditions] = www->comp;
	      tmp_ef->numeric_conditions_lh[tmp_ef->num_numeric_conditions] = dis_copy_Exp( www->lh );
	      tmp_ef->numeric_conditions_rh[tmp_ef->num_numeric_conditions] = dis_copy_Exp( www->rh );
	      tmp_ef->num_numeric_conditions++;
	    }
	  }
	} else {
	  if ( ww->connective == dis_ATOM ) {
	    tmp_ef->conditions = ( dis_Fact * ) calloc( 1, sizeof( dis_Fact ) );
	    tmp_ft = &(tmp_ef->conditions[0]);
	    tmp_ft->predicate = ww->fact->predicate;
	    for ( j = 0; j < dis_garity[tmp_ft->predicate]; j++ ) {
	      tmp_ft->args[j] = ww->fact->args[j];
	    }
	    tmp_ef->num_conditions = 1;
	  }
	  if ( ww->connective == dis_COMP ) {
	    tmp_ef->numeric_conditions_comp = ( dis_Comparator * ) calloc( 1, sizeof( dis_Comparator ) );
	    tmp_ef->numeric_conditions_lh = ( dis_ExpNode_pointer * ) calloc( 1, sizeof( dis_ExpNode_pointer ) );
	    tmp_ef->numeric_conditions_rh = ( dis_ExpNode_pointer * ) calloc( 1, sizeof( dis_ExpNode_pointer ) );
	    tmp_ef->numeric_conditions_comp[0] = ww->comp;
	    tmp_ef->numeric_conditions_lh[0] = dis_copy_Exp( ww->lh );
	    tmp_ef->numeric_conditions_rh[0] = dis_copy_Exp( ww->rh );
	    tmp_ef->num_numeric_conditions = 1;
	  }
	}
	ma = 0; md = 0;
	for ( l = e->effects; l; l = l->next ) {
	  if ( l->negated ) { md++; } else { ma++; }
	}
	tmp_ef->adds = ( dis_Fact * ) calloc( ma, sizeof( dis_Fact ) );
	tmp_ef->dels = ( dis_Fact * ) calloc( md, sizeof( dis_Fact ) );
	for ( l = e->effects; l; l = l->next ) {
	  if ( l->negated ) {
	    tmp_ft = &(tmp_ef->dels[tmp_ef->num_dels++]);
	  } else {
	    tmp_ft = &(tmp_ef->adds[tmp_ef->num_adds++]);
	  }
	  tmp_ft->predicate = l->fact.predicate;
	  for ( j = 0; j < dis_garity[tmp_ft->predicate]; j++ ) {
	    tmp_ft->args[j] = l->fact.args[j];
	  }
	}
	ma = 0;
	for ( ll = e->numeric_effects; ll; ll = ll->next ) ma++;
	tmp_ef->numeric_effects_neft = ( dis_Numericdis_EffectType * ) calloc( ma, sizeof( dis_Numericdis_EffectType ) );
	tmp_ef->numeric_effects_fluent = ( dis_Fluent * ) calloc( ma, sizeof( dis_Fluent ) );
	tmp_ef->numeric_effects_rh = ( dis_ExpNode_pointer * ) calloc( ma, sizeof( dis_ExpNode_pointer ) );
	for ( ll = e->numeric_effects; ll; ll = ll->next ) {
	  tmp_ef->numeric_effects_neft[tmp_ef->num_numeric_effects] = ll->neft;
	  tmp_fl = &(tmp_ef->numeric_effects_fluent[tmp_ef->num_numeric_effects]);
	  tmp_fl->function = ll->fluent.function;
	  for ( j = 0; j < dis_gf_arity[tmp_fl->function]; j++ ) {
	    tmp_fl->args[j] = ll->fluent.args[j];
	  }
	  tmp_ef->numeric_effects_rh[tmp_ef->num_numeric_effects] = dis_copy_Exp( ll->rh );
	  tmp_ef->num_numeric_effects++;
	}
	tmp_ef->next = (*nop)->effects;
	if ( (*nop)->effects ) {
	  (*nop)->effects->prev = tmp_ef;
	}
	(*nop)->effects = tmp_ef;
      }
      break;
    case dis_AND:
      tmp_ef = dis_new_Normdis_Effect1( e );
      m = 0;
      mn = 0;
      for ( ww = w->sons; ww; ww = ww->next ) {
	if ( ww->connective == dis_ATOM ) m++;
	if ( ww->connective == dis_COMP ) mn++;
      }
      tmp_ef->conditions = ( dis_Fact * ) calloc( m, sizeof( dis_Fact ) );
      tmp_ef->numeric_conditions_comp = ( dis_Comparator * ) calloc( mn, sizeof( dis_Comparator ) );
      tmp_ef->numeric_conditions_lh = ( dis_ExpNode_pointer * ) calloc( mn, sizeof( dis_ExpNode_pointer ) );
      tmp_ef->numeric_conditions_rh = ( dis_ExpNode_pointer * ) calloc( mn, sizeof( dis_ExpNode_pointer ) );
      for ( ww = w->sons; ww; ww = ww->next ) {
	if ( ww->connective == dis_ATOM ) {
	  tmp_ft = &(tmp_ef->conditions[tmp_ef->num_conditions]);
	  tmp_ft->predicate = ww->fact->predicate;
	  for ( j = 0; j < dis_garity[tmp_ft->predicate]; j++ ) {
	    tmp_ft->args[j] = ww->fact->args[j];
	  }
	  tmp_ef->num_conditions++;
	}
	if ( ww->connective == dis_COMP ) {
	  tmp_ef->numeric_conditions_comp[tmp_ef->num_numeric_conditions] = ww->comp;
	  tmp_ef->numeric_conditions_lh[tmp_ef->num_numeric_conditions] = dis_copy_Exp( ww->lh );
	  tmp_ef->numeric_conditions_rh[tmp_ef->num_numeric_conditions] = dis_copy_Exp( ww->rh );
	  tmp_ef->num_numeric_conditions++;
	}
      }
      ma = 0; md = 0;
      for ( l = e->effects; l; l = l->next ) {
	if ( l->negated ) { md++; } else { ma++; }
      }
      tmp_ef->adds = ( dis_Fact * ) calloc( ma, sizeof( dis_Fact ) );
      tmp_ef->dels = ( dis_Fact * ) calloc( md, sizeof( dis_Fact ) );
      for ( l = e->effects; l; l = l->next ) {
	if ( l->negated ) {
	  tmp_ft = &(tmp_ef->dels[tmp_ef->num_dels++]);
	} else {
	  tmp_ft = &(tmp_ef->adds[tmp_ef->num_adds++]);
	}
	tmp_ft->predicate = l->fact.predicate;
	for ( j = 0; j < dis_garity[tmp_ft->predicate]; j++ ) {
	  tmp_ft->args[j] = l->fact.args[j];
	}
      }
      ma = 0;
      for ( ll = e->numeric_effects; ll; ll = ll->next ) ma++;
      tmp_ef->numeric_effects_neft = ( dis_Numericdis_EffectType * ) calloc( ma, sizeof( dis_Numericdis_EffectType ) );
      tmp_ef->numeric_effects_fluent = ( dis_Fluent * ) calloc( ma, sizeof( dis_Fluent ) );
      tmp_ef->numeric_effects_rh = ( dis_ExpNode_pointer * ) calloc( ma, sizeof( dis_ExpNode_pointer ) );
      for ( ll = e->numeric_effects; ll; ll = ll->next ) {
	tmp_ef->numeric_effects_neft[tmp_ef->num_numeric_effects] = ll->neft;
	tmp_fl = &(tmp_ef->numeric_effects_fluent[tmp_ef->num_numeric_effects]);
	tmp_fl->function = ll->fluent.function;
	for ( j = 0; j < dis_gf_arity[tmp_fl->function]; j++ ) {
	  tmp_fl->args[j] = ll->fluent.args[j];
	}
	tmp_ef->numeric_effects_rh[tmp_ef->num_numeric_effects] = dis_copy_Exp( ll->rh );
	tmp_ef->num_numeric_effects++;
      }
      tmp_ef->next = (*nop)->effects;
      if ( (*nop)->effects ) {
	(*nop)->effects->prev = tmp_ef;
      }
      (*nop)->effects = tmp_ef;
      break;
    case dis_ATOM:
      tmp_ef = dis_new_Normdis_Effect1( e );
      tmp_ef->conditions = ( dis_Fact * ) calloc( 1, sizeof( dis_Fact ) );
      tmp_ft = &(tmp_ef->conditions[0]);
      tmp_ft->predicate = w->fact->predicate;
      for ( j = 0; j < dis_garity[tmp_ft->predicate]; j++ ) {
	tmp_ft->args[j] = w->fact->args[j];
      }
      tmp_ef->num_conditions = 1;
      ma = 0; md = 0;
      for ( l = e->effects; l; l = l->next ) {
	if ( l->negated ) { md++; } else { ma++; }
      }
      tmp_ef->adds = ( dis_Fact * ) calloc( ma, sizeof( dis_Fact ) );
      tmp_ef->dels = ( dis_Fact * ) calloc( md, sizeof( dis_Fact ) );
      for ( l = e->effects; l; l = l->next ) {
	if ( l->negated ) {
	  tmp_ft = &(tmp_ef->dels[tmp_ef->num_dels++]);
	} else {
	  tmp_ft = &(tmp_ef->adds[tmp_ef->num_adds++]);
	}
	tmp_ft->predicate = l->fact.predicate;
	for ( j = 0; j < dis_garity[tmp_ft->predicate]; j++ ) {
	  tmp_ft->args[j] = l->fact.args[j];
	}
      }
      ma = 0;
      for ( ll = e->numeric_effects; ll; ll = ll->next ) ma++;
      tmp_ef->numeric_effects_neft = ( dis_Numericdis_EffectType * ) calloc( ma, sizeof( dis_Numericdis_EffectType ) );
      tmp_ef->numeric_effects_fluent = ( dis_Fluent * ) calloc( ma, sizeof( dis_Fluent ) );
      tmp_ef->numeric_effects_rh = ( dis_ExpNode_pointer * ) calloc( ma, sizeof( dis_ExpNode_pointer ) );
      for ( ll = e->numeric_effects; ll; ll = ll->next ) {
	tmp_ef->numeric_effects_neft[tmp_ef->num_numeric_effects] = ll->neft;
	tmp_fl = &(tmp_ef->numeric_effects_fluent[tmp_ef->num_numeric_effects]);
	tmp_fl->function = ll->fluent.function;
	for ( j = 0; j < dis_gf_arity[tmp_fl->function]; j++ ) {
	  tmp_fl->args[j] = ll->fluent.args[j];
	}
	tmp_ef->numeric_effects_rh[tmp_ef->num_numeric_effects] = dis_copy_Exp( ll->rh );
	tmp_ef->num_numeric_effects++;
      }
      tmp_ef->next = (*nop)->effects;
      if ( (*nop)->effects ) {
	(*nop)->effects->prev = tmp_ef;
      }
      (*nop)->effects = tmp_ef;
      break;     
    case dis_COMP:
      tmp_ef = dis_new_Normdis_Effect1( e );
      tmp_ef->numeric_conditions_comp = ( dis_Comparator * ) calloc( 1, sizeof( dis_Comparator ) );
      tmp_ef->numeric_conditions_lh = ( dis_ExpNode_pointer * ) calloc( 1, sizeof( dis_ExpNode_pointer ) );
      tmp_ef->numeric_conditions_rh = ( dis_ExpNode_pointer * ) calloc( 1, sizeof( dis_ExpNode_pointer ) );
      tmp_ef->numeric_conditions_comp[0] = w->comp;
      tmp_ef->numeric_conditions_lh[0] = dis_copy_Exp( w->lh );
      tmp_ef->numeric_conditions_rh[0] = dis_copy_Exp( w->rh );
      tmp_ef->num_numeric_conditions = 1;
      ma = 0; md = 0;
      for ( l = e->effects; l; l = l->next ) {
	if ( l->negated ) { md++; } else { ma++; }
      }
      tmp_ef->adds = ( dis_Fact * ) calloc( ma, sizeof( dis_Fact ) );
      tmp_ef->dels = ( dis_Fact * ) calloc( md, sizeof( dis_Fact ) );
      for ( l = e->effects; l; l = l->next ) {
	if ( l->negated ) {
	  tmp_ft = &(tmp_ef->dels[tmp_ef->num_dels++]);
	} else {
	  tmp_ft = &(tmp_ef->adds[tmp_ef->num_adds++]);
	}
	tmp_ft->predicate = l->fact.predicate;
	for ( j = 0; j < dis_garity[tmp_ft->predicate]; j++ ) {
	  tmp_ft->args[j] = l->fact.args[j];
	}
      }
      ma = 0;
      for ( ll = e->numeric_effects; ll; ll = ll->next ) ma++;
      tmp_ef->numeric_effects_neft = ( dis_Numericdis_EffectType * ) calloc( ma, sizeof( dis_Numericdis_EffectType ) );
      tmp_ef->numeric_effects_fluent = ( dis_Fluent * ) calloc( ma, sizeof( dis_Fluent ) );
      tmp_ef->numeric_effects_rh = ( dis_ExpNode_pointer * ) calloc( ma, sizeof( dis_ExpNode_pointer ) );
      for ( ll = e->numeric_effects; ll; ll = ll->next ) {
	tmp_ef->numeric_effects_neft[tmp_ef->num_numeric_effects] = ll->neft;
	tmp_fl = &(tmp_ef->numeric_effects_fluent[tmp_ef->num_numeric_effects]);
	tmp_fl->function = ll->fluent.function;
	for ( j = 0; j < dis_gf_arity[tmp_fl->function]; j++ ) {
	  tmp_fl->args[j] = ll->fluent.args[j];
	}
	tmp_ef->numeric_effects_rh[tmp_ef->num_numeric_effects] = dis_copy_Exp( ll->rh );
	tmp_ef->num_numeric_effects++;
      }
      tmp_ef->next = (*nop)->effects;
      if ( (*nop)->effects ) {
	(*nop)->effects->prev = tmp_ef;
      }
      (*nop)->effects = tmp_ef;
      break;
    case dis_TRU:
      tmp_ef = dis_new_Normdis_Effect1( e );
      ma = 0; md = 0;
      for ( l = e->effects; l; l = l->next ) {
	if ( l->negated ) { md++; } else { ma++; }
      }
      if (ma)
      tmp_ef->adds = ( dis_Fact * ) calloc( ma, sizeof( dis_Fact ) );
      if (md)
      tmp_ef->dels = ( dis_Fact * ) calloc( md, sizeof( dis_Fact ) );
      for ( l = e->effects; l; l = l->next ) {
	if ( l->negated ) {
	  tmp_ft = &(tmp_ef->dels[tmp_ef->num_dels++]);
	} else {
	  tmp_ft = &(tmp_ef->adds[tmp_ef->num_adds++]);
	}
	tmp_ft->predicate = l->fact.predicate;
	for ( j = 0; j < dis_garity[tmp_ft->predicate]; j++ ) {
	  tmp_ft->args[j] = l->fact.args[j];
	}
      }
      ma = 0;
      for ( ll = e->numeric_effects; ll; ll = ll->next ) ma++;
      if (ma)
      {
      tmp_ef->numeric_effects_neft = ( dis_Numericdis_EffectType * ) calloc( ma, sizeof( dis_Numericdis_EffectType ) );
      tmp_ef->numeric_effects_fluent = ( dis_Fluent * ) calloc( ma, sizeof( dis_Fluent ) );
      tmp_ef->numeric_effects_rh = ( dis_ExpNode_pointer * ) calloc( ma, sizeof( dis_ExpNode_pointer ) );
      }
      for ( ll = e->numeric_effects; ll; ll = ll->next ) {
	tmp_ef->numeric_effects_neft[tmp_ef->num_numeric_effects] = ll->neft;
	tmp_fl = &(tmp_ef->numeric_effects_fluent[tmp_ef->num_numeric_effects]);
	tmp_fl->function = ll->fluent.function;
	for ( j = 0; j < dis_gf_arity[tmp_fl->function]; j++ ) {
	  tmp_fl->args[j] = ll->fluent.args[j];
	}
	tmp_ef->numeric_effects_rh[tmp_ef->num_numeric_effects] = dis_copy_Exp( ll->rh );
	tmp_ef->num_numeric_effects++;
      }
      tmp_ef->next = (*nop)->effects;
      if ( (*nop)->effects ) {
	(*nop)->effects->prev = tmp_ef;
      }
      (*nop)->effects = tmp_ef;
      break;
    case dis_FAL:
      break;
    default:
      printf("\nwon't get here: non dis_OR, dis_AND, dis_ATOM, dis_TRUE, dis_FALSE in dis_dnf. debug me\n\n");
      exit( 1 );
    }
  }

}









/*************************************************************************
 * ADDITIONAL: FULL DNF, only compute on fully instantiated formulae!!!! *
 *************************************************************************/










/* dis_dnf
 */

dis_WffNode *lhitting_sets;
dis_WffNode_pointer *lset;
int lmax_set;






void dis_dnf( dis_WffNode **w )

{

  static dis_Bool first_call = dis_TRUE;

  if ( first_call ) {
    lset = ( dis_WffNode_pointer * ) 
      calloc( dis_MAX_HITTING_SET_DEFAULT, sizeof( dis_WffNode_pointer ) );
    lmax_set = dis_MAX_HITTING_SET_DEFAULT;
    first_call = dis_FALSE;
  }

  dis_ANDs_below_dis_ORs_in_wff( w );

}



void dis_ANDs_below_dis_ORs_in_wff( dis_WffNode **w )

{

  dis_WffNode *i, *tmp;
  int c, m;

  switch ( (*w)->connective ) {
  case dis_ALL:
  case dis_EX:
    printf("\ntrying to put quantified formula into DNF! (ands down) debug me\n\n");
    exit( 1 );
    break;
  case dis_AND:
    c = 0;
    m = 0;
    for ( i = (*w)->sons; i; i = i->next ) {
      dis_ANDs_below_dis_ORs_in_wff( &i );
      if ( i->connective == dis_OR ) {
	c++;
      }
      m++;
    }
    if ( c == 0 ) {
      /* no dis_ORs as sons --> all sons are literals. OK
       */
      dis_merge_next_step_dis_ANDs_and_dis_ORs_in_wff( w );
      break;
    }
    /* crucial part: dis_AND node, sons can be merged dis_OR's.
     * (i.e., sons are either literals or disjunctions of 
     * conjunctions of literals)
     * create dis_OR node with one hitting set of w's sons for 
     * each disjunct
     */
    lhitting_sets = NULL;
    if ( m > lmax_set ) {
      xfree( lset );
      lset = ( dis_WffNode_pointer * ) calloc( m, sizeof( dis_WffNode_pointer ) );
      lmax_set = m;
    }
    dis_collect_hitting_sets( (*w)->sons, 0 );
    (*w)->connective = dis_OR;
    tmp = (*w)->sons;
    (*w)->sons = lhitting_sets;
//    dis_free_dis_WffNode( tmp );
    dis_merge_next_step_dis_ANDs_and_dis_ORs_in_wff( w );
    break;
  case dis_OR:
    for ( i = (*w)->sons; i; i = i->next ) {
      dis_ANDs_below_dis_ORs_in_wff( &i );
    }
    dis_merge_next_step_dis_ANDs_and_dis_ORs_in_wff( w );
    break;
  case dis_NOT:
  case dis_ATOM:
  case dis_COMP:
  case dis_TRU:
  case dis_FAL:
    break;
  default:
    printf("\nwon't get here: ands down, non logical %d\n\n",
	   (*w)->connective);
    exit( 1 );
  }

}



void dis_collect_hitting_sets( dis_WffNode *dis_ORlist, int index )

{

  dis_WffNode *tmp1, *tmp2, *j;
  int i;

  if ( !dis_ORlist ) {
    tmp1 = dis_new_dis_WffNode( dis_AND );
    for ( i = 0; i < index; i++ ) {
      tmp2 = dis_copy_Wff( lset[i] );
      tmp2->next = tmp1->sons;
      if ( tmp1->sons ) {
	tmp1->sons->prev = tmp2;
      }
      tmp1->sons = tmp2;
    }
    tmp1->next = lhitting_sets;
    if ( lhitting_sets ) {
      lhitting_sets->prev = tmp1;
    }
    lhitting_sets = tmp1;
    return;
  }

  if ( dis_ORlist->connective != dis_OR ) {
    lset[index] = dis_ORlist;
    dis_collect_hitting_sets( dis_ORlist->next, index + 1 );
    return;
  }

  for ( j = dis_ORlist->sons; j; j = j->next ) {
    lset[index] = j;
    dis_collect_hitting_sets( dis_ORlist->next, index + 1 );
  }

}



void dis_merge_next_step_dis_ANDs_and_dis_ORs_in_wff( dis_WffNode **w )

{

  dis_WffNode *i, *j, *tmp;

  i = (*w)->sons;
  while ( i ) {
    if ( i->connective == (*w)->connective ) {
      if ( !(i->sons) ) {
	if ( i->next ) {
	  i->next->prev = i->prev;
	}
	if ( i->prev ) {
	  i->prev->next = i->next;
	} else {
	  (*w)->sons = i->next;
	}
	tmp = i;
	i = i->next;
	xfree( tmp );
	continue;
      }
      for ( j = i->sons; j->next; j = j->next );
      j->next = i->next;
      if ( i->next ) {
	i->next->prev = j;
      }
      if ( i->prev ) {
	i->prev->next = i->sons;
	i->sons->prev = i->prev;
      } else {
	(*w)->sons = i->sons;
      }
      tmp = i;
      i = i->next;
      xfree( tmp );
      continue;
    }
    i = i->next;
  }

}



/*   switch ( (*w)->connective ) { */
/*   case dis_ALL: */
/*   case dis_EX: */
/*     break; */
/*   case dis_AND: */
/*   case dis_OR: */
/*     for ( i = (*w)->sons; i; i = i->next ) { */
/*     } */
/*     break; */
/*   case dis_NOT: */
/*     break; */
/*   case dis_ATOM: */
/*   case dis_TRU: */
/*   case dis_FAL: */
/*     break; */
/*   default: */
/*     printf("\nwon't get here: remove var, non logical %d\n\n", */
/* 	   (*w)->connective); */
/*     exit( 1 ); */
/*   } */









