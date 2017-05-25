





/*********************************************************************
 * File: instantiateIII.c
 *
 * Description: routines that perform the actual instantiation part
 *
 *                        - expand all quantifiers into con/dis junctions
 *                        - multiply params of (conditional) effects
 *                        - multiply params of operators
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
#include "instantiateII.h"
#include "instantiateIII.h"

 














/*
 *  ----------------------- MULTIPLY ALL QUANTIFIERS AND PARAMS ------------------
 */









CodeNode *ltmpIII;











void multiply_params_and_quantifiers( void )

{

  CodeOperator *i, *j, *prev;
  CodeNode *a, *b, *c;

  /* a little dirty, there are no quantifiers in the initial state.
   * such, the function only affects the atoms, detecting inertia that way
   */
  expand_quantifiers_under_CodeNode( &gcode_initial_state, -1, 0 );

  detect_tautologies_in_condition_CodeNode( &gcode_initial_state, FALSE );
  simplify_condition_CodeNode( &gcode_initial_state, FALSE, FALSE );  


  expand_quantifiers_under_CodeNode( &gcode_goal_state, -1, 0 );
  merge_multiple_ANDs_and_ORs( &gcode_goal_state );
  detect_tautologies_in_condition_CodeNode( &gcode_goal_state, FALSE );
  simplify_condition_CodeNode( &gcode_goal_state, FALSE, TRUE );
  /* D N F could be done here already ( or later )
   */
  if ( !gcode_goal_state ||
       gcode_goal_state->connective == TRU ) {
    times( &gend );
    gtotal_time += ( float ) ( ( gend.tms_utime - gstart.tms_utime 
				 + gend.tms_stime - gstart.tms_stime ) / 100.0 );
    printf("\n\n\nipp: goal can be expanded to TRUE. no plan needed\n");
    printf("\nelapsed time: %7.2f seconds\n\n", gtotal_time);
    exit( 1 );
  }
  if ( gcode_goal_state->connective == FAL ) {
    times( &gend );
    gtotal_time += ( float ) ( ( gend.tms_utime - gstart.tms_utime 
				 + gend.tms_stime - gstart.tms_stime ) / 100.0 );
    printf("\n\n\nipp: goal can be expanded to FALSE. no plan will solve it\n");
    printf("\nelapsed time: %7.2f seconds\n\n", gtotal_time);
    exit( 1 );
  }


  /* expand preconds ALL, EX to AND, OR; simplifiable to False --> skip it
   */
  i = gcode_operators;
  while ( i && 
	  expand_preconditions_of_CodeOperator( &i ) ) {
    if ( gcmd_line.display_info ) {
      printf("\nwarning: op %s has expanded contradicting precondition. skipping it.",
	     i->name);
      fflush( stdout );
    }
    j = i;
    i = i->next;
    free_CodeOperator( j );
  }
  gcode_operators = i;
  prev = i;
  if ( i ) i = i->next;
  while ( i ) {
    if ( expand_preconditions_of_CodeOperator( &i ) ) {
      if ( gcmd_line.display_info ) {
	printf("\nwarning: op %s has expanded contradicting precondition. skipping it.",
	       i->name);
	fflush( stdout );
      }
      prev->next = i->next;
      j = i;
      i = i->next;
      free_CodeOperator( j );
    } else {
      prev = prev->next;
      i = i->next;
    }
  }

  /* conditions ALL, EX to AND, OR; FALSE --> SKIP THEM
   */
  for ( i = gcode_operators; i; i=i->next ) {
    if ( !i->conditionals ) continue;

    a = i->conditionals->sons;
    while ( a && expand_conditions_of_effect( &a ) ) {
      b = a;
      a = a->next;
      free_CodeNode( b->sons );
      free( b );
    }
    i->conditionals->sons = a;
    c = a;
    if ( a ) a = a->next;
    while ( a ) {
      if ( expand_conditions_of_effect( &a ) ) {
	c->next = a->next;
	b = a;
	a = a->next;
	free_CodeNode( b->sons );
	free( b );
      } else {
	c = c->next;
	a = a->next;
      }
    }
    if ( !(i->conditionals->sons) ) {
      free_CodeNode( i->conditionals );
      i->conditionals = NULL;
    }
  }

  /* multiply conditional effects
   */
  for ( i = gcode_operators; i; i=i->next ) {
    if ( !i->conditionals ) continue;
    
    ltmpIII = NULL;
    for ( a = i->conditionals->sons; a; a = a->next ) {
      for ( b = a; b->connective != WHEN; b = b->sons );
      multiply_params_of_effect( a, b->sons );
    }
    free_CodeNode( i->conditionals->sons );
    i->conditionals->sons = ltmpIII;
  }

  /* multiply operator parameters --> actual instances (actions)
   *
   * actions are placed into ginst_code_operators
   */
  i = gcode_operators;
  while ( i ) {
    multiply_params_of_CodeOperator( i, 0, i->preconds );
    j = i;
    i = i->next;
    free_CodeOperator( j );
  }
  gcode_operators = NULL;
  
}



Bool expand_preconditions_of_CodeOperator( CodeOperator **op )

{

  expand_quantifiers_under_CodeNode( &((*op)->preconds), -1, 0 );
  merge_multiple_ANDs_and_ORs( &((*op)->preconds) );
  detect_tautologies_in_condition_CodeNode( &((*op)->preconds), FALSE  );
  simplify_condition_CodeNode( &((*op)->preconds), FALSE, TRUE );

  if ( (*op)->preconds &&
       (*op)->preconds->connective == FAL ) {
    return TRUE;
  }

  /* D N F could be done here already ( or later )
   */
  return FALSE;

}



Bool expand_conditions_of_effect( CodeNode **ef )

{

  CodeNode *i;

  for ( i = *ef; i->connective != WHEN; i = i->sons );
  
  expand_quantifiers_under_CodeNode( &(i->sons), -1, 0 );
  merge_multiple_ANDs_and_ORs( &(i->sons) );
  detect_tautologies_in_condition_CodeNode( &(i->sons), FALSE );
  simplify_condition_CodeNode( &(i->sons), FALSE, TRUE );
  
  if ( i->sons &&
       i->sons->connective == FAL ) {
    return TRUE;
  }

  /* D N F could be done here already ( or later )
   */
  return FALSE;

}



void multiply_params_of_effect( CodeNode *pars, CodeNode *when_sons )

{

  Integers *j;
  CodeNode *tmp;

  if ( pars->connective == ALL ) {
    for ( j = (gtypes_table[pars->var_type]).integers; j; j = j->next ) {
      tmp = deep_copy_CodeTree( when_sons );
      make_inst_version_of_condition( &tmp, pars->var, j->index );
      simplify_condition_CodeNode( &tmp, FALSE, TRUE );
      if ( tmp && 
	   tmp->connective == FAL ) {
	free_CodeNode( tmp );
	continue;
      }
      make_inst_version_of_effect( &tmp->next, pars->var, j->index );

      multiply_params_of_effect( pars->sons, tmp );
      free_CodeNode( tmp );
    }
    return;
  }

  if ( pars->connective != WHEN ) {
    printf("\nnon ALL/WHEN in preliminary of effect! shouldn't happen\n\n");
    exit( 1 );
  }

  tmp = new_CodeNode( WHEN );
  tmp->sons = deep_copy_CodeTree( when_sons );
  tmp->next = ltmpIII;
  ltmpIII = tmp;

}



void multiply_params_of_CodeOperator( CodeOperator *op, int curr,
				      CodeNode *precond )

{

  Integers *j;
  int l, a;
  CodeNode *tmp, *i;
  CodeOperator *tmpop;

  if ( curr < op->num_vars ) {
    for ( j = (gtypes_table[op->var_types[curr]]).integers; j; j=j->next ) {
      if ( 0 ) {
	/* ATTENTION!!  double parameters are skipped here, i.e. we don't
	 * allow the same constant into more than one parameter
	 */
	for ( a=0; a<curr; a++ ) {
	  if ( op->inst_table[a] == j->index ) break;
	}
	if ( a<curr ) continue;
      }

      tmp = deep_copy_CodeTree( precond );
      make_inst_version_of_condition( &tmp, curr, j->index );
      simplify_condition_CodeNode( &tmp, FALSE, TRUE );
      if ( tmp && tmp->connective == FAL ) {
	free_CodeNode( tmp );
	continue;
      }

      op->inst_table[curr] = j->index;
      multiply_params_of_CodeOperator( op, curr+1, tmp );
      free_CodeNode( tmp );
    }
    return;
  }

  
  /* NOTE: it could happen that new tautoligies arise from instatiating.
   *       seems to happen very rarely, however, so we spare the time to 
   *       check that here.
   */
  tmpop = new_CodeOperator();
  tmpop->name = copy_string( op->name );
  tmpop->number_of_real_params = op->number_of_real_params;
  tmpop->num_vars = op->num_vars;
  for ( l=0; l<op->num_vars; l++ ) {
    tmpop->var_types[l] = op->var_types[l];
    tmpop->inst_table[l] = op->inst_table[l];
  }
  tmpop->preconds = deep_copy_CodeTree( precond );

  if ( op->conditionals ) {
    tmpop->conditionals = new_CodeNode( AND );
    for ( i=op->conditionals->sons; i; i = i->next ) {
      tmp = new_CodeNode( WHEN );
      tmp->sons = deep_copy_CodeTree( i->sons );

      for ( l=0; l<op->num_vars; l++ ) {
	make_inst_version_of_condition( &(tmp->sons),
					l, op->inst_table[l] );
	simplify_condition_CodeNode( &(tmp->sons), FALSE, TRUE );
	if ( tmp->sons->connective == FAL ) {
	  free_CodeNode( tmp );
	  break;
	}
      }
      if ( l<op->num_vars ) {
	continue;
      }
      
      for ( l=0; l<op->num_vars; l++ ) {
	make_inst_version_of_effect( &(tmp->sons->next),
				     l, op->inst_table[l] );
      }
      tmp->next = tmpop->conditionals->sons;
      tmpop->conditionals->sons = tmp;
    }
  }

  tmpop->next = ginst_code_operators;
  ginst_code_operators = tmpop;

}



/* --> expand quantifiers
 * --> instantiate atoms, respecting inertia relations
 */
void expand_quantifiers_under_CodeNode( CodeNode **n, 
					short int var, short int constant )

{

  CodeNode *r = NULL, *tmp, *i;
  Integers *k;
  int l;
  Bool change;


  if ( !(*n) ) {
    return;
  }

  if ( (*n)->connective == ALL ) {
    if ( var != -1 ) { 
      expand_quantifiers_under_CodeNode( &((*n)->sons), var, constant );
      return;
    }

    (*n)->connective = AND;
    for ( k = (gtypes_table[(*n)->var_type]).integers; k; k=k->next ) {
      tmp = deep_copy_CodeTree( (*n)->sons );
      expand_quantifiers_under_CodeNode( &tmp, (*n)->var, k->index );
      tmp->next = r;
      r = tmp;
    }

    free_CodeNode( (*n)->sons );
    (*n)->sons = r;
    (*n)->var = 0;
    (*n)->var_type = 0;

    for ( i = (*n)->sons; i; i=i->next ) {
      expand_quantifiers_under_CodeNode( &i, -1, 0 );
    }

    return;
  }

  if ( (*n)->connective == EX ) {
    if ( var != -1 ) { 
      expand_quantifiers_under_CodeNode( &((*n)->sons), var, constant );
      return;
    }

    (*n)->connective = OR;
    for ( k = (gtypes_table[(*n)->var_type]).integers; k; k=k->next ) {
      tmp = deep_copy_CodeTree( (*n)->sons );
      expand_quantifiers_under_CodeNode( &tmp, (*n)->var, k->index );
      tmp->next = r;
      r = tmp;
    }
    
    free_CodeNode( (*n)->sons );
    (*n)->sons = r;
    (*n)->var = 0;
    (*n)->var_type = 0;

    for ( i = (*n)->sons; i; i=i->next ) {
      expand_quantifiers_under_CodeNode( &i, -1, 0 );
    }

    return;
  }
      

  if ( (*n)->connective == ATOM ) {
    if ( var == -1 ) {
      return;
    }

    if ( (*n)->predicate == -1 ) {
      if ( (*n)->arguments[0] == ((-1)*var)-1 ) {
	(*n)->arguments[0] = constant;
      }
      if ( (*n)->arguments[1] == ((-1)*var)-1 ) {
	(*n)->arguments[1] = constant;
      }
      if ( (*n)->arguments[0] == (*n)->arguments[1] ) {
	(*n)->connective = TRU;
	free_CodeNode( (*n)->sons );
	(*n)->sons = NULL;
	return;
      }
      if ( (*n)->arguments[0] > -1 &&
	   (*n)->arguments[1] > -1 &&
	   (*n)->arguments[0] != (*n)->arguments[1] ) {
      	(*n)->connective = FAL;
	free_CodeNode( (*n)->sons );
	(*n)->sons = NULL;
	return;
      }
      return;
    }

    change = FALSE;
    for ( l=0; l<garity[(*n)->predicate]; l++ ) {
      if ( (*n)->arguments[l] == ((-1)*var)-1 ) {
	(*n)->arguments[l] = constant;
	change = TRUE;
      }
    }
    if ( !change && (*n)->visited ) {
      /* we did not change anything and we've already seen that node
       * --> it cant be simplified
       */
      return;
    }
    if ( !possibly_positive( (*n)->predicate, (*n)->arguments ) ) {
      (*n)->connective = FAL;
      free_CodeNode( (*n)->sons );
      (*n)->sons = NULL;
      return;
    }
    if ( !possibly_negative( (*n)->predicate, (*n)->arguments ) ) {
      (*n)->connective = TRU;
      free_CodeNode( (*n)->sons );
      (*n)->sons = NULL;
    }
    (*n)->visited = TRUE;
    return;
  }

  if ( (*n)->connective == NOT ) {
    expand_quantifiers_under_CodeNode( &((*n)->sons), var, constant );
    return;
  }

  if ( (*n)->connective == AND ||
       (*n)->connective == OR ) {
    for ( i = (*n)->sons; i; i=i->next ) {
      expand_quantifiers_under_CodeNode( &i, var, constant );
    }
    return;
  }

  if ( (*n)->connective == TRU ||
       (*n)->connective == FAL ) {
    return;
  }
      
  printf("\nshouldn't get here: non ALL,EX,ATOM,NOT,AND,OR,TRU,FAL in expanding\n\n");
  exit( 1 );

}




/* --> instantiate atoms under inertia - constraints
 */
void make_inst_version_of_condition( CodeNode **n,
				     short int var, short int constant )

{

  CodeNode *i;
  int l;
  Bool change;

  if ( !(*n) ) {
    return;
  }

  if ( (*n)->connective == ATOM ) {
    if ( var == -1 ) {
      return;
    }

    if ( (*n)->predicate == -1 ) {
      if ( (*n)->arguments[0] == ((-1)*var)-1 ) {
	(*n)->arguments[0] = constant;
      }
      if ( (*n)->arguments[1] == ((-1)*var)-1 ) {
	(*n)->arguments[1] = constant;
      }
      if ( (*n)->arguments[0] == (*n)->arguments[1] ) {
	(*n)->connective = TRU;
	free_CodeNode( (*n)->sons );
	(*n)->sons = NULL;
	return;
      }
      if ( (*n)->arguments[0] > -1 &&
	   (*n)->arguments[1] > -1 &&
	   (*n)->arguments[0] != (*n)->arguments[1] ) {
      	(*n)->connective = FAL;
	free_CodeNode( (*n)->sons );
	(*n)->sons = NULL;
	return;
      }
      return;
    }

    change = FALSE;
    for ( l=0; l<garity[(*n)->predicate]; l++ ) {
      if ( (*n)->arguments[l] == ((-1)*var)-1 ) {
	(*n)->arguments[l] = constant;
	change = TRUE;
      }
    }
    if ( !change && (*n)->visited ) {
      return;
    }
    if ( !possibly_positive( (*n)->predicate, (*n)->arguments ) ) {
      (*n)->connective = FAL;
      free_CodeNode( (*n)->sons );
      (*n)->sons = NULL;
      return;
    }
    if ( !possibly_negative( (*n)->predicate, (*n)->arguments ) ) {
      (*n)->connective = TRU;
      free_CodeNode( (*n)->sons );
      (*n)->sons = NULL;
    }
    (*n)->visited = TRUE;
    return;
  }

  if ( (*n)->connective == NOT ) {
    expand_quantifiers_under_CodeNode( &((*n)->sons), var, constant );
    return;
  }

  if ( (*n)->connective == AND || 
       (*n)->connective == OR ) {
    for ( i = (*n)->sons; i; i=i->next ) {
      expand_quantifiers_under_CodeNode( &i, var, constant );
    }
    return;
  }
      
  if ( (*n)->connective != TRU &&
       (*n)->connective != FAL ) {
    printf("\ninst shouldn't get here: non ATOM,NOT,AND,OR,TRU,FAL %d in inst version\n\n",
	   (*n)->connective);
    exit( 1 );
  }

}



/* --> schlicht atome instanziieren.
 */
void make_inst_version_of_effect( CodeNode **n,
				  short int var, short int constant )

{

  CodeNode *i;
  int l;

  if ( !(*n) ) {
    return;
  }
  
  for ( i=(*n)->sons; i; i=i->next ) {
    if ( i->connective == NOT ) {
      for ( l=0; l<garity[i->sons->predicate]; l++ ) {
	if ( i->sons->arguments[l] == ((-1)*var)-1 ) {
	  i->sons->arguments[l] = constant;
	}
      }
      continue;
    }
    for ( l=0; l<garity[i->predicate]; l++ ) {
      if ( i->arguments[l] == ((-1)*var)-1 ) {
	i->arguments[l] = constant;
      }
    }
  }

}



/* during expansion, multiple trees of ANDs / ORs can arise.
 *
 * a little INEFFICIENT: should happen during expanding, but is
 * extremely difficult to implemet there. not that critical
 * with respect to computation time anyway, I think
 */
void merge_multiple_ANDs_and_ORs( CodeNode **n )

{

  CodeNode *i, *j, *prev, *r = NULL;

  if ( !(*n) ) {
    return;
  }

  if ( (*n)->connective == AND ) {

    for ( i = (*n)->sons; i; i = i->next ) {
      merge_multiple_ANDs_and_ORs( &i );
    }

    i = (*n)->sons;
    while ( i && i->connective == AND ) {
      if ( i->sons ) {
	for ( j=i->sons; j->next; j=j->next );
	j->next = r;
	r = i->sons;
      }

      j = i;
      i = i->next;
      free( j );
    }
    (*n)->sons = i;
    prev = i;
    if ( i ) i = i->next;
    while ( i ) {
      if ( i->connective == AND ) {
	if ( i->sons ) {
	  for ( j=i->sons; j->next; j=j->next );
	  j->next = r;
	  r = i->sons;
	}
	
	prev->next = i->next;
	j = i;
	i = i->next;
	free( j );
      } else {
	prev = prev->next;
	i = i->next;
      }
    }
    for ( j = r; j && j->next; j = j->next );
    if ( j ) {
      j->next = (*n)->sons;
      (*n)->sons = r;
    }

    return;
  }

  if ( (*n)->connective == OR ) {

    for ( i = (*n)->sons; i; i = i->next ) {
      merge_multiple_ANDs_and_ORs( &i );
    }

    i = (*n)->sons;
    while ( i && i->connective == OR ) {
      if ( i->sons ) {
	for ( j=i->sons; j->next; j=j->next );
	j->next = r;
	r = i->sons;
      }

      j = i;
      i = i->next;
      free( j );
    }
    (*n)->sons = i;
    prev = i;
    if ( i ) i = i->next;
    while ( i ) {
      if ( i->connective == OR ) {
	if ( i->sons ) {
	  for ( j=i->sons; j->next; j=j->next );
	  j->next = r;
	  r = i->sons;
	}
	
	prev->next = i->next;
	j = i;
	i = i->next;
	free( j );
      } else {
	prev = prev->next;
	i = i->next;
      }
    }
    for ( j = r; j && j->next; j = j->next );
    if ( j ) {
      j->next = (*n)->sons;
      (*n)->sons = r;
    }

    return;
  }

  if ( (*n)->connective == NOT ) {
    merge_multiple_ANDs_and_ORs( &((*n)->sons) );
    return;
  }

  if ( (*n)->connective == ATOM ) {
    return;
  }

  if ( (*n)->connective != TRU &&
       (*n)->connective != FAL ) {
    printf("\nmerge: non AND,OR,NOT,ATOM,TRU,FAL node %d in expanded condition\n\n",
	   (*n)->connective);
    exit( 1 );
  }

}




