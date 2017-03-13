





/*********************************************************************
 * File: instantiateII.c
 *
 * Description:
 *
 * code for handling transformations on CodeNodes that can occur
 * during (almost) all stages of instantiation:
 *
 *                        - detect tautologies in PLI conditions
 *
 *                        - simplify a PLI condition
 *                        - detect trivial or inconsistent or
 *                          never applicable effects
 *                        - simplify an effect list according to that
 *                        - detect unused variables, remove them
 *                        - clean up operators
 *                       
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











/*
 *  ----------------------- DETECT TAUTOLOGIES, LIKE A \WEDGE \NEG A ETC. ... ------------
 */












void detect_all_tautologies( Bool warnings )

{

  CodeOperator *i;
  CodeNode *n, *nn;

  if ( gcmd_line.display_info == 0 ) {
    warnings = FALSE;
  }

  detect_tautologies_in_condition_CodeNode( &gcode_initial_state, warnings );
  detect_tautologies_in_condition_CodeNode( &gcode_goal_state, warnings );

  for ( i = gcode_operators; i; i = i->next ) {
    detect_tautologies_in_condition_CodeNode( &(i->preconds), warnings );
    if ( i->conditionals ) {
      for ( n = i->conditionals->sons; n; n = n->next ) {
	for ( nn = n; nn && nn->connective != WHEN; nn = nn->sons );
	detect_tautologies_in_condition_CodeNode( &(nn->sons), warnings );
	detect_tautologies_in_condition_CodeNode( &(nn->sons->next), warnings );
      }
    }
  }

}



void detect_inst_CodeOperator_tautologies( void )

{

  CodeOperator *i;
  CodeNode *n, *nn;

  for ( i = gcode_operators; i; i = i->next ) {
    detect_tautologies_in_condition_CodeNode( &(i->preconds), FALSE );
    if ( i->conditionals ) {
      for ( n = i->conditionals->sons; n; n = n->next ) {
	for ( nn = n; nn && nn->connective != WHEN; nn = nn->sons );
	detect_tautologies_in_condition_CodeNode( &(nn->sons), FALSE );
	detect_tautologies_in_condition_CodeNode( &(nn->sons->next), FALSE);
      }
    }
  }

}



void detect_tautologies_in_condition_CodeNode( CodeNode **n, Bool warnings )

{

  CodeNode *i, *j, *prev;

  if ( !(*n) ) {
    return;
  }

  if ( (*n)->connective == NOT ||
       (*n)->connective == ALL ||
       (*n)->connective == EX ) {
    detect_tautologies_in_condition_CodeNode( &(*n)->sons, warnings );
  }

  if ( (*n)->connective == AND ||
       (*n)->connective == OR ) {
    for ( i = (*n)->sons; i; i = i->next ) {
      detect_tautologies_in_condition_CodeNode( &i, warnings );
    }
    for ( i = (*n)->sons; i && i->next; i = i->next ) {
      j = i->next;
      prev = i;
      while ( j ) {
	if ( are_identical_CodeNodes( i, j ) ) {
	  if ( warnings ) {
	    printf("\nwarning: detected identical trees in con/dis junction");
	  }
	  prev->next = j->next;
	  free_CodeNode( j->sons );
	  free( j );
	  j = prev->next;
	  continue;
	}
	if ( i->connective == NOT &&
	     are_identical_CodeNodes( i->sons, j ) ) {
	  if ( warnings ) {
	    printf("\nwarning: detected A, neg A tautology");
	  }
	  (*n)->connective = ((*n)->connective == AND) ? FAL : TRU;
	  free_CodeNode( (*n)->sons );
	  (*n)->sons = NULL;
	  return;
	}
	if ( j->connective == NOT &&
	     are_identical_CodeNodes( i, j->sons ) ) {
	  if ( warnings ) {
	    printf("\nwarning: detected A, neg A tautology");
	  }
	  (*n)->connective = ((*n)->connective == AND) ? FAL : TRU;
	  free_CodeNode( (*n)->sons );
	  (*n)->sons = NULL;
	  return;
	}
	prev = j;
	j = j->next;
      }
    }
  }

  if ( (*n)->connective == ATOM &&
       (*n)->predicate == -1 &&
       (*n)->arguments[0] == (*n)->arguments[1] ) {
    if ( warnings ) {
      printf("\nwarning: detected EQ predicate with identical arguments");
    }
    (*n)->connective = TRU;
  }

  if ( (*n)->connective == ATOM &&
       (*n)->predicate == -1 &&
       (*n)->arguments[0] > -1 &&
       (*n)->arguments[1] > -1 &&
       (*n)->arguments[0] != (*n)->arguments[1] ) {
    if ( warnings ) {
      printf("\nwarning: detected EQ predicate on different objects!");
    }
    (*n)->connective = FAL;
  }

}



Bool are_identical_CodeNodes( CodeNode *n1, CodeNode *n2 )

{

  int i;
  CodeNode *c, *d;

  if ( !n1 && !n2 ) {
    return TRUE;
  }
  if ( !n1 || !n2 ) {
    return FALSE;
  }

  if ( n1->connective != n2->connective ) {
    return FALSE;
  }

  if ( n1->connective == AND ||
       n1->connective == OR ) {
    d = n2->sons;
    for ( c = n1->sons; c; c = c->next ) { 
      if ( !are_identical_CodeNodes( c, d ) ) {
	return FALSE;
      }
      d = d->next;
    }
    if ( d ) {
      return FALSE;
    }      
  } else {
    if ( n1->connective == ALL ||
	 n1->connective == EX ) {
      if ( n1->var_type != n2->var_type ) {
	return FALSE;
      }
    }
    if ( !are_identical_CodeNodes( n1->sons, n2->sons ) ) {
      return FALSE;
    }
  }
  
  if ( n1->connective == ATOM ) {
    if ( n1->predicate != n2->predicate ) {
      return FALSE;
    }
    for ( i=0; i<garity[n1->predicate]; i++ ) {
      if ( n1->arguments[i] != n2->arguments[i] ) {
	return FALSE;
      }
    }
  }

  return TRUE;

}














/*
 *  ----------------------- LOGICAL SIMPLIFICATIONS ON ALL NODE STRUCTURES ---------------
 */















void simplify_all_CodeNodes( Bool warnings )

{

  if ( gcmd_line.display_info == 0 ) {
    warnings = FALSE;
  }

  simplify_condition_CodeNode( &gcode_initial_state, warnings, FALSE );

  simplify_condition_CodeNode( &gcode_goal_state, warnings, TRUE );
  remove_unused_vars_under_CodeNode( &gcode_goal_state, warnings );
  rename_vars_under_CodeNode( &gcode_goal_state, 0 );
  if ( !gcode_goal_state ||
       gcode_goal_state->connective == TRU ) {
    times( &gend );
    gtotal_time += ( float ) ( ( gend.tms_utime - gstart.tms_utime 
				 + gend.tms_stime - gstart.tms_stime ) / 100.0 );
    printf("\n\n\nipp: goal can be simplified to TRUE. no plan needed\n");
    printf("\nelapsed time: %7.2f seconds\n\n", gtotal_time);
    exit( 1 );
  }
  if ( gcode_goal_state->connective == FAL ) {
    times( &gend );
    gtotal_time += ( float ) ( ( gend.tms_utime - gstart.tms_utime 
				 + gend.tms_stime - gstart.tms_stime ) / 100.0 );
    printf("\n\n\nipp: goal can be simplified to FALSE. no plan will solve it\n");
    printf("\nelapsed time: %7.2f seconds\n\n", gtotal_time);
    exit( 1 );
  }


  simplify_all_CodeOperators( warnings );

}



void simplify_all_CodeOperators( Bool warnings )

{

  CodeOperator *i, *j, *prev;
  CodeNode *n, *nn;
  int l, k;

  for ( i = gcode_operators; i; i = i->next ) {
    simplify_condition_CodeNode( &(i->preconds), warnings, TRUE );
    if ( i->conditionals ) {
      for ( n = i->conditionals->sons; n; n = n->next ) {
	for ( nn = n; nn->connective != WHEN; nn = nn->sons );
	simplify_condition_CodeNode( &(nn->sons), warnings, TRUE );
	simplify_condition_CodeNode( &(nn->sons->next), warnings, FALSE );
      }
      clean_up_effects( &(i->conditionals), warnings );
    }

    l = 0;
    while ( l < i->num_vars ) {
      if ( !var_used_under_CodeNode( l, i->preconds ) &&
	   !var_used_under_CodeNode( l, i->conditionals ) ) {      
	if ( warnings ) {
	  printf("\nwarning: parameter x%d of op %s is not used. skipping it",
		 l, i->name);
	}
	for ( k = l; k<i->num_vars-1; k++ ) {
	  i->var_types[k] = i->var_types[k+1];
	}
	i->num_vars--;
      } else {
	l++;
      }
    }
    remove_unused_vars_under_CodeNode( &(i->preconds), warnings );
    rename_vars_under_CodeNode( &(i->preconds), i->num_vars );
    remove_unused_vars_under_CodeNode( &(i->conditionals), warnings );
    rename_vars_under_CodeNode( &(i->conditionals), i->num_vars );
  }

  i = gcode_operators;
  while ( i && 
	  i->preconds &&
	  i->preconds->connective == FAL ) {
    if ( warnings ) {
      printf("\nwarning: op %s preconds simplify to FALSE. skipping it.",
	     i->name);
    }
    j = i;
    i = i->next;
    free_CodeOperator( j );
  }
  gcode_operators = i;
  prev = i;
  if ( i ) i = i->next;
  while ( i ) {
    if ( i->preconds &&
	 i->preconds->connective == FAL ) {
      if ( warnings ) {
	printf("\nwarning: op %s preconds simplify to FALSE. skipping it.",
	       i->name);
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


  i = gcode_operators;
  while ( i && 
	  ( !(i->conditionals) ||
	    !(i->conditionals->sons) ) ) {
    if ( warnings ) {
      printf("\nwarning: op %s has no effects. skipping it.",
	     i->name);
    }
    j = i;
    i = i->next;
    free_CodeOperator( j );
  }
  gcode_operators = i;
  prev = i;
  if ( i ) i = i->next;
  while ( i ) {
    if ( !(i->conditionals) ||
	 !(i->conditionals->sons) ) {
      if ( warnings ) {
	printf("\nwarning: op %s has no effects. skipping it.",
	       i->name);
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

  i = gcode_operators;
  while ( i && 
	  has_contradicting_unconditional( i->conditionals ) ) {
    if ( warnings ) {
      printf("\nwarning: op %s has contradicting unconditional effect. skipping it.",
	     i->name);
    }
    j = i;
    i = i->next;
    free_CodeOperator( j );
  }
  gcode_operators = i;
  prev = i;
  if ( i ) i = i->next;
  while ( i ) {
    if ( has_contradicting_unconditional( i->conditionals ) ) {
      if ( warnings ) {
	printf("\nwarning: op %s has contradicting unconditional effect. skipping it.",
	       i->name);
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

}



void simplify_all_inst_CodeOperators( void )

{

  CodeOperator *i, *j, *prev;
  CodeNode *n, *nn;

  for ( i = ginst_code_operators; i; i = i->next ) {
    simplify_condition_CodeNode( &(i->preconds), FALSE, TRUE );
    if ( i->conditionals ) {
      for ( n = i->conditionals->sons; n; n = n->next ) {
	for ( nn = n; nn->connective != WHEN; nn = nn->sons );
	simplify_condition_CodeNode( &(nn->sons), FALSE, TRUE );
	simplify_condition_CodeNode( &(nn->sons->next), FALSE, FALSE );
      }
      clean_up_effects( &(i->conditionals), FALSE );
    }
  }

  i = ginst_code_operators;
  while ( i && 
	  i->preconds &&
	  i->preconds->connective == FAL ) {
    j = i;
    i = i->next;
    free_CodeOperator( j );
  }
  ginst_code_operators = i;
  prev = i;
  if ( i ) i = i->next;
  while ( i ) {
    if ( i->preconds &&
	 i->preconds->connective == FAL ) {
      prev->next = i->next;
      j = i;
      i = i->next;
      free_CodeOperator( j );
    } else {
      prev = prev->next;
      i = i->next;
    }
  }


  i = ginst_code_operators;
  while ( i && 
	  ( !(i->conditionals) ||
	    !(i->conditionals->sons) ) ) {
    j = i;
    i = i->next;
    free_CodeOperator( j );
  }
  ginst_code_operators = i;
  prev = i;
  if ( i ) i = i->next;
  while ( i ) {
    if ( !(i->conditionals) ||
	 !(i->conditionals->sons) ) {
      prev->next = i->next;
      j = i;
      i = i->next;
      free_CodeOperator( j );
    } else {
      prev = prev->next;
      i = i->next;
    }
  }

  i = ginst_code_operators;
  while ( i && 
	  has_contradicting_unconditional( i->conditionals ) ) {
    j = i;
    i = i->next;
    free_CodeOperator( j );
  }
  ginst_code_operators = i;
  prev = i;
  if ( i ) i = i->next;
  while ( i ) {
    if ( has_contradicting_unconditional( i->conditionals ) ) {
      prev->next = i->next;
      j = i;
      i = i->next;
      free_CodeOperator( j );
    } else {
      prev = prev->next;
      i = i->next;
    }
  }

}



void simplify_condition_CodeNode( CodeNode **n, Bool warnings, Bool condition )

{

  CodeNode *i, *j, *prev;
  int count = 0;

  if ( !(*n) ) {
    return;
  }

  if ( (*n)->connective == ATOM ) {
    if ( (*n)->predicate == -1 ) {
      if ( (*n)->arguments[0] < 0 ||
	   (*n)->arguments[1] < 0 ) {
	return;
      }
      if ( (*n)->arguments[0] == (*n)->arguments[1] ) {
	if ( warnings ) {
	  printf("\ndetected eq predicate on equal constants");
	}
	(*n)->connective = TRU;
	free_CodeNode( (*n)->sons );
	(*n)->sons = NULL;
	return;
      } else {
	if ( warnings ) {
	  printf("\ndetected eq predicate on different constants");
	}
      	(*n)->connective = FAL;
	free_CodeNode( (*n)->sons );
	(*n)->sons = NULL;
	return;
      }
    }

    return;
  }

  if ( (*n)->connective == NOT ) {
    simplify_condition_CodeNode( &((*n)->sons), warnings, condition );
    if ( (*n)->sons->connective == TRU ) {
      if ( warnings ) {
	printf("\npropagating TRUE over NOT");
      }
      (*n)->connective = FAL;
      free_CodeNode( (*n)->sons );
      (*n)->sons = NULL;
      return;
    }
    if ( (*n)->sons->connective == FAL ) {
      if ( warnings ) {
	printf("\npropagating FALSE over NOT");
      }
      (*n)->connective = TRU;
      free_CodeNode( (*n)->sons );
      (*n)->sons = NULL;
      return;
    }
    return;
  }

  if ( (*n)->connective == AND ) {
    for ( i = (*n)->sons; i; i=i->next ) {
      simplify_condition_CodeNode( &i, warnings, condition );
      if ( i->connective == FAL ) {
	if ( warnings ) {
	  printf("\nson of AND got FALSE");
	}
	free_CodeNode( (*n)->sons );
	(*n)->sons = NULL;
	(*n)->connective = FAL;
	return;
      }
    }
    i = (*n)->sons;
    while ( i && i->connective == TRU ) {
      if ( warnings ) {
	printf("\nson of AND got TRU");
      }
      j = i;
      i = i->next;
      free_CodeNode( j->sons );
      free( j );
    }
    (*n)->sons = i;
    prev = i;
    if ( i ) {
      i = i->next;
      count++;
    }
    while ( i ) {
      if ( i->connective == TRU ) {
	if ( warnings ) {
	  printf("\nson of AND got TRU");
	}
	prev->next = i->next;
	j = i;
	i = i->next;
	free_CodeNode( j->sons );
	free( j );
      } else {
	count++;
	prev = prev->next;
	i = i->next;
      }
    }
    if ( count == 0 ) {
      if ( warnings ) {
	printf("\nAND has no sons (anymore)");
      }
      free_CodeNode( (*n)->sons );
      (*n)->sons = NULL;
      (*n)->connective = TRU;
      return;
    }
    if ( count == 1 && condition ) {
      if ( warnings ) {
	printf("\nAND has only one son (anymore)");
      }
      i = (*n)->sons;
      copy_contents_of_CodeNode( n, i );
      (*n)->sons = i->sons;
      free( i );
      return;
    }
    return;
  }
      
  if ( (*n)->connective == OR ) {
    for ( i = (*n)->sons; i; i=i->next ) {
      simplify_condition_CodeNode( &i, warnings, condition );
      if ( i->connective == TRU ) {
	if ( warnings ) {
	  printf("\nson of OR got TRUE");
	}
	free_CodeNode( (*n)->sons );
	(*n)->sons = NULL;
	(*n)->connective = TRU;
	return;
      }
    }
    i = (*n)->sons;
    while ( i && i->connective == FAL ) {
      if ( warnings ) {
	printf("\nson of OR got FALSE");
      }
      j = i;
      i = i->next;
      free_CodeNode( j->sons );
      free( j );
    }
    (*n)->sons = i;
    prev = i;
    if ( i ) {
      i = i->next;
      count++;
    }
    while ( i ) {
      if ( i->connective == FAL ) {
	if ( warnings ) {
	  printf("\nson of OR got FALSE");
	}
	prev->next = i->next;
	j = i;
	i = i->next;
	free_CodeNode( j->sons );
	free( j );
      } else {
	count++;
	prev = prev->next;
	i = i->next;
      }
    }
    if ( count == 0 ) {
      if ( warnings ) {
	printf("\nOR has no sons (anymore)");
      }
      free_CodeNode( (*n)->sons );
      (*n)->sons = NULL;
      (*n)->connective = FAL;
      return;
    }
    if ( count == 1 && condition ) {
      if ( warnings ) {
	printf("\nOR has only one son (anymore)");
      }
      i = (*n)->sons;
      copy_contents_of_CodeNode( n, i );
      (*n)->sons = i->sons;
      free( i );
      return;
    }
    return;
  }

  if ( (*n)->connective == ALL ||
       (*n)->connective == EX ) {
    if ( !((*n)->sons) ) {
      return;
    }
    simplify_condition_CodeNode( &(*n)->sons, warnings, condition );
    if ( (*n)->sons->connective == TRU ) {
      if ( warnings ) {
	printf("\npropagating TRUE over ALL/EX");
      }
      free_CodeNode( (*n)->sons );
      (*n)->connective = TRU;
      return;
    }
    if ( (*n)->sons &&
	 (*n)->sons->connective == FAL ) {
      if ( warnings ) {
	printf("\npropagating FALSE over ALL/EX");
      }
      free_CodeNode( (*n)->sons );
      (*n)->connective = FAL;
      return;
    }
    return;
  }

  if ( (*n)->connective != TRU &&
       (*n)->connective != FAL ) {
    printf("\nsimplify shouldn't get here: non ATOM,NOT,AND,OR,ALL,EX,FAL,TRU %s in cond\n\n",
	   gconnectives[(*n)->connective] );
  }


}



void clean_up_effects( CodeNode **n, Bool warnings )

{

  CodeNode *i, *j, *prev;
  int count = 0;

  i = (*n)->sons;
  while ( i && 
	  ( impossible_effect( i, warnings ) ||
	    no_transition_effect( i, warnings ) ) ) {
    if ( warnings ) {
      printf("\nremoving effect");
    }
    j = i;
    i = i->next;
    free_CodeNode( j->sons );
    free( j );
  }
  (*n)->sons = i;
  prev = i;
  if ( i ) {
    i = i->next;
    count++;
  }
  while ( i ) {
    if ( impossible_effect( i, warnings ) ||
	 no_transition_effect( i, warnings ) ) {
      if ( warnings ) {
	printf("\nremoving effect");
      }
      prev->next = i->next;
      j = i;
      i = i->next;
      free_CodeNode( j->sons );
      free( j );
    } else {
      count++;
      prev = prev->next;
      i = i->next;
    }
  }

  if ( count == 0 ) {
    free_CodeNode( (*n) );
    (*n) = NULL;
  }
   

}



Bool impossible_effect( CodeNode *n, Bool warnings )

{

  CodeNode *nn;

  for ( nn = n; nn->connective != WHEN; nn = nn->sons );
  if ( nn->sons->connective == FAL ) {
    if ( warnings ) {
      printf("\ndetected impossible effect");
    }
    return TRUE;
  }

  return FALSE;

}



Bool no_transition_effect( CodeNode *n, Bool warnings )

{

  CodeNode *nn;

  for ( nn = n; nn->connective != WHEN; nn = nn->sons );
  if ( nn->sons->next->connective == TRU ) {
    if ( warnings ) {
      printf("\neffect causes no state transition");
    }
    return TRUE;
  }

  return FALSE;

}



Bool has_contradicting_unconditional( CodeNode *n )

{

  CodeNode *i;

  for ( i = n->sons; i; i = i->next ) {
    if ( contradicting_unconditional_effect( i ) ) {
      return TRUE;
    }
  }

  return FALSE;

}



Bool contradicting_unconditional_effect( CodeNode *n )

{


  CodeNode *nn;

  for ( nn = n; nn->connective != WHEN; nn = nn->sons );
  if ( nn->sons->connective == TRU &&
       nn->sons->next->connective == FAL ) {
    return TRUE;
  }

  return FALSE;

}



Bool var_used_under_CodeNode( int var_num, CodeNode *n )

{

  int i;

  if ( !n ) {
    return FALSE;
  }

  if ( n->connective == ATOM ) {
    if ( n->predicate == -1 ) {
      if ( n->arguments[0] == ((-1)*var_num)-1 ) {
	return TRUE;
      }
      if ( n->arguments[1] == ((-1)*var_num)-1 ) {
	return TRUE;
      }
    } else {
      for ( i=0; i<garity[n->predicate]; i++ ) {
	if ( n->arguments[i] == ((-1)*var_num)-1 ) {
	  return TRUE;
	}
      }
    }
  }

  if ( var_used_under_CodeNode( var_num, n->next ) ) {
    return TRUE;
  }
  if ( var_used_under_CodeNode( var_num, n->sons ) ) {
    return TRUE;
  }

  return FALSE;

}



void remove_unused_vars_under_CodeNode( CodeNode **n, Bool warnings )

{

  if ( !(*n) ) {
    return;
  }  

  rec_remove_unused_vars_under_CodeNode( n, warnings );

  if ( (*n)->connective == EMPTY ) {
    free( *n );
    *n = NULL;
  }

}



void rec_remove_unused_vars_under_CodeNode( CodeNode **n, Bool warnings )

{

  CodeNode *tmp;

  if ( !(*n) ) {
    return;
  }

  rec_remove_unused_vars_under_CodeNode( &((*n)->next), warnings );
  if ( (*n)->next && 
       (*n)->next->connective == EMPTY ) {
    free( (*n)->next );
    (*n)->next = NULL;
  }
  rec_remove_unused_vars_under_CodeNode( &((*n)->sons), warnings );
  if ( (*n)->sons && 
       (*n)->sons->connective == EMPTY ) {
    free( (*n)->sons );
    (*n)->sons = NULL;
  }

  if ( (*n)->connective == ALL ||
       (*n)->connective == EX ) {
    if ( !var_used_under_CodeNode( (*n)->var, (*n)->sons ) ) {
      if ( warnings ) {
	printf("\nwarning: quantified variable x%d is not used. removing it",
	       (*n)->var );
      }
      if ( !((*n)->sons) ) {
	if ( !((*n)->next) ) {
	  (*n)->connective = EMPTY;
	  return;
	}
	copy_contents_of_CodeNode( n, (*n)->next );
	tmp = (*n)->next;
	(*n)->sons = (*n)->next->sons;
	(*n)->next = (*n)->next->next;
	free( tmp );
	return;
      }
      copy_contents_of_CodeNode( n, (*n)->sons );
      tmp = (*n)->sons;
      (*n)->sons = (*n)->sons->sons;
      /* pointer (*n)->sons->next gets lost!!
       *
       * make use of the fact, that this pointer can not have a ->next
       */
      free( tmp );
    }
  }

}




void rename_vars_under_CodeNode( CodeNode **n, int num_vars )

{

  if ( !(*n) ) {
    return;
  }

  if ( (*n)->connective == ALL ||
       (*n)->connective == EX ) {
    if ( (*n)->var != num_vars ) {
      replace_var_with_newvar_under_CodeNode( (*n)->var, num_vars, 
					      &((*n)->sons) );
      (*n)->var = num_vars;
    }
    rename_vars_under_CodeNode( &((*n)->sons), num_vars+1 );
    rename_vars_under_CodeNode( &((*n)->next), num_vars );
    return;
  }

  rename_vars_under_CodeNode( &((*n)->next), num_vars );
  rename_vars_under_CodeNode( &((*n)->sons), num_vars );

}



void replace_var_with_newvar_under_CodeNode( int var, int new_var, 
					     CodeNode **n )

{

  int i;

  if ( !(*n) ) {
    return;
  }

  if ( (*n)->connective == ATOM ) {
    if ( (*n)->predicate == -1 ) {
      if ( (*n)->arguments[0] == ((-1)*var)-1 ) {
	(*n)->arguments[0] = ((-1)*new_var)-1;
      }
      if ( (*n)->arguments[1] == ((-1)*var)-1 ) {
	(*n)->arguments[1] = ((-1)*new_var)-1;
      }
    } else {
      for ( i=0; i<garity[(*n)->predicate]; i++ ) {
	if ( (*n)->arguments[i] == ((-1)*var)-1 ) {
	  (*n)->arguments[i] = ((-1)*new_var)-1;
	}
      }
    }
  }

  replace_var_with_newvar_under_CodeNode( var, new_var, 
					  &((*n)->next) );
  replace_var_with_newvar_under_CodeNode( var, new_var, 
					  &((*n)->sons) );
 
}


 
