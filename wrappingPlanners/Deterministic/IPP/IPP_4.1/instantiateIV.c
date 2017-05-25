





/*********************************************************************
 * File: instantiateIV.c
 *
 * Description: routines that perform the final instantiation 
 *
 *                        - collect relevant facts and adjust ATOMs to them
 *
 *                        - create BitMap representation of domain
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

#include "pddl.h"

#include "instantiateI.h"
#include "instantiateII.h"
#include "instantiateIII.h"
#include "instantiateIV.h"

 








/* ------------------------------ COLLECT RELEVANT FACTS -----------------
 */










/* a bit INEFFICIENT: all but lindex are 1/0 decisions, so it would be sufficient
 * to use BitVectors here. Not a big deal, though, I guess, as memory is freed
 * afterwards anyway.
 */
int_pointer lpos[MAX_PREDICATES_TABLE];
int_pointer lneg[MAX_PREDICATES_TABLE];
int_pointer luse[MAX_PREDICATES_TABLE];
int_pointer lindex[MAX_PREDICATES_TABLE];







void collect_relevant_facts( void )

{

  int i, j, size;
  CodeOperator *oo;
  CodeNode *n;

  for ( i=0; i<gpredicates_table_size; i++ ) {
    size = 1;
    for ( j=0; j<garity[i]; j++ ) {
      size *= gconstants_table_size;
    }
    lpos[i] = ( int_pointer ) calloc( size, sizeof( int ) );
    lneg[i] = ( int_pointer ) calloc( size, sizeof( int ) );
    luse[i] = ( int_pointer ) calloc( size, sizeof( int ) );
    lindex[i] = ( int_pointer ) calloc( size, sizeof( int ) );
    for ( j=0; j<size; j++ ) {
      lpos[i][j] = 0;
      lneg[i][j] = 0;
      luse[i][j] = 0;
      lindex[i][j] = -1;
    }
  }


  if ( gcode_initial_state ) {
    for ( n = gcode_initial_state->sons; n; n=n->next ) {
      lpos[n->predicate][pos_neg_use_index_adress( n->predicate, n->arguments )] = 1;
    }
  }
  for ( oo = ginst_code_operators; oo; oo=oo->next ) {
    if ( oo->conditionals ) {
      /* ops come from multiplying without clean up, so effects can be
       * empty here
       */
      for ( n = oo->conditionals->sons; n; n = n->next ) {
	make_pos_neg_use_index_entries( n->sons->next );
      }
    }
  }

  /* now the relevant atoms are exactly those which
   * have a one at their adress in both lpos[] and lneg[] table.
   * these atoms have their fact index set in the lindex[]
   * table (at the appropriate adress), and, for debugging
   * reasons, their predicate and arguments are stored 
   * at their index in grelevant_facts[] table.
   */

  if ( gcmd_line.display_info == 7 ) {
    printf("\nselected the following facts as relevant:\n");
    for ( i=0; i<gnum_relevant_facts; i++ ) {
      printf("\n%d: ", i);
      print_fact( grelevant_facts[i]->predicate, grelevant_facts[i]->arguments );
    }
    printf("\ntotal number of relevant facts: %d\n\n", gnum_relevant_facts);
  }


  set_relevants_in_condition( &gcode_initial_state );
  simplify_condition_CodeNode( &gcode_initial_state, FALSE, FALSE );

  set_relevants_in_condition( &gcode_goal_state );
  detect_tautologies_in_condition_CodeNode( &gcode_goal_state, FALSE );
  simplify_condition_CodeNode( &gcode_goal_state, FALSE, TRUE );
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
 

  for ( oo = ginst_code_operators; oo; oo=oo->next ) {
    set_relevants_in_condition( &(oo->preconds) );
    if ( oo->conditionals ) {
      for ( n = oo->conditionals->sons; n; n = n->next ) {
	set_relevants_in_condition( &(n->sons) );
	set_relevants_in_effect( &(n->sons->next) );
      }
    }
  }


  for ( i=0; i<gpredicates_table_size; i++ ) {
    free ( lpos[i] );
    free ( lneg[i] );
    free ( luse[i] );
    free ( lindex[i] );
  }

  gft_vector_length = ( ( int ) gnum_relevant_facts / gcword_size );
  if ( ( gnum_relevant_facts % gcword_size ) > 0 ) gft_vector_length++;


}



void set_relevants_in_condition( CodeNode **n )

{

  int adr;
  CodeNode *i;

  if ( !(*n) ) {
    return;
  }

  if ( (*n)->connective == ATOM ) {
    if ( (*n)->predicate == -1 ) {
      printf("\neq predicate in fully instantiated formula! ERROR\n\n");
      exit( 1 );
    }
    adr = pos_neg_use_index_adress( (*n)->predicate, (*n)->arguments );
    if ( !lpos[(*n)->predicate][adr] ) {
      if ( gcmd_line.display_info == 102 ) {
	printf("\ndetected irrelevant (only negative) fact: ");
	print_fact( (*n)->predicate, (*n)->arguments );
      }
      (*n)->connective = FAL;
      free_CodeNode( (*n)->sons );
      (*n)->sons = NULL;
      return;
    }
    if ( !lneg[(*n)->predicate][adr] ) {
      if ( gcmd_line.display_info == 102 ) {
	printf("\ndetected irrelevant (only positive) fact: ");
	print_fact( (*n)->predicate, (*n)->arguments );
      }
      (*n)->connective = TRU;
      free_CodeNode( (*n)->sons );
      (*n)->sons = NULL;
      return;
    }
    /* fact is relevant;
     * store it's index, for simplicity into (*n)->var:
     * at this point, we don't need this int anymore
     */
    (*n)->var = lindex[(*n)->predicate][adr];
    return;
  }

  if ( (*n)->connective == NOT ) {
    set_relevants_in_condition( &((*n)->sons) );
    return;
  }

  if ( (*n)->connective == AND ||
       (*n)->connective == OR ) {
    for ( i = (*n)->sons; i; i=i->next ) {
      set_relevants_in_condition( &i );
    }
    return;
  }

  if ( (*n)->connective != TRU &&
       (*n)->connective != FAL ) {
    printf("\nrelevants shouldn't get here: non ATOM,NOT,AND,OR,TRU,FAL %d in condition\n\n",
	   (*n)->connective);
    exit( 1 );
  }

}



void set_relevants_in_effect( CodeNode **n )

{

  CodeNode *i;
  int adr;

  for ( i = (*n)->sons; i; i = i->next ) {
    if ( i->connective == NOT ) {
      adr = pos_neg_use_index_adress( i->sons->predicate, i->sons->arguments );
      if ( !luse[i->sons->predicate][adr] ) {
	i->sons->var = -1;
	continue;
      }
      i->sons->var = lindex[i->sons->predicate][adr];
      continue;
    }
    adr = pos_neg_use_index_adress( i->predicate, i->arguments );
    i->var = lindex[i->predicate][adr];
  }

}



int pos_neg_use_index_adress( int predicate, ArgArray arguments )

{

  int r = 0, b = 1, i;

  for ( i=garity[predicate]-1; i>-1; i-- ) {
    r += b * arguments[i];
    b *= gconstants_table_size;
    /* NOTE: we could use less space here by relying on the type
     *       defined for argument i of predicate 
     */
  }

  return r;

}



void make_pos_neg_use_index_entries( CodeNode *n )

{

  int adr;
  CodeNode *nn;
  RelevantFact *tmp;

  for ( nn = n->sons; nn; nn=nn->next ) {
    
    if ( nn->connective == NOT ) {
      adr = pos_neg_use_index_adress( nn->sons->predicate, nn->sons->arguments );
      lneg[nn->sons->predicate][adr] = 1;
      if ( lpos[nn->sons->predicate][adr] &&
	   !luse[nn->sons->predicate][adr] ) {
	if ( gnum_relevant_facts == MAX_RELEVANT_FACTS ) {
	  printf("\nincrease MAX_RELEVANT_FACTS! (current value: %d)\n\n",
		 MAX_RELEVANT_FACTS);
	  exit( 1 );
	}
	/* new relevant predicate!
	 *
	 * here, we can have pos[] several times. to make sure each fact
	 * is made relevant only once, we need use - information.
	 *
	 * NOTE: the facts that get here are exactly the initials that can be
	 *       deleted
	 */
	luse[nn->sons->predicate][adr] = 1;
	lindex[nn->sons->predicate][adr] = gnum_relevant_facts;
	tmp = new_RelevantFact( nn->sons );
	grelevant_facts[gnum_relevant_facts++] = tmp;
      }
    } else {
      adr = pos_neg_use_index_adress( nn->predicate, nn->arguments );
      if ( !lpos[nn->predicate][adr] ) {
	if ( gnum_relevant_facts == MAX_RELEVANT_FACTS ) {
	  printf("\nincrease MAX_RELEVANT_FACTS! (current value: %d)\n\n",
		 MAX_RELEVANT_FACTS);
	  exit( 1 );
	}
	/* new added fact, not in initial --> relevant!
	 *
	 * as relevant facts all have pos[] set, we can have this only
	 * once per fact.
	 */
	lpos[nn->predicate][adr] = 1;
	lneg[nn->predicate][adr] = 1;
	luse[nn->predicate][adr] = 1;
	lindex[nn->predicate][adr] = gnum_relevant_facts;
	tmp = new_RelevantFact( nn );
	grelevant_facts[gnum_relevant_facts++] = tmp;
      }
    }
  }

}












/* ---------------------  GENERATE BITMAP REPRESENTATION   -----------------
 */












void generate_bitmap_representation( void )

{

  CodeOperator *i, *j;

  generate_ini_goal_bitmap_representation();

  i = ginst_code_operators;
  while ( i ) {
    generate_BitOperators( i );
    j = i;
    i = i->next;
    free_CodeOperator( j );
  }

}



void generate_ini_goal_bitmap_representation( void )

{

  FactInfo *tpos = new_FactInfo();
  FactInfo *tneg = new_FactInfo();
  CodeNode *i, *l;
  int j;
  RelevantFact *tmp;
  BitOperator *tmpop;

  /* goal state is non trivial, otherwise we would have stopped already.
   */
  dnf( gcode_goal_state );
  if ( gcode_goal_state->connective != OR ) {
    if ( gcode_goal_state->connective != AND ) {
      if ( gcode_goal_state->connective == NOT ) {
	make_entry_in_FactInfo( &tneg, gcode_goal_state->sons->var );
      } else {
	make_entry_in_FactInfo( &tpos, gcode_goal_state->var );
      }
    } else {
      for ( i = gcode_goal_state->sons; i; i = i->next ) {
	if ( i->connective == NOT ) {
	  make_entry_in_FactInfo( &tneg, i->sons->var );
	} else {
	  make_entry_in_FactInfo( &tpos, i->var );
	}
      }
    }
  } else {
    /* disjunctive goals! introduce new GOAL-REACHED fact
     */
    if ( gpredicates_table_size == MAX_PREDICATES_TABLE ) {
      printf("\ntoo many predicates! increase MAX_PREDICATES_TABLE (currently %d)\n\n",
	     MAX_PREDICATES_TABLE);
      exit( 1 );
    }
    gpredicates_table[gpredicates_table_size] = ggoal_reached_name;
    garity[gpredicates_table_size] = 0;
    gpredicates_table_size++;
    if ( gnum_relevant_facts == MAX_RELEVANT_FACTS ) {
      printf("\nincrease MAX_RELEVANT_FACTS! (current value: %d)\n\n",
	     MAX_RELEVANT_FACTS);
      exit( 1 );
    }
    tmp = new_RelevantFact( NULL );
    tmp->predicate = gpredicates_table_size - 1;
    grelevant_facts[gnum_relevant_facts++] = tmp;
    /* adjust the vector length to additional fact
     */
    gft_vector_length = ( ( int ) gnum_relevant_facts / gcword_size );
    if ( ( gnum_relevant_facts % gcword_size ) > 0 ) gft_vector_length++;

    /* now make an operator for each disjunkt
     */
    for ( i = gcode_goal_state->sons; i; i = i->next ) {
      tmpop = new_BitOperator( ggoal_reached_name );
      tmpop->num_vars = 0;
      if ( i->connective != AND ) {
	if ( i->connective == NOT ) {
	  make_entry_in_FactInfo( &(tmpop->n_preconds), i->sons->var );
	} else {
	  make_entry_in_FactInfo( &(tmpop->p_preconds), i->var );
	}
      } else {
	for ( l = i->sons; l; l = l->next ) {
	  if ( l->connective == NOT ) {
	    make_entry_in_FactInfo( &(tmpop->n_preconds), l->sons->var );
	  } else {
	    make_entry_in_FactInfo( &(tmpop->p_preconds), l->var );
	  }
	}
      }
      tmpop->unconditional = new_effect();
      make_entry_in_FactInfo( &(tmpop->unconditional->p_effects), gnum_relevant_facts-1 );
      tmpop->next = gbit_operators;
      gbit_operators = tmpop;
      gnum_bit_operators++;
      if ( gcmd_line.display_info == 103 ) {      
	printf("\ngoal disjunct operator reads as follows:");
	print_BitOperator( gbit_operators );
      }
    }

    /* finally, we make sure that our only goal is to achieve
     * the new GOAL-REACHED fact
     */
    make_entry_in_FactInfo( &tpos, gnum_relevant_facts-1 );
  }
  gbit_goal_state = new_fact_info_pair( tpos, tneg );



  tpos = new_FactInfo();
  tneg = new_FactInfo();

  /* initial state can be empty, or simplified to TRU
   */
  if ( gcode_initial_state &&
       gcode_initial_state->connective != TRU ) {
    if ( gcode_initial_state->connective != AND ) {
      make_entry_in_FactInfo( &tpos, gcode_initial_state->var );
    } else {
      for ( i = gcode_initial_state->sons; i; i = i->next ) {
	make_entry_in_FactInfo( &tpos, i->var );
      }
    }
  }
  for ( j = 0; j<gnum_relevant_facts; j++ ) {
    if ( !get_bit( tpos->vector, gft_vector_length, j ) ) {
      make_entry_in_FactInfo( &tneg, j );
    }
  }
  gbit_initial_state = new_fact_info_pair( tpos, tneg );

  if ( gcmd_line.display_info == 103 ) {
    printf("\nbit coded initial state reads:");
    printf("\npositive");
    print_FactInfo( gbit_initial_state->positive );
    printf("\nnegative");
    print_FactInfo( gbit_initial_state->negative );
    printf("\nbit coded goal state reads:");
    printf("\npositive");
    print_FactInfo( gbit_goal_state->positive );
    printf("\nnegative");
    print_FactInfo( gbit_goal_state->negative );
  }

}




int get_bit( BitVector *vec, int vec_len, int pos )

{

  return (vec[pos / gcword_size] & (1 << (pos % gcword_size)));

}



void generate_BitOperators( CodeOperator *op )

{

  int i, mm;
  BitOperator *tmp, *tmp2;
  CodeNode *n, *j, *k, *l;
  Effect *tef, *ttt;

  tmp = new_BitOperator( op->name );
  tmp->num_vars = op->num_vars;
  for ( i=0; i<MAX_VARS; i++ ) {
    tmp->inst_table[i] = op->inst_table[i];
  }

  if ( op->conditionals ) {
    for ( j = op->conditionals->sons; j; j = j->next ) {
      if ( !(j->sons) ||
	   j->sons->connective == TRU ) {
	if ( !(tmp->unconditional) ) {
	  tmp->unconditional = new_Effect();
	}
	make_effect_entries( &(tmp->unconditional), j->sons->next );
	continue;
      }
      dnf( j->sons );
      if ( j->sons->connective != OR ) {
	tef = new_Effect();
	if ( j->sons->connective != AND ) {
	  if ( j->sons->connective == NOT ) {
	    make_entry_in_FactInfo( &(tef->n_conds), j->sons->sons->var );
	  } else {
	    make_entry_in_FactInfo( &(tef->p_conds), j->sons->var );
	  }
	} else {
	  for ( k = j->sons->sons; k; k = k->next ) {
	    if ( k->connective == NOT ) {
	      make_entry_in_FactInfo( &(tef->n_conds), k->sons->var );
	    } else {
	      make_entry_in_FactInfo( &(tef->p_conds), k->var );
	    }
	  }
	}
	/* conditions finished; see wether we got that already
	 */
	for ( ttt = tmp->conditionals; ttt; ttt = ttt->next ) {
	  for ( mm = 0; mm < gft_vector_length; mm++ ) {
	    if ( ttt->p_conds->vector[mm] != tef->p_conds->vector[mm] ) {
	      break;
	    }
	  }
	  if ( mm < gft_vector_length ) {
	    continue;
	  }
	  for ( mm = 0; mm < gft_vector_length; mm++ ) {
	    if ( ttt->n_conds->vector[mm] != tef->n_conds->vector[mm] ) {
	      break;
	    }
	  }
	  if ( mm < gft_vector_length ) {
	    continue;
	  }
	  free_effect( tef );
	  make_effect_entries( &ttt, j->sons->next );
	  break;
	}
	if ( ttt ) {
	  continue;
	}
	make_effect_entries( &tef, j->sons->next );
	tef->next = tmp->conditionals;
	tmp->conditionals = tef;
	continue;/* finished with effect whose conds where != OR */
      }
      for ( k = j->sons->sons; k; k = k->next ) {
	tef = new_Effect();
	if ( k->connective != AND ) {
	  if ( k->connective == NOT ) {
	    make_entry_in_FactInfo( &(tef->n_conds), k->sons->var );
	  } else {
	    make_entry_in_FactInfo( &(tef->p_conds), k->var );
	  }
	} else {
	  for ( l = k->sons; l; l = l->next ) {
	    if ( l->connective == NOT ) {
	      make_entry_in_FactInfo( &(tef->n_conds), l->sons->var );
	    } else {
	      make_entry_in_FactInfo( &(tef->p_conds), l->var );
	    }
	  }
	}
	/* conditions finished; see wether we got that already
	 */
	for ( ttt = tmp->conditionals; ttt; ttt = ttt->next ) {
	  for ( mm = 0; mm < gft_vector_length; mm++ ) {
	    if ( ttt->p_conds->vector[mm] != tef->p_conds->vector[mm] ) {
	      break;
	    }
	  }
	  if ( mm < gft_vector_length ) {
	    continue;
	  }
	  for ( mm = 0; mm < gft_vector_length; mm++ ) {
	    if ( ttt->n_conds->vector[mm] != tef->n_conds->vector[mm] ) {
	      break;
	    }
	  }
	  if ( mm < gft_vector_length ) {
	    continue;
	  }
	  free_effect( tef );
	  make_effect_entries( &ttt, j->sons->next );
	  break;
	}
	if ( ttt ) {
	  continue;
	}
	/* a bit INEFFICIENT... could also do effect entries only one time
	 * and then copy that for each disjunct
	 */
	make_effect_entries( &tef, j->sons->next );
	tef->next = tmp->conditionals;
	tmp->conditionals = tef;
      }
    }
    /* identical effects are now completely merged
     */
  }


  /* check if the overall merged unconditional effects are contradictory
   */
  if ( tmp->unconditional ) {
    for ( i = 0; i < gft_vector_length; i++ ) {
      if ( tmp->unconditional->p_effects->vector[i] &
	   tmp->unconditional->n_effects->vector[i] ) {
	return;
      }
    }
  }


  if ( !op->preconds ||
       op->preconds->connective == TRU ) {
    tmp->next = gbit_operators;
    gbit_operators = tmp;
    gnum_bit_operators++;

    if ( gcmd_line.display_info == 103 ) {
      printf("\nbit mapped operator reads as follows:");
      print_BitOperator( gbit_operators );
    }

    return;
  }

  dnf( op->preconds );
  n = op->preconds;
  if ( n->connective != OR ) {
    if ( n->connective != AND ) {
      if ( n->connective == NOT ) {
	make_entry_in_FactInfo( &(tmp->n_preconds), n->sons->var );
      } else {
	make_entry_in_FactInfo( &(tmp->p_preconds), n->var );
      }
    } else {
      for ( k = n->sons; k; k = k->next ) {
	if ( k->connective == NOT ) {
	  make_entry_in_FactInfo( &(tmp->n_preconds), k->sons->var );
	} else {
	  make_entry_in_FactInfo( &(tmp->p_preconds), k->var );
	}
      }
    }
    tmp->next = gbit_operators;
    gbit_operators = tmp;
    gnum_bit_operators++;

    if ( gcmd_line.display_info == 103 ) {
      printf("\nbit mapped operator reads as follows:");
      print_BitOperator( gbit_operators );
    }

    return;
  }

  for ( k = n->sons; k; k = k->next ) {
    tmp2 = new_BitOperator( op->name );
    tmp2->num_vars = op->num_vars;
    for ( i=0; i<MAX_VARS; i++ ) {
      tmp2->inst_table[i] = op->inst_table[i];
    }
    tmp2->unconditional = copy_effects( tmp->unconditional );
    tmp2->conditionals = copy_effects( tmp->conditionals );
    if ( k->connective != AND ) {
      if ( k->connective == NOT ) {
	make_entry_in_FactInfo( &(tmp2->n_preconds), k->sons->var );
      } else {
	make_entry_in_FactInfo( &(tmp2->p_preconds), k->var );
      }
    } else {
      for ( l = k->sons; l; l = l->next ) {
	if ( l->connective == NOT ) {
	  make_entry_in_FactInfo( &(tmp2->n_preconds), l->sons->var );
	} else {
	  make_entry_in_FactInfo( &(tmp2->p_preconds), l->var );
	}
      }
    }
    tmp2->next = gbit_operators;
    gbit_operators = tmp2;
    gnum_bit_operators++;

    if ( gcmd_line.display_info == 103 ) {
      printf("\nbit mapped operator reads as follows:");
      print_BitOperator( gbit_operators );
    }
  }
  free_BitOperator( tmp );

}



void make_entry_in_FactInfo( FactInfo **f, int index )

{

  int uid_block;
  unsigned int uid_mask;
  Integers *i;

  if ( index < 0 ) {
    /* can happen in effects: facts that are deleted, but not contained
     * in the initial state and never added.
     */
    return;
  }

  uid_block = index / gcword_size;
  uid_mask = 1 << ( index % gcword_size );
  i = new_integers( index );

  (*f)->vector[uid_block] |= uid_mask;
  i->next = (*f)->indices;
  (*f)->indices = i;

}



void make_effect_entries( Effect **e, CodeNode *n )

{

  CodeNode *j;

  if ( n->connective != AND ) {
    if ( n->connective == NOT ) {
      make_entry_in_FactInfo( &((*e)->n_effects), n->sons->var );
    } else {
      make_entry_in_FactInfo( &((*e)->p_effects), n->var );
    }
  } else {
    for ( j=n->sons; j; j = j->next ) {
      if ( j->connective == NOT ) {
	make_entry_in_FactInfo( &((*e)->n_effects), j->sons->var );
      } else {
	make_entry_in_FactInfo( &((*e)->p_effects), j->var );
      }
    }
  }

}



