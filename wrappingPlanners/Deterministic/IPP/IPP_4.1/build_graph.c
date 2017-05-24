


/*********************************************************************
 * File: build_graph.c
 * Description: main routines for building the graph
 *
 * Author: Joerg Hoffmann 1998
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
 * NOTE: the commentaries in this file, sparse as they are, are all
 *       in German, cause these are thoughts that I had while working
 *       on some really tedious parts of the code (potential effects...)
 * 
 *       If you have problems understanding the code (like I do when I have
 *       a look at it now), contact me at:
 *       
 *       hoffmann@informatik.uni-freiburg.de
 *
 *       and I'll be happy to answer your questions, if I can...
 *
 **********************************************************************/








#include "ipp.h"

#include "output.h"
#include "utilities.h"
#include "memory.h"

#include "build_graph.h"
#include "exclusions.h"









/**********************************
 * INITIAL GRAPH BUILDING PROCESS *
 **********************************/








Bool build_graph( int *min_time )

{

  int time = 0, j;
  Bool reached_goals = FALSE, first = TRUE;
  Integers *i;
  FtNode *ft;

  gft_table = ( FtNode_pointer * ) calloc( gnum_relevant_facts * 2, sizeof( FtNode_pointer ) );
  CHECK_PTR(gft_table);
#ifdef MEMORY_INFO
  gmemory += gnum_relevant_facts * 2 * sizeof( FtNode_pointer );
#endif
  for ( j = 0; j<2*gnum_relevant_facts; j++ ) gft_table[j] = NULL;
  gpos_facts_vector_at[0] = new_bit_vector( gft_vector_length );
  gneg_facts_vector_at[0] = new_bit_vector( gft_vector_length );

  for ( i = gbit_initial_state->positive->indices; i; i = i->next ) {
    ft = new_ft_node( 0, i->index, TRUE, FALSE );
    gft_table[i->index] = ft;
    ft->next = gall_fts_pointer;
    gall_fts_pointer = ft;
    (gpos_facts_vector_at[0])[ft->uid_block] |= ft->uid_mask;
  }

  for ( i = gbit_initial_state->negative->indices; i; i = i->next ) {
    ft = new_ft_node( 0, i->index, FALSE, FALSE );
    gft_table[NEG_ADR( i->index )] = ft;
    ft->next = gall_fts_pointer;
    gall_fts_pointer = ft;
    (gneg_facts_vector_at[0])[ft->uid_block] |= ft->uid_mask;
  }
  free_fact_info_pair( gbit_initial_state );

  if ( gcmd_line.display_info ) {
    fprintf( OUT, "time: %3d, %5d facts and %7d exclusive pairs (%5d, %7d positives)\n",
	     time, gfacts_count, gexclusions_count, gprint_ftnum, gprint_exnum );
  }

  for( ; time < *min_time; time++ ) {
    if ( first ) {
      reached_goals = are_there_non_exclusive( time,
					       gbit_goal_state->positive,
					       gbit_goal_state->negative );
      if ( reached_goals ) {
	if ( gcmd_line.display_info ) {
	  fprintf( OUT, "\ngoals first reachable in %d time steps\n\n", time);
          first = FALSE;
	}
      }
    }
    if ( gsame_as_prev_flag ) break;
    build_graph_evolution_step();
  }
  
  for( ; time < MAX_PLAN; time++ ) {
    reached_goals = are_there_non_exclusive( time,
					     gbit_goal_state->positive,
					     gbit_goal_state->negative );
    if ( reached_goals ) {
      if ( gcmd_line.display_info && time > 0 && first ) {
        fprintf( OUT, "\ngoals first reachable in %d time steps\n\n", time);
      }
      break;
    }
    if ( gsame_as_prev_flag ) break;
    build_graph_evolution_step();
  }

  *min_time = time;
  return reached_goals;

}









/*******************************************************
 * MAIN FUNCTION: ADD AN ADDTIONAL LAYER INFO TO GRAPH *
 *******************************************************/









void build_graph_evolution_step( void )

{

  static int time = 0;
  int facts_count = gfacts_count, exclusions_count = gexclusions_count;
  BitOperator *o, *prev, *tmp;
  OpNode *op, *prev_op, *tmp_op, *i;
  EfNode *j;
  FtNode *l;
  int k;

  struct tms start, end;

  gpos_facts_vector_at[time+1] = copy_bit_vector( gpos_facts_vector_at[time],
						  gft_vector_length );
  gneg_facts_vector_at[time+1] = copy_bit_vector( gneg_facts_vector_at[time],
						  gft_vector_length );

  for ( i = gall_ops_pointer; i; i = i->next ) {
    i->info_at[time] = new_op_level_info();
    i->unconditional->info_at[time] = new_ef_level_info();
    for ( j = i->conditionals; j; j = j->next ) {
      j->info_at[time] = new_ef_level_info();
      if ( j->info_at[time-1]->is_dummy ) {
	/* effekte bleiben erstmal dummy...
	 * spaeter dann nachschauen, ob sie doch eingezogen werden koennen.
	 *
	 * ( kann sein, dass effekt erst drei runden spaeter nicht-dummy wird )
	 */ 
	j->info_at[time]->is_dummy = TRUE;
      }
    }
  }

  /* eigentlich ineffizient; unterscheidung wegen dummys noetig, die beim
   * ersterzeugen NICHT in die globale liste aufgenommen werden.
   *
   * EFFIZIENTER: extra verkettung fuer dummys erstellen.
   */
  for ( k=0; k<2*gnum_relevant_facts; k++ ) {
    if ( (l = gft_table[k]) == NULL ) continue;

    l->info_at[time+1] = new_ft_level_info( l );
    if ( l->info_at[time]->is_dummy ) {
      /* siehe oben...
       *
       * facts, die hier SO markiert sind, sind NUR DIEJENIGEN, DIE VON
       * EINEM DUMMY EFFEKT GEADDET wurden. also einfach markierung wieder
       * umstellen, sobald der entsprechende dummy effekt richtig einziehbar
       * wird!
       */
      l->info_at[time+1]->is_dummy = TRUE;
    }
  }


  gprev_level_ops_pointer = gall_ops_pointer;

  apply_noops( time );

  gprev_level_fts_pointer = gall_fts_pointer;

  op = gops_with_unactivated_effects_pointer;
  while ( op && apply_all_effects( time, op ) ) {
    tmp_op = op;
    op = op->thread;
    tmp_op->thread = NULL;
  }
  gops_with_unactivated_effects_pointer = op;
  prev_op = op;
  if ( op ) op = op->thread;
  while ( op ) {
    if ( apply_all_effects( time, op ) ) {
      prev_op->thread = op->thread;
      tmp_op = op;
      op = op->thread;
      tmp_op->thread = NULL;
    } else {
      prev_op = prev_op->thread;
      op = op->thread;
    }
  }

  o = gbit_operators;
  while ( o && apply_operator( time, o ) ) {
    tmp = o;
    o = o->next;
    free_partial_operator( tmp );
  }
  gbit_operators = o;
  prev = o;
  if ( o ) o = o->next;
  while ( o ) {
    if ( apply_operator( time, o ) ) {
      prev->next = o->next;
      tmp = o;
      o = o->next;
      free_partial_operator( tmp );
    } else {
      prev = prev->next;
      o = o->next;
    }
  }
  gop_vector_length_at[time] = ( ( int ) gops_count / gcword_size );
  if ( ( gops_count % gcword_size ) > 0 ) gop_vector_length_at[time]++;

  /* bei allen anwendbaren ops nachschauen, ob die dummy effekte
   * inzwischen einziehbar sind!
   *
   * ja->markierungen des effekts und der geaddeten facts umstellen!
   */
  integrate_potential_effects( time );

  times(&start);
  find_mutex_ops( time );
  find_mutex_facts( time + 1 );
  times(&end);
  TIME( gexcl_time );

  insert_potential_effects( time );
  
  if ( facts_count == gfacts_count &&
       exclusions_count == gexclusions_count ) {
    gsame_as_prev_flag = TRUE;
    if ( gcmd_line.display_info ) 
      fprintf( OUT, "\ngraph has leveled off at time step %d\n\n", time+1 );
    gfirst_full_time = time;
  }

  if ( gcmd_line.display_info ) {
    fprintf( OUT, "           %5d ops   and %7d exclusive pairs\n",
	     gops_count, gops_exclusions_count );
    fprintf( OUT, "time: %3d, %5d facts and %7d exclusive pairs (%5d, %7d positives)\n",
	     time+1, gfacts_count, gexclusions_count, gprint_ftnum, gprint_exnum );
  }

  time++;
  
}









/********************************************************************
 * GRAPH BUILDING FUNCTIONS; OVERSEEN BY BUILD_GRAPH_EVOLUTION_STEP *
 ********************************************************************/










void apply_noops( int time )

{

  FtNode *ft;
  BitVector *a, *b;
  OpNode *noop;
  EfNode *tmp;
  
  for ( ft = gall_fts_pointer; ft != gprev_level_fts_pointer; ft = ft->next ) {

    a = new_bit_vector( gft_vector_length );
    b = new_bit_vector( gft_vector_length );
    if ( ft->positive ) {
      a[ft->uid_block] |= ft->uid_mask;
    } else {
      b[ft->uid_block] |= ft->uid_mask;
    }
    noop = new_op_node( time, NULL, TRUE, a, b );
    insert_ft_edge( &(noop->preconds), ft );
    tmp = new_ef_node( time, noop, a, b );
    tmp->info_at[time]->cond_pos_exclusives = ft->info_at[time]->pos_exclusives;
    tmp->info_at[time]->cond_neg_exclusives = ft->info_at[time]->neg_exclusives;
    noop->unconditional = tmp;
    noop->next = gall_ops_pointer;
    gall_ops_pointer = noop;

    ft->noop = noop;

  }

}


Bool apply_operator( int time, BitOperator *op )

{

  OpNode *op_node;
  EfNode *ef_node;
  FtNode *ft_node;
  BitVector *precond_pos_exclusives, *precond_neg_exclusives;
  Integers *i;
  int j;

  if ( !get_them_non_exclusive( time, 
				op->p_preconds,	op->n_preconds,
				&precond_pos_exclusives, &precond_neg_exclusives ) ) {
    return FALSE;
  }


  op_node = new_op_node( time, op->name, FALSE,
			 op->p_preconds->vector,
			 op->n_preconds->vector );
  op_node->num_vars = op->num_vars;
  for ( j=0; j<MAX_VARS; j++ ) {
    op_node->inst_table[j] = op->inst_table[j];
  }
  op_node->next = gall_ops_pointer;
  gall_ops_pointer = op_node;

  for ( i=op->p_preconds->indices; i; i=i->next ) {
    insert_ft_edge( &(op_node->preconds), gft_table[i->index] );
    insert_op_edge( &(gft_table[i->index]->preconds), op_node );
  }
  for ( i=op->n_preconds->indices; i; i=i->next ) {
    insert_ft_edge( &(op_node->preconds), gft_table[NEG_ADR( i->index )] );
    insert_op_edge( &(gft_table[NEG_ADR( i->index )]->preconds), op_node );
  }

  ef_node = new_ef_node( time, op_node,
			 (op->unconditional ? op->unconditional->p_effects->vector :
			  new_bit_vector( gft_vector_length ) ),
			 (op->unconditional ? op->unconditional->n_effects->vector :
			  new_bit_vector( gft_vector_length ) ) );
  ef_node->info_at[time]->cond_pos_exclusives = precond_pos_exclusives;
  ef_node->info_at[time]->cond_neg_exclusives = precond_neg_exclusives;    
  op_node->unconditional = ef_node;  
  if ( op->unconditional ) {
    
    for ( i = op->unconditional->p_effects->indices; i; i=i->next ) {
      if ( (ft_node = gft_table[i->index]) == NULL ) {
	ft_node = new_ft_node( time+1, i->index, TRUE, FALSE );
	ft_node->next = gall_fts_pointer;
	gall_fts_pointer = ft_node;
	gft_table[i->index] = ft_node;
	(gpos_facts_vector_at[time+1])[ft_node->uid_block] |= ft_node->uid_mask;
      }
      insert_ef_edge( &(ft_node->adders), ef_node );
      insert_ft_edge( &(ef_node->effects), ft_node );
    }

    for ( i = op->unconditional->n_effects->indices; i; i=i->next ) {
      if ( (ft_node = gft_table[NEG_ADR( i->index )]) == NULL ) {
	ft_node = new_ft_node( time+1, i->index, FALSE, FALSE );
	ft_node->next = gall_fts_pointer;
	gall_fts_pointer = ft_node;
	gft_table[NEG_ADR( i->index )] = ft_node;
	(gneg_facts_vector_at[time+1])[ft_node->uid_block] |= ft_node->uid_mask;
      }
      insert_ef_edge( &(ft_node->adders), ef_node );
      insert_ft_edge( &(ef_node->effects), ft_node );
    }
  }

  op_node->unactivated_effects = op->conditionals;
  if ( !apply_all_effects( time, op_node ) ) {
    op_node->thread = gops_with_unactivated_effects_pointer;
    gops_with_unactivated_effects_pointer = op_node;
  }

  return TRUE;

}


Bool apply_all_effects( int time, OpNode *op_node ) 

{

  Effect *j, *tmp, *prev;


  /* FEHLER!!
   *
   * ...genau ueberpruefen, was ge freet werden darf!!
   */
  j = op_node->unactivated_effects;
  while ( j && apply_effect( time, j, op_node ) ) {
    tmp = j;
    j = j->next;
    if ( 1 ) free_partial_effect( tmp );
  }
  op_node->unactivated_effects = j;
  prev = j;
  if ( j ) j = j->next;
  while ( j ) {
    if ( apply_effect( time, j, op_node ) ) {
      prev->next = j->next;
      tmp = j;
      j = j->next;
      if ( 1 ) free_partial_effect( tmp );
    } else {
      prev = prev->next;
      j = j->next;
    }
  }

  if ( op_node->unactivated_effects == NULL ) {
    return TRUE;
  } else {
    return FALSE;
  }

}


Bool apply_effect( int time, Effect *ef, OpNode *op_node )

{

  Integers *i;
  BitVector *cond_pos_exclusives, *cond_neg_exclusives;
  BitVector *a, *b;
  EfNode *ef_node;
  FtNode *ft_node;
  int r;

  if ( !get_them_non_exclusive( time, 
				ef->p_conds, ef->n_conds,
				&cond_pos_exclusives, &cond_neg_exclusives ) ) {
    return FALSE;
  }

  a = cond_pos_exclusives;
  b = op_node->pos_precond_vector;
  for ( r = 0; r < gft_vector_length; r++ ) {
    if ( a[r] & b[r] ) {
      free( cond_pos_exclusives );
      free( cond_neg_exclusives );
      return FALSE;
    }
  }
  a = cond_neg_exclusives;
  b = op_node->neg_precond_vector;
  for ( r = 0; r < gft_vector_length; r++ ) {
    if ( a[r] & b[r] ) {
      free( cond_pos_exclusives );
      free( cond_neg_exclusives );
      return FALSE;
    }
  }

  ef_node = new_ef_node( time, op_node,
			 ef->p_effects->vector, ef->n_effects->vector );
  ef_node->info_at[time]->cond_pos_exclusives = cond_pos_exclusives;
  ef_node->info_at[time]->cond_neg_exclusives = cond_neg_exclusives;
  for ( i=ef->p_conds->indices; i; i=i->next ) {
    insert_ft_edge( &(ef_node->conditions), gft_table[i->index] );
  }
  for ( i=ef->n_conds->indices; i; i=i->next ) {
    insert_ft_edge( &(ef_node->conditions), gft_table[NEG_ADR( i->index )] );
  }
  ef_node->next = op_node->conditionals;
  op_node->conditionals = ef_node;
    
  for ( i = ef->p_effects->indices; i; i=i->next ) {
    if ( (ft_node = gft_table[i->index]) == NULL ) {
      ft_node = new_ft_node( time+1, i->index, TRUE, FALSE );
      ft_node->next = gall_fts_pointer;
      gall_fts_pointer = ft_node;
      gft_table[i->index] = ft_node;
      (gpos_facts_vector_at[time+1])[ft_node->uid_block] |= ft_node->uid_mask;
   }
    insert_ef_edge( &(ft_node->adders), ef_node );
    insert_ft_edge( &(ef_node->effects), ft_node );
  }

  for ( i = ef->n_effects->indices; i; i=i->next ) {
    if ( (ft_node = gft_table[NEG_ADR( i->index )]) == NULL ) {
      ft_node = new_ft_node( time+1, i->index, FALSE, FALSE );
      ft_node->next = gall_fts_pointer;
      gall_fts_pointer = ft_node;
      gft_table[NEG_ADR( i->index )] = ft_node;
      (gneg_facts_vector_at[time+1])[ft_node->uid_block] |= ft_node->uid_mask;
    }
    insert_ef_edge( &(ft_node->adders), ef_node );
    insert_ft_edge( &(ef_node->effects), ft_node );
  }

  return TRUE;

}











/***************************************
 * CODE FOR APPLYING POTENTIAL EFFECTS *
 ***************************************/










void insert_potential_effects( int time )

{

  OpNode *op, *tmp_op, *prev_op;
  Bool hit = TRUE;

  while ( hit ) {
    hit = FALSE;

    op = gops_with_unactivated_effects_pointer;
    while ( op && apply_potential_effects( time, op, &hit ) ) {
      tmp_op = op;
      op = op->thread;
      tmp_op->thread = NULL;
    }
    gops_with_unactivated_effects_pointer = op;
    prev_op = op;
    if ( op ) op = op->thread;
    while ( op ) {
      if ( apply_potential_effects( time, op, &hit ) ) {
	prev_op->thread = op->thread;
	tmp_op = op;
	op = op->thread;
	tmp_op->thread = NULL;
      } else {
	prev_op = prev_op->thread;
	op = op->thread;
      }
    }

    if ( !hit ) break;
  }

}


Bool apply_potential_effects( int time, OpNode *op_node, Bool *hit ) 

{

  Effect *j, *tmp, *prev;


  /* FEHLER!!
   *
   * ...genau ueberpruefen, was ge freet werden darf!!
   */
  j = op_node->unactivated_effects;
  while ( j && apply_potential_effect( time, j, op_node ) ) {
    tmp = j;
    j = j->next;
    if ( 0 ) free_partial_effect( tmp );
    *hit = TRUE;
  }
  op_node->unactivated_effects = j;
  prev = j;
  if ( j ) j = j->next;
  while ( j ) {
    if ( apply_potential_effect( time, j, op_node ) ) {
      prev->next = j->next;
      tmp = j;
      j = j->next;
      if ( 0 ) free_partial_effect( tmp );
      *hit = TRUE;
    } else {
      prev = prev->next;
      j = j->next;
    }
  }

  if ( op_node->unactivated_effects == NULL ) {
    return TRUE;
  } else {
    return FALSE;
  }

}


Bool apply_potential_effect( int time, Effect *ef, OpNode *op_node )

{

  Integers *i;
  EfNode *ef_node;
  FtNode *ft_node, *ft;

  if ( !potential_applicable( time, ef, op_node ) ) {
    return FALSE;
  }

  ef_node = new_ef_node( time, op_node,
			 ef->p_effects->vector, ef->n_effects->vector );
  ef_node->info_at[time]->is_dummy = TRUE;
  /* gesamtexclusives bleiben leer (cond_pos/neg_exclusives);
   * koennen fuer dummys nicht besonders sinnvoll angegeben werden...
   * (siehe kommentar dazu in search_plan.c)
   *
   * NACHSCHAUEN: KANN DAS ZU PROBLEMEN FUEHREN ?
   *              antwort: NEIN ! (?): falls dieser vektor nicht da ist, 
   *                       wird er in der suche sowieso berechnet.
   */
  for ( i=ef->p_conds->indices; i; i=i->next ) {
    ft = gft_table[i->index];
    insert_ft_edge( &(ef_node->conditions), ft );
    if ( !ft->info_at[time] ) {
      ft->info_at[time] = new_ft_level_info( ft );
      /* diese information wird in suche nicht verwendet!!!
       * ---> aber bei integrate potential effects
       * ---> koennte man auch anders machen und diese info ganz weglassen.
       * ...allerdings wohl zum debuggen ganz praktisch
       * und aufwand vernachlaessigbar, also was solls...
       *
       * ANMERKUNG: exclusivitaet von dummys wird nach
       * integration nachgeregelt... solange sind lediglich
       * die contradicting facts exclusiv.
       */
      ft->info_at[time]->is_dummy = TRUE;
    }
  }
  for ( i=ef->n_conds->indices; i; i=i->next ) {
    ft = gft_table[NEG_ADR( i->index )];
    insert_ft_edge( &(ef_node->conditions), ft );
    if ( !ft->info_at[time] ) {
      ft->info_at[time] = new_ft_level_info( ft );
      ft->info_at[time]->is_dummy = TRUE;
    }
  }
  ef_node->next = op_node->conditionals;
  op_node->conditionals = ef_node;
    
  for ( i = ef->p_effects->indices; i; i=i->next ) {
    if ( (ft_node = gft_table[i->index]) == NULL ) {
      ft_node = new_ft_node( time+1, i->index, TRUE, TRUE );
      ft_node->info_at[time+1]->is_dummy = TRUE;
      gft_table[i->index] = ft_node;
      /* fact wird noch NICHT in globale liste gestellt, fuehrt eine
       * schattenexistenz mit info_at und contradict exclusives ohne
       * adders info bis es zum ersten mal wirklich da ist durch
       * regulaeres eintreten des entsprechenden efekts.
       */
      /* vector wird NICHT mit fact - bit ge ort! dadurch in 
       * get..non_exclusive funktionen fact als nicht da
       * interpretiert!! WICHTIG!!
       */
      /* kann man hier noch etwas ueber exclusives sagen...?
       *
       * ja, und zwar dass das fact die exclusives kriegt, die es
       * spaeter als richtiges fact haben wird!
       */
    }
    insert_ef_edge( &(ft_node->adders), ef_node );
    insert_ft_edge( &(ef_node->effects), ft_node );
  }

  for ( i = ef->n_effects->indices; i; i=i->next ) {
    if ( (ft_node = gft_table[NEG_ADR( i->index )]) == NULL ) {
      ft_node = new_ft_node( time+1, i->index, FALSE, TRUE );
      ft_node->info_at[time+1]->is_dummy = TRUE;
      gft_table[NEG_ADR( i->index )] = ft_node;
      /* siehe oben
       */
    }
    insert_ef_edge( &(ft_node->adders), ef_node );
    insert_ft_edge( &(ef_node->effects), ft_node );
  }
  
  return TRUE;

}


Bool potential_applicable( int time, Effect *ef, OpNode *op_node )

{

  static FtArray all_fts, dum_fts;
  int alln = 0, dumn = 0;

  Integers *in;
  FtEdge *jn;
  FtNode *ft;
  OpNode *op_node2;
  FtLevelInfo *info;
  BitVector *vec;
  EfEdge *i_ef;
  int i, j;

  for ( in = ef->p_conds->indices; in; in = in->next ) {
    ft = gft_table[in->index];
    if ( !ft ) {
      return FALSE;
    }
    if ( alln == ARRAY_SIZE ) {
      printf( "\n\nipp: increase ARRAY_SIZE( preset value: %d )", ARRAY_SIZE );
      exit( 1 );
    }
    all_fts[alln++] = ft;
    if ( !ft->info_at[time] ||
	 ft->info_at[time]->is_dummy ) {
      dum_fts[dumn++] = ft;
    }
  }
  for ( in = ef->n_conds->indices; in; in = in->next ) {
    ft = gft_table[NEG_ADR( in->index )];
    if ( !ft ) {
      return FALSE;
    }
    if ( alln == ARRAY_SIZE ) {
      printf( "\n\nipp: increase ARRAY_SIZE( preset value: %d )", ARRAY_SIZE );
      exit( 1 );
    }
    all_fts[alln++] = ft;
    if ( !ft->info_at[time] ||
	 ft->info_at[time]->is_dummy ) {
      dum_fts[dumn++] = ft;
    }
  }

  for ( i=0; i<alln; i++ ) {
    /* uebernehme fuer dummys exclusions von vorkommen auf naechstem level;
     * das kann entweder contradict sein, falls fact auf time+1 dummy
     * oder aber volle excl, falls fact auf time+1 echt.
     * hier greifen wir etwas vor: ist time+1 echt, so werden spaeter
     * fuer dummy time sowieso die time+1 excl eingetragen.
     */
    if ( !all_fts[i]->info_at[time] ||
	 all_fts[i]->info_at[time]->is_dummy ) {
      info = all_fts[i]->info_at[time+1];
    } else {
      info = all_fts[i]->info_at[time];
    }
    /* info = all_fts[i]->info_at[time] ? all_fts[i]->info_at[time] :
       all_fts[i]->info_at[time+1]; */

    for ( j = i+1; j<alln; j++ ) {
      vec = all_fts[j]->positive ? info->pos_exclusives : info->neg_exclusives;
      if ( vec[all_fts[j]->uid_block] & all_fts[j]->uid_mask ) {
	return FALSE;
      }
    }

    for ( jn = op_node->preconds; jn; jn = jn->next ) {
      vec = jn->ft->positive ? info->pos_exclusives : info->neg_exclusives;
      if ( vec[jn->ft->uid_block] & jn->ft->uid_mask ) {
	return FALSE;
      }
    }
  }

  /* gecheckt ist jetzt: - alle conditions sind zumindest im graph
   *                       (d.h. dummy oder aufm naechsten level)
   *                     - alle conditions sind zumindest nicht exclusiv
   *                       voneinander und von preconds
   *                       ACHTUNG bei facts, die auf time noch nicht da sind:
   *                               solche sind auf time + 1 jedenfalls da und
   *                               auf time mindestens so exclusive wie auf time+1
   *                       (dummys die auf time da sind, haben nur die contradicting
   *                        exclusivitaet)(genauso wie dummys auf time+1)
   */

  
  /* jetzt checken, ob dummy facts zusammen erreicht werden koennen!
   * ( bei verwendung von op_node )
   */

  for ( i=0; i<dumn; i++ ) {
    for ( i_ef = dum_fts[i]->adders; i_ef; i_ef = i_ef->next ) {
      op_node2 = i_ef->ef->op;
      if ( ( op_node->info_at[time]->exclusives[op_node2->uid_block] &
	     op_node2->uid_mask ) == 0 ) {
	break;
      }
    }
    if ( !i_ef ) return FALSE;
  }

  return TRUE;

}





void integrate_potential_effects( int time )

{

  OpNode *op;
  EfNode *ef;
  FtEdge *i, *j;

  for ( op = gall_ops_pointer; op; op = op->next ) {

    for ( ef = op->conditionals; ef; ef = ef->next ) {

      if ( !ef->info_at[time]->is_dummy ) continue;

      /* eine etwas ineffektive methode, die nicht integrierten
       * potentiellen effekte zu finden, besser waere eine
       * globale liste der entsprechenden effekte
       * mit dem ueblichen rausstreichverfahren...
       *
       * INEFFIZIENT, vielleicht spaeter mal aendern.
       */
      for ( i = ef->conditions; i; i = i->next ) {
	if ( i->ft->info_at[time]->is_dummy ) break;
      }
      if ( i ) continue;

      for ( i = ef->conditions; i; i = i->next ) {
	for ( j = i->next; j; j = j->next ) {
	  if ( ARE_MUTEX_FTS( time, i->ft, j->ft ) ) break;
	}
	if ( j ) break;

	/* conditions, die frueher dummy waren, koennen
	 * exclusiv von den preconds sein!
	 *
	 * glaub ich jez eigentlich nicht, aber riskieren wollen
	 * wers mal net. SPAETER MAL TESTEN!
	 */
	for ( j = op->preconds; j; j = j->next ) {
	  if ( ARE_MUTEX_FTS( time, i->ft, j->ft ) ) break;
	}
	if ( j ) break;
      }
      if ( i ) continue;

      /* effekt kann integriert werden!
       */
      ef->info_at[time]->is_dummy = FALSE;
      for ( i = ef->effects; i; i = i->next ) {
	/* JETZT ERST WIRD FACT VON get...non_exclusive FUNKTIONEN
	 * ANERKANNT ---> KANN IN PRECONDS VERWENDET WERDEN.
	 */
	if ( i->ft->positive ) {
	  (gpos_facts_vector_at[time+1])[i->ft->uid_block] |= i->ft->uid_mask;
	} else {
	  (gneg_facts_vector_at[time+1])[i->ft->uid_block] |= i->ft->uid_mask;
	}
	/* jetzt wirds auch von dieser funktion anerkannt...
	 * prinzipiell wuerde eine der beiden informationen schon ausreichen.
	 */
	if ( i->ft->info_at[time+1]->is_dummy ) {
	  i->ft->next = gall_fts_pointer;
	  gall_fts_pointer = i->ft;
	  i->ft->info_at[time+1]->is_dummy = FALSE;
	  gfacts_count++;
	}
      }

    }
  }

}









/********************************************
 * HELPERS HANDLING CONDITIONS AND PRECONDS *
 ********************************************/










  
Bool are_there_non_exclusive( int time, FactInfo *pos, FactInfo *neg )

{

  BitVector *a, *b;
  Integers *i;
  int r;

  /* !!!!!!!!!!!!!!!!!!!!!!!!
   * hier lieber einzeln die fakten auslesen!!
   * genauso eins weiter unten in get_them...
   */
  a = pos->vector;
  b = gpos_facts_vector_at[time];
  for ( r = 0; r < gft_vector_length; r++ ) {
    if ( a[r] != (a[r] & b[r]) ) {
      return FALSE;
    }
  }
  a = neg->vector;
  b = gneg_facts_vector_at[time];
  for ( r = 0; r < gft_vector_length; r++ ) {
    if ( a[r] != (a[r] & b[r]) ) {
      return FALSE;
    }
  }

  /* geht das EFFIZIENTER durch zusammen OR-en der exclusives ??
   */
  for ( i=pos->indices; i; i=i->next ) {
    a = gft_table[i->index]->info_at[time]->pos_exclusives;
    b = gft_table[i->index]->info_at[time]->neg_exclusives;
    for ( r = 0; r < gft_vector_length; r++ ) {
      if ( (pos->vector[r] & a[r]) || (neg->vector[r] & b[r]) ) {
	return FALSE;
      }
    }
  }
  for ( i=neg->indices; i; i=i->next ) {
    a = gft_table[NEG_ADR( i->index )]->info_at[time]->pos_exclusives;
    b = gft_table[NEG_ADR( i->index )]->info_at[time]->neg_exclusives;
    for ( r = 0; r < gft_vector_length; r++ ) {
      if ( (pos->vector[r] & a[r]) || (neg->vector[r] & b[r]) ) {
	return FALSE;
      }
    }
  }
    
  return TRUE;

}


Bool get_them_non_exclusive( int time,
			     FactInfo *pos, FactInfo *neg,
			     BitVector **pos_exclusives, BitVector **neg_exclusives )

{

  BitVector *a, *b;
  int r;
  Integers *i;

  a = pos->vector;
  b = gpos_facts_vector_at[time];
  for ( r = 0; r < gft_vector_length; r++ ) {
    if ( a[r] != (a[r] & b[r]) ) {
      return FALSE;
    }
  }
  a = neg->vector;
  b = gneg_facts_vector_at[time];
  for ( r = 0; r < gft_vector_length; r++ ) {
    if ( a[r] != (a[r] & b[r]) ) {
      return FALSE;
    }
  }

  *pos_exclusives = new_excl_bit_vector( gft_vector_length );
  *neg_exclusives = new_excl_bit_vector( gft_vector_length );

  for ( i=pos->indices; i; i=i->next ) {
    b = gft_table[i->index]->info_at[time]->pos_exclusives;
    for ( r = 0; r < gft_vector_length; r++ ) {
      (*pos_exclusives)[r] |= b[r];
      if ( pos->vector[r] & (*pos_exclusives)[r] ) {
	free( *pos_exclusives );
	free( *neg_exclusives );
	return FALSE;
      }
    }
    b = gft_table[i->index]->info_at[time]->neg_exclusives;
    for ( r = 0; r < gft_vector_length; r++ ) {
      (*neg_exclusives)[r] |= b[r];
      if ( neg->vector[r] & (*neg_exclusives)[r] ) {
	free( *pos_exclusives );
	free( *neg_exclusives );
	return FALSE;
      }
    }
  }
  for ( i=neg->indices; i; i=i->next ) {
    b = gft_table[NEG_ADR( i->index )]->info_at[time]->pos_exclusives;
    for ( r = 0; r < gft_vector_length; r++ ) {
      (*pos_exclusives)[r] |= b[r];
      if ( pos->vector[r] & (*pos_exclusives)[r] ) {
	free( *pos_exclusives );
	free( *neg_exclusives );
	return FALSE;
      }
    }
    b = gft_table[NEG_ADR( i->index )]->info_at[time]->neg_exclusives;
    for ( r = 0; r < gft_vector_length; r++ ) {
      (*neg_exclusives)[r] |= b[r];
      if ( neg->vector[r] & (*neg_exclusives)[r] ) {
	free( *pos_exclusives );
	free( *neg_exclusives );
	return FALSE;
      }
    }
  }

  return TRUE;

}









/******************
 * SIMPLE HELPERS *
 ******************/









void insert_op_edge( OpEdge **l, OpNode *op )

{

  OpEdge *new_edge = new_op_edge( op );

  new_edge->next = *l;
  *l = new_edge;

}

void insert_ef_edge( EfEdge **l, EfNode *ef )

{

  EfEdge *new_edge = new_ef_edge( ef );

  new_edge->next = *l;
  *l = new_edge;

}

void insert_ft_edge( FtEdge **l, FtNode *ft )

{

  FtEdge *new_edge = new_ft_edge( ft );

  new_edge->next = *l;
  *l = new_edge;

}




