


/*********************************************************************
 * File: exclusions.c
 * Description: routines for calculating exclusion relations
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

#include "exclusions.h"







/*************************************
 * SIMPLE HELPERS, SHOULD BE MACROS. *
 *************************************/








void MAKE_OPS_EXCLUSIVE( int time, OpNode *o1, OpNode *o2 ) 

{

  (o1->info_at[time]->exclusives)[o2->uid_block] |= o2->uid_mask;
  (o2->info_at[time]->exclusives)[o1->uid_block] |= o1->uid_mask;

}

void MAKE_FTS_EXCLUSIVE( int time, FtNode *f1, FtNode *f2 )

{

  BitVector *v1, *v2;

  v1 = ( ( f2->positive ) ? f1->info_at[time]->pos_exclusives :
	 f1->info_at[time]->neg_exclusives );
  v2 = ( ( f1->positive ) ? f2->info_at[time]->pos_exclusives :
	 f2->info_at[time]->neg_exclusives );
  v1[f2->uid_block] |= f2->uid_mask;
  v2[f1->uid_block] |= f1->uid_mask;

}
							
void MAKE_OPS_UNEXCLUSIVE( int time, OpNode *o1, OpNode *o2 )

{

  (o1->info_at[time]->exclusives)[o2->uid_block] &= ~(o2->uid_mask);
  (o2->info_at[time]->exclusives)[o1->uid_block] &= ~(o1->uid_mask);

}

void MAKE_FTS_UNEXCLUSIVE( int time, FtNode *f1, FtNode *f2 )

{

  BitVector *v1, *v2;

  v1 = ( ( f2->positive ) ? f1->info_at[time]->pos_exclusives :
	 f1->info_at[time]->neg_exclusives );
  v2 = ( ( f1->positive ) ? f2->info_at[time]->pos_exclusives :
	 f2->info_at[time]->neg_exclusives );

  v1[f2->uid_block] &= ~(f2->uid_mask);
  v2[f1->uid_block] &= ~(f1->uid_mask);

}



Bool ARE_MUTEX_OPS( int time, OpNode *o1, OpNode *o2 )

{

  if ( o1->info_at[time]->exclusives[o2->uid_block] & o2->uid_mask ) {
    return TRUE;
  } else {
    return FALSE;
  }

}


Bool ARE_MUTEX_FTS( int time, FtNode *f1, FtNode *f2 )

{

  BitVector *a = ( f2->positive ) ?
    f1->info_at[time]->pos_exclusives :
    f1->info_at[time]->neg_exclusives;

  if ( a[f2->uid_block] & f2->uid_mask ) {
    return TRUE;
  } else {
    return FALSE;
  }

}


void SET_ADDERS( int time, FtNode *ft ) 

{

  EfEdge *e;

  ft->info_at[time]->adders = new_excl_bit_vector( gop_vector_length_at[time-1] );
  if ( ft->noop ) {
    (ft->info_at[time]->adders)[ft->noop->uid_block] |= ft->noop->uid_mask;
  }
  for ( e = ft->adders; e; e = e->next ) {
    if ( e->ef->info_at[time-1]->is_dummy ) continue;
    (ft->info_at[time]->adders)[e->ef->op->uid_block] |= e->ef->op->uid_mask;
  }
  ft->info_at[time]->adders_pointer = ft->adders;

}









/*****************************************************************
 * THE TWO MAIN FUNCIONS, CALLED FROM BUILD_GRAPH_EVOLUTION_STEP *        
 *****************************************************************/










void find_mutex_ops( int time )

{

  OpNode *i1, *i2;
  OpPair *i, *tmp, *prev;

  BitVector *a, *b;
  int r;

  for ( i1=gall_ops_pointer; i1 != gprev_level_ops_pointer; i1=i1->next ) {
    i1->info_at[time]->exclusives = new_excl_bit_vector( gop_vector_length_at[time] );
  }
  for ( ; i1; i1=i1->next ) {
    i1->info_at[time]->exclusives = new_excl_bit_vector( gop_vector_length_at[time] );
    if ( time > 0 ) {
      a = i1->info_at[time]->exclusives;
      b = i1->info_at[time-1]->exclusives;
      for ( r = 0; r < gop_vector_length_at[time-1]; r++ ) {
	a[r] |= b[r];
      }
    }
  }

  i = gop_mutex_pairs;
  while ( i && !competing_needs( time, i->o1, i->o2 ) ) {
    MAKE_OPS_UNEXCLUSIVE( time, i->o1, i->o2 );
    tmp = i;
    i = i->next;
    free( tmp );
#ifdef MEMORY_INFO
    gexcl_memory -= sizeof( OpPair );
#endif
    gops_exclusions_count--;
  }
  gop_mutex_pairs = i;
  prev = i;
  if ( i ) i = i->next;
  while ( i ) {
    if ( !competing_needs( time, i->o1, i->o2 ) ) {
      MAKE_OPS_UNEXCLUSIVE( time, i->o1, i->o2 );
      prev->next = i->next;
      tmp = i;
      i = i->next;
      free( tmp );
#ifdef MEMORY_INFO
      gexcl_memory -= sizeof( OpPair );
#endif
      gops_exclusions_count--;
    } else {
      prev = prev->next;
      i = i->next;
    }
  }

  for ( i1 = gall_ops_pointer; i1 != gprev_level_ops_pointer; i1 = i1->next ) {
    for ( i2 = i1->next; i2; i2 = i2->next ) {
      if ( interfere( i1, i2 ) ) {
	MAKE_OPS_EXCLUSIVE( time, i1, i2 );
	gops_exclusions_count++;
	continue;
      }
      if ( noop_exclusive( i1, i2 ) ) {
	MAKE_OPS_EXCLUSIVE( time, i1, i2 );
	gops_exclusions_count++;
	continue;
      }	
      if ( !competing_needs( time, i1, i2 ) ) {
	continue;
      }
      MAKE_OPS_EXCLUSIVE( time, i1, i2 );
      gops_exclusions_count++;
      tmp = new_op_pair( i1, i2 );
      tmp->next = gop_mutex_pairs;
      gop_mutex_pairs = tmp;
    }
  }

}






void find_mutex_facts( int time )

{

  FtNode *i1, *i2;
  EfEdge *e;
  FtPair *i, *prev, *tmp;
  BitVector *a, *b;
  int r, j;

  for ( i1=gall_fts_pointer; i1 != gprev_level_fts_pointer; i1=i1->next ) {
    if ( i1->info_at[time]->is_dummy ) {
      printf( "\nein dummy in der liste ??\n\n" );
      exit( 1 );
    }
    i1->info_at[time]->adders = new_excl_bit_vector( gop_vector_length_at[time-1] );
    if ( i1->noop ) {
      (i1->info_at[time]->adders)[i1->noop->uid_block] |= i1->noop->uid_mask;
    }
    for ( e = i1->adders; e && e->ef->first_occurence == time-1; e = e->next ) {
      if ( e->ef->info_at[time-1]->is_dummy ) continue;
      (i1->info_at[time]->adders)[e->ef->op->uid_block] |= e->ef->op->uid_mask;
    }
    i1->info_at[time]->adders_pointer = i1->adders;
  }
  for ( ; i1; i1=i1->next ) {
    if ( i1->info_at[time]->is_dummy ) {
      printf( "\nein dummy in der alten liste ??\n\n" );
      exit( 1 );
    }
    if ( i1->info_at[time-1]->is_dummy ) {
      printf( "\nein ex-dummy in der alten liste ??\n\n" );
      exit( 1 );
    }
    i1->info_at[time]->adders = new_excl_bit_vector( gop_vector_length_at[time-1] );
    if ( time > 1 ) {
      /*
       * achtung! evtl. bei time > 0 die exclusives bereits kopieren;
       * wegen contradicting facts... im augenblick egal, da diese bits
       * bereits in new_ft_level_info gesetzt werden.
       */
      a = i1->info_at[time]->pos_exclusives;
      b = i1->info_at[time-1]->pos_exclusives;
      for ( r = 0; r < gft_vector_length; r++ ) {
	a[r] |= b[r];
      }
      a = i1->info_at[time]->neg_exclusives;
      b = i1->info_at[time-1]->neg_exclusives;
      for ( r = 0; r < gft_vector_length; r++ ) {
	a[r] |= b[r];
      }
      a = i1->info_at[time]->adders;
      b = i1->info_at[time-1]->adders;
      for ( r = 0; r < gop_vector_length_at[time-2]; r++ ) {
	a[r] |= b[r];
      }
    }
    if ( i1->noop ) {
      (i1->info_at[time]->adders)[i1->noop->uid_block] |= i1->noop->uid_mask;
    }
    for ( e = i1->adders; e && e->ef->first_occurence == time-1; e = e->next ) {
      if ( e->ef->info_at[time-1]->is_dummy ) continue;
      (i1->info_at[time]->adders)[e->ef->op->uid_block] |= e->ef->op->uid_mask;
    }
    i1->info_at[time]->adders_pointer = i1->adders;
  }

  i = gft_mutex_pairs;
  while ( i && !facts_are_exclusive( time, i->f1, i->f2 ) ) {
    MAKE_FTS_UNEXCLUSIVE( time, i->f1, i->f2 );
    gexclusions_count--;
    if ( i->f1->positive && i->f2->positive ) {
      gprint_exnum--;
    }
    tmp = i;
    i = i->next;
    free( tmp );
#ifdef MEMORY_INFO
    gexcl_memory -= sizeof( FtPair );
#endif
  }
  gft_mutex_pairs = i;
  prev = i;
  if ( i ) i = i->next;
  while ( i ) {
    if ( !facts_are_exclusive( time, i->f1, i->f2 ) ) {
      MAKE_FTS_UNEXCLUSIVE( time, i->f1, i->f2 );
      gexclusions_count--;
      if ( i->f1->positive && i->f2->positive ) {
	gprint_exnum--;
      }
      prev->next = i->next;
      tmp = i;
      i = i->next;
      free( tmp );
#ifdef MEMORY_INFO
      gexcl_memory -= sizeof( FtPair );
#endif
    } else {
      prev = prev->next;
      i = i->next;
    }
  }

  for ( i1 = gall_fts_pointer; i1 != gprev_level_fts_pointer; i1 = i1->next ) {
    for ( i2 = i1->next; i2; i2 = i2->next ) {
      /* siehe oben
       */
      if ( i2->info_at[time]->is_dummy ) continue;
      if ( i1->index == i2->index ) {
	/* ==> contradicting facts!
	 *
	 * in dieser implemetierung WICHTIG: ft s muessen verschieden sein!
	 */
	continue;
      }
      if ( !facts_are_exclusive( time, i1, i2 ) ) {
	continue;
      }
      MAKE_FTS_EXCLUSIVE( time, i1, i2 );
      if ( i1->positive && i2->positive ) {
	gprint_exnum++;
      }
      gexclusions_count++;
      tmp = new_ft_pair( i1, i2 );
      tmp->next = gft_mutex_pairs;
      gft_mutex_pairs = tmp;
    }

    /* nun sind alle exclusions von i1 berechnet.
     *
     * falls i1 ein dummy war, der gerade erst richtig eingetragen wurde,
     * exclusions runterkopieren auf alle levels, wo i1 noch dummy war!
     */
    if ( i1->info_at[time-1] && 
	 i1->info_at[time-1]->is_dummy ) {
      SET_ADDERS( time, i1 );
      for ( j=time-1; j>0; j-- ) {
	if ( !i1->info_at[j] ) break;

	if ( !i1->info_at[j]->is_dummy ) {/* NUR ZUR VORSICHT... */
	  printf("des kann aber net soi!");
	  exit( 1 );
	}
	
	free( i1->info_at[j]->pos_exclusives );
	free( i1->info_at[j]->neg_exclusives );
	i1->info_at[j]->pos_exclusives = i1->info_at[time]->pos_exclusives;  
	i1->info_at[j]->neg_exclusives = i1->info_at[time]->neg_exclusives;
      }
    }

  }/* for i1 ... */

}








/**********************************************
 * HELPERS ON RELATIONS BETWEEN OPS AND FACTS *
 **********************************************/








Bool competing_needs( int time, OpNode *o1, OpNode *o2 )

{

  BitVector *p = o1->unconditional->info_at[time]->cond_pos_exclusives;
  BitVector *n = o1->unconditional->info_at[time]->cond_neg_exclusives;
  BitVector *b;
  int r;
  FtEdge *i;
  

  if ( !p ) {
    p = new_excl_bit_vector( gft_vector_length );
    n = new_excl_bit_vector( gft_vector_length );
    for ( i = o1->preconds; i; i = i->next ) {
      b = i->ft->info_at[time]->pos_exclusives;
      for ( r = 0; r < gft_vector_length; r++ ) {
	p[r] |= b[r];
      }
      b = i->ft->info_at[time]->neg_exclusives;
      for ( r = 0; r < gft_vector_length; r++ ) {
	n[r] |= b[r];
      }
    }
    o1->unconditional->info_at[time]->cond_pos_exclusives = p;
    o1->unconditional->info_at[time]->cond_neg_exclusives = n;
  }

  b = o2->pos_precond_vector;
  for ( r = 0; r < gft_vector_length; r++ ) {
    if ( p[r] & b[r] ) {
      return TRUE;
    }
  }
  b = o2->neg_precond_vector;
  for ( r = 0; r < gft_vector_length; r++ ) {
    if ( n[r] & b[r] ) {
      return TRUE;
    }
  }
  return FALSE;

}


Bool interfere( OpNode *i1, OpNode *i2 )

{

  BitVector *e1p = i1->unconditional->pos_effect_vector;
  BitVector *e1n = i1->unconditional->neg_effect_vector;
  BitVector *e2p = i2->unconditional->pos_effect_vector;
  BitVector *e2n = i2->unconditional->neg_effect_vector;
  BitVector *p1p = i1->pos_precond_vector;
  BitVector *p1n = i1->neg_precond_vector;
  BitVector *p2p = i2->pos_precond_vector;
  BitVector *p2n = i2->neg_precond_vector;

  int r;

  for ( r = 0; r < gft_vector_length; r++ ) {
    if ( (e1p[r] | p1p[r]) & (e2n[r] | p2n[r]) ) {
      return TRUE;
    }
  }

  for ( r = 0; r < gft_vector_length; r++ ) {
    if ( (e2p[r] | p2p[r]) & (e1n[r] | p1n[r]) ) {
      return TRUE;
    }
  }

  return FALSE;

}


Bool noop_exclusive( OpNode *i1, OpNode *i2 )

{

  OpNode *noop, *op;
  FtNode *ft;
  BitVector *vec;

  if ( !i1->is_noop && !i2->is_noop ) return FALSE;
  if ( i1->is_noop && i2->is_noop ) return FALSE;

  noop = i1->is_noop ? i1 : i2;
  op = i1->is_noop ? i2 : i1;
  ft = noop->preconds->ft;

  /*
   * achtung! wenn s keinen unconditional effect gibt, werden
   * diese vectoren in build_graph.c mit 0 gesetzt!
   */
  if ( ft->positive ) {
    vec = op->unconditional->pos_effect_vector;
  } else {
    vec = op->unconditional->neg_effect_vector;
  }

  if ( vec[ft->uid_block] & ft->uid_mask ) {
    return TRUE;
  } else {
    return FALSE;
  }

}
  
  
Bool facts_are_exclusive( int time, FtNode *f1, FtNode *f2 )

{

  BitVector *excl = f2->info_at[time]->adders_exclusives;
  BitVector *b;
  int r;
  EfEdge *i, *j;
  Bool first = TRUE;
  
  OpNode *op1, *op2;
  FtEdge *ip, *jp;


  if ( f1->noop ) {
    op1 = f1->noop;
    for ( j = f2->adders; j; j = j->next ) {
      op2 = j->ef->op;
      if ( !ARE_MUTEX_OPS( time-1, op1, op2 ) ) {
	for ( jp=j->ef->conditions; jp; jp = jp->next ) {
	  if ( ARE_MUTEX_FTS( time-1, f1, jp->ft ) ) break;
	}
	if ( jp ) {
	  if ( 0 ) {
	    printf("\nnoop - op pair bad!");
	  }
	  continue;
	}
	return FALSE;
      }
    }
  }
  if ( f2->noop ) {
    op1 = f2->noop;
    for ( j = f1->adders; j; j = j->next ) {
      op2 = j->ef->op;
      if ( !ARE_MUTEX_OPS( time-1, op1, op2 ) ) {
	for ( jp=j->ef->conditions; jp; jp = jp->next ) {
	  if ( ARE_MUTEX_FTS( time-1, f2, jp->ft ) ) break;
	}
	if ( jp ) {
	  if ( 0 ) {
	    printf("\nnoop - op pair bad!");
	  }
	  continue;
	}
	return FALSE;
      }
    }
  }

  for ( i = f1->adders; i; i = i->next ) {
    for ( j = f2->adders; j; j = j->next ) {
      op1 = i->ef->op;
      op2 = j->ef->op;
      if ( !ARE_MUTEX_OPS( time-1, op1, op2 ) ) {

	for ( ip = i->ef->conditions; ip; ip = ip->next ) {
          for ( jp = j->ef->conditions; jp; jp = jp->next ) {
            if ( ARE_MUTEX_FTS( time-1, ip->ft, jp->ft ) ) break;
	  }
          if ( jp ) break;

          for ( jp = op2->preconds; jp; jp = jp->next ) {
            if ( ARE_MUTEX_FTS( time-1, ip->ft, jp->ft ) ) break;
	  }
          if ( jp ) break;
        }
        if ( ip ) {
	  if ( 0 ) {
	    printf("\nop pair bad!");
	  }
	  continue;
	}

        for ( ip = op1->preconds; ip; ip = ip->next ) {
          for ( jp = j->ef->conditions; jp; jp = jp->next ) {
            if ( ARE_MUTEX_FTS( time-1, ip->ft, jp->ft ) ) break;
	  }
          if ( jp ) break;
        }
        if ( ip ) {
	  if ( 0 ) {
	    printf("\nop pair bad!");
	  }
	  continue;
	}

	return FALSE;

      }
    }
  }

  return TRUE;


  /* bit vector code a la STAN;
   * funktioniert nur bei unconditional domains,
   * sonst kann man effect conds - excl nicht
   * abfragen.
   *
   * laengerfristig: EXCL UEBER EFFECTS DEFINIEREN!!!
   */
  if ( !excl ) {
    if ( f2->noop ) {
      excl = copy_bit_vector( f2->noop->info_at[time-1]->exclusives, gop_vector_length_at[time-1] );
      first = FALSE;
    }
    for ( i = f2->adders; i; i = i->next ) {
      if ( !f2->noop && first ) {
	excl = copy_bit_vector( i->ef->op->info_at[time-1]->exclusives, gop_vector_length_at[time-1] );
	first = FALSE;
      }
      b = i->ef->op->info_at[time-1]->exclusives;
      for ( r = 0; r < gop_vector_length_at[time-1]; r++ ) {
	excl[r] &= b[r];
      }
    }
    f2->info_at[time]->adders_exclusives = excl;
  }

  b = f1->info_at[time]->adders;
  for ( r = 0; r < gop_vector_length_at[time-1]; r++ ) {
    if ( b[r] != ( b[r] & excl[r] ) ) {
      break;
    }
  }

  return ( r == gop_vector_length_at[time-1] );

}




