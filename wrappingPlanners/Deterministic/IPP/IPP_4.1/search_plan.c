


/*********************************************************************
 * File: search_plan.c
 * Description: contains the machinery necessary to search the graph
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
 * NOTE: the commentaries in this file are all in German, cause these are
 *       notes that I made for my own use while working on the search
 *       code, which is really terribly complicated.
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

#include "search_plan.h"
#include "memoize.h"
#include "wave_front.h"








/**************************************************
 * SIMPLE HELPERS, SHOULD BE MACROS. OFTEN CALLED *
 **************************************************/









void DO_OP( int time, OpNode *op )

{

  int was_used;

  was_used = op->info_at[time-1]->is_used++;
  if ( !was_used && gnum_ops_at[time-1] == ARRAY_SIZE ) {
    printf( "\n\nipp: increase ARRAY_SIZE( preset value: %d )", ARRAY_SIZE );
    exit( 1 );
  }
  if ( !was_used ) {
    gops_at[time-1][gnum_ops_at[time-1]++] = op;
    gnum_of_actions_tried++;
    if ( op->is_noop ) {
      gnum_of_noops_tried++;
    }
  }

}



void UNDO_OP( int time, OpNode *op )

{

  int was_used;

  was_used = --op->info_at[time-1]->is_used;
  if( !was_used ) {
    gops_at[time-1][--gnum_ops_at[time-1]] = NULL;
  }

}



void DO_FT( int time, FtNode *ft, OpNode *op )

{

  int was_used;
  FtNode *neg_ft = ft->positive ? gft_table[NEG_ADR( ft->index )] :
    gft_table[ft->index];
    
  if ( !neg_ft || !neg_ft->info_at[time] ) {
    return;
  }

  was_used = ft->info_at[time]->is_goal++;

  if ( ( !was_used && gnum_goals_at[time] == ARRAY_SIZE ) ||
       ft->info_at[time]->is_goal > ARRAY_SIZE ) {
    printf( "\n\nipp: increase ARRAY_SIZE( preset value: %d )", ARRAY_SIZE );
    exit( 1 );
  }
  (ft->info_at[time]->is_goal_for)[ft->info_at[time]->is_goal-1] = op;
  if ( !was_used ) {
    ggoals_at[time][gnum_goals_at[time]++] = ft;
    if ( ft->positive ) {
      gpos_goals_vector_at[time][ft->uid_block] |= ft->uid_mask;
    } else {
      gneg_goals_vector_at[time][ft->uid_block] |= ft->uid_mask;
    }
  }

}



void UNDO_FT( int time, FtNode *ft )

{

  int was_used;
  FtNode *neg_ft = ft->positive ? gft_table[NEG_ADR( ft->index )] :
    gft_table[ft->index];
    
  if ( !neg_ft || !neg_ft->info_at[time] ) {
    return;
  }

  was_used = --ft->info_at[time]->is_goal;
  (ft->info_at[time]->is_goal_for)[ft->info_at[time]->is_goal] = NULL;
  if ( !was_used ) {
    ggoals_at[time][--gnum_goals_at[time]] = NULL;
    if ( ft->positive ) {
      gpos_goals_vector_at[time][ft->uid_block] &= ~(ft->uid_mask);
    } else {
      gneg_goals_vector_at[time][ft->uid_block] &= ~(ft->uid_mask);
    }
  }

}




FtNode *NEG_FT( FtNode *ft )

{

  return ( ft->positive ? gft_table[NEG_ADR( ft->index )] : gft_table[ft->index] );

}









/*********************
 * SETTING UP SEARCH *
 *********************/









Bool search_plan( int max_time )

{

  int i;
  Bool result;
  Integers *j;


  expand( max_time );


  /* achtung!! hier muss noch irgendwie die reihenfolge der ziele
   * in bezug auf definition im domain file beruecksichtigt werden
   */
  gnum_goals_at[max_time] = 0;
  for ( j = gbit_goal_state->positive->indices; j; j = j->next ) {
    DO_FT( max_time, gft_table[j->index], NULL );
  }
  for ( j = gbit_goal_state->negative->indices; j; j = j->next ) {
    DO_FT( max_time, gft_table[NEG_ADR( j->index )], NULL );
  }


  gnum_ops_at[max_time-1] = 0;
  gnum_goals_at[max_time-1] = 0;

  result = search( max_time, gnum_goals_at[max_time] - 1);

  for ( i=gnum_goals_at[max_time] - 1; i>-1; i-- ) {
    UNDO_FT( max_time, ggoals_at[max_time][i] );
  }

  if ( !result ) memoize( max_time );
                        
  return result;

}








/********************************
 * THE TWO MAIN SEARCH FUNCIONS *        
 ********************************/










Bool search( int time, int curr_index )

{

  EfEdge *ef_i;
  EfNode *ef;
  FtNode *ft;

  if ( time == 0 ) return TRUE;


  if ( curr_index < 0 ) {

    if ( !action_set_is_minimal( time ) ) {
      return FALSE;
    }

    return complete_goals_and_search( time );

  }


  while ( ( ft = ggoals_at[time][curr_index] )->info_at[time]->is_true ) {
    curr_index--;
    if ( curr_index < 0 ) {
      return search( time, -1 );
    }
  }


  if ( ft->noop ) {

    if ( try_noop( time, ft, curr_index ) ) {

      if ( search( time, curr_index - 1 ) ) {
	return TRUE;
      }     

      untry_noop( time, ft );
    }
  }


  for ( ef_i = ft->info_at[time]->adders_pointer; ef_i; ef_i = ef_i->next ) { 
    ef = ef_i->ef;

    if ( !try_ef( time, ef, curr_index ) ) {
      continue;
    }

    if ( search( time, curr_index - 1 ) ) {
      return TRUE;
    }
    

    untry_ef( time, ef );
  }

  return FALSE;

}






Bool complete_goals_and_search( int time )

{

  int i, j;
  OpNode *op;
  EfNode *i_ef;
  FtEdge *i_ft;
  FtNode *ft;

  for ( i = 0; i < gnum_ops_at[time-1]; i++ ) {
    op = gops_at[time-1][i];

    for ( i_ef = op->conditionals; i_ef; i_ef = i_ef->next ) {

      /* ALLE effekte, also auch dummys, werden auf schaedlichkeit
       * geprueft!!!
       */

      if ( i_ef->first_occurence > time-1 ) {
	/* erst soweit runterspulen, bis wir auf dem richtigen level sind...
	 * (zeiger op->conditionals zeigt auf letzten hinzugekommenen
	 * effekt...)
	 *
	 * ACHTUNG: INEFFIZIENT. koennte mit einfachen mitteln direkt
	 * drauf zugegriffen werden (op_level_info den korrekten anfang
	 * mitspeichern oder so...)
	 */
	continue;
      }

      if ( cant_do_conditions( time, i_ef ) ) {
	/* effekt hat onehin bedingungen, die nicht mit neuen zielen
	 * wahr sein koennen; also gar nicht erst auf schaedlichkeit
	 * pruefen.
	 *
	 * beachte: in exclusivitaet ist widerspruechlichkeit bereits
	 * enthalten, also werden hier bereits gewaehlte verhinderungs
	 * goals erkannt.
	 *
	 * ACHTUNG DUMMYS: dummy knoten haben zwar keine explizite exclusivitaet,
	 *                 solange sie noch nicht integriert sind,
	 *                 (PRINZIPIELL IST AUCH EIGENE BERECHNUNG DENKBAR), aber
	 *                 auf widerspruechlichkeit, also insbesondere effekt
	 *                 verhinderung wird hier reagiert, da auch dummy
	 *                 knoten automatisch exclusiv zu ihrer negation sind.
	 */
	continue;
      }

      for ( i_ft = i_ef->effects; i_ft; i_ft = i_ft->next ) {

	if ( (ft = NEG_FT( i_ft->ft )) == NULL ||
	     !ft->info_at[time] ) {
	  /* widersprechendes fact kann kein ziel sein, da (noch)
	   * nicht vorhanden auf step time
	   */
	  continue;
	}

        if ( !ft->info_at[time]->is_goal && 
	     !( ft->info_at[time-1] && ft->info_at[time-1]->is_goal ) ) {
	  /* widersprechendes fact kein ziel und 
	   * auf neuem step nicht da oder auch kein ziel
	   */
	  continue;
	}

	if ( !ft->info_at[time]->is_goal ) {
	  /* ist neues ziel: nur einschreiten, falls nicht ausschliesslich
	   * ziel fuer op selbst
	   */
	  for ( j=0; j<ft->info_at[time-1]->is_goal; j++ ) {
	    if ( (ft->info_at[time-1]->is_goal_for)[j] != op ) break;
	  }
	  if ( j == ft->info_at[time-1]->is_goal ) {
	    /* fact ist nur widerspruch zu op's eigenen effekten
	     */
	    continue;
	  }
	}

	/* wir muessen diesen effekt verhindern!!
	 *
	 * aus uebersichtlichkeitsgruenden sprung in die
	 * effekt schleife.
	 */
	break;
      }

      if ( !i_ft ) {
	/* haben kein schaedliches fact in diesem effekt
	 * gefunden! --> naechsten effekt pruefen.
	 */
	continue;
      }

      /* der reihe nach versuchen, die bedingungen zu 
       * verhindern
       */
      for ( i_ft = i_ef->conditions; i_ft; i_ft = i_ft->next ) {
	
	if ( i_ft->ft->info_at[time-1]->is_goal ) {
	  /* conditions sind auf time-1 garantiert da...
	   * ACHTUNG!!! wenns spaeter dummys gibt
	   *            > kein problem:(?) werden als knoten eingetragen.
	   *
	   * neue ziele koennen nicht verhindert werden.
	   */
	  continue;
	}
	
	if ( (ft = NEG_FT( i_ft->ft )) == NULL ||
	     !ft->info_at[time-1] ) {
	  /* widerspruchs fact, also verhinderung, (noch) nicht da
	   */
	  continue;
	}

	if ( cant_do_ft( time-1, ft ) ||
	     is_deleted( time-1, ft, op ) ) {
	  /* widerspruchsbedingung ist exklusiv zu ausgewaehlten
	   * zielen oder
	   * wird von ausgewaehltem op' != op zerstoert...
	   *
	   * hier auch moeglich: effekte als used markieren und
	   * bei zerstoerung beruecksichtigen!!
	   *
	   * ebenfalls moeglich: getriggerte effekte beruecksichtigen,
	   * solche werden aber spaeter im fixpunkt auch noch erkannt.
	   * effizienzfrage...??
	   */
	  continue;
	}

	/* HOPPLA: effekt bedingung koennte dummy gewesen sein.
	 *         wenn das aber der fall ist, dann ist die negation
	 *         WEGEN CWA schon da, also KEIN DUMMY. deshalb
	 *         werden hier wie gewuenscht nur nicht-dummys
	 *         als ziele eingesetzt!!
	 *
	 *       OHNE CWA NICHT KORREKT!!!!
	 */
	DO_FT( time-1, ft, op );
  
	if ( complete_goals_and_search( time ) ) {
	  return TRUE;
	}

	UNDO_FT( time-1, ft );
      }

      /* schaedlicher effekt kann hier nicht verhindert werden!
       */
      return FALSE;
    }
  }

  /* an dieser stelle sind alle ausgewaehlten ops auf nicht -
   * schaedlichkeit geprueft.
   *
   * suche fortsetzen.
   */

  /* hier nochmal minimalitaets check !! ??
   */
  if ( !action_set_is_minimal( time ) ) {
    return FALSE;
  }


  if ( memoized( time - 1 ) ) {
    return FALSE;
  }

  if ( time > 1 ) {
    gnum_ops_at[time-2] = 0;
    gnum_goals_at[time-2] = 0;
  }
	
  if ( search( time - 1, gnum_goals_at[time-1] - 1 ) ) {
    return TRUE;
  }

  memoize( time - 1 );

  if ( gsame_as_prev_flag && time == gfirst_full_time + 1 ) {
    add_candidate( time - 1 );
  }

  return FALSE;
      
}









/********************************
 * SOPHISTICATED SEARCH HELPERS *
 ********************************/








Bool action_set_is_minimal( int time )

{

  int i;
  Bool result = TRUE;
  EfNode *i_ef;
  FtEdge *i_ft;
  FtNode *neg_ft = NULL;

  /* zunaechst werden alle getriggerten effekte eingetragen.
   *
   * wie ueblich: INEFFIZIENT! besser waere es, explizit ausgewaehlte
   * effekte zu markieren, dann braucht man die nicht mehr anzuschauen.
   */
  for ( i = 0; i < gnum_ops_at[time-1]; i++ ) {
    for ( i_ef = gops_at[time-1][i]->conditionals; i_ef; i_ef = i_ef->next ) {
      if ( i_ef->first_occurence > time-1 ) {
	/* INEFFIZIENT: wie im complete - goals pattern waere es besser,
	 * direkt vom op auf die hier vorhandenen effekte zugreifen zu 
	 * koennen.
	 */
	continue;
      }
      for ( i_ft = i_ef->conditions; i_ft; i_ft = i_ft->next ) {
	neg_ft = i_ft->ft->positive ? gft_table[NEG_ADR( i_ft->ft->index )] :
	  gft_table[i_ft->ft->index];
	if ( neg_ft &&
	     neg_ft->info_at[time-1] &&
	     !i_ft->ft->info_at[time-1]->is_goal ) {
	  break;
	}
      }
      if ( !i_ft ) {
	for ( i_ft = i_ef->effects; i_ft; i_ft = i_ft->next ) {
	  i_ft->ft->info_at[time]->is_true++;
	}
      }
    }
  }

  /* wenn es einen op gibt, der kein einziges fact alleine addet, dann
   * ist das set nicht minimal.
   */
  for ( i = 0; i < gnum_ops_at[time-1]; i++ ) {
    if ( gops_at[time-1][i]->is_noop ) {/* special fuer noops */
      if ( gops_at[time-1][i]->preconds->ft->info_at[time]->is_true == 1 ) {
	continue;
      }
    }
    for ( i_ft = gops_at[time-1][i]->unconditional->effects;
	  i_ft; i_ft = i_ft->next ) {/* der unkonditionale rettet ? */
      if ( !i_ft->ft->info_at[time]->is_goal ) continue;
      if ( i_ft->ft->info_at[time]->is_true == 1 ) break;
    }
    if ( i_ft ) {
      continue;
    }
    for ( i_ef = gops_at[time-1][i]->conditionals; i_ef; i_ef = i_ef->next ) {
      /* eigentlich duerften ja nur getriggerte retten...
       * konzeptuell aendern ?
       */
      if ( i_ef->first_occurence > time-1 ) {
	continue;
      }
      for ( i_ft = i_ef->effects; i_ft; i_ft = i_ft->next ) {
	if ( !i_ft->ft->info_at[time]->is_goal ) {
	  continue;
	}
	if ( i_ft->ft->info_at[time]->is_true == 1 ) {
	  break;
	}
      }
      if ( i_ft ) break;
    }
    if ( i_ef ) {/* ein konditionaler hat gerettet */
      continue;
    }
    /* kein rettender effekt wurde gefunden.
     */
    result = FALSE;
    break;
  }


  /* info von oben wieder zuruecksetzen
   */
  for ( i = 0; i < gnum_ops_at[time-1]; i++ ) {
    for ( i_ef = gops_at[time-1][i]->conditionals; i_ef; i_ef = i_ef->next ) {
      if ( i_ef->first_occurence > time-1 ) {
	continue;
      }
      for ( i_ft = i_ef->conditions; i_ft; i_ft = i_ft->next ) {
	neg_ft = i_ft->ft->positive ? gft_table[NEG_ADR( i_ft->ft->index )] :
	  gft_table[i_ft->ft->index];
	if ( neg_ft &&
	     neg_ft->info_at[time-1] &&
	     !i_ft->ft->info_at[time-1]->is_goal ) {
	  break;
	}
      }
      if ( !i_ft ) {
	for ( i_ft = i_ef->effects; i_ft; i_ft = i_ft->next ) {
	  i_ft->ft->info_at[time]->is_true--;
	}
      }
    }
  }

  return result;


} 





Bool goals_still_possible( int time, int n, OpNode *op )

{

  int i, j, a;
  unsigned int b;
  int e[gop_vector_length_at[time-1]];
  EfEdge *i_ef;

  /* die exclusives aller ausgwaehlten ops und von op zusammen OR en
   */
  for ( i=0; i<gop_vector_length_at[time-1]; i++ ) {
    e[i] &= 0;
    for ( j=0; j<gnum_ops_at[time-1]; j++ ) {
      e[i] |= gops_at[time-1][j]->info_at[time-1]->exclusives[i];
    }
    e[i] |= op->info_at[time-1]->exclusives[i];
  }

  /* alle noch verbleibenden ziele
   */
  for ( i=0; i<n; i++ ){
    if ( ggoals_at[time][i]->noop &&
	 ggoals_at[time][i]->noop->unconditional->first_occurence <= time-1 ) {
      /* geht der noop noch ? */
      a = ggoals_at[time][i]->noop->uid_block;
      b = ggoals_at[time][i]->noop->uid_mask;
      if ( ( e[a] & b ) == 0 ) continue;
    } 
    for ( i_ef = ggoals_at[time][i]->info_at[time]->adders_pointer; 
	  i_ef; i_ef = i_ef->next ) {
      /* geht irgendein effekt noch ?
       */
      if ( ( ( e[i_ef->ef->op->uid_block] ) & ( i_ef->ef->op->uid_mask ) ) == 0 ) {
	break;
      }
    }
    if ( !i_ef ) return FALSE;
  }

  return TRUE;

} 





/**********************************************
 * FUNCIONS FOR TRYING/UNTRYING OPS AND NOOPS *
 **********************************************/









Bool try_noop( int time, FtNode *ft, int curr_index )

{

  OpNode *noop;
  BitVector *a, *b;
  int r;
  FtNode *negft;


  noop = ft->noop;

  if ( noop->unconditional->first_occurence > time-1 ) {
    return FALSE;
  }

  if ( cant_do_op( time, noop ) ) {
    return FALSE; 
  }

  a = ft->info_at[time-1]->pos_exclusives;
  b = ft->info_at[time-1]->neg_exclusives;
  for ( r=0; r<gft_vector_length; r++ ) {
    if ( a[r] & gpos_goals_vector_at[time-1][r] ) return FALSE;
    if ( b[r] & gneg_goals_vector_at[time-1][r] ) return FALSE;
  }

  if ( ft->positive ) {
    negft = gft_table[NEG_ADR( ft->index )];
  } else {
    negft = gft_table[ft->index];
  }

  /* achtung! wenn's negft auf diesem level noch gar nicht gibt, 
   * ist ft automatisch war und braeuchte gar nicht erst in
   * goals at aufgenommen zu werden.
   *
   * muesste man beim new goal einziehen beruecksichtigen.
   */

  if ( negft && negft->info_at[time-1] &&
       negft->info_at[time-1]->is_goal ) {
    return FALSE;
  }

  if ( !goals_still_possible( time, curr_index, noop ) ) {
    return FALSE;
  }

  DO_OP( time, noop );

  DO_FT( time-1, ft, noop );

  ft->info_at[time]->is_true++;

  return TRUE;

}



Bool try_ef( int time, EfNode *ef, int curr_index )

{

  OpNode *op;
  FtEdge *i_ft;
  FtNode *ft;


  if ( ef->info_at[time-1]->is_dummy ) {
    /* das reicht aus, um zu gewaehrleisten, dass KEIN DUMMY ZIEL WIRD.
     * und zwar deswegen, weil keine nicht-dummy-effekte dummy facts
     * benutzen koennen; diese werden in den graph - vorhanden bit strings
     * nicht vermerkt.
     *
     * WICHTIG: das MUSS in BUILD_GRAPH gewaehrleistet werden, d.h. kein
     *          normaler effekt darf dummys benutzen!!
     */
    return FALSE;
  }

  op = ef->op;


  if ( !op->info_at[time-1]->is_used ) {

    if ( cant_do_op( time, op ) ) {
      return FALSE; 
    }

    /* im unterschied zu conditions koennen preconds NICHT
     * von bereits ausgewaehlten ops deleted werden;
     * diese ops sind exclusiv...
     */
    if ( cant_do_preconds( time, ef ) ) {
      return FALSE;
    }

    /* ACHTUNG!! man koennte sowas auch mit noch-nicht-ausgewaehlten 
     * konditionalen effekten machen, die ja onehin getriggert werden!
     *
     * KONSEQUENT HIESSE DAS, EFFEKT -> IS USED EINZUFUEHREN!!!
     */
    for ( i_ft = op->unconditional->effects; i_ft; i_ft = i_ft->next ) {

      if ( (ft = NEG_FT( i_ft->ft )) == NULL ||
	   !ft->info_at[time] ) {
	/* widersprechendes fact (noch) nicht da auf level time
	 */
	continue;
      }

      /* widersprechende neue ziele weil diese effect conditions von
       * ausgewaehlten ops sein koennen (nicht preconds, sonst excl)
       */
      /* widersprechende alte ziele: bei reinem STRIPS waeren alle adder
       * automatisch excl von diesem op; also schlagen alle auswahlen 
       * fehl, dieser op wird spaeter zureuckgenommen.(moeglich, siehe
       * graphplan, aber ineffizient). in unserem fall koennen auch
       * conditionals dieses fact adden, daher explizite abfrage NOETIG!!
       */ 
      if ( ft->info_at[time]->is_goal ||
	   ( ft->info_at[time-1] && ft->info_at[time-1]->is_goal ) ) {
	return FALSE;
      }
    }

    if ( !goals_still_possible( time, curr_index, op ) ) {
      return FALSE;
    }
  }

  if ( ef->conditions && cant_do_conditions( time, ef ) ) {
    return FALSE;
  }
  /* der -wird deleted- test; muss nur fuer effektbedingungen
   * ausgefuehrt werden.
   *
   * faengt zur zeit nur uncond deletes ab; man koennte (siehe oben)
   * auf jeden fall AUSGEWAEHLTE EFFEKTE book keeping machen und
   * diese auch ausschliessen, das wird zur zeit erst im fixpunkt
   * gemacht, wo es keine moeglichkeit zur verhinderung gibt.
   *
   * auch getriggerte effs moeglich, aber wahrscheinlich ineffizient
   *
   * SPAETER HIER DUMMY - TEST!!
   */
  for ( i_ft = ef->conditions; i_ft; i_ft = i_ft->next ) {
    if ( is_deleted( time-1, i_ft->ft, op  ) ) {
      return FALSE;
    }
  }



  DO_OP( time, op );

  for ( i_ft = op->preconds; i_ft; i_ft = i_ft->next ) {
    DO_FT( time - 1, i_ft->ft, op );
  }

  for ( i_ft = ef->conditions; i_ft; i_ft = i_ft->next ) {
    DO_FT( time - 1, i_ft->ft, op );
  }


  for ( i_ft = op->unconditional->effects; i_ft; i_ft = i_ft->next ) {
    i_ft->ft->info_at[time]->is_true++;
  }

  return TRUE;
  
}



void untry_noop( int time, FtNode *ft )

{

  ft->info_at[time]->is_true--;

  UNDO_FT( time - 1, ft );

  UNDO_OP( time, ft->noop );

}



void untry_ef( int time, EfNode *ef )

{

  FtEdge *i_ft;
  OpNode *op = ef->op;

  for ( i_ft = op->unconditional->effects; i_ft; i_ft = i_ft->next ) {
    i_ft->ft->info_at[time]->is_true--;
  }

  for ( i_ft = ef->conditions; i_ft; i_ft = i_ft->next ) {
    UNDO_FT( time - 1, i_ft->ft );
  }

  for ( i_ft = ef->op->preconds; i_ft; i_ft = i_ft->next ) {
    UNDO_FT( time - 1, i_ft->ft );
  }


  UNDO_OP( time, ef->op );

}








/***************************************************
 * HELPERS ON GOAL / OP PROPERTIES AT SEARCH STAGE *
 ***************************************************/









Bool cant_do_ft( int time, FtNode *ft )

{

  int r;
  BitVector *a, *b;

  a = ft->info_at[time]->pos_exclusives;
  b = ft->info_at[time]->neg_exclusives;
  for ( r=0; r<gft_vector_length; r++ ) {
    if ( a[r] & gpos_goals_vector_at[time][r] ) return TRUE;
    if ( b[r] & gneg_goals_vector_at[time][r] ) return TRUE;
  }

  return FALSE;

}

 
Bool is_deleted( int time, FtNode *ft, OpNode *op )

{

  int i;
  OpNode *op2;
  FtNode *ft2;
  FtEdge *i_ft;

  for ( i=0; i<gnum_ops_at[time]; i++ ) {
    op2 = gops_at[time][i];

    if ( op2 == op ) {
      continue;
    }

    for ( i_ft = op2->unconditional->effects; i_ft; i_ft = i_ft->next ) { 
      if ( (ft2 = NEG_FT( i_ft->ft)) != ft ) {
	/* beinhaltet den fall, dass ft2 == NULL
	 */
	continue;
      }
      return TRUE;
    }

    /* ACHTUNG!!! hier koennte man 1. noch eine AUSGEWAEHLTE
     * COND. EFFEKTE - behandlung einbauen, d.h. auch cond. zerstoerung
     * ohne grossen mehraufwand feststellen;
     *
     * 2. GETRIGGERTE effekte suchen; dies wird allerdings     
     * spaeter im fixpunkt gefunden, durch
     * 'leere verhinderungsliste'.
     * also nicht noetig, fragt sich, was schneller ist.
     */
  }

  return FALSE;

}



Bool cant_do_op( int time, OpNode *op )

{

  int i;

  for ( i=0; i<gnum_ops_at[time-1]; i++ ) {
    if ( (gops_at[time-1][i]->info_at[time-1]->exclusives)[op->uid_block]
	 & op->uid_mask ) {
      return TRUE;
    }
  }

  return FALSE;

}


Bool cant_do_preconds( int time, EfNode *ef )

{

  BitVector *p = ef->op->unconditional->info_at[time-1]->cond_pos_exclusives;
  BitVector *n = ef->op->unconditional->info_at[time-1]->cond_neg_exclusives;
  BitVector *b, *c;
  int r;
  OpNode *op = ef->op;
  FtEdge *i;

  if ( !p ) {
    p = new_excl_bit_vector( gft_vector_length );
    n = new_excl_bit_vector( gft_vector_length );
    for ( i = op->preconds; i; i = i->next ) {
      b = i->ft->info_at[time-1]->pos_exclusives;
      c = i->ft->info_at[time-1]->neg_exclusives;
      for ( r = 0; r < gft_vector_length; r++ ) {
	p[r] |= b[r];
	n[r] |= c[r];
      }
    }
    op->unconditional->info_at[time-1]->cond_pos_exclusives = p;
    op->unconditional->info_at[time-1]->cond_neg_exclusives = n;
  }

  for ( r=0; r<gft_vector_length; r++ ) {
    if ( gpos_goals_vector_at[time-1][r] & p[r] ) {
      return TRUE;
    }
    if ( gneg_goals_vector_at[time-1][r] & n[r] ) {
      return TRUE;
    }
  }
  return FALSE;

}
 

  
Bool cant_do_conditions( int time, EfNode *ef )

{

  BitVector *p = ef->info_at[time-1]->cond_pos_exclusives;
  BitVector *n = ef->info_at[time-1]->cond_neg_exclusives;
  BitVector *b, *c;
  int r;
  FtEdge *i;

  if ( !p ) {
    p = new_excl_bit_vector( gft_vector_length );
    n = new_excl_bit_vector( gft_vector_length );
    for ( i = ef->conditions; i; i = i->next ) {
      b = i->ft->info_at[time-1]->pos_exclusives;
      c = i->ft->info_at[time-1]->neg_exclusives;
      for ( r = 0; r < gft_vector_length; r++ ) {
	p[r] |= b[r];
	n[r] |= c[r];
      }
    }
    ef->info_at[time-1]->cond_pos_exclusives = p;
    ef->info_at[time-1]->cond_neg_exclusives = n;
  }

  for ( r=0; r<gft_vector_length; r++ ) {
    if ( gpos_goals_vector_at[time-1][r] & p[r] ) {
      return TRUE;
    }
    if ( gneg_goals_vector_at[time-1][r] & n[r] ) {
      return TRUE;
    }
  }
  return FALSE;

}
 



  
  


/*******************
 * GENERAL HELPERS * 
 *******************/










void expand( int max_time )

{

  static Bool first = TRUE;
  int i;

  if ( ggoals_at != NULL ) free( ggoals_at );
  ggoals_at = ( FtArray * ) calloc( max_time + 1, sizeof( FtArray ) );
  CHECK_PTR(ggoals_at);
  if ( gnum_goals_at != NULL ) free( gnum_goals_at );
  gnum_goals_at = ( int * ) calloc( max_time + 1, sizeof( int ) );
  CHECK_PTR(gnum_goals_at);
  
  if ( first ) {
    for ( i=0; i<max_time; i++ ) {
      gpos_goals_vector_at[i] = new_bit_vector( gft_vector_length );
      gneg_goals_vector_at[i] = new_bit_vector( gft_vector_length );
    }
    first = FALSE;
  }
  gpos_goals_vector_at[max_time] = new_bit_vector( gft_vector_length );
  gneg_goals_vector_at[max_time] = new_bit_vector( gft_vector_length );

  if ( gops_at != NULL ) free( gops_at );
  gops_at = ( OpArray * ) calloc( max_time, sizeof( OpArray ) );
  CHECK_PTR(gops_at);
  if ( gnum_ops_at != NULL ) free( gnum_ops_at );
  gnum_ops_at = ( int * ) calloc( max_time, sizeof( int ) );
  CHECK_PTR(gnum_ops_at);

}

