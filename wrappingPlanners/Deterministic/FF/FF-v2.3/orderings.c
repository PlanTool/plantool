
/*********************************************************************
 * (C) Copyright 2001 Albert Ludwigs University Freiburg
 *     Institute of Computer Science
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 * 
 *********************************************************************/


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
 * File: orderings.c
 *
 * Description: implements, in this version, the standard GAM approach
 *              using the ordering relation \leq_h as described in
 *              Koehler/Hoffmann: ``On reasonable and forced goal
 *              orderings and their use in an incremental planning
 *              algorithm'', JAIR 2000
 *
 *              - compute the False set of each goal fact
 *              - detect ordering constraints between pairs of goals
 *              - create the Goal Agenda
 *
 * Author: Joerg Hoffmann 1999
 *
 *********************************************************************/ 






#include "ff.h"

#include "output.h"
#include "memory.h"

#include "orderings.h"










/* local globals
 */








int *lch;
int lnum_ch;
Bool *lin_ch;

int *lDcount;

Bool *lin;

Bool **lm;









/* main function
 */








void compute_goal_agenda( void )

{

  int i;
  int max = gnum_ef_conn > gnum_ft_conn ? 
    gnum_ef_conn : gnum_ft_conn;
  
  /* initialization stuff
   */
  lch = ( int * ) calloc( max, sizeof( int ) );
  lin_ch = ( Bool * ) calloc( max, sizeof( Bool ) );
  for ( i = 0; i < max; i++ ) {
    lin_ch[i] = FALSE;
  }

  lDcount = ( int * ) calloc( gnum_ft_conn, sizeof( int ) );
  for ( i = 0; i < gnum_ft_conn; i++ ) {
    lDcount[i] = 0;
  }

  /* False sets
   */
  for ( i = 0; i < ggoal_state.num_F; i++ ) { 
    build_False_set( ggoal_state.F[i] );
  }

  /* heuristic reasonable orderings
   */
  detect_ordering_constraints();

  /* build orderings into goal agenda
   */
  build_goal_agenda();

}










/* false set computation for each goal
 */










void build_False_set( int ft )

{

  int i, j, k, count;
  int ef, ft_, ef_;

  lnum_ch = 0;

  count = 0;
  for ( i = 0; i < gft_conn[ft].num_A; i++ ) {
    ef = gft_conn[ft].A[i];
    count++;
    for ( j = 0; j < gef_conn[ef].num_D; j++ ) {
      ft_ = gef_conn[ef].D[j];
      lDcount[ft_]++;
      if ( !lin_ch[ft_] ) {
	lch[lnum_ch++] = ft_;
	lin_ch[ft_] = TRUE;
      }
    } 
    for ( j = 0; j < gef_conn[ef].num_I; j++ ) {
      ef_ = gef_conn[ef].I[j];
      count++;
      for ( k = 0; k < gef_conn[ef_].num_D; k++ ) {
	ft_ = gef_conn[ef_].D[k];
	lDcount[ft_]++;
	if ( !lin_ch[ft_] ) {
	  lch[lnum_ch++] = ft_;
	  lin_ch[ft_] = TRUE;
	}
      }
    }
  }

  /* only those that where deleted can be in False set
   *
   * DANGER: this relies on that the function is called only once
   *         for each fact ft
   */
  gft_conn[ft].False = ( int * ) calloc( lnum_ch, sizeof( int ) );

  gft_conn[ft].num_False = 0;
  for ( i = 0; i < lnum_ch; i++ ) {
    if ( lDcount[lch[i]] == count ) {
      /* each adder deleted this fact
       */
      gft_conn[ft].False[gft_conn[ft].num_False++] = lch[i];
    }
  }

  /* undo Dcount and lch information now
   */
  for ( i = 0; i < lnum_ch; i++ ) {
    lDcount[lch[i]] = 0;
    lin_ch[lch[i]] = FALSE;
  }

  if ( gcmd_line.display_info == 125 ) {
    printf("\n\ncomputed False set of ");
    print_ft_name( ft );
    printf(" as follows:");
    for ( i = 0; i < gft_conn[ft].num_False; i++ ) {
      printf("\n");
      print_ft_name( gft_conn[ft].False[i] );
    }
  }

}
 









/* look at pairs of goals and see if they are ordered
 * heuristically reasonable
 */











void detect_ordering_constraints( void )

{

  int i, j, n = ggoal_state.num_F;

  /* initialize usability array
   */
  lin = ( Bool * ) calloc( gnum_ef_conn, sizeof( Bool ) );
  for ( i = 0; i < gnum_ef_conn; i++ ) {
    lin[i] = TRUE;
  }

  /* initialize orderings matrix.
   *
   * m[i][j] == TRUE gdw. goal[i] \leq_h goal[j]
   */
  lm = ( Bool ** ) calloc( n, sizeof( Bool * ) );
  for ( i = 0; i < n; i++ ) {
    lm[i] = ( Bool * ) calloc( n, sizeof( Bool ) );
  }
  for ( i = 0; i < n; i++ ) {
    for ( j = 0; j < n; j++ ) {
      lm[i][j] = ( i == j ? TRUE : FALSE );
    }
  }

  /* check each pair of goals i, j for heuristic
   * reasonable ordering.
   *
   * order of pairs due to speedup by marking
   * unusable efs for each as long as possible constant
   * goal i
   */
  for ( i = 0; i < n - 1; i++ ) {
    setup_E( ggoal_state.F[i] );
    for ( j = i + 1; j < n; j++ ) {
      lm[j][i] = !possibly_achievable( ggoal_state.F[j] );
      if ( gcmd_line.display_info == 126 &&
	   lm[j][i] ) {
	printf("\norderings: ");
	print_ft_name( ggoal_state.F[j] );
	printf(" <= ");
	print_ft_name( ggoal_state.F[i] );
      }
    }
    unsetup_E( ggoal_state.F[i] );
  }
  for ( i = n - 1; i > 0; i-- ) {
    setup_E( ggoal_state.F[i] );
    for ( j = i - 1; j > -1; j-- ) {
      lm[j][i] = !possibly_achievable( ggoal_state.F[j] );
      if ( gcmd_line.display_info == 126 &&
	   lm[j][i] ) {
	printf("\norderings: ");
	print_ft_name( ggoal_state.F[j] );
	printf(" <= ");
	print_ft_name( ggoal_state.F[i] );
      }
    }
    unsetup_E( ggoal_state.F[i] );
  }

}



void setup_E( int ft )

{

  int i, j;
  int ef, ef_, ft_;

  lnum_ch = 0;

  /* efs that imply a delete ef to ft
   */
  for ( i = 0; i < gft_conn[ft].num_D; i++ ) {
    ef = gft_conn[ft].D[i];
    if ( !lin_ch[ef] ) {
      lin[ef] = FALSE;
      lch[lnum_ch++] = ef;
      lin_ch[ef] = TRUE;
    }
    for ( j = 0; j < gef_conn[ef].num_I; j++ ) {
      ef_ = gef_conn[ef].I[j];
      if ( !lin_ch[ef_] ) {
	lin[ef_] = FALSE;
	lch[lnum_ch++] = ef_;
	lin_ch[ef_] = TRUE;
      }
    }
  }

  /* efs that use False preconds
   */
  for ( i = 0; i < gft_conn[ft].num_False; i++ ) {
    ft_ = gft_conn[ft].False[i];
    for ( j = 0; j < gft_conn[ft_].num_PC; j++ ) {
      ef = gft_conn[ft_].PC[j];
      if ( !lin_ch[ef] ) {
	lin[ef] = FALSE;
	lch[lnum_ch++] = ef;
	lin_ch[ef] = TRUE;
      }
    }
  }

}



void unsetup_E( int ft )

{

  int i;

  for ( i = 0; i < lnum_ch; i++ ) {
    lin[lch[i]] = TRUE;
    lin_ch[lch[i]] = FALSE;
  }

}



Bool possibly_achievable( int ft )

{

  int i, j, k;
  int ef, ft_;

  for ( i = 0; i < gft_conn[ft].num_A; i++ ) {
    ef = gft_conn[ft].A[i];
    if ( !lin[ef] ) {
      continue;
    }
    for ( j = 0; j < gef_conn[ef].num_PC; j++ ) {
      ft_ = gef_conn[ef].PC[j];
      for ( k = 0; k < gft_conn[ft_].num_A; k++ ) {
	if ( lin[gft_conn[ft_].A[k]] ) {
	  break;
	}
      }
      if ( k == gft_conn[ft_].num_A ) {
	break;
      }
    }
    if ( j < gef_conn[ef].num_PC ) {
      continue;
    }
    return TRUE;
  }

  return FALSE;

}









/* take a matrix of goal orderings and build it into
 * the goal agenda
 */












void build_goal_agenda( void )

{

  int i, j, k, n = ggoal_state.num_F, start, entry;
  int *degree;
  int *hits;
  int *slot;

  degree = ( int * ) calloc( n, sizeof( int ) );
  hits = ( int * ) calloc( n, sizeof( int ) );
  slot = ( int * ) calloc( n, sizeof( int ) );
  for ( i = 0; i < n; i++ ) {
    degree[i] = 0;
    hits[i] = 0;
  }

  /* compute transitive closure on orderings matrix
   */
  for ( j = 0; j < n; j++ ) {
    for ( i = 0; i < n; i++ ) {
      if ( lm[i][j] ) {
	for ( k = 0; k < n; k++ ) {
	  if ( lm[j][k] ) {
	    lm[i][k] = TRUE;
	  }
	}
      }
    }
  }
  
  /* count in - and outgoing edges, know those
   * goals that are not connected at all
   */
  for ( i = 0; i < n; i++ ) {
    for ( j = 0; j < n; j++ ) {
      if ( i != j && lm[i][j] ) {
	degree[i]--;
	degree[j]++;
	hits[i]++;
	hits[j]++;
      }
    }
  }

  /* order goals with increasing degree, disconnected
   * at the very end.
   */
  for ( i = 0; i < n; i++ ) {
    if ( hits[i] == 0 ) {
      slot[i] = i;
      continue;
    }
    for ( j = 0; j < i; j++ ) {
      if ( degree[i] < degree[slot[j]] ||
	   hits[slot[j]] == 0 ) {
	break;
      }
    }
    for ( k = i - 1; k >= j; k-- ) {
      slot[k+1] = slot[k];
    }
    slot[j] = i;
  }

  /* sweep over and collect goal agenda
   */
  ggoal_agenda = ( State * ) calloc( n, sizeof( State ) );
  for ( i = 0; i < n; i++ ) {
    make_state( &(ggoal_agenda[i]), gnum_ft_conn );
    ggoal_agenda[i].max_F = gnum_ft_conn;
  }

  start = 0;
  entry = 0;
  for ( i = 1; i < n; i++ ) {
    if ( ( degree[slot[i]] != degree[slot[i-1]] ) ||
	 ( hits[slot[i]] == 0 && hits[slot[i-1]] != 0 ) ) {
      ggoal_agenda[entry].num_F = 0;
      for ( j = start; j < i; j++ ) {
	ggoal_agenda[entry].F[ggoal_agenda[entry].num_F++] = 
	  ggoal_state.F[slot[j]];
      }
      entry++;
      start = i;
    }
  }
  ggoal_agenda[entry].num_F = 0;
  for ( i = start; i < n; i++ ) {
    ggoal_agenda[entry].F[ggoal_agenda[entry].num_F++] = 
      ggoal_state.F[slot[i]];
  }
  entry++;
  gnum_goal_agenda = entry;

  free( degree );
  free( hits );
  free( slot );

  if ( gcmd_line.display_info == 127 ) {
    printf("\ngoal agenda is:\n");
    for ( i = 0; i < gnum_goal_agenda; i++ ) {
      if ( i == 0 ) {
	printf("\nentry %3d: ", i);
      } else {
	printf("\n      %3d: ", i);
      }
      for ( j = 0; j < ggoal_agenda[i].num_F; j++ ) {
	print_ft_name( ggoal_agenda[i].F[j] );
	if ( j < ggoal_agenda[i].num_F - 1 ) {
	  printf("\n           ");
	}
      }
    }
  }

}


