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

 * File: ordering.c

 * Description: modified from ordering.c in LPG

 *

 * Authors:  Chih-wei Hsu and Benjamin W. Wah

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
#include "lpg.h"

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

int foo;







/* main function
 */

void print_ft_ef_mutex()
{
  int i, j, n = ggoal_state.num_F, ft;
  for(j=0; j<n; j++) {
    ft = ggoal_state.F[j];
    print_ft_name(ft);
    printf(" %d\n", ft);
    for(i=0; i<gnum_ef_conn; i++) {
        if(GET_BIT (FT_EF_mutex[ft], i)) {
            printf("    ");
            print_op_name(i);
            printf(" %d\n", i);
        }
    }                           
  }
}

Bool uiuc_possibly_achievable(int fo, int ft )
{   

  int i, j, k, pc,jj;
  int ef, ft_;

  for ( i = 0; i < gft_conn[ft].num_A; i++ ) {
    
    ef = gft_conn[ft].A[i];
    
    if((ft == 53) && (fo==64)) {
        printf("\n");
        print_op_name(ef);
        printf(" %d\n",ef);
    }           
  
    
    if ( GET_BIT (FT_EF_mutex[fo], ef) ) {
      continue;
    }
   
    pc = gef_conn[ef].num_PC;
    if(gef_conn[ef].sf) pc = pc+gef_conn[ef].sf->num_PC_overall + gef_conn[ef].sf->num_PC_end;                      
    for (j=0; j<pc; j++) { 
    //for ( j = 0; j < gef_conn[ef].num_PC; j++ ) {
      if(j< gef_conn[ef].num_PC)
        ft_ = gef_conn[ef].PC[j];
      else {
         if(j<gef_conn[ef].num_PC + gef_conn[ef].sf->num_PC_overall) {
            jj = j - gef_conn[ef].num_PC;
             ft_ = gef_conn[ef].sf->PC_overall[jj]; 
         } else {
            jj = j - gef_conn[ef].num_PC -  gef_conn[ef].sf->num_PC_overall;
            ft_ = gef_conn[ef].sf->PC_end[jj];
         }
      }
      
      if(ft_ < 0) continue;    // avoid seg fault! Added by Y. Chen of UIUC,USA

      for ( k = 0; k < gft_conn[ft_].num_A; k++ ) {
    	if (!GET_BIT (FT_EF_mutex[fo], gft_conn[ft_].A[k]) ) {
	      break;
    	}
      }
      if ( k == gft_conn[ft_].num_A ) {
	    break;
      }
    }
    
    if ( j < pc ) {
      continue;
    }
    
    return TRUE;
  }

  return FALSE;

}


void uiuc_detect_ordering_constraints()
{
  int i, j, n = ggoal_state.num_F;

  print_ft_ef_mutex();
  
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

  for ( i = 0; i < n ; i++ ) {
    for ( j = 0; j < n; j++ ) {
      if(j!=i) {
        lm[j][i] = !uiuc_possibly_achievable(ggoal_state.F[i], ggoal_state.F[j] );
        /*
        if ( lm[j][i] ) {
    	    printf("\norderings: %d ", ggoal_state.F[j]);
	        print_ft_name( ggoal_state.F[j] );
    	    printf(" <= %d ",  ggoal_state.F[i] );
	        print_ft_name( ggoal_state.F[i] );
        }*/
      }
    }
  }
}





void compute_goal_agenda( void )

{

  int i;
  int max = gnum_ef_conn > gnum_ft_conn ? 
    gnum_ef_conn : gnum_ft_conn;
 

 /* 
  uiuc_detect_ordering_constraints();
  build_goal_agenda();
  return;
  */
  
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
    //build_False_set( ggoal_state.F[i] );
    mutex_build_False_set( ggoal_state.F[i] );
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

  //if ( gcmd_line.display_info == 125 ) {
    printf("\n\ncomputed False set of ");
    print_ft_name( ft );
    printf(" as follows:");
    for ( i = 0; i < gft_conn[ft].num_False; i++ ) {
      printf("\n");
      print_ft_name( gft_conn[ft].False[i] );
    }
 // }

 
}
 


// added by Y. Chen, UIUC, USA 
void mutex_build_False_set( int ft )
{

  int i;
  
  gft_conn[ft].False = ( int * ) calloc( gnum_ft_conn, sizeof( int ) );
  gft_conn[ft].num_False = 0;
  
    for ( i = 0; i < gnum_ft_conn; i++ ) {
      	if(GET_BIT (FT_FT_mutex[ft], i)) {  
		gft_conn[ft].False[gft_conn[ft].num_False++] = i;
	}							
    }

    /*
    printf("\n\ncomputed False set of ");
    print_ft_name( ft );
    printf(" as follows:");
    for ( i = 0; i < gft_conn[ft].num_False; i++ ) {
      printf("\n");
      print_ft_name( gft_conn[ft].False[i] );
    }*/
}





/* look at pairs of goals and see if they are ordered
 * heuristically reasonable
 */


Bool GAM_order(int f1, int f2)
{
    Bool lm; 
    setup_E( f1 );
    lm = !possibly_achievable( f2 );
    if ( lm ) {
    	printf("\nGAM orderings: ");
	    print_ft_name( f2 );
	    printf(" <= ");
	    print_ft_name( f1 );
    }
    unsetup_E( f1 );
    return lm;
}



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
    mutex_setup_E( ggoal_state.F[i] );
    for ( j = i + 1; j < n; j++ ) {
      lm[j][i] = !possibly_achievable( ggoal_state.F[j] );
      //if ( gcmd_line.display_info == 126 && lm[j][i] ) {
    /*
      if ( lm[j][i] ) {
	printf("\norderings: %d ", ggoal_state.F[j]);
	print_ft_name( ggoal_state.F[j] );
	printf(" <= %d ",  ggoal_state.F[i] );
	print_ft_name( ggoal_state.F[i] );
      }*/
    }
    unsetup_E( ggoal_state.F[i] );
  }
  for ( i = n - 1; i > 0; i-- ) {
    mutex_setup_E( ggoal_state.F[i] );
    for ( j = i - 1; j > -1; j-- ) {
      lm[j][i] = !possibly_achievable( ggoal_state.F[j] );
      //if ( gcmd_line.display_info == 126 && lm[j][i] ) {
     /*
      if ( lm[j][i] ) {
	printf("\norderings: %d ", ggoal_state.F[j]);
	print_ft_name( ggoal_state.F[j] );
	printf(" <= %d ",  ggoal_state.F[i] );
	print_ft_name( ggoal_state.F[i] );
      }*/             
    }
    unsetup_E( ggoal_state.F[i] );
  }

}




void mutex_setup_E( int ft )

{
  int ef;

  lnum_ch = 0;
  
  for ( ef = 0; ef<gnum_ef_conn; ef++ ) {
   if(GET_BIT (FT_EF_mutex[ft], ef))   { 
    if ( !lin_ch[ef] ) {
      lin[ef] = FALSE;
      lch[lnum_ch++] = ef;
      lin_ch[ef] = TRUE;
    }
   }
  }

}


void setup_E( int ft )

{

  int i, j, k;
  int ef, ef_, ft_;

  lnum_ch = 0;
  foo = ft;
  
  
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
      
      // Y. Chen of UIUC
      for( k=0; k<gft_conn[ft].num_A; k++) 
          if(ef == gft_conn[ft].A[k]) break;
      
      if(k < gft_conn[ft].num_A) continue;
      
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


Bool mutex_fact_achievable(int mf, int ft )
{                   

  int i, j, k, pc,jj;
  int ef, ft_;

  for ( i = 0; i < gft_conn[ft].num_A; i++ ) {
    ef = gft_conn[ft].A[i];
    if(GET_BIT (FT_EF_mutex[mf], ef))   { 
      continue;
    }
   
    
    pc = gef_conn[ef].num_PC;
    if(gef_conn[ef].sf) pc = pc+gef_conn[ef].sf->num_PC_overall + gef_conn[ef].sf->num_PC_end;                      
     
    for (j=0; j<pc; j++) { 
    //for ( j = 0; j < gef_conn[ef].num_PC; j++ ) {
      if(j< gef_conn[ef].num_PC)
        ft_ = gef_conn[ef].PC[j];
      else {
         if(j<gef_conn[ef].num_PC + gef_conn[ef].sf->num_PC_overall) {
            jj = j - gef_conn[ef].num_PC;
             ft_ = gef_conn[ef].sf->PC_overall[jj]; 
         } else {
            jj = j - gef_conn[ef].num_PC -  gef_conn[ef].sf->num_PC_overall;
            ft_ = gef_conn[ef].sf->PC_end[jj];
         }
      }
     
      
      if(ft_ < 0) continue;    // avoid seg fault! Added by Y. Chen of UIUC,USA

      for ( k = 0; k < gft_conn[ft_].num_A; k++ ) {
           //printf("     %d ", gft_conn[ft_].A[k]);print_op_name(gft_conn[ft_].A[k]);printf("\n");
	        if ( !GET_BIT (FT_EF_mutex[mf], gft_conn[ft_].A[k]) ) {
	            break;
	        }
      }
      if ( k == gft_conn[ft_].num_A ) {
        	break;
      }
    }
    
    if ( j < pc ) {
      continue;
    }
    
    return TRUE;
  }

  return FALSE;

}



Bool possibly_achievable( int ft )

{

  int i, j, k, pc,jj;
  int ef, ft_;

  for ( i = 0; i < gft_conn[ft].num_A; i++ ) {
    ef = gft_conn[ft].A[i];
 
    //printf("%d %d ",ft, ef);print_op_name(ef);printf("\n");
    
    if ( !lin[ef] ) {
      continue;
    }
   
    
    pc = gef_conn[ef].num_PC;
    if(gef_conn[ef].sf) pc = pc+gef_conn[ef].sf->num_PC_overall + gef_conn[ef].sf->num_PC_end;                      
     
    for (j=0; j<pc; j++) { 
    //for ( j = 0; j < gef_conn[ef].num_PC; j++ ) {
      if(j< gef_conn[ef].num_PC)
        ft_ = gef_conn[ef].PC[j];
      else {
         if(j<gef_conn[ef].num_PC + gef_conn[ef].sf->num_PC_overall) {
            jj = j - gef_conn[ef].num_PC;
             ft_ = gef_conn[ef].sf->PC_overall[jj]; 
         } else {
            jj = j - gef_conn[ef].num_PC -  gef_conn[ef].sf->num_PC_overall;
            ft_ = gef_conn[ef].sf->PC_end[jj];
         }
      }
     
     // printf("precond %d ",ft_); print_ft_name(ft_); printf("\n");
      
      if(ft_ < 0) continue;    // avoid seg fault! Added by Y. Chen of UIUC,USA

      for ( k = 0; k < gft_conn[ft_].num_A; k++ ) {
           //printf("     %d ", gft_conn[ft_].A[k]);print_op_name(gft_conn[ft_].A[k]);printf("\n");
	        if ( lin[gft_conn[ft_].A[k]] ) {
	            break;
	        }
      }
      if ( k == gft_conn[ft_].num_A ) {
        	break;
      }
    }
    
    if ( j < pc ) {
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

  /*
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
  }*/

}


