


/* (C) Copyright 1997 Albert Ludwigs University Freiburg
 *     Institute of Computer Science
 *
 * All rights reserved. Use of this software is permitted for 
 * non-commercial research purposes, and it may be copied only 
 * for that use.  All copies must include this copyright message.
 * This software is made available AS IS, and neither the authors
 * nor the  Albert Ludwigs University Freiburg make any warranty
 * about the software or its performance. 
 */


/* design by Frank Rittinger,
 * reimplementation to match IPP 4.0 data strucutures by Joerg Hoffmann
 */


/*
 * file: save_graph.c
 * functions for writing the graph created by the planer IP2 
 * to two external files (one for facts and one for the ops).
 * The format of the files is as follows:
 *
 * '^' stands for the separator defined by SEP
 * level^name^is_noop^is_used
 * PRE^name^name...                list of preconditions
 * EXC^name^name...                list of exclusive nodes in same layer
 * ADD^name^condition^condition... add-effect with conditions
 * ADD...                          there could be more add-edges 
 * DEL^name^condition...           the same as for add-edges
 */

/* #include <stdio.h> */
/* #include <stdlib.h> */
/* #include <string.h> */
#include"ipp.h"/* defines, data structures, fn prototypes, global variables */
#include "save_graph.h"


#define SEP        "^"         /* the separator for the strings */
#define SEA        "_"         /* the separator for the arguments */
#define LEVEL      5           /* size of level as string */
#define NOOP       "noop"      /* needed to find noops */
#define NOOP_LEN   4           /* length of noop string */






char *ltmp;
char *ltmp2;


   







Bool SaveGraph( char *filename, int max_op )

{

  FILE *fact_file, *ops_file;

  char temp[MAX_LENGTH];         /* for some temporary use must be carefull to check
                            * for bounds, but MAX_LENGTH should be sufficient
                            */
  int time, i, used;
  OpNode * op;
  Bool had;

  Candidate *c;
      

  /************ Facts ************/
  sprintf( temp, "%s.facts", filename );

  /* open file for writing the facts
   */
  if ((fact_file = fopen( temp, "w")) == NULL) {
    printf("Cannot open file %s.", temp);     
  }

  time = 0;
  while ( time < max_op+1 ) {
    had = FALSE;
    for ( i = 0; i < gnum_relevant_facts * 2; i++ ) {
      if ( gft_table[i] == NULL ) continue;
      if ( ftlevel( gft_table[i] ) <= time ) {
	/* print ft */
	fprintf(fact_file, "%d%s%s%s%d\n", 
		time, SEP, 
		fname(gft_table[i]), SEP, 0);
	WriteFtPreLine(gft_table[i], time, fact_file);
	WriteFtExcLine(gft_table[i], time, fact_file);
	WriteFtAddLines(gft_table[i], time, fact_file);
	had = TRUE;
      }
    }
    if ( !had && time == 0 ) {
    /* special treatment... */
      fprintf(fact_file, "0^ONLY_INERTIA^0\n");
      fprintf(fact_file, "PRE\n");
      fprintf(fact_file, "EXC\n");
      fprintf(fact_file, "ADD\n");
    }
    time++;
  }

  fclose(fact_file);



  /************* Operators **************/
  sprintf( temp, "%s.opers", filename );

  /* open file for writing the operators */
  if((ops_file = fopen( temp, "w" )) == NULL){
    printf("Cannot open file %s.", temp);
  }

  time = 0;
  while ( time < max_op ) {
    for ( op = gall_ops_pointer; op; op = op->next ) {
      if ( oplevel(op) <= time ) {
	break;
      }
    }
    for ( ; op; op = op->next ) {
      if( !op->is_noop ){
	if ( time <= LO_TIME(time) ) {
	  used = op->info_at[LO_TIME(time)]->is_used;
	} else {
	  c = gplan_start;
	  i = gfirst_full_time + 1;
	  while ( i < time ) {
	    c = c->father;
	    i++;
	  }
	  for ( i = 0; c->ops[i] != NULL; i++ ) {
	    if ( c->ops[i] == op ) {
	      break;
	    }
	  }
	  if ( c->ops[i] != NULL ) {
	    used = 1;
	  } else {
	    used = 0;
	  }
	}
	fprintf( ops_file , "%d%s%s%s%d\n", 
		 time, SEP, 
		 opname(op), SEP, used );
	WriteOpPreLine(op, ops_file);
	WriteOpExcLine(op, time, ops_file);
	WriteOpAddLines(op, ops_file);
      }
    }
    time++;
  }

  fclose(ops_file);

  return TRUE;

}










/* Write all the preconditions into one line */
Bool WriteOpPreLine(OpNode *op, FILE *fp)

{

  FtEdge *ft;

  fprintf( fp, "PRE");

  for ( ft=op->preconds; ft; ft=ft->next) {
    fprintf( fp,"%s%s", SEP, fname(ft->ft) );
  }
  fprintf( fp, "\n");

  return TRUE;

}



/* Write all exclusives to one line */
Bool WriteOpExcLine(OpNode * op, int level, FILE *fp )

{

  OpNode *o;

  fprintf( fp, "EXC" );
  for ( o=gall_ops_pointer; o; o=o->next ) {
    if ( o == op || o->is_noop ) continue;
    if ( op->info_at[LO_TIME(level)]->exclusives[o->uid_block] & o->uid_mask ) {
      fprintf( fp, "%s%s", SEP, opname(o) );
    }
  }
  fprintf( fp, "\n");

  return TRUE;

}




/* Write add-effect and its conditions to one line
 * and repeat as long as there are add-effects
 */ 
Bool WriteOpAddLines(OpNode *op, FILE *fp)

{

  FtEdge * ft,*ft2;
  EfNode * ef; 

  for (ft=op->unconditional->effects; ft; ft = ft->next) {
    fprintf( fp, "ADD%s%s\n", SEP,fname(ft->ft) );
  }
  for (ef=op->conditionals; ef; ef = ef->next) {
    for (ft=ef->effects; ft; ft=ft->next) {
      fprintf( fp, "ADD%s%s", SEP,fname(ft->ft) );/* ganze liste? */
      for (ft2=ef->conditions; ft2; ft2=ft2->next) {
	fprintf( fp, "%s%s", SEP,fname(ft2->ft));/* reihenflg */
      }
      fprintf( fp, "\n");
    }
  }

  return TRUE;

}















/* Write all the preconditions into one line 
 */
Bool WriteFtPreLine( FtNode *ft, int level, FILE *fp ) 

{

  OpEdge *op;

  fprintf( fp, "PRE");

  for ( op=ft->preconds; op; op=op->next) {
    if ( oplevel( op->op ) <= level ) {
      fprintf( fp,"%s%s", SEP, opname(op->op) );
    }
  }
  fprintf( fp, "\n");

  return TRUE;

}



/* Write all exclusives to one line 
 */
Bool WriteFtExcLine( FtNode *ft, int level, FILE *fp )

{

  int i;
  FtNode *ft_;

  fprintf( fp, "EXC" );
  for ( i = 0; i < gnum_relevant_facts * 2; i++ ) {
    ft_ = gft_table[i];
    if ( ft->index == ft_->index ) {
      if ( ft->positive != ft_->positive ) {
	fprintf( fp, "%s%s", SEP, fname(ft_) );
      }
      continue;
    }
    if ( ft_->positive ) {
      if ( ft->info_at[LO_TIME(level)]->pos_exclusives[ft_->uid_block] & ft_->uid_mask ) {
	fprintf( fp, "%s%s", SEP, fname(ft_) );
      }
    } else {
      if ( ft->info_at[LO_TIME(level)]->neg_exclusives[ft_->uid_block] & ft_->uid_mask ) {
	fprintf( fp, "%s%s", SEP, fname(ft_) );
      }
    }
  }
  fprintf( fp, "\n");

  return TRUE;

}



Bool WriteFtAddLines( FtNode *ft, int level, FILE *fp )

{

  FtEdge *e;
  EfEdge *ef; 

  for ( ef = ft->info_at[LO_TIME(level)]->adders_pointer; ef; ef = ef->next ) {
    fprintf( fp, "ADD%s%s", SEP, opname(ef->ef->op) );
    for ( e = ef->ef->conditions; e; e = e->next ) {
      fprintf( fp, "%s%s", SEP, fname(e->ft));
    }
    fprintf( fp, "\n");
  }

  return TRUE;

}





















int oplevel( OpNode *op )

{

  int i;

  for (i=0;i<MAX_PLAN;i++) {
    if ( op->info_at[i] ) break;
  }

  return i;

}



int ftlevel( FtNode *ft )

{

  int i;

  for (i=0;i<MAX_PLAN;i++) {
    if ( ft->info_at[i] ) break;
  }

  return i;

}



char *opname( OpNode *op )

{

  int i;

  ltmp = ( char * ) calloc( MAX_LENGTH, sizeof( char) );
  ltmp2 = ( char * ) calloc( MAX_LENGTH, sizeof( char) );
  
  sprintf(ltmp, "%s", op->name);

  for ( i = 0; i < op->num_vars; i++ ) {
    sprintf(ltmp2, "%s%s%s", ltmp, SEA, gconstants_table[op->inst_table[i]]);
    strcpy(ltmp, ltmp2);
  }

  free(ltmp2);

  return ltmp;

}



char *fname( FtNode *ft )

{

  int j;
  RelevantFact_pointer f=grelevant_facts[ft->index];

  ltmp = ( char * ) calloc( MAX_LENGTH, sizeof( char) );
  ltmp2 = ( char * ) calloc( MAX_LENGTH, sizeof( char) );

  if ( ft->positive ) {
    sprintf(ltmp, "%s", gpredicates_table[f->predicate]);
  } else {
    sprintf(ltmp, "!%s", gpredicates_table[f->predicate]);
  }

  for ( j = 0; j < garity[f->predicate]; j++ ) {
    sprintf(ltmp2, "%s%s%s", ltmp, SEA, gconstants_table[f->arguments[j]]);
    strcpy(ltmp, ltmp2);
  }

  free(ltmp2);

  return ltmp;

}



int LO_TIME( int time )

{

  if ( !gsame_as_prev_flag ) {
    return time;
  }

  if ( time < gfirst_full_time ) {
    return time;
  }

  return gfirst_full_time;

}

