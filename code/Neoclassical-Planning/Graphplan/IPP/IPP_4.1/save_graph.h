


/*********************************************************************
 * File: save_graph.h
 * Description: write graph to output files
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




#ifndef _SAVE_GRAPH_H
#define _SAVE_GRAPH_H



Bool SaveGraph( char *filename, int max_op );



Bool WriteOpPreLine(OpNode * op, FILE *fp);
Bool WriteOpExcLine(OpNode * op, int level, FILE *fp);
Bool WriteOpAddLines(OpNode * op, FILE *fp);



Bool WriteFtPreLine( FtNode *ft, int level, FILE *fp );
Bool WriteFtExcLine( FtNode *ft, int level, FILE *fp );
Bool WriteFtAddLines( FtNode *ft, int level, FILE *fp );



int oplevel( OpNode *op );
int ftlevel( FtNode *ft );
char *opname( OpNode *op );
char *fname( FtNode *ft );
int LO_TIME( int time );



#endif /* _SAVE_GRAPH_H */
