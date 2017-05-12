
/*********************************************************************
 * (C) Copyright 1999 Albert Ludwigs University Freiburg
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
 * File: cnfout.h
 * Description: headers for cnf output
 *
 * Author: Joerg Hoffmann 2003
 *
 *********************************************************************/ 

#ifndef _CNFOUT_H
#define _CNFOUT_H


#define MAX_CLAUSES 64000000
#define MAX_CNF_VARS 1000000


void do_cnf_output( int );



void print_action_based_encoding( int layer, int toCreate );
void print_gpstyle_action_based_encoding( int layer, int toCreate );
void print_gp_based_encoding( int layer, int toCreate );
void print_thin_gp_based_encoding( int layer, int toCreate );
#endif /* _CNFOUT_H */
