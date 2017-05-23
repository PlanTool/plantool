


/*********************************************************************
 * File: wave_front.h
 * Description: headers for wave front functions...
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





#ifndef _WAVE_FRONT_H
#define _WAVE_FRONT_H






int search_wave_front( void );
Bool expand_candidate( Candidate *current );

void add_candidate( int time );


#endif /* _WAVE_FRONT_H */
