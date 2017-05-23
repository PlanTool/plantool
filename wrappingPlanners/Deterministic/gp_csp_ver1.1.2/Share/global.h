
/*
 *  CPlan solves planning problems formulated as constraint satisfaction
 *  problems.
 *
 *  Copyright (C) 1999  Peter van Beek and Xinguang Chen
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation.  See the GNU General Public License
 *  for more details (see the file called Copying or contact the Free
 *  Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA).
 */

#include <stdio.h>
#include <stdlib.h>

/*
 * Used whenever a value is needed that is bigger than
 * any integer value that could reasonably appear while
 * solving a planning problem.
 */
#define HUGE	99999
#define DEFAULT_CUTOFF_LIMIT 1000000000

/*
 * External declarations for cross-file access to global variables.
 */
extern double   solution_count;
extern double   checks;
extern double   visits;

/*
 * Convenient type definition.
 */
typedef int BOOL;

/*
 *  Other function prototypes for cross-file compilation.
 */
void      set_limit(double);
int       limit_exceeded();
void	  start_timer();
double	  elapsed_time();
int       *new_calloc();

