
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
#include <sys/time.h>
#include <sys/resource.h>

static struct rusage res;
static double virtual_time;

/*
 *  Start the stopwatch.
 */
void
start_timer()
{
    getrusage( RUSAGE_SELF, &res );
    virtual_time = (double) res.ru_utime.tv_sec +
                   (double) res.ru_stime.tv_sec +
                   (double) res.ru_utime.tv_usec / 1000000.0 +
                   (double) res.ru_stime.tv_usec / 1000000.0;
}

/*
 *  Stop the stopwatch and return the CPU time used in seconds.
 */
double
elapsed_time()
{
    getrusage( RUSAGE_SELF, &res );
    return( (double) res.ru_utime.tv_sec +
            (double) res.ru_stime.tv_sec +
            (double) res.ru_utime.tv_usec / 1000000.0 +
            (double) res.ru_stime.tv_usec / 1000000.0
            - virtual_time );
}

