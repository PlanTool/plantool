
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

#include "global.h"

static double max_consistency_checks;

/*
 *  Set a limit on the maximum number of consistency checks.
 */
void
set_limit( cc_limit )
    double cc_limit;
{
    max_consistency_checks = cc_limit;
}

/*
 *  Answer whether a preset limit has been exceeded.
 */
int
limit_exceeded()
{
    return( checks > max_consistency_checks );
}

