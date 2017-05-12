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

 * File: dis_producible.h

 * Authors:  Chih-wei Hsu and Benjamin W. Wah

 *

 *********************************************************************/
#ifndef _PRODUCIBLE_H
#define _PRODUCIBLE_H

extern int num_level_producible, producible_solution_length, *producible_solution;
extern short *is_producible_fact, *is_producible_fluent, *is_producible_effect;
extern unsigned char *is_producible_operator;
extern void relax_producible_variables(dis_State *, dis_State *);
extern void detect_producible_variables();
extern int producible_main();
extern int psearch();

#endif 
