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

 * File: interProcess.h

 * Authors:  Chih-wei Hsu and Benjamin W. Wah

 *

 *********************************************************************/



#ifndef _INTERPROCESS_H
#define _INTERPROCESS_H

void serverMips();
void shutdownServer();
void sendState(State *S);
void sendNumericInfo();
int call_Mips_service(State *start, State *goal, State *result);
void calloff_Mips_service();


#endif /* _INTERPROCESS_H */
