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

 * File: interProcess.c

 * Authors:  Chih-wei Hsu and Benjamin W. Wah

 *

 *********************************************************************/
/*
# Copyright (C) 2004, Board of Trustees of the University of Illinois.
#
# The program is copyrighted by the University of Illinois, and should
# not be distributed without prior approval.  Commercialization of this 
# product requires prior licensing from the University of Illinois.
# Commercialization includes the integration of this code in part or 
# whole into a product for resale. 
#
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# Author: Y. X. Chen, C. W. Hsu, and B. W. Wah, University of Illinois
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
*/

/********************************************************************
 * File: esp.c
 * Description: Extended Saddle Point Search method
 *                                      
 * Author: Yixin Chen
 *
 *********************************************************************/


#include <values.h>
#include <math.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>

#include "lpg.h"
#include "LpgTime.h"
#include "check.h"
#include "numeric.h"
#include "ActionSubgraph.h"
#include "H_relaxed.h"
#include "H_max.h"
#include "utilities.h"
#include "LpgOutput.h"
#include "output.h"
#include "esp.h" 
#include "ff.h"
#include "orderings.h"
#include "subspace.h"
#include "interProcess.h"

#define SNAME "espIPC"
#define LINELEN 1024
int sock, msgsock, rval;

#define MIPSbin "/home/manip1/chen/ESP+LPG+MIPS/mips/mips"

void serverMips()
{
   struct sockaddr_un server;
   char buf[LINELEN];
   
   
   /* Create socket */
   sock = socket(AF_UNIX, SOCK_STREAM, 0);
   if( sock < 0 ) {
     perror("opening stream socket");
     exit(1);
   } 
   
   /* Name sockets using file system name */
   server.sun_family = AF_UNIX;
   strcpy(server.sun_path, SNAME);

   system("rm -f espIPC");
   
   if(bind(sock, (struct sockaddr *)&server, sizeof(struct
      sockaddr_un))<0) { 
     perror("binding");
     exit(1);
   } 
   
   printf("Socket has name %s\n", server.sun_path);
   
   /* Start accepting connections */
   listen(sock,5);
  
   /* start up Mips client */
   strcpy(buf, MIPSbin);
   strcat(buf, " ");
   strcat(buf, gcmd_line.ops_file_name);
   strcat(buf, " ");   
   strcat(buf, gcmd_line.fct_file_name);
   //printf("Command: %s\n", buf);
  
   if(fork()==0) {
        system(buf);
        exit(0);
   }
   
   /* accept connection */
   msgsock = accept(sock, (struct sockaddr *)NULL, (int *)NULL);
   
   /* sending out numeric info */
   sendNumericInfo();
}

void sendNumericInfo()
{
	char s1[LINELEN],s2[20];
	int i;
	(void) write( msgsock, &gnum_fullnum_initial, 
		      sizeof(gnum_fullnum_initial) );
	printf("gnum_fullnum_initial = %d\n", gnum_fullnum_initial);
	for (i = 0; i < gnum_fullnum_initial; i++) {
		NumVar * f = gfullnum_initial[i];
  		int j = 0;
	        if (f == NULL) continue;
  		if (f->function == -3) continue;
	        if (f->function == -1) continue;
  		if (f->function == -2) continue;
 		sprintf (s1, "%s", gfunctions[f->function]);
  		for (j = 0; j < gfunarity[f->function]; j++)
    		{
	  		strcat (s1, " ");
      			if (f->args[j] >= 0)
			{
	  			strcat (s1, gconstants[(f->args)[j]]);
			}			
    	  		else
			{
	  			sprintf (s2, "x%d", DECODE_VAR (f->args[j]));
				strcat(s1, s2);
			}	
    		}
    		//printf ("%s\n",s1);
		(void) write( msgsock, s1, sizeof(s1)); 		
	}

	read( msgsock, &gnum_mipsnum, sizeof(gnum_mipsnum));
	for(i=0; i<gnum_mipsnum; i++) {
		read( msgsock, &(gmipsnum[i]), sizeof(int));
		//printf("%d %d\n", i, gmipsnum[i]);
	}	
}						

void sendState(State *S)
{
   	char buf[LINELEN];
	int i;
        float v;
	(void) write( msgsock, &(S->num_F), sizeof(S->num_F));
	//printf("numF = %d\n", S->num_F);
	for(i=0; i<S->num_F; i++) {
		print_ft_name_string ( S->F[i], buf );
		//printf("%s\n", buf);
		(void) write( msgsock, buf, sizeof(buf)); 		
	}
	for(i=0; i<gnum_mipsnum; i++) {
		v = S->V[gmipsnum[i]];
		write( msgsock, &v, sizeof(v));
		//printf("numeric %d : index %d value %f\n", i, gmipsnum[i], v);
	}			
}

void shutdownServer()
{
     close(msgsock);   
     close(sock);
     unlink(SNAME);
}

int factID_mips_lpg(char *s)
{
    char buf[LINELEN];
    int i;
    for(i=0; i<gnum_ft_conn; i++) {
        print_ft_name_string ( i, buf );
        if(!strcasecmp(s, buf)) {
            return i;
        }
    }       
    return -1;
}

void recv_Mips_state(State *result)
{
    int fsize, i, fid;
    float v;
    char buf[LINELEN];
    read( msgsock, &fsize, sizeof(fsize));
    //printf("fSize = %d\n", fsize);
    for (i=0;i<fsize; i++)  { 
        read( msgsock, buf, LINELEN);
        buf[LINELEN-1] = '\0';        
        fid = factID_mips_lpg(buf);
        result->F[i] = fid;
        //printf("recvd mips state %s %d\n", buf, fid);
    }          
    result->num_F = fsize;
    
    
	for(i=0; i<gnum_mipsnum; i++) {
		//v = S->V[gmipsnum[i]];
		read( msgsock, &v, sizeof(v));
		//printf("recvd mips state numeric %d : index %d value %f\n", i, gmipsnum[i], v);
        result->V[gmipsnum[i]] = v;
    }			 
}           

int call_Mips_service(State *start, State *goal, State *result)
{
    int sig, res;
    sig = 1;
    write( msgsock, &sig, sizeof(sig));
    
    sendState(start);
    sendState(goal);
    
    read( msgsock, &res, sizeof(res));
    if(res > 0) {
        recv_Mips_state(result);
    }

    return res; 
}


void calloff_Mips_service()
{
    int sig;        
    sig = -1;
    write( msgsock, &sig, sizeof(sig));
}       
