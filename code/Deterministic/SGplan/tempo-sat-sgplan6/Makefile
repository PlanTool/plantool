
 # (C) Copyright 2008, University of Illinois, Urbana-Champaign

 #

 # All rights reserved. Use of this software is permitted ONLY for

 # non-commercial research purposes, and it may be copied only

 # for that use only. All copies must include this copyright message.

 # This software is made available AS IS, and neither the authors

 # nor the University of Illinois, make any warranty about the

 # software or its performance.

 #


 # File: Makefile

 # Authors:  Chih-wei Hsu and Benjamin W. Wah

 #

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

#################################
# Modified from Makefile of LPG #
#################################


####### FLAGS
#TARGET = sgplan
TARGET = _sgplan.so
#CFLAGS = -g -Wall
#CFLAGS	= -O3 -DNDEBUG -Wall
CFLAGS = -O2 -fPIC
#LIBS	= -lm -lnsl -lsocket
LIBS	= -lm

####### COMPILE

CC	= gcc

PATH_HEADERS	= include


SOURCES = main.c \

OBJECTS = $(SOURCES:.c=.o) 

LPG_LIBS = LocalSearch/LocalSearch.a Utilities/Utilities.a HillClimbingSearch/HillClimbingSearch.a Parser/Parser.a Metric-FF/mff.a ./libmetis.so

SUB_DIRS = LocalSearch Utilities HillClimbingSearch Parser Metric-FF

find_header = $(wildcard $(dir)/*.h)

HEADERS := $(foreach dir,./include,$(find_header))

####### Implicit rules

.SUFFIXES:

.SUFFIXES: .c .o

.c.o:; $(CC) -Dmain=run -I$(PATH_HEADERS) -c $(CFLAGS) $<


####### Build rules

sgplan:	islpg $(OBJECTS) COMPILE_SUB sgplan_wrap.o
#	$(CC) -shared -o $(TARGET) $(OBJECTS) $(LPG_LIBS) $(LIBS)
	$(CC) -shared -o $(TARGET) $(OBJECTS) sgplan_wrap.o $(LPG_LIBS) $(LIBS) -I/home/henrysky/anaconda3/envs/py2/include/python2.7

islpg:	
#	rm -f LocalSearch/LocalSearch.a
#	rm -f Utilities/Utilities.a

COMPILE_SUB:
	@ for i in $(SUB_DIRS); do $(MAKE) -C $$i CFLAGS='$(CFLAGS)'; done

$(OBJECTS): $(HEADERS)

sgplan_wrap.o: sgplan_wrap.c
	gcc -O2 -fPIC -c sgplan_wrap.c -I/home/henrysky/anaconda3/envs/py2/include/python2.7

sgplan_wrap.c:
	swig -python -o sgplan_wrap.c main.i

clean:
	@ for i in $(SUB_DIRS); do $(MAKE) -C $$i clean; done
	-rm -f *.o *~ sgplan* $(PATH_HEADERS)/*~ 

distclean:
	@ for i in $(SUB_DIRS); do $(MAKE) -C $$i clean; done
	-rm -f *.o *~ sgplan* $(PATH_HEADERS)/*~
#######
