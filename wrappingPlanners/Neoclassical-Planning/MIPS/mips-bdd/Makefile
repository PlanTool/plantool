
#This makefile was written for GNU make. It may or may not work
#with other make utilities.

#TOPDIR is the directory where MIPS-BDD is located. Set this to
#the directory that you've installed MIPS-BDD in.

TOPDIR = /home/jabbar/mips-bdd
####### FLAGS 

CC  =   g++ -O2 -I $(TOPDIR)/include -I$(TOPDIR)/include 
 LIBPATH = -L$(TOPDIR)/lib -L $(TOPDIR)/lib/galib246 -L $(TOPDIR)/galib246/ga
 INCLUDEPATH = -I/usr/include -I$(TOPDIR)/include/util -I$(TOPDIR)/include -I $(TOPDIR)/lib/galib246 -I$(TOPDIR)/include -I $(TOPDIR)/lib/galib246/ga

CFLAGS=  $(LIBPATH) $(INCLUDEPATH) 

#The libraries will be built using ar. If ar needs different flags
#insert them here.
# AR = ar -cr

##############################################################################
#Do not change anything below this line                                      #
############################################################################## 

MAKEFILE = Makefile

export TOPDIR CC AR CFLAGS

SUBDIRS = src

all: 
	set -e; for i in $(SUBDIRS); do $(MAKE) -C $$i -f $(MAKEFILE); done;
# $(MAKE) -C www -f $(MAKEFILE)

clean: 
	set -e; for i in $(SUBDIRS); do $(MAKE) -C $$i -f $(MAKEFILE) clean; done 

# backup:
#	rm mips.tgz; tar -zcf mips.tgz ~/results/Test3 src/*.cc include Makefile 

# libs: 
#	set -e; for i in $(SUBDIRS); do $(MAKE) -c $$i -f $(MAKEFILE) libs; done





