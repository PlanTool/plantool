SUBDIRS = aips98 blocks-world blocks-world2 briefcase ferry gripper hanoi logistics \
	  logistics2 meet-pass monkey morris movie rockets tire-world tire-world2 travel


#  compile

compile:
			@echo "*** Compiling all in `pwd`"
			@for dir in $(SUBDIRS); do \
			  (cd $$dir ; make compile) \
			done


#  Solving

solve:
			echo "*** Solving all in `pwd`"
			@for dir in $(SUBDIRS); do \
			  (cd $$dir ; make solve SEED=$(SEED)) \
			done


#  Cleanning


clean:
			@echo "*** Cleanning `pwd`"
			@for dir in $(SUBDIRS); do \
			  (cd $$dir ; make clean) \
			done

clean-solve:
			@echo "*** Cleanning solutions in `pwd`"
			@for dir in $(SUBDIRS); do \
			  (cd $$dir ; make clean-solve) \
			done

clean-problems:
			@echo "*** Cleanning problems in `pwd`"
			@for dir in $(SUBDIRS); do \
			  (cd $$dir ; make clean-problems) \
			done

clean-all:		clean clean-solve clean-problems


# Fixed

.SILENT:

