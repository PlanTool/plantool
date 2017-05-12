SUBDIRS = parser planner utils


#  compile

compile:
			@echo "*** Compiling all in `pwd`"
			@for dir in $(SUBDIRS); do \
			  (cd $$dir ; make compile) \
			done


#  Cleanning


clean:
			@echo "*** Cleanning `pwd`"
			@for dir in $(SUBDIRS); do \
			  (cd $$dir ; make clean) \
			done


# Fixed

.SILENT:

