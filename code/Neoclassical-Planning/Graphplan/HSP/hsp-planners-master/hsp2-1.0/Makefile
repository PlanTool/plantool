SUBDIRS = planner


#  Compile

compile:
			@echo "*** Compiling all in `pwd`"
			@for dir in $(SUBDIRS); do \
			  (cd $$dir ; make compile) \
			done


#  Cleaning


clean:
			@echo "*** Cleaning `pwd`"
			@for dir in $(SUBDIRS); do \
			  (cd $$dir ; make clean) \
			done


# Fixed

.SILENT:

