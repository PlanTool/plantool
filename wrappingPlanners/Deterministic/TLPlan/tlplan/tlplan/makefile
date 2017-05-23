OBJS = adl.o btree.o cc.o color.o compute.o \
	crchash.o current.o domain.o eval.o eval1.o \
	formula.o gate.o hash.o heap.o idle.o \
	iface.o internal.o interval.o isaac.o list.o \
	makeform.o makegen.o msort.o oper.o lexpddl.o plan.o \
	progress.o queue.o save.o search.o strips.o \
	tl_tab.o tllex.o tlparse.o user.o util.o \
	var.o world.o zone.o

tlplan: tlplan.o tllib
	gcc -lm -o tlplan tlplan.o /home/fbacchus/Tlplan/tllib

tllib: $(OBJS)
	gcc -shared -ldl -lm -o tllib $(OBJS)

tl_tab.c:	tl.y
	bison -lvd tl.y
	mv tl.tab.c tl_tab.c
	mv tl.tab.h tl_tab.h

tllex.c:	tllex.lxi
	flex -8 -LI tllex.lxi
	mv lex.yy.c tllex.c

lexpddl.c:	lexpddl.lxi
	flex -8 -LI lexpddl.lxi
	mv lex.pddl.c lexpddl.c

.c.o:
	gcc -c -Wall -O6 -g $(CFLAGS) $<

tl_tab.o:	tl_tab.c
	gcc -c -Wall -O6 -g $(CFLAGS) tl_tab.c

tllex.o:	tllex.c
	gcc -c -Wall -O6 -g $(CFLAGS) tllex.c

lexpddl.o:	lexpddl.c
	gcc -c -Wall -O6 -g $(CFLAGS) lexpddl.c

clean:
	rm -f $(OBJS) 
