#FLAGS= -m32 -O6 -g -Wall
FLAGS=  -g -Wall -fPIC

OBJS = adl.o btree.o cc.o color.o compute.o \
	crchash.o current.o domain.o eval.o eval1.o \
	formula.o gate.o hash.o hashrelax.o hbtree.o heap.o idle.o \
	iface.o internal.o interval.o isaac.o list.o \
	makeform.o makegen.o msort.o oper.o lexpddl.o plan.o \
	progress.o queue.o save.o search.o strips.o strhash.o \
	tl_tab.o tllex.o tlparse.o user.o util.o \
	var.o world.o zone.o

tlplan: tlplan.o libtl.so
	gcc $(FLAGS) -lm -ldl -o tlplan tlplan.o $(OBJS)

libtl.so: $(OBJS)
	gcc $(FLAGS) -shared -ldl -lm -o libtl.so $(OBJS)

tl_tab.c:	tl.y
	bison -lvd tl.y
	mv tl.tab.c tl_tab.c
	mv tl.tab.h tl_tab.h

tllex.c:	tllex.lxi
	flex -Ptl tllex.lxi
	mv lex.tl.c tllex.c

lexpddl.c:	lexpddl.lxi
	flex -Ppddl lexpddl.lxi
	mv lex.pddl.c lexpddl.c

.c.o:
	gcc $(FLAGS) -c -Wall $(CFLAGS) $<

tl_tab.o:	tl_tab.c
	gcc $(FLAGS) -c -Wall  $(CFLAGS) tl_tab.c

tllex.o:	tllex.c
	gcc $(FLAGS) -c -Wall  $(CFLAGS) tllex.c

lexpddl.o:	lexpddl.c
	gcc $(FLAGS) -c -Wall  $(CFLAGS) lexpddl.c

clean:
	rm -f $(OBJS) tlplan.o
