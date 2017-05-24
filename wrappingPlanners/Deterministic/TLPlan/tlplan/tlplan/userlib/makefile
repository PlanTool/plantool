OBJS = blocks.o bblocks.o cdf.o dcdflib.o freecell.o ipmpar.o jobshop.o lloyd.o rubik.o schedule.o slaney.o taxi.o tyre.o userlib.o

all: blocks cdf freecell jobshop lloyd rubik schedule slaney taxi tyre

blocks: blocks.o bblocks.o
	gcc -shared -lm -o blocks blocks.o bblocks.o 

cdf: cdf.o dcdflib.o ipmpar.o
	gcc -shared -lm -o cdf cdf.o dcdflib.o ipmpar.o

freecell: freecell.o
	gcc -shared -lm -o freecell freecell.o

jobshop: jobshop.o
	gcc -shared -lm -o jobshop jobshop.o

lloyd: lloyd.o
	gcc -shared -lm -o lloyd lloyd.o

rubik: rubik.o
	gcc -shared -lm -o rubik rubik.o

schedule: schedule.o
	gcc -shared -lm -o schedule schedule.o

slaney: slaney.o
	gcc -shared -lm -o slaney slaney.o

tyre: tyre.o
	gcc -shared -lm -o tyre tyre.o

taxi: taxi.o
	gcc -shared -lm -o taxi taxi.o

.c.o:
	gcc -c $(CFLAGS) $<

clean:
	rm -f $(OBJS) 