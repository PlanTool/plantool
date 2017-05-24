OBJS = blocks.o bblocks.o cdf.o dcdflib.o freecell.o ipmpar.o jobshop.o lloyd.o rubik.o schedule.o slaney.o taxi.o tyre.o userlib.o

FLAGS=  -shared -lm 
CFLAGS= -fPIC -Wall 

all: blocks cdf freecell jobshop lloyd rubik schedule slaney taxi tyre

blocks: blocks.o bblocks.o
	gcc $(FLAGS) -o blocks blocks.o bblocks.o 

cdf: cdf.o dcdflib.o ipmpar.o
	gcc $(FLAGS)  -o cdf cdf.o dcdflib.o ipmpar.o

freecell: freecell.o
	gcc $(FLAGS)  -o freecell freecell.o

jobshop: jobshop.o
	gcc $(FLAGS)  -o jobshop jobshop.o

lloyd: lloyd.o
	gcc $(FLAGS)  -o lloyd lloyd.o

rubik: rubik.o
	gcc $(FLAGS)  -o rubik rubik.o

schedule: schedule.o
	gcc $(FLAGS)  -o schedule schedule.o

slaney: slaney.o
	gcc $(FLAGS)  -o slaney slaney.o

tyre: tyre.o
	gcc $(FLAGS)  -o tyre tyre.o

taxi: taxi.o
	gcc $(FLAGS)  -o taxi taxi.o

.c.o:
	gcc -c $(CFLAGS) $<

clean:
	rm -f $(OBJS) 