#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include <sys/time.h>

#include <sys/types.h>
#include <limits.h>
#include <signal.h>

/* Randomization */
int seed;
struct timeval tv;
struct timezone tzp;

int npackages, ncities, nlocs, ntrucks, nplanes;
int ngoals;
char * name;

int main( int argc, char * argv[])
{
    if (argc != 8){
	fprintf(stderr, "use: GenLogStrips name #packages #cities #planes #locs_per_city #trucks_per_city #goals\n");
	exit(1);
    }
    name = argv[1];
    npackages = atoi(argv[2]);
    ncities = atoi(argv[3]);
    nplanes = atoi(argv[4]);
    nlocs = atoi(argv[5]);
    ntrucks = atoi(argv[6]);
    ngoals = atoi(argv[7]);

    gettimeofday(&tv,&tzp);
    seed = (( tv.tv_sec & 0177 ) * 1000000) + tv.tv_usec;
    srandom(seed);

    print_header();
    print_init();
    print_final();
    
    return 0;
}

int print_header(void)
{
    int i,j;

    printf(";; a logistics problem instance\n");
    printf(";; name: %s\n", name); 
    printf(";; #packages: %d        #cities: %d  #planes: %d\n", npackages, ncities, nplanes);
    printf(";; #locs_per_city: %d   #trucks_per_city: %d\n", nlocs, ntrucks);
    printf(";; #goals: %d           seed: %d\n\n", ngoals, seed);

    printf("(define (problem %s)\n", name);
    printf("    (:domain logistics-strips)\n"
	   "    (:objects \n");
    for (i = 1; i<=npackages; i++)
	printf("        package%i\n", i);
    for (i = 1; i<=nplanes; i++)
	printf("        plane%i\n", i);
    for (i = 1; i<=ncities; i++){
	for (j = 1; j<=ntrucks; j++)
	    printf("        truck%i-%i\n", i,j);
	for (j = 1; j<=nlocs; j++)
	    printf("        loc%i-%i\n", i,j);
	printf("        city%i\n", i);
    }
    printf("    )\n");

    printf("    (:init \n");
    for (i = 1; i<=npackages; i++)
	printf("        (OBJ package%i)\n", i);
    for (i = 1; i<=nplanes; i++)
	printf("        (AIRPLANE plane%i)\n", i);
    for (i = 1; i<=ncities; i++){
	for (j = 1; j<=ntrucks; j++)
	    printf("        (TRUCK truck%i-%i)\n", i,j);
	for (j = 1; j<=nlocs; j++){
	    printf("        (LOCATION loc%i-%i)\n", i,j);
	}
	printf("        (CITY city%i)\n", i);
	printf("        (AIRPORT loc%i-1)\n", i);
    }
}
	
    
int print_init()
{
    int i, j;
    int city, loc;

    /* put down locations in cities */
    for (i=1; i<=ncities; i++){
	for (j = 1; j<=nlocs; j++){
	    printf("        (in-city loc%i-%i city%i)\n", i,j,i);
        }
    }
    /* put down airplanes and trucks */
    for (i=1; i<=nplanes; i++){
	city = random()%ncities + 1;
	printf("        (at plane%i loc%i-1)\n", i, city);
    }
    for (i=1; i<=ncities; i++){
	for (j=1; j<=ntrucks; j++){
	    loc =  random()%nlocs + 1;
	    printf("        (at truck%i-%i loc%i-%i)\n", i, j, i, loc);
	}
    }
    print_random(npackages);
    printf("    )\n");
}

int print_final()
{
    printf("    (:goal (and\n");
    print_random(ngoals);
    printf("    ))\n");
    printf(")\n");
}

int print_random(int n)
{
    int i;
    int city, loc;

    for (i = 1; i<=n; i++){
	city = random()%ncities + 1;
	loc =  random()%nlocs + 1;
	printf("        (at package%i loc%i-%i)\n", i, city, loc);
    }
}


