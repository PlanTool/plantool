/*This file is the header of the implementation by the YOCHAN group
  in Arizona State University. The plan graph expansion is based
  in the Implementation of the STAN planner while the search extraction phase
  is based in the HSP-r planner. The result is ALTALT which integrates
  both approaches and developes a new kind of set heuristics extracted
  directly from the Planning graph. 
*/

/* 
* $Revision: 3.4 $
* $Date: 1999/05/19 19:51:06 $


This is the main file for STAN. It contains two main(...) definitions - 
one which predates PDDL, the other that works with PDDL. All of the
recent development work has been with the PDDL version, and there seems to
be some minor change to a function which means that it takes an extra 
argument since the PDDL stuff started. This means that the non-PDDL version
doesn't compile at present. This isn't really an issue, since our old
domain description language is unlikely to be adopted as a community 
standard.... (Shame, because it's prettier than PDDL.) */

#include <string.h>
#include <stream.h>
#include <stdio.h>
#include <fstream.h>

#include "alldefs.h"
#include "TimInterface.h"
#include "facts.h"
#include "construction.h"
#include "candidates.h"
#include "instantiation.h"
#include "badgoals.h"
#include "search.h"
#include "searchhsp.h"
#include "actions.h"
#include "globals.h"

/* BM: Added by BM */
ofstream my_outfile;
char my_outfile_name[100];
char my_buffer[100000];
/* BM: End */

#ifndef PDDL

main(int argc,char * argv[])
{
#ifdef BADSTATS
	for(int i=0;i<MAX_PLAN;i++)
	{	badstats[i] = 0;
		calls_at[i] = 0;
		fail2s_at[i] = 0;
	};

#endif

#ifdef CHECKSTATS
	for(int i = 0;i<MAX_PLAN;i++)
		for(int j = 0;j<MAX_ACTIONS;j++)
			checks[i][j] = 0;
#endif

	if(argc == 1)
	{
		cout << "No input file specified!\n";
		exit(1);
	};

	FILE * in = freopen(argv[1],"r",stdin);

	if(!in)
	{
		cout << "File not found!\n";
		exit(1);
	};

	allpreds = 0;

	yyparse();
	fclose(in);

	stan(cout);
#ifdef MOBFILTER
	timcall();
#endif

	build_all_actions(topdom->ops,allobs);

	token_list initial_state = 0;
	token_list goals = 0;
	hash_entry * newf;

#ifdef DEBUG
	cout << "Actions built\n";
#endif

	predlist ps,tmp;
	frCount = 1;
	int arc = 0;

	for(plptr fs=topdom->initial_state;fs>0;fs=fs->rest)
	{
		newf = all_facts.insert(catstrs(fs->prop));
		if(newf->is_modifiable())
		{
			initial_state = new token_list_node(newf,initial_state);
			for(ps = allpreds;ps;ps=ps->next)
				if(ps->fr>0 && strcmp(ps->name,fs->prop->nm->varname)==0)
				{
					ps->fr += 1;
					if(ps->fr==3) 
					{	frCount++;
						if(ps->ar==1) arc++;
					};
					break;
				};
		};

		newf->set_achievable();
	};

	rCount = frCount;

	for(ps=allpreds;ps;ps=ps->next)
		if(ps->fr<3 && ps->ar==1) rCount++;

	resnms = new char *[rCount];
	fixedres = new int[rCount];
	fixedreslim = new int[frCount];
	appres = new int[rCount];
	apprate = new int[rCount];

	int frc = 1;
	int rc = frCount;
	rStart = frCount-arc;
	arc = rStart;

	ps=0;

	while(allpreds)
	{
		if(allpreds->fr>2)
		{
			if(allpreds->ar==1)
			{
				resnms[arc] = allpreds->name;
				fixedreslim[arc] = allpreds->fr-1;
				apprate[arc++] = (int) allpreds->op;
			}
			else
			{
				resnms[frc] = allpreds->name;
				fixedreslim[frc] = allpreds->fr-1;
				apprate[frc++] = 0;
			};
		}
		else
		{
			if(allpreds->ar==1)
			{	
				resnms[rc] = allpreds->name;
				apprate[rc++] = (int) allpreds->op;
			};
		};

		if(allpreds->fr>0)
		{
			tmp = allpreds;
			allpreds = allpreds->next;
			tmp->next = ps;
			ps = tmp;
		}
		else
		{
			allpreds = allpreds->next;
		};
	};

	allpreds = ps;

	for(frc = rStart;frc<rCount;frc++)
		apprate[frc] = appRate(resnms[frc],(opptr) apprate[frc]);			

	fixedreslim[0] = MAX_FACTS;
	fixedres[0] = 0;
	apprate[0] = 0;

	if(frCount==1) frCount = 0;
	if(rCount-rStart == 0)
	{
		rStart = 0;
		rCount = frCount;
	};

#ifdef APPRATE
	cout << "Fixed resources:\n";
	for(frc=1;frc<frCount;frc++)
		cout << resnms[frc] << " - " << fixedreslim[frc] << "\n";
	cout << "\n";
	cout << "Other resources:\n";
	for(frc=rStart;frc<rCount;frc++)
		cout << resnms[frc] << " - " << apprate[frc] << "\n";
	cout << "\n";
#endif

	filter_actions();

	for(plptr fs=topdom->goal_state;fs>0;fs=fs->rest)
	{
		newf = all_facts.insert(catstrs(fs->prop));
		if(newf->is_modifiable())
			goals = new token_list_node(newf,goals);
		if(!newf->is_achievable())
		{
			cout << "\nGoal not achievable: " << newf->get_name() << "\n";
			exit(1);
		};
	};

#ifdef DEBUG
	cout << "Built states\n";
#endif

	int f = find_plan(initial_state,goals);

	if(f>=0)
	{
		for(int i = 1;i<=f;i++)
		{	cout << "Time: " << i;
			char * str = "   \t";
			for(action_list j = committed_acts_at[i];j>0;j=j->next)
				if(!j->act->is_noop()) 
				{
					cout << str << j->act->get_name() << "\n";
					str = "\t\t";
				};

		};
	}
	else
	{
		cout << "No plan found!\n";
	};


#ifdef FINALSTATS
	cout << "\n\nThe graph has: " << final_level_count << " layers.\nThe initial segment was " << initial_length << " layers and it leveled after " << level_out << " layers\n";

#endif
	
#ifdef BADSTATS

	cout << "BADSTATS:\n";

	for(int i = 0; i <= level_out; i++)
		cout << i << ": " << badstats[i] << "\n";

	for(int i = 0;i <= f;i++)
		cout << i << ": Calls: " << calls_at[i] << " Fails: " << fail2s_at[i] << "\n";

#endif

#ifdef CHECKSTATS
	for(int i = 1;i<=level_out;i++)
	{	cout << i << ": ";
 		for(int j = 0;j<acts_at[i];j++)
		{	if(checks[i][j] > 100) cout << action_table[j]->get_name() << ": ";
			cout << checks[i][j] << " ";
		};
		cout << "\n";
	};
#endif

#ifdef HASHDEBUG
	cout << "Hit count: " << counthits << "\n";
#endif


}

#endif

#ifdef PDDL

#include <new.h>
#include <sys/time.h>

struct timeval tstart,tend;
ofstream outfile;


/* A function to try to handle memory problems a bit more gracefully
and at least get a time. This is actually used as the catch-all for
all error cases at the moment, so we don't get a great deal of information
about what really caused the failure. */

void memerr()
{
	cerr << "STAN: Some sort of memory allocation error!\n\n";
	cout << "\nNO SOLUTION\n";
	outfile << pname << "\nNO SOLUTION\n";

	gettimeofday(&tend,0);
  	long total_time = (tend.tv_sec - tstart.tv_sec) * 1000 + (tend.tv_usec-tstart.tv_usec)/1000 ;

  	cout << "Total time elapsed: " << total_time << " millisecs\n";
#ifdef PDDLOUTPUT
   	outfile << total_time << "\n";
  	outfile.close();
#endif

	exit(1);
};


void setresults(char * n)
{
	outfile.open(n,ios::app | ios::out);
	if(!outfile)
	{
		cerr << "Can't open " << n << " for results!\n";
		exit(1);
	};

};

main(int argc,char * argv[])
{
       gettimeofday( &tstart,0 );

       gettimeofday( &pg_tstart,0);


	set_new_handler(memerr);
#ifdef BADSTATS
	for(int i=0;i<MAX_PLAN;i++)
	{	badstats[i] = 0;
		calls_at[i] = 0;
		fail2s_at[i] = 0;
	};

	WHICHSEARCH=STANBASED;

#endif

#ifdef CHECKSTATS
	for(int i = 0;i<MAX_PLAN;i++)
		for(int j = 0;j<MAX_ACTIONS;j++)
			checks[i][j] = 0;
#endif

	if(argc < 3)
	{
        /*RS: Added by RS*/         
	cout << "ALTALT Planner - Version 1.0\n\n"<<
 		"This planner extends the work of STAN by Derek Long and Maria Fox,\n"<<
 		"and the HSP-R planner by Hector Geffner and Blai Bonet.\n"<<
 		"Integrates a Planning graph expansion approach into a heuristc\n"<<
 		"search planner. Extracting a Set of heuristics from the Planning graph, and\n"<<
 		"using them to guide a regression state space search for a solution.\n"<<
 		"This planner has been developed by the YOCHAN student group in Arizona State University\n"<<
 		"Direct developers are: Romeo Sanchez, XuanLong Nguyen\n"<<
             	"Output files module: Binhminh Do, Biplav Srivastava\n"<<
 		"Comments and testing: Ullas N, Nie, Terry Z.\n\n"<<
	  "Format of input: altalt domain-name prob-name [-w] [-l] [heuristic: HPLUS, HCOMBO, HRELAX, HPART{1,2,2C},
HADJSUM{1,2,2M}] [SERIAL] [-of output-file-name]. PACTION NOLEVOFF\n\n"<<endl;
        /*RS: Added by RS*/
		return 0;
	};

#ifdef PDDLOUTPUT
	if(argc < 4)
#endif
#ifndef PDDLOUTPUT
	if(argc < 3)
#endif
	{
		cerr << "Missing file specifier!\n";
		exit(1);
	};

	yyin = fopen(argv[1],"r");
#ifdef PDDLOUTPUT
	setresults(argv[3]);
#endif

	if(!yyin)
	{
		cerr << "Operator file not found: " << argv[1] << "\n";
		exit(1);
	};

	allpreds = 0;

	yyparse();

	yyin = fopen(argv[2],"r");

	if(!yyin)
	{
		cerr << "Problem file not found!\n";
		exit(1);
	};

	yyparse();
        

        /* RS: Added by RS */
	if(argc>3)
	  {        
           for(int i=3;i<argc;i++)
	     {

	       if(strcmp(argv[i],"-of")==0){
	         if(i+1<argc)
		   {
               /* BM: Read the file name */
	       strcpy(my_outfile_name, argv[i+1]);
	       my_outfile.open(argv[i+1],ios::app | ios::out);
	       if(!my_outfile)
		 {
		   cerr << "Can't open " << argv[i+1] << " for results!\n";
		   exit(1);
		 }
               else i=i+1;
	       my_outfile << "\n" << pname << ",";
	       my_outfile.close();
	       my_buffer[0] = '\0';
	       /* BM: End */
		   }
		 else
		   cout<<"Not output file specified"<<endl;
	       }
               else if(strcmp(argv[i],"-w")==0){
                 int param;
                 sscanf(argv[++i], "%d", &param);
		 if(param>0&&param<6)
		   GWEIGHT=param;
	       }
               else if(strcmp(argv[i],"-l")==0){
                 int param2;
                 sscanf(argv[++i], "%d", &param2);
		 if(param2>0)
		   GLEVEL=param2;
	       }
	       else if(strcmp(argv[i],"HPLUS")==0)
		 HEURISTYPE=HPLUS;
	       else if(strcmp(argv[i],"HLEVEL")==0)
		 HEURISTYPE=HLEVEL;
	       else if(strcmp(argv[i],"HPART1")==0)
		 HEURISTYPE=HPART1;
	       else if(strcmp(argv[i],"HPART2")==0)
		 HEURISTYPE=HPART2;
	       else if(strcmp(argv[i],"HPART2C")==0)
		 HEURISTYPE=HPART2C;
               else if(strcmp(argv[i],"HCOMBO")==0)
		 HEURISTYPE=HCOMBO;
	       else if(strcmp(argv[i],"HADJSUM1")==0)
		 HEURISTYPE=HADJSUM1;
               else if(strcmp(argv[i],"HADJSUM2")==0)
		 HEURISTYPE=HADJSUM2;
	       else if(strcmp(argv[i],"HADJSUM2M")==0)
		 HEURISTYPE=HADJSUM2M;
	       else if(strcmp(argv[i],"SERIAL")==0)
		 SERIAL=1;	       
	       else if(strcmp(argv[i],"PACTION")==0)
		 PACTION=1;
	       else if(strcmp(argv[i],"NOLEVOFF")==0)
		 NOLEVOFF=1;
	       else if(strcmp(argv[i],"REVCHECK")==0)
		 REVCHECK=0; //NOt fully implemented...
	       else
		 {
		   cout<<"Incorrect input format..."<<endl;
		   exit(0);
		 }
	     }
	  }
	/* RS: End */


#ifdef PDDLOUTPUT
	cout << pname << " read\n";
#endif

	stan(cout);
#ifdef SYMMETRY
	numAllSymObs = object(0,0).howManySymObs()+1;

	brokenSym = new int[numAllSymObs];
	for(int i = 0;i<numAllSymObs;i++) brokenSym[i] = 0;
	symDomobs = new object * [numAllSymObs];
	for(objectlist os = allobs;os;os=os->next)
		symDomobs[os->obj->getSymIndex()] = os->obj;
	symSizes[0] = 0;
	
#endif

#ifdef HSPSEARCH
	  WHICHSEARCH=HSPBASED;
#endif
   

	build_all_actions(topdom->ops,allobs);

	token_list initial_state = 0;
	token_list goals = 0;
	hash_entry * newf;

#ifdef DEBUG
	cout << "Actions built\n";
#endif

	predlist ps,tmp;
	frCount = 1;
	int arc = 0;

/* Code to analyse the initial state and generate fixed resource counts. */


	for(plptr fs=topdom->initial_state;fs>0;fs=fs->rest)
	{
		newf = all_facts.insert(catstrs(fs->prop));
		if(newf->is_modifiable())
		{
			initial_state = new token_list_node(newf,initial_state);
			for(ps = allpreds;ps;ps=ps->next)
				if(ps->fr>0 && strcmp(ps->name,fs->prop->nm->varname)==0)
				{
					ps->fr += 1;
					if(ps->fr==3) 
					{	frCount++;
						if(ps->ar==1) arc++;
					};
					break;
				};
		};

		newf->set_achievable();
	};

	rCount = frCount;

	for(ps=allpreds;ps;ps=ps->next)
		if(ps->fr<3 && ps->ar==1) rCount++;

	resnms = new char *[rCount];
	fixedres = new int[rCount];
	fixedreslim = new int[frCount];
	appres = new int[rCount];
	apprate = new int[rCount];

	int frc = 1;
	int rc = frCount;
	rStart = frCount-arc;
	arc = rStart;

	ps=0;

/* Now set up the fixed and accumulating resource information in an easily
accessible place. */

	while(allpreds)
	{
		if(allpreds->fr>2)
		{
			if(allpreds->ar==1)
			{
				resnms[arc] = allpreds->name;
				fixedreslim[arc] = allpreds->fr-1;
				apprate[arc++] = (int) allpreds->op;
			}
			else
			{
				resnms[frc] = allpreds->name;
				fixedreslim[frc] = allpreds->fr-1;
				apprate[frc++] = 0;
			};
		}
		else
		{
			if(allpreds->ar==1)
			{	
				resnms[rc] = allpreds->name;
				apprate[rc++] = (int) allpreds->op;
			};
		};

		if(allpreds->fr>0)
		{
			tmp = allpreds;
			allpreds = allpreds->next;
			tmp->next = ps;
			ps = tmp;
		}
		else
		{
			allpreds = allpreds->next;
		};
	};

	allpreds = ps;

	for(frc = rStart;frc<rCount;frc++)
		apprate[frc] = appRate(resnms[frc],(opptr) apprate[frc]);			

	fixedreslim[0] = MAX_FACTS;
	fixedres[0] = 0;
	apprate[0] = 0;

	if(frCount==1) frCount = 0;
	if(rCount-rStart == 0)
	{
		rStart = 0;
		rCount = frCount;
	};

#ifdef APPRATE
	cout << "Fixed resources:\n";
	for(frc=1;frc<frCount;frc++)
		cout << resnms[frc] << " - " << fixedreslim[frc] << "\n";
	cout << "\n";
	cout << "Other resources:\n";
	for(frc=rStart;frc<rCount;frc++)
		cout << resnms[frc] << " - " << apprate[frc] << "\n";
	cout << "\n";
#endif

	filter_actions();

	for(plptr fs=topdom->goal_state;fs>0;fs=fs->rest)
	{
		newf = all_facts.insert(catstrs(fs->prop));
		if(newf->is_modifiable())
			goals = new token_list_node(newf,goals);
		if(!newf->is_achievable())
		{
  			gettimeofday(&tend,0);
  			long total_time = (tend.tv_sec - tstart.tv_sec) * 1000 + (tend.tv_usec-tstart.tv_usec)/1000 ;


#ifdef PDDLOUTPUT
			outfile << pname << "\nNO SOLUTION\n";
   			outfile << total_time << "\n";
  			outfile.close();
#endif

			cout << "\nGoal not achievable: " << newf->get_name() << "\nNO SOLUTION\n";
  			cout << "Total time elapsed: " << total_time << " millisecs\n";

                	/* BM: Added */
			my_outfile.open(my_outfile_name,ios::app | ios::out);
			if(!my_outfile)
			  {
			    cerr << "Can't open " << my_outfile_name << " for results (time)!\n";
			    exit(1);
			  };

			my_outfile << (float) total_time/1000 <<  ",";
			my_outfile << my_buffer << "\n\n";
			my_outfile.close();
			/* BM: End */

			exit(1);
		};
	};

#ifdef DEBUG
	cout << "Built States\n";
#endif

     int f = find_plan(initial_state,goals);
     if(WHICHSEARCH==STANBASED)
	  {
#ifdef PDDLOUTPUT
       	if(f>=0)
	{


		outfile << pname << "\n";
		outfile << "(";
		cout << "(";
		char * ss = "(";
		for(int i = 1;i<=f;i++)
		{
			for(action_list j = committed_acts_at[i];j>0;j=j->next)
				if(!j->act->is_noop()) 
				{
					outfile << ss << j->act->get_name() << ")";
					cout << ss << j->act->get_name() << ")";
					ss = "\n(";
				};
		};
		outfile << ")\n";
		cout << ")\n";
	}
	else
	{
		outfile << pname << "\nNO SOLUTION\n";
		cout << "NO SOLUTION\n";
	};
	  }
#endif

/* A prettier output format than PDDL which also respects parallel plans. */

#ifndef PDDLOUTPUT
	cout << "\n\n";
	if(f>=0)
	{

		for(int i = 1;i<=f;i++)
		{	cout << "Time: " << i;
			char * str = "   \t";
			for(action_list j = committed_acts_at[i];j>0;j=j->next)
				if(!j->act->is_noop()) 
				{
					cout << str << j->act->get_name() << "\n";
					str = "\t\t";
				};

		};
	}
	else
	{
		cout << "No plan found!\n";
	};
#endif

#ifdef FINALSTATS
	cout << "\n\nThe graph has: " << final_level_count << " layers.\nThe initial segment was " << initial_length << " layers and it leveled after " << level_out << " layers\n";

#endif
	
#ifdef BADSTATS

	cout << "BADSTATS:\n";

#ifndef WFOFF
	for(int i = 0; i <= level_out; i++)
#endif
#ifdef WFOFF
	for(int i = 0;i <= f;i++)
#endif
		cout << i << ": " << badstats[i] << "\n";

	for(int i = 0;i <= f;i++)
		cout << i << ": Calls: " << calls_at[i] << " Fails: " << fail2s_at[i] << "\n";

#endif

#ifdef CHECKSTATS
	for(int i = 1;i<=level_out;i++)
	{	cout << i << ": ";
 		for(int j = 0;j<acts_at[i];j++)
		{	if(checks[i][j] > 100) cout << action_table[j]->get_name() << ": ";
			cout << checks[i][j] << " ";
		};
		cout << "\n";
	};
#endif

#ifdef HASHDEBUG
	cout << "Hit count: " << counthits << "\n";
#endif

} //ends for stan based search....
  
  gettimeofday(&tend,0);
  long total_time = (tend.tv_sec - tstart.tv_sec) * 1000 + (tend.tv_usec-tstart.tv_usec)/1000 ;



if(WHICHSEARCH==HSPBASED) 
   {
     cout << "Graph construction time elapsed: " << totalgp << " millisecs\n";
     cout << "Heuristic search time elapsed:   " << totalhsp << " millisecs\n";

   }
     cout << "Total time elapsed: " << total_time << " millisecs\n";


/* BM: Added */

my_outfile.open(my_outfile_name,ios::app | ios::out);
if(!my_outfile)
{
  cerr << "Can't open file" << my_outfile_name << " for results (time)!\n";
  exit(1);
};

my_outfile << (float) total_time/1000 << ",";
my_outfile << my_buffer << "\n\n";
my_outfile.close();
/* BM: End */

#ifdef PDDLOUTPUT
   outfile << total_time << "\n";
  outfile.close();
#endif
};

#endif


