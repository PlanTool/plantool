#include <iostream>
#include <fstream>
#include <sstream>
#include <cerrno>
#include <cstdio>
#include <ctime>
#if defined __GNUC__ && __GNUC__ >= 3 && __GNUC_MINOR__ > 0
#include <ext/stdio_filebuf.h>
#endif
#include <stdio.h>
#include <string.h>
#include <stdio.h>

#include <unistd.h>
#include <sys/socket.h>
#include <netdb.h>
#include <netinet/in.h>
#include <ctime>
#include <sys/times.h>
#include <sys/param.h>
#include "global.h"
#include "actions.h"
#include "client.h"
#include "domains.h"
#include "exceptions.h"
#include "planners.h"
#include "problems.h"
#include "states.h"
#include <string>
#include <fstream>
#include <vector>




#ifndef CLIENT_NAME
#define  CLIENT_NAME	"G-pack"
#endif

extern "C" char* strtok_r( char*, const char*, char** );
extern int yyparse();
extern FILE* yyin;
std::string current_file;

void
printBanner( std::ostream &os )
{
  os << "G-pack, Version "<<gpt::version<<std::endl
     << "(Developed for IPPC-2011 by A. Kolobov, P. Dai, Mausam, and D. Weld "
     <<"on the basis of the miniGPT code from B. Bonet and H. Geffner.)"
     << std::endl;
}

void
printUsage( std::ostream &os )
{
  printBanner( os );
  os << std::endl
     << "Usage: ./planner <options>* <problem-server-machine-name>:<server-port> [<domain-file>] <problem-file> <problem-name>"
     << std::endl << std::endl
     << "Options:" << std::endl << std::endl
     << "  [-e <epsilon>]            (default = 0.05) -- The convergence \n"
     << "                                threshold" 
     << std::endl
     << "  [-i <hash-size>]          (default = 1638400) -- The number of \n"
     << "                                buckets in the state-value hash table" 
     << std::endl
     << "  [-p <planner>]            (default = glutton)" << std::endl
     << "  [-r <random-seed>]        (default = 407343) -- The seed to be used\n "
     << "                               in randomized operations" 
     << std::endl
     << "  [-t <timeout>]            (default = ~50000 years) -- The approximate\n" 
     <<"                                amount of time to solve the problem, \n"
     <<"                                in seconds" 
     << std::endl
     << std::endl
     << "  <planner>         := random | lr2tdp | glutton | gourmand" << std::endl
     << std::endl
     << "Example:"
     << std::endl << std::endl
     << " \"Solve problem 'p1' in file 'ex.pddl' with the Glutton algorithm "
     << std::endl
     << "  with a 300-second time limit. Let the epsilon for Glutton be 0.05."
     << std::endl
     << "  The competition server is running on the local machine on port "
     << "2323.\""
     << std::endl << std::endl
     << "  $ ./planner -t 300 -e 0.05 localhost:2323 ex.pddl p1"
     << std::endl << std::endl;
}


/*
  Parses the command line.
*/
bool
readArguments( int argc, char **argv, char* &hostport, char* &domain_file, char* &problem_file, char* &problem )
{
  if( argc == 1 ) goto usage;
  ++argv;
  --argc;
  while( argv[0][0] == '-' )
    switch( argv[0][1] )
      {
      case 'e':
	gpt::epsilon = atof( argv[1] );
	argv += 2;
	argc -= 2;
	break;
      case 'i':
	gpt::initial_hash_size = atoi( argv[1] );
	argv += 2;
	argc -= 2;
	break;
      case 'p':
	gpt::algorithm = argv[1];
	argv += 2;
	argc -= 2;
	break;
      case 'r':
	gpt::seed = atoi( argv[1] );
	argv += 2;
	argc -= 2;
	break;
      case 't':
	gpt::timeout = atoi( argv[1] );
	argv += 2;
	argc -= 2;
	break;
      case 'v':
	gpt::verbosity = atoi( argv[1] );
	argv += 2;
	argc -= 2;
	break;
      default:
	goto usage;
      }

  if( argc != 3 && argc != 4 )
    {
    usage:
      printUsage( std::cout );
      return( false );
    }
  else
    {
      if( argc == 3 )
	{
	  hostport = argv[0];
	  domain_file = NULL;
	  problem_file = argv[1];
	  problem = argv[2];
	}
      else
	{
	  hostport = argv[0];
	  domain_file = argv[1];
	  problem_file = argv[2];
	  problem = argv[3];
	}
      return( true );
    }
}


/*
  Parses an input file with a domain description, a problem description, or both.
*/
static bool 
read_file( const char* name )
{
  yyin = fopen( name, "r" );
  if( yyin == NULL )
    {
      std::cout << "parser:" << name << ": " << strerror( errno ) << std::endl;
      return( false );
    }
  else
    {
      current_file = name;
      bool success;
      try
	{
	  success = (yyparse() == 0);
	}
      catch( Exception exception )
	{
	  fclose( yyin );
	  std::cout << exception << std::endl;
	  return( false );
	}
      fclose( yyin );
      return( success );
    }
}


/*
  Connects to the competition server running on the specified machine at the
  specified port.
*/
static int 
connect( const char *hostname, int port )
{
  struct hostent *host = ::gethostbyname(hostname);
  if( !host )
    {
      perror( "gethostbyname" );
      return( -1 );
    }

  int sock = ::socket( PF_INET, SOCK_STREAM, 0 );
  if( sock == -1 )
    {
      perror( "socket" );
      return( -1 );
    }
  
  struct sockaddr_in addr;
  addr.sin_family = AF_INET;
  addr.sin_port = htons( port );
  addr.sin_addr = *((struct in_addr *)host->h_addr);
  memset( &(addr.sin_zero), '\0', 8 );

  if( ::connect( sock, (struct sockaddr*)&addr, sizeof(addr) ) == -1 )
    {
      perror( "connect" );
      return( -1 );
  }
  return( sock );
}


/*
  Initializes the specified planning algorithm.
*/
planner_t&
createPlanner( const problem_t &problem, char *algorithm )
{
  planner_t *planner;

  if( !strcasecmp( algorithm, "random" ) )
    {
      planner = new plannerRANDOM_t( problem, *gpt::hstack.top() );
    }
  else if( !strcasecmp( algorithm, "lr2tdp" ) )
    {
      planner = new plannerLR2TDP_t( problem, *gpt::hstack.top() );
    }
  else if( !strcasecmp( algorithm, "glutton" ) )
    {
      planner = new plannerGLUTTON_t( problem, *gpt::hstack.top() );
    }
  else if( !strcasecmp( algorithm, "gourmand" ) )
    {
      planner = new plannerGOURMAND_t( problem, *gpt::hstack.top() );
    }
  else
    {
      std::ostringstream msg;
      msg << "undefined planner `" << algorithm << "'";
      throw Exception( msg.str() );
    }

  return( *planner );
}


/*
  The planner's entry point.
*/
int 
main( int argc, char **argv )
{
  planner_t *planner;
  char *hostport, *domain_file, *problem_file, *prob = NULL;
  ushort_t seed[3];

  // set command line
  std::ostringstream cmd;
  for( int i = 0; i < argc; ++i )
    cmd << argv[i] << " ";

  // read arguments and print banner
  if( !readArguments( argc, argv, hostport, domain_file, problem_file,  prob ) )
    return( -1 );
  printBanner( std::cout );
  std::cout << "**" << std::endl;
  std::cout << "Running on a " << __WORDSIZE << "-bit system" <<std::endl;
  std::cout << "COMMAND: " << cmd.str() << std::endl;
  std::cout << "PLANNER: \"" << gpt::algorithm << "\"" << std::endl;
  std::cout << "HEURISTIC: \"" << gpt::heuristic << "\"" << std::endl;
  std::cout << "**" << std::endl;

  // set random seeds
  seed[0] = seed[1] = seed[2] = gpt::seed;
  srand48( gpt::seed );
  seed48( seed );

  tms start;
  times(&start);
  // parse the domain description file if it was provided...
  if( domain_file )
    {
      if( !read_file( domain_file ) )
	{
	  std::cout << "<amin>: ERROR: couldn't read domain description file `"
		    << domain_file << "'" << std::endl;
	  return( -1 );
	}
    }

  // otherwise, assume that the domain description is in the problem file and 
  // parse that
  if( !read_file( problem_file ) )
    {
      std::cout << "<main>: ERROR: couldn't read problem file `" 
		<< problem_file << "'" << std::endl;
      return( -1 );
    }

  // obtain problem from file
  problem_t *problem = (problem_t*)problem_t::find( prob );
  if( !problem )
    {
      std::cout << "<main>: ERROR: problem `" << prob 
		<< "' is not defined in file '"
		<< problem_file << "'" << std::endl;
      return( -1 );
    }

  if( gpt::verbosity >= 300 )
    {
      std::cout << "<domain-begin>" << std::endl
		<< problem->domain() << std::endl
		<< "<domain-end>" << std::endl;
    }

  // instantiate actions
  try
    {
      problem->instantiate_actions();
      problem->flatten();
      state_t::initialize( *problem );

      if( gpt::verbosity >= 300 )
	{
	  std::cout << "<problem-begin>" << std::endl << "goal: ";
	  problem->goalT().print( std::cout );
	  std::cout << std::endl << "<problem-end>" << std::endl;
	}
    }
  catch( Exception exception )
    {
      std::cout << exception << std::endl;
      return( -1 );
    }

  if( gpt::verbosity >= 300 )
    std::cout << "**" << std::endl;

  std::cout << "<begin-session>" << std::endl;

  
  // bind to the specified socket on the specified machine
  char *host = strtok( hostport, ":" );
  char *portstr = strtok( NULL , ":" );
  int port = atoi( portstr );
  int socket = connect( host, port );
  std::cout<<"Connected to socket"<<std::endl;

  if( socket <= 0 )
    {
      std::cout << "<main>: ERROR: couldn't connect to host:port `"
		<< host << ":" << port << "'" << std::endl;
      return( -1 );
    }

  /*
    Initialize the planning algorithm + heuristic and interact with the server
    This involves intiating a session with the server, receiving states from it,
    planning, and sending the actions recommended by the planner for the 
    received states back to the server.
  */
  try
    {
      gpt::hstack.push( new lashHeuristic_t( *problem ) );
      planner = &createPlanner( *problem, gpt::algorithm );
      problem->no_more_atoms();
      tms start_time;
      times (&start_time);

      // This is the line that initiates all the planning 
      XMLClient_t client( planner, problem );
      client.do_session(CLIENT_NAME, socket);
      tms end_time;
      times(&end_time);

      //*** COMPETITION-SPECIFIC LOGIC STARTS HERE **************************
      
      // See if we have enough time to attempt solving this problem
      // again with different parameter values. There parameter values are 
      // likely to result in a better policy than the one we got in the
      // previous attempt, but may make the planning process take longer 
      // than before. Thus, we want to attempt is only if we still have a lot 
      // of time left.
      if (elapsed_time(start_time, end_time) < 2.0 / 7.0 * gpt::timeout 
	  && gpt::solved_completely && !gpt::out_of_memory)
	{
	  std::cout<<"\n\nTOTAL TIME GIVEN "<< gpt::timeout <<std::endl;
	  std::cout<<"TIME USED UP SO FAR: "<< elapsed_time(start_time,end_time)
		   <<std::endl;
	  std::cout<<"RE-SOLVING WITH AN INCREASED SUCCESSOR SAMPLE SIZE...\n\n"
		   <<std::endl;
	  gpt::max_distrib_support *= 2;
	  gpt::num_succ_per_expansion *= 2;
	  planner->reset_state();
	  close(socket);
	  socket = connect( host, port );

	  if( socket <= 0 )
	    {
	      std::cout << "<main>: ERROR: couldn't connect to host:port `"
			<< host << ":" << port << "'" << std::endl;
	      return( -1 );
	    }

	  client.do_session(CLIENT_NAME, socket);
	}

      // It could happen that during the policy execution attempt above,
      // we submitted a policy computed by the planner and that that policy
      // is worse in terms of the expected reward than the best default
      // policy the planner has access to. If so, do another session with
      // the problem server and submit the planner's best default policy
      // during that session.
      if (gpt::previous_solution_attempt_reward 
	  < gpt::best_default_policy_reward)
	{
	  std::cout<<"\n\nTHE EXPECTED REWARD OF THE PREVIOUSLY SUBMITTED "
		   <<"POLICY ("<<gpt::previous_solution_attempt_reward<<") "
		   <<"IS LOWER THAN THE EXPECTED REWARD OF THE BEST DEFAULT "
		   <<"POLICY ("<<gpt::best_default_policy_reward<<"). " 
		   <<"SUBMITTING THE DEFAULT POLICY... \n\n"
		   <<std::endl;
	  gpt::use_best_default_policy = true;
	  planner->reset_state();
	  close(socket);
	  socket = connect( host, port );

	  if( socket <= 0 )
	    {
	      std::cout << "<main>: ERROR: couldn't connect to host:port `"
			<< host << ":" << port << "'" << std::endl;
	      return( -1 );
	    }

	  client.do_session(CLIENT_NAME, socket);
	}

      //*** COMPETITION-SPECIFIC LOGIC ENDS HERE **************************
    }
  catch( Exception exception )
    {
      close( socket );
      std::cout << exception << std::endl;
      return( -1 );
    }

  // print statistics and clean up
  planner->statistics( std::cout, gpt::verbosity );
  tms finish;
  times(&finish);
  delete planner;
  
  // destroy the heuristics
  while( !gpt::hstack.empty() )
    {
      delete gpt::hstack.top();
      gpt::hstack.pop();
    }

  problem_t::unregister_use( problem );
  problem_t::clear();
  Domain::clear();
  state_t::statistics( std::cout );
  state_t::finalize();

#ifdef MEM_DEBUG
  std::cerr << "<end-session>" << std::endl;
#endif
  std::cout << "<end-session>" << std::endl;
  std::cout << "Planning and execution took " << elapsed_time(start, finish) 
	    <<" seconds"<< std::endl;
 
  return( 0 );
}

