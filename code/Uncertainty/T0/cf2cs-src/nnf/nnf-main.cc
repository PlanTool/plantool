#include "nnf.h"
#include "satlib.h"

#include <stdio.h>
#include <sys/resource.h>
#include <iostream>
#include <fstream>
#include <functional>

int verbosity_level = 0;

void
banner( std::ostream &os )
{
  os << "NNF: A nnf reader / processor." << std::endl
     << "Universidad Simon Bolivar, 2004." << std::endl
     << std::endl;
}

void
usage( std::ostream &os )
{
  os << "usage: nnf"
     << " [-c]"
     << " {-l <literal>}*"
     << " [-o {nnf}]"
     << " [-v <verbosity>]"
     << " [-?]"
     << " [<nnf-file>]"
     << std::endl;
  exit( -1 );
}

inline float
read_time_in_seconds( void )
{
  struct rusage r_usage;
  getrusage( RUSAGE_SELF, &r_usage );
  return( (float)r_usage.ru_utime.tv_sec +
	  (float)r_usage.ru_stime.tv_sec +
	  (float)r_usage.ru_utime.tv_usec / (float)1000000 +
	  (float)r_usage.ru_stime.tv_usec / (float)1000000 );
}

int
main( int argc, char **argv )
{
  float i_seconds, l_seconds, seconds;
  std::string nnf_file;

  int nnf_depth;
  unsigned nnf_nodes, nnf_edges;
  bool nnf_satisfiable, nnf_decomposable, nnf_smooth;
  double nnf_models;

  std::vector<int> lits;
  bool count_models = false;
  bool output_nnf = false;
  bool output_models = false;

  banner( std::cout );

  // parse arguments
  ++argv;
  --argc;
  if( argc == 0 ) goto finished_with_args;
  while( argc && ((*argv)[0] == '-') )
    {
      switch( (*argv)[1] )
	{
	case '-':
	  ++argv;
	  --argc;
	  goto finished_with_args;
	  break;
	case 'c':
	  count_models = true;
	  ++argv;
	  --argc;
	  break;
	case 'l':
	  lits.push_back( atoi( argv[1] ) );
	  argv += 2;
	  argc -= 2;
	  break;
	case 'o':
	  if( !strcmp( argv[1], "nnf" ) )
	    output_nnf = true;
	  else if( !strcmp( argv[1], "models" ) )
	    output_models = true;
	  argv += 2;
	  argc -= 2;
	  break;
	case 'v':
	  verbosity_level = atoi( argv[1] );
	  argv += 2;
	  argc -= 2;
	  break;
	case '?':
	default:
	  usage( std::cout );
	}
    }
 finished_with_args:
  if( argc == 0 )
    nnf_file = "-";
  else if( argc == 1 )
    nnf_file = argv[0];
  else
    usage( std::cout );

  // create managers
  nnf::Manager *nnf_manager = new nnf::Manager;
  std::cout << std::endl;

  // set start time
  i_seconds = l_seconds = read_time_in_seconds();

  // read nnf from file
  std::istream *is;
  if( nnf_file == "-" )
    is = &std::cin;
  else
    is = new std::ifstream( nnf_file.c_str() );

  try
    {
      std::cout << "main: reading file '" << (nnf_file=="-"?"<stdin>":nnf_file) << "' ...";
      std::cout.flush();
      satlib::read_nnf_file( *is, *nnf_manager );
      if( nnf_file != "-" ) delete is;
    }
  catch( int e )
    {
      if( nnf_file != "-" ) delete is;
      std::cout << std::endl << "main: error reading nnf file '" << nnf_file << "'." << std::endl;
      exit( -1 );
    }
  seconds = read_time_in_seconds();
  std::cout << " " << seconds-l_seconds << " seconds." << std::endl;
  l_seconds = seconds;

#if 0
  // nnf data
  nnf_depth = nnf_manager->depth();
  nnf_nodes = nnf_manager->count_nodes();
  nnf_edges = nnf_manager->count_edges();
  nnf_satisfiable = nnf_manager->satisfiable();
  nnf_decomposable = nnf_manager->weak_decomposable();
  nnf_smooth = nnf_manager->smooth();
  nnf_models = nnf_manager->count_models();
  seconds = read_time_in_seconds();
  std::cout << "main: nnf: "
	    << "depth=" << nnf_depth
	    << ", #nodes=" << nnf_nodes
	    << ", #edges=" << nnf_edges
    	    << ", " << (nnf_satisfiable?"satisfiable":"non-satisfiable")
    	    << ", " << (nnf_decomposable?"w-decomposable":"non-w-decomposable")
    	    << ", " << (nnf_smooth?"smooth":"non-smooth")
    	    << ", " << (nnf_manager->sorted()?"sorted":"non-sorted")
    	    << ", #models=" << nnf_models
	    << ", " <<  seconds-l_seconds << " seconds"
	    << '.' << std::endl;
  l_seconds = seconds;
  if( output_nnf ) nnf_manager->printXML( std::cout );
  l_seconds = read_time_in_seconds();
  if( output_models )
    {
      nnf::ModelList models;
      nnf_manager->enumerate_models( models );
      seconds = read_time_in_seconds();
      std::cout << "main: models: begin" << std::endl
		<< models
		<< "main: models: end, " << seconds-l_seconds << " seconds." << std::endl;
      l_seconds = seconds;
    }
  std::cout << std::endl;
#endif

  // smooth
  seconds = read_time_in_seconds();
  std::cout << "main: smoothing ...";
  std::cout.flush();
  nnf_manager->make_smooth();
  seconds = read_time_in_seconds();
  std::cout << " " << seconds-l_seconds << " seconds." << std::endl;
  l_seconds = seconds;

  // sort
  seconds = read_time_in_seconds();
  std::cout << "main: sorting ...";
  std::cout.flush();
  nnf_manager->make_sorted();
  seconds = read_time_in_seconds();
  std::cout << " " << seconds-l_seconds << " seconds." << std::endl;
  l_seconds = seconds;
  assert( nnf_manager->verify_sort() );

#if 0
  // nnf data
  nnf_depth = nnf_manager->depth();
  nnf_nodes = nnf_manager->count_nodes();
  nnf_edges = nnf_manager->count_edges();
  nnf_satisfiable = nnf_manager->satisfiable();
  nnf_decomposable = nnf_manager->weak_decomposable();
  nnf_smooth = nnf_manager->smooth();
  seconds = read_time_in_seconds();
  std::cout << "main: nnf: "
	    << "depth=" << nnf_depth
	    << ", #nodes=" << nnf_nodes
	    << ", #edges=" << nnf_edges
    	    << ", " << (nnf_satisfiable?"satisfiable":"non-satisfiable")
    	    << ", " << (nnf_decomposable?"w-decomposable":"non-w-decomposable")
    	    << ", " << (nnf_smooth?"smooth":"non-smooth")
    	    << ", " << (nnf_manager->sorted()?"sorted":"non-sorted")
	    << ", " <<  seconds-l_seconds << " seconds"
	    << '.' << std::endl;
  l_seconds = seconds;
  if( output_nnf ) nnf_manager->printXML( std::cout );
  l_seconds = read_time_in_seconds();
  if( output_models )
    {
      nnf::ModelList models;
      nnf_manager->enumerate_models( models );
      seconds = read_time_in_seconds();
      std::cout << "main: models: begin" << std::endl
		<< models
		<< "main: models: end, " << seconds-l_seconds << " seconds." << std::endl;
      l_seconds = seconds;
    }
#endif

#if 0
    {
      nnf::ModelList models;
      nnf_manager->enumerate_models( models );
      seconds = read_time_in_seconds();
      std::cout << "main: models: begin" << std::endl
		<< models
		<< "main: models: end, " << seconds-l_seconds << " seconds." << std::endl;
      l_seconds = seconds;
    }
#endif

  std::cout << std::endl;
  if( count_models )
    {
      float *output = new float[1+nnf_manager->num_vars()];
      int *litmap = 0;
      if( lits.size() > 0 )
	{
	  litmap = (int*)calloc( 2*(1+nnf_manager->num_vars()), sizeof(int) );
	  for( std::vector<int>::const_iterator li = lits.begin(); li != lits.end(); ++li )
	    litmap[*li] = 1;
	}
      nnf_manager->count_models( output, litmap );  
      seconds = read_time_in_seconds();
      std::cout << "main: #models = " << output[0] << std::endl;
#if 0
      for( size_t i = 0; i < nnf_manager->num_vars(); ++i )
	std::cout << "main: #models with +v" << (1+i) << " = " << output[1+i] << std::endl;
#endif
      delete[] output;
      if( litmap ) free( litmap );
      std::cout << "main: #models in " << seconds-l_seconds << " seconds." << std::endl << std::endl;
      l_seconds = seconds;
    }

  // cleanup
  nnf_manager->unregister_use( nnf_manager->root() );
  delete nnf_manager;

  // total time
  seconds = read_time_in_seconds();
  std::cout << std::endl
	    << "main: total time " << seconds-i_seconds << " seconds." << std::endl;
  return( 0 );
}
