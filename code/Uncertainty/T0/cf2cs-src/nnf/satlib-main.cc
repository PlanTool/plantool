#include "cnf.h"
#include "dtree.h"
#include "hgraph.h"
#include "satlib.h"

#include <stdio.h>
#include <sys/resource.h>
#include <iostream>
#include <fstream>
#include <functional>

#ifdef DEBUG
extern int _global_debug_level;
#endif
int verbosity_level = 0;

void
banner( std::ostream &os )
{
  os << "SATLIB: A ddnnf compiler for cnf's in satlib format." << std::endl
     << "Universidad Simon Bolivar, 2004." << std::endl
     << std::endl;
}

void
usage( std::ostream &os )
{
  os << "usage: satlib"
     << " [-m <method>]"
     << " [-d <dtree-file>]"
     << " [-f <dnnf-formula>]"
     << " [-g]"
     << " [-o {dnnf|dtree|models}]"
     << " [-u <ubfactor>]"
     << " [-v <verbosity>]"
     << " <satlib-file>"
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
  std::string cnf_file;

  int method = -1;

  char *actions_str = 0;
  std::string satlib_file;
  std::string xml_dtree_file;
  std::string xml_dnnf_file;

  std::vector<int> lits;
  bool count_models = false;
  bool output_cnf = false;
  bool output_dtree = false;
  bool output_ddnnf = false;
  bool output_models = false;
  bool print_graphviz = false;
  bool call_solver = false;

  banner( std::cout );

  // parse arguments
  ++argv;
  --argc;
  if( argc == 0 ) goto finished_with_args;
  while( (*argv)[0] == '-' )
    {
      switch( (*argv)[1] )
	{
	case '-':
	  ++argv;
	  --argc;
	  goto finished_with_args;
	  break;
	case 'a':
	  actions_str = argv[1];
	  argv += 2;
	  argc -= 2;
	  break;
	case 'c':
	  count_models = true;
	  ++argv;
	  --argc;
	  break;
	case 'd':
	  xml_dtree_file = argv[1];
	  argv += 2;
	  argc -= 2;
	  break;
	case 'f':
	  xml_dnnf_file = argv[1];
	  argv += 2;
	  argc -= 2;
	  break;
	case 'g':
	  print_graphviz = true;
	  ++argv;
	  --argc;
	  break;
	case 'l':
	  lits.push_back( atoi( argv[1] ) );
	  argv += 2;
	  argc -= 2;
	  break;
	case 'm':
	  method = atoi( argv[1] );
	  argv += 2;
	  argc -= 2;
	  break;
	case 'o':
	  if( !strcmp( argv[1], "cnf" ) )
	    output_cnf = true;
	  else if( !strcmp( argv[1], "dtree" ) )
	    output_dtree = true;
	  else if( !strcmp( argv[1], "nnf" ) )
	    output_ddnnf = true;
	  else if( !strcmp( argv[1], "models" ) )
	    output_models = true;
	  argv += 2;
	  argc -= 2;
	  break;
	case 's':
	  call_solver = true;
	  ++argv;
	  --argc;
	  break;
	case 'u':
	  hgraph::ubfactor = atoi( argv[1] );
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
    cnf_file = "-";
  else if( argc == 1 )
    cnf_file = argv[0];
  else
    usage( std::cout );

  // extract actions (if any)
  cnf::VarList actions;
  if( actions_str )
    {
      char *ptr = strdup( actions_str );
      for( char *p = strtok( ptr, " " ); p; p = strtok( NULL, " " ) )
	actions.push_back( atoi( ptr ) );
      free( ptr );
    }

  // create managers
  cnf::Manager *cnf_manager = new cnf::Manager;
  nnf::Manager *nnf_manager = new nnf::Manager;
  dtree::Manager *dtree_manager = new dtree::Manager( *nnf_manager );
  std::cout << std::endl;

#ifdef DEBUG
  // libsat debug verbosity (must link with a 'debug' version of libsat)
  _global_debug_level = 1;
#endif

  // set start time
  i_seconds = l_seconds = read_time_in_seconds();

  // read cnf from file
  std::istream *is;
  if( cnf_file == "-" )
    is = &std::cin;
  else
    is = new std::ifstream( cnf_file.c_str() );

  try
    {
      std::cout << "main: reading file '" << (cnf_file=="-"?"<stdin>":cnf_file) << "' ...";
      std::cout.flush();
      satlib::read_cnf_file( *is, *cnf_manager );
      cnf_manager->init_solve();
      nnf_manager->set_num_vars( cnf_manager->number_variables() );
      if( cnf_file != "-" ) delete is;
    }
  catch( int e )
    {
      if( cnf_file != "-" ) delete is;
      std::cout << std::endl << "main: error reading nnf file '" << cnf_file << "'." << std::endl;
      exit( -1 );
    }
  seconds = read_time_in_seconds();
  std::cout << " " << seconds-l_seconds << " seconds." << std::endl;
  l_seconds = seconds;

#if 1
  // enumerate models
  std::cout << "main: enumerate-models: begin." << std::endl;
  cnf_manager->enumerate_models( std::cout );
  std::cout << "main: enumerate-models: end." << std::endl;
  double cnf_models = cnf_manager->count_models();
  seconds = read_time_in_seconds();
  std::cout << "main: enumerate-models: " << cnf_models << " models, " << seconds-l_seconds << " seconds." << std::endl << std::endl;
  l_seconds = seconds;
#endif

  // create dtree
  std::cout << "main: making dtree ...";
  std::cout.flush();
  const dtree::Node *dt = 0;
  if( xml_dtree_file != "" )
    {
      std::ifstream ifs( xml_dtree_file.c_str() );
      dt = &dtree::readXML( ifs, *cnf_manager );
      ifs.close();
    }
  else
    {
      method = (method==-1?1:method);
      dtree::Method m = (method?dtree::Hypergraph:dtree::Naive);
      dt = dtree::make_dtree( *dtree_manager, m, *cnf_manager, (actions.size()==0?0:&actions) );
     }
  seconds = read_time_in_seconds();
  std::cout << " " << seconds-l_seconds << " seconds." << std::endl;
  if( output_dtree ) std::cout << *dt << std::endl;
  if( print_graphviz ) dtree::printGraphViz( std::cout, *dt );
  l_seconds = seconds;

  // label cnf clauses
  dt->label_cnf_clauses( *cnf_manager );

  // solve theory 
  if( call_solver )
    {
      int stat = cnf_manager->real_solve();
      seconds = read_time_in_seconds();
      std::cout << "main: solver = " << (stat==SATISFIABLE) << ", " << seconds-l_seconds << " seconds." << std::endl;
      cnf_manager->reset();
      cnf_manager->init_solve();
    }

  // output cnf
  if( output_cnf )
    {
      std::cout << *cnf_manager;
      cnf_manager->print_cls( std::cout );
      l_seconds = read_time_in_seconds();
    }

  // create ddnnf
  std::cout << "main: making ddnnf ...";
  std::cout.flush();
  nnf::node root = dt->make_ddnnf( *dtree_manager, *cnf_manager );
  nnf_manager->set_root( root );
  seconds = read_time_in_seconds();
  std::cout << " " << seconds-l_seconds << " seconds." << std::endl;
  l_seconds = seconds;

#if 1
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
      unsigned m = 1;
      for( size_t i = 0; i < nnf_manager->num_vars(); ++i )
	if( output[1+i] == -1 ) m = m<<1;
      std::cout << "main: #models = " << m*output[0] << std::endl;
      //for( size_t i = 0; i < nnf_manager->num_vars(); ++i )
      //std::cout << "main: #models with +v" << (1+i) << " = " << output[1+i] << std::endl;
      delete[] output;
      if( litmap ) free( litmap );
      std::cout << "main: #models in " << seconds-l_seconds << " seconds." << std::endl << std::endl;
      l_seconds = seconds;
    }
#endif

  // cleanup
  delete cnf_manager;
  //dt->clean_cache( *nnf_manager );
  delete dt;
  delete dtree_manager;
  nnf_manager->unregister_use( nnf_manager->root() );
  delete nnf_manager;

  // total time
  seconds = read_time_in_seconds();
  std::cout << std::endl
	    << "main: total time " << seconds-i_seconds << " seconds." << std::endl;
  return( 0 );
}
