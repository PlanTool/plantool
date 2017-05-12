#include "cnf.h"
#include "nnf.h"
#include "satlib.h"

#include <stdio.h>
#include <ctype.h>
#include <fstream>
#include <string>

#include <list>
#include <map>
#include <set>
#include <vector>

#ifdef DEBUG
extern int verbosity_level;
#endif

namespace satlib {

std::map<int,const std::vector<int>*> clauses_;

bool
read_line( std::istream &is, std::string &line )
{
  char c;
  line.clear();
  while( is.get( c ) )
    {
      if( c == '\n' ) return( true );
      line += c;
    }
  return( false );
}

void
read_cnf_file( std::istream &is, CSolver &manager )
{
  int numvars, numclauses, n = 0;
  std::string line;

  while( read_line( is, line ) )
    {
      if( line[0] == 'c' )
	continue;
      else if( line[0] == 'p' )
	{
	  sscanf( line.c_str(), "p cnf %d %d", &numvars, &numclauses );
#ifdef DEBUG
	  if( verbosity_level >= 100 )
	    {
	      std::cout << "satlib: number variables = " << numvars << "." << std::endl;
	      std::cout << "satlib: number clauses = " << numclauses << "." << std::endl;
	    }
#endif
	  manager.set_variable_number( numvars );
	}
      else if( line[0] == '%' )
	break;
      else
	{
	  // read clause
	  ++n;
	  std::set<int> clause_vars;
	  std::set<int> clause_lits;
	  char *str = strdup( line.c_str() );
	  char *ptr = strtok( str, " " );
	  do {
	    int lit = atoi( ptr );
	    if( lit != 0 )
	      {
		cnf::VarIndex var = (lit<0?-lit:lit);
		int sign = lit < 0;
		clause_vars.insert( var );
		clause_lits.insert( (var<<1) + sign );
	      }
	  } while( (ptr = strtok( NULL, " " )) );
	  free( str );

	  // add clause
	  if( (clause_vars.size() > 0) )//&& (clause_vars.size() == clause_lits.size()) )
	    {
	      std::vector<int> *tmp = new std::vector<int>;
	      for( std::set<int>::iterator it = clause_lits.begin(); it != clause_lits.end(); ++it )
		tmp->push_back( *it );
	      int index = manager.add_orig_clause( &tmp->begin()[0], tmp->size(), 0 );
	      clauses_[index] = tmp;
	    }
	  clause_lits.clear();
	  clause_vars.clear();
	}
    }

#ifdef DEBUG
  if( verbosity_level >= 100 )
    std::cout << "satlib: clauses read = " << n << "." << std::endl;
#endif
  if( n != numclauses ) throw 1;
}

nnf::node
generate_nnf_node( nnf::Manager &manager, int type,
		   std::pair<nnf::node,bool> *nodes,
		   const std::vector<unsigned> &children )
{
  nnf::node n = nnf::null_node;
  for( int i = (int)children.size()-1; i >= 0; --i )
    {
      assert( nodes[children[i]].first != nnf::null_node );
      if( n == nnf::null_node )
	{
	  n = nodes[children[i]].first;
	  if( nodes[children[i]].second ) manager.register_use( n );
	  nodes[children[i]].second = true;
	}
      else if( type == 0 )
	{
	  nnf::node tmp = nnf::make_and( &manager, nodes[children[i]].first, n );
	  if( !nodes[children[i]].second ) manager.unregister_use( nodes[children[i]].first );
	  manager.unregister_use( n );
	  nodes[children[i]].second = true;
	  n = tmp;
	}
      else
	{
	  nnf::node tmp = nnf::make_or( &manager, nodes[children[i]].first, n );
	  if( !nodes[children[i]].second ) manager.unregister_use( nodes[children[i]].first );
	  manager.unregister_use( n );
	  nodes[children[i]].second = true;
	  n = tmp;
	}
    }
  return( n );
}

void
read_nnf_file( std::istream &is, nnf::Manager &manager, std::multimap<int,nnf::node> *olabels )
{
  unsigned n = 0;
  int num_vars, num_nodes, num_edges;
  std::string line;

  std::pair<nnf::node,bool> *nodes = 0;
  std::list<std::pair<unsigned,std::vector<unsigned> > > pending;
  std::map<unsigned,int> labels;

  while( read_line( is, line ) )
    {
      if( line[0] == 'c' )
	continue;
      else if( line[0] == 'n' )
	{
	  sscanf( line.c_str(), "nnf %d %d %d", &num_nodes, &num_edges, &num_vars );
	  manager.allocate_space( num_nodes );
	  manager.set_num_vars( num_vars );
	  manager.set_inc_rate( 1.01 );
	  nodes = new std::pair<nnf::node,bool>[num_nodes];
	  for( int i = 0; i < num_nodes; ++i )
	    nodes[i] = std::make_pair( nnf::null_node, false );
	}
      else if( line[0] == '%' )
	break;
      else
	{
	  // read line
	  unsigned id = n++;
	  char *str = strdup( line.c_str() );
	  char *ptr = strtok( str, " " );
	  char type = toupper(*ptr);
	  ptr = strtok( NULL, " " );
	  if( type == 'L' )
	    {
	      int lit = atoi( ptr );
	      assert( lit != 0 );
	      unsigned var = (lit > 0 ? (lit<<1) : ((-lit)<<1)+1);
	      nnf::node n = nnf::make_variable( &manager, var );
	      nodes[id] = std::make_pair( n, false );
	    }
	  else if( (type == 'A') || (type == 'O') )
	    {
	      int label;
	      if( type == 'O' )
	        {
		  type = 1;
		  label = atoi( ptr );
		  if( olabels ) labels[id] = label;
	          ptr = strtok( NULL, " " );
		}
	      else
	        type = 0;
	      size_t sz = atoi( ptr );
	      ptr = strtok( NULL, " " );
	      std::vector<unsigned> children;
	      for( size_t i = 0; i < sz; ++i )
		{
		  children.push_back( (unsigned)atoi( ptr ) );
		  ptr = strtok( NULL, " " );
		}
	      assert( ptr == NULL );
	      pending.push_front( std::make_pair( (id<<1)+type, children ) );
	    }

	  // process pending nodes
	  while( !pending.empty() )
	    {
	      std::pair<unsigned,std::vector<unsigned> > e = pending.front();
	      std::vector<unsigned>::const_iterator it;
	      for( it = e.second.begin(); it != e.second.end(); ++it )
		if( nodes[*it].first == nnf::null_node ) break;
	      if( it != e.second.end() ) break;

	      pending.pop_front();
	      nnf::node n = generate_nnf_node( manager, e.first%2, nodes, e.second );
	      nodes[e.first>>1] = std::make_pair( n, false );
	      if( olabels && (e.first%2 == 1) )
	        {
		  //std::cout << "nnf " << (e.first>>1) << ":" << labels[e.first>>1] << " --> " << n << std::endl;
	          olabels->insert( std::make_pair(labels[e.first>>1],n) );
		}
	    }
	}
    }

 loop:
  std::list<std::pair<unsigned,std::vector<unsigned> > >::iterator it;
  for( it = pending.begin(); it != pending.end(); ++it )
    {
      std::pair<unsigned,std::vector<unsigned> > e = *it;
      std::vector<unsigned>::const_iterator jt;
      for( jt = e.second.begin(); jt != e.second.end(); ++jt )
	if( nodes[*jt].first == nnf::null_node ) break;
      if( jt != e.second.end() ) continue;

      nnf::node n = generate_nnf_node( manager, e.first%2, nodes, e.second );
      nodes[e.first>>1] = std::make_pair( n, false );
      pending.erase( it );
      goto loop;
    }

  manager.set_sorted();
  assert( pending.size() == 0 );
  size_t root_ref = nodes[n-1].first;
  delete[] nodes;
  manager.set_root( root_ref );
}

}; // satlib namespace
