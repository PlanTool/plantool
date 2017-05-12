#include "cnf.h"
#include "dtree.h"
#include "hgraph.h"

#include <stdlib.h>

extern "C" {
#include <hmetis.h>
};

#ifdef DEBUG
extern int verbosity_level;
#endif

namespace hgraph {

int ubfactor = 25;

void
Edge::printXML( std::ostream &os, int indent ) const
{
  for( int i = 0; i < indent; ++i ) os << ' ';
  os << "<edge>";

  for( const_iterator it = begin(); it != end(); ++it )
    os << "<node>" << *it << "</node>";

  os << "</edge>" << std::endl;
}

void
Graph::printXML( std::ostream &os, int indent ) const
{
  for( int i = 0; i < indent; ++i ) os << ' ';
  os << "<hgraph>" << std::endl;

  for( int i = 0; i < indent+2; ++i ) os << ' ';
  os << "<nodes>" << nodes_.size() << "</nodes>" << std::endl;

  for( LabelMap::const_iterator it = labels_.begin(); it != labels_.end(); ++it )
    {
      for( int i = 0; i < indent+2; ++i ) os << ' ';
      os << "<label><node>"
	 << (*it).first << "</node><value>"
	 << (*it).second << "</value></label>"
	 << std::endl;
    }

  for( int i = 0; i < indent+2; ++i ) os << ' ';
  os << "<edges>" << std::endl;

  for( EdgeList::const_iterator it = edges_.begin(); it != edges_.end(); ++it )
    (*it)->printXML( os, indent+4 );

  for( int i = 0; i < indent+2; ++i ) os << ' ';
  os << "</edges>" << std::endl;

  for( int i = 0; i < indent; ++i ) os << ' ';
  os << "</hgraph>" << std::endl;
}

dtree::Node*
Graph::make_dtree( const cnf::Manager &cnf_manager ) const
{
  dtree::Node *dt;
  if( nodes_.size() == 1 )
    {
      Node node = *nodes_.begin();
      LabelMap::const_iterator li = labels_.find( node );
      //const cnf::Formula *f = (const cnf::Formula*)(*li).second;
      dt = new dtree::Leaf( cnf_manager, (cnf::ClauseIndex)(*li).second );
    }
  else
    {
      Partition p_left, p_right;
      partition( p_left, p_right );

      const Graph *g_left = induced_subgraph( p_left );
      const Graph *g_right = induced_subgraph( p_right );

      dt = new dtree::Internal( g_left->make_dtree( cnf_manager ), g_right->make_dtree( cnf_manager ) );
      delete g_left;
      delete g_right;
    }
  return( dt );
}

const Graph*
Graph::induced_subgraph( const Partition &part ) const
{
  // create graph
  NodeList nodes;
  for( Partition::const_iterator ni = part.begin(); ni != part.end(); ++ni )
    nodes.add_node( *ni );
  Graph *graph = new Graph( nodes, labels_ );
  if( weighted_ ) graph->set_weighted();

  // add edges
  for( EdgeList::const_iterator ei = edges_.begin(); ei != edges_.end(); ++ei )
    {
      bool induced = true;
      for( Edge::const_iterator ni = (*ei)->begin(); ni != (*ei)->end(); ++ni )
	if( part.find( *ni ) == part.end() )
	  {
	    induced = false;
	    break;
	  }

      if( induced )
	{
	  Edge *edge = new Edge( *(*ei) );
	  if( !graph->add_edge( edge ) ) delete edge;
	}
    }

  return( graph );
}

void
Graph::partition( Partition &left, Partition &right ) const
{
  unsigned nodes = nodes_.size();
  unsigned edges = edges_.size();

  unsigned eindsz = 0;
  for( EdgeList::const_iterator ei = edges_.begin(); ei != edges_.end(); ++ei )
    eindsz += (*ei)->size();

  Node node = 0;
  LabelMap label, inv_label;
  for( Edge::const_iterator ni = nodes_.begin(); ni != nodes_.end(); ++ni )
    {
      label[*ni] = (const void*)node;
      inv_label[node] = (const void*)*ni;
      ++node;
    }

  // allocate space
  int *hewgts = (int*)(weighted_?malloc( edges * sizeof(int) ):0);
  int *eptr = (int*)malloc( (edges+1) * sizeof(int) );
  int *eind = (int*)malloc( eindsz * sizeof(int) );
  int *part = (int*)malloc( nodes * sizeof(int) );
  int *opts = (int*)calloc( 9, sizeof(int) );


  // fill structures
  int *p1 = eptr;
  int *p2 = eind;
  for( EdgeList::const_iterator ei = edges_.begin(); ei != edges_.end(); ++ei )
    {
      if( weighted_ ) hewgts[p1-eptr] = (*ei)->weight_;
      *p1++ = p2 - eind;
      for( Edge::const_iterator ni = (*ei)->begin(); ni != (*ei)->end(); ++ni )
	*p2++ = (int)label[*ni];
    }
  *p1 = p2 - eind;

  // call hmetis
#if 1
  opts[0] = 1;
  opts[1] = 10;
  opts[2] = 1;
  opts[3] = 1;
  opts[4] = 1;
#endif

#if 0
  if( weighted_ ) std::cout << "this is a weighted hypergraph" << std::endl;
  std::cout << "hmetis: call( nodes={ ";
  for( NodeList::const_iterator ni = nodes_.begin(); ni != nodes_.end(); ++ni )
    std::cout << (int)label[*ni] << " ";
  std::cout << "}, edges=[ ";
  for( EdgeList::const_iterator ei = edges_.begin(); ei != edges_.end(); ++ei )
    {
      std::cout << (*ei)->weight_ << ":{";
      for( Edge::const_iterator ni = (*ei)->begin(); ni != (*ei)->end(); ++ni )
	std::cout << (int)label[*ni] << " ";
      std::cout << "} ";
    }
  std::cout << "] )" << std::endl;
#endif

#if 1
  int ec = 0;
  HMETIS_PartRecursive( nodes,    // number of nodes
			edges,    // number of edges
			hewgts,   // weight of nodes (NULL if unweighted)
			eptr,     // description of edges
			eind,
			NULL,     // weight of edges (NULL if unweighted)
			2,        // number of desired partitions
			ubfactor, // ubfactor
			opts,     // options
			part,     // output 
			&ec );    // number of edges being cut
#else
  throw 1;
#endif

  // decode hmetis output
  for( unsigned i = 0; i < nodes; ++i )
    {
      if( part[i] )
	left.insert( (Node)inv_label[i] );
      else
	right.insert( (Node)inv_label[i] );
    }

  // fix possible bug in hmetis
  while( (left.size() == 0) || (right.size() == 0) )
    {
#ifdef DEBUG
      if( verbosity_level > 0 )
	std::cout << "re-shuffling graph=" << this << ", nodes=" << nodes << std::endl;
#endif
      left.clear();
      right.clear();
      for( unsigned i = 0; i < nodes; ++i )
	if( drand48() < .5 )
	  left.insert( (Node)inv_label[i] );
	else
	  right.insert( (Node)inv_label[i] );
    }

  // deallocate resources
  if( weighted_ ) free( hewgts );
  free( eptr );
  free( eind );
  free( part );
  free( opts );
}

}; // namespace hgraph
