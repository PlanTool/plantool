#ifndef HGRAPH_INCLUDE
#define HGRAPH_INCLUDE

#include <iostream>
#include <list>
#include <vector>
#include <set>

// forward references
namespace cnf { class Manager; };
namespace dtree { class Manager; class Node; };

namespace hgraph {

  extern int ubfactor;
  typedef unsigned Node;
  class Partition : public std::set<Node> { };
  class LabelMap : public std::map<Node,const void*> { };
  class Edge : public std::set<Node>
  {
  public:
    unsigned weight_;

    Edge() : weight_(1) { }
    void printXML( std::ostream &os, int indent = 0 ) const;
    void add_node( Node node ) { insert( node ); }
    void set_weight( unsigned weight ) { weight_ = weight; }
  };
  class EdgeList : public std::list<const Edge*> { };
  class NodeList : public Edge { };

  class Graph
  {
    bool weighted_;
    NodeList nodes_;
    EdgeList edges_;
    const LabelMap &labels_;

  public:
    Graph( const NodeList &nodes, const LabelMap &labels ) : weighted_(false), nodes_(nodes), labels_(labels) { }
    ~Graph()
    {
      for( EdgeList::const_iterator it = edges_.begin(); it != edges_.end(); ++it )
	delete *it;
    }

    void set_weighted( void ) { weighted_ = true; }
    bool add_edge( const Edge *edge )
    {
      for( EdgeList::iterator it = edges_.begin(); it != edges_.end(); ++it )
	if( *(*it) == *edge ) return( false );
      edges_.push_back( edge );
      return( true );
    }
    void printXML( std::ostream &os, int indent = 0 ) const;
    dtree::Node* make_dtree( const cnf::Manager &cnf_manager ) const;
    const Graph* induced_subgraph( const Partition &part ) const;
    void partition( Partition &left, Partition &right ) const;
  };

}; // namespace hgraph

inline std::ostream& operator<<( std::ostream &os, const hgraph::Graph &g )
{
  g.printXML( os );
  return( os );
}

#endif // HGRAPH_INCLUDE
