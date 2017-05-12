#ifndef NNF_INCLUDE
#define NNF_INCLUDE

#include "cnf.h"
#include "hashing.h"

#include <stdlib.h>
#include <iostream>
#include <list>
#include <map>
#include <set>
#include <string>
#include <vector>

#define MIN(a,b)    ((a)>(b)?(b):(a))
#define MAX(a,b)    ((a)>(b)?(a):(b))

namespace nnf
{
  class Node;
  class Manager;
  typedef size_t node;
  const node null_node = (node)-1;
  class cache
  {
  public:
    mutable unsigned lookups_, hits_;
    cache() : lookups_(0), hits_(0) { }
    ~cache() { }
    unsigned lookups( void ) const { return( lookups_ ); }
    unsigned hits( void ) const { return( hits_ ); }
    float hit_rate( void ) const { return( (float)hits_/(float)lookups_ ); }
  };

  class cache1 : public cache, public hashing::hash_map<unsigned,unsigned> { };
  class cache2 : public cache, public std::map<std::pair<unsigned,unsigned>,unsigned> { };
  class cacheS : public cache, public hashing::hash_map<unsigned,node> { };

  class Model : public std::set<int>
  {
  public:
    void printXML( std::ostream &os, int indent = 0 ) const;
  };
  class ModelList : public std::vector<Model>
  {
  public:
    void printXML( std::ostream &os, int indent = 0 ) const;
  };

  class Node;
  class NodeList : public std::list<Node*> { };
  class NodeVector : public std::vector<node> { };
  enum NodeType { Null, And, Or, Value, Variable };

  class NodeCache
  {
  public:
    bool used_;
    const void *first_;
    const void *second_;
    NodeCache() : used_(false), first_(0), second_(0) { }
    void set( bool used, const void *first )
    {
      used_ = used;
      first_ = first;
    }
    void set( bool used, const void *first, const void *second )
    {
      used_ = used;
      first_ = first;
      second_ = second;
    }
  };

  class Node
  {
  public:
    NodeType type_;
    node left_;
    node right_;
    mutable NodeVector *parents_;
    mutable unsigned ref_;
    mutable NodeCache cache_;

    Node() : type_(Null), left_(0), right_(0), parents_(0), ref_(0) { }
    ~Node() { }
    size_t count_cnf_clauses( const Manager &man ) const;
    void dump_cnf( std::ostream &os, const Manager &man, bool flag, bool satlib_fmt, const char **names ) const;
    void printXML( const Manager &man, std::ostream &os, int indent = 0 ) const;
    void printGraphViz( const Manager &man, std::ostream &os ) const;
  };

  class Manager
  {
    size_t num_vars_;
    size_t num_;
    size_t size_;
    NodeList free_list_;
    mutable unsigned ref_count_;
    mutable float inc_rate_;

    bool sorted_;
    bool smooth_;

    cache2 or_cache_;
    cache2 and_cache_;
    cache1 var_cache_;

    node root_;
    node false_node_;
    node true_node_;

  protected:
    Node *nodes_;

    friend class Node;
    friend node make_and( Manager *man, node left, node right );
    friend node make_or( Manager *man, node left, node right );
    friend node make_value( Manager *man, bool value );
    friend node make_variable( Manager *man, unsigned var );
    friend node make_not( Manager *man, node nref );
    friend node push_disjunctions( Manager *man, node nref );
    friend node time_shift( Manager *man, node nref, int offset );
    friend void dump_cnf( const Manager *man, std::ostream &os, node nref, bool satlib_fmt, const char **names );

  public:
    Manager( size_t num_vars = 0, size_t space = 0 )
      : num_vars_(num_vars), num_(0), size_(0), ref_count_(0), inc_rate_(1.5), sorted_(false), smooth_(false), root_(0), nodes_(0)
    {
      if( space > 0 ) allocate_space( space );
      false_node_ = get_node( Value, false ) - nodes_;
      true_node_ = get_node( Value, true ) - nodes_;
      std::cout << "nnf: new id=" << this << "." << std::endl;
    }
    ~Manager()
    {
      unregister_use( &nodes_[false_node_] );
      unregister_use( &nodes_[true_node_] );
      std::cout << "nnf: delete id=" << this << ", ref_count=" << ref_count_ << "." << std::endl;
      free( nodes_ );
    }

    size_t num( void ) const { return( num_ ); }
    size_t size( void ) const { return( size_ ); }
    size_t num_vars( void ) const { return( num_vars_ ); }
    void set_num_vars( size_t num_vars ) { num_vars_ = num_vars; }
    bool sorted( void ) const { return( sorted_ ); }
    void set_sorted( void ) { sorted_ = true; }
    void set_smooth( void ) { smooth_ = true; }
    void set_root( node root ) { root_ = root; }
    node root( void ) const { return( root_ ); }
    float set_inc_rate( float inc_rate )
    {
      float rate = inc_rate_;
      inc_rate_ = MAX(1.0,inc_rate);
      return( rate );
    }

    bool verify_integrity( void ) const;
    bool verify_sort( void ) const;
    unsigned ref( node nref ) const { return( nodes_[nref].ref_ ); }
    void register_use( const Node *n )
    {
      ++n->ref_;
      ++ref_count_;
    }
    void unregister_use( const Node *n )
    {
      assert( n->ref_ > 0 );
      assert( ref_count_ > 0 );
      --ref_count_;
      if( --n->ref_ == 0 )
	{
	  remove_cache( n );
	  return_node( n );
	}
    }
    void register_use( node nref ) { register_use( &nodes_[nref] ); }
    void unregister_use( node nref ) { unregister_use( &nodes_[nref] ); }

    void allocate_space( size_t req = 1 )
    {
      if( num_ + req > size_ )
	{
	  size_t oldsz = size_;
	  if( req > 1 )
	    size_ = req;
	  else if( !size_ )
	    size_ = 1024;
	  else if( (size_t)(size_*inc_rate_) == size_ )
	    ++size_;
	  else
	    size_ = (size_t)(size_*inc_rate_);
	  nodes_ = (Node*)realloc( nodes_, size_ * sizeof(Node) );
	  memset( &nodes_[oldsz], 0, (size_-oldsz) * sizeof(Node) );
	}
      assert( size_ > num_ );
    }

    void return_node( const Node *n )
    {
      if( (n->type_ == And) || (n->type_ == Or) )
	{
	  unregister_use( n->left_ );
	  unregister_use( n->right_ );
	}
      free_list_.push_back( (Node*)n );
    }

    Node* get_node( void )
    {
      Node *n;
      if( true || free_list_.empty() )
	{
	  allocate_space();
	  n = &nodes_[num_++];
	  n->parents_ = 0;
	}
      else
	{
	  std::cout << "getting from free-list" << std::endl;
	  n = free_list_.front();
	  free_list_.pop_front();
	}
      assert( n->ref_ == 0 );
      register_use( n );
      if( !n->parents_ ) n->parents_ = new NodeVector;
      n->parents_->clear();
      return( n );
    }

    const Node* get_node( NodeType type, unsigned value )
    {
      Node *n = get_node();
      n->type_ = type;
      n->left_ = (node)value;
      n->right_ = 0;
      return( n );
    }

    const Node* get_node( NodeType type, node left, node right )
    {
      assert( (type == And) || (type == Or) );
      Node *n = get_node();
      n->type_ = type;
      n->left_ = MIN(left,right);
      n->right_ = MAX(left,right);
      nodes_[left].parents_->reserve( 1 + nodes_[left].parents_->size() );
      nodes_[left].parents_->push_back( n - nodes_ );
      nodes_[right].parents_->reserve( 1 + nodes_[right].parents_->size() );
      nodes_[right].parents_->push_back( n - nodes_ );
      register_use( left );
      register_use( right );
      return( n );
    }

    node find_cache( NodeType type, unsigned variable ) const
    {
      assert( type == Variable );
      cache1::const_iterator it = var_cache_.find( variable );
      if( it != var_cache_.end() )
	return( (node)(*it).second );
      else
	return( null_node );
    }

    node find_cache( NodeType type, node left, node right ) const
    {
      assert( (type == And) || (type == Or) );
      const cache2 *cache = (type==And?&and_cache_:&or_cache_);
      node l = MIN(left,right);
      node r = MAX(left,right);
      cache2::const_iterator it = cache->find( std::make_pair(l,r) );
      if( it != cache->end() )
	return( (node)(*it).second );
      else
	return( null_node );
    }
    void insert_cache( const Node *n )
    {
      assert( (n->type_ == And) || (n->type_ == Or) || (n->type_ == Variable) );
      if( n->type_ == Variable )
	var_cache_.insert( std::make_pair( n->left_, n - nodes_ ) );
      else
	{
	  cache2 *cache = (n->type_==And?&and_cache_:&or_cache_);
	  cache->insert( std::make_pair( std::make_pair(n->left_,n->right_), n - nodes_ ) );
	}
    }
    void remove_cache( const Node *n )
    {
      if( n->type_ == Variable )
	var_cache_.erase( (unsigned)n );
      else if( (n->type_ == And) || (n->type_ == Or) )
	{
	  cache2 *cache = (n->type_==And?&and_cache_:&or_cache_);
	  cache->erase( std::make_pair(n->left_,n->right_) );
	}
    }
    void clean_node_cache( void ) const;

    void variables( const Node *n ) const;
    void clean_variables( const Node *n ) const;
    void topological_sort( const Node *n, const Node **base, const Node** &ptr ) const;
    void topological_sort( const Node *n, const Node* &prev ) const;
    void make_sorted( void );
    void make_smooth( void );

    node true_node( void ) const { return( true_node_ ); }
    node false_node( void ) const { return( false_node_ ); }
    bool is_constant_false( node n ) const { return( n == false_node_ ); }
    bool is_constant_true( node n ) const { return( n == true_node_ ); }

    size_t depth( node nref = null_node ) const;
    size_t depth_recursive( const Node *n ) const;
    size_t count_nodes( node nref = null_node ) const;
    size_t count_nodes_recursive( const Node *n ) const;
    size_t count_edges( node nref = null_node ) const;
    size_t count_edges_recursive( const Node *n ) const;

    bool satisfiable( node nref = null_node ) const;
    bool satisfiable_recursive( const Node *n ) const;

    float count_models( float *output = 0, const int *litmap = 0, const int *vapmap = 0, node nref = null_node ) const;
    float count_models_sorted_smooth( const Node *n, float *output, const int *litmap, const int *varmap ) const;
    float max_count_models( const int *litmap, const int *nodemap ) const;
    float max_count_models_aux( const Node *n, const int *litmap, const int *nodemap, bool mode ) const;

    void enumerate_models( ModelList &models, node nref = null_node ) const;
    const ModelList* enumerate_models_recursive( const Node *n ) const;

    void project( const int *varmap, node nref = null_node, const std::set<node> *marked = 0, std::map<node,node> *remap = 0 );
    node project_aux( const int *varmap, node nref );

    void dump( std::ostream &os ) const;
    void printXML( std::ostream &os, node nref = null_node, int indent = 0 ) const
    {
      if( nref == null_node ) nref = root_;
      nodes_[nref].printXML( *this, os, indent );
    }
    void printGraphViz( std::ostream &os, node nref = null_node ) const
    {
      if( nref == null_node ) nref = root_;
      os << "digraph G {" << std::endl;
      nodes_[nref].printGraphViz( *this, os );
      os << "};" << std::endl;
      clean_node_cache();
    }
  };

  inline node make_and( Manager *man, node left, node right )
  {
    if( man->is_constant_false( left ) )
      {
	man->register_use( left );
	return( left );
      }
    else if( man->is_constant_true( left ) )
      {
	man->register_use( right );
	return( right );
      }
    else if( man->is_constant_false( right ) )
      {
	man->register_use( right );
	return( right );
      }
    else if( man->is_constant_true( right ) )
      {
	man->register_use( left );
	return( left );
      }
    else
      {
	// check cache
	node nref = man->find_cache( And, left, right );
	if( nref != null_node )
	  man->register_use( nref );
	else
	  {
	    const Node *n = man->get_node( And, left, right );
	    man->insert_cache( n );
	    nref = n - man->nodes_;
	  }
	return( nref );
      }
  }

  inline node make_or( Manager *man, node left, node right )
  {
    if( man->is_constant_true( left ) )
      {
	man->register_use( left );
	return( left );
      }
    else if( man->is_constant_false( left ) )
      {
	man->register_use( right );
	return( right );
      }
    else if( man->is_constant_true( right ) )
      {
	man->register_use( right );
	return( right );
      }
    else if( man->is_constant_false( right ) )
      {
	man->register_use( left );
	return( left );
      }
    else
      {
	// check cache
	node nref = man->find_cache( Or, left, right );
	if( nref != null_node )
	  man->register_use( nref );
	else
	  {
	    const Node *n = man->get_node( Or, left, right );
	    man->insert_cache( n );
	    nref = n - man->nodes_;
	  }
	return( nref );
      }
  }

  node make_not( Manager *man, node n );

  inline node make_value( Manager *man, bool value )
  {
    if( value )
      {
	man->register_use( man->true_node() );
	return( man->true_node() );
      }
    else
      {
	man->register_use( man->false_node() );
	return( man->false_node() );
      }
  }

  inline node make_variable( Manager *man, unsigned var )
  {
    node nref = man->find_cache( Variable, var );
    if( nref != null_node )
      man->register_use( nref );
    else
      {
	const Node *n = man->get_node( Variable, var );
	man->insert_cache( n );
	nref = n - man->nodes_;
      }
    return( nref );
  }

  node make_not( Manager *man, node nref );
  node push_disjunctions( Manager *man, node nref );
  inline node make_cnf( Manager *man, node nref )
  {
    return( push_disjunctions( man, nref ) );
  }

  node time_shift( Manager *man, node nref, int offset = 0 );
  void dump_cnf( const Manager *man, std::ostream &os, node nref = null_node, bool satlib_fmt = true, const char **names = 0 );

  node readXML( std::istream &is, Manager &manager );

}; // nnf namespace

inline std::ostream& operator<<( std::ostream &os, const nnf::ModelList &models )
{
  models.printXML( os );
  return( os );
}

#endif // NNF_INCLUDE
