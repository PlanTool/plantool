#ifndef DTREE_INCLUDE
#define DTREE_INCLUDE

#include "cnf.h"
#include "nnf.h"
#include <iostream>
#include <vector>

namespace dtree
{
  enum Method { Naive = 0, Hypergraph = 1 };

  class Manager
  {
    nnf::Manager &manager_;
    nnf::cacheS cache_;
    std::stack<std::pair<int,nnf::node> > stack_;

  public:
    Manager( nnf::Manager &manager ) : manager_(manager)
    {
      std::cout << "dtree: new id=" << this << "." << std::endl;
    }
    ~Manager()
    {
#if 0
      std::cout << "dtree: delete id=" << this
		<< ", cache: size=" << cache_.size()
		<< ", lookups=" << cache_.lookups()
		<< ", hits=" << cache_.hits()
		<< ", hit-rate=" << 100*cache_.hit_rate() << "%"
		<< "." << std::endl;
#endif

      for( nnf::cacheS::const_iterator it = cache_.begin(); it != cache_.end(); ++it )
	{
	  //std::cout << "cleaning cache-entry: " << (*it).first << " <==> " << (*it).second << std::endl;
	  manager_.unregister_use( (*it).second );
	}
    }
    nnf::Manager& operator()( void ) { return( manager_ ); }

    // cache
    nnf::node lookup_cache( unsigned key ) const
    {
      ++cache_.lookups_;
      nnf::cacheS::const_iterator it = cache_.find( key );
      if( it != cache_.end() )
	{
	  ++cache_.hits_;
	  return( (*it).second );
	}
      else
	return( nnf::null_node );
    }
    bool insert_cache( unsigned key, nnf::node alpha )
    {
      nnf::node cached = lookup_cache( key );
      if( cached == nnf::null_node )
	{
	  cache_.insert( std::make_pair( key, alpha ) );
	  manager_.register_use( alpha );
	  return( true );
	}
      return( false );
    }
    void insert_cache( const cnf::VarList &context, const cnf::Manager &cnf_manager, nnf::node alpha );

    void push_stack( int level, nnf::node node ) { stack_.push( std::make_pair( level, node ) ); }
    nnf::node pop_stack( int level )
    {
      nnf::node node = nnf::null_node;
      if( (stack_.size() > 0) && (stack_.top().first == level) )
	{
	  node = stack_.top().second;
	  stack_.pop();
	}
      return( node );
    }
  };

  class Node
  {
  protected:
    int label_;
    unsigned depth_;
    mutable int dlevel_;
    cnf::ClauseList clauses_;

    cnf::VarList variables_;
    cnf::VarList separator_;
    cnf::VarList context_;

    mutable nnf::cacheS cache_;

    virtual void compute_meta_data( int &label, unsigned depth, cnf::VarList cutset ) = 0;
    virtual int width_aux( void ) const = 0;
    virtual nnf::node make_ddnnf_aux( Manager &dtree_manager, cnf::Manager &cnf_manager ) const = 0;

  public:
    Node() : label_(0), depth_(0), dlevel_(0) { }
    virtual ~Node()
    {
#if 0
      std::cout << "dtree: delete id=" << this
		<< ", cache: size=" << cache_.size()
		<< ", lookups=" << cache_.lookups()
		<< ", hits=" << cache_.hits()
		<< ", hit-rate=" << 100*cache_.hit_rate() << "%"
		<< "." << std::endl;
#endif
#if 0
      for( nnf::cacheS::const_iterator it = cache_.begin(); it != cache_.end(); ++it )
	nnf::Node::unregister_use( (*it).second );
#endif
    }

#if 1
    nnf::node lookup_cache( unsigned key ) const
    {
      ++cache_.lookups_;
      nnf::cacheS::const_iterator it = cache_.find( key );
      if( it != cache_.end() )
	{
	  ++cache_.hits_;
	  return( (*it).second );
	}
      else
	return( nnf::null_node );
    }
    bool insert_cache( unsigned key, nnf::node alpha ) const
    {
      nnf::node cached = lookup_cache( key );
      if( cached == nnf::null_node )
	{
	  cache_.insert( std::make_pair( key, alpha ) );
	  //nnf::Node::register_use( alpha );
	  return( true );
	}
      return( false );
    }
    void insert_cache( const cnf::Manager &cnf_manager, nnf::node alpha ) const;
    virtual void clean_cache( nnf::Manager &nnf_manager ) const = 0;
#endif

    const cnf::VarList& variables( void ) const { return( variables_ ); }
    const cnf::ClauseList& clauses( void ) const { return( clauses_ ); }
    const cnf::VarList& separator( void ) const { return( separator_ ); }
    const cnf::VarList& context( void ) const { return( context_ ); }
    int label( void ) const { return( label_ ); }
    unsigned depth( void ) const { return( depth_ ); }
    int dlevel( void ) const { return( dlevel_ ); }
    void set_label( int label ) { label_ = label; }
    void set_depth( int depth ) { depth_ = depth; }
    void set_dlevel( int dlevel ) const { dlevel_ = dlevel; }
    virtual size_t dump( std::ostream &os, size_t index = 0 ) const = 0;
    virtual void printXML( std::ostream &os, int indent = 0 ) const;
    virtual void printGraphViz( std::ostream &os ) const = 0;
    virtual void label_cnf_clauses( cnf::Manager &cnf_manager ) const = 0;

    int width( void ) const { return( width_aux() - 1 ); }
    nnf::node make_ddnnf( Manager &dtree_manager, const cnf::Manager &cnf_manager ) const;

    friend class Internal;
    friend void case_analysis_continuation( std::pair<const Node*,Manager*> *ctx, cnf::Manager *cnf_manager );
    friend const Node* make_dtree( Manager &manager, Method method, const cnf::Manager &cnf_manager, const cnf::VarList *weak_vars );
    friend Node& readXML( const XML::Node *xml_input, cnf::Manager &cnf_manager );
  };

  class Internal : public Node
  {
    Node *left_;
    Node *right_;

    nnf::node case_analysis( Manager &dtree_manager, cnf::Manager &cnf_manager ) const;
    nnf::node manual_case_analysis_aux( Manager &dtree_manager, cnf::Manager &cnf_manager, int depth ) const;
    nnf::node manual_case_analysis( Manager &dtree_manager, cnf::Manager &cnf_manager ) const;

  protected:
    virtual void compute_meta_data( int &label, unsigned depth, cnf::VarList cutset );
    virtual int width_aux( void ) const;
    virtual nnf::node make_ddnnf_aux( Manager &dtree_manager, cnf::Manager &cnf_manager ) const;

  public:
    Internal( Node *left, Node *right, bool fill = true ) : left_(left), right_(right)
    {
      if( fill )
	{
	  // variables
	  variables_.insert( variables_.end(), left_->variables().begin(), left_->variables().end() );
	  for( cnf::VarList::const_iterator vi = right_->variables().begin(); vi != right_->variables().end(); ++vi )
	    if( !variables_.find( *vi ) ) variables_.push_back( *vi );

	  // clauses
	  clauses_.insert( clauses_.end(), left_->clauses().begin(), left_->clauses().end() );
	  for( cnf::ClauseList::const_iterator ci = right_->clauses().begin(); ci != right_->clauses().end(); ++ci )
	    if( !clauses_.find( *ci ) ) clauses_.push_back( *ci );
	}
    }
    virtual ~Internal()
    {
      delete left_;
      delete right_;
    }

    virtual void clean_cache( nnf::Manager &nnf_manager ) const;
    const Node* left( void ) const { return( left_ ); }
    const Node* right( void ) const { return( right_ ); }
    virtual size_t dump( std::ostream &os, size_t index = 0 ) const;
    virtual void printXML( std::ostream &os, int indent = 0 ) const;
    virtual void printGraphViz( std::ostream &os ) const;
    virtual void label_cnf_clauses( cnf::Manager &cnf_manager ) const;
    nnf::node make_context_dnnf( Manager &dtree_manager, cnf::Manager &cnf_manager ) const;
  };

  class Leaf : public Node
  {
    cnf::ClauseIndex clause_;

  protected:
    virtual void compute_meta_data( int &label, unsigned depth, cnf::VarList cutset );
    virtual int width_aux( void ) const { return( context_.size() ); }
    virtual nnf::node make_ddnnf_aux( Manager &dtree_manager, cnf::Manager &cnf_manager ) const;

  public:
    Leaf( const cnf::Manager &cnf_manager, const cnf::ClauseIndex clause, bool fill = true ) : clause_(clause)
    {
      if( fill )
	{
	  cnf_manager.get_clause_vars( clause, variables_ );
	  clauses_.push_back( clause_ );
	}
    }
    virtual ~Leaf() { }

    virtual cnf::ClauseIndex clause( void ) const { return( clause_ ); }
    virtual void set_clause( cnf::ClauseIndex clause ) { clause_ = clause; }
    virtual void clean_cache( nnf::Manager &nnf_manager ) const;
    virtual size_t dump( std::ostream &os, size_t index = 0 ) const;
    virtual void printXML( std::ostream &os, int indent = 0 ) const;
    virtual void printGraphViz( std::ostream &os ) const;
    virtual void label_cnf_clauses( cnf::Manager &cnf_manager ) const;

    friend void case_analysis_continuation( std::pair<const Node*,Manager*> *ctx, cnf::Manager *cnf_manager );
  };

  void printGraphViz( std::ostream &os, const Node &node );
  const Node* make_dtree( Manager &manager, Method method, const cnf::Manager &cnf_manager, const cnf::VarList *weak_vars = 0 );
  Node& readXML( const XML::Node *xml_input, cnf::Manager &cnf_manager );
  const Node& readXML( std::istream &is, cnf::Manager &cnf_manager );
  
}; // dtree namespace

inline std::ostream& operator<<( std::ostream &os, const dtree::Node &dt )
{
  dt.printXML( os );
  return( os );
}

#endif // DTREE_INCLUDE
