#include "nnf.h"
#include "strxml.h"
#include <iostream>
#include <set>

#include <math.h>
#include <stdio.h>
#include <assert.h>

namespace nnf {

void
Model::printXML( std::ostream &os, int indent ) const
{
  for( int i = 0; i < indent; ++i ) os << ' ';
  os << "model: [ ";
  for( const_iterator it = begin(); it != end(); ++it )
    os << (*it%2?"-v":"+v") << (*it>>1) << ' ';
  os << "]" << std::endl;
}

void
ModelList::printXML( std::ostream &os, int indent ) const
{
  for( const_iterator it = begin(); it != end(); ++it )
    (*it).printXML( os, indent );
}

size_t
Node::count_cnf_clauses( const Manager &man ) const
{
  // ASSUMPTION: this nnf is actually a cnf
  size_t count = 0;
  switch( type_ )
    {
    case Null:
      break;
    case And:
      count += man.nodes_[left_].count_cnf_clauses( man );
      count += man.nodes_[right_].count_cnf_clauses( man );
      break;
    case Or:
    case Value:
    case Variable:
      ++count;
      break;
    }
  return( count );
}

void
Node::dump_cnf( std::ostream &os, const Manager &man, bool flag, bool satlib_fmt, const char **names ) const
{
  // ASSUMPTION: this nnf is actually a cnf
  switch( type_ )
    {
    case Null:
      break;
    case And:
      man.nodes_[left_].dump_cnf( os, man, flag, satlib_fmt, names );
      man.nodes_[right_].dump_cnf( os, man, flag, satlib_fmt, names );
      break;
    case Or:
      if( !flag && !satlib_fmt ) os << "[ ";
      man.nodes_[left_].dump_cnf( os, man, true, satlib_fmt, names );
      man.nodes_[right_].dump_cnf( os, man, true, satlib_fmt, names );
      if( !flag && !satlib_fmt ) os << "]" << std::endl;
      if( !flag && satlib_fmt ) os << "0" << std::endl;
      break;
    case Value:
      if( satlib_fmt )
	{
	}
      else
	{
	  if( !flag ) os << "[ ";
	  os << ((int)left_?"true":"false") << " ";
	  if( !flag ) os << "]" << std::endl;
	}
      break;
    case Variable:
      if( satlib_fmt )
	{
	  os << (((int)left_)%2?"-":"") << (((int)left_)>>1) << " ";
	  if( !flag ) os << "0" << std::endl;
	}
      else
	{
	  if( !flag ) os << "[ ";
	  if( names && names[((int)left_)>>1] )
	    os << (((int)left_)%2?"~":"") << names[((int)left_)>>1] << " ";
	  else
	    os << (((int)left_)%2?"~v":"v") << (((int)left_)>>1) << " ";
	  if( !flag ) os << "]" << std::endl;
	}
      break;
    }
}

void
Node::printXML( const Manager &man, std::ostream &os, int indent ) const
{
  for( int i = 0; i < indent; ++i ) os << ' ';
#if 0
  os << this << ":[";
  for( NodeVector::const_iterator it = parents_->begin(); it != parents_->end(); ++it )
    os << (*it) << " ";
  os << "]:";
#endif
  switch( type_ )
    {
    case Null:
      os << "</null>" << std::endl;
      break;
    case And:
      os << "<and>" << std::endl;
      man.nodes_[left_].printXML( man, os, indent+2 );
      man.nodes_[right_].printXML( man, os, indent+2 );
      for( int i = 0; i < indent; ++i ) os << ' ';
      os << "</and>" << std::endl;
      break;
    case Or:
      os << "<or>" << std::endl;
      man.nodes_[left_].printXML( man, os, indent+2 );
      man.nodes_[right_].printXML( man, os, indent+2 );
      for( int i = 0; i < indent; ++i ) os << ' ';
      os << "</or>" << std::endl;
      break;
    case Value:
      os << "<value>" << (int)left_ << "</value>" << std::endl;
      break;
    case Variable:
      os << "<variable>" << (((int)left_)%2?"~v":"v")
	 << (((int)left_)>>1) << "</variable>" << std::endl;
      break;
    }
}

void
Node::printGraphViz( const Manager &man, std::ostream &os ) const
{
  if( cache_.used_ ) return;
  switch( type_ )
    {
    case And:
    case Or:
      os << "  N_" << this << " -> N_" << &man.nodes_[left_] << ";" << std::endl;
      os << "  N_" << this << " -> N_" << &man.nodes_[right_] << ";" << std::endl;
      if( type_ == And )
        os << "  N_" << this << "[label=\"AND:" << this-man.nodes_ << "\"];" << std::endl;
      else
        os << "  N_" << this << "[label=\"OR:" << this-man.nodes_ << "\"];" << std::endl;
      man.nodes_[left_].printGraphViz( man, os );
      man.nodes_[right_].printGraphViz( man, os );
      break;
    case Variable:
      if( left_ % 2 )
        os << "  N_" << this << "[label=\"-v" << (left_>>1) << "\"];" << std::endl;
      else
        os << "  N_" << this << "[label=\"v" << (left_>>1) << "\"];" << std::endl;
      break;
    default:
      break;
    }
  cache_.used_ = true;
}

void
Manager::clean_node_cache( void ) const
{
  for( size_t i = 0; i < num_; ++i )
    nodes_[i].cache_.set( false, 0, 0 );
}

bool
Manager::verify_integrity( void ) const
{
  for( size_t i = 0; i < num_; ++i )
    {
      const Node *n = &nodes_[i];
      if( n->ref_ < n->parents_->size() )
	return( false );
      else
	{
	  for( size_t j = 0, sz = n->parents_->size(); j < sz; ++j )
	    if( (nodes_[(*n->parents_)[j]].left_ != i) && (nodes_[(*n->parents_)[j]].right_ != i) )
	      return( false );
	}
    }
  return( true );
}

bool
Manager::verify_sort( void ) const
{
  bool rv = true;
  for( size_t i = 0; i < num_; ++i )
    {
      Node *n = &nodes_[i];
      if( (n->type_ == And) || (n->type_ == Or) )
	{
	  assert( nodes_[n->left_].cache_.used_ );
	  assert( nodes_[n->right_].cache_.used_ );
	  if( !nodes_[n->left_].cache_.used_ || !nodes_[n->right_].cache_.used_ )
	    {
	      rv = false;
	      break;
	    }
	}
      n->cache_.used_ = true;
    }
  clean_node_cache();
  return( rv );
}

void
Manager::variables( const Node *n ) const
{
  if( n->cache_.first_ == (void*)0 )
    {
      size_t offset = (size_t)ceil( (float)num_vars() / (float)8 );
      if( (n->type_ == And) || (n->type_ == Or) )
	{
	  variables( &nodes_[n->left_] );
	  variables( &nodes_[n->right_] );
	  const char *lvars = (const char*)nodes_[n->left_].cache_.first_;
	  const char *rvars = (const char*)nodes_[n->right_].cache_.first_;
	  char *vars = (char*)calloc( offset, sizeof(char) );
	  for( size_t j = 0; j < num_vars(); ++j )
	    if( (lvars[j/8]&(1<<(j%8))) || (rvars[j/8]&(1<<(j%8))) )
	      vars[j/8] = vars[j/8] | (1<<(j%8));
          n->cache_.first_ = vars;
	}
      else if( n->type_ == Variable )
        {
	  char *vars = (char*)calloc( offset, sizeof(char) );
	  int var = (n->left_>>1) - 1;
	  vars[var/8] = vars[var/8] | (1<<(var%8));
          n->cache_.first_ = vars;
        }
    }
}

void
Manager::clean_variables( const Node *n ) const
{
  if( n->cache_.first_ != (void*)0 )
    {
      free( (char*)n->cache_.first_ );
      n->cache_.first_ = (void*)0;
      if( (n->type_ == And) || (n->type_ == Or) )
	{
	  clean_variables( &nodes_[n->left_] );
	  clean_variables( &nodes_[n->right_] );
	}
    }
}

void
Manager::topological_sort( const Node *n, const Node **base, const Node** &ptr ) const
{
  if( !n->cache_.used_ )
    {
      if( (n->type_ == And) || (n->type_ == Or) )
	{
	  topological_sort( &nodes_[n->left_], base, ptr );
	  topological_sort( &nodes_[n->right_], base, ptr );
	}
      n->cache_.set( true, (const void*)(ptr - base) );
      *ptr++ = n;
    }
}

void
Manager::topological_sort( const Node *n, const Node* &prev ) const
{
  if( !n->cache_.used_ )
    {
      // check that all parents have been visited
      for( size_t j = 0, sz = n->parents_->size(); j < sz; ++j )
	if( !nodes_[(*n->parents_)[j]].cache_.used_ ) return;

      // visit it and its descendants
      n->cache_.used_ = true;
      assert( (prev == 0) || prev->cache_.used_ );
      if( prev != 0 ) prev->cache_.first_ = (void*)n;
      n->cache_.second_ = prev;
      prev = n;
      if( (n->type_ == And) || (n->type_ == Or) )
	{
	  topological_sort( &nodes_[n->left_], prev );
	  topological_sort( &nodes_[n->right_], prev );
	}
    }
}

void
Manager::make_sorted( void )
{
#if 1
  // perform topological sort starting from the roots
  const Node **table = (const Node**)calloc( 1+num_, sizeof(const Node*) );
  const Node **ptr = table;
  for( size_t i = 0; i < num_; ++i )
    if( nodes_[i].parents_->size() == 0 )
      topological_sort( &nodes_[i], table, ptr );

  // relocate nodes
  root_ = (node)nodes_[root_].cache_.first_;
  Node *nodes = (Node*)calloc( size_, sizeof(const Node) );
  for( const Node **ptr = table; *ptr != 0; ++ptr )
    {
      Node *n = &nodes[ptr - table];
      (*ptr)->parents_->clear();
      *n = **ptr;
      if( ((*ptr)->type_ == And) || ((*ptr)->type_ == Or) )
	{
	  node left = (node)nodes_[(*ptr)->left_].cache_.first_;
	  node right = (node)nodes_[(*ptr)->right_].cache_.first_;
	  n->left_ = left;
	  n->right_ = right;
	  nodes[left].parents_->push_back( n - nodes );
	  nodes[right].parents_->push_back( n - nodes );
	}
    }

  // free resources
  free( table );
  free( nodes_ );
  free_list_.clear();
  nodes_ = nodes;
  clean_node_cache();
  sorted_ = true;
#else
  // trivial case
  if( num_ == 0 ) return;

  // perform topological sort starting from the roots
  Node *head;
  const Node *prev = 0;
  for( size_t i = 2; i < num_; ++i )
    if( nodes_[i].parents_->size() == 0 )
      {
	if( prev == 0 ) head = &nodes_[i];
	topological_sort( &nodes_[i], prev );
      }

  // reallocate nodes
  root_ = num_ - 1;
  Node *h = head;
  for( size_t i = num_ - 1; i >= 2; --i, h = (Node*)h->cache_.first_ )
    {
#if 0
      // check for cycles in linked list
      for( const Node *p = prev, *q = prev; q != 0; )
	{
	  q = (Node*)q->cache_.first_;
	  if( q == p ) std::cout << "cycle!" << std::endl;
	  if( q != 0 ) q = (Node*)q->cache_.first_;
	  if( q == p ) std::cout << "cycle!" << std::endl;
	  p = (Node*)p->cache_.first_;
	}
      std::cout << i << " " << h - nodes_ << std::endl;
      std::cout << "integrity = " << verify_integrity() << std::endl;
#endif

      Node *n = &nodes_[i];
      size_t j = (unsigned)(h - nodes_);
      if( h != n )
	{
	  // update linked list
	  if( (Node*)h->cache_.first_ == n )
	    {
	      if( h->cache_.second_ )
		((Node*)h->cache_.second_)->cache_.first_ = n;
	      if( n->cache_.first_ )
		((Node*)n->cache_.first_)->cache_.second_ = h;
	      h->cache_.first_ = n->cache_.first_;
	      n->cache_.first_ = h;
	      n->cache_.second_ = h->cache_.second_;
	      h->cache_.second_ = n;
	    }
	  else if( (Node*)n->cache_.first_ == h )
	    {
	      if( n->cache_.second_ )
		((Node*)n->cache_.second_)->cache_.first_ = h;
	      if( h->cache_.first_ )
		((Node*)h->cache_.first_)->cache_.second_ = h;
	      n->cache_.first_ = h->cache_.first_;
	      h->cache_.first_ = n;
	      h->cache_.second_ = n->cache_.second_;
	      n->cache_.second_ = h;
	    }
	  else
	    {
	      if( h->cache_.second_ )
		((Node*)h->cache_.second_)->cache_.first_ = n;
	      if( n->cache_.first_ )
		((Node*)n->cache_.first_)->cache_.second_ = h;
	      if( h->cache_.first_ )
		((Node*)h->cache_.first_)->cache_.second_ = n;
	      if( n->cache_.second_ )
		((Node*)n->cache_.second_)->cache_.first_ = h;
	      const void *tmp = h->cache_.first_;
	      h->cache_.first_ = n->cache_.first_;
	      n->cache_.first_ = tmp;
	      tmp = h->cache_.second_;
	      h->cache_.second_ = n->cache_.second_;
	      n->cache_.second_ = tmp;
	    }

	  // fix h's parents of children
	  if( (h->type_ == And) || (h->type_ == Or) )
	    {
	      for( size_t k = 0, sz = nodes_[h->left_].parents_->size(); k < sz; ++k )
		if( (*nodes_[h->left_].parents_)[k] == j )
		  {
		    (*nodes_[h->left_].parents_)[k] = i;
		    break;
		  }
	      for( size_t k = 0, sz = nodes_[h->right_].parents_->size(); k < sz; ++k )
		if( (*nodes_[h->right_].parents_)[k] == j )
		  {
		    (*nodes_[h->right_].parents_)[k] = i;
		    break;
		  }
	    }

	  // fix h's children of parents
	  for( size_t k = 0, sz = h->parents_->size(); k < sz; ++k )
	    {
	      node p = (*h->parents_)[k];
	      if( p == j ) p = i;
	      if( nodes_[p].left_ == j ) nodes_[p].left_ = i;
	      else if( nodes_[p].right_ == j ) nodes_[p].right_ = i;
	    }

	  // fix children of n
	  if( (n->type_ == And) || (n->type_ == Or) )
	    {
	      for( size_t k = 0, sz = nodes_[n->left_].parents_->size(); k < sz; ++k )
		if( (*nodes_[n->left_].parents_)[k] == i )
		  {
		    (*nodes_[n->left_].parents_)[k] = j;
		    break;
		  }
	      for( size_t k = 0, sz = nodes_[n->right_].parents_->size(); k < sz; ++k )
		if( (*nodes_[n->right_].parents_)[k] == i )
		  {
		    (*nodes_[n->right_].parents_)[k] = j;
		    break;
		  }
	    }

	  // fix n's children of parents
	  for( size_t k = 0, sz = n->parents_->size(); k < sz; ++k )
	    {
	      node p = (*n->parents_)[k];
	      if( p == i ) p = j;
	      if( nodes_[p].left_ == i ) nodes_[p].left_ = j;
	      else if( nodes_[p].right_ == i ) nodes_[p].right_ = j;
	    }

	  // swap other info
	  Node tmp1 = *n, tmp2 = *h, *th = h;
	  *n = *h;
	  *h = tmp1;
	  n->cache_ = tmp1.cache_;
	  h->cache_ = tmp2.cache_;
	  h = n;
	  n = th;

	  assert( verify_integrity() );

	  // set new beginning of linked list
	  if( !n->cache_.second_ ) head = n;
	  else if( !h->cache_.second_ ) head = h;
	}
    }

  // free resources
  clean_node_cache();
  sorted_ = true;
#endif
}

void
Manager::make_smooth( void )
{
  // allocate variable space
  size_t offset = (size_t)ceil( (float)num_vars() / (float)8 );
  char *variables = (char*)calloc( num_ * offset, sizeof(char) );

  // compute variables in each node
  for( size_t i = 0; i < num_; ++i )
    {
      Node *n = &nodes_[i];
      assert( n->cache_.first_ == (void*)0 );
      if( (n->type_ == And) || (n->type_ == Or) )
	{
	  assert( nodes_[n->left_].cache_.first_ != (void*)0 );
	  assert( nodes_[n->right_].cache_.first_ != (void*)0 );
	  const char *lvars = (const char*)nodes_[n->left_].cache_.first_;
	  const char *rvars = (const char*)nodes_[n->right_].cache_.first_;
	  char *vars = &variables[i*offset];
	  for( size_t j = 0; j < num_vars(); ++j )
	    if( (lvars[j/8]&(1<<(j%8))) || (rvars[j/8]&(1<<(j%8))) )
	      vars[j/8] = vars[j/8] | (1<<(j%8));
          n->cache_.first_ = vars;
	}
      else if( n->type_ == Variable )
        {
	  char *vars = &variables[i*offset];
	  int var = (n->left_>>1) - 1;
	  vars[var/8] = vars[var/8] | (1<<(var%8));
          n->cache_.first_ = vars;
        }
    }
  std::cout << "(phase-1) "; std::cout.flush();

  // smooth
  node alpha;
  for( size_t i = 0; i < num_; ++i )
    if( nodes_[i].type_ == Or )
      {
	// check if this node was inserted during the smooth
	if( nodes_[i].cache_.first_ == (void*)0 ) continue;

	// compute differences
	const char *lvars = (const char*)nodes_[nodes_[i].left_].cache_.first_;
	const char *rvars = (const char*)nodes_[nodes_[i].right_].cache_.first_;

	// replace left child with conjunction (wrt rdiff)
	alpha = null_node;
	for( size_t j = 0; j < num_vars(); ++j )
	  if( (rvars[j/8]&(1<<(j%8))) && !(lvars[j/8]&(1<<(j%8))) )
	    {
	      node pos = make_variable( this, (1+j)<<1 );
	      node neg = make_variable( this, ((1+j)<<1)+1 );
	      node tmp = make_or( this, pos, neg );
	      unregister_use( pos );
	      unregister_use( neg );
	      if( alpha == null_node )
		alpha = tmp;
	      else
		{
		  node t = make_and( this, tmp, alpha );
		  unregister_use( tmp );
		  unregister_use( alpha );
		  alpha = t;
		}
	    }

	// allocate new delta node, swap left and delta
	if( alpha != null_node )
	  {
	    std::cout << "hola" << std::endl;
	    node delta = make_and( this, alpha, nodes_[i].left_ );
	    unregister_use( alpha );
	    unregister_use( nodes_[i].left_ );

	    NodeVector::iterator pi = nodes_[nodes_[i].left_].parents_->begin();
	    for( ; (*pi) != i; ++pi );
	    assert( pi != nodes_[nodes_[i].left_].parents_->end() );
	    *pi = nodes_[nodes_[i].left_].parents_->back();
	    nodes_[nodes_[i].left_].parents_->pop_back();
	    nodes_[i].left_ = delta;
	    nodes_[delta].parents_->push_back( i );
	    sorted_ = false;
	  }

	// replace right child with conjunction (wrt ldiff)
	alpha = null_node;
	for( size_t j = 0; j < num_vars(); ++j )
	  if( (lvars[j/8]&(1<<(j%8))) && !(rvars[j/8]&(1<<(j%8))) )
	    {
	      node pos = make_variable( this, (1+j)<<1 );
	      node neg = make_variable( this, ((1+j)<<1)+1 );
	      node tmp = make_or( this, pos, neg );
	      unregister_use( pos );
	      unregister_use( neg );
	      if( alpha == null_node )
		alpha = tmp;
	      else
		{
		  node t = make_and( this, tmp, alpha );
		  unregister_use( tmp );
		  unregister_use( alpha );
		  alpha = t;
		}
	    }

	// allocate new delta node, swap left and delta
	if( alpha != null_node )
	  {
	    std::cout << "hola" << std::endl;
	    node delta = make_and( this, alpha, nodes_[i].right_ );
	    unregister_use( alpha );
	    unregister_use( nodes_[i].right_ );

	    NodeVector::iterator pi = nodes_[nodes_[i].right_].parents_->begin();
	    for( ; (*pi) != i; ++pi );
	    assert( pi != nodes_[nodes_[i].right_].parents_->end() );
	    *pi = nodes_[nodes_[i].right_].parents_->back();
	    nodes_[nodes_[i].right_].parents_->pop_back();
	    nodes_[i].right_ = delta;
	    nodes_[delta].parents_->push_back( i );
	    sorted_ = false;
	  }
      }

  smooth_ = true;
  std::cout << "(phase-2)"; std::cout.flush();

  // clean node cache
  clean_node_cache();
  free( variables );
}

size_t
Manager::depth( node nref ) const
{
  if( nref == null_node ) nref = root_;
  const Node *n = &nodes_[nref];
  size_t d = 0;
  if( !sorted_ )
    d = depth_recursive( n );
  else
    {
      for( size_t i = 0; i < num_; ++i )
	{
	  const Node *curr = &nodes_[i];
	  if( (curr->type_ == And) || (curr->type_ == Or) )
	    {
	      assert( (i > curr->left_) && (i > curr->right_) );
	      size_t n = 1+MAX((size_t)nodes_[curr->left_].cache_.first_,(size_t)nodes_[curr->right_].cache_.first_);
	      curr->cache_.set( true, (const void*)n );
	    }
	  else
	    curr->cache_.set( true, (const void*)0 );
	}
      d = (size_t)n->cache_.first_;
    }
  clean_node_cache();
  return( d );
}

size_t 
Manager::depth_recursive( const Node *n ) const
{
  if( n->cache_.used_ ) return( (unsigned)n->cache_.first_ );
  size_t depth = 0;
  if( (n->type_ == And) || (n->type_ == Or) )
    {
      size_t l = depth_recursive( &nodes_[n->left_] );
      size_t r = depth_recursive( &nodes_[n->right_] );
      depth = 1+MAX(l,r);
    }
  n->cache_.set( true, (const void*)depth );
  return( depth );
}

size_t
Manager::count_nodes( node nref ) const
{
  if( nref == null_node ) nref = root_;
  const Node *n = &nodes_[nref];
  size_t count = 0;
  if( !sorted_ )
    count = count_nodes_recursive( n );
  else
    {
      for( size_t i = 0; i < num_; ++i )
	{
	  const Node *curr = &nodes_[i];
	  if( (curr->type_ == And) || (curr->type_ == Or) )
	    {
	      assert( (i > curr->left_) && (i > curr->right_) );
	      size_t n = 1;
	      if( !nodes_[curr->left_].cache_.used_ )
		{
		  n += (size_t)nodes_[curr->left_].cache_.first_;
		  nodes_[curr->left_].cache_.used_ = true;
		}
	      if( !nodes_[curr->right_].cache_.used_ )
		{
		  n += (size_t)nodes_[curr->right_].cache_.first_;
		  nodes_[curr->right_].cache_.used_ = true;
		}
	      curr->cache_.set( false, (const void*)n );
	    }
	  else
	    curr->cache_.set( false, (const void*)1 );
	}
      count = (size_t)n->cache_.first_;
    }
  clean_node_cache();
  return( count );
}

size_t
Manager::count_nodes_recursive( const Node *n ) const
{
  if( n->cache_.used_ ) return( 0 );
  size_t nodes = 1;
  if( (n->type_ == And) || (n->type_ == Or) )
    {
      nodes += count_nodes_recursive( &nodes_[n->left_] );
      nodes += count_nodes_recursive( &nodes_[n->right_] );
    }
  n->cache_.set( true, (const void*)nodes );
  return( nodes );
}

size_t
Manager::count_edges( node nref ) const
{
  if( nref == null_node ) nref = root_;
  const Node *n = &nodes_[nref];
  size_t count = 0;
  if( !sorted_ )
    count = count_edges_recursive( n );
  else
    {
      for( size_t i = 0; i < num_; ++i )
	{
	  const Node *curr = &nodes_[i];
	  if( (curr->type_ == And) || (curr->type_ == Or) )
	    {
	      assert( (i > curr->left_) && (i > curr->right_) );
	      size_t n = 2;
	      if( !nodes_[curr->left_].cache_.used_ )
		{
		  n += (size_t)nodes_[curr->left_].cache_.first_;
		  nodes_[curr->left_].cache_.used_ = true;
		}
	      if( !nodes_[curr->right_].cache_.used_ )
		{
		  n += (size_t)nodes_[curr->right_].cache_.first_;
		  nodes_[curr->right_].cache_.used_ = true;
		}
	      curr->cache_.set( false, (const void*)n );
	    }
	  else
	    curr->cache_.set( false, (const void*)0 );
	}
      count = (size_t)n->cache_.first_;
    }
  clean_node_cache();
  return( count );
}

size_t
Manager::count_edges_recursive( const Node *n ) const
{
  if( n->cache_.used_ ) return( 0 );
  size_t edges = 0;
  if( (n->type_ == And) || (n->type_ == Or) )
    {
      edges += 2;
      edges += count_edges_recursive( &nodes_[n->left_] );
      edges += count_edges_recursive( &nodes_[n->right_] );
    }
  n->cache_.set( true, (const void*)edges );
  return( edges );
}

bool
Manager::satisfiable( node nref ) const
{
  if( nref == null_node ) nref = root_;
  const Node *n = &nodes_[nref];
  bool sat = false;
  if( !sorted_ )
    sat = satisfiable_recursive( n );
  else
    {
      for( size_t i = 0; i < num_; ++i )
	{
	  const Node *curr = &nodes_[i];
	  if( (curr->type_ == And) || (curr->type_ == Or) )
	    {
	      assert( (i > curr->left_) && (i > curr->right_) );
	      bool sat;
	      if( curr->type_ == And )
		sat = (bool)nodes_[curr->left_].cache_.first_ && (bool)nodes_[curr->right_].cache_.first_;
	      else
		sat = (bool)nodes_[curr->left_].cache_.first_ || (bool)nodes_[curr->right_].cache_.first_;
	      curr->cache_.set( true, (const void*)sat );
	    }
	  else if( curr->type_ == Value )
	    curr->cache_.set( true, (const void*)curr->left_ );
	  else
	    curr->cache_.set( true, (const void*)true );
	}
      sat = (bool)n->cache_.first_;
    }
  clean_node_cache();
  return( sat );
}

bool
Manager::satisfiable_recursive( const Node *n ) const
{
  bool sat = true;
  if( n->cache_.used_ ) return( (bool)n->cache_.first_ );
  if( n->type_ == And )
    sat = satisfiable_recursive( &nodes_[n->left_] ) && satisfiable_recursive( &nodes_[n->right_] );
  else if( n->type_ == Or )
    sat = satisfiable_recursive( &nodes_[n->left_] ) || satisfiable_recursive( &nodes_[n->right_] );
  else if( n->type_ == Value )
    sat = (n->left_ != 0);
  n->cache_.set( true, (const void*)sat );
  return( sat );
}

float
Manager::count_models( float *output, const int *litmap, const int *varmap, node nref ) const
{
  float count = 0;
  if( nref == null_node ) nref = root_;
  if( !smooth_ || !sorted_ )
    {
      ModelList models;
      enumerate_models( models, nref );
      for( ModelList::const_iterator it = models.begin(); it != models.end(); ++it )
	count += (float)pow( (double)2.0, (double)(num_vars_ - (*it).size()) );
    }
  else
    {
      count = count_models_sorted_smooth( &nodes_[nref], output, litmap, varmap );
    }
  clean_node_cache();
  return( count );
}

float
Manager::count_models_sorted_smooth( const Node *n, float *output, const int *litmap, const int *varmap ) const
{
  float result;
  assert( sorted_ && smooth_ );

  // first pass (bottom-up): compute model count in each node
  for( size_t i = 0; i < num_; ++i )
    {
      float count = 0, pcount = 0;
      const Node *curr = &nodes_[i];
      if( (curr->type_ == And) || (curr->type_ == Or) )
	{
	  assert( (i > curr->left_) && (i > curr->right_) );
	  float left = *(float*)&nodes_[curr->left_].cache_.first_;
	  float right = *(float*)&nodes_[curr->right_].cache_.first_;
	  float pleft = *(float*)&nodes_[curr->left_].cache_.second_;
	  float pright = *(float*)&nodes_[curr->right_].cache_.second_;
	  count = (curr->type_==And?left*right:left+right);
	  if( (pleft < 0) && (pright < 0) )
	    pcount = -1;
	  else
	    {
	      pcount = (curr->type_==And?1:0);
	      if( pleft >= 0 ) pcount = pleft;
	      if( pright >= 0 ) pcount = (curr->type_==And?pcount*pright:pcount+pright);
	      if( (curr->type_ == Or) && (pcount == 0) && ((pleft < 0) || (pright < 0)) )
		pcount = -1;
	    }
	}
      else if( curr->type_ == Value )
	count = pcount = (curr->left_?1:0);
      else if( curr->type_ == Variable )
	{
	  count = 1;
	  if( litmap && litmap[curr->left_^1] )
	    count = pcount = 0;
	  else if( varmap && !varmap[curr->left_>>1] )
	    pcount = -1;
	  else
	    pcount = 1;
	}
      curr->cache_.set( true, *(const float**)&count, *(const float**)&pcount );
    }
  result = *(float*)&n->cache_.second_;
  if( !output ) return( result );

  // initialize output
  for( size_t i = 0; i <= num_vars_; ++ i ) output[i] = -10;

  // second pass (top-down): differentiate with respect to each node (only for complete count)
  for( size_t i = num_; i > 0; --i )
    {
      float pd = 0;
      const Node *curr = &nodes_[i-1];
      float count = *(float*)&curr->cache_.first_;
      if( (curr->parents_->size() == 0) || (count < 0) )
	pd = 1;
      else
	{
	  for( NodeVector::const_iterator pi = curr->parents_->begin(); pi != curr->parents_->end(); ++pi )
	    {
	      assert( (nodes_[*pi].type_ == And) || (nodes_[*pi].type_ == Or) );
	      float cpd = *(float*)&nodes_[*pi].cache_.second_;
	      if( nodes_[*pi].type_ == And )
		{
		  if( nodes_[*pi].left_ == i-1 )
		    cpd *= *(float*)&nodes_[nodes_[*pi].right_].cache_.first_;
		  else
		    cpd *= *(float*)&nodes_[nodes_[*pi].left_].cache_.first_;
		}
	      pd += cpd;
	    }
	}
      curr->cache_.second_ = *(const float**)&pd;
      if( curr->type_ == Variable )
	{
	  if( (curr->left_ % 2 == 0) && (!litmap || !litmap[curr->left_^1]) )
	    output[curr->left_>>1] = pd;
	  else if( output[curr->left_>>1] == -1 )
	    output[curr->left_>>1] = 0;
	}
    }
  output[0] = *(float*)&n->cache_.first_;
  return( result );
}

#if 0
float
Manager::max_count_models( const int *litmap, const int *nodemap ) const
{
  assert( sorted_ && smooth_ );
  for( size_t i = 0; i < num_; ++i )
    {
      float count = 0;
      const Node *curr = &nodes_[i];
      if( (curr->type_ == And) || (curr->type_ == Or) )
	{
	  assert( (i > curr->left_) && (i > curr->right_) );
	  float left = *(float*)&nodes_[curr->left_].cache_.first_;
	  float right = *(float*)&nodes_[curr->right_].cache_.first_;
	  if( curr->type_ == And )
	    count = left * right;
	  else if( nodemap[i] )
	    count = MAX(left,right);
	  else
	    count = left + right;
	}
      else if( curr->type_ == Value )
	count = (curr->left_?1:0);
      else if( curr->type_ == Variable )
	{
	  count = 1;
	  if( litmap && litmap[curr->left_^1] ) count = 0;
	}
      curr->cache_.set( true, *(const float**)&count );
    }
  return( *(float*)&nodes_[root_].cache_.first_ );
}
#endif

float
Manager::max_count_models_aux( const Node *n, const int *litmap, const int *nodemap, bool mode ) const
{
  float count = 0;
  if( n->cache_.used_ ) return( *(float*)&n->cache_.first_ );
  if( (n->type_ == And) || (n->type_ == Or) )
    {
      if( mode && (n->type_ == Or) && !nodemap[n - nodes_] )
        ;//std::cout << "inconsistency at node " << n - nodes_ << std::endl;
      mode = mode || (nodemap[n - nodes_] == 1);
      float left = max_count_models_aux( &nodes_[n->left_], litmap, nodemap, mode );
      float right = max_count_models_aux( &nodes_[n->right_], litmap, nodemap, mode );
      if( nodemap[n-nodes_] && (n->type_ == Or) )
        count = (n->type_==And?MIN(left,right):MAX(left,right));
      else
        count = (n->type_==And?left*right:left+right);
      if( n->type_ == Or && count > 7 )
        ;//std::cout << n-nodes_ << ":" << left << ":" << right << "=" << count << std::endl;
    }
  else if( n->type_ == Value )
    {
      count = (n->left_?1:0);
    }
  else if( n->type_ == Variable )
    {
      count = 1;
      if( litmap && litmap[n->left_^1] ) count = 0;
    }
  n->cache_.set( true, *(const float**)&count );
  return( count );
}

float
Manager::max_count_models( const int *litmap, const int *nodemap ) const
{
  float count = max_count_models_aux( &nodes_[root_], litmap, nodemap, false );
  clean_node_cache();
  return( count );
}

void
Manager::enumerate_models( ModelList &models, node nref ) const
{
  if( nref == null_node ) nref = root_;
  const Node *n = &nodes_[nref];
  if( !sorted_ )
    {
      const ModelList *tmp = enumerate_models_recursive( n );
      models = *tmp;
    }
  else
    {
      for( size_t i = 0; i < num_; ++i )
	{
	  ModelList *models = new ModelList;
	  const Node *curr = &nodes_[i];
	  if( (curr->type_ == And) || (curr->type_ == Or) )
	    {
	      assert( (i > curr->left_) && (i > curr->right_) );
	      const ModelList *lmodels = (const ModelList*)nodes_[curr->left_].cache_.first_;
	      const ModelList *rmodels = (const ModelList*)nodes_[curr->right_].cache_.first_;
	      if( curr->type_ == And )
		{
		  for( ModelList::const_iterator l = lmodels->begin(); l != lmodels->end(); ++l )
		    for( ModelList::const_iterator r = rmodels->begin(); r != rmodels->end(); ++r )
		      {
			Model model;
			model.insert( (*l).begin(), (*l).end() );
			model.insert( (*r).begin(), (*r).end() );
			models->push_back( model );
		      }
		}
	      else
		{
		  models->insert( models->end(), lmodels->begin(), lmodels->end() );
		  models->insert( models->end(), rmodels->begin(), rmodels->end() );
		}
	    }
	  else if( curr->type_ == Variable )
	    {
	      Model model;
	      model.insert( curr->left_ );
	      models->push_back( model );
	    }
	  else if( curr->type_ == Value )
	    {
	      Model model;
	      if( n->left_ ) models->push_back( model );
	    }
	  curr->cache_.set( true, (const void*)models );
	}
      models = *(const ModelList*)n->cache_.first_;
    }

  // clean node cache
  for( size_t i = 0; i < num_; ++i )
    {
      if( nodes_[i].cache_.used_ ) delete (ModelList*)nodes_[i].cache_.first_;
      nodes_[i].cache_.set( false, (const void*)0, (const void*)0 );
    }
}

const ModelList* 
Manager::enumerate_models_recursive( const Node *n ) const
{
  ModelList *models = 0;
  if( n->cache_.used_ ) return( (const ModelList*)n->cache_.first_ );
  if( n->type_ == And )
    {
      models = new ModelList;
      const ModelList *lmodels = enumerate_models_recursive( &nodes_[n->left_] );
      const ModelList *rmodels = enumerate_models_recursive( &nodes_[n->right_] );
      for( ModelList::const_iterator l = lmodels->begin(); l != lmodels->end(); ++l )
	for( ModelList::const_iterator r = rmodels->begin(); r != rmodels->end(); ++r )
	  {
	    Model model;
	    model.insert( (*l).begin(), (*l).end() );
	    model.insert( (*r).begin(), (*r).end() );
	    models->push_back( model );
	  }
    }
  else if( n->type_ == Or )
    {
      models = new ModelList;
      const ModelList *lmodels = enumerate_models_recursive( &nodes_[n->left_] );
      const ModelList *rmodels = enumerate_models_recursive( &nodes_[n->right_] );
      models->insert( models->end(), lmodels->begin(), lmodels->end() );
      models->insert( models->end(), rmodels->begin(), rmodels->end() );
    }
  else if( n->type_ == Variable )
    {
      Model model;
      models = new ModelList;
      model.insert( n->left_ );
      models->push_back( model );
    }
  else if( n->type_ == Value )
    {
      Model model;
      models = new ModelList;
      if( n->left_ ) models->push_back( model );
    }
  n->cache_.set( true, (const void*)models );
  return( models );
}

void
Manager::dump( std::ostream &os ) const
{
  for( size_t i = 0; i < num_; ++i )
    {
      NodeType type = nodes_[i].type_;
      if( type == And )
	os << i << "=[And:" << nodes_[i].left_ << ":" << nodes_[i].right_ << ":";
      else if( type == Or )
	os << i << "=[Or:" << nodes_[i].left_ << ":" << nodes_[i].right_ << ":";
      else if( type == Variable )
	os << i << "=[Var:" << nodes_[i].left_ << ":-:";
      else if( type == Value )
	os << i << "=[Val:" << nodes_[i].left_ << ":-:";
      if( (type==And) || (type==Or) || (type==Variable) || (type==Value) )
	{
	  for( NodeVector::const_iterator pi = nodes_[i].parents_->begin(); pi != nodes_[i].parents_->end(); ++pi )
	    os << *pi << ",";
	  os << "]" << std::endl;
	}
    }
}

void
Manager::project( const int *varmap, node nref, const std::set<node> *marked, std::map<node,node> *remap )
{
  // invalidate variable cache so will create new var nodes, clear true/false nodes
  size_t old_ref_count = ref_count_;
  var_cache_.clear();
  nodes_[true_node_].ref_ = 1;
  nodes_[true_node_].parents_->clear();
  nodes_[false_node_].ref_ = 1;
  nodes_[false_node_].parents_->clear();
  assert( (true_node_ < 2) && (false_node_ < 2) );

  // project nnf creating new nodes below current threshold
  size_t threshold = num_;
  if( nref == null_node ) nref = root_;
  node res = project_aux( varmap, nref );

  // remap marks
  if( marked && remap )
    {
      for( std::set<node>::const_iterator mi = marked->begin(); mi != marked->end(); ++mi )
        {
	  node r = 2 + (node)nodes_[*mi].cache_.first_ - threshold;
          remap->insert( std::make_pair( *mi, r ) );
	}
    }
  clean_node_cache();

  // invalidate all caches and ref_counts
  or_cache_.clear();
  and_cache_.clear();
  var_cache_.clear();
  ref_count_ -= (old_ref_count - 2);

  // remap new nodes to beginning of array and recreate caches
  for( size_t i = 0, sz = num_ - threshold; i < sz; ++i )
    {
      nodes_[2+i] = nodes_[threshold+i];
      if( (nodes_[2+i].type_ == And) || (nodes_[2+i].type_ == Or) )
	{
	  assert( (nodes_[2+i].left_ < 2) || (nodes_[2+i].left_ >= threshold) );
	  assert( (nodes_[2+i].right_ < 2) || (nodes_[2+i].right_ >= threshold) );
	  if( nodes_[2+i].left_ >= 2 ) nodes_[2+i].left_ -= (threshold-2);
	  if( nodes_[2+i].right_ >= 2 ) nodes_[2+i].right_ -= (threshold-2);
	}
      for( NodeVector::iterator pi = nodes_[2+i].parents_->begin(); pi != nodes_[2+i].parents_->end(); ++pi )
	{
	  assert( (*pi < 2) || (*pi >= threshold) );
	  if( *pi >= 2 ) *pi -= (threshold-2);
	}
      insert_cache( &nodes_[2+i] );
    }
  num_ = 2 + (num_ - threshold);
  root_ = (res >= 2 ? res - threshold + 2 : res);

  // clean all returned nodes
  memset( &nodes_[num_], 0, (size_-num_) * sizeof(Node) );
}

node
Manager::project_aux( const int *varmap, node nref )
{
  node res;
  if( nodes_[nref].cache_.first_ != (void*)0 )
    {
      register_use( (node)nodes_[nref].cache_.first_ );
      return( (node)nodes_[nref].cache_.first_ );
    }
  else if( (nodes_[nref].type_ == And) || (nodes_[nref].type_ == Or) )
    {
      node left = project_aux( varmap, nodes_[nref].left_ );
      node right = project_aux( varmap, nodes_[nref].right_ );
      res = (nodes_[nref].type_==And?make_and(this,left,right):make_or(this,left,right));
      unregister_use( left );
      unregister_use( right );
    }
  else if( nodes_[nref].type_ == Variable )
    {
      if( !varmap[nodes_[nref].left_>>1] )
	{
	  res = true_node();
	  register_use( res );
	}
      else
	{
	  res = make_variable( this, nodes_[nref].left_ );
	}
    }
  else if( nodes_[nref].type_ == Value )
    {
      res = nref;
      register_use( res );
    }
  nodes_[nref].cache_.first_ = (void*)res;
  return( res );
}

node
make_not( Manager *man, node nref )
{
  node res = null_node;
  if( (man->nodes_[nref].type_ == And) || (man->nodes_[nref].type_ == Or) )
    {
      node left = make_not( man, man->nodes_[nref].left_ );
      node right = make_not( man, man->nodes_[nref].right_ );
      if( man->nodes_[nref].type_ == And )
	res = make_or( man, left, right );
      else
	res = make_and( man, left, right );
      man->unregister_use( left );
      man->unregister_use( right );
    }
  else if( man->nodes_[nref].type_ == Variable )
    res = make_variable( man, (unsigned)man->nodes_[nref].left_ ^ 1 );
  else if( man->nodes_[nref].type_ == Value )
    res = make_value( man, !(bool)man->nodes_[nref].left_ );
  return( res );
}

node
push_disjunctions( Manager *man, node nref )
{
  node res = null_node;
  if( (man->nodes_[nref].type_ == And) || (man->nodes_[nref].type_ == Or) )
    {
      node left = push_disjunctions( man, man->nodes_[nref].left_ );
      node right = push_disjunctions( man, man->nodes_[nref].right_ );
      if( man->nodes_[nref].type_ == And )
	res = make_and( man, left, right );
      else
	{
	  // four cases
	  if( (man->nodes_[left].type_ == And) && (man->nodes_[right].type_ == And) )
	    {
	      node tmp = make_or( man, man->nodes_[left].left_, man->nodes_[right].left_ );
	      node ll_tmp = push_disjunctions( man, tmp );
	      man->unregister_use( tmp );
	      tmp = make_or( man, man->nodes_[left].left_, man->nodes_[right].right_ );
	      node lr_tmp = push_disjunctions( man, tmp );
	      man->unregister_use( tmp );
	      tmp = make_or( man, man->nodes_[left].right_, man->nodes_[right].left_ );
	      node rl_tmp = push_disjunctions( man, tmp );
	      man->unregister_use( tmp );
	      tmp = make_or( man, man->nodes_[left].right_, man->nodes_[right].right_ );
	      node rr_tmp = push_disjunctions( man, tmp );
	      man->unregister_use( tmp );
	      node l_tmp = make_and( man, ll_tmp, lr_tmp );
	      man->unregister_use( ll_tmp );
	      man->unregister_use( lr_tmp );
	      node r_tmp = make_and( man, rl_tmp, rr_tmp );
	      man->unregister_use( rl_tmp );
	      man->unregister_use( rr_tmp );
	      res = make_and( man, l_tmp, r_tmp );
	      man->unregister_use( l_tmp );
	      man->unregister_use( r_tmp );
	    }
	  else if( (man->nodes_[left].type_ == And) || (man->nodes_[right].type_ == And) )
	    {
	      node mixed = (man->nodes_[left].type_==And?left:right);
	      node other = (mixed==left?right:left);
	      node tmp = make_or( man, man->nodes_[mixed].left_, other );
	      node l_tmp = push_disjunctions( man, tmp );
	      man->unregister_use( tmp );
	      tmp = make_or( man, man->nodes_[mixed].right_, other );
	      node r_tmp = push_disjunctions( man, tmp );
	      man->unregister_use( tmp );
	      res = make_and( man, l_tmp, r_tmp );
	      man->unregister_use( l_tmp );
	      man->unregister_use( r_tmp );
	    }
	  else
	    res = make_or( man, left, right );
	}
      man->unregister_use( left );
      man->unregister_use( right );
    }
  else
    {
      man->register_use( &man->nodes_[nref] );
      res = nref;
    }
  return( res );
}

node
time_shift( Manager *man, node nref, int offset )
{
  node left, right, res;
  switch( man->nodes_[nref].type_ )
    {
    case Null:
      break;
    case And:
    case Or:
      left = time_shift( man, man->nodes_[nref].left_, offset );
      right = time_shift( man, man->nodes_[nref].right_, offset );
      if( man->nodes_[nref].type_ == And )
	res = make_and( man, left, right );
      else
	res = make_or( man, left, right );
      man->unregister_use( left );
      man->unregister_use( right );
      break;
    case Value:
      res = make_value( man, man->nodes_[nref].left_ );
      break;
    case Variable:
      res = make_variable( man, (int)man->nodes_[nref].left_ + offset );
      break;
    }
  return( res );
}

void
dump_cnf( const Manager *man, std::ostream &os, node nref, bool satlib_fmt, const char **names )
{
  // ASSUMPTION: this nnf is actually a cnf
  if( nref == null_node ) nref = man->root();
  if( satlib_fmt )
    {
      size_t nclauses, nvars = 0;
      man->variables( &man->nodes_[nref] );
      const char *vars = (const char*)man->nodes_[nref].cache_.first_;
      for( size_t i = 0; i < man->num_vars(); ++i )
	if( vars[i/8] & (1<<(i%8)) )
	  nvars = MAX(nvars,1+i);
      nclauses = man->nodes_[nref].count_cnf_clauses( *man );
      os << "p cnf " << nvars << " " << nclauses << std::endl;
      man->clean_variables( &man->nodes_[nref] );
    }
  man->nodes_[nref].dump_cnf( os, *man, false, satlib_fmt, names );
  if( satlib_fmt ) os << "%" << std::endl;
}

node
readXML_aux( const XML::Node *xml_input, Manager &manager )
{
  node n;
  if( xml_input->get_name() == "and" )
    {
      n = make_value( &manager, true );
      for( int i = 0; i < xml_input->size(); ++i )
	{
	  node tmp = readXML_aux( xml_input->get_child(i), manager );
	  node conj = make_and( &manager, n, tmp );
	  manager.unregister_use( n );
	  manager.unregister_use( tmp );
	  n = conj;
	}
    }
  else if( xml_input->get_name() == "or" )
    {
      n = make_value( &manager, false );
      for( int i = 0; i < xml_input->size(); ++i )
	{
	  node tmp = readXML_aux( xml_input->get_child(i), manager );
	  node disj = make_or( &manager, n, tmp );
	  manager.unregister_use( n );
	  manager.unregister_use( tmp );
	  n = disj;
	}
    }
  else if( xml_input->get_name() == "variable" )
    {
      int var = 0;
      std::string text = xml_input->get_child(0)->get_text();
      int sign = (text[0] == '~');
      sscanf( text.c_str(), (sign?"~v%d":"v%d"), &var );
      n = make_variable( &manager, sign + (var<<1) );
    }
  else if( xml_input->get_name() == "value" )
    {
      int value = atoi( xml_input->get_child(0)->get_text().c_str() );
      n = make_value( &manager, value );
    }
  else
    throw 1;
  return( n );
}

node
readXML( std::istream &is, Manager &manager )
{
  XML::Node *xml_input = 0;
  is >> xml_input;
  return( readXML_aux( xml_input, manager ) );
}

}; // nnf namespace
