#include <stdlib.h>
#include <math.h>
#include <assert.h>
#include "global.h"
#include "domains.h"
#include "expressions.h"
#include "formulas.h"
#include "functions.h"
#include "hash.h"
#include "problems.h"
#include "states.h"


/*******************************************************************************
 *
 * state
 *
 ******************************************************************************/

//enum { CLEAR = 0x0, OPEN = 0x1, CLOSED = 0x2 };
stateHash_t* state_t::state_hash_ = 0;
size_t state_t::size_ = 0;
size_t state_t::max_internal_size_in_bytes_ = 0;

void
state_t::initialize( const problem_t &problem )
{
  state_hash_ = new stateHash_t( gpt::initial_hash_size );
  gpt::mem_state_hash += sizeof(stateHash_t);
  size_ = (unsigned)ceil( (double)problem.number_atoms() / 32 );
  max_internal_size_in_bytes_ = gpt::heap_overhead + size_ * sizeof(unsigned);
  if( gpt::verbosity >= 300 )
    std::cout << "<state>: size = " << size_ << std::endl;
}


void
state_t::finalize( void )
{
  gpt::mem_state_hash -= sizeof(stateHash_t);
  delete state_hash_;
}


void
state_t::statistics( std::ostream &os )
{
  os << "Total number of distinct states encountered: " << state_hash_->size() 
     << std::endl;
}


const state_t*
state_t::get_state( const state_t &state )
{
  return( state_hash_->get( state )->state() );
}


std::pair<const state_t*, unsigned>
state_t::get_state_and_hash( const state_t &state )
{
  return( state_hash_->get_with_hash( state ) );
}


void 
state_t::destroy_state( const state_t &state )
{
  state_hash_->destroy( state );
}


state_t::state_t( const atomList_t &alist )
{
  notify( this, "state_t::state_t(const atomList_t&)" );
  data_ = (unsigned*)calloc( size_, sizeof(unsigned) );
  for( size_t i = 0; i < alist.size(); ++i )
    {
      if (alist.atom( i ) % 2 == 0)
	{
	  add( alist.atom( i ) );
	}
    }
}


state_t::state_t( const problem_t &problem )
{
  notify( this, "state_t::state_t(problem_t&)" );
  AtomSet::const_iterator ai;
  EffectList::const_iterator ei;
  AtomList::const_iterator li;
  AssignmentList::const_iterator si;

  for( ai = problem.init_atoms().begin(); ai != problem.init_atoms().end(); ++ai )
    if( !problem.domain().predicates().static_predicate( (*ai)->predicate() ) )
      add( **ai );

  for( ei = problem.init_effects().begin(); ei != problem.init_effects().end(); ++ei )
    {
      AtomList adds;
      AtomList deletes;
      AssignmentList assig;
      (*ei)->state_change( adds, deletes, assig, *this );

      for( li = adds.begin(); li != adds.end(); ++li )
	add( **li );

      for( si = assig.begin(); si != assig.end(); ++si )
	(*si)->affect( *this );
    }

  make_digest();
  assert( make_check() );
}

void
state_t::print( std::ostream &os ) const
{
  os << "[";
  for( state_t::const_predicate_iterator ai = predicate_begin(); ai != predicate_end(); ++ai )
    os << " " << *ai;
  os << " ]";
}

void
state_t::full_print( std::ostream &os, const problem_t *problem ) const
{
  state_t::const_predicate_iterator ai;

  os << "[";
  for( ai = predicate_begin(); ai != predicate_end(); ++ai )
    {
      os << " ";
      const Atom *atom = problem_t::atom_inv_hash_get( *ai );
      os << *ai << (*ai%2?":(not ":":");
      problem->print( os, *atom );
      if( *ai % 2 ) os << ")";
    }
  os << " ]";
}
