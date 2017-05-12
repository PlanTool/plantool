#ifndef ATOM_STATES_H
#define ATOM_STATES_H

#include "problems.h"
#include <list>
#include <deque>
#include <map>

extern "C" {
#include "md4.h"
};

class Application;
class Atom;
class AtomSet;
class State;
class ValueMap;

class hash_t;
class stateHash_t;
template<class T>
class hashEntry_t;


/*******************************************************************************
 *
 * state
 *
 ******************************************************************************/

/*
  The state_t class represents a state as a bit vector. Each bit encodes the 
  value of a state variable. In turn, each state variable is a literal over a 
  ground atom. Thus, the value of a state variable bit is 1/true if the 
  corresponding ground atom holds in a state, and 0 otherwise.
*/
class state_t
{
  // The bit vector representing the state
  unsigned *data_;

  // The number of atoms in the problem, i.e. the number of bits in a state
  static size_t size_;

  // The maximum number of bytes a state's internal data occupies on the heap
  static size_t max_internal_size_in_bytes_;
  /*
    The hash holding pointers to all states encountered so far. These pointers
    serve as unique state identifiers throughout the code base, so that different
    parts of he code don't need to keep their own state copies
  */
  static stateHash_t *state_hash_;

public:
  explicit state_t()
    {
      notify( this, "state_t::state_t()" );
      data_ = (unsigned*)calloc( size_, sizeof(unsigned) );
    }

  state_t( const State& state );
  state_t( const state_t& state )
    {
      notify( this, "state_t::state_t(state_t&)" );
      data_ = (unsigned*)malloc( size_ * sizeof(unsigned) );  
      memcpy( data_, state.data_, size_ * sizeof(unsigned) );
    }
  state_t( const atomList_t &alist );
  state_t( const problem_t& problem );
  ~state_t() { free( data_ ); }

  /*
    Initializes the state hash.
  */
  static void initialize( const problem_t &problem );

  /*
    Initializes the state hash.
  */
  static void finalize( void );

  /*
    Prints the state hash stats.
  */
  static void statistics( std::ostream &os );

  /*
    Retrieves the state ID (i.e., the pointer to the "main" copy of the state)
    for the given state.
  */
  static const state_t* get_state( const state_t &state );

  /*
    Gets the state ID (a state pointer) along with the state's hash value.
  */
  static std::pair<const state_t*, unsigned> get_state_and_hash( const state_t &state );

  /*
    Deletes the given state from the state hash.
  */
  static void destroy_state( const state_t &state );

  static size_t size( void ) { return( size_ ); }

  static size_t size_in_bytes( void ) { return max_internal_size_in_bytes_; } 

  /*
    Deprecated in this version.
  */
  void make_digest( void ) { }
  bool make_check( void ) const { return( true ); }

  /*
    Returns the bit vector representing the state.
  */
  const unsigned* data( void ) const { return( data_ ); }

  /*
    Computes the state's hash value.
  */
  inline unsigned hash_value( void ) const
    {
#if 1
      unsigned *ptr, result;
      unsigned char digest[16];
      MD4_CTX context;

      // compute MD4 digests
      MD4Init( &context );
      MD4Update( &context, (unsigned char*)data_, size_ * sizeof(unsigned) );
      MD4Final( digest, &context );

      // compact digest into unsigned (assumes sizeof(unsigned) = 4)
      ptr = (unsigned*)digest;
      result = (ptr[0] ^ ptr[1] ^ ptr[2] ^ ptr[3]);
      return( result );
#else
      unsigned value = 0;
      for( size_t i = 0; i < size_; ++i )
	value = value ^ data_[i];
      return( value );
#endif
    }

  unsigned digest( void ) const { return( hash_value() ); }
  void operator=( const state_t &state )
    {
      memcpy( data_, state.data_, size_ * sizeof(unsigned) );
    }

  bool operator==( const state_t &state ) const
    {
      return( (size_ == state.size_) &&
	      !memcmp( data_, state.data_, size_ * sizeof(unsigned) ) );
    }

  /*
    Checks whether the state variable corresponding to the given atom has 
    value 1 in this state. This amounts to figuring out the bit corresponding
    to this atom and seeing whether it is set to 1.
  */
  bool holds( ushort_t atom ) const
    {
#if 0
      register size_t i = atom % 32;
      register unsigned data = data_[atom>>5];
      for( size_t j = 0; j < i; data = (data>>1), ++j );
      return( data % 2 );
#else
      register size_t i = atom >> 5;
      register size_t j = atom % 32;
      return( data_[i] & (0x1<<j) );
#endif
    }

  bool holds( const Atom &atom ) const
    {
      return( holds( problem_t::atom_hash_get( atom ) ) );
    }

  /*
    Sets the bit of the state variable correspoding to the given groud atom
    to 1.
  */
  bool add( ushort_t atom )
    {
      register size_t i = atom >> 5;
      register size_t j = atom % 32;
      bool rv = !(data_[i] & (0x1<<j));
      data_[i] = data_[i] | (0x1<<j);
      return( rv );
    }

  bool add( const Atom &atom ) { return( add( problem_t::atom_hash_get( atom ) ) ); }
  
  /*
    Sets the bit of the state variable correspoding to the given groud atom
    to 0.
  */
  bool clear( ushort_t atom )
    {
      register size_t i = atom >> 5;
      register size_t j = atom % 32;
      bool rv = (data_[i] & (0x1<<j));
      data_[i] = data_[i] & ~(0x1<<j);
      return( rv );
    }

  bool clear( const Atom &atom ) { return( clear( problem_t::atom_hash_get( atom ) ) ); }

  /*
    Prints the IDs of atoms that hold in the given state.
  */
  void print( std::ostream &os ) const;

  /*
    Prints the names of atoms that hold in the given state.
  */
  void full_print( std::ostream &os, const problem_t *problem ) const;


public: // iterators
  /*
    Iterator over the set of atoms that are set to "true" in a given state.
  */
  class const_predicate_iterator
  {
    const state_t *state_;
    size_t idx_;

  protected:
    const_predicate_iterator( const state_t &st, size_t idx = 0 ) : state_(&st), idx_(idx) { }
    void first( void )
      {
	for( idx_ = 0; (idx_ < state_->size()) && !state_->data()[idx_]; ++idx_ );
	if( idx_ < state_->size() )
	  {
	    register unsigned data = state_->data()[idx_];
	    for( idx_ = (idx_<<5); data % 2 == 0; data = (data>>1), ++idx_ );
	  }
	else
	  {
	    idx_ = idx_ << 5;
	  }
      }
    size_t increase( void ) const
      {
	register unsigned data = 0;
	register size_t i = idx_ >> 5;
	register size_t j = idx_ % 32;

	if( j == 31 )
	  {
	    j = 0;
	    for( ++i; (i < state_->size()) && !state_->data()[i]; ++i );
	    if( i < state_->size() ) data = state_->data()[i];
	  }
	else
	  {
	    data = (state_->data()[i]) >> (j+1);
	    if( data != 0 )
	      {
		++j;
	      }
	    else
	      {
		j = 0;
		for( ++i; (i < state_->size()) && !state_->data()[i]; ++i );
		if( i < state_->size() ) data = state_->data()[i];
	      }
	  }

	if( i < state_->size() )
	  {
	    i = (i << 5) + j;
	    for( ; data % 2 == 0; data = (data>>1), ++i );
	  }
	else
	  {
	    i = i << 5;
	  }

	return( i );
      }

  public:
    const_predicate_iterator() : state_(0), idx_(0) { }
    ushort_t operator*( void ) const { return( idx_ ); }
    const_predicate_iterator operator++( void ) // pre increment
      {
	idx_ = increase();
	return( *this );
      }

    const_predicate_iterator operator++( int ) // post increment
      {
	const_predicate_iterator it( *this ); // use default copy const.
	idx_ = increase();
	return( it );
      }

    bool operator==( const const_predicate_iterator &it ) const
      {
	return( (state_ == it.state_) && (idx_ == it.idx_) );
      }

    bool operator!=( const const_predicate_iterator &it ) const
      {
	return( (state_ != it.state_) || (idx_ != it.idx_) );
      }

    friend class state_t;
  };

  const const_predicate_iterator predicate_begin( void ) const
    {
      const_predicate_iterator it( *this );
      it.first();
      return( it );
    }

  const const_predicate_iterator predicate_end( void ) const
    {
      return( const_predicate_iterator( *this, (size()<<5) ) );
    }
};

inline std::ostream&
operator<<( std::ostream &os, const state_t &state )
{
  state.print( os );
  return( os );
}


/*******************************************************************************
 *
 * state/probability list
 *
 ******************************************************************************/

/*
  A list intended to hold state-probability pairs, useful in several places
  in the code.
*/
class stateProbList_t : public std::list<std::pair<state_t*,double> >
{
public:
  stateProbList_t()
    {
      notify( this, "stateProbList_t::stateProbList_t()" );
    }
  stateProbList_t( state_t *state, double p )
    {
      notify( this, "stateProbList_t::stateProbList_t(state_t*,double)" );
      push_back( std::make_pair(state,p) );
    }
  stateProbList_t( const stateProbList_t &state_list )
    {
      notify( this, "stateProbList_t::stateProbList_t(stateProbList_t&)" );
      for( stateProbList_t::const_iterator si = state_list.begin(); si != state_list.end(); ++si )
	{
	  state_t *state = new state_t( *(*si).first );
	  push_back( std::make_pair( state, (*si).second ) );
	}
    }
};

#endif // ATOM_STATES_H

