#ifndef UTILS_H
#define UTILS_H

#include "global.h"
#include <stdlib.h>
#include <cassert>
#include <unistd.h>
#include <sys/times.h>
#include <sys/param.h>

extern "C" {
#include "md4.h"
};



class state_t;
class problem_t;




/*******************************************************************************
 *
 * atom list
 *
 ******************************************************************************/

/*
  Implements vectors of state variable values and operations on these vectors.
*/ 
class atomList_t
{
  size_t size_;
  ushort_t *data_, *data_ptr_;

public:
  atomList_t() : size_(0), data_(0), data_ptr_(0)
    {
      notify( this, "atomList_t::atomList_t()" );
    }
  atomList_t( const atomList_t &alist );
  atomList_t( const ushort_t *array, size_t sz );
  ~atomList_t() 
    { 
      if( data_ )
	{
	  free( data_ );
	  data_ = NULL;
	}
    }

  unsigned hash_value( void ) const
  {
    unsigned *ptr, result;
    unsigned char digest[16];
    MD4_CTX context;

    // compute MD4 digests
    MD4Init( &context );
    MD4Update( &context, (unsigned char*)data_, size() );
    MD4Final( digest, &context );

    // compact digest into unsigned (assumes sizeof(unsigned) = 4)
    ptr = (unsigned*)digest;
    result = (ptr[0] ^ ptr[1] ^ ptr[2] ^ ptr[3]);
    return( result );
  }

  bool equal( const ushort_t *array, size_t sz ) const;
  void intersect( const atomList_t &alist );
  bool empty_intersection( const atomList_t &alist ) const;
  size_t intersection_size( const atomList_t &alist ) const;
  size_t size( void ) const { return( data_ptr_ - data_ ); }
  ushort_t atom( size_t i ) const { return( data_[i] ); }
  bool find( ushort_t atm ) const;
  void insert( ushort_t atm );
  void insert( const atomList_t &alist );
  void remove( ushort_t atm );
  void remove( const atomList_t &alist );
  bool holds( ushort_t atm, bool nprec = false ) const
    {
      if( nprec )
	return( find( atm ) );
      else
	return( atm%2 ? !find(atm) : find(atm) );
    }
  bool holds( const state_t &state, bool nprec = false ) const;
  bool holds( const atomList_t &alist, bool nprec = false ) const;
  bool contradiction( void ) const
    {
      for( size_t i = 0; i+1 < size(); ++i )
	if( (data_[i] % 2 == 0) && (data_[i]+1 == data_[i+1]) ) return( true );
      return( false );
    }
  void clear( void ) { data_ptr_ = data_; }
  void print( std::ostream &os ) const;
  void full_print( std::ostream &os, const problem_t *problem ) const;
  bool operator==( const atomList_t &alist ) const;
  atomList_t& operator=( const atomList_t &effect );
};

inline std::ostream&
operator<<( std::ostream &os, const atomList_t& alist )
{
  alist.print( os );
  return( os );
}

inline bool
atomList_t::find( ushort_t atm ) const
{
  for( size_t i = 0; i < size(); ++i )
    if( atom( i ) == atm ) return( true );
  return( false );
}

inline void
atomList_t::insert( ushort_t atm )
{
  size_t i;
  for( i = 0; (i < size()) && (atom( i ) < atm); ++i );
  if( (i == size()) || (atom( i ) > atm) )
    {
      if( i == size() )
	{
	  if( !data_ || (data_ptr_ == &data_[size_]) )
	    {
	      size_ = (!data_ ? 1 : size_ << 1);
	      ushort_t *ndata_ = (ushort_t*)realloc( data_, size_ * sizeof(ushort_t) );
	      data_ptr_ = (!data_ ? ndata_ : &ndata_[size()]);
	      data_ = ndata_;
	    }
	  *data_ptr_++ = atm;
	}
      else
	{
	  assert( data_ != NULL );
	  if( data_ptr_ == &data_[size_] )
	    {
	      size_ = size_ << 1;
	      ushort_t *ndata_ = (ushort_t*)realloc( data_, size_ * sizeof(ushort_t) );
	      data_ptr_ = (!data_ ? ndata_ : &ndata_[size()]);
	      data_ = ndata_;
	    }
	  for( int j = (int)size(); j > (int)i; --j )
	    data_[j] = data_[j-1];
	  data_[i] = atm;
	  ++data_ptr_;
	}
    }
}

inline void
atomList_t::remove( ushort_t atm )
{
  size_t i;
  for( i = 0; (i < size()) && (atom( i ) != atm); ++i );
  if( i < size() )
    {
      for( size_t j = i; j < size(); ++j )
	data_[j] = data_[j+1];
      --data_ptr_;
    }
}


/*******************************************************************************
 *
 * atom list list
 *
 ******************************************************************************/

/*
  Implements a list of vectors of state variable values and operations on such 
  lists.
*/ 
class atomListList_t
{
  size_t size_;
  atomList_t **data_, **data_ptr_;

public:
  atomListList_t() : size_(0), data_(0), data_ptr_(0) { }
  ~atomListList_t() 
    { 
      for (long i = 0; i < data_ptr_ - data_; i++)
	{
	  delete data_[i]; 
	}
      if( data_ ) free( data_ );
    }

  size_t size( void ) const { return( data_ptr_ - data_ ); }
  atomList_t& atom_list( size_t i ) { return( *data_[i] ); }
  const atomList_t& atom_list( size_t i ) const { return( *data_[i] ); }
  bool find( const atomList_t &alist ) const;
  void insert( atomList_t *alist );
  bool holds( const state_t &state, bool nprec = false ) const;
  void clear( void ) { data_ptr_ = data_; }
  void print( std::ostream &os ) const;
  void full_print( std::ostream &os, const problem_t *problem ) const;
  bool operator==( const atomListList_t &alist ) const;
  atomListList_t& operator=( const atomListList_t &alist );
};


/*
  Returns the amount of CPU time that has passed between two time measurements.
*/
inline double
elapsed_time(tms start, tms finish)
{
  return ((double)(finish.tms_cutime + finish.tms_cstime + finish.tms_utime 
		   + finish.tms_stime) - (double)(start.tms_cutime 
		   + start.tms_cstime + start.tms_utime + start.tms_stime))/ HZ;
}

#endif // UTILS_H
