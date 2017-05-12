#ifndef HASH_H
#define HASH_H

#include "global.h"
#include "heuristics.h"
#include "problems.h"
#include "states.h"
#include "math.h"


extern "C" {
#include "md4.h"
};



class action_t;
class state_t;
class problem_t;
template<class T> 
class hash_base_t;
class hash_succ_t;
class hash_t;
class hash_FH_t;
class succDataContainer;


/*
  This file contains the declarations of classes implementing various kinds of 
  hash tables. Since many of these classes' methods are called often by other
  parts of the code, they are inlined (i.e., their definitions are included
  in their declarations into this header file.
*/


#define SOLVED 0x1
#define DELETE_PROTECTED 0x2
#define REMOVE 0x4



static unsigned
prime( unsigned n )
{
  register unsigned i;
  static bool initialized = false;
  static unsigned p, current, size, *primes;

  if( !initialized )
    {
      p = 0;
      current = 2;
      size = gpt::initial_hash_size;
      primes = (unsigned*)calloc( size, sizeof(unsigned) );
    }

  if (!initialized || primes[p-1] < n)
    {
      do 	
	{
	  unsigned bound = (unsigned)ceil( sqrt( (double)current ) );
	  bool is_prime = true;
	  for( i = 0; (i < p) && (primes[i] <= bound); ++i )
	    {
	      if( current % primes[i] == 0 ) 
		{
		  is_prime = false;
		  break;
		}
	    }

	  if ( is_prime )
	    {
	      if(i == size)
		{
		  size = size>>1;
		  primes = (unsigned*)realloc( primes, size * sizeof(unsigned) );
		}

	      primes[p++] = current;
	      current += (p==1?1:2);
	    }
	  else
	    current += 2;
	} 
      while( primes[p-1] < n );
    }

  initialized = true;
  return( primes[p-1] );
}



/*******************************************************************************
 *
 * hash entry
 *
 ******************************************************************************/

/*
  This template implements an entry in a hash table that maps state pointers to
  values of some type T.
*/

template<typename T>
class hashEntry_t
{
protected:

  // The value corresponding to the state
  T value_;

  // Bits describing some useful information about this entry. The meaning of 
  // this field depends on the semantics of the hash table that uses this entry.
  unsigned bits_;

  // The state pointer corresponding to this entry (serves as key in 
  // the hash table)
  const state_t *state_;

  // Pointers to the preceding and next entry in the same hash table bucket as 
  // this one. 
  hashEntry_t *next_, *prev_;

  friend class hash_base_t<T>;
  friend class hash_t;
  friend class hash_succ_t;


public:

 hashEntry_t() : value_(T()), bits_(0), state_(NULL), next_(0), prev_(0) { }
 hashEntry_t( const state_t &state, T value = T() )
   : value_(value), bits_(0), next_(0), prev_(0)
    {
      notify( this, "hashEntry_t::hashEntry_t(state_t&,double)" );
      state_ = state_t::get_state( state );
    }

  virtual ~hashEntry_t() { }
  void update( T value ) { value_ = value; }
  T value( void ) const { return( value_ ); }
  unsigned bits( void ) const { return( bits_ ); }
  void set_bits( unsigned bits ) { bits_ |= bits; }
  void reset_bits( unsigned bits ) { bits_ &= ~bits; }
  const state_t* state( void ) const { return( state_ ); }
  hashEntry_t<T>* next( void ) const { return( next_ ); }
};



/*******************************************************************************
 *
 * hash base
 *
 ******************************************************************************/

/*
  This class template implements a hash table mapping state pointers to values
  of some type T.
*/

template<typename T> 
class hash_base_t
{
 protected:

  // The current number of key-value pairs in the hash.
  unsigned size_;

  // The number of buckets in the hash
  unsigned dimension_;

  // An array holding the numbers of key-value pairs in each bucket
  unsigned *number_;

  // An array of hash table buckets
  hashEntry_t<T> **table_;

  // A pointer to the problem description
  const problem_t *problem_;

  // An indicator telling whether the table has been initialized
  bool initialized_;

public:

  hash_base_t()
    : size_(0), dimension_(0), number_(0), table_(0),  problem_(0),
    initialized_(false)
    { }

  hash_base_t(const problem_t &problem, unsigned dimension)
    {
      initialize(problem, dimension);
    }

  bool initialized( void ) const { return initialized_; }
  void initialize(const problem_t &problem, unsigned dimension)
  {
    problem_ = &problem;
    size_ = 0;
    dimension_ = prime( dimension );
    table_ = (hashEntry_t<T>**)calloc( dimension_, sizeof(hashEntry_t<T>*) );
    number_ = (unsigned*)calloc( dimension_, sizeof(unsigned) );
    initialized_ = true;
  }

  /*
    This function resets the internal state of the hash table, destroying all
    the stored key-value pairs.
  */
  virtual void clear()
  {
    assert(initialized_);
    for( unsigned i = 0; i < dimension_; ++i )
      {
	for( hashEntry_t<T> *ptr = table_[i]; ptr != NULL; )
	  {
	    hashEntry_t<T> *next = ptr->next_;
	    delete ptr;
	    ptr = next;
	    --size_;
	    --number_[i];
	  }

	table_[i] = NULL;
      }
  }

  virtual ~hash_base_t() 
    {
      if (table_ != NULL)
	{
	  for( unsigned i = 0; i < dimension_; ++i )
	    for( hashEntry_t<T> *ptr = table_[i]; ptr != NULL; )
	      {
		hashEntry_t<T> *next = ptr->next_;
		delete ptr;
		ptr = next;
	      }
	  free( table_ );
	}

      if (number_ != NULL)
	{
	  free( number_ ); 
	} 
    }


  unsigned hash_value( const state_t &state ) const 
  { 
    return(state.hash_value()%dimension_); 
  }

  /*
    Finds the value corresponding the given state.
  */
  hashEntry_t<T>* find( const state_t &state ) const
  {
    unsigned idx = hash_value( state );
    return find( state, idx );
  }

  /*
    Finds the value corresponding the the given state, whose hash 
    is provided. This function enables clients to compute the hash of a state
    once and use it to retrieve information about this state from different
    hash tables.
  */ 
  hashEntry_t<T>* find( const state_t &state, unsigned hash ) const
  {
    assert(initialized_);

    for( hashEntry_t<T> *ptr = table_[hash]; ptr != NULL; ptr = ptr->next_ )
      {
	if( *(ptr->state_) == state ) 
	  {
	    return( ptr );
	  }
      }

    return( NULL );
  }

  /*
    Inserts the given state into the hash table with the default value. 
    The default value is type-dependent, so the template doesn't implement
    this method.
  */ 
  virtual hashEntry_t<T>* insert( const state_t &state ) = 0;

  /*
    As above, but also takes the hash of the state to be inserted into the 
    hash table.
  */
  virtual hashEntry_t<T>* insert( const state_t &state, unsigned hash ) = 0;

  /*
    Inserts the entry for the given key (state) into the hash table.
  */
  virtual void insert( hashEntry_t<T> *entry )
    {
      insert(entry, hash_value(*entry->state()));
    }

  /*
    As above, but also takes the hash of the state whose entry is being 
    inserted. As with other methods that take the hash of a key as an argument,
    this lets callers compute the hash of a key state just once and use it to 
    store or retrieve information about this key from different hash tables 
    derived from this template.
  */
  virtual void insert( hashEntry_t<T> *entry, unsigned hash )
    {
      assert(initialized_);
      entry->next_ = table_[hash];
      entry->prev_ = NULL;

      if( table_[hash] )
	{
	  table_[hash]->prev_ = entry;
	}

      table_[hash] = entry;
      ++number_[hash];
      ++size_;
    }

  /* 
    Retrieves the entry (i.e., the state-value pair) corresponding to the
    given key (state).
  */
  virtual hashEntry_t<T>* get(const state_t &state)
  {
    return get(state, hash_value(state));
  }

  /*
    As above, but also takes the hash of the state whose entry is to be 
    retrieved.
  */
  virtual hashEntry_t<T>* get(const state_t &state, unsigned hash)
    {
      assert(initialized_);
      hashEntry_t<T> *result = find( state, hash );
      
      if (!result)
	{	 
	  result = insert(state, hash);
	  assert(result);
	}

      return result;
    }


  /*
    Retrieves the value corresponding to the given key (state).
  */
  virtual T value( const state_t &state )
  {
    return value(state, hash_value(state));
  }

  /*
    As above, but also takes the hash of the state whose value is to be 
    retrieved.
  */
  virtual T value( const state_t &state, unsigned hash )
  {
    return get(state, hash)->value();
  }

  unsigned dimension( void ) const { return( dimension_ ); }
  unsigned size( void ) const { return( size_ ); }

  /*
    Returns the highest number of entries in any bucket of this hash table.
  */
  unsigned diameter( void ) const
  {
    unsigned result = 0;

    for( unsigned i = 0; i < dimension_; ++i )
      {
	result = (number_[i] > result ? number_[i] : result);
      }

    return( result );
  }

  virtual void print( std::ostream &os, const problem_t &problem ) const = 0;
};



/*******************************************************************************
 *
 * hash
 *
 ******************************************************************************/

/*
  This class implements a value function, a mapping from states to double values
*/
class hash_t : public hash_base_t<double>
{
  mutable heuristic_t *heuristic_;


public:
 
  hash_t() : hash_base_t<double>(), heuristic_(NULL) { }
  hash_t(const problem_t &problem,  unsigned dimension, heuristic_t &heuristic )
  {
    initialize(problem, dimension, heuristic);
  }

  void initialize(const problem_t &problem,  unsigned dimension, 
		  heuristic_t &heuristic );
  virtual ~hash_t();

  /*
    Computes the heuristic value of the given state.
  */
  double heuristic( const state_t &state )
  { 
    return heuristic(state, hash_value(state));
  }
  
  /* 
    As above, but also takes the hash of the state to be evaluated.
  */
  double heuristic( const state_t &state, unsigned hash )
  { 
    return heuristic_->value(state, hash);
  }
 
  /*
    Inserts the given state with its hash value into the hash table.
  */
  hashEntry_t<double>* insert( const state_t &state );

  /* 
    As above, but also takes the hash of the state to be evaluated.
  */
  hashEntry_t<double>* insert( const state_t &state, unsigned idx );
  virtual void print( std::ostream &os, const problem_t &problem ) const;
};







/*******************************************************************************
 *
 * successor data container
 *
 ******************************************************************************/

/*
  Represents empirical distributions over the successors of a given state under 
  each action.

  A state s' is a successor of a state s under an action a is executing a in s
  brings the agent to s' with a positive probability. Determining which action
  is the best one in a given state s involves computing an expectation over the
  succesors of s under each action a. Unfortunately, if the number of
  successors under an action is large, computing this expectation exactly is
  infeasible. One way to approximate it is to sample some number of successors
  of s under each action a, and compute the expectations over only these
  sampled sets. Intuitively, the more successors of s under a are sampled, the
  better the empirical expectation taken over these samples will approximate
  the true expectation.

  succDataContainer contains all the data necessary to compute such empirical
  expectation for each action. succDataContainers are the values in the 
  hash_succ_t hash table, which maps states to the data about their 
  successors.

  To maintain empirical distributions over 
  successors, for each action succDataContainer maintains a hash table that
  maps states to positive integers, representing an empirical distribution
  over successors under that action as a histogram. The histogram is populated
  by sampling the successors by simulating the execution of the corresponding
  action in the given state. The reason for memoizing these empirical 
  distributions in the hash_succ_t table (as opposed to constructing them
  by sampling every time an action needs to be evaluated in a state) is 
  speed -- the sampling process tends to be very expensive. Therefore, it
  makes sense to trade memory for speed as much as possible (i.e., as long
  as there is available memory).
*/
class succDataContainer
{
 public: // definitions of internal classes

  class succHashEntry_t;
  friend class succDataContainer::succHashEntry_t;
  class succHash_t;

  /*****************************************************************************
   *
   * internal successor hash entry
   *
   ****************************************************************************/

  /*
    Represents an entry of the successor hash internal to the succDataContainer
    class. The entry has a pointer to a state and a counter that gives the 
    number of times this state has been sampled as a successor to a particular 
    state-action pair.
  */
  class succHashEntry_t
  {
    // Pointer to the state whose number of occurrences this entry contains.
    const state_t *state_;

    // The number of times this state was sampled as a successor to a particular
    // state-action pair.
    unsigned short num_occ_;

    // The hash value of this state according to the hash_succ_t table. 
    // Technically, we don't need to store this value, since we can easily
    // recompute it. However, since we would have to recompute it many times,
    // caching it as we do here gives a significant speed boost.
    unsigned hash_;

    // Pointers to the next and previous entries in the same bucket of the hash
    // table as this one.
    succHashEntry_t *next_, *prev_;
    friend class succHash_t;


  public:
 
    succHashEntry_t() : state_(NULL), num_occ_(0), hash_(0), next_(0), prev_(0) 
      { }

    succHashEntry_t( const state_t* state, unsigned hash ) : state_(state), 
      num_occ_(1), hash_(hash), next_(0), prev_(0)
      { }
 
    ~succHashEntry_t() { }
    const state_t* state( void ) const { return( state_ ); }
    const succHashEntry_t* next( void ) const { return( next_ ); }
    ushort_t& counter( void ) { return num_occ_; } 
    ushort_t value( void ) const  { return num_occ_; }
    unsigned hash_value( void ) const { return hash_; } 
  };




  /*****************************************************************************
   *
   * internal successor hash
   *
   ****************************************************************************/

  /*
    A hash table that maps successor states of a state-action pair to
    the number of times they have been sampled from the true successor state 
    distribution under a given action in order to produce an empirical 
    distribution. Effectively, it represents an empirical distribution over
    a set of states as a histogram.

    This class does not inherit from hash_base_t for efficiency reasons, since 
    it doesn't need many of that template's members.
  */
  class succHash_t
  {
  protected:

    unsigned size_;
    unsigned dimension_;
    unsigned *number_;
    succHashEntry_t **table_;

    unsigned hash_value( const state_t* state ) const 
    { 
      return( ((size_t)state) % dimension_); 
    }


  public:

    succHash_t() : size_(0), dimension_(0), number_(0), table_(0) { }
    void initialize( void )
    {
      size_ = 0;

      // Ideally, dimension_ should be a prime, but since the table is intended
      // to contain few elements, a composite is fine too. Invoking the 
      // prime(.) method is just too expensive, because for this kind of tables
      // it would need to be done many times.
      dimension_ = gpt::num_succ_per_expansion;
      table_ = (succHashEntry_t**)calloc(dimension_, sizeof(succHashEntry_t*));
      gpt::mem_successor_hash += (gpt::heap_overhead 
			 + dimension_ * sizeof( succHashEntry_t* ) );

      for (size_t i = 0; i < dimension_; i++)
	{
	  table_[i] = NULL;
	}

      number_ = (unsigned*)calloc( dimension_, sizeof(unsigned) );
      gpt::mem_successor_hash += (gpt::heap_overhead 
				           + dimension_ * sizeof( unsigned ) );

      for (size_t i = 0; i < dimension_; i++)
	{
	  number_[i] = 0;
	}

      if( gpt::verbosity >= 300 )
	std::cout << "<internal succ hash>: new hash: dimension = " 
		  << dimension_ << std::endl;
    }

    ~succHash_t( void )
    {
      clear();
      free( table_ );
      gpt::mem_successor_hash -= gpt::heap_overhead 
			             + dimension_ * sizeof( succHashEntry_t* );
      free( number_ );
      gpt::mem_successor_hash -= gpt::heap_overhead 
	                                     + dimension_ * sizeof( unsigned );

      if( gpt::verbosity >= 300 )
        std::cout << "<succ-hash>: deleted" << std::endl;
    }

    succHashEntry_t* find( const state_t* state ) const
    {
      size_t idx = hash_value( state );

      for( succHashEntry_t *ptr = table_[idx]; ptr != NULL; ptr = ptr->next_ )
	{
	  if( ptr->state_ == state ) 
	    return( ptr );
	}
  
      return( NULL );
    }

    /*
      Makes an entry for the state in this hash table and stores the state's
      hash _according to that state and state-value hash tables). This hash
      is generally different from this state's hash according to succHash_t/
    */
    void insert( const state_t* state, unsigned hash )
    {
      succHashEntry_t* entry = new succHashEntry_t(state, hash);
      gpt::mem_successor_hash += gpt::heap_overhead + sizeof(succHashEntry_t);
      size_t idx = hash_value( entry->state() );
      entry->next_ = table_[idx];
      entry->prev_ = NULL;
      if( table_[idx] )
	table_[idx]->prev_ = entry;
      table_[idx] = entry;
      ++number_[idx];
      ++size_;
    }

    void clear()
    {
      for( unsigned i = 0; i < dimension_; ++i )
	{
	  for( succHashEntry_t *ptr = table_[i]; ptr != NULL; )
	    {
	      succHashEntry_t *next = ptr->next_;
	      delete ptr;
	      gpt::mem_successor_hash -= gpt::heap_overhead 
		+ sizeof(succHashEntry_t);
	      ptr = next;
	    }

	  table_[i] = NULL;
	  number_[i] = 0;
	}

      size_ = 0;
    }

    unsigned dimension( void ) const { return( dimension_ ); }
    unsigned size( void ) const { return( size_ ); }
    unsigned diameter( void ) const
    {
      unsigned result = 0;
      for( unsigned i = 0; i < dimension_; ++i )
	result = (number_[i] > result ? number_[i] : result);
      return( result );
    }


  public: // iterator

    class const_iterator;
    friend class succHash_t::const_iterator;

    class const_iterator
    {
      const succHash_t *hash_;
      const succHashEntry_t *ptr_;
      size_t idx_;


    protected:

      const_iterator( const succHash_t *shash, int pos ) 
      : hash_(shash), ptr_(0), idx_(0)
      {
        if( pos == 0 )
          {
            for( ; idx_ < hash_->dimension_; ++idx_ )
	      if( (ptr_ = hash_->table_[idx_]) )
		break;
          }
        else
          {
            idx_ = hash_->dimension_;
            ptr_ = 0;
          }
      }


    public:

      const_iterator() : hash_(0), ptr_(0), idx_(0) { }
      const state_t* state( void ) const { return ptr_->state(); }
      unsigned short value( void ) const { return ptr_->value(); }  
      unsigned hash_value( void ) const { return ptr_->hash_value(); }
      const_iterator operator++( void ) // pre increment
      {
	if( ptr_ != NULL )
	  {
	    if( !(ptr_ = ptr_->next()) )
	      {
		for( ++idx_; idx_ < hash_->dimension_; ++idx_ )
		  if( (ptr_ = hash_->table_[idx_]) )
		    break;
	      }
	  }
	return( *this );
      }

      const_iterator operator++( int ) // post increment
      {
	const_iterator it( *this ); // use default copy const.
	++(*this);
	return( it );
      }

      bool operator==( const const_iterator &it ) const
      {
	return( (ptr_ == it.ptr_) && (idx_ == it.idx_) );
      }

      bool operator!=( const const_iterator &it ) const
      {
	return( (ptr_ != it.ptr_) || (idx_ != it.idx_) );
      }

      friend class succHash_t;
    };

    const const_iterator begin( void ) const
    {
      return( const_iterator( this, 0 ) );
    }

    const const_iterator end( void ) const
    {
      return( const_iterator( this, 1 ) );
    }
  };


// end of internal class definitions

private:

  // An array of tables mapping actions to the empirical distributions over
  // successors under those actions. The i-th entry corresponds to the table
  // for the i-th action in the problem.
  succHash_t* successors_;

  // An array containing immediate rewards of each action. The i-th entry 
  // corresponds to the reward of the i-th action in the problem.
  double *imm_rew_;

  // An array of indicators showing, for each action, if the empirical 
  // successor distribution for the i-th action has sufficient data (i.e.,
  // is built from a sufficient number of samples.
  bool* bReady_;

  // An array showing the number of samples used to construct each action's
  // empirical successor distribution so far.
  size_t* counts_;

  // A flag indicating whether the memory occupied by this object should be 
  // released by the successor hash (hash_succ_t) or by others. 
  bool owned_by_hash_;


public:

  succDataContainer()
  {
    assert(gpt::num_acts > 0);
    successors_ = new succHash_t[gpt::num_acts];
    gpt::mem_successor_hash += (gpt::heap_overhead 
		       + gpt::num_acts * sizeof(succHash_t) );
    owned_by_hash_ = false;
      
    for (size_t i = 0; i < gpt::num_acts; i++)
      {
	successors_[i].initialize();
      }

    imm_rew_ = new double[gpt::num_acts]();
    gpt::mem_successor_hash += (gpt::heap_overhead 
		       + gpt::num_acts * sizeof( imm_rew_[0] ) );
    bReady_ = new bool[gpt::num_acts]();
    gpt::mem_successor_hash += (gpt::heap_overhead 
		       + gpt::num_acts * sizeof( bReady_[0] ) );
    counts_ = new size_t[gpt::num_acts]();
    gpt::mem_successor_hash += (gpt::heap_overhead 
		       + gpt::num_acts * sizeof( counts_[0] ) );
  }

  ~succDataContainer()
  {
    gpt::mem_successor_hash -= (gpt::heap_overhead 
		       + gpt::num_acts * sizeof( bReady_[0] ) );
    delete [] bReady_;
    gpt::mem_successor_hash -= (gpt::heap_overhead 
		       + gpt::num_acts * sizeof( counts_[0] ) );
    delete [] counts_;
    gpt::mem_successor_hash -= (gpt::heap_overhead 
		       + gpt::num_acts * sizeof( imm_rew_[0] ) );
    delete [] imm_rew_;
      
    for( size_t a = 0; a < gpt::num_acts; a++ )
      {
	successors_[a].clear();
      }

    delete [] successors_;
    gpt::mem_successor_hash -= (gpt::heap_overhead
		       + gpt::num_acts * sizeof(succHash_t) );
  }

  bool owned_by_hash() { return owned_by_hash_; }
  void set_owned_by_hash( bool owned ) { owned_by_hash_ = owned; } 
  double get_imm_rew( size_t action ) { return imm_rew_[action]; }
  bool get_ready( size_t action ) { return bReady_[action]; }

  /*
    Returns the number of distinct state in the empirical successor state
    distribution (i.e., the size of the distribution's support.
  */
  size_t get_num_succ( size_t action ) { return successors_[action].size(); }
  void set_imm_rew( size_t action, double reward ) { imm_rew_[action] = reward;}
  void set_ready( size_t action, bool b ) { bReady_[action] = b; }
  
  void reset()
  {
    for( size_t a = 0; a < gpt::num_acts; a++ )
      {
	bReady_[a] = false;
	counts_[a] = 0;
	imm_rew_[a] = 0;
	successors_[a].clear();
      }
  }

  void reset_ready()
  {
    for (size_t a = 0; a < gpt::num_acts; a++)
      {
	bReady_[a] = false;
      }
  }

  bool all_ready()
  {
    for( size_t a = 0; a < gpt::num_acts; a++ )
      {
	if (!bReady_[a])
	  {
	    return false;
	  }
      }
    return true;
  }

  void insert(size_t actIdx, const state_t& state);

  succHash_t::const_iterator succ_begin( size_t action )
  {
    return successors_[action].begin();
  }

  succHash_t::const_iterator succ_end( size_t action )
  {
    return successors_[action].end();
  }

  /* 
     Samples a successor from the action's current _empirical_ successor 
     distribution. Note that the empirical distribution may assign a probability
     of 0 to some states that have a non-zero probability under the 
     corresponding _true_ successor distribution.
  */
  const state_t& sample(size_t actIdx);
};



/*******************************************************************************
 *
 * successor hash
 *
 ******************************************************************************/

/*
  This class implements a hash table that, for every state key S holds a 
  container with information about the immediate successors of S under every
  action A.
*/
class hash_succ_t : public hash_base_t<succDataContainer*>
{
  // A flag indicating whether this hash table has been initialized.
  bool initialized_;

  // If this flag is set the size of the hash may not be increased any more. 
  // Adding new entries to this hash table is only possilbe if some old ones 
  // are deleted.
  bool expansion_stopped_;
  
  // The bucket from which the next entry should be deleted to make space for a 
  // new entry (if necessary)
  size_t remove_idx_;

  // A pointer to the next entry that should be deleted to make space for a new
  // entry (if necessary)
  hashEntry_t<succDataContainer*> *remove_ptr_;

  // If this flag is set, all the entries have been removed from this hash
  // (possibly except for the entries that were in use when the delete happened)
  bool table_emptied_;

  // A ponter to an empty entry (needed when the table has been emptied but a 
  // caller is requesting an entry to be created) 
  hashEntry_t<succDataContainer*>* reused_entry_;

  // A pointer to an empty container for state successors (needed when the 
  // table has been emptied but a caller is requesting an entry to be created) 
  succDataContainer* reused_entry_value_;

  // An array whose i-th element is a pointer to the oldest entry in the 
  // hash table's i-th bucket. It is useful to implement a kind of LRU eviction
  // strategy, under which the oldest entry in a given bucket is the first 
  // to be evicted if bucket size needs to be reduced
  hashEntry_t<succDataContainer*> **table_oldest_entries_;

  
  /*
    Moves the remove_ptr_ to the next table entry that can be deleted
    (i.e., that is not in use by someone outside this class).
  */
  void advance_to_deletable_entry();

  /*
    Creates a container with info about the successors of a state. 
    Such containers play the role of values in this hash table.
  */
  succDataContainer* make_data_cont();


public:

  hash_succ_t() 
    : hash_base_t<succDataContainer*>(), initialized_ (false),
    expansion_stopped_(false), remove_idx_(0), remove_ptr_(NULL), 
    table_emptied_(false), reused_entry_(NULL), reused_entry_value_(NULL)
    { }

  hash_succ_t(const problem_t &problem,  unsigned dimension)
    { 
      initialize(problem, dimension);
    }

  void initialize(const problem_t &problem,  unsigned dimension);
  virtual ~hash_succ_t();

  /*
    Resets the readiness of containers with state successors info. See the 
    documentationn of the succDataContainer class for more information.
  */
  void reset_readiness();
  bool table_emptied() { return table_emptied_; }
  void stop_expansion() { expansion_stopped_ = true; }

  /*
    Reduces the size of the table by the number of bytes specified by the 
    global gpt::successor_hash_reduction_amount variable.
  */
  void reduce_size();  

  /*
    Inserts the given state with an empty container for its successors into 
    the hash table.
  */
  hashEntry_t<succDataContainer*>* insert( const state_t &state, unsigned idx );
  hashEntry_t<succDataContainer*>* insert( const state_t &state )
  {
    return insert(state, hash_value(state));
  } 

  /* 
     Samples a successor of the given state under the given action according
     to the transition function.
  */
  const state_t& sample(const state_t& state, unsigned hash, size_t actIdx)
  {
    return get(state, hash)->value()->sample(actIdx);
  }

  virtual void print( std::ostream &os, const problem_t &problem ) const { }  
};




/*******************************************************************************
 *
 * hash_FH
 *
 ******************************************************************************/

/* 
  This class implements the main hash table of the planner. This hash table has
  a subtable for each number of steps-to-go from 1 to the horizon, containing 
  values of states at the corresponding distance from the horizon. In addition,
  it maintains an instance of the successor hash.
*/ 
 

class hash_FH_t
{
private:

  // Successor hash
  static hash_succ_t hash_succ_;

  // State-value hash tables for each number of steps-to-go from 1 to horizon
  hash_t *hashes_;

  // The distance to horizon
  size_t horizon_;

  // The number of buckets in each of the hash subtables
  unsigned dimension_;

  // Total time spent computing successors of states under different actions
  double totalActTime_;

  // Pointer to the problem instance
  const problem_t *problem_;

  // A helper structure for performing intermediary calculations
  std::pair<state_t*,double> *display_;


public:

  hash_FH_t() : hashes_(NULL), horizon_(0), dimension_(0) { }
  hash_FH_t(const problem_t &problem,  unsigned dimension, 
	                               heuristic_t &heuristic, size_t horizon );
  virtual ~hash_FH_t();

  /*
    Tells whether the hash table corresponding to s steps-to-go is initialized.
  */
  bool initialized(size_t s) 
  { 
    assert(s > 0 && s <= horizon_);
    return hashes_[s-1].initialized();
  }

  /*
    Initializes the hash table corresponding to s steps-to-go.
  */
  void initialize(size_t s) 
  { 
    assert(s > 0 && s <= horizon_);

    if (!initialized(s))
      {
	lashHeuristic_t* heur = new lashHeuristic_t(*problem_);
	heur->set_stepsToGo(s);
	heur->set_hash(this);
	hashes_[s-1].initialize(*problem_, dimension_, *heur);
      }
  }

  /*
    Resets the readiness status of the successor hash.
  */
  void reset_readiness() { hash_succ_.reset_readiness(); }

  /*
    Empties the state-value hash for each number of steps-to-go and resets the 
    readiness of the successor hash.
  */ 
  void cleanup()
  {
    for (size_t i = 0; i < horizon_; i++)
    { 
      gpt::mem_state_value_hash -= hashes_[i].size() * (gpt::heap_overhead
						 + sizeof(hashEntry_t<double>));
      hashes_[i].clear();
    }

    reset_readiness();
  }

  /*
    Retrieves data about the successors of a given state.
  */
  hashEntry_t<succDataContainer*> *get_from_succ_hash(
					    const state_t& state, unsigned hash)
  {
    return hash_succ_.get(state, hash);
  }

  /*
    Samples a successor of the given state ("hash" is the hash value of this 
    state) under the given action.
  */
  const state_t& sample(const state_t& state, unsigned hash, size_t actIdx)
  {
    return hash_succ_.sample(state, hash, actIdx);
  }

  /*
    Stops the growth of the successor hash and reduces its size.
  */
  static bool reduce_succ_hash_size()
  {
    if (hash_succ_.table_emptied())
      {
	return false;
      }

    hash_succ_.stop_expansion();
    hash_succ_.reduce_size();
    return true;
  }

  /*
    Inserts the state into the state-value hash corresponding to the specified
    number of states-to-go.
  */
  hashEntry_t<double>* insert( const state_t &state, size_t stepsToGo )
  {
    return hashes_[stepsToGo-1].insert(state);
  }

  /*
    Finds the table entry for the given state at the given number of 
    states-to-go; if such an entry isn't in the table yet, returns NULL.
    "Hash" is the hash value of the given state.
  */
  hashEntry_t<double>* find(const state_t &state, unsigned hash,
			                                 size_t stepsToGo) const
  {
    return hashes_[stepsToGo-1].find(state, hash);
  }

  hashEntry_t<double>* find(const state_t &state, size_t stepsToGo) const
  {
    return hashes_[stepsToGo-1].find(state, hash_value(state));
  }
  
  /*
    Gets the table entry for the given state at the given number of states-to-go
    "Hash" is the hash value of the given state.
  */
  hashEntry_t<double>* get(const state_t &state, unsigned hash,size_t stepsToGo)
  {
    return hashes_[stepsToGo-1].get(state, hash);
  }

  hashEntry_t<double>* get(const state_t &state, size_t stepsToGo)
  {
    return hashes_[stepsToGo-1].get(state, hash_value(state));
  }

  /*
    Computes the hash value of the given state.
  */
  unsigned hash_value( const state_t &state ) const 
  { 
    return(state.hash_value() % dimension_); 
  }

  /*
    Gets the value of the given state at the given number of states-to-go.
    "Hash" is the hash value of the given state.
  */
  double value( const state_t &state, unsigned hash,  size_t stepsToGo)
  {
    const hashEntry_t<double> *entry = hashes_[stepsToGo-1].get( state, hash );

    if( !entry )
      {  
	return( hashes_[stepsToGo-1].heuristic( state, hash ) );
      }
    else
      return( entry->value() );
  }

  /*
    Prints the contents of all state-value hash subtables.
  */
  void print( std::ostream &os, const problem_t &problem ) const;
 
  /*
    Returns the size of the state-value hash table corresponding to the given
    number of steps-to-go.
  */
  size_t size( size_t stepsToGo ) const 
  { 
    return hashes_[stepsToGo-1].size(); 
  }

  /*
    Computes the total number of entries in all state-value hash subtables.
  */
  size_t size( ) const 
  {
    size_t size = 0;

    for (size_t i = 0; i < horizon_; i++)
      {
	size += hashes_[i].size();
      }

    return size;
  }

  /*
    Determines the greedy-best action in the given state. The Q-value of
    this action is returned in the "val" parameter.
  */
  int bestAction( const state_t &state, unsigned hash, size_t stepsToGo,
			  const problem_t &problem,
		  double &val, hashEntry_t<succDataContainer*> *s = NULL );

  int bestAction( const state_t &state, size_t stepsToGo,
			  const problem_t &problem,
		  double &val, hashEntry_t<succDataContainer*> *s = NULL)
  {
    return bestAction(state, hash_value(state), stepsToGo, problem, val, s);
  }

  /*
    Returns a heuristic value for the given state at the given number of
    steps-to-go. "Hash" specifies the hash value of the given state.
  */
  double heuristic(const state_t &state, unsigned hash, size_t stepsToGo)
  {
    return hashes_[stepsToGo-1].heuristic(state, hash);
  }
};



/*******************************************************************************
 *
 * state hash entry
 *
 ******************************************************************************/

class stateHashEntry_t
{
  const state_t *state_;
  stateHashEntry_t *next_, *prev_;
  friend class stateHash_t;

public:
  stateHashEntry_t() : state_(NULL), next_(0), prev_(0) { }
  stateHashEntry_t( const state_t &state ) : next_(0), prev_(0)
    {
      notify( this, "stateHashEntry_t::stateHashEntry_t(state_t&)" );
      state_ = new state_t( state );
      gpt::mem_state_hash += gpt::heap_overhead +  sizeof(state_t) 
	+ state_->size_in_bytes();
    }

  ~stateHashEntry_t() 
    { 
      delete state_; 
      gpt::mem_state_hash -= gpt::heap_overhead + sizeof(state_t)  
	+ state_->size_in_bytes();
      state_ = NULL;
    }

  const state_t* state( void ) const { return( state_ ); }
  const stateHashEntry_t* next( void ) const { return( next_ ); }
};



/*******************************************************************************
 *
 * state hash
 *
 ******************************************************************************/

class stateHash_t
{
protected:

  unsigned size_;
  unsigned dimension_;
  unsigned *number_;
  stateHashEntry_t **table_;


public:

  stateHash_t() : size_(0), dimension_(0), number_(0), 
    table_(0) { }
  stateHash_t( unsigned dimension );
  ~stateHash_t();
  unsigned hash_value( const state_t &state ) const 
  { 
    return(state.hash_value()%dimension_); 
  }

  stateHashEntry_t* find( const state_t &state, unsigned idx ) const;
  stateHashEntry_t* find( const state_t &state ) const
  {
    return find(state, hash_value(state));
  }

  stateHashEntry_t* insert( const state_t &state, unsigned idx );
  stateHashEntry_t* insert( const state_t &state )
  {
    return insert(state, hash_value(state));
  }

  void insert( stateHashEntry_t *entry, unsigned idx )
    {
      entry->next_ = table_[idx];
      entry->prev_ = NULL;
      if( table_[idx] )
	table_[idx]->prev_ = entry;
      table_[idx] = entry;
      ++number_[idx];
      ++size_;
    }

  void insert( stateHashEntry_t *entry )
  {
    insert(entry, hash_value(*entry->state()));
  }

  void destroy( const state_t &state );
  stateHashEntry_t* get( const state_t &state )
    {
      unsigned idx = hash_value(state);
      stateHashEntry_t *result = find( state, idx );
      return( !result ? insert( state, idx ) : result );
    }

  std::pair<const state_t*, unsigned> get_with_hash( const state_t &state )
  {
    unsigned idx = hash_value(state);
    stateHashEntry_t *result = find( state, idx );
    if (!result)
      {
	result = insert(state, idx );
      }

    return std::pair<const state_t*, unsigned>(result->state(), idx);
  }

  unsigned dimension( void ) const { return( dimension_ ); }
  unsigned size( void ) const { return( size_ ); }
  unsigned diameter( void ) const;
  void print( std::ostream &os, const problem_t &problem ) const;


public: // iterator

  class const_iterator;
  friend class stateHash_t::const_iterator;

  class const_iterator
  {
    const stateHash_t *hash_;
    const stateHashEntry_t *ptr_;
    size_t idx_;

  protected:
    const_iterator( const stateHash_t *shash, int pos ) 
                                                : hash_(shash), ptr_(0), idx_(0)
      {
	if( pos == 0 )
	  {
	    for( ; idx_ < hash_->dimension_; ++idx_ )
	      if( (ptr_ = hash_->table_[idx_]) )
		break;
	  }
	else
	  {
	    idx_ = hash_->dimension_;
	    ptr_ = 0;
	  }
      }

  public:
    const_iterator() : hash_(0), ptr_(0), idx_(0) { }
    const state_t* operator*( void ) const { return( ptr_->state() ); }
    const_iterator operator++( void ) // pre increment
      {
	if( ptr_ != NULL )
	  {
	    if( !(ptr_ = ptr_->next()) )
	      {
		for( ++idx_; idx_ < hash_->dimension_; ++idx_ )
		  if( (ptr_ = hash_->table_[idx_]) )
		    break;
	      }
	  }
	return( *this );
      }

    const_iterator operator++( int ) // post increment
      {
	const_iterator it( *this ); // use default copy const.
	++(*this);
	return( it );
      }

    bool operator==( const const_iterator &it ) const
      {
	return( (ptr_ == it.ptr_) && (idx_ == it.idx_) );
      }

    bool operator!=( const const_iterator &it ) const
      {
	return( (ptr_ != it.ptr_) || (idx_ != it.idx_) );
      }

    friend class stateHash_t;
  };

  const const_iterator begin( void ) const
    {
      return( const_iterator( this, 0 ) );
    }

  const const_iterator end( void ) const
    {
      return( const_iterator( this, 1 ) );
    }
};



/*******************************************************************************
 *
 * expand_all_actions
 *
 ******************************************************************************/

/*
  For each action, builds an empirical distribution over the successors of 
  the given state under that action by sampling a set of states from the 
  action's true successor distribution. The resulting empirical distributions
  are used (e.g., by hash_FH_t::bestAction(.)) to efficiently (although
  approximately) determine the best action in the given state.
*/
void expand_all_actions(const problem_t* problem, const state_t &state, 
			std::pair<state_t*,double> *list, 
			succDataContainer* successors);

#endif // HASH_H
