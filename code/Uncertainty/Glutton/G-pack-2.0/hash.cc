#include "hash.h"


/*******************************************************************************
 *
 * hash
 *
 ******************************************************************************/

void
hash_t::initialize(const problem_t &problem, unsigned dimension, 
		                                        heuristic_t &heuristic )
{
  hash_base_t<double>::initialize(problem, dimension);
  gpt::mem_state_value_hash += gpt::heap_overhead 
    + dimension_* sizeof(hashEntry_t<double>*);
  gpt::mem_state_value_hash += gpt::heap_overhead 
    + dimension_* sizeof(unsigned);
  heuristic_ = &heuristic;

  if( gpt::verbosity >= 300 )
    std::cout << "<hash>: new hash: dimension = " << dimension_ << std::endl;
}


hash_t::~hash_t()
{
  if (initialized_)
    {
      gpt::mem_state_value_hash -= size_ 
	* (gpt::heap_overhead + sizeof(hashEntry_t<double>));
      gpt::mem_state_value_hash -= gpt::heap_overhead 
	+ dimension_* sizeof(hashEntry_t<double>*);
      gpt::mem_state_value_hash -= gpt::heap_overhead 
	+ dimension_* sizeof(unsigned);
    }

  delete heuristic_;

  if( gpt::verbosity >= 300 )
    std::cout << "<hash>: deleted" << std::endl;
}


hashEntry_t<double>* 
hash_t::insert( const state_t &state, unsigned idx )
{
  unsigned long extra_mem = gpt::heap_overhead 
    + sizeof( hashEntry_t<double> );

  unsigned long projected_mem = gpt::mem_state_hash 
    + gpt::mem_state_value_hash + gpt::mem_successor_hash + extra_mem;

  if ((projected_mem / gpt::tick_size) * gpt::tick_size != gpt::nearest_tick)
    {
      gpt::nearest_tick = (projected_mem / gpt::tick_size) * gpt::tick_size;
      std::cout<<"Total size of the hash tables has exceeded "
	       <<gpt::nearest_tick<<" bytes."<<std::endl;
    }

  if (projected_mem > gpt::total_RAM)
    {
      if (!hash_FH_t::reduce_succ_hash_size())
	{
	  gpt::out_of_memory = true;
	}
    }

  double val = heuristic( state, idx );
  hashEntry_t<double> *entry = new hashEntry_t<double>( state, val );
  gpt::mem_state_value_hash += extra_mem;
  hash_base_t<double>::insert( entry, idx );
  return( entry ); 
}


hashEntry_t<double>* 
hash_t::insert( const state_t &state )
{
  return insert(state, hash_value(state));
}


void
hash_t::print( std::ostream &os, const problem_t &problem ) const
{
  os << "<hash>: table begin" << std::endl;

  if (!initialized())
    {
      std::cout<<"<empty>"<<std::endl;
    }
  else
    { 
      for( unsigned i = 0; i < dimension_; ++i )
	{
	  if( table_[i] ) os << "  bucket[" << i << "] = { ";
	  for( hashEntry_t<double> *ptr = table_[i]; ptr != NULL; 
	                                                      ptr = ptr->next_ )
	    {
	      os << "(" << ptr << ":" << ptr->bits() << ":";
	      if( gpt::verbosity >= 450 )
		ptr->state()->full_print( os, &problem );
	      else
		os << ptr->state()->hash_value() << ":";
	      os << ":" << ptr->value() << ") ";
	    }
	  if( table_[i] ) os << "}" << std::endl;
	}
    }

  os << "<hash>: table end" << std::endl;
}



/*******************************************************************************
 *
 * hash_succ
 *
 ******************************************************************************/

void 
hash_succ_t::initialize(const problem_t &problem,  unsigned dimension)
{
  hash_base_t<succDataContainer*>::initialize(problem, dimension);

  // Add the size of the bucket array and of the bucket size array to the total 
  // size of this table (these arrays are initialized by the default 
  // constructor).
  gpt::mem_successor_hash += gpt::heap_overhead 
    + dimension_* sizeof(hashEntry_t<succDataContainer*>*);
  gpt::mem_successor_hash += gpt::heap_overhead + dimension_* sizeof(unsigned);

  expansion_stopped_ = false;
  remove_idx_ = 0;
  remove_ptr_ = NULL;
  table_emptied_ = false;
  reused_entry_ = NULL;
  reused_entry_value_ = NULL;
  gpt::num_acts = problem.actionsT().size();
  table_oldest_entries_ = (hashEntry_t<succDataContainer*>**) calloc( 
			 dimension_, sizeof(hashEntry_t<succDataContainer*>*)); 
    
  // Add the size of the table_oldest_entries_ array to the total 
  // size of this table
  gpt::mem_successor_hash += gpt::heap_overhead 
    + dimension_ *  sizeof(hashEntry_t<succDataContainer*>*);
  initialized_ = true;
}


hash_succ_t::~hash_succ_t() 
{
  if (initialized_)
    {  
      if (reused_entry_value_ != NULL)
	{
	  delete reused_entry_value_;
	}

      if (reused_entry_ != NULL)
	{
	  delete reused_entry_;
	}

      if (hash_base_t<succDataContainer*>::table_ != NULL)
	{
	  for( unsigned i = 0; 
	       i < hash_base_t<succDataContainer*>::dimension_; ++i )
	    {
	      for( hashEntry_t<succDataContainer*> *ptr = 
		     hash_base_t<succDataContainer*>::table_[i]; ptr != NULL; )
		{
		  hashEntry_t<succDataContainer*> *ptr_helper = ptr;
		  ptr = ptr->next_;

		  // The hash entry itself will be deleted by ~hash_base_t
		  delete ptr_helper->value();

		  // Subtract the size of the entry from  this hash table's 
		  // memory counter. The size of succDataContainer will be 
		  // subtracted from it by ~succDataContainer. 
		  gpt::mem_successor_hash -= 2 * gpt::heap_overhead 
		    + sizeof(hashEntry_t<succDataContainer*>) 
		    + sizeof(succDataContainer);
		}
	    }
	}

      free( table_oldest_entries_ );

      // Subtract the amount of memory occupied by table_oldest_entries_ and
      // the members deleted by ~hash_base_t from this hash table's memory 
      // counter.
      gpt::mem_successor_hash -= gpt::heap_overhead
	+ dimension_* sizeof(hashEntry_t<succDataContainer*>*);
      gpt::mem_successor_hash -= gpt::heap_overhead
	+ dimension_* sizeof(hashEntry_t<succDataContainer*>*);
      gpt::mem_successor_hash -= gpt::heap_overhead 
	+ dimension_* sizeof(unsigned);
    }
}


void 
hash_succ_t::advance_to_deletable_entry()
{
  bool found = false;
  size_t prev_remove_idx = ((remove_idx_ == 0) ? dimension_ - 1 
			                                   : remove_idx_ - 1);
  // Iterate over the table entries, from the least recently to the most
  // recenty added one within each bucket, until you find one suitable for
  // removal. 
  while (remove_ptr_ == NULL)
    {
      // advance to the next bucket
      for (; ; remove_idx_ = (remove_idx_ + 1) % dimension_)
	{
	  // if this bucket is nonempty...
	  if ((remove_ptr_ = table_oldest_entries_[remove_idx_]))
	    {
	      // go through the entries in this bucket...
	      while (remove_ptr_ != NULL)
		{
		  // until you find one that is not currently in use
		  if (!(remove_ptr_->bits() & DELETE_PROTECTED))
		    {
		      found = true;
		      break;
		    }
		  else
		    {
		      remove_ptr_ = remove_ptr_->prev_;
		    }
		}

	      if (found)
		{
		  break;
		}
	    }

	  // If you went through the whole table and didn't find 
	  // an entry suitable for removal, stop. 
	  if ((!found) && prev_remove_idx == remove_idx_)
	    {
	      break;
	    }
	}

      if (found)
	{
	  break;
	}
      else if (prev_remove_idx == remove_idx_)
	{
	  {
	    std::cout<<"<successor hash>: Warning: "
		     <<"the successor hash's size cannot be reduced further."
		     <<std::endl;
	    table_emptied_ = true;
	  }

	  remove_ptr_ = NULL;
	  break;
	}
    }
}


succDataContainer* 
hash_succ_t::make_data_cont()
{
  succDataContainer* s = NULL;
    
  // First, check if the reused_entry_value points to a valid container
  // if so, just return that.
  if (reused_entry_value_ != NULL)
    {
      s = reused_entry_value_;
      reused_entry_value_ = NULL;
      return s;
    }

  // Next, check if the hash table is still allowed to grow; if so, simply
  // create a new container; on this container, set the "owned_by_hash" flag,
  // telling the caller that the caller may not delete this container when
  // the caller is done with it.
  if (!expansion_stopped_)
    {
      s = new succDataContainer();
      gpt::mem_successor_hash += gpt::heap_overhead + sizeof(succDataContainer);
      s->set_owned_by_hash(true);
      return s;
    }

  // Iff the table isn't allowed to grow anymore but doesn't look 
  // completely empty either, try to find an entry to evict in order to 
  // make space for the new container.
  if (!table_emptied_)
    {
      advance_to_deletable_entry();
    }

  // If such an entry couldn't be found, create a new container, but set the
  // "owned_by_hash" flag on it to false. This effectively transfers the 
  // responsibility for the container (in particular, for the memory allocated
  // to it) to the caller. Thus, the hash table can serve requests for new
  // entries even when it isn't allowed to grow -- logically, the memory it 
  // allocates to these new entries doesn't actually belong to the hash table.
  if (remove_ptr_ == NULL)
    {
      s = new succDataContainer();
      s->set_owned_by_hash(false);
      return s;
    }

  // Finally, if an entry to evict could be found, remove this entry from the
  // table, reset the state of its container, and return that container.
  hashEntry_t<succDataContainer*>* ptr_helper = remove_ptr_;
  remove_ptr_ = remove_ptr_->prev_;
  remove_ptr_->next_ = ptr_helper->next_;

  if (ptr_helper->next_ == NULL)
    {
      table_oldest_entries_[remove_idx_] = remove_ptr_;
    }

  s = ptr_helper->value();
  s->reset();
  ptr_helper->prev_ = NULL;
  ptr_helper->next_ = NULL;
  ptr_helper->state_ = NULL;
  ptr_helper->value_ = NULL;
  ptr_helper->reset_bits(0xffffffff);
  reused_entry_ = ptr_helper;	                
  return s;
}


void 
hash_succ_t::reset_readiness() 
{
  for( unsigned i = 0; i < dimension_; ++i )
    {
      for( hashEntry_t<succDataContainer*> *ptr = table_[i]; 
	                                         ptr != NULL; ptr = ptr->next_ )
	{
	  if (ptr->value() != NULL)
	    {
	      ptr->value()->reset_ready();
	    }
	}
    }
}


void 
hash_succ_t::reduce_size()
{
  if (table_emptied_)
    {
      return;
    }

  tms beginTime;
  times(&beginTime);
  unsigned long target_size_in_bytes = 0;

  // Set target_size_in_bytes to the desired size of the hash table
  if (gpt::successor_hash_reduction_amount < gpt::mem_successor_hash)
    {
      target_size_in_bytes = gpt::mem_successor_hash 
	                                - gpt::successor_hash_reduction_amount;
    }

  std::cout<<"<successor hash>: Warning: reducing the successor hash "
	   <<"table size to "<<target_size_in_bytes<<" bytes."<<std::endl;
  assert(dimension_ > 0);
  size_t prev_remove_idx = ((remove_idx_ == 0) ? dimension_ - 1 
			                                     : remove_idx_ - 1);
  bool enough = false;
  size_t dp = 0;
  
  // Keep removing entries from the table until its size reaches the desired 
  // value.
  while (gpt::mem_successor_hash > target_size_in_bytes && !table_emptied_)
    {
      if (remove_ptr_ == NULL)
	{
          // Go to the next valid entry
	  for (; ; remove_idx_ = (remove_idx_ + 1) % dimension_)
	    {
	      if ((remove_ptr_ = table_oldest_entries_[remove_idx_]) 
		                              || prev_remove_idx == remove_idx_)
		{
		  break;
		}
	    }
	}	

      // Remove entries that are currently not in use by other parts of the code
      while (remove_ptr_ != NULL)
	{
	  if (!(remove_ptr_->bits() & DELETE_PROTECTED))
	    {
	      hashEntry_t<succDataContainer*>* ptr_helper = remove_ptr_;
	      remove_ptr_ = remove_ptr_->prev_;
		  
	      if (remove_ptr_ != NULL)
		{
		  remove_ptr_->next_ = ptr_helper->next_;
		}
	      else
		{
		  table_[remove_idx_] = NULL;
		}

	      if (ptr_helper->next_ == NULL)
		{
		  table_oldest_entries_[remove_idx_] = remove_ptr_;
		}

	      delete ptr_helper->value();
	      delete ptr_helper;
	      gpt::mem_successor_hash -= 2 * gpt::heap_overhead 
		+ sizeof(hashEntry_t<succDataContainer*>) 
		+ sizeof(succDataContainer);
	      --number_[remove_idx_];
	      --size_;
			
	      if (gpt::mem_successor_hash <= target_size_in_bytes)
		{
		  enough = true;
		  break;
		}
	    }
	  else
	    {
	      remove_ptr_ = remove_ptr_->prev_;
	      dp++;
	    }
	}

      if (remove_ptr_ == NULL && prev_remove_idx == remove_idx_)
	{
	  // Ignore the delete-protected entries -- at any point, there are
	  // only a few of them,
	  std::cout<<"<successor hash>: Warning: the successor hash table's "
		   <<"size cannot be reduced any further."
		   <<std::endl;
	  table_emptied_ = true;
	  break;
	}

      if (enough)
	{
	  break;
	}

      remove_idx_ = (remove_idx_ + 1) % dimension_;
    }
}


hashEntry_t<succDataContainer*>* 
hash_succ_t::insert( const state_t &state, unsigned idx )
{
  hashEntry_t<succDataContainer*> *entry = NULL;
  bool newlyCreated = false;

  if (table_emptied_)
    {
      entry = new hashEntry_t<succDataContainer*>(state, make_data_cont());
      newlyCreated = true;
    }
  else if (!expansion_stopped_)
    {
      entry = new hashEntry_t<succDataContainer*>(state, make_data_cont());
      newlyCreated = true;
      
    }
  else // !table_emptied_ && expansion_stopped_
    {
      // Since the table isn't allowed to grow anymore, we need to find 
      // an entry to evict in order to make space for a new one.
      if (reused_entry_ != NULL)
	{
	  entry = reused_entry_;
	  reused_entry_ = NULL;
	  entry->state_ = state_t::get_state( state );
	  entry->value_ = make_data_cont();
	}
      else
	{
	  advance_to_deletable_entry();
	  
	  if (remove_ptr_ == NULL)
	    {
	      entry = new hashEntry_t<succDataContainer*>(
						       state, make_data_cont());
	      newlyCreated = true;
	    }
	  else
	    {
	      entry = remove_ptr_;
	      remove_ptr_ = remove_ptr_->prev_;
		  
	      if (remove_ptr_ != NULL)
		{
		  remove_ptr_->next_ = entry->next_;
		}
	      else
		{
		  table_[remove_idx_] = NULL;
		}

	      if (entry->next_ == NULL)
		{
		  table_oldest_entries_[remove_idx_] = remove_ptr_;
		}

	      // Reset the state of the found entry container. We will
	      // use this container to store information about the
	      // successors of the specified state.
	      entry->state_ = state_t::get_state( state );
	      entry->value_->reset();
	      entry->reset_bits(0xffffffff); 
	      entry->value_->set_owned_by_hash(true);
	    }
	}
    }

  // If the memory of this entry is managed by this hash table, insert
  // this entry into the hash table.
  if (entry->value_->owned_by_hash())
    {
      if (newlyCreated)
	{
	  gpt::mem_successor_hash += gpt::heap_overhead 
	    + sizeof(hashEntry_t<succDataContainer*>);
	}

      entry->next_ = table_[idx];
      entry->prev_ = NULL;
      if( table_[idx] )
	{
	  table_[idx]->prev_ = entry;

	  if (table_oldest_entries_[idx] == NULL)
	    {
	      table_oldest_entries_[idx] = entry;
	    }
	}
      else
	{
	  table_oldest_entries_[idx] = entry;
	}

      table_[idx] = entry;
      ++number_[idx];
      ++size_;
    }

  return( entry );
}



/*******************************************************************************
 *
 * hash_FH
 *
 ******************************************************************************/


hash_succ_t hash_FH_t::hash_succ_ = hash_succ_t();

hash_FH_t::hash_FH_t(const problem_t &problem, unsigned dimension, 
		     heuristic_t &heuristic, size_t horizon): horizon_(horizon),
	      dimension_(prime(dimension)), totalActTime_(0), problem_(&problem)
{ 
  hash_succ_.initialize(problem, dimension);
  display_ = new std::pair<state_t*,double>[DISP_SIZE];

  for( size_t j = 0; j < DISP_INT_SIZE; ++j )
	display_[j].first = new state_t;

  hashes_ = new hash_t[horizon_];
  gpt::mem_state_value_hash += horizon_ * sizeof(hash_t);
}



hash_FH_t::~hash_FH_t()
{
  for( size_t j = 0; j < DISP_INT_SIZE; ++j )
    delete display_[j].first;

  delete [] display_;
  size_t total = 0;

  for (size_t i = 0; i < horizon_; i++)
    {
      std::cout<< "Number of state-value hash table entries at distance "
	       << i+1 <<" from the horizon: "
	       << hashes_[i].size()
	       << (hashes_[i].initialized() ? "" : "(not initialized)")
	       << std::endl;
      total += hashes_[i].size();
    }

  delete [] hashes_;
  gpt::mem_state_value_hash -= horizon_ * sizeof(hash_t);

  if( gpt::verbosity >= 300 )
    std::cout << "<hash_FH>: deleted" << std::endl;
}


void
hash_FH_t::print( std::ostream &os, const problem_t &problem ) const
{
  for (size_t i = 0; i < horizon_; i++)
    {
      os<<"Entries at distance "<<i+1<<" from the horizon: ********"<<std::endl;
      hashes_[i].print(os, problem);
    }
}


int
hash_FH_t::bestAction( const state_t &state, unsigned hash, size_t stepsToGo, 
		       const problem_t &problem, double &val, 
		       hashEntry_t<succDataContainer*> *s )
{
  int maxa = -1;
  double max = -1000000000;
  bool some = false;

  hashEntry_t<succDataContainer*> *succ_entry = ((s == NULL) ? 
				     get_from_succ_hash(state, hash) : s);
  succDataContainer* succ = succ_entry->value();
  succ_entry->set_bits(DELETE_PROTECTED);

  if (!succ->all_ready())
    {
      // If we haven't sampled enough successors of the given state under
      // every action, sample more.
      expand_all_actions(problem_, state, display_, succ);
    }

  double equal_ctr = 1;

  // For every action...
  for( size_t i = 0; i < problem.actionsT().size(); ++i )
    {
      double qvalue = 0;
      size_t num_succ = 0;
      tms start;
      times(&start);
      assert(stepsToGo > 0);

      if (stepsToGo > 1)
	{
	  // ... compute the expected value of this action (i.e., its
	  // immediate reward plus the average over the successors under
	  // this action).
	  for (succDataContainer::succHash_t::const_iterator it 
		           = succ->succ_begin(i); it != succ->succ_end(i); it++)
	    {
	      qvalue += (it.value() * (value(*(it.state()), 
		      it.hash_value(), stepsToGo - 1) + succ->get_imm_rew(i)));
	      num_succ += it.value();
	    }
	}
      else if (stepsToGo == 1)
	{
	  // If there is just one more step to go, the expected value 
	  // of any action is simply that action's immediate reward.
	  qvalue = succ->get_imm_rew(i);
	  num_succ = 1;
	}
	  
      assert(num_succ > 0);
      qvalue = qvalue / num_succ;

      // If this is the first action we've seen, or its value is larger than
      // the value of any previously seen action, memorize its value and its
      // index.
      // 
      // Of all actions that have the highest Q-value, we want to pick one
      // uniformly at random. This is handled by the case qvalue == max
      // in the below if-else statement.
      if (!some)
	{
	  some = true;
	  max = qvalue;
	  maxa = i; 
	  equal_ctr = 1;
	}
      else if (qvalue == max )
	{
	  equal_ctr++;

	  if (drand48() > (equal_ctr - 1) / equal_ctr)
	    {
	      maxa = i;
	    }
	}
      else if (qvalue > max)
	{
	  max = qvalue;
	  equal_ctr = 1;
	  maxa = i;
	}

      tms finish;
      times(&finish);
      totalActTime_ += elapsed_time(start, finish);
    }

  val = max;
  
  if (s == NULL && !succ->owned_by_hash())
    {
      // If the successor hash gave the ownership of the succ_entry 
      // container to us (the caller), delete it. 
      delete succ_entry;
      delete succ;
    }
  else
    {
      // Otherwise, tell the successor hash that we are not using 
      // the container anymore
      succ_entry->reset_bits(DELETE_PROTECTED);
    }

  assert(maxa != -1);
  return( maxa );
}



/*******************************************************************************
 *
 * state hash
 *
 ******************************************************************************/

stateHash_t::stateHash_t( unsigned dimension )
{
  size_ = 0;
  dimension_ = prime( dimension );
  table_ = (stateHashEntry_t**)calloc( dimension_, sizeof(stateHashEntry_t*) );
  gpt::mem_state_hash += gpt::heap_overhead 
    + dimension_ * sizeof(stateHashEntry_t*); 

  for (size_t i = 0; i < dimension_; i++)
    {
      table_[i] = NULL;
    }

  number_ = (unsigned*)calloc( dimension_, sizeof(unsigned) );
  gpt::mem_state_hash += gpt::heap_overhead + dimension_ * sizeof(unsigned);

  for (size_t i = 0; i < dimension_; i++)
    {
      number_[i] = 0;
    }
  
  if( gpt::verbosity >= 300 )
    std::cout << "<state-hash>: new hash: dimension = " 
	      << dimension_ << std::endl;
}


stateHash_t::~stateHash_t()
{
  for( unsigned i = 0; i < dimension_; ++i )
    for( stateHashEntry_t *ptr = table_[i]; ptr != NULL; )
      {
	stateHashEntry_t *next = ptr->next_;
	gpt::mem_state_hash -= gpt::heap_overhead + sizeof(*ptr);
	delete ptr;
	ptr = next;
      }

  free( table_ );
  gpt::mem_state_hash -= gpt::heap_overhead 
    + dimension_ * sizeof(stateHashEntry_t*);
  free( number_ );
  gpt::mem_state_hash -= gpt::heap_overhead 
    + dimension_ * sizeof(unsigned);

  if( gpt::verbosity >= 300 )
    std::cout << "<state-hash>: deleted" << std::endl;
}


stateHashEntry_t* 
stateHash_t::find( const state_t &state, unsigned idx ) const
  {
    for( stateHashEntry_t *ptr = table_[idx]; ptr != NULL; ptr = ptr->next_ )
      {
	if( *(ptr->state_) == state )
	  {
	    return( ptr );
	  }
      }
    return( NULL );
  }


stateHashEntry_t* 
stateHash_t::insert( const state_t &state, unsigned idx )
  {
    unsigned long extra_mem = 2 * gpt::heap_overhead 
      + sizeof(stateHashEntry_t) + state.size_in_bytes();
    unsigned long projected_mem = gpt::mem_state_hash 
      + gpt::mem_state_value_hash + gpt::mem_successor_hash + extra_mem;

    if ((projected_mem / gpt::tick_size) * gpt::tick_size != gpt::nearest_tick)
      {
	gpt::nearest_tick = (projected_mem / gpt::tick_size) * gpt::tick_size;
	std::cout<<"Total size of the hash tables has exceeded "
		 <<gpt::nearest_tick<<" bytes."<<std::endl;
      }

    if (projected_mem > gpt::total_RAM)
      {
	if (!hash_FH_t::reduce_succ_hash_size())
	  {
	    gpt::out_of_memory = true;
	  }
      }

    stateHashEntry_t *entry = new stateHashEntry_t( state );
    gpt::mem_state_hash += gpt::heap_overhead + sizeof(stateHashEntry_t);
    insert( entry, idx );
    return( entry );
  }


void 
stateHash_t::destroy( const state_t &state )
{
  unsigned idx = hash_value( state );
  size_t num = 0;
  for( stateHashEntry_t *ptr = table_[idx]; ptr != NULL; ptr = ptr->next_ )
    {
      if( *(ptr->state_) == state )
	{
	  if (ptr->prev_)
	    {
	      ptr->prev_->next_ = ptr->next_;
	    }
	  if (ptr->next_)
	    {
	      ptr->next_->prev_ = ptr->prev_;
	    }

	  // If you deleted the first entry in a bucket, set the bucket
	  // pointer to point at the entry that follows the deleted one.
	  if (num == 0)
	    {
	      table_[idx] = ptr->next_;
	    }

	  gpt::mem_state_hash -= (2 * gpt::heap_overhead 
			        + sizeof(*ptr) + ptr->state()->size_in_bytes());
	  delete ptr;
	  --number_[idx];
	  --size_;
	  break;
	}

      num++;
    }
}


unsigned
stateHash_t::diameter( void ) const
{
  unsigned result = 0;
  for( unsigned i = 0; i < dimension_; ++i )
    result = (number_[i] > result ? number_[i] : result);
  return( result );
}


void
stateHash_t::print( std::ostream &os, const problem_t &problem ) const
{
  os << "<state-hash>: table begin" << std::endl;
  for( unsigned i = 0; i < dimension_; ++i )
    {
      if( table_[i] ) os << "  bucket[" << i << "] = { ";
      for( stateHashEntry_t *ptr = table_[i]; ptr != NULL; ptr = ptr->next_ )
	os << "(" << ptr << ":" << ptr->state()->hash_value() << ") ";
      if( table_[i] ) os << "}" << std::endl;
    }
  os << "<state-hash>: table end" << std::endl;
}


/*******************************************************************************
 *
 * successor data container
 *
 ******************************************************************************/

void
succDataContainer::insert(size_t actIdx, const state_t& state)
{
  std::pair<const state_t*,unsigned> sh = state_t::get_state_and_hash( state );
  succHashEntry_t* se = successors_[actIdx].find(sh.first);
   
  if (se == NULL)
    {
      successors_[actIdx].insert(sh.first, sh.second);
    }
  else
    {
      se->counter()++;
    }

  counts_[actIdx]++;
}


const state_t& 
succDataContainer::sample(size_t actIdx)
{
  assert(counts_[actIdx] > 0);  
  size_t ctr = 0;
  size_t s = floor(drand48() * counts_[actIdx]);

  for (succHash_t::const_iterator it = successors_[actIdx].begin(); 
                                          it != successors_[actIdx].end(); it++)
    {
      ctr += it.value();

      if (ctr > s)
	return *(it.state());
    }

  return *(successors_[actIdx].begin().state());
}



/*******************************************************************************
 *
 * expand_all_actions
 *
 ******************************************************************************/

/* 
  This method produces an empirical successor distribution for each action in
  the way described in detail in Kolobov, Dai, Mausam, Weld "Reverse Iterative 
  Deepening for Finite-Horizon MDPs with Large Branching Factors', ICAPS-2012
  and briefly summarized below. Remember that in a single invocation of this 
  method, we want to produce gpt::num_succ_per_expansion (not necessarily 
  distinct) samples from each action's true successor distribution and add them 
  to the existing samples (if any) to form an empirical successor distribution 
  for that action. 

  First, we will produce gpt::num_succ_per_expansion sample states for the 
  noop's action empirical distribution. To do so, we will take  
  the noop action's (proper) effects and figure out which ones apply in the 
  given state. Only those will be used to generate the samples. Next, we will 
  divide the applicable effects into deterministic and probabilistic ones.
  Finally, to efficiently generate gpt::num_succ_per_expansion samples,
  we will first apply all the deterministic effects to the given state and make
  gpt::num_succ_per_expansion copies of the result. Intuively, this lets us 
  apply the deterministic effects only once instead of doing it 
  gpt::num_succ_per_expansion times. Then we will apply all the probabilistic
  effects once to each copy from the previous step. This gives us the 
  desired gpt::num_succ_per_expansion noop samples.

  To produce gpt::num_succ_per_expansion samples for each of the non-noop 
  actions, for each such action we will make a copy of all samples generated 
  for the noop, and then apply the proper effects of the non-noop action once 
  to each of the samples.

  Note that the samples for non-noop actions generated as above are biased,
  since they are all produced from the same set of noop samples. This is a
  price we pay for efficiency. The above method of sampling is much faster than
  constructing samples for each action completely independently, because
  in many problems, the noop action effects are shared by all other actions 
  and are responsible for most of the changes due to any given action. This
  makes intuitive sense -- the noop effects capture what happens if an action
  does not affect a state variable. Now, non-noop actions usually purposefully
  affect only a few state variables (the proper ones for that action), 
  leaving the rest to behave the same way as they would under the noop action. 
  Therefore, generating samples for a non-noop action requires only an 
  incremental effort (resampling that action's proper state variables) if the
  samples for the noop action are available. To summarize, generating samples
  for the noop is the most expensive part of this procedure.
*/
void 
expand_all_actions(const problem_t* problem, const state_t &state, 
	        std::pair<state_t*,double> *list, succDataContainer* successors)
{
  // Maximum number of successors that will be sampled during this call for 
  // each action in the given state.
  size_t lim = gpt::num_succ_per_expansion;

  // An identifier for the noop action
  size_t noopIdx = problem->noop_idx();

  // The mapping from the noop action's proper variables to the sets of its
  // proper effects (the sets of effects affecting each of the proper 
  // variables.
  std::map<ushort_t, std::list<const effect_t*> >* proper_noop_effects 
                     = problem->actionsT()[noopIdx]->get_proper_vars_and_effs();
  
  // The list of the noop action's probabilistic effeccts.
  effectList_t applicable_prob;

  // The list of the noop action's deterministic effects.
  effectList_t applicable_det;

  // The variable that will track the immediate reward from applying various
  // actions.
  ValueMap vm;

  // A dummy state variable needed to fill a parameter in various method calls.
  state_t dummy;

  // Go through all of the state variable affected by noop's effects (all of 
  // them are proper for noop)
  for (std::map<ushort_t, std::list<const effect_t*> >::iterator it 
	 = proper_noop_effects->begin(); it != proper_noop_effects->end(); it++)
    {
      int ctr = 0;

      // For each of noop's proper variables, examine the set of noop's effects
      // that affect that variable. Remember (or see the documentation for the
      // find_proper_vars_and_effs(.) method in actions.h) that the effects
      // in this set are mutually exclusive -- exactly one of them applies in a
      // any state. We need to find which one it is for the state we are given.
      for (std::list<const effect_t*>::iterator it_l = it->second.begin(); 
	                                       it_l != it->second.end(); it_l++)
	{
	  ctr++;

	  if ((*it_l)->applicable(state))
	    {
	      // Once we find the applicable one, we need to determine its type.
	      // If it's a conditional one, consider its subeffect, otherwise
	      // consider the effect itself.
	      const conditionalEffect_t* c 
		             = dynamic_cast<const conditionalEffect_t*> (*it_l);
	      const effect_t* e = (c == NULL ? *it_l : c->effect());

	      // Make sure this is not a reward effect -- this is not supposed
	      // to happen.
	      assert(!dynamic_cast<const assignmentEffect_t*> (e));

	      // Figure out if it's a deterministic or probabilistic one and 
	      // store it in the appropriate list.
	      if (e->is_det())
		{
		  applicable_det.insert(e);
		}
	      else
		{
		  applicable_prob.insert(e);
		}

	      break;
	    }	    
	}
    }

  // Apply the noop action's reward effects to determine this action's reward.
  vm[problem->app_] = 0;
      
  std::list<const effect_t*>* noopRewardEffs 
                              = problem->actionsT()[noopIdx]->get_reward_effs();

  for (std::list<const effect_t*>::iterator it_eff = noopRewardEffs->begin(); 
                                      it_eff != noopRewardEffs->end(); it_eff++)
    {
      (*it_eff)->affect( state, dummy, &vm);
    }

  // Now, apply the noop's deterministic affects to our given state and make
  // gpt::num_succ_per_expansion copies of the result; to each of the result,
  // apply all of noop's probabilistic effects. Voila, we have the successor
  // samples under noop.
  state_t temp_state = state;
  applicable_det.affect( state, temp_state, &vm);

  for( size_t i = 0; i < lim; ++i )
    {      
      *list[i].first = temp_state;
      applicable_prob.affect( state, *list[i].first, &vm);
    }

  // If the noop action doesn't yet have enough samples for its empirical
  // successor distribution or its immediate reward hasn't been recorded
  // previously, store its reward and samples that we have just 
  // computed.
  if (!successors->get_ready(noopIdx))
    {
      size_t old_size = successors->get_num_succ(noopIdx);
      successors->set_imm_rew(noopIdx, vm[problem->app_]);

      for( size_t i = 0; i < lim; ++i )
	{
	  successors->insert(noopIdx, *list[i].first);
	}

      size_t new_size = successors->get_num_succ(noopIdx);

      // Determine if noop's empirical successor distribution is based on a 
      // sufficient number of state samples, and we don't need to generate
      // more for it in the future.
      //
      // There are many ways of deciding how much data are "enough" to make 
      // an empirical distribution an accurate representation of the true one.
      // In this implementation, we use two heuristic. First, it is considered
      // "good enough" if its support reaches some predefined size, 
      // gpt::max_distrib_support. Since a good general value for 
      // gpt::max_distrib_support is hard and error-prone to guess (the true 
      // distribution may have a smaller support than gpt::max_distrib_support,
      // in which case this stopping condition will never be met), we have a 
      // second termination criterion. Namely, sampling stops when sampling
      // another batch of gpt::num_succ_per_expansion successors increases the
      // size of empirical distribution's support by no more than 
      // gpt::min_support_growth_rate. Intuitively, this means that the
      // empirical distribution's support is more or less the same as
      // the true distribution's and hence, hopefully, that all regions of the 
      // true distribution are reflected relatively well.
      //
      // Although there are certainly cases where this combination of 
      // termination conditions will fail to do well, in our experiments
      // it proved quite robust.
      if (new_size - old_size < gpt::min_support_growth_rate 
	                                || new_size >= gpt::max_distrib_support)
	{
	  successors->set_ready(noopIdx, true);
	}
    }

  // Now, generate samples for the remaining actions
  for( size_t k = 0; k < problem->actionsT().size(); k++ )
    {
      // Skip the noop action and those actions that don't need any more samples
      if (k == noopIdx || successors->get_ready(k))
	{
	  continue;
	}

      // Compute the action's immediate reward and record it.
      vm[problem->app_] = 0;
      std::list<const effect_t*>* rewardEffs 
	                            = problem->actionsT()[k]->get_reward_effs();

      for (std::list<const effect_t*>::iterator it_eff = rewardEffs->begin(); 
	                                  it_eff != rewardEffs->end(); it_eff++)
	{
	  (*it_eff)->affect( state, dummy, &vm);
	}
    
      successors->set_imm_rew(k, vm[problem->app_]); 

      // As we did for noop, go through the proper variables of this action,
      // figure out the proper effect for each of them that is applicable in
      // our current state, and remember it.
      std::map<ushort_t, std::list<const effect_t*> >* proper_effects 
	                   = problem->actionsT()[k]->get_proper_vars_and_effs();
      std::map<ushort_t, const effect_t*> applicable_nonnoop;

      for (std::map<ushort_t, std::list<const effect_t*> >::iterator it 
	           = proper_effects->begin(); it != proper_effects->end(); it++)
	{
	  for (std::list<const effect_t*>::iterator it_l = it->second.begin(); 
	                                       it_l != it->second.end(); it_l++)
	    {
	      if ((*it_l)->applicable(state))
		{
		  const conditionalEffect_t* c 
		             = dynamic_cast<const conditionalEffect_t*> (*it_l);
		  
		  if ( c != NULL )
		    {
		      applicable_nonnoop[it->first] = c->effect();
		    }
		  else
		    {
		      applicable_nonnoop[it->first] = *it_l;
		    }

		  break;
		}
	    }
	}

      size_t old_size = successors->get_num_succ(k);

      // Now, create a copy of every sample generate for noop -- we will use 
      // them to generate samples for the current action by re-sampling this
      // action's proper variables.
      for( size_t i = 0; i < lim; ++i )
	{
	  state_t init = *list[i].first;

	  // For each noop sample, examine each of the current action's proper
	  // variables and set their values to be the same as their values in 
	  // our given state (the state whose successors we are sampling). Then,
	  // apply the current action's proper effect appropriate for the 
	  // current state that affects this variable. Add the resulting 
	  // sample to the action's empirical successor distribution.
	  for (std::map<ushort_t, const effect_t*>::iterator it 
	     = applicable_nonnoop.begin(); it != applicable_nonnoop.end(); it++)
	    {
	      state.holds(it->first) ? init.add(it->first) 
		                                        : init.clear(it->first);
	      it->second->affect(state, init, &vm);
	    }

	  successors->insert(k, init);
	}

      size_t new_size = successors->get_num_succ(k);

      // Finally, using the combination of conditions described previously
      // in this method, determine whether we will need to generate more samples
      // for this action's empirical successor distribution.    
      if (new_size - old_size < gpt::min_support_growth_rate 
	                                || new_size >= gpt::max_distrib_support)
	{
	  successors->set_ready(k, true);
	}
    }
}



