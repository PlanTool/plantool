#include <assert.h>
#include "prime-impl.hpp"

namespace prime_impl
{
  int saved = 0;

  void
  filter( clauses& cs, set<int>& processed, int var, int desired_vars_init )
  {
    if( desired_vars_init < 0 )
      return;
    vector<clauses::iterator> todel;
    for( clauses::iterator c = cs.begin(); c != cs.end(); c++ )
      for( clause::iterator lit = c->begin(); lit != c->end(); ++lit )
	if( abs(*lit) != abs(var) &&
	    processed.count( abs(*lit)) > 0 &&
	    *lit < desired_vars_init )
	  {
	      todel.push_back(c);
	      break;
	  }
    if(0)
      if( !todel.empty() )
	std::cerr << "Deleting " << todel.size() << " clauses" << std::endl;
      else
	std::cerr << "D" << std::endl;
    saved += todel.size();
    for( vector<clauses::iterator>::iterator del = todel.begin(); 
	 del != todel.end(); del++ )
      cs.erase(*del);
  }

  // In: clauses cs: set of set of int
  //     order: function: var -> number, as vector of pair. Each number is unique
  //
  // Assume: no unit clauses
  //
  // Out: prime implicates, no tautologies
  int
  get(clauses& cs, order_t& order, clauses& pi, int desired_vars_init)
  {
    const bool debug_it = false;
    const bool debug_it2 = false;
    map<int,int> index2var;
    map<int,int> var2index;

    sort( order.begin(), order.end(), Comp() );
    for( order_t::iterator it = order.begin();
	 it != order.end(); ++it )
      {
	int var = it->first;
	int ord = it->second;
	index2var[ord] = var;
	var2index[var] = ord;
      }
    // Ojo: quitar por unit resolution
    pi = cs;
    
    // Put vars in buckets
    map<int, clauses> buckets; // var to clauses
    for( order_t::iterator it = order.begin();
	 it != order.end(); ++it )
      {
	int var = it->first;
	assert( var > 0 );
	int idx = it->second;
	if(debug_it)
	  cout << "Filling bucket for var " << var
	       << " with order " << idx << endl;
	clauses& bucket = buckets[idx] = clauses();
	clauses cs_tmp;
	for( clauses::iterator it = cs.begin();
	     it != cs.end(); ++it )
	  if( it->count(var) + it->count(-var) > 0 )
	    {
	      if(debug_it)
		{
		  cout << "Adding clause: ";
		  pr_clause(*it);
		}
	      bucket.insert(*it);
	    }
	  else
	    cs_tmp.insert(*it);
	if(debug_it)
	  cout << "Have " << bucket.size() << " elements" << endl;
	//cs = cs_tmp;
      }
  
    //assert( cs.size() == 0 ); // no more clauses
  
    set<int> processed;
    // for each bucket
    for( map<int, clauses>::iterator it = buckets.begin(); 
	 it != buckets.end(); ++it )
      {
	int idx = it->first;
	int var = index2var[idx];
	clauses& c = it->second;
	if(debug_it || debug_it2)
	  cout << "Processing bucket " << idx << " for var " << var 
	       << " with size = " << c.size() << endl;
	if(c.size() <= 1) continue;
	filter( c, processed, var, desired_vars_init );
	// For every pair of clauses
	for( clauses::const_iterator c1 = c.begin();
	     c1 != c.end(); ++c1 )
	  {
	    clauses::const_iterator c2 = c1;
	    ++c2;
	    for( ;
		 c2 != c.end(); ++c2 )
	      {
		// Resolve c1 and c2 with var 
		assert( c1->count(var) + c1->count(-var) > 0 );
		assert( c2->count(var) + c2->count(-var) > 0 );
	    
		if( c1->count(var) > 0 && c2->count(var) > 0 )
		  continue;
		if( c1->count(-var) > 0 &&  c2->count(-var) > 0 )
		  continue;
		if(debug_it)
		  {
		    cout << "c1 has " << c1->size() << " c2 has " << c2->size() << endl;
		    cout << "c1 "; pr_clause( *c1 );
		    cout << "c2 "; pr_clause( *c2 );
		  }
		clause c;
		set_union( c1->begin(), c1->end(),
			   c2->begin(), c2->end(),
			   inserter(c, c.begin() ) );
		c.erase( var );
		c.erase( -var );
		if(debug_it)
		  cout << " c has " << c.size() << endl;

		bool tauto = false;
		// Check that c is not a tautology
		for(clause::const_iterator v = c.begin();
		    v != c.end(); ++v )
		  if( c.count( -*v ) > 0 )
		    {
		      // is tautology
		      tauto = true;
		      break;
		    }
	    
		if(tauto)		
		  continue;
	    
		clauses subssum;
		bool subssumed = false;
		for( clauses::const_iterator ci = pi.begin();
		     ci != pi.end(); ++ci )
		  {
		    if( includes( c.begin(), c.end(), ci->begin(), ci->end() ) )
		      {
			subssumed = true;
			break;
		      }
		    else if( includes( ci->begin(), ci->end(), c.begin(), c.end() ) )
		      subssum.insert(*ci);
		  }
	    
		if( !subssumed )
		  {
		    // Erase subssumed
		    for(clauses::iterator del = subssum.begin();
			del != subssum.end(); ++del )
		      pi.erase(*del);

		    // Add to pi
		    pi.insert( c );

		    // add to proper bucket: smallest index
		    int idx2 = numeric_limits<int>::max();
		    if(debug_it)
		      cout << "init put bucket. size = " << c.size() << endl;
		    for( clause::iterator v = c.begin();
			 v != c.end(); ++v )
		      {
			if(debug_it)
			  cout << "v = " << *v << " var2index = " 
			       << var2index[abs(*v)]
			       << " idx2 = " << idx2 << endl; 
			idx2 = min(var2index[abs(*v)], idx2);
		      }
		    assert( idx2 != numeric_limits<int>::max() );
		    //assert( idx2 > idx );
		
		    if(debug_it)
		      {
			cout << "Adding clause: ";
			pr_clause(c);
		      }
		    buckets[idx2].insert( c );
		  }
	      }
	  }
	processed.insert(abs(var));
      }
    return saved;
  }
}
