#include "clauses.hpp"
#include "prime-impl.hpp"

using namespace std;
//using namespace prime_impl;


int
main( int argc, char **argv )
{

  clauses cs;  

  cout << "Introduce theory: each line is a clause" << endl;
  cout << "End with 0. Finish by a line only with 0" << endl;

  int max_var = -1;
  set<int> vars;
  while( true )
    {
      clause c;
      int v = 0;
      cin >> v;
      while( v != 0)
	{
	  int absv = abs(v);
	  max_var = max(max_var, absv);
	  vars.insert(absv);
	  c.insert(v);
	  cin >> v;
	}
      if( c.empty() )
	break;
      else 
	cs.insert(c);
    }

  cout << "Loaded theory " << endl;
  
/*

Order: map from var to int
to follow, iterate over map.
To compare, compare ints.

Param for var for starting to keep.
Before: forget those vars,
after: keep.

*/

  // any order. Every number should be different.
  prime_impl::order_t order; // pair of var, number
  int ord = 11;
  for( set<int>::iterator it = vars.begin();
       it != vars.end(); ++it )
    {
      int var = *it;
      order.push_back( make_pair( var, ord++ ) );
    }
  // Order by number
  

  clauses cs2;
  set<int> unit;
  if( !unit_res(cs, cs2, unit) )
    {
      cout << "Inconsistent theory" << endl;  
      exit(1);
    }
  clauses pi;

  // Calc prime implicates
  prime_impl::get(cs2, order, pi);

  clauses pi2;
  if( !unit_res(pi, pi2, unit) )
    {
      cout << "Inconsistent theory (after pi)" << endl;  
      exit(1);
    }

  
  cout << "Unit: ";
  pr_clause(unit);
  cout << "\n\nPrime implicates:" << endl;
  for(clauses::iterator c = pi2.begin();
      c != pi2.end(); ++c )
    {
      pr_clause(*c);
    }
  cout << "end of prime implicates:" << endl;
  
}

