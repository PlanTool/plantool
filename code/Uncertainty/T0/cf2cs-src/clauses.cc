#include <iostream>
#include "clauses.hpp"

void
pr_clause( const clause& c )
{
  for(clause::iterator v = c.begin();
      v != c.end(); ++v )
    std::cout << *v << ", ";
  std::cout << std::endl;
}

void
pr_clauses( const clauses& cs )
{
  std::cout << "clauses:" << std::endl;
  for(clauses::iterator c = cs.begin();
      c != cs.end(); ++c )
    pr_clause(*c);
}


// In: clauses
// In & Out: unit clauses
// Out: res: clauses after unit resolution
bool
unit_res(const clauses& cs, clauses& res, set<int>& unit)
{
  res.clear();

  for(clauses::iterator c = cs.begin();
      c != cs.end(); ++c )
    if(c->size() == 1)
      unit.insert(*c->begin());
    else
      res.insert(*c);

  clauses cpy = res;
  bool new_unit; 
  do
    {
      res.clear();
      new_unit = false;
      for(clauses::iterator c = cpy.begin();
	  c != cpy.end(); ++c )
	{
	  bool add = true;
	  clause c2 = *c;
	  for(set<int>::iterator u = unit.begin();
	      u != unit.end(); ++u )
	    {
	      if( c2.count( *u ) > 0 )
		{
		  add = false;
		  break;
		}
	      if( c2.count( -*u ) > 0 )
		{
		  c2.erase( -*u );
		  if( c2.empty() ) // empty clause
		    return false;
		}
	    }
	  
	  if( c2.size() == 1 )
	    {
	      unit.insert(*c2.begin());
	      new_unit = true;
	    }
	  else if( add )
	      res.insert(c2);

	}
      cpy = res;
    }
  while(new_unit);
  
  const bool debug_unit = false;
  if(debug_unit)
    {
      std::cout << "Unit: ";
      pr_clause(unit);
      std::cout << "Clauses" << std::endl;
      for(clauses::iterator it = res.begin();
	  it != res.end(); ++it )
	pr_clause(*it);
      std::cout << "end of Clauses" << std::endl;
    }
  for(set<int>::iterator u = unit.begin();
      u != unit.end(); ++u )
    if( unit.count( -*u ) > 0 )
      return false;
  return true;
}
