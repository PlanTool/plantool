#include <iostream>
#include <set>
#include <map>
#include <vector>
#include <algorithm>
#include <limits>
#include "clauses.hpp"

namespace prime_impl
{
  using std::set;
  using std::vector;
  using std::map;
  using std::pair;
  using std::min;
  using std::cout;
  using std::endl;
  using std::numeric_limits;

  typedef vector<pair<int,int> > order_t;

  class Comp{
  public:
    int operator()(const pair<int,int>& p1, const pair<int,int>& p2 )
    {
      return p1.second < p2.second;
    }
  };


  // In: clauses cs: set of set of int
  //     order: function: var -> number, as vector of pair. Each number is unique
  //
  // Assume: no unit clauses
  //
  // Out: prime implicates, no tautologies
  int get(clauses& cs, order_t& order, clauses& pi, int desired_vars_init = -1);
}
