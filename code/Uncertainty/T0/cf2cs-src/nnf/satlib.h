#ifndef SATLIB_INCLUDE
#define SATLIB_INCLUDE

#include <iostream>
#include <map>
#include <string>
#include <vector>

// forward references
class CSolver;
namespace nnf { class Manager; };

namespace satlib
{
  extern std::map<int,const std::vector<int>*> clauses_;

  inline const std::vector<int>* clause( int index )
  {
    std::map<int,const std::vector<int>*>::const_iterator it = clauses_.find( index );
    return( it != clauses_.end() ? (*it).second : 0 );
  }

  void read_cnf_file( std::istream &is, CSolver &manager );
  void read_nnf_file( std::istream &is, nnf::Manager &manager, std::multimap<int,nnf::node> *olabels = 0 );
}; // satlib namespace

#endif // SATLIB_INCLUDE
