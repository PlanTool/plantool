/*******************************************************************************
 **
 **   Macros
 **
 ******************************************************************************/

#ifndef __PLANNER_H
#define __PLANNER_H

#include "parser.h"

#define MIN(x,y)        ((x)>(y)?(y):(x))
#define MAX(x,y)        ((x)>(y)?(x):(y))
#define fatal(code)     _fatal( (code), NULL, __FILE__, __LINE__ )
#define fatal1(code,s)  _fatal( (code), (s), __FILE__, __LINE__ )


/*******************************************************************************
 **
 **   Constants
 **
 ******************************************************************************/

#define MAXPARAMETERS        20
/* #define MAXSCHEMA            1024 */
#define MAXSCHEMA            65536
#define ATOMHASHSIZE         1021            // prime nearest to 1K
//#define ATOMHASHSIZE         9973            // prime nearest to 10K
#define INCRATE              (1.5)           // increase rate for dynamic structures

// PDDL requirements
#define REQ_EQUALITY         1
#define REQ_TYPING           2
#define REQ_STRIPS           3
#define REQ_CONDITIONAL_EFFECTS    4
#define REQ_NEGATIVE_PRECONDITIONS 5

// exit codes
#define noError              00
#define noMoreMemory         10
#define maxSchema            20
#define maxAtoms             30

// verbosity
#define LEVEL0               0
#define LEVEL1               4
#define LEVEL2               9
#define LEVEL3               14
#define LEVEL4               19
#define LEVEL5               24
#define LEVEL6               29

#include <iostream>
#include <algorithm>

/*******************************************************************************
 **
 **   Structures
 **
 ******************************************************************************/

class procRegister_t
{
public:
  char *procedure;
  float diffTime;
  procRegister_t *next;
};

class iatom_t
{
public:
  int  idx;
  char name[128];
  int  parameters[MAXPARAMETERS];
  iatom_t *next;
};

class atom_t 
{
public:
  unsigned pack;
};

class lit_effect_t : public std::vector<std::pair<formula_t*,std::pair<nnf::node,nnf::node> > >
{
};

class grounded_schema_t
{
public:
  bool active_;
  int schema_;
  int *parameters_;
};

class operator_t
{
public:
  char *name;
  int schema_;
  nnf::node cnf_;
  lit_effect_t *frame_;
};

extern bool use_delete_free;

class When
{
public:
  std::set<int> prec; 
  std::set<int> eff;
  size_t father;
  size_t index;
  string comment;
  mutable bool used;
  void clear() {
    prec.clear();
    eff.clear();
  }
  void dump( ) const
  {
    dump( std::cout );
  }
  void dump( ostream& out ) const
    {
      out << "When: father = " << father
	  << " index = " << index << endl
	  << "prec: ";
      for( set<int>::const_iterator it = prec.begin();
	   it != prec.end(); it++ )
	out << *it << " ";
      out << endl << "eff: ";
      for( set<int>::const_iterator it = eff.begin();
	   it != eff.end(); it++ )
	out << *it << " ";
      out << endl << "comment: " << comment << endl;
    }
  static const bool p = false;
//   ~When() { 
//     if(p)
//       cout << "When " << this 
// 	   << " con index " << index << " destruido" << endl;
//   } 
  When():father(0), index(0), used(true) {
    prec.clear();
    eff.clear();
    if(p)
      cout << "When " << this 
	   << " con index " << index << " creado sin copia" << endl;
  } 
//   void do_copy(const When& orig) 
//   {
//     index = orig.index; 
//     father = orig.father;
//     prec = orig.prec;
//     eff = orig.eff;
//     comment = orig.comment;
//     used = orig.used;
//   }
//   When(const When& orig) 
//     {
//       do_copy(orig);
//       if(p)
// 	cout << "When " << this 
// 	     << " con index " << index << " creado" 
// 	     << " copia de " << &orig << endl;
//     }
//   When& operator=(const When& orig) 
//     {
//       do_copy(orig);
//       if(p)
// 	cout << "When " << this 
// 	     << " con index " << index << " creado" 
// 	     << " copia (operator=) de " << &orig << endl;
//       return *this;
//     }

  static const bool debug = false;
  void dumpPDDL( ostream& outd, string (*name_l)( int lit ), string ind, 
		 bool observation = false ) const
    {
      observation = false; // Ojo: al imprimir siempre es clasico. Quitar!
      bool old_used = used;
      if( use_delete_free )
	// If effects are all negative, comment out
	{
	  bool one_negative = false;
	  for( set<int>::const_iterator ai = eff.begin(); ai != eff.end(); ++ai )
	    if( *ai > 0 )
	      one_negative = true;
	  if(!one_negative)
	    used = false;
	}

      string comm;
      if( !used )
	comm = ";";
      if( debug )
	{
	  outd << ind << "; When: father = " << father
	       << " index = " << index << endl;
	}
      outd << ind << "; " << comment << endl;
      outd << ind << comm << "(when ";


      // Cond:
      //assert( !w.prec.empty());
      if( prec.size() > 1 )
	outd << " (and ";
      if( prec.empty() )
	outd << " <PREC EMPTY>"; 
      for( set<int>::const_iterator ai = prec.begin(); ai != prec.end(); ++ai )
	outd << name_l(*ai) << " "; 
      if( prec.size() > 1 )
	outd << ")"; 
      
      //outd << "   "; 
      outd << endl << ind << "  " << comm;
      // Effect:
      //assert( !w.eff.empty());

      if( eff.empty() )
	outd << " <EFF EMPTY>"; 
      if( observation )
	{
	  outd << " (and ";       
	  for( set<int>::const_iterator ai = eff.begin(); ai != eff.end(); ++ai )
	    {
	      outd << name_l(*ai) << " "; 
	      outd << name_l(-*ai) << " "; 
	    }
	  outd << ")";
	}
      else
	{
	  if( eff.size() > 1 )
	    outd << " (and ";       
	  for( set<int>::const_iterator ai = eff.begin(); ai != eff.end(); ++ai )
	    if( *ai > 0 || !use_delete_free )
	      outd << name_l(*ai) << " "; 
	  if( eff.size() > 1 )
	    outd << ")";
	}
      outd << ")" << endl;
      outd << endl;
      used = old_used;
    }

};

void di( ostream& o, size_t z );
extern vector<When*> whens;

class Conds
{
 public:
  vector<When> conds;
  typedef vector<When>::iterator iterator;
  typedef vector<When>::const_iterator const_iterator;
  typedef map<set<int>, size_t> myhash;
  myhash set2index;

  iterator begin()
  {
    return conds.begin();
  }
  const_iterator begin() const
  {
    return conds.begin();
  }
  iterator end() 
  {
    return conds.end();
  }
  const_iterator end() const
  {
    return conds.end();
  }
  size_t size() const
  {
    return conds.size();
  }
  void clear()
  {
    set2index.clear();
    conds.clear();
  }

  When& operator[]( const size_t index )
    {
      return conds[index];
    }
  When& back() 
  {
    return conds.back();
  }

  // Deprecated, better to add a full When
  void addWhen(size_t index)
  {
    assert(0!=0);
    conds.push_back(When());
    conds.back().father = index;
    //set2index[conds.back().prec] = conds.size()-1;
  }

  void addWhen(When& wo, int index = -1)
  {
    assert( !wo.eff.empty() );
    bool addit = true;
    wo.father = index;
    if(1)
      {
	myhash::iterator it = set2index.find(wo.prec);
	set<int>* effp = 0;
	string* commentp = 0;
	if(it!=set2index.end())
	  {
	    effp = &conds[it->second].eff;
	    commentp = &conds[it->second].comment;
	  }
/* 	else */
/* 	  for( size_t i = 0; i < conds.size(); i++)  */
/* 	    assert(conds[i].prec != wo.prec); */
/* 	      { */
/* 		set2index[wo.prec] = i; */
/* 		effp = &wo.prec; */
/* 		commentp = &wo.comment; */
/* 		break; */
/* 	      } */
	if(effp!=0) 
	  {
	    addit = false;
	    for(set<int>::iterator e = wo.eff.begin();
		e != wo.eff.end(); e++ )
	      {
		// Not necessary true. For example:
		/*
		  (when (ill ?i) (not (ill ?i)))
		  (when (not (ill ?i)) (x))
		 */
		//assert(effp->count(-*e) == 0);
		effp->insert(*e);
	      }
	    if( true || *commentp !=  wo.comment )
	      *commentp += ". " + wo.comment;
	    assert( !effp->empty() );
	  }
      }
    if(addit)
      {
	conds.push_back(When());
	conds.back() = wo;
	conds.back().father = index;
	set2index[conds.back().prec] = conds.size()-1;
      }
  }
};


class Act 
{
public:
  size_t index;
  size_t ground;
  string name;
  bool is_observation;
  std::set<int> prec; 
  std::set<int> eff;
  Conds conds;
  typedef pair<set<int>, Conds> branch_t;
  vector<branch_t> branch_eff;
  static const bool p = false;

  void clear()
  {
    index = 0;
    ground = 0;
    name = "";
    prec.clear();
    eff.clear();
    conds.clear();
    branch_eff.clear();
  }
  
  Act(): index(0), ground(0), name(""), is_observation(false)
    {
      if(p)
	std::cout << "act creado " << (void*)this << endl;
    }
//   void do_copy(const Act& orig)
//   {
//     index = orig.index;
//     ground = orig.ground;
//     name = orig.name;
//     is_observation = orig.is_observation;
//     prec = orig.prec;
//     eff = orig.eff;
//     conds = orig.conds;
//     set2index = orig.set2index;
//   }
//   Act& operator=(const Act& orig)
//     {
//       do_copy(orig);
//       if(p)
// 	std::cout << "act creado " << (void*)this
// 		  << " by operator=, copia de " << (void*)&orig<< endl;
//       return *this;
//     }
//   Act(const Act& orig)
//     {
//       do_copy(orig);
//       if(p)
// 	std::cout << "act creado " << (void*)this 
// 		  << " copia de " << (void*)&orig<< endl;
//     }
//   ~Act()
//     {
//       if(p)
// 	std::cout << "act destruido " << name  << " " << (void*)this
// 		  << " w:" << conds.size() << endl;
//     }

  void addWhen(When& wo)
  {
    conds.addWhen(wo, index);
  }

  void addWhen( size_t index )
  {
    conds.addWhen(index);
  }

  void delWhen()
  {
    conds.conds.pop_back();
  }

  void addBranch(set<int>& eff, Conds& conds )
  {
    for( Conds::iterator it = conds.begin();
	 it != conds.end(); ++it )
      it->index = index;
    branch_eff.push_back( make_pair(eff, conds ) );
  }
  
  void delBranch()
  {
    branch_eff.pop_back();
  }

  void dump( ostream& out ) const
    {
      out << "----------------"<< endl;
      out << "Act: name = " << name
	  << " index = " << index << endl
	  << "prec: ";
      for( set<int>::const_iterator it = prec.begin();
	   it != prec.end(); it++ )
	out << *it << " ";
      out << endl << "eff: ";
      for( set<int>::const_iterator it = eff.begin();
	   it != eff.end(); it++ )
	out << *it << " ";
      out << endl;
      for( Conds::const_iterator it2 = conds.begin();
	   it2 != conds.end(); it2++ )
	it2->dump( out );
      out << endl << "branches: " << endl;
      for( vector<branch_t>::const_iterator it = branch_eff.begin();
	   it != branch_eff.end(); it++ )
	{
	  out << "branch: ";
	  const branch_t& b = *it;
	  for( set<int>::const_iterator it = b.first.begin();
	       it != b.first.end(); ++it )
	    out << *it << " ";
	  for( Conds::const_iterator it2 = b.second.begin();
	       it2 != b.second.end(); ++it2 )
	    it2->dump( out );
	  out << endl;
      	}
      out << endl << "----------------"<< endl;
    }

  void dumpPDDL( ostream& outd, string (*name_l)( int lit ), size_t ind, 
		 bool use_contingent_obs_nondeter = false, 
		 bool use_contingent_obs_determ = false ) const
  {
    const bool observation_as_action = false;
    size_t n_effects = eff.size();
    for( Conds::const_iterator wi = conds.begin();
	 wi != conds.end(); ++wi )
      if( wi->used )
	++n_effects;
    // Don't print actions with empty effect
    if(n_effects == 0 && branch_eff.size() == 0 )
      return;
    std::set<int>::const_iterator ai;
    Conds::const_iterator wi;
    di(outd, ind);
    if( is_observation && !observation_as_action && use_contingent_obs_nondeter )
      outd << "(:observation ";
    else
      outd << "(:action ";

    outd << name << endl;
    ind += 2; 

    if( !prec.empty() )
      {
	di(outd,ind);
	outd << ":precondition ";
	if( prec.size() > 1 )
	  outd << " (and ";
	outd << endl;
	ind += 2; 
	for( ai = prec.begin(); ai != prec.end(); ++ai )
	  {
	    di(outd,ind);
	    outd << name_l(*ai) << endl;
	  }
	ind -= 2; 
	if( prec.size() > 1 )
	  {
	    di(outd,ind);
	    outd << ")" << endl;
	  }
      }
    
    di(outd,ind);
    outd << ":effect ";
    if( is_observation && 
	!use_contingent_obs_determ )
      {
	di(outd,ind);
	outd << "(kn_FALSE)" << endl; // OJO: BAD, should be implemented as a new dummy
      }
    else
      {
	
	if( eff.size() + conds.size() > 1 )
	  outd << " (and ";
	outd << endl;
	ind += 2; 
	
	for( ai = eff.begin(); ai != eff.end(); ++ai )
	  if( *ai > 0 || !use_delete_free )
	    {
	      di(outd,ind);
	      outd << name_l(*ai) << endl;
	    }
	
	string ind_when(ind, ' ');
	for( wi = conds.begin(); wi != conds.end(); ++wi )
	  wi->dumpPDDL( outd, name_l, ind_when, is_observation );
	
	ind -= 2; 
	if( eff.size() + conds.size() > 1 )
	  {
	    di(outd,ind);
	    outd << ")" << endl;
	  }
      }
    
    if( !use_delete_free &&
	is_observation &&
	use_contingent_obs_nondeter )
      {
	assert( branch_eff.size() > 1 );
	di(outd,ind);
	outd << ":branches (or " << endl;
	ind += 2; 
	
	for( vector<branch_t>::const_iterator it = branch_eff.begin();
	     it != branch_eff.end(); it++ )
	  {
	    di(outd,ind);
	    outd << "(and " << endl;
	    ind += 2; 
	    const branch_t& b = *it;
	    for( set<int>::const_iterator ai = b.first.begin();
		 ai != b.first.end(); ++ai )
	      {
		di(outd,ind);
		outd << name_l(*ai) << endl;
	      }
	    string ind_when(ind, ' ');
	    for( Conds::const_iterator wi = b.second.begin();
		 wi != b.second.end(); ++wi )
	      wi->dumpPDDL( outd, name_l, ind_when, is_observation );
	    ind -= 2; 
	    di(outd,ind);
	    outd << ")" << endl;
	  }

	ind -= 2; 
	di(outd,ind);
	outd << ")" << endl;
      }

    ind -= 2; 
    di(outd,ind);
    outd << ")" << endl;
  }
};

iatom_t* readAtomByNumber( int index );
std::ostream& operator<<( std::ostream& o, const Act& a );
std::ostream& operator<<( std::ostream& o, const When& w );

void                   _fatal( int, char*, char*, int );

#endif
