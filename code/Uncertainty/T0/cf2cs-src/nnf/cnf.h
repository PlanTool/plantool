#ifndef CNF_INCLUDE
#define CNF_INCLUDE

#include "strxml.h"

#include <zchaff_solver.h>

#include <assert.h>
#include <functional>
#include <iostream>
#include <list>
#include <set>
#include <string>
#include <cstring>
#include <vector>

// forward references
namespace nnf { class Manager; typedef size_t node; };
namespace dtree { class Manager; class Node; };

namespace cnf
{
  class Manager;
  typedef int VarIndex;
  typedef int ClauseIndex;
  typedef class CClause Clause;
  typedef class CVariable Variable;
  enum SatStatus { Satisfiable = SATISFIABLE, Unsatisfiable = UNSATISFIABLE };
  enum ClauseStatus { Original = ORIGINAL_CL, Conflict = CONFLICT_CL, Deleted = DELETED_CL, Probe = PROBE_CL, Unknown = UNKNOWN_CL, Frozen };

  class IntList : public std::vector<int>
  {
  public:
    bool find( int i ) const
    {
      for( const_iterator it = begin(); it != end(); ++it )
	if( *it == i ) return( true );
      return( false );
    }
    void difference( const IntList &il, IntList &ol ) const
    {
      ol.clear();
      for( const_iterator it = begin(); it != end(); ++it )
	if( !il.find( *it ) ) ol.push_back( *it );
    }
    void uinsert( const IntList &il )
    {
      for( const_iterator it = il.begin(); it != il.end(); ++it )
	if( !find( *it ) ) push_back( *it );
    }
    void intersect( const IntList &il, IntList &ol ) const
    {
      ol.clear();
      for( const_iterator it = begin(); it != end(); ++it )
	if( il.find( *it ) ) ol.push_back( *it );
    }
    void printXML( std::ostream &os, int indent = 0 ) const
    {
      for( int i = 0; i < indent; ++i ) os << ' ';
      for( const_iterator it = begin(); it != end(); ++it )
	os << "<int>" << *it << "</int>";
    }
    void readXML( const XML::Node *xml_input );
    void readXML( std::istream &is )
    {
      XML::Node *xml_input = 0;
      is >> xml_input;
      readXML( xml_input );
    }
  };
  class ClauseList : public IntList { };

  class VarList : public IntList
  {
  public:
    void printXML( std::ostream &os, int indent = 0 ) const
    {
      for( int i = 0; i < indent; ++i ) os << ' ';
      for( const_iterator vi = begin(); vi != end(); ++vi )
	os << "<var>" << *vi << "</var>";
    }
  };

  class Manager : public CSolver
  {
    mutable std::vector<int> meta_index_;
    std::vector<std::pair<ClauseIndex,std::pair<CLitPoolElement*,CLitPoolElement*> > > frozen_clauses_;

    template <class _UnaryFunction> void enumerate_models_aux( int initial_dlevel, int depth, _UnaryFunction func )
    {
      if( deduce() == CONFLICT )
	{
	  _conflicts.clear();
	  if( depth > 0 )
	    {
	      back_track( initial_dlevel + depth );
	    }
	}
      else
	{
	  VarIndex dvar = choose_decision_variable();
	  if( !dvar )
	    {
	      func( this );
	    }
	  else
	    {
	      //std::cout << "decision<" << 1+depth << "> = +v" << dvar << std::endl;
	      make_decision( dvar, 0 );
	      enumerate_models_aux( initial_dlevel, 1+depth, func );

	      //std::cout << "decision<" << 1+depth << "> = -v" << dvar << std::endl;
	      make_decision( dvar, 1 );
	      enumerate_models_aux( initial_dlevel, 1+depth, func );
	    }
	  if( depth > 0 )
	    {
	      back_track( initial_dlevel + depth );
	    }
	}
    }

  public:
    Manager()
    {
      _params.verbosity = 10;
      std::cout << "cnf: new id=" << this << "." << std::endl;
    }
    ~Manager()
    {
      std::cout << "cnf: delete id=" << this <<"." << std::endl;
    }

    void reset( void ) { CSolver::reset(); }
    void set_number_variables( int num_vars ) { set_variable_number( num_vars ); }
    VarIndex  number_variables( void ) const { return( ((Manager*)this)->num_variables() ); }
    VarIndex add_variable( void ) { return( (VarIndex)add_variable() ); }
    ClauseIdx add_clause( const int *clause_lits, int num_lits, int gid = 0 )
    {
      meta_index_.resize( 1+num_clauses() );
      return( (ClauseIdx)add_orig_clause( (int*)clause_lits, num_lits, gid ) );
    }
    void freeze_clause( ClauseIndex idx );
    void reanimate_clause( ClauseIndex idx );

    Clause& clause( ClauseIndex idx ) { return( CSolver::clause( idx ) ); }
    const Clause& clause( ClauseIndex idx ) const { return( ((CSolver*)this)->clause( idx ) ); }
    void get_clause_vars( ClauseIndex idx, VarList &ovl ) const
    {
      CLitPoolElement *lits = ((Manager*)this)->clause( idx ).literals();
      for( int i = 0, sz = ((Manager*)this)->clause(idx).num_lits(); i < sz; ++i )
	ovl.push_back( lits[i].var_index() );
    }
    void get_clause_lits( ClauseIndex idx, VarList &ovl ) const
    {
      CLitPoolElement *lits = ((Manager*)this)->clause( idx ).literals();
      for( int i = 0, sz = ((Manager*)this)->clause(idx).num_lits(); i < sz; ++i )
	ovl.push_back( lits[i].s_var() );
    }
    bool var_in_clause( VarIndex var, ClauseIndex idx ) const
    {
      CLitPoolElement *lits = ((Manager*)this)->clause( idx ).literals();
      for( int i = 0, sz = ((Manager*)this)->clause(idx).num_lits(); i < sz; ++i )
	if( var == (VarIndex)lits[i].var_index() ) return( true );
      return( false );
    }
    bool alive( Clause &cl ) const
    {
      if( (ClauseStatus)cl.status() == Original )
	{
	  bool satisfied = false;
	  for( int i = 0, sz = cl.num_lits(); !satisfied && (i < sz); ++i )
	    {
	      CLitPoolElement &lit = cl.literal( i );
	      int value = get_var_assignment( lit.var_index() );
	      int sign = lit.var_sign();
	      if( (value != UNKNOWN) && (sign == 1 - value) )
		satisfied = true;
	    }
	  return( !satisfied );
	}
      return( false );
    }
    void set_meta_index( ClauseIndex idx, int mindex ) { meta_index_[idx] = mindex; }
    int meta_index( ClauseIndex idx ) const { return( meta_index_[idx] ); }

    void enable_branch( VarIndex var ) { mark_var_branchable( var ); }
    void disable_branch( VarIndex var ) { mark_var_unbranchable( var ); }
    bool is_var_branchable( VarIndex var ) const
    {
      return( ((Manager*)this)->variables()[var].is_branchable() );
    }

    int num_clauses( void ) const { return( ((CSolver*)this)->num_clauses() ); }
    int count_clauses( void ) const { return( ((Manager*)this)->num_clauses()-frozen_clauses_.size() ); }

    // decision variable
    VarIndex choose_decision_variable( int mindex ) const;
    VarIndex choose_decision_variable( void ) const;
    VarIndex operator()( void ) const
    {
      return( choose_decision_variable() );
    }

    void preprocess( VarList &lits );
    void init_solve( void ) { CSolver::init_solve(); }
    void clear_conflicts( void ) { _conflicts.clear(); }
    void back_track( int level ) { CSolver::back_track( level ); }
    int deduce( void ) { return( CSolver::deduce() ); }
    int solve( void ) { return( CSolver::solve() ); }
    int real_solve( void ) { CSolver::real_solve(); return( outcome() ); }
    int analyze_conflicts( void ) { return( CSolver::analyze_conflicts() ); }
    int dlevel( void ) const { return( ((CSolver*)this)->dlevel() ); }
    template <class _UnaryFunction> void enumerate_models( _UnaryFunction func ) const
    {
      ((Manager*)this)->enumerate_models_aux( dlevel(), 0, func );
    }
    void enumerate_models( std::ostream &os ) const
    {
      void print_var_assignment( std::ostream*, cnf::Manager* );
      std::pointer_to_binary_function<std::ostream*,cnf::Manager*,void> print( print_var_assignment );
      enumerate_models( std::bind1st( print, &os ) );
    }
    double count_models( void ) const
    {
      double num_models = 0;
      void increase_model_count( double*, cnf::Manager* );
      std::pointer_to_binary_function<double*,cnf::Manager*,void> count( increase_model_count );
      enumerate_models( std::bind1st( count, &num_models ) );
      return( num_models );
    }
    double plain_count_models( void ) const
    {
      double num_models = 0;
      void plain_increase_model_count( double*, cnf::Manager* );
      std::pointer_to_binary_function<double*,cnf::Manager*,void> count( plain_increase_model_count );
      enumerate_models( std::bind1st( count, &num_models ) );
      return( num_models );
    }
    int get_var_assignment( VarIndex var ) const { return( ((Manager*)this)->variable(var).value() ); }
    void make_decision( VarIndex var, int sign ) { CSolver::make_decision( sign + (var<<1) ); }
    void clear_implication_queue( void ) { while( !_implication_queue.empty() ) _implication_queue.pop(); }
    size_t size_implication_queue( void ) const { return( _implication_queue.size() ); }

    dtree::Node* make_dtree( dtree::Manager &dtree_manager, int method, const VarList *weak_vars ) const;
    nnf::node make_ddnnf( nnf::Manager &nnf_manager ) const;

#ifndef ZBDD
    unsigned hash( const char *blueprint = 0, const VarList *context = 0 ) const;
    const char* compute_blueprint( const VarList &context ) const;
#else
    unsigned hash( void ) const;
#endif

    bool satisfied_or_trivial( ClauseIndex clause ) const;
    void dump( std::ostream &os ) const;
    void printXML( std::ostream &os, int indent = 0 ) const;
    void print_cls( std::ostream &os ) const;
  };

  void print_var_assignment( std::ostream *os, cnf::Manager *cnf_manager );
  void increase_model_count( double *num_models, cnf::Manager *cnf_manager );

}; // cnf namespace

inline std::ostream& operator<<( std::ostream &os, const cnf::IntList &il )
{
  il.printXML( os );
  return( os );
}

inline std::ostream& operator<<( std::ostream &os, const cnf::VarList &ivl )
{
  ivl.printXML( os );
  return( os );
}

inline std::ostream& operator<<( std::ostream &os, const cnf::Manager &cnf_manager )
{
  cnf_manager.printXML( os );
  return( os );
}

#endif // CNF_INCLUDE
