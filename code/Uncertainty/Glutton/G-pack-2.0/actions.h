#ifndef ACTIONS_H
#define ACTIONS_H

#include "global.h"
#include "effects.h"
#include "formulas.h"
#include "terms.h"
#include "utils.h"
#include <assert.h>
#include <iostream>
#include <list>
#include <map>
#include <string>

class PredicateTable;
class FunctionTable;
class ValueMap;
class StateFormula;
class AtomSet;
class Effect;
class effect_t;
class action_t;
class state_t;
class stateProbList_t;
class problem_t;
class conditionalEffect_t;
class probabilisticEffect_t;
class assignmentEffect_t;

class Action;
class ActionList;


/*******************************************************************************
 *
 * action schema
 *
 ******************************************************************************/

class ActionSchema
{
  std::string name_;
  VariableList parameters_;
  const StateFormula* precondition_;
  const Effect* effect_;

public:
  ActionSchema( const std::string& name );
  ~ActionSchema();

  void add_parameter( Variable parameter ) {parameters_.push_back( parameter );}
  void set_precondition( const StateFormula& precondition );
  void set_effect( const Effect& effect );
  const std::string& name( void ) const { return( name_ ); }
  size_t arity( void ) const { return( parameters_.size() ); }
  Variable parameter( size_t i ) const { return( parameters_[i] ); }
  const StateFormula& precondition( void ) const { return( *precondition_ ); }
  const Effect& effect( void ) const { return( *effect_ ); }
  void instantiations( ActionList& actions, const problem_t& problem ) const;
  const Action& instantiation( const SubstitutionMap& subst,
			       const problem_t& problem,
			       const StateFormula& precond ) const;
  void print( std::ostream& os, const PredicateTable& predicates,
	      const FunctionTable& functions, const TermTable& terms ) const;
  void analyze( PredicateTable &predicates, TermTable &terms,
		std::map<const StateFormula*,const Atom*> &hash ) const;
  const ActionSchema& rewrite( std::map<const StateFormula*,
			       const Atom*> &hash ) const;
};

class ActionSchemaMap : public std::map<std::string, const ActionSchema*> { };


/*******************************************************************************
 *
 * action
 *
 ******************************************************************************/

class Action
{
  mutable size_t ref_count_;
  std::string name_;
  ObjectList arguments_;
  const StateFormula *precondition_;
  const Effect* effect_;

  Action() : ref_count_(0) { }

public:
  Action( const std::string& name );
  ~Action();

  static void register_use( const Action* a )
    {
#ifdef MEM_DEBUG
      if(a) std::cerr << "<ACT>: inc-ref-count " << a << " = " 
		      << a->ref_count_+1 << std::endl;
#endif
      if( a != NULL ) ++a->ref_count_;
    }

  static void unregister_use( const Action* a )
    {
#ifdef MEM_DEBUG
      if(a) std::cerr << "<ACT>: dec-ref-count " << a << " = " 
		      << a->ref_count_-1 << std::endl;
#endif
      if( a && (--a->ref_count_ == 0) ) delete a;
    }

  void add_argument( Object argument ) { arguments_.push_back( argument ); }
  void set_precondition( const StateFormula& precondition );
  void set_effect( const Effect& effect );
  const std::string& name( void ) const { return( name_ ); }
  size_t arity( void ) const { return( arguments_.size() ); }
  Object argument( size_t i ) const { return( arguments_[i] ); }
  const StateFormula& precondition( void ) const { return( *precondition_ ); }
  const Effect& effect( void ) const { return( *effect_ ); }
  const Action& flatten( const problem_t &problem ) const;
  action_t& translate( const problem_t &problem ) const;
  bool enabled( const state_t& state ) const;
  void affect( state_t& state ) const;
  double cost( const state_t& state ) const { return( 1 ); }
  void print_full( std::ostream& os, const PredicateTable& predicates,
		   const FunctionTable& functions,
		   const TermTable& terms ) const;
  void print( std::ostream& os, const TermTable& terms ) const;
  void printXML( std::ostream& os, const TermTable& terms ) const;
};

class ActionList : public std::vector<const Action*> { };




/*******************************************************************************
 *
 * effect list
 *
 ******************************************************************************/

class effectList_t
{
  size_t size_;
  const effect_t **data_, **data_ptr_;

public:
  effectList_t() : size_(0), data_(0), data_ptr_(0) { }
  ~effectList_t() { if( data_ ) free( data_ ); }

  size_t size( void ) const { return( data_ptr_ - data_ ); }
  const effect_t* effect( size_t i ) const { return( data_[i] ); }
  bool find( const effect_t &eff ) const;
  bool insert( const effect_t *effect );

  /*
    Inserts the given effect into the effect list and returns the position in 
    the list at which the effect was inserted.
  */
  int insert_ret_pos( const effect_t *eff );

  /*
    Determinizes all probabilistic effects in the list by choosing the highest-
    probability outcome for each such effect.
  */
  void freeze();

  /*
    Affects the given state (state_in)  by each of the effects in the list, 
    stores the result in a different state (state_out), and returns the reward
    due to applying these effects (in the vm parameter). If the bMostLikely flag
    is set, deterministically applies the most likely outcome of each 
    probabilistic effect.
  */
  virtual bool affect( const state_t& state_in, 
				       state_t& state_out, ValueMap *vm, 
				       bool bMostLikely = false) const;

  /*
    Removes all the effects from the effect list.
  */
  void clear( void ) { data_ptr_ = data_; }

  /*
    Returns true if and only if the effect list is empty.
  */
  bool empty( void ) const { return (data_ptr_ == data_); }
  void print( std::ostream &os, const problem_t &problem ) const;

  /* 
    Performing a deep ordered comparison of the lists. That is, returns false
    unless the lists not only contain the same effects, but also store them
    in the same order.
  */
  bool operator ==( const effectList_t &effect ) const;

  /*
    Same as ==, but gets around some issues with operator inheritance.
  */
  bool compare( const effectList_t &effect ) const;
};


/*******************************************************************************
 *
 * effect (abstract class)
 *
 ******************************************************************************/
enum effect_type { CONDE, CONJE, STRIPSE, PROBE, ASSIGNE };


/*
  Represents an effect of an action.
*/
class effect_t
{
  double max_reward_;


protected:

  /*
    Holds the type of this effect (conditional, conjunctive, strips.
    probabilistic, or assignment).
  */
  enum effect_type type_;

  /*
    Tells whether this effect is deterministic (i.e., affects a state in the
    same way every time it is applied to that state) or not.
  */
  bool bDet_;


public:

  effect_t() : max_reward_(0), bDet_(true) { }
  virtual ~effect_t() { }
  bool is_det() const { return bDet_; }
  virtual effect_type type() const { return type_; }

  /*
    Applies this effect to the given state (state_in), stores the result in 
    a different state (state_out), and returns the reward
    due to applying this effects (in the vm parameter). If the bMostLikely flag
    is set, deterministically applies the most likely outcome of this effect if
    this effect is probabilitic, otherwise has no influence on this method's
    behavior.
  */
  virtual bool affect( const state_t& state_in, 
				       state_t& state_out, ValueMap *vm, 
				       bool bMostLikely = false) const = 0;

  /*
    Computes the set of state variables affected by this effect.
  */
  virtual void get_affected_vars(std::set<ushort_t>* setAffectedVars) const = 0;

  /*
    Computes the set of literals in the effect's precondition.
  */
  virtual void collect_prec_atoms( atomList_t &atoms ) const = 0;

  /*
    Computes the set of variables this effect may set to "true".
  */
  virtual void collect_add_atoms( atomList_t &atoms ) const = 0;

  /*
    Computes the set of variables this effect may set to "false".
  */
  virtual void collect_del_atoms( atomList_t &atoms ) const = 0;

  /*
    Determinizes all probabilistic effects in the list by choosing the highest-
    probability outcome for each such effect.
  */
  virtual void freeze() = 0;

  /*
    Returns the maximum reward that can be obtained by applying this effect to
    any state.
  */
  virtual double get_max_reward() { return max_reward_; }

  /*
    Sets the maximum reward that can be obtained by applying this effect to
    any state.
  */
  virtual void set_max_reward( double max_reward ) { max_reward_ = max_reward; }

  /* 
    Returns true iff this effect can be applied to this state (i.e., either if 
    the effect has no precondition or if its precondition holds in this state).
  */ 
  virtual bool applicable( const state_t& state ) const = 0;

  /*
    Returns true iff this effect is empty. The meaning of "emptiness" varies
    across the implementations of this abstract class.
  */
  virtual bool empty( void ) const = 0;
  virtual void print( std::ostream &os, const problem_t &problem ) const = 0;

  /*
    Effect comparison operator.
  */
  virtual bool operator==( const effect_t &effect ) const = 0;
  
  /*
    Same as ==, but gets around some issues with operator inheritance.
  */
  virtual bool compare( const effect_t &effect ) const = 0;
};



/*******************************************************************************
 *
 * conjunctive effect
 *
 ******************************************************************************/

/*
  Represents a conjunctive effect, an effect of the kind eff_1 AND...AND eff_n,
  where eff_1, ... , eff_n are themselves some kind of effects.

  For the documentation for the different methods of this class, see the
  declaration of the effect_t parent abstract class.
*/
class conjunctiveEffect_t : public effect_t
{
protected:

  effectList_t eff_list_;


public:

 conjunctiveEffect_t() : effect_t() { type_ = CONJE; }
  virtual ~conjunctiveEffect_t() { }
  effectList_t& effect( void ) { return( eff_list_); }
  const effectList_t& effect( void ) const { return( eff_list_); }
  virtual void insert( const effect_t *eff) { effect().insert(eff); }
  virtual bool affect( const state_t& state_in, state_t& state_out, 
		       ValueMap *vm, bool bMostLikely = false) const;
  virtual void get_affected_vars(std::set<ushort_t>* setAffectedVars) const;
  virtual void collect_prec_atoms( atomList_t &atoms ) const;
  virtual void collect_add_atoms( atomList_t &atoms ) const;
  virtual void collect_del_atoms( atomList_t &atoms ) const;
  virtual void freeze() { effect().freeze(); }
  virtual bool applicable( const state_t& state ) const { return true; }
  virtual bool empty( void ) const { return eff_list_.empty(); }  
  virtual void print( std::ostream &os, const problem_t &problem ) const;
  virtual bool operator==( const effect_t &effect ) const;
  virtual bool compare( const effect_t &effect ) const;
};





/*******************************************************************************
 *
 * assignment effect
 *
 ******************************************************************************/

/*
  Represents an assignment effect, an effect that assigns the value of a 
  numeric expression to a state variable, e.g., x = 1+5. Effects of this kind
  are used to keep track of reward by affecting a "virtual" reward
  state variable.

  For the documentation for most of the methods of this class, see the
  declaration of the effect_t parent abstract class.
*/
class assignmentEffect_t : public effect_t
{
protected:

  // Stores a set of mathematical operations to apply to state variables. 
  std::list<const Assignment*> assignments_;


public:

 assignmentEffect_t() : effect_t()  { type_ = ASSIGNE; }
 assignmentEffect_t(const std::list<const Assignment*> *assignments)  
   : effect_t() { assignments_ = *assignments; type_ = ASSIGNE; };
  virtual ~assignmentEffect_t() { }
  std::list<const Assignment*>& assignments( void ) { return( assignments_ ); }
  const std::list<const Assignment*>& assignments( void ) 
                                              const { return( assignments_ ); }
  void addAssignment(const Assignment* a) { assignments_.push_back(a); }
  size_t size() const { return assignments_.size(); }  

  /*
    This method operates on the variable-value mapping captured in the "vm" 
    parameter. The method goes through its "assignments_" list and applies 
    each operation in that list to the value of the appropriate variabble in
    the value mapping, storing the result back in the value mapping.
  */ 
  virtual bool affect( const state_t& state_in, state_t& state_out, 
		       ValueMap *vm, bool bMostLikely = false) const;
  /*
    In the current implementation, this effect can only affect the "virtual"
    reward variable; thus, no actual state variables are affected by it.
  */
  virtual void get_affected_vars(std::set<ushort_t>* setAffectedVars) 
                                                              const { return; }
  virtual void collect_prec_atoms( atomList_t &atoms ) const {}
  virtual void collect_add_atoms( atomList_t &atoms ) const {}
  virtual void collect_del_atoms( atomList_t &atoms ) const {}
  virtual void freeze() { }
  virtual bool applicable( const state_t& state ) const { return true; }
  virtual bool empty( void ) const { return (assignments_.size() == 0); }
  virtual void print( std::ostream &os, const problem_t &problem ) const;

  /*
    WARNING: "==" and the "compare" method do not have a meaningfulfunctionality
    yet -- the present impplementation does not use them. Currently, they both
    return "false" for all inputs.
  */
  virtual bool operator==( const effect_t &effect ) const;
  virtual bool compare( const effect_t &effect ) const;
  assignmentEffect_t& operator=( const assignmentEffect_t &effect );
};



/*******************************************************************************
 *
 * strips effect
 *
 ******************************************************************************/

/*
  Represents a strips effect, an effect that flips the values of some of
  the state variables from negative to positive (these variables are specified
  in the add-list), and of some of the variables from positive to negative
  (these are specified in the delete-list).

  For the documentation for most of the methods of this class, see the
  declaration of the effect_t parent abstract class.
*/
class stripsEffect_t : public effect_t
{
protected:

  atomList_t add_list_;
  atomList_t del_list_;


public:

 stripsEffect_t()  : effect_t() { type_ = STRIPSE; }
  virtual ~stripsEffect_t() { }

  atomList_t& add_list( void ) { return( add_list_ ); }
  const atomList_t& add_list( void ) const { return( add_list_ ); }
  atomList_t& del_list( void ) { return( del_list_ ); }
  const atomList_t& del_list( void ) const { return( del_list_ ); }
  virtual bool empty( void ) const
    {
      return( (add_list().size() == 0) && (del_list().size() == 0) );
    }
  void insert_add( ushort_t atom ) { add_list_.insert( atom ); }
  void insert_del( ushort_t atom ) { del_list_.insert( atom ); }
  virtual void freeze() { }
  virtual bool affect( const state_t& state_in, state_t& state_out, 
		       ValueMap *vm, bool bMostLikely = false) const;
  virtual void get_affected_vars(std::set<ushort_t>* setAffectedVars) const;
  virtual void collect_prec_atoms( atomList_t &atoms ) const;
  virtual void collect_add_atoms( atomList_t &atoms ) const;
  virtual void collect_del_atoms( atomList_t &atoms ) const;
  virtual bool applicable( const state_t& state ) const { return true; }
  virtual void print( std::ostream &os, const problem_t &problem ) const;
  virtual bool operator==( const effect_t &effect ) const;
  virtual bool compare( const effect_t &effect ) const;
  stripsEffect_t& operator=( const stripsEffect_t &effect );
};


/*******************************************************************************
 *
 * conditional effect
 *
 ******************************************************************************/

/*
  Represents a conditional effect, an effect of the form "IF prec THEN eff", 
  where prec is a formula that must hold in the given state in order for
  the effect eff to be applicable in this state. In general, the precondition
  can be any formula, but in this implementation we assume that it is
  a conjunction of literals

  For the documentation for most of the methods of this class, see the
  declaration of the effect_t parent abstract class.
*/
class conditionalEffect_t : public effect_t
{
protected:
  
  atomListList_t prec_list_;
  effect_t *effect_;


public:

  conditionalEffect_t() : effect_t(),  effect_(NULL)
    {
      type_ = CONDE;
      notify( this, "conditionalEffect_t::conditionalEffect_t()" );
    }
  virtual ~conditionalEffect_t() { }
  atomListList_t& precondition( void ) { return( prec_list_ ); }
  const atomListList_t& precondition( void ) const { return( prec_list_ ); }
  effect_t* effect( void ) { return( effect_ ); }
  const effect_t* effect( void ) const { return( effect_ ); }
  bool empty( void ) const { return( effect()->empty() ); }
  void set_effect( effect_t *eff ) 
  { 
    effect_ = eff; 
    if (!effect_->is_det()) 
      bDet_ = false; 
  }

  virtual void freeze() { effect_->freeze(); bDet_ = effect_->is_det(); }
  virtual bool affect( const state_t& state_in, state_t& state_out, 
		       ValueMap *vm, bool bMostLikely = false) const;
  virtual void get_affected_vars(std::set<ushort_t>* setAffectedVars) 
    const { return effect_->get_affected_vars(setAffectedVars); }
  virtual void collect_prec_atoms( atomList_t &atoms ) const;
  virtual void collect_add_atoms( atomList_t &atoms ) const;
  virtual void collect_del_atoms( atomList_t &atoms ) const;
  virtual bool applicable( const state_t& state ) 
    const { return precondition().holds( state); }
  virtual void print( std::ostream &os, const problem_t &problem ) const;
  virtual bool operator==( const effect_t &effect ) const;
  virtual bool compare( const effect_t &effect ) const;
  conditionalEffect_t& operator=( conditionalEffect_t &effect );
};


/*******************************************************************************
 *
 * probabilistic effect
 *
 ******************************************************************************/

/*
  Represents a probabilistic effect, an effect of the form 
  "p_1: eff_1; ... ; p_n: eff_n", s.t. when this effect is applied to a state,
  one of its sub-effects eff_1, ..., eff_n is chosen probabilistically
  according to its probability p_i. It is this chosen sub-effect that 
  modifies the state.

  For the documentation for most of the methods of this class, see the
  declaration of the effect_t parent abstract class.
*/
class probabilisticEffect_t : public conjunctiveEffect_t
{
protected:

  // Sub-effect probabilities
  std::vector<double> weights_;

  // Keeps track of the index of the highest-probability sub-effect in the 
  // effect list. 
  size_t def_pos;

  // Keeps track of the probability of the highest-probability sub-effect. 
  double def_prob;


public:

  const std::vector<double>& weights() const  { return weights_; }
  probabilisticEffect_t() 
    : conjunctiveEffect_t(), def_pos(0), def_prob(0) { type_ = PROBE; };
  ~probabilisticEffect_t() { }
  virtual void insert( double p, const effect_t *eff );

  /*
    This method turns a probabilistic effect into a deterministic one by 
    figures out its most likely probabilistic sub-effect and forcing this 
    sub-effect to be always chosen for execution.

    WARNING: the current implementation assumes that there can be at most 
    two sub-effects. Therefore, determinizing a probabilistic effect amounts
    to picking the more likely of the two.
  */
  virtual void freeze();
  virtual bool affect( const state_t& state_in, state_t& state_out, 
		       ValueMap *vm, bool bMostLikely = false) const;
  virtual bool applicable( const state_t& state ) const { return true; }
  virtual void print( std::ostream &os,  const problem_t &problem ) const;
  virtual bool operator==( const effect_t &effect ) const;
  virtual bool compare( const effect_t &effect ) const;
};


/*******************************************************************************
 *
 * action 
 *
 ******************************************************************************/

/*
  Represents an operator that the agent can use in a state in order to 
  transition to a different state and get a reward.

  For the documentation for most of the methods of this class, see similarly
  named methods of the effect_t class -- they have analogous functionality.
*/
class action_t
{
 protected:

  mutable size_t ref_count_;
  const char *name_, *nameXML_;
  atomListList_t precondition_;
  effect_t *effect_;
  double max_reward_;

  // Tells is this is the noop action. We assume that the problem we are given 
  // has a unique such action, named "noop". Intuitively, the noop action 
  // describes the problem's natural dynamics, i.e., what happens if the agent 
  // doesn't do anything.
  bool bNoop_;

  // Keeps track of the action's effects that affect the agent's reward.
  std::list<const effect_t*> rewardEffs_;

  // Keeps track of the effects of this action that are distinct from the
  // effects of the noop action. Typically, a non-noop action will have many 
  // effects in common with the noop action, but will also have some of its 
  // own, proper effects.
  std::map<ushort_t, std::list<const effect_t*> > proper_vars_and_effs_;


public:

  static const Application *app_;

  action_t( const std::string &name, const std::string &nameXML );
  action_t() : ref_count_(0), bNoop_(false) { }
  virtual ~action_t();

  static void register_use( const action_t *a )
    {
#ifdef MEM_DEBUG
      if(a) std::cerr << "<act>: inc-ref-count " << a << " = " << a->ref_count_+1 << std::endl;
#endif
      if( a != NULL ) ++a->ref_count_;
    }

  static void unregister_use( const action_t *a )
    {
#ifdef MEM_DEBUG
      if(a) std::cerr << "<act>: dec-ref-count " << a << " = " 
		      << a->ref_count_-1 << std::endl;
#endif
      if( a && (--a->ref_count_ == 0) ) delete a;
    }

  atomListList_t& precondition( void ) { return( precondition_ ); }
  const atomListList_t& precondition( void ) const { return( precondition_ ); }
  effect_t* effect( void ) { return( effect_ ); }
  const effect_t* effect( void ) const { return( effect_ ); }
  void set_effect(effect_t * eff) { effect_ = eff; }
  bool is_noop() const  { return bNoop_; }

  /* 
    Determines the set of this action's proper variables and effects. 
    For a noop action, the set of its proper variables is simply all state
    variables that the noop action can affect, and its set of proper effects is
    the set of all of its effects.

    For a non-noop action, the set of its proper state variables consists of 
    all variables that this action affects _differently_ than the noop action. 
    E.g., if the noop action changes the value of variable X with p = 0.3, and 
    action A does that with p - 0.4, then variable X is in the set of proper
    variables of action A. The set of proper effects for a non-noop action is 
    the subset of its effects that affect its proper variables. Intuitively,
    a non-noop action's proper variables and effects describe the part of that
    action's dynamics that is different from noop's.

    In this implementation, we assume that all the effects of a given action 
    that affect a given variable are mutually exclusive, and exactly one such
    effect must be applicable in any state. That is, each (conditional) effect 
    that may potentially affect variable X has a precondition, and the 
    precondition of exactly one of these effects holds in any given state.
    Moreover, we assume that any effect affects exactly one variable. Both of
    these assumptions hold for the translations of all IPPC-2011 problems into
    PPDDL.

    The parameter to this method is a pointer to a mapping from the noop 
    action's proper variables to the sets of effects affecting these variables.
    For the noop action itself, this mapping must be empty.

    The method populates the private proper_vars_and_effs_ mapping in such 
    a way that upon the method's return the set of keys of the mapping is the
    set of the action's proper variables, and the value of each key is the 
    subset of this action's (mutually exclusive) effects affecting the
    corresponding variable.
  */ 
  void find_proper_vars_and_effs(const std::map<ushort_t, 
				     std::list<const effect_t*> >* eff_of_noop);
  std::map<ushort_t, std::list<const effect_t*> >* get_proper_vars_and_effs() 
    { 
      return &proper_vars_and_effs_; 
    }

  std::list<const effect_t*>* get_reward_effs() { return &rewardEffs_; }
  const char* name( void ) const { return( name_ ); }
  const char* nameXML( void ) const { return( nameXML_ ); }
  void print( std::ostream &os) const { os << name(); }
  void printXML( std::ostream &os ) const { os << nameXML(); }
  bool enabled( const state_t& state, bool nprec = false ) const
    {
      return( precondition().holds( state, nprec ) );
    }

  void insert_precondition( const atomList_t &alist );
  void insert_precondition( const atomListList_t &alist );
  void set_max_reward(double max_reward) { max_reward_ = max_reward; }
  double get_max_reward() const { return max_reward_; }
  void freeze() { effect_->freeze(); }
  virtual bool empty( void ) const;
  virtual bool affect( state_t& state_in, ValueMap *vm, 
		                                bool bMostLikely = false) const;
  virtual void print_full( std::ostream &os, const problem_t &problem) const;
  virtual void collect_prec_atoms( atomList_t &atoms ) const;
  virtual void collect_add_atoms( atomList_t &atoms ) const;
  virtual void collect_del_atoms( atomList_t &atoms ) const;
};

class actionList_t : public std::vector<action_t*> { };


/*******************************************************************************
 *
 * misc. inline functions
 *
 ******************************************************************************/

inline bool
effectList_t::find( const effect_t &eff ) const
{
  for( size_t i = 0; i < size(); ++i )
    if( *effect( i ) == eff ) return( true );
  return( false );
}


inline bool
effectList_t::insert( const effect_t *eff )
{
  size_t i;
  for( i = 0; i < size(); ++i )
    if( *effect( i ) == *eff ) break;

  if( i == size() )
    {
      if( !data_ || (data_ptr_ == &data_[size_]) )
	{
	  size_ = (!data_ ? 1 : size_ << 1);
	  const effect_t **ndata_ =
	    (const effect_t**)
	    realloc( data_, size_ * sizeof(const effect_t*) );
	  data_ptr_ = (!data_ ? ndata_ : &ndata_[data_ptr_ - data_]);
	  data_ = ndata_;
	}
      *data_ptr_++ = eff;
      return( true );
    }
  else
    {
      return( false );
    }
}


inline int
effectList_t::insert_ret_pos( const effect_t *eff )
{
  size_t i;
  for( i = 0; i < size(); ++i )
    if( *effect( i ) == *eff ) return i;

  if( !data_ || (data_ptr_ == &data_[size_]) )
    {
      size_ = (!data_ ? 1 : size_ << 1);
      const effect_t **ndata_ =
	(const effect_t**)
	realloc( data_, size_ * sizeof(const effect_t*) );
      data_ptr_ = (!data_ ? ndata_ : &ndata_[data_ptr_ - data_]);
      data_ = ndata_;
    }
  *data_ptr_++ = eff;
      
  // -1 denotes the end-of-the-list position
  return( -1 );
}

#endif // ACTIONS_H
