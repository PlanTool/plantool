#include "actions.h"
#include "exceptions.h"
#include "problems.h"
#include "domains.h"
#include "formulas.h"
#include "states.h"

#include <stdlib.h>
#include <assert.h>
#include <sstream>
#include <stack>
#include <typeinfo>
#include <limits>

/*******************************************************************************
 *
 * action schema
 *
 ******************************************************************************/

ActionSchema::ActionSchema( const std::string& name )
  : name_(name), precondition_(&StateFormula::TRUE), 
    effect_(new ConjunctiveEffect())
{
  StateFormula::register_use( precondition_ );
}

ActionSchema::~ActionSchema()
{
  StateFormula::unregister_use( precondition_ );
  Effect::unregister_use( effect_ );
}

void
ActionSchema::set_precondition( const StateFormula& precondition )
{
  if( &precondition != precondition_ )
    {
      StateFormula::unregister_use( precondition_ );
      precondition_ = &precondition;
      StateFormula::register_use( precondition_ );
    }
}

void
ActionSchema::set_effect( const Effect& effect )
{
  if( &effect != effect_ )
    {
      Effect::unregister_use( effect_ );
      effect_ = &effect;
      Effect::register_use( effect_ );
    }
}

void
ActionSchema::instantiations( ActionList& actions, 
			                        const problem_t& problem ) const
{
  size_t n = arity();
  if( n == 0 )
    {
      SubstitutionMap subst;
      const StateFormula& precond = 
	                         precondition().instantiation( subst, problem );
      if( !precond.contradiction() )
	actions.push_back( &instantiation( subst, problem, precond ) );
      else
	{
	  std::cout<<name()<<" -- precondition is a contradiction!!"<<std::endl;
	  StateFormula::unregister_use( &precond );
	}
    }
  else
    {
      SubstitutionMap args;
      std::vector<ObjectList> arguments( n, ObjectList() );
      std::vector<ObjectList::const_iterator> next_arg;
      for( size_t i = 0; i < n; ++i )
	{
	  problem.compatible_objects( arguments[i],
				problem.domain().terms().type( parameter(i) ) );
	  if( arguments[i].empty() ) 
	    {
	      std::cout<<"Arguments are empty!!"<<std::endl;
	      return;
	    }
	  next_arg.push_back( arguments[i].begin() );
	}

      std::stack<const StateFormula*> preconds;
      preconds.push( precondition_ );
      StateFormula::register_use( preconds.top() );
      for( size_t i = 0; i < n; )
	{
	  SubstitutionMap pargs;
	  args.insert( std::make_pair( parameter(i), *next_arg[i] ) );
	  pargs.insert( std::make_pair( parameter(i), *next_arg[i] ) );
	  const StateFormula& precond = 
	                        preconds.top()->instantiation( pargs, problem );
	  preconds.push( &precond );

	  if( (i + 1 == n) || precond.contradiction() )
	    {
	      if( !precond.contradiction() )
		{
		  StateFormula::register_use( preconds.top() );
		  actions.push_back( &instantiation( args, problem, precond ) );
		}

	      for( int j = i; j >= 0; --j )
		{
		  StateFormula::unregister_use( preconds.top() );
		  preconds.pop();
		  args.erase( parameter( j ) );
		  ++next_arg[j];
		  if( next_arg[j] == arguments[j].end() )
		    {
		      if( j == 0 )
			{
			  i = n;
			  break;
			}
		      else
			next_arg[j] = arguments[j].begin();
		    }
		  else
		    {
		      i = j;
		      break;
		    }
		}
	    }
	  else
	    ++i;
	}

      while( !preconds.empty() )
	{
	  StateFormula::unregister_use( preconds.top() );
	  preconds.pop();
	}
    }
}

const Action&
ActionSchema::instantiation( const SubstitutionMap& subst, 
			     const problem_t& problem,
			     const StateFormula& precond ) const
{
  Action *action = new Action( name() );

  for( size_t i = 0; i < arity(); ++i )
    {
      SubstitutionMap::const_iterator si = subst.find( parameter( i ) );
      action->add_argument( (*si).second );
    }

  action->set_precondition( precond );
  StateFormula::unregister_use( &precond );
  const Effect *eff = &effect().instantiation( subst, problem );
  action->set_effect( *eff );
  Effect::unregister_use( eff );

  return( *action );
}

void
ActionSchema::print( std::ostream& os, const PredicateTable& predicates,
		     const FunctionTable& functions,
		     const TermTable& terms ) const
{
  os << "  " << name();
  os << std::endl << "    parameters:";
  for( VariableList::const_iterator vi = parameters_.begin(); 
                                                 vi != parameters_.end(); ++vi )
    {
      os << ' ';
      terms.print_term( os, *vi );
    }
  os << std::endl << "    precondition: ";
  precondition().print( os, predicates, functions, terms );
  os << std::endl << "    effect: ";
  effect().print( os, predicates, functions, terms );
}

void
ActionSchema::analyze( PredicateTable &predicates, TermTable &terms,
		       std::map<const StateFormula*,const Atom*> &hash ) const
{
  if( gpt::verbosity >= 350 )
    std::cout << "analyzing schema for action `" << name() << "'" << std::endl;
  precondition().analyze( predicates, terms, hash );
  effect().analyze( predicates, terms, hash );
}

const ActionSchema&
ActionSchema::rewrite( std::map<const StateFormula*,const Atom*> &hash ) const
{
  ActionSchema *action = new ActionSchema( name() );
  for( size_t i = 0; i < arity(); ++i )
    action->add_parameter( parameter( i ) );
  action->set_precondition( precondition().rewrite( hash ) );
  action->set_effect( effect().rewrite( hash ) );
  return( *action );
}




/*******************************************************************************
 *
 * action
 *
 ******************************************************************************/

Action::Action( const std::string& name )
  : ref_count_(0), name_(name), precondition_(&StateFormula::TRUE),
    effect_(new ConjunctiveEffect())
{
  notify( this, "Action::Action(std::string&)" );
  Action::register_use( this );
  StateFormula::register_use( precondition_ );
}

Action::~Action()
{
  assert( ref_count_ == 0 );
  StateFormula::unregister_use( precondition_ );
  Effect::unregister_use( effect_ );
}

void
Action::set_precondition( const StateFormula& precondition )
{
  if( &precondition != precondition_ )
    {
      StateFormula::unregister_use( precondition_ );
      precondition_ = &precondition;
      StateFormula::register_use( precondition_ );
    }
}

void
Action::set_effect( const Effect& effect )
{
  if( &effect != effect_ )
    {
      Effect::unregister_use( effect_ );
      effect_ = &effect;
      Effect::register_use( effect_ );
    }
}

const Action&
Action::flatten( const problem_t &problem ) const
{
  Action *action = new Action( name() );

  for( ObjectList::const_iterator oi = arguments_.begin(); 
                                                       oi != arguments_.end(); )
    action->add_argument( *oi++ );

  const StateFormula *prec = &precondition().flatten( false );
  action->set_precondition( *prec );
  StateFormula::unregister_use( prec );

  const Effect *eff = &effect().flatten();
  action->set_effect( *eff );
  Effect::unregister_use( eff );

  return( *action );
}

action_t&
Action::translate( const problem_t &problem ) const
{
  // generate names
  std::ostringstream ost, ostXML;
  ost << "(" << name();
  ostXML << "<action><name>" << name() << "</name>";
  for( ObjectList::const_iterator oi = arguments_.begin(); 
                                                  oi != arguments_.end(); ++oi )
    {
      ost << ' ';
      problem.terms().print_term( ost, *oi );
      ostXML << "<term>";
      problem.terms().print_term( ostXML, *oi );
      ostXML << "</term>";
    }
  ost << ")";
  ostXML << "</action>";

  action_t *action = new action_t(ost.str(), ostXML.str());
  precondition().translate( action->precondition());
  effect_t *eff = effect().translate();
  action->set_effect(eff);
  action->set_max_reward(eff->get_max_reward());
  return *action;
}

bool 
Action::enabled( const state_t& state ) const
{
  return( precondition().holds( state ) );
}

void 
Action::affect( state_t& state ) const
{
  AtomList adds;
  AtomList deletes;
  AssignmentList assig;
  effect().state_change( adds, deletes, assig, state );

  for( AtomList::const_iterator ai = deletes.begin(); ai != deletes.end(); ++ai)
    state.clear( **ai );

  for( AtomList::const_iterator ai = adds.begin(); ai != adds.end(); ++ai )
    state.add( **ai );

  for( AssignmentList::const_iterator ai = assig.begin(); ai!=assig.end(); ++ai)
    (*ai)->affect( state );

  state.make_digest();
  assert( state.make_check() );
}


void
Action::print_full( std::ostream& os, const PredicateTable& predicates,
		    const FunctionTable& functions,
		    const TermTable& terms ) const
{
  os << "(action (" << name();

  for( ObjectList::const_iterator oi = arguments_.begin(); 
                                                  oi != arguments_.end(); ++oi )
    {
      os << ' ';
      terms.print_term( os, *oi );
    }

  os << ")" << std::endl << "        (prec ";
  precondition().print( os, predicates, functions, terms );
  os << ")" << std::endl << "        (eff ";
  effect().print( os, predicates, functions, terms );
  os << "))";
}

void
Action::print( std::ostream& os, const TermTable& terms ) const
{
  os << '(' << name();
  for( ObjectList::const_iterator oi = arguments_.begin(); 
                                                  oi != arguments_.end(); ++oi )
    {
      os << ' ';
      terms.print_term( os, *oi );
    }
  os << ')';
}

void
Action::printXML( std::ostream& os, const TermTable& terms ) const
{
  os << "<action><name>" << name() << "</name>";
  for( ObjectList::const_iterator oi = arguments_.begin(); 
                                                  oi != arguments_.end(); ++oi )
    {
      os << "<term>";
      terms.print_term( os, *oi );
      os << "</term>";
    }
  os << "</action>";
}



/*******************************************************************************
 *
 * effect list
 *
 ******************************************************************************/

void 
effectList_t::freeze() 
{ 
  for (size_t i = 0; i < size(); i++) 
    { 
      ((effect_t*)effect(i))->freeze(); 
    } 
} 


bool
effectList_t::affect( const state_t& state_in, state_t& state_out, ValueMap *vm,
		                                         bool bMostLikely) const
{
  bool rv = false;

  for( size_t i = 0; i < size(); ++i )
    {
      bool rv_temp = effect( i )->affect( state_in, state_out, vm, bMostLikely);
      rv =  rv_temp || rv;    
    }

  return( rv );
}


void
effectList_t::print( std::ostream &os, const problem_t &problem ) const
{
  for( size_t i = 0; i < size(); ++i )
    {
      os << " ";
      effect( i )->print( os, problem );
      os << std::endl;
    }
}


bool
effectList_t::operator==( const effectList_t &effect ) const 
{
  if (size() != effect.size())
    return false;
  else
    {
      for( size_t i = 0; i < size(); ++i )
	{
	  if(!( *this->effect( i ) == *effect.effect(i) ))
	    return false;
	}
      return true;
    }
}


bool
effectList_t::compare( const effectList_t &effect ) const 
{
  if (size() != effect.size())
    return false;
  else
    {
      for( size_t i = 0; i < size(); ++i )
	{
	  if(!( this->effect( i )->compare(*effect.effect(i))))
	    return false;
	}
      return true;
    }
}


/*******************************************************************************
 *
 * assignment effect
 *
 ******************************************************************************/


bool
assignmentEffect_t::affect( const state_t& state_in, state_t& state_out, 
			                   ValueMap *vm, bool bMostLikely) const
{ 
  if (vm != NULL)
    {
      for (std::list<const Assignment*>::const_iterator it = 
	                   assignments_.begin(); it != assignments_.end(); it++)
	{
	  (*it)->affect(*vm);
	}

      return true; 
    }

  return false;
} 


void 
assignmentEffect_t::print( std::ostream &os, const problem_t &problem ) const
{
  for (std::list<const Assignment*>::const_iterator it = assignments_.begin(); 
                                                 it != assignments_.end(); it++)
    {
      (*it)->print(os, problem.domain().functions(), problem.terms());
    }
}


bool
assignmentEffect_t::operator==( const effect_t &effect ) const
{
  std::cout<<"<assignmentEffect_t>: error: the comparison operator has not"
	   <<"been implemented yet. Returning \"false\"..."<<std::endl;
  return false;
}


bool
assignmentEffect_t::compare( const effect_t &effect ) const
{
  std::cout<<"<assignmentEffect_t>: error: the \"compare\" method has not"
	   <<"been implemented yet. Returning \"false\"..."<<std::endl;
  return false;
}

assignmentEffect_t&
assignmentEffect_t::operator=( const assignmentEffect_t &effect )
{
  assignments() = effect.assignments();
  return( *this );
}


/*******************************************************************************
 *
 * conjunctive effect
 *
 ******************************************************************************/

bool 
conjunctiveEffect_t::affect( const state_t& state_in, state_t& state_out, 
			                   ValueMap *vm, bool bMostLikely) const
{
  return eff_list_.affect( state_in, state_out, vm, bMostLikely );
}


void 
conjunctiveEffect_t::get_affected_vars(std::set<ushort_t>* setAffectedVars)const
{
  for (size_t i = 0; i < effect().size(); i++)
    {
      effect().effect(i)->get_affected_vars(setAffectedVars);
    }
} 


void 
conjunctiveEffect_t::collect_prec_atoms( atomList_t &atoms ) const
{
  for( size_t i = 0; i < effect().size(); ++i )
    effect().effect( i )->collect_prec_atoms( atoms );
}


void 
conjunctiveEffect_t::collect_add_atoms( atomList_t &atoms ) const
{
  for( size_t i = 0; i < effect().size(); ++i )
    effect().effect( i )->collect_add_atoms( atoms );
}


void 
conjunctiveEffect_t::collect_del_atoms( atomList_t &atoms ) const
{
  for( size_t i = 0; i < effect().size(); ++i )
    effect().effect( i )->collect_del_atoms( atoms );
}


void
conjunctiveEffect_t::print( std::ostream &os, const problem_t &problem ) const
{
  os << "(and";
  effect().print(os, problem);
  os << ")";
}

bool
conjunctiveEffect_t::operator==( const effect_t &effect ) const
{
  if (type() == effect.type())
    {
      return(eff_list_ == ((const conjunctiveEffect_t&) effect).effect());
    }
  
  return false;
}


bool
conjunctiveEffect_t::compare( const effect_t &effect ) const
{
  if (type() == effect.type())
    {
      return(eff_list_.compare(((const conjunctiveEffect_t&) effect).effect()));
    }

  return false;
}



/*******************************************************************************
 *
 * strips effect
 *
 ******************************************************************************/

bool
stripsEffect_t::affect( const state_t& state_in, state_t& state_out, 
			                   ValueMap *vm, bool bMostLikely) const
{
  bool rv = false;
  for( size_t i = 0; i < add_list().size(); ++i )
    rv = state_out.add( add_list().atom( i ) ) || rv;
  for( size_t i = 0; i < del_list().size(); ++i )
    rv = state_out.clear( del_list().atom( i ) ) || rv;
  return( rv );
}


void 
stripsEffect_t::get_affected_vars( std::set<ushort_t>* setAffectedVars ) const
{
  for( size_t i = 0; i < add_list().size(); ++i )
    {
      setAffectedVars->insert(add_list().atom( i ));
    }

  for( size_t i = 0; i < del_list().size(); ++i )
    {
      setAffectedVars->insert(del_list().atom( i ));
    }
}


void
stripsEffect_t::collect_prec_atoms( atomList_t &atoms ) const
{
}

void
stripsEffect_t::collect_add_atoms( atomList_t &atoms ) const
{
  atoms.insert( add_list() );
}


void
stripsEffect_t::collect_del_atoms( atomList_t &atoms ) const
{
  atoms.insert( del_list() );
}


void
stripsEffect_t::print( std::ostream &os, const problem_t &problem ) const
{
  if( add_list().size() > 0 )
    {
      os << "(add ";
      add_list().full_print( os, &problem );
      os << ")";
    }

  if( del_list().size() > 0 )
    {
      os << (add_list().size() > 0 ? " (del " : "(del ");
      del_list().full_print( os, &problem );
      os << ")";
    }
}


bool
stripsEffect_t::operator==( const effect_t &effect ) const
{
  if (type() == effect.type())
    {
      return( (add_list() == ((const stripsEffect_t&)effect).add_list()) &&
	      (del_list() == ((const stripsEffect_t&)effect).del_list()) );
    }

  return false;
}


bool
stripsEffect_t::compare( const effect_t &effect ) const
{
  if (type() == effect.type())
    {
      return( (add_list() == ((const stripsEffect_t&)effect).add_list()) &&
	      (del_list() == ((const stripsEffect_t&)effect).del_list()) );
    }

  return false;
}


stripsEffect_t&
stripsEffect_t::operator=( const stripsEffect_t &effect )
{
  add_list() = effect.add_list();
  del_list() = effect.del_list();
  return( *this );
}


/*******************************************************************************
 *
 * conditional effect
 *
 ******************************************************************************/

bool
conditionalEffect_t::affect( const state_t& state_in, state_t& state_out, 
			                   ValueMap *vm, bool bMostLikely) const
{
  return( precondition().holds( state_in, false ) && effect()->affect( 
				       state_in, state_out, vm, bMostLikely ) );
}


void
conditionalEffect_t::collect_prec_atoms( atomList_t &atoms ) const
{
  for( size_t i = 0; i < precondition().size(); ++i )
    atoms.insert( precondition().atom_list( i ) );
  effect()->collect_prec_atoms( atoms );
}


void
conditionalEffect_t::collect_add_atoms( atomList_t &atoms ) const
{
  effect()->collect_add_atoms( atoms );
}


void
conditionalEffect_t::collect_del_atoms( atomList_t &atoms ) const
{
  effect()->collect_del_atoms( atoms );
}


void
conditionalEffect_t::print( std::ostream &os, const problem_t &problem ) const
{
  os << "(when (";
  precondition().full_print( os, &problem );
  os << ") ";
  effect()->print( os, problem );
  os << ")";
}


bool
conditionalEffect_t::operator==( const effect_t &effect ) const
{
  if (type() == effect.type())
    {
      return((precondition() 
	      == ((const conditionalEffect_t&)effect).precondition()) 
	 && ((effect_ == NULL && ((const conditionalEffect_t&)effect).effect()
	      == NULL) 
	      || *effect_ == *(((const conditionalEffect_t&)effect).effect())));
    }

  return false;
}

bool
conditionalEffect_t::compare( const effect_t &effect ) const
{
  if (type() == effect.type())
    {
      return((precondition() 
	      == ((const conditionalEffect_t&)effect).precondition()) 
	 && ((effect_ == NULL && ((const conditionalEffect_t&)effect).effect() 
	      == NULL) 
        || effect_->compare(*(((const conditionalEffect_t&)effect).effect()))));
    }

  return false;
}

conditionalEffect_t&
conditionalEffect_t::operator=( conditionalEffect_t &effect )
{
  precondition() = effect.precondition();
  effect_ = effect.effect();
  return( *this );
}


/*******************************************************************************
 *
 * probabilistic effect
 *
 ******************************************************************************/

void 
probabilisticEffect_t::insert( double p, const effect_t *eff ) 
{ 
  // Try inserting the sub-effect in the list of sub-effects.
  int pos = effect().insert_ret_pos(eff);
  if (pos != -1)
    {
      // If this sub-effect is already present in the list, we will be simply
      // adding to that sub-effect's probability.
      assert(pos >= 0);
      weights_[pos] = weights_[pos] + p;

      if (weights_[pos] > def_prob 
	                      || (weights_[pos] == def_prob && drand48() < 0.5))
	{
	  // If the new sub-effect's probability reaches or exceeds the 
	  // probability of the current most likely sub-effect, update the 
	  // def_pos and def_prob variables.
	  def_pos = pos;
	  def_prob = weights_[pos];

	  if (weights_[pos] == 1)
	    {
	      bDet_= true;
	    }
	}
    }
  else
    {
      // If this sub-effect wasn't present in the list, it got appended to the
      // end of the list. Update the def_pos and def_prob variables as 
      // mecessary.
      if (p > def_prob || (p == def_prob && drand48() < 0.5))
	{
	  def_pos = weights_.size();
	  def_prob = p;
	}
	
      if (p < 1.0)
	{
	  bDet_ = false;
	}

      weights_.push_back(p);
    }    
}


void 
probabilisticEffect_t::freeze() 
{ 
  // WARNING: the current implementation assumes that there can be at most 
  // two sub-effects. Therefore, determinizing a probabilistic effect amounts
  // to picking the more likely of the two.
  if (drand48() < weights_[0]) 
    def_pos = 0; 
  else 
    def_pos = 1; 
    
  bDet_ = true;
}


bool
probabilisticEffect_t::affect( const state_t& state_in, state_t& state_out, 
			                   ValueMap *vm, bool bMostLikely) const
{
  if (bMostLikely)
    {
      return effect().effect(def_pos)->affect( state_in, state_out, vm, 
					                          bMostLikely );
    }
  
  if (bDet_ && effect().size() > 0)
    {
      return effect().effect(0)->affect( state_in, state_out, vm, bMostLikely );
    }

  // Sample from the distribution over sub-effects ~ their probabilities.
  double r = drand48();
  double sum = 0;
  
  for ( size_t i = 0; i < effect().size(); ++i )
    {
      sum +=  weights_[i];

      if ( r < sum )
	{
	  return effect().effect(i)->affect( state_in, state_out, vm, 
					                          bMostLikely );
	}
    }
 
  return false;  
}


void
probabilisticEffect_t::print( std::ostream &os, const problem_t &problem ) const
{
  os << "(probabilistic ";

  for ( size_t i = 0; i < effect().size(); ++i )
    {
      os << "(" << weights_[i] << " ";
      effect().effect(i)->print(os, problem);
      os << ")";
    }
  os << ")";
}


bool
probabilisticEffect_t::operator==( const effect_t &effect ) const
{
  if (type() == effect.type())
    {
      const std::vector<double>& weights = 
	                      ((const probabilisticEffect_t&) effect).weights();

      if (weights_.size() != weights.size())
	return false;

      bool bAllEqual = true;

      for (size_t i = 0; i < weights_.size(); i++)
	{
	  if (weights_[i] != weights[i])
	    {
	      bAllEqual = false;
	      break;
	    }
	}

      return(bAllEqual && 
	     (eff_list_ == ((const probabilisticEffect_t&) effect).effect()));
    }

  return false;
}


bool
probabilisticEffect_t::compare( const effect_t &effect ) const
{
  if (type() == effect.type())
    {
      const std::vector<double>& weights = 
	                      ((const probabilisticEffect_t&) effect).weights();
      
      if (weights_.size() != weights.size())
	return false;

      bool bAllEqual = true;

      for (size_t i = 0; i < weights_.size(); i++)
	{
	  if (weights_[i] != weights[i])
	    {
	      bAllEqual = false;
	      break;
	    }
	}

      return(bAllEqual && 
	 (eff_list_.compare(((const probabilisticEffect_t&) effect).effect())));
    }

  return false;
}


/*******************************************************************************
 *
 * action
 *
 ******************************************************************************/


action_t::action_t( const std::string &name, const std::string &nameXML )
  : ref_count_(0)
{
  action_t::register_use( this );
  char * tempName = strdup( name.c_str() );
  name_ = tempName;

  // We assume that the name of the noop action in the problem description is
  // "(noop)".
  if (strcasecmp(name_, "(noop)") == 0)
      bNoop_ = true;
  else
      bNoop_ = false;

  nameXML_ = strdup( nameXML.c_str() );
}


action_t::~action_t()
{
  assert( ref_count_ == 0 );
  free( (void*)name_ );
  free( (void*)nameXML_ );
}


void 
action_t::find_proper_vars_and_effs(
	     const std::map<ushort_t, std::list<const effect_t*> >* eff_of_noop)
{ 
  std::map<ushort_t, std::list<const effect_t*> > mapTemp;
  conjunctiveEffect_t* ce = (conjunctiveEffect_t*)effect_;

  if (ce == NULL)
    {
      return;
    }

  // First, construct a mapping from the set of all variables affected by 
  // this action to the subsets of this action's effects affecting each of 
  // these variables.
  for (size_t i = 0; i < ce->effect().size(); i++)
    {
      // For each effect, get the set of variables it affects. In the current
      // implementation, this set must consist of just one variable.
      std::set<ushort_t> setAffectedVars;
      ce->effect().effect(i)->get_affected_vars(&setAffectedVars);

      if (setAffectedVars.empty())
	{
	  // If the effect doesn't affect any variables, it must only be 
	  // affecting the agent's reward. 
	  rewardEffs_.push_back(ce->effect().effect(i));
	}
      
      for (std::set<ushort_t>::iterator it = setAffectedVars.begin(); 
	                                      it != setAffectedVars.end(); it++)
	{
	  // For each affected variable, insert the effect into the set of 
	  // effects affecting this variable.
	  std::map<ushort_t, std::list<const effect_t*> >::iterator ita_e =
	                                                    mapTemp.find((*it));
	  if (ita_e != mapTemp.end())
	    {
	      ita_e->second.push_back(ce->effect().effect(i));
	    }
	  else
	    {
	      std::list<const effect_t*> l;
	      l.push_back(ce->effect().effect(i));
	      mapTemp[(*it)] = l;
	    }
	}
    }
      
  // Now, go through tbe noop action's proper-variable-to-proper-effect mapping,
  // identify variables for which the sets of effects affecting them are 
  // different for the noop action and for this one, and memorize these
  // variables and the effect sets affecting them. These are exactly the proper
  // variables and the proper effects for this action.
  for (std::map<ushort_t, std::list<const effect_t*> >::iterator ita_e = 
	                       mapTemp.begin(); ita_e != mapTemp.end(); ita_e++)
    {
      if (bNoop_)
	{
	  // For the noop action, the proper variables are all variables 
	  // affected by the noop action. All of the noop action's effects
	  // are proper.
	  proper_vars_and_effs_[ita_e->first] = ita_e->second;
	}
      else
	{
	  std::map<ushort_t, std::list<const effect_t*> >::const_iterator noop_e
	                                      = eff_of_noop->find(ita_e->first);
	  
	  if (noop_e != eff_of_noop->end() && noop_e->second.size() 
	                                                == ita_e->second.size())
	    {
	      bool bFoundAll = true;
	      for (std::list<const effect_t*>::iterator it_local 
	               = ita_e->second.begin(); it_local != ita_e->second.end();
		                                                     it_local++)
		{
		  bool bFoundIt = false;

		  for (std::list<const effect_t*>::const_iterator it_noops 
		       = noop_e->second.begin(); it_noops!=noop_e->second.end();
		                                                     it_noops++)
		    {
		      if ((*it_local)->compare(**it_noops))
			{
			  bFoundIt = true;
			  break;
			}
		    }

		  if (!bFoundIt)
		    {
		      bFoundAll = false;
		      break;
		    }
		}

	      if (!bFoundAll)
		{
		  proper_vars_and_effs_[ita_e->first] = ita_e->second;
		}
	    }
	  else
	    {
	      proper_vars_and_effs_[ita_e->first] = ita_e->second;
	    }
	}
    }
  
  // For debugging purposes:
  //std::cout<<name_<<" -- Total number of state variables affected by this "
  //       <<"action: "<<mapTemp.size()<<std::endl;
  //std::cout<<name_<<"-- Number of proper state  variables of this action: "
  //	   <<proper_vars_and_effs_.size()<<std::endl;
  mapTemp.clear();
}


void
action_t::insert_precondition( const atomList_t &alist )
{
  atomList_t *al = new atomList_t;
  *al = alist;
  precondition().insert( al );
}


void
action_t::insert_precondition( const atomListList_t &alist )
{
  for( size_t i = 0; i < alist.size(); ++i )
    insert_precondition( alist.atom_list( i ) );
}


bool
action_t::empty( void ) const
{
  return( effect()->empty() );
}


bool
action_t::affect( state_t& state_in, ValueMap *vm, bool bMostLikely) const
{
  bool rv = false;
  if (precondition().holds(state_in, false));
  {
    state_t state_out = state_in;
    rv = effect()->affect( state_in, state_out,  vm, bMostLikely );
    state_in = state_out;
    state_in.make_digest();
  }
  return( rv );
}


void
action_t::print_full( std::ostream &os, const problem_t &problem ) const
{
  os << "(action " << name() << std::endl
     << "        (prec ";
  precondition().full_print( os, &problem );
  os << ")" << std::endl
     << "        (eff ";
  effect()->print( os, problem );
  os << "))";
}


void
action_t::collect_prec_atoms( atomList_t &atoms ) const
{
  for( size_t i = 0; i < precondition().size(); ++i )
    atoms.insert( precondition().atom_list( i ) );
  effect()->collect_prec_atoms( atoms );
}


void
action_t::collect_add_atoms( atomList_t &atoms ) const
{
  effect()->collect_add_atoms( atoms );
}


void
action_t::collect_del_atoms( atomList_t &atoms ) const
{
  effect()->collect_del_atoms( atoms );
}


