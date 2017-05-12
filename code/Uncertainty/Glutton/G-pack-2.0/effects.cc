#include "global.h"
#include "actions.h"
#include "effects.h"
#include "problems.h"
#include "formulas.h"
#include "expressions.h"
#include "exceptions.h"
#include "states.h"
#include <assert.h>
#include <stack>

bool ConditionalEffect::working_ = false;
bool ProbabilisticEffect::working_ = false;


/*******************************************************************************
 *
 * assignment
 *
 ******************************************************************************/

Assignment::Assignment( unsigned oper, const Application& application,
			const Expression& expr )
  : operator_(oper), application_(&application), expr_(&expr)
{
  Expression::register_use( application_ );
  Expression::register_use( expr_ );
  notify( this, "Assignment::Assignment(unsigned,Application&,Expression&)" );
}


Assignment::~Assignment()
{
  Expression::unregister_use( application_ );
  Expression::unregister_use( expr_ );
}

void
Assignment::affect( ValueMap& values ) const
{
  if( operator_ == ASSIGN_OP )
    {
      values[application_] = expr_->value( values );
    }
  else
    {
      ValueMap::const_iterator vi = values.find( application_ );
      
      if( vi == values.end() )
	{
	  throw Exception( "changing undefined value" );
	}
      else if( operator_ == SCALE_UP_OP )
	values[application_] = (*vi).second * expr_->value( values );
      else if( operator_ == SCALE_DOWN_OP )
	values[application_] = (*vi).second / expr_->value( values );
      else if( operator_ == INCREASE_OP )
	  values[application_] = (*vi).second + expr_->value( values );
      else
	  values[application_] = (*vi).second - expr_->value( values );
    }
}


void
Assignment::affect( state_t& state ) const
{
#ifndef NO_STRICT
  throw Exception( "Assignment::affect: error: unsupported effect" );
#else
  std::cout << "Assignment::affect: error: unsupported effect" << std::endl;
#endif
}


const Assignment&
Assignment::instantiation( const SubstitutionMap& subst,
			   const problem_t& problem ) const
{
  return( *new Assignment( operator_, 
			   application_->substitution( subst ),
			   expr_->instantiation( subst, problem ) ) );
}


bool
Assignment::operator==( const Assignment& assig ) const
{
  return( (operator_ == assig.operator_) &&
	  (*application_ == *assig.application_) &&
	  (*expr_ == *assig.expr_) );
}


void
Assignment::print( std::ostream& os, const FunctionTable& functions,
		   const TermTable& terms ) const
{
  os << '(';
  if( operator_ == ASSIGN_OP )
    os << "assign ";
  else if( operator_ == SCALE_UP_OP )
    os << "scale-up ";
  else if( operator_ == SCALE_DOWN_OP )
    os << "scale-down ";
  else if( operator_ == INCREASE_OP )
    os << "increase ";
  else
    os << "decrease ";
  application_->print( os, functions, terms );
  os << ' ';
  expr_->print( os, functions, terms );
  os << ')';
}


/*******************************************************************************
 *
 * add effect
 *
 ******************************************************************************/

AddEffect::AddEffect( const Atom& atom )
  : atom_(&atom)
{
  Effect::register_use( this );
  StateFormula::register_use( atom_ );
  notify( this, "AddEffect::AddEffect(Atom&)" );
}


AddEffect::~AddEffect()
{
  StateFormula::unregister_use( atom_ );
}


const Effect&
AddEffect::flatten( void ) const
{
  Effect::register_use( this );
  return( *this );
}


void
AddEffect::state_change( AtomList& adds, AtomList& deletes, 
			 AssignmentList& assignments,
			 const state_t& state ) const
{
  adds.push_back( atom_ );
}


effect_t* 
AddEffect::translate( void ) const
{
  stripsEffect_t* temp = new stripsEffect_t();
  ushort_t atm = problem_t::atom_hash_get( atom() );
  temp->insert_add( atm );
  return temp;
}


const Effect& 
AddEffect::instantiation( const SubstitutionMap& subst,
			  const problem_t& problem ) const
{
  const Atom* inst_atom = &atom().substitution( subst );
  if( inst_atom == atom_ )
    {
      StateFormula::unregister_use( inst_atom );
      Effect::register_use( this );
      return( *this );
    }
  else
    {
      const Effect *result = new AddEffect( *inst_atom );
      StateFormula::unregister_use( inst_atom );
      return( *result );
    }
}


bool
AddEffect::operator==( const Effect& eff ) const
{
  const AddEffect *aeff = dynamic_cast<const AddEffect*>(&eff);
  return( (aeff != NULL) && (atom() == aeff->atom()) );
}


void
AddEffect::print( std::ostream& os, const PredicateTable& predicates,
		  const FunctionTable& functions,
		  const TermTable& terms ) const
{
  atom().print( os, predicates, functions, terms );
}


void
AddEffect::analyze( PredicateTable &predicates, TermTable &terms,
		    std::map<const StateFormula*,const Atom*> &hash ) const
{
}


const Effect&
AddEffect::rewrite( std::map<const StateFormula*,const Atom*> &hash ) const
{
  Effect::register_use( this );
  return( *this );
}


/*******************************************************************************
 *
 * delete effect
 *
 ******************************************************************************/

DeleteEffect::DeleteEffect( const Atom& atom )
  : atom_(&atom)
{
  Effect::register_use( this );
  StateFormula::register_use( atom_ );
  notify( this, "DeleteEffect::DeleteEffect(Atom&)" );
}


DeleteEffect::~DeleteEffect()
{
  StateFormula::unregister_use( atom_ );
}


const Effect&
DeleteEffect::flatten( void ) const
{
  Effect::register_use( this );
  return( *this );
}


void
DeleteEffect::state_change( AtomList& adds, AtomList& deletes,
			    AssignmentList& assignments,
			    const state_t& state ) const
{
  deletes.push_back( atom_ );
}


effect_t* 
DeleteEffect::translate( void ) const
{
  stripsEffect_t* temp = new stripsEffect_t();
  ushort_t atm = problem_t::atom_hash_get( atom() );
  temp->insert_del( atm );
  return temp;
}


const Effect&
DeleteEffect::instantiation( const SubstitutionMap& subst,
			     const problem_t& problem ) const
{
  const Atom* inst_atom = &atom().substitution( subst );
  if( inst_atom == atom_ )
    {
      StateFormula::unregister_use( inst_atom );
      Effect::register_use( this );
      return( *this );
    }
  else
    {
      const Effect *result = new DeleteEffect( *inst_atom );
      StateFormula::unregister_use( inst_atom );
      return( *result );
    }
}


bool
DeleteEffect::operator==( const Effect& eff ) const
{
  const DeleteEffect *deff = dynamic_cast<const DeleteEffect*>(&eff);
  return( (deff != NULL) && (atom() == deff->atom()) );
}


void
DeleteEffect::print( std::ostream& os, const PredicateTable& predicates,
		     const FunctionTable& functions,
		     const TermTable& terms ) const
{
  os << "(not ";
  atom().print( os, predicates, functions, terms );
  os << ")";
}


void
DeleteEffect::analyze( PredicateTable &predicates, TermTable &terms,
		       std::map<const StateFormula*,const Atom*> &hash ) const
{
}


const Effect&
DeleteEffect::rewrite( std::map<const StateFormula*,const Atom*> &hash ) const
{
  Effect::register_use( this );
  return( *this );
}


/*******************************************************************************
 *
 * assignment effect
 *
 ******************************************************************************/

AssignmentEffect::AssignmentEffect( const Assignment& assignment )
  : assignment_(&assignment)
{
  Effect::register_use( this );
  notify( this, "AssignmentEffect::AssignmentEffect(Assignment&)" );
}


AssignmentEffect::~AssignmentEffect()
{
  delete assignment_;
}


const Effect&
AssignmentEffect::flatten( void ) const
{
  std::cout << "AssignmentEffect::flatten: error: unsupported effect" 
	    << std::endl;
  Effect::register_use( this );
  return (*this);
}


void
AssignmentEffect::state_change( AtomList& adds, AtomList& deletes,
				AssignmentList& assignments,
				const state_t& state ) const
{
  assignments.push_back( assignment_ );
}


effect_t* 
AssignmentEffect::translate( void ) const
{
  const Application& app = assignment_->application();
  const Expression& exp = assignment_->expression();
  assignmentEffect_t *temp = new assignmentEffect_t();
  ValueMap vm;
  vm[&app] = 0;
  Assignment *a = new Assignment(assignment_->oper(), app, exp);
  a->affect(vm);
  double max_reward = MAX(0, vm[&app]);

  temp->addAssignment(a);
  temp->set_max_reward(max_reward);
  return temp;
}


const Effect& 
AssignmentEffect::instantiation( const SubstitutionMap& subst,
				 const problem_t& problem ) const
{
  return( *new AssignmentEffect(assignment().instantiation( subst, problem )));
}


bool
AssignmentEffect::operator==( const Effect& eff ) const
{
  const AssignmentEffect *assig = dynamic_cast<const AssignmentEffect*>(&eff);
  return( (assig != NULL) && (assignment() == assig->assignment()) );
}


void
AssignmentEffect::print( std::ostream& os,
			 const PredicateTable& predicates,
			 const FunctionTable& functions,
			 const TermTable& terms ) const
{
  assignment().print( os, functions, terms );
}


void
AssignmentEffect::analyze( PredicateTable &predicates, TermTable &terms,
			 std::map<const StateFormula*,const Atom*> &hash ) const
{
}


const Effect&
AssignmentEffect::rewrite(std::map<const StateFormula*,const Atom*> &hash) const
{
  Effect::register_use( this );
  return( *this );
}


/*******************************************************************************
 *
 * conjunctive effect
 *
 ******************************************************************************/

ConjunctiveEffect::ConjunctiveEffect()
{
  Effect::register_use( this );
  notify( this, "ConjunctiveEffect::ConjunctiveEffect()" );
}


ConjunctiveEffect::~ConjunctiveEffect()
{
  for( EffectList::const_iterator ei = conjuncts_.begin(); 
                                                  ei != conjuncts_.end(); ++ei )
    Effect::unregister_use( *ei );
}


void
ConjunctiveEffect::add_conjunct( const Effect& conjunct )
{
  const ConjunctiveEffect* conj_effect = 
                              dynamic_cast<const ConjunctiveEffect*>(&conjunct);
  if( conj_effect != NULL )
    {
      for( EffectList::const_iterator ei = conj_effect->conjuncts_.begin();
	   ei != conj_effect->conjuncts_.end(); ++ei )
	{
	  Effect::register_use( *ei );
	  conjuncts_.push_back( *ei );
	}
      Effect::unregister_use( &conjunct );
    }
  else
    {
      conjuncts_.push_back( &conjunct );
    }
}

const Effect&
ConjunctiveEffect::flatten( void ) const
{
  std::cout<<"ConjunctiveEffect::flatten: error: method not implemented"
	   <<std::endl;
  return *this;
}

void
ConjunctiveEffect::state_change( AtomList& adds, AtomList& deletes,
				 AssignmentList& assignments,
				 const state_t& state ) const
{
  for( EffectList::const_iterator ei = conjuncts_.begin(); ei != conjuncts_.end(); ++ei )
    (*ei)->state_change( adds, deletes, assignments, state );
}


effect_t* 
ConjunctiveEffect::translate( void ) const
{
  stripsEffect_t *temp_s = new stripsEffect_t();
  assignmentEffect_t *temp_a = new assignmentEffect_t();
  conjunctiveEffect_t *temp_c = new conjunctiveEffect_t();

  double max_reward = 0;

  for( size_t i = 0; i < size(); ++i )
    {
      effect_t *temp = conjunct( i ).translate( );

      stripsEffect_t *temp_temp_s = dynamic_cast<stripsEffect_t*>(temp);
      if (temp_temp_s != NULL)
	{
	  for (size_t i = 0; i < temp_temp_s->add_list().size(); i++)
	    {
	      temp_s->insert_add(temp_temp_s->add_list().atom(i));
	    }

	  for (size_t i = 0; i < temp_temp_s->del_list().size(); i++)
	    {
	      temp_s->insert_del(temp_temp_s->del_list().atom(i));
	    }

	  delete temp_temp_s;
	}
      else
	{
	  assignmentEffect_t *temp_temp_a 
	                              = dynamic_cast<assignmentEffect_t*>(temp);
	  if (temp_temp_a != NULL)
	    {
	      for (std::list<const Assignment*>::iterator it 
		                   = temp_temp_a->assignments().begin(); 
		                   it != temp_temp_a->assignments().end(); it++)
		{
		  temp_a->addAssignment(*it);
		}
	      temp_a->set_max_reward(temp_temp_a->get_max_reward());
	      max_reward += temp_temp_a->get_max_reward();

	      delete temp_temp_a;
	    }
	  else // must be probabilistic
	    {
	      max_reward += temp->get_max_reward();
	      temp_c->insert(temp);
	    }
	}
    }

  if (!temp_s->empty())
    {
      temp_c->insert(temp_s);
    }
  else
    {
      delete temp_s;
    }

  if (!temp_a->empty())
    {
      temp_c->insert(temp_a);
    }
  else
    {
      delete temp_a;
    }

  temp_c->set_max_reward(max_reward);

  return temp_c;
}
	

const Effect& 
ConjunctiveEffect::instantiation( const SubstitutionMap& subst,
				  const problem_t& problem ) const
{
  ConjunctiveEffect& inst_effect = *new ConjunctiveEffect();
  for( EffectList::const_iterator ei = conjuncts_.begin(); 
                                                  ei != conjuncts_.end(); ++ei )
    inst_effect.add_conjunct( (*ei)->instantiation( subst, problem ) );
  return( inst_effect );
}


bool
ConjunctiveEffect::operator==( const Effect& eff ) const
{
  const ConjunctiveEffect *ceff = dynamic_cast<const ConjunctiveEffect*>(&eff);
  if( (ceff != NULL) && (size() == ceff->size()) )
    {
      for( size_t i = 0; i < size(); ++i )
	if( !(conjunct( i ) == ceff->conjunct( i )) )
	  return( false );
      return( true );
    }
  return( true );
}


void
ConjunctiveEffect::print( std::ostream& os,
			  const PredicateTable& predicates,
			  const FunctionTable& functions,
			  const TermTable& terms ) const
{
  if( size() == 1 )
    conjunct(0).print( os, predicates, functions, terms );
  else
    {
      os << "(and";
      for( EffectList::const_iterator ei = conjuncts_.begin();
	   ei != conjuncts_.end(); ++ei )
	{
	  os << ' ';
	  (*ei)->print( os, predicates, functions, terms );
	}
      os << ")";
    }
}


void
ConjunctiveEffect::analyze( PredicateTable &predicates, TermTable &terms,
			 std::map<const StateFormula*,const Atom*> &hash ) const
{
  for( EffectList::const_iterator ei = conjuncts_.begin(); 
                                                  ei != conjuncts_.end(); ++ei )
    (*ei)->analyze( predicates, terms, hash );
}

const Effect&
ConjunctiveEffect::rewrite(std::map<const StateFormula*,const Atom*> &hash)const
{
  ConjunctiveEffect *conj = new ConjunctiveEffect;
  for( EffectList::const_iterator ei = conjuncts_.begin(); 
                                                  ei != conjuncts_.end(); ++ei )
    conj->add_conjunct( (*ei)->rewrite( hash ) );
  return( *conj );
}


/*******************************************************************************
 *
 * conditional effect
 *
 ******************************************************************************/

const Effect&
ConditionalEffect::make( const StateFormula& condition, const Effect& effect )
{
  if( condition.tautology() )
    {
      StateFormula::unregister_use( &condition );
      return( effect );
    }
  else if( condition.contradiction() )
    {
      StateFormula::unregister_use( &condition );
      Effect::unregister_use( &effect );
      return( *new ConjunctiveEffect() );
    }
  else
    {
      const Effect *eff = new ConditionalEffect( condition, effect );
      StateFormula::unregister_use( &condition );
      Effect::unregister_use( &effect );
      return( *eff );
    }
}


ConditionalEffect::ConditionalEffect( const StateFormula& condition,
				      const Effect& effect )
  : condition_(&condition), effect_(&effect)
{
  Effect::register_use( this );
  StateFormula::register_use( condition_ );
  Effect::register_use( effect_ );
  notify( this, "ConditionalEffect::ConditionalEffect(StateFormula&,Effect&)" );
}


ConditionalEffect::~ConditionalEffect()
{
  StateFormula::unregister_use( condition_ );
  Effect::unregister_use( effect_ );
}


const Effect&
ConditionalEffect::flatten( void ) const
{
  const Effect *effect = &effect_->flatten();
  const ProbabilisticEffect *prob_effect 
    = dynamic_cast<const ProbabilisticEffect*>(effect);
  const ConditionalEffect *cond_effect 
    = dynamic_cast<const ConditionalEffect*>(effect);

  if( effect == this )
    {
      Effect::unregister_use( effect );
      Effect::register_use( this );
      return( *this );
    }
  else if( prob_effect != NULL )
    {
      ProbabilisticEffect *result = new ProbabilisticEffect;
      for( size_t i = 0; i < prob_effect->size(); ++i )
	{
	  StateFormula::register_use( &condition() );
	  Effect::register_use( &prob_effect->effect( i ) );
	  const Effect *ceff = &make( condition(), prob_effect->effect( i ) );
	  result->add_outcome( prob_effect->probability(i), ceff->flatten() );
	  Effect::unregister_use( ceff );
	}
      Effect::unregister_use( effect );
      return( *result );
    }
  else if( cond_effect != NULL )
    {
      Conjunction *cond = new Conjunction;
      StateFormula::register_use( &condition() );
      cond->add_conjunct( condition() );
      StateFormula::register_use( &cond_effect->condition() );
      cond->add_conjunct( cond_effect->condition() );

      Effect::register_use( &cond_effect->effect() );
      const Effect *result = &make( *cond, cond_effect->effect() );
      Effect::unregister_use( effect );
      return( *result );
    }
  else
    {
      StateFormula::register_use( &condition() );
      const Effect *result = &make( condition(), *effect );
      return( *result );
    }
  
}


void
ConditionalEffect::state_change( AtomList& adds, AtomList& deletes,
				 AssignmentList& assignments,
				 const state_t& state ) const
{
  if( condition().holds( state ) )
    effect().state_change( adds, deletes, assignments, state );
}


effect_t* 
ConditionalEffect::translate( void ) const
{
  if( working_ )
    throw Exception( "ConditionalEffect::translate: error: already working" );

  working_ = true;
  conditionalEffect_t *ceffect = new conditionalEffect_t();
  condition().translate( ceffect->precondition() );
  effect_t *eff = effect().translate();
  ceffect->set_effect(eff);
  ceffect->set_max_reward(eff->get_max_reward());
  working_ = false;
  return ceffect;
}


const Effect&
ConditionalEffect::instantiation( const SubstitutionMap& subst,
				  const problem_t& problem ) const
{
  const StateFormula *cond = &condition().instantiation( subst, problem );
  const Effect *eff = &effect().instantiation( subst, problem );
  const Effect *result = &make( *cond, *eff );
  return( *result );
}


bool
ConditionalEffect::operator==( const Effect& eff ) const
{
  const ConditionalEffect *ceff = dynamic_cast<const ConditionalEffect*>(&eff);
  return( (ceff != NULL) 
        && (condition() == ceff->condition()) && (effect() == ceff->effect()) );
}


void
ConditionalEffect::print( std::ostream& os,
			  const PredicateTable& predicates,
			  const FunctionTable& functions,
			  const TermTable& terms ) const
{
  os << "(when ";
  condition().print( os, predicates, functions, terms );
  os << ' ';
  effect().print( os, predicates, functions, terms );
  os << ")";
}


void
ConditionalEffect::analyze( PredicateTable &predicates, TermTable &terms,
			    std::map<const StateFormula*,const Atom*> &hash ) const
{
  condition().analyze( predicates, terms, hash );
  effect().analyze( predicates, terms, hash );
}


const Effect&
ConditionalEffect::rewrite( std::map<const StateFormula*,const Atom*> &hash ) const
{
  return( ConditionalEffect::make( condition().rewrite( hash ), effect().rewrite( hash ) ) );
}


/*******************************************************************************
 *
 * probabilistic effect
 *
 ******************************************************************************/

ProbabilisticEffect::ProbabilisticEffect()
  : weight_sum_(0)
{
  weights_ = std::vector<double>(0);
  Effect::register_use( this );
  notify( this, "ProbabilisticEffect::ProbabilisticEffect()" );
}


ProbabilisticEffect::~ProbabilisticEffect()
{
  for( EffectList::const_iterator ei = effects_.begin(); 
                                                    ei != effects_.end(); ++ei )
    Effect::unregister_use( *ei );
}


bool
ProbabilisticEffect::add_outcome( double p, const Effect& effect )
{
  const ProbabilisticEffect* prob_effect =
    dynamic_cast<const ProbabilisticEffect*>(&effect);
  if( prob_effect != NULL )
    {
      for( size_t i = 0; i < prob_effect->size(); ++i )
	{
	  Effect::register_use( &prob_effect->effect( i ) );
	  if( !add_outcome( p*prob_effect->probability(i), 
			    prob_effect->effect(i) ) )
	    return( false );
	}
      Effect::unregister_use( &effect );
    }
  else if( p != 0 )
    {
      effects_.push_back( &effect );    
      weights_.push_back( p );
      weight_sum_ += p;

      // This hack is here to get around a bug in the RDDL->PPDDL translator 
      // whereby, due to precision issues, the sum of effect probabilities may
      // slightly exceed 1.
      if (weight_sum_ > 1) 
	weight_sum_ = 1;

      return( weight_sum_ <= 1 );
    }
  return( true );
}


double
ProbabilisticEffect::probability( size_t i ) const
{
  return weights_[i];
}


const Effect&
ProbabilisticEffect::flatten( void ) const
{
  if( size() == 0 )
    {
      Effect::register_use( this );
      return( *this );
    }
  else
    {
      ProbabilisticEffect *result = new ProbabilisticEffect;
      for( size_t i = 0; i < size(); ++i )
	{
	  const Effect *eff = &effect( i ).flatten();
	  const ProbabilisticEffect *peff = 
	                        dynamic_cast<const ProbabilisticEffect*>( eff );
	  if( peff != NULL )
	    {
	      for( size_t j = 0; j < peff->size(); ++j )
		{
		  Effect::register_use( &peff->effect( j ) );
		  result->add_outcome( probability(i)*peff->probability(j), 
				                              peff->effect(j) );
		}
	      Effect::unregister_use( peff );
	    }
	  else
	    {
	      result->add_outcome( probability( i ), *eff );
	    }
	}

      if( (result->size() != 1) || (result->probability( 0 ) != 1) )
	return( *result );
      else
	{
	  const Effect *tmp = &result->effect( 0 );
	  Effect::register_use( tmp );
	  Effect::unregister_use( result );
	  return( *tmp );
	}
    }
}


void
ProbabilisticEffect::state_change( AtomList& adds, AtomList& deletes,
				   AssignmentList& assignments,
				   const state_t& state ) const
{
  if( size() != 0 )
    {
      double w = drand48();
      double wtot = 0;
      size_t n = size();
      for( size_t i = 0; i < n; ++i )
	{
	  wtot += weights_[i];
	  if( w < wtot )
	    {
	      effect( i ).state_change( adds, deletes, assignments, state );
	      return;
	    }
	}
    }
}


effect_t *
ProbabilisticEffect::translate( void ) const
{
  if( working_ )
    throw Exception( "ProbabilisticEffect::translate: error: already working" );
  
  working_ = true;

  probabilisticEffect_t *peffect = new probabilisticEffect_t( );

  double sum = 0;
  double max_reward = 0;
  for( size_t i = 0; i < size(); ++i )
    {
      effect_t *eff = effect( i ).translate( );
      max_reward = MAX(max_reward, eff->get_max_reward());
      peffect->insert(probability( i ), eff);
      sum = sum + probability( i );
    }

  // null effect
  if( sum != 1 )
    {
      stripsEffect_t *seffect = new stripsEffect_t( );
      peffect->insert(1 - sum, seffect );
    }

  peffect->set_max_reward(max_reward);
  working_ = false;

  return peffect;
}


const Effect&
ProbabilisticEffect::instantiation( const SubstitutionMap& subst, 
				    const problem_t& problem ) const
{
  ProbabilisticEffect& inst_effect = *new ProbabilisticEffect();
  for( size_t i = 0; i < size(); ++i )
    inst_effect.add_outcome( probability( i ), effect( i ).instantiation( subst,
								    problem ) );
  return( inst_effect );
}


bool
ProbabilisticEffect::operator==( const Effect& eff ) const
{
  const ProbabilisticEffect *peff 
    = dynamic_cast<const ProbabilisticEffect*>(&eff);
  if( (peff != NULL) && (size() == peff->size()) )
    {
      for( size_t i = 0; i < size(); ++i )
	if( !(effect( i ) == peff->effect( i )) )
	  return( false );
      return( true );
    }
  return( false );
}


void
ProbabilisticEffect::print( std::ostream& os,
			    const PredicateTable& predicates,
			    const FunctionTable& functions,
			    const TermTable& terms ) const
{
  if( weight_sum_ == 0 )
    os << "(and)";
  else if( weight_sum_ == weights_.back() )
    {
      os << "(probabilistic 1 ";
      effect(0).print( os, predicates, functions, terms );
      os << ")";
    }
  else
    {
      os << "(probabilistic";
      size_t n = size();
      for( size_t i = 0; i < n; ++i )
	{
	  os << ' ' << probability( i ) << ' ';
	  effect( i ).print( os, predicates, functions, terms );
	}
      os << ")";
    }
}


void
ProbabilisticEffect::analyze( PredicateTable &predicates, TermTable &terms,
			 std::map<const StateFormula*,const Atom*> &hash ) const
{
  for( size_t i = 0; i < size(); ++i )
    effect( i ).analyze( predicates, terms, hash );
}


const Effect&
ProbabilisticEffect::rewrite( std::map<const StateFormula*,
			                              const Atom*> &hash ) const
{
  ProbabilisticEffect *prob = new ProbabilisticEffect;
  for( size_t i = 0; i < size(); ++i )
    prob->add_outcome( probability( i ), effect( i ).rewrite( hash ) );
  return( *prob );
}


/*******************************************************************************
 *
 * quantified effect
 *
 ******************************************************************************/

QuantifiedEffect::QuantifiedEffect( const Effect& effect )
  : effect_(&effect)
{
  Effect::register_use( this );
  Effect::register_use( effect_ );
  notify( this, "QuantifiedEffect::QuantifiedEffect(Effect&)" );
}


QuantifiedEffect::~QuantifiedEffect()
{
  Effect::unregister_use( effect_ );
}


const Effect&
QuantifiedEffect::flatten( void ) const
{
#ifndef NO_STRICT
  throw Exception( "QuantifiedEffect::flatten erroneously called" );
#else
  std::cout << "QuantifiedEffect::flatten erroneously called" << std::endl;
  Effect::register_use( this );
  return( *this );
#endif
}


void
QuantifiedEffect::state_change( AtomList& adds, AtomList& deletes,
				AssignmentList& assignments,
				const state_t& state ) const
{
  effect().state_change( adds, deletes, assignments, state );
}


effect_t*
QuantifiedEffect::translate( ) const
{
#ifndef NO_STRICT
  throw Exception( "QuantifiedEffect::translate: erroneously called" );
#else
  std::cout << "QuantifiedEffect::translate: erroneously called" << std::endl;
  return NULL;
#endif
}


const Effect&
QuantifiedEffect::instantiation( const SubstitutionMap& subst,
				 const problem_t& problem ) const
{
  int n = arity();
  if( n == 0 )
    return( effect().instantiation( subst, problem ) );
  else
    {
      SubstitutionMap args( subst );
      std::vector<ObjectList> arguments( n, ObjectList() );
      std::vector<ObjectList::const_iterator> next_arg;
      for( int i = 0; i < n; ++i )
	{
	  problem.compatible_objects( arguments[i], 
				      problem.terms().type( parameter(i) ) );
	  if( arguments[i].empty() ) return( *new ConjunctiveEffect() );
	  next_arg.push_back( arguments[i].begin() );
	}

      ConjunctiveEffect* conj = new ConjunctiveEffect();
      std::stack<const Effect*> conjuncts;
      conjuncts.push( &effect().instantiation( args, problem ) );
      for( int i = 0; i < n; )
	{
	  SubstitutionMap pargs;
	  pargs.insert( std::make_pair( parameter(i), *next_arg[i] ) );
	  const Effect& conjunct = conjuncts.top()->instantiation( pargs, 
								   problem );
	  conjuncts.push( &conjunct );
	  if( i + 1 == n )
	    {
	      conj->add_conjunct( conjunct );
	      for( int j = i; j >= 0; --j )
		{
		  if( j < i ) Effect::unregister_use( conjuncts.top() );
		  conjuncts.pop();
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
	    {
	      ++i;
	    }
	}

      while( !conjuncts.empty() )
	{
	  Effect::unregister_use( conjuncts.top() );
	  conjuncts.pop();
	}
      return( *conj );
    }
}


bool
QuantifiedEffect::operator==( const Effect& eff ) const
{
  return( false );
}


void
QuantifiedEffect::print( std::ostream& os,
			 const PredicateTable& predicates,
			 const FunctionTable& functions,
			 const TermTable& terms ) const
{
  if( parameters_.empty() )
    {
      effect().print( os, predicates, functions, terms );
    }
  else
    {
      os << "(forall (";
      VariableList::const_iterator vi = parameters_.begin();
      terms.print_term( os, *vi );
      for( ++vi; vi != parameters_.end(); ++vi )
	{
	  os << ' ';
	  terms.print_term( os, *vi );
	}
      os << ") ";
      effect().print( os, predicates, functions, terms );
      os << ")";
    }
}


void
QuantifiedEffect::analyze( PredicateTable &predicates, TermTable &terms,
			 std::map<const StateFormula*,const Atom*> &hash ) const
{
  effect().analyze( predicates, terms, hash );
}


const Effect&
QuantifiedEffect::rewrite(std::map<const StateFormula*,const Atom*> &hash) const
{
  QuantifiedEffect *q = new QuantifiedEffect( effect().rewrite( hash ) );
  Effect::unregister_use( &effect() );
  q->parameters_ = parameters_;
  return( *q );
}
