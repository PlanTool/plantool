#include "problems.h"
#include "domains.h"
#include "exceptions.h"
#include "graph.h"
#include "states.h"
#include<math.h>
#include <sstream>
#include <typeinfo>
#include <map>
#include <limits>


#include <ctime>
#include <sys/times.h>
#include <sys/param.h>

/*******************************************************************************
 *
 * problem_t
 *
 ******************************************************************************/

problem_t::ProblemMap problem_t::problems = problem_t::ProblemMap();

ushort_t problem_t::atom_index_ = 0;
bool problem_t::no_more_atoms_ = false;
std::map<const Atom*,ushort_t> problem_t::atom_hash_;
std::map<ushort_t,const Atom*> problem_t::atom_inv_hash_;

ushort_t problem_t::fluent_index_ = 0;
std::map<const Application*,ushort_t> problem_t::fluent_hash_;
std::map<ushort_t,const Application*> problem_t::fluent_inv_hash_;

ushort_t
problem_t::atom_hash_get( const Atom& atom, bool negated )
{
  std::map<const Atom*,ushort_t>::const_iterator it = atom_hash_.find( &atom );
  if( it == atom_hash_.end() )
    {
      StateFormula::register_use( &atom );
      
      if( no_more_atoms_ ) 
	throw Exception( "sorry, no more atoms available" );

      if( atom_index_ == USHORT_MAX ) 
	throw Exception( "maximum number of atoms reached" );

      atom_hash_.insert( std::make_pair( &atom, atom_index_ ) );
      atom_inv_hash_.insert( std::make_pair( atom_index_, &atom ) );
      atom_index_ += 2;
      return( atom_index_ - 2 + (ushort_t)negated );
    }
  else
    return( (*it).second + (ushort_t)negated );
}

const Atom*
problem_t::atom_inv_hash_get( ushort_t atom )
{
  std::map<ushort_t,const Atom*>::const_iterator it;
  it = atom_inv_hash_.find( atom & ~0x1 );
  return( it == atom_inv_hash_.end() ? NULL : (*it).second );
}

ushort_t
problem_t::fluent_hash_get( const Application& app )
{
  std::map<const Application*,ushort_t>::const_iterator it 
                                                    = fluent_hash_.find( &app );
  if( it == fluent_hash_.end() )
    {
      Expression::register_use( &app );
      fluent_hash_.insert( std::make_pair( &app, fluent_index_ ) );
      fluent_inv_hash_.insert( std::make_pair( fluent_index_, &app ) );
      ++fluent_index_;
      return( fluent_index_ - 1 );
    }
  else
    return( (*it).second );
}

const Application*
problem_t::fluent_inv_hash_get( ushort_t fluent )
{
  std::map<ushort_t,const Application*>::const_iterator it;
  it = fluent_inv_hash_.find( fluent & ~0x1 );
  return( it == fluent_inv_hash_.end() ? NULL : (*it).second );
}

problem_t* 
problem_t::find_problem( const std::string& name )
{
  ProblemMap::const_iterator pi = problems.find( name );
  return( (pi!=problems.end()?(*pi).second:NULL) );
}

const problem_t* 
problem_t::find( const std::string& name )
{
  ProblemMap::const_iterator pi = problems.find( name );
  return( (pi!=problems.end()?(*pi).second:NULL) );
}

void
problem_t::clear( void )
{
  ProblemMap::const_iterator pi = begin();
  while( pi != end() )
    {
      problem_t::unregister_use( (*pi).second );
      pi = begin();
    }
  problems.clear();
}

problem_t *
problem_t::allocate( const std::string &name, const Domain &domain )
{
  problem_t *p = find_problem( name );
  if( p == NULL )
    p = new problem_t( name, domain );
  else
    problem_t::register_use( p );
  return( p );
}

problem_t *
problem_t::allocate( const std::string &name, const problem_t &problem )
{
  problem_t *p = find_problem( name );
  if( p == NULL )
    p = new problem_t( name, problem );
  else
    problem_t::register_use( p );
  return( p );
}

problem_t::problem_t( const std::string &name, const problem_t &problem )
  : ref_count_(0), name_(name), max_reward_(0), domain_(&problem.domain()),
    terms_(TermTable(problem.domain().terms())), 
    goal_(&problem.goal()), metric_(problem.metric()),  
    horizon_(problem.horizon()), discount_factor_(problem.discount_factor()), 
    nprec_(false), goal_atom_(false), application_graph_(NULL), noopIdx_(0)
{
  problem_t::register_use( this );
  StateFormula::register_use( goal_ );
  problems[name] = this;
  goalT_ = problem.goalT();
  terms_ = problem.terms_;
  instantiated_hash_ = problem.instantiated_hash_;

  // copy initial atoms, fluents, effects, and actions
  for( AtomSet::const_iterator ai = problem.init_atoms().begin(); ai != problem.init_atoms().end(); ++ai )
    add_init_atom( **ai );

  for( ValueMap::const_iterator vi = problem.init_fluents().begin(); vi != problem.init_fluents().end(); ++vi )
    add_init_fluent( *(*vi).first, (*vi).second );

  for( EffectList::const_iterator ei = problem.init_effects().begin(); ei != problem.init_effects().end(); ++ei )
    add_init_effect( **ei );

  for( ActionList::const_iterator ai = problem.actions().begin(); ai != problem.actions().end(); ++ai )
    {
      Action::register_use( *ai );
      actions_.push_back( *ai );
    }

  for( actionList_t::const_iterator ai = problem.actionsT().begin(); ai != problem.actionsT().end(); ++ai )
    {
      action_t::register_use( *ai );
      actionsT().push_back( *ai );
    }

  if( problem.application_graph_ != NULL )
    {
      application_graph_ = new digraph_t( *problem.application_graph_ );
      for( size_t i = 0; i < problem.restriction_.size(); ++i )
	restriction_.push_back( new atomList_t( *problem.restriction_[i] ) );
    }
}

problem_t::problem_t( const std::string &name, const Domain &domain )
  : ref_count_(0), name_(name), max_reward_(0), domain_(&domain), 
    terms_(TermTable(domain.terms())), 
    goal_(&StateFormula::FALSE), metric_(MINIMIZE_EXPECTED_COST), 
    horizon_(UNSPECIFIED), discount_factor_(UNSPECIFIED),
    nprec_(false), goal_atom_(false), application_graph_(NULL), noopIdx_(0)
{
  problem_t::register_use( this );
  StateFormula::register_use( goal_ );
  problems[name] = this;
}

problem_t::~problem_t()
{
  assert( ref_count_ == 0 );
  problems.erase( name() );

  StateFormula::unregister_use( goal_ );

  for( AtomSet::const_iterator ai = init_atoms_.begin(); 
                                                 ai != init_atoms_.end(); ++ai )
    StateFormula::unregister_use( *ai );

  for( ValueMap::const_iterator vi = init_fluents_.begin(); 
                                               vi != init_fluents_.end(); ++vi )
    Expression::unregister_use( (*vi).first );

  for( EffectList::const_iterator ei = init_effects_.begin(); 
                                               ei != init_effects_.end(); ++ei )
    Effect::unregister_use( *ei );

  for( ActionList::const_iterator ai = actions().begin(); 
                                                   ai != actions().end(); ++ai )
    Action::unregister_use( *ai );

  for( actionList_t::const_iterator ai = actionsT().begin(); 
                                                  ai != actionsT().end(); ++ai )
    action_t::unregister_use( *ai );

  if( application_graph_ != NULL )
    {
      delete application_graph_;
      for( size_t i = 0; i < restriction_.size(); ++i )
	delete restriction_[i];
    }
}

void 
problem_t::add_init_atom( const Atom& atom )
{
  if( init_atoms_.find( &atom ) == init_atoms_.end() )
    {
      StateFormula::register_use( &atom );
      init_atoms_.insert( &atom );
    }
}

void 
problem_t::add_init_fluent( const Application& application, const double& value)
{
  if( init_fluents_.find( &application ) == init_fluents_.end() )
    {
      Expression::register_use( &application );
      init_fluents_.insert( std::make_pair( &application, value ) );
    }
  else
    init_fluents_[&application] = value;
}

void 
problem_t::add_init_effect( const Effect& effect )
{
  Effect::register_use( &effect );
  init_effects_.push_back( &effect );
}

void 
problem_t::set_goal( const StateFormula& goal )
{
  if( &goal != goal_ )
    {
      StateFormula::unregister_use( goal_ );
      goal_ = &goal;
      StateFormula::register_use( goal_ );
    }
}



void 
problem_t::instantiate_actions( void )
{
  const StateFormula *ngoal = &goal().instantiation( SubstitutionMap(), *this );
  set_goal( *ngoal );
  StateFormula::unregister_use( ngoal );
  domain().instantiated_actions( actions_, instantiated_hash_, *this);

  // generate atoms
  std::map<const StateFormula*,const Atom*>::const_iterator hi;
  for( hi = instantiated_hash_.begin(); hi != instantiated_hash_.end(); ++hi )
    {
      (*hi).first->generate_atoms();
      (*hi).second->generate_atoms();
    }
}

const Application *action_t::app_;
const Application *problem_t::app_;

void 
problem_t::flatten( void )
{
  // goal
  const StateFormula &g = goal().flatten();
  g.translate( goalT_ );
  StateFormula::unregister_use( &g );
  
  std::pair<Function,bool> fp = domain().functions().find_function("reward");
  if (fp.second == false)
    {
      std::cout<<"Why is it missing?"<<std::endl;
    }
  // check if this memory needs to be released!

  action_t::app_ = &Application::make_application(fp.first, TermList());
  problem_t::app_ =  &Application::make_application(fp.first, TermList());

  // actions
  std::map<ushort_t, std::list<const effect_t*> >* mapNoopsEffects = NULL;

  size_t noopIdx = 0;
  for( ActionList::const_iterator it = actions().begin(); 
                                                   it != actions().end(); ++it )
    {
      action_t *action = &(*it)->translate( *this );

      if (action->is_noop())
	{
	  noopIdx_ = noopIdx;
	  action->find_proper_vars_and_effs(mapNoopsEffects);
	  mapNoopsEffects = action->get_proper_vars_and_effs();
	}

      max_reward_ = MAX(max_reward_, action->get_max_reward());
      actionsT().push_back( action );
      noopIdx++;
      restriction_.push_back( new atomList_t );
    }

  for (size_t i = 0; i < actionsT().size(); i++)
    {
      if (!actionsT()[i]->is_noop())
	{
	  actionsT()[i]->find_proper_vars_and_effs(mapNoopsEffects);
	}
    }
}


// Fills the provided object list with objects (including constants
// declared in the domain) that are compatible with the given type.
void 
problem_t::compatible_objects( ObjectList& objects, Type type ) const
{
  domain().compatible_constants( objects, type );
  Object last = terms().last_object();
  for( Object i = terms().first_object(); i <= last; ++i )
    if( domain().types().subtype( terms().type( i ), type ) )
      objects.push_back( i );
}

void
problem_t::complete_state( state_t &state ) const
{
  // insert negative atoms
  if( nprec() )
    {
      std::cout<<"do we ever get here?"<<std::endl;
      for( ushort_t atom = 0; atom < problem_t::number_atoms(); atom += 2 )
	if( !state.holds( atom ) )
	  state.add( atom + 1 );
    }

  // insert internal predicates
  std::map<const StateFormula*,const Atom*>::const_iterator hi;
  for( hi = instantiated_hash_.begin(); hi != instantiated_hash_.end(); ++hi )
    {
      ushort_t atom = problem_t::atom_hash_get( *(*hi).second );
      if( (*hi).first->holds( state ) )
	{
	  state.clear( 1+atom );
	  state.add( atom );
	}
      else
	{
	  state.clear( atom );
	  state.add( 1+atom );
	}
    }
}

void 
problem_t::enabled_actions( ActionList& actions, const state_t& state ) const
{
  for( ActionList::const_iterator ai = actions_.begin(); ai != actions_.end(); 
                                                                          ++ai )
    if( (*ai)->enabled( state ) ) actions.push_back( *ai );
}

void 
problem_t::print( std::ostream& os, const StateFormula &formula ) const
{
  formula.print( os, domain_->predicates(), domain_->functions(), terms() );
}

void 
problem_t::print( std::ostream& os, const Application &app ) const
{
  app.print( os, domain_->functions(), terms() );
}

void 
problem_t::print( std::ostream& os, const Action &action ) const
{
  action.print( os, terms() );
}

void 
problem_t::print_full( std::ostream& os, const Action &action ) const
{
  action.print_full( os, domain_->predicates(), domain_->functions(), terms() );
}

double 
problem_t::eval_action( action_t &action, const state_t &state ) const
{
  ValueMap vm;
  vm[app_] = 0;
  state_t dummy;
  std::list<const effect_t*>* rewardEffs = action.get_reward_effs();

  for (std::list<const effect_t*>::iterator it_eff = rewardEffs->begin(); 
                                          it_eff != rewardEffs->end(); it_eff++)
    {
      (*it_eff)->affect( state, dummy, &vm);
    }

  return vm[app_];
}

double 
problem_t::eval_all_actions( const state_t &state ) const
{
  double max_rew = -1000000000;

  for( size_t k = 0; k < actionsT().size(); k++ )
    {
      max_rew = MAX(max_rew, eval_action(*actionsT()[k], state));
    }

  return max_rew;
}

void
problem_t::initial_states( std::pair<state_t*,double> *list ) const
{
  state_t *state = new state_t;
  for( AtomSet::const_iterator ai = init_atoms().begin(); 
                                                ai != init_atoms().end(); ++ai )
    if( !domain().predicates().static_predicate( (*ai)->predicate() ) )
      state->add( **ai );

#ifdef FULL_STATES
  for( ValueMap::const_iterator vi = init_fluents().begin(); 
                                              vi != init_fluents().end(); ++vi )
    {
      Function function = (*vi).first->function();
      if( !domain().functions().static_function( function ) )
	state->add( *(*vi).first, (*vi).second );
    }
#endif

  complete_state( *state );
  state->make_digest();
  const stateProbList_t *state_list = new stateProbList_t( state, 1 );
  
  // In this implementation, we ignore initial effects. Initial effects can
  // change the state of the world before the agent gets to execute any actions.
  // Thus, in problems with initial effects the agent could stochastically start
  // in one of several possible initial states. Here, we assume instead that
  // the initial state is deterministic and is given as part of the problem
  // description. 

  // fill the given list wth result
  size_t i = 0;
  for( stateProbList_t::const_iterator si = state_list->begin(); 
                                                 si != state_list->end(); ++si )
    {
      *list[i].first = *(*si).first;
      list[i].second = (*si).second;
      delete (*si).first;
      ++i;
    }
  list[i].second = std::numeric_limits<double>::max( );
  delete state_list;
}

void
problem_t::analyze_symmetries( void )
{
  atomList_t add_i, del_i, add_j, del_j;

  // first: operators that everything they add is persistent and do not delete 
  // anything
  std::vector<const action_t*> operators;
  std::map<const action_t*,ushort_t> map;
  for( size_t i = 0; i < actionsT().size(); ++i )
    {
      size_t j;
      actionsT()[i]->collect_del_atoms( del_i );
      if( del_i.size() == 0 )
	{
	  actionsT()[i]->collect_add_atoms( add_i );
	  for( j = 0; j < actionsT().size(); ++j )
	    {
	      actionsT()[j]->collect_del_atoms( del_j );
	      if( !add_i.empty_intersection( del_j ) )
		{
		  del_j.clear();
		  break;
		}
	      del_j.clear();
	    }
	  add_i.clear();

	  if( j == actionsT().size() )
	    {
	      operators.push_back( actionsT()[i] );
	      map[actionsT()[i]] = i;
	    }
	}
      del_i.clear();
    }
  if( operators.size() == 0 ) return;

  // create operator graph
  graph_t operator_graph( operators.size() );
  for( size_t i = 0; i < operators.size(); ++i )
    {
      operators[i]->collect_add_atoms( add_i );
      for( size_t j = i+1; j < operators.size(); ++j )
	{
	  // add edge if actions i and j are interfering
	  operators[j]->collect_add_atoms( add_j );
	  if( !add_i.empty_intersection( add_j ) )
	    operator_graph.add_edge( i, j );
	  add_j.clear();
	}
      add_i.clear();
    }

#if 0  
  std::cout << "<operator-graph>: begin" << std::endl;
  operator_graph.print( std::cout );
  std::cout << "<operator-graph>: end" << std::endl;
#endif

  std::vector<atomList_t*> cc;
  operator_graph.connected_components( cc );
#if 0
  for( size_t i = 0; i < cc.size(); ++i )
    {
      std::cout << "<cc-" << 1+i << ">: {";
      for( size_t j = 0; j < cc[i]->size(); ++j )
	{
	  ushort_t act = cc[i]->atom( j );
	  std::cout << " " << operators[act]->name();
	}
      std::cout << " }" << std::endl;
    }
#endif

  // create connected-component (directed) graph
  digraph_t cc_graph( cc.size() );
  for( size_t i = 0; i < cc.size(); ++i )
    {
      for( size_t k = 0; k < cc[i]->size(); ++k )
	operators[cc[i]->atom(k)]->collect_add_atoms( add_i );

      for( size_t j = 0; j < cc.size(); ++j )
	if( i != j )
	  {
	    for( size_t k = 0; k < cc[j]->size(); ++k )
	      operators[cc[j]->atom(k)]->collect_prec_atoms( add_j );
	    if( !add_i.empty_intersection( add_j ) )
	      cc_graph.add_edge( i, j );
	    add_j.clear();
	  }
      add_i.clear();
    }
  if( !cc_graph.acyclic() ) return;

#if 0
  std::cout << "<cc-digraph>: begin" << std::endl;
  cc_graph.print( std::cout );
  std::cout << "<cc-digraph>: end" << std::endl;
#endif

  std::vector<atomList_t*> layers;
  cc_graph.layer( layers );
  for( size_t i = 0; i < layers.size(); ++i )
    {
      std::cout << "<layer-" << 1+i << ">: {";
      for( size_t j = 0; j < layers[i]->size(); ++j )
	std::cout << " cc-" << 1+layers[i]->atom( j );
      std::cout << " }" << std::endl;
    }

  // create application graph
  application_graph_ = new digraph_t( actionsT().size() );
  for( size_t i = 0; i < layers.size(); ++i )
    {
      for( size_t j = 0; j < layers[i]->size()-1; ++j )
	application_graph_->add_edge( map[operators[layers[i]->atom(j)]],
				      map[operators[layers[i]->atom(1+j)]] );
    }
  application_graph_->closure();

  std::cout << "<application-digraph>: begin" << std::endl;
  application_graph_->print( std::cout );
  std::cout << "<application-digraph>: end" << std::endl;

  for( ushort_t i = 0; i < actionsT().size(); ++i )
    {
      atomList_t *alist = new atomList_t;
      for( ushort_t j = 0; j < actionsT().size(); ++j )
	if( (i != j) && application_graph_->edge(i,j) )
	  alist->insert( j );
      restriction_[i] = alist;
      std::cout << "restriction[" << i << "] = " << *alist << std::endl;
    }

  // clean
  for( size_t i = 0; i < cc.size(); ++i )
    delete cc[i];
  for( size_t i = 0; i < layers.size(); ++i )
    delete layers[i];
}

void
problem_t::add_orbit( std::string *name, std::vector<const Atom*> *atoms )
{
  orbits_[*name] = atoms;
}

void
problem_t::add_system( std::string *name, 
		       std::vector<const std::string*> *focus,
		       std::vector<const std::string*> *base,
		       std::vector<const std::string*> *frame )
{
  systems_[*name] = new system_t( focus, base, frame );
}

std::ostream& 
operator<<( std::ostream& os, const problem_t& p )
{
  os << "name: " << p.name();
  os << std::endl << "domain: " << p.domain().name();
  os << std::endl << "objects:";
  for( Object i = p.terms().first_object(); i <= p.terms().last_object(); ++i )
    {
      os << std::endl << "  ";
      p.terms().print_term( os, i );
      os << " - ";
      p.domain().types().print_type( os, p.terms().type( i ) );
    }
  os << std::endl << "init:";
  for( AtomSet::const_iterator ai = p.init_atoms_.begin(); 
                                               ai != p.init_atoms_.end(); ++ai )
    {
      os << std::endl << "  ";
      (*ai)->print( os, p.domain().predicates(), p.domain().functions(), 
		                                                    p.terms() );
    }
  for( ValueMap::const_iterator vi = p.init_fluents_.begin(); 
                                             vi != p.init_fluents_.end(); ++vi )
    {
      os << std::endl << "  (= ";
      (*vi).first->print( os, p.domain().functions(), p.terms() );
      os << ' ' << (*vi).second << ")";
    }
  for( EffectList::const_iterator ei = p.init_effects_.begin(); 
                                             ei != p.init_effects_.end(); ++ei )
    {
      os << std::endl << "  ";
      (*ei)->print( os, p.domain().predicates(), p.domain().functions(), 
		                                                    p.terms() );
  }
  os << std::endl << "goal: ";
  p.goal().print( os, p.domain().predicates(), p.domain().functions(), 
		                                                    p.terms() );
  os << std::endl << "metric: ";
  if( p.metric() == problem_t::MINIMIZE_EXPECTED_COST )
    os << "minimize-expected-cost";
  else
    os << "maximize-goal-probability";
  os << std::endl << "actions:";
  for( ActionList::const_iterator ai = p.actions_.begin(); ai != p.actions_.end(); ++ai )
    {
      os << std::endl << "  ";
      (*ai)->print( os, p.terms() );
    }
  return( os );
}

