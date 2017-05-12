#include "cnf.h" 
#include "dtree.h"
#include "satlib.h"
#include "strxml.h"

#include <stdio.h>
#include <strings.h>
#include <assert.h>

#ifdef DEBUG
extern int verbosity_level;
#endif

namespace dtree {

void
case_analysis_continuation( std::pair<const Node*,Manager*> *ctx, cnf::Manager *cnf_manager );

void
Manager::insert_cache( const cnf::VarList &context, const cnf::Manager &cnf_manager, nnf::node alpha )
{
  const char *blueprint = cnf_manager.compute_blueprint( context );
  unsigned key = cnf_manager.hash( blueprint );
  if( insert_cache( key, alpha ) )
    {
#ifdef DEBUG
      if( verbosity_level >= 100 )
	{
	  std::cout << "cache: insert: "
		    << "key=" << key
		    << ", blueprint=" << blueprint << std::endl
		    << cnf_manager
		    << alpha
		    << std::endl;
	}
#endif
    }
}

#if 1
void
Node::insert_cache( const cnf::Manager &cnf_manager, nnf::node alpha ) const
{
  const char *blueprint = cnf_manager.compute_blueprint( context_ );
  unsigned key = cnf_manager.hash( blueprint );
  if( insert_cache( key, alpha ) )
    {
#ifdef DEBUG
      if( verbosity_level >= 100 )
	{
	  std::cout << "cache: insert: "
		    << "key=" << key
		    << ", blueprint=" << blueprint << std::endl
		    << cnf_manager
		    << alpha
		    << std::endl;
	}
#endif
    }
}
#endif

nnf::node
Node::make_ddnnf( Manager &dtree_manager, const cnf::Manager &cnf_manager ) const
{
  // perform all initial unit resolutions
  cnf::VarList lits;
  ((cnf::Manager&)cnf_manager).preprocess( lits );

  // disable branching on all variables (save previous status)
  cnf::VarList vars;
  for( cnf::VarIndex var = 1; var <= cnf_manager.number_variables(); ++var )
    if( cnf_manager.is_var_branchable( var ) )
      {
	vars.push_back( var );
	((cnf::Manager&)cnf_manager).disable_branch( var );
      }

  nnf::node alpha = make_ddnnf_aux( dtree_manager, (cnf::Manager&)cnf_manager );

  // restore branching status
  for( cnf::VarList::const_iterator vi = vars.begin(); vi != vars.end(); ++vi )
    ((cnf::Manager&)cnf_manager).enable_branch( *vi );

  // conjoin alpha with literals found during unit resolutions
  nnf::node node = nnf::null_node;
  for( cnf::VarList::const_iterator li = lits.begin(); li != lits.end(); ++li )
    {
      nnf::node tmp = nnf::make_variable( &dtree_manager(), *li );
      if( node == nnf::null_node )
	node = tmp;
      else
	{
	  nnf::node t = nnf::make_and( &dtree_manager(), node, tmp );
	  dtree_manager().unregister_use( node );
	  dtree_manager().unregister_use( tmp );
	  node = t;
	}
    }

  if( node == nnf::null_node )
    node = alpha;
  else
    {
      nnf::node tmp = nnf::make_and( &dtree_manager(), node, alpha );
      dtree_manager().unregister_use( node );
      dtree_manager().unregister_use( alpha );
      node = tmp;
    }
  return( node );
}

void
Node::printXML( std::ostream &os, int indent ) const
{
  for( int i = 0; i < indent; ++i ) os << ' ';
  os << "<label>" << label_ << "</label>" << std::endl;
  for( int i = 0; i < indent; ++i ) os << ' ';
  os << "<depth>" << depth_ << "</depth>" << std::endl;
  for( int i = 0; i < indent; ++i ) os << ' ';
  os << "<dlevel>" << dlevel_ << "</dlevel>" << std::endl;
  for( int i = 0; i < indent; ++i ) os << ' ';
  os << "<variables>" << variables_ << "</variables>" << std::endl;
  for( int i = 0; i < indent; ++i ) os << ' ';
  os << "<clauses>" << clauses_ << "</clauses>" << std::endl;
  for( int i = 0; i < indent; ++i ) os << ' ';
  os << "<separator>" << separator_ << "</separator>" << std::endl;
  for( int i = 0; i < indent; ++i ) os << ' ';
  os << "<context>" << context_ << "</context>" << std::endl;
}

void
Internal::clean_cache( nnf::Manager &nnf_manager ) const
{
  left_->clean_cache( nnf_manager );
  right_->clean_cache( nnf_manager );
  for( nnf::cacheS::const_iterator it = cache_.begin(); it != cache_.end(); ++it )
    nnf_manager.unregister_use( (*it).second );
}

size_t
Internal::dump( std::ostream &os, size_t index ) const
{
  size_t l = left_->dump( os, index );
  size_t r = right_->dump( os, 1+l );
  os << "I " << l << " " << r << std::endl;
  dlevel_ = 1 + r;
  return( 1+r );
}

void
Internal::printXML( std::ostream &os, int indent ) const
{
  for( int i = 0; i < indent; ++i ) os << ' ';
  os << "<dtree>" << std::endl;

  Node::printXML( os, indent + 2 );
  left_->printXML( os, indent + 2 );
  right_->printXML( os, indent + 2 );

  for( int i = 0; i < indent; ++i ) os << ' ';
  os << "</dtree>" << std::endl;
}

void
Internal::printGraphViz( std::ostream &os ) const
{
  os << "  N_" << (unsigned)this << " -> N_" << (unsigned)left_ << ";" << std::endl;
  os << "  N_" << (unsigned)this << " -> N_" << (unsigned)right_ << ";" << std::endl;
#if 0
  os << "  N_" << (unsigned)this << "[label=\"{ ";
  for( cnf::VarList::const_iterator it = separator_.begin(); it != separator_.end(); ++it )
    os << *it << ' ';
  os << "}:{ ";
  for( cnf::VarList::const_iterator it = context_.begin(); it != context_.end(); ++it )
    os << *it << ' ';
  os << "}\"];" << std::endl;
#endif
  left_->printGraphViz( os );
  right_->printGraphViz( os );
}

void
Internal::label_cnf_clauses( cnf::Manager &cnf_manager ) const
{
  left_->label_cnf_clauses( cnf_manager );
  right_->label_cnf_clauses( cnf_manager );
}

void
Internal::compute_meta_data( int &label, unsigned depth, cnf::VarList cutset )
{
  // depth
  depth_ = depth;

  // separator
  cnf::VarList tmp;
  left_->variables().intersect( right_->variables(), tmp );
  for( cnf::VarList::const_iterator vi = tmp.begin(); vi != tmp.end(); ++vi )
    if( !cutset.find( *vi ) ) separator_.push_back( *vi );

  // new cutset
  cnf::VarList ncutset = cutset;
  ncutset.insert( ncutset.end(), separator_.begin(), separator_.end() );

  // context
  variables_.intersect( ncutset, context_ );

  // left recursion
  left_->compute_meta_data( label, 1+depth, ncutset );

  // label
  label_ = label++;

  // right recursion
  right_->compute_meta_data( label, 1+depth, ncutset );
}

int
Internal::width_aux( void ) const
{
  int sz1 = context_.size(), sz2 = left_->width_aux(), sz3 = right_->width_aux();
  int max = sz1 > sz2 ? sz1 : sz2;
  return( max > sz3 ? max : sz3 );
}

nnf::node
Internal::make_ddnnf_aux( Manager &dtree_manager, cnf::Manager &cnf_manager ) const
{
  // check cache
  const char *blueprint = cnf_manager.compute_blueprint( context() );
  unsigned key = cnf_manager.hash( blueprint );
  nnf::node cached = lookup_cache( key );
  if( cached != nnf::null_node )
    {
#ifdef DEBUG
      if( verbosity_level >= 200 )
	{
	  std::cout << "cache: hit on internal@" << this << ": "
		    << "key=" << key
		    << ", blueprint=" << blueprint << std::endl
		    << cnf_manager
		    << cached
		    << std::endl;
	}
#endif
      dtree_manager().register_use( cached );
      return( cached );
    }
  else
    {
      nnf::node ddnnf = manual_case_analysis( dtree_manager, cnf_manager );
      if( false && insert_cache( key, ddnnf ) )
	{
	  dtree_manager().register_use( ddnnf );
#ifdef DEBUG
	  if( verbosity_level >= 100 )
	    {
	      std::cout << "cache: insert on internal@" << this << ": "
			<< "key=" << key
			<< ddnnf
			<< cnf_manager
			<< std::endl;
	    }
#endif
	}
      return( ddnnf );
    }
}

nnf::node
Internal::make_context_dnnf( Manager &dtree_manager, cnf::Manager &cnf_manager ) const
{
  //std::cout << "making separator at depth " << depth() << std::endl;
  nnf::node alpha = nnf::make_value( &dtree_manager(), true );
  for( cnf::VarIndex idx = 1; idx <= cnf_manager.number_variables(); ++idx )
    {
      cnf::Variable &var = cnf_manager.variable( idx );
      unsigned value = var.value();
      if( (value != UNKNOWN) && (var.get_dlevel() > dlevel()) )
	{
	  nnf::node right = nnf::make_variable( &dtree_manager(), (1-var.value()) + (idx<<1) );
	  nnf::node tmp = nnf::make_and( &dtree_manager(), alpha, right );
	  dtree_manager().unregister_use( alpha );
	  dtree_manager().unregister_use( right );
	  alpha = tmp;
	}
      else if( value != UNKNOWN )
	{
	  //std::cout << "not adding " << (value?"v":"~v") << idx << " cause " << var.get_dlevel() << ":" << dlevel() << std::endl;
	}
    }
  //std::cout << "finish separator at depth " << depth() << std::endl;
  return( alpha );
}

nnf::node
Internal::case_analysis( Manager &dtree_manager, cnf::Manager &cnf_manager ) const
{
  // enable branching on separator variables (save status)
  cnf::VarList vars;
  for( cnf::VarList::const_iterator vi = separator_.begin(); vi != separator_.end(); ++vi )
    if( !cnf_manager.is_var_branchable( *vi ) )
      {
	vars.push_back( *vi );
	cnf_manager.enable_branch( *vi );
      }

  set_dlevel( cnf_manager.dlevel() );
  std::pair<const Node*,Manager*> ctx( this, &dtree_manager );
  std::pointer_to_binary_function<std::pair<const Node*,Manager*>*,cnf::Manager*,void> cont( case_analysis_continuation );
  cnf_manager.enumerate_models( std::bind1st( cont, &ctx ) );

  // restore branching status
  for( cnf::VarList::const_iterator vi = vars.begin(); vi != vars.end(); ++vi )
    cnf_manager.disable_branch( *vi );

  // make disjunction of results (from stack at current level)
  nnf::node alpha = nnf::make_value( &dtree_manager(), false );
  for( nnf::node tmp = dtree_manager.pop_stack( depth() ); tmp != 0; tmp = dtree_manager.pop_stack( depth() ) )
    {
      nnf::node tmp2 = nnf::make_or( &dtree_manager(), alpha, tmp );
      dtree_manager().unregister_use( alpha );
      dtree_manager().unregister_use( tmp );
      alpha = tmp2;
    }
  return( alpha );
}

nnf::node
Internal::manual_case_analysis( Manager &dtree_manager, cnf::Manager &cnf_manager ) const
{
  void manual_case_analysis_loopy( Manager&, cnf::Manager&, int );

  // enable branching on separator variables (save status)
  cnf::VarList vars;
  for( cnf::VarList::const_iterator vi = separator_.begin(); vi != separator_.end(); ++vi )
    if( !cnf_manager.is_var_branchable( *vi ) )
      {
	vars.push_back( *vi );
	cnf_manager.enable_branch( *vi );
      }

  // do the real thing
  set_dlevel( cnf_manager.dlevel() );
  nnf::node alpha = manual_case_analysis_aux( dtree_manager, cnf_manager, 0 );
  //std::cout << "separator = " << separator_ << std::endl;
  //manual_case_analysis_loopy( dtree_manager, cnf_manager, label_ );
  //nnf::node alpha = nnf::make_value( &dtree_manager(), false );

  // restore branching status
  for( cnf::VarList::const_iterator vi = vars.begin(); vi != vars.end(); ++vi )
    cnf_manager.disable_branch( *vi );

  return( alpha );
}

void
dump_stack( const std::vector<int> &stk, bool mixed = false )
{
  std::cout << "<" << stk.size() << ">:";
  for( size_t i = 0; i < stk.size(); ++i )
    {
      int lit = stk[i] & 0x0000ffff;
      int lev = stk[i] >> 16;
      std::cout << (lit%2?" -":" +") << (lit>>1);
      if( mixed ) std::cout << "@" << lev;
    }
  std::cout << std::endl;
}

inline void
wrapup_stacks( cnf::Manager &cnf_manager, std::vector<int> &dstack, std::vector<int> &fstack, int lit )
{
  do {
    lit = dstack.back();
    dstack.pop_back();
  } while( (lit%2 == 1) && !dstack.empty() );

  int new_dl = dstack.size();
  cnf_manager.back_track( 1+new_dl );
  //while( !fstack.empty() && (fstack.back()>>16) > new_dl ) fstack.pop_back();

  if( lit%2 == 0 )
    {
      dstack.push_back( 1 + lit );
      //fstack.push_back( (new_dl<<16)+lit );
    }
}

void
manual_case_analysis_loopy( Manager &dtree_manager, cnf::Manager &cnf_manager, int label )
{
  std::vector<int> dstack, fstack;

  cnf::VarIndex dvar = cnf_manager.choose_decision_variable( label );
  dstack.push_back( dvar<<1 );

  while( !dstack.empty() )
    {
#if 1
      // dump stack
      std::cout << "dstack = "; dump_stack( dstack );
      //std::cout << "fstack = "; dump_stack( fstack, true );
      cnf_manager.dump_assignment_stack();
#endif

      assert( cnf_manager.dlevel() == (int)dstack.size() - 1 );
      int dlit = dstack.back();
      int sign = dlit%2;
      dvar = dlit>>1;

      if( !dvar )
	{
	  dstack.pop_back(); // get rid of v0

	  // do some useful stuff ...
	  std::cout << "model = ";
	  print_var_assignment( &std::cout, &cnf_manager );
	  std::cout << std::endl;

	  // backtrack
	  if( !dstack.empty() )
	    wrapup_stacks( cnf_manager, dstack, fstack, dlit );
	  continue;
	}

      // assert new decision
      std::cout << "at level " << dstack.size()-1 << ", assert " << (sign?"-v":"+v") << dvar << std::endl;
      cnf_manager.make_decision( dvar, sign );

    deduce:
      // unit resolution
      if( cnf_manager.deduce() == CONFLICT )
	{
	  //int dl = cnf_manager.dlevel();
	  size_t new_dl = cnf_manager.analyze_conflicts();
	  cnf_manager.dump_implication_queue(); std::cout << std::endl;

	  // backtrack decision-stack
	  std::cout << "before backtrack:" << std::endl;
	  std::cout << "  dstack = "; dump_stack( dstack );
	  //std::cout << "  fstack = "; dump_stack( fstack, true );
	  while( dstack.size() > new_dl ) dstack.pop_back();
	  sign = dstack.back() % 2;
#if 0
	  while( !fstack.empty() && (fstack.back()>>16) > new_dl ) fstack.pop_back();
	  if( new_dl > 0 )
	    {
	      int lit = dstack.back();
	      sign = lit%2;
	      fstack.push_back( (new_dl<<16)+lit );
	    }
	  dstack.pop_back();
#endif
	  std::cout << "after backtrack:" << std::endl;
	  std::cout << "  dstack = "; dump_stack( dstack );
	  //std::cout << "  fstack = "; dump_stack( fstack, true );
	  cnf_manager.dump_assignment_stack();

	  if( dstack.empty() || (sign == 0) )
	    {
	      if( sign == 1 )
		{
		  cnf_manager.clear_implication_queue();
		  continue;
		}
	      else goto deduce;
	    }
	  else
	    {
	      cnf_manager.clear_implication_queue();
	      wrapup_stacks( cnf_manager, dstack, fstack, dlit );
	      continue;
	    }
	}
      else
	{
	  dvar = cnf_manager.choose_decision_variable( label );
	  dstack.push_back( dvar<<1 );
	}
    }
}

nnf::node
Internal::manual_case_analysis_aux( Manager &dtree_manager, cnf::Manager &cnf_manager, int depth ) const
{
  if( cnf_manager.deduce() == CONFLICT )
    {
      cnf_manager.clear_conflicts();
      if( depth > 0 ) cnf_manager.back_track( dlevel() + depth );
      return( nnf::make_value( &dtree_manager(), false ) );
    }
  else
    {
      nnf::node alpha = nnf::null_node;
      cnf::VarIndex dvar = cnf_manager.choose_decision_variable( label_ );
      if( !dvar )
	{
	  // extract clauses from left node, i,e, freeze all clauses in right node
	  for( cnf::ClauseList::const_iterator ci = right_->clauses().begin(); ci != right_->clauses().end(); ++ci )
	    cnf_manager.freeze_clause( *ci );
	  nnf::node alpha_l = left_->make_ddnnf_aux( dtree_manager, cnf_manager );
	  for( cnf::ClauseList::const_reverse_iterator ci = right_->clauses().rbegin(); ci != right_->clauses().rend(); ++ci )
	    cnf_manager.reanimate_clause( *ci );

	  if( !dtree_manager().is_constant_false( alpha_l ) )
	    {
	      // extract clauses from right node, i,e, freeze all clauses in left node
	      for( cnf::ClauseList::const_iterator ci = left_->clauses().begin(); ci != left_->clauses().end(); ++ci )
		cnf_manager.freeze_clause( *ci );
	      nnf::node alpha_r = right_->make_ddnnf_aux( dtree_manager, cnf_manager );
	      for( cnf::ClauseList::const_reverse_iterator ci = left_->clauses().rbegin(); ci != left_->clauses().rend(); ++ci )
		cnf_manager.reanimate_clause( *ci );

	      // conjoin prefix, left and right ddnnf's
	      alpha = nnf::make_and( &dtree_manager(), alpha_l, alpha_r );
	      dtree_manager().unregister_use( alpha_l );
	      dtree_manager().unregister_use( alpha_r );
	    }
	  else
	    alpha = alpha_l;

	  // make result
	  if( !dtree_manager().is_constant_false( alpha ) )
	    {
	      nnf::node sep = make_context_dnnf( dtree_manager, cnf_manager );
	      nnf::node tmp = nnf::make_and( &dtree_manager(), sep, alpha );
	      dtree_manager().unregister_use( sep );
	      dtree_manager().unregister_use( alpha );
	      alpha = tmp;
	    }
	}
      else
	{
	  nnf::node alpha_pos, alpha_neg;

	  //int dl = cnf_manager.dlevel();
	  //xxxx std::cout << "at level " << dl << ", decision<" << 1+dl << "> = +v" << dvar << std::endl;
	  cnf_manager.make_decision( dvar, 0 );
	  alpha_pos = manual_case_analysis_aux( dtree_manager, cnf_manager, 1+depth );

	  if( !dtree_manager().is_constant_true( alpha_pos ) )
	    {
	      //xxxx std::cout << "at level " << dl << ", decision<" << 1+dl << "> = -v" << dvar << std::endl;
	      cnf_manager.make_decision( dvar, 1 );
	      alpha_neg = manual_case_analysis_aux( dtree_manager, cnf_manager, 1+depth );
	      alpha = nnf::make_or( &dtree_manager(), alpha_neg, alpha_pos );
	      dtree_manager().unregister_use( alpha_neg );
	      dtree_manager().unregister_use( alpha_pos );
	    }
	  else
	    alpha = alpha_pos;
	}
      if( depth > 0 ) cnf_manager.back_track( dlevel() + depth );
      return( alpha );
    }
}

void
case_analysis_continuation( std::pair<const Node*,Manager*> *ctx, cnf::Manager *cnf_manager )
{
  nnf::node alpha = nnf::null_node;
  Manager *dtree_manager = ctx->second;
  const Internal *node = dynamic_cast<const Internal*>( ctx->first );
  if( node )
    {
      // compute dnnf prefix
      nnf::node prefix = node->make_context_dnnf( *dtree_manager, *cnf_manager );

      // extract clauses from left node, i,e, freeze all clauses in right node
      for( cnf::ClauseList::const_iterator ci = node->right()->clauses().begin(); ci != node->right()->clauses().end(); ++ci )
	  cnf_manager->freeze_clause( *ci );
      nnf::node ddnnf_l = node->left()->make_ddnnf_aux( *dtree_manager, *cnf_manager );
      for( cnf::ClauseList::const_reverse_iterator ci = node->right()->clauses().rbegin(); ci != node->right()->clauses().rend(); ++ci )
	  cnf_manager->reanimate_clause( *ci );

      // extract clauses from right node, i,e, freeze all clauses in left node
      for( cnf::ClauseList::const_iterator ci = node->left()->clauses().begin(); ci != node->left()->clauses().end(); ++ci )
	cnf_manager->freeze_clause( *ci );
      nnf::node ddnnf_r = node->right()->make_ddnnf_aux( *dtree_manager, *cnf_manager );
      for( cnf::ClauseList::const_reverse_iterator ci = node->left()->clauses().rbegin(); ci != node->left()->clauses().rend(); ++ci )
	cnf_manager->reanimate_clause( *ci );

      // conjoin prefix, left and right ddnnf's
      nnf::node tmp = nnf::make_and( &(*dtree_manager)(), ddnnf_l, ddnnf_r );
      (*dtree_manager)().unregister_use( ddnnf_l );
      (*dtree_manager)().unregister_use( ddnnf_r );
      alpha = nnf::make_and( &(*dtree_manager)(), prefix, tmp );
      (*dtree_manager)().unregister_use( prefix );
      (*dtree_manager)().unregister_use( tmp );
    }
  else
    {
      // this is a leaf node
      alpha = ((const Leaf*)ctx->first)->make_ddnnf_aux( *dtree_manager, *cnf_manager );
    }
  dtree_manager->push_stack( node->depth(), alpha );
}

void
Leaf::clean_cache( nnf::Manager &nnf_manager ) const
{
  for( nnf::cacheS::const_iterator it = cache_.begin(); it != cache_.end(); ++it )
    nnf_manager.unregister_use( (*it).second );
}

size_t
Leaf::dump( std::ostream &os, size_t index ) const
{
  os << "L " << clause_ << std::endl;
  dlevel_ = index;
  return( index );
}

void
Leaf::printXML( std::ostream &os, int indent ) const
{
  for( int i = 0; i < indent; ++i ) os << ' ';
  os << "<dtree>" << std::endl;

  Node::printXML( os, indent + 2 );
  for( int i = 0; i < indent + 2; ++i ) os << ' ';
  os << "<clause>" << clause_ << "</clause>" << std::endl;

  for( int i = 0; i < indent; ++i ) os << ' ';
  os << "</dtree>" << std::endl;
}

void
Leaf::printGraphViz( std::ostream &os ) const
{
  os << "  N_" << (unsigned)this << "[label=\"clause-" << clause_ << "\"];" << std::endl;
}

void
Leaf::label_cnf_clauses( cnf::Manager &cnf_manager ) const
{
  cnf_manager.set_meta_index( clause_, label_ );
}

void
Leaf::compute_meta_data( int &label, unsigned depth, cnf::VarList cutset )
{
  depth_ = depth;
  variables_.intersect( cutset, context_ );
  label_ = label++;
}

nnf::node
Leaf::make_ddnnf_aux( Manager &dtree_manager, cnf::Manager &cnf_manager ) const
{
  const char *blueprint = cnf_manager.compute_blueprint( context() );
  unsigned key = cnf_manager.hash( blueprint );
  nnf::node cached = lookup_cache( key );
  if( cached != nnf::null_node )
    {
#ifdef DEBUG
      if( verbosity_level >= 200 )
	{
	  std::cout << "cache: hit on leaf@" << this << ": "
		    << "key=" << key
		    << ", blueprint=" << blueprint << std::endl
		    << cnf_manager
		    << cached
		    << std::endl;
	}
#endif
      dtree_manager().register_use( cached );
      return( cached );
    }
  else
    {
      nnf::node ddnnf = cnf_manager.make_ddnnf( dtree_manager() );
      if( false && insert_cache( key, ddnnf ) )
	{
	  dtree_manager().register_use( ddnnf );
#ifdef DEBUG
	  if( verbosity_level >= 100 )
	    {
	      std::cout << "cache: insert on leaf@" << this << ": "
			<< "key=" << key
			<< ", blueprint=" << blueprint << std::endl
			<< ddnnf
			<< cnf_manager
			<< std::endl;
	    }
#endif
	}
      return( ddnnf );
    }
}

void
printGraphViz( std::ostream &os, const Node &node )
{
  os << "digraph G { " << std::endl;
  node.printGraphViz( os );
  os << "};" << std::endl;
}

const Node*
make_dtree( Manager &manager, Method method, const cnf::Manager &cnf_manager, const cnf::VarList *weak_vars )
{
  int label = 0;
  cnf::VarList cutset;
  Node *dt = cnf_manager.make_dtree( manager, method, weak_vars );
  dt->compute_meta_data( label, 0, cutset );
  return( dt );
}

Node&
readXML( const XML::Node *xml_input, cnf::Manager &cnf_manager )
{
  Node *node = 0;
  if( xml_input->get_name() != "dtree" ) throw 1;
  if( xml_input->size() == 8 )
    {
      // this is a leaf node
      const XML::Node *child = xml_input->get_child(7);
      node = new Leaf( cnf_manager, atoi( child->get_text().c_str() ), false );
    }
  else
    {
      // this is an internal node
      assert( xml_input->size() == 9 );
      Node *left = &readXML( xml_input->get_child(7), cnf_manager );
      Node *right = &readXML( xml_input->get_child(8), cnf_manager );
      node = new Internal( left, right, false );
    }
  node->set_label( atoi( xml_input->get_child(0)->get_text().c_str() ) );
  node->set_depth( atoi( xml_input->get_child(1)->get_text().c_str() ) );
  node->set_dlevel( atoi( xml_input->get_child(2)->get_text().c_str() ) );
  node->variables_.readXML( xml_input->get_child(3) );
  node->clauses_.readXML( xml_input->get_child(4) );
  node->separator_.readXML( xml_input->get_child(5) );
  node->context_.readXML( xml_input->get_child(6) );
  return( *node );
}

const Node&
readXML( std::istream &is, cnf::Manager &cnf_manager )
{
  XML::Node *xml_input = 0;
  is >> xml_input;
  return( readXML( xml_input, cnf_manager ) );
}

}; // dtree namespace
