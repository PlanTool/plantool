#include "cnf.h"
#include "dtree.h"
#include "hgraph.h"

#if 0
extern "C" {
#include <util.h>
#include <cudd.h>
#include <cuddInt.h>
};
#include <csoper.h>
#endif

extern "C" {
#include "md4.h"
};

#include <math.h>
#include <sstream>

#ifdef DEBUG
extern int verbosity_level;
#endif

namespace cnf
{

#if 0
DdManager *ZManager = 0;
#endif

void
IntList::readXML( const XML::Node *xml_input )
{
  clear();
  for( int i = 0; i < xml_input->size(); ++i )
    push_back( atoi( xml_input->get_child(i)->get_text().c_str() ) );
}

void
Manager::freeze_clause( ClauseIndex idx )
{
  Clause &cl = clause( idx );
  //assert( (ClauseStatus)cl.status() == Original );
  if( (ClauseStatus)cl.status() == Original )
    {
      CLitPoolElement *wlit[] = { 0, 0 }, **ptr = wlit;
      for( unsigned i = 0; i < cl.num_lits(); ++i )
	{
	  CLitPoolElement &lit = cl.literal( i );
	  --variable( lit.var_index() ).lits_count( lit.var_sign() );

	  // remove watched literals from watch lists
	  if( lit.is_watched() )
	    {
	      assert( ptr - wlit < 2 );
	      *ptr++ = &lit;
	      std::vector<CLitPoolElement*> &wlits = variable( lit.var_index() ).watched( lit.var_sign() );
	      for( std::vector<CLitPoolElement*>::iterator li = wlits.begin(); li != wlits.end(); ++li )
		if( *li == &lit )
		  {
		    *li = wlits.back();
		    wlits.pop_back();
		    break;
		  }
	    }
	}
      if( wlit[0] != 0 )
	{
	  frozen_clauses_.push_back( std::make_pair( idx, std::make_pair( wlit[0], wlit[1] ) ) );
	}
      cl.set_status( (CLAUSE_STATUS)Frozen );
    }
}

void
Manager::reanimate_clause( ClauseIndex idx )
{
  Clause &cl = clause( idx );
  assert( (ClauseStatus)cl.status() == Frozen );
  if( (ClauseStatus)cl.status() == Frozen )
    {
      for( unsigned i = 0; i < cl.num_lits(); ++i )
	{
	  CLitPoolElement &lit = cl.literal( i );
	  ++variable( lit.var_index() ).lits_count( lit.var_sign() );
	}

      // restore watched literals
      std::vector<std::pair<ClauseIndex,std::pair<CLitPoolElement*,CLitPoolElement*> > >::reverse_iterator wi;
      for( wi = frozen_clauses_.rbegin(); ((*wi).first != idx) && (wi != frozen_clauses_.rend()); ++wi );
      if( wi != frozen_clauses_.rend() )
	{
	  CLitPoolElement *lit1 = (*wi).second.first;
	  std::vector<CLitPoolElement*> &wlits1 = variable( lit1->var_index() ).watched( lit1->var_sign() );
	  wlits1.push_back( lit1 );

	  CLitPoolElement *lit2 = (*wi).second.second;
	  std::vector<CLitPoolElement*> &wlits2 = variable( lit2->var_index() ).watched( lit2->var_sign() );
	  wlits2.push_back( lit2 );

	  *wi = frozen_clauses_.back();
	  frozen_clauses_.pop_back();
	}
      cl.set_status( (CLAUSE_STATUS)Original );
    }
}

VarIndex
Manager::choose_decision_variable( int mindex ) const
{
  int max_var = 0;
  int max_score = 0;
  for( VarIndex vi = 1; vi <= number_variables(); ++vi )
    {
      Variable &var = ((Manager*)this)->variable( vi );
      if( var.is_branchable() && (var.value() == UNKNOWN) )
	{
	  int n[2];
	  n[0] = n[1] = 0;
	  for( int p = 0; p < 2; ++p )
	    {
#if 0
	      for( std::vector<ClauseIndex>::const_iterator ci = var.lit_clause(p).begin(); ci != var.lit_clause(p).end(); ++ci )
		{
		  Clause &cl = ((Manager*)this)->clause( *ci );
		  if( ((ClauseStatus)cl.status() == Original) && alive( cl ) )
		    ++n[meta_index(*ci)<mindex?0:1];
		}
#endif
	    }
	  
	  int score = MIN(n[0],n[1]);
	  if( score > max_score )
	    {
	      max_var = vi;
	      max_score = score;
	    }
	}
    }
  return( max_var );
}

VarIndex
Manager::choose_decision_variable( void ) const
{
#if 1
  int max_var = 0;
  int max_score = 0;
  for( VarIndex vi = 1; vi <= number_variables(); ++vi )
    {
      Variable &var = ((Manager*)this)->variable( vi );
      if( var.is_branchable() && (var.value() == UNKNOWN) )
	{
	  int score = 0;
	  for( int phase = 0; phase < 2; ++phase )
	    {
	      std::vector<CLitPoolElement*> &watch_list = var.watched( phase );
	      for( std::vector<CLitPoolElement*>::const_iterator wi = watch_list.begin(); wi != watch_list.end(); ++wi )
		{
		  Clause &cl = ((Manager*)this)->clause( (*wi)->find_clause_index() );
		  if( (ClauseStatus)cl.status() == Original )
		    {
		      bool satisfied = false, nil = true;
		      for( int i = 0, sz = cl.num_lits(); !satisfied && (i < sz); ++i )
			{
			  CLitPoolElement &lit = cl.literal( i );
			  int value = get_var_assignment( lit.var_index() );
			  int sign = lit.var_sign();
			  if( (value != UNKNOWN) && (sign == 1 - value) )
			    satisfied = true;
			  else if( value == UNKNOWN )
			    nil = false;
			}
		      score += (!satisfied && !nil);
		    }
		}
	    }
	  if( score > max_score )
	    {
	      max_score = score;
	      max_var = vi;
	    }
	}
    }
  return( max_var );
#else
  // need to choose a relevant variable
  for( VarIndex var = 1; var <= number_variables(); ++var )
    if( is_var_branchable( var ) && (get_var_assignment( var ) == UNKNOWN) )
      return( var );
  return( 0 );
#endif
}

void
Manager::preprocess( VarList &lits )
{
  // 3. Unit clauses (from zchaff code)
  for( unsigned i = 0, sz = clauses().size(); i <  sz; ++i )
    {
      if( clause(i).status() != DELETED_CL )
	if( clause(i).num_lits() == 1 )
	  {
	    // unit clause
	    if( variable( clause(i).literal(0).var_index() ).value() == UNKNOWN )
	      queue_implication( clause(i).literal(0).s_var(), i, 0 );
	  }
    }

  std::set<int> literals;
  size_t sz = 1;
  while( literals.size() != sz )
    {
      sz = literals.size();
      if( deduce() == CONFLICT )
        {
          std::cout << "CONFLICT during preprocess." << std::endl;
  	  return;
  	}
      else
      	{
	  for( std::vector<int>::const_iterator it = _assignment_stack[0]->begin(); it != _assignment_stack[0]->end(); ++it )
  	    literals.insert( *it );
    	}
    }

  for( std::vector<int>::const_iterator it = _assignment_stack[0]->begin(); it != _assignment_stack[0]->end(); ++it )
    lits.push_back( *it );
}

dtree::Node*
Manager::make_dtree( dtree::Manager &dtree_manager, int method, const VarList *weak_vars ) const
{
  if( method == dtree::Naive )
    {
      dtree::Node *right = 0;
      for( ClauseIdx idx = 0; idx < num_clauses(); ++idx )
	{
	  dtree::Node *left = new dtree::Leaf( *this, idx );
	  if( !right )
	    right = left;
	  else
	    {
	      dtree::Node *tmp = new dtree::Internal( left, right );
	      right = tmp;
	    }
	}
      return( right );
    }
  else if( method == dtree::Hypergraph )
    {
      // count nodes and build label map
      hgraph::Node node = 0;
      hgraph::NodeList nodes;
      hgraph::LabelMap labels;

      for( ClauseIdx idx = 0; idx < num_clauses(); ++idx )
	{
	  labels[node] = (const void*)idx;
	  nodes.add_node( node );
	  ++node;
	}

      // build graph and add (hyper)edges (one per variable)
      hgraph::Graph *graph = new hgraph::Graph( nodes, labels );
      if( weak_vars != 0 ) graph->set_weighted();

      hgraph::Edge edge;
      for( VarIndex var = 1; var <= number_variables(); ++var )
	{
	  edge.clear();
	  for( hgraph::LabelMap::const_iterator li = labels.begin(); li != labels.end(); ++li )
	    if( var_in_clause( var, (ClauseIndex)(*li).second ) )
	      edge.add_node( (*li).first );

	  if( edge.size() > 0 )
	    {
	      hgraph::Edge *n_edge = new hgraph::Edge( edge );
	      if( weak_vars && !weak_vars->find( var ) ) n_edge->set_weight( 5 );
	      graph->add_edge( n_edge );
	    }
	}

      // construct decomposition tree and deallocate resources
      dtree::Node *dt = graph->make_dtree( *this );
      delete graph;
      return( dt );
    }
  return( NULL );
}

nnf::node 
Manager::make_ddnnf( nnf::Manager &nnf_manager ) const
{
  // only applicable if formula is a single clause
  //xxxx if( count_clauses() != 1 ) throw 1;
  //xxxx std::cout << "num-clauses = " << count_clauses() << std::endl;

  // locate clause
  ClauseIndex idx = -1;
  for( unsigned i = 0; i < ((Manager*)this)->clauses().size(); ++i )
    if( (ClauseStatus)((CSolver*)this)->clause(i).status() == Original )
      {
	idx = (ClauseIndex)i;
	break;
      }
  assert( idx != -1 );

  // compute literals within clause
  CLitPoolElement *clits = ((Manager*)this)->clause( idx ).literals();
  int sz = ((Manager*)this)->clause(idx).num_lits();
  int *lits = (int*)malloc( sz * sizeof(int) ), *ptr = lits;
  bool satisfied = false;
  for( int i = 0; i < sz; ++i )
    {
      int value = get_var_assignment( clits[i].var_index() );
      if( value == UNKNOWN )
	*ptr++ = clits[i].s_var();
      else if( ((Manager*)this)->literal_value( clits[i] ) )
	{
	  satisfied = true;
	  break;
	}
    }

  // base case
  if( satisfied || (ptr - lits == 0) )
    {
      free( lits );
      return( nnf::make_value( &nnf_manager, satisfied ) );
    }

  // create ddnnf
  nnf::node left, right = nnf::null_node;
  for( int i = (ptr - lits) - 1; i >= 0; --i )
    {
      // create left child
      left = nnf::null_node;
      for( int j = 0; j < i; ++j )
	{
	  int lit = ((lits[j]>>1)<<1) + (1-(lits[j]%2));
	  nnf::node tmp = nnf::make_variable( &nnf_manager, lit );
	  if( left == nnf::null_node )
	    left = tmp;
	  else
	    {
	      nnf::node tmp2 = nnf::make_and( &nnf_manager, left, tmp );
	      nnf_manager.unregister_use( left );
	      nnf_manager.unregister_use( tmp );
	      left = tmp2;
	    }
	}

      nnf::node tmp = nnf::make_variable( &nnf_manager, lits[i] );
      if( left == nnf::null_node )
	left = tmp;
      else
	{
	  nnf::node tmp2 = nnf::make_and( &nnf_manager, left, tmp );
	  nnf_manager.unregister_use( left );
	  nnf_manager.unregister_use( tmp );
	  left = tmp2;
	}

      // create right node
      if( right == nnf::null_node )
	right = left;
      else
	{
	  nnf::node tmp = nnf::make_or( &nnf_manager, left, right );
	  nnf_manager.unregister_use( left );
	  nnf_manager.unregister_use( right );
	  right = tmp;
	}
    }
  free( lits );
  assert( right != nnf::null_node );
  assert( nnf_manager.verify_integrity() );
  return( right );
}

#ifndef ZBDD
unsigned
Manager::hash( const char *blueprint, const VarList *context ) const
{
  const char *buf = (!blueprint?compute_blueprint( *context ):blueprint);

  // digest buf with md4
  unsigned *ptr, result;
  unsigned char digest[16];
  MD4_CTX md4_context;

  // compute MD4 digest
  MD4Init( &md4_context );
  MD4Update( &md4_context, (unsigned char*)buf, strlen( buf ) );
  MD4Final( digest, &md4_context );

  // compact digest into unsigned (assumes sizeof(unsigned) = 4)
  ptr = (unsigned*)digest;
  result = (ptr[0] ^ ptr[1] ^ ptr[2] ^ ptr[3]);
  return( result );
}

const char*
Manager::compute_blueprint( const VarList &context ) const
{
  static char blueprint[16535000];

#if 0
  static VarList cvars;
  static std::set<VarIndex> vars;
  vars.clear();
  for( unsigned i = 0; i < ((Manager*)this)->clauses().size(); ++i )
    {
      Clause &cl = ((Manager*)this)->clause( i );
      if( (ClauseStatus)cl.status() == Original )
	{
	  cvars.clear();
	  bool satisfied = false;
	  for( unsigned j = 0; !satisfied && (j < cl.num_lits()); ++j )
	    {
	      CLitPoolElement &lit = cl.literal( j );
	      VarIndex var = lit.var_index();
	      int value = get_var_assignment( var );
	      int sign = lit.var_sign();
	      if( value != UNKNOWN )
		{
		  cvars.push_back( var );
		  if( sign == 1 - value ) satisfied = true;
		}
	    }
	  if( !satisfied )
	    {
	      key << i << ',';
	      vars.insert( cvars.begin(), cvars.end() );
	    }
	}
    }

  // attach suffix
  for( cnf::VarList::const_iterator vi = context.begin(); vi != context.end(); ++vi )
    {
      int value = get_var_assignment( *vi );
      if( value != UNKNOWN ) key << ':' << *vi;
    }
#endif

#if 0
  static char clause[1653500];
  static std::set<std::string> clauses;
  clauses.clear();
  for( unsigned i = 0; i < ((Manager*)this)->clauses().size(); ++i )
    {
      Clause &cl = ((Manager*)this)->clause( i );
      if( (ClauseStatus)cl.status() == Original )
	{
	  char *ptr = clause;
	  bool satisfied = false;
	  for( unsigned j = 0; !satisfied && (j < cl.num_lits()); ++j )
	    {
	      CLitPoolElement &lit = cl.literal( j );
	      VarIndex var = lit.var_index();
	      int value = get_var_assignment( var );
	      int sign = lit.var_sign();
	      if( value != UNKNOWN )
		{
		  if( sign == 1 - value ) satisfied = true;
		}
	      else
		{
		  *ptr++ = (sign?'-':'+');
		  for( int v = var; v > 0; v = v >> 1 ) *ptr++ = '0'+(v%2);
		  *ptr++ = ',';
		}
	    }
	  *ptr = 0;

	  if( !satisfied )
	    {
	      std::string c = clause;
	      clauses.insert( c );
	    }
	}
    }

  // generate blueprint
  char *ptr = &blueprint[0];
  for( std::set<std::string>::const_iterator ci = clauses.begin(); ci != clauses.end(); ++ci )
    {
      for( const char *s = (*ci).c_str(); *s != 0; *ptr++ = *s++ );
      *ptr++ = ':';
    }
  *ptr = 0;
#endif

#if 1
  char *ptr = &blueprint[0];
  for( unsigned i = 0, sz = ((Manager*)this)->clauses().size(); i < sz; ++i )
    {
      Clause &cl = ((Manager*)this)->clause( i );
      if( (ClauseStatus)cl.status() == Original )
	{
	  bool satisfied = false;
	  for( unsigned j = 0; !satisfied && (j < cl.num_lits()); ++j )
	    {
	      CLitPoolElement &lit = cl.literal( j );
	      VarIndex var = lit.var_index();
	      int value = get_var_assignment( var );
	      int sign = lit.var_sign();
	      if( value != UNKNOWN )
		{
		  if( sign == 1 - value ) satisfied = true;
		}
	    }
	  *ptr++ = '0'+(char)satisfied;
	}
    }
  *ptr++ = ':';

  for( VarList::const_iterator vi = context.begin(); vi != context.end(); ++vi )
  //for( VarIndex var = 1; var <= number_variables(); ++var )
    {
      int value = get_var_assignment( *vi );
      if( value != UNKNOWN )
	*ptr++ = '0';
      else
	*ptr++ = '1';
    }
  *ptr = 0;
#endif

  return( blueprint );
}
#else

unsigned
Manager::hash( void ) const
{
  if( ZManager == 0 )
    {
      ZManager = Cudd_Init( 0, 2*number_variables(), CUDD_UNIQUE_SLOTS, 10*CUDD_CACHE_SLOTS, 0 );
    }

  DdNode *zclauses = DD_ZERO(ZManager);
  Cudd_Ref(zclauses);
  for( unsigned i = 0; i < ((Manager*)this)->clauses().size(); ++i )
    {
      Clause &cl = ((Manager*)this)->clause( i );
      if( (ClauseStatus)cl.status() == Original )
	{
	  DdNode *zclause = DD_ONE(ZManager);
	  Cudd_Ref(zclause);
	  bool satisfied = false;
	  for( unsigned j = 0; !satisfied && (j < cl.num_lits()); ++j )
	    {
	      CLitPoolElement &lit = cl.literal( j );
	      VarIndex var = lit.var_index();
	      int value = get_var_assignment( var );
	      int sign = lit.var_sign();
	      if( value != UNKNOWN )
		{
		  if( sign == 1 - value ) satisfied = true;
		}
	      else
		{
		  DdNode *ztmp = Cudd_zddChange( ZManager, zclause, (sign?((var-1)<<1)+1:((var-1)<<1)) );
		  Cudd_Ref(ztmp);
		  Cudd_RecursiveDerefZdd( ZManager, zclause );
		  zclause = ztmp;
		}
	    }
	  if( !satisfied )
	    {
	      DdNode *ztmp = Cudd_CSunion( ZManager, zclauses, zclause );
	      Cudd_Ref(ztmp);
	      Cudd_RecursiveDerefZdd( ZManager, zclauses );
	      Cudd_RecursiveDerefZdd( ZManager, zclause );
	      zclauses = ztmp;
	    }
	}
    }
#ifdef DEBUG
  if( verbosity_level >= 100 )
    {
      Cudd_CSprint( ZManager, zclauses );
      std::cout << std::endl;
    }
#endif
  return( (unsigned)zclauses );
}
#endif

bool
Manager::satisfied_or_trivial( ClauseIndex clause ) const
{
  Clause &cl = ((Manager*)this)->clause( clause );
  for( size_t l = 0; l < cl.num_lits(); ++l )
    {
      int var = cl.literal(l).var_index();
      int sign = cl.literal(l).var_sign();
      int value = get_var_assignment( var );
      if( (value != UNKNOWN) && (sign == 1 - value) )
	return( true );
      for( size_t m = l+1; m < cl.num_lits(); ++m )
	if( cl.literal(l).s_var() == (cl.literal(m).s_var() ^ 1) )
	  return( true );
    }
  return( false );
}

void
Manager::dump( std::ostream &os ) const
{
  // count clauses
  size_t num = 0;
  for( VarIndex var = 1; var <= number_variables(); ++var )
    {
      int value = get_var_assignment( var );
      num += (value != UNKNOWN);
    }
  for( size_t i = 0; i < ((Manager*)this)->clauses().size(); ++i )
    num += (int)(!satisfied_or_trivial(i));

  // dump clauses
  os << "p cnf " << number_variables() << " " << num << std::endl;
  for( VarIndex var = 1; var <= number_variables(); ++var )
    {
      int value = get_var_assignment( var );
      if( value != UNKNOWN ) os << (value?"-":"") << var << " 0" << std::endl;
    }
  for( size_t i = 0; i < ((Manager*)this)->clauses().size(); ++i )
    if( !satisfied_or_trivial( i ) )
      {
	std::ostringstream clause;
	Clause &cl = ((Manager*)this)->clause( i );
	for( size_t l = 0; l < cl.num_lits(); ++l )
	  {
	    int var = cl.literal(l).var_index();
	    int sign = cl.literal(l).var_sign();
	    int value = get_var_assignment( var );
	    if( value == UNKNOWN )
	      clause << (sign?"-":"") << var << " ";
	  }
	os << clause.str() << "0" << std::endl;
      }
  os << "%" << std::endl;
}

void
Manager::printXML( std::ostream &os, int indent ) const
{
  for( int i = 0; i < indent; ++i ) os << ' ';
  os << "<cnf>" << std::endl;
  for( unsigned ci = 0; ci < ((Manager*)this)->clauses().size(); ++ci )
    {
      Clause &cl = ((Manager*)this)->clause( ci );
      if( (ClauseStatus)cl.status() == Original )
	{
	  std::ostringstream clause;
	  bool sat = false;
	  unsigned lit;
	  for( lit = 0; lit < cl.num_lits(); ++lit )
	    {
	      int var = cl.literal(lit).var_index();
	      int sign = cl.literal(lit).var_sign();
	      int value = get_var_assignment( var );
	      if( value == UNKNOWN )
		{
		  clause << "<variable>" << (sign?"~v":"v") << var << "</variable>";
		}
	      else if( sign == 1 - value )
		{
		  sat = true;
		  break;
		}
	    }

	  if( !sat )
	    {
	      for( int i = 0; i < indent+2; ++i ) os << ' ';
	      os << "<clause>"
		 << "<index>" << ci << "</index>"
		 << "<meta-index>" << meta_index_[ci] << "</meta-index>"
		 << clause.str()
		 << "</clause>"
		 << std::endl;
	    }
	}
    }
  for( int i = 0; i < indent; ++i ) os << ' ';
  os << "</cnf>" << std::endl;
}

void
Manager::print_cls( std::ostream &os ) const
{
  for( unsigned i = 0; i < ((Manager*)this)->clauses().size(); ++i )
    {
      Clause &cl = ((Manager*)this)->clause( i );
      if( (ClauseStatus)cl.status() == Deleted )
	continue;
      else if( (ClauseStatus)cl.status() == Original )
	os << "O ";
      else if( (ClauseStatus)cl.status() == Frozen )
	os << "F ";
      else
	{
	  assert( (ClauseStatus)cl.status() == Conflict );
	  os <<  "A ";
	}

      for( int j = 1; j < 33; ++j ) 
	os << (cl.gid(j)?1:0);
      os << "\t";
      for( unsigned j = 0; j < cl.num_lits(); ++j )
	os << (cl.literal(j).var_sign()?'-':'+') << cl.literal(j).var_index() << ' ';
      os << '0' <<  std::endl;
    }
}

void
print_var_assignment( std::ostream *os, cnf::Manager *cnf_manager )
{
  (*os) << "[ ";
  for( VarIndex var = 1; var <= cnf_manager->number_variables(); ++var )
    {
      int value = cnf_manager->get_var_assignment( var );
      if( value == 0 )
	(*os) << "-v" << var << ' ';
      else if( value == 1 )
	(*os) << "+v" << var << ' ';
    }
  (*os) << ']' << std::endl;
}

void
increase_model_count( double *num_models, cnf::Manager *cnf_manager )
{
  int inst_vars = 0;
  for( VarIndex var = 1; var <= cnf_manager->number_variables(); ++var )
    {
      int value = cnf_manager->get_var_assignment( var );
      inst_vars += (value != UNKNOWN);
    }
  *num_models += pow( (double)2.0, (double)(cnf_manager->number_variables() - inst_vars) );
}

void
plain_increase_model_count( double *num_models, cnf::Manager *cnf_manager )
{
  ++(*num_models);
}

}; // cnf namespace
