#include <iostream>
#include "cf2cs-ocaml.hpp"
#include "planner.h"
#include "clauses.hpp"
#include "parser-global.hpp"
#include "main.hpp"

extern "C" {
#include <caml/mlvalues.h>
#include <caml/callback.h>
#include <caml/alloc.h>
#include <caml/memory.h>
}

void                   registerEntry( char* );
int                    registerExit( void );

using namespace std;

/*******************************************************************************
 **
 **   Ocaml Section
 **
 ******************************************************************************/

void 
init_problem(int n)
{
  CAMLparam0();
  static value * init_problem_closure = NULL;
  if (init_problem_closure == NULL) 
    init_problem_closure = caml_named_value("init_problem");
  caml_callback(*init_problem_closure, Val_int(n));
  CAMLreturn0;
}

void 
fill_atom(int n, string s)
{
  CAMLparam0();
  CAMLlocal1(x);
  static value * fill_atom_closure = NULL;
  if (fill_atom_closure == NULL) 
    fill_atom_closure = caml_named_value("fill_atom");
  x = caml_copy_string(s.c_str());
  caml_callback2(*fill_atom_closure, Val_int(n), x);
  CAMLreturn0;
}

value
set_to_array( const set<int>& s )
{
  CAMLparam0();
  CAMLlocal1(array);
  array = caml_alloc_tuple(s.size());
  size_t i = 0;
  FOR(set<int>::const_iterator, it, s)
    Store_field(array, i++, Val_int(*it));
  CAMLreturn(array);
}

void 
create_cond_effect(const When& w)
{
  CAMLparam0();
  CAMLlocal3(tuple_prec, tuple_eff, comm);
  static value * create_cond_effect_closure = NULL;
  if (create_cond_effect_closure == NULL) 
    create_cond_effect_closure = caml_named_value("create_cond_effect");
  tuple_prec = set_to_array( w.prec );
  tuple_eff = set_to_array( w.eff );
  comm = caml_copy_string(w.comment.c_str());
  caml_callback3(*create_cond_effect_closure, 
		 comm,
		 tuple_prec,
		 tuple_eff);
  CAMLreturn0;
}

void 
create_action(const Act& a)
{
  CAMLparam0();
  CAMLlocal3(tuple_prec, tuple_eff, comm);
  static value * create_action_closure = NULL;
  if (create_action_closure == NULL) 
    create_action_closure = caml_named_value("create_action");

  // Cond effects
  FOR( Conds::const_iterator, w, a.conds )
    create_cond_effect(*w);
  
  // Unconditional
  tuple_prec = set_to_array( a.prec );
  tuple_eff = set_to_array( a.eff );
  comm = caml_copy_string(a.name.c_str());

  // Create action
  caml_callback3(*create_action_closure, 
		 comm,
		 tuple_prec,
		 tuple_eff);

  CAMLreturn0;
}

void
load_goal()
{
  CAMLparam0();
  CAMLlocal1(c);

  static value * add_goal_lit_closure = NULL;
  if (add_goal_lit_closure == NULL) 
    add_goal_lit_closure = caml_named_value("add_goal_lit");

  FOR( set<int>::iterator, g, goal_literals )
    caml_callback(*add_goal_lit_closure, Val_int(*g) );

  static value * add_goal_clause_closure = NULL;
  if (add_goal_clause_closure == NULL) 
    add_goal_clause_closure = caml_named_value("add_goal_clause");

  FOR( vector<set<int> >::iterator, it, g_or )
    {
      c = set_to_array( *it );
      caml_callback(*add_goal_clause_closure, c );
    }
  CAMLreturn0;
}

void
load_init()
{
  CAMLparam0();
  CAMLlocal1(c);

  static value * add_init_lit_closure = NULL;
  if (add_init_lit_closure == NULL) 
    add_init_lit_closure = caml_named_value("add_init_lit");

  FOR( set<int>::iterator, l, true_literals )
    caml_callback(*add_init_lit_closure, Val_int(*l) );

  static value * add_init_clause_closure = NULL;
  if (add_init_clause_closure == NULL) 
    add_init_clause_closure = caml_named_value("add_init_clause");

  FOR( vector<set<int> >::iterator, it, disj )
    {
      c = set_to_array( *it );
      caml_callback(*add_init_clause_closure, c );
    }
  CAMLreturn0;
}

void 
create_problem(char* domain, char* instance)
{
  CAMLparam0();
  CAMLlocal2(d,i);
  static value * create_problem_closure = NULL;
  if (create_problem_closure == NULL) 
    create_problem_closure = caml_named_value("create_problem");
  d = caml_copy_string(domain);
  i = caml_copy_string(instance);
  caml_callback2(*create_problem_closure, d, i);
  CAMLreturn0;
}

void 
do_t0()
{
  CAMLparam0();
  static value * do_t0_closure = NULL;
  if (do_t0_closure == NULL) 
    do_t0_closure = caml_named_value("do_t0");
  caml_callback(*do_t0_closure, Val_unit);
  CAMLreturn0;
}

/*******************************************************************************
 **
 **   Implementing functions for main.cc
 **
 ******************************************************************************/

void 
usage( std::ostream &os )
{
  cout << "usage: <pddl domain> <pddl problem>" << endl;
  char argv1[] = "cf2cs";
  char **argv = (char**)calloc(2,sizeof(char*));
  argv[0] = argv1;
  int argc = 1;
  parseArg( argc, argv );
  do_t0();
  exit(1);
}

void
parseArg( int argc, char **argv )
{
  cout << "parsing arguments to caml " << endl;
  vector<string> args;
  char **argv_orig = argv;
  for( ++argv, --argc ; argc != 0; --argc, ++argv )
    args.push_back(string(*argv));

  size_t argn = 0;

  for( argn = 0; argn < args.size(); ++argn )
    {
      string& arg = args[argn];
      if( arg == "-v" )
	verbose = atoi( args[++argn].c_str() );
    }

  argc = 1;
  caml_startup(argv_orig);
}

void 
problem_to_ocaml()
{
  // Load atoms
  size_t realsize = 0;
  FOR_0( i, atoms )
    if( atoms[i] == "" )
      break;
    else
      realsize++;
  init_problem(realsize);
  for( size_t i = 0; i < realsize; ++i )
    fill_atom(i, atoms[i]);

  // Load actions
  FOR_1( i, actions )
    create_action(actions[i]);

  // Load init and goal
  load_init();
  load_goal();

  create_problem(_low_domainName, _low_problemName);
}


void
do_cf2cs()
{
  registerEntry( "do_cf2cs()" );
  cout << "==============================" << endl
       << "Going into OCaml code...\n" << endl; 
  problem_to_ocaml();
  do_t0();
  registerExit();
}
void 
prepare_action( Act& a )
{
  //cout << "Loaded action" << a.name << endl; 
}
