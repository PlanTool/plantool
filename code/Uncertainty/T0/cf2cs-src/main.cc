/*******************************************************************************
 **
 **   CF2CS: A compiler from conformant planning problems into classical planning problems
 **          Sound, but incomplete...
 **
 ******************************************************************************/

#include <stdio.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <fcntl.h>
#include <fstream>

#include "nnf.h"
#include "cnf.h"

#include "cf2cs-ocaml.hpp"
#include "parser.h"
#include "planner.h"
#include "clauses.hpp"
#include "parser-global.hpp"
#include "main.hpp"

/*******************************************************************************
 **
 **   Macros
 **
 ******************************************************************************/

#define   HASHSIZE       27449
#define   MILLION        (float)1000000

/*******************************************************************************
 **
 **   Global Data
 **
 ******************************************************************************/

int             numberAtoms =  0;
int             verbose = 0, verbosity_level = 0;
int             numberReachableAtoms =  0, numberIMapAtoms = 0;
unsigned        time_shift = 0;
bool observation_detected = false;
bool use_oneof_or = false;
bool use_cnf_complex_goal = true;

std::set<int> reachable_atoms;
std::set<int> negative_precs;
std::set<int> modified_atoms;
std::set<int> static_atoms;
std::set<int> static_positive_atoms;
std::map<int,int> reachable, inv_reachable;
std::vector<grounded_schema_t> grounded_schema;

//  name of the atoms 
vector<string> atoms;
size_t natoms = 0;
// from reachable atoms to normal atoms 
vector<int> r2a;

vector<Act> actions;
void *dir_actions = 0;
// ground2action[grounded] -> index at actions
vector<int> ground2action;
vector<Act> observations;

set<int> all_atoms;
set<int> known;
set<int> goal_literals;
set<int> precs_literals;

// Sets used inside t0c.cc
set<int> unknown; // lits original mentioned in oneof, or at Init 

vector<set<int> > disj_orig; // Or at Init
vector<set<int> > disj; // Or + Oneof + exclusion, after unit resolution
vector<set<int> > oneof; // Real oneof (with trivial p || -p)

vector<set<int> > g_or;

set<int> simple_unknown; // Unknown not in disj, so of the form p || -p
set<int> true_literals; // Literals true at Init, after unit resolution
set<int> lit_unknown; // Literals at disj + oneof
set<int> disj_orig_lits; // Literals at disj_orig
set<int> oneof_lits; // Literals at oneof
// End of sets used inside t0c.cc


const string dummy_pred("FOO");
int dummy_pred_n = 0;


/*******************************************************************************
 **
 **   Static Data
 **
 ******************************************************************************/

static char*           problemFile;
static char*           domainFile;

// atom hash table
static iatom_t *       atomHashTable[ATOMHASHSIZE];
static iatom_t *       atomHashPool = NULL;
static int             atomHashClaimSize = 0;

// procedure registration stack
static procRegister_t* procStackTop = NULL;

/*******************************************************************************
 **
 **   Forward References
 **
 ******************************************************************************/

void                   _fatal( int, char*, char*, int );
std::ostream&          printOperator( std::ostream&, operator_t* );
std::ostream&          operator<<( std::ostream&, operator_t& );
void                   registerEntry( char* );
int                    registerExit( void );
char*                  operatorName( int*, int );
char*                  readAtomName( int );
iatom_t*               readAtomByNumber( int );

/*******************************************************************************
 **
 **   Atoms/Operators Names
 **
 ******************************************************************************/

char *
operatorName( int *parameters, int schema )
{
  int *ip;
  static char name[1024];

  name[0] = '(';
  name[1] = '\0';
  strcat( name, _low_schemaName[schema] );
  for( ip = &parameters[1]; *ip != -1; ++ip )
    {
      strcat( name, " " );
      strcat( name, _low_objectName[*ip - 1] );
    }
  strcat( name, ")" );
  return( name );
}

char *
operatorName2( int *parameters, int schema )
{
  int *ip;
  static char name[1024];

  name[0] = '\0';
  strcat( name, _low_schemaName[schema] );
  for( ip = &parameters[1]; *ip != -1; ++ip )
    {
      strcat( name, " " );
      strcat( name, _low_objectName[*ip - 1] );
    }
  return( name );
}

void
printOperatorGrounded( int *parameters, int schema )
{
  int *ip;
  static char name[128];

  name[0] = '(';
  name[1] = '\0';
  strcat( name, _low_schemaName[schema] );
  for( ip = &parameters[1]; *ip != -1; ++ip )
    {
      strcat( name, " " );
      strcat( name, _low_objectName[*ip - 1] );
    }
  strcat( name, ")" );
  printf( "%s\n", name );
}

char *
atomName( int *parameters )
{
  int *ip;
  static char name[1024];

  name[0] = '(';
  name[1] = '\0';
  strcat( name, _low_predicateName[parameters[0] - 1] );
  for( ip = &parameters[1]; *ip != -1; ++ip )
    {
      strcat( name, " " );
      strcat( name, _low_objectName[*ip - 1] );
    }
  strcat( name, ")" );
  return( name );
}

// A cache can be done
string name_l_old( int lit )
{
  assert( lit != 0 );
  if(lit>0)
    return "(" + atoms[lit] + ")";
  else
    return "(not (" + atoms[-lit] + "))";
}

void
put_spaces( char *s )
{
  char *c = strchr( s, ' ');
  while( c != NULL )
    {
      *c = '_';
      c = strchr( s, ' ');
    }
}

string
set_atoms2str( const set<int>& s )
{
  string res = "(";
  FOR( set<int>::const_iterator, l, s)
    res += atoms[abs(*l)] + "(" + int2str(*l) + "), ";
  return res + ")";
}


/*******************************************************************************
 **
 **   Atom Hash
 **
 ******************************************************************************/

unsigned 
atomHashFunction( int *parameters )
{
  int *ip;
  unsigned hash = 5381;
  for( ip = parameters; *ip != -1; ++ip )
    hash = ((hash << 5) + hash) + *ip;
  return( hash % ATOMHASHSIZE );
}


iatom_t *
insertIntoAtomHash( int *parameters )
{
  unsigned index;
  iatom_t *iatom;
  static int currentAtom = 1;

  index = atomHashFunction( parameters );
  if( currentAtom >= atomHashClaimSize )
    {
      atomHashClaimSize = (int)(atomHashClaimSize==0?1024:INCRATE*atomHashClaimSize);
      atomHashPool = (iatom_t*)malloc( atomHashClaimSize * sizeof( iatom_t ) );
      if( !atomHashPool )
	fatal( noMoreMemory );
      currentAtom = 0;
    }
  iatom = &atomHashPool[currentAtom++];
  memcpy( iatom->parameters, parameters, MAXPARAMETERS * sizeof( int ) );
  strcpy( iatom->name, atomName( parameters ) );
  iatom->idx = ++numberAtoms;
  iatom->next = atomHashTable[index];
  atomHashTable[index] = iatom;

  if( verbose > LEVEL4 )
    std::cout << "new atom = " << iatom->name << ", idx = " << iatom->idx << std::endl;

  return( iatom );
}

//#define __STATS_HASH
#ifdef __STATS_HASH
size_t max_rec = 0;
size_t n_rec = 0;
double sum_rec = 0;
#endif

iatom_t*
readAtomHash( int *parameters )
{
  iatom_t *iatom;
  int *ip, *jp;
#ifdef __STATS_HASH
  size_t rec = 0;
#endif
  for( iatom = atomHashTable[atomHashFunction( parameters )]; iatom != NULL; iatom = iatom->next )
    {
#ifdef __STATS_HASH
      rec++;
#endif
      for( ip = parameters, jp = iatom->parameters; (*ip != -1) && (*ip == *jp); ++ip, ++jp);
      if( (*ip == -1) && (*jp == -1) )
	{
#ifdef __STATS_HASH
	  max_rec = max(rec, max_rec);
	  sum_rec += rec;
	  n_rec++;
#endif
	  return( iatom );
	}
    }

  // if not present, insert it right now!
  return( insertIntoAtomHash( parameters ) );
}

iatom_t*
readAtomByNumber( int index )
{
  int i;
  iatom_t *iatom;
  for( i = 0; i < ATOMHASHSIZE; ++i )
    {
      for( iatom = atomHashTable[i]; (iatom != NULL) && (iatom->idx != index); iatom = iatom->next );
      if( iatom != NULL )
	break;
    }
  assert( i != ATOMHASHSIZE );
  return( iatom );
}

char*
readAtomName( int index )
{
  return( atomName( readAtomByNumber( index )->parameters ) );
}

void
printAtomHash( std::ostream &os )
{
  for( int i = 0; i < ATOMHASHSIZE; ++i )
    for( iatom_t *iatom = atomHashTable[i]; iatom != NULL; iatom = iatom->next )
      os << "ATOM-HASH: v" << iatom->idx << " = " << iatom->name << std::endl;
}

string
int2str( int i )
{
  ostringstream str_l;
  str_l << i;
  return str_l.str();
}

string
set2str( const set<int>& s )
{
  string res = "(";
  FOR( set<int>::const_iterator, l, s)
    res += int2str(*l) + ",";
  return res + ")";
}

// this way to create atoms can have problem with original names including "_"
void
strip_space( char *s )
{
  char *c = strchr( s, ' ');
  while( c != NULL )
    {
      *c = '_';
      c = strchr( s, ' ');
    }
}

/*******************************************************************************
 **
 **   Reachable Atoms && Operator Instantiation
 **
 ******************************************************************************/

class op
{
public:
  std::set<int> prec, add, del;
  bool assigned;
  op(): assigned(false)
  {}
};


op o;

size_t depth = 0;


// ip = array of vars for schema, terminate with *ip = 0
void
buildParameterList( int *parameters, int schema, int *ip, void (*insertOp)(int*,int) )
{
  if( verbose > LEVEL5 )
    cout << "depth = " << depth++ << " ip = " << ip << " *ip= " << *ip << endl;
  int *vp, *jp;
  if( *ip != 0 )
    {
      for( vp = _low_values[_low_numberSchema * (*ip - 1) + schema]; *vp != 0; ++vp )
	{
	  if( !(_low_requirements & REQ_EQUALITY) )
	    {
	      for( jp = &parameters[1]; jp < &parameters[MAXPARAMETERS]; ++jp )
		if( (*jp > 0) && (*jp == *vp) )
		  break;
	    }
	  else
	    jp = &parameters[MAXPARAMETERS];

	  if( jp == &parameters[MAXPARAMETERS] )
	    {
	      parameters[*ip] = *vp;
	      
	      std::set<int>::const_iterator ai;
	      o.prec.clear(); o.add.clear(); o.del.clear();
	      set<int> neg_precs;
	      if( operatorAtoms( _low_schemaTable[schema], parameters, o.prec, o.add, o.del, 
				 &reachable_atoms, &neg_precs ) )
		{
		  for( ai = o.prec.begin(); ai != o.prec.end(); ++ai )
		    {
		      if( reachable_atoms.find( *ai ) == reachable_atoms.end() ) break;
		    }
		  if( ai == o.prec.end() )
		    {
		      negative_precs.insert( neg_precs.begin(), neg_precs.end() );
		      buildParameterList( parameters, schema, ip + 1, insertOp );
		    }
		}
	    }
	}
      parameters[*ip] = 0;
    }
  else
    {
      // insert delimiting marker
      for( jp = parameters; *jp > 0; ++jp );
      *jp = -1;

      // insert operator
      (*insertOp)( parameters, schema );
    }
  depth--;
}

void
instantiateOperators( void (*insertOp)(int*,int) )
{
  static int parameters[MAXPARAMETERS];
  for( int schema = 0; schema < _low_numberSchema; ++schema )
    {
      if( _low_numberSchema < 20 ) // OJO: magic number...
	cout << "schema " << schema << " de " << _low_numberSchema <<  endl;
      //memset( parameters, 0, MAXPARAMETERS * sizeof( int ) );
      //for( size_t z = 0; z < MAXPARAMETERS; z++ )
      //parameters[z] = 0;
      parameters[0] = schema + 1;
      buildParameterList( parameters, schema, _low_vars[schema], insertOp );
    }
}

set<pair<vector<int>, int> > grounded;

void
insertGroundedOperator( int *parameters, int schema )
{
  set<int> neg_precs;
  o.prec.clear(); o.add.clear(); o.del.clear();
  set<int> conds;
  //if( operatorAtoms( _low_schemaTable[schema], parameters, o.prec, o.add, o.del, &neg_precs ) )
  if( operatorAtoms( _low_schemaTable[schema], parameters, o.prec, o.add, o.del, 
		     &reachable_atoms, &neg_precs, &conds ) )
    {
      std::set<int>::const_iterator ai;
      for( ai = o.prec.begin(); ai != o.prec.end(); ++ai )
	if( reachable_atoms.find( *ai ) == reachable_atoms.end() ) break;
      if( ai == o.prec.end() )
	{
	  negative_precs.insert( neg_precs.begin(), neg_precs.end() );
	  
	  vector<int> p;
	  for( int *jp = parameters; *jp != -1; ++jp )
	    p.push_back(*jp);
	  if( grounded.count(make_pair(p, schema)) == 0 )
	    {
	      grounded_schema_t op;
	      op.schema_ = schema;
	      op.parameters_ = (int*)malloc( MAXPARAMETERS * sizeof(int) );
	      memcpy( op.parameters_, parameters, MAXPARAMETERS * sizeof(int) );
	      grounded_schema.push_back( op );
	      grounded.insert( make_pair(p, schema) );
	      if( verbose > LEVEL5 )
		{
		  cout << "Grounded: "; 
		  printOperatorGrounded( op.parameters_, op.schema_ );
		}
	    }
	  
	  reachable_atoms.insert( o.add.begin(), o.add.end() );  
	  modified_atoms.insert(  o.add.begin(), o.add.end() );  
	  modified_atoms.insert(  o.del.begin(), o.del.end() );  
	  if( true )
	    // Doesn't make sense to said that a condition was reached
	    FOR( set<int>::iterator, c, conds )
	      if( *c > 0 )
		reachable_atoms.insert( *c );
	      else
		reachable_atoms.insert( -*c );
	}
    }
}

const bool debug_breach = false;

void
backReachable( set<int>& breachable_atoms )
{
  registerEntry( "backReachable()" );

  std::set<int>::const_iterator ai;
  std::vector<When>::const_iterator wi;
  breachable_atoms.clear();
  formulaAtoms( goalSituation, NULL, breachable_atoms );
  
  if(debug_breach)
    cout << "#Breachable = " << breachable_atoms.size() << endl;
  size_t osize = 1 + breachable_atoms.size();
  while( osize != breachable_atoms.size() )
    {
      osize = breachable_atoms.size();
      if(debug_breach)
	cout << "osize = " << osize << endl;
      for( size_t i = 0; i < grounded_schema.size(); ++i )
	{
	  grounded_schema_t gs = grounded_schema[i];
	  Act a;
	  
	  bool relevant = false;
	  if(debug_breach)
	    {
	      cout << "considering ";
	      printOperatorGrounded( gs.parameters_, gs.schema_ );
	    }
	  if( !operatorAtomsWhen( _low_schemaTable[gs.schema_], gs.parameters_, a, NULL, 
				  reachable_atoms, NULL ) )
	    continue;

	  if(debug_breach)
	    cout << "Eff: " << set2str(a.eff) << endl;
	  for( ai = a.eff.begin(); !relevant && ai != a.eff.end(); ++ai )
	    if( breachable_atoms.count(abs(*ai)) > 0 )
	      {
		relevant = true;
		break;
	      }

	  for( wi = a.conds.begin(); !relevant && wi != a.conds.end(); ++wi )
	    {
	      if(debug_breach)
		cout << "Cond: " << set2str(wi->eff) << endl;
	      for( ai = wi->eff.begin(); ai != wi->eff.end(); ++ai )
		if( breachable_atoms.count(abs(*ai)) > 0 )
		  {
		    relevant = true;
		    break;
		  }
	    }

	  // relevant iff a modify some active atom

	  if( !relevant ) continue;
	  if(debug_breach)
	    cout << "is relevant" << endl;
	  for( ai = a.prec.begin(); ai != a.prec.end(); ++ai )
	    breachable_atoms.insert( abs(*ai ) );
	  for( wi = a.conds.begin(); wi != a.conds.end(); ++wi )
	    for( ai = wi->prec.begin(); ai != wi->prec.end(); ++ai )
	      breachable_atoms.insert( abs(*ai) );
	}
      if(debug_breach)
	cout << "end osize = " << osize << endl;
    }
  registerExit();
}

void
print_reachable()
{
  std::cout << "reachable-atoms: begin" << std::endl;
  FOR( set<int>::const_iterator, ai, reachable_atoms )
    std::cout << "atom " << *ai << " = " << readAtomByNumber(*ai)->name << std::endl;
  std::cout << "reachable-atoms: end" << std::endl;
}

void
computeReachableAtoms( ) 
{
  const bool debug_reach = false;

  std::set<int> prec, add, del;

  registerEntry( "computeReachableAtoms()" );

  // set initial set of atoms (positive atoms from init formula)
  //formulaAtomsPos( initialSituation, NULL, 0, reachable_atoms );
  formulaAtoms( initialSituation, NULL, reachable_atoms );
  if( debug_reach )
    print_reachable();
  size_t osize = 1 + reachable_atoms.size();
  while( osize != reachable_atoms.size() )
    {
      osize = reachable_atoms.size();
      instantiateOperators( &insertGroundedOperator );
      if( debug_reach )
	print_reachable();
    }

  std::set<int> tmp;
  set_intersection( negative_precs.begin(), negative_precs.end(),
		    modified_atoms.begin(), modified_atoms.end(), 
		    inserter(tmp, tmp.begin()));
  if( tmp.size() > 0 )
    {
      cerr << "FATAL ERROR: Non-static negative preconditions not supported" << endl;
      exit(1);
    }

  // Refine reachable atoms by going backward from goal
  // set initial set of atoms (positive atoms from init formula)
  if(0) // OJO: Da error en adder p01, porque borra un atomo del estado inial
    {
      set<int> breachable_atoms;
      backReachable( breachable_atoms );
      if(debug_breach)
	cout << "#Reachable = " << reachable_atoms.size() << endl
	     << "#Breachable = " << breachable_atoms.size() << endl;
      set<int> tmp_reach;
      swap( reachable_atoms, tmp_reach );
      FOR( set<int>::iterator, e, tmp_reach )
	if( breachable_atoms.count(*e) > 0 )
	  reachable_atoms.insert(*e);
	else
	  if( verbose > LEVEL2 )
	    cout << "Ignoring atom " << *e 
		 << " because backward reachability" << endl;
      if(debug_breach)
	cout << "Reachable atoms after backward reachability = " << reachable_atoms.size() << endl;
    }

  // Maybe is needed here...
  //collect_init_uncertainty();
  // create static atoms
  //tmp.clear();
  static_atoms.clear();
  set_difference( reachable_atoms.begin(), reachable_atoms.end(),
		  modified_atoms.begin(), modified_atoms.end(), 
		  inserter(static_atoms, static_atoms.begin()));
  std::set<int> tmp2;
  collectPlainFormulaPosAtoms( initialSituation, tmp2 );
  set_intersection( static_atoms.begin(), static_atoms.end(),
		    tmp2.begin(), tmp2.end(), 
		    inserter(static_positive_atoms, static_positive_atoms.begin()));
  if( verbose > LEVEL2 )
    {
      cout << "Static positive atoms: " << static_positive_atoms.size() << ": ";
      FOR( std::set<int>::iterator, it, static_positive_atoms )
	cout << *it << " ";
      cout << endl;
      if( verbose > LEVEL4 )
	{
	  cout << "Static atoms: " << static_atoms.size() << ": ";
	  FOR( std::set<int>::iterator, it, static_atoms )
	    cout << *it << " ";
	  cout << endl;
	}
    }
  // create reachable-atoms map
  numberReachableAtoms = numberIMapAtoms;
  FOR( set<int>::const_iterator, ai, reachable_atoms )
    {
      reachable[*ai] = ++numberReachableAtoms;
      inv_reachable[numberReachableAtoms] = *ai;
    }

  // output
  if( verbose > LEVEL2 )
    print_reachable();
  registerExit();
}



/*******************************************************************************
 **
 **   Compressed Representation
 **
 ******************************************************************************/

/*******************************************************************************
 **
 **   Main Initilization
 **
 ******************************************************************************/

void
initialize( )
{
  registerEntry( "initialize()" );

  // atomHashTable initialization
  memset( atomHashTable, 0, ATOMHASHSIZE * sizeof( iatom_t* ) );

  // check dimensionality
  if( _low_numberSchema >= MAXSCHEMA )
    fatal( maxSchema );

  // ground atoms and generate predicate skeletons
  generateAtoms();
  generateVarsAndValues();

  // ground operators and compute reachable atoms
  // use alternative grounding method... very naive, but seems to work
  computeReachableAtoms( );

  // some general info
  std::cout << "GENERAL: number of atoms = " << numberAtoms << std::endl;
  std::cout << "GENERAL: number of reachable atoms = " << numberReachableAtoms << std::endl;
  std::cout << "GENERAL: number of grounded operators = " << grounded_schema.size() << std::endl;

  // successful initialization
  registerExit();
}

void
clean_of_source( set<int>& toclean, const set<int> source )
{
  static const bool clean_unreach = true;
  if( !clean_unreach )
    return;
  
  set<int> tmp;
  set_intersection( toclean.begin(), toclean.end(),
		    source.begin(), source.end(), 
		    inserter(tmp, tmp.begin()));
  swap(tmp,toclean);
}

void
trans_and_add( const set<int>& src, vector<int>& r2a, set<int>& dest, bool print_atoms = false )
{
  FOR( set<int>::const_iterator, it, src )
    {
      int var = abs(*it);
      int sign = (*it>0) ? 1 : -1;
      if( verbose > LEVEL1 && print_atoms )
	{
	  cout << (sign>0?"":"-") 
	       << atoms[r2a[var]] 
	       << (r2a[var]?"":" ("+int2str(var)+")") << " " << flush; 
	}
      if( r2a[var] == 0 )
	{
	  if(r2a[var] <= 0)
	    cout << "to raise: r2a[var] > 0, for var = " << var << endl;  
	  assert(r2a[var] > 0);
	}
      dest.insert( sign*r2a[var] );
    }
  if( print_atoms )
    cout << src.size();
}

bool
are_valid( const set<int>& s1 ){
  for( set<int>::const_iterator ai = s1.begin(); 
       ai != s1.end(); ai++ )
    if( *ai == 0 )
      return false;
  return true;
}


const bool trace_calc_models = false;
void
calc_models( const set<int>& lits, vector<clause>& cnf, vector<set<int> >& models )
{
  map<int,int> var2cnf;
  map<int,int> cnf2var;
  set<int> vars;
  size_t idx = 0;

  string name (".goal.cnf");
  string nameout (name+".sols");
  ofstream out( name.c_str() );

  // Adding vars for clauses
  FOR( vector<clause>::iterator, c, cnf )
    FOR_P( clause::iterator, lit, c )
      if(var2cnf.find(abs(*lit)) == var2cnf.end() )
        {
	  int var = abs(*lit);
	  vars.insert(var);
	  var2cnf[var] = ++idx;
	  cnf2var[idx] = var;
	  if(trace_calc_models)
	    cout << "var " << var << " = cnf var " << idx << endl; 
	}

  // Adding vars for lits
  FOR( set<int>::iterator, lit, lits )
    if(var2cnf.find(abs(*lit)) == var2cnf.end() )
      {
	int var = abs(*lit);
	vars.insert(var);
	var2cnf[var] = ++idx;
	cnf2var[idx] = var;
	if(trace_calc_models)
	  cout << "var " << var << " = cnf var " << idx << endl; 
      }
    
  
  // Adding clauses
  out << "p cnf " << idx << " " << (cnf.size()+lits.size()) << endl;
  FOR( vector<clause>::iterator, c, cnf )
    {
      FOR_P( clause::iterator, lit, c )
	out << (*lit>0?1:-1) * var2cnf[abs(*lit)] << " ";
      out << "0" << endl;
    }
  FOR( set<int>::iterator, lit, lits )
    out << (*lit>0?1:-1) * var2cnf[abs(*lit)] << " 0" << endl;
  
  string cmd("$TRANSLATOR_HOME/relsat -#a "+name+"|grep ^Solution|cut -d : -f 2 |sed 's/^\\ //' >"+nameout);
  system( cmd.c_str() );

  
  ifstream in( nameout.c_str() );
  
  string buff;
  std::getline(in, buff);
  while( !in.eof() )
    {
      set<int> model;
      char *ptr = strdup(buff.c_str());
      char *str = strtok( ptr, " " );
      
      if(str)
	{
	  int cnf = atoi(str);
	  if(cnf != 0)
	    {
	      int cnflit = (cnf>0?1:-1)*cnf2var[abs(cnf)];
	      model.insert(cnflit);
	    }
	  
	  while(str)
	    {
	      cnf = atoi(str);
	      if(cnf != 0)
		{
		  int cnflit = (cnf>0?1:-1)*cnf2var[abs(cnf)];
		  model.insert(cnflit);
		}
	      
	      str = strtok( NULL, " " );
	    }
	}
      free(ptr);
      if(1) // Neg vars not necessary? Are necessary, because inclusion verification
      FOR( set<int>::iterator, v, vars )
	if(model.count(*v)==0)
	  model.insert(-*v);
      models.push_back(model);
      if(trace_calc_models)
	cout << "Model: " << set2str(model) << endl;
      std::getline(in, buff);
    }
  
  in.close();

  string cmd2("/bin/rm "+name+" "+nameout);
  system( cmd.c_str() );
}

void
initInstanceDB( void )
{
  registerEntry( "initInstanceDB()" );

  // Constructing the infra structure

  std::set<int>::const_iterator ai;
  set<int> all_atoms_old;
  for( ai = reachable_atoms.begin(); ai != reachable_atoms.end(); ++ai )
    all_atoms_old.insert(*ai);

  set<int> mentioned;
  set<int> assume_false;
  formulaAtoms( initialSituation, NULL, mentioned );
  set_difference( all_atoms_old.begin(), all_atoms_old.end(),
		  mentioned.begin(), mentioned.end(), 
		  inserter(assume_false, assume_false.begin()));

  set<int> true_literals_tmp;
  FOR( set<int>::iterator, f, assume_false )
    {
      true_literals_tmp.insert( -*f );
      assert( *f != 0 );
    }

  clauses c_oneof_tmp;
  clauses c_or_tmp;
  set<int> all_vars_uncert;
  getCNF( initialSituation, c_or_tmp, c_oneof_tmp, true_literals_tmp, true );
  
  if( use_oneof_or ) // Assume oneof as or
    {
      FOR( clauses::iterator, c, c_oneof_tmp )
	c_or_tmp.insert(*c);
      c_oneof_tmp.clear();
    }
  
  // Calc vars
  FOR( clauses::iterator, c, c_oneof_tmp )
    FOR_P( clause::iterator, l, c )
    all_vars_uncert.insert(abs(*l));
  FOR( clauses::iterator, c, c_or_tmp )
    FOR_P( clause::iterator, l, c )
    all_vars_uncert.insert(abs(*l));
  
  clean_of_source(all_vars_uncert, reachable_atoms );
  if( verbose > LEVEL1 )
    {
      cout << "Initial unknown vars: ";
      FOR( set<int>::iterator, a, all_vars_uncert )
	cout << *a << " ";
      cout << endl;
    }

  // Del tautologies
  vector<clauses::iterator> todel;
  FOR( clauses::iterator, c, c_oneof_tmp )
    FOR_P( clause::iterator, l, c )
      if( c->count(-*l) > 0 )
	{
	  todel.push_back(c);
	  break;
	}
  FOR(vector<clauses::iterator>::iterator, it, todel )
    c_oneof_tmp.erase(*it);
  todel.clear();
  FOR( clauses::iterator, c, c_or_tmp )
    FOR_P( clause::iterator, l, c )
      if( c->count(-*l) > 0 )
	{
	  todel.push_back(c);
	  break;
	}
  FOR(vector<clauses::iterator>::iterator, it, todel )
    c_or_tmp.erase(*it);

  // Calc remaining vars
  set<int> real_vars_uncert;
  FOR( clauses::iterator, c, c_oneof_tmp )
    FOR_P( clause::iterator, l, c )
      real_vars_uncert.insert(abs(*l));
  FOR( clauses::iterator, c, c_or_tmp )
    FOR_P( clause::iterator, l, c )
      real_vars_uncert.insert(abs(*l));

  clean_of_source(real_vars_uncert, reachable_atoms );

  // Vars mentioned in formula, but only in tautologies are simply unknown
  set<int> unknowntmp = all_vars_uncert;
  set<int> simple_unknowntmp;
  set_difference( all_vars_uncert.begin(), all_vars_uncert.end(),
		  real_vars_uncert.begin(), real_vars_uncert.end(), 
		  inserter(simple_unknowntmp, simple_unknowntmp.begin()));

  // Save original or, but unit-resolution free
  clauses c_tmp;
  unit_res( c_or_tmp, c_tmp, true_literals_tmp );
  swap(c_or_tmp, c_tmp);
  // Transform oneof into clauses
  clauses c_or_tmp_orig = c_or_tmp;
  if(true) // was before: if(use_t0c)
    {
      // Translate exclusions
      FOR( clauses::iterator, c, c_oneof_tmp )
	{
	  c_or_tmp.insert( *c );
	  FOR_P( clause::iterator, lit1, c )
	    {
	      clause::iterator lit2 = lit1;
	      ++lit2;
	      for( ; lit2 != c->end(); lit2++ )
		{
		  clause nc;
		  nc.insert( -*lit1 );
		  nc.insert( -*lit2 );
		  c_or_tmp.insert( nc );
		}
	    }
	}

      // Ensure that c_or_tmp doesn't contain units
      clauses res;
      unit_res( c_or_tmp, res, true_literals_tmp );
      c_or_tmp = res;
    }

  if(1)
    FOR(clauses::iterator, it,  c_or_tmp )
      FOR_P(clause::iterator, lit, it )
        assert(*lit!=0);
  // Use c_oneof_tmp for kp and t0
  // use only c_or for t0c
  // OJO: Rechazar or for kp and t0

  std::vector<When>::const_iterator wi;
  // Filling atoms tables
  atoms.resize( reachable_atoms.size() + 6 );

  // translation from old atoms to new ones
  r2a.resize( numberAtoms + 1 );
  for( size_t i = 0; i < r2a.size(); i++)
    r2a[i] = 0;
  atoms[0] = string("ERROR");
//   true_literals.insert(0);
  atoms[++natoms] = string("FALSE");
  true_literals.insert(-natoms);
  atoms[++natoms] = dummy_pred;
//   true_literals.insert(-natoms);
  dummy_pred_n = natoms;
  true_literals.insert(dummy_pred_n);

  for( ai = reachable_atoms.begin(); ai != reachable_atoms.end(); ++ai )
    { 
      char *c = readAtomByNumber(*ai)->name;
      put_spaces(c);
      char *d = strchr(c, ')');
      *d = 0;
      atoms[++natoms] = string(c+1);
      assert( *ai <= numberAtoms );
      if( verbose > LEVEL1 )
	cout << "for atom " << atoms[natoms] << ". old value = " << *ai << ". new value = "  << natoms << endl;
      r2a[*ai] = natoms;
      all_atoms.insert(natoms);
    }
  //assert( natoms + 1 == atoms.size() );

  set<int> static_atoms_tmp = static_atoms;
  static_atoms.clear();
  cout << "new Static atoms: ";
  trans_and_add( static_atoms_tmp, r2a, static_atoms, true );
  cout << endl;

  // Translating uncertainty
  cout << "Unknown atoms: ";
  trans_and_add( unknowntmp, r2a, unknown, true );
  cout << endl;
  
  cout << "Simply unknown atoms: ";
  trans_and_add( simple_unknowntmp, r2a, simple_unknown, true );
  cout << endl;
  
  set_difference( all_atoms.begin(), all_atoms.end(),
		  unknown.begin(), unknown.end(), 
		  inserter(known, known.begin()));
  cout << "Known atoms: ";
  if( verbose > LEVEL1 )
    FOR( set<int>::const_iterator, a, known )
      cout << atoms[*a] << " ";
  else
    cout << atoms.size();
  cout << endl;

  cout << "True literals (after unit resolution): ";
  trans_and_add( true_literals_tmp, r2a, true_literals, true );
  cout << endl;

  size_t i = 0;
  FOR( clauses::iterator, it, c_oneof_tmp )
    {
      const set<int>& s = *it;
      oneof.push_back(set<int>());
      set<int>& s2 = oneof.back();
      cout << "Oneof (" << i++ << "): ";
      trans_and_add( s, r2a, s2, true );
      cout << endl;
    }

  i = 0;
  bool print_or = false;
  FOR( clauses::iterator, it, c_or_tmp )
    {
      const set<int>& s = *it;
      disj.push_back(set<int>());
      set<int>& s2 = disj.back();
      if( print_or) 
	cout << "Or (" << i++ << "): ";
      trans_and_add( s, r2a, s2, print_or );
      if( print_or) 
	cout << endl;
    }
  cout << endl << "---- end uncertainty: "<< endl;
  FOR( clauses::iterator, it, c_or_tmp_orig )
    {
      const set<int>& s = *it;
      disj_orig.push_back(set<int>());
      set<int>& s2 = disj_orig.back();
      trans_and_add( s, r2a, s2 );
    }
  
  FOR( vector<clause>::iterator, c, disj )
    FOR_P( clause::iterator, l, c )
      lit_unknown.insert(*l);
  FOR( vector<clause>::iterator, c, oneof )
    FOR_P( clause::iterator, l, c )
      lit_unknown.insert(*l);

  FOR( vector<clause>::iterator, c, disj_orig )
    FOR_P( clause::iterator, l, c )
      disj_orig_lits.insert(*l);
  FOR( vector<clause>::iterator, c, oneof )
    FOR_P( clause::iterator, l, c )
      oneof_lits.insert(*l);

  // Filling action tables
  // all atoms refer to new atoms
  int naction = 0;
  // Avoid relocalization of When, etc... 
  // If violated, detected by assert over dir_actions
  actions.reserve( numberReachableAtoms + 10 ); 

  // First action empty
  actions.push_back(Act());
  actions.back().name = string("NO ACTION");
  observations.push_back(Act());
  observations.back().name = string("NO OBSERVATION");
  
  ground2action.resize( grounded_schema.size() );
  cout << "Reachable " << reachable_atoms.size() << endl;
  for( size_t i = 0; i < grounded_schema.size(); ++i )
    {
      grounded_schema_t gs = grounded_schema[i];
      //actions.push_back(Act());
      //Act& a = actions.back();
      Act a;
      a.ground = i;
      a.index = ++naction;

      const bool debug_reach = false;
      bool reachable = true;
      if( debug_reach )
	{
	  cout << "considering ";
	  printOperatorGrounded( gs.parameters_, gs.schema_ );
	}
      if( !operatorAtomsWhen( _low_schemaTable[gs.schema_], gs.parameters_, a, &r2a, 
			      reachable_atoms, &static_positive_atoms ) )
	reachable = false;
      if( debug_reach )
	{
	  cout << "extracted as ";
	  a.dump(cout);
	}
      if( debug_reach && !reachable ) cout << "not reachable 1 " << endl;
      reachable = reachable && are_valid( a.prec );
      if( debug_reach && !reachable ) cout << "not reachable 2 " << endl;
      if( reachable )
	for( ai = a.prec.begin(); ai != a.prec.end(); ++ai )
	  if( a.prec.count(-*ai) > 0 )
	    {
	      reachable = false;
	      break;
	    }
      if( debug_reach && !reachable ) cout << "not reachable 3 " << endl;
      reachable = reachable && are_valid( a.eff );
      if( debug_reach && !reachable ) cout << "not reachable 4 " << endl;
      if( reachable )
	for( ai = a.eff.begin(); ai != a.eff.end(); ++ai )
	  if( a.eff.count(-*ai) > 0 )
	    {
	      reachable = false;
	      break;
	    }
      if( debug_reach && !reachable ) cout << "not reachable 5 " << endl;
      if( reachable )
	for( wi = a.conds.begin(); reachable && wi != a.conds.end(); ++wi )
	  {
	    reachable = reachable && are_valid( wi->prec );
	    if( debug_reach && !reachable ) cout << "not valid prec of  " << *wi;
	    reachable = reachable && are_valid( wi->eff );
	    if( debug_reach && !reachable ) cout << "not valid eff of  " << *wi;
	  }
      if( debug_reach && !reachable ) cout << "not reachable 6 " << endl;

      // If not breachable || not use reachable conds
      // reachable = false;

      if( verbose > LEVEL4 )
	{
	  cout << "Action (" << i << ") ";
	  printOperatorGrounded( gs.parameters_, gs.schema_ );
	  if( reachable )
	    cout <<  " active " <<  endl;
	  else
	    cout << " inactive " <<  endl;
	}

      char *name = operatorName2( gs.parameters_, gs.schema_ );
      strip_space( name );
      a.name = string(name);

      if( !reachable )
	{
	  ground2action[i] = -1;
	  naction--;
	  continue;
	}

      if( _low_schemaTable[gs.schema_]->is_observation )
	{
	  a.is_observation = true;
	  observations.push_back(a);
	  naction--;
	  if( verbose > LEVEL4 )
	    cout << "Observation " << a.name << " saved" << endl;
	}
      else
	{
	  ground2action[i] = naction;

	  a.is_observation = false;
	  prepare_action(a);
	  for( ai = a.prec.begin(); ai != a.prec.end(); ++ai )
	    precs_literals.insert( *ai );
	  actions.push_back(a);
	}
    }
  dir_actions = (void*) &actions[0];
  std::cout << "GENERAL: number of actions = " << actions.size()-1 << std::endl;

  if( verbose > LEVEL1 )
    std::cout << "Adding goal atoms not reachable" << std::endl;
  const bool gen_false = false;
  std::set<int> goal_atoms;
  formulaAtomsPos( goalSituation, NULL, 0, goal_atoms );
  for( ai = goal_atoms.begin(); ai != goal_atoms.end(); ++ai )
    if( r2a[*ai] == 0 )
      { 
	if(! gen_false )
	  {
	    char *c = readAtomByNumber(*ai)->name;
	    put_spaces(c);
	    char *d = strchr(c, ')');
	    *d = 0;
	    atoms.push_back(string(c+1) + string("_FALSE"));
	    ++natoms;
	    assert( *ai <= numberAtoms );
	    if( verbose > LEVEL1 )
	      cout << "atom " << atoms[natoms] << " = " << *ai << " is "  << natoms << endl;
	    cout << "WARNING: goal literal " << atoms[natoms] << " seems to be non reachable" << endl;
	  }
	r2a[*ai] = natoms;
      }
  //initAtomsDB( natoms+1 );
  //assert( natoms + 1 == atoms.size() );

  // Translating complex goal into simple
  clauses g_oneof_tmp;
  clauses g_or_tmp;
  set<int> goal_literals_tmp;
  getCNF( goalSituation, g_or_tmp, g_oneof_tmp, goal_literals_tmp, true );
  if( !g_oneof_tmp.empty() )
    {
      cerr << "FATAL ERROR: Oneof does not accepted in goal" << endl;
      assert( g_oneof_tmp.empty() );
    }
  FOR( clauses::iterator, it, g_or_tmp )
    {
      const clause& s = *it;
      clause s2;
      trans_and_add( s, r2a, s2 );
      g_or.push_back(s2);
    }
  trans_and_add( goal_literals_tmp, r2a, goal_literals );
  /*
    if g_or is not empty, 
    for each clause, create new atom to said it was satified
    for every literal, cause this atom to be true.
    OJO: seems to be broken. Alex need it for some benchmarks
   */
  const bool keep_complex_goal = false;
  if( !g_or.empty() && !keep_complex_goal )
    {
      FOR( vector<Act>::iterator, ap, actions )
	ap->prec.insert( dummy_pred_n );

      actions.push_back(Act());
      Act& a = actions.back();
      a.ground = 0;
      a.index = ++naction;
      a.name = string("make_end_disj_goal");
      a.prec.insert( dummy_pred_n );
      a.eff.insert( -dummy_pred_n );

      int n = 0;
      if( use_cnf_complex_goal )
      {
	FOR( vector<clause>::iterator, it, g_or )
	{
	  const clause& s = *it;

	  // New atom
	  if( natoms+1 == atoms.size() )
	    atoms.push_back("");
	  atoms[++natoms] = string( "end_" + int2str(n++) );
	  int end_n = natoms;

  	  //atoms[end_n] = 
// 	  assert( atoms.size() == end_n+1);
	  goal_literals.insert( end_n );
	  true_literals.insert(-end_n );
	  if( verbose > LEVEL2 )
	    cout << "atom " << atoms[end_n] << " for goal clause " << set2str(s) << endl;
      
	  // Add one action with a cond effect for every literal
	  for( clause::const_iterator e = s.begin();
	       e != s.end(); e++ )
	    {
	      When w;
	      w.prec.insert(*e);
	      w.eff.insert(end_n);
	      a.addWhen(w);
	    }
	}
      }
      else 
	{
	  /*
	    Work, but generates too many possible goal states
	    as many of them are not reachable.
	    
	    Adding mutex or invariantes (Malte's for example)
	    num of goal state should reduce and become feasible.
	   */
	  FOR( vector<clause>::iterator, c, disj )
	    {
	      bool all_static = true;
	      for( clause::iterator it = c->begin();
		   all_static && it != c->end(); it++)
		if( static_atoms.count(abs(*it)) == 0 )
		  all_static = false;
	      if( all_static )
		{
		  cout << "Adding clause to goal CNF:" << set2str(*c) << endl;
		  g_or.push_back(*c);
		}
	      else
		cout << "NOT Adding clause to goal CNF:"  << set2str(*c) << endl;
	    }

	  vector<set<int> > models;
	  calc_models(goal_literals, g_or, models );

	  // Only a new atom: the super-goal
	  if( natoms+1 == atoms.size() )
	    atoms.push_back("");
	  atoms[++natoms] = string( "super_end_" + int2str(n++) );
	  int end_n = natoms;
	  
	  goal_literals.insert( end_n );
	  true_literals.insert(-end_n );

	  FOR( vector<set<int> >::iterator, it, models )
	    {
	      const set<int>& model = *it;
	      
	      if( verbose > LEVEL2 )
		cout << "cond effect for goal model " << set2str(model) << endl;
	      
	      // For each model, a cond effect causing the goal
	      When w;
	      for( set<int>::iterator e = model.begin();
		   e != model.end(); e++ )
		w.prec.insert(*e);
	      w.eff.insert(end_n);
	      a.addWhen(w);
	    }

	}
      g_or.clear();
    }

  registerExit();
}


/*******************************************************************************
 **
 **   Registers
 **
 ******************************************************************************/

void
registerEntry( char *procedure )
{
  procRegister_t *proc = 0;
  struct rusage r_usage;
  if( !(proc = (procRegister_t*)malloc( sizeof( procRegister_t ) )) )
    fatal( noMoreMemory );
  proc->procedure = procedure;
  proc->next = procStackTop;
  getrusage( RUSAGE_SELF, &r_usage );
  proc->diffTime = (float)r_usage.ru_utime.tv_sec + (float)r_usage.ru_stime.tv_sec +
    (float)r_usage.ru_utime.tv_usec/MILLION + (float)r_usage.ru_stime.tv_usec/MILLION;
  procStackTop = proc;
  std::cout << "REGISTER (entering): " << proc->procedure << std::endl;
}

int
registerExit( void )
{
  procRegister_t *proc;
  struct rusage r_usage;
  if( procStackTop != NULL )
    {
      proc = procStackTop;
      getrusage( RUSAGE_SELF, &r_usage );
      float res = ((float)r_usage.ru_utime.tv_sec + (float)r_usage.ru_stime.tv_sec +
		   (float)r_usage.ru_utime.tv_usec/MILLION + (float)r_usage.ru_stime.tv_usec/MILLION ) - 
	proc->diffTime;
      if(res < 0 && res > -0.00000001) res = 0;
      std::cout << "REGISTER: " << proc->procedure << " took " << res << " secs" << std::endl;
      procStackTop = proc->next;
      free( proc );
      return( 1 );
    }
  else
    {
      return( 0 );
    }
}

float
currentElapsedTime( void )
{
  struct rusage r_usage;
  getrusage( RUSAGE_SELF, &r_usage );
  return( (float)r_usage.ru_utime.tv_sec + (float)r_usage.ru_stime.tv_sec +
	  (float)r_usage.ru_utime.tv_usec/MILLION + (float)r_usage.ru_stime.tv_usec/MILLION );
}

void
flushAllRegisters( void )
{
  while( registerExit() );
}


/*******************************************************************************
 **
 **   Main Section
 **
 ******************************************************************************/

void
_fatal( int returnCode, char *s, char *file, int line )
{
  switch( returnCode )
    {
    case noMoreMemory:
      std::cout << "ERROR[" << file << ":" << line << "]: no more memory" << std::endl;
      break;
    case maxAtoms:
      std::cout << "ERROR: maximum atoms reached. Recompile" << std::endl;
      break;
    case maxSchema:
      std::cout << "ERROR: maximum operator schemata reached. Recompile" << std::endl;
      break;
    case noError:
      break;
    }
  std::cout << "ERROR: fatal error." << std::endl;
  flushAllRegisters();
  exit( returnCode );
}

int
parseProblem( void )
{
  int rv, fd, file;
  static char *files[2];
  extern int lineno;

  rv = 0;
  files[0] = problemFile;
  files[1] = domainFile;
  for( file = 0; file < 2; ++file )
    if( (fd = open( files[file], O_RDONLY )) == -1 )
      {
	perror( "ERROR: parsing files" );
	exit( -1 );
      }
    else
      {
	// redirection of fd to stdin
	if( file == 0 )
	  close( fileno( stdin ) );
	else
	  clearerr( stdin );
	dup( fd );
	_low_yyfile = files[file];
	lineno = 1;
	rv += yyparse();
	cout << "Parsed: " << files[file] << endl;
	close( fileno( stdin ) );
	close( fd );
      }
  rescue_later();
  return( rv );
}

std::ostream& operator<<( std::ostream& o, const Act& a )
{
  a.dump(o);
  return o;
}

std::ostream& operator<<( std::ostream& o, const When& w )
{
  w.dump(o);
  return o;
}

int
main( int argc, char **argv )
{
  predicates.clear();
  defined.clear();
  std::vector<std::string> output;

  // check arguments
  if( argc < 3 )
    {
    print_usage:
      usage( std::cout );
    }

  // banner
  registerEntry( "main()" );
  std::cout << "cf2cs v 2.00 -- A compiler from conformant planning problems into classical ones." 
	    << std::endl;
  std::cout << "CALL:";
  for( int i = 0; i < argc; ++i )
    std::cout << " " << argv[i];
  std::cout << std::endl;

  parseArg( argc, argv );

  // set seed
  unsigned short seed[3];
  seed[0] = seed[1] = seed[2] = 0;
  seed48( seed );

  // parse problem
  problemFile = strdup(argv[argc-2]);
  domainFile = strdup(argv[argc-1]);
  registerEntry( "parse()" );
  int rv = parseProblem();
  registerExit();

  if( !rv )
    {
      // initialize planner
      initialize();
      initInstanceDB();
  
      do_cf2cs();
    }

  assert( dir_actions == (void*)&actions[0] );

  registerExit();
  flushAllRegisters();

  // return parse status
  return( rv );
}
