#include "global.h"
#include "limits.h"

namespace gpt
{
  const char* version = (char *)"2.0";
  char *algorithm = (char *)"glutton";
  bool domain_analysis = false;
  double epsilon = 0.05;
  char *heuristic = (char *)"lash";
  size_t initial_hash_size = 8 * 204800;
  unsigned seed = 407343;
  unsigned verbosity = 5;
  unsigned warning_level = 0;
  std::stack<heuristic_t*> hstack;
  size_t timeout = UINT_MAX;
  const unsigned long total_RAM =  6800000000;

#if __WORDSIZE == 32
  const unsigned short heap_overhead = 8;
#elif __WORDSIZE == 64
  const unsigned short heap_overhead = 16;
#else
  const unsigned short heap_overhead = 0;
#endif

  const unsigned long successor_hash_reduction_amount = 200000000;
  unsigned long mem_state_hash = 0;
  unsigned long mem_state_value_hash = 0;
  unsigned long mem_successor_hash = 0;
  bool out_of_memory = false;
  unsigned long nearest_tick = 0;
  unsigned long tick_size = 100000000;
  bool use_best_default_policy = false;
  double best_default_policy_reward = -1000000000;
  double previous_solution_attempt_reward = -1000000000;
  bool solved_completely = false;
  size_t num_acts = 0;
  size_t max_distrib_support = 10;
  size_t num_succ_per_expansion = 15;
  const size_t min_support_growth_rate = 2;
  size_t num_rounds = 30;
};


#if MEM_DEBUG

void *
operator new( size_t size )
{
  void *result = malloc( size );
  fprintf( stderr, "new %p %d\n", result, size );
  return( result );
}

void *
operator new[]( size_t size )
{
  void *result = malloc( size );
  fprintf( stderr, "new[] %p %d\n", result, size );
  return( result );
}

void
operator delete( void *ptr )
{
  if( ptr )
    {
      fprintf( stderr, "del %p\n", ptr );
      free( ptr );
    }
}

void
operator delete[]( void *ptr )
{
  if( ptr )
    {
      fprintf( stderr, "del[] %p\n", ptr );
      free( ptr );
    }
}

#endif // MEM_DEBUG
