#ifndef GLOBAL_H
#define GLOBAL_H

#include <stdio.h>
#include <string>
#include <memory>
#include <stack>

#ifndef UCHAR_MAX
#define UCHAR_MAX       255
#endif
#ifndef USHORT_MAX
#define USHORT_MAX      65535
#endif

#define NUMBER_NAME     "number"
#define OBJECT_NAME     "object"
#define DISP_SIZE       128
#define DISP_INT_SIZE   128

class heuristic_t;

namespace gpt
{
  // Current version of this package.
  //
  // *** IS THIS PARAMETER SET MANUALLY? Yes, set by the developer.
  extern const char* version;

  // The algorithm that is currently being used.
  //
  // *** IS THIS PARAMETER SET MANUALLY? Yes, set via the command line.
  extern char *algorithm;

  // Flag indicating whether domain analysis should be performed -- deprecated.
  extern bool domain_analysis;

  // Convergence tolerance. The planner halts when the values of all relevant
  // states change by a fraction of no more than epsilon as a result of updates.
  //
  // *** IS THIS PARAMETER SET MANUALLY? Yes, set via the command line.
  extern double epsilon;

  // The heuristic that is currently being used.
  //
  // *** IS THIS PARAMETER SET MANUALLY? Yes, set via the command line.
  extern char *heuristic;

  // The number of buckets in the state-value hash tables.
  //
  // *** IS THIS PARAMETER SET MANUALLY? Yes. A good value depends on the number
  // of states in the problem, since this will approximately determine the 
  // number of entries in this hash. 
  extern size_t initial_hash_size;

  // The seed used for randomized operations.
  extern unsigned seed;

  // Verbosity level (0-550). The higher it is, the more deugging information
  // is printed. 
  //
  // *** IS THIS PARAMETER SET MANUALLY? Not at present.
  extern unsigned verbosity;

  // Warning level for the parser -- deprecated.
  extern unsigned warning_level;

  // The heuristics stack. Planners are supposed to evaluate states using all 
  // the heuristics on the stack and then combine the values given by these
  // heuristics in some way.
  //
  // *** IS THIS PARAMETER SET MANUALLY? Yes, indirectly, by specifying the 
  // heuristics via the commang line.
  extern std::stack<heuristic_t*> hstack;

  // The amount of time in seconds available for planning. After planning for 
  // this amount of time, the chosen planner terminates and commnunicates the 
  // policy computed so far to the server. 
  //
  // *** IS THIS PARAMETER SET MANUALLY? Yes, set via the command line.
  extern size_t timeout;

  // The amount of memory in bytes by which the successor hash table gets 
  // reduced every time there is a necessity to do so. The planners in this 
  // package, Glutton and Gourmand, both use the so-called successor hash table.
  // This table isn't essential, ut gives them a significant speed boost. 
  // If memory is needed by crucial parts of the planner, the successor hash
  // table gets shrunk to free up some space. The value of the 
  // successor_hash_reduction_amount variable is the amount of memory that is
  // supposed to get freed up each time the successor table is shrunk 
  // (provided, of course, that the table is bigger than that in size).
  //
  // *** IS THIS PARAMETER SET MANUALLY? Yes. Generally, the smaller this value,
  // the few entries get removed from the successor hash, the rarer this hash
  // table gets reduced in size, and hence the less the overhead of maintaining
  // the hash. On the other hand, the more you remove from the hash at a time,
  // the less effective hash becomes. Genreally, provided that that this 
  // parameter is on the order of tens of MB, the planner's performance isn't 
  // very sensitive to it.
  extern const unsigned long successor_hash_reduction_amount;

  // The amount of memory in bytes available to the chosen planner. This 
  // variable controls the planners' internal memory management. If its value 
  // is smaller than the amount of memory _actually_ available to the planner,
  // the planner may crash.
  //
  // *** IS THIS PARAMETER SET MANUALLY? Yes. It should be set to the amount
  // of memory you are willing to give to the planner. On Amazon's large EC2 
  // instances with 7.5GB RAM, a recommended setting of this parameter is 6.8GB
  // (0.7GB accounts for Linux itself and unforeseen spikes in memory usage).
  extern const unsigned long total_RAM;

  // The number of administrative bytes allocated by the heap per program
  // -requested object. When the runtime allocates memory for an object (e.g.,
  // as a result of a call to the operator "new"), it also takes a few extra 
  // bytes for bookkeeping purposes. This number varies from 
  // allocation to allocation, but is at most 8 bytes on a 32-bit system and 16 
  // bytes on a 64-bit one. We need to take these chunks into account in order 
  // to accurately maintain the hash table sizes (see below).
  //
  // *** IS THIS PARAMETER SET MANUALLY? No, it isset automatically depending on
  // architecture type.
  extern const unsigned short heap_overhead;

  // The amount of memory in bytes occupied by the hash table that maps const
  // pointers to states.
  //
  // *** IS THIS PARAMETER SET MANUALLY? No, it must start at 0 and 
  // is maintained by the planner.
  extern unsigned long mem_state_hash;

  // The amount of memory in bytes occupied by the hash table that maps const
  // state pointers (that is, effectively, states themselves) to values.
  //
  // *** IS THIS PARAMETER SET MANUALLY? No, it must start at 0 and 
  // is maintained by the planner.
  extern unsigned long mem_state_value_hash;

  // The amount of memory in bytes occupied by the hash table that maps const
  // state pointers (that is, effectively, states themselves) to data containers
  // keeping information about the successors of these states under all actions.
  //
  // *** IS THIS PARAMETER SET MANUALLY? No, it must start at 0 and 
  // is maintained by the planner.
  extern unsigned long mem_successor_hash;

  // A flag indicating whether the total size of all hash tables has exceeded
  // the total_RAM threshold and none of the tables can be reduced, i.e., the
  // system has exhaused all the allowed memory.
  //
  // *** IS THIS PARAMETER SET MANUALLY? No, it must start at "true" and is 
  // set automatically when appropriate.
  extern bool out_of_memory;

  // The largest number smaller than the total amount of memory occupied by 
  // the hash tables that is evenly divisible by gpt::tick_size (see below).
  //
  // *** IS THIS PARAMETER SET MANUALLY? No, it must start at 0 and 
  // is maintained by the planner.
  extern unsigned long nearest_tick;

  // Every time the memory occupied by all hash tables increases by this amount,
  // it is reported.
  //
  // *** IS THIS PARAMETER SET MANUALLY? Yes. It determines the frequency with 
  // which total hash table memory size is printed out.
  extern unsigned long tick_size;

  // A flag indicating whether the invoked planner should use the best default
  // policy or whether it should compute an "intelligent" policy. The definition
  // of a default pplicy varies from planner to planner, but typically these
  // are some "canned" policies that don't require much computation and serve
  // as a fallback option in case an intelligent policy is too hard to comppute.
  //
  // *** IS THIS PARAMETER SET MANUALLY? Can be, but doesn't have to be. See 
  // main.cc for an example of how to use it.
  extern bool use_best_default_policy;

  // The empirical expected reward of the highest-reward default policy.
  //
  // *** IS THIS PARAMETER SET MANUALLY? No, it is set by the planner.
  extern double best_default_policy_reward;

  // The reward yielded by the most recent policy execution on the problem
  // server.
  //
  // *** IS THIS PARAMETER SET MANUALLY? No, it is set by the planner client
  // that communicates with the server.
  extern double previous_solution_attempt_reward;

  // A flag indicating whether the problem has been solved completely during
  // the previous problem-solving attempt, i.e. whether the initial state of the
  // problem has been solved for the number of steps-to-go equal to 
  // the problem's horizon.
  //
  // *** IS THIS PARAMETER SET MANUALLY? No, it is set by the planner.
  extern bool solved_completely;

  // The number of actions in the problem being solved.
  //
  // *** IS THIS PARAMETER SET MANUALLY? No, it is set by the code that
  // processes the problem description.
  extern size_t num_acts;

  // The number of successor states of a state under each action that are 
  // sampled each time a data container for the mem_successor_hash is filled.
  //
  // A successor data container for a state holds, among other things, a sampled
  // set of successors of that state under every action in the problem. These
  // successor sets are created by sampling the successors for each action as
  // dictated by that action's transition probabilities until the successor
  // set is "large enough" (i.e., empirically represents the true distribution
  // over the successors under that action well). Since we don't know
  // a-priori how many sampled successors are "enough", the sampling process 
  // proceeds in stages, where we sample some number of successors in every 
  // stage and then heuristically check whether we need to sample more;
  // gpt::num_succ_per_expansion is the number of successors sampled per one
  // such stage.
  //
  // *** IS THIS PARAMETER SET MANUALLY? Yes. Its value should be largely 
  // determined by the tradeoff of how memory you have and how fast you want 
  // the planner to be vs. how good of a solution you want to get. The larger 
  // this parameter, the better the solution policy quality, but the more 
  // expensive the solution is to compute. We haven't tuned it much, but values
  // of around 15-30 appear to work well across a range of problems.
  extern size_t num_succ_per_expansion;

  // The maximum number of distinct successors sampled for each action for a 
  // given state.
  //
  // One way to stop the process of sampling state successors described above is
  // to halt it when the number of distinct state samples reaches some 
  // threshold (note that, depending on the action, the same state may be 
  // sampled several times as a successor, resulting in samples that are not 
  // distinct). The gpt::max_distrib_support parameter is this threshold.
  //
  // *** IS THIS PARAMETER SET MANUALLY? Yes. Generally, you want to set it to 
  // something around or smaller than gpt::num_succ_per_expansion. Values around
  // 10-20 appear to work well across the IPPC-2011 problems.
  extern size_t max_distrib_support;

  // The number of new distict samples each successor sampling stage adds to
  // the distribution. 
  //
  // In addition to gpt::max_distrib_support, another way to stop the successor
  // sampling process (which works in combination with the first one) is to halt
  // it when a sampling stage adds only very few distinct new samples to the 
  // already collected sample set; gpt::min_support_growth_rate is the minimum
  // number of additional samples per stage required for the sampling process 
  // to go on.
  //
  // *** IS THIS PARAMETER SET MANUALLY? Yes. Set it to something small, e.g., 1
  // or 2. The IPPC-2011 setting was 2.
  extern const size_t min_support_growth_rate;

  // The number of policy execution rounds. Its value should come from the 
  // problem server that, at the beginning of a sessoion, tells the client (us)
  // how many times it wants the planner to execute the computed policy.
  //
  // *** IS THIS PARAMETER SET MANUALLY? No.
  extern size_t num_rounds;
};

typedef unsigned char uchar_t;
typedef unsigned short ushort_t;

inline void
notify( void *ptr, const char *name )
{
#ifdef MEM_DEBUG
  fprintf( stderr, "notify %s %p\n", name, ptr );
#endif
}

#endif // GLOBAL_H
