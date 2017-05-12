#include <set>

using std::set;

typedef set<int> clause;
typedef set<clause> clauses;

void pr_clause( const clause& c );
void pr_clauses( const clauses& c );

// In: clauses
// In & Out: unit clauses
// Out: res: clauses after unit resolution

bool unit_res(const clauses& cs, clauses& res, set<int>& unit);
