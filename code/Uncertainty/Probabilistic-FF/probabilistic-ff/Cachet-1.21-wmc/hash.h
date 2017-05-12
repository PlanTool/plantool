#include <assert.h>
#include <iostream>
#include <vector>
#include <list>

#include <linux/kernel.h>
#include <linux/sys.h>
#include <sys/sysinfo.h>

#include "zchaff_header.h"
#include "zchaff_base.h"
		
class CHashTable {
public:
	vector<Component_value_pair *> hash_table;
	unsigned num_literals;
	unsigned mem_usage;
	unsigned content_usage;
	unsigned literal_usage;
	unsigned hashtable_size;
	unsigned num_collisions;
	unsigned active_entries;
	unsigned num_removed_entries;
	unsigned num_hit;
	unsigned pos_hit;
	unsigned neg_hit;
	unsigned bound;
	unsigned hit_bound;
	unsigned del_bound;
	unsigned max_entry;
	unsigned oldest;
	unsigned clean_limit;
	unsigned clean_interval;
	bool full;
	bool quiet;
	double load_factor;
	struct sysinfo si;

	void test();
	int clean_cache();	// return the number of entries removed during the cleaning
	bool removeChildren(child_to_remove &); // remove all children of a node(component)

	// add a formula associated with value to the specified entry in hash table
	bool set_hash_table_entry(unsigned, Component_value_pair &);
#ifdef APPROXIMATE_HASHING
#ifdef BIG_NUM
	bool CHashTable::in_hash_table(formula &, unsigned &, unsigned long &, BigNum &);
#else
	bool CHashTable::in_hash_table(formula &, unsigned &, unsigned long &, long double &);
#endif
#else
#ifdef BIG_NUM
	bool in_hash_table(formula &, unsigned &, BigNum &);	// return true and value if found, if not found hash index
#else
	bool in_hash_table(formula &, unsigned &, long double &);	// return true and value if found, if not found hash index
#endif	// end ifdef BIG_NUM
#endif	// end ifdef APPROXIMATE_HASHING

	bool compare_formula(formula &, formula &); // check if two formulas are indentical
	void print_formula(formula &);	// print the formulared stored in a hash entry
	void dump_formula_stack(vector<Component_value_pair *> &, int &); // output current formula stack

	// quicksort	//	added by sang
	bool less_than(vector<int> &, vector<int> &);
	void quicksort(formula & f, int, int);
	void insertion_sort(formula & f, int, int);
	void swap_references(vector<int> * &, vector<int> * &);
	bool check_sorted(formula & f);

	CHashTable(void) {
	}
	CHashTable(unsigned size, unsigned maximum_entry, unsigned oldest_entry, unsigned limit_clean) 
	{
		num_literals = 0;
		mem_usage = 0;
		content_usage = 0;
		literal_usage = 0;
		num_collisions = 0;
		active_entries = 0;
		num_removed_entries = 0;
		num_hit = 0;
		pos_hit = neg_hit = 0;
		bound = 100000;
		hit_bound = 100000;
		del_bound = 100000;
		full = false;
		quiet = false;
		hashtable_size = size;
		max_entry = maximum_entry;
		oldest = oldest_entry;
		clean_limit = limit_clean;
		clean_interval = 1000;		// check if freeram every 10K entries added
		//if (oldest != max_entry)
		//	clean_limit = 20 * oldest_entry;
		hash_table.resize(size);	// allocate meomry for hashtable entries only
		
		//const double megabyte = 1024 * 1024;
		/* Obtain system statistics. */
		//struct sysinfo si;
		//sysinfo (&si);
		//cout << "total RAM = " <<  si.totalram << ", free RAM = " << si.freeram 
		//	 << ", buffered RAM = " <<  si.bufferram << endl; //<< ", shared RAM = " << si.sharedram << endl;
	 }
	
	~CHashTable() {
		while (!hash_table.empty()) {
			delete hash_table.back();
			hash_table.pop_back();
		}
	}
};
