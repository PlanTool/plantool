#include "hash.h"
#include "crc32.h"
#include "stdio.h"
#include <algorithm>


bool CHashTable::set_hash_table_entry(unsigned hash_index, Component_value_pair & content)
{
	unsigned index = hash_index;
	if (hash_table[index])
	{
		//	cout << "warning: entry " << index << " is not empty!" << endl;
		content.next = hash_table[index];	// insert content af front
		hash_table[index] = & content;
	}
	else
	{
		content.next = NULL;
		hash_table[index] = & content;
	}
	content.sequence_number = active_entries;
	//content_usage = content_usage + (8 + content.f.capacity() * sizeof(content.f[0])) + sizeof(vector<vector<int> *>);
	//for (int i = content.f.capacity()-1; i >= 0; --i)
	//	literal_usage = literal_usage + content.f[i]->capacity() * sizeof ((* content.f[0])[0]) + sizeof(vector<int>);
	//mem_usage = content_usage + literal_usage;
	//cout << "literal_usage = " << literal_usage << endl;
	//for (int i = content.f.size()-1; i >= 0; --i)
	//	num_literals = num_literals + content.f[i]->size();
	if (++active_entries >= bound)
	{
		if (!quiet)
		cout << "Added cache entries: " << active_entries << endl;
		bound = bound + 100000;
	}
	if (active_entries - num_removed_entries >= clean_limit)	// if more than 20*oldest entries exist, clean cache
	{
		int temp = oldest;
		//oldest = 0;	// empty cache
		oldest = (active_entries - num_removed_entries)/4;		// keep 1/4 entries
		//if (!quiet)
		//	cout << "more than " << clean_limit << " entries exist, cache cleaning triggered" << endl;
		int removed = clean_cache();
		//if (!quiet)
		//	cout << removed << " cache entries removed by cleaning cache"
		//		 << ", removed entries = " << num_removed_entries << endl; 
		oldest = temp;	// restore oldest
	}
	return true;
}


#ifndef APPROXIMATE_HASHING
#ifdef BIG_NUM
bool CHashTable::in_hash_table(formula & f, unsigned & hash_index, BigNum & value)
#else
bool CHashTable::in_hash_table(formula & f, unsigned & hash_index, long double & value)
#endif
{
	bool result = false;
	char buf;
	char byte0, byte1; // byte2, byte3;
	unsigned long crc;
	unsigned literal_num = f.size(); // number of clauses in the formula
	hash_index = 0;			// reset hash_index
	INIT_CRC32(crc);

	for (int i = 0; i < literal_num; i++)	// computing hash_index and secondary_index
	{
		byte0 = f[i];
		byte1 = f[i] >> 8;
		//byte2 = f[i] >> 16;
		//byte3 = f[i] >> 24;
		CALC_CRC32(crc, byte0);
		CALC_CRC32(crc, byte1);
		//CALC_CRC32(crc, byte2);
		//CALC_CRC32(crc, byte3);
	}
	//if (hash_index > hashtable_size)
	//hash_index = hash_index % hashtable_size;
	hash_index = crc % hashtable_size;
#ifdef DEBUG_OUT
	cout << "hash_index after % hashtable_size: " << hash_index << endl;
#endif

	Component_value_pair * current = hash_table[hash_index];
	Component_value_pair * previous = current;

	while (current)
	{
		// if the current is not empty, check the content of it
		if (compare_formula(f, current->f))
		{	
#ifdef BIG_NUM
			mpz_set(value.numerator, current->value.numerator);
			value.denominator = current->value.denominator;
			value.zero_flag = current->value.zero_flag;			// copy cached value
			if (!value.zero_flag)
#else
			value = current->value;
			if (value > 0)
#endif
				++pos_hit;
			else
				++neg_hit;
			if (++num_hit >= hit_bound)
			{
				hit_bound = hit_bound + 100000;
				if (!quiet)
					cout << "number of cache hit: " << num_hit
						 << ", pos_hit = " << pos_hit << ", neg_hit = " << neg_hit << endl;
			}
			result = true;	// f already in the hash table
			break;
		}
		if (active_entries - current->sequence_number > oldest)
		{
			if (previous == current)	// current == hash_table[hash_index]
				hash_table[hash_index] = NULL;
			else
				previous->next = NULL;	// all links after previous will be removed

			while (current)
			{
				previous = current;
				current = current->next;
				//formula & f = previous->f;
				//while (!f.empty())
				//{
				//	delete f.back();
				//	f.pop_back();
				//}
#ifdef BIG_NUM
				//mpz_clear(previous->value.numerator);
#endif
				for (int k = previous->cached_child_list.size()-1; k >= 0; --k)
					removeChildren(* previous->cached_child_list[k]);
				delete previous;
				++num_removed_entries;
 				if (num_removed_entries >= del_bound)
				{
					if (!quiet)
						cout << "Removed cache entries: " << num_removed_entries << endl;
					del_bound += 100000;
				}
			}	//	end while
			break;
		}	//	end if
		//if (result)
		//	break;
		previous = current;
		current = current->next;	
		// if the entry is not empty, do a linear probe in the chain
		++num_collisions;	// a collision found
	}	// end while	
	return result;
}
#else // APPROXIMATE_HASHING defined
#ifdef BIG_NUM
bool CHashTable::in_hash_table(formula & f, unsigned & hash_index, 
							   unsigned long & secondary_index, BigNum & value)
#else
bool CHashTable::in_hash_table(formula & f, unsigned & hash_index, 
							   unsigned long & secondary_index, long double & value)
#endif
{
	bool result = false;
	//char buf, reverse, temp;
	char byte0, byte1; // byte2, byte3;
	unsigned long crc, second_crc;
	//unsigned clause_num = f.size(); // number of clauses in the formula
	unsigned literal_num = f.size(); // number of clauses in the formula
	hash_index = 0;			// reset hash_index
	INIT_CRC32(crc);
	INIT_CRC32(second_crc);

	for (int i = 0; i < literal_num; i++)	// computing hash_index and secondary_index
	{
		byte0 = f[i];
		byte1 = f[i] >> 8;
		//byte2 = f[i] >> 16;
		//byte3 = f[i] >> 24;
		CALC_CRC32(crc, byte0);
		CALC_CRC32(crc, byte1);
		//CALC_CRC32(crc, byte2);
		//CALC_CRC32(crc, byte3);
	}
	CALC_CRC32(crc,literal_num);	// use literal_num as checksum
	CALC_CRC32(crc, -1);			// terminating symbol

	for (int i = literal_num - 1; i >= 0; i--)	// computing hash_index and secondary_index
	{
		byte0 = f[i];
		byte1 = f[i] >> 8;
		//byte2 = f[i] >> 16;
		//byte3 = f[i] >> 24;
		CALC_CRC32(second_crc, byte0);
		CALC_CRC32(second_crc, byte1);
		//CALC_CRC32(second_crc, byte2);
		//CALC_CRC32(second_crc, byte3);
	}
	CALC_CRC32(second_crc, -1);			// terminating symbol
	CALC_CRC32(second_crc,literal_num);	// use literal_num as checksum
	

	hash_index = crc % hashtable_size;
	secondary_index = second_crc;
	//cout << "hash_index = " << hash_index << ", secondary_index = " << secondary_index << endl;

	Component_value_pair * current = hash_table[hash_index];
	Component_value_pair * previous = current;

	while (current)
	{
		// if the current is not empty, check the content of it
		// if (compare_formula(f, current->f))
		if (current->secondary_index == secondary_index)	// check if two formulas equal
		{			
#ifdef BIG_NUM
			mpz_set(value.numerator, current->value.numerator);
			value.denominator = current->value.denominator;
			value.zero_flag = current->value.zero_flag;			// copy cached value
			if (!value.zero_flag)
#else
			value = current->value;
			if (value > 0)
#endif
				++pos_hit;
			else
				++neg_hit;
			if (++num_hit >= hit_bound)
			{
				hit_bound = hit_bound + 100000;
				if (!quiet)
					cout << "number of cache hit: " << num_hit
						 << ", pos_hit = " << pos_hit << ", neg_hit = " << neg_hit << endl;
			}
			result = true;	// f already in the hash table
			break;
		}
		if (active_entries - current->sequence_number > oldest)	// old entry found
		{
			if (previous == current)	// current == hash_table[hash_index]
				hash_table[hash_index] = NULL;
			else
				previous->next = NULL;	// all links after previous will be removed

			while (current)	// remove all nodes in the rest of the chain
			{
				previous = current;
				current = current->next;
				for (int k = previous->cached_child_list.size()-1; k >= 0; --k)
					removeChildren(* previous->cached_child_list[k]);
				delete previous;
				++num_removed_entries;
 				if (num_removed_entries >= del_bound)
				//if (num_removed_entries % 100 == 0)
				{
					if (!quiet)
						cout << "Removed entries: " << num_removed_entries << endl;
					del_bound += 100000;
				}
			}	//	end while
			break;
		}	//	end if
		previous = current;
		current = current->next;	
		// if the entry is not empty, do a linear probe in the chain
		++num_collisions;	// a collision found
	}	// end while	
	return result;
}
#endif // end ifndef APPROXIMATE_HASHING


bool CHashTable::compare_formula(formula & fa, formula & fb)
{
// check if two formulas are indentical
	if (fa.size() != fb.size()) 
		return false;
	for (unsigned i = 0; i < fa.size(); ++i)
	{
		if (fa[i] != fb[i])
			return false;
	} // end for i
	return true;
}


void CHashTable::print_formula(formula & f)	// This is a friend function of CSolver class
{
	unsigned literal_num = f.size();
	if (literal_num == 0)
	{
		cout << "empty formula!" << endl;
		return;
	}
	cout << "printing formula, literal num: " << literal_num << endl;
	bool first = true;
	int index = 0;
	for (unsigned i = 0; i < literal_num; i++)
	{
		if (first)
		{
			cout << index++ << ": (";
			first = false;
		}
		if (f[i] != 0)
			cout << (f[i] & 0x1 ? "-":"") << (f[i] >> 1) << " ";
		else
		{
			cout << "0) " << endl;
			first = true;
		}
	} // end for i
	cout << endl;
	return;
}

/*
//************************************************************************
// return true if first < second
bool CHashTable::less_than(vector<int> & first, vector<int> & second)
{
	int f_size = first.size();
	int s_size = second.size();
	int size = f_size < s_size ? f_size : s_size; // size is the smaller one
	int pos = 0;
	
	while(pos < size && first[pos] == second[pos])
		++pos;
	if (pos < size)
		if (first[pos] < second[pos])
			return true;
		else
			return false;
	if (f_size < s_size)
		return true;
	else
		return false;
}

//************************************************************************

void CHashTable::swap_references(vector<int> * & x, vector<int> * & y)
{
	vector<int> * temp;
	temp = x;
	x = y;
	y = temp;
	return;
}

//************************************************************************

void CHashTable::insertion_sort(formula & f, int left, int right)
{
	int j;
	for (int i = left; i <= right; ++i)
	{
		vector<int> * temp = f[i];
		for (j = i; j > left && less_than(* temp, * f[j -1]); --j)
			f[j] = f[j - 1];
		if (j < i)
			f[j] = temp;
	}
	return;
}
*/
//************************************************************************
/*
void CHashTable::quicksort(formula & f, int left, int right)
{
	if (right - left < CUT_OFF)	//	<= CUT_OFF elements to sort
	{
		insertion_sort(f, left, right);
		return;
	}

	int center = (left + right) / 2;
	//cout << "sorting left: " << left 
	//	 << ", right: " << right 
	//	 << ", center: " << center << endl;
	//cout << "before median3: " << endl;
	//print_formula(f);

	// median3 procedure begin, set the pivot to be the median of left, center, and right
	
	if (less_than(* f[center], * f[left]))
		swap_references(f[center], f[left]);
	if (less_than(* f[right], * f[left]))
		swap_references(f[right], f[left]);
	if (less_than(* f[right], * f[center]))
		swap_references(f[right], f[center]);
	swap_references(f[center], f[right - 1]);
	int pivot = right - 1;	// place the pivot at f[right-1]

	//cout << "before partition: " << endl;
	//print_formula(f);
	// median3 procedure end

	int i = left;
	int j = right -1;
	
	while(true)
	{
		while (less_than(* f[++i], * f[pivot]));
		while (less_than(* f[pivot], * f[--j])); 
		if (i < j) 
			swap_references(f[i], f[j]);
		else break;
	}
	swap_references(f[i], f[right - 1]);	// restore pivot
	//cout << "after partition: " << endl;
	//print_formula(f);
	//cout << "i = " << i << endl;
	//cout << "j = " << j << endl;
	//cout << "divided to sub-problems, sorting (" << left << ", " << i-1 << ") and "
	//	 << "(" << i+1 << ", " << right << ")" << endl;
	quicksort(f, left, i - 1);
	quicksort(f, i + 1, right);

	return;
}
//************************************************************************

bool CHashTable::check_sorted(formula & f)
{
	int size = f.size();
	for (int i = size - 1; i > 0; --i)
		if (less_than(* f[i], * f[i-1]))
		{
			cout << "error found, not sorted: "
				 << "f[" << i << "]: " << endl;

			vector<int> & cl = * f[i];
			cout << "(";
			for (unsigned j=0; j< cl.size(); ++j)
				cout << cl[j] << " ";
			cout << ") " << endl;

			vector<int> & cc = * f[i-1];

			cout << "f[" << i-1 << "]: " << endl;
			cout << "(";
			for (unsigned j=0; j< cc.size(); ++j)
				cout << cc[j] << " ";
			cout << ") " << endl;

			cout << "the whole formula is: " << endl;
			print_formula(f);

			return false;
		}
	return true;
}
*/

int CHashTable::clean_cache()
{
	int num_removed = 0;
	Component_value_pair * current, * previous;

	for (int i = this->hashtable_size - 1; i >= 0; --i)
	{
		current = hash_table[i];
		if (!current)
			continue;	//	an empty entry
		previous = current;

		while (current)
		{
			if (active_entries - current->sequence_number > oldest)
			{
				if (previous == current)	// current == hash_table[hash_index]
					hash_table[i] = NULL;
				else
					previous->next = NULL;	// all links after previous will be removed

				while (current)
				{
					previous = current;					
					current = current->next;
					//formula & f = previous->f;
					//while (!f.empty())
					//{
					//	delete f.back();
					//	f.pop_back();
					//}
#ifdef BIG_NUM
					//mpz_clear(previous->value.numerator);
#endif
					delete previous;

					int temp = num_removed_entries;
					for (int k = previous->cached_child_list.size()-1; k >= 0; --k)
						removeChildren(* previous->cached_child_list[k]);
					delete previous;
					num_removed += num_removed_entries - temp + 1;
					++num_removed_entries;
 					if (num_removed_entries >= del_bound)
					{
						if (!quiet)
							cout << "Removed cache entries: " << num_removed_entries << endl;
						del_bound += 100000;
					}
				}	//	end while (current)
				break;	// break from while (current)
			}	//	end if
			previous = current;
			current = current->next;	// proceed
			// if the entry is not empty, do a linear probe in the chain
		}	// end while (current)
	}	// end for
	return num_removed;
}


bool CHashTable::removeChildren(child_to_remove & child) // remove all children of a node(child component)
{
	vector<child_to_remove *> entry_to_remove_stack;
	child_to_remove * top_child;
	Component_value_pair * current, * previous;
	
	entry_to_remove_stack.push_back(& child);

	while(!entry_to_remove_stack.empty())
	{
		top_child = entry_to_remove_stack.back();
		entry_to_remove_stack.pop_back();
		//cout << "top_child: hash_index = " << top_child->hash_index
		//	 << ", seq_num = " << top_child->sequence_number << endl;
		current = hash_table[top_child->hash_index];
		if (current)	// if it is an empty entry, do nothing but delete top_child
		if (top_child->sequence_number == current->sequence_number)
		{	// sequence number matched, the first entry should be removed
			// first, push all the new children to the stack for the future removal
			// cout << "got the first entry" << endl;
			for(int i = current->cached_child_list.size() - 1; i >= 0; --i)
			{
				entry_to_remove_stack.push_back(current->cached_child_list[i]);
				//cout << "a new child pushed into stack: "
				//	 << " hash_index = " << current->cached_child_list[i]->hash_index
				//	 << ", seq_num = " << current->cached_child_list[i]->sequence_number << endl;
			}
			current->cached_child_list.clear();
			//for (int i = 0; i < entry_to_remove_stack.size(); ++i)
			//{
				//cout << "entry to remove " << i << " : "
				//	 << " hash_index = " << entry_to_remove_stack[i]->hash_index
				//	 << ", seq_num = " << entry_to_remove_stack[i]->sequence_number << endl;
			//}

			// second, update the chain
			hash_table[top_child->hash_index] = current->next;
#ifndef APPROXIMATE_HASHING
			//formula & f = current->f;
			//for (int i = f.size()-1; i >= 0; --i)
			//	delete f[i];
			//f.clear();
#endif
			delete current;
			//cout << "delete current done" << endl;
			++num_removed_entries;
		}			
		else // more than one entry, need to probe
		{
			previous = current;
			current = current->next;
			while (current)
				// if the current is not empty, check the content of it
				if (top_child->sequence_number == current->sequence_number)
				{	// current should be removed
					// first, push all the new children to the stack for the future removal
					for(int i = current->cached_child_list.size() - 1; i >= 0; --i)
					entry_to_remove_stack.push_back(current->cached_child_list[i]);
					// second, update the chain
					previous->next = current->next;
#ifndef APPROXIMATE_HASHING
					//formula & f = current->f;
					//while (!f.empty())
					//{
					//	delete f.back();
					//	f.pop_back();
					//}
#endif
					delete current;
					++num_removed_entries;
					break;
				}
				else
				{
					previous = current;
					current = current->next;
				}
		}	// end else
		delete top_child;
	}	// end while (!entry_to_remove_stack.empty())
	return true;
}











