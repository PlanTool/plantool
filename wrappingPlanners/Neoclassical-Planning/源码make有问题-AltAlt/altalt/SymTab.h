/*
* $Revision: 1.2 $
* $Date: 1999/06/16 13:34:44 $
*/
#ifndef __SymTab
#define __SymTab

#include "switches.h"

#define cnv(c) (int) c>=0?((int) c == 0?128:(int) c):(int) c + 256
#define BIGPRIME 8000977

class symbol_table {
private:
	char * entries[256];
	int num_entries;

	inline unsigned int hash(char *);

public: char enter(char *);
	symbol_table();
	void showsym(char);
	int occupied(int);
	char * getsym(char);
	char newenter(char *);
	void wipe();
};

extern symbol_table statetab;
extern symbol_table symtab;


#endif
