/*
* $Revision: 1.2 $
* $Date: 1999/06/16 13:34:44 $
*/

#include "SymTab.h"
#include <stream.h>

symbol_table statetab;
symbol_table symtab;



void
symbol_table::wipe()
{
	for(int i = 0;i<256;i++)
		entries[i] = 0;
	num_entries = 0;
};

char *
symbol_table::getsym(char c)
{
	return entries[cnv(c)];
};


int
symbol_table::occupied(int x)
{
	return entries[x] != 0;
};

inline unsigned int
symbol_table::hash(char * nm)
{
	unsigned int h;
	for(h = 0;*nm != '\0';nm++) h = (h*256+(*nm)) % BIGPRIME;
	h = (h % 255)+1;
	if(h==128) h++;
	return h;
};
symbol_table::symbol_table()
{
	for(int i = 0;i<256;i++)
		entries[i] = 0;
	num_entries = 0;
};

char
symbol_table::enter(char * sym)
{

	unsigned int x = hash(sym);

	for(int i = 0;i<256;i++)
	  {
		if(entries[x] == 0)
		{
			entries[x] = new char[strlen(sym)+1];
			strcpy(entries[x],sym);
			num_entries++;

			return (char) x;
		};

		if(strcmp(entries[x],sym)==0)
			return (char) x;

		if(++x==256) x = 1;
	};

	cout << "Table full\n";

	exit(1);
};

char
symbol_table::newenter(char * sym)
{

	unsigned int x = hash(sym);

	for(int i = 0;i<256;i++)
	{
		if(entries[x] == 0)
		{
			entries[x] = new char[strlen(sym)+1];
			strcpy(entries[x],sym);
			num_entries++;

			return (char) x;
		};

		if(strcmp(entries[x],sym)==0)
			return '\0';

		if(++x==256) x = 1;
	};

	cout << "Table full\n";

	exit(1);
};

void 
symbol_table::showsym(char c)
{
	int x = cnv(c);
	cout << entries[x];
};



