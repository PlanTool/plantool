//#include "zchaff_base.h"
#include "stdio.h"
#include "gmp.h"

class BigNum
{
public:
	mpz_t numerator;
	unsigned denominator;	// exponent of 2
	bool zero_flag;			// zero_flag == 1 if the big_num is 0

	BigNum()
	{
		mpz_init(numerator);
		//mpz_init2(numerator, 512);
		mpz_set_ui(numerator, 0);
		denominator = 0;
		zero_flag = true;
	}
	BigNum(unsigned long numer, unsigned denom, bool zero)
	{
		zero_flag = zero;
		mpz_init(numerator);
		//mpz_init2(numerator, 512);
		mpz_set_ui(numerator, numer);
		denominator = denom;
	}
	BigNum(mpz_t & numer, unsigned denom, bool zero)
	{
		zero_flag = zero;
		mpz_init(numerator);
		//mpz_init2(numerator, 512);
		mpz_set(numerator, numer);
		denominator = denom;
	}

	~BigNum()
	{
		mpz_clear(numerator);
	}

	static void add(BigNum & r, BigNum & x, BigNum & y);
	static void mul(BigNum & r, BigNum & x, BigNum & y);
};


