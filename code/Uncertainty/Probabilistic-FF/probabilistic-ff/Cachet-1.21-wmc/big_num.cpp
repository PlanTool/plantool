#include "big_num.h"
#include <iostream>

void BigNum::add(BigNum & r, BigNum & x, BigNum & y)	// result of x + y will be stored in r, x and y will be changed
{
	//r.denominator = x.denominator + y.denominator;
	if (x.denominator >= y.denominator)
	{
		r.denominator = x.denominator;
		mpz_mul_2exp(y.numerator, y.numerator, x.denominator - y.denominator);
		
	}
	else
	{
		r.denominator = y.denominator;
		mpz_mul_2exp(x.numerator, x.numerator, y.denominator - x.denominator);
	}
	mpz_add(r.numerator, x.numerator, y.numerator);
	r.zero_flag = false;	// any result should be > 0

	unsigned index = 0;
	while(!mpz_tstbit(r.numerator, index))	// mpz_tstbit(r.numerator, index) == 0, test 0 bit by bit
		++index;
	if (index > 0)	// reduce numerator and denominator as much as possible
	{
		mpz_tdiv_q_2exp(r.numerator, r.numerator, index);
		r.denominator = r.denominator - index;
	}
	
	return;
}

void BigNum::mul(BigNum & r, BigNum & x, BigNum & y)	// result of x * y will be stored in x, r is unused
{
	/*cout << "x.numerator = "; 
	mpz_out_str(stdout, 10, x.numerator);
	cout << ", y.numerator = "; 
	mpz_out_str(stdout, 10, y.numerator); 
	cout << endl;
	cout << "x.denominator = " << x.denominator;
	cout << ", y.denominator = " << y.denominator << endl;

	double dx = mpz_get_d(x.numerator);
	for (int k = 0; k < x.denominator; ++k)
		dx *= 0.5;
	double dy = mpz_get_d(y.numerator);
	for (int k = 0; k < y.denominator; ++k)
		dy *= 0.5;*/

	//r.denominator = x.denominator + y.denominator;
	x.denominator = x.denominator + y.denominator;
	//mpz_mul(r.numerator, x.numerator, y.numerator);
	mpz_mul(x.numerator, x.numerator, y.numerator);
	//r.zero_flag = false;	// any result should be > 0
	x.zero_flag = false;	// any result should be > 0

	/*cout << "r.numerator = "; 
	mpz_out_str(stdout, 10, r.numerator);
	cout << ", r.denominator = " << r.denominator << endl;

	double dr = mpz_get_d(r.numerator);
	for (int k = 0; k < r.denominator; ++k)
		dr *= 0.5;
	cout << "result to double = " << dr << endl;

	cout << "dx * dy = " << dx*dy << endl;*/

	return;
}


/*int main(int argc, char** argv)
{
	BigNum a;
	BigNum b(3, 4, false); 
	BigNum c(2, 3, false);

	mpz_set(b.numerator, c.numerator);
	mpz_add_ui(b.numerator, b.numerator, 10);

	cout << "b's numerator = ";
	mpz_out_str (stdout, 10, b.numerator);
	cout << endl;

	cout << "c's numerator = ";
	mpz_out_str (stdout, 10, c.numerator);
	cout << endl;

	mpz_add_ui(c.numerator, c.numerator, 1);

	cout << "b's numerator = ";
	mpz_out_str (stdout, 10, b.numerator);
	cout << endl;

	cout << "c's numerator = ";
	mpz_out_str (stdout, 10, c.numerator);
	cout << endl;

	//x.add(x,y);
	/*BigNum::add(a, b, c);

	cout << "a's numerator = ";
	mpz_out_str (stdout, 10, a.numerator);
	cout << ", a's denominator = " << a.denominator << endl;

	cout << "b's numerator = ";
	mpz_out_str (stdout, 10, b.numerator);
	cout << ", b's denominator = " << b.denominator << endl;

	cout << "c's numerator = ";
	mpz_out_str (stdout, 10, c.numerator);
	cout << ", c's denominator = " << c.denominator << endl;

	BigNum::mul(a, b, c);

	cout << "a's numerator = ";
	mpz_out_str (stdout, 10, a.numerator);
	cout << ", a's denominator = " << a.denominator << endl;

	mpz_clear(a.numerator);
	mpz_clear(b.numerator);
	mpz_clear(c.numerator);
	*/
	//return 0;
//}


