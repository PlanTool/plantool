#ifndef __UTILS_H
#define __UTILS_H

#include <cmath>
#include <cstdlib>
#include <vector>
#include <iostream>
#include <limits>

const double epsilon = 1e-5;

const long Never = -1;
#define Inf (std::numeric_limits<double>::infinity())
#define NegInf (-(std::numeric_limits<double>::max()-1))

#ifdef DEBUG
#define safe_cast dynamic_cast
#else
#define safe_cast static_cast
#endif

inline double power(double x, long i)
{
    if (i==0) return 1;
    double temp = power(x,i/2);
    if (i % 2 == 0)
        return temp * temp;
    else
        return temp * temp * x;
}

inline bool equal(double x, double y)
{
    return (fabs(y - x) <= epsilon);
}

#endif
