#ifndef __GAUSS_H
#define __GAUSS_H

#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

#include "RandSource.h"

class Gauss
{
  public:
    Gauss() {}
    Gauss(double const mean, double const variance);

    // Evaluate the Gaussian function at x
    double evaluate(double x) const;

    // Draw a sample from this distribution. We have 2 samples and
    // discard 1 for the purpose of consistency with the use of
    // rand_stream
    double sample(RandStream* rand_stream) const;
    // Draw a sample from this distribution. Return 1 sample and don't
    // cache the other one
    double sample() const;
    void sample(double x1, double x2, double w,
                double* normal1, double* normal2) const;

    double mean, variance, deviation, factor;
};
#endif
