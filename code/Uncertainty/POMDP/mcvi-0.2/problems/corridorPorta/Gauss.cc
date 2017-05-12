#include <cmath>
#include <cstdlib>
#include "Gauss.h"
#include "Utils.h"
using namespace std;

Gauss::Gauss(double const mean, double const variance):
        mean(mean), variance(variance),
        deviation(sqrt(variance)),
        factor(sqrt(2 * M_PI * variance))
{}

double Gauss::evaluate(double x) const
{
    return exp(-(x - mean) * (x-mean) / (2 * variance)) / factor;
}

double Gauss::sample(RandStream* rand_stream) const
{
    double result, x1, x2, w;

    do {
        x1 = 2 * rand_stream->getf() - 1;
        x2 = 2 * rand_stream->getf() - 1;
        w = x1 * x1 + x2 * x2;
    } while (w == 0 || w >= 1);

    sample(x1, x2, w,
           &result, &result);
    return result;
}

double Gauss::sample() const
{
    double result, x1, x2, w;

    do {
        x1 = 2 * randf() - 1;
        x2 = 2 * randf() - 1;
        w = x1 * x1 + x2 * x2;
    } while (w == 0 || w >= 1);

    double dummy;
    sample(x1, x2, w, &result, &dummy);

    return result;
}

void Gauss::sample(double x1, double x2, double w,
                   double* normal1, double* normal2) const
{
    w = sqrt( -2 * log(w) / w);
    double y1 = x1 * w, y2 = x2 * w;

    *normal1 = deviation * y1 + mean;
    *normal2 = deviation * y2 + mean;
}
