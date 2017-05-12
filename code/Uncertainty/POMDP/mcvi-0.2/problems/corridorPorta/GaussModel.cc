#include "GaussModel.h"
using namespace std;

int GaussModel::sample(double x, RandStream* rand_stream)
{
    double cdf = rand_stream->getf(), sum = 0;
    int index = 0;

    while (sum < cdf && index < gauss_mixture.size()) {
        sum += gauss_mixture[index].evaluate(x) * weights[index];
        index++;
    }

    return index-1;
}
