#include "GaussMixture.h"
#include "RandSource.h"
using namespace std;

double GaussMixture::evaluate(double x) const
{
    double result = 0.0;
    for (int i = 0; i < gauss.size(); ++i)
        result += gauss[i].evaluate(x) * weights[i];

    return result;
}

double GaussMixture::sample() const
{
    double sum = 0.0;
    for (int i = 0; i < weights.size(); ++i)
        sum += weights[i];

    int index = 0;
    double draw = randf() * sum, curr = 0.0;

    while (curr < draw && index < gauss.size()) {
        curr += weights[index];
        index++;
    }
    --index;

    return gauss[index].sample();
}
