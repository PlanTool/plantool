#ifndef __GAUSSMODEL_H
#define __GAUSSMODEL_H

#include <vector>
#include "RandSource.h"
#include "Gauss.h"
#include "GaussMixture.h"

class GaussModel
{
  public:
    GaussModel(std::vector<GaussMixture> gauss_mixture, std::vector<double> weights):
            gauss_mixture(gauss_mixture),
            weights(weights)
    {}

    GaussModel() {}

    void setMixture(std::vector<GaussMixture> const& gauss_mixture,
                    std::vector<double> const& weights)
    {
        this->gauss_mixture = gauss_mixture;
        this->weights = weights;
    }

    // draw a sample index of a gauss mixture at x using the uniform
    // distribution. Assume the sum of all the gauss_mixture is the
    // constant function f(x) = 1
    int sample(double x, RandStream* rand_stream);

    std::vector<GaussMixture> gauss_mixture;
    std::vector<double> weights;
};
#endif
