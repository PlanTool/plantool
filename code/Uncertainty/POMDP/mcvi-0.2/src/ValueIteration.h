#ifndef __VALUEITERATION_H
#define __VALUEITERATION_H

#include "Utils.h"
#include <vector>
#include <string>

/**
   @class ValueIteration
   @brief Simple value iteration for MDPs.
   @details
   @author Wee Sun Lee
   @date 26 October 2009
*/
class ValueIteration
{
  public:
    ValueIteration(long numStates, long numActions, double discount): numStates(numStates), numActions(numActions), discount(discount) {};

    ValueIteration(long numActions, double discount): numActions(numActions), discount(discount) {};

    void doValueIteration(std::vector<std::vector<double> >& rewardMatrix, std::vector<std::vector<std::vector<std::pair<long,double> > > >& transMatrix, double targetPrecision, long displayInterval);

    std::vector<double> values;
    std::vector<long> actions;

    /**
       Write out the policy \a filename
    */
    void write(std::string filename);

    /**
       Reads in what has be written out with \a write
    */
    void read(std::string filename);

  private:
    long numStates;
    long numActions;
    double discount;

};

#endif // __VALUEITERATION_H
