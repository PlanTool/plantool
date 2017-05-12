#ifndef __OBS_H
#define __OBS_H

#include <vector>

class Obs
{
  public:
    explicit Obs(std::vector<long> obs);
    Obs(Obs const& other);
    Obs();
    virtual ~Obs() {}

    bool operator==(const Obs& obs) const;
    bool operator<(const Obs& obs) const;

    int compare(const Obs& obs) const;

    void computeHash();

    std::vector<long> obs;
    unsigned long hashCode;

    // Should we just use public, like hashCode is public
  private:
    // static member of type integral (int, char, byte) can be
    // initialized in the declaration
    const static long largePrime = 105907;
    const static unsigned long modulo = 179426549;
};

#endif
