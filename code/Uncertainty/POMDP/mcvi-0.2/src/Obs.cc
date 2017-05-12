#include "Obs.h"
using namespace std;

Obs::Obs(vector<long> obs): obs(obs)
{
    hashCode = 0;
    // computeHash();
}

Obs::Obs(Obs const& other): obs(other.obs)
{
    hashCode = other.hashCode;
    // computeHash();
}

Obs::Obs()
{
    hashCode = 0;
}

bool Obs::operator==(const Obs& obs) const
{
    if (obs.hashCode != this->hashCode) return false;

    for (long i = 0; i < (long)this->obs.size(); i++)
        if (this->obs[i] != obs.obs[i]) return false;

    return true;
}

bool Obs::operator<(const Obs& obs) const
{
    return (this->compare(obs) == -1);
}

int Obs::compare(const Obs& obs) const
{
    for (long i = 0; i < (long)this->obs.size(); i++) {
        if (this->obs[i] > obs.obs[i]) return 1;
        if (this->obs[i] < obs.obs[i]) return -1;
    }
    return 0;
}

void Obs::computeHash()
{
    hashCode = 0;
    for (int i=0; i < (long)obs.size(); i++)
        hashCode = (hashCode*largePrime + obs[i]) % modulo;
}
