#ifndef __HISTORY_H
#define __HISTORY_H

#include "Action.h"
#include "Obs.h"
#include <deque>
#include <climits>

class History {
  public:
    /**
       Constructor. Compute the hash codes for action and observation
       deque

       @param[in] acts  A deque of actions
       @param[in] obses A deque of observations
    */
    History(deque<Action> acts, deque<Obs> obses): acts(acts), obses(obses)
    {
        actHash = obsHash = 0;
        for (int i=0; i < acts.size(); i++)
            actHash = (actHash * largePrime + acts[i]) % ULONG_MAX;

        for (int i=0; i < obses.size(); i++)
            obsHash = (obsHash * largePrime + obses[i]) % ULONG_MAX;
    }

    History() {}

    // Do we ever need to compare 2 histories??
    /**
       Compare this and another history

       @param[in] his  Another history
    */
    inline operator==(History& his) const
    {
        if (actHash != his.actHash || obsHash != his.obsHash)
            return false;

        if (acts.size() != his.acts.size() || obses.size() != his.obses.size())
            return false;

        for (int i=0; i < acts.size(); i++)
            if (acts[i] != his.acts[i]) return false;

        for (int i=0; i < obses.size(); i++)
            if (obses[i] != his.obses[i]) return false;

        return true;
    }

    /**
       Add an action into the action deque

       @param[in] act An action to be added
    */
    inline void add(Action act)
    {
        acts.push_back(act);
        actHash = (actHash * largePrime + act.actNum) % modulo;
    }

    /**
       Add an observation into the observation deque

       @param[in] obs An observation to be added
    */
    inline void add(Obs obs)
    {
        obses.push_back(obs);
        obsHash = (obsHash * largePrime + obs.hashCode) % modulo;
    }

    /**
       Prune the prefix and compute the new hash codes

       @param[in] length The length of the prefix to be pruned
     */
    inline void prefixPrune(int length)
    {
        int remain = length % 2;
        length /= 2;
        // assert(length < acts.size() && length < obses.size());
        unsigned long aHash = 0, oHash = 0;

        for (int i=0; i < length; i++) {
            aHash = (aHash * largePrime + acts[0]) % modulo;
            oHash = (oHash * largePrime + obses[0]) % modulo;
            acts.pop_front();
            obses.pop_front();
        }

        if (remain) {
            aHash = (aHash * largePrime + acts[0]) % modulo;
            acts.pop_front();
        }

        actHash -= aHash * powerModulo(largePrime, acts.size() - length, modulo);
        oHash -= oHash * powerModulo(largePrime, obses.size() - length, modulo);
    }

    /**
       Clear the deques
    */
    void clear() {
        acts.clear();
        obses.clear();
        actHash = obsHash = 0;
    }

    /**
       Return the length of the history
    */
    int size()
    {
        return acts.size() + obses.size();
    }

    /**
       x ^ power % modulo
    */
    static unsigned long powerModulo(long x, long power, unsigned long modulo)
    {
        x %= modulo;
        if (power % 2)
            return powerModulo(x*x, power/2, modulo);
        return (x * powerModulo(x*x, power/2, modulo)) % modulo;
    }

  private:
    deque<Action> acts;
    deque<Obs> obses;
    const static long largePrime = 105143;
    const static unsigned long modulo = 36028797018963913;
}

#endif
