#ifndef __RANDSOURCE_H
#define __RANDSOURCE_H

#include <cstdlib>
#include <vector>
#include "Utils.h"

const double rand_max = ((double)RAND_MAX + 1);

inline double randf() {
    return (double)rand() / rand_max;
}

class RandSource;

class RandStream
{
public:
    RandStream():
            seed(0)
    {}

    void initseed(unsigned int num) {
        #pragma omp critical
        {
            seed=num;
        }
    }

    unsigned get()
    {
        unsigned int result;
        #pragma omp critical
        {
            result = rand_r(&seed);
        }
        return result;
    }

    inline double getf()
    {
        return (get() / rand_max);
    }

    friend class RandSource;
  private:
    unsigned int seed;
};

/**
   @class RandSource
   @brief Source of random numbers
   @details Generates streams of random numbers that are then reused.

   @author Wee Sun Lee
   @date 26 October 2009
*/
class RandSource
{
 public:
    RandSource(long numStream): numStream(numStream)
    {
        seeds.resize(numStream);
        for(int i=0; i < numStream; ++i)
            sources.push_back(RandStream());
        currStream = 0;
        currNum = 0;
    }

    void initseed(unsigned seed)
    {
        #pragma omp parallel for
        for(int i=0; i<numStream; ++i) {
            seeds[i] = (seed ^ i);
            sources[i].initseed(seeds[i]);
        }
    }

    inline unsigned get() {
        // not thread-safe
        ++currNum;
        return sources[currStream].get();
    }

    inline double getf()
    {
        return ((double)get() / RAND_MAX);
    }

    inline void startStream(long streamNum)
    {
        // not thread-safe
        currNum = 0;
        currStream = streamNum;
        sources[streamNum].initseed(seeds[streamNum]);
    }

    inline void setStreamPos(long streamNum, long numPos)
    {
        startStream(streamNum);
        for (long i=0; i < numPos; i++)
            sources[streamNum].get();
        currNum = numPos;
    }

    inline long getStreamNum() { return currStream; };
    inline long getPosInStream() { return currNum; };

    inline RandStream& getStream(long streamNum, long numPos)
    {
        sources[streamNum].initseed(seeds[streamNum]);
        for (long i = 0; i < numPos; i++)
            sources[streamNum].get();
        return sources[streamNum];
    }

    inline RandStream& getStream(long streamNum)
    {
        return sources[streamNum];
    }


    inline operator RandStream&()
    {
        return sources[currStream];
    }

    inline void reset()
    {
        currStream = 0;
        currNum = 0;
    }

    friend class RandStream;
 private:
    long numStream;
    long currStream;
    size_t currNum;
    std::vector<RandStream> sources;
    std::vector<unsigned> seeds;
};
#endif //  __RANDSOURCE_H
