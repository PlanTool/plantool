// Macro guard
#ifndef TIMER_CSE473_H
#define TIMER_CSE473_H

// Files included
#include "Assertion.h"
#include "CommonErrors.h"
#include "StandardFiles.h"

// Unix-specific
#include <sys/times.h>


/*============================================================================//
    Timer.h: Quick implementation of a stop watch.

    Purpose is to give real, user and system times.  Start timer using Start().
     Stop timer using Stop().

    Implementation: Shane J. Neph, June 2004, University of Washington
//============================================================================*/


struct Timer {
    Timer() { Reset(); }


    static std::string CurrentTime() {    
        std::time_t t = std::time(0);
        char buf[20];
        std::strftime(buf, 20, "%X", std::localtime(&t));
        std::stringstream s(buf);
        return(s.str());
    }

    static std::string CurrentDate() {
        std::time_t t(0);
        static std::string err = "Error retrieving Date";
        bool check = (std::time(&t) != std::time_t(-1));
        Assert<CE::RunTimeError>(check, err);
        static tm* gt = std::gmtime(&t);
        std::stringstream s;
        s << gt->tm_mon+1 << '/' << gt->tm_mday << '/' << 1900+gt->tm_year;
        return(s.str());
    }

    bool IsTiming() const {
        return(timing_);
    }

    double RealTime() const {
        using namespace std;
        if ( !timing_ ) return(std::difftime(totalStop_, totalStart_) / 100);
        tms tm;
        return(std::difftime(times(&tm), totalStart_) / 100); 
    }

    double SystTime() const {
        if ( !timing_ ) return(std::difftime(sysStop_, sysStart_) / 100);
        using namespace std;
        tms tm;
        times(&tm);
        std::clock_t tmp = tm.tms_stime + tm.tms_cstime;
        return(std::difftime(tmp, sysStart_) / 100);
    }

    double UserTime() const {
        if ( !timing_ ) return(std::difftime(usrStop_, usrStart_) / 100);
        using namespace std;
        tms tm;
        times(&tm);
        std::clock_t tmp = tm.tms_utime + tm.tms_cutime;
        return(std::difftime(tmp, usrStart_) / 100);
    }

    void Reset() {
        timing_ = false;
        totalStart_ = 0;
        totalStop_ = 0;
        usrStart_ = 0;
        usrStop_ = 0;
        sysStart_ = 0;
        sysStop_ = 0;
    }    

    void Start() {
        using namespace Exc;
        Assert<LogicError>(!timing_, "Can't start watch when timing already");
        Reset();
        timing_ = true;
        using namespace std;
        tms tm;
        totalStart_ = times(&tm);
        usrStart_ = tm.tms_utime + tm.tms_cutime;
        sysStart_ = tm.tms_stime + tm.tms_cstime;
    }

    void Stop() {
        using namespace Exc;
        Assert<LogicError>(timing_, "Can't stop watch when not even timing");
        timing_ = false;
        using namespace std;
        tms tm;
        totalStop_ = times(&tm);
        usrStop_ = tm.tms_utime + tm.tms_cutime;
        sysStop_ = tm.tms_stime + tm.tms_cstime;
    }

    friend std::ostream& operator<<(std::ostream& os, const Timer& t) {
        os << std::endl;
        os << "Real Time: = " << t.RealTime() << std::endl;
        os << "User Time: = " << t.UserTime() << std::endl;
        os << "Syst Time: = " << t.SystTime() << std::endl;
        return(os);
    }

private:
    typedef CE::LogicError LogicError;


private:
    bool timing_;
    std::clock_t totalStart_;
    std::clock_t totalStop_;
    std::clock_t usrStart_;
    std::clock_t usrStop_;
    std::clock_t sysStart_;
    std::clock_t sysStop_;
};


#endif // TIMER_CSE473_H
