// Files included
#include "ByLine.h"
#include "Input.h"
#include "PreferredSource.h"

// unix-specific Files included
#include <sys/resource.h>


/*============================================================================//
    Input.cpp: Implementation of Input.h.

    Implementation: Shane J. Neph, June 2004, University of Washington
//============================================================================*/

//==============
// checkInput()
//==============
void Input::checkInput(int argc, char* argv[]) {
    // Assert some things; stuff applicable values in params_
    using StringAlgs::Trim;
    using CE::Usage;
    typedef std::map<std::string, std::string>::iterator Iter;

    // Define the path to the directory where this program resides
    exeDir_ = argv[0];
    std::string::size_type pos = exeDir_.rfind("/");
    if ( pos != std::string::npos )
        exeDir_ = exeDir_.substr(0, ++pos);
    else { // search PATH environment variable for this executable
        char* pathInfo = getenv("PATH");
        Assert<CE::ProgramError>(pathInfo != NULL,
                                 "No value found for 'PATH' env var?");
        bool done = false;
        std::string info(pathInfo);
        std::vector<std::string> allPaths = StringAlgs::SplitString(info, ':');
        std::vector<std::string>::iterator i = allPaths.begin();
        while ( i != allPaths.end() ) { // keep looking for this directory
            std::string nextTry = *i;
            if ( nextTry[nextTry.size()-1] != '/' )
                nextTry += '/';
            std::string nameAndDir = nextTry + std::string(argv[0]);
            std::ifstream findIt(nameAndDir.c_str());
            if ( findIt ) {
                exeDir_ = nextTry;
                done = true;
                break;
            }
            ++i;
        }
        Assert<CE::ProgramError>(done, "Cannot locate 'this' exe directory?");
    }

    bool listAll = false, getOptions = false;
    for ( long idx = 1; idx < argc; ) {
        // Option
        std::string nextA = Trim(std::string(argv[idx]));
        bool dummy = (nextA[0] == '-');
        Assert<Usage>(dummy, "Expected -option, received: " + nextA);
        Iter iter = params_.find(nextA);
        Assert<Usage>(iter != params_.end(), "Unknown option: " + nextA);
        Assert<Usage>(iter->second.empty(), "more than 1 def'n: " + nextA);
        Assert<HelpRequested>(iter->first != "-listsolvers", "Help Requested");
        if ( iter->first == "-listoptions" ) {
            /* list all -solver options... later */
            listAll = true;
            ++idx;
            continue;
        } 
        else if ( iter->first == "-options" )
            getOptions = true;

        // Value
        Assert<Usage>(++idx < argc, "No value found for: " + nextA);
        std::string nextB = Trim(std::string(argv[idx]));
        Assert<Usage>(!nextB.empty(), "No value found for: " + nextA);
        dummy = (nextB[0] != '-');
        Assert<Usage>(dummy, "No value found for option: " + nextA);

        iter->second = nextB;
        ++idx;

        if ( getOptions ) { // get stuff in between [...]
            /* directly specified options for -solver */
            getOptions = false;
            dummy = (nextB[0] == '[');
            Assert<Usage>(dummy, "-options must begin with '['");
            if ( 1 != nextB.size() )
                iter->second = iter->second.substr(1);
            else
                iter->second = "";
           
            bool done = false, found = false;
            if ( nextB.find("]") != std::string::npos ) {
                dummy = (nextB[nextB.size()-1] == ']');
                Assert<Usage>(dummy, "-options must end with ']'");
                nextB = iter->second.substr(0, iter->second.size()-1);
                iter->second = Trim(nextB);
                done = true;
                found = true;
            }
            while ( (idx < argc) && !done ) {
                nextB = Trim(std::string(argv[idx]));
                iter->second += std::string(" ") + nextB;
                if ( nextB.find("]") != std::string::npos ) {
                    dummy = (nextB[nextB.size()-1] == ']');
                    Assert<Usage>(dummy, "-options must end with ']'");
                    nextB = iter->second.substr(0, iter->second.size()-1);
                    iter->second = nextB;
                    ++idx;
                    found = true;
                    break;
                }
                ++idx;
            } // while
            Assert<Usage>(found, "-options must end with ']'");
        }
    }
    makeConversions(!listAll);
    if ( listAll )
        listAllSolverOptions();
}

//========================
// listAllSolverOptions()
//========================
void Input::listAllSolverOptions() {
    // -listalloptions
    typedef SolverTraits::SolverFactoryType SFT;
    std::auto_ptr<SolverInterface> ptr(0);
    try {
        ptr.reset(SFT::Instance()->CreateObject(satSolver_));
    } catch(CE::BadArg&) { // could not find satSolver_
        std::string cannot = "Can't -listoptions for unknown solver";
        Assert<CE::Usage>(false, "No listed -solver: " + satSolver_, cannot);
    }
    ptr->ListOptions(std::cout, exeDir_);
    Assert<MakeList>(false, "listed all -solver options");
}

//===================
// makeConversions()
//===================
void Input::makeConversions(bool enforce) {
    typedef std::map<std::string, std::string> MapType;
    typedef MapType::iterator Iter;

    using CE::Usage;
    using StringAlgs::IsInteger;

    // -solver
    bool dummy;
    Iter iter;
    iter = params_.find("-solver");
    typedef SolverTraits::FactoryType FT;
    typedef SolverTraits::SolverFactoryType SFT;
    if ( iter->second.empty() ) { // Select default solver
        std::set<FT::IDType> allSolvers = SFT::Instance()->GetIDs();
        Assert<NoSolvers>(!allSolvers.empty(), 
                          "No Solvers Registered ?!?!?", "Program Error");
        satSolver_ = PreferredSource::SelectPreferred(exeDir_);
        Assert<NoSolvers>(!satSolver_.empty(), "No Default Solver available!");
    }
    else { // Use specified solver (ensure it exists)
        satSolver_ = iter->second;
        Assert<NoSolvers>(true == PreferredSource::Exists(exeDir_, satSolver_),
                          "No binary or script found for: " + satSolver_);
    }

    // -options
    iter = params_.find("-options");
    if ( !iter->second.empty() )
        options_ = iter->second;

    // -seed
    iter = params_.find("-seed");
    if ( iter->second.empty() ) {
        seed_ = 89;
        iter->second = convert<std::string>(seed_);
    }
    else {
        Assert<Usage>(dummy = IsInteger(iter->second), 
                             "Expected +Int value for -seed");
        seed_ = convert<long>(iter->second);
        Assert<Usage>(seed_ > 0, "Expect +Int value for -seed");
    }
    srand(seed_);

    // -globaltime 
    iter = params_.find("-globaltime");
    if ( iter->second.empty() ) {
        time_ = 30*60; // min*s/min
        iter->second = convert<std::string>(time_);
    }
    else { // receive from user in minutes
        Assert<Usage>(dummy = IsInteger(iter->second), 
                             "Expected Int value for -globaltime");
        time_ = convert<long>(iter->second)*60; // min*s/min
        Assert<Usage>(time_ > 0, "Expect +Int value for -globaltime");
    }

    // -globalmemory
    iter = params_.find("-globalmemory");
    if ( iter->second.empty() ) {
        mem_ = 1073741824; // (1GB default)
        iter->second = convert<std::string>(mem_);
    }
    else {
        Assert<Usage>(dummy = IsInteger(iter->second), 
                             "Expected +Int value for -globalmemory");
        mem_ = convert<long>(iter->second);
        Assert<Usage>(mem_ > 0, "Expected +Int value for -globalmemory");
    }

    // -timeout
    iter = params_.find("-timeout");
    if ( iter->second.empty() ) {
        restartTime_ = 1800; // s
        iter->second = convert<std::string>(restartTime_);
    }
    else { // receive from user in seconds
        Assert<Usage>(dummy = IsInteger(iter->second), 
                             "Expected Int value for -timeout");
        restartTime_ = convert<long>(iter->second);
        Assert<Usage>(restartTime_ > 0, "Expected +Int value for -timeout");
    }

    // -restart
    iter = params_.find("-restart");
    if ( iter->second.empty() ) {
        restarts_ = 1;
        iter->second = convert<std::string>(restarts_);
    }
    else {
        Assert<Usage>(dummy = IsInteger(iter->second),
                             "Expected +Int value for -restart");
        restarts_ = convert<long>(iter->second);
        Assert<Usage>(restarts_ > 0, "Expected +Int value for -restart");
    }

    // -level
    iter = params_.find("-level");
    if ( iter->second.empty() )
        level_ = -1;    
    else {
        Assert<Usage>(dummy = IsInteger(iter->second),
                             "Expected Int value for -level");
        level_ = convert<long>(iter->second);
        Assert<Usage>(level_ > 0, "Expected +Int value for -level");
    }

    // -maxlevel
    iter = params_.find("-maxlevel");
    if ( iter->second.empty() ) {
        maxLevel_ = 1000; // arbitrarily large
        iter->second = convert<std::string>(maxLevel_);
    }
    else {
        Assert<Usage>(dummy = IsInteger(iter->second),
                             "Expected Int value for -maxlevel");
        maxLevel_ = convert<long>(iter->second);
        Assert<Usage>(maxLevel_ > 0, "Expected +Int value for -maxlevel");
    }
    Assert<Usage>(maxLevel_ >= level_, 
                  "Expect -maxlevel's value >= -level's value");

    // -cnf
    iter = params_.find("-cnf");
    std::string cnfTmp = "CNF_" + convert<std::string>(getpid());
    for ( std::string::size_type idx = 0; idx < cnfTmp.size(); ++idx ) {
        if ( isdigit(cnfTmp[idx]) )
            cnfTmp[idx] = 'a' + (cnfTmp[idx] - '0');
    } // for
    cnf_ = iter->second.empty() ? cnfTmp : iter->second;
    std::remove(cnf_.c_str());
    iter->second = cnf_;

    // -cnfonly
    iter = params_.find("-cnfonly");
    cnfOnly_ = iter->second.empty() ? false : (iter->second != "0");

    // -bcheck
    iter = params_.find("-bcheck");
    bCheck_ = iter->second.empty() ? false : (iter->second != "0");

    // -encoding
    iter = params_.find("-encoding");
    if ( !iter->second.empty() ) {
        Assert<Usage>(dummy = IsInteger(iter->second),
                      "Expected Int Value for -encoding");
        int tmp = convert<int>(iter->second);
        Assert<Usage>(tmp >= static_cast<int>(ACTION), "Out of range -encoding");
        Assert<Usage>(tmp <= static_cast<int>(THINGPBASED), "Out of range -encoding");
        encoding_ = static_cast<Style>(tmp);
     }
     else
         encoding_ = THINGPBASED;

    // create varFile_
    varFile_ = "VARFILE" + convert<std::string>(getpid());

    // -verbose
    iter = params_.find("-verbose");
    if ( iter->second.empty() )        
        makeNoise_ = true;
    else {
        Assert<Usage>(dummy = IsInteger(iter->second),
                             "Expected Int value for -verbose");
        makeNoise_ = (convert<long>(iter->second) != 0);
    }
 
    // -domain
    iter = params_.find("-domain");
    if ( enforce )
        Assert<Usage>(!iter->second.empty(), "must define -domain");
    domain_ = iter->second;

    // -problem
    iter = params_.find("-problem");
    if ( enforce )
        Assert<Usage>(!iter->second.empty(), "must define -problem");
    problem_ = iter->second;

    // -solution (must follow -problem)
    iter = params_.find("-solution");
    solution_ = iter->second.empty() ? problem_ + ".soln" : iter->second;
    std::string::size_type pos = solution_.rfind('/'); // problem_ relative?
    if ( pos != std::string::npos )
        solution_ = solution_.substr(++pos);
    iter->second = solution_;

    // -path
    iter = params_.find("-path");
    path_ = iter->second.empty() ? "./" : iter->second;

    if ( enforce ) { // ensure -domain and -problem make sense
        std::string topath = (path_ == "./") ? "" : path_;
        std::string tocheck1 = topath + domain_;
        std::string tocheck2 = topath + problem_;
        std::ifstream check1(tocheck1.c_str());
        std::ifstream check2(tocheck2.c_str());
        if ( !topath.empty() ) {
            Assert<Usage>(check1, "-domain not found: " + tocheck1);
            Assert<Usage>(check2, "-problem not found: " + tocheck2);
        }
        else { // search PATH environment variable for their locations
            char* pathInfo = getenv("PATH");
            if ( !check1 ) { // find domain; not in working directory
                Assert<Usage>(pathInfo != NULL,
                              "-domain not found: " + tocheck1);
                bool done = false;
                std::string info(pathInfo);
                std::vector<std::string> allPaths = 
                                        StringAlgs::SplitString(info, ':');
                std::vector<std::string>::iterator i = allPaths.begin();
                while ( i != allPaths.end() ) { // keep looking
                    std::string nextTry = *i;
                    if ( nextTry[nextTry.size()-1] != '/' )
                        nextTry += '/';
                    std::string nameAndDir = nextTry + domain_;
                    std::ifstream findIt(nameAndDir.c_str());
                    if ( findIt ) {
                        domain_ = nameAndDir;
                        tocheck1 = nameAndDir;
                        done = true;
                        break;
                    }
                    ++i;
                } // while
                Assert<Usage>(done, "-domain not found: " + tocheck1);
            }

            if ( !check2 ) { // find problem; not in working directory
                Assert<Usage>(pathInfo != NULL,
                              "-problem not found: " + tocheck2);
                bool done = false;
                std::string info(pathInfo);
                std::vector<std::string> allPaths = 
                                        StringAlgs::SplitString(info, ':');
                std::vector<std::string>::iterator i = allPaths.begin();
                while ( i != allPaths.end() ) { // keep looking
                    std::string nextTry = *i;
                    if ( nextTry[nextTry.size()-1] != '/' )
                        nextTry += '/';
                    std::string nameAndDir = nextTry + problem_;
                    std::ifstream findIt(nameAndDir.c_str());
                    if ( findIt ) {
                        problem_ = nameAndDir;
                        tocheck2 = nameAndDir;
                        done = true;
                        break;
                    }
                    ++i;
                } // while
                Assert<Usage>(done, "-problem not found: " + tocheck2);
            }
        }

        std::vector<std::string> checkAgain;
        std::ifstream checkA(tocheck1.c_str());
        std::ifstream checkB(tocheck2.c_str());
        Assert<Usage>(checkA, "-domain not found: " + tocheck1);
        Assert<Usage>(checkB, "-problem not found: " + tocheck2);
        std::copy(std::istream_iterator<ByLine>(checkA),
                  std::istream_iterator<ByLine>(),
                  std::back_inserter(checkAgain));
        Assert<Usage>(!checkAgain.empty(),
                      "-domain is directory or empty?: " + tocheck1);
        checkAgain.clear();
        std::copy(std::istream_iterator<ByLine>(checkB),
                  std::istream_iterator<ByLine>(),
                  std::back_inserter(checkAgain));
        Assert<Usage>(!checkAgain.empty(),
                      "-path is directory or empty?: " + tocheck2);
    }
}

//=============
// setMemory()
//=============
void Input::setMemory() {
    rlimit limit;
    limit.rlim_cur = mem_;
    limit.rlim_max = mem_;
    setrlimit(RLIMIT_AS, &limit);     
}

//================
// unknownUsage()
//================
std::string Input::unknownUsage(const std::string& str) {
    return("Unknown usage... " + str);
}
