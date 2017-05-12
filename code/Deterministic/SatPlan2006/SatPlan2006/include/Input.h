// Macro Guard
#ifndef INPUT_CSE473_H
#define INPUT_CSE473_H

// Files included
#include "Assertion.h"
#include "CommonErrors.h"
#include "Conversion.h"
#include "GeneralSolver.h"
#include "SolverInterface.h"
#include "SolverTraits.h"
#include "StringAlgorithms.h"


/*============================================================================//
    Input.h: header file for Input UDT

    Purpose is to process the user inputs and provide defaults when certain
     inputs are left undefined.  Static member function Revision() maintains
     the revision of the entire SatPlan program.

    NOTE: Use to 'setrlimit' for global time here, but it seemed to interfere 
          somewhat with ulimit settings in other parts of the program.  Now,
          SatPlan checks TimeAllotted() against user time.  This method works
          because we do not fork() other processes any longer (we use system()
          calls).  However, it is now possible for our program to run up to
          (-globaltime + -timeout) instead of throwing at -globaltime.  The
          good news is that we will still catch the problem ( > -globaltime )
          and report a program timeout...bad news is that we may run a little
          longer than we wanted to.  Some constraints on the ratio of -timeout
          as compared to -globaltime have been added to reduce this problem.

    Implementation: Shane J. Neph, June 2004, University of Washington
//============================================================================*/


//=======
// Input
//=======
struct Input {
    typedef CE::Help HelpRequested;
    typedef Exc::Exception<1001> MakeList;
    typedef Exc::Exception<1002> NoSolvers;

    enum Style { ACTION = 1, GPSTYLEACTION = 2, GPBASED = 3, THINGPBASED = 4 };

    // Constructor
    Input(int argc, char* argv[]) : domain_(""), problem_("") {
        std::string empty("");
        params_.insert(std::make_pair(std::string("-bcheck"), empty));
        params_.insert(std::make_pair(std::string("-cnf"), empty));
        params_.insert(std::make_pair(std::string("-cnfonly"), empty));
        params_.insert(std::make_pair(std::string("-domain"), empty));
        params_.insert(std::make_pair(std::string("-encoding"), empty));
        params_.insert(std::make_pair(std::string("-globalmemory"), empty));
        params_.insert(std::make_pair(std::string("-globaltime"), empty));
        params_.insert(std::make_pair(std::string("-level"), empty));
        params_.insert(std::make_pair(std::string("-listoptions"), empty));
        params_.insert(std::make_pair(std::string("-listsolvers"), empty));
        params_.insert(std::make_pair(std::string("-maxlevel"), empty));
        params_.insert(std::make_pair(std::string("-options"), empty));
        params_.insert(std::make_pair(std::string("-path"), empty));
        params_.insert(std::make_pair(std::string("-problem"), empty));
        params_.insert(std::make_pair(std::string("-restart"), empty));
        params_.insert(std::make_pair(std::string("-seed"), empty));
        params_.insert(std::make_pair(std::string("-solution"), empty));
        params_.insert(std::make_pair(std::string("-solver"), empty));
        params_.insert(std::make_pair(std::string("-timeout"), empty));
        params_.insert(std::make_pair(std::string("-verbose"), empty));

        if ( 0 == argc ) // dummy Input object constructed
            return;
          
        // check user input and set resources
        checkInput(argc, argv);
        setMemory();
    }

    // Public Member Functions
    std::string AllInputs(const std::string& add = "") const {
        std::string toRtn;
        std::map<std::string, std::string>::const_iterator i = params_.begin();
        while ( i != params_.end() ) {
            if ( !i->second.empty() )
                toRtn += (add + " " + i->first + " " + i->second + "\n");
            ++i;
        }
        return(toRtn);
    }
    bool BinaryCheck() const {
        return(bCheck_);
    }
    std::string CNF() const {
        return(cnf_);
    }
    bool CNFOnly() const {
        return(cnfOnly_);
    }
    std::string Domain() const {
        return(domain_);
    }
    Style Encoding() const {
        return(encoding_);
    }
    std::string ExeDirectory() const {
        return(exeDir_);
    }
    bool FullVerbose() const {
        return(makeNoise_);
    }
    bool KeepCNF() const {
        std::string cnfTmp = "CNF_" + convert<std::string>(getpid());
        for ( std::string::size_type idx = 0; idx < cnfTmp.size(); ++idx ) {
            if ( isdigit(cnfTmp[idx]) )
                cnfTmp[idx] = 'a' + (cnfTmp[idx] - '0');
        } // for
        return((cnf_ != cnfTmp) && !cnfOnly_);
    }
    long Level() const {
        return(level_);
    }
    long LevelTimeOut() const {
        return(restartTime_); 
    }
    long MaxLevel() const {
        return(maxLevel_); 
    }
    std::string Path() const {
        return(path_);
    }
    std::string Problem() const {
        return(problem_);
    }
    SolverTraits::FactoryType::IDType Program() const {
        return(satSolver_);
    }
    std::string ProgramOptions() const {
        return(options_);
    }
    std::string ProgramRevision(const std::vector<std::string>& revs) const {
        std::vector<std::string>::const_iterator i = revs.begin();
        while ( i != revs.end() ) {
            if ( i->find(satSolver_) != std::string::npos )
                return(*i);
            ++i;
        }
        return("unknown revision");
    }
    long Rand() const {
        return(rand());
    }
    long Restarts() const {
        return(restarts_);
    }
    static std::string Revision() {
        static const std::string rev = "1.1";
        return(rev);
    }
    static void ShowHelp(std::ostream& os) {
        std::string place = "University of Washington";
        typedef SolverTraits::FactoryType FT;
        typedef SolverTraits::SolverFactoryType SFT;

        std::string mark = "**************************************************";
        os << std::endl << std::endl << mark << std::endl;
        os << "SatPlan Revision: " << Revision() << ", " << place 
           << std::endl << std::endl;
        os << "[Registered Solvers:]" << std::endl;
        std::set<FT::IDType> allSolvers = SFT::Instance()->GetIDs();
        if ( allSolvers.find(GeneralSolver::Name()) != allSolvers.end() )
            allSolvers.erase(allSolvers.find(GeneralSolver::Name()));
        std::copy(allSolvers.begin(), allSolvers.end(),
                  std::ostream_iterator<FT::IDType>(os, "\n"));
        os << "\n*Note: if a binary is unavailable, then a" << std::endl;
        os << "       registered solver will fail to run." << std::endl;
        os << mark << std::endl << std::endl;

        Assert<NoSolvers>(!allSolvers.empty(), "No Sat Solvers registered???");
    }
    std::string Solution() const {
        return(solution_);
    }
    long TimeAlloted() const {
        return(time_);
    }
    std::string VarFile() const {
        return(varFile_); // bb produces this file
    }
    friend std::ostream& operator<<(std::ostream& os, const Input&) {
        os << "==================================================="<< std::endl;
        os << "Usage: [...] is optional (unless args to -options)" << std::endl;
        os << "==================================================="<< std::endl;
        os << "-problem X (:string - problem filename)"            << std::endl;
        os << "-domain X (:string - domain filename)"              << std::endl;
        os << "[-bcheck X (:bool 1 means additional checks)]"      << std::endl;
        os << "[-cnf X (:string - CNF encoding output filename)]"  << std::endl;
        os << "[-cnfonly X (:bool - 1 means don't solve)]"         << std::endl;
        os << "[-encoding X (:integer - 1 to 4)]"                  << std::endl;
        os << "[-globalmemory X (:integer - bytes)]"               << std::endl;
        os << "[-globaltime X (:integer - minutes)]"               << std::endl;
        os << "[-level X (:integer - try this plan level only)]"   << std::endl;
        os << "[-maxlevel X (:integer - try <= this plan level)]"  << std::endl;
        os << "[-options [ X ] (:list - direct input to -solver)]" << std::endl;
        os << "[-path X (:string - problem and domain directory)]" << std::endl;
        os << "[-restart X (:integer - #tries to solve at level)]" << std::endl;
        os << "[-seed X (:integer - for rand())]"                  << std::endl;
        os << "[-solution X (:string - solution filename)]"        << std::endl;
        os << "[-solver X (:string - sat solver name)]"            << std::endl;
        os << "[-timeout X (:integer - seconds before new rand())]"<< std::endl;
        os << "[-verbose X (:bool - 0 means OFF, 1 means ON)]"     << std::endl;

        os << "==================================================="<< std::endl;
        os << "                  Helpful Items:"                   << std::endl;
        os << "==================================================="<< std::endl;
        os << "[-listsolvers (revision and all available solvers)]"<< std::endl;
        os << "[-listoptions (all input options for -solver)]"     << std::endl;
        return(os);
    }

private:
    void checkInput(int argc, char* argv[]);
    void listAllSolverOptions();
    void makeConversions(bool enforce = true);
    void setMemory();
    std::string unknownUsage(const std::string& str);

private:
    bool makeNoise_;
    long seed_, time_, mem_, restartTime_, restarts_, level_, maxLevel_;
    std::string cnf_, domain_, problem_, solution_, path_, tmpFile_;
    SolverTraits::FactoryType::IDType satSolver_;
    std::map<std::string, std::string> params_;  
    std::string options_, varFile_, exeDir_;
    bool cnfOnly_, bCheck_;
    Style encoding_;
};

#endif // INPUT_CSE473_H

