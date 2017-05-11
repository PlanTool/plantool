// Files included
#include "Assertion.h"
#include "CNFEncoding.h"
#include "Conversion.h"
#include "CommonErrors.h"
#include "GeneralSolver.h"
#include "Input.h"
#include "PreferredSource.h"
#include "SolverTraits.h"
#include "StandardFiles.h"
#include "StringAlgorithms.h"
#include "Timer.h"
#include "Typify.h"


// unix-specific headers
#include <fcntl.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <signal.h>
#include <unistd.h>



/*============================================================================//
    SatPlan.cpp: where main() is located.

    Implementation: Shane J. Neph, June 2004, University of Washington
//============================================================================*/

namespace {

    // Function Prototypes
    void AddCNFInfo(const Input&);
    void AddTime(const std::string&, const Timer&);
    void AlwaysCleanup();
    void CheckProgramTime(const Input&, const Timer&);
    void Cleanup(const std::string&, bool finalResult = false);
    void MoveSolution(const std::string&);
    void RedirectOutput();
    std::string RoundNumber(double, long);
    bool SolveCNF(const Input&);

    typedef Exc::Exception<1> Unsolved;

    enum ReturnValues 
      { GOOD = 0, NOSOLVE = 1, NOMEMORYTIME = 2, NOGOOD = 3, BADUSAGE = 4 };

    std::string solverResults, finalSolutionFile;
    static const std::string undefined = "undefined";
    std::string exeDir = undefined;

    std::map<long, long> reducibles;
} // unnamed


//========
// main()
//========
int main(int argc, char* argv[])
{
    try {
        // Some locals
        Timer timer;
        timer.Start();        
        Input input(argc, argv);
        exeDir = input.ExeDirectory();
        bool binaryCheck = input.BinaryCheck();
        CNFEncoding encoding(input);
        solverResults = encoding.TempSolutionFile();
        finalSolutionFile = encoding.FinalSolutionFile();
        long maxLevel = input.MaxLevel();
        long idx = input.Level();
        bool firstOnly = true, result = false;
        std::string phase1 = "Abnormal condition during CNF creation phase";
        std::string phase2 = "Abnormal condition during extract solution phase";
        if ( idx < 0 ) {
            firstOnly = false;
            idx = 1;
        }
        else if ( maxLevel < idx ) 
            maxLevel = idx;

        // Clean up files from previous runs
        Cleanup(input.CNF(), true);
        // Turn output off unless otherwise specified
        if ( !input.FullVerbose() )
            RedirectOutput();
        std::cout << "---SatPlan Version: " << input.Revision() << std::endl;

        /* 
           if !firstOnly, then find optimal solution by incrementing
            plan-level from 1 until solution is found.  else, try to solve
            using specified plan-level only.
        */
        for ( ; idx <= maxLevel; ++idx ) {
            if ( firstOnly ) { // looking for specific plan-level only
                if ( idx != input.Level() ) {
                    std::cout << "Unsat" << std::endl;
                    break;
                }
            }

            // Build CNF encoding
            encoding.SetPhase1(binaryCheck);
            CNFEncoding::Status status = encoding.Run(idx);
            if ( status == CNFEncoding::UNSAT ) // Couldn't reach goal state
                continue;            
            Assert<CE::ErrorOccurred>(status == CNFEncoding::SAT, phase1);

            if ( !input.CNFOnly() ) {
                if ( binaryCheck ) { // solve using binary clauses only first
                    bool bsolved = false;
                    std::cout << std::endl << "**Binary Clause Check**"
                              << std::endl << std::endl;
                    try {
                        bsolved = SolveCNF(input);
                        if ( !bsolved ) {
                            std::cout << "***UNSAT***" << std::endl << std::endl;
                            continue; // CNF Unsat; try another layer
                        }
                        else { // build real encoding to be solved
                            encoding.SetPhase1(!binaryCheck);
                            status = encoding.Run(idx);
                            Assert<CE::ErrorOccurred>(status == CNFEncoding::SAT, phase1);
                            CheckProgramTime(input, timer);
                        }
                    } catch(CE::TimeOut&) {
                        /* this shouldn't happen in practice */
                        encoding.SetPhase1(!binaryCheck);
                        /* just move on... */
                    }
                }

                // Throw solver at real CNF sentence
                bool solved = false;
                long cntr = input.Restarts();
                Assert<CE::BadArg>(cntr > 0, "# restarts must be > 0");
                while ( cntr-- > 0 ) { // Use new random number
                    try {
                        std::cout << std::endl << "***Solving***" 
                                  << std::endl << std::endl;
                        solved = SolveCNF(input);
                        break;
                    } catch(CE::TimeOut&) {
                        /* another rand will be used when SolveCNF() is called */
                        std::cout << "Timeout: "  << input.Restarts() - cntr;
                        std::cout << " at level " << idx << std::endl;
                        CheckProgramTime(input, timer);
                    }
                } // while
                CheckProgramTime(input, timer);
                if ( !solved ) {
                    std::cout << "***UNSAT***" << std::endl << std::endl;
                    continue; // CNF Unsat: try another layer
                }

                // Re-build CNF encoding and extract final solution
                encoding.SetPhase2();
                status = encoding.Run(idx);
                Assert<CE::ErrorOccurred>(status == CNFEncoding::SAT, phase2);
                result = solved;
                break;
            }
            else { // only care about encoding
                result = true;
                break;
            }
        } // for

        std::cout << "---SatPlan Version: " << input.Revision() << std::endl;

        if ( input.CNFOnly() ) // No variable info to add
            return(GOOD);
      
        if ( input.KeepCNF() ) // Add extra info to CNF file
            AddCNFInfo(input);
        else if ( !result )
            Cleanup(input.CNF());
        Assert<Unsolved>(result, "unable to solve");

        // Display some stats
        std::cout << "***SAT!***" << std::endl << std::endl;
        std::cout << "Solved in " << idx << " layers" << std::endl;
        timer.Stop();
        std::cout << timer << std::endl;

        // Add user time to final solution
        AddTime(finalSolutionFile, timer);

        // Move and rename final solution file to user's requirements
        MoveSolution(input.Solution());

        // Clean up scratch files
        if ( !input.KeepCNF() )
            Cleanup(input.CNF());
        else
            AlwaysCleanup();
        
        return(GOOD);

    } catch(CE::Usage& u) {
        std::cerr << std::endl << u.GetMessage() << std::endl;
        char* tmp[2];
        Input cheat(0, tmp);
        std::cerr << cheat << std::endl;
        return(BADUSAGE);
    } catch(Input::MakeList&) { // thrown by Input upon user-request
        return(GOOD);
    } catch(Input::HelpRequested& hr) { // always return GOOD on help
        try {
            Input::ShowHelp(std::cout);
        } catch(Input::NoSolvers& ns) {
            std::cout << ns.GetMessage() << std::endl;
        } catch(...) { /* */ }
        return(GOOD);
    } catch(Unsolved& uns) {
        std::cout << std::endl << uns.GetMessage() << std::endl;
        AlwaysCleanup();
        return(NOSOLVE);
    } catch(CE::MemoryLimit& ml) {
        std::cerr << std::endl << ml.GetMessage() << std::endl;
        AlwaysCleanup();
        return(NOMEMORYTIME);
    } catch(CE::TimeOut& tm) {
        std::cerr << std::endl << tm.GetMessage() << std::endl;
        AlwaysCleanup();
        return(NOMEMORYTIME);
    } catch(Exc::BaseException& eb) {
        std::cerr << std::endl << eb.GetMessage() << std::endl;
    } catch(...) {
        std::cerr << std::endl << "Unknown runtime error..." << std::endl;
    }

    // Clean up on error
    AlwaysCleanup();
    return(NOGOOD);
}



namespace {

struct absComp : public std::binary_function<bool, long, long> {
    bool operator()(long a, long b) {
        return(abs(a) < abs(b));
    }
};


// AddCNFInfo()
void AddCNFInfo(const Input& input) {
    std::list<std::string> lst;    
    {
        std::ifstream varfile(input.VarFile().c_str());
        if ( varfile ) { // copy variable info to 'lst'
            std::istream_iterator<ByLine> in(varfile), eof;
            std::copy(in, eof, std::back_inserter(lst));
            varfile.close();
        }
        std::remove(input.VarFile().c_str());
        lst.push_back("\n\n\n");

        // copy current CNF contents to 'lst'
        std::ifstream ifile(input.CNF().c_str());
        Assert<CE::FileError>(ifile, "Where did the WFF file go?");
        std::istream_iterator<ByLine> inCNF(ifile), end;
        std::copy(inCNF, end, std::back_inserter(lst));

        // copy general info to 'lst'
        std::string dir = input.ExeDirectory();
        std::vector<std::string> revs = SolverInterface::GetRevisions(dir);
        lst.push_front("c User Arguments:\n" + input.AllInputs("c"));
        lst.push_front("c Solver Rev: " + input.ProgramRevision(revs));
        lst.push_front("c Solver: " + input.Program());
        lst.push_front("c SatPlan Rev: " + input.Revision());
        lst.push_front("c Domain: " + input.Domain());
        lst.push_front("c Problem: " + input.Problem());
        lst.push_front("c Time: " + Timer::CurrentTime());
        lst.push_front("c Date: " + Timer::CurrentDate());

    } // end local scope

    // copy 'lst' back to CNF file
    std::ofstream ofile(input.CNF().c_str());
    Assert<CE::FileError>(ofile, "Could not open " + input.CNF());
    std::ostream_iterator<std::string> out(ofile, "\n");
    std::copy(lst.begin(), lst.end(), out);
}

// AddTime()
void AddTime(const std::string& file, const Timer& timer) {
    typedef CE::FileError FE;
    std::list<std::string> tmp;
    { // start local scope
        std::ifstream infile(file.c_str());
        Assert<FE>(infile, "Can't add time to non-file: " + file);
        std::istream_iterator<ByLine> in(infile), eof;
        std::copy(in, eof, std::back_inserter(tmp));
        tmp.push_front("; Time " + RoundNumber(timer.UserTime(), 2));
    } // end of scope
    std::ofstream outfile(file.c_str());
    std::ostream_iterator<std::string> out(outfile, "\n");
    std::copy(tmp.begin(), tmp.end(), out);
}

// AlwaysCleanup()
void AlwaysCleanup() {
    std::remove(solverResults.c_str());
    std::remove(finalSolutionFile.c_str());
}

// CheckProgramTime()
void CheckProgramTime(const Input& in, const Timer& t) {
    Assert<CE::TimeOut>(t.UserTime() <= in.TimeAlloted(), "Program Timeout");
}

// Cleanup()
void Cleanup(const std::string& thisFile, bool finalResult) {
    AlwaysCleanup();
    std::remove(thisFile.c_str());
    if ( finalResult )
        std::remove(finalSolutionFile.c_str());
}

// MoveSolution()
void MoveSolution(const std::string& newName) {
    std::ifstream in(finalSolutionFile.c_str());
    Assert<CE::FileError>(in, "Could not open: " + finalSolutionFile);
    std::istream_iterator<ByLine> infile(in), eof;
    std::ofstream out(newName.c_str());
    Assert<CE::FileError>(out, "Could not open " + newName);
    std::ostream_iterator<std::string> outfile(out, "\n");
    std::copy(infile, eof, outfile);
}

// RedirectOutput()
void RedirectOutput() {
    int fd = open("/dev/null", O_WRONLY|O_CREAT|O_TRUNC, 0200|0400);
    Assert<CE::RunTimeError>(fd != -1, "could not redirect output...");
    bool ok = (dup2(fd, 1) != -1);
    Assert<CE::RunTimeError>(ok, "can't redirect output");
}

// RoundNumber()
std::string RoundNumber(double value, long precision) {
    std::stringstream s;
    s.precision(precision);
    s.setf(std::stringstream::fixed, std::stringstream::floatfield);
    s << value;
    std::string toRtn = s.str();
    if ( (value < 0) && (!toRtn.empty()) &&
         (toRtn[0] == '-') &&
         (toRtn.find_first_not_of("-.0") == std::string::npos)
       ) { // of form  -0.000...0 --> make 0.000...0
        return(toRtn.substr(1));
    }
    return(toRtn);
}

// SolveCNF()
bool SolveCNF(const Input& input) {

    // create solver to be used
    using CE::Usage;
    typedef SolverTraits::SolverFactoryType SF;
    std::auto_ptr<SolverInterface> solver;
    try {
        solver.reset(SF::Instance()->CreateObject(input.Program()));
    } catch(CE::BadArg&) { // No solver found: try to wrap in GeneralSolver
        solver.reset(SF::Instance()->CreateObject(GeneralSolver::Name()));
        solver->SetExeName(input.Program());
        if ( !PreferredSource::Exists(exeDir, input.Program()) )
            solver.reset(0);
    }
    Assert<Usage>(solver.get() != 0, "unknown SAT solver", input.Program());
    solver->SetUnique(convert<std::string>(getpid())); // give unique name

    // Try to solve CNF sentence
    try {        
        solver->RunSolver(input);
    } catch(...) { // empty any solution file left over
        solver->RenameSolution("");
        throw;
    }

    // Deterimine if 'solution' is a real solution
    try {
        solver->RenameSolution(solverResults);
        return(solver->IsSat(solverResults));
    } catch(...) {
        solver->RenameSolution("");
        throw;
    }
}

} // unnamed namespace

