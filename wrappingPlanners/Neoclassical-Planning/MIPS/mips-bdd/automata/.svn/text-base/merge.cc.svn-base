#include <iostream>
#include <fstream>
#include <string>
#include <unistd.h> 

using namespace std;


int main(int argc, char *argv[]) {
  if (argc <= 1) {
    cout << " usage: "<< endl; 
    cout << " merge <grounded-problem> <grounded-domain> <grounded-sas> <automata-1> .. <automata-n>" << endl;  
    exit (1);
  }
  ofstream problemnew("problem-merge.pddl");
  ofstream domainnew("domain-merge.pddl");
  ofstream sasnew("problem-merge.psas");
  
  cout << "  Reading grounded problem file " << argv[1] << "..." << endl;
  ifstream problemold(argv[1]);
    
  while (!problemold.eof()) {
      string s;
      problemold >> s;
      problemnew << s << endl;
      if (s == "(:init") {
	problemnew << "(sync-ordinary)" << endl;
	for (int i=4; i< argc; i++) {
	  cout << "  Reading init state from grounded automata file "  << argv[i] << "..." << endl;
	  ifstream automata(argv[i]);
	  string t;
	  while (!automata.eof()) {
	    automata >> t;
	    if (t == "(:init") 
	      break;  
	  }
	  while (!automata.eof()) {
	    automata >> t;
	    if (t == ")")  break;
	    problemnew << t << endl;
	  }
	}
      }

       if (s == "(:goal") {
	problemold >> s;
	problemnew << s << endl;
	if (s == "(and") {
	  problemnew << "(sync-ordinary)" << endl;
	  for (int i=4; i< argc; i++) {
	    cout << "  Reading goal state from grounded automata file "  << argv[i] << "..." << endl;
	    ifstream automata(argv[i]);
	    string t;
	    while (!automata.eof()) {
	      automata >> t;
	      if (t == "(:goal") 
	      break;  
	    }
	    while (!automata.eof()) {
	      automata >> t;
	      if (t == ")")
		break;
	      problemnew << t << endl;
	    }
	  }
        }
     }
  }
  
   

  cout << "  Reading grounded domain file " << argv[2] << "..." << endl;
  ifstream domainold(argv[2]);
  bool flag = true;
  
  while (!domainold.eof()) {
      string s;
      domainold >> s;
      domainnew << s << endl;
      if (s == "(:predicates") {
	  domainnew << "(sync-ordinary)" << endl;
	  for (int i=4; i< argc; i++) {
	      cout << "  Reading predicates from grounded automata file "  
		   << argv[i] << "..." << endl;
		ifstream automata(argv[i]);
		string t;
		while (!automata.eof()) {
		    automata >> t;
		    if (t == "(:predicates") 
			break;  
		}
		while (!automata.eof()) {
		    automata >> t;
		    if (t == ")")
			break;
		    domainnew << t << endl;
		}
	    }
	}
	
	if (s == "(:action" && flag) {
	    flag = false;
	    for (int i=4; i< argc; i++) {
		cout << "  Reading actions from grounded automata file "  
		     << argv[i] << "..." << endl;
		ifstream automata(argv[i]);
		string t;
		while (!automata.eof()) {
		    automata >> t;
		    if (t == "(:action") 
			break;  
		}
		while (!automata.eof()) {
		    automata >> t;
		    if (automata.eof()) break;
		    domainnew << t << endl;
		}
		domainnew << "(:action" << endl;
	    }
	}
  }

  cout << "  Reading grounded sas file " << argv[3] << "..." << endl;
  ifstream sasold(argv[3]);
  
  while (!sasold.eof()) {
      string s;
      sasold >> s;
      sasnew << s << endl;
      if (s == "(:partition") {
	  sasnew << "   (:fact-group-sync" << endl;
	  sasnew << "       (sync-ordinary)" << endl;

	  for (int i=4; i< argc; i++) {
	      string filename(argv[i]);
	      int l = filename.length();
	      cout << "  Reading grounded file " << filename;  
/*
		   << filename << " from position " 
		   << 9 << " to position " << l-6
		   << "..." << endl;
*/
	      string purename = filename.substr(9,l-5-9);
	      cout << ", synchronizing "  
		   << purename << "..." << endl;
	      sasnew << "       (sync-automaton-" << purename << ")" << endl;
	  }
	  sasnew << "   )" << endl;
	  for (int i=4; i< argc; i++) {
	      cout << "  Reading fact-groups from grounded file "  
		   << argv[i] << "..." << endl;
		ifstream automata(argv[i]);
		string t;
		while (!automata.eof()) {
		    automata >> t;
		    if (t == "(:partition") 
			break;  
		}
		string pref;
		t = string("");
		while (!automata.eof()) {
		    pref = t;
		    automata >> t;
		    if (t == "(:predicates") {
			break;
		    }
		    sasnew << pref << endl;
		}
	  }
      }
  }
}
