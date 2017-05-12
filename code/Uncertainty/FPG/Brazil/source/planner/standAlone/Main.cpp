/*
 * $Id: BrazilSimulator.cpp 131 2006-08-25 01:59:40Z daa $ 
 * 
 * This file is part of the Brazil Planner. Copyright NICTA 2006.
 *
 * This file is commercial in confidence. You should no be looking at
 * it unless you are an employee of NICTA, a student who has signed
 * their IP to NICTA, or you have signed an NDA covering the Brazil
 * source code. If you are not one of these people we will poke out
 * your eyes with a gerkhin while forcing you to sing the Brazilian
 * national anthem.
 * 
 */

#include <xercesc/parsers/XercesDOMParser.hpp>
#include <xercesc/dom/DOM.hpp>
#include <xercesc/sax/HandlerBase.hpp>
#include <xercesc/util/XMLString.hpp>
#include <xercesc/util/PlatformUtils.hpp>

XERCES_CPP_NAMESPACE_USE

#include"BrazilSetup.h"
#include"BrazilPlanner.h"

#include "DOMDomain.h"
#include "DOMProbabilistic.h"

/** 
 * Read in an XML file that defines a domain and problem, and any
 * other parameters and plan!
 */
int main(int argc, char** argv) {
   
    // XML init. Bus error without it. Don't know why it's needed, just is.
    try {
	XMLPlatformUtils::Initialize();
    }
    catch (const XMLException& toCatch) {
	char* message = XMLString::transcode(toCatch.getMessage());
	cout << "Error during initialization! :\n"
	     << message << "\n";
	return EXIT_FAILURE;
    }
    
    // Now the actual program
    try {

	if (argc < 2 || argc > 3) throw std::invalid_argument("Need .xml filename");

	// Turn on debugging via global vars defined in BrazilSetup
	brazilDebugModule = 0xffff; // Everything
	brazilPlannerDebugLevel = 0; // Low

	// Override debug level
	if (argc == 3) brazilPlannerDebugLevel = atoi(argv[2]);

	// Okay, all of this painful Xerces stuff is just to get a
	// DOMDomain object.
	XercesDOMParser* parser = new XercesDOMParser ();
	ErrorHandler* errHandler = (ErrorHandler*) new HandlerBase();
	parser->setErrorHandler(errHandler);
	parser->parse (argv[1]);
	DOMDomain* domain = new DOMDomain(parser->getDocument()->getDocumentElement());

	BrazilPlanner* planner = new BrazilPlanner(domain);

	if (domain->getProperties()->findProperty("planner regression test")) {
	    planner->regressionTest();
	    exit(EXIT_SUCCESS);
	}
		

	planner->run(true); // True to indicate learning is on.
    }  
    catch (const SAXParseException& toCatch) {
	cout << "XML Parse Error :\n"
	     << "\tLine: "<<toCatch.getLineNumber()<<endl
	     << "\tCol: "<<toCatch.getColumnNumber()<<endl
	     << "\tName: "<<XMLString::transcode(toCatch.getMessage())<<endl;
	terminate();
    } 
    catch (const exception& e) {
	cout<<e.what()<<endl;
	terminate();
    }

    cout<<"Success!\n";
    return EXIT_SUCCESS;
}
