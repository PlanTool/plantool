/*
 *  GanttNameArea.cpp
 *  
 *
 *  Created by Owen Thomas on 17/05/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#include <xercesc/dom/DOM.hpp>
#include <xercesc/dom/DOMWriter.hpp>
#include <xercesc/dom/DOMImplementation.hpp>

#include <xercesc/framework/LocalFileFormatTarget.hpp>

#include <xercesc/parsers/XercesDOMParser.hpp>
#include <xercesc/sax/HandlerBase.hpp>
#include <xercesc/util/PlatformUtils.hpp>

#include <set>
#include "impl/DOMPredicate.h"
#include "impl/DOMDomain.h"

void printPredicates (DOMDomain& domain)
{
	cout << "predicates:" << endl;
	set<DOMPredicate*> predicates = domain.getPredicates();
	cout << predicates.size() << endl;
	set<DOMPredicate*>::iterator it;
	for (it = predicates.begin(); it != predicates.end(); it++) {
		DOMPredicate* predicate = *it;
		cout << "\t" << predicate->getName().toUtf8().data() << ": " << domain.getPredicate (predicate->getName())->getName().toUtf8().data()<< endl;
	}
}
void printActions (DOMDomain& domain)
{
	cout << "actions: " << endl;
	list<DOMAction*> actions = domain.getActions ();
	list<DOMAction*>::iterator it;
	
	for(it = actions.begin(); it != actions.end(); it++) {
//		printAction (*((*it)));
	}
}

int main (int argc, char** argv) {
	
	try {
		XMLPlatformUtils::Initialize();
	}
	catch (const XMLException& toCatch) {
		return 1;
	}
	
	DOMImplementation* impl = DOMImplementation::getImplementation ();
	XercesDOMParser* parser = new XercesDOMParser();
	
	ErrorHandler* errHandler = (ErrorHandler*) new HandlerBase();
	parser->setErrorHandler(errHandler);
	
    try {
		parser->parse(argv [1]);
	}
	catch (const XMLException& toCatch) {
		char* message = XMLString::transcode(toCatch.getMessage());
		cout << "Exception message is: \n"
			<< message << "\n";
		XMLString::release(&message);
		return -1;
	}
	catch (const DOMException& toCatch) {
		char* message = XMLString::transcode(toCatch.msg);
		cout << "Exception message is: \n"
			<< message << "\n";
		XMLString::release(&message);
		return -1;
	}
	
	DOMDocument* doc = parser->getDocument ();
	DOMElement* element = doc->getDocumentElement ();

	DOMDomain domain (element);
	
	printPredicates (domain);
//	printActions(domain);
}
