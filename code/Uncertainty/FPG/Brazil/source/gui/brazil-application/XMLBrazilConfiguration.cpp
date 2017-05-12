/*
 *  XMLBrazilConfiguration.cpp
 *  
 *
 *  Created by Owen Thomas on 11/10/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#include "XMLBrazilConfiguration.h"
#include <iostream>
using namespace std;
/*
XMLBrazilConfiguration::XMLBrazilConfiguration (DOMElement& config) {
	this->config = &config;
	
	this->dirString = XMLString::transcode ("defaultDir");
	this->valueString = XMLString::transcode ("value");
}
*/

XMLBrazilConfiguration::XMLBrazilConfiguration (DOMElement& config) {
	this->config = &config;
	
	this->dirString = XMLString::transcode ("defaultDir");
	this->valueString = XMLString::transcode ("value");
}

QString XMLBrazilConfiguration::helper (XMLCh* name) {
	DOMNodeList* nodeList = config->getElementsByTagName (name);
	
	if(!nodeList->getLength ()) return "";
	
	DOMElement* elem = (DOMElement*) nodeList->item (0);
	
	return XMLString::transcode (elem->getAttribute ( valueString ));
}

void XMLBrazilConfiguration::helperSave (XMLCh* name, QString value) {
	DOMNodeList* nodeList = config->getElementsByTagName (name);
	
	if(!nodeList->getLength ()) return;
	
	DOMElement* elem = (DOMElement*) nodeList->item (0);
	
	elem->setAttribute (valueString, XMLString::transcode (value.toUtf8().data()));
}

QString XMLBrazilConfiguration::getDefaultDir () {
	return helper (dirString);
}

void XMLBrazilConfiguration::setDefaultDir (QString& file) {
	helperSave (dirString, file);
}

void XMLBrazilConfiguration::save (QString& fileName) {

	DOMImplementation* impl  = DOMImplementation::getImplementation ();
	DOMWriter* writer = impl->createDOMWriter ();
 	
	writer->setFeature(XMLString::transcode("format-pretty-print"), true);
	
	XMLFormatTarget* fileTarget = new LocalFileFormatTarget (fileName.toUtf8 ().data ());
	
	
	writer->writeNode (fileTarget, *config);
	
	delete writer;
	delete fileTarget;
}

