/*
 *  DOMWrapper.cpp
 *  
 *
 *  Created by Owen Thomas on 5/04/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#include "DOMWrapper.h"

/**
 * A Static function for converting a Xerces
 * XML String to a QString.
 *
 * Both are Unicode-16 encoding strings, so the
 * transformation is quite simple. Just copy one buffer
 * into the other.
 
 QString DOMWrapper::toQString (const XMLCh* string)
{
	int length = XMLString::stringLen(string);
	QChar *qCharString = new QChar [length];
	while(*string)
	{
		*(qCharString ++) = *(string ++);
	}
	
	return QString(qCharString - length, length);
}
*/

double DOMWrapper::getAttributeAsDouble (const char* attributeName)
{
	XMLCh *name = XMLString::transcode (attributeName);
	const XMLCh *value = node->getAttribute (name);
	char *cString = XMLString::transcode(value);
	
	double retVal = atof (cString);
	
	XMLString::release(&cString);
	XMLString::release (&name);
	
	return retVal;
}

bool DOMWrapper::getAttributeAsBool (const char* attributeName) {
	XMLCh *name = XMLString::transcode (attributeName);
	const XMLCh *value = node->getAttribute (name);
	char *cString = XMLString::transcode(value);
	
	if(0 == strcmp (cString, "false")) return false;
	else if(0 == strcmp (cString, "true")) return true;
	
	//As a consequence this will return false if the string
	//is not a valid integer.
	return atoi (cString);
}
char* DOMWrapper::getAttribute (const char* attributeName)
{
	XMLCh* name = XMLString::transcode (attributeName);
	const XMLCh* value = node->getAttribute (name);
	XMLString::release (&name);
	return XMLString::transcode (value);
}

time_t DOMWrapper::getAttributeAsTime (const char* attributeName)
{
	XMLCh *name = XMLString::transcode (attributeName);
	const XMLCh *value = node->getAttribute (name);
	char *cString = XMLString::transcode(value);
	
	//perhaps we should use atoi ?
	time_t returnVal =  (time_t) atol (cString);
	
	XMLString::release(&cString);
	XMLString::release (&name);
	
	return returnVal;
}

char* DOMWrapper::getTagName ()
{
	const XMLCh * name = node->getTagName ();
	return  XMLString::transcode (name);
}

/*
QColor DOMWrapper::getAttributeAsQColor (const char* attributeName)
{
	XMLCh* name = XMLString::transcode (attributeName);
	const XMLCh* value = node->getAttribute (name);
	
	QString colourCode = toQString (value);
	XMLString::release (&name);
	
	return QColor (colourCode);
}


void DOMWrapper::setAttribute (const char* name, QColor value)
{
	XMLCh* xmlName = XMLString::transcode(name);
	XMLCh* xmlValue = XMLString::transcode (value.name().toUtf8().data());
	
	this->node->setAttribute (xmlName,xmlValue);
	
	XMLString::release(&xmlName);
	XMLString::release(&xmlValue);
}
*/
void DOMWrapper::setAttribute (const char* name, const char* value)
{
	XMLCh* xmlName = XMLString::transcode(name);
	XMLCh* xmlValue = XMLString::transcode (value);
	
	this->node->setAttribute (xmlName,xmlValue);
	
	XMLString::release(&xmlName);
	XMLString::release(&xmlValue);
	
}
void DOMWrapper::setAttribute (const char* name, double value)
{
	XMLCh* xmlName = XMLString::transcode(name);
	
	ostringstream stream;
	
	stream << value << std::ends;
	
	if(stream)
	{
		string valueString = stream.str();
		const char *valueCString = valueString.c_str();
		XMLCh *valueXMLCh = XMLString::transcode (valueCString);
		
		this->node->setAttribute (xmlName, valueXMLCh);
		XMLString::release(&valueXMLCh);
	}
	else
	{
		//Error, throw exception
		throw std::invalid_argument("cannot encode value"); 
	}
	XMLString::release(&xmlName);
}

void DOMWrapper::setAttribute (const char* name, char* value)
{
	XMLCh* xmlName = XMLString::transcode(name);
	XMLCh* valueXMLCh = XMLString::transcode (value);
	
	this->node->setAttribute (xmlName, valueXMLCh);
	
	XMLString::release(&valueXMLCh);
	XMLString::release (&xmlName);
}

void DOMWrapper::setAttribute (const char* name, bool value)
{
	XMLCh* xmlName = XMLString::transcode(name);
	
	ostringstream stream;
	
	XMLCh* valueCh;
	if(value) valueCh = XMLString::transcode ("true");
	else valueCh = XMLString::transcode ("false");
	
	this->node->setAttribute (xmlName, valueCh);
	
	XMLString::release(&xmlName);
}

void DOMWrapper::setAttribute (const char* name, time_t value)
{
	XMLCh* xmlName = XMLString::transcode (name);
	
	ostringstream stream;
	stream << value << std::ends;
	
	if(stream) {
		string valueString = stream.str();
		const char *valueCString = valueString.c_str();
		XMLCh *valueXMLCh = XMLString::transcode (valueCString);
		
		this->node->setAttribute (xmlName, valueXMLCh);
		XMLString::release(&valueXMLCh);

	}
	else {
		//Error, throw exception
		throw std::invalid_argument("cannot encode value"); 
	}
	XMLString::release(&xmlName);

}


