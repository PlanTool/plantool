/*
 *  FunctionComboBox.cpp
 *  
 *
 *  Created by Owen Thomas on 28/11/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#include <set>
#include <QValidator>
#include <QRegExp>
#include <QRegExpValidator>


#include "FunctionComboBox.h"

#include "../../model/impl/DOMDomain.h"
#include "../../model/impl/DOMFunction.h"

FunctionComboBox::FunctionComboBox (DOMDomain& domain, QWidget* parent) 
	: BrazilComboBox (parent) {

	this->domain = &domain;
	domain.addListener (this);
	
	set<DOMFunction*> functions = domain.getFunctions ();
	set<DOMFunction*>::iterator it;
	for(it = functions.begin(); it != functions.end(); it++) {
		addItem ((*it)->getName ());
	}
	
	QRegExp whiteSpace("(\\b([a-zA-Z0-9])+\\b(\\s+))+");
	QValidator* validator = new QRegExpValidator (whiteSpace,this);
			
	QObject::connect (this, SIGNAL (currentIndexChanged (int)), this,
		SLOT (handleIndexChanged (int)));
				setMinimumWidth (fontMetrics().width ("XXXXXXXXXXXXXXXXXXXXXXX"));

	setEditable (true);
	setValidator (validator);
}

void FunctionComboBox::focusOutEvent (QFocusEvent* event) {
	if(currentText().trimmed().size() > 0 && -1 == findText (currentText ())) {
		addFunction (currentText());
	}
	BrazilComboBox::focusOutEvent (event);
}

DOMFunction* FunctionComboBox::getCurrentFunction () {
	return domain->getFunction (currentText().toUtf8().data());
}

void FunctionComboBox::addFunction (const QString& name) {
	DOMFunction* function = new DOMFunction (name.toUtf8().data(), 
		domain->getDOMElement()->getOwnerDocument ());
	
	domain->addFunction (function);
}

void FunctionComboBox::functionAdded (DOMFunction* function) {
	addItem (function->getName ());
}

void FunctionComboBox::setCurrentFunction (DOMFunction* function) {
	setCurrentIndex (findText (function->getName ()));
}
void FunctionComboBox::setDomain (DOMDomain& domain) {
	this->domain->removeListener (this);
	this->domain = &domain;
	this->domain->addListener (this);
	
	//remove items..
	for(int i = 0; i < count (); i++) {
		removeItem (0);
	}
	
	set<DOMFunction*> functions = domain.getFunctions ();
	set<DOMFunction*>::iterator it;
	for(it = functions.begin(); it != functions.end(); it++) {
		addItem ((*it)->getName ());
	}	
}

void FunctionComboBox::handleIndexChanged (int newIndex) {
	emit currentFunctionChanged (getCurrentFunction ());
}

