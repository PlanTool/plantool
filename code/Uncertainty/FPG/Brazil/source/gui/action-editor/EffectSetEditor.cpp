/*
 *  EffectSetEditor.cpp
 *  
 *
 *  Created by Owen Thomas on 5/12/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#include "EffectSetEditor.h"
#include "AtomicEffectEditor.h"

#include "../../model/impl/DOMDomain.h"
#include "../../model/impl/DOMAtomicEffect.h"
#include "../../model/impl/DOMFunctionEffect.h"
#include "../../model/impl/DOMPredicateEffect.h"
#include "../../model/impl/DOMEffectSet.h"

#include <QVBoxLayout>
#include <QSizePolicy>
#include <QPushButton>
#include <QMenu>
#include <QAction>
#include <list>
#include <map>

EffectSetEditor::EffectSetEditor (DOMEffectSet& effectSet,
	DOMDomain& domain, QWidget* parent) : QWidget (parent) {
	
	this->domain = &domain;
	this->effectSet = &effectSet;
	effectSet.addListener (this);
	
	QVBoxLayout* vBoxLayout = new QVBoxLayout ();
	setLayout (vBoxLayout);
	vBoxLayout->setSpacing (0);
	
	vBoxLayout->setSizeConstraint (QLayout::SetMinimumSize);
	setSizePolicy (QSizePolicy::MinimumExpanding, QSizePolicy::Fixed);
	//Create Atomic Effect Editors
	list<DOMAtomicEffect*>::iterator it;
	for(it = effectSet.getAtomicEffects().begin (); it !=
		effectSet.getAtomicEffects().end (); it++) {
			
		AtomicEffectEditor* aee = new AtomicEffectEditor ( *(*it), domain,this);
		
		editors [*it] = aee;
		
		vBoxLayout->addWidget (aee);
		
		//Each atomic effect editor maintains a Predicate and Function effect.
		//Only one is 'current' at a time, when this changes we update the
		//effect set to contain the current atomic effect.
		QObject::connect (aee, SIGNAL (atomicEffectChanged (DOMAtomicEffect*,
			DOMAtomicEffect*)), this, SLOT (handleAtomicEffectSwitch (
			DOMAtomicEffect*, DOMAtomicEffect*)));
		
		QObject::connect (aee, SIGNAL (selectStatusChanged(bool)), this,
			SLOT (handleAtomicEffectSelection(bool)));
	}
	
	//Create New and Delete Functionality.
	QPushButton* newButton = new QPushButton ();
	deleteButton = new QPushButton ();
	deleteButton->setEnabled (false);
	
	QIcon* plusIcon = new QIcon ("plus.png");
	QIcon* minusIcon = new QIcon ("minus.png");
	
	//deleteButton->setIconSize (QSize (12,12));
	//newButton->setIconSize (QSize (12,12));
	
	newButton->setIcon (*plusIcon);
	deleteButton->setIcon (*minusIcon);
	
	newButton->setMaximumSize (QSize(35,21));
	deleteButton->setMaximumSize (QSize(21,21));
	QMenu* newButtonMenu = new QMenu ();
	
	newFunctionAction = newButtonMenu->addAction ("New Function Effect");
	newPredicateAction = newButtonMenu->addAction ("New Predicate Effect");
	
	newFunctionAction->setEnabled (domain.getFunctions().size() > 0);
	newPredicateAction->setEnabled (domain.getPredicates().size() > 0);
	//Connect Action/Button signals with slots to actually do the work./
	QObject::connect (newFunctionAction, SIGNAL (triggered (bool)), this, SLOT (handleNewFunctionEffect()));
	QObject::connect (newPredicateAction, SIGNAL (triggered(bool)), this, SLOT (handleNewPredicateEffect()));
	QObject::connect (deleteButton, SIGNAL (clicked(bool)), this, SLOT (handleDelete()));
	
	newButton->setMenu (newButtonMenu);
	
	//Add to the layout at the bottom.
	QBoxLayout* buttonLayout = new QHBoxLayout ();
	buttonLayout->addSpacing (10);
	buttonLayout->addWidget (newButton);
	buttonLayout->addSpacing (5);
	buttonLayout->addWidget (deleteButton);
	buttonLayout->addStretch(1);
	
	vBoxLayout->addLayout (buttonLayout);
}

/*
Each atomic effect editor maintains a Predicate and Function effect.
Only one is 'current' at a time, when this changes we update the
effect set to contain the current atomic effect.
*/	
void EffectSetEditor::handleAtomicEffectSwitch (DOMAtomicEffect* 
	newEffect, DOMAtomicEffect* oldEffect) {
	
	//This will trigger a remove and add of atomic effects as we
	//swap out the old and swap in the new. 
	//These are both being maintained by the same editor, so we don't
	//want to create a new editor or delete an existing one in response
	//to the adds and removes.
	effectSet->removeListener (this);
	
	//do the swap..
	if(oldEffect != NULL) effectSet->removeAtomicEffect (oldEffect);
	effectSet->addAtomicEffect (newEffect);
	
	//keep editors map up to date.
	if(oldEffect != NULL && editors.count (oldEffect)) {
		AtomicEffectEditor* editor = editors [oldEffect];
		editors.erase (oldEffect);
		
		editors [newEffect] = editor;
	}
	effectSet->addListener (this);
}

void EffectSetEditor::effectAdded (DOMAtomicEffect* effect) {
	AtomicEffectEditor* aee = new AtomicEffectEditor ( *effect, *(this->domain),this);
	aee->setVisible(false);
	((QBoxLayout*)layout())->insertWidget (layout()->count() - 1,aee);
	aee->setVisible (true);
	//Each atomic effect editor maintains a Predicate and Function effect.
	//Only one is 'current' at a time, when this changes we update the
	//effect set to contain the current atomic effect.
	QObject::connect (aee, SIGNAL (atomicEffectChanged (DOMAtomicEffect*,
		DOMAtomicEffect*)), this, SLOT (handleAtomicEffectSwitch (
		DOMAtomicEffect*, DOMAtomicEffect*)));
		
	QObject::connect (aee, SIGNAL (selectStatusChanged(bool)), this,
		SLOT (handleAtomicEffectSelection(bool)));
	
	editors [effect] = aee;

}

void EffectSetEditor::effectRemoved (DOMAtomicEffect* effect) {
	if(!editors.count(effect))return;
	
	//Find editor to remove and remove it from the editors map.
	AtomicEffectEditor* editorToRemove = editors [effect];
	editors.erase (effect);
	
	//This should remove the editor from the layout.
	delete editorToRemove;
	
	resize(layout()->minimumSize ());
}

void EffectSetEditor::handleNewFunctionEffect () {
	DOMFunctionExpression* expression = new DOMFunctionExpressionValue 
		(*(domain->getDOMElement()->getOwnerDocument()), 0);
			
	this->effectSet->addAtomicEffect (new DOMFunctionEffect 
		( **(domain->getFunctions().begin ()), DOMFunctionEffect::assign, 
		*expression,*(domain->getDOMElement()->getOwnerDocument())));
}

void EffectSetEditor::handleNewPredicateEffect () {
	this->effectSet->addAtomicEffect (
		new DOMPredicateEffect (**(domain->getPredicates().begin()), 
		*(domain->getDOMElement()->getOwnerDocument()), true));
}

void EffectSetEditor::handleDelete () { 
	map<DOMAtomicEffect*, AtomicEffectEditor*>::iterator it;
	for(it = editors.begin(); it != editors.end(); it++) {
		if(it->second->isSelected()) {
			effectSet->removeAtomicEffect (it->first);
			return;
		}
	}
}

void EffectSetEditor::handleAtomicEffectSelection (bool selected) {
	deleteButton->setEnabled (selected);
}
