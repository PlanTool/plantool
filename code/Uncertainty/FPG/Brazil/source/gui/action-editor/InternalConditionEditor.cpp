/*
 *  InternalConditionEditor.cpp
 *  
 *
 *  Created by Owen Thomas on 29/11/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#include "InternalConditionEditor.h"

#include <QVBoxLayout>

#define AND_IND 0
#define OR_IND 1
#define NOT_OR_IND 2
#define NOT_AND_IND 3

#include "../../model/impl/DOMDomain.h"
#include "../../model/impl/DOMConditionBase.h"
#include "../../model/impl/DOMPredicateCondition.h"
#include "../../model/impl/DOMFunctionCondition.h"
#include "../../model/impl/DOMInternalCondition.h"
 
#include "PredicateConditionEditor.h"
#include "FunctionConditionEditor.h"

#include "EmptyInternalConditionEditorWidget.h"

#include "FunctionExpressionLineEdit.h"
#include "FunctionComboBox.h"

InternalConditionEditor::InternalConditionEditor (DOMInternalCondition& condition,
	DOMDomain& domain,
	QWidget* parent) : ConditionEditor (condition, domain, parent) {
	
	currentRequester = NULL;
	
	condition.addListener (this);
	
	setLayout (new QVBoxLayout (this));
	((QBoxLayout*)layout())->setSizeConstraint (QLayout::SetMinimumSize);
	
	//Create operator bar.
	operatorComboBox = new BrazilComboBox ();
	operatorComboBox->insertItem (AND_IND, QObject::tr ("All of"));
	operatorComboBox->insertItem (OR_IND, QObject::tr ("At Least One of"));
	operatorComboBox->insertItem (NOT_OR_IND, QObject::tr ("None of"));
	operatorComboBox->insertItem (NOT_AND_IND, QObject::tr("At Least Not One of"));
	
	//Set operator combo box index correctly
	if(!condition.isNegated () && condition.isConjunctive ()) {
		operatorComboBox->setCurrentIndex (AND_IND);
	}
	else if (!condition.isNegated () && !condition.isConjunctive ()) {
		operatorComboBox->setCurrentIndex (OR_IND);
	}
	else if (condition.isNegated () && condition.isConjunctive ()) {
		operatorComboBox->setCurrentIndex (NOT_AND_IND);
	}
	else {
		operatorComboBox->setCurrentIndex (NOT_OR_IND);
	}
	
	QObject::connect (operatorComboBox, SIGNAL (currentIndexChanged (int)),
		this, SLOT (handleOperatorComboBoxSelection (int)));
	
	operatorBar = new QWidget (this);
	operatorBar->setSizePolicy (QSizePolicy::MinimumExpanding, QSizePolicy::Fixed);
	operatorBar->setLayout (new QHBoxLayout ());
	((QBoxLayout*)operatorBar->layout ())->setMargin (0);
	operatorBar->layout()->addWidget (operatorComboBox);
	((QBoxLayout*)operatorBar->layout())->addStretch (10);
	operatorBar->layout()->addWidget (deleteButton);
	deleteButton->show ();
		
	layout ()->addWidget (operatorBar);
	((QBoxLayout*)layout())->setStretchFactor(operatorBar, 0);
	
	QHBoxLayout* horizontalPadding = new QHBoxLayout ();
	horizontalPadding->addSpacing (20);
		
	((QBoxLayout*)layout ())->addLayout (horizontalPadding);
	
	childEditorsFrame = new QFrame (this);
	QVBoxLayout* vBoxLayout = new QVBoxLayout ();
	vBoxLayout->setSpacing  (0);
	vBoxLayout->setMargin (0);
	vBoxLayout->setSizeConstraint (QLayout::SetMinimumSize);
	
	childEditorsFrame->setLayout (vBoxLayout);
	childEditorsFrame->setFrameStyle (QFrame::Box | QFrame::Plain);
	childEditorsFrame->setSizePolicy (QSizePolicy::MinimumExpanding, QSizePolicy::Fixed);
	emptyEditorWidget = new EmptyInternalConditionEditorWidget (condition, 
		domain, childEditorsFrame);
	
	emptyEditorWidget->hide ();
	
	//Create child editors.
	list<DOMCondition*>::iterator it;
	for(it = condition.getChildren().begin (); it != condition.getChildren().end ();
		it ++) {
			
		(*it)->visit (this);
	}
	
	if(0 == mapOfChildren.size())  {
		childEditorsFrame->layout()->addWidget (emptyEditorWidget);
		emptyEditorWidget->show ();
		operatorBar->hide ();
		childEditorsFrame->setFrameStyle (QFrame::NoFrame);
	}
	
	horizontalPadding->addWidget (childEditorsFrame);
	
		
	setFocusPolicy (Qt::ClickFocus);
	setAutoFillBackground (true);
}

bool InternalConditionEditor::visitInternal (DOMInternalCondition* condition) {
	addChildConditionEditor (new InternalConditionEditor (*condition, *(this->domain),
		childEditorsFrame));
	
	return false;
}

bool InternalConditionEditor::visitPredicate (DOMPredicateCondition* condition) {
	
	PredicateConditionEditor* conditionEditor = 
		new PredicateConditionEditor (*condition,
		*domain, 
		new PredicateComboBox (*domain), childEditorsFrame);
		
	conditionEditor->hide ();
	
	QObject::connect (conditionEditor, SIGNAL (requestNewInternalCondition (ConditionEditor*)),
		this, SLOT (handleNewInternalCondition (ConditionEditor*)));
		
	QObject::connect (conditionEditor, SIGNAL (requestNewPredicateCondition (ConditionEditor*)),
		this, SLOT (handleNewPredicateCondition (ConditionEditor*)));
	
	QObject::connect (conditionEditor, SIGNAL (requestNewFunctionCondition (ConditionEditor*)),
		this, SLOT (handleNewFunctionCondition (ConditionEditor*)));
	
	addChildConditionEditor (conditionEditor);
	
	return false;
	
}

bool InternalConditionEditor::visitFunction (DOMFunctionCondition* condition) {
	FunctionExpressionLineEdit* edit = new FunctionExpressionLineEdit;
	edit->setDomain (*domain);
	edit->setFunctionExpression (*(condition->getExpression()));
	FunctionConditionEditor* conditionEditor =
		new FunctionConditionEditor (*condition, *domain,new FunctionComboBox (*domain),
		edit, childEditorsFrame);
	
	conditionEditor->hide ();
	
	QObject::connect (conditionEditor, SIGNAL (requestNewInternalCondition (ConditionEditor*)),
		this, SLOT (handleNewInternalCondition (ConditionEditor*)));
		
	QObject::connect (conditionEditor, SIGNAL (requestNewPredicateCondition (ConditionEditor*)),
		this, SLOT (handleNewPredicateCondition (ConditionEditor*)));
	
	QObject::connect (conditionEditor, SIGNAL (requestNewFunctionCondition (ConditionEditor*)),
		this, SLOT (handleNewFunctionCondition (ConditionEditor*)));
	
	addChildConditionEditor (conditionEditor);
	//connect signals as with visitPredicate.
	
	return false;
}


void InternalConditionEditor::addChildConditionEditor (ConditionEditor* child) {
	if(mapOfChildren.size () == 0) {
		childEditorsFrame->layout()->removeWidget (emptyEditorWidget);
		emptyEditorWidget->hide ();
		operatorBar->show ();
		childEditorsFrame->setFrameStyle (QFrame::Box | QFrame::Plain);
	}
	
	mapOfChildren [child->getDOMCondition ()] = child;
	
	QObject::connect (child, SIGNAL (requestDelete (DOMCondition*)),
		this, SLOT (handleDeleteCondition (DOMCondition*)));

	if(NULL == currentRequester) {
		if( mapOfChildren.size() % 2) {
			child->setBackgroundRole(QPalette::AlternateBase);
		}
		if(mapOfChildren.size() > 1) {
			QFrame* hLine = new QFrame (childEditorsFrame);
			hLine->setFrameStyle (QFrame::HLine | QFrame::Plain);
			childEditorsFrame->layout()->addWidget (hLine);
		}
		childEditorsFrame->layout ()->addWidget (child);
	}
	else {
		int insertIndex = childEditorsFrame->layout ()->indexOf (currentRequester);
		if((insertIndex / 2) % 2) {
			child->setBackgroundRole (QPalette::AlternateBase);
		}
		if(mapOfChildren.size() > 1) {
			QFrame* hLine = new QFrame (childEditorsFrame);
			hLine->setFrameStyle (QFrame::HLine | QFrame::Plain);
			((QBoxLayout*)childEditorsFrame->layout())->insertWidget (insertIndex + 1, hLine);
		}
		((QBoxLayout*)childEditorsFrame->layout())->insertWidget (insertIndex + 2, child);
		
		map<DOMCondition*, ConditionEditor*>::iterator it;
		for(it = mapOfChildren.begin (); it != mapOfChildren.end(); it++) {
			int indexOfChild = childEditorsFrame->layout ()->indexOf (it->second);
			
			if(((indexOfChild / 2) % 2)) {
				(it->second)->setBackgroundRole (QPalette::AlternateBase);
				(it->second)->setAutoFillBackground (true);
			}
			else {
				(it->second)->setAutoFillBackground (false);
				
			}
		}
	}
	child->show ();
	
	//Hide the combo box if we only have one child.
	if(mapOfChildren.size () > 1) operatorComboBox->setEnabled (true);
}

//condition listener

void InternalConditionEditor::conditionAdded (DOMCondition* condition) {
	condition->visit (this);
}

void InternalConditionEditor::conditionRemoved (DOMCondition* condition) {
	if(!mapOfChildren.count (condition)) return;
	
	//Find ConditionEditor associated with condition.
	ConditionEditor* editor = mapOfChildren [condition];
	int editorIndex = childEditorsFrame->layout()->indexOf (editor);
	
	//Find the horizontal frame above or below the widget
	QWidget* deleteFrame = NULL;
	
	if(mapOfChildren.size() > 1) {
		if(editorIndex > 0) {
			deleteFrame = ((QBoxLayout*)childEditorsFrame->layout ())->itemAt (editorIndex - 1)->widget ();
		}
		
		else {
			deleteFrame = ((QBoxLayout*)childEditorsFrame->layout ())->itemAt (1)->widget ();
		}
	}
	
	childEditorsFrame->layout()->removeWidget (editor);
	editor->hide();
	mapOfChildren.erase (condition);
	
	if(deleteFrame) delete deleteFrame;
	delete editor;
	
	map<DOMCondition*, ConditionEditor*>::iterator it;
	for(it = mapOfChildren.begin (); it != mapOfChildren.end(); it++) {
		int indexOfChild = childEditorsFrame->layout ()->indexOf (it->second);
		
		if(((indexOfChild / 2) % 2)) {
			(it->second)->setBackgroundRole (QPalette::AlternateBase);
			(it->second)->setAutoFillBackground (true);
		}
		else {
			(it->second)->setAutoFillBackground (false);
			
		}
	}
	
	//Hide the combo box if we only have one child.
	if(mapOfChildren.size () < 2) {
		operatorComboBox->setEnabled (false);
		if(mapOfChildren.size () == 0) {
			childEditorsFrame->layout ()->addWidget (emptyEditorWidget);
			emptyEditorWidget->show ();
			operatorBar->hide ();
			childEditorsFrame->setFrameStyle (QFrame::NoFrame);
		}
	}
	
	//I don't know how else to get this to resize other than doing it explicitly like this.
	resize(minimumSize());
	//cout << "condition removed, min size: " << minimumSize().height() << "," << layout()->minimumSize().height () << endl;
}

void InternalConditionEditor::setNegated (bool) {
	if(condition->isNegated() && ((DOMInternalCondition*)condition)->isConjunctive())
		operatorComboBox->setCurrentIndex (NOT_AND_IND);
	else if(condition->isNegated () && !((DOMInternalCondition*)condition)->isConjunctive())
		operatorComboBox->setCurrentIndex (NOT_OR_IND);
	else if(!condition->isNegated() && ((DOMInternalCondition*)condition)->isConjunctive())
		operatorComboBox->setCurrentIndex(AND_IND);
	else
		operatorComboBox->setCurrentIndex (OR_IND);
}

void InternalConditionEditor::handleDeleteCondition (DOMCondition* condition) {
	//Just delete the condition from its parent condition.
	((DOMInternalCondition*)this->condition)->removeChild (condition);
}

void InternalConditionEditor::handleNewInternalCondition (ConditionEditor* requester) {
	currentRequester = requester;
	//Must create a new DOMInternalCondition and add to this->condition.
	DOMInternalCondition* newCondition = 
		((DOMInternalCondition*)condition)->createAndCondition ();
		
	((DOMInternalCondition*)this->condition)->addChild (newCondition);
	
	//This widget will then be updated to include a new child editor by 
	//the listener methods listening on this->condition.
	currentRequester = NULL;
}

void InternalConditionEditor::handleNewFunctionCondition (ConditionEditor* requester) {
	currentRequester = requester;
	
	DOMFunctionExpression* expression = 
		new DOMFunctionExpressionValue (*(domain->getDOMElement()->getOwnerDocument ()),0.0);
		
	//Must create a new DOMFunctionCondition and add to this->condition.
	DOMFunctionCondition* functionCondition = 
		new DOMFunctionCondition (*(domain->getFunctions().begin()),
			DOMFunctionCondition::greaterThan, expression, 
			domain->getDOMElement()->getOwnerDocument ());
	

	((DOMInternalCondition*)this->condition)->addChild (functionCondition);

	currentRequester = NULL;
}

void InternalConditionEditor::handleNewPredicateCondition (ConditionEditor* requester) {
	currentRequester = requester;
	//Create a new PredicateCondition and add to this->condition.
	DOMPredicateCondition* predicateCondition = 
		new DOMPredicateCondition (*(domain->getPredicates().begin ()),
			domain->getDOMElement()->getOwnerDocument ());
	
	((DOMInternalCondition*)this->condition)->addChild (predicateCondition);
	currentRequester = NULL;
}

void InternalConditionEditor::handleOperatorComboBoxSelection (int index) {
//This is very simple now, we just set the type
	//and the negated attribute accordingly.
	switch (index) {
		case AND_IND:
			((DOMInternalCondition*)condition)->setConjunctive(true);
			condition->setNegated (false);
			break;
			
		case OR_IND:
			((DOMInternalCondition*)condition)->setConjunctive(false);
			condition->setNegated(false);
			break;
			
		case NOT_OR_IND:
			((DOMInternalCondition*)condition)->setConjunctive(false);
			condition->setNegated(true);
			break;
			
		case NOT_AND_IND:
			((DOMInternalCondition*)condition)->setConjunctive(true);
			condition->setNegated(false);
			break;
	}
}

void InternalConditionEditor::setOperatorBarText (const QString& text) {
	
	((QBoxLayout*)operatorBar->layout ())->insertWidget (0, new QLabel ("If"));
	operatorBar->layout()->addWidget (new QLabel (text));
}

void InternalConditionEditor::setCondition (DOMInternalCondition& condition,
	DOMDomain& domain) {
	
	QWidget* childEditorsParent = childEditorsFrame->parentWidget();
	
	delete emptyEditorWidget;
	delete childEditorsFrame;
	
	mapOfChildren.clear ();
	
	currentRequester = NULL;
	
	childEditorsFrame = new QFrame (this);
	
	QVBoxLayout* vBoxLayout = new QVBoxLayout ();
	
	vBoxLayout->setMargin (0);
	vBoxLayout->setSpacing (0);
	
	childEditorsFrame->setLayout (vBoxLayout);
	
	emptyEditorWidget =  new EmptyInternalConditionEditorWidget (condition, 
		domain, childEditorsFrame);
	
	emptyEditorWidget->hide ();	
	
	childEditorsFrame->layout()->addWidget (emptyEditorWidget);
	
	((DOMInternalCondition*)this->condition)->removeListener (this);
	
	this->domain = &domain;
	this->condition = &condition;

	((DOMInternalCondition*)this->condition)->addListener (this);
	
	//Create child editors.
	list<DOMCondition*>::iterator it;
	for(it = condition.getChildren().begin (); it != condition.getChildren().end ();
		it ++) {
			
		(*it)->visit (this);
	}
	
	if(0 == mapOfChildren.size())  {
		emptyEditorWidget->show ();
		operatorBar->hide ();
		childEditorsFrame->setFrameStyle (QFrame::NoFrame);
	}
	
	childEditorsFrame->setSizePolicy (QSizePolicy::Minimum, QSizePolicy::Fixed);
	childEditorsParent->layout()->addWidget (childEditorsFrame);
}

