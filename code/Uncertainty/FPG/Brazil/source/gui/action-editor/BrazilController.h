/* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is Brazil.
 *
 * The Initial Developer of the Original Code is
 * National ICT Australia.
 * Portions created by the Initial Developer are Copyright (C) 2007
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *	Douglas Aberdeen	(doug.aberdeen@gmail.com)
 *	Owen Thomas		(owen.thomas@nicta.com.au)
 *	Olivier Buffet		(olivier.buffet@loria.fr)
 *
 * ***** END LICENSE BLOCK ***** */
/*
 *  BrazilController.h
 *  
 *
 *  Created by Owen Thomas on 16/04/06.
 *  
 *
 */

#ifndef brazil_controller
#define brazil_controller

#include "DeleteModel.h"
#include "NewModel.h"
#include <QObject>
#include <QAction>
#include <QString>
#include <QColorDialog>
#include <map>
#include <list>
class BrazilController
{
	public:
		
		virtual ~BrazilController() { }
		virtual void setActiveModel (DOMWrapper* wrapper) = 0;
};

class DeleteAction : public QAction, public BrazilController
{
	Q_OBJECT
	
	private:
		map<QString, DeleteModel*> deleters;
		DeleteModel* deleter;
		
	protected slots:
		void doDelete ()
		{
			if(deleter) {
				deleter->callDelete ();
				if(!deleter->getModel()) setEnabled ( false );
			}
		}
		
	
	public:
		DeleteAction (map<QString,DeleteModel*> deleters) : QAction(NULL), BrazilController ()
		{
			QObject::connect (this, SIGNAL (triggered ()), this, SLOT (doDelete ()));
			deleter = NULL;
			this->deleters = deleters;
			setEnabled (false);
			setText (QObject::tr ("Delete"));
		}
		
		void setActiveModel (DOMWrapper* model)
		{
			deleter = deleters [model->getTypeName ()];
			
			if(NULL == deleter) setEnabled ( false );
			else {
				deleter->setModel (model);
				setEnabled (true);
			}
		}
};

class NewAction : public QAction, public BrazilController
{
	Q_OBJECT
	private:
		map<QString, NewModel*> newModels;
		NewModel* current;
	
	protected slots:
		void doNew ()
		{
			if(current && current->canCallNew())
				current->callNew ();
		}
	public: 
		NewAction (map<QString, NewModel*> newModels) : QAction(NULL), BrazilController ()
		{
			QObject::connect (this, SIGNAL (triggered ()), this, SLOT (doNew ()));
			current = NULL;
			this->newModels = newModels;
			setEnabled (false);
		}
		
		
		void setActiveModel (DOMWrapper* model)
		{
			current = newModels [model->getTypeName ()];
			if(current){
				current->setModel(model);
				if(current->canCallNew ()) setEnabled(true);
				else setEnabled(false);
			} else {
				setEnabled (false);
			}
		}
};

class NewConditionGroupAction : public QAction, public BrazilController
{
	Q_OBJECT
	
	private:
		NewDOMInternalCondition* newModel;
	
	protected slots:
		void doNew ()
		{
			if(newModel && newModel->canCallNew())
				newModel->callNew ();
		}
		
	public:
		NewConditionGroupAction () : QAction (NULL), BrazilController ()
		{
			QObject::connect (this, SIGNAL (triggered ()), this, SLOT (doNew ()));
			newModel = new NewDOMInternalCondition ();
			setEnabled (false);
		}
		
		void setActiveModel (DOMWrapper* model)
		{
			newModel->setModel (model);
			if(newModel->canCallNew())setEnabled(true);
		}
	
};

class NewPredicateConditionAction : public QAction, public BrazilController
{
	Q_OBJECT
	
	private:
		NewDOMPredicateCondition* newModel;
		
	protected slots:
		void doNew ()
		{
			if(newModel && newModel->canCallNew ())
				newModel->callNew ();
		}
		
	public:
		NewPredicateConditionAction () : QAction (NULL), BrazilController ()
		{
			QObject::connect (this, SIGNAL (triggered()), this, SLOT (doNew()));
			newModel = new NewDOMPredicateCondition();
			setEnabled (false);
		}
		
		void setActiveModel (DOMWrapper* model)
		{
			newModel->setModel (model);
			if(newModel->canCallNew()) setEnabled (true);
		}
};

class DeleteConditionAction : public QAction, public BrazilController
{
	Q_OBJECT
	
	private:
		DeleteDOMCondition* deleteModel;
		
	protected slots:
		void doDelete ()
		{
			if(deleteModel && deleteModel->canCallDelete()) deleteModel->callDelete();
		}
	
	public:
		DeleteConditionAction () : QAction (NULL), BrazilController ()
		{
			QObject::connect (this, SIGNAL (triggered()), this, SLOT (doDelete()));
			deleteModel = new DeleteDOMCondition();
			setEnabled (false);
		}
		void setActiveModel (DOMWrapper* model)
		{
			deleteModel->setModel (model);
			if(deleteModel->canCallDelete()) setEnabled (true);
		}

};


class ChangeDOMOutcomeColourAction : public QAction, public BrazilController
{
	Q_OBJECT
	private:
		DOMOutcome* current;
		
	protected slots:
		void doChangeColour ()
		{
			QColor setColour = QColorDialog::getColor (current->getBackgroundColour());
			if(setColour.isValid())
				current->setBackgroundColour(setColour.name().toUtf8().data());
		}
		
	public:
		ChangeDOMOutcomeColourAction (): QAction (NULL),  BrazilController()
		{
			QObject::connect (this, SIGNAL (triggered()), this, SLOT (doChangeColour()));
			current = NULL;
			setEnabled(false);
			setText (QObject::tr("Change Colour"));
		}
		
		void setActiveModel (DOMWrapper* wrapper)
		{
			if(wrapper->getTypeName() == QString ("DOMOutcome")) {
				current = (DOMOutcome*)wrapper;
				setEnabled(true);
			} else {
				setEnabled (false);
			}
		}
};

class ChangeDOMActionColourAction : public QAction, public BrazilController
{
	Q_OBJECT
	private:
		DOMAction* current;
		
	protected slots:
		void doChangeColour ()
		{
			QColor setColour = QColorDialog::getColor (current->getBackgroundColour());
			if(setColour.isValid())
				current->setBackgroundColour(setColour.name().toUtf8().data());
		}
		
	public:
		ChangeDOMActionColourAction (): QAction (NULL),  BrazilController()
		{
			QObject::connect (this, SIGNAL (triggered()), this, SLOT (doChangeColour()));
			current = NULL;
			setEnabled(false);
			setText (QObject::tr("Change Colour"));
		}
		
		void setActiveModel (DOMWrapper* wrapper)
		{
			if(wrapper->getTypeName() == QString ("DOMAction")) {
				current = (DOMAction*)wrapper;
				setEnabled(true);
			} else {
				setEnabled (false);
			}
		}
};


class BrazilControllerGroup : public BrazilController
{
	private:
		list<BrazilController*> subControllers;
		
	public:
		BrazilControllerGroup ()
		{
		}
		
		void addController (BrazilController* controller)
		{
			subControllers.push_back (controller);
		}
		
		void setActiveModel (DOMWrapper* model)
		{
			list<BrazilController*>::iterator it;
			for(it = subControllers.begin (); 
				it != subControllers.end(); 
				it++) {
				(*it)->setActiveModel (model);
			}
		}
};

#endif

