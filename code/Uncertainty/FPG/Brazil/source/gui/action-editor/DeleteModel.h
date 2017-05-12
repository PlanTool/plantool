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
 *  DeleteModel.h
 *  
 * 
 *
 *  Created by Owen Thomas on 18/04/06.
 *  
 *
 */

#include "../../model/impl/DOMWrapper.h"

/**
 *
 * Abstract base class for defining functionality to delete a model.
 *
 */
class DeleteModel
{
	public:
		virtual ~DeleteModel() { }
		virtual void callDelete () = 0;
		virtual void setModel (DOMWrapper* model) = 0;
		virtual DOMWrapper* getModel () = 0;
};

#include "../../model/impl/DOMOutcome.h"
#include "../../model/impl/DOMProbabilistic.h"

#include "ProbabilityController.h"
class DeleteDOMOutcome : public DeleteModel
{
	private:
		DOMOutcome* model;
		
	public:
		DeleteDOMOutcome ()
		{
			model = NULL;
		}
		
		void setModel (DOMWrapper* model)
		{
			this->model = (DOMOutcome*)model;
		}
		
		DOMWrapper* getModel () { return this->model; }
		
		void callDelete ()
		{
			if (model) {
				ProbabilityController c (model->getProbabilistic () );
				DOMOutcome* deleteModel = model;
				c.removeOutcome (model);
				if(model == deleteModel) model = NULL;
				delete deleteModel;
			}
		}
};

#include "../../model/impl/DOMInternalCondition.h"
#include "../../model/impl/DOMConditionBase.h"

#include <xercesc/dom/DOM.hpp>
#include <xercesc/dom/DOMWriter.hpp>
#include <xercesc/dom/DOMImplementation.hpp>

#include <xercesc/framework/LocalFileFormatTarget.hpp>

#include <xercesc/parsers/XercesDOMParser.hpp>
#include <xercesc/sax/HandlerBase.hpp>
#include <xercesc/util/PlatformUtils.hpp>
class DeleteDOMCondition : public DeleteModel
{
	private:
		DOMCondition* currentCondition;
	public:
		DeleteDOMCondition ()
		{
			currentCondition = NULL;
		}
		
		void setModel (DOMWrapper* wrapper)
		{
			if(strcmp(wrapper->getTypeName (), "DOMCondition") == 0) {
				currentCondition = (DOMCondition*) wrapper;				
			}
			else {
				currentCondition = NULL;
			}
		}
		
		DOMWrapper* getModel () { return this->currentCondition; }
		
		bool canCallDelete () { return currentCondition != NULL 
			&& NULL != currentCondition->parent; }
		
		void callDelete ()
		{
		
			if(!canCallDelete()) return;
			
			
			DOMInternalCondition* parentCondition = currentCondition->parent;
			
			
			parentCondition->removeChild (currentCondition);
		
			delete currentCondition;
			
			if(parentCondition->parent) {
				if(parentCondition->getChildren ().size() == 0) {
					setModel (parentCondition);
					callDelete ();
				}
			}
		}
};

