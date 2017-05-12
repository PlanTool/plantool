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
 *  EmptyEffectEditorWidget.h
 *  
 *
 *  Created by Owen Thomas on 2/08/06.
 *  
 *
 */

#ifndef empty_effect_editor_widget
#define empty_effect_editor_widget

#include "NewModel.h"
#include <QFrame>
#include <QPushButton>
#include <QLabel>
#include <QHBoxLayout>
class EmptyEffectEditorWidget : public QWidget {
	
	Q_OBJECT
	private:
		NewModel* model;
		QPushButton* createButton;
		
	protected slots:
		void callCreate () {
			if(model->canCallNew()) model->callNew ();
		}
	public:
		
		EmptyEffectEditorWidget (NewModel* model, QWidget* parent = NULL) : QWidget (parent) {
			QVBoxLayout* overallLayout = new QVBoxLayout ();
			setLayout (overallLayout);
			QFrame* lineFrame = new QFrame ();
			lineFrame->setFrameStyle (QFrame::HLine | QFrame::Plain);
			overallLayout->addWidget (lineFrame);
			
			createButton = new QPushButton ("New Effect");

			QHBoxLayout* layout = new QHBoxLayout ();
			
			layout->addWidget (createButton);
			layout->addStretch (5);
			
			overallLayout->addLayout (layout);
			overallLayout->setSizeConstraint (QLayout::SetMinAndMaxSize);
			this->model = model;
			
			QObject::connect (createButton, SIGNAL (clicked (bool)), this, SLOT (callCreate()) );
			
		}
		
		bool childOrSelfHasFocus () {
			return hasFocus () || createButton->hasFocus ();
		}
};

#endif

