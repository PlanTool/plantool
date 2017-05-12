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
 *  FunctionExpressionLineEdit.h
 *  
 *
 *  Created by Owen Thomas on 21/11/06.
 *  
 *
 */

#include <QLineEdit>

#include "FunctionExpressionParser.h"

#include "../../model/impl/DOMFunctionExpression.h"
#include "FunctionExpressionStringBuilder.h"
#include "FunctionExpressionListWidget.h"

class FunctionExpressionLineEdit : public QLineEdit {

	Q_OBJECT
	
	signals:
		void expressionChanged (DOMFunctionExpression*);
		
	private:
		DOMDomain* domain;
		FunctionExpressionParser* parser;
		DOMFunctionExpression* currentExpression;
		FunctionExpressionListWidget* listWidget;
		
		bool ignoreListWidget;
		
		int sizeHintMinimumWidth;
		int textSizeHint;
		
		//Set to true when a parse exception is caught,
		//set to false when a function expression is parsed
		//or set function expression is called.
		//Read through the invalidFunctionDisplayed member
		bool parseExceptionCaught;
		
		bool isFunctionDefChar (const QChar& testChar);
		
		
	protected slots:
		void parseExpression ();
		void handleTextEdited (const QString& newText);
		void handleEditingFinished ();
		
		void handleListWidgetExit ();
		void handleListWidgetSelected (const QString& text);
		void handleListWidgetIgnoredKeyEvent (QKeyEvent* event);
		
	
	public slots:
		/**
		 * Rebuild the text in the line edit from the current 
		 * function expression, discarding any changes that may have 
		 * been made. After a call to this function isDisplayInvalidFunction
		 * will return false.
		 */
		void revert ();
		
	public:
		
		FunctionExpressionLineEdit (QWidget* parent = NULL, int autoResizeDelta = 10);
		
		virtual void setDomain (DOMDomain& domain) { 
			this->domain = &domain; 
			
			if(this->parser) delete this->parser;
			parser = new FunctionExpressionParser (domain);
			
			if(this->listWidget) delete this->listWidget;
			listWidget = new FunctionExpressionListWidget (domain);
			listWidget->hide ();
			
			QObject::connect (listWidget, SIGNAL (exited ()), this,
				SLOT (handleListWidgetExit ()));
				
			QObject::connect (listWidget, SIGNAL (ignoreKeyEvent(QKeyEvent*)),
				this, SLOT (handleListWidgetIgnoredKeyEvent (QKeyEvent*)));
			
			QObject::connect (listWidget, SIGNAL (selected (const QString&)),
				this, SLOT (handleListWidgetSelected (const QString&)));
				
			setEnabled (true);
		}
		
		virtual void setFunctionExpression (DOMFunctionExpression& expr);
		
		
		virtual bool isDisplayInvalidFunctionExpression () {
			return parseExceptionCaught;
		}
		
	};

