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
 *  FunctionExpressionListWidget.h
 *  
 *
 *  Created by Owen Thomas on 23/11/06.
 *  
 *
 */

#ifndef function_expression_list_widget
#define function_expression_list_widget

#include <QKeyEvent>
#include <QListWidget>
#include <QWidget>
#include <QString>

#include "../../model/impl/DOMDomain.h"

#include <set>
class FunctionExpressionListWidget : public QListWidget {
	Q_OBJECT
	
	private:
		set<QString*> listData;
		
	signals:
		void exited ();
		void selected (const QString&);
		void ignoreKeyEvent (QKeyEvent*);
	public:
		
		FunctionExpressionListWidget (DOMDomain& domain,
			QWidget* parent = NULL): QListWidget (parent) {
			
			set<DOMFunction*>::iterator it;
			for(it = domain.getFunctions().begin ();
				it != domain.getFunctions(). end ();
				it++) {
					QString* newString = new QString ((*it)->getName ());
					listData.insert (newString);
					addItem (*newString);
			}
			sortItems ();
			
			if(count () > 0) {
				setItemSelected (item (0), true);
			}
		}
		
		virtual ~FunctionExpressionListWidget () {
			for(set<QString*>::iterator it = listData.begin ();
				it != listData.end ();
				it ++) {
					
				delete *it;
			}
		}
		
		virtual void setPrefix (const QString& prefix);
		
		virtual void keyPressEvent (QKeyEvent* event);
		
};

#endif
