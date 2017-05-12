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
 *  SideMenu.h
 *  
 *
 *  Created by Owen Thomas on 5/09/06.
 *  
 *
 */
 #ifndef side_menu
 #define side_menu
 
#include <QObject>
#include <QString>
#include <QWidget>
#include <QVBoxLayout>
#include <map>

#include "SideMenuItem.h"
#include <QIcon>

using namespace std;
class SideMenu : public QWidget {

	Q_OBJECT
	
	private:
		map<SideMenuItem*, QWidget*> itemWidgets;
		SideMenuItem* currentSelectedItem;
		QPalette p;
		QColor backgroundColour;
		int itemCount;
		
	signals:
		void selectedWidgetChanged (QWidget*);
		
	protected slots:
	
		/**
		 * Find the qwidget for item and display it.
		 */
		void selected (SideMenuItem* item);
	
	public:
		
		SideMenu (QWidget* parent = NULL);
		
		virtual ~SideMenu () {
			//To-Do.
		}
		
		SideMenuItem* addItem (QString label, QWidget* widget); 
	//	void addItem (QString , QIcon , QWidget* ) { }
		
		QWidget* setItem (SideMenuItem*, QWidget* widget);
		
		QWidget* currentSelectedWidget ();
		//
		//Definition, Overview, Gantt Chart, Bookmarks
};

#endif

