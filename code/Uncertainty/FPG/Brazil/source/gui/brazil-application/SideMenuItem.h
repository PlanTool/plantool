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
 *  SideMenuItem.h
 *  
 *
 *  Created by Owen Thomas on 5/09/06.
 *  
 *
 */
#include <iostream>
#include <QString>
#include <QPainter>
#include <QLabel>
#include <QObject>
#include <QHBoxLayout>
#include <QPalette>
#include <QColor>
#include <QFocusEvent>
using namespace std;
class SideMenuItem : public QWidget {

	Q_OBJECT
	
	signals:
		void selected (SideMenuItem*);
	
	private:
		QColor* selectedColour;
		QColor* unselectedColour;
		QColor* selectedUnfocusedColour;
		
		QPalette p;
		
	private slots:
	
		void select () {
			 p = palette ();
			p.setCurrentColorGroup (QPalette::Active);
			p.setColor (QPalette::Active, QPalette::Window, *selectedColour);
			setPalette (p);
			emit selected (this);
		}
	
	public:
		SideMenuItem (QString text, QWidget* parent = NULL) : QWidget (parent) {
			QLabel* label = new QLabel (text, this);
			
			setLayout (new QHBoxLayout());
			setBackgroundRole (QPalette::Window);
			
			layout()->addWidget (label);
			p = palette ();
			this->selectedColour = new QColor ("dodgerblue");
			this->unselectedColour = new QColor ("white");
			this->selectedUnfocusedColour = new QColor ("gray");
			p.setColor (QPalette::Active, QPalette::Window, *selectedColour);
			p.setColor (QPalette::Inactive, QPalette::Window, *unselectedColour);
			setPalette(p);
			setAutoFillBackground (true);
			setFocusPolicy (Qt::StrongFocus);
		}
		
		virtual void focusInEvent (QFocusEvent* event) {
			select ();
			event->ignore ();
		}
		
		virtual void focusOutEvent (QFocusEvent* event) {
			if(palette().currentColorGroup() == QPalette::Active) {
				p = palette();
				p.setColor (QPalette::Active, QPalette::Window, *selectedUnfocusedColour);
				setPalette (p);
			}
			event->ignore();
		}
		virtual void deselect () {
			p = palette ();
			p.setColor (QPalette::Active, QPalette::Window, *unselectedColour);
			setPalette (p);
		}
};

