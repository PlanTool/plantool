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
 * $Id$
 *
 * This file is part of the Brazil Planner. Copyright NICTA 2006.
 *
 * This file is commercial in confidence. You should no be looking at
 * it unless you are an employee of NICTA, a student who has signed
 * their IP to NICTA, or you have signed an NDA covering the Brazil
 * source code. If you are not one of these people we will poke out
 * your eyes with a gerkhin while forcing you to sing the Brazilian
 * national anthem.
 *
 */

#ifndef hot_area_widget_h
#define hot_area_widget_h

/*
 *  HotAreaWidget.h
 *  
 *
 *  Created by Owen Thomas on 27/10/06.
 *  
 *
 */

#include <QWidget>
#include <QBrush>
#include <QPen>
#include <QEvent>
#include <QMouseEvent>
#include <iostream>
#include <QColor>

/**
 *  Just draws a little square or circle that can be interacted with by dragging or double clicking.
 */
class HotAreaWidget : public QWidget {

	Q_OBJECT
	
	signals:
		
		void newPosition (int x);
		void doubleClicked (HotAreaWidget* w);
		void enter ();
		void leave ();
	private:
		
		bool round;  // Circle or square?
		bool locked; //

		int radius;
		QBrush brush;
		QPen pen;
		
	protected:
		
		void paintEvent (QPaintEvent*);
		
		void mouseMoveEvent (QMouseEvent* event);
		
		void enterEvent (QEvent* event);
		
		void leaveEvent (QEvent* event);
	
		void mouseDoubleClickEvent(QMouseEvent* event);
        public:
		
		HotAreaWidget (int radius, QColor color, bool round, bool locked, QWidget* parent);
	
		virtual ~HotAreaWidget () {}


};
#endif

