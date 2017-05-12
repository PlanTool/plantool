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
 *  BrazilComboBox.h
 *  
 *
 *  Created by Owen Thomas on 28/11/06.
 *  
 *
 */

#ifndef brazil_combo_box
#define brazil_combo_box

#include <QList>
#include <QComboBox>
#include <QWidget>


class BrazilComboBox : public QComboBox 
{
	Q_OBJECT
	
	private:
			
		bool childrenHaveFocus ()
		{
			QList<QWidget*> children = findChildren<QWidget *>();
	
			for(int i = 0; i < children.size(); i++) {
				if(children [i]->hasFocus ()) return true;
			}
			return false;
		}
		
		
	signals:
		void focusOut ();
		void focusIn ();
		
	public:
		BrazilComboBox (QWidget* parent = NULL) : QComboBox (parent)
		{
			//set min width to something reasonable.
		}
		
		virtual void focusOutEvent (QFocusEvent* event)
		{
			if(!childOrSelfHasFocus ())
			{
				emit focusOut ();
			}
					
			QWidget::focusOutEvent (event);
		}
		
		virtual void focusInEvent (QFocusEvent* event)
		{
			emit focusIn ();
			QWidget::focusInEvent (event);
		}
		
		virtual bool childOrSelfHasFocus ()
		{
			return hasFocus () || childrenHaveFocus ();
		}
};

#endif
