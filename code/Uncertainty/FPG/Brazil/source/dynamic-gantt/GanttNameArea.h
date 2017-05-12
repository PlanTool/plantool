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
 *  GanttNameArea.h
 *  
 *
 *  Created by Owen Thomas on 12/05/06.
 *  
 *
 */

#ifndef gantt_name_area
#define gantt_name_area

using namespace std;
#include <QVBoxLayout>
#include <QWidget>
#include <QLabel>

#include <map>

class GanttBarContainer;

class GanttNameArea : public QWidget
{
	private:
		QVBoxLayout* layout;
		map<GanttBarContainer*, QWidget*> containerNames;
		map<QWidget*, GanttBarContainer*> reverseContainerNames;
		
	public:
		GanttNameArea (QWidget* parent = NULL) : QWidget (parent)
		{
			layout = new QVBoxLayout (this);
			layout->setSpacing (0);
			
			layout->setSizeConstraint (QLayout::SetFixedSize);
			setLayout (layout);
			
		}
		
		//Insert into set,
		//get index, - I think we could use iterator subtraction here?
		//insert into layout at that index.
		void addGanttBarContainer (GanttBarContainer& container);
		
		//Remove from set
		//Remove from layout.
		void removeGanttBarContainer (GanttBarContainer& container);
		
		//calls removeGanttBarContainer, followed by add 
		//GanttBarContainer
		void reposition (GanttBarContainer& container)
		{
			removeGanttBarContainer (container);
			addGanttBarContainer (container);
		}
		
		void hide(GanttBarContainer& container) {
			if(!containerNames.count (&container)) return;
			QWidget* label = containerNames [&container];
			label->hide ();
		}

};
#endif

