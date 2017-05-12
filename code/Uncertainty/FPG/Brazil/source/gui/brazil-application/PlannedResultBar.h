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
 *  PlannedResultBar.h
 *  
 *
 *  Created by Owen Thomas on 3/08/06.
 *  
 *
 */

/**
 * Presents a result bar for the case where we have planning
 * information generated already. Maps gui commands to three
 * signals.
 */
class PlannedResultBar : public QWidget {
	
	signals:
		/**
		 * Request that the median plan be 
		 * opened. Planning not necessarially
		 * started.
		 */
		void openMedianPlan ();
		
		/**
		 * Request that planning be resumed.
		 */
		void resumePlanning ();
		
		/**
		 * Request that a particular book mark 
		 * be opened.
		 */
		void openBookmark (DOMElement* bookmark);
	
	public:
	
		/**
		 * Create a PlannedResultBar widget over 
		 * the results for domain.
		 */
		PlannedResultBar (DOMDomain& domain, QWidget* parent = NULL)
			: QWidget (parent) {
			
			
		}
};

