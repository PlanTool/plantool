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
 *  BrazilStateList.h
 *  
 *
 *  Created by Owen Thomas on 7/12/06.
 *  
 *
 */
#ifndef brazil_state_list
#define brazil_state_list

#include <QObject>
#include <list>

class BrazilState;
using namespace std;

/**
 * A BrazilStateList maintains a collection of Brazil States
 * inside a buffer with a fixed maximum size. When the buffer
 * fills adding adding an additional state will cause the oldest
 * state in the buffer to be deleted.
 *
 * This class emits two signals: aboutToDelete, emitted before
 * the state is deleted and stateAdded, emitted when a state has
 * successfully been added to the list. Where adding a state triggers
 * deletion, aboutToDelete will be emitted before stateAdded.
 */
class BrazilStateList : public QObject {
	
	Q_OBJECT
	
	private:
		int bufferSize;
		list<BrazilState*> states;
		
	signals:
		
		/**
		 * Indicates that state is about to be deleted
		 * from this list and memory.
		 */
		void aboutToDelete (BrazilState* state);
		
		/**
		 * Indicates that state has been added to this list.
		 */
		void stateAdded (BrazilState* state);
		
	public slots:
		
		/**
		 * Adds a copy of state to the internal list
		 * of states. Emits the stateAdded signal when the state
		 * has been added.
		 */
		void addState (BrazilState* state);
		
	public:
		
		/**
		 * Construct an empty BrazilStateList.
		 * @param bufferSize, the size of the buffer which must be
		 * greater than 0.
		 */
		BrazilStateList (int bufferSize);
		
		/**
		 * Deletes the contents of the buffer. This emits
		 * aboutToDelete signals for each state in the list
		 * before it is deleted.
		 */
		virtual ~BrazilStateList ();
		
		/**
		 * Returns the maximum buffer size - not the current count
		 * of states in the list.
		 */
		virtual int getBufferSize ();

		/**
		 * Returns the underlying list of states. This is returned as
		 * a reference and so the contents may change during iteration of 
		 * this list.
		 */
		virtual const list<BrazilState*>& getStates ();

		/**
		 * Deletes all the states in the state list.
		 */
		virtual void clear ();
};

#endif

