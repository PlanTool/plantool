/*
 *  BrazilStateList.cpp
 *  
 *
 *  Created by Owen Thomas on 7/12/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#include "BrazilStateList.h"

#include "../planner/BrazilState.h"

BrazilStateList::BrazilStateList (int bufferSize) {
	assert(bufferSize > 0);
	this->bufferSize = bufferSize;
}

BrazilStateList::~BrazilStateList () {

	//Clear out the buffer and emit aboutToDelete
	//signals.
	while(states.size()) {
		BrazilState* frontState = states.front ();
		emit aboutToDelete (frontState);
		states.pop_front ();
		delete frontState;
	}
}
	
	
void BrazilStateList::addState (BrazilState* state) {
	//if the buffer is full then delete the first state.
        if (states.size () == (size_t)bufferSize) {
		BrazilState* frontState = states.front ();
		emit aboutToDelete (frontState);
		states.pop_front ();
		delete frontState;
	}
	
	//Add the new state onto the end, ensuring that if it is
	//to be deleted, it will be deleted after all states
	//currently in the list.
	states.push_back (state);
	
	emit stateAdded (states.back());
}

int BrazilStateList::getBufferSize () {
	return bufferSize;
}

const list<BrazilState*>& BrazilStateList::getStates () {
	return states;
}

void BrazilStateList::clear () {
	//Clear out the buffer and emit aboutToDelete
	//signals.
	while(states.size()) {
		BrazilState* frontState = states.front ();
		emit aboutToDelete (frontState);
		states.pop_front ();
		delete frontState;
	}
}

