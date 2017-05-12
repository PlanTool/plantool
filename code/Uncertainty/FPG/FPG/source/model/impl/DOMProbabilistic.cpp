/*
 *  DOMProbabilistic.cpp
 *  
 *
 *  Created by Owen Thomas on 18/05/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#include "DOMAction.h"
#include "DOMProbabilistic.h"

// For dealing with errors in floating point numbers
#define DOM_PROB_EPSILON 0.001

#include <string>
#include <sstream>
#include <iostream>


DOMProbabilistic::DOMProbabilistic (DOMElement* node, DOMAction* parent)
{
	this->node = node;
	this->parent = parent;
	this->doc = node->getOwnerDocument ();

	//Construct list of DOMOutcome children.
	DOMNodeList* children = this->node->getChildNodes ();
	for(unsigned int i = 0; i < children->getLength(); i++)
	{
		if(children->item(i)->getNodeType() == DOMNode::ELEMENT_NODE)
		{
			if(DOMOutcome::isOutcome ((DOMElement*)children->item(i))) {
				DOMOutcome* outcome = new DOMOutcome ((DOMElement*)children->item (i), this);
				outcomeProbabilities [outcome] = outcome->getProbability ();
				
			}
		}
	}
	
	mostLikelyOutcome = findMostLikelyOutcome();
	listener = NULL;
}


DOMProbabilistic::DOMProbabilistic (DOMDocument* doc, DOMAction* parent) 
{
	this->node = doc->createElement (XMLString::transcode ("probabilistic"));
	this->parent = parent;
	this->doc = doc;
	listener = NULL;
}


void DOMProbabilistic::setOutcomes (map<DOMOutcome*, double> probabilities) {
	
	
	
	//Check that newProbabilities sums to approx 1.0
	double sum = 0;
	map<DOMOutcome*, double>::iterator it = probabilities.begin ();
	for(; it != probabilities.end (); it++) {
		sum += it->second;
	}

	
	//Fix with global def. Can't find this..
	if ((sum < 0.99 || sum > 1.01) && !probabilities.size() == 0) 
		throw std::invalid_argument ("Outcome probabilities do not sum to 1."); 
	
	//Here we need to set the underlying dom element bits...
	//And notify the audience of what's going on!
	
	//Add new outcomes.
	for(it = probabilities.begin (); it != probabilities.end ();it++) {
		
		//This is a new outcome that is being added.
		if(!outcomeProbabilities.count (it->first)) {
			this->node->appendChild (it->first->getDOMElement ());
			it->first->setProbabilistic (this);
			if(listener) listener->outcomeAdded (it->first);
		}
	}
	
	
	//Find outcomes being removed.
	for(it = outcomeProbabilities.begin (); it != outcomeProbabilities.end(); it++) {
		
		//This is an outcome that has been removed.
		if(!probabilities.count (it->first)) {
			this->node->removeChild (it->first->getDOMElement ());
			if(listener) listener->outcomeRemoved (it->first);
			//delete it->first;
		} 
	}	
	
	XMLCh* probabilityString = XMLString::transcode ("probability");

	
	//Ensure that probabilities are updated.
	for(it = probabilities.begin (); it != probabilities.end (); it ++) {
		ostringstream stream;
	
		if(it->second != outcomeProbabilities [it->first]) {
			stream << it->second << std::ends;

			it->first->getDOMElement ()->setAttribute  (probabilityString,
				XMLString::transcode(stream.str().c_str()));  
				
			listener->probabilityChanged (it->first, it->second);
		}
	}
	
	
	
	XMLString::release (&probabilityString);
	
	outcomeProbabilities = probabilities;

	mostLikelyOutcome = findMostLikelyOutcome ();
}

/**
 * Scan through all outcomes for most likely.
 * Not sensitive to cummulative prob.
 * Side effect is to set class var mostLikelyOutcome
 * @author daa 
 */
DOMOutcome* DOMProbabilistic::findMostLikelyOutcome() 
{
	DOMOutcome* mlo = NULL;
	map<DOMOutcome*, double>::iterator it;
	for(it = outcomeProbabilities.begin (); it != outcomeProbabilities.end(); it++) {
		if(!mlo) mlo = it->first;
		else if(it->second > outcomeProbabilities [mlo]) mlo = it->first;
	}

	return mlo;
}


/** 
 * Assumes probabilities sum to 1.0
 * @author daa
 */
DOMOutcome* DOMProbabilistic::sampleOutcome() {
    
    double thresh = random()/(double)RAND_MAX;
    assert(thresh <= 1.0);
    map<DOMOutcome*, double>::const_iterator it;
    double sum = 0;
    int eventCount=0;

    //cout<<"Sample loop for "<<parent->getName()<<" with "<<outcomeProbabilities.size()<<" events"<<endl;

    for (it = outcomeProbabilities.begin(); it != outcomeProbabilities.end(); it++) {
	sum += it->second;
	eventCount++;
	if (sum >= thresh) break; 
    }
    
    if (sum > 1.0001) {
	cout.precision(10);
	cout<<"Probabilities sum to "<<sum<<" after "<<eventCount<<" events and thresh="<<thresh<<endl;
	throw std::invalid_argument ("Probabilities sum to greater than 1 in sample()"); 
    }
    if (it == outcomeProbabilities.end()) throw std::runtime_error("Sampled outcome is iterator end()");

    assert(it->first != NULL);

    return it->first;
    
}
