#include "DOMAction.h"
DOMAction::DOMAction (DOMElement& element, DOMDomain& domain) {
	this->node = &element;
	this->domain = &domain;
	this->typeName = "DOMAction";
	
	//Create wrapper classes from element content.
	XMLCh* effectName = XMLString::transcode("effect");
	XMLCh* atStartName = XMLString::transcode("at-start");

	this->effect = createEffect (effectName);
	this->atStartEffect = createEffect(atStartName);
	this->precondition = createPrecondition ();
	this->overallCondition = createOverallCondition ();
	createProbabilisticChildren ();
}

DOMAction::DOMAction (char* name, DOMDomain& domain) {
	XMLCh* action = XMLString::transcode ("action");
	this->node = domain.getDOMElement()->getOwnerDocument()->createElement (action);
	this->domain = &domain;
	this->typeName = "DOMAction";

	setAttribute ("name", name);

	DOMDocument* doc = domain.getDOMElement()->getOwnerDocument();
	
	//Element names
	XMLCh* preconditionString = XMLString::transcode ("precondition");
	XMLCh* effectString = XMLString::transcode ("effect");
	XMLCh* atStartString = XMLString::transcode ("at-start");
	XMLCh* overallString = XMLString::transcode ("over-all");

	//Create new dom elements.
	DOMElement* preconditionElement = doc->createElement (preconditionString);
	DOMElement* overallElement = doc->createElement (overallString);
	DOMElement* effectElement = doc->createElement (effectString);
	DOMElement* atStartElement = doc->createElement (atStartString);
	
	//Append children.
	node->appendChild (preconditionElement); 
	node->appendChild (overallElement);
	node->appendChild (atStartElement);
	node->appendChild (effectElement);

	
	//Create (empty) wrapper classes for children.
	effect = new DOMEffectSet (*effectElement, domain);
	atStartEffect = new DOMEffectSet (*atStartElement, domain);
	precondition = new DOMRootCondition (preconditionElement, domain);
	overallCondition = new DOMRootCondition (overallElement, domain);
}


//
// Three constructor helper methods for creating child wrappers.
// 
// For precondition, overall condition and the deterministic effect
// set.
//
DOMRootCondition* DOMAction::createPrecondition () {
	static XMLCh* preconditionName = XMLString::transcode ("precondition");
	
	DOMNodeList* preConditionList = node->getElementsByTagName (preconditionName);
	
	if(preConditionList->getLength() > 0)
	{
		return new
			DOMRootCondition ((DOMElement*)preConditionList->item(0), *domain);
	}
	
	else {
		throw std::invalid_argument ("No precondition element");
	}
}

DOMRootCondition* DOMAction::createOverallCondition () {
	static XMLCh* overallName = XMLString::transcode ("over-all");
	
	DOMNodeList* overallList = node->getElementsByTagName (overallName);
	
	if(overallList->getLength() > 0)
	{
		return new
			DOMRootCondition ((DOMElement*)overallList->item(0), *domain);
	}
	
	else {
		throw std::invalid_argument ("No over-all element");
	}
}

DOMEffectSet* DOMAction::createEffect (XMLCh* effectName) {
	
	DOMNodeList* effectList = node->getElementsByTagName (effectName);
	
	if(effectList->getLength() > 0)
	{
		return new
			DOMEffectSet (*(DOMElement*)effectList->item(0), *domain);
	}
	
	else {
	    DOMDocument* doc = domain->getDOMElement()->getOwnerDocument();
	    DOMElement* effectElement = doc->createElement (effectName);
	
	    //Append children.
	    node->appendChild (effectElement);
	    return new DOMEffectSet (*effectElement, *domain);
	}	
}

void DOMAction::createProbabilisticChildren () {
	static XMLCh* probabilisticName = XMLString::transcode ("probabilistic");
	
	DOMNodeList* probabilisticChildNodes = node->getElementsByTagName (probabilisticName);
	for(unsigned int i = 0; i < probabilisticChildNodes->getLength (); i++) {
		DOMElement* next = (DOMElement*) probabilisticChildNodes->item (i);
		
		DOMProbabilistic* probabilistic = new DOMProbabilistic (next, this);;
		probabilisticChildren.push_back (probabilistic);
	}
}


DOMAction::~DOMAction () {
	//Delete precondition
	this->node->removeChild (precondition->getDOMElement ());
	delete precondition;
	//Delete overall condition
	this->node->removeChild (overallCondition->getDOMElement ());
	delete overallCondition;
	//Delete effect
	this->node->removeChild (effect->getDOMElement ());
	delete effect;
	//Delete at-start effects
	this->node->removeChild (atStartEffect->getDOMElement ());
	delete atStartEffect;
	//Delete probabilistic.
	list<DOMProbabilistic*>::iterator it;
	for(it = probabilisticChildren.begin (); it != probabilisticChildren.end();
		it++) {
		this->node->removeChild ( (*it)->getDOMElement ());
		delete (*it);
	}
	//If I have no parent node, release my node.
	if(node->getParentNode () == NULL) {
		node->release ();
	}
}

DOMDomain* DOMAction::getDomain () {
	return domain;
}	

list<DOMProbabilistic*>& DOMAction::getProbabilisticChildren () {
	return probabilisticChildren;
}

void DOMAction::addProbabilistic (DOMProbabilistic* probabilistic) {
	unsigned int sizeBefore = probabilisticChildren.size();
	probabilisticChildren.push_back (probabilistic);
	if(probabilisticChildren.size() != sizeBefore) {
		node->appendChild (probabilistic->getDOMElement ());
				
		//notify listeners...
		list<ActionListener*>::iterator actionListener;
		for(actionListener = actionListeners.begin(); 
			actionListener != actionListeners.end();
			actionListener ++) {
			
			(*actionListener)->probabilisticAdded (probabilistic);
		}
	}
}

void DOMAction::removeProbabilistic (DOMProbabilistic* probabilistic) {
	unsigned int sizeBefore = probabilisticChildren.size();
	probabilisticChildren.remove (probabilistic);
	if(probabilisticChildren.size() != sizeBefore) {
		node->removeChild (probabilistic->getDOMElement ());
		
		//notify listeners...
		list<ActionListener*>::iterator actionListener;
		for(actionListener = actionListeners.begin(); 
			actionListener != actionListeners.end();
			actionListener ++) {
			
			(*actionListener)->probabilisticRemoved (probabilistic);
		}
	}
}


DOMRootCondition* DOMAction::getPrecondition () {
	return precondition;
}

DOMRootCondition* DOMAction::getOverallCondition () {
	return overallCondition;
}

DOMEffectSet* DOMAction::getEffectSet () {
	return effect;
}

DOMEffectSet* DOMAction::getAtStartEffectSet () {
        return atStartEffect;
}

void DOMAction::addActionListener (ActionListener* listener) {
	actionListeners.push_back (listener);
}

void DOMAction::removeActionListener (ActionListener* listener) {
	actionListeners.remove (listener);
}


char* DOMAction::getBackgroundColour () {
	return getAttribute("backgroundColour");
}
		
void DOMAction::setBackgroundColour(char* colour)
{
	setAttribute ("backgroundColour", colour);
	list<ActionListener*>::iterator actionListener;
	for(actionListener = actionListeners.begin(); actionListener != actionListeners.end();
		actionListener ++) {
		(*actionListener)->backgroundColourChanged ();
	}
			
}

char* DOMAction::getName () {
	return getAttribute ("name");
}
		
void DOMAction::setName (char* name) {	
	setAttribute ("name", name);
	list<ActionListener*>::iterator actionListener;
	for(actionListener = actionListeners.begin(); actionListener != actionListeners.end();
		actionListener ++) {
		(*actionListener)->nameChanged ();
	}
}

char* DOMAction::getTypeName () {
	return typeName;
}


