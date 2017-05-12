

#include "DOMFunctionExpression.h"
#include "DOMDomain.h"
DOMFunctionExpression* DOMFunctionExpression::createExpression (DOMElement& element, DOMDomain& domain) {
	if(DOMFunctionExpressionValue::canRead (element)) return new DOMFunctionExpressionValue (element);
	if(DOMFunctionExpressionFunction::canRead (element)) return new DOMFunctionExpressionFunction (element, domain);
	if(DOMFunctionExpressionInternal::canRead (element)) return new DOMFunctionExpressionInternal (element, domain);
	return NULL;
}

DOMFunctionExpressionValue::DOMFunctionExpressionValue (DOMElement& element) {
	this->node = &element;
}

DOMFunctionExpressionValue::DOMFunctionExpressionValue (DOMDocument& doc, double value) {
	this->node = doc.createElement (XMLString::transcode ("number"));
	
	std::stringstream sstream;
	sstream << value;
	
	this->node->setTextContent (XMLString::transcode (sstream.str().c_str()));
}

void DOMFunctionExpressionValue::setValue (double value)
{
	std::stringstream sstream;
	sstream << value;
	
	this->node->setTextContent (XMLString::transcode (sstream.str().c_str()));
}

double DOMFunctionExpressionValue::getValue ()
{
        return strtod(XMLString::transcode (this->node->getTextContent ()), NULL);

}

DOMFunctionExpressionFunction::DOMFunctionExpressionFunction (DOMElement& element, DOMDomain& domain) {
	this->node = &element;
	this->function = domain.getFunction (getAttribute("name"));
}

DOMFunctionExpressionFunction::DOMFunctionExpressionFunction (DOMFunction& function)
{
	this->node = function.getDOMElement()->getOwnerDocument()->createElement (XMLString::transcode ("function"));
	
	this->node->setAttribute (XMLString::transcode ("name"), XMLString::transcode (function.getName()));
}

void DOMFunctionExpressionFunction::setFunction (DOMFunction& function)
{
	this->node = function.getDOMElement()->getOwnerDocument()->createElement (XMLString::transcode ("function"));
	
	this->node->setAttribute (XMLString::transcode ("name"), XMLString::transcode (function.getName()));
	this->function = &function;
}

DOMFunction* DOMFunctionExpressionFunction::getFunction ()
{
	return function;
}

DOMFunctionExpressionInternal::DOMFunctionExpressionInternal (DOMElement& element, DOMDomain& domain) {
	this->node = &element;
	this->lhs = NULL;
	this->rhs = NULL;
	this->domain = &domain;
	
	op = createOperator ();
	
	DOMNodeList* children = element.getChildNodes ();
	for(unsigned int i = 0; i < children->getLength(); i++) {
		if(readChildNode (children->item (i))) break;
	}
}

DOMFunctionExpressionInternal::DOMFunctionExpressionInternal (DOMDocument& document, Operator op, 
	DOMFunctionExpression& lhs, DOMFunctionExpression& rhs) {
		
		this->node = document.createElement (XMLString::transcode ("functionExpression"));
		this->op = op;
		setAttribute ("type", writeOperator ());
		
		this->lhs = &lhs;
		this->rhs = &rhs;
		this->domain = NULL;
		
		this->node->appendChild (lhs.getDOMElement());
		this->node->appendChild (rhs.getDOMElement());
}

DOMFunctionExpressionInternal::Operator DOMFunctionExpressionInternal::getOperator () {
	return op;
}

void DOMFunctionExpressionInternal::setOperator (Operator op) {
	this->op = op;
	setAttribute ("type", writeOperator());
}
DOMFunctionExpression* DOMFunctionExpressionInternal::getLeftHandSide () {
	return lhs;
}

DOMFunctionExpression* DOMFunctionExpressionInternal::getRightHandSide () {
	return rhs;
}

void DOMFunctionExpressionInternal::setLeftHandSide (DOMFunctionExpression& lhs) {
	this->node->replaceChild (lhs.getDOMElement(), this->lhs->getDOMElement ());
	delete this->lhs;
	this->lhs = &lhs;
}

void DOMFunctionExpressionInternal::setRightHandSide (DOMFunctionExpression& rhs) {
	this->node->replaceChild (rhs.getDOMElement(), this->rhs->getDOMElement ());
	delete this->rhs;
	this->rhs = &rhs;
}

DOMFunctionExpressionInternal::Operator DOMFunctionExpressionInternal::createOperator () {
	char* operatorName = getAttribute ("type");
	
	DOMFunctionExpressionInternal::Operator returnOperator;
	
	if(XMLString::equals (operatorName, "add")) {
		returnOperator = DOMFunctionExpressionInternal::add;
	}
	else if (XMLString::equals (operatorName, "subtract")) {
		returnOperator = DOMFunctionExpressionInternal::subtract;
	}
	else if (XMLString::equals (operatorName, "multiply")) {
		returnOperator = DOMFunctionExpressionInternal::multiply;
	}
	
	else if (XMLString::equals (operatorName, "divide")) {
		returnOperator = DOMFunctionExpressionInternal::divide;
	}
		
	XMLString::release (&operatorName);
	
	return returnOperator;
}

bool DOMFunctionExpressionInternal::readChildNode (DOMNode* node) {
	//if lhs and rhs expressions are defined, return true indiciating
	//that we have read both expressions.
	if(lhs != NULL && rhs != NULL) return true;
	
	//if lhs is NULL and node is a dom element then attempt to create lhs.
	if(lhs == NULL && node->getNodeType ()  == DOMNode::ELEMENT_NODE) {
		lhs = createExpression (*((DOMElement*)node), *domain);
	}
	//else if we have created a lhs expression then attempt to create a rhs expression.
	else if(rhs == NULL && lhs != NULL  && 	node->getNodeType () == DOMNode::ELEMENT_NODE) {
		rhs = createExpression (*((DOMElement*)node), *domain);
		if(rhs)	return true;
	}
	return false;
}

char* DOMFunctionExpressionInternal::writeOperator () {
	if(op == DOMFunctionExpressionInternal::add) return "add";
	if(op == DOMFunctionExpressionInternal::subtract) return "subtract";
	if(op == DOMFunctionExpressionInternal::divide) return "divide";
	else return "multiply";
}
