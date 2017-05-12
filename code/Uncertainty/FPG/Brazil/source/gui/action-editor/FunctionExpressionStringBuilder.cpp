/*
 *  FunctionExpressionStringBuilder.cpp
 *  
 *
 *  Created by Owen Thomas on 22/11/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#include "FunctionExpressionStringBuilder.h"


FunctionExpressionStringBuilder::FunctionExpressionStringBuilder () {
	depth = 0;
}

double FunctionExpressionStringBuilder::visitValue (DOMFunctionExpressionValue* expr) {
	QString valueString;
	valueString.setNum (expr->getValue ());
	currentString.append (valueString);
	return expr->getValue ();
}

double FunctionExpressionStringBuilder::visitFunction (DOMFunctionExpressionFunction* expr) {
	currentString.append (expr->getFunction()->getName ());
	return 0.0;
}

double FunctionExpressionStringBuilder::visitInternal (DOMFunctionExpressionInternal* expr) {
	bool drawCloseBracket = false;
	if(depth > 0 
		&& (parentOperator == DOMFunctionExpressionInternal::multiply
		|| parentOperator == DOMFunctionExpressionInternal::divide) 
		&& (expr->getOperator() == DOMFunctionExpressionInternal::add
		|| expr->getOperator() == DOMFunctionExpressionInternal::subtract)) {
	
		currentString.append ("(");	
		drawCloseBracket = true;
	}
	
	parentOperator = expr->getOperator ();
	depth ++;
	expr->getLeftHandSide ()->visit (*this);
	switch (expr->getOperator ()) {
		case DOMFunctionExpressionInternal::add:
			currentString.append (" + ");
			break;
		case DOMFunctionExpressionInternal::subtract:
			currentString.append (" - ");
			break;
		case DOMFunctionExpressionInternal::multiply:
			currentString.append (" * ");
			break;
		case DOMFunctionExpressionInternal::divide:
			currentString.append (" / ");
			break;
	}
	parentOperator = expr->getOperator ();
	expr->getRightHandSide ()->visit (*this);
	if(drawCloseBracket) currentString.append (")");
	depth --;
	
	return 0.0;
}


