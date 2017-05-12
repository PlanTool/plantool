/*
 *  FunctionExpressionParser.cpp
 *  
 *
 *  Created by Owen Thomas on 21/11/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#include "FunctionExpressionParser.h"

#include "../../model/impl/DOMFunctionExpression.h"
#include "../../model/impl/DOMDomain.h"
#include "../../model/impl/DOMFunction.h"

QStringList FunctionExpressionParser::toTokens (QString& string) {
	string.replace (QString ("("), QString (" ( "));
	string.replace (QString (")"), QString (" ) "));
	string.replace (QString ("+"), QString (" + "));
	string.replace (QString ("-"), QString (" - "));
	string.replace (QString ("*"), QString (" * "));
	string.replace (QString ("/"), QString (" / "));
	
	return string.split (" ",QString::SkipEmptyParts);
}

DOMFunctionExpression* FunctionExpressionParser::parseTerminal (QString& string) {
	DOMFunction* function = domain->getFunction (string.toUtf8().data());
	if(NULL == function) {
		//assume terminal is a numeric value..
		bool ok = false;
		double value = string.toDouble (&ok);

		if(!ok) {
			throw new ParseException;
		}
		return new DOMFunctionExpressionValue (*(domain->getDOMElement()->getOwnerDocument ()), value);
	}
	return new DOMFunctionExpressionFunction (*function);
}

bool FunctionExpressionParser::isTerminal (QStringList& tokens) {
	return tokens.size() == 1 || tokens.at (1) == ")";
}

DOMFunctionExpression* FunctionExpressionParser::parseInternal (QStringList& tokens) {
	if(tokens.count () == 0) {
		throw  new ParseException;
	}
	
	if(isTerminal (tokens)) {
		DOMFunctionExpression* expr = parseTerminal (tokens.front ());
		tokens.pop_front (); // remove terminal. 
		return expr;
	}
	
	DOMFunctionExpressionInternal* returnExpression;
	DOMFunctionExpression* lhs;
	DOMFunctionExpression* rhs;
	
	DOMFunctionExpressionInternal::Operator newOp;
	
	if("(" == tokens.front ()) {
		lhs = parseBracketedExpression (tokens);
		if(tokens.count() == 0) {
			return lhs;
		}
	}
	else {
		lhs = parseTerminal (tokens.front ());
		tokens.pop_front ();
	}
	
	if(tokens.count () == 0) {
		throw new ParseException;
	}
	
	if(tokens.front() == "*") {
		newOp = DOMFunctionExpressionInternal::multiply;
	}
	else if(tokens.front() == "/") newOp = DOMFunctionExpressionInternal::divide;
	else if (tokens.front() == "-") newOp = DOMFunctionExpressionInternal::subtract;
	else newOp = DOMFunctionExpressionInternal::add;
	
	tokens.pop_front ();
	
	
	while (newOp != DOMFunctionExpressionInternal::add) {
		
		if(tokens.count () == 0) throw new ParseException;
		
		if(tokens.front () == "(") {
			rhs = parseBracketedExpression (tokens);	
		}
		else {
			rhs  = parseTerminal (tokens.front ());
			tokens.pop_front ();
		}
		
		returnExpression =  
			new DOMFunctionExpressionInternal ( *(domain->getDOMElement()->getOwnerDocument ()),
				newOp, *lhs, *rhs); 
				
		if(!tokens.count ()) return returnExpression;
		else {
			lhs = returnExpression;
		}
		if(tokens.front() == "*") {
			newOp = DOMFunctionExpressionInternal::multiply;
		}
		else if(tokens.front() == "/") newOp = DOMFunctionExpressionInternal::divide;
		else if (tokens.front() == "-") newOp = DOMFunctionExpressionInternal::subtract;
		else newOp = DOMFunctionExpressionInternal::add;
		
		tokens.pop_front ();
	}
	
	rhs = parseInternal (tokens);
	
	returnExpression = new DOMFunctionExpressionInternal ( *(domain->getDOMElement()->getOwnerDocument ()),
				newOp, *lhs, *rhs);

	return returnExpression;
}

DOMFunctionExpression* FunctionExpressionParser::parseBracketedExpression (QStringList& tokens) {
	if(tokens.count () == 0) throw new ParseException;
	if("(" != tokens.front ()) throw new ParseException;
	tokens.pop_front ();
	
	int bracketCount = 1;
	QStringList bracketedExpressionTokens;
	
	while (bracketCount > 0) {
		if(tokens.count () == 0) throw new ParseException;
		if(tokens.front().compare (")") == 0) {
			bracketCount --;
			if(bracketCount != 0) {
				bracketedExpressionTokens.push_back (tokens.front());
			}
		}
		else if(tokens.front().compare("(") == 0) {
			bracketCount ++;
			bracketedExpressionTokens.push_back (tokens.front());
		}
		else {
			bracketedExpressionTokens.push_back (tokens.front());
		}
		tokens.pop_front ();
	}
	
	
	return parseInternal (bracketedExpressionTokens);
}
