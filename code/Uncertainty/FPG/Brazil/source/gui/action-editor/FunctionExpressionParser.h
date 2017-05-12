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
 *  FunctionExpressionParser.h
 *  
 *
 *  Created by Owen Thomas on 21/11/06.
 *  
 *
 */

class DOMDomain;
class DOMFunctionExpression;
#include <QStringList>

#include <iostream>

using namespace std;

class FunctionExpressionParser {

	private:
		DOMDomain* domain;
		
	protected:
		DOMFunctionExpression* parseTerminal (QString& string);
		
		QStringList toTokens (QString& string);
		
		DOMFunctionExpression* parseInternal (QStringList& tokens);
		
		DOMFunctionExpression* parseBracketedExpression (QStringList& tokens);
		
		bool isTerminal (QStringList& tokens);
		
		
	public:
		FunctionExpressionParser (DOMDomain& domain) {
			this->domain = &domain;
		}
	
		
		DOMFunctionExpression* parse (QString& string) {
			QStringList tokens = toTokens (string);
			return parseInternal (tokens);
		}
};

#include <stdexcept>

class ParseException : public exception {

};
