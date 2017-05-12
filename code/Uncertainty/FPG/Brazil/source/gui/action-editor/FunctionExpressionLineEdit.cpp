/*
 *  FunctionExpressionLineEdit.cpp
 *  
 *
 *  Created by Owen Thomas on 21/11/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#include "FunctionExpressionLineEdit.h"
#include "../../model/impl/DOMDomain.h"
#include "../../model/impl/DOMFunction.h"

#include <QFontMetrics>
FunctionExpressionLineEdit::FunctionExpressionLineEdit (QWidget* parent, int autoResizeDelta) : QLineEdit (parent) {
	this->domain = NULL;
	this->listWidget = NULL;
	this->parser = NULL;
	this->parseExceptionCaught = false;
	ignoreListWidget = false;
	
	sizeHintMinimumWidth = 50;
	textSizeHint  = 0;
	
	QObject::connect (this, SIGNAL (editingFinished ()), this, SLOT (handleEditingFinished ()));
	QObject::connect (this, SIGNAL (textEdited (const QString&)), this,
		SLOT (handleTextEdited (const QString&)));
	
	setEnabled (false);
}

void FunctionExpressionLineEdit::parseExpression () {
	QString expressionString = text ();
	DOMFunctionExpression* oldExpression = currentExpression;
	try {
		currentExpression =  parser->parse (expressionString);
	} catch (ParseException* exception) {
		selectAll ();
		delete exception;
		parseExceptionCaught = true;
		return;
	}
	
	parseExceptionCaught = false;
	emit expressionChanged (currentExpression);
}

void FunctionExpressionLineEdit::setFunctionExpression (DOMFunctionExpression& expr) {
	currentExpression = &expr;
	FunctionExpressionStringBuilder builder;
	expr.visit (builder);
	setText (builder.getCurrentString ());
	parseExceptionCaught = false;
}

void FunctionExpressionLineEdit::handleTextEdited (const QString& newText) {
	QString prefix;
	int p = cursorPosition () - 1;
	
	while (p >= 0 && isFunctionDefChar (newText [p])) {
		prefix.prepend (newText [p --]);
	}
	
	if(prefix.size() && !ignoreListWidget) {
		listWidget->setPrefix (prefix);
		listWidget->setWindowModality(Qt::WindowModal);
		listWidget->setWindowFlags (Qt::Popup);
		listWidget->show ();
		
		QString uptoCursor = text ().left (p);
		int cursorX = fontMetrics().width (uptoCursor);
		
		QPoint globalPoint = mapToGlobal (QPoint(cursorX,height()));
		listWidget->move (globalPoint);
		listWidget->setFocus ();
	}
	else {
		listWidget->hide ();
		activateWindow ();
		setFocus ();
		if(!prefix.size()) ignoreListWidget = false;
	}
}

void FunctionExpressionLineEdit::handleEditingFinished () {
	if(!listWidget->isVisible ()) {
		parseExpression ();
	}
}

void FunctionExpressionLineEdit::handleListWidgetExit () {
	activateWindow ();  //this is required to enable us to get focus
						//this activates the window this widget belongs to.
	
	setFocus ();
	ignoreListWidget = true;	//This will stop us opening the list widget again 
								//until we finish defining the current funciton.
}

void FunctionExpressionLineEdit::handleListWidgetIgnoredKeyEvent (QKeyEvent* event) {
	keyPressEvent (event);
}

void FunctionExpressionLineEdit::handleListWidgetSelected (const QString& functionName) {
	activateWindow ();  //this is required to enable us to get focus
						//this activates the window this widget belongs to.
	
	setFocus ();

	QString prefix;
	int p = cursorPosition() - 1;
	
	while (p >= 0 && isFunctionDefChar (text() [p])) {
		prefix.prepend (text() [p --]);
	}
	
	
	QString newText = text ();
	
	QString appendText = functionName;
	appendText.remove (0, prefix.size ());
	
	int insertPosition = cursorPosition ();
	newText = newText.insert (insertPosition, appendText);
	
	setText (newText);
	
	setCursorPosition (insertPosition + appendText.size());
	
}

bool FunctionExpressionLineEdit::isFunctionDefChar (const QChar& testChar) {
	static const QChar minus('-');
	static const QChar lbracket('(');
	static const QChar rbracket (')');
	static const QChar plus ('+');
	static const QChar divide ('/');
	static const QChar multiply ('*');
	
	return !(testChar.isNumber () || testChar.isSymbol () 
		|| testChar.isSpace () || testChar == minus
		|| testChar == lbracket || testChar == rbracket 
		|| testChar == plus || testChar == divide 
		|| testChar == multiply);
}

void FunctionExpressionLineEdit::revert () {
	if(NULL == currentExpression) return;
	
	FunctionExpressionStringBuilder builder;
	
	this->currentExpression->visit (builder);
	setText (builder.getCurrentString ());
}
