/*
 *  InvisibleLineEdit.cpp
 *  
 *
 *  Created by Owen Thomas on 9/06/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#include "OutcomeWidget.h"

#include <QFontMetrics>
#include <QStyle>
#include <QStyleOption>

//Update the size of the line edit to fit the text typed in
//only if the dynamicUpdateWidth flag is set
void InvisibleLineEdit::updateWidth (const QString& text)
{
	if(!dynamicUpdateWidth) return;
	int newWidth = fontMetrics().width (text);
	
	//If the newWidth is larger than the current width, then increase
	//the width by RESIZE_THRESHOLD
	if((newWidth + 1) > width()) {
		resize (newWidth + RESIZE_THRESHOLD, height());
	
	//Else, if the difference is greater than the resize
	//threshold, then decrease the width.
	} else if (width() - newWidth > RESIZE_THRESHOLD) {
		if(newWidth < minimumWidth()) newWidth = minimumWidth();
		
		//+2 because we want a little bit of buffer for presentation.
		resize (newWidth + 2, height());
	}
}


