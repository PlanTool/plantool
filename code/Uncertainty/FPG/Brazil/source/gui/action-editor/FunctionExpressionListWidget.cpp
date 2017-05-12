/*
 *  FunctionExpressionListWidget.cpp
 *  
 *
 *  Created by Owen Thomas on 23/11/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#include "FunctionExpressionListWidget.h"
#include <QList>
#include <QListWidgetItem>

void FunctionExpressionListWidget::setPrefix (const QString& prefix) {
	
	
	for(int i = 0; i < count(); i++) {
		const QListWidgetItem* string = item (i);
		setItemHidden (string, !string->text().startsWith (prefix));
		setItemSelected (string, isItemSelected (string) && !isItemHidden (string));
	}
	
	bool selectedItemVisible = false;
	
	//Is there a selected item that is visible?
	if(selectedItems ().count () == 1) 
		selectedItemVisible = 
			!isItemHidden (selectedItems().at (0));
	
	//Nothing selected that is visible, set the first visible item to
	//be selected.
	if(!selectedItemVisible) {
		for(int i = 0; i < count(); i++) {
			if(!isItemHidden (item(i))) {
				setItemSelected (item(i), true);
				selectedItemVisible = true;
				break;
			}
		}
	}
	
	
	//if selectedItemVisible is STILL false, then there are no items visible.
	if(!selectedItemVisible) {
		emit exited ();
		hide ();
	}
}

void FunctionExpressionListWidget::keyPressEvent (QKeyEvent* event) {
	if(event->key () == Qt::Key_Escape) {
		emit exited ();
		hide ();
	}
	else if(event->key() == Qt::Key_Up || event->key() == Qt::Key_Down) {
		QListWidget::keyPressEvent (event);
	}
	else if (event->key() == Qt::Key_Return || event->key() == Qt::Key_Enter) {
		if(selectedItems().count()) {
			emit selected (selectedItems().at(0)->text ());
			hide ();
		}
	}
	else {
		emit ignoreKeyEvent (event);
	}
}
