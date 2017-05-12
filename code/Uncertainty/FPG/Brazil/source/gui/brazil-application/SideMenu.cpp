/*
 *  SideMenu.cpp
 *  
 *
 *  Created by Owen Thomas on 5/09/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#include "SideMenu.h"

SideMenu::SideMenu (QWidget* parent) : QWidget (parent) {

	QVBoxLayout* menuLayout = new QVBoxLayout ();
	setLayout (menuLayout);
	setAutoFillBackground (true);
	this->backgroundColour = QColor ("white");
	p = palette();
	p.setColor (QPalette::Active, QPalette::Window, backgroundColour);
	setPalette (p);
	menuLayout->addStretch (10);
	menuLayout->setMargin (0);
	menuLayout->setSpacing (0);
	itemCount = 0;
	currentSelectedItem = NULL;
}

void SideMenu::selected (SideMenuItem* item) {
	if(item != currentSelectedItem) {
		if(currentSelectedItem)
			currentSelectedItem->deselect ();
		
		currentSelectedItem = item;
		
		emit selectedWidgetChanged (itemWidgets [item] );
	}
}

SideMenuItem* SideMenu::addItem (QString label, QWidget* item) {
	SideMenuItem* menuItem = new  SideMenuItem (label);
	((QBoxLayout*)layout ())->insertWidget (itemCount ++, menuItem);
	itemWidgets [menuItem] = item;
	if(!currentSelectedItem) {
		currentSelectedItem = menuItem;
	}
	else {
		menuItem->deselect ();
	}
		
	QObject::connect (menuItem, SIGNAL (selected(SideMenuItem*)),
		this, SLOT (selected (SideMenuItem*)));
	
	return menuItem;
}

QWidget* SideMenu::setItem (SideMenuItem* item, QWidget* widget) {
	QWidget* oldWidget = itemWidgets [item];
	itemWidgets [item] = widget;
	return oldWidget;
}

QWidget* SideMenu::currentSelectedWidget () {
	if(!currentSelectedItem) return NULL;
	return itemWidgets [currentSelectedItem];
}
