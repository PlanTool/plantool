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
 * $Id$
 *
 * This file is part of the Brazil Planner. Copyright NICTA 2006.
 *
 * This file is commercial in confidence. You should no be looking at
 * it unless you are an employee of NICTA, a student who has signed
 * their IP to NICTA, or you have signed an NDA covering the Brazil
 * source code. If you are not one of these people we will poke out
 * your eyes with a gerkhin while forcing you to sing the Brazilian
 * national anthem.
 *
 */

#ifndef stats_widget_h
#define stats_widget_h



#include <QString>
#include <QStringList>

#include <QPaintEvent>
#include <QBrush>
#include <QWidget>
#include <QFrame>
#include <QComboBox>

#include"../action-editor/HotAreaWidget.h"

#include"DOMFunction.h"
#include "../../model/impl/DOMDomain.h"
#include "BrazilState.h"
#include "BrazilStateList.h"

/** 
 * Displays a Brazil Action.
 *
 *  A Brazil Action is represented as a single ActionEffectWidget
 *  for display and manipulation of the Action's deterministic 
 *  effect. Together with a collection of OutcomeWidgets, for display
 *  and manipulation of the Action's single probabilistic collection of
 *  Outcomes.
 *
 *  The ActionEffectWidget is displayed in the top left hand corner. 
 *  The OutcomeWidgets are stacked vertically, benath and to the right of
 *  the AcitonEffectWidget.
 *
 *
 */

class StatsWidget : public QFrame {

    Q_OBJECT

private:

    QWidget* graph; // clipping widget for points

    DOMDomain* domain; // Just for getting properties and functions
    DOMPropertySet* props;

    typedef map<BrazilState*, HotAreaWidget*> Points; // Individual data points
    Points points; // Individual data points
    
    size_t maxTime; // Length of time axis
    double yMax; // Length of y axis
    double yMin; // Min of y axis

    QColor successColor;
    QColor failureColor;

    QComboBox* functionList;
    DOMDomain::FunctionMap functionMap;

public:

    StatsWidget(QWidget* parent, DOMDomain* domain);
    virtual ~StatsWidget();

    /**
     * QT System function called whenever the widget needs
     * to redraw itself.
     *
     * This simply draws the background of the widget,
     * the child widgets are responsible for their own
     * drawing.
     */
    virtual void paintEvent (QPaintEvent* event);
    virtual void showEvent ( QShowEvent * event );    
    virtual void resizeEvent (QResizeEvent*);

    bool locatePoint(BrazilState* s, HotAreaWidget* p);
    void repositionAll();
    
    void setDomain(DOMDomain* domai);
    void repopulateFunctionList();


public slots:
    
    /**
     * BrazilStateList will send signal to add states
     */
    void newState(BrazilState* s);

    void removeState(BrazilState* s);

    void pointChosen(HotAreaWidget* p);

    void changeFunction(const QString& f);

 signals:

    /**
     * Let the BrazilStateList holding GanttChart states know about this stochastic state
     */
    void viewState(BrazilState* s);


};
	    
#endif
