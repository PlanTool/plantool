TEMPLATE = app
TARGET += 
DEPENDPATH += . 

INCLUDEPATH += .
INCLUDEPATH += /Users/owen/include

LIBS +=  -lxerces-c -L/Users/owen/lib

HEADERS += GanttBar.h \
           GanttChart.h \
           GanttNameArea.h \
           GanttBarContainer.h \
           GanttBarArea.h 

SOURCES += Main.cpp \
	   GanttBarContainer.cpp \
	   GanttChart.cpp \
	   GanttBarArea.cpp \
	   GanttNameArea.cpp \
	   GanttBar.cpp
