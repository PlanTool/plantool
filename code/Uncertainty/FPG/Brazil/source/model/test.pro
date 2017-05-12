TEMPLATE = app
TARGET += 
DEPENDPATH += . impl 
INCLUDEPATH += . impl 
INCLUDEPATH += ${HOME}/include
LIBS +=  -lxerces-c -lcppunit -L${HOME}/lib 

HEADERS += *.h impl/*.h impl/test/*.h
SOURCES +=  impl/*.cpp impl/test/*.cpp
