/*
 *  Main.cpp
 *  
 *  Runs DOMWrapper subclass tests.
 *
 *  Created by Owen Thomas on 17/05/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#include <xercesc/dom/DOM.hpp>
#include <xercesc/dom/DOMWriter.hpp>
#include <xercesc/dom/DOMImplementation.hpp>

#include <xercesc/framework/LocalFileFormatTarget.hpp>

#include <xercesc/parsers/XercesDOMParser.hpp>
#include <xercesc/sax/HandlerBase.hpp>
#include <xercesc/util/PlatformUtils.hpp>

#include <cppunit/TestFixture.h>
#include <cppunit/extensions/HelperMacros.h>
#include <cppunit/TestResult.h>
#include <cppunit/TestSuite.h>
#include <cppunit/TestListener.h>
#include <cppunit/TextTestResult.h>
#include "DOMFunctionExpressionTest.h"

using namespace std;

XERCES_CPP_NAMESPACE_USE

int main (int argc, char** argv) {
	
	try {
		XMLPlatformUtils::Initialize();
	}
	catch (const XMLException& toCatch) {
		return 1;
	}
	
	DOMImplementation* impl = DOMImplementation::getImplementation ();
	DOMDocument* doc = impl->createDocument();
	
	CppUnit::TextTestResult listener;
	
	CppUnit::TestSuite suite;
	CppUnit::TestResult result;
	
	result.addListener (&listener);
	
	//Test Function Expression
	DOMFunctionExpressionValueTest* valueTest = new DOMFunctionExpressionValueTest (doc);
	
	suite.addTest (new CppUnit::TestCaller<DOMFunctionExpressionValueTest>("testCreation",
		&DOMFunctionExpressionValueTest::testCreation, valueTest));
		
	suite.addTest (new CppUnit::TestCaller<DOMFunctionExpressionValueTest>("testAssignment",
		&DOMFunctionExpressionValueTest::testAssignment, valueTest));
	
	DOMFunctionExpressionFunctionTest* functionTest = new DOMFunctionExpressionFunctionTest (doc);
	
	suite.addTest (new CppUnit::TestCaller<DOMFunctionExpressionFunctionTest> ("testCreation",
		&DOMFunctionExpressionFunctionTest::testCreation, functionTest));
	
	
	suite.addTest (new CppUnit::TestCaller<DOMFunctionExpressionFunctionTest> ("testAssignment",
		&DOMFunctionExpressionFunctionTest::testAssignment, functionTest));
	
	DOMFunctionExpressionInternalTest* internalTest = new DOMFunctionExpressionInternalTest (doc);
	
	
	suite.addTest (new CppUnit::TestCaller<DOMFunctionExpressionInternalTest> ("testCreation",
		&DOMFunctionExpressionInternalTest::testCreation, internalTest));
		
	
	/*
	DOMConditionTest* domConditionTest = new DOMConditionTest (doc);
	
	//Test Conditions.
	suite.addTest (new CppUnit::TestCaller<DOMConditionTest>("testCondition", 
		&DOMConditionTest::testCondition, domConditionTest));
	suite.addTest (new CppUnit::TestCaller<DOMConditionTest>("testInternalCondition", 
		&DOMConditionTest::testInternalCondition, domConditionTest));

	//Test Problem
	
	DOMProblemTest* domProblemTest = new DOMProblemTest (doc);
	suite.addTest (new CppUnit::TestCaller<DOMProblemTest>("testSinglePredicateInit", 
		&DOMProblemTest::testSinglePredicateInit, domProblemTest));
	
	suite.addTest (new CppUnit::TestCaller<DOMProblemTest>("testMultiplePredicateInit", 
		&DOMProblemTest::testMultiplePredicateInit, domProblemTest));
	suite.addTest (new CppUnit::TestCaller<DOMProblemTest>("testEmptyInit", 
		&DOMProblemTest::testEmptyInit, domProblemTest));
	suite.addTest (new CppUnit::TestCaller<DOMProblemTest>("testNullInit", 
		&DOMProblemTest::testNullInit, domProblemTest));

	*/
	
	//Run Test.
	suite.run (&result);
	
	listener.print (cout);

}
