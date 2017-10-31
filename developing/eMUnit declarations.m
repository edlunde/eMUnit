(* ::Package:: *)

(* ::Section:: *)
(*Declarations*)


(* ::Subsection:: *)
(*Asserts*)


AssertEquals::usage = "AssertEquals[value, expression] returns Null if expression \
evaluates to value. Otherwise it throws an AssertEquals-exception to be caught \
by RunTest.";


AssertMatch::usage = "AssertMatch[form, expression] returns Null if expression \
matches form. Otherwise it throws an AssertMatch-exception to be caught \
by RunTest.";


AssertTrue::usage = "AssertTrue[expression] returns Null if expression \
evaluates to True. Otherwise it throws an AssertTrue-exception to be caught \
by RunTest.";


AssertEqualsN::usage = "AssertEquals[value, expression, Tolerance -> 0.001] returns \
Null if expression evaluates numerically to within Tolerance of value. Otherwise it \
throws an AssertEquals-exception to be caught by RunTest.";


AssertMatchN::usage = "AssertMemberN[form, expression, Tolerance -> 0.001] returns \
Null if expression matches form with numerical values matching within Tolerance. \
Otherwise it throws an AssertMember-exception to be caught by RunTest.";


AssertMessage::usage = "AssertMessage[message, expr] evaluates expr and checks if \
the message is generated (and quiets it). If not, it throws an AssertMessage-exception \
to be caught by RunTest.\n\
AssertNoMessage[expr] evaluates expr. If a message is sent,\
 it throws an\[NonBreakingSpace]AssertMessage-exception to be caught by RunTest.";
AssertNoMessage::usage = AssertMessage::usage;


(* ::Subsection::Closed:: *)
(*Begin, List, Add, Delete*)


ListTests::usage = "ListTests[suite] lists the names of all installed tests in suite.\n\
ListTests[] lists the tests in the current suite";

AddTest::usage = "AddTest[suite, name, test] will add a test with a given name\
 to the suite. \n\
AddTest[name, test] adds a test to the current suite.";

AddSuite::usage = "AddSuite[mainSuite, subsuite] adds subsuite under mainSuite. \
Causes all tests in subsuite to be run when those in mainSuite is.\n\
AddSuite[subsuite] adds subsuite under the current suite.";

DeleteTest::usage = "DeleteTest[suite, name] deletes a given test from the suite.\n\
DeleteTest[name] deletes a given test from the current suite";

BeginSuite::usage = "BeginSuite[suite] sets the current suite until next EndSuite[].\
It is recommended that corresponding BeginSuite and EndSuite be placed in the same cell.";
EndSuite::usage = "EndSuite[] sets the current suite to whatever it was before the\
last BeginSuite[].";

BeginSubsuite::usage = "BeginSubsuite[subsuite] requires a suite to be set and runs\
AddSuite[subsuite] followed by BeginSuite[subsuite]. Should be used in conjunction\
with EndSuite[] just like BeginSuite.";


(* ::Subsection::Closed:: *)
(*RunTest*)


RunTest::usage = "RunTest[suite, stringPattern] runs all tests matching stringPattern \
and formats the output.\n\
RunTest[suite] runs all tests in the suite and formats the output.\n\
RunTest[] runs all tests in the current suite.";


(* ::Subsection::Closed:: *)
(*Test package*)


TestEMUnitPackage::usage = "TestEMUnitPackage[] runs all unit tests \
for the eMUnit package.";


(* ::Subsection::Closed:: *)
(*Messages*)


eMUnitMessages::usage = "eMUnitMessages::tag - Messages used in the eMUnit package.";
