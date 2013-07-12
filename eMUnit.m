(* ::Package:: *)

(* ::Section::Closed:: *)
(*Declarations*)


BeginPackage["eMUnit`"];


AssertEquals::usage = "AssertEquals[value, expression] returns Null if expression \
evaluates to value. Otherwise it throws an AssertEquals-exception to be caught \
by RunTest.";

AssertTrue::usage = "AssertTrue[expression] returns Null if expression \
evaluates to True. Otherwise it throws an AssertTrue-exception to be caught \
by RunTest.";

ListTests::usage = "ListTests[suite] lists the names of all installed tests in suite.\n\
ListTests[] lists the tests in the current suite";

AddTest::usage = "AddTest[suite, name, test] will add a test with a given name\
 to the suite. \n\
AddTest[name, test] adds a test to the current suite.";

AddSuite::usage = "";

DeleteTest::usage = "DeleteTest[suite, name] deletes a given test from the suite.\n\
DeleteTest[name] deletes a given test from the current suite";

RunTest::usage = "RunTest[suite, stringPattern] runs all tests matching stringPattern \
and formats the output.\n\
RunTest[suite] runs all tests in the suite and formats the output.\n\
RunTest[] runs all tests in the current suite.";

BeginSuite::usage = "BeginSuite[suite] sets the current suite until next EndSuite[].";
EndSuite::usage = "EndSuite[] sets the current suite to whatever it was before the\
last BeginSuite[].";

TestEMUnitPackage::usage = "TestEMUnitPackage[] runs all unit tests \
for the eMUnit package.";

eMUnitMessages::usage = "eMUnitMessages::tag - Messages used in the eMUnit package.";


(* ::Section::Closed:: *)
(*Implementations*)


Begin["`Private`"];


SetAttributes[AssertEquals, HoldRest]
AssertEquals[shouldBe_, expr_] := 
 If[Unevaluated[shouldBe] === expr, Null, 
  Throw[HoldComplete[AssertEquals[shouldBe, expr]], "AssertEquals"]]


SetAttributes[AssertTrue, HoldFirst]
AssertTrue[expr_] :=
 If[TrueQ@expr, Null, 
  Throw[HoldComplete[AssertTrue[expr]], "AssertTrue"]]


BeginSuite[suite_] := (If[!ListQ[suiteStack], suiteStack = {}]; 
  AppendTo[suiteStack, suite])
EndSuite[] := If[Length[suiteStack] > 0, suiteStack = Drop[suiteStack, -1]]
currentSuite[] := suiteStack[[-1]]
currentSuiteSetQ[] := If[Length[suiteStack] > 0, True, 
  Message[eMUnitMessages::suitNotSet]; False]
eMUnitMessages::suitNotSet = "No suite set with BeginSuite[].";


ListTests[] /; currentSuiteSetQ[] := ListTests[currentSuite[]]
ListTests[suite_] := suite[UnitTests]


SetAttributes[AddTest, HoldAll];
AddTest[name_, test_] /; currentSuiteSetQ[] := AddTest[currentSuite[], name, test]
AddTest[suite_, name_, test_] := Module[{},
  suite[name] := test;
  updateTestList[suite, name];
]
updateTestList[suite_, name_] := Module[{},
  If[!ListQ@suite[UnitTests], suite[UnitTests] = {}];
  If[shouldBeAdded[suite, name], AppendTo[suite[UnitTests], name]]
]
shouldBeAdded[suite_, name_] := 
 Not@MemberQ[Join[suite[UnitTests], {"Set Up", "Tear Down"}], name]


AddSuite[subsuite_] /; currentSuiteSetQ[] := AddSuite[currentSuite[], subsuite]
AddSuite[mainSuite_, subsuite_] := AddTest[mainSuite, subsuite, runTest[subsuite]]


DeleteTest[name_] /; currentSuiteSetQ[] := DeleteTest[currentSuite[], name]
DeleteTest[suite_, name_] := (suite[name] =.; 
  suite[UnitTests] = suite[UnitTests] /. name -> Sequence[];)


RunTest[] /; currentSuiteSetQ[] := RunTest[currentSuite[]]
RunTest[suite_] := formatTestResult[runTest[suite]]
RunTest[suite_, stringPattern_StringExpression\[NonBreakingSpace]| stringPattern_String] := 
 formatTestResult[runTest[suite, #] & /@ 
  Select[ListTests[suite], StringMatchQ[#, stringPattern]&]] /; 
    If[Or @@ (StringMatchQ[#, stringPattern] & /@ ListTests[suite]), True, 
     Message[eMUnitMessages::nonexistentTest, suite, stringPattern]; False]
eMUnitMessages::nonexistentTest = "No test in suite '`1`' matches '`2`'";

runTest[suite_] := runTest[suite, #] & /@ ListTests[suite]
runTest[suite_, name_] := Module[{result},
  suite["Set Up"];
  result = Catch[suite[name];,"AssertEquals"|"AssertTrue"];
  suite["Tear Down"];
  If[result === Null, testResult["Success"], testResult[suite, name, result]]
 ]
isFailure[result_testResult] := Not[result[[1]] === "Success"]


formatTestResult[results : {__testResult}] :=
 Module[{reportString, failures, nResults, nFailures},
  nResults = Length@results;
  failures = Select[results, isFailure];
  nFailures = Length@failures;
  Column[Join[{drawBar[nFailures], 
               formatSummaryString[nResults, nFailures]},
              formatFailureString /@ failures]]
  ]
formatSummaryString[nResults_Integer, nFailures_Integer] := 
  ToString[nResults] <> " run, " <> ToString[nFailures] <> " failed"
formatFailureString[failure_testResult] := 
 Module[{test, assertString, failureString},
  test = failure[[2]];
  assertString = toString @@ failure[[3]];
  test <> " - Failed " <> assertString <>  ", gave " <> ToString@failure[[3, 1, -1]]
 ]
SetAttributes[toString, HoldAll]
toString[a_] := ToString[Unevaluated[a]]
drawBar[nFailures_Integer] := 
 Graphics[{If[nFailures > 0, Red, Green], 
   Rectangle[{0, 0}, {15, 1}]}, Method -> {"ShrinkWrap" -> True}, 
  ImageSize -> 600]


End[];


(* ::Section:: *)
(*Tests*)


(* ::Subsection::Closed:: *)
(*Head*)


Begin["`PackageTests`"];


TestEMUnitPackage[] := RunTest[frameworkTests]


ClearAll[frameworkTests];
BeginSuite[frameworkTests];


AddTest["Set Up", ClearAll[mytests]; BeginSuite[mytests];];
AddTest["Tear Down", EndSuite[]; ClearAll[mytests]];


(* ::Subsection::Closed:: *)
(*Test EndSuite*)


AddTest[eMUnit`PackageTests`frameworkTests, "testEndSuiteEmptyStack",
 Quiet[
   BeginSuite[mytests];
   Do[EndSuite[];,{5}]
   AssertEquals[{}, $MessageList];
   , {Drop::drop}]
]


(* ::Subsection::Closed:: *)
(*Test AssertEquals*)


AddTest["testAssertEqualsSuccess",
 Module[{result},
  result = Catch[AssertEquals[1, 1], "AssertEquals"] === Null;
  If[Not@result, Throw[{"testAssertEqualsSuccess failed"}, "AssertEquals"]]
 ]]

AddTest["testAssertEqualsThrow", 
 Module[{i, result},
  i := 2;
  result = Catch[AssertEquals[1, i], "AssertEquals"] === HoldComplete[AssertEquals[1, i]];
  If[Not@result, Throw[{"testAssertEqualsThrow failed"}, "AssertEquals"]]
 ]]

AddTest["testAssertEqualsUnevaluated",
 Module[{result, f, i = 0},
  f[a_] /; (i += a; False) := Throw["This shouldn't evaluate"];
  result = Catch[AssertEquals[Unevaluated@f[2], f[3]], "AssertEquals"];
  AssertEquals[Unevaluated@HoldComplete[AssertEquals[f[2], f[3]]], result];
  AssertEquals[3, i];
 ]];


(* ::Subsection::Closed:: *)
(*Test AssertTrue*)


AddTest["testAssertTrueSuccess", 
 Module[{a, result},
  a := True;
  result = Catch[AssertTrue[a], "AssertTrue"] === Null;
  If[Not@result, Throw[{"testAssertTrueSuccess failed"}, "AssertEquals"]]
 ]]

AddTest["testAssertTrueFailure", 
 Module[{a, result},
  a := False;
  result = Catch[AssertTrue[a], "AssertTrue"] === HoldComplete[AssertTrue[a]];
  If[Not@result, Throw[{"testAssertTrueFailure failed"}, "AssertEquals"]]
 ]]

AddTest["testAssertTrueUnevaluating",
 Module[{a, result}, ClearAll[a]; 
  result = MatchQ[Catch[AssertTrue[a], "AssertTrue"], HoldComplete[AssertTrue[_]]]; 
  If[Not@result, Throw[{"testAssertTrueUnevaluating failed"}, "AssertEquals"]]
 ]]


(* ::Subsection:: *)
(*Test AddTest*)


 AddTest["testAddAndListTests",
  AddTest["aTest", 1 + 1];
  AddTest[mytests, "anotherTest", 1 + 2];
  AssertEquals[{"aTest", "anotherTest"}, ListTests[mytests]];
 ];

 AddTest["testAddTestDontEvaluateTheTest",
  Module[{i = 1},
   AddTest["aTest", Do[i++, {10}]];
   AssertEquals[1, i];
  ]];

 AddTest["testAddingTwiceOverwrites",
  AddTest["aTest", 1 + 1];
  AddTest["aTest", 1 + 2];
  AssertEquals[{"aTest"}, ListTests[]];
 ];


(* ::Subsection::Closed:: *)
(*Test AddSuite*)


 AddTest["testAddSuite",
  ClearAll[mySubsuite];
  AddSuite[mytests, mySubsuite];
  AddTest[mySubsuite, "aTest", 1+1];
  AssertEquals[{eMUnit`PackageTests`mySubsuite}, ListTests[mytests]];
  AssertEquals[{"aTest"}, ListTests[ListTests[mytests][[1]]]]
 ];


AddTest["testRunSubSuite", Module[{i = 0},
  ClearAll[mySubsuite];
  BeginSuite[mytests];
  AddTest["aTest", i++]
  AddSuite[mySubsuite];
  AddTest[mySubsuite, "anotherTest", i+=2];
  AddSuite[mySubsuite, mySubsubsuite];
  AddTest[mySubsubsuite, "aThirdTest", i+=3];
  RunTest[];
  EndSuite[];
  AssertEquals[6, i]
 ]];


(* ::Subsection::Closed:: *)
(*Test RunTest*)


 AddTest["testRunTest",
  Module[{i = 1},
   AddTest[mytests, "aTest", Do[i++, {10}]];
   AssertEquals[1, i];
   RunTest[mytests, "aTest"];
   AssertEquals[11, i];
  ]];

 AddTest["testRunNonexistentTest",
  Module[{result},
   AddTest[mytests, "aTest", 1 == 2];
   Quiet[result = RunTest[mytests, "nonexistentTest"];, 
         eMUnitMessages::nonexistentTest];
   AssertEquals[Unevaluated@RunTest[mytests, "nonexistentTest"], result];
  ]];

 AddTest["testRunTestWithPattern",
  Module[{i = 0},
    AddTest[mytests, "aTest", i++]; 
    AddTest[mytests, "anotherTest", i += 2];
    RunTest[mytests, "a" ~~ ___ ~~ "Test"];
    AssertEquals[3, i];
  ]];


 AddTest["testRunTestOnSuite",
  Module[{a, b, c},
   BeginSuite[mytests];
   AddTest["test1", a = 1];
   AddTest["test2", b = 2];
   AddTest["test3", c = a + b];
   RunTest[];
   EndSuite[];
   AssertEquals[1, a];
   AssertEquals[2, b];
   AssertEquals[3, c];
  ]];


(* ::Subsection::Closed:: *)
(*Test Set Up*)


AddTest["testSetUp",
 mytests["isSetUp"] = False;
 AddTest[mytests, "Set Up", mytests["isSetUp"] = True];
 mytests["Set Up"];
 AssertTrue[mytests["isSetUp"]];
];

AddTest["testRunTestRunsSetUp",
 mytests["isSetUp"] = False;
 AddTest[mytests, "Set Up", mytests["isSetUp"] = True];
 AddTest[mytests, "emptyTest", Null];
 RunTest[mytests];
 AssertTrue[mytests["isSetUp"]];
];


(* ::Subsection::Closed:: *)
(*Test Tear Down*)


 AddTest["testTearDown",
  Module[{isStillSetUp},
   mytests["Set Up"];
   isStillSetUp = True;
   AddTest[mytests, "Tear Down", Clear[isStillSetUp]];
   mytests["Tear Down"];
   AssertTrue[!ValueQ[isStillSetUp]];
  ]];

 AddTest["testRunTestRunsTearDown",
  Module[{isStillSetUp},
   AddTest[mytests, "Tear Down", Clear[isStillSetUp]];
   AddTest[mytests, "setsUp", isStillSetUp = True];
   RunTest[mytests, "setsUp"];
   AssertTrue[!ValueQ[isStillSetUp]];
  ]];


(* ::Subsection::Closed:: *)
(*Test DeleteTest*)


 AddTest["testDeleteTest",
  BeginSuite[mytests];
  AddTest["aTest", a = 1];
  AddTest["anotherTest", b = 1];
  DeleteTest["aTest"];
  AssertEquals[{"anotherTest"}, ListTests[]];
  EndSuite[];
 ];


(* ::Subsection::Closed:: *)
(*Test formatTestResult*)


 AddTest["testFormatSingleSuccessfulTestResult", 
  Module[{formattedResult},
   AddTest[mytests, "aTest", AssertEquals[1, 1]];
   formattedResult = RunTest[mytests, "aTest"];
   AssertTrue[MatchQ[formattedResult, Column[{_Graphics, "1 run, 0 failed"}]]]
  ]];

 AddTest["testFormatTwoSuccessfulTestResult", 
  Module[{formattedResult},
   AddTest[mytests, "aTest", AssertEquals[1, 1]];
   AddTest[mytests, "anotherTest", AssertEquals[1, 1]];
   formattedResult = RunTest[mytests];
   AssertTrue[MatchQ[formattedResult, Column[{_Graphics, "2 run, 0 failed"}]]]
  ]];

 AddTest["testFormatSingleFailedTestResult", 
  Module[{formattedResult},
   ClearAll[uniqueA]; uniqueB := False;
   AddTest[mytests, "aTest", AssertTrue[uniqueB || uniqueA]];
   formattedResult = RunTest[mytests, "aTest"];
   AssertTrue[
    MatchQ[formattedResult, 
     Column[{_Graphics, 
       "1 run, 1 failed", 
       "aTest - Failed AssertTrue[eMUnit`PackageTests`uniqueB || \
eMUnit`PackageTests`uniqueA], gave eMUnit`PackageTests`uniqueA"}]]
    ]]];

 AddTest["testFormatOneEachTestResult", 
  Module[{formattedResult},
   AddTest[mytests, "aTest", AssertEquals[1, 1]];
   AddTest[mytests, "anotherTest", AssertEquals[1, -1]];
   formattedResult = RunTest[mytests];
   AssertTrue[
    MatchQ[formattedResult, 
     Column[{_Graphics, 
       "2 run, 1 failed", "anotherTest - Failed AssertEquals[1, -1], gave -1"}]]
    ]]];


(* ::Section::Closed:: *)
(*Tail*)


EndSuite[];


End[];


EndPackage[]
