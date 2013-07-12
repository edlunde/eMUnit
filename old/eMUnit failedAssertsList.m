(* ::Package:: *)

(* ::Section:: *)
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

DeleteTest::usage = "DeleteTest[suite, name] deletes a given test from the suite.\n\
DeleteTest[name] deletes a given test from the current suite";

RunTest::usage = "RunTest[suite, stringPattern] runs all tests matching stringPattern \
and formats the output.\n\
RunTest[suite] runs all tests in the suite.\n\
RunTest[] runs all tests in the current suite.";

BeginSuite::usage = "BeginSuite[suite] sets the current suite until next EndSuite[].";
EndSuite::usage = "EndSuite[] sets the current suite to whatever it was before the\
last BeginSuite[].";

TestEMUnitPackage::usage = "TestEMUnitPackage[] runs all unit tests \
for the eMUnit package.";

eMUnitMessages::usage = "eMUnitMessages::tag - Messages used in the eMUnit package.";


(* ::Section:: *)
(*Implementations*)


Begin["`Private`"];


resetFailedAsserts[] := failedAsserts = {}
addFailedAssert[HoldComplete[assert_], result_] := 
  AppendTo[failedAsserts, {HoldComplete[assert], result}]
addFailedAssert[HoldComplete[assert_]] := 
  AppendTo[failedAsserts, {HoldComplete[assert]}]
FailedAsserts[] := failedAsserts


SetAttributes[AssertEquals, HoldRest]
AssertEquals[shouldBe_, expr_] := 
 If[Unevaluated[shouldBe] === expr, Null, 
  addFailedAssert[HoldComplete[AssertEquals[shouldBe, expr]], expr]]


SetAttributes[AssertTrue, HoldFirst]
AssertTrue[expr_] :=
 If[TrueQ@expr, Null, 
  addFailedAssert[HoldComplete[AssertTrue[expr]]]]


BeginSuite[suite_] := (If[!ListQ[suiteStack],suiteStack = {}]; 
  AppendTo[suiteStack, suite];)
EndSuite[] := If[Length[suiteStack] > 0, suiteStack = Drop[suiteStack, -1]];
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


DeleteTest[name_] /; currentSuiteSetQ[] := DeleteTest[currentSuite[], name]
DeleteTest[suite_, name_] := (suite[name] =.; 
  suite[UnitTests] = suite[UnitTests] /. name -> Sequence[];)


Options[RunTest] = {Verbose -> False};
RunTest[opts : OptionsPattern[]] /; currentSuiteSetQ[] := RunTest[currentSuite[], opts]
RunTest[suite_, opts : OptionsPattern[]] := 
 formatTestResult[runTest[suite, #] & /@ ListTests[suite], opts]
RunTest[suite_, stringPattern_StringExpression | stringPattern_String, 
 opts : OptionsPattern[]] := 
  formatTestResult[runTest[suite, #] & /@ 
   Select[ListTests[suite], StringMatchQ[#, stringPattern]&], opts] /; 
     If[suiteHasTestQ[suite, stringPattern], True, 
      Message[eMUnitMessages::nonexistentTest, suite, stringPattern]; False]
eMUnitMessages::nonexistentTest = "No test in suite '`1`' matches '`2`'";
suiteHasTestQ[suite_, stringPattern_] := 
  Or @@ (StringMatchQ[#, stringPattern] & /@ ListTests[suite])

runTest[suite_, name_] := (
  suite["Set Up"]; resetFailedAsserts[];
  suite[name];
  suite["Tear Down"];
  testResult[suite, name, FailedAsserts[]]
 )

isFailure[result_testResult] := Not[result[[-1]] === {}]


Options[formatTestResult] = {Verbose -> False};
formatTestResult[results : {__testResult}, opts : OptionsPattern[]] :=
 Module[{summaryString, failures, nResults},
  nResults = Length@results;
  failures = Select[results, isFailure];
  summaryString = formatSummaryString[nResults, Length@failures];
  Column[Join[{drawBar[Length@failures > 0], summaryString},
              If[OptionValue[Verbose], 
                  formatResultString /@ results, 
                  formatFailureString /@ failures]]]
  ]
formatSummaryString[nResults_Integer, nFailures_Integer] := 
  ToString[nResults] <> " run, " <> ToString[nFailures] <> " failed"
formatResultString[result_testResult] := 
 If[isFailure[result], formatFailureString[result], result[[2]] <> " Succeeded"]
formatFailureString[failure_testResult] := 
 Module[{test, failureString},
  test = failure[[2]];
  failureString = test <> " Failed: ";
  If[Length[failure[[3]]] > 1, failureString = failureString <> "\n "];
  failureString <> StringJoin[Riffle[formatFailedAssert /@ failure[[3]],"\n "]]  
 ]
formatFailedAssert[assert : {HoldComplete[_],___}] := 
  toString @@ assert[[1]] <> If[Length@assert == 1, "", " gave " <> ToString@assert[[2]]]
SetAttributes[toString, HoldAll]
toString[a_] := ToString[Unevaluated[a]]
drawBar[nFailuresNonZero_] := 
 Graphics[{If[nFailuresNonZero, Red, Green], 
   Rectangle[{0, 0}, {15, 1}]}, Method -> {"ShrinkWrap" -> True}, 
  ImageSize -> 600]


End[];


(* ::Section:: *)
(*Tests*)


Begin["`PackageTests`"];


TestEMUnitPackage[opts : OptionsPattern[]] := RunTest[frameworkTests, opts]


ClearAll[frameworkTests];
BeginSuite[frameworkTests];


AddTest["Set Up", ClearAll[mytests]];
AddTest["Tear Down", ClearAll[mytests]];


(* ::Subsection:: *)
(*Test EndSuite*)


AddTest[eMUnit`PackageTests`frameworkTests, "testEndSuiteEmptyStack",
 Quiet[
   BeginSuite[mytests];
   EndSuite[];
   EndSuite[];
   AssertEquals[{}, $MessageList];
   , {Drop::drop}]
]


(* ::Subsection:: *)
(*Test AssertEquals*)


 AddTest["testAssertEqualsSuccess",
  Module[{result},
  result = AssertEquals[1, 1] === Null;
  If[Not@result, 
   addFailedAssert[HoldComplete["testAssertEqualsSuccess failed"]]]
  ]]

 AddTest["testAssertEqualsFailure", 
  Module[{i, result},
   i := 2;
   result = AssertEquals[1, i] === {{HoldComplete[AssertEquals[1, i]], 2}};
   eMUnit`Private`resetFailedAsserts[];
   If[Not@result, addFailedAssert[HoldComplete["testAssertEqualsFailure failed"]]]
  ]]

 AddTest["testAssertEqualsUnevaluated",
  Module[{result, f, i},
   f[a_] /; (i += a; False) := Null;
   i = 0;
   result = AssertEquals[Unevaluated@f[2], f[3]];
   AssertEquals[Unevaluated@{HoldComplete[AssertEquals[f[2], f[3]]], f[3]}, result];
   AssertEquals[3, i];
  ]];


(* ::Subsection:: *)
(*Test AssertTrue*)


 AddTest["testAssertTrueSuccess", 
 Module[{a, result},
  a := True;
  result = AssertTrue[a] === Null;
  If[Not@result, 
   addFailedAssert[HoldComplete["testAssertTrueSuccess failed"]]]
  ]]

 AddTest["testAssertTrueFailure", 
 Module[{a, result},
  a := False;
  result = AssertTrue[a] === {{HoldComplete[AssertTrue[a]]}};
  If[Not@result, 
   addFailedAssert[HoldComplete["testAssertTrueFailure failed"]]]
  ]]

AddTest["testAssertTrueUnevaluating",
 Module[{a, result}, ClearAll[a]; 
  result = MatchQ[AssertTrue[a], {{HoldComplete[AssertTrue[_]]}}]; 
  If[Not@result, 
   addFailedAssert[HoldComplete["testAssertTrueUnevaluating failed"]]]
  ]]


(* ::Subsection:: *)
(*Test AddTest*)


 AddTest["testAddAndListTests",
  BeginSuite[mytests];
  AddTest["aTest", 1 + 1];
  EndSuite[];
  AddTest[mytests, "anotherTest", 1 + 2];
  AssertEquals[{"aTest", "anotherTest"}, ListTests[mytests]];
 ];

 AddTest["testAddTestDontEvaluateTheTest",
  Module[{i = 1},
   AddTest[mytests, "aTest", Do[i++, {10}]];
   AssertEquals[1, i];
  ]];

 AddTest["testAddingTwiceOverwrites",
  BeginSuite[mytests];
  AddTest["aTest", 1 + 1];
  AddTest["aTest", 1 + 2];
  AssertEquals[{"aTest"}, ListTests[]];
  EndSuite[];
 ];


(* ::Subsection:: *)
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


(* ::Subsection:: *)
(*Test Set Up*)


 AddTest["testSetUp",
  mytests["isSetUp"] = False;
  AddTest[mytests, "Set Up", mytests["isSetUp"] = True];
  mytests["Set Up"];
  AssertTrue[mytests["isSetUp"]];
 ];


(* ::Subsection:: *)
(*Test Tear Down*)


 AddTest["testTearDown",
  Module[{isStillSetUp},
   mytests["Set Up"];
   isStillSetUp = True;
   AddTest[mytests, "Tear Down", Clear[isStillSetUp]];
   mytests["Tear Down"];
   AssertTrue[!ValueQ[isStillSetUp]];
  ]];


(* ::Subsection:: *)
(*Test DeleteTest*)


 AddTest["testDeleteTest",
  BeginSuite[mytests];
  AddTest["aTest", a = 1];
  AddTest["anotherTest", b = 1];
  DeleteTest["aTest"];
  AssertEquals[{"anotherTest"}, ListTests[]];
  EndSuite[];
 ];


(* ::Subsection:: *)
(*Test formatTestResult*)


 AddTest["testFormatSingleSuccessfulTestResult", 
  Module[{formattedResult},
   AddTest[mytests, "aTest", AssertEquals[1, 1]];
   formattedResult = RunTest[mytests, "aTest"];
   AssertTrue[MatchQ[formattedResult, Column[{_Graphics, "1 run, 0 failed"}]]]
  ]];

 AddTest["testFormatSingleFailedTestResult", 
  Module[{formattedResult},
   uniqueF := False;
   AddTest[mytests, "aTest", AssertTrue[uniqueF]];
   formattedResult = RunTest[mytests, "aTest"];
   AssertTrue[
    MatchQ[formattedResult, 
     Column[{_Graphics, 
       "1 run, 1 failed", "aTest Failed: AssertTrue[eMUnit`PackageTests`uniqueF]"}]]
    ]]];

 AddTest["testFormatOneEachTestResult", 
  Module[{formattedResult},
   AddTest[mytests, "aTest", AssertEquals[1, 1]];
   AddTest[mytests, "anotherTest", AssertEquals[1, -1]];
   formattedResult = RunTest[mytests];
   AssertTrue[
    MatchQ[formattedResult, 
     Column[{_Graphics, 
       "2 run, 1 failed", "anotherTest Failed: AssertEquals[1, -1] gave -1"}]]
    ]]];

 AddTest["testFormatTestResultVerbose", 
  Module[{formattedResult},
   AddTest[mytests, "aTest", AssertEquals[1, 1]];
   AddTest[mytests, "anotherTest", AssertEquals[-1, 1]];
   formattedResult = RunTest[mytests, Verbose -> True];
   AssertTrue[MatchQ[formattedResult, Column[{_Graphics, "2 run, 1 failed",
    "aTest Succeeded", "anotherTest Failed: AssertEquals[-1, 1] gave 1"}]]]
  ]];


(* ::Subsection:: *)
(*Tail*)


EndSuite[];


End[];


EndPackage[]
