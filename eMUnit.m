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
RunTest[suite] runs all tests in the suite and formats the output.\n\
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


SetAttributes[AssertEquals, HoldRest]
AssertEquals[shouldBe_, expr_] := 
 If[Unevaluated[shouldBe] === expr, Null, 
  Throw[{HoldComplete[AssertEquals[shouldBe, expr]], expr}, "AssertEquals"]]


SetAttributes[AssertTrue, HoldFirst]
AssertTrue[expr_] :=
 If[TrueQ@expr, Null, 
  Throw[{HoldComplete[AssertTrue[expr]]}, "AssertTrue"]]


BeginSuite[suite_] := (If[!ListQ[suiteStack],suiteStack = {}]; 
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


DeleteTest[name_] /; currentSuiteSetQ[] := DeleteTest[currentSuite[], name]
DeleteTest[suite_, name_] := (suite[name] =.; 
  suite[UnitTests] = suite[UnitTests] /. name -> Sequence[];)


RunTest[] /; currentSuiteSetQ[] := RunTest[currentSuite[]]
RunTest[suite_] := 
 formatTestResult[runTest[suite, #] & /@ ListTests[suite]]
RunTest[suite_, stringPattern_] := 
 formatTestResult[runTest[suite, #] & /@ 
  Select[ListTests[suite], StringMatchQ[#, stringPattern]&]] /; 
    If[Or @@ (StringMatchQ[#, stringPattern] & /@ ListTests[suite]), True, 
     Message[eMUnitMessages::nonexistentTest, suite, stringPattern]; False]
eMUnitMessages::nonexistentTest = "No test in suite '`1`' matches '`2`'";

runTest[suite_, name_] := Module[{result},
  suite["Set Up"];
  result = Catch[suite[name];,"AssertEquals"|"AssertTrue"];
  suite["Tear Down"];
  If[result === Null, "Success", {suite, name, result}]
  ]


formatTestResult[result : {__}] :=
 Module[{reportString, failures, nResults},
  nResults = Length@result;
  failures = Cases[result, Except["Success"]];
  reportString = ToString[nResults] <> " run, " <> ToString[Length@failures] <> " failed";
  reportString = StringJoin[{reportString, formatFailureString /@ failures}];
  Column[{drawBar[Length@failures > 0], reportString}]
  ]
formatFailureString[failure_] := 
 Module[{test, assertString, failureString},
  test = failure[[2]];
  assertString = toString @@ failure[[3, 1]];
  failureString = "\n" <> test <> " - Failed " <> assertString;
  If[Length[failure[[3]]] > 1,
   failureString = failureString <> ", gave " <> ToString@failure[[3, 2]];];
  failureString
  ]
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


TestEMUnitPackage[] := RunTest[frameworkTests]


BeginSuite[frameworkTests];


AddTest["Set Up", ClearAll[mytests]];
AddTest["Tear Down", ClearAll[mytests]];


AddTest[eMUnit`PackageTests`frameworkTests, "testEndSuiteEmptyStack",
 Quiet[
   BeginSuite[mytests];
   EndSuite[];
   EndSuite[];
   AssertEquals[{}, $MessageList];
   , {Drop::drop}]
]


 AddTest["testAssertEqualsSuccess",
  Module[{result},
  result = Catch[AssertEquals[1, 1], "AssertEquals"] === Null;
  If[Not@result, 
   Throw[{"testAssertEqualsSuccess failed"}, "AssertEquals"]]
  ]]

 AddTest["testAssertEqualsThrow", 
  Module[{i, result},
   i := 2;
   result = 
    Catch[AssertEquals[1, i], 
      "AssertEquals"] === {HoldComplete[AssertEquals[1, i]], 2};
   If[Not@result, Throw[{"testAssertEqualsThrow failed"}, "AssertEquals"]]
  ]]

 AddTest["testAssertEqualsUnevaluated",
  Module[{result, f, i},
   f[a_] /; (i += a; False) := Null;
   i = 0;
   result = Catch[AssertEquals[Unevaluated@f[2], f[3]], "AssertEquals"];
   AssertEquals[Unevaluated@{HoldComplete[AssertEquals[f[2], f[3]]], f[3]}, result];
   AssertEquals[3, i];
  ]];


 AddTest["testAssertTrueSuccess", 
 Module[{a, result},
  a := True;
  result = Catch[AssertTrue[a], "AssertTrue"] === Null;
  If[Not@result, 
   Throw[{"testAssertTrueSuccess failed"}, "AssertEquals"]]
  ]]

 AddTest["testAssertTrueFailure", 
 Module[{a, result},
  a := False;
  result = 
   Catch[AssertTrue[a], 
     "AssertTrue"] === {HoldComplete[AssertTrue[a]]};
  If[Not@result, 
   Throw[{"testAssertTrueFailure failed"}, "AssertEquals"]]
  ]]

AddTest["testAssertTrueUnevaluating",
 Module[{a, result}, ClearAll[a]; 
  result = MatchQ[Catch[AssertTrue[a], "AssertTrue"], {HoldComplete[AssertTrue[_]]}]; 
  If[Not@result, 
   Throw[{"testAssertTrueUnevaluating failed"}, "AssertEquals"]]
  ]]


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


 AddTest["testSetUp",
  mytests["isSetUp"] = False;
  AddTest[mytests, "Set Up", mytests["isSetUp"] = True];
  mytests["Set Up"];
  AssertTrue[mytests["isSetUp"]];
 ];


 AddTest["testTearDown",
  Module[{isStillSetUp},
   mytests["Set Up"];
   isStillSetUp = True;
   AddTest[mytests, "Tear Down", Clear[isStillSetUp]];
   mytests["Tear Down"];
   AssertTrue[!ValueQ[isStillSetUp]];
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


 AddTest["testDeleteTest",
  BeginSuite[mytests];
  AddTest["aTest", a = 1];
  AddTest["anotherTest", b = 1];
  DeleteTest["aTest"];
  AssertEquals[{"anotherTest"}, ListTests[]];
  EndSuite[];
 ];


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
   AddTest[mytests, "aTest", AssertTrue[False]];
   formattedResult = RunTest[mytests, "aTest"];
   AssertTrue[
    MatchQ[formattedResult, 
     Column[{_Graphics, 
       "1 run, 1 failed\naTest - Failed AssertTrue[False]"}]]
    ]]];

 AddTest["testFormatOneEachTestResult", 
  Module[{formattedResult},
   AddTest[mytests, "aTest", AssertEquals[1, 1]];
   AddTest[mytests, "anotherTest", AssertEquals[1, -1]];
   formattedResult = RunTest[mytests];
   AssertTrue[
    MatchQ[formattedResult, 
     Column[{_Graphics, 
       "2 run, 1 failed\nanotherTest - Failed AssertEquals[1, -1], gave -1"}]]
    ]]];


EndSuite[];


End[];


EndPackage[]
