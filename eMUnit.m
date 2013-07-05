(* ::Package:: *)

(* ::Section:: *)
(*Declarations*)


BeginPackage["eMUnit`"];


AssertEquals::usage="AssertEquals[value, expression] returns Null if expression \
evaluates to value, throws an AssertEquals-exception to be caught by RunTest \
otherwise."

AssertTrue::usage="AssertTrue[expression] returns Null if expression \
evaluates to True, throws an AssertTrue-exception to be caught by RunTest \
otherwise."

ListTests::usage="ListTests[suite] lists the names of all installed tests.";

AddTest::usage="AddTest[suite, name, test] will add a test with a given name\
 to the suite.";

RunTest::usage="RunTest[suite, name] runs a specified test and formats the output.\n\
RunTest[suite] runs all tests in the suite and formats the output.";

DeleteTest::usage="DeleteTest[suite, name] deletes the test from the suite.";

TestEMUnitPackage::usage="TestEMUnitPackage[] runs all unit tests \
for the eMUnit package.";


(* ::Section:: *)
(*Implementations*)


Begin["`Private`"];


SetAttributes[AssertEquals, HoldRest]
AssertEquals[shouldBe_, expr_] := 
 If[shouldBe === expr, Null, 
  Throw[{HoldComplete[AssertEquals[shouldBe, expr]], expr}, "AssertEquals"]]


SetAttributes[AssertTrue, HoldFirst]
AssertTrue[expr_] :=
 If[expr, Null, 
  Throw[{HoldComplete[AssertTrue[expr]], expr}, "AssertTrue"]]


ListTests[suite_]:=suite[UnitTests]


SetAttributes[AddTest, HoldAll];
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


RunTest[suite_, name_] := formatTestResult[{runTest[suite, name]}]
RunTest[suite_] := 
 formatTestResult[runTest[suite, #] & /@ suite[UnitTests]]

runTest[suite_, name_] := Module[{result},
  suite["SetUp"];
  result = Catch[suite[name];,"AssertEquals"|"AssertTrue"];
  suite["Tear Down"];
  If[result === Null, "Success", {suite, name, result}]
  ]


formatTestResult[result : {__}] :=
 Module[{reportString, failures, nResults, nFailures},
  nResults = Length@result;
  failures = Cases[result, Except["Success"]];
  nFailures = Length@failures;
  reportString = 
   ToString[nResults] <> " run, " <> ToString[nFailures] <> 
    " failed";
  If[nFailures > 0, 
   reportString = 
     reportString <> 
      StringJoin @@ ("\n" <> ToString[#[[2]]] <> " - Failed: " <> 
           ToString@{toString @@@ #[[3]]} & /@ failures);];
  Column[{drawBar[nFailures > 0], reportString}]
  ];

toString[a_] := ToString[Unevaluated[a]]
SetAttributes[toString, HoldAll]

drawBar[nFailuresNonZero_] := 
 Graphics[{If[nFailuresNonZero, Red, Green], 
   Rectangle[{0, 0}, {15, 1}]}, Method -> {"ShrinkWrap" -> True}, 
  ImageSize -> 600]


DeleteTest[suite_, name_] := (suite[name] =.; 
  suite[UnitTests] = suite[UnitTests] /. name -> Sequence[];)


End[];


(* ::Section:: *)
(*Tests*)


Begin["`PackageTests`"];


TestEMUnitPackage[] := RunTest[frameworkTests]


AddTest[frameworkTests, "Set Up", ClearAll[mytests]];
AddTest[frameworkTests, "Tear Down", ClearAll[mytests]];


AddTest[frameworkTests, "testAssertEqualsSuccess",
  Module[{result},
  result = Catch[AssertEquals[1, 1], "AssertEquals"] === Null;
  If[Not@result, 
   Throw[{"testAssertEqualsSuccess failed"}, "AssertEquals"]]
  ]]

AddTest[frameworkTests, "testAssertEqualsThrow", 
 Module[{i, result},
  i := 2;
  result = 
   Catch[AssertEquals[1, i], 
     "AssertEquals"] === {HoldComplete[AssertEquals[1, i]], 2};
  If[Not@result, Throw[{"testAssertEqualsThrow failed"}, "AssertEquals"]]
  ]]


AddTest[frameworkTests, "testAssertTrueSuccess", 
 Module[{a, result},
  a := True;
  result = Catch[AssertTrue[a], "AssertTrue"] === Null;
  If[Not@result, 
   Throw[{"testAssertTrueSuccess failed"}, "AssertEquals"]]
  ]]

AddTest[frameworkTests, "testAssertTrueFailure", 
 Module[{a, result},
  a := False;
  result = 
   Catch[AssertTrue[a], 
     "AssertTrue"] === {HoldComplete[AssertTrue[a]], False};
  If[Not@result, 
   Throw[{"testAssertTrueFailure failed"}, "AssertEquals"]]
  ]]


AddTest[frameworkTests, "testAddAndListTests",
  AddTest[mytests, "aTest", 1 + 1];
  AddTest[mytests, "anotherTest", 1 + 2];
  AssertEquals[{"aTest", "anotherTest"}, ListTests[mytests]];
 ];

AddTest[frameworkTests, "testAddTestDontEvaluateTheTest",
  Module[{i = 1},
   AddTest[mytests, "aTest", Do[i++, {10}]];
   AssertEquals[1, i];
  ]];

AddTest[frameworkTests, "testAddingTwiceOverwrites",
  AddTest[mytests, "aTest", 1 + 1];
  AddTest[mytests, "aTest", 1 + 2];
  AssertEquals[{"aTest"}, ListTests[mytests]];
 ];


AddTest[frameworkTests, "testRunTest",
  Module[{i = 1},
   AddTest[mytests, "aTest", Do[i++, {10}]];
   AssertEquals[1, i];
   RunTest[mytests, "aTest"];
   AssertEquals[11, i];
  ]];


AddTest[frameworkTests, "testSetUp",
  mytests["isSetUp"] = False;
  AddTest[mytests, "Set Up", mytests["isSetUp"] = True];
  mytests["Set Up"];
  AssertTrue[mytests["isSetUp"]];
 ];


AddTest[frameworkTests, "testTearDown",
  Module[{isStillSetUp},
   mytests["Set Up"];
   isStillSetUp = True;
   AddTest[mytests, "Tear Down", Clear[isStillSetUp]];
   mytests["Tear Down"];
   AssertTrue[!ValueQ[isStillSetUp]];
  ]];


AddTest[frameworkTests, "testRunTestOnSuite",
  Module[{a, b, c},
   AddTest[mytests, "test1", a = 1];
   AddTest[mytests, "test2", b = 2];
   AddTest[mytests, "test3", c = a + b];
   RunTest[mytests];
   AssertEquals[1, a];
   AssertEquals[2, b];
   AssertEquals[3, c];
  ]];


AddTest[frameworkTests, "testDeleteTest",
  AddTest[mytests, "aTest", a = 1];
  AddTest[mytests, "anotherTest", b = 1];
  DeleteTest[mytests, "aTest"];
  AssertEquals[{"anotherTest"}, ListTests[mytests]];
 ];


AddTest[frameworkTests, "testFormatSingleSuccessfulTestResult", 
  Module[{formattedResult},
   AddTest[mytests, "aTest", AssertEquals[1, 1]];
   formattedResult = RunTest[mytests, "aTest"];
   AssertTrue[MatchQ[formattedResult, Column[{_Graphics, "1 run, 0 failed"}]]]
  ]];

AddTest[frameworkTests, "testFormatTwoSuccessfulTestResult", 
  Module[{formattedResult},
   AddTest[mytests, "aTest", AssertEquals[1, 1]];
   AddTest[mytests, "anotherTest", AssertEquals[1, 1]];
   formattedResult = RunTest[mytests];
   AssertTrue[MatchQ[formattedResult, Column[{_Graphics, "2 run, 0 failed"}]]]
  ]];

AddTest[frameworkTests, "testFormatSingleFailedTestResult", 
  Module[{formattedResult},
   AddTest[mytests, "aTest", AssertEquals[1, 0]];
   formattedResult = RunTest[mytests, "aTest"];
   AssertTrue[
    MatchQ[formattedResult, 
     Column[{_Graphics, 
       "1 run, 1 failed\naTest - Failed: {{AssertEquals[1, 0], 0}}"}]]
    ]]];

AddTest[frameworkTests, "testFormatOneEachTestResult", 
  Module[{formattedResult},
   AddTest[mytests, "aTest", AssertEquals[1, 1]];
   AddTest[mytests, "anotherTest", AssertEquals[1, -1]];
   formattedResult = RunTest[mytests];
   AssertTrue[
    MatchQ[formattedResult, 
     Column[{_Graphics, 
       "2 run, 1 failed\nanotherTest - Failed: {{AssertEquals[1, -1], -1}}"}]]
    ]]];


End[];


EndPackage[]
