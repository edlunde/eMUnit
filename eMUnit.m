(* ::Package:: *)

(* ::Section:: *)
(*Declarations*)


BeginPackage["eMUnit`"];


ListTests::usage="ListTests[suite] lists the names of all installed tests.";

AddTest::usage="AddTest[suite, name, test] will add a test with a given name\
 to the suite.";

RunTest::usage="RunTest[suite, name] runs a specified test and formats the output.\n\
RunTest[suite] runs all tests in the suite and formats the output.";

DeleteTest::usage="DeleteTest[suite, name] deletes the test from the suite.";

TestEMUnitPackage::usage="TestEMUnitPackage[] runs all unit tests for the package.";


(* ::Section:: *)
(*Implementations*)


Begin["`Private`"];


On[Assert]


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
  Block[{$AssertFunction = Throw[{##}] &},
   result = Catch[suite[name];];
   ];
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
           toString @@ #[[3, 1]] & /@ failures);];
  Column[{drawBar[nFailures > 0], reportString}]
  ]

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


AddTest[frameworkTests, "testAssertOn", 
 Block[{$AssertFunction = Throw[{##}] &}, 
  result = Catch[Assert[1 == 2]];
  ];
 If[Not[result === {HoldComplete[Assert[1 == 2]]}], 
  Throw[{"Assert failed to trigger"}]];
 ];


AddTest[frameworkTests, "testAddAndListTests",
  AddTest[mytests, "aTest", 1 + 1];
  AddTest[mytests, "anotherTest", 1 + 2];
  Assert[ListTests[mytests] === {"aTest", "anotherTest"}];
 ];

AddTest[frameworkTests, "testAddTestDontEvaluateTheTest",
  Module[{i = 1},
   AddTest[mytests, "aTest", Do[i++, {10}]];
   Assert[i === 1];
  ]];

AddTest[frameworkTests, "testAddingTwiceOverwrites",
  AddTest[mytests, "aTest", 1 + 1];
  AddTest[mytests, "aTest", 1 + 2];
  Assert[ListTests[mytests] === {"aTest"}];
 ];


AddTest[frameworkTests, "testRunTest",
  Module[{i = 1},
   AddTest[mytests, "aTest", Do[i++, {10}]];
   Assert[i === 1];
   RunTest[mytests, "aTest"];
   Assert[i === 11];
   ]];


AddTest[frameworkTests, "testSetUp",
  mytests["isSetUp"] = False;
  AddTest[mytests, "Set Up", mytests["isSetUp"] = True];
  mytests["Set Up"];
  Assert[TrueQ@mytests["isSetUp"]];
  ];


AddTest[frameworkTests, "testTearDown",
  Module[{isStillSetUp},
   mytests["Set Up"];
   isStillSetUp = True;
   AddTest[mytests, "Tear Down", Clear[isStillSetUp]];
   mytests["Tear Down"];
   Assert[Not@ValueQ[isStillSetUp]];
   ]];


AddTest[frameworkTests, "testRunTestOnSuite",
  Module[{a, b, c},
   AddTest[mytests, "test1", a = 1];
   AddTest[mytests, "test2", b = 2];
   AddTest[mytests, "test3", c = a + b];
   RunTest[mytests];
   Assert[a === 1];
   Assert[b === 2];
   Assert[c === 3];
  ]];


AddTest[frameworkTests, "testDeleteTest",
  AddTest[mytests, "aTest", a = 1];
  AddTest[mytests, "anotherTest", b = 1];
  DeleteTest[mytests, "aTest"];
  Assert[ListTests[mytests] === {"anotherTest"}];
 ];


AddTest[frameworkTests, "testFormatSingleSuccessfulTestResult", 
  Module[{formattedResult},
   AddTest[mytests, "aTest", Assert[1 == 1]];
   formattedResult = RunTest[mytests, "aTest"];
   Assert[MatchQ[formattedResult, Column[{_Graphics, "1 run, 0 failed"}]]]
  ]];

AddTest[frameworkTests, "testFormatTwoSuccessfulTestResult", 
  Module[{formattedResult},
   AddTest[mytests, "aTest", Assert[1 == 1]];
   AddTest[mytests, "anotherTest", Assert[1 == 1]];
   formattedResult = RunTest[mytests];
   Assert[
    MatchQ[formattedResult, Column[{_Graphics, "2 run, 0 failed"}]]
   ]]];

AddTest[frameworkTests, "testFormatSingleFailedTestResult", 
  Module[{formattedResult},
   AddTest[mytests, "aTest", Assert[1 == 0]];
   formattedResult = RunTest[mytests, "aTest"];
   Assert[
    MatchQ[formattedResult, 
     Column[{_Graphics, 
       "1 run, 1 failed\naTest - Failed: Assert[1 == 0]"}]]
    ]]];

AddTest[frameworkTests, "testFormatOneEachTestResult", 
  Module[{formattedResult},
   AddTest[mytests, "aTest", Assert[1 == 1]];
   AddTest[mytests, "anotherTest", Assert[1 == -1]];
   formattedResult = RunTest[mytests];
   Assert[
    MatchQ[formattedResult, 
     Column[{_Graphics, 
       "2 run, 1 failed\nanotherTest - Failed: Assert[1 == -1]"}]]
    ]]];


End[];


EndPackage[]
