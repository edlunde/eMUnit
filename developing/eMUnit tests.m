(* ::Package:: *)

(* ::Section:: *)
(*Tests*)


(* ::Subsubsection::Closed:: *)
(*Helper functions*)


(* Used to test AssertEquals and AssertMatch, can't use them to test themselves *)
throwSomething[text_] := 
  eMUnit`Private`throwAssertException["AssertEquals or AssertMatch", text, ""]


(* ::Subsection::Closed:: *)
(*Head*)


TestEMUnitPackage[] := RunTest[frameworkTests]


ClearAll[frameworkTests];
BeginSuite[frameworkTests];


AddTest["Set Up", 
 ClearAll[mytests, anotherSuite]; 
 BeginSuite[mytests];];
 
AddTest["Tear Down", 
 EndSuite[]; 
 ClearAll[mytests, anotherSuite]];


(* ::Subsection:: *)
(*Test Asserts*)


(* ::Subsubsection::Closed:: *)
(*Test AssertEquals*)


AddTest["testAssertEqualsSuccess",
 Module[{result},
  result = Catch[AssertEquals[1, 1], "AssertEquals"] === Null;
  If[Not@result, throwSomething["testAssertEqualsSuccess failed"]]
 ]];

AddTest["testAssertEqualsThrow", 
 Module[{i, result},
  i := 2;
  result = Catch[AssertEquals[1, i], "AssertEquals"];
  If[result === eMUnit`Private`assertException[HoldComplete[AssertEquals[1, i]], 2], 
     Null,
     throwSomething["testAssertEqualsThrow failed"]]
 ]];

AddTest["testAssertEqualsUnevaluated",
 Module[{result, f, i = 0},
  f[a_] /; (i += a; False) := Throw["This shouldn't evaluate"];
  result = Catch[AssertEquals[Unevaluated@f[2], f[3]], "AssertEquals"];
  AssertEquals[Unevaluated@eMUnit`Private`assertException[
      HoldComplete[AssertEquals[f[2], f[3]]], 
      f[3]]
   , result];
  AssertEquals[3, i];
 ]];


(* ::Subsubsection::Closed:: *)
(*Test AssertMatch*)


AddTest["testAssertMatchSuccess",
  If[Not[Catch[AssertMatch[_?NumericQ, 1], "AssertMatch"] === Null], 
     throwSomething["testAssertMatch failed"]]
 ];

AddTest["testAssertMatchFailed", Module[{result},
  result = Catch[AssertMatch[_Symbol, 1], "AssertMatch"];
  If[Not[result === 
       eMUnit`Private`assertException[HoldComplete[AssertMatch[_Symbol, 1]], 1]], 
     throwSomething["testAssertMatch failed"]]
 ]];

AddTest["testAssertMatchFailedEvaluatesOnce", Module[{i = 0, result},
  result = Catch[AssertMatch[0, ++i], "AssertMatch"];
  AssertEquals[1, i];
  If[Not[result === 
         eMUnit`Private`assertException[HoldComplete[AssertMatch[0, ++i]], 1]],
     throwSomething["testAssertMatchFailedEvaluatesOnce failed"]];
  AssertEquals[1, i];
]];


(* ::Subsubsection::Closed:: *)
(*Test AssertTrue*)


AddTest["testAssertTrueSuccess", 
 Module[{a, result},
  a := True;
  result = Catch[AssertTrue[a], "AssertTrue"] === Null;
  If[Not@result, throwSomething["testAssertTrueSuccess failed"]]
 ]];

AddTest["testAssertTrueFailure", 
 Module[{a, result},
  a := False;
  result = Catch[AssertTrue[a], "AssertTrue"];
  If[result === eMUnit`Private`assertException[HoldComplete[AssertTrue[a]], False], 
     Null, 
     throwSomething["testAssertTrueFailure failed"]]
 ]];

AddTest["testAssertTrueUnevaluating",
 Module[{a, result}, ClearAll[a]; 
  result = Catch[AssertTrue[a], "AssertTrue"]; 
  AssertMatch[_[HoldComplete[AssertTrue[_]], _], result];
 ]];


(* ::Subsubsection::Closed:: *)
(*Test AssertEqualsN*)


AddTest["testAssertEqualsNSuccessExact",
 AssertEquals[Null, Catch[AssertEqualsN[1, 1], "AssertEqualsN"]]
];

AddTest["testAssertEqualsNSuccessDefaultTolerance",
 AssertEquals[Null, 
  Catch[AssertEqualsN[1, 1 + 0.1*Tolerance /. Options[AssertEqualsN]], "AssertEqualsN"]]
];

AddTest["testAssertEqualsNSuccessOnExactTolerance",
 AssertEquals[Null, Catch[AssertEqualsN[1, 2, Tolerance -> 1], "AssertEqualsN"]]
];

AddTest["testAssertEqualsNSuccessLargeTolerance",
 AssertEquals[Null, Catch[AssertEqualsN[1, 5.1, Tolerance -> 4.5], "AssertEqualsN"]]
];

AddTest["testAssertEqualsNNonNumericTolerance",
 AssertMessage[eMUnitMessages::nonNumericTolerance, 
  Catch[AssertEqualsN[1, 5.1, Tolerance -> "string"], "AssertEqualsN"]]
];

AddTest["testAssertEqualsNThrow", 
 With[{x = 1.7},
  AssertEquals[eMUnit`Private`assertException[
                   HoldComplete[AssertEqualsN[1, x, Tolerance -> 0.5]], x], 
               Catch[AssertEqualsN[1, x, Tolerance -> 0.5], "AssertEqualsN"]]
 ]];


(* ::Subsubsection:: *)
(*Test AssertMatchN*)


AddTest["testAssertMatchNSuccessExact",
 AssertEquals[Null, Catch[AssertMatchN[{1, 2, 3}, 1], "AssertMatchN"]];
 AssertEquals[Null, Catch[AssertMatchN[{{1, 2}, {3, 4}, 3}, {1, 2}], "AssertMatchN"]];
];

AddTest["testAssertMatchNSuccessDefaultTolerance",
AssertEquals[Null, Catch[
   AssertMatchN[{1, 2, 3}, 1 + 0.1*Tolerance /. Options[AssertMatchN]], "AssertMatchN"]]
];

AddTest["testAssertMatchNSuccessOnExactTolerance",
 AssertEquals[Null, Catch[AssertMatchN[{1, 3}, 2, Tolerance -> 1], "AssertMatchN"]];
 AssertEquals[Null, Catch[AssertMatchN[{{1}, {3}}, {2}, Tolerance -> 1], "AssertMatchN"]];
];

AddTest["testAssertMatchNSuccessLargeTolerance",
 AssertEquals[Null, 
  Catch[AssertMatchN[{1, 0}, 5.1, Tolerance -> 4.5], "AssertMatchN"]];
 AssertEquals[Null, 
   Catch[AssertMatchN[{{1}, {0}}, {5.1}, Tolerance -> 4.5], "AssertMatchN"]];
];

AddTest["testAssertMemberNSuccessMixedArguments",
 AssertEquals[Null, 
  Catch[AssertMatchN[{1, "a"}, 5.1, Tolerance -> 4.5], "AssertMatchN"]];
 AssertEquals[Null, 
   Catch[AssertMatchN[{{0, "a"}, {1}}, {5.1}, Tolerance -> 4.5], "AssertMatchN"]];
 AssertEquals[Null, 
  Catch[AssertMatchN[{{1, "a"}, 2, 3}, {1, "a"}], "AssertMatchN"]];
 AssertEquals[Null, 
  Catch[AssertMatchN[{{2}, {1, "a"}, 3}, {1, "a"}], "AssertMatchN"]];
];

AddTest["testAssertMatchNNonNumericTolerance",
 AssertMessage[eMUnitMessages::nonNumericTolerance, 
  Catch[AssertMatchN[{2, 3, 4.}, 5.1, Tolerance -> "string"], "AssertMatchN"]]
];

AddTest["testAssertMatchNThrow", 
  AssertEquals[eMUnit`Private`assertException[HoldComplete[
                 AssertMatchN[{1, 3}, 2, Tolerance -> 0.3]], 2], 
   Catch[AssertMatchN[{1, 3}, 2, Tolerance -> 0.3], "AssertMatchN"]];
  AssertEquals[eMUnit`Private`assertException[HoldComplete[
                 AssertMatchN[{2, {1, "b"}, 3}, {1, "a"}, Tolerance -> 0.3]], {1, "a"}], 
   Catch[AssertMatchN[{2, {1, "b"}, 3}, {1, "a"}, Tolerance -> 0.3], "AssertMatchN"]];
 ];


(* ::Subsubsection::Closed:: *)
(*Test AssertMessage*)


AddTest["testAssertMessageRuns", Module[{mess, i = 0},
  mess::aMessage = "Message!";
  Catch[AssertMessage[mess::aMessage, i++], "AssertMessage"];
  AssertEquals[1, i];
]];


AddTest["testAssertNoMessage", Module[{mess, messenger, result},
  result = Catch[AssertNoMessage[1+1]; "noThrow", "AssertMessage"];
  AssertEquals["noThrow", result];
  mess::aMessage = "Message!";
  messenger := Message[mess::aMessage];
  Quiet[
    result = Catch[AssertNoMessage[messenger]; "noThrow", "AssertMessage"];
  , mess::aMessage];
  AssertEquals[eMUnit`Private`assertException[
      HoldComplete[AssertNoMessage[messenger]], 
      {HoldForm[mess::aMessage]}]
   , result];
]];

AddTest["testAssertMessageCorrectMessage", Module[{mess, messenger, result},
  mess::aMessage = "Message!";
  messenger := Message[mess::aMessage];
  result = Catch[AssertMessage[mess::aMessage, messenger]; "noThrow", "AssertMessage"];
  AssertEquals["noThrow", result];
]];

AddTest["testAssertMessageThrows", Module[{mess, result},
  mess::aMessage = "Message!";
  result = Catch[AssertMessage[mess::aMessage, "noMessage"], "AssertMessage"];
  AssertEquals[eMUnit`Private`assertException[
      HoldComplete[AssertMessage[mess::aMessage, "noMessage"]],
      {}]
    , result];
]];


AddTest["testAssertMessageQuiet", 
  EndSuite[];
  Block[{$MessageList = {}}, 
   Quiet[
    Catch[AssertMessage[eMUnitMessages::suiteNotSet, ListTests[]], "AssertMessage"];
    AssertEquals[{}, $MessageList];
   , eMUnitMessages::suiteNotSet];
]];
AddTest["testAssertMessageNotQuietOther",
  EndSuite[];
  Block[{$MessageList = {}}, 
   Quiet[
    Catch[AssertMessage[eMUnitMessages::nonexistentTest, ListTests[]]
        , "AssertMessage"];
    AssertEquals[{HoldForm[eMUnitMessages::suiteNotSet]}, $MessageList];
   , eMUnitMessages::suiteNotSet];
]];


AddTest["testAssertMessageIndepOfOtherMessages", Module[{mess, messenger, result},
  mess::aMessage = "Message!";
  messenger := Message[mess::aMessage];
  Quiet[
   messenger;
   result = Catch[AssertMessage[mess::aMessage, messenger], "AssertMessage"];
  , mess::aMessage];
  AssertEquals[Null, result];
]];


(* ::Subsection:: *)
(*Test Begin, List, Add, Delete*)


(* ::Subsubsection::Closed:: *)
(*Test AddTest*)


 AddTest["testAddAndListTests",
  AddTest["aTest", 1 + 1];
  AddTest[mytests, "anotherTest", 1 + 2];
  AssertEquals[{"aTest", "anotherTest"}, ListTests[]];
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


(* ::Subsubsection::Closed:: *)
(*Test Suite workings*)


AddTest["testAddSuite",
 ClearAll[mySubsuite];
 AddSuite[mySubsuite];
 AddTest[mySubsuite, "aTest", 1+1];
 AssertEquals[{eMUnit`PackageTests`mySubsuite}, ListTests[]];
 AssertEquals[{"aTest"}, ListTests[ListTests[mytests][[1]]]]
];


AddTest["testRunSubSuite", Module[{i = 0},
  ClearAll[mySubsuite];
  AddTest["aTest", i++]
  AddSuite[mySubsuite];
  AddTest[mySubsuite, "anotherTest", i+=2];
  AddSuite[mySubsuite, mySubsubsuite];
  AddTest[mySubsubsuite, "aThirdTest", i+=3];
  RunTest[];
  AssertEquals[6, i]
]];


AddTest["testSuiteNotSetMessage",
 EndSuite[];
 AssertMessage[eMUnitMessages::suiteNotSet, AddTest["testWithoutSuite", 1+1]]
];


AddTest["testEndSuiteEmptyStack",
  BeginSuite[mytests];
  Do[EndSuite[];,{5}];
  AssertNoMessage[EndSuite[]];
];


(* 
Tests for a bug where having 
ListTests[] /; currentSuiteSetQ[] := ...
rather than
ListTests[] := ... /; currentSuiteSetQ[]
caused the check not to be run if inside tests.

******************************************************************
Basic mechanism:
ClearAll[extractIfListExists,list];
extractIfListExists[] /; checkList := list[[1]];
checkList:=If[Length[list]>0,True,Print["error: no list"];False];
ClearAll[extractIfListExists,list];
extractIfListExists[]/;checkList:=list[[1]];
checkList:=If[Length[list]>0,True,Print["error: no list"];False];

extractIfListExists[]
list={1,2,3};
extractIfListExists[]

ClearAll[list];

runExtract[] := extractIfListExists[];
runExtract[]
runExtract[]
list={1,2,3};
runExtract[]
******************************************************************
*)

(* ListTests, AddTest, AddSuite, BeginSubsuite, DeleteTest, RunTests (both)  *)
addRecheckCurrentSuiteTest = Function[{name, expr, result},
 AddTest["testCurrentSuiteRecheck" <> name, Module[{a = "notTouched (just checkin)"},
   EndSuite[];
   AddTest[mytests, "atest", a = expr];
   AssertMessage[eMUnitMessages::suiteNotSet, RunTest[mytests]];
   AssertEquals[Null, Unevaluated@a];(*holdform*)
   BeginSuite[anotherSuite];
   AssertNoMessage[RunTest[mytests]];
   EndSuite[];
   AssertEquals[result, a];
   AssertMessage[eMUnitMessages::suiteNotSet, RunTest[mytests]];
 ]], HoldAll];
addRecheckCurrentSuiteTest @@@ Unevaluated[{
  {"ListTests", ListTests[], anotherSuite[eMUnit`Private`UnitTests]},
  {"AddTest", AddTest["anotherTest", 1+1], "anotherTest"},
  {"AddSuite", AddSuite[someSubsuite], someSubsuite},
  {"BeginSubsuite", 
   Module[{temp}, temp = BeginSubsuite[someSubsuite];
     If[!temp===Null, EndSuite[]; DeleteTest[someSubsuite]];
     temp], 
   {anotherSuite, someSubsuite}},
  {"DeleteTest", 
   (AddTest[anotherSuite, "anotherTest", 1+1]; DeleteTest["anotherTest"]), 
    "anotherTest"},
  {"RunTest", 
   (AddTest[anotherSuite, "anotherTest", 1+1]; RunTest[]),
    Column[{eMUnit`Private`drawBar[0], "1 run in 0. s, 0 failed"}]},
  {"RunTestPattern", 
   (AddTest[anotherSuite, "anotherTest", 1+1]; RunTest["anotherTest"]),
    Column[{eMUnit`Private`drawBar[0], "1 run in 0. s, 0 failed"}]}
}];


(* ::Subsubsection::Closed:: *)
(*Test BeginSubsuite*)


AddTest["testBeginSubsuite",
 ClearAll[mySubsuite];
 BeginSubsuite[mySubsuite];
 AddTest["aTest", 1+1];
 EndSuite[];
 AssertEquals[{eMUnit`PackageTests`mySubsuite}, ListTests[]];
 AssertEquals[{"aTest"}, ListTests[ListTests[mytests][[1]]]]
];


(* ::Subsubsection::Closed:: *)
(*Test DeleteTest*)


 AddTest["testDeleteTest",
  AddTest["aTest", a = 1];
  AddTest["anotherTest", b = 1];
  DeleteTest["aTest"];
  AssertEquals[{"anotherTest"}, ListTests[]];
 ];


(* ::Subsection:: *)
(*Test Run*)


(* ::Subsubsection::Closed:: *)
(*Test RunTest*)


AddTest["testRunTest",
 Module[{i = 1},
  AddTest["aTest", Do[i++, {10}]];
  AssertEquals[1, i];
  RunTest["aTest"];
  AssertEquals[11, i];
 ]];

AddTest["testRunTestWithPattern",
 Module[{i = 0},
   AddTest["aTest", i++]; 
   AddTest["anotherTest", i += 2];
   RunTest["an" ~~ __ ~~ "Test"];
   AssertEquals[2, i];
   RunTest["a" ~~ ___ ~~ "Test"];
   AssertEquals[5, i];
 ]];

AddTest["testRunTestWithNonmatchingPattern", Module[{result},
 AssertMessage[eMUnitMessages::nonexistentTest, 
  result = RunTest[__ ~~ "nonmatchingPattern"]
 ];
 AssertEquals[
  Unevaluated@RunTest[eMUnit`PackageTests`mytests,__~~"nonmatchingPattern"], 
  result];
]];


 AddTest["testRunTestOnSuite",
  Module[{a, b, c},
   AddTest["test1", a = 1];
   AddTest["test2", b = 2];
   AddTest["test3", c = a + b];
   RunTest[];
   AssertEquals[1, a];
   AssertEquals[2, b];
   AssertEquals[3, c];
  ]];


AddTest["testRunTestOnParentWithStringPattern", Module[{i = 0},
  ClearAll[mySubsuite];
  AddSuite[mySubsuite];
  AddTest[mySubsuite, "aTestNotExistingInParent", i++];
  AssertMessage[eMUnitMessages::nonexistentTest, 
    RunTest[mytests, __~~"NotExistingInParent"]];
  AssertEquals[0, i];
  RunTest[mySubsuite, __~~"NotExistingInParent"]
  AssertEquals[1, i];
]];


(* ::Subsubsection::Closed:: *)
(*Test Set Up*)


AddTest["testSetUp",
 mytests["isSetUp"] = False;
 AddTest["Set Up", mytests["isSetUp"] = True];
 mytests["Set Up"];
 AssertTrue[mytests["isSetUp"]];
];

AddTest["testRunTestRunsSetUp",
 mytests["isSetUp"] = False;
 AddTest["Set Up", mytests["isSetUp"] = True];
 AddTest["emptyTest", Null];
 RunTest[];
 AssertTrue[mytests["isSetUp"]];
];


(* ::Subsubsection::Closed:: *)
(*Test Tear Down*)


 AddTest["testTearDown",
  Module[{isStillSetUp},
   mytests["Set Up"];
   isStillSetUp = True;
   AddTest["Tear Down", Clear[isStillSetUp]];
   mytests["Tear Down"];
   AssertTrue[!ValueQ[isStillSetUp]];
  ]];

 AddTest["testRunTestRunsTearDown",
  Module[{isStillSetUp},
   AddTest["Tear Down", Clear[isStillSetUp]];
   AddTest["setsUp", isStillSetUp = True];
   RunTest["setsUp"];
   AssertTrue[!ValueQ[isStillSetUp]];
  ]];


(* ::Subsection:: *)
(*Test Format*)


(* ::Subsubsection::Closed:: *)
(*Test formatTestResult*)


AddTest["testFormatSingleSuccessfulTestResult", 
  AddTest["aTest", AssertEquals[1, 1]];
  AssertMatch[Column[{_Graphics, "1 run in 0. s, 0 failed"}], RunTest["aTest"]];
];

AddTest["testFormatTwoSuccessfulTestResult", 
  AddTest["aTest", AssertEquals[1, 1]];
  AddTest["anotherTest", AssertEquals[1, 1]];
  AssertMatch[Column[{_Graphics, "2 run in 0. s, 0 failed"}], RunTest[]]
];

AddTest["testFormatSingleFailedTestResult", 
  AddTest["aTest", AssertTrue[False]];
  AssertMatch[
   Column[{_Graphics, 
    "1 run in 0. s, 1 failed", 
    "aTest - Failed AssertTrue[False], gave False"}], 
   RunTest[]];
];

AddTest["testFormatOneEachTestResult", 
 AddTest["aTest", AssertEquals[1, 1]];
 AddTest["anotherTest", AssertEquals[1, -1]];
 AssertMatch[
  Column[{_Graphics, 
    "2 run in 0. s, 1 failed", 
    "anotherTest - Failed AssertEquals[1, -1], gave -1"}], 
  RunTest[]];
];


AddTest["testFormatHierarchicalTestResult",
 Module[{formattedResult, level1, level2, level3, i = 0},
  BeginSuite[level1];
  AddTest["test1.1", i++];
    BeginSubsuite[level2];
      AddTest["test2.1", i+=2];
      AddTest["test2.2", i+=3; AssertEquals[1, -1]];
      BeginSubsuite[level3];
        AddTest["test3.1", i+=4; AssertTrue[1 < 0]];
        AddTest["test3.2", i+=5];
      EndSuite[];
    EndSuite[];
  EndSuite[];
  formattedResult = RunTest[level1];
  AssertEquals[15, i];
  AssertMatch[
   Column[{_Graphics, 
    "5 run in 0. s, 2 failed", 
    "test2.2 - Failed AssertEquals[1, -1], gave -1",
    "test3.1 - Failed AssertTrue[1 < 0], gave False"}], 
   formattedResult];
]];


AddTest["testFormatAssertMessageExpectedMessage", 
 AddTest["aTest", AssertMessage[Drop::drop, Drop[{1}, 1]]];
 AssertMatch[
   Column[{_Graphics, 
      "1 run in 0. s, 1 failed", 
      "aTest - Failed AssertMessage[Drop::drop, Drop[{1}, 1]], gave {}"}], 
   RunTest[]];
];
AddTest["testFormatAssertNoMessage", 
 Module[{formattedResult},
  mess::aMessage = "Message!";
  AddTest["aTest", AssertNoMessage[Message[mess::aMessage]]];
  Quiet[formattedResult = RunTest[], mess::aMessage];
  AssertMatch[
   Column[{_Graphics, 
      "1 run in 0. s, 1 failed", 
      "aTest - Failed AssertNoMessage[Message[eMUnit`PackageTests`mess::aMessage]],\
 gave {HoldForm[eMUnit`PackageTests`mess::aMessage]}"}], 
   formattedResult];
]];


(* ::Subsection::Closed:: *)
(*Tail*)


EndSuite[];
