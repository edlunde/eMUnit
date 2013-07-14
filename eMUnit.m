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

AssertMessage::usage = "";
AssertNoMessage::usage = "";

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

RunTest::usage = "RunTest[suite, stringPattern] runs all tests matching stringPattern \
and formats the output.\n\
RunTest[suite] runs all tests in the suite and formats the output.\n\
RunTest[] runs all tests in the current suite.";

BeginSuite::usage = "BeginSuite[suite] sets the current suite until next EndSuite[].\
It is recommended that corresponding BeginSuite and EndSuite be placed in the same cell.";
EndSuite::usage = "EndSuite[] sets the current suite to whatever it was before the\
last BeginSuite[].";

BeginSubsuite::usage = "BeginSubsuite[subsuite] requires a suite to be set and runs\
AddSuite[subsuite] followed by BeginSuite[subsuite]. Should be used in conjunction\
with EndSuite[] just like BeginSuite.";

TestEMUnitPackage::usage = "TestEMUnitPackage[] runs all unit tests \
for the eMUnit package.";

eMUnitMessages::usage = "eMUnitMessages::tag - Messages used in the eMUnit package.";


(* ::Section:: *)
(*Implementations*)


Begin["`Private`"];


SetAttributes[AssertEquals, HoldRest]
AssertEquals[shouldBe_, expr_] := 
 If[Unevaluated[shouldBe] === expr, Null, 
  Throw[HoldComplete[AssertEquals[shouldBe, expr]], "AssertEquals"]]

SetAttributes[AssertTrue, HoldFirst]
AssertTrue[expr_] :=
 If[TrueQ@expr, Null, Throw[HoldComplete[AssertTrue[expr]], "AssertTrue"]]

SetAttributes[{AssertNoMessage, AssertMessage}, HoldAll]
AssertNoMessage[expr_] := AssertMessage[{}, expr]
AssertMessage[message_MessageName | message : {}, expr_] := 
 Module[{assertSucceeded, messageList},
  Block[{$MessageList = {}}, 
   Quiet[
    expr;
    assertSucceeded = If[Unevaluated@message === {}, 
                         $MessageList === {}, 
                         {HoldForm@message} === $MessageList];
    , message];
   messageList = $MessageList;
  ];
  Unprotect[$MessageList]; $MessageList = messageList; Protect[$MessageList];
  If[assertSucceeded, Null,
     Throw[HoldComplete[AssertMessage[message, expr]], "AssertMessage"]];
 ]


BeginSuite[suite_] := (If[!ListQ[suiteStack], suiteStack = {}]; 
  AppendTo[suiteStack, suite])
EndSuite[] := If[Length[suiteStack] > 0, suiteStack = Drop[suiteStack, -1]]
currentSuite[] := suiteStack[[-1]]
currentSuiteSetQ[] := If[Length[suiteStack] > 0, True, 
  Message[eMUnitMessages::suiteNotSet]; False]
eMUnitMessages::suiteNotSet = "No suite set with BeginSuite[].";


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

BeginSubsuite[subsuite_] /; currentSuiteSetQ[] := 
 (AddSuite[subsuite]; 
  BeginSuite[subsuite])


DeleteTest[name_] /; currentSuiteSetQ[] := DeleteTest[currentSuite[], name]
DeleteTest[suite_, name_] := (suite[name] =.; 
  suite[UnitTests] = suite[UnitTests] /. name -> Sequence[];)


RunTest[] /; currentSuiteSetQ[] := RunTest[currentSuite[]]
RunTest[suite_] := formatTestResult[runTest[suite]]
RunTest[stringPattern_?isStringPatternQ] /; currentSuiteSetQ[] := 
  RunTest[currentSuite[], stringPattern]

RunTest[suite_, stringPattern_?isStringPatternQ] /; testExists[suite, stringPattern] := 
  formatTestResult[runTest[suite, #] & /@ selectTests[suite, stringPattern]]
selectTests[suite_, pattern_] := 
  Select[ListTests[suite], StringQ[#] && StringMatchQ[#, pattern] &]
testExists[suite_, pattern_] := 
  If[Length[selectTests[suite, pattern]] > 0, True, 
     Message[eMUnitMessages::nonexistentTest, suite, pattern]; False]
eMUnitMessages::nonexistentTest = "No test in suite '`1`' matches '`2`'";
isStringPatternQ[expr_] := MemberQ[{String, StringExpression}, Head[expr]]

runTest[suite_] := runTest[suite, #] & /@ ListTests[suite]
runTest[suite_, name_] := Module[{result},
  suite["Set Up"];
  result = Catch[suite[name], "AssertEquals"|"AssertTrue"];
  suite["Tear Down"];
  createTestResult[suite, name, result]
 ]


createTestResult[suite_, name_, result_] := testResult[suite, name, result]
isFailure[result_testResult] := Head[result[[-1]]] === HoldComplete
getTest[result_testResult] := result[[2]]
getResult[result_testResult] := result[[3]]
evaluateAssertExpr[failure_?isFailure] := failure[[3, 1, -1]]


formatTestResult[results : {__testResult}] :=
 Module[{reportString, failures, nTests, nFailures},
  nTests = countTests[results];
  failures = extractFailures[results];
  nFailures = Length@failures;
  Column[Join[{drawBar[nFailures], 
               formatSummaryString[nTests, nFailures]},
              formatFailureString /@ failures]]
  ]
formatSummaryString[nResults_Integer, nFailures_Integer] := 
  ToString[nResults] <> " run, " <> ToString[nFailures] <> " failed"
formatFailureString[failure_testResult] := 
 Module[{assertString, failureString},
  assertString = replaceHoldWithToString @@ getResult[failure];
  getTest[failure] <> " - Failed " <> assertString <>  
    ", gave " <> ToString@evaluateAssertExpr[failure]
 ]
SetAttributes[replaceHoldWithToString, HoldAll]
replaceHoldWithToString[expr_] := ToString[Unevaluated[expr]]
drawBar[nFailures_Integer] := 
 Graphics[{If[nFailures > 0, Red, Green], 
   Rectangle[{0, 0}, {15, 1}]}, Method -> {"ShrinkWrap" -> True}, 
  ImageSize -> 600]

countTests[results : {__testResult}] := Length @ 
    CasesDontEnterHold[results, res_testResult /; ! MatchQ[res[[3]], {__testResult}]]
extractFailures[results : {__testResult}] := CasesDontEnterHold[results, _?isFailure]

casesHold[HoldComplete[exp_], patt_] := HoldComplete[exp]
casesHold[exp_, patt_] := 
  (If[MatchQ[exp, patt], Sow[exp]]; casesHold[#, patt] & /@ exp;)
CasesDontEnterHold[exp_, patt_] := 
  Reap[casesHold[exp, patt]] /. Null -> Sequence[] // Flatten

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
(*Test suiteNotSet message*)


(*AddTest["testCurrentSuiteRecheck", Module[{a = "notTouched"},
 EndSuite[];
 AddTest[mytests, "atest", a = If[Length@# > 1, #[[1]], #]& @ Trace[ListTests[]]];
 Block[{$MessageList = {}}, 
  Quiet[
   RunTest[mytests];
   AssertEquals[HoldForm@ListTests[], a];
   AssertEquals[{HoldForm[eMUnitMessages::suiteNotSet]}, $MessageList];
   BeginSuite[mytests];
   RunTest[mytests];
   EndSuite[];
   AssertEquals[{"atest"}, ReleaseHold@a];
   AssertEquals[{HoldForm[eMUnitMessages::suiteNotSet]}, $MessageList];
   RunTest[mytests];
   AssertEquals[{HoldForm[eMUnitMessages::suiteNotSet], 
                 HoldForm[eMUnitMessages::suiteNotSet]}, $MessageList];
  , eMUnitMessages::suiteNotSet];
]]]*)


AddTest["testSuiteNotSetMessage",
 EndSuite[];
 Block[{$MessageList = {}}, 
  Quiet[
   AddTest["testWithoutSuite", 1+1];
   AssertEquals[{HoldForm[eMUnitMessages::suiteNotSet]}, $MessageList]
   , eMUnitMessages::suiteNotSet];
  AssertEquals[{}, $MessageList];
 ]
]


(* ::Subsection::Closed:: *)
(*Test EndSuite*)


AddTest[eMUnit`PackageTests`frameworkTests, "testEndSuiteEmptyStack",
 Block[{$MessageList = {}}, 
  Quiet[
   BeginSuite[mytests];
   Do[EndSuite[];,{5}];
   AssertEquals[{}, $MessageList];
   , {Drop::drop}]
 ]]


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
(*Test AssertMessage*)


(* ::Text:: *)
(*AssertNoMessage should throw differently*)
(*test format AssertMessage*)
(**)
(*use to refactor runtest, suitenotset*)


AddTest["testAssertMessageRuns", Module[{mess, i = 0},
  mess::aMessage = "Message!";
  Catch[AssertMessage[mess::aMessage, i++], "AssertMessage"];
  AssertEquals[1, i];
]]


AddTest["testAssertNoMessage", Module[{mess, messenger, result},
  result = Catch[AssertNoMessage[1+1]; "noThrow", "AssertMessage"];
  AssertEquals["noThrow", result];
  mess::aMessage = "Message!";
  messenger := Message[mess::aMessage];
  Quiet[
    result = Catch[AssertNoMessage[messenger]; "noThrow", "AssertMessage"];
  , mess::aMessage];
  AssertEquals[HoldComplete[AssertMessage[{}, messenger]], result];
]]

AddTest["testAssertMessageCorrectMessage", Module[{mess, messenger, result},
  mess::aMessage = "Message!";
  messenger := Message[mess::aMessage];
  result = Catch[AssertMessage[mess::aMessage, messenger]; "noThrow", "AssertMessage"];
  AssertEquals["noThrow", result];
]]

AddTest["testAssertMessageThrows", Module[{mess, result},
  mess::aMessage = "Message!";
  result = Catch[AssertMessage[mess::aMessage, "noMessage"], "AssertMessage"];
  AssertEquals[HoldComplete[AssertMessage[mess::aMessage, "noMessage"]], result];
]]


AddTest["testAssertMessageQuiet", 
  EndSuite[];
  Block[{$MessageList = {}}, 
   Quiet[
    Catch[AssertMessage[eMUnitMessages::suiteNotSet, ListTests[]], "AssertMessage"];
    AssertEquals[{}, $MessageList];
   , eMUnitMessages::suiteNotSet];
]]
AddTest["testAssertMessageNotQuietOther",
  EndSuite[];
  Block[{$MessageList = {}}, 
   Quiet[
    Catch[AssertMessage[eMUnitMessages::nonexistentTest, ListTests[]]
        , "AssertMessage"];
    AssertEquals[{HoldForm[eMUnitMessages::suiteNotSet]}, $MessageList];
   , eMUnitMessages::suiteNotSet];
]]


AddTest["testAssertMessageIndepOfOtherMessages", Module[{mess, messenger, result},
  mess::aMessage = "Message!";
  messenger := Message[mess::aMessage];
  Quiet[
   messenger;
   result = Catch[AssertMessage[mess::aMessage, messenger], "AssertMessage"];
  , mess::aMessage];
  AssertEquals[Null, result];
]]


(* ::Subsection::Closed:: *)
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


(* ::Subsection::Closed:: *)
(*Test AddSuite*)


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


(* ::Subsection::Closed:: *)
(*Test BeginSubsuite*)


AddTest["testBeginSubsuite",
 ClearAll[mySubsuite];
 BeginSubsuite[mySubsuite];
 AddTest["aTest", 1+1];
 EndSuite[];
 AssertEquals[{eMUnit`PackageTests`mySubsuite}, ListTests[]];
 AssertEquals[{"aTest"}, ListTests[ListTests[mytests][[1]]]]
];


(* ::Subsection::Closed:: *)
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
 Block[{$MessageList = {}}, 
  Quiet[
   result = RunTest[__ ~~ "nonmatchingPattern"];
   AssertEquals[{HoldForm[eMUnitMessages::nonexistentTest]}, $MessageList]
   , eMUnitMessages::nonexistentTest];
  AssertEquals[{}, $MessageList];
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
  Block[{$MessageList = {}}, 
   Quiet[
    RunTest[mytests, __~~"NotExistingInParent"];
    AssertEquals[{HoldForm[eMUnitMessages::nonexistentTest]}, $MessageList]
   , eMUnitMessages::nonexistentTest];
   AssertEquals[{}, $MessageList];
  ];
  AssertEquals[0, i];
  RunTest[mySubsuite, __~~"NotExistingInParent"]
  AssertEquals[1, i];
]];


(* ::Subsection::Closed:: *)
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


(* ::Subsection::Closed:: *)
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


(* ::Subsection::Closed:: *)
(*Test DeleteTest*)


 AddTest["testDeleteTest",
  AddTest["aTest", a = 1];
  AddTest["anotherTest", b = 1];
  DeleteTest["aTest"];
  AssertEquals[{"anotherTest"}, ListTests[]];
 ];


(* ::Subsection::Closed:: *)
(*Test formatTestResult*)


AddTest["testFormatSingleSuccessfulTestResult", 
 Module[{formattedResult},
  AddTest["aTest", AssertEquals[1, 1]];
  formattedResult = RunTest["aTest"];
  AssertTrue[MatchQ[formattedResult, Column[{_Graphics, "1 run, 0 failed"}]]]
 ]];

AddTest["testFormatTwoSuccessfulTestResult", 
 Module[{formattedResult},
  AddTest["aTest", AssertEquals[1, 1]];
  AddTest["anotherTest", AssertEquals[1, 1]];
  formattedResult = RunTest[];
  AssertTrue[MatchQ[formattedResult, Column[{_Graphics, "2 run, 0 failed"}]]]
 ]];

AddTest["testFormatSingleFailedTestResult", 
 Module[{formattedResult},
  ClearAll[uniqueA]; uniqueB := False;
  AddTest["aTest", AssertTrue[uniqueB || uniqueA]];
  formattedResult = RunTest["aTest"];
  AssertTrue[
   MatchQ[formattedResult, 
    Column[{_Graphics, 
      "1 run, 1 failed", 
      "aTest - Failed AssertTrue[eMUnit`PackageTests`uniqueB || \
eMUnit`PackageTests`uniqueA], gave eMUnit`PackageTests`uniqueA"}]]
   ]]];

AddTest["testFormatOneEachTestResult", 
 Module[{formattedResult},
  AddTest["aTest", AssertEquals[1, 1]];
  AddTest["anotherTest", AssertEquals[1, -1]];
  formattedResult = RunTest[];
  AssertTrue[
   MatchQ[formattedResult, 
    Column[{_Graphics, 
      "2 run, 1 failed", "anotherTest - Failed AssertEquals[1, -1], gave -1"}]]
  ]]];

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
  AssertTrue[
   MatchQ[formattedResult, 
    Column[{_Graphics, 
      "5 run, 2 failed", 
      "test2.2 - Failed AssertEquals[1, -1], gave -1",
      "test3.1 - Failed AssertTrue[1 < 0], gave False"}]]
  ]
]]


(* ::Section::Closed:: *)
(*Tail*)


EndSuite[];


End[];


EndPackage[]
