(* ::Package:: *)

(* ::Text:: *)
(*ToDo:*)
(**)
(*report timing*)
(*reorganize package file for easier development*)
(*Check errors in AddTest is caught by our testing*)
(**)
(*Maybe:*)
(*Add clearing of suite to BeginSuite so commenting out a test removes it without deleting it explicitly? Or a function ClearSuite to do it explicitly?*)
(*errors - catching unexpected exceptions (and messages)*)
(*Logging?*)
(*redesign public interface, RunTest called something else?*)
(*Test coverage analysis*)
(*go through UnitTest to get ideas (catch errors from tests? time/memoryConstrained)*)


(* ::Text:: *)
(*bug? counting teardown in number of tests run?*)


(* ::Text:: *)
(*bug? something to do with earlier definition of function more permissive and not cleared?*)
(*testIsCorrectExtraVectors - Failed AssertEquals[False, isCorrectExtraVectors$36936[{{1, 2}}]], gave HoldComplete[AssertEquals[False, isCorrectExtraVectors$36936[{{1, 2}}]]]*)


(* ::Section::Closed:: *)
(*Declarations and usage*)


BeginPackage["eMUnit`"];


AssertEquals::usage = "AssertEquals[value, expression] returns Null if expression \
evaluates to value. Otherwise it throws an AssertEquals-exception to be caught \
by RunTest.";

AssertEqualsN::usage = "AssertEquals[value, expression, Tolerance -> 0.001] returns \
Null if expression evaluates numerically to within Tolerance of value. Otherwise it \
throws an AssertEquals-exception to be caught by RunTest.";

AssertMatch::usage = "AssertMatch[form, expression] returns Null if expression \
matches form. Otherwise it throws an AssertMatch-exception to be caught \
by RunTest.";

AssertTrue::usage = "AssertTrue[expression] returns Null if expression \
evaluates to True. Otherwise it throws an AssertTrue-exception to be caught \
by RunTest.";

AssertMessage::usage = "AssertMessage[message, expr] evaluates expr and checks if \
the message is generated (and quiets it). If not, it throws an AssertMessage-exception \
to be caught by RunTest.\n\
AssertNoMessage[expr] evaluates expr. If a message is sent,\
 it throws an\[NonBreakingSpace]AssertMessage-exception to be caught by RunTest.";
AssertNoMessage::usage = AssertMessage::usage;

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


(* ::Subsection::Closed:: *)
(*Asserts*)


SetAttributes[AssertEquals, HoldRest]
AssertEquals[shouldBe_, expr_] := With[{evaluated = expr},
 If[Unevaluated[shouldBe] === evaluated, (* Unevaluated to make sure shouldBe is 
      only run twice, reduces possibility of confusion if there are side effects *)
  Null, 
  throwAssertException["AssertEquals", AssertEquals[shouldBe, expr], evaluated]]]

With[{defaultTolerance = 0.001},
 AssertEqualsN::nonNumericTolerance = "Value of option Tolerance -> `1` is not numeric";
 Options[AssertEqualsN] = {Tolerance -> defaultTolerance};
 SetAttributes[AssertEqualsN, HoldRest];
 AssertEqualsN[shouldBe_, expr_, OptionsPattern[]] := 
  With[
   {evaluated = expr, 
    tol = If[NumericQ@OptionValue[Tolerance],
     OptionValue[Tolerance],
     (Message[AssertEqualsN::nonNumericTolerance,OptionValue[Tolerance]]; 
      defaultTolerance)]},
   If[N@Abs[shouldBe - evaluated] <= tol, Null, 
    throwAssertException["AssertEqualsN", AssertEqualsN[shouldBe, expr, Tolerance -> tol], 
     evaluated]]];
]

SetAttributes[AssertMatch, HoldRest]
AssertMatch[form_, expr_] := With[{evaluated = expr},
 If[MatchQ[evaluated, form], Null,
    throwAssertException["AssertMatch", AssertMatch[form, expr], evaluated]]]

SetAttributes[AssertTrue, HoldFirst]
AssertTrue[expr_] := With[{evaluated = expr},
 If[TrueQ@evaluated, Null, 
    throwAssertException["AssertTrue", AssertTrue[expr], evaluated]]]


SetAttributes[{AssertNoMessage, AssertMessage, 
               assertMessage, quietEvaluateAndCheckMessages}, HoldAll]
AssertNoMessage[expr_] := assertMessage[{}, expr, AssertNoMessage[expr]]
AssertMessage[message_MessageName, expr_] := 
  assertMessage[{message}, expr, AssertMessage[message, expr]]

assertMessage[expectedMessages : {___MessageName}, expr_, originalCall_] := 
Module[{onlyExpectedMessagesQ, uncaughtMessages},
 {onlyExpectedMessagesQ, uncaughtMessages} = 
        quietEvaluateAndCheckMessages[expectedMessages, expr];
 If[onlyExpectedMessagesQ, Null,
    passOnMessages[uncaughtMessages];
    throwAssertException["AssertMessage", originalCall, uncaughtMessages]]
]

quietEvaluateAndCheckMessages[expectedMessages_, expr_] := Block[{$MessageList = {}}, 
 Module[{onlyExpectedMessagesQ},
   Quiet[
    expr;
    onlyExpectedMessagesQ = HoldForm /@ Unevaluated[expectedMessages] === $MessageList;
    , expectedMessages];
   {onlyExpectedMessagesQ, $MessageList}]]
passOnMessages[uncaughtMessages_] := 
  (Unprotect[$MessageList]; $MessageList = uncaughtMessages; Protect[$MessageList];)


SetAttributes[throwAssertException, HoldRest];
throwAssertException[name_?isAssertExceptionName, expr_, result_] := 
   Throw[assertException[HoldComplete[expr], result], name]
isAssertExceptionName[name_String] := MemberQ[assertExceptionNames, name]
assertExceptionNames = 
  {"AssertEquals", "AssertEqualsN", "AssertMatch", "AssertTrue", "AssertMessage"};

createTestResult[suite_Symbol, name_, result_] := testResult[suite, name, result]
isFailure[result_testResult] := Head[getResult[result]] === assertException
getTest[result_testResult] := result[[2]]
getResult[result_testResult] := result[[3]]
getEvaluatedAssertExpr[failure_?isFailure] := getResult[failure][[-1]]
getResultString[failure_?isFailure] := replaceHoldWithToString@@getResult[failure][[1]];
SetAttributes[replaceHoldWithToString, HoldAll]
replaceHoldWithToString[expr_] := ToString[Unevaluated[expr], InputForm]


(* ::Subsection::Closed:: *)
(*Begin, List, Add, Delete*)


BeginSuite[suite_Symbol] := (If[!ListQ[suiteStack], suiteStack = {}]; 
  AppendTo[suiteStack, suite])
EndSuite[] := If[Length[suiteStack] > 0, suiteStack = Drop[suiteStack, -1]]
currentSuite[] := suiteStack[[-1]]

SetAttributes[runIfSuiteSet, HoldAll]
runIfSuiteSet[expr_] := If[currentSuiteSetQ[], expr]
currentSuiteSetQ[] := If[Length[suiteStack] > 0, True, 
  Message[eMUnitMessages::suiteNotSet]; False]
eMUnitMessages::suiteNotSet = "No suite set with BeginSuite[].";


ListTests[] := runIfSuiteSet[ListTests[currentSuite[]]]
ListTests[suite_Symbol] := suite[UnitTests]


SetAttributes[AddTest, HoldRest];
AddTest[name_, test_] := runIfSuiteSet[AddTest[currentSuite[], name, test]]
AddTest[suite_Symbol, name_, test_] := (
  suite[name] := test;
  updateTestList[suite, name];
  name
)
updateTestList[suite_Symbol, name_] := (
  If[!ListQ@suite[UnitTests], suite[UnitTests] = {}];
  If[shouldBeAdded[suite, name], AppendTo[suite[UnitTests], name]]
)
shouldBeAdded[suite_Symbol, name_] := 
 Not@MemberQ[Join[suite[UnitTests], {"Set Up", "Tear Down"}], name]


AddSuite[subsuite_Symbol] := runIfSuiteSet[AddSuite[currentSuite[], subsuite]]
AddSuite[mainSuite_Symbol, subsuite_Symbol] := 
   AddTest[mainSuite, subsuite, runTest[subsuite]]

BeginSubsuite[subsuite_Symbol] := 
  runIfSuiteSet[AddSuite[subsuite]; BeginSuite[subsuite]]


DeleteTest[name_] := runIfSuiteSet[DeleteTest[currentSuite[], name]]
DeleteTest[suite_Symbol, name_] := (suite[name] =.; 
  suite[UnitTests] = suite[UnitTests] /. name -> Sequence[];
  name)


(* ::Subsection:: *)
(*RunTest*)


RunTest[] := runIfSuiteSet[RunTest[currentSuite[]]]
RunTest[suite_Symbol] := formatTestResult[runTest[suite]]
RunTest[stringPattern_?isStringPatternQ] := 
  runIfSuiteSet[RunTest[currentSuite[], stringPattern]]

RunTest[suite_Symbol, stringPattern_?isStringPatternQ] /; 
 testExists[suite, stringPattern] := 
    formatTestResult[runTest[suite, #] & /@ selectTests[suite, stringPattern]]
selectTests[suite_Symbol, pattern_] := 
  Select[ListTests[suite], StringQ[#] && StringMatchQ[#, pattern] &]
testExists[suite_Symbol, pattern_] := 
  If[Length[selectTests[suite, pattern]] > 0, True, 
     Message[eMUnitMessages::nonexistentTest, suite, pattern]; False]
eMUnitMessages::nonexistentTest = "No test in suite '`1`' matches '`2`'";
isStringPatternQ[expr_] := MemberQ[{String, StringExpression}, Head[expr]]

runTest[suite_Symbol] := runTest[suite, #] & /@ ListTests[suite]
runTest[suite_Symbol, name_] := Module[{result},
  suite["Set Up"];
  result = Catch[suite[name], exceptionName_?isAssertExceptionName];
  suite["Tear Down"];
  createTestResult[suite, name, result]
 ]


(* ::Subsection:: *)
(*Format*)


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
formatFailureString[failure_testResult] := Module[{assertString, failureString},
  assertString = getResultString[failure];
  getTest[failure] <> " - Failed " <> assertString <>  
    ", gave " <> ToString[getEvaluatedAssertExpr[failure], InputForm]
]
drawBar[nFailures_Integer] := 
 Graphics[{If[nFailures > 0, Red, Green], 
   Rectangle[{0, 0}, {15, 1}]}, Method -> {"ShrinkWrap" -> True}, 
  ImageSize -> 600]

countTests[results : {__testResult}] := Length @ 
    CasesDontEnterHold[results, res_testResult /; Not@isSubsuiteResultQ[res]]
isSubsuiteResultQ[result_testResult] := MatchQ[getResult[result], {__testResult}]
extractFailures[results : {__testResult}] := CasesDontEnterHold[results, _?isFailure]

casesHold[HoldComplete[exp_], patt_] := HoldComplete[exp]
casesHold[exp_, patt_] := 
  (If[MatchQ[exp, patt], Sow[exp]]; casesHold[#, patt] & /@ exp;)
CasesDontEnterHold[exp_, patt_] := 
  Reap[casesHold[exp, patt]] /. Null -> Sequence[] // Flatten


(* ::Subsection:: *)
(*Tail*)


End[];


(* ::Section::Closed:: *)
(*Tests*)


(* ::Subsection::Closed:: *)
(*Head*)


Begin["`PackageTests`"];


TestEMUnitPackage[] := RunTest[frameworkTests]


(* Used to test AssertEquals, can't use it to test itself *)
throwSomething[text_] := 
  eMUnit`Private`throwAssertException["AssertEquals", text, ""]


ClearAll[frameworkTests];
BeginSuite[frameworkTests];


AddTest["Set Up", ClearAll[mytests, anotherSuite]; BeginSuite[mytests];];
AddTest["Tear Down", EndSuite[]; ClearAll[mytests, anotherSuite]];


(* ::Subsection::Closed:: *)
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


(* ::Subsection::Closed:: *)
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
 AssertMessage[AssertEqualsN::nonNumericTolerance, 
  Catch[AssertEqualsN[1, 5.1, Tolerance -> "string"], "AssertEqualsN"]]
];

AddTest["testAssertEqualsNThrow", 
 With[{x = 1.7},
  AssertEquals[eMUnit`Private`assertException[
                   HoldComplete[AssertEqualsN[1, x, Tolerance -> 0.5]], x], 
               Catch[AssertEqualsN[1, x, Tolerance -> 0.5], "AssertEqualsN"]]
 ]];


(* ::Subsection::Closed:: *)
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


(* ::Subsection::Closed:: *)
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


(* ::Subsection::Closed:: *)
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
    Column[{eMUnit`Private`drawBar[0], "1 run, 0 failed"}]},
  {"RunTestPattern", 
   (AddTest[anotherSuite, "anotherTest", 1+1]; RunTest["anotherTest"]),
    Column[{eMUnit`Private`drawBar[0], "1 run, 0 failed"}]}
}];


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
  AddTest["aTest", AssertEquals[1, 1]];
  AssertMatch[Column[{_Graphics, "1 run, 0 failed"}], RunTest["aTest"]];
];

AddTest["testFormatTwoSuccessfulTestResult", 
  AddTest["aTest", AssertEquals[1, 1]];
  AddTest["anotherTest", AssertEquals[1, 1]];
  AssertMatch[Column[{_Graphics, "2 run, 0 failed"}], RunTest[]]
];

AddTest["testFormatSingleFailedTestResult", 
  AddTest["aTest", AssertTrue[False]];
  AssertMatch[
   Column[{_Graphics, 
    "1 run, 1 failed", 
    "aTest - Failed AssertTrue[False], gave False"}], 
   RunTest[]];
];

AddTest["testFormatOneEachTestResult", 
 AddTest["aTest", AssertEquals[1, 1]];
 AddTest["anotherTest", AssertEquals[1, -1]];
 AssertMatch[
  Column[{_Graphics, 
    "2 run, 1 failed", 
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
    "5 run, 2 failed", 
    "test2.2 - Failed AssertEquals[1, -1], gave -1",
    "test3.1 - Failed AssertTrue[1 < 0], gave False"}], 
   formattedResult];
]];


AddTest["testFormatAssertMessageExpectedMessage", 
 AddTest["aTest", AssertMessage[Drop::drop, Drop[{1}, 1]]];
 AssertMatch[
   Column[{_Graphics, 
      "1 run, 1 failed", 
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
      "1 run, 1 failed", 
      "aTest - Failed AssertNoMessage[Message[eMUnit`PackageTests`mess::aMessage]],\
 gave {HoldForm[eMUnit`PackageTests`mess::aMessage]}"}], 
   formattedResult];
]];


(* ::Section::Closed:: *)
(*Tail*)


EndSuite[];


End[];


EndPackage[]
