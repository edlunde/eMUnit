(* ::Package:: *)

(* ::Section:: *)
(*Implementations*)


(* ::Subsection::Closed:: *)
(*Asserts*)


assertExceptionNames = {}; (* Added for each assert where defined *)
isAssertExceptionName[name_String] := MemberQ[assertExceptionNames, name]

SetAttributes[throwAssertException, HoldRest];
throwAssertException[name_?isAssertExceptionName, callExpression_, result_] := 
   Throw[assertException[HoldComplete[callExpression], result], name]

getAssertExceptionExpr[exception_assertException] := exception[[1]]
getAssertExceptionResult[exception_assertException] := exception[[2]]
getAssertExceptionExprString[exception_assertException] := 
 replaceHoldWithToString@@getAssertExceptionExpr[exception]
SetAttributes[replaceHoldWithToString, HoldAll]
replaceHoldWithToString[expr_] := ToString[Unevaluated[expr], InputForm]


AppendTo[assertExceptionNames, "AssertEquals"];
SetAttributes[AssertEquals, HoldRest]
AssertEquals[shouldBe_, expr_] := With[{evaluated = expr},
 If[Unevaluated[shouldBe] === evaluated, (* Unevaluated to make sure shouldBe is 
      only run twice, reduces possibility of confusion if there are side effects *)
  Null, 
  throwAssertException["AssertEquals", AssertEquals[shouldBe, expr], evaluated]]]

AppendTo[assertExceptionNames, "AssertEqualsN"];
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

AppendTo[assertExceptionNames, "AssertMatch"];
SetAttributes[AssertMatch, HoldRest]
AssertMatch[form_, expr_] := With[{evaluated = expr},
 If[MatchQ[evaluated, form], Null,
    throwAssertException["AssertMatch", AssertMatch[form, expr], evaluated]]]

AppendTo[assertExceptionNames, "AssertTrue"];
SetAttributes[AssertTrue, HoldFirst]
AssertTrue[expr_] := With[{evaluated = expr},
 If[TrueQ@evaluated, Null, 
    throwAssertException["AssertTrue", AssertTrue[expr], evaluated]]]


AppendTo[assertExceptionNames, "AssertMessage"];

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


(* ::Subsection::Closed:: *)
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


createTestResult[suite_Symbol, name_, result_] := testResult[suite, name, result]
(*getSuite[result_testResult] := result[[1]]*)
getTest[result_testResult] := result[[2]]
getResult[result_testResult] := result[[3]]
isFailure[result_testResult] := Head[getResult[result]] === assertException

getFailureResult[failure_?isFailure] := getAssertExceptionResult[getResult[failure]]
getFailureExpressionString[failure_?isFailure] := 
 getAssertExceptionExprString[getResult[failure]];


(* ::Subsection::Closed:: *)
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
formatFailureString[failure_testResult] := 
  getTest[failure] <> " - Failed " <> getFailureExpressionString[failure] <>  
    ", gave " <> ToString[getFailureResult[failure], InputForm]
    
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
