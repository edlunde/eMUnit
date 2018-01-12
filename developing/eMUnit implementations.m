(* ::Package:: *)

(* ::Section:: *)
(*Implementations*)


(* ::Subsection:: *)
(*Asserts*)


(* ::Subsubsection::Closed:: *)
(*Common*)


assertExceptionNames = {}; (* Added for each assert where defined *)
isAssertExceptionName[name_] := StringQ@name && MemberQ[assertExceptionNames, name]

SetAttributes[throwAssertException, HoldRest];
throwAssertException[name_?isAssertExceptionName, callExpression_, result_] := 
   Throw[assertException[HoldComplete[callExpression], result], name]

getAssertExceptionExpr[exception_assertException] := exception[[1]]
getAssertExceptionResult[exception_assertException] := exception[[2]]
getAssertExceptionExprString[exception_assertException] := 
 replaceHoldWithToString@@getAssertExceptionExpr[exception]
SetAttributes[replaceHoldWithToString, HoldAll]
replaceHoldWithToString[expr_] := ToString[Unevaluated[expr], InputForm]


(* ::Subsubsection::Closed:: *)
(*AssertEquals, AssertMatch, AssertTrue*)


AppendTo[assertExceptionNames, "AssertEquals"];
SetAttributes[AssertEquals, HoldRest]
AssertEquals[shouldBe_, expr_] := With[{evaluated = expr},
 If[Unevaluated[shouldBe] === evaluated,
  Null, 
  throwAssertException["AssertEquals", AssertEquals[shouldBe, expr], evaluated]]]

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


(* ::Subsubsection::Closed:: *)
(*AssertEqualsN, AssertMatchN*)


With[{defaultTolerance = 0.001},
 eMUnitMessages::nonNumericTolerance = "Value of option Tolerance -> `1` is not numeric,\
 using default tolerance " <> ToString@defaultTolerance;
 
 Options[AssertEqualsN] = {Tolerance -> defaultTolerance};
 Options[AssertMatchN] = {Tolerance -> defaultTolerance};
 
 getTolerance[tol_] := 
   If[NumericQ@tol,
     tol,
     (Message[eMUnitMessages::nonNumericTolerance, tol]; 
       defaultTolerance)(* Falling back on default tolerance and sending warning *)
     ]; 
]


AppendTo[assertExceptionNames, "AssertEqualsN"];
SetAttributes[AssertEqualsN, HoldRest];
AssertEqualsN[shouldBe_, expr_, OptionsPattern[]] := 
 With[
  {evaluated = expr, tol = getTolerance@OptionValue[Tolerance]},
  If[sameQN[shouldBe, evaluated, tol], Null, 
   throwAssertException["AssertEqualsN", AssertEqualsN[shouldBe, expr, Tolerance -> tol], 
    evaluated]]];


AppendTo[assertExceptionNames, "AssertMatchN"];
SetAttributes[AssertMatchN, HoldRest];
AssertMatchN[form_, expr_, OptionsPattern[]] := 
 With[{evaluated = expr, tol = getTolerance@OptionValue[Tolerance]}, 
 If[matchQN[evaluated, form, tol],
  Null, 
  throwAssertException["AssertMatchN", AssertMatchN[form, expr, Tolerance -> tol],
   evaluated]]];


ClearAll@sameQN
sameQN[lhs_, rhs_, tol_] /; SameQ[lhs, rhs] := True
sameQN[lhs_?NumericQ, rhs_?NumericQ, tol_] :=
 Abs[lhs - rhs] <= tol

sameQN[lhsIn_, rhsIn_, tol_] := 
 With[{lhs = handleAssociations[lhsIn], rhs = handleAssociations[rhsIn]},
  nonNumericalStructureSameQ[lhs, rhs] &&
   numericalLeavesSameToToleranceQ[lhs, rhs, tol]]
   
nonNumericalStructureSameQ[lhs_, rhs_] := 
  SameQ @@ (replaceNumericsWithSymbol /@ {lhs, rhs})
replaceNumericsWithSymbol[expr_] := expr /. _?NumericQ -> Symbol

numericalLeavesSameToToleranceQ[lhs_, rhs_, tol_] := 
  And @@ (sameQN[#1, #2, tol] & @@@ pairUpNumericalLeaves[lhs, rhs])


pairUpNumericalLeaves[expr_, form_] := Module[{res, newForm, varsAndVals},
 res = Reap[form /. z_?NumericQ :> With[{y = Unique[x]}, Sow@{y,z}; y_?NumericQ]];
 newForm = First@res;
 varsAndVals = res[[2,1]];
 First@Cases[expr, newForm :> Evaluate@varsAndVals, All]
]

(* Associations behave as atoms in replacements so we need to change them to
   some head with more standard behavior. Chose association over List or similar
   to hopefully make stack traces easier to read. *)
handleAssociations[expr_] := 
 expr /. Association -> association


ClearAll@matchQN
matchQN[expr_, form_, tol_] /; MatchQ[expr, form] := True
matchQN[expr_?NumericQ, form_?NumericQ, tol_] :=
 Abs[expr - form] <= tol
(*matchQN[expr_, form_, tol_] /; AtomQ@expr || AtomQ@expr := 
 MatchQ[expr, form]*)
matchQN[exprIn_, formIn_, tol_] := 
 With[{expr = handleAssociations[exprIn], form = handleAssociations[formIn]},
  nonNumericalStructureMatchesQ[expr, form] &&
   numericalLeavesMatchToToleranceQ[expr, form, tol]]
  
nonNumericalStructureMatchesQ[expr_, form_] :=
 MatchQ[expr, replaceNumericsWithNumericQPatterns[form]]
replaceNumericsWithNumericQPatterns[form_] := form /.  _?NumericQ -> _?NumericQ

numericalLeavesMatchToToleranceQ[expr_, form_, tol_] :=
 And@@(leavesMatchQ[#,tol]& /@ handleAlternatives@pairUpNumericalLeaves[expr, form])

(* If there are Alternatives[...] in form, pairUpNumericalLeaves will give the value
   to the first alternative and put rest of the alternatives as length 1 lists after.
   Handle alternatives pulls them together into one list *)
handleAlternatives[listOfLeaves : {{__}..}] :=
 listOfLeaves //. 
  {before___, {value_, alternatives__}, {anotherAlternative_}, after___} :>
   {before, {value, alternatives, anotherAlternative}, after}

leavesMatchQ[{leaf1_, leaf2_}, tol_] := matchQN[leaf1, leaf2, tol]
(* If there are Alternatives[...] in form, handleAlternatives has pulled them together
   into one list, only one needs to match. *)
leavesMatchQ[{leaf1_, alternatives__}, tol_] :=
 Or@@(matchQN[leaf1, #, tol]& /@ {alternatives})



(* ::Subsubsection::Closed:: *)
(*AssertMessage*)


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


(* ::Subsection:: *)
(*Begin, List, Add, Delete*)


(* ::Subsubsection::Closed:: *)
(*BeginSuite*)


BeginSuite[suite_Symbol] := (If[!ListQ[suiteStack], suiteStack = {}]; 
  AppendTo[suiteStack, suite])
EndSuite[] := If[Length[suiteStack] > 0, suiteStack = Drop[suiteStack, -1]]
currentSuite[] := suiteStack[[-1]]

SetAttributes[runIfSuiteSet, HoldAll]
runIfSuiteSet[expr_] := If[currentSuiteSetQ[], expr]
currentSuiteSetQ[] := If[Length[suiteStack] > 0, True, 
  Message[eMUnitMessages::suiteNotSet]; False]
eMUnitMessages::suiteNotSet = "No suite set with BeginSuite[].";


(* ::Subsubsection::Closed:: *)
(*ListTests*)


ListTests[] := runIfSuiteSet[ListTests[currentSuite[]]]
ListTests[suite_Symbol] := suite[UnitTests]


(* ::Subsubsection::Closed:: *)
(*AddTest, DeleteTest*)


SetAttributes[AddTest, HoldRest];
AddTest[name_, test_] := runIfSuiteSet[AddTest[currentSuite[], name, test]]
AddTest[suite_Symbol, name_, test_] := (
  suite[name] := test;
  updateTestList[suite, name];
  name
)
(* suite[UnitTests] contains unit tests, suite also has internal "tests" that 
   shouldn't be visible to ListTest etc. *)
updateTestList[suite_Symbol, name_] := (
  If[!ListQ@suite[UnitTests], suite[UnitTests] = {}];
  If[shouldBeAdded[suite, name], AppendTo[suite[UnitTests], name]]
)
shouldBeAdded[suite_Symbol, name_] := 
 Not@MemberQ[Join[suite[UnitTests], {"Set Up", "Tear Down", "Parent Suite"}], name]


DeleteTest[name_] := runIfSuiteSet[DeleteTest[currentSuite[], name]]
DeleteTest[suite_Symbol, name_] := (suite[name] =.; 
  suite[UnitTests] = suite[UnitTests] /. name -> Sequence[];
  name)


(* ::Subsubsection::Closed:: *)
(*AddSuite, BeginSubsuite*)


AddSuite[subsuite_Symbol] := runIfSuiteSet[AddSuite[currentSuite[], subsuite]]
AddSuite[mainSuite_Symbol, subsuite_Symbol] := 
  (AddTest[subsuite, "Parent Suite", mainSuite];
   AddTest[mainSuite, subsuite, runTest[subsuite]])

BeginSubsuite[subsuite_Symbol] := 
  runIfSuiteSet[AddSuite[subsuite]; BeginSuite[subsuite]]


(* ::Subsection:: *)
(*RunTest*)


(* ::Subsubsection::Closed:: *)
(*RunTest*)


Options[RunTest] = {ReportMethod -> "Failures"};
RunTest[opts : OptionsPattern[]] := 
 runIfSuiteSet[RunTest[currentSuite[], opts]]
RunTest[suite_Symbol, opts : OptionsPattern[]] := 
 formatTestResult[runTest[suite], opts]
RunTest[stringPattern_?isStringPatternQ, opts : OptionsPattern[]] := 
 runIfSuiteSet[RunTest[currentSuite[], stringPattern, opts]]

RunTest[suite_Symbol, stringPattern_?isStringPatternQ, opts : OptionsPattern[]] /; 
    testExists[suite, stringPattern] := 
 formatTestResult[runTest[suite, #] & /@ selectTests[suite, stringPattern], opts]
selectTests[suite_Symbol, pattern_] := 
  Select[ListTests[suite], StringQ[#] && StringMatchQ[#, pattern] &]
testExists[suite_Symbol, pattern_] := 
  If[Length[selectTests[suite, pattern]] > 0, True, 
     Message[eMUnitMessages::nonexistentTest, suite, pattern]; False]
eMUnitMessages::nonexistentTest = "No test in suite '`1`' matches '`2`'";
isStringPatternQ[expr_] := MemberQ[{String, StringExpression}, Head[expr]]

runTest[suite_Symbol] := runTest[suite, #] & /@ ListTests[suite]
runTest[suite_Symbol, name_] := Module[{result, time},
  time = First@Timing[
   runSetUp[suite];
   result = Catch[suite[name], exceptionName_?isAssertExceptionName];
   runTearDown[suite]];
  createTestResult[suite, name, result, time]
 ]


runSetUp[suite_Symbol] :=
  {If[isSubsuite[suite], runSetUp[suite["Parent Suite"]]], 
   suite["Set Up"]}
runTearDown[suite_Symbol] :=
  {suite["Tear Down"], 
   If[isSubsuite[suite], runTearDown[suite["Parent Suite"]]]}
  
isSubsuite[suite_Symbol] := Head[suite["Parent Suite"]] === Symbol


createTestResult[suite_Symbol, name_, result_, time_?NumericQ] := 
 testResult[suite, name, result, time]
(*getSuite[result_testResult] := result[[1]]*)
getTest[result_testResult] := result[[2]]
getResult[result_testResult] := result[[3]]
getTime[result_testResult] := result[[4]]

isFailure[result_testResult] := Head[getResult[result]] === assertException
getFailureResult[failure_?isFailure] := getAssertExceptionResult[getResult[failure]]
getFailureExpressionString[failure_?isFailure] := 
 getAssertExceptionExprString[getResult[failure]]


(* ::Subsubsection::Closed:: *)
(*Format test results*)


formatTestResult[results : {__testResult}, OptionsPattern[RunTest]] :=
 Module[{nTests, time, nFailures, reportString},
  nTests = countTests[results];
  time = Total[getTime /@ results];
  nFailures = Length@extractFailures@results;
  reportString = getReportString[results, OptionValue[ReportMethod]];
  Column[Join[{drawBar[nFailures], 
               formatSummaryString[nTests, time, nFailures]},
               reportString]]
  ]

getReportString[results : {__testResult}, "Failures"] := 
 formatFailureString /@ extractFailures[results]
getReportString[results : {__testResult}, "Hierarchical"] := 
 formatHierarchical /@ results
getReportString[results : {__testResult}, opt___] := 
 Throw["Unknown option in formatTestResult"]


(* Counts the number of actual test, no matter how the subsuite structure looks like *)
countTests[results : {__testResult}] := Length @ 
    casesDontEnterHold[results, res_testResult /; isNotSubsuiteResultQ[res]]
isNotSubsuiteResultQ[result_testResult] := Not@MatchQ[getResult[result], {__testResult}]


extractFailures[results : {__testResult}] := casesDontEnterHold[results, _?isFailure]


(* Failed tests reports the instruction that made them fail within HoldComplete 
   so they don't run the instruction again. casesDontEnterHold is for doing Cases[..]
   without entering such HoldCompletes *)
casesDontEnterHold[exp_, patt_] := 
  Reap[casesHold[exp, patt]] /. Null -> Sequence[] // Flatten
casesHold[HoldComplete[exp_], patt_] := HoldComplete[exp]
casesHold[exp_, patt_] := 
  (If[MatchQ[exp, patt], Sow[exp]]; casesHold[#, patt] & /@ exp;)


drawBar[nFailures_Integer] := 
 Graphics[{If[nFailures > 0, Red, Green], 
   Rectangle[{0, 0}, {15, 1}]}, Method -> {"ShrinkWrap" -> True}, 
  ImageSize -> 600]


formatSummaryString[nResults_Integer, time_?NumericQ, nFailures_Integer] := 
  ToString[nResults] <> " run in " <> ToString@Round[time, 0.01] <> " s, " <> 
   ToString[nFailures] <> " failed"
formatFailureString[failure_testResult] := 
  getTest[failure] <> " - Failed " <> getFailureExpressionString[failure] <>  
    ", gave " <> ToString[getFailureResult[failure], InputForm]


formatHierarchical[result_?isNotSubsuiteResultQ, _String] :=
 statusMark@result <> getTest@result<>" "<> formatTime@getTime@result

formatHierarchical[result_testResult] := formatHierarchical[result, ""]
formatHierarchical[result_testResult, indentationIn_String] :=
 With[{indentation = indentationIn <> "    "},
  statusMark@result <> ToString@getTest@result <> " " <> 
   formatTime@getTime@result <> "\n" <> 
   indentation <> StringRiffle[formatHierarchical[#, indentation] & /@ 
                                getResult@result, "\n" <> indentation]
 ]

formatTime[t_?NumericQ] := If[#==0., "0.00", ToString@#] & @ Round[t, 0.01]

statusMark[result_testResult] := If[containsFailure@result, failureMark, successMark]
containsFailure[result_?isNotSubsuiteResultQ] := isFailure@result
containsFailure[result_testResult] := Length[extractFailures@getResult@result] > 0
failureMark = "-";
successMark = StringJoin@@Array[" "&, StringLength@failureMark];
