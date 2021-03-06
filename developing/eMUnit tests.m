(* ::Package:: *)

(* ::Section:: *)
(*Tests*)


(* ::Subsection::Closed:: *)
(*Import specific private symbols*)


(* ::Text:: *)
(*The tests should be agnostic w.r.t. the implementations and only test the public interface. We allow exceptions for these specific symbols.*)


{pAssertException, pSameQN, pMatchQN} = 
 If[NameQ@"eMUnit`Private`assertExceptionNames",
  {eMUnit`Private`assertException, eMUnit`Private`sameQN, eMUnit`Private`matchQN},
  {assertException, sameQN, matchQN}
 ]


(* ::Subsection:: *)
(*Tests not using framework or incrementally using parts that are tested*)


(* ::Text:: *)
(*Testing a test framework in itself sounds circular, because it is. We have to be careful to build it up in steps, starting with testing the basic functionality with regular Ifs and Throws.*)


nonFrameworkTests = {};


(* ::Subsubsection::Closed:: *)
(*Test AssertEquals*)


(* Used to test AssertEquals, can't use it to test itself *)
(* Only works when run from complete package due to explicit mention of context eMUnit`Private` *)
throwSomething[text_] := 
  Throw[text, "nonFrameworkTestThrow"]


(* testThrowSomething *) AppendTo[nonFrameworkTests, Hold[
Module[{result},
 result = Catch[throwSomething["testThrowSomething throw"], 
   "nonFrameworkTestThrow"];
 If[Not[result === "testThrowSomething throw"], 
  Throw["!!! testThrowSomething failed, gave " <> ToString@result <> 
    "\nCannot trust tests of AssertEquals.", "throwSomethingFailed"]];
]]];


(* testAssertEqualsSuccess *) AppendTo[nonFrameworkTests, Hold[
If[Not[Catch[AssertEquals[1, 1], "AssertEquals"] === Null], 
 throwSomething["testAssertEqualsSuccess failed"]]
]];

(* testAssertEqualsThrow *) AppendTo[nonFrameworkTests, Hold[
With[{i = 2},
 If[MatchQ[
     Catch[AssertEquals[1, i], "AssertEquals"], 
     _[HoldComplete[AssertEquals[1, i]], 2]], 
   Null,
   throwSomething["testAssertEqualsThrow failed"]]
]]];

(* testAssertEqualsUnevaluated *) AppendTo[nonFrameworkTests, Hold[
Module[{result, f, i = 0},
 f[a_] /; (i += a; False) := Throw["This shouldn't evaluate"];
 result = Catch[AssertEquals[Unevaluated@f[2], f[3]], "AssertEquals"];
 If[MatchQ[result, Unevaluated[_[HoldComplete[AssertEquals[f[2], f[3]]], f[3]]]], 
    Null, 
    throwSomething["testAssertEqualsUnevaluated failed, result is not the expected exception"]];
 If[3 != i, throwSomething["testAssertEqualsUnevaluated failed, i != 3"]];
]]];


(* ::Subsection::Closed:: *)
(*Start of framework*)


(* Start with running non-framework tests, if any fail an uncaught throw ends execution.
   Otherwise report 0 failed non-framework tests and continue with framework tests. *)
TestEMUnitPackage[] := With[{string = 
 "Running non-framework tests\n" <>
 (ReleaseHold /@ nonFrameworkTests; ToString@Length@nonFrameworkTests) <> 
 " run, 0 failed\n" <>  
 "Running framework tests\n"},
 (* Prepend string to the regular summary given by RunTest without interfering with 
    the rest of its reporting. *)
 MapAt[string <> # &, RunTest[frameworkTests], {1, 2}]]


ClearAll[frameworkTests];


AddTest[frameworkTests, "Set Up", 
 ClearAll[mytests, anotherSuite, aThirdSuite]];
 
AddTest[frameworkTests, "Tear Down",  
 ClearAll[mytests, anotherSuite, aThirdSuite]];


(* ::Subsection:: *)
(*Test Asserts*)


AddSuite[frameworkTests, assertTests];


(* ::Subsubsection::Closed:: *)
(*Test AssertMatch*)


AddSuite[assertTests, assertMatchTests];


AddTest[assertMatchTests, "testSuccess",
  AssertEquals[Null, Catch[AssertMatch[
    {{_String, {_String | _Integer, _List}}, _String | _Integer}, 
    {{"a", {"b", {}}}, 1}], "AssertMatch"]];
  (* Using positive examples from MatchQ documentation *)
  AssertEquals[Null, Catch[AssertMatch[
    _Integer, 12345], "AssertMatch"]];
  AssertEquals[Null, Catch[AssertMatch[
    Plus[_, __], Expand[x(1 + 2 x + 3 x^2)]], "AssertMatch"]];
  AssertEquals[Null, Catch[AssertMatch[
    0, Simplify[1 + 1/GoldenRatio - GoldenRatio]], "AssertMatch"]];
  AssertEquals[Null, Catch[AssertMatch[
    _Association, <|a->1, b->2|>], "AssertMatch"]];
  AssertEquals[Null, Catch[AssertMatch[
    <|a->_|>, <|a->1|>], "AssertMatch"]];
  AssertEquals[Null, Catch[AssertMatch[
    <|_->1|>, <|a->1|>], "AssertMatch"]];
  AssertEquals[Null, Catch[AssertMatch[
    <|a->x_/;StringQ[x]|>, <|a->"foo"|>], "AssertMatch"]];
  AssertEquals[Null, Catch[AssertMatch[
    <|a->Verbatim[_]|>, <|a->_|>], "AssertMatch"]];
 ];


AddTest[assertMatchTests, "testFailed", Module[{result},
  AssertEquals[
      pAssertException[HoldComplete[AssertMatch[_Symbol, 1]], 1],
      Catch[AssertMatch[_Symbol, 1], "AssertMatch"]];
  (* Using negative examples from MatchQ documentation *)
  AssertEquals[
      pAssertException[
        HoldComplete[AssertMatch[_+__, (x-1)(1+2 x+3 x^2)]], (-1+x)(1+2 x+3 x^2)],
      Catch[AssertMatch[Plus[_, __], (x-1)(1 + 2 x + 3 x^2)], "AssertMatch"]];
  AssertEquals[
      pAssertException[
        HoldComplete[AssertMatch[0, 1 + 1/GoldenRatio - GoldenRatio]], 1 + 1/GoldenRatio - GoldenRatio],
      Catch[AssertMatch[0, 1 + 1/GoldenRatio - GoldenRatio], "AssertMatch"]];
 ]];


AddTest[assertMatchTests, "testFailedEvaluatesOnce", Module[{i = 0, result},
  result = Catch[AssertMatch[0, ++i], "AssertMatch"];
  AssertEquals[1, i];
  AssertEquals[
    pAssertException[HoldComplete[AssertMatch[0, ++i]], 1],
    result];
  AssertEquals[1, i];
]];


(* ::Subsubsection::Closed:: *)
(*Test AssertTrue*)


AddSuite[assertTests, assertTrueTests];


AddTest[assertTrueTests, "testSuccess", 
 AssertEquals[Null, Catch[AssertTrue[1 == 1], "AssertTrue"]];
 AssertEquals[Null, Catch[AssertTrue[2 > 1], "AssertTrue"]];
 AssertEquals[Null, Catch[AssertTrue[NumericQ@0.2], "AssertTrue"]];
];


AddTest[assertTrueTests, "testFailure", 
 AssertEquals[pAssertException[HoldComplete[AssertTrue[1 < 0]], False],
  Catch[AssertTrue[1 < 0], "AssertTrue"]];
];


AddTest[assertTrueTests, "testUnevaluating",
 Module[{a, result}, ClearAll[a]; 
  result = Catch[AssertTrue[a], "AssertTrue"]; 
  AssertMatch[_[HoldComplete[AssertTrue[_]], _], result];
 ]];


(* ::Subsubsection::Closed:: *)
(*Test AssertEqualsN*)


AddSuite[assertTests, assertEqualsNTests];


AddTest[assertEqualsNTests, "testSameQNExact",
 AssertTrue[pSameQN[1, 1, 0.1]];
 AssertTrue[pSameQN["a", "a", 0.1]];
 AssertTrue[pSameQN[{"a"}, {"a"}, 0.1]];
 AssertTrue[pSameQN[{"a", 1, 2}, {"a", 1, 2}, 0.1]];
 AssertTrue[pSameQN[{"a", {1}, 2}, {"a", {1}, 2}, 0.1]];
 AssertTrue[pSameQN[{"a", b[1], 2}, {"a", b[1], 2}, 0.1]];
 
 AssertTrue[!pSameQN[1, 2, 0.1]];
 AssertTrue[!pSameQN["a", "b", 0.1]];
 AssertTrue[!pSameQN[{"a"}, {"b"}, 0.1]];
 AssertTrue[!pSameQN[{"a", b[1], 2}, {"a", b[2], 2}, 0.1]];
 
 AssertTrue[!pSameQN[{"a", 1, 2}, {"a", {1}, 2}, 0.1]];
 AssertTrue[!pSameQN[{"a", {1}, 2}, {"a", {{1}}, 2}, 0.1]];
 AssertTrue[!pSameQN[{"a", b[1], 2}, {"a", a[1], 2}, 0.1]];
]


AddTest[assertEqualsNTests, "testSameQNApproximate",
 AssertTrue[pSameQN[1, 1.01, 0.1]];
 AssertTrue[pSameQN[{"a", 1, 2}, {"a", 1, 2.01}, 0.1]];
 AssertTrue[pSameQN[{"a", {1}, 2}, {"a", {1.01}, 2}, 0.1]];
 AssertTrue[pSameQN[{"a", b[1], 2}, {"a", b[1.01], 2}, 0.1]];
]


AddTest[assertEqualsNTests, "testSameQNAssociation",
 AssertTrue[pSameQN[<|1 -> "a", 2 -> 1, 3 -> 2|>, <|1 -> "a", 2 -> 1.01, 3 -> 2|>, 0.1]];
 AssertTrue[pSameQN[<|1 -> "a", 2 -> 1, 3 -> 2|>, <|1 -> "a", 2.01 -> 1, 3 -> 2|>, 0.1]];
 AssertTrue[pSameQN[<|1.01 -> "a", 2 -> 1, 3.01 -> 2|>, <|1 -> "a", 2 -> 1, 3 -> 2.01|>, 0.1]];
 AssertTrue[pSameQN[<|1.01 -> "a", 2 -> {1}, 3.01 -> 2|>, <|1 -> "a", 2 -> {1}, 3 -> 2.01|>, 0.1]];
 AssertTrue[pSameQN[<|1.01 -> "a", 2 -> <|1.01 -> 1|>, 3.01 -> 2|>, <|1 -> "a", 2 -> <|1 -> 1|>, 3 -> 2.01|>, 0.1]];
 AssertTrue[pSameQN[<|1.01 -> "a", 2 -> <|1 -> 1|>, 3.01 -> 2|>, <|1 -> "a", 2 -> <|1 -> 1.01|>, 3 -> 2.01|>, 0.1]];
]


AddTest[assertEqualsNTests, "testSameQNisNotMatchQN",
 AssertTrue[!pSameQN[1, _, 0.1]];
 AssertTrue[!pSameQN[{"a", 1, 2}, {"a", _, 2}, 0.1]];
 AssertTrue[!pSameQN[{"a", _, 2}, {"a", 1, 2}, 0.1]];
 AssertTrue[!pSameQN[{"a", 1, 2}, {"a", 2 | 1, 2}, 0.1]];
 AssertTrue[!pSameQN[{"a", {1}, 2}, {"a", {_}, 2}, 0.1]];
 AssertTrue[!pSameQN[{"a", b[1], 2}, {"a", b[_], 2}, 0.1]];
]


AddTest[assertEqualsNTests, "testSuccessExact",
 AssertEquals[Null, Catch[AssertEqualsN[1, 1], "AssertEqualsN"]]
];

AddTest[assertEqualsNTests, "testSuccessDefaultTolerance",
 AssertEquals[Null, 
  Catch[AssertEqualsN[1, 1 + 0.1*Tolerance /. Options[AssertEqualsN]], "AssertEqualsN"]]
];

AddTest[assertEqualsNTests, "testListSuccess",
 AssertEquals[Null, 
  Catch[AssertEqualsN[{1, 1}, {1, 1} + 0.1*Tolerance /. Options[AssertEqualsN]], "AssertEqualsN"]]
];

AddTest[assertEqualsNTests, "testSuccessOnExactTolerance",
 AssertEquals[Null, Catch[AssertEqualsN[1, 2, Tolerance -> 1], "AssertEqualsN"]]
];

AddTest[assertEqualsNTests, "testSuccessLargeTolerance",
 AssertEquals[Null, Catch[AssertEqualsN[1, 5.1, Tolerance -> 4.5], "AssertEqualsN"]]
];

AddTest[assertEqualsNTests, "testNonNumericTolerance",
 AssertMessage[eMUnitMessages::nonNumericTolerance, 
  Catch[AssertEqualsN[1, 5.1, Tolerance -> "string"], "AssertEqualsN"]]
];

AddTest[assertEqualsNTests, "testThrow", 
 With[{x = 1.7},
  AssertEquals[pAssertException[
                   HoldComplete[AssertEqualsN[1, x, Tolerance -> 0.5]], x], 
               Catch[AssertEqualsN[1, x, Tolerance -> 0.5], "AssertEqualsN"]]
 ]];
AddTest[assertEqualsNTests, "testListThrow",
 AssertEquals[pAssertException[
                   HoldComplete[AssertEqualsN[{{1}, 1}, {1, 1} + 0.1*Tolerance /. Options[AssertEqualsN], Tolerance -> 0.001]], {1.0001, 1.0001}], 
               Catch[AssertEqualsN[{{1}, 1}, {1, 1} + 0.1*Tolerance /. Options[AssertEqualsN]], "AssertEqualsN"]]
];
AddTest[assertEqualsNTests, "testListWithNonNumericEntriesThrow",
Module[{a},
  AssertEquals[pAssertException[
                HoldComplete[AssertEqualsN[{{a}, 1}, {1, 1} + 0.1*Tolerance /. Options[AssertEqualsN], Tolerance -> 0.001]], {1.0001, 1.0001}], 
               Catch[AssertEqualsN[{{a}, 1}, {1, 1} + 0.1*Tolerance /. Options[AssertEqualsN]], "AssertEqualsN"]]
 ]
];


(* ::Subsubsection::Closed:: *)
(*Test AssertMatchN*)


AddSuite[assertTests, assertMatchNTests];


With[{matchQN = pMatchQN},
AddTest[assertMatchNTests, "testMatchQNExact", With[{tol = 0},
 AssertTrue[matchQN[
   {{"a", {"b", {}}}, 1},
   {{_String, {_String | _Integer, _List}}, _String | _Integer}, tol]];
 AssertTrue[matchQN[
   {{"a", {"b", {1, {2}}}}, 1}, 
   {{_String, {_String | _Integer, _}}, _String | _Integer}, tol]];
 AssertTrue[matchQN[
   f[a, g[0.1, {a, {1, 2}}], g], _[a, g[__], _], tol]];
  (* Using examples from MatchQ documentation *)
 AssertTrue[matchQN[12345, _Integer, tol]];
 AssertTrue[Not@matchQN[x(1 + 2 x + 3 x^2), Plus[_, __], tol]];
 AssertTrue[matchQN[Expand[x(1 + 2 x + 3 x^2)], Plus[_, __], tol]];
    (* Not doing the "not explicitly 0" test as they are of course equal within even tol = 0. *)
 AssertTrue[matchQN[<|a->1, b->2|>, _Association, tol]];
 AssertTrue[matchQN[<|a->1|>, <|a->_|>, tol]];
 AssertTrue[matchQN[<|a->1|>, <|_->1|>, tol]];
 AssertTrue[matchQN[<|a->"foo"|>, <|a->x_/;StringQ[x]|>, tol]];
 AssertTrue[matchQN[<|a->_|>, <|a->Verbatim[_]|>, tol]];
]];

AddTest[assertMatchNTests, "testMatchQN", With[{tol = 0.2},
 AssertTrue[tol < 1]; (* Assumption used in tests below *)
 (* Start simple, one approximate numerical match to make sure the terms don't MatchQ exactly *)
 AssertTrue[matchQN[0., 0, tol]];
 AssertTrue[matchQN[0, 0., tol]];
 AssertTrue[matchQN[0, 0. | 1., tol]];
 AssertTrue[matchQN[0, 1. | 0., tol]];
 AssertTrue[matchQN[{0}, {1.} | {0.}, tol]];
 AssertTrue[matchQN[{"a", 1 + tol/2}, {_String, 1}, tol]];
 AssertTrue[matchQN[{1, 1 + tol/2}, {_Integer, 1}, tol]];
 AssertTrue[matchQN[{1, 1 + tol/2}, {_Integer | _String, 1}, tol]];
 AssertTrue[matchQN[{"a", "b", 1 + tol/2}, {__, 1}, tol]];
 Quiet@AssertNoMessage[matchQN[{1 + tol/2}, {___, 1}, tol]];
 AssertTrue[matchQN[{1.1}, {___, 1}, tol/2]];
 AssertTrue[matchQN[{1, 0}, {___, 5.1, ___}, 4.5]];
 AssertTrue[matchQN[{1, 0}, {___, 5.1, __}, 4.5]];
 AssertTrue[matchQN[{1, 0, 0}, {_, tol ..}, tol]];
 AssertTrue[matchQN[{1, 0, 0}, {_, tol ...}, tol]];
 AssertTrue[matchQN[{1}, {_, tol ...}, tol]];
 (* Using examples from MatchQ documentation *)
 AssertTrue[Not@matchQN[x(1 + 2 x + 3 x^2), Plus[_, _[2.1, _], __], tol]];
 AssertTrue[matchQN[Expand[x(1 + 2 x + 3 x^2)], Plus[_, _[2.1, _], __], tol]];
 AssertTrue[matchQN[<|a -> 1|>, <|_ -> 1.1|>, tol]];
 AssertTrue[matchQN[<|1 -> "foo"|>, <|1.1 -> x_/;StringQ[x]|>, tol]];
 AssertTrue[matchQN[<|1 -> _|>, <|1.1 -> Verbatim[_]|>, tol]];
 AssertTrue[Not@matchQN[<|a -> 1|>, <|_ -> 1.1+tol|>, tol]];
 AssertTrue[Not@matchQN[<|1 -> "foo"|>, <|1.1+tol -> x_/;StringQ[x]|>, tol]];
 AssertTrue[Not@matchQN[<|1 -> _|>, <|1.1+tol -> Verbatim[_]|>, tol]];
 (* Check orderless *)
 AssertTrue[matchQN[g[a + b, b, 0], g[x_ + y_, x_, tol], tol]];
 AssertTrue[matchQN[g[b + a, b, 0], g[x_ + y_, x_, tol], tol]];
 (* Some longer ones *)
 AssertTrue[matchQN[{{0, 0}}, {{0., 0.}} | {{1., 0.}}, 0.001]];
 AssertTrue[matchQN[{{0}, 2}, {{1 | 0. | 0.1}, 1.9 | 1.8}, 0.2]];
 AssertTrue[matchQN[
   {{"a", {"b", {}}}, 1},
   {{_String, {_String | _Integer, _List}}, 1.1}, tol]];
 AssertTrue[matchQN[
   {{"a", {"b", {1, {2}}}}, 1}, 
   {{_String, {_String | _Integer, {__, {2.1}}}}, _String | _Integer}, tol]];
 AssertTrue[matchQN[
   f[a, g[0.1, {a, {1, 2}}], g], _[a, g[__, {_, {1.1, 1.9}}], _], tol]];
 AssertTrue[matchQN[
   <|1 -> a, 2 -> g[0.1, {<|1 -> _|>, {1, 2}}], 3 -> g|>, 
   _[0.9 -> _, _ -> g[__, {<|1.1 -> Verbatim[_]|>, {1.2, 1.9}}], _], tol]];
]];
]


AddTest[assertMatchNTests, "testAssertMatchNSuccessExact",
 AssertEquals[Null, Catch[AssertMatchN[
    {{_String, {_String | _Integer, _List}}, _String | _Integer}, 
    {{"a", {"b", {}}}, 1}], "AssertMatchN"]];
 AssertEquals[Null, Catch[AssertMatchN[
    {{_String, {_String | _Integer, _}}, _String | _Integer}, 
    {{"a", {"b", {1, {2}}}}, 1}], "AssertMatchN"]];
 AssertEquals[Null, Catch[AssertMatchN[
    _[a, g[__], _], 
    f[a, g[0.1, {a, {1, 2}}], g]], "AssertMatchN"]];
];

AddTest[assertMatchNTests, "testAssertMatchNSuccessDefaultTolerance", 
With[{tol = Tolerance /. Options[AssertMatchN]},
 AssertEquals[Null, Catch[AssertMatchN[
   1, 1 + 0.1*tol], "AssertMatchN"]];
 AssertEquals[Null, Catch[AssertMatchN[
   <|1 -> a, 2 -> g[0.1, {<|1 -> _|>, {1, 2}}], 3 -> g|>, 
   <|1 -> a, 2 -> g[0.1, {<|1 -> _|>, {1, 2}}], 3 -> g|>], "AssertMatchN"]];
 AssertEquals[Null, Catch[AssertMatchN[
    <|_ -> 1|>, <|a -> 1 + 0.1*tol|>], "AssertMatchN"]];
]];

AddTest[assertMatchNTests, "testAssertMatchNSuccessOnExactTolerance",
 AssertEquals[Null, Catch[AssertMatchN[1, 2, Tolerance -> 1], "AssertMatchN"]];
 AssertEquals[Null, Catch[AssertMatchN[
   {{2}, _}, {{1}, {3}}, Tolerance -> 1], "AssertMatchN"]];
];

AddTest[assertMatchNTests, "testAssertMatchNSuccessLargeTolerance",
 AssertEquals[Null, Catch[AssertMatchN[
   {5.1, _}, {1, 0}, Tolerance -> 4.5], "AssertMatchN"]];
 AssertEquals[Null, Catch[AssertMatchN[
   {___, {5.1}, ___}, {{1}, {0}}, Tolerance -> 4.5], "AssertMatchN"]];
];

AddTest[assertMatchNTests, "testAssertMatchNNonNumericTolerance",
 AssertMessage[eMUnitMessages::nonNumericTolerance, 
  Catch[AssertMatchN[{2, 3, 4.}, 5.1, Tolerance -> "string"], "AssertMatchN"]]
];

AddTest[assertMatchNTests, "testAssertMatchNThrow", 
  AssertEquals[pAssertException[HoldComplete[
                 AssertMatchN[{1, 3}, 2, Tolerance -> 0.3]], 2], 
   Catch[AssertMatchN[{1, 3}, 2, Tolerance -> 0.3], "AssertMatchN"]];
  AssertEquals[pAssertException[HoldComplete[
                 AssertMatchN[{2, {1, "b"}, 3}, {1, "a"}, Tolerance -> 0.3]], {1, "a"}], 
   Catch[AssertMatchN[{2, {1, "b"}, 3}, {1, "a"}, Tolerance -> 0.3], "AssertMatchN"]];
 ];


(* ::Subsubsection::Closed:: *)
(*Test AssertMessage*)


AddSuite[assertTests, assertMessageTests];


AddTest[assertMessageTests, "Runs only once", Module[{mess, i = 0},
  mess::aMessage = "Message!";
  Catch[AssertMessage[mess::aMessage, i++], "AssertMessage"];
  AssertEquals[1, i];
]];


AddTest[assertMessageTests, "AssertNoMessage", Module[{mess, messenger, result},
  result = Catch[AssertNoMessage[1+1]; "noThrow", "AssertMessage"];
  AssertEquals["noThrow", result];
  mess::aMessage = "Message!";
  messenger := Message[mess::aMessage];
  Quiet[
    result = Catch[AssertNoMessage[messenger]; "noThrow", "AssertMessage"];
  , mess::aMessage];
  AssertEquals[pAssertException[
      HoldComplete[AssertNoMessage[messenger]], 
      {HoldForm[mess::aMessage]}]
   , result];
]];


AddTest[assertMessageTests, "Check correct message", Module[{mess, messenger, result},
  mess::aMessage = "Message!";
  messenger := Message[mess::aMessage];
  result = Catch[AssertMessage[mess::aMessage, messenger]; "noThrow", "AssertMessage"];
  AssertEquals["noThrow", result];
]];


AddTest[assertMessageTests, "testAssertMessageThrows", Module[{mess, result},
  mess::aMessage = "Message!";
  result = Catch[AssertMessage[mess::aMessage, "noMessage"], "AssertMessage"];
  AssertEquals[pAssertException[
      HoldComplete[AssertMessage[mess::aMessage, "noMessage"]],
      {}]
    , result];
]];


AddTest[assertMessageTests, "testAssertMessageQuiet", 
  Block[{$MessageList = {}}, 
   Quiet[
    Catch[AssertMessage[eMUnitMessages::suiteNotSet, ListTests[]], "AssertMessage"];
    AssertEquals[{}, $MessageList];
   , eMUnitMessages::suiteNotSet];
]];
AddTest[assertMessageTests, "testAssertMessageNotQuietOther",
  Block[{$MessageList = {}}, 
   Quiet[
    Catch[AssertMessage[eMUnitMessages::nonexistentTest, ListTests[]]
        , "AssertMessage"];
    AssertEquals[{HoldForm[eMUnitMessages::suiteNotSet]}, $MessageList];
   , eMUnitMessages::suiteNotSet];
]];


AddTest[assertMessageTests, "testAssertMessageIndepOfOtherMessages", Module[{mess, messenger, result},
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


AddSuite[frameworkTests, suiteTests];


(* ::Subsubsection::Closed:: *)
(*Test AddTest*)


AddSuite[suiteTests, addTestTests];


AddTest[addTestTests, "testAddAndListTests",
 AddTest[mytests, "aTest", 1 + 1];
 AddTest[mytests, "anotherTest", 1 + 2];
 AssertEquals[{"aTest", "anotherTest"}, ListTests[mytests]];
];

AddTest[addTestTests, "testAddTestDontEvaluateTheTest",
 Module[{i = 1},
  AddTest[mytests, "aTest", Do[i++, {10}]];
  AssertEquals[1, i];
 ]];

AddTest[addTestTests, "testAddingTwiceOverwrites",
 AddTest[mytests, "aTest", 1 + 1];
 AddTest[mytests, "aTest", 1 + 2];
 AssertEquals[{"aTest"}, ListTests[mytests]];
];


(* ::Subsubsection::Closed:: *)
(*Test Suite workings*)


AddSuite[suiteTests, suiteWorkingsTests];


AddTest[suiteWorkingsTests, "testAddSuite",
 AddSuite[mytests, anotherSuite];
 AddTest[anotherSuite, "aTest", 1+1];
 AssertEquals[{"anotherSuite"}, SymbolName/@ListTests[mytests]];
 AssertEquals[{"aTest"}, ListTests[ListTests[mytests][[1]]]]
];


AddTest[suiteWorkingsTests, "testRunSubSuite", Module[{i = 0},
  AddTest[mytests, "aTest", i++];
  AddSuite[mytests, anotherSuite];
  AddTest[anotherSuite, "anotherTest", i+=2];
  AddSuite[anotherSuite, aThirdSuite];
  AddTest[aThirdSuite, "aThirdTest", i+=3];
  RunTest[mytests];
  AssertEquals[6, i]
]];


AddTest[suiteWorkingsTests, "testSuiteNotSetMessage",
 AssertMessage[eMUnitMessages::suiteNotSet, AddTest["testWithoutSuite", 1+1]]
];


AddTest[suiteWorkingsTests, "testEndSuiteEmptyStack",
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
addRecheckCurrentSuiteTest = Function[{name, expr, form},
 AddTest[suiteWorkingsTests, "testCurrentSuiteRecheck" <> name,
  Module[{a = "notTouched (just checking)"},
   AddTest[mytests, "atest", a = expr];
   AssertMessage[eMUnitMessages::suiteNotSet, RunTest[mytests]];
   AssertEquals[Null, Unevaluated@a];(*holdform*)
   BeginSuite[anotherSuite];
   AssertNoMessage[RunTest[mytests]];
   EndSuite[];
   AssertMatch[form, a];
   AssertMessage[eMUnitMessages::suiteNotSet, RunTest[mytests]];
 ]], HoldAll];
addRecheckCurrentSuiteTest @@@ Unevaluated[{
  {"ListTests", ListTests[], anotherSuite["Unit Tests"]},
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
    Column[{_, "1 run in 0. s, 0 failed"}]},
  {"RunTestPattern", 
   (AddTest[anotherSuite, "anotherTest", 1+1]; RunTest["anotherTest"]),
    Column[{_, "1 run in 0. s, 0 failed"}]}
}];


(* ::Subsubsection::Closed:: *)
(*Test BeginSubsuite*)


AddTest[suiteTests, "testBeginSubsuite", Module[{subsuite},
 BeginSuite[mytests];
  BeginSubsuite[subsuite];
   AddTest["aTest", 1+1];
  EndSuite[];
 EndSuite[];
 AssertEquals[{subsuite}, ListTests[mytests]];
 AssertEquals[{"aTest"}, ListTests[ListTests[mytests][[1]]]]
]];


(* ::Subsubsection::Closed:: *)
(*Test DeleteTest*)


 AddTest[suiteTests, "testDeleteTest",
  AddTest[mytests, "aTest", a = 1];
  AddTest[mytests, "anotherTest", b = 1];
  DeleteTest[mytests, "aTest"];
  AssertEquals[{"anotherTest"}, ListTests[mytests]];
 ];


(* ::Subsection:: *)
(*Test Run*)


AddSuite[frameworkTests, runTests];


(* ::Subsubsection::Closed:: *)
(*Test RunTest*)


AddSuite[runTests, testRunTest];


AddTest[testRunTest, "testRunTest",
 Module[{i = 1},
  AddTest[mytests, "aTest", Do[i++, {10}]];
  AssertEquals[1, i];
  RunTest[mytests, "aTest"];
  AssertEquals[11, i];
 ]];

AddTest[testRunTest, "testRunTestWithPattern",
 Module[{i = 0},
   AddTest[mytests, "aTest", i++]; 
   AddTest[mytests, "anotherTest", i += 2];
   RunTest[mytests, "an" ~~ __ ~~ "Test"];
   AssertEquals[2, i];
   RunTest[mytests, "a" ~~ ___ ~~ "Test"];
   AssertEquals[5, i];
 ]];

AddTest[testRunTest, "testRunTestWithNonmatchingPattern", Module[{result},
 AssertMessage[eMUnitMessages::nonexistentTest, 
  result = RunTest[mytests, __ ~~ "nonmatchingPattern"]
 ];
 AssertEquals[
  Unevaluated@RunTest[mytests, __~~"nonmatchingPattern"], 
  result];
]];


 AddTest[testRunTest, "testRunTestOnSuite",
  Module[{a, b, c},
   AddTest[mytests, "test1", a = 1];
   AddTest[mytests, "test2", b = 2];
   AddTest[mytests, "test3", c = a + b];
   RunTest[mytests];
   AssertEquals[1, a];
   AssertEquals[2, b];
   AssertEquals[3, c];
  ]];


AddTest[testRunTest, "testRunTestOnParentWithStringPattern", Module[{i = 0},
  AddSuite[mytests, anotherSuite];
  AddTest[anotherSuite, "aTestNotExistingInParent", i++];
  AssertMessage[eMUnitMessages::nonexistentTest, 
    RunTest[mytests, __~~"NotExistingInParent"]];
  AssertEquals[0, i];
  RunTest[anotherSuite, __~~"NotExistingInParent"];
  AssertEquals[1, i];
]];


(* ::Subsubsection::Closed:: *)
(*Test Set Up*)


AddSuite[runTests, testSetUp];


AddTest[testSetUp, "testSetUp",
 mytests["isSetUp"] = False;
 AddTest[mytests, "Set Up", mytests["isSetUp"] = True];
 mytests["Set Up"];
 AssertTrue[mytests["isSetUp"]];
];

AddTest[testSetUp, "testRunTestRunsSetUp",
 mytests["isSetUp"] = False;
 AddTest[mytests, "Set Up", mytests["isSetUp"] = True];
 AddTest[mytests, "emptyTest", Null];
 RunTest[mytests];
 AssertTrue[mytests["isSetUp"]];
];

AddTest[testSetUp, "Hierarchical suites runs Set Up",
 mytests["isSetUp"] = False;
 AddSuite[mytests, anotherSuite];
 AddTest[mytests, "Set Up", mytests["isSetUp"] = True];
 AddTest[anotherSuite, "emptyTest", Null];
 RunTest[anotherSuite];
 AssertTrue[mytests["isSetUp"]];
];

AddTest[testSetUp, "Hierarchical suites runs Set Up 2 levels down",
 mytests["isSetUp"] = False;
 AddSuite[mytests, anotherSuite];
 AddSuite[anotherSuite, aThirdSuite];
 AddTest[mytests, "Set Up", mytests["isSetUp"] = True];
 AddTest[aThirdSuite, "emptyTest", Null];
 RunTest[aThirdSuite];
 AssertTrue[mytests["isSetUp"]];
];


(* ::Subsubsection::Closed:: *)
(*Test Tear Down*)


AddSuite[runTests, testTearDown];


AddTest[testTearDown, "testTearDown",
 Module[{isStillSetUp},
  isStillSetUp = True;
  AddTest[mytests, "Tear Down", Clear[isStillSetUp]];
  mytests["Tear Down"];
  AssertTrue[!ValueQ[isStillSetUp]];
 ]];


AddTest[testTearDown, "testRunTestRunsTearDown",
 Module[{isStillSetUp},
  AddTest[mytests, "Tear Down", Clear[isStillSetUp]];
  AddTest[mytests, "setsUp", isStillSetUp = True];
  RunTest[mytests, "setsUp"];
  AssertTrue[!ValueQ[isStillSetUp]];
 ]];


AddTest[testTearDown, "testRunTestRunsTearDownEvenAfterFailedTest",
 Module[{isStillSetUp},
  AddTest[mytests, "Tear Down", Clear[isStillSetUp]];
  AddTest[mytests, "setsUp", isStillSetUp = True; AssertTrue[False]];
  RunTest[mytests, "setsUp"];
  AssertTrue[!ValueQ[isStillSetUp]];
 ]];


AddTest[testTearDown, "Hierarchical suites runs Tear Down",
 Module[{isStillSetUp},
  AddSuite[mytests, anotherSuite];
  AddTest[mytests, "Tear Down", Clear[isStillSetUp]];
  AddTest[anotherSuite, "setsUp", isStillSetUp = True];
  RunTest[anotherSuite];
  AssertTrue[!ValueQ[isStillSetUp]];
 ]];


AddTest[testTearDown, "Hierarchical suites runs Tear Down 2 levels down",
 Module[{isStillSetUp},
  AddSuite[mytests, anotherSuite];
  AddSuite[anotherSuite, aThirdSuite];
  AddTest[mytests, "Tear Down", Clear[isStillSetUp]];
  AddTest[aThirdSuite, "setsUp", isStillSetUp = True];
  RunTest[aThirdSuite];
  AssertTrue[!ValueQ[isStillSetUp]];
 ]];


AddTest[testTearDown, "Hierarchical suites runs Tear Down 2 levels down after failed test",
 Module[{isStillSetUp},
  AddSuite[mytests, anotherSuite];
  AddSuite[anotherSuite, aThirdSuite];
  AddTest[mytests, "Tear Down", Clear[isStillSetUp]];
  AddTest[aThirdSuite, "setsUp", isStillSetUp = True; AssertTrue[False]];
  RunTest[aThirdSuite];
  AssertTrue[!ValueQ[isStillSetUp]];
 ]];


(* ::Subsubsection:: *)
(*Test formatTestResult*)


AddSuite[runTests, testFormatTestResults];


AddTest[testFormatTestResults, "Single success", 
  AddTest[mytests, "aTest", AssertEquals[1, 1]];
  AssertMatch[Column[{_Graphics, "1 run in 0. s, 0 failed"}], 
   RunTest[mytests, "aTest"]];
];


AddTest[testFormatTestResults, "Two successes", 
  AddTest[mytests, "aTest", AssertEquals[1, 1]];
  AddTest[mytests, "anotherTest", AssertEquals[1, 1]];
  AssertMatch[Column[{_Graphics, "2 run in 0. s, 0 failed"}], RunTest[mytests]]
];


AddTest[testFormatTestResults, "Single failure", 
  AddTest[mytests, "aTest", AssertTrue[False]];
  AssertMatch[
   Column[{_Graphics, 
    "1 run in 0. s, 1 failed", 
    "mytests aTest - Failed AssertTrue[False], gave False"}], 
   RunTest[mytests]];
];


AddTest[testFormatTestResults, "One success, one failure", 
 AddTest[mytests, "aTest", AssertEquals[1, 1]];
 AddTest[mytests, "anotherTest", AssertEquals[1, -1]];
 AssertMatch[
  Column[{_Graphics, 
    "2 run in 0. s, 1 failed", 
    "mytests anotherTest - Failed AssertEquals[1, -1], gave -1"}], 
  RunTest[mytests]];
];


AddTest[testFormatTestResults, "testFormatAssertMessageExpectedMessage", 
 AddTest[mytests, "aTest", AssertMessage[Drop::drop, Drop[{1}, 1]]];
 AssertMatch[
   Column[{_Graphics, 
      "1 run in 0. s, 1 failed", 
      "mytests aTest - Failed AssertMessage[Drop::drop, Drop[{1}, 1]], gave {}"}], 
   RunTest[mytests]];
];
AddTest[testFormatTestResults, "testFormatAssertNoMessage", 
 Module[{formattedResult, mess},
  mess::aMessage = "Message!";
  AddTest[mytests, "aTest", AssertNoMessage[Message[mess::aMessage]]];
  Quiet[formattedResult = RunTest[mytests], mess::aMessage];
  AssertMatch[
   Column[{_Graphics, 
      "1 run in 0. s, 1 failed", 
      "mytests aTest - Failed AssertNoMessage[Message[" <> ToString@mess <>"::aMessage]],\
 gave {HoldForm[" <> ToString@mess <>"::aMessage]}"}], 
   formattedResult];
]];


AddTest[testFormatTestResults, "testFormatTestResultSubsuites",
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
    SymbolName@level1 <> " " <> SymbolName@level2 <> 
      " test2.2 - Failed AssertEquals[1, -1], gave -1",
    SymbolName@level1 <> " " <> SymbolName@level2 <> " " <> SymbolName@level3 <> 
      " test3.1 - Failed AssertTrue[1 < 0], gave False"}], 
   formattedResult];
]];


AddTest[testFormatTestResults, "testFormatHierarchicalTestResultSubsuites",
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
  formattedResult = RunTest[level1, ReportMethod -> "Hierarchical"];
  AssertEquals[15, i];
  AssertMatch[
   Column[{_Graphics, 
    "5 run in 0. s, 2 failed", 
    " test1.1 0.00", 
    "-" <> ToString@level2 <> " 0.00\n" <> 
    "     test2.1 0.00\n" <> 
    "    -test2.2 0.00\n" <> 
    "    -" <> ToString@level3 <> " 0.00\n" <> 
    "        -test3.1 0.00\n" <> 
    "         test3.2 0.00"
    }], 
   formattedResult];
]];


AddTest[testFormatTestResults, "Check Set Up and Tear Down not counted as tests",
 Module[{i = 0},
  AddTest[mytests, "Set Up", i++];
  AddTest[mytests, "Tear Down", i++];
  AddTest[mytests, "aTest", i++];
  AddTest[mytests, "anotherTest", i++];
  AssertMatch[Column[{_Graphics, "2 run in 0. s, 0 failed"}], 
   RunTest[mytests]];
  AssertEquals[6, i];
]];


(* ::Subsection:: *)
(*Tail*)
