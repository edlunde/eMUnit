(* ::Package:: *)

(* ::Section:: *)
(*Tests*)


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
  eMUnit`Private`throwAssertException["AssertEquals", text, ""]


(* testThrowSomething *) AppendTo[nonFrameworkTests, Hold[
Module[{result},
 result = Catch[throwSomething["testThrowSomething throw"], 
   "AssertEquals"];
 If[Not[result === 
        eMUnit`Private`assertException[HoldComplete["testThrowSomething throw"], ""]], 
  Throw["!!! testThrowSomething failed, gave " <> ToString@result <> 
    "\nCannot trust tests of AssertEquals.", "throwSomethingFailed"]];
]]];


(* testAssertEqualsSuccess *) AppendTo[nonFrameworkTests, Hold[
If[Not[Catch[AssertEquals[1, 1], "AssertEquals"] === Null], 
 throwSomething["testAssertEqualsSuccess failed"]]
]];

(* testAssertEqualsThrow *) AppendTo[nonFrameworkTests, Hold[
With[{i = 2},
 If[Catch[AssertEquals[1, i], "AssertEquals"] === 
     eMUnit`Private`assertException[HoldComplete[AssertEquals[1, i]], 2], 
   Null,
   throwSomething["testAssertEqualsThrow failed"]]
]]];

(* testAssertEqualsUnevaluated *) AppendTo[nonFrameworkTests, Hold[
Module[{result, f, i = 0},
 f[a_] /; (i += a; False) := Throw["This shouldn't evaluate"];
 result = Catch[AssertEquals[Unevaluated@f[2], f[3]], "AssertEquals"];
 If[result === Unevaluated@eMUnit`Private`assertException[
     HoldComplete[AssertEquals[f[2], f[3]]], 
     f[3]], 
    Null, 
    throwSomething["testAssertEqualsUnevaluated failed, result is not the expected exception"]];
 If[3 != i, throwSomething["testAssertEqualsUnevaluated failed, i != 3"]];
]]];


(* ::Subsection:: *)
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
BeginSuite[frameworkTests];


AddTest["Set Up", 
 ClearAll[mytests, anotherSuite]; 
 BeginSuite[mytests];];
 
AddTest["Tear Down", 
 EndSuite[]; 
 ClearAll[mytests, anotherSuite]];


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
      eMUnit`Private`assertException[HoldComplete[AssertMatch[_Symbol, 1]], 1],
      Catch[AssertMatch[_Symbol, 1], "AssertMatch"]];
  (* Using negative examples from MatchQ documentation *)
  AssertEquals[
      eMUnit`Private`assertException[
        HoldComplete[AssertMatch[_+__, (x-1)(1+2 x+3 x^2)]], (-1+x)(1+2 x+3 x^2)],
      Catch[AssertMatch[Plus[_, __], (x-1)(1 + 2 x + 3 x^2)], "AssertMatch"]];
  AssertEquals[
      eMUnit`Private`assertException[
        HoldComplete[AssertMatch[0, 1 + 1/GoldenRatio - GoldenRatio]], 1 + 1/GoldenRatio - GoldenRatio],
      Catch[AssertMatch[0, 1 + 1/GoldenRatio - GoldenRatio], "AssertMatch"]];
 ]];


AddTest[assertMatchTests, "testFailedEvaluatesOnce", Module[{i = 0, result},
  result = Catch[AssertMatch[0, ++i], "AssertMatch"];
  AssertEquals[1, i];
  AssertEquals[
    eMUnit`Private`assertException[HoldComplete[AssertMatch[0, ++i]], 1],
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
 AssertEquals[eMUnit`Private`assertException[HoldComplete[AssertTrue[1 < 0]], False],
  Catch[AssertTrue[1 < 0], "AssertTrue"]];
];


AddTest[assertTrueTests, "testUnevaluating",
 Module[{a, result}, ClearAll[a]; 
  result = Catch[AssertTrue[a], "AssertTrue"]; 
  AssertMatch[_[HoldComplete[AssertTrue[_]], _], result];
 ]];


(* ::Subsubsection:: *)
(*Test AssertEqualsN*)


AddSuite[assertTests, assertEqualsNTests];


With[{sameQN = eMUnit`Private`sameQN},
AddTest[assertEqualsNTests, "testSameQNExact",
 AssertTrue[sameQN[1, 1, 0.1]];
 AssertTrue[sameQN["a", "a", 0.1]];
 AssertTrue[sameQN[{"a"}, {"a"}, 0.1]];
 AssertTrue[sameQN[{"a", 1, 2}, {"a", 1, 2}, 0.1]];
 AssertTrue[sameQN[{"a", {1}, 2}, {"a", {1}, 2}, 0.1]];
 AssertTrue[sameQN[{"a", b[1], 2}, {"a", b[1], 2}, 0.1]];
 
 AssertTrue[!sameQN[1, 2, 0.1]];
 AssertTrue[!sameQN["a", "b", 0.1]];
 AssertTrue[!sameQN[{"a"}, {"b"}, 0.1]];
 AssertTrue[!sameQN[{"a", b[1], 2}, {"a", b[2], 2}, 0.1]];
 
 AssertTrue[!sameQN[{"a", 1, 2}, {"a", {1}, 2}, 0.1]];
 AssertTrue[!sameQN[{"a", {1}, 2}, {"a", {{1}}, 2}, 0.1]];
 AssertTrue[!sameQN[{"a", b[1], 2}, {"a", a[1], 2}, 0.1]];
];

AddTest[assertEqualsNTests, "testSameQNApproximate",
 AssertTrue[sameQN[1, 1.01, 0.1]];
 AssertTrue[sameQN[{"a", 1, 2}, {"a", 1, 2.01}, 0.1]];
 AssertTrue[sameQN[{"a", {1}, 2}, {"a", {1.01}, 2}, 0.1]];
 AssertTrue[sameQN[{"a", b[1], 2}, {"a", b[1.01], 2}, 0.1]];
];

AddTest[assertEqualsNTests, "testSameQNAssociation",
 AssertTrue[sameQN[<|1 -> "a", 2 -> 1, 3 -> 2|>, <|1 -> "a", 2 -> 1.01, 3 -> 2|>, 0.1]];
 AssertTrue[sameQN[<|1 -> "a", 2 -> 1, 3 -> 2|>, <|1 -> "a", 2.01 -> 1, 3 -> 2|>, 0.1]];
 AssertTrue[sameQN[<|1.01 -> "a", 2 -> 1, 3.01 -> 2|>, <|1 -> "a", 2 -> 1, 3 -> 2.01|>, 0.1]];
 AssertTrue[sameQN[<|1.01 -> "a", 2 -> {1}, 3.01 -> 2|>, <|1 -> "a", 2 -> {1}, 3 -> 2.01|>, 0.1]];
 AssertTrue[sameQN[<|1.01 -> "a", 2 -> <|1.01 -> 1|>, 3.01 -> 2|>, <|1 -> "a", 2 -> <|1 -> 1|>, 3 -> 2.01|>, 0.1]];
 AssertTrue[sameQN[<|1.01 -> "a", 2 -> <|1 -> 1|>, 3.01 -> 2|>, <|1 -> "a", 2 -> <|1 -> 1.01|>, 3 -> 2.01|>, 0.1]];
];

AddTest[assertEqualsNTests, "testSameQNisNotMatchQN",
 AssertTrue[!sameQN[1, _, 0.1]];
 AssertTrue[!sameQN[{"a", 1, 2}, {"a", _, 2}, 0.1]];
 AssertTrue[!sameQN[{"a", _, 2}, {"a", 1, 2}, 0.1]];
 AssertTrue[!sameQN[{"a", 1, 2}, {"a", 2 | 1, 2}, 0.1]];
 AssertTrue[!sameQN[{"a", {1}, 2}, {"a", {_}, 2}, 0.1]];
 AssertTrue[!sameQN[{"a", b[1], 2}, {"a", b[_], 2}, 0.1]];
];
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
  AssertEquals[eMUnit`Private`assertException[
                   HoldComplete[AssertEqualsN[1, x, Tolerance -> 0.5]], x], 
               Catch[AssertEqualsN[1, x, Tolerance -> 0.5], "AssertEqualsN"]]
 ]];
AddTest[assertEqualsNTests, "testListThrow",
 AssertEquals[eMUnit`Private`assertException[
                   HoldComplete[AssertEqualsN[{{1}, 1}, {1, 1} + 0.1*Tolerance /. Options[AssertEqualsN], Tolerance -> 0.001]], {1.0001, 1.0001}], 
               Catch[AssertEqualsN[{{1}, 1}, {1, 1} + 0.1*Tolerance /. Options[AssertEqualsN]], "AssertEqualsN"]]
];
AddTest[assertEqualsNTests, "testListWithNonNumericEntriesThrow",
Module[{a},
  AssertEquals[eMUnit`Private`assertException[
                HoldComplete[AssertEqualsN[{{a}, 1}, {1, 1} + 0.1*Tolerance /. Options[AssertEqualsN], Tolerance -> 0.001]], {1.0001, 1.0001}], 
               Catch[AssertEqualsN[{{a}, 1}, {1, 1} + 0.1*Tolerance /. Options[AssertEqualsN]], "AssertEqualsN"]]
 ]
];


(* ::Subsubsection::Closed:: *)
(*Test AssertMatchN*)


With[{matchQN = eMUnit`Private`matchQN},
AddTest["testMatchQNExact", With[{tol = 0},
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

AddTest["testMatchQN", With[{tol = 0.2},
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


AddTest["testAssertMatchNSuccessExact",
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

AddTest["testAssertMatchNSuccessDefaultTolerance", 
With[{tol = Tolerance /. Options[AssertMatchN]},
 AssertEquals[Null, Catch[AssertMatchN[
   1, 1 + 0.1*tol], "AssertMatchN"]];
 AssertEquals[Null, Catch[AssertMatchN[
   <|1 -> a, 2 -> g[0.1, {<|1 -> _|>, {1, 2}}], 3 -> g|>, 
   <|1 -> a, 2 -> g[0.1, {<|1 -> _|>, {1, 2}}], 3 -> g|>], "AssertMatchN"]];
 AssertEquals[Null, Catch[AssertMatchN[
    <|_ -> 1|>, <|a -> 1 + 0.1*tol|>], "AssertMatchN"]];
]];

AddTest["testAssertMatchNSuccessOnExactTolerance",
 AssertEquals[Null, Catch[AssertMatchN[1, 2, Tolerance -> 1], "AssertMatchN"]];
 AssertEquals[Null, Catch[AssertMatchN[
   {{2}, _}, {{1}, {3}}, Tolerance -> 1], "AssertMatchN"]];
];

AddTest["testAssertMatchNSuccessLargeTolerance",
 AssertEquals[Null, Catch[AssertMatchN[
   {5.1, _}, {1, 0}, Tolerance -> 4.5], "AssertMatchN"]];
 AssertEquals[Null, Catch[AssertMatchN[
   {___, {5.1}, ___}, {{1}, {0}}, Tolerance -> 4.5], "AssertMatchN"]];
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
