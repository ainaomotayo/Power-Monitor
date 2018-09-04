Package["Macros`"]


Panic[] := Panic[Stack[]];

Panic[value_] := (
	If[$FrontEnd === Null, 
		Print[value], 
		With[{boxes = Block[{BoxForm`$UseTextFormattingWhenEvaluating = True}, ToBoxes @ Short[value, 0.5]]},
			CellPrint[Cell[boxes, "Output", Background -> LightRed]]
		]
	];
	Clear["System`$PanicValue"];
	With[{s = Symbol["System`$PanicValue"]}, Set[s, value]];
	Abort[];
);


PackageExport["FailOnMessages"]

SetAttributes[FailOnMessages, HoldAll];
FailOnMessages[expr_] := Block[
	{Message, capture = True},
	Message[msg_, args___] /; capture && (msg =!= $Off[]) := Block[{capture = False}, ThrowFailure[msg, args]];
	expr
];


PackageExport["$FailureScope"]
PackageExport["$FailureScopeStack"]

$FailureScope = None;
$FailureScopeStack = {};


PackageExport["CheckFailure"]

CheckFailure[expr_] := If[failedQ[expr], ThrowFailure[expr], expr]; 
	
CheckFailure[expr_, f_] := If[failedQ[expr], f @ expr, expr];


PackageExport["CatchFailure"]

SetAttributes[CatchFailure, HoldAll];
SetAttributes[chain, HoldRest];

CatchFailure::badscope = "`` is not a valid scope.";

CatchFailure[body_] := 
	Catch[FailOnMessages @  ToFailure @ body, CatchFailure];
	
CatchFailure[body_, f___, l:Except[_Rule]] := 
	Catch[FailOnMessages @ chain[body, f, l], CatchFailure];

CatchFailure[body___, "Scope" -> scope_] := 
	Switch[scope,
		Inherited, 
			CatchFailure[body],
		_Symbol, 
			Block[
				{$FailureScope = scope, $FailureScopeStack = Append[$FailureScopeStack, scope]}, 
				CatchFailure[body]
			],
		_, Message[CatchFailure::badscope, scope]; CatchFailure[body];
	];

$toplevel = True;
CatchFailure /: Verbatim[SetDelayed][lhs:(head_Symbol[___]), CatchFailure[rhs___]] /; $toplevel := 
	Block[{$toplevel = False}, lhs := Block[
		{$FailureScope = head, $FailureScopeStack = Append[$FailureScopeStack, head]}, 
		CatchFailure[rhs]
	]];

CatchFailure /: Verbatim[SetDelayed][lhs:(head_Symbol[___]), CatchFailure[rhs___, "Scope" -> Inherited]] /; $toplevel := 
	Block[{$toplevel = False}, lhs := CatchFailure[rhs]];

(* copied out of GU because I don't want to introduce a dependancy *)
$failedSymbols = $Failed | $Aborted | $TimedOut | _Failure;
failedQ[expr_] := failedQ[expr, 0];
failedQ[expr_, depth:(_Integer | Infinity)] := !FreeQ[expr, $failedSymbols, {0, depth}];


chain[expr_, func_, rest___] := 
	If[failedQ[expr], ToFailure @ expr, 
		Block[{f = func},
			If[failedQ[f], ToFailure @ f,
				chain[f[expr], rest]
			]
		]
	];
	
chain[expr_] := ToFailure[expr];


PackageExport["ThrowFailure"]

SetAttributes[ThrowFailure, HoldFirst];
SetAttributes[iFail, HoldAll];

ThrowFailure::malformed = "Invalid call to ThrowFailure with arguments ``";

ThrowFailure[x___] := (Message[ThrowFailure::malformed, HoldForm[ThrowFailure[x]]]; Abort[]);

ThrowFailure[sym_Symbol /; MatchQ[OwnValues[sym], {_ :> Failure[_]}]] := ThrowFailure[Evaluate[sym]];

ThrowFailure[f_Failure] := Throw[f, CatchFailure];

ThrowFailure[] := ThrowFailure["internal"];

ThrowFailure[msg_String, args___] := 
	If[StringFreeQ[msg, " "] && MemberQ[MessageNames[$FailureScope], msg],
		ThrowFailure[MessageName[Inherited, msg], args],
		iFail[$FailureScope, msg, args]
	];

ThrowFailure[msg:HoldPattern[MessageName[sym_, _]], args___] := 
	iFail[sym, msg, args];

ThrowFailure[MessageName[Inherited, name_], args___] := 
	With[{scope = Replace[$FailureScope, None -> General]},
		ThrowFailure[MessageName[scope, name], args]
	];

iFail[tag_, msg_, args___] := 
	With[{fo = Failure[tag, <|
			"MessageParameters" :> {args}, 
			"MessageTemplate" :> msg, 
			"ScopeStack" -> $FailureScopeStack
		|>]},
		If[$FailureScope === None,
			Panic[fo]
		,
			Throw[fo, CatchFailure]
		]
	];


PackageExport["VerifyTrue"]
PackageExport["VerifyFalse"]
PackageExport["Verify"]

SetAttributes[VerifyTrue, HoldAll];
SetAttributes[VerifyFalse, HoldAll];
SetAttributes[Verify, HoldAll];

VerifyTrue[value_, args___] :=  
	If[!TrueQ[value], vfail[VerifyTrue, value, args]];

VerifyFalse[value_, args___] := 
	If[TrueQ[value], vfail[VerifyFalse, value, args]];

Verify[test_, args___][expr_] := 
	If[TrueQ[test[expr]], expr, vfail[Verify[test], expr, args]];

vfail[type_, value_] := 
	vfail[type, value, General::intfailure, $FailureScope, ToUpperCase @ GeneralUtilities`Base36Hash[type]];

vfail[type_, value_, msg_String | msg_MessageName, args___] := ThrowFailure[msg, args, "Test" -> type, "Value" :> value];

General::intfailure = "An internal error occured while evaluating expression with head ``. Please contact technical support with the error code ``.";


SetAttributes[ToFailure, HoldAll];
ToFailure[x_] := x;
ToFailure[Message[MessageName[sym_Symbol, msg_String], args___]] := 
	Failure[sym, <|"MessageParameters" :> {args}, "MessageTemplate" :> MessageName[sym, msg]|>];
ToFailure[HoldPattern[Failure[_, meta_]]] /; $FailureScope =!= None := Failure[$FailureScope, meta];
ToFailure[f:($Failed|$TimedOut|$Aborted)] := Failure[f];


PackageExport["AbortFailure"]

SetAttributes[AbortFailure, HoldAll];
AbortFailure[expr___] := Block[{ThrowFailure = GeneralUtilities`AbortWithStack}, AbortFailure[expr]];


PackageExport["IgnoreFailure"]

SetAttributes[IgnoreFailure, HoldAll];
IgnoreFailure[expr_] := expr;
IgnoreFailure[expr_, a_, rest___] := IgnoreFailure[a[expr], rest];


PackageExport["MessageNames"]

MessageNames[sym_] := Messages[sym][[All, 1, 1, 2]];

General::internal = "An internal error occurred. Please contact technical support.";
