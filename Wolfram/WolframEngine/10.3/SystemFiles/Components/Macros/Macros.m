Package["Macros`"]


PackageExport["SetUsage"]

SetAttributes[SetUsage, HoldFirst];

SetUsage[msg_Symbol, string_String] := 
	Set[MessageName[msg, "usage"], tolinearsyntax[string]];
	
SetUsage[msg_Symbol, strings__String] :=
	Set[MessageName[msg, "usage"], StringJoin @ Riffle[tolinearsyntax /@ {strings}, "\n"]];

slotSubscriptForm[sym_, i_] := 
	"\!\(\*StyleBox[SubscriptBox[\"\\\"" <> sym <> "\\\"\", StyleBox[\"\\\"" <> i <> "\\\"\", \"TR\"]], \"TI\"]\)\[VeryThinSpace]";

slotForm[sym_] := 
	"\!\(\*StyleBox[\"\\\"" <> sym <> "\\\"\", \"TI\"]\)\[VeryThinSpace]"

$ellipsis = "\[Ellipsis]";

tolinearsyntax[string_] := StringReplace[string, {
		"->" -> "\[Rule]",
		"'" -> "\"",
		slot:LetterCharacter.. ~~ "$$" :> slotForm[slot] <> $ellipsis,
		slot:LetterCharacter.. ~~ "$" ~~ sub:DigitCharacter|LetterCharacter :> slotSubscriptForm[slot, sub],
		slot:LetterCharacter.. ~~ "$(" ~~ Shortest[sub___] ~~ ")" :> slotSubscriptForm[slot, sub],
		slot:LetterCharacter.. ~~ "$" :> slotForm[slot],
		"$$" -> $ellipsis
	}]

SetUsage[SetUsage, 
	"SetUsage[f$, \"usage$\"] attaches a usage message to f$ in which the special symbols $ and $\[InvisibleSpace]$ can be used as they are in DocuTools.",
	"SetUsage[f$, \"usage$1\", \"usage$2\", $$] concatenates several usage messages to f$, each of which will show as one line when displayed with ?."
];

PackageExport["InactivateFull"]

SetAttributes[InactivateFull,HoldFirst];
InactivateFull[x_] := 
	Inactivate @@ ReplaceAll[Hold[x], 
		s_Symbol ? ValueQ :> Inactive[s]
	];


PackageExport["InactiveSymbol"]

InactiveSymbol[str_String] :=
	ToExpression[str, InputForm, Inactive];


PackageExport["DeclareMacro"]
PackageScope["$MacroHead"]

DeclareMacro[sym_Symbol, func_] := 
	TagSetDelayed[sym,
		Verbatim[SetDelayed][lhs_, Verbatim[sym][args___]],
		Activate[
			Inactive[SetDelayed][
				Inactive[lhs], 
				ParseInactives[
					Block[{$MacroHead = gethead[lhs]},
						func @@ InactivateFull[{args}]]
				]
			]
		]
	];


PackageExport["ParseInactives"]

ParseInactives[expr_] :=
	ReplaceAll[
		expr,
		s_Symbol /; StringMatchQ[SymbolName[Unevaluated[s]], "$" ~~ ___ ~~ "$"] :> 
			ToExpression[StringTake[SymbolName[s], {2, -2}], InputForm, Inactive]
	];
	
	
SetAttributes[gethead, HoldAll];
gethead[head_[___]] := gethead[head];
gethead[head_Symbol] := head;
gethead[_] := None;