(* physical quantity variable formatting *)
makeTag[pq_String] := 
  Replace[StringReplace[pq, 
    RegularExpression["[[:upper:]]"] :> " " <> ToLowerCase["$0"]], 
   "" -> " unitless"]
   
SetAttributes[makeVariableBoxes, HoldAllComplete]
makeVariableBoxes[x_, fmt_] := Replace[HoldComplete[x],
  {
   HoldComplete[s_String] /; StringLength[s] == 1 :> 
    StyleBox["\"" <> s <> "\""],
   HoldComplete[s_String] :> "\"" <> s <> "\"",
   HoldComplete[h_[s_String, rest___]] /; StringLength[s] == 1 :>
        MakeBoxes[
     h[Style[s, StripOnInput -> True], rest], 
     fmt],
   HoldComplete[other_] :> MakeBoxes[other, fmt]
   }
  ]
	
QuantityVariable /: MakeBoxes[QuantityVariable[x_?(UnsameQ[#,None]&), pq_String], fmt_] := With[{t = makeTag[pq]}, 
 TemplateBox[{makeVariableBoxes[x, fmt], MakeBoxes[pq, fmt]}, 
  "QuantityVariable", 
  DisplayFunction -> (TooltipBox[
      StyleBox[#1, FontColor->GrayLevel[.4], 
       ShowStringCharacters -> False], 
      RowBox[{"Quantity variable:", t}], 
      TooltipStyle -> "TextStyling"] &)]]
     
QuantityVariable /: OutputForm[QuantityVariable[x_?(UnsameQ[#,None]&), ___]]:=x;

QuantityVariable /: MakeBoxes[QuantityVariable[pq_String], fmt_] := With[{t = makeTag[pq]}, 
 TemplateBox[{makeVariableBoxes[pq, fmt], MakeBoxes[pq, fmt]}, 
  "QuantityVariable", 
  DisplayFunction -> (TooltipBox[
      StyleBox[#1, FontColor->GrayLevel[.4], 
       ShowStringCharacters -> False], 
      RowBox[{"Quantity variable:", t}], 
      TooltipStyle -> "TextStyling"] &)]];

QuantityVariable[q_Quantity,___]/;TrueQ[$QuantityVariableQuantityIdentity]:=q

SetAttributes[{QuantityVariableIdentifier,QuantityVariablePhysicalQuantity,QuantityVariableDimensions,QuantityVariableCanonicalUnit}, Listable];

QuantityVariableIdentifier[]:=(Message[QuantityVariableIdentifier::argx, QuantityVariableIdentifier, 0];Null/;False)
QuantityVariableIdentifier[d_QuantityVariable] := First@d /; Length[d] > 0;
QuantityVariableIdentifier[input_QuantityVariable[___]] := QuantityVariableIdentifier[input]
QuantityVariableIdentifier[Derivative[__][input_QuantityVariable][__]] := QuantityVariableIdentifier[input]
QuantityVariableIdentifier[arg:Except[_Symbol]]:=(Message[QuantityVariableIdentifier::qvprm, arg, 1];Null/;False) 
QuantityVariableIdentifier[_,args__]:=(Message[QuantityVariableIdentifier::argx, QuantityVariableIdentifier, Length[{args}]+1];Null/;False)

QuantityVariablePhysicalQuantity[]:=(Message[QuantityVariablePhysicalQuantity::argx, QuantityVariablePhysicalQuantity, 0];Null/;False)
QuantityVariablePhysicalQuantity[d_QuantityVariable] := d[[2]] /; Length[d] > 1;
QuantityVariablePhysicalQuantity[d_QuantityVariable] := d[[1]] /; Length[d] === 1;
QuantityVariablePhysicalQuantity[input_QuantityVariable[___]] := QuantityVariablePhysicalQuantity[input]
QuantityVariablePhysicalQuantity[Derivative[__][input_QuantityVariable][__]] := QuantityVariablePhysicalQuantity[input]
QuantityVariablePhysicalQuantity[arg:Except[_Symbol]]:=(Message[QuantityVariablePhysicalQuantity::qvprm, arg, 1];Null/;False) 
QuantityVariablePhysicalQuantity[_,args__]:=(Message[QuantityVariablePhysicalQuantity::argx, QuantityVariablePhysicalQuantity, Length[{args}]+1];Null/;False)

QuantityVariableDimensions[pq_String]:=With[{res=oQuantityVariableDimensions[QuantityVariable[pq]]},Sort[DeleteCases[(List@@@res),{"DimensionlessUnit",_}]]/;res=!=$Failed]
QuantityVariableDimensions[args_]:=With[{res=oQuantityVariableDimensions[args]},Sort[DeleteCases[(List@@@res),{"DimensionlessUnit",_}]]/;res=!=$Failed]
QuantityVariableDimensions[_,args__]:=(Message[QuantityVariableDimensions::argx,QuantityVariableDimensions,Length[{args}]+1];Null/;False)
QuantityVariableDimensions[]:=(Message[QuantityVariableDimensions::argx,QuantityVariableDimensions,0];Null/;False)

(*oUnitDimensions returns a list of physical dimension rules*)
SetAttributes[oQuantityVariableDimensions,Listable];
oQuantityVariableDimensions[args_?(Not[FreeQ[#,QuantityVariable]]&)] := Module[
	{converted=(args /. QuantityVariable[arg__][__] :> QuantityVariable[arg])/.{
		QuantityVariable[_, pq_String, ___] :> canonunit[pq], QuantityVariable[pq_String] :> canonunit[pq],
		HoldPattern[Quantity[_, q_]] :> q, HoldPattern[Quantity[q_]] :> q,
		Derivative[n_][input_QuantityVariable][var_QuantityVariable]:>canonunit[input,{var,n}],
		Derivative[n_, no__][input_QuantityVariable][var_QuantityVariable, varo__QuantityVariable]:>canonunit[input,Transpose[{Prepend[varo,var],Prepend[no,n]}]]}},
  	If[FreeQ[converted, QuantityVariable | $Failed],
   		UnitDimensions[ReleaseHold[converted/.q_String:>Quantity[q]]],
   		$Failed
   	]
]
oQuantityVariableDimensions[___]:=$Failed

canonunit[QuantityVariable[_, pq_String, ___]]:=canonunit[pq]
canonunit[QuantityVariable[pq_String]]:=canonunit[pq]
canonunit[pq_String]:=Module[{unit=QuantityVariableLookup[pq, "Unit"]},
	If[FreeQ[unit, $Failed],
		If[FreeQ[unit,"PureUnities"],
			unit,
			1
		],
		Message[QuantityVariable::unkpq,pq];
		$Failed
	]
]
canonunit[input_,{var_,n_Integer}] := Module[
	{base = canonunit[input], reduce = canonunit[var]},
   	If[MatchQ[base,$Failed],Message[QuantityVariable::unkpq,input]];
   	If[MatchQ[reduce,$Failed],Message[QuantityVariable::unkpq,reduce]];
   	If[FreeQ[{reduce,base}, $Failed],
		If[Not[FreeQ[base,"PureUnities"]],base=1];
		If[Not[FreeQ[reduce,"PureUnities"]],reduce=1];
		base/reduce^n,
		$Failed
	]
]
canonunit[input_,dx:{{_,_Integer}..}] := Module[
	{base = canonunit[input], 
   		reduce=(canonunit[#[[1]]]^#[[2]]&)/@dx},
   	If[MatchQ[base,$Failed],Message[QuantityVariable::unkpq,input]];
   	If[FreeQ[{reduce,base}, $Failed],
		If[Not[FreeQ[base,"PureUnities"]],base=1];
		base/Times@@reduce,
		$Failed
	]
]
canonunit[___]:=$Failed

QuantityVariableLookup[pq_String,"Unit"]:=With[{unit=QuantityVariableLookup[pq,"CanonicalUnit"]/.HoldForm[Divide[a_, b_]] :> HoldForm[Times[a, Power[b, -1]]]},
	With[{un=ReleaseHold[unit]},If[Equal[unit,HoldForm[un]],un,unit,unit]]]
QuantityVariableLookup[pq_String,"Dimensions"]:=With[{unit=QuantityVariableLookup[pq,"CanonicalUnit"]},
	If[unit===$Failed,$Failed,UnitDimensions[Quantity[1,unit]]]]
QuantityVariableLookup[__]:=$Failed


QuantityVariableCanonicalUnit[]:=(Message[QuantityVariableCanonicalUnit::argx, QuantityVariableCanonicalUnit, 0];Null/;False)
QuantityVariableCanonicalUnit[QuantityVariable[_,pq_,___]]:=
	Module[{res=QuantityVariableLookup[pq,"Unit"]},
	If[SameQ[res,$Failed],Message[QuantityVariable::unkpq,pq];res=$Failed];
	If[Not[FreeQ[res,"PureUnities"]],res=1];
	res/;res=!=$Failed]
QuantityVariableCanonicalUnit[QuantityVariable[pq_String]]:=QuantityVariableCanonicalUnit[QuantityVariable["",pq]]
QuantityVariableCanonicalUnit[pq_String]:=QuantityVariableCanonicalUnit[QuantityVariable["",pq]]
QuantityVariableCanonicalUnit[input_QuantityVariable[___]] := QuantityVariableCanonicalUnit[input]
QuantityVariableCanonicalUnit[arg:Except[_Symbol]]:=(Message[QuantityVariableCanonicalUnit::qvprm, arg, 1];Null/;False) 
QuantityVariableCanonicalUnit[_,args__]:=(Message[QuantityVariableCanonicalUnit::argx, QuantityVariableCanonicalUnit, Length[{args}]+1];Null/;False)

pqCheck[pq_String] := QuantityVariableLookup[pq, "Dimensions"]
pqCheck[QuantityVariable[pq_]] := QuantityVariableLookup[pq, "Dimensions"]
pqCheck[QuantityVariable[_, pq_, ___]] := QuantityVariableLookup[pq, "Dimensions"]
pqCheck[___] := $Failed

checkPQs[input_List] := Module[{pqs = input}, 
	pqs = {#, pqCheck[#]} & /@ pqs;
  	If[FreeQ[pqs, $Failed], 
  		True, 
   		pqs = Cases[pqs, {x_, $Failed} :> x] /. {QuantityVariable[pq_] -> pq, QuantityVariable[_, pq_, ___] -> pq};
   		Message[QuantityVariable::unkpq, #] & /@ pqs; False
   	]
]
checkPQs[___] := False

checkGoalPQ[input_]:=Module[{dim=Quiet[QuantityVariableDimensions[input],{QuantityVariableDimensions::qvprm}]},
	If[MatchQ[dim,_List],True,False]
]

Options[DimensionalCombinations] = {IncludeQuantities -> {}, GeneratedParameters -> C};
Options[iqvCombinations] = {GeneratedParameters -> C};
Options[trimConstants] = {GeneratedParameters -> C};
$standardconstants = "PhysicalConstants" -> {Quantity[1, "BoltzmannConstant"], 
    Quantity[1, "ElectricConstant"], 
    Quantity[1, "GravitationalConstant"], 
    Quantity[1, "MagneticConstant"], Quantity[1, "PlanckConstant"], 
    Quantity[1, "SpeedOfLight"]};

System`DimensionalCombinations[listofPQs_?checkPQs,opts:OptionsPattern[]] := Module[{
	pqs = If[StringQ[#], QuantityVariable[#], #] & /@ listofPQs, method=MatchQ[OptionValue[GeneratedParameters],None],
	quantities=OptionValue[System`IncludeQuantities]/.$standardconstants,qdimensions,
	pqdimensions},
	quantities=If[ListQ[quantities],Flatten[quantities],{quantities}];
	quantities=Flatten[If[MatchQ[#,_Quantity], #, If[MatchQ[#,_String],Quantity[#], {}]] & /@ quantities];
	qdimensions=DeleteCases[{#, UnitDimensions[#]} & /@ quantities,{_,UnitDimensions[_]}];
  	pqdimensions = DeleteCases[checkDimensions /@ pqs,$Failed];
  	
  	pqdimensions=Join[pqdimensions,qdimensions];
  	If[pqdimensions==={},Return[{}]];
  	
  	If[method,
  		DeleteDuplicates[Flatten[Select[DeleteCases[iqvCombinations /@ Subsets[pqdimensions], {}], FreeQ[#, _C, \[Infinity]] &]], SameQ[#1, #2] || SameQ[(#1)^-1, #2] &],
  		iqvCombinations[pqdimensions,FilterRules[{opts},Options[iqvCombinations]]]
  	]
]
System`DimensionalCombinations[listofPQs_?checkPQs,dimension_?checkGoalPQ,opts:OptionsPattern[]] := Module[{
	pqs = If[StringQ[#], QuantityVariable[#], #] & /@ listofPQs, method=MatchQ[OptionValue[GeneratedParameters],None],
	quantities=OptionValue[System`IncludeQuantities]/.$standardconstants,qdimensions,
	pqdimensions,lhs=checkDimensions[If[StringQ[dimension], QuantityVariable[dimension], dimension]]},
	If[Not[FreeQ[lhs,$Failed]],Return[{}]];
	quantities=If[ListQ[quantities],Flatten[quantities],{quantities}];
	quantities=Flatten[If[MatchQ[#,_Quantity], #, If[MatchQ[#,_String],Quantity[#], {}]] & /@ quantities];
	qdimensions=DeleteCases[{#, UnitDimensions[#]} & /@ quantities,{_,UnitDimensions[_]}];
  	pqdimensions = DeleteCases[checkDimensions /@ pqs,$Failed];
  	
  	pqdimensions=Join[pqdimensions,qdimensions];
  	If[pqdimensions==={},Return[{}]];
  	
  	If[method,
  		DeleteDuplicates[Flatten[Select[DeleteCases[iqvCombinations[#,lhs]& /@ Subsets[pqdimensions], {}], FreeQ[#, _C, \[Infinity]] &]], SameQ[#1, #2] || SameQ[(#1)^-1, #2] &],
  		iqvCombinations[pqdimensions,lhs,FilterRules[{opts},Options[iqvCombinations]]]
  	]
]

iqvCombinations[pqdimensions_, opts : OptionsPattern[]] := Module[{dimensionrules, units, ansatz, expansion, sol, result}, 
  	units = Cases[Flatten[pqdimensions[[All, 2]]], _String];
  	dimensionrules = (#1 -> Times @@ ((Power @@@ #2))) & @@@ pqdimensions;
  	ansatz = 1 == Product[Power[pqdimensions[[i, 1]], Unique[]], {i, Length[pqdimensions]}];
  	expansion = PowerExpand[Log /@ (ansatz /. dimensionrules)];
  	sol = SolveAlways[expansion, Log /@ units];
  	sol = DeleteCases[trimConstants[#, opts] & /@ sol, $Failed];
  	result = If[sol === {}, {}, DeleteCases[ansatz[[2]] /. sol/. None -> 1, 1]]
]
      
iqvCombinations[pqdimensions_, lhs_, opts : OptionsPattern[]] := Module[
	{dimensionrules, units, ansatz, expansion, sol, result}, 
  	units = Cases[Flatten[pqdimensions[[All, 2]]], _String];
  	dimensionrules = (#1 -> Times @@ ((Power @@@ #2))) & @@@ Join[pqdimensions, {lhs}];
  	ansatz = lhs[[1]] == Product[Power[pqdimensions[[i, 1]], Unique[]], {i, Length[pqdimensions]}];
  	expansion = PowerExpand[Log /@ (ansatz /. dimensionrules)];
  	sol = SolveAlways[expansion, Log /@ units];
  	sol = DeleteCases[trimConstants[#, "LHS", opts] & /@ sol, $Failed];
  	result = If[sol === {}, {}, DeleteCases[ansatz[[2]] /. sol/. None -> 1, 1]/.HoldPattern[Quantity[1, (_String)^0]]:>1]
]


checkDimensions[pq_] := Module[{dim = QuantityVariableDimensions[pq]}, 
  	If[dim === {}, 
   		Message[DimensionalCombinations::dim, First[pq]]; 
   		Return[$Failed]
   	];
  	If[ListQ[dim], {pq, dim}, $Failed]
]
checkDimensions[___] := $Failed

trimConstants[list_?(VectorQ[#, Function[{x}, MatchQ[x, _Rule]]] &), opts : OptionsPattern[]] := Module[
	{l = list, sublist, constant = OptionValue[GeneratedParameters], factor, rhs = list[[All, 2]], default=1}, 
  	If[Not[FreeQ[list, Log[_]]], Return[$Failed]];
  	If[Element[rhs, Reals], 
  		factor = GCD @@ (Rationalize /@ rhs);
   		If[factor === 0, Return[$Failed], factor = 1/factor];
   		l = Rule[#[[1]], #[[2]]*factor] & /@ l;
   		Return[l]
   	];
  	sublist = Select[Union[Level[l[[All, 2]], {-1}]], Not[TrueQ[Element[#, Reals]]] &];
  	factor = (Join[Rationalize /@ (rhs /. (# -> default & /@ sublist)), Cases[rhs, Rational[_, _], Infinity]]);
  	If[Length[factor] === 1, 
  		factor = 1, 
  		If[AllTrue[factor, # === 0 &],
  			default=2;
  			factor = (Join[Rationalize /@ (rhs /. (# -> default & /@ sublist)), Cases[rhs, Rational[_, _], Infinity]])
  		];
  		factor = 1/(GCD @@ factor)
  	];
  	If[Length[sublist] === 1, 
  		sublist = sublist[[1]];
   		l = Map[#[[1]] -> factor*#[[2]] &, l];
   		Append[l /. sublist -> default, sublist -> factor], 
   		sublist = MapIndexed[#1 -> factor*constant[#2[[1]]] &, sublist];
   		l = Map[#[[1]] -> Expand[factor*#[[2]]] &, l];
   		Join[l /. sublist, sublist]
   	]
]
trimConstants[list_?(VectorQ[#, Function[{x}, MatchQ[x, _Rule]]] &), "LHS", opts : OptionsPattern[]] :=
Module[
	{l = list, sublist, constant = OptionValue[GeneratedParameters], rhs = list[[All, 2]], default=1}, 
  	If[Not[FreeQ[list, Log[_]]], Return[$Failed]];
  	If[Element[rhs, Reals], Return[l]];
  	sublist = Select[Union[Level[l[[All, 2]], {-1}]], Not[TrueQ[Element[#, Reals]]] &];
  	If[Length[sublist] === 1, 
  		sublist = sublist[[1]];
   		l = Map[#[[1]] -> #[[2]] &, l];
   		Append[l /. sublist -> default, sublist -> 1], 
   		sublist = MapIndexed[#1 -> constant[#2[[1]]] &, sublist];
   		l = Map[#[[1]] -> Expand[#[[2]]] &, l];
   		Join[l /. sublist, sublist]
   	]
]
trimConstants[_] := $Failed

QV2Q[HoldPattern[QuantityVariable[_, pq_String, ___]]] := 
 With[{units = QuantityVariableLookup[pq, "Unit"]}, 
  If[units == $Failed, Throw[$Failed, $tag]];
  	Quantity[1, units]
  ]
QV2Q[HoldPattern[QuantityVariable[pq_String]]] := QV2Q[QuantityVariable["", pq]]

QuantityVariableSantityCheck[expr_,specified___] := 
 Catch[With[{rules = 
     Cases[expr, q_QuantityVariable :> Rule[q, QV2Q[q]], -1]},
   If[SameQ[rules, {}], Throw[True, $tag]];
   If[MatchQ[
     Quiet[
     	Check[SeparateUnits[expr /. rules, specified], 
     		Throw[$Failed, $tag],
     		{Quantity::compat}],
     	{Quantity::compat}], 
     _SeparateUnits],
    Throw[$Failed, $tag],
    True]], $tag]
