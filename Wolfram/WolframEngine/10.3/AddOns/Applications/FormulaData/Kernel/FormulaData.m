(* ::Package:: *)

Begin["Tools`MathematicaFormulas`FormulaData`Private`"]

(* eliminate? *)
MathematicaFormula;
System`Entity;

(* Load the downvalues.m file. *)
$ProtectedSymbols = {
	MathematicaFormula, FormulaData, FormulaLookup, 
	RequiredPhysicalQuantities, ExcludedPhysicalQuantities,
	PlanckRadiationLaw
};
	
Unprotect@@$ProtectedSymbols;
$QueryTimeout = Automatic;
$tag = "FormulaDataCatchThrowTag";

downvalues = Get["FormulaData`downvalues`"]
DownValues[MathematicaFormula] = downvalues[[1]]
	
WolframAlpha;(*load WolframAlphaClient`*)

Attributes[APICompute] = {HoldAll};
APICompute[type_, args_] := Internal`MWACompute[type,args,"ContextPath" -> {"Internal`MWASymbols`", "System`", 
  "Tools`MathematicaFormulas`FormulaData`Private`"},"MessageHead"->FormulaLookup]

decamel[input_String] := 
 StringTrim[
  StringReplace[input, 
   MapThread[
    Function[{u, l}, 
     Rule[u, StringJoin[" ", l]]], {CharacterRange["A", "Z"], 
     CharacterRange["a", "z"]}]]]

localParse[input_String] := 
 Module[{data, n = Floor[StringLength[input]/2]}, 
  data = If[MatchQ[#, _List], StringJoin[Riffle[#, ":"]], #] & /@ 
    FormulaData[];
  data = Nearest[data, input, {All, 1}, 
    DistanceFunction -> (1/(SmithWatermanSimilarity[#1, #2, 
           GapPenalty -> 1] + $MachineEpsilon) &)];
  data = Select[data, 
    SmithWatermanSimilarity[input, #, GapPenalty -> 1] >= 
      Max[n, Floor[StringLength[#]/2]] &];
  If[StringFreeQ[#, ":"], #, StringSplit[#, ":"]] & /@ data]


MWANameLookup[input_String] := 
  Module[{data = 
     APICompute["MWAFormulaNameLookup", 
      Internal`MWASymbols`MWAFormulaNameLookup[input]], st}, 
   If[MatchQ[data, (HoldComplete|Hold)[{__}]], DeleteCases[ReleaseHold[data], {}],
    st = decamel[input];
    data = 
     APICompute["MWAFormulaNameLookup", 
      Internal`MWASymbols`MWAFormulaNameLookup[st]]; 
    If[MatchQ[data, (HoldComplete|Hold)[{__}]], DeleteCases[ReleaseHold[data], {}],
     If[StringLength[input] > 15, localParse[input], {}]]]
   ];



(* to sort between popular and rarely used formulas for FormulaDataLookup *)
validOptions = {RequiredPhysicalQuantities, ExcludedPhysicalQuantities, 
  "RequiredPhysicalQuantities", "ExcludedPhysicalQuantities"}

Options[iFormulaLookup]={ExcludedPhysicalQuantities->{},RequiredPhysicalQuantities->{}};

iFormulaLookup[input_,n_,o:OptionsPattern[]]:=Catch[Module[{
	list, limit=n, hasOptions, rpqlist, xpqlist},
	Quiet[(*check for bad options*)
		Check[
			OptionValue[RequiredPhysicalQuantities],
			Block[{FormulaLookup},
				Message[
					FormulaLookup::optx,
					First[First[DeleteCases[{o}, Rule[x_, _] /; MemberQ[validOptions, x]]]],
					FormulaLookup[input,n,o]
				];
				Throw[$Failed,$tag]
			],
			{OptionValue::nodef}],
		{OptionValue::nodef}
	];
	rpqlist=OptionValue[RequiredPhysicalQuantities]/.Automatic->{};
	xpqlist=OptionValue[ExcludedPhysicalQuantities]/.Automatic->{};
	If[Not[MatchQ[rpqlist,List[___String]]],Message[FormulaLookup::pqlst,RequiredPhysicalQuantities,rpqlist];Throw[$Failed,$tag]];
	If[Not[MatchQ[xpqlist,List[___String]]],Message[FormulaLookup::pqlst,ExcludedPhysicalQuantities,xpqlist];Throw[$Failed,$tag]];
	If[Or[UnsameQ[rpqlist,{}],UnsameQ[xpqlist,{}]],hasOptions=True];
	Which[
		limit===All,
			limit=Infinity,
		And[Not[IntegerQ[limit]],limit=!=Infinity,Not[TrueQ[limit>=0]]],
			Block[{FormulaLookup},Message[FormulaLookup::innf,FormulaLookup[input,n,o],2]];
			Throw[$Failed,$tag]
	];
	Which[
		input===All,
			If[TrueQ[hasOptions],
					list=ReleaseHold[DownValues[MathematicaFormula]/.MathematicaFormula -> Identity],
					list=FormulaData[]
			],
		StringQ[input],
			list=MWANameLookup[input];
			If[list==={},
				Return[Missing["NotAvailable"]],
				list=DeleteCases[If[MathematicaFormula[#]===Missing["NotAvailable"],{},#]&/@list,{}];
				If[list==={},
					Return[Missing["NotAvailable"]]
				]
			];
			If[TrueQ[hasOptions],list={#,MathematicaFormula[#]}&/@list];,
		True,
			Throw[$Failed,$tag]
	];
	If[Length[rpqlist]>0,
		list = Function[{pq}, 
			Cases[list, _?(Not[FreeQ[#, "PhysicalQuantity" -> pq] && 
				FreeQ[#, "PhysicalQuantity" -> {_ -> pq}]] &)]] /@ rpqlist;
		list=Intersection @@ list
	];
	If[Length[xpqlist]>0,
		list = Function[{pq}, 
			Cases[list, _?(FreeQ[#, "PhysicalQuantity" -> pq] && 
				FreeQ[#, "PhysicalQuantity" -> {_ -> pq}] &)]] /@ xpqlist;
		list=Intersection @@ list;
		list = list[[All,1]];
		If[Length[list]<limit,list,list[[;;limit]]],
		If[TrueQ[hasOptions],list=list[[All,1]]];
		If[Length[list]<limit,list,list[[;;limit]]]
	]
],
	$tag]

		
Options[FormulaLookup]={ExcludedPhysicalQuantities->{},RequiredPhysicalQuantities->{}};

FormulaLookup[]:=FormulaData[]
FormulaLookup[All]:=FormulaData[]
FormulaLookup[string_String,o:OptionsPattern[]]:=With[{res=iFormulaLookup[string,All,o]},res/;res=!=$Failed]
FormulaLookup[All,o:OptionsPattern[]]:=With[{res=iFormulaLookup[All,All,o]},res/;res=!=$Failed]
(* with limit argument *)
FormulaLookup[string_String,n_,o:OptionsPattern[]]:=With[{res=iFormulaLookup[string,n,o]},res/;res=!=$Failed]
FormulaLookup[All,n_,o:OptionsPattern[]]:=With[{res=iFormulaLookup[All,n,o]},res/;res=!=$Failed]
FormulaLookup[arg:Except[_String],OptionsPattern[]]:=(Message[FormulaLookup::notent,arg,FormulaLookup];Null/;False)
FormulaLookup[arg:Except[_String],_,OptionsPattern[]]:=(Message[FormulaLookup::notent,arg,FormulaLookup];Null/;False)
FormulaLookup[args__] := (ArgumentCountQ[FormulaLookup,Length[DeleteCases[{args},_Rule,Infinity]],1,2];Null/;False)

(* verfiying that input is a PQ *)
verifyPQ[name_,data_,inputrules_]:=Module[{rules=Cases[inputrules,HoldPattern[_ -> _Quantity]],
	vars=DeleteDuplicates[Cases[data,QuantityVariable[x_,___,"PhysicalQuantity"->y_,___]:>Rule[x,y],Infinity]]},
	If[Length[Cases[vars[[All,1]],#]]>1||Length[Cases[vars[[All,2]],#/.Entity["PhysicalQuantity", x_] :> x]]>1,
		Message[FormulaData::indet,#]]&/@inputrules[[All,1]];
	rules=rules/.Entity["PhysicalQuantity", x_] :> x;
	If[Length[rules]>0,
		(* quantities included are correct *)
		verifyPQVariable[vars,#]&/@rules
	]
]; 

verifyPQVariable[data_,inputvar_->value_Quantity]:=Module[
{vars=data,pq,temperatureQ,unit},
  If[MemberQ[vars[[All,1]],inputvar]||MemberQ[vars[[All, 2]],inputvar],
    pq=inputvar/.vars;
    unit=QuantityVariableCanonicalUnit[pq];
    temperatureQ=ReleaseHold@(UnitDimensions[unit]/."TemperatureDifferenceUnit"->"TemperatureUnit")===
    	(UnitDimensions[QuantityUnit[value]]/."TemperatureDifferenceUnit"->"TemperatureUnit");
    If[CompatibleUnitQ[QuantityUnit[value], unit]||temperatureQ,
        Null,
        Message[FormulaData::pq,inputvar,pq]
    ],
    Message[QuantityVariable::unkpq,inputvar]
  ]
];

$cleanDVRules={
	QuantityVariable[x_,___,"PhysicalQuantity"->{y_->_},___]:>QuantityVariable[x,y],
	QuantityVariable[x_,___,"PhysicalQuantity"->y_,___]:>QuantityVariable[x,y]};
	
FormulaData["Properties"] := {"Formula", 
  "QuantityVariableDimensions", "QuantityVariableNames", 
  "QuantityVariablePhysicalQuantities", "QuantityVariables", 
  "QuantityVariableTable"};
  
FormulaData[string_String,"Properties"] := With[{res=Check[FormulaData[string],False]},FormulaData["Properties"]/;res=!=False]

FormulaData[]=Cases[ReleaseHold[DownValues[MathematicaFormula] /. MathematicaFormula -> Identity][[All, 1]], _String | {_String ..}]
FormulaData[All]:=FormulaData[]

FormulaData[string:(_String|{__String})]:=Module[{name=MathematicaFormula[string]},(*check if is standard name *)
	If[name===Missing["NotAvailable"],
		Message[FormulaData::nvfn,string];
		(*FormulaData[string,"FormulaName"]*)Missing["NotAvailable"],
		name/.$cleanDVRules]
];
FormulaData[string:(_String|{__String}), "Formula"]:=FormulaData[string]/; If[MathematicaFormula[string]===Missing["NotAvailable"], 
	Message[FormulaData::nvfn,string];False, True];

FormulaData[string:(_String|{__String}),"QuantityVariableTable"]:=Module[{data,vars},
	data=MathematicaFormula[string];
	vars=DeleteDuplicates[Cases[data,QuantityVariable[x_,___,"Name"->name_,___,
			"PhysicalQuantity"->y_,___,"UnitDimensions"->z_,___]:>{x,name,y,z},Infinity]/.
		{x_,n_,{y_->z_},a_}:>{x,n,z,a}];
	vars=Prepend[vars,{"","name","physical quantity","dimensions"}];
	Style[Grid[vars,
		Alignment->{Left,Baseline},
		Dividers->{{2->GrayLevel[0.7]},{2->GrayLevel[0.7]}}], "DialogStyle", StripOnInput->False]
]/; If[MathematicaFormula[string]===Missing["NotAvailable"], Message[FormulaData::nvfn,string];False, True];

FormulaData[string:(_String|{__String}),"QuantityVariables"]:=Module[{data},
	data=MathematicaFormula[string];
	DeleteDuplicates[Cases[data,QuantityVariable[x_,___,"PhysicalQuantity"->y_,___]:>QuantityVariable[x,y],Infinity]]
]/; If[MathematicaFormula[string]===Missing["NotAvailable"], Message[FormulaData::nvfn,string];False, True];

FormulaData[string:(_String|{__String}),"QuantityVariablePhysicalQuantities"]:=Module[{data},
	data=MathematicaFormula[string];
	DeleteDuplicates[Cases[data,QuantityVariable[x_,___,"PhysicalQuantity"->y_,___]:>QuantityVariable[x,y]->y,Infinity]/.
		HoldPattern[x_->{y_->_}]:>x->y]
]/; If[MathematicaFormula[string]===Missing["NotAvailable"], Message[FormulaData::nvfn,string];False, True];

FormulaData[string:(_String|{__String}),"QuantityVariableDimensions"]:=Module[{data},
	data=MathematicaFormula[string];
	DeleteDuplicates[Cases[data,QuantityVariable[x_,___,"PhysicalQuantity"->y_,___,"UnitDimensions"->z_,___]:>QuantityVariable[x,y]->z,Infinity]]
]/; If[MathematicaFormula[string]===Missing["NotAvailable"], Message[FormulaData::nvfn,string];False, True];

FormulaData[string:(_String|{__String}),"QuantityVariableNames"]:=Module[{data},
	data=MathematicaFormula[string];
	DeleteDuplicates[Cases[data,QuantityVariable[x_,___,"Name"->z_,___,"PhysicalQuantity"->y_,___]:>QuantityVariable[x,y]->z,Infinity]]
]/; If[MathematicaFormula[string]===Missing["NotAvailable"], Message[FormulaData::nvfn,string];False, True];

FormulaData[string:(_String|{__String}),v_Association]:=FormulaData[string,Normal[v]]/; If[MathematicaFormula[string]===Missing["NotAvailable"], Message[FormulaData::nvfn,string];False, True]
FormulaData[string:(_String|{__String}),v_?(MatchQ[#,{(Rule | RuleDelayed)[_,_]..}]&)]:=Module[{data,vars,final,rules,PQtoIDrules},
	rules=(v/.QuantityVariable[x_,___]:>x);
	data=MathematicaFormula[string];
	PQtoIDrules=Cases[data,QuantityVariable[x_,___,"PhysicalQuantity"->y_,___]:>Rule[y,x],Infinity];
	verifyPQ[string,data,rules]; (* check that variables are valid *)
	rules=rules/.PQtoIDrules/.Entity["PhysicalQuantity", x_] :> x;
	vars=Cases[data,QuantityVariable[x_,___]:>x,Infinity];
	If[Length[Complement[vars,rules[[All,1]]]]===Length[Flatten[{data}]]&&VectorQ[rules[[All, 2]], MatchQ[#, _Quantity] || NumericQ[#] &],
		final=Complement[vars,rules[[All,1]]];
		final=Flatten[{Cases[data,QuantityVariable[#,___],Infinity][[1]]}&/@final];
		data=(data/.Map[Rule[QuantityVariable[#[[1]],__],#[[2]]]&,rules]);
		final=Quiet[Reduce[data,final,Reals]/.$cleanDVRules];
		If[Quiet[MatchQ[final,_Reduce]],final[[1]],final],
		data/.Map[Rule[QuantityVariable[#[[1]],__],#[[2]]]&,rules]/.$cleanDVRules
	]
]/; If[MathematicaFormula[string]===Missing["NotAvailable"], Message[FormulaData::nvfn,string];False, True];


FormulaData[arg:Except[_String|{__String}]]:=(Message[FormulaData::notent, arg, FormulaData];Null/;False)
FormulaData[arg:Except[_String|{__String}],args___]:=(Message[FormulaData::notent, arg, FormulaData];Null/;False)/;Quiet[MemberQ[Append[FormulaData["Properties"],"Properties"],args]]
FormulaData[_,args_,___]:=((Message[FormulaData::notprop, args,FormulaData];Null/;False)/;Not[MemberQ[Append[FormulaData["Properties"],"Properties"],args]])

If[FileExistsQ[#],Get[#]]&[FileNameJoin[{DirectoryName[$InputFileName],"PlancksLaw.m"}]]

With[{s=$ProtectedSymbols},SetAttributes[s,{ReadProtected}]];
Protect@@$ProtectedSymbols;

End[]; (* End Private Context *)
