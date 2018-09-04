(*method for getting quantity in it's SI base unit form*)
ConvertToSIBaseUnits[n_?NumericQ]:=n
c2sibu[quantity_Quantity]:=Block[{$IgnoreMalformedQuantities=True},AlphaBlock[Module[
	{res = ConvertToSIBaseUnits[toAlphaQuantity[quantity]],q,unit}, 
 If[TrueQ[res == 0], 
 	q=Block[{$AlphaFlag=False},Quantity[1, QuantityUnit[quantity]]];
 	unit=QuantityUnit[ConvertToSIBaseUnits[q]];
  UnitConvert[quantity, unit], 
  res]
]]]

c2sibu[__] = $Failed

$TempDiffRR = {
 CalculateUnits`UnitCommonSymbols`DegreesCelsiusDifference->CalculateUnits`UnitCommonSymbols`DegreesCelsius,
 CalculateUnits`UnitCommonSymbols`DegreesFahrenheitDifference->CalculateUnits`UnitCommonSymbols`DegreesFahrenheit,
 CalculateUnits`UnitCommonSymbols`KelvinsDifference->CalculateUnits`UnitCommonSymbols`Kelvins};
(*generalized unit conversion using Alpha's ConvertUnits function*)
cu[args___]:=Internal`InheritedBlock[{CalculateUnits`UnitTable`MUnit},
	TagSetDelayed[CalculateUnits`UnitTable`MUnit,CalculateParse`Content`Calculate`Unit[CalculateUnits`UnitTable`MUnit[a_, 1]], CalculateParse`Content`Calculate`Unit[a]];
	AlphaBlock[
	Module[
			{res=ConvertUnits[args],
			args1=If[Quiet[Head[First[{args}]]]===Quantity,
				QuantityUnit[First[{args}]],
				First[{args}],Message[cu::failed];$Failed],
			args2=If[Quiet[Head[{args}[[2]]]]===Quantity,
				QuantityUnit[{args}[[2]]],
					{args}[[2]],Message[cu::failed];$Failed]},
		If[
			res===$Failed,
			If[
				And[Not[TrueQ[$RecFlag]],SameQ[UnitDimensions[args2], {{"TemperatureDifferenceUnit", 1}}]],
				Block[{$RecFlag=True},cu[First[{args}],args2/.$TempDiffRR]],
				Message[Quantity::compat,fromAlphaUnit[args1],fromAlphaUnit[args2]];res],
			If[Head[res]===CalculateUnits`UnitCommonSymbols`ConvertUnits,
				Message[Quantity::compat,fromAlphaUnit[args1],fromAlphaUnit[args2]];$Failed,
				res],
			Message[UnitConvert::fail];$Failed]
		]
	]]

Unprotect[CurrencyConvert];
(*SetAttributes[CurrencyConvert,Listable];*)
CurrencyConvert[_,_,args__]:=(Message[CurrencyConvert::argrx,CurrencyConvert,Length[{args}]+2,2];False)
CurrencyConvert[args__]:=With[{res=iUnitConvert[args]},res/;res=!=False]
CurrencyConvert[]:=(Message[CurrencyConvert::argrx,CurrencyConvert,0,2];Null/;False)
	
Unprotect[UnitConvert];
(*SetAttributes[UnitConvert,Listable];*)
UnitConvert[args__]:=With[{res=iUnitConvert@@handleNone[{args}]},res/;res=!=False]
UnitConvert[]:=(Message[UnitConvert::argrx,UnitConvert,0,2];Null/;False)

convertAssociation[input_String, target_]/; KnownUnitQ[input] := iUnitConvert[input, target] (*don't try and parse random strings in association*)
convertAssociation[input_?QuantityQ, target_] := iUnitConvert[input, target]
convertAssociation[input_List, target_] := Map[convertAssociation[#,target]&, input]
convertAssociation[input_Association, target_] := Map[convertAssociation[#,target]&, input] /; AssociationQ[input]
convertAssociation[input_TemporalData, target_] := convertTimeSeries[input, target]
convertAssociation[sa_StructuredArray, newunit_] := StructuredArrayUnitConvert[sa, newunit]
convertAssociation[input_, _] := input(*all others pass through unevaluated*)

iUnitConvert[a_Association, target_:"Base"] := Map[convertAssociation[#, target]&, a] /; AssociationQ[a]
iUnitConvert[ts_TemporalData,target_:"Base"] := convertTimeSeries[ts,target]
iUnitConvert[sa_StructuredArray, newunit_] := StructuredArrayUnitConvert[sa, newunit];
iUnitConvert[HoldPattern[x_Entity],___]:=x
(*"Base" is the primary reference to SIBase unit conversion, but also fires for single-argument form and "SIBase" specs*)
iUnitConvert[_,_,args__]:=(Message[UnitConvert::argrx,UnitConvert,Length[{args}]+2,2];False)
iUnitConvert[]:=(Message[UnitConvert::argrx,UnitConvert,0];False)
iUnitConvert[(exp_)?ValidUnitQ]:=iUnitConvert[exp,"Base"]
iUnitConvert[(unit_)?KnownUnitQ]:=iUnitConvert[unit,"Base"]
iUnitConvert[s_String]/;Not[KnownUnitQ[s]]:=With[{q=Quantity[s]},If[Quiet[QuantityQ[q]],iUnitConvert[q,"Base"],False,False]]
iUnitConvert[n_?NumericQ]:=n
iUnitConvert[exp_,"SIBase"]:=iUnitConvert[exp,"Base"]
iUnitConvert[n_?NumericQ,"Base"]:=n
(*Outer-like Pseudolistability*)
iUnitConvert[q:{Quantity[_,unit_]..},args_?KnownUnitQ] := With[{r=Catch[bulkUnitConvert[q,args],$tag]}, r/;r=!=False](*fallback is 'standard' evaluation*)
iUnitConvert[l_List,arg_List]/;Length[l]===Length[arg] := Which[
	ArrayDepth[l]>ArrayDepth[arg],Map[UnitConvert[#,arg]&,l],
	ArrayDepth[l]==ArrayDepth[arg], MapThread[UnitConvert,{l,arg},ArrayDepth[l]],
	True, Message[UnitConvert::arrdpt]
]
	
iUnitConvert[l_List,arg___] := UnitConvert[#, arg]&/@l
iUnitConvert[arg_,l_List] := UnitConvert[arg,#]&/@l

iUnitConvert[q_Quantity,unit_?KnownUnitQ] := With[{r=Catch[fastConvert[q,unit],$tag]},r/;r=!=$Failed]
iUnitConvert[q_?QuantityQ, d_?DateObjectQ] := unitConvertForDate[q,d]
iUnitConvert[u_?KnownUnitQ, d_?DateObjectQ] := unitConvertForDate[Quantity[u], d]

$UnitSystems={"SI","SIBase","Base","Imperial","Metric"};
iUnitConvert[n_?NumericQ,system_String]/;MemberQ[{"SI","Imperial","Metric"},system]:=n
iUnitConvert[s1_String,s2_String]/;Not[MemberQ[$UnitSystems,s2]]/;Or[
	Not[KnownUnitQ[s1]],Not[KnownUnitQ[s2]]]:=With[{q1=Quantity[s1],q2=Quantity[s2]},If[Quiet[And[ValidUnitQ[q1],ValidUnitQ[q2]]],iUnitConvert[q1,q2],False,False]]
iUnitConvert[s1_String,arg2_]/;Not[KnownUnitQ[s1]]:=With[{q1=Quantity[s1]},If[Quiet[ValidUnitQ[q1]],iUnitConvert[q1,arg2],False,False]]
iUnitConvert[arg1_,s2_String]/;Not[MemberQ[$UnitSystems,s2]]/;Not[KnownUnitQ[s2]]:=With[{q2=Quantity[s2]},If[Quiet[ValidUnitQ[q2]],iUnitConvert[arg1,q2],False,False]]
iUnitConvert[(u_)?KnownUnitQ, arg_] := 
 With[{q = Quantity[1, u]}, 
  If[Quiet[ValidUnitQ[q]], iUnitConvert[q, arg], False, False]]
iUnitConvert[HoldForm[u_], arg_] /; KnownUnitQ[u] := 
 With[{q = Quantity[1, u]}, 
  If[Quiet[ValidUnitQ[q]], iUnitConvert[q, arg], False, False]]
iUnitConvert[(q:Quantity[mag_,unit_,opts___?OptionQ])?ValidUnitQ,unit_]:=q
iUnitConvert[(q:Quantity[mag_,unit_,opts___?OptionQ])?ValidUnitQ,targ_]/;QuantityTensorQ[q]:=With[
	{u=QuantityUnit[UnitConvert[Quantity[1,unit],targ]]},
If[KnownUnitQ[u],
Module[
	{f=uCF[unit,u]},
	Quantity[f/@mag,u,opts]
	],
	$Failed
]
]
	
uCF[oldunit_, newunit_] := With[{
	factor = QuantityMagnitude[Quantity[2, oldunit], newunit] - QuantityMagnitude[Quantity[1, oldunit], newunit], 
    constant = QuantityMagnitude[Quantity[0, oldunit], newunit]}, 
   Function[{value}, value*factor + constant]
]

iUnitConvert[(quantity_Quantity)?ValidUnitQ,MixedRadixUnit[units__]]:=cu[quantity,{units}]
iUnitConvert[(quantity_Quantity)?ValidUnitQ,mr_MixedRadix]:=MixedRadixConvert[quantity,mr]

(*special case to handle some internal Alpha unit conversions*)
iUnitConvert[(quantity_Quantity)?ValidUnitQ,str_String]/;StringMatchQ[str,"MixedRadixUnit["~~__]:=Block[
	{$IgnoreMalformedQuantities=True},
	Module[{un=toAlphaUnit[str]/.CalculateUnits`UnitCommonSymbols`MixedRadixUnit->List},
	cu[quantity,un]]]

iUnitConvert[(quantity_Quantity)?ValidUnitQ,target_?hasCurrencyQ]:=currencyConvert[quantity,target]
iUnitConvert[(quantity_Quantity)?ValidUnitQ,"Base"]/;hasCurrencyQ[quantity]:=iSimplifyQuantity[currencyConvert[quantity,"Base"]]
iUnitConvert[(quantity_Quantity)?ValidUnitQ,"Base"]/;Not[MixedRadixQ[quantity]]:=iSimplifyQuantity[c2sibu[quantity]]
iUnitConvert[(quantity_Quantity)?ValidUnitQ,"Base"]/;MixedRadixQ[quantity]:=iSimplifyQuantity[UnitConvert[unmixradixquantity[quantity],"Base"]]

iUnitConvert[{(quantities:_Quantity)?ValidUnitQ..},"Base"]:=Block[{$AlphaFlag=True},
	UnitConvert[#,"Base"]&/@{quantities}
]

(*non SIBase systems don't include any official currencies, so these are return the input identity*)
iUnitConvert[(quantity_Quantity)?ValidUnitQ,unitsystem_String]/;hasCurrencyQ[quantity]/;MemberQ[{"SI","Imperial","Metric"},unitsystem]:=quantity
iUnitConvert[(quantity_Quantity)?ValidUnitQ,unitsystem_String]/;MemberQ[{"SI","Imperial","Metric"},unitsystem]:=Block[{$IgnoreMalformedQuantities=True},
	Quiet[cu[toAlphaQuantity[quantity],unitsystem]/.CalculateScan`UnitScanner`Private`ConversionToUnitSystemResult[q_]:>q]]

iUnitConvert[(quantity_Quantity)?ValidUnitQ,targetunit_?KnownUnitQ]/;hasCurrencyQ[quantity]||hasCurrencyQ[targetunit]/;Not[MemberQ[{"SI","Imperial","Metric"},targetunit]]:=currencyConvert[quantity,targetunit]

iUnitConvert[(quantity_Quantity)?ValidUnitQ,targetunit_?KnownUnitQ]/;MixedRadixQ[quantity]:=
cu[toAlphaQuantity[unmixradixquantity[quantity]],toAlphaUnit[targetunit]]

iUnitConvert[(quantity_Quantity)?ValidUnitQ,targetunit_?KnownUnitQ]:=
cu[toAlphaQuantity[quantity],toAlphaUnit[targetunit]]

iUnitConvert[(quantity_Quantity)?ValidUnitQ,(targetunit_Quantity)?ValidUnitQ]:=UnitConvert[quantity,QuantityUnit[targetunit]]
iUnitConvert[(quantity_Quantity)?ValidUnitQ,HoldForm[u_]]:=UnitConvert[quantity,toAlphaUnit[u]]
	
iUnitConvert[quantities:{(_Quantity)?ValidUnitQ..},(targetunit_Quantity)?ValidUnitQ]:=(UnitConvert[#,targetunit]&/@quantities)
iUnitConvert[quantities:{(_Quantity)?ValidUnitQ..},targetunit_?KnownUnitQ]:=(UnitConvert[#,targetunit]&/@quantities)

iUnitConvert[list:{(_Quantity)..},targetunit_Quantity]:=Map[If[Head[#]===Quantity,UnitConvert[#,targetunit],#,Message[UnitConvert::failed];$Failed]&,list]
iUnitConvert[list:{(_Quantity)?ValidUnitQ..},targetunit_?KnownUnitQ]:=Map[If[Head[#]===Quantity,UnitConvert[#,targetunit],#,Message[UnitConvert::failed];$Failed]&,list]

iUnitConvert[n_?NumericQ,t_]/;MemberQ[{"DimensionlessUnit","PureUnities"},t]:=n
iUnitConvert[n_?NumericQ,_?NumericQ]:=n
iUnitConvert[n_?NumericQ,targ_?KnownUnitQ]:=If[oUnitDimensions[targ]==={"DimensionlessUnit"->1},With[{value=iUnitConvert[targ,"Base"]},Quantity[n/value,targ]],(Message[Quantity::compat,"DimensionlessUnit",targ];False)]
iUnitConvert[n_?NumericQ,targ_?ValidUnitQ]:=If[oUnitDimensions[targ]==={"DimensionlessUnit"->1},With[{value=iUnitConvert[targ,"Base"],unit=QuantityUnit[targ]},Quantity[n/value,unit]],(Message[Quantity::compat,"DimensionlessUnit",QuantityUnit[targ]];False)]
iUnitConvert[(q_Quantity)?ValidUnitQ,n_?NumericQ]/;oUnitDimensions[q]==={"DimensionlessUnit"->1}:=QuantityExpand[q]
iUnitConvert[(q_Quantity)?ValidUnitQ,n_?NumericQ]:=(Message[Quantity::compat,"DimensionlessUnit",QuantityUnit[q]];False)
iUnitConvert[(q_Quantity)?ValidUnitQ,d_DatedUnit]/;Not[KnownUnitQ[d]]:=With[{res=Quantity[1,d]},
	(iUnitConvert[q,res])/;Quiet[QuantityQ[res]]]
iUnitConvert[(q_Quantity)?ValidUnitQ,unit:Except[_?KnownUnitQ|_?ValidUnitQ]]:=(Message[Quantity::unkunit,unit];False)
iUnitConvert[expr:Except[_Quantity|_List], system:"SI"|"Base"|"Imperial"|"SIBase"] /; UnsameQ[$RecFlag, "iUC"] := Block[{$RecFlag = "iUC"},
	expr/.q_?QuantityQ:>iUnitConvert[q,system]
]
iUnitConvert[expr:Except[_Quantity|_List]] := iUnitConvert[expr,"SIBase"]
iUnitConvert[___]=False;

StructuredArrayUnitConvert[sa_, newunit_] := Which[
	QuantityArray`QuantityArrayQ[sa],
		QuantityArray`QuantityArrayUnitConvert[sa, newunit],
	True,
		UnitConvert[Normal[sa, StructuredArray], newunit]
];

unitConvertForDate[q:Quantity[_,unit_,___], date_DateObject] := Catch[With[{currency = getCurrencyFromUnit[unit]},
	UnitConvert[q, DatedUnit[currency, date]]
], $tag]

SetAttributes[getCurrencyFromUnit, HoldAll];
getCurrencyFromUnit[u_String] := If[UnitDimensions[u]==={{"MoneyUnit",1}}, u, Throw[False, $tag]]
getCurrencyFromUnit[_MixedRadix] := Throw[False, $tag]
getCurrencyFromUnit[DatedUnit[u_,_]] := If[UnitDimensions[u]==={{"MoneyUnit",1}}, u, Throw[False, $tag]]
getCurrencyFromUnit[u_?KnownUnitQ] := Module[{units=Cases[HoldComplete[u],_String,-1]},
	units=Select[units,UnitDimensions[#]==={{"MoneyUnit",1}}&];
	If[Length[units]===1, First[units], Throw[False,$tag]]
]
getCurrencyFromUnit[___] := Throw[False, $tag]

(*optimized version of cu that handles most common simple cases of conversion*)
fastConvert[q:Quantity[_, unit_],unit_] := q
fastConvert[Quantity[value_, unit_], targetUnit_] /; FreeQ[{unit, targetUnit}, IndependentUnit | DatedUnit | MixedRadix] := If[
	fasttrackableUnitsQ[unit,targetUnit],
	doFastConvert[unit, targetUnit, value],
	$Failed
]
fastConvert[___] := $Failed

fasttrackableUnitsQ[unit_, targetUnit_]:= And[
		nonArithmaticFreeQ[unit], 
		CompatibleUnitQ[unit, targetUnit]
		]
fasttrackableUnitsQ[___] False

(*abs temperatures are non-zero base*)(*TODO: improve performance here*)
nonArithmaticFreeQ[__] = False
nonArithmaticFreeQ[unit_] := nonArithmaticFreeQ[unit] = inonArithmaticFreeQ[unit]

inonArithmaticFreeQ[unit_String] := FreeQ[
	CalculateUnits`UnitCommonSymbols`UnitLookup[toAlphaUnitPart[unit], "UnitDimensions"], 
	CalculateUnits`UnitCommonSymbols`TemperatureUnit
]

inonArithmaticFreeQ[unit_] := FreeQ[
	UnitDimensions[unit], 
	"TemperatureUnit"
]

 
doFastConvert[unit_, targetUnit_, value_] := Quantity[
  convertToValue[unit, targetUnit, value],
  targetUnit]
  
convertToValue[unit_, targetUnit_, value_] := getConversionRatio[
  	getCUF1[targetUnit],
  	getConversionRatio[
  		getCUF0[unit], 
  		value
  	]
   ]  
  
getCUF1[unit_String] := CalculateUnits`UnitTable`CUF1[toAlphaUnitPart[unit]]

getCUF1[Times[unit1_, unit2_]] := Times[getCUF1[unit1], getCUF1[unit2]]
getCUF1[Power[unit_String, n_]] := Power[getCUF1[unit], n]
getCUF1[other_] := Throw[$Failed, $tag]

getCUF0[unit_String] := CalculateUnits`UnitTable`CUF0[toAlphaUnitPart[unit]]

getCUF0[Times[unit1_, unit2_]] := Times[getCUF0[unit1], getCUF0[unit2]]
getCUF0[Power[unit_String, n_]] := Power[getCUF0[unit], n]
getCUF0[other_] := Throw[$Failed, $tag]
   
getConversionRatio[arg_, value_] := (arg /. f_Function :> f[1]) value
		
(*special conversion code to keep track of currency conversion and manage communication with FinancialData*)
currencyConvert[(q_Quantity)?ValidUnitQ,target_]:=Module[{res},If[Or[hasDatedUnitQ[q],hasDatedUnitQ[target]],
	If[UnitDimensions[q]==={{"MoneyUnit",1}},
		If[UnsameQ[$DCCRecurssionFlag,True],
			Block[{$DCCRecurssionFlag=True},res=datedCurrencyConvert[q,target/.Thread[Rule[$UnitSystems,"USDollars"]]]],
			$Failed],
			If[And[UnsameQ[$DCCRecurssionFlag,True],FreeQ[UnitDimensions[target],"MoneyUnit"]],
				Block[{$DCCRecurssionFlag=True},
				convertWithoutDatedUnit[q,target]],
		If[And[UnsameQ[$DCCRecurssionFlag,True],MatchQ[target,_DatedUnit|Quantity[_,_DatedUnit]]],
			extractAndConvertHistoricalCurrency[q,target],
			If[UnsameQ[$RecFlag,True],
				Block[{$RecFlag=True},InflationAdjust[q,target]],
			$Failed]]]],
	res=Quiet[Check[Check[If[target==="Base",c2sibu[q],cu[q,toAlphaUnit[target]]],"conopen",{FinancialData::conopen}],"notent",FinancialData::notent],{FinancialData::conopen,FinancialData::notent,Quantity::compat}];
	Switch[res,"conopen",Message[UnitConvert::conopen];q,
		"notent",secondarycurrencyConvert[q,target],$Failed,secondarycurrencyConvert[q,target],
		_,
		If[MatchQ[res,_Quantity]&&Not[ValidUnitQ[res]],
			Quiet[With[{simp=iSimplifyQuantity[res]},
				If[ValidUnitQ[simp],simp,iFixBaseUnit[q]]],{Quantity::unkunit}],
			res,
			res]]]]
currencyConvert[___]:=$Failed

convertWithoutDatedUnit[q_,target_]:=With[{du=Rule[#,IndependentUnit[ToString[Unique[]]]]&/@Cases[q,_DatedUnit,-1]},
	If[UnsameQ[du,{}],
	With[{r=Quiet[Check[UnitConvert[q/.du,target]/.Reverse[du,{2}],$Failed,{Quantity::compat}],{Quantity::compat}]},
	If[SameQ[r,$Failed],Message[Quantity::compat,q/.quant_?QuantityQ:>QuantityUnit[quant],target]];r],
	$Failed,
	$Failed]
]

extractAndConvertHistoricalCurrency[q_,target_]:=Catch[Module[{
	rules = Rule[#, IndependentUnit[ToString[Unique[]]]] & /@ Cases[exp2list[{QuantityUnit[q]}], du_DatedUnit /; UnitDimensions[du] === {{"MoneyUnit", 1}}], 
	working, dims, ratio, outunit, outmag,tu}, 
	If[rules=={},rules = Rule[#, IndependentUnit[ToString[Unique[]]]] & /@ Cases[exp2list[{QuantityUnit[q]}], currency_String /; UnitDimensions[currency] === {{"MoneyUnit", 1}}]];
	dims = Rule @@ # & /@ Cases[UnitDimensions[working = q /. rules], {dim_, _} /; MemberQ[rules[[All, 2, 1]], dim]]; 
	tu=With[
		{testunit=Quiet[Check[First[First[rules]],Throw[Missing["NotAvailable"],$tag]]]},
		QuantityUnit[
			Check[
				Check[
					InflationAdjust[Quantity[1, testunit], target,$InflationAdjustOptions],
					Throw[Missing["NotAvailable"],$tag],
					{InflationAdjust::insfferd,InflationAdjust::curnc,InflationAdjust::datenc,InflationAdjust::badcpi}],
				Throw[$Failed,$tag]
			]
		]];
	ratio = Times @@ (Power[QuantityMagnitude[Check[InflationAdjust[Quantity[1, #[[1]]], target,$InflationAdjustOptions],Throw[Missing["NotAvailable"],$tag]]], ((#[[2]]) /. IndependentUnit[a_] :> a) /. dims] & /@ rules);
	outunit = QuantityUnit[q] /. (rules/.HoldPattern[Rule[x_,_]]:>Rule[x,tu]);
	outmag = QuantityMagnitude[q]*ratio;
	Quantity[outmag, outunit]],$tag]
extractAndConvertHistoricalCurrency[__]:=$Failed

datedCurrencyConvert[q_,target_Quantity?ValidUnitQ]:=With[{tu=QuantityUnit[target]},InflationAdjust[q,tu,$InflationAdjustOptions]]
datedCurrencyConvert[q_,target_]:=InflationAdjust[q,target,$InflationAdjustOptions]

secondarycurrencyConvert[q_Quantity,target_]:=If[KnownUnitQ[target]&&UnsameQ[UnitDimensions[q],UnitDimensions[target]],
	Message[Quantity::compat,QuantityUnit[q],target];$Failed,
	Message[UnitConvert::nodat];q]

$InflationAdjustOptions=Sequence[];
(*Options[InflationAdjustUnitConvert]={System`InflationMethod->Automatic};  Removed because of 249735*)

InflationAdjustUnitConvert[arguments__,o:OptionsPattern[]]:= Block[{$InflationAdjustOptions=o},
	If[And[Length[{arguments}]===2,NumericQ[Part[{arguments},2]],hasDatedUnitQ[Part[{arguments},1]]],
	With[{target=First[Cases[{First[{arguments}]},DatedUnit[unit_,_]:>DatedUnit[unit,Part[{arguments},2]],-1,1]]},
		If[MatchQ[target,DatedUnit[_,_]],
			UnitConvert[First[{arguments}],target],
			UnitConvert[arguments]]],
	UnitConvert[arguments]
]]

(*special case handling for base unit conversion w/ currency*)
iFixBaseUnit[q_Quantity]/;$iFixFlag=!=True:=Block[{$iFixFlag=True},With[{r=Quiet@UnitConvert[q,"USDollars"]},If[ValidUnitQ[r],UnitConvert[r,"Base"],r]]]
iFixBaseUnit[___]:=$Failed

MixedRadixConvert[quantity_Quantity, MixedRadix[units__]] /; CompatibleUnitQ[quantity, units] := With[
  {order = Reverse@Ordering[Quantity[1, #] & /@ {units}]}, 
  Internal`InheritedBlock[{FractionalPart},
  	Unprotect[FractionalPart];FractionalPart[Interval[i_]]:=Interval[FractionalPart[i]];
  Module[{remainder = quantity, results = {}},
   With[
      {uc = UnitConvert[remainder, {units}[[#]]]}, 
      If[Length[results] < (Length[order] - 1), 
       AppendTo[results, IntegerPart[uc]]; remainder = FractionalPart[uc], 
       AppendTo[results, uc]]] & /@ order; 
    If[Not[FreeQ[results,Interval]],fixIntervalResults[results],
    With[{mag = MixedRadix @@ QuantityMagnitude[results], unit = MixedRadix @@ QuantityUnit[results]}, 
    If[ValidUnitQ[Quantity[mag, unit]],Quantity[mag, unit],$Failed]]
    ]
    ]
  ]]
MixedRadixConvert[___]:=$Failed

(*bulkUnitConvert[units_List] := bulkUnitConvert[units,"Base"]*)
bulkUnitConvert[_,"SI"|"Imperial"|"Metric"|"Base"|"SIBase"] := False
bulkUnitConvert[q:{Quantity[_,unit_]..}, unit_] := q
bulkUnitConvert[q:{Quantity[_,unit_]..}, HoldForm[unit_]] := q
bulkUnitConvert[q:{Quantity[_,unit_]..},targetUnit_]/;fasttrackableUnitsQ[unit, targetUnit] := Module[{values, factor},
	values=QuantityMagnitude[q]; factor = convertToValue[unit,targetUnit,1];
	Quantity[values*factor, targetUnit]
]
bulkUnitConvert[___] := False
(*-----NormalizeMixedRadixQuantity------*)
(*-this function is used in construction and validation of MixedRadix Quantity expressions-*)
(*-to avoid redundant validation and to speed things up, this utility calls low-level Alpha functions directly-*)
getOrdering[units_List]:= Reverse[
	Ordering[
		Map[
			UnitLookup[toAlphaUnitPart[#], "FundamentalUnitValue"]&,
			units]
	]
]

quickUnmix[q : Quantity[mag_MixedRadix, units:MixedRadix[u_,___],___]] := With[
	{qs = Cases[Map[Quantity@@# &, Transpose[{List @@ mag, List @@ units}]],HoldPattern[_Quantity]]},  
  Quantity[Total[First[fastTrackConvert[#, u]] & /@ qs], u]]

fastTrackConvert[quantity:Quantity[_MixedRadix,__], targetUnit_] := 
	CalculateUnits`UnitCommonSymbols`ConvertUnits[toAlphaQuantity[quickUnmix[quantity]],toAlphaUnit[targetUnit]]
fastTrackConvert[quantity:Quantity[Except[_MixedRadix],__], targetUnit_] := 
	CalculateUnits`UnitCommonSymbols`ConvertUnits[toAlphaQuantity[quantity],toAlphaUnit[targetUnit]]
fastTrackConvert[other_,___]:=other

convertTimeSeries[series_?TemporalData`StructurallyValidTemporalDataQ, targetUnit_] :=
 Module[{states = series["ValueList"], vals, res},
  vals = UnitConvert[states, targetUnit];(* validate result *)
  res = TemporalData`ReplaceStates[series, vals];
  If[TemporalData`StructurallyValidTemporalDataQ[res],
   res, Message[UnitConvert::tsq, series, targetUnit]$Failed
   ]] 
convertTimeSeries[___] := $Failed

getIntegerPart[quantity_] := Internal`InheritedBlock[{IntegerPart},
		Unprotect[IntegerPart];
		IntegerPart[Quantity[mag_, unit_,___]] := Quantity[IntegerPart[mag], unit];
		
		With[{res = IntegerPart[quantity]},
		If[FreeQ[res,IntegerPart],
			res,
			Throw[$Failed,"Normalizer"]
		]	
	]
]

getFractionalPart[quantity_] := Internal`InheritedBlock[{FractionalPart},
		Unprotect[FractionalPart];
		FractionalPart[Interval[i_]] := Interval[FractionalPart[i]];
		FractionalPart[Quantity[mag_, unit_,___]] := Quantity[FractionalPart[mag], unit];
		
		With[{res = FractionalPart[quantity]},
		If[FreeQ[res,FractionalPart],
			res,
			Throw[$Failed,"Normalizer"]
		]	
	]
]

(*TODO: add handler for Quantity[Interval[{_MixedRadix..*)
NormalizeMixedRadixQuantity[args___] := Catch[Block[{$AlphaBlockFlag = True },iNormalizeMixedRadixQuantity[args]], "Normalizer"]
iNormalizeMixedRadixQuantity[quantity : Quantity[_MixedRadix, MixedRadix[units__], ___]] := With[{order = getOrdering[{units}]}, 	
		Module[{remainder = quickUnmix[quantity], results = {}}, 
			Map[
				With[{uc = fastTrackConvert[remainder, {units}[[#]]]}, 
					If[
						Length[results] < (Length[order] - 1), 
						AppendTo[results, getIntegerPart[uc]]; remainder = getFractionalPart[uc], 
						AppendTo[results, uc]
					]
				] &,
				order];
			MixedRadix@@(First/@results)
		]
]
      
iNormalizeMixedRadixQuantity[___]:=$Failed

fixIntervalResults[list:{Quantity[Interval[{_,_}],__]..}]:=With[{low=list[[All,1,1,1]],high=list[[All,1,1,2]]},
	With[{mag=Interval[{MixedRadix@@low,MixedRadix@@high}],unit=MixedRadix@@QuantityUnit[list]},
	If[ValidUnitQ[Quantity[mag,unit]],Quantity[mag,unit],$Failed]]
]
fixIntervalResults[___]:=$Failed

(*symbols was excised from System`, currently only used by QA for testing*)
QuantityAlternatives[(quant_Quantity)?ValidUnitQ]/;hasCurrencyQ[quant]:={quant}
QuantityAlternatives[(quant_Quantity)?ValidUnitQ]:=If[
		Not[MemberQ[{Power,Times},Head[toAlphaUnitPart[QuantityUnit[quant]]]]],
		If[Head[QuantityUnit[quant]]===MixedRadix,
			kuba[toAlphaQuantity[unmixradixquantity[quant]]],
			kuba[toAlphaQuantity[quant]]],
		Module[{res=UnitSimplify[quant]},
			If[res===quant,{quant},{quant,res},Message[QuantityAlternatives::failed];$Failed]],
		Message[QuantityAlternatives::failed];$Failed
	]
	
predictionsSubstitutions = {"GregorianYears"->"Years"};

SetAttributes[UnitPrimaryDestinations,HoldFirst];
UnitPrimaryDestinations[u_HoldForm] := Module[{res=QuantityUnits`Private`PrimaryDestinationLookup[u]/.predictionsSubstitutions},
		If[
		ListQ[res],
		manageHeldForm/@res,
		{},
		{}]]
UnitPrimaryDestinations[u:Except[_HoldForm]]:=UnitPrimaryDestinations[HoldForm[u]]

QuantityPrimaryDestinationFunction[Quantity[_,unit_,___]?ValidUnitQ]:=UnitPrimaryDestinations[u]
QuantityPrimaryDestinationFunction[___]:={}

CalculateUnits`UnitCommonSymbols`UnitExpressionToQuantityUnitsExpression["PureUnities"]=1;

manageHeldForm[unit_HoldForm]:=With[{uh = ReleaseHold[unit]}, If[HoldForm[uh] === unit,uh,unit,unit]]	
manageHeldForm[other___]:=other

(*fastest method available to push mixed-radix quantities to single-unit quantities*)
unmixradixquantity[q:Quantity[l_List,u:{_CalculateParse`Content`Calculate`Unit..},opts___?OptionQ]]:=Total@(Quantity[Sequence@@#]&/@Transpose[{l, u}])
unmixradixquantity[q:Quantity[(_MixedRadix|_Interval),_MixedRadix,___?OptionQ]]/;MixedRadixQ[q]:=umrq[q]
unmixradixquantity[q:Quantity[_List,unit_List,opts___?OptionQ]]/;MixedRadixQ[q]:=unmixradixquantity/@(Quantity[#,unit,opts]&/@QuantityMagnitude[q])
unmixradixquantity[something__]:=something

(*unmix mixed-radix quantity*)
umrq[(q : Quantity[mag_MixedRadix, units_MixedRadix])?MixedRadixQ] := 
 With[{qs = Quantity[Sequence @@ #] & /@ Transpose[{List @@ mag, List @@ units}]}, 
  With[{u = Last[units]}, 
   Quantity[Total@QuantityMagnitude[UnitConvert[#, u] & /@ qs], u]]]
umrq[(q : Quantity[Interval[{lower_MixedRadix,upper_MixedRadix}], unit:MixedRadix[u_,l__]])?MixedRadixQ] :=With[{
	v1 = QuantityMagnitude[UnitConvert[Quantity[lower, unit], u]],
	v2 = QuantityMagnitude[UnitConvert[Quantity[upper, unit], u]]}, 
	Quantity[Interval[{v1, v2}], u]]
umrq[other___]:=other

(*kuba calls KnownUnitBasicAlgorithm in the Alpha code, used in QuantitySimplify and on 'wolfing' units*)
kuba[quantity_]/;Not[QuantityTensorQ[quantity]]:=Quiet[DeleteCases[fromAlphaQuantity/@AlphaBlock[Module[
	{res=DeleteCases[CalculateUnits`UnitCommonSymbols`KnownUnitBasicAlgorithm[quantity],Quantity[x_, p_Part, ___], Infinity]},
		If[Head[res]===List,If[res==={},{quantity},res,Message[kuba::failed];$Failed],{quantity},Message[kuba::failed];$Failed]
	]],_UnknownQuantity]]
	
kuba[q_]/;QuantityTensorQ[q]:={fromAlphaQuantity[q]}

SetAttributes[UnitSimplify,Listable];
Options[UnitSimplify]={UnityDimensions->{}};

UnitSimplify[n_?NumericQ]:=n
UnitSimplify[(quan:Quantity[_,unit_,___?OptionQ])?ValidUnitQ]:= If[MemberQ[iUSFCRP,unit],
	iUnitConvert[quan,unit/.iUSFCR],
	With[{q=iSimplifyQuantity[quan]},altQR[q]]]
UnitSimplify[(quan:Quantity[_,unit_,___?OptionQ])?ValidUnitQ,OptionsPattern[]]:=With[
	{res=Switch[Check[OptionValue[UnityDimensions],$Failed],
	Automatic,
	With[{r=nonDimensionalize[quan]},UnitSimplify[r]],
	{},
	UnitSimplify[quan],
	{_String..},
	With[{r=nonDimensionalize[quan,OptionValue[UnityDimensions]]},UnitSimplify[r]],
	_,
	Message[UnitSimplify::udim,OptionValue[UnityDimensions]];$Failed
	]},res/;res=!=$Failed]
UnitSimplify[ts_TemporalData] := unitSimplifyTimeSeries[ts]
(*UnitSimplify[a_Association] := Map[unitSimplifyAssociation, a] /; AssociationQ[a]*)
UnitSimplify[_,args:Except[__?OptionQ]]:=(Message[UnitSimplify::argx,UnitSimplify,Length[{args}]+1];Null/;False)
UnitSimplify[]:=(Message[UnitSimplify::argx,UnitSimplify,0];Null/;False)

unitSimplifyAssociation[q_?QuantityQ] := UnitSimplify[q]
unitSimplifyAssociation[q_TemporalData] := UnitSimplify[q]
(*unitSimplifyAssociation[sa_StructuredArray] := StructuredArrayUnitSimplify[sa]*)
unitSimplifyAssociation[list_List] := Map[unitSimplifyAssociation[#]&, list]
unitSimplifyAssociation[a_Association] := Map[unitSimplifyAssociation[#]&, a] /; AssociationQ[a]
unitSimplifyAssociation[other_,___] := other

unitSimplifyTimeSeries[series_?TemporalData`StructurallyValidTemporalDataQ] :=
 Module[{states = series["ValueList"], vals, res},
  vals = UnitSimplify[states];(* validate result *)
  res = TemporalData`ReplaceStates[series, vals];
  If[TemporalData`StructurallyValidTemporalDataQ[res],
   res, Message[UnitSimplify::tsq, series]$Failed
   ]]
unitSimplifyTimeSeries[___] := $Failed

nonDimensionalize[q_Quantity,dimensions___] := 
 With[{ndim = constructnonDimensionalizer[UnitDimensions[q],dimensions]}, 
  With[{r = Quiet[Times[ndim, q]]}, 
   If[Quiet[Or[QuantityQ[r], NumericQ[r]]], r, q, q]]]
   
nonDimensionalize[x_,___] := x

constructnonDimensionalizer[dims_List, dimensions_:$nonDimensionalDimensions] := 
 With[{dimensionless = Cases[dims, {d_String, _?NumericQ} /; MemberQ[dimensions, d]]}, 
  Times @@ (With[{u = Power[Internal`DimensionToBaseUnit[#[[1]]], -#[[2]]]}, 
        Quantity[1, u]] & /@ dimensionless)]

constructnonDimensionalizer[___] := $Failed

$nonDimensionalDimensions = {"AngleUnit", "SolidAngleUnit"};

(*simplify compound unit expressions and check for complex components*)
iSimplifyQuantity[q:(Quantity[mag_, unit_, opts___?OptionQ])] := 
   	Module[{res},
   		res = With[{un = PowerExpand[unit]}, Quantity[mag, un, opts]];
   		If[Or[ValidUnitQ[res],NumericQ[res]], 
   			res, 
   			With[{fixed=checkForI[res]},
   				If[Or[ValidUnitQ[fixed],NumericQ[res]],
   					fixed,
   					q
   				]]
   		]]
iSimplifyQuantity[x___]:=x

(*used to remove complex components from unit part*)
checkForI[Quantity[mag_,unit_,opts___?OptionQ]]:=Module[{num,denom},
	num=Cases[Numerator[unit],_Complex,Infinity];
	denom=Cases[Denominator[unit],_Complex,Infinity];
	num=If[num==={},1,Times@@num];denom=If[denom==={},1,Times@@denom];
	With[{m=mag*(num/denom),u=DeleteCases[unit,_Complex,Infinity]},Quantity[m,u,opts]]]
checkForI[___]:=$Failed

(*look at alternative quantities and find best fit*)
iQuantityReduce[quan_Quantity]:= 
 Module[{ul, un, qe}, 
  ul = UnitPrimaryDestinations[QuantityUnit[quan]];
  un = If[Length[ul] > 0, First[ul], 
  		If[hasCurrencyQ[quan]||MixedRadixQ[quan](*too many calls to FinancialData*),
   			{},
   			Quiet[QuantityUnit[iFilterUnits[quan]]](*choose best fitting unit from alternatives*)
   		], 
   $Failed];
  If[Head[un]=!=QuantityUnit,
  	If[un==="1"||un===1||un==={},
  		If[oUnitDimensions[quan]==={"DimensionlessUnit"->1},QuantityExpand[quan],Sqrt[quan^2]],(*alternative simplification*)
  		If[With[{u=un},Not[KnownUnitQ[u]]],
  			$Failed,
  			UnitConvert[quan, un],
  			$Failed]],
  	If[oUnitDimensions[qe=QuantityExpand[quan]]==={"DimensionlessUnit" -> 1},qe,quan,$Failed],
  	$Failed]]
  	
(*uses GenericConversions to try and simplify quantity, picks out output with least complex unit component*)
iFilterUnits[q_Quantity] := 
 With[{u = QuantityUnit[q]}, Module[
   {res = gclist[q], sunits, qus, tus},
   If[res==={},{},
   sunits = iSelectLeastComplexUnits[QuantityUnit[res]];(*choose best fitting unit from alternatives*)
   If[TrueQ[Length[sunits] === 1],
    First[Select[res, QuantityUnit[#] == First[sunits] &]],
    qus = iGetUnitSystem[toAlphaUnit[u]];
    tus = 
     With[{tu = QuantityUnit[#]}, 
        iGetUnitSystem[toAlphaUnit[tu]]] & /@ 
      res;
    fromAlphaQuantity[Part[res, iSecondaryUnitSelection[qus, tus]]](*break ties based on unit system of input*)
    ]]]]

(*check iQuantityReduce result against input, return whichever is lest complex*)
altQR[q_Quantity] := 
 Module[{qr = iSimplifyQuantity[iQuantityReduce[q]], 
   l1 = exp2list[QuantityUnit[q]], l2}, 
  l2 = exp2list[QuantityUnit[qr]]; 
  If[Length[l2] >= Length[l1], q, qr,q]]
altQR[arg_] := arg

(*take a list of input unit expressions and select the one with fewest terms*)
iSelectLeastComplexUnits[units_List] := 
 Module[{exps = exp2list[{#}] & /@ units, sort, 
   longest, texps}, sort = Sort[exps, Length[#1] < Length[#2] &]; 
  longest = Length[First[sort]]; 
  texps = Select[sort, Length[#] == longest &]; 
  Part[units, Flatten[Position[exps, #] & /@ texps]]]

(*look up UnitSystems associated with the input unit expression*)
iGetUnitSystem[unit_CalculateParse`Content`Calculate`Unit] := 
 With[{r1 = UnitLookup[unit[[1]], "UnitSystem"]}, 
  If[Head[r1] =!= CalculateUnits`UnitCommonSymbols`UnitLookup, r1, 
   Intersection[
    Sequence @@ (UnitLookup[#, "UnitSystem"] & /@ exp2list[unit])]]]
    
(*used to break ties in unit selection based on unit system(s) of possible units*)
iSecondaryUnitSelection[qus_List, tus_List] := 
 With[{intersections = Intersection[qus, #] & /@ tus}, 
  Module[{sort, longest, si}, 
   sort = Sort[intersections, Length[#1] > Length[#2] &]; 
   longest = First[sort]; 
   If[longest =!= {}, First@Flatten[Position[intersections, longest]], 
    si = Select[tus, MemberQ[#, "SI"] &]; 
    If[si =!= {}, First@Flatten[Position[tus, First[si]]], 1]]]]

(*private alias to UnitConvert[_,"Base"*)
SetAttributes[QuantityExpand,Listable]
QuantityExpand[n_?NumericQ]:=n
QuantityExpand[(quan_Quantity)?ValidUnitQ]:= With[{res=UnitConvert[quan,"Base"]},
	If[ValidUnitQ[res],
		res,
		iSimplifyQuantity[checkExpandedQuantity[res]]]]

(*handle some edge-cases to ensure output unit is valid*)
checkExpandedQuantity[q:(Quantity)[mag_,unit_,opts___?OptionQ]]:=If[KnownUnitQ[unit], q, 
 Module[{components = 
    Cases[exp2list[unit], 
     x_ /; Not[KnownUnitQ[x]]], np, 
   dp}, {np, 
    dp} = {Cases[Numerator[unit], 
     Power[u_, _] /; MemberQ[components, u], Infinity], 
    Cases[Denominator[unit], Power[u_, _] /; MemberQ[components, u], 
     Infinity]};
  With[{u = PowerExpand[unit /. (Rule[#, 1] & /@ components)], 
    m = (mag*Times @@ np)/Times @@ dp},
   Quantity[m, u]]]]
checkExpandedQuantity[res___]:=res

(*use Alpha's UnitClique to pick out desired units from input list*)
gettargetUnit[unitlist:{_Quantity..}]/;Not[MemberQ[unitlist,$Failed]]/;Length[unitlist]>1 := 
Quiet@AlphaBlock[With[{units=unitlist[[All,2,1]]},
 	QuantityUnit[Quantity[1,UnitClique[Sequence @@units]]
 		]]]
gettargetUnit[unitlist:{_Quantity..}]/;Not[MemberQ[unitlist,$Failed]]/;Length[unitlist]===1 := 
QuantityUnit[First@unitlist]
gettargetUnit[unitlist:{_Quantity..}]/;MemberQ[unitlist,$Failed]:=gettargetUnit[Cases[unitlist,Except[$Failed]]]
gettargetUnit[l:{_?KnownUnitQ..}]/;Length[Union[l]]===1:=First[l]
gettargetUnit[l:{_?KnownUnitQ..}]:=gettargetUnit[Quantity[1,#]&/@l]
gettargetUnit[x__]:=x

iGatherAndConvertByDimension[list:{_?QuantityOrQuantityListQ..}]:=Catch[
	With[
		{grouped=GatherBy[Flatten[list],UnitDimensions]},
		With[{rules=Flatten[Check[Thread[Rule[#,StandardizeQuantities[#]]],Throw[$Failed,$tag]]&/@grouped]},list/.rules]],$tag]
iGatherAndConvertByDimension[series_TemporalData] := commonUnitsTimeSeries[series]
iGatherAndConvertByDimension[a_Association] := commonUnitsAssociation[a]
iGatherAndConvertByDimension[___]:=$Failed

commonUnitsTimeSeries[series_?TemporalData`StructurallyValidTemporalDataQ] :=
 Module[{states = series["ValueList"], vals, res},
  vals = CommonUnits[states];(* validate result *)
  res = TemporalData`ReplaceStates[series, vals];
  If[TemporalData`StructurallyValidTemporalDataQ[res],
   res, Message[CommonUnits::tsq, series];$Failed
   ]]
commonUnitsTimeSeries[___] := $Failed

commonUnitsAssociation[a_?AssociationQ] := Module[{res, quantities = Cases[a,HoldPattern[q_Quantity]/;QuantityQ[q],{1,Infinity}]},
	If[quantities === {},
		a,
		res = iGatherAndConvertByDimension[quantities];
		If[UnsameQ[res, $Failed] && SameQ[Length[res], Length[quantities]],
			a/.Thread[Rule[quantities,res]],
			$Failed
		]
	]
]
commonUnitsAssociation[___] := $Failed

CommonUnits[qa_StructuredArray?QuantityArray`QuantityArrayQ] := QuantityArray`QuantityArrayCommonUnits[qa]
CommonUnits[args_]:=With[{res=Quiet[iGatherAndConvertByDimension[args]]},res/;Quiet[res=!=$Failed]]
CommonUnits[_,args__]:=(Message[CommonUnits::argx,CommonUnits,Length[{args}]+1];Null/;False)
CommonUnits[]:=(Message[CommonUnits::argx,CommonUnits,0];Null/;False)
StandardizeQuantities[args__]:=With[{res=iStandardizeQuantities[args]},res/;res=!=$Failed]
(*works only on compatible units*)
iStandardizeQuantities[l:{_?NumericQ..}]:=l
iStandardizeQuantities[l:{{_?NumericQ..}..}]:=l
iStandardizeQuantities[l:{_?NumericQ..},"DimensionlessUnit"]:=l
iStandardizeQuantities[l:{{_?NumericQ..}..},"DimensionlessUnit"]:=l
iStandardizeQuantities[l:{_?NumericQ...,_Quantity...,___}]/;Union[UnitDimensions/@l] === {{}} := UnitConvert[l]
iStandardizeQuantities[l_List,"DimensionlessUnit"]:=StandardizeQuantities[l,"PureUnities"]
iStandardizeQuantities[l:{Quantity[_,unit_,___?OptionQ]..}]:=l
iStandardizeQuantities[l:{Quantity[_,unit_,___?OptionQ]..},unit_]:=l
iStandardizeQuantities[l_List,target_]/;AllQuantityQ[l]/;Or@@(MixedRadixQ/@l):=iStandardizeQuantities[unmixradixquantity/@l,target]
iStandardizeQuantities[l_List]/;AllQuantityQ[l]/;Or@@(MixedRadixQ/@l):=iStandardizeQuantities[unmixradixquantity/@l]
iStandardizeQuantities[list_List]/;AllQuantityQ[list]/;Length[Union[QuantityUnit[list]]]===1:=list
iStandardizeQuantities[list_List]/; AllQuantityQ[list] /; CompatibleUnitQ[Union[QuantityUnit[list]]]:=Catch[
Module[{units=Union[QuantityUnit[list]],tu},
	tu=gettargetUnit[units];
	Table[(If[#===$Failed,Throw[$Failed,$tag],#]&[UnitConvert[i,tu]]),{i,list}]
],$tag]
iStandardizeQuantities[list_List]/;AllQuantityQ[list]:= 
Catch[Module[{un=gettargetUnit[list[[All, 2]]],res,resh},
 res=Table[(If[#===$Failed,Throw[$Failed,$tag],#]&[UnitConvert[i,un]]),{i,list}];
 resh=Union[QuantityUnit[res]];
 If[TrueQ[Length[resh]==1&&Head[resh]==List],res,$Failed]
 ],$tag]
iStandardizeQuantities[list_, Automatic]:=StandardizeQuantities[list]
iStandardizeQuantities[list_List,unit:(_Power|_Times|_String|_IndependentUnit|_Divide)]/;AllQuantityQ[list]/;KnownUnitQ[unit] := 
Catch[Module[{mult,res,resh},
	If[
		And[Length[Union[QuantityUnit[list]]]===1,CompatibleUnitQ[QuantityUnit[First[list]],unit],Not[isTemperatureQ[First[list]]]],
			mult=With[{u=QuantityUnit[First[list]]},QuantityMagnitude[UnitConvert[Quantity[1,u],unit]]];
			Quantity[mult*#,unit]&/@QuantityMagnitude[list],
 			res=Table[(If[#===$Failed,Throw[$Failed,$tag],#]&[UnitConvert[i,unit]]),{i,list}];
 			resh=Union[QuantityUnit[res]];
 			If[TrueQ[Length[resh]==1&&Head[resh]==List],res,$Failed],
 			$Failed]
 ],$tag]
iStandardizeQuantities[{list:_List..}]:=Catch[
	If[
		Not[CompatibleUnitQ[Flatten[{list}]]],
		iFindInCompatibleUnits[Sequence@@Flatten[{list}]];Throw[$Failed,$tag]
	];
	With[{target=gettargetUnit[Flatten[QuantityUnit[{list}]]]},
	Table[(If[#===$Failed,Throw[$Failed,$tag],#]&[StandardizeQuantities[i,target]]),{i,{list}}]],$tag
]
iStandardizeQuantities[{list:_List..},target:(_String|_Power|_Times|_Divide)]/;KnownUnitQ[target]:=Catch[
	If[
		Not[CompatibleUnitQ[Flatten[{list}]]],
		iFindInCompatibleUnits[Sequence@@Flatten[{list}]];Throw[$Failed,$tag]
		];
	Table[(If[Quiet[MatchQ[#,_StandardizeQuantities]],Throw[$Failed,$tag],#]&[StandardizeQuantities[i,target]]),{i,{list}}],$tag
]
iStandardizeQuantities[l_List] :=  l /; VectorQ[Flatten[l], NumericQ]
iStandardizeQuantities[l_List, "DimensionlessUnit"] :=  l /; VectorQ[Flatten[l], NumericQ] 
iStandardizeQuantities[(q_Quantity)?QuantityTensorQ]:=q
iStandardizeQuantities[(q:Quantity[_,unit_,___])?QuantityTensorQ,unit_]:=q
iStandardizeQuantities[(q:Quantity[m_,unit_,opts___?OptionQ])?QuantityTensorQ,target_]/;CompatibleUnitQ[unit,target]:=With[
	{ratio=QuantityMagnitude[UnitConvert[Quantity[2,unit],target]]-QuantityMagnitude[UnitConvert[Quantity[1,unit],target]],
	constant = QuantityMagnitude[UnitConvert[Quantity[0,unit],target]]},(*handle temperature and non-zero based units*)
	Quantity[constant+(m*ratio),target,opts]
]
iStandardizeQuantities[l_List]/;AllQuantityQ[l]:=l
iStandardizeQuantities[___]=$Failed;

(*used to find appropriate unit associated with multiple Quantity expressions*)
CanonicalUnits[(quantity__Quantity)?ValidUnitQ]:=CanonicalUnits[{quantity}]
CanonicalUnits[(quantity__Quantity)?ValidUnitQ,"BestFit"]:=CanonicalUnits[{quantity},"BestFit"]
CanonicalUnits[{(quantities__Quantity)?ValidUnitQ}]/;Length[Union[QuantityUnit[{quantities}]]]===1:=ReleaseHold[QuantityUnit[First[{quantities}]]]
CanonicalUnits[{(quantities__Quantity)?ValidUnitQ}]:=Module[{cn=UnitConvert[{quantities},"Base"],unit},unit=Union[QuantityUnit[cn]];If[Length[unit]===1,ReleaseHold[First@unit],$Failed,$Failed]]
CanonicalUnits[{(quantities__Quantity)?ValidUnitQ},"BestFit"]:=Module[{cn=UnitConvert[{quantities},"Base"],unit},unit=Union[QuantityUnit[cn]];If[Length[unit]===1,gettargetUnit[{quantities}[[All,2]]],$Failed,$Failed]]
CanonicalUnits[___]:=$Failed

(*overloading Alpha functions to utilize FinancialData*)
CalculateUnits`UnitCommonSymbols`UnitLookup[CalculateParse`Content`Calculate`Unit[unit_],args___]/;$AlphaFlag=!=True:=UnitLookup[unit,args]
CalculateParse`Content`Calculate`GeneralData[FinancialData,args_, args2___]/;args2=!="Last":=Quiet[iGetExchangeRate[args,args2],{FinancialData::notent}]
CalculateParse`Content`Calculate`GeneralData[FinancialData,args_, "Last"]:=Quiet[
	If[Head[args]=!=Internal`DataList,
		iGetExchangeRate[args],
		iGetExchangeRate/@List@@args],
			{FinancialData::notent}]
CalculateScan`CommonSymbols`FromTemplateBox[args___]:=TemplateBox[args]
CalculateUnits`UnitCommonSymbols`ExchangeRateToUSD[cur_]:=iGetExchangeRate[{ToString[cur],"USD"}]
CalculateParse`Content`Calculate`GeneralData[FinancialData,args___]:=True

(*currencies that don't play well with FinancialData*)
$MoneyCases={"ZWD","SVC", "GHC", "SRG","TMT", "VEF","EEK","BTC"}

(*modularized exchange rate manager*)
If[Not[ValueQ[$UserExchangeRateTable]],$UserExchangeRateTable={}];
iGetExchangeRate[l_List]:=If[
	MemberQ[$UserExchangeRateTable[[All,1]],First[l]],
	First[l]/.$UserExchangeRateTable,
	Which[$DynamicCurrencyConversion===False,
		iRetrieveExchangeRate[l],
		$DynamicCurrencyConversion===True,
		iGetFinancialData[l],
		True,First[l]<>"/USD"]]
iGetFinancialData[{elem_String,"USD"}]/;MemberQ[$MoneyCases,elem]:=elem<>"/USD"
iGetFinancialData[l_List]:=With[{res=FinancialData[l]},If[res===Missing["NotAvailable"],1/FinancialData[Reverse[l]],res,res]]
iSetExchangeRateFromUSD[cur_String,rate_?NumericQ]:=If[
	Length[$UserExchangeRateTable]>0,$UserExchangeRateTable=Prepend[DeleteCases[$UserExchangeRateTable,Rule[cur,_]],cur->rate],
	$UserExchangeRateTable={cur->rate}]
iClearExchangeRateFromUSD[cur_String]:=$UserExchangeRateTable=DeleteCases[$UserExchangeRateTable,Rule[cur,_]]
iRetrieveExchangeRate[{s_String,"USD"}]:=(s/.$UserExchangeRateTable)/.st_String:>(st<>"/USD")

(*currencies that misreport their "CurrencyCode", so need to be handled appropriately*)
$badCC={"AustrianSchillings", "BelgianFrancs", "CyprusPounds", "FinnishMarkkas", "FrenchFrancs", "GermanMarks", "GoldPounds", 
"GoldTons", "GreekDrachmas", "IrishPounds", "ItalianLire", "LuxembourgFrang", "MalteseLiri", "NetherlandsGuilders", 
"PortugueseEscudos", "RomanianLeiOld", "SilverPounds", "SilverTons", "SpanishPesetas"};
iCurrencyCodeLookup[c_String]/;MemberQ[$badCC,c]:=CalculateUnits`UnitCommonSymbols`UnitLookup[]
iCurrencyCodeLookup[c_String]:=CalculateUnits`UnitCommonSymbols`UnitLookup[c/.$UnitReplacementRules,"CurrencyCode"]

(*used to overwrite/set exchange-rate information*)
UnitExchangeRate::nocur = "`1` is not a known primary currency unit.";
SetAttributes[UnitExchangeRate,Listable];
UnitExchangeRate[All]:=$UserExchangeRateTable
UnitExchangeRate[Rule[All,Automatic]]:=$UserExchangeRateTable={};
UnitExchangeRate[currency_String]/;hasCurrencyQ[currency]:=With[{cc=iCurrencyCodeLookup[currency]},cc/.$UserExchangeRateTable]
UnitExchangeRate[Rule["USDollars",___]]:=$Failed
UnitExchangeRate[Rule["USCents",___]]:=$Failed
UnitExchangeRate[Rule[currency_String,rate_?NumericQ]]/;hasCurrencyQ[currency]:=(With[{cc=iCurrencyCodeLookup[currency]},If[Head[cc]=!=CalculateUnits`UnitCommonSymbols`UnitLookup,iSetExchangeRateFromUSD[cc,rate];Rule[cc/"USD",rate],Message[UnitExchangeRate::nocur,currency];$Failed,$Failed]])
UnitExchangeRate[Rule[currency_String,Automatic]]/;hasCurrencyQ[currency]:=(With[{cc=iCurrencyCodeLookup[currency]},If[Head[cc]=!=CalculateUnits`UnitCommonSymbols`UnitLookup,iClearExchangeRateFromUSD[cc];,Message[UnitExchangeRate::nocur,currency];$Failed,$Failed]])
UnitExchangeRate[_,args__]:=(Message[UnitExchangeRate::argx,UnitExchangeRate,Length[{args}]+1];Null/;False)
UnitExchangeRate[]:=(Message[UnitExchangeRate::argx,UnitExchangeRate,0];Null/;False)

Internal`QuantityToValue[args__] := Block[{DatedUnit},
	DatedUnit[u_,n_?Integer]:=DatedUnit[u,{n}]; (*catch syntatic differences on same date*)
	With[{res = iQuantityToValue[args]}, res]
]
Options[iQuantityToValue] = {System`TargetUnits -> Automatic,"Compatibility"->All};
Internal`QuantityToValue::invld = "Invalid Quantity or TargetUnit specification";
iQuantityToValue[sa_StructuredArray, options___?OptionQ] := Which[
	QuantityArray`QuantityArrayQ[sa],
		QuantityArray`Private`getData[sa, options],
	True,
		iQuantityToValue[Normal[sa, StructuredArray], options]
];
iQuantityToValue[arg_,args___]/;Not[FreeQ[arg,MixedRadix]] := iQuantityToValue[arg/.q:Quantity[_,_MixedRadix]:>UnitSimplify[q],args]
iQuantityToValue[arg_,TargetUnits->q_Quantity]:=Catch[With[{qu=Check[QuantityUnit[q],Throw[$Failed,$tag]]},If[Head[qu]=!=QuantityUnit,iQuantityToValue[arg,TargetUnits->qu],$Failed,$Failed]],$tag]
iQuantityToValue[l_List]/;FreeQ[l,Quantity]:={l,"DimensionlessUnit"}
iQuantityToValue[l:{_List..}]/;FreeQ[l,Quantity]:={l,"DimensionlessUnit"}
iQuantityToValue[l_List,System`TargetUnits->Automatic]/;FreeQ[l,Quantity]:={l,"DimensionlessUnit"}
iQuantityToValue[n_?NumericQ..]:={n,"DimensionlessUnit"}
iQuantityToValue[Quantity[list_List,unit_?KnownUnitQ,ThreadDepth->0]] := {list,unit}
iQuantityToValue[q:(Quantity[Except[_MixedRadix], Except[_MixedRadix], ___?OptionQ])?ValidUnitQ] := Through[{QuantityMagnitude, QuantityUnit}[q]]
iQuantityToValue[(q:Quantity[_MixedRadix,_MixedRadix, ___?OptionQ])?ValidUnitQ] := With[{quant=unmixradixquantity[q]},{QuantityMagnitude[quant], QuantityUnit[quant]}]
iQuantityToValue[(q:Quantity[Except[_MixedRadix],unit:Except[_MixedRadix], ___?OptionQ])?ValidUnitQ,System`TargetUnits -> t_?KnownUnitQ] := {QuantityMagnitude[UnitConvert[q, t]], t}
iQuantityToValue[(q:Quantity[Except[_MixedRadix], unit:Except[_MixedRadix], ___?OptionQ])?ValidUnitQ, System`TargetUnits -> t_String] /; Not[KnownUnitQ[t]] := Catch[With[{u = 
     Check[QuantityUnit[Quantity[t]], 
      Throw[$Failed,$tag]]}, {QuantityMagnitude[UnitConvert[q, u]], u}],$tag]
iQuantityToValue[(q:Quantity[_MixedRadix,_MixedRadix, ___?OptionQ])?ValidUnitQ,System`TargetUnits -> t_?KnownUnitQ] := With[{quant=unmixradixquantity[q]},Internal`QuantityToValue[quant,System`TargetUnits->t]]
iQuantityToValue[(q:Quantity[_MixedRadix, _MixedRadix, ___?OptionQ])?ValidUnitQ, System`TargetUnits -> t_String] /; Not[KnownUnitQ[t]] := Catch[With[{u = Check[QuantityUnit[Quantity[t]], Throw[$Failed,$tag]]}, 
   With[{quant = unmixradixquantity[q]}, 
    Internal`QuantityToValue[quant, System`TargetUnits -> u]]],$tag]
iQuantityToValue[q : {Quantity[_, unit_]..}] /; KnownUnitQ[unit] := {QuantityMagnitude[q], QuantityUnit[Quantity[1,unit]]}
iQuantityToValue[q : {__Quantity}] /;Internal`SameUnitDimension[q] := 
 With[{sq = StandardizeQuantities[q]}, {QuantityMagnitude[sq], QuantityUnit[First@sq]}]
iQuantityToValue[q : {_Quantity..}, System`TargetUnits -> t_?KnownUnitQ] /; With[{l=Append[q,Quantity[1,t]]},Internal`SameUnitDimension[l]] := 
	With[{sq=StandardizeQuantities[q, t]}, {QuantityMagnitude[sq], t}]
iQuantityToValue[q : {_Quantity ..}, System`TargetUnits -> t_String] /;Not[KnownUnitQ[t]] /; With[{l = Append[q, Quantity[1, t]]}, Internal`SameUnitDimension[l]] := Catch[With[{u = Check[QuantityUnit[Quantity[t]], Throw[$Failed,$tag]]}, 
   With[{sq = StandardizeQuantities[q, u]}, {QuantityMagnitude[sq], u}]],$tag]
iQuantityToValue[q : {_Quantity..}, System`TargetUnits -> Automatic] /; Internal`SameUnitDimension[q] := 
	{QuantityMagnitude[#],QuantityUnit[First[#]]}&[StandardizeQuantities[q, Automatic]]
iQuantityToValue[(s__Quantity)?ValidUnitQ, opts___?OptionQ] := iQuantityToValue[{s}, opts]
iQuantityToValue[l:{(_Quantity)?ValidUnitQ..}]:=(iFindInCompatibleUnits[Sequence@@l];$Failed)
iQuantityToValue[l:{(_Quantity)?ValidUnitQ..},opts___?OptionQ]:=If[
	UnsameQ[All,"Compatibility"/.Flatten[{opts}]/.Options[iQuantityToValue]],
	iQuantityToValue[l,Sequence@@DeleteCases[{opts},HoldPattern[Rule["Compatibility",_]]]],
	With[{targ=(System`TargetUnits/.Flatten[{opts}]/.Options[iQuantityToValue])},
	(iFindInCompatibleUnits[Sequence@@If[targ=!=Automatic,Prepend[l,Quantity[1,targ]],l]];$Failed)]]
iQuantityToValue[m_?MixedQuantityMatrixQ,opts___?OptionQ]:=Switch[
	("Compatibility"/.Flatten[{opts}]/.Options[iQuantityToValue]),
	All,iQuantityMatrixToValue[m,opts],
	"Columnwise",iQuantityColumnwiseMatrixToValue[m,opts],
	"Rowwise",iQuantityRowwiseMatrixToValue[m,opts],
	_,Message[Internal`QuantityToValue::invld];$Failed
]
iQuantityToValue[a_?ArrayQ,opts___?OptionQ]:=Catch[If[VectorQ[a],Throw[$Failed,$tag]];Module[{r,units},
        r=Table[If[#===$Failed, Throw[$Failed,$tag], #]&[iQuantityToValue[i,opts]], {i, a}];
        With[{pt=Append[ConstantArray[All,ArrayDepth[r]-1],2]},
        	units=Flatten[Part[r,Sequence@@pt]];
        	If[CompatibleUnitQ[units],
        		If[Length[Union[units]]===1,
        			{Part[r,Sequence@@Drop[pt,-1],1],First@units},
        			iQuantityToValue[a,System`TargetUnits->First[units]]
        	],
        	iFindInCompatibleUnits[units];$Failed]
        ]],$tag]
iQuantityToValue[___] := (Message[Internal`QuantityToValue::invld]; $Failed)

Options[iQuantityMatrixToValue] = {System`TargetUnits -> Automatic,"Compatibility"->All}
iQuantityMatrixToValue[mat_?MixedQuantityMatrixQ,opts___?OptionQ]:=With[
	{res=If[(System`TargetUnits/.Flatten[{opts}]/.Options[iQuantityToValue])=!=Automatic,
		StandardizeQuantities[mat,(System`TargetUnits/.Flatten[{opts}]/.Options[iQuantityToValue])],
		StandardizeQuantities[mat]]},
		If[Quiet[MatchQ[res,_?MixedQuantityMatrixQ]],{QuantityMagnitude[#], First[Flatten[QuantityUnit[#]]]}&[res],$Failed]]
iQuantityMatrixToValue[___]:=(Message[Internal`QuantityToValue::invld]; $Failed)

Options[iQuantityColumnwiseMatrixToValue] = {System`TargetUnits -> Automatic,"Compatibility"->All}
iQuantityColumnwiseMatrixToValue[mat_?MixedQuantityMatrixQ,opts___?OptionQ]:=Catch[
	With[{tu=System`TargetUnits/.Flatten[{opts}]/.Options[iQuantityToValue]},
		Module[{res,it=0,tlist=If[
			tu=!=Automatic,
			If[Length[tu]=!=Length[mat[[1]]],
				Message[Internal`QuantityToValue::invld];Throw[$Failed,$tag],
				tu],
			ConstantArray[Automatic,Length[mat[[1]]]]]},
	res=Table[it++;If[Quiet[MatchQ[#,{_List,_?KnownUnitQ}]],#,Throw[$Failed,$tag]]&[Internal`QuantityToValue[i,System`TargetUnits->tlist[[it]]]],{i,Transpose[mat]}];
	{Transpose[res[[All,1]]],res[[All,2]]}]],$tag]
iQuantityColumnwiseMatrixToValue[___]:=(Message[Internal`QuantityToValue::invld]; $Failed)

Options[iQuantityRowwiseMatrixToValue] = {System`TargetUnits -> Automatic,"Compatibility"->All}
iQuantityRowwiseMatrixToValue[mat_?MixedQuantityMatrixQ,opts___?OptionQ]:=Catch[
	With[{tu=System`TargetUnits/.Flatten[{opts}]/.Options[iQuantityToValue]},
		Module[{res,it=0,tlist=If[
			tu=!=Automatic,
			If[Length[tu]=!=Length[mat],
				Message[Internal`QuantityToValue::invld];Throw[$Failed,$tag],
				tu],
			ConstantArray[Automatic,Length[mat]]]},
	res=Table[it++;If[Quiet[MatchQ[#,{_List,_?KnownUnitQ}]],#,Throw[$Failed,$tag]]&[Internal`QuantityToValue[i,System`TargetUnits->tlist[[it]]]],{i,mat}];
	{res[[All,1]],res[[All,2]]}]],$tag]
iQuantityRowwiseMatrixToValue[___]:=(Message[Internal`QuantityToValue::invld]; $Failed)


scanForUnit[data_List] := 
 First[Cases[data, q_Quantity :> QuantityUnit[q], 1, 
    1] /. {} :> {"PureUnities"}]

replaceAutomatic[{data_List, unit_}] := 
 ReplaceAll[data, Automatic :> Quantity[Automatic, unit]]

preprocess[array_?MatrixQ] := With[{data = Transpose[array]},
  With[{units = scanForUnit /@ data}, 
   Transpose[replaceAutomatic /@ Transpose[{data, units}]]]]
preprocess[data_?VectorQ] := 
 With[{units = scanForUnit[data]}, replaceAutomatic[{data, units}]]

(*special version of QuantityToValue which allows for columnwise handling of 'Automatic' parameters*)
Internal`QuantityArrayToNumericArray[arg_,opts___]:=Internal`QuantityToValue[preprocess[arg],opts]

(*internal utility for taking matrices of values and units and threading them, not currently used*)
Internal`GenerateQuantityMatrix[args___]:=With[{res=iGenerateQuantityMatrix[args]},res/;res=!=$Failed]

iGenerateQuantityMatrix[mags_?MatrixQ, units_?MatrixQ] /; 
   And @@ (KnownUnitQ /@ Flatten[units]) /; 
  Dimensions[mags] === Dimensions[units] := 
 Block[{$IgnoreMalformedQuantities = True}, 
  Thread /@ Thread[Quantity[mags, units]]]

iGenerateQuantityMatrix[___]=$Failed

SimplifyUnits[unit_?KnownUnitQ]:=With[{res=UnitSimplify[Quantity[1,unit]]},
	If[
		QuantityMagnitude[res]=!=1,
		{False},
		{True,QuantityUnit[res]/."DimensionlessUnit"->"PureUnities"}]
]

SimplifyUnits[rules:{Rule[QuantityUnit[_],_?KnownUnitQ]..}]:=Catch[Module[{results},
	results=Table[
		With[{res=SimplifyUnits[Last[i]]},
			If[SameQ[res,{False}], Throw[{False},$tag]];
			Rule[First[i],Last[res]]]
		,{i,rules}];
		{True,results}
	],$tag]
	
SimplifyUnits[other___]:={False}
    
preferredUnit[l_List]:=With[{res=System`InflationAdjust`Private`PreferredTargetUnit[l]},
	If[KnownUnitQ[res],res,
		If[QuantityQ[res],
			QuantityUnit[res],
			"Base"]]]
preferredUnit[___]:="Base"
    
standardizeUnits[l_?Internal`QuantityVectorQ, unit_] := 
	If[unit === Automatic,
		If[hasDatedUnitQ[First[l]],
		With[{target=preferredUnit[l]},UnitConvert[l, target]],
		UnitConvert[l, QuantityUnit[First[l]]]],
		UnitConvert[l, unit]
		]

FindCurrencyUnitValue[currency_String] := 
 Catch[Block[{CalculateUnits`UnitCommonSymbols`ExchangeRateToUSD = 
     temp}, With[{res =
      CalculateUnits`UnitCommonSymbols`UnitLookup[
        currency /. $UnitReplacementRules, 
        "FundamentalUnitValue"] /. 
       $AlphaReplacementRules},
    If[MatchQ[res, _CalculateUnits`UnitCommonSymbols`UnitLookup], 
     Throw[$Failed, $tag]]; 
    Module[{cc = Cases[res, temp[x_] :> x]}, {If[cc == {}, 
       cc = {"USD"}, cc], 
      res /. {temp[_] -> 1, "USDollars" -> 1}}]]], $tag]
FindCurrencyUnitValue[___]:=$Failed

$DefaultPhysicalConstants = {"PlanckConstant", "BoltzmannConstant", 
   "ReducedPlanckConstant", "GravitationalConstant", "SpeedOfLight", 
   "AvogadroConstant", "ElectricConstant", "ElementaryCharge", "RydbergConstant", 
   "FaradayConstant", "FineStructureConstant", "MolarGasConstant", "MagneticConstant"};

makeRule[constant_String] := 
 Rule[constant, 
  IndependentUnit[constant]*QuantityUnit[UnitConvert[constant]]]

makeIdentityRule[constant_String] := Rule[IndependentUnit[constant], 1]

PhysicalConstantsFactor[dimensions_List, constants_List] := 
 Times @@ Map[
   With[{cases = Cases[dimensions, {#, n_} :> n]},
     If[cases =!= {}, PhysicalConstantValue[#]^First[cases], 1]] &, 
   constants]

Clear[preprocessQuantity];
preprocessQuantity[q : Quantity[v_, u_], 
  constants_List: $DefaultPhysicalConstants] := 
 If[Cases[HoldForm[u], Alternatives @@ constants, -1] =!= {}, 
  With[{unit = ReleaseHold[HoldForm[u] /. makeRule /@ constants]}, 
   With[{ud = UnitDimensions[Quantity[1, unit]], 
     outUnit = unit /. makeIdentityRule /@ constants}, 
    With[{vm = PhysicalConstantsFactor[ud, constants]}, 
     UnitConvert[Quantity[v*vm, outUnit]]]]], q]

Clear[preprocessQuantityExpression];
SetAttributes[preprocessQuantityExpression,HoldAll];
preprocessQuantityExpression[expr_] := 
 ReleaseHold[Hold[expr] /. q_Quantity :> preprocessQuantity[q]]   
 
Clear[PhysicalConstantRefactor];
PhysicalConstantRefactor[dimensions_] := Times @@ Map[
   With[{unit = QuantityUnit[UnitConvert[#[[1, 1]]]]/#[[1, 1]]},
     unit^#[[2]]] &, dimensions]

Clear[doSomething];
doSomething[v_, constants_List] := Catch[
  With[{factored = FactorList[v]},
   With[{constantsWithPowers = 
      Cases[factored, {PhysicalConstantValue[_String], _}]},
    If[Union[(constantsWithPowers /. {PhysicalConstantValue[
            s_String], _} :> s)] =!= constants, 
     Throw[$Failed, $tag]];
    PhysicalConstantRefactor[constantsWithPowers]
    ]],
  $tag]


Clear[postprocessQuantity];
postprocessQuantity[q : Quantity[v_, u_]] := 
 With[{constants = 
    Union[Cases[v, 
      PhysicalConstantValue[const_String] :> const, -1]]}, 
  If[constants =!= {},
   With[{unitFactor = doSomething[v, constants]},
    If[unitFactor =!= $Failed,
     Quantity @@ {v /. PhysicalConstantValue[_String] :> 1, 
       u/unitFactor},
     $Failed]],
   q]]

postprocessQuantity[q : Quantity[v_, u_], "Numericalize"] := ReleaseHold[
  Hold[q] /. PhysicalConstantValue[s_String] :> QuantityMagnitude[UnitConvert[s]]
  ]

Clear[postprocessQuantityExpression];
postprocessQuantityExpression[expr_] := (((ReleaseHold[ Hold[expr] /. q_Quantity :> postprocessQuantity[q]]) /. 
    PhysicalConstantValue[s_String] :> placeholder[s]/Quantity[QuantityUnit[UnitConvert[s]]]) /. 
  placeholder -> Quantity)
   
SetAttributes[Internal`PreprocessSymbolicQuantityExpression,HoldAll];
Internal`PreprocessSymbolicQuantityExpression[expr_] := preprocessQuantityExpression[expr]
Internal`PostprocessSymbolicQuantityExpression[expr_] := postprocessQuantityExpression[expr]

QuantityUnits`UnitConvertArray[values_, unit_, targetunit_] := 
 bulkConvertArray[values, unit, targetunit]
 
bulkConvertArray[values_, unit_, targetunit_] := 
 If[CompatibleUnitQ[unit, targetunit],
  Module[{fun = uCF[unit, targetunit]},
   fun[values]
   ]
  ,
  $Failed
  ]
