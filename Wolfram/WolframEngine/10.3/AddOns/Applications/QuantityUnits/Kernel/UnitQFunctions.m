(*track whether 'interpreting unit' has been issued yet, use appropriate display based on stand-alone kernel or FE*)
$hasQueried=False;
$QueryTimeout=Automatic;

blueBox[text_String]:=If[Head[$FrontEnd]===FrontEndObject,
DisplayTemporary[
 Internal`LoadingPanel[text]],
  Print[text],
  Print[text]]

(*basic utilities to access/modify system quantity-cache*)
addToUHT[input_String,quantity_Quantity]:=Internal`SetQuantityCache[input,quantity];
addToUHT[___]:=$Failed
getFromUHT[input_String]:=If[
	And[
		UnsameQ[$QUnitInputAliasesFailed,True],
		System`Utilities`HashTableContainsQ[QUnitInputAliases,input]
		],
	With[{u=System`Utilities`HashTableGet[QUnitInputAliases,input]},
		Quantity[1,u]],
	Internal`CheckQuantityCache[input],
	Internal`CheckQuantityCache[input]
	]
getFromUHT[___]:=$Failed

ClearQuantityCache[]:=(Clear[$UnitDisplayCache,$KnownUnitQCache,$CompatibleUnitQCache];
$UnitDisplayCache=System`Utilities`HashTable[];
$KnownUnitQCache=System`Utilities`HashTable[];
$CompatibleUnitQCache=System`Utilities`HashTable[];
$UnitDisplayCacheSize=0;
$KnownUnitQCacheSize=0;
$CompatibleUnitQCacheSize=0;);

(*get paclet units and unit short name aliases from UnitAliases.m; bail out if not found*)
getOrFail[FileNameJoin[{DirectoryName[$InputFileName],"UnitAliases.m"}]]
	
(*parse units using special WolframAlpha[] flag*)
ToQuantity[""]:=$Failed
ToQuantity[input_String]/;TrueQ[$AutomaticUnitParsing]:= With[{r=getFromUHT[input]},
	If[r=!=$Failed,r,Module[{bb},TimeConstrained[If[$hasQueried===False,$hasQueried=True;bb=blueBox["Interpreting unit ...."]];With[
	{res=WolframAlpha[input, "ToQuantity",InputAssumptions->{"ClashPrefs_Unit"}]},
	Quiet[NotebookDelete[bb]];
	If[Not[FreeQ[res,_Quantity,-1]],
		Block[{$AutomaticUnitParsing=False},
			Module[{q=Quiet[ReleaseHold[res]]},
				If[QuantityQ[q],
					q;
					addToUHT[input,q];q,
					$Failed,$Failed]
			]
		],
		$Failed,$Failed]],($QueryTimeout/.{Automatic->15}),Message[Quantity::timeout,Quantity];$Failed]]]
]
ToQuantity[Quantity[mag_,unit_String,opts___]]:=With[{res=ToQuantity[unit]},If[res=!=$Failed,
	If[Not[MixedRadixQ[res]],
		With[{u=QuantityUnit[res]},Quantity[mag,u,opts]],
		res*mag],
	$Failed,$Failed]]
(*interpret units passed up from kernel-code w/out triggering down-code*)
ToQuantity[UnknownQuantity[mag_,unit_String,opts___]]:=With[{res=ToQuantity[unit]},If[res=!=$Failed,If[Not[MixedRadixQ[res]],
		With[{m=mag,u=QuantityUnit[res]},Quantity[m,u,opts]],
		res*mag],$Failed,$Failed]]
ToQuantity[UnknownQuantity[mag_,DatedUnit[unit_String],opts___]]:=ToQuantity[UnknownQuantity[mag,unit,opts]]

ToQuantity[UnknownQuantity[mag_,DatedUnit[unit_String,date_],opts___]]:=If[
	And[KnownUnitQ[unit],DateObjectQ[date]],(*quick validation for DateObject's which might not have validated*)
	Quantity[mag,DatedUnit[unit,date]],
	If[Not[Or[NumericQ[date],MatchQ[date,{_?NumericQ..}],DateObjectQ[date]]],
	$Failed,
	With[{res=ToQuantity[unit]},If[res=!=$Failed,If[Not[MixedRadixQ[res]],
		With[{m=mag,u=DatedUnit[QuantityUnit[res],date]},Quantity[m,u,opts]],
		$Failed],$Failed,$Failed]]]
]
ToQuantity[q:UnknownQuantity[mag_,unit_,___]]/;Not[FreeQ[HoldForm[unit],DatedUnit]]:= Catch[
	With[{u=Cases[HoldForm[unit],_DatedUnit,-1],test=unit/._DatedUnit->1},
	If[Not[Or[Quiet[QuantityQ[Quantity[1,test]]],NumericQ[test]]],Throw[$Failed,$tag]];
	Module[{rules=parseDatedUnits[u]},
		(q/.rules)/.UnknownQuantity->Quantity
	]	
], $tag]
ToQuantity[___]:=$Failed

parseDatedUnits[{}]:=Throw[$Failed,$tag]
parseDatedUnits[u_List]:= Table[
	With[{r=ToQuantity[UnknownQuantity[1,i]/.DatedUnit[s_String,_]:>s]},
		If[SameQ[r,$Failed],Throw[$Failed,$tag]];
		Rule[i,DatedUnit[QuantityUnit[r],i[[2]]]]],
		{i, u}
]
parseDatedUnits[___]:=Throw[$Failed,$tag]

UnitCliqueQ[UnitClique[__]]:=True
UnitCliqueQ[___]=False

(*quick test to see if the input is a known Alpha unit*)
basicUnitTest[CalculateParse`Content`Calculate`Unit[cu_IndependentUnit]]:=True
basicUnitTest[CalculateParse`Content`Calculate`Unit[cu_DatedUnit]]:=True
basicUnitTest[unit_CalculateParse`Content`Calculate`Unit] := Block[{$IgnoreMalformedQuantities=True},
 If[Quiet[CalculateUnits`UnitCommonSymbols`GetFUnitLabel[unit] =!= {}], True,
   False, False]]
basicUnitTest[_] = False;

(*flatten expression and check to see if it's just known units*)
secondaryUnitTest[unit_CalculateParse`Content`Calculate`Unit] := 
 With[{l = exp2list[unit]}, And @@ (basicUnitTest[CalculateParse`Content`Calculate`Unit[#]] & /@ l)]
secondaryUnitTest[_] = False;

(*alternative flattening system to catch sums of units check to see if it's just known units*)
tertiaryUnitTest[unit_CalculateParse`Content`Calculate`Unit]:=With[
	{l=Union@Flatten[altexp2list[unit]]},And@@(basicUnitTest[CalculateParse`Content`Calculate`Unit[#]]&/@l)]
tertiaryUnitTest[_]=False;

(*main track for checking unit validity*)
SetAttributes[fastUnitScan,HoldAll];
fastUnitScan[unit_]:=If[
	MatchQ[Hold[unit],Hold[Times[1,Power[s_String,-1]]]/;KnownUnitQ[s]],
		True,
		With[
			{l=Union[Flatten[fastexp2list[unit]]]},
			And@@(
				Or[
					MemberQ[Keys[$UnitReplacementRules],#],
					MatchQ[#,_IndependentUnit](*, ommitted for speed's sake.  Should be caught in secondary scan
					MatchQ[#,_DatedUnit]*)
				] & /@ l)]]
fastUnitScan[__]:=Throw[False,$tag];

secondaryUnitScan[unit_] := 
 With[{l = exp2list[unit]}, 
 	And @@ (
 	Or[
 		MemberQ[Keys[$UnitReplacementRules],#],
 		MatchQ[#,_IndependentUnit],
 		MatchQ[#,DatedUnit[u_,date_]/;And[MemberQ[Keys[$UnitReplacementRules],u],Or[NumericQ[date],MatchQ[date,{_?NumericQ..}]]]],
 		MatchQ[#,DatedUnit[u_,date_DateObject]/;With[{d=System`DateObjectDump`ValidateDateObject[date]},DateObjectQ[d]]]
 	] & /@ l)]
secondaryUnitScan[__]:= Throw[False,$tag];

$KnownUnitQCache = System`Utilities`HashTable[];
$KnownUnitQCacheSize = 0;

SetAttributes[KnownUnitQCacheContainsQ, HoldAll];
KnownUnitQCacheContainsQ[unit_] := Quiet[Check[(*possible messages from 268125*)
 System`Utilities`HashTableContainsQ[$KnownUnitQCache, HoldForm[unit]],False]]
KnownUnitQCacheContainsQ[___] := False

KnownUnitQCacheAdd[
   heldform_] := (If[TrueQ[$KnownUnitQCacheSize >= $MaxCacheSize], 
    Clear[$KnownUnitQCache]; $KnownUnitQCache = 
     System`Utilities`HashTable[]; $KnownUnitQCacheSize = 
     1;, $KnownUnitQCacheSize++];
   System`Utilities`HashTableAdd[$KnownUnitQCache, heldform, Null]);

Attributes[KnownUnitQ] = {HoldAll};
KnownUnitQ[]:=(Message[KnownUnitQ::argx,KnownUnitQ,0];Null/;False)
KnownUnitQ[expr_]:=If[
	KnownUnitQCacheContainsQ[expr], 
	True, 
 	With[{r=iKnownUnitQ[expr]}, 
  		If[
  			TrueQ[r], 
  			KnownUnitQCacheAdd[HoldForm[expr]]; True, 
  			False,
  			False]],
  	False]
KnownUnitQ[_,args__]:=(Message[KnownUnitQ::argx,KnownUnitQ,Length[{args}]+1];Null/;False)


Attributes[iKnownUnitQ] = {HoldAll};
iKnownUnitQ["DimensionlessUnit"]=True;
iKnownUnitQ[unit_String] := MemberQ[Keys[$UnitReplacementRules],unit]
iKnownUnitQ[times_Times]:=nestedKnownUnitQ[times]
iKnownUnitQ[power_Power]:=nestedKnownUnitQ[power]
iKnownUnitQ[divide_Divide]:=nestedKnownUnitQ[divide]
iKnownUnitQ[IndependentUnit[_String]]:=True
iKnownUnitQ[MixedRadix[args__]]:=Internal`SameUnitDimension[args]
iKnownUnitQ[Power[IndependentUnit[_String],x__]]:=True
iKnownUnitQ[HoldForm[e_]] := iKnownUnitQ[e]
iKnownUnitQ[_?NumericQ]:= False
iKnownUnitQ[DatedUnit[unit_String,date_]]:=With[{d=date},(*deal with funkyness in held unit*)
And[KnownUnitQ[unit],Or[NumericQ[d],MatchQ[d,{_?NumericQ..}],DateObjectQ[d]]]]
(*With[{k=KnownUnitQ[unit]},
	If[TrueQ[k],
		True,
		With[{q=Quantity[1,unit]},
			If[Quiet[QuantityQ[q]],
				True,
				False
			]
		]]
	]*)
iKnownUnitQ[Power[d:DatedUnit[_String,___],x__]]:=KnownUnitQ[d]

iKnownUnitQ[unit_CalculateParse`Content`Calculate`Unit] := TrueQ[
 If[basicUnitTest[unit], True, 
  If[secondaryUnitTest[unit], True, 
  	If[
  		tertiaryUnitTest[unit], 
  		True, False],
  	False], 
  False]
  ]

iKnownUnitQ[_] = False;

SetAttributes[nestedKnownUnitQ,HoldAll];
nestedKnownUnitQ[unit_] := Catch[TrueQ[
  If[fastUnitScan[unit], True, 
  	If[
  		secondaryUnitScan[unit], 
  		True, False],
  	False]
  ],$tag]

(*fast method for picking out currency-inclusive quantities*)
hasCurrencyQ[DatedUnit[target_String,_]]:=hasCurrencyQ[target]
hasCurrencyQ[target_String]:=Module[{ul=UnitLookup[toAlphaUnit[target],"UnitLabel"]},
		If[Head[ul]=!=UnitLookup,
			MemberQ[Flatten[StringSplit[ul,"Per"]],"Money"],
			False,
			Message[hasCurrencyQ::failed];$Failed]]
hasCurrencyQ[h:(_Times|_Power|_Divide)]:=Module[{list=exp2list[toAlphaUnit[h]],uls},uls=UnitLookup[#,"UnitLabel"]&/@list;
	If[MemberQ[Head/@uls,UnitLookup],
		False,
		MemberQ[Flatten[StringSplit[Flatten[uls],"Per"]],"Money"],
		$Failed]]
hasCurrencyQ[u_CalculateParse`Content`Calculate`Unit]:=hasCurrencyQ[fromAlphaUnit[u]]
hasCurrencyQ[(q_Quantity)?ValidUnitQ]:=hasCurrencyQ[QuantityUnit[q]]
hasCurrencyQ[__]=False;

couldBeACurrencyQ[input_]:=With[{res=hasCurrencyQ[input]},
	If[TrueQ[res],
		True,
		If[MatchQ[input,_String|_DatedUnit],
			With[{q=Quiet[Quantity[1,input]]},
			Quiet[
				TrueQ[And[
					QuantityQ[q],
					hasCurrencyQ[q]
					]]
				]
			],
			False
		]
	]
]
couldBeACurrencyQ[___]:=False

hasDatedUnitQ[target_String]:=False
hasDatedUnitQ[_DatedUnit]:=True
hasDatedUnitQ[h:(_Times|_Power|_Divide|_HoldForm)]:=Not[FreeQ[Hold[h],DatedUnit]]
hasDatedUnitQ[(q_Quantity)?ValidUnitQ]:=hasDatedUnitQ[QuantityUnit[q]]
hasDatedUnitQ[__]=False;

QuantityTensorQ[(Quantity[_List,Except[_List],___])?ValidUnitQ]:=True
QuantityTensorQ[(Quantity[_,_,opts___?OptionQ])?ValidUnitQ]:=If[("ThreadDepth"/.Flatten[{opts}]/."ThreadDepth"->Infinity)=!=Infinity,True,False,False]
QuantityTensorQ[___]=False

(*only documented to work on pairs of Quantities, but also functions for various other cases when needed*)
Unprotect[CompatibleUnitQ];
CompatibleUnitQ[]:=(Message[CompatibleUnitQ::argx,CompatibleUnitQ,0,2];Null/;False)
CompatibleUnitQ[l : (_List ..)] := With[{s = Union[Flatten[{l}]]}, CompatibleUnitQ @@ s]
CompatibleUnitQ[ l : (Alternatives[_List, _SparseArray?ArrayQ, _StructuredArray?ArrayQ] ..)] :=
  CompatibleUnitQ[ Replace[{l}, {
      s_SparseArray :> {s["NonzeroValues"], If[ TrueQ[s["Density"]==1], {}, s["Background"]]}, 
      s_StructuredArray :> If[s["Structure"] === QuantityArray, Map[Quantity[1, #] &, Flatten[{s["UnitBlock"]}]], Normal[s]]
  }, {1}]]
CompatibleUnitQ[arg1_] := Block[{$PureUnitiesFlag = True}, 
  With[{carg1 = iCompatibleUnitQFixArg[arg1]/.DatedUnit[unit_,_]:>unit}, 
   If[UnsameQ[carg1, $Failed], True, False, False]]]
CompatibleUnitQ[arg1_, arg2_] := Block[{$PureUnitiesFlag = True}, 
  With[{carg1 = iCompatibleUnitQFixArg[arg1]/.DatedUnit[unit_,_]:>unit, 
    carg2 = iCompatibleUnitQFixArg[arg2]/.DatedUnit[unit_,_]:>unit}, 
   If[And[UnsameQ[carg1, $Failed], UnsameQ[carg2, $Failed]], 
    iCompatibleUnitQCached[carg1, carg2], False, False]]]
CompatibleUnitQ[arg1_, arg2_, args__] := Catch[iThreadAndCatchCompatibility[arg1, arg2, args],$tag]

grabFirstQuantityFromMixedRadix[Quantity[MixedRadix[v1_,__],MixedRadix[u_,__],___]]:= With[
	{q=Quantity[v1,u]},
	If[Quiet[ValidUnitQ[q]],
		q,
		$Failed,
		$Failed]
]

iThreadAndCatchCompatibility[arg1_, args__] := (Scan[With[{r = CompatibleUnitQ[arg1, #]}, 
     If[Not[TrueQ[r]], Throw[False,$tag]]] &, {args}]; True)

iCompatibleUnitQFixArg[n_?NumericQ] := Quantity[1, "PureUnities"]
iCompatibleUnitQFixArg[(q_Quantity)?ValidUnitQ] := If[MixedRadixQ[q], grabFirstQuantityFromMixedRadix[q], q, q]
iCompatibleUnitQFixArg[s_String] := If[KnownUnitQ[s], 
	Quantity[1, s], 
  	With[{r = Quantity[s]}, 
   		If[Quiet[ValidUnitQ[r]], 
   			r, 
   			$Failed, 
   			$Failed]
   	]
]

iCompatibleUnitQFixArg[MixedRadix[unit_, __]?KnownUnitQ] := With[{r = Quantity[1, unit]}, 
  If[Quiet[ValidUnitQ[r]], r, $Failed, $Failed]]
iCompatibleUnitQFixArg[unit_?KnownUnitQ] := Quantity[1, unit]
iCompatibleUnitQFixArg[HoldForm[unit_?KnownUnitQ]] := Quantity[1, unit]
iCompatibleUnitQFixArg[_] := $Failed

$CompatibleUnitQCache = System`Utilities`HashTable[];
$CompatibleUnitQCacheSize = 0;

addToCUQHT[hf_HoldForm, 
  r_] := (If[TrueQ[$CompatibleUnitQCacheSize >= $MaxCacheSize], 
   Clear[$CompatibleUnitQCache]; $CompatibleUnitQCache = 
    System`Utilities`HashTable[]; $CompatibleUnitQCacheSize = 1;, $CompatibleUnitQCacheSize++;];
  System`Utilities`HashTableAdd[$CompatibleUnitQCache, hf, r];)

retrieveFromCUQHT[hf_HoldForm] := System`Utilities`HashTableGet[$CompatibleUnitQCache, hf]

SetAttributes[iCompatibleUnitQCached, HoldAll];
iCompatibleUnitQCached[u_, u_] := True
iCompatibleUnitQCached[arg1_, arg2_] := If[
	System`Utilities`HashTableContainsQ[$CompatibleUnitQCache, HoldForm[{arg1, arg2}]], 
	retrieveFromCUQHT[HoldForm[{arg1, arg2}]],
   	With[{r = iCompatibleUnitQ[arg1, arg2]}, 
   		If[Not[System`Utilities`HashTableContainsQ[$CompatibleUnitQCache, HoldForm[{arg1, arg2}]]], 
    		addToCUQHT[HoldForm[{arg1, arg2}], r]
    	]; r]
]

iCompatibleUnitQ[Quantity[_,u_,___],Quantity[_,u_,___]] := True (*same unit; 265320*)
iCompatibleUnitQ[arg1_, arg2_] := 
 Block[{$IgnoreMalformedQuantities = True, $AlphaBlockFlag = True}, 
  With[{a = toAlphaQuantity[arg1], b = toAlphaQuantity[arg2]}, 
   TrueQ[CalculateUnits`UnitTable`CompatibleQ[a, b]]]]

AllQuantityQ[data_List]/;Not[Developer`PackedArrayQ[data]] := FreeQ[QuantityQ[#]& /@ data, False]
AllQuantityQ[___]:=False

(*overload Internal`PossibleQuantityQ with c-code QuantityQ function once QuantityUnits` has been loaded*)
Unprotect[Internal`PossibleQuantityQ];
Internal`PossibleQuantityQ[args__]:=QuantityQ[args]
Protect[Internal`PossibleQuantityQ];

ValidUnitQ=QuantityQ

MixedRadixQ[(Quantity[mag_MixedRadix,unit_MixedRadix])?ValidUnitQ]:=
	TrueQ[Length[mag]===Length[unit]&&Length[unit]>1]
MixedRadixQ[(Quantity[Interval[{l_MixedRadix,u_MixedRadix}],unit_MixedRadix])?ValidUnitQ] :=
	TrueQ[And[SameQ[Length[l],Length[u],Length[unit]], Length[unit]>1]]
MixedRadixQ[___]=False;

(*used for psudo-listable functions, currently just CommonUnits*)
QuantityOrQuantityListQ[input__] := iQuantityOrQuantityListQ@@DeleteCases[{input},_?NumericQ,-1]
iQuantityOrQuantityListQ[_?ValidUnitQ]:=True
iQuantityOrQuantityListQ[list_List]:=And@@(iQuantityOrQuantityListQ/@list)
iQuantityOrQuantityListQ[arg1_,args__]:=iQuantityOrQuantityListQ[{arg1,args}]
iQuantityOrQuantityListQ[{}] := True
iQuantityOrQuantityListQ[] := True
iQuantityOrQuantityListQ[___]:=False

justInertUnitQ[Quantity[_,_IndependentUnit,___?OptionQ]]:=True
justInertUnitQ[Quantity[_,Power[_IndependentUnit,_]]]:=True
justInertUnitQ[___]:=False

checkforInertUnit[q_Quantity]:=q
checkforInertUnit[n_?NumericQ]:=n
checkforInertUnit[t_Times]:=If[MatchQ[t,Times[___,_IndependentUnit]],With[{m=DeleteCases[t,_IndependentUnit],u=Times@@Cases[t,_IndependentUnit]},Quantity[m,u]],t]
checkforInertUnit[other___]:=other

isTemperatureQ[x_Quantity]:=MatchQ[oUnitDimensions[x],{"TemperatureUnit"->_}]
isTemperatureQ[___]:=False

UniformUnitCheck[{Quantity[_,unit_]..}]:=True
UniformUnitCheck[___]:=False

QuantityIntervalQ[l_List, q_Quantity] := Or @@ (QuantityIntervalQ[#, q] & /@ l);

QuantityIntervalQ[int : Quantity[Interval[{lo_, hi_}], unit_, ___], q_Quantity] := 
 If[Quantity[lo, unit] <= q <= Quantity[hi, unit], True, False, False]

QuantityIntervalQ[l : Quantity[_List, _?KnownUnitQ, ___], q_] := QuantityIntervalQ[Thread[l], q]

QuantityIntervalQ[int : Quantity[Interval[{lo_, hi_}], unit_, ___], q_Quantity] := 
 If[Quantity[lo, unit] <= q <= Quantity[hi, unit], True, False, False]
 
QuantityIntervalQ[Interval[{q1_Quantity,q2_Quantity}],q_] := 
 If[q1 <= q <= q2, True, False, False]
 
QuantityIntervalQ[___] := False

(*testing function for matrix function arguments*)
UniformQuantityMatrixQ[_?Statistics`Library`RealMatrixQ]:=True
UniformQuantityMatrixQ[mat_?MatrixQ]:=MatchQ[mat,HoldForm[{{Quantity[_,unit_]..}..}]]
UniformQuantityMatrixQ[___]:=False

iQuantityCoprimeQ[(q__Quantity)?ValidUnitQ] /; CompatibleUnitQ[q] :=
With[{qs = QuantityMagnitude[StandardizeQuantities[{q}]]},CoprimeQ[Sequence @@ qs]]
iQuantityCoprimeQ[___]=$Failed;

(*test to handle cases like Quantity[a,b]*)
SetAttributes[hfUnitQ,HoldFirst];
hfUnitQ[unit_] := 
 With[{c = Cases[Hold[unit], _String, Infinity]}, 
  Length[c] =!= Length[Union[c]]]

(*utility function used in oQuantity DownCode*)
PureUnitiesQ[___]=False;
PureUnitiesQ["PureUnities"]=True;
PureUnitiesQ["DimensionlessUnit"]=True;
PureUnitiesQ[CalculateParse`Content`Calculate`Unit[CalculateUnits`UnitCommonSymbols`PureUnities]]=True;

badTDQ[Infinity]=False
badTDQ[i_Integer]:=Negative[i]
badTDQ[___]=True

(*test function that returns True for Or[QuantityMatrixQ,RealMatrixQ,Columnwise compatible units]*)
MixedQuantityMatrixQ[_?Statistics`Library`RealMatrixQ]:=False
MixedQuantityMatrixQ[_?Internal`QuantityMatrixQ]:=True
MixedQuantityMatrixQ[_?Internal`ColumnwiseQuantityVectorQ]:=True
MixedQuantityMatrixQ[mat_?MatrixQ]:=Catch[Apply[And,
        Table[If[#===False, Throw[False,$tag], #]&[Statistics`Library`RealVectorQ[i] ||Internal`QuantityVectorQ[i]], {i, mat}]
        ],$tag]/;Not[FreeQ[mat[[1]],Quantity]]
MixedQuantityMatrixQ[___]=False;

(*messaging function used to pick out incompatibility in operations*)
iFindInCompatibleUnits[args__]:=Module[{units=Union[QuantityUnit[{args}]],second},
			second=Quiet[Check[First[Select[units, Not[CompatibleUnitQ[First[units], #]]&]],"fail"]];
			If[second=!="fail",Message[Quantity::compat,
				First[units],
				second],Message[Quantity::compatu]]
		]

QuantityArrayQ[__] = False
QuantityArrayQ[s_StructuredArray] := MatchQ[s, HoldPattern[
   StructuredArray[QuantityArray, _, 
    StructuredArray`StructuredData[QuantityArray, _, _, _]]
   ]
]

(*new QuantityUnits` context predicates suggested by jose*)

QuantityUnits`IntegerQuantityQ[(Quantity[mag_,__])?QuantityQ] := IntegerQ[mag]
QuantityUnits`IntegerQuantityQ[___] := False

QuantityUnits`MachineIntegerQuantityQ[(Quantity[mag_,__])?QuantityQ] := Developer`MachineIntegerQ[mag]
QuantityUnits`MachineIntegerQuantityQ[___]:=False

QuantityUnits`MachineNumberQuantityQ[(Quantity[mag_,__])?QuantityQ] := MachineNumberQ[mag]
QuantityUnits`MachineNumberQuantityQ[___]:=False

QuantityUnits`NumberQuantityQ[(Quantity[mag_,__])?QuantityQ] := NumberQ[mag]
QuantityUnits`NumberQuantityQ[___]:=False

QuantityUnits`NumericQuantityQ[(Quantity[mag_,__])?QuantityQ] := NumericQ[mag]
QuantityUnits`NumericQuantityQ[___]:=False

QuantityUnits`RealValuedNumberQuantityQ[(Quantity[mag_,__])?QuantityQ] :=Internal`RealValuedNumberQ[mag]
QuantityUnits`RealValuedNumberQuantityQ[___]:=False

QuantityUnits`RealValuedNumericQuantityQ[(Quantity[mag_,__])?QuantityQ] := Internal`RealValuedNumericQ[mag]
QuantityUnits`RealValuedNumericQuantityQ[___]:=False
