Begin["DataPaclets`HumanGrowthDataDump`"];

$ProtectedSymbols = {
	System`HumanGrowthData
};

Unprotect@@$ProtectedSymbols;

$resourceDirectory=DirectoryName[$InputFileName](*NotebookDirectory[]*);

getResourceFile[file_String] := If[
	FileExistsQ[#],
	Get[#],
	Message[HumanGrowthData::init];Throw[$Failed,"MissingResource"]
]&[FileNameJoin[{$resourceDirectory,file}]]

(*AbortProtect[getResourceFile/@{"growthcurves.m"}];*)

(* Get Data *)
rawGrowthData=Import[FileNameJoin[{$resourceDirectory,"growthcurves.m"}]];

(*build LMS functions*)
(* sex, property, ethnicity, location (last two not used currently) *)
(HumanGrowthDataLMS[#[[1]], #[[2]], #[[3]],#[[4]]] =
  Interpolation[Developer`ToPackedArray@Transpose[Map[Function[{x}, Flatten[x, 1]], #[[{-2, -1}]]]]])&/@rawGrowthData;

$sexes={"Female","Male",All};
$enthnicities=Union[rawGrowthData[[All,3]]];
$countries=Union[rawGrowthData[[All,4]]];

(*extract valid ranges*)
Which[StringMatchQ[#[[2]],__~~"Age"],
	hgdDomain["Age", #[[1]], #[[2]], #[[3]],#[[4]]]=Quantity[Interval[#], "Months"]&@#[[5, 1, {1, -1}]],
	StringMatchQ[#[[2]],__~~"Length"],
	hgdDomain["Length", #[[1]], #[[2]], #[[3]],#[[4]]]=Quantity[Interval[#], "Centimeters"]&@#[[5, 1, {1, -1}]],
	True,
	hgdDomain["Height", #[[1]], #[[2]], #[[3]],#[[4]]]=Quantity[Interval[#], "Centimeters"]&@#[[5, 1, {1, -1}]]]&/@rawGrowthData;

(* LMS to Percentile or ZScore*)
(* if z is too big and l is negative, then this goes complex, the 1+lsz went through zero, so just use that *)
HumanGrowthDataFromLMS[z_, {l_, m_, s_}, "FromZScore"] := 
    Replace[If[l =!= 0., m*(1 + l*s*z)^(1/l), m*Exp[s*z]], _Complex -> If[l<0,Infinity,0]];
 
HumanGrowthDataFromLMS[p_, {l_, m_, s_}, "FromPercentile"] := 
    HumanGrowthDataFromLMS[PercentileToZ[p], {l, m, s}, "FromZScore"]
 
HumanGrowthDataFromLMS[val_, {l_, m_, s_}, "ToZScore"] := 
    If[l =!= 0., ((val/m)^l - 1)/(l*s), Log[val/m]/s]
 
HumanGrowthDataFromLMS[val_, {l_, m_, s_}, "ToPercentile"] := 
    ZToPercentile[HumanGrowthDataFromLMS[val, {l, m, s}, "ToZScore"]]
 
PercentileToZ[perc_] := Sqrt[2.]*InverseErf[0.02*perc - 1.]
ZToPercentile[z_] := 50*(Erf[z/Sqrt[2.]] + 1)

(* precompute some frequently used ones *)
PercentileToZ[0.1] = -3.0902323061678136;
PercentileToZ[2] = -2.053748910631822;
PercentileToZ[3] = -1.8807936081512506;
PercentileToZ[4] = -1.7506860712521704;
PercentileToZ[5] = -1.6448536269514724;
PercentileToZ[10] = -1.2815515655446001;
PercentileToZ[25] = -0.6744897501960818;
PercentileToZ[50] = 0.;
PercentileToZ[75] = 0.6744897501960818;
PercentileToZ[90] = 1.2815515655446001;
PercentileToZ[95] = 1.6448536269514737;
PercentileToZ[96] = 1.75068607125217;
PercentileToZ[97] = 1.8807936081512506;
PercentileToZ[98] = 2.053748910631822;
PercentileToZ[99.9] = 3.090232306167847;
ZToPercentile[-3] = 0.13498980316301035;
ZToPercentile[-2] = 2.275013194817921;
ZToPercentile[-1] = 15.865525393145708;
ZToPercentile[0] = 50;
ZToPercentile[1] = 84.1344746068543;
ZToPercentile[2] = 97.72498680518208;
ZToPercentile[3] = 99.86501019683699;
(* done precomputing *)

$hgdProperties = {
    "Height",
    "Length",
    "HeadCircumference",
    "Weight",
    "BMI"(*,
    "Percentile",
    "ZScore"*)
};

propQuantity[Infinity|0|_?Negative,_,_]:=Missing["NotAvailable"]
propQuantity[x_,"BMI","Metric"]:=Quantity[x,"Kilograms"/"Meters"^2]
propQuantity[x_,"ZScore",_]:=x
propQuantity[x_,"Percentile",_]:=Quantity[x,"Percent"]
propQuantity[x_,"Age",_]:=Quantity[x,"Months"]

propQuantity[x_,"Height"|"Length"|"HeadCircumference","Imperial"]:=UnitConvert[Quantity[x,"Centimeters"],"Inches"]
propQuantity[x_,"Weight","Imperial"]:=UnitConvert[Quantity[x,"Kilograms"],"Pounds"]
propQuantity[x_,"BMI","Imperial"]:=UnitConvert[Quantity[x*703,"Kilograms"/"Meters"^2],"Pounds"/"Inches"^2]

propQuantity[x_,"Height"|"Length"|"HeadCircumference",_]:=Quantity[x,"Centimeters"]
propQuantity[x_,"Weight",_]:=Quantity[x,"Kilograms"]

propQuantity[x_,{"Height"|"Length"|"HeadCircumference","ProbabilityDensity"},"Imperial"]:=UnitConvert[Quantity[x,"Centimeters"^(-1)],"Inches"^(-1)]
propQuantity[x_,{"Height"|"Length"|"HeadCircumference","ProbabilityDensity"},_]:=Quantity[x,"Centimeters"^(-1)]
propQuantity[x_,{"Weight","ProbabilityDensity"},"Imperial"]:=UnitConvert[Quantity[x,"Kilograms"^(-1)],"Pounds"^(-1)]
propQuantity[x_,{"Weight","ProbabilityDensity"},_]:=Quantity[x,"Kilograms"^(-1)]
propQuantity[x_,{"BMI","ProbabilityDensity"},"Imperial"]:=UnitConvert[Quantity[x/703,"Meters"^2/"Kilograms"],"Inches"^2/"Pounds"]
propQuantity[x_,{"BMI","ProbabilityDensity"},_]:=Quantity[x,"Meters"^2/"Kilograms"]

quantitypart[q_,"Height"|"Length"|"HeadCircumference"]:=QuantityMagnitude@UnitConvert[q,"Centimeters"]
quantitypart[q_,"Weight"]:=QuantityMagnitude@UnitConvert[q,"Kilograms"]
quantitypart[q_,"BMI"]:=Module[{units=QuantityUnit[q],metricQ},
	metricQ=QuantityMagnitude@UnitConvert[Quantity[1,units],"Kilograms"/"Meters"^2];
	metricQ=IntegerQ[metricQ]||IntegerQ[1/metricQ];
	If[metricQ,
		QuantityMagnitude@UnitConvert[q,"Kilograms"/"Meters"^2],
		QuantityMagnitude@UnitConvert[q,"Pounds"/"Inches"^2]
	]
]

(* set up mapping from "entity tuples" to the corresponding curve(s) *)
hgdCurve["Height", "Weight"] = {"WeightFromStature"};
hgdCurve["Age", "Weight"] = {"InfantWeightFromAge", "WeightFromAge"};
hgdCurve["Age", "Height"] = {"StatureFromAge"};
hgdCurve["Length", "Weight"] = {"InfantWeightFromLength"};
hgdCurve["Age", "Length"] = {"InfantLengthFromAge"};
hgdCurve["Age", "HeadCircumference"] = {"InfantHeadCircumferenceFromAge"};
hgdCurve["Age", "BMI"] = {"BMIFromAge"};

hgageQ[x_Quantity]:=Module[{age=x/.HoldPattern[ThreadDepth -> _]:>ThreadDepth->Infinity},
	If[ListQ[age],
		hgageQ[age],
		CompatibleUnitQ[x,Quantity["Months"]]&&TrueQ[Element[QuantityMagnitude[x], Reals]]
	]
]
hgageQ[x_DateObject]:=CompatibleUnitQ[Now-x,Quantity["Months"]]
hgageQ[x_?(VectorQ[#,NumericQ]&)]:=Module[{age=If[Length[x]<=6,DateObject[x],$Failed]},
	hgageQ[age]
]
hgageQ[{}]:=False
hgageQ[x_List] := If[hgageQ[x[[1]]],And @@ (hgageQ /@ x),False]
hgageQ[___]:=False

fgtestQ[x_DateObject]:=Module[{time=QuantityMagnitude[Now-x]},
	If[TrueQ[time<0],Message[System`HumanGrowthData::fdate,x];True,False]
]
fgtestQ[x_Quantity]:=Module[{age=x/.HoldPattern[ThreadDepth -> _]:>ThreadDepth->Infinity},
	If[ListQ[age],
		fgtestQ[age],
		age=QuantityMagnitude[x];
		If[TrueQ[age<0],Message[System`HumanGrowthData::negage,x];True,False]
	]
]
fgtestQ[x_?(VectorQ[#,NumericQ]&)]:=Module[{age=DateObject[x]},
	fgtestQ[age]
]
fgtestQ[x_List] := And @@ (fgtestQ /@ x)
fgtestQ[___]:=False

propQ[All]:=True
propQ[_String]:=True
propQ[_EntityProperty]:=True
propQ[___]:=False

indexQ[_Real]:=True
indexQ[_Integer]:=True
indexQ[HoldPattern[Quantity[_,"Percent",___]]]:=True
indexQ[x_Association]:=KeyExistsQ[x,"Percentage"]||KeyExistsQ[x,"ZScore"]
indexQ[___]:=False

quantitycheckQ[q_Quantity,"Height"|"Length"|"HeadCircumference"]:=CompatibleUnitQ[q,Quantity["Feet"]]
quantitycheckQ[q_Quantity,"Weight"]:=CompatibleUnitQ[q,Quantity["Kilograms"]]
quantitycheckQ[q_Quantity,"BMI"]:=CompatibleUnitQ[q,Quantity["Kilograms"/"Meters"^2]]
(*quantitycheckQ[q_NumericQ,"BMI"]:=True*)
quantitycheckQ[___]:=False

hgentityformat[e_Quantity]:={e,All,$Failed};
hgentityformat[e_Association]:=Module[
	{age=If[KeyExistsQ[e,"Age"],e["Age"]/.(ThreadDepth->_)->ThreadDepth->Infinity,$Failed],
		ethnicity=If[KeyExistsQ[e,"Ethnicity"],e["Ethnicity"],$Failed],
		gender=If[KeyExistsQ[e,"Gender"],e["Gender"],All]/.Entity["Gender",x_]:>x},
	If[MemberQ[{age,gender},$Failed],
		{$Failed,$Failed,$Failed},
		{age,gender,ethnicity}
	]
]
hgentityformat[e_]:={$Failed,$Failed,$Failed}

ageformat[e_List]:=ageformat/@e
ageformat[e_DateList]:=ageformat[DateObject[e]]
ageformat[e_DateObject]:=Now-e
ageformat[e_]:=e

indexformat[z_Association, All, f_]:=Which[
	KeyExistsQ[z,"ZScore"],indexformat[z["ZScore"],"ZScore", f],
	KeyExistsQ[z,"Percentage"],indexformat[z["Percentage"], All, f],
	True,$Failed
]
indexformat[z_?NumericQ,"ZScore",f_]:=If[z>=5||z<=-5,
	Message[f::zscore,z];False,
	z
]
indexformat[Quantity[z_,"Percent"],All,f_]:=Module[{zScore=z},
	If[z>=100||z<=0,
		Message[f::percentile,Quantity[z,"Percent"],Quantity[0,"Percent"],Quantity[100,"Percent"]];
		Return[False]
	];
	PercentileToZ[zScore]
]
indexformat[z_?NumericQ,_,f_]:=Module[{zScore=z},
	If[z>=1||z<=0,
		Message[f::percentile,z,0,1];
		Return[False]
	];
	PercentileToZ[zScore*100]
]
indexformat[___]:=$Failed

Options[System`HumanGrowthData]={UnitSystem:>$UnitSystem,Method->Automatic};
System`HumanGrowthData["Properties",opt:OptionsPattern[]] :=Sort[$hgdProperties]
System`HumanGrowthData[e_List,opt:OptionsPattern[]]:=System`HumanGrowthData[#,opt]&/@e
System`HumanGrowthData[e_,opt:OptionsPattern[]] :=With[{res = oHumanGrowthData[e,All,
		Sequence @@ FilterRules[{opt}, Options[System`HumanGrowthData]]]},
    res /; res =!= $Failed
];
System`HumanGrowthData[e_List,p_?(Not[MatchQ[#,_Rule]]&),opt:OptionsPattern[]]:=System`HumanGrowthData[#,p,opt]&/@e
System`HumanGrowthData[e_,p_?(Not[MatchQ[#,_Rule]]&),opt:OptionsPattern[]] :=With[
	{res = If[StringQ[p]||p===All,oHumanGrowthData[e,p,Sequence @@ FilterRules[{opt}, Options[System`HumanGrowthData]]],
		If[MatchQ[p,_Rule],$Failed,oHumanGrowthData[e,All,p,
			Sequence @@ FilterRules[{opt}, Options[System`HumanGrowthData]]]]]},
    res /; res =!= $Failed
];
System`HumanGrowthData[e_List,p_,z_?(Not[MatchQ[#,_Rule]]&),opt:OptionsPattern[]]:=System`HumanGrowthData[#,p,z,opt]&/@e
System`HumanGrowthData[e_,p_,z_?(Not[MatchQ[#,_Rule]]&),opt:OptionsPattern[]] :=With[{res = oHumanGrowthData[e,p,z,
		Sequence @@ FilterRules[{opt}, Options[System`HumanGrowthData]]]},
    res /; res =!= $Failed
];
System`HumanGrowthData[args___,opt:OptionsPattern[]]:=With[{res = If[1<=Length[{args}]<=3,$Failed,
	(Message[General::argb, System`HumanGrowthData, Length[{args}], 1, 3]; $Failed),$Failed]},
    res /; res =!= $Failed
];

Options[oHumanGrowthData]={UnitSystem->$UnitSystem,Method->Automatic};
oHumanGrowthDataEC[entity_,opt:OptionsPattern[]]:=Module[{age,sex,ethnicity,e},
	If[MatchQ[entity, Except[_List]],
		{age,sex,ethnicity}=hgentityformat[entity];
		If[MemberQ[{age,sex},$Failed],Return[$Failed]];
		If[Not[hgageQ[age]],Message[System`HumanGrowthData::notage, age];Return[$Failed]];
		If[fgtestQ[age],Return[$Failed]];
		If[MemberQ[$sexes,sex],entity,
			Message[System`HumanGrowthData::notsex,sex,Entity["Gender", "Male"],Entity["Gender", "Female"]];Return[$Failed]],
		Which[MatchQ[entity,{_Association..}],
			e=oHumanGrowthData/@entity;
			If[FreeQ[e[[All,;;2]],$Failed],entity,$Failed],
			True,
			Message[System`HumanGrowthData::notent, entity, System`HumanGrowthData];Return[$Failed]
		]
	]
]
oHumanGrowthData[entity_,property_?propQ,opt:OptionsPattern[]]:=Module[{e=If[entity==="Properties",$Failed,oHumanGrowthDataEC[entity]],
	p=If[MemberQ[Append[$hgdProperties,All],property],property,Message[System`HumanGrowthData::notprop, property, System`HumanGrowthData];$Failed],
	age,sex,ethnicity,nationality=OptionValue[Method]/.Automatic->("Model"->"CDC"),unit=OptionValue[UnitSystem]},
	If[MemberQ[{e,p},$Failed],Return[$Failed]];
	{age,sex,ethnicity}=hgentityformat[entity];
	If[Not[MemberQ[{"Imperial","Metric"},unit]],Message[System`HumanGrowthData::unit, unit];Return[$Failed]];
	age=ageformat[age];
	ethnicity=Automatic;
	nationality=If[MatchQ[nationality,_Rule]||MatchQ[nationality,{_Rule..}],
		("Model"/.nationality)/.{"Model"->"UnitedStates","CDC"->"UnitedStates"},
		"UnitedStates"
	];
	If[sex===All,
		If[p===All,
			Association["Female"->Association[(#->oHGDCompute[{age,"Female", ethnicity, nationality,Interval},#,unit,False]&)/@$hgdProperties],
				"Male"->Association[(#->oHGDCompute[{age,"Male", ethnicity, nationality,Interval},#,unit,False]&)/@$hgdProperties]
			],
			Association["Female"->oHGDCompute[{age,"Female", ethnicity, nationality,Interval},p,unit,True],
				"Male"->oHGDCompute[{age,"Male", ethnicity, nationality,Interval},p,unit,True]
			]
		],
		If[p===All,
			Association[(#->oHGDCompute[{age,sex, ethnicity, nationality,Interval},#,unit,False]&)/@$hgdProperties],
			oHGDCompute[{age,sex, ethnicity, nationality,Interval},p,unit,True]
		]
	]
]
oHumanGrowthData[entity_,property_?propQ,index_?indexQ,opt:OptionsPattern[]]:=Module[{e=If[entity==="Properties",$Failed,oHumanGrowthDataEC[entity]],
	p=If[MemberQ[Append[$hgdProperties,All],property],property,Message[System`HumanGrowthData::notprop, property, System`HumanGrowthData];$Failed],
	age,sex,ethnicity, nationality=OptionValue[Method]/.Automatic->("Model"->"CDC"),zscore,unit=OptionValue[UnitSystem]},
	If[MemberQ[{e,p},$Failed],Return[$Failed]];
	zscore=indexformat[index,All,System`HumanGrowthData];
	If[zscore===$Failed,Message[System`HumanGrowthData::invindex,index];Return[$Failed]];
	If[zscore===False,Return[Missing["NotAvailable"]]];
	{age,sex,ethnicity}=hgentityformat[entity];
	If[Not[MemberQ[{"Imperial","Metric"},unit]],Message[System`HumanGrowthData::unit, unit];Return[$Failed]];
	age=ageformat[age];
	ethnicity=Automatic;
	nationality=If[MatchQ[nationality,_Rule]||MatchQ[nationality,{_Rule..}],
		("Model"/.nationality)/.{"Model"->"UnitedStates","CDC"->"UnitedStates"},
		"UnitedStates"
	];
	If[sex===All,
		If[p===All,
			Association["Female"->Association[(#->oHGDCompute[{age,"Female", ethnicity, nationality,zscore},#,unit,False]&)/@$hgdProperties],
				"Male"->Association[(#->oHGDCompute[{age,"Male", ethnicity, nationality,zscore},#,unit,False]&)/@$hgdProperties]
			],
			Association["Female"->oHGDCompute[{age,"Female", ethnicity, nationality,zscore},p,unit,True],
				"Male"->oHGDCompute[{age,"Male", ethnicity, nationality,zscore},p,unit,True]
			]
		],
		If[p===All,
			Association[(#->oHGDCompute[{age,sex, ethnicity, nationality,zscore},#,unit,False]&)/@$hgdProperties],
			oHGDCompute[{age,sex, ethnicity, nationality,zscore},p,unit,True]
		]
	]
]
oHumanGrowthData[entity_,property_?propQ,func_String,opt:OptionsPattern[]]:=Module[{arg,
	e=If[entity==="Properties",$Failed,oHumanGrowthDataEC[entity]],
	p=If[MemberQ[Append[$hgdProperties,All],property],property,Message[System`HumanGrowthData::notprop, property, System`HumanGrowthData];$Failed],
	age,sex,ethnicity, nationality=OptionValue[Method]/.Automatic->("Model"->"CDC"),unit=OptionValue[UnitSystem]},
	arg=If[MemberQ[{"StandardDeviation","Distribution"},func],func,Message[System`HumanGrowthData::ncomp, func];$Failed];
	If[MemberQ[{e,p,arg},$Failed],Return[$Failed]];
	{age,sex,ethnicity}=hgentityformat[entity];
	If[Not[MemberQ[{"Imperial","Metric"},unit]],Message[System`HumanGrowthData::unit, unit];Return[$Failed]];
	age=ageformat[age];
	ethnicity=Automatic;
	nationality=If[MatchQ[nationality,_Rule]||MatchQ[nationality,{_Rule..}],
		("Model"/.nationality)/.{"Model"->"UnitedStates","CDC"->"UnitedStates"},
		"UnitedStates"
	];
	If[sex===All,
		Association["Female"->oHGDCompute[{age,"Female", ethnicity, nationality,arg},p,unit,True],
			"Male"->oHGDCompute[{age,"Male", ethnicity, nationality,arg},p,unit,True]
		],
		oHGDCompute[{age,sex, ethnicity, nationality,arg},p,unit,True]
	]
]
oHumanGrowthData[entity_,property_?propQ,quantity_,opt:OptionsPattern[]]:=Module[{e=If[entity==="Properties",$Failed,oHumanGrowthDataEC[entity]],
	p=If[MemberQ[Append[$hgdProperties,All],property],property,Message[System`HumanGrowthData::notprop, property, System`HumanGrowthData];$Failed],
	age,sex,ethnicity, nationality=OptionValue[Method]/.Automatic->("Model"->"CDC"),unit=OptionValue[UnitSystem]},
	If[MemberQ[{e,p},$Failed],Return[$Failed]];
	If[Not[quantitycheckQ[quantity,p]],If[p===All,
			Message[System`HumanGrowthData::invindex,quantity],
			Message[System`HumanGrowthData::punit, quantity, p]
		];Return[$Failed]
	];
	If[p===All,Return[$Failed]];(*should never use this*)
	{age,sex,ethnicity}=hgentityformat[entity];
	If[Not[MemberQ[{"Imperial","Metric"},unit]],Message[System`HumanGrowthData::unit, unit];Return[$Failed]];
	age=ageformat[age];
	ethnicity=Automatic;
	nationality=If[MatchQ[nationality,_Rule]||MatchQ[nationality,{_Rule..}],
		("Model"/.nationality)/.{"Model"->"UnitedStates","CDC"->"UnitedStates"},
		"UnitedStates"
	];
	If[sex===All,
		Association["Female"->oHGDCompute[{age,"Female", ethnicity, nationality,quantity},p,unit,True],
			"Male"->oHGDCompute[{age,"Male", ethnicity, nationality,quantity},p,unit,True]
		],
		oHGDCompute[{age,sex, ethnicity, nationality,quantity},p,unit,True]
	]
]
oHumanGrowthData[args___]:=(System`Private`Arguments[System`HumanGrowthData[args], {1,3}];$Failed)

oHGDCompute[{age_,sex_, ethnicity_, nationality_, zscore_},p_,unit_,ms_]:=Module[{curve,range,pos,a},
	(*determine curve if multiple and cut out cases that out of range *)
	curve=hgdCurve["Age", p];
	range=hgdDomain["Age", sex, #, ethnicity, nationality]&/@curve;
	a=QuantityMagnitude[UnitConvert[age,"Months"]];
	
	(* get data *)
	If[ListQ[a],(* to return missings for values out of bounds *)
		(pos=FirstPosition[Function[{x},IntervalMemberQ[x,#]]/@QuantityMagnitude[range],True];
			If[MemberQ[{{}, Missing["NotFound"]}, pos],
				Missing["NotAvailable"],
				pos=curve[[First@pos]];
				oHGDResult[{sex, pos, ethnicity, nationality,p,unit,zscore},#]
			])&/@a,
		pos=FirstPosition[IntervalMemberQ[#,age]&/@range,True];
		If[MemberQ[{{}, Missing["NotFound"]}, pos],
			If[ms,Message[System`HumanGrowthData::range, "Age", Chop/@IntervalUnion@@range]];
			Return[Missing["NotAvailable"]],
			curve=curve[[First@pos]]
		];
		oHGDResult[{sex, curve, ethnicity, nationality,p,unit,zscore},a]
	]
]
oHGDResult[{sex_, curve_, ethnicity_, nationality_,"Weight",unit_,"StandardDeviation"},age_]:=Module[{data,values,cdf,
	sol,mu,sigma,
	ratio=QuantityMagnitude@UnitConvert[propQuantity[1,"Weight",unit],QuantityUnit[propQuantity[1,"Weight",unit]]]},
	data=HumanGrowthDataLMS[sex, curve, ethnicity, nationality][age];
	values = Table[{HumanGrowthDataFromLMS[x, data, "FromPercentile"],x},{x, 0.1, 99.9,0.1}];
	cdf = Interpolation[{#1*ratio, #2/100} & @@@ values];
	With[{v1=QuantityMagnitude@values[[1,1]]*ratio,v2=QuantityMagnitude@values[[-1,1]]*ratio},
		sol = FindFit[Table[{x, cdf'[x]}, {x, v1, v2, 0.1}], 
  			PDF[LogNormalDistribution[mu, sigma], x], {mu, sigma}, x];
  		propQuantity[sigma/.sol,"Weight",unit]
	]
]
oHGDResult[{sex_, curve_, ethnicity_, nationality_,property_,unit_,"StandardDeviation"},age_]:=Module[{data,values,sol},
	data=HumanGrowthDataLMS[sex, curve, ethnicity, nationality][age];
	sol=HumanGrowthDataFromLMS[0, data, "FromZScore"];
	values=HumanGrowthDataFromLMS[1, data, "FromZScore"]-sol;
	propQuantity[values,property,unit]
]
oHGDResult[{sex_, curve_, ethnicity_, nationality_,"Weight",unit_,"Distribution"},age_]:=Module[{data,values,cdf,
	sol,mu,sigma,
	ratio=QuantityMagnitude@UnitConvert[propQuantity[1,"Weight",unit],QuantityUnit[propQuantity[1,"Weight",unit]]]},
	data=HumanGrowthDataLMS[sex, curve, ethnicity, nationality][age];
	values = Table[{HumanGrowthDataFromLMS[x, data, "FromPercentile"],x},{x, 0.1, 99.9,0.1}];
	cdf = Interpolation[{#1*ratio, #2/100} & @@@ values];
	With[{v1=QuantityMagnitude@values[[1,1]]*ratio,v2=QuantityMagnitude@values[[-1,1]]*ratio},
		sol = FindFit[Table[{x, cdf'[x]}, {x, v1, v2, 0.1}], 
  			PDF[LogNormalDistribution[mu, sigma], x], {mu, sigma}, x];
  		LogNormalDistribution[mu, sigma]/.sol
	]
]
oHGDResult[{sex_, curve_, ethnicity_, nationality_,property_,unit_,"Distribution"},age_]:=Module[{data,values,sol,
	ratio=QuantityMagnitude@UnitConvert[propQuantity[1,property,unit],QuantityUnit[propQuantity[1,property,unit]]]},
	data=HumanGrowthDataLMS[sex, curve, ethnicity, nationality][age];
	sol=HumanGrowthDataFromLMS[0, data, "FromZScore"];
	values=HumanGrowthDataFromLMS[1, data, "FromZScore"]-sol;
	NormalDistribution[sol*ratio, values*ratio]
]
oHGDResult[{sex_, curve_, ethnicity_, nationality_,p_,unit_,quantity_Quantity},age_]:=Module[{data,values,cdf,q},
	q=quantitypart[quantity,p];
	data=HumanGrowthDataLMS[sex, curve, ethnicity, nationality][age];
	values = Table[{HumanGrowthDataFromLMS[x, data, "FromPercentile"],x},{x, 0.1, 99.9,0.1}];
	If[IntervalMemberQ[Interval[values[[{1,-1},1]]],q],
		cdf = Interpolation[{#1, #2/100} & @@@ values];
		Association["Percentile"->Quantity[cdf[q]*100,"Percent"],"ProbabilityDensity"->propQuantity[cdf'[q],{p,"ProbabilityDensity"},unit]],
		Message[System`HumanGrowthData::qrange,quantity,propQuantity[values[[1,1]],p,unit],propQuantity[values[[-1,1]],p,unit]];
		Missing["NotAvailable"]
	]
]
oHGDResult[{sex_, curve_, ethnicity_, nationality_,p_,unit_,Interval},age_]:=Module[{data,pm1,p1},
	data=HumanGrowthDataLMS[sex, curve, ethnicity, nationality][age];
	pm1=HumanGrowthDataFromLMS[-1, data, "FromZScore"];
	p1=HumanGrowthDataFromLMS[1, data, "FromZScore"];
	propQuantity[Interval[{pm1,p1}],p,unit]
]
oHGDResult[{sex_, curve_, ethnicity_, nationality_,p_,unit_,zscore_?NumericQ},age_]:=Module[{data},
	data=HumanGrowthDataLMS[sex, curve, ethnicity, nationality][age];
	data=HumanGrowthDataFromLMS[zscore, data, "FromZScore"];
	propQuantity[data,p,unit]
]

With[{s=$ProtectedSymbols},SetAttributes[s,{ReadProtected}]];
Protect@@$ProtectedSymbols;

getResourceFile["FetalGrowthData.m"];

End[]; (* End Private Context *)