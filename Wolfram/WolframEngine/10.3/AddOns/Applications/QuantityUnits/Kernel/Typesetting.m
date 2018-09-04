(*need to reinitialize MakeBoxes rules since loading Alpha code clobbered these*)
Unprotect[Quantity];
BoxForm`MakeConditionalTextFormattingRule[System`Quantity];
BoxForm`MakeConditionalTextFormattingRule[System`MixedRadixQuantity];

System`Quantity /: MakeBoxes[x:System`Quantity[_,u_?LooksLikeAUnitQ,___], fmt_] /;And[UnsameQ[$UnitMBF,True], KnownUnitQ[u]]:=
    Block[{boxes = QuantityBox[x, fmt]}, boxes /; boxes =!= $Failed]
    
System`MixedRadixQuantity /: MakeBoxes[x_System`MixedRadixQuantity, fmt_]/;UnsameQ[$UnitMBF,True] /;Quiet[ValidUnitQ[x]] :=
    Block[{boxes = QuantityBox[Evaluate[x], fmt]}, boxes /; boxes =!= $Failed]
    
Format[(q:Quantity[_,_,___?OptionQ])?ValidUnitQ,OutputForm]:=QuantityLabel[q,"Format"->"UnitString","OutputForm"->True]

(*TODO: put in TypesetInit.m*)
protected = Unprotect[TemplateBox]

$QuantityStyles = Alternatives[
	"Quantity",
	"QuantityPostfix",
	"QuantityUnitPostfix",
	"QuantityPrefix",
	"QuantityPrefixUnit",
	"QuantityPrefixPostfix",
	"QuantityPrefixUnitPostfix"
]

TemplateBox /: MakeExpression[TemplateBox[{__, interp_, _}, $QuantityStyles, ___], fmt_] := MakeExpression[interp, fmt];

Protect @@ protected;

(*============================================*)
(*primary display function for QuantityForm, called from Source/Output c-code*)
ToQuantityBox[q_?ValidUnitQ,fmt_]:=ToQuantityBox[q,fmt,{}]
ToQuantityBox[q_?ValidUnitQ,fmt_,s_String]:=ToQuantityBox[q,fmt,{s}]
ToQuantityBox[q_?ValidUnitQ,fmt_,list:{_String...}]:=
With[{specs=Sort[Union[list]],bad=Complement[list,{"Abbreviation", "LongForm", "SingularForm"}]},
 With[{display=If[TrueQ[Length[bad]>0],
	list,
	Switch[specs,
	{},"UnitAbbreviation",
	{"Abbreviation"},"UnitAbbreviation",
	{"Abbreviation","LongForm"},"UnitAbbreviationWithDescription",
	{"Abbreviation","SingularForm"},"UnitAbbreviation",
	{"Abbreviation","LongForm","SingularForm"},"UnitAbbreviationWithSingularDescription",
	{"LongForm"},"UnitDescription",
	{"LongForm","SingularForm"},"UnitSingularDescription",
	{"SingularForm"},"UnitAbbreviation",
	_, "UnitAbbreviation"]]},
	Switch[display,
	"UnitAbbreviation",
	QuantityLabel[q,"Format"->"FullUnitLabel"],
	"UnitDescription",
	QuantityLabel[q,"Format"->"UnitString"],
	"UnitAbbreviationWithDescription",
	QuantityLabel[q,"Format"->"FullUnitLabelWithDescription"],
	"UnitSingularDescription",
	QuantityLabel[q,"Format"->"UnitString","Singular"->True],
	"UnitAbbreviationWithSingularDescription",
	QuantityLabel[q,"Format"->"FullUnitLabelWithDescription","Singular"->True],
	{"PlotLabel"},
	With[{un=QuantityUnit[q]},
		TooltipBox[QuantityLabel[un,"Format"->"FullUnitLabel"],QuantityLabel[un,"Format"->"UnitString"]]],
	_,
	(Message[QuantityForm::form,display];Block[{$UnitMBF=True},
	RowBox[{"QuantityForm","[",RowBox[{MakeBoxes[q,fmt],",",MakeBoxes[list,fmt]}],"]"}]])
]]]

ToQuantityBox[un_?KnownUnitQ,fmt_]:=ToQuantityBox[un,fmt,{}]
ToQuantityBox[un_?KnownUnitQ,fmt_,s_String]:=ToQuantityBox[un,fmt,{s}]
ToQuantityBox[un_?KnownUnitQ,fmt_,list:{_String...}]:=
With[{specs=Sort[Union[list]],bad=Complement[list,{"Abbreviation", "LongForm", "SingularForm"}]},
 With[{display=If[TrueQ[Length[bad]>0],
	list,
	Switch[specs,
	{},"UnitAbbreviation",
	{"Abbreviation"},"UnitAbbreviation",
	{"Abbreviation","LongForm"},"UnitAbbreviationWithDescription",
	{"Abbreviation","SingularForm"},"UnitAbbreviation",
	{"Abbreviation","LongForm","SingularForm"},"UnitAbbreviationWithSingularDescription",
	{"LongForm"},"UnitDescription",
	{"LongForm","SingularForm"},"UnitSingularDescription",
	{"SingularForm"},"UnitAbbreviation",
	_, "UnitAbbreviation"]]},
Switch[display,
	"UnitAbbreviation",
	QuantityLabel[un,"Format"->"FullUnitLabel"],
	"UnitDescription",
	QuantityLabel[un,"Format"->"UnitString"],
	"UnitAbbreviationWithDescription",
	QuantityLabel[un,"Format"->"FullUnitLabelWithDescription"],
	"UnitSingularDescription",
	QuantityLabel[un,"Format"->"UnitString","Singular"->True],
	"UnitAbbreviationWithSingularDescription",
	QuantityLabel[un,"Format"->"FullUnitLabelWithDescription","Singular"->True],
	{"PlotLabel"},
	TooltipBox[QuantityLabel[un,"Format"->"FullUnitLabel"],QuantityLabel[un,"Format"->"UnitString"]],
	_,
	(Message[QuantityForm::form,display];RowBox[{"QuantityForm","[",RowBox[{MakeBoxes[un,fmt],",",MakeBoxes[list,fmt]}],"]"}])
]]]

ToQuantityBox[q_?ValidUnitQ,fmt_,other___]:=(Message[QuantityForm::form,other];RowBox[{"QuantityForm","[",RowBox[{MakeBoxes[q,fmt],",",MakeBoxes[other,fmt]}],"]"}])
ToQuantityBox[un_?KnownUnitQ,fmt_,other___]:=(Message[QuantityForm::form,other];RowBox[{"QuantityForm","[",RowBox[{MakeBoxes[un,fmt],",",MakeBoxes[other,fmt]}],"]"}])
ToQuantityBox[nq_,fmt_,list:Except[_List]]/;MemberQ[{"Abbreviation", "LongForm", "SingularForm","PlotLabel"},list]:=ToQuantityBox[nq,fmt,{list}]
ToQuantityBox[nq_,fmt_,list_List]/;Complement[list,{"Abbreviation", "LongForm", "PlotLabel", "SingularForm"}]==={}:=Block[{Quantity},
	Quantity/:MakeBoxes[x:(_Quantity)?ValidUnitQ,fmt]/;UnsameQ[$UnitMBF,True]:=ToQuantityBox[x,fmt,list];
	MakeBoxes[nq,fmt]
]
ToQuantityBox[nq_,fmt_,other___]:=(Message[QuantityForm::form,other];RowBox[{"QuantityForm","[",RowBox[{MakeBoxes[nq,fmt],",",MakeBoxes[other,fmt]}],"]"}])
ToQuantityBox[other___]:=(Message[QuantityForm::notunit];RowBox[{"QuantityForm","[",MakeBoxes[other],"]"}])

(*OutputForm display function for QuantityForm, called from Source/Output c-code (ToString[QuantityForm[...]])*)
ToQuantityString[q_?ValidUnitQ]:=ToQuantityString[q,{}]
ToQuantityString[q_?ValidUnitQ,s_String]:=ToQuantityString[q,{s}]
ToQuantityString[q_?ValidUnitQ,list:{_String...}]:=
With[{specs=Sort[Union[list]],bad=Complement[list,{"Abbreviation", "LongForm", "SingularForm"}]},
 With[{display=If[TrueQ[Length[bad]>0],
	list,
	Switch[specs,
	{},"UnitAbbreviation",
	{"Abbreviation"},"UnitAbbreviation",
	{"Abbreviation","LongForm"},"UnitAbbreviationWithDescription",
	{"Abbreviation","SingularForm"},"UnitAbbreviation",
	{"Abbreviation","LongForm","SingularForm"},"UnitAbbreviationWithSingularDescription",
	{"LongForm"},"UnitDescription",
	{"LongForm","SingularForm"},"UnitSingularDescription",
	{"SingularForm"},"UnitAbbreviation",
	_, "UnitAbbreviation"]]},
Switch[display,
	"UnitAbbreviation",
	getStringForm[QuantityLabel[q,"Format"->"FullUnitLabel"]],
	"UnitDescription",
	QuantityLabel[q,"Format"->"UnitString","OutputForm"->True],
	"UnitAbbreviationWithDescription",
	getStringForm[QuantityLabel[q,"Format"->"FullUnitLabelWithDescription"]],
	"UnitSingularDescription",
	QuantityLabel[q,"Format"->"UnitString","Singular"->True],
	"UnitAbbreviationWithSingularDescription",
	getStringForm[QuantityLabel[q,"Format"->"FullUnitLabelWithDescription","Singular"->True]],
	_,
	(Message[QuantityForm::form,display];"QuantityForm["<>ToString[q,InputForm]<>","<>ToString[display]<>"]")
]]]


ToQuantityString[un_?KnownUnitQ]:=ToQuantityString[un,{}]
ToQuantityString[un_?KnownUnitQ,s_String]:=ToQuantityString[un,{s}]
ToQuantityString[un_?KnownUnitQ,list:{_String...}]:=
With[{specs=Sort[Union[list]],bad=Complement[list,{"Abbreviation", "LongForm", "SingularForm"}]},
 With[{display=If[TrueQ[Length[bad]>0],
	list,
	Switch[specs,
	{},"UnitAbbreviation",
	{"Abbreviation"},"UnitAbbreviation",
	{"Abbreviation","LongForm"},"UnitAbbreviationWithDescription",
	{"Abbreviation","SingularForm"},"UnitAbbreviation",
	{"Abbreviation","LongForm","SingularForm"},"UnitAbbreviationWithSingularDescription",
	{"LongForm"},"UnitDescription",
	{"LongForm","SingularForm"},"UnitSingularDescription",
	{"SingularForm"},"UnitAbbreviation",
	_, "UnitAbbreviation"]]},
	Switch[display,
	"UnitAbbreviation",
	getStringForm[QuantityLabel[un,"Format"->"FullUnitLabel"]],
	"UnitDescription",
	QuantityLabel[un,"Format"->"UnitString"],
	"UnitAbbreviationWithDescription",
	getStringForm[QuantityLabel[un,"Format"->"FullUnitLabelWithDescription"]],
	"UnitSingularDescription",
	QuantityLabel[un,"Format"->"UnitString","Singular"->True],
	"UnitAbbreviationWithSingularDescription",
	getStringForm[QuantityLabel[un,"Format"->"FullUnitLabelWithDescription","Singular"->True]],
	_,
	(Message[QuantityForm::form,display];"QuantityForm["<>ToString[un,InputForm]<>","<>ToString[display]<>"]")
]]]

ToQuantityString[nq_,list:Except[_List]]/;MemberQ[{"Abbreviation", "LongForm", "PlotLabel", "SingularForm"},list]:=ToQuantityString[nq,{list}]
ToQuantityString[nq_,list_List]/;Complement[list,{"Abbreviation", "LongForm",  "PlotLabel", "SingularForm"}]==={}:=Block[{Quantity},
	Format[(q:Quantity[_,_,___?OptionQ])?ValidUnitQ,OutputForm]:=ToQuantityString[q,list];
	ToString[nq]
]
ToQuantityString[q_?ValidUnitQ,other___]:=(Message[QuantityForm::form,other];"QuantityForm["<>ToString[q,InputForm]<>","<>ToString[other]<>"]")
ToQuantityString[un_?KnownUnitQ,other___]:=(Message[QuantityForm::form,other];"QuantityForm["<>ToString[un,InputForm]<>"]")
ToQuantityString[a1_,other_]:=(Message[QuantityForm::notunit];"QuantityForm["<>ToString[a1,InputForm]<>","<>ToString[other,InputForm]<>"]")
ToQuantityString[other_]:=(Message[QuantityForm::notunit];"QuantityForm["<>ToString[other,InputForm]<>"]")

getStringForm[label_]:=ToString[DisplayForm[label],NumberMarks->False]



$UnitDisplayCache=System`Utilities`HashTable[];
$UnitDisplayCacheSize=0;
$MaxCacheSize=50;

SetAttributes[UnitDisplayCacheContainsQ,HoldAll];
UnitDisplayCacheContainsQ[unit_]:=System`Utilities`HashTableContainsQ[$UnitDisplayCache,HoldForm[unit]]
UnitDisplayCacheContainsQ[___]:=False

UnitDisplayCacheAdd[heldform_,boxes_]:=(If[TrueQ[$UnitDisplayCacheSize>=$MaxCacheSize],
	Clear[$UnitDisplayCache];$UnitDisplayCache=System`Utilities`HashTable[];$UnitDisplayCacheSize=1;,
	$UnitDisplayCacheSize++];
	System`Utilities`HashTableAdd[$UnitDisplayCache,heldform,boxes]);
UnitDisplayCacheGet[heldform_]:=System`Utilities`HashTableGet[$UnitDisplayCache,heldform]

SetAttributes[getUnitDisplayForm,HoldAll];
getUnitDisplayForm[unit_,1] := QuantityUnitBox[Quantity[1,unit],StandardForm] (*don't used cached name for unity values*)

getUnitDisplayForm[unit_,None] := With[{hf=HoldForm[unit]},
	UnitDisplayCacheGet[hf]/.{
		ToString[$UnitDisplayTemporaryVariable]->InterpretationBox["\[InvisibleSpace]", 1],
		ToString[$UnitDisplayTemporaryValue] ->None}]
		
getUnitDisplayForm[unit_,n_]:=With[{hf=HoldForm[unit]},
	UnitDisplayCacheGet[hf]/.{
		ToString[$UnitDisplayTemporaryVariable]->makeNumberValue[n, unit], 
		ToString[$UnitDisplayTemporaryValue] -> MakeBoxes[n]}]

(*SetAttributes[makeNumberValue, HoldAll]*)

$2digitCurrencyPattern = Alternatives["USDollars", "Euros",
	"AfghanAfghani", "AlbanianLeke", "ArmenianDram", "NetherlandsAntillesGuilders", "AngolaKwanzaReajustado",
	"AngolaNewKwanza", "AngolanKwanza", "AngolanKwanza1990", "ArgentinaAustral", "ArgentinaPesoArgentino",
	"ArgentinaPesoLey", "ArgentinaPesoMonedaNacional", "ArgentinePesos", "AustralianDollars", "ArubanFlorins",
	"AustralianPound", "NetherlandsAntilleanGuilder", "AzerbaijanAzerbaijanianManat", "AzerbaijaniManat",
	"AzerbaijanSovietRuble", "BosnianHerzegovinianMaraka", "BarbadianDollars", "BangladeshiTaka", "BulgarianLev1999",
	"BulgarianLeva", "BermudaDollars", "BruneiDollars", "BolivianBolivianos", "BolivianBolivianos1962", 
	"BoliviaPesoBoliviano", "BrazilCruzado", "BrazilCruzeiro", "BrazilCruzeiroNovo", "BrazilCruzeiroReal",
	"BrazilianReais", "BrazilNewCruzado", "BahamianDollars", "BhutanNgultrum", "BotswanaPula", "BelizeDollars",
	"CanadianDollars", "CongoleseFranc", "CongoleseFranc1967", "ZaireanNewZaire", "ZaireanZaire", "SwissFrancs",
	"ChineseYuan", "ColombianPesos", "CostaRicanColones", "CubanConvertiblePesos", "CubanPesos", "CzechKoruny",
	"DanishKroner", "DominicanPesos", "AlgerianDinars", "EgyptianPounds", "EritreanNakfas", "EthiopianBirr",
	"EstonianKrooni", "LatvianLati", "LithuanianLitai", "SlovakKorun", "SlovenianTolarjev", "FijianDollars",
	"FalklandIslandsPounds", "BritishPounds", "GibraltarPounds", "GuernseyPounds", "IsleOfManPounds", 
	"JerseyPounds", "GeorgiaGeorgianCoupon", "GeorgianLari", "GeorgiaSovietRuble", "GhanaCedi1967", "GhanaCedi2007",
	"GhanaGhanaianPound", "GhanianCedis", "GambianDalasi", "GambianPound", "GuatemalaQuetzales", "GuyaneseDollars",
	"HongKongDollars", "HonduranLempiras", "CroatiaCroatianDinar", "CroatianKuna", "CroatiaYugoslavDinar", 
	"CroatiaYugoslavHardDinar", "HaitiGourdes", "HungarianForints", "IndonesianRupiahs", "IndonesiaOldRupiah",
	"IsraeliLira", "IsraeliShekels", "OldShekel", "IndianRupees", "IranianRials", "JamaicanDollars", "JamaicanPound",
	"EastAfricanShilling", "KenyanShillings", "KyrgyzstanSom", "CambodianRiels", "NorthKoreanWon", "CaymanIslandsDollars",
	"KazakhstanTenge", "LaoKips", "LebanesePounds", "SriLankaRupees", "LiberianDollars", "LesothoMaloti", "LibyanPound",
	"MoroccanDirhams", "MoldovanLei", "MacedonianDenars", "MyanmarKyat", "MongolianTugrik", "MacauPatacas", 
	"MaldivesRufiyaa", "MalawiKwacha", "MexicanPesos", "MexicanPesos1992", "MalayaAndBritishBorneoDollar", 
	"MalaysianRinggits", "MozambiqueEscudo", "MozambiqueMeticais", "MozambiqueMetical", "NamibianDollars",
	"NigeriaNigerianPound", "NigerianNaira", "NicaraguaCordobas", "NorwegianKroner", "NepaleseRupees", "CookIslandsDollars",
	"NewZealandDollars", "NewZealandPound", "PitcairnIslandsDollars", "PeruInti", "PeruvianSoles", "PeruvianSoles1985",
	"PapuaNewGuineaKina", "PhilippinePesos", "PakistaniRupees", "PolishZlotych", "PolishZlotych1994", "QatarRials",
	"RomanianLei", "SerbianDinars", "RussianRubles", "RussianRubles1997", "RussianSovietRubles", "SaudiArabianRiyals",
	"SolomonIslandsDollars", "SeychellesRupees", "SudanesePounds", "SudanesePounds1992", "SudanSudaneseDinar", 
	"SwedishKronor", "SingaporeDollars", "SaintHelenaPounds", "SierraLeoneLeones", "SomaliShillings", "SurinameDollars",
	"SurinameSurinamGuilder", "SaoTomeDobras", "ElSalvadorColones", "SyrianPounds", "SwazilandEmalangeni", "ThaiBaht",
	"TajikistanSomoni", "TurkmenManat", "TongaPaanga", "TurkeyOldTurkishLira", "TurkishLiras", "BritishWestIndiesDollar", 
	"TrinidadTobagoDollars", "TaiwanDollars", "TanzanianShillings", "UkraineHryven", "UkraineKarbovanet",
	"UkraineSovietRuble", "UruguayanPesos", "UruguayanPesos1972", "UruguayNuevoPeso", "UzbekistanSom", 
	"VenezuelanBolivares", "SamoanTala", "EastCaribbeanDollars", "SouthYemeniDinar", "SouthAfricanPound",
	"SouthAfricanRand", "ZambianKwacha", "ZambianKwacha2012"
];
$3digitCurrencyPattern = Alternatives[
	"BahrainiDinars", "IraqiDinars", "JordanianDinars", "KuwaitiDinars", "LibyanDinars", "OmaniRials", "TunisianDinars"
];
SetAttributes[makeNumberValue, HoldFirst];

makeNumberValue[n_Real, $2digitCurrencyPattern] := With[{boxes = Parenthesize[
	(* per mtrott: 	As we will only do this for money and because virtually all sensible amounts of money 
	are less than a trillion dollars*)
	NumberForm[n, {Infinity, 2},  ExponentFunction -> (If[-2<= # <= 12, Null, #] &)], 
	StandardForm, Mod]},
	InterpretationBox[boxes, n, Selectable -> False]
]
makeNumberValue[n_Real, $3digitCurrencyPattern] := With[{boxes = Parenthesize[
	(* per mtrott: 	As we will only do this for money and because virtually all sensible amounts of money 
	are less than a trillion dollars*)
	NumberForm[n, {Infinity, 3},  ExponentFunction -> (If[-3<= # <= 12, Null, #] &)], 
	StandardForm, Mod]},
	InterpretationBox[boxes, n, Selectable -> False]
]
makeNumberValue[n_, _] := Parenthesize[n, StandardForm, Mod]


(*box constructor called from Startup/typesetinit.m *)
SetAttributes[QuantityBox,HoldAllComplete];

QuantityBox[q:Quantity[n_,unit_?UnitDisplayCacheContainsQ],StandardForm]:=Quiet[getUnitDisplayForm[unit,n],{Part::partw}]
QuantityBox[q:Quantity[n_,unit_?KnownUnitQ,___?OptionQ],fmt__]:=QuantityUnitBox[q,fmt]
	
QuantityBox[q:Quantity[___],fmt__]:= Block[{$UnitMBF=True},MakeBoxes[q,fmt]]
QuantityBox[other_,fmt__]:=MakeBoxes[other,fmt]

SetAttributes[LooksLikeAUnitQ,HoldAllComplete];(*utility to avoid unneeded evaluation leaks in typesetting*)
LooksLikeAUnitQ[__]=False
LooksLikeAUnitQ[_String]=True
LooksLikeAUnitQ[_Times]=True
LooksLikeAUnitQ[_Power]=True
LooksLikeAUnitQ[_Divide]=True
LooksLikeAUnitQ[_DatedUnit]=True
LooksLikeAUnitQ[_IndependentUnit]=True
LooksLikeAUnitQ[_MixedRadix]=True
LooksLikeAUnitQ[1]=True

(*function for looking up various properties from Alpha UnitTables, mostly for display boxes*)
Options[unitinfo] = {"Output" -> Default};
unitinfo[num_,u_, opts___?OptionQ]:=With[{unit=u/.{HoldPattern[Divide[x_,y_]]:>Times[x,Power[y,-1]], DatedUnit[unit_,_]:>unit}},
	AlphaBlock[
	Module[
		{res=CalculateScan`UnitScanner`Private`UnitInformation[num,unit,"SingularTest" -> (MatchQ[#, 1]&),opts]},
		If[Head[res]=!=CalculateScan`UnitScanner`Private`UnitInformation,
			res/.{RowBox[List["RowNoSeparators", "["]]->"",RowBox[List["\[InvisibleSpace]", "\[ThickSpace]", "\[InvisibleSpace]"]]->""},
			Message[unitinfo::failed];$Failed,
			Message[unitinfo::failed];$Failed]
		]
]]

(*these units use characters which aren't included with standard Windows fonts*)
$fonttroubleunits={CalculateUnits`UnitCommonSymbols`PercentPercent,CalculateUnits`UnitCommonSymbols`ApothecariesPounds,
	 CalculateUnits`UnitCommonSymbols`ApothecariesOunces, CalculateUnits`UnitCommonSymbols`ApothecariesScruples};

unitinfo[num_,unit_,"Boxes"]:=If[hasFontTroubleUnitQ[unit],
	StyleBox[getunitBoxes[num,unit],FontFamily->"Arial Unicode MS"],
	getunitBoxes[num,unit]]

getunitBoxes[num_,unit_]:=With[{b=unitinfo[num,unit,"Output"->"UnitBoxes"]},
	If[b==="",
		ToString[unitinfo[num,unit,"Output"->"LongName"],InputForm],
		b]
]

(*display box for IndependentUnits*)
freeUnitFrameBox[s_String]:=FrameBox[StyleBox[MakeBoxes[s],ShowStringCharacters -> False], 
     FrameMargins -> 1,
     FrameStyle -> GrayLevel[0.85],
     BaselinePosition -> Baseline,
     RoundingRadius -> 3,
     StripOnInput -> False]
     
FreeUnitForm/:MakeBoxes[FreeUnitForm[x_String],_]:=freeUnitFrameBox[x]

(*alpha UnitLookup overloaded to return appropriate labels for IndependentUnit inputs*)
CalculateUnits`UnitCommonSymbols`UnitLookup[IndependentUnit[s_String], _, "UnitToString"] := s
CalculateUnits`UnitCommonSymbols`UnitLookup[IndependentUnit[s_String], "UnitShortName"] := FreeUnitForm[s]
CalculateUnits`UnitCommonSymbols`UnitLookup[IndependentUnit[s_String], "UnitPluralName"] := s
CalculateUnits`UnitCommonSymbols`UnitLookup[IndependentUnit[s_String], "UnitSingularName"] := s
CalculateUnits`UnitCommonSymbols`UnitLookup[DatedUnit[unit_, _], args__] := CalculateUnits`UnitCommonSymbols`UnitLookup[unit, args]
CalculateUnits`UnitCommonSymbols`KnownUnit0Q[IndependentUnit[_String]] := True

Clear[CalculateUnits`UnitCommonSymbols`ToCompoundUnit];
CalculateUnits`UnitCommonSymbols`ToCompoundUnit[_]={}

(*used to prevent Alpha form output from escaping, shouldn't ever end up with this being output*)
SetAttributes[qlsafe,HoldFirst];

qlsafe[arg1_,args___]:=Module[
	{ql=QuantityLabel[arg1,args]},
	If[Head[ql]=!=QuantityLabel,ql,With[{u=Quiet[Check[ql[[1,2]],"unk"]]},Message[Quantity::unkunit,u]];"UnknownUnit",Message[qlsafe::failed];$Failed]]


(*QuantityLabel is used to create the various display forms associated with Quantity & unit labels*)
Options[QuantityLabel] = {Format -> Automatic, "Singular"->False, "OutputForm"->False};
Attributes[QuantityLabel] = {HoldAll};

(*allows HoldForm, for things like Meters/Meters*)
QuantityLabel[HoldForm[args__],opts___]:=QuantityLabel[args,opts]

QuantityLabel[Quantity[val_,IndependentUnit[s_String],qopts___?OptionQ],OptionsPattern[]]:=Switch[OptionValue[Format],
	"UnitString",
	ToString[val]<>" "<>s,
	"FullUnitLabelWithDescription",
	RowBox[{MakeBoxes[val]," ",freeUnitFrameBox[s]}],
	"UnitLabel",
	RowBox[{MakeBoxes[val]," ",freeUnitFrameBox[s]}],
	"UnitPrefixLabel",
	"",
	"UnitPostfixLabel",
	"",
	"FullUnitLabel",
	RowBox[{MakeBoxes[val]," ",freeUnitFrameBox[s]}],
	"TypesetUnit"|Automatic,
	Row[{val," ",s}],
	_,
	Message[QuantityLabel::bdfmt,OptionValue[Format]];$Failed]
	
QuantityLabel[IndependentUnit[s_String],OptionsPattern[]]:=Switch[OptionValue[Format],
	"UnitPrefixLabel",
	"",
	"UnitLabel",
	freeUnitFrameBox[s],
	"UnitPostfixLabel",
	"",
	"FullUnitLabel",
	freeUnitFrameBox[s],
	"FullUnitLabelWithDescription",
	RowBox[{"",freeUnitFrameBox[s],"",StyleBox[RowBox[{" (",MakeBoxes[s],")"}],FontColor -> GrayLevel[0.5]]}],
	"TypesetUnit"|Automatic|"UnitString",
	s,
	_,
	Message[QuantityLabel::bdfmt,OptionValue[Format]];$Failed]
	
(*protection against edge-cases where independent units sneak into alpha formatting*)
QuantityLabel[IndependentUnit[s_Symbol],OptionsPattern[]]/;KnownUnitQ[s]:=""

QuantityLabel[DatedUnit[u_,___],o:OptionsPattern[]]:=QuantityLabel[u,o]
QuantityLabel[Quantity[val_,DatedUnit[u_,___],qopts___?OptionQ],o:OptionsPattern[]] := QuantityLabel[Quantity[val,u,qopts],o]

(*general case*)
QuantityLabel[unit:(_String|_Power|_Times|_Divide), opts:OptionsPattern[]]/;KnownUnitQ[unit] := 
Module[
	{exp=toAlphaUnitPart[unit],ul,num=If[TrueQ[OptionValue["Singular"]],1,2,2]},
	If[Head[exp]===Times||Head[exp]===Integer,
		Switch[OptionValue[Format],
			"UnitString",
			With[{un=toAlphaUnit[unit]},unitinfo[num,un, "Output" -> "LongName"]],
			"UnitLabel",
			With[{un=toAlphaUnit[unit]},unitinfo[num,un,"Boxes"]],
			"UnitPrefixLabel",
			With[{un=toAlphaUnit[unit]},unitinfo[num,un,"Output" -> "PrefixBoxes"]],
			"UnitPostfixLabel",
			With[{un=toAlphaUnit[unit]},unitinfo[num,un,"Output" -> "PostfixBoxes"]],
			"TypesetUnit",
			unitFunction2String[exp],
			"FullUnitLabel",
			UnitAbbreviation[unit,opts],
			"FullUnitLabelWithDescription",
			With[{un=toAlphaUnit[unit]},
				RowBox[{UnitAbbreviation[unit,opts],
					StyleBox[RowBox[{
						"(",
						unitinfo[num,un, "Output" -> "LongName"],
						")"}],
						FontColor -> GrayLevel[0.5]]}]],
			"TypesetUnit"|Automatic,
			unitFunction2String[unit],
			_,
			Message[QuantityLabel::bdfmt,OptionValue[Format]];$Failed
		],
		Switch[OptionValue[Format],
			"UnitLabel",
			With[{un=toAlphaUnit[unit]},unitinfo[num,un,"Boxes"]],
			"UnitPrefixLabel",
			With[{un=toAlphaUnit[unit]},unitinfo[num,un,"Output" -> "PrefixBoxes"]],
			"UnitPostfixLabel",
			With[{un=toAlphaUnit[unit]},unitinfo[num,un,"Output" -> "PostfixBoxes"]],
			"FullUnitLabel",
			UnitAbbreviation[unit,opts],
			"FullUnitLabelWithDescription",
			With[{un=toAlphaUnit[unit]},
				RowBox[{UnitAbbreviation[unit,opts],
					StyleBox[RowBox[{"(",unitinfo[num,un, "Output" -> "LongName"],")"}],FontColor -> GrayLevel[0.5]]}]],			
			"TypesetUnit"|Automatic|"UnitString",
			If[Head[exp]=!=Symbol&&Head[exp]=!=Power,
			unit,
			ul=With[{un=toAlphaUnit[unit]},unitinfo[num,un, "Output" -> "LongName"]];
			If[
				Head[ul]=!=UnitLookup,
				ul,
				Message[Quantity::unkunit,unit];exp
			]
			],
			_,
			Message[QuantityLabel::bdfmt,OptionValue[Format]];$Failed]
		]
	]
	
(*general case for just unit*)
QuantityLabel[unit_,opts:OptionsPattern[]]/;Head[unit]=!=Quantity/;KnownUnitQ[unit]:= 
Block[{$ContextPath=Prepend[$ContextPath, "CalculateUnits`UnitCommonSymbols`"]},
	Module[{exp=If[Head[unit]===CalculateParse`Content`Calculate`Unit,unit[[1]],unit],ul,num=If[TrueQ[OptionValue["Singular"]],1,2,2]},
	If[Head[exp]===Times||Head[exp]===Integer,
		Switch[OptionValue[Format],
			"UnitString",
			unitinfo[num,unit, "Output" -> "LongName"],
			"UnitLabel",
			unitinfo[num,unit,"Boxes"],
			"UnitPrefixLabel",
			unitinfo[num,unit,"Output" -> "PrefixBoxes"],
			"UnitPostfixLabel",
			unitinfo[num,unit,"Output" -> "PostfixBoxes"],
			"TypesetUnit",
			unitFunction2String[QuantityUnit@fromAlphaQuantity[Quantity[1,unit]]],
			"FullUnitLabel",
			UnitAbbreviation[unit,opts],
			"FullUnitLabelWithDescription",
			RowBox[{UnitAbbreviation[unit,opts],
				StyleBox[RowBox[{"(",unitinfo[num,unit, "Output" -> "LongName"],")"}],FontColor -> GrayLevel[0.5]]}],
			"TypesetUnit"|Automatic,
			With[{u=QuantityUnit@fromAlphaQuantity[Quantity[1,unit]]},unitFunction2String[u]],
			_,
			Message[QuantityLabel::bdfmt,OptionValue[Format]];$Failed
		],
		Switch[OptionValue[Format],
			"UnitLabel",
			unitinfo[num,unit,"Boxes"],
			"UnitPrefixLabel",
			unitinfo[num,unit,"Output" -> "PrefixBoxes"],
			"UnitPostfixLabel",
			unitinfo[num,unit,"Output" -> "PostfixBoxes"],
			"FullUnitLabel",
			UnitAbbreviation[unit,opts],
			"FullUnitLabelWithDescription",
			RowBox[{UnitAbbreviation[unit,opts],
				StyleBox[RowBox[{"(",unitinfo[num,unit, "Output" -> "LongName"],")"}],FontColor -> GrayLevel[0.5]]}],
			"TypesetUnit"|Automatic|"UnitString",
			If[Head[exp]=!=Symbol&&Head[exp]=!=Power,
			unit,
			ul=unitinfo[num,unit,"Output"->"LongName"];
			If[Head[ul]=!=UnitLookup,ul,Message[Quantity::unkunit,unit];exp]
			],
			_,
			Message[QuantityLabel::bdfmt,OptionValue[Format]];$Failed]
		]
	]]
	
QuantityLabel[s_Symbol]/;KnownUnitQ[CalculateParse`Content`Calculate`Unit[s]]:=QuantityLabel[CalculateParse`Content`Calculate`Unit[s]]
	
QuantityLabel[q:Quantity[mag_,unit:(_Times|_Power|_String|_Divide),qopts___?OptionQ],opts___?OptionQ]:=With[{quant=toAlphaQuantity[q]},QuantityLabel[quant,opts]]

(*pattern used to check for various spaces in formatting*)
whitespacepattern=(WhitespaceCharacter|"\[ThinSpace]"|""|"\[ThickSpace]"|"\[InvisibleSpace]");	
	
(*general case for alpha-form quantities*)
QuantityLabel[Quantity[mag_,unit:CalculateParse`Content`Calculate`Unit[_],qopts___?OptionQ], opts:OptionsPattern[]] := 
Switch[OptionValue[Format],
	"UnitString",
(*	StringJoin[{ToString[mag,If[OptionValue["OutputForm"],OutputForm,InputForm,InputForm]]," ",QuantityLabel[unit,opts]}],*)
	ToString[StringForm["`1` `2`",mag,QuantityLabel[unit,opts]],If[OptionValue["OutputForm"],OutputForm,StandardForm,StandardForm]],
	"UnitLabel",
	RowBox[{MakeBoxes[mag]," ",QuantityLabel[unit,opts]}],
	"UnitPrefixLabel",
	RowBox[{QuantityLabel[unit,opts],MakeBoxes[mag]}],
	"UnitPostfixLabel",
	RowBox[{MakeBoxes[mag],QuantityLabel[unit,opts]}],
	"FullUnitLabel",
	(QuantityAbbreviation[mag,unit,opts]/.Slot[1]->MakeBoxes[mag]),
	"FullUnitLabelWithDescription",
	With[{box=(QuantityAbbreviation[mag,unit,opts]/.Slot[1]->MakeBoxes[mag])},
	RowBox[{box,StyleBox[RowBox[{"(",QuantityLabel[unit,"Format"->"UnitString","Singular"->OptionValue[Singular]],")"}],FontColor -> GrayLevel[0.5]]}]],
	"TypesetUnit"|Automatic,
	Row[{mag," ",QuantityLabel[unit,opts]}],
	_,
	Message[QuantityLabel::bdfmt,OptionValue[Format]];$Failed
]

QuantityLabel[list_List, opts___?OptionQ] := Row[QuantityLabel[#,opts]&/@list]

(*general case for singular-cases*)
QuantityLabel[Quantity[one_,u:CalculateParse`Content`Calculate`Unit[unit_],qopts___?OptionQ], opts:OptionsPattern[]]/;Abs[one]===1:=
Switch[OptionValue[Format],
	"UnitString",
	StringJoin[{ToString[one]," ",QuantityLabel[u,"Singular"->True,opts]}],
	"UnitLabel",
	RowBox[{MakeBoxes[one]," ",QuantityLabel[u,"Singular"->True,opts]}],
	"UnitPrefixLabel",
	RowBox[{QuantityLabel[u,"Singular"->True,opts],MakeBoxes[one]}],
	"UnitPostfixLabel",
	RowBox[{MakeBoxes[one],QuantityLabel[u,"Singular"->True,opts]}],
	"FullUnitLabel",
	(QuantityAbbreviation[1,u]/.Slot[1]->MakeBoxes[one]),
	"FullUnitLabelWithDescription",
	With[{box=(QuantityAbbreviation[1,u]/.Slot[1]->MakeBoxes[one])},
	RowBox[{box,StyleBox[RowBox[{"(",QuantityLabel[u,"Format"->"UnitString","Singular"->True],")"}],FontColor -> GrayLevel[0.5]]}]],
	"TypesetUnit"|Automatic,
	Row[{one," ",QuantityLabel[u,"Singular"->True,opts]}],
	_,
	Message[QuantityLabel::bdfmt,OptionValue[Format]];$Failed
]

QuantityLabel[q:Quantity[i:Interval[{lower_MixedRadix,upper_MixedRadix}], unit_MixedRadix], opts:OptionsPattern[]] := With[
	{box1 = QuantityLabel[Quantity[lower, unit],opts], box2 = QuantityLabel[Quantity[upper, unit],opts]},
	Switch[OptionValue[Format],
	"UnitString",
	StringJoin[box1," to ", box2],
	_,
	RowBox[{box1, " to ", box2}]
]
]

QuantityLabel[q_Quantity,opts:OptionsPattern[]]/;MixedRadixQ[q]:=Switch[
	OptionValue[Format],
	"UnitString",
	StringJoin[Riffle[QuantityLabel[#,opts]&/@mixedRadix2List[q]," "]],
	"UnitLabel",
	RowBox[Riffle[QuantityLabel[#,opts]&/@mixedRadix2List[q]," "]],
	"UnitPrefixLabel",
	RowBox[Riffle[QuantityLabel[#,opts]&/@mixedRadix2List[q]," "]],
	"UnitPostfixLabel",
	RowBox[Riffle[QuantityLabel[#,opts]&/@mixedRadix2List[q]," "]],
	"FullUnitLabel",
	MixedRadixLabel[q],
	"FullUnitLabelWithDescription",
	With[{names=Riffle[With[{u=#[[2]]},QuantityLabel[u,"Format"->"UnitString"]]&/@mixedRadix2List[q],","]},
	RowBox[{MixedRadixLabel[q],StyleBox[RowBox[{"(",Sequence@@names,")"}],FontColor -> GrayLevel[0.5]]}]],
	"TypesetUnit"|Automatic,
	Row[Riffle[QuantityLabel[#,opts]&/@mixedRadix2List[q]," "]],
	_,
	Message[QuantityLabel::bdfmt,OptionValue[Format]];$Failed
]

MixedRadixLabel[q_?MixedRadixQ]:=With[{mags=QuantityMagnitude[q],units=QuantityUnit[q]},
	With[{bx=RowBox[{Sequence@@(RowBox/@Transpose[{List@@mags,MixedRadixBox/@(List@@units)}])}]},
		StyleBox[bx,ShowStringCharacters->False]]]

mixedRadix2List[q_Quantity]/;MixedRadixQ[q]:=Quantity[Sequence @@ #] & /@ Transpose[{List@@QuantityMagnitude[q], List@@QuantityUnit[q]}]

(*typesetting for units, not currently exposed*)
SetAttributes[unitFunction2String,HoldFirst];

unitFunction2String[exp_]:=
Module[{u1=Union@Cases[HoldForm[exp],_IndependentUnit,Infinity],rest,un, rules},
	rest=HoldForm[exp]/._IndependentUnit->1;
	un = Union@Cases[rest, _String, Infinity];
	un = Flatten@Prepend[un,u1];
	rules = Rule[#, QuantityLabel[#]] & /@ un; 
	HoldForm[exp] /. rules]

(*generates displaybox form for mixed-radix*)
makedisplayboxes[list_] := 
 Module[{n = Length[list], s, content}, s = Array[Slot, n]; 
  content = 
   Flatten@Riffle[
     List[s[[#]], " ", StyleBox[ToBoxes[list[[#]]], "QuantityUnitLabels"]] & /@ 
      Range[n], " "]; RowBox[content]]

(*generates interpretation boxes for mixed-radix*)
makeinterpboxes[list_] := 
 Module[{n = Length[list], s, content,m=List@@(ToString[#,InputForm]&/@list)}, s = Array[Slot, n]; 
  content = 
   RowBox[{RowBox[{"MixedRadix","[", RowBox[Riffle[s, ","]], "]"}], ",", RowBox[{"MixedRadix","[",RowBox[Riffle[m,","]],"]"}]}]]


DatedUnitBox[{n_Integer}] := MakeBoxes[n]
DatedUnitBox[l:{_?NumberQ..}] := MakeBoxes[l]
DatedUnitBox[HoldPattern[dObj_DateObject]] := With[{d=System`DateObjectDump`ValidateDateObject[dObj]},
	ToBoxes[DateString[d]]/;DateObjectQ[d]]
DatedUnitBox[other_] := ToBoxes[other]
(*TemplateBox for Quantity expressions*)
SetAttributes[QuantityUnitBox,HoldAll];
QuantityUnitBox[Quantity[],___]:=RowBox[{"Quantity","[","]"}]

QuantityUnitBox[Quantity[value_, unit:IndependentUnit[u_String],opts___?OptionQ],form_] := Module[{style, boxes},
   {style, boxes} = getStyleAndDisplayBoxes[value, unit,form];
   
   TemplateBox[{
   	makeNumberValue[value, unit], 
   	Sequence @@ boxes,
   	u,
   	MakeBoxes[unit]
   	}, style, SyntaxForm->Mod]
]
  
QuantityUnitBox[q:Quantity[val_,DatedUnit[u_,date_]],___]:=With[{boxes=RowBox[{
	QuantityUnitBox[Quantity[val,u]],
	"\[InvisibleSpace]",
	StyleBox[RowBox[{"(",StyleBox[ToBoxes[QuantityLabel[u]],ShowStringCharacters->False],DatedUnitBox[date],")"}],FontColor -> GrayLevel[0.5]]}]},
	InterpretationBox[boxes,q]]
QuantityUnitBox[Quantity[val_,DatedUnit[u_]],___]:=QuantityUnitBox[Quantity[val,u]]
QuantityUnitBox[q:Quantity[val_,u_],form___]/;Not[FreeQ[u,DatedUnit]]/;$HoldFlag=!="Hold":=Block[{$HoldFlag="Hold"},
	With[{dus=Cases[u,_DatedUnit,-1]},
		With[{labels=RowBox[Riffle[makeDatedUnitLabel/@dus,","]],un=HoldForm[u]/.DatedUnit[unit_,_]:>unit},
			With[{boxes=RowBox[{
	QuantityUnitBox[Quantity[val,un], form],
	"\[InvisibleSpace]",
	StyleBox[RowBox[{"(",labels,")"}],FontColor -> GrayLevel[0.5]]
}]},InterpretationBox[boxes,q]]
]]]

makeDatedUnitLabel[DatedUnit[unit_,date_]]:=With[{label=StyleBox[ToBoxes[QuantityLabel[unit]],ShowStringCharacters->False]},
	RowBox[{label,DatedUnitBox[date]}]]
	
(*don't cache becaues of singular/plural clash*)
QuantityUnitBox[input:Quantity[1,unit:Except[_MixedRadix],opts___?OptionQ],form_]:=iQuantityUnitBox[Quantity[1,unit,opts],form]

QuantityUnitBox[input:Quantity[e_,unit:Except[_MixedRadix]],StandardForm]:=With[
 	{b=iQuantityUnitBox[Quantity[$UnitDisplayTemporaryVariable,unit],form]},
 	 (If[Not[UnitDisplayCacheContainsQ[unit]],UnitDisplayCacheAdd[HoldForm[unit],b]];
 	   getUnitDisplayForm[unit,e])]
 	   
QuantityUnitBox[input:Quantity[e_,unit:Except[_MixedRadix]],form_] := iQuantityUnitBox[input,form]

SetAttributes[QuantityAbbreviation, HoldAll];
Options[QuantityAbbreviation]={"Singular"->Automatic,"Format"->Automatic};
QuantityAbbreviation[e_, unit_, OptionsPattern[]] := Block[{$AlphaBlockFlag=True},
 With[{u = toAlphaUnit[unit],num=Switch[OptionValue["Singular"],
 	True,1,
 	False,2,
 	_,If[MatchQ[HoldComplete[e],HoldComplete[1]],1,2]]}, 
  Module[{pre, post, label, style,out}, 
  Block[{BoxForm`UseTextFormattingQ=False},	
  	{pre, post, label, style} = CalculateScan`UnitScanner`Private`UnitInformation[num, u, "SingularTest" -> (MatchQ[#, 1] &), 
  		"Output"->{"PrefixBoxes","PostfixBoxes", "UnitBoxes", "DefaultStyle"}]];
   pre = nullifyLabel[pre];
   post = nullifyLabel[post];
   label = nullifyLabel[label];
   If[UnsameQ[style, "Long"],
    out=makeStandardDisplay[pre, post, label];,
    With[{l = nullifyLabel[unitinfo[num, u, "Boxes"]]}, 
     out=makeStandardDisplay[pre, post, l]
     ]
   ];
   If[hasFontTroubleUnitQ[u],
   	StyleBox[out,FontFamily -> "Arial Unicode MS"],
   (out/.{"\[NegativeMediumSpace]" -> "\[InvisibleSpace]"}),
   	out]
]]]

SetAttributes[getStyleAndDisplayBoxes, HoldAll];
Options[getStyleAndDisplayBoxes] = {"Singular" -> Automatic, "Format" -> Automatic};

getStyleAndDisplayBoxes[e_, unit_, form_, OptionsPattern[]] := Block[{$AlphaBlockFlag = True,styleName, boxes}, 
  With[{
  	u = toAlphaUnit[unit], 
    num = Switch[OptionValue["Singular"], True, 1, False, 2, _, If[MatchQ[HoldComplete[e], HoldComplete[1]], 1, 2]]
    }, 
   Module[{pre, post, label, style},
    {pre, post, label, style} = CalculateScan`UnitScanner`Private`UnitInformation[num, u, 
      "SingularTest" -> (MatchQ[#, 1] &), "Output" -> {"PrefixBoxes", "PostfixBoxes", "UnitBoxes", "DefaultStyle"}];
    
    If[SameQ[style, "Long"], label = unitinfo[num, u, "Boxes"]];
    {pre, post, label} = nullifyLabel /@ {pre, post, label};
    {styleName,boxes} =Switch[{pre, post, label},
     {Null, Null, Except[Null]}, {"Quantity", {label}},
     {Null, Except[Null], Null}, {"QuantityPostfix", {post}},
     {Null, Except[Null], Except[Null]}, {"QuantityUnitPostfix", {post, label}},
     {Except[Null], Null, Null}, {"QuantityPrefix", {pre}},
     {Except[Null], Null, Except[Null]}, {"QuantityPrefixUnit", {pre, label}},
     {Except[Null], Except[Null], Null}, {"QuantityPrefixPostfix", {pre, post}},
     {Except[Null], Except[Null], Except[Null]}, {"QuantityPrefixUnitPostfix", {pre, post, label}},
     _, {"Quantity", ""}(*fallback; blank label*)
     ];
     If[SameQ[form,TraditionalForm],styleName = StringJoin[styleName,"TF"]];
     {styleName,boxes}
    ]
   ]
  ]

SetAttributes[UnitAbbreviation, HoldAll];
Options[UnitAbbreviation] = {"Singular" -> Automatic, "Format" -> Automatic};
UnitAbbreviation[unit_, OptionsPattern[]] := 
 Block[{$AlphaBlockFlag = True}, 
  With[{u = toAlphaUnit[unit], 
    num = Switch[OptionValue["Singular"], True, 1, _, 2]}, 
   Module[{pre, post, label, style, out}, {pre, post, label, style} = 
     CalculateScan`UnitScanner`Private`UnitInformation[num, u, "SingularTest" -> (MatchQ[#, 1] &), 
      "Output" -> {"PrefixBoxes", "PostfixBoxes", "UnitBoxes", "DefaultStyle"}];
    pre = nullifyLabel[pre];
    post = nullifyLabel[post];
    label = nullifyLabel[label];
    If[UnsameQ[style, "Long"], 
     out = makeUnitDisplay[pre, post, label];, 
     With[{l = nullifyLabel[unitinfo[num, u, "Boxes"]]}, 
      out = makeUnitDisplay[pre, post, l]]];
    If[hasFontTroubleUnitQ[u], 
     StyleBox[out, FontFamily -> "Arial Unicode MS"], (out/.{"\[NegativeMediumSpace]" -> "\[InvisibleSpace]"}), out]]]]


SetAttributes[hasFontTroubleUnitQ,HoldAll];
hasFontTroubleUnitQ[expr_]:=If[
	MemberQ[{"Windows", "Windows-x86-64"}, $SystemID],
	If[FreeQ[exp2list[{expr}], x_ /; MemberQ[$fonttroubleunits, x]],
		False,
		True,
		True],
	False,
	False]

nullifyLabel[l_] := 
 If[Or[And[MatchQ[l, _String], StringMatchQ[l, whitespacepattern ..]],
    MatchQ[l, RowBox[List[whitespacepattern ..]]]], Null, l]

makeStandardDisplay[pre_, post_, label_] := 
 With[{after = 
    If[SameQ[post, Null], If[SameQ[label,Null],"\[InvisibleSpace]",RowBox[{" ",label}]], 
     If[SameQ[label, Null], post, 
      RowBox[{post, " ", label}]]]}, 
  If[SameQ[pre, Null],
   StyleBox[RowBox[{#1,"\[InvisibleSpace]",after}], 
    ShowStringCharacters -> False], 
   StyleBox[
    RowBox[{pre, "\[InvisibleSpace]", #1, "\[InvisibleSpace]",
      after}], ShowStringCharacters -> False]]]
      
makeUnitDisplay[pre_, post_, label_] := 
 With[{after = 
    If[SameQ[post, Null], 
     If[SameQ[label, Null], "\[InvisibleSpace]", 
      label], 
     If[SameQ[label, Null], post, 
      RowBox[{post, " ", label}]]]}, 
  If[SameQ[pre, Null], 
   StyleBox[after, 
    ShowStringCharacters -> False], 
   StyleBox[
    RowBox[{pre, "\[InvisibleSpace]", after}], 
    ShowStringCharacters -> False]]]
    
SetAttributes[abnormalLabelQ,HoldAll];
abnormalLabelQ[unit_]:=With[{u=toAlphaUnit[unit]},UnsameQ[unitinfo[2,u,"Output"->"DefaultStyle"],Default]]   

SetAttributes[iQuantityUnitBox,HoldAll];
   
iQuantityUnitBox[Quantity[val_, unit:Except[_MixedRadix], opts___?OptionQ],form_] := With[
	{value = Which[
		SameQ[val,None],InterpretationBox["\[InvisibleSpace]",1],
		True, makeNumberValue[val, unit]]},
	Module[{style, boxes},
   {style, boxes} = getStyleAndDisplayBoxes[value, unit,form];
   
   TemplateBox[{
   	value, 
   	Sequence @@ boxes,
   	(qlsafe[unit, "Format" -> "UnitString"] /. $Failed -> ""),
   	MakeBoxes[unit],
   	Sequence@@(MakeBoxes/@{opts})
   	}, style, SyntaxForm->Mod]
]]
      
QuantityUnitBox[q:Quantity[value_, unit:Except[_MixedRadix], opts__?OptionQ],form_] :=  iQuantityUnitBox[q,form]
       
QuantityUnitBox[q:Quantity[mags:MixedRadix[0..],units_MixedRadix,opts___?OptionQ],___] /; UnsameQ[$ShowZero,True] := Block[{$ShowZero = True},
	QuantityUnitBox[q]
	]
	
QuantityUnitBox[q:Quantity[mags_MixedRadix,units_MixedRadix,opts___?OptionQ],form_] := ToMixedRadixQuantityTemplateBox[q,form]
    
QuantityUnitBox[Quantity[mag:Interval[{lower_MixedRadix,upper_MixedRadix}],unit_MixedRadix,opts___?OptionQ],___] := With[{
	box1=QuantityUnitBox[Quantity[lower,unit]],
	box2=QuantityUnitBox[Quantity[upper,unit]]},
InterpretationBox[RowBox[{
	box1,
	StyleBox["to", FontColor->GrayLevel[.4], ShowStringCharacters -> False],
	box2
	}],Quantity[mag,unit,opts]]] /; SameQ[Length[lower],Length[upper],Length[unit]]

QuantityUnitBox[Quantity[l_List,units_MixedRadix,opts___?OptionQ],___]:=With[{q=Quantity[l,units]},MakeBoxes[q]]

QuantityUnitBox[Quantity[q1_,q2___],other___]/;Head[q1]===Pattern:=MakeBoxes[Quantity[q1,q1],other]
QuantityUnitBox[other___]:=MakeBoxes[other]

MixedRadixBox[{arg_,unit_}]:=With[{box = (iMixedRadixUnitBox[unit]/. {"\[NegativeMediumSpace]" -> "\[InvisibleSpace]"})},
	Which[SameQ[box,$Failed],
		$Failed,
		
		And[SameQ[arg,"0"],UnsameQ[$ShowZero,True]],
		"",
		
		True,(*else*)
		$ShowZero = False;(*ensure that only leading zeros would be shown*)
		TemplateBox[{arg},"QuantityUnit",DisplayFunction->(RowBox[{#,"\[InvisibleSpace]",box}]&)]
	]
]

iMixedRadixUnitBox[unit_] := Module[
	{mb=unitinfo[2,toAlphaUnit[unit],"Output"->"MixedBoxes"]},
	If[mb=!=None,
		mb,
		RowBox[{"\[ThinSpace]",QuantityLabel[unit,"Format"->"UnitLabel"]}],
		$Failed]
]

getMixedRadixLabel[MixedRadix[units__]] := StringJoin[
	Riffle[
		qlsafe[#, "Format" -> "UnitString"] & /@ {units} /. $Failed -> "",
		 ","
	]
]
getMixedRadixLabel[___] := ""

getMixedRadixStyle[units_MixedRadix, form_] := If[
	1 <= Length[units] <= 6,
	StringJoin[
		"QuantityMixedRadix",
		If[SameQ[form,TraditionalForm],"TF",""], 
		ToString[Length[units]]
	],
	StringJoin[
		"QuantityMixedRadixGeneric",(*fall-back style*)
		If[SameQ[form,TraditionalForm],"TF",""]
	]
]
getMixedRadixStyle[___] := "QuantityMixedRadixGeneric"

getMixedRadixMagnitudeAndUnitBoxes[mag_MixedRadix,units_MixedRadix, form_] := With[{
	mags = makeNumberValue[#, units] & /@ List @@ mag, 
	unitLabels = iMixedRadixUnitBox[#] & /@ List @@ units /. {"\[NegativeMediumSpace]" -> "\[InvisibleSpace]"}
},
  If[1 <= Length[units] <= 6, 
	Join[mags, unitLabels], 
	{RowBox[RowBox /@ Transpose[{
		mags, 
		ConstantArray["\[InvisibleSpace]", Length[units]], 
		unitLabels}]
	]}
  ]
]

getMixedRadixInterpBoxes[q : Quantity[_, units_MixedRadix,opts___]] := If[TrueQ[1 <= Length[units] <= 6],
  Sequence@@(MakeBoxes/@{units,opts}),
  Block[{Quantity},MakeBoxes[q]]
  ]

ToMixedRadixQuantityTemplateBox[q : Quantity[mag_MixedRadix, units_MixedRadix], form_] := With[{
	style = getMixedRadixStyle[units, form], 
	boxes = getMixedRadixMagnitudeAndUnitBoxes[mag, units, form], 
	interp = {getMixedRadixInterpBoxes[q]}, 
	tip = {getMixedRadixLabel[units]}
},
	TemplateBox[Join[boxes, tip,interp], style]
]
  
ToMixedRadixQuantityTemplateBox[other___] := Block[{Quantity},MakeBoxes[other]]