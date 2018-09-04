(*System`Quantity;*)
System`Entity;
System`EntityValue;
System`EntityProperty;
System`EntityProperties;
System`CommonName;
System`CanonicalName;
System`EntityTypeName;
System`EntityClass;
System`EntityPropertyClass;
System`EntityList;
System`EntityClassList;
System`RandomEntity;
System`EntityInstance;
System`EntityCopies;
System`EntityGroup;
Experimental`FindEntities;
Internal`DisableEntityFramework;
Internal`AddToEntityNameCache;
Internal`PreloadEntityNameCache;
Internal`BulkFetchEntityNames;

EntityFramework`MakeEntityFrameworkBoxes;

Begin["EntityFramework`Private`"];

(*symbols to be protected/read protected (except option names FormatName/Qualifiers/SourceEntityType)*)
$readProtectedSymbols={Entity, EntityValue, EntityClass, EntityProperty, EntityPropertyClass, 
	Experimental`FindEntities, CanonicalName, CommonName, EntityList, EntityClassList, RandomEntity,
	EntityProperties, EntityTypeName, EntityInstance, EntityCopies, EntityGroup};
	
Unprotect@@$readProtectedSymbols;

$tag = "EntityFrameworkCatchThrowTag";
$ARGEntityTypes = {};
(*** Formatting code ***)
Needs["WolframAlphaClient`"];(*initialize WAClient*)
(* this should only have to live in TypesetInit.m, but for some reason gets clobbered when this is loaded... *)
Unprotect[System`Entity, System`EntityProperty, EntityFramework`MakeEntityFrameworkBoxes];

Options[EntityPropertyClass] = Options[EntityProperty] = {UnitSystem :> $UnitSystem}

SetAttributes[EntityFramework`MakeEntityFrameworkBoxes, HoldAllComplete];
Unprotect[Entity];
Entity /: MakeBoxes[x_Entity, fmt_] := (Entity;(*trigger auto-loader*)
  With[{boxes = EntityFramework`MakeEntityFrameworkBoxes[x, fmt]}, 
   boxes /; boxes =!= $Failed])

EntityProperty /: MakeBoxes[x_EntityProperty, fmt_] := (Entity;(*trigger auto-loader*)
  With[{boxes = EntityFramework`MakeEntityFrameworkBoxes[x, fmt]}, boxes /; boxes =!= $Failed])

EntityValue /: MakeBoxes[x_EntityValue, fmt_] := (Entity;(*trigger auto-loader*)
  With[{boxes = EntityFramework`MakeEntityFrameworkBoxes[x, fmt]}, boxes /; boxes =!= $Failed])
  
EntityClass /: MakeBoxes[x_EntityClass, fmt_] := (Entity;(*trigger auto-loader*)
  With[{boxes = EntityFramework`MakeEntityFrameworkBoxes[x, fmt]}, boxes /; boxes =!= $Failed])

EntityPropertyClass /: MakeBoxes[x_EntityPropertyClass, fmt_] := (Entity;(*trigger auto-loader*)
  With[{boxes = EntityFramework`MakeEntityFrameworkBoxes[x, fmt]}, boxes /; boxes =!= $Failed])

(* MakeExpression rules now live in TypesetInit.m *)

$entitynametimeout = 0.8; (* Seconds timeout for single entity formatting *)

getetypeLabel[etype_] := If[MatchQ[entityLabelRules, {_Rule..}],
	etype /. entityLabelRules,
	etype
]

geteclasstypeLabel[etype_] := If[MatchQ[entityClassLabelRules, {_Rule..}],
	etype /. entityClassLabelRules,
	etype
]

makeTooltip[e_] := With[{str=ToString[e, InputForm]}, 
   MakeBoxes[str, StandardForm]
]

MakeTypesetBoxes = Function[Null, Block[{BoxForm`UseTextFormattingQ = False}, MakeBoxes[##]], HoldAllComplete];

SetAttributes[{
	makeGeneralizedEntityBaseBoxes, 
	makeGeneralizedEntityClassBaseBoxes, 
	makeGeneralizedEntityInfoBoxes, 
	genericEntityPrimaryLabel
},	HoldAllComplete];

genericEntityPrimaryLabel[etype_String, fmt_] := With[{label = geteclasstypeLabel[etype]},
	MakeBoxes[label,fmt]
]

makeGeneralizedEntityBaseBoxes[_[etype_, ___], fmt_] :=
	TemplateBox[{genericEntityPrimaryLabel[etype, fmt]}, "GeneralizedEntityBase"]

$separator = StyleBox["\":\"", "GeneralizedEntityInfoItem"];

SetAttributes[makeGeneralizedEntityInfoRow, HoldAllComplete]
makeGeneralizedEntityInfoRow[label_ -> value_, fmt_] := TemplateBox[
	{MakeBoxes[label, fmt], $separator , MakeBoxes[value,fmt]}, 
	"GeneralizedEntityInfoRow"
]
makeGeneralizedEntityInfoRow[label_ :> value_, fmt_] := TemplateBox[
	{MakeBoxes[label, fmt], $separator, MakeBoxes[value,fmt]}, 
	"GeneralizedEntityInfoRow"
]

makeGeneralizedEntityInfoRow[x_, fmt_] := TemplateBox[{MakeBoxes[x, fmt]}, "GeneralizedEntityInfo"]
	
makeGeneralizedEntityClassBaseBoxes[_[etype_, ___], fmt_] :=
	TemplateBox[{genericEntityPrimaryLabel[etype, fmt]}, "GeneralizedEntityClassBase"]
		
makeGeneralizedEntityInfoBoxes[_[etype_, cond_], fmt_] := Block[{BoxForm`UseTextFormattingQ = False},
	Module[{boxes},
		boxes = Map[
			Function[x, makeGeneralizedEntityInfoRow[x, fmt], HoldAllComplete],
			Unevaluated[cond]
		];
		ToBoxes[Column[RawBoxes /@ boxes, DefaultBaseStyle -> "GeneralizedEntityInfoGrid"], fmt]
	]
]

EntityFramework`MakeEntityFrameworkBoxes[__] = $Failed;

(* Entity[] formatting *)

EntityFramework`MakeEntityFrameworkBoxes[e:Entity[etype_String, ename:(_String|_List|_Integer)], StandardForm]/;!TrueQ[$dontFormatEntity] := 
  With[{fname = GetEntityName[e, $entitynametimeout]},
    With[{tooltip = makeTooltip[e],
      label = ToBoxes[getetypeLabel[etype], StandardForm],
      boxes = Block[{$dontFormatEntity=True}, 
        With[{strip=e}, MakeBoxes[strip, StandardForm]]]
      },
      TemplateBox[{MakeTypesetBoxes[fname,StandardForm], boxes, tooltip, label}, "Entity"
      ]
    ]/; OKEntityNameQ[fname]
  ]

EntityFramework`MakeEntityFrameworkBoxes[e:Entity[etype_String, ename:(_String|_List|_Integer)], TraditionalForm] := 
  With[{fname = GetEntityName[e, $entitynametimeout]},
    With[{boxes = MakeTypesetBoxes[fname, TraditionalForm]},
      InterpretationBox[boxes, e, BaseStyle -> "Entity"]
    ]/;OKEntityNameQ[fname]
  ]

(* Generalized Entity[] formatting *)
GEFormatQ[] := And[Not[TrueQ[$dontFormatEntity]], BoxForm`sufficientVersionQ[10.1, 1]]

EntityFramework`MakeEntityFrameworkBoxes[e:(Entity|EntityClass)[etype_String, cond:{(Rule | RuleDelayed)[_EntityProperty | _String, _] ..}], fmt_] /;GEFormatQ[] :=
  Module[{base, rest, tooltip},
		base = Switch[Head[e],
			Entity,makeGeneralizedEntityBaseBoxes[e, fmt],
			EntityClass,makeGeneralizedEntityClassBaseBoxes[e, fmt]
		];
		rest = makeGeneralizedEntityInfoBoxes[e, fmt];
		tooltip = makeTooltip[e];
		With[{base = base, rest = rest, tooltip = tooltip},
			InterpretationBox[
				DynamicModuleBox[{open = False},
					TemplateBox[{base, rest, Dynamic[open], tooltip},"GeneralizedEntityToggle"]],
				e, SelectWithContents -> True]
		]
	]

EntityFramework`MakeEntityFrameworkBoxes[Entity[etype_String, cond:(Rule | RuleDelayed)[_EntityProperty | _String, _]], fmt_] :=
 EntityFramework`MakeEntityFrameworkBoxes[Entity[etype,{cond}], fmt]
 
EntityFramework`MakeEntityFrameworkBoxes[EntityClass[etype_String, cond:(Rule | RuleDelayed)[_EntityProperty | _String, _]], fmt_] :=
 EntityFramework`MakeEntityFrameworkBoxes[EntityClass[etype,{cond}], fmt]


(* EntityProperty[] formatting *)

EntityFramework`MakeEntityFrameworkBoxes[e:EntityProperty[etype_String, pname:(_String|_List), opts:OptionsPattern[]], StandardForm]/;!TrueQ[$dontFormatEntity] := 
  With[{fname = GetEntityName[e, $entitynametimeout]},
    With[{tooltip = makeTooltip[e],
      boxes = Block[{$dontFormatEntity=True}, MakeBoxes[e, StandardForm]]},
      TemplateBox[{MakeTypesetBoxes[fname,StandardForm], boxes, tooltip}, "EntityProperty"]
    ]/; OKEntityNameQ[fname]
  ]
  
EntityFramework`MakeEntityFrameworkBoxes[e:EntityProperty[etype_String,pname:(_String|_List), opts:OptionsPattern[]], TraditionalForm] := 
  With[{fname = GetEntityName[e, $entitynametimeout]},
    With[{boxes = MakeTypesetBoxes[fname, TraditionalForm]},
      InterpretationBox[boxes, e, BaseStyle -> "EntityProperty"]
    ]/;OKEntityNameQ[fname]
  ]

(* EntityValue[] formatting *)

EntityFramework`MakeEntityFrameworkBoxes[EntityValue[e:(_Entity | _EntityClass), prop_EntityProperty], fmt:StandardForm] := 
Module[{eboxes = MakeBoxes[e, fmt], propboxes = MakeBoxes[prop, fmt]},
	If[ TrueQ[BoxForm`$UseTextFormattingForEntityValue] || Head[eboxes] =!= TemplateBox || Head[propboxes] =!= TemplateBox,
		RowBox[{"EntityValue", "[", RowBox[{eboxes, ",", propboxes}], "]"}],
		TemplateBox[{eboxes, propboxes}, "EntityValue"]
	]
]

(* EntityClass[] formatting *)

EntityFramework`MakeEntityFrameworkBoxes[e:EntityClass[etype_String, ename:(_String|_List|All), opts___], StandardForm]/;!TrueQ[$dontFormatEntity] := 
  With[{fname = GetEntityName[e, $entitynametimeout]},
    With[{tooltip = makeTooltip[e],
      label = ToBoxes[geteclasstypeLabel[etype], StandardForm],
      boxes = Block[{$dontFormatEntity=True}, 
        MakeBoxes[e, StandardForm]]
      },
      TemplateBox[{MakeTypesetBoxes[fname,StandardForm], boxes, tooltip, label}, "EntityClass"]
    ]/; OKEntityNameQ[fname]
  ]

EntityFramework`MakeEntityFrameworkBoxes[e:EntityClass[etype_String, ename:(_String|_List|All), opts___], TraditionalForm] := 
  With[{fname = GetEntityName[e, $entitynametimeout]},
    With[{boxes = MakeTypesetBoxes[fname, TraditionalForm]},
      InterpretationBox[boxes, e, BaseStyle -> "EntityClass"]
    ]/;OKEntityNameQ[fname]
  ]

(* EntityPropertyClass[] formatting *)

EntityFramework`MakeEntityFrameworkBoxes[e:EntityPropertyClass[ptype_String, pname:(_String|_List), opts:OptionsPattern[]], StandardForm]/;!TrueQ[$dontFormatEntity] := 
  With[{fname = GetEntityName[e, $entitynametimeout]},
    With[{tooltip = makeTooltip[e],
      boxes = Block[{$dontFormatEntity=True}, MakeBoxes[e, StandardForm]]},
      TemplateBox[{MakeTypesetBoxes[fname,StandardForm], boxes, tooltip}, "EntityPropertyClass"]
    ]/; OKEntityNameQ[fname]
  ]
  
EntityFramework`MakeEntityFrameworkBoxes[e:EntityPropertyClass[ptype_String, pname:(_String|_List), opts:OptionsPattern[]], TraditionalForm] := 
  With[{fname = GetEntityName[e, $entitynametimeout]},
    With[{boxes = MakeTypesetBoxes[fname, TraditionalForm]},
      InterpretationBox[boxes, e, BaseStyle -> "EntityPropertyClass"]
    ]/;OKEntityNameQ[fname]
  ]

(*EntityFramework`MakeEntityFrameworkBoxes[ei:EntityFramework`EntityInstance[e_Entity, instance_], form_]  /; !TrueQ[$dontFormatEntity] := 
  With[{fname = MakeBoxes[e,form], instanceboxes = makeInstanceBoxes[instance]},
    With[{boxes = composeEntityInstanceBoxes[fname, instanceboxes]},
      InterpretationBox[boxes,ei]
    ]
  ]*)

makeInstanceBoxes[expr_List] := instanceFrame[RowBox[ToBoxes/@Riffle[expr,"|"]]]
makeInstanceBoxes[expr_] := instanceFrame[ToBoxes[expr]]

instanceFrame[boxes_] := FrameBox[
	boxes,
	FrameStyle -> $instanceFrameColor,
	Background -> $instanceBackgroundColor,
	RoundingRadius -> 3,
	FrameMargins -> {{2, 2}, {0, 0}},
	BoxFrame -> .1,
	BaselinePosition -> Baseline
]

$instanceFrameColor = RGBColor[0.847059, 0.780392, 0.713725];
$instanceBackgroundColor = RGBColor[0.956863, 0.945098, 0.913725];
$frameColor = RGBColor[1., 0.737255, 0.470588];  
$backgroundColor = RGBColor[1., 0.988235, 0.956863];

composeEntityInstanceBoxes[entityboxes_, instanceboxes_] := RowBox[{
	"EntityInstance","[",
	FrameBox[
		StyleBox[
		RowBox[{entityboxes,"\[InvisibleSpace],",instanceboxes}],
		FontFamily->"SansSerif"],
		FrameStyle-> $frameColor,
		Background -> $backgroundColor,
		RoundingRadius -> 3,
		FrameMargins -> {{2, 2}, {1, 1}},
		BoxFrame -> .1,
		BaselinePosition -> Baseline
	],"]"
}]
(*** Entity Name Cache ***)
(*** Very basic down values based, partly because the plan is to cache it on disk as well ***)
$SystemArchitecture := Switch[$ProcessorType,
	"x86-64", "64Bit",
	"x86", "32Bit",
	_, "32Bit"];
$EntityNamesMXFile = FileNameJoin[{DirectoryName[DirectoryName[$InputFileName]],"Resources",$SystemArchitecture,"EntityNames.mx"}];
$EntityNameCacheDirectory = FileNameJoin[{DirectoryName[DirectoryName[$InputFileName]],"Resources",$SystemArchitecture}]
$MaxEntityCacheSizeTotal = 4*10^6;

$specialCacheEntityTypes = {"City", "Chemical", "WeatherStation"};
$ARGCaches := Complement[$ARGEntityTypes,$specialCacheEntityTypes]

initSpecialCacheCases[] :=Module[{},
	Set[specialCacheNotLoadedQ[#],True]&/@$specialCacheEntityTypes;
	Set[specialCacheNotLoadedQ[#],True]&/@$ARGCaches
];



markCacheLoaded["City"] := Set[specialCacheNotLoadedQ["City"],False]
markCacheLoaded["Chemical"] := Set[specialCacheNotLoadedQ["Chemical"],False]
markCacheLoaded["WeatherStation"] := Set[specialCacheNotLoadedQ["WeatherStation"],False]
(*catch-all for ARG-related entities; all names are in the same cache*)
markCacheLoaded[type_String]/; MemberQ[$ARGCaches, type] := Map[
	Set[specialCacheNotLoadedQ[#],False]&,
	$ARGCaches
]

EntityNameCacheFileLookup["City"] = FileNameJoin[{$EntityNameCacheDirectory,"CityEntityNames.mx"}]
EntityNameCacheFileLookup["Chemical"] = FileNameJoin[{$EntityNameCacheDirectory,"ChemicalEntityNames.mx"}]
EntityNameCacheFileLookup["WeatherStation"] = FileNameJoin[{$EntityNameCacheDirectory,"WeatherStationEntityNames.mx"}]
EntityNameCacheFileLookup[_] := FileNameJoin[{$EntityNameCacheDirectory,"ARGEntityNames.mx"}]

loadMXFile[file_String] := Module[{},
	If[FileExistsQ[file],
		Get[file]
	]
];

initEntityNameCache[] := Module[{},
  Clear[rawENC,rawENClass,rawEPC,rawEPClass,rawEPQ];
  rawENC[__] = None;
  rawENClass[__] = None;
  rawEPC[__] = None;
  rawEPClass[__] = None;
  rawEPQ[__] = None;
  loadMXFile[$EntityNamesMXFile];(*initialize with shipping EntityNames if available*)
  $rawENCSize = 0;(*build-in entities don't count against cache size*)
  specialCacheNotLoadedQ[__]=False;(*reset loading flags for special data-paclet caches*)
  initSpecialCacheCases[];
  EPDC[__] = False;
  EPBS[__] = Automatic;
  LENC[__][__] = None;
];
initEntityNameCache[];

$EntityFormatSpecialCaseTypes = {"Earthquake", "PartOfSpeech"};

EntityNameCacheFetch[Entity[type_, name_]]/;specialCacheNotLoadedQ[type] := Module[{},
	markCacheLoaded[type];
	loadMXFile[EntityNameCacheFileLookup[type]];
	EntityNameCacheFetch[Entity[type,name]]
]
EntityNameCacheFetch[Entity[type_, name_]] /; MemberQ[$EntityFormatSpecialCaseTypes, type] := With[{res = specialCaseFormatting[type,name]}, res /; res =!= $Failed]
EntityNameCacheFetch[Entity[type_, name_]] := rawENC[{type, name}]  (* add file loading later *)
EntityNameCacheFetch[EntityClass[type_, name_]] := rawENClass[{type, name}]
EntityNameCacheFetch[EntityProperty[type_, name_]] := rawEPC[{type, name}]
EntityNameCacheFetch[EntityPropertyClass[type_, name_]] := rawEPClass[{type,name}]
EntityNameCacheFetch[EntityPropertyQualifier[type_, prop_,name_]] := rawEPQ[{type,prop,name}]
EntityNameCacheFetch[EntityPropertyQualifier[type_, prop_, qualname_, name_]] := With[{first=rawEPQ[{type,prop,qualname,name}]},
	If[UnsameQ[first,None],first,EntityNameCacheFetch[EntityPropertyQualifier[type, prop,name]]]](*TODO: remove once this is finalized*)
EntityNameCacheFetch[Entity[type_, name_, lang_String]] := LENC[lang][{type,name}]

cachableNameQ[_?OKEntityNameQ] :=True
cachableNameQ[_Missing] := True
cachableNameQ[_] := False

EntityNameCacheAdd[h_[type_, name_,extra___], fname_?cachableNameQ] := Catch[With[
	{container=Switch[h,
		Entity,rawENC,
		EntityClass,rawENClass,
		EntityProperty,rawEPC,
		EntityPropertyClass,rawEPClass,
		EntityPropertyQualifier, rawEPQ,
		_,Throw[$Failed,$tag]]},
  If[$rawENCSize > $MaxEntityCacheSizeTotal, initEntityNameCache[]];
  If[!OKEntityNameQ[container[{type, name, extra}]], $rawENCSize++];
  container[{type, name, extra}] = fname
],$tag]

EntityPropertyDataCachingAdd[prop:{_String,_String},True] := Set[EPDC[prop],True]
EntityPropertyBatchSizeAdd[prop:{_,_String}, n_Integer] := Set[EPBS[prop], n]
LocaleEntityNameCacheAdd[{type_String, entity_, lang_String}, value_] := Set[LENC[lang][{type,entity}], value]

EntityNameCacheClear[] := initEntityNameCache[]

EntityNameCacheSize[] := $rawENCSize;

$knownPoSNames = {"Noun", "Verb", "Adjective", "Adverb", "Preposition", "Conjunction", "Pronoun", "Determiner", "Interjection"};

specialCaseFormatting["Earthquake", name_] := name
specialCaseFormatting["PartOfSpeech", name_] := If[MemberQ[$knownPoSNames, name],
	System`ToLowerCase[name],
	$Failed
]
specialCaseFormatting[other_, name_] := $Failed

Internal`AddToEntityNameCache[x_List] := EntityNameCacheAdd[Apply[Entity,#,{1}]]&/@x
Internal`AddToEntityNameCache[entity_List,name_] := EntityNameCacheAdd[Entity@@entity,name]
Internal`AddToEntityNameCache[x__] := EntityNameCacheAdd[x];  (* externally exposed function *)

Internal`PreloadEntityNameCache[expr_] := Module[{res = {}, entities=Cases[Hold[expr], Entity[type_,name_]/;rawENC[{type,name}]===None, Infinity]},
	warmUpEntityValue[];
	If[entities=!={}, res = Internal`MWACompute["MWAEntityNames",{DeleteDuplicates[entities]}]];
	If[MatchQ[res, (HoldComplete|Hold)[{__Rule}]],
		EntityNameCacheAdd[Entity@@#[[1]], #[[2]]] &/@ ReleaseHold[res];
		Length[res]
		,
		0
	]
]

(* used by Dataset component for faster formatting -- taliesinb *)
Internal`BulkFetchEntityNames[list_] := Module[
	{res = {}, entities= DeleteCases[list, Entity[type_,name_]/;rawENC[{type,name}] =!= None]},
	warmUpEntityValue[];
	If[entities=!={}, 
		 entities = DeleteDuplicates[entities];
		 If[Length[entities] > 32,
		 	GeneralUtilities`TemporaryInformation["Retrieving Entity names"]; (* if GU isn't loaded, well, nothing happens *)
		 ];
		res = Internal`MWACompute["MWAEntityNames",{entities}];
	];
	If[MatchQ[res, (HoldComplete|Hold)[{__Rule}]],
		EntityNameCacheAdd[Entity@@#[[1]], #[[2]]] &/@ ReleaseHold[res];
	];		
	Replace[rawENC[{#[[1]],#[[2]]}]& /@ list, None -> $Failed, {1}]
]

(*** End Entity Name Cache code ***)
OKEntityNameQ[_String] :=True
OKEntityNameQ[Null] :=False
OKEntityNameQ[$Failed] :=False
OKEntityNameQ[_$Failed] :=False
OKEntityNameQ[_rawENC] :=False
OKEntityNameQ[_rawENClass] :=False
OKEntityNameQ[_rawEPC] :=False
OKEntityNameQ[_rawEPClass] :=False
OKEntityNameQ[_rawEPQ] :=False
OKEntityNameQ[_Symbol] :=False
OKEntityNameQ[_Missing] :=False
OKEntityNameQ[_Rule] :=False
OKEntityNameQ[_List] :=False
OKEntityNameQ[other_] :=With[{n=ToString[Head[other]]},Catch[(*TODO: put something more robust here*)
	MemberQ[$ContextPath,Quiet[Check[Context[n],Throw[False,OKEntityNameQ]]]],
	OKEntityNameQ
]]
(*TODO:Migrate to MWANames once it's working*)
getEntityNameServer[Entity[type_String,name_]] :=Module[{apires},
  warmUpEntityValue[];
  apires = Internal`MWACompute["MWANames",{Entity[type,name]}];
  apires = ReleaseHold[apires];
  If[!MatchQ[apires,{___,"EntityNameRules" -> {_Rule},___}], $Failed, 
  	{type,name}/.("EntityNameRules"/.apires)]
]

getEntityNameServer[EntityClass[type_String,name_]] :=Module[{apires},
  warmUpEntityValue[];
  apires = Internal`MWACompute["MWANames",{EntityClass[type,name]}];
  apires = ReleaseHold[apires];
  If[!MatchQ[apires,{___,"EntityClassNameRules" -> {_Rule},___}], $Failed, 
  	{type,name}/.("EntityClassNameRules"/.apires)]
]

getEntityNameServer[EntityProperty[type_String,name_]] :=Module[{apires},
  warmUpEntityValue[];
  apires = Internal`MWACompute["MWANames",{EntityProperty[type,name]}];
  apires = ReleaseHold[apires];
  If[!MatchQ[apires,{___,"PropertyNameRules" -> {_Rule},___}], $Failed, 
  	{type,name}/.("PropertyNameRules"/.apires)]
]

getEntityNameServer[EntityProperty[type_String,name_,qualifiers_List]] :=Module[{apires,qual},
  warmUpEntityValue[];
  apires = Internal`MWACompute["MWANames",{EntityProperty[type,name,qualifiers]}];
  apires = ReleaseHold[apires];
  If[!MatchQ[apires,{___,"PropertyNameRules" -> {_Rule},___}], $Failed, 
  	qual=replaceWithDefault["QualifierValueNameRules",apires,{}];
  	qual=({type,name,Sequence@@#}&/@qualifiers)/.qual;
  	qual = (qual /. {type,name,_,x_} :> {type,name,x})/.replaceWithDefault["QualifierValueNameRules",apires,{}];
  	qual = qual /. {type,name,x_} :> x;
  	qual = (qual /. Interval[i_List]:>Row[i,"\"to\""]) /. date_DateObject :> DateString[date];
  	{
  		{type,name}/.replaceWithDefault["PropertyNameRules",apires,{}],
  		qual
  		}]
]

getEntityNameServer[EntityPropertyClass[type_String,name_]] :=Module[{apires},
  warmUpEntityValue[];
  apires = Internal`MWACompute["MWANames",{EntityPropertyClass[type,name]}];
  apires = ReleaseHold[apires];
  If[!MatchQ[apires,{___,"PropertyClassNameRules" -> {_Rule},___}], $Failed, 
  	{type,name}/.("PropertyClassNameRules"/.apires)]
]

getEntityNameServer[other___] := $Failed

makeEPQualifierName[None,___] := None
makeEPQualifierName[propName_, qualNames_List] := Row[
	{propName, "\[ThinSpace]|\[ThinSpace]", Row[
	DeleteCases[qualNames,None],
	";"]}
]


(*special case for EntityProperty with Qualifiers*)
GetEntityName[EntityProperty[etype_,ename_,qualifiers:{_Rule..}], timeout_] :=
Module[{propName = None, qualNames = None},
	propName = EntityNameCacheFetch[EntityProperty[etype, ename]];
	qualNames = Map[EntityNameCacheFetch[EntityPropertyQualifier[etype,ename,Sequence@@#]]&,qualifiers];
	If[Not[FreeQ[{propName,qualNames},None]] && timeout >0 ,
		If[Length[#]===2,{propName,qualNames}=#]&[TimeConstrained[getEntityNameServer[EntityProperty[etype, ename,qualifiers]], timeout, $Failed]];
		EntityNameCacheAdd[EntityProperty[etype, ename],propName];
		Thread[EntityNameCacheAdd[Map[EntityPropertyQualifier[etype, ename, Sequence@@#] &, qualifiers], qualNames]]
	];
	makeEPQualifierName[propName,qualNames]
]

GetEntityName[_[_, {}, ___], _] := None (* fixing odd display of Entity|EntityClass[type, {}] *)

GetEntityName[h_[etype_, ename_, opts___], timeout_] :=
Module[{res = None},
  res = EntityNameCacheFetch[h[etype, ename]];
  If[res === None && timeout > 0,
    res = TimeConstrained[getEntityNameServer@@List[h[etype, ename]], timeout, $Failed];
    EntityNameCacheAdd[h[etype, ename], res]  (* only fires if res is _String *)
  ];
  res
]


(* takes list of message data from MWACompute and issues the messages *)
issueMWAMessages[msg_] :=Cases[
  msg/.{CalculateUtilities`ConvertMWAUtilities`Private`EntityProperty->EntityProperty}, (* temporary context confusion *)
  {{s_Symbol,tag_String},args___}:>Message[MessageName[s,tag],args],Infinity]


Unprotect[EntityProperty,EntityPropertyClass];

(**  Core EntityValue  code **)

ConvertTemporaryMWASymbols[x_, newcontext_String:"Global`"] := Module[{names, new, warning},
	names = Names["Internal`MWASymbols`Temporary`*"];
	If[names==={}, Return[x]];
	new = StringReplace[names,__~~"`"->newcontext];
	warning = Quiet[Select[new, Names[#] =!= {} && Check[ToExpression[#, InputForm, OwnValues], {}] =!= {}&]];
	If[warning =!= {}, Message[EntityValue::val, ToExpression[#,InputForm,HoldForm]&/@warning]];
	x/.((ToExpression[#,InputForm,HoldPattern]:>With[{tmp=Symbol[StringReplace[#,__~~"`"->newcontext]]}, tmp/;True])&/@names)

]

Internal`CacheEntityNames[res_] := Module[{rules,apires=ReleaseHold[res]},
	If[And[!MatchQ[apires,{_Rule..}],MatchQ[apires,{_,_Rule..}]], apires = Rest[apires]];
	If[MatchQ[apires,{_Rule..}],
		rules = replaceWithDefault["EntityNameRules",apires, {}];
	    EntityNameCacheAdd[Entity@@#[[1]], #[[2]]] &/@ rules;
	    rules = replaceWithDefault["EntityClassNameRules",apires, {}];
	    EntityNameCacheAdd[EntityClass@@#[[1]], #[[2]]] &/@ rules;
	    rules = replaceWithDefault["PropertyNameRules",apires, {}];
	    EntityNameCacheAdd[EntityProperty@@#[[1]], #[[2]]] &/@ rules;
	    rules = replaceWithDefault["PropertyClassNameRules",apires, {}];
	    EntityNameCacheAdd[EntityPropertyClass@@#[[1]], #[[2]]] &/@ rules;
	    rules = replaceWithDefault["QualifierValueNameRules", apires, {}];
	    EntityNameCacheAdd[EntityPropertyQualifier@@#[[1]], #[[2]]] &/@ rules;
	    rules = replaceWithDefault["CachingRules", apires, {}];
	    EntityPropertyDataCachingAdd@@#& /@ rules;
	    rules = replaceWithDefault["BatchSizes", apires, {}];
	    EntityPropertyBatchSizeAdd@@#& /@ rules;
	    rules = replaceWithDefault["AdditionalLocaleEntityNameRules", apires,{}];
	    LocaleEntityNameCacheAdd@@#& /@ rules;
	]
]

(* possibly add security features here *)
ReleaseMWAComputeHold[args___] := ReleaseHold[
	ReplaceAll[args, 
		{Internal`MWASymbols`MWAHold[e_] :> With[{res=e}, res/;True], remote_Internal`MWASymbols`MWARemoteObject:>fetchRemoteObject[remote]}
		]
]
SetAttributes[Internal`MWASymbols`MWAData, HoldAll];(*see 268678*)
SetAttributes[Internal`MWASymbols`MWARemoteObject, HoldAll];

fetchRemoteObject[Internal`MWASymbols`MWARemoteObject[url_String,  "ImportAction" -> fun_]] := Module[{res},
	res = Monitor[
	Quiet[Check[fun[url], $Failed]], 
	Internal`LoadingPanel["Fetching remote content ...."]
	];
	If[res === $Failed, 
		Message[EntityValue::conopen, EntityValue]; Missing["RetrievalFailure"], 
		res
	]
]
fetchRemoteObject[other__] := Missing["RetrievalFailure"]


replaceWithDefault[expr_,rules_,default_,level_:{0}] :=Replace[expr,Flatten[{rules,_->default}],level]
Unprotect[EntityValue]; (*deal with weird edge-case; sometimes get re-protected during loading*)
$GetEntitiesInBatches = True;(*on by default*)
$CacheEntityMembers = True; (*on by default*)
$CacheEntityData = False; (*off by default*)
$GetEntityDataInBatches = True;(*on by default*)
$GetEntityClassDataInBatches = True;(*on by default*)

$SpecialEVStrings = {
	"Entities","EntityClasses","EntityCount","Properties", "LastUpdate",
	"EntityCanonicalNames", "EntityClassCanonicalNames","RandomEntities",
	"PropertyCanonicalNames","SampleEntities","SampleEntityClasses",
	"EntityClassCount","PropertyClasses","PropertyClassCanonicalNames",
	"RandomEntityClasses", "RandomEntity", "RandomEntityClass", "PropertyCount", "PropertyClassCount",
	"EntityPropertyAssociation", "Dataset", "PropertyAssociation", "EntityAssociation"};
$SpecialEVRanges = {
	{"RandomEntities", _} , {"Entities", _} , {"EntityCanonicalNames", _} , 
	{"EntityClasses", _} , {"RandomEntityClasses", _} , {"Properties", _}, 
	{"PropertyClasses", _}, {"EntityClassCanonicalNames", _}, 
	{"PropertyCanonicalNames", _}, {"PropertyClassCanonicalNames", _}};
$SpecialEVPatterns = Alternatives@@Join[$SpecialEVStrings,$SpecialEVRanges];
$BatchedQualifiers = {"EntityAssociation", "PropertyAssociation", "EntityPropertyAssociation",
	"PropertyEntityAssociation", "Dataset", "Date", "Note"};
$UnbatchedQualifierPatterns = With[{alts=Alternatives@@$BatchedQualifiers},{_,Except[alts]}];

getQueryID[] :=Developer`EncodeBase64[Hash[{$MachineID,$LicenseID,AbsoluteTime[]}]](*TODO: replace with CreateUUID if doesn't req loading CloudObject*)
$EVQIDF = False;

iEVECPCount[EntityClass[type_String,_]] := With[{v=EntityValue[type, "PropertyCount"]},
	If[IntegerQ[v], Max[1, Floor[$EntityValueListThresholdValue/v]], $EntityValueListThresholdValue]
]
iEVECPCount[___] := $EntityValueListThresholdValue

EntityValue[args___]/;TrueQ[$EVQIDF] :=Block[{Internal`$QueryID=getQueryID[],$EVQIDF=False,res},
		res = iEntityValue[args];
		res /; res =!= $Failed
	]
EntityValue[args___] := With[{res=iEntityValue[args]},
	res /; res =!= $Failed
]

iEntityValue[Entity[type_,pat___,Span[start_Integer,end_Integer]],"Entities"]/;TrueQ[$CacheEntityMembers] := Block[
	{$CacheEntityMembers=False},
EntityFramework`Caching`cacheEntityMembers[{type,pat}, {start, end}]
]
iEntityValue[e:(Entity|EntityClass)[_, _, All], prop_] := Module[{},callMWACompute[e,prop]]
iEntityValue[group:(_Entity|_String|_EntityClass),"Entities"]/;And[TrueQ[$GetEntitiesInBatches],FreeQ[group,Span]] := Module[{},GetAllEntities[group,"Entities"]]
iEntityValue[group:(_Entity|_String|_EntityClass),"EntityCanonicalNames"]/;TrueQ[$GetEntitiesInBatches] := Module[{},GetAllEntities[group,"EntityCanonicalNames"]]
iEntityValue[type:(_Entity|_String), classtype:("EntityClasses"|"EntityClassCanonicalNames")]/;TrueQ[$GetEntitiesInBatches] := Module[{},GetAllEntityClasses[type, classtype]]
(*queries with a non-string aggregation function shouldn't batch*)
iEntityValue[list_List, prop_, agg:Except[_String]] := callMWACompute[list, prop, agg]
iEntityValue[input_,props_List,arg___]/;Not[TrueQ[$EPLF]] := Block[{
	$EPLF = True, 
	$EntityValueListThresholdValue = Max[1,Floor[2*$EntityValueListThresholdValue/Max[Length[props],1]]]},
	iEntityValue[input,props,arg]
]

iEntityValue[list:{(_Entity|_Missing)..},args___]/;TrueQ[Length[list]>$EntityValueListThresholdValue] := Module[{},GetEntityValueInChunks[list,args]]
iEntityValue[type:(_String|Entity[_String]),args__] /; TrueQ[$GetEntityDataInBatches] := Block[{$GetEntityDataInBatches=False},
	If[MatchQ[{args},{$SpecialEVPatterns}|$UnbatchedQualifierPatterns],
		iEntityValue[type,args],
		GetAllEntityValues[type,args]
	]
]
iEntityValue[class_EntityClass,"Dataset"] /; TrueQ[$GetEntityClassDataInBatches] := Block[
	{$GetEntityClassDataInBatches=False, $EntityValueListThresholdValue = iEVECPCount[class]},
	Dataset[GetAllEntityClassValues[class, "EntityPropertyAssociation"]]
]
iEntityValue[class_EntityClass,prop:Alternatives["PropertyAssociation","EntityPropertyAssociation", "EntityAssociation", "NonMissingPropertyAssociation"]] /; TrueQ[$GetEntityClassDataInBatches] := Block[
	{$GetEntityClassDataInBatches=False, $EntityValueListThresholdValue = iEVECPCount[class]},
	GetAllEntityClassValues[class, prop]
]
iEntityValue[class_EntityClass,args__]/;TrueQ[$GetEntityClassDataInBatches] := Block[{$GetEntityClassDataInBatches=False},
	If[MatchQ[{args},{$SpecialEVPatterns}|$UnbatchedQualifierPatterns],
		iEntityValue[class,args],
		GetAllEntityClassValues[class,args]
	]
]
iEntityValue[list:{_EntityClass..}, args___] /;And[!TrueQ[$galf], Length[list]>1] := Block[{$galf=True},With[{groups=Table[{i, i}, {i, Length[list]}]},
	EntityFramework`Dialog`interruptableDataDownloadManager[groups,list,args]]]
iEntityValue[e___] := Module[{},callMWACompute[e]]

callMWACompute[args___] := With[{e = optionsToQualifier[args]},warmUpEntityValue[];
Module[{res,apires, msg},Block[
	{WolframAlphaClient`Private`$AlphaQueryMMode=If[
		MemberQ[$EVMMODES,WolframAlphaClient`Private`$AlphaQueryMMode],
		WolframAlphaClient`Private`$AlphaQueryMMode,
		"entity"], usys = OptionValue[EntityProperty,UnitSystem]},
	apires = Internal`MWACompute[
		"MWACalculateData",
		{Internal`MWASymbols`MWAData[e], "Version" -> 0.2},
		"UnitSystem" -> usys,
		"ContextPath" -> {"Internal`MWASymbols`", "System`", "EntityFramework`"}
	];
    apires=ReleaseMWAComputeHold[apires];
    If[SameQ[apires,$Failed["ComputationTimeout"]],Message[EntityValue::timeout,EntityValue]];
    If[!OptionQ[apires], res = $Failed];
    If[res =!= $Failed, 
	    res=replaceWithDefault["Result",apires,$Failed];
	    msg = replaceWithDefault["Messages", apires, {}];
	    If[msg === {{{EntityValue, "ctimeout"}}}, 
	     apires = retryWithSmallerBatches[{e},usys];
	     If[OptionQ[apires], 
	     	res = replaceWithDefault["Result",apires,$Failed]; msg = replaceWithDefault["Messages", apires, {}], 
	        res = $Failed]];
	    If[msg =!= {}, issueMWAMessages[msg]];
		res = ConvertTemporaryMWASymbols[res]/.$Failed[_]->$Failed;
    ];
	res]
]]

splitIntoBatches[entities_List, extra___] := If[Length[entities]> 10,
	Block[{$EntityBatchThreshold=10, groups},
	groups=divideEntityCountIntoBatches[Length[entities]];
	Map[{Take[entities,#],extra}&, groups]
],
	{entities, extra}
]

splitIntoBatches[___] := Throw[$Failed,"MWAComputeRetryFlag"]

retryWithSmallerBatches[{e___},usys_] := Catch[
Module[{res, batches = splitIntoBatches[e]},
	 res = Map[ReleaseMWAComputeHold[
	 	Internal`MWACompute["MWACalculateData",Internal`MWASymbols`MWAData[Sequence@@#, "Version"->0.1], "UnitSystem"->usys]
	 ]&,
	 	batches
	 ];
	 If[TrueQ[And@@(OptionQ/@res)],
	 	mergeResults[res],
	 	$Failed
	 ]
], "MWAComputeRetryFlag"
]

mergeResults[res:{{_Rule..}..}]:= Module[{rules = Union[Flatten[res[[All,All,1]]]]},
	Map[With[{rule=#},
		rule -> Join @@ Map[replaceWithDefault[rule, #, {}]&, res]]&, rules
	]
]
mergeResults[_] := $Failed
optionsToQualifier[input___] := Sequence@@({input}/.EntityProperty[type_,prop_,opts__]:>EntityProperty[type,prop,fixQualifiers[opts]])
fixQualifiers[l_List] := l
fixQualifiers[r:Rule[UnitSystem,_]] := {r}
(*fixQualifiers[l:(_List..)] := Join[l]*)
fixQualifiers[l_List,r:Rule[UnitSystem,_]] := Append[l,r]
fixQualifiers[r:Rule[UnitSystem,_],l_List] := Prepend[l,r]
fixQualifiers[other__]:=other

$WarmUp = True;

warmUpEntityValue[] /; TrueQ[$WarmUp] := AbortProtect[Module[{cell},
  cell = If[TrueQ[$Notebooks], PrintTemporary[EntityFramework`Dialog`loadingPanel[#]]&, Print][
   "Initializing Wolfram Knowledgebase connection ...."];
  Needs["WolframAlphaClient`"];
  $UnitSystem(*calls WA via MWACompute and sets $UnitSystem*);
  Set[$WarmUp, False];
  If[UnsameQ[cell,Null],Quiet[NotebookDelete[cell]]];
  ]
]


Experimental`FindEntities[s_String,filter_:Automatic] :=Module[{res,apires, rules},
  warmUpEntityValue[];
  apires=Internal`MWACompute["MWAFindEntities",{s,filter}];
  apires=ReleaseMWAComputeHold[apires];
  If[!OptionQ[apires], Message[Internal`FindEntities::TODO]; Return[$Failed]];
  res=replaceWithDefault["Result",apires,$Failed];
  If[MatchQ["Messages"/.apires,{__}],Message[Internal`FindEntities::TODO]];   
  rules = replaceWithDefault["EntityNameRules",apires, {}];
  EntityNameCacheAdd[Entity@@#[[1]], #[[2]]] &/@ rules;
  ConvertTemporaryMWASymbols[res]
]

GetEntityNames[expr_, n_] := Catch[
 Module[{apires}, warmUpEntityValue[];
  apires = TimeConstrained[
  	Internal`MWACompute["MWANames", {expr}],
  	n,
  	Throw[Table[Missing["NotAvailable"],{i,Length[expr]}],$tag]];(*populate with missings if timed out*)
  apires = ReleaseHold[apires];
  cleanUpFormatNameResults[EntityNameCacheFetch/@expr]
],$tag]
  
cleanUpFormatNameResults[res_List] := Map[
	If[OKEntityNameQ[#],#,Missing["NotAvailable"]]&,
	res]
cleanUpFormatNameResults[other_] := other
  
bulkFetchCommonNames[entities_List, n_] := Module[{res = 
   Reap[Map[
     With[{r = EntityNameCacheFetch[#]}, 
       If[cachableNameQ[r], r, Sow[#]; sowPlaceholder[#]]] &, entities]], ask, temp}, 
    ask = Last[res];
    If[UnsameQ[ask, {}], 
    	ask = First[ask];(*remove outer list*)
    	temp = GetEntityNames[ask, n];
    	ask = Thread[sowPlaceholder /@ ask -> temp];
    	res = First[res] /. ask,(*TODO:handle errors*)
    res = First[res]];
  res]
Unprotect[Entity];
Entity/:EntityProperty[ep__][Entity[e__]] := EntityValue[Entity[e],EntityProperty[ep]]
Entity/:EntityPropertyClass[epc__][Entity[e__]] := EntityValue[Entity[e],EntityPropertyClass[epc]]
Entity/:Entity[e__][args__] := EntityValue[Entity[e],args]

EntityClass/:EntityProperty[ep__][EntityClass[ec__]] := EntityValue[EntityClass[ec],EntityProperty[ep]]
EntityClass/:EntityPropertyClass[epc__][EntityClass[ec__]] := EntityValue[EntityClass[ec],EntityPropertyClass[epc]]
EntityClass/:EntityClass[e__][args__] := EntityValue[EntityClass[e],args]

EntityProperty/:Entity[e__][EntityProperty[ep__]] := EntityValue[Entity[e],EntityProperty[ep]]
EntityProperty/:EntityClass[ec__][EntityProperty[ep__]] := EntityValue[EntityClass[ec],EntityProperty[ep]]

EntityPropertyClass/:Entity[e__][EntityPropertyClass[epc__]] := EntityValue[Entity[e],EntityPropertyClass[epc]]
EntityPropertyClass/:EntityClass[ec__][EntityPropertyClass[epc__]] := EntityValue[EntityClass[ec],EntityPropertyClass[epc]]

EntityInstance[e__][args__] := EntityValue[EntityInstance[e],args]
EntityGroup[e__][args__] := EntityValue[EntityGroup[e],args]
EntityCopies[e__][args__] := EntityValue[EntityCopies[e],args]

EntityProperties[e_] := With[{res=EntityValue[e, "Properties"]},res/;!MatchQ[res,_EntityValue]]
EntityProperties[args___] := (ArgumentCountQ[EntityProperties,Length[{args}],1,1];Null/;False)

EntityTypeName[e:(Entity|EntityClass|EntityProperty|EntityPropertyClass)[type_,___]] := type
EntityTypeName[EntityInstance[e_Entity, ___]] := EntityTypeName[e]
EntityTypeName[l_List] := Map[EntityTypeName,l]
EntityTypeName[args___] := (ArgumentCountQ[EntityTypeName,Length[{args}],1,1];Null/;False)
EntityTypeName[arg:Except[_Entity|_String|_EntityClass|_EntityProperty|_EntityPropertyClass|_List]] := (Message[EntityTypeName::noent,arg];Null/;False)

CommonName[e:(Entity|EntityClass|EntityProperty|EntityPropertyClass)[type_,name_,___]] := With[{res=GetEntityName[e, 10]},If[OKEntityNameQ[res],res,Missing["NotAvailable"]]]
CommonName[entities:{(_Entity|_EntityClass|_EntityProperty|_EntityPropertyClass|_Missing)..}] := bulkFetchCommonNames[entities,10]
CommonName[l_List] := Map[CommonName,l]
CommonName[args___] := (ArgumentCountQ[CommonName,Length[{args}],1,1];Null/;False)
CommonName[arg:Except[_Entity|_EntityProperty|_EntityClass|_EntityPropertyClass|_List]] := (Message[CommonName::noent,arg];Null/;False)

CanonicalName[e:(Entity|EntityClass|EntityProperty|EntityPropertyClass)[type_, name_, ___]] := name
CanonicalName[l_List] := Map[CanonicalName,l]
CanonicalName[args___] := (ArgumentCountQ[CanonicalName,Length[{args}],1,1];Null/;False)
CanonicalName[arg:Except[_Entity|_EntityProperty|_EntityClass|_EntityPropertyClass|_List]] := (Message[CanonicalName::noent,arg];Null/;False)

EntityList[type:(_String|_Entity|_EntityClass)] := With[{res=EntityValue[type,"Entities"]},res/;MatchQ[res,_List|_Missing]]
EntityList[args___] := (ArgumentCountQ[EntityList,Length[{args}],1,1];Null/;False)
EntityList[arg:Except[_Entity|_String|_EntityClass]] := (Message[EntityList::noent,arg];Null/;False)

EntityClassList[type:(_Entity|_String)] := With[{res=EntityValue[type,"EntityClasses"]},res/;MatchQ[res,_List|_Missing]]
EntityClassList[args___] := (ArgumentCountQ[EntityClassList,Length[{args}],1,1];Null/;False)
EntityClassList[arg:Except[_Entity|_String]] := (Message[EntityClassList::noent,arg];Null/;False)

RandomEntity[args___] := With[{res = Catch[iRandomEntity[args], $tag]},
	res /; res =!= $Failed 
]

iRandomEntity[type_] := With[{res = iRandomEntity[type,1]}, If[ListQ[res], First[res], $Failed, $Failed]]

iRandomEntity[type_, n_] := Module[{res, count, samples = validateCount[RandomEntity, type, n]},
	count = RESEC[type];
	If[count < samples, Message[RandomEntity::insfct, samples, count]; samples = count];
	res = EntityValue[validateType[RandomEntity, type],{"RandomEntities", samples}];
	If[!MatchQ[res, {_Entity..}], checkEntityValueFailReason[RandomEntity, type, res]];
	If[Length[res] === n, 
		res, 
		$Failed
	]
]

iRandomEntity[args___] := CompoundExpression[
	System`Private`Arguments[RandomEntity[args], {1, 2}];
	$Failed
]
(*RandomEntity EntityCount*)
RESEC[type_] := With[{res = EntityValue[validateType[RandomEntity, type], "EntityCount"]},
	If[!IntegerQ[res],
		checkEntityValueFailReason[RandomEntity, type, res];
		Throw[$Failed, $tag],
		res
	]
]

checkEntityValueFailReason[head_, type_, fail_] := Switch[fail,
	Missing["UnknownType"|"UnknownEntityClass"|"UnknownEntity", __], Message[head::ntype, type],
	(*TODO: add more failure validation*)
	_, Null
]

validateType[_, type:(_String|_Entity|_EntityClass)] := type
validateType[head_, other_] := CompoundExpression[ 
	Message[head::ntype, other]; 
	Throw[$Failed, $tag]
]

validateCount[_, _, i_Integer]/; i >= 0 := i
validateCount[head_, x_, other_] := CompoundExpression[ 
	Message[head::intnm, HoldForm[head[x, other]], 2];
	Throw[$Failed,$tag]
]
(*** Wrappers for *Data functions ***)
$EVDataPacletHeads = Hold[System`AdministrativeDivisionData, System`AircraftData,
System`AirportData, 
System`BridgeData, System`BroadcastStationData, System`BuildingData, System`CometData,
System`CompanyData, System`ConstellationData, System`DamData, 
System`DeepSpaceProbeData, System`EarthImpactData, 
System`ExoplanetData, System`GalaxyData,
System`GeologicalPeriodData, System`HistoricalPeriodData, 
System`IslandData, System`LanguageData, System`LakeData, 
System`LaminaData, System`MannedSpaceMissionData, 
System`MedicalTestData, System`MeteorShowerData, 
System`MineralData, System`MinorPlanetData, System`MountainData, 
System`MovieData, System`NebulaData,
System`NeighborhoodData, System`NuclearExplosionData, 
System`NuclearReactorData, System`OceanData, System`ParkData, 
System`ParticleAcceleratorData, 
System`PersonData, System`PhysicalSystemData, System`PlaneCurveData, 
System`PlanetData, System`PlanetaryMoonData, System`PlantData, 
System`PulsarData, System`SatelliteData, 
System`SolarSystemFeatureData, System`SolidData, System`SpaceCurveData, 
System`SpeciesData, System`StarData, System`StarClusterData, System`SupernovaData, 
System`SurfaceData, System`TropicalStormData, 
System`TunnelData, System`UnderseaFeatureData, 
System`UniversityData, System`VolcanoData, System`ZIPCodeData];

$SpecialEVDataPacletHeads = Hold[
	System`AnatomyData,
	System`WolframLanguageData
];

$SpecialEVDataPacletCases = {
	System`AnatomyData -> "AnatomicalStructure",
	System`WolframLanguageData -> "WolframLanguageSymbol"
}

$entityStandardNamePattern[dp_] = _String;
$entityStandardNamePattern["Acronym" | "AdministrativeDivision" | "City" | "GivenName"] = {__String};
$entityStandardNamePattern["AlgebraicCode"] = {_String, {__Integer}};
$entityStandardNamePattern["AreaCode"] = _Integer;
$entityStandardNamePattern["CrystallographicSpaceGroup"] = {_String, _Integer};
$entityStandardNamePattern["Gene"] = {_String, {"Species" -> _String}};
$entityStandardNamePattern["WolframLanguageSymbol"] = Alternatives[_String, _Symbol];

$dataHeadToEntityType = Join[(# -> StringReplace[SymbolName[#], RegularExpression["Data$"] -> ""]) & /@ 
	List @@ EntityFramework`Private`$EVDataPacletHeads, $SpecialEVDataPacletCases]

(dataHeadToEntityTypeLookup[#[[1]]] = #[[2]]) & /@ $dataHeadToEntityType;

dataHeadToEntityTypeLookup[_] = None;

Unprotect@@$EVDataPacletHeads;
Unprotect@@$SpecialEVDataPacletHeads;

Clear@@$EVDataPacletHeads;
Clear@@$SpecialEVDataPacletHeads;

(#[args___] := With[{res=EVDataPacletDispatch[#, {args}]}, res/;res=!=$Failed]) & /@ (List@@$EVDataPacletHeads);
(#[args___] := With[{res=EVDataPacletDispatch[#, {args}]}, res/;res=!=$Failed]) & /@ (List@@$SpecialEVDataPacletHeads);

With[{heads=List@@$EVDataPacletHeads}, SetAttributes[heads,ReadProtected]];
With[{heads=List@@$SpecialEVDataPacletHeads}, SetAttributes[heads,ReadProtected]];

Protect@@$EVDataPacletHeads;
Protect@@$SpecialEVDataPacletHeads;

(* TODO: add flag to indicate entity or entity class when not specified *)
Clear[EVDataPacletDispatch];
EVDataPacletDispatch[head_, args_] := Module[{etype, res},
  etype = dataHeadToEntityTypeLookup[head];
  If[etype === None, Return[$Failed] (*shouldn't happen*)]; Block[{WolframAlphaClient`Private`$AlphaQueryMMode="paclet"},
  res = Switch[args,
   {} | {All | "Entities"}, 
       EntityValue[Entity[etype], "Entities"],
   {"Classes"|"EntityClasses"}, 
       EntityValue[Entity[etype], "EntityClasses"],
   {"Properties" | "PropertyCanonicalNames" | "SampleEntities" | "SampleEntityClasses"| 
   	"EntityCanonicalNames" | "EntityCount" | "EntityClassCount" | "EntityClassCanonicalNames"|
   	"RandomEntityClasses"|"PropertyClassCanonicalNames"|"PropertyClasses"|"RandomEntities"|
   	"RandomEntity" |"RandomEntityClass" | "PropertyCount" | "PropertyClassCount" | "LastUpdate"},
       EntityValue[etype, args[[1]]],
   {Alternatives@@$SpecialEVRanges},
   	   EntityValue[etype, args[[1]]],
   {_, _, _, __},(*too many args*)
   	   ArgumentCountQ[head,Length[args],0,3];$Failed,
   {All, __},
   		EntityValue[EntityClass[etype,All], Sequence@@Rest[args]],
   {$entityStandardNamePattern[etype], ___},
       If[ValidArgsForEtypeQ[head, etype, args],EntityValue[Entity[etype, args[[1]]], Sequence @@ Rest[args]],$Failed],
   {{($entityStandardNamePattern[etype] | _Entity) ..}, ___},
       If[ValidArgsForEtypeQ[head, etype, args],EntityValue[If[MatchQ[#, _Entity], #, Entity[etype, #]] & /@ args[[1]], Sequence @@ Rest[args]],$Failed],
   {Entity[etype,___], ___},
       If[ValidArgsForEtypeQ[head, etype, args],EntityValue @@ args,$Failed],
   {EntityClass[etype,___]},
   	   If[ValidArgsForEtypeQ[head, etype, args],EntityValue[First[args],"Entities"],$Failed],
   {EntityClass[etype,___],___},
   	   If[ValidArgsForEtypeQ[head, etype, args],EntityValue @@ args,$Failed],
   {EntityPropertyClass[etype,___] ___},
       If[ValidArgsForEtypeQ[head, etype, args],EntityValue @@ args,$Failed],
   _,
       With[{arg=If[ListQ[args]&&Length[args]>0,First[args],Null]},(*safety valve in case we have bad arguments; shouldn't actually need this...*)
       	Message[head::notent,arg,head];$Failed]
   ]];
   If[MatchQ[res, $Failed|_EntityValue], res = $Failed];
   res
  ]
  
ValidArgsForEtypeQ[head_,etype_,args_List] := Switch[args,
	{},True,
	{_}, True,
	{_,_String,___},True,
	{_,(EntityProperty|EntityPropertyClass)[etype,__],___},True,
	{_,_List,___},True,(*TODO: fine-tune this; need to support things like {"RandomEntities",8} on top of _EntityProperty.. and _EntityPropertyClass..*)
	{_,_,___},With[{prop=Part[args,2]},Message[head::notprop,prop,head];False],
	_, False
]
  
(*** End code for wrappers ***)
$EVMMODES= {"utility", "paclet", "entity", "semantic"};

entityCount[class:(_String|_Entity|_EntityClass)] := Block[{WolframAlphaClient`Private`$AlphaQueryMMode="utility"},
	With[{r = EntityValue[class, "EntityCount"]}, 
		If[MatchQ[r,_Missing], Throw[r,$tag]];
		Set[entityCount[class], r]/;IntegerQ[r]]]
entityCount[___] := Throw[$Failed, $tag]

entityClassCount[class:(_String|_Entity)] := Block[{WolframAlphaClient`Private`$AlphaQueryMMode="utility"},
	With[{r = EntityValue[class, "EntityClassCount"]}, Set[entityClassCount[class], r]/;IntegerQ[r]]]
entityClassCount[___] := Throw[$Failed, $tag]

$EntityBatchThreshold = 2500;
$EntityValueListThresholdValue = 128;

batchEntitiesQ[class:(_String|_Entity|_EntityClass)] := TimeConstrained[
	TrueQ[entityCount[class] > $EntityBatchThreshold],
	10,False]
batchEntitiesQ[__] := False

batchEntityClassesQ[class_] := TimeConstrained[
	TrueQ[entityClassCount[class] > $EntityBatchThreshold],
	10,False]
batchEntityClassesQ[__] := False

divideEntityCountIntoBatches[0] := Throw[{},$tag]
divideEntityCountIntoBatches[n_Integer] /; n > $EntityBatchThreshold := Block[
	{steps = Range[1, n, $EntityBatchThreshold], pairs},
	pairs = {#, # + $EntityBatchThreshold - 1} & /@ steps;
	ReplacePart[pairs, -1 -> {pairs[[-1, 1]], n}]]
	
divideEntityCountIntoBatches[n_Integer] /; n > 0 := {{1, n}}
divideEntityCountIntoBatches[___] := Throw[$Failed, $tag]

wrapWithAppropriateHead[etype_String,{start_,stop_},type_String] := wrapWithAppropriateHead[etype,_,{start,stop},type]
wrapWithAppropriateHead[etype_String,pat_,{start_,stop_},type_] := Switch[type,
	"Entities"|"EntityCanonicalNames",Entity[etype,pat,Span[start,stop]],
	_, EntityClass[etype,pat,Span[start,stop]]
]

wrapWithAppropriateHead[EntityClass[etype_],{start_,stop_},type_String] := wrapWithAppropriateHead[EntityClass[etype,_],{start,stop},type]
wrapWithAppropriateHead[class:EntityClass[etype_, pat_],{start_,stop_},type_String] := Switch[type,
	"Entities"|"EntityCanonicalNames",Entity[class,_,Span[start,stop]],
	_,EntityClass[etype,pat,Span[start,stop]]
]
wrapWithAppropriateHead[other_, ___] := other

downloadEntityBatch[etype_String,{start_Integer,stop_Integer},type:("EntityClasses"|"EntityClassCanonicalNames")] := Module[{},
	If[ListQ[#],#,Message[EntityValue::batmis,start,stop];{}]&[EntityValue[etype, {type, Span[start, stop]}]]
]
downloadEntityBatch[etype_String,{start_Integer,stop_Integer},type_String] := Module[{},
	If[ListQ[#],#,Message[EntityValue::batmis,start,stop];{}]&[EntityValue[wrapWithAppropriateHead[etype,{start,stop},type], type]]
]
downloadEntityBatch[e:Entity[_EntityClass,_,___],_, type_String] := Module[{},
	If[ListQ[#],#,Message[EntityValue::batmis,start,stop];{}]&[EntityValue[e, type]]
]
downloadEntityBatch[Entity[etype_],args__] := downloadEntityBatch[Entity[etype,_],args]
downloadEntityBatch[Entity[etype_,pat_,___],{start_Integer,stop_Integer},type_String] := Module[{},
	If[ListQ[#],#,Message[EntityValue::batmis,start,stop];{}]&[EntityValue[wrapWithAppropriateHead[etype,pat,{start,stop},type], type]]
]
downloadEntityBatch[class_EntityClass, {start_Integer, stop_Integer}, type_String] := Module[{},
 If[ListQ[#],#,Message[EntityValue::batmis,start,stop];{}]&[EntityValue[wrapWithAppropriateHead[class,{start,stop},type], type]]
]
downloadEntityBatch[___] := Throw[$Failed, $tag]

GetAllEntities[e:Entity[_String,Except[_Rule|{_Rule..}]], type_String] := callMWACompute[e,type]
GetAllEntities[class:(_Entity|_String|_EntityClass), type_String] := Block[{$GetEntitiesInBatches=False},Catch[
  With[{batches = divideEntityCountIntoBatches[entityCount[class]]},
    EntityFramework`Dialog`interruptableEntityDownloadManager[class, batches, type]], $tag]
]

getDataAndApplyAggrigationFunction[type_, batches_, prop_ ] := EntityFramework`Dialog`interruptableDataDownloadManager[type, batches, {prop}]
getDataAndApplyAggrigationFunction[type_, batches_, prop_, "Dataset"] := Dataset[getDataAndApplyAggrigationFunction[type, batches, prop, "EntityPropertyAssociation"]]
getDataAndApplyAggrigationFunction[type_, batches_, prop_, agg_] := EntityFramework`Dialog`interruptableDataDownloadManager[type, batches, {prop, agg}]

GetAllEntityValues[type_, prop_, agg___] := Block[{$GetEntityDataInBatches = False,$EntityBatchThreshold=$EntityValueListThresholdValue},Catch[
  If[batchEntitiesQ[type],
   With[{batches = divideEntityCountIntoBatches[entityCount[type]]},
    getDataAndApplyAggrigationFunction[type, batches, prop, agg]],
   iEntityValue[type, prop, agg]
   ], $tag]
	
]

GetAllEntityClassValues[class_EntityClass, prop_, agg___] := Block[{
	$GetEntityClassDataInBatches = False,$EntityBatchThreshold=$EntityValueListThresholdValue},Catch[
  If[batchEntitiesQ[class],
   With[{batches = divideEntityCountIntoBatches[entityCount[class]]},
    getDataAndApplyAggrigationFunction[class, batches, prop, agg]],
   iEntityValue[class, prop, agg]
   ], $tag]
	
]

$knownAnnotations = Alternatives[
	"Qualifiers", "QualifierValues", "Description", "Definition", "Source",
	"Date", "PhysicalQuantity", "Unit", "EntityAssociation", "PropertyAssociation",
	"EntityPropertyAssociation", "PropertyEntityAssociation", "Dataset"
];
checkAnn[$knownAnnotations] := True
checkAnn[agg_] := Module[{res},
	res = Internal`MWACompute["MWACalculateData",
		Internal`MWASymbols`MWAData[{Entity["Planet", "Earth"], Entity["Planet", "Mars"]}, "Mass", agg]];
	okAnnQ[res]
]

okAnnQ[res:Verbatim[HoldComplete][{_Rule ..}]] := FreeQ["Result"/.ReleaseMWAComputeHold[res], $Failed]
okAnnQ[___] := False

GetAllEntityClasses[class_, type_String] := Block[{$GetEntitiesInBatches=False},Catch[
  If[batchEntityClassesQ[class],
   With[{batches = divideEntityCountIntoBatches[entityClassCount[class]]},
    EntityFramework`Dialog`interruptableEntityDownloadManager[class, batches, type]],
   iEntityValue[class, type]
   ], $tag]
]
(* because you can't just Join a bunch of Datasets, one for each chunk *)
GetEntityValueInChunks[entities_, props_, "Dataset"] := Dataset[iGetEntityValueInChunks[entities, props, "EntityPropertyAssociation"]]
GetEntityValueInChunks[entities_, props_, agg_] := With[{okQ = checkAnn[agg]}, 
	If[okQ, 
		iGetEntityValueInChunks[entities, props, agg],
		Message[EntityValue::annf, agg]; $Failed
	]]
GetEntityValueInChunks[entities_, props_] := iGetEntityValueInChunks[entities, props]
GetEntityValueInChunks[___] := $Failed (*shouldn't get hit?*)

iGetEntityValueInChunks[entities_,args___] := Block[{$EntityBatchThreshold=$EntityValueListThresholdValue,groups},
	groups=divideEntityCountIntoBatches[Length[entities]];
	EntityFramework`Dialog`interruptableDataDownloadManager[groups,entities,args]
]


(*keep various flags & symbols from triggering Dynamic updating*)
Internal`SetValueNoTrack[{$dontFormatEntity,$EVQIDF,Internal`$QueryID,$EPLF}, True];

With[{s=$readProtectedSymbols},SetAttributes[s,{ReadProtected}]];
Protect@@$readProtectedSymbols;

If[FileExistsQ[#],Get[#]]&[FileNameJoin[{DirectoryName[$InputFileName],"Caching.m"}]];
If[FileExistsQ[#],Get[#]]&[FileNameJoin[{DirectoryName[$InputFileName],"Dialog.m"}]];
If[FileExistsQ[#],Get[#]]&[FileNameJoin[{DirectoryName[$InputFileName],"ToFromEntity.m"}]];
   
End[];

