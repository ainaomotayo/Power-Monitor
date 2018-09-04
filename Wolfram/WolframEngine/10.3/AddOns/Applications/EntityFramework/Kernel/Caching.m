(* Mathematica package *)
Begin["EntityFramework`Caching`"];

Clear[cacheElements,findFirstElementNotInCache,findLastElementNotInCache];
SetAttributes[{cacheElements,findFirstElementNotInCache,findLastElementNotInCache}, HoldFirst];

cacheElements[association_, 
   elements_List, {min_Integer, max_Integer}] /; 
  Length[elements] === max - min + 1 := Block[{it = 0},
  Do[it++; association[i] = elements[[it]], {i, min, max}]
  ]

findFirstElementNotInCache[association_, {min_Integer, max_Integer}] :=
  Catch[Do[
  	If[Not[KeyExistsQ[association, i]], Throw[i, "Stop"]],
  	{i, min, max}], "Stop"]
   
findLastElementNotInCache[association_, {min_Integer, max_Integer}] :=
  Catch[Scan[If[Not[KeyExistsQ[association, #]], Throw[#, "Stop"]]&, 
  	Reverse[Range[min,max]]], "Stop"]
   
$MaxCacheSize = 10000;

InitializeEntityCaches[] := Module[{},
	Clear[$EntityMemberCache, $EntityValueDataCache];
	$EntityMemberCache = Association[];
	$EntityValueDataCache = Association[];
	$EntityValueTracker = Association[];
	$EntityValueCurrent = 1;
	$EVDCSize = 0;
	$EMCSize = 0;
]

InitializeEntityCaches[];(*initialize the caches*)

cacheEntityMembers[Entity[type_String],{start_,end_}] := cacheEntityMembers[type,{start,end}] 
cacheEntityMembers[type_, {start_Integer, end_Integer}] := 
 Block[{var = Association[]},
  If[Not[KeyExistsQ[$EntityMemberCache, type]], 
   $EntityMemberCache[type] = Association[];];
  Block[{first = findFirstElementNotInCache[$EntityMemberCache[type], {start, end}],
     last},(*don't try to find last missing unless there are missings; avoids extra work*)
   If[UnsameQ[first, Null], 
   	last = findLastElementNotInCache[$EntityMemberCache[type], {start, end}];
    cacheElements[var, 
     EntityValue[Append[Entity@@type,Span[first,last]], 
      "Entities"], {first, last}];
    $EntityMemberCache[type] = Join[$EntityMemberCache[type], var]];
   DeleteCases[Map[$EntityMemberCache[type], Range[start, end]],Missing["KeyAbsent", ___]]]
  ]
  
addData[key_, info_] := Module[{},
  If[$EntityValueCurrent > $MaxCacheSize, $EntityValueCurrent = 1];
  If[Not[MatchQ[$EntityValueTracker[$EntityValueCurrent], Missing["KeyAbsent",___]]], 
  	$EntityValueDataCache[$EntityValueTracker[$EntityValueCurrent]] =.];
  $EntityValueTracker[$EntityValueCurrent] = key;
  $EntityValueDataCache[key] = info;
  $EntityValueCurrent++
  ]


cachablePropertyQ[Entity[type_String,_], prop_String] := TrueQ[EntityFramework`Private`EPDC[{type,prop}]]
cachablePropertyQ[_, EntityProperty[type_String,prop_String,___]] := TrueQ[EntityFramework`Private`EPDC[{type,prop}]]
cachablePropertyQ[__] := False


SetAttributes[toEntityCache, Listable];
toEntityCache[entity_Entity, property_, value_] := If[cachablePropertyQ[entity,property],
 With[{key = Hash[{entity, property}]}, 
 	addData[key,value];value
  ], value](*else don't cache*)

fromEntityCache[entity_Entity, property_] := 
 With[{key = Hash[{entity, property}]}, $EntityValueDataCache[key]
  ]
 
cacheEntityData[entity_Entity,property_] :=
 With[{r=fromEntityCache[entity,property]},
 If[MatchQ[r,Missing["KeyAbsent",___]],
 toEntityCache[entity,property,EntityValue[entity,property]],
 r]
 ]
 
cacheEntityData[entities:{_Entity..},property_] :=  Module[{
	res=Reap[Map[With[{r = fromEntityCache[#,property]}, 
		If[MatchQ[r, Missing["KeyAbsent",___]], Sow[#]; sowPlaceholder[#], r]] &, 
		entities]],ask,temp},
	ask=Last[res];
	If[UnsameQ[ask,{}],
		ask=First[ask];(*remove outer list*)
		temp = EntityValue[ask,property];
		toEntityCache[ask,property,temp];
		ask=Thread[sowPlaceholder/@ask->temp];
		res=First[res]/.ask
		,(*TODO: handle errors*)
		res=First[res]
	];
	res
]

cacheEntityData[other___] := EntityValue[other]

  
End[];