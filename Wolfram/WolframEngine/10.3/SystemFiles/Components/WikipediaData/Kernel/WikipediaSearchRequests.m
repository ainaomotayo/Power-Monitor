(* ::Package:: *)

(* Mathematica Package *)

BeginPackage["WikipediaData`"]
(* Exported symbols added here with SymbolName::usage *)  

System`WikipediaData

Begin["`Private`"] (* Begin Private Context *) 

Unprotect[WikipediaSearch]

Options[WikipediaSearch] = {"MaxItems"->Automatic,"Level"->Automatic,"Section"->Automatic,Method->Automatic}

WikipediaSearch["Category"->category_,opt:OptionsPattern[]] := Module[{result,limit},
	limit=OptionValue["MaxItems"]/.Automatic->100;
	result=wikipedia["CategorySearch",{"Search"->category,"MaxItems"->limit}];
	result
];

WikipediaSearch[search_String,opt:OptionsPattern[]] := WikipediaSearch["Title"->search,opt]

WikipediaSearch[search_List,opt:OptionsPattern[]] := WikipediaSearch["Title"->search,opt]

WikipediaSearch["Title"->search_String,opt:OptionsPattern[]] := Module[{result,limit,method},
	limit=OptionValue["MaxItems"]/.Automatic->100;
	method=OptionValue[Method]/.Automatic->"OpenSearch";

	If[!MemberQ[{"OpenSearch","MostLikely"},method],Throw[$Failed]];
	If[!MatchQ[limit,_Integer]&&!MatchQ[limit,All],Throw[$Failed]];
	
	Which[
		method=="OpenSearch",
		
		result=wikipedia["ArticleOpenSearch",{"Search"->search,"MaxItems"->limit}],

		method=="MostLikely",

		result=wikipedia["TitleSearch",{"Search"->search,"MaxItems"->limit}]
	];

	result
];

WikipediaSearch["Content"->search_,opt:OptionsPattern[]] := Module[{result,limit},
	limit=OptionValue["MaxItems"]/.Automatic->100;
	If[!MatchQ[limit,_Integer]&&!MatchQ[limit,All],Throw[$Failed]];
	result=wikipedia["ContentSearch",{"Content"->search,"MaxItems"->limit}];
	result
];

(*
WikipediaSearch["Content"->text_,opt:OptionsPattern[]] := Module[{result,limit},
	limit=OptionValue["MaxItems"]/.Automatic->Missing["NotAvailable"];
	result=wikipedia["ContentSearch",{"Search"->text,"MaxItems"->limit}];
	result
];
*)

WikipediaSearch["GeoLocation"->title_String,opt___] := Module[{result},
	result=wikipedia["GeoNearbyArticles",{"Title"->title,opt}];
	result
];

Protect[WikipediaSearch]
End[] (* End Private Context *)

EndPackage[]