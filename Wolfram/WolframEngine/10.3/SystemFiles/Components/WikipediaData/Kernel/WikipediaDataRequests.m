(* ::Package:: *)

(* Mathematica Package *)

BeginPackage["WikipediaData`"]
(* Exported symbols added here with SymbolName::usage *)  

System`WikipediaData

Begin["`Private`"] (* Begin Private Context *) 

Unprotect[WikipediaData]

Options[WikipediaData] = {"MaxLevelItems"->Automatic, "MaxItems"->Automatic,"MaxLevel"->Automatic,"Section"->Automatic,"StartDate"->Automatic,"EndDate"->Automatic}

WikipediaData[arg_,"ArticleContributors",opt:OptionsPattern[]] := Module[{result,limit,title,pageid},
	limit=OptionValue["MaxItems"]/.Automatic->100;
	title=arg;
	pageid=If[MatchQ[arg,Rule["PageID",_]],"PageID"/.arg,Missing["NotAvailable"]];

	Which[
		MatchQ[pageid,_Missing],
		result=wikipedia["ArticleContributors",{"Title"->title,"MaxItems"->limit}],
		!MatchQ[pageid,_Missing],
		result=wikipedia["ArticleContributors",{"PageID"->pageid,"MaxItems"->limit}],
		True,
		result=Missing["BadInput"]
	];

	result
];

WikipediaData[search_,"ArticleOpenSearch",opt:OptionsPattern[]] := Module[{result,limit},
	limit=OptionValue["MaxItems"]/.Automatic->100;
	If[MatchQ[search,Rule["Search",_String]],
		result=wikipedia["ArticleOpenSearch",{search,"MaxItems"->limit}],
		result=Missing["BadInput"]
	];
	result
];

WikipediaData[arg_,"ArticlePlaintext"] := Module[{result,title,pageid},
	title=arg;
	pageid=If[MatchQ[arg,Rule["PageID",_]],"PageID"/.arg,Missing["NotAvailable"]];
	
	Which[
		MatchQ[pageid,_Missing],
		result=wikipedia["ArticlePlaintext",{"Title"->title}],
		!MatchQ[pageid,_Missing],
		result=wikipedia["ArticlePlaintext",{"PageID"->pageid}],
		True,
		result=Missing["BadInput"]
	];
	
	result
];

WikipediaData[arg_] := Module[{result,title,pageid,category},
	category=If[MatchQ[arg,Rule["Category",_]],"Category"/.arg,Missing["NotAvailable"]];
	title=arg;
	pageid=If[MatchQ[arg,Rule["PageID",_]],"PageID"/.arg,Missing["NotAvailable"]];

	Which[
		!MatchQ[category,_Missing],
		result=wikipedia["CategoryArticles",{"Category"->category}],
		MatchQ[pageid,_Missing],
		result=wikipedia["ArticlePlaintext",{"Title"->title}],
		!MatchQ[pageid,_Missing],
		result=wikipedia["ArticlePlaintext",{"PageID"->pageid}],
		True,
		result=Missing["BadInput"]
	];

	result
];

WikipediaData[arg_,"ArticleWikicode"] := Module[{result,title,pageid},
	title=arg;
	pageid=If[MatchQ[arg,Rule["PageID",_]],"PageID"/.arg,Missing["NotAvailable"]];

	Which[
		MatchQ[pageid,_Missing],
		result=wikipedia["ArticleWikicode",{"Title"->title}],
		!MatchQ[pageid,_Missing],
		result=wikipedia["ArticleWikicode",{"PageID"->pageid}],
		True,
		result=Missing["BadInput"]
	];

	result
];

WikipediaData[arg_,"BacklinksRules",opt:OptionsPattern[]] := Module[{result,limit,level,title,pageid},
	limit=OptionValue["MaxLevelItems"]/.Automatic->Missing["NotAvailable"];
	level=OptionValue["MaxLevel"]/.Automatic->1;

	title=arg;
	pageid=If[MatchQ[arg,Rule["PageID",_]],"PageID"/.arg,Missing["NotAvailable"]];

	Which[
		MatchQ[pageid,_Missing],
		result=wikipedia["BacklinksRules",{"Title"->title,"MaxLevelItems"->limit,"MaxLevel"->level}],
		!MatchQ[pageid,_Missing],
		result=wikipedia["BacklinksRules",{"PageID"->pageid,"MaxLevelItems"->limit,"MaxLevel"->level}],
		True,
		result=Missing["BadInput"]
	];

	result
];

WikipediaData[arg_,"BacklinksList",opt:OptionsPattern[]] := Module[{result,limit,level,title,pageid},
	limit=OptionValue["MaxLevelItems"]/.Automatic->Missing["NotAvailable"];
	level=OptionValue["MaxLevel"]/.Automatic->1;

	title=arg;
	pageid=If[MatchQ[arg,Rule["PageID",_]],"PageID"/.arg,Missing["NotAvailable"]];

	Which[
		MatchQ[pageid,_Missing],
		result=wikipedia["BacklinksList",{"Title"->title,"MaxLevelItems"->limit,"MaxLevel"->level}],
		!MatchQ[pageid,_Missing],
		result=wikipedia["BacklinksLists",{"PageID"->pageid,"MaxLevelItems"->limit,"MaxLevel"->level}],
		True,
		result=Missing["BadInput"]
	];

	result
];

WikipediaData[arg_,"CategoryArticles",opt:OptionsPattern[]] := Module[{result,limit},
	If[MatchQ[arg,Rule["Category",_String]]||MatchQ[arg,Rule["Category",{__String}]],
		limit=OptionValue["MaxItems"]/.Automatic->100;
		result=wikipedia["CategoryArticles",{arg,"MaxItems"->limit}],
		result=Missing["BadInput"]
	];

	result
];

WikipediaData[arg_,"CategoryArticleIDs",opt:OptionsPattern[]] := Module[{result,limit},
	If[MatchQ[arg,Rule["Category",_String]]||MatchQ[arg,Rule["Category",{__String}]],
		limit=OptionValue["MaxItems"]/.Automatic->All;
		result=wikipedia["CategoryArticleIDs",{arg,"MaxItems"->limit}],
		result=Missing["BadInput"]
	];

	result
];

WikipediaData[arg_,"CategoryLinks",opt:OptionsPattern[]] := Module[{result,level},
	If[MatchQ[arg,Rule["Category",_String]]||MatchQ[arg,Rule["Category",{__String}]],
		level=OptionValue["MaxLevel"]/.Automatic->2;
		result=wikipedia["CategoryLinks",{arg,"MaxLevel"->level}],
		result=Missing["BadInput"]
	];

	result
];

WikipediaData[arg_,"CategoryMembers",opt:OptionsPattern[]] := Module[{result,limit},
	If[MatchQ[arg,Rule["Category",_String]]||MatchQ[arg,Rule["Category",{__String}]],
		limit=OptionValue["MaxItems"]/.Automatic->100;
		result=wikipedia["CategoryMembers",{arg,"MaxItems"->limit}],
		result=Missing["BadInput"]
	];

	result
];

WikipediaData[arg_,"CategoryMemberIDs",opt:OptionsPattern[]] := Module[{result,limit},
	If[MatchQ[arg,Rule["Category",_String]]||MatchQ[arg,Rule["Category",{__String}]],
		limit=OptionValue["MaxItems"]/.Automatic->100;
		result=wikipedia["CategoryMemberIDs",{arg,"MaxItems"->limit}],
		result=Missing["BadInput"]
	];

	result
];

WikipediaData[arg_,"CategorySearch",opt:OptionsPattern[]] := Module[{result,limit},
	If[MatchQ[arg,Rule["Search",_String]],
		limit=OptionValue["MaxItems"]/.Automatic->100;
		result=wikipedia["CategorySearch",{arg,"MaxItems"->limit}],
		result=Missing["BadInput"]
	];

	result
];

WikipediaData[arg_,"ContentSearch",opt:OptionsPattern[]] := Module[{result,limit},
	If[MatchQ[arg,Rule["Content",_String]]||MatchQ[arg,Rule["Content",{__String}]],
		limit=OptionValue["MaxItems"]/.Automatic->100;
		result=wikipedia["ContentSearch",{arg,"MaxItems"->limit}],
		result=Missing["BadInput"]
	];

	result
];

WikipediaData[arg_,"ContributorArticles",opt:OptionsPattern[]] := Module[{result,limit},
	If[MatchQ[arg,Rule["Contributor",_String]],
		limit=OptionValue["MaxItems"]/.Automatic->100;
		result=wikipedia["ContributorArticles",{arg,"MaxItems"->limit}],
		result=Missing["BadInput"]
	];

	result
];

WikipediaData[arg_,"ExternalLinks"] := Module[{result,title,pageid},
	title=arg;
	pageid=If[MatchQ[arg,Rule["PageID",_]],"PageID"/.arg,Missing["NotAvailable"]];

	Which[
		MatchQ[pageid,_Missing],
		result=wikipedia["ExternalLinks",{"Title"->title}],
		!MatchQ[pageid,_Missing],
		result=wikipedia["ExternalLinks",{"PageID"->pageid}],
		True,
		result=Missing["BadInput"]
	];

	result
];

(***********************************************************************************)

WikipediaData[arg_,"GeoPosition"] := Module[{result,title,pageid},
	title=arg;
	pageid=If[MatchQ[arg,Rule["PageID",_]],"PageID"/.arg,Missing["NotAvailable"]];

	Which[
		MatchQ[pageid,_Missing],
		result=wikipedia["GeoPosition",{"Title"->title}],
		!MatchQ[pageid,_Missing],
		result=wikipedia["GeoPosition",{"PageID"->pageid}],
		True,
		result=Missing["BadInput"]
	];

	result
];

WikipediaData[title_String,"GeoNearbyArticles",opt:OptionsPattern[]] := Module[{result},
	If[MatchQ[opt,_List],
		result=wikipedia["GeoNearbyArticles",{"Title"->title,Sequence@@opt}],
		result=wikipedia["GeoNearbyArticles",{"Title"->title,opt}]
	];
	result
];

WikipediaData["GeoNearbyArticles",opt_List] := Module[{result},
	result=wikipedia["GeoNearbyArticles",opt];
	result
];

WikipediaData["GeoNearbyArticles",opt__Rule] := Module[{result},
	result=wikipedia["GeoNearbyArticles",{opt}];
	result
];

WikipediaData[title_String,"GeoNearbyDataset",opt:OptionsPattern[]] := Module[{result},
	If[MatchQ[opt,_List],
		result=wikipedia["GeoNearbyDataset",{"Title"->title,Sequence@@opt}],
		result=wikipedia["GeoNearbyDataset",{"Title"->title,opt}];
	];
	result
];

WikipediaData["GeoNearbyDataset",opt__] := Module[{result},
	result=wikipedia["GeoNearbyDataset",{opt}];
	result
];

(*
WikipediaData[title_String,"GeoNearbyDataset",opt_List] := Module[{options,result},
	result={};
	options={"Title"->title};
	options=Join[options,opt];
	result=wikipedia["GeoNearbyArticles",options];
	If[MatchQ[result,{__}],
		result=Dataset[Association@@@result]
	];
	result
];

WikipediaData["GeoNearbyDataset",opt_List] := Module[{result},
	result=wikipedia["GeoNearbyArticles",opt];
	If[MatchQ[result,{__}],
		result=Dataset[Association@@@result]
	];
	result
];
*)

(***********************************************************************************)

WikipediaData[arg_,"ImageDataset",opt:OptionsPattern[]] := Module[{result,title,pageid},
	title=arg;
	pageid=If[MatchQ[arg,Rule["PageID",_]],"PageID"/.arg,Missing["NotAvailable"]];

	Which[
		MatchQ[pageid,_Missing],
		result=wikipedia["ImageDataset",{"Title"->title,opt}],
		!MatchQ[pageid,_Missing],
		result=wikipedia["ImageDataset",{"PageID"->pageid,opt}],
		True,
		result=Missing["BadInput"]
	];

	result
];

WikipediaData[arg_,"ImageList"] := Module[{result,title,pageid},
	title=arg;
	pageid=If[MatchQ[arg,Rule["PageID",_]],"PageID"/.arg,Missing["NotAvailable"]];

	Which[
		MatchQ[pageid,_Missing],
		result=wikipedia["ImageList",{"Title"->title}],
		!MatchQ[pageid,_Missing],
		result=wikipedia["ImageList",{"PageID"->pageid}],
		True,
		result=Missing["BadInput"]
	];

	result
];

WikipediaData[arg_,"ImageURLs"] := Module[{result,title,pageid},
	title=arg;
	pageid=If[MatchQ[arg,Rule["PageID",_]],"PageID"/.arg,Missing["NotAvailable"]];

	Which[
		MatchQ[pageid,_Missing],
		result=wikipedia["ImageURLs",{"Title"->title}],
		!MatchQ[pageid,_Missing],
		result=wikipedia["ImageURLs",{"PageID"->pageid}],
		True,
		result=Missing["BadInput"]
	];

	result
];

(***********************************************************************************)

WikipediaData[arg_,"LanguagesURLRules"] := Module[{result,title,pageid},
	title=arg;
	pageid=If[MatchQ[arg,Rule["PageID",_]],"PageID"/.arg,Missing["NotAvailable"]];

	Which[
		MatchQ[pageid,_Missing],
		result=wikipedia["LanguagesURLRules",{"Title"->title}],
		!MatchQ[pageid,_Missing],
		result=wikipedia["LanguagesURLRules",{"PageID"->pageid}],
		True,
		result=Missing["BadInput"]
	];

	result
];

WikipediaData[arg_,"LanguagesList"] := Module[{result,title,pageid},
	title=arg;
	pageid=If[MatchQ[arg,Rule["PageID",_]],"PageID"/.arg,Missing["NotAvailable"]];

	Which[
		MatchQ[pageid,_Missing],
		result=wikipedia["LanguagesList",{"Title"->title}],
		!MatchQ[pageid,_Missing],
		result=wikipedia["LanguagesList",{"PageID"->pageid}],
		True,
		result=Missing["BadInput"]
	];

	result
];

WikipediaData[arg_,"LanguagesURLs"] := Module[{result,title,pageid},
	title=arg;
	pageid=If[MatchQ[arg,Rule["PageID",_]],"PageID"/.arg,Missing["NotAvailable"]];

	Which[
		MatchQ[pageid,_Missing],
		result=wikipedia["LanguagesURLs",{"Title"->title}],
		!MatchQ[pageid,_Missing],
		result=wikipedia["LanguagesURLs",{"PageID"->pageid}],
		True,
		result=Missing["BadInput"]
	];

	result
];

(***********************************************************************************)

WikipediaData[arg_,"LinksRules",opt:OptionsPattern[]] := Module[{result,limit,level,title,pageid},
	limit=OptionValue["MaxLevelItems"]/.Automatic->All;
	level=OptionValue["MaxLevel"]/.Automatic->1;

	title=arg;
	pageid=If[MatchQ[arg,Rule["PageID",_]],"PageID"/.arg,Missing["NotAvailable"]];

	Which[
		MatchQ[pageid,_Missing],
		result=wikipedia["LinksRules",{"Title"->title,"MaxLevelItems"->limit,"MaxLevel"->level}],
		!MatchQ[pageid,_Missing],
		result=wikipedia["LinksRules",{"PageID"->pageid,"MaxLevelItems"->limit,"MaxLevel"->level}],
		True,
		result=Missing["BadInput"]
	];

	result
];

WikipediaData[arg_,"LinksList",opt:OptionsPattern[]] := Module[{result,limit,level,title,pageid},
	limit=OptionValue["MaxLevelItems"]/.Automatic->All;
	level=OptionValue["MaxLevel"]/.Automatic->1;

	title=arg;
	pageid=If[MatchQ[arg,Rule["PageID",_]],"PageID"/.arg,Missing["NotAvailable"]];

	Which[
		MatchQ[pageid,_Missing],
		result=wikipedia["LinksList",{"Title"->title,"MaxLevelItems"->limit,"MaxLevel"->level}],
		!MatchQ[pageid,_Missing],
		result=wikipedia["LinksList",{"PageID"->pageid,"MaxLevelItems"->limit,"MaxLevel"->level}],
		True,
		result=Missing["BadInput"]
	];

	result
];

(***********************************************************************************)

WikipediaData[arg_,"PageID"] := Module[{result,title,pageid},
	title=arg;
	pageid=If[MatchQ[arg,Rule["PageID",_]],"PageID"/.arg,Missing["NotAvailable"]];
	
	Which[
		MatchQ[pageid,_Missing],
		result=wikipedia["PageID",{"Title"->title}],
		!MatchQ[pageid,_Missing],
		result=wikipedia["PageID",{"PageID"->pageid}],
		True,
		result=Missing["BadInput"]
	];
	
	result
];

(***********************************************************************************)

WikipediaData[arg_,"ParentCategories"] := Module[{result,title,pageid},
	title=arg;
	pageid=If[MatchQ[arg,Rule["PageID",_]],"PageID"/.arg,Missing["NotAvailable"]];

	Which[
		MatchQ[pageid,_Missing],
		result=wikipedia["ParentCategories",{"Title"->title}],
		!MatchQ[pageid,_Missing],
		result=wikipedia["ParentCategories",{"PageID"->pageid}],
		True,
		result=Missing["BadInput"]
	];

	result
];

(***********************************************************************************)

WikipediaData[arg_,"Revisions",opt:OptionsPattern[]] := Module[{result,limit,startDate,endDate,title,pageid},
	limit=OptionValue["MaxItems"]/.Automatic->10;
	startDate=OptionValue["StartDate"]/.Automatic->Now;
	If[!MatchQ[startDate,_DateObject],Throw[$Failed]];
	endDate=OptionValue["EndDate"]/.Automatic->DateObject[{2000, 1, 1}];
	If[!MatchQ[endDate,_DateObject],Throw[$Failed]];

	title=arg;
	pageid=If[MatchQ[arg,Rule["PageID",_]],"PageID"/.arg,Missing["NotAvailable"]];

	Which[
		MatchQ[pageid,_Missing],
		result=wikipedia["Revisions",{"Title"->title,"MaxItems"->limit,"StartDate"->startDate,"EndDate"->endDate}],
		!MatchQ[pageid,_Missing],
		result=wikipedia["Revisions",{"PageID"->pageid,"MaxItems"->limit,"StartDate"->startDate,"EndDate"->endDate}],
		True,
		result=Missing["BadInput"]
	];

	result
];

(***********************************************************************************)

WikipediaData[arg_,"SeeAlsoList"] := Module[{result,title,pageid},
	title=arg;
	pageid=If[MatchQ[arg,Rule["PageID",_]],"PageID"/.arg,Missing["NotAvailable"]];

	Which[
		MatchQ[pageid,_Missing],
		result=wikipedia["SeeAlsoList",{"Title"->title}],
		!MatchQ[pageid,_Missing],
		result=wikipedia["SeeAlsoList",{"PageID"->pageid}],
		True,
		result=Missing["BadInput"]
	];

	result
];

WikipediaData[arg_,"SeeAlsoRules"] := Module[{result,title,pageid},
	title=arg;
	pageid=If[MatchQ[arg,Rule["PageID",_]],"PageID"/.arg,Missing["NotAvailable"]];

	Which[
		MatchQ[pageid,_Missing],
		result=wikipedia["SeeAlsoRules",{"Title"->title}],
		!MatchQ[pageid,_Missing],
		result=wikipedia["SeeAlsoRules",{"PageID"->pageid}],
		True,
		result=Missing["BadInput"]
	];

	result
];

(***********************************************************************************)

WikipediaData[arg_,"SummaryPlaintext"] := Module[{result,title,pageid},
	title=arg;
	pageid=If[MatchQ[arg,Rule["PageID",_]],"PageID"/.arg,Missing["NotAvailable"]];

	Which[
		MatchQ[pageid,_Missing],
		result=wikipedia["SummaryPlaintext",{"Title"->title}],
		!MatchQ[pageid,_Missing],
		result=wikipedia["SummaryPlaintext",{"PageID"->pageid}],
		True,
		result=Missing["BadInput"]
	];

	result
];

(***********************************************************************************)

WikipediaData[arg_,"SummaryWikicode",opt:OptionsPattern[]] := Module[{result,section,title,pageid},
	section=OptionValue["Section"]/.Automatic->0;

	title=arg;
	pageid=If[MatchQ[arg,Rule["PageID",_]],"PageID"/.arg,Missing["NotAvailable"]];

	Which[
		MatchQ[pageid,_Missing],
		result=wikipedia["SummaryWikicode",{"Title"->title,"Section"->section}],
		!MatchQ[pageid,_Missing],
		result=wikipedia["SummaryWikicode",{"PageID"->pageid,"Section"->section}],
		True,
		result=Missing["BadInput"]
	];

	result
];

(***********************************************************************************)

WikipediaData[arg_,"Tables"] := Module[{result,title,pageid},

	title=arg;
	pageid=If[MatchQ[arg,Rule["PageID",_]],"PageID"/.arg,Missing["NotAvailable"]];

	Which[
		MatchQ[pageid,_Missing],
		result=wikipedia["Tables",{"Title"->title}],
		!MatchQ[pageid,_Missing],
		result=wikipedia["Tables",{"PageID"->pageid}],
		True,
		result=Missing["BadInput"]
	];

	result
];

(***********************************************************************************)

WikipediaData[arg_,"Title"] := Module[{result,title,pageid},
	title=arg;
	pageid=If[MatchQ[arg,Rule["PageID",_]],"PageID"/.arg,Missing["NotAvailable"]];
	
	Which[
		MatchQ[pageid,_Missing],
		result=wikipedia["Title",{"Title"->title}],
		!MatchQ[pageid,_Missing],
		result=wikipedia["Title",{"PageID"->pageid}],
		True,
		result=Missing["BadInput"]
	];
	
	result
];

(***********************************************************************************)

WikipediaData[search_,"TitleSearch",opt:OptionsPattern[]] := Module[{result,limit},
	limit=OptionValue["MaxItems"]/.Automatic->100;
	If[MatchQ[search,Rule["Search",_String]],
		result=wikipedia["TitleSearch",{search,"MaxItems"->limit}],
		result=Missing["BadInput"]
	];
	result
];

(***********************************************************************************)

WikipediaData[arg_,"TitleTranslationRules",opt:OptionsPattern[]] := Module[{result,limit,title,pageid},
	limit=OptionValue["MaxItems"]/.Automatic->All;

	title=arg;
	pageid=If[MatchQ[arg,Rule["PageID",_]],"PageID"/.arg,Missing["NotAvailable"]];

	Which[
		MatchQ[pageid,_Missing],
		result=wikipedia["TitleTranslationRules",{"Title"->title,"MaxItems"->limit}],
		!MatchQ[pageid,_Missing],
		result=wikipedia["TitleTranslationRules",{"PageID"->pageid,"MaxItems"->limit}],
		True,
		result=Missing["BadInput"]
	];

	result
];

WikipediaData[arg_,"TitleTranslations",opt:OptionsPattern[]] := Module[{result,limit,title,pageid},
	limit=OptionValue["MaxItems"]/.Automatic->All;

	title=arg;
	pageid=If[MatchQ[arg,Rule["PageID",_]],"PageID"/.arg,Missing["NotAvailable"]];

	Which[
		MatchQ[pageid,_Missing],
		result=wikipedia["TitleTranslations",{"Title"->title,"MaxItems"->limit}],
		!MatchQ[pageid,_Missing],
		result=wikipedia["TitleTranslations",{"PageID"->pageid,"MaxItems"->limit}],
		True,
		result=Missing["BadInput"]
	];

	result
];

(***********************************************************************************)

WikipediaData["WikipediaRecentChanges"] := wikipedia["WikipediaRecentChanges"];

WikipediaData["WikipediaRecentChanges",opt:OptionsPattern[]] := Module[{result,limit},
	limit=OptionValue["MaxItems"]/.Automatic->100;
	result=wikipedia["WikipediaRecentChanges",{"MaxItems"->limit}]
];

(***********************************************************************************)

Protect[WikipediaData]
End[] (* End Private Context *)

EndPackage[]
