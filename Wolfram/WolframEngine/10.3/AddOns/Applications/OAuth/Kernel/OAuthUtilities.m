System`Private`NewContextPath[{"OAuthClient`","System`"}];
(*
OAuthClient`ob;
OAuthClient`deob;
OAuthClient`getclientinfo;

OAuthClient`getdata;
OAuthClient`prettygrid;
OAuthClient`formatvalue;
OAuthClient`createquantity;
OAuthClient`filterParameters;
OAuthClient`camelCase;
OAuthClient`fromunicode;
OAuthClient`formatpath;
OAuthClient`flatGrid;
OAuthClient`gridList;
OAuthClient`addtitles;
OAuthClient`createMap;
OAuthClient`createAnimatedMap;
OAuthClient`updateOAuthPaclet;
OAuthClient`eventtimeline;
OAuthClient`gridRequests;
*)
Begin["OAuthClient`"] (* Begin Private Context *) 

Begin["`Private`"]

ServiceConnect::pacupd="The OAuth paclet was updated";
ServiceConnect::npacup="The OAuth paclet is up to date";

(Unprotect[#]; Clear[#])& /@ {ob,deob,getdata,prettygrid,filterParameters,getclientinfo,updateOAuthPaclet}


formatvalue[_[label_,fun_]]:=(Rule[label,value_]:>Rule[label,fun[value]])

createquantity[l_List]:=createquantity/@l
createquantity[_[label_,unit_]]:=(Rule[label,value_]:>Rule[label,Quantity[value, unit]])
 
formatpath[fields_,values___]:=With[{n=Length[{values}]},
	Sequence@@Join[MapThread[(#1[#2])&,{Take[fields,n],{values}}],
		Map[#1[]&,Drop[fields,n]]
	]
]

getdata[data_,keys_List,rest___]:=Fold[getdata[##,rest]&,data,keys]
getdata[{},key_,___]:={}
getdata[data_,key_,___]:=(key/.data)/;!FreeQ[data,Rule[key,_]]
getdata[_,key_]:=(Message[ServiceExecute::ndata,key];Throw[$Failed])
getdata[_,key_,def_,___]:=def

$includeGridRequests=False;
gridRequests[args__]:=Sequence@@If[$includeGridRequests,{args},{}]

prettygrid[args___]:=Grid[args, Background -> {None, {Lighter[Yellow, .9], {White, 
    Lighter[Blend[{Blue, Green}], .8]}}}, Dividers -> {{Darker[
    Gray, .6], {Lighter[Gray, .5]}, 
   Darker[Gray, .6]}, {Darker[Gray, .6], Darker[Gray, .6], {False}, 
   Darker[Gray, .6]}}, Alignment -> {{Left, Right, {Left}}}]

flatGrid[params_,data_,formatrules_:{},rest___]:=flatgridfun[params,data/.formatrules,rest]
flatgridfun[params_List, data_, gridopts___] := 
 prettygrid[{camelCase[#[[1]]],#[[2]]}&/@DeleteCases[Reap[flatgridfun0[#, data] & /@ params][[2, 1]],{a_,a_},{1}], gridopts]
 
flatgridfun0[{label_, params_List}, 
  data_] := (flatgridfun0[#, label /. data] & /@ params) /; !ListQ[label]
flatgridfun0[params_List, data_] := flatgridfun0[#, data] & /@ params
flatgridfun0[param_, data_] := Sow[{param, param /. data}]

addtitles[grid_Grid,title_]:=ReplacePart[grid,1->Prepend[grid[[1]],title]]

gridList[_,{},___]:={}

gridList[params_,data_,rest___]:=gridlistfun[params,data,rest]
gridlistfun[params_List, data_, gridopts___] := With[{tmp=Reap[flatgridfun0[#, data] & /@ params][[2, 1]]},
 prettygrid[Join[{camelCase[tmp[[All, 1]]]}, Transpose[tmp[[All, 2]]]], gridopts]]
 
filterParameters[given:{(_Rule|_RuleDelayed)...},accepted_,separators_:{"_"}]:=Module[{camel=camelCase[accepted,separators]},
	Cases[given,HoldPattern[Rule|RuleDelayed][Alternatives@@Join[accepted, camel],_],Infinity]/.Thread[camel->accepted]
]
filterParameters[___]:=Throw[$Failed]

camelCase[l_List, rest___]:=camelCase[#,rest]&/@l
camelCase[str_String, separators_:{"_"}]:=StringReplace[
 StringReplace[
  StringReplace[str, 
   Thread[separators -> " "]], {WordBoundary ~~ word_ :> 
    ToUpperCase[word]}], {"Id"~~WordBoundary->"ID",WhitespaceCharacter -> "","Url"~~WordBoundary->"URL","Urls"~~WordBoundary->"URLs"}]

fromunicode[str_]:=StringReplace[str, "\\u" ~~ x : (WordCharacter ..)/;StringLength[x]<5 :> (FromCharacterCode@FromDigits[x, 16])]

(****************** createMap **********************)
llpattern={_?NumberQ,_?NumberQ}

createMap[latlongs:{{llpattern..}..},times0_,dists_,opts___]:=Module[
	{range1, range2, bg,center,lines,markers,colors,i, n=Length[latlongs],size},
	range1=Through@{Min,Max}@latlongs[[All,All,1]];
	range2=Through@{Min,Max}@latlongs[[All,All,2]];
	size=Norm[Subtract @@@ {range1, range1}];
	range1+=.1 size {-1,1};
	range2+=.1 size {-1,1};
	center=Point[{Mean[range2],Mean[range1]}];
	colors=Table[ColorData[1][i],{i,n}];
	markers=
		Flatten[MapThread[getMapMarkers["PathMarkers"/.{opts},#1,#2,#3,
			First[GeoDistance[Sequence @@ Transpose[{range1, range2}]]]/(Max[n,3] * 10^3),#4]&,
			{times0, dists, latlongs,colors}]];

	bg=GeoGraphics[{Blue, center,markers}, 
	 GeoBackground -> {GeoStyling["StreetMap"]}, 
	 GeoRange -> {range1, range2}];
	lines=Flatten[MapIndexed[{colors[[#2[[1]]]],Line[GeoPosition[ #1]]}&,latlongs]];
	bg/.center->{Thickness[Large], lines}
]

createAnimatedMap[latlongs:{llpattern..}, times0_,dists_, opts___]:=Module[
	{range1, range2, bg,line,t,linesf,pointf,size},
	range1=Through@{Min,Max}@latlongs[[All,1]];
	range2=Through@{Min,Max}@latlongs[[All,2]];
	size=Norm[Subtract @@@ {range1, range1}];
	range1+=.1 size {-1,1};
	range2+=.1 size {-1,1};
	bg=GeoGraphics[{}, 
	 GeoBackground -> {GeoStyling["StreetMap"]}, 
	 GeoRange -> {range1, range2}, GeoRangePadding -> .0025];
	line=latlongs;
	line=GeoGridPosition[GeoPosition[line], "Mercator"][[1]];
	
	With[{background=bg, animationline=line, times=Union[times0]},
		If[Length[times]<2,Throw[$Failed]];
		Animate[
			With[{pos=First[Flatten@Position[times,_?(#>t&),1,1]]},
				Show[{Graphics @@ background, 
					Graphics[{Red,PointSize[Large],Point[animationline[[pos]]],Blue,Line[animationline[[;;pos]]]}]}]
			]
			,
			{{t,times[[1]],"time"},times[[1]],times[[-2]]},opts]
	]
]

createMap[latlongs:{llpattern..},times:{_?NumberQ..},dists:{llpattern..},rest___]:=createMap[{latlongs}, {times},{dists},rest]
createMap[{},___]:={}
createMap[___]:=$Failed

getMapMarkers[type_, times0_, dists_, latlongs_, scale_, color_]:=Module[
	{times, t0, tf, delt, markers, ts, pos, dtimes, ds,labels, targetn=5, maxn=15, minn=1, range},
	Switch[type,
		"Time", 
			times=times0-times0[[1]];
			dtimes=dists[[All,1]];
			t0=dtimes[[1]];
			tf=dtimes[[-1]];
			range=tf-t0;
			delt=First@Nearest[{3600, 600, 60, 10, 1},range/targetn];
			delt=If[maxn>(range/delt)>minn,delt,range/targetn];
			ts=Range[t0,tf,delt];
			pos=Flatten@Table[Position[times,_?(#>=i&),1,1],{i,ts}];
			markers=latlongs[[pos]];
			labels=DateString[#,"Time"]&/@Take[ts,Length[markers]]
			,
		"Distance",
			times=times0-times0[[1]];
			range=dists[[-1,-1]];
			delt=First@Nearest[{5280, 1000, 100, 10, 1},range/targetn];
			delt=If[maxn>(range/delt)>minn,delt,range/targetn];
			ds=Range[0,dists[[-1,-1]],delt];
			pos=Flatten@Table[Position[dists[[All,-1]],_?(#>=i&),1,1],{i,ds}];
			ts=dists[[pos,1]];
			pos=Flatten@Table[Position[times,_?(#>=i&),1,1],{i,ts}];
			markers=latlongs[[pos]];
			labels=Quantity[#,"Feet"]&/@Take[ds,Length[markers]]
		,
		_,markers={};labels={}
	];
	

	MapThread[Tooltip[GeoMarker[#1,"Scale"->scale,"Color"->color],#2]&,{markers,labels}]
	
]


(***************** timelines ********************)
eventtimeline[data_, times_]:=DateListPlot[MapThread[Tooltip[{#,1},#2]&,{times,data}],Filling->Axis,FrameTicks -> {None, {Automatic, Automatic}},Joined->False]


(****************** ob/deob **********************)


getclientinfo[servicename_, type_:"OAuth"]:=Block[{OAuthClient`Private`deobflag = True, info},
	info=Switch[type,
		"OAuth",OAuthClient`OAuthServicesData[servicename,"ClientInfo"],
		"APIKey",KeyClient`KeyServicesData[servicename,"ClientInfo"]
	];
	(* If[!MatchQ[info,_String|{_String..}],Throw[$Failed]]; *)
	info
]

deobflag=False;
$count=1;
rand={19, 63, 112, 111, 75, 117, 1, 111, 51, 99, 8, 34, 67, 1, 73, 3, 35, 
87, 2, 51, 14, 82, 27, 92, 15, 16, 8, 101, 95, 61};

ob[l_]/;deobflag:=Block[{tf,tf2,res},
	tf = newfile[$TemporaryDirectory];
	Put[l, tf];
	tf2 = newfile[$TemporaryDirectory];
	Encode[tf, tf2, FromCharacterCode[rand,"UTF-8"]];
	DeleteFile[tf];
	res=Import[tf2,"String"];
	DeleteFile[tf2];
	ToCharacterCode[res,"UTF-8"]
	
]

deob[chars_]/;deobflag:=Block[{tf,res,string},
	tf = newfile[$TemporaryDirectory];
	string=FromCharacterCode[chars,"UTF-8"];
	Export[tf, string,"String"];
	res=Get[tf, FromCharacterCode[rand,"UTF-8"]];
	DeleteFile[tf];
	res
	
]

newfile[dir_]:=With[{file=FileNameJoin[{dir, "m-" <> ToString[RandomInteger[10000]] <> ".txt"}]},
	If[FileExistsQ[file],
		If[$count>100,Throw[$Failed]];$count++;newfile[dir],
		file
	]
]

ob[___]:=$Failed
deob[___]:=$Failed

(******************** paclet update ********************)

updateOAuthPaclet[]:=(
	Needs["PacletManager`"];
    Quiet[result = PacletUpdate["OAuth"]];
    If[Head[result] === Paclet,
        (* Update occurred. *)
        Message[ServiceConnect::pacupd];
        (* Load the code from the new version. Perhaps other "reset" steps are required? *)
        Get["OAuth`"],
    (* else *)
        (* No update occurred. *)
        Message[ServiceConnect::npacup]
    ])


SetAttributes[{ob,deob,getdata,prettygrid,filterParameters,getclientinfo,updateOAuthPaclet},{ReadProtected, Protected}];

End[]

End[] (* End Private Context *)

System`Private`RestoreContextPath[];
