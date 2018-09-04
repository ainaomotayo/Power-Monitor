
System`Private`NewContextPath[{"OAuthClient`","System`"}];

Begin["GoogleMapsAPI`"] (* Begin Private Context *) 

Begin["`Private`"](* Begin Private Context *) 

(******************************* GoogleMaps *************************************)

(* Authentication information *)

$googlemapsfetchfun=URLFetch;

googlemapsdata[]={
		"ServiceName" 		-> "GoogleMaps", 
        "URLFetchFun"		:> (ExportString[URLFetch[#1, "ContentData", ##2], "Byte"] &),
		"ClientInfo"			-> "WolframKey",
	 	"Gets"				-> {"Map","DirectionsMap"},
	 	"Posts"				-> {},
	 	"RawGets"			-> {"RawStaticMap","RawDirections"},
	 	"RawPosts"			-> {},
 		"Information"		-> "Create Google maps using the Wolfram Language"
}

(* a function for importing the raw data - usually json or xml - from the service *)
googlemapsimport[$Failed]:=Throw[$Failed]
googlemapsimport[json_String]:=With[{res=ImportString[json,"JSON"]},
	Switch[res,
			_Rule|{_Rule...},Association@res,
			{{_Rule...}...},Association/@res,
			_,res
		]
]/;StringFreeQ[json,"error"]

googlemapsimport[x_]:=x

googlemapsmapimport=ImportString

(****** Raw Properties ******)
(* information:
 Each entry includes the api endpoint, the HTTP method ("GET" or "POST") as well as the different types of parameters that are used
 "Parameters" - standard URL parameters that appear in the query string as ?param1=val1&param2=val2...
 "PathParameters" - parameters that are included in the url path scheme://domain/path1/`1`/`2`...  make the URL (ToString[StringForm[...,#1,#2]]&) 
 "BodyData" - parameters in HTTP Post requests that are includes as body data
 "MultipartData" - parameters in HTTP Post requests that are includes as multip part data, 
 		usually large files or images are multipart data, each parameter should be given as {"parametername","datatype"} 
 "RequiredParameters" - all above parameters are assumed to be optional, list required parameters a second time here
 "Headers" - additional headers to be included in the HTTP request
 "RequiredPermissions" - If we support incrementally adding permission for a service, list the required permissions for the request here*)
 
(*** Raw ***)

(* Static Maps *)
googlemapsdata["RawStaticMap"] = {
        "URL"				-> "https://maps.googleapis.com/maps/api/staticmap",
        "HTTPSMethod"		-> "GET",
        "Parameters"		-> {"center","zoom","size","visual_refresh","scale",
        	"format","maptype","language","region","markers","path","visible","style","sensor"},
        "RequiredParameters"-> {"center"|"markers"|"path","zoom"|"markers"|"path","size","sensor"},
        "IncludeAuth"		-> False,
        "ResultsFunction"	-> googlemapsmapimport
    }   

googlemapsdata["RawDirections"] = {
        "URL"				-> "https://maps.googleapis.com/maps/api/directions/json",
        "HTTPSMethod"		-> "GET",
        "Parameters"		-> {"origin","destination","mode","waypoints","alternatives","avoid","language","units","region","departure_time","arrival_time","sensor"},
        "RequiredParameters"-> {"origin","destination","sensor"},
        "IncludeAuth"		-> False,
        "ResultsFunction"	-> googlemapsimport
    }   
    
googlemapsdata[___]:=$Failed
(****** Cooked Properties ******)
  
(* cooked data queries 
	Cooked queries call at least one of the raw queried underneath but add pre and post processing to make the function easier to use and the result clearer.
*)  

googlemapscookeddata[prop_,id_,rule_Rule, rest___]:=googlemapscookeddata[prop,id,{rule}, rest]
googlemapscookeddata[prop_,id_]:=googlemapscookeddata[prop,id,{}]

googlemapscookeddata["Map",id_,args_]:=Block[
	{params,map, root,data,$googlemapsfetchfun,rawdata},
	$googlemapsfetchfun=(URLFetch[#1,{"ContentData"},##]&);
	params=filterparameters[Join[args/."Location"->"center",
		{"sensor"->"false"}],getallparameters["RawStaticMap"]];
	params=fixmarkers[params];
	params=adddefaults[params];
	params=formatposition[params]/.{
		HoldPattern[Rule["zoom",z_]]:>Rule["zoom",ToString[z]]};
	rawdata=KeyClient`rawkeydata[id,"RawStaticMap",params];
	map=googlemapsmapimport[rawdata];
	If[Head[map]===Image,
		map,
		$Failed
	]
]


googlemapscookeddata["DirectionsMap",id_,args_]:=Block[
	{params,map, root,data,$googlemapsfetchfun,rawdata,rawdir,polyline},
	$googlemapsfetchfun=(URLFetch[#1,{"ContentData"},##]&);
	params=filterparameters[Join[args,{"sensor"->"false","origin"->Here}],getallparameters["RawDirections"]];
	params=formatposition[params]/.{
		HoldPattern[Rule["departure_time"|"arrival_time",date_]]:>Rule["zoom",formattime[date]]};
	rawdata=KeyClient`rawkeydata[id,"RawDirections",params];
	data=googlemapsimport[rawdata];
	
	rawdir=First[Lookup[data, "routes", {{}}]];
	polyline="points" /. ("overview_polyline" /. rawdir /. "overview_polyline" -> {});
	
	If[polyline==="points"||!StringQ[polyline],Throw[$Failed]];
	
	map=googlemapscookeddata["Map",id,Join[{"path"->("enc:"<>polyline)},args]]
	
]

adddefaults[params_]:=Flatten[{
	If[FreeQ[params,"markers"|"path"|"center"],{"center"->Here},{}],
	If[FreeQ[params,"markers"|"path"]&&FreeQ[params,"zoom"],{"zoom"->"10"},{}],
	If[FreeQ[params,"size"],{"size"->"500x500"},{}],
	params
}]

googlemapscookeddata[___]:=$Failed 
(* Send Message *)

googlemapssendmessage[___]:=$Failed

(*** Service specific utilites ****)
filterparameters=OAuthClient`Private`filterParameters;
camelcase=OAuthClient`Private`camelCase;

formattime[time_]:=Round@First[DateDifference[DateList[{1970, 1, 1}], time, "Second"]]

getallparameters[str_]:=DeleteCases[Flatten[{"Parameters","PathParameters","BodyData","MultipartData"}/.googlemapsdata[str]],
	("Parameters"|"PathParameters"|"BodyData"|"MultipartData")]

fixmarkers[params_]:=params/.HoldPattern[Rule["markers",m_]]:>fixmarkers0[formatposition[m]]
fixmarkers0[{}|""|None]:=Sequence[]
fixmarkers0[l:{_String..}]:="markers"->StringJoin[Riffle[DeleteCases[l,""],"|"]]
fixmarkers0[str_String]:="markers"->str
fixmarkers0[expr_]:=(Message[ServiceExecute::geon,expr];fixmarkers0[{}])

formatposition[x_]:=(x/.en_Entity:>EntityValue[en,"Coordinates"])/.GeoPosition[{lat_,long_,___},___]:>(ToString[lat]<>","<>ToString[long])

entitytogeo[en_]:=Quiet[
	With[{geo=EntityValue[en,"Coordinates"]},
		If[Head[geo]===GeoPosition,
			geo,
			Message[ServiceExecute::geoent,en];""
		]
	]
]
End[] (* End Private Context *)
           		
End[]


SetAttributes[{},{ReadProtected, Protected}];

System`Private`RestoreContextPath[];

(* Return three functions to define oauthservicedata, oauthcookeddata, oauthsendmessage  *)
{GoogleMapsAPI`Private`googlemapsdata,GoogleMapsAPI`Private`googlemapscookeddata,GoogleMapsAPI`Private`googlemapssendmessage}
