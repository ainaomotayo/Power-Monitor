
System`Private`NewContextPath[{"OAuthClient`","System`"}];

Begin["GooglePlusOAuth`"] (* Begin Private Context *) 

Begin["`Private`"](* Begin Private Context *) 

(******************************* GooglePlus *************************************)

(* Authentication information *)

googleplusdata[]={
		"OAuthVersion"		->"2.0",
		"ServiceName"       -> "Google+",
	    "AuthorizeEndpoint" -> "https://accounts.google.com/o/oauth2/auth",
	    "AccessEndpoint"    -> "https://accounts.google.com/o/oauth2/token",
	    "RedirectURI"       -> "urn:ietf:wg:oauth:2.0:oob",
	    "VerifierLabel"      -> "code",
	 	"ClientInfo"		-> {"Wolfram","Token"},
	 	"AuthenticationDialog" :> (OAuthClient`tokenOAuthDialog[#, "Google+",gpicon]&),
	 	"Gets"				-> {"UserData","ActivityList","UserSearch","ActivitySearch","ActivityData",
	 		"ActivityPlusOners","ActivityResharers","UserPosts","UserPostsTimeline","UserPostsEventSeries","CircledUsers"},
	 	"Posts"				-> {},
	 	"RawGets"			-> {"RawUserData","RawPeopleSearch","RawPeopleByActivity","RawPeopleByCollection",
	 			"RawActivity","RawActivitySearch","RawComment","RawActivityComments","RawUserMoments","RawUserActivities"},
	 	"RawPosts"			-> {"RawInsertMoment"},
	 	"Scope"				-> {"https%3A%2F%2Fwww.googleapis.com%2Fauth%2Fplus.login&request_visible_actions=https%3A%2F%2Fschemas.google.com%2FAddActivity+https%3A%2F%2Fschemas.google.com%2FReviewActivity+http%3A%2F%2Fschemas.google.com%2FCheckInActivity"},
 		"Information"		-> "A service for receiving data from a Google+ account"
}

(* a function for importing the raw data - usually json or xml - from the service *)
googleplusimport[$Failed]:=Throw[$Failed]
googleplusimport[json_String]:=With[{res=ImportString[json,"JSON"]},
	If[FreeQ[res,_["error",_]],
		Association@res,
		Message[ServiceExecute::apierr,"message"/.("error"/.res)];
		Throw[$Failed]
	]
]


googleplusimport[raw_]:=raw



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

(** People **)
googleplusdata["RawUserData"] = {
        "URL"				-> (ToString@StringForm["https://www.googleapis.com/plus/v1/people/`1`",formatuserid[##]]&),
        "PathParameters"	-> {"userID"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> googleplusimport
    }

googleplusdata["RawPeopleSearch"] = {
        "URL"				-> "https://www.googleapis.com/plus/v1/people",
        "Parameters"		-> {"query","language","maxResults","pageToken"},
        "RequiredParameters"-> {"query"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> googleplusimport
    }

googleplusdata["RawPeopleByActivity"] = {
        "URL"				-> (ToString@StringForm["https://www.googleapis.com/plus/v1/activities/`1`/people/`2`",ToString[#1],tostring[##2,"plusoners"]]&),
        "PathParameters"	-> {"activityID","collection"},
        "Parameters"		-> {"maxResults","pageToken"},
        "RequiredParameters"-> {"activityID"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> googleplusimport
    }

googleplusdata["RawPeopleByCollection"] = {
        "URL"				-> (ToString@StringForm["https://www.googleapis.com/plus/v1/people/`1`/people/`2`",fp[{formatuserid,tostring[##,"visible"]&},##]]&),
        "PathParameters"	-> {"userID","collection"},
        "Parameters"		-> {"maxResults","orderBy","pageToken"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> googleplusimport
    }
    
(** Activities **)
googleplusdata["RawActivity"] = {
        "URL"				-> (ToString@StringForm["https://www.googleapis.com/plus/v1/activities/`1`",ToString[#]]&),
        "PathParameters"	-> {"activityID"},
        "RequiredParameters"-> {"activityID"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> googleplusimport
    }

googleplusdata["RawActivitySearch"] = {
        "URL"				-> "https://www.googleapis.com/plus/v1/activities",
        "Parameters"		-> {"query","language","maxResults","orderBy","pageToken"},
        "RequiredParameters"-> {"query"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> googleplusimport
    }

googleplusdata["RawUserActivities"] = {
        "URL"				-> (ToString@StringForm["https://www.googleapis.com/plus/v1/people/`1`/activities/`2`",fp[{formatuserid,tostring[##,"public"]&},##]]&),
        "PathParameters"	-> {"userID","collection"},
        "Parameters"		-> {"maxResults","pageToken"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> googleplusimport
    }    
    
(** Comments **)
googleplusdata["RawComment"] = {
        "URL"				-> (ToString@StringForm["https://www.googleapis.com/plus/v1/comments/`1`",ToString[#]]&),
        "PathParameters"	-> {"commentID"},
        "RequiredParameters"-> {"commentID"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> googleplusimport
    }
    
googleplusdata["RawActivityComments"] = {
        "URL"				-> (ToString@StringForm["https://www.googleapis.com/plus/v1/activities/`1`/comments",ToString[#]]&),
        "PathParameters"	-> {"activityID"},
        "RequiredParameters"-> {"activityID"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> googleplusimport
    }
(** Moments **)
googleplusdata["RawUserMoments"] = {
        "URL"				-> (ToString@StringForm["https://www.googleapis.com/plus/v1/people/`1`/moments/`2`",fp[{formatuserid,tostring[##,"vault"]&},##]]&),
        "PathParameters"	-> {"userID","collection"},
        "Parameters"		-> {"maxResults","targetUrl","pageToken","type"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> googleplusimport
    }
    
googleplusdata["RawInsertMoment"] = {
        "URL"				-> (ToString@StringForm["https://www.googleapis.com/plus/v1/people/`1`/moments/`2`",fp[{formatuserid,tostring[##,"vault"]&},##]]&),
        "PathParameters"	-> {"userID","collection"},
        "Parameters"		-> {"debug"},
        "BodyData"			-> {"ParameterlessBodyData"},
        "RequiredParameters"-> {"ParameterlessBodyData"},
        "HTTPSMethod"		-> "POST",
      	"Headers" 			-> {"Content-Type" -> "application/json"},
        "ResultsFunction"	-> googleplusimport
    }

$GooglePlusRefreshAPI="https://www.wolframcloud.com/objects/user-fa95220f-871c-4331-84ab-7951dd0666ca/googleplusrefresh";
googleplusdata["RefreshAccessTokenFunction"]:=(
	ToExpression[
		URLFetch[$GooglePlusRefreshAPI, 
  			"Parameters" -> {"refreshtoken" -> ToString[#,InputForm], 
    		"AccessEndpoint" -> ("AccessEndpoint"/.googleplusdata[])},
   			"VerifyPeer" -> False]]&)/;OAuthClient`Private`$OAuthCloudCredentialsQ
   			
googleplusdata["RefreshAccessTokenFunction"]:=(Block[{url,info,res, data,key, time},
	If[#===None,Return[$Failed]];
	info=OAuthClient`Private`getclientinfo["GooglePlus"];
	url={"AccessEndpoint"/.googleplusdata[],
		"BodyData"->URLQueryEncode[{"refresh_token"->#[[1]],
		 "client_id"->info[[1]] ,
		 "client_secret"->info[[2]],
		 "grant_type"->"refresh_token"
		}],
		"Method"->"POST"
	};
	res=URLFetch@@url;
	If[StringQ[res],
		data=ImportString[res,"JSON"];
		If[MatchQ[data, _?OptionQ],
			key="access_token"/.data;
			If[StringQ[key]&&key=!="access_token",
				time=ToExpression["expires_in"/.data]+AbsoluteTime[];
				{key,time},
				$Failed
			],
			$Failed
		],
		$Failed
	]
]&)

googleplusdata["icon"]=gpicon;
 
googleplusdata[___]:=$Failed
(****** Cooked Properties ******)
  
(* cooked data queries 
	Cooked queries call at least one of the raw queried underneath but add pre and post processing to make the function easier to use and the result clearer.
*)  

googlepluscookeddata[prop_,id_,rule_Rule, rest___]:=googlepluscookeddata[prop,id,{rule}, rest]
googlepluscookeddata[prop_,id_]:=googlepluscookeddata[prop,id,{}]
  

(* Cooked *)
googlepluscookeddata["UserData",id_,args_]:=Module[{params,rawdata, data},
	params=filterparameters[args,getallparameters["RawUserData"]];
	rawdata=OAuthClient`rawoauthdata[id,"RawUserData",params];
	data=googleplusimport[rawdata];
	Association[Replace[Normal[data],(Rule[a_,b_]):>(Rule[camelcase[a],b]),Infinity]]
]

$googleplusactivitysearchlimit=20;
$googleplususersearchlimit=50;

googlepluscookeddata["UserSearch",id_,args_]:=Module[{params,rawdata, data,limit},
	params=filterparameters[args,getallparameters["RawPeopleSearch"]]/.fixlimit;
	If[FreeQ[params,"query"],Message[ServiceExecute::nparam,"query"];Throw[$Failed]];
	limit=("maxResults"/.params)/."maxResults"->$googleplususersearchlimit;
	If[ToExpression[limit]>$googleplususersearchlimit,
		rawdata=pagethrough[OAuthClient`rawoauthdata,{id,"RawPeopleSearch",
			Join[FilterRules[params,Except["maxResults"]],{"maxResults"->ToString[$googleplususersearchlimit]}]},
			{ToExpression[limit],$googleplususersearchlimit}];
		data=Join@@(Lookup[googleplusimport[#],"items",{}]&/@rawdata);
		,
		rawdata=OAuthClient`rawoauthdata[id,"RawPeopleSearch",params];
		data=googleplusimport[rawdata];
		data=Lookup[data,"items",{}];
	];
	parseuser/@data
]

googlepluscookeddata["ActivitySearch",id_,args_]:=Module[{params,rawdata, data,limit},
	params=filterparameters[args,getallparameters["RawActivitySearch"]]/.fixlimit;
	If[FreeQ[params,"query"],Message[ServiceExecute::nparam,"query"];Throw[$Failed]];
	limit=("maxResults"/.params)/."maxResults"->$googleplusactivitysearchlimit;
	If[ToExpression[limit]>$googleplusactivitysearchlimit,
		rawdata=pagethrough[OAuthClient`rawoauthdata,{id,"RawActivitySearch",
			Join[FilterRules[params,Except["maxResults"]],{"maxResults"->ToString[$googleplusactivitysearchlimit]}]},
			{ToExpression[limit],$googleplususersearchlimit}];
		data=Join@@(Lookup[googleplusimport[#],"items",{}]&/@rawdata);
		,
		rawdata=OAuthClient`rawoauthdata[id,"RawActivitySearch",params];
		data=googleplusimport[rawdata];
		data=Lookup[data,"items",{}];
	];
	parseactivity/@data
]

(*
googlepluscookeddata["ActivitySearch",id_,args_]:=Module[{params,rawdata, data},
	params=filterparameters[args,getallparameters["RawActivitySearch"]]/.fixlimit;
	If[FreeQ[params,"query"],Message[ServiceExecute::nparam,"query"];Throw[$Failed]];
	rawdata=OAuthClient`rawoauthdata[id,"RawActivitySearch",params];
	data=googleplusimport[rawdata];
	data=Lookup[data,"items",{}];
	parseactivity/@data
]
*)

googlepluscookeddata["ActivityData",id_,args_]:=Module[{params,rawdata, data},
	params=filterparameters[args,getallparameters["RawActivity"]];
	If[FreeQ[params,"activityID"],Message[ServiceExecute::nparam,"ActivityID"];Throw[$Failed]];
	rawdata=OAuthClient`rawoauthdata[id,"RawActivity",params];
	data=googleplusimport[rawdata];
	parseactivity[Normal[data]]
]

googlepluscookeddata["UserPosts",id_,args_]:=Module[{types,pos=All,rawdata, data},
	rawdata=googlepluscookeddata["ActivityList",id,args];
	(* types="ObjectType"/.(Lookup[#,"Object",{}]&/@rawdata);
	pos=Flatten[Position[types,"note"]]; *)
	data=Lookup[#,"Object",{}]&/@rawdata[[pos]];
	fromHTML[("Content"/.data)/."Content":>Sequence@@{}]
]

googlepluscookeddata["UserPostsEventSeries",id_,args_]:=Module[{rawdata, data, times},
	rawdata=googlepluscookeddata["ActivityList",id,args];
	times=Lookup[#,"Published",{}]&/@rawdata;
	data=Lookup[#,"Object",{}]&/@rawdata;
	If[data=!={},
		data=fromHTML[("Content"/.data)/."Content":>""];
		EventSeries[MapThread[{#1,#2}&,{times,data}]],
		Missing[]
	]
]

googlepluscookeddata["UserPostsTimeline",id_,args_]:=Module[{rawdata, data, times},
	rawdata=googlepluscookeddata["ActivityList",id,args];
	times=Lookup[#,"Published",{}]&/@rawdata;
	data=Lookup[#,"Object",{}]&/@rawdata;
	If[data=!={},
		data=fromHTML[("Content"/.data)/."Content":>""]
	];
	OAuthClient`Private`eventtimeline[data, times]
]

$googleplusactivitylistlimit=100;

googlepluscookeddata["ActivityList",id_,args_]:=Module[{limit,params,rawdata, data},
	params=filterparameters[args,getallparameters["RawUserActivities"]]/.fixlimit;
	limit=("maxResults"/.params)/."maxResults"->$googleplusactivitylistlimit;
	If[ToExpression[limit]>$googleplusactivitysearchlimit,
		rawdata=pagethrough[OAuthClient`rawoauthdata,{id,"RawUserActivities",
			Join[FilterRules[params,Except["maxResults"]],{"maxResults"->ToString[$googleplusactivitylistlimit]}]},
			{ToExpression[limit],$googleplusactivitylistlimit}];
		data=Join@@(Lookup[googleplusimport[#],"items",{}]&/@rawdata);
		,
		rawdata=OAuthClient`rawoauthdata[id,"RawUserActivities",params];
		data=googleplusimport[rawdata];
		data=Lookup[data,"items",{}];
	];	
	parseactivity/@data
]

googlepluscookeddata[prop:("ActivityPlusOners"|"ActivityResharers"),id_,args_]:=Module[{limit,params,rawdata, data, col},
	params=filterparameters[args,getallparameters["RawPeopleByActivity"]]/.fixlimit;
	col=If[prop==="ActivityPlusOners","plusoners","resharers"];
	limit=("maxResults"/.params)/."maxResults"->$googleplusactivitylistlimit;
	If[ToExpression[limit]>$googleplusactivitysearchlimit,
		rawdata=pagethrough[OAuthClient`rawoauthdata,{id,"RawPeopleByActivity",
			Join[{"collection"->col},FilterRules[params,Except["maxResults"]],{"maxResults"->ToString[$googleplusactivitylistlimit]}]},
			{ToExpression[limit],$googleplusactivitylistlimit}];
		data=Join@@(Lookup[googleplusimport[#],"items",{}]&/@rawdata);
		,
		rawdata=OAuthClient`rawoauthdata[id,"RawPeopleByActivity",Join[{"collection"->col},params]];
		data=googleplusimport[rawdata];
		data=Lookup[data,"items",{}];
	];	
	parseuser/@data
]

$defaultmaxuser=100;

googlepluscookeddata["CircledUsers",id_,args_]:=Module[{params,rawdata, data,limit},
	params=filterparameters[args,getallparameters["RawPeopleByCollection"]]/.fixlimit;
	limit=("maxResults"/.params)/."maxResults"->$defaultmaxuser;
	If[ToExpression[limit]>$defaultmaxuser,
		rawdata=pagethrough[OAuthClient`rawoauthdata,{id,"RawPeopleByCollection",
			Join[FilterRules[params,Except["maxResults"]],{"maxResults"->ToString[$defaultmaxuser]}]},
			{limit,$defaultmaxuser}];
		data=Join@@(Lookup[googleplusimport[#],"items",{}]&/@rawdata);
		,
		rawdata=OAuthClient`rawoauthdata[id,"RawPeopleByCollection",params];
		data=googleplusimport[rawdata];
		data=Lookup[data,"items",{}];
	];
	parseuser/@data
]


pagethrough[fun_, args_, {n_, n0_}]:=Block[{i,token, res},
	res={fun@@args};
	token=getpagetoken[res[[1]]];
	If[token=!=$Failed,
		Do[
			res=Join[res,{fun@@Join[args,{"pageToken"->token}]}];
			token=getpagetoken[Last[res]];
			If[token===$Failed,Return[res]],
			{i,Ceiling[n/n0]-1}
		];
		,Return[res]
	];
	res/;res=!=$Failed
]

getpagetoken[json_]:=With[
	{tokens=StringCases[json, "\"nextPageToken\": \"" ~~ (t : Shortest[__]) ~~ "\"" :> t]},
	If[Length[tokens]===1,First[tokens],$Failed]
]
(* Send Message *)

googleplussendmessage[___]:=$Failed

(*** Utilities ***)
filterparameters=OAuthClient`Private`filterParameters;
camelcase=OAuthClient`Private`camelCase;
fp=OAuthClient`Private`formatpath;

fixlimit=HoldPattern[Rule["maxResults",l_]]:>Rule["maxResults",ToString[l]];

parseactivity[act_]:=Association@Replace[FilterRules[Replace[act
		,{(Rule[a_,b_]):>(Rule[camelcase[a],b])},Infinity],{"Actor","URL","Updated","Object","Published","ID"}]/.{
			OAuthClient`Private`formatvalue["Updated"->(readDate[#]&)],OAuthClient`Private`formatvalue["Published"->(readDate[#]&)]},"ID"->"ActivityID",{2}]
			
parseuser[user_]:=Association@Replace[FilterRules[Replace[user,
	{(Rule[a_,b_]):>(Rule[camelcase[a],b])},Infinity],{"DisplayName","ID"}]/.{OAuthClient`Private`formatvalue["Published"->(readDate[#]&)]},"ID"->"UserID",{2}]
			
formatuserid[]:="me"
formatuserid[Automatic]:="me"
formatuserid[id_]:=ToString[id]

tostring[str_String,_]:=str
tostring[default_]:=default
tostring[Automatic,default_]:=default
tostring[str_,_]:=ToString[str]

readDate[date_,form_:DateObject]:=form[DateList[date]]

getallparameters[str_]:=DeleteCases[Flatten[{"Parameters","PathParameters","BodyData","MultipartData"}/.googleplusdata[str]],
	("Parameters"|"PathParameters"|"BodyData"|"MultipartData")]

(*
fromHTML[str_]:=StringReplace[str, ("&#" ~~ (char : (WordCharacter ..) /; 
      StringLength[char] < 5) ~~ ";") :> (FromCharacterCode@
    FromDigits[char])]
    *)

fromHTML[str_String]:=ImportString[str,"HTML"]   
fromHTML[l:{___String}]:=ImportString[#,"HTML"]  &/@l  
fromHTML[___]:={}
    
gpicon=Image[RawArray["Byte", {{{255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 
  255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 
  255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, 
  {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 
  0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 
  255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 
  255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, 
  {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}}, {{255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 
  255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 
  255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, 
  {255, 255, 255, 0}, {255, 255, 255, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 
  0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 
  0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, 
  {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 
  0}}, {{255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 
  255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, 
  {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {0, 0, 0, 0}, 
  {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 
  0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {255, 255, 255, 0}, 
  {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 
  0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}}, {{255, 255, 255, 0}, {255, 255, 255, 0}, {255, 
  255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, 
  {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 
  0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, 
  {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 
  0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 
  0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 
  255, 0}}, {{255, 255, 255, 0}, {255, 255, 255, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 19}, {0, 0, 
  0, 34}, {0, 0, 0, 38}, {0, 0, 0, 38}, {0, 0, 0, 38}, {0, 0, 0, 38}, {0, 0, 0, 38}, {0, 0, 0, 38}, {0, 0, 0, 38}, 
  {0, 0, 0, 38}, {0, 0, 0, 38}, {0, 0, 0, 38}, {0, 0, 0, 38}, {0, 0, 0, 38}, {0, 0, 0, 38}, {0, 0, 0, 38}, {0, 0, 0, 
  38}, {0, 0, 0, 38}, {0, 0, 0, 38}, {0, 0, 0, 38}, {0, 0, 0, 38}, {0, 0, 0, 38}, {0, 0, 0, 38}, {0, 0, 0, 38}, {0, 
  0, 0, 38}, {0, 0, 0, 38}, {0, 0, 0, 38}, {0, 0, 0, 38}, {0, 0, 0, 38}, {0, 0, 0, 38}, {0, 0, 0, 34}, {0, 0, 0, 19}, 
  {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {255, 255, 255, 0}}, {{255, 255, 255, 0}, {255, 255, 255, 
  0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 19}, {186, 186, 186, 100}, {251, 251, 251, 234}, {255, 255, 255, 255}, 
  {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 
  255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 
  255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 
  255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, 
  {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 
  255, 255, 255}, {251, 251, 251, 234}, {186, 186, 186, 100}, {0, 0, 0, 19}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 
  0}, {255, 255, 255, 0}}, {{255, 255, 255, 0}, {255, 255, 255, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 34}, {251, 
  251, 251, 234}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 
  255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 
  255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, 
  {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 
  255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 
  255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {251, 251, 251, 
  234}, {0, 0, 0, 34}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {255, 255, 255, 0}}, {{255, 255, 255, 0}, {255, 255, 
  255, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 38}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 
  255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, 
  {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 
  255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 
  255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 
  255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, 
  {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {0, 0, 0, 38}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 
  0, 0}, {255, 255, 255, 0}}, {{255, 255, 255, 0}, {255, 255, 255, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 38}, 
  {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 
  255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 
  255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 
  255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, 
  {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 
  255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 
  255, 255}, {0, 0, 0, 38}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {255, 255, 255, 0}}, {{255, 255, 255, 0}, {255, 
  255, 255, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 38}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 
  255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 
  255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, 
  {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 
  255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 
  255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 
  255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {0, 0, 0, 38}, {0, 0, 0, 0}, {0, 0, 0, 0}, 
  {0, 0, 0, 0}, {255, 255, 255, 0}}, {{255, 255, 255, 0}, {255, 255, 255, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 
  38}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, 
  {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {245, 201, 196, 255}, {230, 
  121, 110, 255}, {225, 93, 78, 255}, {222, 80, 63, 255}, {221, 75, 57, 255}, {221, 75, 57, 255}, {221, 75, 57, 255}, 
  {221, 75, 57, 255}, {223, 85, 68, 255}, {239, 171, 163, 255}, {255, 253, 253, 255}, {255, 255, 255, 255}, {255, 
  255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 
  255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 
  255}, {0, 0, 0, 38}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {255, 255, 255, 0}}, {{255, 255, 255, 0}, {255, 255, 
  255, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 38}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 
  255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 253, 253, 255}, 
  {233, 135, 124, 255}, {224, 91, 76, 255}, {245, 202, 198, 255}, {255, 253, 253, 255}, {246, 205, 201, 255}, {225, 
  96, 81, 255}, {221, 75, 57, 255}, {222, 82, 66, 255}, {245, 199, 193, 255}, {254, 249, 249, 255}, {255, 255, 255, 
  255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, 
  {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 
  255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {0, 0, 0, 38}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 
  0}, {255, 255, 255, 0}}, {{255, 255, 255, 0}, {255, 255, 255, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 38}, {255, 
  255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 
  255, 255}, {255, 255, 255, 255}, {234, 143, 133, 255}, {221, 75, 57, 255}, {245, 197, 192, 255}, {255, 255, 255, 
  255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {251, 229, 227, 255}, {222, 81, 64, 255}, {221, 75, 57, 255}, 
  {224, 92, 77, 255}, {255, 253, 253, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 
  255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 
  255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 
  255}, {0, 0, 0, 38}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {255, 255, 255, 0}}, {{255, 255, 255, 0}, {255, 255, 
  255, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 38}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 
  255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {250, 224, 221, 255}, {221, 77, 59, 255}, 
  {222, 79, 62, 255}, {255, 253, 253, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 
  255, 255, 255}, {232, 131, 120, 255}, {221, 75, 57, 255}, {221, 75, 57, 255}, {241, 182, 175, 255}, {255, 255, 255, 
  255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, 
  {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 
  255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {0, 0, 0, 38}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 
  0}, {255, 255, 255, 0}}, {{255, 255, 255, 0}, {255, 255, 255, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 38}, {255, 
  255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 
  255, 255}, {237, 156, 147, 255}, {221, 75, 57, 255}, {222, 79, 63, 255}, {255, 255, 255, 255}, {255, 255, 255, 
  255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {245, 202, 198, 255}, {221, 75, 57, 255}, 
  {221, 75, 57, 255}, {235, 144, 134, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 
  255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 
  255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 
  255}, {0, 0, 0, 38}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {255, 255, 255, 0}}, {{255, 255, 255, 0}, {255, 255, 
  255, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 38}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 
  255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {233, 138, 127, 255}, {221, 75, 57, 255}, 
  {221, 75, 57, 255}, {250, 224, 221, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 
  255, 255, 255}, {252, 243, 242, 255}, {221, 75, 57, 255}, {221, 75, 57, 255}, {236, 153, 143, 255}, {255, 255, 255, 
  255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, 
  {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 
  255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {0, 0, 0, 38}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 
  0}, {255, 255, 255, 0}}, {{255, 255, 255, 0}, {255, 255, 255, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 38}, {255, 
  255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 
  255, 255}, {238, 162, 153, 255}, {221, 75, 57, 255}, {221, 75, 57, 255}, {235, 143, 133, 255}, {255, 255, 255, 
  255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {222, 78, 61, 255}, 
  {221, 75, 57, 255}, {244, 195, 191, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 
  255, 255, 255}, {255, 255, 255, 255}, {236, 151, 141, 255}, {225, 95, 80, 255}, {255, 255, 255, 255}, {255, 255, 
  255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 
  255}, {0, 0, 0, 38}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {255, 255, 255, 0}}, {{255, 255, 255, 0}, {255, 255, 
  255, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 38}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 
  255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {251, 235, 233, 255}, {222, 80, 64, 255}, 
  {221, 75, 57, 255}, {223, 84, 67, 255}, {252, 237, 235, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 
  255, 255, 255}, {250, 232, 229, 255}, {221, 75, 57, 255}, {227, 105, 92, 255}, {255, 255, 255, 255}, {255, 255, 
  255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {233, 138, 127, 
  255}, {222, 81, 64, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, 
  {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {0, 0, 0, 38}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 
  0, 0}, {255, 255, 255, 0}}, {{255, 255, 255, 0}, {255, 255, 255, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 38}, 
  {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 
  255, 255, 255}, {255, 255, 255, 255}, {241, 182, 175, 255}, {221, 77, 59, 255}, {221, 75, 57, 255}, {226, 101, 87, 
  255}, {251, 229, 226, 255}, {255, 255, 255, 255}, {253, 245, 244, 255}, {230, 118, 106, 255}, {226, 97, 83, 255}, 
  {251, 235, 233, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 
  255, 255, 255}, {255, 255, 255, 255}, {233, 138, 127, 255}, {222, 81, 64, 255}, {255, 255, 255, 255}, {255, 255, 
  255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 
  255}, {0, 0, 0, 38}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {255, 255, 255, 0}}, {{255, 255, 255, 0}, {255, 255, 
  255, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 38}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 
  255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, 
  {245, 202, 198, 255}, {228, 111, 98, 255}, {222, 83, 66, 255}, {222, 79, 62, 255}, {225, 93, 78, 255}, {222, 78, 
  61, 255}, {224, 88, 73, 255}, {252, 239, 238, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 
  255}, {255, 255, 255, 255}, {234, 141, 131, 255}, {224, 88, 73, 255}, {224, 88, 73, 255}, {222, 81, 64, 255}, {221, 
  76, 58, 255}, {224, 88, 73, 255}, {224, 88, 73, 255}, {224, 88, 73, 255}, {255, 255, 255, 255}, {255, 255, 255, 
  255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {0, 0, 0, 38}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {255, 
  255, 255, 0}}, {{255, 255, 255, 0}, {255, 255, 255, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 38}, {255, 255, 255, 
  255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, 
  {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {254, 
  251, 250, 255}, {222, 79, 62, 255}, {221, 75, 57, 255}, {231, 128, 118, 255}, {255, 255, 255, 255}, {255, 255, 255, 
  255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {239, 171, 164, 255}, {231, 126, 115, 255}, 
  {231, 126, 115, 255}, {225, 95, 80, 255}, {221, 77, 59, 255}, {231, 126, 115, 255}, {231, 126, 115, 255}, {231, 
  126, 115, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {0, 0, 0, 
  38}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {255, 255, 255, 0}}, {{255, 255, 255, 0}, {255, 255, 255, 0}, {0, 0, 
  0, 0}, {0, 0, 0, 0}, {0, 0, 0, 38}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 
  255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 
  255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {253, 245, 244, 255}, {221, 76, 60, 255}, {221, 75, 57, 255}, 
  {222, 82, 66, 255}, {248, 214, 210, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 
  255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {233, 138, 127, 255}, {222, 81, 
  64, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 
  255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {0, 0, 0, 38}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {255, 
  255, 255, 0}}, {{255, 255, 255, 0}, {255, 255, 255, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 38}, {255, 255, 255, 
  255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, 
  {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 
  255, 255, 255}, {231, 128, 117, 255}, {221, 75, 57, 255}, {221, 75, 57, 255}, {222, 81, 64, 255}, {242, 188, 182, 
  255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, 
  {255, 255, 255, 255}, {233, 138, 127, 255}, {222, 81, 64, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 
  255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {0, 0, 0, 
  38}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {255, 255, 255, 0}}, {{255, 255, 255, 0}, {255, 255, 255, 0}, {0, 0, 
  0, 0}, {0, 0, 0, 0}, {0, 0, 0, 38}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 
  255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {245, 201, 196, 
  255}, {233, 135, 124, 255}, {227, 105, 92, 255}, {226, 100, 85, 255}, {226, 100, 85, 255}, {223, 84, 67, 255}, 
  {221, 75, 57, 255}, {221, 75, 57, 255}, {221, 77, 59, 255}, {241, 182, 175, 255}, {255, 255, 255, 255}, {255, 255, 
  255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {239, 171, 164, 255}, {230, 121, 109, 
  255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, 
  {255, 255, 255, 255}, {255, 255, 255, 255}, {0, 0, 0, 38}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {255, 255, 
  255, 0}}, {{255, 255, 255, 0}, {255, 255, 255, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 38}, {255, 255, 255, 255}, 
  {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {248, 
  218, 214, 255}, {226, 103, 90, 255}, {222, 81, 64, 255}, {236, 149, 139, 255}, {248, 214, 210, 255}, {255, 253, 
  253, 255}, {255, 255, 255, 255}, {253, 247, 247, 255}, {233, 133, 123, 255}, {221, 75, 57, 255}, {221, 75, 57, 
  255}, {222, 79, 63, 255}, {250, 232, 229, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, 
  {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 
  255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {0, 0, 0, 
  38}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {255, 255, 255, 0}}, {{255, 255, 255, 0}, {255, 255, 255, 0}, {0, 0, 
  0, 0}, {0, 0, 0, 0}, {0, 0, 0, 38}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 
  255, 255}, {255, 255, 255, 255}, {248, 215, 211, 255}, {222, 81, 64, 255}, {222, 79, 63, 255}, {248, 217, 213, 
  255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, 
  {255, 255, 255, 255}, {239, 166, 158, 255}, {221, 76, 58, 255}, {221, 75, 57, 255}, {239, 165, 157, 255}, {255, 
  255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 
  255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 
  255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {0, 0, 0, 38}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {255, 
  255, 255, 0}}, {{255, 255, 255, 0}, {255, 255, 255, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 38}, {255, 255, 255, 
  255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {229, 114, 102, 255}, 
  {221, 75, 57, 255}, {231, 126, 115, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 
  255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {231, 123, 
  111, 255}, {221, 75, 57, 255}, {235, 143, 133, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 
  255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, 
  {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {0, 
  0, 0, 38}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {255, 255, 255, 0}}, {{255, 255, 255, 0}, {255, 255, 255, 0}, 
  {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 38}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 
  255, 255, 255}, {255, 255, 255, 255}, {222, 79, 62, 255}, {221, 75, 57, 255}, {235, 143, 133, 255}, {255, 255, 255, 
  255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, 
  {255, 255, 255, 255}, {255, 255, 255, 255}, {238, 169, 162, 255}, {221, 75, 57, 255}, {238, 162, 153, 255}, {255, 
  255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 
  255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 
  255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {0, 0, 0, 38}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {255, 
  255, 255, 0}}, {{255, 255, 255, 0}, {255, 255, 255, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 38}, {255, 255, 255, 
  255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {223, 84, 67, 255}, 
  {221, 75, 57, 255}, {226, 100, 86, 255}, {255, 253, 253, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 
  255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {235, 146, 
  136, 255}, {222, 79, 63, 255}, {250, 232, 231, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 
  255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, 
  {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {0, 
  0, 0, 38}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {255, 255, 255, 0}}, {{255, 255, 255, 0}, {255, 255, 255, 0}, 
  {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 38}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 
  255, 255, 255}, {255, 255, 255, 255}, {233, 138, 127, 255}, {221, 75, 57, 255}, {221, 75, 57, 255}, {233, 137, 126, 
  255}, {254, 251, 250, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, 
  {255, 255, 255, 255}, {250, 221, 219, 255}, {223, 84, 68, 255}, {239, 165, 157, 255}, {255, 255, 255, 255}, {255, 
  255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 
  255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 
  255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {0, 0, 0, 38}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {255, 
  255, 255, 0}}, {{255, 255, 255, 0}, {255, 255, 255, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 38}, {255, 255, 255, 
  255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {254, 249, 249, 255}, 
  {229, 116, 105, 255}, {221, 75, 57, 255}, {221, 75, 57, 255}, {224, 91, 76, 255}, {236, 149, 139, 255}, {242, 185, 
  179, 255}, {245, 202, 198, 255}, {241, 182, 176, 255}, {233, 137, 126, 255}, {223, 86, 70, 255}, {238, 168, 161, 
  255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, 
  {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 
  255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {0, 0, 0, 
  38}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {255, 255, 255, 0}}, {{255, 255, 255, 0}, {255, 255, 255, 0}, {0, 0, 
  0, 0}, {0, 0, 0, 0}, {0, 0, 0, 38}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 
  255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {241, 182, 175, 255}, {229, 114, 102, 
  255}, {223, 87, 71, 255}, {222, 78, 61, 255}, {222, 78, 61, 255}, {223, 85, 69, 255}, {227, 107, 95, 255}, {236, 
  153, 143, 255}, {251, 235, 233, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 
  255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 
  255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, 
  {255, 255, 255, 255}, {255, 255, 255, 255}, {0, 0, 0, 38}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {255, 255, 
  255, 0}}, {{255, 255, 255, 0}, {255, 255, 255, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 38}, {255, 255, 255, 255}, 
  {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 
  255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 
  255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 
  255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, 
  {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 
  255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {0, 0, 0, 
  38}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {255, 255, 255, 0}}, {{255, 255, 255, 0}, {255, 255, 255, 0}, {0, 0, 
  0, 0}, {0, 0, 0, 0}, {0, 0, 0, 38}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 
  255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 
  255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, 
  {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 
  255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 
  255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 
  255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {0, 0, 0, 38}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {255, 
  255, 255, 0}}, {{255, 255, 255, 0}, {255, 255, 255, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 38}, {255, 255, 255, 
  255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, 
  {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 
  255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 
  255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 
  255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, 
  {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {0, 
  0, 0, 38}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {255, 255, 255, 0}}, {{255, 255, 255, 0}, {255, 255, 255, 0}, 
  {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 34}, {251, 251, 251, 234}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 
  255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 
  255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 
  255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, 
  {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 
  255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 
  255, 255}, {255, 255, 255, 255}, {251, 251, 251, 235}, {0, 0, 0, 34}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, 
  {255, 255, 255, 0}}, {{255, 255, 255, 0}, {255, 255, 255, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 19}, {187, 187, 
  187, 101}, {251, 251, 251, 235}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 
  255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, 
  {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 
  255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 
  255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 
  255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {251, 251, 251, 232}, {187, 187, 187, 101}, 
  {0, 0, 0, 19}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {255, 255, 255, 0}}, {{255, 255, 255, 0}, {255, 255, 255, 
  0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 25}, {0, 0, 0, 35}, {0, 0, 0, 38}, {0, 0, 0, 38}, {0, 0, 0, 
  38}, {0, 0, 0, 38}, {0, 0, 0, 38}, {0, 0, 0, 38}, {0, 0, 0, 38}, {0, 0, 0, 38}, {0, 0, 0, 38}, {0, 0, 0, 38}, {0, 
  0, 0, 38}, {0, 0, 0, 38}, {0, 0, 0, 38}, {0, 0, 0, 38}, {0, 0, 0, 38}, {0, 0, 0, 38}, {0, 0, 0, 38}, {0, 0, 0, 38}, 
  {0, 0, 0, 38}, {0, 0, 0, 38}, {0, 0, 0, 38}, {0, 0, 0, 38}, {0, 0, 0, 38}, {0, 0, 0, 38}, {0, 0, 0, 38}, {0, 0, 0, 
  38}, {0, 0, 0, 38}, {0, 0, 0, 38}, {0, 0, 0, 35}, {0, 0, 0, 24}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 
  0, 0}, {255, 255, 255, 0}}, {{255, 255, 255, 0}, {255, 255, 255, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 
  0, 0, 4}, {0, 0, 0, 12}, {0, 0, 0, 13}, {0, 0, 0, 13}, {0, 0, 0, 13}, {0, 0, 0, 13}, {0, 0, 0, 13}, {0, 0, 0, 13}, 
  {0, 0, 0, 13}, {0, 0, 0, 13}, {0, 0, 0, 13}, {0, 0, 0, 13}, {0, 0, 0, 13}, {0, 0, 0, 13}, {0, 0, 0, 13}, {0, 0, 0, 
  13}, {0, 0, 0, 13}, {0, 0, 0, 13}, {0, 0, 0, 13}, {0, 0, 0, 13}, {0, 0, 0, 13}, {0, 0, 0, 13}, {0, 0, 0, 13}, {0, 
  0, 0, 13}, {0, 0, 0, 13}, {0, 0, 0, 13}, {0, 0, 0, 13}, {0, 0, 0, 13}, {0, 0, 0, 13}, {0, 0, 0, 13}, {0, 0, 0, 12}, 
  {0, 0, 0, 4}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {255, 255, 255, 0}}, {{255, 255, 255, 0}, 
  {255, 255, 255, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 
  0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, 
  {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 
  0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 
  0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {255, 255, 
  255, 0}}, {{255, 255, 255, 0}, {255, 255, 255, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 
  0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 
  0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, 
  {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 
  0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 
  0, 0}, {0, 0, 0, 0}, {255, 255, 255, 0}}, {{255, 255, 255, 0}, {255, 255, 255, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 
  0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 
  0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 
  0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, 
  {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 
  0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {255, 255, 255, 0}}}], "Byte", ColorSpace -> "RGB", 
 Interleaving -> True];

End[] (* End Private Context *)
           		
End[]


SetAttributes[{},{ReadProtected, Protected}];

System`Private`RestoreContextPath[];

(* Return three functions to define oauthservicedata, oauthcookeddata, oauthsendmessage  *)
{GooglePlusOAuth`Private`googleplusdata,GooglePlusOAuth`Private`googlepluscookeddata,GooglePlusOAuth`Private`googleplussendmessage}
