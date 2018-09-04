
System`Private`NewContextPath[{"OAuthClient`","System`"}];

Begin["GoogleAnalyticsOAuth`"] (* Begin Private Context *) 

Begin["`Private`"](* Begin Private Context *) 

(******************************* GoogleAnalytics *************************************)

(* Authentication information *)

googleanalyticsdata[]={
		"OAuthVersion"		->"2.0",
		"ServiceName"       -> "Google Analytics",
	    "AuthorizeEndpoint" -> "https://accounts.google.com/o/oauth2/auth",
	    "AccessEndpoint"    -> "https://accounts.google.com/o/oauth2/token",
	    "RedirectURI"       -> "urn:ietf:wg:oauth:2.0:oob",
	    "VerifierLabel"      -> "code",
	 	"ClientInfo"		-> {"745766852669-d32c30e6imf6o4uerv5hr40kjgprudvv.apps.googleusercontent.com","Pvu4Bu1cKg-0Wg6r-oGaqh-7"},(*{CLIENT ID, CLIENT SECRET} You can get this info from Google Developers Console > APIs & auth > Credentials > Client ID for native application*)
	 	"AuthenticationDialog" :> (OAuthClient`tokenOAuthDialog[#, "Google Analytics"]&),
	 	"Gets"				-> {},
	 	"Posts"				-> {},
	 	"RawGets"			-> {"RawData"},
	 	"RawPosts"			-> {},
	 	"Scope"				-> {"https%3A%2F%2Fwww.googleapis.com%2Fauth%2Fanalytics"},
 		"Information"		-> "A service to use Google Analytics"
}

(* a function for importing the raw data - usually json or xml - from the service *)
googleanalyticsimport[$Failed]:=Throw[$Failed]
(*googleanalyticsimport[json_String]:=With[{res=ImportString[json,"JSON"]},
	If[FreeQ[res,_["error",_]],
		Association@res,
		Message[ServiceExecute::apierr,"message"/.("error"/.res)];
		Throw[$Failed]
	]
]*)


googleanalyticsimport[raw_]:=ImportString[raw,"JSON"]



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

googleanalyticsdata["RawData"] = {
        "URL"				-> "https://www.googleapis.com/analytics/v3/data/ga",
        "BodyData"			-> {"ParameterlessBodyData"},
        "Headers" 			-> {"Content-Type" -> "application/json"},
        "Parameters"		-> {"ids", "start-date", "end-date", "metrics", "dimensions", "sort", "filters", "segment", "samplingLevel", "start-index", "max-results", "output", "fields", "prettyPrint", "userIp", "quotaUser", "access_token", "callback", "key"},
        "RequiredParameters"-> {"ids", "start-date", "end-date", "metrics"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> googleanalyticsimport
    }

$GoogleAnalyticsRefreshAPI="https://www.wolframcloud.com/objects/user-fa95220f-871c-4331-84ab-7951dd0666ca/googleplusrefresh";

googleanalyticsdata["RefreshAccessTokenFunction"]:=(
	ToExpression[
		URLFetch[$GoogleAnalyticsRefreshAPI, 
  			"Parameters" -> {"refreshtoken" -> ToString[#,InputForm], 
    		"AccessEndpoint" -> ("AccessEndpoint"/.googleanalyticsdata[])},
   			"VerifyPeer" -> False]]&)/;OAuthClient`Private`$OAuthCloudCredentialsQ
   			
googleanalyticsdata["RefreshAccessTokenFunction"]:=(Block[{url,info,res, data,key, time},
	If[#===None,Return[$Failed]];
	info=OAuthClient`Private`getclientinfo["GoogleAnalytics"];
	url={"AccessEndpoint"/.googleanalyticsdata[],
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

 
googleanalyticsdata[___]:=$Failed
(****** Cooked Properties ******)
  
(* cooked data queries 
	Cooked queries call at least one of the raw queried underneath but add pre and post processing to make the function easier to use and the result clearer.
*)  

googleanalyticscookeddata[prop_,id_,rule_Rule, rest___]:=googleanalyticscookeddata[prop,id,{rule}, rest]
googleanalyticscookeddata[prop_,id_]:=googleanalyticscookeddata[prop,id,{}]
  

(* Cooked *)
googleanalyticscookeddata["UserData",id_,args_]:=Module[{params,rawdata, data},
	params=filterparameters[args,getallparameters["RawUserData"]];
	rawdata=OAuthClient`rawoauthdata[id,"RawUserData",params];
	data=googleanalyticsimport[rawdata];
	Association[Replace[Normal[data],(Rule[a_,b_]):>(Rule[camelcase[a],b]),Infinity]]
]

getpagetoken[json_]:=With[
	{tokens=StringCases[json, "\"nextPageToken\": \"" ~~ (t : Shortest[__]) ~~ "\"" :> t]},
	If[Length[tokens]===1,First[tokens],$Failed]
]


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
			

tostring[str_String,_]:=str
tostring[default_]:=default
tostring[Automatic,default_]:=default
tostring[str_,_]:=ToString[str]

readDate[date_,form_:DateObject]:=form[DateList[date]]

getallparameters[str_]:=DeleteCases[Flatten[{"Parameters","PathParameters","BodyData","MultipartData"}/.googleanalyticsdata[str]],
	("Parameters"|"PathParameters"|"BodyData"|"MultipartData")]

(*
fromHTML[str_]:=StringReplace[str, ("&#" ~~ (char : (WordCharacter ..) /; 
      StringLength[char] < 5) ~~ ";") :> (FromCharacterCode@
    FromDigits[char])]
    *)

fromHTML[str_String]:=ImportString[str,"HTML"]   
fromHTML[l:{___String}]:=ImportString[#,"HTML"]  &/@l  
fromHTML[___]:={}
    

End[] (* End Private Context *)
           		
End[]


SetAttributes[{},{ReadProtected, Protected}];

System`Private`RestoreContextPath[];

(* Return three functions to define oauthservicedata, oauthcookeddata, oauthsendmessage  *)
{GoogleAnalyticsOAuth`Private`googleanalyticsdata,GoogleAnalyticsOAuth`Private`googleanalyticscookeddata,GoogleAnalyticsOAuth`Private`googleanalyticssendmessage}
