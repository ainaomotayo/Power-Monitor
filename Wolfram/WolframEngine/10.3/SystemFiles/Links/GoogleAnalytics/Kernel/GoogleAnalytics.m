
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
	 	"RequestFormat" 	-> {"Headers","Bearer"},
	 	"Gets"				-> {"ReportData","AllData"},
	 	"Posts"				-> {},
	 	"RawGets"			-> {"RawData"},
	 	"RawPosts"			-> {},
	 	"RawDeletes"		-> {},
	 	"Deletes"           -> {},
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

googleanalyticscookeddata[prop_,id_]:=googleanalyticscookeddata[prop,id,{}]
  
googleanalyticscookeddata[prop_,id_,rules___Rule]:=googleanalyticscookeddata[prop,id,{rules}]
(* Cooked *)

googleanalyticscookeddata["UserData",id_,args_]:=Module[{params,rawdata, data},
	params=filterparameters[args,getallparameters["RawUserData"]];
	rawdata=OAuthClient`rawoauthdata[id,"RawUserData",params];
	data=googleanalyticsimport[rawdata];
	Association[Replace[Normal[data],(Rule[a_,b_]):>(Rule[camelcase[a],b]),Infinity]]
]

googleanalyticscookeddata[req:"ReportData"|"AllData",id_,args_]:=Block[{rawdata,invalidParameters,params={},fields,columns,maxitems,sdate,edate,metrics,dimensions,sort},
	invalidParameters = Select[Keys[args],!MemberQ[{"ProfileID","StartDate","EndDate","Metrics","Dimensions","Sort","Filters","Segment","SamplingLevel","MaxItems","StartIndex","UserIP","QuotaUser","Fields"},#]&]; 
	If[Length[invalidParameters]>0,
	(
		Message[ServiceObject::noget,#,"GoogleAnalytics"]&/@invalidParameters;
		Throw[$Failed]
	)];
	If[KeyExistsQ[args,"ProfileID"],
	(
		If[!(IntegerQ["ProfileID"/.args]||StringQ["ProfileID"/.args]),
		(	
			Message[ServiceExecute::nval,"ProfileID","GoogleAnalytics"];
			Throw[$Failed]
		)];
		params = Append[params,Rule["ids","ga:"<>ToString["ProfileID" /. args]]]
	)];
	If[ KeyExistsQ[args,"StartDate"],
	(
		If[!(StringQ["StartDate"/.args]||MatchQ["StartDate"/.args,DateObject[__]]),
		(	
			Message[ServiceExecute::nval,"StartDate","GoogleAnalytics"];
			Throw[$Failed]
		)];
		sdate = DateObject[("StartDate" /. args)];
		If[MatchQ[sdate,DateObject[__String]],
		(	
			Message[ServiceExecute::nval,"StartDate","GoogleAnalytics"];
			Throw[$Failed]
		)];
        params =Append[params, Rule["start-date",DateString[sdate, {"Year", "-", "Month", "-", "Day"}]]]     
	)];
	If[ KeyExistsQ[args,"EndDate"],
	(
		If[!(StringQ["EndDate"/.args]||MatchQ["EndDate"/.args,DateObject[__]]),
		(	
			Message[ServiceExecute::nval,"EndDate","GoogleAnalytics"];
			Throw[$Failed]
		)];
		edate = DateObject[("EndDate" /. args)];
		If[MatchQ[edate,DateObject[__String]],
		(	
			Message[ServiceExecute::nval,"EndDate","GoogleAnalytics"];
			Throw[$Failed]
		)];
        params =Append[params, Rule["end-date",DateString[edate, {"Year", "-", "Month", "-", "Day"}]]]             
	)];
	If[KeyExistsQ[args,"Metrics"],
	(
		metrics="Metrics" /. args;
		If[!(MatchQ[metrics, {__String}]||StringQ[metrics]),
		(	
			Message[ServiceExecute::nval,"Metrics","GoogleAnalytics"];
			Throw[$Failed]
		)];
		If[MatchQ[metrics,{__String}],
			params = Append[params,Rule["metrics",StringJoin[Riffle[deCamelizeDM/@metrics, ","]]]]
		];
		If[StringQ[metrics],	
			params = Append[params,Rule["metrics",deCamelizeDM[metrics]]]
		];
	)];
	If[KeyExistsQ[args,"Dimensions"],
	(
		dimensions="Dimensions" /. args;
		If[!(MatchQ[dimensions, {__String}]||StringQ[dimensions]),
		(	
			Message[ServiceExecute::nval,"Dimensions","GoogleAnalytics"];
			Throw[$Failed]
		)];
		If[MatchQ[dimensions,{__String}],
			params = Append[params,Rule["dimensions",StringJoin[Riffle[deCamelizeDM/@dimensions, ","]]]]
		];
		If[StringQ[dimensions],	
			params = Append[params,Rule["dimensions",deCamelizeDM[dimensions]]]
		];
	)];
	If[KeyExistsQ[args,"Sort"],
	(
		sort="Sort" /. args;
		If[!(MatchQ[sort, {__String}]||StringQ[sort]),
		(	
			Message[ServiceExecute::nval,"Sort","GoogleAnalytics"];
			Throw[$Failed]
		)];
		If[MatchQ[sort,{__String}],
			params = Append[params,Rule["sort",StringJoin[Riffle[deCamelizeDM/@sort, ","]]]]
		];
		If[StringQ[sort],	
			params = Append[params,Rule["sort",deCamelizeDM[sort]]]
		];
	)];
	If[KeyExistsQ[args,"Filters"],
	(
		
		params = Append[params,Rule["filters",GAFiltersParse["Filters" /. args]]]
	)];
	If[KeyExistsQ[args,"Segment"],
	(
		params = Append[params,Rule["segment","Segment" /. args]]
	)];
	If[ KeyExistsQ[args,"SamplingLevel"],
  	(
		If[!StringMatchQ[ToString["SamplingLevel" /. args], "Default" | "Faster" | "HigherPrecision"],
		(	
			Message[ServiceExecute::nval,"SamplingLevel","GoogleAnalytics"];
			Throw[$Failed]
		)];  	
		params = Append[params,Rule["samplingLevel",("SamplingLevel" /. args)/.{"Default"->"DEFAULT","Faster"->"FASTER","HigherPrecision"->"HIGHER_PRECISION"}]];
	)];
	If[KeyExistsQ[args,"MaxItems"],
	(
		If[!(IntegerQ["MaxItems"/.args]&&("MaxItems"/.args)>0&&("MaxItems"/.args)<=10000),
		(	
			Message[ServiceExecute::nval,"MaxItems","GoogleAnalytics"];
			Throw[$Failed]
		)];
		maxitems="MaxItems"/.args;
		params = Append[params,Rule["max-results",ToString[maxitems]]]
	)];
	If[KeyExistsQ[args,"StartIndex"],
	(
		If[!(IntegerQ["StartIndex"/.args]&&("StartIndex"/.args)>0),
		(	
			Message[ServiceExecute::nval,"StartIndex","GoogleAnalytics"];
			Throw[$Failed]
		)];
		params = Append[params,Rule["start-index",ToString[(("StartIndex"/.args)-1)*maxitems + 1]]]
	)];
	If[KeyExistsQ[args,"UserIP"],
	(
		If[!StringQ["UserIP"/.args],
		(	
			Message[ServiceExecute::nval,"UserIP","GoogleAnalytics"];
			Throw[$Failed]
		)];
		params = Append[params,Rule["userIp","UserIP" /. args]]
	)];
	If[KeyExistsQ[args,"QuotaUser"],
	(
		If[!StringQ["QuotaUser"/.args],
		(	
			Message[ServiceExecute::nval,"QuotaUser","GoogleAnalytics"];
			Throw[$Failed]
		)];
		params = Append[params,Rule["quotaUser","QuotaUser" /. args]]
	)];
	
	Switch[req,
		"ReportData",
		(
			rawdata = ServiceExecute["GoogleAnalytics","RawData",Join[params,{"fields" -> "rows,columnHeaders/name","output"->"json"}]];
			If[KeyExistsQ[rawdata,"error"],
  		 	(
   		   		Message[ServiceExecute::serrormsg,("message" /. ("error" /. rawdata))];
   		    	Throw[$Failed]
 			)];
			columns="name" /. ("columnHeaders" /. rawdata);
			If[!KeyExistsQ[rawdata,"rows"],
			(
				rawdata = Append[rawdata,"rows"->{ConstantArray["",Length[Flatten[{metrics}]]+Length[Flatten[{dimensions}]]]}];
				Dataset[Association @@@ (Reverse[Inner[Rule, ("rows" /. rawdata), camelizeDM/@columns, List], 3])]
			),
			(
				Dataset[Association @@@ (Reverse[Inner[Rule, ("rows" /. rawdata), camelizeDM/@columns, List], 3]/.
					{("Date"->a_):>("Date"->DateObject[a]),("Year"->a_):>("Year"->DateObject[a]),("YearMonth"->a_):>("YearMonth"->DateObject[StringInsert[a," ",5]]),
						("DateHour"->a_):>("DateHour"->DateObject[StringInsert[a," ",9]<>":00:00",TimeZone->0])
					})]
			)]	
		),
		"AllData",
		(
			If[KeyExistsQ[args,"Fields"],
			(
				params = Append[params,Rule["fields","Fields" /. args]]
			)];
			rawdata = ServiceExecute["GoogleAnalytics","RawData",Join[params,{"output"->"json"}]];
			If[KeyExistsQ[rawdata,"error"],
  		 	(
   		  	 	Message[ServiceExecute::serrormsg,("message" /. ("error" /. rawdata))];
   		    	Throw[$Failed]
 			)];
 			If[!KeyExistsQ[rawdata,"rows"],
			(
				rawdata = Append[rawdata,"rows"->{ConstantArray["",Length[metrics]+Length[dimensions]]}]
			)];
			Dataset[Association @@ Replace[rawdata, r : {__Rule} :> Association[r], -1]]
		)
	]
]



getpagetoken[json_]:=With[
	{tokens=StringCases[json, "\"nextPageToken\": \"" ~~ (t : Shortest[__]) ~~ "\"" :> t]},
	If[Length[tokens]===1,First[tokens],$Failed]
]

(*** Utilities ***)

camelizeDM[text_] := StringReplace[text, StartOfString ~~ "ga:" ~~ a_ ~~ b___ :> ToUpperCase[a] ~~ b]

deCamelizeDM[text_] := StringReplace[text, {StartOfString ~~ "-" ~~ a_ ~~ b___ :> "-ga:" ~~ ToLowerCase[a] ~~ b, StartOfString ~~ a_ ~~ b___ :> "ga:" ~~ ToLowerCase[a] ~~ b}]

GAFiltersParse[e_] :=
 e //. {Verbatim[Alternatives][x_] :> x, Verbatim[Alternatives][x_, y__] :> "" ~~ x ~~ "," ~~ Alternatives[y] ~~ "",
   Verbatim[Rule][x_, Verbatim[RegularExpression][y_]] :> "" ~~ deCamelizeDM[ToString[x]] ~~ "=~" ~~ ToString[y] ~~ "",
   Verbatim[Rule][x_, Verbatim[Except][Verbatim[RegularExpression][y_]]] :> "" ~~ deCamelizeDM[ToString[x]] ~~ "!~" ~~ ToString[y] ~~ "",
   Verbatim[Rule][x_, Verbatim[Except][y_]] :> "" ~~ deCamelizeDM[ToString[x]] ~~ "!=" ~~ ToString[y] ~~ "",
   Verbatim[Rule][x_, y_] :> "" ~~ deCamelizeDM[ToString[x]] ~~ "==" ~~ ToString[y] ~~ "",
   Verbatim[NotSuperset][x_, y_] :> "" ~~ deCamelizeDM[ToString[x]] ~~ "!@" ~~ ToString[y] ~~ "",
   Verbatim[Superset][x_, y_] :> "" ~~ deCamelizeDM[ToString[x]] ~~ "=@" ~~ ToString[y] ~~ "",
   Verbatim[Greater][x_, y_] :> "" ~~ deCamelizeDM[ToString[x]] ~~ ">" ~~ ToString[y] ~~ "",
   Verbatim[GreaterEqual][x_, y_] :> "" ~~ deCamelizeDM[ToString[x]] ~~ ">=" ~~ ToString[y] ~~ "",
   Verbatim[Less][x_, y_] :> "" ~~ deCamelizeDM[ToString[x]] ~~ "<" ~~ ToString[y] ~~ "",
   Verbatim[LessEqual][x_, y_] :> "" ~~ deCamelizeDM[ToString[x]] ~~ "<=" ~~ ToString[y] ~~ "",
   List[x_] :> x, List[x_, y__] :> "" ~~ x ~~ ";" ~~ List[y] ~~ ""
   }
       
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
