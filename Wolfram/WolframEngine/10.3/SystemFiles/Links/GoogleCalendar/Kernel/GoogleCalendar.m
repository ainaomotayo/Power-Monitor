Begin["GoogleCalendar`"] (* Begin Private Context *) 

Begin["`Private`"](* Begin Private Context *) 

(******************************* GoogleCalendar *************************************)

(* Authentication information *)

googlecalendardata[]={
		"OAuthVersion"		->"2.0",
		"ServiceName"       -> "GoogleCalendar",
	    "AuthorizeEndpoint" -> "https://accounts.google.com/o/oauth2/auth",
	    "AccessEndpoint"    -> "https://accounts.google.com/o/oauth2/token",
	    "RedirectURI"       -> "urn:ietf:wg:oauth:2.0:oob",
	    "VerifierLabel"      -> "code",
	 	"ClientInfo"		-> {"745766852669-d32c30e6imf6o4uerv5hr40kjgprudvv.apps.googleusercontent.com","Pvu4Bu1cKg-0Wg6r-oGaqh-7"},
	 	"AuthenticationDialog" :> (OAuthClient`tokenOAuthDialog[#, "GoogleCalendar"]&),
	 	"Gets"				-> {"CalendarList","CalendarDataset","CalendarInformation","EventList","EventDataset","EventInformation"},
	 	"Posts"				-> {},
	 	"RawGets"			-> {"RawCalendarList","RawCalendarDetails","RawEventList","RawEventDetails","RawUserSettings"},
	 	"RawPosts"			-> {},
	 	"Scope"				-> {"https%3A%2F%2Fwww.googleapis.com%2Fauth%2Fcalendar+https%3A%2F%2Fwww.googleapis.com%2Fauth%2Fcalendar.readonly"},
 		"Information"		-> "A service for receiving data from Google Calendars"
}

(* a function for importing the raw data - usually json or xml - from the service *)
googlecalendarimport[$Failed]:=Throw[$Failed]
(*googlecalendarimport[json_String]:=With[{res=ImportString[json,"JSON"]},
	If[FreeQ[res,_["error",_]],
		Association@res,
		Message[ServiceExecute::apierr,"message"/.("error"/.res)];
		Throw[$Failed]
	]
]*)

googlecalendarimport[raw_]:=ImportString[ToString[raw,"CharacterEncoding"->"UTF8"],"JSON"]


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

(* details about parameters here https://developers.google.com/apis-explorer/#s/calendar/v3/calendar.settings.list *)
googlecalendardata["RawUserSettings"] = {
        "URL"					-> "https://www.googleapis.com/calendar/v3/users/me/settings",
        "Parameters"			-> {"maxResults","pageToken","syncToken","fields"},
        "RequiredParameters"	-> {},
        "HTTPSMethod"			-> "GET",
        "ResultsFunction"		-> googlecalendarimport
    }

(* details about parameters here https://developers.google.com/apis-explorer/#s/calendar/v3/calendar.calendarList.list *)    
googlecalendardata["RawCalendarList"] = {
        "URL"					-> "https://www.googleapis.com/calendar/v3/users/me/calendarList",
        "Parameters"			-> {"maxResults","pageToken","showDeleted","showHidden","minAccessRole","syncToken","fields"},
        "RequiredParameters"	-> {},
        "HTTPSMethod"			-> "GET",
        "ResultsFunction"		-> googlecalendarimport
    }

(* details about parameters here https://developers.google.com/apis-explorer/#s/calendar/v3/calendar.calendars.get *)
googlecalendardata["RawCalendarDetails"] = {
        "URL"					-> (ToString@StringForm["https://www.googleapis.com/calendar/v3/calendars/`1`",#]&),
        "PathParameters"		-> {"calendarID"},
        "Parameters"			-> {"fields"},
        "RequiredParameters"	-> {"calendarID"},
        "HTTPSMethod"			-> "GET",
        "ResultsFunction"		-> googlecalendarimport
    }

(* details about parameters here https://developers.google.com/apis-explorer/#s/calendar/v3/calendar.events.list *)    
googlecalendardata["RawEventList"] = {
        "URL"					-> (ToString@StringForm["https://www.googleapis.com/calendar/v3/calendars/`1`/events",#]&),
        "PathParameters"		-> {"calendarID"},
        "Parameters"			-> {"orderBy","maxResults","pageToken","maxAttendees","alwaysIncludeEmail","iCalUID","privateExtendedProperty",
        							"q","sharedExtendedProperty","showDeleted","showHiddenInvitations","singleEvents","syncToken",
        							"timeMax","timeMin","timeZone","updatedMin","fields"},
        "RequiredParameters"	-> {"calendarID"},
        "HTTPSMethod"			-> "GET",
        "ResultsFunction"		-> googlecalendarimport
    }

(* details about parameters here https://developers.google.com/apis-explorer/#s/calendar/v3/calendar.events.get *)    
googlecalendardata["RawEventDetails"] = {
        "URL"					-> (ToString@StringForm["https://www.googleapis.com/calendar/v3/calendars/`1`/events/`2`",##]&),
        "PathParameters"		-> {"calendarID","eventID"},
        "Parameters"			-> {"alwaysIncludeEmail","maxAttendees","timeZone","fields"},
        "RequiredParameters"	-> {"calendarID","eventID"},
        "HTTPSMethod"			-> "GET",
        "ResultsFunction"		-> googlecalendarimport
    }

$GooglePlusRefreshAPI="https://www.wolframcloud.com/objects/user-fa95220f-871c-4331-84ab-7951dd0666ca/googlecalendarrefresh";
googlecalendardata["RefreshAccessTokenFunction"]:=(
	ToExpression[
		URLFetch[$GooglePlusRefreshAPI, 
  			"Parameters" -> {"refreshtoken" -> ToString[#,InputForm], 
    		"AccessEndpoint" -> ("AccessEndpoint"/.googlecalendardata[])},
   			"VerifyPeer" -> False]]&)/;OAuthClient`Private`$OAuthCloudCredentialsQ
   			
googlecalendardata["RefreshAccessTokenFunction"]:=(Block[{url,info,res, data,key, time},
	If[#===None,Return[$Failed]];
	info=OAuthClient`Private`getclientinfo["GooglePlus"];
	url={"AccessEndpoint"/.googlecalendardata[],
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

googlecalendardata[___]:=$Failed
(****** Cooked Properties ******)
  
(* cooked data queries 
	Cooked queries call at least one of the raw queried underneath but add pre and post processing to make the function easier to use and the result clearer.
*)  

googlecalendarcookeddata[prop_,id_,rules___Rule]:=googlecalendarcookeddata[prop,id,{rules}]  

(* Cooked *)
googlecalendarcookeddata[prop:("CalendarList"|"CalendarDataset"), id_, args_] := Module[{params={},pagetoken,synctoken,sd,sh,rawdata,invalidParameters,limit=100,maxPerPage=250,
											fieldnames,orderList,result},
		invalidParameters = Select[Keys[args],!MemberQ[{"MaxItems","PageToken","SyncToken","ShowHidden","ShowDeleted"},#]&]; 
	
		If[Length[invalidParameters]>0,
		(
			Message[ServiceObject::noget,#,prop]&/@invalidParameters;
			Throw[$Failed]
		)];	
		
		If[KeyExistsQ[args,"MaxItems"],
		(
			limit = "MaxItems" /. args;
			If[!IntegerQ[limit],
			(	
				Message[ServiceExecute::nval,"MaxItems","GoogleCalendar"];
				Throw[$Failed]
			)];	
			params = Append[params,"maxResults"->ToString[Min[limit,maxPerPage]]];					
		)];
		
		If[KeyExistsQ[args,"PageToken"],
		(
			pagetoken = "PageToken" /. args;
			params = Append[params,"pageToken"->pagetoken];			
		)];
		
		If[KeyExistsQ[args,"SyncToken"],
		(
			synctoken = "SyncToken" /. args;
			params = Append[params,"syncToken"->synctoken];			
		)];
		
		If[KeyExistsQ[args,"ShowDeleted"],
		(
			sd = "ShowDeleted" /. args;
			Switch[sd,
				True,
				params = Append[params,"showDeleted"->"True"],
				False,
				params = Append[params,"showDeleted"->"False"],
				_,
				(
					Message[ServiceExecute::nval,"ShowDeleted","GoogleCalendar"];	
					Throw[$Failed]
				)
			];		
		)];
		
		If[KeyExistsQ[args,"ShowHidden"],
		(
			sh = "ShowHidden" /. args;
			Switch[sh,
				True,
				params = Append[params,"showHidden"->"True"],
				False,
				params = Append[params,"showHidden"->"False"],
				_,
				(
					Message[ServiceExecute::nval,"ShowHidden","GoogleCalendar"];	
					Throw[$Failed]
				)
			];		
		)];
		
		rawdata = OAuthClient`rawoauthdata[id,"RawCalendarList",params];
		result = formatresults[rawdata];
		result = "items" /. result;		

		(*If[KeyExistsQ[data,"error"],
		(
			Message[ServiceExecute::serrormsg,"message"/.data];
			Throw[$Failed]
		)];*)
				
		fieldnames = {"id","summary","description","location","timeZone","etag"};
		result = FilterRules[#, fieldnames] & /@ result;
		
		orderList = Thread[fieldnames -> Range[Length[fieldnames]]];
   		result = Function[r,SortBy[r, (#[[1]] /. orderList&)]]/@result;
   
		result = ReplaceAll[result,Rule["id",y_]:>Rule["ID",y]];
		result = ReplaceAll[result,Rule["summary",y_]:>Rule["Summary",y]];
		result = ReplaceAll[result,Rule["description",y_]:>Rule["Description",y]];
		result = ReplaceAll[result,Rule["location",y_]:>Rule["Location",y]];
		result = ReplaceAll[result,Rule["timeZone",y_]:>Rule["TimeZone",Interpreter["TimeZone"][y]]];
		result = ReplaceAll[result,Rule["etag",y_]:>Rule["ETag",y]];
		
		If[prop=="CalendarList",
			result,
			(
				If[Length[result]==0,
					Dataset[Association[]],
					Dataset[Association /@ result]
				]	
			)
		]	
]

googlecalendarcookeddata["CalendarInformation", id_, args_] := Module[{rawdata, invalidParameters,cId,fieldnames,orderList,result},
		invalidParameters = Select[Keys[args],!MemberQ[{"CalendarID"},#]&]; 
	
		If[Length[invalidParameters]>0,
		(
			Message[ServiceObject::noget,#,"CalendarInformation"]&/@invalidParameters;
			Throw[$Failed]
		)];	
	
		If[KeyExistsQ[args,"CalendarID"],
			cId = "CalendarID" /. args,
			(
				Message[ServiceExecute::nparam,"CalendarID"];			
				Throw[$Failed]
			)
		];
		
		rawdata = OAuthClient`rawoauthdata[id,"RawCalendarDetails",{"calendarID"->ToString[cId]}];
		result = formatresults[rawdata];
		
		fieldnames = {"id","summary","description","location","timeZone","etag"};
		result = FilterRules[result, fieldnames];
		
		(*orderList = Thread[fieldnames -> Range[Length[fieldnames]]];
   		result = Function[r,SortBy[r, (#[[1]] /. orderList&)]]/@result;*)
   
		result = ReplaceAll[result,Rule["id",y_]:>Rule["ID",y]];
		result = ReplaceAll[result,Rule["summary",y_]:>Rule["Summary",y]];
		result = ReplaceAll[result,Rule["description",y_]:>Rule["Description",y]];
		result = ReplaceAll[result,Rule["location",y_]:>Rule["Location",y]];
		result = ReplaceAll[result,Rule["timeZone",y_]:>Rule["TimeZone",Interpreter["TimeZone"][y]]];
		result = ReplaceAll[result,Rule["etag",y_]:>Rule["ETag",y]];
		
		Association[result]		
]

googlecalendarcookeddata[prop:("EventList"|"EventDataset"), id_, args_] := Module[{cId,maxAttendees,sort,query,params={},pagetoken,synctoken,sd,sh,rawdata,invalidParameters,limit=250,maxPerPage=2500,
											fieldnames,orderList,result,tMax,tMin,updateMin,singleEvents=False,msg},
		invalidParameters = Select[Keys[args],!MemberQ[{"CalendarID","MaxAttendees","MaxItems","PageToken","SyncToken","ShowHiddenInvitations","ShowDeleted","SortBy","Query",
														"SingleEvents","TimeMax","TimeMin","UpdatedMin"},#]&]; 
	
		If[Length[invalidParameters]>0,
		(
			Message[ServiceObject::noget,#,prop]&/@invalidParameters;
			Throw[$Failed]
		)];	
		
		If[KeyExistsQ[args,"CalendarID"],
			cId = "CalendarID" /. args,
			cId = "primary"
		];
		params = Append[params,"calendarID"->ToString[cId]];
		
		If[KeyExistsQ[args,"MaxAttendees"],
		(
			maxAttendees = "MaxAttendees" /. args;
			If[!IntegerQ[maxAttendees],
			(	
				Message[ServiceExecute::nval,"MaxAttendees","GoogleCalendar"];
				Throw[$Failed]
			)];	
			params = Append[params,"maxAttendees"->ToString[maxAttendees]];			
		)];
		
		If[KeyExistsQ[args,"MaxItems"],
		(
			limit = "MaxItems" /. args;
			If[!IntegerQ[limit],
			(	
				Message[ServiceExecute::nval,"MaxItems","GoogleCalendar"];
				Throw[$Failed]
			)];	
			params = Append[params,"maxResults"->ToString[Min[limit,maxPerPage]]];					
		)];
		
		If[KeyExistsQ[args,"PageToken"],
		(
			pagetoken = "PageToken" /. args;
			params = Append[params,"pageToken"->pagetoken];			
		)];
		
		If[KeyExistsQ[args,"Query"],
		(
			query = "Query" /. args;
			params = Append[params,"q"->query];			
		)];
		
		If[KeyExistsQ[args,"SyncToken"],
		(
			synctoken = "SyncToken" /. args;
			params = Append[params,"syncToken"->synctoken];			
		)];
		
		If[KeyExistsQ[args,"ShowDeleted"],
		(
			sd = "ShowDeleted" /. args;
			Switch[sd,
				True,
				params = Append[params,"showDeleted"->"True"],
				False,
				params = Append[params,"showDeleted"->"False"],
				_,
				(
					Message[ServiceExecute::nval,"ShowDeleted","GoogleCalendar"];	
					Throw[$Failed]
				)
			];		
		)];
		
		If[KeyExistsQ[args,"ShowHiddenInvitations"],
		(
			sd = "ShowHiddenInvitations" /. args;
			Switch[sd,
				True,
				params = Append[params,"showHiddenInvitations"->"True"],
				False,
				params = Append[params,"showHiddenInvitations"->"False"],
				_,
				(
					Message[ServiceExecute::nval,"ShowHiddenInvitations","GoogleCalendar"];	
					Throw[$Failed]
				)
			];		
		)];
		
		If[KeyExistsQ[args,"SingleEvents"],
		(
			singleEvents = "SingleEvents" /. args;
			Switch[singleEvents,
				True,
				params = Append[params,"singleEvents"->"True"],
				False,
				params = Append[params,"singleEvents"->"False"],
				_,
				(
					Message[ServiceExecute::nval,"SingleEvents","GoogleCalendar"];	
					Throw[$Failed]
				)
			];		
		)];
		
		If[KeyExistsQ[args,"SortBy"],
		(
			sort = "SortBy" /. args;
			Switch[sort,
				"StartTime" && singleEvents,
				params = Append[params,"orderBy"->"startTime"],
				"Updated",
				params = Append[params,"orderBy"->"updated"],
				_,
				(
					Message[ServiceExecute::nval,"SortBy","GoogleCalendar"];	
					Throw[$Failed]
				)
			];			
		)];
		
		If[KeyExistsQ[args,"TimeMax"],
		(
			tMax = "TimeMax" /. args;
			tMax = DateString[tMax, "ISODateTime"] <> "Z";
			params = Append[params,"timeMax"->tMax];			
		)];
		
		If[KeyExistsQ[args,"TimeMin"],
		(
			tMin = "TimeMin" /. args;
			tMin = DateString[tMin, "ISODateTime"] <> "Z";
			params = Append[params,"timeMin"->tMin];			
		)];
		
		If[KeyExistsQ[args,"UpdatedMin"],
		(
			updateMin = "UpdatedMin" /. args;
			updateMin = DateString[updateMin, "ISODateTime"] <> "Z";
			params = Append[params,"updatedMin"->updateMin];			
		)];
		
		rawdata = OAuthClient`rawoauthdata[id,"RawEventList",params];
		result = formatresults[rawdata];
		
		If[KeyExistsQ[result,"error"],
			msg = "message" /. ("error" /. result);
			Message[ServiceExecute::serrormsg,msg];
			Throw[$Failed]			
		];
		
		result = "items" /. result;		

		fieldnames = {"start","end","location","summary","created","organizer","attendees","updated","id","etag","status"};
		result = FilterRules[#, fieldnames] & /@ result;
		
		orderList = Thread[fieldnames -> Range[Length[fieldnames]]];
   		result = Function[r,SortBy[r, (#[[1]] /. orderList&)]]/@result;
   
		result = ReplaceAll[result,Rule[x_,y_]:>Rule[camelCase[x],y]];
		result = ReplaceAll[result,Rule["Id",y_]:>Rule["ID",y]];
		result = ReplaceAll[result,Rule["Updated",y_]:>Rule["Updated",DateObject[StringReplace[y,a:RegularExpression["\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}\\w?"] ~~ ___ :> a]]]];
		result = ReplaceAll[result,Rule["Created",y_]:>Rule["Created",DateObject[StringReplace[y,a:RegularExpression["\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}\\w?"] ~~ ___ :> a]]]];
		result = ReplaceAll[result,Rule["Start",y_]:>Rule["Start",ReplaceAll[y,Rule[a_,b_]:>Rule[camelCase[a],b]]]];
		result = ReplaceAll[result,Rule["Start",{b___,Rule["DateTime",dt_],a___}]:>Rule["Start",{b,Rule["DateTime",DateObject[StringReplace[dt,r:RegularExpression["\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}\\w?"] ~~ ___ :> r]]],a}]];
		result = ReplaceAll[result,Rule["End",y_]:>Rule["End",ReplaceAll[y,Rule[a_,b_]:>Rule[camelCase[a],b]]]];
		result = ReplaceAll[result,Rule["End",{b___,Rule["DateTime",dt_],a___}]:>Rule["End",{b,Rule["DateTime",DateObject[StringReplace[dt,r:RegularExpression["\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}\\w?"] ~~ ___ :> r]]],a}]];
		result = ReplaceAll[result,Rule["Organizer",y_]:>Rule["Organizer",ReplaceAll[y,Rule[a_,b_]:>Rule[camelCase[a],b]]]];
		result = ReplaceAll[result,Rule["Attendees",y_]:>Rule["Attendees",(ReplaceAll[#,Rule[a_,b_]:>Rule[camelCase[a],b]]&/@y)]];
		result = ReplaceAll[result,Rule["Etag",y_]:>Rule["ETag",y]];
		
		If[prop=="EventList",
			result,
			(
				If[Length[result]==0,
					Dataset[Association[]],
					Dataset[Association /@ result]
				]	
			)
		]	
]

googlecalendarcookeddata["EventInformation", id_, args_] := Module[{params={},rawdata, invalidParameters,cId,eId,maxAttendees,fieldnames,orderList,result},
		invalidParameters = Select[Keys[args],!MemberQ[{"CalendarID","EventID","MaxAttendees"},#]&]; 
	
		If[Length[invalidParameters]>0,
		(
			Message[ServiceObject::noget,#,"EventInformation"]&/@invalidParameters;
			Throw[$Failed]
		)];	
	
		If[KeyExistsQ[args,"CalendarID"],
			cId = "CalendarID" /. args,
			cId = "primary"
		];
		params = Append[params,"calendarID"->ToString[cId]];
		
		If[KeyExistsQ[args,"EventID"],
			eId = "EventID" /. args,
			(
				Message[ServiceExecute::nparam,"EventID"];			
				Throw[$Failed]
			)
		];
		params = Append[params,"eventID"->ToString[eId]];
		
		If[KeyExistsQ[args,"MaxAttendees"],
		(
			maxAttendees = "MaxAttendees" /. args;
			If[!IntegerQ[maxAttendees],
			(	
				Message[ServiceExecute::nval,"MaxAttendees","GoogleCalendar"];
				Throw[$Failed]
			)];	
			params = Append[params,"maxAttendees"->ToString[maxAttendees]];			
		)];
		
		rawdata = OAuthClient`rawoauthdata[id,"RawEventDetails",params];
		result = formatresults[rawdata];
		
		fieldnames = {"start","end","location","summary","created","organizer","attendees","updated","id","etag","status"};
		result = FilterRules[result, fieldnames];
		
		orderList = Thread[fieldnames -> Range[Length[fieldnames]]];
   		result = SortBy[result, (#[[1]] /. orderList&)];
   
		result = ReplaceAll[result,Rule[x_,y_]:>Rule[camelCase[x],y]];
		result = ReplaceAll[result,Rule["Id",y_]:>Rule["ID",y]];
		result = ReplaceAll[result,Rule["Updated",y_]:>Rule["Updated",DateObject[StringReplace[y,a:RegularExpression["\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}\\w?"] ~~ ___ :> a]]]];
		result = ReplaceAll[result,Rule["Created",y_]:>Rule["Created",DateObject[StringReplace[y,a:RegularExpression["\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}\\w?"] ~~ ___ :> a]]]];
		result = ReplaceAll[result,Rule["Start",y_]:>Rule["Start",ReplaceAll[y,Rule[a_,b_]:>Rule[camelCase[a],b]]]];
		result = ReplaceAll[result,Rule["Start",{b___,Rule["DateTime",dt_],a___}]:>Rule["Start",{b,Rule["DateTime",DateObject[StringReplace[dt,r:RegularExpression["\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}\\w?"] ~~ ___ :> r]]],a}]];
		result = ReplaceAll[result,Rule["End",y_]:>Rule["End",ReplaceAll[y,Rule[a_,b_]:>Rule[camelCase[a],b]]]];
		result = ReplaceAll[result,Rule["End",{b___,Rule["DateTime",dt_],a___}]:>Rule["End",{b,Rule["DateTime",DateObject[StringReplace[dt,r:RegularExpression["\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}\\w?"] ~~ ___ :> r]]],a}]];
		result = ReplaceAll[result,Rule["Organizer",y_]:>Rule["Organizer",ReplaceAll[y,Rule[a_,b_]:>Rule[camelCase[a],b]]]];
		result = ReplaceAll[result,Rule["Attendees",y_]:>Rule["Attendees",(ReplaceAll[#,Rule[a_,b_]:>Rule[camelCase[a],b]]&/@y)]];
		result = ReplaceAll[result,Rule["Etag",y_]:>Rule["ETag",y]];
		
		Association[result]		
]

(* Send Message *)

googlecalendarsendmessage[___]:=$Failed

(*** Utilities ***)
formatresults[rawdata_] := ImportString[ToString[rawdata,CharacterEncoding->"UTF-8"],"JSON"]
filterparameters=OAuthClient`Private`filterParameters;
camelCase[text_] := Module[{split, partial}, (
    split = StringSplit[text, {" ","_","-"}];
    partial = Prepend[Rest[Characters[#]], ToUpperCase[Characters[#][[1]]]] & /@ split;
    partial = StringJoin[partial];
    partial = StringReplace[partial,RegularExpression["[Uu][Rr][Ll]"]->"URL"];
    partial
    )]
fp=OAuthClient`Private`formatpath;

getallparameters[str_]:=DeleteCases[Flatten[{"Parameters","PathParameters","BodyData","MultipartData"}/.googlecalendardata[str]],
	("Parameters"|"PathParameters"|"BodyData"|"MultipartData")]

End[] (* End Private Context *)
           		
End[]


SetAttributes[{},{ReadProtected, Protected}];

(* Return three functions to define oauthservicedata, oauthcookeddata, oauthsendmessage  *)
{GoogleCalendar`Private`googlecalendardata,GoogleCalendar`Private`googlecalendarcookeddata,GoogleCalendar`Private`googlecalendarsendmessage}
