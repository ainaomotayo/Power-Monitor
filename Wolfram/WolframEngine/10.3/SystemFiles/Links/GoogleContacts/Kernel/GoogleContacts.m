Begin["GoogleContacts`"] (* Begin Private Context *) 

Begin["`Private`"](* Begin Private Context *) 

(******************************* GoogleContacts *************************************)

(* Authentication information *)

googlecontactsdata[]={
		"OAuthVersion"		->"2.0",
		"ServiceName"       -> "GoogleContacts",
	    "AuthorizeEndpoint" -> "https://accounts.google.com/o/oauth2/auth",
	    "AccessEndpoint"    -> "https://accounts.google.com/o/oauth2/token",
	    "RedirectURI"       -> "urn:ietf:wg:oauth:2.0:oob",
	    "VerifierLabel"      -> "code",
	 	"ClientInfo"		-> {"745766852669-d32c30e6imf6o4uerv5hr40kjgprudvv.apps.googleusercontent.com","Pvu4Bu1cKg-0Wg6r-oGaqh-7"},
	 	"AuthenticationDialog" :> (OAuthClient`tokenOAuthDialog[#, "GoogleContacts"]&),
	 	"Gets"				-> {"ContactsList","ContactsDataset","GroupList","GroupDataset","ContactInformation"},
	 	"Posts"				-> {},
	 	"RawGets"			-> {"RawContacts","RawContactDetails","RawGroups"},
	 	"RawPosts"			-> {},
	 	"Scope"				-> {"https%3A%2F%2Fwww.google.com%2Fm8%2Ffeeds+https%3A%2F%2Fwww.googleapis.com%2Fauth%2Fcontacts.readonly"},
 		"Information"		-> "A service for receiving data from Google Contacts"
}

(* a function for importing the raw data - usually json or xml - from the service *)
googlecontactsimport[$Failed]:=Throw[$Failed]
(*googlecontactsimport[json_String]:=With[{res=ImportString[json,"JSON"]},
	If[FreeQ[res,_["error",_]],
		Association@res,
		Message[ServiceExecute::apierr,"message"/.("error"/.res)];
		Throw[$Failed]
	]
]*)

googlecontactsimport[raw_]:=raw


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
googlecontactsdata["RawContacts"] = {
        "URL"					-> (ToString@StringForm["https://www.google.com/m8/feeds/contacts/`1`/full",#]&),
        "PathParameters"		-> {"userEmail"},
        "Parameters"			-> {"max-results","start-index","updated-min","alt","q","orderby","showdeleted","requirealldeleted","sortorder","group"},
        "RequiredParameters"	-> {"userEmail"},
        "HTTPSMethod"			-> "GET",
        "Headers"				-> {"GData-Version"->"3.0"},
        "ResultsFunction"		-> googlecontactsimport
    }

googlecontactsdata["RawContactDetails"] = {
        "URL"					-> (ToString@StringForm["https://www.google.com/m8/feeds/contacts/`1`/full/`2`",##]&),
        "PathParameters"		-> {"userEmail","contactID"},
        "Parameters"			-> {"alt"},
        "RequiredParameters"	-> {"userEmail","contactID"},
        "HTTPSMethod"			-> "GET",
        "Headers"				-> {"GData-Version"->"3.0"},
        "ResultsFunction"		-> googlecontactsimport
    }

googlecontactsdata["RawGroups"] = {
        "URL"					-> (ToString@StringForm["https://www.google.com/m8/feeds/groups/`1`/full",#]&),
        "PathParameters"		-> {"userEmail"},
        "Parameters"			-> {"max-results","start-index","updated-min","alt","q","orderby","showdeleted","requirealldeleted","sortorder"},
        "RequiredParameters"	-> {"userEmail"},
        "HTTPSMethod"			-> "GET",
        "Headers"				-> {"GData-Version"->"3.0"},
        "ResultsFunction"		-> googlecontactsimport
    }

$GooglePlusRefreshAPI="https://www.wolframcloud.com/objects/user-fa95220f-871c-4331-84ab-7951dd0666ca/googlecontactsrefresh";
googlecontactsdata["RefreshAccessTokenFunction"]:=(
	ToExpression[
		URLFetch[$GooglePlusRefreshAPI, 
  			"Parameters" -> {"refreshtoken" -> ToString[#,InputForm], 
    		"AccessEndpoint" -> ("AccessEndpoint"/.googlecontactsdata[])},
   			"VerifyPeer" -> False]]&)/;OAuthClient`Private`$OAuthCloudCredentialsQ
   			
googlecontactsdata["RefreshAccessTokenFunction"]:=(Block[{url,info,res, data,key, time},
	If[#===None,Return[$Failed]];
	info=OAuthClient`Private`getclientinfo["GooglePlus"];
	url={"AccessEndpoint"/.googlecontactsdata[],
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

googlecontactsdata[___]:=$Failed
(****** Cooked Properties ******)
  
(* cooked data queries 
	Cooked queries call at least one of the raw queried underneath but add pre and post processing to make the function easier to use and the result clearer.
*)  

googlecontactscookeddata[prop_,id_,rules___Rule]:=googlecontactscookeddata[prop,id,{rules}]  

(* Cooked *)
googlecontactscookeddata[prop:("ContactsList"|"ContactsDataset"), id_, args_] := Module[{params={},date,query,sort,sd,group,rawdata,invalidParameters,limit,defaultPerPage=25,maxPerPage=250,startIndex,
											calls,residual,progress,data,fieldnames,orderList,result,totalResults,items={}},
		invalidParameters = Select[Keys[args],!MemberQ[{"MaxItems","StartIndex","UpdatedDate","Query","SortBy","ShowDeleted","GroupID"},#]&]; 
	
		If[Length[invalidParameters]>0,
		(
			Message[ServiceObject::noget,#,prop]&/@invalidParameters;
			Throw[$Failed]
		)];	
		
		params = Join[params,{"userEmail"->"default","alt"->"json"}];
		
		If[KeyExistsQ[args,"UpdatedDate"],
		(
			date = "UpdateDate" /. args;
			date = DateString[date, "ISODateTime"];
			params = Append[params,"updated-min"->date];			
		)];
		
		If[KeyExistsQ[args,"Query"],
		(
			query = "Query" /. args;
			params = Append[params,"q"->query];			
		)];
		
		If[KeyExistsQ[args,"SortBy"],
		(
			sort = "SortBy" /. args;
			Switch[sort,
				"LastModified",
				params = Append[params,"orderby"->"lastmodified"],
				_,
				(
					Message[ServiceExecute::nval,"SortBy","GoogleContacts"];	
					Throw[$Failed]
				)
			];			
		)];
	
		If[KeyExistsQ[args,"ShowDeleted"],
		(
			sd = "ShowDeleted" /. args;
			Switch[sd,
				True,
				params = Append[params,"showdeleted"->"true"],
				False,
				params = Append[params,"showdeleted"->"false"],
				_,
				(
					Message[ServiceExecute::nval,"ShowDeleted","GoogleContacts"];	
					Throw[$Failed]
				)
			];		
		)];
		
		If[KeyExistsQ[args,"GroupID"],
		(
			group = "GroupID" /. args;
			params = Append[params,"group"->group];			
		)];
		
		If[KeyExistsQ[args,"MaxItems"],
		(
			limit = "MaxItems" /. args;
			If[!IntegerQ[limit],
			(	
				Message[ServiceExecute::nval,"MaxItems","GoogleContacts"];
				Throw[$Failed]
			)];						
		),
			limit = defaultPerPage;
		];
	
		If[KeyExistsQ[args,"StartIndex"],
		(
			startIndex = "StartIndex" /. args;
			If[!IntegerQ[startIndex],
			(	
				Message[ServiceExecute::nval,"StartIndex","GoogleContacts"];
				Throw[$Failed]
			)];
		),
			startIndex = 1		
		];
		
		calls = Quotient[limit, maxPerPage];	
		residual = limit - (calls*maxPerPage);
	
		params = Join[params,{"max-results"->ToString[maxPerPage], "start-index"->ToString[startIndex]}];
	
		(* this prints the progress indicator bar *)
		PrintTemporary[ProgressIndicator[Dynamic[progress], {0, calls}]];
	
		If[calls > 0,
		(
			(	
				params = ReplaceAll[params,Rule["start-index",_] -> Rule["start-index",ToString[startIndex+#*maxPerPage]]];
				
				rawdata = OAuthClient`rawoauthdata[id,"RawContacts",params];
				data = formatresults[rawdata];
				

				If[KeyExistsQ[data,"error"],
				(
					Message[ServiceExecute::serrormsg,"message"/.data];
					Throw[$Failed]
				)];
				
				totalResults = FromDigits["$t" /. ("openSearch$totalResults" /. ("feed" /. data))];
				items = Join[items, If[totalResults>0,("entry"/.("feed"/.data)),{}]];	
				progress = progress + 1;	
			)& /@ Range[0,calls-1];		
		
		)];
	
		If[residual > 0,
		(
			params = ReplaceAll[params,Rule["start-index",_] -> Rule["start-index",ToString[startIndex+calls*maxPerPage]]];
			params = ReplaceAll[params,Rule["max-results",_] -> Rule["max-results",ToString[residual]]];
			
			rawdata = OAuthClient`rawoauthdata[id,"RawContacts",params];
			data = formatresults[rawdata];
			
	
			If[KeyExistsQ[data,"error"],
			(
				Message[ServiceExecute::serrormsg,"message"/.data];
				Throw[$Failed]
			)];
			
			totalResults = FromDigits["$t" /. ("openSearch$totalResults" /. ("feed" /. data))];
			items = Join[items, If[totalResults>0,("entry"/.("feed"/.data)),{}]];
		)];
	
		result = items[[1;;Min[limit,Length[items]]]];
	
		fieldnames = {"id","updated","title","gd$email","gd$organization","gd$phoneNumber","gd$postalAddress"};
		result = FilterRules[#, fieldnames] & /@ result;
		
		orderList = Thread[fieldnames -> Range[Length[fieldnames]]];
   		result = Function[r,SortBy[r, (#[[1]] /. orderList&)]]/@result;
   
		result = ReplaceAll[result,Rule["id",y_]:>Rule["ID",Last[StringSplit["$t"/.y,"/"]]]];
		result = ReplaceAll[result,Rule["updated",y_]:>Rule["Updated",DateObject["$t"/.y]]];
		result = ReplaceAll[result,Rule["title",y_]:>Rule["Title","$t"/.y]];
		result = ReplaceAll[result,Rule["gd$email",y_]:>Rule["Email","address"/.y]];
		result = ReplaceAll[result,Rule["gd$organization",y_]:>Rule["Organization",FilterRules[#,{"gd$orgTitle","gd$orgName"}]&/@y]];
		result = ReplaceAll[result,Rule["gd$phoneNumber",y_]:>Rule["PhoneNumber","$t"/.y]];
		result = ReplaceAll[result,Rule["gd$postalAddress",y_]:>Rule["PostalAddress","$t"/.y]];
		
		If[prop=="ContactsList",
			result,
			(
				If[Length[result]==0,
					Dataset[Association[]],
					Dataset[Association /@ result]
				]	
			)
		]	
]

googlecontactscookeddata[prop:("GroupList"|"GroupDataset"), id_, args_] := Module[{params={},date,query,sort,sd,group,rawdata,invalidParameters,limit,defaultPerPage=25,maxPerPage=250,startIndex,
											calls,residual,progress,data,fieldnames,orderList,result,totalResults,items={}},
		invalidParameters = Select[Keys[args],!MemberQ[{"MaxItems","StartIndex","UpdatedDate","Query","SortBy","ShowDeleted"},#]&]; 
	
		If[Length[invalidParameters]>0,
		(
			Message[ServiceObject::noget,#,prop]&/@invalidParameters;
			Throw[$Failed]
		)];	
		
		params = Join[params,{"userEmail"->"default","alt"->"json"}];
		
		If[KeyExistsQ[args,"UpdatedDate"],
		(
			date = "UpdateDate" /. args;
			date = DateString[date, "ISODateTime"];
			params = Append[params,"updated-min"->date];			
		)];
		
		If[KeyExistsQ[args,"Query"],
		(
			query = "Query" /. args;
			params = Append[params,"q"->query];			
		)];
		
		If[KeyExistsQ[args,"SortBy"],
		(
			sort = "SortBy" /. args;
			Switch[sort,
				"LastModified",
				params = Append[params,"orderby"->"lastmodified"],
				_,
				(
					Message[ServiceExecute::nval,"SortBy","GoogleContacts"];	
					Throw[$Failed]
				)
			];			
		)];
	
		If[KeyExistsQ[args,"ShowDeleted"],
		(
			sd = "ShowDeleted" /. args;
			Switch[sd,
				True,
				params = Append[params,"showdeleted"->"true"],
				False,
				params = Append[params,"showdeleted"->"false"],
				_,
				(
					Message[ServiceExecute::nval,"ShowDeleted","GoogleContacts"];	
					Throw[$Failed]
				)
			];		
		)];
		
		If[KeyExistsQ[args,"MaxItems"],
		(
			limit = "MaxItems" /. args;
			If[!IntegerQ[limit],
			(	
				Message[ServiceExecute::nval,"MaxItems","GoogleContacts"];
				Throw[$Failed]
			)];						
		),
			limit = defaultPerPage;
		];
	
		If[KeyExistsQ[args,"StartIndex"],
		(
			startIndex = "StartIndex" /. args;
			If[!IntegerQ[startIndex],
			(	
				Message[ServiceExecute::nval,"StartIndex","GoogleContacts"];
				Throw[$Failed]
			)];
		),
			startIndex = 1		
		];
		
		calls = Quotient[limit, maxPerPage];	
		residual = limit - (calls*maxPerPage);
	
		params = Join[params,{"max-results"->ToString[maxPerPage], "start-index"->ToString[startIndex]}];
	
		(* this prints the progress indicator bar *)
		PrintTemporary[ProgressIndicator[Dynamic[progress], {0, calls}]];
	
		If[calls > 0,
		(
			(	
				params = ReplaceAll[params,Rule["start-index",_] -> Rule["start-index",ToString[startIndex+#*maxPerPage]]];
			
				rawdata = OAuthClient`rawoauthdata[id,"RawGroups",params];
				data = formatresults[rawdata];
				

				If[KeyExistsQ[data,"error"],
				(
					Message[ServiceExecute::serrormsg,"message"/.data];
					Throw[$Failed]
				)];
				
				totalResults = FromDigits["$t" /. ("openSearch$totalResults" /. ("feed" /. data))];
				items = Join[items, If[totalResults>0,("entry"/.("feed"/.data)),{}]];	
				progress = progress + 1;	
			)& /@ Range[0,calls-1];		
		
		)];
	
		If[residual > 0,
		(
			params = ReplaceAll[params,Rule["start-index",_] -> Rule["start-index",ToString[startIndex+calls*maxPerPage]]];
			params = ReplaceAll[params,Rule["max-results",_] -> Rule["max-results",ToString[residual]]];
		
			rawdata = OAuthClient`rawoauthdata[id,"RawGroups",params];
			data = formatresults[rawdata];
			
	
			If[KeyExistsQ[data,"error"],
			(
				Message[ServiceExecute::serrormsg,"message"/.data];
				Throw[$Failed]
			)];
			
			totalResults = FromDigits["$t" /. ("openSearch$totalResults" /. ("feed" /. data))];
			items = Join[items, If[totalResults>0,("entry"/.("feed"/.data)),{}]];
		)];
	
		result = items[[1;;Min[limit,Length[items]]]];
			
		fieldnames = {"id","updated","title"};
		result = FilterRules[#, fieldnames] & /@ result;
		
		orderList = Thread[fieldnames -> Range[Length[fieldnames]]];
   		result = Function[r,SortBy[r, (#[[1]] /. orderList&)]]/@result;
   
		result = ReplaceAll[result,Rule["id",y_]:>Rule["ID","$t"/.y]];
		result = ReplaceAll[result,Rule["updated",y_]:>Rule["Updated",DateObject["$t"/.y]]];
		result = ReplaceAll[result,Rule["title",y_]:>Rule["Title","$t"/.y]];
		
		If[prop=="GroupList",
			result,
			(
				If[Length[result]==0,
					Dataset[Association[]],
					Dataset[Association /@ result]
				]	
			)
		]	
]

googlecontactscookeddata["ContactInformation", id_, args_] := Module[{rawdata, invalidParameters,cId,fieldnames,orderList,result},
		invalidParameters = Select[Keys[args],!MemberQ[{"ContactID"},#]&]; 
	
		If[Length[invalidParameters]>0,
		(
			Message[ServiceObject::noget,#,"ContactInformation"]&/@invalidParameters;
			Throw[$Failed]
		)];	
	
		If[KeyExistsQ[args,"ContactID"],
			cId = "ContactID" /. args,
			(
				Message[ServiceExecute::nparam,"ContactID"];			
				Throw[$Failed]
			)
		];
		
		rawdata = OAuthClient`rawoauthdata[id,"RawContactDetails",{"userEmail"->"default","alt"->"json","contactID"->ToString[cId]}];
		rawdata = formatresults[rawdata];
		rawdata = "entry" /. rawdata;
		
		fieldnames = {"id","updated","title","gd$email","gd$organization","gd$phoneNumber","gd$postalAddress","gd$deleted"};
		rawdata = FilterRules[rawdata,fieldnames];
		orderList = Thread[fieldnames -> Range[Length[fieldnames]]];
   	
   		result = SortBy[rawdata, (#[[1]] /. orderList&)];
   		
		result = ReplaceAll[result,Rule["id",y_]:>Rule["ID",Last[StringSplit["$t"/.y,"/"]]]];
		result = ReplaceAll[result,Rule["updated",y_]:>Rule["Updated",DateObject["$t"/.y]]];
		result = ReplaceAll[result,Rule["title",y_]:>Rule["Title","$t"/.y]];
		result = ReplaceAll[result,Rule["gd$email",y_]:>Rule["Email","address"/.y]];
		result = ReplaceAll[result,Rule["gd$organization",y_]:>Rule["Organization",FilterRules[#,{"gd$orgTitle","gd$orgName"}]&/@y]];
		result = ReplaceAll[result,Rule["gd$phoneNumber",y_]:>Rule["PhoneNumber","$t"/.y]];
		result = ReplaceAll[result,Rule["gd$postalAddress",y_]:>Rule["PostalAddress","$t"/.y]];
		result = ReplaceAll[result,Rule["gd$deleted",y_]:>Rule["Deleted",y]];
		
		Association[result]		
]

(* Send Message *)

googlecontactssendmessage[___]:=$Failed

(*** Utilities ***)
formatresults[rawdata_] := ImportString[ToString[rawdata,CharacterEncoding->"UTF-8"],"JSON"]
filterparameters=OAuthClient`Private`filterParameters;
camelcase=OAuthClient`Private`camelCase;
fp=OAuthClient`Private`formatpath;

getallparameters[str_]:=DeleteCases[Flatten[{"Parameters","PathParameters","BodyData","MultipartData"}/.googlecontactsdata[str]],
	("Parameters"|"PathParameters"|"BodyData"|"MultipartData")]

End[] (* End Private Context *)
           		
End[]


SetAttributes[{},{ReadProtected, Protected}];

(* Return three functions to define oauthservicedata, oauthcookeddata, oauthsendmessage  *)
{GoogleContacts`Private`googlecontactsdata,GoogleContacts`Private`googlecontactscookeddata,GoogleContacts`Private`googlecontactssendmessage}
