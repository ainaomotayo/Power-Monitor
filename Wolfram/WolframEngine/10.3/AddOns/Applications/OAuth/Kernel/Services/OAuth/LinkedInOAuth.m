
System`Private`NewContextPath[{"OAuthClient`","System`"}];

Begin["LinkedInOAuth`"] (* Begin Private Context *) 

Begin["`Private`"](* Begin Private Context *) 

(******************************* LinkedIn *************************************)

(* Authentication information *)

linkedindata[]={
		"OAuthVersion"		->"2.0",
		"ServiceName" 		-> "LinkedIn", 
	 	"AuthorizeEndpoint" -> "https://www.linkedin.com/uas/oauth2/authorization", 
     	"AccessEndpoint"    -> "https://www.linkedin.com/uas/oauth2/accessToken",
     	"RedirectURI"       -> "https://www.wolfram.com/oauthlanding?service=LinkedIn",
     	"VerifierLabel"     -> "code",
	 	"ClientInfo"		-> {"Wolfram","Token"},
	 	"AuthenticationDialog" :> (OAuthClient`tokenOAuthDialog[#, "LinkedIn",liicon]&),
	 	(*"Gets"				-> {"UserData","Connections","ConnectionIDs","EgoNetwork","GroupNames" (*,"UserSearch" *)},*)
	 	"Gets"				-> {"UserData"},
	 	"Posts"				-> {"Share"},
	 	(*"RawGets"			-> {"RawUserData",(* "RawPeopleSearch", *)"RawUserGroups","RawUserGroup","RawGroups","RawGroupPosts","RawSuggestedGroups","RawConnections"},*)
	 	"RawGets"			-> {"RawUserData"},
	 	(*"RawPosts"			-> {"RawJoinGroup","RawShare"},*)
	 	"RawPosts"			-> {"RawShare"},
	 	"RequestFormat"		-> (Block[{params=Cases[{##},("Parameters"->x_):>x,Infinity], 
	 		url=DeleteCases[{##},"Parameters"->_,Infinity],
	 		method=Cases[{##},("Method"->x_):>x,Infinity]},
	 		If[method==={"GET"},
	 			URLFetch@@({Sequence@@url, "Parameters"->Flatten[{params,"format"->"json"}]}/."access_token"->"oauth2_access_token"),
	 			url[[1]]=StringReplace[URLBuild[url[[1]], {"access_token" -> ("access_token" /. (params))}],"access_token"->"oauth2_access_token"];
	 			URLFetch@@{Sequence@@url, "Parameters"->Flatten[{Normal@KeyDrop[params,"access_token"]}]}
	 		]
	 	]&),
	 	"Scope"				-> {"r_basicprofile+w_share"},
	 	(*"Scope"				-> {"r_basicprofile+r_fullprofile+r_network+rw_nus+rw_groups+r_contactinfo"},*)
 		"Information"		-> "A service for receiving data from a LinkedIn account"
}

(* a function for importing the raw data - usually json or xml - from the service *)
linkedinimport[$Failed]:=Throw[$Failed]
linkedinimport[json_String]:=With[{res=ImportString[json,"JSON"]},
	If[FreeQ[res,"errorCode"],
		Association@res,
		Message[ServiceExecute::apierr,"message"/.res];
		Throw[$Failed]
	]
]

linkedinimportxml[$Failed]:=Throw[$Failed]
linkedinimportxml[json_String]:=Module[{res=ImportString[json,"XML"],msg=""},
	If[FreeQ[res,_["error",___]],
		res,
		(
			msg = Cases[res,XMLElement["message",__],Infinity];
			msg = If[Length[msg]>0,msg[[1]] /. XMLElement["message", _, m_] :> m,""];
			msg = If[MatchQ[Head[msg], List], If[Length[msg] > 0, msg[[1]], ""], ""];
			Message[ServiceExecute::apierr,msg];
			Throw[$Failed]
		)
	]
]

linkedinimport[raw_]:=raw
linkedinimportxml[raw_]:=raw



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
linkedindata["RawUserData"] = {
        "URL"				-> (ToString@StringForm["https://api.linkedin.com/v1/people/`1`:`2`",fp[{formatuserid,formatuserfields},##]]&),
        "PathParameters"		-> {"UserID","Fields"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> linkedinimport
    }
(*
linkedindata["RawPeopleSearch"] = {
        "URL"				->  (ToString@StringForm["https://api.linkedin.com/v1/people-search:`1`",formatusersearchfields[##]]&),
        "Parameters" 		-> {"keywords", "first-name", "last-name", "school-name", "company-name","current-company","title","current-title","postal-code","distance","count","sort"},
        "RequiredParameters" 		-> {"keywords"|"first-name"|"last-name"|"school-name"|"company-name"|"current-company"|"title"|"current-title"|"postal-code"|"distance"},
        "PathParameters"		-> {"Fields"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> linkedinimport
    }
   *)
linkedindata["RawConnections"] = {
        "URL"				-> (ToString@StringForm["https://api.linkedin.com/v1/people/`1`/connections:`2`",fp[{formatuserid,formatuserfields},##]]&),
        "PathParameters"	-> {"UserID","Fields"},
        "Parameters" 		-> {"start", "count", "modified", "modified-since"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> linkedinimport
    }
 
linkedindata["RawUserGroups"] = {
        "URL"				-> (ToString@StringForm["https://api.linkedin.com/v1/people/`1`/group-memberships:`2`",fp[{formatuserid,formatgroupmfields},##]]&),
        "PathParameters"	-> {"UserID","Fields"},
        "Parameters" 		-> {"start", "count","membership-state"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> linkedinimport
    }
 
linkedindata["RawUserGroup"] = {
        "URL"				-> (ToString@StringForm["https://api.linkedin.com/v1/people/`1`/group-memberships/`2`:`3`",
        	fp[{formatuserid,formatuserfields,formatgroupmfields},##]]&),
        "Parameters" 		-> {"start", "count","group-id","person-id"},
        "PathParameters"	-> {"UserID","GroupID","Fields"},
        "RequiredParameters" -> {"GroupID"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> linkedinimport
    }
 
(** Groups **)   
 linkedindata["RawGroups"] = {
        "URL"				-> (ToString@StringForm["https://api.linkedin.com/v1/groups/`1`:`2`",fp[{formatgroupid,formatgroupfields},##]]&),
        "PathParameters"	-> {"GroupID","Fields"},
        "RequiredParameters" -> {"GroupID"},
        "Parameters" 		-> {"start", "count"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> linkedinimport
    }

linkedindata["RawGroupPosts"] = {
        "URL"				-> (ToString@StringForm["https://api.linkedin.com/v1/groups/`1`/posts:`2`",fp[{formatgroupid,formatgrouppostfields},##]]&),
        "PathParameters"	-> {"GroupID","Fields"},
        "RequiredParameters" -> {"GroupID"},
        "Parameters" 		-> {"start", "count"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> linkedinimport
    }

linkedindata["RawJoinGroup"] = {
        "URL"				-> (ToString@StringForm["https://api.linkedin.com/v1/people/`1`/group-memberships/`2`",fp[{formatuserid,formatgroupmfields},##]]&),
        "PathParameters"	-> {"UserID","GroupID"},
        "RequiredParameters" -> {"GroupID"},
        "HTTPSMethod"		-> "POST",
        "ResultsFunction"	-> linkedinimport
    }

linkedindata["RawSuggestedGroups"] = {
        "URL"				-> (ToString@StringForm["https://api.linkedin.com/v1/people/`1`/suggestions/groups:`2`",fp[{formatuserid,formatgroupfields},##]]&),
        "PathParameters"	-> {"UserID","Fields"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> linkedinimport
    }

linkedindata["RawShare"] = {
        "URL"				-> "https://api.linkedin.com/v1/people/~/shares",
        "BodyData"			-> {"ParameterlessBodyData"},
        "HTTPSMethod"		-> "POST",
      	"Headers" 			-> {"Content-Type" -> "application/xml"},
        "ResultsFunction"	-> linkedinimportxml
    }     
    
linkedindata["icon"]=liicon

linkedindata[___]:=$Failed
(****** Cooked Properties ******)
  
(* cooked data queries 
	Cooked queries call at least one of the raw queried underneath but add pre and post processing to make the function easier to use and the result clearer.
*)  

linkedincookeddata[prop_,id_,rules___Rule]:=linkedincookeddata[prop,id,{rules}]
  
(* Cooked *)
linkedincookeddata["Share",id_,args_]:=Module[{params,rawdata, data},
	params=toxmlcomment[args];
	rawdata=OAuthClient`rawoauthdata[id,"RawShare",params];
	data=linkedinimportxml[rawdata];
	data=Cases[data,XMLElement["update-key", __]|XMLElement["update-url", {}, {_}],Infinity];
	If[data==={},
		$Failed,
		data/.XMLElement[a_,_,b_]:>Rule[camelcase[a,{"-","_"}],b]
	]
]

userdatafields={"id", "formatted-name", "headline", "industry", "distance", 
"current-status", "current-share", "num-connections","picture-url", "public-profile-url"};
	
linkedincookeddata["UserData",id_,args_]:=Module[{params,rawdata, data},
	params=filterparameters[args,getallparameters["RawUserData"],{"-","_"}];
	params=If[FreeQ[params,"UserID"->_],
		Flatten[{params,"UserID"->"~","Fields"->userdatafields}],
		Flatten[{params,"Fields"->userdatafields}]
		];
	rawdata=OAuthClient`rawoauthdata[id,"RawUserData",params];
	data=Normal@linkedinimport[rawdata];
	Association[Replace[Thread[userdatafields->((first[getcamelcased[data,#]]&/@userdatafields))],
		HoldPattern[Rule[a_,b_]]:>Rule[camelcase[a,{"-","_"}],b],Infinity]]
]
(*
linkedincookeddata["UserSearch",id_,args_]:=Module[{params,rawdata, data,values},
	params=filterparameters[args,getallparameters["RawPeopleSearch"],{"-","_"}];
	params=params/.HoldPattern[Rule[a:Except["Fields"],b_?(!StringQ[#]&)]]:>Rule[a,ToString[b]];
	rawdata=OAuthClient`rawoauthdata[id,"RawPeopleSearch",params];
	data=Normal@linkedinimport[rawdata];
	data=Lookup[data,"people",{}];
	If[data=!={},
		values="values"/.data/."values"->{};
		values=Replace[values,HoldPattern[Rule[a_,b_]]:>Rule[camelcase[a,{"-","_"}],b],Infinity];
		If[values=!={},
			Association/@values,
			{}
		]
		,
		{}
	]
]
*)


linkedincookeddata[prop:("Connections"|"ConnectionIDs"),id_,args_]:=Module[{params,rawdata, data, field},
	field=If[prop==="Connections","formatted-name","id"];
	params=filterparameters[args,getallparameters["RawConnections"],{"-","_"}];
	params=If[FreeQ[params,"UserID"->_],
		Flatten[{params,"UserID"->"~","Fields"->{field}}],
		Flatten[{params,"Fields"->{field}}]
		];
	params=params/.HoldPattern[Rule[a:Except["Fields"],b_?(!StringQ[#]&)]]:>Rule[a,ToString[b]];
	rawdata=OAuthClient`rawoauthdata[id,"RawConnections",params];
	data=linkedinimport[rawdata];
	Flatten[getcamelcased[Lookup[data,"values",{field->{}}],field,Infinity]]
]
	
linkedincookeddata["EgoNetwork",id_,args_]:=Module[{names,ids,root, rawdata,edges,vertices,labels,res,pos},
	rawdata=OAuthClient`rawoauthdata[id,"RawConnections",Join[args,{"Fields"->{"id","formatted-name"}}]];
	rawdata=linkedinimport[rawdata];
	rawdata=Lookup[rawdata,"values",{"formatted-name"->{},"id"->{}}];
	names=first[getcamelcased[#,"formatted-name"]]&/@rawdata;
	ids=first[getcamelcased[#,"id"]]&/@rawdata;
	rawdata=linkedincookeddata["UserData",id];
	root=Lookup[rawdata,#,""]&/@{"FormattedName","ID"};
	pos = Union[Flatten@Join[Position[ids, "private"], Position[names, "private"]]];
	ids = ReplacePart[ids, Thread[pos -> Sequence[]]];
	names = ReplacePart[names, Thread[pos -> Sequence[]]];
	edges = DirectedEdge[root[[2]], #] & /@ ids;
	ids = Join[{root[[2]]}, ids];
	names = Join[{root[[1]]}, names];
    {vertices,labels} =
        Transpose[MapThread[{Property[#1, "name" -> #2] , (#1 -> Placed[#2, Tooltip])} &, {ids, names}]];
    res = Graph[vertices, edges, VertexLabels -> labels, GraphLayout -> "StarEmbedding"];
    res /; GraphQ[res]
]


linkedincookeddata["GroupNames",id_,args_]:=Module[
	{rawdata,groups, params},
	params=filterparameters[args,getallparameters["RawUserGroups"],{"-","_"}];
	params=params/.HoldPattern[Rule[a:Except["Fields"],b_?(!StringQ[#]&)]]:>Rule[a,ToString[b]];
	rawdata=OAuthClient`rawoauthdata[id,"RawUserGroups",Flatten[{args,"Fields"->{"group:(id,name)"}}]];
	rawdata=linkedinimport[rawdata];
	groups=Lookup[rawdata,"values",{"group"->{}}];
	groups="group"/.groups;
	groups=DeleteCases[groups,"group",Infinity];
	If[Flatten[groups]==={},Return[{}]];
	Association/@Replace[groups,HoldPattern[Rule[a_,b_]]:>Rule[camelcase[a],b],Infinity] /; (groups =!= $Failed)
]

linkedincookeddata[___]:=$Failed 
(* Send Message *)

linkedinsendmessage[id_,message_String]:=With[{res=linkedincookeddata["Share",id,"Message"->message]},
	If[ListQ[res]&&FreeQ[res,"error"],message,$Failed]
]

linkedinsendmessage[___]:=$Failed

(**** Available fields ****)
(*** Profiles ***)
profilebasicfields={"id","first-name","last-name","maiden_name","formatted-name","phoentic-first-name","phoentic-last-name",
	"formatted-phonetic-name","headline","location:{name}","location:(country:(code))","industry","distance","relation-to-viewer:(distance)",
	"current-share","num-connections","num-connections-capped","summary","specialties","positions","picture-url",
	"site-standard-profile-request","api-standard-profile-request:(url)","api-standard-profile-request:(headers)","public-profile-url"};
profileemailfields={"email-address"};
profilefullfields={"last-modified-timestamp","proposal-comments","associations","interests","publications","patents","languages","skills","certifications",
	"educations","courses","volunteer","three-current-positions","three-past-positions","num-recommenders","recommendations-received","mfeed-rss-url",
	"following","job-bookmarks","suggestions","date-of-birth","member-url-resources","member-url-resources:(url)","member-url-resources:(name)","related-profile-views","honors-awards"};
profilecontactinfofields={"phone-numbers","bound-account-types","im-accounts","main-address","twitter-accounts","primary-twitter-account"};
profileconnectionfields={"connections"};
profilegroupmembershipfields={"group-memberships"};
profilepositionfields={"id","title","summary","start-date","end-date","is-current","company"};
profilecompanyfields={"id","name","type","size","industry","ticker"};
profilepublicationfields={"id","title","publisher:(name)","authors:(id)","authors:(name)","authors:(person)","date","url","summary"};
profilepatentfields={"id","title","summary","number","status:(id)","status:(name)","office:(name)","inventors:(id)","inventors:(name)","inventors:(person)","date","url"};
profilelanguagefields={"id","language:(name)","proficiency:(level)","proficiency:(name)"};
profileskillsfields={"id","skill:(name)"};
profilecertificationfields={"id","name","authority:(name)","number","start-date","end-date"};
profileeducationfields={"id","school-name","field-of-study","start-date","end-date","degree","activities","notes"};
profilecoursesfields={"id","name","number"};
profilevolunteerfields={"id","role","organization:(name)","cause:(name)"};
profilerecommendationfields={"id","recommendation-type","recommendation-text","recommender"};

(*** Groups ***)
groupoutputfields={"id","name","short-description","description","relation-to-viewer:(membership-state)",
	"relation-to-viewer:(available-actions)","posts","counts-by-category","is-open-to-non-members","category",
	"website-url","site-group-url","locale","location:(country)","location:(postal-code)","allow-member-invites"
	,"small-logo-url","large-logo-url","num-members"};

grouppostoutputfields={"id","type","category","creator","title",
	"summary","creation-timestamp","relation-to-viewer:(is-following)","relation-to-viewer:(is-liked)"
	,"relation-to-viewer:(available-actions)","likes","comments","attachment","site-group-post-url"};

groupcommentoutputfields={"id","text","creator","creation-timestamp","relation-to-viewer:(available-actions)"};

groupmembershipoutputfields={"person","group:(id)","group:(name)","membership-state","show-group-logo-in-profile",
	"allow-messages-from-members","email-digest-frequency","email-announcements-from-managers","email-for-every-new-post","posts"};
	
(*** Service specific utilites ****)
filterparameters=OAuthClient`Private`filterParameters;
camelcase=OAuthClient`Private`camelCase;
fp=OAuthClient`Private`formatpath;

readDate[date_, form_: DateString] := 
 form@DateList[{StringSplit[date, {" +"," -"}][[1]], 
 	{"DayName", "Day", "MonthNameShort","Year", "Hour", ":", "Minute", ":",
      "Second"}}]

formatfield[str_String]:=str
formatfield[l_List]:=StringJoin["(",Riffle[l,","],")"]

formatuserid[Automatic|"Me"|"me"]:="~";
formatuserid[id_String]:="id="<>id
formatuserid[]:="~"

formatuserfields[]:="(id,formatted-name,headline,industry,distance,current-status,current-share,num-connections,summary,positions,picture-url,public-profile-url)"
formatuserfields[args___]:=formatfield[args]

formatusersearchfields[]:="(people:(id,first-name,last-name,formatted-name),num-results)"
formatusersearchfields[args___]:=formatfield[args]

formatgroupid[id_String]:=id
formatgroupid[int_Integer]:=ToString[int]
formatgroupid[___]:=(Message[ServiceExecute::nparam,"id"];Throw[$Failed])

formatgroupfields[]:="(id,name,short-description,posts,category,website-url,site-group-url,num-members)"
formatgroupfields[args___]:=formatfield[args]

formatgroupmfields[]:="(person,group:(id,name),membership-state)"
formatgroupmfields[args___]:=formatfield[args]

formatgrouppostfields[]:="(id,type,category,creator,title,summary,creation-timestamp,likes,comments,site-group-post-url)"
formatgrouppostfields[args___]:=formatfield[args]

getallparameters[str_]:=DeleteCases[Flatten[{"Parameters","PathParameters","BodyData","MultipartData"}/.linkedindata[str]],
	("Parameters"|"PathParameters"|"BodyData"|"MultipartData")]

toxmlcomment[args_]:=Block[{message,visibility, opts=Cases[args,_?OptionQ,Infinity]},
	message="Message"/.opts;
	visibility="Visibility"/.opts/."Visibility"->"anyone";
	Flatten@{"ParameterlessBodyData"->toxml["share"->{"comment"->message,"visibility"->{"code"->visibility}}],FilterRules[args,Except["Message"|"Visibility"]]}
]

toxml[rules_] := 
 "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" <> 
  ExportString[
   rules //. Rule[a_, b_] :> XMLElement[a, {}, Flatten[{b}]], "XML"]
  
getcamelcased[data_,key_, rest___]:=With[{cc=camelcase[key,{"_","-"}]},
	Cases[data,((key|cc|((ToLowerCase[StringTake[#,1]]<>StringDrop[#,1])&@cc))->x_):>x,rest]
]

first[{}]={};
first[l_]:=First[l]

liicon=Image[RawArray["Byte", {{{9, 118, 180, 16}, {9, 118, 180, 196}, {9, 119, 181, 255}, {9, 118, 180, 255}, {9, 119, 181, 
  255}, {9, 118, 180, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 119, 
  181, 255}, {9, 118, 180, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 
  119, 181, 255}, {9, 119, 181, 255}, {9, 118, 180, 255}, {9, 118, 180, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, 
  {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 118, 180, 255}, {9, 119, 181, 255}, {9, 118, 180, 255}, {9, 119, 181, 
  255}, {9, 118, 180, 196}, {9, 119, 181, 16}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 
  255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}}, {{9, 
  119, 181, 196}, {9, 119, 181, 255}, {9, 118, 180, 255}, {9, 118, 180, 255}, {9, 119, 181, 255}, {9, 118, 180, 255}, 
  {9, 118, 180, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 119, 181, 
  255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 118, 
  180, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 118, 180, 255}, {9, 118, 180, 255}, {9, 119, 181, 255}, {9, 
  118, 180, 255}, {9, 118, 180, 255}, {9, 118, 180, 255}, {9, 118, 180, 255}, {9, 119, 181, 255}, {9, 118, 180, 255}, 
  {9, 119, 181, 196}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 
  0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}}, {{9, 119, 181, 255}, {9, 119, 
  181, 255}, {9, 119, 181, 255}, {9, 118, 180, 255}, {9, 119, 181, 255}, {9, 118, 180, 255}, {9, 119, 181, 255}, {9, 
  119, 181, 255}, {9, 119, 181, 255}, {9, 118, 180, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 118, 180, 255}, 
  {9, 118, 180, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 118, 180, 255}, {9, 119, 181, 255}, {9, 119, 181, 
  255}, {9, 119, 181, 255}, {9, 118, 180, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 118, 180, 255}, {9, 119, 
  181, 255}, {9, 119, 181, 255}, {9, 118, 180, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, 
  {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 
  0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}}, {{9, 118, 180, 255}, {9, 119, 181, 255}, {9, 119, 
  181, 255}, {9, 119, 181, 255}, {9, 118, 180, 255}, {9, 119, 180, 255}, {9, 118, 180, 255}, {9, 119, 181, 255}, {9, 
  118, 180, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 118, 180, 255}, {9, 118, 180, 255}, {9, 119, 181, 255}, 
  {9, 118, 180, 255}, {9, 119, 181, 255}, {9, 119, 180, 255}, {9, 119, 181, 255}, {9, 118, 180, 255}, {9, 118, 180, 
  255}, {9, 118, 180, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 118, 180, 255}, {9, 118, 180, 255}, {9, 119, 
  181, 255}, {9, 118, 180, 255}, {9, 118, 180, 255}, {9, 118, 180, 255}, {9, 119, 181, 255}, {9, 119, 181, 32}, {255, 
  255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, 
  {255, 255, 255, 0}, {255, 255, 255, 0}}, {{9, 119, 181, 255}, {9, 119, 181, 255}, {9, 118, 180, 255}, {9, 119, 181, 
  255}, {9, 119, 181, 255}, {32, 137, 190, 255}, {127, 186, 217, 255}, {128, 187, 218, 255}, {32, 136, 190, 255}, {9, 
  119, 181, 255}, {9, 119, 181, 255}, {9, 118, 180, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, 
  {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 118, 180, 255}, {9, 119, 181, 255}, {9, 119, 181, 
  255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 118, 180, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 119, 
  181, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {255, 255, 255, 0}, {255, 255, 255, 0}, 
  {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 
  0}, {255, 255, 255, 0}}, {{9, 119, 180, 255}, {9, 118, 180, 255}, {9, 119, 181, 255}, {9, 118, 180, 255}, {32, 137, 
  190, 255}, {240, 247, 250, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {240, 247, 250, 255}, {32, 136, 190, 
  255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 118, 180, 255}, {9, 119, 
  181, 255}, {9, 118, 180, 255}, {9, 119, 181, 255}, {9, 118, 180, 255}, {9, 118, 180, 255}, {9, 118, 180, 255}, {9, 
  119, 181, 255}, {9, 119, 181, 255}, {9, 118, 180, 255}, {9, 119, 181, 255}, {9, 118, 180, 255}, {9, 118, 180, 255}, 
  {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 
  0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 
  255, 0}}, {{9, 119, 181, 255}, {9, 119, 181, 255}, {9, 118, 180, 255}, {9, 119, 181, 255}, {128, 187, 218, 255}, 
  {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {127, 186, 217, 255}, {9, 
  119, 181, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, 
  {9, 119, 181, 255}, {9, 118, 180, 255}, {9, 119, 181, 255}, {9, 118, 180, 255}, {9, 119, 181, 255}, {9, 119, 181, 
  255}, {9, 119, 181, 255}, {9, 118, 180, 255}, {9, 118, 180, 255}, {9, 118, 180, 255}, {9, 119, 181, 255}, {9, 118, 
  180, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, 
  {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 
  0}}, {{9, 119, 181, 255}, {9, 119, 181, 255}, {9, 118, 180, 255}, {9, 118, 180, 255}, {128, 187, 218, 255}, {255, 
  255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {128, 187, 218, 255}, {9, 119, 
  181, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 118, 180, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 
  118, 180, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 118, 180, 255}, {9, 119, 181, 255}, 
  {9, 119, 181, 255}, {9, 118, 180, 255}, {9, 118, 180, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 119, 181, 
  255}, {9, 118, 180, 255}, {9, 118, 180, 255}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 
  255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}}, 
  {{9, 118, 180, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {32, 136, 190, 255}, {240, 247, 
  250, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {240, 247, 250, 255}, {32, 136, 190, 255}, {9, 118, 180, 
  255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 118, 180, 255}, {9, 119, 
  181, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 118, 180, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 
  119, 181, 255}, {9, 118, 180, 255}, {9, 119, 181, 255}, {9, 118, 180, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, 
  {9, 119, 181, 255}, {9, 118, 180, 255}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 
  0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}}, {{9, 119, 
  181, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 118, 180, 255}, {9, 119, 181, 255}, {32, 136, 190, 255}, 
  {128, 187, 218, 255}, {128, 187, 217, 255}, {32, 137, 190, 255}, {9, 118, 180, 255}, {9, 118, 180, 255}, {9, 119, 
  181, 255}, {9, 119, 181, 255}, {9, 118, 180, 255}, {9, 119, 181, 255}, {9, 118, 180, 255}, {9, 118, 180, 255}, {9, 
  118, 180, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, 
  {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 118, 180, 255}, {9, 119, 181, 255}, {9, 118, 180, 
  255}, {9, 119, 181, 255}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 
  255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}}, {{9, 119, 181, 255}, 
  {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 118, 180, 255}, {9, 119, 181, 255}, {9, 119, 181, 
  255}, {9, 118, 180, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 118, 
  180, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 118, 180, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {32, 
  137, 190, 255}, {65, 153, 201, 255}, {64, 153, 200, 255}, {65, 154, 201, 255}, {9, 119, 181, 255}, {9, 119, 181, 
  255}, {9, 118, 180, 255}, {9, 118, 180, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 118, 180, 255}, {9, 119, 
  181, 255}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, 
  {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}}, {{9, 119, 181, 255}, {9, 119, 181, 
  255}, {9, 118, 180, 255}, {9, 119, 181, 255}, {9, 118, 180, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 
  255, 255, 255}, {255, 255, 255, 255}, {9, 118, 180, 255}, {9, 118, 180, 255}, {9, 118, 180, 255}, {255, 255, 255, 
  255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {193, 222, 238, 255}, {9, 118, 180, 255}, {144, 196, 223, 255}, 
  {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {224, 238, 246, 255}, {79, 
  162, 203, 255}, {9, 118, 180, 255}, {9, 118, 180, 255}, {9, 118, 180, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, 
  {9, 118, 180, 255}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 
  0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}}, {{9, 119, 181, 255}, {9, 118, 
  180, 255}, {9, 118, 180, 255}, {9, 118, 180, 255}, {9, 118, 180, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, 
  {255, 255, 255, 255}, {255, 255, 255, 255}, {9, 118, 180, 255}, {9, 118, 180, 255}, {9, 119, 181, 255}, {255, 255, 
  255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {160, 204, 228, 255}, {255, 255, 255, 
  255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, 
  {255, 255, 255, 255}, {48, 145, 195, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 119, 
  181, 255}, {9, 119, 181, 255}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, 
  {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}}, {{9, 119, 181, 
  255}, {9, 118, 180, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {255, 255, 255, 255}, {255, 
  255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 118, 180, 
  255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, 
  {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 
  255, 255, 255}, {255, 255, 255, 255}, {160, 203, 227, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 119, 181, 
  255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 
  255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}}, 
  {{9, 119, 181, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {255, 255, 
  255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {9, 119, 181, 255}, {9, 118, 180, 
  255}, {9, 119, 181, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, 
  {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 
  255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {240, 247, 252, 255}, {9, 119, 181, 255}, {9, 118, 180, 
  255}, {9, 118, 180, 255}, {9, 118, 180, 255}, {9, 119, 181, 255}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 
  255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, 
  {255, 255, 255, 0}}, {{9, 119, 181, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 118, 180, 255}, {9, 119, 181, 
  255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {9, 118, 180, 255}, 
  {9, 119, 181, 255}, {9, 118, 180, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 
  255, 255, 255}, {207, 230, 241, 255}, {33, 137, 191, 255}, {9, 119, 181, 255}, {16, 128, 186, 255}, {176, 213, 231, 
  255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {9, 119, 181, 255}, 
  {9, 119, 181, 255}, {9, 118, 180, 255}, {9, 118, 180, 255}, {9, 119, 181, 255}, {255, 255, 255, 0}, {255, 255, 255, 
  0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 
  255, 0}, {255, 255, 255, 0}}, {{9, 119, 181, 255}, {9, 119, 181, 255}, {9, 118, 180, 255}, {9, 118, 180, 255}, {9, 
  118, 180, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {9, 119, 
  181, 255}, {9, 118, 180, 255}, {9, 119, 181, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 
  255}, {255, 255, 255, 255}, {80, 162, 204, 255}, {9, 119, 181, 255}, {9, 118, 180, 255}, {9, 119, 181, 255}, {64, 
  153, 200, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {9, 119, 
  181, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 118, 180, 255}, {255, 255, 255, 0}, 
  {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 
  0}, {255, 255, 255, 0}, {255, 255, 255, 0}}, {{9, 119, 181, 255}, {9, 118, 180, 255}, {9, 118, 180, 255}, {9, 119, 
  181, 255}, {9, 119, 181, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 
  255}, {9, 119, 181, 255}, {9, 118, 180, 255}, {9, 119, 181, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 
  255, 255, 255}, {255, 255, 255, 255}, {65, 154, 201, 255}, {9, 118, 180, 255}, {9, 119, 181, 255}, {9, 119, 181, 
  255}, {32, 136, 190, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, 
  {9, 119, 180, 255}, {9, 118, 180, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {255, 255, 255, 
  0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 
  255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}}, {{9, 119, 181, 255}, {9, 118, 180, 255}, {9, 119, 181, 255}, {9, 
  119, 181, 255}, {9, 119, 181, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 
  255, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 118, 180, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, 
  {255, 255, 255, 255}, {255, 255, 255, 255}, {9, 118, 180, 255}, {9, 119, 181, 255}, {9, 119, 180, 255}, {9, 118, 
  180, 255}, {9, 119, 180, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 
  255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 118, 180, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {255, 
  255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, 
  {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}}, {{9, 119, 181, 255}, {9, 119, 181, 255}, {9, 119, 181, 
  255}, {9, 119, 180, 255}, {9, 119, 181, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, 
  {255, 255, 255, 255}, {9, 118, 180, 255}, {9, 118, 180, 255}, {9, 118, 180, 255}, {255, 255, 255, 255}, {255, 255, 
  255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 118, 180, 255}, 
  {9, 119, 181, 255}, {9, 119, 181, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 
  255, 255, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 118, 180, 255}, {9, 119, 181, 255}, {9, 118, 180, 255}, 
  {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {0, 0, 0, 26}, {0, 0, 0, 102}, {0, 0, 0, 102}, {0, 0, 
  0, 102}, {0, 0, 0, 26}, {255, 255, 255, 0}}, {{9, 119, 181, 255}, {9, 119, 181, 255}, {9, 118, 180, 255}, {9, 119, 
  181, 255}, {9, 119, 181, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 
  255}, {9, 119, 181, 255}, {9, 118, 180, 255}, {9, 119, 181, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 
  255, 255, 255}, {255, 255, 255, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 119, 181, 
  255}, {9, 118, 180, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, 
  {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {255, 255, 255, 
  0}, {255, 255, 255, 0}, {0, 0, 0, 26}, {0, 0, 0, 102}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, 
  {0, 0, 0, 102}, {0, 0, 0, 26}}, {{9, 119, 181, 255}, {9, 119, 180, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, 
  {9, 119, 181, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {9, 
  119, 181, 255}, {9, 118, 180, 255}, {9, 119, 181, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 
  255}, {255, 255, 255, 255}, {9, 118, 180, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 118, 180, 255}, {9, 
  119, 181, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {9, 119, 
  181, 255}, {9, 118, 180, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 118, 180, 255}, {255, 255, 255, 0}, 
  {255, 255, 255, 0}, {0, 0, 0, 102}, {255, 255, 255, 0}, {0, 0, 0, 102}, {0, 0, 0, 51}, {0, 0, 0, 102}, {255, 255, 
  255, 0}, {0, 0, 0, 102}}, {{9, 119, 181, 255}, {9, 119, 181, 255}, {9, 118, 180, 255}, {9, 119, 181, 255}, {9, 118, 
  180, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {9, 119, 181, 
  255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, 
  {255, 255, 255, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 119, 180, 255}, {9, 118, 180, 
  255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {9, 119, 181, 255}, 
  {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {255, 255, 255, 0}, {255, 255, 255, 
  0}, {0, 0, 0, 102}, {255, 255, 255, 0}, {0, 0, 0, 102}, {0, 0, 0, 102}, {0, 0, 0, 26}, {255, 255, 255, 0}, {0, 0, 
  0, 102}}, {{9, 119, 181, 255}, {9, 119, 181, 255}, {9, 118, 180, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, 
  {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {9, 119, 181, 255}, {9, 
  119, 181, 255}, {9, 119, 181, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 
  255, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 118, 180, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, 
  {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {9, 119, 181, 255}, {9, 
  119, 181, 255}, {9, 119, 181, 255}, {9, 118, 180, 255}, {9, 119, 181, 255}, {255, 255, 255, 0}, {255, 255, 255, 0}, 
  {0, 0, 0, 102}, {255, 255, 255, 0}, {0, 0, 0, 102}, {255, 255, 255, 0}, {0, 0, 0, 102}, {255, 255, 255, 0}, {0, 0, 
  0, 102}}, {{9, 119, 181, 255}, {9, 119, 181, 255}, {9, 118, 180, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, 
  {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {9, 118, 180, 255}, {9, 
  119, 181, 255}, {9, 119, 181, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 
  255, 255}, {9, 119, 181, 255}, {9, 118, 180, 255}, {9, 118, 180, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, 
  {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {9, 119, 181, 255}, {9, 
  118, 180, 255}, {9, 119, 181, 255}, {9, 118, 180, 255}, {9, 119, 181, 255}, {255, 255, 255, 0}, {255, 255, 255, 0}, 
  {0, 0, 0, 26}, {0, 0, 0, 102}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {0, 0, 0, 102}, {0, 0, 
  0, 26}}, {{9, 118, 180, 255}, {9, 119, 181, 255}, {9, 119, 180, 255}, {9, 118, 180, 255}, {9, 119, 181, 255}, {9, 
  119, 181, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, 
  {9, 118, 180, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 118, 180, 
  255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 118, 180, 255}, {9, 118, 
  180, 255}, {9, 118, 180, 255}, {9, 118, 180, 255}, {9, 119, 180, 255}, {9, 118, 180, 255}, {9, 119, 181, 255}, {9, 
  119, 181, 255}, {9, 118, 180, 255}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {0, 0, 0, 26}, {0, 
  0, 0, 102}, {0, 0, 0, 102}, {0, 0, 0, 102}, {0, 0, 0, 26}, {255, 255, 255, 0}}, {{9, 119, 180, 255}, {9, 119, 181, 
  255}, {9, 118, 180, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 119, 180, 255}, {9, 119, 
  181, 255}, {9, 118, 180, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 
  119, 180, 255}, {9, 118, 180, 255}, {9, 119, 181, 255}, {9, 118, 180, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, 
  {9, 118, 180, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 118, 180, 255}, {9, 119, 181, 255}, {9, 119, 181, 
  255}, {9, 118, 180, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {255, 
  255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, 
  {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}}, {{9, 119, 181, 255}, {9, 119, 181, 255}, {9, 119, 181, 
  255}, {9, 119, 181, 255}, {9, 118, 180, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 119, 180, 255}, {9, 118, 
  180, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 
  118, 180, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 118, 180, 255}, {9, 119, 181, 255}, 
  {9, 118, 180, 255}, {9, 119, 181, 255}, {9, 118, 180, 255}, {9, 118, 180, 255}, {9, 119, 181, 255}, {9, 119, 181, 
  255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {255, 255, 255, 0}, {255, 
  255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, 
  {255, 255, 255, 0}, {255, 255, 255, 0}}, {{9, 119, 181, 196}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 119, 181, 
  255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 118, 180, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 118, 
  180, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 118, 180, 255}, {9, 119, 181, 255}, {9, 118, 180, 255}, {9, 
  119, 181, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, 
  {9, 119, 181, 255}, {9, 119, 180, 255}, {9, 119, 181, 255}, {9, 118, 180, 255}, {9, 118, 180, 255}, {9, 119, 181, 
  255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 119, 181, 196}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 
  255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, 
  {255, 255, 255, 0}}, {{9, 118, 180, 16}, {9, 119, 181, 196}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 119, 181, 
  255}, {9, 119, 181, 255}, {9, 118, 180, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 119, 180, 255}, {9, 119, 
  181, 255}, {9, 118, 180, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 118, 180, 255}, {9, 118, 180, 255}, {9, 
  119, 181, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, 
  {9, 119, 180, 255}, {9, 118, 180, 255}, {9, 119, 181, 255}, {9, 119, 181, 255}, {9, 118, 180, 255}, {9, 119, 181, 
  255}, {9, 119, 181, 196}, {9, 119, 181, 16}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 
  255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}}}], 
 "Byte", ColorSpace -> "RGB", Interleaving -> True];


End[] (* End Private Context *)
           		
End[]


SetAttributes[{},{ReadProtected, Protected}];

System`Private`RestoreContextPath[];

(* Return three functions to define oauthservicedata, oauthcookeddata, oauthsendmessage  *)
{LinkedInOAuth`Private`linkedindata,LinkedInOAuth`Private`linkedincookeddata,LinkedInOAuth`Private`linkedinsendmessage}
