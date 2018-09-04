
(* ::Package:: *)

(* $Id$ *)

(* :Summary:
	A framework for authenticating and exchanging data with api-key authenticated services
*)

(* :Mathematica Version: Mathematica 10.0 *)

(* :Keywords:
API Key
*)

(* :Examples:
*)


System`Private`NewContextPath[{"KeyClient`","System`"}];

(Unprotect[#]; Clear[#])& /@ {KeyClient`rawkeydata,
KeyClient`keyauthenticate,
KeyClient`keydisconnect,
KeyClient`keydata
}

KeyClient`rawkeydata;
KeyClient`keyauthenticate;
KeyClient`keydisconnect;
KeyClient`keydata;



Begin["KeyClient`"];

Begin["`Private`"];


keyservicesdata=KeyClient`KeyServicesData;

(* Use the cloud stored api keys *)
$KeyCloudCredentialsQ=False;
ServiceConnections`Private`$keyservices:=KeyClient`$predefinedKeyservicelist;

(* Because the many of the services update thier data frequently (i.e. Twitter) caching is false by default.
	In some places where calls are often repeated, this is set to true *)
KeyClient`$CacheResults=False;

(* Messages *)
ServiceConnect::key1="The `1` service does not save user connections, a standard connection will be created."
ServiceConnect::key2="The `1` service does not create new user connections, a standard connection will be created."

(* Import Functions *)
serviceName=ServiceConnections`Private`serviceName;
getServiceObject=ServiceConnections`Private`getServiceObject;
checkservicelist=ServiceConnections`Private`checkservicelist;
getServiceID=ServiceConnections`Private`getServiceID;
getServiceName=ServiceConnections`Private`getServiceName;
serviceRawRequests=ServiceConnections`Private`serviceRawRequests;
serviceRawPosts=ServiceConnections`Private`serviceRawPosts;
serviceRequests=ServiceConnections`Private`serviceRequests;
servicePosts=ServiceConnections`Private`servicePosts;
logout=ServiceConnections`Private`logout;
urlfetchFun=ServiceConnections`Private`urlfetchFun;
serviceInfo=ServiceConnections`Private`serviceInfo;
debugPrint=ServiceConnections`Private`debugPrint;
serviceAuthentication=ServiceConnections`Private`serviceAuthentication;
formatMultipartData=ServiceConnections`Private`formatMultipartData;

(************************************** API Key Authentication **********************************)

keyauthenticate[]:=Sort[$predefinedKeyservicelist]
keyauthenticate["Services"]:=keyauthenticate[]

keyauthenticate[service_, ___]:=(Message[ServiceConnect::unkn, service];$Failed)/;!MemberQ[$predefinedKeyservicelist,service]

keyauthenticate[name_,"New",rest___?OptionQ]:=(Message[ServiceConnect::key2, name];keyauthenticate[name,rest])

keyauthenticate[name_,_String,rest___?OptionQ]:=(Message[ServiceConnect::key1, name];keyauthenticate[name,rest])


keyauthenticate[name_,rest___]:=newkeyauthenticate[name, rest]

newkeyauthenticate[name_,rest___]:=Module[{service,
	rawgets=keyservicesdata[name, "RawGets"],
	gets=keyservicesdata[name, "Gets"],
	rawposts=keyservicesdata[name, "RawPosts"],
	posts=keyservicesdata[name, "Posts"], id, info,key},
	key=OAuthClient`Private`getclientinfo[name,"APIKey"];
	(*info=If[!MatchQ[key,_String],
		Print["here"];Throw[$Failed],
		Join[{"APIKey"->key},keyservicesdata[name, "Authentication"]]
	];*)
	info=Join[{"APIKey"->key},keyservicesdata[name, "Authentication"]];

	service=newunknownkeyauthenticate[name,Sequence@@info, rest];
	id=getServiceID[service];

	serviceRawRequests[id]=Sort[Flatten[{serviceRawRequests[id],rawgets}]];
	serviceRawPosts[id]=Sort[Flatten[{serviceRawPosts[id],rawposts}]];

	serviceRequests[id]=Sort[Flatten[{serviceRequests[id],gets}]];
	servicePosts[id]=Sort[Flatten[{servicePosts[id],posts}]];

	service
]/;MemberQ[KeyClient`$predefinedKeyservicelist,name]

(*newkeyauthenticate[name_,rest___]:=newunknownkeyauthenticate[name, rest]*)

defaultKeyOptions={
	"ServiceName"       -> Null,
    "APIKey"			-> "",
    "URLFetchFun"		-> URLFetch
};

newunknownkeyauthenticate[name_,opts___]:=Module[{apikey,params,urlfetchfun,
	service, id},

	params={"APIKey","URLFetchFun"};

	{apikey, urlfetchfun}=params/.Flatten[{opts}]/.defaultKeyOptions;
	(* extraopts=FilterRules[Flatten[{opts}],Except[params]]; *)
	apikey=getapikey[name,apikey];

	If[ListQ[apikey] || StringQ[apikey],
    (
		service=ServiceConnections`Private`createServiceObject["APIKey",name,apikey];

		id=getServiceID[service];
		urlfetchFun[id]=urlfetchfun;
		service
	),
	(
    	Message[ServiceConnect::genconerr, name];
        Throw[$Failed]
    )]
]

(***************************** Exchanging data ***********************************)

keydata[service_ServiceObject,property_,rest___]:=Module[{raw, id},
	id=getServiceID[service];
	If[MemberQ[Join[serviceRequests[id],servicePosts[id]], property],
			KeyClient`keycookeddata[getServiceName[service], property, id,rest]
		,
		(
			If[MemberQ[Join[serviceRawRequests[id],serviceRawPosts[id]], property],
			(
				raw=rawkeydata[id,property,rest];
				parsedata[id,property]@raw
			),
			$Failed]
		)
	]
]


keydata[args___]:=$Failed

rawkeydata[id_,parameter_,rest_]:=keydata[id,parameter,{rest}]/;!ListQ[rest]

rawkeydata[id_,url0_String]:=Module[{url, res},
		url=addapikey[url0,serviceAuthentication[id]];
		If[url === $Failed, Throw[$Failed]];
        (
     		res = urlfetchFun[id]@@url;
     		res /; (res =!= $Failed)
        ) /; (url =!= $Failed)
	]/;!MemberQ[ServiceConnections`Private`availablequeries[id],url0]


rawkeydata[id_,property_,rest___]:=Module[
		{url0,method,pathparams,params,bodyparams,mpdata,headers,reqparams,
			url, res, key, tmp, pvpairs=Flatten[{rest}], params1, bodyparams1,mpdata1,headers1, useauth,returncontentdata,querydata},
		If[KeyClient`$CacheResults,
			res = Internal`CheckCache[{"OAuth", {id, property, rest}}];
			If[res =!= $Failed, Return[res]];
		];
		querydata=ServiceConnections`Private`getQueryData[id, property];
		{url0,method,pathparams,params,bodyparams,mpdata,headers,reqparams,returncontentdata,useauth}=Drop[querydata,{-3}];
		(* check for required parameters *)

		If[!MemberQ[First/@pvpairs,#],
			Message[ServiceExecute::nparam,#];Throw[$Failed]
		]&/@reqparams;

		(* Path Parameters use a StringForm Function *)

		(*hectorg: Call serviceAuthentication for building the url with pathparams in clientinfo*)
		key=serviceAuthentication[id];

		url=If[Head[url0]===Function,
			ServiceConnections`Private`insertpathparameters[url0,pathparams,Join[pvpairs,key]],
			url0
		];

		params1=Cases[params,_?(!FreeQ[pvpairs,#]&)];
		params1=Thread[params1->(params1/.pvpairs)];

		bodyparams1=Cases[bodyparams[[All,2]],x_?(!FreeQ[pvpairs,#]&):>(x/.bodyparams)];
		bodyparams1=Thread[bodyparams[[All,1]]->(bodyparams1/.pvpairs)];
		mpdata1=Cases[mpdata,_?(!FreeQ[pvpairs,First[#]]&)];
		mpdata1=formatMultipartData[mpdata1,pvpairs];
		url={url,
		"Parameters"->params1,
		"BodyData"->
			If[KeyExistsQ[bodyparams1,"ParameterlessBodyData"],
				"ParameterlessBodyData"/.bodyparams1,
				bodyparams1
			],
		"MultipartData"->mpdata1,
		"Method"->method};

		If[MatchQ[useauth,True|"APIKey"],
			url=addapikey[url, key]
		];

		If[!MatchQ[url,_String|{_String,___}],Throw[$Failed]];

		If[headers=!={},
			(* Headers should have default values, check for given values *)
			headers1=If[FreeQ[pvpairs,First[#]],#,First[#]->(First[#]/.pvpairs)]&/@headers;
			url=Join[url,{"Headers"->headers1}]
		];
		If[url === $Canceled, Return[$Canceled]];
        (
     		res=urlfetchFun[id]@@url;
     		(If[KeyClient`$CacheResults,Internal`SetCache[{"OAuth", {id, property, rest}}, res]]; res) /; (res =!= $Failed)
        ) /; (url =!= $Failed)
	]/;MemberQ[Join[serviceRawRequests[id],serviceRawPosts[id]], property]


rawkeydata[___]:=Throw[$Failed]


parsedata[id_,property_]:=(("ResultsFunction"/.keyservicesdata[serviceName[id],property])/."ResultsFunction"->Identity
	)/;MemberQ[Join[serviceRawRequests[id],serviceRawPosts[id]], property]

parsedata[__]:=Identity

(************************************** ServiceDisconnect *****************************)
keydisconnect[service_ServiceObject]:=Module[
	{id=getServiceID[service]},

	serviceName[id]=None;
	serviceRawRequests[id]={};
	serviceRequests[id]={};
	serviceRawPosts[id]={};
	servicePosts[id]={};
	serviceAuthentication[id]={};
	urlfetchFun[id]=URLFetch;
	serviceInfo[id]="";

	ServiceConnections`Private`$authenticatedservices=DeleteCases[ServiceConnections`Private`$authenticatedservices,id];
]


keydisconnect[___]:=$Failed

(************************ Utilities *********************************************)
addapikey[url_String,key_]:=URLBuild[url,{"apikey"->key}]
addapikey[url_List,key_]:=With[{params="Parameters"/.Rest[url]},
	{First[url],"Parameters"->Join[{"apikey"->key},FilterRules[params,Except["apikey"]]],
		Sequence@@FilterRules[Rest[url],Except["Parameters"]]}
]

(* new function to handle multiple keys dialog *)
addapikey[url_List,fields_List]:=With[{params="Parameters"/.Rest[url]},
	{First[url],"Parameters"->Join[fields,FilterRules[params,Except[fields[[All,1]]]]],
		Sequence@@FilterRules[Rest[url],Except["Parameters"]]}
]

(*********************** Cloud Stored Client credentials *************************)

cloudapikeBaseURL="https://www.wolframcloud.com/objects/user-fa95220f-871c-4331-84ab-7951dd0666ca/apikey"

cloudgetapikey[name_]:=Block[{url, key},
	url=URLBuild[cloudapikeBaseURL,{"ServiceName"->name}];
	key=URLFetch[url];
	url=ToExpression[key];
	If[!StringQ[url], $Failed,url]
]

getapikey[name_,apikey_]:=cloudgetapikey[name]/;$KeyCloudCredentialsQ&&MemberQ[KeyClient`$predefinedKeyservicelist,name]
getapikey[_,apikey_]:=apikey


End[];
End[];


System`Private`RestoreContextPath[];
