
(* ::Package:: *)

(* $Id$ *)

(* :Summary:
	A free-form framework for connecting to services
*)

(* :Mathematica Version: Mathematica 10.0 *)

(* :Keywords: 

*)

(* :Examples:
*)


System`Private`NewContextPath[{"OtherClient`","System`"}];

OtherClient`rawotherdata;
OtherClient`otherdata;
OtherClient`otherdisconnect;

(Unprotect[#]; Clear[#])& /@ {}

Begin["OtherClient`"];

Begin["`Private`"];


otherservicesdata=OtherClient`OtherServicesData;

ServiceConnections`Private`$otherservices:=OtherClient`$predefinedOtherservicelist;

(* Because the many of the services update thier data frequently (i.e. Twitter) caching is false by default.
	In some places where calls are often repeated, this is set to true *)
OtherClient`$CacheResults=False;

(* Messages *)

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

(************************************** Other Authentication **********************************)

otherauthenticate[]:=Sort[$predefinedOtherservicelist]
otherauthenticate["Services"]:=otherauthenticate[]

otherauthenticate[service_, ___]:=(Message[ServiceConnect::unkn, service];$Failed)/;!MemberQ[$predefinedOtherservicelist,service]

otherauthenticate[name_,"New",rest___?OptionQ]:=(Message[ServiceConnect::key2, name];otherauthenticate[name,rest])
otherauthenticate[name_,_String,rest___?OptionQ]:=(Message[ServiceConnect::key1, name];otherauthenticate[name,rest])


otherauthenticate[name_,rest___]:=newotherauthenticate[name, rest]
	
newotherauthenticate[name_,rest___]:=Module[{service, 
	rawgets=otherservicesdata[name, "RawGets"], 
	gets=otherservicesdata[name, "Gets"],
	rawposts=otherservicesdata[name, "RawPosts"], 
	posts=otherservicesdata[name, "Posts"], id},

	service=newunknownotherauthenticate[name, rest];
	id=getServiceID[service];
	   
	serviceRawRequests[id]=Sort[Flatten[{serviceRawRequests[id],rawgets}]];
	serviceRawPosts[id]=Sort[Flatten[{serviceRawPosts[id],rawposts}]];
	
	serviceRequests[id]=Sort[Flatten[{serviceRequests[id],gets}]];
	servicePosts[id]=Sort[Flatten[{servicePosts[id],posts}]];
	
	service	
]/;MemberQ[OtherClient`$predefinedOtherservicelist,name]

newotherauthenticate[name_,rest___]:=newunknownotherauthenticate[name, rest]

newunknownotherauthenticate[name_,opts___]:=ServiceConnections`Private`createServiceObject["Other",name,None]

(***************************** Exchanging data ***********************************)

otherdata[service_ServiceObject,property_,rest___]:=Module[{id},
	id=getServiceID[service];
	If[MemberQ[Join[serviceRequests[id],servicePosts[id]], property],
		OtherClient`othercookeddata[getServiceName[service], property,rest]
		,
		rawotherdata[id,property,rest]
	]
]


otherdata[args___]:=$Failed

rawotherdata[id_,parameter_,rest_]:=otherdata[id,parameter,{rest}]/;!ListQ[rest]

rawotherdata[id_,property_,rest___]:=Module[
		{url, res},	
		If[OtherClient`$CacheResults,
			res = Internal`CheckCache[{"OAuth", {id, property, rest}}];
			If[res =!= $Failed, Return[res]];
		];
        (
     		res=OtherClient`otherrawdata[serviceName[id],property, rest];
     		(If[OtherClient`$CacheResults,Internal`SetCache[{"OAuth", {id, property, rest}}, res]]; res) /; (res =!= $Failed)
        ) /; (url =!= $Failed)
	]/;MemberQ[Join[serviceRawRequests[id],serviceRawPosts[id]], property]


rawotherdata[___]:=Throw[$Failed]

(*OtherClient`otherdisconnect[service_,args___]:=otherservicesdata[service,"Disconnect",args]*)
(************************************** ServiceDisconnect *****************************)
otherdisconnect[service_ServiceObject]:=Module[
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


otherdisconnect[___]:=$Failed

End[];
End[];


System`Private`RestoreContextPath[];

