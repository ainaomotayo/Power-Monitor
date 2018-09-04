
(* ::Package:: *)

(* $Id$ *)

(* :Summary:
	A framework for authenticating and exchanging data with API services
*)

(* :Mathematica Version: Mathematica 10.0 *)

(* :Keywords:
OAuth
*)

(* :Examples:
*)

System`ServiceConnect;
System`ServiceExecute;
System`ServiceDisconnect;
System`SendMessage;
System`ServiceObject;
System`$Services;


(Unprotect[#]; Clear[#])& /@ {
  ServiceConnect,ServiceDisconnect,ServiceExecute,SendMessage,ServiceObject,ServiceConnections`ServiceInformation
}
Unprotect[$Services];

Needs["HTTPClient`"];
Needs["CloudObject`"];

Begin["ServiceConnections`"];

ServiceConnections`ServiceInformation;

Begin["`Private`"];

$debug=False;
debugPrint[args__]:=Print[args/.{("ConsumerSecret"->_):>("ConsumerSecret"->"xxxx")}]/;$debug

$authenticatedservices={};
$predefinedservices:=Flatten[{$oauthservices,$keyservices,$otherservices,localpacletServices[]}/.{
		HoldPattern[$oauthservices]->{},HoldPattern[$keyservices]->{},HoldPattern[$otherservices]->{}}];

(* Messages *)
ServiceConnect::done="The service `1` has already been authenticated";
ServiceConnect::oauthver="The OAuth version should be 1.0 or 2.0";
ServiceConnect::url="The given url `1` is not a valid string";
ServiceConnect::skey="The key or secret `1` must be a valid string";
ServiceConnect::dialog="The value `1` is not valid for AuthenticationDialog";
ServiceConnect::reqform="The value `1` is not valid for RequestFormat";
ServiceConnect::token="Could not obtain a request token for the OAuth service `1`";
ServiceConnect::apikey="Could not obtain credentials for the service `1`";
ServiceConnect::genconerr="Could not connect to the service `1`";
ServiceExecute::nolink="The service `1` is not authenticated. Try using ServiceConnect."
ServiceConnect::multst="There are multiple connections stored in the `1` directory. Please specify an id."
ServiceConnect::nost="The specified connection could not be found. Try to create a new connection."
ServiceConnect::nostc="The given connection id is not stored in your cloud."
ServiceConnect::nsave="The connection could not be saved."
ServiceConnect::ncloud="The specified connection was not found locally. Use CloudConnect before ServiceConnect to check for cloud-stored connections."
(*
defineQueries::nolink="The service `1` is not authenticated. Try using ServiceConnect."
*)
ServiceExecute::nargs="The number of arguments given does not match the number of slots in the url template."
ServiceExecute::ratel="The rate limit for this query has been exceded."
ServiceExecute::nparam="The parameter `1` is required"
ServiceConnect::unkn="The service `1` is unknown, try providing authentication options."
ServiceConnect::multser="One service was chosen from multiple `1` services."
ServiceExecute::multser="One service was chosen from multiple `1` services."
ServiceObject::noget="The parameter `1` is not available for the service `2`."
ServiceExecute::apierr="The service returned the following error message: `1`."
ServiceExecute::reauth="The service returned the following error message: `1`. Try reauthenticating with ServiceConnect[service, \"New\"]."
ServiceExecute::ndata="The returned data is missing the `1` value."
ServiceInformation::nolink="The service `1` is not authenticated. Try using ServiceConnect."
ServiceConnect::nameid="The given connection id `1` is corresponds to a different service than the specified service, `2`."
ServiceDisconnect::nsc="The value given, `1`, should be a connected ServiceObject."
ServiceExecute::geoent="The given entity `1` does not include coordinates."
ServiceExecute::geon="The given expression `1` can not be interpreted as a location."

ServiceExecute::nval="Invalid value for parameter `1` in service `2`."
ServiceExecute::serror="Server error."
ServiceExecute::serrormsg="Server error. Error message: `1`"
ServiceExecute::invreq="The request `1` should be one of `2`[\"Requests\"], `2`[\"RawRequests\"], or the url for the desired api."

(************************************** ServiceConnect **********************************)
ServiceConnect[args___]:=With[{res=Catch[authenticate[args]]},
	res/;res=!=$Failed
]

authenticate[]:=$Services
authenticate["Services"]:=authenticate[]

authenticate[name_,as_Association]:=authenticate[name, Normal[as]]

authenticate[name_,rest___]:=authenticate0[name,rest]/;MemberQ[$Services,name]

authenticate0[name_,rest___]:=OAuthClient`oauthauthenticate[name,rest]/;MemberQ[$oauthservices,name]
authenticate0[name_,rest___]:=KeyClient`keyauthenticate[name,rest]/;MemberQ[$keyservices,name]
authenticate0[name_,rest___]:=OtherClient`otherauthenticate[name,rest]/;MemberQ[$otherservices,name]
authenticate0[name_String,rest___]:=pacletService[name, rest]/;MemberQ[localpacletServices[],name]

authenticate0[___]:=$Failed

authenticate[name_String, rest__]:=OAuthClient`oauthauthenticate[name, rest]/;!FreeQ[{rest},"OAuthVersion"]

authenticate[name_String,rest___]:=pacletService[name, rest]

authenticate[___]:=$Failed

(************************************** ServiceDisconnect *****************************)
ServiceDisconnect[args___]:=With[{res=Catch[servicedisconnect[args]]},
	res/;res=!=$Failed]

servicedisconnect[service_,___]:=(Message[ServiceDisconnect::nsc,service];$Failed)/;!authenticatedServiceQ[service]

servicedisconnect[service_,rest___]:=OAuthClient`oauthdisconnect[service,rest]/;serviceType[getServiceID[service]]==="OAuth"

servicedisconnect[service_,rest___]:=KeyClient`keydisconnect[service,rest]/;serviceType[getServiceID[service]]==="APIKey"

servicedisconnect[service_,rest___]:=OtherClient`otherdisconnect[service,rest]/;serviceType[getServiceID[service]]==="Other"

servicedisconnect[___]:=$Failed

(************************************** ServiceObject **********************************)
(* objects can be used as pure functions on certain parameter values *)
(service_ServiceObject)[args___]:=With[{res=Catch[serviceobjectdata[service, args]]},
	res/;res=!=$Failed
]

serviceobjectdata[service_, param_, rest___]:=Module[{},
	If[MemberQ[serviceobjectRequests[service],param],
		externalservice[service, param, rest]
		,
		Message[ServiceObject::noget,param, getServiceName[service]];Throw[$Failed]
	]
]


serviceobjectdata[___]:=$Failed

(*************************************** $Services **************************************)
$Services:=Sort[Union[$predefinedservices,serviceName/@$authenticatedservices]]

(************************************** ExternalService **********************************)
ServiceExecute[args___]:=With[{res=Catch[externalservice[args]]},
	res/;res=!=$Failed
]

externalservice[service_ServiceObject,rest___]:=(Message[ServiceExecute::nolink,service];$Failed)/;!authenticatedServiceQ[service]

$specialRequests={"Authentication", "ID", "Information", "Name", "Requests","RawRequests"};

externalservice[service_ServiceObject,"Name",___]:=getServiceName[service]
externalservice[service_ServiceObject,"Requests",___]:=With[{id=getServiceID[service]},
	Sort[DeleteCases[Join[serviceRequests[id],servicePosts[id],serviceDeletes[id],	$specialRequests],"Requests"]]]
externalservice[service_ServiceObject,"RawRequests",___]:=With[{id=getServiceID[service]},
	Sort[Join[serviceRawRequests[id],serviceRawPosts[id],serviceRawDeletes[id]]]]
externalservice[service_ServiceObject,"ID",___]:=getServiceID[service]
externalservice[service_ServiceObject,"Information",___]:=serviceinfo[service]
externalservice[service_ServiceObject,"Authentication",rest___]:=OAuthClient`oauthdata[service,"Authentication",rest]/;serviceType[getServiceID[service]]==="OAuth"
externalservice[service_ServiceObject,"Authentication",rest___]:={}

externalservice[service_ServiceObject,req_, as_Association]:=externalservice[service, req, Normal[as]]

externalservice[service_ServiceObject,rest___]:=(OAuthClient`oauthdata[service, rest])/;serviceType[getServiceID[service]]==="OAuth"
externalservice[service_ServiceObject,rest___]:=KeyClient`keydata[service, rest]/;serviceType[getServiceID[service]]==="APIKey"
externalservice[service_ServiceObject,rest___]:=(OtherClient`otherdata[service, rest])/;serviceType[getServiceID[service]]==="Other"

externalservice[name_String,rest___]:=With[{service=checkservicelist[$authenticatedservices, name, ServiceExecute]},
	If[service===$Failed,
		If[MemberQ[$predefinedservices,name],
			externalservice[authenticate[name], rest],
			Throw[$Failed]
		]
		,
		externalservice[service,rest]
	]

]

externalservice[___]:=$Failed

(* Special Requests *)

(******************** ServiceInformation **********)
ServiceConnections`ServiceInformation[args___]:=With[{res=Catch[serviceinfo[args]]},
	res/;res=!=$Failed
]

serviceinfo[service_ServiceObject,rest___]:=(serviceInfo[getServiceID[service]])/;authenticatedServiceQ[service]

serviceinfo[service_,___]:=(Message[ServiceInformation::nolink,service];$Failed)

serviceInfo[id_]:=""/;!authenticatedServiceQ[id]

(****************** SendMessage *******************)

SendMessage[args___]:=With[{res=Catch[sendmessage[args]]},
	res/;res=!=$Failed]

sendmessage[name_String,rest___]:=With[{service=checkservicelist[$authenticatedservices, name, ServiceExecute]},
	If[service===$Failed,
		If[MemberQ[$predefinedservices,name],
			sendmessage[authenticate[name], rest],
			Message[SendMessage::nolink,name];Throw[$Failed]
		]
		,
		sendmessage[service,rest]
	]
]

sendmessage[service_ServiceObject,as_Association]:=sendmessage[service, Normal[as]]

sendmessage[service_ServiceObject,rest__]:=(Message[SendMessage::nolink,service];Throw[$Failed])/;!authenticatedServiceQ[service]

sendmessage[service_ServiceObject,rest__]:=
	OAuthClient`oauthsendmessage[getServiceName[service],getServiceID[service],rest]/;serviceType[getServiceID[service]]==="OAuth"

sendmessage[service_ServiceObject,rest__]:=KeyClient`keysendmessage[getServiceName[service],getServiceID[service],rest]/;serviceType[getServiceID[service]]==="APIKey"
sendmessage[service_ServiceObject,rest__]:=OtherClient`othersendmessage[getServiceName[service],getServiceID[service],rest]/;serviceType[getServiceID[service]]==="Other"

sendmessage["Email", rest__, o:OptionsPattern[]] := SendMail[rest, o]
sendmessage["Email" -> dest_, rest__, o:OptionsPattern[]] := SendMail[dest, rest, o]

sendmessage["Voice", rest__, o:OptionsPattern[]] := Speak[rest, o]

sendmessage[___]:=$Failed

(******************* pacletService **************************)

pacletService[name_, rest___]:=Block[{paclet=findservicepaclet[name], loaded},
	If[Head[paclet]=!=PacletManager`Paclet,
		Return[$Failed]
	];
	loaded=loadServicePaclet[paclet];
	If[TrueQ[loaded],
		authenticate0[name, rest]
		,
		$Failed
	]
]

findservicepaclet[name_]:=Block[{fullname=createPacletName[name], local},
	local=PacletManager`PacletFind[fullname];
	Switch[Length[local],
		0,findservicepacletRemote[name,fullname],
		_,
		First[local]
	]
]

findservicepacletRemote[name_,fullname_]:=Block[{remote, paclet},
	remote=PacletManager`PacletFindRemote[fullname];
	paclet=Switch[Length[remote],
		0,(Message[ServiceConnect::unkn, name];Return@$Failed),
		_,
		First[remote]
	];
	If[Head[paclet]===PacletManager`Paclet,
		PacletManager`PacletInstall[paclet],
		$Failed
	]
]

createPacletName[name_]:="ServiceConnection_"<>name

loadServicePaclet[paclet_]:=Block[{location, file},
	location="Location"/.PacletManager`PacletInformation[paclet];
	If[location==="location"||!StringQ[location],
		Return[$Failed]
	];
	file=FileNameJoin[{location,"Kernel","load.m"}];
	If[FileExistsQ[file],
		If[!ListQ[$Services],Get["OAuth`"]];
		Get[file];
		True,
		$Failed
	]
]

mostRecentPaclet[paclet_]:=With[{info=PacletManager`PacletInformation[paclet]},
	Switch[Length[info],
		0,$Failed,
		1,First[paclet],
		_,paclet[[Ordering["Version"/.info]//Last]]
	]
]

localpacletServices[]:=Flatten[StringCases[("Name" /. First[#]) & /@
   PacletManager`PacletFind["ServiceConnection*"],
  "ServiceConnection_" ~~ x___ ~~ EndOfString :> x]]
(****************** Utilities *********************)
servicesdata[name_,property_]:=OAuthClient`OAuthServicesData[name,property]/;MemberQ[$oauthservices,name]
servicesdata[name_,property_]:=KeyClient`KeyServicesData[name,property]/;MemberQ[$keyservices,name]
servicesdata[name_,property_]:=OtherClient`OtherServicesData[name,property]/;MemberQ[$otherservices,name]
servicesdata[___]:=$Failed

appendservicelist[service_ServiceObject,type_]:=appendservicelist[getServiceName[service],type]

appendservicelist[name_String,"OAuth"]:=($oauthservices=Union[Flatten[{$oauthservices,name}]])
appendservicelist[name_String,"APIKey"]:=($keyservices=Union[Flatten[{$keyservices,name}]])
appendservicelist[name_String,"Other"]:=($otherservices=Union[Flatten[{$otherservices,name}]])

appendauthservicelist[service_]:=($authenticatedservices=Union[Flatten[{$authenticatedservices,service}]])

makeuuid[]:=StringJoin["connection-",IntegerString[RandomInteger[{0, 16^32}], 16, 32]]

createServiceObject[type_,name_, token_, id0_:Automatic]:=Module[{link, id},
	id=If[id0===Automatic,makeuuid[], id0];
	link=ServiceObject[name, "ID"->id];
	appendservicelist[link,type];
	appendauthservicelist[id];

	serviceName[id]=name;
	serviceRawRequests[id]={};
	serviceRequests[id]={};
	serviceRawPosts[id]={};
	servicePosts[id]={};
	serviceRawDeletes[id]={};
	serviceDeletes[id]={};
	serviceAuthentication[id]=token;
	serviceType[id]:=type;
	urlfetchFun[id]=URLFetch;

	link
]

getQueryData[id_,property_]:=With[{data=servicesdata[serviceName[id],property]},
	(* URL, method, pathparams,params, bodyparams, mpdata, headers, optionalparams, returncontentdata *)
	{("URL"/.data)/."URL"->"",
	("HTTPSMethod"/.data)/."HTTPSMethod"->"GET",
	listwrap@(("PathParameters"/.data)/."PathParameters"->{}),
	listwrap@(("Parameters"/.data)/."Parameters"->{}),
	listwrap@(("BodyData"/.data)/."BodyData"->{}),
	listwrap@(("MultipartData"/.data)/."MultipartData"->{}),
	listwrap@(("Headers"/.data)/."Headers"->{}),
	listwrap@(("RequiredParameters"/.data)/."RequiredParameters"->{}),
	listwrap@(("RequiredPermissions"/.data)/."RequiredPermissions"->{}),
	("ReturnContentData"/.data)/."ReturnContentData"->False,
	(("IncludeAuth"/.data)/."IncludeAuth"->True)}
]

setQueryData[id_,Rule[prop_,data_]]:=getQueryData[id,prop]=data

servicedata[id_]:=Association[{
	"ServiceName"->serviceName[id],
	"ID"->id,
	"RawRequests"->serviceRawRequests[id],
	"Requests"->serviceRequests[id],
	"RawPostRequests"->serviceRawPosts[id],
	"PostRequests"->servicePosts[id],
	"RawDeleteRequests"->serviceRawDeletes[id],
	"DeleteRequests"->serviceDeletes[id],
	"Authentication"->serviceAuthentication[id],
	"Information"->serviceInfo[id]
}]

formatMultipartData[mpdata_,pvpairs_]:={#[[1]],#[[2]],#[[1]]/.pvpairs}&/@mpdata

availablequeries[id_]:=Join[serviceRequests[id],serviceRawRequests[id],servicePosts[id],serviceRawPosts[id],serviceDeletes[id],serviceRawDeletes[id],
	$specialRequests]/;authenticatedServiceQ[id]
availablequeries[_]:={}

getServiceObject[id_]:=ServiceObject[serviceName[id],"ID"->id]

getServiceName[ServiceObject[args___]]:=First[{args}]
getServiceID[service_ServiceObject]:="ID"/.Cases[service,_Rule,{1}]
getServiceIcon[service_ServiceObject]:=With[{icon=servicesdata[getServiceName[service],"icon"]},
	If[MatchQ[icon,_Image|_Graphics],icon,defaultServiceObjectIcon]]

serviceName[id_]:=None/;!authenticatedServiceQ[id]
serviceRawRequests[id_]:={}/;!authenticatedServiceQ[id]
serviceRequests[id_]:={}/;!authenticatedServiceQ[id]
serviceRawPosts[id_]:={}/;!authenticatedServiceQ[id]
servicePosts[id_]:={}/;!authenticatedServiceQ[id]
serviceRawDeletes[id_]:={}/;!authenticatedServiceQ[id]
serviceDeletes[id_]:={}/;!authenticatedServiceQ[id]
serviceAuthentication[id_]:={}/;!authenticatedServiceQ[id]

urlfetchFun[id_]:=URLFetch/;!authenticatedServiceQ[id]
serviceType[id_]:=None/;!authenticatedServiceQ[id]
logout[id_]:=Null/;!authenticatedServiceQ[id]

serviceobjectRequests[service_]:=With[{id=getServiceID[service]},
	availablequeries[id]
]

authenticatedServiceQ[service_ServiceObject]:=authenticatedServiceQ[getServiceID[service]]
authenticatedServiceQ[id_]:=MemberQ[$authenticatedservices,id]

checkservicelist[list_, name_String, fn_]:=With[{matches=Select[list,serviceName[#]===name&]},
	Switch[Length[matches],
		1,getServiceObject[First[matches]],
		0,$Failed,
		_,Message[fn::multser,name];(*getServiceObject[First[matches]]*) getServiceObject[First[matches]]
	]
]

listwrap[l_List]:=l
listwrap[x_]:={x}

insertpathparameters[url0_,pathparams_,pvpairs_]:=Module[{given, pparams1},
		given=Cases[pathparams,_?(!FreeQ[pvpairs,#]&)];

		pparams1=If[Length[given]<Length[pathparams],
			(Replace[pathparams,Except[Alternatives@@given]->Automatic,{1}]/.{before___,Automatic..}:>{before})/.pvpairs
			,
			pathparams/.pvpairs
		];
		Check[url0@@(pparams1),Message[ExternalService::nargs];Throw[$Failed]]
]

(********************************** ServiceObject Typesetting ******************************)

(*** probably need another option? or just check *Values?  *)
simplePropQ[x_] := NumberQ[x] || StringQ[x] || MemberQ[{True,False,Automatic},x]

paddedRow[e_,margins_:{{0,0},{0,5}},opts___?OptionQ] := Row[e,opts,ImageMargins->margins]

greenLight[] := Graphics[
	{{LightGreen, Disk[{0, 0}]}, {RGBColor[0, .85, 0], Circle[{0, 0}]}}
	,
	PlotRange->{-2.2, 1.1},
	ImageSize -> 8
]


ServiceObject/:
MakeBoxes[service_ServiceObject, form:StandardForm|TraditionalForm] :=
With[{name=getServiceName[service], id=getServiceID[service], icon=getServiceIcon[service]},
	If[$VersionNumber>=10,
		BoxForm`ArrangeSummaryBox[
			(* Head *)ServiceObject,
			(* Interpretation *)service,
			(* Icon *)icon,
			(* Column or Grid *){name, (* id, *) Dynamic[If[TrueQ[authenticatedServiceQ[id]],"Connected","Not Connected"]]},
			(* Plus Box Column or Grid *){},
			form]
			,
		InterpretationBox[#,service]&@ToBoxes[Framed@Row[{"ServiceObject       ",Column[{name, id, "Authenticated"->authenticatedServiceQ[id]}]}]]
	]
]


defaultServiceObjectIcon=BoxForm`GenericIcon[LinkObject](*
Graphics[{Thickness[0.0016339869281045752], Style[{FilledCurve[{{{1, 4, 3}, {0, 1, 0}, {1, 3, 3}, {0, 1,
        0}, {1, 3, 3}, {0, 1, 0}, {1, 3, 3}, {0, 1, 0}}}, {{{25.499999999999996, 2.5}, {25.499999999999996,1.3953100000000003},
        {24.604699999999998, 0.49999999999999994}, {23.5, 0.49999999999999994}, {2.5, 0.49999999999999994}, {1.3953100000000003,
        0.49999999999999994}, {0.49999999999999994, 1.3953100000000003}, {0.49999999999999994, 2.5}, {0.49999999999999994, 23.5}, {0.49999999999999994,
        24.604699999999998}, {1.3953100000000003, 25.499999999999996}, {2.5, 25.499999999999996}, {23.5, 25.499999999999996}, {24.604699999999998,
        25.499999999999996}, {25.499999999999996, 24.604699999999998}, {25.499999999999996, 23.5}, {25.499999999999996, 2.5}}}]},
   FaceForm[RGBColor[0.9411621, 0.9607848999999999, 0.9568481, 1.]]], Style[{JoinedCurve[{{{1, 4, 3}, {0, 1, 0}, {1, 3, 3}, {0, 1,
        0}, {1, 3, 3}, {0, 1, 0}, {1, 3, 3}, {0, 1, 0}}}, {{{25.499999999999996, 2.5}, {25.499999999999996,
        1.3953100000000003}, {24.604699999999998, 0.49999999999999994}, {23.5, 0.49999999999999994}, {2.5, 0.49999999999999994}, {1.3953100000000003,
        0.49999999999999994}, {0.49999999999999994, 1.3953100000000003}, {0.49999999999999994, 2.5}, {0.49999999999999994, 23.5}, {0.49999999999999994,
        24.604699999999998}, {1.3953100000000003,  25.499999999999996}, {2.5, 25.499999999999996}, {23.5, 25.499999999999996}, {24.604699999999998,
        25.499999999999996}, {25.499999999999996, 24.604699999999998}, {25.499999999999996, 23.5}, {25.499999999999996, 2.5}}}]},
   JoinForm[{"Miter", 10.}], RGBColor[0.699951, 0.699951, 0.699951, 1.]], Style[{FilledCurve[{{{0, 2, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1,
        0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}}}, {{{22.306600000000003, 16.3496}, {17.184399999999997,
        19.748000000000005}, {17.184399999999997, 17.4641}, {5.542580000000001, 17.4641}, {5.542580000000001,
        15.2355}, {17.184399999999997, 15.2355}, {17.184399999999997,  12.951599999999997}, {22.306600000000003, 16.3496}}}],
         FilledCurve[{{{0, 2, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}}},
           {{{3.541019999999999, 9.84961}, {8.663279999999999, 6.45156}, {8.663279999999999, 8.735549999999998}, {20.3051,
        8.735549999999998}, {20.3051, 10.964100000000002}, {8.663279999999999, 10.964100000000002}, {8.663279999999999,
        13.248000000000003}, {3.541019999999999, 9.84961}}}]},
   FaceForm[RGBColor[0.5, 0.5, 0.5, 1.]]]}, {Background -> GrayLevel[0.93], Axes -> False, AspectRatio -> 1,
     ImageSize -> {Automatic, Dynamic[3.5*(CurrentValue["FontCapHeight"]/ AbsoluteCurrentValue[Magnification])]}, Frame -> True,
  FrameTicks -> None, FrameStyle -> Directive[Thickness[Tiny], GrayLevel[0.7]]}] *)

End[];
End[];

SetAttributes[{
  ServiceConnect,ServiceDisconnect,ServiceExecute,SendMessage,ServiceObject,ServiceConnections`ServiceInformation,$Services
},
   {ReadProtected, Protected}
];


{System`ServiceConnect,System`ServiceDisconnect,
System`ServiceExecute,System`SendMessage,
System`ServiceObject}
