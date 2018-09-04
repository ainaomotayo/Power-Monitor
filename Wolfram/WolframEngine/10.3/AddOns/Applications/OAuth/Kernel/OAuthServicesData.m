(* 
This package loads individual OAuth Services so that they are available is the Service Connection framework. 
*)

System`Private`NewContextPath[{"OAuthClient`","System`"}];
(*
BeginPackage["OAuthServicesData`"]
*)
(* Exported symbols added here with SymbolName::usage *)  
OAuthClient`$predefinedOAuthservicelist;
OAuthClient`OAuthServicesData;
OAuthClient`oauthcookeddata;
OAuthClient`oauthsendmessage;
OAuthClient`addOAuthservice;

Begin["OAuthServicesData`"] (* Begin Private Context *) 

Begin["`Private`"] (* Begin Private Context *) 

(Unprotect[#]; Clear[#])& /@ {OAuthServicesData,oauthcookeddata,oauthsendmessage,addOAuthservice}
Unprotect[$predefinedOAuthservicelist];

defaultOAuthParams={
					(* defaults *)
					"ServiceName"       -> Null,
				    "OAuthVersion"		-> "1.0a",
				    "RequestEndpoint"   -> "",
				    "AccessEndpoint"    -> Null,
				    "AuthorizeEndpoint" -> Null,
				   	"RedirectURI"		->	"oob",
				   	"VerifierLabel"		-> "verifier",
				   	"AdditionalOAuthParameter"	-> None,
				   	"Scope"				-> None,
				    "AuthenticationDialog" 	-> "TokenDialog",
				    "RequestFormat"		-> "URL",
				    "ResponseType"		-> "code",
				    "AccessTokenExtractor"	-> None,
				    "Information"		-> ""
				    };

defaultOAuthLabels=First/@defaultOAuthParams;		    
(*************************** OAuthServices *************************************)
(* A simple function for retrieving data from below *)
$predefinedOAuthservicelist={}

OAuthServicesData[args___]:=With[{res=oauthservices[args]},
	res/;res=!=$Failed&&Head[res]=!=oauthservicedata]

oauthservices[name_,prop___]:=Module[{data=oauthservicedata[name],availableproperties},
	availableproperties=First/@data;
	Switch[{prop},
		{},	data,
		{"Requests"},availableproperties,
		{"Authentication"},
			Join[Thread[defaultOAuthLabels->(defaultOAuthLabels/.Join[data,defaultOAuthParams])],
				FilterRules[data,Except[Join[defaultOAuthLabels,{"Gets","Posts","RawGets","RawPosts","ClientInfo"}]]]]
		,
		{Alternatives@@availableproperties},
		prop/.data,
		_,
		oauthservicedata[name,prop]		
	]
]

oauthservices[___]:=$Failed
OAuthServicesData[___]:=$Failed
(*************************** Data for Services *********************************)
(* Storage of information about our apps and the services they work with *)

$packagedirectory=FileNameJoin[{DirectoryName[System`Private`$InputFileName],"Services"}];

addOAuthservice[name_, dir_:$packagedirectory]:=Module[{funs, file},
	Unprotect[$predefinedOAuthservicelist,oauthservicedata,oauthcookeddata,oauthsendmessage];
	$predefinedOAuthservicelist=Union[AppendTo[$predefinedOAuthservicelist,name]];
	ServiceConnections`Private`appendservicelist[name,"OAuth"];
	file=FileNameJoin[{dir, "OAuth",name<>"OAuth.m"}];
	If[!FileExistsQ[file],
		(* alternate *)
		file=FileNameJoin[{dir,name<>".m"}]
	];
	If[!FileExistsQ[file],Return[$Failed]];
	funs=Get[file];
	oauthservicedata[name,args___]:=funs[[1]][args];
	oauthcookeddata[name,args___]:=funs[[2]][args];
	oauthsendmessage[name,args___]:=funs[[3]][args];
	If[Length[funs]>4,
		OAuthClient`checkpermissions[name,args___]:=funs[[4]][args];
		OAuthClient`addpermissions[name,args___]:=funs[[5]][args];
	];
	Protect[$predefinedOAuthservicelist,oauthservicedata,oauthcookeddata,oauthsendmessage];
]

(*PLEASE KEEP ALPHABETICAL ORDER*)

addOAuthservice["Dropbox"]
addOAuthservice["Facebook"]
addOAuthservice["Fitbit"]
(*addOAuthservice["GoogleAnalytics"]*)
(*addOAuthservice["GoogleBigQuery"]*)
addOAuthservice["GooglePlus"]
addOAuthservice["Instagram"]
addOAuthservice["LinkedIn"]
addOAuthservice["RunKeeper"]
addOAuthservice["Twitter"]
(* will be phased out by Jawbone
addOAuthservice["BodyMedia"]
*)


Unprotect[oauthcookeddata,oauthsendmessage,oauthservicedata];

oauthservicedata[___]:=$Failed

(**** error handling ***)
oauthcookeddata[args___]:=Throw[$Failed]
oauthsendmessage[___]:=Throw[$Failed]
OAuthClient`checkpermissions[___]:=All
OAuthClient`addpermissions[___]:=Throw[$Failed]

SetAttributes[{$predefinedOAuthservicelist,OAuthServicesData,oauthcookeddata,oauthsendmessage,addOAuthservice},{ReadProtected, Protected}];

End[] (* End Private Context *)
End[] 


System`Private`RestoreContextPath[];

{}
(*
{OAuthClient`OAuthServicesData,OAuthClient`oauthcookeddata,OAuthClient`oauthsendmessage}
*)