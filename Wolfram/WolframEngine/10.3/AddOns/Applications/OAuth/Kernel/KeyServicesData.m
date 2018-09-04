(* 
This package loads individual Services that authenticate using a simple API key in the url query
*)

System`Private`NewContextPath[{"KeyClient`","System`"}];

KeyClient`$predefinedKeyservicelist;
KeyClient`KeyServicesData;
KeyClient`keycookeddata;
KeyClient`keysendmessage;
KeyClient`addKeyservice;

Begin["KeyServicesData`"] 

Begin["`Private`"] (* Begin Private Context *) 

(Unprotect[#]; Clear[#])& /@ {KeyServicesData,keycookeddata,keysendmessage,addKeyservice}
Unprotect[$predefinedKeyservicelist];

defaultKeyParams={
					(* defaults *)
					"ServiceName"       -> Null,
				    "Information"		-> "",
				    "URLFetchFun"		-> URLFetch
				    };

defaultKeyLabels=First/@defaultKeyParams;		    
(*************************** KeyServices *************************************)
(* A simple function for retrieving data from below *)
$predefinedKeyservicelist={}

KeyServicesData[args___]:=With[{res=keyservices[args]},
	res/;res=!=$Failed&&Head[res]=!=keyservicedata]

keyservices[name_,prop___]:=Module[{data=keyservicedata[name],availableproperties},
	availableproperties=First/@data;
	Switch[{prop},
		{},	data,
		{"Requests"},availableproperties,
		{"Authentication"},
			Thread[defaultKeyLabels->(defaultKeyLabels/.Join[data,defaultKeyParams])]
		,
		{Alternatives@@availableproperties},
		prop/.data,
		_,
		keyservicedata[name,prop]		
	]
]

keyservices[___]:=$Failed
KeyServicesData[___]:=$Failed

$packagedirectory=FileNameJoin[{DirectoryName[System`Private`$InputFileName],"Services"}];

addKeyservice[name_, dir_:$packagedirectory]:=Module[{funs, file},
	Unprotect[$predefinedKeyservicelist,keyservicedata,keycookeddata,keysendmessage];
	$predefinedKeyservicelist=Union[AppendTo[$predefinedKeyservicelist,name]];
	ServiceConnections`Private`appendservicelist[name,"APIKey"];
	file=FileNameJoin[{dir, "APIKey",name<>"API.m"}];
	If[!FileExistsQ[file],
		(* alternate *)
		file=FileNameJoin[{dir,name<>".m"}]
	];
	If[!FileExistsQ[file],Return[$Failed]];
	funs=Get[file];
	keyservicedata[name,args___]:=funs[[1]][args];
	keycookeddata[name,args___]:=funs[[2]][args];
	keysendmessage[name,args___]:=funs[[3]][args];
	Protect[$predefinedKeyservicelist,keyservicedata,keycookeddata,keysendmessage];
]

(*

PLEASE KEEP ALPHABETICAL ORDER

addKeyservice["GoogleMaps"]


addKeyservice["MailChimp"]
*)
Unprotect[keycookeddata,keysendmessage,keyservicedata];

keyservicedata[___]:=$Failed

(**** error handling ***)
keycookeddata[args___]:=Throw[$Failed]
keysendmessage[___]:=Throw[$Failed]

End[] (* End Private Context *)
End[] 

SetAttributes[{$predefinedKeyservicelist,KeyServicesData,keycookeddata,keysendmessage,addKeyservice},{ReadProtected, Protected}];

System`Private`RestoreContextPath[];

{}