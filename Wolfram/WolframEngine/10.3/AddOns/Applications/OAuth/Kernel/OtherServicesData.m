(* 
This package loads individual Services which do not use the built in authentication frameworks
*)

System`Private`NewContextPath[{"OtherClient`","System`"}];

OtherClient`$predefinedOtherservicelist;
OtherClient`OtherServicesData;
OtherClient`othercookeddata;
OtherClient`otherrawdata;
OtherClient`othersendmessage;
OtherClient`addOtherservice;

Begin["OtherServicesData`"] 

Begin["`Private`"] (* Begin Private Context *) 

(Unprotect[#]; Clear[#])& /@ {OtherServicesData,othercookeddata,otherrawdata,othersendmessage,addOtherservice}
Unprotect[$predefinedOtherservicelist];

defaultOtherParams={
					(* defaults *)
					"ServiceName"       -> Null,
				    "Information"		-> ""
				    };

defaultOtherLabels=First/@defaultOtherParams;		    
(*************************** OtherServices *************************************)
(* A simple function for retrieving data from below *)
$predefinedOtherservicelist={}

OtherServicesData[args___]:=With[{res=otherservices[args]},
	res/;res=!=$Failed&&Head[res]=!=otherservicedata]

otherservices[name_,prop___]:=Module[{data=otherservicedata[name],availableproperties},
	availableproperties=First/@data;
	Switch[{prop},
		{},	data,
		{"Requests"},availableproperties,
		{"Authentication"},{},
		{Alternatives@@availableproperties},
		prop/.data,
		_,
		otherservicedata[name,prop]		
	]
]

otherservices[___]:=$Failed
OtherServicesData[___]:=$Failed

$packagedirectory=FileNameJoin[{DirectoryName[System`Private`$InputFileName],"Services"}];

addOtherservice[name_, dir_:$packagedirectory]:=Module[{funs, file},
	Unprotect[$predefinedOtherservicelist,otherservicedata,othercookeddata,otherrawdata,othersendmessage];
	$predefinedOtherservicelist=Union[AppendTo[$predefinedOtherservicelist,name]];
	ServiceConnections`Private`appendservicelist[name,"Other"];
	file=FileNameJoin[{dir,"Other", name<>".m"}];
	If[!FileExistsQ[file],
		(* alternate *)
		file=FileNameJoin[{dir,name<>".m"}]
	];
	If[!FileExistsQ[file],Return[$Failed]];
	funs=Get[file];
	otherservicedata[name,args___]:=funs[[1]][args];
	othercookeddata[name,args___]:=funs[[2]][args];
	othersendmessage[name,args___]:=funs[[3]][args];
	otherrawdata[name,args___]:=funs[[4]][args];
	Protect[$predefinedOtherservicelist,otherservicedata,othercookeddata,otherrawdata,othersendmessage];
]

(*
add services here

PLEASE KEEP ALPHABETICAL ORDER

addOtherservice["ServiceName"]

addOtherservice["WaybackMachine"]
*)
Unprotect[othercookeddata,otherrawdata,othersendmessage,otherservicedata];

otherservicedata[___]:=$Failed

(**** error handling ***)
othercookeddata[args___]:=Throw[$Failed]
otherrawdata[args___]:=Throw[$Failed]
othersendmessage[___]:=Throw[$Failed]

End[] (* End Private Context *)
End[] 

SetAttributes[{$predefinedOtherservicelist,OtherServicesData,otherrawdata,othercookeddata,othersendmessage,addOtherservice},{ReadProtected, Protected}];

System`Private`RestoreContextPath[];

{}