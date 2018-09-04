(* Mathematica Package *)

BeginPackage["DataDropClient`"]

Begin["`Private`"] (* Begin Private Context *) 

(*** API Function ***)
apifun[Except[_String],_]:=(Message[Databin::invreq];$Failed)

apifun["Add", as_] := Block[{raw, mpdata, strkeys, imgkeys},
	{strkeys, imgkeys, mpdata} = makeMPData[Normal[as]];
  	raw = CloudObject`Private`authenticatedURLFetch[gatewayapi,{"StatusCode", "Content"},
    	"MultipartData" -> {
      		{"API", "text/plain", {65, 100, 100}},
      		{"InputStrings", "text/plain", ToCharacterCode@strkeys},
      		{"CompressedWDFImages", "text/plain", ToCharacterCode@imgkeys},
      		{"SourceType", "text/plain", {67, 111, 110, 110, 101, 99, 116, 101, 100,
      			32, 87, 111, 108, 102, 114, 97, 109, 32, 76, 97, 110, 103, 117, 97, 103, 101}},
            {"ClientVersion", "text/plain",ToCharacterCode@$datadropclientversion},
      		Sequence @@ mpdata
      	},  "Method" -> "POST", "VerifyPeer" -> False, "CredentialsProvider" -> None]; 
    importResults["Add"][checkAvailable[Quiet[ToExpression[raw]], "Add"]]
    ]/;$CloudConnected&&!FreeQ[as,_Image]&&ByteCount[as]>$UncompressedImageLimit

apifun["Add",as_]:=
With[{
	raw=If[$CloudConnected,CloudObject`Private`authenticatedURLFetch,URLFetch][gatewayapi,{"StatusCode","Content"},
		"Parameters"->Join[{"API"->"Add","SourceType"->"\"Connected Wolfram Language\"","InputStrings"->"True",
			"ClientVersion"->ToString[$datadropclientversion,InputForm]},
			preparedata[Normal[as]]],"Method"->"POST", 
			"VerifyPeer" -> False,"CredentialsProvider" -> None]},
	importResults["Add"][checkAvailable[Quiet[ToExpression[raw]],"Add"]]
]

apifun["Upload", as_] := Block[{raw, mpdata, strkeys, imgkeys},
    {strkeys, imgkeys, mpdata} = makeMPData[Normal[as]];
    raw = CloudObject`Private`authenticatedURLFetch[gatewayapi,{"StatusCode", "Content"},
        "MultipartData" -> {
            {"API", "text/plain", {85, 112, 108, 111, 97, 100}},
            {"InputStrings", "text/plain", ToCharacterCode@strkeys},
            {"CompressedWDFImages", "text/plain", ToCharacterCode@imgkeys},
            {"SourceType", "text/plain", {67, 111, 110, 110, 101, 99, 116, 101, 100,
                32, 87, 111, 108, 102, 114, 97, 109, 32, 76, 97, 110, 103, 117, 97, 103, 101}},
            {"ClientVersion", "text/plain",ToCharacterCode@$datadropclientversion},
            Sequence @@ mpdata
        },  "Method" -> "POST", "VerifyPeer" -> False, "CredentialsProvider" -> None]; 
    importResults["Add"][checkAvailable[Quiet[ToExpression[raw]], "Add"]]
    ]/;$CloudConnected&&KeyExistsQ[as,"DataDropReferences"]
    
apifun["Upload",as_]:=
With[{
    raw=CloudObject`Private`authenticatedURLFetch[gatewayapi,{"StatusCode","Content"},
        "Parameters"->Join[{"API"->"Upload","SourceType"->"\"Connected Wolfram Language\"","InputStrings"->"True",
            "ClientVersion"->ToString[$datadropclientversion,InputForm]},
            preparedata[Normal[as]]],"Method"->"POST", 
            "VerifyPeer" -> False,"CredentialsProvider" -> None]},
    importResults["Upload"][checkAvailable[Quiet[ToExpression[raw]],"Add"]]
]/;$CloudConnected

apifun[name_,as_]:=
With[{
	raw=CloudObject`Private`authenticatedURLFetch[gatewayapi,{"StatusCode","Content"},
		"Parameters"->Join[{"API"->name,"SourceType"->"\"Connected Wolfram Language\"","InputStrings"->"True",
            "ClientVersion"->ToString[$datadropclientversion,InputForm]},
			preparedata[Normal[as]]], 
			"VerifyPeer" -> False,"CredentialsProvider" -> None]},
	importResults[name][checkAvailable[Quiet[ToExpression[raw]],name]]
]/;$CloudConnected

apifun[name_,as_]:=
With[{
	raw=URLFetch[gatewayapi,{"StatusCode","Content"},
		"Parameters"->Join[{"API"->name,"SourceType"->"\"Unconnected Wolfram Language\"","InputStrings"->"True",
			"ClientVersion"->ToString[$datadropclientversion,InputForm]},
			preparedata[Normal[as]]],"VerifyPeer" -> False,"CredentialsProvider" -> None]},
	importResults[name][checkAvailable[Quiet[ToExpression[raw]],name]]
]/;MemberQ[Join[$nonauthenticatedrequests,$nonauthenticatedapinames],name]


apifun[name_,as_]:=(
	CloudConnect[];
	If[$CloudConnected,
		apifun[name,as]
		,
		Message[Databin::cloudc];
		Throw[$Failed]
	]
)

$DataDropHTTPCodes=_;
checkAvailable[{501,res_},_]:=(Message[Databin::notav];Throw[$Failed])
checkAvailable[{504,res_},"Read"|"Dashboard"]:=(Message[Databin::timeout1];Throw[$Failed])
checkAvailable[{504,res_},_]:=(Message[Databin::timeout2];Throw[$Failed])
checkAvailable[{code_,res_},name_]:=errorcheck[res, name]/;!MatchQ[code,$DataDropHTTPCodes]
checkAvailable[$Failed,_]:=$Failed
checkAvailable[{_,res_},_]:=res
checkAvailable[___]:=$Failed

readrequestpattern=("Read"|"Recent"|"Entries"|"Latest"|"Values");

importResults[readrequestpattern]:=(With[{data=datadropMXRead[#]},
	If[$ImportDataDropReferences,
		importDataDropReferences[checkWarnings[data]],
		checkWarnings[data]
	]
]&)

importResults["Raw"]=importRawResults
importResults[_]:=Identity


importResults["Raw"]=importRawResults
importResults[_]:=Identity

Attributes[importRawResults]={HoldFirst};
importRawResults[x_] := Block[{ToExpression = Identity}, x]

makeMPData[rules_] := Block[{toexpkeys = {}, mpdata},
  mpdata = makempdata /@ rules;
  toexpkeys = Flatten[First /@ mpdata];
  {StringJoin[Riffle[toexpkeys, ","]], 
   StringJoin[Riffle[Complement[First /@ rules, toexpkeys], ","]], 
   Last /@ mpdata}
  ]
makempdata[_[key_, img_Image]] := {{}, {key, "image/png", 
   ToCharacterCode[ExportString[img, "PNG"]]}}
makempdata[_[key_, value_]] := {{key}, {key, "text/plain", 
   ToCharacterCode[ToString[value, InputForm]]}}
   

End[] (* End Private Context *)

EndPackage[]