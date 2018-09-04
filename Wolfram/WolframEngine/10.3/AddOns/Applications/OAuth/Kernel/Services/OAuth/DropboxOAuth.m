
System`Private`NewContextPath[{"OAuthClient`","System`"}];

Begin["DropboxOAuth`"] (* Begin Private Context *) 

Begin["`Private`"](* Begin Private Context *) 

(******************************* Dropbox *************************************)

ServiceExecute::ndir="The specified path `1` is not a directory in the connected Dropbox account. Data will be given for the file instead"
ServiceExecute::nfile="The specified path `1` is not a file in the connected Dropbox account. Data will be given for the directory instead"
ServiceExecute::grext="The graphic could not be exported as the file type, `1`, given in the path."


(* Authentication information *)



dropboxdata[]={
		"OAuthVersion"		->"2.0",
		"ServiceName" 		-> "Dropbox", 
		"AuthorizeEndpoint" -> "https://www.dropbox.com/1/oauth2/authorize", 
		"AccessEndpoint" 	-> "https://api.dropbox.com/1/oauth2/token", 
		"RedirectURI" 		-> "https://www.wolfram.com/oauthlanding?service=Dropbox",
	 	"ClientInfo"		-> {"Wolfram","Token"},
	 	"AuthenticationDialog" :> (OAuthClient`tokenOAuthDialog[#, "Dropbox",dbicon]&),
	 	"Gets"				-> {"DirectoryTreePlot","FileSearch","FileData","FileNames","FileContents","DirectoryData","UserData","ImportFile"},
	 	"Posts"				-> {"DataUpload","GraphicsUpload"},
	 	"RawGets"			-> {"RawUserData","RawFileDownload","RawPathData","RawFileRevisions",
	 		"RawFileSearch","RawCopyFileReference",  "RawThumbnail"},
	 	"RawPosts"			-> {"RawFileUpload","RawFileRestore","RawFileChange","RawFilePreviewLink","RawFileLink",
	 		"RawChunkUpload","RawChunkedUploadCommit","RawFileCopy","RawCreateFolder","RawFileDelete","RawFileMove"},
	 	"RequestFormat"		-> (Block[{params=Cases[{##},("Parameters"->x_):>x,Infinity], 
	 		url=DeleteCases[{##},"Parameters"->_,Infinity],
	 		method=Cases[{##},("Method"->x_):>x,Infinity]},
	 		If[method==={"GET"},
	 			URLFetch@@({Sequence@@url, "Parameters"->Flatten@params}),
	 			url[[1]]=URLBuild[url[[1]], params];
	 			URLFetch@@{Sequence@@url}
	 		]
	 	]&),
	 	"LogoutURL"			-> "https://www.dropbox.com/logout",
 		"Information"		-> "Connect the Wolfram Language with your dropbox account"
}

(* a function for importing the raw data - usually json or xml - from the service *)
dropboximport[$Failed]:=Throw[$Failed]
dropboximport[raw_String]:=If[StringFreeQ[raw,"error"],raw,Message[ServiceExecute::apierr,raw]
]
dropboximport[raw_]:=raw


dropboximportjson[$Failed]:=Throw[$Failed]
dropboximportjson[json_, forcelistQ_:False]:=With[{res=ImportString[json,"JSON"]},
	If[FreeQ[res,_["errors",_]],
		If[forcelistQ,Association/@res,
			Switch[res,
				_Rule|{_Rule...},Association@res,
				{{_Rule...}...},Association/@res,
				_,res
			]
		],
		Message[ServiceExecute::apierr,("errors"/.res)];
		Throw[$Failed]
	]
]


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

dropboxdata["RawUserData"] = {
        "URL"				-> "https://api.dropbox.com/1/account/info",
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> dropboximportjson
    }   

dropboxdata["RawFileDownload"] = {
        "URL"				-> (ToString@StringForm["https://api-content.dropbox.com/1/files/`1`", formatrootpath[##]]&),
        "PathParameters"	-> {"Root","Path"},
        "Parameters"		-> {"rev"},
        "RequiredParameters"-> {"Root","Path"}, 
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> dropboximport
    }   
    
dropboxdata["RawPathData"] = {
        "URL"				-> (ToString@StringForm["https://api.dropbox.com/1/metadata/`1`", formatrootpath[##]]&),
        "PathParameters"	-> {"Root","Path"},
        "Parameters"		-> {"file_limit","list","hash","include_deleted","rev","locale"},
        "RequiredParameters"-> {"Root","Path"}, 
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> dropboximportjson
    }  
      
dropboxdata["RawFileChange"] = {
        "URL"				-> "https://api.dropbox.com/1/delta",
        "Parameters"		-> {"cursor","locale"},
        "HTTPSMethod"		-> "POST",
        "ResultsFunction"	-> dropboximportjson
    }  
      
dropboxdata["RawFileRevisions"] = {
        "URL"				-> (ToString@StringForm["https://api.dropbox.com/1/revisions/`1`", formatrootpath[##]]&),
        "PathParameters"	-> {"Root","Path"},
        "Parameters"		-> {"rev_limit","locale"},
        "RequiredParameters"-> {"Root","Path"}, 
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> dropboximportjson
    }  
    
dropboxdata["RawFileSearch"] = {
        "URL"				-> (ToString@StringForm["https://api.dropbox.com/1/search/`1`", formatrootpath[##]]&),
        "PathParameters"	-> {"Root","Path"},
        "Parameters"		-> {"query","file_limit","include_deleted","locale"},
        "RequiredParameters"-> {"Root","Path","query"}, 
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> dropboximportjson
    }  
             
dropboxdata["RawFileUpload"] = {
        "URL"				-> (ToString@StringForm["https://api-content.dropbox.com/1/files_put/`1`", formatrootpath[##]]&),
        "PathParameters"	-> {"Root","Path"},
        "Parameters"		-> {"rev","locale","overwrite","parent_rev"},
        "RequiredParameters"-> {"Root","Path"}, 
        "BodyData"			-> {"ParameterlessBodyData"},
      (*   "MultipartData"		-> {{"FileData","text/plain"}}, *)
      	"Headers" 			-> {"Content-Type" -> "text/plain"},
        "HTTPSMethod"		-> "POST",
        "ResultsFunction"	-> dropboximportjson
    }   
            
dropboxdata["RawFileRestore"] = {
        "URL"				-> (ToString@StringForm["https://api.dropbox.com/1/restore/`1`", formatrootpath[##]]&),
        "PathParameters"	-> {"Root","Path"},
        "Parameters"		-> {"rev","locale"},
        "RequiredParameters"-> {"Root","Path","rev"}, 
        "HTTPSMethod"		-> "POST",
        "ResultsFunction"	-> dropboximportjson
    }   
            
dropboxdata["RawFilePreviewLink"] = {
        "URL"				-> (ToString@StringForm["https://api.dropbox.com/1/shares/`1`", formatrootpath[##]]&),
        "PathParameters"	-> {"Root","Path"},
        "Parameters"		-> {"short_url","locale"},
        "RequiredParameters"-> {"Root","Path"}, 
        "HTTPSMethod"		-> "POST",
        "ResultsFunction"	-> dropboximportjson
    }  

dropboxdata["RawFileLink"] = {
        "URL"				-> (ToString@StringForm["https://api.dropbox.com/1/media/`1`", formatrootpath[##]]&),
        "PathParameters"	-> {"Root","Path"},
        "Parameters"		-> {"locale"},
        "RequiredParameters"-> {"Root","Path"}, 
        "HTTPSMethod"		-> "POST",
        "ResultsFunction"	-> dropboximportjson
    }     
    
dropboxdata["RawCopyFileReference"] = {
        "URL"				-> (ToString@StringForm["https://api.dropbox.com/1/search/`1`", formatrootpath[##]]&),
        "PathParameters"	-> {"Root","Path"},
        "RequiredParameters"-> {"Root","Path"}, 
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> dropboximportjson
    } 
    
dropboxdata["RawThumbnail"] = {
        "URL"				-> (ToString@StringForm["https://api-content.dropbox.com/1/thumbnails/`1`", formatrootpath[##]]&),
        "PathParameters"	-> {"Root","Path"},
        "Parameters"		-> {"format","size"},
        "RequiredParameters"-> {"Root","Path"}, 
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> dropboximportjson
    }  
    
dropboxdata["RawChunkUpload"] = {
        "URL"				-> "https://api-content.dropbox.com/1/chunked_upload",
        "BodyData"			-> {"ParameterlessBodyData"},
        "Parameters"		-> {"upload_id","offset"},
        "RequiredParameters"-> {"ParameterlessBodyData"}, 
        "HTTPSMethod"		-> "POST",
        "ResultsFunction"	-> dropboximportjson
    }  
       
dropboxdata["RawChunkedUploadCommit"] = {
        "URL"				-> (ToString@StringForm["https://api-content.dropbox.com/1/commit_chunked_upload/`1`", formatrootpath[##]]&),
        "PathParameters"	-> {"Root","Path"},
        "Parameters"		-> {"locale","overwrite","parent_rev","upload_id"},
        "RequiredParameters"-> {"Root","Path"}, 
        "HTTPSMethod"		-> "POST",
        "ResultsFunction"	-> dropboximportjson
    } 

(** File Operations **)
dropboxdata["RawFileCopy"] = {
        "URL"				-> "https://api.dropbox.com/1/fileops/copy",
        "Parameters"		-> {"root","from_path","to_path","locale","from_copy_ref"},
        "RequiredParameters"-> {"root","to_path","from_path"|"from_copy_ref"}, 
        "HTTPSMethod"		-> "POST",
        "ResultsFunction"	-> dropboximportjson
    } 

dropboxdata["RawCreateFolder"] = {
        "URL"				-> "https://api.dropbox.com/1/fileops/create_folder",
        "Parameters"		-> {"root","path","locale"},
        "RequiredParameters"-> {"root","path"}, 
        "HTTPSMethod"		-> "POST",
        "ResultsFunction"	-> dropboximportjson
    } 
    
dropboxdata["RawFileDelete"] = {
        "URL"				-> "https://api.dropbox.com/1/fileops/delete",
        "Parameters"		-> {"root","path","locale"},
        "RequiredParameters"-> {"root","path"}, 
        "HTTPSMethod"		-> "POST",
        "ResultsFunction"	-> dropboximportjson
    } 
     
dropboxdata["RawFileMove"] = {
        "URL"				-> "https://api.dropbox.com/1/fileops/move",
        "Parameters"		-> {"root","from_path","to_path","locale"},
        "RequiredParameters"-> {"root","from_path","to_path"}, 
        "HTTPSMethod"		-> "POST",
        "ResultsFunction"	-> dropboximportjson
    } 
    
    
dropboxdata["icon"]=dbicon
    
dropboxdata[___]:=$Failed
(****** Cooked Properties ******)
  
(* cooked data queries 
	Cooked queries call at least one of the raw queried underneath but add pre and post processing to make the function easier to use and the result clearer.
*)  

dropboxcookeddata[prop_,id_,rule_Rule, rest___]:=dropboxcookeddata[prop,id,{rule}, rest]
dropboxcookeddata[prop_,id_]:=dropboxcookeddata[prop,id,{}]

dropboxcookeddata["FileContents",id_,args_]:=Block[
	{params,rawdata, root,data},
	params=filterparameters[Join[args,{"Root"->"dropbox"}],getallparameters["RawFileDownload"]];
	If[FreeQ[params,#],Message[ServiceExecute::nparam,#1];Throw[$Failed]]&/@{"Root","Path"};
	rawdata=OAuthClient`rawoauthdata[id,"RawFileDownload",params];
	data=dropboximport[rawdata];
	data/;data=!=$Failed
]

dropboxcookeddata["ImportFile",id_,args_]:=Block[
	{params,rawdata, root,data, ext,res,url},
	params=filterparameters[Join[args,{"Root"->"dropbox"}],getallparameters["RawFileLink"]];
	If[FreeQ[params,#],Message[ServiceExecute::nparam,#1];Throw[$Failed]]&/@{"Root","Path"};
	rawdata=OAuthClient`rawoauthdata[id,"RawFileLink",params];
	data=dropboximportjson[rawdata];
	(url=Lookup[data,"url",$Failed];
		(	
			res=Import[url];
			res/;res=!=$Failed
		)/;url=!=$Failed
	)/;data=!=$Failed
]

dropboxcookeddata[prop:("UserData"),id_,args_]:=Block[
	{params,rawdata, root,data},
	params=filterparameters[args,getallparameters["RawUserData"]];
	rawdata=OAuthClient`rawoauthdata[id,"RawUserData",params];
	data=dropboximportjson[rawdata];
	Association[Replace[Normal[data],HoldPattern[Rule][a_,b_]:>Rule[camelcase[a],b],Infinity]/."Uid"->"UserID"]
]


dropboxcookeddata[prop:("FileData"|"DirectoryData"),id_,args_]:=Block[
	{params,rawdata, root,data},
	params=filterparameters[Join[args,{"Root"->"dropbox","Path"->""}]/."File"->"Path",getallparameters["RawPathData"]];
	rawdata=OAuthClient`rawoauthdata[id,"RawPathData",params];
	data=dropboximportjson[rawdata];
	If[Lookup[data,"is_dir",False],
		If[prop==="FileData",Message[ServiceExecute::nfile,"Path"/.params]];
		Association[Replace[Normal[data],HoldPattern[Rule][a_,b_]:>Rule[camelcase[a],b],Infinity]/.{
			fval["ClientMtime"->(readDate[#]&)],
			fval["Modified"->(readDate[#]&)]
			}]
		,
		If[prop==="DirectoryData",Message[ServiceExecute::ndir,"Path"/.params]];
		Association[Replace[Normal[data],HoldPattern[Rule][a_,b_]:>Rule[camelcase[a],b],Infinity]/.{
			fval["ClientMtime"->(readDate[#]&)],
			fval["Modified"->(readDate[#]&)]
			}]
	]
]

dropboxcookeddata["FileNames",id_,args_]:=Block[
	{data},
	data=dropboxcookeddata["DirectoryData",id,args];
	data=Lookup[data,"Contents",{}];
	"Path"/.data

]

dropboxcookeddata["DataUpload",id_,args_]:=Block[
	{params,rawdata, root,data},
	If[FreeQ[args,#],Message[ServiceExecute::nparam,#1];Throw[$Failed]]&/@{"Data"};
	params=filterparameters[Join[args,{"Root"->"dropbox", "overwrite" -> "true"}]/.{
		"Data"->"ParameterlessBodyData"},getallparameters["RawFileUpload"]]/.HoldPattern[Rule["overwrite",tf_]]:>Rule["overwrite",ToLowerCase[ToString[tf]]];
	If[FreeQ[params,#],Message[ServiceExecute::nparam,#1];Throw[$Failed]]&/@{"Root","Path"};
	params=params/.HoldPattern[Rule]["ParameterlessBodyData",d:Except[_String]]:>Rule["ParameterlessBodyData",ToString[InputForm[d]]];
	rawdata=OAuthClient`rawoauthdata[id,"RawFileUpload",params];
	data=dropboximportjson[rawdata];
	Association[
		Map[(camelcase[#]->data[#])&,{"modified", "revision", "rev","path", "root", "mime_type"}]/.{
			fval["Modified"->(readDate[#]&)]}
	]
]

dropboxcookeddata["GraphicsUpload",id_,args_]:=Block[
	{params,rawdata, root,data, ext},
	If[FreeQ[args,#],Message[ServiceExecute::nparam,#1];Throw[$Failed]]&/@{"Graphics"};
	params=filterparameters[Join[args,{"Root"->"dropbox", "overwrite" -> "true"}]/.{
		"Graphics"->"ParameterlessBodyData"},getallparameters["RawFileUpload"]]/.HoldPattern[Rule["overwrite",tf_]]:>Rule["overwrite",ToLowerCase[ToString[tf]]];
	If[FreeQ[params,#],Message[ServiceExecute::nparam,#1];Throw[$Failed]]&/@{"Root","Path"};
	ext=FileExtension["Path"/.params];
	data="ParameterlessBodyData"/.params;
	data=Check[ImportString[ExportString[data, ext], "Byte"],
		Message[ServiceExecute::grext, ext];Throw[$Failed]
	];
	params=params/.HoldPattern[Rule]["ParameterlessBodyData",g_]:>Rule["ParameterlessBodyData",data];
	rawdata=OAuthClient`rawoauthdata[id,"RawFileUpload",params];
	data=dropboximportjson[rawdata];
	
	Association[Map[(camelcase[#]->data[#])&,{"modified", "revision", "rev","path", "root", "mime_type"}]/.{
			fval["Modified"->(readDate[#]&)]}
	]
]

dropboxcookeddata["FileSearch",id_,args_]:=Block[
	{params,rawdata, root,data},
	params=filterparameters[Join[args,{"Root"->"dropbox","Path"->""}],getallparameters["RawFileSearch"]];
	If[FreeQ[params,#],Message[ServiceExecute::nparam,#1];Throw[$Failed]]&/@{"Root","Path","query"};
	rawdata=OAuthClient`rawoauthdata[id,"RawFileSearch",params];
	data=dropboximportjson[rawdata,True];
	Lookup[#,"path",Sequence@@{}]&/@data
]

$drobpoxdirectoryplotsizelimit=12;

dropboxcookeddata["DirectoryTreePlot",id_,args_]:=Block[
	{params,rawdata,OAuthClient`$CacheResults=True, path, root,data,flatdata},
	params=filterparameters[Join[args,{"Root"->"dropbox","Path"->""}],getallparameters["RawPathData"]];
	If[FreeQ[params,#],Message[ServiceExecute::nparam,#1];Throw[$Failed]]&/@{"Root","Path"};
	{root,path}={"Root","Path"}/.params;
	data=getcontents[id,root,path];
	flatdata=Flatten[Cases[data, HoldPattern[Rule[__]], {0, Infinity}] //. (Rule[a_, b_List]) :> (Thread[Rule[a, b]])];
	LayeredGraphPlot[Union[flatdata /. (Rule[a_, Rule[b_, _]]) :> Rule[a, b]]]
]

getcontents[id_,root_,path_]:=Module[
	{rawdata = OAuthClient`rawoauthdata[id,"RawPathData",{"Root"->root,"Path"->path}], subpaths, contents, dirQ, headdata, res},
	headdata=dropboximportjson[rawdata];
	res=If[TrueQ[headdata["is_dir"]],
		contents = headdata["contents"];
		If[Length[contents]>$drobpoxdirectoryplotsizelimit,
			contents=Join[Take[contents,$drobpoxdirectoryplotsizelimit],{{"path"->(path<>"/..."),"is_dir"->False}}];
		];
		subpaths = "path" /. contents;
		dirQ = "is_dir" /. contents;
		If[subpaths=!="path",			
			path -> MapThread[If[TrueQ[#2]&&!MatchQ[#1,"..."], getcontents[id, root,#1], #1 -> {}]&, {subpaths, dirQ}],
    		path -> {}
		]
		,
    	path -> {}
    ];
    res/.HoldPattern[ Rule[x_, {}]] :> x
]
    
dropboxcookeddata[___]:=$Failed 
(* Send Message *)

dropboxsendmessage[___]:=$Failed

(*** Service specific utilites ****)
filterparameters=OAuthClient`Private`filterParameters;
camelcase=OAuthClient`Private`camelCase;
fval=OAuthClient`Private`formatvalue;

readDate[date_, form_: DateObject] := 
 form@DateList[{StringSplit[date, {" +"," -"}][[1]], 
 	{"DayName",",", "Day", "MonthNameShort","Year", "Hour", ":", "Minute", ":",
      "Second"}}]
    
getallparameters[str_]:=DeleteCases[Flatten[{"Parameters","PathParameters","BodyData","MultipartData"}/.dropboxdata[str]],
	("Parameters"|"PathParameters"|"BodyData"|"MultipartData")]

formatrootpath[root_,path_]:=StringJoin[stripslash[root],"/",stripslash[path]]
stripslash[""|"/"]="";
stripslash[str_]:=If[StringTake[#,1]==="/",StringDrop[#,1],#]&@If[StringTake[str,-1]==="/",StringDrop[str,-1],str]

dbicon=Image[RawArray["Byte", {{{38, 38, 38, 217}, {99, 99, 99, 156}, {97, 97, 97, 158}, {99, 99, 99, 156}, {99, 99, 99, 
  156}, {99, 99, 99, 156}, {99, 99, 99, 156}, {99, 99, 99, 156}, {99, 99, 99, 156}, {99, 99, 99, 156}, {99, 99, 99, 
  156}, {99, 99, 99, 156}, {99, 99, 99, 156}, {99, 99, 99, 156}, {99, 99, 99, 156}, {99, 99, 99, 156}, {99, 99, 99, 
  156}, {99, 99, 99, 156}, {99, 99, 99, 156}, {99, 99, 99, 156}, {99, 99, 99, 156}, {99, 99, 99, 156}, {99, 99, 99, 
  156}, {99, 99, 99, 156}, {99, 99, 99, 156}, {99, 99, 99, 156}, {99, 99, 99, 156}, {99, 99, 99, 156}, {99, 99, 99, 
  156}, {98, 98, 98, 157}, {99, 99, 99, 156}, {22, 22, 22, 233}}, {{108, 108, 108, 147}, {255, 255, 255, 0}, {255, 
  255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, 
  {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 
  0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 
  255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 
  255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, 
  {63, 63, 63, 192}}, {{97, 97, 97, 158}, {251, 251, 251, 4}, {247, 247, 247, 8}, {251, 251, 251, 4}, {251, 251, 251, 
  4}, {251, 251, 251, 4}, {251, 251, 251, 4}, {251, 251, 251, 4}, {251, 251, 251, 4}, {251, 251, 251, 4}, {251, 251, 
  251, 4}, {251, 251, 251, 4}, {251, 251, 251, 4}, {251, 251, 251, 4}, {251, 251, 251, 4}, {251, 251, 251, 4}, {251, 
  251, 251, 4}, {251, 251, 251, 4}, {251, 251, 251, 4}, {251, 251, 251, 4}, {251, 251, 251, 4}, {251, 251, 251, 4}, 
  {251, 251, 251, 4}, {251, 251, 251, 4}, {251, 251, 251, 4}, {251, 251, 251, 4}, {251, 251, 251, 4}, {251, 251, 251, 
  4}, {251, 251, 251, 4}, {248, 248, 248, 7}, {251, 251, 251, 4}, {57, 57, 57, 198}}, {{99, 99, 99, 156}, {255, 255, 
  255, 0}, {251, 251, 251, 4}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 
  255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, 
  {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 
  0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 
  255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {252, 252, 252, 3}, {255, 
  255, 255, 0}, {58, 58, 58, 197}}, {{99, 99, 99, 156}, {255, 255, 255, 0}, {251, 251, 251, 4}, {255, 255, 255, 0}, 
  {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 
  0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 
  255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 
  255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, 
  {255, 255, 255, 0}, {255, 255, 255, 0}, {252, 252, 252, 3}, {255, 255, 255, 0}, {58, 58, 58, 197}}, {{99, 99, 99, 
  156}, {255, 255, 255, 0}, {251, 251, 251, 4}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 
  255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {253, 254, 255, 1}, {253, 254, 255, 3}, 
  {253, 254, 255, 2}, {254, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 
  0}, {253, 254, 255, 1}, {255, 255, 255, 2}, {253, 254, 255, 3}, {254, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 
  255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {252, 
  252, 252, 3}, {255, 255, 255, 0}, {58, 58, 58, 197}}, {{99, 99, 99, 156}, {255, 255, 255, 0}, {251, 251, 251, 4}, 
  {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 
  0}, {252, 253, 255, 3}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 1}, {254, 254, 
  255, 1}, {255, 255, 255, 0}, {255, 255, 255, 0}, {252, 254, 255, 2}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 
  255, 255, 0}, {255, 255, 255, 1}, {252, 254, 255, 2}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, 
  {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {252, 252, 252, 3}, {255, 255, 255, 0}, {58, 58, 58, 
  197}}, {{99, 99, 99, 156}, {255, 255, 255, 0}, {251, 251, 251, 4}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 
  255, 255, 0}, {255, 255, 255, 0}, {253, 254, 255, 1}, {253, 254, 255, 3}, {255, 255, 255, 0}, {218, 237, 251, 17}, 
  {71, 159, 236, 157}, {104, 178, 239, 126}, {255, 255, 255, 0}, {255, 255, 255, 0}, {252, 254, 255, 2}, {251, 253, 
  255, 4}, {255, 255, 255, 0}, {222, 239, 252, 17}, {53, 149, 234, 171}, {121, 186, 241, 106}, {250, 253, 255, 0}, 
  {255, 255, 255, 0}, {251, 253, 255, 4}, {254, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 
  0}, {255, 255, 255, 0}, {252, 252, 252, 3}, {255, 255, 255, 0}, {58, 58, 58, 197}}, {{99, 99, 99, 156}, {255, 255, 
  255, 0}, {251, 251, 251, 4}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {253, 254, 255, 2}, {255, 
  255, 255, 0}, {255, 255, 255, 0}, {148, 200, 244, 81}, {14, 130, 230, 221}, {0, 118, 227, 255}, {0, 117, 227, 255}, 
  {72, 161, 236, 158}, {239, 247, 253, 5}, {255, 255, 255, 0}, {255, 255, 255, 0}, {191, 223, 248, 42}, {22, 133, 
  230, 213}, {0, 119, 227, 255}, {0, 118, 227, 255}, {52, 150, 234, 179}, {199, 227, 249, 37}, {255, 255, 255, 0}, 
  {255, 255, 255, 1}, {254, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {252, 252, 252, 
  3}, {255, 255, 255, 0}, {58, 58, 58, 197}}, {{99, 99, 99, 156}, {255, 255, 255, 0}, {251, 251, 251, 4}, {255, 255, 
  255, 0}, {255, 255, 255, 0}, {254, 254, 255, 1}, {255, 255, 255, 0}, {219, 237, 252, 21}, {71, 160, 236, 160}, {0, 
  117, 227, 255}, {0, 125, 229, 255}, {3, 128, 229, 251}, {4, 128, 229, 251}, {0, 118, 227, 255}, {41, 145, 233, 
  193}, {202, 228, 250, 36}, {153, 203, 245, 80}, {7, 125, 229, 235}, {0, 123, 228, 255}, {3, 128, 229, 250}, {4, 
  128, 229, 252}, {0, 120, 228, 255}, {5, 125, 229, 240}, {123, 187, 241, 108}, {245, 250, 254, 0}, {255, 255, 255, 
  0}, {254, 254, 255, 1}, {255, 255, 255, 0}, {255, 255, 255, 0}, {252, 252, 252, 3}, {255, 255, 255, 0}, {58, 58, 
  58, 197}}, {{99, 99, 99, 156}, {255, 255, 255, 0}, {251, 251, 251, 4}, {255, 255, 255, 0}, {252, 254, 255, 2}, 
  {254, 255, 255, 1}, {163, 209, 246, 60}, {3, 122, 228, 241}, {0, 121, 228, 255}, {4, 128, 229, 252}, {1, 126, 229, 
  253}, {0, 126, 229, 254}, {4, 128, 229, 251}, {0, 124, 228, 255}, {0, 114, 227, 255}, {80, 166, 237, 126}, {37, 
  145, 233, 183}, {0, 114, 226, 255}, {1, 127, 229, 255}, {3, 128, 229, 252}, {0, 126, 229, 255}, {2, 127, 229, 252}, 
  {2, 127, 229, 254}, {0, 115, 227, 255}, {31, 139, 232, 193}, {224, 239, 252, 14}, {254, 254, 255, 1}, {254, 254, 
  255, 1}, {255, 255, 255, 0}, {252, 252, 252, 3}, {255, 255, 255, 0}, {58, 58, 58, 197}}, {{99, 99, 99, 156}, {255, 
  255, 255, 0}, {251, 251, 251, 4}, {255, 255, 255, 0}, {254, 254, 255, 1}, {253, 254, 255, 2}, {220, 237, 251, 22}, 
  {75, 162, 236, 159}, {0, 117, 227, 255}, {2, 127, 229, 254}, {1, 127, 229, 252}, {0, 126, 229, 255}, {0, 117, 227, 
  255}, {33, 139, 231, 201}, {173, 213, 246, 60}, {255, 255, 255, 0}, {244, 249, 254, 2}, {123, 187, 241, 108}, {6, 
  126, 229, 236}, {0, 119, 227, 255}, {2, 128, 229, 253}, {2, 127, 229, 252}, {0, 123, 228, 255}, {1, 123, 228, 242}, 
  {128, 190, 242, 103}, {246, 250, 254, 5}, {253, 254, 255, 1}, {255, 255, 255, 0}, {255, 255, 255, 0}, {252, 252, 
  252, 3}, {255, 255, 255, 0}, {58, 58, 58, 197}}, {{99, 99, 99, 156}, {255, 255, 255, 0}, {251, 251, 251, 4}, {255, 
  255, 255, 0}, {255, 255, 255, 0}, {254, 255, 255, 1}, {255, 255, 255, 0}, {254, 255, 255, 0}, {114, 182, 240, 110}, 
  {0, 119, 228, 251}, {0, 123, 228, 255}, {0, 123, 228, 242}, {114, 183, 241, 115}, {239, 247, 253, 4}, {255, 255, 
  255, 0}, {253, 254, 255, 2}, {255, 255, 255, 0}, {255, 255, 255, 0}, {204, 229, 250, 31}, {66, 156, 235, 166}, {0, 
  119, 227, 255}, {0, 122, 228, 255}, {16, 131, 230, 220}, {179, 217, 247, 51}, {255, 255, 255, 0}, {255, 255, 255, 
  1}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {252, 252, 252, 3}, {255, 255, 255, 0}, {58, 58, 
  58, 197}}, {{99, 99, 99, 156}, {255, 255, 255, 0}, {251, 251, 251, 4}, {255, 255, 255, 0}, {255, 255, 255, 0}, 
  {255, 255, 255, 0}, {254, 254, 255, 1}, {250, 253, 255, 6}, {255, 255, 255, 0}, {154, 204, 245, 68}, {36, 137, 231, 
  183}, {214, 234, 251, 18}, {255, 255, 255, 0}, {254, 254, 255, 3}, {252, 253, 255, 3}, {255, 255, 255, 0}, {254, 
  255, 255, 0}, {250, 252, 255, 4}, {255, 255, 255, 0}, {255, 255, 255, 0}, {151, 202, 244, 76}, {34, 137, 231, 184}, 
  {230, 243, 253, 6}, {255, 255, 255, 0}, {249, 252, 255, 5}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 
  0}, {255, 255, 255, 0}, {252, 252, 252, 3}, {255, 255, 255, 0}, {58, 58, 58, 197}}, {{99, 99, 99, 156}, {255, 255, 
  255, 0}, {251, 251, 251, 4}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {252, 253, 255, 3}, {255, 
  255, 255, 0}, {229, 243, 253, 11}, {78, 164, 236, 151}, {15, 128, 230, 225}, {115, 182, 240, 115}, {233, 244, 253, 
  11}, {255, 255, 255, 0}, {253, 254, 255, 3}, {253, 254, 255, 1}, {252, 253, 255, 3}, {255, 255, 255, 0}, {255, 255, 
  255, 0}, {199, 226, 249, 40}, {72, 160, 236, 164}, {15, 128, 229, 226}, {139, 195, 243, 92}, {255, 255, 255, 0}, 
  {255, 255, 255, 1}, {253, 254, 255, 1}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {252, 252, 252, 
  3}, {255, 255, 255, 0}, {58, 58, 58, 197}}, {{99, 99, 99, 156}, {255, 255, 255, 0}, {251, 251, 251, 4}, {255, 255, 
  255, 0}, {255, 255, 255, 0}, {253, 254, 255, 1}, {255, 255, 255, 0}, {201, 228, 250, 32}, {31, 139, 231, 200}, {0, 
  117, 227, 255}, {0, 126, 229, 255}, {0, 116, 227, 255}, {28, 138, 231, 209}, {158, 205, 245, 71}, {255, 255, 255, 
  0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {238, 247, 253, 5}, {108, 179, 240, 119}, {1, 123, 228, 241}, {0, 119, 
  227, 255}, {0, 125, 229, 255}, {0, 115, 227, 255}, {87, 168, 237, 140}, {243, 249, 254, 0}, {255, 255, 255, 0}, 
  {254, 254, 255, 1}, {255, 255, 255, 0}, {255, 255, 255, 0}, {252, 252, 252, 3}, {255, 255, 255, 0}, {58, 58, 58, 
  197}}, {{99, 99, 99, 156}, {255, 255, 255, 0}, {251, 251, 251, 4}, {255, 255, 255, 0}, {252, 254, 255, 2}, {255, 
  255, 255, 1}, {165, 209, 246, 56}, {0, 117, 227, 247}, {0, 123, 228, 255}, {5, 129, 230, 249}, {0, 126, 229, 255}, 
  {4, 128, 229, 251}, {0, 123, 228, 255}, {0, 118, 227, 255}, {71, 160, 236, 160}, {199, 227, 249, 40}, {162, 208, 
  245, 72}, {33, 139, 231, 202}, {0, 117, 227, 255}, {1, 126, 229, 255}, {2, 128, 229, 252}, {0, 126, 229, 254}, {7, 
  130, 230, 248}, {0, 115, 227, 255}, {33, 141, 232, 193}, {224, 240, 252, 13}, {253, 254, 255, 2}, {254, 254, 255, 
  1}, {255, 255, 255, 0}, {252, 252, 252, 3}, {255, 255, 255, 0}, {58, 58, 58, 197}}, {{99, 99, 99, 156}, {255, 255, 
  255, 0}, {251, 251, 251, 4}, {255, 255, 255, 0}, {254, 254, 255, 1}, {254, 254, 255, 1}, {211, 233, 251, 26}, {85, 
  166, 237, 148}, {0, 121, 228, 253}, {0, 122, 228, 255}, {3, 128, 229, 251}, {0, 126, 229, 255}, {3, 128, 229, 251}, 
  {2, 126, 229, 255}, {0, 116, 227, 255}, {71, 162, 236, 125}, {37, 145, 233, 181}, {0, 115, 227, 255}, {5, 129, 229, 
  249}, {1, 126, 229, 254}, {1, 126, 229, 254}, {3, 128, 229, 253}, {0, 118, 227, 255}, {11, 128, 229, 226}, {128, 
  190, 242, 102}, {240, 248, 254, 7}, {253, 254, 255, 1}, {254, 255, 255, 0}, {255, 255, 255, 0}, {252, 252, 252, 3}, 
  {255, 255, 255, 0}, {58, 58, 58, 197}}, {{99, 99, 99, 156}, {255, 255, 255, 0}, {251, 251, 251, 4}, {255, 255, 255, 
  0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {171, 212, 246, 64}, {29, 137, 
  231, 208}, {0, 118, 227, 255}, {1, 127, 229, 255}, {0, 123, 228, 255}, {11, 128, 229, 236}, {84, 169, 237, 119}, 
  {46, 147, 233, 185}, {65, 159, 236, 153}, {63, 158, 236, 152}, {0, 121, 228, 255}, {1, 126, 229, 255}, {0, 124, 
  228, 255}, {0, 117, 227, 255}, {73, 160, 236, 162}, {208, 231, 250, 24}, {255, 255, 255, 0}, {255, 255, 255, 0}, 
  {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {252, 252, 252, 3}, {255, 255, 255, 0}, {58, 58, 58, 
  197}}, {{99, 99, 99, 156}, {255, 255, 255, 0}, {251, 251, 251, 4}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 
  255, 255, 0}, {254, 254, 255, 1}, {252, 253, 255, 4}, {255, 255, 255, 0}, {233, 244, 253, 9}, {95, 172, 238, 122}, 
  {7, 125, 229, 242}, {23, 135, 231, 216}, {85, 170, 238, 116}, {28, 137, 231, 209}, {0, 120, 228, 255}, {0, 121, 
  228, 255}, {55, 152, 234, 169}, {75, 164, 236, 134}, {4, 125, 229, 248}, {27, 137, 231, 211}, {151, 201, 244, 77}, 
  {255, 255, 255, 0}, {255, 255, 255, 0}, {251, 253, 255, 3}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 
  0}, {255, 255, 255, 0}, {252, 252, 252, 3}, {255, 255, 255, 0}, {58, 58, 58, 197}}, {{99, 99, 99, 156}, {255, 255, 
  255, 0}, {251, 251, 251, 4}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 
  255, 255, 0}, {250, 253, 255, 4}, {255, 255, 255, 0}, {90, 172, 238, 131}, {80, 165, 237, 129}, {89, 172, 238, 
  121}, {15, 129, 230, 232}, {0, 122, 228, 255}, {2, 127, 229, 252}, {2, 127, 229, 252}, {0, 119, 227, 255}, {37, 
  143, 232, 199}, {98, 176, 239, 104}, {63, 156, 235, 154}, {172, 213, 247, 80}, {254, 255, 255, 2}, {249, 252, 255, 
  5}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {252, 252, 
  252, 3}, {255, 255, 255, 0}, {58, 58, 58, 197}}, {{99, 99, 99, 156}, {255, 255, 255, 0}, {251, 251, 251, 4}, {255, 
  255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {253, 254, 255, 2}, 
  {253, 254, 255, 1}, {130, 192, 242, 106}, {11, 127, 229, 224}, {0, 119, 227, 255}, {0, 125, 229, 255}, {3, 128, 
  229, 250}, {0, 126, 229, 255}, {0, 126, 229, 255}, {5, 129, 230, 249}, {0, 121, 228, 255}, {0, 120, 227, 252}, {33, 
  138, 231, 205}, {198, 226, 249, 52}, {254, 254, 255, 1}, {253, 254, 255, 2}, {255, 255, 255, 0}, {255, 255, 255, 
  0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {252, 252, 252, 3}, {255, 255, 255, 0}, {58, 58, 
  58, 197}}, {{99, 99, 99, 156}, {255, 255, 255, 0}, {251, 251, 251, 4}, {255, 255, 255, 0}, {255, 255, 255, 0}, 
  {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {254, 254, 255, 1}, {255, 255, 255, 
  0}, {208, 231, 250, 27}, {74, 161, 236, 154}, {0, 119, 228, 254}, {0, 122, 228, 255}, {4, 129, 229, 251}, {3, 127, 
  229, 254}, {0, 118, 227, 255}, {11, 128, 229, 231}, {124, 187, 241, 112}, {245, 251, 254, 1}, {255, 255, 255, 0}, 
  {254, 255, 255, 1}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 
  0}, {255, 255, 255, 0}, {252, 252, 252, 3}, {255, 255, 255, 0}, {58, 58, 58, 197}}, {{99, 99, 99, 156}, {255, 255, 
  255, 0}, {251, 251, 251, 4}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 
  255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {252, 254, 255, 3}, {255, 255, 255, 0}, {255, 255, 255, 0}, 
  {168, 210, 246, 61}, {37, 141, 232, 196}, {0, 116, 227, 255}, {0, 119, 227, 255}, {77, 163, 237, 155}, {212, 233, 
  251, 27}, {255, 255, 255, 0}, {255, 255, 255, 1}, {253, 254, 255, 2}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 
  255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {252, 252, 252, 3}, 
  {255, 255, 255, 0}, {58, 58, 58, 197}}, {{99, 99, 99, 156}, {255, 255, 255, 0}, {251, 251, 251, 4}, {255, 255, 255, 
  0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 
  255, 0}, {255, 255, 255, 0}, {253, 254, 255, 1}, {253, 254, 255, 3}, {255, 255, 255, 0}, {241, 248, 254, 0}, {124, 
  187, 241, 103}, {165, 209, 246, 69}, {255, 255, 255, 0}, {255, 255, 255, 0}, {251, 253, 255, 4}, {254, 255, 255, 
  0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 
  255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {252, 252, 252, 3}, {255, 255, 255, 0}, {58, 58, 58, 197}}, {{99, 
  99, 99, 156}, {255, 255, 255, 0}, {251, 251, 251, 4}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, 
  {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 
  0}, {255, 255, 255, 0}, {252, 253, 255, 3}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {254, 254, 
  255, 3}, {253, 254, 255, 1}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 
  255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, 
  {252, 252, 252, 3}, {255, 255, 255, 0}, {58, 58, 58, 197}}, {{99, 99, 99, 156}, {255, 255, 255, 0}, {251, 251, 251, 
  4}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 
  255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {254, 
  254, 255, 1}, {251, 253, 255, 4}, {252, 253, 255, 3}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, 
  {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 
  0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {252, 252, 252, 3}, {255, 255, 255, 0}, {58, 58, 
  58, 197}}, {{99, 99, 99, 156}, {255, 255, 255, 0}, {251, 251, 251, 4}, {255, 255, 255, 0}, {255, 255, 255, 0}, 
  {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 
  0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 
  255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 
  255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, 
  {255, 255, 255, 0}, {252, 252, 252, 3}, {255, 255, 255, 0}, {58, 58, 58, 197}}, {{99, 99, 99, 156}, {255, 255, 255, 
  0}, {251, 251, 251, 4}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 
  255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 
  255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, 
  {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 
  0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {252, 252, 252, 3}, {255, 255, 
  255, 0}, {58, 58, 58, 197}}, {{98, 98, 98, 157}, {252, 252, 252, 3}, {248, 248, 248, 7}, {252, 252, 252, 3}, {252, 
  252, 252, 3}, {252, 252, 252, 3}, {252, 252, 252, 3}, {252, 252, 252, 3}, {252, 252, 252, 3}, {252, 252, 252, 3}, 
  {252, 252, 252, 3}, {252, 252, 252, 3}, {252, 252, 252, 3}, {252, 252, 252, 3}, {252, 252, 252, 3}, {252, 252, 252, 
  3}, {252, 252, 252, 3}, {252, 252, 252, 3}, {252, 252, 252, 3}, {252, 252, 252, 3}, {252, 252, 252, 3}, {252, 252, 
  252, 3}, {252, 252, 252, 3}, {252, 252, 252, 3}, {252, 252, 252, 3}, {252, 252, 252, 3}, {252, 252, 252, 3}, {252, 
  252, 252, 3}, {252, 252, 252, 3}, {249, 249, 249, 6}, {252, 252, 252, 3}, {57, 57, 57, 198}}, {{105, 105, 105, 
  150}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 
  255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, 
  {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 
  0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 
  255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 
  255, 255, 0}, {255, 255, 255, 0}, {62, 62, 62, 193}}, {{23, 23, 23, 232}, {58, 58, 58, 197}, {57, 57, 57, 198}, 
  {58, 58, 58, 197}, {58, 58, 58, 197}, {58, 58, 58, 197}, {58, 58, 58, 197}, {58, 58, 58, 197}, {58, 58, 58, 197}, 
  {58, 58, 58, 197}, {58, 58, 58, 197}, {58, 58, 58, 197}, {58, 58, 58, 197}, {58, 58, 58, 197}, {58, 58, 58, 197}, 
  {58, 58, 58, 197}, {58, 58, 58, 197}, {58, 58, 58, 197}, {58, 58, 58, 197}, {58, 58, 58, 197}, {58, 58, 58, 197}, 
  {58, 58, 58, 197}, {58, 58, 58, 197}, {58, 58, 58, 197}, {58, 58, 58, 197}, {58, 58, 58, 197}, {58, 58, 58, 197}, 
  {58, 58, 58, 197}, {58, 58, 58, 197}, {57, 57, 57, 198}, {58, 58, 58, 197}, {13, 13, 13, 242}}}], "Byte", 
 ColorSpace -> "RGB", Interleaving -> True];

End[] (* End Private Context *)
           		
End[]


SetAttributes[{},{ReadProtected, Protected}];

System`Private`RestoreContextPath[];

(* Return three functions to define oauthservicedata, oauthcookeddata, oauthsendmessage  *)
{DropboxOAuth`Private`dropboxdata,DropboxOAuth`Private`dropboxcookeddata,DropboxOAuth`Private`dropboxsendmessage}
