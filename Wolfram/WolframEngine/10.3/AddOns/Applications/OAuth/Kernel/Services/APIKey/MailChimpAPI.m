
System`Private`NewContextPath[{"OAuthClient`","System`"}];

Begin["MailChimpAPI`"] (* Begin Private Context *) 

Begin["`Private`"](* Begin Private Context *) 

(******************************* MailChimp *************************************)

(* Authentication information *)

mailchimpdata[]={
		"ServiceName" 		-> "MailChimp", 
        "URLFetchFun"		:> (Block[{dc,url,params=Lookup[{##2},"Parameters",{}]},
                                    dc=Lookup[params,"apikey",Throw[$Failed]];
                                    url=StringCases[dc,"-"~~x__->x][[1]]<>"."<>#1;
                                    URLFetch[URLBuild[Association["Scheme"->"https","Domain"->url]],
                                        Sequence@@FilterRules[{##2},Except["Parameters"|"Headers"]], 
                                            "Parameters" -> Join[params,{"apikey"-> Lookup[params,"apikey",Throw[$Failed]]}],
                                            "Headers" -> {}
                                            ]
                                    ]
                                &),
        "ClientInfo"		:> OAuthDialogDump`Private`KeyDialog["MailChimp"],
	 	"Gets"				-> {"Lists","Subscribers","Stats","Campaigns","Templates"},
	 	"Posts"				-> {"AddSubscriberToList"},
	 	"RawGets"			-> {"RawLists","RawSubscribers","RawStats","RawCampaigns","RawTemplates"},
	 	"RawPosts"			-> {"RawAddSubscriberToList"},
 		"Information"		-> "Testing MailChimp"
}

mailchimpimport[rawdata_]:=Association/@(ImportString[rawdata,"JSON"])
(* Raw *)
mailchimpdata["RawLists"] := {
        "URL"				-> URLBuild[{"api.mailchimp.com","2.0","lists","list.json"}],
        "HTTPSMethod"		-> "GET",
        "Parameters"		-> {},
        "RequiredParameters"-> {},
        "ResultsFunction"	-> mailchimpimport
    } 

mailchimpdata["RawSubscribers"] := {
        "URL"               -> URLBuild[{"api.mailchimp.com","2.0","lists","members.json"}],
        "HTTPSMethod"       -> "GET",
        "Parameters"        -> {"id"},
        "RequiredParameters"-> {"id"},
        "ResultsFunction"   -> mailchimpimport
    } 
 
mailchimpdata["RawStats"] := {
        "URL"               -> URLBuild[{"api.mailchimp.com","2.0","reports","opened.json"}],
        "HTTPSMethod"       -> "GET",
        "Parameters"        -> {"cid"},
        "RequiredParameters"-> {"cid"},
        "ResultsFunction"   -> mailchimpimport
    }

mailchimpdata["RawCampaigns"] := {
        "URL"               -> URLBuild[{"api.mailchimp.com","2.0","campaigns","list.json"}],
        "HTTPSMethod"       -> "GET",
        "Parameters"        -> {},
        "RequiredParameters"-> {},
        "ResultsFunction"   -> mailchimpimport
    } 

mailchimpdata["RawTemplates"] := {
        "URL"               -> URLBuild[{"api.mailchimp.com","2.0","templates","list.json"}],
        "HTTPSMethod"       -> "GET",
        "Parameters"        -> {},
        "RequiredParameters"-> {},
        "ResultsFunction"   -> mailchimpimport
    } 
(* Raw Post *)
mailchimpdata["RawAddSubscriberToList"] = {
        "URL"               -> URLBuild[{"api.mailchimp.com","2.0","lists","subscribe.json"}],
        "Parameters"          -> {"id","email[email]"},
        "HTTPSMethod"       -> "POST",
        "ResultsFunction"   -> mailchimpimport
    }

(* Cooked *)
mailchimpcookeddata[req_, id_]:=MailChimpcookeddata[req, id,{}]

mailchimpcookeddata["Lists", id_] := Block[{rawdata},
	rawdata=KeyClient`rawkeydata[id,"RawLists"];
	mailchimpimport@rawdata
]

mailchimpcookeddata["Campaigns", id_] := Block[{rawdata},
    rawdata=KeyClient`rawkeydata[id,"RawCampaigns"];
    mailchimpimport@rawdata
]

mailchimpcookeddata["Subscribers", id_,args_] := Block[{rawdata},
    rawdata=KeyClient`rawkeydata[id,"RawSubscribers",args];
    mailchimpimport@rawdata
]

mailchimpcookeddata["Stats", id_,args_] := Block[{rawdata},
    rawdata=KeyClient`rawkeydata[id,"RawStats",args];
    rawdata
]

mailchimpcookeddata["Templates", id_] := Block[{rawdata},
    rawdata=KeyClient`rawkeydata[id,"RawTemplates"];
    rawdata
]
(* Cooked Post *)
mailchimpcookeddata["AddSubscriberToList",id_,args_]:=Module[
    {rawdata, params},
    params=filterparameters[args,getallparameters["RawAddSubscriberToList"]];
    rawdata=KeyClient`rawkeydata[id,"RawAddSubscriberToList",args]; 
    rawdata
]
mailchimpcookeddata[___]:=$Failed

mailchimpsendmessage[___]:=$Failed


(* Utilities *)
getallparameters[str_]:=DeleteCases[Flatten[{"Parameters","PathParameters","BodyData","MultipartData"}/.mailchimpdata[str]],
	("Parameters"|"PathParameters"|"BodyData"|"MultipartData")]
End[]

End[]

SetAttributes[{},{ReadProtected, Protected}];

System`Private`RestoreContextPath[];

{MailChimpAPI`Private`mailchimpdata,MailChimpAPI`Private`mailchimpcookeddata,MailChimpAPI`Private`mailchimpsendmessage}
