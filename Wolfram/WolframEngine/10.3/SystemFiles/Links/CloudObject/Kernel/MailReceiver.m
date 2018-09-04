BeginPackage["MailReceiver`"]

System`MailReceiverFunction
System`MailResponseFunction
System`ReturnReceiptFunction

Begin["`Private`"]


$UserCloudObjectURLs=True;
$MailReceiverAddressFunction=Automatic;
$MailReceiverAddressTemplate:=$MailReceiverAddressTemplate = StringTemplate["receiver+`1`@wolframcloud.com"]

(****************** Cloud Deploy **************************)
System`MailReceiverFunction/:CloudDeploy[System`MailReceiverFunction[mrfargs___]]:=Catch[cloudDeployMRF[{mrfargs}]]
System`MailReceiverFunction/:CloudDeploy[System`MailReceiverFunction[mrfargs___],cdargs__]:=Catch[cloudDeployMRF[{mrfargs},cdargs]]

cloudDeployMRF[receiver_List,cdargs___]:=clouddeployMRF[createReceiver@@receiver, cdargs]

clouddeployMRF[receiver_Association,cdargs___]:=Block[{co, cdargs1, info, id, shortid,url,email},
    If[!$CloudConnected,CloudConnect[]];
    If[!$CloudConnected,Message[System`MailReceiverFunction::cloudc];Throw[$Failed]];
    cdargs1=checkCDargs[cdargs];
    co=CloudDeploy[APIFunction[{"data"->"String"},
          (handlemail[receiver,importmailString[#data]]&)
       ],
       Sequence@@cdargs1,
       Permissions->{All->"Execute"}
    ];
    If[Head[co]=!=CloudObject,
        Message[System`MailReceiverFunction::noco];Throw[$Failed]
    ];
    email=createMRFEmail[co];
    co=setcloudinfo[co,email];
    setMRFTypesetting[co,email];
    co
]

createMRFEmail[co_]:=If[$MailReceiverAddressFunction===Automatic,
    $MailReceiverAddressTemplate[createShortID[co]]
    ,
    $MailReceiverAddressFunction[co]
]

createShortID[co_]:=Block[{url, id,shortid},
    (* always use UUID ? *)
    If[$UserCloudObjectURLs,
        url=First[co];
        ,   
        id=getclouduuid[co];
        If[StringQ[id],
            Message[System`MailReceiverFunction::noco];Throw[$Failed]
        ];
        url=$CloudBase <> "/objects/" <> id
    ];
    shortid=URLShorten[url];
    If[StringQ[shortid],
        FileNameTake[shortid],
        getclouduuid[co]
    ]
    
]

setcloudinfo[co_, email_]:=Block[{res},
    res=SetOptions[co,MetaInformation->{"EmailAddress"->email}];
    If[res===Null||ListQ[res],
        CloudObject[First[co],MetaInformation->{"EmailAddress"->email}]
        ,
        $Failed
    ]   
]

getclouduuid[co_]:=getclouduuid[co]=With[{info=System`CloudObjectInformation[co]},
    If[Head[info]=!=System`CloudObjectInformationData,
        Message[System`MailReceiverFunction::noco];Throw[$Failed]
    ];
    First[info]["UUID"]
]

checkCDargs[args___]:={args}/;FreeQ[{args},Permissions]
checkCDargs[___]:=(Message[System`MailReceiverFunction::perms];Throw[$Failed])
(****************** LOCAL OPERATION ************************)

System`MailReceiverFunction[mrfargs___][args___]:=Catch[handleMail[{mrfargs},{args}]]

Options[System`MailReceiverFunction]={System`MailResponseFunction->Automatic,System`ReturnReceiptFunction->Automatic}

handleMail[receiver_List,mail_List]:=handlemail[createReceiver@@receiver,importmail[First[mail],Rest[mail]]]/;Length[mail]>0
handleMail[receiver_List,mail_List]:=(Message[System`MailReceiverFunction::invmail,mail];$Failed)

handleMail[___]:=$Failed

handlemail[receiver_,{mail_, rawmail_}]:=Block[{res,raw,prepped, co, receiveraddress=Missing["NotAvailable"]},
	co=$EvaluationCloudObject;
	If[Head[co]===CloudObject,
		receiveraddress="EmailAddress" /. (MetaInformation /. Options[co, MetaInformation]);
		If[receiveraddress==="EmailAddress"||!StringQ[receiveraddress],
			receiveraddress=Missing["NotAvailable"]
		]
	];
    (* import mail *)
    raw=Lookup[receiver,"Import",Identity][mail];
    If[raw===$Failed,Throw[$Failed]];
    (* prefix? Interpreters? *)
    prepped=Lookup[receiver,"Prefix",Identity][raw];
    If[prepped===$Failed,Throw[$Failed]];
    prepped=addMailReceiverKeys[prepped,rawmail,receiveraddress];
    (* receiver *)
    res=Lookup[receiver,"Receive",Identity][prepped];
    If[res===$Failed,Throw[$Failed]];
    (* interpret results *)
    res=Lookup[receiver,"Interpret",Identity][res];
    If[res===$Failed,Throw[$Failed]];
    (* response *)
    Lookup[receiver,"Response",First][Join[prepped,Association["Result"->res]], rawmail];
    (* receipt *)
    handleReceipt[Lookup[receiver,"ReturnReceipt",Identity],Join[prepped,Association["Result"->res]]];
    (* returned results *)
    Lookup[receiver,"Result",Identity][res]
]

handlemail[___]:=$Failed

createReceiver[fun_Function,opts___?OptionQ]:=createReceiver[{},fun, opts]

(* Future: interpretation in first argument *)
createReceiver[{},fun_Function, opts:OptionsPattern[System`MailReceiverFunction]]:=Block[{responsefun,receiptfun},
    responsefun=createResponseFunction[OptionValue[System`MailResponseFunction]];
    receiptfun=createReceiptFunction[OptionValue[System`ReturnReceiptFunction]];
    Association[
        "Receive"->fun,
        "Response"->responsefun ,
        "ReturnReceipt"->receiptfun
    ]
]


$predefinedfunctions={}

predefinedfunction[_]:=Throw[$Failed]

createReceiver[interp_,str:(Alternatives@@$predefinedfunctions), opts___]:=Block[{},
    createReceiver[interp,predefinedfunction[str],opts]
]

createReceiver[_,expr_,___]:=(Message[System`MailReceiverFunction::invfun,expr];Throw[$Failed])

createResponseFunction[f_Function]:=customresponseFunction[f];
createResponseFunction[True]=defaultResponseFun;
createResponseFunction[False]=Null;
createResponseFunction[Automatic]=(If[humanSenderQ[#2],defaultResponseFun[##],Null]&);
createResponseFunction[expr_]:=(Message[System`MailResponseFunction::invopt, expr];Throw[$Failed]);


humanSenderRegex=
    RegularExpression[
    	"(?m)(^(((Resent-)?(From|Sender)|X-Envelope-From):|>?From)([^>]*[^(.%@a-z0-9])?(Post(ma(st(er)?|n)|office)|(send)?Mail(er)?|daemon|mmdf|n?uucp|ops|r(esponse|oot)|(bbs\\.)?smtp(error)?|s(erv(ices?|er)|ystem)|A(dmin(istrator)?|MMGR))(([^).!:a-z0-9][-_a-z0-9]*)?[%@>\\t ][^<)]*(\\(.*\\).*)?)?$([^>]|$))"]

humanSenderQ[rawmail_]:=StringFreeQ[rawmail,humanSenderRegex,IgnoreCase->True]

mrfAddressQ[address_String]:=(!StringFreeQ[address,"receiver"])&&(!StringFreeQ[address,"wolfram"])
mrfAddressQ[address_]:=False

defaultResponseFun[as_,raw_]:=
    With[{from=getReplyAddress[as],
    	subj=ToString[Lookup[as,"Subject",Missing["NotAvailable"]]], mrf=Lookup[as,"ReceiverAddress",Missing["NotAvailable"]]},
		If[!mrfAddressQ[from],
	        sendmailmrf[][Association[
	        	
	        	"To"->from,
	        	Sequence@@If[StringQ[mrf],
	        		{"From"->mrf},
	        		{}
	        	],
	        	"Subject"->"Automatic Response re: "<>subj,
	        	
	        	"Body"->"Your message was received."<>
				"\n\nDestination: "<>ToString[mrf]<>
				"\n\nSubject: "<>subj<>
				"\n\nSize: "<>ToString[Round[ByteCount[raw]/1000]]<>" kb"<>
				"\n\nMessage-ID: "<>ToString[Lookup[as,"MessageID",Missing["NotAvailable"]]]
            ]
	        ]
	    ]
    ]
    
customresponseFunction[fun_]:=(With[{
	from=getReplyAddress[#],
	mrf=Lookup[#,"ReceiverAddress",Missing["NotAvailable"]]},
	If[!mrfAddressQ[from],
		If[StringQ[mrf],
	        sendmailmrf[][
	        	fun[#],
	        	"To"->from,
	            "From"->mrf
	        ]
	        ,
	        sendmailmrf[][
                fun[#],
                "To"->from
            ]
		]
    ]
]&)

createReceiptFunction[True]=(True&);
createReceiptFunction[False]=(False&);
createReceiptFunction[Automatic]=createReceiptFunction[True];
createReceiptFunction[f_Function]:=f;
createReceiptFunction[expr_]:=(Message[System`ReturnReceiptFunction::invopt, expr];Throw[$Failed]);

sendreceipt[to_String, as_]:=With[{mrf=Lookup[as,"ReceiverAddress",Missing["NotAvailable"]]},
	sendmailmrf[][Association["To" -> to, 
	
        Sequence@@If[StringQ[mrf],
            {"From"->mrf},
            {}
        ],
	
        "Subject" -> "Read Receipt: "<>Lookup[as,"Subject",""], 
        "Body" -> 
            "The message sent on "<>DateString[Lookup[as,"OriginatingDate",DateObject[]]]<>" to "<>
            ToString[Lookup[as,"ReceiverAddress",""]]<>" with subject \""<>ToString[Lookup[as,"Subject",""]]<>"\" has been received."]]
]
   
sendreceipt[_]:=Null

handleReceipt[fun_, as_]:=Block[{from, res},
	res=fun[as];
	If[TrueQ[res],
	   from=getReturnReceiptAddress[as];
       sendreceipt[from,as]
	]
]

getReplyAddress[as_]:=Block[{address},
	address=Lookup[as,"ReplyTo"];
	If[StringQ[address],
		address=Quiet[takeAddress[address]]
	];
	If[!StringQ[address],
        address=Lookup[as,"ReturnPath"],
        If[StringQ[address],
	        address=Quiet[takeAddress[address]]
	    ]
    ];
    If[!StringQ[address],
        address=Lookup[as,"From"],
        If[StringQ[address],
            address=Quiet[takeAddress[address]]
        ]
    ];
    address
]

getReturnReceiptAddress[as_]:=Block[{address},
    address=checkHeaders[as,"DispositionNotificationTo"];
    If[StringQ[address],
        address=Quiet[takeAddress[address]]
    ];
    If[!StringQ[address],
        address=checkHeaders[as,"ReturnReceiptTo"],
        If[StringQ[address],
            address=Quiet[takeAddress[address]]
        ]
    ];
    address
]

(* importing *)
importmail[$Failed,_]:=$Failed
importmail[as_Association,_]:={as,Missing["NotAvailable"]}/;KeyExistsQ[as,"From"]
importmail[as_Association,_]:=(Message[System`MailReceiverFunction::nfrom];Throw[$Failed])
importmail[file_System`File,rest_]:=importmailfile[First[file],rest]
importmailfile[file_String,rest_]:=If[StringQ[FindFile[file]],
    With[{res=Import[file,"MBOX"]},
        If[Length[res]>0,
            {First[importmail[First[res],rest]],Import[file,"Text"]},
            (Message[System`MailReceiverFunction::nomail,file];Throw[$Failed])
        ]
    ]
    ,
    (Message[System`MailReceiverFunction::nfile, file];Throw[$Failed])
]/;MatchQ[FileExtension[file],"MBOX"|"mbox"]
    
importmail[str_String,rest_]:=importmailString[str]

importmailString[str_]:=With[{res=ImportString[str,"MBOX"]},
        If[ListQ[res],
            {Association[First[res]],str},
            (Message[System`MailReceiverFunction::invmail,str];Throw[$Failed])
        ]
    ]



importmail[l:{_Rule...},rest_]:=importmail[Association[l],rest]
importmail[mail_,_]:=(Message[System`MailReceiverFunction::invmail,mail];Throw[$Failed])


addMailReceiverKeys[as0_, rawmail_,receiveraddress_]:=Module[{as=as0},
	With[{ip=takelast[checkHeaders[as, "Received"],"IPAddress"], 
		date=checkHeaders[as, "Date"],received=Flatten[{checkHeaders[as, "Received"]}]},
	    If[!KeyExistsQ[as,"ToList"],
	    	If[KeyExistsQ[as,"To"],
	            AssociateTo[as,"ToList"->DeleteCases[Flatten[{as["To"]}],receiveraddress]],
	            AssociateTo[as,"ToList"->{}]
	    	]
	    	,
	        as=MapAt[DeleteCases[#,receiveraddress]&,as,"ToList"];
	    ];
	    
	    If[!KeyExistsQ[as,"CcList"],
            If[KeyExistsQ[as,"Cc"],
                AssociateTo[as,"CcList"->DeleteCases[Flatten[{as["Cc"]}],receiveraddress]],
                AssociateTo[as,"CcList"->toemaillist[checkHeaders[as,{"Cc","CC"}]]]
            ]
            ,
            as=MapAt[DeleteCases[#,receiveraddress]&,as,"CcList"];
        ];
	    If[!KeyExistsQ[as,"Body"],
	       AssociateTo[as,"Body"->as["Data"]]
	    ];
	    If[!KeyExistsQ[as,"ReceiverAddress"],
	       AssociateTo[as,"ReceiverAddress"->receiveraddress]
	    ];
	    If[!KeyExistsQ[as,"FromAddress"],
	       AssociateTo[as,"FromAddress"->takeAddress[as["From"]]]
	    ];
	    If[!KeyExistsQ[as,"FromName"],
	       AssociateTo[as,"FromName"->takeName[as["From"]]]
	    ];
        If[!KeyExistsQ[as,"AttachmentNames"],
        	If[KeyExistsQ[as,"AttachmentAssociations"],
                AssociateTo[as,"AttachmentNames"->Lookup[as["AttachmentAssociations"],"Name"]]
                ,
                AssociateTo[as,"AttachmentNames"->takenames[Lookup[as,"Attachments",{}]]]
        	]
        ];
	    If[KeyExistsQ[as,"Attachments"],
	       as=MapAt[takeattachments,as,"Attachments"],
	       AssociateTo[as,"Attachments"->{}]
	    ];
        If[!KeyExistsQ[as,"AttachmentAssociations"],
           AssociateTo[as,"AttachmentAssociations"->{}]
        ];
	    If[!KeyExistsQ[as,"ToAddressList"],
	       AssociateTo[as,"ToAddressList"->DeleteCases[takeAddress[as["ToList"]],receiveraddress]]
	    ];
	    If[!KeyExistsQ[as,"ToNameList"],
	       AssociateTo[as,"ToNameList"->takeName[as["ToList"]]]
	    ];
	    If[!KeyExistsQ[as,"CcAddressList"],
	       AssociateTo[as,"CcAddressList"->DeleteCases[takeAddress[as["CcList"]],receiveraddress]]
	    ];
	    If[!KeyExistsQ[as,"CcNameList"],
	       AssociateTo[as,"CcNameList"->takeName[as["CcList"]]]
	    ];
	    If[!KeyExistsQ[as,"ReturnPath"],
	       AssociateTo[as,"ReturnPath"->checkHeaders[as,"Return-Path"]]
	    ];
        If[!KeyExistsQ[as,"ReplyTo"],
           AssociateTo[as,"ReplyTo"->checkHeaders[as,"Reply-To"]]
        ];
	    If[!KeyExistsQ[as,"ReplyToAddress"],
	       AssociateTo[as,"ReplyToAddress"->takeAddress[checkHeaders[as,"Reply-To"]]]
	    ];
	    If[!KeyExistsQ[as,"ReplyToMessageID"],
	       AssociateTo[as,"ReplyToMessageID"->checkHeaders[as, "In-Reply-To"]]
	    ];
        If[!KeyExistsQ[as,"MessageID"],
        	If[KeyExistsQ[as,"MessageId"],
        		AssociateTo[as,"MessageID"->as["MessageId"]];
        		as=KeyDrop[as,"MessageId"]
        		,
                AssociateTo[as,"MessageID"->checkHeaders[as, "Message-ID"]]
        	]
        ];
        If[!KeyExistsQ[as,"ReferenceMessageIDList"],
           AssociateTo[as,"ReferenceMessageIDList"->{}]
        ];
        If[!KeyExistsQ[as,"ReturnReceiptRequested"],
           AssociateTo[as,"ReturnReceiptRequested"->StringQ[getReturnReceiptAddress[as]]]
        ];
	    If[!KeyExistsQ[as,"Precedence"],
	       AssociateTo[as,"Precedence"->Lookup[Lookup[as,"Headers",Association[]],"Precedence",Missing["NotAvailable"]]]
	    ];
	    If[!KeyExistsQ[as,"DeliveryChainHostnames"],
	       AssociateTo[as,"DeliveryChainHostnames"->takeHostnames[received]]
	    ];
	    If[!KeyExistsQ[as,"DeliveryChainRecords"],
	       AssociateTo[as,"DeliveryChainRecords"->checkHeaders[as, "Received"]]
	    ];
	    If[!KeyExistsQ[as,"HeaderString"],
	       AssociateTo[as,"HeaderString"->If[StringQ[rawmail],First[StringSplit[rawmail,"\n\n"]],Missing["NotAvailable"]]]
	    ];
	    If[!KeyExistsQ[as,"HeaderRules"],
	    	If[KeyExistsQ[as,"Headers"],
	           AssociateTo[as,"HeaderRules"->Normal[as["Headers"]]],
	           If[StringQ[as["HeaderString"]],
	           	AssociateTo[as,"HeaderRules"->headerrules[as["HeaderString"]]]
	           	,
	           	AssociateTo[as,"HeaderRules"->Missing["NotAvailable"]]	           	
	           ]
	    	]
	    ];
	    If[!KeyExistsQ[as,"CharacterEncoding"],
	       AssociateTo[as,"CharacterEncoding"->takecharset[checkHeaders[as, "ContentType"]]]
	    ];
        If[KeyExistsQ[as,"ContentType"],
            as=MapAt[takecontenttype,as,"ContentType"],
           AssociateTo[as,"ContentType"->takecontenttype[checkHeaders[as, "Content-Type"]]]
        ];
	    If[!KeyExistsQ[as,"OriginatingMailClient"],
	       AssociateTo[as,"OriginatingMailClient"->checkHeaders[as, {"User-Agent","x-mailer"}]]
	    ];
	    If[!KeyExistsQ[as,"OriginatingIPAddress"],
	       AssociateTo[as,"OriginatingIPAddress"->ip]
	    ];
	    If[!KeyExistsQ[as,"OriginatingHostname"],
	       AssociateTo[as,"OriginatingHostname"->Last[takeHostnames[received]]]
	    ];
	    If[!KeyExistsQ[as,"OriginatingLocation"],
	       AssociateTo[as,"OriginatingLocation"->findGeoLocation[ip]]
	    ];
	    If[!KeyExistsQ[as,"OriginatingCountry"],
	       AssociateTo[as,"OriginatingCountry"->geoCountry[ip]]
	    ];
	    If[!KeyExistsQ[as,"OriginatingDate"],
	       AssociateTo[as,"OriginatingDate"->Interpreter["DateTime"][date]]
	    ];
	    If[!KeyExistsQ[as,"OriginatingTimezone"],
	       AssociateTo[as,"OriginatingTimezone"->takeTimeZone[date]]
	    ];
	    If[!KeyExistsQ[as,"ServerOriginatingDate"],
	       AssociateTo[as,"ServerOriginatingDate"->takelast[received,"DateTime"]]
	    ];
	    If[!KeyExistsQ[as,"ServerOriginatingTimezone"],
	       AssociateTo[as,"ServerOriginatingTimezone"->takelast[received,"TimeZone"]]
	    ];
        AssociateTo[as,"ReceiverAddress"->receiveraddress];
	    If[!KeyExistsQ[as,"ReceiverRoutingType"],
	       AssociateTo[as,"ReceiverRoutingType"->getroutingtype[receiveraddress, as]]
	    ];
	    
	    If[!KeyExistsQ[as,"Association"],
	       AssociateTo[as,"Association"->sortMRFKeys[as]]
	    ];
    (* TODO:
    NewBodyContent,QuotedContent,ContentList,ContentAssociationList,
    ForwardedContent,ThreadFromList,ThreadFromAddressList
    *)
	];
    as
    
]

mrfParameterOrder={"From","FromAddress","Subject","Body","Attachments","AttachmentData",
	   "ReceiverAddress","ReceiverRoutingType","FromName","ToList","ToAddressList","ToNameList",
	   "CcList","CcAddressList","CcNameList","ReturnPath","ReplyTo","ReplyToAddress","DoNotReply",
	   "Answered","NewBodyContent","QuotedContent","ContentList","ContentAssociationList",
	   "ForwardedContent","ThreadFromList","ThreadFromAddressList","Attachments","AttachementNames",
	   "AttachmentAssociations","MessageID","ReplyToMessageID","ReferenceMessageIDList","Precedence",
	   "ReturnReceiptRequested","DeliveryChainHostnames","DeliveryChainRecords","HeaderString",
	   "HeaderRules","CharacterEncoding","ContentType","OriginatingMailClient","OriginatingIPAddress",
	   "OriginatingHostname","OriginatingCountry","OriginatingDate","OriginatingTimezone",
	   "ServerOriginatingDate","ServerOriginatingTimezone","Document","Association"};

sortMRFKeys[as_]:=KeySortBy[as, (First[Flatten[Position[mrfParameterOrder, #] /. {} -> {Infinity}]] &)]

(* typesetting *)
setMRFTypesetting[co_, email_]:=(
    Unprotect[CloudObject];
    With[{c = co,link=ToBoxes[Hyperlink[email,"mailto:"<>email]] }, c /: MakeBoxes[c, form : StandardForm | TraditionalForm] := 
        InterpretationBox[RowBox[{"CloudObject", "[", "\"mailto:\"",
            link
            , "]"}], co]];
    Protect[CloudObject]
)

(* parameter utilities *)
takeAddress[addresses:(_List|_String)]:=Interpreter["EmailAddress"][addresses]
takeAddress[expr_]:=expr

takeName[str_String]:=First[takeName[{str}]]
takeName[addresses_List]:=With[{split=StringSplit[addresses,"<"]},
	If[Length[#]>1,StringTrim[First[#]],Missing["NotAvailable"]]&/@split]
takeName[expr_]:=expr

takeHostnames[str_]:=takeHostnames[StringSplit[str,"Received"]]
takeHostnames[l_List]:=DeleteDuplicates[takeHostnames1/@Flatten[StringSplit[l,"\n"]]]
takeHostnames[expr_]:=expr

takeHostnames1[line_]:=StringTrim[StringReplace[line,{"from"->"","by"->"",("("~~___~~EndOfString)->""}]]

takecharset[str_String]:=If[StringFreeQ[str,"charset"],Missing["NotAvailable"],
	StringTrim[StringTrim[First[StringCases[str,"charset"~~x:(__)~~(Whitespace|EndOfString):>x]]],"="]
]
takecharset[expr_]:=expr

takecontenttype[str_String]:=First[StringSplit[str,";"]]
takecontenttype[expr_]:=expr

takelast[str_, type_]:=takelast[StringSplit[str,"from"], type]
takelast[l_List, type_]:=takelast1[Last[l], type]
takelast[expr_, _]:=expr

takelast1[received_, type_]:=With[{res=takelast2[Switch[type,"IPAddress",First,_,Last][StringSplit[received,{"\n", "by", "for"}]], type]},
	If[Length[res]>0,First[res],Missing["NotAvailable"]]
]
takelast2[line_, "IPAddress"]:=Interpreter["IPAddress"][StringCases[line,(ip : ((HexadecimalCharacter | "." | ":") ...) /; (stringLength[ip] > 5)):>ip]]
takelast2[line_, "DateTime"]:=(Interpreter["DateTime"][StringCases[line,";"~~date:(__)~~("("|EndOfString):>date]])/._Failure->Missing["NotAvailable"]
takelast2[line_, "TimeZone"]:=(Interpreter["TimeZone"][StringCases[line,tz : (("-" | "+") ~~ (DigitCharacter ...)):>tz]])/._Failure->Missing["NotAvailable"]


takelastTimestamp[str_]:=takelastTimestamp[StringSplit[str,"from"]]
takelastTimestamp[l_List]:=takelastTimestamp1[Last[l]]
takelastTimestamp[expr_]:=expr

takelastTimestamp1[received_]:=With[{res=takelastTimestamp2[First[StringSplit[received,"\n"]]]},
    If[Length[res]>0,First[res],Missing["NotAvailable"]]
]
takelastTimestamp2[line_]:=Interpreter["DateTime"][StringCases[line,";"~~date:(__)~~("("|EndOfString):>date]]

takeTimeZone[do_DateObject]:=(TimeZone /. Options[do])/.TimeZone->Missing["NotAvailable"]
takeTimeZone[str_String]:=takelast2[str, "TimeZone"]
takeTimeZone[_]:=Missing["NotAvailable"]

takeattachments[l:{_Rule...}]:=Last/@l
takeattachments[l_List]:=l
takeattachments[as_Association]:=Values[as]
takeattachments[l:{_Association...}]:=Flatten[Lookup[l,"Content",{}]]
takeattachments[expr_]:=expr
takeattachments[expr_]:={}

takenames[l:{_Rule...}]:=First/@l
takenames[as_Association]:=Keys[as]
takenames[l:{_Association...}]:=Flatten[Lookup[l,"Name",{}]]
takenames[___]:={}

findGeoLocation[str_String]:=FindGeoLocation[str]
findGeoLocation[expr_]:=Missing["NotAvailable"]

geoCountry[HoldPattern[pos_GeoPosition]]:=GeoNearest[Entity["Country"],pos]
geoCountry[_]:=Missing["NotAvailable"]

getAttachments[attachmentdata_]:=Missing["NotAvailable"] (* TODO *)
getroutingtype[receiveraddress_String, as_]:=Which[
	!(And@@StringFreeQ[Lookup[as,"ToList",{}],receiveraddress]),"To",
	!(And@@StringFreeQ[Lookup[as,"CcList",{}],receiveraddress]),"Cc",
	True,"Bcc"
]

getroutingtype[_, _]:=Missing["NotAvailable"]

checkHeaders[as_,headers_List]:=Catch[(If[KeyExistsQ[as,#],Throw[as[#],"checkheaders"]]&/@headers;
    If[KeyExistsQ[as,"Headers"],checkHeaders[as["Headers"],headers],Missing["NotAvailable"]]),"checkheaders"]
checkHeaders[as_,header_]:=as[header]/;KeyExistsQ[as,header]
checkHeaders[as_,header_]:=Lookup[as["Headers"],header]/;KeyExistsQ[as,"Headers"]
checkHeaders[__]:=Missing["NotAvailable"]

headerrules[headerstr_]:=With[{rules=Quiet[Rule @@@ StringTrim/@StringSplit[
	StringJoin /@  Partition[Rest[StringSplit[headerstr, 
        "\n" ~~ (x : Except[WhitespaceCharacter]) :> x]], 2], ":", 2]]},
    If[MatchQ[rules,{_Rule...}],
    	rules,
    	Missing["NotAvailable"]
    ]
]
  
sendmailmrf[]:=If[(Length[DownValues[CloudSystem`SendMail`Private`sendMailMRF]]>0)&&$CloudEvaluation,
	CloudSystem`SendMail`Private`sendMailMRF,
	SendMail	
]

toemaillist[str_String]:=StringTrim[StringSplit[str,","]]
toemaillist[_]:={}

stringLength[str_String]:=StringLength[str]
stringLength[_]:=0

End[]

EndPackage[]
