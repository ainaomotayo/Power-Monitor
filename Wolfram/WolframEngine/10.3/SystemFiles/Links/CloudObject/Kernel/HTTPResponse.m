BeginPackage["CloudObject`"]

System`HTTPResponse
System`HTTPErrorResponse

Begin["`Private`"]

(*  Developer riccardod, carlob.

    official xml is took from wikipedia and linked here
    http://www.iana.org/assignments/http-status-codes/http-status-codes.xml
    to update them run this code:

    $StatusCodes = Association @ Cases[
        Import["http://www.iana.org/assignments/http-status-codes/http-status-codes.xml", "XML"], 
        {___, XMLElement["value", _, {val_}], ___, XMLElement["description", _, {desc:Except["Unassigned"]}], ___} 
            :> {FromDigits[val] -> desc}, 
        Infinity
    ] *)

toStatusCodeDescription := toStatusCodeDescription = Replace @ Dispatch[{
    100 -> "Continue",
    101 -> "Switching Protocols",
    102 -> "Processing",
    200 -> "OK",
    201 -> "Created",
    202 -> "Accepted",
    203 -> "Non-Authoritative Information",
    204 -> "No Content",
    205 -> "Reset Content",
    206 -> "Partial Content",
    207 -> "Multi-Status",
    208 -> "Already Reported",
    226 -> "IM Used",
    300 -> "Multiple Choices",
    301 -> "Moved Permanently",
    302 -> "Found",
    303 -> "See Other",
    304 -> "Not Modified",
    305 -> "Use Proxy",
    307 -> "Temporary Redirect",
    308 -> "Permanent Redirect",
    400 -> "Bad Request",
    401 -> "Unauthorized",
    402 -> "Payment Required",
    403 -> "Forbidden",
    404 -> "Not Found",
    405 -> "Method Not Allowed",
    406 -> "Not Acceptable",
    407 -> "Proxy Authentication Required",
    408 -> "Request Timeout",
    409 -> "Conflict",
    410 -> "Gone",
    411 -> "Length Required",
    412 -> "Precondition Failed",
    413 -> "Payload Too Large",
    414 -> "URI Too Long",
    415 -> "Unsupported Media Type",
    416 -> "Range Not Satisfiable",
    417 -> "Expectation Failed",
    421 -> "Misdirected Request",
    422 -> "Unprocessable Entity",
    423 -> "Locked",
    424 -> "Failed Dependency",
    426 -> "Upgrade Required",
    428 -> "Precondition Required",
    429 -> "Too Many Requests",
    431 -> "Request Header Fields Too Large",
    500 -> "Internal Server Error",
    501 -> "Not Implemented",
    502 -> "Bad Gateway",
    503 -> "Service Unavailable",
    504 -> "Gateway Timeout",
    505 -> "HTTP Version Not Supported",
    506 -> "Variant Also Negotiates",
    507 -> "Insufficient Storage",
    508 -> "Loop Detected",
    510 -> "Not Extended",
    511 -> "Network Authentication Required",
    _   -> Missing["Undefined"]
}];

toStatusCode := toStatusCode = Replace[{
    status_Integer /; 100 <= status < 600 :> status,
    "PageNotFound"|"NotFound" -> 404,
    "PermissionDenied"|"Forbidden" -> 403,
    "ServerError"|"InternalServerError" -> 500,
    s_?StringQ /; StringMatchQ[s, DigitCharacter..] :> FromDigits[s],
    status_ :> (Message[HTTPResponse::nvldstatus, status];500)
}]

$bodyPattern = _?StringQ|None|_List


HTTPResponse::nvldstatus = "Invalid status code ``. Using 500."
HTTPResponse::nvldheaders = "Invalid headers specification ``. Using {}."

HTTPResponse[body_:None] := HTTPResponse[body, <||>]

HTTPResponse[body_, meta:_Rule|_RuleDelayed|{RepeatedNull[_Rule|_RuleDelayed]}] :=
    HTTPResponse[body, <|meta|>]

HTTPResponse[body_, None|_Missing] := HTTPResponse[body, <||>]

HTTPResponse[body:$bodyPattern, meta_?AssociationQ]["Properties"] := {
    "StatusCode",
    "StatusCodeDescription",
    "ContentType",
    "Headers",
    "Body",
    "BodyBytes",
    "BodyByteArray",
    "CharacterEncoding"
};

(hr:HTTPResponse[body:$bodyPattern, meta_?AssociationQ])["Meta"] := AssociationMap[
    hr,
    {
        "StatusCode",
        "ContentType",
        "Headers"
    }
]

HTTPResponse[body:$bodyPattern, meta_?AssociationQ]["StatusCode"]  := 
    toStatusCode @ Lookup[meta, "StatusCode", 200]

HTTPResponse[body:$bodyPattern, meta_?AssociationQ]["StatusCodeDescription"]  := 
    toStatusCodeDescription[
        HTTPResponse[body, meta]["StatusCode"]
    ];

HTTPResponse[body:$bodyPattern, meta_?AssociationQ]["ContentType"|"Content-Type"] := 
    Lookup[
        meta, 
        "ContentType", 
        FirstCase[
            HTTPResponse[body, meta]["Headers"], 
            _["Content-Type", r_] :> r, 
            "text/html"
        ]
    ];

HTTPResponse[body:$bodyPattern, meta_?AssociationQ][prop:"Headers"] := 
    Rule @@@ Replace[
        Lookup[meta, prop, {}], {
            {s_?StringQ, f_?StringQ} :> {s -> f},
            h:(Rule|RuleDelayed)[___] :> {h},
            h:{___List} :> h,
            h_?AssociationQ :> Normal[h],
            h:{RepeatedNull[_Rule|_RuleDelayed]} :> h,
            h_ :> (Message[HTTPResponse::nvldheaders, h];{})
        }
    ];

HTTPResponse[body:$bodyPattern, meta_?AssociationQ]["Body"] := 
    Replace[
        body, {
            None -> "",
            (* content_List :> FromCharacterCode[content, HTTPResponse[body, meta]["CharacterEncoding"]], *)
            content_List :> FromCharacterCode[content],
            content_?StringQ :> content
        }
    ];

HTTPResponse[body:$bodyPattern, meta_?AssociationQ]["BodyBytes"] := 
    Developer`ToPackedArray @ Replace[
        body, {
            None -> {},
            content_List :> content,
            (*content_?StringQ :> ToCharacterCode[content, HTTPResponse[body, meta]["CharacterEncoding"]]*)
            content_?StringQ :> ToCharacterCode[content]
        }
    ];

HTTPResponse[body:$bodyPattern, meta_?AssociationQ]["BodyLength"] := Switch[body,
    _?StringQ, StringLength[body],
    _List, Length[body],
    None, 0
]

HTTPResponse[body:$bodyPattern, meta_?AssociationQ]["BodyByteArray"] := ByteArray[HTTPResponse[body, meta]["BodyBytes"]]
    
HTTPResponse[body:$bodyPattern, meta_?AssociationQ]["CharacterEncoding"] := 
    First @ PadRight[
        StringCases[
            Lookup[meta, "ContentType", "text/html"], 
            "charset=" ~~ enc__ ~~ Repeated[";", {0, 1}] :> enc, 
            1,
            IgnoreCase -> True
        ],
        1,
        None
    ];

HTTPResponse[body:$bodyPattern, meta_?AssociationQ]["BinaryFormatQ"] := 
    ! MemberQ[{
            "c", "csv", "dif", "dot", "eps", "expressionml", "fasta", "fastq", 
            "graph6", "graphlet", "graphml", "gxl", "harwellboeing", "html", 
            "jvx", "kml", "leda", "mathml", "maya", "mol", "mol2", "mtx", "nb", 
            "nexus", "obj", "package", "pajek", "pdb", "pov", "rtf", "sdf", 
            "sparse6", "string", "svg", "table", "tex", "text", "tgf", "tsv", 
            "uue", "vrml", "wl", "x3d", "xbm", "xhtml", "xhtmlmathml", "xml", 
            "xyz", "rawhtml", "json"
        },
        ToLowerCase[mimetypeToFormat[HTTPResponse[body, meta]["ContentType"]]]
    ]

HTTPResponse[body:$bodyPattern, meta_?AssociationQ][] :=
    HTTPResponse[body, meta][All]

HTTPResponse[body:$bodyPattern, meta_?AssociationQ][All|"PropertyAssociation"] := 
    AssociationMap[
        HTTPResponse[body, meta],
        HTTPResponse[body, meta]["Properties"]
    ];

HTTPResponse[body:$bodyPattern, meta_?AssociationQ][args_List] :=
    Map[HTTPResponse[body, meta], args];

HTTPResponse[body:$bodyPattern, meta_?AssociationQ][prop_] /; KeyExistQ[meta, prop] :=
    meta[prop] 

HTTPResponse /: MakeBoxes[response:HTTPResponse[body:_?StringQ|None|_List, meta_?AssociationQ], StandardForm] := With[
    {status = response["StatusCode"]},
    With[{
    graphics = Graphics[{
                Which[
                    200 <= status < 400 , Darker[Green],
                    400 <= status < 500, Orange,
                    500 <= status < 600, Red,
                    True, Black
                ],
                Disk[], 
                Text[
                    Style[status, Directive[11, White, Bold]], 
                    Scaled[{.53, .48}]
                ]}, 
                ImageSize -> Dynamic[{
                    Automatic, 
                    (3 * CurrentValue["FontCapHeight"]) / AbsoluteCurrentValue[Magnification]
                }],
                Background -> None
            ]},
    BoxForm`ArrangeSummaryBox[
        HTTPResponse,
        response,
        If[
            TrueQ[!$CloudEvaluation] && response["ContentType"] === "text/html",
            Button[
                Mouseover[graphics, graphics /. c_?ColorQ :> Darker[c]],
	            SystemOpen @ Export[
	                FileNameJoin[{$TemporaryDirectory, "HTTPResponse" <> IntegerString[Hash @ response, 36, 13] <> ".html"}],
	                response["Body"],
	                "String"
	            ],
	            Appearance -> "Frameless"
            ],
            Button[graphics, Null, Appearance -> "Frameless", Active -> False]
        ],
        {
            BoxForm`MakeSummaryItem[{"Status: ", toStatusCodeDescription[status]}, StandardForm],
            BoxForm`MakeSummaryItem[{"Content type: ", response["ContentType"]}, StandardForm]
        },
        {
            BoxForm`MakeSummaryItem[{"Body: ", Button[
                Mouseover[
                    Style[Short[response["Body"]], 10, "StandardForm"],
                    Style[Short[response["Body"]], 10, "StandardForm", "HyperlinkActive"]
                ],
                Print[response["Body"]],
                Appearance -> None
            ]}, StandardForm],
            BoxForm`MakeSummaryItem[{"Headers: ", Length[response["Headers"]]}, StandardForm],
            If[
                Length[response["Headers"]] > 0,
                Grid[
                    Function[{#1, Short[#2]}] @@@ response["Headers"],
                    Alignment -> {{Right, Left}},
                    Dividers -> {{False}, {GrayLevel[0, 0.2]}}
                ],
                Sequence @@ {}
            ]
        },
        StandardForm
    ]
]]


HTTPErrorResponse[] := HTTPErrorResponse[500]
HTTPErrorResponse[status_?StringQ] := HTTPErrorResponse[toStatusCode[status]]
HTTPErrorResponse[status_Integer, ___]["StatusCodeDescription"] := 
    toStatusCodeDescription[
        status
    ];

End[]

EndPackage[]