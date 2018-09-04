System`Private`NewContextPath[{"OAuthClient`","System`"}];
Begin["WaybackMachine`"] (* Begin Private Context *) 
Begin["`Private`"](* Begin Private Context *) 
(******************************* WaybackMachine *************************************)
waybackmachinedata[]={
		"Gets"				-> {"Available"(*,"CDX"*),"Import"},
		"RawGets"			-> {"RawAvailable"(*,"RawCDX"*),"RawImport"},
		"Posts"				-> {},
		"RawPosts"			-> {},
		"ServiceName"		-> "WaybackMachine",
 		"Information"		-> "Wolfram Language connection to the internet archive machine API"
}
(****** Auxiliary Functions ******)
waybackmachinegettimestamp[$Failed]:=Throw[$Failed]
waybackmachinegettimestamp[date_String]:=Block[{$DateStringFormat = {"Year", "Month", "Day"}}, 
			DateString@DateList@Interpreter["Date"][date]
];
waybackmachineformatassociation[rawdata_]/;If[KeyExistsQ[("archived_snapshots"/.rawdata),"closest"],True,Message[waybackmachineformatassociation::nnarg, url];False]:=Association@("closest"/.("archived_snapshots"/.rawdata))
waybackmachineformatassociation::nnarg = 
  "The given url : `1` is not archived on the wayback machine.";
(****** Raw Properties ******)
waybackmachinerawdata["RawAvailable",args_]:=Block[{url,timestamp, res},
	url=ToString[Lookup[args,"url",Lookup[args,"URL",0]]];
	timestamp=waybackmachinegettimestamp[DateString[Lookup[args,"timestamp",Lookup[args,"TIMESTAMP",0]]]];
	res=URLFetch["http://archive.org/wayback/available","Parameters"->{"url"->url,"timestamp"->timestamp},"Method" -> "GET"];
	waybackmachineformatassociation[ImportString[res,"JSON"]]
  (*ImportString[res,"JSON"]*)
]

waybackmachinerawdata["RawImport",args_]:=Block[{call,url, elements,imagelinks},
	call=waybackmachinerawdata["RawAvailable", args];
	elements=ToString[Lookup[args,"elements",Lookup[args,"Elements",0]]]/.{"All"->"Elements"};
  If[!MatchQ[call,$Failed],
  (If[elements=="Images",
    (
      imagelinks=Import[call["url"],"ImageLinks"][[6;;-1]];
      Import/@imagelinks
    ),
      Import[call["url"],elements]
    ]
  ),
  $Failed]
]
(*waybackmachinerawdata["RawCDX",args_]:=Block[{url, res},
	url=ToString[Lookup[args,"url",Lookup[args,"URL",0]]];
	res=URLFetch["http://web.archive.org/cdx/search/cdx","Parameters"->{"url"->url},"Method" -> "GET"];
	res
]*)

waybackmachinerawdata[___]:=$Failed

(****** Cooked Properties ******)
waybackmachinecookeddata["Available",args_]:=With[{res=waybackmachinerawdata["RawAvailable", args]},
	If[KeyExistsQ[res,"url"],
		Hyperlink[res["url"],res["url"]],
		$Failed
	]
]
waybackmachinecookeddata["Import",args_]:=With[{res=waybackmachinerawdata["RawImport", args]},
	res	
]
(*waybackmachinecookeddata["CDX",args_]:=With[{res=waybackmachinerawdata["RawCDX",args]},
	res
]*)
waybackmachinecookeddata[___]:=$Failed

(****** Send Message ******)
waybackmachinesendmessage[args_]:=waybackmachinecookeddata["PostData",args]
End[] (* End Private Context *)       		
End[]
System`Private`RestoreContextPath[];
{WaybackMachine`Private`waybackmachinedata,WaybackMachine`Private`waybackmachinecookeddata,WaybackMachine`Private`waybackmachinesendmessage,WaybackMachine`Private`waybackmachinerawdata}
