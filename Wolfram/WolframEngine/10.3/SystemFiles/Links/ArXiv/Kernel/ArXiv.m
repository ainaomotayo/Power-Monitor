Begin["ArXivAPI`"] (* Begin Private Context *) 

Begin["`Private`"](* Begin Private Context *) 

(******************************* ArXiv *************************************)

(* Authentication information *)

arxivdata[] = {
        "ServiceName"         -> "ArXiv", 
        "URLFetchFun"        :> (With[{params=Lookup[{##2},"Parameters",{}]},
        							(URLFetch[#1,Sequence@@FilterRules[{##2},Except["Parameters"|"Headers"]],
        							"Parameters" -> params,
        							"Headers" -> {}])
        						]&),
        						                               
       	 "ClientInfo"        :> {},
                                               
         "Gets"                -> {"Query","TitleSearch","AuthorSearch","AbstractSearch","CommentSearch","JournalReferenceSearch","SubjectCategorySearch","ReportNumberSearch","Categories"},
         "Posts"                -> {},
         "RawGets"            -> {"RawQuery"},
         "RawPosts"            -> {},
         "Information"        -> "Wolfram Language connection to the arXiv.org API"
}
(*Utility*)
arxivimport[rawdata_]:=ImportString[rawdata,"XML"]

AXImport[raw_String, option_String] := Block[{res = ImportString[ToString[raw, CharacterEncoding -> "UTF-8"], option],error, msg},
	 If[res === $Failed, Throw[$Failed]];
	 error = Cases[res, XMLElement["title", _, {"Error"}] :> "Error", Infinity];
	 msg = Flatten[Cases[res, XMLElement["summary", _, x_] :> x, Infinity], Infinity];
	 If[Length[error] > 0,
	 	Message[ServiceExecute::serrormsg, First[msg]];
	 	Throw[$Failed]
	 ];
  res]
  
XMLToRules[xml_]:=Module[{temp,entry},
	temp=Replace[Cases[xml, XMLElement["feed", _, _], Infinity, 1], {XMLElement["link", b_, _] :> "link" -> b,XMLElement[{_,"primary_category"}, b_, _] :> "primary_category" -> Cases[b,HoldPattern["term" -> _]],XMLElement["category", b_, _] :> "category" -> Cases[b,HoldPattern["term" -> _]],XMLElement[a_String, _, b_] :> a -> b, XMLElement[a_List, _, b_] :> a[[2]] -> b}, Infinity];
	entry=("entry" /. Normal[Merge["feed" /. temp, Identity]]);
	If[entry=="entry",entry={}];
  	temp=Map[Normal[Merge[#, Identity]] &, entry] /. {("author" -> a_) :> ("author" -> Normal[Merge[a, Identity]])} //. {{a_String} :> a, {} -> Missing["NotAvailable"]};
  	temp=Replace[Replace[temp, (Null | "") :> Missing["NotAvailable"], Infinity], {___Missing} :> Missing["NotAvailable"], Infinity];
  	If[MatchQ[temp,_Missing],temp={},temp]
]

camelCase[text_] := Module[{split, partial}, (
    split = StringSplit[text, {" ","_","-"}];
    partial = Prepend[Rest[Characters[#]], ToUpperCase[Characters[#][[1]]]] & /@ split;
    StringJoin[partial]
    )]
    
AXFieldsParse[e_] := e //. 
	{Verbatim[Alternatives][x_] :> x, Verbatim[Alternatives][x_, y__] :> "" ~~ x ~~ " OR " ~~ Alternatives[y] ~~ "",
   List[Verbatim[Except][y_]] :> "" ~~ " ANDNOT " ~~ y ~~ "",
   List[Verbatim[Except][x_], y__] :> "" ~~ " ANDNOT " ~~ x ~~ " AND " ~~ List[y] ~~ "",
   List[x_, Verbatim[Except][y_], z__] :> "" ~~ x ~~ " ANDNOT " ~~ y ~~ " AND " ~~ List[z] ~~ "",
   List[x_, Verbatim[Except][y_]] :> "" ~~ x ~~ " ANDNOT " ~~ y ~~ "",
   List[x_] :> x, List[x_, y__] :> "" ~~ x ~~ " AND " ~~ List[y] ~~ ""
   }

categoriesRules={"Statistics - Applications" -> "stat.AP", 
 "Statistics - Computation" -> "stat.CO", 
 "Statistics - Machine Learning" -> "stat.ML", 
 "Statistics - Methodology" -> "stat.ME", 
 "Statistics - Theory" -> "stat.TH", 
 "Quantitative Biology - Biomolecules" -> "q-bio.BM", 
 "Quantitative Biology - Cell Behavior" -> "q-bio.CB", 
 "Quantitative Biology - Genomics" -> "q-bio.GN", 
 "Quantitative Biology - Molecular Networks" -> "q-bio.MN", 
 "Quantitative Biology - Neurons and Cognition" -> "q-bio.NC", 
 "Quantitative Biology - Other" -> "q-bio.OT", 
 "Quantitative Biology - Populations and Evolution" -> "q-bio.PE", 
 "Quantitative Biology - Quantitative Methods" -> "q-bio.QM", 
 "Quantitative Biology - Subcellular Processes" -> "q-bio.SC", 
 "Quantitative Biology - Tissues and Organs" -> "q-bio.TO", 
 "Computer Science - Architecture" -> "cs.AR", 
 "Computer Science - Artificial Intelligence" -> "cs.AI", 
 "Computer Science - Computation and Language" -> "cs.CL", 
 "Computer Science - Computational Complexity" -> "cs.CC", 
 "Computer Science - Computational Engineering; Finance; and Science" \
-> "cs.CE", "Computer Science - Computational Geometry" -> "cs.CG", 
 "Computer Science - Computer Science and Game Theory" -> "cs.GT", 
 "Computer Science - Computer Vision and Pattern Recognition" -> 
  "cs.CV", "Computer Science - Computers and Society" -> "cs.CY", 
 "Computer Science - Cryptography and Security" -> "cs.CR", 
 "Computer Science - Data Structures and Algorithms" -> "cs.DS", 
 "Computer Science - Databases" -> "cs.DB", 
 "Computer Science - Digital Libraries" -> "cs.DL", 
 "Computer Science - Discrete Mathematics" -> "cs.DM", 
 "Computer Science - Distributed; Parallel; and Cluster Computing" -> 
  "cs.DC", "Computer Science - General Literature" -> "cs.GL", 
 "Computer Science - Graphics" -> "cs.GR", 
 "Computer Science - Human-Computer Interaction" -> "cs.HC", 
 "Computer Science - Information Retrieval" -> "cs.IR", 
 "Computer Science - Information Theory" -> "cs.IT", 
 "Computer Science - Learning" -> "cs.LG", 
 "Computer Science - Logic in Computer Science" -> "cs.LO", 
 "Computer Science - Mathematical Software" -> "cs.MS", 
 "Computer Science - Multiagent Systems" -> "cs.MA", 
 "Computer Science - Multimedia" -> "cs.MM", 
 "Computer Science - Networking and Internet Architecture" -> "cs.NI",
  "Computer Science - Neural and Evolutionary Computing" -> "cs.NE", 
 "Computer Science - Numerical Analysis" -> "cs.NA", 
 "Computer Science - Operating Systems" -> "cs.OS", 
 "Computer Science - Other" -> "cs.OH", 
 "Computer Science - Performance" -> "cs.PF", 
 "Computer Science - Programming Languages" -> "cs.PL", 
 "Computer Science - Robotics" -> "cs.RO", 
 "Computer Science - Software Engineering" -> "cs.SE", 
 "Computer Science - Sound" -> "cs.SD", 
 "Computer Science - Symbolic Computation" -> "cs.SC", 
 "Nonlinear Sciences - Adaptation and Self-Organizing Systems" -> 
  "nlin.AO", 
 "Nonlinear Sciences - Cellular Automata and Lattice Gases" -> 
  "nlin.CG", "Nonlinear Sciences - Chaotic Dynamics" -> "nlin.CD", 
 "Nonlinear Sciences - Exactly Solvable and Integrable Systems" -> 
  "nlin.SI", 
 "Nonlinear Sciences - Pattern Formation and Solitons" -> "nlin.PS", 
 "Mathematics - Algebraic Geometry" -> "math.AG", 
 "Mathematics - Algebraic Topology" -> "math.AT", 
 "Mathematics - Analysis of PDEs" -> "math.AP", 
 "Mathematics - Category Theory" -> "math.CT", 
 "Mathematics - Classical Analysis and ODEs" -> "math.CA", 
 "Mathematics - Combinatorics" -> "math.CO", 
 "Mathematics - Commutative Algebra" -> "math.AC", 
 "Mathematics - Complex Variables" -> "math.CV", 
 "Mathematics - Differential Geometry" -> "math.DG", 
 "Mathematics - Dynamical Systems" -> "math.DS", 
 "Mathematics - Functional Analysis" -> "math.FA", 
 "Mathematics - General Mathematics" -> "math.GM", 
 "Mathematics - General Topology" -> "math.GN", 
 "Mathematics - Geometric Topology" -> "math.GT", 
 "Mathematics - Group Theory" -> "math.GR", 
 "Mathematics - History and Overview" -> "math.HO", 
 "Mathematics - Information Theory" -> "math.IT", 
 "Mathematics - K-Theory and Homology" -> "math.KT", 
 "Mathematics - Logic" -> "math.LO", 
 "Mathematics - Mathematical Physics" -> "math.MP", 
 "Mathematics - Metric Geometry" -> "math.MG", 
 "Mathematics - Number Theory" -> "math.NT", 
 "Mathematics - Numerical Analysis" -> "math.NA", 
 "Mathematics - Operator Algebras" -> "math.OA", 
 "Mathematics - Optimization and Control" -> "math.OC", 
 "Mathematics - Probability" -> "math.PR", 
 "Mathematics - Quantum Algebra" -> "math.QA", 
 "Mathematics - Representation Theory" -> "math.RT", 
 "Mathematics - Rings and Algebras" -> "math.RA", 
 "Mathematics - Spectral Theory" -> "math.SP", 
 "Mathematics - Statistics" -> "math.ST", 
 "Mathematics - Symplectic Geometry" -> "math.SG", 
 "Astrophysics" -> "astro-ph", 
 "Physics - Disordered Systems and Neural Networks" -> 
  "cond-mat.dis-nn", 
 "Physics - Mesoscopic Systems and Quantum Hall Effect" -> 
  "cond-mat.mes-hall", 
 "Physics - Materials Science" -> "cond-mat.mtrl-sci", 
 "Physics - Other" -> "cond-mat.other", 
 "Physics - Soft Condensed Matter" -> "cond-mat.soft", 
 "Physics - Statistical Mechanics" -> "cond-mat.stat-mech", 
 "Physics - Strongly Correlated Electrons" -> "cond-mat.str-el", 
 "Physics - Superconductivity" -> "cond-mat.supr-con", 
 "General Relativity and Quantum Cosmology" -> "gr-qc", 
 "High Energy Physics - Experiment" -> "hep-ex", 
 "High Energy Physics - Lattice" -> "hep-lat", 
 "High Energy Physics - Phenomenology" -> "hep-ph", 
 "High Energy Physics - Theory" -> "hep-th", 
 "Mathematical Physics" -> "math-ph", 
 "Nuclear Experiment" -> "nucl-ex", "Nuclear Theory" -> "nucl-th", 
 "Physics - Accelerator Physics" -> "physics.acc-ph", 
 "Physics - Atmospheric and Oceanic Physics" -> "physics.ao-ph", 
 "Physics - Atomic Physics" -> "physics.atom-ph", 
 "Physics - Atomic and Molecular Clusters" -> "physics.atm-clus", 
 "Physics - Biological Physics" -> "physics.bio-ph", 
 "Physics - Chemical Physics" -> "physics.chem-ph", 
 "Physics - Classical Physics" -> "physics.class-ph", 
 "Physics - Computational Physics" -> "physics.comp-ph", 
 "Physics - Data Analysis; Statistics and Probability" -> 
  "physics.data-an", "Physics - Fluid Dynamics" -> "physics.flu-dyn", 
 "Physics - General Physics" -> "physics.gen-ph", 
 "Physics - Geophysics" -> "physics.geo-ph", 
 "Physics - History of Physics" -> "physics.hist-ph", 
 "Physics - Instrumentation and Detectors" -> "physics.ins-det", 
 "Physics - Medical Physics" -> "physics.med-ph", 
 "Physics - Optics" -> "physics.optics", 
 "Physics - Physics Education" -> "physics.ed-ph", 
 "Physics - Physics and Society" -> "physics.soc-ph", 
 "Physics - Plasma Physics" -> "physics.plasm-ph", 
 "Physics - Popular Physics" -> "physics.pop-ph", 
 "Physics - Space Physics" -> "physics.space-ph", 
 "Quantum Physics" -> "quant-ph"}    
(*Raw*)

arxivdata["RawQuery"]:={
        "URL"				-> "http://export.arxiv.org/api/query",
        "HTTPSMethod"		-> "GET",
        "Parameters"		-> {"search_query","id_list","start","max_results","sortBy","sortOrder"},
        "RequiredParameters"-> {},
        "ResultsFunction"	-> arxivimport (*(AXImport[#,"XML"]&)*)
        }

(*Cooked*)

arxivcookeddata["Query", id_,args_]:=Block[{params={},rawdata,invalidParameters,maxitems,withCamelTitles,error,msg,query},
	invalidParameters = Select[Keys[args],!MemberQ[{"SearchQuery","IDList","StartIndex","MaxItems","SortBy","SortOrder"},#]&]; 
	If[Length[invalidParameters]>0,
	(
		Message[ServiceObject::noget,#,"ArXiv"]&/@invalidParameters;
		Throw[$Failed]
	)];
	If[KeyExistsQ[args,"SearchQuery"],
	(
		query="SearchQuery"/.args;
		If[!(ListQ[query]||MatchQ[query,_Alternatives]||StringQ[query]),
		(	
			Message[ServiceExecute::nval,"SearchQuery","ArXiv"];
			Throw[$Failed]
		)];
		If[!StringQ[query],
			query = AXFieldsParse[query /.{
				y : Rule[All, x_] :> ("all:" <> StringReplace[x, {a : (__ ~~ " " ~~ __) :> ("\"" <> a <> "\"")}]),
				y : Rule["Title", x_] :> ("ti:" <> StringReplace[x, {a : (__ ~~ " " ~~ __) :> ("\"" <> a <> "\"")}]),
				y : Rule["Author", x_] :> ("au:" <> StringReplace[x, {a : (__ ~~ " " ~~ __) :> ("\"" <> a <> "\"")}]),
				y : Rule["Abstract", x_] :> ("abs:" <> StringReplace[x, {a : (__ ~~ " " ~~ __) :> ("\"" <> a <> "\"")}]),
				y : Rule["Comment", x_] :> ("co:" <> StringReplace[x, {a : (__ ~~ " " ~~ __) :> ("\"" <> a <> "\"")}]),
				y : Rule["JournalReference", x_] :> ("jr:" <> StringReplace[x, {a : (__ ~~ " " ~~ __) :> ("\"" <> a <> "\"")}]),
				y : Rule["SubjectCategory", x_] :> ("cat:" <> StringReplace[x, {a : (__ ~~ " " ~~ __) :> ("\"" <> a <> "\"")}]),
				y : Rule["ReportNumber", x_] :> ("rn:" <> StringReplace[x, {a : (__ ~~ " " ~~ __) :> ("\"" <> a <> "\"")}])
			}]
		];
		params = Append[params,Rule["search_query",query]]
	)];
	If[KeyExistsQ[args,"IDList"],
	(
		If[!(MatchQ["IDList" /. args, {__String}]||StringQ["IDList" /. args]),
		(	
			Message[ServiceExecute::nval,"IDList","ArXiv"];
			Throw[$Failed]
		)];		
		If[ListQ["IDList" /. args],
			params = Append[params,Rule["id_list",(StringJoin @@ Riffle["IDList" /. args, ","])]]
		];
		If[StringQ["IDList" /. args],	
			params = Append[params,Rule["id_list","IDList" /. args]]
		];
	)];
	If[KeyExistsQ[args,"MaxItems"],
	(
		If[!(IntegerQ["MaxItems"/.args]&&("MaxItems"/.args)>0&&("MaxItems"/.args)<=30000),
		(	
			Message[ServiceExecute::nval,"MaxItems","ArXiv"];
			Throw[$Failed]
		)];
		maxitems="MaxItems"/.args;
		params = Append[params,Rule["max_results",ToString[maxitems]]]
	),
  	(
  		maxitems=10;
  		params = Append[params,Rule["max_results",ToString[maxitems]]]
  	)];
	If[KeyExistsQ[args,"StartIndex"],
	(
		If[!(IntegerQ["StartIndex"/.args]&&("StartIndex"/.args)>0),
		(	
			Message[ServiceExecute::nval,"StartIndex","ArXiv"];
			Throw[$Failed]
		)];
		params = Append[params,Rule["start",ToString[(("StartIndex"/.args)-1)*maxitems]]]
	)];
	If[KeyExistsQ[args,"SortBy"],
	(
		If[!StringMatchQ[ToString["SortBy" /. args],  "Relevance" | "LastUpdatedDate" | "SubmittedDate"],
		(	
			Message[ServiceExecute::nval,"SortBy","ArXiv"];
			Throw[$Failed]
		)]; 
		params = Append[params,Rule["sortBy",ToString["SortBy"/.args]/.{"Relevance"->"relevance", "LastUpdatedDate"->"lastUpdatedDate", "SubmittedDate"->"submittedDate"}]]
	)];
	If[KeyExistsQ[args,"SortOrder"],
	(
		If[!StringMatchQ[ToString["SortOrder" /. args],  "Descending" | "Ascending" ],
		(	
			Message[ServiceExecute::nval,"SortOrder","ArXiv"];
			Throw[$Failed]
		)]; 
		params = Append[params,Rule["sortOrder",ToString["SortOrder"/.args]/.{"Descending"->"descending", "Ascending"->"ascending"}]]
	)];
	rawdata = ServiceExecute["ArXiv","RawQuery",params];
	error = Cases[rawdata, XMLElement["title", _, {"Error"}] :> "Error", Infinity];
	msg = Flatten[Cases[rawdata, XMLElement["summary", _, x_] :> x, Infinity], Infinity];
	If[Length[error] > 0,
 		Message[ServiceExecute::serrormsg, First[msg]];
	 	Throw[$Failed]
	];
	rawdata = XMLToRules[rawdata];
	withCamelTitles=Replace[rawdata, {Rule[a_, b_] :> Rule[camelCase[a], b],(Null|"") -> Missing["NotAvailable"]}, Infinity]
	/.{(y : ("Updated" | "Published") -> x_) :> (y -> If[MatchQ[x, _String], DateObject[x, TimeZone -> 0], x]),
		"Id"->"ID","Doi"->"DOI","JournalRef"->"JournalReference"};
	withCamelTitles=Replace[withCamelTitles, {Rule["ID", a_] :> Sequence[Rule["ID", StringReplace[a, {__ ~~ Shortest["/" ~~ b__ ~~ EndOfString] :> b}]], Rule["URL", a]]}, Infinity];
	Dataset[Replace[withCamelTitles, r : {__Rule} :> Association[r], -1]]
]

arxivcookeddata[req:"TitleSearch"|"AuthorSearch"|"AbstractSearch"|"CommentSearch"|"JournalReferenceSearch"|"SubjectCategorySearch"|"ReportNumberSearch", id_,args_]:=Block[{params={},rawdata,query,invalidParameters,maxitems,withCamelTitles,error,msg},
	invalidParameters = Select[Keys[args],!MemberQ[{"SearchQuery","StartIndex","MaxItems","SortBy","SortOrder"},#]&]; 
	If[Length[invalidParameters]>0,
	(
		Message[ServiceObject::noget,#,"ArXiv"]&/@invalidParameters;
		Throw[$Failed]
	)];
	If[KeyExistsQ[args,"MaxItems"],
	(
		If[!(IntegerQ["MaxItems"/.args]&&("MaxItems"/.args)>0&&("MaxItems"/.args)<=30000),
		(	
			Message[ServiceExecute::nval,"MaxItems","ArXiv"];
			Throw[$Failed]
		)];
		maxitems="MaxItems"/.args;
		params = Append[params,Rule["max_results",ToString[maxitems]]]
	),
  	(
  		maxitems=10;
  		params = Append[params,Rule["max_results",ToString[maxitems]]]
  	)];
	If[KeyExistsQ[args,"StartIndex"],
	(
		If[!(IntegerQ["StartIndex"/.args]&&("StartIndex"/.args)>0),
		(	
			Message[ServiceExecute::nval,"StartIndex","ArXiv"];
			Throw[$Failed]
		)];
		params = Append[params,Rule["start",ToString[(("StartIndex"/.args)-1)*maxitems]]]
	)];
	If[KeyExistsQ[args,"SortBy"],
	(
		If[!StringMatchQ[ToString["SortBy" /. args],  "Relevance" | "LastUpdatedDate" | "SubmittedDate"],
		(	
			Message[ServiceExecute::nval,"SortBy","ArXiv"];
			Throw[$Failed]
		)]; 
		params = Append[params,Rule["sortBy",ToString["SortBy"/.args]/.{"Relevance"->"relevance", "LastUpdatedDate"->"lastUpdatedDate", "SubmittedDate"->"submittedDate"}]]
	)];
	If[KeyExistsQ[args,"SortOrder"],
	(
		If[!StringMatchQ[ToString["SortOrder" /. args],  "Descending" | "Ascending" ],
		(	
			Message[ServiceExecute::nval,"SortOrder","ArXiv"];
			Throw[$Failed]
		)]; 
		params = Append[params,Rule["sortOrder",ToString["SortOrder"/.args]/.{"Descending"->"descending", "Ascending"->"ascending"}]]
	)];
	
	If[KeyExistsQ[args,"SearchQuery"],
	(
		query="SearchQuery"/.args;
		If[!(ListQ[query]||MatchQ[query,_Alternatives]||StringQ[query]),
		(	
			Message[ServiceExecute::nval,"SearchQuery","ArXiv"];
			Throw[$Failed]
		)];
		Switch[req,
			"TitleSearch",query = AXFieldsParse[query /. {x_String :> ("ti:" <> StringReplace[x, {a : (__ ~~ " " ~~ __) :> ("\"" <> a <> "\"")}])}],
			"AuthorSearch",query = AXFieldsParse[query /. {x_String :> ("au:" <> StringReplace[x, {a : (__ ~~ " " ~~ __) :> ("\"" <> a <> "\"")}])}],
			"AbstractSearch",query = AXFieldsParse[query /. {x_String :> ("abs:" <> StringReplace[x, {a : (__ ~~ " " ~~ __) :> ("\"" <> a <> "\"")}])}],
			"CommentSearch",query = AXFieldsParse[query /. {x_String :> ("co:" <> StringReplace[x, {a : (__ ~~ " " ~~ __) :> ("\"" <> a <> "\"")}])}],
			"JournalReferenceSearch",query = AXFieldsParse[query /. {x_String :> ("jr:" <> StringReplace[x, {a : (__ ~~ " " ~~ __) :> ("\"" <> a <> "\"")}])}],
			"SubjectCategorySearch",query = AXFieldsParse[query /.categoriesRules/. {x_String :> ("cat:" <> StringReplace[x, {a : (__ ~~ " " ~~ __) :> ("\"" <> a <> "\"")}])}],
			"ReportNumberSearch",query = AXFieldsParse[query /. {x_String :> ("rn:" <> StringReplace[x, {a : (__ ~~ " " ~~ __) :> ("\"" <> a <> "\"")}])}]
		];
		params = Append[params,Rule["search_query",query]]
	)];
	
	rawdata = ServiceExecute["ArXiv","RawQuery",params];
	error = Cases[rawdata, XMLElement["title", _, {"Error"}] :> "Error", Infinity];
	msg = Flatten[Cases[rawdata, XMLElement["summary", _, x_] :> x, Infinity], Infinity];
	If[Length[error] > 0,
 		Message[ServiceExecute::serrormsg, First[msg]];
	 	Throw[$Failed]
	];
	rawdata = XMLToRules[rawdata];
	withCamelTitles=Replace[rawdata, {Rule[a_, b_] :> Rule[camelCase[a], b],(Null|"") -> Missing["NotAvailable"]}, Infinity]
	/.{(y : ("Updated" | "Published") -> x_) :> (y -> If[MatchQ[x, _String], DateObject[x, TimeZone -> 0], x]),
		"Id"->"ID","Doi"->"DOI","JournalRef"->"JournalReference"};
	withCamelTitles=Replace[withCamelTitles, {Rule["ID", a_] :> Sequence[Rule["ID", StringReplace[a, {__ ~~ Shortest["/" ~~ b__ ~~ EndOfString] :> b}]], Rule["URL", a]]}, Infinity];
	Dataset[Replace[withCamelTitles, r : {__Rule} :> Association[r], -1]]
]

arxivcookeddata["Categories", id_,args_]:=Block[{query,rawdata,invalidParameters},
	invalidParameters = Select[Keys[args],!MemberQ[{"SearchQuery"},#]&]; 
	If[Length[invalidParameters]>0,
	(
		Message[ServiceObject::noget,#,"ArXiv"]&/@invalidParameters;
		Throw[$Failed]
	)];
	If[KeyExistsQ[args,"SearchQuery"],
	(
		query = ToString["SearchQuery"/.args]
	),
	(
		query = "All"
	)];
	
	If[query == "All",
	(
		categoriesRules[[All,1]]
	),
	(
		Select[categoriesRules[[All,1]], StringContainsQ[#, query, IgnoreCase -> True] &]
	)]	
]



arxivcookeddata[req_, id_]:=arxivcookeddata[req, id,{}]

arxivcookeddata[prop_,id_,rules___Rule]:=arxivcookeddata[prop,id,{rules}]

arxivcookeddata[___]:=$Failed

arxivrawdata[___]:=$Failed

arxivsendmessage[args_]:=$Failed

End[]

End[]

SetAttributes[{},{ReadProtected, Protected}];

System`Private`RestoreContextPath[];

(* Return two functions to define oauthservicedata, oauthcookeddata  *)

{ArXivAPI`Private`arxivdata,ArXivAPI`Private`arxivcookeddata,ArXivAPI`Private`arxivsendmessage,ArXivAPI`Private`arxivrawdata}
