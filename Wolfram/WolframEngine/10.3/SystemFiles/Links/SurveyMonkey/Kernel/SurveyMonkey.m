Begin["SurveyMonkeyOAuth`"] (* Begin Private Context *) 

Begin["`Private`"](* Begin Private Context *) 

(******************************* SurveyMonkey *************************************)

(* Authentication information *)

surveymonkeydata[]={
		"OAuthVersion"		-> "2.0",
		"ServiceName" 		-> "SurveyMonkey", 
	 	"AuthorizeEndpoint" -> "https://api.surveymonkey.net/oauth/authorize?api_key=2mbu86kpt2dvn8udvh8jc44g", 
     	"AccessEndpoint"    -> "https://api.surveymonkey.net/oauth/token?api_key=2mbu86kpt2dvn8udvh8jc44g",
     	"RedirectURI"       -> "https://www.wolfram.com/oauthlanding?service=SurveyMonkey",
	 	"ClientInfo"		-> {"pieros","JKDCuDVvgAuqcbUhjZTask9Hcg5gb3Yx"},
	 	"AuthenticationDialog" :> (OAuthClient`tokenOAuthDialog[StringReplace[#, "?client_id" :> "&client_id"], "SurveyMonkey"]&),
	 	"RequestFormat" 	-> (Block[{APIkey="2mbu86kpt2dvn8udvh8jc44g",params=Cases[{##},("Parameters"->x_):>x,Infinity],url=DeleteCases[{##},"Parameters"->_,Infinity]},
	 		URLFetch@@{(Sequence@@(url[[1]]<>"?api_key="<>APIkey)),Sequence@@Rest[url],
	 			"Headers"->Flatten[{"Authorization"-> ("Bearer " <> ("access_token"/.params)),"Content-Type" -> "application/json"}]}
	 	]&), (*Does not support more Headers or Parameters*)
	 	"Gets"				:> {},
	 	"Posts"				-> {"SurveyList","SurveyDetails","CollectorList","ResponseCounts","RespondentList","Responses","UserDetails","TemplateList","CreateWeblinkCollector","CreateEmailCollector","CreateSurvey","SurveyResults"},
	 	"Scope"				-> {},
	 	"RawGets"			-> {},
	 	"RawPosts"			-> {"RawSurveyList","RawSurveyDetails","RawCollectorList","RawResponseCounts","RawRespondentList","RawResponses","RawUserDetails","RawTemplateList","RawCreateCollector","RawSendFlow","RawCreateFlow"},
 		"Information"		-> "A service for sending and receiving data from SurveyMonkey"
}

(*Raw*)
surveymonkeyimport[rawdata_]:=ImportString[rawdata,"JSON"]

surveymonkeydata["RawSurveyList"]:={
        "URL"				-> "https://api.surveymonkey.net/v2/surveys/get_survey_list",
        "HTTPSMethod"		-> "POST",
        "BodyData"			-> {"ParameterlessBodyData"},
        "Parameters"		-> {},
        "RequiredParameters"-> {},
        "ResultsFunction"	-> surveymonkeyimport
}

surveymonkeydata["RawSurveyDetails"]:={
        "URL"				-> "https://api.surveymonkey.net/v2/surveys/get_survey_details",
        "HTTPSMethod"		-> "POST",
        "BodyData"			-> {"ParameterlessBodyData"},
        "Parameters"		-> {},
        "RequiredParameters"-> {},
        "ResultsFunction"	-> surveymonkeyimport
}
        
surveymonkeydata["RawCollectorList"]:={
        "URL"				-> "https://api.surveymonkey.net/v2/surveys/get_collector_list",
        "HTTPSMethod"		-> "POST",
        "BodyData"			-> {"ParameterlessBodyData"},
        "Parameters"		-> {},
        "RequiredParameters"-> {},
        "ResultsFunction"	-> surveymonkeyimport
}

surveymonkeydata["RawResponseCounts"]:={
        "URL"				-> "https://api.surveymonkey.net/v2/surveys/get_response_counts",
        "HTTPSMethod"		-> "POST",
        "BodyData"			-> {"ParameterlessBodyData"},
        "Parameters"		-> {},
        "RequiredParameters"-> {},
        "ResultsFunction"	-> surveymonkeyimport
}

surveymonkeydata["RawRespondentList"]:={
        "URL"				-> "https://api.surveymonkey.net/v2/surveys/get_respondent_list",
        "HTTPSMethod"		-> "POST",
        "BodyData"			-> {"ParameterlessBodyData"},
        "Parameters"		-> {},
        "RequiredParameters"-> {},
        "ResultsFunction"	-> surveymonkeyimport
}

surveymonkeydata["RawResponses"]:={
        "URL"				-> "https://api.surveymonkey.net/v2/surveys/get_responses",
        "HTTPSMethod"		-> "POST",
        "BodyData"			-> {"ParameterlessBodyData"},
        "Parameters"		-> {},
        "RequiredParameters"-> {},
        "ResultsFunction"	-> surveymonkeyimport
}

surveymonkeydata["RawUserDetails"]:={
        "URL"				-> "https://api.surveymonkey.net/v2/user/get_user_details",
        "HTTPSMethod"		-> "POST",
        "BodyData"			-> {"ParameterlessBodyData"},
        "Parameters"		-> {},
        "RequiredParameters"-> {},
        "ResultsFunction"	-> surveymonkeyimport
}

surveymonkeydata["RawTemplateList"]:={
        "URL"				-> "https://api.surveymonkey.net/v2/templates/get_template_list",
        "HTTPSMethod"		-> "POST",
        "BodyData"			-> {"ParameterlessBodyData"},
        "Parameters"		-> {},
        "RequiredParameters"-> {},
        "ResultsFunction"	-> surveymonkeyimport
}
 
surveymonkeydata["RawCreateCollector"]:={
        "URL"				-> "https://api.surveymonkey.net/v2/collectors/create_collector",
        "HTTPSMethod"		-> "POST",
        "BodyData"			-> {"ParameterlessBodyData"},
        "Parameters"		-> {},
        "RequiredParameters"-> {},
        "ResultsFunction"	-> surveymonkeyimport
}

surveymonkeydata["RawSendFlow"]:={
        "URL"				-> "https://api.surveymonkey.net/v2/batch/send_flow",
        "HTTPSMethod"		-> "POST",
        "BodyData"			-> {"ParameterlessBodyData"},
        "Parameters"		-> {},
        "RequiredParameters"-> {},
        "ResultsFunction"	-> surveymonkeyimport
}

surveymonkeydata["RawCreateFlow"]:={
        "URL"				-> "https://api.surveymonkey.net/v2/batch/create_flow",
        "HTTPSMethod"		-> "POST",
        "BodyData"			-> {"ParameterlessBodyData"},
        "Parameters"		-> {},
        "RequiredParameters"-> {},
        "ResultsFunction"	-> surveymonkeyimport
}               
(*Cooked*)

surveymonkeycookeddata[req_, id_]:=surveymonkeycookeddata[req, id,{}]

surveymonkeycookeddata[prop_,id_,rules___Rule]:=surveymonkeycookeddata[prop,id,{rules}]

camelCase[text_] := Module[{split, partial}, (
    split = StringSplit[text, {" ","_","-"}];
    partial = Prepend[Rest[Characters[#]], ToUpperCase[Characters[#][[1]]]] & /@ split;
    StringJoin[partial]
    )]

fieldsRules = {"survey_id"->"SurveyID","title" -> "Title", "analysis_url" -> "AnalysisURL", "preview_url" -> "PreviewURL", "date_created" -> "DateCreated", 
 "date_modified" -> "DateModified", "language_id" -> "LanguageID", "question_count" -> "QuestionCount", "num_responses" -> "NumResponses","page_id"->"PageID",
 "question_id"->"QuestionID","answer_id"->"AnswerID","collector_id"->"CollectorID","url"->"URL", "open"->"Open", "type"->"Type", "name"->"Name",
  "custom_id" -> "CustomID", "ip_address" -> "IPAddress", "recipient_id" -> "RecipientID", "date_start" -> "DateStart", "collection_mode" -> "CollectionMode",
  "email" -> "Email", "first_name" -> "FirstName", "last_name" -> "LastName", "status" -> "Status",
  "short_description" -> "ShortDescription", "long_description" -> "LongDescription", "is_available_to_current_user" -> "IsAvailableToCurrentUser",
  "is_featured" -> "IsFeatured", "is_certified" -> "IsCertified", "page_count" -> "PageCount", "category_name" -> "CategoryName","category_description" -> "CategoryDescription",
  "redirect_url"->"RedirectURL","respondent_id"->"RespondentID","user_id"->"UserID","category_id"->"CategoryID"}

surveymonkeycookeddata["SurveyResults", id_,args_]:=Block[{rawdata,params={},invalidParameters,withCamelTitles,resp,questions,GetAnswers,maxitems,startindex,mod,surveyID,responses},
	invalidParameters = Select[Keys[args],!MemberQ[{"SurveyID","MaxItems","StartIndex"},#]&]; 
	If[Length[invalidParameters]>0,
	(
		Message[ServiceObject::noget,#,"SurveyMonkey"]&/@invalidParameters;
		Throw[$Failed]
	)];
	If[KeyExistsQ[args,"SurveyID"],
	(
		If[!(StringQ["SurveyID"/.args]||IntegerQ["SurveyID"/.args]),
		(	
			Message[ServiceExecute::nval,"SurveyID","SurveyMonkey"];
			Throw[$Failed]
		)];
		params = Append[params,Rule["survey_id",ToString["SurveyID"/.args]]];
		surveyID = ToString["SurveyID"/.args]
	),
	(
		Message[ServiceExecute::nparam,"SurveyID","SurveyMonkey"];
		Throw[$Failed]
	)];
	(*If[KeyExistsQ[args,"MaxItems"],
	(
		If[!(IntegerQ["MaxItems"/.args]&&("MaxItems"/.args)>0&&("MaxItems"/.args)<=100),
		(	
			Message[ServiceExecute::nval,"MaxItems","SurveyMonkey"];
			Throw[$Failed]
		)];
		maxitems="MaxItems"/.args
	),
  	(
  		maxitems=10
  	)];
	If[KeyExistsQ[args,"StartIndex"],
	(
		If[!(IntegerQ["StartIndex"/.args]&&("StartIndex"/.args)>0),
		(	
			Message[ServiceExecute::nval,"StartIndex","SurveyMonkey"];
			Throw[$Failed]
		)];
		startindex="StartIndex"/.args
	),
  	(
  		startindex=1
  	)];*)
	resp = ServiceExecute["SurveyMonkey","RawRespondentList","ParameterlessBodyData"->ExportString[params,"JSON"]];
	If[("status"/.resp)!= 0,
   	(
      	Message[ServiceExecute::serrormsg,("errmsg"/.resp)];
       	Throw[$Failed]
 	)];
 	Pause[0.5];
	resp = ("respondents" /. ("data" /. resp))[[All, 1, 2]];
	questions = ServiceExecute["SurveyMonkey","RawSurveyDetails","ParameterlessBodyData"->ExportString[params,"JSON"]];
	If[("status"/.questions)!= 0,
   	(
      	Message[ServiceExecute::serrormsg,("errmsg"/.questions)];
       	Throw[$Failed]
 	)];
	questions = Flatten[Cases[questions, a : Rule["questions", b_] :> b, Infinity], 1];
	mod = Mod[Length[resp], 100];
	responses[respid_]:=Module[{},
		Pause[0.5];
		"data" /. ServiceExecute["SurveyMonkey","RawResponses", {"ParameterlessBodyData" -> ExportString[{"survey_id" -> surveyID, "respondent_ids" -> respid}, "JSON"]}]
	];
	If[mod!=0,
		rawdata = Flatten[responses /@ Append[Partition[resp, 100], Take[resp, -mod]], 1],
		rawdata = Flatten[responses & /@ Partition[resp, 100], 1]
	];
	If[("status"/.rawdata)!= 0,
   	(
      	Message[ServiceExecute::serrormsg,("errmsg"/.rawdata)];
       	Throw[$Failed]
 	)];
 	
 	GetAnswers[data_] := Replace[data, {({"answers" -> a_, "question_id" -> b_}) :> ({"answers" -> Map[(Replace[Replace[#, {
             (List["col" -> f_, "row" -> g_]) :> (List["answer_id" -> {f, g}, "text" -> {("text" /. Select[("answers" /. Select[ questions, (("question_id" /. #) == b) &][[1]]), (("answer_id" /. #) == f) &][[1]]), ("text" /. Select[("answers" /. Select[questions, (("question_id" /. #) == b) &][[1]]), (("answer_id" /. #) == g) &][[1]])}])
             }], {
             ("row" -> "0") :> ("answer_id" -> Missing["NotAvailable"]),
             ("row" -> d_) :> (Sequence["answer_id" -> d, "text" -> ("text" /. Select[("answers" /. Select[questions, (("question_id" /. #) == b) &][[1]]), (("answer_id" /. #) == d) &][[1]])])
            }, Infinity] &), a, {1}], "question_id" -> b,
      "heading" -> ("heading" /. 
         Select[questions, (("question_id" /. #) == b) &][[1]])})
   }, Infinity];
 
 	withCamelTitles=Replace[(GetAnswers /@ (rawdata)),{ Rule[a_, b_] :> Rule[camelCase[a/.fieldsRules], b],(Null | "") -> Missing["NotAvailable"]}, Infinity];
	Dataset[Replace[withCamelTitles, r : {__Rule} :> Association[r], -1]]		
]


surveymonkeycookeddata["SurveyList", id_,args_]:=Block[{rawdata,params={},invalidParameters,withCamelTitles,sdate,edate},
	invalidParameters = Select[Keys[args],!MemberQ[{"StartIndex","MaxItems","StartDate","EndDate","Title","RecipientMail","OrderAsc","Fields"},#]&]; 
	If[Length[invalidParameters]>0,
	(
		Message[ServiceObject::noget,#,"SurveyMonkey"]&/@invalidParameters;
		Throw[$Failed]
	)];
	If[KeyExistsQ[args,"MaxItems"],
	(
		If[!(IntegerQ["MaxItems"/.args]&&("MaxItems"/.args)>0&&("MaxItems"/.args)<=1000),
		(	
			Message[ServiceExecute::nval,"MaxItems","SurveyMonkey"];
			Throw[$Failed]
		)];
		params = Append[params,Rule["page_size","MaxItems"/.args]]
	),
  	(
  		params = Append[params,Rule["page_size",10]]
  	)];
	If[KeyExistsQ[args,"StartIndex"],
	(
		If[!(IntegerQ["StartIndex"/.args]&&("StartIndex"/.args)>0),
		(	
			Message[ServiceExecute::nval,"StartIndex","SurveyMonkey"];
			Throw[$Failed]
		)];
		params = Append[params,Rule["page","StartIndex"/.args]]
	)];
	If[KeyExistsQ[args,"Fields"],
	(
		If[!(MatchQ["Fields"/.args,{__String}]),
		(	
			Message[ServiceExecute::nval,"Fields","SurveyMonkey"];
			Throw[$Failed]
		)];
		params = Append[params,Rule["fields",("Fields"/.args)/.Reverse[fieldsRules,2]]]
	)];
	If[ KeyExistsQ[args,"StartDate"],
	(
		If[!(StringQ["StartDate"/.args]||MatchQ["StartDate"/.args,DateObject[__]]),
		(	
			Message[ServiceExecute::nval,"StartDate","SurveyMonkey"];
			Throw[$Failed]
		)];
		sdate = DateObject[("StartDate" /. args)];
		If[MatchQ[sdate,DateObject[__String]],
		(	
			Message[ServiceExecute::nval,"StartDate","SurveyMonkey"];
			Throw[$Failed]
		)];
        params = Append[params, Rule["start_date",DateString[TimeZoneConvert[sdate,0], {"Year", "-", "Month", "-", "Day", " ", "Time"}]]]     
	)];
	If[ KeyExistsQ[args,"EndDate"],
	(
		If[!(StringQ["EndDate"/.args]||MatchQ["EndDate"/.args,DateObject[__]]),
		(	
			Message[ServiceExecute::nval,"EndDate","SurveyMonkey"];
			Throw[$Failed]
		)];
		edate = DateObject[("EndDate" /. args)];
		If[MatchQ[edate,DateObject[__String]],
		(	
			Message[ServiceExecute::nval,"EndDate","SurveyMonkey"];
			Throw[$Failed]
		)];
        params = Append[params, Rule["end_date",DateString[TimeZoneConvert[edate,0], {"Year", "-", "Month", "-", "Day", " ", "Time"}]]]             
	)];
	If[KeyExistsQ[args,"Title"],
	(
		If[!StringQ["Title"/.args],
		(	
			Message[ServiceExecute::nval,"Title","SurveyMonkey"];
			Throw[$Failed]
		)];
		params = Append[params,Rule["title","Title"/.args]]
	)];
	If[KeyExistsQ[args,"RecipientMail"],
	(
		If[!StringQ["RecipientMail"/.args],
		(	
			Message[ServiceExecute::nval,"RecipientMail","SurveyMonkey"];
			Throw[$Failed]
		)];
		params = Append[params,Rule["recipient_mail","RecipientMail"/.args]]
	)];
	If[KeyExistsQ[args,"OrderAsc"],
	(
		If[!MemberQ[{True,False},"OrderAsc"/.args],
		(	
			Message[ServiceExecute::nval,"OrderAsc","SurveyMonkey"];
			Throw[$Failed]
		)];
		params = Append[params,Rule["order_asc","OrderAsc"/.args]]
	)];
	rawdata = ServiceExecute["SurveyMonkey","RawSurveyList","ParameterlessBodyData"->ExportString[params,"JSON"]];
	If[("status"/.rawdata)!= 0,
   	(
      	Message[ServiceExecute::serrormsg,("errmsg"/.rawdata)];
       	Throw[$Failed]
 	)];
	withCamelTitles=Replace[("surveys" /. ("data" /. rawdata)),{ Rule[a_, b_] :> Rule[camelCase[a/.fieldsRules], b],Null -> Missing["NotAvailable"]}, Infinity]
	/.{(y : ("DateCreated" | "DateModified") -> x_) :> (y -> If[MatchQ[x, _String], DateObject[x, TimeZone -> 0], x])
	};
	Dataset[Replace[withCamelTitles, r : {__Rule} :> Association[r], -1]]		
]

surveymonkeycookeddata["SurveyDetails", id_,args_]:=Block[{rawdata,params={},invalidParameters,withCamelTitles},
	invalidParameters = Select[Keys[args],!MemberQ[{"SurveyID"},#]&]; 
	If[Length[invalidParameters]>0,
	(
		Message[ServiceObject::noget,#,"SurveyMonkey"]&/@invalidParameters;
		Throw[$Failed]
	)];
	If[KeyExistsQ[args,"SurveyID"],
	(
		If[!(StringQ["SurveyID"/.args]||IntegerQ["SurveyID"/.args]),
		(	
			Message[ServiceExecute::nval,"SurveyID","SurveyMonkey"];
			Throw[$Failed]
		)];
		params = Append[params,Rule["survey_id",ToString["SurveyID"/.args]]]
	),
	(
		Message[ServiceExecute::nparam,"SurveyID","SurveyMonkey"];
		Throw[$Failed]
	)];
	rawdata = ServiceExecute["SurveyMonkey","RawSurveyDetails","ParameterlessBodyData"->ExportString[params,"JSON"]];
	If[("status"/.rawdata)!= 0,
   	(
      	Message[ServiceExecute::serrormsg,("errmsg"/.rawdata)];
       	Throw[$Failed]
 	)];
	withCamelTitles=Replace[("data" /. rawdata),{ Rule[a_, b_] :> Rule[camelCase[a/.fieldsRules], b],Null -> Missing["NotAvailable"]}, Infinity]
	/.{(y : ("DateCreated" | "DateModified") -> x_) :> (y -> If[MatchQ[x, _String], DateObject[x, TimeZone -> 0], x])};
	Dataset[Association @@ Replace[withCamelTitles, r : {__Rule} :> Association[r], -1]]		
]

surveymonkeycookeddata["CollectorList", id_,args_]:=Block[{rawdata,params={},invalidParameters,withCamelTitles,sdate,edate},
	invalidParameters = Select[Keys[args],!MemberQ[{"SurveyID","StartIndex","MaxItems","StartDate","EndDate","Name","OrderAsc","Fields"},#]&]; 
	If[Length[invalidParameters]>0,
	(
		Message[ServiceObject::noget,#,"SurveyMonkey"]&/@invalidParameters;
		Throw[$Failed]
	)];
	If[KeyExistsQ[args,"SurveyID"],
	(
		If[!(StringQ["SurveyID"/.args]||IntegerQ["SurveyID"/.args]),
		(	
			Message[ServiceExecute::nval,"SurveyID","SurveyMonkey"];
			Throw[$Failed]
		)];
		params = Append[params,Rule["survey_id",ToString["SurveyID"/.args]]]
	),
	(
		Message[ServiceExecute::nparam,"SurveyID","SurveyMonkey"];
		Throw[$Failed]
	)];
	If[KeyExistsQ[args,"MaxItems"],
	(
		If[!(IntegerQ["MaxItems"/.args]&&("MaxItems"/.args)>0&&("MaxItems"/.args)<=1000),
		(	
			Message[ServiceExecute::nval,"MaxItems","SurveyMonkey"];
			Throw[$Failed]
		)];
		params = Append[params,Rule["page_size","MaxItems"/.args]]
	),
  	(
  		params = Append[params,Rule["page_size",10]]
  	)];
	If[KeyExistsQ[args,"StartIndex"],
	(
		If[!(IntegerQ["StartIndex"/.args]&&("StartIndex"/.args)>0),
		(	
			Message[ServiceExecute::nval,"StartIndex","SurveyMonkey"];
			Throw[$Failed]
		)];
		params = Append[params,Rule["page","StartIndex"/.args]]
	)];
	If[KeyExistsQ[args,"Fields"],
	(
		If[!(MatchQ["Fields"/.args,{__String}]),
		(	
			Message[ServiceExecute::nval,"Fields","SurveyMonkey"];
			Throw[$Failed]
		)];
		params = Append[params,Rule["fields",("Fields"/.args)/.Reverse[fieldsRules,2]]]
	)];
	If[ KeyExistsQ[args,"StartDate"],
	(
		If[!(StringQ["StartDate"/.args]||MatchQ["StartDate"/.args,DateObject[__]]),
		(	
			Message[ServiceExecute::nval,"StartDate","SurveyMonkey"];
			Throw[$Failed]
		)];
		sdate = DateObject[("StartDate" /. args)];
		If[MatchQ[sdate,DateObject[__String]],
		(	
			Message[ServiceExecute::nval,"StartDate","SurveyMonkey"];
			Throw[$Failed]
		)];
        params = Append[params, Rule["start_date",DateString[TimeZoneConvert[sdate,0], {"Year", "-", "Month", "-", "Day", " ", "Time"}]]]     
	)];
	If[ KeyExistsQ[args,"EndDate"],
	(
		If[!(StringQ["EndDate"/.args]||MatchQ["EndDate"/.args,DateObject[__]]),
		(	
			Message[ServiceExecute::nval,"EndDate","SurveyMonkey"];
			Throw[$Failed]
		)];
		edate = DateObject[("EndDate" /. args)];
		If[MatchQ[edate,DateObject[__String]],
		(	
			Message[ServiceExecute::nval,"EndDate","SurveyMonkey"];
			Throw[$Failed]
		)];
        params = Append[params, Rule["end_date",DateString[TimeZoneConvert[edate,0], {"Year", "-", "Month", "-", "Day", " ", "Time"}]]]             
	)];
	If[KeyExistsQ[args,"Name"],
	(
		If[!StringQ["Name"/.args],
		(	
			Message[ServiceExecute::nval,"Name","SurveyMonkey"];
			Throw[$Failed]
		)];
		params = Append[params,Rule["name","Name"/.args]]
	)];
	If[KeyExistsQ[args,"OrderAsc"],
	(
		If[!MemberQ[{True,False},"OrderAsc"/.args],
		(	
			Message[ServiceExecute::nval,"OrderAsc","SurveyMonkey"];
			Throw[$Failed]
		)];
		params = Append[params,Rule["order_asc","OrderAsc"/.args]]
	)];
	rawdata = ServiceExecute["SurveyMonkey","RawCollectorList","ParameterlessBodyData"->ExportString[params,"JSON"]];
	If[("status"/.rawdata)!= 0,
   	(
      	Message[ServiceExecute::serrormsg,("errmsg"/.rawdata)];
       	Throw[$Failed]
 	)];
	withCamelTitles=Replace[("collectors" /. ("data" /. rawdata)),{ Rule[a_, b_] :> Rule[camelCase[a/.fieldsRules], b],Null -> Missing["NotAvailable"]}, Infinity]
	/.{(y : ("DateCreated" | "DateModified") -> x_) :> (y -> If[MatchQ[x, _String], DateObject[x, TimeZone -> 0], x])};
	Dataset[Replace[withCamelTitles, r : {__Rule} :> Association[r], -1]]		
]

surveymonkeycookeddata["ResponseCounts", id_,args_]:=Block[{rawdata,params={},invalidParameters,withCamelTitles},
	invalidParameters = Select[Keys[args],!MemberQ[{"CollectorID"},#]&]; 
	If[Length[invalidParameters]>0,
	(
		Message[ServiceObject::noget,#,"SurveyMonkey"]&/@invalidParameters;
		Throw[$Failed]
	)];
	If[KeyExistsQ[args,"CollectorID"],
	(
		If[!(StringQ["CollectorID"/.args]||IntegerQ["CollectorID"/.args]),
		(	
			Message[ServiceExecute::nval,"CollectorID","SurveyMonkey"];
			Throw[$Failed]
		)];
		params = Append[params,Rule["collector_id",ToString["CollectorID"/.args]]]
	),
	(
		Message[ServiceExecute::nparam,"CollectorID","SurveyMonkey"];
		Throw[$Failed]
	)];
	rawdata = ServiceExecute["SurveyMonkey","RawResponseCounts","ParameterlessBodyData"->ExportString[params,"JSON"]];
	If[("status"/.rawdata)!= 0,
   	(
      	Message[ServiceExecute::serrormsg,("errmsg"/.rawdata)];
       	Throw[$Failed]
 	)];
	withCamelTitles=Replace[("data" /. rawdata),{ Rule[a_, b_] :> Rule[camelCase[a/.fieldsRules], b],Null -> Missing["NotAvailable"]}, Infinity];
	Dataset[Association @@ withCamelTitles]		
]

surveymonkeycookeddata["RespondentList", id_,args_]:=Block[{rawdata,params={},invalidParameters,withCamelTitles,sdate,edate,smoddate,emoddate},
	invalidParameters = Select[Keys[args],!MemberQ[{"SurveyID","CollectorID","StartIndex","MaxItems","StartDate","EndDate","StartModifiedDate","EndModifiedDate","OrderBy","OrderAsc","Fields"},#]&]; 
	If[Length[invalidParameters]>0,
	(
		Message[ServiceObject::noget,#,"SurveyMonkey"]&/@invalidParameters;
		Throw[$Failed]
	)];
	If[KeyExistsQ[args,"SurveyID"],
	(
		If[!(StringQ["SurveyID"/.args]||IntegerQ["SurveyID"/.args]),
		(	
			Message[ServiceExecute::nval,"SurveyID","SurveyMonkey"];
			Throw[$Failed]
		)];
		params = Append[params,Rule["survey_id",ToString["SurveyID"/.args]]]
	),
	(
		Message[ServiceExecute::nparam,"SurveyID","SurveyMonkey"];
		Throw[$Failed]
	)];
	If[KeyExistsQ[args,"CollectorID"],
	(
		If[!(StringQ["CollectorID"/.args]||IntegerQ["CollectorID"/.args]),
		(	
			Message[ServiceExecute::nval,"CollectorID","SurveyMonkey"];
			Throw[$Failed]
		)];
		params = Append[params,Rule["collector_id",ToString["CollectorID"/.args]]]
	)];
	If[KeyExistsQ[args,"MaxItems"],
	(
		If[!(IntegerQ["MaxItems"/.args]&&("MaxItems"/.args)>0&&("MaxItems"/.args)<=1000),
		(	
			Message[ServiceExecute::nval,"MaxItems","SurveyMonkey"];
			Throw[$Failed]
		)];
		params = Append[params,Rule["page_size","MaxItems"/.args]]
	),
  	(
  		params = Append[params,Rule["page_size",10]]
  	)];
	If[KeyExistsQ[args,"StartIndex"],
	(
		If[!(IntegerQ["StartIndex"/.args]&&("StartIndex"/.args)>0),
		(	
			Message[ServiceExecute::nval,"StartIndex","SurveyMonkey"];
			Throw[$Failed]
		)];
		params = Append[params,Rule["page","StartIndex"/.args]]
	)];
	If[KeyExistsQ[args,"Fields"],
	(
		If[!(MatchQ["Fields"/.args,{__String}]),
		(	
			Message[ServiceExecute::nval,"Fields","SurveyMonkey"];
			Throw[$Failed]
		)];
		params = Append[params,Rule["fields",("Fields"/.args)/.Reverse[fieldsRules,2]]]
	)];
	If[ KeyExistsQ[args,"StartDate"],
	(
		If[!(StringQ["StartDate"/.args]||MatchQ["StartDate"/.args,DateObject[__]]),
		(	
			Message[ServiceExecute::nval,"StartDate","SurveyMonkey"];
			Throw[$Failed]
		)];
		sdate = DateObject[("StartDate" /. args)];
		If[MatchQ[sdate,DateObject[__String]],
		(	
			Message[ServiceExecute::nval,"StartDate","SurveyMonkey"];
			Throw[$Failed]
		)];
        params = Append[params, Rule["start_date",DateString[TimeZoneConvert[sdate,0], {"Year", "-", "Month", "-", "Day", " ", "Time"}]]]     
	)];
	If[ KeyExistsQ[args,"EndDate"],
	(
		If[!(StringQ["EndDate"/.args]||MatchQ["EndDate"/.args,DateObject[__]]),
		(	
			Message[ServiceExecute::nval,"EndDate","SurveyMonkey"];
			Throw[$Failed]
		)];
		edate = DateObject[("EndDate" /. args)];
		If[MatchQ[edate,DateObject[__String]],
		(	
			Message[ServiceExecute::nval,"EndDate","SurveyMonkey"];
			Throw[$Failed]
		)];
        params = Append[params, Rule["end_date",DateString[TimeZoneConvert[edate,0], {"Year", "-", "Month", "-", "Day", " ", "Time"}]]]             
	)];
	If[ KeyExistsQ[args,"StartModifiedDate"],
	(
		If[!(StringQ["StartModifiedDate"/.args]||MatchQ["StartModifiedDate"/.args,DateObject[__]]),
		(	
			Message[ServiceExecute::nval,"StartModifiedDate","SurveyMonkey"];
			Throw[$Failed]
		)];
		smoddate = DateObject[("StartModifiedDate" /. args)];
		If[MatchQ[smoddate,DateObject[__String]],
		(	
			Message[ServiceExecute::nval,"StartModifiedDate","SurveyMonkey"];
			Throw[$Failed]
		)];
        params = Append[params, Rule["start_modified_date",DateString[TimeZoneConvert[smoddate,0], {"Year", "-", "Month", "-", "Day", " ", "Time"}]]]     
	)];
	If[ KeyExistsQ[args,"EndModifiedDate"],
	(
		If[!(StringQ["EndModifiedDate"/.args]||MatchQ["EndModifiedDate"/.args,DateObject[__]]),
		(	
			Message[ServiceExecute::nval,"EndModifiedDate","SurveyMonkey"];
			Throw[$Failed]
		)];
		emoddate = DateObject[("EndModifiedDate" /. args)];
		If[MatchQ[emoddate,DateObject[__String]],
		(	
			Message[ServiceExecute::nval,"EndModifiedDate","SurveyMonkey"];
			Throw[$Failed]
		)];
        params = Append[params, Rule["end_modified_date",DateString[TimeZoneConvert[emoddate,0], {"Year", "-", "Month", "-", "Day", " ", "Time"}]]]             
	)];
	If[KeyExistsQ[args,"OrderBy"],
	(
		If[!StringQ["OrderBy"/.args],
		(	
			Message[ServiceExecute::nval,"OrderBy","SurveyMonkey"];
			Throw[$Failed]
		)];
		params = Append[params,Rule["order_by","OrderBy"/.args]]
	)];
	If[KeyExistsQ[args,"OrderAsc"],
	(
		If[!MemberQ[{True,False},"OrderAsc"/.args],
		(	
			Message[ServiceExecute::nval,"OrderAsc","SurveyMonkey"];
			Throw[$Failed]
		)];
		params = Append[params,Rule["order_asc","OrderAsc"/.args]]
	)];
	rawdata = ServiceExecute["SurveyMonkey","RawRespondentList","ParameterlessBodyData"->ExportString[params,"JSON"]];
	If[("status"/.rawdata)!= 0,
   	(
      	Message[ServiceExecute::serrormsg,("errmsg"/.rawdata)];
       	Throw[$Failed]
 	)];
	withCamelTitles=Replace[("respondents" /. ("data" /. rawdata)),{ Rule[a_, b_] :> Rule[camelCase[a/.fieldsRules], b],Null -> Missing["NotAvailable"]}, Infinity]
	/.{(y : ("DateStart" | "DateModified") -> x_) :> (y -> If[MatchQ[x, _String], DateObject[x, TimeZone -> 0], x])};
	Dataset[Replace[withCamelTitles, r : {__Rule} :> Association[r], -1]]		
]

surveymonkeycookeddata["Responses", id_,args_]:=Block[{rawdata,params={},invalidParameters,withCamelTitles},
	invalidParameters = Select[Keys[args],!MemberQ[{"SurveyID","RespondentID"},#]&]; 
	If[Length[invalidParameters]>0,
	(
		Message[ServiceObject::noget,#,"SurveyMonkey"]&/@invalidParameters;
		Throw[$Failed]
	)];
	If[KeyExistsQ[args,"SurveyID"],
	(
		If[!(StringQ["SurveyID"/.args]||IntegerQ["SurveyID"/.args]),
		(	
			Message[ServiceExecute::nval,"SurveyID","SurveyMonkey"];
			Throw[$Failed]
		)];
		params = Append[params,Rule["survey_id",ToString["SurveyID"/.args]]]
	),
	(
		Message[ServiceExecute::nparam,"SurveyID","SurveyMonkey"];
		Throw[$Failed]
	)];
	If[KeyExistsQ[args,"RespondentID"],
	(
		If[!(MatchQ["RespondentID"/.args,{__String|__Integer}]),
		(	
			Message[ServiceExecute::nval,"RespondentID","SurveyMonkey"];
			Throw[$Failed]
		)];
		params = Append[params,Rule["respondent_ids",ToString/@("RespondentID"/.args)]]
	),
	(
		Message[ServiceExecute::nparam,"RespondentID","SurveyMonkey"];
		Throw[$Failed]
	)];
	rawdata = ServiceExecute["SurveyMonkey","RawResponses","ParameterlessBodyData"->ExportString[params,"JSON"]];
	If[("status"/.rawdata)!= 0,
   	(
      	Message[ServiceExecute::serrormsg,("errmsg"/.rawdata)];
       	Throw[$Failed]
 	)];
	withCamelTitles=Replace[("data" /. rawdata),{ Rule[a_, b_] :> Rule[camelCase[a/.fieldsRules], b],Null -> Missing["NotAvailable"]}, Infinity];
	Dataset[Replace[withCamelTitles, r : {__Rule} :> Association[r], -1]]		
]

surveymonkeycookeddata["UserDetails", id_,args_]:=Block[{rawdata,params={},invalidParameters,withCamelTitles},
	invalidParameters = Select[Keys[args],!MemberQ[{},#]&]; 
	If[Length[invalidParameters]>0,
	(
		Message[ServiceObject::noget,#,"SurveyMonkey"]&/@invalidParameters;
		Throw[$Failed]
	)];
	rawdata = ServiceExecute["SurveyMonkey","RawUserDetails","ParameterlessBodyData"->"{}"];
	If[("status"/.rawdata)!= 0,
   	(
      	Message[ServiceExecute::serrormsg,("errmsg"/.rawdata)];
       	Throw[$Failed]
 	)];
	withCamelTitles=Replace[("user_details" /. ("data" /. rawdata)),{ Rule[a_, b_] :> Rule[camelCase[a/.fieldsRules], b],Null -> Missing["NotAvailable"]}, Infinity];
	Dataset[Association @@ withCamelTitles]
]

surveymonkeycookeddata["TemplateList", id_,args_]:=Block[{rawdata,params={},invalidParameters,withCamelTitles,sdate,edate},
	invalidParameters = Select[Keys[args],!MemberQ[{"LanguageID","CategoryID","StartIndex","MaxItems","AvailableToUser","Fields"},#]&]; 
	If[Length[invalidParameters]>0,
	(
		Message[ServiceObject::noget,#,"SurveyMonkey"]&/@invalidParameters;
		Throw[$Failed]
	)];
	If[KeyExistsQ[args,"LanguageID"],
	(
		If[!(IntegerQ["LanguageID"/.args]),
		(	
			Message[ServiceExecute::nval,"LanguageID","SurveyMonkey"];
			Throw[$Failed]
		)];
		params = Append[params,Rule["language_id","LanguageID"/.args]]
	)];
	If[KeyExistsQ[args,"CategoryID"],
	(
		If[!(StringQ["CategoryID"/.args]||IntegerQ["CategoryID"/.args]),
		(	
			Message[ServiceExecute::nval,"CategoryID","SurveyMonkey"];
			Throw[$Failed]
		)];
		params = Append[params,Rule["category_id",ToString["CategoryID"/.args]]]
	)];
	If[KeyExistsQ[args,"MaxItems"],
	(
		If[!(IntegerQ["MaxItems"/.args]&&("MaxItems"/.args)>0&&("MaxItems"/.args)<=1000),
		(	
			Message[ServiceExecute::nval,"MaxItems","SurveyMonkey"];
			Throw[$Failed]
		)];
		params = Append[params,Rule["page_size","MaxItems"/.args]]
	),
  	(
  		params = Append[params,Rule["page_size",10]]
  	)];
	If[KeyExistsQ[args,"StartIndex"],
	(
		If[!(IntegerQ["StartIndex"/.args]&&("StartIndex"/.args)>0),
		(	
			Message[ServiceExecute::nval,"StartIndex","SurveyMonkey"];
			Throw[$Failed]
		)];
		params = Append[params,Rule["page","StartIndex"/.args]]
	)];
	If[KeyExistsQ[args,"Fields"],
	(
		If[!(MatchQ["Fields"/.args,{__String}]),
		(	
			Message[ServiceExecute::nval,"Fields","SurveyMonkey"];
			Throw[$Failed]
		)];
		params = Append[params,Rule["fields",("Fields"/.args)/.Reverse[fieldsRules,2]]]
	)];
	If[KeyExistsQ[args,"AvailableToUser"],
	(
		If[!MemberQ[{True,False},"AvailableToUser"/.args],
		(	
			Message[ServiceExecute::nval,"AvailableToUser","SurveyMonkey"];
			Throw[$Failed]
		)];
		params = Append[params,Rule["show_only_available_to_current_user","AvailableToUser"/.args]]
	)];
	rawdata = ServiceExecute["SurveyMonkey","RawTemplateList","ParameterlessBodyData"->ExportString[params,"JSON"]];
	If[("status"/.rawdata)!= 0,
   	(
      	Message[ServiceExecute::serrormsg,("errmsg"/.rawdata)];
       	Throw[$Failed]
 	)];
	withCamelTitles=Replace[("templates" /. ("data" /. rawdata)),{ Rule[a_, b_] :> Rule[camelCase[a/.fieldsRules], b],Null -> Missing["NotAvailable"]}, Infinity]
	/.{(y : ("DateCreated" | "DateModified") -> x_) :> (y -> If[MatchQ[x, _String], DateObject[x, TimeZone -> 0], x])};
	Dataset[Replace[withCamelTitles, r : {__Rule} :> Association[r], -1]]
]

surveymonkeycookeddata["CreateWeblinkCollector", id_,args_]:=Block[{rawdata,params={},invalidParameters,withCamelTitles},
	invalidParameters = Select[Keys[args],!MemberQ[{"SurveyID","Name"},#]&]; 
	If[Length[invalidParameters]>0,
	(
		Message[ServiceObject::noget,#,"SurveyMonkey"]&/@invalidParameters;
		Throw[$Failed]
	)];
	If[KeyExistsQ[args,"SurveyID"],
	(
		If[!(StringQ["SurveyID"/.args]||IntegerQ["SurveyID"/.args]),
		(	
			Message[ServiceExecute::nval,"SurveyID","SurveyMonkey"];
			Throw[$Failed]
		)];
		params = Append[params,Rule["survey_id",ToString["SurveyID"/.args]]]
	),
	(
		Message[ServiceExecute::nparam,"SurveyID","SurveyMonkey"];
		Throw[$Failed]
	)];
	params = Append[params,Rule["collector",{Rule["type","weblink"]}]];
	If[KeyExistsQ[args,"Name"],
	(
		If[!StringQ["Name"/.args],
		(	
			Message[ServiceExecute::nval,"Name","SurveyMonkey"];
			Throw[$Failed]
		)];
		params=params /.{("collector"/.params) :> Append["collector"/.params,Rule["name","Name"/.args]]}
	)];
	rawdata = ServiceExecute["SurveyMonkey","RawCreateCollector","ParameterlessBodyData"->ExportString[params,"JSON"]];
	If[("status"/.rawdata)!= 0,
   	(
      	Message[ServiceExecute::serrormsg,("errmsg"/.rawdata)];
       	Throw[$Failed]
 	)];
	withCamelTitles=Replace[("collector" /. ("data" /. rawdata)),{ Rule[a_, b_] :> Rule[camelCase[a/.fieldsRules], b],Null -> Missing["NotAvailable"]}, Infinity]
	/.{(y : ("DateCreated" | "DateModified") -> x_) :> (y -> If[MatchQ[x, _String], DateObject[x, TimeZone -> 0], x])};
	Dataset[Association @@ withCamelTitles]
]

(*surveymonkeycookeddata["CreateEmailCollector", id_,args_]:=Block[{rawdata,params={"collector"->{"type"->"email"}},invalidParameters,withCamelTitles},
	invalidParameters = Select[Keys[args],!MemberQ[{"SurveyID","Name","Send","Subject","FromAddress","Body","Recipients"},#]&]; 
	If[Length[invalidParameters]>0,
	(
		Message[ServiceObject::noget,#,"SurveyMonkey"]&/@invalidParameters;
		Throw[$Failed]
	)];
	If[KeyExistsQ[args,"SurveyID"],
	(
		If[!(StringQ["SurveyID"/.args]||IntegerQ["SurveyID"/.args]),
		(	
			Message[ServiceExecute::nval,"SurveyID","SurveyMonkey"];
			Throw[$Failed]
		)];
		params = Append[params,Rule["survey_id",ToString["SurveyID"/.args]]]
	),
	(
		Message[ServiceExecute::nparam,"SurveyID","SurveyMonkey"];
		Throw[$Failed]
	)];
	If[KeyExistsQ[args,"Name"],
	(
		If[!StringQ["Name"/.args],
		(	
			Message[ServiceExecute::nval,"Name","SurveyMonkey"];
			Throw[$Failed]
		)];
		params=params /.{("collector"/.params) :> Append["collector"/.params,Rule["name","Name"/.args]]}
	)];
	rawdata = ServiceExecute["SurveyMonkey","RawCreateCollector","ParameterlessBodyData"->ExportString[params,"JSON"]];
	If[("status"/.rawdata)!= 0,
   	(
      	Message[ServiceExecute::serrormsg,("errmsg"/.rawdata)];
       	Throw[$Failed]
 	)];
	withCamelTitles=Replace[("collector" /. ("data" /. rawdata)),{ Rule[a_, b_] :> Rule[camelCase[a], b],Null -> Missing["NotAvailable"]}, Infinity]
	/.{(y : ("DateCreated" | "DateModified") -> x_) :> (y -> If[MatchQ[x, _String], DateObject[x, TimeZone -> 0], x])};
	Dataset[Association @@ withCamelTitles]
]*)

surveymonkeycookeddata[___]:=$Failed

surveymonkeyrawdata[___]:=$Failed

surveymonkeysendmessage[args_]:=$Failed

End[]

End[]

SetAttributes[{},{ReadProtected, Protected}];

System`Private`RestoreContextPath[];

(* Return two functions to define oauthservicedata, oauthcookeddata  *)

{SurveyMonkeyOAuth`Private`surveymonkeydata,SurveyMonkeyOAuth`Private`surveymonkeycookeddata,SurveyMonkeyOAuth`Private`surveymonkeysendmessage,SurveyMonkeyOAuth`Private`surveymonkeyrawdata}
