
System`Private`NewContextPath[{"OAuthClient`","System`"}];

Begin["FacebookOAuth`"] 

Begin["`Private`"](* Begin Private Context *) 

(******************************* Facebook *************************************)

camelcase=OAuthClient`Private`camelCase;
(* Authentication information *)

facebookdata[]={
		"OAuthVersion"		->"2.0",
		"ServiceName" 		-> "Facebook", 
	 	"AuthorizeEndpoint" -> "https://graph.facebook.com/oauth/authorize", 
     	"AccessEndpoint"    -> "https://graph.facebook.com/oauth/access_token",
     	"RedirectURI"       -> "https://www.wolfram.com/oauthlanding?service=Facebook",
     	"VerifierLabel"     -> "code",
	 	"ClientInfo"		-> {"Wolfram","Token"},
	 	"AuthenticationDialog" :> (OAuthClient`tokenOAuthDialog[#, "Facebook",fbicon]&),
	 	"Gets"				:> Join[{"FriendIDs","Friends",OAuthClient`Private`gridRequests["FriendsGrid"],"UserData","PageData","PermissionList",
	 			"ActivityRecentHistory","ActivityTypes","ActivityWeeklyDistribution", "Cover",
	 			"Books","Family","Feeds","Movies","Music","Photos","PhotoLinks","Picture","Posts",
	 			OAuthClient`Private`gridRequests["WallPostStatisticsGrid"],"WallMostCommentedPostData","WallMostCommentedPost","WallMostLikedPostData","WallMostLikedPost",
	 			"WallPostLength","WallPostLengthTimeline","WallWeeklyAppActivity","WallWordFrequencies"(*,"LikeCommentNetwork",
	 			"CommentNetwork","LikeNetwork","PostLikeCommentNetwork","PostCommentNetwork","PostLikeNetwork","BimodalLikeCommentNetwork","BimodalCommentNetwork","BimodalLikeNetwork"*)
	 			,"TaggedPhotos","TaggedPosts","TaggedVideos",OAuthClient`Private`gridRequests["PostGrid"],"PostEventSeries","PostTimeline"},
	 			facebookuserdataNames,DeleteCases[facebookpageNames,Alternatives@@commonuserpagenames]],
	 	"Posts"				-> {"PostLink","PostMessage"},
	 	"RawGets"			-> {"RawFriendsList", "RawUserData","RawPageData", "RawPermissions", "RawGetPosts","RawGetFeeds","RawUserBooks","RawUserMovies","RawUserMusic","RawUserFamily",
	 			"RawUserPhotos","RawUserPicture","RawPageTagged"},
	 	"RawPosts"			-> {"RawSendPost"},
 		"Information"		-> "A service for sending and receiving data from a Facebook account",
 		"AccessTokenExtractor"	-> "Text/2.0"
}

(* a function for importing the raw data - usually json or xml - from the service *)
facebookimport[$Failed]:=Throw[$Failed]
facebookimport[json_String]:=With[{res=ImportString[json,"JSON"]},
	If[res===$Failed,Throw[$Failed]];
	If[FreeQ[res,_["error"|"Error",_]],
		Association@res,
		Message[ServiceExecute::apierr,gdata[res,{"error","message"}]];
		Throw[$Failed]
	]
]

facebookimport[raw_]:=raw



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
facebookdata["RawFriendsList"] = {
        "URL"				->  (ToString@StringForm["https://graph.facebook.com/v2.3/`1`/friends", formatuser[##]]&), (* only supports "me" as user *)
        "PathParameters"	-> {"UserID"},
        "Parameters" 		-> {"limit","fields"},
        "ResultsFunction"	-> facebookimport,
        "RequiredPermissions"-> {"user_friends"}
    }
 
$facebookuserdatapermissions ={};
facebookdata["RawUserData"] = {
        "URL"				->  (ToString@StringForm["https://graph.facebook.com/v2.3/`1`", formatuser[##]]&),
        "PathParameters"	-> {"UserID"},
        "Parameters" 		-> {"fields"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> facebookimport,
        "RequiredPermissions":> $facebookuserdatapermissions
    }
    
facebookdata["RawAccounts"] = {
        "URL"				->  (ToString@StringForm["https://graph.facebook.com/v2.3/`1`/accounts", formatuser[##]]&),
        "PathParameters"	-> {"UserID"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> facebookimport,
        "RequiredPermissions"-> {}
    }
    
facebookdata["RawPermissions"] = {
        "URL"				-> "https://graph.facebook.com/v2.3/me/permissions",
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> facebookimport,
        "RequiredPermissions"-> {}
    } 
    
facebookdata["RawGetPosts"] = {
        "URL"				->  (ToString@StringForm["https://graph.facebook.com/v2.3/`1`/posts", formatuser[##]]&),
        "PathParameters"	-> {"UserID"},
        "Parameters" 		-> {"limit","fields","summary"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> facebookimport,
        "RequiredPermissions"-> {"read_stream"}
    } 
       
$facebookfieldpermissions ={};
facebookdata["RawGetFeeds"] = {
        "URL"				->  (ToString@StringForm["https://graph.facebook.com/v2.3/`1`/feed", formatuser[##]]&),
        "PathParameters"	-> {"UserID"},
        "Parameters" 		-> {"limit","fields","summary"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> facebookimport,
        "RequiredPermissions":>$facebookfieldpermissions
    } 
     
(* Raw Post *)
facebookdata["RawSendPost"] = {
        "URL"				-> "https://graph.facebook.com/v2.3/me/feed",
        "Parameters"			-> {"message","link"},
        "HTTPSMethod"		-> "POST",
        "ResultsFunction"	-> facebookimport,
        "RequiredPermissions"-> {"publish_actions"}
    }

facebookdata["RawUserBooks"] = {
        "URL"				->  (ToString@StringForm["https://graph.facebook.com/v2.3/`1`/books", formatuser[##]]&),
        "PathParameters"	-> {"UserID"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> facebookimport,
        "RequiredPermissions":> $facebookuserdatapermissions
    }

facebookdata["RawUserMovies"] = {
        "URL"				->  (ToString@StringForm["https://graph.facebook.com/v2.3/`1`/movies", formatuser[##]]&),
        "PathParameters"	-> {"UserID"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> facebookimport,
        "RequiredPermissions":> $facebookuserdatapermissions
    }

facebookdata["RawUserMusic"] = {
        "URL"				->  (ToString@StringForm["https://graph.facebook.com/v2.3/`1`/music", formatuser[##]]&),
        "PathParameters"	-> {"UserID"},
        "Parameters"		-> {"limit"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> facebookimport,
        "RequiredPermissions":> $facebookuserdatapermissions
    }

$facebookphotopermissions={};
facebookdata["RawUserPhotos"] = {
        "URL"				->  (ToString@StringForm["https://graph.facebook.com/v2.3/`1`/photos", formatuser[##]]&),
        "PathParameters"	-> {"UserID"},
        "Parameters"		-> {"limit"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> facebookimport,
        "RequiredPermissions":> $facebookphotopermissions
    }

(*Removing request has been deprecated on Graph API v2.3 https://developers.facebook.com/docs/graph-api/reference/v2.3/user/checkins*)
(*facebookdata["RawUserPlaces"] = {
        "URL"				->  (ToString@StringForm["https://graph.facebook.com/`1`/checkins", formatuser[##]]&),
        "PathParameters"	-> {"UserID"},
        "Parameters"		-> {"limit"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> facebookimport,
        "RequiredPermissions":> $facebookuserdatapermissions
    }*)
    
facebookdata["RawUserPicture"] = {
        "URL"				->  (ToString@StringForm["https://graph.facebook.com/v2.3/`1`/picture", formatuser[##]]&),
        "PathParameters"	-> {"UserID"},
        "Parameters"		-> {"type","redirect","height","width"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> Identity,
        "RequiredPermissions"-> {}
    }

(*This endpoint has been removed from the API (https://developers.facebook.com/docs/graph-api/reference/v2.3/user/mutualfriends). There's an alternative one but has not worked for me yet*)    
(*facebookdata["RawMutualFriends"] = { (* the documentation lists userida and useridb, but the results onlt depend on useridb and give the mutual friends of "me" *)
        "URL"				-> (ToString@StringForm["https://graph.facebook.com/me/mutualfriends/`1`", formatuser[#]]&),
        "HTTPSMethod"		-> "GET",
        "PathParameters"	-> {"UserID"},
        "RequiredParameters"-> {"UserID"},
        "ResultsFunction"	-> facebookimport
    }*)

facebookdata["RawUserFamily"] = {
        "URL"				-> (ToString@StringForm["https://graph.facebook.com/v2.3/`1`/family", formatuser[##]]&),
        "HTTPSMethod"		-> "GET",
        "PathParameters" 	-> {"UserID"},
        "Parameters" 		-> {"fields"},
        "RequiredPermissions":> $facebookuserdatapermissions
    }
 
(*Request has been deprecated in Graph API https://developers.facebook.com/docs/reference/fql/*)
(*facebookdata["RawQuery"] = {
        "URL"				-> "https://graph.facebook.com/fql",
        "HTTPSMethod"		-> "GET",
        "Parameters" 		-> {"q"},
        "RequiredPermissions"->{}
    }  *) 
    
facebookdata["RawPageData"] = {
        "URL"				->  (ToString@StringForm["https://graph.facebook.com/v2.3/`1`", formatpage[##]]&),
        "PathParameters"	-> {"PageID"},
        "Parameters" 		-> {"fields"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> facebookimport,
        "RequiredPermissions"->{}
    }
    
    
facebookdata["RawPageTagged"] = {
        "URL"				->  (ToString@StringForm["https://graph.facebook.com/v2.3/`1`/tagged", formatpage[##]]&),
        "PathParameters"	-> {"PageID"},
        "Parameters" 		-> {"fields"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> facebookimport,
        "RequiredPermissions"->{}
    }
 
facebookdata["icon"]:=fbicon
   
facebookdata[___]:=$Failed
(****** Cooked Properties ******)
  
(* cooked data queries 
	Cooked queries call at least one of the raw queried underneath but add pre and post processing to make the function easier to use and the result clearer.
*)  

facebookcookeddata[prop_,id_,rule_Rule, rest___]:=facebookcookeddata[prop,id,{rule}, rest]
facebookcookeddata[prop_,id_]:=facebookcookeddata[prop,id,{}]
  
(* Cooked *)

(* Cooked *)
facebookcookeddata["PermissionList",id_,args_]:=Module[
    {rawdata,as, params},
    params=filterparameters[args,getallparameters["RawPermissions"]];
    rawdata=OAuthClient`rawoauthdata[id,"RawPermissions",params];
    as=facebookimport[rawdata];
    If[KeyExistsQ[as,"data"],
        Flatten[checkFacebookPermissions[as["data"]]]
        ,
        {}
    ]
]
 
checkFacebookPermissions[l_List]:=checkfacebookPermissions/@l
checkFacebookPermissions[_]:={}

checkfacebookPermissions[p_]:=If[("status"/.p)==="granted",
    {"permission"/.p}/."permission"->{},{}
]/;KeyExistsQ[p,"status"]

checkfacebookPermissions[p_]:=Cases[p,(perm_->1):>perm,Infinity]
(******************** USERS **************************)
facebookuserdatafields={"id", "name", "first_name", "middle_name", "last_name", "gender", "locale", "languages", "link",
                "third_party_id", "installed", "timezone", "updated_time", "verified", "bio",
        		"birthday", "cover", "currency", "devices", "education", "email", "hometown", "interested_in",
                "location", "political", "favorite_athletes", "favorite_teams", "picture", "quotes", "relationship_status",
                "religion", "security_settings", "significant_other", "video_upload_limits", "website", "work"};
facebookuserdataNames=(camelcase/@facebookuserdatafields)/.{"Name"->"FullName","ID"->"UserID","Installed"->"WolframConnectedQ",
	"Picture"->"PictureLink","Cover"->"CoverLink"};
facebookuserdatapermissionrules={"languages"->"user_likes","bio"->"user_about_me","education"->"user_education_history",
	"email"->"email","hometown"->"user_hometown","interested_in"->"user_relationship_details","location"->"user_location",
	"political"->"user_religion_politics","favorite_athletes"->"user_likes","favorite_teams"->"user_likes","quotes"->"user_about_me",
	"relationship_status"->"user_relationships","religion"->"user_religion_politics","significant_other"->"user_relationships",
	"website"->"user_website","work"->"user_work_history",_String:>Sequence@@{}};

commonuserpagenames={"Birthday", "CoverLink", "Hometown", "Link", "Location", "Username", "Website"}; 
              
facebookcookeddata["UserData",id_,args_]:=Block[
	{$facebookuserdatapermissions,rawdata,params,data},
	params=filterparameters[args,getallparameters["RawUserData"]];
	params=params/.HoldPattern[Rule[a_,b_Integer]]:>Rule[a,ToString[b]];
	If[MatchQ["UserID"/.params,"me"|"UserID"],
		$facebookuserdatapermissions={"user_likes","user_about_me","user_birthday","user_education_history","email",
			"user_hometown","user_relationship_details","user_location","user_religion_politics",
			"user_website","user_work_history","user_relationships"},
		$facebookuserdatapermissions={"friends_likes", "friends_about_me", "friends_birthday", 
			"friends_education_history", "email", "friends_hometown", "friends_relationship_details", "friends_location", 
			"friends_religion_politics", "friends_website", "friends_work_history", "friends_relationships"}
	];
	rawdata=OAuthClient`rawoauthdata[id,"RawUserData",Join[params,{"fields"->StringJoin[Riffle[facebookuserdatafields,","]]}]];      
	data=facebookimport[rawdata];
	Association[Replace[(Replace[Normal[data],HoldPattern[Rule[a_String, b_]] :> Rule[a,userdataparse[b,a]],Infinity] )/. Thread[facebookuserdatafields -> facebookuserdataNames], 
 		HoldPattern[Rule[a_String, b_]] :> Rule[camelcase[a], b], Infinity]]   
]

facebookcookeddata[prop:(Alternatives@@facebookuserdataNames),id_,args_]:=Block[
	{field=prop/.Thread[facebookuserdataNames->facebookuserdatafields],
		$facebookuserdatapermissions,rawdata, res,params},
		$facebookuserdatapermissions={field/.facebookuserdatapermissionrules};
		params=filterparameters[args,getallparameters["RawUserData"]];
		params=params/.HoldPattern[Rule[a_,b_Integer]]:>Rule[a,ToString[b]];
		If[!MatchQ["UserID"/.params,"me"|"UserID"],
			$facebookuserdatapermissions=StringReplace[$facebookuserdatapermissions,"user"->"friends"]
		];
		rawdata=OAuthClient`rawoauthdata[id,"RawUserData",Join[{"fields"->field},params]];          
     	res=facebookimport[rawdata];
     	If[KeyExistsQ[res,field],userdataparse[Lookup[res,field],field],Missing["NotAvailable"]]           
]/;FreeQ[args,"PageID"]||!MemberQ[commonuserpagenames,prop]

facebookcookeddata["FriendsGrid",id_,args_]:=Module[
	{rawdata,as,params},
	params=filterparameters[args,getallparameters["RawFriendsList"]];
		params=params/.HoldPattern[Rule[a_,b_Integer]]:>Rule[a,ToString[b]];
	rawdata=OAuthClient`rawoauthdata[id,"RawFriendsList",params];
	as=facebookimport[rawdata];
	If[KeyExistsQ[as,"data"],
		OAuthClient`Private`prettygrid[Join[{{"Name","User ID"}},{"name", "id"} /. as[["data"]]]],
		{}
	]
]

facebookcookeddata[prop:("Books"|"Movies"|"Music"),id_,args_]:=Block[
	{data,params,rawdata,rawprop,$facebookuserdatapermissions},
		rawprop="RawUser"<>prop;
		params=filterparameters[args,getallparameters[rawprop]];
		params=params/.HoldPattern[Rule[a_,b_Integer]]:>Rule[a,ToString[b]];
		If[!MatchQ["UserID"/.params,"me"|"UserID"],
			$facebookuserdatapermissions={"friends_likes"},
			$facebookuserdatapermissions={"user_likes"}
		];
		rawdata=OAuthClient`rawoauthdata[id,rawprop,params];          
     	data=facebookimport[rawdata];
     	(
     		Association[Replace[#,HoldPattern[Rule][a_String,b_]:>Rule[camelcase[a],b],Infinity]/.{
     			"ID"->(prop<>"ID"),fval["UpdatedTime"->readdate],fval["CreatedTime"->readdate]}]&/@data["data"]
     	)/;res=!=$Failed
]

facebookcookeddata[prop:"Picture",id_,args_]:=Block[
	{data,params,rawdata,rawprop,img,$facebookuserdatapermissions={}},
		params=filterparameters[Join[args,{"redirect"->"false"}],getallparameters["RawUserPicture"]];
		params=params/.HoldPattern[Rule[a_,b_Integer]]:>Rule[a,ToString[b]];
		rawdata=OAuthClient`rawoauthdata[id,"RawUserPicture",params]; 
		img=If[MatchQ["redirect"/.params,"false"],
			(* we have the meta data *)
     		data=facebookimport[rawdata];
     		iImportImage["url"/.data["data"]]
			
			,
			(* we have the image *)
			ImportString[rawdata]	
		];
		img/;ImageQ[img]
]

facebookcookeddata[prop:"Cover",id_,args_]:=Block[
	{data,params,rawdata,rawprop,img,$facebookuserdatapermissions={}},
		params=filterparameters[args/."PageID"->"UserID",getallparameters["RawUserData"]];
		params=params/.HoldPattern[Rule[a_,b_Integer]]:>Rule[a,ToString[b]];
		rawdata=OAuthClient`rawoauthdata[id,"RawUserData",Join[{"fields"->"cover"},params]]; 
		data=facebookimport[rawdata];
		If[data===$Failed,Return[Missing["NotAvailable"]]];
		If[KeyExistsQ[data,"cover"],
			img=iImportImage["source"/.data["cover"]];
			
			If[ImageQ[img],img,Missing["NotAvailable"]]
			,
			Missing["NotAvailable"]
		]
]

facebookcookeddata["Family",id_,args_]:=Block[
	{res,params,rawdata,$facebookuserdatapermissions},
		params=filterparameters[args,getallparameters["RawUserFamily"]];
		params=params/.HoldPattern[Rule[a_,b_Integer]]:>Rule[a,ToString[b]];
		If[!MatchQ["UserID"/.params,"me"|"UserID"],
			$facebookuserdatapermissions={"friends_relationships"},
			$facebookuserdatapermissions={"user_relationships"}
		];
		rawdata=OAuthClient`rawoauthdata[id,"RawUserFamily",params];          
     	res=facebookimport[rawdata];
     	(
     		res=res["data"];
     		If[res==={},
     			res,
     			res/.HoldPattern[Rule[a_String,b_]]:>Rule[camelcase[a],b]
     		]
     	)/;res=!=$Failed
]

facebookcookeddata[prop:("FriendIDs"|"Friends"),id_,args_]:=Block[
	{data,params,rawdata},
		params=filterparameters[args,getallparameters["RawFriendsList"]];
		params=params/.HoldPattern[Rule[a_,b_Integer]]:>Rule[a,ToString[b]];
		rawdata=OAuthClient`rawoauthdata[id,"RawFriendsList",Join[params,{"limit"->"500"}]];          
     	data=facebookimport[rawdata];
     	(
     		Replace[If[prop==="Friends","name","id"]/.data["data"],{"name"->{},"id"->{}},{0}]
     	)/;data=!=$Failed
]

(*Remove because RawMutualFriends has been removed from the API*)
(*facebookcookeddata[prop:("MutualFriends"|"MutualFriendIDs"),id_,args_]:=Block[
	{data,params,rawdata},
		params=filterparameters[args,getallparameters["RawMutualFriends"]];
		params=params/.HoldPattern[Rule[a_,b_Integer]]:>Rule[a,ToString[b]];
		rawdata=OAuthClient`rawoauthdata[id,"RawMutualFriends",Join[params,{"limit"->"500"}]];          
     	data=facebookimport[rawdata];
     	(
     		If[prop==="MutualFriends","name","id"]/.data["data"]/.{"name"->{},"id"->{}}
     	)/;data=!=$Failed
]*)

$facebookphotolimit=20;

facebookcookeddata[prop:("Photos"|"PhotoLinks"),id_,args_]:=Block[
	{res,params,rawdata,$facebookuserdatapermissions,links},
		params=filterparameters[args,getallparameters["RawUserPhotos"]];
		params=params/.HoldPattern[Rule[a_,b_Integer]]:>Rule[a,ToString[b]];
		If[!MatchQ["UserID"/.params,"me"|"UserID"],
			$facebookphotopermissions={"friends_photos"},
			$facebookphotopermissions={"user_photos"}
		];
		rawdata=OAuthClient`rawoauthdata[id,"RawUserPhotos",Join[params,{"limit"->ToString[$facebookphotolimit]}]];          
     	res=facebookimport[rawdata];
     	(
     		res=res["data"];
     		If[res==={},
     			res,
     			links="images"/.res;
     			If[MatchQ[links,{}|"images"],
     				{},
     				links=Flatten["source"/.links[[All,1]]];
     				If[MatchQ[links,{}|"source"],Return[{}]];
     				If[prop==="Photos",
	     				res=iImportImage/@links;
	     				DeleteCases[Flatten[res], Missing["NotAvailable"]]
	     				,
	     				Hyperlink/@links
	     				
	     			]
     			]
     			
     		]
     	)/;res=!=$Failed
]

(*Removing because RawUserPlaces request has been deprecated on Graph API v2.3 https://developers.facebook.com/docs/graph-api/reference/v2.3/user/checkins*)
(*facebookcookeddata["Places",id_,args_]:=Block[
	{res,params,rawdata,$facebookuserdatapermissions,place,time},
		params=filterparameters[args,getallparameters["RawUserPlaces"]];
		params=params/.HoldPattern[Rule[a_,b_Integer]]:>Rule[a,ToString[b]];
		If[!MatchQ["UserID"/.params,"me"|"UserID"],
			$facebookuserdatapermissions={"friends_status"},
			$facebookuserdatapermissions={"user_status"}
		];
		rawdata=OAuthClient`rawoauthdata[id,"RawUserPlaces",params];          
     	res=facebookimport[rawdata];
     	(
     		res=res["data"];
     		If[res==={},
     			res,
     			place="place"/.res;
     			time=Thread["CreatedTime"->("created_time"/.res)];
     			res=MapThread[fixlatlong[Join[getlocation[#],FilterRules[#,Except["location"]],{#2}]]&,{place,time}]/.fval["CreatedTime"->readdate];
     			Association[Replace[#,HoldPattern[Rule][a_String,b_]:>Rule[camelcase[a],b],Infinity]/.{"ID"->("Place"<>"ID")}]&/@res
     		]
     	)/;res=!=$Failed
]*)

getlocation[l_]:=With[{loc="location"/.l},
	Sequence@@Switch[loc,
		"location",{},
		_List,{loc},
		_,{{"Location"->loc}}
	]
]
(************* PAGES ******************)

facebookpagefields={"id","about", "attire", "band_members", "best_page", "birthday", "booking_agent", "can_post", "category", "category_list", "checkins", 
	"company_overview", "cover", "current_location", "description", "directed_by", "founded", "general_info", "general_manager", 
	"hometown", "is_published", "is_unclaimed", "likes", "link", "location", "mission", "name", "parking", "phone", "press_contact", 
	"price_range", "products", "restaurant_services", "restaurant_specialties", "talking_about_count", "username", "website", "were_here_count"};
	
facebookpageNames=(camelcase/@facebookpagefields)/.{"Name"->"PageName","ID"->"PageID","Cover"->"CoverLink"};

facebookcookeddata["PageData",id_,args_]:=Block[
	{rawdata,params,data},
	params=filterparameters[args,getallparameters["RawPageData"]];
		params=params/.HoldPattern[Rule[a_,b_Integer]]:>Rule[a,ToString[b]];
	If[FreeQ[params,"PageID"],Message[ServiceExecute::nparam,"PageID"];Throw[$Failed]];
	rawdata=OAuthClient`rawoauthdata[id,"RawPageData",Join[params,{"fields"->StringJoin[Riffle[facebookpagefields,","]]}]];      
	data=facebookimport[rawdata];
	Association[Replace[Normal[data],HoldPattern[Rule[a_String, b_]] :> Rule[a,pagedataparse[b,a]],Infinity]/.Thread[facebookpagefields->facebookpageNames]]   
]

facebookcookeddata[prop:(Alternatives@@facebookpageNames),id_,args_]:=Block[
	{field=prop/.Thread[facebookpageNames->facebookpagefields],rawdata, res,params},
	params=filterparameters[args,getallparameters["RawPageData"]];
		params=params/.HoldPattern[Rule[a_,b_Integer]]:>Rule[a,ToString[b]];
	If[FreeQ[params,"PageID"],Message[ServiceExecute::nparam,"PageID"];Throw[$Failed]];
	rawdata=OAuthClient`rawoauthdata[id,"RawPageData",Join[{"fields"->field},params]];          
 	res=facebookimport[rawdata];
 	If[KeyExistsQ[res,field],pagedataparse[res[field],field],Missing["NotAvailable"]]           
]


facebookcookeddata[prop:("TaggedPhotos"|"TaggedPosts"|"TaggedVideos"),id_,args_]:=Block[
	{rawdata, res,params,types,pos},
	params=filterparameters[args,getallparameters["RawPageTagged"]];
		params=params/.HoldPattern[Rule[a_,b_Integer]]:>Rule[a,ToString[b]];
	If[FreeQ[params,"PageID"],Message[ServiceExecute::nparam,"PageID"];Throw[$Failed]];
	rawdata=OAuthClient`rawoauthdata[id,"RawPageTagged",params]; 
 	res=facebookimport[rawdata];
 	res=If[KeyExistsQ[res,"data"],res["data"],Return[Missing["NotAvailable"]]];
 	types="type"/.res;
 	pos=Flatten[Position[types,
 		Switch[prop,"TaggedPhotos","photo","TaggedVideos","video",_,Except["photo"|"video"]]
 		,{1},Heads->False
 	]];
 	If[pos==={}||!ListQ[pos],Return[{}]];
 	res=res[[pos]];
	Switch[prop,
		"TaggedPhotos",iImportImage/@("picture"/.res),
		"TaggedVideos",Hyperlink@@@({iImportImage[#[[1]]],#[[2]]}&/@({"picture","link"}/.res)),
		_,((FilterRules[#,{"message","id","created_time"}]&/@res)/.HoldPattern[Rule[a_,b_]]:>Rule[camelcase[a],b])/.fval["CreatedTime"->readdate]		
	]       
]
(********************* Posts **************************)

facebookpostfields={"id", "from", "to", "message", "message_tags", "picture", "link", "name", "caption", "description",
                                "source", "properties", "icon", "actions", "privacy", "type", "likes", "place", "story", "story_tags",
                                "with_tags", "comments", "object_id", "application", "created_time", "updated_time","status_type"};
facebookpostNames=(("Post"<>camelcase[#])&/@facebookpostfields);
facebookpostDisplayNames=(camelcase/@facebookpostfields)/.{"ID"->"PostID"};
facebookpostpermissionrules={"privacy"->"read_stream","place"->"read_stream","story"->"read_stream","story_tags"->"read_stream",
	"with_tags"->"read_stream","object_id"->"read_stream","comments"->"read_stream","application"->"read_stream",
	"created_time"->"read_stream","updated_time"->"read_stream","status_type"->"read_stream",
	_String:>Sequence@@{}};

facebookcookeddata[prop:("Feeds"|"Posts"),id_,args_]:=Block[
	{data,params,rawdata,$facebookfieldpermissions={"read_stream"}, rawprop},
		rawprop="RawGet"<>prop;
		params=filterparameters[args,getallparameters[rawprop]];
		params=params/.HoldPattern[Rule[a_,b_Integer]]:>Rule[a,ToString[b]];
		rawdata=OAuthClient`rawoauthdata[id,rawprop,Join[params,{"fields"->StringJoin[Riffle[facebookpostfields,","]],"limit"->"500"}]];          
     	data=facebookimport[rawdata];
     	(
     		formatposts[data["data"]]
     	)/;data=!=$Failed
]
 
facebookcookeddata[prop:("PostGrid"|"PostEventSeries"|"PostTimeline"),id_,args_]:=Block[
	{data,params,rawdata,$facebookfieldpermissions={"read_stream"}, rawprop,dates,messages,ids},
	params=filterparameters[args,getallparameters["RawGetPosts"]];
	params=params/.HoldPattern[Rule[a_,b_Integer]]:>Rule[a,ToString[b]];
	rawdata=OAuthClient`rawoauthdata[id,"RawGetPosts",Join[params,{"fields"->"id,created_time,message","limit"->"500"}]];          
    data=facebookimport[rawdata];
    data=Association/@Lookup[data,"data",{}];
	dates=Lookup[#,"created_time",""]&/@data;
	messages=Lookup[#,"message",""]&/@data;
	ids=Lookup[#,"id",""]&/@data;
	data=Cases[Transpose[{dates, messages, ids}], _?(FreeQ[#, ""] &)];
	data[[All,1]]=readdate/@data[[All,1]];
	Switch[prop,
		"PostGrid",
		OAuthClient`Private`prettygrid[Join[{{"CreatedTime","Message","PostID"}},	data]],
		"PostEventSeries",
		If[data!={},
		(
			EventSeries[data[[All,1;;2]]]
		),
		(
			Missing["NotAvailable"]
		)],
		"PostTimeline",
		OAuthClient`Private`eventtimeline[data[[All,2]],data[[All,1]]]
	]
	
]

facebookcookeddata["PostMessage",id_,args_]:=Module[
	{rawdata,as, params},
	params=filterparameters[args,getallparameters["RawSendPost"]];
		params=params/.HoldPattern[Rule[a_,b_Integer]]:>Rule[a,ToString[b]];
	If[FreeQ[params,#],Message[ServiceExecute::nparam,#1];Throw[$Failed]]&/@{"message"};
	rawdata=OAuthClient`rawoauthdata[id,"RawSendPost",params];
	as=facebookimport[rawdata];
	("message"/.params)/;MatchQ[as,_Association]
]
 
facebookcookeddata["PostLink",id_,args_]:=Module[
	{rawdata,as, params},
	params=filterparameters[args,getallparameters["RawSendPost"]];
		params=params/.HoldPattern[Rule[a_,b_Integer]]:>Rule[a,ToString[b]];
	If[FreeQ[params,#],Message[ServiceExecute::nparam,#1];Throw[$Failed]]&/@{"link"};
	
	rawdata=OAuthClient`rawoauthdata[id,"RawSendPost",params];
	as=facebookimport[rawdata];
	("link"/.params)/;MatchQ[as,_Association]
]

(******************* User Activity **********************)

facebookactivitydata[id_,args_]:=Block[
	{rawdata,as, params,OAuthClient`$CacheResults=True},
	params=filterparameters[args,getallparameters["RawGetPosts"]];	
		params=params/.HoldPattern[Rule[a_,b_Integer]]:>Rule[a,ToString[b]];
	rawdata=OAuthClient`rawoauthdata[id,"RawGetPosts",Join[params,{"limit" -> "500",
		"fields" -> "id,from,to,message,message_tags,picture,link,name,caption,description,source,properties,icon,actions,privacy,type,likes,place,story,story_tags,with_tags,comments,object_id,application,created_time,updated_time"}]];
	as=facebookimport[rawdata];
	as["data"]
]

facebookChartColor = "DarkRainbow";

facebookcookeddata["ActivityRecentHistory",id_,args_]:=Module[
	{rawdata,times,data, date, gatherByAct,rules, days,bardata, items,
	daylength,dayposition,plength,ticks, res},
	rawdata=facebookactivitydata[id,args];
	If[!MemberQ[rawdata,"updated_time", Infinity],
		Return[BarChart[{}]]
	];
	times={"updated_time", "type"}/.rawdata;
	data ={readdate[#1], #2} & @@@ times;
    gatherByAct = GatherBy[data, Last];
    items = gatherByAct[[All, 1, 2]];
    date = Map[DateList,gatherByAct[[All, All, 1]],{2}];    
	rules = ToString[#1] -> #2 & @@@ Tally[#] & /@ date[[All, All, ;; 2]];
	days = Table[ToString[DateList[DateList[data[[-1, 1]]] + {0,i,0,0,0,0}][[ ;; 2]]],
              {i, 0, DateDifference[data[[-1, 1]], data[[1, 1]], "Month"][[1]]}];
	bardata = Transpose[days /. rules /. _String -> 0];
    days=ToExpression[days];
	(

         daylength = Length[days];
         dayposition = Select[FindDivisions[{1, daylength}, 7], IntegerQ[#] && (0 < # < daylength) &];
         plength = Length[dayposition];
         dayposition = Which[plength == 0, {1}, plength > 6, dayposition[[;; 6]], True, dayposition];
         ticks =
            Transpose[{dayposition, DateString[#, {"Month", "/", "Year"}] & /@ days[[dayposition]], ConstantArray[0, Length[dayposition]]}];
         res =
          BarChart[bardata,
             ChartLayout -> "Stacked",
             ChartStyle -> facebookChartColor,
             ChartLegends -> Placed[items, Below],
             FrameTicks -> {{Automatic, None}, {ticks, None}},
             Frame -> True, Axes -> None,
             AxesOrigin -> {0, 0},
             BarSpacing -> {0, 0},
             GridLines -> {None, Automatic},
             GridLinesStyle -> GrayLevel[.8],
             PlotRangeClipping -> True,
             PlotRangePadding -> {{Automatic, Automatic}, {None, Scaled[0.08]}}
          ];
          res /; (Head[res] =!= BarChart)
        ) /; (data =!= $Failed)
]
  
facebookcookeddata["ActivityTypes",id_,args_]:=Module[
	{rawdata,types,tally,res},
	rawdata=facebookactivitydata[id,args];
	If[rawdata =!= $Failed,
	(
		If[MatchQ[rawdata,_List] && Length[rawdata]==0, 
			Missing["NotAvailable"],
			(
				types="type"/.rawdata;
      			tally = Tally[types/. "type" -> {}];
        		tally = Transpose[tally];
        		If[ListQ[tally], 
        			(
        				res = PieChart[tally[[2]], ChartLegends -> tally[[1]], ChartStyle->facebookChartColor];
        				res
        			),
        			$Failed
        		]
        	)
		]
	),$Failed]
		
]
  
facebookcookeddata["ActivityWeeklyDistribution",id_,args_]:=Module[
	{rawdata,times,weeklydata,gatherByActWeekly,weeklyitems,dateweekly,rulesweekly,datelistweekly,
	dayrules,weeklyPart,bubbledata,chartelem,res},
	rawdata=facebookactivitydata[id,args];
	If[rawdata =!= $Failed,
	(
		If[MatchQ[rawdata,_List] && Length[rawdata]==0,
			Missing["NotAvailable"],
			(
				times={"updated_time", "type"}/.rawdata;
 				weeklydata ={StringSplit@DateString[readdate[#1],{"Hour24", " ", "DayNameShort"}
 					(* Using the zero time zone gives backward compatibility with SocialMediaData *)
 					(*, TimeZone->0 *)], #2} & @@@ times;
 				gatherByActWeekly = GatherBy[weeklydata, Last];
 				weeklyitems = gatherByActWeekly[[All, 1, 2]];

 				dateweekly = gatherByActWeekly[[All, All, 1]];
 				rulesweekly = #1 -> #2 & @@@ Tally[#] & /@ dateweekly[[All, All, ;; 2]];
 				datelistweekly = Union[weeklydata[[All, 1]]];

 				dayrules = Thread[{"Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"} -> Range[7]];
 				weeklyPart = datelistweekly /. rulesweekly /. {_String, _} -> 0;
 				bubbledata = Transpose[Join[Transpose[ToExpression[datelistweekly /. dayrules]], {Total[weeklyPart, {1}]}]];
 				(
		         	chartelem = Table[PieChart[d,LabelingFunction->None, ChartStyle->facebookChartColor], {d, Transpose[weeklyPart]}];
    		     	res =BubbleChart[bubbledata,
        		 		ChartElements -> chartelem,
         				BubbleSizes -> {0.06, 0.14},
             			AspectRatio -> .4,
            			ChartLegends -> {None, weeklyitems},
            			FrameTicks -> {{List@@@Reverse[Rule @@@ dayrules, 2], None}, 
		            		{List@@@Table[i -> DateString[{1, 1, 1, i}, {"Hour12Short", " ", "AMPMLowerCase"}], {i, 0, 21, 3}], None}}
    		      	];

        		 	Legended[res, SwatchLegend[facebookChartColor, weeklyitems]] (*/; (Head[res] =!= BubbleChart)*)
        		)
			)]			
	
	),$Failed]	
]


(******************* Wall Activity **********************)

facebookwalldata[id_,args_]:=Module[
	{rawdata,params,as, res},
	params=filterparameters[args,getallparameters["RawGetFeeds"]];	
		params=params/.HoldPattern[Rule[a_,b_Integer]]:>Rule[a,ToString[b]];
	rawdata=OAuthClient`rawoauthdata[id,"RawGetFeeds",Join[params,{"limit" -> "500",
		"fields" -> "id,message,type,likes.summary(1),story,comments.summary(1),created_time,application"}]];
	as=facebookimport[rawdata];
	res=Lookup[as,"data",{}];
	If[MatchQ[res,Null|_Lookup|_Missing],
		{},
		res
	]
]

(*Removed because RawQuery has been removed too.*) 
(*facebookcookeddata["WallPostStatistics",id_,args_]:=Module[
	{wall,postid,pstring,commentInfo,query,totalcomment,messagedata,postlength,
	totallike,avpostwords,avpostcharacter},
	wall=facebookwalldata[id,args];
	If[wall==={},Return[{}]];
	(
          postid = "id" /. wall;
          pstring = ToString[StringJoin[Riffle[ToString[#, InputForm] & /@ postid, ","]]];
          query="SELECT post_id, comment_info FROM stream WHERE post_id IN ("<>pstring<>") LIMIT 5000";
          commentInfo = OAuthClient`rawoauthdata[id,"RawQuery","q"->query];
                   
          If[commentInfo === $Canceled, Return[$Canceled]];
          If[commentInfo === $Failed, Return[$Failed]];
          
          commentInfo=facebookimport[commentInfo];
          If[commentInfo === $Failed, Return[$Failed]];
          commentInfo="comment_info"/.commentInfo["data"];
          
          
          totalcomment = Total["comment_count" /. commentInfo];
          messagedata = {
          	gdata[#,{"likes","summary","total_count"},{}]/.{}->0,
            gdata[#,{"message"},""]} &/@wall;
            
          postlength = Length[messagedata];

          If[postlength > 0,

             totallike = Total[messagedata[[All, 1]]];
             avpostwords = Mean[StringCount[#, WordCharacter ..] & /@ messagedata[[All, 2]]];
             avpostcharacter = Mean[StringLength /@ messagedata[[All, 2]]],

             {totallike, totalcomment, avpostwords, avpostcharacter} = {0, 0, 0, 0}
          ];

          {"AnalyzedPosts" -> postlength, "TotalLikes" -> totallike, "TotalComments" -> totalcomment,
           "AveragePostLength" -> {"Words" -> avpostwords, "Characters" -> avpostcharacter}}

        ) /; ( wall =!= $Failed )
]*)

facebookcookeddata["WallPostStatisticsGrid",id_,args_]:=Module[
	{rawdata,plength, tlike, tcomments, avplength,avlike,avcomment,tlikestring,tcommentstring,
	avwordsstring,avcharstring},
	rawdata=facebookcookeddata["WallPostStatistics",id,args];
	If[rawdata==={},Return[{}]];
	
	If[rawdata === $Canceled, Return[$Canceled]];

        If[Head[rawdata] === Missing, Return[rawdata]];
        If[rawdata === {}, Return[{}]];

        (
         {plength, tlike, tcomments, avplength} =
         {"AnalyzedPosts", "TotalLikes", "TotalComments", "AveragePostLength"} /. rawdata;

         If[plength>0,
            avlike = tlike / plength; avcomment = tcomments / plength,
            avlike = 0; avcomment = 0;
         ];

         tlikestring =
          ToString[tlike] <>
           ToString[Style[StringJoin[{" (average: ", ToString[NumberForm[N@avlike, {Infinity, 3}], StandardForm], " per post)"}], Gray], StandardForm];

         tcommentstring =
          ToString[tcomments] <>
           ToString[Style[StringJoin[{" (average: ", ToString[NumberForm[N@avcomment, {Infinity, 3}], StandardForm], " per post)"}], Gray], StandardForm];

         avwordsstring = StringJoin[{ToString[NumberForm[N["Words" /. avplength], {Infinity, 3}], StandardForm], " words"}];
         avcharstring = StringJoin[{ToString[NumberForm[N["Characters" /. avplength], {Infinity, 3}], StandardForm], " characters"}];

         OAuthClient`Private`prettygrid[
            {{"Facebook Wall Statistics",SpanFromLeft},
             {"analyzed posts", ToString[plength]},
             {"total likes", tlikestring},
             {"total comments", tcommentstring},
             {"average post length", Row[{avwordsstring, avcharstring}, Style["\[VerticalSeparator]", Gray]]}
            },
            Frame -> All, Alignment -> Left, FrameStyle -> Gray, Spacings -> {1, 1}
        ]

        ) /; (rawdata =!= $Failed)
]

facebookcookeddata["WallMostCommentedPostData",id_,args_]:=Module[
	{wall,ncomments,max,pos},
	wall=facebookwalldata[id,args];
	If[wall==={},Return[{}]];
	(
		ncomments=gdata[#,{"comments","summary","total_count"},{}]&/@wall/.{}->0;
		max=Max[ncomments];
		pos=Flatten[Position[ncomments,max]];
		formatposts[Join[#,{"CommentCount"->max}]&/@wall[[pos]]]
	) /; ( wall =!= $Failed )
]

facebookcookeddata["WallMostCommentedPost",id_,args_]:=Module[
	{most,title, data},
	most=facebookcookeddata["WallMostCommentedPostData",id,args];
	If[most==={},Return[{}]];
	data=({"Message", "Picture", "Name", "CreatedTime","CommentCount", "Story"} /. (Normal/@most));
	(
         Column[
                With[{
                    grid = {{If[#2 =!= "Picture", Import[#2], ""], If[#3 =!= "Name", #3, ""]}},
                    message = If[#1 =!= "Message", #1, ""], story = If[#6 =!= "Story", #6, ""]},
                    If[message === "", title = story, title = message];
                    Column[{title,
                        If[grid =!= {{"",""}},Grid[grid, Frame -> True],""],
                            Style[StringJoin[{"(\[TildeTilde] ", iDateDifference[#4]," | ", ToString[#5], " comments)"}],
                            Gray]}
                    ]] & @@@ data,
                Dividers -> Center, FrameStyle -> Opacity[.3], Spacings -> 1.5
         ]

        ) /; (most =!= $Failed)
]

facebookcookeddata["WallMostLikedPostData",id_,args_]:=Module[
	{wall,nlikes,max,pos},
	wall=facebookwalldata[id,args];
	If[wall==={},Return[{}]];
	(
		nlikes=gdata[#,{"likes","summary","total_count"},{}]&/@wall/.{}->0;
		max=Max[nlikes];
		pos=Flatten[Position[nlikes,max]];
		formatposts[Join[#,{"LikeCount"->max}]&/@wall[[pos]]]
	) /; ( wall =!= $Failed )
]

facebookcookeddata["WallMostLikedPost",id_,args_]:=Module[
	{most,title, data},
	most=facebookcookeddata["WallMostLikedPostData",id,args];
	If[most==={},Return[{}]];
	data=({"Message", "Picture", "Name", "CreatedTime","LikeCount", "Story"} /. (Normal/@most));    
	(
         Column[
                With[{
                    grid = {{If[#2 =!= "Picture", Import[#2], ""], If[#3 =!= "Name", #3, ""]}},
                    message = If[#1 =!= "Message", #1, ""], story = If[#6 =!= "Story", #6, ""]},
                    If[message === "", title = story, title = message];
                    Column[{title,
                        If[grid =!= {{"",""}},Grid[grid, Frame -> True],""],
                            Style[StringJoin[{"(\[TildeTilde] ", iDateDifference[#4]," | ", ToString[#5], " likes)"}],
                            Gray]}
                    ]] & @@@ data,
                Dividers -> Center, FrameStyle -> Opacity[.3], Spacings -> 1.5
         ]

        ) /; (most =!= $Failed)
]

facebookcookeddata["WallPostLength",id_,args_]:=Module[
	{wall,messagedata,char,gather,res},
	wall=facebookwalldata[id,args];
	If[wall==={},Return[{}]];
	(
  		messagedata =
    		({"created_time", "message"}/.wall)/."message"->"";
    	messagedata =	
            {readdate[#1,;;2], #2} & @@@ messagedata;
  		char = StringLength /@ messagedata[[All, 2]];
  		gather = GatherBy[Transpose[{messagedata[[All, 1]], char}], First];

  		res = Rule @@@ Transpose[{gather[[All, 1, 1]], Total[gather[[All, All, 2]], {2}]}];
  		res /; ListQ[res]

        ) /; ( wall =!= $Failed )
]

facebookcookeddata["WallPostLengthTimeline",id_,args_]:=Module[
	{rawdata,messagedata,res},
	rawdata=facebookcookeddata["WallPostLength",id,args];
	If[rawdata==={},Return[{}]];
	(
         messagedata = List@@@rawdata;
         res=
          DateListPlot[
            messagedata,
            Filling -> Bottom, FillingStyle -> Blue,
            GridLines -> {None, Automatic}, GridLinesStyle -> Opacity[.4],
            PlotRange -> {Automatic, {0, Automatic}},
            DateTicksFormat -> {"Month", "/", "Year"},
            FrameTicks -> {{Automatic, None}, {Automatic, None}}
          ];

         res /; (Head[res] =!= DateListPlot)

        ) /; (rawdata =!= $Failed)
]

(*Removed because RawQuery has been removed too.*)
(*facebookcookeddata["WallTopCommenter",id_,args_]:=Module[
	{wall,postid,pstring,query,commentInfo,count,countlimit,commenter,topcomment,commentername,
	names, uids},
	wall=facebookwalldata[id,args];
	If[wall==={},Return[{}]];
	(
         
          postid = "id" /. wall;
          pstring = ToString[StringJoin[Riffle[ToString[#, InputForm] & /@ postid, ","]]];
          query="SELECT post_id, comment_info FROM stream WHERE post_id IN ("<>pstring<>") LIMIT 5000";
          commentInfo = OAuthClient`rawoauthdata[id,"RawQuery","q"->query];
                   
          If[commentInfo === $Canceled, Return[$Canceled]];
          If[commentInfo === $Failed, Return[$Failed]];
          
          commentInfo=facebookimport[commentInfo];
          If[commentInfo === $Failed, Return[$Failed]];
          commentInfo="comment_info"/.commentInfo["data"];

          count = Total["comment_count" /. commentInfo];
          countlimit = If[NumericQ[DataPaclets`Private`$FacebookCommenterLimit],
              DataPaclets`Private`$FacebookCommenterLimit, 2000];

          If[count > countlimit, Return[Missing["NotApplicable"]]];
		  query="SELECT post_id,fromid,text FROM comment WHERE post_id IN ("<>pstring<>") LIMIT 5000";
          commenter = OAuthClient`rawoauthdata[id,"RawQuery","q"->query];
         (* Work around for JSON import bug  255691
         commenter=facebookimport[commenter];
          If[MatchQ[commenter, {}|$Canceled|$Failed], Return[commenter]];
          
          topcomment=gdata[commenter["data"],{"fromid"},{}];
          *)
          
          topcomment=StringCases[commenter, "\"fromid\":" ~~ (digit : (DigitCharacter ..)) ~~ "," :> digit];
 		(* End workaround *)
          query="SELECT uid,name FROM user WHERE uid IN ("<>StringJoin[Riffle[ToString/@Union[topcomment], ","]]<>") LIMIT 5000";
          commentername = OAuthClient`rawoauthdata[id,"RawQuery","q"->query];
	(* Work around for JSON import bug  255691
          commentername=facebookimport[commentername];
          If[MatchQ[commentername, {}|$Canceled|$Failed], Return[commentername]];
          commentername=(ToString[#1]->#2)&@@@(Union[{"uid","name"} /. commentername["data"]]);
          *)
          names=StringCases[commentername, "\"name\":" ~~ (Shortest[na___]) ~~ ("," | "}") :> na];
          uids=StringCases[commentername, "\"uid\":" ~~ (uu : (DigitCharacter ..)) ~~ ("," | "}") :> uu];
          names=Union[Transpose[{uids,names}]];
          If[MatchQ[commentername, {}|$Canceled|$Failed], Return[commentername]];
          commentername=(ToString[#1]->#2)&@@@(names);
 		(* End workaround *)
          (

           topcomment = Tally[topcomment /. commentername];
           Rule@@@Reverse[SortBy[topcomment, Last]]

         ) /; (commenter =!= $Failed)

        ) /; ( wall =!= $Failed )
]*)

facebookcookeddata["WallWeeklyAppActivity",id_,args_]:=Module[
	{wall,weeklydata,gatherByActWeeckly,weeklyitems,dateweekly,
		datelistweekly,rulesweekly,dayrules,weeklyPart,
		bubbledata,itemdata,timerule,chartelem,res},
	wall=facebookwalldata[id,args];
	If[wall==={},Return[{}]];
	(
         weeklydata =
         {StringSplit[DateString[readdate[#1],{"Hour24", " ", "DayNameShort"}]], ("name" /. #2)} & @@@ DeleteCases[({"created_time", "application"} /. wall), {_, "application"}];

         gatherByActWeeckly = GatherBy[weeklydata, Last];
         weeklyitems = gatherByActWeeckly[[All, 1, 2]];

         dateweekly = gatherByActWeeckly[[All, All, 1]];
         rulesweekly = #1 -> #2 & @@@ Tally[#] & /@ dateweekly[[All, All, ;; 2]];
         datelistweekly = Union[weeklydata[[All, 1]]];

         dayrules = Thread[{"Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"} -> Range[7]];
         weeklyPart = datelistweekly /. rulesweekly /. {_String, _} -> 0;

         If[weeklydata =!= {},
             bubbledata = Transpose[Join[Transpose[ToExpression[datelistweekly /. dayrules]], {Total[weeklyPart, {1}]}]];
             itemdata = Transpose[weeklyPart],
             bubbledata = {};
             itemdata = {}
         ];
         
         dayrules=Reverse[Rule @@@ dayrules, 2];
         timerule=Table[i -> DateString[{1, 1, 1, i}, {"Hour12Short", " ", "AMPMLowerCase"}], {i, 0, 21, 3}];

         If[bubbledata === {},
             Return[
                 BubbleChart[{}, BubbleSizes -> {0.06, 0.14}, AspectRatio -> .4,
                 FrameTicks -> {{List @@@ dayrules, None}, {List @@@ timerule, None}},
                 PlotRange -> {{-2, 23}, {.5, 7.5}}]]
         ];

         chartelem = Table[PieChart[d,LabelingFunction->None, ChartStyle->facebookChartColor], {d, itemdata}];
         res =
          BubbleChart[bubbledata,
             ChartElements -> chartelem,
             BubbleSizes -> {0.06, 0.14},
             AspectRatio -> .4,
            ChartLegends -> {None, weeklyitems},
            FrameTicks -> {{List@@@dayrules, None}, {List@@@timerule, None}}
          ];

         Legended[res, SwatchLegend[facebookChartColor, weeklyitems]] /; (Head[res] =!= BubbleChart)

        
        ) /; ( wall =!= $Failed )
]

facebookcookeddata["WallWordFrequencies",id_,args_]:=Module[
	{wall,message},
	wall=facebookwalldata[id,args];
	If[wall==={},Return[{}]];
	(
      message = "message" /. wall /. "message" -> "";
      Rule@@@Reverse[SortBy[Tally[StringCases[StringJoin[Riffle[message, " "]], WordCharacter ..]], Last]]

    ) /; ( wall =!= $Failed )
]

(*************************** Networks *****************************)
$facebooknetworklimit=1000;

(*Removed because RawQuery has been removed too.*)
(*facebookcookeddata[prop:("FriendNetwork"), id_, args_] :=
    Block[{res, friends, ids, names, pics, edges, urls, vertices, rawdata,queries,OAuthClient`$CacheResults=True},
    	rawdata=OAuthClient`rawoauthdata[id,"RawFriendsList",Join[args,{"limit"->ToString[$facebooknetworklimit],"fields"->"name,id,picture"}]];     
		friends=facebookimport[rawdata];
   		If[MatchQ[friends,$Canceled|$Failed], Return[friends]];
   		friends=friends["data"];
		(
           ids = ToString /@ ("id" /. friends);
           names = "name" /. friends;
           pics = gdata[#,{"picture","data","url"},{}]&/@friends;

           If[Length[ids] == 0, Return[Graph[{}]]];
           (
          	queries=friendshipinterconnectionsqueries[ids];
            rawdata=OAuthClient`rawoauthdata[id,"RawQuery","q"->#]&/@queries;

            (
              edges = Union[Flatten[parseFacebookData[#, "mutualfql"] &/@ Flatten[rawdata]]];
              (
               vertices =
                 Property[#1, {"Name" -> #2, "Picture"-> #3,VertexLabels->Placed[#2,Tooltip]}] & @@@ Transpose[{ids, names, pics}];
               res = Graph[vertices, edges, PerformanceGoal -> "Speed"];
               res /;GraphQ[res]

              ) /; !MemberQ[edges, $Failed]

            ) /; FreeQ[rawdata,$Failed]

           ) /; (Length[ids] == Length[names]==Length[pics])

         ) /; (friends =!= $Failed)
    ]*)


rawsearchposts[id_,pids_,type_String]:=With[{pidstr=StringJoin[Riffle[ToString[#, InputForm] & /@ pids, ","]],
	fields=Switch[type,
		"like","post_id,user_id",
		"comment","post_id,fromid"
	]},
	OAuthClient`rawoauthdata[id,"RawQuery","q"->"SELECT "<>fields<>" FROM "<>type<>" WHERE post_id IN ("<>pidstr<>") LIMIT 5000"]
]

networkprops=("LikeCommentNetwork"|"CommentNetwork"|"LikeNetwork"|
	"PostLikeCommentNetwork"|"PostCommentNetwork"|"PostLikeNetwork"|
	"BimodalLikeCommentNetwork"|"BimodalCommentNetwork"|"BimodalLikeNetwork")

facebookcookeddata[prop:networkprops,id_,args_] :=
    Block[{res, upost, pids, commenter, liker, cTable, lTable, data, vertices={}, edges,queries,
         ilikes={},icomments={},ilikespost={},icommentspost={},likedata,commenterdata,vproperty,uids,fids,params, 
         cQ, lQ,pQ,uQ,types={},posttypes={},pIDTypes,
         OAuthClient`$CacheResults=True},
		cQ=MatchQ[prop,"LikeCommentNetwork"|"CommentNetwork"|"PostLikeCommentNetwork"|"PostCommentNetwork"|
			"BimodalLikeCommentNetwork"|"BimodalCommentNetwork"];
		lQ=MatchQ[prop,"LikeCommentNetwork"|"LikeNetwork"|"PostLikeCommentNetwork"|"PostLikeNetwork"|
			"BimodalLikeNetwork"|"BimodalLikeCommentNetwork"];
		pQ=MatchQ[prop,"PostLikeCommentNetwork"|"PostLikeNetwork"|"PostCommentNetwork"|
			"BimodalLikeCommentNetwork"|"BimodalCommentNetwork"|"BimodalLikeNetwork"];
		uQ=MatchQ[prop,"LikeCommentNetwork"|"LikeNetwork"|"CommentNetwork"|
			"BimodalLikeCommentNetwork"|"BimodalCommentNetwork"|"BimodalLikeNetwork"];
		params=filterparameters[args,getallparameters["RawGetPosts"]];
		params=params/.HoldPattern[Rule[a_,b_Integer]]:>Rule[a,ToString[b]];
		 upost=OAuthClient`rawoauthdata[id,"RawGetPosts",Join[params,{"fields"->"id","limit"->ToString[$facebooknetworklimit]}]];
         If[upost === $Canceled, Return[$Canceled]];
		 upost=facebookimport[upost];
         If[upost === $Failed, Return[$Failed]];

         If[Head[upost] === Missing, Return[upost]];
         If[upost === {}, Return[Graph[{}]]];

         (
          	pIDTypes = ("id" -> "type") /. upost;
          	pids="id"/.Lookup[upost,"data",{}];
			If[cQ,
				commenter=rawsearchposts[id, pids,"comment"];
	          	If[MatchQ[commenter,$Canceled|$Failed], Return[commenter]];
	          	(* bug 255691 workaround *)fids=StringCases[commenter, "\"fromid\":" ~~ (ff : (DigitCharacter ..)) ~~ ("," | "}") :> ff];
	          	commenter=facebookimport[commenter]["data"];
	          	If[MatchQ[commenter,$Canceled|$Failed], Return[commenter]];
            	cTable = {"post_id", "fromid"} /. commenter /. {"post_id", "fromid"} -> {};
          		(* bug 255691 workaround *)cTable[[All,2]]=fids,
	          	cTable={}
			];
			If[lQ,
				liker=rawsearchposts[id, pids,"like"];
	          	If[MatchQ[liker,$Canceled|$Failed], Return[liker]];
	          	(* bug 255691 workaround *)uids=StringCases[liker, "\"user_id\":" ~~ (ll : (DigitCharacter ..)) ~~ ("," | "}") :> ll];
	          	liker=facebookimport[liker]["data"];
	          	If[MatchQ[liker,$Canceled|$Failed], Return[liker]];
            	lTable = {"post_id", "user_id"} /. liker /. {"post_id", "user_id"} -> {};
          		(* bug 255691 workaround *)lTable[[All,2]]=uids,
	          	lTable={}
			];          	

            data = {#1, ToString[#2]} & @@@ Union[cTable, lTable];

			If[pQ,
				(* Post Vertex Networks *)
				ilikes = (#[[1, 1]] -> {"Liker" -> ToString /@ #[[All, 2]]}) & /@ GatherBy[lTable, First];
	            icomments = (#[[1, 1]] -> {"Commenter" -> ToString /@ #[[All, 2]]}) & /@ GatherBy[cTable, First];
	            posttypes = MapThread[#1 -> {"Type" -> #2} &, {data[[All,1]], data[[All,1]] /. pIDTypes}];
	            vertices = data[[All, 1]];
	            If[vertices === {}, Return[Graph[{}]]];
	            
            	edges = Union[Flatten[uuLEdges[#[[All, 1]]] & /@ GatherBy[data, #[[2]] &]]];
            	vproperty={};
			];
			If[uQ,
				(* User Vertex Networks *)
	            ilikespost = (ToString[#[[1, 2]]] -> {"Likes" -> #[[All, 1]]}) & /@ GatherBy[lTable, Last];
	            icommentspost = (ToString[#[[1, 2]]] -> {"Comments" -> #[[All, 1]]}) & /@ GatherBy[cTable, Last];
	            vertices = Join[vertices,data[[All, 2]]];
	            If[vertices === {}, Return[Graph[{}]]];
				types = (# -> {"Type" -> "user"}) & /@ data[[All, 2]];
          	
	          	If[lQ,
		          	queries=userdataqueries[ToString/@lTable[[All, 2]]];
		            likedata=(OAuthClient`rawoauthdata[id,"RawQuery","q"->#]&/@queries),
		            likedata={}
	          	];
	            
	            If[cQ,
		          	queries=userdataqueries[ToString/@cTable[[All, 2]]];
		            commenterdata=(OAuthClient`rawoauthdata[id,"RawQuery","q"->#]&/@queries),
		            commenterdata={}
	            ];
	            (* bug 255691 workaround *)uids=Union@Flatten[StringCases[#, "\"uid\":" ~~ (uuu : (DigitCharacter ..)) ~~ ("," | "}") :> uuu]&/@Join[likedata,commenterdata]];
	          
	            If[likedata === $Failed, likedata = {},likedata=Join@@((facebookimport[#]["data"])&/@likedata)];
	            If[commenterdata === $Failed, commenterdata = {},commenterdata=Join@@((facebookimport[#]["data"])&/@commenterdata)];
	            vproperty = Union[likedata, commenterdata];
	            If[vproperty =!= {},
	            	vproperty=({"uid","name","pic"} /. vproperty);
	            	(* bug 255691 workaround *)vproperty[[All,1]]=uids;
	                vproperty = (ToString[#1]->{"Name"->#2, "Picture"->#3,VertexLabels->Placed[#2,Tooltip]})&@@@vproperty
	            ];
	            
	            If[pQ,
	            	(* Bimodal networks *)
	            	edges = UndirectedEdge @@@ data,
	            	edges = Union[Flatten[uuLEdges[#[[All, 2]]] & /@ GatherBy[data, First]]]
	            ];
	            types={};
	            
			];
            res = Graph[vertices, edges, Properties -> Join[ilikes,icomments,ilikespost,icommentspost,vproperty,types,posttypes], PerformanceGoal -> "Speed"];
            res /; GraphQ[res]

         ) /; (upost =!= $Failed)
    ]




userdataqueries[uids_,opts___]:=Block[{users1,res},
         users1 = Partition[uids, 50, 50, 1, {}];
         res=StringReplace["SELECT uid,name,pic_square,pic FROM user WHERE uid IN (`ids`) LIMIT 5000"
         	, {"`ids`" -> StringJoin[Riffle[#, ","]]}] & /@ users1;
         res /; ListQ[res]
    ]



friendshipinterconnectionsqueries[fids_, opts___] :=
    Block[{token, res, query, friends, friend1},
         friend1 = Partition[fids, 50, 50, 1, {}];
         friends = ToString[StringJoin[Riffle[fids, ","]]];
         res=StringReplace["SELECT uid1, uid2 FROM friend WHERE uid1 > uid2 AND uid1 IN
                     (`friend1`) AND uid2 IN (`friends`)", {"`friend1`" -> StringJoin[Riffle[#, ","]],"`friends`" -> friends}] & /@ friend1;
         res /; ListQ[res]
    ]
    
    
uuLEdges[{}] := {}
uuLEdges[{{id_String, _}}] := {}

uuLEdges[users_] :=
 Block[{ids, ls},
  ids = users;
  ls = Length[ids];
  If[ls > 1,
   Table[Sort[ids[[i]] \[UndirectedEdge] ids[[j]]], {i, ls - 1}, {j, i + 1, ls}],
   {}
   ]
  ]


facebookcookeddata[___]:=$Failed

(* Send Message *)
facebooksendmessage[id_,message_String]:=facebookcookeddata["PostMessage",id,"message"->message]

facebooksendmessage[___]:=$Failed
(******** Permission management **********)
facebookcheckpermissions[id_]:=With[{res=facebookimport[OAuthClient`rawoauthdata[id,"RawPermissions"]]},
	If[KeyExistsQ[res,"data"],
		Flatten[checkFacebookPermissions[res["data"]]],
		{}
	]
]

$FacebookPermissionsURL="https://www.wolframcloud.com/objects/user-fa95220f-871c-4331-84ab-7951dd0666ca/facebookkey"

getfacebookkey[]:=ToExpression[URLFetch[$FacebookPermissionsURL,"VerifyPeer"->False]]/;OAuthClient`Private`$OAuthCloudCredentialsQ
getfacebookkey[]:=OAuthClient`Private`getclientinfo["Facebook"][[1]]

requestedFacebookPermissions[_]:={};

facebookaddpermissions[id_,permissions_]:=facebookaddpermissions0[id,Complement[permissions,requestedFacebookPermissions[id]]]

facebookaddpermissions0[id_,{}]:=Null

facebookaddpermissions0[id_,permissions_]:=Module[{
	res,url, key=getfacebookkey[]
	},
	requestedFacebookPermissions[id]=Join[requestedFacebookPermissions[id],permissions];
	If[!StringQ[key],Throw[$Failed]];
	url="https://www.facebook.com/dialog/oauth?client_id="<>
        	(key)<>"&redirect_uri="<>
        	("RedirectURI"/.facebookdata[])<>"&scope="<>
        	StringJoin[Riffle[permissions,","]];
	res=OAuthClient`tokenOAuthDialog[url,"Add Permissions"];
	res
]


(****** utilities ************)
gdata=OAuthClient`Private`getdata;
fval=OAuthClient`Private`formatvalue;
filterparameters=OAuthClient`Private`filterParameters;
camelcase=OAuthClient`Private`camelCase;

formatuser[]:="me"
formatuser[Automatic]:="me"
formatuser[str_String]:=str
formatuser[x_]:=ToString[x]
formatuser[__]:=Throw[$Failed]

formatpage[str_String]:=str
formatpage[x_]:=ToString[x]
formatpage[__]:=Throw[$Failed]

getallparameters[str_]:=DeleteCases[Flatten[{"Parameters","PathParameters","BodyData","MultipartData"}/.facebookdata[str]],
	("Parameters"|"PathParameters"|"BodyData"|"MultipartData")]

$loadentitydata=False;
	                             
userdataparse[x_,"location"|"hometown"]:=If[$loadentitydata,WolframAlpha["name"/.x, "MathematicaResult"]/.Missing[___]:>("name"/.x),"name"/.x]
userdataparse[x_,"installed"]:=False/;!TrueQ[x]
userdataparse[x:Missing[___],_]:=x
userdataparse[x_,"id"]:=ToExpression[x]
userdataparse[x_,"birthday"]:=DateObject[DateString[x]]
userdataparse[x_,"cover"]:=Hyperlink["source"/.x]
userdataparse[x_,"link"]:=Hyperlink[x]
userdataparse[x_,"currency"]:="user_currency"/.x
userdataparse[x_,"education"]:="name" /. ("school" /. x)
userdataparse[x_,"favorite_athletes"|"favorite_teams"|"hometown"|"languages"|"location"|"significant_other"]:="name"/.x
userdataparse[x_,"picture"]:=Hyperlink["url" /. ("data" /. x)]
userdataparse[x_,"updated_time"]:=readdate[x]
userdataparse[x_,"work"]:=		
		(Cases[x, HoldPattern[Rule]["employer" | "position" | "description", _], 2] /. {___, "name" -> z_, ___} :> z) /. {"employer" -> "Employer", "position" -> "Position", "description" -> "Decription"}
userdataparse[x_,_]:=x

pagedataparse[x:Missing[___],_]:=x
pagedataparse[x_,"hometown"|"current_location"|"location"]:=If[$loadentitydata,WolframAlpha[x, "MathematicaResult"]/.Missing[___]:>(x),x]
pagedataparse[x_,"price_range"]:=Quantity[#, "Dollars"] & /@ ToExpression[StringCases[x, DigitCharacter ..]]/;StringMatchQ[x, "$*"]
pagedataparse[x_,"parking"|"restaurant_services"|"restaurant_specialties"]:=camelcase/@Cases[x,HoldPattern[Rule[p_,1|"1"]]:>p]
pagedataparse[x_,"category_list"]:=gdata[x, "name",{}]
pagedataparse[x_,"cover"]:=Hyperlink["source"/.x]
pagedataparse[x_,"best_page"]:=ToExpression[gdata[x, "id",{}]]
pagedataparse[x_,"id"]:=ToExpression[x]
pagedataparse[x_,_]:=x

readdate[date_, part_:All]:=TimeZoneConvert[
	DateObject[DateList[{StringDrop[date, -5], 
		{"Year", "-", "Month", "-", "Day", "T", "Hour", ":", "Minute", ":", "Second"}}][[part]], TimeZone -> 0], $TimeZone]

fixlatlong[list_List,latname_:"latitude",lngname_:"longitude"]:=With[{lat=latname/.list,lng=lngname/.list},
	If[!(NumberQ[lat]&&NumberQ[lng]),list,
		Join[{"GeoPosition"->GeoPosition[{lat,lng}]},FilterRules[list,Except[latname|lngname]]]
	]
]

formatposts[posts_]:=(Association[Replace[#,HoldPattern[Rule][a_String,b_]:>Rule[camelcase[a],b],Infinity]/.{"ID"->"PostID",fval["UpdatedTime"->readdate],fval["CreatedTime"->readdate]}]&/@posts)

iImportImage[""] := Missing["NotAvailable"]

iImportImage[url_String] :=
    Block[{res},
        res = Quiet[Import[url]];
        If[ImageQ[res],
            res,
            If[StringMatchQ[url, __ ~~ "_" ~~ _ ~~ ".jpg"],
                res = StringReplacePart[url, "q", {-5, -5}];
                res = Quiet[Import[res]];
                If[ImageQ[res], res, Missing["NotAvailable"]]
                ,
                Missing["NotAvailable"]
            ]
        ]
    ];
    
iDateDifference[date_] :=
    Block[{old, now, diff},
        old = DateList[date];
        now = DateList[];

        Which[
            diff = Round[First[DateDifference[old, now, "Year"]]];
            diff >= 2,
            {ToString[diff], " years ago"},

            diff = Round[First[DateDifference[old, now, "Month"]]];
            diff >= 2,
            {ToString[diff], " months"},

            diff = Round[First[DateDifference[old, now, "Day"]]];
            True,
            {ToString[diff], " days ago"}
        ]
    ]    

iDateDifference[___] := Missing["NotAvailable"]


parseFacebookData[expr_, "mutualfql"] :=
    StringCases[expr, RegularExpression["(?ms)\"uid1\":\\s*\"(\\d+)\",\"uid2\":\\s*\"(\\d+)\""] :>
           UndirectedEdge["$1", "$2"]]
 
fbicon=Image[RawArray["Byte", {{{59, 87, 157, 1}, {59, 87, 157, 115}, {59, 87, 157, 243}, {59, 87, 157, 255}, 
  {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, 
  {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, 
  {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, 
  {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, 
  {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 243}, {59, 87, 157, 115}, {59, 87, 157, 0}}, 
  {{59, 87, 157, 115}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, 
  {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, 
  {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, 
  {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, 
  {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, 
  {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 115}}, {{59, 87, 157, 
  244}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 
  255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 
  255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 
  255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 
  255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 
  255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 244}}, {{59, 87, 157, 255}, {59, 87, 
  157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 
  157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 
  157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 
  157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 
  157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 
  157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}}, {{59, 87, 157, 255}, {59, 87, 157, 255}, {59, 
  87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 
  87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 
  87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 
  87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 
  87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 
  87, 157, 255}, {59, 87, 157, 255}}, {{59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, 
  {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, 
  {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, 
  {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {70, 96, 163, 255}, 
  {151, 166, 203, 255}, {216, 222, 235, 255}, {248, 249, 251, 255}, {255, 255, 255, 255}, {246, 247, 
  250, 255}, {230, 234, 243, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 
  87, 157, 255}, {59, 87, 157, 255}}, {{59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, 
  {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, 
  {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, 
  {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {95, 119, 176, 255}, {230, 234, 243, 
  255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 
  255, 255, 255}, {255, 255, 255, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, 
  {59, 87, 157, 255}, {59, 87, 157, 255}}, {{59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 
  255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 
  255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 
  255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {71, 97, 163, 255}, {236, 239, 246, 255}, {255, 255, 
  255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, 
  {255, 255, 255, 255}, {255, 255, 255, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 
  255}, {59, 87, 157, 255}, {59, 87, 157, 255}}, {{59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 
  157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 
  157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 
  157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {160, 173, 208, 255}, {255, 255, 255, 255}, {255, 
  255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 
  255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 
  157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}}, {{59, 87, 157, 255}, {59, 87, 157, 255}, {59, 
  87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 
  87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 
  87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {223, 228, 239, 255}, {255, 255, 255, 255}, 
  {255, 255, 255, 255}, {255, 255, 255, 255}, {174, 186, 216, 255}, {71, 97, 163, 255}, {59, 87, 157, 
  255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 
  255}, {59, 87, 157, 255}, {59, 87, 157, 255}}, {{59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 
  157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 
  157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 
  157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {252, 252, 253, 255}, {255, 255, 255, 255}, {255, 
  255, 255, 255}, {255, 255, 255, 255}, {77, 103, 167, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, 
  {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, 
  {59, 87, 157, 255}, {59, 87, 157, 255}}, {{59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 
  255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 
  255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 
  255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 
  255, 255}, {255, 255, 255, 255}, {60, 88, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 
  87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 
  87, 157, 255}, {59, 87, 157, 255}}, {{59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, 
  {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, 
  {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, 
  {59, 87, 157, 255}, {59, 87, 157, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 
  255}, {255, 255, 255, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 
  157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 
  157, 255}, {59, 87, 157, 255}}, {{59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 
  87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 
  87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 
  87, 157, 255}, {59, 87, 157, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, 
  {255, 255, 255, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 
  255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 
  255}, {59, 87, 157, 255}}, {{59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 
  157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 
  157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 
  255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 
  255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 
  255, 255, 255}, {250, 251, 253, 255}, {62, 90, 159, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, 
  {59, 87, 157, 255}, {59, 87, 157, 255}}, {{59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 
  255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 
  255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {255, 255, 255, 255}, {255, 255, 
  255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, 
  {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 
  255, 255}, {255, 255, 255, 255}, {220, 226, 238, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 
  87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}}, {{59, 87, 157, 255}, {59, 87, 157, 255}, 
  {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, 
  {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {255, 255, 255, 
  255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 
  255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 
  255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {188, 197, 222, 255}, {59, 87, 157, 255}, {59, 87, 
  157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}}, {{59, 87, 157, 255}, {59, 
  87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 
  87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {255, 
  255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 
  255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 
  255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {156, 170, 206, 255}, {59, 87, 157, 
  255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}}, {{59, 87, 
  157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 
  157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 
  157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {255, 
  255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {59, 87, 157, 
  255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 
  255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}}, {{59, 87, 
  157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 
  157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 
  157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {255, 
  255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {59, 87, 157, 
  255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 
  255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}}, {{59, 87, 
  157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 
  157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 
  157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {255, 
  255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {59, 87, 157, 
  255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 
  255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}}, {{59, 87, 
  157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 
  157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 
  157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {255, 
  255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {59, 87, 157, 
  255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 
  255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}}, {{59, 87, 
  157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 
  157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 
  157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {255, 
  255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {59, 87, 157, 
  255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 
  255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}}, {{59, 87, 
  157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 
  157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 
  157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {255, 
  255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {59, 87, 157, 
  255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 
  255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}}, {{59, 87, 
  157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 
  157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 
  157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {255, 
  255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {59, 87, 157, 
  255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 
  255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}}, {{59, 87, 
  157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 
  157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 
  157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {255, 
  255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {59, 87, 157, 
  255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 
  255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}}, {{59, 87, 
  157, 244}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 
  157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 
  157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {255, 
  255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {59, 87, 157, 
  255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 
  255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 244}}, {{59, 87, 
  157, 123}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 
  157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 
  157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {255, 
  255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {59, 87, 157, 
  255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 
  255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 123}}, {{59, 87, 
  157, 1}, {59, 87, 157, 115}, {59, 87, 157, 244}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 
  157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 
  157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {255, 
  255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {59, 87, 157, 
  255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 255}, {59, 87, 157, 
  255}, {59, 87, 157, 255}, {59, 87, 157, 244}, {59, 87, 157, 115}, {59, 87, 157, 0}}}], "Byte", 
 ColorSpace -> "RGB", Interleaving -> True];
           		
End[] (* End Private Context *)
           		
End[]

SetAttributes[{},{ReadProtected, Protected}];

System`Private`RestoreContextPath[];

(* Return five functions to define oauthservicedata, oauthcookeddata, oauthsendmessage, checkpermission, add permissions  *)
{FacebookOAuth`Private`facebookdata,FacebookOAuth`Private`facebookcookeddata,
	FacebookOAuth`Private`facebooksendmessage,FacebookOAuth`Private`facebookcheckpermissions,
	FacebookOAuth`Private`facebookaddpermissions}
