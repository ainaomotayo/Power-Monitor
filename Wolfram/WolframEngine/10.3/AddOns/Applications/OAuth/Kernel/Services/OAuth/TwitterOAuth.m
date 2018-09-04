
System`Private`NewContextPath[{"OAuthClient`","System`"}];

Begin["TwitterOAuth`"] (* Begin Private Context *) 

Begin["`Private`"](* Begin Private Context *) 

(******************************* Twitter *************************************)

(* Authentication information *)

twitterdata[]={
		"OAuthVersion"		->"1.0a",
		"ServiceName" 		-> "Twitter", 
		"RequestEndpoint" 	-> "https://api.twitter.com/oauth/request_token",
	  	"AccessEndpoint" 	-> "https://api.twitter.com/oauth/access_token", 
	 	"AuthorizeEndpoint" -> "https://api.twitter.com/oauth/authorize", 
	 	"ClientInfo"		-> {"Wolfram","Token"},
	 	"AuthenticationDialog" :> (OAuthClient`tokenOAuthDialog[#, "Twitter",bird]&),
	 	"Gets"				-> {"GetTweet",OAuthClient`Private`gridRequests["TweetGrid"],"LastTweet","FollowerIDs","FriendIDs","UserData","UserMentions","UserReplies","FollowerMentionNetwork",
	 		"FriendMentionNetwork","FollowerReplyToNetwork","FriendReplyToNetwork","FriendNetwork","FollowerNetwork","Friends","Followers",
	 		"RateLimit","UserIDSearch","SearchNetwork","SearchReplyToNetwork","SearchMentionNetwork","UserHashtags","TweetSearch",
	 		"TweetEventSeries","TweetTimeline","TweetList"},
	 	"Posts"				-> {"Tweet","ImageTweet"},
	 	"RawGets"			-> {"RawMentionsTimeline","RawUserTimeline","RawHomeTimeline","RawRetweetTimeline",
	 				"RawStatus","RawRetweets","RawRetweeterIDs","RawTweetSearch",
	 				"RawDirectMessages","RawDirectMessagesSent","RawDirectMessage",
	 				"RawNoRetweetUserIDs", "RawFriendIDs", "RawFollowerIDs", 
					"RawMyFriendship", "RawIncomingFriendships", 
					"RawOutgoingFriendships", "RawFriendship", "RawFriends", 
					"RawFollowers","RawUserSettings","RawVerifyCredentials","RawBlockList","RawBlockIDs",
					"RawUsers","RawUser","RawUserSearch","RawContributees","RawContributors",
					"RawSuggestedUsers","RawSuggestedUserCategories","RawSuggestedUserStatuses","RawFavorites","RawAccountStatus"},
	 	"RawPosts"			-> {
	 				"RawDeleteTweet",
	 				"RawUpdate","RawRetweet","RawMediaUpload",
	 				"RawDeleteDirectMessage","RawSendDirectMessage",
	 				"RawUpdateFollowing", "RawStopFollowing", "RawStartFollowing",
	 				"RawSetUserSettings","RawUpdateDevice","RawUpdateProfile","RawUpdateBackgroundImage",
	 				"RawUpdateProfileColors",
	 				(* "RawUpdateProfileImage",*)"RawCreateBlock","RawRemoveBlock","RawAddFavorite","RawRemoveFavorite"},
	 	"LogoutURL"			-> "https://twitter.com/logout",
 		"Information"		-> "A service for sending and receiving tweets from a Twitter account"
}

(* a function for importing the raw data - usually json or xml - from the service *)
twitterimport[$Failed]:=Throw[$Failed]
twitterimport[json_]:=With[{res=ImportString[json,"JSON"]},
	If[FreeQ[res,_["errors",_]],
		Switch[res,
			{},{},
			_Rule|{_Rule...},Association@res,
			{{_Rule...}...},Association/@res,
			_,res
		],
		Message[ServiceExecute::apierr,("errors"/.res)];
		Throw[$Failed]
	]
]


twitterStyleLimit = 50;

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

(** Timelines **)
twitterdata["RawMentionsTimeline"] = {
        "URL"				-> "https://api.twitter.com/1.1/statuses/mentions_timeline.json",
        "Parameters" 		-> {"count","since_id","max_id","trim_user","contributor_details","include_entities"},
        "RequiredParameters"-> {},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> twitterimport
    }   

twitterdata["RawUserTimeline"] = {
        "URL"				-> "https://api.twitter.com/1.1/statuses/user_timeline.json",
        "Parameters" 		-> {"user_id","screen_name","count","since_id","max_id","trim_user","exclude_replies","contributor_details","include_rts"},
        "RequiredParameters"-> {"user_id"|"screen_name"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> twitterimport
    }   
    
twitterdata["RawHomeTimeline"] = {
        "URL"				-> "https://api.twitter.com/1.1/statuses/home_timeline.json",
        "Parameters" 		-> {"count","since_id","max_id","trim_user","exclude_replies","contributor_details","include_entities"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> twitterimport
    }   
    
twitterdata["RawRetweetTimeline"] = {
        "URL"				-> "https://api.twitter.com/1.1/statuses/retweets_of_me.json",
        "Parameters" 		-> {"count","since_id","max_id","trim_user","include_entities","include_user_entities"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> twitterimport
    }   
    
(** Tweets **)
           
twitterdata["RawRetweets"] = {
     	"URL"				-> "https://api.twitter.com/1.1/statuses/retweets.json",
        "Parameters" 		-> {"id","count","trim_user"},
        "RequiredParameters"-> {"id"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> twitterimport
    }  
          
twitterdata["RawStatus"] = {
     	"URL"				-> "https://api.twitter.com/1.1/statuses/show.json",
        "Parameters" 		-> {"id","include_my_retweet","trim_user","include_entities"},
        "RequiredParameters"-> {"id"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> twitterimport
    }  
      
twitterdata["RawDeleteTweet"] = {
        "URL"				-> (ToString@StringForm["https://api.twitter.com/1.1/statuses/destroy/`1`.json", #]&),
        "PathParameters"	-> {"id"},
        "BodyData" 			-> {"trim_user"},
        "RequiredParameters"-> {"id","trim_user"(* Should not be required but URLFetch fails when the BodyData is empty *)},
        "HTTPSMethod"		-> "POST",
        "ResultsFunction"	-> twitterimport
    }
    
twitterdata["RawUpdate"] = {
        "URL"				-> "https://api.twitter.com/1.1/statuses/update.json",
        "BodyData"			-> {"status","in_reply_to_status_id","lat","long","place_id","display_coordinates","trim_user"},
        "RequiredParameters"-> {"status"},
        "HTTPSMethod"		-> "POST",
        "ResultsFunction"	-> twitterimport
    }     

twitterdata["RawRetweet"] = {
        "URL"				-> (ToString@StringForm["https://api.twitter.com/1.1/statuses/retweet/`1`.json", #]&),
        "PathParameters"	-> {"id"},
        "BodyData" 			-> {"trim_user"},
        "RequiredParameters"-> {"id","trim_user"(* Should not be required but URLFetch fails when the BodyData is empty *)},
        "HTTPSMethod"		-> "POST",
        "ResultsFunction"	-> twitterimport
    }        
          
twitterdata["RawMediaUpload"] = {
        "URL"				-> "https://api.twitter.com/1.1/statuses/update_with_media.json",
        "MultipartData"		-> {{"media[]","image/jpeg"},{"status","text/plain"},
        	{"possibly_sensitive","text/plain"},{"in_reply_to_status_id","text/plain"},
        	{"lat","text/plain"},{"long","text/plain"},{"place_id","text/plain"},{"display_coordinates","text/plain"} },
        "RequiredParameters"-> {"media[]","status"},
        "HTTPSMethod"		-> "POST",
        "ResultsFunction"	-> twitterimport
    }       
    
(* TODO: statuses/oembed *)

twitterdata["RawRetweeterIDs"] = {(* redundant with RawRetweets ?*)
        "URL"				-> "https://api.twitter.com/1.1/statuses/retweeters/ids.json",
        "Parameters" 		-> {"id","cursor","stringify_ids"},
        "RequiredParameters"-> {"id"},
        "HTTPSMethod"		-> "Get",
        "ResultsFunction"	-> twitterimport
    }  

twitterdata["RawTweetSearch"] = {
        "URL"				-> "https://api.twitter.com/1.1/search/tweets.json",
        "Parameters" 		-> {"q","geocode","lang","locale","result_type","count","until","since_id","max_id","include_entities"},
        "RequiredParameters"-> {"q"},
        "HTTPSMethod"		-> "Get",
        "ResultsFunction"	-> twitterimport
    }  
    
(* Stream API requires server support *)

(** Direct messages **)


twitterdata["RawDirectMessages"] = {
        "URL"				-> "https://api.twitter.com/1.1/direct_messages.json",
        "Parameters" 		-> {"count","since_id","max_id","include_entities","skip_status"},
        "HTTPSMethod"		-> "Get",
        "ResultsFunction"	-> twitterimport
    }  

twitterdata["RawDirectMessagesSent"] = {
        "URL"				-> "https://api.twitter.com/1.1/direct_messages/sent.json",
        "Parameters" 		-> {"count","since_id","max_id","include_entities","page"},
        "HTTPSMethod"		-> "Get",
        "ResultsFunction"	-> twitterimport
    }  

twitterdata["RawDirectMessage"] = {
        "URL"				-> "https://api.twitter.com/1.1/direct_messages/show.json",
        "Parameters" 		-> {"id"},
        "RequiredParameters"-> {"id"},
        "HTTPSMethod"		-> "Get",
        "ResultsFunction"	-> twitterimport
    }  

twitterdata["RawDeleteDirectMessage"] = {
        "URL"				-> "https://api.twitter.com/1.1/direct_messages/destroy.json",
        "BodyData" 			-> {"id","include_entities"},
        "RequiredParameters"-> {"id"},
        "HTTPSMethod"		-> "POST",
        "ResultsFunction"	-> twitterimport
    }  

twitterdata["RawSendDirectMessage"] = {
        "URL"				-> "https://api.twitter.com/1.1/direct_messages/new.json",
        "BodyData"			-> {"user_id","screen_name","text"},
        "RequiredParameters"-> {"user_id"|"screen_name","text"},
        "HTTPSMethod"		-> "POST",
        "ResultsFunction"	-> twitterimport
    }  

(* Friends and Followers *)
           
twitterdata["RawNoRetweetUserIDs"] = {
     	"URL"				-> "https://api.twitter.com/1.1/friendships/no_retweets/ids.json",
        "Parameters" 		-> {"stringify_ids"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> twitterimport
    }  
     
twitterdata["RawFriendIDs"] = { (* who the authenticated user is following *)
     	"URL"				-> "https://api.twitter.com/1.1/friends/ids.json",
        "Parameters" 		-> {"user_id","screen_name","cursor","stringify_ids","count"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> twitterimport
    }  
     
twitterdata["RawFollowerIDs"] = { (* who is following the specified user *)
     	"URL"				-> "https://api.twitter.com/1.1/followers/ids.json",
        "Parameters" 		-> {"user_id","screen_name","cursor","stringify_ids","count"},
        "RequiredParameters"-> {"user_id"|"screen_name"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> twitterimport
    }   
     
twitterdata["RawMyFriendship"] = { (* for the specified user, takes a comma separated list *)
     	"URL"				-> "https://api.twitter.com/1.1/friendships/lookup.json",
        "Parameters" 		-> {"user_id","screen_name"},
        "RequiredParameters"-> {"user_id"|"screen_name"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> twitterimport
    }          
      
twitterdata["RawIncomingFriendships"] = { (* for the specified user *)
     	"URL"				-> "https://api.twitter.com/1.1/friendships/incoming.json",
        "Parameters" 		-> {"cursor","stringify_ids"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> twitterimport
    }   

twitterdata["RawOutgoingFriendships"] = { (* for the specified user *)
     	"URL"				-> "https://api.twitter.com/1.1/friendships/outgoing.json",
        "Parameters" 		-> {"cursor","stringify_ids"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> twitterimport
    }   

twitterdata["RawStartFollowing"] = {
        "URL"				-> "https://api.twitter.com/1.1/friendships/create.json",
        "BodyData"			-> {"user_id","screen_name","follow"},
        "RequiredParameters"-> {"user_id"|"screen_name"},
        "HTTPSMethod"		-> "POST",
        "ResultsFunction"	-> twitterimport
    }  

twitterdata["RawStopFollowing"] = {
        "URL"				-> "https://api.twitter.com/1.1/friendships/destroy.json",
        "BodyData"			-> {"user_id","screen_name"},
        "RequiredParameters"-> {"user_id"|"screen_name"},
        "HTTPSMethod"		-> "POST",
        "ResultsFunction"	-> twitterimport
    } 
    
twitterdata["RawUpdateFollowing"] = { (* turn devive or retweet notifications on/off *)
        "URL"				-> "https://api.twitter.com/1.1/friendships/update.json",
        "BodyData"			-> {"user_id","screen_name","device","retweets"},
        "RequiredParameters"-> {"user_id"|"screen_name"},
        "HTTPSMethod"		-> "POST",
        "ResultsFunction"	-> twitterimport
    }    
    
twitterdata["RawFriendship"] = { (* between any two users *)
     	"URL"				-> "https://api.twitter.com/1.1/friendships/show.json",
        "Parameters" 		-> {"source_id","source_screen_name","target_id","target_screen_name"},
        "RequiredParameters"-> {"source_id"|"source_screen_name","target_id"|"target_screen_name"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> twitterimport
    }      
    
twitterdata["RawFriends"] = { (* who the specified user is following *)
     	"URL"				-> "https://api.twitter.com/1.1/friends/list.json",
        "Parameters" 		-> {"user_id","screen_name","cursor","skip_status","include_user_entities","count"},
        "RequiredParameters"-> {"user_id"|"screen_name"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> twitterimport
    }  
      
twitterdata["RawFollowers"] = { (* who is following the specified user *)
     	"URL"				-> "https://api.twitter.com/1.1/followers/list.json",
        "Parameters" 		-> {"user_id","screen_name","cursor","skip_status","include_user_entities","count"},
        "RequiredParameters"-> {"user_id"|"screen_name"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> twitterimport
    }  

(** Users **)   
twitterdata["RawUserSettings"] = { 
     	"URL"				-> "https://api.twitter.com/1.1/account/settings.json",
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> twitterimport
    }  
    
twitterdata["RawVerifyCredentials"] = { 
     	"URL"				-> "https://api.twitter.com/1.1/account/verify_credentials.json",
        "HTTPSMethod"		-> "GET",
        "Parameters" 		-> {"include_entities","skip_status"},
        "ResultsFunction"	-> twitterimport
    }   
    
twitterdata["RawSetUserSettings"] = { 
     	"URL"				-> "https://api.twitter.com/1.1/account/settings.json",
        "HTTPSMethod"		-> "POST",
        "BodyData" 			-> {"trend_location_woeid","sleep_time_enabled","start_sleep_time","end_sleep_time","time_zone","lang"},
        "RequiredParameters"-> {"trend_location_woeid"|"sleep_time_enabled"|"start_sleep_time"|"end_sleep_time"|"time_zone"|"lang"},
        "ResultsFunction"	-> twitterimport
    }     
    
twitterdata["RawUpdateDevice"] = { 
     	"URL"				-> "https://api.twitter.com/1.1/account/settings.json",
        "HTTPSMethod"		-> "POST",
        "BodyData" 			-> {"device","include_entities"},
        "RequiredParameters"-> {"device"},
        "ResultsFunction"	-> twitterimport
    }  
    
twitterdata["RawUpdateProfile"] = { 
     	"URL"				-> "https://api.twitter.com/1.1/account/update_profile.json",
        "HTTPSMethod"		-> "POST",
        "BodyData" 			-> {"name","url","location","description","include_entities","skip_status"},
        "RequiredParameters"-> {"name"|"url"|"location"|"description"|"include_entities"|"skip_status"},
        "ResultsFunction"	-> twitterimport
    }  
    
twitterdata["RawUpdateBackgroundImage"] = { 
     	"URL"				-> "https://api.twitter.com/1.1/account/update_profile_background_image.json",
        "HTTPSMethod"		-> "POST",
        "MultipartData"		-> {{"image","image/jpeg"},{"tile","text/plain"},{"status","text/plain"},{"include_entities","text/plain"},
        	{"include_entities","text/plain"},{"skip_status","text/plain"},{"use","text/plain"}},
        "RequiredParameters"-> {"image"|"tile"|"use"},
        "ResultsFunction"	-> twitterimport
    }    
    
twitterdata["RawUpdateProfileColors"] = { 
     	"URL"				-> "https://api.twitter.com/1.1/account/update_profile_colors.json",
        "HTTPSMethod"		-> "POST",
        "BodyData" 			-> {"profile_background_color","profile_link_color","profile_sidebar_border_color",
        	"profile_sidebar_fill_color","profile_text_color","include_entities","skip_status"},
        "ResultsFunction"	-> twitterimport
    }  
    
twitterdata["RawUpdateProfileImage"] = { 
     	"URL"				-> "https://api.twitter.com/1.1/account/update_profile.json",
        "HTTPSMethod"		-> "POST",
        "BodyData"			-> {"image","include_entities","skip_status"},
       (* "MultipartData"		-> {{"image","image/jpeg"},{"include_entities","text/plain"},{"skip_status","text/plain"}},*)
        "RequiredParameters"-> {"image"},
        "ResultsFunction"	-> twitterimport
    }  
   
    
(* Blocks *)
twitterdata["RawBlockList"] = { 
     	"URL"				-> "https://api.twitter.com/1.1/blocks/list.json",
        "HTTPSMethod"		-> "GET",
        "Parameters" 		-> {"include_entities","skip_status","cursor"},
        "ResultsFunction"	-> twitterimport
    }       

twitterdata["RawBlockIDs"] = { 
     	"URL"				-> "https://api.twitter.com/1.1/blocks/ids.json",
        "HTTPSMethod"		-> "GET",
        "Parameters" 		-> {"stringify_ids","cursor"},
        "ResultsFunction"	-> twitterimport
    }  

twitterdata["RawCreateBlock"] = { 
     	"URL"				-> "https://api.twitter.com/1.1/blocks/create.json",
        "HTTPSMethod"		-> "POST",
        "BodyData" 			-> {"screen_name","user_id","include_entities","skip_status"},
        "RequiredParameters"-> {"screen_name"|"user_id"},
        "ResultsFunction"	-> twitterimport
    }   

twitterdata["RawRemoveBlock"] = { 
     	"URL"				-> "https://api.twitter.com/1.1/blocks/destroy.json",
        "HTTPSMethod"		-> "POST",
        "BodyData" 			-> {"screen_name","user_id","include_entities","skip_status"},
        "RequiredParameters"-> {"screen_name"|"user_id"},
        "ResultsFunction"	-> twitterimport
    }      
(* Users *)
twitterdata["RawUsers"] = { (* comma separated list of users *)
     	"URL"				-> "https://api.twitter.com/1.1/users/lookup.json",
        "HTTPSMethod"		-> "GET",
        "Parameters" 		-> {"screen_name","user_id","include_entities"},
        "RequiredParameters"-> {"screen_name"|"user_id"},
        "ResultsFunction"	-> twitterimport
    }  
    
twitterdata["RawUser"] = { (* a single user, with more information *)
     	"URL"				-> "https://api.twitter.com/1.1/users/show.json",
        "HTTPSMethod"		-> "GET",
        "Parameters" 		-> {"screen_name","user_id","include_entities"},
        "RequiredParameters"-> {"screen_name"|"user_id"},
        "ResultsFunction"	-> twitterimport
    } 
 
twitterdata["RawUserSearch"] = { 
     	"URL"				-> "https://api.twitter.com/1.1/users/search.json",
        "HTTPSMethod"		-> "GET",
        "Parameters" 		-> {"q","page","count","include_entities"},
        "RequiredParameters"-> {"q"},
        "ResultsFunction"	-> twitterimport
    } 
 
twitterdata["RawContributees"] = { 
     	"URL"				-> "https://api.twitter.com/1.1/users/contributees.json",
        "HTTPSMethod"		-> "GET",
        "Parameters" 		-> {"screen_name","user_id","include_entities","skip_status"},
        "RequiredParameters"-> {"screen_name"|"user_id"},
        "ResultsFunction"	-> twitterimport
    } 
 
twitterdata["RawContributors"] = { 
     	"URL"				-> "https://api.twitter.com/1.1/users/contributors.json",
        "HTTPSMethod"		-> "GET",
        "Parameters" 		-> {"screen_name","user_id","include_entities","skip_status"},
        "RequiredParameters"-> {"screen_name"|"user_id"},
        "ResultsFunction"	-> twitterimport
    } 

(* profile banners omitted for now *)

(** Suggested Users **)
twitterdata["RawSuggestedUsers"] = { 
     	"URL"				-> (ToString@StringForm["https://api.twitter.com/1.1/users/suggestions/`1`.json", #]&),
        "HTTPSMethod"		-> "GET",
        "PathParameters"	-> {"slug"},
        "Parameters" 		-> {"lang"},
        "RequiredParameters"-> {"slug"},
        "ResultsFunction"	-> twitterimport
    } 
    
twitterdata["RawSuggestedUserCategories"] = { 
     	"URL"				-> "https://api.twitter.com/1.1/users/suggestions.json",
        "HTTPSMethod"		-> "GET",
        "Parameters" 		-> {"lang"},
        "ResultsFunction"	-> twitterimport
    } 
    
twitterdata["RawSuggestedUserStatuses"] = { 
     	"URL"				-> (ToString@StringForm["https://api.twitter.com/1.1/users/suggestions/`1`/members.json", #]&),
        "HTTPSMethod"		-> "GET",
        "PathParameters"	-> {"slug"},
        "Parameters" 		-> {"lang"},
        "RequiredParameters"-> {"slug"},
        "ResultsFunction"	-> twitterimport
    } 

(** Favorites **)
twitterdata["RawFavorites"] = { 
     	"URL"				-> "https://api.twitter.com/1.1/favorites/list.json",
        "HTTPSMethod"		-> "GET",
        "Parameters" 		-> {"screen_name","user_id","count","include_entities","since_id","max_id"},
        "ResultsFunction"	-> twitterimport
    }      
     
twitterdata["RawRemoveFavorite"] = { 
     	"URL"				-> "https://api.twitter.com/1.1/favorites/destroy.json",
        "HTTPSMethod"		-> "POST",
        "BodyData" 			-> {"id","include_entities"},
        "RequiredParameters"-> {"id"},
        "ResultsFunction"	-> twitterimport
    } 

twitterdata["RawAddFavorite"] = { 
     	"URL"				-> "https://api.twitter.com/1.1/favorites/create.json",
        "HTTPSMethod"		-> "POST",
        "BodyData" 			-> {"id","include_entities"},
        "RequiredParameters"-> {"id"},
        "ResultsFunction"	-> twitterimport
    } 



(*** App ***)
twitterdata["RawAccountStatus"] = {
        "URL"				-> "https://api.twitter.com/1.1/application/rate_limit_status.json",
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> twitterimport
    }
            
(** Lists **)
(** Saved Searches **)
(** Places and Geo **)
(** Trends **)
(** Spam reporting **)

twitterdata["icon"]:=bird

twitterdata[___]:=$Failed
(****** Cooked Properties ******)
  
(* cooked data queries 
	Cooked queries call at least one of the raw queried underneath but add pre and post processing to make the function easier to use and the result clearer.
*)  


(* SocialMediaData 
{"FollowerIDs", "FollowerMentionNetwork", "FollowerNetwork", 
"FollowerReplyToNetwork", "Followers", "FriendIDs", 
"FriendMentionNetwork", "FriendNetwork", "FriendReplyToNetwork", 
"Friends", "RateLimit", "SearchMentionNetwork", "SearchNetwork", 
"SearchReplyToNetwork", "UserData"}
*)

getAuthenticatedTwitterID[id_]:=If[ValueQ[twitterid[id]]&&twitter[id]=!=$Failed,
	twitter[id],
	twitter[id]=With[{rawdata=OAuthClient`rawoauthdata[id,"RawVerifyCredentials"]},
		Lookup[twitterimport[rawdata],"id_str",Throw[$Failed]]
	]
]

twittercookeddata[prop_,id_,rule_Rule, rest___]:=twittercookeddata[prop,id,{rule}, rest]
twittercookeddata[prop_,id_]:=twittercookeddata[prop,id,{}]

twittercookeddata[prop:("FollowerIDs"|"Followers"|"FriendIDs"|"Friends"),id_,args_]:=Module[
	{rawdata,params, ids, prop1},
	prop1=Switch[prop,
		"FollowerIDs"|"Followers","RawFollowerIDs",
		"FriendIDs"|"Friends","RawFriendIDs"
	];
	params=filterparameters[args,getallparameters[prop1]];
	params=If[FreeQ[params,("user_id"|"screen_name")->_],
		Flatten[{params,"user_id"->getAuthenticatedTwitterID[id]}],
		Flatten[{params}]
		];
	rawdata=OAuthClient`rawoauthdata[id,prop1,params];
	(* Bug 255746 work around
	Lookup[twitterimport[rawdata],"ids",$Failed]
	*)	
    ids=Flatten[Map[StringCases[#, DigitCharacter ..] & ,
        StringCases[rawdata, RegularExpression["(?ms)\"ids\":\\s*\\[\\s*(.+)\\s*\\]"] -> "$1"]]];
    If[MatchQ[prop,("Followers"|"Friends")],
    	getscreennames[id,ids],
    	ids
    	]
	
]
    
twittercookeddata["GetTweet",id_,args_]:=Module[
	{rawdata},
	rawdata=OAuthClient`rawoauthdata[id,"RawStatus",Flatten[{args}/."TweetID"->"id"]];
	twitterimport[rawdata]["text"]
]         
     
twittercookeddata[prop:("TweetGrid"|"TweetEventSeries"|"TweetTimeline"|"TweetList"),id_,args_]:=Module[
	{rawdata,params,data,dates, tweets},
	params=filterparameters[args,getallparameters["RawUserTimeline"]];
	
	params=params/.HoldPattern[Rule[a_,b_?(!StringQ[#]&)]]:>Rule[a,ToString[b]];
	params=If[FreeQ[params,("user_id"|"screen_name")->_],
		Flatten[{params,"user_id"->getAuthenticatedTwitterID[id]}],
		Flatten[{params}]
	]/."TweetID"->"id";
	rawdata=OAuthClient`rawoauthdata[id,"RawUserTimeline",params];
	data=twitterimport[rawdata];
	Switch[prop,
		"TweetGrid",
		OAuthClient`Private`prettygrid[Join[{{"CreatedAt","Text","TweetID"}},
			{"created_at", "text","id_str"}/.((Normal/@data)/.OAuthClient`Private`formatvalue["created_at"->(readDate[#]&)])]],
		"TweetList",
		Association[Replace[FilterRules[Normal[#],{"created_at", "text","id_str"}],
			HoldPattern[Rule][a_String,b_]:>Rule[camelcase[a],b],Infinity]/.{
				"IDStr"->"TweetID",OAuthClient`Private`formatvalue["CreatedAt"->readDate]}]&/@data,
		"TweetEventSeries",
		EventSeries[{"created_at", "text"}/.((Normal/@data)/.OAuthClient`Private`formatvalue["created_at"->(readDate[#]&)])],
		"TweetTimeline",
		{dates, tweets}=Transpose[{"created_at", "text"}/.((Normal/@data)/.OAuthClient`Private`formatvalue["created_at"->(readDate[#]&)])];
		OAuthClient`Private`eventtimeline[tweets, dates]	
	]
]      
        
twittercookeddata["LastTweet",id_,args_]:=Module[
	{rawdata,params,screennames},
	params=filterparameters[args,getallparameters["RawUserTimeline"]];
	params=If[FreeQ[params,("user_id"|"screen_name")->_],
		Flatten[{params,"user_id"->getAuthenticatedTwitterID[id],"count"->"1"}],
		Flatten[{params,"count"->"1"}]
	]/."TweetID"->"id";
	rawdata=OAuthClient`rawoauthdata[id,"RawUserTimeline",params];
	First[twitterimport[rawdata]]["text"]
]   

twittercookeddata["TweetSearch",id_,args_]:=Module[
	{rawdata,res,params,ids, users, tweets,n,cut,screennames},
	params=filterparameters[args/."Query"->"q",getallparameters["RawTweetSearch"]];
	params=params/.HoldPattern[Rule[a_,b_?(!StringQ[#]&)]]:>Rule[a,ToString[b]];
	rawdata=OAuthClient`rawoauthdata[id,"RawTweetSearch",params];
	
	
	(* Bug 255746 work around - TODO: remove this hack when JSON import is fixed
	res=twitterimport[rawdata];
	res=Lookup[res,"statuses",{}];
	Thread[{"ID","User","Tweet"}->OAuthClient`Private`getdata[#,{"id_str","user","text"}]]&/@res
	*)
	
	
	cut = Delete[StringSplit[rawdata, "metadata"], {{1}, {-1}}];
	ids = First /@ StringCases[cut, "\"id_str\":" ~~ Shortest[x___] ~~ (","|"\\") -> x];
	screennames = First /@ StringCases[cut, "\"screen_name\":" ~~ Shortest[x___] ~~ (","|"\\") -> x];
	users = First /@ StringCases[cut, "\"name\":" ~~ Shortest[x___] ~~ (","|"\\") -> x];
	tweets = First /@ StringCases[cut, "\"text\":" ~~ Shortest[x___] ~~ ("\",\""):> (x <> "\"")];

	n=Min[Length/@{ids, users, tweets}];
	Association[Thread[{"TweetID","Name","ScreenName","Message"}->#]]&/@Map[ImportString[#,"JSON"]&,Transpose[Take[#,n]&/@{ids, users, screennames,tweets}],{2}]
]  

twittercookeddata["UserData",id_,args_]:=Module[
	{rawdata,params,users,res, users1},
	params=filterparameters[args,getallparameters["RawUsers"]];
	params=If[FreeQ[params,("user_id"|"screen_name")->_],
		Flatten[{params,"user_id"->getAuthenticatedTwitterID[id]}],
		Flatten[{params}]
		];
	users=Cases[params,HoldPattern[Rule|RuleDelayed]["user_id"|"screen_name",u_]:>u];
	(* comma separate multiple users *)
	If[MatchQ[users,{{___}}],
		users=First[users];
		users1=ToString/@users;
		If[Length[users1]>100, (* split it up *)
			Return[
				Flatten[twittercookeddata["UserData",id,
					(params/.(users->StringJoin[Riffle[#,","]]))]&/@Partition[users1,100,100,1,{}]]
			]
		];
		params=(params/.(users->StringJoin[Riffle[users1,","]]))];
	(* strip @ from screen name *)
	params=params/.("screen_name"->x_):>("screen_name"->StringReplace[x,"@"->""]);
	rawdata=OAuthClient`rawoauthdata[id,"RawUsers",params];
	res=twitterimport[rawdata];
	If[!MatchQ[res,{_Association...}],Throw[$Failed]];
	Association[Replace[
			Replace[FilterRules[Normal[#],{"id", "screen_name", "name", "location", "favourites_count", "followers_count", "friends_count"}],
				HoldPattern[Rule[a_String, b_]] :> Rule[a,userdataparse[b,a]],Infinity],
		HoldPattern[Rule[a_,b_]]:>Rule[camelcase[a],b],Infinity]]&/@res
]

twittercookeddata["UserHashtags",id_,args_]:=Module[
	{rawdata,params,users,res, users1},
	params=filterparameters[args,getallparameters["RawUserTimeline"]];
	params=If[FreeQ[params,("user_id"|"screen_name")->_],
		Flatten[{params,"user_id"->getAuthenticatedTwitterID[id]}],
		Flatten[{params}]
		];
	users=Cases[params,HoldPattern[Rule|RuleDelayed]["user_id"|"screen_name",u_]:>u];
	If[MatchQ[users,{{___}}],
		users=First[users];
		users1=ToString/@users;
		params=(params/.(users->StringJoin[Riffle[users1,","]]))];
		
	rawdata=OAuthClient`rawoauthdata[id,"RawUserTimeline",params];
	res=twitterimport[rawdata];
	Union[Flatten[StringCases[#, 
		(StartOfString | Whitespace) ~~ x : ("#" ~~ Shortest[___]) ~~ (Whitespace | EndOfString) :> x]&/@(Lookup[#, "text", ""] & /@ res)]]

]

twittercookeddata["UserMentions",id_,args_]:=Module[
	{rawdata,params,users,res, users1},
	params=filterparameters[args,getallparameters["RawUserTimeline"]];
	params=If[FreeQ[params,("user_id"|"screen_name")->_],
		Flatten[{params,"user_id"->getAuthenticatedTwitterID[id]}],
		Flatten[{params}]
		];
	users=Cases[params,HoldPattern[Rule|RuleDelayed]["user_id"|"screen_name",u_]:>u];
	If[MatchQ[users,{{___}}],
		users=First[users];
		users1=ToString/@users;
		params=(params/.(users->StringJoin[Riffle[users1,","]]))];
		
	rawdata=OAuthClient`rawoauthdata[id,"RawUserTimeline",params];
	(* Bug 255746 work around
	res=twitterimport[rawdata];
	Union[Flatten[StringCases[#, RegularExpression["@((?:[[:alnum:]])+)"]]&/@(Lookup[#, "text", ""] & /@ res)]]
	*)
	StringCases[rawdata, RegularExpression["(?ms)\"text\":.*?@((?:[[:alnum:]])+)"] -> "$1"]
]

twittercookeddata["UserReplies",id_,args_]:=Module[
	{rawdata,params,users,res, users1,pos, temp},
	params=filterparameters[args,getallparameters["RawUserTimeline"]];
	params=If[FreeQ[params,("user_id"|"screen_name")->_],
		Flatten[{params,"user_id"->getAuthenticatedTwitterID[id]}],
		Flatten[{params}]
		];
	users=Cases[params,HoldPattern[Rule|RuleDelayed]["user_id"|"screen_name",u_]:>u];
	If[MatchQ[users,{{___}}],
		users=First[users];
		users1=ToString/@users;
		params=(params/.(users->StringJoin[Riffle[users1,","]]))];
		
	rawdata=OAuthClient`rawoauthdata[id,"RawUserTimeline",params];
	res=twitterimport[rawdata];
	temp=(Lookup[#,"in_reply_to_user_id_str",Null]&/@res);
	pos=Flatten[Position[temp,Except[Null],{1},Heads->False]];
	temp=(Lookup[#,"text",Null]&/@res[[pos]])/.Null:>Sequence@@{}
	(* StringCases[rawdata, RegularExpression["(?ms)in_reply_to_user_id_str\":\"(\\d+)\""] -> "$1"]*)
]

twittercookeddata["Tweet",id_,args_]:=Module[
	{rawdata,otherparams},
	otherparams=FilterRules[args,Except["Message"]];
	otherparams=otherparams/.HoldPattern[Rule["GeoPosition",pos_]]:>(Sequence@@formatgeoposition[pos]);
	otherparams=filterparameters[otherparams,getallparameters["RawUpdate"]];
	rawdata=OAuthClient`rawoauthdata[id,"RawUpdate",Flatten[{otherparams,"status"->("Message"/.args)}]];
	twitterimport[rawdata]["text"]
]    
    
    
twittercookeddata["RateLimit",id_,args_]:=Module[
	{rawdata, res},
	rawdata=OAuthClient`rawoauthdata[id,"RawAccountStatus"];
	res=twitterimport[rawdata];
	If[KeyExistsQ[res, "resources"],
		Association[Replace[res["resources"], Rule[a_String, b_] :> (StringReplace[a, {StartOfString ~~ "/" -> "", ":" -> "", "/" -> " "}] -> b), {2, 5}]],
		$Failed
	]
]    

twittercookeddata["UserIDSearch",id_,args_]:=Module[
	{rawdata,res},
	rawdata=OAuthClient`rawoauthdata[id,"RawUserSearch",Flatten[args/."Query"->"q"]];
	res=twitterimport[rawdata];
	(Lookup[#,"id_str",Null]&/@res)/.Null:>Sequence@@{}
]    
    
       
networkprops=("FollowerMentionNetwork"|"FollowerReplyToNetwork"|"FriendMentionNetwork"|"FriendReplyToNetwork"|"FriendNetwork"|"FollowerNetwork");
twittercookeddata[prop:networkprops,id_,args_]:=Module[
	{params,twitterid},
	params=filterparameters[args,{"user_id"}];
	params=If[FreeQ[params,("user_id"|"screen_name")->_],
		Flatten[{params,"user_id"->getAuthenticatedTwitterID[id]}],
		Flatten[{params}]
		];
	twitterid="user_id"/.params;
	If[twitterid==="user_id",
		twitterid=First[getuserids[id,{"screen_name"/.params}]]
	];
	buildnetwork[prop,id,{"user_id"->twitterid,"stringify_ids"->"true"}]
]     
   
twittercookeddata[prop:("SearchNetwork"|"SearchReplyToNetwork"|"SearchMentionNetwork"),id_,args_]:=buildnetwork[prop,id,args]   
 
twittercookeddata["ImageTweet",id_,args___]:=Module[
	{rawdata, status,media,statusBytes,mediaBytes,params,otherparams},
	{status,media}=Switch[{args},
		{_Rule..}|{{_Rule..}},
			{("Message"/.Flatten[{args}])/."Message"->"",
				("Image"/.Flatten[{args}])/."Image":>(Message[ServiceExecute::nparam,"Image"];Throw[$Failed])},
		{_},{"",args},
		_,(Message[ServiceExecute::nparam,"Image"];Throw[$Failed])
	];
	statusBytes=Switch[status,
		{_?IntegerQ...},status,
		_String,ImportString[status, "Byte"],
		_,ImportString[ToString[status], "Byte"]
	]/.{}->{32} (* Create a space, because our MultiPartData does not accept an empty list *);
	
	mediaBytes=Which[
		MatchQ[media,{_?IntegerQ...}],media,
		TrueQ[Quiet[FileExistsQ[media],FileExistsQ::fstr]],Import[media, "Byte"],
		ImageQ[media],ImportString[ExportString[media, "JPG"], "Byte"],
		True,
			Check[ImportString[ExportString[media, "JPG"], "Byte"],Throw[$Failed]]
	];
	otherparams=FilterRules[args,Except["Message"|"Image"]];
	otherparams=otherparams/.HoldPattern[Rule["GeoPosition",pos_]]:>(Sequence@@formatgeoposition[pos]);
	otherparams=filterparameters[otherparams,getallparameters["RawMediaUpload"]];
	If[otherparams=!={},
		otherparams[[All,2]]=ImportString[#, "Byte"]&/@otherparams[[All,2]];
	];
	
	params=Flatten[{"media[]"->mediaBytes,"status"->statusBytes,otherparams}];
	rawdata=OAuthClient`rawoauthdata[id,"RawMediaUpload",params];
	twitterimport[rawdata]["text"]
]   
     
(***************** Network *********************)
buildnetwork[prop_,id_,params_]:=Block[
	{names, rootid, rootname, v, len, res1, res2, a, edges, res, vertices,vertprop,edgeprop,OAuthClient`$CacheResults=True,twitterid},
	(* Get vertices *)
	vertprop=Switch[prop,
		"FollowerMentionNetwork"|"FollowerNetwork"|"FollowerReplyToNetwork","FollowerIDs",
		"FriendMentionNetwork"|"FriendNetwork"|"FriendReplyToNetwork","FriendIDs",
		"SearchNetwork" | "SearchReplyToNetwork" | "SearchMentionNetwork","UserIDSearch"
	];
	edgeprop=Switch[prop,
		"FollowerMentionNetwork"|"FriendMentionNetwork"|"SearchMentionNetwork","UserMentions",
		"FollowerReplyToNetwork"|"FriendReplyToNetwork"|"SearchReplyToNetwork", "UserReplies",
		"FriendNetwork","FriendIDs",
		"FollowerNetwork"|"SearchNetwork","FollowerIDs"
	];
	v = twittercookeddata[vertprop,id,params];
    (* get user names and prepend the root user to the list *)
    If[v === $Canceled, Return[$Canceled]];
    (
	    names = getscreennames[id,v];
	    If[!ListQ[names], Return[$Failed]];
	    (
	    	If[!StringMatchQ[prop,"Search*"],
	    		twitterid="user_id"/.params;
			    res1 = twittercookeddata["UserData",id,"user_id"->twitterid];
			    If[!ListQ[res1], Return[$Failed]];
		    	{rootid, rootname} = {"ID", "Name"} /. First[res1];
		    	v     = Join[{ToString[rootid]}, v];
		    	names = Join[{rootname}, names];
		   	];
	    	If[Length[v]===1,
	    		vertices = MapThread[Property[#1, "Name" -> #2] &, {v, names}];
	    		res = Graph[vertices, {}];
	    		Return[res];
	    	];
	    	len   = Min[twitterStyleLimit, Length[v]];
	    	v     = v[[;; len]];
	    	names = names[[;; len]];
	    	(* Get the outgoing edges from each vertex *)
	    	res2=(Catch[twittercookeddata[edgeprop,id,{"user_id"->#}]]&/@v)/.($Failed|Missing|Missing[__])->{};
	       If[edgeprop==="UserMentions",
		       	(* Bug 255746 work around 
		        res2=getuserids[id,#]&/@res2;
				*)
				res2=StringReplace[StringJoin[Riffle[#,","]],"@"->""]&/@res2;
				res2=With[{
					rawdata=OAuthClient`rawoauthdata[id,"RawUsers","screen_name"->#]
					},
		       	StringCases[rawdata, RegularExpression["(?ms)\"id\":\\s*(\\d+)"] -> "$1"]
					]&/@res2;
					
	       	];
			(* End work around *) 
			edges = Flatten[Table[a = Complement[Intersection[v, res2[[i]]], {v[[i]]}];
				If[a != {}, DirectedEdge[v[[i]], #] & /@ a, {}], {i, len}]];
			vertices = MapThread[Property[#1, "Name" -> #2] &, {v, names}];
			res = Graph[vertices, edges];
			res=SetProperty[res, VertexLabels -> MapThread[#1 -> Placed[#2, Tooltip] &, {v, names}]];
			res /; GraphQ[res]
		) /; (names =!= $Failed)
	) /; (v =!= $Failed)
]

buildnetwork[___]:=$Failed

getscreennames[_,{}]:={}
getscreennames[id_,twitterids_]:=getscreennames[_,twitterids]=Flatten[Lookup[#,"Name",{}]&/@twittercookeddata["UserData",id,"user_id"->twitterids]]
getuserids[_,{}]:={}
getuserids[id_,screennames_List]:= getuserids[_,screennames]=Flatten[Lookup[#,"ID",{}]&/@twittercookeddata["UserData",id,"user_id"->twitterids]]

getallparameters[str_]:=DeleteCases[Flatten[{"Parameters","PathParameters","BodyData","MultipartData"}/.twitterdata[str]],
	("Parameters"|"PathParameters"|"BodyData"|"MultipartData")]

(* Send Message *)
twittersendmessage[id_,message_String]:=twittercookeddata["Tweet",id,"Message"->message]
twittersendmessage[id_,message_]:=twittercookeddata["ImageTweet",id,{"Image"->message}]/;ImageQ[message]||MatchQ[message,(_Graphics|_Graphics3D)]
twittersendmessage[___]:=$Failed

(*** Service specific utilites ****)
filterparameters=OAuthClient`Private`filterParameters;
camelcase=OAuthClient`Private`camelCase;

readDate[date_, form_: DateObject] := 
 form@DateList[{StringReplace[
     date, (RegularExpression["[+-]"] ~~ DigitCharacter .. ~~ " ") -> 
      ""], {"DayName", "MonthName", "Day", "Hour", ":", "Minute", ":",
      "Second", "Year"}}]
 
 
$loadentitydata=False;                           
userdataparse[x_,"location"]:=If[$loadentitydata,WolframAlpha[x, "MathematicaResult"]/.Missing[___]:>(x),x]
userdataparse[x_,_]:=x
   
formatgeoposition[pos_]:={"lat"->ToString[QuantityMagnitude@Latitude[pos]],"long"->ToString[QuantityMagnitude@Longitude[pos]]}


bird=Image[RawArray["Byte", {{{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 
  0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, 
  {0, 0, 0, 0}, {40, 170, 225, 9}, {40, 170, 225, 113}, {40, 170, 225, 207}, {40, 170, 225, 249}, {40, 170, 225, 252}, 
  {40, 170, 225, 217}, {40, 170, 225, 131}, {40, 170, 225, 16}, {0, 0, 0, 0}, {0, 0, 0, 0}, {40, 170, 225, 1}, {40, 
  170, 225, 74}, {40, 170, 225, 19}}, {{0, 0, 0, 0}, {40, 170, 225, 62}, {40, 170, 225, 124}, {0, 0, 0, 0}, {0, 0, 0, 
  0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, 
  {0, 0, 0, 0}, {0, 0, 0, 0}, {40, 170, 225, 28}, {40, 170, 225, 210}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 
  170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 230}, {40, 170, 225, 
  86}, {40, 170, 225, 130}, {40, 170, 225, 212}, {40, 170, 225, 187}, {0, 0, 0, 0}}, {{0, 0, 0, 0}, {40, 170, 225, 
  158}, {40, 170, 225, 255}, {40, 170, 225, 119}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 
  0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {40, 170, 225, 5}, {40, 170, 225, 207}, 
  {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 
  225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 228}, 
  {40, 170, 225, 31}, {40, 170, 225, 33}}, {{0, 0, 0, 0}, {40, 170, 225, 201}, {40, 170, 225, 255}, {40, 170, 225, 
  255}, {40, 170, 225, 153}, {40, 170, 225, 8}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, 
  {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {40, 170, 225, 102}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 
  225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, 
  {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 252}, {40, 170, 225, 175}, {40, 170, 225, 206}, {40, 170, 
  225, 117}}, {{0, 0, 0, 0}, {40, 170, 225, 201}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 
  170, 225, 213}, {40, 170, 225, 61}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 
  0}, {0, 0, 0, 0}, {40, 170, 225, 189}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 
  255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 
  170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 139}, {0, 0, 0, 0}}, {{0, 0, 0, 0}, {40, 
  170, 225, 153}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 
  255}, {40, 170, 225, 177}, {40, 170, 225, 60}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, 
  {40, 170, 225, 221}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 
  225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, 
  {40, 170, 225, 255}, {40, 170, 225, 111}, {0, 0, 0, 0}, {0, 0, 0, 0}}, {{0, 0, 0, 0}, {40, 170, 225, 59}, {40, 170, 
  225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, 
  {40, 170, 225, 255}, {40, 170, 225, 217}, {40, 170, 225, 138}, {40, 170, 225, 75}, {40, 170, 225, 26}, {40, 170, 
  225, 1}, {40, 170, 225, 212}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, 
  {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 
  225, 255}, {40, 170, 225, 255}, {40, 170, 225, 16}, {0, 0, 0, 0}, {0, 0, 0, 0}}, {{0, 0, 0, 0}, {0, 0, 0, 0}, {40, 
  170, 225, 154}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 
  255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 
  170, 225, 251}, {40, 170, 225, 252}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 
  255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 
  170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 8}, {0, 0, 0, 0}, {0, 0, 0, 0}}, {{0, 0, 0, 0}, {40, 170, 225, 
  170}, {40, 170, 225, 120}, {40, 170, 225, 215}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 
  170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 
  255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 
  170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 
  255}, {40, 170, 225, 255}, {40, 170, 225, 239}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}, {{0, 0, 0, 0}, {40, 170, 
  225, 177}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, 
  {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 
  225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, 
  {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 
  225, 255}, {40, 170, 225, 255}, {40, 170, 225, 198}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}, {{0, 0, 0, 0}, {40, 
  170, 225, 91}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 
  255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 
  170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 
  255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 
  170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 140}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}, {{0, 0, 0, 0}, 
  {40, 170, 225, 3}, {40, 170, 225, 197}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 
  225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, 
  {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 
  225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, 
  {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 67}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}, {{0, 0, 0, 
  0}, {0, 0, 0, 0}, {40, 170, 225, 21}, {40, 170, 225, 201}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 
  255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 
  170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 
  255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 
  170, 225, 255}, {40, 170, 225, 226}, {40, 170, 225, 3}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}, {{0, 0, 0, 0}, 
  {0, 0, 0, 0}, {0, 0, 0, 0}, {40, 170, 225, 12}, {40, 170, 225, 104}, {40, 170, 225, 217}, {40, 170, 225, 255}, {40, 
  170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 
  255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 
  170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 
  255}, {40, 170, 225, 118}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}, {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 
  0, 0, 0}, {40, 170, 225, 144}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, 
  {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 
  225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, 
  {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 231}, {40, 170, 
  225, 11}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}, {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {40, 
  170, 225, 21}, {40, 170, 225, 232}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 
  255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 
  170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 
  255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 89}, {0, 0, 0, 0}, {0, 0, 0, 0}, 
  {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}, {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {40, 170, 225, 
  48}, {40, 170, 225, 230}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 
  170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 
  255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 
  170, 225, 255}, {40, 170, 225, 165}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 
  0}}, {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {40, 170, 225, 16}, {40, 170, 225, 131}, 
  {40, 170, 225, 216}, {40, 170, 225, 252}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 
  225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, 
  {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 201}, {40, 170, 225, 10}, {0, 0, 0, 
  0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}, {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 
  0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {40, 170, 225, 59}, {40, 170, 225, 215}, {40, 170, 225, 
  255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 
  170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 
  201}, {40, 170, 225, 15}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 
  0, 0}}, {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {40, 170, 225, 8}, {40, 170, 225, 74}, {40, 170, 
  225, 183}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, 
  {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 
  225, 255}, {40, 170, 225, 255}, {40, 170, 225, 172}, {40, 170, 225, 11}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, 
  {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}, {{40, 170, 225, 81}, {40, 170, 225, 147}, 
  {40, 170, 225, 167}, {40, 170, 225, 195}, {40, 170, 225, 248}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 
  225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, 
  {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 235}, {40, 170, 
  225, 95}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 
  0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}, {{0, 0, 0, 0}, {40, 170, 225, 62}, {40, 170, 225, 179}, {40, 170, 225, 251}, 
  {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 
  225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, {40, 170, 225, 255}, 
  {40, 170, 225, 224}, {40, 170, 225, 120}, {40, 170, 225, 12}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 
  0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}, {{0, 0, 0, 
  0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {40, 170, 225, 22}, {40, 170, 225, 101}, {40, 170, 225, 156}, {40, 170, 225, 211}, 
  {40, 170, 225, 235}, {40, 170, 225, 253}, {40, 170, 225, 255}, {40, 170, 225, 246}, {40, 170, 225, 224}, {40, 170, 
  225, 184}, {40, 170, 225, 130}, {40, 170, 225, 60}, {40, 170, 225, 2}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 
  0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 
  0, 0}, {0, 0, 0, 0}}}], "Byte", ColorSpace -> "RGB", Interleaving -> True];


End[] (* End Private Context *)
           		
End[]


SetAttributes[{},{ReadProtected, Protected}];

System`Private`RestoreContextPath[];

(* Return three functions to define oauthservicedata, oauthcookeddata, oauthsendmessage  *)
{TwitterOAuth`Private`twitterdata,TwitterOAuth`Private`twittercookeddata,TwitterOAuth`Private`twittersendmessage}
