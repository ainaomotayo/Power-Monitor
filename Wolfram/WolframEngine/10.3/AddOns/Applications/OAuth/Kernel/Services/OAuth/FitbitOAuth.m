System`Private`NewContextPath[{"OAuthClient`","System`"}];

Begin["FitbitOAuth`"] (* Begin Private Context *) 

Begin["`Private`"](* Begin Private Context *) 

(******************************* Fitbit *************************************)

(* Authentication information *)

fitbitdata[]={
        "ServiceName"       -> "Fitbit",
        "OAuthVersion"		-> "1.0a",
        "RequestEndpoint"   -> "https://api.fitbit.com/oauth/request_token",
        "AccessEndpoint"    -> "https://api.fitbit.com/oauth/access_token",
        "AuthorizeEndpoint" -> "https://www.fitbit.com/oauth/authorize",
        "VerifierLabel"		->	"security code",
	 	"ClientInfo"		-> {"Wolfram","Token"},
	 	"AuthenticationDialog" :> (OAuthClient`tokenOAuthDialog[#, "Fitbit",fitbiticon]&),
 		"Gets"				-> Join[{OAuthClient`Private`gridRequests["UserDataGrid", "WeightGrid", "MeasurementsGrid", "FoodGrid"], "FoodList",
 			"ActivityData",	OAuthClient`Private`gridRequests["ActivityGrid", "SleepGrid"],"SleepData", "SleepList","SleepCalendar","SleepDensityTimeline","UserData"},
 			fitbittimeseriesproperties,fitbittimeseriesPlots],
 		"Posts"				-> {"RecordWeight"},
 		"RawGets"			->	Join[{"RawUserData", "RawWeight", "RawBodyFat", "RawMeasurements", 
				"RawFood", "RawWater", "RawActivity", "RawSleep", "RawFoodUnit"},
 			rawfitbittimeseriesproperties],
 		"RawPosts"			->	{"RawLogWeight","RawLogBodyFat","RawLogMeasurement","RawLogFood"},
    	"RequestFormat"		-> "Headers",
 		"Information"		-> "A service for sending and receiving data from a Fitbit account"
    }
    
(* a function for importing the raw data - usually json or xml - from the service *)
fitbitimport[$Failed]:=Throw[$Failed]
fitbitimport[json_String]:=With[{res=ImportString[json,"JSON"]},
	If[res===$Failed,Throw[$Failed]];
	If[FreeQ[res,_["errors",_]],
		Switch[res,
			_Rule|{_Rule...},Association@res,
			{{_Rule...}...},Association/@res,
			_,res
		],
		If[Quiet[MemberQ[("errorType" /. ("errors" /. res)), "oauth"]],
			 Message[ServiceExecute::reauth, "message" /. ("errors" /. res)],
			 Message[ServiceExecute::apierr, "message" /. ("errors" /. res)]
		];
		Throw[$Failed]
	]
]

fitbitimport[raw_]:=raw

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
 
     
fitbitdata["RawUserData"] = {
        "URL"				->	"https://api.fitbit.com/1/user/-/profile.json",
        "HTTPSMethod"		-> "GET",
       	"Headers" 			-> {"Accept-Language"->"en_US"},  (* Can be "en_US", "en_GB", or "" *)
        "ResultsFunction"	-> fitbitimport
    }
 
fitbitdata["RawMeasurements"] = {
        "URL"				->	(ToString@StringForm["https://api.fitbit.com/1/user/-/body/date/`1`.json", formatDate[##]]&),
       	"Headers" 			-> {"Accept-Language"->"en_US"},  (* Can be "en_US", "en_GB", or "" *)
        "PathParameters" 	-> {"Date"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> fitbitimport
    }
       
fitbitdata["RawWeight"] = {
        "URL"				->	(ToString@
        	StringForm["https://api.fitbit.com/1/user/-/body/log/weight/date/`1`.json", formatDate[##]]&),
       	"Headers" 			-> {"Accept-Language"->"en_US"},  (* Can be "en_US", "en_GB", or "" *)
        "PathParameters" 	-> {"Date"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> fitbitimport
    }

fitbitdata["RawBodyFat"] = {
        "URL"				->	(ToString@StringForm["https://api.fitbit.com/1/user/-/body/log/fat/date/`1`.json", formatDate[##]]&),
        "PathParameters" 	-> {"Date","StartDate","EndDate"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> fitbitimport
    }
           
fitbitdata["RawFood"] = {
        "URL"				->	(ToString@StringForm["https://api.fitbit.com/1/user/-/foods/log/date/`1`.json", formatDate[##]]&),
        "HTTPSMethod"		-> "GET",
        "PathParameters" 	-> {"Date","StartDate","EndDate"},
        "ResultsFunction"	-> fitbitimport
    }
           
fitbitdata["RawWater"] = {
        "URL"				->	(ToString@StringForm["https://api.fitbit.com/1/user/-/foods/log/water/date/`1`.json", formatDate[##]]&),
        "HTTPSMethod"		-> "GET",
        "PathParameters" 	-> {"Date"},
        "ResultsFunction"	-> fitbitimport
    }
    
fitbitdata["RawActivity"] = {
        "URL"				->	(ToString@StringForm["https://api.fitbit.com/1/user/-/activities/date/`1`.json", formatDate[##]]&),
        "PathParameters" 	-> {"Date"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> fitbitimport
    }
fitbitdata["RawSleep"] = {
        "URL"				->	(ToString@StringForm["https://api.fitbit.com/1/user/-/sleep/date/`1`.json", formatDate[##]]&),
        "PathParameters" 	-> {"Date"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	->fitbitimport
    } 
           
fitbitdata["RawFoodUnit"] = {
        "URL"				->	"https://api.fitbit.com/1/foods/units.json",
        "PathParameters" 	-> {},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	->fitbitimport
    } 
        
fitbitdata["RawLogFood"] = {
        "URL"				->	"https://api.fitbit.com/1/user/-/foods/log.json",
        "BodyData"			-> {"foodID","foodName","calories","brandName","mealTypeId","unitId","amount","date"},
   		"RequiredParameters"-> {"mealTypeId","unitId","amount","date"},
        "HTTPSMethod"		-> "POST",
        "ResultsFunction"	->fitbitimport
    } 
                   
fitbitdata["RawLogWeight"] = {
        "URL"				->	"https://api.fitbit.com/1/user/-/body/log/weight.json",
       	"Headers" 			-> {"Accept-Language"->"en_US"},  (* Can be "en_US", "en_GB", or "" *)
        "BodyData"			-> {"weight","date"},
   		"RequiredParameters"-> {"weight","date"},
        "HTTPSMethod"		-> "POST",
        "ResultsFunction"	->fitbitimport
    } 
             
fitbitdata["RawLogBodyFat"] = {
        "URL"				->	"https://api.fitbit.com/1/user/-/body/log/fat.json",
        "BodyData"			-> {"fat","date"},
   		"RequiredParameters"-> {"fat","date"},
        "HTTPSMethod"		-> "POST",
        "ResultsFunction"	->fitbitimport
    } 
    
fitbitdata["RawLogMeasurement"] = {
        "URL"				->	"https://api.fitbit.com/1/user/-/body.json",
       	"Headers" 			-> {"Accept-Language"->"en_US"},
        "BodyData"			-> {"bicep","calf","chest","fat","forearm","hips","neck","thigh","waist","weight","date"},
   		"RequiredParameters"-> {"bicep"|"calf"|"chest"|"fat"|"forearm"|"hips"|"neck"|"thigh"|"waist"|"weight"},
        "HTTPSMethod"		-> "POST",
        "ResultsFunction"	->fitbitimport
    } 
    
fitbittimeseriesproperties={"CaloriesInTimeSeries", "WaterTimeSeries", "CaloriesTimeSeries", 
   "CaloriesBMRTimeSeries", "StepsTimeSeries", "DistanceTimeSeries", 
   "FloorsTimeSeries", "ElevationTimeSeries", 
   "MinutesSedentaryTimeSeries", "MinutesLightlyActiveTimeSeries", 
   "MinutesFairlyActiveTimeSeries", "MinutesVeryActiveTimeSeries", 
   "ActivityCaloriesTimeSeries", (* "CaloriesTimeSeries", "StepsTimeSeries", 
   "DistanceTimeSeries", "MinutesSedentaryTimeSeries", 
   "MinutesLightlyActiveTimeSeries", "MinutesFairlyActiveTimeSeries", 
   "MinutesVeryActiveTimeSeries", "ActivityCaloriesTimeSeries",  *)
   "Bedtimes", "TimeInBedTimeSeries", 
   "MinutesAsleepTimeSeries", "AwakeningsCountTimeSeries", 
   "MinutesAwakeTimeSeries", "MinutesToFallAsleepTimeSeries", 
   "MinutesAfterWakeupTimeSeries", "SleepEfficiencyTimeSeries", 
   "WeightTimeSeries", "BMITimeSeries", "BodyFatTimeSeries"}
   
fitbittimeseriespaths={"foods/log/caloriesIn", "foods/log/water", "activities/calories", 
   "activities/caloriesBMR", "activities/steps", "activities/distance", 
   "activities/floors", "activities/elevation", 
   "activities/minutesSedentary", "activities/minutesLightlyActive", 
   "activities/minutesFairlyActive", "activities/minutesVeryActive", 
   "activities/activityCalories", (* "activities/tracker/calories" ,
   "activities/tracker/steps","activities/tracker/distance", 
   "activities/tracker/floors", "activities/tracker/elevation", 
    "activities/tracker/minutesSedentary", 
   "activities/tracker/minutesLightlyActive", 
  "activities/tracker/minutesFairlyActive", 
   "activities/tracker/minutesVeryActive", *)
 (*  "activities/tracker/activityCalories",*)"sleep/startTime", 
   "sleep/timeInBed", "sleep/minutesAsleep", "sleep/awakeningsCount", 
   "sleep/minutesAwake", "sleep/minutesToFallAsleep", 
   "sleep/minutesAfterWakeup", "sleep/efficiency", "body/weight", 
   "body/bmi", "body/fat"};

rawfitbittimeseriesproperties=("Raw"<>#&/@fitbittimeseriesproperties)

fitbittimeseriesPlots=DeleteCases[StringReplace[fitbittimeseriesproperties, "TimeSeries" -> "Plot"],"Bedtimes"]

fitbitdata[prop:(Alternatives@@rawfitbittimeseriesproperties)] := {
        "URL"				->	(ToString@StringForm["https://api.fitbit.com/1/user/-/"<>
        	(prop/.Thread[rawfitbittimeseriesproperties->fitbittimeseriespaths])<>"/date/`1`/`2`.json", formatDate[#1],formatDate[#2]]&),
        "PathParameters" 	-> {"StartDate", "EndDate"},
   		"RequiredParameters"-> {"StartDate", "EndDate"},
        "HTTPSMethod"		-> "GET",
       	"Headers" 			-> {"Accept-Language"->"en_US"},  (* Can be "en_US", "en_GB", or "" *)
        "ResultsFunction"	->fitbitimport
    } 
   
   
fitbitdata["icon"]=fitbiticon

fitbitdata[___]:=$Failed
(****** Cooked Properties ******)
  
(* cooked data queries 
	Cooked queries call at least one of the raw queried underneath but add pre and post processing to make the function easier to use and the result clearer.
*)  

fitbitcookeddata[prop_,id_,rule_Rule, rest___]:=fitbitcookeddata[prop,id,{rule}, rest]
fitbitcookeddata[prop_,id_]:=fitbitcookeddata[prop,id,{}]

fitbittimeseriesdata[prop_,id_,args_]:=Block[{rawprop, params,start, end,rawdata},
	rawprop="Raw"<>prop;
	params=filterparameters[args,getallparameters[rawprop]];
	If[FreeQ[params,#],Message[ServiceExecute::nparam,#1];Throw[$Failed]]&/@{"StartDate"};
	{start, end} = {"StartDate", "EndDate"} /. Flatten[{args}]/."EndDate"->DateString[];
	If[!TrueQ[DateDifferenceInDays[start, end] > 0], Throw[$Failed]];
	rawdata=OAuthClient`rawoauthdata[id,rawprop,Join[{"StartDate"->formatDate[start],"EndDate"->formatDate[end]},params]];
	rawdata=fitbitimport[rawdata];
	If[rawdata===$Failed,Return[$Failed]];
	rawdata
];

formattimeseriesdata[data_,_]:={"dateTime","value"}/.((data/.{("value"->val_):>("value"->ToExpression[val]),
		("dateTime"->date_):>("dateTime"->readDate[date,AbsoluteTime])}))

fitbitcookeddata["Bedtimes",id_,args_] :=Module[
	{data, dates, times},
	data=fitbittimeseriesdata["Bedtimes",id, args];
	data=Normal[data][[1,-1]];
	data=DeleteCases[data,{___,"value"->"",___}];
	dates="dateTime"/.data;
	times="value"/.data;
	MapThread[DateObject[#1<>" "<>#2]&,{dates,times}]
]

fitbitcookeddata[prop:(Alternatives@@fitbittimeseriesproperties),id_,args_] :=Module[
	{data},
	data=fitbittimeseriesdata[prop,id, args];
	data=formattimeseriesdata[Normal[data][[1,-1]],prop];
	TimeSeries[data]
]

fitbitcookeddata[prop:(Alternatives@@fitbittimeseriesPlots),id_,args_] :=Module[
	{data, opts},
	opts=FilterRules[args,Except["StartDate"|"EndDate"]];
	data=fitbittimeseriesdata[StringReplace[prop,"Plot"->"TimeSeries"],id, args];
	data=formattimeseriesdata[Normal[data][[1,-1]],prop];
	DateListPlot[data, Sequence@@opts, Filling->Axis]
]

fitbitsleeplist[id_,args_]:=Module[
	{rawdata, startDate, endDate, ndays, n, day},
	If[FreeQ[args,#],Message[ServiceExecute::nparam,#1];Throw[$Failed]]&/@{"StartDate"};
	{startDate, endDate} = {"StartDate", "EndDate"} /. Flatten[{args}]/."EndDate"->DateString[];
	If[TrueQ[DateDifferenceInDays[startDate, endDate] < 0], Throw[$Failed]];
	ndays = First[DateDifference[startDate, endDate, "Day"]];
	rawdata = Table[day = DatePlus[startDate, n];
 		OAuthClient`rawoauthdata[id, "RawSleep", "Date" -> day], {n, 0, ndays}];
	If[rawdata === {}, Return[{}]];
	rawdata=DeleteCases[(Normal@fitbitimport[#]&)/@rawdata,FreeQ[#,"sleep"]&];
	"sleep" /. rawdata
]

fitbitcookeddata["SleepList",id_,args_]:=Module[
	{rawdata},
	rawdata=fitbitsleeplist[id, args];
	Association/@(Map[Cases[#, _[
    "awakeningsCount" | "duration" | "efficiency" | 
     "minutesAfterWakeup" | "minutesAsleep" | "minutesAwake" | 
     "minutesToFallAsleep" | "startTime" | 
     "timeInBed", _], {1}] &, rawdata, {2}]/.{fval["startTime"->(readDate[#]&)],
     	quant["minutesAfterWakeup"->"Minutes"],quant["minutesAsleep"->"Minutes"],
     	quant["minutesAwake"->"Minutes"],quant["minutesToFallAsleep"->"Minutes"]
     	,quant["timeInBed"->"Minutes"],quant["duration"->"Seconds"]}/.HoldPattern[Rule[a_String,b_]]:>Rule[camelcase[a],b])
     
]
         
fitbitcookeddata["SleepCalendar",id_,args_]:=Module[
	{data, starts, durations, effs,minEfficiency,maxEfficiency,ndays, t0, i},
	data=fitbitsleeplist[id, args];
	If[Flatten[data]==={},Return[Graphics[]]];
	data=Cases[data,_?(! FreeQ[#, "startTime"] &)];
	starts = Check[readDate[#, Identity] & /@ 
   		Flatten[("startTime" /. data)],Throw[$Failed]];
	durations = Select[ToExpression[Flatten["timeInBed" /. data]], NumberQ];
	effs = Select[ToExpression[Flatten["efficiency" /. data]], NumberQ];
	minEfficiency = Min[effs];
	maxEfficiency = Max[effs];
	ndays = Ceiling[First[DateDifference["StartDate"/. Flatten[{args}], "EndDate"/. Flatten[{args}], "Day"]]];
	data=Transpose[{starts,durations, effs}];
	t0=Min[AbsoluteTime/@starts];
	Graphics[
		makeSleepBox[#, minEfficiency, maxEfficiency, t0]&/@data,
		AspectRatio -> 1/GoldenRatio,
		Axes -> True,
		AxesOrigin->{0,0},
		Ticks -> {Range[-2, 24], Table[{i+.5,DateString[DatePlus[t0,{i,"Day"}], {"MonthNameShort", " ", "Day", " ", "Year"}]},{i,0,ndays-1}]}
	]
]      


fitbitcookeddata[prop:("SleepData"|"SleepGrid"),id_,args_]:=Module[
	{rawdata,params,data},
	params=filterparameters[args,getallparameters["RawSleep"]];
	rawdata=OAuthClient`rawoauthdata[id,"RawSleep",params];
	data=fitbitimport[rawdata];
	data=Normal[data];
	If[data==={},Return[{}]];
	Switch[prop,
		"SleepGrid",
		OAuthClient`Private`addtitles[Replace[flatgrid[{
	        	{"sleep",{"efficiency","startTime","duration","minutesAsleep","minutesAwake","minutesAfterWakeup","awakeningsCount","timeInBed"}},
	        	{"summary",{"totalSleepRecords","totalMinutesAsleep","totalTimeInBed"}}},
	        	data,
	        	{fval["startTime"->(readDate[#]&)],quant["minutesAsleep"->"Minutes"],
	        	quant["minutesAwake"->"Minutes"],quant["minutesAfterWakeup"->"Minutes"],quant["timeInBed"->"Minutes"]}],{a_} -> a, {3}],
	        	{"Parameter","Value"}],
	   	"SleepData",
	   	If[data=!={},
	    		data=data/.{HoldPattern["sleep"->{sl___}]:>sl,HoldPattern["summary"->{su___}]:>su};
	    		data=FilterRules[data, {"efficiency","startTime","duration","minutesAsleep","minutesAwake","minutesAfterWakeup","awakeningsCount",
	    			"timeInBed","totalSleepRecords","totalMinutesAsleep","totalTimeInBed"}];
	    		Association@Replace[data, HoldPattern[Rule[a_, b_]] :> Rule[camelcase[a], b], Infinity]
	    		,
	    		{}
	    		
	    	]
	]
]    
 
         
fitbitcookeddata["SleepDensityTimeline",id_,args_]:=Module[
	{data, starts, durations, effs,minEfficiency,maxEfficiency,ndays, t0},
	data=fitbitsleeplist[id, args];
	data=DeleteCases[data,{}];
	starts =readDate[#,Identity] & /@ 
   		 DeleteCases[Flatten[("startTime" /. data)],"startTime"];
	durations = Select[ToExpression[Flatten["timeInBed" /. data]], NumberQ];
	effs = Select[ToExpression[Flatten["efficiency" /. data]], NumberQ];
	minEfficiency = Min[effs];
	maxEfficiency = Max[effs];
	ndays = Ceiling[First[DateDifference["StartDate"/. Flatten[{args}], "EndDate"/. Flatten[{args}], "Day"]]];
	data=Transpose[{starts,durations, effs}];
	t0=Min[AbsoluteTime/@starts];
	Graphics[
		makeSleepBox[#, minEfficiency, maxEfficiency, AbsoluteTime[#[[1,1;;3]]],True]&/@data,
		Axes -> True,
		AxesOrigin->{0,0},
		Ticks -> {Range[-2, 24], {1}}
	]
]


fitbitcookeddata[prop:("UserDataGrid"|"UserData"),id_,args_]:=Module[
	{rawdata,params,data, grid},
	params=filterparameters[args,getallparameters["RawMeasurements"]];
	rawdata=OAuthClient`rawoauthdata[id,"RawUserData",params];
	data=Normal@fitbitimport[rawdata];
	data=data/.{fval["dateOfBirth"->(readDate[#]&)],fval["memberSince"->(readDate[#]&)],
        	quant["height"->"Inches"],quant["strideLengthRunning"->"Inches"],quant["strideLengthWalking"->"Inches"],
        	quant["weight"->"Pounds"]};
    If[prop==="UserData",
    	Association[("user"/.data)/.HoldPattern[Rule[a_String,b_]]:>Rule[camelcase[a],b]]
    	,	
    	grid=flatgrid[{{"user",{"city", "country", "dateOfBirth", "displayName", "gender", "height", "locale", 
        	"memberSince", "state", "strideLengthRunning","strideLengthWalking", "timezone", "weight"}}},
        	data];
		OAuthClient`Private`addtitles[grid,{"User Data",SpanFromLeft}]
    ]
]
   
fitbitcookeddata["WeightGrid",id_,args_]:=Module[
	{rawdata,params,data,date},
	params=filterparameters[Join[args,{"Date"->DateString[]}],getallparameters["RawWeight"]];
	rawdata=OAuthClient`rawoauthdata[id,"RawWeight",params];
	data=fitbitimport[rawdata];
	data=data["weight"];
	If[data==={},Return[Missing["NotAvailable"]]];
	data=First[data];
	date=Check[DateObject[DateList@StringJoin[{"date"," ","time"}/.data]],""];
	data=FilterRules[data,Except["date"|"time"]];
	OAuthClient`Private`addtitles[flatgrid[{{"date","bmi","weight"}},Join[{"date"->date},data]/.{quant["weight"->"Pounds"]}],{"Weight",SpanFromLeft}]
]    

fitbitcookeddata["MeasurementsGrid",id_,args_]:=Module[
	{rawdata,params,data},
	params=filterparameters[Join[args,{"Date"->DateString[]}],getallparameters["RawMeasurements"]];
	rawdata=OAuthClient`rawoauthdata[id,"RawMeasurements",params];
	data=fitbitimport[rawdata];
	OAuthClient`Private`addtitles[flatgrid[{"bicep", "bmi", "calf", "chest", "fat", 
		"forearm", "hips", "neck", "thigh", "waist", "weight"},data["body"],
		Join[{quant["weight"->"Pounds"]},quant[#->"Inches"]&/@{"chest","calf","bicep","forearm","hips","neck","thigh","waist"}]],
			{"Measurements","Date"/.params/."Date"->""}]
]    
    
fitbitcookeddata[prop:("FoodGrid"|"FoodList"),id_,args_]:=Module[
	{rawdata,params,data},
	params=filterparameters[args,getallparameters["RawFood"]];
	rawdata=OAuthClient`rawoauthdata[id,"RawFood",Join[params,{"Date"->DateString[]}]];
	data=fitbitimport[rawdata];
	data=Lookup[data,"foods",{}];
	Switch[prop,
		"FoodGrid",
		OAuthClient`Private`gridList[{{"loggedFood",{"amount", "brand", "foodId", "mealTypeId", "name"}},
	        	{"nutritionalValues",{"calories", "carbs", "fat", "fiber", "protein", "sodium"}}},data],
	    "FoodList",
	    	If[data=!={},
	    		data=data/.HoldPattern["loggedFood"->{lf___}]:>lf;
	    		data=FilterRules[#, {"logId", "logDate", "nutritionalValues", "amount","foodID","unit","brand","calories","mealTypeId","name","units"}]&/@data;
	    		Association/@Replace[data, HoldPattern[Rule[a_, b_]] :> Rule[camelcase[a], b], Infinity]
	    		,
	    		{}
	    		
	    	]
	    	
	        	
	]
]   


fitbitcookeddata[prop:("ActivityData"|"ActivityGrid"),id_,args_]:=Module[
	{rawdata,data,params,data1,data2,fields},
	fields={"activityCalories","caloriesBMR","caloriesOut","fairlyActiveMinutes", 
		"lightlyActiveMinutes", "marginalCalories", "sedentaryMinutes", "steps", "veryActiveMinutes"};
	params=filterparameters[args,getallparameters["RawActivity"]];
	rawdata=OAuthClient`rawoauthdata[id,"RawActivity",params];
	data=fitbitimport[rawdata];
	data=data["summary"];
	data1=(camelcase[#]->OAuthClient`Private`getdata[data,#,Null])&/@fields;
	data2=parsedistances[OAuthClient`Private`getdata[data,"distances",{}]];
	data=Join[data1,data2];
	Switch[prop,
		"ActivityGrid",
		OAuthClient`Private`addtitles[OAuthClient`Private`prettygrid[List@@@data],{"Activity Data","Date"/.params/."Date"->DateString[{"Year","/","Month","/","Day"}]}],
		"ActivityData",
		Association@data	
	]
]   

parsedistances[dists_]:=Cases[l:{___,HoldPattern[Rule["activity",act_]],___}:>(camelcase[act]->"distance"/.l),Infinity]

(* depricated 
fitbitcookeddata["TimeSeriesActivityScores",id_,args_]:=Module[
	{rawdata},
	Switch[Length[{args}],
		1,
		rawdata=OAuthClient`rawoauthdata[id,"AllActivityScores",args],
		2,
		rawdata=OAuthClient`rawoauthdata[id,"ActivityScores",args]
	];
	TemporalData[{"dateTime", "value"} /. ("activities-activeScore" /. (
        	ImportString[rawdata, "JSON"]/.{fval["dateTime"->DateList],fval["value"->ToExpression]}))]
]   
fitbitcookeddata["TimeSeriesActivityCalories",id_,args_]:=Module[
	{rawdata,params},
	params=filterparameters[args,getallparameters["RawFood"]];
	rawdata=OAuthClient`rawoauthdata[id,"AllActivityCalories",params];
	TemporalData[{"dateTime", "value"} /. ("activities-activityCalories" /. (
        	ImportString[rawdata, "JSON"]/.{fval["dateTime"->DateList],fval["value"->ToExpression]}))]
]   
 
  *)
fitbitcookeddata["RecordWeight",id_,args_]:=Module[
	{rawdata, params,data},
	params=filterparameters[Join[args,{"date"->DateString[]}],getallparameters["RawLogWeight"]];
	params=params/.HoldPattern[Rule[a_,b_?(!StringQ[#]&)]]:>Rule[a,ToString[b]];
	params=params/.HoldPattern[Rule["date",date_]]:>Rule["date",formatDate[date]];
	rawdata=OAuthClient`rawoauthdata[id,"RawLogWeight",params];
	data=fitbitimport[rawdata];
	OAuthClient`Private`getdata[data["weightLog"],"weight"]
]    

makeSleepBox[data_, minEfficiency_, maxEfficiency_, t0_, timelineQ_:False] :=
	Module[{day, duration, start, efficiency, xmin, xmax, end, tQ=Boole[timelineQ]},
		day = data[[1,1;;3]];
		start = data[[1]];
		xmin = First@DateDifference[day, start, "Hour"];
		duration = data[[2]];
		end=DatePlus[start,{duration,"Minute"}];
		xmax = First@DateDifference[day, end, "Hour"];
		efficiency = Rescale[data[[3]], {maxEfficiency, minEfficiency}];
		day=(AbsoluteTime[day]-t0)/86400;
		{
			Opacity[0.95-tQ/2],
			ColorData["TemperatureMap"][efficiency],
			Tooltip[
				If[xmax>24,
					If[xmax>48, Throw[$Failed]];
					{Rectangle[{0, day+1.95-tQ}, {xmax-24, day + 1.05-tQ}],
					Rectangle[{xmin, day+.95}, {24, day + 0.05}]}
					,
					Rectangle[{xmin, day+.95}, {xmax, day + 0.05}]
				],
				ToString@DateDifference[start, end, "Hour"]
			]
		}
	]

fitbitcookeddata[args___]:=($Failed )

(* Send Message *)
fitbitsendmessage[___]:=$Failed

(*** Service specific utilites ****)
quant=OAuthClient`Private`createquantity;
fval=OAuthClient`Private`formatvalue;
filterparameters=OAuthClient`Private`filterParameters;
camelcase=OAuthClient`Private`camelCase;
flatgrid=OAuthClient`Private`flatGrid;

DateDifferenceInDays[args___]:=Block[{DataPaclets`CalendarDataDump`$DateDifferenceQuantity = False},
	DateDifference[args]
]


getallparameters[str_]:=DeleteCases[Flatten[{"Parameters","PathParameters","BodyData","MultipartData"}/.fitbitdata[str]],
	("Parameters"|"PathParameters"|"BodyData"|"MultipartData")]

readDate[date_,form_:DateObject]:=form[DateList[{date,{"Year", "-", "Month", "-", "Day"}}]]/;StringFreeQ[date,"T"]
readDate[date_,form_:DateObject]:=form[DateList[{date,{"Year", "-", "Month", "-", "Day", "T", "Hour", ":", "Minute", ":","Second", ".", "Millisecond"}}]]


formatDate[]:=formatDate[DateList[]]
formatDate[per:("1d"|"7d"|"30d"|"1w"|"1m"|"3m"|"6m"|"1y"|"max"|"today")]:=per
formatDate[date_]:=Quiet[DateString[DateList[date], {"Year", "-", "Month", "-", "Day"}],DateList::arg]
formatDate[dates__]:=StringJoin[Riffle[formatDate[#]&/@{dates},"/"]]


fitbiticon=Image[RawArray["Byte", {{{255, 255, 255, 0}, {255, 255, 255, 3}, {255, 255, 255, 0}, {255, 255, 255, 15}, {254, 254, 
  254, 94}, {253, 253, 253, 170}, {253, 253, 253, 220}, {253, 253, 253, 246}, {253, 253, 253, 255}, {253, 253, 253, 
  255}, {253, 253, 253, 255}, {253, 253, 253, 255}, {253, 253, 253, 255}, {253, 253, 253, 255}, {253, 253, 253, 255}, 
  {253, 253, 253, 255}, {253, 253, 253, 255}, {253, 253, 253, 255}, {253, 253, 253, 255}, {253, 253, 253, 255}, {253, 
  253, 253, 255}, {253, 253, 253, 255}, {253, 253, 253, 255}, {253, 253, 253, 255}, {253, 253, 253, 246}, {253, 253, 
  253, 220}, {253, 253, 253, 170}, {254, 254, 254, 94}, {255, 255, 255, 15}, {255, 255, 255, 0}, {255, 255, 255, 3}, 
  {255, 255, 255, 0}}, {{255, 255, 255, 3}, {255, 255, 255, 0}, {254, 254, 254, 76}, {252, 252, 252, 229}, {252, 252, 
  252, 255}, {252, 252, 252, 255}, {252, 252, 252, 255}, {252, 252, 252, 255}, {252, 252, 252, 255}, {252, 252, 252, 
  255}, {252, 252, 252, 255}, {252, 252, 252, 255}, {252, 252, 252, 255}, {252, 252, 252, 255}, {252, 252, 252, 255}, 
  {252, 252, 252, 255}, {252, 252, 252, 255}, {252, 252, 252, 255}, {252, 252, 252, 255}, {252, 252, 252, 255}, {252, 
  252, 252, 255}, {252, 252, 252, 255}, {252, 252, 252, 255}, {252, 252, 252, 255}, {252, 252, 252, 255}, {252, 252, 
  252, 255}, {252, 252, 252, 255}, {252, 252, 252, 255}, {252, 252, 252, 229}, {254, 254, 254, 76}, {255, 255, 255, 
  0}, {255, 255, 255, 3}}, {{255, 255, 255, 0}, {254, 254, 254, 73}, {251, 251, 251, 254}, {251, 251, 251, 255}, 
  {251, 251, 251, 250}, {251, 251, 251, 252}, {251, 251, 251, 254}, {251, 251, 251, 255}, {251, 251, 251, 255}, {251, 
  251, 251, 255}, {251, 251, 251, 255}, {251, 251, 251, 255}, {251, 251, 251, 255}, {251, 251, 251, 255}, {251, 251, 
  251, 255}, {249, 251, 251, 255}, {249, 251, 251, 255}, {251, 251, 251, 255}, {251, 251, 251, 255}, {251, 251, 251, 
  255}, {251, 251, 251, 255}, {251, 251, 251, 255}, {251, 251, 251, 255}, {251, 251, 251, 255}, {251, 251, 251, 255}, 
  {251, 251, 251, 254}, {251, 251, 251, 252}, {251, 251, 251, 250}, {251, 251, 251, 255}, {251, 251, 251, 254}, {254, 
  254, 254, 73}, {255, 255, 255, 0}}, {{255, 255, 255, 13}, {251, 251, 251, 234}, {250, 250, 250, 255}, {251, 251, 
  251, 252}, {251, 251, 251, 255}, {251, 251, 251, 255}, {251, 251, 251, 255}, {251, 251, 251, 255}, {251, 251, 251, 
  255}, {251, 251, 251, 255}, {251, 251, 251, 255}, {251, 251, 251, 255}, {251, 251, 251, 255}, {250, 250, 250, 255}, 
  {251, 251, 251, 255}, {255, 254, 254, 255}, {255, 254, 254, 255}, {251, 251, 251, 255}, {250, 250, 250, 255}, {251, 
  251, 251, 255}, {251, 251, 251, 255}, {251, 251, 251, 255}, {251, 251, 251, 255}, {251, 251, 251, 255}, {251, 251, 
  251, 255}, {251, 251, 251, 255}, {251, 251, 251, 255}, {251, 251, 251, 255}, {251, 251, 251, 252}, {250, 250, 250, 
  255}, {251, 251, 251, 234}, {255, 255, 255, 13}}, {{253, 253, 253, 93}, {249, 249, 249, 255}, {250, 250, 250, 251}, 
  {250, 250, 250, 255}, {250, 250, 250, 255}, {250, 250, 250, 255}, {250, 250, 250, 255}, {250, 250, 250, 255}, {250, 
  250, 250, 255}, {250, 250, 250, 255}, {250, 250, 250, 255}, {250, 250, 250, 255}, {249, 250, 250, 255}, {250, 250, 
  250, 255}, {250, 250, 250, 255}, {180, 229, 229, 255}, {180, 229, 229, 255}, {250, 250, 250, 255}, {250, 250, 250, 
  255}, {249, 250, 250, 255}, {250, 250, 250, 255}, {250, 250, 250, 255}, {250, 250, 250, 255}, {250, 250, 250, 255}, 
  {250, 250, 250, 255}, {250, 250, 250, 255}, {250, 250, 250, 255}, {250, 250, 250, 255}, {250, 250, 250, 255}, {250, 
  250, 250, 251}, {249, 249, 249, 255}, {253, 253, 253, 93}}, {{251, 251, 251, 172}, {248, 248, 248, 254}, {249, 249, 
  249, 252}, {249, 249, 249, 255}, {249, 249, 249, 255}, {249, 249, 249, 255}, {249, 249, 249, 255}, {249, 249, 249, 
  255}, {249, 249, 249, 255}, {249, 249, 249, 255}, {249, 249, 249, 255}, {249, 249, 249, 255}, {246, 248, 248, 255}, 
  {255, 252, 252, 255}, {163, 223, 223, 255}, {45, 188, 188, 255}, {45, 188, 188, 255}, {163, 223, 223, 255}, {255, 
  252, 252, 255}, {246, 248, 248, 255}, {249, 249, 249, 255}, {249, 249, 249, 255}, {249, 249, 249, 255}, {249, 249, 
  249, 255}, {249, 249, 249, 255}, {249, 249, 249, 255}, {249, 249, 249, 255}, {249, 249, 249, 255}, {249, 249, 249, 
  255}, {249, 249, 249, 252}, {248, 248, 248, 254}, {251, 251, 251, 172}}, {{248, 248, 248, 222}, {248, 248, 248, 
  255}, {248, 248, 248, 254}, {248, 248, 248, 255}, {247, 247, 247, 255}, {248, 248, 248, 255}, {248, 248, 248, 255}, 
  {248, 248, 248, 255}, {248, 248, 248, 255}, {248, 248, 248, 255}, {248, 248, 248, 255}, {248, 248, 248, 255}, {244, 
  246, 246, 255}, {255, 251, 251, 255}, {134, 214, 214, 255}, {43, 188, 188, 255}, {43, 188, 188, 255}, {134, 214, 
  214, 255}, {255, 251, 251, 255}, {244, 246, 246, 255}, {247, 247, 247, 255}, {248, 248, 248, 255}, {248, 248, 248, 
  255}, {248, 248, 248, 255}, {248, 248, 248, 255}, {248, 248, 248, 255}, {248, 248, 248, 255}, {248, 248, 248, 255}, 
  {248, 248, 248, 255}, {248, 248, 248, 254}, {247, 247, 247, 255}, {248, 248, 248, 222}}, {{246, 246, 246, 248}, 
  {246, 246, 246, 255}, {246, 246, 246, 255}, {246, 246, 246, 255}, {246, 246, 246, 255}, {246, 246, 246, 255}, {246, 
  246, 246, 255}, {246, 246, 246, 255}, {246, 246, 246, 255}, {246, 246, 246, 255}, {244, 246, 246, 255}, {243, 246, 
  246, 255}, {245, 246, 246, 255}, {250, 248, 248, 255}, {227, 241, 241, 255}, {112, 207, 207, 255}, {112, 207, 207, 
  255}, {227, 241, 241, 255}, {250, 248, 248, 255}, {245, 245, 245, 255}, {243, 243, 243, 255}, {244, 244, 244, 255}, 
  {246, 246, 246, 255}, {246, 246, 246, 255}, {246, 246, 246, 255}, {246, 246, 246, 255}, {246, 246, 246, 255}, {246, 
  246, 246, 255}, {246, 246, 246, 255}, {246, 246, 246, 255}, {246, 246, 246, 255}, {246, 246, 246, 248}}, {{245, 
  245, 245, 255}, {245, 245, 245, 255}, {245, 245, 245, 255}, {245, 245, 245, 255}, {245, 245, 245, 255}, {245, 245, 
  245, 255}, {245, 245, 245, 255}, {245, 245, 245, 255}, {245, 245, 245, 255}, {246, 245, 245, 255}, {255, 249, 249, 
  255}, {255, 249, 249, 255}, {247, 246, 246, 255}, {244, 245, 245, 255}, {251, 247, 247, 255}, {255, 251, 251, 255}, 
  {255, 251, 251, 255}, {251, 247, 247, 255}, {244, 244, 244, 255}, {248, 247, 247, 255}, {255, 255, 255, 255}, {255, 
  255, 255, 255}, {246, 246, 246, 255}, {245, 245, 245, 255}, {245, 245, 245, 255}, {245, 245, 245, 255}, {245, 245, 
  245, 255}, {245, 245, 245, 255}, {245, 245, 245, 255}, {245, 245, 245, 255}, {245, 245, 245, 255}, {245, 245, 245, 
  255}}, {{244, 244, 244, 255}, {244, 244, 244, 255}, {244, 244, 244, 255}, {244, 244, 244, 255}, {244, 244, 244, 
  255}, {244, 244, 244, 255}, {244, 244, 244, 255}, {244, 244, 244, 255}, {244, 244, 244, 255}, {245, 244, 244, 255}, 
  {168, 223, 223, 255}, {155, 219, 219, 255}, {237, 242, 242, 255}, {246, 245, 245, 255}, {240, 243, 243, 255}, {159, 
  220, 220, 255}, {159, 220, 220, 255}, {240, 242, 242, 255}, {246, 246, 246, 255}, {236, 237, 237, 255}, {149, 149, 
  149, 255}, {164, 164, 164, 255}, {245, 245, 245, 255}, {244, 244, 244, 255}, {244, 244, 244, 255}, {244, 244, 244, 
  255}, {244, 244, 244, 255}, {244, 244, 244, 255}, {244, 244, 244, 255}, {244, 244, 244, 255}, {244, 244, 244, 255}, 
  {244, 244, 244, 255}}, {{243, 243, 243, 255}, {243, 243, 243, 255}, {243, 243, 243, 255}, {243, 243, 243, 255}, 
  {243, 243, 243, 255}, {243, 243, 243, 255}, {243, 243, 243, 255}, {240, 242, 242, 255}, {254, 246, 246, 255}, {167, 
  222, 222, 255}, {45, 188, 188, 255}, {43, 188, 188, 255}, {134, 213, 213, 255}, {255, 249, 249, 255}, {149, 217, 
  217, 255}, {44, 188, 188, 255}, {43, 188, 188, 255}, {149, 215, 215, 255}, {255, 255, 255, 255}, {127, 129, 129, 
  255}, {31, 31, 31, 255}, {33, 33, 33, 255}, {163, 163, 163, 255}, {254, 254, 254, 255}, {240, 240, 240, 255}, {243, 
  243, 243, 255}, {243, 243, 243, 255}, {243, 243, 243, 255}, {243, 243, 243, 255}, {243, 243, 243, 255}, {243, 243, 
  243, 255}, {243, 243, 243, 255}}, {{241, 241, 241, 255}, {241, 241, 241, 255}, {241, 241, 241, 255}, {241, 241, 
  241, 255}, {241, 241, 241, 255}, {241, 241, 241, 255}, {241, 241, 241, 255}, {238, 240, 240, 255}, {255, 245, 245, 
  255}, {152, 217, 217, 255}, {43, 188, 188, 255}, {45, 188, 188, 255}, {119, 208, 208, 255}, {255, 248, 248, 255}, 
  {134, 212, 212, 255}, {44, 188, 188, 255}, {44, 188, 188, 255}, {134, 211, 211, 255}, {255, 255, 255, 255}, {111, 
  113, 113, 255}, {32, 32, 32, 255}, {31, 31, 31, 255}, {148, 148, 148, 255}, {255, 255, 255, 255}, {238, 238, 238, 
  255}, {241, 241, 241, 255}, {241, 241, 241, 255}, {241, 241, 241, 255}, {241, 241, 241, 255}, {241, 241, 241, 255}, 
  {241, 241, 241, 255}, {241, 241, 241, 255}}, {{240, 240, 240, 255}, {240, 240, 240, 255}, {240, 240, 240, 255}, 
  {240, 240, 240, 255}, {240, 240, 240, 255}, {237, 237, 237, 255}, {237, 237, 237, 255}, {238, 239, 239, 255}, {242, 
  241, 241, 255}, {233, 238, 238, 255}, {132, 209, 209, 255}, {118, 205, 205, 255}, {220, 234, 234, 255}, {245, 242, 
  242, 255}, {227, 236, 236, 255}, {124, 207, 207, 255}, {124, 207, 207, 255}, {226, 235, 235, 255}, {245, 245, 245, 
  255}, {219, 219, 219, 255}, {110, 110, 110, 255}, {126, 126, 126, 255}, {233, 233, 233, 255}, {242, 242, 242, 255}, 
  {238, 238, 238, 255}, {237, 237, 237, 255}, {237, 237, 237, 255}, {240, 240, 240, 255}, {240, 240, 240, 255}, {240, 
  240, 240, 255}, {240, 240, 240, 255}, {240, 240, 240, 255}}, {{239, 239, 239, 255}, {239, 239, 239, 255}, {239, 
  239, 239, 255}, {238, 238, 238, 255}, {239, 239, 239, 255}, {250, 250, 250, 255}, {252, 252, 252, 255}, {243, 243, 
  243, 255}, {237, 237, 237, 255}, {243, 241, 241, 255}, {255, 254, 254, 255}, {255, 255, 255, 255}, {246, 243, 243, 
  255}, {236, 237, 237, 255}, {245, 242, 242, 255}, {255, 254, 254, 255}, {255, 254, 254, 255}, {245, 242, 242, 255}, 
  {236, 237, 237, 255}, {247, 247, 247, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {243, 243, 243, 255}, {237, 
  237, 237, 255}, {243, 243, 243, 255}, {252, 252, 252, 255}, {250, 250, 250, 255}, {239, 239, 239, 255}, {239, 239, 
  239, 255}, {239, 239, 239, 255}, {239, 239, 239, 255}, {239, 239, 239, 255}}, {{237, 237, 237, 255}, {237, 237, 
  237, 255}, {237, 237, 237, 255}, {237, 237, 237, 255}, {239, 239, 239, 255}, {152, 152, 152, 255}, {124, 124, 124, 
  255}, {218, 218, 218, 255}, {242, 242, 242, 255}, {234, 234, 234, 255}, {141, 143, 143, 255}, {127, 129, 129, 255}, 
  {224, 225, 225, 255}, {242, 242, 242, 255}, {229, 230, 230, 255}, {134, 136, 136, 255}, {134, 136, 136, 255}, {229, 
  230, 230, 255}, {242, 242, 242, 255}, {223, 223, 223, 255}, {127, 127, 127, 255}, {142, 142, 142, 255}, {234, 234, 
  234, 255}, {242, 242, 242, 255}, {218, 218, 218, 255}, {124, 124, 124, 255}, {152, 152, 152, 255}, {239, 239, 239, 
  255}, {237, 237, 237, 255}, {237, 237, 237, 255}, {237, 237, 237, 255}, {237, 237, 237, 255}}, {{236, 236, 236, 
  255}, {236, 236, 236, 255}, {233, 233, 233, 255}, {248, 248, 248, 255}, {167, 167, 167, 255}, {34, 34, 34, 255}, 
  {32, 32, 32, 255}, {101, 101, 101, 255}, {255, 255, 255, 255}, {149, 149, 149, 255}, {32, 32, 32, 255}, {32, 32, 
  32, 255}, {117, 117, 117, 255}, {255, 255, 255, 255}, {133, 133, 133, 255}, {31, 31, 31, 255}, {31, 31, 31, 255}, 
  {133, 133, 133, 255}, {255, 255, 255, 255}, {116, 116, 116, 255}, {32, 32, 32, 255}, {32, 32, 32, 255}, {150, 150, 
  150, 255}, {255, 255, 255, 255}, {101, 101, 101, 255}, {32, 32, 32, 255}, {34, 34, 34, 255}, {167, 167, 167, 255}, 
  {248, 248, 248, 255}, {233, 233, 233, 255}, {236, 236, 236, 255}, {236, 236, 236, 255}}, {{234, 234, 234, 255}, 
  {234, 234, 234, 255}, {232, 232, 232, 255}, {246, 246, 246, 255}, {166, 166, 166, 255}, {34, 34, 34, 255}, {33, 33, 
  33, 255}, {100, 100, 100, 255}, {255, 255, 255, 255}, {148, 148, 148, 255}, {32, 32, 32, 255}, {32, 32, 32, 255}, 
  {116, 116, 116, 255}, {255, 255, 255, 255}, {132, 132, 132, 255}, {32, 32, 32, 255}, {32, 32, 32, 255}, {132, 132, 
  132, 255}, {255, 255, 255, 255}, {115, 115, 115, 255}, {32, 32, 32, 255}, {32, 32, 32, 255}, {149, 149, 149, 255}, 
  {255, 255, 255, 255}, {100, 100, 100, 255}, {33, 33, 33, 255}, {34, 34, 34, 255}, {166, 166, 166, 255}, {246, 246, 
  246, 255}, {232, 232, 232, 255}, {235, 235, 235, 255}, {234, 234, 234, 255}}, {{233, 233, 233, 255}, {233, 233, 
  233, 255}, {233, 233, 233, 255}, {233, 233, 233, 255}, {235, 235, 235, 255}, {149, 149, 149, 255}, {121, 121, 121, 
  255}, {214, 214, 214, 255}, {239, 238, 238, 255}, {229, 230, 230, 255}, {139, 140, 140, 255}, {125, 126, 126, 255}, 
  {219, 220, 220, 255}, {238, 238, 238, 255}, {225, 225, 225, 255}, {131, 133, 133, 255}, {131, 133, 133, 255}, {225, 
  225, 225, 255}, {238, 238, 238, 255}, {219, 219, 219, 255}, {125, 125, 125, 255}, {139, 139, 139, 255}, {229, 229, 
  229, 255}, {238, 238, 238, 255}, {214, 214, 214, 255}, {121, 121, 121, 255}, {149, 149, 149, 255}, {235, 235, 235, 
  255}, {233, 233, 233, 255}, {233, 233, 233, 255}, {233, 233, 233, 255}, {233, 233, 233, 255}}, {{232, 232, 232, 
  255}, {232, 232, 232, 255}, {232, 232, 232, 255}, {231, 231, 231, 255}, {232, 232, 232, 255}, {243, 243, 243, 255}, 
  {245, 245, 245, 255}, {236, 236, 236, 255}, {230, 230, 230, 255}, {237, 234, 234, 255}, {253, 246, 246, 255}, {253, 
  247, 247, 255}, {239, 236, 236, 255}, {229, 230, 230, 255}, {237, 235, 235, 255}, {255, 247, 247, 255}, {255, 247, 
  247, 255}, {237, 235, 235, 255}, {229, 229, 229, 255}, {240, 240, 240, 255}, {254, 254, 254, 255}, {253, 253, 253, 
  255}, {237, 237, 237, 255}, {230, 230, 230, 255}, {236, 236, 236, 255}, {245, 245, 245, 255}, {243, 243, 243, 255}, 
  {232, 232, 232, 255}, {231, 231, 231, 255}, {232, 232, 232, 255}, {232, 232, 232, 255}, {232, 232, 232, 255}}, 
  {{230, 230, 230, 255}, {230, 230, 230, 255}, {230, 230, 230, 255}, {230, 230, 230, 255}, {230, 230, 230, 255}, 
  {228, 228, 228, 255}, {227, 227, 227, 255}, {228, 229, 229, 255}, {234, 231, 231, 255}, {216, 227, 227, 255}, {113, 
  202, 202, 255}, {109, 201, 201, 255}, {210, 225, 225, 255}, {238, 232, 232, 255}, {218, 227, 227, 255}, {121, 204, 
  204, 255}, {121, 204, 204, 255}, {217, 226, 226, 255}, {239, 236, 236, 255}, {204, 205, 205, 255}, {97, 97, 97, 
  255}, {111, 111, 111, 255}, {219, 219, 219, 255}, {233, 233, 233, 255}, {228, 228, 228, 255}, {227, 227, 227, 255}, 
  {228, 228, 228, 255}, {230, 230, 230, 255}, {230, 230, 230, 255}, {230, 230, 230, 255}, {230, 230, 230, 255}, {230, 
  230, 230, 255}}, {{229, 229, 229, 255}, {229, 229, 229, 255}, {229, 229, 229, 255}, {229, 229, 229, 255}, {229, 
  229, 229, 255}, {229, 229, 229, 255}, {229, 229, 229, 255}, {226, 228, 228, 255}, {241, 231, 231, 255}, {129, 207, 
  207, 255}, {45, 189, 189, 255}, {46, 189, 189, 255}, {117, 205, 205, 255}, {252, 234, 234, 255}, {129, 208, 208, 
  255}, {44, 189, 189, 255}, {44, 189, 189, 255}, {129, 206, 206, 255}, {251, 242, 242, 255}, {100, 102, 102, 255}, 
  {35, 35, 35, 255}, {32, 32, 32, 255}, {133, 133, 133, 255}, {242, 242, 242, 255}, {226, 226, 226, 255}, {229, 229, 
  229, 255}, {229, 229, 229, 255}, {229, 229, 229, 255}, {229, 229, 229, 255}, {229, 229, 229, 255}, {229, 229, 229, 
  255}, {229, 229, 229, 255}}, {{228, 228, 228, 255}, {228, 228, 228, 255}, {228, 228, 228, 255}, {228, 228, 228, 
  255}, {228, 228, 228, 255}, {228, 228, 228, 255}, {228, 228, 228, 255}, {225, 227, 227, 255}, {238, 230, 230, 255}, 
  {146, 210, 210, 255}, {46, 190, 190, 255}, {45, 189, 189, 255}, {134, 208, 208, 255}, {249, 232, 232, 255}, {142, 
  210, 210, 255}, {45, 190, 190, 255}, {45, 190, 190, 255}, {142, 208, 208, 255}, {249, 240, 240, 255}, {118, 120, 
  120, 255}, {32, 32, 32, 255}, {35, 35, 35, 255}, {150, 150, 150, 255}, {239, 239, 239, 255}, {225, 225, 225, 255}, 
  {228, 228, 228, 255}, {228, 228, 228, 255}, {228, 228, 228, 255}, {228, 228, 228, 255}, {228, 228, 228, 255}, {228, 
  228, 228, 255}, {228, 228, 228, 255}}, {{226, 226, 226, 255}, {226, 226, 226, 255}, {226, 226, 226, 255}, {226, 
  226, 226, 255}, {226, 226, 226, 255}, {226, 226, 226, 255}, {226, 226, 226, 255}, {226, 226, 226, 255}, {227, 227, 
  227, 255}, {225, 226, 226, 255}, {153, 211, 211, 255}, {149, 211, 211, 255}, {223, 226, 226, 255}, {229, 227, 227, 
  255}, {223, 226, 226, 255}, {150, 211, 211, 255}, {150, 211, 211, 255}, {223, 225, 225, 255}, {230, 229, 229, 255}, 
  {220, 220, 220, 255}, {140, 140, 140, 255}, {152, 152, 152, 255}, {227, 227, 227, 255}, {227, 227, 227, 255}, {226, 
  226, 226, 255}, {226, 226, 226, 255}, {226, 226, 226, 255}, {226, 226, 226, 255}, {226, 226, 226, 255}, {226, 226, 
  226, 255}, {226, 226, 226, 255}, {226, 226, 226, 255}}, {{225, 225, 225, 255}, {225, 225, 225, 255}, {225, 225, 
  225, 255}, {225, 225, 225, 255}, {225, 225, 225, 255}, {225, 225, 225, 255}, {225, 225, 225, 255}, {225, 225, 225, 
  255}, {225, 225, 225, 255}, {226, 225, 225, 255}, {236, 227, 227, 255}, {237, 228, 228, 255}, {227, 225, 225, 255}, 
  {224, 225, 225, 255}, {230, 226, 226, 255}, {244, 229, 229, 255}, {244, 229, 229, 255}, {230, 226, 226, 255}, {223, 
  224, 224, 255}, {227, 227, 227, 255}, {238, 238, 238, 255}, {237, 237, 237, 255}, {226, 226, 226, 255}, {225, 225, 
  225, 255}, {225, 225, 225, 255}, {225, 225, 225, 255}, {225, 225, 225, 255}, {225, 225, 225, 255}, {225, 225, 225, 
  255}, {225, 225, 225, 255}, {225, 225, 225, 255}, {225, 225, 225, 255}}, {{224, 224, 224, 248}, {224, 224, 224, 
  255}, {224, 224, 224, 255}, {224, 224, 224, 255}, {224, 224, 224, 255}, {224, 224, 224, 255}, {224, 224, 224, 255}, 
  {224, 224, 224, 255}, {224, 224, 224, 255}, {224, 224, 224, 255}, {221, 223, 223, 255}, {221, 223, 223, 255}, {222, 
  224, 224, 255}, {228, 225, 225, 255}, {207, 221, 221, 255}, {106, 201, 201, 255}, {106, 201, 201, 255}, {207, 221, 
  221, 255}, {228, 225, 225, 255}, {222, 223, 223, 255}, {221, 221, 221, 255}, {221, 221, 221, 255}, {224, 224, 224, 
  255}, {224, 224, 224, 255}, {224, 224, 224, 255}, {224, 224, 224, 255}, {224, 224, 224, 255}, {224, 224, 224, 255}, 
  {224, 224, 224, 255}, {224, 224, 224, 255}, {224, 224, 224, 255}, {224, 224, 224, 248}}, {{226, 226, 226, 222}, 
  {222, 222, 222, 255}, {223, 223, 223, 254}, {223, 223, 223, 255}, {223, 223, 223, 255}, {223, 223, 223, 255}, {223, 
  223, 223, 255}, {223, 223, 223, 255}, {223, 223, 223, 255}, {223, 223, 223, 255}, {223, 223, 223, 255}, {223, 223, 
  223, 255}, {220, 222, 222, 255}, {235, 225, 225, 255}, {124, 205, 205, 255}, {46, 189, 189, 255}, {46, 190, 190, 
  255}, {124, 205, 205, 255}, {235, 225, 225, 255}, {220, 222, 222, 255}, {223, 223, 223, 255}, {223, 223, 223, 255}, 
  {223, 223, 223, 255}, {223, 223, 223, 255}, {223, 223, 223, 255}, {223, 223, 223, 255}, {223, 223, 223, 255}, {223, 
  223, 223, 255}, {223, 223, 223, 255}, {223, 223, 223, 254}, {222, 222, 222, 255}, {226, 226, 226, 222}}, {{232, 
  232, 232, 172}, {220, 220, 220, 254}, {222, 222, 222, 252}, {222, 222, 222, 255}, {222, 222, 222, 255}, {222, 222, 
  222, 255}, {222, 222, 222, 255}, {222, 222, 222, 255}, {222, 222, 222, 255}, {222, 222, 222, 255}, {222, 222, 222, 
  255}, {222, 222, 222, 255}, {219, 221, 221, 255}, {232, 224, 224, 255}, {148, 209, 209, 255}, {48, 190, 190, 255}, 
  {47, 190, 190, 255}, {148, 208, 208, 255}, {232, 224, 224, 255}, {219, 221, 221, 255}, {222, 222, 222, 255}, {222, 
  222, 222, 255}, {222, 222, 222, 255}, {222, 222, 222, 255}, {222, 222, 222, 255}, {222, 222, 222, 255}, {222, 222, 
  222, 255}, {222, 222, 222, 255}, {222, 222, 222, 255}, {222, 222, 222, 252}, {220, 220, 220, 254}, {232, 232, 232, 
  172}}, {{241, 241, 241, 93}, {218, 218, 218, 255}, {221, 221, 221, 251}, {221, 221, 221, 255}, {221, 221, 221, 
  255}, {221, 221, 221, 255}, {221, 221, 221, 255}, {221, 221, 221, 255}, {221, 221, 221, 255}, {221, 221, 221, 255}, 
  {221, 221, 221, 255}, {221, 221, 221, 255}, {220, 221, 221, 255}, {221, 221, 221, 255}, {222, 221, 221, 255}, {161, 
  210, 210, 255}, {161, 210, 210, 255}, {222, 221, 221, 255}, {221, 221, 221, 255}, {220, 221, 221, 255}, {221, 221, 
  221, 255}, {221, 221, 221, 255}, {221, 221, 221, 255}, {221, 221, 221, 255}, {221, 221, 221, 255}, {221, 221, 221, 
  255}, {221, 221, 221, 255}, {221, 221, 221, 255}, {221, 221, 221, 255}, {221, 221, 221, 251}, {218, 218, 218, 255}, 
  {242, 242, 242, 93}}, {{253, 253, 253, 13}, {222, 222, 222, 234}, {219, 219, 219, 255}, {220, 220, 220, 252}, {220, 
  220, 220, 255}, {220, 220, 220, 255}, {220, 220, 220, 255}, {220, 220, 220, 255}, {220, 220, 220, 255}, {220, 220, 
  220, 255}, {220, 220, 220, 255}, {220, 220, 220, 255}, {220, 220, 220, 255}, {220, 220, 220, 255}, {220, 220, 220, 
  255}, {230, 221, 221, 255}, {230, 222, 222, 255}, {220, 220, 220, 255}, {220, 220, 220, 255}, {220, 220, 220, 255}, 
  {220, 220, 220, 255}, {220, 220, 220, 255}, {220, 220, 220, 255}, {220, 220, 220, 255}, {220, 220, 220, 255}, {220, 
  220, 220, 255}, {220, 220, 220, 255}, {220, 220, 220, 255}, {220, 220, 220, 252}, {219, 219, 219, 255}, {222, 222, 
  222, 234}, {253, 253, 253, 13}}, {{255, 255, 255, 0}, {244, 244, 244, 73}, {217, 217, 217, 254}, {218, 218, 218, 
  255}, {220, 220, 220, 250}, {219, 219, 219, 252}, {219, 219, 219, 254}, {219, 219, 219, 255}, {219, 219, 219, 255}, 
  {219, 219, 219, 255}, {219, 219, 219, 255}, {219, 219, 219, 255}, {219, 219, 219, 255}, {219, 219, 219, 255}, {219, 
  219, 219, 255}, {217, 218, 218, 255}, {217, 218, 218, 255}, {219, 219, 219, 255}, {219, 219, 219, 255}, {219, 219, 
  219, 255}, {219, 219, 219, 255}, {219, 219, 219, 255}, {219, 219, 219, 255}, {219, 219, 219, 255}, {219, 219, 219, 
  255}, {219, 219, 219, 254}, {219, 219, 219, 252}, {220, 220, 220, 250}, {218, 218, 218, 255}, {217, 217, 217, 254}, 
  {244, 244, 244, 73}, {255, 255, 255, 0}}, {{255, 255, 255, 3}, {255, 255, 255, 0}, {243, 243, 243, 76}, {220, 220, 
  220, 229}, {215, 215, 215, 255}, {216, 216, 216, 255}, {217, 217, 217, 255}, {218, 218, 218, 255}, {218, 218, 218, 
  255}, {218, 218, 218, 255}, {218, 218, 218, 255}, {218, 218, 218, 255}, {218, 218, 218, 255}, {218, 218, 218, 255}, 
  {218, 218, 218, 255}, {218, 218, 218, 255}, {218, 218, 218, 255}, {218, 218, 218, 255}, {218, 218, 218, 255}, {218, 
  218, 218, 255}, {218, 218, 218, 255}, {218, 218, 218, 255}, {218, 218, 218, 255}, {218, 218, 218, 255}, {218, 218, 
  218, 255}, {217, 217, 217, 255}, {216, 216, 216, 255}, {215, 215, 215, 255}, {220, 220, 220, 229}, {243, 243, 243, 
  76}, {255, 255, 255, 0}, {255, 255, 255, 3}}, {{255, 255, 255, 0}, {255, 255, 255, 3}, {255, 255, 255, 0}, {253, 
  253, 253, 15}, {240, 240, 240, 94}, {229, 229, 229, 170}, {222, 222, 222, 220}, {218, 218, 218, 246}, {217, 217, 
  217, 255}, {217, 217, 217, 255}, {217, 217, 217, 255}, {217, 217, 217, 255}, {217, 217, 217, 255}, {217, 217, 217, 
  255}, {217, 217, 217, 255}, {217, 217, 217, 255}, {217, 217, 217, 255}, {217, 217, 217, 255}, {217, 217, 217, 255}, 
  {218, 218, 218, 255}, {217, 217, 217, 255}, {217, 217, 217, 255}, {217, 217, 217, 255}, {217, 217, 217, 255}, {218, 
  218, 218, 246}, {222, 222, 222, 220}, {229, 229, 229, 170}, {240, 240, 240, 94}, {253, 253, 253, 15}, {255, 255, 
  255, 0}, {255, 255, 255, 3}, {255, 255, 255, 0}}}], "Byte", ColorSpace -> "RGB", Interleaving -> True];
  
End[] (* End Private Context *)
           		
End[]


SetAttributes[{},{ReadProtected, Protected}];

System`Private`RestoreContextPath[];

(* Return three functions to define oauthservicedata, oauthcookeddata, oauthsendmessage  *)
{FitbitOAuth`Private`fitbitdata,FitbitOAuth`Private`fitbitcookeddata,FitbitOAuth`Private`fitbitsendmessage}
