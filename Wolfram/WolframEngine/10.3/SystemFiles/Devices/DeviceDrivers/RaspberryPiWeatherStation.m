(* Mathematica Package *)

BeginPackage["RaspberryPiWeatherStation`", { "RPiWeatherTools`"}]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 


(* ::Section:: *) (* API Registration Function *)
If[ $SystemID === "Linux-ARM",
	DeviceFramework`DeviceClassRegister[ "RaspberryPiWeatherStation", 
		"FindFunction" -> ({{}}&),
		"ReadFunction" -> iRPiWeatherRead
	]
]


(* ::Section:: *) (* Device API Adapter Functions *)


(* ::SubSection:: *) (* Synchronous Read/Write *)

iRPiWeatherRead[{ iHandle_, dHandle_}, "Temperature"]:= Quantity[RPiWeatherToolsIO[{16^^42}, 1][[1]], "DegreesCelsius"]

iRPiWeatherRead[{ iHandle_, dHandle_}, "Humidity"]:= Quantity[RPiWeatherToolsIO[{16^^43}, 1][[1]], "Percent"]

iRPiWeatherRead[{ iHandle_, dHandle_}, "Pressure"]:= Quantity[ FromDigits[ Apply[ Join, IntegerDigits[#,2]& /@ Reverse[ RPiWeatherToolsIO[{16^^47},2]]],2], "hPa"  ];

End[] (* End Private Context *)

EndPackage[]