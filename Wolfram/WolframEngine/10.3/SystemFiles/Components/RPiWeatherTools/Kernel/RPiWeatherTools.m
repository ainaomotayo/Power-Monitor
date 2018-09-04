(* Mathematica Package *)

(* Created by the Wolfram Workbench Nov 8, 2013 *)

BeginPackage["RPiWeatherTools`"]
(* Exported symbols added here with SymbolName::usage *) 

RPiWeatherToolsIO::usage = "Main IO function for weather station.";


Begin["`Private`"]
(* Implementation of the package *)

$packageFile = $InputFileName;
$libName = Switch[ $SystemID,
	"Linux-ARM",
		"RPiWeatherTools.so"
]

$adapterLib = FileNameJoin[{FileNameTake[$packageFile, {1,-3}], "LibraryResources", $SystemID, $libName}];
$adapterInitialized;

loadAdapter[]:=  
(
	(* Get["C:\\Work\\DevWIN\\Devices\\DeviceInformationEditor\\DeviceInformationEditor\\DeviceInformationEditor.m"]; (*TODO: Hardcoded package *) 
	If[!ValueQ@$devInfoTreeObj, $devInfoTreeObj = DeviceInformationEditor`DeviceInformationTreeLoad[$$devInfoFile];]; *)
	If[!ValueQ@$adapterInitialized, 
		lfWeatherStationI2Cio = LibraryFunctionLoad[ $adapterLib, "WeatherStation_I2C_IO", LinkObject, LinkObject];
		$adapterInitialized = True;
	]
)

loadAdapter[];


RPiWeatherToolsIO[ args___]:= lfWeatherStationI2Cio[ args];

End[]

EndPackage[]

