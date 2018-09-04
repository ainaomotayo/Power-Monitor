(* ::Package:: *)

(* Mathematica Package *)

(*this just calls DeviceClassRegister with the MRAALink functions, so that MRAALink doesn't have to be loaded manually with Needs to use GPIO*)

(*driver version: 2.0*)

BeginPackage["DeviceFramework`Drivers`GPIO`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

(*need to load RaspiTools before registering the class*)
Needs["MRAALink`"];

If[Not[Devices`DeviceAPI`DeviceDump`knownClassQ["GPIO"]],
	(*THEN*)
	(*there aren't any gpio drivers registered, so we should register it one*)
	DeviceFramework`DeviceClassRegister["GPIO",
		"OpenFunction"->MRAALink`Private`gpioOpenDriver,
		"ReadFunction"->MRAALink`Private`gpioReadDriver,
		"WriteFunction"->MRAALink`Private`gpioWriteDriver,
		"FindFunction" -> ({{}}&),
		"GetPropertyFunction"->MRAALink`Private`gpioPropertyGetDriver,
		"Properties"->{"PinConfigurations"->Null},
		"ConfigureFunction"->MRAALink`Private`gpioConfigureDriver,
		"MakeManagerHandleFunction"->MRAALink`Private`makeManagerHandle,
		"DeviceIconFunction"->MRAALink`Private`gpioIconFunction,
		"DriverVersion"->2.0
	]
	(*ELSE*)
	(*there already is a driver registered, so don't register anything*)
]


End[] (* End Private Context *)

EndPackage[]
