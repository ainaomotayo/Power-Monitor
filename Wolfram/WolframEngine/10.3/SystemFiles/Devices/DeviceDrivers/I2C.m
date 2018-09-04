(* ::Package:: *)

(* Mathematica Package *)

(*this just calls DeviceClassRegister with the MRAALink functions, so that MRAALink doesn't have to be loaded manually with Needs to use I2C*)

(*driver version: 1.0*)

BeginPackage["DeviceFramework`Drivers`I2C`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

(*need to load RaspiTools before registering the class*)
Needs["MRAALink`"];

If[Not[Devices`DeviceAPI`DeviceDump`knownClassQ["I2C"]],
	(*THEN*)
	(*there aren't any i2c drivers registered, so we should register one*)
	DeviceFramework`DeviceClassRegister["I2C",
		"OpenFunction"->MRAALink`Private`i2cOpenDriver,
		"ReadFunction"->MRAALink`Private`i2cReadDriver,
		"WriteFunction"->MRAALink`Private`i2cWriteDriver,
		"FindFunction"->MRAALink`Private`FindI2CDevicesDriver,
		"MakeManagerHandleFunction"->MRAALink`Private`makeManagerHandle,
		(*"DeviceIconFunction"->MRAALink`Private`i2cIconFunction,*)
		"DriverVersion"->1.0
	]
	(*ELSE*)
	(*there already is a driver registered, so don't register anything*)
]


End[] (* End Private Context *)

EndPackage[]
