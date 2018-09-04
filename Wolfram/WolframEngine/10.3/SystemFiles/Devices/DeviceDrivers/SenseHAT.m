(* Mathematica Package *)

(* This package calls the DeviceClassRegister loading SenseHAT automatically*)

BeginPackage["DeviceFramework`Drivers`SenseHAT`"]

Begin["`Private`"]

(*need to load RaspiTools before registering the class*)
Needs["RaspberryPiTools`"];

If[Not[Devices`DeviceAPI`DeviceDump`knownClassQ["SenseHAT"]],
	(*THEN*)
	DeviceFramework`DeviceClassRegister["SenseHAT",
		"OpenFunction"->RaspberryPiTools`Private`deviceOpenDriver,
		"ReadFunction"->RaspberryPiTools`Private`RaspiSenseHat,
		"WriteFunction"->RaspberryPiTools`Private`DisplayMessage,
		"MakeManagerHandleFunction"->makeManagerHandle
	];
	(*ELSE*)
	(*there already is a driver registered, so don't register anything*)
]

makeManagerHandle[___]:=CreateUUID[];

End[] (* End Private Context *)

EndPackage[]
