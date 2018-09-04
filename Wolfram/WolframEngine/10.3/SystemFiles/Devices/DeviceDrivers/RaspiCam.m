(* Mathematica Package *)

(* This package calls the DeviceClassRegister loading RaspiCam automatically*)

BeginPackage["DeviceFramework`Drivers`RaspiCam`"]

Begin["`Private`"]

(*need to load RaspiTools before registering the class*)
Needs["RaspberryPiTools`"];

If[Not[Devices`DeviceAPI`DeviceDump`knownClassQ["RaspiCam"]],
	(*THEN*)
	DeviceFramework`DeviceClassRegister["RaspiCam",
		"ReadFunction"->RaspberryPiTools`Private`RaspiCam,
		"OpenFunction"->RaspberryPiTools`Private`CheckMMAL,
		"MakeManagerHandleFunction"->makeManagerHandle
	]
	(*ELSE*)
	(*there already is a driver registered, so don't register anything*)
]

makeManagerHandle[___]:=CreateUUID[];


End[] (* End Private Context *)

EndPackage[]
