(* $Id: ShellCommandDemo.m,v 1.4 2014/04/07 18:41:07 bakshee Exp $ *)

(* A discoverable singleton class that executes a shell command. *)

BeginPackage["DeviceAPI`Drivers`Demos`ShellCommandDemo`Dump`"];

DeviceFramework`DeviceClassRegister["ShellCommandDemo",
	"ReadFunction" -> (ReadList["!"<>#2, Record]&),
	"ExecuteAsynchronousFunction" -> (Missing["NotAvailable"]&),
	"Singleton" -> True,
	"DriverVersion" -> 0.001
];

EndPackage[];
