(* $Id: FunctionDemo.m,v 1.2 2014/04/07 21:06:32 bakshee Exp $ *)

BeginPackage["DeviceAPI`Drivers`Demos`FunctionDemo`"];

PrependTo[$ContextPath, "DeviceFramework`"];

Begin["`Private`"];

exec[_,f_String,args___] := exec[Null,ToExpression[f],args]
exec[_,f_,args___] := f[args]

DeviceClassRegister["FunctionDemo",
	"ExecuteFunction" -> exec,
	"FindFunction" -> ({{}}&),
	"DriverVersion" -> 0.001
];

End[];
EndPackage[];
