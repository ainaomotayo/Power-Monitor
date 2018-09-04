BeginPackage["CloudObject`"]

System`$CloudBase;

Begin["`Private`"]

If[!StringQ[$UrlScheme],
    $UrlScheme = "https";
];

$CloudBase /: Set[$CloudBase, base_] := verifyCloudBase[base]

verifyCloudBase[base_] := 
 If[StringQ[base], $cloudbase = base, 
  Message[$CloudBase::cbase, base]; base]
  
If[StringQ[$CloudBase], $cloudbase = $CloudBase];  
  
$CloudBase := If[StringQ[$cloudbase], $cloudbase, "https://www.wolframcloud.com/"]  

If[ValueQ[CloudSystem`$ApplicationDomain],
    If[CloudSystem`$ApplicationDomain === "localhost" || CloudSystem`$ApplicationDomain === "localhost:8080",
        $UrlScheme = "http";
    ];
    $CloudBase = $UrlScheme<>"://" <> CloudSystem`$ApplicationDomain <> "/";
];

End[]

EndPackage[]
