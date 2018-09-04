(* This OAuthLoader file only exists to separate the updating of the FormulaData paclet from its use. 
   This allows any update to the paclet, triggered below, to take effect in the same session. In other words,
   we load the OAuthLoader.m file from the currently-installed version of the paclet, it calls PacletUpdate
   (via getPacletWithProgress[] below), and then the Get["OAuth`"] will load the "real" code from the new paclet.
*)

PacletManager`Package`getPacletWithProgress["OAuth", "OAuth", "IsDataPaclet" -> True, "AllowUpdate" -> TrueQ[PacletManager`$AllowDataUpdates]]

Get[ "OAuth`"]

