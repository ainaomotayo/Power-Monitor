(* All loading of the paclet's Wolfram Language code should go through this file. *)

(* Developer maintains this list of symbols.
   autoloadSymbols must agree with the symbols listed in the Kernel extension in the PacletInfo.m file.
*)

Templating`Private`autoloadSymbols = {
    "System`CombinerFunction", 
    "System`InsertionFunction", 
    "System`StringTemplate", 
    "System`DefaultValue",
    "System`FileTemplate",
    "System`FileTemplateApply", 
    "System`TemplateApply", 
    "System`TemplateObject",
    "System`TemplateIf", 
    "System`TemplateSequence", 
    "System`TemplateSlot", 
    "System`TemplateExpression",
    "System`TemplateUnevaluated",
    "System`TemplateVerbatim",
    "System`TemplateWith", 
    "System`XMLTemplate",
    "System`Webpage",
    "System`$HTMLExportRules",
    "System`$TemplatePath",
    "Templating`ExportHTML"
};

Map[
    (Unprotect[#];ClearAll[#]) &, Join[
        Templating`Private`autoloadSymbols, {
            "Templating`*",
            "Templating`PackageScope`*",
            "Templating`*`PackagePrivate`*"
        }
    ]
]

PacletManager`Package`loadWolframLanguageCode[
    "Templating", 
    "Templating`", 
    DirectoryName[$InputFileName], 
    "Primitives.m",
    "AutoUpdate" -> True, 
    "ForceMX" -> False, 
    "Lock" -> False,
    "AutoloadSymbols" -> Templating`Private`autoloadSymbols,
    "SymbolsToProtect" -> Templating`Private`autoloadSymbols, 
    "HiddenImports" -> {"GeneralUtilities`"}
]