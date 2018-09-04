(* Paclet Info File *)

(* created 2015/04/07*)

Paclet[
    Name -> "Templating",
    Version -> "1.2",
    MathematicaVersion -> "10.1+",
    Description -> "Templating",
    Creator -> "Riccardo Di Virgilio <riccardod@wolfram.com>, Carlo Barbieri <carlob@wolfram.com>, Taliesin Beynon <taliesinb@wolfram.com>",
    Loading -> Automatic,
    Extensions -> 
        {
            {"Kernel", Symbols -> 

                {
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
                    "System`TemplateEvaluate",
                	"System`TemplateVerbatim",
                	"System`TemplateWith",
                	"System`XMLTemplate",
                	"System`Webpage",
                	"System`$HTMLExportRules",
                	"System`$TemplatePath",
                	"Templating`ExportHTML"
                }
            , Context -> 
                {"TemplatingLoader`", "Templating`"}
            , HiddenImport -> True}, 
            {"Resource", Root -> "Resources", Resources -> 
                {"Pluralize", "StaticLoader", "TemplateLoader"}
            }
        }
]


