BingMaps[latitude_, longitude_] := Module[{template, embedding},
 template = FileTemplate[ FileNameJoin[ {$TemplatesDirectory, "bingmaps.template"} ] ];
 embedding = TemplateApply[ template, <| "lat" -> latitude, "lon" -> longitude |> ];
 EmbeddedHTML[ embedding ]
]
