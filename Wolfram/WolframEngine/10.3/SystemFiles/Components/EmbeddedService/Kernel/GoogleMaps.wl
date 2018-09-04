GoogleMaps[latitude_, longitude_] := Module[{},
 template = FileTemplate[ FileNameJoin[ {$TemplatesDirectory, "googlemaps.template"} ] ];
 embedding = TemplateApply[ template, <| "latitude" -> latitude, "longitude" -> longitude |> ];
 EmbeddedHTML[ embedding ]
]
