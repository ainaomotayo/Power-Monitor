OpenStreetMaps[lat_, lon_] := Module[{template, embedding},
 template = FileTemplate[ FileNameJoin[ {$TemplatesDirectory, "openstreetmaps.template"} ] ];
 lat1 = lat - 0.0945;
 lat2 = lat + 0.0945;
 lon1 = lon - 0.201;
 lon2 = lon + 0.201;
 embedding = TemplateApply[ template, <| "lat1" -> lat1, "lat2" -> lat2, "lon1" -> lon1, "lon2" -> lon2 |> ];
 EmbeddedHTML[ embedding ]
]
