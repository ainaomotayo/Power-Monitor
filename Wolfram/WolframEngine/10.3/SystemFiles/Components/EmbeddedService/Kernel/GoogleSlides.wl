GoogleSlides[id_] := Module[{},
 template = FileTemplate[ FileNameJoin[ {$TemplatesDirectory, "googleslides.template"} ] ];
 embedding = TemplateApply[ template, <| "id" -> id |> ];
 EmbeddedHTML[ embedding ]
]
