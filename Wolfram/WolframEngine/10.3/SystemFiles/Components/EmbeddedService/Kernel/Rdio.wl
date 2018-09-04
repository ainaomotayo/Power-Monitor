Rdio[id_] := Module[{},
 template = FileTemplate[ FileNameJoin[ {$TemplatesDirectory, "rdio.template"} ] ];
 embedding = TemplateApply[ template, <| "id" -> id |> ];
 EmbeddedHTML[ embedding ]
]
