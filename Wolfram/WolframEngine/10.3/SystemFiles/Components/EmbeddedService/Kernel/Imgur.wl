Imgur[id_] := Module[{},
 template = FileTemplate[ FileNameJoin[ {$TemplatesDirectory, "imgur.template"} ] ];
 embedding = TemplateApply[ template, <| "id" -> id |> ];
 EmbeddedHTML[ embedding ]
]
