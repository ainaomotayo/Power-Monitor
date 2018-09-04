Twitter[user_String,id_String] := Module[{template, embedding},
 template = FileTemplate[ FileNameJoin[ {$TemplatesDirectory, "twitter.template"} ] ];
 embedding = TemplateApply[ template, <| "id" -> id |> ];
 EmbeddedHTML[ embedding ]
]
