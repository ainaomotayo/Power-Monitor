Unprotect[System`EmbeddedService];
Unprotect[System`$EmbeddableServices];

BeginPackage["EmbeddedService`"];
Begin["`Private`"];

(* where to find the html template files *)
$TemplatesDirectory = FileNameJoin[ {"EmbeddedService", "Kernel", "Templates"} ];

(* update this list when you add a new embeddable service *)
$EmbeddableServices := Sort[ { "YouTube", "Vimeo", "Vine", "UStream", "Twitter", "SoundCloud", "Reddit", "Instagram", "Imgur", "GoogleDocs", "GoogleSheets", "GoogleSlides", "GoogleForms", "GoogleMaps", "BingMaps", "OpenStreetMaps" } ];



Initialize[] := Module[ {},

 (* Heuristically determine embedding from a given url string *)
 EmbeddedService[ url_String/;StringMatchQ[url, ("http"|"https")~~___] ] := EmbeddedService[ AutoDecode[url] ];

 (* YouTube *)
 Get[ FileNameJoin[ { "EmbeddedService", "Kernel", "YouTube.wl" } ] ];
 EmbeddedService[ {"YouTube", id_String} ] := YouTube[id];
 EmbeddedService[ {"YouTube", id_String}, options_Association ] := YouTube[id, Sequence@@options];
 
 (* Vimeo *)
 Get[ FileNameJoin[ { "EmbeddedService", "Kernel", "Vimeo.wl" } ] ];
 EmbeddedService[ {"Vimeo", id_String} ] := Vimeo[id];
 
 (* Vine *)
 Get[ FileNameJoin[ { "EmbeddedService", "Kernel", "Vine.wl" } ] ];
 EmbeddedService[ {"Vine", id_String} ] := Vine[id];
 
 (* UStream *)
 Get[ FileNameJoin[ { "EmbeddedService", "Kernel", "UStream.wl" } ] ];
 EmbeddedService[ {"UStream", id_String} ] := UStream[id];
 
 (* SoundCloud *)
 Get[ FileNameJoin[ { "EmbeddedService", "Kernel", "SoundCloud.wl" } ] ];
 EmbeddedService[ {"SoundCloud", id_String} ] := SoundCloud[id];
 
 (* Rdio *)
 Get[ FileNameJoin[ { "EmbeddedService", "Kernel", "Rdio.wl" } ] ];
 EmbeddedService[ {"Rdio", id_String} ] := Rdio[id];

 (* Imgur *)
 Get[ FileNameJoin[ { "EmbeddedService", "Kernel", "Imgur.wl" } ] ];
 EmbeddedService[ {"Imgur", id_String} ] := Imgur[id];

 (* GoogleDocs *)
 Get[ FileNameJoin[ { "EmbeddedService", "Kernel", "GoogleDocs.wl" } ] ];
 EmbeddedService[ {"GoogleDocs", id_String} ] := GoogleDocs[id];

 (* GoogleSheets *)
 Get[ FileNameJoin[ { "EmbeddedService", "Kernel", "GoogleSheets.wl" } ] ];
 EmbeddedService[ {"GoogleSheets", id_String} ] := GoogleSheets[id];

 (* GoogleSlides *)
 Get[ FileNameJoin[ { "EmbeddedService", "Kernel", "GoogleSlides.wl" } ] ];
 EmbeddedService[ {"GoogleSlides", id_String} ] := GoogleSlides[id];

 (* GoogleForms *)
 Get[ FileNameJoin[ { "EmbeddedService", "Kernel", "GoogleForms.wl" } ] ];
 EmbeddedService[ {"GoogleForms", id_String} ] := GoogleForms[id];

 (* GoogleMaps *)
 Get[ FileNameJoin[ { "EmbeddedService", "Kernel", "GoogleMaps.wl" } ] ];
 EmbeddedService[ {"GoogleMaps", latitude_, longitude_} ] := GoogleMaps[latitude, longitude];

 (* Reddit *)
 Get[ FileNameJoin[ { "EmbeddedService", "Kernel", "Reddit.wl" } ] ];
 EmbeddedService[ {"Reddit", sub_String, id_String} ] := Reddit[sub, id];

 (* Instagram *)
 Get[ FileNameJoin[ { "EmbeddedService", "Kernel", "Instagram.wl" } ] ];
 EmbeddedService[ {"Instagram", id_String} ] := Instagram[id];

 (* Twitter *)
 Get[ FileNameJoin[ { "EmbeddedService", "Kernel", "Twitter.wl" } ] ];
 EmbeddedService[ {"Twitter", id_String} ] := Twitter[user, id];

 (* Bing Maps *)
 Get[ FileNameJoin[ { "EmbeddedService", "Kernel", "BingMaps.wl" } ] ];
 EmbeddedService[ {"BingMaps", lat_, lon_} ] := BingMaps[lat, lon];

 (* OpenStreet Maps *)
 Get[ FileNameJoin[ { "EmbeddedService", "Kernel", "OpenStreetMaps.wl" } ] ];
 EmbeddedService[ {"OpenStreetMaps", lat_, lon_} ] := OpenStreetMaps[lat, lon];

];



(* various heuristics to decode simple service urls to embeddable content *)

AutoDecode[ url_String ] := Module[{},
 First[ StringCases[ url, {
 "https://www.youtube.com/watch?v=" ~~ (id__) :> {"YouTube", id},
 "https://vimeo.com/"~~(id__) :> {"Vimeo", id},
 "https://vine.co/v/"~~(id__) :> {"Vine", id},
 "http://imgur.com/"~~(id__) :> {"Imgur", id},
 "https://www.reddit.com/r/" ~~ (sub__) ~~ "/comments/" ~~ (id__) :> {"Reddit", sub, id},
 "https://instagram.com/p/" ~~ (id__) ~~ "/" :> {"Instagram", id},
 "https://twitter.com/" ~~ (user__) ~~ "/status/" ~~ (id__) :> {"Twitter", id}
 }]
 ]
]


(*

 Main initialization of EmbeddedService 

*)

Initialize[];



End[];

EndPackage[];

SetAttributes[System`EmbeddedService,{Protected,ReadProtected}];
SetAttributes[System`$EmbeddableServices,{Protected,ReadProtected}];
