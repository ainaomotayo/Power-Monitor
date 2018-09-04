Package["TextSearch`"]

PackageImport["Macros`"]
PackageImport["PacletManager`"]
PackageImport["JLink`"]

PackageExport["Autocomplete"]
PackageExport["AutocompletionFunction"]

$JDriverInitialized = None;

setup[] := If[!JavaObjectQ[$JDriverInitialized],
    InstallJava[];
    AddToClassPath[PacletResource["TextSearch", "javalibs"]];
    $JDriverInitialized = JavaNew["java.lang.Object"];
    KeepJavaObject[$JDriverInitialized, Manual];
];
SetAttributes[setupBlock, HoldAll];
setupBlock[expr_] := (setup[]; ReleaseHold[expr])

(* each word is either a string, or an association with keys String, Result (opt), Weight (opt) *)
createAutocompleteIndex[words_List] := setupBlock@Module[{index, w},

    index = JavaNew["com.wolfram.textSearch.utils.Autocomplete"];
    Map[If[StringQ[#],
            index@addWord[#, 0],
            w = Lookup[#, "String"];
            index@addWord[w, Lookup[#, "Result", w], Lookup[#, "Weight", 0]]
        ] &,
        words
    ];
    AutocompletionFunction[index]
]

createAutocompleteIndex[words_Association] :=
    createAutocompleteIndex[<|"String" -> #[[1]], "Weight" -> #[[2]]|> & /@ Normal[words]]

saveAutocompleteIndex[path_, AutocompletionFunction[index_]] := setupBlock[JavaBlock[
    index@save[path]
]]

loadAutocompleteIndex[path_] := setupBlock@Module[{index},
    index = JavaNew["com.wolfram.textSearch.utils.Autocomplete"];
    If[index@load[path], AutocompletionFunction[index], $Failed]
]

ImportExport`RegisterImport["AutocompletionFunction", loadAutocompleteIndex[#] &]
ImportExport`RegisterExport["AutocompletionFunction", saveAutocompleteIndex[#, #2] &]

applyAutocompleteFunction[AutocompletionFunction[index_], prefix_, n_ : -1] := setupBlock[JavaBlock[
    index@getWordsWithPrefix[prefix, n]@toArray[]
]]

(*
spec:
- Autocomplete[{s1, s2, xxx}] -> AutocompletionFunction[   ]
- Autocomplete[{string1, string2,}, "prefix"]
- AutocompletionFunction[LocalObject[ ]]["prefix"] (* TODO: start using LocalObject *)
*)

SetArgumentCount[Autocomplete, {1, 3}];

Autocomplete::invinp = "`` is not a valid input";
Autocomplete::integer = "`` is not a valid integer";
Autocomplete[words_] := ConditionalRHS[
    validVectorQ[words], {"invinp", words},
    createAutocompleteIndex[words]
];

validVectorQ[words_] := 
    VectorQ[
        words, 
        (StringQ[#] || autocompleteAssociationQ[#])&
    ] || scoringAssociationQ[words]

Autocomplete[words_, prefix_, n_ : -1] := ConditionalRHS[
    validVectorQ[words], {"invinp", words},
    StringQ[prefix], {"invinp", prefix},
    IntegerQ[n], {"integer", n},
    createAutocompleteIndex[words][prefix, n]
];

Autocomplete[words_, prefix_, All] := ConditionalRHS[
    StringQ[prefix], {"invinp", prefix},
    createAutocompleteIndex[words][prefix, n]
];

autocompleteAssociationQ[assoc_] :=  AssociationQ[assoc] && !FreeQ[Keys[assoc], "String"]

scoringAssociationQ[assoc_] := AssociationQ[assoc] && VectorQ[Keys[assoc], StringQ] && VectorQ[Values[assoc], NumberQ]

AutocompletionFunction::invinp = "`` in not a valid input";
AutocompletionFunction::integer = "`` is not a valid integer";

AutocompletionFunction[data___][prefix_String, All] := AutocompletionFunction[data][prefix]
AutocompletionFunction[data___][prefix_, All] := Message[AutocompletionFunction::invinp, prefix]
AutocompletionFunction[data___][prefix_String, n_Integer : -1] := applyAutocompleteFunction[AutocompletionFunction[data], prefix, n]
AutocompletionFunction[data___][prefix_String, n_: -1] := Message[AutocompletionFunction::integer, n]
AutocompletionFunction[data___][prefix_, n_ : -1] := Message[AutocompletionFunction::invinp, prefix]

AutocompletionFunction /: MakeBoxes[expr:AutocompletionFunction[index_, ___], StandardForm] := BoxForm`ArrangeSummaryBox[
    AutocompletionFunction, expr, 
    $autoCompleteIcon, 
    {BoxForm`SummaryItem[{"", ""}]}, (* TODO: replace by something simpler *)
    {},
    StandardForm]

