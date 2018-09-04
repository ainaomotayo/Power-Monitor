Package["TextSearch`"]

PackageImport["Macros`"]

SetArgumentCount[DeleteSearchIndex, 1];

DeleteSearchIndex[expr_] := If[!FreeQ[iDeleteSearchIndex[expr], $Failed], $Failed];
DeleteSearchIndex[All] := If[FileExistsQ[$SearchIndicesDirectory],
	DeleteAllHandles[];
	Quiet @ DeleteDirectory[$SearchIndicesDirectory, DeleteContents -> True]
];

iDeleteSearchIndex[list_List] := 
	Map[iDeleteSearchIndex, list];

iDeleteSearchIndex[obj:SearchIndexObject[File[path_String]]] := (  
    DeleteHandle[obj];
    Quiet @ Check[DeleteDirectory[path, DeleteContents -> True], $Failed];
)


iDeleteSearchIndex[name_String] := 
	iDeleteSearchIndex[SearchIndexObject[name]];

iDeleteSearchIndex[obj_] := 
	(Message[DeleteSearchIndex::invind, obj]; $Failed);

iDeleteSearchIndex[$Failed] := $Failed;