Package["TextSearch`"]

PackageScope["$IndexedFileCount"]

PackageScope["DCreateEmptyIndex"]
PackageScope["DAddToIndex"]

PackageScope["DQueryColumn"]
PackageScope["DQueryColumnBulkReturn"]
PackageScope["DQueryColumnCount"]
PackageScope["DGetAllColumns"]
DQueryColumnBulkReturn[___] := $NotImplemented;
DQueryColumnCount[___] := $NotImplemented;
DGetAllColumns[___] := $NotImplemented;

PackageScope["$NotImplemented"]

PackageScope["NIFallback"]
SetAttributes[NIFallback, HoldAll];
NIFallback[body_, then___] := Replace[body, $NotImplemented :> NIFallback[then]];
NIFallback[] := $Failed;


PackageScope["DRemoveStaleDocumentsFromIndex"]
PackageScope["DGetIndexedFiles"]
PackageScope["DGetDocumentCount"]

PackageScope["DCreateHandle"]
PackageScope["DDeleteHandle"]
DCreateHandle[_, _, _] := $Failed;

PackageScope["$ValidDrivers"]

$ValidDrivers = {"CLucene", "Lucene", "WLNative"};