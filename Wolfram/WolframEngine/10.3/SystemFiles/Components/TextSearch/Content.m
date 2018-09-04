Package["TextSearch`"]


PackageScope["TextField"]
PackageScope["TextFile"]

PackageExport["ReadPlaintext"]

ReadPlaintext[file_String | File[file_String]] := Module[{ext = FileExtension[file]},
	Which[
		!FileExistsQ[file], 
			Missing["FileMissing"],
		MemberQ[$ImportedFileExtensions, ext],
			Quiet @ Check[TextField @ Import[file, "Plaintext"], $Failed], 
		MemberQ[$IgnoredFileExtensions, ext],
			$Failed,
		True,
			ReadStringASCII[file]
	]
];

ReadPlaintext[_] := $Failed

PackageExport["$Verbose"]

$Verbose::usage = "If $Verbose is True, files that are ignored during indexing will be reported as messages.";
$Verbose = False;

PackageExport["$FileSizeLimitsEnabled"]

$FileSizeLimitsEnabled::usage = "If $FileSizeLimitsEnabled is True, files will be checked against one of {$TextFileSizeLimit, $ImportedFileSizeLimit, $UnrecognizedFileSizeLimit, $ImportedFileTimeLimit}.";
$FileSizeLimitsEnabled = True;

PackageExport["$TextFileSizeLimit"]
PackageExport["$ImportedFileSizeLimit"]
PackageExport["$UnrecognizedFileSizeLimit"]
PackageExport["$ImportedFileTimeLimit"]

$TextFileSizeLimit = 200000000; (* ~200 megabytes *)
$ImportedFileSizeLimit = 25000000; (* ~25 megabytes *)
$UnrecognizedFileSizeLimit = 5000000; (* ~1 megabyte *)
$ImportedFileTimeLimit = 15.0;

CreateSearchIndex::docignored = "Ignoring file \"``\", whose size (``) exceeds the limit (``) for files of this type. Adjust `` to override this limit.";
CreateSearchIndex::doctimedout = "Ignoring file \"``\", which took longer than `` seconds to import. Adjust `` to override this limit.";
CreateSearchIndex::doccorrupt = "Ignoring file \"``\", which issued messages when imported as text.";

SetAttributes[checksize, HoldAllComplete];
checksize[path_, sz_, limit_, body_] :=
	If[TrueQ[sz > limit] && $FileSizeLimitsEnabled, 
		If[$Verbose, Message[CreateSearchIndex::docignored, path, bytesToSize[sz], bytesToSize[limit], HoldForm[limit]]];
		$Failed
	,
		body
	];


PackageExport["LimitedReadPlaintext"]

(* this is really only for debugging purposes, as a convenience for calling iLimitedReadPlanText *)
LimitedReadPlaintext[path_] := If[FileType[path] =!= File, Missing["FileMissing"],
	iLimitedReadPlaintext[path, ToLowerCase @ FileExtension[path], FileByteCount[path]]
];


PackageScope["ProbablyBinaryFileQ"]

ProbablyBinaryFileQ[path_] := Module[{bytes, lim},
	bytes = BinaryReadList[path, "Byte", 256];
	lim = Length[bytes] / 10;
	Min[bytes] == 0 || 
	Max[bytes] == 255 || 
	(lim > 2 &&  (* not enough spaces *)    (* too many high bytes *) 
		(Count[bytes, 9|10|32] < lim || Total[UnitStep[bytes - 128]] > lim)
	)
];

(* this will import as a string, or return File[...] if the file would be read raw anyway, so
we can pass it straight to the driver as an optimization.
ext must be lowercase *)
iLimitedReadPlaintext[path_, ext_, size_] := 
    Which[
    	size == 0,
    		$Failed,
    	MemberQ[$IgnoredFileExtensions, ext], 
    		$Failed,
    	MemberQ[$TextFileExtensions, ext],
    		checksize[path, size, $TextFileSizeLimit, TextFile[path]],
        MemberQ[$ImportedFileExtensions, ext],
        	checksize[path, size, $ImportedFileSizeLimit, 
        		TimeConstrained[
        			Quiet @ Check[
        				TextField @ Import[path, "Plaintext"], 
        				If[$Verbose, Message[CreateSearchIndex::doccorrupt, path]];
        				$Failed
        			], 
        			$ImportedFileTimeLimit, 
        			If[$Verbose, Message[CreateSearchIndex::doctimedout, path, $ImportedFileTimeLimit, HoldForm[$ImportedFileTimeLimit]]];
        			$Failed
        		]
      		],
      	(size < $UnrecognizedFileSizeLimit) && ProbablyBinaryFileQ[path],
      		$Failed,
        True, 
        	checksize[path, size, $UnrecognizedFileSizeLimit, TextFile[path]]
    ];


PackageExport["$IgnoredFileExtensions"]
PackageExport["$ImportedFileExtensions"]
PackageExport["$TextFileExtensions"]

$IgnoredFileExtensions = {"dmg", "iso", "app", "mx", "a", "tar", "gz", "ar", "doc", "o", "dll", "exe", "dylib", "bin", "dat", "zip" ,"ds_store", "gif", "jpg", "bmp", "png", "tiff", "raw", "jpeg", "img", "jar", "mpg", "avi", "mp4", "mp3", "mkv", "m4a"};
$ImportedFileExtensions = {"cdf", "nb", "html", "htm", "rtf", "pdf", "eps", "tex", "latex", "xml"};
$TextFileExtensions = {"txt", "csv", "tsv", "json", "mbox", "md", "mc", "c", "cpp", "h", "hpp", "java", "py", "rb", "rs", "go", "js", "csx", "php", "pl", "sh", "css", "cmake", "m", "mt", "wl", "sql", "r", "scala", "d", "lua", "hs", "swift", "ini", "toml"};


PackageExport["ReadFileFields"]

(* assumes path exists, code that calls this typically already knows this *)
(*Shouldn't we have includeSnippet rather than include the full text?*)
PackageScope["$FileFields"]

$FileFields = {
	"Location", "FileName", "Plaintext", 
	"ModificationDate", "CreationDate", "FileByteCount", 
	"FileExtension"
};

ReadFileFields[path_, includetext_:True] := Module[
	{ext, size, filename, plaintext = None},
	ext = ToLowerCase @ FileExtension[path];
	filename = FileNameTake[path];
	size = FileByteCount[path];
	If[includetext, 
		plaintext = iLimitedReadPlaintext[path, ext, size];
		If[FailureQ[plaintext], Return[$Failed]];
	];
	<|
	    "Location" -> File[path],
	    "FileName" -> filename,
	    If[plaintext === None, {}, "Plaintext" -> plaintext],
	    "ModificationDate" -> FileDate[path],
	    If[!$linuxQ, "CreationDate" -> FileDate[path, "Creation"], {}],
	    "FileByteCount" -> size,
	    "FileExtension" -> ext
	|>
];
