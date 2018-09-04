(* Author:          Christopher Williamson *)
(* Copyright:       Copyright 2004-2013, Wolfram Research, Inc. *)
Begin[ "`DataSources`Private`" ] 

Needs["ResourceLocator`"];

Options[WriteDataSource] = 
    JoinOptions[
      {
        "Description" -> "", 
        "URL" -> Automatic, 
        "Username" -> Automatic, 
        "Password" -> Automatic,
        "Properties"-> Automatic,
        "Location" -> "User",
        "RelativePath" -> Automatic,
        "UseConnectionPool" -> Automatic
      },
      {
        "Catalog" -> Automatic,
        "ReadOnly" -> Automatic,
        "TransactionIsolationLevel" -> Automatic
      }
    ]

DatabaseResourcesPath::install = "Multiple installations of Database Package exist \
at `1`. This may lead to unpredictable results when running Database Package.";
WriteDataSource::location = "Illegal value for Location option: `1`"; 
WriteDataSource::url = "`1` does not support generating a URL. A URL must be supplied.";
WriteDataSource::exists = "DataSource `1` already exists.";

AddDatabaseResources[ x_String] := ResourceAdd[x, "DatabaseResources"];

DatabaseResourcesPath[] := ResourcesLocate["DatabaseResources"];

If[!MemberQ[DatabaseResourcesPath[], 
            {ToFileName[$UserBaseDirectory, "DatabaseResources"], None}], 
  AddDatabaseResources[ToFileName[$UserBaseDirectory, "DatabaseResources"]]
];
    
If[!MemberQ[DatabaseResourcesPath[], 
            {ToFileName[$BaseDirectory, "DatabaseResources"], None}], 
  AddDatabaseResources[ToFileName[$BaseDirectory, "DatabaseResources"]]
];

dataSourceQ[file_String] := 
  Module[{is, word},
    is = OpenRead[file];
    word = Read[is, Word , WordSeparators -> {" ", "\n", "\r", "\t", "["}];
    Close[is];
    word === "SQLConnection"
  ]
  
DataSources[] := 
  Cases[ Flatten[FileNames["*.m", First[#]]& /@ DatabaseResourcesPath[]], 
    file_String/;(FileType[file] =!= Directory && dataSourceQ[file]):>Append[Get[file], "Location" -> file]];
    
DataSources[dataSourceName_String]:=
  Module[{cases},
    cases = Cases[DataSources[], SQLConnection[___, "Name"->dataSourceName,___]];
    If[cases === {}, Null, First[cases]]
  ];

DataSourceNames[] := 
  ("Name" /. canonicalOptions[Options[#]] /. Options[SQLConnection]) & /@ DataSources[];

queryQ[file_String] := 
  Module[{is, word},
    is = OpenRead[file];
    word = Read[is, Word , WordSeparators -> {" ", "\n", "\r", "\t", "["}];
    Close[is];
    word === "SQLSelect"
  ]

SQLQueries[] := 
  Cases[ Flatten[FileNames["*.m", First[#]]& /@ DatabaseResourcesPath[]], 
    file_String/;(FileType[file] =!= Directory && queryQ[file]):>Append[Get[file], "Location" -> file]]; 

SQLQueryNames[] := 
  ("Name" /. canonicalOptions[Options[#]] /. Options[SQLSelect]) & /@ SQLQueries[];

WriteDataSource[name_String, opts:OptionsPattern[]] := 
  WriteDataSource[name, "HSQL(Standalone)", opts]

WriteDataSource[name_String, driver_String, opts:OptionsPattern[]] := 
  Module[{desc, url, user, passwd, ucp, ro, til, cat, props, loc, i, rel, conn, 
          dbrdir, dir, useOpts},
          
     useOpts = canonicalOptions[Flatten[{opts}]];     
     desc = "Description" /. useOpts /. Options[ WriteDataSource ]; 
     url = "URL" /. useOpts /. Options[WriteDataSource];
     user = "Username" /. useOpts /. Options[ WriteDataSource ]; 
     passwd = "Password" /. useOpts /. Options[ WriteDataSource ]; 
     props = "Properties" /. useOpts /. Options[ WriteDataSource ]; 
     loc = "Location" /. useOpts /. Options[ WriteDataSource ];
     rel = "RelativePath" /. useOpts /. Options[ WriteDataSource ]; 
     ucp = "UseConnectionPool" /. useOpts /. Options[ WriteDataSource ]; 
     cat = "Catalog" /. useOpts /. Options[ WriteDataSource ]; 
     ro = "ReadOnly" /. useOpts /. Options[ WriteDataSource ]; 
     til = "TransactionIsolationLevel" /. useOpts /. Options[ WriteDataSource ]; 
     
     Switch[loc,
       "User", 
         dbrdir = $UserBaseDirectory;
         dir = ToFileName[{$UserBaseDirectory, "DatabaseResources"}],
       "System", 
         dbrdir = $BaseDirectory;
         dir = ToFileName[{$BaseDirectory, "DatabaseResources"}], 
       _, 
         Message[WriteDataSource::location, loc];
         Return[$Failed]
     ]; 

     If[url === Automatic, 
       Switch[driver, 
         "HSQL(Standalone)" | "H2(Embedded)",
           url = name;
           If[user === Automatic, user = None];
           If[rel === Automatic, rel = True], 
         "Derby(Embedded)",
           url = name <> ";create=true",
         "HSQL(Memory)", 
           url = name;
           If[user === Automatic, user = None], 
         "SQLite(Memory)" | "H2(Memory)", 
           url = "";
           If[user === Automatic, user = None], 
         _, 
           Message[WriteDataSource::url, driver];
           Return[$Failed] 
       ];
     ];
     If[user === Automatic, user = None];
     If[passwd === Automatic, passwd = None];
     If[props === Automatic, props = {}];
     
     (* Make a unique name *) 
     loc = name <> ".m";
     i = 0;
     While[FileNames[loc, dir] =!= {},
       loc = name <> "(" <> ToString[++i] <> ").m"
     ];
     loc = ToFileName[dir, loc];
          
     conn = SQLConnection[JDBC[driver, url],
                             "Name" -> name,
                             "Description" -> desc,
                             "Username" -> user, 
                             "Password" -> passwd,
                             "Properties" -> props,
                             "RelativePath" -> TrueQ[rel],
                             "UseConnectionPool" -> ucp,
                             "Catalog" -> cat,
                             "ReadOnly" -> ro,
                             "TransactionIsolationLevel" -> til,
                             "Version" -> DatabaseLink`Information`$VersionNumber];
     
     If[FileNames["DatabaseResources", {dbrdir}] === {},  
       CreateDirectory[dir];
     ];
     
     If[MemberQ[DataSourceNames[], name], 
       Message[WriteDataSource::exists, name];
       $Failed, 
       Put[conn, loc];
       conn
     ]     
  ]

End[]
