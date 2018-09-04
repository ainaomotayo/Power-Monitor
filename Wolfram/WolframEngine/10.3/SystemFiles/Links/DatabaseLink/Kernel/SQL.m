(* Author:          Christopher Williamson *)
(* Copyright:       Copyright 2004-2013, Wolfram Research, Inc. *)

Begin[ "`SQL`Private`"] 

Needs["JLink`"];

(*===================================================================*)
(*=================== Global Variables Defaults =====================*)
(*===================================================================*)

$SQLTimeout = Automatic;

$SQLUseConnectionPool = False;

Spew[args___] := If[TrueQ[$Debug], Print[args]];

(*===================================================================*)
(*=================== Error Messages ================================*)
(*===================================================================*)

JDBC::error = "`1`"

JDBC::classnotfound = "`1`"

SQLBeginTransaction::nested = 
"Nested transactions are not allowed.  Continuing with the first \
transaction."

SQLExecute::columnsymbols = 
"Illegal value for ColumnSymbols option: `1`"

SQLExecute::maxrows = 
"Illegal value for MaxRows option: `1`"

SQLExecute::timeout = 
"Illegal value for Timeout option: `1`"

SQLExecute::fetchsize = 
"Illegal value for FetchSize option: `1`"

SQLExecute::fetchdirection = 
"Illegal value for FetchDirection option: `1`"

SQLExecute::maxfieldsize = 
"Illegal value for MaxFieldSize option: `1`"

SQLExecute::multirowodbc =
"Possible inline multirow insert; not allowed by ODBC Access and Excel drivers. Try parameterized query if problems persist."

SQLResultSet::forwardonly = 
"This SQLResultSet is required to be ForwardOnly."

SQLResultSetOpen::mode = 
"Invalid SQLResultSet mode: `1`"

SQLResultSetTake::invalidrange = 
"Invalid range: `1`"

SQLSetSavepoint::version = 
"This feature requires Java 1.4."

SQLReleaseSavepoint::sqlsavepoint = 
"Invalid sqlsavepoint: `1`"

SQLReleaseSavepoint::javasavepoint = 
"Invalid Java savepoint: `1` (in `2`)"

SQLValue::illegalvalue = 
"The value `1` cannot be converted to a value in an SQL statement."

SQLServerLaunch::address = 
"Invalid value for address: `1`"

SQLServerLaunch::port = 
"Invalid value for port: `1`"

SQLConnection::optreset = 
"Option `1` cannot be reset in `2`"


ThrowException[symbol_Symbol, tagname_String, message_String] := 
  Module[{exception = GetJavaException[]}, 
    Which[
      InstanceOf[exception, LoadJavaClass["java.lang.ClassNotFoundException"]],
         Message[JDBC::classnotfound, exception@getMessage[]],         
      exception@getMessage[] === Null,       
        JLink`Exceptions`Private`$internalJavaExceptionHandler[symbol, tagname, message],
      True,
        Message[JDBC::error, exception@getMessage[]]
    ];
    Throw[$Failed];
  ];

(*===================================================================*)
(*=================== Options =======================================*)
(*===================================================================*)


(*
  If any of opts are options of sym then we issue a message 
  saying that these cannot be reset.  Else we issue a message 
  saying that these are not options of sym.
*)

optionsErrorMessage[opts_, sym_, e_] :=
	Module[ {notOpts, notResetOpts},
		notOpts = FilterRules[ opts, Except[Options[ sym]]];
		Scan[ Message[sym::optx, #1, e]&, Map[ First, notOpts]];
		notResetOpts = FilterRules[ opts, Options[ sym]];
		Scan[ Message[sym::optreset, #1, e]&, Map[ First, notResetOpts]];
	]



optionsMessage[optTest, OpenSQLConnection]

    
Options[ SQLResultSet] = 
    {
      "FetchDirection" -> Automatic,
      "FetchSize" -> Automatic
    }
    
Options[ SQLResultSetOpen ] = 
	JoinOptions[
	    {
    	  "Mode"->"ScrollInsensitive"
    	},
    	Options[ SQLResultSet]
    ]


Options[ SQLExecute ] = 
    {
      "ColumnSymbols" -> None, 
      "EscapeProcessing"->True,
      "FetchDirection"->"Forward",
      "FetchSize"->Automatic, 
      "GetAsStrings" -> False, 
      "GetGeneratedKeys"->False,
      "MaxFieldSize"->Automatic,
      "MaxRows" -> Automatic, 
      "ShowColumnHeadings" -> False,
      "Timeout" :> $SQLTimeout,
      "BatchSize" -> 1000,
      "JavaBatching" -> True
    }

Options[ SQLResultSetRead ] = 
    { 
      "GetAsStrings" -> False
    }

Options[ SQLResultSetCurrent ] = 
    { 
      "GetAsStrings" -> False
    }

Options[ SQLResultSetTake ] = 
    { 
      "GetAsStrings" -> False
    }


Options[ SQLServer ] = 
    {
      "Name" -> "", 
      "Description" -> "", 
      "Address"->Automatic,
      "Port"->Automatic,
      "SecureSockets"->False,
      "Version"->""
    }
    
Options[ SQLServerLaunch ] =
  Options[SQLServer]
  
Options[SQLSavepoint] =
    {
      "Name"->""
    }

(*===================================================================*)
(*======================= JDBC Functionality ========================*)
(*===================================================================*)
  
Options[ JDBCDriver ] = 
    {
      "Name" -> "" , 
      "Description" -> "" , 
      "Driver" -> "", 
      "Protocol" -> "",
      "Location" -> "",
      "Version" -> ""
    }

JDBCDrivers[] := 
  Cases[ Flatten[FileNames["*.m", First[#]]& /@ DatabaseResourcesPath[]], 
    file_String/;(FileType[file] =!= Directory && jdbcDriverQ[file]):>Append[Get[file], "Location" -> file]];  

JDBCDrivers[driverName_String]:=
  Module[{cases},
    cases = Cases[JDBCDrivers[], JDBCDriver[___, "Name"->driverName,___]];
    If[cases === {}, Null, First[cases]]
  ];
    
JDBCDriverNames[] := 
  ("Name" /. canonicalOptions[Options[#]] /. Options[ JDBCDriver]) & /@ JDBCDrivers[];

jdbcDriverQ[file_String] := 
  Module[{is, word},
    is = OpenRead[file];
    word = Read[is, Word , WordSeparators -> {" ", "\n", "\r", "\t", "["}];
    Close[is];
    word === "JDBCDriver"
  ]

(*===================================================================*)
(*=================== SQLConnection Functionality ===================*)
(*===================================================================*)

Options[ SetSQLConnectionOptions ] = 
    {
      "Catalog"->Automatic,
      "ReadOnly"->Automatic,
      "TransactionIsolationLevel"->Automatic       
    }

Options[ SQLConnection ] = 
    JoinOptions[
      {
        "Name" -> None, 
        "Description" -> None, 
        "Username" -> None, 
        "Password" -> None,
        "Properties"->{},
        "Location" -> None,
        "RelativePath" -> False,
        "UseConnectionPool"->Automatic,
        "Version"->None
      },
      Options[SetSQLConnectionOptions] 
    ]

Options[ OpenSQLConnection ] = 
    JoinOptions[
      Options[SQLConnection],
      {
        "Timeout" :> $SQLTimeout
      }
    ]

Options[ SQLConnectionWarnings ] = 
    {
      "ShowColumnHeadings" -> True
    }

SQLConnection::conn = "Connection is not opened properly."

OpenSQLConnection::driver = "Illegal Driver value specified by the JDBCDriver: `1`"
OpenSQLConnection::location = "When the RelativePath option is set to True, the Location option must be a string: `1`"
OpenSQLConnection::notfound = "DataSource not found: `1`"
OpenSQLConnection::password = "Illegal value for Password option: `1`"
OpenSQLConnection::properties = "Illegal value for Properties option: `1`"
OpenSQLConnection::protocol = "Illegal Protocol value specified by the JDBCDriver: `1` (ignoring value)"
OpenSQLConnection::relativepath = "Illegal value for RelativePath option: `1`"
OpenSQLConnection::sqltimeout = "Illegal value for $SQLTimeout: `1`"
OpenSQLConnection::sqluseconnectionpool = "Illegal value for $SQLUseConnectionPool: `1`"
OpenSQLConnection::timeout = "Illegal value for Timeout option: `1` (continuing with option default value)"
OpenSQLConnection::useconnectionpool = "Illegal value for UseConnectionPool option: `1` (continuing with default value)"
OpenSQLConnection::username = "Illegal value for Username option: `1`"

SQLConnectionUsableQ::notest = "No test query available for driver `1`; use two-argument form"

SQLConnection::til = "Illegal value for TransactionIsolationLevel option: `1`"
SQLConnection::readonly = "Illegal value for ReadOnly option: `1`"
SQLConnection::catalog = "Illegal value for Catalog option: `1`"

$connectionIndex = 0;
$poolIndex = 0;

If[!ListQ[$sqlConnections], 
  $sqlConnections = {}
];

SQLConnections[] := $sqlConnections;

OpenSQLConnection[ JDBC[driver_String, url_String],
                   opts:OptionsPattern[]] := 
  JavaBlock[
    Module[ {result, useOpts, name, username, password, useConnectionPool, location, relativePath, timeout,
             properties, readOnly, transactionIsolationLevel, catalog,
             u = url, d = driver, drivers, jdbc, protocol, props,
             connectionPool = Null, basicDataSource, to, id, connection, conn, fromPool = False},

      Block[{$JavaExceptionHandler = ThrowException},
        result = Catch[

          (* Process options 
           * Description and Version are informational options that are not used here. *)
          useOpts = canonicalOptions[Flatten[{opts}]];
          name = "Name" /. useOpts /. Options[OpenSQLConnection];
          location = "Location" /. useOpts /. Options[ OpenSQLConnection ];
          relativePath = "RelativePath" /. useOpts /. Options[ OpenSQLConnection ];
          username = "Username" /. useOpts /. Options[ OpenSQLConnection ];
          password = "Password" /. useOpts /. Options[ OpenSQLConnection ];
          timeout = "Timeout" /. useOpts /. Options[OpenSQLConnection];
          useConnectionPool = "UseConnectionPool" /. useOpts /. Options[OpenSQLConnection];
          properties = "Properties" /. useOpts /. Options[OpenSQLConnection];
          readOnly = "ReadOnly" /. useOpts /. Options[OpenSQLConnection];
          transactionIsolationLevel = "TransactionIsolationLevel" /. useOpts /. Options[OpenSQLConnection];
          catalog = "Catalog" /. useOpts /. Options[OpenSQLConnection];

          (* Location specifies the filename that is used to store the connection.  
             RelativePath uses the Location directory as the base directory for the URL parameter. 
             For file URLs this will allow the URL to find the database relative to the Location 
             directory.  However, this is not applicable to other URLs, so RelativePath should 
             be set to False when using URLs that are not file based. If RelativePath is set to 
             True, the URL will be treated as a file URL and the base directory will be set. *)

          Switch[relativePath, 
            False, Null, 
            True, 
              If[StringQ[location],
              	u = FileNameJoin[{If[DirectoryQ[location], location, DirectoryName[location]], u}],
                Message[OpenSQLConnection::location, location];
                Return[$Failed]
              ],  
            _, 
              (* Since the user is attempting to use relativePath it appears the default location is not 
                 acceptable to the user. Since continuing using a default would result in the database 
                 being created in a place the user does not wish, $Failed is returned when 
                 RelativePath is invalid. *)
              Message[OpenSQLConnection::relativepath, relativePath];
              Return[$Failed]
          ];
      
          (* The driver parameter may be used to specify a JDBCDriver configuration.
             If the driver value is found among the names of JDBCDriver configurations, 
             then the driver value will be set to the value of the driver option specified 
             in the JDBCDriver configuration.  This must be a Java class to work correctly.  
             Also the protocol specified within the JDBCDriver configuration is prepended 
             to the URL, if it is not already there. This saves users from having to 
             remember complicated protocols. *)
          drivers = Select[JDBCDrivers[], ("Name" /. canonicalOptions[Options[#]] /. Options[JDBCDriver]) === driver &];
          If[Length[drivers] > 0, 
            jdbc = First[drivers];
            {d, protocol} = {"Driver", "Protocol"} /. canonicalOptions[Options[jdbc]] /. Options[ JDBCDriver ];
            Which[
              !StringQ[d], 
                (* If the driver is not a String, $Failed is returned.  This cannot be fixed at this point. *)
                Message[OpenSQLConnection::driver, d];
                Return[$Failed],
              !StringQ[protocol], 
                (* If the protocol is not a String, a Message is returned, but the function 
                   ignores the protocol and continues on. It could be that the user has 
                   already specified the correct protocol. *)
                Message[OpenSQLConnection::protocol, protocol],
              !StringMatchQ[u, protocol <> "*"],
                u = protocol <> u
            ]
          ];

          (* Initialize Java and Java classes *)
          Spew@"butt 1";
          InstallJava[];
          LoadJavaClass["com.wolfram.jlink.JLinkClassLoader"];
          LoadJavaClass["com.wolfram.databaselink.JDBCConnectionManager"];

          (* Initialize the JDBC driver.  This is required per JDBC. *)
          Spew@d;
          JLinkClassLoader`classFromName[d]@newInstance[];
          Spew@"butt 2";

		  (* Set useConnectionPool to the global default if invalid. *)
		  If[useConnectionPool === Automatic, useConnectionPool = $SQLUseConnectionPool];
		  If[useConnectionPool =!= True && useConnectionPool =!= False,
		      Message[OpenSQLConnection::useconnectionpool, useConnectionPool];
		      useConnectionPool = $SQLUseConnectionPool
		  ];
		
		  (* Process the useConnectionPool option *)
		  Switch[useConnectionPool, 
		      True | False, Null, 
		      (* A Message is returned if the value is not True or False.  However, TrueQ 
		         is called on the value, so if the value is not True then it is False. 
		         So this does not fail for invalid values.  Rather it tries to continue by 
		         not using connection pools. *)
		      _, Message[OpenSQLConnection::sqluseconnectionpool, useConnectionPool];
		  ];

          (* If specified by Password, prompt for the password.*)
          If[(StringQ[password] && StringMatchQ[password, "$Prompt"]) && 
             (* Prompt only if the connection pool has not started or if a connection pool is not used *)
             ((TrueQ[useConnectionPool] && SQLConnectionPools[SQLConnection[JDBC[driver, url], opts]] === Null) || 
              !TrueQ[useConnectionPool]), 
            (* 
             * PasswordDialog uses DialogInput and will hang the FE if OpenSQLConnection has been called from within
             * another DialogInput. If calling from another DialogInput, intercept the password check
             * and use NestablePasswordDialog. See DataSourceWizard.m for an example of this technique.
             *)
            {username, password} = DatabaseLink`UI`Private`PasswordDialog[{username, None}];
          ];

          (*
           * Set the timeout to the global default if invalid.
           * setLoginTimeout(int seconds) only
           *)
          If[(!IntegerQ[timeout] && timeout =!= None && timeout =!= Automatic) || timeout <= 0, 
            Message[OpenSQLConnection::timeout, timeout];
            timeout = $SQLTimeout;
          ];

          (* Process the timeout option *)
          Switch[timeout,
          	(* These will effectively fall back on driver defaults in JDBCConnectionManager. *)
            None|Automatic,
              timeout = 0,
            _Integer?NonNegative,
              Null,
            _,
              Message[OpenSQLConnection::sqltimeout, timeout];
              Return[$Failed];
          ];

          (* Process the properties including Username and Password *)
          If[MatchQ[properties, {(_String -> _String) ...}], 
		    (*
		     * Bug 249675: HSQL in standalone mode won't delete lock files on connection close, unless
		     * a SHUTDOWN command is issued first or the shutdown property is true. Properties are processed in order,
		     * so this can be overridden by user settings.
		     *)
		     If[d === "HSQL(Standalone)", PrependTo[properties, "shutdown" -> "true"]];
            props = JavaNew["java.util.Properties"];
            props@setProperty[First[#], Last[#]] & /@ properties;
            Switch[username, 
              None, 
                Null, 
              _String, 
                Switch[password, 
                  None, 
                    Null, 
                  _String, 
                    (* Only set username and password if they are both strings *)
                    props@setProperty["user", username];
                    props@setProperty["password", password], 
                  _, 
                    Message[OpenSQLConnection::password, password]
                ], 
              _, 
                Message[OpenSQLConnection::username, username]
            ];
            ,
            (* Since the properties may be very important to how a connection is made, 
               $Failed is returned when invalid Properties are received. *)
            Message[OpenSQLConnection::properties, properties];
            Return[$Failed]
          ];
          
          If[TrueQ[useConnectionPool],
            (* Make connection using connection pool *)
            connectionPool = SQLConnectionPools[SQLConnection[JDBC[driver, url], opts]];
            If[connectionPool === Null,
              basicDataSource = JDBCConnectionManager`getPool[d, u, props];
              connectionPool = 
                SQLConnectionPool[
                  basicDataSource,
                  JDBC[driver, url],
                  ++$poolIndex, 
                  Sequence@@DeleteCases[useOpts, "Timeout" -> _]
                ];
              AppendTo[$connectionPools, connectionPool];
              AppendTo[$poolToConnections, $poolIndex -> {}];
              KeepJavaObject[basicDataSource];
              , 
              basicDataSource = First[connectionPool];
            ];
            
            (* Set the pool options here so when a user specifies a dynamic 
               property, it will be updated in an existing pool. *)
            SetSQLConnectionPoolOptions[connectionPool, Sequence@@FilterRules[useOpts, Options[SetSQLConnectionPoolOptions]]];

            to = basicDataSource@getMaxWait[];
            basicDataSource@setMaxWait[timeout*1000];
            connection = basicDataSource@getConnection[];
            fromPool = True;
            basicDataSource@setMaxWait[to];
            ,
            (* else *)
            (* Make connection without using connection pool. *)
            Spew@"butt 3";
            Spew[u, props, timeout];
            connection = JDBCConnectionManager`getConnection[u, props, timeout];
            Spew@"butt 3.3";
          ];

          Spew@"butt 4";
          
          (* Setup SQLConnection expression *)
          id = ++$connectionIndex;
          conn = 
            SQLConnection[
              JDBC[
                driver, 
                url
              ],
              connection,
              id, 
              opts
            ];
          
          (* Set options that may be configured dynamically. *)  
          conn = SetSQLConnectionOptions[conn, "ReadOnly"->readOnly, "TransactionIsolationLevel"->transactionIsolationLevel, "Catalog"->catalog];
          
          (* Protect the connection from cleanup in Java *)
          KeepJavaObject[connection];
          
          (* Add SQLConnection to the list of open connections. *)
          AppendTo[$sqlConnections, conn];
          If[TrueQ@fromPool, 
            AppendTo[$poolToConnections[$poolIndex], conn];
          ];
          conn
        ];
        If[result === $Failed && TrueQ[useConnectionPool] && connectionPool =!= Null, 
          SQLConnectionPoolClose[connectionPool];
        ];
        result
      ]
    ] 
  ]
    
OpenSQLConnection[SQLConnection[jdbc_JDBC, opts:OptionsPattern[]], opts2:OptionsPattern[]] := 
  OpenSQLConnection[SQLConnection[jdbc, Null, -1, opts], opts2]

OpenSQLConnection[SQLConnection[
                    jdbc_JDBC,
                    _,
                    _Integer,
                    opts:OptionsPattern[]], 
                  opts2:OptionsPattern[]] := 
  Module[{cat, desc, location, name, pw, ro, relativePath, to, transactionIsolationLevel, un, v, ucp, properties},

    (* The options are processed, so that options specified in the connection may be overriden 
       by options specified in the function. *)
    {cat, desc, location, name, pw, properties, ro, relativePath, to, transactionIsolationLevel, ucp, un, v} = 
      {"Catalog", "Description", "Location", "Name", "Password", 
       "Properties", "ReadOnly", "RelativePath", "Timeout", 
       "TransactionIsolationLevel", "UseConnectionPool", "Username", "Version"} 
         /. canonicalOptions[Flatten[{opts2}]] /. canonicalOptions[Flatten[{opts}]] /. Options[OpenSQLConnection];

    OpenSQLConnection[jdbc, "Catalog"->cat, "Description"->desc, "Location"->location, "Name"->name, "Password"->pw,
                            "Properties"->properties, "ReadOnly"-> ro, "RelativePath"->relativePath, "Timeout"->to,                             
                            "TransactionIsolationLevel"->transactionIsolationLevel, "UseConnectionPool"->ucp, "Username"->un, 
                            "Version"->v]
  ]

OpenSQLConnection[name_String, opts:OptionsPattern[]] := 
  Module[{list},
    list = Cases[DataSources[], 
      conn_ /; ("Name" /. canonicalOptions[Options[conn]] /. Options[SQLConnection]) === name];
    If[Length[list] > 0,
      OpenSQLConnection[First[list], opts], 
      Message[OpenSQLConnection::notfound, name];
      $Failed
    ]
  ]

SQLConnection /:
	SetOptions[ SQLConnection[jdbc_JDBC,
                          connection_,
                          id_Integer,
                          opts:OptionsPattern[]], opts2___] := 
		SetSQLConnectionOptions[ SQLConnection[jdbc, connection, id, opts], opts2]


SetSQLConnectionOptions[SQLConnection[
                          jdbc_JDBC,
                          connection_,
                          id_Integer,
                          opts:OptionsPattern[]], 
                        opts2:OptionsPattern[]] := 
  Module[{cat, desc, location, name, pw, properties, ro, relativePath, til, un, ucp, v, conn, optTest},
    Block[{$JavaExceptionHandler = ThrowException},
      Catch[
        {desc, location, name, pw, properties, relativePath, ucp, un, v} = 
          {"Description", "Location", "Name", "Password", "Properties", 
           "RelativePath", "UseConnectionPool", "Username", "Version"} /.
             canonicalOptions[Flatten[{opts}]] /. Options[OpenSQLConnection];
    
    	optTest = FilterRules[ {opts2}, Except[Options[SetSQLConnectionOptions]]];
    	If[ optTest =!= {}, optionsErrorMessage[optTest, SQLConnection, SQLConnection]; Return[$Failed]];
    	
        {cat, ro, til} = 
          {"Catalog", "ReadOnly", "TransactionIsolationLevel"} /.
             canonicalOptions[Flatten[{opts2}]] /. canonicalOptions[Flatten[{opts}]] /. Options[OpenSQLConnection];
    
        If[!JavaObjectQ[connection], 
          Message[SQLConnection::conn];
          Return[$Failed]
        ]; 
         
        (* Catalog *)
        Switch[cat, 
          _?StringQ, 
            connection@setCatalog[cat],
          Automatic, 
            Null,
          _, 
            Message[SQLConnection::catalog, cat];
            cat = "Catalog" /. canonicalOptions[Flatten[{opts}]] /. Options[OpenSQLConnection]        
        ];
    
        (* Transaction Isolation Level *)
        Switch[til, 
          "ReadUncommitted",
            connection@setTransactionIsolation[1],
          "ReadCommitted",
            connection@setTransactionIsolation[2],
          "RepeatableRead",
            connection@setTransactionIsolation[4],
          "Serializable",
            connection@setTransactionIsolation[8],
          Automatic, 
            Null,
          _, 
            Message[SQLConnection::til, til];
            til = "TransactionIsolationLevel" /. canonicalOptions[Flatten[{opts}]] /. Options[OpenSQLConnection]
        ];
    
        (* Read Only *)
        Switch[ro, 
          (True | False), 
            connection@setReadOnly[ro],
          Automatic, 
            Null,
          _, 
            Message[SQLConnection::readonly, ro];
            ro = "ReadOnly" /. canonicalOptions[Flatten[{opts}]] /. Options[OpenSQLConnection]
        ];
    
        conn = SQLConnection[jdbc, connection, id, 
	                      "Catalog"->cat, "Description"->desc, "Location"->location, "Name"->name, "Password"->pw,
	                      "Properties"->properties, "ReadOnly"-> ro, "RelativePath"->relativePath,                           
	                      "TransactionIsolationLevel"->til, "UseConnectionPool"->ucp, "Username"->un, "Version"->v];
	    $sqlConnections = ReplaceAll[$sqlConnections, SQLConnection[_, _, id, ___] -> conn];
	    conn
	  ]
    ]
  ];


CloseSQLConnection[ SQLConnection[
                      _JDBC,
                      connection_,
                      id_Integer,
                      ___Rule]] :=
  Block[{$JavaExceptionHandler = ThrowException},
    Catch[
	  If[JavaObjectQ[connection],
	    If[!connection@isClosed[],
	      connection@close[]
	    ];
	    ReleaseJavaObject[connection];
	    $inTransaction = False;
	    $sqlConnections = Drop[ $sqlConnections, 
	                            First@Position[ $sqlConnections, 
	                                            SQLConnection[_, _, id, ___Rule]]]
      ]
    ]
  ]  

(*
 * http://download.oracle.com/javase/1.4.2/docs/api/java/sql/Connection.html#isClosed()
 * This method itself is not reliable enough to test for usable connections.
 *)
SQLConnectionOpenQ[SQLConnection[_JDBC, conn_, id_Integer, opts:OptionsPattern[]]] := 
    (JavaObjectQ[conn] && !conn@isClosed[]);
    
SQLConnectionOpenQ[_] = False;

(*
 * This executes a query on the passed connection and may raise an exception in some
 * cases, like an open streaming result set on the connection. Note there isn't a 1:1 mapping
 * between the JDBC driver and the RDBMS, but there isn't a clean way to infer the RDBMS from 
 * the connection object.
 *)
$usabilityTests = {
	StartOfString ~~ "Oracle" ~~ ___ ~~ EndOfString -> {"SELECT 1 FROM DUAL", {{1.}}}
	, StartOfString ~~ "HSQL Database Engine" ~~ ___ ~~ EndOfString -> {"SELECT 1 FROM INFORMATION_SCHEMA.SYSTEM_USERS", {{1}}}
    , StartOfString ~~ "Firebird" ~~ ___ ~~ EndOfString -> {"SELECT 1 FROM RDB$RELATIONS WHERE 1=0", {}}
    , StartOfString ~~ "ACCESS" ~~ ___ ~~ EndOfString -> {"SELECT 1", {{1}}}
    , StartOfString ~~ "EXCEL" ~~ ___ ~~ EndOfString -> {"SELECT 1", {{1}}}
    , StartOfString ~~ "Microsoft SQL Server" ~~ ___ ~~ EndOfString -> {"SELECT 1", {{1}}}
    , StartOfString ~~ "MySQL" ~~ ___ ~~ EndOfString -> {"SELECT 1", {{1}}}
    , StartOfString ~~ "PostgreSQL" ~~ ___ ~~ EndOfString -> {"SELECT 1", {{1}}}
    , StartOfString ~~ "SQLite" ~~ ___ ~~ EndOfString -> {"SELECT 1", {{1}}}
    , StartOfString ~~ "H2" ~~ ___ ~~ EndOfString -> {"SELECT 1", {{1}}}
    , StartOfString ~~ "Apache Derby" ~~ ___ ~~ EndOfString -> {"VALUES 1", {{1}}}
	(*, StartOfString ~~ __ ~~ EndOfString -> {"SELECT 1", {{1}}}*)
};

SQLConnectionUsableQ[conn_SQLConnection] := 
    SQLConnectionUsableQ[conn, StringCases[getRDBMS[conn], $usabilityTests]] /;
    SQLConnectionOpenQ[conn];
    
SQLConnectionUsableQ[conn_SQLConnection, {}] := (
    Message[SQLConnectionUsableQ::notest, getRDBMS[conn]];
    (* Fall through to SELECT 1 *)
    SQLConnectionUsableQ[conn, {"SELECT 1", {{1}}}]
)
    
SQLConnectionUsableQ[conn_SQLConnection, {{testSQL_String, res_}}] := 
    SQLConnectionUsableQ[conn, {testSQL, res}];
    
SQLConnectionUsableQ[conn_SQLConnection, {testSQL_String, res_}] := (
    MatchQ[Quiet@SQLExecute[conn, testSQL], res]
)

SQLConnectionUsableQ[_] = False;

(*
 * Given an SQLConnection returns the RDBMS as a verboseish string.
 * This is the basic unit of reliability in doing dialect switches.
 * This works for connections pulled from a pool, or not.
 *)
getRDBMS[SQLConnection[_JDBC, conn_?JavaObjectQ, _Integer, ___Rule]] := 
    getRDBMS[conn];

getRDBMS[conn_?JavaObjectQ] := 
    conn@getMetaData[]@getDatabaseProductName[];


SQLConnectionInformation[SQLConnection[
                         _JDBC,
                         connection_,
                         _Integer,
                         ___Rule]] := 
  Block[{$JavaExceptionHandler = ThrowException},
    Catch[
      If[!JavaObjectQ[connection], 
        Message[SQLConnection::conn];
        Return[$Failed]
      ];
      
      LoadJavaClass["com.wolfram.databaselink.SQLStatementProcessor"];
      SQLStatementProcessor`getConnectionMetaData[ connection ]
    ]
  ]

SQLConnectionInformation[SQLConnection[
                         _JDBC,
                         connection_,
                         _Integer,
                         ___Rule], (metaDataItem_String | metaDataItem_List)] := 
  Block[{$JavaExceptionHandler = ThrowException},
    Catch[
      If[!JavaObjectQ[connection], 
        Message[SQLConnection::conn];
        Return[$Failed]
      ];
      
      LoadJavaClass["com.wolfram.databaselink.SQLStatementProcessor"];
      SQLStatementProcessor`getConnectionMetaData[ connection, metaDataItem ]
    ]
  ]

SQLConnectionWarnings[SQLConnection[ _JDBC, connection_, _Integer, ___Rule], opts:OptionsPattern[]] :=
    JavaBlock[
      Module[ {useOpts,sch,warn,warnList},
          Block[ {$JavaExceptionHandler = ThrowException},
              Catch[
                  
                  useOpts = canonicalOptions[Flatten[{opts}]];
                  warnList = {};
                  
                  sch = "ShowColumnHeadings" /. useOpts /. Options[ SQLConnectionWarnings];
                  If[ TrueQ[sch],
                      AppendTo[warnList,{"Message","SQLState","ErrorCode"}]
                  ];
                  
                  If[ !JavaObjectQ[connection],
                      Message[SQLConnection::conn];
                      Return[$Failed]
                  ];
                  
                  warn = connection@getWarnings[];
                  
                  (* During testing the below warning messages were created/forced to test
                       the While loop and front-end display, since a connection warning
                       could not be caused.
                       
                  warn=JavaNew["java.sql.SQLWarning","HELP"];
                  warn2=JavaNew["java.sql.SQLWarning","HELP2"];
                  warn@setNextWarning[warn2];
                  *)
                  
                  While[warn=!=Null,
                      AppendTo[warnList,{warn@getMessage[],warn@getSQLState[],warn@getErrorCode[]}];
                      warn = warn@getNextWarning[];
                  ];
                  warnList
              ]
          ]
      ]
    ]

(*===================================================================*)
(*================= SQLConnectionPool Functionality =================*)
(*===================================================================*)

Options[ SetSQLConnectionPoolOptions ] = 
  JoinOptions[
    Options[ SetSQLConnectionOptions ], 
    {
      "MaximumActiveConnections"->Automatic,
      "MaximumIdleConnections"->Automatic,
      "MinimumIdleConnections"->Automatic
    }
  ]
  
Options[ SQLConnectionPool ] = 
	Options[ SetSQLConnectionPoolOptions]
 

SQLConnectionPool::til = "Illegal value for TransactionIsolationLevel option: `1`"
SQLConnectionPool::readonly = "Illegal value for ReadOnly option: `1`"
SQLConnectionPool::catalog = "Illegal value for Catalog option: `1`"
SQLConnectionPool::maxactive = "Illegal value for MaximumActiveConnections option: `1`"
SQLConnectionPool::maxidle = "Illegal value for MaximumIdleConnections option: `1`"
SQLConnectionPool::minidle = "Illegal value for MinimumIdleConnections option: `1`"

$connectionPools = {};

If[!ListQ[$connectionPools], 
  $connectionPools = {};
];

(*
 * Maintain mapping of which connections are associated with which pools, so that
 * we can close connections on pool closure.
 *)
If[!MatchQ[$poolToConnections, _Association], 
  $poolToConnections = Association[];
];

SQLConnectionPools[] := $connectionPools;

SQLConnectionPools[SQLConnection[
                         jdbc_JDBC,
                         connection_,
                         id_Integer,
                         options:OptionsPattern[]]] := 
  Module[{list, desc, location, name, pw, relativePath, un, v, ucp, properties, cat, transactionIsolationLevel, ro},
  
    {cat, desc, location, name, pw, properties, ro, relativePath, transactionIsolationLevel, ucp, un, v} = 
      {"Catalog", "Description", "Location", "Name", "Password", 
       "Properties", "ReadOnly", "RelativePath", "TransactionIsolationLevel", 
       "UseConnectionPool", "Username", "Version"} 
         /. canonicalOptions[Flatten[{options}]] /. Options[OpenSQLConnection];
    
    list = Cases[ $connectionPools, 
                  SQLConnectionPool[
                    _?JavaObjectQ, 
                    jdbc, 
                    _Integer, 
                    "Catalog"->cat, "Description"->desc, "Location"->location, "Name"->name, "Password"->pw,
                    "Properties"->properties, "ReadOnly"-> ro, "RelativePath"->relativePath, 
                    "TransactionIsolationLevel"->transactionIsolationLevel, "UseConnectionPool"->ucp, 
                    "Username"->un, "Version"->v]];
    If[Length[list] > 0, 
      First[list]
    ]
  ]

SQLConnectionPools[SQLConnection[jdbc_JDBC, opts:OptionsPattern[]]] := 
  SQLConnectionPools[SQLConnection[jdbc, Null, -1, opts]]

SQLConnectionPools[name_String] := 
  Module[{dataSource = DataSources[name]},        
    If[dataSource =!= Null,
      SQLConnectionPools[dataSource]
    ]
  ]


SQLConnectionPool /:
	SetOptions[ SQLConnectionPool[
						javaObject_,
						jdbc_JDBC,
                        id_Integer,
                        opts:OptionsPattern[]], opts2___] := 
		SetSQLConnectionPoolOptions[ SQLConnectionPool[javaObject, jdbc, id, opts], opts2]



SetSQLConnectionPoolOptions[SQLConnectionPool[                                     
                                     javaObject_,
                                     jdbc_JDBC,
                                     id_Integer,
                                     opts:OptionsPattern[]], 
                                   opts2:OptionsPattern[]] := 
  Module[{cat, desc, location, name, pw, properties, ro, relativePath, til, 
          un, ucp, v, pool, maxActive, maxIdle, minIdle, optTest},
    
    Block[{$JavaExceptionHandler = ThrowException},
      Catch[
        {desc, location, name, pw, properties, relativePath, ucp, un, v} = 
          {"Description", "Location", "Name", "Password", "Properties", 
           "RelativePath", "UseConnectionPool", "Username", "Version"} /.
             canonicalOptions[Flatten[{opts}]] /. Options[OpenSQLConnection];
    
    	optTest = FilterRules[ {opts2}, Except[Options[SetSQLConnectionPoolOptions]]];
    	If[ optTest =!= {}, optionsErrorMessage[optTest, SQLConnectionPool, SQLConnectionPool]; Return[$Failed]];

        {cat, ro, til} = 
          {"Catalog", "ReadOnly", "TransactionIsolationLevel"} /.
             canonicalOptions[Flatten[{opts2}]] /. canonicalOptions[Flatten[{opts}]] /. Options[OpenSQLConnection];
    
        {maxActive, maxIdle, minIdle} = 
          {"MaximumActiveConnections", "MaximumIdleConnections", "MinimumIdleConnections"} /. 
            canonicalOptions[Flatten[{opts2}]] /. Options[SetSQLConnectionPoolOptions];
        
        If[!JavaObjectQ[javaObject], 
          Message[SQLConnection::conn];
          Return[$Failed]
        ]; 
         
        (* Catalog *)
        Switch[cat, 
          _?StringQ, 
            javaObject@setDefaultCatalog[cat],
          Automatic, 
            Null,
          _, 
            Message[SQLConnectionPool::catalog, cat];
            cat = "Catalog" /. canonicalOptions[Flatten[{opts}]] /. Options[OpenSQLConnection]
        ];
    
        (* Transaction Isolation Level *)
        Switch[til, 
          "ReadUncommitted",
            javaObject@setDefaultTransactionIsolation[1],
          "ReadCommitted",
            javaObject@setDefaultTransactionIsolation[2],
          "RepeatableRead",
            javaObject@setDefaultTransactionIsolation[4],
          "Serializable",
            javaObject@setDefaultTransactionIsolation[8],
          Automatic, 
            Null,
          _, 
            Message[SQLConnectionPool::til, til];
            til = "TransactionIsolationLevel" /. canonicalOptions[Flatten[{opts}]] /. Options[OpenSQLConnection]
        ];
    
        (* Read Only *)
        Switch[ro, 
          (True | False), 
            javaObject@setDefaultReadOnly[ro],
          Automatic, 
            Null,
          _, 
            Message[SQLConnectionPool::readonly, ro];
            ro = "ReadOnly" /. canonicalOptions[Flatten[{opts}]] /. Options[OpenSQLConnection]
        ];
    
        Switch[maxActive, 
          _Integer, 
            javaObject@setMaxActive[maxActive],
          Automatic, 
            Null,
          _, 
            Message[SQLConnectionPool::maxactive, maxActive]
        ];

        Switch[maxIdle, 
          _Integer, 
            javaObject@setMaxIdle[maxIdle],
          Automatic, 
            Null,
          _, 
            Message[SQLConnectionPool::maxidle, maxIdle]
        ];

        Switch[minIdle, 
          _Integer, 
            javaObject@setMinIdle[minIdle],
          Automatic, 
            Null,
          _, 
            Message[SQLConnectionPool::minidle, minIdle]
        ];
    
        pool = SQLConnectionPool[javaObject, jdbc, id, 
                      "Catalog"->cat, "Description"->desc, "Location"->location, "Name"->name, "Password"->pw,
                      "Properties"->properties, "ReadOnly"-> ro, "RelativePath"->relativePath,                             
                      "TransactionIsolationLevel"->til, "UseConnectionPool"->ucp, "Username"->un, "Version"->v];
        $connectionPools = ReplaceAll[$connectionPools, SQLConnectionPool[_, _, id, ___] -> pool];
        pool    
      ]
    ]
  ];

SQLConnectionPoolClose[ SQLConnectionPool[
                      javaObject_?JavaObjectQ,
                      jdbc_JDBC,
                      id_Integer,
                      options:OptionsPattern[]]] :=
  Block[{$JavaExceptionHandler = ThrowException},
    Catch[
      (* This will close all connections checked out from the pool. The documented Java behavior
       * is to leave connections "checked out to clients" unaffected by the pool closure; however,
       * if you try to close one of these connections after the pool is gone you get a
       * "pool not open" exception. So force closure of all pool connections on pool close.
       *)
      If[KeyExistsQ[$poolToConnections, id],
          (* Lots of Association stuff currently broken ... *)
          CloseSQLConnection[#] & /@ $poolToConnections[id];
          (*$poolToConnections = KeyDrop[$poolToConnections, id];*)
          $poolToConnections = Association[DeleteCases[Normal@$poolToConnections, id -> _]];
      ];
      javaObject@close[];
      ReleaseJavaObject[javaObject];
      $inTransaction = False;
      $connectionPools = DeleteCases[ $connectionPools, 
                                      SQLConnectionPool[
                                      _, 
                                      _,
                                      id,
                                      ___Rule]];
    ]
  ];

SQLConnectionPoolClose[SQLConnection[jdbc_JDBC, opts:OptionsPattern[]]] := 
  SQLConnectionPoolClose[SQLConnection[jdbc, Null, -1, opts]]

SQLConnectionPoolClose[conn:SQLConnection[
                         jdbc_JDBC,
                         connection_,
                         id_Integer,
                         options:OptionsPattern[]]] := 
  Module[{pool},
  
    pool = SQLConnectionPools[conn];
    If[pool =!= Null, 
      SQLConnectionPoolClose[pool]
    ]
  ]

SQLConnectionPoolClose[name_String] := 
  Module[{dataSource = DataSources[name]},        
    If[dataSource =!= Null,
      SQLConnectionPoolClose[dataSource]
    ]
  ]

(*===================================================================*)
(*==== Table, Column, and DataType Lookup Functionality =============*)
(*===================================================================*)

Options[ SQLTable ] = 
    { 
      "TableType" -> $DefaultTableType
    }

Options[ SQLTables ] = 
    { 
      "Catalog" -> None,
      "Schema" -> None,
      "TableType" -> $DefaultTableType 
    }

Options[ SQLTableNames ] = 
    { 
      "Catalog" -> None,
      "Schema" -> None,
      "TableType" -> $DefaultTableType 
    }

Options[ SQLTableInformation ] = 
    { 
      "Catalog" -> None,
      "Schema" -> None,
      "ShowColumnHeadings"->False,
      "TableType" -> $DefaultTableType
    }

Options[ SQLTablePrivileges ] = 
    { 
      "Catalog" -> None,
      "Schema" -> None,
      "ShowColumnHeadings"->False
    }

Options[ SQLTableExportedKeys ] = 
    { 
      "Catalog" -> None,
      "Schema" -> None,
      "ShowColumnHeadings"->False
    }

Options[ SQLTableImportedKeys ] = 
    { 
      "Catalog" -> None,
      "Schema" -> None,
      "ShowColumnHeadings"->False
    }

Options[ SQLTableIndexInformation ] = 
    { 
      "Catalog" -> None,
      "Schema" -> None,
      "ShowColumnHeadings"->False
    }

Options[ SQLTablePrimaryKeys ] = 
    { 
      "Catalog" -> None,
      "Schema" -> None,
      "ShowColumnHeadings"->False
    }
    
Options[ SQLTableVersionColumns] = 
    { 
      "Catalog" -> None, 
      "Schema" -> None, 
      "ShowColumnHeadings"->False
    }
    
Options[ SQLUserDefinedTypeInformation ] = 
    { 
      "Catalog" -> None,
      "Schema" -> None,
      "Types" -> None,
      "ShowColumnHeadings"->False
    }

Options[ SQLColumn ] = 
    { 
      "DataTypeName" -> None, 
      "DataLength" -> None,
      "Default" -> None,
      "Nullable" -> False,
      "PrimaryKey" -> False
    }

Options[ SQLColumns ] = 
    { 
      "Catalog" -> None, 
      "Schema" -> None
    }

Options[ SQLColumnNames ] = 
    { 
      "Catalog" -> None, 
      "Schema" -> None
    }

Options[ SQLColumnInformation] = 
    { 
      "Catalog" -> None, 
      "Schema" -> None, 
      "ShowColumnHeadings"->False
    }

Options[ SQLColumnPrivileges] = 
    { 
      "Catalog" -> None, 
      "Schema" -> None, 
      "ShowColumnHeadings"->False
    }

Options[ SQLDataTypeInformation ] = 
    { 
      "ShowColumnHeadings"->False
    }

Options[ SQLSchemaInformation ] = 
    { 
      "ShowColumnHeadings"->False
    }

If[ !StringQ[ $DefaultTableType],
    $DefaultTableType = "TABLE"];


SQLTableInformation::tabletype="Illegal value for TableType option: `1`"
SQLTableInformation::catalog="Illegal value for Catalog option: `1`"
SQLTableInformation::schema="Illegal value for Schema option: `1`"

SQLTableVersionColumns::catalog="Illegal value for Catalog option: `1`"
SQLTableVersionColumns::schema="Illegal value for Schema option: `1`"

SQLUserDefinedTypeInformation::types="Illegal value for Types option: `1`"
SQLUserDefinedTypeInformation::catalog="Illegal value for Catalog option: `1`"
SQLUserDefinedTypeInformation::schema="Illegal value for Schema option: `1`"

userDefinedTypeCheck::udtype="Illegal value for Types option: `1`"

SQLColumnInformation::catalog="Illegal value for Catalog option: `1`"
SQLColumnInformation::schema="Illegal value for Schema option: `1`"

SQLTableInformation[ SQLConnection[ _JDBC, connection_, _Integer, ___Rule], 
                     table_String | table:Null, opts:OptionsPattern[]] :=
  JavaBlock[
    Module[ {useOpts, tt, sch, meta, rs, schema, catalog}, 
      Block[{$JavaExceptionHandler = ThrowException}, 
        Catch[
          useOpts = canonicalOptions[Flatten[{opts}]];
          catalog = "Catalog" /. useOpts /. Options[ SQLTableInformation ];
          schema = "Schema" /. useOpts /. Options[ SQLTableInformation ];
          tt = "TableType" /. useOpts /. Options[ SQLTableInformation ];
          sch = "ShowColumnHeadings" /. useOpts /. Options[ SQLTableInformation ];

          Which[
            tt === None, tt = Null,
            StringQ[tt], tt = {tt}, 
            !MatchQ[tt, {___String}], Message[SQLTableInformation::tabletype, tt];Return[$Failed]
          ];
          Which[
            catalog === None, catalog = Null, 
            !StringQ[catalog], Message[SQLTableInformation::catalog, catalog];Return[$Failed]
          ];
          Which[
            schema === None, schema = Null, 
            !StringQ[schema], Message[SQLTableInformation::schema, schema];Return[$Failed]
          ];
          If[!JavaObjectQ[connection], 
            Message[SQLConnection::conn];
            Return[$Failed]
          ];
      
          meta = connection@getMetaData[]; 
          rs = meta@getTables[catalog,schema,table,tt];

          LoadJavaClass["com.wolfram.databaselink.SQLStatementProcessor"];
          SQLStatementProcessor`getAllResultData[ rs, False, TrueQ[sch]]
        ]
      ]
    ]
  ]

SQLTableInformation[ conn_SQLConnection, opts:OptionsPattern[]] :=
  SQLTableInformation[ conn, Null, opts]
  
SQLTableNames[conn_SQLConnection, table_String | table:Null, opts:OptionsPattern[]] :=
  Module[{tables},
    tables = SQLTables[conn, table, opts];
    If[tables === $Failed, 
      $Failed,
      First /@ tables
    ]
  ]

SQLTableNames[ conn_SQLConnection, opts:OptionsPattern[]] :=
  SQLTableNames[ conn, Null, opts]

SQLTables[ conn_SQLConnection, table_String | table:Null, opts:OptionsPattern[]] :=
  Module[ {data, nameIndex, typeIndex, useOpts, tt, catalog, schema}, 
    useOpts = canonicalOptions[Flatten[{opts}]];
    tt = "TableType" /. useOpts /. Options[ SQLTables ];
    catalog = "Catalog" /. useOpts /. Options[ SQLTables ];
    schema = "Schema" /. useOpts /. Options[ SQLTables ];
    
    data = SQLTableInformation[ conn, table, "Catalog"->catalog, 
                                             "Schema"->schema, 
                                             "TableType"->tt, 
                                             "ShowColumnHeadings"->True];
    If[data === $Failed, Return[$Failed]];
    
    {nameIndex, typeIndex} =
       Flatten[Position[ToUpperCase /@ data[[1]], #] & /@ {"TABLE_NAME", "TABLE_TYPE"}];
       
    data = SQLTable[#[[nameIndex]], "TableType" -> #[[typeIndex]]] & /@ Drop[data, 1]
  ]

SQLTables[ conn_SQLConnection, opts:OptionsPattern[]] :=
  SQLTables[ conn, Null, opts]

SQLTablePrivileges[ conn:SQLConnection[ _JDBC, connection_, _Integer, ___Rule], 
                      table_String | table:Null, opts:OptionsPattern[]] :=
  sqlTableInfoHelper[conn, SQLTablePrivileges, table, opts];

SQLTablePrivileges[ conn_SQLConnection, opts:OptionsPattern[]] :=
  SQLTablePrivileges[ conn, Null, opts]

SQLTableExportedKeys[ conn:SQLConnection[ _JDBC, connection_, _Integer, ___Rule], 
                      table_String | table:Null, opts:OptionsPattern[]] :=
  sqlTableInfoHelper[conn, SQLTableExportedKeys, table, opts];

SQLTableExportedKeys[ conn_SQLConnection, opts:OptionsPattern[]] :=
  SQLTableExportedKeys[ conn, Null, opts]

SQLTableImportedKeys[ conn:SQLConnection[ _JDBC, connection_, _Integer, ___Rule], 
                      table_String | table:Null, opts:OptionsPattern[]] :=
  sqlTableInfoHelper[conn, SQLTableImportedKeys, table, opts];

SQLTableImportedKeys[ conn_SQLConnection, opts:OptionsPattern[]] :=
  SQLTableImportedKeys[ conn, Null, opts]
  
SQLTablePrimaryKeys[ conn:SQLConnection[ _JDBC, connection_, _Integer, ___Rule], 
                      table_String | table:Null, opts:OptionsPattern[]] :=
  sqlTableInfoHelper[conn, SQLTablePrimaryKeys, table, opts];

SQLTablePrimaryKeys[ conn_SQLConnection, opts:OptionsPattern[]] :=
  SQLTablePrimaryKeys[ conn, Null, opts]
  
SQLTableIndexInformation[ conn:SQLConnection[ _JDBC, connection_, _Integer, ___Rule], 
                      table_String | table:Null, opts:OptionsPattern[]] :=
  sqlTableInfoHelper[conn, SQLTableIndexInformation, table, opts];

SQLTableIndexInformation[ conn_SQLConnection, opts:OptionsPattern[]] :=
  SQLTableIndexInformation[ conn, Null, opts]
    
sqlTableInfoHelper[  SQLConnection[ _JDBC, connection_, _Integer, ___Rule], 
                     func_Symbol,
                     table_String | table:Null, 
                     opts:OptionsPattern[]] :=
  JavaBlock[
    Module[ {useOpts, sch, meta, rs, schema, catalog}, 
      Block[{$JavaExceptionHandler = ThrowException}, 
        Catch[
          useOpts = canonicalOptions[Flatten[{opts}]];
          catalog = "Catalog" /. useOpts /. Options[ SQLTablePrivileges ];
          schema = "Schema" /. useOpts /. Options[ SQLTablePrivileges];
          sch = "ShowColumnHeadings" /. useOpts /. Options[ SQLTablePrivileges ];

          Which[
            catalog === None, catalog = Null, 
            !StringQ[catalog], Message[SQLTableInformation::catalog, catalog];Return[$Failed]
          ];
          Which[
            schema === None, schema = Null, 
            !StringQ[schema], Message[SQLTableInformation::schema, schema];Return[$Failed]
          ];
          If[!JavaObjectQ[connection], 
            Message[SQLConnection::conn];
            Return[$Failed]
          ];
      
          meta = connection@getMetaData[]; 
          Switch[func, 
            SQLTablePrivileges,
              rs = meta@getTablePrivileges[catalog,schema,table], 
            SQLTableExportedKeys, 
              rs = meta@getExportedKeys[catalog,schema,table], 
            SQLTableImportedKeys,
              rs = meta@getImportedKeys[catalog,schema,table],
            SQLTablePrimaryKeys,
              rs = meta@getPrimaryKeys[catalog,schema,table],
            SQLTableIndexInformation,
              rs = meta@getIndexInfo[catalog,schema,table, False, True]
          ];

          LoadJavaClass["com.wolfram.databaselink.SQLStatementProcessor"];
          SQLStatementProcessor`getAllResultData[ rs, False, TrueQ[sch]]
        ]
      ]
    ]
  ]

SQLTableVersionColumns[SQLConnection[ _JDBC, connection_, _Integer, ___Rule],
                     table_String | table:Null, opts:OptionsPattern[]] :=
    JavaBlock[
      Module[ {useOpts, sch, meta, rs, schema, catalog},
          Block[ {$JavaExceptionHandler = ThrowException},
              Catch[
              
                useOpts = canonicalOptions[Flatten[{opts}]];
                catalog = "Catalog" /. useOpts /. Options[ SQLColumnInformation ];
                schema = "Schema" /. useOpts /. Options[SQLColumnInformation ];
                sch = "ShowColumnHeadings" /. useOpts /. Options[ SQLColumnInformation];
                Which[
                  catalog === None, catalog = Null, 
                  !StringQ[catalog], Message[SQLTableVersionColumns::catalog, catalog];
                                     Return[$Failed]
                ];
                Which[
                  schema === None, schema = Null, 
                  !StringQ[schema], Message[SQLTableVersionColumns::schema, schema];
                                    Return[$Failed]
                ];
                If[ !JavaObjectQ[connection],
                    Message[SQLConnection::conn];
                    Return[$Failed]
                ];
                meta = connection@getMetaData[];
                rs = meta@getVersionColumns[catalog,schema,table];
                
                LoadJavaClass["com.wolfram.databaselink.SQLStatementProcessor"];
                SQLStatementProcessor`getAllResultData[ rs, False, TrueQ[sch]]
              ]
          ]
      ]
    ]

userDefinedTypeCheck[udtype_] :=
    Module[ {},
        Block[ {$JavaExceptionHandler = ThrowException},
            Catch[
            Which[
                StringQ[udtype],Switch[udtype,
                    "DISTINCT", {2001}, 
                    "STRUCT", {2002}, 
                    "JAVA_OBJECT", {2000}, 
                    _, Message[userDefinedTypeCheck::udtype, udtype];
                       Return[$Failed]
                ],
                True, Message[userDefinedTypeCheck::udtype, udtype];
                      Return[$Failed]
            ]
            ]
        ]
    ]

SQLUserDefinedTypeInformation[ SQLConnection[ _JDBC, connection_, _Integer, ___Rule], 
                     typeName_String | typeName:Null, opts:OptionsPattern[]] :=
    JavaBlock[
      Module[ {useOpts, types, sch, meta, rs, schema, catalog},
          Block[ {$JavaExceptionHandler = ThrowException},
              Catch[
                useOpts = canonicalOptions[Flatten[{opts}]];
                catalog = "Catalog" /. useOpts /. Options[ SQLUserDefinedTypeInformation ];
                schema = "Schema" /. useOpts /. Options[ SQLUserDefinedTypeInformation ];
                types = "Types" /. useOpts /. Options[ SQLUserDefinedTypeInformation ];
                sch = "ShowColumnHeadings" /. useOpts /. Options[ SQLUserDefinedTypeInformation ];
                Which[
                  types === None, typesValues = Null,
                  StringQ[types], Check[typesValues = userDefinedTypeCheck[types],Return[$Failed]],
                  ListQ[types], Check[typesValues = Flatten[userDefinedTypeCheck/@types],Return[$Failed]],
                  True, Message[SQLUserDefinedTypeInformation::types, types];
                        Return[$Failed]
                ];
                Which[
                  catalog === None, catalog = Null, 
                  !StringQ[catalog], Message[SQLUserDefinedTypeInformation::catalog, catalog];
                                     Return[$Failed]
                ];
                Which[
                  schema === None, schema = Null, 
                  !StringQ[schema], Message[SQLUserDefinedTypeInformation::schema, schema];
                                    Return[$Failed]
                ];
                If[ !JavaObjectQ[connection],
                    Message[SQLConnection::conn];
                    Return[$Failed]
                ];
                meta = connection@getMetaData[];
                rs = meta@getUDTs[catalog,schema,typeName,MakeJavaObject[typesValues]];
                
                LoadJavaClass["com.wolfram.databaselink.SQLStatementProcessor"];
                SQLStatementProcessor`getAllResultData[ rs, False, TrueQ[sch]]
              ]
          ]
      ]
    ]

SQLUserDefinedTypeInformation[ conn_SQLConnection, opts:OptionsPattern[]] :=
  SQLUserDefinedTypeInformation[ conn, Null, opts]
  
SQLColumnInformation[SQLConnection[ _JDBC, connection_, _Integer, ___Rule],
                     {table_String | table:Null, column_String | column:Null}, opts:OptionsPattern[]] := 
  JavaBlock[
    Module[ {useOpts, sch, meta, rs, schema, catalog}, 
      Block[{$JavaExceptionHandler = ThrowException},
        Catch[

          useOpts = canonicalOptions[Flatten[{opts}]];
          catalog = "Catalog" /. useOpts /. Options[ SQLColumnInformation ];
          schema = "Schema" /. useOpts /. Options[SQLColumnInformation ];
          sch = "ShowColumnHeadings" /. useOpts /. Options[ SQLColumnInformation];

          Which[
            catalog === None, catalog = Null, 
            !StringQ[catalog], Message[SQLColumnInformation::catalog, catalog];Return[$Failed]
          ];
          Which[
            schema === None, schema = Null, 
            !StringQ[schema], Message[SQLColumnInformation::schema, schema];Return[$Failed]
          ];
          If[!JavaObjectQ[connection], 
            Message[SQLConnection::conn];
            Return[$Failed]
          ];
      
          meta = connection@getMetaData[]; 
          rs = meta@getColumns[catalog,schema,table,column];

          LoadJavaClass["com.wolfram.databaselink.SQLStatementProcessor"];
          SQLStatementProcessor`getAllResultData[ rs, False, TrueQ[sch]]
        ]
      ]
    ]
  ]

SQLColumnInformation[conn_SQLConnection, opts:OptionsPattern[]] := 
  SQLColumnInformation[conn, {Null, Null}, opts]
  
SQLColumnInformation[conn_SQLConnection, table_String, opts:OptionsPattern[]] := 
  SQLColumnInformation[conn, {table, Null}, opts]

SQLColumnInformation[conn_SQLConnection, SQLTable[table_String ,___Rule], opts:OptionsPattern[]] := 
  SQLColumnInformation[conn, {table, Null}, opts]

SQLColumnInformation[conn_SQLConnection, SQLColumn[col_String,___Rule ], opts:OptionsPattern[]] := 
  SQLColumnInformation[conn, {Null, col}, opts]

SQLColumnInformation[conn_SQLConnection, SQLColumn[{table_String, col_String},___Rule ], opts:OptionsPattern[]] := 
  SQLColumnInformation[conn, {table, col}, opts]

SQLColumns[conn_SQLConnection, {table_String | table:Null, column_String | column:Null}, opts:OptionsPattern[]] := 
  Module[ { data, tableIndex, columnIndex, typeIndex, nullableIndex, lengthIndex, useOpts, catalog, schema,
  	defIndex}, 

    useOpts = canonicalOptions[Flatten[{opts}]];
    catalog = "Catalog" /. useOpts /. Options[ SQLTableInformation ];
    schema = "Schema" /. useOpts /. Options[ SQLTableInformation ];

    data = SQLColumnInformation[ conn, {table, column}, "Catalog"->catalog,
                                                        "Schema"->schema, 
                                                        "ShowColumnHeadings"->True];
    If[data === $Failed, Return[$Failed]];
    
    {tableIndex, columnIndex, typeIndex, nullableIndex, lengthIndex, defIndex} =
       Flatten[Position[ToUpperCase /@ data[[1]], #] & /@ 
         {"TABLE_NAME", "COLUMN_NAME", "TYPE_NAME", "NULLABLE", "COLUMN_SIZE", "COLUMN_DEF"}];
       
    data = SQLColumn[{#[[tableIndex]], #[[columnIndex]]}, 
             "DataTypeName" -> #[[typeIndex]],
             "DataLength" -> #[[lengthIndex]],
             "Default" -> #[[defIndex]],
             "Nullable" -> #[[nullableIndex]]
             
] & /@ Drop[data, 1]
  ]

SQLColumns[conn_SQLConnection, opts:OptionsPattern[]] := 
  SQLColumns[conn, {Null, Null}, opts]
  
SQLColumns[conn_SQLConnection, table_String, opts:OptionsPattern[]] := 
  SQLColumns[conn, {table, Null}, opts]

SQLColumns[conn_SQLConnection, SQLTable[table_String ,___Rule], opts:OptionsPattern[]] := 
  SQLColumns[conn, {table, Null}, opts]

SQLColumns[conn_SQLConnection, SQLColumn[col_String,___Rule ], opts:OptionsPattern[]] := 
  SQLColumns[conn, {Null, col}, opts]

SQLColumns[conn_SQLConnection, SQLColumn[{table_String, col_String},___Rule ], opts:OptionsPattern[]] := 
  SQLColumns[conn, {table, col}, opts]

SQLColumnNames[conn_SQLConnection, {table_String | table:Null, column_String | column:Null}, opts:OptionsPattern[]] := 
  Module[{columns},
    columns = SQLColumns[conn, {table, column}, opts];
    If[columns === $Failed, 
      $Failed,
      First /@ columns
    ]
  ]
  
SQLColumnNames[conn_SQLConnection, opts:OptionsPattern[]] := 
  SQLColumnNames[conn, {Null, Null}, opts]
  
SQLColumnNames[conn_SQLConnection, table_String, opts:OptionsPattern[]] := 
  SQLColumnNames[conn, {table, Null}, opts]

SQLColumnNames[conn_SQLConnection, SQLTable[table_String ,___Rule], opts:OptionsPattern[]] := 
  SQLColumnNames[conn, {table, Null}, opts]

SQLColumnNames[conn_SQLConnection, SQLColumn[col_String,___Rule ], opts:OptionsPattern[]] := 
  SQLColumnNames[conn, {Null, col}, opts]

SQLColumnNames[conn_SQLConnection, SQLColumn[{table_String, col_String},___Rule ], opts:OptionsPattern[]] := 
  SQLColumnNames[conn, {table, col}, opts]

SQLColumnPrivileges[SQLConnection[ _JDBC, connection_, _Integer, ___Rule],
                     {table_String | table:Null, column_String | column:Null}, opts:OptionsPattern[]] := 
  JavaBlock[
    Module[ {useOpts, sch, meta, rs, schema, catalog}, 
      Block[{$JavaExceptionHandler = ThrowException},
        Catch[

          useOpts = canonicalOptions[Flatten[{opts}]];
          catalog = "Catalog" /. useOpts /. Options[ SQLColumnPrivileges ];
          schema = "Schema" /. useOpts /. Options[ SQLColumnPrivileges ];
          sch = "ShowColumnHeadings" /. useOpts /. Options[ SQLColumnPrivileges ];

          Which[
            catalog === None, catalog = Null, 
            !StringQ[catalog], Message[SQLColumnInformation::catalog, catalog];Return[$Failed]
          ];
          Which[
            schema === None, schema = Null, 
            !StringQ[schema], Message[SQLColumnInformation::schema, schema];Return[$Failed]
          ];
          If[!JavaObjectQ[connection], 
            Message[SQLConnection::conn];
            Return[$Failed]
          ];
      
          meta = connection@getMetaData[]; 
          rs = meta@getColumnPrivileges[catalog, schema, table, column];
        
          LoadJavaClass["com.wolfram.databaselink.SQLStatementProcessor"];
          SQLStatementProcessor`getAllResultData[ rs, False, TrueQ[sch]]
        ]
      ]
    ]
  ]

SQLColumnPrivileges[conn_SQLConnection, opts:OptionsPattern[]] := 
  SQLColumnPrivileges[conn, {Null, Null}, opts]
  
SQLColumnPrivileges[conn_SQLConnection, table_String, opts:OptionsPattern[]] := 
  SQLColumnPrivileges[conn, {table, Null}, opts]

SQLColumnPrivileges[conn_SQLConnection, SQLTable[table_String, ___Rule], opts:OptionsPattern[]] := 
  SQLColumnPrivileges[conn, {table, Null}, opts]

SQLColumnPrivileges[conn_SQLConnection, SQLColumn[col_String, ___Rule], opts:OptionsPattern[]] := 
  SQLColumnPrivileges[conn, {Null, col}, opts]

SQLColumnPrivileges[conn_SQLConnection, SQLColumn[{table_String, col_String}, ___Rule], opts:OptionsPattern[]] := 
  SQLColumnPrivileges[conn, {table, col}, opts]


SQLDataTypeInformation[SQLConnection[ _JDBC, connection_, _Integer, ___Rule],
                       opts:OptionsPattern[] ] := 
  JavaBlock[
    Module[ {useOpts, sch, meta, rs}, 
      Block[{$JavaExceptionHandler = ThrowException}, 
        Catch[
          useOpts = canonicalOptions[Flatten[{opts}]];
          sch = "ShowColumnHeadings" /. useOpts /. Options[ SQLTableInformation ];

          If[!JavaObjectQ[connection], 
            Message[SQLConnection::conn];
            Return[$Failed]
          ];
      
          meta = connection@getMetaData[]; 
          rs = meta@getTypeInfo[];

          LoadJavaClass["com.wolfram.databaselink.SQLStatementProcessor"];
          SQLStatementProcessor`getAllResultData[ rs, False, TrueQ[sch]]      
        ]
      ]
    ]
  ]

SQLDataTypeNames[ conn_SQLConnection] := 
  Module[ { data, nameIndex }, 
    data = SQLDataTypeInformation[ conn, ShowColumnHeadings->True];
    If[data === $Failed, Return[$Failed]];
    
    {nameIndex} = Flatten[Position[ToUpperCase /@ data[[1]], "TYPE_NAME"]];   
    data = Flatten[#[[nameIndex]] & /@ Drop[data, 1]]
  ]  

SQLTableTypeNames[SQLConnection[ _JDBC, connection_, _Integer, ___Rule]] := 
  JavaBlock[
    Module[ {meta, rs, data}, 
      Block[{$JavaExceptionHandler = ThrowException}, 
        Catch[
          If[!JavaObjectQ[connection], 
            Message[SQLConnection::conn];
            Return[$Failed]
          ];
      
          meta = connection@getMetaData[]; 
          rs = meta@getTableTypes[];

          LoadJavaClass["com.wolfram.databaselink.SQLStatementProcessor"];
          data = SQLStatementProcessor`getAllResultData[ rs, False, False];
          If[MatrixQ[data], data = Flatten[data]];
          data
        ]
      ]
    ]
  ]
  
SQLSchemaInformation[SQLConnection[ _JDBC, connection_, _Integer, ___Rule], 
                     opts:OptionsPattern[]] := 
  JavaBlock[
    Module[ {meta, rs, data, useOpts, sch}, 
      Block[{$JavaExceptionHandler = ThrowException}, 
        Catch[
          useOpts = canonicalOptions[Flatten[{opts}]];
          sch = "ShowColumnHeadings" /. useOpts /. Options[ SQLTableInformation ];

          If[!JavaObjectQ[connection], 
            Message[SQLConnection::conn];
            Return[$Failed]
          ];
      
          meta = connection@getMetaData[]; 
          rs = meta@getSchemas[];

          LoadJavaClass["com.wolfram.databaselink.SQLStatementProcessor"];
          data = SQLStatementProcessor`getAllResultData[ rs, False, TrueQ[sch]]    
        ]
      ]
    ]
  ]

SQLSchemaNames[ conn_SQLConnection] := 
  Module[ { data, nameIndex }, 
    data = SQLSchemaInformation[ conn, ShowColumnHeadings->True];
    If[data === $Failed, Return[$Failed]];
    
    {nameIndex} = Flatten[Position[ToUpperCase /@ data[[1]], "TABLE_SCHEM"]];   
    data = Flatten[#[[nameIndex]] & /@ Drop[data, 1]]
  ]  
  
SQLCatalogNames[SQLConnection[ _JDBC, connection_, _Integer, ___Rule]] := 
  JavaBlock[
    Module[ {meta, rs, data}, 
      Block[{$JavaExceptionHandler = ThrowException},
        Catch[
          If[!JavaObjectQ[connection], 
            Message[SQLConnection::conn];
            Return[$Failed]
          ];
        
          meta = connection@getMetaData[]; 
          rs = meta@getCatalogs[];

          LoadJavaClass["com.wolfram.databaselink.SQLStatementProcessor"];
          data = SQLStatementProcessor`getAllResultData[ rs, False, False];
          If[MatrixQ[data], data = Flatten[data]];
          data
        ]
      ]
    ]
  ]

(*===================================================================*)
(*================= SQL Generation Functionality ====================*)
(*===================================================================*)

Options[SQLCreateTable] = 
    {
      "Timeout" :> $SQLTimeout,
      "Index" -> None
    }

Options[SQLDropTable] = 
    {
      "Timeout" :> $SQLTimeout
    }

Options[SQLInsert] = 
  Options[SQLExecute]

Options[ SQLSelect ] = 
    { 
      "SortingColumns" -> None, 
      "ColumnSymbols" -> None, 
      "Distinct" -> False,
      "EscapeProcessing" -> True,
      "FetchDirection" -> "Forward",
      "FetchSize" -> Automatic, 
      "GetAsStrings" -> False, 
      "MaxFieldSize" -> Automatic,
      "MaxRows" -> Automatic, 
      "ShowColumnHeadings" -> False,
      "Timeout" :> $SQLTimeout,
      "BatchSize" -> 0,
      "JavaBatching" -> True
    }

Options[SQLDelete] = 
    {
      "Timeout" :> $SQLTimeout
    }

Options[SQLUpdate] = 
    {
      "Timeout" :> $SQLTimeout
    }
    
SQLColumn::datatypename = "Illegal value for DataTypeName option: `1`";
SQLColumn::datalength = "Illegal value for DataLength option: `1`";
SQLColumn::nullable = "Illegal value for Nullable option: `1`";

SQLCreateTable[conn_SQLConnection,
               table:(_SQLTable | _String), 
               col:(_SQLColumn | {__SQLColumn}),
               opts:OptionsPattern[]] := 
  Module[ {tbl, useOpts, timeout, index, cols, res}, 

    Catch[
      useOpts = canonicalOptions[Flatten[{opts}]];
      timeout = "Timeout" /. useOpts /. Options[ SQLCreateTable ];
      index   = Flatten@List["Index" /. useOpts /. Options[SQLCreateTable]];

      (* Providing a string for the table name is okay, but it needs
         to be wrapped in SQLTable so the formatSQL function knows 
         what to do with it. *)
      tbl = If[StringQ[table], SQLTable[table], table];

      (* Process columns *)
      cols = sqlCreateColumn /@ Flatten[{ col }];
      (* Put commas in between column definitions *)
      cols = Fold[ToString[#1] <> ", " <> ToString[ #2] &, First[cols], Rest[cols]];
    
      SQLBeginTransaction[conn];
      res = SQLExecute[conn, "CREATE TABLE `1` ( " <> cols <> ")", {tbl}, "Timeout" -> timeout];
      If[index =!= {None},
        SQLExecute[conn, "CREATE INDEX " <> (tbl /. SQLTable[t_] :> t) <> First@index <> "_idx" <> " ON `1` (" <>  StringJoin @@ Riffle[index, ","] <> ")", 
        	{tbl}, 
        	"Timeout" -> timeout
        ]
      ];
      SQLCommitTransaction[conn];
      res
    ]
  ]
          
sqlCreateColumn[ SQLColumn[(name_String | {_String, name_String}), opts:OptionsPattern[] ] ] := 
    Module[ { dt, dtn, dw, nl, pk, def, useOpts, stmt }, 
            useOpts = canonicalOptions[Flatten[{opts}]];
            dtn = "DataTypeName" /. useOpts /. Options[ SQLColumn ]; 
            dw  = "DataLength"   /. useOpts /. Options[ SQLColumn ]; 
            nl  = "Nullable"     /. useOpts /. Options[ SQLColumn ]; 
            pk  = "PrimaryKey"   /. useOpts /. Options[ SQLColumn ]; 
            def = "Default"      /. useOpts /. Options[ SQLColumn ]; 
            
            dt = Which[ 
                   StringQ[ dtn ], " " <> dtn, 
                   True, Message[SQLColumn::datatypename];Throw[$Failed];
                 ]; 
            dw = Which[ 
                   dw === None , "", 
                   IntegerQ[ dw ], StringJoin@{ "(", ToString[dw], ")" }, 
                   True, Message[ SQLColumn::datalength ];Throw[$Failed]; 
                 ]; 
            nl = Which[ 
                   nl === None, "", 
                   TrueQ[nl], " NULL",
                   nl === False, " NOT NULL", 
                   True, Message[ SQLColumn::nullable ];Throw[$Failed]; 
                 ]; 
            pk = Which[ 
                   TrueQ[pk], " PRIMARY KEY", 
                   True, ""
                 ];
            def = Which[ 
                   def === None, "",
                   True, " DEFAULT '" <> ToString@def <> "'"
                 ];
            stmt = StringJoin@{ name, dt, dw, def, nl, pk }
          ]

SQLDropTable[ conn_SQLConnection, table:(_SQLTable | _String), opts:OptionsPattern[]] := 
  Module[ { tbl, useOpts, timeout }, 

    useOpts = canonicalOptions[Flatten[{opts}]];
    timeout = "Timeout" /. useOpts /. Options[ SQLDropTable ];

    tbl = If[StringQ[table], SQLTable[table], table];
    
    SQLExecute[conn, "DROP TABLE `1`", {tbl}, "Timeout" -> timeout]
  ] 

SQLInsert[conn_SQLConnection,
          table:(_SQLTable | _String),
          names:{___String}, 
          values:{__},
          opts:OptionsPattern[]] :=
  SQLInsert[conn, table, SQLColumn/@names, values, opts]

SQLInsert[conn_SQLConnection,
          table:(_SQLTable | _String),
          names:{{__String,_String}...}, 
          values:{__},
          opts:OptionsPattern[]] :=
  SQLInsert[conn, table, SQLColumn[Last[#]]&/@names, values, opts]

SQLInsert[conn_SQLConnection,
          (SQLTable[table_String, ___Rule] | table_String),
          names:{ SQLColumn[(_String|{_String, _String}), ___Rule] ... }, 
          values:{__},
          opts:OptionsPattern[]] := 
  Module[ { useOpts, timeout, tbl, cols, vals, rgk, maxrows, gas, sch, mfs, fs, fd, ep, cs, bs, jb },

    useOpts  = canonicalOptions[Flatten[{opts}]];
    rgk      = "GetGeneratedKeys" /. useOpts /. Options[ SQLInsert ];
    timeout  = "Timeout" /. useOpts /. Options[ SQLInsert ];
    gas      = "GetAsStrings" /. useOpts /. Options[ SQLInsert ];
    sch      = "ShowColumnHeadings" /. useOpts /. Options[ SQLInsert ];
    ep       = "EscapeProcessing" /. useOpts /. Options[ SQLInsert ];
    cs       = "ColumnSymbols" /. useOpts /. Options[ SQLInsert ];
    maxrows  = "MaxRows" /. useOpts /. Options[ SQLInsert ];
    mfs      = "MaxFieldSize" /. useOpts /. Options[ SQLInsert ];
    fs       = "FetchSize" /. useOpts /. Options[ SQLInsert ];
    fd       = "FetchDirection" /. useOpts /. Options[ SQLInsert ];
    bs       = "BatchSize" /. useOpts /. {Infinity :> Length[values], Except[_Integer] -> 0} 
                    /. Options[ SQLInsert ];
    jb       = "JavaBatching" /. useOpts /. Options[SQLInsert];

    tbl = If[StringQ[table], tbl, First[table]];    

    cols = 
      If[names === {},
        "",
        StringJoin[" (", mingleComma[getSQLColumnName /@ names], ")"]
      ];
    
    vals = 
      If[MatchQ[values, {__List}],
        StringJoin["(", mingleComma["?" & /@ Range[Length[First[values]]]] ,")"], 
        StringJoin["(", mingleComma["?" & /@ Range[Length[values]]] ,")"]
      ];

    SQLExecute[conn, "INSERT INTO " <> table <> cols <> " VALUES " <> vals, values, 
      "Timeout"->timeout, "GetGeneratedKeys"->rgk, "GetAsStrings"->gas, "ShowColumnHeadings"->sch,
      "EscapeProcessing"->ep, "ColumnSymbols"->cs, "MaxRows"->maxrows, "MaxFieldSize"->mfs, 
      "FetchSize"->fs, "FetchDirection"->fd, "BatchSize" -> bs, "JavaBatching" -> jb]
  ]

getSQLColumnName[SQLColumn[(col_String|{table_String,col_String}),opts:OptionsPattern[]]]:=
  Module[{nm},
    nm= If[Head[Unevaluated[table]] =!= String, col, table <> "." <> col];
    stripSpaces@If[areSpaces[nm], "\"" <> nm <> "\"", nm]
  ]

          
SQLSelect[conn_SQLConnection | conn_String,
          table:(_SQLTable | {__SQLTable} | _String | {__String}),
          opts:OptionsPattern[]
         ] := 
  SQLSelect[conn, table, SQLColumn["*"], None, opts];
  
SQLSelect[conn_SQLConnection | conn_String,
          table:(_SQLTable | {__SQLTable} | _String | {__String}),
          columns:(_SQLColumn | {__SQLColumn}),
          opts:OptionsPattern[]] :=
  SQLSelect[conn, table, Flatten@{columns}, None, opts];

SQLSelect[conn_SQLConnection | conn_String,
          table:(_SQLTable | {__SQLTable} | _String | {__String}),
          columns:(_String | {__String}),
          opts:OptionsPattern[]] :=
  SQLSelect[conn, table, SQLColumn/@(Flatten@{columns}), None, opts];

SQLSelect[conn_SQLConnection | conn_String,
          table:(_SQLTable | {__SQLTable} | _String | {__String}),
          columns:{{_String, _String}..},
          opts:OptionsPattern[]] :=
  SQLSelect[conn, table, SQLColumn/@columns, None, opts];

SQLSelect[conn_SQLConnection | conn_String,
          table:(_SQLTable | {__SQLTable} | _String | {__String}),
          condition_,
          opts:OptionsPattern[]] := 
  SQLSelect[conn, table, SQLColumn["*"], condition, opts];

SQLSelect[conn_SQLConnection,
          table:(_SQLTable | {__SQLTable} | _String | {__String}),
          columns:(_SQLColumn | {__SQLColumn}),
          condition_,
          opts:OptionsPattern[]] :=
  Module[ { tbls, cols, useOpts, distinct, orderby, order,
            maxrows, timeout, gas, sch, where, stmt,
            mfs, fd, fs, ep, rrs, cs, bs, jb}, 
  
    tbls = 
      If[!MatchQ[Flatten@{table}, {__SQLTable}], 
        SQLArgument @@ (SQLTable/@Flatten@{table}),
        SQLArgument @@ Flatten@{table}
      ];
    cols = SQLArgument @@ Flatten@{columns};
            
    useOpts  = canonicalOptions[Flatten[{opts}]];
    distinct = "Distinct"         /. useOpts /. Options[ SQLSelect ]; 
    order    = "SortingColumns"   /. useOpts /. Options[ SQLSelect ];
    maxrows  = "MaxRows" /. useOpts /. Options[ SQLSelect ];
    timeout  = "Timeout" /. useOpts /. Options[ SQLSelect ];
    gas      = "GetAsStrings" /. useOpts /. Options[ SQLSelect ];
    sch      = "ShowColumnHeadings" /. useOpts /. Options[ SQLSelect ];
    mfs      = "MaxFieldSize" /. useOpts /. Options[ SQLSelect ];
    fs       = "FetchSize" /. useOpts /. Options[ SQLSelect ];
    fd       = "FetchDirection" /. useOpts /. Options[ SQLSelect ];
    ep       = "EscapeProcessing" /. useOpts /. Options[ SQLSelect ];
    cs       = "ColumnSymbols" /. useOpts /. Options[ SQLSelect ];
    
    rrs      = "ResultSet" /. useOpts /. {"ResultSet"->False};   
    bs       = "BatchSize" /. useOpts /. {Infinity :> 1, Except[_Integer] -> 0} 
                    /. Options[ SQLSelect ];
    jb       = "JavaBatching" /. useOpts /. Options[SQLSelect];

    distinct = 
      If[ ( distinct === True ), 
        "DISTINCT ", 
        ""
      ]; 
    orderby = 
      If[ ( order  === None ), 
        order = {};
        "",          
        " ORDER BY `4`"
      ]; 
    where = 
      If[condition === None,
        "",
        " WHERE `3`"
      ]; 

    stmt = "SELECT " <> distinct <> "`1` FROM `2`" <> where <> orderby;
    
    SQLExecute[conn, stmt, {cols, tbls, condition, SQLArgument @@ Flatten[{order}]}, 
      "MaxRows"->maxrows, "Timeout"->timeout, "GetAsStrings"->gas, 
      "ShowColumnHeadings"->sch, "MaxFieldSize"->mfs, "FetchDirection"->fd, 
      "FetchSize"->fs, "EscapeProcessing"->ep, "ColumnSymbols"->cs, "ResultSet"->rrs,
      "BatchSize" -> bs, "JavaBatching" -> jb]
  ]

SQLSelect[conn_SQLConnection | conn_String,
          table:(_SQLTable | {__SQLTable} | _String | {__String}),
          columns:(_String | {__String}),
          condition_,
          opts:OptionsPattern[]] :=
  SQLSelect[conn, table, SQLColumn/@(Flatten@{columns}), condition, opts];

SQLSelect[conn_SQLConnection | conn_String,
          table:(_SQLTable | {__SQLTable} | _String | {__String}),
          columns:{{_String, _String}..},
          condition_,
          opts:OptionsPattern[]] :=
  SQLSelect[conn, table, SQLColumn/@columns, condition, opts];

SQLDelete[ conn_SQLConnection,
           table:(_SQLTable | {__SQLTable} | _String | {__String}), 
           opts:OptionsPattern[]] := 
  SQLDelete[ conn, table, None, opts] 

SQLDelete[ conn_SQLConnection,
           table:(_SQLTable | _String), 
           condition_, 
           opts:OptionsPattern[]] := 
  Module[ { useOpts, timeout, tbl, where }, 

    useOpts = canonicalOptions[Flatten[{opts}]];
    timeout = "Timeout" /. useOpts /. Options[ SQLDelete ];

    tbl = If[StringQ[table], SQLTable[table], table];

    where = 
      If[condition === None,
        "",
        " WHERE `2`"
      ]; 
      
    SQLExecute[conn, "DELETE FROM `1`" <> where, {tbl, condition}, "Timeout"->timeout]
  ]

SQLUpdate[conn_SQLConnection,
          table:(_SQLTable | {__SQLTable} | _String | {__String}),
          names:{__String}, 
          values:{__},
          opts:OptionsPattern[]] :=
  SQLUpdate[conn, table, SQLColumn/@names, values, None, opts]

SQLUpdate[conn_SQLConnection,
          table:(_SQLTable | {__SQLTable} | _String | {__String}),
          names:{{__String,_String}..}, 
          values:{__},
          opts:OptionsPattern[]] :=
  SQLUpdate[conn, table, SQLColumn[Last[#]]&/@names, values, None, opts]

SQLUpdate[conn_SQLConnection,
          table:(_SQLTable | {__SQLTable} | _String | {__String}),
          names:{__SQLColumn }, 
          values:{ __ },
          opts:OptionsPattern[]] := 
  SQLUpdate[conn, table, names, values, None, opts]


SQLUpdate[conn_SQLConnection,
          table:(_SQLTable | {__SQLTable} | _String | {__String}),
          names:{__String}, 
          values:{__},
          condition_,
          opts:OptionsPattern[]] :=
  SQLUpdate[conn, table, SQLColumn/@names, values, condition, opts]

SQLUpdate[conn_SQLConnection,
          table:(_SQLTable | {__SQLTable} | _String | {__String}),
          names:{{__String,_String}..}, 
          values:{__},
          condition_,
          opts:OptionsPattern[]] :=
  SQLUpdate[conn, table, SQLColumn[Last[#]]&/@names, values, condition, opts]

SQLUpdate[ conn_SQLConnection,
           table:(_SQLTable | {__SQLTable} | _String | {__String}), 
           names:{__SQLColumn}, 
           values:{__},
           condition_,
           opts:OptionsPattern[]
         ] := 
  Module[ { useOpts, timeout, tbls, where, set }, 
  
    useOpts = canonicalOptions[Flatten[{opts}]];
    timeout = "Timeout" /. useOpts /. Options[ SQLUpdate ];

    tbls = 
      If[!MatchQ[Flatten@{table}, {__SQLTable}], 
        SQLArgument @@ (SQLTable/@Flatten@{table}),
        SQLArgument @@ Flatten@{table}
      ];
    
    where = 
      If[condition === None,
        "",
        " WHERE `3`"
      ]; 
      
    set = MapThread[Rule[#1, #2] &, {names, values}];
    
    SQLExecute[conn, "UPDATE `1` SET `2`" <> where, {tbls, SQLArgument @@ set, condition}, "Timeout"->timeout] 
  ]

(*===================================================================*)
(*================== FORMATTING AND CONVERSION ======================*)
(*===================================================================*)

summaryBoxIcon = Graphics[{Thickness[0.0625], 
  Style[{FilledCurve[{{{0, 2, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}, 
      {0, 1, 0}}}, {{{15.236999999999998, 15.07}, {11.078, 17.829}, {11.078, 
      15.975000000000001}, {1.625, 15.975000000000001}, {1.625, 14.165000000000001}, 
      {11.078, 14.165000000000001}, {11.078, 12.31}, {15.236999999999998, 15.07}}}], 
    FilledCurve[{{{0, 2, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}, {0, 
      1, 0}}}, {{{0., 9.792}, {4.159, 7.033}, {4.159, 8.887}, {13.612, 8.887}, {13.612, 
      10.697}, {4.159, 10.697}, {4.159, 12.551}, {0., 9.792}}}], 
    FilledCurve[{{{1, 4, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {0, 
      1, 0}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {0, 1, 0}, 
      {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {0, 1, 0}, {1, 3, 
      3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {0, 1, 0}}}, 
     {{{2.4789999999999996, 1.508}, {2.504, 1.327}, {2.554, 1.1920000000000002}, {2.63, 
      1.103}, {2.7670000000000003, 0.9390000000000001}, {3.002, 0.8580000000000001}, 
      {3.3339999999999996, 0.8580000000000001}, {3.533, 0.8580000000000001}, {3.695, 
      0.88}, {3.82, 0.923}, {4.055, 1.005}, {4.1739999999999995, 1.1580000000000001}, 
      {4.1739999999999995, 1.3820000000000001}, {4.1739999999999995, 1.513}, 
      {4.114999999999999, 1.613}, {4., 1.6860000000000002}, {3.885, 1.755}, {3.701, 
      1.817}, {3.4499999999999997, 1.87}, {3.022, 1.9649999999999999}, {2.601, 2.058}, 
      {2.3109999999999995, 2.159}, {2.154, 2.269}, {1.887, 2.4499999999999997}, {1.754, 
      2.7359999999999998}, {1.754, 3.125}, {1.754, 3.4789999999999996}, 
      {1.8840000000000001, 3.773}, {2.145, 4.009}, {2.4059999999999997, 4.243}, {2.789, 
      4.359999999999999}, {3.295, 4.359999999999999}, {3.718, 4.359999999999999}, {4.077, 
      4.25}, {4.376, 4.028}, {4.6739999999999995, 3.808}, {4.83, 3.4859999999999998}, 
      {4.843999999999999, 3.065}, {4.05, 3.065}, {4.035, 3.304}, {3.928, 3.473}, {3.73, 
      3.573}, {3.598, 3.64}, {3.4339999999999997, 3.673}, {3.238, 3.673}, 
      {3.0189999999999997, 3.673}, {2.8449999999999998, 3.63}, {2.715, 3.544}, {2.584, 
      3.4579999999999997}, {2.5189999999999997, 3.3379999999999996}, {2.5189999999999997, 
      3.184}, {2.5189999999999997, 3.042}, {2.583, 2.9359999999999995}, 
      {2.7119999999999997, 2.867}, {2.794, 2.82}, {2.969, 2.766}, {3.238, 
      2.7030000000000003}, {3.9319999999999995, 2.5389999999999997}, {4.237, 2.468}, 
      {4.465, 2.3719999999999994}, {4.616999999999999, 2.252}, {4.853, 2.065}, 
      {4.971000000000001, 1.796}, {4.971000000000001, 1.4429999999999998}, 
      {4.971000000000001, 1.082}, {4.831, 0.7809999999999999}, {4.552, 0.543}, {4.272, 
      0.304}, {3.877, 0.185}, {3.367, 0.185}, {2.8449999999999998, 0.185}, 
      {2.4359999999999995, 0.302}, {2.137, 0.537}, {1.839, 0.772}, {1.689, 1.097}, 
      {1.689, 1.508}, {2.4789999999999996, 1.508}}}], 
    FilledCurve[{{{1, 4, 3}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}, {1, 3, 3}, {1, 3, 3}, {1, 
      3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}}, {{1, 4, 
      3}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, 
      {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}}}, 
     {{{8.173, 0.912}, {8.220999999999998, 0.925}, {8.283, 0.9470000000000001}, 
      {8.359000000000002, 0.979}, {7.958, 1.36}, {8.383000000000001, 1.804}, {8.785, 
      1.4249999999999998}, {8.847999999999999, 1.554}, {8.892000000000001, 1.667}, 
      {8.917, 1.764}, {8.956000000000001, 1.908}, {8.976, 2.077}, {8.976, 2.27}, {8.976, 
      2.715}, {8.885000000000002, 3.0589999999999997}, {8.703, 3.3009999999999997}, 
      {8.522, 3.543}, {8.256, 3.665}, {7.907, 3.665}, {7.579000000000001, 3.665}, {7.318, 
      3.548}, {7.122999999999999, 3.3149999999999995}, {6.927, 3.083}, {6.83, 2.734}, 
      {6.83, 2.27}, {6.83, 1.728}, {6.970000000000001, 1.34}, {7.2490000000000006, 
      1.105}, {7.430000000000001, 0.9530000000000001}, {7.646999999999999, 
      0.8770000000000001}, {7.899, 0.8770000000000001}, {7.994, 0.8770000000000001}, 
      {8.086, 0.889}, {8.173, 0.912}}, {{9.674, 1.4429999999999998}, {9.604, 
      1.2169999999999999}, {9.502, 1.028}, {9.366000000000001, 0.8780000000000001}, 
      {9.821, 0.45}, {9.389000000000001, 0.}, {8.914, 0.451}, {8.769, 0.363}, 
      {8.642999999999999, 0.301}, {8.537999999999998, 0.265}, {8.360000000000001, 
      0.20600000000000002}, {8.147999999999998, 0.17600000000000002}, {7.901000000000001, 
      0.17600000000000002}, {7.385, 0.17600000000000002}, {6.958, 0.32999999999999996}, 
      {6.6209999999999996, 0.638}, {6.213, 1.009}, {6.009, 1.553}, {6.009, 2.27}, {6.009, 
      2.9939999999999998}, {6.218, 3.541}, {6.636, 3.9109999999999996}, 
      {6.979000000000001, 4.2139999999999995}, {7.404, 4.364999999999999}, {7.912, 
      4.364999999999999}, {8.425, 4.364999999999999}, {8.854000000000001, 4.205}, 
      {9.200999999999999, 3.885}, {9.602, 3.5149999999999997}, {9.803, 
      2.9959999999999996}, {9.803, 2.3299999999999996}, {9.803, 1.978}, {9.76, 
      1.6820000000000002}, {9.674, 1.4429999999999998}}}]}, 
   FaceForm[RGBColor[0.5, 0.5, 0.5, 1.]]], 
  Style[{FilledCurve[{{{0, 2, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 
      0}}}, {{{11.039, 4.245}, {11.866000000000001, 4.245}, {11.866000000000001, 0.998}, 
      {13.842, 0.998}, {13.842, 0.28600000000000003}, {11.039, 0.28600000000000003}, 
      {11.039, 4.245}}}]}, FaceForm[RGBColor[0.5, 0.5, 0.5, 1.]]]}, 
 PlotRangePadding -> 4, Background -> GrayLevel[0.93], Axes -> False, AspectRatio -> 1, 
 ImageSize -> {Automatic, Dynamic[3.5*(CurrentValue["FontCapHeight"]/
      AbsoluteCurrentValue[Magnification])]}, Frame -> True, FrameTicks -> None, 
 FrameStyle -> Directive[Thickness[Tiny], GrayLevel[0.7 ]]];

SQLResultSet /: MakeBoxes[
    SQLResultSet[
        id_Integer,
        rs_Symbol,
        opts___?BoxForm`HeldOptionQ
    ],
    StandardForm] := Module[
    
    {icon = summaryBoxIcon, mode = "", fd = "", fs = "", sometimesOpts, 
    	o = canonicalOptions[Join[Flatten[{opts}], Options[SQLResultSet]]], oPrime},

    If[JavaObjectQ[rs] && rs =!= Null,
        (* fs = ... *)
        mode = Quiet@Switch[rs@getType[],
            1003, "ForwardOnly",
            1004, "ScrollInsensitive",
            1005, "ScrollSensitive",
            _, Style["Unknown Mode", {Italic, GrayLevel[0.55]}]
        ];
        fd = Quiet@Switch[rs@getFetchDirection[],
        	1000, "Forward",
        	1001, "Reverse",
        	1002, "Unknown",
            _, ""
        ];
        fs = With[{i = Quiet[rs@getFetchSize[]]}, If[IntegerQ[i], i, ""]];,
        (* else *)
        mode = Style["Closed", {Italic, GrayLevel[0.55]}]
    ];

    sometimesOpts = Sort[
        DeleteCases[Options[SQLResultSet][[All, 1]], Alternatives @@ {
            "FetchDirection"
        }]
    ];

    oPrime = FilterRules[Join[{"FetchSize" -> fs}, o], Options[SQLResultSet]];
    
    BoxForm`ArrangeSummaryBox[SQLResultSet, 
        SQLResultSet[id, rs, opts],
        icon,
        (* Always *)
        {
            {BoxForm`SummaryItem[{"Mode: ", mode}], BoxForm`SummaryItem[{"ID: ", id}]},
            {BoxForm`SummaryItem[{"FetchDirection: ", fd}], ""}
        },
        (* Sometimes *)
        BoxForm`SummaryItem[{# <> ": ", # /. oPrime /. {Null -> "", None -> ""}}] & /@ sometimesOpts,

        StandardForm
    ]
]

(*Format[ SQLResultSet[ i_Integer, rs_?JavaObjectQ ] ] := 
  Module[{}, 
    Switch[rs@getType[], 
      1003, 
        SQLResultSet[ i, "<>", "ForwardOnly" ], 
      1004, 
        SQLResultSet[ i, "<>", "ScrollInsensitive" ], 
      1005, 
        SQLResultSet[ i, "<>", "ScrollSensitive" ], 
      _, 
        SQLResultSet[ i, "<>", "Unknown Mode"]
    ]
  ]*)

SQLConnection /: MakeBoxes[
	SQLConnection[
		j:JDBC[_String, _String],
		conn_Symbol,
		id_Integer,
		opts___?BoxForm`HeldOptionQ
	],
	StandardForm] := Module[
	
	{name, icon = summaryBoxIcon, til = "", status = Style["Closed", {Italic, GrayLevel[0.55]}], 
		catalog = "", ro = "", sometimesOpts, o = canonicalOptions[Flatten[{opts}]], oPrime},

    name = "Name" /. o /. Options[SQLConnection]; 

    If[JavaObjectQ[conn] && conn =!= Null && !conn@isClosed[],
		status = Style["Open", {Black, Bold}];
		catalog = With[{c = conn@getCatalog[]}, If[StringQ[c], c, ""]];
        ro = conn@isReadOnly[];
        (* Not all drivers implement this method, so Quiet here *)
        til = Quiet@Switch[conn@getTransactionIsolation[],
            2, "ReadCommitted",
            4, "RepeatableRead",
            8, "Serializable",
            _, Null
        ],
        (* else *)
        Null
    ];

    sometimesOpts = Sort[
		DeleteCases[Options[SQLConnection][[All, 1]], Alternatives @@ {
	    	"Catalog",
	    	"Name",
	    	"Username",
	    	"Password",
	    	"Properties"
        }]
    ];

    oPrime = FilterRules[Join[{"ReadOnly" -> ro, "TransactionIsolationLevel" -> til}, o], Options[SQLConnection]];
    
    BoxForm`ArrangeSummaryBox[SQLConnection, 
    	SQLConnection[j, conn, id, opts],
    	icon,
    	(* Always *)
    	{
    		{BoxForm`SummaryItem[{"Name: ", name}], BoxForm`SummaryItem[{"ID: ", id}]},
            {BoxForm`SummaryItem[{"Status: ", status}], BoxForm`SummaryItem[{"Catalog: ", catalog}]}
    	},
    	(* Sometimes *)
    	BoxForm`SummaryItem[{# <> ": ", # /. oPrime /. {Null -> "", None -> ""}}] & /@ sometimesOpts,

        StandardForm
    ]
]

(*Format[ SQLConnection[ _JDBC, conn_, id_Integer, opts:OptionsPattern[]]] := 
  Module[{name}, 
    name = "Name" /. canonicalOptions[Flatten[{opts}]] /. Options[ SQLConnection ]; 
    If[JavaObjectQ[conn], 
      If[conn =!= Null && !conn@isClosed[], 
         DeleteCases[
           SQLConnection[ If[StringQ[name], name], id, "Open",
             If[conn@getCatalog[] =!= Null, "Catalog" -> conn@getCatalog[]],
             If[conn@isReadOnly[], "ReadOnly"->True], 
             (* Not all drivers implement this method, so Quiet here *)
             Quiet@Switch[conn@getTransactionIsolation[], 
               2, "TransactionIsolationLevel"->"ReadCommitted", 
               4, "TransactionIsolationLevel"->"RepeatableRead", 
               8, "TransactionIsolationLevel"->"Serializable",
               _, Null]], Null ],
        SQLConnection[ name, id, "Closed", "<>" ]
      ],
      SQLConnection[ name, id, "Closed", "<>" ]
    ]
  ]*)

SQLServer /: MakeBoxes[
    SQLServer[
        server_Symbol,
        id_Integer,
        opts___?BoxForm`HeldOptionQ
    ],
    StandardForm] := Module[
    
    {name, addr = "", port = "", icon = summaryBoxIcon, status = Style["Unavailable", {Italic, GrayLevel[0.55]}],
        sometimesOpts, o = canonicalOptions[Join[Flatten[{opts}], Options[SQLServer]]], oPrime},
    
    name = "Name" /. o;
    
    If[JavaObjectQ[server] && server =!= Null,
        status = server@getStateDescriptor[] /. {
            "ONLINE" -> Style["Online", {Black, Bold}],
            "SHUTDOWN" -> Style["Shutdown", {Black, Bold}]
        };
        addr = server@getAddress[];
        port = server@getPort[],
        (* else *)
        Null
    ];

    sometimesOpts = Sort[
        DeleteCases[Options[SQLServer][[All, 1]], Alternatives @@ {
        	"Name"
        }]
    ];

    oPrime = FilterRules[Join[{"Address" -> addr, "Port" -> port}, o], Options[SQLServer]];
    
    BoxForm`ArrangeSummaryBox[SQLServer, 
        SQLServer[server, id, opts],
        icon,
        (* Always *)
        {
            {BoxForm`SummaryItem[{"Name: ", name}], BoxForm`SummaryItem[{"ID: ", id}]},
            {BoxForm`SummaryItem[{"Status: ", status}], ""}
        },
        (* Sometimes *)
        BoxForm`SummaryItem[{# <> ": ", # /. oPrime /. {Null -> "", None -> ""}}] & /@ sometimesOpts,

        StandardForm
    ]
]

(*Format[ SQLServer[ server_, id_Integer, opts:OptionsPattern[]]] := 
  Module[{name}, 
    name = "Name" /. canonicalOptions[Flatten[{opts}]] /. Options[ SQLServer ]; 
    If[JavaObjectQ[server], 
      If[conn =!= Null, 
        SQLConnection[ name, id, server@getStateDescriptor[], "<>" ],
        SQLConnection[ name, id, "Unavailable", "<>" ]
      ],
      SQLConnection[ name, id, "Unavailable", "<>" ]
    ]
  ]*)

SQLSavepoint /: MakeBoxes[
    SQLSavepoint[
        sp_Symbol,
        opts___?BoxForm`HeldOptionQ
    ],
    StandardForm] := Module[
    
    {name, id = "", icon = summaryBoxIcon, sometimesOpts, 
    	o = canonicalOptions[Join[Flatten[{opts}], Options[SQLSavepoint]]], oPrime},
    
    name = "Name" /. o; 
    
    If[JavaObjectQ[sp] && sp =!= Null,
        id = Quiet[sp@getSavepointId[]] /. $Failed -> "",
        (* else *)
        Null
    ];

    sometimesOpts = Sort[
        DeleteCases[Options[SQLSavepoint][[All, 1]], Alternatives @@ {
            "Name"
        }]
    ];

    oPrime = FilterRules[Join[{}, o], Options[SQLSavepoint]];
    
    BoxForm`ArrangeSummaryBox[SQLSavepoint,
        SQLSavepoint[sp, opts],
        icon,
        (* Always *)
        {
            {BoxForm`SummaryItem[{"Name: ", name}], BoxForm`SummaryItem[{"ID: ", id}]}
            (*,{BoxForm`SummaryItem[{"Status: ", status}], ""}*)
        }
        (* Sometimes *)
        , BoxForm`SummaryItem[{# <> ": ", # /. oPrime /. {Null -> "", None -> ""}}] & /@ sometimesOpts

        , StandardForm
    ]
]

(*Format[ SQLSavepoint[_, opts:OptionsPattern[]]] := 
  Module[{name}, 
    name = "Name" /. canonicalOptions[Flatten[{opts}]] /. Options[ SQLSavepoint ]; 
    SQLSavepoint[ name, "<>" ]
  ]*)

(*===================================================================*)
(*================= SQLExecute ======================================*)
(*===================================================================*)

SQLExecute[conn_SQLConnection, sql_String, opts:OptionsPattern[]] :=
  Module[{query, pos},
    If[MemberQ[SQLQueryNames[], sql], 
      pos = First[Flatten[Position[SQLQueryNames[], sql]]];
      query = SQLQueries[][[pos]];
      SQLExecute[conn, query],
      (* else *)
      (* N.B. Empty list for args matches OptionsPattern[]! *)
      SQLExecute[ conn, sql, None, opts]
    ]
  ];

  
SQLExecute[ SQLConnection[ JDBC[driver_, ___], connection_, _Integer, ___Rule], ps_String, argsArg:{__}|None, opts:OptionsPattern[]] :=
  JavaBlock[
    Module[{sql = ps, params, useOpts, maxrows, 
            timeout, gas, sch, rrs, rrsBool, rsc = 1007, rst = 1003,
            mfs, fs, fd, ep, rgk, result, cols, cs, results, bs, jb,
            args = argsArg /. None -> {}, useLongs, rdbms = getRDBMS[connection]},
            
      Block[{$JavaExceptionHandler = ThrowException},
        Catch[
                
          If[!JavaObjectQ[connection], 
            Message[SQLConnection::conn];
            Return[$Failed]
          ];

          useOpts = canonicalOptions[Flatten[{opts}]];
          maxrows = "MaxRows" /. useOpts /. Options[ SQLExecute ];
          timeout = "Timeout" /. useOpts /. Options[ SQLExecute ];
          gas     = "GetAsStrings" /. useOpts /. Options[ SQLExecute ];
          sch     = "ShowColumnHeadings" /. useOpts /. Options[ SQLExecute ];
          mfs     = "MaxFieldSize" /. useOpts /. Options[ SQLExecute ];
          fs      = "FetchSize" /. useOpts /. Options[ SQLExecute ];
          fd      = "FetchDirection" /. useOpts /. Options[ SQLExecute ];
          ep      = Boole@TrueQ["EscapeProcessing" /. useOpts /. Options[SQLExecute]];
          rgk     = "GetGeneratedKeys" /. useOpts /. Options[ SQLExecute ];
          cs      = "ColumnSymbols" /. useOpts /. Options[SQLExecute];
          rrs     = "ResultSet" /. useOpts /. {"ResultSet"->False};     (* set by SQLResultSet* *)
          bs      = "BatchSize" /. useOpts /. {Infinity :> Length[args], Except[_Integer] -> 0} 
                    /. Options[ SQLExecute ];
          jb      = "JavaBatching" /. useOpts /. Options[SQLExecute];

          If[cs =!= Automatic && cs =!= None && !MatchQ[cs, _Function] && !MatchQ[cs, {___Symbol}],
            Message[SQLExecute::columnsymbols, cs];
            cs = None
          ];

          If[maxrows =!= Automatic && maxrows =!= All, 
            If[maxrows === None, maxrows = 0];
            If[!IntegerQ[maxrows] || maxrows < 0, 
              Message[SQLExecute::maxrows, maxrows]
            ],
            maxrows = 0;
          ];

          If[timeout =!= None && timeout =!= Automatic,
            If[!IntegerQ[timeout] || timeout < 0,
              Message[SQLExecute::timeout, timeout]
            ],
            (* This will have the effect of not calling setQueryTimeout at all in the statement processor,
             * and presumably use the driver default *)
            timeout = 0;
          ];

          If[mfs =!= Automatic, 
            If[mfs === None, mfs = 0];
            If[!IntegerQ[mfs] || mfs < 0, 
              Message[SQLExecute::maxfieldsize, mfs]
            ],
            mfs = 0;
          ];

          Switch[fd,
            "Forward", fd = 1000,
            "Reverse", fd = 1001, 
            "Unknown", fd = 1002,
            _, 
              Message[SQLExecute::fetchdirection, fd];
              fd = 1000;
          ];

          If[fs =!= Automatic && fs =!= All, 
            If[fs === None, fs = 0];
            If[!IntegerQ[fs] || fs < 0, 
              Message[SQLExecute::fetchsize, fs]
            ],
            fs = 0;
          ];

          If[MatchQ[rrs, {_String}], 
            rst =
              Switch[First[rrs],
                "ForwardOnly", 1003,
                "ScrollInsensitive", 1004,
                "ScrollSensitive", 1005,
                "MySQLStreaming", 1003,
                _, Message[SQLResultSetOpen::mode, First[rrs]];Return[$Failed]
              ];
            rrsBool = True,
            rrsBool = False
          ];
          
          (* Format the SQL into something the driver will understand *)
          {sql, params} = formatSQL[ps, args];
          
          (* Convert SQLExpr objects to strings ahead of time. *) 
          params = params /. {SQLExpr[x_] :> SQLExpr["SQLExpr[" <> ToString[InputForm[x]] <> "]"]};
          
          If[TrueQ[$printPreparedStatement], 
            Print[{sql, params}];
          ];

          (* Thin databases like SQLite don't support all options.
           * Clobber the unimplemented ones before handing off to Java.
           *)
          If[StringMatchQ[rdbms, "SQLite*"],
              ep = -1;
              fd = -1;
          ];
          
          (* The ODBC Microsoft Access driver and ODBC Excel driver don't implement ps.setLong(),
           * so attempts to insert Mathematica integers fail.  Tell Java to fall back on setInt().
           *)
          useLongs = "UseLongs" /. useOpts /. Options[SQLExecute] /. {
              "UseLongs" :> False /; StringMatchQ[rdbms, Alternatives @@ {"Microsoft Access*", "Microsoft Excel*"}],
              "UseLongs" -> True
          };
          
          (* The ODBC Microsoft Access driver and ODBC Excel driver don't support inline multirow inserts;
           * issue a message if detected.
           *)
          If[params === {{}} && StringMatchQ[rdbms, Alternatives @@ {"Microsoft Access*", "Microsoft Excel*"}]
              && StringMatchQ[sql, RegularExpression["^(?i)INSERT\\s.*VALUES\\s+(\\(.+\\),\\s*)+\\(.+\\)$"]],
              Message[SQLExecute::multirowodbc];
          ];


          (* Execute the JDBC prepared statement with parameters that can be handled by Java *)
          LoadJavaClass["com.wolfram.databaselink.SQLStatementProcessor"];
          
          (* You can enable streaming result sets on many drivers by setting the appropriate options
           * (e.g. "ForwardOnly"/TYPE_FORWARD_ONLY for Oracle).
           * However MySQL requires some weird settings and a special unprepared statement code path.
           *)
          If[rrs === {"MySQLStreaming"} && params === {{}} && 
            StringMatchQ[rdbms, "MySQL*"],
              (* rst set above *)
              rsc = 1007; (* present default, hard-coded in SQLExecute *)
              fs = -2^31; (* Integer.MIN_VALUE, required by driver, note Java has to handle values < 0 *)
              result = SQLStatementProcessor`processUnpreparedSQLStatement[
                connection, sql, TrueQ[gas], TrueQ[sch], TrueQ[rrsBool], TrueQ[rgk], maxrows, timeout, rst, rsc, ep, fd, fs, mfs];,

              (* else *)
	          (* Batch the inserts on the Mathematica side to reduce JLink memory usage.
	           * This could potentially cause problems if someone asks for a result set on an insert operation,
	           * but the Java side can't deal with that anyway.
	           * 
	           * Avoid Partition[] on params here.
	           *)
	          If[!TrueQ[jb] && bs > 0 && Length@params > 1,
	              takeIndices = With[{chunks = Ceiling[Length[params]/bs]},
	                  Table[{i*bs + 1, Min[(i + 1)*bs, Length[params]]}, {i, 0, chunks - 1}]
	              ];
	              result = Flatten[Map[
	                With[{piece = Take[params, #]}, SQLStatementProcessor`processSQLStatement[
	                    connection, sql, piece, TrueQ[gas], TrueQ[sch], TrueQ[rrsBool], TrueQ[rgk], TrueQ[useLongs],
	                    maxrows, timeout, rst, rsc, ep, fd, fs, mfs, Length@piece]] &,
	                takeIndices
	              ], 1],
	
	              (* else *)
	              (* Escape processing can be set to false only for unprepared statements *)
	              If[ep != 1 && params == {{}},
                      result = SQLStatementProcessor`processUnpreparedSQLStatement[
                          connection, sql, TrueQ[gas], TrueQ[sch], TrueQ[rrsBool], TrueQ[rgk], 
                          maxrows, timeout, rst, rsc, ep, fd, fs, mfs
                      ],
                      result = SQLStatementProcessor`processSQLStatement[
                          connection, sql, params, TrueQ[gas], TrueQ[sch], TrueQ[rrsBool], TrueQ[rgk], TrueQ[useLongs],
                          maxrows, timeout, rst, rsc, ep, fd, fs, mfs, bs
                      ]
	              ]
	          ];
          ];

          
          Which[
            MatchQ[result, {_Integer}],
              First[result],
            MatchQ[result, {_?JavaObjectQ}],
              KeepJavaObject[First[result]];
              First[result],
            cs === None,
              result,
            cs === Automatic || MatchQ[cs, _Function],
              If[TrueQ[sch],
                cols = First[result];
                results = Drop[result, 1],
                cols = Null;
                results = result
              ];  
              If[cs === Automatic,
                setColumnSymbols["Global`", cols, results],
                cs[cols, results]
              ];
              result,
            MatchQ[cs, {___Symbol}],
              If[TrueQ[sch], 
                Evaluate[cs] = Transpose[Drop[result, 1]],
                Evaluate[cs] = Transpose[result]
              ];
              result
          ]
        ]
      ]
    ]
  ]  

SQLExecute[queryName_String] :=
  Module[{query, pos},
    If[MemberQ[SQLQueryNames[], queryName], 
      pos = First[Flatten[Position[SQLQueryNames[], queryName]]];
      query = SQLQueries[][[pos]];
    ];    
    SQLExecute[query]
  ]

(* 
 * This pattern (I think undocumented) manages its own connection and is used 
 * by e.g. DatabaseExplorer to programmatically construct queries that
 * will work without an active connection. --dillont
 *)
SQLExecute[SQLSelect[connName:(_String|_JDBC),
                     table:(_SQLTable | {__SQLTable} | _String | {__String}),
                     columns:(_SQLColumn | {__SQLColumn}),
                     condition_,
                     opts:OptionsPattern[]]] :=
  Module[{conn = OpenSQLConnection[connName], data = {}},
    data = SQLSelect[conn, table, columns, condition, opts];
    CloseSQLConnection[conn];
    data
  ]

SQLExecute[conn_SQLConnection, 
           SQLSelect[connName:(_String|_JDBC),
                     table:(_SQLTable | {__SQLTable} | _String | {__String}),
                     columns:(_SQLColumn | {__SQLColumn}),
                     condition_,
                     opts:OptionsPattern[]]] :=
  SQLSelect[conn, table, columns, condition, opts]
  
(*===================================================================*)
(*================= SQLExecute helper functions =====================*)
(*===================================================================*)

formatSQL[ps_String, args_List]:=
  Module[{params, posList, otherPosList, sql, sortedPosList, indexes, somePosList, stringPosList},
  
    If[MatchQ[args, {__List}],
      params = First[args], 
      params = args
    ];
    (* Check to see if anything needs to be replaced *) 
    posList = StringPosition[ps, "`" <> ToString[#] <> "`"] & /@Range[Length[params]];
    otherPosList = StringPosition[ps, "``"];
    AppendTo[posList, otherPosList];
    If[Flatten[posList] === {}, 
      sql = ps;
      If[!MatchQ[args, {__List}],
        params = {args},
        params = args
      ],        
      (* If things need to be replaced, get a list of indexes for each replacement. 
         Then replace the values and get a set of parameters that can be handled by Java *)
      sortedPosList = Sort[Flatten[posList, 1]];
      indexes = (First[Flatten[Position[posList,#]]] & /@sortedPosList); 
      somePosList = Flatten[Position[indexes, Length[params] + 1]];
      indexes = If[# === Length[params] + 1, idx++; idx - 1, idx = # + 1; #] & /@ indexes;
      stringPosList = "`" <> ToString[indexes[[#]]] <> "`" & /@ somePosList;    
      If[MatchQ[args, {__List}],           
        {sql, params} = Transpose[formatSQL[StringReplacePart[ps, stringPosList, otherPosList], #, indexes] & /@ args];
        sql = First[sql], 
        {sql, params} = formatSQL[StringReplacePart[ps, stringPosList, otherPosList], args, indexes];
        params = {params};
      ];
    ];
    {sql, params}
  ];

formatSQL[stmt_String, params_List, indexes_List]:=
  Module[{newStmt = stmt, newParams = {}, j, i, val, localStmt, localParams, 
          timesStmt, timesParams, plusStmt, plus, minus, operator,
          inequalityStmt = "", inequalityParams = {}},
    For[j = 1, j <= Length[indexes], j++,
      i = indexes[[j]];
      val = params[[i]];
      Switch[val, 
        _Integer | _Real | _String | True | False | 
        Null | _SQLBinary | _SQLDateTime | _SQLExpr, 
          newStmt = StringReplace[newStmt, {"`" <> ToString[i] <> "`" -> "?"}];
          newParams = Append[newParams, val], 
        SQLTable[_String, ___Rule], 
          newStmt = StringReplace[newStmt, {"`" <> ToString[i] <> "`" -> First[val]}], 
        SQLColumn[_String, ___Rule], 
          newStmt = StringReplace[newStmt, {"`" <> ToString[i] <> "`" -> First[val]}],
        SQLColumn[{_String, _String}, ___Rule], 
          newStmt = StringReplace[newStmt, {"`" <> ToString[i] <> "`" -> val[[1, 1]] <> "." <> val[[1,2]]}], 
        SQLArgument[_List], 
          {localStmt, localParams} = formatSQL[ "(`1`)", {SQLArgument@@val[[1]]}, {1}];
          newStmt = StringReplace[newStmt, {"`" <> ToString[i] <> "`" -> localStmt}];
          newParams = Join[newParams, localParams],
        _SQLArgument,
          {localStmt, localParams} = formatSQL[ generateBinaryStatementNoParens[",", Length[val]], List@@val, Range[Length[val]]];
          newStmt = StringReplace[newStmt, {"`" <> ToString[i] <> "`" -> localStmt}];
          newParams = Join[newParams, localParams],
        SQLStringMatchQ[SQLColumn[_String, ___Rule], _String], 
          newStmt = StringReplace[newStmt, {"`" <> ToString[i] <> "`" -> "(" <> val[[1,1]] <> " LIKE ?)"}];
          newParams = Append[newParams, val[[2]]],
        SQLStringMatchQ[SQLColumn[{_String, _String}, ___Rule], _String], 
          newStmt = StringReplace[newStmt, {"`" <> ToString[i] <> "`" -> "(" <> val[[1, 1, 1]] <> "." <> val[[1,1,2]] <> " LIKE ?)"}];
          newParams = Append[newParams, val[[2]]],
        SQLMemberQ[_List, _SQLColumn], 
          {localStmt, localParams} = formatSQL[ "(`1` IN (`2`))", {val[[2]], SQLArgument@@val[[1]]}, {1, 2}];          
          newStmt = StringReplace[newStmt, {"`" <> ToString[i] <> "`" -> localStmt}];
          newParams = Join[newParams, localParams],
        Or[_, __],         
          {localStmt, localParams} = formatSQL[generateBinaryStatement["OR", Length[val]], List@@val, Range[Length[val]]];
          newStmt = StringReplace[newStmt, {"`" <> ToString[i] <> "`" -> localStmt}];
          newParams = Join[newParams, localParams],
        And[_, __], 
          {localStmt, localParams} = formatSQL[generateBinaryStatement["AND", Length[val]], List@@val, Range[Length[val]]];
          newStmt = StringReplace[newStmt, {"`" <> ToString[i] <> "`" -> localStmt}];
          newParams = Join[newParams, localParams],
        Not[SQLStringMatchQ[SQLColumn[_String, ___Rule], _String]], 
          newStmt = StringReplace[newStmt, {"`" <> ToString[i] <> "`" -> "(" <> val[[1,1,1]] <> " NOT LIKE ?)"}];
          newParams = Append[newParams, val[[1, 2]]],
        Not[SQLStringMatchQ[SQLColumn[{_String, _String}, ___Rule], _String]], 
          newStmt = StringReplace[newStmt, {"`" <> ToString[i] <> "`" -> "(" <> val[[1, 1, 1, 1]] <> "." <> val[[1,1,1,2]] <> " NOT LIKE ?)"}];
          newParams = Append[newParams, val[[1, 2]]],
        Not[SQLMemberQ[_List, _SQLColumn]], 
          {localStmt, localParams} = formatSQL[ "(`1` NOT IN (`2`))", {val[[1, 2]], SQLArgument@@val[[1, 1]]}, {1, 2}];          
          newStmt = StringReplace[newStmt, {"`" <> ToString[i] <> "`" -> localStmt}];
          newParams = Join[newParams, localParams],
        Not[_], 
          {localStmt, localParams} = formatSQL[ "(NOT `1`)", {First[val]}, {1}];
          newStmt = StringReplace[newStmt, {"`" <> ToString[i] <> "`" -> localStmt}];
          newParams = Join[newParams, localParams], 
        Equal[SQLColumn[_String, opts:OptionsPattern[]], Null], 
          newStmt = StringReplace[newStmt, {"`" <> ToString[i] <> "`" -> "(" <> val[[1,1]] <> " IS NULL)"}],
        Equal[SQLColumn[{_String, _String}, opts:OptionsPattern[]], Null], 
          newStmt = StringReplace[newStmt, {"`" <> ToString[i] <> "`" -> "(" <> val[[1, 1, 1]] <> "." <> val[[1,1,2]] <> " IS NULL)"}],
        Equal[Null, SQLColumn[_String, opts:OptionsPattern[]]], 
          newStmt = StringReplace[newStmt, {"`" <> ToString[i] <> "`" -> "(" <> val[[2,1]] <> " IS NULL)"}],
        Equal[Null, SQLColumn[{_String, _String}, opts:OptionsPattern[]]], 
          newStmt = StringReplace[newStmt, {"`" <> ToString[i] <> "`" -> "(" <> val[[2, 1, 1]] <> "." <> val[[2,1,2]] <> " IS NULL)"}],
        Unequal[SQLColumn[_String, opts:OptionsPattern[]], Null], 
          newStmt = StringReplace[newStmt, {"`" <> ToString[i] <> "`" -> "(" <> val[[1,1]] <> " IS NOT NULL)"}],
        Unequal[SQLColumn[{_String, _String}, opts:OptionsPattern[]], Null], 
          newStmt = StringReplace[newStmt, {"`" <> ToString[i] <> "`" -> "(" <> val[[1, 1, 1]] <> "." <> val[[1,1,2]] <> " IS NOT NULL)"}],
        Unequal[Null, SQLColumn[_String, opts:OptionsPattern[]]], 
          newStmt = StringReplace[newStmt, {"`" <> ToString[i] <> "`" -> "(" <> val[[2,1]] <> " IS NOT NULL)"}],
        Unequal[Null, SQLColumn[{_String, _String}, opts:OptionsPattern[]]], 
          newStmt = StringReplace[newStmt, {"`" <> ToString[i] <> "`" -> "(" <> val[[2, 1, 1]] <> "." <> val[[2,1,2]] <> " IS NOT NULL)"}],
        Equal[_, __] | Unequal[_, __] | LessEqual[_, __] | GreaterEqual[_, __] | Less[_, __] | Greater[_, __], 
          operator = Switch[Head[val], 
            Equal, "=", 
            Unequal, "!=", 
            LessEqual, "<=",
            GreaterEqual, ">=", 
            Less, "<", 
            Greater, ">"
          ];
          {localStmt, localParams} = formatSQL[generateEqualStatement[operator, Length[val]], List@@val, Range[Length[val]]];
          newStmt = StringReplace[newStmt, {"`" <> ToString[i] <> "`" -> localStmt}];
          newParams = Join[newParams, localParams], 
        _Inequality,
          If[Length[val] > 4 && OddQ[Length[val]],  
            Do[If[OddQ[i], 
                 inequalityStmt = inequalityStmt <> 
                   Switch[i,
                     1, "(`" <> ToString[(i + 1)/2] <> "`", 
                     Length[val], "`" <> ToString[(i + 1)/2] <> "`)",
                     _, "`" <> ToString[(i + 1)/2] <> "`) AND (`" <> ToString[(i + 1)/2] <> "`"
                   ]; 
                 AppendTo[inequalityParams, val[[i]]]
                 ,
                 inequalityStmt = inequalityStmt <> 
                   Switch[val[[i]], 
                     Equal, " = ",
                     Unequal, " != ", 
                     LessEqual, " <= ", 
                     GreaterEqual, " >= ", 
                     Less, " < ", 
                     Greater, " > "]
               ], {i, Length[val]}];          
            {localStmt, localParams} = formatSQL[inequalityStmt, inequalityParams, Range[Length[inequalityParams]]];
            newStmt = StringReplace[newStmt, {"`" <> ToString[i] <> "`" -> localStmt}];
            newParams = Join[newParams, localParams]
            ,
            Message[SQLValue::illegalvalue, val];
            Throw[$Failed] 
          ],
        Times[-1, SQLColumn[_String, opts:OptionsPattern[]]], 
          newStmt = StringReplace[newStmt, {"`" <> ToString[i] <> "`" -> "-" <> val[[2,1]]}],          
        Times[-1, SQLColumn[{_String, _String}, ___Rule]], 
          newStmt = StringReplace[newStmt, {"`" <> ToString[i] <> "`" -> "-" <> val[[2, 1, 1]] <> "." <> val[[2,1,2]]}],          
        Times[_, __], 
          Switch[Numerator[val], 
            _Times,  
              timesStmt = "(" <> generateBinaryStatementNoParens["*", Length[Numerator[val]]] <> ")";
              timesParams = List@@ Numerator[val],       
            _, 
              timesStmt = "`1`";
              timesParams = {Numerator[val]};
          ];             
          Switch[Denominator[val], 
            1, 
              Null,
            _Times,  
              timesStmt = timesStmt <> " / (" <> generateBinaryStatementNoParens["*", Length[timesParams] + 1, Length[Denominator[val]] + Length[timesParams]] <> ")";
              timesParams = Join[timesParams, List@@ Denominator[val]],
            _, 
              timesStmt = timesStmt <> " / `" <> ToString[Length[timesParams] + 1] <> "`";
              timesParams = Join[timesParams, {Denominator[val]}];
          ];
          {localStmt, localParams} = formatSQL[timesStmt, timesParams, Range[Length[timesParams]]];
          newStmt = StringReplace[newStmt, {"`" <> ToString[i] <> "`" -> localStmt}];
          newParams = Join[newParams, localParams],              
        Power[SQLColumn[_String, opts:OptionsPattern[]], -1], 
          newStmt = StringReplace[newStmt, {"`" <> ToString[i] <> "`" -> "(1/" <> val[[1,1]] <> ")"}],
        Power[SQLColumn[{_String, _String}, ___Rule], -1], 
          newStmt = StringReplace[newStmt, {"`" <> ToString[i] <> "`" -> "(1/" <> val[[1, 1, 1]] <> "." <> val[[1,1,2]] <> ")"}],
        Power[SQLColumn[_String, opts:OptionsPattern[]], x_Integer?Positive], 
          {localStmt, localParams} = formatSQL[ "(`1` * `2`)", {First[val], Power[val[[1]], val[[2]] - 1]}, {1, 2}];
          newStmt = StringReplace[newStmt, {"`" <> ToString[i] <> "`" -> localStmt}];
          newParams = Join[newParams, localParams],      
        Rational[ _Integer, _Integer ], 
          {localStmt, localParams} = formatSQL[ "(`1` / `2`)", {Numerator[val], Denominator[val]}, {1, 2}];
          newStmt = StringReplace[newStmt, {"`" <> ToString[i] <> "`" -> localStmt}];
          newParams = Join[newParams, localParams],                    
        Plus[_, __], 
          {plus, minus} = splitPlus[val];
          Switch[Length[plus], 
            0, 
              plusStmt = "", 
            1, 
              plusStmt = "`1`",
            x_/;x > 1,  
              plusStmt = generateBinaryStatementNoParens["+", Length[plus]];
          ];             
          Switch[Length[minus], 
            0, 
              Null,
            1,  
              plusStmt = plusStmt <> " - `" <> ToString[Length[plus] + 1] <> "`",
            _, 
              plusStmt = plusStmt <> " - " <> generateBinaryStatementNoParens["-", Length[plus] + 1, Length[plus] + Length[minus]];
          ];
          {localStmt, localParams} = formatSQL[plusStmt, Join[plus, minus], Range[Length[Join[plus, minus]]]];
          newStmt = StringReplace[newStmt, {"`" <> ToString[i] <> "`" -> localStmt}];
          newParams = Join[newParams, localParams],                        
        SQLColumn[_String, opts:OptionsPattern[]] -> "Ascending",
          newStmt = StringReplace[newStmt, {"`" <> ToString[i] <> "`" -> val[[1, 1]] <> " ASC"}],          
        SQLColumn[{_String, _String}, opts:OptionsPattern[]] -> "Ascending",
          newStmt = StringReplace[newStmt, {"`" <> ToString[i] <> "`" -> val[[1, 1, 1]] <> "." <> val[[1,1,2]] <> " ASC"}],          
        SQLColumn[_String, opts:OptionsPattern[]] -> "Descending",
          newStmt = StringReplace[newStmt, {"`" <> ToString[i] <> "`" -> val[[1, 1]] <> " DESC"}],          
        SQLColumn[{_String, _String}, opts:OptionsPattern[]] -> "Descending",
          newStmt = StringReplace[newStmt, {"`" <> ToString[i] <> "`" -> val[[1, 1, 1]] <> "." <> val[[1,1,2]] <> " DESC"}], 
        Rule[_SQLColumn, _], 
          {localStmt, localParams} = formatSQL[ "`1` = `2`", {First[val], Last[val]}, {1, 2}];
          newStmt = StringReplace[newStmt, {"`" <> ToString[i] <> "`" -> localStmt}];
          newParams = Join[newParams, localParams],
        _,
          Message[SQLValue::illegalvalue, val];
          Throw[$Failed]
      ];
    ];
    {newStmt, newParams}
  ]


generateBinaryStatement[ operator_String, length_Integer] :=
  Module[{table},
    table = Reverse[Table["`" <> ToString[i] <> "`", {i, length}]]; 
    Fold["(" <> ToString[#2] <> " " <> operator <> " " <> ToString[ #1] <> ")" &, First[table], Rest[table]]	
  ]

generateBinaryStatementNoParens[ operator_String, length_Integer] :=
  Module[{table},
    table = Table["`" <> ToString[i] <> "`", {i, length}]; 
    Fold[ToString[#1] <> " " <> operator <> " " <> ToString[ #2] &, First[table], Rest[table]]	
  ]
  
generateBinaryStatementNoParens[ operator_String, min_Integer, max_Integer] :=
  Module[{table},
    table = Table["`" <> ToString[i] <> "`", {i, min, max}]; 
    Fold[ToString[#1] <> " " <> operator <> " " <> ToString[ #2] &, First[table], Rest[table]]	
  ]

generateEqualStatement[operator_String, length_Integer] :=
  Module[{table},
    table = Reverse[Table["`" <> ToString[i] <> "`", {i, length}]];
    Fold[(
      Switch[#2, 
        #1, #2 <> ")", 
        Last[table],  "(" <> #2 <> " " <> operator <> " " <> #1 ,
        _, #2 <> ") AND (" <> #2 <> " " <> operator <> " " <> #1
      ]) &, First[table], table]
  ]
  
splitPlus[x_Plus] := 
  Module[{list}, 
    list = List @@ x;
    list = Map[If[MatchQ[#, Times[x1_ /; x1 < 0, ___] | x1_ /; x1 < 0], {{}, {-1 #}}, {{#}, {}}] &, list];
    list = Transpose[list];
    Map[Flatten, list]
  ]

mingleObject[lst_List, object_] := 
  If[lst === {}, 
    {}, 
    Drop[Flatten[Thread[{lst, object}, List, 1], 1, List], -1]
  ]
    
mingleComma[ lst_List ] := 
    mingleObject[ lst, ", " ];

mingleComma[ lst_ ] := 
    mingleObject[ {lst}, ", " ];              

areSpaces[str_String] := Or@@((# === " ") &/@(Characters@str))

stripSpaces[ str_String ] := 
    FixedPoint[ If[ StringTake[#, 1] === " ", 
                    StringDrop[#, 1], #
                  ]&, 
                FixedPoint[ If[ StringTake[#, -1] === " ", 
                                StringDrop[#, -1], #
                              ]&, 
                            str
                          ]
              ]

setColumnSymbols[context_String, cols_List | cols:Null, results_List] := 
  Module[{columnSymbols = {}},
    If[cols === Null, 
      columnSymbols = Symbol[context <> "col" <> ToString[#]] & /@ Range[Length[First[results]]];              
      ,      
      columnSymbols = Symbol[context <> normalizeSymbolName[#]] & /@ cols;
    ];
    Evaluate[columnSymbols] = Transpose[results];
  ];

normalizeSymbolName[name_String] :=
  StringReplace[name, {"."->"", "_"->"", "~"->"", "!"->"", 
                       "@"->"", "#"->"", "$"->"", "%"->"", "^"->"", 
                       "&"->"", "*"->"", "("->"", ")"->"", "-"->"", 
                       "+"->"", "="->"", "{"->"", "["->"", "}"->"", 
                       "]"->"", "|"->"", "\\"->"", ":"->"", ";"->"",
                       "\""->"", "\'"->"", "<"->"", ","->"", ">"->"",
                       "?"->"", "/"->"", " "->""}]
  
(*===================================================================*)
(*========================== ResultSets  ============================*)
(*===================================================================*)

$resultSetIndex = 0;

If[!ListQ[$resultSets],
  $resultSets = {};
];

SQLResultSets[] := $resultSets;

SetAttributes[ SQLResultSetOpen, HoldFirst]

SQLResultSetOpen[ (s_SQLSelect | s_SQLExecute), opts:OptionsPattern[]] := Module[
	{resultSet, useOpts, mode, fetchDir, fetchSize}, 
 
    useOpts = canonicalOptions[Flatten[{opts}]];
    mode = "Mode" /. useOpts /. Options[ SQLResultSetOpen ];
   	fetchDir = "FetchDirection" /. useOpts /. Options[ SQLResultSetOpen ];
   	fetchSize = "FetchSize" /. useOpts /. Options[ SQLResultSetOpen ];
   	
    resultSet = ReleaseHold[Insert[Hold[s], "ResultSet" -> {mode}, {1, -1}]];
    If[!JavaObjectQ[resultSet] || resultSet === $Failed, Return[$Failed]];
    resultSet = SQLResultSet[$resultSetIndex++, resultSet];
    AppendTo[$resultSets, resultSet];
    SetSQLResultSetOptions[ resultSet, "FetchDirection" -> fetchDir, "FetchSize" -> fetchSize];
    resultSet
]



SQLResultSet /:
	SetOptions[ SQLResultSet[id_Integer, rs_?JavaObjectQ ]
                          , opts___] := 
		SetSQLResultSetOptions[ SQLResultSet[id, rs], opts]


SetSQLResultSetOptions[SQLResultSet[ id_Integer, rs_?JavaObjectQ ], opts:OptionsPattern[]] := 
  Module[{fs, fd, optTest},
  
	optTest = FilterRules[ {opts}, Except[Options[SQLResultSet]]];
    	If[ optTest =!= {}, optionsErrorMessage[optTest, SQLResultSetOpen, SQLResultSet]; Return[$Failed]];
  
    {fs, fd} = 
      {"FetchSize", "FetchDirection"} 
         /. canonicalOptions[Flatten[{opts}]] /. Options[ SQLResultSet];
         
    Switch[fd,
      Automatic, Null,
      "Forward", rs@setFetchDirection[1000],
      "Reverse", rs@setFetchDirection[1001], 
      "Unknown", rs@setFetchDirection[1002],
      _, 
        Message[SQLExecute::fetchdirection, fd];
    ];

    If[fs =!= Automatic && fs =!= All, 
      If[fs === None, fs = 0];
      If[!IntegerQ[fs] || fs < 0, 
        Message[SQLExecute::fetchsize, fs]
      ],fs = 0;
    ];
    rs@setFetchSize[fs];
    SQLResultSet[ id, rs]
  ]

SQLResultSetClose[ SQLResultSet[ id_Integer, rs_?JavaObjectQ ] ] := 
  Module[ {}, 
    If[ ( MemberQ[ $resultSets, SQLResultSet[ id, rs ] ] ), 
      $resultSets = Drop[$resultSets, First@Position[$resultSets, SQLResultSet[id, rs]]]; 
      rs@close[];
      ReleaseJavaObject[rs];
    ] 
  ] 
  
SQLResultSetCurrent[ SQLResultSet[ _Integer, rs_?JavaObjectQ], opts:OptionsPattern[] ] := 
  Module[ {useOpts, gas = False, results},
    Block[{$JavaExceptionHandler = ThrowException},     
      Catch[
        useOpts = canonicalOptions[Flatten[{opts}]];
        gas = "GetAsStrings" /. useOpts /. Options[ SQLResultSetCurrent ]; 
  
        results = SQLStatementProcessor`getLimitedResultData[0, rs, gas];
        If[ListQ[results] && Length[results] > 0, First[results], results]
      ]
    ]
  ]

SQLResultSetRead[ rs_SQLResultSet, opts:OptionsPattern[] ] := 
  Module[{results}, 
    results = SQLResultSetRead[rs, 1, opts];
    If[ListQ[results] && Length[results] > 0, First[results], results]
  ]

SQLResultSetRead[ rs_SQLResultSet, 0, opts:OptionsPattern[] ] := {}

SQLResultSetRead[ SQLResultSet[ _Integer, rs_?JavaObjectQ], limit_Integer, opts:OptionsPattern[]] := 
  Module[ {useOpts, gas = False},
    Block[{$JavaExceptionHandler = ThrowException},     
      Catch[
        useOpts = canonicalOptions[Flatten[{opts}]];
        gas = "GetAsStrings" /. useOpts /. Options[ SQLResultSetRead ]; 
  
        If[limit < 0 && rs@getType[] == 1003,
          Message[SQLResultSet::forwardonly];
          Return[$Failed];
        ];            
        SQLStatementProcessor`getLimitedResultData[limit, rs, gas]
      ]
    ]
  ]

SQLResultSetTake[ rs_SQLResultSet, {start_}, opts:OptionsPattern[]] :=
  SQLResultSetTake[rs, {start, start}, opts]  

SQLResultSetTake[ SQLResultSet[ _Integer, rs_?JavaObjectQ], {start_Integer, end_Integer}, opts:OptionsPattern[]] :=
  Module[ {useOpts, gas = False, eRow = 0, sRow = 0, diff, current},
    Block[{$JavaExceptionHandler = ThrowException},     
      Catch[
        current = rs@getRow[];
        
        If[rs@getType[] == 1003,
          Message[SQLResultSet::forwardonly];
          Return[$Failed];
        ];            

        useOpts = canonicalOptions[Flatten[{opts}]];
        gas = "GetAsStrings" /. useOpts /. Options[ SQLResultSetTake ]; 
        
        If[start == 0, 
          Message[SQLResultSetTake::invalidrange, {start, end}];Return[$Failed]
        ];

        If[end =!= 0, 
          If[!rs@absolute[end], 
            resetCursor[rs, current];
            Message[SQLResultSetTake::invalidrange, {start, end}];
            Return[$Failed]
          ];
          eRow = rs@getRow[], 
          Message[SQLResultSetTake::invalidrange, {start, end}];
          Return[$Failed]
        ];

        If[!rs@absolute[start],
          resetCursor[rs, current];
          Message[SQLResultSetTake::invalidrange, {start, end}];
          Return[$Failed];
        ];
        sRow = rs@getRow[];
        
        diff = eRow - sRow;
        Which[
          diff < 0, 
            diff = diff - 1;
            If[!rs@relative[1], rs@afterLast[]],
          diff > 0, 
            diff = diff + 1;
            If[!rs@relative[-1], rs@beforeFirst[]]
        ];

        SQLStatementProcessor`getLimitedResultData[diff, rs, gas]
      ]
    ]
  ]

resetCursor[rs_, position_Integer] :=
  Module[{}, 
    If[!rs@absolute[position], 
      If[position == 0, rs@beforeFirst[], rs@afterLast[]]
    ]
  ]

SQLResultSetShift[ SQLResultSet[ _Integer, rs_?JavaObjectQ], rows_Integer] := 
  Module[ {valid = False},
    Block[{$JavaExceptionHandler = ThrowException},     
      Catch[
        If[rs@getType[] == 1003, (* This is TYPE_FORWARD_ONLY *)
          If[rows < 0, 
            Message[SQLResultSet::forwardonly];
            Return[$Failed];
          ];
          For[i = 0, i < rows, i++, valid = rs@next[]]; 
          valid,
          rs@relative[rows]        
        ]
      ]
    ]
  ]
  
SQLResultSetGoto[ SQLResultSet[ _Integer, rs_?JavaObjectQ], row_Integer | row:Infinity] := 
  Block[{$JavaExceptionHandler = ThrowException},     
    Catch[
      If[rs@getType[] == 1003,
        Message[SQLResultSet::forwardonly];
        Return[$Failed];
      ];   
      Switch[row, 
        0, rs@beforeFirst[];False,
        Infinity, rs@afterLast[];False, 
        _, rs@absolute[row]
      ] 
    ]
  ]

SQLResultSetPosition[ SQLResultSet[ _Integer, rs_?JavaObjectQ]] := 
  Block[{$JavaExceptionHandler = ThrowException},     
    Catch[rs@getRow[]]]
    
SQLResultSetColumnNames[ SQLResultSet[ _Integer, rs_?JavaObjectQ]] :=
  Block[{$JavaExceptionHandler = ThrowException},     
    Catch[
      SQLStatementProcessor`getHeadings[ rs, True]
    ]
  ]
  

(*===================================================================*) 
(*======================== Transactions =============================*) 
(*===================================================================*) 

$inTransaction = False;

SQLBeginTransaction[SQLConnection[ _JDBC, connection_, _Integer, ___Rule]] :=
  (If[!JavaObjectQ[connection], 
     Message[SQLConnection::conn];
     Return[$Failed]
   ];    
   If[$inTransaction,
     Message[SQLBeginTransaction::nested],
     $inTransaction = True;
     connection@setAutoCommit[False]
   ])

SQLCommitTransaction[SQLConnection[ _JDBC, connection_, _Integer, ___Rule]] :=
  (If[!JavaObjectQ[connection], 
     Message[SQLConnection::conn];
     Return[$Failed]
   ];    
   connection@commit[];
   $inTransaction = False;
   connection@setAutoCommit[True];)

SQLRollbackTransaction[SQLConnection[ _JDBC, connection_, _Integer, ___Rule]] :=
  (If[!JavaObjectQ[connection], 
     Message[SQLConnection::conn];
     Return[$Failed]
   ];    
   If[!connection@getAutoCommit[], 
     connection@rollback[];
     $inTransaction = False;
     connection@setAutoCommit[True];
   ])

SQLRollbackTransaction[SQLConnection[ _JDBC, connection_, _Integer, ___Rule], 
                       SQLSavepoint[savepoint_, opts:OptionsPattern[]]] :=
  (If[!JavaObjectQ[connection], 
     Message[SQLConnection::conn];
     Return[$Failed]
   ];    
   If[!JavaObjectQ[savepoint], 
     Message[SQLRollbackTransaction::savepoint];
     Return[$Failed]
   ];    
   If[!connection@getAutoCommit[], 
     connection@rollback[savepoint];
   ])

SetAttributes[SQLReleaseSavepoint,HoldRest]

SQLReleaseSavepoint[SQLConnection[ _JDBC, connection_, _Integer, ___Rule], 
                       sqlSavepoint_Symbol] :=
    Module[ {javaSavepoint, spSymbolName},
        If[ !JavaObjectQ[connection],
            Message[SQLConnection::conn];
            Return[$Failed]
        ];
        
        spSymbolName=SymbolName[Unevaluated[sqlSavepoint]];
        If[ !MatchQ[sqlSavepoint, SQLSavepoint[_, _]],
            Message[SQLReleaseSavepoint::sqlsavepoint,spSymbolName];
            Return[$Failed]
        ];
        
        javaSavepoint = ReleaseHold[sqlSavepoint][[1]];
        If[ !JavaObjectQ[javaSavepoint],
            Message[SQLReleaseSavepoint::javasavepoint,javaSavepoint,spSymbolName];
            Return[$Failed]
        ];
        
        connection@releaseSavepoint[javaSavepoint];
        Clear[sqlSavepoint]
    ]

SQLSetSavepoint[SQLConnection[ _JDBC, connection_, _Integer, ___Rule]] :=
  (If[!JavaObjectQ[connection], 
     Message[SQLConnection::conn];
     Return[$Failed]
   ];    
   Check[SQLSavepoint[connection@setSavepoint[]], 
     (Message[SQLSetSavepoint::version];$Failed), JLink`Java::nometh])

SQLSetSavepoint[SQLConnection[ _JDBC, connection_, _Integer, ___Rule], name_String] :=
  (If[!JavaObjectQ[connection], 
     Message[SQLConnection::conn];
     Return[$Failed]
   ];    
   Check[SQLSavepoint[connection@setSavepoint[name], "Name"->name], 
     (Message[SQLSetSavepoint::version];$Failed), JLink`Java::nometh])

(*===================================================================*) 
(*====================== Stored Procedures ==========================*) 
(*===================================================================*) 

(*
InstallStoredProcedures[SQLConnection[ _JDBC, connection_, _Integer, ___Rule],
                        context_String] :=
  Module[{meta, procedures},

    If[!StringMatchQ[context, "*`"], 
      Message[
        InstallService::"context", 
        context, 
        "contexts must end with a '`'"];
      Return[$Failed];
    ];
  
    If[!MatchQ[
         StringPosition[
           context, 
           {".", "_", "~", "!", "@", "#", "$", "%", "^", 
            "&", "*", "(", ")", "-", "+", "=", "{", "[", 
            "}", "]", "|", "\\", ":", ";", "\"", "\'", 
            "<", ",", ">", "?", "/", " "}], 
         {}], 
      Message[
        InstallService::"context", 
        context, 
        "contexts must be alpha-numeric."];
      Return[$Failed];
    ];
 
    positions = 
      Drop[Prepend[(First[#] + 1) & 
        /@ StringPosition[context, "`"], 1], -1];
    test = (If[DigitQ[StringTake[context, {#,#}]], $Failed] & /@ positions);
    If[Length[Cases[test, $Failed]] > 0, 
      Message[
        InstallService::"context", 
        context, 
        "contexts must not begin with a digit."];
      Return[$Failed];
    ];
    
    meta = connection@getMetaData[];
    procedures = meta @getProcedures[Null, Null, Null];
    SQLStatementProcessor`getResultData[procedures, False, False]
  ]
*)

(*===================================================================*) 
(*====================== SQL Server =================================*) 
(*===================================================================*) 

$serverIndex = 0;

SQLServers[] := $sqlServers;

If[!ListQ[$sqlServers], 
  $sqlServers = {};
];

SQLServerLaunch[databases:{Rule[_String,_String] .. }, opts:OptionsPattern[]] := 
  JavaBlock[
    Module[{server, useOpts, name, address, port, ss, id, s },
      Block[{$JavaExceptionHandler = ThrowException},
        Catch[
        
          (* Process Options *)
          useOpts  = canonicalOptions[Flatten[{opts}]];
          name     = "Name" /. useOpts /. Options[ SQLServerLaunch ];
          address  = "Address" /. useOpts /. Options[ SQLServerLaunch ];
          port     = "Port" /. useOpts /. Options[ SQLServerLaunch ];
          ss       = "SecureSockets" /. useOpts /. Options[ SQLServerLaunch ];
          
          If[address =!= Automatic && !StringQ[address], 
            Message[SQLServerLaunch::address, address];
            Return[$Failed];
          ];
          
          If[port =!= Automatic && !IntegerQ[port], 
            Message[SQLServerLaunch::port, port];
            Return[$Failed];
          ];
          
          (* Create Server *)        
          InstallJava[];
          server = JavaNew["org.hsqldb.Server"];
          KeepJavaObject[server];
      
          (* Configure server *)
          server@setNoSystemExit[True];
          If[address =!= Automatic, server@setAddress[address]];
          If[port =!= Automatic, server@setPort[port]];
          server@setTls[TrueQ[ss]];
      
          (* Add databases *)  
          Do[
            server@setDatabaseName[i - 1, First[databases[[i]]]];
            server@setDatabasePath[i - 1, Last[databases[[i]]]];
            , {i, Length[databases]}];
          
          (* Start server *)          
          server@start[];
          
          id = ++$serverIndex;
          s = SQLServer[server, id, opts];
          AppendTo[$sqlServers, s];
          s
        ]
      ]
    ]
  ]

SQLServerShutdown[ SQLServer[
                      server_,
                      id_Integer,
                      ___Rule]] :=  
                     
  If[JavaObjectQ[server], 
    server@shutdown[];
    ReleaseJavaObject[server];
    $sqlServers = Drop[ $sqlServers, 
                        First@Position[ $sqlServers, 
                                        SQLServer[_, id, ___Rule]]];      
  ]  
  
SQLServerInformation[ SQLServer[
                      server_?JavaObjectQ,
                      id_Integer,
                      ___Rule]] := 
  {{"ADDRESS", "PORT", "PRODUCT_NAME", "PRODUCT_VERSION", "PROTOCOL", "SECURE_SOCKETS", "STATE"}, 
   {server@getAddress[], server@getPort[], server@getProductName[], server@getProductVersion[], server@getProtocol[], server@isTls[], server@getStateDescriptor[]}}


End[] (* `SQL`Private` *)
