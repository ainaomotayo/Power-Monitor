(* Author:          Christopher Williamson *)
(* Copyright:       Copyright 2004-2013, Wolfram Research, Inc. *)

BeginPackage[ "DatabaseLink`"]

`Information`$Version = "DatabaseLink Version 3.0.0 (December 2013)";

`Information`$VersionNumber = 3.0;

`Information`$ReleaseNumber = 0;

(* Usage Statements *)

AddDatabaseResources::usage = 
"AddDatabaseResources[ dir] adds a directory to the DatabaseResources path."

CloseSQLConnection::usage = 
"CloseSQLConnection[ conn] disconnects the current connection associated with a data source."

DatabaseExplorer::usage=
"DatabaseExplorer[ ] launches a graphical user interface to DatabaseLink."

DataSourceNames::usage = 
"DataSourceNames[ ] returns a list of the names of data sources made available through the DatabaseResourcesPath[]."

DataSources::usage = 
"DataSources[ ] returns a list of information about named data sources made available through the DatabaseResourcesPath[].
DataSources[ \"name\"] returns a list of information about the data sources called name."

DatabaseResourcesPath::usage = 
"DatabaseResourcesPath[ ] gives the list of directories that are searched to find database resources."

(*
InstallStoredProcedures::usage = ""
*)

JDBC::usage = 
"JDBC[ args] is an object that holds parameters for making a JDBC connection to a database."

JDBCDriver::usage = 
"JDBCDriver[ args] specifies the configuration for connecting to a database produced by a specific vendor."

JDBCDriverNames::usage = 
"JDBCDriverNames[ ] returns a list of the names of databases for which JDBC drivers are available through the DatabaseResourcesPath[]."

JDBCDrivers::usage = 
"JDBCDrivers[ ] returns a list of information about JDBC drivers available through the DatabaseResourcesPath[].
JDBCDrivers[ \"name\"] returns a list of information about the JDBC driver called name."

WriteDataSource::usage = 
"WriteDataSource[ name, opts] stores on the file system an SQLConnection configuration."

OpenSQLConnection::usage = 
"OpenSQLConnection[ src, opts] makes a connection to a data source.
OpenSQLConnection[ ] opens a GUI that helps to make a connection to a data source."

SQLConnectionOpenQ::usage =
"SQLConnectionOpenQ[ conn] returns a boolean indicating whether conn is open or closed on the client."

SQLConnectionUsableQ::usage =
"SQLConnectionUsableQ[ conn] returns a boolean indicating whether queries may be executed with conn."

SQLArgument::usage = 
"SQLArgument[ arg1, arg2, ...] holds a sequence of arguments to an SQL query."

SQLBeginTransaction::usage = 
"SQLBeginTransaction[ conn] initiates an SQL transaction. A group of SQL commands grouped into a transaction will only take effect permanently when the transaction is committed."

SQLBinary::usage = 
"SQLBinary[ data] represents raw binary data that can be stored in a database."

SQLCatalogNames::usage = 
"SQLCatalogNames[ conn] returns the names of the catalogs in an SQL connection."

SQLColumn::usage = 
"SQLColumn[ args] represents a column in an SQL table."

SQLColumnInformation::usage = 
"SQLColumnInformation[ conn] returns a list of information about the columns in an SQL connection."

SQLColumnNames::usage = 
"SQLColumnNames[ conn] returns a list of elements, {table, name}, for each column in an SQL connection."

SQLColumnPrivileges::usage = 
"SQLColumnPrivileges[ conn] returns a table of access rights about the columns in an SQL connection."

SQLColumns::usage = 
"SQLColumns[ conn] returns the SQLColumn objects for each column in an SQL connection."

SQLCommitTransaction::usage = 
"SQLCommitTransaction[ conn] commits an SQL transaction. A group of SQL commands grouped into a transaction will only take effect permanently when the transaction is committed."

SQLConnection::usage = 
"SQLConnection[ args] is an object that represents a connection to a data source."

SQLConnectionInformation::usage = 
"SQLConnectionInformation[ conn] returns a list of information about the SQL connection.
SQLConnectionInformation[ conn, mdi] returns the value of the meta data item (mdi) requested.
SQLConnectionInformation[ conn, {mdi1, mdi2, ...}] returns a list of the values of the meta data items (mdi) requested."

SQLConnectionWarnings::usage = 
"SQLConnectionWarnings[ conn] returns a list of warnings for the SQL connection."

SQLConnectionPool::usage = 
"SQLConnectionPool[ args] is an object that represents a connection pool to a data source."

SQLConnectionPoolClose::usage = 
"SQLConnectionPoolClose[ args] closes a connection pool."

SQLCreateTable::usage = 
"SQLCreateTable[ conn, table, columns, opts] creates a new table in an SQL connection."

SQLDataTypeInformation::usage = 
"SQLDataTypeInformation[ conn] returns information about the data types that can be stored in an SQL connection."

SQLDataTypeNames::usage = 
"SQLDataTypeNames[ conn] returns the names of datatypes that can be stored in an SQL connection."

SQLDateTime::usage = 
"SQLDateTime[ date|time] represents date and time information that can be stored in a database."

SQLDelete::usage = 
"SQLDelete[ conn, table] deletes the data in a table in an SQL connection.
SQLDelete[ conn, table, cond] deletes data that matches cond. This function should be used cautiously."

SQLDropTable::usage = 
"SQLDropTable[ conn, table] drops a table in an SQL connection. This function should be used with caution."

SQLExecute::usage = 
"SQLExecute[ conn, comm, args...] executes a command in an SQL connection."

SQLExpr::usage = 
"SQLExpr[ expr] allows a Mathematica expression to be stored in a database."

SQLInsert::usage = 
"SQLInsert[ conn, table, cols, data] inserts data into a table in an SQL connection."

SQLMemberQ::usage =
"SQLMemberQ[ data, column] is used to test the value of data in a column when using a condition as part of an SQL query."

SQLQueries::usage = 
"SQLQueries[ ] returns a list of datasources made available through the DatabaseResourcesPath[]."

SQLQueryNames::usage = 
"SQLQueryNames[ ] returns a list of the names of datasources made available through the DatabaseResourcesPath[].  Each name can be used to execute a query."

SQLResultSet::usage = 
"SQLResultSet[ args] is an object that represents the results from an SQL query."

SQLResultSetColumnNames::usage = 
"SQLResultSetColumnNames[ rs] returns a list of elements, {table, name}, for each column in a result set."

SQLResultSetClose::usage = 
"SQLResultSetClose[ rs] closes a result set."

SQLResultSetCurrent::usage = 
"SQLResultSetCurrent[ rs] reads the current row from a result set."

SQLResultSetGoto::usage = 
"SQLResultSetGoto[ rs, pos] sets the current position of a result set to pos."

SQLResultSetOpen::usage = 
"SQLResultSetOpen[ query, opts] makes a result set from an SQL query."

SQLResultSetPosition::usage = 
"SQLResultSetPosition[ rs] returns an integer that specifies the current position in a result set."

SQLResultSetRead::usage = 
"SQLResultSetRead[ rs] shifts the current position and then reads a row from a result set.
SQLResultSetRead[ rs, num] reads num rows from a result set."

SQLResultSetShift::usage = 
"SQLResultSetShift[ rs, num] shifts the current position of a result set by num."

SQLResultSetTake::usage = 
"SQLResultSetTake[ rs, {m, n}] reads rows m through n from a result set."

SQLRollbackTransaction::usage = 
"SQLRollbackTransaction[ conn] is used to terminate an SQL transaction.
SQLRollbackTransaction[ conn, savepoint] is used to return to an SQLSavepoint.
A group of SQL commands grouped into a transaction will only take effect permanently when the transaction is committed."

SQLReleaseSavepoint::usage = 
"SQLReleaseSavepoint[ conn, savepoint] removes the given savepoint from the current transaction."

SQLSavepoint::usage = 
"SQLSavepoint[ args] is an object that represents a savepoint in an SQL transaction."

SQLSchemaInformation::usage=
"SQLSchemaInformation[ conn] returns information about the schemas available through an SQL connection."

SQLSchemaNames::usage = 
"SQLSchemaNames[ conn] returns the names of the schema in an SQL connection."

SQLSelect::usage = 
"SQLSelect[ conn, table]  extracts data from a table in an SQL connection.
SQLSelect[ conn, table, cols]  extracts data from particular columns.
SQLSelect[ conn, table, cols, cond]  only extracts data that matches cond."

SQLServer::usage = 
"SQLServer[ args] is an object that represents a server process started in Mathematica."

SQLServerInformation::usage = 
"SQLServerInformation[ server] returns a list of information about the SQL server."

SQLServerLaunch::usage = 
"SQLServerLaunch[ {name->location .. }] launches a database server that hosts access to the databases specified in the parameters."

SQLServerShutdown::usage = 
"SQLServerShutdown[ server] shuts down an active SQLServer started in Mathematica."

SQLSetSavepoint::usage = 
"SQLSetSavepoint[ conn, name] creates a savepoint to be used as part of an SQL transaction."

SQLStringMatchQ::usage =
"SQLStringMatchQ[ col, patt]  uses patt to test the value of data in a column when using a condition as part of an SQL query. The actual format for the pattern varies from one database to another."

SQLTable::usage = 
"SQLTable[ args] represents a table in an SQL connection."

SQLTableExportedKeys::usage = 
"SQLTableExportedKeys[ conn] returns a table of foreign key descriptions that reference the table's primary key."

SQLTableImportedKeys::usage = 
"SQLTableImportedKeys[ conn] returns a table of primary key descriptions that are referenced by the table's foreign key."

SQLTableIndexInformation::usage = 
"SQLTableIndexInformation[ conn] returns a table of indices and statistics for a table."

SQLTableInformation::usage = 
"SQLTableInformation[ conn] returns a list of information about the tables in an SQL connection."

SQLTableNames::usage = 
"SQLTableNames[ conn] returns the names of each table in an SQL connection."

SQLTablePrimaryKeys::usage = 
"SQLTablePrimaryKeys[ conn] returns a table of primary key descriptions."

SQLTablePrivileges::usage = 
"SQLTablePrivileges[ conn] returns a table of access rights about the tables in an SQL connection."

SQLTables::usage = 
"SQLTables[ conn] returns the SQLTable objects for each table in an SQL connection."

SQLTableTypeNames::usage = 
"SQLTableTypeNames[ datasourceobject] returns the names of the table types in the current data source."

SQLTableVersionColumns::usage = 
"SQLTableVersionColumns[ conn] retrieves an unordered description of a table's columns that are automatically updated when any value in a row is updated."

SQLUserDefinedTypeInformation::usage = 
"SQLUserDefinedTypeInformation[ conn] retrieves a description of the user-defined types (UDTs) defined in a particular schema."

SQLUpdate::usage = 
"SQLUpdate[ conn, table, cols, data]  updates data in a table in an SQL connection."

SQLConnections::usage = 
"SQLConnections[ ] returns a list of the open SQLConnections."

SQLServers::usage = 
"SQLServers[ ] returns a list of the open SQLServers."

SQLResultSets::usage = 
"SQLResultSets[ ] returns a list of the open SQLResultSets."

SQLConnectionPools::usage = 
"SQLConnectionPools[ ] returns a list of the open SQLConnectionPools."

$SQLTimeout::usage = 
"$SQLTimeout gives the default time in seconds that DatabaseLink waits while opening connections and executing database queries.";

$SQLUseConnectionPool::usage = 
"$SQLUseConnectionPool specifies whether a connection pool is used to retrieve a connection.";

$DatabaseLinkDirectory::usage =
"$DatabaseLinkDirectory gives the directory where DatabaseLink is installed."

Begin["`Package`"];

(*
 It is better to just create these package symbols alone in 
 the Package context, not their implementation as well.
*)
canonicalOptions

testOpt

JoinOptions

$databaseLinkPackageDirectory

$DatabaseLinkDirectory

End[]; (* DatabaseLink`Package` *)

(* Make the Package` symbols visible to all implementation files as they are read in. *)
AppendTo[$ContextPath, "DatabaseLink`Package`"]

Begin["`Private`"];

SetAttributes[ canonicalOptions, {Listable}];
canonicalOptions[name_Symbol -> val_] := SymbolName[name] -> val;
canonicalOptions[expr___] := expr;

(*
 Utility for joining options
*)

testOpt[ hash_, _[name_ , val_]] :=
	If[ hash[name] === True, False, hash[name] = True]

JoinOptions[ opts___] :=
	Module[ {optList, found, ef},
		optList = Join[opts];
		ef = Select[ optList, testOpt[found,#]&];
		Clear[ found];
		ef
	]
	


(* Set package directory used to find implementation files *)
$databaseLinkPackageDirectory = DirectoryName[System`Private`FindFile[$Input]];

$DatabaseLinkDirectory = $databaseLinkPackageDirectory;

End[]; (* DatabaseLink`Private` *)

Get[FileNameJoin[{$databaseLinkPackageDirectory, "Kernel", #}]] & /@ {
	"DataSources.m",
	"SQL.m",
	"UI.m",
	"DataSourceWizard.m",
	"JDBCWizard.m",
	"DatabaseExplorer.m"
};

EndPackage[] (* DatabaseLink` *)
