(* ::Package:: *)

(* ::Title:: *)
(*SQLite database connectivity package*)


(* ::Text:: *)
(*This package provides basic wrappers for creating and querying SQLite databases.*)


SQLiteOpenDatabase[db_String]:=Database`OpenDatabase[db];


SQLiteCloseDatabase[db_Database`Database]:=Database`CloseDatabase[db];


SQLiteCreateTable[db_Database`Database, tableName_String, columnNames_List, columnTypes_List]:=Module[{query},
	query = "CREATE TABLE "<>tableName<>" (id INTEGER PRIMARY KEY, "<>StringJoin[Riffle[Riffle[ToString/@columnNames,ToString/@columnTypes],{" ",", "}]]<>");";
	Database`QueryDatabase[db, query]
];


SQLiteInsertRecord[db_Database`Database,tableName_String,columnNames_List,values_List]:=Module[{query},
	query = "INSERT INTO "<>tableName<>" ("<>StringJoin[Riffle[ToString/@columnNames,", "]]<>") values ("<>StringJoin[Riffle[ToString/@Table["?",{Length[columnNames]}],", "]]<>");";
    Database`QueryDatabase[db, query, values]
];


SQLiteLookupRecord[db_Database`Database,tableName_String,columnNames_List,values_List]:=Module[{query,validColumns,validValues,returnedColumns},
	validColumns = Delete[columnNames,Position[values,Null]];
	returnedColumns = Delete[columnNames,Complement[Table[{i},{i,Length[values]}],Position[values,Null]]];
	If[Length[returnedColumns]==0, returnedColumns = {"*"}];
	validValues = Delete[values,Position[values,Null]];
	validValues = Table[If[TrueQ[Head[validValues[[i]]]==String],"\""<>validValues[[i]]<>"\"",validValues[[i]]],{i,Length[validValues]}];
	query = "SELECT "<>StringJoin[Riffle[ToString/@returnedColumns,", "]]<>" FROM "<>tableName<>" WHERE "<>StringJoin[Riffle[Riffle[ToString/@validColumns,ToString/@validValues],If[Length[validColumns]==1,{" = "},{" = "," and "}]]]<>";";
    Database`QueryDatabase[db, query]
];
