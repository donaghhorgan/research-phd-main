(* ::Package:: *)

(* ::Title:: *)
(*Database logging functions*)


(* ::Subsection::Closed:: *)
(*Copyright notice*)


(* ::Text:: *)
(*Mathematica database logging functions.*)
(*Copyright (C) 2012 Donagh Horgan.*)
(*Email: donaghh@rennes.ucc.ie.*)
(**)
(*This program is free software : you can redistribute it and/or modify*)
(*it under the terms of the GNU General Public License as published by*)
(*the Free Software Foundation, either version 3 of the License, or*)
(*(at your option) any later version.*)
(**)
(*This program is distributed in the hope that it will be useful,*)
(*but WITHOUT ANY WARRANTY; without even the implied warranty of*)
(*MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See *)
(*COPYING for more details.*)
(**)
(*You should have received a copy of the GNU General Public License*)
(*along with this program. If not, see http://www.gnu.org/licenses.*)


(* ::Subsection::Closed:: *)
(*Version information*)


(* ::Text:: *)
(*27/06/2012*)
(*1.0*)


(* ::Subsection::Closed:: *)
(*Changelog*)


(* ::Text:: *)
(*Version 1.0: Basic database logging functionality, minor bug fixes to follow.*)


(* ::Section:: *)
(*Public*)


BeginPackage["DBLogging`"];


(* ::Subsection::Closed:: *)
(*Result fetching*)


GetResult;


(* ::Subsection::Closed:: *)
(*Result caching*)


CacheResult;


(* ::Section:: *)
(*Private*)


Begin["`Private`"];


Needs["SQLite`"];


(* ::Subsection::Closed:: *)
(*Database parameters*)


tableName="data";
databaseName="data.sqlite";
columnNames={"algorithm","channelType","sampleComplexity","n","m","snrdb","pf","pd"};
columnTypes={"TEXT","TEXT","NUMERIC","INTEGER","NUMERIC","INTEGER","NUMERIC","NUMERIC"};


(* ::Subsection::Closed:: *)
(*Result fetching*)


GetResult::usage="GetResult[algorithm, channelType, M, \[Gamma], Pf, n, m] fetches the specified record from the database.";
GetResult[algorithm_?StringQ,channelType_?StringQ,M_?NumericQ,\[Gamma]_?NumericQ,Pf_?NumericQ,n_?IntegerQ,m_?NumericQ]:=Module[{db,result},
	If[FileExistsQ[databaseName],
		db = SQLiteOpenDatabase[databaseName];
		result = SQLiteLookupRecord[db,tableName,columnNames,{algorithm,channelType,M,n,m,10Log[10,\[Gamma]]//Round,Pf//N,Null}];
		SQLiteCloseDatabase[db];,
		result = Undefined;
	];
	If[result == {}//TrueQ,
		Null,
		result[[1]][[1]]
	]
]


(* ::Subsection::Closed:: *)
(*Result caching*)


CacheResult::usage="CacheResult[algorithm, channelType, M, \[Gamma], Pf, n, m, result] caches the specified record in the database.";
CacheResult[algorithm_?StringQ,channelType_?StringQ,M_?NumericQ,\[Gamma]_?NumericQ,Pf_?NumericQ,n_?IntegerQ,m_?NumericQ,result_?NumericQ]:=Module[{db},
	If[!FileExistsQ[databaseName],
		db = SQLiteOpenDatabase[databaseName];
		SQLiteCreateTable[db, tableName, columnNames, columnTypes],
		db = SQLiteOpenDatabase[databaseName];
	];
	SQLiteInsertRecord[db,tableName,columnNames,{algorithm,channelType,M,n,m,10Log[10,\[Gamma]]//Round,Pf//N,result,algorithm}];
	SQLiteCloseDatabase[db];
]


End[];


EndPackage[];
