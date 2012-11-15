(* ::Package:: *)

(* ::Title:: *)
(*Extra functions*)


(* ::Subsection::Closed:: *)
(*Copyright notice*)


(* ::Text:: *)
(*Mathematica function definitions to support other packages.*)
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


(* ::Subsection:: *)
(*Version information*)


(* ::Text:: *)
(*15/11/2012*)
(*1.01*)


(* ::Subsection:: *)
(*Changelog*)


(* ::Text:: *)
(*Version 1.01: Added SEC support, and removed SSC support.*)
(*Version 1.0: First working version.*)


(* ::Section:: *)
(*Public*)


BeginPackage["Extras`"]; 


ProcessSNR;


ProcessDiversityType;


(* ::Section:: *)
(*Private*)


Begin["`Private`"];


ProcessSNR::usage="ProcessSNR[\[Gamma], diversityType] converts lists of SNR values to averages or maxima, depending on the specified diversity type.";
ProcessSNR[\[Gamma]_,diversityType_:"SLC"]:=Which[
	diversityType == "None",
		If[ListQ[\[Gamma]], Undefined, \[Gamma]],
	diversityType == "MRC" || diversityType == "EGC" || diversityType == "SEC" || diversityType == "SLC",
		Mean[Flatten[{\[Gamma]}]],
	diversityType == "SC",
		Max[Flatten[{\[Gamma]}]],
	diversityType == "SLS",
		\[Gamma],
	True,
		Undefined
]


ProcessDiversityType::usage="ProcessDiversityType[x] either returns the list {\"SEC\", \[Gamma]t} or {diversityType, Null}.";
ProcessDiversityType[diversityType_]:=If[ListQ[diversityType], {diversityType[[1]], diversityType[[2]]}, {diversityType, Null}, Undefined]


End[];


EndPackage[];
