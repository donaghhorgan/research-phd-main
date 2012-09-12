(* ::Package:: *)

(* ::Title:: *)
(*Rayleigh channel functions*)


(* ::Subsection::Closed:: *)
(*Copyright notice*)


(* ::Text:: *)
(*Mathematica function definitions for cooperative energy detection in Rayleigh channels.*)
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
(*02/08/2012*)
(*1.11*)


(* ::Subsection::Closed:: *)
(*Changelog*)


(* ::Text:: *)
(*Version 1.11: Moved database logging functions to the Network package.*)
(*Version 1.1: Introduced RelevantOptions function and changed function definitions, so that child options are inherited from parents.*)
(*Version 1.0: First working version, minor bug fixes to follow.*)


(* ::Section:: *)
(*Public*)


BeginPackage["Rayleigh`"];


(* ::Subsection::Closed:: *)
(*PDF of the signal to noise ratio*)


RayleighPDF;


(* ::Subsection::Closed:: *)
(*Detection probability*)


NRayleighProbabilityOfDetection;


(* ::Subsection::Closed:: *)
(*Sample complexity*)


NRayleighSampleComplexity;


(* ::Section:: *)
(*Private*)


Begin["`Private`"];


Needs["Nakagami`"];


(* ::Subsection::Closed:: *)
(*PDF of the signal to noise ratio*)


Options[RayleighPDF] = Options[NakagamiPDF];
RayleighPDF::usage="";
RayleighPDF[\[Gamma]_,x_,OptionsPattern[]]:=Module[{n = 1},RayleighPDF[\[Gamma],x,n,Method->OptionValue[Method]]]
RayleighPDF[\[Gamma]_,x_,n_,OptionsPattern[]]:=Module[{m = 1},NakagamiPDF[\[Gamma],m,x,n,Method->OptionValue[Method]]]


(* ::Subsection::Closed:: *)
(*Detection probability*)


Options[NRayleighProbabilityOfDetection]=Options[NNakagamiProbabilityOfDetection];
NRayleighProbabilityOfDetection::usage="";
NRayleighProbabilityOfDetection[M_?NumericQ,\[Gamma]_?NumericQ,\[Lambda]_,OptionsPattern[]]:=Module[{RelevantOptions, n = 1},
	RelevantOptions[target_]:=FilterRules[Table[#[[i]]->OptionValue[#[[i]]],{i,Length[#]}]&[Options[NRayleighProbabilityOfDetection][[All,1]]],Options[target][[All,1]]];
	NRayleighProbabilityOfDetection[M,\[Gamma],\[Lambda],n,RelevantOptions[NRayleighProbabilityOfDetection]]
]
NRayleighProbabilityOfDetection[M_?NumericQ,\[Gamma]_?NumericQ,\[Lambda]_,n_?IntegerQ,OptionsPattern[]]:=Module[{RelevantOptions, m = 1},
	RelevantOptions[target_]:=FilterRules[Table[#[[i]]->OptionValue[#[[i]]],{i,Length[#]}]&[Options[NRayleighProbabilityOfDetection][[All,1]]],Options[target][[All,1]]];
	NNakagamiProbabilityOfDetection[M,\[Gamma],\[Lambda],m,n,RelevantOptions[NNakagamiProbabilityOfDetection]]
]


(* ::Subsection:: *)
(*Sample complexity*)


Options[NRayleighSampleComplexity]=Options[NNakagamiSampleComplexity];
NRayleighSampleComplexity::usage="";
NRayleighSampleComplexity[\[Gamma]_?NumericQ,Pf_?NumericQ,Pd_?NumericQ,OptionsPattern[]]:=Module[{RelevantOptions, n = 1},
	RelevantOptions[target_]:=FilterRules[Table[#[[i]]->OptionValue[#[[i]]],{i,Length[#]}]&[Options[NRayleighSampleComplexity][[All,1]]],Options[target][[All,1]]];
	NRayleighSampleComplexity[\[Gamma],Pf,Pd,n,RelevantOptions[NRayleighSampleComplexity]]
]
NRayleighSampleComplexity[\[Gamma]_?NumericQ,Pf_?NumericQ,Pd_?NumericQ,n_?IntegerQ,OptionsPattern[]]:=Module[{RelevantOptions, m = 1},
	RelevantOptions[target_]:=FilterRules[Table[#[[i]]->OptionValue[#[[i]]],{i,Length[#]}]&[Options[NRayleighSampleComplexity][[All,1]]],Options[target][[All,1]]];
	NNakagamiSampleComplexity[\[Gamma],Pf,Pd,m,n,RelevantOptions[NNakagamiSampleComplexity]]
]


End[];


EndPackage[];
