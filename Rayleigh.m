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
(*27/06/2012*)
(*1.0*)


(* ::Subsection::Closed:: *)
(*Changelog*)


(* ::Text:: *)
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
RayleighPDF::usage="RayleighPDF[\!\(\*OverscriptBox[\(\[Gamma]\), \(_\)]\), x] evaluates the probability density function of the instantaneous signal to noise ratio at a single energy detector operating on a Rayleigh fading channel at x.\nRayleighPDF[\!\(\*OverscriptBox[\(\[Gamma]\), \(_\)]\), x, n] evaluates the probability density function of the average instantaneous signal to noise ratio at the fusion center of a cooperative network operating on a Rayleigh fading channel at x.\n\nThe following methods can be given:\n\nMethod\[Rule]\"Approximate\"\nMethod\[Rule]\"Exact\"\n\nBy default, Method\[Rule]\"Exact\".";
RayleighPDF[\[Gamma]_,x_,OptionsPattern[]]:=Module[{n = 1},RayleighPDF[\[Gamma],x,n,Method->OptionValue[Method]]]
RayleighPDF[\[Gamma]_,x_,n_,OptionsPattern[]]:=Module[{m = 1},NakagamiPDF[\[Gamma],m,x,n,Method->OptionValue[Method]]]


(* ::Subsection::Closed:: *)
(*Detection probability*)


Options[NRayleighProbabilityOfDetection]=Options[NNakagamiProbabilityOfDetection];
NRayleighProbabilityOfDetection::usage="NRayleighProbabilityOfDetection[M, \[Gamma], \[Lambda]] calculates the probability of detection for a single energy detector operating on a Rayleigh fading channel.\nNRayleighProbabilityOfDetection[M, \[Gamma], \[Lambda], n] calculates the probability of detection for the fusion center of a cooperative network operating on a Rayleigh fading channel.\n\nThe following methods can be given:\n\nMethod\[Rule]\"Approximate\"\nMethod\[Rule]\"Exact\"\n\nBy default, Method\[Rule]\"Approximate\".\n\nFor a given method, an algorithm must be specified. For the approximate method, the following algorithms may be specified:\n\nAlgorithm\[Rule]\"Gaussian\"\nAlgorithm\[Rule]\"NGaussian\"\n\nBy default, Algorithm\[Rule]\"NGaussian\". If Algorithm\[Rule]\"NGaussian\", then the LowSNR option may also be specified. By default, LowSNR\[Rule]True.\n\nFor the exact method, the following algorithms may be specified:\n\nAlgorithm\[Rule]\"Annamalai\"\nAlgorithm\[Rule]\"Digham\"\nAlgorithm\[Rule]\"Herath\"\n\nBy default, Algorithm\[Rule]\"Annamalai\".\n\nIn addition, timing and database lookup/caching options may be (exclusively) specified. The timing option is specified by:\n\nTimed\[Rule]False\nMaxIterations\[Rule]1000\nMaxTime\[Rule]600\n\nwhere the above options are the defaults, if not specified. If Timed\[Rule]True, then a {Pd, time} list of values will be returned.\n\nIf timing is not used, then database lookup/caching may be enabled. This requires that the sqlite.m package be loaded. By default:\n\nDatabaseLookup\[Rule]False\nDatabaseCaching\[Rule]False\n\nand the data is stored in the database specified in sqlite.m.";
NRayleighProbabilityOfDetection[M_?NumericQ,\[Gamma]_?NumericQ,\[Lambda]_,OptionsPattern[]]:=Module[{n = 1},NRayleighProbabilityOfDetection[M,\[Gamma],\[Lambda],n,Method->OptionValue[Method], Algorithm->OptionValue[Algorithm], LowSNR->OptionValue[LowSNR], Timed->OptionValue[Timed], MaxIterations->OptionValue[MaxIterations], MaxTime->OptionValue[MaxTime], DatabaseLookup->OptionValue[DatabaseLookup], DatabaseCaching->OptionValue[DatabaseCaching]]]
NRayleighProbabilityOfDetection[M_?NumericQ,\[Gamma]_?NumericQ,\[Lambda]_,n_?IntegerQ,OptionsPattern[]]:=Module[{m = 1},NNakagamiProbabilityOfDetection[M,\[Gamma],\[Lambda],m,n,Method->OptionValue[Method], Algorithm->OptionValue[Algorithm], LowSNR->OptionValue[LowSNR], Timed->OptionValue[Timed], MaxIterations->OptionValue[MaxIterations], MaxTime->OptionValue[MaxTime], DatabaseLookup->OptionValue[DatabaseLookup], DatabaseCaching->OptionValue[DatabaseCaching]]]


(* ::Subsection::Closed:: *)
(*Sample complexity*)


Options[NRayleighSampleComplexity]=Options[NNakagamiSampleComplexity];
NRayleighSampleComplexity::usage="NRayleighSampleComplexity[\[Gamma], Pf, Pd] calculates the sample complexity for a single energy detector operating on a Rayleigh fading channel.\nNRayleighSampleComplexity[\[Gamma], Pf, Pd, n] calculates the sample complexity for a cooperative network operating on a Rayleigh fading channel.\n\nThe following methods can be given:\n\nMethod\[Rule]\"Approximate\"\nMethod\[Rule]\"Exact\"\n\nBy default, Method\[Rule]\"Approximate\".\n\nIf Method\[Rule]\"Approximate\", the LowSNR option may be specified. By default, LowSNR\[Rule]True.\n\nNumerical tolerance can be specified using the Tolerance option. By default, Tolerance\[Rule]1.*^-6.";
NRayleighSampleComplexity[\[Gamma]_?NumericQ,Pf_?NumericQ,Pd_?NumericQ,OptionsPattern[]]:=Module[{n = 1},NRayleighSampleComplexity[\[Gamma],Pf,Pd,n,Method->OptionValue[Method], LowSNR->OptionValue[LowSNR],Tolerance->OptionValue[Tolerance]]]
NRayleighSampleComplexity[\[Gamma]_?NumericQ,Pf_?NumericQ,Pd_?NumericQ,n_?IntegerQ,OptionsPattern[]]:=Module[{m = 1},NNakagamiSampleComplexity[\[Gamma],Pf,Pd,m,n,Method->OptionValue[Method], LowSNR->OptionValue[LowSNR],Tolerance->OptionValue[Tolerance]]]


End[];


EndPackage[];
