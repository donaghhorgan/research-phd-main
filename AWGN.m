(* ::Package:: *)

(* ::Title:: *)
(*AWGN channel functions*)


(* ::Subsection::Closed:: *)
(*Copyright notice*)


(* ::Text:: *)
(*Mathematica function definitions for cooperative energy detection in AWGN channels.*)
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
(*06/11/2012*)
(*1.2*)


(* ::Subsection:: *)
(*Changelog*)


(* ::Text:: *)
(*Version 1.2: Recoded ProbabilityOfFalseAlarm, ProbabilityOfDetection and \[Lambda] functions, so they are easier to read.*)
(*Version 1.12: Added EGC support.*)
(*Version 1.11: Added protection for symbols.*)
(*Version 1.1: Added diversity types to functions.*)
(*Version 1.0: First working version, minor bug fixes to follow.*)


(* ::Section:: *)
(*Public*)


Protect[LowSNR, DiversityType];


BeginPackage["AWGN`"];


(* ::Subsection::Closed:: *)
(*PDF of the recieved energy*)


AWGNPDF;


(* ::Subsection:: *)
(*Decision probabilities*)


(* ::Subsubsection::Closed:: *)
(*Probability of false alarm*)


AWGNProbabilityOfFalseAlarm;


(* ::Subsubsection::Closed:: *)
(*Probability of detection*)


LowSNRErrorBound;


AWGNProbabilityOfDetection;


(* ::Subsubsection::Closed:: *)
(*Threshold*)


\[Lambda];


(* ::Subsection::Closed:: *)
(*Sample complexity*)


AWGNSampleComplexity;


(* ::Section:: *)
(*Private*)


Begin["`Private`"];


<<QFunction`;
<<Extras`;


(* ::Subsection::Closed:: *)
(*PDF of the recieved energy*)


Options[AWGNPDF] = {Method->"Exact"};
AWGNPDF::usage="AWGNPDF[M, \!\(\*OverscriptBox[\(\[Gamma]\), \(_\)]\), x] evaluates the probability density function of the recieved energy at a single energy detector operating on an AWGN channel at x.
AWGNPDF[M, \!\(\*OverscriptBox[\(\[Gamma]\), \(_\)]\), x, n] evaluates the probability density function of the recieved energy at the fusion center of a cooperative network operating on an AWGN fading channel at x.

The following methods can be given:

Method\[Rule]\"Approximate\"
Method\[Rule]\"Exact\"

By default, Method\[Rule]\""<>ToString[Method/.Options[AWGNPDF]]<>"\".";
AWGNPDF[M_,\[Gamma]_,x_,OptionsPattern[]]:=Module[{n = 1},AWGNPDF[M,\[Gamma],x,n,Method->OptionValue[Method]]]
AWGNPDF[M_,\[Gamma]_,x_,n_,OptionsPattern[]]:=Switch[OptionValue[Method],
	"Exact",
	PDF[NoncentralChiSquareDistribution[M n, M n \[Gamma]], x],
	"Approximate",
	PDF[NormalDistribution[M n(1+\[Gamma]),Sqrt[2M n(1+2\[Gamma])]],x],
	_,
	AWGNPDF[M,\[Gamma],x,n,Method->"Exact"]
]


(* ::Subsection:: *)
(*Decision probabilities*)


(* ::Subsubsection::Closed:: *)
(*Probability of false alarm*)


Options[AWGNProbabilityOfFalseAlarm]={Method->"Approximate", DiversityType->"SLC"};
AWGNProbabilityOfFalseAlarm::usage="AWGNProbabilityOfFalseAlarm[M, \[Lambda]] calculates the probability of false alarm for a single energy detector operating on an AWGN channel.
AWGNProbabilityOfFalseAlarm[M, \[Lambda], n] calculates the probability of false alarm for a cooperative network operating on an AWGN channel.

The following methods may be specified:

Method\[Rule]\"Approximate\"
Method\[Rule]\"Exact\"

By default, Method\[Rule]\""<>ToString[Method/.Options[AWGNProbabilityOfFalseAlarm]]<>"\".

In addition, the following diversity reception schemes may be specified:

DiversityType->\"None\"
DiversityType->\"MRC\"
DiversityType->\"EGC\"
DiversityType->\"SC\"
DiversityType->\"SSC\"
DiversityType->\"SLC\"
DiversityType->\"SLS\"

By default, DiversityType->"<>ToString[DiversityType/.Options[AWGNProbabilityOfFalseAlarm]]<>" if n is specified and DiversityType->\"None\" otherwise.";
AWGNProbabilityOfFalseAlarm[M_,\[Lambda]_,OptionsPattern[]]:=Module[{n = 1, RelevantOptions},
	RelevantOptions[target_]:=FilterRules[Table[#[[i]]->OptionValue[#[[i]]],{i,Length[#]}]&[Options[AWGNProbabilityOfFalseAlarm][[All,1]]],Options[target][[All,1]]];
	AWGNProbabilityOfFalseAlarm[M,\[Lambda],n,#/.(DiversityType/.#)->"None"&[RelevantOptions[AWGNProbabilityOfFalseAlarm]]]
]
AWGNProbabilityOfFalseAlarm[M_,\[Lambda]_,n_,OptionsPattern[]]:=Module[{diversityType = OptionValue[DiversityType], \[Gamma]t, method = OptionValue[Method], g},
	(* Handle both lists and scalar values for diversityType *)
	{diversityType, \[Gamma]t} = ProcessDiversityType[diversityType];

	(* Check for invalid combinations of inputs *)
	If[diversityType == "None" && n > 1, Return[Undefined]];

	g[a_] := Which[
		method == "Approximate",
			Q[(\[Lambda] - M a) / Sqrt[2M a]],
		method == "Exact",
			GammaRegularized[M a / 2, \[Lambda] / 2],
		True,
			Undefined
	];

	Which[
		diversityType == "None" || diversityType == "MRC" || diversityType == "EGC" || diversityType == "SC" || diversityType == "SSC",
			g[1],
		diversityType == "SLC",
			g[n],
		diversityType == "SLS",
			1 - (1 - g[1])^n,
		True,
			Undefined
	]
]


(* ::Subsubsection::Closed:: *)
(*Probability of detection*)


Options[LowSNRErrorBound] = {DiversityType->"SLC"};
LowSNRErrorBound::usage="LowSNRErrorBound[M, \[Lambda], n] gives the upper bound for the error due to the low SNR approximation.";
LowSNRErrorBound[M_,\[Lambda]_] := LowSNRErrorBound[M, \[Lambda]] = Module[{n = 1},
	LowSNRErrorBound[M, \[Lambda], n, DiversityType->"None"]
]
LowSNRErrorBound[M_,\[Lambda]_,n_,OptionsPattern[]] := Module[{diversityType = OptionValue[DiversityType], \[Gamma]t, g, \[Epsilon], \[Epsilon]max = 10},
	(* Handle both lists and scalar values for diversityType *)
	{diversityType, \[Gamma]t} = ProcessDiversityType[diversityType];

	(* Check for invalid combinations of inputs *)
	If[diversityType == "None" && n > 1, Return[Undefined]];

	g[\[Epsilon]_?NumericQ] := Which[
		diversityType == "None" || diversityType == "SLC",
			Abs[AWGNProbabilityOfDetection[M, \[Epsilon], \[Lambda], n, DiversityType->diversityType, LowSNR->False] - AWGNProbabilityOfDetection[M, \[Epsilon], \[Lambda], n, DiversityType->diversityType, LowSNR->True]],
		diversityType == "MRC" || diversityType == "EGC" || diversityType == "SC" || diversityType == "SSC",
			LowSNRErrorBound[M, \[Lambda]],
		diversityType == "SLS",
			Abs[(1 / 2 - LowSNRErrorBound[M, \[Lambda]])^n - (1 / 2)^n],
		True,
			Undefined
	];

	NMaximize[{g[\[Epsilon]], 0 <= \[Epsilon] <= \[Epsilon]max}, {\[Epsilon], 0, \[Epsilon]max}][[1]]
]


Options[AWGNProbabilityOfDetection]={Method->"Approximate", LowSNR->True, DiversityType->"SLC"};
AWGNProbabilityOfDetection::usage="AWGNProbabilityOfDetection[M, \[Gamma], \[Lambda]] calculates the approximate probability of detection for a single energy detector operating on an AWGN channel.
AWGNProbabilityOfDetection[M, \[Gamma], \[Lambda], n] calculates the approximate probability of detection for a cooperative network operating on an AWGN channel.

The following methods may be specified:

Method\[Rule]\"Approximate\"
Method\[Rule]\"Exact\"

By default, Method\[Rule]\""<>ToString[Method/.Options[AWGNProbabilityOfDetection]]<>"\".

If the approximate method is specified, the LowSNR option can be used to specify whether to use a low signal to noise ratio approximation. By default, LowSNR\[Rule]"<>ToString[LowSNR/.Options[AWGNProbabilityOfDetection]]<>".

In addition, the following diversity reception schemes may be specified:

DiversityType->\"None\"
DiversityType->\"MRC\"
DiversityType->\"EGC\"
DiversityType->\"SC\"
DiversityType->\"SSC\"
DiversityType->\"SLC\"
DiversityType->\"SLS\"

By default, DiversityType->"<>ToString[DiversityType/.Options[AWGNProbabilityOfDetection]]<>" if n is specified and DiversityType->\"None\" otherwise.";
AWGNProbabilityOfDetection[M_,\[Gamma]_,\[Lambda]_,OptionsPattern[]]:=Module[{n = 1, RelevantOptions},
	RelevantOptions[target_]:=FilterRules[Table[#[[i]]->OptionValue[#[[i]]],{i,Length[#]}]&[Options[AWGNProbabilityOfDetection][[All,1]]],Options[target][[All,1]]];
	AWGNProbabilityOfDetection[M,\[Gamma],\[Lambda],n,#/.(DiversityType/.#)->"None"&[RelevantOptions[AWGNProbabilityOfDetection]]]
]
AWGNProbabilityOfDetection[M_,\[Gamma]_,\[Lambda]_,n_,OptionsPattern[]]:=Module[{RelevantOptions, diversityType = OptionValue[DiversityType], \[Gamma]t, \[Gamma]0, method = OptionValue[Method], g},
	(* Handle both lists and scalar values for diversityType *)
	{diversityType, \[Gamma]t} = ProcessDiversityType[diversityType];
	
	(* Convert lists of SNR values to averages or maxima, depending on the specified diversity type *)
	\[Gamma]0 = ProcessSNR[\[Gamma], diversityType];

	(* Check for invalid combinations of inputs *)
	If[diversityType == "None" && n > 1, Return[Undefined]];
	If[\[Gamma]0 == Undefined, Return[Undefined]];

	g[a_,b_] := Which[
		method == "Approximate",
			If[OptionValue[LowSNR],
				Q[(\[Lambda] - M (a + b \[Gamma]0)) / Sqrt[2M a]],
				Q[(\[Lambda] - M (a + b \[Gamma]0)) / Sqrt[2M (a + b 2 \[Gamma]0)]]
			],
		method == "Exact",
			MarcumQ[M a / 2, Sqrt[M b \[Gamma]0], Sqrt[\[Lambda]]],
		True,
			Undefined
	];

	Which[
		diversityType == "None" || diversityType == "SC",
			g[1, 1],
		diversityType == "MRC" || diversityType == "EGC",
			g[1, n],
		diversityType == "SSC" && ListQ[\[Gamma]] && (Length[\[Gamma]] == 2),
			(* No \[Gamma]0 here - this is a special case *)
			If[\[Gamma][[1]] >= \[Gamma]t,
				AWGNProbabilityOfDetection[M, \[Gamma][[1]], \[Lambda], DiversityType->"None", Method->OptionValue[Method], LowSNR->OptionValue[LowSNR]],
				AWGNProbabilityOfDetection[M, \[Gamma][[2]], \[Lambda], DiversityType->"None", Method->OptionValue[Method], LowSNR->OptionValue[LowSNR]]
			],
		diversityType == "SLC",
			g[n, n],
		diversityType == "SLS",
			Which[
				ListQ[\[Gamma]0],
					1 - Product[1 - AWGNProbabilityOfDetection[M, \[Gamma]0[[i]], \[Lambda], DiversityType->"None", Method->OptionValue[Method], LowSNR->OptionValue[LowSNR]],{i,n}],
				!ListQ[\[Gamma]0],
					1 - (1 - AWGNProbabilityOfDetection[M, \[Gamma]0, \[Lambda], DiversityType->"None", Method->OptionValue[Method], LowSNR->OptionValue[LowSNR]])^n,
				True,
					Undefined
			],
		True,
			Undefined
	]
]


(* ::Subsubsection::Closed:: *)
(*Threshold*)


Options[\[Lambda]]={Method->"Approximate", DiversityType->"SLC"};
\[Lambda]::usage="\[Lambda][M, Pf] calculates a threshold suitable for use in the calculation of the decision probabilities for a single energy detector.
\[Lambda][M, Pf, n] calculates a threshold suitable for use in the calculation of the fusion center decision probabilities when Nb = \[Infinity].

The following methods may be specified:

Method\[Rule]\"Approximate\"
Method\[Rule]\"Exact\"

By default, Method\[Rule]\""<>ToString[Method/.Options[\[Lambda]]]<>"\".

In addition, the following diversity reception schemes may be specified:

DiversityType->\"None\"
DiversityType->\"MRC\"
DiversityType->\"EGC\"
DiversityType->\"SC\"
DiversityType->\"SSC\"
DiversityType->\"SLC\"
DiversityType->\"SLS\"

By default, DiversityType->"<>ToString[DiversityType/.Options[\[Lambda]]]<>" if n is specified and DiversityType->\"None\" otherwise.";
\[Lambda][M_,Pf_,OptionsPattern[]]:=Module[{n = 1, RelevantOptions},
	RelevantOptions[target_]:=FilterRules[Table[#[[i]]->OptionValue[#[[i]]],{i,Length[#]}]&[Options[\[Lambda]][[All,1]]],Options[target][[All,1]]];
	\[Lambda][M,Pf,n,#/.(DiversityType/.#)->"None"&[RelevantOptions[\[Lambda]]]]
]
\[Lambda][M_,Pf_,n_,OptionsPattern[]]:=Module[{method = OptionValue[Method], diversityType = OptionValue[DiversityType], \[Gamma]t, g},
	(* Handle both lists and scalar values for diversityType *)
	{diversityType, \[Gamma]t} = ProcessDiversityType[diversityType];

	g[a_,b_] := Which[
		method == "Exact",
			2 InverseGammaRegularized[M a / 2, b],
		method == "Approximate",
			Sqrt[2M a] InverseQ[b] + M a,
		True,
			Undefined
	];

	Which[
		diversityType == "None" || diversityType == "MRC" || diversityType == "EGC" || diversityType == "SC" || diversityType == "SSC",
			g[1, Pf],
		diversityType == "SLC",
			g[n, Pf],
		diversityType == "SLS",
			g[1, 1 - (1 - Pf)^(1 / n)],
		True,
			Undefined
	]
]


(* ::Subsection::Closed:: *)
(*Sample complexity*)


AWGNSampleComplexity::usage="AWGNSampleComplexity[\[Gamma], Pf, Pd] calculates the approximate number of samples required for a single energy detector to operate with the specified decision probabilities at a given signal to noise ratio in an AWGN channel.
AWGNSampleComplexity[\[Gamma], Pf, Pd, n] calculates the approximate number of samples required for a cooperative network to operate with the specified decision probabilities at a given signal to noise ratio in an AWGN channel.";
AWGNSampleComplexity[\[Gamma]_,Pf_,Pd_,n_:1]:= (2 / n) * ((InverseQ[Pf] - InverseQ[Pd]) / \[Gamma])^2


End[]


EndPackage[];
