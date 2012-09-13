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


(* ::Subsection::Closed:: *)
(*Version information*)


(* ::Text:: *)
(*05/09/2012*)
(*1.11*)


(* ::Subsection::Closed:: *)
(*Changelog*)


(* ::Text:: *)
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


Needs["QFunction`"];


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
AWGNProbabilityOfFalseAlarm[M_,\[Lambda]_,n_,OptionsPattern[]]:=Module[{RelevantOptions, diversityType = OptionValue[DiversityType], method = OptionValue[Method]},
	RelevantOptions[target_]:=FilterRules[Table[#[[i]]->OptionValue[#[[i]]],{i,Length[#]}]&[Options[AWGNProbabilityOfFalseAlarm][[All,1]]],Options[target][[All,1]]];
	Which[
		method == "Approximate",
			Which[
				!ListQ[diversityType] && diversityType == "None",
					Q[(\[Lambda] - M) / Sqrt[2M]],
				!ListQ[diversityType] && diversityType == "MRC",
					Q[(\[Lambda] - M) / Sqrt[2M]],
				!ListQ[diversityType] && diversityType == "EGC",
					Q[(\[Lambda] - M) / Sqrt[2M]],
				!ListQ[diversityType] && diversityType == "SC",
					Q[(\[Lambda] - M) / Sqrt[2M]],
				ListQ[diversityType] && diversityType[[1]] == "SSC",
					Q[(\[Lambda] - M) / Sqrt[2M]],
				!ListQ[diversityType] && diversityType == "SLC",
					Q[(\[Lambda] - M n) / Sqrt[2M n]],
				!ListQ[diversityType] && diversityType == "SLS",
					1 - (1 - Q[(\[Lambda] - M) / Sqrt[2M]])^n,
				True,
					Undefined
			],
		method == "Exact",
			Which[
				!ListQ[diversityType] && diversityType == "None",
					GammaRegularized[M / 2, \[Lambda] / 2],
				!ListQ[diversityType] && diversityType == "MRC",
					GammaRegularized[M / 2, \[Lambda] / 2],
				!ListQ[diversityType] && diversityType == "EGC",
					GammaRegularized[M / 2, \[Lambda] / 2],
				!ListQ[diversityType] && diversityType == "SC",
					GammaRegularized[M / 2, \[Lambda] / 2],
				ListQ[diversityType] && diversityType[[1]] == "SSC",
					GammaRegularized[M / 2, \[Lambda] / 2],
				!ListQ[diversityType] && diversityType == "SLC",
					GammaRegularized[M n / 2, \[Lambda] / 2],
				!ListQ[diversityType] && diversityType == "SLS",
					1 - (1 - GammaRegularized[M / 2, \[Lambda] / 2])^n,
				True,
					Undefined
			],
		True,
			Undefined
	]
]


(* ::Subsubsection::Closed:: *)
(*Probability of detection*)


Options[AWGNProbabilityOfDetection]={Method->"Approximate", LowSNR->True, DiversityType->"SLC"}
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
DiversityType->\"SC\"
DiversityType->\"SSC\"
DiversityType->\"SLC\"
DiversityType->\"SLS\"

By default, DiversityType->"<>ToString[DiversityType/.Options[AWGNProbabilityOfDetection]]<>" if n is specified and DiversityType->\"None\" otherwise.";
AWGNProbabilityOfDetection[M_,\[Gamma]_,\[Lambda]_,OptionsPattern[]]:=Module[{n = 1, RelevantOptions},
	RelevantOptions[target_]:=FilterRules[Table[#[[i]]->OptionValue[#[[i]]],{i,Length[#]}]&[Options[AWGNProbabilityOfDetection][[All,1]]],Options[target][[All,1]]];
	AWGNProbabilityOfDetection[M,\[Gamma],\[Lambda],n,#/.(DiversityType/.#)->"None"&[RelevantOptions[AWGNProbabilityOfDetection]]]
]
AWGNProbabilityOfDetection[M_,\[Gamma]_,\[Lambda]_,n_,OptionsPattern[]]:=Module[{RelevantOptions, diversityType = OptionValue[DiversityType], method = OptionValue[Method], \[Gamma]t},
	RelevantOptions[target_]:=FilterRules[Table[#[[i]]->OptionValue[#[[i]]],{i,Length[#]}]&[Options[AWGNProbabilityOfDetection][[All,1]]],Options[target][[All,1]]];
	Which[
		method == "Approximate",
			Which[
				!ListQ[diversityType] && (diversityType == "None") && !ListQ[\[Gamma]],
					If[OptionValue[LowSNR],
						Q[(\[Lambda] - M (1 + \[Gamma])) / Sqrt[2M]],
						Q[(\[Lambda] - M (1 + \[Gamma])) / Sqrt[2M (1 + 2 \[Gamma])]]
					],
				!ListQ[diversityType] && diversityType == "MRC",
					Which[
						ListQ[\[Gamma]],
							If[OptionValue[LowSNR],
								Q[(\[Lambda] - M (1 + Total[\[Gamma]])) / Sqrt[2M]],
								Q[(\[Lambda] - M (1 + Total[\[Gamma]])) / Sqrt[2M (1 + 2 Total[\[Gamma]])]]
							],
						!ListQ[\[Gamma]],
							If[OptionValue[LowSNR],
								Q[(\[Lambda] - M (1 + n \[Gamma])) / Sqrt[2M]],
								Q[(\[Lambda] - M (1 + n \[Gamma])) / Sqrt[2M (1 + 2 n \[Gamma])]]
							],
						True,
							Undefined
					],
				!ListQ[diversityType] && diversityType == "EGC",
					Undefined,
				!ListQ[diversityType] && diversityType == "SC",
					Which[
						ListQ[\[Gamma]],
							If[OptionValue[LowSNR],
								Q[(\[Lambda] - M (1 + Max[\[Gamma]])) / Sqrt[2M]],
								Q[(\[Lambda] - M (1 + Max[\[Gamma]])) / Sqrt[2M (1 + 2 Max[\[Gamma]])]]
							],
						!ListQ[\[Gamma]],
							If[OptionValue[LowSNR],
								Q[(\[Lambda] - M (1 + \[Gamma])) / Sqrt[2M]],
								Q[(\[Lambda] - M (1 + \[Gamma])) / Sqrt[2M (1 + 2 \[Gamma])]]
							],
						True,
							Undefined
					],
				ListQ[diversityType] && (diversityType[[1]] == "SSC") && ListQ[\[Gamma]] && (Length[\[Gamma]] == 2),
					\[Gamma]t = diversityType[[2]];
					Which[
						\[Gamma][[1]] < \[Gamma]t,
							If[OptionValue[LowSNR],
								Q[(\[Lambda] - M (1 + \[Gamma][[1]])) / Sqrt[2M]],
								Q[(\[Lambda] - M (1 + \[Gamma][[1]])) / Sqrt[2M (1 + 2 \[Gamma][[1]])]]
							],
						\[Gamma][[1]] >= \[Gamma]t,
							If[OptionValue[LowSNR],
								Q[(\[Lambda] - M (1 + \[Gamma][[2]])) / Sqrt[2M]],
								Q[(\[Lambda] - M (1 + \[Gamma][[2]])) / Sqrt[2M (1 + 2 \[Gamma][[2]])]]
							],
						True,
							Undefined
					],
				!ListQ[diversityType] && diversityType == "SLC",
					Which[
						ListQ[\[Gamma]],
							If[OptionValue[LowSNR],
								Q[(\[Lambda] - M (n + Total[\[Gamma]])) / Sqrt[2M n]],
								Q[(\[Lambda] - M (n + Total[\[Gamma]])) / Sqrt[2M (n + 2 Total[\[Gamma]])]]
							],
						!ListQ[\[Gamma]],
							If[OptionValue[LowSNR],
								Q[(\[Lambda] - M n (1 + \[Gamma])) / Sqrt[2M n]],
								Q[(\[Lambda] - M n (1 + \[Gamma])) / Sqrt[2M n (1 + 2\[Gamma])]]
							],
						True,
							Undefined
					],
				!ListQ[diversityType] && diversityType == "SLS",
					Which[
						ListQ[\[Gamma]],
							If[OptionValue[LowSNR],
								1 - Product[1 - Q[(\[Lambda] - M (1 + \[Gamma][[i]])) / Sqrt[2M]],{i,n}],
								1 - Product[1 - Q[(\[Lambda] - M (1 + \[Gamma][[i]])) / Sqrt[2M (1 + 2\[Gamma][[i]])]],{i,n}]
							],
						!ListQ[\[Gamma]],
							If[OptionValue[LowSNR],
								1 - (1 - Q[(\[Lambda] - M (1 + \[Gamma])) / Sqrt[2M]])^n,
								1 - (1 - Q[(\[Lambda] - M (1 + \[Gamma])) / Sqrt[2M (1 + 2\[Gamma])]])^n
							],
						True,
							Undefined
					],
				True,
					Undefined
			],
		method == "Exact",
			Which[
				!ListQ[diversityType] && diversityType == "None" && !ListQ[\[Gamma]],
					MarcumQ[M / 2, Sqrt[M \[Gamma]], Sqrt[\[Lambda]]],
				!ListQ[diversityType] && diversityType == "MRC",
					Which[
						ListQ[\[Gamma]],
							MarcumQ[M / 2, Sqrt[M Total[\[Gamma]]], Sqrt[\[Lambda]]],
						!ListQ[\[Gamma]],
							MarcumQ[M / 2, Sqrt[M \[Gamma]], Sqrt[\[Lambda]]],
						True,
							Undefined
					],
				!ListQ[diversityType] && diversityType == "EGC",
					Undefined,
				!ListQ[diversityType] && diversityType == "SC",
					Which[
						ListQ[\[Gamma]],
							MarcumQ[M / 2, Sqrt[M Max[\[Gamma]]], Sqrt[\[Lambda]]],
						!ListQ[\[Gamma]],
							MarcumQ[M / 2, Sqrt[M \[Gamma]], Sqrt[\[Lambda]]],
						True,
							Undefined
					],
				ListQ[diversityType] && (diversityType[[1]] == "SSC") && ListQ[\[Gamma]] && (Length[\[Gamma]] == 2),
					\[Gamma]t = diversityType[[2]];
					Which[
						\[Gamma][[1]] < \[Gamma]t,
							MarcumQ[M / 2, Sqrt[M \[Gamma][[1]]], Sqrt[\[Lambda]]],
						\[Gamma][[1]] >= \[Gamma]t,
							MarcumQ[M / 2, Sqrt[M \[Gamma][[2]]], Sqrt[\[Lambda]]],
						True,
							Undefined
					],
				!ListQ[diversityType] && diversityType == "SLC",
					Which[
						ListQ[\[Gamma]],
							MarcumQ[M n / 2, Sqrt[M Total[\[Gamma]]], Sqrt[\[Lambda]]],
						!ListQ[\[Gamma]],
							MarcumQ[M n / 2, Sqrt[M n \[Gamma]], Sqrt[\[Lambda]]],
						True,
							Undefined
					],
				!ListQ[diversityType] && diversityType == "SLS",
					Which[
						ListQ[\[Gamma]],
							1 - Product[1 - MarcumQ[M / 2, Sqrt[M \[Gamma][[i]]], Sqrt[\[Lambda]]],{i,n}],
						!ListQ[\[Gamma]],
							1 - (1 - MarcumQ[M / 2, Sqrt[M \[Gamma]], Sqrt[\[Lambda]]])^n,
						True,
							Undefined
					],
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
\[Lambda][M_,Pf_,n_,OptionsPattern[]]:=Module[{method = OptionValue[Method], diversityType = OptionValue[DiversityType]},
	Which[
		!ListQ[diversityType] && diversityType == "None",
			Which[
				method == "Exact",
					2 InverseGammaRegularized[M / 2, Pf],
				method == "Approximate",
					Sqrt[2M] InverseQ[Pf] + M,
				True,
				Undefined
			],
		!ListQ[diversityType] && diversityType == "MRC",
			Which[
				method == "Exact",
					2 InverseGammaRegularized[M / 2, Pf],
				method == "Approximate",
					Sqrt[2M] InverseQ[Pf] + M,
				True,
				Undefined
			],
		!ListQ[diversityType] && diversityType == "EGC",
			Which[
				method == "Exact",
					2 InverseGammaRegularized[M / 2, Pf],
				method == "Approximate",
					Sqrt[2M] InverseQ[Pf] + M,
				True,
				Undefined
			],
		!ListQ[diversityType] && diversityType == "SC",
			Which[
				method == "Exact",
					2 InverseGammaRegularized[M / 2, Pf],
				method == "Approximate",
					Sqrt[2M] InverseQ[Pf] + M,
				True,
				Undefined
			],
		ListQ[diversityType] && diversityType[[1]] == "SSC",
			Which[
				method == "Exact",
					2 InverseGammaRegularized[M / 2, Pf],
				method == "Approximate",
					Sqrt[2M] InverseQ[Pf] + M,
				True,
				Undefined
			],
		!ListQ[diversityType] && diversityType == "SLC",
			Which[
				method == "Exact",
					2 InverseGammaRegularized[M n / 2, Pf],
				method == "Approximate",
					Sqrt[2M n] InverseQ[Pf] + M n,
				True,
				Undefined
			],
		!ListQ[diversityType] && diversityType == "SLS",
			Which[
				method == "Exact",
					2 InverseGammaRegularized[M / 2, 1 - (1 - Pf)^(1 / n)],
				method == "Approximate",
					Sqrt[2M] InverseQ[1 - (1 - Pf)^(1 / n)] + M,
				True,
				Undefined
			],
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
