(* ::Package:: *)

(* ::Title:: *)
(*Nakagami channel functions*)


(* ::Subsection::Closed:: *)
(*Copyright notice*)


(* ::Text:: *)
(*Mathematica function definitions for cooperative energy detection in Nakagami channels.*)
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
(*13/09/2012*)
(*1.32*)


(* ::Subsection::Closed:: *)
(*Changelog*)


(* ::Text:: *)
(*Version 1.32: Added SC and SSC diversity to the IntegerMN method.*)
(*Version 1.31: Filled out all known diversity types for each exact method.*)
(*Version 1.3: Added MRC, EGC, SLC, SSC, SC and SLS diversity cases to all methods.*)
(*Version 1.2: Split Horgan's approximation into separate IntegerMN and LargeMN functions, so that full functionality can be accessed through the NNakagamiProbabilityOfDetection interface.*)
(*Version 1.11: Moved database logging functions to the Network package.*)
(*Version 1.1: Introduced RelevantOptions function and changed function definitions, so that child options are inherited from parents. The Gaussian approximation method is now called Horgan's approximation to avoid confusion with the numerical Gaussian method.*)
(*Version 1.0: First working version, minor bug fixes to follow.*)


(* ::Section:: *)
(*Public*)


Protect[DiversityType, Algorithm, LowSNR, Timed, MaxTime];


BeginPackage["Nakagami`"]; 


(* ::Subsection::Closed:: *)
(*PDF of the signal to noise ratio*)


NakagamiPDF;


(* ::Subsection:: *)
(*Detection probability*)


(* ::Subsubsection::Closed:: *)
(*Main function*)


NNakagamiProbabilityOfDetection;


(* ::Subsubsection::Closed:: *)
(*Annamalai' s method*)


NAnnamalaiNakagamiProbabilityOfDetection;


(* ::Subsubsection::Closed:: *)
(*Digham' s method*)


NDighamNakagamiProbabilityOfDetection;


(* ::Subsubsection::Closed:: *)
(*Herath' s method*)


NHerathNakagamiProbabilityOfDetection;


(* ::Subsubsection::Closed:: *)
(*Sun' s method*)


NSunNakagamiProbabilityOfDetection;


(* ::Subsubsection::Closed:: *)
(*Integer mn method*)


IntegerMNNakagamiProbabilityOfDetection;


(* ::Subsubsection::Closed:: *)
(*Large mn method*)


LargeMNNakagamiProbabilityOfDetection;


(* ::Subsubsection::Closed:: *)
(*Numerical Gaussian method*)


NGaussianNakagamiProbabilityOfDetection;


(* ::Subsection::Closed:: *)
(*Sample complexity*)


NNakagamiSampleComplexity;


(* ::Section:: *)
(*Private*)


Begin["`Private`"];


<<Network`;
<<AWGN`;
<<ErfApprox`;
<<QFunction`;


(* ::Subsection::Closed:: *)
(*PDF of the signal to noise ratio*)


Options[NakagamiPDF] = {Method->"Exact",DiversityType->"SLC"};
NakagamiPDF::usage="NakagamiPDF[\!\(\*OverscriptBox[\(\[Gamma]\), \(_\)]\), m, x] evaluates the probability density function of the instantaneous signal to noise ratio at a single energy detector operating on a Nakagami-m fading channel at x.
NakagamiPDF[\!\(\*OverscriptBox[\(\[Gamma]\), \(_\)]\), m, x, n] evaluates the probability density function of the average instantaneous signal to noise ratio at the fusion center of a cooperative network operating on a Nakagami-m fading channel at x.

The following methods can be given:

Method\[Rule]\"Approximate\"
Method\[Rule]\"Exact\"

By default, Method\[Rule]\""<>ToString[Method/.Options[NakagamiPDF]]<>"\".

In addition, the receiver diversity type may be specified using the DiversityType option:

DiversityType->\"SLC\"

By default, DiversityType->\"SLC\".";
NakagamiPDF[\[Gamma]_,m_,x_,OptionsPattern[]]:=Module[{n = 1},NakagamiPDF[\[Gamma],m,x,n,Method->OptionValue[Method],DiversityType->OptionValue[DiversityType]]]
NakagamiPDF[\[Gamma]_,m_,x_,n_,OptionsPattern[]]:=Module[{method=OptionValue[Method],diversityType=OptionValue[DiversityType]},
	Which[
		method == "Exact",
			Which[
				!ListQ[diversityType] && diversityType == "None",
					PDF[GammaDistribution[m, \[Gamma] / m], x],
				!ListQ[diversityType] && diversityType == "MRC",
					PDF[GammaDistribution[m n, \[Gamma] / m], x],
				!ListQ[diversityType] && diversityType == "EGC",
					Undefined,
				!ListQ[diversityType] && diversityType == "SC",
					(n / Gamma[m]) Sum[(-1)^(l) Binomial[n - 1, l] Sum[MultinomialCoefficient[l, k, m] (m / \[Gamma])^(m + k) x^(m + k - 1) Exp[-m (l + 1) x / \[Gamma]],{k, 0, l (m - 1)}],{l, 0, n - 1}],
				ListQ[diversityType] && diversityType[[1]] == "SSC",
					Module[{\[Gamma]t = diversityType[[2]]},
						Which[
							x < \[Gamma]t,
								(1 - GammaRegularized[m, m \[Gamma]t / \[Gamma]])PDF[GammaDistribution[m, \[Gamma] / m], x],
							x >= \[Gamma]t,
								(2 - GammaRegularized[m, m \[Gamma]t / \[Gamma]])PDF[GammaDistribution[m, \[Gamma] / m], x],
							True,
								Undefined
						]
					],
				!ListQ[diversityType] && diversityType == "SLC",
					PDF[GammaDistribution[m n, \[Gamma] / m], x],
				!ListQ[diversityType] && diversityType == "SLS",
					PDF[GammaDistribution[m, \[Gamma] / m], x],
				True,
					Undefined
			],
		method == "Approximate",
			Which[
				!ListQ[diversityType] && diversityType == "None",
					PDF[NormalDistribution[\[Gamma], Sqrt[\[Gamma]^2 / m]], x],
				!ListQ[diversityType] && diversityType == "MRC",
					PDF[NormalDistribution[n \[Gamma], Sqrt[n \[Gamma]^2 / m]], x],
				!ListQ[diversityType] && diversityType == "EGC",
					Undefined,
				!ListQ[diversityType] && diversityType == "SC",
					n PDF[NormalDistribution[\[Gamma], Sqrt[\[Gamma]^2 / m]], x] (CDF[NormalDistribution[2 m, 2 Sqrt[m]],2 m x / \[Gamma]]^(n - 1)),
				ListQ[diversityType] && diversityType[[1]] == "SSC",
					Module[{\[Gamma]t = diversityType[[2]]},
						Which[
							\[Gamma] < \[Gamma]t,
								(1 - GammaRegularized[m, m \[Gamma]t / \[Gamma]])PDF[NormalDistribution[\[Gamma], Sqrt[\[Gamma]^2 / m]], x],
							\[Gamma] >= \[Gamma]t,
								(2 - GammaRegularized[m, m \[Gamma]t / \[Gamma]])PDF[NormalDistribution[\[Gamma], Sqrt[\[Gamma]^2 / m]], x],
							True,
								Undefined
						]
					],
				!ListQ[diversityType] && diversityType == "SLC",
					PDF[NormalDistribution[n \[Gamma], Sqrt[n \[Gamma]^2 / m]], x],
				!ListQ[diversityType] && diversityType == "SLS",
					PDF[NormalDistribution[\[Gamma], Sqrt[\[Gamma]^2 / m]], x],
				True,
					Undefined
			],
		True,
			Undefined
	]
]


(* ::Subsection:: *)
(*Detection probability*)


(* ::Subsubsection::Closed:: *)
(*Main function*)


Options[NNakagamiProbabilityOfDetection]={Method->OptionValue[ProbabilityOfDetection,Method],Algorithm->OptionValue[ProbabilityOfDetection,Algorithm],LowSNR->OptionValue[ProbabilityOfDetection,LowSNR],DiversityType->"SLC",Timed->OptionValue[ProbabilityOfDetection,Timed],MaxTime->OptionValue[ProbabilityOfDetection,MaxTime],MaxIterations->OptionValue[ProbabilityOfDetection,MaxIterations]};
NNakagamiProbabilityOfDetection::usage="NNakagamiProbabilityOfDetection[M, \[Gamma], \[Lambda], m] calculates the probability of detection for a single energy detector operating on a Nakagami-m fading channel.
NNakagamiProbabilityOfDetection[M, \[Gamma], \[Lambda], m, n] calculates the probability of detection for the fusion center of a cooperative network operating on a Nakagami-m fading channel.

The following methods can be given:

Method\[Rule]\"Approximate\"
Method\[Rule]{\"Approximate\", Algorithm\[Rule]...}
Method\[Rule]\"Exact\"
Method\[Rule]{\"Exact\", Algorithm\[Rule]...}

By default, Method\[Rule]\""<>ToString[Method/.Options[NNakagamiProbabilityOfDetection]]<>"\".

For a given method, an algorithm may be specified. If Method\[Rule]\"Approximate\", then the following algorithms may be specified:

Algorithm\[Rule]\"IntegerMN\"
Algorithm\[Rule]\"LargeMN\"
Algorithm\[Rule]{\"SwitchedMN\", SwitchingPoint}
Algorithm\[Rule]\"NGaussian\"

By default, Algorithm\[Rule]\""<>ToString[Algorithm/.Options[NNakagamiProbabilityOfDetection]]<>"\". If Algorithm\[Rule]\"SwitchedMN\", then the switching point between the IntegerMN and LargeMN algorithms may be specified using the SwitchingPoint option. If Algorithm\[Rule]\"NGaussian\", then a LowSNR boolean option may also be specified so that Method\[Rule]{\"Approximate\", Algorithm\[Rule]\"NGaussian\", LowSNR->"<>ToString[LowSNR/.Options[NNakagamiProbabilityOfDetection]]<>"}. By default, LowSNR\[Rule]"<>ToString[LowSNR/.Options[NNakagamiProbabilityOfDetection]]<>".

For the exact method, the following algorithms may be specified:

Algorithm\[Rule]\"Annamalai\"
Algorithm\[Rule]\"Digham\"
Algorithm\[Rule]\"Herath\"
Algorithm\[Rule]\"Sun\"

By default, Algorithm\[Rule]\"Annamalai\".

In addition, timing options may be specified. The timing option is specified by:

Timed\[Rule]"<>ToString[Timed/.Options[ProbabilityOfDetection]]<>"
MaxIterations\[Rule]"<>ToString[MaxIterations/.Options[ProbabilityOfDetection]]<>"
MaxTime\[Rule]"<>ToString[MaxTime/.Options[ProbabilityOfDetection]]<>"

where the above options are the defaults, if not specified. If Timed\[Rule]True, then a {Pd, time} list of values will be returned.";
NNakagamiProbabilityOfDetection[M_,\[Gamma]_,\[Lambda]_,m_,OptionsPattern[]]:=Module[{n = 1, RelevantOptions},
	RelevantOptions[target_]:=FilterRules[Table[#[[i]]->OptionValue[#[[i]]],{i,Length[#]}]&[Options[NNakagamiProbabilityOfDetection][[All,1]]],Options[target][[All,1]]];
	NNakagamiProbabilityOfDetection[M,\[Gamma],\[Lambda],m,n,#/.(DiversityType/.#)->"None"&[RelevantOptions[NNakagamiProbabilityOfDetection]]]
]
NNakagamiProbabilityOfDetection[M_,\[Gamma]_,\[Lambda]_,m_,n_,OptionsPattern[]]:=Module[{RelevantOptions, method = OptionValue[Method], algorithm = OptionValue[Algorithm]},
	RelevantOptions[target_]:=FilterRules[Table[#[[i]]->OptionValue[#[[i]]],{i,Length[#]}]&[Options[NNakagamiProbabilityOfDetection][[All,1]]],Options[target][[All,1]]];
	Which[
		method == "Exact",
			Which[
				algorithm == "Annamalai",
					NAnnamalaiNakagamiProbabilityOfDetection[M,\[Gamma],\[Lambda],m,n,RelevantOptions[NAnnamalaiNakagamiProbabilityOfDetection]],
				algorithm == "Digham",
					NDighamNakagamiProbabilityOfDetection[M,\[Gamma],\[Lambda],m,n,RelevantOptions[NDighamNakagamiProbabilityOfDetection]],
				algorithm == "Herath",
					NHerathNakagamiProbabilityOfDetection[M,\[Gamma],\[Lambda],m,n,RelevantOptions[NHerathNakagamiProbabilityOfDetection]],
				algorithm == "Sun",
					NSunNakagamiProbabilityOfDetection[M,\[Gamma],\[Lambda],m,n,RelevantOptions[NHerathNakagamiProbabilityOfDetection]],
				True,
					Undefined
			],
		method == "Approximate",
			Which[
				!ListQ[algorithm] && algorithm == "IntegerMN",
					IntegerMNNakagamiProbabilityOfDetection[M,\[Gamma],\[Lambda],m,n,RelevantOptions[IntegerMNNakagamiProbabilityOfDetection]],
				!ListQ[algorithm] && algorithm == "LargeMN",
					LargeMNNakagamiProbabilityOfDetection[M,\[Gamma],\[Lambda],m,n,RelevantOptions[LargeMNNakagamiProbabilityOfDetection]],
				ListQ[algorithm] && algorithm[[1]] == "SwitchedMN",
					If[m n >= algorithm[[2]],
						LargeMNNakagamiProbabilityOfDetection[M,\[Gamma],\[Lambda],m,n,RelevantOptions[LargeMNNakagamiProbabilityOfDetection]],
						IntegerMNNakagamiProbabilityOfDetection[M,\[Gamma],\[Lambda],m,n,RelevantOptions[IntegerMNNakagamiProbabilityOfDetection]]
					],
				!ListQ[algorithm] && algorithm == "NGaussian",
					NGaussianNakagamiProbabilityOfDetection[M,\[Gamma],\[Lambda],m,n,RelevantOptions[NGaussianNakagamiProbabilityOfDetection]],
				True,
					Undefined
			],
		True,
			Undefined
	]
]


(* ::Subsubsection::Closed:: *)
(*Annamalai' s method*)


Options[AnnamalaiLimit]={DiversityType->"SLC",Tolerance->10^-6};
AnnamalaiLimit::usage="AnnamalaiLimit[M, \[Gamma], \[Lambda], m] calculates the truncation point for use in Herath's algorithm using the default tolerance for a single energy detector.
AnnamalaiLimit[M, \[Gamma], \[Lambda], m, n] calculates the truncation point for use in Herath's algorithm using the default tolerance for a cooperative network.

The calculation tolerance may be specified using the Tolerance option. By default, Tolerance\[Rule]"<>ToString[Tolerance/.Options[AnnamalaiLimit]//N//InputForm]<>".";
AnnamalaiLimit[M_?NumericQ,\[Gamma]_?NumericQ,\[Lambda]_,m_?NumericQ,OptionsPattern[]]:=Module[{n = 1},AnnamalaiLimit[M,\[Gamma],\[Lambda],m,n,DiversityType->"None",Tolerance->OptionValue[Tolerance]]]
AnnamalaiLimit[M_?NumericQ,\[Gamma]_?NumericQ,\[Lambda]_,m_?NumericQ,n_?IntegerQ,OptionsPattern[]]:=Module[{j, j0, tol = OptionValue[Tolerance], diversityType = OptionValue[DiversityType]},
	Quiet[
		Which[
			!ListQ[diversityType] && diversityType == "None",
				j0 = (\[Lambda] / 2) - (M / 2) - Sqrt[M / 2] InverseQ[1 - tol];
				j/.FindRoot[1 - GammaRegularized[(M / 2) + j + 1, \[Lambda] / 2] == tol,{j, j0, 1, \[Infinity]}],
			!ListQ[diversityType] && diversityType == "MRC",
				j0 = (\[Lambda] / 2) - (M / 2) - Sqrt[M / 2] InverseQ[1 - tol];
				j/.FindRoot[1 - GammaRegularized[(M / 2) + j + 1, \[Lambda] / 2] == tol,{j, j0, 1, \[Infinity]}],
			!ListQ[diversityType] && diversityType == "EGC",
				Undefined,
			!ListQ[diversityType] && diversityType == "SC",
				Undefined,
			ListQ[diversityType] && diversityType[[1]] == "SSC",
				Undefined,
			!ListQ[diversityType] && diversityType == "SLC",
				j0 = (\[Lambda] / 2) - (M n / 2) - Sqrt[M n / 2] InverseQ[1 - tol];
				j/.FindRoot[1 - GammaRegularized[(M / 2) n + j + 1, \[Lambda] / 2] == tol,{j, j0, 1, \[Infinity]}],
			!ListQ[diversityType] && diversityType == "SLS",
				AnnamalaiLimit[M,\[Gamma],\[Lambda],m,n,DiversityType->"None"],
			True,
				Undefined
		]//N//Ceiling
	]
]


Options[NAnnamalaiNakagamiProbabilityOfDetection]={DiversityType->"SLC",Timed->OptionValue[ProbabilityOfDetection,Timed],MaxTime->OptionValue[ProbabilityOfDetection,MaxTime],MaxIterations->OptionValue[ProbabilityOfDetection,MaxIterations]};
NAnnamalaiNakagamiProbabilityOfDetection::usage="NAnnamalaiNakagamiProbabilityOfDetection[M, \[Gamma], \[Lambda], m, lim] calculates the exact probability of detection for a single energy detector operating in a Nakagami-m fading channel using Annamalai's algorithm.
NAnnamalaiNakagamiProbabilityOfDetection[M, \[Gamma], \[Lambda], m, n, lim] calculates the exact probability of detection for a cooperative network operating in a Nakagami-m fading channel using Annamalai's algorithm.

Function timing may be specified using the following options:

Timed\[Rule]"<>ToString[Timed/.Options[ProbabilityOfDetection]]<>"
MaxIterations\[Rule]"<>ToString[MaxIterations/.Options[ProbabilityOfDetection]]<>"
MaxTime\[Rule]"<>ToString[MaxTime/.Options[ProbabilityOfDetection]]<>"

where the above settings are the defaults, if not specified. If Timed\[Rule]True, then a {Pd, time} list of values will be returned.";
NAnnamalaiNakagamiProbabilityOfDetection[M_?NumericQ,\[Gamma]_?NumericQ,\[Lambda]_,m_?NumericQ,OptionsPattern[]]:=Module[{RelevantOptions, n = 1},
	RelevantOptions[target_]:=FilterRules[Table[#[[i]]->OptionValue[#[[i]]],{i,Length[#]}]&[Options[NAnnamalaiNakagamiProbabilityOfDetection][[All,1]]],Options[target][[All,1]]];
	NAnnamalaiNakagamiProbabilityOfDetection[M,\[Gamma],\[Lambda],m,n,#/.(DiversityType/.#)->"None"&[RelevantOptions[NAnnamalaiNakagamiProbabilityOfDetection]]]
]
NAnnamalaiNakagamiProbabilityOfDetection[M_?NumericQ,\[Gamma]_?NumericQ,\[Lambda]_,m_?NumericQ,n_?IntegerQ,OptionsPattern[]]:=Module[{lim, f, totaltime = 0, iterations = 0, time, result, diversityType = OptionValue[DiversityType]},
	lim = AnnamalaiLimit[M,\[Gamma],\[Lambda],m,n,DiversityType->diversityType];
	f:=Which[
		!ListQ[diversityType] && diversityType == "None",
			1 - ((m / (m + (M / 2) \[Gamma]))^m) (1 - GammaRegularized[M / 2, \[Lambda] / 2]) - Total[Table[(Gamma[m + k] / (Gamma[m] Gamma[k+1])) ((m / (m + (M / 2) \[Gamma]))^m) ((((M / 2) \[Gamma]) / (m + (M / 2) \[Gamma]))^k) (1 - GammaRegularized[M / 2 + k, \[Lambda] / 2]),{k, 1, lim}]],
		!ListQ[diversityType] && diversityType == "MRC",
			1 - (2m / (2m + M \[Gamma]))^(m n) (1 - GammaRegularized[M / 2, \[Lambda] / 2]) - Total[Table[Gamma[m n+k]/(Gamma[m n]Gamma[k+1]) (m/(m+M/2 \[Gamma]))^(m n) (((M/2 \[Gamma])/(m+M/2 \[Gamma]))^k) (1-GammaRegularized[M / 2 + k, \[Lambda] / 2]),{k,1,lim}]],
		!ListQ[diversityType] && diversityType == "EGC",
			Undefined,
		!ListQ[diversityType] && diversityType == "SC",
			Undefined,
		ListQ[diversityType] && diversityType[[1]] == "SSC",
			Undefined,
		!ListQ[diversityType] && diversityType == "SLC",
			1 - (m / (m + (M / 2) \[Gamma]))^(m n) (1 - GammaRegularized[M n/2, \[Lambda] / 2]) - Total[Table[Gamma[m n+k]/(Gamma[m n]Gamma[k+1]) (m/(m+M/2 \[Gamma]))^(m n) (((M/2 \[Gamma])/(m+M/2 \[Gamma]))^k) (1-GammaRegularized[M n/2+k, \[Lambda] / 2]),{k,1,lim}]],
		!ListQ[diversityType] && diversityType == "SLS",
			Which[
				ListQ[\[Gamma]],
					1 - Product[1 - NAnnamalaiNakagamiProbabilityOfDetection[M,\[Gamma][[i]],\[Lambda],m,DiversityType->"None"],{i,n}],
				!ListQ[\[Gamma]],
					1 - (1 - NAnnamalaiNakagamiProbabilityOfDetection[M,\[Gamma],\[Lambda],m,DiversityType->"None"])^n,
				True,
					Undefined
			],
		True,
			Undefined
	];
	If[OptionValue[Timed],
		(* Evaluate result until MaxTime seconds of CPU time have been used or MaxIterations have been performed, whichever comes first *)
		While[totaltime < OptionValue[MaxTime] && iterations < OptionValue[MaxIterations],
			ClearSystemCache[];
			{time, result} = TimeConstrained[Timing[f],OptionValue[MaxTime],{OptionValue[MaxTime],Null}];
			totaltime += time;
			iterations++;
		];
		{result,totaltime/iterations},
		f
	]
]


(* ::Subsubsection::Closed:: *)
(*Digham' s method*)


Options[NDighamNakagamiProbabilityOfDetection]={DiversityType->"SLC",Timed->OptionValue[ProbabilityOfDetection,Timed],MaxTime->OptionValue[ProbabilityOfDetection,MaxTime],MaxIterations->OptionValue[ProbabilityOfDetection,MaxIterations]};
NDighamNakagamiProbabilityOfDetection::usage="NDighamNakagamiProbabilityOfDetection[M, \[Gamma], \[Lambda], m] calculates the exact probability of detection for a single energy detector operating in a Nakagami-m fading channel using Digham's algorithm.
NDighamNakagamiProbabilityOfDetection[M, \[Gamma], \[Lambda], m, n] calculates the exact probability of detection for a cooperative network operating in a Nakagami-m fading channel using Digham's algorithm.

Function timing may be specified using the following options:

Timed\[Rule]"<>ToString[Timed/.Options[ProbabilityOfDetection]]<>"
MaxIterations\[Rule]"<>ToString[MaxIterations/.Options[ProbabilityOfDetection]]<>"
MaxTime\[Rule]"<>ToString[MaxTime/.Options[ProbabilityOfDetection]]<>"

where the above settings are the defaults, if not specified. If Timed\[Rule]True, then a {Pd, time} list of values will be returned.";
NDighamNakagamiProbabilityOfDetection[M_?NumericQ,\[Gamma]_?NumericQ,\[Lambda]_,m_?NumericQ,OptionsPattern[]]:=Module[{RelevantOptions, n = 1},
	RelevantOptions[target_]:=FilterRules[Table[#[[i]]->OptionValue[#[[i]]],{i,Length[#]}]&[Options[NDighamNakagamiProbabilityOfDetection][[All,1]]],Options[target][[All,1]]];
	NDighamNakagamiProbabilityOfDetection[M,\[Gamma],\[Lambda],m,n,RelevantOptions[NDighamNakagamiProbabilityOfDetection]]
]
NDighamNakagamiProbabilityOfDetection[M_?NumericQ,\[Gamma]_?NumericQ,\[Lambda]_,m_?NumericQ,n_?IntegerQ,OptionsPattern[]]:=Module[{lim, f, totaltime = 0, iterations = 0, time, result, diversityType = OptionValue[DiversityType], RelevantOptions},
	RelevantOptions[target_]:=FilterRules[Table[#[[i]]->OptionValue[#[[i]]],{i,Length[#]}]&[Options[NDighamNakagamiProbabilityOfDetection][[All,1]]],Options[target][[All,1]]];
	f:=Which[
		!ListQ[diversityType] && diversityType == "None",
			Module[{A1, \[Beta]},
				A1 = Exp[-\[Lambda] \[Beta] / (2 m)] (\[Beta]^(m - 1) LaguerreL[m - 1, -\[Lambda] (1 - \[Beta]) / 2] + (1 - \[Beta]) Total[Table[\[Beta]^i LaguerreL[i,-\[Lambda] (1 - \[Beta]) / 2], {i, 0, m - 2}]]);
				\[Beta] = (2m) / (2m + M \[Gamma]);
				A1 + \[Beta]^(m) Exp[-\[Lambda] / 2] Total[Table[((\[Lambda] / 2)^i / i!) Hypergeometric1F1[m, i + 1, \[Lambda] (1 - \[Beta]) / 2], {i, 1, (M / 2) - 1}]]
			],
		!ListQ[diversityType] && diversityType == "MRC",
			Undefined,
		!ListQ[diversityType] && diversityType == "EGC",
			Undefined,
		!ListQ[diversityType] && diversityType == "SC",
			Undefined,
		ListQ[diversityType] && diversityType[[1]] == "SSC",
			Undefined,
		!ListQ[diversityType] && diversityType == "SLC",
			Module[{A1, \[Beta]},
				A1 = Exp[-\[Lambda] \[Beta] / (2 m n)] (\[Beta]^(m n - 1) LaguerreL[m n - 1, -\[Lambda] (1 - \[Beta]) / 2] + (1 - \[Beta]) Total[Table[\[Beta]^i LaguerreL[i,-\[Lambda] (1 - \[Beta]) / 2], {i, 0, m n - 2}]]);
				\[Beta] = (2m) / (2m + M \[Gamma]);
				A1 + \[Beta]^(m n) Exp[-\[Lambda] / 2] Total[Table[((\[Lambda] / 2)^i / i!) Hypergeometric1F1[m n, i + 1, \[Lambda] (1 - \[Beta]) / 2], {i, 1, (M n / 2) - 1}]]
			],
		!ListQ[diversityType] && diversityType == "SLS",
			Which[
				ListQ[\[Gamma]],
					1 - Product[1 - NDighamNakagamiProbabilityOfDetection[M,\[Gamma][[i]],\[Lambda],m,DiversityType->"None"],{i,n}],
				!ListQ[\[Gamma]],
					1 - (1 - NDighamNakagamiProbabilityOfDetection[M,\[Gamma],\[Lambda],m,DiversityType->"None"])^n,
				True,
					Undefined
			],
		True,
			Undefined
	];
	If[OptionValue[Timed],
		(* Evaluate result until MaxTime seconds of CPU time have been used or MaxIterations have been performed, whichever comes first *)
		While[totaltime < OptionValue[MaxTime] && iterations < OptionValue[MaxIterations],
			ClearSystemCache[];
			{time, result} = TimeConstrained[Timing[f],OptionValue[MaxTime],{OptionValue[MaxTime],Null}];
			totaltime += time;
			iterations++;
		];
		{result,totaltime/iterations},
		f
	]
]


(* ::Subsubsection::Closed:: *)
(*Herath' s method*)


Options[HerathLimit]={DiversityType->"SLC",Tolerance->10^-6};
HerathLimit::usage="HerathLimit[M, \[Gamma], \[Lambda], m] calculates the truncation point for use in Herath's algorithm using the default tolerance for a single energy detector.
HerathLimit[M, \[Gamma], \[Lambda], m, n] calculates the truncation point for use in Herath's algorithm using the default tolerance for a cooperative network.

The calculation tolerance may be specified using the Tolerance option. By default, Tolerance\[Rule]"<>ToString[Tolerance/.Options[HerathLimit]//N//InputForm]<>".";
HerathLimit[M_?NumericQ,\[Gamma]_?NumericQ,\[Lambda]_,m_?NumericQ,OptionsPattern[]]:=Module[{n = 1},HerathLimit[M,\[Gamma],\[Lambda],m,n,Tolerance->OptionValue[Tolerance]]]
HerathLimit[M_?NumericQ,\[Gamma]_?NumericQ,\[Lambda]_,m_?NumericQ,n_?IntegerQ,OptionsPattern[]]:=Module[{j, j0, tol = OptionValue[Tolerance], diversityType = OptionValue[DiversityType]},
	Which[
		!ListQ[diversityType] && diversityType == "None",
			j0 = (\[Lambda] / 2) - 1 - InverseQ[1 - tol];
			Print[j0];
			j/.FindRoot[(m / ((M / 2) \[Gamma] + m))^(m) Hypergeometric1F1[m, j + 1, \[Lambda] (M / 2) \[Gamma] / (2 ((M / 2) \[Gamma] + m))] (1 - GammaRegularized[j + 1, \[Lambda] / 2]) == tol,{j, j0, 1, \[Infinity]}],
		!ListQ[diversityType] && diversityType == "MRC",
			j0 = (\[Lambda] / 2) - 1 - InverseQ[1 - tol];
			j/.FindRoot[(m / ((M / 2) \[Gamma] + m))^(m) Hypergeometric1F1[m, j + 1, \[Lambda] (M / 2) \[Gamma] / (2 ((M / 2) \[Gamma] + m))] (1 - GammaRegularized[j + 1, \[Lambda] / 2]) == tol,{j, j0, 1, \[Infinity]}],
		!ListQ[diversityType] && diversityType == "EGC",
			Undefined,
		!ListQ[diversityType] && diversityType == "SC",
			j0 = (\[Lambda] / 2) - 1 - InverseQ[1 - tol];
			Print[j0];
			j/.FindRoot[n (m / \[Gamma])^(m) (1 - GammaRegularized[j + 1, \[Lambda] / 2]) Total[Table[Binomial[n - 1, k] Total[Table[MultinomialCoefficient[k, i, m] Pochhammer[m, i] (\[Gamma] / (\[Gamma] + m (k + 1)))^(i + m) Hypergeometric1F1[i + m, j + 1, \[Lambda] \[Gamma] / (2 (\[Gamma] + m (k + 1)))],{i, 0, k*(m - 1)}]],{k, 0, n - 1}]] == tol,{j, j0, 1, \[Infinity]}],
		ListQ[diversityType] && diversityType[[1]] == "SSC",
			Module[{\[Gamma]t = diversityType[[2]]},
				j0 = {(\[Lambda] / 2) - 1 - InverseQ[1 - tol], Null};
				{j/.FindRoot[(1 - GammaRegularized[m, m (M / 2) \[Gamma]t / ((M / 2) \[Gamma])]) Hypergeometric1F1[m, j + 1, \[Lambda] (M / 2) \[Gamma] / (2 ((M / 2) \[Gamma] + m))] (1 - GammaRegularized[j + 1, \[Lambda] / 2]) (m / (m + (M / 2) \[Gamma]))^(m) == tol,{j, j0[[1]], 1, \[Infinity]}], InverseCDF[NegativeBinomialDistribution[m, m / ((M / 2) \[Gamma] + m)], 1 - tol]}
			],
		!ListQ[diversityType] && diversityType == "SLC",
			j0 = (\[Lambda] / 2) - 1 - InverseQ[1 - tol];
			j/.FindRoot[(m / ((M / 2) \[Gamma] + m))^(m n) Hypergeometric1F1[m n, j + 1, \[Lambda] (M / 2) \[Gamma] / (2 ((M / 2) \[Gamma] + m))] (1 - GammaRegularized[j + 1, \[Lambda] / 2]) == tol,{j, j0, 1, \[Infinity]}],
		!ListQ[diversityType] && diversityType == "SLS",
			j0 = (\[Lambda] / 2) - 1 - InverseQ[1 - tol];
			j/.FindRoot[(m / ((M / 2) \[Gamma] + m))^(m) Hypergeometric1F1[m, j + 1, \[Lambda] (M / 2) \[Gamma] / (2 ((M / 2) \[Gamma] + m))] (1 - GammaRegularized[j + 1, \[Lambda] / 2]) == tol,{j, M / 2}],
		True,
			Undefined
	]//Ceiling
]


Options[NHerathNakagamiProbabilityOfDetection]={DiversityType->"SLC",Timed->OptionValue[ProbabilityOfDetection,Timed],MaxTime->OptionValue[ProbabilityOfDetection,MaxTime],MaxIterations->OptionValue[ProbabilityOfDetection,MaxIterations]};
NHerathNakagamiProbabilityOfDetection::usage="NHerathNakagamiProbabilityOfDetection[M, \[Gamma], \[Lambda], m] calculates the exact probability of detection for a single energy detector operating in a Nakagami-m fading channel using Herath's algorithm.
NHerathNakagamiProbabilityOfDetection[M, \[Gamma], \[Lambda], m, n] calculates the exact probability of detection for a cooperative network operating in a Nakagami-m fading channel using Herath's algorithm.

Function timing may be specified using the following options:

Timed\[Rule]"<>ToString[Timed/.Options[ProbabilityOfDetection]]<>"
MaxIterations\[Rule]"<>ToString[MaxIterations/.Options[ProbabilityOfDetection]]<>"
MaxTime\[Rule]"<>ToString[MaxTime/.Options[ProbabilityOfDetection]]<>"

where the above settings are the defaults, if not specified. If Timed\[Rule]True, then a {Pd, time} list of values will be returned.";
NHerathNakagamiProbabilityOfDetection[M_?NumericQ,\[Gamma]_?NumericQ,\[Lambda]_,m_?NumericQ,OptionsPattern[]]:=Module[{RelevantOptions, n = 1},
	RelevantOptions[target_]:=FilterRules[Table[#[[i]]->OptionValue[#[[i]]],{i,Length[#]}]&[Options[NHerathNakagamiProbabilityOfDetection][[All,1]]],Options[target][[All,1]]];
	NHerathNakagamiProbabilityOfDetection[M,\[Gamma],\[Lambda],m,n,RelevantOptions[NHerathNakagamiProbabilityOfDetection]]
]
NHerathNakagamiProbabilityOfDetection[M_?NumericQ,\[Gamma]_?NumericQ,\[Lambda]_,m_?NumericQ,n_?IntegerQ,OptionsPattern[]]:=Module[{lim, f, totaltime = 0, iterations = 0, time, result, diversityType = OptionValue[DiversityType]},
	lim = HerathLimit[M,\[Gamma],\[Lambda],m,n,DiversityType->diversityType];
	f:=Which[
		!ListQ[diversityType] && diversityType == "None",
			1 - Exp[-\[Lambda] / 2] (m / ((M / 2) \[Gamma] + m))^(m) Total[Table[((\[Lambda] / 2)^j / j!) Hypergeometric1F1[m, j + 1, \[Lambda] (M / 2) \[Gamma] / (2 ((M / 2) \[Gamma] + m))],{j, (M / 2), lim}]],
		!ListQ[diversityType] && diversityType == "MRC",
			1 - Exp[-\[Lambda] / 2] (m / ((M / 2) \[Gamma] + m))^(n m) Total[Table[((\[Lambda] / 2)^j / j!) Hypergeometric1F1[m n, j + 1, \[Lambda] (M / 2) \[Gamma] / (2 ((M / 2) \[Gamma] + m))],{j, (M / 2), lim}]],
		!ListQ[diversityType] && diversityType == "EGC",
			Undefined,
		!ListQ[diversityType] && diversityType == "SC",
			1 - n Exp[-\[Lambda] / 2] (m / \[Gamma])^(m) Total[Table[Total[Table[Binomial[n - 1, k] (-1)^(k) ((\[Lambda] / 2)^(j) / (j!)) Total[Table[MultinomialCoefficient[k, i, m] Pochhammer[m, i] (\[Gamma] / (\[Gamma] + m (k + 1)))^(i + m) Hypergeometric1F1[i + m, j + 1, \[Lambda] \[Gamma] / (2 (\[Gamma] + m (k + 1)))],{i, 0, k*(m - 1)}]],{k, 0, n - 1}]],{j, M / 2, lim}]],
		ListQ[diversityType] && diversityType[[1]] == "SSC",
			Module[{\[Gamma]t = diversityType[[2]]},
				(1 - GammaRegularized[m, m \[Gamma]t / \[Gamma]]) (1 - Exp[-\[Lambda] / 2] (m / (m + (M / 2) \[Gamma]))^(m) Total[Table[((\[Lambda] / 2)^(j) / j!) Hypergeometric1F1[m, j + 1, \[Lambda] (M / 2) \[Gamma] / (2 ((M / 2) \[Gamma] + m))],{j, M / 2, lim[[1]]}]]) + (m / ((M / 2) \[Gamma] + m))^(m) (Exp[-\[Lambda] / 2] / Gamma[m]) Total[Table[Total[Table[((M / 2) \[Gamma] / ((M / 2) \[Gamma] + m))^(j) Gamma[j + m, (1 + m / ((M / 2) \[Gamma])) (M / 2) \[Gamma]t] (\[Lambda] / 2)^(k) / (j! k!),{k, 0, j + (M / 2) - 1}]],{j, 0, lim[[2]]}]]
			],
		!ListQ[diversityType] && diversityType == "SLC",
			1 - Exp[-\[Lambda] / 2] (m / ((M / 2) \[Gamma] + m))^(n m) Total[Table[((\[Lambda] / 2)^j / j!) Hypergeometric1F1[m n, j + 1, \[Lambda] (M / 2) \[Gamma] / (2 ((M / 2) \[Gamma] + m))],{j, (M n / 2), lim}]],
		!ListQ[diversityType] && diversityType == "SLS",
			Which[
				ListQ[\[Gamma]],
					1 - Product[1 - NHerathNakagamiProbabilityOfDetection[M,\[Gamma][[i]],\[Lambda],m,DiversityType->"None"],{i,n}],
				!ListQ[\[Gamma]],
					1 - (1 - NHerathNakagamiProbabilityOfDetection[M,\[Gamma],\[Lambda],m,DiversityType->"None"])^n,
				True,
					Undefined
			],
		True,
			Undefined
	];
	If[OptionValue[Timed],
		(* Evaluate result until MaxTime seconds of CPU time have been used or MaxIterations have been performed, whichever comes first *)
		While[totaltime < OptionValue[MaxTime] && iterations < OptionValue[MaxIterations],
			ClearSystemCache[];
			{time, result} = TimeConstrained[Timing[f],OptionValue[MaxTime],{OptionValue[MaxTime],Null}];
			totaltime += time;
			iterations++;
		];
		{result,totaltime/iterations},
		f
	]
];


(* ::Subsubsection::Closed:: *)
(*Sun' s method*)


Options[SunLimit]={DiversityType->"SLC",Tolerance->10^-6};
SunLimit::usage="SunLimit[M, \[Gamma], \[Lambda], m] calculates the truncation point for use in Sun's algorithm using the default tolerance for a single energy detector.
SunLimit[M, \[Gamma], \[Lambda], m, n] calculates the truncation point for use in Sun's algorithm using the default tolerance for a cooperative network.

The calculation tolerance may be specified using the Tolerance option. By default, Tolerance\[Rule]"<>ToString[Tolerance/.Options[SunLimit]//N//InputForm]<>".";
SunLimit[M_?NumericQ,\[Gamma]_?NumericQ,\[Lambda]_,m_?NumericQ,OptionsPattern[]]:=Module[{n = 1},SunLimit[M,\[Gamma],\[Lambda],m,n,Tolerance->OptionValue[Tolerance]]]
SunLimit[M_?NumericQ,\[Gamma]_?NumericQ,\[Lambda]_,m_?NumericQ,n_?IntegerQ,OptionsPattern[]]:=Module[{j, j0, tol = OptionValue[Tolerance], diversityType = OptionValue[DiversityType]},
	Which[
		!ListQ[diversityType] && diversityType == "None",
			j0 = (\[Lambda] / 2) - (M / 2) - Sqrt[M / 2] InverseQ[1 - tol];
			j/.FindRoot[(1 - GammaRegularized[M / 2 + j - 1, \[Lambda] / 2]) (1 - CDF[NegativeBinomialDistribution[m, (m / (m + (M / 2) \[Gamma]))^(m)], j + 1]) == tol,{j, j0, 1, \[Infinity]}],
		!ListQ[diversityType] && diversityType == "MRC",
			AnnamalaiLimit[M, \[Gamma], \[Lambda], m, n, DiversityType->diversityType],
		!ListQ[diversityType] && diversityType == "EGC",
			Undefined,
		!ListQ[diversityType] && diversityType == "SC",
			j0 = (\[Lambda] / 2) - (M / 2) - Sqrt[M / 2] InverseQ[1 - tol];
			j/.FindRoot[n Total[Table[(-1)^(l) Binomial[n - 1, l] Total[Table[MultinomialCoefficient[l, k, m] (Gamma[m + k] / Gamma[m]) (1 - GammaRegularized[M / 2 + j, \[Lambda] / 2]) (1 / (l + 1))^(m + k), {k, 0, l (m - 1)}]], {l, 0, n - 1}]] == tol, {j, j0, 1, \[Infinity]}],
		ListQ[diversityType] && diversityType[[1]] == "SSC",
			Undefined,
		!ListQ[diversityType] && diversityType == "SLC",
			AnnamalaiLimit[M, \[Gamma], \[Lambda], m, n, DiversityType->diversityType],
		!ListQ[diversityType] && diversityType == "SLS",
			SunLimit[M, \[Gamma], \[Lambda], m, n, DiversityType->"None"],
		True,
			Undefined
	]//N//Ceiling
]


Options[NSunNakagamiProbabilityOfDetection]={DiversityType->"SLC",Timed->OptionValue[ProbabilityOfDetection,Timed],MaxTime->OptionValue[ProbabilityOfDetection,MaxTime],MaxIterations->OptionValue[ProbabilityOfDetection,MaxIterations]};
NSunNakagamiProbabilityOfDetection::usage="NSunNakagamiProbabilityOfDetection[M, \[Gamma], \[Lambda], m] calculates the exact probability of detection for a single energy detector operating in a Nakagami-m fading channel using Sun's algorithm.
NSunNakagamiProbabilityOfDetection[M, \[Gamma], \[Lambda], m, n] calculates the exact probability of detection for a cooperative network operating in a Nakagami-m fading channel using Sun's algorithm.

Function timing may be specified using the following options:

Timed\[Rule]"<>ToString[Timed/.Options[ProbabilityOfDetection]]<>"
MaxIterations\[Rule]"<>ToString[MaxIterations/.Options[ProbabilityOfDetection]]<>"
MaxTime\[Rule]"<>ToString[MaxTime/.Options[ProbabilityOfDetection]]<>"

where the above settings are the defaults, if not specified. If Timed\[Rule]True, then a {Pd, time} list of values will be returned.";
NSunNakagamiProbabilityOfDetection[M_?NumericQ,\[Gamma]_?NumericQ,\[Lambda]_,m_?NumericQ,OptionsPattern[]]:=Module[{RelevantOptions, n = 1},
	RelevantOptions[target_]:=FilterRules[Table[#[[i]]->OptionValue[#[[i]]],{i,Length[#]}]&[Options[NSunNakagamiProbabilityOfDetection][[All,1]]],Options[target][[All,1]]];
	NSunNakagamiProbabilityOfDetection[M,\[Gamma],\[Lambda],m,n,RelevantOptions[NSunNakagamiProbabilityOfDetection]]
]
NSunNakagamiProbabilityOfDetection[M_?NumericQ,\[Gamma]_?NumericQ,\[Lambda]_,m_?NumericQ,n_?IntegerQ,OptionsPattern[]]:=Module[{lim, f, totaltime = 0, iterations = 0, time, result, diversityType = OptionValue[DiversityType]},
	lim = SunLimit[M,\[Gamma],\[Lambda],m,n,DiversityType->diversityType];
	f:=Which[
		!ListQ[diversityType] && diversityType == "None",
			GammaRegularized[M / 2, \[Lambda] / 2] + Exp[-\[Lambda] / 2] Total[Table[((\[Lambda] / 2)^(j) / j!) (1 - (m / (m + (M / 2) \[Gamma]))^(m) Total[Table[(m + k - 1)! / (Gamma[m] k!) ((M / 2) \[Gamma] / (m + (M / 2) \[Gamma]))^(k),{k, 0, j - M / 2}]]),{j, M / 2, M / 2 + lim}]],
		!ListQ[diversityType] && diversityType == "MRC",
			1 - (m / (m + (M / 2) \[Gamma]))^(m n) Total[Table[(1 - GammaRegularized[(M / 2) + j, \[Lambda] / 2]) Pochhammer[m n, j] / j! ((M / 2) \[Gamma] / (m + (M / 2) \[Gamma]))^(j),{j, 0, lim}]],
		!ListQ[diversityType] && diversityType == "EGC",
			Undefined,
		!ListQ[diversityType] && diversityType == "SC",
			1 - n Total[Table[(-1)^(l) Binomial[n - 1, l] Total[Table[MultinomialCoefficient[l, k, m] (m / ((M / 2) \[Gamma]))^(m + k) Total[Table[(1 - GammaRegularized[i + M / 2, \[Lambda] / 2]) Pochhammer[m, i + k] / i! ((M / 2) \[Gamma] / ((M / 2) \[Gamma] + m (l + 1)))^(i + m + k),{i, 0, lim}]],{k, 0, l (m - 1)}]],{l, 0, n - 1}]],
		ListQ[diversityType] && diversityType[[1]] == "SSC",
			Undefined,
		!ListQ[diversityType] && diversityType == "SLC",
			1 - (m / (m + (M / 2) \[Gamma]))^(m n) Total[Table[(1 - GammaRegularized[(M / 2) n + j, \[Lambda] / 2]) Pochhammer[m n, j] / j! ((M / 2) \[Gamma] / (m + (M / 2) \[Gamma]))^(j),{j, 0, lim}]],
		!ListQ[diversityType] && diversityType == "SLS",
			Which[
				ListQ[\[Gamma]],
					1 - Product[1 - NSunNakagamiProbabilityOfDetection[M,\[Gamma][[i]],\[Lambda],m,DiversityType->"None"],{i,n}],
				!ListQ[\[Gamma]],
					1 - (1 - NSunNakagamiProbabilityOfDetection[M,\[Gamma],\[Lambda],m,DiversityType->"None"])^n,
				True,
					Undefined
			],
		True,
			Undefined
	];
	If[OptionValue[Timed],
		(* Evaluate result until MaxTime seconds of CPU time have been used or MaxIterations have been performed, whichever comes first *)
		While[totaltime < OptionValue[MaxTime] && iterations < OptionValue[MaxIterations],
			ClearSystemCache[];
			{time, result} = TimeConstrained[Timing[f],OptionValue[MaxTime],{OptionValue[MaxTime],Null}];
			totaltime += time;
			iterations++;
		];
		{result,totaltime/iterations},
		f
	]
];


(* ::Subsubsection::Closed:: *)
(*Integer mn method*)


Options[IntegerMNNakagamiProbabilityOfDetection]={DiversityType->"SLC",Timed->OptionValue[ProbabilityOfDetection,Timed],MaxTime->OptionValue[ProbabilityOfDetection,MaxTime],MaxIterations->OptionValue[ProbabilityOfDetection,MaxIterations]};
IntegerMNNakagamiProbabilityOfDetection::usage="IntegerMNNakagamiProbabilityOfDetection[M, \[Gamma], \[Lambda], m] calculates the approximate probability of detection for a single energy detector operating in a Nakagami-m fading channel using the integer mn approximation.
IntegerMNNakagamiProbabilityOfDetection[M, \[Gamma], \[Lambda], m, n] calculates the approximate probability of detection for a cooperative network operating in a Nakagami-m fading channel using the integer mn approximation.

Function timing may be specified using the following options:

Timed\[Rule]"<>ToString[Timed/.Options[ProbabilityOfDetection]]<>"
MaxIterations\[Rule]"<>ToString[MaxIterations/.Options[ProbabilityOfDetection]]<>"
MaxTime\[Rule]"<>ToString[MaxTime/.Options[ProbabilityOfDetection]]<>"

where the above settings are the defaults, if not specified. If Timed\[Rule]True, then a {Pd, time} list of values will be returned.";
IntegerMNNakagamiProbabilityOfDetection[M_?NumericQ,\[Gamma]_?NumericQ,\[Lambda]_,m_?NumericQ,OptionsPattern[]]:=Module[{RelevantOptions, n = 1},
	RelevantOptions[target_]:=FilterRules[Table[#[[i]]->OptionValue[#[[i]]],{i,Length[#]}]&[Options[IntegerMNNakagamiProbabilityOfDetection][[All,1]]],Options[target][[All,1]]];
	IntegerMNNakagamiProbabilityOfDetection[M,\[Gamma],\[Lambda],m,n,#/.(DiversityType/.#)->"None"&[RelevantOptions[IntegerMNNakagamiProbabilityOfDetection]]]
]
IntegerMNNakagamiProbabilityOfDetection[M_?NumericQ,\[Gamma]_?NumericQ,\[Lambda]_,m_?NumericQ,n_?NumericQ,OptionsPattern[]]:=Module[{lim, f, totaltime = 0, iterations = 0, time, result, diversityType = OptionValue[DiversityType], x = Round[m n], tol = 10^-6},
	(* This method can only be used when m * n is an integer *)
	If[Abs[m n - x] <= tol,
		f:=Which[
			!ListQ[diversityType] && diversityType == "None",
				AWGNProbabilityOfFalseAlarm[M,\[Lambda]] - (D[((-1)^(x) / (2 Gamma[x])) * Exp[((m^2) / (M \[Gamma]^2)) * t^2 - ((\[Lambda] - M) / (M (\[Gamma] / m))) * t]Erfc[(2 t - (\[Lambda] - M) * (\[Gamma] / m)) / (2Sqrt[M] (\[Gamma] / m))] / t, {t, x - 1}]/.t->1),
			!ListQ[diversityType] && diversityType == "MRC",
				AWGNProbabilityOfFalseAlarm[M,\[Lambda]] - (D[((-1)^(x) / (2 Gamma[x])) * Exp[((m^2) / (M \[Gamma]^2)) * t^2 - ((\[Lambda] - M) / (M (\[Gamma] / m))) * t]Erfc[(2 t - (\[Lambda] - M) * (\[Gamma] / m)) / (2Sqrt[M] (\[Gamma] / m))] / t, {t, x - 1}]/.t->1),
			!ListQ[diversityType] && diversityType == "EGC",
				Undefined,
			!ListQ[diversityType] && diversityType == "SC",
				(n / Gamma[m]) Total[Table[(-1)^(l) Binomial[n - 1, l] Total[Table[MultinomialCoefficient[l, k, m] (D[-(-(1 / (1 + l)))^(k + m) (1 + Erf[(M - \[Lambda]) / (2 Sqrt[M])] + Exp[((1 + l) m t ((1 + l) m t + \[Gamma] (M - \[Lambda]))) / (M \[Gamma]^2)] Erfc[(2 (1 + l) m t + \[Gamma] (M - \[Lambda]))/(2 Sqrt[M] \[Gamma])]) / (2 t),{t, k + m - 1}]/.t->1),{k, 0, l (m - 1)}]],{l, 0, n - 1}]],
			ListQ[diversityType] && diversityType[[1]] == "SSC",
				Module[{\[Gamma]t = diversityType[[2]]},
					(1 - GammaRegularized[m, m \[Gamma]t/\[Gamma]]) IntegerMNNakagamiProbabilityOfDetection[M,\[Gamma],\[Lambda],m,DiversityType->"None"] + D[((-1)^m E^(-((m t (2 M \[Gamma]t+\[Lambda]))/(M \[Gamma]))) (-E^(((m t (m t+M \[Gamma] (1+2 \[Gamma]t)))/(M \[Gamma]^2))) Erfc[(2 m t+\[Gamma] (M+M \[Gamma]t-\[Lambda]))/(2 Sqrt[M] \[Gamma])]+E^((m t (M \[Gamma]t+\[Lambda]))/(M \[Gamma])) (-2+Erfc[(M+M \[Gamma]t-\[Lambda])/(2 Sqrt[M])])))/(2 t Gamma[m]),{t, m - 1}]/.t->1
				],
			!ListQ[diversityType] && diversityType == "SLC",
				AWGNProbabilityOfFalseAlarm[M,\[Lambda],n] - (D[((-1)^(x) / (2 Gamma[x])) * Exp[((m^2 n) / (M \[Gamma]^2)) * t^2 - ((\[Lambda] - M n) / (M (\[Gamma] / m))) * t]Erfc[(2n t - (\[Lambda] - M n) * (\[Gamma] / m)) / (2Sqrt[M n] (\[Gamma] / m))] / t, {t, x - 1}]/.t->1),
			!ListQ[diversityType] && diversityType == "SLS",
				Which[
					ListQ[\[Gamma]],
						1 - Product[1 - IntegerMNNakagamiProbabilityOfDetection[M,\[Gamma][[i]],\[Lambda],m,DiversityType->"None"],{i,n}],
					!ListQ[\[Gamma]],
						1 - (1 - IntegerMNNakagamiProbabilityOfDetection[M,\[Gamma],\[Lambda],m,DiversityType->"None"])^n,
					True,
						Undefined
				],
			True,
				Undefined
			],
		f:=Undefined
	];
	If[OptionValue[Timed],
		(* Evaluate result until MaxTime seconds of CPU time have been used or MaxIterations have been performed, whichever comes first *)
		While[totaltime < OptionValue[MaxTime] && iterations < OptionValue[MaxIterations],
			ClearSystemCache[];
			{time, result} = TimeConstrained[Timing[f],OptionValue[MaxTime],{OptionValue[MaxTime],Null}];
			totaltime += time;
			iterations++;
		];
		{result,totaltime/iterations},
		f//N
	]
]


(* ::Subsubsection::Closed:: *)
(*Large mn method*)


Options[LargeMNNakagamiProbabilityOfDetection]={DiversityType->"SLC",Timed->OptionValue[ProbabilityOfDetection,Timed],MaxTime->OptionValue[ProbabilityOfDetection,MaxTime],MaxIterations->OptionValue[ProbabilityOfDetection,MaxIterations]};
LargeMNNakagamiProbabilityOfDetection::usage="LargeMNNakagamiProbabilityOfDetection[M, \[Gamma], \[Lambda], m] calculates the approximate probability of detection for a single energy detector operating in a Nakagami-m fading channel using the large mn approximation.
LargeMNNakagamiProbabilityOfDetection[M, \[Gamma], \[Lambda], m, n] calculates the approximate probability of detection for a cooperative network operating in a Nakagami-m fading channel using the large mn approximation..

Function timing may be specified using the following options:

Timed\[Rule]"<>ToString[Timed/.Options[ProbabilityOfDetection]]<>"
MaxIterations\[Rule]"<>ToString[MaxIterations/.Options[ProbabilityOfDetection]]<>"
MaxTime\[Rule]"<>ToString[MaxTime/.Options[ProbabilityOfDetection]]<>"

where the above settings are the defaults, if not specified. If Timed\[Rule]True, then a {Pd, time} list of values will be returned.";
LargeMNNakagamiProbabilityOfDetection[M_?NumericQ,\[Gamma]_?NumericQ,\[Lambda]_,m_?NumericQ,OptionsPattern[]]:=Module[{RelevantOptions, n = 1},
	RelevantOptions[target_]:=FilterRules[Table[#[[i]]->OptionValue[#[[i]]],{i,Length[#]}]&[Options[LargeMNNakagamiProbabilityOfDetection][[All,1]]],Options[target][[All,1]]];
	LargeMNNakagamiProbabilityOfDetection[M,\[Gamma],\[Lambda],m,n,#/.(DiversityType/.#)->"None"&[RelevantOptions[LargeMNNakagamiProbabilityOfDetection]]]
]
LargeMNNakagamiProbabilityOfDetection[M_?NumericQ,\[Gamma]_?NumericQ,\[Lambda]_,m_?NumericQ,n_?NumericQ,OptionsPattern[]]:=Module[{lim, f, totaltime = 0, iterations = 0, time, result, diversityType = OptionValue[DiversityType]},
	f:=Which[
		!ListQ[diversityType] && diversityType == "None",
			(1/2 (1+Erf[(m (M (1+\[Gamma])-\[Lambda]))/(Sqrt[2] M Sqrt[m] \[Gamma])])+1/2 E^((4 c M (-m+a M \[Gamma]^2)+b (-b M^2 \[Gamma]^2+2 Sqrt[2] m Sqrt[M] (M (1+\[Gamma])-\[Lambda]))-2 a m (-M (1+\[Gamma])+\[Lambda])^2)/(4 M (-m+a M \[Gamma]^2))) Sqrt[m/(m-a M \[Gamma]^2)] (Erf[(b Sqrt[M^3] \[Gamma]^2+Sqrt[2] m (-M (1+\[Gamma])+\[Lambda]))/(2 M \[Gamma] Sqrt[(m-a M \[Gamma]^2)])]-Erf[(-2 m+\[Gamma] (-2 a M+Sqrt[2] b Sqrt[M]+2 a \[Lambda]))/(2 Sqrt[2] Sqrt[(m-a M \[Gamma]^2)])])+1/2 E^(-((4 c M (m-a M \[Gamma]^2)+b (b M^2 \[Gamma]^2+2 Sqrt[2] m Sqrt[M] (M (1+\[Gamma])-\[Lambda]))+2 a m (-M (1+\[Gamma])+\[Lambda])^2)/(4 M (-m+a M \[Gamma]^2)))) Sqrt[m/(m-a M \[Gamma]^2)] (-2+Erfc[(b Sqrt[M^3] \[Gamma]^2+Sqrt[2] m (M (1+\[Gamma])-\[Lambda]))/(2 M \[Gamma] Sqrt[(m-a M \[Gamma]^2)])]))/.LopezBenitezParameters[(-M (1+\[Gamma])+\[Lambda])/(2 Sqrt[M])],
		!ListQ[diversityType] && diversityType == "MRC",
			(-((E^(-((b^2 M n \[Gamma]^2+4 c (m-a M n \[Gamma]^2)+(2 Sqrt[2] b m (M+M n \[Gamma]-\[Lambda]))/Sqrt[M]+(2 a m (M+M n \[Gamma]-\[Lambda])^2)/M)/(4 (-m+a M n \[Gamma]^2)))) m (1+Erf[(b M^(3/2) n \[Gamma]^2+Sqrt[2] m (M+M n \[Gamma]-\[Lambda]))/(2 M \[Gamma] Sqrt[n (m-a M n \[Gamma]^2)])]))/(2 Sqrt[m (m-a M n \[Gamma]^2)]))+(E^(-((b^2 M n \[Gamma]^2+4 c (m-a M n \[Gamma]^2)-(2 Sqrt[2] b m (M+M n \[Gamma]-\[Lambda]))/Sqrt[M]+(2 a m (M+M n \[Gamma]-\[Lambda])^2)/M)/(4 (-m+a M n \[Gamma]^2)))) m (Erf[(Sqrt[2] b M^(3/2) n \[Gamma]^2-2 m (M+M n \[Gamma]-\[Lambda]))/(2 M n Sqrt[-2 a M+(2 m)/(n \[Gamma]^2)] \[Gamma]^2)]-Erf[(-2 m+\[Gamma] (Sqrt[2] b Sqrt[M]-2 a M+2 a \[Lambda]))/(2 Sqrt[-2 a M+(2 m)/(n \[Gamma]^2)] \[Gamma])]))/(2 Sqrt[m (m-a M n \[Gamma]^2)])+1/2 Erfc[-((m (M+M n \[Gamma]-\[Lambda]))/(Sqrt[2] M Sqrt[m n] \[Gamma]))])/.LopezBenitezParameters[(-M (1+\[Gamma])+\[Lambda])/(2 Sqrt[M])],
		!ListQ[diversityType] && diversityType == "EGC",
			Undefined,
		!ListQ[diversityType] && diversityType == "SC",
			Undefined,
		ListQ[diversityType] && diversityType[[1]] == "SSC",
			Undefined,
		!ListQ[diversityType] && diversityType == "SLC",
			(1/2 (1+Erf[(m (M n (1+\[Gamma])-\[Lambda]))/(Sqrt[2] M Sqrt[m n] \[Gamma])])+1/2 E^((4 c M n (-m+a M \[Gamma]^2)+b (-b M^2 n \[Gamma]^2+2 Sqrt[2] m Sqrt[M n] (M n (1+\[Gamma])-\[Lambda]))-2 a m (-M n (1+\[Gamma])+\[Lambda])^2)/(4 M n (-m+a M \[Gamma]^2))) Sqrt[m/(m-a M \[Gamma]^2)] (Erf[(b Sqrt[M^3 n] \[Gamma]^2+Sqrt[2] m (-M n (1+\[Gamma])+\[Lambda]))/(2 M \[Gamma] Sqrt[n (m-a M \[Gamma]^2)])]-Erf[(-2 m n+\[Gamma] (-2 a M n+Sqrt[2] b Sqrt[M n]+2 a \[Lambda]))/(2 Sqrt[2] Sqrt[n (m-a M \[Gamma]^2)])])+1/2 E^(-((4 c M n (m-a M \[Gamma]^2)+b (b M^2 n \[Gamma]^2+2 Sqrt[2] m Sqrt[M n] (M n (1+\[Gamma])-\[Lambda]))+2 a m (-M n (1+\[Gamma])+\[Lambda])^2)/(4 M n (-m+a M \[Gamma]^2)))) Sqrt[m/(m-a M \[Gamma]^2)] (-2+Erfc[(b Sqrt[M^3 n] \[Gamma]^2+Sqrt[2] m (M n (1+\[Gamma])-\[Lambda]))/(2 M \[Gamma] Sqrt[n (m-a M \[Gamma]^2)])]))/.LopezBenitezParameters[(-M (n+\[Gamma])+\[Lambda])/(2 Sqrt[M n])],
		!ListQ[diversityType] && diversityType == "SLS",
			Which[
				ListQ[\[Gamma]],
					1 - Product[1 - LargeMNNakagamiProbabilityOfDetection[M,\[Gamma][[i]],\[Lambda],m,DiversityType->"None"],{i,n}],
				!ListQ[\[Gamma]],
					1 - (1 - LargeMNNakagamiProbabilityOfDetection[M,\[Gamma],\[Lambda],m,DiversityType->"None"])^n,
				True,
					Undefined
			],
		True,
			Undefined
	];
	If[OptionValue[Timed],
		(* Evaluate result until MaxTime seconds of CPU time have been used or MaxIterations have been performed, whichever comes first *)
		While[totaltime < OptionValue[MaxTime] && iterations < OptionValue[MaxIterations],
			ClearSystemCache[];
			{time, result} = TimeConstrained[Timing[f],OptionValue[MaxTime],{OptionValue[MaxTime],Null}];
			totaltime += time;
			iterations++;
		];
		{result,totaltime/iterations},
		f//N
	]
]


(* ::Subsubsection::Closed:: *)
(*Numerical Gaussian method*)


Options[NGaussianNakagamiProbabilityOfDetection] = {DiversityType->"SLC",LowSNR->OptionValue[ProbabilityOfDetection,LowSNR],Timed->OptionValue[ProbabilityOfDetection,Timed],MaxTime->OptionValue[ProbabilityOfDetection,MaxTime],MaxIterations->OptionValue[ProbabilityOfDetection,MaxIterations]};
NGaussianNakagamiProbabilityOfDetection::usage="NGaussianNakagamiProbabilityOfDetection[M, \[Gamma], \[Lambda], m] calculates the approximate probability of detection for a single energy detector operating in a Nakagami-m fading channel using a numerical algorithm
NGaussianNakagamiProbabilityOfDetection[M, \[Gamma], \[Lambda], m, n] calculates the approximate probability of detection for a cooperative network operating in a Nakagami-m fading channel using a numerical algorithm.

A LowSNR option may be specified. By default, LowSNR\[Rule]"<>ToString[LowSNR/.Options[NGaussianNakagamiProbabilityOfDetection]]<>".

Function timing may be specified using the following options:

Timed\[Rule]"<>ToString[Timed/.Options[ProbabilityOfDetection]]<>"
MaxIterations\[Rule]"<>ToString[MaxIterations/.Options[ProbabilityOfDetection]]<>"
MaxTime\[Rule]"<>ToString[MaxTime/.Options[ProbabilityOfDetection]]<>"

where the above settings are the defaults, if not specified. If Timed\[Rule]True, then a {Pd, time} list of values will be returned.";
NGaussianNakagamiProbabilityOfDetection[M_?NumericQ,\[Gamma]_?NumericQ,\[Lambda]_,m_?NumericQ,OptionsPattern[]]:=Module[{RelevantOptions, n = 1},
	RelevantOptions[target_]:=FilterRules[Table[#[[i]]->OptionValue[#[[i]]],{i,Length[#]}]&[Options[NGaussianNakagamiProbabilityOfDetection][[All,1]]],Options[target][[All,1]]];
	NGaussianNakagamiProbabilityOfDetection[M,\[Gamma],\[Lambda],m,n,#/.(DiversityType/.#)->"None"&[RelevantOptions[NGaussianNakagamiProbabilityOfDetection]]]
]
NGaussianNakagamiProbabilityOfDetection[M_?NumericQ,\[Gamma]_?NumericQ,\[Lambda]_,m_?NumericQ,n_?IntegerQ,OptionsPattern[]]:=Module[{lim, f, totaltime = 0, iterations = 0, time, result, RelevantOptions, diversityType = OptionValue[DiversityType]},
	RelevantOptions[target_]:=FilterRules[Table[#[[i]]->OptionValue[#[[i]]],{i,Length[#]}]&[Options[NGaussianNakagamiProbabilityOfDetection][[All,1]]],Options[target][[All,1]]];
	f:=Which[
		!ListQ[diversityType] && diversityType == "None",
			NIntegrate[AWGNProbabilityOfDetection[M,x,\[Lambda],RelevantOptions[AWGNProbabilityOfDetection]]NakagamiPDF[\[Gamma],m,x,DiversityType->diversityType],{x,0,\[Infinity]}],
		!ListQ[diversityType] && diversityType == "MRC",
			NIntegrate[AWGNProbabilityOfDetection[M,x/n,\[Lambda],n,RelevantOptions[AWGNProbabilityOfDetection]]NakagamiPDF[\[Gamma],m,x,n,DiversityType->diversityType],{x,0,\[Infinity]}],
		!ListQ[diversityType] && diversityType == "EGC",
			Undefined,
		!ListQ[diversityType] && diversityType == "SC",
			NIntegrate[AWGNProbabilityOfDetection[M,x,\[Lambda],RelevantOptions[AWGNProbabilityOfDetection]]NakagamiPDF[\[Gamma],m,x,n,DiversityType->diversityType],{x,0,\[Infinity]}],
		ListQ[diversityType] && diversityType[[1]] == "SSC",
			NIntegrate[AWGNProbabilityOfDetection[M,x,\[Lambda],RelevantOptions[AWGNProbabilityOfDetection]]NakagamiPDF[\[Gamma],m,x,DiversityType->diversityType],{x,0,\[Infinity]}],
		!ListQ[diversityType] && diversityType == "SLC",
			NIntegrate[AWGNProbabilityOfDetection[M,x/n,\[Lambda],n,RelevantOptions[AWGNProbabilityOfDetection]]NakagamiPDF[\[Gamma],m,x,n],{x,0,\[Infinity]}],
		!ListQ[diversityType] && diversityType == "SLS",
			Which[
				ListQ[\[Gamma]],
					1 - Product[1 - NGaussianNakagamiProbabilityOfDetection[M,\[Gamma][[i]],\[Lambda],m,DiversityType->"None"],{i,n}],
				!ListQ[\[Gamma]],
					1 - (1 - NGaussianNakagamiProbabilityOfDetection[M,\[Gamma],\[Lambda],m,DiversityType->"None"])^n,
				True,
					Undefined
			],
		True,
			Undefined
	];
	If[OptionValue[Timed],
		(* Evaluate result until MaxTime seconds of CPU time have been used or MaxIterations have been performed, whichever comes first *)
		While[totaltime < OptionValue[MaxTime] && iterations < OptionValue[MaxIterations],
			ClearSystemCache[];
			{time, result} = TimeConstrained[Timing[f],OptionValue[MaxTime],{OptionValue[MaxTime],Null}];
			totaltime += time;
			iterations++;
		];
		{result,totaltime/iterations},
		f
	]
]


(* ::Subsection::Closed:: *)
(*Sample complexity*)


Options[NNakagamiSampleComplexity]={Method->OptionValue[SampleComplexity,Method],LowSNR->OptionValue[SampleComplexity,LowSNR],Tolerance->OptionValue[SampleComplexity,Tolerance]};
NNakagamiSampleComplexity::usage="NNakagamiSampleComplexity[\[Gamma], Pf, Pd, m] calculates the sample complexity for a single energy detector operating on a Nakagami-m fading channel.
NNakagamiSampleComplexity[\[Gamma], Pf, Pd, m, n] calculates the sample complexity for a cooperative network operating on a Nakagami-m fading channel.

The following methods can be given:

Method\[Rule]\"Approximate\"
Method\[Rule]\"Exact\"

By default, Method\[Rule]\""<>ToString[Method/.Options[NNakagamiSampleComplexity]]<>"\".

If Method\[Rule]\"Approximate\", the LowSNR option may be specified. By default, LowSNR\[Rule]"<>ToString[LowSNR/.Options[NNakagamiSampleComplexity]]<>".

Numerical tolerance can be specified using the Tolerance option. By default, Tolerance\[Rule]"<>ToString[Tolerance/.Options[NNakagamiSampleComplexity]//N//InputForm]<>".";
NNakagamiSampleComplexity::tol="The difference between the result `1` and the constraint `2` was greater than the specified tolerance `3`.";
NNakagamiSampleComplexity[\[Gamma]_?NumericQ,Pf_?NumericQ,Pd_?NumericQ,m_?NumericQ,OptionsPattern[]]:=Module[{RelevantOptions, n = 1},
	RelevantOptions[target_]:=FilterRules[Table[#[[i]]->OptionValue[#[[i]]],{i,Length[#]}]&[Options[NNakagamiSampleComplexity][[All,1]]],Options[target][[All,1]]];
	NNakagamiSampleComplexity[\[Gamma],Pf,Pd,m,n,RelevantOptions[NNakagamiSampleComplexity]]
]
NNakagamiSampleComplexity[\[Gamma]_?NumericQ,Pf_?NumericQ,Pd_?NumericQ,m_?NumericQ,n_?IntegerQ,OptionsPattern[]]:=Module[{RelevantOptions, tol = OptionValue[Tolerance], intialGuess = Max[(20 / (n m^2)), 1] * SampleComplexity[\[Gamma],Pf,Pd,n], courseGuess, fineGuess, result},
	RelevantOptions[target_]:=FilterRules[Table[#[[i]]->OptionValue[#[[i]]],{i,Length[#]}]&[Options[NNakagamiSampleComplexity][[All,1]]],Options[target][[All,1]]];
	(* Temporarily disable error checking - we'll do our own *)
	Off[FindRoot::reged,FindRoot::lstol];
	Switch[OptionValue[Method],
		"Approximate",
		(* Only use Gaussian method if it is valid *)
		If[intialGuess <= 250,
			result = NNakagamiSampleComplexity[\[Gamma],Pf,Pd,m,n,tol,Method->"Exact",Tolerance->OptionValue[Tolerance]];,
			fineGuess = M/.FindRoot[NNakagamiProbabilityOfDetection[M,\[Gamma],\[Lambda][M,Pf,n],m,n,RelevantOptions[NNakagamiProbabilityOfDetection]] == Pd, {M, intialGuess, 1, \[Infinity]}];
			result = NNakagamiProbabilityOfDetection[fineGuess,\[Gamma],\[Lambda][fineGuess,Pf,n],m,n,RelevantOptions[NNakagamiProbabilityOfDetection]];
		];,
		"Exact",
		(* If Gaussian approximation is valid, then use it to speed up the calculation *)
		If[intialGuess <= 250,
			courseGuess = intialGuess;,
			courseGuess = M/.FindRoot[NNakagamiProbabilityOfDetection[M,\[Gamma],\[Lambda][M,Pf,n],m,n,RelevantOptions[NNakagamiProbabilityOfDetection]] == Pd, {M, intialGuess, 1, \[Infinity]}];
		];
		fineGuess = M/.FindRoot[NNakagamiProbabilityOfDetection[courseGuess,\[Gamma],\[Lambda][courseGuess,Pf,n,RelevantOptions[\[Lambda]]],m,n] == Pd, {M, courseGuess, 1, \[Infinity]}];
		result = NNakagamiProbabilityOfDetection[fineGuess,\[Gamma],\[Lambda][fineGuess,Pf,n,RelevantOptions[\[Lambda]]],m,n];
	];
	On[FindRoot::reged,FindRoot::lstol];
	If[Abs[result - Pd] <= tol//TrueQ,
		fineGuess,
		Message[NNakagamiSampleComplexity::tol, result//N, Pd//N, tol//N]
	]
]


(* ::Subsection::Closed:: *)
(*Miscellaenous*)


MultinomialCoefficient[l_,k_,m_]:=Which[
	k == 0,
		1,
	k == 1,
		l,
	k == (m - 1) l,
		1 / (Gamma[m]^l),
	2 <= k <= (m - 1) l - 1,
		(1 / k) NSum[MultinomialCoefficient[k - j, l] (j (l + 1) - k) / j!, {j, 1, Min[k, m - 1]}],
	True,
		Undefined
]


End[];


EndPackage[];
