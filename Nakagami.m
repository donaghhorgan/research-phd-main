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
(*09/10/2012*)
(*1.41*)


(* ::Subsection::Closed:: *)
(*Changelog*)


(* ::Text:: *)
(*Version 1.41: Added large SNR approximation method for SLC and no diversity types.*)
(*Version 1.4: Added an exact numeric method, minor bug fixes and retitled some methods.*)
(*Version 1.32: Added SC and SSC diversity to the IntegerMN method.*)
(*Version 1.31: Filled out all known diversity types for each exact method.*)
(*Version 1.3: Added MRC, EGC, SLC, SSC, SC and SLS diversity cases to all methods.*)
(*Version 1.2: Split Horgan's approximation into separate IntegerMN and LargeMN functions, so that full functionality can be accessed through the NakagamiProbabilityOfDetection interface.*)
(*Version 1.11: Moved database logging functions to the Network package.*)
(*Version 1.1: Introduced RelevantOptions function and changed function definitions, so that child options are inherited from parents. The Gaussian approximation method is now called Horgan's approximation to avoid confusion with the numerical Gaussian method.*)
(*Version 1.0: First working version, minor bug fixes to follow.*)


(* ::Section:: *)
(*Public*)


Protect[DiversityType, Algorithm, LowSNR, Timed, MaxTime, Resolution];


BeginPackage["Nakagami`"]; 


(* ::Subsection::Closed:: *)
(*PDF of the signal to noise ratio*)


NakagamiPDF;


(* ::Subsection:: *)
(*Detection probability*)


(* ::Subsubsection::Closed:: *)
(*Main function*)


NakagamiProbabilityOfDetection;


(* ::Subsubsection::Closed:: *)
(*Annamalai' s method*)


AnnamalaiLimit;
AnnamalaiNakagamiProbabilityOfDetection;


(* ::Subsubsection::Closed:: *)
(*Digham' s method*)


DighamNakagamiProbabilityOfDetection;


(* ::Subsubsection::Closed:: *)
(*Herath' s method*)


HerathLimit;
HerathNakagamiProbabilityOfDetection;


(* ::Subsubsection::Closed:: *)
(*Sun' s method*)


SunLimit;
SunNakagamiProbabilityOfDetection;


(* ::Subsubsection::Closed:: *)
(*Numerical method*)


NumericalNakagamiProbabilityOfDetection;


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
					Which[
						ListQ[\[Gamma]],
							Undefined,
						!ListQ[\[Gamma]],
							PDF[GammaDistribution[m, \[Gamma] / m], x],
						True,
							Undefined
					],
				!ListQ[diversityType] && diversityType == "MRC",
					Which[
						ListQ[\[Gamma]],
							PDF[GammaDistribution[m n, Mean[\[Gamma]] / m], x],
						!ListQ[\[Gamma]],
							PDF[GammaDistribution[m n, \[Gamma] / m], x],
						True,
							Undefined
					],
				!ListQ[diversityType] && diversityType == "EGC",
					Which[
						n == 2,
							(2 Sqrt[\[Pi]] x^(2m - 1) Exp[-2m x / \[Gamma]] / (2^(4m - 1))) (Gamma[2m] / (Gamma[m]^2 Gamma[2m + (1 / 2)])) (2m / \[Gamma])^(2m) Hypergeometric1F1[2m, 2m + (1 / 2), m x / \[Gamma]],
						n == 3,
							(4 Sqrt[\[Pi]] Gamma[2m] Exp[-3m x / \[Gamma]] / (Gamma[m]^3 2^(4m - 1))) Sum[((Gamma[2m+l] Gamma[4m+2l]) / (Gamma[2m+l+1/2] Gamma[6m+2l] Gamma[l+1] 2^l)) x^(3m+l-1) (3m / \[Gamma])^(3m+l) HypergeometricPFQ[{2m,4m+2l},{3m+l+1/2,3m+l},3m x / (2\[Gamma])],{l,0,\[Infinity]}],
						True,
							Undefined
					],
				!ListQ[diversityType] && diversityType == "SC",
					Which[
						ListQ[\[Gamma]],
							(n / Gamma[m]) Sum[(-1)^(l) Binomial[n - 1, l] Sum[MultinomialCoefficient[l, k, m] (m / Max[\[Gamma]])^(m + k) x^(m + k - 1) Exp[-m (l + 1) x / Max[\[Gamma]]],{k, 0, l (m - 1)}],{l, 0, n - 1}],
						!ListQ[\[Gamma]],
							(n / Gamma[m]) Sum[(-1)^(l) Binomial[n - 1, l] Sum[MultinomialCoefficient[l, k, m] (m / \[Gamma])^(m + k) x^(m + k - 1) Exp[-m (l + 1) x / \[Gamma]],{k, 0, l (m - 1)}],{l, 0, n - 1}],
						True,
							Undefined
					],
				ListQ[diversityType] && diversityType[[1]] == "SSC",
					Module[{\[Gamma]t = diversityType[[2]]},
						Which[
							x < \[Gamma]t,
								Which[
									ListQ[\[Gamma]],
										(1 - GammaRegularized[m, m \[Gamma]t / Mean[\[Gamma]]])PDF[GammaDistribution[m, Mean[\[Gamma]] / m], x],
									!ListQ[\[Gamma]],
										(1 - GammaRegularized[m, m \[Gamma]t / \[Gamma]])PDF[GammaDistribution[m, \[Gamma] / m], x],
									True,
										Undefined
								],
							x >= \[Gamma]t,
								Which[
									ListQ[\[Gamma]],
										(2 - GammaRegularized[m, m \[Gamma]t / Mean[\[Gamma]]])PDF[GammaDistribution[m, Mean[\[Gamma]] / m], x],
									!ListQ[\[Gamma]],
										(2 - GammaRegularized[m, m \[Gamma]t / \[Gamma]])PDF[GammaDistribution[m, \[Gamma] / m], x],
									True,
										Undefined
								],
							True,
								Undefined
						]
					],
				!ListQ[diversityType] && diversityType == "SLC",
					Which[
						ListQ[\[Gamma]],
							PDF[GammaDistribution[m n, Mean[\[Gamma]] / m], x],
						!ListQ[\[Gamma]],
							PDF[GammaDistribution[m n, \[Gamma] / m], x],
						True,
							Undefined
					],
				!ListQ[diversityType] && diversityType == "SLS",
					Which[
						ListQ[\[Gamma]],
							Undefined,
						!ListQ[\[Gamma]],
							PDF[GammaDistribution[m, \[Gamma] / m], x],
						True,
							Undefined
					],
				True,
					Undefined
			],
		method == "Approximate",
			Which[
				!ListQ[diversityType] && diversityType == "None",
					Which[
						ListQ[\[Gamma]],
							Undefined,
						!ListQ[\[Gamma]],
							PDF[NormalDistribution[\[Gamma], Sqrt[\[Gamma]^2 / m]], x],
						True,
							Undefined
					],
				!ListQ[diversityType] && diversityType == "MRC",
					Which[
						ListQ[\[Gamma]],
							PDF[NormalDistribution[n Mean[\[Gamma]], Sqrt[n Mean[\[Gamma]]^2 / m]], x],
						!ListQ[\[Gamma]],
							PDF[NormalDistribution[n \[Gamma], Sqrt[n \[Gamma]^2 / m]], x],
						True,
							Undefined
					],
				!ListQ[diversityType] && diversityType == "EGC",
					Undefined,
				!ListQ[diversityType] && diversityType == "SC",
					Which[
						ListQ[\[Gamma]],
							n PDF[NormalDistribution[Max[\[Gamma]], Sqrt[Max[\[Gamma]]^2 / m]], x] (CDF[NormalDistribution[2 m, 2 Sqrt[m]],2 m x / Max[\[Gamma]]]^(n - 1)),
						!ListQ[\[Gamma]],
							n PDF[NormalDistribution[\[Gamma], Sqrt[\[Gamma]^2 / m]], x] (CDF[NormalDistribution[2 m, 2 Sqrt[m]],2 m x / \[Gamma]]^(n - 1)),
						True,
							Undefined
					],
				ListQ[diversityType] && diversityType[[1]] == "SSC",
					Module[{\[Gamma]t = diversityType[[2]]},
						Which[
							x < \[Gamma]t,
								Which[
									ListQ[\[Gamma]],
										(1 - GammaRegularized[m, m \[Gamma]t / Mean[\[Gamma]]])PDF[NormalDistribution[Mean[\[Gamma]], Sqrt[Mean[\[Gamma]]^2 / m]], x],
									!ListQ[\[Gamma]],
										(1 - GammaRegularized[m, m \[Gamma]t / \[Gamma]])PDF[NormalDistribution[\[Gamma], Sqrt[\[Gamma]^2 / m]], x],
									True,
										Undefined
								],
							x >= \[Gamma]t,
								Which[
									ListQ[\[Gamma]],
										(2 - GammaRegularized[m, m \[Gamma]t / Mean[\[Gamma]]])PDF[NormalDistribution[Mean[\[Gamma]], Sqrt[Mean[\[Gamma]]^2 / m]], x],
									!ListQ[\[Gamma]],
										(2 - GammaRegularized[m, m \[Gamma]t / \[Gamma]])PDF[NormalDistribution[\[Gamma], Sqrt[\[Gamma]^2 / m]], x],
									True,
										Undefined
								],
							True,
								Undefined
						]
					],
				!ListQ[diversityType] && diversityType == "SLC",
					Which[
						ListQ[\[Gamma]],
							PDF[NormalDistribution[n Mean[\[Gamma]], Sqrt[n Mean[\[Gamma]]^2 / m]], x],
						!ListQ[\[Gamma]],
							PDF[NormalDistribution[n \[Gamma], Sqrt[n \[Gamma]^2 / m]], x],
						True,
							Undefined
					],
				!ListQ[diversityType] && diversityType == "SLS",
					Which[
						ListQ[\[Gamma]],
							Undefined,
						!ListQ[\[Gamma]],
							PDF[NormalDistribution[\[Gamma], Sqrt[\[Gamma]^2 / m]], x],
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


(* ::Subsection:: *)
(*Detection probability*)


(* ::Subsubsection::Closed:: *)
(*Main function*)


Options[NakagamiProbabilityOfDetection]={Method->OptionValue[ProbabilityOfDetection,Method],Algorithm->OptionValue[ProbabilityOfDetection,Algorithm],LowSNR->OptionValue[ProbabilityOfDetection,LowSNR],DiversityType->"SLC",Timed->OptionValue[ProbabilityOfDetection,Timed],MaxTime->OptionValue[ProbabilityOfDetection,MaxTime],MaxIterations->OptionValue[ProbabilityOfDetection,MaxIterations]};
NakagamiProbabilityOfDetection::usage="NakagamiProbabilityOfDetection[M, \[Gamma], \[Lambda], m] calculates the probability of detection for a single energy detector operating on a Nakagami-m fading channel.
NakagamiProbabilityOfDetection[M, \[Gamma], \[Lambda], m, n] calculates the probability of detection for the fusion center of a cooperative network operating on a Nakagami-m fading channel.

The following methods can be given:

Method\[Rule]\"Approximate\"
Method\[Rule]{\"Approximate\", Algorithm\[Rule]...}
Method\[Rule]\"Exact\"
Method\[Rule]{\"Exact\", Algorithm\[Rule]...}

By default, Method\[Rule]\""<>ToString[Method/.Options[NakagamiProbabilityOfDetection]]<>"\".

For a given method, an algorithm may be specified. If Method\[Rule]\"Approximate\", then the following algorithms may be specified:

Algorithm\[Rule]\"IntegerMN\"
Algorithm\[Rule]\"LargeMN\"
Algorithm\[Rule]{\"SwitchedMN\", SwitchingPoint}
Algorithm\[Rule]\"NGaussian\"

By default, Algorithm\[Rule]\""<>ToString[Algorithm/.Options[NakagamiProbabilityOfDetection]]<>"\". If Algorithm\[Rule]\"SwitchedMN\", then the switching point between the IntegerMN and LargeMN algorithms may be specified using the SwitchingPoint option. If Algorithm\[Rule]\"NGaussian\", then a LowSNR boolean option may also be specified so that Method\[Rule]{\"Approximate\", Algorithm\[Rule]\"NGaussian\", LowSNR->"<>ToString[LowSNR/.Options[NakagamiProbabilityOfDetection]]<>"}. By default, LowSNR\[Rule]"<>ToString[LowSNR/.Options[NakagamiProbabilityOfDetection]]<>".

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
NakagamiProbabilityOfDetection[M_,\[Gamma]_,\[Lambda]_,m_,OptionsPattern[]]:=Module[{n = 1, RelevantOptions},
	RelevantOptions[target_]:=FilterRules[Table[#[[i]]->OptionValue[#[[i]]],{i,Length[#]}]&[Options[NakagamiProbabilityOfDetection][[All,1]]],Options[target][[All,1]]];
	NakagamiProbabilityOfDetection[M,\[Gamma],\[Lambda],m,n,#/.(DiversityType/.#)->"None"&[RelevantOptions[NakagamiProbabilityOfDetection]]]
]
NakagamiProbabilityOfDetection[M_,\[Gamma]_,\[Lambda]_,m_,n_,OptionsPattern[]]:=Module[{RelevantOptions, method = OptionValue[Method], algorithm = OptionValue[Algorithm]},
	RelevantOptions[target_]:=FilterRules[Table[#[[i]]->OptionValue[#[[i]]],{i,Length[#]}]&[Options[NakagamiProbabilityOfDetection][[All,1]]],Options[target][[All,1]]];
	Which[
		method == "Exact",
			Which[
				algorithm == "Annamalai",
					AnnamalaiNakagamiProbabilityOfDetection[M,\[Gamma],\[Lambda],m,n,RelevantOptions[AnnamalaiNakagamiProbabilityOfDetection]],
				algorithm == "Digham",
					DighamNakagamiProbabilityOfDetection[M,\[Gamma],\[Lambda],m,n,RelevantOptions[DighamNakagamiProbabilityOfDetection]],
				algorithm == "Herath",
					HerathNakagamiProbabilityOfDetection[M,\[Gamma],\[Lambda],m,n,RelevantOptions[HerathNakagamiProbabilityOfDetection]],
				algorithm == "Sun",
					SunNakagamiProbabilityOfDetection[M,\[Gamma],\[Lambda],m,n,RelevantOptions[HerathNakagamiProbabilityOfDetection]],
				algorithm == "Numerical",
					NumericalNakagamiProbabilityOfDetection[M,\[Gamma],\[Lambda],m,n,RelevantOptions[NumericalNakagamiProbabilityOfDetection]],
				True,
					Undefined
			],
		method == "Approximate",
			Which[
				!ListQ[algorithm] && algorithm == "IntegerMN",
					IntegerMNNakagamiProbabilityOfDetection[M,\[Gamma],\[Lambda],m,n,RelevantOptions[IntegerMNNakagamiProbabilityOfDetection]],
				!ListQ[algorithm] && algorithm == "LargeSNR",
					LargeSNRNakagamiProbabilityOfDetection[M,\[Gamma],\[Lambda],m,n,RelevantOptions[LargeSNRNakagamiProbabilityOfDetection]],
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
AnnamalaiLimit[M_?NumericQ,\[Gamma]_,\[Lambda]_,m_?NumericQ,OptionsPattern[]]:=Module[{n = 1},AnnamalaiLimit[M,\[Gamma],\[Lambda],m,n,DiversityType->"None",Tolerance->OptionValue[Tolerance]]]
AnnamalaiLimit[M_?NumericQ,\[Gamma]_,\[Lambda]_,m_?NumericQ,n_?IntegerQ,OptionsPattern[]]:=Module[{j, j0, tol = OptionValue[Tolerance], diversityType = OptionValue[DiversityType]},
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


Options[AnnamalaiNakagamiProbabilityOfDetection]={DiversityType->"SLC",Timed->OptionValue[ProbabilityOfDetection,Timed],MaxTime->OptionValue[ProbabilityOfDetection,MaxTime],MaxIterations->OptionValue[ProbabilityOfDetection,MaxIterations]};
AnnamalaiNakagamiProbabilityOfDetection::usage="AnnamalaiNakagamiProbabilityOfDetection[M, \[Gamma], \[Lambda], m, lim] calculates the exact probability of detection for a single energy detector operating in a Nakagami-m fading channel using Annamalai's algorithm.
AnnamalaiNakagamiProbabilityOfDetection[M, \[Gamma], \[Lambda], m, n, lim] calculates the exact probability of detection for a cooperative network operating in a Nakagami-m fading channel using Annamalai's algorithm.

Function timing may be specified using the following options:

Timed\[Rule]"<>ToString[Timed/.Options[ProbabilityOfDetection]]<>"
MaxIterations\[Rule]"<>ToString[MaxIterations/.Options[ProbabilityOfDetection]]<>"
MaxTime\[Rule]"<>ToString[MaxTime/.Options[ProbabilityOfDetection]]<>"

where the above settings are the defaults, if not specified. If Timed\[Rule]True, then a {Pd, time} list of values will be returned.";
AnnamalaiNakagamiProbabilityOfDetection[M_?NumericQ,\[Gamma]_,\[Lambda]_,m_?NumericQ,OptionsPattern[]]:=Module[{RelevantOptions, n = 1},
	RelevantOptions[target_]:=FilterRules[Table[#[[i]]->OptionValue[#[[i]]],{i,Length[#]}]&[Options[AnnamalaiNakagamiProbabilityOfDetection][[All,1]]],Options[target][[All,1]]];
	AnnamalaiNakagamiProbabilityOfDetection[M,\[Gamma],\[Lambda],m,n,#/.(DiversityType/.#)->"None"&[RelevantOptions[AnnamalaiNakagamiProbabilityOfDetection]]]
]
AnnamalaiNakagamiProbabilityOfDetection[M_?NumericQ,\[Gamma]_,\[Lambda]_,m_?NumericQ,n_?IntegerQ,OptionsPattern[]]:=Module[{lim, f, totaltime = 0, iterations = 0, time, result, diversityType = OptionValue[DiversityType]},
	lim = AnnamalaiLimit[M,\[Gamma],\[Lambda],m,n,DiversityType->diversityType];
	f:=Which[
		!ListQ[diversityType] && diversityType == "None",
			Which[
				ListQ[\[Gamma]],
					Undefined,
				!ListQ[\[Gamma]],
					1 - ((m / (m + (M / 2) \[Gamma]))^m) (1 - GammaRegularized[M / 2, \[Lambda] / 2]) - Total[Table[(Gamma[m + k] / (Gamma[m] Gamma[k+1])) ((m / (m + (M / 2) \[Gamma]))^m) ((((M / 2) \[Gamma]) / (m + (M / 2) \[Gamma]))^k) (1 - GammaRegularized[M / 2 + k, \[Lambda] / 2]),{k, 1, lim}]],
				True,
					Undefined
			],
		!ListQ[diversityType] && diversityType == "MRC",
			Which[
				ListQ[\[Gamma]],
					1 - (2m / (2m + M Mean[\[Gamma]]))^(m n) (1 - GammaRegularized[M / 2, \[Lambda] / 2]) - Total[Table[Gamma[m n+k]/(Gamma[m n]Gamma[k+1]) (m/(m+M/2 Mean[\[Gamma]]))^(m n) (((M/2 Mean[\[Gamma]])/(m+M/2 Mean[\[Gamma]]))^k) (1-GammaRegularized[M / 2 + k, \[Lambda] / 2]),{k,1,lim}]],
				!ListQ[\[Gamma]],
					1 - (2m / (2m + M \[Gamma]))^(m n) (1 - GammaRegularized[M / 2, \[Lambda] / 2]) - Total[Table[Gamma[m n+k]/(Gamma[m n]Gamma[k+1]) (m/(m+M/2 \[Gamma]))^(m n) (((M/2 \[Gamma])/(m+M/2 \[Gamma]))^k) (1-GammaRegularized[M / 2 + k, \[Lambda] / 2]),{k,1,lim}]],
				True,
					Undefined
			],
		!ListQ[diversityType] && diversityType == "EGC",
			Undefined,
		!ListQ[diversityType] && diversityType == "SC",
			Undefined,
		ListQ[diversityType] && diversityType[[1]] == "SSC",
			Undefined,
		!ListQ[diversityType] && diversityType == "SLC",
			Which[
				ListQ[\[Gamma]],
					1 - (m / (m + (M / 2) Mean[\[Gamma]]))^(m n) (1 - GammaRegularized[M n/2, \[Lambda] / 2]) - Total[Table[Gamma[m n+k]/(Gamma[m n]Gamma[k+1]) (m/(m+M/2 Mean[\[Gamma]]))^(m n) (((M/2 Mean[\[Gamma]])/(m+M/2 Mean[\[Gamma]]))^k) (1-GammaRegularized[M n/2+k, \[Lambda] / 2]),{k,1,lim}]],
				!ListQ[\[Gamma]],
					1 - (m / (m + (M / 2) \[Gamma]))^(m n) (1 - GammaRegularized[M n/2, \[Lambda] / 2]) - Total[Table[Gamma[m n+k]/(Gamma[m n]Gamma[k+1]) (m/(m+M/2 \[Gamma]))^(m n) (((M/2 \[Gamma])/(m+M/2 \[Gamma]))^k) (1-GammaRegularized[M n/2+k, \[Lambda] / 2]),{k,1,lim}]],
				True,
					Undefined
			],
		!ListQ[diversityType] && diversityType == "SLS",
			Which[
				ListQ[\[Gamma]],
					1 - Product[1 - AnnamalaiNakagamiProbabilityOfDetection[M,\[Gamma][[i]],\[Lambda],m,DiversityType->"None"],{i,n}],
				!ListQ[\[Gamma]],
					1 - (1 - AnnamalaiNakagamiProbabilityOfDetection[M,\[Gamma],\[Lambda],m,DiversityType->"None"])^n,
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


Options[DighamNakagamiProbabilityOfDetection]={DiversityType->"SLC",Timed->OptionValue[ProbabilityOfDetection,Timed],MaxTime->OptionValue[ProbabilityOfDetection,MaxTime],MaxIterations->OptionValue[ProbabilityOfDetection,MaxIterations]};
DighamNakagamiProbabilityOfDetection::usage="DighamNakagamiProbabilityOfDetection[M, \[Gamma], \[Lambda], m] calculates the exact probability of detection for a single energy detector operating in a Nakagami-m fading channel using Digham's algorithm.
DighamNakagamiProbabilityOfDetection[M, \[Gamma], \[Lambda], m, n] calculates the exact probability of detection for a cooperative network operating in a Nakagami-m fading channel using Digham's algorithm.

Function timing may be specified using the following options:

Timed\[Rule]"<>ToString[Timed/.Options[ProbabilityOfDetection]]<>"
MaxIterations\[Rule]"<>ToString[MaxIterations/.Options[ProbabilityOfDetection]]<>"
MaxTime\[Rule]"<>ToString[MaxTime/.Options[ProbabilityOfDetection]]<>"

where the above settings are the defaults, if not specified. If Timed\[Rule]True, then a {Pd, time} list of values will be returned.";
DighamNakagamiProbabilityOfDetection[M_?NumericQ,\[Gamma]_,\[Lambda]_,m_?NumericQ,OptionsPattern[]]:=Module[{RelevantOptions, n = 1},
	RelevantOptions[target_]:=FilterRules[Table[#[[i]]->OptionValue[#[[i]]],{i,Length[#]}]&[Options[DighamNakagamiProbabilityOfDetection][[All,1]]],Options[target][[All,1]]];
	DighamNakagamiProbabilityOfDetection[M,\[Gamma],\[Lambda],m,n,RelevantOptions[DighamNakagamiProbabilityOfDetection]]
]
DighamNakagamiProbabilityOfDetection[M_?NumericQ,\[Gamma]_,\[Lambda]_,m_?NumericQ,n_?IntegerQ,OptionsPattern[]]:=Module[{lim, f, totaltime = 0, iterations = 0, time, result, diversityType = OptionValue[DiversityType], RelevantOptions},
	RelevantOptions[target_]:=FilterRules[Table[#[[i]]->OptionValue[#[[i]]],{i,Length[#]}]&[Options[DighamNakagamiProbabilityOfDetection][[All,1]]],Options[target][[All,1]]];
	f:=Which[
		!ListQ[diversityType] && diversityType == "None",
			Which[
				ListQ[\[Gamma]],
					Undefined,
				!ListQ[\[Gamma]],
					Module[{A1, \[Beta]},
						A1 = Exp[-\[Lambda] \[Beta] / (2 m)] (\[Beta]^(m - 1) LaguerreL[m - 1, -\[Lambda] (1 - \[Beta]) / 2] + (1 - \[Beta]) Total[Table[\[Beta]^i LaguerreL[i,-\[Lambda] (1 - \[Beta]) / 2], {i, 0, m - 2}]]);
						\[Beta] = (2m) / (2m + M \[Gamma]);
						A1 + \[Beta]^(m) Exp[-\[Lambda] / 2] Total[Table[((\[Lambda] / 2)^i / i!) Hypergeometric1F1[m, i + 1, \[Lambda] (1 - \[Beta]) / 2], {i, 1, (M / 2) - 1}]]
					],
				True,
					Undefined
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
			Which[
				ListQ[\[Gamma]],
					Module[{A1, \[Beta]},
						A1 = Exp[-\[Lambda] \[Beta] / (2 m n)] (\[Beta]^(m n - 1) LaguerreL[m n - 1, -\[Lambda] (1 - \[Beta]) / 2] + (1 - \[Beta]) Total[Table[\[Beta]^i LaguerreL[i,-\[Lambda] (1 - \[Beta]) / 2], {i, 0, m n - 2}]]);
						\[Beta] = (2m) / (2m + M Mean[\[Gamma]]);
						A1 + \[Beta]^(m n) Exp[-\[Lambda] / 2] Total[Table[((\[Lambda] / 2)^i / i!) Hypergeometric1F1[m n, i + 1, \[Lambda] (1 - \[Beta]) / 2], {i, 1, (M n / 2) - 1}]]
					],
				!ListQ[\[Gamma]],
					Module[{A1, \[Beta]},
						A1 = Exp[-\[Lambda] \[Beta] / (2 m n)] (\[Beta]^(m n - 1) LaguerreL[m n - 1, -\[Lambda] (1 - \[Beta]) / 2] + (1 - \[Beta]) Total[Table[\[Beta]^i LaguerreL[i,-\[Lambda] (1 - \[Beta]) / 2], {i, 0, m n - 2}]]);
						\[Beta] = (2m) / (2m + M \[Gamma]);
						A1 + \[Beta]^(m n) Exp[-\[Lambda] / 2] Total[Table[((\[Lambda] / 2)^i / i!) Hypergeometric1F1[m n, i + 1, \[Lambda] (1 - \[Beta]) / 2], {i, 1, (M n / 2) - 1}]]
					],
				True,
					Undefined
			],
		!ListQ[diversityType] && diversityType == "SLS",
			Which[
				ListQ[\[Gamma]],
					1 - Product[1 - DighamNakagamiProbabilityOfDetection[M,\[Gamma][[i]],\[Lambda],m,DiversityType->"None"],{i,n}],
				!ListQ[\[Gamma]],
					1 - (1 - DighamNakagamiProbabilityOfDetection[M,\[Gamma],\[Lambda],m,DiversityType->"None"])^n,
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
HerathLimit[M_?NumericQ,\[Gamma]_,\[Lambda]_,m_?NumericQ,OptionsPattern[]]:=Module[{n = 1},HerathLimit[M,\[Gamma],\[Lambda],m,n,Tolerance->OptionValue[Tolerance]]]
HerathLimit[M_?NumericQ,\[Gamma]_,\[Lambda]_,m_?NumericQ,n_?IntegerQ,OptionsPattern[]]:=Module[{j, j0, tol = OptionValue[Tolerance], diversityType = OptionValue[DiversityType], \[Psi]},
	Which[
		!ListQ[diversityType] && diversityType == "None",
			j0 = (\[Lambda] / 2) - 1 - InverseQ[1 - tol];
			Which[
				ListQ[\[Gamma]],
					Undefined,
				!ListQ[\[Gamma]],
					j/.FindRoot[(m / ((M / 2) \[Gamma] + m))^(m) Hypergeometric1F1[m, j + 1, \[Lambda] (M / 2) \[Gamma] / (2 ((M / 2) \[Gamma] + m))] (1 - GammaRegularized[j + 1, \[Lambda] / 2]) == tol,{j, j0, 1, \[Infinity]}],
				True,
					Undefined
			],
		!ListQ[diversityType] && diversityType == "MRC",
			j0 = (\[Lambda] / 2) - 1 - InverseQ[1 - tol];
			Which[
				ListQ[\[Gamma]],
					j/.FindRoot[(m / ((M / 2) Mean[\[Gamma]] + m))^(m) Hypergeometric1F1[m, j + 1, \[Lambda] (M / 2) Mean[\[Gamma]] / (2 ((M / 2) Mean[\[Gamma]] + m))] (1 - GammaRegularized[j + 1, \[Lambda] / 2]) == tol,{j, j0, 1, \[Infinity]}],
				!ListQ[\[Gamma]],
					j/.FindRoot[(m / ((M / 2) \[Gamma] + m))^(m) Hypergeometric1F1[m, j + 1, \[Lambda] (M / 2) \[Gamma] / (2 ((M / 2) \[Gamma] + m))] (1 - GammaRegularized[j + 1, \[Lambda] / 2]) == tol,{j, j0, 1, \[Infinity]}],
				True,
					Undefined
			],
		!ListQ[diversityType] && diversityType == "EGC",
			j0 = Max[M / 2, (\[Lambda] / 2) - 1 - InverseQ[1 - tol]];
			(* \[Psi][\[Alpha]_,\[Beta]_,\[Gamma]0_,\[Gamma]1_,x_,y_]:=(1-y)^-\[Alpha] HypergeometricPFQ[{\[Alpha],\[Beta]},{\[Gamma]0,\[Gamma]1},-(x/(-1+y))]; *)
			\[Psi][\[Alpha]_,\[Beta]_,\[Gamma]0_,\[Gamma]1_,x_,y_]:=NSum[Pochhammer[\[Alpha],m0+n0] Pochhammer[\[Beta],m0] / (Pochhammer[\[Gamma]0, m0] Pochhammer[\[Gamma]1, n0]) (x^m0 / m0!) (y^n0 / n0!),{m0,0,\[Infinity]},{n0,0,\[Infinity]}];
			Which[
				ListQ[\[Gamma]],
					Which[
						n == 2 && Length[\[Gamma]] == 2,
							j/.FindRoot[(Gamma[2m]^2 Gamma[1 / 2] / (Gamma[m]^(2) Gamma[2m + (1 / 2)] 2^(2m - 2))) (m / ((M / 2) Mean[\[Gamma]] + 2m))^(2m) \[Psi][2m, 2m, 2m + 1 / 2, j + 1, m / ((M / 2) Mean[\[Gamma]] + 2m), \[Lambda] (M / 2) Mean[\[Gamma]] / (2 ((M / 2) Mean[\[Gamma]] + 2m))] (1 - GammaRegularized[j + 1, \[Lambda] / 2]) == tol, {j, j0, M / 2, \[Infinity]}],
						n == 3 && Length[\[Gamma]] == 3,
							{100, 100, M / 2 + 100},
						n >= 4 && Length[\[Gamma]] == n,
							Undefined,
						True,
							Undefined
					],
				!ListQ[\[Gamma]],
					Which[
						n == 2,
							j/.FindRoot[(Gamma[2m]^2 Gamma[1 / 2] / (Gamma[m]^(2) Gamma[2m + (1 / 2)] 2^(2m - 2))) (m / ((M / 2) \[Gamma] + 2m))^(2m) \[Psi][2m, 2m, 2m + 1 / 2, j + 1, m / ((M / 2) \[Gamma] + 2m), \[Lambda] (M / 2) \[Gamma] / (2 ((M / 2) \[Gamma] + 2m))] (1 - GammaRegularized[j + 1, \[Lambda] / 2]) == tol, {j, j0, M / 2, \[Infinity]}],
						n == 3,
							{100, 100, M / 2 + 100},
						n >= 4,
							Undefined,
						True,
							Undefined
					],
				True,
					Undefined
			],
		!ListQ[diversityType] && diversityType == "SC",
			Undefined,
			(*j0 = (\[Lambda] / 2) - 1 - InverseQ[1 - tol];
			Print[j0];
			j/.FindRoot[n (m / \[Gamma])^(m) (1 - GammaRegularized[j + 1, \[Lambda] / 2]) Total[Table[Binomial[n - 1, k] Total[Table[MultinomialCoefficient[k, i, m] Pochhammer[m, i] (\[Gamma] / (\[Gamma] + m (k + 1)))^(i + m) Hypergeometric1F1[i + m, j + 1, \[Lambda] \[Gamma] / (2 (\[Gamma] + m (k + 1)))],{i, 0, k*(m - 1)}]],{k, 0, n - 1}]] == tol,{j, j0, 1, \[Infinity]}]*)
		ListQ[diversityType] && diversityType[[1]] == "SSC",
			j0 = {(\[Lambda] / 2) - 1 - InverseQ[1 - tol], Null};
			Which[
				ListQ[\[Gamma]],
					Module[{\[Gamma]t = diversityType[[2]]},
						{j/.FindRoot[(1 - GammaRegularized[m, m (M / 2) \[Gamma]t / ((M / 2) Mean[\[Gamma]])]) Hypergeometric1F1[m, j + 1, \[Lambda] (M / 2) Mean[\[Gamma]] / (2 ((M / 2) Mean[\[Gamma]] + m))] (1 - GammaRegularized[j + 1, \[Lambda] / 2]) (m / (m + (M / 2) Mean[\[Gamma]]))^(m) == tol,{j, j0[[1]], 1, \[Infinity]}], InverseCDF[NegativeBinomialDistribution[m, m / ((M / 2) Mean[\[Gamma]] + m)], 1 - tol]}
					],
				!ListQ[\[Gamma]],
					Module[{\[Gamma]t = diversityType[[2]]},
						{j/.FindRoot[(1 - GammaRegularized[m, m (M / 2) \[Gamma]t / ((M / 2) \[Gamma])]) Hypergeometric1F1[m, j + 1, \[Lambda] (M / 2) \[Gamma] / (2 ((M / 2) \[Gamma] + m))] (1 - GammaRegularized[j + 1, \[Lambda] / 2]) (m / (m + (M / 2) \[Gamma]))^(m) == tol,{j, j0[[1]], 1, \[Infinity]}], InverseCDF[NegativeBinomialDistribution[m, m / ((M / 2) \[Gamma] + m)], 1 - tol]}
					],
				True,
					Undefined
			],
		!ListQ[diversityType] && diversityType == "SLC",
			j0 = (\[Lambda] / 2) - 1 - InverseQ[1 - tol];
			Which[
				ListQ[\[Gamma]],
					j/.FindRoot[(m / ((M / 2) Mean[\[Gamma]] + m))^(m n) Hypergeometric1F1[m n, j + 1, \[Lambda] (M / 2) Mean[\[Gamma]] / (2 ((M / 2) Mean[\[Gamma]] + m))] (1 - GammaRegularized[j + 1, \[Lambda] / 2]) == tol,{j, j0, 1, \[Infinity]}],
				!ListQ[\[Gamma]],
					j/.FindRoot[(m / ((M / 2) \[Gamma] + m))^(m n) Hypergeometric1F1[m n, j + 1, \[Lambda] (M / 2) \[Gamma] / (2 ((M / 2) \[Gamma] + m))] (1 - GammaRegularized[j + 1, \[Lambda] / 2]) == tol,{j, j0, 1, \[Infinity]}],
				True,
					Undefined
			],
		!ListQ[diversityType] && diversityType == "SLS",
			j0 = (\[Lambda] / 2) - 1 - InverseQ[1 - tol];
			Which[
				ListQ[\[Gamma]],
					j/.FindRoot[(m / ((M / 2) Mean[\[Gamma]] + m))^(m) Hypergeometric1F1[m, j + 1, \[Lambda] (M / 2) Mean[\[Gamma]] / (2 ((M / 2) Mean[\[Gamma]] + m))] (1 - GammaRegularized[j + 1, \[Lambda] / 2]) == tol,{j, M / 2}],
				!ListQ[\[Gamma]],
					j/.FindRoot[(m / ((M / 2) \[Gamma] + m))^(m) Hypergeometric1F1[m, j + 1, \[Lambda] (M / 2) \[Gamma] / (2 ((M / 2) \[Gamma] + m))] (1 - GammaRegularized[j + 1, \[Lambda] / 2]) == tol,{j, M / 2}],
				True,
					Undefined
			],
		True,
			Undefined
	]//Ceiling
]


Options[HerathNakagamiProbabilityOfDetection]={DiversityType->"SLC",Timed->OptionValue[ProbabilityOfDetection,Timed],MaxTime->OptionValue[ProbabilityOfDetection,MaxTime],MaxIterations->OptionValue[ProbabilityOfDetection,MaxIterations]};
HerathNakagamiProbabilityOfDetection::usage="HerathNakagamiProbabilityOfDetection[M, \[Gamma], \[Lambda], m] calculates the exact probability of detection for a single energy detector operating in a Nakagami-m fading channel using Herath's algorithm.
HerathNakagamiProbabilityOfDetection[M, \[Gamma], \[Lambda], m, n] calculates the exact probability of detection for a cooperative network operating in a Nakagami-m fading channel using Herath's algorithm.

Function timing may be specified using the following options:

Timed\[Rule]"<>ToString[Timed/.Options[ProbabilityOfDetection]]<>"
MaxIterations\[Rule]"<>ToString[MaxIterations/.Options[ProbabilityOfDetection]]<>"
MaxTime\[Rule]"<>ToString[MaxTime/.Options[ProbabilityOfDetection]]<>"

where the above settings are the defaults, if not specified. If Timed\[Rule]True, then a {Pd, time} list of values will be returned.";
HerathNakagamiProbabilityOfDetection[M_?NumericQ,\[Gamma]_,\[Lambda]_,m_?NumericQ,OptionsPattern[]]:=Module[{RelevantOptions, n = 1},
	RelevantOptions[target_]:=FilterRules[Table[#[[i]]->OptionValue[#[[i]]],{i,Length[#]}]&[Options[HerathNakagamiProbabilityOfDetection][[All,1]]],Options[target][[All,1]]];
	HerathNakagamiProbabilityOfDetection[M,\[Gamma],\[Lambda],m,n,RelevantOptions[HerathNakagamiProbabilityOfDetection]]
]
HerathNakagamiProbabilityOfDetection[M_?NumericQ,\[Gamma]_,\[Lambda]_,m_?NumericQ,n_?IntegerQ,OptionsPattern[]]:=Module[{lim, f, g, \[Psi], totaltime = 0, iterations = 0, time, result, diversityType = OptionValue[DiversityType]},
	lim = HerathLimit[M,\[Gamma],\[Lambda],m,n,DiversityType->diversityType];
	Print[lim];
	f:=Which[
		!ListQ[diversityType] && diversityType == "None",
			Which[
				ListQ[\[Gamma]],
					Undefined,
				!ListQ[\[Gamma]],
					1 - Exp[-\[Lambda] / 2] (m / ((M / 2) \[Gamma] + m))^(m) Total[Table[((\[Lambda] / 2)^j / j!) Hypergeometric1F1[m, j + 1, \[Lambda] (M / 2) \[Gamma] / (2 ((M / 2) \[Gamma] + m))],{j, (M / 2), lim}]],
				True,
					Undefined
			],
		!ListQ[diversityType] && diversityType == "MRC",
			Which[
				ListQ[\[Gamma]],
					1 - Exp[-\[Lambda] / 2] (m / ((M / 2) Mean[\[Gamma]] + m))^(n m) Total[Table[((\[Lambda] / 2)^j / j!) Hypergeometric1F1[m n, j + 1, \[Lambda] (M / 2) Mean[\[Gamma]] / (2 ((M / 2) Mean[\[Gamma]] + m))],{j, (M / 2), lim}]],
				!ListQ[\[Gamma]],
					1 - Exp[-\[Lambda] / 2] (m / ((M / 2) \[Gamma] + m))^(n m) Total[Table[((\[Lambda] / 2)^j / j!) Hypergeometric1F1[m n, j + 1, \[Lambda] (M / 2) \[Gamma] / (2 ((M / 2) \[Gamma] + m))],{j, (M / 2), lim}]],
				True,
					Undefined
			],
		!ListQ[diversityType] && diversityType == "EGC",
			Which[
				ListQ[\[Gamma]],
					Which[
						n == 2,
							1 - (2 Sqrt[\[Pi]] Gamma[2m] (m)^(2m) / (Gamma[m]^(2) Gamma[2m + (1 / 2)] 2^(4m - 1))) Exp[-\[Lambda] / 2] (2 / ((M / 2) Mean[\[Gamma]]))^(2m) Total[Table[(\[Lambda] / 2)^(j / 2) NIntegrate[x^(2m - (j / 2) - 1) Exp[-(2m / ((M / 2) Mean[\[Gamma]]) + 1) x] BesselI[j, Sqrt[2 \[Lambda] x]] Hypergeometric1F1[2m, 2m + (1 / 2), m x / ((M / 2) Mean[\[Gamma]])],{x, 0, \[Infinity]}],{j, M / 2, lim}]],
						n == 3,
							1 - Sqrt[\[Pi]] Exp[-\[Lambda] / 2] Total[Table[Total[Table[Total[Table[(\[Lambda] / 2)^j (3m / ((M / 2) Mean[\[Gamma]] + 3m))^(3m + p + k) ((Gamma[2m + p] Gamma[2m + k] Gamma[3m + p] Gamma[3m + p + 1 / 2] Gamma[4m + 2p + k]) / (Gamma[m]^3 Gamma[n + 1] Gamma[p + 1] Gamma[2m + p + 1 / 2] Gamma[3m + p + k + 1 / 2] Gamma[6m + 2p])) (1 / (2^(4m + p + k - 3) k!)) Hypergeometric1F1[3m + p + k, j + 1, \[Lambda] (M / 2) Mean[\[Gamma]] / (2 ((M / 2) Mean[\[Gamma]] + 3m))], {k, 0, lim[[1]]}]], {p, 0, lim[[2]]}]], {j, M / 2, lim[[3]]}]],
						n >= 4,
							Undefined,
						True,
							Undefined
					],
				!ListQ[\[Gamma]],
					Which[
						n == 2,
							1 - (2 Sqrt[\[Pi]] Gamma[2m] (m)^(2m) / (Gamma[m]^(2) Gamma[2m + (1 / 2)] 2^(4m - 1))) Exp[-\[Lambda] / 2] (2 / ((M / 2) \[Gamma]))^(2m) Total[Table[(\[Lambda] / 2)^(j / 2) NIntegrate[x^(2m - (j / 2) - 1) Exp[-(2m / ((M / 2) \[Gamma]) + 1) x] BesselI[j, Sqrt[2 \[Lambda] x]] Hypergeometric1F1[2m, 2m + (1 / 2), m x / ((M / 2) \[Gamma])],{x, 0, \[Infinity]}],{j, M / 2, lim}]],
							(*1 - Sqrt[\[Pi]] Exp[-\[Lambda] / 2]Total[Table[Total[Table[(\[Lambda] / 2)^(j) (2 m / (2 m + (M / 2) \[Gamma]))^(2 m + k) (((Gamma[2 m + k]^(2) / (Gamma[m]^(2) Gamma[j + 1] Gamma[2 m + k + (1 / 2)]))) / (2^(4 m + k - 2) k!)) Hypergeometric1F1[2 m + k, j + 1, \[Lambda] (M / 2) \[Gamma] / (2 (2 m + (M / 2) \[Gamma]))],{k, 0, lim[[1]]}]],{j, M / 2, lim[[2]]}]],*)
							(*\[Psi][\[Alpha]_,\[Beta]_,\[Gamma]0_,\[Gamma]1_,x_,y_]:=(1-y)^-\[Alpha] HypergeometricPFQ[{\[Alpha],\[Beta]},{\[Gamma]0,\[Gamma]1},-(x/(-1+y))];*)
							(*1 - (Exp[-\[Lambda] / 2] / 2^(2m - 2)) ((Gamma[2m]^2 Gamma[1 / 2]) / (Gamma[m]^2 Gamma[2m + 1 / 2])) (m / ((M / 2) \[Gamma] + m))^(2m) Total[Table[((\[Lambda] / 2)^j / j!) \[Psi][2m, 2m, 2m + 1 / 2, j + 1, m / ((M / 2) \[Gamma] + 2m), \[Lambda] (M / 2) \[Gamma] / (2 ((M / 2) \[Gamma] + 2m))], {j, M / 2, lim}]],*)
						n == 3,
							1 - Sqrt[\[Pi]] Exp[-\[Lambda] / 2] Total[Table[Total[Table[Total[Table[(\[Lambda] / 2)^j (3m / ((M / 2) \[Gamma] + 3m))^(3m + p + k) ((Gamma[2m + p] Gamma[2m + k] Gamma[3m + p] Gamma[3m + p + 1 / 2] Gamma[4m + 2p + k]) / (Gamma[m]^3 Gamma[n + 1] Gamma[p + 1] Gamma[2m + p + 1 / 2] Gamma[3m + p + k + 1 / 2] Gamma[6m + 2p])) (1 / (2^(4m + p + k - 3) k!)) Hypergeometric1F1[3m + p + k, j + 1, \[Lambda] (M / 2) \[Gamma] / (2 ((M / 2) \[Gamma] + 3m))], {k, 0, lim[[1]]}]], {p, 0, lim[[2]]}]], {j, M / 2, lim[[3]]}]],
						n >= 4,
							Undefined,
						True,
							Undefined
					],
				True,
					Undefined
			],
		!ListQ[diversityType] && diversityType == "SC",
			(*1 - n Exp[-\[Lambda] / 2] (m / \[Gamma])^(m) Total[Table[Total[Table[Binomial[n - 1, k] (-1)^(k) ((\[Lambda] / 2)^(j) / (j!)) Total[Table[MultinomialCoefficient[k, i, m] Pochhammer[m, i] (\[Gamma] / (\[Gamma] + m (k + 1)))^(i + m) Hypergeometric1F1[i + m, j + 1, \[Lambda] \[Gamma] / (2 (\[Gamma] + m (k + 1)))],{i, 0, k*(m - 1)}]],{k, 0, n - 1}]],{j, M / 2, lim}]],*)
			Undefined,
		ListQ[diversityType] && diversityType[[1]] == "SSC",
			Which[
				ListQ[\[Gamma]],
					Module[{\[Gamma]t = diversityType[[2]]},
						(1 - GammaRegularized[m, m \[Gamma]t / Mean[\[Gamma]]]) (1 - Exp[-\[Lambda] / 2] (m / (m + (M / 2) Mean[\[Gamma]]))^(m) Total[Table[((\[Lambda] / 2)^(j) / j!) Hypergeometric1F1[m, j + 1, \[Lambda] (M / 2) Mean[\[Gamma]] / (2 ((M / 2) Mean[\[Gamma]] + m))],{j, M / 2, lim[[1]]}]]) + (m / ((M / 2) Mean[\[Gamma]] + m))^(m) (Exp[-\[Lambda] / 2] / Gamma[m]) Total[Table[Total[Table[((M / 2) Mean[\[Gamma]] / ((M / 2) Mean[\[Gamma]] + m))^(j) Gamma[j + m, (1 + m / ((M / 2) Mean[\[Gamma]])) (M / 2) \[Gamma]t] (\[Lambda] / 2)^(k) / (j! k!),{k, 0, j + (M / 2) - 1}]],{j, 0, lim[[2]]}]]
					],
				!ListQ[\[Gamma]],
					Module[{\[Gamma]t = diversityType[[2]]},
						(1 - GammaRegularized[m, m \[Gamma]t / \[Gamma]]) (1 - Exp[-\[Lambda] / 2] (m / (m + (M / 2) \[Gamma]))^(m) Total[Table[((\[Lambda] / 2)^(j) / j!) Hypergeometric1F1[m, j + 1, \[Lambda] (M / 2) \[Gamma] / (2 ((M / 2) \[Gamma] + m))],{j, M / 2, lim[[1]]}]]) + (m / ((M / 2) \[Gamma] + m))^(m) (Exp[-\[Lambda] / 2] / Gamma[m]) Total[Table[Total[Table[((M / 2) \[Gamma] / ((M / 2) \[Gamma] + m))^(j) Gamma[j + m, (1 + m / ((M / 2) \[Gamma])) (M / 2) \[Gamma]t] (\[Lambda] / 2)^(k) / (j! k!),{k, 0, j + (M / 2) - 1}]],{j, 0, lim[[2]]}]]
					],
				True,
					Undefined
			],
		!ListQ[diversityType] && diversityType == "SLC",
			Which[
				ListQ[\[Gamma]],
					1 - Exp[-\[Lambda] / 2] (m / ((M / 2) Mean[\[Gamma]] + m))^(n m) Total[Table[((\[Lambda] / 2)^j / j!) Hypergeometric1F1[m n, j + 1, \[Lambda] (M / 2) Mean[\[Gamma]] / (2 ((M / 2) Mean[\[Gamma]] + m))],{j, (M n / 2), lim}]],
				!ListQ[\[Gamma]],
					1 - Exp[-\[Lambda] / 2] (m / ((M / 2) \[Gamma] + m))^(n m) Total[Table[((\[Lambda] / 2)^j / j!) Hypergeometric1F1[m n, j + 1, \[Lambda] (M / 2) \[Gamma] / (2 ((M / 2) \[Gamma] + m))],{j, (M n / 2), lim}]],
				True,
					Undefined
			],
		!ListQ[diversityType] && diversityType == "SLS",
			Which[
				ListQ[\[Gamma]],
					1 - Product[1 - HerathNakagamiProbabilityOfDetection[M,\[Gamma][[i]],\[Lambda],m,DiversityType->"None"],{i,n}],
				!ListQ[\[Gamma]],
					1 - (1 - HerathNakagamiProbabilityOfDetection[M,\[Gamma],\[Lambda],m,DiversityType->"None"])^n,
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
SunLimit[M_?NumericQ,\[Gamma]_,\[Lambda]_,m_?NumericQ,OptionsPattern[]]:=Module[{n = 1},SunLimit[M,\[Gamma],\[Lambda],m,n,Tolerance->OptionValue[Tolerance]]]
SunLimit[M_?NumericQ,\[Gamma]_,\[Lambda]_,m_?NumericQ,n_?IntegerQ,OptionsPattern[]]:=Module[{j, j0, tol = OptionValue[Tolerance], diversityType = OptionValue[DiversityType]},
	Which[
		!ListQ[diversityType] && diversityType == "None",
			j0 = (\[Lambda] / 2) - (M / 2) - Sqrt[M / 2] InverseQ[1 - tol];
			Which[
				ListQ[\[Gamma]],
					Undefined,
				!ListQ[\[Gamma]],
					j/.FindRoot[(1 - GammaRegularized[M / 2 + j - 1, \[Lambda] / 2]) (1 - CDF[NegativeBinomialDistribution[m, (m / (m + (M / 2) \[Gamma]))^(m)], j + 1]) == tol,{j, j0, 1, \[Infinity]}],
				True,
					Undefined
			],
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


Options[SunNakagamiProbabilityOfDetection]={DiversityType->"SLC",Timed->OptionValue[ProbabilityOfDetection,Timed],MaxTime->OptionValue[ProbabilityOfDetection,MaxTime],MaxIterations->OptionValue[ProbabilityOfDetection,MaxIterations]};
SunNakagamiProbabilityOfDetection::usage="SunNakagamiProbabilityOfDetection[M, \[Gamma], \[Lambda], m] calculates the exact probability of detection for a single energy detector operating in a Nakagami-m fading channel using Sun's algorithm.
SunNakagamiProbabilityOfDetection[M, \[Gamma], \[Lambda], m, n] calculates the exact probability of detection for a cooperative network operating in a Nakagami-m fading channel using Sun's algorithm.

Function timing may be specified using the following options:

Timed\[Rule]"<>ToString[Timed/.Options[ProbabilityOfDetection]]<>"
MaxIterations\[Rule]"<>ToString[MaxIterations/.Options[ProbabilityOfDetection]]<>"
MaxTime\[Rule]"<>ToString[MaxTime/.Options[ProbabilityOfDetection]]<>"

where the above settings are the defaults, if not specified. If Timed\[Rule]True, then a {Pd, time} list of values will be returned.";
SunNakagamiProbabilityOfDetection[M_?NumericQ,\[Gamma]_,\[Lambda]_,m_?NumericQ,OptionsPattern[]]:=Module[{RelevantOptions, n = 1},
	RelevantOptions[target_]:=FilterRules[Table[#[[i]]->OptionValue[#[[i]]],{i,Length[#]}]&[Options[SunNakagamiProbabilityOfDetection][[All,1]]],Options[target][[All,1]]];
	SunNakagamiProbabilityOfDetection[M,\[Gamma],\[Lambda],m,n,RelevantOptions[SunNakagamiProbabilityOfDetection]]
]
SunNakagamiProbabilityOfDetection[M_?NumericQ,\[Gamma]_,\[Lambda]_,m_?NumericQ,n_?IntegerQ,OptionsPattern[]]:=Module[{lim, f, totaltime = 0, iterations = 0, time, result, diversityType = OptionValue[DiversityType]},
	lim = SunLimit[M,\[Gamma],\[Lambda],m,n,DiversityType->diversityType];
	f:=Which[
		!ListQ[diversityType] && diversityType == "None",
			Which[
				ListQ[\[Gamma]],
					Undefined,
				!ListQ[\[Gamma]],
					GammaRegularized[M / 2, \[Lambda] / 2] + Exp[-\[Lambda] / 2] Total[Table[((\[Lambda] / 2)^(j) / j!) (1 - (m / (m + (M / 2) \[Gamma]))^(m) Total[Table[(m + k - 1)! / (Gamma[m] k!) ((M / 2) \[Gamma] / (m + (M / 2) \[Gamma]))^(k),{k, 0, j - M / 2}]]),{j, M / 2, M / 2 + lim}]],
				True,
					Undefined
			],
		!ListQ[diversityType] && diversityType == "MRC",
			Which[
				ListQ[\[Gamma]],
					1 - (m / (m + (M / 2) Mean[\[Gamma]]))^(m n) Total[Table[(1 - GammaRegularized[(M / 2) + j, \[Lambda] / 2]) Pochhammer[m n, j] / j! ((M / 2) Mean[\[Gamma]] / (m + (M / 2) Mean[\[Gamma]]))^(j),{j, 0, lim}]],
				!ListQ[\[Gamma]],
					1 - (m / (m + (M / 2) \[Gamma]))^(m n) Total[Table[(1 - GammaRegularized[(M / 2) + j, \[Lambda] / 2]) Pochhammer[m n, j] / j! ((M / 2) \[Gamma] / (m + (M / 2) \[Gamma]))^(j),{j, 0, lim}]],
				True,
					Undefined
			],
		!ListQ[diversityType] && diversityType == "EGC",
			Undefined,
		!ListQ[diversityType] && diversityType == "SC",
			Which[
				ListQ[\[Gamma]],
					1 - n Total[Table[(-1)^(l) Binomial[n - 1, l] Total[Table[MultinomialCoefficient[l, k, m] (m / ((M / 2) Max[\[Gamma]]))^(m + k) Total[Table[(1 - GammaRegularized[i + M / 2, \[Lambda] / 2]) Pochhammer[m, i + k] / i! ((M / 2) Max[\[Gamma]] / ((M / 2) Max[\[Gamma]] + m (l + 1)))^(i + m + k),{i, 0, lim}]],{k, 0, l (m - 1)}]],{l, 0, n - 1}]],
				!ListQ[\[Gamma]],
					1 - n Total[Table[(-1)^(l) Binomial[n - 1, l] Total[Table[MultinomialCoefficient[l, k, m] (m / ((M / 2) \[Gamma]))^(m + k) Total[Table[(1 - GammaRegularized[i + M / 2, \[Lambda] / 2]) Pochhammer[m, i + k] / i! ((M / 2) \[Gamma] / ((M / 2) \[Gamma] + m (l + 1)))^(i + m + k),{i, 0, lim}]],{k, 0, l (m - 1)}]],{l, 0, n - 1}]],
				True,
					Undefined
			],
		ListQ[diversityType] && diversityType[[1]] == "SSC",
			Undefined,
		!ListQ[diversityType] && diversityType == "SLC",
			Which[
				ListQ[\[Gamma]],
					1 - (m / (m + (M / 2) Mean[\[Gamma]]))^(m n) Total[Table[(1 - GammaRegularized[(M / 2) n + j, \[Lambda] / 2]) Pochhammer[m n, j] / j! ((M / 2) Mean[\[Gamma]] / (m + (M / 2) Mean[\[Gamma]]))^(j),{j, 0, lim}]],
				!ListQ[\[Gamma]],
					1 - (m / (m + (M / 2) \[Gamma]))^(m n) Total[Table[(1 - GammaRegularized[(M / 2) n + j, \[Lambda] / 2]) Pochhammer[m n, j] / j! ((M / 2) \[Gamma] / (m + (M / 2) \[Gamma]))^(j),{j, 0, lim}]],
				True,
					Undefined
			],
		!ListQ[diversityType] && diversityType == "SLS",
			Which[
				ListQ[\[Gamma]],
					1 - Product[1 - SunNakagamiProbabilityOfDetection[M,\[Gamma][[i]],\[Lambda],m,DiversityType->"None"],{i,n}],
				!ListQ[\[Gamma]],
					1 - (1 - SunNakagamiProbabilityOfDetection[M,\[Gamma],\[Lambda],m,DiversityType->"None"])^n,
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
(*Numerical method*)


Options[NumericalNakagamiProbabilityOfDetection]={DiversityType->"SLC",Timed->OptionValue[ProbabilityOfDetection,Timed],MaxTime->OptionValue[ProbabilityOfDetection,MaxTime],MaxIterations->OptionValue[ProbabilityOfDetection,MaxIterations],Resolution->1000};
NumericalNakagamiProbabilityOfDetection::usage="NumericalNakagamiProbabilityOfDetection[M, \[Gamma], \[Lambda], m] calculates the exact probability of detection for a single energy detector operating in a Nakagami-m fading channel using a numerical algorithm.
NumericalNakagamiProbabilityOfDetection[M, \[Gamma], \[Lambda], m, n] calculates the exact probability of detection for a cooperative network operating in a Nakagami-m fading channel using a numerical algorithm.

Function timing may be specified using the following options:

Timed\[Rule]"<>ToString[Timed/.Options[ProbabilityOfDetection]]<>"
MaxIterations\[Rule]"<>ToString[MaxIterations/.Options[ProbabilityOfDetection]]<>"
MaxTime\[Rule]"<>ToString[MaxTime/.Options[ProbabilityOfDetection]]<>"

where the above settings are the defaults, if not specified. If Timed\[Rule]True, then a {Pd, time} list of values will be returned.";
NumericalNakagamiProbabilityOfDetection[M_?NumericQ,\[Gamma]_,\[Lambda]_,m_?NumericQ,OptionsPattern[]]:=Module[{RelevantOptions, n = 1},
	RelevantOptions[target_]:=FilterRules[Table[#[[i]]->OptionValue[#[[i]]],{i,Length[#]}]&[Options[NumericalNakagamiProbabilityOfDetection][[All,1]]],Options[target][[All,1]]];
	NumericalNakagamiProbabilityOfDetection[M,\[Gamma],\[Lambda],m,n,RelevantOptions[NumericalNakagamiProbabilityOfDetection]]
]
NumericalNakagamiProbabilityOfDetection[M_?NumericQ,\[Gamma]_,\[Lambda]_,m_?NumericQ,n_?IntegerQ,OptionsPattern[]]:=Module[{lim, f, totaltime = 0, iterations = 0, time, result, diversityType = OptionValue[DiversityType], resolution = OptionValue[Resolution], pdf, x},
	f:=Which[
		!ListQ[diversityType] && diversityType == "None",
			Which[
				ListQ[\[Gamma]],
					Undefined,
				!ListQ[\[Gamma]],
					pdf = Table[RandomVariate[NoncentralChiSquareDistribution[M, M RandomVariate[GammaDistribution[m, \[Gamma] / m]]]],{resolution}];
					Length[Select[pdf, # > \[Lambda] &]] / resolution,
				True,
					Undefined
			],
		!ListQ[diversityType] && diversityType == "MRC",
			Which[
				ListQ[\[Gamma]],
					pdf = Table[RandomVariate[NoncentralChiSquareDistribution[M, M RandomVariate[GammaDistribution[m n, Mean[\[Gamma]] / m]]]],{resolution}],
				!ListQ[\[Gamma]],
					pdf = Table[RandomVariate[NoncentralChiSquareDistribution[M, M RandomVariate[GammaDistribution[m n, \[Gamma] / m]]]],{resolution}],
				True,
					Undefined
			];
			Length[Select[pdf, # > \[Lambda] &]] / resolution,
		!ListQ[diversityType] && diversityType == "EGC",
			Undefined,
		!ListQ[diversityType] && diversityType == "SC",
			Which[
				ListQ[\[Gamma]],
					pdf = Table[RandomVariate[NoncentralChiSquareDistribution[M, M RandomVariate[ProbabilityDistribution[Piecewise[{{(n / Gamma[m]) (m / Mean[\[Gamma]])^m x^(m - 1) Exp[-m x / Mean[\[Gamma]]] (1 - GammaRegularized[m, m x / Mean[\[Gamma]]])^(n - 1), x > 0}}], {x, 0, \[Infinity]}]]]],{resolution}],
				!ListQ[\[Gamma]],
					pdf = Table[RandomVariate[NoncentralChiSquareDistribution[M, M RandomVariate[ProbabilityDistribution[Piecewise[{{(n / Gamma[m]) (m / \[Gamma])^m x^(m - 1) Exp[-m x / \[Gamma]] (1 - GammaRegularized[m, m x / \[Gamma]])^(n - 1), x > 0}}], {x, 0, \[Infinity]}]]]],{resolution}],
				True,
					Undefined
			];
			Length[Select[pdf, # > \[Lambda] &]] / resolution,
		ListQ[diversityType] && diversityType[[1]] == "SSC",
			Module[{\[Gamma]t = diversityType[[2]]},
				Which[
					ListQ[\[Gamma]] && Length[\[Gamma]] == 2,
						pdf = Table[RandomVariate[NoncentralChiSquareDistribution[M, M RandomVariate[ProbabilityDistribution[Piecewise[{{(1 - GammaRegularized[m, m \[Gamma]t/Mean[\[Gamma]]]) PDF[GammaDistribution[m, Mean[\[Gamma]]/m], x], 0 <= x < \[Gamma]t}, {(2 - GammaRegularized[m, m \[Gamma]t/Mean[\[Gamma]]]) PDF[GammaDistribution[m, Mean[\[Gamma]]/m], x], x >= \[Gamma]t}}], {x, 0, \[Infinity]}]]]],{resolution}],
					!ListQ[\[Gamma]],
						pdf = Table[RandomVariate[NoncentralChiSquareDistribution[M, M RandomVariate[ProbabilityDistribution[Piecewise[{{(1 - GammaRegularized[m, m \[Gamma]t/\[Gamma]]) PDF[GammaDistribution[m, \[Gamma]/m], x], 0 <= x < \[Gamma]t}, {(2 - GammaRegularized[m, m \[Gamma]t/\[Gamma]]) PDF[GammaDistribution[m, \[Gamma]/m], x], x >= \[Gamma]t}}], {x, 0, \[Infinity]}]]]],{resolution}],
					True,
						Undefined
				];
				Length[Select[pdf, # > \[Lambda] &]] / resolution
			],
		!ListQ[diversityType] && diversityType == "SLC",
			Which[
				ListQ[\[Gamma]],
					pdf = Table[RandomVariate[NoncentralChiSquareDistribution[M n, M RandomVariate[GammaDistribution[m n, Mean[\[Gamma]] / m]]]],{resolution}],
				!ListQ[\[Gamma]],
					pdf = Table[RandomVariate[NoncentralChiSquareDistribution[M n, M RandomVariate[GammaDistribution[m n, \[Gamma] / m]]]],{resolution}],
				True,
					Undefined
			];
			Length[Select[pdf, # > \[Lambda] &]] / resolution,
		!ListQ[diversityType] && diversityType == "SLS",
			Which[
				ListQ[\[Gamma]],
					1 - Product[1 - NumericalNakagamiProbabilityOfDetection[M,\[Gamma][[i]],\[Lambda],m,DiversityType->"None"],{i,n}],
				!ListQ[\[Gamma]],
					1 - (1 - NumericalNakagamiProbabilityOfDetection[M,\[Gamma],\[Lambda],m,DiversityType->"None"])^n,
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


Options[IntegerMNLimit]={DiversityType->"EGC",Tolerance->10^-6};
IntegerMNLimit[M_?NumericQ,\[Gamma]_,\[Lambda]_,m_?NumericQ,n_?NumericQ,OptionsPattern[]]:=Module[{j, j0, diversityType = OptionValue[DiversityType], tol = OptionValue[Tolerance]},
	Which[
		diversityType=="EGC",
			Which[
				n == 2,
					j0 = 0;
					j/.FindRoot[1/2 Gamma[1/2]/2^(4m-2) Sum[(Gamma[2m+k]Gamma[2m+k])/Gamma[2m+1/2+k] (1/2)^k/k!,{k, j + 1, \[Infinity]}] == tol, {j, j0, 0, \[Infinity]}],
				n == 3,
					Module[{l0, k0, f},
						k0 = 0;
						l0 = 0;
						While[
							(4 Sqrt[\[Pi]] Gamma[2m] / (Gamma[m]^3 2^(4m))) Sum[((Gamma[2m + l] Gamma[4m + 2l]) / (Gamma[2m + l + 1 / 2] Gamma[6m + 2l])) ((1 / 2)^l / l!) Gamma[3m + l] Sum[(Pochhammer[2m, k] Pochhammer[4m + 2l, k] / Pochhammer[3m + l + 1 / 2, k]) ((1 / 2)^k / k!), {k, k0 + 1, \[Infinity]}], {l, 0, \[Infinity]}] > tol,
							k0++
						];
						f[l_?NumericQ]:=Total[Table[(Pochhammer[2m, k] Pochhammer[4m + 2l, k] / Pochhammer[3m + l + 1 / 2, k]) ((1 / 2)^k / k!), {k, 0, k0}]];
						While[
							(4 Sqrt[\[Pi]] Gamma[2m] / (Gamma[m]^3 2^(4m))) NSum[((Gamma[2m + l] Gamma[4m + 2l]) / (Gamma[2m + l + 1 / 2] Gamma[6m + 2l])) ((1 / 2)^l / l!) Gamma[3m + l] f[l], {l, l0 + 1, \[Infinity]}] > tol,
							l0++
						];
						{l0, k0}
					],
				True,
					Undefined
			],
		True,
			Undefined
	]//Ceiling
]


Options[IntegerMNNakagamiProbabilityOfDetection]={DiversityType->"SLC",Timed->OptionValue[ProbabilityOfDetection,Timed],MaxTime->OptionValue[ProbabilityOfDetection,MaxTime],MaxIterations->OptionValue[ProbabilityOfDetection,MaxIterations]};
IntegerMNNakagamiProbabilityOfDetection::usage="IntegerMNNakagamiProbabilityOfDetection[M, \[Gamma], \[Lambda], m] calculates the approximate probability of detection for a single energy detector operating in a Nakagami-m fading channel using the integer mn approximation.
IntegerMNNakagamiProbabilityOfDetection[M, \[Gamma], \[Lambda], m, n] calculates the approximate probability of detection for a cooperative network operating in a Nakagami-m fading channel using the integer mn approximation.

Function timing may be specified using the following options:

Timed\[Rule]"<>ToString[Timed/.Options[ProbabilityOfDetection]]<>"
MaxIterations\[Rule]"<>ToString[MaxIterations/.Options[ProbabilityOfDetection]]<>"
MaxTime\[Rule]"<>ToString[MaxTime/.Options[ProbabilityOfDetection]]<>"

where the above settings are the defaults, if not specified. If Timed\[Rule]True, then a {Pd, time} list of values will be returned.";
IntegerMNNakagamiProbabilityOfDetection[M_?NumericQ,\[Gamma]_,\[Lambda]_,m_?NumericQ,OptionsPattern[]]:=Module[{RelevantOptions, n = 1},
	RelevantOptions[target_]:=FilterRules[Table[#[[i]]->OptionValue[#[[i]]],{i,Length[#]}]&[Options[IntegerMNNakagamiProbabilityOfDetection][[All,1]]],Options[target][[All,1]]];
	IntegerMNNakagamiProbabilityOfDetection[M,\[Gamma],\[Lambda],m,n,#/.(DiversityType/.#)->"None"&[RelevantOptions[IntegerMNNakagamiProbabilityOfDetection]]]
]
IntegerMNNakagamiProbabilityOfDetection[M_?NumericQ,\[Gamma]_,\[Lambda]_,m_?NumericQ,n_?NumericQ,OptionsPattern[]]:=Module[{lim, f, totaltime = 0, iterations = 0, time, result, diversityType = OptionValue[DiversityType], x = Round[m n], tol = 10^-6},
	(* This method can only be used when m * n is an integer *)
	lim = IntegerMNLimit[M, \[Gamma], \[Lambda], m, n, DiversityType->diversityType];
	If[Abs[m n - x] <= tol,
		f:=Which[
			!ListQ[diversityType] && diversityType == "None",
				Which[
					ListQ[\[Gamma]],
						Undefined,
					!ListQ[\[Gamma]],
						AWGNProbabilityOfFalseAlarm[M,\[Lambda]] - (D[((-1)^(x) / (2 Gamma[x])) * Exp[((m^2) / (M \[Gamma]^2)) * t^2 - ((\[Lambda] - M) / (M (\[Gamma] / m))) * t]Erfc[(2 t - (\[Lambda] - M) * (\[Gamma] / m)) / (2Sqrt[M] (\[Gamma] / m))] / t, {t, x - 1}]/.t->1),
					True,
						Undefined
				],
			!ListQ[diversityType] && diversityType == "MRC",
				Which[
					ListQ[\[Gamma]],
						AWGNProbabilityOfFalseAlarm[M,\[Lambda]] - (D[((-1)^(x) / (2 Gamma[x])) * Exp[((m^2) / (M Mean[\[Gamma]]^2)) * t^2 - ((\[Lambda] - M) / (M (Mean[\[Gamma]] / m))) * t]Erfc[(2 t - (\[Lambda] - M) * (Mean[\[Gamma]] / m)) / (2Sqrt[M] (Mean[\[Gamma]] / m))] / t, {t, x - 1}]/.t->1),
					!ListQ[\[Gamma]],
						AWGNProbabilityOfFalseAlarm[M,\[Lambda]] - (D[((-1)^(x) / (2 Gamma[x])) * Exp[((m^2) / (M \[Gamma]^2)) * t^2 - ((\[Lambda] - M) / (M (\[Gamma] / m))) * t]Erfc[(2 t - (\[Lambda] - M) * (\[Gamma] / m)) / (2Sqrt[M] (\[Gamma] / m))] / t, {t, x - 1}]/.t->1),
					True,
						Undefined
				],
			!ListQ[diversityType] && diversityType == "EGC",
					Which[
					ListQ[\[Gamma]],
						Which[
							n == 2,
								(Sqrt[\[Pi]] / 2^(4m - 2)) (Gamma[2m] / (Gamma[m]^2 Gamma[2m + (1 / 2)])) Total[Table[(Pochhammer[2m, k] / Pochhammer[2m + (1 / 2), k]) ((1 / 2)^k / k!) (-1)^(k+2 m -1) (D[ (1+Erf[(M-\[Lambda])/(2 Sqrt[M])]+E^((2 m t (2 m t+M Mean[\[Gamma]]-Mean[\[Gamma]] \[Lambda]))/(M Mean[\[Gamma]]^2)) Erfc[(4 m t+M Mean[\[Gamma]]-Mean[\[Gamma]] \[Lambda])/(2 Sqrt[M] Mean[\[Gamma]])])/(2 t),{t, 2m + k - 1}]/.t->1),{k,0,lim}]],
							n == 3,
								(4 Sqrt[\[Pi]] / (2^(4m - 1))) (Gamma[2m] / (Gamma[m]^3)) Total[Table[(Gamma[2m+l] Gamma[4m+2l] / (Gamma[2m+l+1/2] Gamma[6m+2l] Gamma[l+1] (2^l))) Total[Table[(Pochhammer[2m,k] Pochhammer[4m+2l,k] / (Pochhammer[3m+l+1/2,k] Pochhammer[3m+l,k]))((1/2)^k/k!)(-1)^(k+2+3 m) D[(1+Erf[(M-\[Lambda])/(2 Sqrt[M])]+E^((3 m t (3 m t+M Mean[\[Gamma]]-Mean[\[Gamma]] \[Lambda]))/(M Mean[\[Gamma]]^2)) Erfc[(6 m t+M Mean[\[Gamma]]-Mean[\[Gamma]] \[Lambda])/(2 Sqrt[M] Mean[\[Gamma]])])/(2 t),{t,3m+l+k-1}],{k,0,lim[[2]]}]],{l,0,lim[[1]]}]],
							True,
								Undefined
						],
					!ListQ[\[Gamma]],
						Which[
							n == 2,
								(Sqrt[\[Pi]] / 2^(4m - 2)) (Gamma[2m] / (Gamma[m]^2 Gamma[2m + (1 / 2)])) Total[Table[(Pochhammer[2m, k] / Pochhammer[2m + (1 / 2), k]) ((1 / 2)^k / k!) (-1)^(k+2 m -1) (D[ (1+Erf[(M-\[Lambda])/(2 Sqrt[M])]+E^((2 m t (2 m t+M \[Gamma]-\[Gamma] \[Lambda]))/(M \[Gamma]^2)) Erfc[(4 m t+M \[Gamma]-\[Gamma] \[Lambda])/(2 Sqrt[M] \[Gamma])])/(2 t),{t, 2m + k - 1}]/.t->1),{k,0,lim}]],
							n == 3,
								(4 Sqrt[\[Pi]] / (2^(4m - 1))) (Gamma[2m] / (Gamma[m]^3)) Total[Table[(Gamma[2m+l] Gamma[4m+2l] / (Gamma[2m+l+1/2] Gamma[6m+2l] Gamma[l+1] (2^l))) Total[Table[(Pochhammer[2m,k] Pochhammer[4m+2l,k] / (Pochhammer[3m+l+1/2,k] Pochhammer[3m+l,k]))((1/2)^k/k!)(-1)^(k+2+3 m) D[(1+Erf[(M-\[Lambda])/(2 Sqrt[M])]+E^((3 m t (3 m t+M \[Gamma]-\[Gamma] \[Lambda]))/(M \[Gamma]^2)) Erfc[(6 m t+M \[Gamma]-\[Gamma] \[Lambda])/(2 Sqrt[M] \[Gamma])])/(2 t),{t,3m+l+k-1}],{k,0,lim[[2]]}]],{l,0,lim[[1]]}]],
							True,
								Undefined
						],
					True,
						Undefined
				],
			!ListQ[diversityType] && diversityType == "SC",
				Which[
					ListQ[\[Gamma]],
						(n / Gamma[m]) Total[Table[(-1)^(l) Binomial[n - 1, l] Total[Table[MultinomialCoefficient[l, k, m] (D[-(-(1 / (1 + l)))^(k + m) (1 + Erf[(M - \[Lambda]) / (2 Sqrt[M])] + Exp[((1 + l) m t ((1 + l) m t + Max[\[Gamma]] (M - \[Lambda]))) / (M Max[\[Gamma]]^2)] Erfc[(2 (1 + l) m t + Max[\[Gamma]] (M - \[Lambda]))/(2 Sqrt[M] Max[\[Gamma]])]) / (2 t),{t, k + m - 1}]/.t->1),{k, 0, l (m - 1)}]],{l, 0, n - 1}]],
					!ListQ[\[Gamma]],
						(n / Gamma[m]) Total[Table[(-1)^(l) Binomial[n - 1, l] Total[Table[MultinomialCoefficient[l, k, m] (D[-(-(1 / (1 + l)))^(k + m) (1 + Erf[(M - \[Lambda]) / (2 Sqrt[M])] + Exp[((1 + l) m t ((1 + l) m t + \[Gamma] (M - \[Lambda]))) / (M \[Gamma]^2)] Erfc[(2 (1 + l) m t + \[Gamma] (M - \[Lambda]))/(2 Sqrt[M] \[Gamma])]) / (2 t),{t, k + m - 1}]/.t->1),{k, 0, l (m - 1)}]],{l, 0, n - 1}]],
					True,
						Undefined
				],
			ListQ[diversityType] && diversityType[[1]] == "SSC",
				Which[
					ListQ[\[Gamma]],
						Module[{\[Gamma]t = diversityType[[2]]},
							(1 - GammaRegularized[m, m \[Gamma]t/Mean[\[Gamma]]]) IntegerMNNakagamiProbabilityOfDetection[M,Mean[\[Gamma]],\[Lambda],m,DiversityType->"None"] + D[((-1)^m E^(-((m t (2 M \[Gamma]t+\[Lambda]))/(M Mean[\[Gamma]]))) (-E^(((m t (m t+M Mean[\[Gamma]] (1+2 \[Gamma]t)))/(M Mean[\[Gamma]]^2))) Erfc[(2 m t+Mean[\[Gamma]] (M+M \[Gamma]t-\[Lambda]))/(2 Sqrt[M] Mean[\[Gamma]])]+E^((m t (M \[Gamma]t+\[Lambda]))/(M Mean[\[Gamma]])) (-2+Erfc[(M+M \[Gamma]t-\[Lambda])/(2 Sqrt[M])])))/(2 t Gamma[m]),{t, m - 1}]/.t->1
						],
					!ListQ[\[Gamma]],
						Module[{\[Gamma]t = diversityType[[2]]},
							(1 - GammaRegularized[m, m \[Gamma]t/\[Gamma]]) IntegerMNNakagamiProbabilityOfDetection[M,\[Gamma],\[Lambda],m,DiversityType->"None"] + D[((-1)^m E^(-((m t (2 M \[Gamma]t+\[Lambda]))/(M \[Gamma]))) (-E^(((m t (m t+M \[Gamma] (1+2 \[Gamma]t)))/(M \[Gamma]^2))) Erfc[(2 m t+\[Gamma] (M+M \[Gamma]t-\[Lambda]))/(2 Sqrt[M] \[Gamma])]+E^((m t (M \[Gamma]t+\[Lambda]))/(M \[Gamma])) (-2+Erfc[(M+M \[Gamma]t-\[Lambda])/(2 Sqrt[M])])))/(2 t Gamma[m]),{t, m - 1}]/.t->1
						],
					True,
						Undefined
				],
			!ListQ[diversityType] && diversityType == "SLC",
				Which[
					ListQ[\[Gamma]],
						N[AWGNProbabilityOfFalseAlarm[M,\[Lambda],n] - (D[((-1)^(x) / (2 Gamma[x])) * Exp[((m^2 n) / (M Mean[\[Gamma]]^2)) * t^2 - ((\[Lambda] - M n) / (M (Mean[\[Gamma]] / m))) * t]Erfc[(2n t - (\[Lambda] - M n) * (Mean[\[Gamma]] / m)) / (2Sqrt[M n] (Mean[\[Gamma]] / m))] / t, {t, x - 1}]/.t->1),20],
					!ListQ[\[Gamma]],
						N[AWGNProbabilityOfFalseAlarm[M,\[Lambda],n] - (D[((-1)^(x) / (2 Gamma[x])) * Exp[((m^2 n) / (M \[Gamma]^2)) * t^2 - ((\[Lambda] - M n) / (M (\[Gamma] / m))) * t]Erfc[(2n t - (\[Lambda] - M n) * (\[Gamma] / m)) / (2Sqrt[M n] (\[Gamma] / m))] / t, {t, x - 1}]/.t->1),20],
					True,
						Undefined
				],
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




(* ::Subsubsection:: *)
(*Large SNR method*)


Options[LargeSNRNakagamiProbabilityOfDetection]={DiversityType->"SLC",Timed->OptionValue[ProbabilityOfDetection,Timed],MaxTime->OptionValue[ProbabilityOfDetection,MaxTime],MaxIterations->OptionValue[ProbabilityOfDetection,MaxIterations]};
LargeSNRNakagamiProbabilityOfDetection::usage="LargeSNRNakagamiProbabilityOfDetection[M, \[Gamma], \[Lambda], m] calculates the approximate probability of detection for a single energy detector operating in a Nakagami-m fading channel using the integer mn approximation.
LargeSNRNakagamiProbabilityOfDetection[M, \[Gamma], \[Lambda], m, n] calculates the approximate probability of detection for a cooperative network operating in a Nakagami-m fading channel using the integer mn approximation.

Function timing may be specified using the following options:

Timed\[Rule]"<>ToString[Timed/.Options[ProbabilityOfDetection]]<>"
MaxIterations\[Rule]"<>ToString[MaxIterations/.Options[ProbabilityOfDetection]]<>"
MaxTime\[Rule]"<>ToString[MaxTime/.Options[ProbabilityOfDetection]]<>"

where the above settings are the defaults, if not specified. If Timed\[Rule]True, then a {Pd, time} list of values will be returned.";
LargeSNRNakagamiProbabilityOfDetection[M_?NumericQ,\[Gamma]_,\[Lambda]_,m_?NumericQ,OptionsPattern[]]:=Module[{RelevantOptions, n = 1},
	RelevantOptions[target_]:=FilterRules[Table[#[[i]]->OptionValue[#[[i]]],{i,Length[#]}]&[Options[LargeSNRNakagamiProbabilityOfDetection][[All,1]]],Options[target][[All,1]]];
	LargeSNRNakagamiProbabilityOfDetection[M,\[Gamma],\[Lambda],m,n,#/.(DiversityType/.#)->"None"&[RelevantOptions[LargeSNRNakagamiProbabilityOfDetection]]]
]
LargeSNRNakagamiProbabilityOfDetection[M_?NumericQ,\[Gamma]_,\[Lambda]_,m_?NumericQ,n_?NumericQ,OptionsPattern[]]:=Module[{lim, f, totaltime = 0, iterations = 0, time, result, diversityType = OptionValue[DiversityType], x = Round[m n], tol = 10^-6},
	(* This method can only be used when m * n is an integer *)
	lim = LargeSNRLimit[M, \[Gamma], \[Lambda], m, n, DiversityType->diversityType];
	If[Abs[m n - x] <= tol,
		f:=Which[
			!ListQ[diversityType] && diversityType == "None",
				Which[
					ListQ[\[Gamma]],
						Undefined,
					!ListQ[\[Gamma]],
						AWGNProbabilityOfFalseAlarm[M,\[Lambda]] + (1 - AWGNProbabilityOfFalseAlarm[M,\[Lambda]]) ( Gamma[m,(m (-M+\[Lambda]))/(M \[Gamma])]/Gamma[m]),
					True,
						Undefined
				],
			!ListQ[diversityType] && diversityType == "MRC",
				Which[
					ListQ[\[Gamma]],
						Undefined,
					!ListQ[\[Gamma]],
						Undefined,
					True,
						Undefined
				],
			!ListQ[diversityType] && diversityType == "EGC",
					Which[
					ListQ[\[Gamma]],
						Which[
							n == 2,
								Undefined,
							n == 3,
								Undefined,
							True,
								Undefined
						],
					!ListQ[\[Gamma]],
						Which[
							n == 2,
								Undefined,
							n == 3,
								Undefined,
							True,
								Undefined
						],
					True,
						Undefined
				],
			!ListQ[diversityType] && diversityType == "SC",
				Which[
					ListQ[\[Gamma]],
						Undefined,
					!ListQ[\[Gamma]],
						Undefined,
					True,
						Undefined
				],
			ListQ[diversityType] && diversityType[[1]] == "SSC",
				Which[
					ListQ[\[Gamma]],
						Module[{\[Gamma]t = diversityType[[2]]},
							Undefined
						],
					!ListQ[\[Gamma]],
						Module[{\[Gamma]t = diversityType[[2]]},
							Undefined
						],
					True,
						Undefined
				],
			!ListQ[diversityType] && diversityType == "SLC",
				Which[
					ListQ[\[Gamma]],
						AWGNProbabilityOfFalseAlarm[M,\[Lambda],n] + (1 - AWGNProbabilityOfFalseAlarm[M,\[Lambda],n]) ( Gamma[m n,(m (-M n+\[Lambda]))/(M Mean[\[Gamma]])]/Gamma[m n]),
					!ListQ[\[Gamma]],
						AWGNProbabilityOfFalseAlarm[M,\[Lambda],n] + (1 - AWGNProbabilityOfFalseAlarm[M,\[Lambda],n]) ( Gamma[m n,(m (-M n+\[Lambda]))/(M \[Gamma])]/Gamma[m n]),
					True,
						Undefined
				],
			!ListQ[diversityType] && diversityType == "SLS",
				Which[
					ListQ[\[Gamma]],
						Undefined,
					!ListQ[\[Gamma]],
						Undefined,
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
LargeMNNakagamiProbabilityOfDetection[M_?NumericQ,\[Gamma]_,\[Lambda]_,m_?NumericQ,OptionsPattern[]]:=Module[{RelevantOptions, n = 1},
	RelevantOptions[target_]:=FilterRules[Table[#[[i]]->OptionValue[#[[i]]],{i,Length[#]}]&[Options[LargeMNNakagamiProbabilityOfDetection][[All,1]]],Options[target][[All,1]]];
	LargeMNNakagamiProbabilityOfDetection[M,\[Gamma],\[Lambda],m,n,#/.(DiversityType/.#)->"None"&[RelevantOptions[LargeMNNakagamiProbabilityOfDetection]]]
]
LargeMNNakagamiProbabilityOfDetection[M_?NumericQ,\[Gamma]_,\[Lambda]_,m_?NumericQ,n_?NumericQ,OptionsPattern[]]:=Module[{lim, f, totaltime = 0, iterations = 0, time, result, diversityType = OptionValue[DiversityType]},
	f:=Which[
		!ListQ[diversityType] && diversityType == "None",
			Which[
				ListQ[\[Gamma]],
					Undefined,
				!ListQ[\[Gamma]],
					(1/2 (1+Erf[(m (M (1+\[Gamma])-\[Lambda]))/(Sqrt[2] M Sqrt[m] \[Gamma])])+1/2 E^((4 c M (-m+a M \[Gamma]^2)+b (-b M^2 \[Gamma]^2+2 Sqrt[2] m Sqrt[M] (M (1+\[Gamma])-\[Lambda]))-2 a m (-M (1+\[Gamma])+\[Lambda])^2)/(4 M (-m+a M \[Gamma]^2))) Sqrt[m/(m-a M \[Gamma]^2)] (Erf[(b Sqrt[M^3] \[Gamma]^2+Sqrt[2] m (-M (1+\[Gamma])+\[Lambda]))/(2 M \[Gamma] Sqrt[(m-a M \[Gamma]^2)])]-Erf[(-2 m+\[Gamma] (-2 a M+Sqrt[2] b Sqrt[M]+2 a \[Lambda]))/(2 Sqrt[2] Sqrt[(m-a M \[Gamma]^2)])])+1/2 E^(-((4 c M (m-a M \[Gamma]^2)+b (b M^2 \[Gamma]^2+2 Sqrt[2] m Sqrt[M] (M (1+\[Gamma])-\[Lambda]))+2 a m (-M (1+\[Gamma])+\[Lambda])^2)/(4 M (-m+a M \[Gamma]^2)))) Sqrt[m/(m-a M \[Gamma]^2)] (-2+Erfc[(b Sqrt[M^3] \[Gamma]^2+Sqrt[2] m (M (1+\[Gamma])-\[Lambda]))/(2 M \[Gamma] Sqrt[(m-a M \[Gamma]^2)])]))/.LopezBenitezParameters[(-M (1+\[Gamma])+\[Lambda])/(2 Sqrt[M])],
				True,
					Undefined
			],
		!ListQ[diversityType] && diversityType == "MRC",
			Which[
				ListQ[\[Gamma]],
					(-((E^(-((b^2 M n Mean[\[Gamma]]^2+4 c (m-a M n Mean[\[Gamma]]^2)+(2 Sqrt[2] b m (M+M n Mean[\[Gamma]]-\[Lambda]))/Sqrt[M]+(2 a m (M+M n Mean[\[Gamma]]-\[Lambda])^2)/M)/(4 (-m+a M n Mean[\[Gamma]]^2)))) m (1+Erf[(b M^(3/2) n Mean[\[Gamma]]^2+Sqrt[2] m (M+M n Mean[\[Gamma]]-\[Lambda]))/(2 M Mean[\[Gamma]] Sqrt[n (m-a M n Mean[\[Gamma]]^2)])]))/(2 Sqrt[m (m-a M n Mean[\[Gamma]]^2)]))+(E^(-((b^2 M n Mean[\[Gamma]]^2+4 c (m-a M n Mean[\[Gamma]]^2)-(2 Sqrt[2] b m (M+M n Mean[\[Gamma]]-\[Lambda]))/Sqrt[M]+(2 a m (M+M n Mean[\[Gamma]]-\[Lambda])^2)/M)/(4 (-m+a M n Mean[\[Gamma]]^2)))) m (Erf[(Sqrt[2] b M^(3/2) n Mean[\[Gamma]]^2-2 m (M+M n Mean[\[Gamma]]-\[Lambda]))/(2 M n Sqrt[-2 a M+(2 m)/(n Mean[\[Gamma]]^2)] Mean[\[Gamma]]^2)]-Erf[(-2 m+Mean[\[Gamma]] (Sqrt[2] b Sqrt[M]-2 a M+2 a \[Lambda]))/(2 Sqrt[-2 a M+(2 m)/(n Mean[\[Gamma]]^2)] Mean[\[Gamma]])]))/(2 Sqrt[m (m-a M n Mean[\[Gamma]]^2)])+1/2 Erfc[-((m (M+M n Mean[\[Gamma]]-\[Lambda]))/(Sqrt[2] M Sqrt[m n] Mean[\[Gamma]]))])/.LopezBenitezParameters[(-M (1+Mean[\[Gamma]])+\[Lambda])/(2 Sqrt[M])],
				!ListQ[\[Gamma]],
					(-((E^(-((b^2 M n \[Gamma]^2+4 c (m-a M n \[Gamma]^2)+(2 Sqrt[2] b m (M+M n \[Gamma]-\[Lambda]))/Sqrt[M]+(2 a m (M+M n \[Gamma]-\[Lambda])^2)/M)/(4 (-m+a M n \[Gamma]^2)))) m (1+Erf[(b M^(3/2) n \[Gamma]^2+Sqrt[2] m (M+M n \[Gamma]-\[Lambda]))/(2 M \[Gamma] Sqrt[n (m-a M n \[Gamma]^2)])]))/(2 Sqrt[m (m-a M n \[Gamma]^2)]))+(E^(-((b^2 M n \[Gamma]^2+4 c (m-a M n \[Gamma]^2)-(2 Sqrt[2] b m (M+M n \[Gamma]-\[Lambda]))/Sqrt[M]+(2 a m (M+M n \[Gamma]-\[Lambda])^2)/M)/(4 (-m+a M n \[Gamma]^2)))) m (Erf[(Sqrt[2] b M^(3/2) n \[Gamma]^2-2 m (M+M n \[Gamma]-\[Lambda]))/(2 M n Sqrt[-2 a M+(2 m)/(n \[Gamma]^2)] \[Gamma]^2)]-Erf[(-2 m+\[Gamma] (Sqrt[2] b Sqrt[M]-2 a M+2 a \[Lambda]))/(2 Sqrt[-2 a M+(2 m)/(n \[Gamma]^2)] \[Gamma])]))/(2 Sqrt[m (m-a M n \[Gamma]^2)])+1/2 Erfc[-((m (M+M n \[Gamma]-\[Lambda]))/(Sqrt[2] M Sqrt[m n] \[Gamma]))])/.LopezBenitezParameters[(-M (1+\[Gamma])+\[Lambda])/(2 Sqrt[M])],
				True,
					Undefined
			],
		!ListQ[diversityType] && diversityType == "EGC",
			Undefined,
		!ListQ[diversityType] && diversityType == "SC",
			Undefined,
		ListQ[diversityType] && diversityType[[1]] == "SSC",
			Which[
				ListQ[\[Gamma]],
					Module[{\[Gamma]t = diversityType[[2]]},
						Which[
							-((M-\[Lambda])/M) < \[Gamma]t,
								(1 - GammaRegularized[m, m \[Gamma]t / Mean[\[Gamma]]]) LargeMNNakagamiProbabilityOfDetection[M, Mean[\[Gamma]], \[Lambda], m] + (-(1/(2 Sqrt[2])) E^(-((b^2 M^2 Mean[\[Gamma]]^2+4 c M (m-a M Mean[\[Gamma]]^2)+2 Sqrt[2] b m Sqrt[M] (M+M Mean[\[Gamma]]-\[Lambda])+2 a m (M+M Mean[\[Gamma]]-\[Lambda])^2)/(4 M (-m+a M Mean[\[Gamma]]^2)))) Sqrt[m] ((Sqrt[2] (1+Erf[(Sqrt[2] m+Mean[\[Gamma]] (b Sqrt[M]+Sqrt[2] a (M-\[Lambda])))/(2 Sqrt[m-a M Mean[\[Gamma]]^2])]))/Sqrt[m-a M Mean[\[Gamma]]^2]+((2 m (Mean[\[Gamma]]-\[Gamma]t)+Mean[\[Gamma]]^2 (Sqrt[2] b Sqrt[M]+2 a (M+M \[Gamma]t-\[Lambda]))) Erf[Abs[2 m (Mean[\[Gamma]]-\[Gamma]t)+Mean[\[Gamma]]^2 (Sqrt[2] b Sqrt[M]+2 a (M+M \[Gamma]t-\[Lambda]))]/(2 Sqrt[2] Mean[\[Gamma]] Sqrt[m-a M Mean[\[Gamma]]^2])])/Sqrt[(m-a M Mean[\[Gamma]]^2) (2 m^2 (Mean[\[Gamma]]-\[Gamma]t)^2-2 m Mean[\[Gamma]]^2 (-Mean[\[Gamma]]+\[Gamma]t) (Sqrt[2] b Sqrt[M]+2 a (M+M \[Gamma]t-\[Lambda]))+Mean[\[Gamma]]^4 (b^2 M+2 Sqrt[2] a b Sqrt[M] (M+M \[Gamma]t-\[Lambda])+2 a^2 (M+M \[Gamma]t-\[Lambda])^2))]-(Sqrt[2] (2 m+Mean[\[Gamma]] (Sqrt[2] b Sqrt[M]+2 a (M-\[Lambda]))) Erf[Abs[2 m+Sqrt[2] b Sqrt[M] Mean[\[Gamma]]+2 a M Mean[\[Gamma]]-2 a Mean[\[Gamma]] \[Lambda]]/(2 Sqrt[2] Sqrt[m-a M Mean[\[Gamma]]^2])])/(Sqrt[m-a M Mean[\[Gamma]]^2] Abs[2 m+Sqrt[2] b Sqrt[M] Mean[\[Gamma]]+2 a M Mean[\[Gamma]]-2 a Mean[\[Gamma]] \[Lambda]]))+1/2 Erfc[(Sqrt[m] (-Mean[\[Gamma]]+\[Gamma]t))/(Sqrt[2] Mean[\[Gamma]])])/.LopezBenitezParameters[(-M (1+Mean[\[Gamma]])+\[Lambda])/(2 Sqrt[M])],
							-(M-\[Lambda])/M >= \[Gamma]t,
								(1 - GammaRegularized[m, m \[Gamma]t / Mean[\[Gamma]]]) LargeMNNakagamiProbabilityOfDetection[M, Mean[\[Gamma]], \[Lambda], m] + (1/2 (-Erf[(Sqrt[m] (-Mean[\[Gamma]]+\[Gamma]t))/(Sqrt[2] Mean[\[Gamma]])]-Erf[(Sqrt[m] (M+M Mean[\[Gamma]]-\[Lambda]))/(Sqrt[2] M Mean[\[Gamma]])])-1/(2 Sqrt[2-(2 a M Mean[\[Gamma]]^2)/m]) E^(-((b^2 M^2 Mean[\[Gamma]]^2+4 c M (m-a M Mean[\[Gamma]]^2)+2 Sqrt[2] b m Sqrt[M] (M+M Mean[\[Gamma]]-\[Lambda])+2 a m (M+M Mean[\[Gamma]]-\[Lambda])^2)/(4 M (-m+a M Mean[\[Gamma]]^2)))) (Sqrt[2] (1+Erf[(Sqrt[2] m+Mean[\[Gamma]] (b Sqrt[M]+Sqrt[2] a (M-\[Lambda])))/(2 Sqrt[m-a M Mean[\[Gamma]]^2])])+((Sqrt[2] b M^(3/2) Mean[\[Gamma]]^2+2 m (M+M Mean[\[Gamma]]-\[Lambda])) Erf[Abs[Sqrt[2] b M^(3/2) Mean[\[Gamma]]^2+2 m (M+M Mean[\[Gamma]]-\[Lambda])]/(2 Sqrt[2] M Mean[\[Gamma]] Sqrt[m-a M Mean[\[Gamma]]^2])])/Sqrt[b^2 M^3 Mean[\[Gamma]]^4+2 Sqrt[2] b m M^(3/2) Mean[\[Gamma]]^2 (M+M Mean[\[Gamma]]-\[Lambda])+2 m^2 (M+M Mean[\[Gamma]]-\[Lambda])^2]-(Sqrt[2] (2 m+Mean[\[Gamma]] (Sqrt[2] b Sqrt[M]+2 a (M-\[Lambda]))) Erf[Abs[2 m+Sqrt[2] b Sqrt[M] Mean[\[Gamma]]+2 a M Mean[\[Gamma]]-2 a Mean[\[Gamma]] \[Lambda]]/(2 Sqrt[2] Sqrt[m-a M Mean[\[Gamma]]^2])])/Abs[2 m+Sqrt[2] b Sqrt[M] Mean[\[Gamma]]+2 a M Mean[\[Gamma]]-2 a Mean[\[Gamma]] \[Lambda]])-(E^((-b^2 M^2 Mean[\[Gamma]]^2+4 c M (-m+a M Mean[\[Gamma]]^2)+2 Sqrt[2] b m Sqrt[M] (M+M Mean[\[Gamma]]-\[Lambda])-2 a m (M+M Mean[\[Gamma]]-\[Lambda])^2)/(4 M (-m+a M Mean[\[Gamma]]^2))) Sqrt[m/(m-a M Mean[\[Gamma]]^2)] (((2 m (Mean[\[Gamma]]-\[Gamma]t)+Mean[\[Gamma]]^2 (-Sqrt[2] b Sqrt[M]+2 a (M+M \[Gamma]t-\[Lambda]))) Abs[Sqrt[2] b M^(3/2) Mean[\[Gamma]]^2-2 m (M+M Mean[\[Gamma]]-\[Lambda])] Erf[Abs[2 m (Mean[\[Gamma]]-\[Gamma]t)+Mean[\[Gamma]]^2 (-Sqrt[2] b Sqrt[M]+2 a (M+M \[Gamma]t-\[Lambda]))]/(2 Sqrt[2] Mean[\[Gamma]] Sqrt[m-a M Mean[\[Gamma]]^2])])/(m-a M Mean[\[Gamma]]^2)+2 (Sqrt[2] b M^(3/2) Mean[\[Gamma]]^2-2 m (M+M Mean[\[Gamma]]-\[Lambda])) Abs[\[Gamma]t+(Mean[\[Gamma]] (-2 m+Mean[\[Gamma]] (Sqrt[2] b Sqrt[M]+2 a (-M+\[Lambda]))))/(2 (m-a M Mean[\[Gamma]]^2))] Erf[Abs[Sqrt[2] b Sqrt[M] Mean[\[Gamma]]^2+m (-2-2 Mean[\[Gamma]]+(2 \[Lambda])/M)]/(2 Sqrt[2] Mean[\[Gamma]] Sqrt[m-a M Mean[\[Gamma]]^2])]))/(4 Abs[Sqrt[2] b M^(3/2) Mean[\[Gamma]]^2-2 m (M+M Mean[\[Gamma]]-\[Lambda])] Abs[\[Gamma]t+(Mean[\[Gamma]] (-2 m+Mean[\[Gamma]] (Sqrt[2] b Sqrt[M]+2 a (-M+\[Lambda]))))/(2 (m-a M Mean[\[Gamma]]^2))])+1/2 Erfc[-((Sqrt[m] (M+M Mean[\[Gamma]]-\[Lambda]))/(Sqrt[2] M Mean[\[Gamma]]))])/.LopezBenitezParameters[(-M (1+Mean[\[Gamma]])+\[Lambda])/(2 Sqrt[M])],
							True,
								Undefined
						]
					],
				!ListQ[\[Gamma]],
					Module[{\[Gamma]t = diversityType[[2]]},
						Which[
							-((M-\[Lambda])/M) < \[Gamma]t,
								(1 - GammaRegularized[m, m \[Gamma]t / \[Gamma]]) LargeMNNakagamiProbabilityOfDetection[M, \[Gamma], \[Lambda], m] + (-(1/(2 Sqrt[2])) E^(-((b^2 M^2 \[Gamma]^2+4 c M (m-a M \[Gamma]^2)+2 Sqrt[2] b m Sqrt[M] (M+M \[Gamma]-\[Lambda])+2 a m (M+M \[Gamma]-\[Lambda])^2)/(4 M (-m+a M \[Gamma]^2)))) Sqrt[m] ((Sqrt[2] (1+Erf[(Sqrt[2] m+\[Gamma] (b Sqrt[M]+Sqrt[2] a (M-\[Lambda])))/(2 Sqrt[m-a M \[Gamma]^2])]))/Sqrt[m-a M \[Gamma]^2]+((2 m (\[Gamma]-\[Gamma]t)+\[Gamma]^2 (Sqrt[2] b Sqrt[M]+2 a (M+M \[Gamma]t-\[Lambda]))) Erf[Abs[2 m (\[Gamma]-\[Gamma]t)+\[Gamma]^2 (Sqrt[2] b Sqrt[M]+2 a (M+M \[Gamma]t-\[Lambda]))]/(2 Sqrt[2] \[Gamma] Sqrt[m-a M \[Gamma]^2])])/Sqrt[(m-a M \[Gamma]^2) (2 m^2 (\[Gamma]-\[Gamma]t)^2-2 m \[Gamma]^2 (-\[Gamma]+\[Gamma]t) (Sqrt[2] b Sqrt[M]+2 a (M+M \[Gamma]t-\[Lambda]))+\[Gamma]^4 (b^2 M+2 Sqrt[2] a b Sqrt[M] (M+M \[Gamma]t-\[Lambda])+2 a^2 (M+M \[Gamma]t-\[Lambda])^2))]-(Sqrt[2] (2 m+\[Gamma] (Sqrt[2] b Sqrt[M]+2 a (M-\[Lambda]))) Erf[Abs[2 m+Sqrt[2] b Sqrt[M] \[Gamma]+2 a M \[Gamma]-2 a \[Gamma] \[Lambda]]/(2 Sqrt[2] Sqrt[m-a M \[Gamma]^2])])/(Sqrt[m-a M \[Gamma]^2] Abs[2 m+Sqrt[2] b Sqrt[M] \[Gamma]+2 a M \[Gamma]-2 a \[Gamma] \[Lambda]]))+1/2 Erfc[(Sqrt[m] (-\[Gamma]+\[Gamma]t))/(Sqrt[2] \[Gamma])])/.LopezBenitezParameters[(-M (1+\[Gamma])+\[Lambda])/(2 Sqrt[M])],
							-(M-\[Lambda])/M >= \[Gamma]t,
								(1 - GammaRegularized[m, m \[Gamma]t / \[Gamma]]) LargeMNNakagamiProbabilityOfDetection[M, \[Gamma], \[Lambda], m] + (1/2 (-Erf[(Sqrt[m] (-\[Gamma]+\[Gamma]t))/(Sqrt[2] \[Gamma])]-Erf[(Sqrt[m] (M+M \[Gamma]-\[Lambda]))/(Sqrt[2] M \[Gamma])])-1/(2 Sqrt[2-(2 a M \[Gamma]^2)/m]) E^(-((b^2 M^2 \[Gamma]^2+4 c M (m-a M \[Gamma]^2)+2 Sqrt[2] b m Sqrt[M] (M+M \[Gamma]-\[Lambda])+2 a m (M+M \[Gamma]-\[Lambda])^2)/(4 M (-m+a M \[Gamma]^2)))) (Sqrt[2] (1+Erf[(Sqrt[2] m+\[Gamma] (b Sqrt[M]+Sqrt[2] a (M-\[Lambda])))/(2 Sqrt[m-a M \[Gamma]^2])])+((Sqrt[2] b M^(3/2) \[Gamma]^2+2 m (M+M \[Gamma]-\[Lambda])) Erf[Abs[Sqrt[2] b M^(3/2) \[Gamma]^2+2 m (M+M \[Gamma]-\[Lambda])]/(2 Sqrt[2] M \[Gamma] Sqrt[m-a M \[Gamma]^2])])/Sqrt[b^2 M^3 \[Gamma]^4+2 Sqrt[2] b m M^(3/2) \[Gamma]^2 (M+M \[Gamma]-\[Lambda])+2 m^2 (M+M \[Gamma]-\[Lambda])^2]-(Sqrt[2] (2 m+\[Gamma] (Sqrt[2] b Sqrt[M]+2 a (M-\[Lambda]))) Erf[Abs[2 m+Sqrt[2] b Sqrt[M] \[Gamma]+2 a M \[Gamma]-2 a \[Gamma] \[Lambda]]/(2 Sqrt[2] Sqrt[m-a M \[Gamma]^2])])/Abs[2 m+Sqrt[2] b Sqrt[M] \[Gamma]+2 a M \[Gamma]-2 a \[Gamma] \[Lambda]])-(E^((-b^2 M^2 \[Gamma]^2+4 c M (-m+a M \[Gamma]^2)+2 Sqrt[2] b m Sqrt[M] (M+M \[Gamma]-\[Lambda])-2 a m (M+M \[Gamma]-\[Lambda])^2)/(4 M (-m+a M \[Gamma]^2))) Sqrt[m/(m-a M \[Gamma]^2)] (((2 m (\[Gamma]-\[Gamma]t)+\[Gamma]^2 (-Sqrt[2] b Sqrt[M]+2 a (M+M \[Gamma]t-\[Lambda]))) Abs[Sqrt[2] b M^(3/2) \[Gamma]^2-2 m (M+M \[Gamma]-\[Lambda])] Erf[Abs[2 m (\[Gamma]-\[Gamma]t)+\[Gamma]^2 (-Sqrt[2] b Sqrt[M]+2 a (M+M \[Gamma]t-\[Lambda]))]/(2 Sqrt[2] \[Gamma] Sqrt[m-a M \[Gamma]^2])])/(m-a M \[Gamma]^2)+2 (Sqrt[2] b M^(3/2) \[Gamma]^2-2 m (M+M \[Gamma]-\[Lambda])) Abs[\[Gamma]t+(\[Gamma] (-2 m+\[Gamma] (Sqrt[2] b Sqrt[M]+2 a (-M+\[Lambda]))))/(2 (m-a M \[Gamma]^2))] Erf[Abs[Sqrt[2] b Sqrt[M] \[Gamma]^2+m (-2-2 \[Gamma]+(2 \[Lambda])/M)]/(2 Sqrt[2] \[Gamma] Sqrt[m-a M \[Gamma]^2])]))/(4 Abs[Sqrt[2] b M^(3/2) \[Gamma]^2-2 m (M+M \[Gamma]-\[Lambda])] Abs[\[Gamma]t+(\[Gamma] (-2 m+\[Gamma] (Sqrt[2] b Sqrt[M]+2 a (-M+\[Lambda]))))/(2 (m-a M \[Gamma]^2))])+1/2 Erfc[-((Sqrt[m] (M+M \[Gamma]-\[Lambda]))/(Sqrt[2] M \[Gamma]))])/.LopezBenitezParameters[(-M (1+\[Gamma])+\[Lambda])/(2 Sqrt[M])],
							True,
								Undefined
						]
					],
				True,
					Undefined
			],
		!ListQ[diversityType] && diversityType == "SLC",
			Which[
				ListQ[\[Gamma]],
					(1/2 (1+Erf[(m (M n (1+Mean[\[Gamma]])-\[Lambda]))/(Sqrt[2] M Sqrt[m n] Mean[\[Gamma]])])+1/2 E^((4 c M n (-m+a M Mean[\[Gamma]]^2)+b (-b M^2 n Mean[\[Gamma]]^2+2 Sqrt[2] m Sqrt[M n] (M n (1+Mean[\[Gamma]])-\[Lambda]))-2 a m (-M n (1+Mean[\[Gamma]])+\[Lambda])^2)/(4 M n (-m+a M Mean[\[Gamma]]^2))) Sqrt[m/(m-a M Mean[\[Gamma]]^2)] (Erf[(b Sqrt[M^3 n] Mean[\[Gamma]]^2+Sqrt[2] m (-M n (1+Mean[\[Gamma]])+\[Lambda]))/(2 M Mean[\[Gamma]] Sqrt[n (m-a M Mean[\[Gamma]]^2)])]-Erf[(-2 m n+Mean[\[Gamma]] (-2 a M n+Sqrt[2] b Sqrt[M n]+2 a \[Lambda]))/(2 Sqrt[2] Sqrt[n (m-a M Mean[\[Gamma]]^2)])])+1/2 E^(-((4 c M n (m-a M Mean[\[Gamma]]^2)+b (b M^2 n Mean[\[Gamma]]^2+2 Sqrt[2] m Sqrt[M n] (M n (1+Mean[\[Gamma]])-\[Lambda]))+2 a m (-M n (1+Mean[\[Gamma]])+\[Lambda])^2)/(4 M n (-m+a M Mean[\[Gamma]]^2)))) Sqrt[m/(m-a M Mean[\[Gamma]]^2)] (-2+Erfc[(b Sqrt[M^3 n] Mean[\[Gamma]]^2+Sqrt[2] m (M n (1+Mean[\[Gamma]])-\[Lambda]))/(2 M Mean[\[Gamma]] Sqrt[n (m-a M Mean[\[Gamma]]^2)])]))/.LopezBenitezParameters[(-M (n+Mean[\[Gamma]])+\[Lambda])/(2 Sqrt[M n])],
				!ListQ[\[Gamma]],
					(1/2 (1+Erf[(m (M n (1+\[Gamma])-\[Lambda]))/(Sqrt[2] M Sqrt[m n] \[Gamma])])+1/2 E^((4 c M n (-m+a M \[Gamma]^2)+b (-b M^2 n \[Gamma]^2+2 Sqrt[2] m Sqrt[M n] (M n (1+\[Gamma])-\[Lambda]))-2 a m (-M n (1+\[Gamma])+\[Lambda])^2)/(4 M n (-m+a M \[Gamma]^2))) Sqrt[m/(m-a M \[Gamma]^2)] (Erf[(b Sqrt[M^3 n] \[Gamma]^2+Sqrt[2] m (-M n (1+\[Gamma])+\[Lambda]))/(2 M \[Gamma] Sqrt[n (m-a M \[Gamma]^2)])]-Erf[(-2 m n+\[Gamma] (-2 a M n+Sqrt[2] b Sqrt[M n]+2 a \[Lambda]))/(2 Sqrt[2] Sqrt[n (m-a M \[Gamma]^2)])])+1/2 E^(-((4 c M n (m-a M \[Gamma]^2)+b (b M^2 n \[Gamma]^2+2 Sqrt[2] m Sqrt[M n] (M n (1+\[Gamma])-\[Lambda]))+2 a m (-M n (1+\[Gamma])+\[Lambda])^2)/(4 M n (-m+a M \[Gamma]^2)))) Sqrt[m/(m-a M \[Gamma]^2)] (-2+Erfc[(b Sqrt[M^3 n] \[Gamma]^2+Sqrt[2] m (M n (1+\[Gamma])-\[Lambda]))/(2 M \[Gamma] Sqrt[n (m-a M \[Gamma]^2)])]))/.LopezBenitezParameters[(-M (n+\[Gamma])+\[Lambda])/(2 Sqrt[M n])],
				True,
					Undefined
			],
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
NGaussianNakagamiProbabilityOfDetection[M_?NumericQ,\[Gamma]_,\[Lambda]_,m_?NumericQ,OptionsPattern[]]:=Module[{RelevantOptions, n = 1},
	RelevantOptions[target_]:=FilterRules[Table[#[[i]]->OptionValue[#[[i]]],{i,Length[#]}]&[Options[NGaussianNakagamiProbabilityOfDetection][[All,1]]],Options[target][[All,1]]];
	NGaussianNakagamiProbabilityOfDetection[M,\[Gamma],\[Lambda],m,n,#/.(DiversityType/.#)->"None"&[RelevantOptions[NGaussianNakagamiProbabilityOfDetection]]]
]
NGaussianNakagamiProbabilityOfDetection[M_?NumericQ,\[Gamma]_,\[Lambda]_,m_?NumericQ,n_?IntegerQ,OptionsPattern[]]:=Module[{lim, f, totaltime = 0, iterations = 0, time, result, RelevantOptions, diversityType = OptionValue[DiversityType]},
	RelevantOptions[target_]:=FilterRules[Table[#[[i]]->OptionValue[#[[i]]],{i,Length[#]}]&[Options[NGaussianNakagamiProbabilityOfDetection][[All,1]]],Options[target][[All,1]]];
	f:=Which[
		!ListQ[diversityType] && diversityType == "None",
			NIntegrate[AWGNProbabilityOfDetection[M,x,\[Lambda],RelevantOptions[AWGNProbabilityOfDetection]]NakagamiPDF[\[Gamma],m,x,DiversityType->diversityType],{x,0,\[Infinity]}],
		!ListQ[diversityType] && diversityType == "MRC",
			NIntegrate[AWGNProbabilityOfDetection[M,x/n,\[Lambda],n,RelevantOptions[AWGNProbabilityOfDetection]]NakagamiPDF[\[Gamma],m,x,n,DiversityType->diversityType],{x,0,\[Infinity]}],
		!ListQ[diversityType] && diversityType == "EGC",
			NIntegrate[AWGNProbabilityOfDetection[M,x/n,\[Lambda],n,RelevantOptions[AWGNProbabilityOfDetection]]NakagamiPDF[\[Gamma],m,x,n,DiversityType->diversityType],{x,0,\[Infinity]}],
		!ListQ[diversityType] && diversityType == "SC",
			NIntegrate[AWGNProbabilityOfDetection[M,x,\[Lambda],RelevantOptions[AWGNProbabilityOfDetection]]NakagamiPDF[\[Gamma],m,x,n,DiversityType->diversityType],{x,0,\[Infinity]}],
		ListQ[diversityType] && diversityType[[1]] == "SSC",
			NIntegrate[AWGNProbabilityOfDetection[M,x,\[Lambda],RelevantOptions[AWGNProbabilityOfDetection]]NakagamiPDF[\[Gamma],m,x,DiversityType->diversityType],{x,0,\[Infinity]}],
		!ListQ[diversityType] && diversityType == "SLC",
			NIntegrate[AWGNProbabilityOfDetection[M,x/n,\[Lambda],n,RelevantOptions[AWGNProbabilityOfDetection]]NakagamiPDF[\[Gamma],m,x,n,DiversityType->diversityType],{x,0,\[Infinity]}],
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
			fineGuess = M/.FindRoot[NakagamiProbabilityOfDetection[M,\[Gamma],\[Lambda][M,Pf,n],m,n,RelevantOptions[NakagamiProbabilityOfDetection]] == Pd, {M, intialGuess, 1, \[Infinity]}];
			result = NakagamiProbabilityOfDetection[fineGuess,\[Gamma],\[Lambda][fineGuess,Pf,n],m,n,RelevantOptions[NakagamiProbabilityOfDetection]];
		];,
		"Exact",
		(* If Gaussian approximation is valid, then use it to speed up the calculation *)
		If[intialGuess <= 250,
			courseGuess = intialGuess;,
			courseGuess = M/.FindRoot[NakagamiProbabilityOfDetection[M,\[Gamma],\[Lambda][M,Pf,n],m,n,RelevantOptions[NakagamiProbabilityOfDetection]] == Pd, {M, intialGuess, 1, \[Infinity]}];
		];
		fineGuess = M/.FindRoot[NakagamiProbabilityOfDetection[courseGuess,\[Gamma],\[Lambda][courseGuess,Pf,n,RelevantOptions[\[Lambda]]],m,n] == Pd, {M, courseGuess, 1, \[Infinity]}];
		result = NakagamiProbabilityOfDetection[fineGuess,\[Gamma],\[Lambda][fineGuess,Pf,n,RelevantOptions[\[Lambda]]],m,n];
	];
	On[FindRoot::reged,FindRoot::lstol];
	If[Abs[result - Pd] <= tol//TrueQ,
		fineGuess,
		Message[NNakagamiSampleComplexity::tol, result//N, Pd//N, tol//N]
	]
]


(* ::Subsection::Closed:: *)
(*Miscellaenous*)


MultinomialCoefficient[l_?NumericQ,k_?NumericQ,m_?NumericQ]:=Which[
	k == 0,
		1,
	k == 1,
		l,
	k == (m - 1) l,
		1 / (Gamma[m]^l),
	2 <= k <= (m - 1) l - 1,
		(1 / k) Total[Table[MultinomialCoefficient[l, k - j, m] (j (l + 1) - k) / j!, {j, 1, Min[k, m - 1]}]],
	True,
		Undefined
]


End[];


EndPackage[];
