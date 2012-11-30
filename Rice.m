(* ::Package:: *)

(* ::Title:: *)
(*Rice channel functions*)


(* ::Subsection::Closed:: *)
(*Copyright notice*)


(* ::Text:: *)
(*Mathematica function definitions for cooperative energy detection in Rice channels.*)
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
(*30/11/2012*)
(*1.1*)


(* ::Subsection:: *)
(*Changelog*)


(* ::Text:: *)
(*Version 1.1: Major clean up of code, added approximations and numerical methods for no diversity Rice channels.*)
(*Version 1.02: Moved database logging functions to the Network package.*)
(*Version 1.01: Added sample complexity function.*)
(*Version 1.0: First working version, minor bug fixes to follow.*)


(* ::Section:: *)
(*Public*)


BeginPackage["Rice`"];


(* ::Subsection::Closed:: *)
(*PDF of the signal to noise ratio*)


RicePDF;


(* ::Subsection:: *)
(*Probabiity of detection*)


(* ::Subsubsection::Closed:: *)
(*Main function*)


RiceProbabilityOfDetection;


(* ::Subsubsection::Closed:: *)
(*Annamalai's method*)


AnnamalaiRiceProbabilityOfDetection;


AnnamalaiRiceLimit;


(* ::Subsubsection::Closed:: *)
(*Small-K method*)


SmallKRiceProbabilityOfDetection;


(* ::Subsubsection::Closed:: *)
(*Asymptotic method*)


AsymptoticRiceProbabilityOfDetection;


(* ::Subsubsection::Closed:: *)
(*Numerical Gaussian method*)


NGaussianRiceProbabilityOfDetection;


(* ::Subsection::Closed:: *)
(*Sample complexity*)


NRiceSampleComplexity


(* ::Section:: *)
(*Private*)


Begin["`Private`"];


<<Network`;
<<AWGN`;
<<ErfApprox`;
<<QFunction`;
<<Extras`;


(* ::Subsection::Closed:: *)
(*Help generation*)


AlgorithmHelp[fName_, methods_, algorithms_] := Module[{help, n, m},
	For[n = 1, n <= Length[methods], n++,
		If[n == 1,
			help = "If Method\[Rule]\"" <> ToString[methods[[n]]] <> "\", then the following algorithms may be specified:\n\n",
			help = help <> "\nIf Method\[Rule]\"" <> ToString[methods[[n]]] <> "\", then the following algorithms may be specified:\n\n"
		];

		For[m = 1, m <= Length[algorithms[[n]]], m++,
			help = help <> "Algorithm\[Rule]" <> ToString[algorithms[[n, m]]] <> "\n"
		];
	];
	help = help <> "\n" <> Evaluate[DefaultHelp[fName, {Method, Algorithm}]]
];


GenerateAlgorithmHelp[fName_, algorithmName_] := ToString[fName] <> "[M, \[Gamma], \[Lambda], K] calculates the probability of detection for a single energy detector operating in a Rice fading channel using the " <> algorithmName <> " algorithm.
" <> ToString[fName] <> "[M, \[Gamma], \[Lambda], K, n] calculates the probability of detection for energy detection with diversity reception in a Rice fading channel using the " <> algorithmName <> " algorithm."<>"\n\n"<>DiversityTypeHelp[fName]<>"\n\n"<>TimingHelp[fName];


GenerateTruncationHelp[fName_,algorithmName_] := ToString[fName] <> "[M, \[Gamma], \[Lambda], K] calculates the truncation point for use in the " <> algorithmName <> " algorithm for a single energy detector operating on a Rice channel.
" <> ToString[fName] <> "[M, \[Gamma], \[Lambda], K, n] calculates the truncation point for use in the " <> algorithmName <> " algorithm for energy detection with diversity reception in a Rice channel."<>"\n\n"<>DiversityTypeHelp[fName]<>"\n\n"<>ToleranceHelp[fName];


(* ::Subsection::Closed:: *)
(*PDF of the signal to noise ratio*)


Options[RicePDF] = {Method->"Exact", DiversityType->OptionValue[ProbabilityOfDetection,Algorithm]};
RicePDF::usage="RicePDF[\[Gamma], K, x] evaluates the probability density function of the instantaneous signal to noise ratio at a single energy detector operating on a Rice fading channel at x.
RicePDF[\[Gamma], K, x, n] evaluates the probability density function of the average instantaneous signal to noise ratio for energy detection with diversity reception in a Rice fading channel.\n\n"<>MethodHelp[RicePDF]<>"\n\n"<>DiversityTypeHelp[RicePDF];
RicePDF[\[Gamma]_,K_,x_,OptionsPattern[]]:=Module[{n = 1}, RicePDF[\[Gamma], K, x, n, Method->OptionValue[Method], DiversityType->OptionValue[DiversityType]]]
RicePDF[\[Gamma]_,K_,x_,n_,OptionsPattern[]]:=Module[{method = OptionValue[Method], diversityType = OptionValue[DiversityType], \[Gamma]t, \[Gamma]0, g},
	(* Handle both lists and scalar values for diversityType *)
	{diversityType, \[Gamma]t} = ProcessDiversityType[diversityType];
	
	(* Convert lists of SNR values to averages or maxima, depending on the specified diversity type *)
	\[Gamma]0 = ProcessSNR[\[Gamma], diversityType];

	(* Check for invalid combinations of inputs *)
	If[diversityType == "None" && n > 1, Return[Undefined]];
	If[\[Gamma]0 == Undefined, Return[Undefined]];

	Which[
		method == "Exact",
			Which[
				diversityType == "None",
					((2 (K + 1)) / \[Gamma]) PDF[NoncentralChiSquareDistribution[2, 2K], (2 (K + 1) x) / \[Gamma]],
				diversityType == "MRC",
					Undefined,
				diversityType == "EGC",
					Undefined,
				diversityType == "SC",
					Undefined,
				diversityType == "SEC",
					Undefined,
				diversityType == "SLC",
					(K + 1) / \[Gamma] Exp[-K n - (K + 1) x / \[Gamma]]((K + 1) x / (K n \[Gamma]))^((n - 1) / 2) BesselI[n - 1, 2 Sqrt[K n (K + 1)x / \[Gamma]]],
				!ListQ[diversityType] && diversityType == "SLS",
					Undefined,
				True,
					Undefined
			],
		method == "Approximate",
			Which[
				diversityType == "None",
					PDF[NormalDistribution[\[Gamma], Sqrt[2K + 1](\[Gamma] / (K + 1))], x],
				diversityType == "MRC",
					Undefined,
				diversityType == "EGC",
					Undefined,
				diversityType == "SC",
					Undefined,
				diversityType == "SEC",
					Undefined,
				diversityType == "SLC",
					2 (K + 1) / \[Gamma] PDF[NormalDistribution[2 n + 2 K n, Sqrt[4 n + 8 K n]], 2 (K + 1) x / \[Gamma]],
				diversityType == "SLS",
					Undefined,
				True,
					Undefined
			],
		True,
			Undefined
	]
]


(* ::Subsection:: *)
(*Probabiity of detection*)


(* ::Subsubsection::Closed:: *)
(*Main function*)


Options[RiceProbabilityOfDetection]={Method->OptionValue[ProbabilityOfDetection,Method],Algorithm->OptionValue[ProbabilityOfDetection,Algorithm],LowSNR->OptionValue[ProbabilityOfDetection,LowSNR],DiversityType->OptionValue[ProbabilityOfDetection,DiversityType],Timed->OptionValue[ProbabilityOfDetection,Timed],MaxTime->OptionValue[ProbabilityOfDetection,MaxTime],MaxIterations->OptionValue[ProbabilityOfDetection,MaxIterations]};
RiceProbabilityOfDetection::usage="RiceProbabilityOfDetection[M, \[Gamma], \[Lambda], K] calculates the probability of detection for a single energy detector operating on a Rice fading channel.
RiceProbabilityOfDetection[M, \[Gamma], \[Lambda], K, n] calculates the probability of detection for energy detection with diversity reception in a Rice fading channel.\n\n"<>MethodHelp[RiceProbabilityOfDetection]<>"\n\n"<>AlgorithmHelp[RiceProbabilityOfDetection, {"Approximate", "Exact"}, {{"\"SmallK\"", "\"Asymptotic\"", "\"NGaussian\""},{"\"Annamalai\"", "\"Herath\"", "\"Numerical\""}}]<>"\n\n"<>LowSNRHelp<>"\n\n"<>DiversityTypeHelp[RiceProbabilityOfDetection]<>"\n\n"<>TimingHelp[RiceProbabilityOfDetection];
RiceProbabilityOfDetection[M_,\[Gamma]_,\[Lambda]_,K_,OptionsPattern[]]:=Module[{n = 1, RelevantOptions},
	RelevantOptions[target_]:=FilterRules[Table[#[[i]]->OptionValue[#[[i]]],{i,Length[#]}]&[Options[RiceProbabilityOfDetection][[All,1]]],Options[target][[All,1]]];
	RiceProbabilityOfDetection[M, \[Gamma], \[Lambda], K, n, RelevantOptions[RiceProbabilityOfDetection]]
]
RiceProbabilityOfDetection[M_,\[Gamma]_,\[Lambda]_,K_,n_,OptionsPattern[]]:=Module[{RelevantOptions, method = OptionValue[Method], algorithm = OptionValue[Algorithm]},
	RelevantOptions[target_]:=FilterRules[Table[#[[i]]->OptionValue[#[[i]]],{i,Length[#]}]&[Options[RiceProbabilityOfDetection][[All,1]]],Options[target][[All,1]]];

	Which[
		method == "Exact",
			Which[
				algorithm == "Annamalai",
					AnnamalaiRiceProbabilityOfDetection[M,\[Gamma],\[Lambda],K,n,RelevantOptions[AnnamalaiRiceProbabilityOfDetection]],
				algorithm == "Herath",
					HerathRiceProbabilityOfDetection[M,\[Gamma],\[Lambda],K,n,RelevantOptions[HerathRiceProbabilityOfDetection]],
				algorithm == "Numerical",
					NumericalRiceProbabilityOfDetection[M,\[Gamma],\[Lambda],K,n,RelevantOptions[NumericalRiceProbabilityOfDetection]],
				True,
					Undefined
			],
		method == "Approximate",
			Which[
				!ListQ[algorithm] && algorithm == "SmallK",
					SmallKRiceProbabilityOfDetection[M,\[Gamma],\[Lambda],K,n,RelevantOptions[SmallKRiceProbabilityOfDetection]],
				!ListQ[algorithm] && algorithm == "Asymptotic",
					AsymptoticRiceProbabilityOfDetection[M,\[Gamma],\[Lambda],K,n,RelevantOptions[AsymptoticRiceProbabilityOfDetection]],
				!ListQ[algorithm] && algorithm == "NGaussian",
					NGaussianRiceProbabilityOfDetection[M,\[Gamma],\[Lambda],K,n,RelevantOptions[NGaussianRiceProbabilityOfDetection]],
				True,
					Undefined
			],
		True,
			Undefined
	]
]


(* ::Subsubsection:: *)
(*Annamalai's method*)


Options[AnnamalaiRiceProbabilityOfDetection] = {DiversityType->OptionValue[RiceProbabilityOfDetection,DiversityType],Timed->OptionValue[RiceProbabilityOfDetection,Timed],MaxTime->OptionValue[RiceProbabilityOfDetection,MaxTime],MaxIterations->OptionValue[RiceProbabilityOfDetection,MaxIterations]};
AnnamalaiRiceProbabilityOfDetection::usage = GenerateAlgorithmHelp[AnnamalaiRiceProbabilityOfDetection, "Annamalai"];
AnnamalaiRiceProbabilityOfDetection[M_?NumericQ,\[Gamma]_?NumericQ,\[Lambda]_,K_?NumericQ,OptionsPattern[]]:=Module[{RelevantOptions, n = 1},
	RelevantOptions[target_]:=FilterRules[Table[#[[i]]->OptionValue[#[[i]]],{i,Length[#]}]&[Options[AnnamalaiRiceProbabilityOfDetection][[All,1]]],Options[target][[All,1]]];
	AnnamalaiRiceProbabilityOfDetection[M,\[Gamma],\[Lambda],K,n,RelevantOptions[AnnamalaiRiceProbabilityOfDetection]]
]
AnnamalaiRiceProbabilityOfDetection[M_?NumericQ,\[Gamma]_?NumericQ,\[Lambda]_,K_?NumericQ,n_?IntegerQ,OptionsPattern[]]:=Module[{lim, f, \[Gamma]0, \[Gamma]t, totaltime = 0, iterations = 0, time, result, diversityType = OptionValue[DiversityType]},
	(* Handle both lists and scalar values for diversityType *)
	{diversityType, \[Gamma]t} = ProcessDiversityType[diversityType];
	
	(* Convert lists of SNR values to averages or maxima, depending on the specified diversity type *)
	\[Gamma]0 = ProcessSNR[\[Gamma], diversityType];

	(* Check for invalid combinations of inputs *)
	If[diversityType == "None" && n > 1, Return[Undefined]];
	If[\[Gamma]0 == Undefined, Return[Undefined]];

	lim = AnnamalaiRiceLimit[M,\[Gamma],\[Lambda],K,n,DiversityType->diversityType];

	f := Which[
		diversityType == "None",
			1 - (2 (1 + K) / (2 + 2K + M \[Gamma]0)) Exp[-K M \[Gamma]0 / (2 + 2K + M \[Gamma]0)] (1 - GammaRegularized[M/2, \[Lambda] / 2]) - Total[Table[k! (2 (1 + K) / (2 + 2K + M \[Gamma]0)) (((M \[Gamma]0) / (2 + 2K + M \[Gamma]0))^k) Exp[-K M \[Gamma]0 / (2 + 2K + M \[Gamma]0)] (1 / k! + Total[Table[((2 K (1 + K) / (2 + 2K + M \[Gamma]0))^i) / (i! i! (k - i)!),{i , 1, k}]]) (1 - GammaRegularized[M / 2 + k, \[Lambda] / 2]),{k, 1, lim}]],
		diversityType == "MRC",
			1 - ((1 - GammaRegularized[M / 2, \[Lambda] / 2]) (2 (1 + K) / (2 + 2K + M \[Gamma]0))^n Exp[-K M n \[Gamma]0 / (2 + 2K + M \[Gamma]0)]) - Total[Table[(1 - GammaRegularized[M / 2 + k, \[Lambda] / 2]) (n + k - 1)! ((M \[Gamma]0) / (2 + 2K + M \[Gamma]0))^k (2 (1 + K) / (2 + 2K + M \[Gamma]0))^n Exp[-K M n \[Gamma]0 / (2 + 2K + M \[Gamma]0)] Total[Table[(1 / (i! (n + i - 1)! (k - i)!)) (2K n (K + 1) / (2 + 2K + M \[Gamma]))^i,{i, 0, k}]], {k, 1, lim}]],
		diversityType == "EGC",
			Undefined,
		diversityType == "SC",
			Undefined,
		diversityType == "SEC",
			Undefined,
		diversityType == "SLC",
			1 - ((2 (1 + K) / (2 + 2K + M \[Gamma]0))^n) Exp[-K n M \[Gamma]0 / (2 + 2K + M \[Gamma]0)] (1 - GammaRegularized[M n/2, \[Lambda] / 2]) - Total[Table[(n + k - 1)! ((2 (1 + K) / (2 + 2K + M \[Gamma]0))^n) (((M \[Gamma]0) / (2 + 2K + M \[Gamma]0))^k) Exp[-K n M \[Gamma]0 / (2 + 2K + M \[Gamma]0)] (1 / ((n - 1)! k!) + Total[Table[((2 K n (1 + K) / (2 + 2K + M \[Gamma]0))^i) / (i! (n + i - 1)! (k - i)!),{i , 1, k}]]) (1 - GammaRegularized[M n / 2 + k, \[Lambda] / 2]),{k, 1, lim}]],
		diversityType == "SLS",
			Undefined,
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


Options[AnnamalaiRiceLimit]={DiversityType->OptionValue[AnnamalaiRiceProbabilityOfDetection,DiversityType], Tolerance->10^-6};
AnnamalaiRiceLimit::usage = GenerateTruncationHelp[AnnamalaiRiceLimit, "Annamalai"];
AnnamalaiRiceLimit[M_?NumericQ,\[Gamma]_,\[Lambda]_,K_?NumericQ,OptionsPattern[]]:=Module[{n = 1},AnnamalaiRiceLimit[M,\[Gamma],\[Lambda],K,n,DiversityType->"None",Tolerance->OptionValue[Tolerance]]]
AnnamalaiRiceLimit[M_?NumericQ,\[Gamma]_,\[Lambda]_,K_?NumericQ,n_?IntegerQ,OptionsPattern[]]:=Module[{j, j0, tol = OptionValue[Tolerance], diversityType = OptionValue[DiversityType]},
	(* Handle both lists and scalar values for diversityType *)
	{diversityType, \[Gamma]t} = ProcessDiversityType[diversityType];

	(* Check for invalid combinations of inputs *)
	If[diversityType == "None" && n > 1, Return[Undefined]];

	Quiet[
		Which[
			diversityType == "None",
				j0 = (\[Lambda] / 2) - (M / 2) - Sqrt[M / 2] InverseQ[1 - tol];
				j/.FindRoot[1 - GammaRegularized[(M / 2) + j + 1, \[Lambda] / 2] == tol,{j, j0, 1, \[Infinity]}],
			diversityType == "MRC",
				j0 = (\[Lambda] / 2) - (M / 2) - Sqrt[M / 2] InverseQ[1 - tol];
				j/.FindRoot[1 - GammaRegularized[(M / 2) + j + 1, \[Lambda] / 2] == tol,{j, j0, 1, \[Infinity]}],
			diversityType == "EGC",
				Undefined,
			diversityType == "SC",
				Undefined,
			diversityType == "SEC",
				Undefined,
			diversityType == "SLC",
				j0 = (\[Lambda] / 2) - (M n / 2) - Sqrt[M n / 2] InverseQ[1 - tol];
				j/.FindRoot[1 - GammaRegularized[(M / 2) n + j + 1, \[Lambda] / 2] == tol,{j, j0, 1, \[Infinity]}],
			diversityType == "SLS",
				AnnamalaiRiceLimit[M,\[Gamma],\[Lambda],K,n,DiversityType->"None"],
			True,
				Undefined
		]//N//Ceiling
	]
]


(* ::Subsubsection::Closed:: *)
(*Herath' s method*)


Options[HerathLimit]={Tolerance->10^-6};
HerathLimit::usage="HerathLimit[M, \[Gamma], \[Lambda], K] calculates the truncation point for use in Herath's algorithm using the default tolerance for a single energy detector.
HerathLimit[M, \[Gamma], \[Lambda], m, n] calculates the truncation point for use in Herath's algorithm using the default tolerance for a cooperative network.

The calculation tolerance may be specified using the Tolerance option. By default, Tolerance\[Rule]"<>ToString[Tolerance/.Options[HerathLimit]//N//InputForm]<>".";
HerathLimit[M_?NumericQ,\[Gamma]_?NumericQ,\[Lambda]_,m_?NumericQ,OptionsPattern[]]:=Module[{n = 1},HerathLimit[M,\[Gamma],\[Lambda],m,n,Tolerance->OptionValue[Tolerance]]]
HerathLimit[M_?NumericQ,\[Gamma]_?NumericQ,\[Lambda]_,m_?NumericQ,n_?IntegerQ,OptionsPattern[]]:=Module[{x0,x1,f,fx0,tol=OptionValue[Tolerance]},
	Block[{$MaxExtraPrecision=\[Infinity]},
		f[j_?NumericQ]:=N[(m / ((M / 2) \[Gamma] + m))^(n m) Hypergeometric1F1[m n, j + 1, \[Lambda] (M / 2) \[Gamma] / (2 ((M / 2) \[Gamma] + m))] (1 - GammaRegularized[j + 1, \[Lambda] / 2]),20];
		(* Use Newton's method to find root *)
		Ceiling[j/.FindRoot[f[j]==tol,{j,Ceiling[M n/2]}]]
	]
]


Options[HerathRiceProbabilityOfDetection]={Timed->OptionValue[ProbabilityOfDetection,Timed],MaxTime->OptionValue[ProbabilityOfDetection,MaxTime],MaxIterations->OptionValue[ProbabilityOfDetection,MaxIterations]};
HerathRiceProbabilityOfDetection::usage="HerathRiceProbabilityOfDetection[M, \[Gamma], \[Lambda], m] calculates the exact probability of detection for a single energy detector operating in a Rice-K fading channel using Herath's algorithm.
HerathRiceProbabilityOfDetection[M, \[Gamma], \[Lambda], m, n] calculates the exact probability of detection for a cooperative network operating in a Rice-K fading channel using Herath's algorithm.

Function timing may be specified using the following options:

Timed\[Rule]"<>ToString[Timed/.Options[ProbabilityOfDetection]]<>"
MaxIterations\[Rule]"<>ToString[MaxIterations/.Options[ProbabilityOfDetection]]<>"
MaxTime\[Rule]"<>ToString[MaxTime/.Options[ProbabilityOfDetection]]<>"

where the above settings are the defaults, if not specified. If Timed\[Rule]True, then a {Pd, time} list of values will be returned.";
HerathRiceProbabilityOfDetection[M_?NumericQ,\[Gamma]_?NumericQ,\[Lambda]_,m_?NumericQ,OptionsPattern[]]:=Module[{RelevantOptions, n = 1},
	RelevantOptions[target_]:=FilterRules[Table[#[[i]]->OptionValue[#[[i]]],{i,Length[#]}]&[Options[HerathRiceProbabilityOfDetection][[All,1]]],Options[target][[All,1]]];
	HerathRiceProbabilityOfDetection[M,\[Gamma],\[Lambda],m,n,RelevantOptions[HerathRiceProbabilityOfDetection]]
]
HerathRiceProbabilityOfDetection[M_?NumericQ,\[Gamma]_?NumericQ,\[Lambda]_,m_?NumericQ,n_?IntegerQ,OptionsPattern[]]:=Module[{lim, f, totaltime = 0, iterations = 0, time, result},
	lim = HerathLimit[M,\[Gamma],\[Lambda],m,n];
	f:=Block[{$MaxExtraPrecision=\[Infinity]},
		N[
			Exp[-\[Lambda] / 2](((1 + m)Exp[-m \[Gamma] / (\[Gamma] + m + 1)])/(\[Gamma] + 1 + m))^n Total[Table[(m (\[Gamma] / (\[Gamma] + m + 1)) (1 - (\[Gamma] / (\[Gamma] + m + 1))))^(k-1)/(k - 1)! (1/(u - n - 1) (D[Exp[\[Lambda] z / 2]/((1 - z)(z - (\[Gamma] / (\[Gamma] + m + 1)))^(n + k - 1)),{z, u - L - 1}]/.z->0)+1/(n + k - 2)! (D[Exp[\[Lambda] z / 2]/(z^(u-n) (1 - z)),{z, n + k - 2}]/.z->(\[Gamma] / (\[Gamma] + m + 1)))),{k,1,lim}]]
		20]
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


Options[NumericalRiceProbabilityOfDetection] = {DiversityType->OptionValue[RiceProbabilityOfDetection,DiversityType],Timed->OptionValue[RiceProbabilityOfDetection,Timed],MaxTime->OptionValue[RiceProbabilityOfDetection,MaxTime],MaxIterations->OptionValue[RiceProbabilityOfDetection,MaxIterations]};
NumericalRiceProbabilityOfDetection::usage = GenerateAlgorithmHelp[NumericalRiceProbabilityOfDetection, "Numerical"];
NumericalRiceProbabilityOfDetection[M_?NumericQ,\[Gamma]_,\[Lambda]_,K_?NumericQ,OptionsPattern[]]:=Module[{RelevantOptions, n = 1},
	RelevantOptions[target_]:=FilterRules[Table[#[[i]]->OptionValue[#[[i]]],{i,Length[#]}]&[Options[NumericalRiceProbabilityOfDetection][[All,1]]],Options[target][[All,1]]];
	NumericalRiceProbabilityOfDetection[M,\[Gamma],\[Lambda],K,n,RelevantOptions[NumericalRiceProbabilityOfDetection]]
]
NumericalRiceProbabilityOfDetection[M_?NumericQ,\[Gamma]_,\[Lambda]_,K_?NumericQ,n_?IntegerQ,OptionsPattern[]]:=Module[{lim, f, g, \[Gamma]0, \[Gamma]t, totaltime = 0, iterations = 0, time, result, diversityType = OptionValue[DiversityType], \[ScriptCapitalD], x},
	\[ScriptCapitalD] = NumericalRiceDistribution[M, \[Gamma], K, n, diversityType];

	(* Handle both lists and scalar values for diversityType *)
	{diversityType, \[Gamma]t} = ProcessDiversityType[diversityType];
	
	(* Convert lists of SNR values to averages or maxima, depending on the specified diversity type *)
	\[Gamma]0 = ProcessSNR[\[Gamma], diversityType];

	(* Check for invalid combinations of inputs *)
	If[diversityType == "None" && n > 1, Return[Undefined]];
	If[\[Gamma]0 == Undefined, Return[Undefined]];

	f := Which[
		diversityType == "None" || diversityType == "MRC" || diversityType == "EGC" || diversityType == "SC" || diversityType == "SEC" || diversityType == "SLC",
			1 - CDF[\[ScriptCapitalD], \[Lambda]],
		diversityType == "SLS",
			Which[
				ListQ[\[Gamma]0],
					1 - Product[1 - NumericalRiceProbabilityOfDetection[M,\[Gamma]0[[i]],\[Lambda],K,DiversityType->"None"],{i,n}],
				!ListQ[\[Gamma]0],
					1 - (1 - NumericalRiceProbabilityOfDetection[M,\[Gamma]0,\[Lambda],K,DiversityType->"None"])^n,
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


NumericalRiceDistribution[M_?NumericQ, \[Gamma]_, K_?NumericQ, n_?IntegerQ, diversityType0_] := NumericalRiceDistribution[M, \[Gamma], K, n, diversityType0] = Module[{\[Gamma]t, \[Gamma]0, g, numberOfPoints = 100000, x, diversityType, y, \[ScriptCapitalD]},
	(* Handle both lists and scalar values for diversityType *)
	{diversityType, \[Gamma]t} = ProcessDiversityType[diversityType0];
	
	(* Convert lists of SNR values to averages or maxima, depending on the specified diversity type *)
	\[Gamma]0 = ProcessSNR[\[Gamma], diversityType];

	(* Check for invalid combinations of inputs *)
	If[diversityType == "None" && n > 1, Return[Undefined]];
	If[\[Gamma]0 == Undefined, Return[Undefined]];

	\[ScriptCapitalD] = ProbabilityDistribution[RicePDF[\[Gamma]0, K, y, n, DiversityType->diversityType, Method->"Exact"], {y, 0, \[Infinity]}];

	g[a_,b_] := SmoothKernelDistribution[RandomVariate[ParameterMixtureDistribution[NoncentralChiSquareDistribution[M a, M x], x \[Distributed] \[ScriptCapitalD]], numberOfPoints]];

	Which[
		diversityType == "None",
			g[1, 1],
		diversityType == "MRC",
			Undefined,
		diversityType == "EGC",
			Undefined,
		diversityType == "SC",
			Undefined,
		diversityType == "SEC",
			Undefined,
		diversityType == "SLC",
			Undefined,
		diversityType == "SLS",
			(* We don't need the SLS PDF here, the master function will use the no diversity PDF instead *)
			Null,
		True,
			Undefined
	]
]


(* ::Subsubsection::Closed:: *)
(*Small-K method*)


Options[SmallKRiceProbabilityOfDetection]={DiversityType->OptionValue[RiceProbabilityOfDetection,DiversityType],Timed->OptionValue[RiceProbabilityOfDetection,Timed],MaxTime->OptionValue[RiceProbabilityOfDetection,MaxTime],MaxIterations->OptionValue[RiceProbabilityOfDetection,MaxIterations], Tolerance->10^-6};
SmallKRiceProbabilityOfDetection::usage = GenerateAlgorithmHelp[SmallKRiceProbabilityOfDetection, "SmallK"];
SmallKRiceProbabilityOfDetection[M_?NumericQ,\[Gamma]_?NumericQ,\[Lambda]_,K_?NumericQ,OptionsPattern[]]:=Module[{RelevantOptions, n = 1},
	RelevantOptions[target_]:=FilterRules[Table[#[[i]]->OptionValue[#[[i]]],{i,Length[#]}]&[Options[SmallKRiceProbabilityOfDetection][[All,1]]],Options[target][[All,1]]];
	SmallKRiceProbabilityOfDetection[M,\[Gamma],\[Lambda],K,n,RelevantOptions[SmallKRiceProbabilityOfDetection]]
]
SmallKRiceProbabilityOfDetection[M_?NumericQ,\[Gamma]_?NumericQ,\[Lambda]_,K_?NumericQ,n_?NumericQ,OptionsPattern[]]:=Module[{lim, \[Nu], \[Gamma]0, \[Gamma]t, f, J, totaltime = 0, iterations = 0, time, result, diversityType = OptionValue[DiversityType], x = Round[m n], tol = OptionValue[Tolerance]},
	(* This method can only be used when K * n is an integer *)
	If[Abs[K n - x] > tol, Return[Undefined]];
	
	(* Handle both lists and scalar values for diversityType *)
	{diversityType, \[Gamma]t} = ProcessDiversityType[diversityType];
	
	(* Convert lists of SNR values to averages or maxima, depending on the specified diversity type *)
	\[Gamma]0 = ProcessSNR[\[Gamma], diversityType];

	(* Check for invalid combinations of inputs *)
	If[diversityType == "None" && n > 1, Return[Undefined]];
	If[\[Gamma]0 == Undefined, Return[Undefined]];

	lim = SmallKRiceLimit[M, \[Lambda], K, n, DiversityType->diversityType];

	J[k_, a_, b_, c_] := (Exp[-a^2] / 2) ((I c / (2 b))^k / k!) FaddeevaDerivative[k, -I (a + (c / (2b)))] // Re;

	f := Which[
		diversityType == "None",
			With[{a = (\[Lambda] - M) / (2 Sqrt[M]), b = - Sqrt[M] / 2, c = (K + 1) / \[Gamma]0},
				AWGNProbabilityOfFalseAlarm[M, \[Lambda], n, DiversityType->diversityType] + Exp[-K] Total[Table[K^k / k! Total[Table[J[p, a, b, c], {p, 0, k}]], {k, 0, lim}]]
			],
		diversityType == "MRC",
			Undefined,
		diversityType == "EGC",
			Undefined,
		diversityType == "SC",
			Undefined,
		diversityType == "SEC",
			Undefined,
		diversityType == "SLC",
			Undefined,
		diversityType == "SLS",
			Undefined,
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


Options[SmallKRiceLimit] = {DiversityType->OptionValue[SmallKRiceProbabilityOfDetection,DiversityType], Tolerance->OptionValue[SmallKRiceProbabilityOfDetection,Tolerance]};
SmallKRiceLimit[M_?NumericQ,\[Lambda]_,K_?NumericQ,n_?NumericQ,OptionsPattern[]]:=Module[{j, j0, tol = OptionValue[Tolerance], diversityType = OptionValue[DiversityType]},
	Which[
		diversityType == "None",
			j0 = 1;
			j/.FindRoot[(1 - GammaRegularized[j + 1, K]) (1 - AWGNProbabilityOfFalseAlarm[M, \[Lambda], n, DiversityType->diversityType]), {j, j0, 1, \[Infinity]}],
		diversityType == "MRC",
			Undefined,
		diversityType == "EGC",
			Undefined,
		diversityType == "SC",
			Undefined,
		diversityType == "SEC",
			Undefined,
		diversityType == "SLC",
			Undefined,
		diversityType == "SLS",
			Undefined,
		True,
			Undefined
	]//Ceiling//Quiet
]


(* ::Subsubsection::Closed:: *)
(*Asymptotic method*)


Options[AsymptoticRiceProbabilityOfDetection] = {DiversityType->OptionValue[RiceProbabilityOfDetection,DiversityType],Timed->OptionValue[RiceProbabilityOfDetection,Timed],MaxTime->OptionValue[RiceProbabilityOfDetection,MaxTime],MaxIterations->OptionValue[RiceProbabilityOfDetection,MaxIterations]};
AsymptoticRiceProbabilityOfDetection::usage = GenerateAlgorithmHelp[AsymptoticRiceProbabilityOfDetection, "Asymptotic"];
AsymptoticRiceProbabilityOfDetection[M_?NumericQ,\[Gamma]_?NumericQ,\[Lambda]_,K_?NumericQ,OptionsPattern[]]:=Module[{RelevantOptions, n = 1},
	RelevantOptions[target_]:=FilterRules[Table[#[[i]]->OptionValue[#[[i]]],{i,Length[#]}]&[Options[AsymptoticRiceProbabilityOfDetection][[All,1]]],Options[target][[All,1]]];
	AsymptoticRiceProbabilityOfDetection[M,\[Gamma],\[Lambda],K,n,RelevantOptions[AsymptoticRiceProbabilityOfDetection]]
]
AsymptoticRiceProbabilityOfDetection[M_?NumericQ,\[Gamma]_?NumericQ,\[Lambda]_,K_?NumericQ,n_?NumericQ,OptionsPattern[]]:=Module[{lim, f, g, \[Gamma]0, \[Gamma]t, totaltime = 0, iterations = 0, time, result, diversityType = OptionValue[DiversityType]},
	(* Handle both lists and scalar values for diversityType *)
	{diversityType, \[Gamma]t} = ProcessDiversityType[diversityType];
	
	(* Convert lists of SNR values to averages or maxima, depending on the specified diversity type *)
	\[Gamma]0 = ProcessSNR[\[Gamma], diversityType];

	(* Check for invalid combinations of inputs *)
	If[diversityType == "None" && n > 1, Return[Undefined]];
	If[\[Gamma]0 == Undefined, Return[Undefined]];

	g[a_, b_, \[Mu]_, \[Sigma]_] := Q[(a + b \[Mu]) / Sqrt[b^2 \[Sigma]^2 + 1]];

	f := Which[
		diversityType == "None",
			g[(\[Lambda] - M) / Sqrt[2 M], - Sqrt[M / 2], \[Gamma]0, Sqrt[2K + 1] (\[Gamma]0 / (K + 1))],
		diversityType == "MRC",
			Undefined,
		diversityType == "EGC",
			Undefined,
		diversityType == "SC",
			Undefined,
		diversityType == "SEC",
			Undefined,
		diversityType == "SLC",
			Undefined,
		diversityType == "SLS",
			Undefined,
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


Options[NGaussianRiceProbabilityOfDetection] = {DiversityType->OptionValue[RiceProbabilityOfDetection,DiversityType],Timed->OptionValue[RiceProbabilityOfDetection,Timed],MaxTime->OptionValue[RiceProbabilityOfDetection,MaxTime],MaxIterations->OptionValue[RiceProbabilityOfDetection,MaxIterations],LowSNR->OptionValue[RiceProbabilityOfDetection,LowSNR]};
NGaussianRiceProbabilityOfDetection::usage = GenerateAlgorithmHelp[NGaussianRiceProbabilityOfDetection, "NGaussian"];
NGaussianRiceProbabilityOfDetection[M_?NumericQ,\[Gamma]_?NumericQ,\[Lambda]_,K_?NumericQ,OptionsPattern[]]:=Module[{RelevantOptions, n = 1},
	RelevantOptions[target_]:=FilterRules[Table[#[[i]]->OptionValue[#[[i]]],{i,Length[#]}]&[Options[NGaussianRiceProbabilityOfDetection][[All,1]]],Options[target][[All,1]]];
	NGaussianRiceProbabilityOfDetection[M,\[Gamma],\[Lambda],K,n,RelevantOptions[NGaussianRiceProbabilityOfDetection]]
]
NGaussianRiceProbabilityOfDetection[M_?NumericQ,\[Gamma]_?NumericQ,\[Lambda]_,K_?NumericQ,n_?IntegerQ,OptionsPattern[]]:=Module[{lim, \[Gamma]t, \[Gamma]0, f, totaltime = 0, iterations = 0, time, result, RelevantOptions, diversityType = OptionValue[DiversityType]},
	(* Handle both lists and scalar values for diversityType *)
	{diversityType, \[Gamma]t} = ProcessDiversityType[diversityType];
	
	(* Convert lists of SNR values to averages or maxima, depending on the specified diversity type *)
	\[Gamma]0 = ProcessSNR[\[Gamma], diversityType];

	(* Check for invalid combinations of inputs *)
	If[diversityType == "None" && n > 1, Return[Undefined]];
	If[\[Gamma]0 == Undefined, Return[Undefined]];

	RelevantOptions[target_]:=FilterRules[Table[#[[i]]->OptionValue[#[[i]]],{i,Length[#]}]&[Options[NGaussianRiceProbabilityOfDetection][[All,1]]],Options[target][[All,1]]];

	f := Which[
		diversityType == "None",
			NIntegrate[AWGNProbabilityOfDetection[M,x,\[Lambda],RelevantOptions[AWGNProbabilityOfDetection]]RicePDF[\[Gamma]0,K,x,DiversityType->diversityType],{x,0,\[Infinity]}],
		diversityType == "MRC",
			NIntegrate[AWGNProbabilityOfDetection[M,x/n,\[Lambda],n,RelevantOptions[AWGNProbabilityOfDetection]]RicePDF[\[Gamma]0,K,x,n,DiversityType->diversityType],{x,0,\[Infinity]}],
		diversityType == "EGC",
			NIntegrate[AWGNProbabilityOfDetection[M,x/n,\[Lambda],n,RelevantOptions[AWGNProbabilityOfDetection]]RicePDF[\[Gamma]0,K,x,n,DiversityType->diversityType],{x,0,\[Infinity]}],
		diversityType == "SC",
			NIntegrate[AWGNProbabilityOfDetection[M,x,\[Lambda],RelevantOptions[AWGNProbabilityOfDetection]]RicePDF[\[Gamma]0,K,x,n,DiversityType->diversityType],{x,0,\[Infinity]}],
		diversityType == "SEC",
			NIntegrate[AWGNProbabilityOfDetection[M,x,\[Lambda],RelevantOptions[AWGNProbabilityOfDetection]]RicePDF[\[Gamma]0,K,x,n,DiversityType->{diversityType, \[Gamma]t}],{x,0,\[Infinity]}],
		diversityType == "SLC",
			NIntegrate[AWGNProbabilityOfDetection[M,x/n,\[Lambda],n,RelevantOptions[AWGNProbabilityOfDetection]]RicePDF[\[Gamma]0,K,x,n,DiversityType->diversityType],{x,0,\[Infinity]}],
		diversityType == "SLS",
			Which[
				ListQ[\[Gamma]0],
					1 - Product[1 - NGaussianRiceProbabilityOfDetection[M,\[Gamma]0[[i]],\[Lambda],K,DiversityType->"None",LowSNR->OptionValue[LowSNR]],{i,n}],
				!ListQ[\[Gamma]0],
					1 - (1 - NGaussianRiceProbabilityOfDetection[M,\[Gamma]0,\[Lambda],K,DiversityType->"None",LowSNR->OptionValue[LowSNR]])^n,
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


Options[NRiceSampleComplexity]={Method->OptionValue[SampleComplexity,Method],LowSNR->OptionValue[SampleComplexity,LowSNR],Tolerance->OptionValue[SampleComplexity,Tolerance]};
NRiceSampleComplexity::usage="NRiceSampleComplexity[\[Gamma], Pf, Pd, m] calculates the sample complexity for a single energy detector operating on a Rice-m fading channel.
NRiceSampleComplexity[\[Gamma], Pf, Pd, m, n] calculates the sample complexity for a cooperative network operating on a Rice-m fading channel.

The following methods can be given:

Method\[Rule]\"Approximate\"
Method\[Rule]\"Exact\"

By default, Method\[Rule]\""<>ToString[Method/.Options[NRiceSampleComplexity]]<>"\".

If Method\[Rule]\"Approximate\", the LowSNR option may be specified. By default, LowSNR\[Rule]"<>ToString[LowSNR/.Options[NRiceSampleComplexity]]<>".

Numerical tolerance can be specified using the Tolerance option. By default, Tolerance\[Rule]"<>ToString[Tolerance/.Options[NRiceSampleComplexity]//N//InputForm]<>".";
NRiceSampleComplexity::tol="The difference between the result `1` and the constraint `2` was greater than the specified tolerance `3`.";
NRiceSampleComplexity[\[Gamma]_?NumericQ,Pf_?NumericQ,Pd_?NumericQ,m_?NumericQ,OptionsPattern[]]:=Module[{RelevantOptions, n = 1},
	RelevantOptions[target_]:=FilterRules[Table[#[[i]]->OptionValue[#[[i]]],{i,Length[#]}]&[Options[NRiceSampleComplexity][[All,1]]],Options[target][[All,1]]];
	NRiceSampleComplexity[\[Gamma],Pf,Pd,m,n,RelevantOptions[NRiceSampleComplexity]]
]
NRiceSampleComplexity[\[Gamma]_?NumericQ,Pf_?NumericQ,Pd_?NumericQ,m_?NumericQ,n_?IntegerQ,OptionsPattern[]]:=Module[{RelevantOptions, tol = OptionValue[Tolerance], intialGuess = Max[(20 / (n m^2)), 1] * SampleComplexity[\[Gamma],Pf,Pd,n], courseGuess, fineGuess, result},
	RelevantOptions[target_]:=FilterRules[Table[#[[i]]->OptionValue[#[[i]]],{i,Length[#]}]&[Options[NRiceSampleComplexity][[All,1]]],Options[target][[All,1]]];
	(* Temporarily disable error checking - we'll do our own *)
	Off[FindRoot::reged,FindRoot::lstol];
	Switch[OptionValue[Method],
		"Approximate",
		(* Only use Gaussian method if it is valid *)
		If[intialGuess <= 250,
			result = NRiceSampleComplexity[\[Gamma],Pf,Pd,m,n,tol,Method->"Exact",Tolerance->OptionValue[Tolerance]];,
			fineGuess = M/.FindRoot[RiceProbabilityOfDetection[M,\[Gamma],\[Lambda][M,Pf,n],m,n,RelevantOptions[RiceProbabilityOfDetection]] == Pd, {M, intialGuess, 1, \[Infinity]}];
			result = RiceProbabilityOfDetection[fineGuess,\[Gamma],\[Lambda][fineGuess,Pf,n],m,n,RelevantOptions[RiceProbabilityOfDetection]];
		];,
		"Exact",
		(* If Gaussian approximation is valid, then use it to speed up the calculation *)
		If[intialGuess <= 250,
			courseGuess = intialGuess;,
			courseGuess = M/.FindRoot[RiceProbabilityOfDetection[M,\[Gamma],\[Lambda][M,Pf,n],m,n,RelevantOptions[RiceProbabilityOfDetection]] == Pd, {M, intialGuess, 1, \[Infinity]}];
		];
		fineGuess = M/.FindRoot[RiceProbabilityOfDetection[courseGuess,\[Gamma],\[Lambda][courseGuess,Pf,n,RelevantOptions[\[Lambda]]],m,n] == Pd, {M, courseGuess, 1, \[Infinity]}];
		result = RiceProbabilityOfDetection[fineGuess,\[Gamma],\[Lambda][fineGuess,Pf,n,RelevantOptions[\[Lambda]]],m,n];
	];
	On[FindRoot::reged,FindRoot::lstol];
	If[Abs[result - Pd] <= tol//TrueQ,
		fineGuess,
		Message[NRiceSampleComplexity::tol, result//N, Pd//N, tol//N]
	]
]


End[];


EndPackage[];
