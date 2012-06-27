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
(*27/06/2012*)
(*1.0*)


(* ::Subsection::Closed:: *)
(*Changelog*)


(* ::Text:: *)
(*Version 1.0: First working version, minor bug fixes to follow.*)


(* ::Section:: *)
(*Public*)


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
(*Digham' s method*)


NDighamNakagamiProbabilityOfDetection;


(* ::Subsubsection::Closed:: *)
(*Herath' s method*)


HerathLimit;


NHerathNakagamiProbabilityOfDetection;


(* ::Subsubsection::Closed:: *)
(*Annamalai' s method*)


AnnamalaiLimit;


NAnnamalaiNakagamiProbabilityOfDetection;


(* ::Subsubsection::Closed:: *)
(*Horgan' s method*)


NHorganNakagamiProbabilityOfDetection


(* ::Subsubsection::Closed:: *)
(*Numerical Gaussian method*)


NGaussianNakagamiProbabilityOfDetection;


(* ::Subsection:: *)
(*Sample complexity*)


NNakagamiSampleComplexity;


(* ::Section:: *)
(*Private*)


Begin["`Private`"];


(* ::Subsection::Closed:: *)
(*PDF of the signal to noise ratio*)


Options[NakagamiPDF] = {Method->"Exact"};
NakagamiPDF::usage="NakagamiPDF[\!\(\*OverscriptBox[\(\[Gamma]\), \(_\)]\), m, x] evaluates the probability density function of the instantaneous signal to noise ratio at a single energy detector operating on a Nakagami-m fading channel at x.
NakagamiPDF[\!\(\*OverscriptBox[\(\[Gamma]\), \(_\)]\), m, x, n] evaluates the probability density function of the average instantaneous signal to noise ratio at the fusion center of a cooperative network operating on a Nakagami-m fading channel at x.

The following methods can be given:

Method\[Rule]\"Approximate\"
Method\[Rule]\"Exact\"

By default, Method\[Rule]\""<>ToString[Method/.Options[NakagamiPDF]]<>"\".";
NakagamiPDF[\[Gamma]_,m_,x_,OptionsPattern[]]:=Module[{n = 1},NakagamiPDF[\[Gamma],m,x,n,Method->OptionValue[Method]]]
NakagamiPDF[\[Gamma]_,m_,x_,n_,OptionsPattern[]]:=Switch[OptionValue[Method],
	"Exact",
	PDF[GammaDistribution[m n,\[Gamma]/m],x],
	"Approximate",
	PDF[NormalDistribution[n \[Gamma],Sqrt[n \[Gamma]^2 / m]],x],
	_,
	NakagamiPDF[\[Gamma],m,x,n,Method->"Exact"]
]


(* ::Subsection:: *)
(*Detection probability*)


(* ::Subsubsection::Closed:: *)
(*Main function*)


Needs["DBLogging`"];
Needs["Network`"];


Options[NNakagamiProbabilityOfDetection]={Method->"Approximate",Algorithm->"NGaussian",LowSNR->True,Timed->False,MaxIterations->1000,MaxTime->600,DatabaseLookup->False,DatabaseCaching->False};
NNakagamiProbabilityOfDetection::usage="NNakagamiProbabilityOfDetection[M, \[Gamma], \[Lambda], m] calculates the probability of detection for a single energy detector operating on a Nakagami-m fading channel.
NNakagamiProbabilityOfDetection[M, \[Gamma], \[Lambda], m, n] calculates the probability of detection for the fusion center of a cooperative network operating on a Nakagami-m fading channel.

The following methods can be given:

Method\[Rule]\"Approximate\"
Method\[Rule]\"Exact\"

By default, Method\[Rule]\""<>ToString[Method/.Options[NNakagamiProbabilityOfDetection]]<>"\".

For a given method, an algorithm must be specified. For the approximate method, the following algorithms may be specified:

Algorithm\[Rule]\"Gaussian\"
Algorithm\[Rule]\"NGaussian\"

By default, Algorithm\[Rule]\""<>ToString[Algorithm/.Options[NNakagamiProbabilityOfDetection]]<>"\". If Algorithm\[Rule]\"NGaussian\", then the LowSNR option may also be specified. By default, LowSNR\[Rule]"<>ToString[LowSNR/.Options[NNakagamiProbabilityOfDetection]]<>".

For the exact method, the following algorithms may be specified:

Algorithm\[Rule]\"Annamalai\"
Algorithm\[Rule]\"Digham\"
Algorithm\[Rule]\"Herath\"

By default, Algorithm\[Rule]\"Annamalai\".

In addition, timing and database lookup/caching options may be (exclusively) specified. The timing option is specified by:

Timed\[Rule]"<>ToString[Timed/.Options[NNakagamiProbabilityOfDetection]]<>"
MaxIterations\[Rule]"<>ToString[MaxIterations/.Options[NNakagamiProbabilityOfDetection]]<>"
MaxTime\[Rule]"<>ToString[MaxTime/.Options[NNakagamiProbabilityOfDetection]]<>"

where the above options are the defaults, if not specified. If Timed\[Rule]True, then a {Pd, time} list of values will be returned.

If timing is not used, then database lookup/caching may be enabled. This requires that the sqlite.m package be loaded. By default:

DatabaseLookup\[Rule]"<>ToString[DatabaseLookup/.Options[NNakagamiProbabilityOfDetection]]<>"
DatabaseCaching\[Rule]"<>ToString[DatabaseCaching/.Options[NNakagamiProbabilityOfDetection]]<>"

and the data is stored in the database specified in sqlite.m.";
NNakagamiProbabilityOfDetection::opt="`1` and `2` options are mutually exclusive. Aborting...";
NNakagamiProbabilityOfDetection[M_,\[Gamma]_,\[Lambda]_,m_,OptionsPattern[]]:=Module[{n = 1},NNakagamiProbabilityOfDetection[M,\[Gamma],\[Lambda],m,n,Method->OptionValue[Method], Algorithm->OptionValue[Algorithm], LowSNR->OptionValue[LowSNR], Timed->OptionValue[Timed], MaxIterations->OptionValue[MaxIterations], MaxTime->OptionValue[MaxTime], DatabaseLookup->OptionValue[DatabaseLookup], DatabaseCaching->OptionValue[DatabaseCaching]]]
NNakagamiProbabilityOfDetection[M_,\[Gamma]_,\[Lambda]_,m_,n_,OptionsPattern[]]:=Module[{f, lim, result, time = 0, totaltime = 0, iterations = 0, channelType = "Nakagami", rationalPf},
	If[OptionValue[DatabaseLookup]&&OptionValue[Timed],
		Message[NNakagamiProbabilityOfDetection::opt,"DatabaseLookup","Timed"];
		Abort[];
	];
	If[OptionValue[DatabaseCaching]&&OptionValue[Timed],
		Message[NNakagamiProbabilityOfDetection::opt,"DatabaseCaching","Timed"];
		Abort[];
	];
	lim = Switch[OptionValue[Algorithm],
		"Herath",
		HerathLimit[M,\[Gamma],\[Lambda],m,n],
		"Annamalai",
		AnnamalaiLimit[M,\[Gamma],\[Lambda],m,n],
		_,
		Null
	];
	f := Switch[OptionValue[Method],
		"Exact",
		Switch[OptionValue[Algorithm],
			"Digham",
			NDighamNakagamiProbabilityOfDetection[M,\[Gamma],\[Lambda],m,n],
			"Herath",
			NHerathNakagamiProbabilityOfDetection[M,\[Gamma],\[Lambda],m,n,lim],
			"Annamalai",
			NAnnamalaiNakagamiProbabilityOfDetection[M,\[Gamma],\[Lambda],m,n,lim],
			_,
			NNakagamiProbabilityOfDetection[M,\[Gamma],\[Lambda],m,n,Method->"Exact",Algorithm->"Annamalai"]
		],
		"Approximate",
		Switch[OptionValue[Algorithm],
			"Gaussian",
			NHorganNakagamiProbabilityOfDetection[M,\[Gamma],\[Lambda],m,n],
			"NGaussian",
			NGaussianNakagamiProbabilityOfDetection[M,\[Gamma],\[Lambda],m,n, LowSNR->OptionValue[LowSNR]],
			_,
			NNakagamiProbabilityOfDetection[M,\[Gamma],\[Lambda],m,n,Method->"Approximate",Algorithm->"NGaussian"]
		],
		_,
		NNakagamiProbabilityOfDetection[M,\[Gamma],\[Lambda],m,n]
	];
	If[OptionValue[DatabaseLookup],
		result = GetResult[OptionValue[Algorithm],channelType,M,\[Gamma],ProbabilityOfFalseAlarm[M,\[Lambda],n,Method->OptionValue[Method]]//N,n,m];
		If[TrueQ[result==Null],
			result = f;
			If[OptionValue[DatabaseCaching],
				(* For correct retrieval of results later, attempt to convert Pf to rational form *)
				rationalPf = Round[ProbabilityOfFalseAlarm[M,\[Lambda],n,Method->"Exact"]*10^6//N]/10^6;
				CacheResult[OptionValue[Algorithm],channelType,M,\[Gamma],rationalPf,n,m,result//N,OptionValue[Algorithm]];
			];
		];
		result,
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
]


(* ::Subsubsection::Closed:: *)
(*Digham' s method*)


NDighamNakagamiProbabilityOfDetection::usage="NDighamNakagamiProbabilityOfDetection[M, \[Gamma], \[Lambda], m] calculates the exact probability of detection for a single energy detector operating in a Nakagami-m fading channel using Digham's algorithm.
NDighamNakagamiProbabilityOfDetection[M, \[Gamma], \[Lambda], m, n] calculates the exact probability of detection for a cooperative network operating in a Nakagami-m fading channel using Digham's algorithm.";
NDighamNakagamiProbabilityOfDetection[M_?NumericQ,\[Gamma]_?NumericQ,\[Lambda]_,m_?NumericQ,n_:1]:=Block[{$MaxExtraPrecision=\[Infinity]},
	N[
		Module[{A1, \[Beta]},
			A1 = Exp[-\[Lambda] \[Beta] / (2 m n)] (\[Beta]^(m n - 1) LaguerreL[m n - 1, -\[Lambda] (1 - \[Beta]) / 2] + (1 - \[Beta]) Total[Table[\[Beta]^i LaguerreL[i,-\[Lambda] (1 - \[Beta]) / 2], {i, 0, m n - 2}]]);
			\[Beta] = (2m) / (2m + M \[Gamma]);
			A1 + \[Beta]^(m n) Exp[-\[Lambda] / 2] Total[Table[((\[Lambda] / 2)^i / i!) Hypergeometric1F1[m n, i + 1, \[Lambda] (1 - \[Beta]) / 2], {i, 1, (M n / 2) - 1}]]
		],
	20]
]


(* ::Subsubsection::Closed:: *)
(*Herath' s method*)


Options[HerathLimit]={Tolerance->10^-6};
HerathLimit::usage="HerathLimit[M, \[Gamma], \[Lambda], m] calculates the truncation point for use in Herath's algorithm using the default tolerance for a single energy detector.
HerathLimit[M, \[Gamma], \[Lambda], m, n] calculates the truncation point for use in Herath's algorithm using the default tolerance for a cooperative network.

The calculation tolerance may be specified using the Tolerance option. By default, Tolerance\[Rule]"<>ToString[Tolerance/.Options[HerathLimit]//N//InputForm]<>".";
HerathLimit[M_?NumericQ,\[Gamma]_?NumericQ,\[Lambda]_,m_?NumericQ,n_?IntegerQ,OptionsPattern[]]:=Module[{x0,x1,f,fx0,tol=OptionValue[Tolerance]},
	Block[{$MaxExtraPrecision=\[Infinity]},
		f[j_?NumericQ]:=N[(m / ((M / 2) \[Gamma] + m))^(n m) Hypergeometric1F1[m n, j + 1, \[Lambda] (M / 2) \[Gamma] / (2 ((M / 2) \[Gamma] + m))] (1 - GammaRegularized[j + 1, \[Lambda] / 2]),20];
		(* Use Newton's method to find root *)
		Ceiling[j/.FindRoot[f[j]==tol,{j,Ceiling[M n/2]}]]
	]
]


NHerathNakagamiProbabilityOfDetection::usage="NHerathNakagamiProbabilityOfDetection[M, \[Gamma], \[Lambda], m, lim] calculates the exact probability of detection for a single energy detector operating in a Nakagami-m fading channel using Herath's algorithm and truncation point lim.
NHerathNakagamiProbabilityOfDetection[M, \[Gamma], \[Lambda], m, n, lim] calculates the exact probability of detection for a cooperative network operating in a Nakagami-m fading channel using Herath's algorithm and truncation point lim.";
NHerathNakagamiProbabilityOfDetection[M_?NumericQ,\[Gamma]_?NumericQ,\[Lambda]_,m_?NumericQ,lim_?IntegerQ]:=Module[{n = 1},HerathLimit[M,\[Gamma],\[Lambda],m,n,lim]]
NHerathNakagamiProbabilityOfDetection[M_?NumericQ,\[Gamma]_?NumericQ,\[Lambda]_,m_?NumericQ,n_?IntegerQ,lim_?IntegerQ]:=Block[{$MaxExtraPrecision=\[Infinity]},
	N[
		1 - Exp[-\[Lambda] / 2] (m / ((M / 2) \[Gamma] + m))^(n m) Total[Table[((\[Lambda] / 2)^j / j!) Hypergeometric1F1[m n, j + 1, \[Lambda] (M / 2) \[Gamma] / (2 ((M / 2) \[Gamma] + m))],{j, (M n/ 2), lim}]],
	20]
]


(* ::Subsubsection::Closed:: *)
(*Annamalai' s method*)


Options[AnnamalaiLimit]={Tolerance->10^-8};
AnnamalaiLimit::usage="AnnamalaiLimit[M, \[Gamma], \[Lambda], m] calculates the truncation point for use in Herath's algorithm using the default tolerance for a single energy detector.
AnnamalaiLimit[M, \[Gamma], \[Lambda], m, n] calculates the truncation point for use in Herath's algorithm using the default tolerance for a cooperative network.

The calculation tolerance may be specified using the Tolerance option. By default, Tolerance\[Rule]"<>ToString[Tolerance/.Options[AnnamalaiLimit]//N//InputForm]<>".";
AnnamalaiLimit[M_?NumericQ,\[Gamma]_?NumericQ,\[Lambda]_,m_?NumericQ,n_?IntegerQ,OptionsPattern[]]:=Module[{tol=OptionValue[Tolerance]},
	1/2 (2(\[Lambda] / Sqrt[2M n] - InverseCDF[NormalDistribution[],tol])^2 - M n)//N//Ceiling
]


NAnnamalaiNakagamiProbabilityOfDetection::usage="NAnnamalaiNakagamiProbabilityOfDetection[M, \[Gamma], \[Lambda], m, lim] calculates the exact probability of detection for a single energy detector operating in a Nakagami-m fading channel using Annamalai's algorithm and truncation point lim.
NAnnamalaiNakagamiProbabilityOfDetection[M, \[Gamma], \[Lambda], m, n, lim] calculates the exact probability of detection for a cooperative network operating in a Nakagami-m fading channel using Annamalai's algorithm and truncation point lim.";
NAnnamalaiNakagamiProbabilityOfDetection[M_?NumericQ,\[Gamma]_?NumericQ,\[Lambda]_,m_?NumericQ,lim_?IntegerQ]:=Module[{n = 1},NAnnamalaiNakagamiProbabilityOfDetection[M,\[Gamma],\[Lambda],m,n,lim]]
NAnnamalaiNakagamiProbabilityOfDetection[M_?NumericQ,\[Gamma]_?NumericQ,\[Lambda]_,m_?NumericQ,n_?IntegerQ,lim_?IntegerQ]:=Block[{$MaxExtraPrecision=\[Infinity]},
	N[
		1 - (2m / (2m + M \[Gamma]))^(m n) (1 - GammaRegularized[M n/2, \[Lambda] / 2]) - Total[Table[Gamma[m n+k]/(Gamma[m n]Gamma[k+1]) (m/(m+M/2 \[Gamma]))^(m n) (((M/2 \[Gamma])/(m+M/2 \[Gamma]))^k) (1-GammaRegularized[M n/2+k, \[Lambda] / 2]),{k,1,lim}]],
	20]
]


(* ::Subsubsection::Closed:: *)
(*Horgan' s method*)


Needs["AWGN`"];
Needs["ErfApprox`"];


Options[NHorganNakagamiProbabilityOfDetection]={LargeMN->10};
NHorganNakagamiProbabilityOfDetection::usage="NHorganNakagamiProbabilityOfDetection[M, \[Gamma], \[Lambda], m] calculates the approximate probability of detection for a single energy detector operating in a Nakagami-m fading channel using Horgan's algorithm
NHorganNakagamiProbabilityOfDetection[M, \[Gamma], \[Lambda], m, n] calculates the approximate probability of detection for a cooperative network operating in a Nakagami-m fading channel using Horgan's algorithm";
NHorganNakagamiProbabilityOfDetection[M_?NumericQ,\[Gamma]_?NumericQ,\[Lambda]_,m_?NumericQ,OptionsPattern[]]:=Module[{n=1},NHorganNakagamiProbabilityOfDetection[M,\[Gamma],\[Lambda],m,n,LargeMN->OptionValue[LargeMN]]]
NHorganNakagamiProbabilityOfDetection[M_?NumericQ,\[Gamma]_?NumericQ,\[Lambda]_,m_?NumericQ,n_?NumericQ,OptionsPattern[]]:=Block[{$MaxExtraPrecision=\[Infinity]},
	N[
		Module[{x = Round[m n], tol = 10^-6},
			(* This method can only be used when m * n is an integer *)
			If[Abs[m n - x] <= tol,
				If[x < OptionValue[LargeMN],
					AWGNProbabilityOfFalseAlarm[M,\[Lambda],n] - (D[((-1)^(x) / (2 Gamma[x])) * Exp[((m^2 n) / (M \[Gamma]^2)) * t^2 - ((\[Lambda] - M n) / (M (\[Gamma] / m))) * t]Erfc[(2n t - (\[Lambda] - M n) * (\[Gamma] / m)) / (2Sqrt[M n] (\[Gamma] / m))] / t, {t, x - 1}]/.t->1),
					(1/2 (1+Erf[(m (M n (1+\[Gamma])-\[Lambda]))/(Sqrt[2] M Sqrt[m n] \[Gamma])])+1/2 E^((4 c M n (-m+a M \[Gamma]^2)+b (-b M^2 n \[Gamma]^2+2 Sqrt[2] m Sqrt[M n] (M n (1+\[Gamma])-\[Lambda]))-2 a m (-M n (1+\[Gamma])+\[Lambda])^2)/(4 M n (-m+a M \[Gamma]^2))) Sqrt[m/(m-a M \[Gamma]^2)] (Erf[(b Sqrt[M^3 n] \[Gamma]^2+Sqrt[2] m (-M n (1+\[Gamma])+\[Lambda]))/(2 M \[Gamma] Sqrt[n (m-a M \[Gamma]^2)])]-Erf[(-2 m n+\[Gamma] (-2 a M n+Sqrt[2] b Sqrt[M n]+2 a \[Lambda]))/(2 Sqrt[2] Sqrt[n (m-a M \[Gamma]^2)])])+1/2 E^(-((4 c M n (m-a M \[Gamma]^2)+b (b M^2 n \[Gamma]^2+2 Sqrt[2] m Sqrt[M n] (M n (1+\[Gamma])-\[Lambda]))+2 a m (-M n (1+\[Gamma])+\[Lambda])^2)/(4 M n (-m+a M \[Gamma]^2)))) Sqrt[m/(m-a M \[Gamma]^2)] (-2+Erfc[(b Sqrt[M^3 n] \[Gamma]^2+Sqrt[2] m (M n (1+\[Gamma])-\[Lambda]))/(2 M \[Gamma] Sqrt[n (m-a M \[Gamma]^2)])]))/.LopezBenitezParameters[(-M (n+\[Gamma])+\[Lambda])/(2 Sqrt[M n])]
				],
				Undefined
			]
		],
	20]
]


(* ::Subsubsection::Closed:: *)
(*Numerical Gaussian method*)


Needs["AWGN`"];


Options[NGaussianNakagamiProbabilityOfDetection] = {LowSNR->True};
NGaussianNakagamiProbabilityOfDetection::usage="NGaussianNakagamiProbabilityOfDetection[M, \[Gamma], \[Lambda], m] calculates the approximate probability of detection for a single energy detector operating in a Nakagami-m fading channel using a numerical algorithm
NGaussianNakagamiProbabilityOfDetection[M, \[Gamma], \[Lambda], m, n] calculates the approximate probability of detection for a cooperative network operating in a Nakagami-m fading channel using a numerical algorithm.

In addition, the LowSNR option may be specified. By default, LowSNR\[Rule]"<>ToString[LowSNR/.Options[NGaussianNakagamiProbabilityOfDetection]]<>".";
NGaussianNakagamiProbabilityOfDetection[M_?NumericQ,\[Gamma]_?NumericQ,\[Lambda]_,m_?NumericQ,OptionsPattern[]]:=Module[{n = 1},NGaussianNakagamiProbabilityOfDetection[M,\[Gamma],\[Lambda],m,n,LowSNR->OptionValue[LowSNR]]]
NGaussianNakagamiProbabilityOfDetection[M_?NumericQ,\[Gamma]_?NumericQ,\[Lambda]_,m_?NumericQ,n_?IntegerQ,OptionsPattern[]]:=Block[{$MaxExtraPrecision=\[Infinity]},
	N[
		NIntegrate[AWGNProbabilityOfDetection[M,x/n,\[Lambda],n, Method->"Approximate", LowSNR->OptionValue[LowSNR]]NakagamiPDF[\[Gamma],m,x,n],{x,0,\[Infinity]}],
	20]
]


(* ::Subsection::Closed:: *)
(*Sample complexity*)


Needs["Network`"];


Options[NNakagamiSampleComplexity]={Method->"Approximate", LowSNR->True, Tolerance->10^-6};
NNakagamiSampleComplexity::usage="NNakagamiSampleComplexity[\[Gamma], Pf, Pd, m] calculates the sample complexity for a single energy detector operating on a Nakagami-m fading channel.
NNakagamiSampleComplexity[\[Gamma], Pf, Pd, m, n] calculates the sample complexity for a cooperative network operating on a Nakagami-m fading channel.

The following methods can be given:

Method\[Rule]\"Approximate\"
Method\[Rule]\"Exact\"

By default, Method\[Rule]\""<>ToString[Method/.Options[NNakagamiSampleComplexity]]<>"\".

If Method\[Rule]\"Approximate\", the LowSNR option may be specified. By default, LowSNR\[Rule]"<>ToString[LowSNR/.Options[NNakagamiSampleComplexity]]<>".

Numerical tolerance can be specified using the Tolerance option. By default, Tolerance\[Rule]"<>ToString[Tolerance/.Options[NNakagamiSampleComplexity]//N//InputForm]<>".";
NNakagamiSampleComplexity::tol="The difference between the result `1` and the constraint `2` was greater than the specified tolerance `3`.";
NNakagamiSampleComplexity[\[Gamma]_?NumericQ,Pf_?NumericQ,Pd_?NumericQ,m_?NumericQ,OptionsPattern[]]:=Module[{n = 1},NNakagamiSampleComplexity[\[Gamma],Pf,Pd,m,n,Method->OptionValue[Method], LowSNR->OptionValue[LowSNR],Tolerance->OptionValue[Tolerance]]]
NNakagamiSampleComplexity[\[Gamma]_?NumericQ,Pf_?NumericQ,Pd_?NumericQ,m_?NumericQ,n_?IntegerQ,OptionsPattern[]]:=Module[{tol = OptionValue[Tolerance], intialGuess = Max[(20 / (n m^2)), 1] * SampleComplexity[\[Gamma],Pf,Pd,n], courseGuess, fineGuess, result},
	(* Temporarily disable error checking - we'll do our own *)
	Off[FindRoot::reged,FindRoot::lstol];
	Switch[OptionValue[Method],
		"Approximate",
		(* Only use Gaussian method if it is valid *)
		If[intialGuess <= 250,
			result = NNakagamiSampleComplexity[\[Gamma],Pf,Pd,n,m,tol,Method->"Exact"];,
			fineGuess = M/.FindRoot[NNakagamiProbabilityOfDetection[M,\[Gamma],\[Lambda][M,Pf,n],m,n,Method->"NGaussian",LowSNR->OptionValue[LowSNR]] == Pd, {M, intialGuess, 1, \[Infinity]}];
			result = NNakagamiProbabilityOfDetection[fineGuess,\[Gamma],\[Lambda][fineGuess,Pf,n],m,n,Method->"NGaussian",LowSNR->OptionValue[LowSNR]];
		];,
		"Exact",
		(* If Gaussian approximation is valid, then use it to speed up the calculation *)
		If[intialGuess <= 250,
			courseGuess = intialGuess;,
			courseGuess = M/.FindRoot[NNakagamiProbabilityOfDetection[M,\[Gamma],\[Lambda][M,Pf,n],m,n,Method->"NGaussian"] == Pd, {M, intialGuess, 1, \[Infinity]}];
		];
		fineGuess = M/.FindRoot[NNakagamiProbabilityOfDetection[courseGuess,\[Gamma],\[Lambda][courseGuess,Pf,n,Method->"Exact"],m,n] == Pd, {M, courseGuess, 1, \[Infinity]}];
		result = NNakagamiProbabilityOfDetection[fineGuess,\[Gamma],\[Lambda][fineGuess,Pf,n,Method->"Exact"],m,n];
	];
	On[FindRoot::reged,FindRoot::lstol];
	If[Abs[result - Pd] <= tol//TrueQ,
		fineGuess,
		Message[NNakagamiSampleComplexity::tol, result//N, Pd//N, tol//N]
	]
]


End[];


EndPackage[];
