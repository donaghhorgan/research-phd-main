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


BeginPackage["Rice`"];


(* ::Subsection::Closed:: *)
(*PDF of the signal to noise ratio*)


RicePDF;


(* ::Subsection:: *)
(*Probabiity of detection*)


(* ::Subsubsection::Closed:: *)
(*Annamalai's method*)


AnnamalaiRiceLimit;


NAnnamalaiRiceProbabilityOfDetection;


(* ::Subsubsection::Closed:: *)
(*Approximate method*)


NApproximateRiceProbabilityOfDetection;


(* ::Subsubsection::Closed:: *)
(*Numerical Gaussian method*)


NGaussianRiceProbabilityOfDetection;


(* ::Section:: *)
(*Private*)


Begin["`Private`"];


(* ::Subsection:: *)
(*PDF of the signal to noise ratio*)


Needs["BesselIApprox`"];


Options[RicePDF] = {Method->"Exact"};
RicePDF::usage="RicePDF[\!\(\*OverscriptBox[\(\[Gamma]\), \(_\)]\), m, x] evaluates the probability density function of the instantaneous signal to noise ratio at a single energy detector operating on a Rice fading channel at x.
RicePDF[\!\(\*OverscriptBox[\(\[Gamma]\), \(_\)]\), m, x, n] evaluates the probability density function of the average instantaneous signal to noise ratio at the fusion center of a cooperative network operating on a Rice fading channel at x.

The following methods can be given:

Method\[Rule]\"Approximate\"
Method\[Rule]\"Exact\"

By default, Method\[Rule]\""<>ToString[Method/.Options[RicePDF]]<>"\".";
RicePDF[\[Gamma]_,m_,x_,OptionsPattern[]]:=Module[{n = 1},RicePDF[\[Gamma],m,x,n,Method->OptionValue[Method]]]
RicePDF[\[Gamma]_,m_,x_,n_,OptionsPattern[]]:=Switch[OptionValue[Method],
	"Exact",
	(m + 1) / \[Gamma] Exp[-m n - (m + 1) x / \[Gamma]]((m + 1) x / (m n \[Gamma]))^((n - 1) / 2) BesselI[n - 1, 2 Sqrt[m n (m + 1)x / \[Gamma]]],
	"Approximate",
	2 (m + 1) / \[Gamma] PDF[NormalDistribution[2 n + 2 m n, Sqrt[4 n + 8 m n]], 2 (m + 1) x / \[Gamma]],
	_,
	RicePDF[\[Gamma],m,x,n,Method->"Exact"]
]


(* ::Subsection:: *)
(*Probabiity of detection*)


(* ::Subsubsection::Closed:: *)
(*Annamalai's method*)


Options[AnnamalaiRiceLimit]={Tolerance->10^-8};
AnnamalaiRiceLimit[M_?NumericQ,\[Gamma]_?NumericQ,\[Lambda]_,m_?NumericQ,n_?IntegerQ,OptionsPattern[]]:=Module[{tol=OptionValue[Tolerance]},
	1/2 (2(\[Lambda] / Sqrt[2M n] - InverseCDF[NormalDistribution[],tol])^2 - M n)//N//Ceiling
]


NAnnamalaiRiceProbabilityOfDetection[M_?NumericQ,\[Gamma]_?NumericQ,\[Lambda]_,m_?NumericQ,n_?IntegerQ,lim_?IntegerQ]:=Block[{$MaxExtraPrecision=\[Infinity]},
	1 - ((2 (1 + m) / (2 + 2m + M \[Gamma]))^n) Exp[-m n M \[Gamma] / (2 + 2m + M \[Gamma])] (1 - GammaRegularized[M n/2, \[Lambda] / 2]) - Total[Table[(n + k - 1)! ((2 (1 + m) / (2 + 2m + M \[Gamma]))^n) (((M \[Gamma]) / (2 + 2m + M \[Gamma]))^k) Exp[-m n M \[Gamma] / (2 + 2m + M \[Gamma])] (1 / ((n - 1)! k!) + Total[Table[((2 m n (1 + m) / (2 + 2m + M \[Gamma]))^i) / (i! (n + i - 1)! (k - i)!),{i , 1, k}]]) (1 - GammaRegularized[M n / 2 + k, \[Lambda] / 2]),{k, 1, lim}]]//N
]


(* ::Subsubsection::Closed:: *)
(*Approximate method*)


NApproximateRiceProbabilityOfDetection[M_?NumericQ,\[Gamma]_?NumericQ,\[Lambda]_,m_?NumericQ,n_?IntegerQ]:=Module[{f},
	f[x_]:=(-((2048 E^(-((x+m x+m \[Gamma])/\[Gamma])) f0 m^6 (1+m)^7 x^6 (-2+Erfc[(M+M x-\[Lambda])/(2 Sqrt[M])]))/(g0^12 \[Gamma]^7))-(512 E^(-((x+m x+m \[Gamma])/\[Gamma])) e0 m^5 (1+m)^6 x^5 (-2+Erfc[(M+M x-\[Lambda])/(2 Sqrt[M])]))/(g0^10 \[Gamma]^6)-(128 d0 E^(-((x+m x+m \[Gamma])/\[Gamma])) m^4 (1+m)^5 x^4 (-2+Erfc[(M+M x-\[Lambda])/(2 Sqrt[M])]))/(g0^8 \[Gamma]^5)-(32 c0 E^(-((x+m x+m \[Gamma])/\[Gamma])) m^3 (1+m)^4 x^3 (-2+Erfc[(M+M x-\[Lambda])/(2 Sqrt[M])]))/(g0^6 \[Gamma]^4)-(8 b0 E^(-((x+m x+m \[Gamma])/\[Gamma])) m^2 (1+m)^3 x^2 (-2+Erfc[(M+M x-\[Lambda])/(2 Sqrt[M])]))/(g0^4 \[Gamma]^3)-(2 a0 E^(-((x+m x+m \[Gamma])/\[Gamma])) m (1+m)^2 x (-2+Erfc[(M+M x-\[Lambda])/(2 Sqrt[M])]))/(g0^2 \[Gamma]^2)-(E^(-((x+m x+m \[Gamma])/\[Gamma])) (1+m) (-2+Erfc[(M+M x-\[Lambda])/(2 Sqrt[M])]))/(2 \[Gamma]))/.BesselIApproxParameters[0];
	NIntegrate[f[x],{x,0,\[Infinity]}]
]


(* ::Subsubsection::Closed:: *)
(*Numerical Gaussian method*)


Needs["AWGN`"];


Options[NGaussianRiceProbabilityOfDetection] = {LowSNR->True};
NGaussianRiceProbabilityOfDetection[M_?NumericQ,\[Gamma]_?NumericQ,\[Lambda]_,m_?NumericQ,n_?IntegerQ,OptionsPattern[]]:=Block[{$MaxExtraPrecision=\[Infinity]},
	NIntegrate[AWGNProbabilityOfDetection[M,x/n,\[Lambda],n, Method->"Approximate", LowSNR->OptionValue[LowSNR]]RicePDF[\[Gamma],m,x,n],{x,0,\[Infinity]}]
]


End[];


EndPackage[];
