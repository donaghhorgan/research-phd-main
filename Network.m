(* ::Package:: *)

(* ::Title:: *)
(*Network / fusion center functions*)


(* ::Subsection::Closed:: *)
(*Copyright notice*)


(* ::Text:: *)
(*Mathematica function definitions for cooperative energy detection.*)
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


BeginPackage["Network`"];


(* ::Subsection:: *)
(*Decision probabilities*)


(* ::Subsubsection::Closed:: *)
(*Probability of false alarm (general)*)


ProbabilityOfFalseAlarm;


(* ::Subsubsection::Closed:: *)
(*Probability of detection (general)*)


ProbabilityOfDetection;


(* ::Subsubsection::Closed:: *)
(*Probability of false alarm (1 bit decision only)*)


FusionCenterProbabilityOfFalseAlarm;


(* ::Subsubsection::Closed:: *)
(*Probability of detecion (1 bit decision only)*)


FusionCenterProbabilityOfDetection;


(* ::Subsection::Closed:: *)
(*Voting rule*)


k;


(* ::Subsection::Closed:: *)
(*Sample complexity*)


SampleComplexity;


(* ::Section:: *)
(*Private*)


Begin["`Private`"];


(* ::Subsection:: *)
(*Decision probabilities*)


(* ::Subsubsection::Closed:: *)
(*Probability of false alarm (general)*)


Needs["AWGN`"];


Options[ProbabilityOfFalseAlarm] = {DecisionBits->\[Infinity],CorrelationCoefficient->0,Method->"Approximate"};
ProbabilityOfFalseAlarm::usage="ProbabilityOfFalseAlarm[M, \[Lambda]] calculates the probability of false alarm for a single energy detector.
ProbabilityOfFalseAlarm[M, \[Lambda], n] calculates the probability of false alarm for a cooperative network of energy detectors with infinite precision decision fusion.
ProbabilityOfFalseAlarm[M, \[Lambda], n, k] calculates the probability of false alarm for a cooperative network of energy detectors with 1 bit decision fusion.

The following methods may be specified:

Method\[Rule]\"Approximate\"
Method\[Rule]\"Exact\"

By default, Method\[Rule]\""<>ToString[Method/.Options[ProbabilityOfFalseAlarm]]<>"\".

Additionally, the number of decision bits used in fusion may be specified by the DecisionBits option. By default, DecisionBits\[Rule]"<>ToString[DecisionBits/.Options[ProbabilityOfFalseAlarm]]<>". DecisionBits \[Element] {1,\[Infinity]}.

If 1 bit decision fusion is specified, then the average correlation between nodes may be specified with the CorrelationCoefficient option. By default, CorrelationCoefficient\[Rule]"<>ToString[CorrelationCoefficient/.Options[ProbabilityOfFalseAlarm]]<>".";
ProbabilityOfFalseAlarm[M_,\[Lambda]_,OptionsPattern[]]:=Module[{n=1},ProbabilityOfFalseAlarm[M,\[Lambda],n,DecisionBits->OptionValue[DecisionBits],CorrelationCoefficient->OptionValue[CorrelationCoefficient],Method->OptionValue[Method]]]
ProbabilityOfFalseAlarm[M_,\[Lambda]_,n_,OptionsPattern[]]:=Module[{k=Null},ProbabilityOfFalseAlarm[M,\[Lambda],n,k,DecisionBits->OptionValue[DecisionBits],CorrelationCoefficient->OptionValue[CorrelationCoefficient],Method->OptionValue[Method]]]
ProbabilityOfFalseAlarm[M_,\[Lambda]_,n_,k_,OptionsPattern[]]:=If[OptionValue[DecisionBits]==\[Infinity],
	If[OptionValue[CorrelationCoefficient]==0,
		AWGNProbabilityOfFalseAlarm[M,\[Lambda],n,Method->OptionValue[Method]],
		Undefined
	],
	Module[{Pf},
		Pf = AWGNProbabilityOfFalseAlarm[M,\[Lambda],Method->OptionValue[Method]];
		FusionCenterProbabilityOfFalseAlarm[Pf,n,k,OptionValue[CorrelationCoefficient]]
	]
]


(* ::Subsubsection::Closed:: *)
(*Probability of detection (general)*)


Needs["AWGN`"];
Needs["Rayleigh`"];
Needs["Nakagami`"];


Options[ProbabilityOfDetection] = {ChannelType->"AWGN",DecisionBits->\[Infinity],CorrelationCoefficient->0,Method->"Approximate"};
ProbabilityOfDetection::usage="ProbabilityOfDetection[M, \[Gamma], \[Lambda]] calculates the probability of detection for a single energy detector for the specified channel type.
ProbabilityOfDetection[M, \[Gamma], \[Lambda], n] calculates the probability of detection for a cooperative network of energy detectors with infinite precision decision fusion for the specified channel type.
ProbabilityOfDetection[M, \[Gamma], \[Lambda], n, k] calculates the probability of detection for a cooperative network of energy detectors with 1 bit decision fusion for the specified channel type.

The following methods may be specified:

Method\[Rule]\"Approximate\"
Method\[Rule]\"Exact\"

By default, Method\[Rule]\""<>ToString[Method/.Options[ProbabilityOfDetection]]<>"\".

The following channel types may be specified:

ChannelType\[Rule]\"AWGN\"
ChannelType\[Rule]\"Rayleigh\"
ChannelType\[Rule]{\"Nakagami\",m}

By default, ChannelType\[Rule]\""<>ToString[ChannelType/.Options[ProbabilityOfDetection]]<>"\".

Additionally, the number of decision bits used in fusion may be specified by the DecisionBits option. By default, DecisionBits\[Rule]"<>ToString[DecisionBits/.Options[ProbabilityOfFalseAlarm]]<>". DecisionBits \[Element] {1,\[Infinity]}.

If 1 bit decision fusion is specified, then the average correlation between nodes may be specified with the CorrelationCoefficient option. By default, CorrelationCoefficient\[Rule]"<>ToString[CorrelationCoefficient/.Options[ProbabilityOfDetection]]<>".";
ProbabilityOfDetection::k="Error: must specify a voting rule when DecisionBits\[Rule]1";
ProbabilityOfDetection[M_,\[Gamma]_,\[Lambda]_,OptionsPattern[]]:=Module[{n=1},ProbabilityOfDetection[M,\[Gamma],\[Lambda],n,ChannelType->OptionValue[ChannelType],DecisionBits->OptionValue[DecisionBits],CorrelationCoefficient->OptionValue[CorrelationCoefficient],Method->OptionValue[Method]]]
ProbabilityOfDetection[M_,\[Gamma]_,\[Lambda]_,n_,OptionsPattern[]]:=Module[{k=Null},ProbabilityOfDetection[M,\[Gamma],\[Lambda],n,k,ChannelType->OptionValue[ChannelType],DecisionBits->OptionValue[DecisionBits],CorrelationCoefficient->OptionValue[CorrelationCoefficient],Method->OptionValue[Method]]]
ProbabilityOfDetection[M_,\[Gamma]_,\[Lambda]_,n_,k_,OptionsPattern[]]:=Module[{channelType,m,\[Rho]=OptionValue[CorrelationCoefficient]},
	If[ListQ[OptionValue[ChannelType]],
		{channelType,m} = OptionValue[ChannelType];,
		channelType = OptionValue[ChannelType];
	];
	If[OptionValue[DecisionBits]==\[Infinity],
		If[\[Rho]==0,
			Switch[channelType,
				"AWGN",
				AWGNProbabilityOfDetection[M,\[Gamma],\[Lambda],n,Method->OptionValue[Method]],
				"Rayleigh",
				NRayleighProbabilityOfDetection[M,\[Gamma],\[Lambda],n,Method->OptionValue[Method]],
				"Nakagami",
				NNakagamiProbabilityOfDetection[M,\[Gamma],\[Lambda],m,n,Method->OptionValue[Method]],
				_,
				ProbabilityOfDetection[M,\[Gamma],\[Lambda],n,k,ChannelType->"AWGN",CorrelationCoefficient->\[Rho],Method->OptionValue[Method]]
			],
			Undefined
		],
		If[k==Null,
			Message[ProbabilityOfDetection::k];
			Abort[],
			Module[{Pd},
				Pd = Switch[channelType,
					"AWGN",
					AWGNProbabilityOfDetection[M,\[Gamma],\[Lambda],Method->OptionValue[Method]],
					"Rayleigh",
					NRayleighProbabilityOfDetection[M,\[Gamma],\[Lambda],Method->OptionValue[Method]],
					"Nakagami",
					NNakagamiProbabilityOfDetection[M,\[Gamma],\[Lambda],m,Method->OptionValue[Method]],
					_,
					ProbabilityOfDetection[M,\[Gamma],\[Lambda],n,k,ChannelType->"AWGN",CorrelationCoefficient->\[Rho],Method->OptionValue[Method]]
				];
				FusionCenterProbabilityOfDetection[Pd,n,k,\[Rho]]
			]
		]
	]
]


(* ::Subsubsection::Closed:: *)
(*Probability of false alarm (1 bit decision only)*)


FusionCenterProbabilityOfFalseAlarm::usage="FusionCenterProbabilityOfFalseAlarm[Pf, n, k] calculates the overall probability of false alarm for a cooperative network with 1 bit decision fusion.
FusionCenterProbabilityOfFalseAlarm[Pf, n, k, \[Rho]] calculates the overall probability of false alarm for a cooperative network with 1 bit decision fusion and correlated decisions.";
FusionCenterProbabilityOfFalseAlarm[Pf_?NumericQ,n_?NumericQ,k_?NumericQ,\[Rho]_:0]:=If[\[Rho] == 0,
	1 - CDF[BinomialDistribution[n, Pf], k - 1],
	Switch[n,
		1,
		Pf,
		2,
		Undefined,
		_,
		Sum[Sum[(-1)^i Binomial[l,i] Pf Product[(\[Rho](s + 1 - Pf) + Pf) / (1 + s \[Rho]),{s, 0, n - l + i - 2}],{i, 0, l}],{l, 0, k - 1}]
	]
]


(* ::Subsubsection::Closed:: *)
(*Probability of detecion (1 bit decision only)*)


FusionCenterProbabilityOfDetection::usage="FusionCenterProbabilityOfDetection[Pd, n, k] calculates the overall probability of detection for a cooperative network with 1 bit decision fusion.
FusionCenterProbabilityOfDetection[Pd, n, k, \[Rho]] calculates the overall probability of detection for a cooperative network with 1 bit decision fusion and correlated decisions.";
FusionCenterProbabilityOfDetection[Pd_?NumericQ,n_?NumericQ,k_?NumericQ,\[Rho]_:0]:=FusionCenterProbabilityOfFalseAlarm[Pd,n,k,\[Rho]]


(* ::Subsection::Closed:: *)
(*Voting rule*)


k::usage="k[Pf, Pd, n] calculates the optimum fusion rule for a cooperative network with 1 bit decision fusion.
k[Pf, Pd, n, \[Rho]] calculates the optimum fusion rule for a cooperative network with 1 bit decision fusion and correlated decisions.";
k[Pf_?NumericQ,Pd_?NumericQ,n_?NumericQ,\[Rho]_:0]:=If[\[Rho] == 0,
	Min[n, \[LeftCeiling](n Log[(1 - Pd) / (1 - Pf)]) / Log[((1 - Pd) Pf) / (Pd (1 - Pf))]\[RightCeiling]],
	Module[{a = Table[FusionCenterProbabilityOfFalseAlarm[Pf,n,k,\[Rho]]+1-FusionCenterProbabilityOfDetection[Pd,n,k,\[Rho]],{k,1,n}]},
		Position[a, Min[a]][[1, 1]]
	]
]


(* ::Subsection::Closed:: *)
(*Sample complexity*)


Needs["AWGN`"];
Needs["Rayleigh`"];
Needs["Nakagami`"];


Options[SampleComplexity]={ChannelType->"AWGN",DecisionBits->\[Infinity],CorrelationCoefficient->0,Method->"Approximate",LowSNR->True,Tolerance->10^-6};
SampleComplexity::usage="SampleComplexity[\[Gamma], Pf, Pd] calculates the number of samples required for a single energy detector to operate with the specified decision probabilities at the given signal to noise ratio.
SampleComplexity[\[Gamma], Pf, Pd, n] calculates the number of samples required for a cooperative network of energy detectors to operate with the specified decision probabilities at the given signal to noise ratio.

The following methods may be specified for Rayleigh and Nakagami channels:

Method\[Rule]\"Approximate\"
Method\[Rule]\"Exact\"

By default, Method\[Rule]\""<>ToString[Method/.Options[SampleComplexity]]<>"\".

The following channel types may be specified:

ChannelType\[Rule]\"AWGN\"
ChannelType\[Rule]\"Rayleigh\"
ChannelType\[Rule]{\"Nakagami\",m}

By default, ChannelType\[Rule]\""<>ToString[ChannelType/.Options[SampleComplexity]]<>"\".

Additionally, the number of decision bits used in fusion may be specified by the DecisionBits option. By default, DecisionBits\[Rule]"<>ToString[DecisionBits/.Options[SampleComplexity]]<>". DecisionBits \[Element] {1,\[Infinity]}.

If 1 bit decision fusion is specified, then the average correlation between nodes may be specified with the CorrelationCoefficient option. By default, CorrelationCoefficient\[Rule]"<>ToString[CorrelationCoefficient/.Options[SampleComplexity]]<>".";
SampleComplexity::tol="The difference between the result `1` and the constraint `2` was greater than the specified tolerance `3`.";
SampleComplexity[\[Gamma]_,Pf_,Pd_,OptionsPattern[]]:=Module[{n=1},SampleComplexity[\[Gamma],Pf,Pd,n,ChannelType->OptionValue[ChannelType],DecisionBits->OptionValue[DecisionBits],CorrelationCoefficient->OptionValue[CorrelationCoefficient],Method->OptionValue[Method],LowSNR->OptionValue[LowSNR],Tolerance->OptionValue[Tolerance]]]
SampleComplexity[\[Gamma]_,Pf_,Pd_,n_,OptionsPattern[]]:=Module[{channelType,m,\[Rho]=OptionValue[CorrelationCoefficient],x,y,tol=OptionValue[Tolerance]},
	If[ListQ[OptionValue[ChannelType]],
		{channelType,m} = OptionValue[ChannelType];,
		channelType = OptionValue[ChannelType];
	];
	If[OptionValue[DecisionBits]==\[Infinity],
		If[\[Rho]==0,
			Switch[channelType,
				"AWGN",
				AWGNSampleComplexity[\[Gamma],Pf,Pd,n],
				"Rayleigh",
				NRayleighSampleComplexity[\[Gamma],Pf,Pd,n,Method->OptionValue[Method],LowSNR->OptionValue[LowSNR],Tolerance->OptionValue[Tolerance]],
				"Nakagami",
				NNakagamiSampleComplexity[\[Gamma],Pf,Pd,m,n,Method->OptionValue[Method],LowSNR->OptionValue[LowSNR],Tolerance->OptionValue[Tolerance]],
				_,
				SampleComplexity[\[Gamma],Pf,Pd,n,k,ChannelType->"AWGN",DecisionBits->OptionValue[DecisionBits],CorrelationCoefficient->\[Rho],Method->OptionValue[Method],LowSNR->OptionValue[LowSNR],Tolerance->OptionValue[Tolerance]]
			],
			(* No solution for correlated infinite precision fusion *)
			Undefined
		],
		If[\[Rho]!=0&&n==2,
			(* No solution for correlated fusion when n = 2 *)
			Undefined,
			(* Temporarily disable checks - we'll do our own after *)
			Off[FindRoot::lstol,NMinimize::cvmit];
			{x,y} = {x,y}/.(NMinimize[{Abs[FusionCenterProbabilityOfDetection[x,n,k[y,x,n,\[Rho]],\[Rho]]-Pd]+Abs[FusionCenterProbabilityOfFalseAlarm[y,n,k[y,x,n,\[Rho]],\[Rho]]-Pf],0<x<1&&0<y<1},{x,y}][[2]]);
			On[FindRoot::lstol,NMinimize::cvmit];
			If[!(Abs[FusionCenterProbabilityOfDetection[x,n,k[y,x,n,\[Rho]],\[Rho]] - Pd] <= tol),Message[SampleComplexity::tol, FusionCenterProbabilityOfDetection[x,n,k[y,x,n,\[Rho]],\[Rho]]//N, Pd//N, tol//N]];
			If[!(Abs[FusionCenterProbabilityOfFalseAlarm[y,n,k[y,x,n,\[Rho]],\[Rho]] - Pf] <= tol),Message[SampleComplexity::tol, FusionCenterProbabilityOfFalseAlarm[y,n,k[y,x,n,\[Rho]],\[Rho]]//N, Pf//N, tol//N]];
			Switch[channelType,
				"AWGN",
				AWGNSampleComplexity[\[Gamma],y,x],
				"Rayleigh",
				NRayleighSampleComplexity[\[Gamma],y,x,Method->OptionValue[Method],LowSNR->OptionValue[LowSNR],Tolerance->OptionValue[Tolerance]],
				"Nakagami",
				NNakagamiSampleComplexity[\[Gamma],y,x,m,Method->OptionValue[Method],LowSNR->OptionValue[LowSNR],Tolerance->OptionValue[Tolerance]],
				_,
				SampleComplexity[\[Gamma],y,x,n,ChannelType->"AWGN",DecisionBits->OptionValue[DecisionBits],CorrelationCoefficient->\[Rho],Method->OptionValue[Method],LowSNR->OptionValue[LowSNR],Tolerance->OptionValue[Tolerance]]
			]
		]
	]
]


End[];


EndPackage[];
