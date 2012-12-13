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


(* ::Subsection:: *)
(*Version information*)


(* ::Text:: *)
(*13/12/2012*)
(*1.41*)


(* ::Subsection:: *)
(*Changelog*)


(* ::Text:: *)
(*Version 1.41: Updated help for some functions.*)
(*Version 1.40: Changed LowSNR to False by default.*)
(*Version 1.34: Changed "NRice" to "Rice", changed SSC to SEC.*)
(*Version 1.33: Added TimingHelp and ToleranceHelp functions and enabled symbol protection.*)
(*Version 1.32: Added help generation functions.*)
(*Version 1.31: Added support for diversity reception.*)
(*Version 1.3: Added support for n-bit decisions.*)
(*Version 1.2: Added basic support for single bit decision double threshold type networks - more work needs to be done on this to extend support for n-bit decision type networks.*)
(*Version 1.11: Added database lookup and caching functionality to the SampleComplexity function.*)
(*Version 1.1: Introduced RelevantOptions function to simplify option management, restructured ProbabilityOfDetection and SampleComplexity functions so that they carry the same options as their children.*)
(*Version 1.0: First working version, minor bug fixes to follow.*)


(* ::Section:: *)
(*Public*)


BeginPackage["Network`"];


Protect[ChannelType, DiversityType, DecisionBits, CorrelationCoefficient, Algorithm, LowSNR, Timed, MaxTime, DatabaseLookup, DatabaseCaching, MNSwitchingPoint];


(* ::Subsection:: *)
(*Decision probabilities*)


(* ::Subsubsection::Closed:: *)
(*Probability of false alarm (general)*)


ProbabilityOfFalseAlarm;


(* ::Subsubsection::Closed:: *)
(*Probability of acquisition (general)*)


ProbabilityOfAcquisition;


(* ::Subsubsection::Closed:: *)
(*Probability of detection (general)*)


ProbabilityOfDetection;


(* ::Subsubsection::Closed:: *)
(*Probability of missed detection (general)*)


ProbabilityOfMissedDetection;


(* ::Subsubsection::Closed:: *)
(*Probability of false alarm (1 bit decision only)*)


FusionCenterProbabilityOfFalseAlarm;


(* ::Subsubsection::Closed:: *)
(*Probability of detecion (1 bit decision only)*)


FusionCenterProbabilityOfDetection;


(* ::Subsection::Closed:: *)
(*Optimum threshold*)


\[Lambda]opt;


(* ::Subsection::Closed:: *)
(*Optimum voting rule*)


k;


(* ::Subsection::Closed:: *)
(*Sample complexity*)


SampleComplexity;


(* ::Subsection::Closed:: *)
(*Help generation*)


DefaultHelp;


MethodHelp;


LowSNRHelp;


DiversityTypeHelp;


TimingHelp;


ToleranceHelp;


(* ::Section:: *)
(*Private*)


Begin["`Private`"];


Needs["AWGN`"];
Needs["Rayleigh`"];
Needs["Nakagami`"];
Needs["Rice`"];
Needs["DBLogging`"];


(* ::Subsection::Closed:: *)
(*Help generation*)


DefaultHelp[fName_,optionSpec_] := Module[{help, n, options, defaultOption},
	If[ListQ[optionSpec], options = optionSpec, options = {optionSpec}];
	help = "By default, ";
	For[n = 1, n <= Length[options], n++,
		Which[
			1 < n < Length[options],
				help = help <> ", ",
			n == Length[options] && n != 1,
				help = help <> " and "
		];
		defaultOption = options[[n]]/.Options[fName];
		If[StringQ[defaultOption],
			defaultOption = "\"" <> ToString[defaultOption] <> "\"",
			If[NumericQ[defaultOption] && !IntegerQ[defaultOption],
				defaultOption = ToString[defaultOption, InputForm],
				defaultOption = ToString[defaultOption]
			]
		];
		help = help <> ToString[options[[n]]] <> "\[Rule]" <> defaultOption;
	];
	help = help <> "."
]


MethodHelp[fName_] := "The following methods may be specified:

Method\[Rule]\"Approximate\"
Method\[Rule]\"Exact\"

" <> Evaluate[DefaultHelp[fName, Method]];


LowSNRHelp := "If Method\[Rule]\"Approximate\", then the LowSNR option can be used to specify whether to use a low signal to noise ratio approximation. "<>DefaultHelp[AWGNProbabilityOfDetection,LowSNR];


DiversityTypeHelp[fName_] := "The following diversity reception schemes may be specified:

DiversityType\[Rule]\"None\"
DiversityType\[Rule]\"MRC\"
DiversityType\[Rule]\"EGC\"
DiversityType\[Rule]\"SC\"
DiversityType\[Rule]{\"SEC\", \[Gamma]t} (where \[Gamma]t is the threshold switching value)
DiversityType\[Rule]\"SLC\"
DiversityType\[Rule]\"SLS\"

" <> DefaultHelp[fName, DiversityType];


TimingHelp[fName_] := "Function timing options may be specified using the Timed option. " <> DefaultHelp[fName, {Timed, MaxIterations, MaxTime}] <> " If Timed\[Rule]True, then a {Pd, time} list of values will be returned. The Timed option is incompatible with database lookup/caching."


ToleranceHelp[fName_] := "The calculation tolerance may be specified using the Tolerance option. " <> DefaultHelp[fName, Tolerance];


ChannelTypeHelp[fName_] := "The following channel types may be specified:

ChannelType\[Rule]\"AWGN\"
ChannelType\[Rule]\"Rayleigh\"
ChannelType\[Rule]{\"Nakagami\", m}
ChannelType\[Rule]{\"Rice\", K}

" <> DefaultHelp[fName, ChannelType];


DecisionBitsHelp[fName_] := "Test statistic compression may be specified using the DecisionBits option. " <> DefaultHelp[fName, DecisionBits];


DatabaseHelp[fName_] := "Database lookup/caching may be enabled with the DatabaseLookup and DatabaseCaching options. For database caching, both the DatabaseLookup and DatabaseCaching options must be set to True. " <> DefaultHelp[fName, {DatabaseLookup, DatabaseCaching}];


CorrelationHelp[fName_] := "Additionally, the average correlation between nodes may be specified with the CorrelationCoefficient option. " <> DefaultHelp[fName, CorrelationCoefficient];


(* ::Subsection:: *)
(*Decision probabilities*)


(* ::Subsubsection::Closed:: *)
(*Probability of detection (general)*)


Options[ProbabilityOfDetection] = {ChannelType->"AWGN", DiversityType->"SLC", DecisionBits->\[Infinity], CorrelationCoefficient->0, Method->"Approximate", Algorithm->"Numerical", LowSNR->False, Timed->False, MaxTime->600, MaxIterations->1000, DatabaseLookup->False, DatabaseCaching->False};
ProbabilityOfDetection::usage="ProbabilityOfDetection[M, \[Gamma], \[Lambda], n] calculates the probability of detection for the given number of samples, M, signal to noise ratio per sample, \[Gamma], threshold, \[Lambda], and number of diversity branches, n.\n\n" <> DiversityTypeHelp[ProbabilityOfDetection] <> "\n\n" <> ChannelTypeHelp[ProbabilityOfDetection] <> "\n\n" <> MethodHelp[ProbabilityOfDetection] <> "\n\n" <> DecisionBitsHelp[ProbabilityOfDetection] <> "\n\n" <> CorrelationHelp[ProbabilityOfDetection] <> "\n\n" <> TimingHelp[ProbabilityOfDetection] <> "\n\n" <> DatabaseHelp[ProbabilityOfDetection];
ProbabilityOfDetection::opt="`1` and `2` options are mutually exclusive. Aborting...";
ProbabilityOfDetection::k=ProbabilityOfFalseAlarm::k;
ProbabilityOfDetection::Nb=ProbabilityOfFalseAlarm::Nb;
ProbabilityOfDetection::\[Lambda]=ProbabilityOfFalseAlarm::\[Lambda];
ProbabilityOfDetection::\[Gamma]=ProbabilityOfFalseAlarm::\[Lambda];
ProbabilityOfDetection[M_,\[Gamma]_,\[Lambda]_,OptionsPattern[]]:=Module[{RelevantOptions, n = 1},
	RelevantOptions[target_]:=FilterRules[Table[#[[i]]->OptionValue[#[[i]]],{i,Length[#]}]&[Options[ProbabilityOfDetection][[All,1]]],Options[target][[All,1]]];
	ProbabilityOfDetection[M,\[Gamma],\[Lambda],n,#/.(DiversityType/.#)->"None"&[RelevantOptions[ProbabilityOfDetection]]]
]
ProbabilityOfDetection[M_,\[Gamma]_,\[Lambda]_,n_,OptionsPattern[]]:=Module[{k=Null},
	RelevantOptions[target_]:=FilterRules[Table[#[[i]]->OptionValue[#[[i]]],{i,Length[#]}]&[Options[ProbabilityOfDetection][[All,1]]],Options[target][[All,1]]];
	ProbabilityOfDetection[M,\[Gamma],\[Lambda],n,k,RelevantOptions[ProbabilityOfDetection]]
]
ProbabilityOfDetection[M_,\[Gamma]_,\[Lambda]_,n_,k_,OptionsPattern[]]:=Module[{channelType, m, \[Rho] = OptionValue[CorrelationCoefficient], Nb = OptionValue[DecisionBits], RelevantOptions, f, g, time, result, rationalPf},
	RelevantOptions[target_]:=FilterRules[Table[#[[i]]->OptionValue[#[[i]]],{i,Length[#]}]&[Options[ProbabilityOfDetection][[All,1]]],Options[target][[All,1]]];
	If[OptionValue[DatabaseLookup] && OptionValue[Timed], Message[ProbabilityOfDetection::opt,"DatabaseLookup","Timed"]; Abort[]];
	If[OptionValue[DatabaseCaching] && OptionValue[Timed], Message[ProbabilityOfDetection::opt,"DatabaseCaching","Timed"]; Abort[]];
	If[ListQ[\[Gamma]] && (Length[\[Gamma]] != n), Message[ProbabilityOfDetection::\[Gamma], \[Gamma], n]; Abort[]];
	If[ListQ[OptionValue[ChannelType]],
		If[Length[OptionValue[ChannelType]]==2,
			{channelType,m} = OptionValue[ChannelType],
			channelType = OptionValue[ChannelType][[1]]
		],
		channelType = OptionValue[ChannelType];
	];
	g[\[Gamma]0_,\[Lambda]0_,n0_]:=Switch[channelType,
		"AWGN",
		AWGNProbabilityOfDetection[M,\[Gamma]0,\[Lambda]0,n0,RelevantOptions[AWGNProbabilityOfDetection]],
		"Rayleigh",
		NRayleighProbabilityOfDetection[M,\[Gamma]0,\[Lambda]0,n0,RelevantOptions[NRayleighProbabilityOfDetection]],
		"Nakagami",
		NakagamiProbabilityOfDetection[M,\[Gamma]0,\[Lambda]0,m,n0,RelevantOptions[NakagamiProbabilityOfDetection]],
		"Rice",
		RiceProbabilityOfDetection[M,\[Gamma]0,\[Lambda]0,m,n0,RelevantOptions[RiceProbabilityOfDetection]],
		_,
		Undefined
	];
	f := Which[
		Nb == \[Infinity],
			Which[
				\[Rho] == 0,
					Which[
						!ListQ[\[Lambda]],
							g[\[Gamma],\[Lambda],n],
						ListQ[\[Lambda]],
							Which[
								Length[\[Lambda]] == 1,
									g[\[Gamma],Last[\[Lambda]],n],
								Length[\[Lambda]] == 2,
									g[\[Gamma],Last[\[Lambda]],n],
								True,
									Undefined
							],
						True,
							Undefined
					],
				True,
					Undefined
			],
		Nb >= 1,
			If[TrueQ[k==Null],Message[ProbabilityOfDetection::k,Nb];Abort[]];
			Module[{probabilities,temp},
				Which[
					!ListQ[\[Lambda]],
						If[Nb != 1, Message[ProbabilityOfDetection::Nb, \[Lambda], 2^Nb - 1, Nb]; Abort[]];
						Which[
							!ListQ[\[Gamma]],
								probabilities = g[\[Gamma],\[Lambda],1],
							ListQ[\[Gamma]],
								If[OptionValue[Timed],
									{temp, time} = Table[g[\[Gamma][[n0]],\[Lambda],1],{n0,n}]//Transpose,
									temp = Table[g[\[Gamma][[n0]],\[Lambda],1],{n0,n}]
								];
								probabilities = Table[{1 - temp[[i]], temp[[i]]},{i,n}],
							True,
								Undefined
						],
					ListQ[\[Lambda]],
						Which[
							!ListQ[First[\[Lambda]]],
								If[Length[\[Lambda]] != 2^Nb - 1, Message[ProbabilityOfDetection::Nb, \[Lambda], 2^Nb - 1, Nb]; Abort[]];
								Which[
									!ListQ[\[Gamma]],
										If[OptionValue[Timed],
											{temp, time} = Table[g[\[Gamma],\[Lambda]0,1],{\[Lambda]0,\[Lambda]}]//Transpose,
											temp = Table[g[\[Gamma],\[Lambda]0,1],{\[Lambda]0,\[Lambda]}]
										];
										probabilities = Flatten[{-Differences[Flatten[{1,temp}]],Last[temp]}],
									ListQ[\[Gamma]],
										probabilities = Table[
											If[OptionValue[Timed],
												{temp, time} = Table[g[\[Gamma][[n0]],\[Lambda]0,1],{\[Lambda]0,\[Lambda]}]//Transpose,
												temp = Table[g[\[Gamma][[n0]],\[Lambda]0,1],{\[Lambda]0,\[Lambda]}]
											];
											Flatten[{-Differences[Flatten[{1,temp}]],Last[temp]}],{n0,n}
										],
									True,
										Undefined
								],
							ListQ[First[\[Lambda]]],
								If[Length[\[Lambda]] != n, Message[ProbabilityOfDetection::\[Lambda], \[Lambda], n]; Abort[]];
								Table[If[Length[\[Lambda][[i]]] != 2^Nb - 1, Message[ProbabilityOfDetection::Nb, \[Lambda][[i]], 2^Nb - 1, Nb]; Abort[]],{i,Length[\[Lambda]]}];
								Which[
									!ListQ[\[Gamma]],
										probabilities = Table[
											If[OptionValue[Timed],
												{temp, time} = Table[g[\[Gamma],\[Lambda]0,1],{\[Lambda]0,\[Lambda][[n0]]}]//Transpose,
												temp = Table[g[\[Gamma],\[Lambda]0,1],{\[Lambda]0,\[Lambda][[n0]]}]
											];
											Flatten[{-Differences[Flatten[{1,temp}]],Last[temp]}],{n0,n}
										],
									ListQ[\[Gamma]],
										probabilities = Table[
											If[OptionValue[Timed],
												{temp, time} = Table[g[\[Gamma][[n0]],\[Lambda]0,1],{\[Lambda]0,\[Lambda][[n0]]}]//Transpose,
												temp = Table[g[\[Gamma][[n0]],\[Lambda]0,1],{\[Lambda]0,\[Lambda][[n0]]}]
											];
											Flatten[{-Differences[Flatten[{1,temp}]],Last[temp]}],{n0,n}
										],
									True,
										Undefined
								],
							True,
								Undefined
						],
					True,
						Undefined
				];
				If[OptionValue[Timed],
					{FusionCenterProbabilityOfDetection[probabilities,n,k,\[Rho]],Total[time]},
					FusionCenterProbabilityOfDetection[probabilities,n,k,\[Rho]]
				]
			],
		True,
			Undefined
	];
	If[OptionValue[DatabaseLookup],
		result = GetProbabilityOfDetection[OptionValue[Algorithm],channelType,M,\[Gamma],ProbabilityOfFalseAlarm[M,\[Lambda],n,RelevantOptions[ProbabilityOfFalseAlarm]]//N,n,m,RelevantOptions[GetProbabilityOfDetection]];
		If[TrueQ[result==Null],
			result = f;
			If[OptionValue[DatabaseCaching],
				(* For correct retrieval of results later, attempt to convert Pf to rational form *)
				rationalPf = Round[ProbabilityOfFalseAlarm[M,\[Lambda],n,RelevantOptions[ProbabilityOfFalseAlarm]]*10^6//N]/10^6;
				CacheProbabilityOfDetection[OptionValue[Algorithm],channelType,M,\[Gamma],rationalPf,n,m,result//N,RelevantOptions[CacheProbabilityOfDetection]];
			];
		];
		result,
		f
	]
]


(* ::Subsubsection::Closed:: *)
(*Probability of missed detection (general)*)


Options[ProbabilityOfMissedDetection] = Options[ProbabilityOfDetection];
ProbabilityOfMissedDetection::usage = StringReplace[ProbabilityOfDetection::usage,{"ProbabilityOfDetection"->"ProbabilityOfMissedDetection","detection"->"missed detection"}];
ProbabilityOfMissedDetection[M_,\[Gamma]_,\[Lambda]_,OptionsPattern[]]:=Module[{RelevantOptions, n = 1},
	RelevantOptions[target_]:=FilterRules[Table[#[[i]]->OptionValue[#[[i]]],{i,Length[#]}]&[Options[ProbabilityOfMissedDetection][[All,1]]],Options[target][[All,1]]];
	ProbabilityOfMissedDetection[M,\[Gamma],\[Lambda],n,#/.(DiversityType/.#)->"None"&[RelevantOptions[ProbabilityOfMissedDetection]]]
]
ProbabilityOfMissedDetection[M_,\[Gamma]_,\[Lambda]_,n_,OptionsPattern[]]:=Module[{k=Null},
	RelevantOptions[target_]:=FilterRules[Table[#[[i]]->OptionValue[#[[i]]],{i,Length[#]}]&[Options[ProbabilityOfMissedDetection][[All,1]]],Options[target][[All,1]]];
	ProbabilityOfMissedDetection[M,\[Gamma],\[Lambda],n,k,RelevantOptions[ProbabilityOfMissedDetection]]
]
ProbabilityOfMissedDetection[M_,\[Gamma]_,\[Lambda]_,n_,k_,OptionsPattern[]]:=Module[{channelType,m,\[Rho]=OptionValue[CorrelationCoefficient],RelevantOptions,f},
	RelevantOptions[target_]:=FilterRules[Table[#[[i]]->OptionValue[#[[i]]],{i,Length[#]}]&[Options[ProbabilityOfMissedDetection][[All,1]]],Options[target][[All,1]]];
	Which[
		!ListQ[\[Lambda]],
			1 - ProbabilityOfDetection[M,\[Gamma],\[Lambda],n,k,RelevantOptions[ProbabilityOfDetection]],
		ListQ[\[Lambda]],
			Which[
				Length[\[Lambda]] == 1,
					1 - ProbabilityOfDetection[M,\[Gamma],First[\[Lambda]],n,k,RelevantOptions[ProbabilityOfDetection]],
				Length[\[Lambda]] == 2,
					1 - ProbabilityOfDetection[M,\[Gamma],First[\[Lambda]],n,k,RelevantOptions[ProbabilityOfDetection]],
				True,
					Undefined
			],
		True,
			Undefined
	]
]


(* ::Subsubsection::Closed:: *)
(*Probability of false alarm (general)*)


Options[ProbabilityOfFalseAlarm] = {DiversityType->OptionValue[ProbabilityOfDetection, DiversityType], DecisionBits->OptionValue[ProbabilityOfDetection, DecisionBits], CorrelationCoefficient->OptionValue[ProbabilityOfDetection, CorrelationCoefficient], Method->OptionValue[ProbabilityOfDetection, Method]};
ProbabilityOfFalseAlarm::usage="ProbabilityOfFalseAlarm[M, \[Lambda], n] calculates the probability of false alarm for the given number of samples, M, threshold, \[Lambda], and number of diversity branches, n.\n\n" <> DiversityTypeHelp[ProbabilityOfFalseAlarm] <> "\n\n" <> MethodHelp[ProbabilityOfFalseAlarm] <> "\n\n" <> DecisionBitsHelp[ProbabilityOfFalseAlarm] <> "\n\n" <> CorrelationHelp[ProbabilityOfFalseAlarm];
ProbabilityOfFalseAlarm::k="Error: Must specify a voting rule when DecisionBits\[Rule]`1`";
ProbabilityOfFalseAlarm::Nb="Error: The list `1` must be of length `2` when DecisionBits->`3`.";
ProbabilityOfFalseAlarm::\[Lambda]="Error: The list `1` must be of length `2` when n = `2`.";
ProbabilityOfFalseAlarm[M_,\[Lambda]_,OptionsPattern[]]:=Module[{RelevantOptions, n = 1},
	RelevantOptions[target_]:=FilterRules[Table[#[[i]]->OptionValue[#[[i]]],{i,Length[#]}]&[Options[ProbabilityOfFalseAlarm][[All,1]]],Options[target][[All,1]]];
	ProbabilityOfFalseAlarm[M,\[Lambda],n,#/.(DiversityType/.#)->"None"&[RelevantOptions[ProbabilityOfFalseAlarm]]]
]
ProbabilityOfFalseAlarm[M_,\[Lambda]_,n_,OptionsPattern[]]:=Module[{RelevantOptions, k = Null},
	RelevantOptions[target_]:=FilterRules[Table[#[[i]]->OptionValue[#[[i]]],{i,Length[#]}]&[Options[ProbabilityOfFalseAlarm][[All,1]]],Options[target][[All,1]]];
	ProbabilityOfFalseAlarm[M,\[Lambda],n,k,RelevantOptions[ProbabilityOfFalseAlarm]]
]
ProbabilityOfFalseAlarm[M_,\[Lambda]_,n_,k_,OptionsPattern[]]:=Module[{RelevantOptions, \[Rho] = OptionValue[CorrelationCoefficient], Nb = OptionValue[DecisionBits], diversityType = OptionValue[DiversityType]},
	RelevantOptions[target_]:=FilterRules[Table[#[[i]]->OptionValue[#[[i]]],{i,Length[#]}]&[Options[ProbabilityOfFalseAlarm][[All,1]]],Options[target][[All,1]]];
	Which[
		Nb == \[Infinity],
			Which[
				\[Rho] == 0,
					Which[
						!ListQ[\[Lambda]],
							AWGNProbabilityOfFalseAlarm[M,\[Lambda],n,RelevantOptions[AWGNProbabilityOfFalseAlarm]],
						ListQ[\[Lambda]],
							Which[
								Length[\[Lambda]] == 1,
								AWGNProbabilityOfFalseAlarm[M,Last[\[Lambda]],n,RelevantOptions[AWGNProbabilityOfFalseAlarm]],
								Length[\[Lambda]] == 2,
									AWGNProbabilityOfFalseAlarm[M,Last[\[Lambda]],n,RelevantOptions[AWGNProbabilityOfFalseAlarm]],
								True,
									Undefined
							],
						True,
							Undefined
					],
				True,
					Undefined
			],
		Nb >= 1,
			If[TrueQ[k==Null],Message[ProbabilityOfFalseAlarm::k,Nb];Abort[]];
			Module[{probabilities,temp},
				Which[
					!ListQ[\[Lambda]],
						If[Nb != 1, Message[ProbabilityOfFalseAlarm::Nb, \[Lambda], 2^Nb - 1, Nb]; Abort[]];
						probabilities = AWGNProbabilityOfFalseAlarm[M,\[Lambda],RelevantOptions[AWGNProbabilityOfFalseAlarm]],
					ListQ[\[Lambda]],
						Which[
							!ListQ[First[\[Lambda]]],
								If[Length[\[Lambda]] != 2^Nb - 1, Message[ProbabilityOfFalseAlarm::Nb, \[Lambda], 2^Nb - 1, Nb]; Abort[]];
								temp = Table[AWGNProbabilityOfFalseAlarm[M,x,RelevantOptions[AWGNProbabilityOfFalseAlarm]],{x,\[Lambda]}];
								probabilities = Flatten[{-Differences[Flatten[{1,temp}]],Last[temp]}],
							ListQ[First[\[Lambda]]],
								If[Length[\[Lambda]] != n, Message[ProbabilityOfFalseAlarm::\[Lambda], \[Lambda], n]; Abort[]];
								Table[If[Length[\[Lambda][[i]]] != 2^Nb - 1, Message[ProbabilityOfFalseAlarm::Nb, \[Lambda][[i]], 2^Nb - 1, Nb]; Abort[]],{i,Length[\[Lambda]]}];
								probabilities = Table[temp = Table[AWGNProbabilityOfFalseAlarm[M,x,RelevantOptions[AWGNProbabilityOfFalseAlarm]],{x,\[Lambda][[n0]]}];Flatten[{-Differences[Flatten[{1,temp}]],Last[temp]}],{n0,n}],
							True,
								Undefined
						],
					True,
						Undefined
				];
				FusionCenterProbabilityOfFalseAlarm[probabilities,n,k,\[Rho]]
			],
		True,
			Undefined
	]
]


(* ::Subsubsection::Closed:: *)
(*Probability of acquisition (general)*)


Options[ProbabilityOfAcquisition] = Options[ProbabilityOfFalseAlarm];
ProbabilityOfAcquisition::usage = StringReplace[ProbabilityOfFalseAlarm::usage,{"ProbabilityOfFalseAlarm"->"ProbabilityOfAcquisition","false alarm"->"acquisition"}];
ProbabilityOfAcquisition[M_,\[Lambda]_,OptionsPattern[]]:=Module[{RelevantOptions, n = 1},
	RelevantOptions[target_]:=FilterRules[Table[#[[i]]->OptionValue[#[[i]]],{i,Length[#]}]&[Options[ProbabilityOfAcquisition][[All,1]]],Options[target][[All,1]]];
	ProbabilityOfAcquisition[M,\[Lambda],n,#/.(DiversityType/.#)->"None"&[RelevantOptions[ProbabilityOfAcquisition]]]
]
ProbabilityOfAcquisition[M_,\[Lambda]_,n_,OptionsPattern[]]:=Module[{RelevantOptions, k = Null},
	RelevantOptions[target_]:=FilterRules[Table[#[[i]]->OptionValue[#[[i]]],{i,Length[#]}]&[Options[ProbabilityOfAcquisition][[All,1]]],Options[target][[All,1]]];
	ProbabilityOfAcquisition[M,\[Lambda],n,k,RelevantOptions[ProbabilityOfAcquisition]]
]
ProbabilityOfAcquisition[M_,\[Lambda]_,n_,k_,OptionsPattern[]]:=Module[{RelevantOptions},
	RelevantOptions[target_]:=FilterRules[Table[#[[i]]->OptionValue[#[[i]]],{i,Length[#]}]&[Options[ProbabilityOfAcquisition][[All,1]]],Options[target][[All,1]]];
	Which[
		!ListQ[\[Lambda]],
			1 - ProbabilityOfFalseAlarm[M,\[Lambda],n,k,RelevantOptions[ProbabilityOfFalseAlarm]],
		ListQ[\[Lambda]],
			Which[
				Length[\[Lambda]] == 1,
					1 - ProbabilityOfFalseAlarm[M,First[\[Lambda]],n,k,RelevantOptions[ProbabilityOfFalseAlarm]],
				Length[\[Lambda]] == 2,
					1 - ProbabilityOfFalseAlarm[M,First[\[Lambda]],n,k,RelevantOptions[ProbabilityOfFalseAlarm]],
				True,
					Undefined
			]
		True,
			Undefined
	]
]


(* ::Subsubsection::Closed:: *)
(*Probability of detection (fusion center)*)


FusionCenterProbabilityOfDetection::usage="FusionCenterProbabilityOfDetection[Pd, n, k] calculates the overall probability of detection for a cooperative network with 1 bit decision fusion.
FusionCenterProbabilityOfDetection[Pd, n, k, \[Rho]] calculates the overall probability of detection for a cooperative network with 1 bit decision fusion and correlated decisions.";
FusionCenterProbabilityOfDetection[P_,n_?NumericQ,k_?NumericQ,\[Rho]_:0]:=FusionCenterProbabilityOfFalseAlarm[P,n,k,\[Rho]]


(* ::Subsubsection::Closed:: *)
(*Probability of false alarm (fusion center)*)


FusionCenterProbabilityOfFalseAlarm::usage="FusionCenterProbabilityOfFalseAlarm[Pf, n, k] calculates the overall probability of false alarm for a cooperative network with 1 bit decision fusion.
FusionCenterProbabilityOfFalseAlarm[Pf, n, k, \[Rho]] calculates the overall probability of false alarm for a cooperative network with 1 bit decision fusion and correlated decisions.";
FusionCenterProbabilityOfFalseAlarm[P_,n_?IntegerQ,k_?IntegerQ,\[Rho]_:0]:=FusionCenterProbabilityOfFalseAlarm[{P},n,k,\[Rho]]
FusionCenterProbabilityOfFalseAlarm[P_?ListQ,n_?IntegerQ,k_?IntegerQ,\[Rho]_:0]:=Which[
	Length[P] == 1,
		Which[
			\[Rho] == 0,
				1 - CDF[BinomialDistribution[n, First[P]], k - 1],
			0 < \[Rho] <= 1,
				Which[
					n == 1,
						First[P],
					n == 2,
						Undefined,
					n > 2,
						Sum[Sum[(-1)^i Binomial[l, i] First[P] Product[(\[Rho] (s + 1 - First[P]) + First[P]) / (1 + s \[Rho]),{s, 0, n - l + i - 2}],{i, 0, l}],{l, 0, k - 1}],
					True,
						Undefined
				],
			True,
				Undefined
		],
	Length[P] > 1,
		Which[
			\[Rho] == 0,
				Which[
					!ListQ[First[P]],
						Which[
							Length[P] == 2,
								FusionCenterProbabilityOfFalseAlarm[Last[P],n,k,\[Rho]],
							Length[P] == 3,
								Total[Table[Binomial[n,l] P[[1]]^(n-l) P[[3]]^l / (1-P[[2]])^n,{l,k,n}]],
							TrueQ[N[Mod[Log[2,Length[P]],1]] == 0],
								Module[{decisions=Table[Unique[],{n}],probabilities,Nb=Log[2,Length[P]],p,precision=10000},
									Total[
										Table[
											probabilities=Table[Subscript[p, decisions[[x]]],{x,n}]/.FindInstance[Flatten[{Total[decisions]==x,Table[0<=decisions[[x]]<=2^Nb-1,{x,n}]}],decisions,Integers,precision];
											Table[Product[probabilities[[x,y]],{y,Length[probabilities[[x]]]}],{x,Length[probabilities]}]//Total,
											{x,k,n (2^Nb-1)}
										]/.Table[Subscript[p, x]->P[[x+1]],{x,0,2^Nb-1}]
									]
								],
							True,
								Undefined
						],
					ListQ[First[P]],
						Which[
							TrueQ[N[Mod[Log[2,Length[First[P]]],1]] == 0],
								Module[{decisions=Table[Unique[],{n}],probabilities,Nb=Log[2,Length[First[P]]],p,precision=10000},
									Total[
										Table[
											probabilities=Table[Subscript[p, x, decisions[[x]]],{x,n}]/.FindInstance[Flatten[{Total[decisions]==x,Table[0<=decisions[[x]]<=2^Nb-1,{x,n}]}],decisions,Integers,precision];
											Table[Product[probabilities[[x,y]],{y,Length[probabilities[[x]]]}],{x,Length[probabilities]}]//Total,
											{x,k,n (2^Nb-1)}
										]/.Flatten[Table[Subscript[p, x, d]->P[[x, d + 1]],{x, n},{d, 0, 2^Nb - 1}]]
									]
								],
							True,
								Undefined
						],
					True,
						Undefined
				],
			True,
				Undefined
		],
	True,
		Undefined
]


(* ::Subsection::Closed:: *)
(*Optimum threshold*)


(*\[Lambda]opt::length="Error: the list `1` should be the same length as `2`.";
\[Lambda]opt[M_,Pf_,Pd_,threshold_,k_]:=\[Lambda]opt[{M},{Pf},{Pd},{threshold},k]
\[Lambda]opt[M_?ListQ,Pf_?ListQ,Pd_?ListQ,thresholds_?ListQ,k_]:=Module[{n = Length[Pf]},
	If[Length[Pf]!=Length[Pd],
		Message[\[Lambda]opt::length,Pf,Pd];
		Abort[];
	];
	If[Length[Union[Pf]] == 1 && Length[Union[Pd]] == 1,
		,
		FindRoot[Table[D[Sum[Exp[-I 2 Pi j k / (n + 1)] / (n + 1) Product[Pf[[x]]Exp[I 2 Pi j / (n + 1) + (1 - Pf[[x]])],{x,n}],{j,0,n}] + Sum[Exp[-I 2 Pi j k / (n + 1)] / (n + 1) Product[Pd[[x]]Exp[I 2 Pi j / (n + 1) + (1 - Pd[[x]])],{x,n}],{j,0,n}],\[Lambda]]==0,{\[Lambda],thresholds}],Table[{thresholds[[x]],M[[x]]},{x,Length[thresholds]}]]
	]
]*)


(* ::Subsection::Closed:: *)
(*Optimum voting rule*)


k::usage="k[Pf, Pd, n] calculates the optimum fusion rule for a cooperative network with 1 bit decision fusion.
k[Pf, Pd, n, \[Rho]] calculates the optimum fusion rule for a cooperative network with 1 bit decision fusion and correlated decisions.
k[{\!\(\*SubscriptBox[\(P\), \(00\)]\), \!\(\*SubscriptBox[\(P\), \(01\)]\), ...}, {\!\(\*SubscriptBox[\(P\), \(10\)]\), \!\(\*SubscriptBox[\(P\), \(11\)]\), ...}, n] calculates the optimum fusion rule for a cooperative network with arbitrary decision quantisation.
k[{\!\(\*SubscriptBox[\(P\), \(00\)]\), \!\(\*SubscriptBox[\(P\), \(01\)]\), ...}, {\!\(\*SubscriptBox[\(P\), \(10\)]\), \!\(\*SubscriptBox[\(P\), \(11\)]\), ...}, n, \[Rho]] calculates the optimum fusion rule for a cooperative network with arbitrary decision quantisation and correlated decisions.";
k::length="Error: the list `1` should be the same length as `2`.";
k[Pf_?NumericQ,Pd_?NumericQ,n_?NumericQ,\[Rho]_:0]:=k[{1-Pf,Pf},{1-Pd,Pd},{n,n},\[Rho]]
k[P0_?ListQ,P1_?ListQ,votes_?ListQ,\[Rho]_:0]:=Module[{v,n,Pa,\[CapitalDelta]0,Pf,Pm,\[CapitalDelta]1,Pd},
	If[Length[P0]!=Length[P1],
		Message[k::length,P0,P1];
		Abort[];
	];
	If[\[Rho] == 0,
		Switch[Length[P0],
			2,
			n = votes[[1]];
			{Pa, Pf} = P0;
			{Pm, Pd} = P1;
			Min[n, \[LeftCeiling](n Log[Pm / Pa]) / Log[(Pf Pm) / (Pd Pa)]\[RightCeiling]],
			3,
			{v,n} = votes;
			{Pa, \[CapitalDelta]0, Pf} = P0;
			{Pm, \[CapitalDelta]1, Pd} = P1;
			Min[v, \[LeftCeiling](v Log[Pa (1 - \[CapitalDelta]1) / (Pm (1 - \[CapitalDelta]0))]) / Log[(Pd Pa) / (Pf Pm)]\[RightCeiling]],
			_,
			Module[{a = Table[FusionCenterProbabilityOfFalseAlarm[Pf,n,k,\[Rho]]+1-FusionCenterProbabilityOfDetection[Pd,n,k,\[Rho]],{k,1,n}]},
				Position[a, Min[a]][[1, 1]]
			]
		],
		Module[{a = Table[FusionCenterProbabilityOfFalseAlarm[Pf,n,k,\[Rho]]+1-FusionCenterProbabilityOfDetection[Pd,n,k,\[Rho]],{k,1,n}]},
			Position[a, Min[a]][[1, 1]]
		]
	]
]


(* ::Subsection::Closed:: *)
(*Sample complexity*)


Options[SampleComplexity] = {DiversityType->"SLC", ChannelType->"AWGN", DecisionBits->\[Infinity], CorrelationCoefficient->0, Method->"Approximate", Algorithm->"Numerical", LowSNR->True, Tolerance->10^-6, DatabaseLookup->False, DatabaseCaching->False};
SampleComplexity::usage="SampleComplexity[\[Gamma], Pf, Pd, n] calculates the number of samples required for a cooperative network of energy detectors to operate with the specified decision probabilities at the given signal to noise ratio.\n\n" <> DiversityTypeHelp[SampleComplexity] <> "\n\n" <> ChannelTypeHelp[SampleComplexity] <> "\n\n" <> MethodHelp[SampleComplexity] <> "\n\n" <> DecisionBitsHelp[SampleComplexity] <> "\n\n" <> CorrelationHelp[SampleComplexity] <> "\n\n" <> DatabaseHelp[SampleComplexity];
SampleComplexity::tol="The difference between the result `1` and the constraint `2` was greater than the specified tolerance `3`.";
SampleComplexity::opt="`1` and `2` options are mutually exclusive. Aborting...";
SampleComplexity[\[Gamma]_,Pf_,Pd_,OptionsPattern[]]:=Module[{RelevantOptions, n = 1},
	RelevantOptions[target_]:=FilterRules[Table[#[[i]]->OptionValue[#[[i]]],{i,Length[#]}]&[Options[SampleComplexity][[All,1]]],Options[target][[All,1]]];
	SampleComplexity[\[Gamma],Pf,Pd,n,RelevantOptions[SampleComplexity]]
]
SampleComplexity[\[Gamma]_,Pf_,Pd_,n_,OptionsPattern[]]:=Module[{result,channelType,m,\[Rho]=OptionValue[CorrelationCoefficient],x,y,tol=OptionValue[Tolerance],RelevantOptions,f},
	RelevantOptions[target_]:=FilterRules[Table[#[[i]]->OptionValue[#[[i]]],{i,Length[#]}]&[Options[SampleComplexity][[All,1]]],Options[target][[All,1]]];

	If[ListQ[OptionValue[ChannelType]],
		If[Length[OptionValue[ChannelType]]==2,
			{channelType,m} = OptionValue[ChannelType],
			channelType = OptionValue[ChannelType][[1]]
		],
		channelType = OptionValue[ChannelType];
	];

	Switch[channelType,
		"AWGN",
		m = Null,
		"Rayleigh",
		m = 1;
	];

	f := If[OptionValue[DecisionBits]==\[Infinity],
		If[\[Rho]==0,
			Switch[channelType,
				"AWGN",
					AWGNSampleComplexity[\[Gamma], Pf, Pd, n, RelevantOptions[AWGNSampleComplexity]],
				"Rayleigh",
					RayleighSampleComplexity[\[Gamma], Pf, Pd, n, RelevantOptions[RayleighSampleComplexity]],
				"Nakagami",
					NakagamiSampleComplexity[\[Gamma], Pf, Pd, m, n, RelevantOptions[NakagamiSampleComplexity]],
				"Rice",
					RiceSampleComplexity[\[Gamma], Pf, Pd, m, n, RelevantOptions[RiceSampleComplexity]],
				_,
					SampleComplexity[\[Gamma], Pf, Pd, n, RelevantOptions[SampleComplexity]]
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
				RayleighSampleComplexity[\[Gamma],y,x,RelevantOptions[RayleighSampleComplexity]],
				"Nakagami",
				NakagamiSampleComplexity[\[Gamma],y,x,m,RelevantOptions[NakagamiSampleComplexity]],
				"Rice",
				RiceSampleComplexity[\[Gamma],Pf,Pd,m,RelevantOptions[RiceSampleComplexity]],
				_,
				SampleComplexity[\[Gamma],y,x,n,RelevantOptions[SampleComplexity]]
			]
		]
	];

	If[OptionValue[DatabaseLookup],
		result = GetSampleComplexity[{channelType,m},OptionValue[DecisionBits],n,\[Gamma],Pf,1 - Pd];
		If[TrueQ[result==Null],
			result = f;
			If[OptionValue[DatabaseCaching],
				CacheSampleComplexity[{channelType,m},OptionValue[DecisionBits],n,\[Gamma],Pf,1 - Pd,result];
			];
		];
		result,
		f
	]
]


End[];


EndPackage[];
