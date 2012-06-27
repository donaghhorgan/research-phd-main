(* ::Package:: *)

(* ::Title:: *)
(*Rice channel functions*)


(* ::Section:: *)
(*PDF of the signal to noise ratio*)


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
	PDF[GammaDistribution[m n,\[Gamma]/m],x],
	"Approximate",
	PDF[NormalDistribution[n \[Gamma],Sqrt[n \[Gamma]^2 / m]],x],
	_,
	RicePDF[\[Gamma],m,x,n,Method->"Exact"]
]
