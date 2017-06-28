(* Wolfram Language Package *)

(* Created by the Wolfram Workbench Dec 24, 2016 *)

(* Author: flip phillips *)
(* Date: Fall 2000, repackaged Winter 2016 *)
(* Version: 1.0.2 *)
(* MathematicaVersion: 4+ *)
(* Keywords: statistics, distributions, sampling, random, von Mieses *)
(* References: 
Fisher, N.I. Statistical analysis of circular data. Cambridge, 1993.
von Mises, R. Über die "Ganzzahligkeit" der Atomgewichte und Verwandte Fragen. Physikal. Z. 19, 490-500, (11), 1918.
*)
(* Limitations: CircularNormalDistribution isn't fully defined (bug in integration / summation in 4.1 is causing trouble) *)


BeginPackage["CircularStatistics`"]
(* Exported symbols added here with SymbolName::usage *) 


CircularStatistics::usage = "CircularStatistics is a package that provides various circular distributions and statistical methods on them."

(* Distributions *)
CircularUniformDistribution::usage = "CircularUniformDistribution[] represents the uniform circular distribution on the range [0,2\[Pi]]."
CardioidDistribution::usage = "CardioidDistribution[\[Mu],\[Rho]] represents the symmetric, unimodal cardioid distribution, also known as the cosine distribution with mean direction \[Mu] and resultant length \[Rho]."
WrappedCauchyDistribution::usage = "WrappedCauchyDistribution[\[Mu],\[Rho]] represents a symmetric unimodal distribution based on a circular remapping of the Cauchy distribution with mean direction \[Mu] and resultant length \[Rho]."
WrappedNormalDistribution::usage = "WrappedNormalDistribution[\[Mu],\[Rho]] represents a symmetric unimodal distribution based on a circular remapping of the Normal (Gaussian) distribution with mean direction \[Mu] and resultant length \[Rho]."
VonMisesDistribution::usage = "VonMisesDistribution[\[Mu],\[Kappa]] represents a symmetric unimodal distribution of planar directions with mean direction \[Mu] and concentration paramater \[Kappa]."

(* measures *)
DomainQ::usage = 
"DomainQ[dist, {theta1, theta2,...}] returns True if all thetas are in the domain of the distribution dist.
 DomainQ[dist, theta] returns True if theta is in the domain of the distribution dist."
Domain::usage = "Domain[dist] returns the domain of dist."
MeanDirection::usage = "MeanDirection[dist] returns the mean direction of distribution dist."
MeanResultantLength::usage = "MeanResultantLength[dist] returns the mean resultant vector length of dist."
CircularDispersion::usage = "CircularDispersion[dist] returns the circular dispersion of distribution dist."
CircularAlpha::usage = "CircularAlpha[dist] returns the circular \[Alpha] for dist."
CircularBeta::usage = "CircularBeta[dist] returns the circular \[Beta] for dist."

(* error messages *)
CircularUniformDistribution::undefined = "Trigonometric parameters undefined for p < 1."
CardioidDistribution::posparm = "Parameter `` is expected to be positive."
CardioidDistribution::realparm = "Parameter `` is expected to be real."
CardioidDistribution::rhomax = "Parameter \[Rho] = `` is expected to be less than 1/2."
WrappedCauchyDistribution::posparm = "Parameter `` is expected to be positive."
WrappedCauchyDistribution::realparm = "Parameter `` is expected to be real."
WrappedCauchyDistribution::rhomax = "Parameter \[Rho] = `` is expected to be \[LessEqual] 1."
VonMisesDistribution::posparm = "Parameter `` is expected to be positive."
VonMisesDistribution::realparm="Parameter `` is expected to be real."


Begin["`Private`"]
(* Implementation of the package *)

A[p_,\[Kappa]_]:=BesselI[p,\[Kappa]]/BesselI[0,\[Kappa]]

(* Circular Uniform Distribution *)
(*
CircularUniformDistribution[]["RandomType"] = RandomReal

DistributionParameterQ[CircularUniformDistribution[]]^:=True

CircularUniformDistribution/:
	DomainQ[CircularUniformDistribution[],list_?VectorQ]:=
  		FreeQ[N[list],Complex]&&x
    	Scan[If[!TrueQ[#1\[GreaterEqual]0],Return[False]]&,list]=!=False

CircularUniformDistribution/:
	DomainQ[CircularUniformDistribution[],\[Theta]_]:=
  		FreeQ[N[\[Theta]],Complex]&&TrueQ[\[Theta]\[GreaterEqual]0]

CircularUniformDistribution/: Domain[CircularUniformDistribution[]]:=
  Interval[{0,2\[Pi]}]

CircularUniformDistribution/: PDF[CircularUniformDistribution[],\[Theta]_]:=
  1/2\[Pi]

CircularUniformDistribution/: CDF[CircularUniformDistribution[],\[Theta]_]:=
  Mod[\[Theta]/2\[Pi],2\[Pi]]

CircularUniformDistribution/: 
  MeanDirection[CircularUniformDistribution[]]:=Indeterminate

CircularUniformDistribution/: 
  MeanResultantLength[CircularUniformDistribution[]]:=0

CircularUniformDistribution/: 
  CircularDispersion[CircularUniformDistribution[]]:=\[Infinity]

CircularUniformDistribution/: CircularAlpha[CircularUniformDistribution[],p_]:=
  If[p\[GreaterEqual]1,0,Indeterminate]

CircularUniformDistribution/: CircularBeta[CircularUniformDistribution[],p_]:=
  If[p\[GreaterEqual]1,0,Indeterminate] 

CircularUniformDistribution/:
  Random[CircularUniformDistribution[]]:=
  2\[Pi] Random[]
*)
(*CircularUniformDistribution/:
  RandomVariate[CircularUniformDistribution[],dim_]:=
  	Table[RandomVariate[CircularUniformDistribution[]],{dim}]/;(IntegerQ[dim]&&dim>0)||
      VectorQ[dim,(IntegerQ[#]&&#>0)&]

*)

CircularUniformDistribution[]:=ProbabilityDistribution[]

End[]

EndPackage[]

