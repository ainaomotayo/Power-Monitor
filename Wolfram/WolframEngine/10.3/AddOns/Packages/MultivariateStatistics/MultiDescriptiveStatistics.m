(* :Name: Statistics`MultiDescriptiveStatistics` *)

(* :Title: Multivariate Descriptive Statistics *)

(* :Author: E. C. Martin *)

(* :Summary:
This package computes descriptive statistics (location, dispersion, shape,
and association statistics) for a sample represented as a data matrix.
The data matrix is a list of independent identically distributed
vector-valued or multivariate observations.
*)

(* :Context: Statistics`MultiDescriptiveStatistics` *)

(* :Package Version: 2.2 *)

(* :Copyright: Copyright 1993-2010 by Wolfram Research, Inc. *)

(* :History:
Original version by ECM (Wolfram Research), September 1993.
Version 2.1, January 2006 by Darren Glosemeyer (Wolfram Research),
	speed and memory improvements for a number of functions
Version 2.2 moved Ellipsoid to MultivariateStatistics` context from RegressionCommon` 
	in Mathematica version 7.0, 2008, Darren Glosemeyer (Wolfram Research).	
Version 2.3 updated for kernelization of PrincipalComponents in version 8.0, 2010, Darren Glosemeyer (Wolfram Research).
*)

(* :Source:
	Multivariate Analysis, Mardia, Kent, Bibby, Academic Press, 1979.
	A Survey of Multidimensional Medians, C. G. Small, International
		Statistical Review, 58, 3, pp. 263-277, 1990.
	Robust Statistics, P. J. Huber, John Wiley & Sons, Inc., 1980.
	Robust Estimates, Residuals, and Outlier Detection with
		Multiresponse Data, R. Gnanadesikan & J. R. Kettenring,
		Biometrics 28, March 1972.
	Rank Correlation Methods, M. G. Kendall, Hafner Publishing Co., 1955.
*)

(* :Warnings: 
	Functions based on ConvexHull currently only available for 
	dimension 2.
	Rules attached to Graphics and Graphics3D. *)
(* :Limitations: 
	Functions based on ConvexHull currently only available for 
	dimension 2. *)

(* :Mathematica Version: 8.0 *)


(* multivariate location statistics *)

If[ Not@ValueQ[SpatialMedian::usage],
SpatialMedian::usage =
"SpatialMedian[{{x11, ..., x1p}, ..., {xn1, ..., xnp}}] estimates the \
p-dimensional median to be that vector minimizing the sum of the Euclidean \
distances between the vector and the n p-dimensional points in the sample."]

If[ Not@ValueQ[SimplexMedian::usage],
SimplexMedian::usage =
"SimplexMedian[{{x11, ..., x1p}, ..., {xn1, ..., xnp}}] estimates the \
p-dimensional median to be that vector minimizing the sum of the volumes \
of the p-dimensional simplices the vector forms with all possible combinations \
of p members from the sample."]

If[ Not@ValueQ[MultivariateTrimmedMean::usage],
MultivariateTrimmedMean::usage = 
"MultivariateTrimmedMean[{{x11, ..., x1p}, ..., {xn1, ..., xnp}}, f] gives the \
mean of the p-dimensional vectors, with a fraction f of the most outlying \
vectors dropped. When f = 0, MultivariateTrimmedMean gives the mean. \
As f -> 1.0, MultivariateTrimmedMean approaches the multivariate median \
ConvexHullMedian."]

If[ Not@ValueQ[EllipsoidQuantile::usage],
EllipsoidQuantile::usage =
"EllipsoidQuantile[{{x11, ..., x1p}, ..., {xn1, ..., xnp}}, q] gives the locus \
of the qth quantile of the p-variate data, where the data have been ordered \
using ellipsoids centered on Mean[{{x11, ..., x1p}, ..., {xn1, ..., xnp}}]. \
The fraction of the data lying inside of this locus is q.
EllipsoidQuantile[distribution, q] gives the ellipsoid centered on \
Mean[distribution] containing a fraction q of the specified distribution."]

If[ Not@ValueQ[EllipsoidQuartiles::usage],
EllipsoidQuartiles::usage =
"EllipsoidQuartiles[{{x11, ..., x1p}, ..., {xn1, ..., xnp}}] gives a list of \
the locii of the quartiles (q = .25, .50, .75) of the p-variate data, where \
the data have been ordered using ellipsoids centered on \
Mean[{{x11, ..., x1p}, ..., {xn1, ..., xnp}}]."]

If[ Not@ValueQ[Polytope::usage],
Polytope::usage =
"Polytope[{{x11, ..., x1p}, ..., {xn1, ..., xnp}}, connectivity] represents \
a p-dimensional polytope, with n vertices {x11, ..., x1p}, ..., {xn1, ..., xnp}, \
where the connections between vertices is specified by connectivity."]

If[ Not@ValueQ[PolytopeQuantile::usage],
PolytopeQuantile::usage =
"PolytopeQuantile[{{x11, x12}, ..., {xn1, xn2}}, q] gives the locus of the \
qth quantile of the bivariate data, where the data have been ordered using \
convex hulls centered on ConvexHullMedian[{{x11, x12}, ..., {xn1, xn2}}]. \
The fraction of the data lying inside of this locus is q."]

If[ Not@ValueQ[PolytopeQuartiles::usage],
PolytopeQuartiles::usage =
"PolytopeQuartiles[{{x11, x12}, ..., {xn1, xn2}}] gives a list of the locii \
of the quartiles (q = .25, .50, .75) of the bivariate data, where the data \
have been ordered using convex hulls centered on \
ConvexHullMedian[{{x11, x12}, ..., {xn1, xn2}}]."]

(* bivariate association statistics *)

If[ Not@ValueQ[SpearmanRankCorrelation::usage],
SpearmanRankCorrelation::usage =
"SpearmanRankCorrelation[{x1, ...., xn}, {y1, ...., yn}] gives Spearman's \
rank correlation coefficient (termed rho-b) between the x and y variables. \
(The alternatives rho-a and rho-b are different approaches for handling \
ties in rankings. The absolute value of rho-b is greater than the \
absolute value of rho-a in the case of ties.)"]

If[ Not@ValueQ[KendallRankCorrelation::usage],
KendallRankCorrelation::usage =
"KendallRankCorrelation[{x1, ...., xn}, {y1, ...., yn}] gives Kendall's \
rank correlation coefficient (termed tau-b) between the x and y variables. \
(The alternatives tau-a and tau-b are different approaches for handling \
ties in rankings. The absolute value of tau-b is greater than the \
absolute value of tau-a in the case of ties.)"]

(* scalar-valued multivariate dispersion statistics *)

If[ Not@ValueQ[GeneralizedVariance::usage],
GeneralizedVariance::usage =
"GeneralizedVariance[{{x11, ..., x1p}, ..., {xn1, ..., xnp}}] gives the \
generalized variance of the n p-dimensional vectors. This is equivalent to \
the determinant of the covariance matrix, or the product of the variances of the \
principal components of the data."]

If[ Not@ValueQ[TotalVariation::usage],
TotalVariation::usage =
"TotalVariation[{{x11, ..., x1p}, ..., {xn1, ..., xnp}}] gives the total \
variation of the n p-dimensional vectors. This is equivalent to the trace of \
the covariance matrix, or the sum of the variances of the principal components \
of the data."]

If[ Not@ValueQ[MultivariateMeanDeviation::usage],
MultivariateMeanDeviation::usage =
"MultivariateMeanDeviation[{{x11, ..., x1p}, ..., {xn1, ..., xnp}}] gives the \
scalar mean of the Euclidean distances between the p-variate mean and the \
p-variate data."]

If[ Not@ValueQ[MultivariateMedianDeviation::usage],
MultivariateMedianDeviation::usage =
"MultivariateMedianDeviation[{{x11, ..., x1p}, ..., {xn1, ..., xnp}}] gives the \
scalar median of the Euclidean distances between the p-variate median and the \
p-variate data."]

If[ Not@ValueQ[MedianMethod::usage],
MedianMethod::usage =
"MedianMethod is an option of multivariate descriptive statistic \
functions indicating which estimate of multivariate median (Median, \
SpatialMedian, SimplexMedian, or ConvexHullMedian) is used."]

(* multivariate shape statistics *)

If[ Not@ValueQ[MultivariateSkewness::usage],
MultivariateSkewness::usage =
"MultivariateSkewness[{{x11, ..., x1p}, ..., {xn1, ..., xnp}}] gives the \
multivariate coefficient of skewness for the p-variate data. A value close \
to zero indicates elliptical symmetry.
MultivariateSkewness[distribution] gives the coefficient of skewness of the \
specified multivariate statistical distribution."]

If[ Not@ValueQ[MultivariateKurtosis::usage],
MultivariateKurtosis::usage =
"MultivariateKurtosis[{{x11, ..., x1p}, ..., {xn1, ..., xnp}}] gives the \
multivariate kurtosis coefficient for the p-variate data. A value close to \
p*(p+2) indicates multinormality.
MultivariateKurtosis[distribution] gives the coefficient of kurtosis of the \
specified multivariate statistical distribution."]

(* multivariate data transformations *)

If[ Not@ValueQ[EstimateDOF::usage],
EstimateDOF::usage="EstimatedDOF is an obsolete option. It will be removed in a future version."]

If[ Not@ValueQ[MLE::usage],
MLE::usage =
"MLE is an option of descriptive statistic functions specifying whether \
the maximum likelihood estimate (MLE) or unbiased estimate of a statistic \
should be used."]

(* geometric primitives *)
(* following commented out to prevent shadowing problems with System`Ellipsoid *)
(*
If[ Not@ValueQ[Ellipsoid::usage],
Ellipsoid::usage =
"Ellipsoid[{x1, ..., xp}, {r1, ..., rp}, {d1, ..., dp}] represents a \
p-dimensional ellipsoid centered at the point {x1, ..., xp}, where the ith \
semi-axis has radius ri and lies in direction di. Ellipsoid[{x1, ..., xp}, \
{r1, ..., rp}, IdentityMatrix[p]] simplifies to Ellipsoid[{x1, ..., xp}, \
{r1, ..., rp}], an ellipsoid aligned with the coordinate axes."]
*)

KendallRankCorrelation::realvec=SpearmanRankCorrelation::realvec="The argument `1` \
at position `2` is expected to be a vector of real values with length greater than 1."

KendallRankCorrelation::len=SpearmanRankCorrelation::len="The length `1` of the second \
argument is not the same as the length `2` of the first argument."

Unprotect[EllipsoidQuantile, EllipsoidQuartiles, Ellipsoid,
	GeneralizedVariance, KendallRankCorrelation, MedianMethod, 
	MultivariateKurtosis, MultivariateMeanDeviation, 
	MultivariateMedianDeviation,  MultivariateSkewness, 
	MultivariateTrimmedMean, PolytopeQuantile, 
	PolytopeQuartiles, SimplexMedian, 
	SpatialMedian, SpearmanRankCorrelation, TotalVariation];


Begin["`Private`"]

(* SVD is changed *)
getSingularValues[w_?MatrixQ] := Tr[w, List];

QuantileQ[q_] :=
 If[FreeQ[N[q], Complex] && !TrueQ[N[q] < 0] && !TrueQ[N[q] > 1], True,
    False]


(* =========================== EllipsoidQuantile =========================== *)
(* ==================  multivariate estimate of location =================== *)

Options[EllipsoidQuantile] = {MLE -> False}

EllipsoidQuantile[m_?MatrixQ, q_, opt___] :=
  Module[{quantile, s, sinv, mean, dist, layer, n, qlevel},
    (
    quantile
    ) /; If[TrueQ[MLE /. {opt} /. Options[EllipsoidQuantile]], 
	    FreeQ[s = Covariance[m]*(Length[m]-1)/Length[m], Covariance],
	    FreeQ[s = Covariance[m], Covariance]] &&
         FreeQ[sinv = Inverse[s], Inverse] &&
         (mean = Mean[m];
	  {dist, layer} = ellipticLayer[Map[Sqrt[(#-mean).sinv.(#-mean)]&, m]]; 
          (* qlevel = {1., (n-n1)/n, (n-n1-n2)/n, ...} corresponding to the 
             fact that the 1st layer contains 100% of the points, the 2nd 
             contains (n-n1)/n of the points, etc. *)
	  n = Length[m];
          qlevel = Drop[N[(n - FoldList[Plus, 0, Map[Length, layer]]) / n], -1];
          quantile = iEllipsoidQuantile[s, mean, qlevel, dist, q];
	  quantile =!= $Failed
         )
  ] /; Head[q]=!=List && NumberQ[N[q]] && FreeQ[N[q], Complex] && 0<=N[q]<=1

iEllipsoidQuantile[s_, mu_, qlevel_, dist_, q_] :=
  Module[{outer, inner, do, di, qi, distance},
    (* locate quantile between layers for a local fit *)
    outer = 1;
    Scan[If[q > #, Return[outer], outer++]&, Drop[qlevel, 1]];
    do = dist[[outer]];
    If[outer == Length[dist],
	{qi, di} = {0, 0},
	inner = outer+1;
	qi = qlevel[[inner]]; 
	di = dist[[inner]]
    ];
    distance = di + (q-qi)/(qlevel[[outer]]-qi) (do-di);
    ellipsoidalLocus[mu, distance^2 s]
  ]

(* Case where q is a list of quantiles. *)
EllipsoidQuantile[m_?MatrixQ, q_?VectorQ, opt___] :=
  Module[{quantiles, quantile, s, sinv, mean, dist, layer, n, qlevel, scan},
    (
    quantiles
    ) /; If[TrueQ[MLE /. {opt} /. Options[EllipsoidQuantile]],
	    FreeQ[s = Covariance[m]*(Length[m]-1)/Length[m], Covariance],
	    FreeQ[s = Covariance[m], Covariance]] &&
         FreeQ[sinv = Inverse[s], Inverse] &&
         (mean = Mean[m];
	  {dist, layer} = ellipticLayer[Map[Sqrt[(#-mean).sinv.(#-mean)]&, m]]; 
          (* qlevel = {1., (n-n1)/n, (n-n1-n2)/n, ...} corresponding to the 
             fact that the 1st layer contains 100% of the points, the 2nd 
             contains (n-n1)/n of the points, etc. *)
	  n = Length[m];
          qlevel = Drop[N[(n - FoldList[Plus, 0, Map[Length, layer]]) / n], -1];
	  quantiles = {};
	  scan = Scan[(quantile =
			 iEllipsoidQuantile[s, mean, qlevel, dist, #];
		       If[quantile =!= $Failed,
			  quantiles = Append[quantiles, quantile],
			  Return[$Failed]])&,
		      q];
          scan =!= $Failed)
  ] /; Apply[And,
	 Map[(NumberQ[#] && FreeQ[#, Complex] && 0<=#<=1)&, N[q]]]

(* provides list of distances together with indices of points that are
	that distance from the mean; distances listed from largest to
	smallest; {{d1, ..., dn}, {ilist1, ..., ilist2}} *)	
ellipticLayer[d_] :=
   Module[{distance, distinct},
     distance = Transpose[{d, Range[Length[d]]}];
     distinct = Sort[Union[distance[[All,1]]], Greater];
     distinct = Map[Cases[distance, {#, _}]&, distinct];
     Transpose[ Map[Function[{y}, {y[[1, 1]], y[[All, 2]]}],
                    distinct] ]
   ]

(* used by EllipsoidQuantile *)
ellipsoidalLocus[mu_, sigma_] :=
  Module[{esystem, esystemT, sorted, r, dir},
    (* (x-mu).Inverse[sigma].(x-mu) == 1 *)
    If[!FreeQ[esystem = Eigensystem[sigma], Eigensystem], Return[$Failed]];
    (* radii are square roots of eigenvalues *)
    esystemT = Transpose[MapAt[Sqrt, esystem, 1]];
    (* sort semi-axes from largest to smallest *)
    sorted = Sort[esystemT, #1[[1]] > #2[[1]]&];
    {r, dir} = Transpose[sorted];
    Ellipsoid[mu, r, dir]
  ]


(* =========================== EllipsoidQuartiles ========================== *)
(* ==================  multivariate estimate of location =================== *)

Options[EllipsoidQuartiles] = {MLE -> False}

EllipsoidQuartiles[m_?MatrixQ, opt___] :=
  EllipsoidQuantile[m, {.25, .50, .75}, opt] 


(* ========= SpatialMedian multivariate robust measure of location ========= *)
Options[SpatialMedian] =
	{AccuracyGoal -> Automatic, 
	 MaxIterations -> 60, PrecisionGoal -> Automatic,
	 WorkingPrecision -> MachinePrecision}


SpatialMedian::invpar =
"Warning: `` `` must be a positive real number.  Default value assumed."

SpatialMedian::findmin =
"Minimization of objective function using FindMinimum failed."

SpatialMedian[m_?MatrixQ, opts___] :=
   Module[{median = iSpatialMedian[m, opts]},
	median /; median =!= $Failed
   ]

iSpatialMedian[m_, opts___] :=
   Module[{n, p, coordvar, coord, 
	   ag, maxiter, pg, wp, 
	   marginalMedian, L1marg, mean, L1mean, init, L1init,
	   result, distlist, args, 
	   FMresult},
	{n, p} = Dimensions[m];
	coordvar = Table[Unique[coord], {p}]; 
	{ag, maxiter, pg, wp} =
	    {AccuracyGoal, MaxIterations, PrecisionGoal, WorkingPrecision} /.
		{opts} /. Options[SpatialMedian];
	If[!IntegerQ[maxiter] || maxiter < 1,
	   Message[SpatialMedian::invpar, MaxIterations, maxiter];
	   maxiter = MaxIterations /. Options[SpatialMedian]
        ];
	If[!TrueQ[Positive[wp]],
	   Message[SpatialMedian::invpar, WorkingPrecision, wp];
	   wp = WorkingPrecision /. Options[SpatialMedian]
        ];

	marginalMedian = Median[m];
	L1marg = L1[m, marginalMedian];
	mean = Mean[m];
	L1mean = L1[m, mean];
	{init, L1init} = If[L1marg < L1mean,
				{marginalMedian, L1marg},
				{mean, L1mean}];

	distlist = Map[Norm[(coordvar - #)]&, m];

	distlist = Map[If[Precision[#] < wp,
			   SetPrecision[#, wp],
			   #]&,
		        distlist];
	args = Join[ { Total[distlist] },
		       Transpose[{coordvar, init}],
		     { AccuracyGoal -> ag, MaxIterations -> maxiter,
			PrecisionGoal -> pg, WorkingPrecision -> wp}
		];
	FMresult = Quiet[Apply[FindMinimum, args],
				{FindMinimum::lstol,FindMinimum::fmgz}];

        If[!FreeQ[FMresult, FindMinimum],
                Message[SpatialMedian::findmin];
                Return[$Failed]];
	  
	result = coordvar /. (FMresult[[2]]);

	result
	
   ]

L1[m_, vector_] := Total[Sqrt[Map[With[{newvec = (vector - #)}, newvec.newvec] &, m]]]


(* ========= SimplexMedian multivariate robust measure of location ========= *)
Options[SimplexMedian] =
	{AccuracyGoal -> Automatic, 
	 MaxIterations -> 60, PrecisionGoal -> Automatic,
	 WorkingPrecision -> MachinePrecision}


SimplexMedian::invpar =
"Warning: `` `` must be a positive real number.  Default value assumed."

SimplexMedian::findmin =
"Minimization of objective function using FindMinimum failed."

SimplexMedian[m_?MatrixQ, opts___] :=
   Module[{median = iSimplexMedian[m, opts]},
	median /; median =!= $Failed
   ]

iSimplexMedian[m_, opts___] :=
   Module[{n, p, coordvar, coord, unitVec,
	   ag, maxiter, pg, wp, allpsubsets,
	   marginalMedian, volmarg, mean, volmean, init, volinit,
	   savesubvalues, result, detlist, args, 
	   FMresult},
	{n, p} = Dimensions[m];
	coordvar = Table[Unique[coord], {p}]; 
	unitVec = Table[1, {p + 1}];
	{ag, maxiter, pg, wp} =
	    {AccuracyGoal, MaxIterations, PrecisionGoal, WorkingPrecision} /.
		{opts} /. Options[SimplexMedian];
	If[!IntegerQ[maxiter] || maxiter < 1,
	   Message[SimplexMedian::invpar, MaxIterations, maxiter];
	   maxiter = MaxIterations /. Options[SimplexMedian]
        ];
	If[!TrueQ[Positive[wp]],
	   Message[SimplexMedian::invpar, WorkingPrecision, wp];
	   wp = WorkingPrecision /. Options[SimplexMedian]
        ];
	(* For n = 20, p = 4, KSubsets[Range[n], p] has length 4845. *)
	allpsubsets = Subsets[Range[n], {p}];

	marginalMedian = Median[m];
	volmarg = totalvolume[m, allpsubsets, marginalMedian];
	mean = Mean[m];
	volmean = totalvolume[m, allpsubsets, mean];
	{init, volinit} = If[volmarg < volmean,
				{marginalMedian, volmarg},
				{mean, volmean}];

	savesubvalues = SubValues[Derivative]; (* save SubValues *)
	Derivative[1][Abs][x_] := Sign[x];

	detlist = Map[Det[ Prepend[Transpose[Append[#, coordvar]],
				 unitVec] ]&,
			Map[m[[#]]&, allpsubsets]  (* p p-vectors *)
		    ];
	detlist = Map[If[Precision[#] < wp,
			   SetPrecision[#, wp],
			   #]&,
		        detlist];
	args = Join[ { Norm[detlist,1] },
		       Transpose[{coordvar, init}],
		     { AccuracyGoal -> ag, MaxIterations -> maxiter,
			PrecisionGoal -> pg, WorkingPrecision -> wp}
		];
	FMresult = Quiet[Apply[FindMinimum, args],
				{FindMinimum::lstol,FindMinimum::fmgz}];

        If[!FreeQ[FMresult, FindMinimum],
                Message[SimplexMedian::findmin];
                Return[$Failed]];
	  
	result = coordvar /. (FMresult[[2]]);

	SubValues[Derivative] = savesubvalues; (* restore SubValues *)

	result
	
   ]


(* sum of (unsigned) volumes of p-dimensional simplices *)
(* Take the simplex formed by the p-vector "vector" and each set
	of p p-vectors specified in psubsets.  Find the volume of
	that simplex.  Sum the volumes of all simplices so formed. *)
totalvolume[m_, psubsets_, vector_] :=
 With[{unitvec = Table[1, {Length[vector] + 1}]},
  Apply[Plus, Map[
	Abs[Det[
		Prepend[Transpose[ Append[#, vector] ], unitvec ]
	]]&,
        Map[m[[#]]&, psubsets]
  ] ]
 ]


(* ================================ Layer ============================== *)
(* 
Layer[{{x11, ... , x1p}, ... , {xn1, ... , xnp}}] gives the indices of
those points lying on the convex layers of the p-dimensional n-point input
set ordered from outermost to innermost.  Layer[{{x11, ... , x1p}, ... ,
{xn1, ... , xnp}}, m] gives the outermost m layers.
*)

Layer[coord_] :=
	Module[{result = iLayer[coord, Infinity]},
	       result /; result =!= $Failed
	]

Layer[coord_, m_Integer?Positive] :=
	Module[{result = iLayer[coord, m]},
			result /; result =!= $Failed
	]

iLayer[coord_, m_] :=
	Module[{n, p, output = {}, i, singlelayer, vertices},
	   {n, p} = Dimensions[coord];
	   vertices = Range[n];
	   If[n <= p+1, Return[{vertices}]];
	   For[i = 1, (i <= m) && (Length[vertices] > p+1), i++,
	      singlelayer = ConvexHull[coord[[vertices]]];
	      If[!FreeQ[singlelayer, ConvexHull],
	         (* do not issue message when Layer is used internally... 
		 Message[Layer::conv, i]; *)
		 Return[$Failed]
	      ];
	      singlelayer = vertices[[singlelayer]];
	      output = Join[output, {singlelayer}];
	      vertices = Complement[vertices, singlelayer];
	   ];
	   If[Length[vertices] > 0 && m === Infinity,
		(* Anywhere from 1 to p+1 points in the interior. *)
		output = Join[output, {vertices}]];
	   output
        ]

Layer::conv = "ConvexHull failed at layer ``."


(* ==== MultivariateTrimmedMean multivariate robust measure of location ===== *)

MultivariateTrimmedMean::notimp =
"MultivariateTrimmedMean[matrix, f] using the convex hull method of trimming \
is not implemented for matrices n x p, where p > 2."

MultivariateTrimmedMean::conv = "Unable to find convex layer ``."

MultivariateTrimmedMean::fit = "Unable to interpolate between convex layers."

MultivariateTrimmedMean[m_?MatrixQ, 0] :=
    Mean[m]

(* NOTE: need to add support for other MedianMethods, such as SpatialMedian,
	and need to add ability to do convex hulls in 3 or more dimensions.
   Unfortunately, it is tricky to define a multivariate trimmed mean that
   transitions reasonable smoothly from the mean to a multivariate median. *)
MultivariateTrimmedMean[m_?MatrixQ, f_] :=
 Module[{mean},
   (
   mean
   ) /; 	 (If[Length[m[[1]]] > 2,
		     Message[MultivariateTrimmedMean::notimp];
		     mean = $Failed,
		     (* 2-dimensional data *)
		     mean = iTrimmedHullMean[m, f] 
		   ];
		  mean =!= $Failed)
	
   ] /;  f <= 1 

iTrimmedHullMean[m_, f_] :=
  Module[{n = Length[m], trimmed = m, level = 1, goalNum, outerNum = 0,
	  layer, currentNum, meano, meani, innerNum, x, fit, meanf},
    goalNum = n f; 
    layer = Layer[trimmed, 1];
    If[ !FreeQ[layer, Layer],
	Message[TrimmedHullMean::conv, level];
	Return[$Failed]];
    layer = layer[[1]];
    currentNum = Length[layer];
    While[goalNum > outerNum + currentNum, 
      (* add most recent layer to outer layers *)
      outerNum += currentNum;
      trimmed = Delete[trimmed, Map[List, layer]];
      (* compute new layer *)
      layer = Layer[trimmed, 1];
      level++;
      If[ !FreeQ[layer, Layer],
	  Message[TrimmedHullMean::conv, level];
	  Return[$Failed]];
      layer = layer[[1]];
      currentNum = Length[layer]
    ];	(* end While *)
    (* interpolate between layers *)
    meano = Mean[trimmed];
    trimmed = Delete[trimmed, Map[List, layer]];
    If[trimmed === {},
       meanf = meano,
       meani = Mean[trimmed];
       innerNum = outerNum+currentNum;
       If[innerNum == goalNum,
	  outerNum = innerNum;
	  meanf = meani,
          If[!FreeQ[fit =
	      MapThread[Fit[{{outerNum, #1}, {innerNum, #2}}, {1, x}, x]&,
                 {meano, meani}], Fit],
             Message[TrimmedHullMean::fit];
             Return[$Failed]];
       meanf = fit /. x -> goalNum
       ]
    ];
    meanf
  ]



(* =========================== Polytope ================================= *)

Format[Polytope[vertices_, connectivity_List]] :=
        Polytope[vertices, "-Connectivity-"]

(* For p=2, Graphics[Polytope[]] evaluates.
   For p=3, Graphics3D[Polytope[]] does not evaluate because 3D convex hulls
        are not yet implemented.
   For p=1 or p>3, no graphics is defined.
*)

(* p=2 *)
Polytope/: Graphics[Polytope[vertices_?MatrixQ, connectivity_], opts___] :=
        Graphics[Line[Append[vertices, First[vertices]]], opts] /;
                        Dimensions[vertices][[2]]==2

Unprotect[Graphics]

Graphics[{d1___, Polytope[p___], d2___}, opts___Rule] :=
        With[{primitive1 = Graphics[Polytope[p]][[1]]},
                Graphics[Join[{d1}, {primitive1}, {d2}], opts]
        ]

Graphics[{g1___, {directives___, Polytope[p___]}, g2___}, opts___Rule] :=
        With[{primitive1 = Graphics[Polytope[p]][[1]]},
                Graphics[{g1, Join[{directives}, {primitive1}], g2}]
        ]

(* =========================== PolytopeQuantile ========================== *)
(* ================= multivariate robust measure of location =============== *)
PolytopeQuantile::notimp =
"PolytopeQuantile[matrix, q] not implemented for matrices n x p, \
where p > 2."

PolytopeQuantile[m_?MatrixQ, q_] :=
  Module[{layer, innermost, median, n, qlevel, quantile},
   (
   quantile
   ) /; FreeQ[layer  = Layer[m], Layer] &&
	(innermost = Last[layer];
	 median = Mean[m[[innermost]]];
	 If[q == 0,
	    quantile = Polytope[{median}, {1}]; True,
            (* qlevel = {1., (n-n1)/n, (n-n1-n2)/n, ...} corresponding to the 
    	       fact that the 1st layer contains 100% of the points, the 2nd 
	       contains (n-n1)/n of the points, etc. *)
            n = Length[m];
	    qlevel = Drop[N[(n - FoldList[Plus, 0, Map[Length, layer]]) / n],
		 -1];
	    quantile = iPolytopeQuantile[m, layer, innermost, median,
		qlevel, q];
	    quantile =!= $Failed
         ])
  ] /; Length[m[[1]]] == 2 && Head[q]=!=List && NumberQ[N[q]] &&
	FreeQ[N[q], Complex] &&  0<=N[q]<=1 

iPolytopeQuantile[m_, layer_, innermost_, median_, qlevel_, q_] := 
  Module[{delta, outer, inner, qi, r, quantileVertices,
	  convexhull},
    If[Last[qlevel] - q >= 0,
       If[Length[innermost] == 1,
	  Return[ Polytope[{median}, {1}] ] ]; 
       If[Length[innermost] == 2,
	  delta = (q/Last[qlevel]) Apply[Subtract, m[[ innermost ]]];
	  Return[ Polytope[{median+delta/2, median-delta/2}, {1, 2}] ] ]
    ];
    (* locate quantile between layers for a local fit *)
    outer = 1;
    Scan[If[q > #, Return[outer], outer++]&, Drop[qlevel, 1]];
    (* determine points lying on quantile polygon *)
    Which[outer == Length[layer] || Length[layer[[outer+1]]] == 1,
	     (* quantile corresponds to innermost layer OR
		inner layer composed of one point *)
             qi = If[outer == Length[layer], 0., qlevel[[outer+1]]];
             r = (q-qi)/(qlevel[[outer]]-qi);
             quantileVertices =
	       Map[(median + r (#-median))&, m[[ layer[[outer]] ]] ],
	  True,
	     (* inner layer composed of two or more points *) 
	     Module[{qo, qi, sorted, ls, interquantileSegments,
		     INNER, OUTER, BOTH},
               qo = qlevel[[outer]];
               inner = outer + 1; qi = qlevel[[inner]];
               sorted = PolarSort[Join[
			Map[{INNER, m[[#]]}&, layer[[inner]]],
			Map[{OUTER, m[[#]]}&, layer[[outer]]]
		      ], median];
	       (* Relabel a pair of vertices sharing a common polar angle
		  (one INNER, one OUTER) as BOTH. *)
	       sorted  = sorted //.
		{{x___, {INNER, p1_, a_}, {OUTER, p2_, a_}, y___} :>
		 {x, {BOTH, {p1, p2}, a}, y}};
	       ls = Length[sorted];
               (* The interquantileSegments run between the two convex layers
		  that contain the desired quantile.  Each segment contains a
		  vertex from the INNER layer, a vertex from the OUTER layer, 
		  or vertices from BOTH layers.  The vertices of the desired
		  quantile layer lie on these segments.  *)
	       interquantileSegments = Map[Switch[sorted[[#, 1]],
		        BOTH,
			sorted[[#, 2]],
			INNER,
		        Module[{i=#, j=#, pi, pj, x, y, rule},
		         While[sorted[[i, 1]] === INNER, i = Mod[i, ls]+1 ];
		         While[sorted[[j, 1]] === INNER, j = Mod[j-2, ls]+1 ];
		         pi = If[sorted[[i, 1]]===BOTH,
			   sorted[[i, 2, 2]], sorted[[i, 2]] ];
		         pj = If[sorted[[j, 1]]===BOTH,
			   sorted[[j, 2, 2]], sorted[[j, 2]] ];
	                 rule = Solve[{Det[{{x, y, 1}, Append[pi, 1],
				Append[pj, 1]}] == 0,
		           	     Det[{{x, y, 1}, Append[median, 1],
				Append[sorted[[#, 2]], 1]}] == 0}, {x, y}][[1]];
			 {sorted[[#, 2]], {x, y} /. rule}] (* end Module *),
			OUTER,
		        Module[{i=#, j=#, pi, pj, x, y, rule},
		         While[sorted[[i, 1]] === OUTER, i = Mod[i, ls]+1 ];
		         While[sorted[[j, 1]] === OUTER, j = Mod[j-2, ls]+1 ];
		         pi = If[sorted[[i, 1]]===BOTH,
			   sorted[[i, 2, 1]], sorted[[i, 2]] ];
		         pj = If[sorted[[j, 1]]===BOTH,
			   sorted[[j, 2, 1]], sorted[[j, 2]] ];
	                 rule = Solve[{Det[{{x, y, 1}, Append[pi, 1],
				Append[pj, 1]}] == 0,
		           	     Det[{{x, y, 1}, Append[median, 1],
				Append[sorted[[#, 2]], 1]}] == 0}, {x, y}][[1]];
			 {{x, y} /. rule, sorted[[#, 2]]}] (* end Module *)
                       ]&, Range[ls] ]; (* end Map *)
               (* Now determine the points on the interquantileSegments
		  corresponding to the qth quantile. *)
	       r = (q-qlevel[[inner]])/(qlevel[[outer]]-qlevel[[inner]]);
	       quantileVertices =
	         Map[(#[[1]] + r (#[[2]]-#[[1]]))&, interquantileSegments]
             ] (* end Module *)
    ];	 (* end Which *)
    (* Make quantile locus convex. *)
    If[!FreeQ[convexhull = ConvexHull[quantileVertices], ConvexHull],
	Return[$Failed]
    ];
    Polytope[quantileVertices[[convexhull]], Range[Length[convexhull]]]
  ]


PolytopeQuantile[m_?MatrixQ, q_, opts___] :=
   Module[{},
	   Null /; (Message[PolytopeQuantile::notimp];
		    False)
   ] /; Length[m[[1]]] > 2 && NumberQ[N[q]] && FreeQ[N[q], Complex] &&
	 0<=N[q]<=1

(* Case where q is a list of quantiles. *)
PolytopeQuantile[m_?MatrixQ, q_?VectorQ] :=
  Module[{layer, innermost, median, n, qlevel, quantiles, quantile, scan},
   (
   quantiles
   ) /; FreeQ[layer  = Layer[m], Layer] &&
	(innermost = Last[layer];
	 median = Mean[m[[innermost]]];
         (* qlevel = {1., (n-n1)/n, (n-n1-n2)/n, ...} corresponding to the fact 
    	    that the 1st layer contains 100% of the points, the 2nd contains
	    (n-n1)/n of the points, etc. *)
         n = Length[m];
	 qlevel = N[(n - Most[Accumulate[Join[{0}, Map[Length, layer]]]]) / n];
	 quantiles = {};
	 scan = Scan[(If[# == 0,
			 quantile = Polytope[{median}, {1}],
			 quantile = iPolytopeQuantile[m, layer, innermost,
				       median, qlevel, #]];
		      If[quantile =!= $Failed,
			 quantiles = Append[quantiles, quantile],
			 Return[$Failed]])&,
		     q];
         scan =!= $Failed)
  ] /; Length[m[[1]]] == 2 && Apply[And, 
	 Map[(NumberQ[#] && FreeQ[#, Complex] && 0<=#<=1)&, N[q]]]


(* =========================== PolytopeQuartiles ========================= *)
(* ================= multivariate robust measure of location =============== *)

PolytopeQuartiles::notimp =
"PolytopeQuartiles[matrix, q] not implemented for matrices n x p, \
where p > 2."

PolytopeQuartiles[m_?MatrixQ] :=
	PolytopeQuantile[m, {.25, .50, .75}] /; Length[m[[1]]] == 2

PolytopeQuartiles[m_?MatrixQ] :=
   Module[{},
	   Null /; (Message[PolytopeQuartiles::notimp];
		    False)
   ] /; Length[m[[1]]] > 2


(* ================= computational geometry utilities ==================== *)

PolarSort[l_, median_]:=
   Module[{in,sorted},
	(* The centroid of the points is interior to the convex hull. *)
	origin=Mean[l[[All,2]]];
	(* 1st component of elements of 'in' is label,
	   2nd component of elements of 'in' is original coordinate,
	   3rd component of elements of 'in' is centered coordinate,
	   4th component of elements of 'in' is polar angle *)
	in = Map[Join[#, {#[[2]]-median, PolarAngle[#[[2]]-median]}]&, l];
	sorted = Sort[in,
                      Function[{p1,p2},
			       p1[[4]]<p2[[4]] ||
			       (* Changed the test p1[[4]]==p2[[4]] to
			       p1[[4]]-p2[[4]]==0 for numerical precision. *)
			      (p1[[4]]-p2[[4]]==0 &&
			      (p1[[3,1]]^2 + p1[[3,2]]^2 <
			       p2[[3,1]]^2 + p2[[3,2]]^2))
		      ] (* end Function *)
	]; (* end Sort *)
	Map[Delete[#, 3]&, sorted]
   ]

PolarAngle[{x_,y_}] := ArcTan[x, y] 

(* left turn if positive, right turn if negative *)
SignOfArea[{x1_,y1_},{x2_,y2_},{x3_,y3_}]:=
  Module[{area = x1(y2-y3) - x2(y1-y3) + x3(y1-y2), prec},
	  If[(prec = Precision[area]) === Infinity || prec == 0,
		  prec = MachinePrecision];
	  Sign[Chop[area, 10^(1-prec)]]
  ]


(* =========== scalar-valued bivariate association statistics ============ *)


$SpearmanRhoMessage=True;

$KendallTauMessage=True;

(* p. 38, Kendall *)
SpearmanRankCorrelation[{_?NumericQ},{_?NumericQ}] := (If[TrueQ[$SpearmanRhoMessage],
	$SpearmanRhoMessage=False;
	Message[General::obsfun, SpearmanRankCorrelation, SpearmanRho]];
	Indeterminate)

SpearmanRankCorrelation[args___] :=(If[TrueQ[$SpearmanRhoMessage],
	$SpearmanRhoMessage=False;
	Message[General::obsfun, SpearmanRankCorrelation, SpearmanRho]];
	Block[{res=iRankCorrelation[{args}, SpearmanRankCorrelation]},
	res/;res=!=$Failed] )


KendallRankCorrelation[{_?NumericQ},{_?NumericQ}] := (If[TrueQ[$KendallTauMessage],
	$KendallTauMessage=False;
	Message[General::obsfun, KendallRankCorrelation, KendallTau]];
	Indeterminate)

KendallRankCorrelation[args___] :=(If[TrueQ[$KendallTauMessage],
	$KendallTauMessage=False;
	Message[General::obsfun, KendallRankCorrelation, KendallTau]];
	Block[{res=iRankCorrelation[{args},KendallRankCorrelation]},
		res/;res=!=$Failed])

(* Checking that the Min and Max of the imaginary parts is exactly 0 is faster 
   than FreeQ[list, Complex] for machine numeric list *)
realVectorQ[x_]:=(VectorQ[#, NumberQ]&&({0,0}==={Min[#],Max[#]}&[Im[#]]))&[N[x]]

	
iRankCorrelation[arglist_,caller_]:=Block[{len1,len2},
	If[Not[ArgumentCountQ[caller, Length[arglist], 2, 2]],
		Return[$Failed]];	
	{len1,len2}=Map[Length, arglist];
	If[Not[realVectorQ[arglist[[1]]]]||len1<2,
		Message[caller::realvec, arglist[[1]], 1];
		Return[$Failed]];
	If[Not[realVectorQ[arglist[[2]]]]||len2<2,
		Message[caller::realvec, arglist[[2]], 2];
		Return[$Failed]];
	If[len1=!=len2,
		Message[caller::len, len2, len1];
		Return[$Failed]];
	If[caller===SpearmanRankCorrelation,
		spearmanrho, kendalltau][arglist[[1]],arglist[[2]],len1]
	]

spearmanrho[xlist_,ylist_,n_] :=
  Block[{xrank, yrank, xCorrection, yCorrection, SoS},
    {xrank, xCorrection} = rank[xlist];
    {yrank, yCorrection} = rank[ylist];
    SoS = Dot[#,#]&[(xrank-yrank)];
    ( 1/6 (n^3 - n) - SoS - xCorrection - yCorrection ) /
	Sqrt[ (1/6 (n^3 - n) - 2 xCorrection) (1/6 (n^3 - n) - 2 yCorrection) ]
  ] 

rank[zlist_] :=
  Block[{splitvals, adjustRank, lisval, ranklist, correctionTerm},
      	If[Length[Union[zlist]]===Length[zlist]
          ,
          (* case of no ties in zlist *)
          {Ordering[Ordering[zlist]], 0}
          ,
          (* case of ties in zlist *)
          (* splitvals groups {{value, zlistposition}, sortposition} by value *)
          splitvals=Split[
          	Transpose[{Sort[Transpose[{zlist, Range[Length[zlist]]}], #1[[1]]<#2[[1]]&],
                	Range[Length[zlist]]}]
                ,
                #1[[1,1]]-#2[[1,1]]==0&];
          (* adjustRank takes {{{value, zlistposition_}, sortposition_},...} and returns
             {zlistposition, ranking} where ranking is the mean sortposition for value *)
          adjustRank[lisval_List]:=If[Length[lisval]>1
          	,
            	Block[{oldvals=lisval},
              	  oldvals[[All,2]]=Mean[oldvals[[All,2]]];
              	  Transpose[{oldvals[[All,1,2]], oldvals[[All,2]]}]]
              	,
              	{{lisval[[1,1,2]], lisval[[1,2]]}}]; 
          (* adjust ranks for ties, return to the original ordering of zlist, 
             and return the list of ranks in ranklist *)
          ranklist=Sort[Flatten[Map[adjustRank, splitvals],1]][[All,2]];
          correctionTerm=Total[Map[(#^3-#)&, Map[Length, splitvals]]]/12;
          {ranklist, correctionTerm}
      	  ]
      	]
      

kendalltau[xlist_,ylist_,n_] :=
  Block[{s, nx, ny, num, prs = n*((n - 1)/2)}, 
 		s = Subsets[Transpose[{xlist, ylist}], {2}];
 		s = Sign[s[[All, 1]] - s[[All, 2]]]; 
 		{nx, ny} = prs - Total[Abs[s]];
 		num = s[[All, 1]] . s[[All, 2]]; 
 		num/Sqrt[(prs - nx)*(prs - ny)]]



(* =========== scalar-valued multivariate dispersion statistics ============ *)

Options[GeneralizedVariance] = Options[TotalVariation] =  {MLE -> False}

GeneralizedVariance[m_?MatrixQ, opt___] :=
  Module[{c},
    (
    Det[c]
    ) /; If[TrueQ[!(MLE /. {opt} /. Options[GeneralizedVariance])],
		FreeQ[c = Covariance[m], Covariance],
		FreeQ[c = Covariance[m]*(Length[m]-1)/Length[m], Covariance]]
  ]
	  
TotalVariation[m_?MatrixQ, opt___] :=
  Module[{c},
    (
    Tr[c] 
    ) /; If[TrueQ[!(MLE /. {opt} /. Options[TotalVariation])],
		FreeQ[c = Covariance[m], Covariance],
		FreeQ[c = Covariance[m]*(Length[m]-1)/Length[m], Covariance]]
  ]
	  
MultivariateMeanDeviation[m_?MatrixQ] :=
  Module[{mean},
    (
    Mean[Map[Norm[#-mean]&, m]]
    ) /; FreeQ[mean = Mean[m], Mean]
  ]

MultivariateMedianDeviation::conv =
"Unable to find median by peeling convex hulls. Using coordinate-wise median."
MultivariateMedianDeviation::simp =
"Unable to find median by minimizing total simplex volume. Using \
coordinate-wise median."
MultivariateMedianDeviation::spat =
"Unable to find median by minimizing total Euclidean distance. Using \
coordinate-wise median."

Options[MultivariateMedianDeviation] = {MedianMethod -> Median}

MultivariateMedianDeviation[m_?MatrixQ, opt___] :=
  Module[{median, temp},
    median =
      If[Length[m[[1]]] === 1,
	 (* all medians are identical in the univariate case *)
	 Median[m],
	 (* choice of medians in the multivariate case *)
         Switch[MedianMethod /. {opt} /. Options[MedianMethod],
   	  	Median,
		  Median[m],
		ConvexHullMedian,
		  If[FreeQ[temp = ConvexHullMedian[m], 
			   ConvexHullMedian] && temp =!= $Failed,
		     temp,
	  	     Message[MultivariateMedianDeviation::conv];	
		     Median[m]
		  ],
		SimplexMedian,
		  If[FreeQ[temp = SimplexMedian[m], 
			   SimplexMedian] && temp =!= $Failed,
		     temp,
	  	     Message[MultivariateMedianDeviation::simp];	
		     Median[m]
		  ],
		SpatialMedian,
		  If[FreeQ[temp = SpatialMedian[m], 
			   SpatialMedian] && temp =!= $Failed,
		     temp,
	  	     Message[MultivariateMedianDeviation::spat];	
		     Median[m]
		  ],
		_,
		  Median[m]
	 ]
      ];
     Median[Map[Norm[#-median]&, m]] 
  ]


MultivariateSkewness[m_?MatrixQ] :=
   Module[{s, sinv, mean = Mean[m], m1, n = Length[m]},
     (
     m1 = Map[(#-mean)&, m];
     Mean[Flatten[(m1.sinv.Transpose[m1])]^3]
     ) /; FreeQ[s = Covariance[m]*(n-1)/n, Covariance] &&
          FreeQ[sinv = Inverse[s], Inverse]
   ]


MultivariateKurtosis[m_?MatrixQ] :=
   Module[{s, sinv, mean = Mean[m], m1, n = Length[m]},
     (m1 = Map[(#-mean)&, m];
     Mean[(Map[(#.sinv.#)&, m1])^2]
     ) /; FreeQ[s = Covariance[m]*(n-1)/n, Covariance] &&
	  FreeQ[sinv = Inverse[s], Inverse]
   ]



(* ========================= data transformations ======================== *)


(* Eliminate third argument specifying
         direction if the Ellipsoid is aligned with coordinate axes. *)
Ellipsoid[mu_?VectorQ, r_?VectorQ, dir_?MatrixQ] :=
  Module[{rdir = Transpose[{r, dir}], p = Length[mu],
          sortedR, sortedDIR, newEllipsoid},
           newEllipsoid /;   (
                      {sortedR, sortedDIR} =
                        Transpose[Sort[rdir, Order[#1[[2]], #2[[2]]]==-1&]];
                      If[sortedDIR[[1, 1]] != 0,
                         sortedDIR /= sortedDIR[[1, 1]] ];
                      If[Apply[And, Map[#==0&, Flatten[
                                sortedDIR - IdentityMatrix[p] ]]],
                         newEllipsoid = Ellipsoid[mu, sortedR]; True,
                         False
                      ])
  ] /; Apply[Equal, Join[Dimensions[dir], {Length[mu], Length[r]}]]

(* For p=2, Graphics[Ellipsoid[]] evaluates.
   For p=3, Graphics3D[Ellipsoid[]] evaluates.
   For p=1 or p>3, no graphics is defined.
*)

(* p=2 *)
Ellipsoid/: Graphics[Ellipsoid[mu_, r_, dir_?MatrixQ], opts___Rule] :=
  graphicsEllipsoid[mu, r, dir[[1]], opts] /; (Length[mu] == Length[r] ==
                                        Dimensions[dir][[2]] == 2) &&
                                        1 <= Dimensions[dir][[1]] <= 2

(* For p=2, permit the direction to be specified as {a, b}, in addition to
        {{a, b}} and {{a, b}, {c, d}}. *)
Ellipsoid/: Graphics[Ellipsoid[mu_, r_, d1_?VectorQ], opts___Rule] :=
  graphicsEllipsoid[mu, r, d1, opts] /; Length[mu] == Length[r] ==
                                        Length[d1] == 2


(* For p=2, use Circle instead of ParametricPlot when ellipse is not tilted. *)
(*
Ellipsoid/: Graphics[Ellipsoid[mu_, r_], opts___Rule] :=
  Graphics[Circle[mu, r], opts] /; Length[mu]==Length[r]==2
*)


(* p=3 *)
Ellipsoid/: Graphics3D[Ellipsoid[mu_, r_, dir_?MatrixQ], opts___Rule] :=
  Module[{plotpoints, graphicsPrimitives},
     plotpoints = PlotPoints /. {opts} /. {PlotPoints -> Automatic};
     (* assume all direction vectors have unit length *)
     graphicsPrimitives = First[ParametricPlot3D[mu +
        Transpose[dir] . (r {Cos[t] Cos[u], Sin[t] Cos[u], Sin[u]}),
         {t, 0, 2Pi}, {u, -Pi/2, Pi/2},
         DisplayFunction->Identity, PlotPoints->plotpoints]];
     Graphics3D[graphicsPrimitives, opts]
  ] /; (Length[mu] == Length[r] ==
                        Dimensions[dir][[1]] == Dimensions[dir][[2]] == 3)

(* Graphics[Ellipsoid[]] code by Jeff Adams, 10/93 *)
graphicsEllipsoid[mu_, r_, d1_, opts___Rule] :=
  Module[{cos, sin, plotpoints, graphicsPrimitives, theta},
     plotpoints = PlotPoints /. {opts} /. {PlotPoints -> 30};
     (* assume d1 has unit length *)
     {cos, sin} = d1;
     graphicsPrimitives = First[ParametricPlot[mu +
         {{cos, -sin}, {sin, cos}} .
         (r {Cos[theta], Sin[theta]}), {theta, -Pi, Pi},
         DisplayFunction->Identity, PlotPoints->plotpoints]];
     Graphics[graphicsPrimitives, opts]
  ]


(* === rules permitting graphics directives for 3-arg forms =========== *)

Unprotect[Graphics]
Graphics[{d1___, Ellipsoid[e1_,e2_, e3_], d2___}, opts___Rule] :=
        With[{primitive1 = Graphics[Ellipsoid[e1, e2, e3]][[1,1,-1,-1]]},
                Graphics[Join[{d1}, {primitive1}, {d2}], opts]
        ]

Graphics[{g1___, {directives___, Ellipsoid[e1_, e2_, e3_]}, g2___}, opts___Rule] :=
        With[{primitive1 = Graphics[Ellipsoid[e1, e2, e3]][[1,1,-1,-1]]},
                Graphics[{g1, Join[{directives}, {primitive1}], g2}]
        ]

Protect[Graphics]

Unprotect[Graphics3D]
Graphics3D[{d1___, Ellipsoid[e1_, e2_, e3_], d2___}, opts___Rule] :=
        With[{primitive1 = Graphics3D[Ellipsoid[e1, e2, e3]][[1]]},
                Graphics3D[Join[{d1}, {primitive1}, {d2}], opts]
        ]

Graphics3D[{g1___, {directives___, Ellipsoid[e1_, e2_, e3_]}, g2___}, opts___Rule] :=
        With[{primitive1 = Graphics3D[Ellipsoid[e1, e2, e3]][[1]]},
                Graphics3D[{g1, Join[{directives}, {primitive1}], g2}]
        ]
Protect[Graphics3D]


(* ====================================================================== *)
End[]

Protect[EllipsoidQuantile, EllipsoidQuartiles, Ellipsoid,
	GeneralizedVariance, KendallRankCorrelation, MedianMethod, 
	MultivariateKurtosis, MultivariateMeanDeviation, MultivariateMedianDeviation,  
	MultivariateSkewness, MultivariateTrimmedMean, PolytopeQuantile, 
	PolytopeQuartiles, SimplexMedian, 
	SpatialMedian, SpearmanRankCorrelation, TotalVariation];


(* :Examples:

Table 1.2.1 "Multivariate Analysis", Mardia, Kent, & Bibby:

data = 
{{77, 82, 67, 67, 81}, {63, 78, 80, 70, 81}, {75, 73, 71, 66, 81},
{55, 72, 63, 70, 68}, {63, 63, 65, 70, 63}, {53, 61, 72, 64, 73},
{51, 67, 65, 65, 68}, {59, 70, 68, 62, 56}, {62, 60, 58, 62, 70},
{64, 72, 60, 62, 45}, {52, 64, 60, 63, 54}, {55, 67, 59, 62, 44},
{50, 50, 64, 55, 63}, {65, 63, 58, 56, 37}, {31, 55, 60, 57, 73},
{60, 64, 56, 54, 40}, {44, 69, 53, 53, 53}, {42, 69, 61, 55, 45},
{62, 46, 61, 57, 45}, {31, 49, 62, 63, 62}, {44, 61, 52, 62, 46},
{49, 41, 61, 49, 64}, {12, 58, 61, 63, 67}, {49, 53, 49, 62, 47},
{54, 49, 56, 47, 53}, {54, 53, 46, 59, 44}, {44, 56, 55, 61, 36},
{18, 44, 50, 57, 81}, {46, 52, 65, 50, 35}, {32, 45, 49, 57, 64},
{30, 69, 50, 52, 45}, {46, 49, 53, 59, 37}, {40, 27, 54, 61, 61},
{31, 42, 48, 54, 68}, {36, 59, 51, 45, 51}, {56, 40, 56, 54, 35},
{46, 56, 57, 49, 32}, {45, 42, 55, 56, 40}, {42, 60, 54, 49, 33},
{40, 63, 53, 54, 25}, {23, 55, 59, 53, 44}, {48, 48, 49, 51, 37},
{41, 63, 49, 46, 34}, {46, 52, 53, 41, 40},
{46, 61, 46, 38, 41}, {40, 57, 51, 52, 31}, {49, 49, 45, 48, 39},
{22, 58, 53, 56, 41}, {35, 60, 47, 54, 33}, {48, 56, 49, 42, 32},
{31, 57, 50, 54, 34}, {17, 53, 57, 43, 51}, {49, 57, 47, 39, 26},
{59, 50, 47, 15, 46}, {37, 56, 49, 28, 45}, {40, 43, 48, 21, 61},
{35, 35, 41, 51, 50}, {38, 44, 54, 47, 24}, {43, 43, 38, 34, 49},
{39, 46, 46, 32, 43}, {62, 44, 36, 22, 42}, {48, 38, 41, 44, 33},
{34, 42, 50, 47, 29}, {18, 51, 40, 56, 30}, {35, 36, 46, 48, 29},
{59, 53, 37, 22, 19}, {41, 41, 43, 30, 33}, {31, 52, 37, 27, 40},
{17, 51, 52, 35, 31}, {34, 30, 50, 47, 36}, {46, 40, 47, 29, 17},
{10, 46, 36, 47, 39}, {46, 37, 45, 15, 30}, {30, 34, 43, 46, 18},
{13, 51, 50, 25, 31}, {49, 50, 38, 23, 9}, {18, 32, 31, 45, 40},
{8, 42, 48, 26, 40}, {23, 38, 36, 48, 15}, {30, 24, 43, 33, 25},
{3, 9, 51, 47, 40}, {7, 51, 43, 17, 22}, {15, 40, 43, 23, 18},
{15, 38, 39, 28, 17}, {5, 30, 44, 36, 18}, {12, 30, 32, 35, 21},
{5, 26, 15, 20, 20}, {0, 40, 21, 9, 14}};


PrincipalComponents[data, Method->CovarianceMatrix]

   PrincipalComponents[,Method->CovarianceMatrix],
	 s = {{305.768, 127.223, 101.579, 106.273, 117.405}, 
    		{127.223, 172.842, 85.1573, 94.6729, 99.012}, 
    		{101.579, 85.1573, 112.886, 112.113, 121.871}, 
    		{106.273, 94.6729, 112.113, 220.38, 155.536}, 
    		{117.405, 99.012, 121.871, 155.536, 297.755}}
	 evals = Eigenvalues[s]
		 {686.99, 202.111, 103.747, 84.6304, 32.1533}
	 sum = Apply[Plus, evals]
		 1109.63
	 evals/sum
		 {0.619115, 0.182142, 0.0934968, 0.0762689, 0.0289766}
	 	(percentages agree with p. 220, Mardia, Kent, & Bibby)

	evecs = Eigenvectors[s]
	evecs = Sign[evecs[[1, 1]]] evecs
		{{0.505446, 0.368349, 0.345661, 0.451123, 0.53465}, 
    		{0.748748, 0.207403, -0.0759081, -0.300888, -0.547782}, 
    		{-0.299789, 0.41559, 0.145318, 0.596626, -0.600276}, 
    		{-0.296184, 0.782888, 0.00323634, -0.51814, 0.175732}, 
    		{0.0793939, 0.188876, -0.92392, 0.285522, 0.151232}}
		
	mean = {38.9545, 50.5909, 50.6023, 46.6818, 42.3068}
	Expand[evecs.({x1, x2, x3, x4, x5}-mean)]
		{-99.4943 + 0.505446 x1 + 0.368349 x2 + 0.345661 x3 +
			 0.451123 x4 + 0.53465 x5,
		1.40223 + 0.748748 x1 + 0.207403 x2 - 0.0759081 x3 - 
			0.300888 x4 - 0.547782 x5, 
		-19.1562 - 0.299789 x1 + 0.41559 x2 + 0.145318 x3 +
			 0.596626 x4 - 0.600276 x5,
		-11.48 - 0.296184 x1 + 0.782888 x2 + 0.00323634 x3 - 
			0.51814 x4 + 0.175732 x5, 
		14.3774 + 0.0793939 x1 + 0.188876 x2 - 0.92392 x3 +
			 0.285522 x4 + 0.151232 x5}
		(agrees with p.218, Mardia, Kent, & Bibby, to within roundoff
			error)


PrincipalComponents[data, Method->CorrelationMatrix]

   PrincipalComponents[,Method->CorrelationMatrix]
	s = {{1., 0.553405, 0.546751, 0.409392, 0.389099}, 
	    {0.553405, 1., 0.609645, 0.485081, 0.436449}, 
	    {0.546751, 0.609645, 1., 0.710806, 0.664736}, 
	    {0.409392, 0.485081, 0.710806, 1., 0.607174}, 
	    {0.389099, 0.436449, 0.664736, 0.607174, 1.}}
	evals = Eigenvalues[s]
		{3.18098, 0.739572, 0.444965, 0.387892, 0.246591}
	sum = Apply[Plus, evals]
		5.
	evals/sum
		{0.636196, 0.147914, 0.088993, 0.0775784, 0.0493182}
	 	(percentages agree with p. 220, Mardia, Kent, & Bibby)

	evecs = Eigenvectors[s]
	evecs = Sign[evecs[[1, 1]]] evecs
		{{0.399605, 0.431419, 0.503282, 0.456994, 0.438244}, 
    		{0.645458, 0.441505, -0.129068, -0.387906, -0.470454}, 
    		{0.620782, -0.705006, -0.037049, -0.136182, 0.312533}, 
    		{-0.145787, 0.298135, -0.108599, -0.666256, 0.658916}, 
    		{-0.130672, -0.181748, 0.846689, -0.422189, -0.234022}}

	(evecs.{u1, u2, u3, u4, u5})[[1]]
		0.399605 u1 + 0.431419 u2 + 0.503282 u3 + 0.456994 u4 + 
			0.438244 u5
		(agrees with bottom of p.219, Mardia, Kent, & Bibby)
	
	sd = StandardDeviation[N[data]]
		{17.4862, 13.1469, 10.6248, 14.8452, 17.2556}

	((evecs.{u1, u2, u3, u4, u5})[[1]]) /.
		 {u1->x1/17.4862, u2->x2/13.1469, u3->x3/10.6248,
			 u4->x4/14.8452, u5->x5/17.2556}

		0.0228526 x1 + 0.0328153 x2 + 0.0473686 x3 + 0.030784 x4 + 
			0.0253972 x5
		(agrees with top of p.220, Mardia, Kent, & Bibby)
		

	





*)


