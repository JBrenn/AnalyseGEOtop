\name{GEOtop_VisSoilWaterRet}
\alias{GEOtop_VisSoilWaterRet}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Visualisation Soil Water Retention Curve
}
\description{
Visulisation of the Soil Water Retention Curve (van Genuchten 1980) for soil parameter used in GEOtop simulation. Theoretical and parametererized physical soil water parameters are compared. For soil water calculathon from soil water pressure head (hPa or cm water column) the package \pkg{soilwater} is used.
}
\usage{
GEOtop_VisSoilWaterRet(alpha, n, theta_sat, theta_res, theta_pwp, theta_fc)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{alpha}{
  alpha van Genuchten parameter [1/m]; inverse of a length - scale parameter
}
  \item{n}{
  N van Genuchten parameter [-]; shape parameter
}
  \item{theta_sat}{
  saturated soil water content [m³/m³]
}
  \item{theta_res}{
  residual soil water content [m³/m³]
}
  \item{theta_pwp}{
  permanent wilting point [m³/m³]
}
  \item{theta_fc}{
  field capacity [m³/m³]
}

}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Genuchten, M. Van. (1980). A closed-form equation for predicting the hydraulic conductivity of unsaturated soils. Soil Science Society of America Journal, 44, 892–898.
}
\author{
Johannes Brenner, \email{Johannes.Brenner@eurac.edu}
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
