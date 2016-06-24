\name{Geotop_VisSoilWaterRet_gg}
\alias{Geotop_VisSoilWaterRet_gg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Visualisation Soil Water Retention Curve with ggplot
}
\description{
Visulisation of the Soil Water Retention Curve van Genuchten Model (1980). For soil water calculation from soil water pressure head (hPa or cm water column) the package \pkg{soilwater} is used. Density functions for provided soil depths are visualized as side plots.
}
\usage{
Geotop_VisSoilWaterRet_gg(alpha, n, theta_sat, theta_res, accurate=10,
                        observed=NULL, add_ref_curves=TRUE)
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
  \item{accurate}{
  default = 10: water potential is given as \emph{seq(1,10^7,10)}, for accurate plotting set argument 1
}
  \item{observed}{
  numeric data.frame, providing observed soil moisture data [m³/m³] (first column, named SWC) and water potential [hPa] (second column, named SWP) and soil depth [cm] (third coumn, named depth)
  }
  \item{add_ref_curves}{
 boolean, default = TRUE: reference soil water retention curves are added for clay, loam and sand; van Genuchten parameterisation according to ROSETTA manual
}
  \item{colors}{
  colors for water retention curves and density functions, default = NULL: grey color range is used
}

}
\details{
%%  ~~ If necessary, more details than the description above ~~
If multiple vanGenuchten models should be visualised, simply provide alpha, n, theta_sat, and theta_res values as vectors.
}
\value{
gg table object
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Van Genuchten, M. (1980). A closed-form equation for predicting the hydraulic conductivity of unsaturated soils. Soil Science Society of America Journal, 44, 892–898.
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
## Not run:
library(AnalyseGeotop)

data("Kaltern_SoilWatRet")

colors <- c("red","#E69F00","blue","darkgreen")

gg <- Geotop_VisSoilWaterRet_gg(alpha = parameter$alpha, n = parameter$n, 
                                theta_sat = parameter$thetaS, 
                                theta_res = rep(.05,length(parameter$alpha)), 
                                accurate = 10, 
                                add_ref_curves = T, observed = observed, 
                                colors = colors)
## End(**Not run**)                                
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
