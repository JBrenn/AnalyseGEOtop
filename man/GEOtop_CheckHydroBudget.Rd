\name{GEOtop_CheckHydroBudget}
\alias{GEOtop_CheckHydroBudget}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Check the hydrological budget of a GEOtop 3d simulation
}
\description{
Comparison of simulated and observed runoff (if provided). Checking the hydrological budget of the simulation (dS/dt = P - Q - ET).
}
\usage{
GEOtop_CheckHydroBudget(wpath, Q_obs, Q_obs_data, soil_files, list_file="listpoints.txt")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{wpath}{
  working path, pointing into simulation folder
  }
  \item{Q_obs}{
  character describing if and in which time scale observed discharge data is provided. "hour": hourly data; "day": daily data;"n": no observed discharge data prvided
  }
  \item{Q_obs_data}{
  zoo object, observed discharge data in m^3/s
  }
  \item{soil_files}{
  boolean, TRUE: soil files are provided as GEOtop input. FALSE: soil is parameterized in the geotop.inpts file
  }
    \item{list_file}{
  character, name of the listpoint file defining GEOtop output points, if not available: list_file = NULL, information is read from geotop.inpts
  }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
PDF files containing specific analyse plots:
\item{Ppartitioning.pdf}{Areal precipitation amounts and partitioning in the components snow and rain, first glew on discharge}
\item{QsimVSQobs.pdf}{Simulated versus observed discharge, hourly - daily - monhly aggregations and GOFs}
\item{WaterBudget.pdf}{Analytic plot on the water budget of the simulation}
}
\references{
%% geotop reference Endrizzi2013, Bertoldi2006
Endrizzi, S., Gruber, S., Amico, M. D., & Rigon, R. (2014). \strong{GEOtop 2.0 : simulating the combined energy and water balance at and below the land surface accounting for soil freezing , snow cover and terrain effects.} \emph{Geosci. Model Dev., 7, 2831-2857}. doi:10.5194/gmd-7-2831-2014
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
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--  or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x) 
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{}
\keyword{}% __ONLY ONE__ keyword per line
