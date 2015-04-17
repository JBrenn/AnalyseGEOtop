\name{GEOtop_Read_multipoint}
\alias{GEOtop_Read_multipoint}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Read multi point output from GEOtop simulation
}
\description{
Read multi point output from GEOtop simulation using the \pkg{geotopbricks} package.
}
\usage{
GEOtop_Read_multipoint(path, model_run, stations, val_aggr, soil_files, lc_classes, linux)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{path}{
  path pointing to simulation folder
  }
  \item{model_run}{
  name of simulation folder
  }
  \item{stations}{
  character vector, names of GEOtop output sites
  }
  \item{val_aggr}{
  aggregation performed on output data. "h": hourly; "d": daily
  }
  \item{soil_files}{
  boolean, TRUE: soil files are provided as GEOtop input. FALSE: soil is parameterized in the geotop.inpts file
  }
  \item{lc_classes}{
  character vector, names of landcover classes in GEOtop simulation
  }
  \item{linux}{
  boolean, TRUE: working on linux laptop, FALSE: working on windows laptop; for path retrieval via \emph{data(SWCinfo)}
  }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
workspace \emph{point.RData}, containing point output variables, soil moisture and soil temperature output (list_station), input soil characteristics (soil_input). Stored in GEOtop simulation folder, defined by the arguments \emph{path} and \emph{model_run}.
}
\references{
Endrizzi, S., Gruber, S., Amico, M. D., & Rigon, R. (2014). \strong{GEOtop 2.0 : simulating the combined energy and water balance at and below the land surface accounting for soil freezing , snow cover and terrain effects.} \emph{Geosci. Model Dev., 7, 2831-2857}. doi:10.5194/gmd-7-2831-2014
}
\author{
Johannes Brenner, \email{Johannes.Brenner@eurac.edu}
}
\note{
Slow, as point files are read for each variable!
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
