\name{GEOtop_multiplePointPlot_Montecini}
\alias{GEOtop_multiplePointPlot_Montecini}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Analysis of GEOtop one-dimensional simulations for the Montecini domain
}
\description{
Reading and analysing GEOTtop simulations providing multiple point output for the Montecini domain (micro-meteo stations B, I, and P). 
}
\usage{
GEOtop_multiplePointPlot_Montecini(path, model_run, stations, val_aggr, read_data, calibrate, 
                                    use_swc_liq, soil_files, lc_classes, linux)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
 \item{path}{
  path pointing into simulation folder
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
  \item{read_data}{
  boolean, TRUE: GEOtop point output is read; FALSE: R workspace point.RData (from former run) is read
  }
  \item{calibrate}{
  boolean, TRUE: calibrated SMC sensor data is used for comparision with simulated SMC; FALSE: use of uncalibrated SMC data
  }
    \item{use_swc_liq}{
  boolean, TRUE: only liquid component of SMC is used for comparision with simulated SMC; FALSE: use of liquid and solid SMC data
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

}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Endrizzi, S., Gruber, S., Amico, M. D., & Rigon, R. (2014). \strong{GEOtop 2.0 : simulating the combined energy and water balance at and below the land surface accounting for soil freezing , snow cover and terrain effects.} \emph{Geosci. Model Dev., 7, 2831-2857}. doi:10.5194/gmd-7-2831-2014
}
\author{
Johannes Brenner, \email{Johannes.Brenner@eurac.edu}
}
\note{
Observed SMC data derived from function ... (package)
Observed Soil Temperature data from function ... (package)
Observed latent and sensible heat data 
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
