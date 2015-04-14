\name{GEOtop_CreateInptsMAPS}
\alias{GEOtop_CreateInptsMAPS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create GEOtop input maps
}
\description{
Create GEOtop input maps using \pkg{RSAGA}. Base maps are a digital elevation model and a map locating the basin outlet (defined by -1). Postprocessing includes terrain analysis (slope, aspect, curvature) and sky view factor calculation. All maps, including landcover, soil type and bedrock (if provided), are stored in SAGA .srgd format. Further processing includes watershed delineation, resampling to required resolution, masking to the extent of the watershed (either for all maps or for landcover map only) and writing of GEOtop input ascii files.
}
\usage{
GEOtop_CreateInptsMAPS(dem, res, stream_gauge, name_gauge, preprocess=TRUE, soil=FALSE, landcover=FALSE, bedrock=FALSE, SAGAENV, mask_only_lc=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dem}{
  ascii digital elevation model, located in working directory
  }
  \item{res}{
  resolution of the output maps in meters
  }
  \item{stream_gauge}{
  ascii, defining location of stream gauge, located in working directory (NA ascii, stream gauge location = -1)
  }
   \item{name_gauge}{
  name of the stream gauge
  }
  \item{preprocess}{
  boolean, TRUE: preprocessing (terrain analysis); FALSE: no preprocessing, use maps created in former run; default = TRUE
  }
  \item{soil}{
  boolean, TRUE: soil type map provided in working directory, ascii format, name: soil.asc; FALSE: no soil type map provided; default = FALSE
  }
   \item{landcover}{
  boolean, TRUE: landcover map provided in working directory, ascii format, name: landcover.asc; FALSE: no landcover map provided; default = FALSE
  }
  \item{bedrock}{
  boolean, TRUE: bedrock map provided in working directory, ascii format, name: soil.asc; FALSE: no bedrock map provided; default = FALSE
  }
  \item{SAGAENV}{
  SAGA environment, created with function \emph{rsaga.env} of the \pkg{RSAGA} package
  }
  \item{mask_only_lc}{
  boolean, TRUE: mask only landcover map; FALSE: mask all maps and crop to data; default = FALSE
  }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
GEOtop input maps, storged in working directory as ascii maps
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
JOhannes Brenner, \email{Johannes.Brenner@eurac.edu}
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
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
