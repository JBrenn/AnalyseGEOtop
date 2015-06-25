\name{GEOtop_ReadValidationData}
\alias{GEOtop_ReadValidationData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Read point output from GEOtop for verification of the model results}
\description{
Read point output from GEOtop for verification of the model results}
\usage{
GEOtop_ReadValidationData(wpath, observations, soil_files=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{wpath}{
path into simulation folder
}
  \item{observation}{
zoo object, data frame, with specific names of variables used for validate the model results; name conventions according to CF Standard Name Table \url{http://cfconventions.org}.
}
 \item{soil_files}{
  boolean, TRUE: soil files are provided as GEOtop input. FALSE: soil is parameterized in the geotop.inpts file
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{

}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Johannes Brenner
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
