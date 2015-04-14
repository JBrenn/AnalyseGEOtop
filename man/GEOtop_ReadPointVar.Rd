\name{GEOtop_ReadPointVar}
\alias{GEOtop_ReadPointVar}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Read specific variable from GEOtop point output
}
\description{
Read specific variable from GEOtop point output, e.g. precipitation from PointOutputFile, SMC of specific layer PointOutputFile. 
}
\usage{
GEOtop_ReadPointVar(wpath, keyword, varOFint, coordinates_value_df=FALSE, landcover_classes)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{wpath}{
  path pointing into simulation folder
  }
    \item{keyword}{
  keyword for files to read (point: "PointOutputFile", snow: "SnowProfileFile", snowcover: "SnowCoveredAreaFile", thetaliq: "SoilLiqContentProfileFile", thetaliq: "SoilIceContentProfileFile")
  }
  \item{varOFint}{
    keyword for variable of interest (see headers of GEOtop output files)
    }
   \item{coordinates_value_df}{
  boolean, TRUE: create data frame containing output coordinates and corresponding characteristics (input maps have to be provided, default = FALSE)
  }
    \item{landcover_classes}{
  character vector, names of landcover classes in GEOtop simulation
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
%% ~put references to the literature/web site here ~
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
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x) 
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
