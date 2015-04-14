\name{GEOtop_ReadPlotRst}
\alias{GEOtop_ReadPlotRst}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Visualize .asc raster map
}
\description{
Visualize .asc raster map (GEOtop map output) with ggplot
}
\usage{
GEOtop_ReadPlotRst(map, date, variable, layer, limits, legend, lowcol="#f7fbff", highcol="#08306b")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{map}{character, full path and name of .asc map to visualize}
  \item{date}{character, date characterization}
  \item{variable}{character, variable characterization}
  \item{map}{numeric, vector setting y-limits for plot}
  \item{legend}{character, legend characterization, e.g. "vol\%" for soil moisture data}
  \item{lowcol}{minimum colour for visualization, default = "#f7fbff"}
  \item{highcol}{maximum colour for visualization, default = "#08306b"; default is creating a blue colour range}
} 
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
ggplot object
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

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
